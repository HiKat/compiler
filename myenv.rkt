#lang racket
(require (prefix-in stx: "mysyntax.rkt"))
(provide (all-defined-out))



(struct type_pointer (pointer type) #:transparent)
;funはシンボル 'fun、outは戻り値の型、inは引数の型のリスト.
(struct type_array (type size) #:transparent)
;パラメータの無い時'noparaが入る.
(struct type_fun (fun out in) #:transparent)
;ポインタ型のみリスト構造で(list 'pointe var)の形式.

(struct para_flag (out-type para))
(struct fundef_flag (out-type para))
(struct comp_flag (decl stat n))


;環境操作関数.
(struct obj (name lev kind type)#:transparent)
(define initial-env '())

(define (lookup-env name e)
  (if (equal? e initial-env) 
      #f
      (if (equal? (obj-name (car e)) name)
          (car e)
          (lookup-env name (cdr e)))))


(define (in-env? name e)
  (if (equal? e initial-env) 
      #f
      (if (equal? (obj-name (car e)) name)
          #t
          (in-env? name (cdr e)))))

(define (extend-env x e)
  (cons x e))

(define (add-list l e)
  (if (equal? l '()) 
      e
      (let* ((newenv (extend-env (car l) e)))
        (add-list (cdr l) newenv))))

;例) '((a) (b c d) (e f) (g)) -> '(a b c d e f g)
(define (separate-list l)
  (cond 
    ((equal? '() (cdr l)) (car l))
    ((equal? '() (cddr l)) (append (car l) (cadr l)))
    (else (separate-list (list (append (car l) (cadr l)) (caddr l))))))

;例)'(a (b c d) (e f) g) -> '((a) (b c d) (e f) (g))
;map関数と合わせて使う
(define (make-list-list l)
  (cond ((list? l) l)
        (else (list l))))



;objやobjのlist構造など  
;を受け取って
;環境と照らしあわせて
;異常があれば、
;エラー、(error 'failed)等もしくは
;警告
; (format "ERROR! redifinition of '~a'."(obj-name obj))等
;を出力する.
;正しければ
;たとえば(format "'~a' is OK!" obj)とか
;正常に意味解析が行われていることがわかればよい?
;理想は何も出力しないこと
;check-proto-paraは  
;(list obj...)か  
;'noparaを  
;受け取って  
;エラーもしくはメッセージを返す  
;para-envとenvの両方で探す  
(define (check-decl obj env)
  (map (lambda (x) (cond ((and (equal? (obj-name x) (obj-name obj))
                               (equal? 'fun (obj-kind x))
                               (equal? 0 (obj-lev obj)))
                          (error (display (format "ERROR! REDEFINITION OF '~a' AT" x))))
                         ((and (equal? (obj-name x) (obj-name obj))
                               (equal? 'var (obj-kind x))
                               (equal? (obj-lev x) (obj-lev obj)))
                          (error "ERROR! REDEFINITION OF "(obj-name obj)))
                         ((and (equal? (obj-name x) (obj-name obj))
                               (equal? 'parm (obj-kind x)))
                          (begin(display (format "WARNING!! SAME NAME IN PARAMETERS AND VAR DECLARATIONS\n"))
                                #t))))
       env))


(define (check-proto-para obj-list)
  (cond 
    ((equal? '() obj-list) (display (format "OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.\n")))
    ((equal? 'nopara obj-list) (display(format "OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.\n")))
    ((in-env? (obj-name (car obj-list)) (cdr obj-list))
     (error "ERROR! SOME REDEFINITION IN PARAMETERS OF FUNCTION PROTOTYPE"))
    (else (check-proto-para (cdr obj-list)))))
(define (check-def-para obj-list)
  (cond 
    ((equal? '() obj-list) (display (format "OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.\n")))
    ((equal? 'nopara obj-list) (display(format "OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.\n")))
    ((in-env? (obj-name (car obj-list)) (cdr obj-list))
     (error "ERROR! SOME REDEFINITION IN PARAMETERS OF FUNCTION DEFINITION"))
    (else (check-proto-para (cdr obj-list)))))
(define (check-proto obj env)
  (map 
   ;プロトタイプとnameが同じ、levelがで0、typeが同じでないかどうかをmapで一つづつ判定する.
   (lambda (x)(cond ((and (equal? (obj-name x) (obj-name obj))
                          (equal? 0 (obj-lev x))
                          (not (equal? (obj-type x) (obj-type obj))))
                     (error "ERROR! REDEFINITION OF "(obj-type x)(obj-type obj)))
                    (else (display (format "OK! CRRECT FUNCTION PROTOTYPE OF '~a'.\n" (obj-name obj))))))
   env))
(define (check-func obj env)
  (map 
   ;関数定義とnameが同じ、kindが'funかどうかをmapで一つづつ判定する.
   (lambda (x)(cond ((and (equal? (obj-name x) (obj-name obj))
                          (equal? 'fun (obj-kind x)))
                     (error "ERROR! REDEFINITION OF "(obj-name obj)))
                    (else (display (format "OK! CRRECT FUNCTION PROTOTYPE OF '~a'.\n" (obj-name obj))))))
   env))  
;引数stはstx:id_stもしくはstx:id_ast_st
(define (check-var-ref st lev env)
  (let* ((name (cond ((stx:id_st? st) (stx:id_st-name st))
                     ((stx:id_ast_st? st) (stx:id_ast_st-name st))))
         (referred-obj 
          (lookup-env name 
                      (map (lambda (x) 
                             (if (in-env? name env)
                                 (cond ((and (equal? name (obj-name x))
                                             (or (equal? 'var (obj-kind x))
                                                 (equal? 'parm (obj-kind x))))
                                        x)
                                       ((and (equal? name (obj-name x))
                                             (equal? 'fun (obj-kind x))) 
                                        (error "ERROR SAME NAME VAR AND FUNCTION " name))
                                       (else (obj 'invalid 'invalid 'invalid 'invalid)))
                                 (error "ERROR AN UNDEFINED IDENTIFIER OF VAR " name)))
                           env))))
  (if (equal? 'invalid (obj-type referred-obj))
      (error "ERROR! INVALID IDENTIFIER" name)
      referred-obj)))

(define (check-func-ref st lev env)
  (let* ((name (stx:func_st-name st))
         (referred-obj 
          (lookup-env name 
                      (map (lambda (x) (if (in-env? name env)
                                           (cond ((and (equal? name (obj-name x))
                                                       (or (equal? 'fun (obj-kind x))
                                                           (equal? 'proto (obj-kind x))))
                                                  x)
                                                 ((and (equal? name (obj-name x))
                                                       (equal? 'var (obj-kind x))) 
                                                  (error "ERROR AN UNDEFINED IDENTIFIER " name))
                                                 (else (obj 'invalid 'invalid 'invalid 'invalid)))
                                           (error "ERROR AN UNDEFINED IDENTIFIER OF FUNCTION" name)))
                           env))))
    (if (equal? 'invalid (obj-type referred-obj))
        (error "ERROR! INVALID IDENTIFIER" name)
        referred-obj)))

;comp-env内のみで二重定義などが無いかどうかをチェックする.
(define (check-comp-env comp-env)
  ;(display (format "~a\n" comp-env))
  (cond ((equal? 'nodecl comp-env)
         (display (format "OK! NO DECLARATIONS IN COMPONUND STATEMENT\n")))
        ((equal? '() (cdr comp-env)) 
         (display (format "OK! CORRENCT DECLARATIONS IN COMPONUND STATEMENT!\n")))
        (else (cond ((in-env? (obj-name (car comp-env)) (cdr comp-env))
                     (error "ERROR! REDEFINITION OF "(obj-name (car comp-env))))
                    (else (check-comp-env (cdr comp-env)))))))
         




;;;;;;;;テスト;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(
   (define test101
     (list (obj 'a 1 'parm 'int) (obj 'b 1 'parm '(pointer int)) (obj 'c 1 'parm 'int)))
   (define test102
     (list (obj 'a 1 'parm 'int) (obj 'a 1 'parm '(pointer int)) (obj 'c 1 'parm 'int)))
   (check-proto-para test101)
   ;下はエラー発生
   ;(check-proto-para test102)
   
   (define env2 initial-env)
   (define a (obj 'name1 'lev1 'kind1 'int))
   (define b (obj 'name2 'lev2 'kind2 'void))
   (define c (obj 'name2 'lev3 'kind3 'void))
   (set! env2 (extend-env a env2))
   (set! env2 (extend-env b env2))
   (set! env2 (extend-env c env2))
   ;(set! env (add-list (list a b) env))
   env2
   (in-env? 'name1 env2)
   (in-env? 'x env2)
   (lookup-env 'x env2)
   (lookup-env 'name2 env2)
   (define test (list 'a (list 'b 'c 'd) (list 'e 'f) 'g))
   (define test103 (map make-list-list test))
   (separate-list test103)
   )





