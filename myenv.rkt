#lang racket
(provide (all-defined-out))
;環境操作関数.
(struct obj (name lev kind type)#:transparent)
(define initial-env '())

(define (lookup-env name e)
  (if (eq? e initial-env) 
      #f
      (if (equal? (obj-name (car e)) name)
          (car e)
          (lookup-env name (cdr e)))))

(define (in-env? name e)
  (if (eq? e initial-env) 
      #f
      (if (equal? (obj-name (car e)) name)
          #t
          (in-env? name (cdr e)))))

(define (extend-env x e)
  (cons x e))

(define (add-list l e)
  (if (eq? l '()) 
      e
      (let* ((newenv (extend-env (car l) e)))
        (add-list (cdr l) newenv))))

;例) '((a) (b c d) (e f) (g)) -> '(a b c d e f g)
(define (separate-list l)
  (cond 
    ((eq? '() (cdr l)) (car l))
    ((eq? '() (cddr l)) (append (car l) (cadr l)))
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
(define (check-proto-para obj-list)
  (cond 
    ((eq? '() obj-list) (format "OK! CRRECT PARAMETER OF FUNCTION PROTOTYPE"))
    ((eq? 'nopara obj-list) (format "OK! CRRECT PARAMETER OF FUNCTION PROTOTYPE"))
    ((in-env? (obj-name (car obj-list)) (cdr obj-list))
     (error "FAILED! SOME REDEFINITION IN PARAMETER OF FUNCTION PROTOTYPE"))
    (else (check-proto-para (cdr obj-list)))))
(define (check-def-para obj-list)
   (cond 
    ((eq? '() obj-list) (format "OK! CRRECT PARAMETER OF FUNCTION PROTOTYPE"))
    ((eq? 'nopara obj-list) (format "OK! CRRECT PARAMETER OF FUNCTION PROTOTYPE"))
    ((in-env? (obj-name (car obj-list)) (cdr obj-list))
     (error "FAILED! SOME REDEFINITION IN PARAMETER OF FUNCTION PROTOTYPE"))
    (else (check-proto-para (cdr obj-list)))))


;テスト
(define test101
  (list (obj 'a 1 'parm 'int) (obj 'b 1 'parm '(pointer int)) (obj 'c 1 'parm 'int)))
(define test102
  (list (obj 'a 1 'parm 'int) (obj 'a 1 'parm '(pointer int)) (obj 'c 1 'parm 'int)))
(check-proto-para test101)
;下はエラー発生
;(check-proto-para test102)

#;(
(define env initial-env)
(define a (obj 'name1 'lev1 'kind1 'int))
(define b (obj 'name2 'lev2 'kind2 'void))
(set! env (extend-env a env))
env
(set! env (extend-env b env))
env
(set! env (add-list (list a b) env))
env
(in-env? 'name1 env)
(in-env? 'x env)
(lookup-env 'x env)
(lookup-env 'name2 env)
(define test (list 'a (list 'b 'c 'd) (list 'e 'f) 'g))
(define test2 (map make-list-list test))
(separate-list test2)
)




