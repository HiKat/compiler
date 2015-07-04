#lang racket
(require (prefix-in k08: "kadai08.rkt"))
(require (prefix-in sem: "semantic-analy.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "myenv.rkt")
;myenv.rkt内で定義
;(struct obj (name lev kind type)#:transparent)

(define (analy-type t)
  (begin (map check-type t)
         (display "OK! THIS PROGRAM IS WELL TYPED.")))

;引数は構造体
;戻り値は'well-typed
(define (check-type st)
  (cond 
    
    ((stx:declaration_st? st) 
     (let* ((decl-obj-list (stx:declaration_st-declarator-list st)))
       (map (lambda (x) 
              (cond 
                ;宣言した変数が配列の時
                ;void型、voidポインタ型はエラー
                ((type_array? (obj-type x))
                 (cond ((or (eq? 'void 
                                 (type_array-type (obj-type x)))
                            (eq? (type_pointer 'pointer 'void) 
                                 (type_array-type (obj-type x))))
                        (error "ERROR NOT WELL TYPED" st))))
                ;宣言した変数が配列でない時
                ;void型、voidポインタ型はエラー
                (else (cond ((eq? (type_pointer 'pointer 'void) (obj-type x))
                             (error "ERROR NOT WELL TYPED" st))
                            ((eq? 'void (obj-type x)))
                            (else 'well-typed)))))
            decl-obj-list)
       'well-typed))
    ((stx:func_proto_st? st) 
     (let* ((func-declarator (stx:func_proto_st-func-declarator-st st))
            (func-para-list (stx:func_declarator_st-para-list func-declarator))
            (func-obj (stx:func_declarator_st-name func-declarator))
            (func-type (obj-type func-obj))
            (func-out-type (type_fun-out func-type)))
     ;関数プロトタイプの
     ;戻り値がvoidポインタはエラー
     ;パラメータがvoid型、voidポインタはエラー
       (cond ;戻り値がvoidのポインタ型であるとき
         ((eq? (type_pointer 'pointer 'void )
               func-out-type)
          (error "ERROR NOT WELL TYPED" st))
         ;パラメターががvoid型、voidポインタ型であるとき
         ;chcek-type-paraはこれらの型の以上がパラメータの中に無いかどうかを判定する.
         ((eq? 'well-typed (check-type-para func-para-list))
          'well-typed)
         (else 'well-typed))))                                  
    ((stx:func_def_st? st) 
     ;戻り値がvoidポインタはエラー
     ;パラメータがvoid型、voidポインタはエラー
     (let* ((func-declarator (stx:func_def_st-func-declarator-st st))
            (func-para-list (stx:func_declarator_st-para-list func-declarator))
            (func-obj (stx:func_declarator_st-name func-declarator))
            (func-type (obj-type func-obj))
            (func-out-type (type_fun-out func-type))
            (func-compound-state (stx:compound_st-statement-list 
                                 (stx:func_def_st-compound-state-list st))))
       (cond ((and (eq? 'well-typed 
                        (cond ((eq? (type_pointer 'pointer 'void )
                                    func-out-type)
                               (error "ERROR NOT WELL TYPED" st))
                              ((eq? 'well-typed (check-type-para func-para-list))
                               'well-typed)
                              (else 'well-typed)))
                   ;begin文の前者がうまく実行されれば#tが出力される.
                   ;そうでなければ勝手にエラーで止まる.
                   (begin (map check-type func-compound-state)
                          #t))
              'well-typed)
             (else (error "ERROR NOT WELL TYPED" st)))))
    ((stx:null_statement_st? st) 'well-typed)       
    ((stx:exp_in_paren_st? st) 
     (check-type (stx:exp_in_paren_st-exp st)))
    ((stx:sem_return_st? st) 
     (cond ((eq? (stx:sem_return_st-exp st) 'noreturn) 
            'well-typed)
           ((type-void? (stx:sem_return_st-exp st))
            (error "ERROR NOT WELL TYPED" st))
           ((and (type-int? (stx:sem_return_st-exp st))
                 (eq? 'int (obj-type (stx:sem_return_st-tag st))))
            'well-typed)
           ((and (type-intp? (stx:sem_return_st-exp st))
                 (eq? (type_pointer 'pointer 'int) 
                      (obj-type (stx:sem_return_st-tag st))))
            'well-typed)
           ((and (type-intp? (stx:sem_return_st-exp st))
                 (eq? (type_pointer 'pointer 'int) 
                      (obj-type (stx:sem_return_st-tag st))))
            'well-typed)))
    ((stx:if_else_st? st)
     (cond ((and (type-int? (stx:if_else_st-cond-exp st)) 
                 (eq? 'well-typed (check-type (stx:if_else_st-state st)))
                 (eq? 'well-typed (check-type (stx:if_else_st-else-state st))))
            'well-typed)
           (else (error "ERROR NOT WELL TYPED" st))))
    ((stx:while_st? st) 
     (cond ((and (type-int? (stx:while_st-cond-exp st))
                 (eq? 'well-typed (check-type (stx:while_st-statement st))))
            'well-typed)
           (else (error "ERROR NOT WELL TYPED" st))))
    ((stx:compound_st? st) 
     (begin (map check-type (stx:compound_st-statement-list st))
            ;各要素はerrorか'well-typedを返すので、mapが実行されれば
            ;必然的にlistの要素は'well-typedになっている.
            'well-typed))
    (else (cond ((or (eq? 'int (type st))
                     (eq? 'int-p (type st))
                     (eq? 'int-pp (type st))
                     (eq? 'void (type st)))
                 'well-typed)
                (else (error "ERROR NOT WELL TYPED" st))))))
    
  
;型は'int、'int-p、int-pp 
(define (sametype? x y)
  (eq? (type x) (type y)))
(define (type-int? x) 
  (eq? 'int (type x)))
(define (type-intp? x)
  (eq? 'int-p (type x)))
(define (type-intpp? x)
  (eq? 'int-pp (type x)))
(define (type-void? x)
  (eq? 'void (type x)))  

;objのlistもしくは(list 'nopara)を受け取って
;エラーすなわち
;パラメターの中にvoid型、voidポインタ型がなければ
;well-typedを返す.
(define (check-type-para para-list) 
  (cond ((eq? 'nopara para-list) 'well-typed)
        (else (map (lambda (x) 
                     (let ((x-type (obj-type x)))
                       (begin (cond ((or (eq? 'void x-type)
                                         (eq? (type_pointer 'pointer 'void) x-type))
                                     (error "ERROR NOT WELL TYPED" x))
                                    (else 'well-typed))
                              'well-typed)))
                   para-list))))

(define (type st) 
  (cond ((stx:assign_exp_st? st) 
         (let* ((type-dest (type (stx:assign_exp_st-dest st)))
                (type-src (type (stx:assign_exp_st-src st))))
           (cond ((eq? type-dest type-src)
                  type-dest)
                 (else (error "ERROR NOT WELL TYPED" st)))))
        ((stx:logic_exp_st? st) 
         (let* ((type-op1 (type (stx:logic_exp_st-op1 st)))
                (type-op2 (type (stx:logic_exp_st-op2 st))))
           (cond ((and (type-int? type-op1) 
                       (type-int? type-op2))
                  'int)
                 (else (error "ERROR NOT WELL TYPED" st)))))
        
        ((stx:rel_exp_st? st) 
         (let* ((type-op1 (type (stx:rel_exp_st-op1 st)))
                (type-op2 (type (stx:rel_exp_st-op2 st))))
           (cond ((eq? type-op1 type-op2)
                  'int)
                 (else (error "ERROR NOT WELL TYPED" st)))))
        ((stx:alge_exp_st? st) 
         (let* ((type-op1 (stx:logic_exp_st-op1 st))
                (type-op2 (stx:logic_exp_st-op2 st))
                (ope (stx:alge_exp_st-alge-ope st)))
           (cond ((eq? 'add ope) 
                  (cond ((and (type-int? type-op1)
                              (type-int? type-op2))
                         'int)
                        ((and (type-intp? type-op1)
                              (type-int? type-op2))
                         'int-p)
                        ((and (type-int? type-op1)
                              (type-intp? type-op2))
                         'int-p)
                        ((and (type-intpp? type-op1)
                              (type-int? type-op2))
                         'int-pp)
                        ((and (type-int? type-op1)
                              (type-intpp? type-op2))
                         'int-pp)
                        (else (error "ERROR NOT WELL TYPED" st))))
                 ((eq? 'sub ope) 
                  (cond ((and (type-intp? type-op1)
                              (type-int? type-op2))
                         'int-p)
                        ((and (type-intpp? type-op1)
                              (type-int? type-op2))
                         'int-pp)
                        (else (error "ERROR NOT WELL TYPED" st))))
                 ((or (eq? 'mul ope) 
                      (eq? 'div ope)) 
                  (cond ((and (type-int? type-op1)
                              (type-int? type-op2))
                         'int)
                        (else (error "ERROR NOT WELL TYPED" st))))
                  (else (error "ERROR NOT WELL TYPED" st)))))
        ((stx:unary_exp_st? st) 
         (let* ((mark (stx:unary_exp_st-mark st))
                (op (stx:unary_exp_st-op st))
                (type-op (type op)))
           (cond ((eq? 'amp mark)
                  (cond ((eq? 'int op) 'int)
                        (else (error "ERROR NOT WELL TYPED" st))))
                 ((eq? 'ast mark)
                  (cond ((eq? 'int-p type-op) 'int)
                        ((eq? 'int-pp type-op) 'int-p)
                        (else (error "ERROR NOT WELL TYPED" st))))
                 (else (error "ERROR NOT WELL TYPED" st)))))
        ((stx:constant_st? st) 'int)
        ((stx:func_st? st) 
         (let* (;funcを表すobj
                (func-ref (stx:func_st-name st))
                ;funcを表すobj中のtype_fun
                (func-type-fun (obj-type func-ref))
                ;funcを表すobj中のtyp_fun内のパラメータの型のlist
                ;もしくは'nopara
                (func-in-list (type_fun-in func-type-fun))
                ;type_fun中の関数の戻り値
                ;(type_pointer 'pointer 'int)か'intか'void
                (func-out (type_fun-out func-type-fun))
                ;パラメータのobjのlistもしくは'nopara
                (func-para (stx:func_st-para st)))
           (cond ((eq? (map (lambda (x) 
                              (cond 
                                ((eq? 'nopara x) 'nopara)
                                ((obj? x) (obj-type x))
                                ((stx:constant_st? x) 'int)
                                (else (error "ERROR! INVALID FUNCTIONS." st))))
                            func-para)
                       func-in-list)
                  (cond 
                    ;関数の戻り値として許されるのは
                    ;int、intのポインタ型、void型
                    ((eq? 'int func-out) 'int)
                    ((eq? 'void func-out) 'void)
                    ((eq? (type_pointer 'pointer 'int) func-out) 'int-p)
                    (else (error "ERROR NOT WELL TYPED" st))))
                 (else (error "ERROR NOT WELL TYPED" st)))))
        ((obj? st) 
         (let* ((type-obj (obj-type st)))
           (cond 
             ;配列型のとき
             ((type_array? type-obj) 
              ;ポインタ型として許されるのはintのみ
              (cond ((eq? 'int (type_array-type type-obj)) 'int-p)
                    (else (error "ERROR NOT WELL TYPED" st))))
             ;配列型でないとき
             ;許されるのはint、intのポインタ型のみ
             ((not (type_array? type-obj)) 
              (cond ((eq? 'int type-obj) 'int)
                    ((eq? (type_pointer 'pointer 'int) type-obj) 'int-p)
                    #;(else (begin 
                            (display (format "~a ========== ~a\n" type-obj (type_pointer 'pointer 'int)))
                            (if (symbol? (type_pointer-type type-obj)) 
                                (display "なんでやねん1")  
                                (display "なんでやねん") )
                            (error "ERROR NOT WELL TYPED" type-obj))
                     ;(eq? (type_pointer 'pointer 'int) type-obj)
                     )
                    )))))))




  
;テスト
(define p (open-input-file "test01.c"))
(port-count-lines! p)
;(sem:sem-analyze-tree (k08:parse-port p))
(analy-type (sem:sem-analyze-tree (k08:parse-port p)))