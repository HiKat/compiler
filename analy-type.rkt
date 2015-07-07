#lang racket
(require (prefix-in k08: "kadai08.rkt"))
(require (prefix-in sem: "semantic-analy.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "myenv.rkt")
(provide (all-defined-out))
;myenv.rkt内で定義
;(struct obj (name lev kind type)#:transparent)

(define (analy-type t)
  (begin (map check-type t)
         (display "OK! THIS PROGRAM IS WELL TYPED.")))

;引数は構造体
;戻り値は'well-typed
(define (check-type st)
  (cond 
    ((list? st) (map check-type st))
    ((stx:declaration_st? st) 
     (let* ((decl-obj-list (stx:declaration_st-declarator-list st)))
       (map (lambda (x) 
              (cond 
                ;宣言した変数が配列の時
                ;void型、voidポインタ型はエラー
                ((type_array? (obj-type x))
                 (cond ((or (equal? 'void 
                                    (type_array-type (obj-type x)))
                            (equal? (type_pointer 'pointer 'void) 
                                    (type_array-type (obj-type x))))
                        (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                       (obj-name x)(obj-pos x))))))
                ;宣言した変数が配列でない時
                ;void型、voidポインタ型はエラー
                (else (cond ((equal? (type_pointer 'pointer 'void) (obj-type x))
                             (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                            (obj-name x)(obj-pos x))))
                            ((equal? 'void (obj-type x))
                             (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                            (obj-name x)(obj-pos x))))
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
         ((equal? (type_pointer 'pointer 'void )
                  func-out-type)
          (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                         (obj-name func-obj)(obj-pos func-obj))))
         ;パラメターががvoid型、voidポインタ型であるとき
         ;chcek-type-paraはこれらの型の以上がパラメータの中に無いかどうかを判定する.
         ((equal? 'well-typed (check-type-para func-para-list))
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
                                  (stx:func_def_st-compound-state-list st)))
            (func-compound-decl (stx:compound_st-declaration-list 
                                 (stx:func_def_st-compound-state-list st))))
       (cond ((and (equal? 'well-typed 
                           (cond ((equal? (type_pointer 'pointer 'void )
                                          func-out-type)
                                  (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                                 (obj-name func-obj)(obj-pos func-obj))))
                                 ((equal? 'well-typed (check-type-para func-para-list))
                                  'well-typed)
                                 (else 'well-typed)))
                   ;begin文の前者がうまく実行されれば#tが出力される.
                   ;そうでなければ勝手にエラーで止まる.
                   (begin
                     (cond ((equal? 'nodecl func-compound-decl) 'well-typed)
                           (else (map check-type func-compound-decl)))
                     (cond ((equal? 'nostat func-compound-state) 'well-typed)
                           (else (map check-type func-compound-state)))
                     #t))
              'well-typed)
             (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                  (obj-name func-obj)(obj-pos func-obj)))))))
    ((stx:null_statement_st? st) 'well-typed)       
    ((stx:exp_in_paren_st? st) 
     (check-type (stx:exp_in_paren_st-exp st)))
    ((stx:sem_return_st? st) 
     (cond ((equal? (stx:sem_return_st-exp st) 'noreturn) 
            'well-typed)
           ((type-void? (stx:sem_return_st-exp st))
            (error (format "ERROR NOT WELL TYPED RETURN-STATEMENT AT ~a" 
                           (stx:sem_return_st-pos st))))
           ((and (type-int? (stx:sem_return_st-exp st))
                 (equal? 'int (type_fun-out (obj-type (stx:sem_return_st-tag st)))))
            'well-typed)
           ((and (type-intp? (stx:sem_return_st-exp st))
                 (equal? (type_pointer 'pointer 'int) 
                         (type_fun-out (obj-type (stx:sem_return_st-tag st)))))
            'well-typed)
           ((and (type-intp? (stx:sem_return_st-exp st))
                 (equal? (type_pointer 'pointer 'int) 
                         (type_fun-out (obj-type (stx:sem_return_st-tag st)))))
            'well-typed)
           (else (error (format "ERROR NOT WELL TYPED RETURN-STATEMENT AT ~a" 
                                (stx:sem_return_st-pos st))))))
    ((stx:if_else_st? st)
     (cond ((and (type-int? (stx:if_else_st-cond-exp st)) 
                 (equal? 'well-typed (check-type (stx:if_else_st-state st)))
                 (equal? 'well-typed (check-type (stx:if_else_st-else-state st))))
            'well-typed)
           (else (error (format "ERROR NOT WELL TYPED IF-STATEMENT AT ~a" 
                                (stx:if_else_st-if-pos st))))))
    ((stx:while_st? st) 
     (cond ((and (type-int? (stx:while_st-cond-exp st))
                 (equal? 'well-typed (check-type (stx:while_st-statement st))))
            'well-typed)
           (else (error (format "ERROR NOT WELL TYPED WHILE-STATEMENT AT ~a" 
                                (stx:while_st-pos st))))))
    ((stx:compound_st? st) 
     (begin 
       (cond ((equal? 'nostat (stx:compound_st-statement-list st)) 'well-typed)
             (else (map check-type (stx:compound_st-statement-list st))))
       (cond ((equal? 'nodecl (stx:compound_st-declaration-list st)) 'well-typed)
             (else (map check-type (stx:compound_st-declaration-list st))))
       ;各要素はerrorか'well-typedを返すので、mapが実行されれば
       ;必然的にlistの要素は'well-typedになっている.
       'well-typed))
    (else (cond ((or (equal? 'int (type st))
                     (equal? (type_pointer 'pointer 'int) (type st))
                     (equal? (type_pointer 'pointer (type_pointer 'pointer 'int)) (type st))
                     (equal? 'void (type st)))
                 'well-typed)
                (else (error "ERROR NOT WELL TYPED" st))))))


;型は'int、(type_pointer 'pointer 'int)、(type_pointer 'pointer (type_pointer 'pointer 'int))
(define (sametype? x y)
  (equal? (type x) (type y)))
(define (type-int? x) 
  (equal? 'int (type x)))
(define (type-intp? x)
  (equal? (type_pointer 'pointer 'int) (type x)))
(define (type-intpp? x)
  (equal? (type_pointer 'pointer (type_pointer 'pointer 'int)) (type x)))
(define (type-void? x)
  (equal? 'void (type x)))  

;objのlistもしくは(list 'nopara)を受け取って
;エラーすなわち
;パラメターの中にvoid型、voidポインタ型がなければ
;well-typedを返す.
(define (check-type-para para-list) 
  (cond ((equal? 'nopara para-list) 'well-typed)
        (else (map (lambda (x) 
                     (let ((x-type (obj-type x)))
                       (begin (cond ((or (equal? 'void x-type)
                                         (equal? (type_pointer 'pointer 'void) x-type))
                                     (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                                    (obj-name x)(obj-pos x))))
                                    (else 'well-typed))
                              'well-typed)))
                   para-list))))

(define (type st) 
  (cond 
    ((stx:sem_return_st? st) (type (stx:sem_return_st-exp st)))
    ((stx:exp_in_paren_st? st) (type (stx:exp_in_paren_st-exp st)))
    ((stx:assign_exp_st? st) 
     (let* ((type-dest (type (stx:assign_exp_st-dest st)))
            (type-src (type (stx:assign_exp_st-src st))))
       (cond ((equal? type-dest type-src)
              type-dest)
             (else (error (format "ERROR NOT WELL TYPED '=' AT ~a ~a" 
                                  (stx:assign_exp_st-pos st) st))))))
    ((stx:logic_exp_st? st) 
     (let* ((type-op1 (type (stx:logic_exp_st-op1 st)))
            (type-op2 (type (stx:logic_exp_st-op2 st))))
       (cond ((and (type-int? type-op1) 
                   (type-int? type-op2))
              'int)
             (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                  (cond ((equal? 'or (stx:logic_exp_st-log-ope st)) '||)
                                        ((equal? 'or (stx:logic_exp_st-log-ope st)) '&&))
                                  (stx:logic_exp_st-pos st)))))))
    
    ((stx:rel_exp_st? st) 
     (let* ((type-op1 (type (stx:rel_exp_st-op1 st)))
            (type-op2 (type (stx:rel_exp_st-op2 st))))
       (cond ((equal? type-op1 type-op2)
              'int)
             (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                  (cond ((equal? 'equal (stx:rel_exp_st-rel-ope st)) '==)
                                        ((equal? 'not (stx:rel_exp_st-rel-ope st)) '!=)
                                        ((equal? 'less (stx:rel_exp_st-rel-ope st)) '<)
                                        ((equal? 'and_less (stx:rel_exp_st-rel-ope st)) '<=)
                                        ((equal? 'more (stx:rel_exp_st-rel-ope st)) '>)
                                        ((equal? 'and_more (stx:rel_exp_st-rel-ope st)) '>=))
                                  (stx:rel_exp_st-pos st)))))))
    ((stx:alge_exp_st? st) 
     (let* ((type-op1 (stx:alge_exp_st-op1 st))
            (type-op2 (stx:alge_exp_st-op2 st))
            (ope (stx:alge_exp_st-alge-ope st))
            (pos (stx:alge_exp_st-pos st)))
       (cond ((equal? 'add ope) 
              (cond ((and (type-int? type-op1)
                          (type-int? type-op2))
                     'int)
                    ((and (type-intp? type-op1)
                          (type-int? type-op2))
                     (type_pointer 'pointer 'int))
                    ((and (type-int? type-op1)
                          (type-intp? type-op2))
                     (type_pointer 'pointer 'int))
                    ((and (type-intpp? type-op1)
                          (type-int? type-op2))
                     (type_pointer 'pointer (type_pointer 'pointer 'int)))
                    ((and (type-int? type-op1)
                          (type-intpp? type-op2))
                     (type_pointer 'pointer (type_pointer 'pointer 'int)))
                    (else (error (format "ERROR NOT WELL TYPED '+' AT ~a" pos)))))
             ((equal? 'sub ope) 
              (cond ((and (type-intp? type-op1)
                          (type-int? type-op2))
                     (type_pointer 'pointer 'int))
                    ((and (type-intpp? type-op1)
                          (type-int? type-op2))
                     (type_pointer 'pointer (type_pointer 'pointer 'int)))
                    ((and (type-int? type-op1)
                          (type-int? type-op2)) 'int)
                    (else (error (format "ERROR NOT WELL TYPED '-' AT ~a" pos)))))
             ((or (equal? 'mul ope) 
                  (equal? 'div ope)) 
              (cond ((and (type-int? type-op1)
                          (type-int? type-op2))
                     'int)
                    (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a" 
                                         (cond ((equal? 'mul ope) '*)
                                               ((equal? 'div ope) '/))
                                         pos)))))
             ;デバグ用.（本来はどんなプログラムを書いてもこの分岐には入らないはず.）
             (else (error "ERROR NOT WELL TYPED" st)))))
    ((stx:unary_exp_st? st) 
     (let* ((mark (stx:unary_exp_st-mark st))
            (op (stx:unary_exp_st-op st))
            (type-op (type op))
            (pos (stx:unary_exp_st-pos st)))
       (cond ((equal? 'amp mark)
              (cond ((equal? 'int op) 'int)
                    (else (error (format "ERROR NOT WELL TYPED '&' AT ~a" pos)))))
             ((equal? 'ast mark)
              (cond ((equal? (type_pointer 'pointer 'int) type-op) 'int)
                    ((equal? (type_pointer 'pointer (type_pointer 'pointer 'int)) type-op) 
                     (type_pointer 'pointer 'int))
                    (else (error (format "ERROR NOT WELL TYPED '*' AT ~a" st)))))
             ;デバグ用.(本来はどんなプログラムを書いてもこの分岐には入らないはず.)
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
            (func-para (stx:func_st-para st))
            (pos (obj-pos func-ref))
            (func-name (obj-name func-ref)))
       (cond ((equal? (map (lambda (x) 
                             (cond 
                               ((equal? 'nopara x) 'nopara)
                               (else (type x))))
                           func-para)
                      func-in-list)
              (cond 
                ;関数の戻り値として許されるのは
                ;int、intのポインタ型、void型
                ((equal? 'int func-out) 'int)
                ((equal? 'void func-out) 'void)
                ((equal? (type_pointer 'pointer 'int) func-out) (type_pointer 'pointer 'int))
                (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a"
                                     func-name pos)))))
             (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a"
                                  func-name pos))))))
    ((obj? st) 
     (let* ((type-obj (obj-type st)))
       (cond 
         ;配列型のとき
         ((type_array? type-obj) 
          ;ポインタ型として許されるのはintのみ
          (cond ((equal? 'int (type_array-type type-obj)) 'int)
                (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a"
                                     (obj-name st) (obj-pos st))))))
         ;配列型でないとき
         ;許されるのはint、intのポインタ型のみ
         (else 
          (cond ((equal? 'int type-obj) 'int)
                ((equal? (type_pointer 'pointer 'int) type-obj) (type_pointer 'pointer 'int))
                (else (error (format "ERROR NOT WELL TYPED '~a' AT ~a"
                                     (obj-name st) (obj-pos st)))))))))))





;テスト
;(define p100 (open-input-file "kadai01.c"))
;(port-count-lines! p100)
;;(sem:sem-analyze-tree (k08:parse-port p100))
;(analy-type (sem:sem-analyze-tree (k08:parse-port p100)))