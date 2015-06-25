#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))

;構造体の型の参照は(stx:spec_st-type s)

(define env 'empty)
;大域変数にするとcompoundstatementを出たときにenvを捨てられないのであとで修正する
;必要あり!!!!!!!!!!

;(struct obj (name lev kind type)#:transparent)
(struct obj (name)#:transparent)
;(struct obj (name lv)#:transparent) 現在作成中.

#;(define (analy-func_declarator_st st)
  (let* ((name (stx:func_declarator_st-name st)))
    ;(env:extend-env (obj name env lv st) env)
    (set! env (env:extend-env (obj name) env))
    ))

#;(define (analy-func_declarator_null_st st)
  (let* ((name (stx:func_declarator_null_st-name st)))
    ;(env:extend-env (obj name env lv st) env)
    (set! env (env:extend-env (obj name) env))
    ))

#;(define (analy-func_declarator_ast_st st)
  (let* ((name (stx:func_declarator_ast_st-name st)))
    ;(env:extend-env (obj name env lv st) env)
    (set! env (env:extend-env (obj name) env))
    ))

#;(define (analy-func_declarator_ast_null_st st)
  (let* ((name (stx:func_declarator_ast_null_st-name st)))
    ;(env:extend-env (obj name env lv st))
    (set! env (env:extend-env (obj name) env))
    ))


(define (analy-func_proto_st st)
  (let* ((proto-name (cond ((stx:func_declarator_st? 
                       (stx:func_proto_st-func-declarator-st st)) 
                      (stx:func_declarator_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     
                     ((stx:func_declarator_null_st? 
                       (stx:func_proto_st-func-declarator-st st))
                      (stx:func_declarator_null_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     
                     ((stx:func_declarator_ast_st? 
                       (stx:func_proto_st-func-declarator-st st)) 
                      (stx:func_declarator_ast_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     
                     ((stx:func_declarator_ast_null_st? 
                       (stx:func_proto_st-func-declarator-st st)) 
                      (stx:func_declarator_ast_null_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     ))
         (proto-lev 0)
         (proto-kind 'proto)
         (proto-type "under const"))
    (set! env (env:extend-env (obj proto-name proto-lev proto-kind proto-type) env))))



(define (analy-func_def_st st)
  (let* ((func-name (cond ((stx:func_declarator_st? 
                            (stx:func_def_st-func-declarator-st st)) 
                           (stx:func_declarator_st-name 
                            (stx:func_def_st-func-declarator-st st)))
                          
                          ((stx:func_declarator_null_st? 
                            (stx:func_def_st-func-declarator-st st))
                           (stx:func_declarator_null_st-name 
                            (stx:func_def_st-func-declarator-st st)))
                          
                          ((stx:func_declarator_ast_st? 
                            (stx:func_def_st-func-declarator-st st)) 
                           (stx:func_declarator_ast_st-name 
                            (stx:func_def_st-func-declarator-st st)))
                          
                          ((stx:func_declarator_ast_null_st? 
                            (stx:func_def_st-func-declarator-st st)) 
                           (stx:func_declarator_ast_null_st-name 
                            (stx:func_def_st-func-declarator-st st)))
                          ))
         (func-lev 0)
         (func-kind 'fun)
         (func-type "under const"))
    (set! env (env:extend-env (obj func-name func-lev func-kind func-type) env))))


#;(define (analy-declarator_st st)
    (let* ((name (stx:id_st-name (stx:declarator_st-var st))))
      (set! env (env:extend-env (obj name) env))
      ))

#;(define (analy-declarator_ast_st st)
    (let* ((name (stx:id_st-name (stx:declarator_st-var st))))
      (set! env (env:extend-env (obj name) env))
      ))

#;(define (analy-id_st st)
  (let* ((name (stx:id_st-name st)))
    (set! env (env:extend-env (obj name) env))
    ))

#;(define (analy-id_ast_st st)
  (let* ((name (stx:id_ast_st-name st)))
    (set! env (env:extend-env (obj name) env))
    ))

#;(define (analy-array_st st)
  (let* ((name (stx:array_st-name env st)))
    (set! env (env:extend-env (obj name) env))
    ))

#;(define (analy-compound_st st)
  (cond ((and (cons? (stx:compound_st-declaration-list st)) 
              (cons? (stx:compound_st-statement-list st)))
         (cons (analy-declaration-list st)
               (analy-statement-list st)))
        
        ((and (struct? (stx:compound_st-declaration-list st)) 
              (cons? (stx:compound_st-statement-list st)))
         (cons (analy-declaration_st st)
               (analy-statement-list st)))
        
        ((and (struct? (stx:compound_st-declaration-list st))
              (struct? (stx:compound_st-statement-list st)))
         (cons (analy-declaration_st st)
               (analy-statement st)))
        
        ((and (cons? (stx:compound_st-declaration-list st))
              (struct? (stx:compound_st-statement-list)))
         (cons (analy-declaration-list st)
               (analy-statement st)))))

#;(define (analy-compound_dec_st st)
  (cond ((cons? (stx:compound_dec_st-declaration-list st)) 
         (analy-declaration-list (stx:compound_dec_st-declaration-list st)))
        ((struct? (stx:compound_dec_st-declaration-list st)) 
         (analy-declaration_st (stx:compound_dec_st-declaration-list st)))))


(define (analy-declaration_st st)
  (let* ((name (extract-name-from-declarator_st (stx:declaration_st-declarator-list st))))
        (set! env (env:extend-env (obj name) env))))

;declarator_stもしくはdeclarator_ast_st
;およびそれらのlist*からvarを取り出す関数
;このとき返されるのはvarの名前のlist*になっていることに注意
(define (extract-name-from-declarator_st st)
  (cond ((struct? st)
         (cond ((stx:declarator_st? st) (stx:id_st-name (stx:declarator_st-var st)))
               ((stx:declarator_ast_st? st) (stx:id_st-name (stx:declarator_ast_st-var st)))))
        ((cons? st)
         (cons (extract-name-from-declarator_st (car st))
               (extract-name-from-declarator_st (cdr st))))))
                                                

    


;(define (analy-declaration-list st) #t)
;(define (analy-statement st) #t)
;(define (analy-statement-list st) #t)

        

;構文木を引数に取りその意味解析を行う関数
;構文木は一番外側から見てlist*になっているものと 
;何らかの構造体になっているものに分けられる.
(define (analyze-tree t)
  (cond ((cons? t) (cons (analyze-st (car t)) 
                         (analyze-tree (cdr t))))
        ((struct? t) (analyze-st t))
        (else "ERROR! WRONG TREE?")))


;構造体の外側になりうるものを判別
;;は環境の参照が必要になる構造体
(define (analyze-st st)
  (cond ;((stx:func_declarator_st? st) #t);
        ;((stx:func_declarator_null_st? st) #t);
        ;((stx:func_declarator_ast_st? st) #t);
        ;((stx:func_declarator_ast_null_st? st) #t);
        ;((stx:func_proto_st? st) (analy-func_proto_st st))
        ((stx:func_def_st? st) (analy-func_def_st st))
        ((stx:declaration_st? st) (analy-declaration_st st))
        ;((stx:func_declarator_ast_st? st) #t);
        ;((stx:para_declaration_st? st) #t);
        ((stx:exp_st? st) #t);;
        ((stx:assign_exp_st? st) #t);;
        ((stx:logic_exp_st? st) #t);;
        ((stx:rel_exp_st? st) #t);;
        ((stx:alge_exp_st? st) #t);;
        ((stx:id_st? st) #t);;
        ((stx:id_ast_st? st) #t);;
        ((stx:array_st? st) #t);;
        ((stx:array_var_st? st) #t);;
        ((stx:spec_st? st) #t);;
        ((stx:unary_exp_st? st) #t);;
        ((stx:constant_st? st) #t);;
        ((stx:null_statement_st? st) #t);;
        ((stx:exp_with_semi_st? st) #t);;
        ((stx:exp_in_paren_st? st) #t);;
        ((stx:if_st? st) #t);;
        ((stx:if_else_st? st) #t);;
        ((stx:while_st? st) #t);;
        ((stx:for_0_st? st) #t);;
        ((stx:for_1_st? st) #t);;
        ((stx:for_2_st? st) #t);;
        ((stx:for_3_st? st) #t);;
        ((stx:for_4_st? st) #t);;
        ((stx:for_5_st? st) #t);;
        ((stx:for_6_st? st) #t);;
        ((stx:for_7_st? st) #t);;
        ((stx:return_st? st) #t);;
        ((stx:return_null_st? st) #t);;
        ((stx:compound_st? st) #t)
        ((stx:compound_dec_st? st) #t)
        ((stx:compound_sta_st? st) #t)
        ((stx:compound_null_st? st) #t)
        ((stx:func_st? st) #t);;
        ((stx:func_nopara_st? st) #t)));;
        
        
        
               
;テストf
(define p (open-input-file "kadai01.c"))
(port-count-lines! p)

(define t (k08:parse-port p))
t
;(car (k08:parse-port p))
;(cdr (k08:parse-port p))
(analyze-tree t)
;(cons? (k08:parse-port p))
env


