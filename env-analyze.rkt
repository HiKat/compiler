#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))

;構造体の型の参照は(stx:spec_st-type s)

(define env 'empty)
;大域変数にするとcompoundstatementを出たときにenvを捨てられないのであとで修正する
;必要あり!!!!!!!!!!

(define current-lev 0)
;意味解析を実行中の関数のレベルを入れる変数.

;(struct obj (name lev kind type)#:transparent)
(struct obj (name)#:transparent)
;(struct obj (name lv)#:transparent) 現在作成中.


;obj構造体のtypeの要素になりうる構造体.
(struct type-pointer (pointer type))
(struct array (type size))


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
  ;内部定義
  ;(func_proto_st-func-declarator-st st)
  ;を受け取ってを引数の型のlist*か
  ;引数の型の単体を返す関数か
  ;シンボル'noparameter
  ;を返す関数.
  (define (analy-para_declaration-list st)
    (let*((declarator (stx:func_proto_st-func-declarator-st st))
          (parameter-list 
           ;parameter-listに入るのは'noparameterか
           ;(list* pada_declaration_st ,,,)か
           ;para_declaration_st単体
           ;あとでここから型を抜き出す必要がある.
           ;型を抜き出す際に注意すべきは
           ;para_declaration-paraが
           ;id_stかid_ast_stかを調べる必要がある.
           (cond ((stx:func_declarator_st? declarator) 
                  (stx:func_declarator_st-para-list declarator))
                 
                 ((stx:func_declarator_null_st? declarator) 'noparameter)
                 
                 ((stx:func_declarator_ast_st? declarator) 
                  (stx:func_declarator_ast_st-para-list declarator))
                 
                 ((stx:func_declarator_ast_null_st? declarator) 'noparameter))))
      (cond ((eq? parameter-list 'noparameter) 'noparameter)
            (else (extract-type-from-para parameter-list)))))
  ;para_declaration_stもしくはそのlist*を受け取って
  ;型のlist*を作成する関数の内部定義
  (define (extract-type-from-para parameter-list)
    (cond ((struct? parameter-list) 
           (cond ((stx:id_ast_st? (stx:para_declaration_st-para parameter-list))
                  (type-pointer 'pointer 
                                (stx:spec_st-type (stx:para_declaration_st-type-spec parameter-list))))
                 ((stx:id_st? (stx:para_declaration_st-para parameter-list)) 
                  (stx:spec_st-type (stx:para_declaration_st-type-spec parameter-list)))))
          ((cons? parameter-list) 
           (cons (extract-type-from-para (car parameter-list))
                 (extract-type-from-para (cdr parameter-list))))))
      ;ここまで内部定義
  (let* ((name (cond ((stx:func_declarator_st? 
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
         (lev 0)
         (kind 'proto)
         ;func_typeは関数の戻り値の型
         (func-type
          (cons 'fun 
                (if (stx:func_declarator_ast_st? (stx:func_proto_st-func-declarator-st st))
                    (type-pointer 'poiner (stx:spec_st-type (stx:func_proto_st-type-spec st)))
                    (stx:spec_st-type (stx:func_proto_st-type-spec st)))))
         (func-inputtype (analy-para_declaration-list 
                          (stx:func_proto_st-func-declarator-st st)))
         (type (cons func-type func-inputtype)))
         (set! current-lev 0)
         (set! env (env:extend-env (obj name lev kind type) env))))



  
  
(define (analy-func_def_st st)
  (let* ((name (cond ((stx:func_declarator_st? 
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
                            (stx:func_def_st-func-declarator-st st)))))
         (lev 0)
         (kind 'fun)
         (type #t))
    (set! current-lev 1)
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
  ;内部定義
  ;関数separate-nameはextract-name-from-declarator_stの返り値
  ;nameがlist*の形であった時にそれを分解して残りの情報を付加したobjを生成する関数.
  (define (separate-name name lev kind type)
    (cond ((cons? name) (let* ((meaningless 1)) 
                          (separate-name (car name) lev kind type)
                          (set! env (env:extend-env (obj (cdr name) lev kind type) env))))
          (else (set! env (env:extend-env (obj name lev kind type) env)))))
  ;ここまで内部定義
  (let* ((name (extract-name-from-declarator_st (stx:declaration_st-declarator-list st)))
         (lev current-lev);大域変数もしくはcompound-statement内のどちらか
         (kind 'var)
         (type (stx:spec_st-type (stx:declaration_st-type-spec st))))
    (separate-name name)
    ;(set! env (env:extend-env (obj name lev kind type) env))
    ))


        

;declarator_stもしくはdeclarator_ast_st
;およびそれらのlist*からvarを取り出す関数
;このとき返されるnameはvarの名前のlist*になっていることに注意
(define (extract-name-from-declarator_st st)
  (cond ((struct? st)
         (cond ((stx:declarator_st? st) (stx:id_st-name (stx:declarator_st-var st)))
               ((stx:declarator_ast_st? st) (stx:id_st-name (stx:declarator_ast_st-var st)))))
        ((cons? st)
         (cons (extract-name-from-declarator_st (car st))
               (extract-name-from-declarator_st (cdr st))))))

;テスト
;(require (prefix-in env: "myenv.rkt"))
;(require (prefix-in stx: "mysyntax.rkt"))
;(require (prefix-in k08: "kadai08.rkt"))
#;(extract-name-from-declarator_st
   (cons
    (cons
     (stx:declarator_st (stx:id_st 'j 'test))
     (stx:declarator_st (stx:id_st 'k 'test)))
    (stx:declarator_st (stx:id_st 'l 'test))))


;function_de_stのcompound_stを意味解析する関数.
;このcompound_st内にはmysyntax.rktの4種類のcompound_stが入ることに注意.
(define (analy-all_compound_st st)
  (cond ((stx:compound_st? st) (analy-compound_st st))
        ((stx:compound_dec_st? st) (analy-compound_dec_st st))
        ((stx:compound_sta_st? st) (analy-compound_sta_st st))
        ((stx:compound_null_st? st) (analy-compound_null_st st))))

  
                           
                                                

    


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


