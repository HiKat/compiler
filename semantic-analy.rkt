#lang racket
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(provide (all-defined-out))

;(define current-lev 0)
;(define comp-lev 0)
(define env '())
(define para-env '())
(define comp-env '())


(define (analy-declaration_st st lev)
  ;;;;
  ;内部定義
  ;(stx:declarator_st...)と
  ;levと  
  ;'intもしくは'void
  ;を引数にとり
  ;obj
  ;を返す関数.
  (define (make-obj-from-decl decl type lev)
    (let* ((id (cond ((stx:declarator_st? decl) 
                      (stx:declarator_st-var decl))
                     ((stx:declarator_ast_st? decl)
                      (stx:declarator_ast_st-var decl))))
           (name (cond ((stx:id_st? id) (stx:id_st-name id))
                       ((stx:array_st? id) (stx:array_st-name id))))
           (flag (cond ((stx:declarator_st? decl) 'nomal)
                       ((stx:declarator_ast_st? decl) 'pointer)))
           (kind 'var)
           (type (cond ((stx:array_st? id) (type_array type (stx:array_st-num id)))
                       (else (cond ((equal? flag 'nomal) type)
                                   ((equal? flag 'pointer) (type_pointer 'pointer type)))))))
      (obj name lev kind type)))
      ;;;;
  (let* (;typeに入っているのは (stx:spec_st 'intか'void ポジション)
         (type (stx:declaration_st-type-spec st))
         (declarator-list (stx:declaration_st-declarator-list st))
         ;objのlistを作成する.
         (obj-list (map* 
                    (lambda (x) (make-obj-from-decl x (stx:spec_st-type type) lev))
                    declarator-list)))
    ;意味解析上のエラーがないか確認する.
    (map (lambda (x) (check-decl x env)) obj-list)
    (map (lambda (x) (check-decl x para-env)) obj-list)
    ;なければ環境に追加.
    (set! env (add-list obj-list env))
    ;構造体を返す.
    (stx:declaration_st type obj-list)))


(define (analy-func_proto_st st)
  ;;;;;内部定義
  ;make-obj-from-paralistは
  ;(list* (stx:para_declaration_st...)...)
  ;を引数に取り、
  ;(list obj...)
  ;を返す.
  ;'noparaの時は関数の外で処理する.
  (define (make-obj-from-paralist para-list)
    (map* (lambda (para-decl) 
            (let* (;typeは'intとか.この時点では確定しない.最終的にはflagと合わせて決定.
                   (type (stx:spec_st-type (stx:para_declaration_st-type-spec para-decl)))
                   ;idはstx:id_stかstx:id_ast_st
                   (id (stx:para_declaration_st-para para-decl))
                    ;flagはポインタ型なら'pointer、そうでなければ'normal
                   (flag (cond ((stx:id_st? id) 'normal)
                               ((stx:id_ast_st? id) 'pointer)))
                   (name (cond ((equal? flag 'normal) (stx:id_st-name id))
                               ((equal? flag 'pointer)(stx:id_ast_st-name id))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((equal? flag 'normal) type)
                               ((equal? flag 'pointer)(type_pointer 'pointer type)))))
              (obj name lev kind type)))
          para-list))
   ;;;;;内部定義ここまで                
  (let* (;このspecがintで返り値が*intの場合あり.
         ;返り値は最終的にはここの型とflagで決定される.
         ;specはstx:spec_st
         (spec (stx:func_proto_st-type-spec st))
         ;declはstx:func_declarator/_null/_ast/_stの4つの場合がある.
         (decl (stx:func_proto_st-func-declarator-st st))
         ;返り値がnormalかpointerか、パラメータの有無がnoremalかnoneか
         ;para_flagは(struct para_flag (out-type para))で定義される構造体.
         (flag (cond ((stx:func_declarator_st? decl)(para_flag 'normal 'normal))
                     ((stx:func_declarator_null_st? decl)(para_flag 'normal 'none))
                     ((stx:func_declarator_ast_st? decl) (para_flag 'pointer 'normal))
                     ((stx:func_declarator_ast_null_st? decl) (para_flag 'pointer 'none))))
         (proto-name (cond ((stx:func_declarator_st? decl) 
                            (stx:func_declarator_st-name decl))
                           ((stx:func_declarator_null_st? decl)
                            (stx:func_declarator_null_st-name decl))
                           ((stx:func_declarator_ast_st? decl) 
                            (stx:func_declarator_ast_st-name decl))
                           ((stx:func_declarator_ast_null_st? decl) 
                            (stx:func_declarator_ast_null_st-name decl))))
         ;プロトタイプの位置情報
         (proto-pos (cond ((stx:func_declarator_st? decl) 
                            (stx:func_declarator_st-pos decl))
                           ((stx:func_declarator_null_st? decl)
                            (stx:func_declarator_null_st-pos decl))
                           ((stx:func_declarator_ast_st? decl) 
                            (stx:func_declarator_ast_st-pos decl))
                           ((stx:func_declarator_ast_null_st? decl) 
                            (stx:func_declarator_ast_null_st-pos decl))))
         ;para-listは(list* (stx:para_declaration_st...)...)
         ;もしくはパラメータが無いときは'noparaが入っている.
         (para-list (cond ((stx:func_declarator_st? decl) 
                            (stx:func_declarator_st-para-list decl))
                           ((stx:func_declarator_null_st? decl)
                            'nopara)
                           ((stx:func_declarator_ast_st? decl) 
                            (stx:func_declarator_ast_st-para-list decl))
                           ((stx:func_declarator_ast_null_st? decl) 
                            'nopara)))
         ;para-obj-listは(list obj...)もしくは'nopara
         (para-obj-list (cond ((equal? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist para-list))))
         (proto-type (cond ((equal? 'normal (para_flag-out-type flag))
                            (type_fun 'fun 
                                      (stx:spec_st-type spec) 
                                      (cond ((equal? 'nopara para-obj-list)
                                             'nopara)
                                            (else (map (lambda (x) (obj-type x)) para-obj-list)))))
                           ;(struct type-pointer (pointer type) #:transparent)
                           ((equal? 'pointer (para_flag-out-type flag))
                            (type_fun 'fun
                                      (type_pointer 'pointer (stx:spec_st-type spec))
                                      (cond ((equal? 'nopara para-obj-list)
                                             'nopara)
                                            (else (map (lambda (x) (obj-type x)) para-obj-list)))))))
         (proto-obj (obj proto-name 0 'proto proto-type)))
    ;プロトタイプのオブジェクトのチェック
    (check-proto proto-obj env)
    ;プロトタイプのオブジェクトを環境に追加.
    (set! env (extend-env proto-obj env))
    ;パラメータ内の二重宣言をチェック.
    (check-proto-para para-obj-list)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;返す構造体
    ;ここで返したいものはlet*で取り出しておく必要がある.
    (stx:func_proto_st spec (stx:func_declarator_st proto-obj para-obj-list proto-pos))))

(define (analy-func_def_st st)
  ;;;;;内部定義
  ;make-obj-from-paralistは
  ;(list* (stx:para_declaration_st...)...)
  ;を引数に取り、
  ;(list obj...)
  ;を返す.
  ;'noparaの時は関数の外で処理する.
  (define (make-obj-from-paralist para-list)
    (map* (lambda (para-decl) 
            (let* (;typeは'intとか.この時点では確定しない.最終的にはflagと合わせて決定.
                   (type (stx:spec_st-type (stx:para_declaration_st-type-spec para-decl)))
                   ;idはstx:id_stかstx:id_ast_st
                   (id (stx:para_declaration_st-para para-decl))
                   ;flagはポインタ型なら'pointer、そうでなければ'normal
                   (flag (cond ((stx:id_st? id) 'normal)
                               ((stx:id_ast_st? id) 'pointer)))
                   (name (cond ((equal? flag 'normal) (stx:id_st-name id))
                               ((equal? flag 'pointer)(stx:id_ast_st-name id))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((equal? flag 'normal) type)
                               ((equal? flag 'pointer)(type_pointer 'pointer type)))))
              (obj name lev kind type)))
          para-list))
  ;;;;;内部定義ここまで                
  (let* (;このspecがintで返り値が*intの場合あり.
         ;返り値は最終的にはここの型とflagで決定される.
         ;specはstx:spec_st
         (spec (stx:func_def_st-type-spec st))
         ;declはstx:func_declarator/_null/_ast/_stの4つの場合がある.
         (decl (stx:func_def_st-func-declarator-st st))
         (compo (stx:func_def_st-compound-state-list st))
         ;返り値がnormalかpointerか、パラメータの有無がnoremalかnoneか
         ;fundef_flagは(struct fundef_flag (out-type para))で定義される構造体.
         (flag (cond ((stx:func_declarator_st? decl)(fundef_flag 'normal 'normal))
                     ((stx:func_declarator_null_st? decl)(fundef_flag 'normal 'none))
                     ((stx:func_declarator_ast_st? decl) (fundef_flag 'pointer 'normal))
                     ((stx:func_declarator_ast_null_st? decl) (fundef_flag 'pointer 'none))))
         (fundef-name (cond ((stx:func_declarator_st? decl) 
                              (stx:func_declarator_st-name decl))
                             ((stx:func_declarator_null_st? decl)
                              (stx:func_declarator_null_st-name decl))
                             ((stx:func_declarator_ast_st? decl) 
                              (stx:func_declarator_ast_st-name decl))
                             ((stx:func_declarator_ast_null_st? decl) 
                              (stx:func_declarator_ast_null_st-name decl))))
         ;プロトタイプの位置情報
         (fundef-pos (cond ((stx:func_declarator_st? decl) 
                           (stx:func_declarator_st-pos decl))
                          ((stx:func_declarator_null_st? decl)
                           (stx:func_declarator_null_st-pos decl))
                          ((stx:func_declarator_ast_st? decl) 
                           (stx:func_declarator_ast_st-pos decl))
                          ((stx:func_declarator_ast_null_st? decl) 
                           (stx:func_declarator_ast_null_st-pos decl))))
         ;para-listは(list* (stx:para_declaration_st...)...)
         ;もしくはパラメータが無いときは'noparaが入っている.
         (para-list (cond ((stx:func_declarator_st? decl) 
                           (stx:func_declarator_st-para-list decl))
                          ((stx:func_declarator_null_st? decl)
                           'nopara)
                          ((stx:func_declarator_ast_st? decl) 
                           (stx:func_declarator_ast_st-para-list decl))
                          ((stx:func_declarator_ast_null_st? decl) 
                           'nopara)))
         ;para-obj-listは(list obj...)もしくは'nopara
         (para-obj-list (cond ((equal? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist para-list))))
         (fundef-type (cond ((equal? 'normal (fundef_flag-out-type flag))
                             (type_fun 'fun
                                       (stx:spec_st-type spec)
                                       (cond ((equal? 'nopara para-obj-list) 'nopara)
                                             (else(map (lambda (x) (obj-type x)) para-obj-list)))))
                            ;(struct type-pointer (pointer type) #:transparent)
                            ((equal? 'pointer (fundef_flag-out-type flag))
                             (type_fun 'fun
                                       (type_pointer 'pointer (stx:spec_st-type spec))
                                       (cond ((equal? 'nopara para-obj-list) 'nopara)
                                             (else (map (lambda (x) (obj-type x)) para-obj-list)))))
                            (else (error "IN VALID FUNCTION"))))
         (fundef-obj (obj fundef-name 0 'fun fundef-type)))
    ;関数定義のオブジェクトのチェック
    (check-func fundef-obj env)
    ;関数定義のオブジェクトを環境に追加.
    (set! env (extend-env fundef-obj env))
    ;パラメータ内の二重宣言をチェック
    (check-def-para para-obj-list)
    ;パラメータの環境を登録
    (set! para-env para-obj-list)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;返す構造体
    ;ここで返したいものはlet*で取り出しておく必要がある.
    (stx:func_def_st spec  
                     (stx:func_declarator_st fundef-obj para-obj-list fundef-pos)
                     (analy-compound_st compo 2 env fundef-obj)
                     )))

(define (analy-compound_st st lev outer-env func-tag)
  (let* ((flag (cond ((stx:compound_st? st) (comp_flag 'normal 'normal 1))
                     ((stx:compound_dec_st? st) (comp_flag 'normal 'nostat 2))
                     ((stx:compound_sta_st? st) (comp_flag 'nodecl 'normal 3))
                     ((or (stx:null_statement_st? st) 
                          (stx:compound_null_st? st)) (comp_flag 'nodecl 'nostat 4))
                     (else (error "UNEXPECTED STRUCTURE! ERROR IN ANALY-COMPOUND." st))))
         ;decl-listには(list* stx:declaration_st...)
         (decl-list (cond ((equal? 1 (comp_flag-n flag)) (stx:compound_st-declaration-list st))
                          ((equal? 2 (comp_flag-n flag)) (stx:compound_dec_st-declaration-list st))
                          ((or (equal? 3 (comp_flag-n flag))(equal? 4 (comp_flag-n flag))) 'nodecl)))
         ;statement-listにはstatementのlist*が入る.
         ;処理する際はmap*で
         (stat-list (cond ((equal? 1 (comp_flag-n flag)) (stx:compound_st-statement-list st))
                          ((equal? 3 (comp_flag-n flag)) (stx:compound_sta_st-statement-list st))
                          ((or (equal? 2 (comp_flag-n flag))(equal? 4 (comp_flag-n flag))) 'nostat)))
         ;意味解析開始時にlevを一つ繰り上げる 
         (this-lev (+ lev 1))
         ;decl-listに入っているのは(list stx:declaration_st...)か'nodecl
         (decl-list (cond ((equal? 'normal (comp_flag-decl flag)) 
                            ;このときオブジェクトはcomp-envに追加する必要がある.
                            ;(map* analy-compdecl decl-list
                            (map* (lambda (x) (analy-compdecl x this-lev)) decl-list))                  
                           ((equal? 'nodecl (comp_flag-decl flag))
                            'nodecl)))
         ;decl-listからこのcompoun-statement内で新しく生成される環境を格納する.
         ;comp-env内はこのcompound-statement内で新しく定義されたオブジェクトのlist
         (comp-env 
          (cond 
            ((equal? 'nodecl decl-list) 'nodecl)
            (else (separate-list 
                ;listの各要素がlistであることを保証させるためのmake-list-list
                (map make-list-list
                (map (lambda (x) (stx:declaration_st-declarator-list x)) decl-list))))))
         ;comp-envのチェック
         ;代入は意味は無い.let*の代入文の段階でチェックを実行しておく必要があるためこのようにした.
         ;comp-env内のみで二重定義などが無いかどうかをチェックする.
         (comp-env-check (check-comp-env comp-env))
         ;大域環境と照らし合わせる
         (comp-env-check (map (lambda (x) (check-decl x outer-env)) comp-env))
         (comp-env-check (map (lambda (x) (check-decl x para-env)) comp-env))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         (new-comp-env (cond ((equal? 'nodecl comp-env) env)
                         (else (append comp-env outer-env))))
         (stat-list (cond ((equal? 'normal (comp_flag-stat flag)) 
                           (map* (lambda (x) (analy-compstate x this-lev new-comp-env func-tag)) stat-list))
                          ((equal? 'nostat (comp_flag-stat flag)) 
                           'nostat))))
    ;意味解析終了時にlevを一つ繰り下げる
    ;(display (format "comp-env is>>>>>>>>> ~a \n\n" comp-env))
    ;(display (format "outer-env is>>>>>>>>> ~a \n\n" outer-env))
    ;(display (format "new-comp-env is>>>>>>>>> ~a \n\n" new-comp-env))
    (stx:compound_st decl-list stat-list)))

(define (analy-compdecl st lev)
  ;;;;
  ;内部定義
  ;(stx:declarator_st...)と
  ;levと  
  ;'intもしくは'void
  ;を引数にとり
  ;obj
  ;を返す関数.
  (define (make-obj-from-decl decl type lev)
    (let* ((id (cond ((stx:declarator_st? decl) 
                      (stx:declarator_st-var decl))
                     ((stx:declarator_ast_st? decl)
                      (stx:declarator_ast_st-var decl))))
           (name (cond ((stx:id_st? id) (stx:id_st-name id))
                       ((stx:id_ast_st? id) (stx:id_ast_st-name id))
                       ((stx:array_st? id) (stx:array_st-name id))))          
           (flag (cond ((stx:declarator_st? decl) 'nomal)
                       ((stx:declarator_ast_st? decl) 'pointer)))
           (kind 'var)
           (type (cond ((stx:array_st? id) (type_array type (stx:array_st-num id)))
                       (else (cond ((equal? flag 'nomal) type)
                                   ((equal? flag 'pointer) (type_pointer 'pointer type)))))))
      (obj name lev kind type)))
  ;;;;内部定義ここまで
  (let* (;typeに入っているのは (stx:spec_st 'intか'void ポジション)
         (type (stx:declaration_st-type-spec st))
         (declarator-list (stx:declaration_st-declarator-list st))
         ;objのlistを作成する.
         (obj-list (map* 
                    (lambda (x) (make-obj-from-decl x (stx:spec_st-type type) lev))
                    declarator-list)))
    ;意味解析上のエラーがないかは外側でチェックするのでここでは実装しなくて良い.
    (stx:declaration_st type obj-list)))

;envはnodeclの場合あり.
(define (analy-compstate st lev env func-tag)
  (cond ((stx:null_statement_st? st) 
         st)
        ((stx:assign_exp_st? st) 
         (stx:assign_exp_st (analy-compstate (stx:assign_exp_st-dest st) lev env func-tag)
                            (analy-compstate(stx:assign_exp_st-src st) lev env func-tag)
                            (stx:assign_exp_st-pos st)))
        ((stx:logic_exp_st? st) 
         (stx:logic_exp_st (stx:logic_exp_st-log-ope st) 
                           (analy-compstate (stx:logic_exp_st-op1 st) lev env func-tag)
                           (analy-compstate (stx:logic_exp_st-op2 st) lev env func-tag)
                           (stx:logic_exp_st-pos st)))
        ((stx:rel_exp_st? st) 
         (stx:rel_exp_st (stx:rel_exp_st-rel-ope st) 
                         (analy-compstate (stx:rel_exp_st-op1 st) lev env func-tag)
                         (analy-compstate (stx:rel_exp_st-op2 st) lev env func-tag)
                         (stx:rel_exp_st-pos st)))
        ((stx:alge_exp_st? st) 
         (stx:alge_exp_st (stx:alge_exp_st-alge-ope st) 
                          (analy-compstate (stx:alge_exp_st-op1 st) lev env func-tag)
                          (analy-compstate (stx:alge_exp_st-op2 st) lev env func-tag)
                          (stx:alge_exp_st-pos st)))
        ((stx:unary_exp_st? st) 
         (stx:unary_exp_st (stx:unary_exp_st-mark st) 
                           (analy-compstate (stx:unary_exp_st-op st) lev env func-tag)
                           (stx:unary_exp_st-pos st)))
        ((stx:constant_st? st) st)
        ((stx:exp_with_semi_st? st) 
         (stx:exp_with_semi_st (analy-compstate (stx:exp_with_semi_st-exp st) lev env func-tag)))
        ((stx:exp_in_paren_st? st) 
         (stx:exp_in_paren_st (analy-compstate (stx:exp_in_paren_st-exp st) lev env func-tag)))
        ((stx:if_else_st? st) 
         (stx:if_else_st (analy-compstate (stx:if_else_st-cond-exp st) lev env func-tag)
                         (analy-compstate (stx:if_else_st-state st) lev env func-tag)
                         (analy-compstate (stx:if_else_st-else-state st) lev env func-tag)
                         (stx:if_else_st-if-pos st)
                         (stx:if_else_st-else-pos st)))
        ((stx:while_st? st) 
         (stx:while_st (analy-compstate (stx:while_st-cond-exp st) lev env func-tag)
                       (analy-compound_st (stx:while_st-statement st) lev env func-tag)))
        ((stx:return_st? st) 
         (stx:sem_return_st (analy-compstate (stx:return_st-exp st) lev env func-tag) 
                            (stx:return_st-pos st) 
                            func-tag))
        ((or (stx:compound_st? st) 
             (stx:compound_dec_st? st) 
             (stx:compound_sta_st? st) 
             (stx:compound_null_st? st))
             (analy-compound_st st lev env func-tag))
        ;チェック時は環境に'nodeclが入ることがあることに注意.
        ((stx:func_st? st) 
         (stx:func_st (check-func-ref st lev env) 
                      (cond ((equal? 'nopara (stx:func_st-para st)) 'nopara)
                            (else (map (lambda (x) 
                                         (analy-compstate x lev env func-tag))
                                       (flatten (stx:func_st-para st)))))))
        ((or (stx:id_st? st)     
             (stx:id_ast_st? st))
         (check-var-ref st lev env))
        (else (error "AN UNEXPECTED STRUCTURE! CONDITION ERROR IN ANALY-COMPSTATE FOR" st))
        ))

(define (sem-analyze-tree t)
  (define (sem-analyze-struct st)
    (cond ((stx:declaration_st? st) (analy-declaration_st st 0))
          ((stx:func_proto_st? st) (analy-func_proto_st st))
          ((stx:func_def_st? st) (analy-func_def_st st))
          (else 
           (error
            "SYNTAX ERROR! AN EXPECTED ARGUMENT >
            DECLARATION/FUNCTION PROTOTYPE/FUNCTION DEFINITION."))))
  (let* ((out-tree (map* sem-analyze-struct t)))
    (set! env '())
    out-tree))




;テスト
(define p101 (open-input-file "test01.c"))
(port-count-lines! p101)
(sem-analyze-tree (k08:parse-port p101))

