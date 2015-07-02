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
(struct type_pointer (pointer type) #:transparent)
;funはシンボル 'fun、outは戻り値の型、inは引数の型のリスト.
;パラメータの無い時'noparaが入る.
(struct type_fun (fun out in) #:transparent)
;ポインタ型のみリスト構造で(list 'pointe var)の形式.
;myenv.rkt内で定義
;(struct obj (name lev kind type)#:transparent)
(struct para_flag (out-type para))
(struct fundef_flag (out-type para))
(struct comp_flag (decl stat n))

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
           (name (stx:id_st-name id))          
           (flag (cond ((stx:declarator_st? decl) 'nomal)
                       ((stx:declarator_ast_st? decl) 'pointer)))
           (kind 'var)
           (type (cond ((eq? flag 'nomal) type)
                       ((eq? flag 'pointer) (list 'pointer type)))))
      (obj name lev kind type)))
  ;;;;
  (let* (;typeに入っているのは (stx:spec_st 'intか'void ポジション)
         (type (stx:declaration_st-type-spec st))
         (declarator-list (stx:declaration_st-declarator-list st))
         ;objのlistを作成する.
         (obj-list (map* 
                    (lambda (x) (make-obj-from-decl x (stx:spec_st-type type) lev))
                    declarator-list)))
     ;(list? obj-list)
    ;意味解析上のエラーがないか確認する.
    (map (lambda (x) (check-decl x env)) obj-list)
    ;(map (lambda (x) (check-decl x para-env)) obj-list)
    ;なければ環境に追加.
    (set! env (add-list obj-list env))
    ;構造体を返す.
    (stx:declaration_st type obj-list)))

;(map (lambda (x) (check-decl x env)) (list (obj 'b 0 'var (type_pointer 'pointer 'void))))
;(map (lambda (x) (check-decl x env)) 
     ;(list (obj 'a 0 'var 'int) (obj 'b 0 'var (type_pointer 'pointer 'int)) (obj 'c 0 'var 'int)))

;(list (obj 'b 0 'var '(pointer void)))


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
                   (name (cond ((eq? flag 'normal) (stx:id_st-name id))
                               ((eq? flag 'pointer)(stx:id_ast_st-name id))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((eq? flag 'normal) type)
                               ((eq? flag 'pointer)(type_pointer 'pointer type)))))
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
         (para-obj-list (cond ((eq? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist para-list))))
         (proto-type (cond ((eq? 'normal (para_flag-out-type flag))
                            (type_fun 'fun 
                                      (stx:spec_st-type spec) 
                                      (cond ((eq? 'nopara para-obj-list)
                                             'nopara)
                                            (else (map (lambda (x) (obj-type x)) para-obj-list)))))
                           ;(struct type-pointer (pointer type) #:transparent)
                           ((eq? 'pointer (para_flag-out-type flag))
                            (type_fun 'fun
                                      (type_pointer 'pointer (stx:spec_st-type spec))
                                      (cond ((eq? 'nopara para-obj-list)
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
    (stx:func_proto_st spec  
                      (cond 
                        ;para-obj-listはパラメータのobjのlist
                        ((eq? 'normal (para_flag-out-type flag)) 
                         ;nameの部分をプロトタイプのオブジェクトで置き換える.
                         (stx:func_declarator_st proto-obj para-obj-list proto-pos))
                        ((eq? 'pointer (para_flag-out-type flag)) 
                         ;nameの部分をプロトタイプのオブジェクトで置き換える.
                         (stx:func_declarator_ast_st proto-obj para-obj-list proto-pos))
                        (else (error "SYNTAX ERROR IN FUNC_PROTO_ST"))))))

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
                   (name (cond ((eq? flag 'normal) (stx:id_st-name id))
                               ((eq? flag 'pointer)(stx:id_ast_st-name id))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((eq? flag 'normal) type)
                               ((eq? flag 'pointer)(type_pointer 'pointer type)))))
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
         (para-obj-list (cond ((eq? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist para-list))))
         (fundef-type (cond ((eq? 'nopara para-obj-list) 'nopara) 
                            ((eq? 'normal (fundef_flag-out-type flag))
                             (type_fun 'fun
                                       (stx:spec_st-type spec)
                                       (map (lambda (x) (obj-type x)) para-obj-list)))
                            ;(struct type-pointer (pointer type) #:transparent)
                            ((eq? 'pointer (fundef_flag-out-type flag))
                             (type_fun 'fun
                                       (type_pointer 'pointer (stx:spec_st-type spec))
                                       (map (lambda (x) (obj-type x)) para-obj-list)))))
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
                     (cond 
                       ;para-obj-listはパラメータのobjのlist
                       ((eq? 'noramal (fundef_flag-out-type flag)) 
                        ;nameの部分をプロトタイプのオブジェクトで置き換える.
                        (stx:func_declarator_st fundef-obj para-obj-list fundef-pos))
                       ((eq? 'pointer (fundef_flag-out-type flag)) 
                        ;nameの部分をプロトタイプのオブジェクトで置き換える.
                        (stx:func_declarator_ast_st fundef-obj para-obj-list fundef-pos) 
                        ))
                     (analy-compound_st compo 2 env)
                     )))

(define (analy-compound_st st lev env)
  (let* ((flag (cond ((stx:compound_st? st) (comp_flag 'normal 'normal 1))
                     ((stx:compound_dec_st? st) (comp_flag 'normal 'nostat 2))
                     ((stx:compound_sta_st? st) (comp_flag 'nodecl 'normal 3))
                     ((stx:compound_null_st? st) (comp_flag 'nodecl 'nostat 4))
                     (else (error "ERROR IN ANALY-COMPOUND"))))
         ;decl-listには(list* stx:declaration_st...)
         (decl-list (cond ((eq? 1 (comp_flag-n flag)) (stx:compound_st-declaration-list st))
                          ((eq? 2 (comp_flag-n flag)) (stx:compound_dec_st-declaration-list st))
                          ((or (eq? 3 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 'nodecl)))
         ;statement-listにはstatementのlist*が入る.
         ;処理する際はmap*で
         (stat-list (cond ((eq? 1 (comp_flag-n flag)) (stx:compound_st-statement-list st))
                          ((eq? 3 (comp_flag-n flag)) (stx:compound_sta_st-statement-list st))
                          ((or (eq? 2 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 'nostat)))
         ;意味解析開始時にlevを一つ繰り上げる 
         (this-lev (+ lev 1))
         ;decl-listに入るのは(list stx:declaration_st...)か'nodecl
         (decl-list (cond ((eq? 'normal (comp_flag-decl flag)) 
                            ;このときオブジェクトはcomp-envに追加する必要がある.
                            ;(map* analy-compdecl decl-list
                            (map* (lambda (x) (analy-compdecl x this-lev)) decl-list))                  
                           ((eq? 'nodecl (comp_flag-decl flag))
                            'nodecl)))
         ;decl-listから環境を生成する.
         (comp-env 
          (cond 
            ((eq? 'nodecl decl-list) 'nodecl)
            (else (separate-list 
                ;listの各要素がlistであることを保証させるためのmake-list-list
                (map make-list-list
                (map (lambda (x) (stx:declaration_st-declarator-list x)) decl-list))
                )))
          )
         ;comp-envのチェック（自分自身のチェックと大域環境とのチェック.）
         
         (original-env env)
         (comp-env (cond ((eq? 'nodecl comp-env) env)
                         (else (append comp-env env))))
         (stat-list (cond ((eq? 'normal (comp_flag-stat flag)) 
                           (map* (lambda (x) (analy-compstate x this-lev comp-env)) stat-list))
                          ((eq? 'nostat (comp_flag-stat flag)) 
                           'nostat)))
         )
    ;意味解析終了時にlevを一つ繰り下げる
    ;(set! comp-lev (- lev 1))
    (stx:compound_st decl-list stat-list)
    (set! env original-env)))

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
           (name (stx:id_st-name id))          
           (flag (cond ((stx:declarator_st? decl) 'nomal)
                       ((stx:declarator_ast_st? decl) 'pointer)))
           (kind 'var)
           (type (cond ((eq? flag 'nomal) type)
                       ((eq? flag 'pointer) (type_pointer 'pointer type)))))
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
(define (analy-compstate st lev env)
  (cond ((stx:null_statement_st? st) 
         st)
        ((stx:assign_exp_st? st) 
         (stx:assign_exp_st (analy-compstate (stx:assign_exp_st-dest st) lev env)
                            (analy-compstate(stx:assign_exp_st-src st) lev env)
                            (stx:assign_exp_st-pos st)))
        ((stx:logic_exp_st? st) 
         (stx:logic_exp_st (stx:logic_exp_st-log-ope st) 
                           (analy-compstate (stx:logic_exp_st-op1 st) lev env)
                           (analy-compstate (stx:logic_exp_st-op2 st) lev env)
                           (stx:logic_exp_st-pos st)))
        ((stx:rel_exp_st? st) 
         (stx:rel_exp_st (stx:rel_exp_st-rel-ope st) 
                         (analy-compstate (stx:rel_exp_st-op1 st) lev env)
                         (analy-compstate (stx:rel_exp_st-op2 st) lev env)
                         (stx:rel_exp_st-pos st)))
        ((stx:alge_exp_st? st) 
         (stx:alge_exp_st (stx:alge_exp_st-alge-ope st) 
                          (analy-compstate (stx:alge_exp_st-op1 st) lev env)
                          (analy-compstate (stx:alge_exp_st-op2 st) lev env)
                          (stx:alge_exp_st-pos st)))
        ((stx:unary_exp_st? st) 
         (stx:unary_exp_st (stx:unary_exp_st-mark st) 
                           (analy-compstate (stx:unary_exp_st-op st) lev env)
                           (analy-compstate (stx:unary_exp_st-pos st) lev env)))
        ((stx:constant_st? st) st)
        ((stx:exp_with_semi_st? st) 
         (stx:exp_with_semi_st (analy-compstate (stx:exp_with_semi_st-exp st) lev env)))
        ((stx:exp_in_paren_st? st) 
         (stx:exp_in_paren_st (analy-compstate (stx:exp_in_paren_st-exp st) lev env)))
        ((stx:if_st? st) 
         (stx:if_st (analy-compstate (stx:if_st-cond-exp st) lev env)
                    (analy-compound_st (stx:if_st-state st) lev)
                    (stx:if_st-pos st)))
        ((stx:if_else_st? st) 
         (stx:if_else_st (analy-compstate (stx:if_else_st-cond-exp st) lev env)
                         (analy-compstate (stx:if_else_st-state st) lev env)
                         (analy-compstate (stx:if_else_st-else-state st) lev env)
                         (stx:if_else_st-if-pos st)
                         (stx:if_else_st-else-pos st)))
        ((stx:while_st? st) 
         (stx:while_st (analy-compstate (stx:while_st-cond-exp st) lev env)
                       (analy-compound_st (stx:while_st-statement st) lev) env))
        ((stx:return_st? st) 
         (stx:return_st (analy-compstate (stx:return_st-exp st) lev env)))
        ((or (stx:compound_st? st) 
             (stx:compound_dec_st? st) 
             (stx:compound_sta_st? st) 
             (stx:compound_null_st? st))
             (analy-compound_st st lev env))
        ;チェック時は環境に'nodeclが入ることがあることに注意.
        ((stx:func_st? st) 
         (let* ((out st)) 
           ;チェック実行
           ;#t
           out))
        ((stx:id_st? st) 
         (let* ((out st)
                (name (stx:id_st-name st)))
           ;チェック実行
           ;#t
           out))
        ((stx:id_ast_st? st) 
         (let* ((out st)
                (name (stx:id_ast_st-name st)))
           ;チェック実行
           (cond ((#t) (#t))
                 ((#t) (#t)))
           ;#t
           out))
        (else st)
        ))

(define (sem-analyze-tree t)
  (define (sem-analyze-struct st)
    (cond ((stx:declaration_st? st) (analy-declaration_st st))
          ((stx:func_proto_st? st) (analy-func_proto_st st))
          ((stx:func_def_st? st) (analy-func_def_st st))
          (else 
           (error
            "SYNTAX ERROR! AN EXPECTED ARGUMENT >
            DECLARATION/FUNCTION PROTOTYPE/FUNCTION DEFINITION."))))
  (map* sem-analyze-struct t))




;;;;テスト;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test1
  (stx:func_proto_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_ast_st
    'func
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) 
                              (stx:id_st 'c 'test)))
    'test)))
(define test2
  (stx:func_proto_st 
   (stx:spec_st 'int 'test) 
   (stx:func_declarator_ast_null_st 'func 'test)))
(define test3
  (stx:func_def_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_ast_st
    'functioooooon
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'c 'test)))
    'test)
   (stx:compound_sta_st (stx:null_statement_st 'null))))
(define test4
  (stx:compound_st
   ;;;;declaration-list
   ;(cons
    ;(stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st (stx:id_st 'a 'test)))
    (stx:declaration_st
     (stx:spec_st 'int 'test)
     (cons
      (cons
       (stx:declarator_ast_st (stx:id_st 'b 'test))
       (stx:declarator_st (stx:id_st 'c 'test)))
      (stx:declarator_st (stx:id_st 'd 'test))))
    ;)
   ;;;;statement-list
   (stx:exp_with_semi_st (stx:func_st 'func2 (stx:id_st 'a 'test)))))
(define test5
  (stx:compound_st
   ;;;;declaration-list
   (stx:declaration_st
    (stx:spec_st 'int 'test)
    (cons
     (cons
      (stx:declarator_ast_st (stx:id_st 'b 'test))
      (stx:declarator_st (stx:id_st 'c 'test)))
     (stx:declarator_st (stx:id_st 'd 'test))))
   ;;;;statement-list
   (cons
    ;(cons
     (stx:func_st 'func2 (stx:id_st 'a 'test))
     ;;
     #;(stx:if_else_st
      (stx:constant_st 1 'test)
      ;;;;compound-statementのネスト
      (stx:compound_st
       (stx:declaration_st (stx:spec_st 'int 'test) 
                           (stx:declarator_st (stx:id_st 'bddddddddd 'test)))
       (stx:assign_exp_st
        (stx:id_st 'b 'test)
        (stx:constant_st 2 'test)
        'test))
      ;;;;
      (stx:null_statement_st 'null)
      'test
      'syntax-sygar)
     ;)
     ;;
    (stx:assign_exp_st
     (stx:id_st 'a 'test)
     (stx:alge_exp_st
      'add
      (stx:constant_st 1 'test)
      (stx:constant_st 2 'test)
      'test)
     'test))))
(define test6
  (stx:compound_st
   (stx:declaration_st
    (stx:spec_st 'int 'test)
     (cons
      (stx:declarator_ast_st (stx:id_st 'b 'test))
      (stx:declarator_st (stx:id_st 'c 'test))))
   (stx:func_st 'func2 (stx:id_st 'a 'test))))
(define test7
  (stx:compound_st
   (cons
    (stx:declaration_st
     (stx:spec_st 'int 'test)
     (cons
      (cons
       (stx:declarator_ast_st (stx:id_st 'b 'test))
       (stx:declarator_st (stx:id_st 'c 'test)))
      (stx:declarator_st (stx:id_st 'd 'test))))
    (stx:declaration_st (stx:spec_st 'int 'test) 
                        (stx:declarator_st (stx:id_st 'a 'test))))
   (stx:func_st 'func2 (stx:id_st 'a 'test))))
(define test8
  (stx:func_proto_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_ast_st
    'func
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) 
                              (stx:id_st 'c 'test)))
    'test)))
(define test9
  (stx:declaration_st
   (stx:spec_st 'int 'test)
   ;以下がdeclarator-list
   (cons
    (cons
     (stx:declarator_st (stx:id_st 'a 'test))
     (stx:declarator_ast_st (stx:id_st 'b 'test)))
    (stx:declarator_st (stx:id_st 'c 'test)))))
(define test10
    (stx:declaration_st
   (stx:spec_st 'void 'test)
   ;以下がdeclarator-list
     (stx:declarator_ast_st (stx:id_st 'b 'test))))

;(format "(analy-func_proto_st test1)!!!!!!!!!!")
;(analy-func_proto_st test1)
;(format "(analy-func_proto_st test2)!!!!!!!!!!")
;(analy-func_proto_st test2)
;(format "(analy-func_def_st test3)!!!!!!!!!!")
;(analy-func_def_st test3)
;(format "(analy-compound_st test4 current-lev)!!!!!!!!!!")
;(analy-compound_st test4 current-lev)
;(format "(analy-compound_st test5 current-lev)!!!!!!!!!!")
;(analy-compound_st test5 current-lev)
;(format "(analy-compound_st test6 current-lev)!!!!!!!!!!")
;(analy-compound_st test6 current-lev)
;(format "(analy-compound_st test7 current-lev)!!!!!!!!!!")
;(analy-compound_st test7 current-lev)
;プロトタイプのエラー
;(format "(analy-func_proto_st test8)!!!!!!!!!!")
;(analy-func_proto_st test8)
;(format "(analy-declaration_st test9)!!!!!!!!!!")
;(analy-declaration_st test9 current-lev)
;(format "(analy-declaration_st test10)!!!!!!!!!!")
;(analy-declaration_st test10 0)
(define p (open-input-file "kadai01.c"))
(port-count-lines! p)
(sem-analyze-tree (k08:parse-port p))
