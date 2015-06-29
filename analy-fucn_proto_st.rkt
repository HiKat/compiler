#lang racket
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require "check-env.rkt")
(provide (all-defined-out))
;(struct obj (name lev kind type)#:transparent)
(define current-lev 0)
(define env '())

;プロトタイプ宣言を行った関数の
;返り値がnormalかpointerか、パラメータの有無がnoremalかnoneか
;を保存するフラグ
(struct para_flag (out-type para))




;analy-func_proto_stは
;(stx:func_proto_st...)
;を受け取って
;(stx:func_proto_st (stx:spec_st...) 
;                   (stx:func_declarator/_ast/_st 関数名
;                                (list obj...)))
;を返し 
;同時に関数プロトタイプのobject(obj name 0 'proto type)を
;環境に登録.
;パラメータのobject(obj name 1 'parm type)の(list obj...)を
;パラメータ専用の環境をまず初期化してから登録
;これは初期化->エラーチェックもしくはエラーチェック->初期化 ?????????????????????
(define (analy-func_proto_st st)
  ;;;;;内部定義
  ;make-obj-from-paralistは
  ;(list* (stx:para_declaration_st...)...)
  ;を引数に取り、
  ;(list obj...)
  ;を返す.
  ;'noparaの時は関数の外で処理する.
  (define　(make-obj-from-paralist para-list)
    (map* (lambda (para-decl) 
            (let* (;typeは'intとか.この時点では確定しない.最終的にはflagと合わせて決定.
                   (type (stx:spec_st-type (stx:para_declaration_st-type-spec para-decl)))
                   ;idはstx:id_stかstx:id_ast_st
                   (id (stx:para_declaration_st-para para-decl))
                    ;flagはポインタ型なら'pointer、そうでなければ'normal
                   ;flagとtypeは1つにまとめることも可能
                   (flag (cond ((stx:id_st? id) 'normal)
                               ((stx:id_ast_st? id) 'pointer)))
                   (name (cond ((eq? flag 'normal) (stx:id_st-name id))
                               ((eq? flag 'pointer)(stx:id_ast_st-name id))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((eq? flag 'normal) type)
                               ((eq? flag 'pointer)(list 'pointer type)))))
              (obj name lev kind type)))
          para-list))
   ;;;;;内部定義ここまで                
  (let* (;このspecがintで返り値が*intの場合あり.
         ;返り値は最終的にはここの型とflagで決定される.
         ;specはstx:spec_st
         (spec (stx:func_proto_st-type-spec st))
         ;declはstx:func_declarator/_null/_ast/_stの4つの場合がある.
         (decl (stx:func_proto_st-func-declarator st))
         ;返り値がnormalかpointerか、パラメータの有無がnoremalかnoneか
         (flag (cond ((stx:func_declarator_st? decl)(para_flag 'normal 'normal))
                     ((stx:func_declarator_null_st? decl)(para_flag 'normal 'none))
                     ((stx:func_declarator_ast_st? decl) (para_flag 'pointer 'normal))
                     ((stx:func_declarator_ast_null_st? decl) (para_flag 'pointer 'none))))
         (proto-name (cond ((stx:func_declarator_st? decl) 
                            (stx:func_declarator_st-name decl))
                           ((stx:func_declarator_null_st? decl)
                            (stx:func_declarator_null-name decl))
                           ((stx:func_declarator_ast_st? decl) 
                            (stx:func_declarator_ast_st-name decl))
                           ((stx:func_declarator_ast_null_st? decl) 
                            (stx:func_declarator_ast_null_st-name decl))))
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
         (para-obj-list (cond ((eq? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist paralist))))
    
         
         (proto-type "specの値とflagで決まる.")
         (proto-obj (obj proto-name 0 'proto proto-type)))

    (set! env (extend proto-obj env))
    ;返す構造体
    ;ここで返したいものはlet*で取り出しておく必要がある.
    (stx:func_prot_st spec  
                      (cond ;para-obj-listはパラメータのobjのlist
                        ((eq? 'noramal (para_flag-out-type flag)) 
                         (stx:func_declarator_st proto-name para-obj-list))
                        ((eq? 'pointer (para_flag-out-type flag)) 
                         (stx:func_declarator_ast_st proto-name para-obj-list))))))
                      


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
  