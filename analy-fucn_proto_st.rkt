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
  (let* (;このfunc-typeがintで返り値が*intの場合あり.
         ;返り値は最終的にはここの型とflagで決定される.
         ;func-typeはstx:spec_st
         (func-type (stx:func_proto_st-type-spec st))
         ;declはstx:func_declarator/_null/_ast/_stの4つの場合がある.
         (decl (stx:func_proto_st-func-declarator st))
         ;返り値がnormalかpointerか、パラメータの有無がnoremalかnoneか
         (flag (cond ((stx:func_declarator_st? decl)(stx:func_declarator_ast_null_st? decl)'normal)
                     ((stx:func_declarator_null_st? decl)(list 'normal 'none))
                     ((stx:func_declarator_ast_st? decl) (list 'pointer 'normal))
                     ((stx:func_declarator_ast_null_st? decl) (list 'pointer 'none))))
         (proto-name (cond ((stx:func_declarator_st? decl) 
                            (stx:func_declarator_st-name decl))
                           ((stx:func_declarator_null_st? decl)
                            (stx:func_declarator_null-name decl))
                           ((stx:func_declarator_ast_st? decl) 
                            (stx:func_declarator_ast_st-name decl))
                           ((stx:func_declarator_ast_null_st? decl) 
                            (stx:func_declarator_ast_null_st-name decl))))
         (proto-type "under const")
         (proto-obj (obj proto-name 0 'proto proto-type)))
    (check-env proto-obj env)
    (set! env (extend proto-obj env))
         


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
  