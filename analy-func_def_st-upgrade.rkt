#lang racket
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require "check-env.rkt")
(provide (all-defined-out))
;(struct obj (name lev kind type)#:transparent)
(define current-lev 0)
;env、para-env初期化
(define env '())
(define para-env '())
;objtypeの要素になりうる構造体.
(struct type-pointer (pointer type) #:transparent)
;stx:func_def_stを
;引数に取り 
;(stx:func_def_st stx:spec_st 
;                 (func_declarator_st '関数宣言のオブジェクト'
;                                     'パラメータのオブジェクトのlist')
;                 compound-statement)
;(compound-statement部分については関数analy-compoundに任せる.)
;を返す.
;同時にパラメータのオブジェクトをパラメータ専用の環境に追加、チェック
;同時に関数宣言のオブジェクトを環境に追加、チェック
(define (analy-func_def_st st)
  



(define test1
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