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


;(stx:declaration_st...)
;を受け取って
;(stx;declaration_st type-spec (list (obj...) (obj...)...))
;を返す.
;同時にlistの形で環境に追加.
;同時に環境のチェックも行う.
(define (analy-declaration_st st)
  ;;;;
  ;内部定義
  ;(stx:declarator_st...)と
  ;'intもしくは'void
  ;を引数にとり
  ;obj
  ;を返す関数.
  (define (make-obj-from-decl decl type)
    (let* ((id (cond ((stx:declarator_st? decl) 
                      (stx:declarator_st-var decl))
                     ((stx:declarator_ast_st? decl)
                      (stx:declarator_ast_st-var decl))))
           (name (stx:id_st-name id))          
           (flag (cond ((stx:declarator_st? decl) 'nomal)
                       ((stx:declarator_ast_st? decl) 'pointer)))
           (lev current-lev)
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
                    (lambda (x) (make-obj-from-decl x (stx:spec_st-type type)))
                    declarator-list)))
    ;意味解析上のエラーがないか確認する.
    ;under construction
    (map (lambda (list) (check-env env list)) obj-list)
    ;なければ環境に追加.
    (set! env (add-list obj-list env))
    ;構造体を返す.
    (stx:declaration_st type obj-list)))

(define test1
  (stx:declaration_st
   (stx:spec_st 'int 'test)
   ;以下がdeclarator-list
   (cons
    (cons
     (stx:declarator_st (stx:id_st 'a 'test))
     (stx:declarator_ast_st (stx:id_st 'b 'test)))
    (stx:declarator_st (stx:id_st 'c 'test)))))

(define test2
    (stx:declaration_st
   (stx:spec_st 'void 'test)
   ;以下がdeclarator-list
     (stx:declarator_ast_st (stx:id_st 'b 'test))))

(analy-declaration_st test2)
