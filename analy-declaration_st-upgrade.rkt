#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "map*.rkt")
(provide (all-defined-out))


;(stx:declaration_st...)
;を受け取って
;(stx;declaration_st type-spec (list (obj...) (obj...)...))
;を返す.
;同時にlistの形で環境に追加.
;同時に環境のチェックも行う.
(define (analy-declaration_st st)
   (let* ((type (stx:declaration_st-type-spec st))
          (declarator-list (stx:declaration_st-declarator-list))
          ;objのlistを作成する.
          (obj-list (map* make-obj declarator-list)))
     ;意味解析上のエラーがないか確認する.
     (map check-env obj-list)
     ;なければ環境に追加.
     (set
     ;構造体を返す.
          
  
  
(define test1
  (stx:declaration_st
   (stx:spec_st 'int 'test)
   (cons
    (cons
   (stx:declarator_st (stx:id_st 'a 'test))
   (stx:declarator_ast_st (stx:id_st 'b 'test)))
    (stx:declarator_st (stx:id_st 'c 'test)))))