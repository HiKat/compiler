#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))


(define (analy-declaration_st st)
   #t)
  
  
(define test1
  (stx:declaration_st
   (stx:spec_st 'int 'test)
   (cons
    (cons
   (stx:declarator_st (stx:id_st 'a 'test))
   (stx:declarator_ast_st (stx:id_st 'b 'test)))
    (stx:declarator_st (stx:id_st 'c 'test)))))