#lang racket
(struct object (name lev kind type)#:transparent)

(define initial-env 'empty)

(define (in-env? x e)
  (cond
   [(eq? e initial-env) #f]
   [(equal? (object-name (car e)) x) #t]
   [else (in-env? x (cdr e))]))

(define (lookup-env name e)
  (if (eq? e initial-env) 
      #f
      (if (equal? (object-name (car e)) name)
          (car e)
          (lookup-env name (cdr e)))))

(define (extend-env x e)
  (cons x e))

(provide (all-defined-out))

;テスト
;(define env initial-env)
;(define a (object 'name1 'lev1 'kind1 'int))
;(define b (object 'name2 'lev2 'kind2 'void))
;(set! env (extend-env a env))
;env
;(set! env (extend-env b env))
;env
;(in-env? 'name1 env)
;(in-env? 'x env)
;(lookup-env 'x env)
;(lookup-env 'name2 env)

