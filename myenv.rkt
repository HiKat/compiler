#lang racket
;環境操作関数.
(struct obj (name lev kind type)#:transparent)

(define initial-env '())


#;(define (lookup-env name e)
  (if (eq? e initial-env) 
      #f
      (if (equal? (obj-name (car e)) name)
          (car e)
          (lookup-env name (cdr e)))))

(define (extend-env x e)
  (cons x e))

(define (add-list l e)
  (if (eq? l '()) 
      e
      (let* ((newenv (extend-env (car l) e)))
        (add-list (cdr l) newenv))))

;例) '((a) (b c d) (e f) (g)) -> '(a b c d e f g)
(define (separate-list l)
  (cond ((eq? '() (cddr l)) (append (car l) (cadr l)))
        (else (separate-list (list (append (car l) (cadr l)) (caddr l))))))

   
   

;例)'(a (b c d) (e f) g) -> '((a) (b c d) (e f) (g))
(define (make-list-list l)
  (cond ((list? l) l)
        (else (list l))))

(provide (all-defined-out))

;テスト
;#;(
(define env initial-env)
(define a (obj 'name1 'lev1 'kind1 'int))
(define b (obj 'name2 'lev2 'kind2 'void))
(set! env (extend-env a env))
env
(set! env (extend-env b env))
env
(set! env (add-list (list a b) env))
env
;(in-env? 'name1 env)
;(in-env? 'x env)
;(lookup-env 'x env)
;(lookup-env 'name2 env)
(define test (list 'a (list 'b 'c 'd) (list 'e 'f) 'g))
(define test2 (map make-list-list test))
(separate-list test2)




