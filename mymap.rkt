#lang racket
(provide (all-defined-out))
(define (map* func list*)
  (cond ((cons? list*) (append (map* func (car list*))
                               (map* func (cdr list*))))
        (else (cons (func list*) '()))))

;テスト
(define (squa x)
  ((lambda (x) (* x x)) x))

;(map* squa (cons 1(cons 2 (cons 3 4))))

;(map* squa (cons (cons (cons 3 4) 5) 6))
