#lang racket
(provide (all-defined-out))
;作成中
;obj
;を受け取って
;環境と照らしあわせて
;異常があれば、
;エラー、(error 'failed)等もしくは
;警告
; (format "ERROR! redifinition of '~a'."(obj-name obj))等
;を出力する.
;正しければ
;#t
;を出力する.
(define (check-env obj env)
  #t)


(define (check-test x)
  (if (eq? x 1) 
      (error 'failed)
       #t))
(map check-test (list 2 3 4))