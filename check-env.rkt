#lang racket
(require "myenv.rkt")
(provide (all-defined-out))
;myenv.rkt内で定義
;(struct obj (name lev kind type) #:transparent)
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
;？？？？何を出力すればいいか？？？？？
;たとえば(format "'~a' is OK!" obj)とか
;正常に意味解析が行われていることがわかればよい？
;理想は何も出力しないこと
(define (check-env env:obj env)
  #t)


;check-proto-paraは  
;(list obj...)か  
;'noparaを  
;受け取って  
;エラーもしくはメッセージを返す  
(define (check-proto-para obj-list)
  (cond 
    ((eq? '() obj-list) (format "OK! CRRECT PARAMETER OF FUNCTION PROTOTYPE"))
    ((eq? 'nopara obj-list) (format "OK! CRRECT PARAMETER OF FUNCTION PROTOTYPE"))
    ((in-env? (obj-name (car obj-list)) (cdr obj-list))
     (error "FAILED! SOME REDEFINITION IN PARAMETER OF FUNCTION PROTOTYPE"))
    (else (check-proto-para (cdr obj-list)))))


         
         

(define (check-test x)
  (if (eq? x 1) 
      (error 'failed)
       #t))
(map check-test (list 2 3 4))

 (define testttt
   (list (obj 'a 1 'parm 'int) (obj 'b 1 'parm '(pointer int)) (obj 'c 1 'parm 'int)))
 
(in-env? (obj-name (car test)) (cdr test))

