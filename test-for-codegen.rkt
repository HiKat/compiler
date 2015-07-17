#lang racket
(require "intermed.rkt")
(require "assign-addr.rkt")
(require "codegen.rkt")
(require "gen-intermed.rkt")
(require "semantic-analy.rkt")
(require (prefix-in k08: "kadai08.rkt"))
(require rackunit)

#;(intermed-exp->code 
  (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (intexp 77))

#;(intermed-exp->code 
  (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (varexp (obj-off 'temp1 'temp 'temp 'temp 'temp -16)))

#;(intermed-exp->code 
  (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (aopexp '+
          (obj-off 'i 2 'var 'int 'test 0)
          (obj-off 'temp1 'temp 'temp 'temp 'temp -16)))

#;(intermed-exp->code
 (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
 (varexp (obj-off 'temp2 'temp 'temp 'temp 'temp -20)))

#;(intermed-exp->code
 (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
 (returnstmt (obj-off 'temp4 'temp 'temp 'temp 'temp -16)))


(define p (open-input-file "test01.c"))
(port-count-lines! p)
(define test-itmd
  (gen-assigned-itmd
   (assign-add-intermed 
    (gen-optimized-intermed (sem-analyze-tree (k08:parse-port p))))))

(intermed-prog->code test-itmd)
