#lang racket
(require (prefix-in itmd: "intermed.rkt"))
(require (prefix-in assn: "assign-addr.rkt"))
(require "codegen.rkt")
(require rackunit)

(intermed-exp->code 
  (assn:obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (itmd:intexp 77))

#;(intermed-exp->code 
  (assn:obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (itmd:aopexp '+
          (itmd:varexp (assn:obj-off 'i 2 'var 'int 'test 0))
          (itmd:varexp (assn:obj-off 'temp1 'temp 'temp 'temp 'temp -16))))

(intermed-exp->code
 (assn:obj-off 'temp0 'temp 'temp 'temp 'temp -12)
 (itmd:varexp (assn:obj-off 'temp2 'temp 'temp 'temp 'temp -20)))

(intermed-stmt->code localvarsinbytes argsinbytes s)
