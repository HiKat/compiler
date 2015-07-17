#lang racket
(require "intermed.rkt")
(require "assign-addr.rkt")
(require "codegen.rkt")
(require rackunit)

(intermed-exp->code 
  (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (intexp 77))

(intermed-exp->code 
  (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (varexp (obj-off 'temp1 'temp 'temp 'temp 'temp -16)))

(intermed-exp->code 
  (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
  (aopexp '+
          (obj-off 'i 2 'var 'int 'test 0)
          (obj-off 'temp1 'temp 'temp 'temp 'temp -16)))

(intermed-exp->code
 (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
 (varexp (obj-off 'temp2 'temp 'temp 'temp 'temp -20)))

(intermed-exp->code
 (obj-off 'temp0 'temp 'temp 'temp 'temp -12)
 (returnstmt (obj-off 'temp4 'temp 'temp 'temp 'temp -16)))

;(intermed-stmt->code localvarsinbytes argsinbytes s)
