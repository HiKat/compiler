#lang racket
(require "semantic-analy.rkt")
(require (prefix-in k08: "kadai08.rkt"))
(require "gen-intermed.rkt")
(require "codegen.rkt")
(require "intermed.rkt")
(require "assign-addr.rkt")
(require "myenv.rkt")

;テスト
(begin
(define test-ass (open-input-file "test01.c"))
(port-count-lines! test-ass)
(define test-intermed 
  (assign-add-intermed 
   (gen-optimized-intermed (sem-analyze-tree (k08:parse-port test-ass)))))
(display 
 (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当ての実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(gen-assigned-itmd test-intermed))

#;(intermed-exp->code 
 (obj-off 'i 2 'var 'int 'test 0)
 (obj-off 'sort_array 2 'var (type_array 'int 8) (position 188 14 8) (obj 'temp5 'temp 'temp 'temp 'temp)))