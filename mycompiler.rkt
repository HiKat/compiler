#lang racket
(require "semantic-analy.rkt")
(require (prefix-in k08: "kadai08.rkt"))
(require "gen-intermed.rkt")
(require "codegen.rkt")
(require "intermed.rkt")
(require "assign-addr.rkt")
(require "myenv.rkt")
(require "printcode.rkt")

;basic/[arith.sc/array.sc/cmp.sc/fib.sc/gcd.sc/global.sc/logic.sc/scope.sc/swap.sc/while.sc]
(define sc-program "basic/swap.sc")

;テスト

(begin
  (define p1 (open-input-file sc-program))
  (port-count-lines! p1)
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が意味解析の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (sem-analyze-tree (k08:parse-port p1)))

(begin
  (define p (open-input-file sc-program))
  (port-count-lines! p)
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が中間命令生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (gen-optimized-intermed (sem-analyze-tree (k08:parse-port p))))

(begin
  (define test-ass (open-input-file sc-program))
  (port-count-lines! test-ass)
  (define test-intermed 
    (assign-add-intermed 
     (gen-optimized-intermed (sem-analyze-tree (k08:parse-port test-ass)))))
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当ての実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (define test-ass-itmd (gen-assigned-itmd test-intermed))
  test-ass-itmd)

#;(begin
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下がアセンブリ生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (define test-asmbl (intermed-prog->code test-ass-itmd))
  test-asmbl)

#;(begin
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下がコードを文字列にした結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (display (code->strings test-asmbl)))

(define (compiler filename)
  (let* ((file-port (open-input-file filename)))
    (port-count-lines! file-port)
    (display 
     (code->strings 
      (intermed-prog->code 
       (gen-assigned-itmd 
        (assign-add-intermed 
         (gen-optimized-intermed 
          (sem-analyze-tree 
           (k08:parse-port file-port))))))))))

(compiler sc-program)

