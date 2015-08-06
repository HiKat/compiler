#lang racket
(require "semantic-analy.rkt")
(require "kadai08.rkt")
(require "gen-intermed.rkt")
(require "codegen.rkt")
(require "intermed.rkt")
(require "assign-addr.rkt")
(require "myenv.rkt")
(require "printcode.rkt")
(require "analy-type.rkt")
(require "analy-form.rkt")


;テスト

#;(begin
  (define p1 (open-input-file sc-program))
  (port-count-lines! p1)
  (define sem-analyzed-tree (sem-analyze-tree (parse-port p1)))
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が意味解析の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  sem-analyzed-tree
  (analy-type sem-analyzed-tree)
  (analy-form sem-analyzed-tree))
#;(begin
  (define p (open-input-file sc-program))
  (port-count-lines! p)
  (define parsed-port (parse-port p))
   (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下がパーサーの実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
   parsed-port
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が中間命令生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (gen-optimized-intermed (sem-analyze-tree parsed-port)))

#;(begin
  (define test-ass (open-input-file sc-program))
  (port-count-lines! test-ass)
  (define test-intermed 
    (assign-add-intermed 
     (gen-optimized-intermed (sem-analyze-tree (parse-port test-ass)))))
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当ての実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (define test-ass-itmd (gen-assigned-itmd test-intermed))
  test-ass-itmd)

#;(define test-asmbl (intermed-prog->code test-ass-itmd))
#;(begin
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下がアセンブリ生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  test-asmbl)

#;(begin
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下がコードを文字列にした結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (display (code->strings test-asmbl)))

#;(define (compile filename)
  (let* ((file-port (open-input-file filename)))
    (port-count-lines! file-port)
    (display 
     (code->strings 
      (intermed-prog->code 
       (gen-assigned-itmd 
        (assign-add-intermed 
         (gen-optimized-intermed 
          (sem-analyze-tree 
           (parse-port file-port))))))))))

(define (compile filename)
  (let* ((file-port (open-input-file filename))
         (meaningless  (port-count-lines! file-port))
         (sem-analyzed-tree (sem-analyze-tree 
                             (parse-port file-port))))
    (analy-type sem-analyzed-tree)
    (analy-form sem-analyzed-tree) 
     (display 
      (string-join 
       (code->strings 
        (intermed-prog->code 
         (gen-assigned-itmd 
          (assign-add-intermed 
           (gen-optimized-intermed 
            sem-analyzed-tree))))) "\n"))))

;(compile sc-program)
#;(begin
(compile "try_first.sc")
(compile "basic/arith.sc")
(compile "basic/array.sc")
(compile "basic/cmp.sc")
(compile "basic/fib.sc")
(compile "basic/gcd.sc")
(compile "basic/global.sc")
(compile "basic/logic.sc")
(compile "basic/scope.sc")
(compile "basic/swap.sc")
(compile "basic/while.sc"))

;(compile "error/name02.sc")
;(compile "error/name04.sc")
;(compile "error/name05.sc")
;(compile "error/name07.sc")
;(compile "error/name08.sc")
;(compile "error/name09.sc")
;(compile "error/name10.sc")
;(compile "error/name11.sc")
;(compile "error/shape01.sc")
;(compile "error/shape03.sc")
;(compile "error/type01.sc")
;(compile "error/type02.sc")
;(compile "error/type03.sc")
;(compile "error/type04.sc")
;(compile "error/type05.sc")
;(compile "error/type06.sc")
;(compile "error/type08.sc")
;(compile "error/type09.sc")
;(compile "error/type10.sc")
;(compile "error/type11.sc")
;(compile "error/type12.sc")
;(compile "error/type13.sc")
;(compile "error/type14.sc")
;(compile "error/type15.sc")
;(compile "error/type16.sc")
;(compile "error/type17.sc")
;(compile "error/type18.sc")

(provide (all-defined-out))


