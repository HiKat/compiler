#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require (prefix-in in: "intermed.rkt"))
(require "semantic-analy.rkt")
(require "gen-intermed.rkt")
(require "assign-addr.rkt")
(provide (all-defined-out))
;テスト
(define test (open-input-file "test01.c"))
(define test2 (open-input-file "test01.c"))
(define test3 (open-input-file "test01.c"))
(define test4 (open-input-file "test01.c"))
(port-count-lines! test)
(port-count-lines! test2)
(port-count-lines! test3)
(port-count-lines! test4)
(display 
 (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が意味解析sem-analyze-treeの実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(sem-analyze-tree (k08:parse-port test))

(display 
 (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が中間命令生成gen-optimized-intermedの実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(gen-optimized-intermed (sem-analyze-tree (k08:parse-port test2)))

(display 
 (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当て（宣言）assign-add-intermedの実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(assign-add-intermed 
 (gen-optimized-intermed (sem-analyze-tree (k08:parse-port test3))))

(display 
 (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当て（参照）ref-add-intermedの実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(ref-add-intermed 
 (assign-add-intermed 
  (gen-optimized-intermed (sem-analyze-tree (k08:parse-port test4)))))


