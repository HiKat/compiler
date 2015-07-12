#lang racket
(require (prefix-in k08: "kadai08.rkt"))
(require "semantic-analy.rkt")
(require "analy-form.rkt")
(require "analy-type.rkt")

(begin 
  (define s (open-input-file "test01.c"))
  (port-count-lines! s)
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が課題8の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  ;(k08:parse-port s)
  (define p (open-input-file "test01.c"))
  (port-count-lines! p)
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が意味解析の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (sem-analyze-tree (k08:parse-port p))
  (define q (open-input-file "test01.c"))
  (port-count-lines! q)
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が形検査の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  ;(analy-form (sem-analyze-tree (k08:parse-port q)))
  (define r (open-input-file "test01.c"))
  (port-count-lines! r)
  (display 
   (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が型検査の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  ;(analy-type (sem-analyze-tree (k08:parse-port r)))
  )
