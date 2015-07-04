#lang racket
(require (prefix-in k08: "kadai08.rkt"))
(require "semantic-analy.rkt")
(require "analy-form.rkt")
(require "analy-type.rkt")
(begin
  (define s (open-input-file "test01.c"))
  (port-count-lines! s)
  (k08:parse-port s)
  )
(begin 
  (define p (open-input-file "test01.c"))
  (port-count-lines! p)
  (sem-analyze-tree (k08:parse-port p))
  )

(begin 
  (define q (open-input-file "test01.c"))
  (port-count-lines! q)
  (analy-form (sem-analyze-tree (k08:parse-port q)))
  )

(begin
  (define r (open-input-file "test01.c"))
  (port-count-lines! r)
  (analy-type (sem-analyze-tree (k08:parse-port r)))
)
