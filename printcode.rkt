#lang racket
(require (prefix-in code: "codegen.rkt"))
(require rackunit)


(define (address->string addr)
  (cond
   [(symbol? addr) (symbol->string addr)]
   [(number? addr) (number->string addr)]))

(define (instr->string i)
  (cond
   [(code:instr? i)
    (let* ([op (code:instr-op i)]
	   [args (code:instr-args i)]
	   [argsstr (string-join (map address->string args) ",")])
      (format "\t~a\t~a" (symbol->string op) argsstr))]
   [(code:label? i)
    (let* ([label (code:label-name i)])
      (format "~a:" (symbol->string label)))]
   [(code:dir? i)
    (let* ([label (code:dir-label i)]
	   [args (code:dir-args i)]
	   [argsstr (string-join (map symbol->string args) ",")])
      (format "\t~a\t~a" label argsstr))]
   [(code:global? i)
    (let* ([name (code:global-name i)]
           [size (code:global-size i)])
      (cond ((equal? '() size)
             (format "~a:\t.word 0" name))
            (else 
             (format "~a:\t.word 0:~a" name size))))]
))

#;(define (instr->string i)
  (cond
   [(code:instr? i)
    (let* ([op (code:instr-op i)]
	   [args (code:instr-args i)]
	   [argsstr (string-join (map address->string args) ",")])
      (format "\t~a\t~a\n" (symbol->string op) argsstr))]
   [(code:label? i)
    (let* ([label (code:label-name i)])
      (format "~a:\n" (symbol->string label)))]
   [(code:dir? i)
    (let* ([label (code:dir-label i)]
	   [args (code:dir-args i)]
	   [argsstr (string-join (map symbol->string args) ",")])
      (format "\t~a\t~a\n" label argsstr))]
   [(code:global? i)
    (let* ([name (code:global-name i)]
           [size (code:global-size i)])
      (cond ((equal? '() size)
             (format "~a:\t.word 0\n" name))
            (else 
             (format "~a:\t.word 0:~a\n" name size))))]
))
   

(define (code->strings code)
  (let* (#;[_ (printf "code:~a~n" code)]
         [mapped (map instr->string code)]
         #;[_ (printf "mapped:~a~n" mapped)])
    mapped))
(provide (all-defined-out))
