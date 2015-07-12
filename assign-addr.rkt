(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require (prefix-in in: "intermed.rkt"))
(require "semantic-analy.rkt")
(provide (all-defined-out))
;引数
;in:fundef
;戻り値
;アドレス割り当てを行ったあとのin:fundef
(define (assign-add-fundef st sp)
  (let* ((def-obj (in:fundef-var st))