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
(provide (all-defined-out))

(define sp 0)

;オフセットを追加したdecl構造体obj-off
(struct obj-off (name lev kind type pos off) #:transparent)
;オフセットなしのobjをオフセット付きのdecl構造体obj-offに変換する関数.
(define (convert-obj ob off)
  (let* ((name (obj-name ob))
         (lev (obj-lev ob))
         (kind (obj-kind ob))
         (type (obj-type ob))
         (pos (obj-pos ob)))
    (obj-off name lev kind type pos off)))

;引数
;vardeclのlist
;iはスタートの位置
;jは次の位置との差
;戻り値
;offsetを加えたvardeclのlist
(define (convert-iter i j vardecl-list)
  (cond ((equal? '() vardecl-list)  '())
        (else 
         (let* ((car-convert (list (in:vardecl (convert-obj (in:vardecl-var (car vardecl-list)) i)))))
           (set! sp (+ j i))
           (flatten 
               (append 
                car-convert
                ;(list (convert-iter (+ j i) j (cdr vardecl-list)))
                (list (convert-iter sp j (cdr vardecl-list)))))))))
;引数
;プログラムを中間命令文に変換したもの
;戻り値
;アドレス割り当てを行ったあとのin:fundef
(define (sub-assign-add-intermed st)
  (cond ((in:fundef? st)
         (let* ((def-obj (in:fundef-var st))
                ;vardeclのlist
                (vardecl-list (in:fundef-parms st))
                (meaningless (set! sp 0))
                (convert-vardecl (convert-iter 4 4 vardecl-list))
                (meaningless (set! sp 0))
                (body (in:fundef-body st)))
           (in:fundef def-obj 
                      convert-vardecl
                      (assign-add-cmpd body sp))))
        (else st)))
(define (assign-add-intermed in)
  (map sub-assign-add-intermed in))

;引数
;中間命令の構造体
;iは開始位置
;戻り値
;アドレス行ったあとのin:compdstmt
(define (assign-add-cmpd st i)
  (cond ((in:compdstmt? st)
         (let* ((decls (in:compdstmt-decls st))
                (stmts (in:compdstmt-stmts st)))
           ;(set! sp (+ sp (* -4 (length decls))))
           (in:compdstmt 
            (convert-iter i -4 decls) 
            (map (lambda (x) (assign-add-cmpd x sp)) stmts))))
        ((in:ifstmt? st)
         (let* ((var (in:ifstmt-var st))
                (stmt1 (in:ifstmt-stmt1 st))
                (stmt2 (in:ifstmt-stmt2 st)))
           (in:ifstmt var 
                      (assign-add-cmpd stmt1 sp) 
                      (assign-add-cmpd stmt2 sp))))
        ((in:whilestmt? st)
         (let* ((var (in:whilestmt-var st))
                (stmt (in:whilestmt-stmt st)))
           (in:whilestmt var (assign-add-cmpd stmt sp))))
        (else st)
        ))



;テスト
(define test-ass (open-input-file "test01.c"))
(port-count-lines! test-ass)
(display 
 (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当ての実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(assign-add-intermed (gen-optimized-intermed (sem-analyze-tree (k08:parse-port test-ass))))
