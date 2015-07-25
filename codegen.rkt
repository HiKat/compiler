#lang racket
(require (prefix-in k08: "kadai08.rkt"))
(require (prefix-in itmd: "intermed.rkt"))
(require (prefix-in assn: "assign-addr.rkt"))
(require "gen-intermed.rkt")
(require "semantic-analy.rkt")
(require "myenv.rkt")
(require rackunit)
(require racket/trace)
(provide (all-defined-out))



;命令
(struct instr (op args) #:transparent)
;ラベル
(struct label (name) #:transparent)
;ディレクティブ
(struct dir (label args) #:transparent)
;コメント
(struct comment (arg) #:transparent)
;ラベル生成用maxlabel、nextlabel、rest
(define maxlabel 0)
(define (nextlabel)
  (let* ([ret (string->symbol (string-append "L" (number->string maxlabel)))])
    (set! maxlabel (+ maxlabel 1))
    ret))
(define (reset)
  (begin (set! maxlabel 0)))
; 一時的に使用するレジスタ名の定義
(define reg1 '$t0)
(define reg2 '$t1)
; 関数が返り値を記録するためのレジスタの定義.
(define retreg '$v0)
; フレームポインタとして使うレジスタ名の定義
(define fpreg '$fp)
;引数
;構造体obj-off
;戻り値
;シンボル '|77($fp)| など
(define (addr->sym sym)
  (cond 
    ((assn:obj-off? sym) 
     (cond ((type_array? (assn:obj-off-type sym)) 
            (string->symbol (format "~a($fp)" (assn:obj-off-off (assn:obj-off-off sym)))))
           (else (string->symbol (format "~a($fp)" (assn:obj-off-off sym))))))
    ((and (obj? sym) (array_base? (obj-name sym))) 
     "これは大域変数の配列のベースアドレスであるので後で実装.")
    ((and (obj? sym) (type_array? (obj-type sym)))
     (type_array-size (obj-type sym)))
    (else (error (format "debug in addr->sym \n~a\n" sym)))))

;addrは数字
(define (makesp of)
  (string->symbol (format "~a($sp)" of)))

;引数
;itme-and-stack
(define (intermed-prog->code it-and-st)
  (let* (;itmdは中間命令文のlist
         (itmd (assn:itmd-and-stack-it it-and-st))
         ;fun-stackのlist
         (st (assn:itmd-and-stack-st it-and-st))
         ;fdsはfundefのlist
         [fds (filter (lambda (x) (itmd:fundef? x)) itmd)]
         ;localvarsizeは大域変数で確保する領域
	 [localvarsize 
          (* assn:wordsize (length (filter (lambda (x) (itmd:vardecl? x)) itmd)))]
         ;mainはmain関数のfundef
	 [main 
          (car (filter (lambda (x) (equal? 'main (obj-name (itmd:fundef-var x)))) fds))]
	 [fdscode 
          (flatten 
           (map 
            (lambda (x) (intermed-fundef->code x st))
            (remove main fds)))]
	 [maincode 
          (intermed-stmt->code localvarsize 0 (itmd:fundef-body main))])
    (flatten
     (list
      (dir '.text '())
      ;(dir '.globl '(main))
      fdscode
      (label 'main)
      (savecode localvarsize 0)
      maincode
      (restorecode localvarsize 0)))))

;fdはfundef
;stはfun-stackのlist
(define (intermed-fundef->code fd st)
  (let* (;fは関数名
         [f (obj-name (itmd:fundef-var fd))]
         ;fstはこの関数のfun-stack
         [fst 
          (car 
           (filter 
            (lambda (x) (equal? (itmd:fundef-var fd) (assn:fun-stack-fun x))) 
            st))]
         ;lacaandargsは局所変数と引数のlist
         [localandargs (assn:fun-stack-vars fst)]
         ;lacavarは局所変数のlist
         [localvar (filter (lambda (x) (> 4 (assn:obj-off-off x))) localandargs)]
         ;argsは引数のlist
         [args (filter (lambda (x) (< 0 (assn:obj-off-off x))) localandargs)]
         ;localvarsizeは局所変数のサイズ
         [localvarsize (* assn:wordsize (length localvar))]
         [argsinbytes (* assn:wordsize (length args))]
         [stmts (itmd:fundef-body fd)]
         [code (intermed-stmt->code localvarsize argsinbytes stmts)])
    (flatten 
     (list
      (label f)
      (savecode localvarsize argsinbytes)
      code
      (restorecode localvarsize argsinbytes)))))

;引数
;localvarsinbytesは局所変数の数*wordsize
;argsinbytesは引数の数*wordsize
(define (savecode localvarsinbytes argsinbytes)
  (let* (;localandrpraは局所変数と $fp と $ra を保存するのに必要なサイズ.
	 [localandfpra (+ localvarsinbytes (* assn:wordsize 2))]
	 ;framesizeはフレーム全体のサイズ
	 [framesize (+ localandfpra argsinbytes)])
    (list
     (instr 'subu `($sp $sp ,framesize))
     (instr 'sw `($ra |4($sp)|))
     (instr 'sw `($fp |0($sp)|))
     (instr 'addiu `($fp $sp ,(- localandfpra 4))))))

;引数
;localvarsinbytesは局所変数の数*wordsize
;argsinbytesは引数の数*wordsize
(define (restorecode localvarsinbytes argsinbytes)
  (let* ([localandfpra (+ localvarsinbytes 8)]
	 [framesize (+ localandfpra argsinbytes)])
    (list
     (instr 'lw `($fp |0($sp)|))
     (instr 'lw `($ra |4($sp)|))
     (instr 'addiu `($sp $sp ,framesize))
     (instr 'jr `($ra)))))

(define (intermed-stmt->code localvarsinbytes argsinbytes s)
  ;check
  ;emptystmt ok
  ;writestmt ok
  ;readstmt ok (テストコードと一致)
  ;letstmt ok
  ;ifstmt ok
  ;whilestmt ok (テストコードと一致)
  ;returnstmt ok（テストコードと一致）
  ;callstmt ok（テストコードとは一致しているが内部関数が正常に動くかどうかは調べる必要あり）
  ;printstmt ok
  ;compdstmt ok
  (cond
   [(itmd:emptystmt? s)
    (list (instr 'nop '()))]
   [(itmd:writestmt? s)
    (let* ([dest (itmd:writestmt-dest s)]
	   [symdest (addr->sym dest)]
	   [destderef (string->symbol (format "0(~a)" (symbol->string reg2)))]
	   [src (itmd:writestmt-src s)]
	   [symsrc (addr->sym src)])
      (list (instr 'lw `(,reg1 ,symsrc))
	    (instr 'lw `(,reg2 ,symdest))
	    (instr 'sw `(,reg1 ,destderef))))]
   [(itmd:readstmt? s)
    (let* ([dest (itmd:readstmt-dest s)]
           [symdest (addr->sym dest)]
           [srcderef (string->symbol (format "0(~a)" (symbol->string reg1)))]
           [src (itmd:readstmt-src s)]
           [symsrc (addr->sym src)])
      (list (instr 'lw `(,reg1 ,symsrc))
            (instr 'lw `(,reg1 ,srcderef))
            (instr 'sw `(,reg1 ,symdest))))]
   [(itmd:letstmt? s)
    (let* ([dest (itmd:letstmt-var s)]
	   [exp (itmd:letstmt-exp s)])
      (intermed-exp->code dest exp))]
   [(itmd:ifstmt? s)
    (let* ([sym (itmd:ifstmt-var s)]
	   [sym (addr->sym sym)]
	   [stmts1 (let* ((stmt1 (itmd:ifstmt-stmt1 s)))
                     (cond ((itmd:compdstmt? stmt1)
                            (itmd:compdstmt-stmts stmt1))
                           ((itmd:emptystmt? stmt1)
                            (list (itmd:emptystmt)))
                           (else (list stmt1))))]
	   [stmts2 (let* ((stmt2 (itmd:ifstmt-stmt2 s)))
                     (cond ((itmd:compdstmt? stmt2)
                            (itmd:compdstmt-stmts stmt2))
                           ((itmd:emptystmt? stmt2)
                            (list (itmd:emptystmt)))
                           (else (list stmt2))))]
	   [code1 
            (flatten 
             (map 
              (lambda (stmt) (intermed-stmt->code localvarsinbytes argsinbytes stmt)) 
              stmts1))]
           [code2 
            (flatten 
             (map 
              (lambda (stmt) (intermed-stmt->code localvarsinbytes argsinbytes stmt)) 
              stmts2))]
	   [label1 (nextlabel)]
	   [label2 (nextlabel)])
      (flatten (list 
		(instr 'lw `(,reg1 ,sym))
		(instr 'beqz `(,reg1 ,label1))
		code1
		(instr 'j `(,label2))
		(label label1)
		code2
		(label label2))))]
   [(itmd:whilestmt? s)
    (let* ([var (itmd:whilestmt-var s)]
           [var (addr->sym var)]
           [stmt (itmd:compdstmt-stmts (itmd:whilestmt-stmt s))]
           [code1 
            (flatten 
             (map 
              (lambda (st)(intermed-stmt->code localvarsinbytes argsinbytes st)) 
              stmt))]
           [label1 (nextlabel)]
           [label2 (nextlabel)])
      (flatten (list 
                (instr 'lw `(,reg1 ,var))
                (instr 'beqz `(,reg1 ,label2))
                (label label1)
                code1
                (instr 'lw `(,reg1 ,var))
                (instr 'beqz `(,reg1 ,label2))
                (instr 'j `(,label1)) 
                (label label2))))]
   [(itmd:returnstmt? s)
   (let* ((var (itmd:returnstmt-var s))
          (var (addr->sym var)))
     (list (instr 'lw `(,reg1 ,var))
           (instr 'move `(,retreg ,reg1))
           (restorecode localvarsinbytes argsinbytes)
           (instr `jr `($ra))))]
   [(itmd:callstmt? s)
    ;内部手続
    ;引数varlistは引数のlist
    ;varnumは引数の数
    ;引数の退避を行う命令文のlistを生成関数.
    (define (save-args varlist varnum)
      (cond ((equal? '() varlist) '())
            (else (let* ((arg (addr->sym (car varlist)))
                         (n (* 4 varnum))
                         (sp (makesp n))
                         (m (- varnum 1)))
                    (flatten 
                     (append 
                      (list (instr 'lw `(,reg1 ,arg))
                            (instr 'sw `(,reg1 ,sp)))
                      (save-args (cdr varlist) m)))))))
    ;;;ここまで
   (let* [;funcは関数名
          (func (obj-name (itmd:callstmt-f s)))
          ;varnumは引数の数
          (varnum (length (itmd:callstmt-vars s)))
          ;varlistは引数のlist
          (varlist (itmd:callstmt-vars s))
          (dest (itmd:callstmt-dest s))
          (dest (addr->sym dest))]
     (list (save-args varlist varnum)
           (instr 'jal `(,func))
           (instr 'sw `(,retreg ,dest))))]
   [(itmd:printstmt? s)
    (let* ([src (itmd:printstmt-var s)]
	   [symsrc (addr->sym src)])
      (list (instr 'li `($v0 1))
	    (instr 'lw `(,reg1 ,symsrc))
	    (instr 'move `($a0 ,reg1))
            (instr 'syscall '())))]
   [(itmd:compdstmt? s)
    (let* ((stmts (itmd:compdstmt-stmts s)))
      (flatten 
       (list (map 
              (lambda (x) (intermed-stmt->code localvarsinbytes argsinbytes x)) 
              stmts))))]
   [else (error (format "debug in intermed-stmt->code \n~a\n" s))]))

(define (intermed-exp->code dest e)
  ;intexp ok
  ;varexp ok
  ;aopexp ok
  ;relopexp ok
  (cond
    [(itmd:intexp? e)
     (let* ([arg (itmd:intexp-num e)]
            [val arg]
	   [symaddr (addr->sym dest)])
       (list (instr 'li `(,reg1 ,val))
             (instr 'sw `(,reg1 ,symaddr))))]
   [(itmd:varexp? e)
    (let* ([arg (itmd:varexp-var e)]
	   [symsrc (addr->sym arg)]
           [symdest (addr->sym dest)])
      (list (instr 'lw `(,reg1 ,symsrc))
            (instr 'sw `(,reg1 ,symdest))))]
   [(itmd:aopexp? e)
    (let* ([op (string->symbol 
                (format "~a" 
                        (cond ((equal? '+ (itmd:aopexp-op e)) 'add)
                              ((equal? '- (itmd:aopexp-op e)) 'sub)
                              ((equal? '* (itmd:aopexp-op e)) 'mul)
                              ((equal? '/ (itmd:aopexp-op e)) 'div))))]
           [sym1 (itmd:aopexp-var1 e)]
           [sym2 (itmd:aopexp-var2 e)]
	   [sym1 (addr->sym sym1)]
	   [sym2 (addr->sym sym2)]
	   [dest (addr->sym dest)])
      (list (instr 'lw `(,reg1 ,sym1))
	    (instr 'lw `(,reg2 ,sym2))
	    (instr op `(,reg1 ,reg1 ,reg2))
	    (instr 'sw `(,reg1 ,dest))))]
   [(itmd:relopexp? e)
    (let* ((op (string->symbol 
                (format "~a" 
                        (cond ((equal? '< (itmd:relopexp-op e)) 'slt)
                              ((equal? '== (itmd:relopexp-op e)) 'seq)
                              ((equal? '>= (itmd:relopexp-op e)) 'sge)
                              ((equal? '> (itmd:relopexp-op e)) 'sgt)
                              ((equal? '<= (itmd:relopexp-op e)) 'sle)
                              ((equal? '!= (itmd:relopexp-op e)) 'sne)))))
           (var1 (itmd:relopexp-var1 e))
           (var2 (itmd:relopexp-var2 e))
           (var1 (addr->sym var1))
           (var2 (addr->sym var2))
           (dest (addr->sym dest)))
      (list (instr 'lw `(,reg1 ,var1))
            (instr 'lw `(,reg2 ,var2))
            (instr op `(,reg1 ,reg1 ,reg2))
            (instr 'sw `(,reg1 ,dest))))]
   (else (error (format "debug in intermed-exp->code \n~a\n" e)))))



;テスト
#;(begin
  (define testcg (open-input-file "test01.c"))
  (port-count-lines! testcg)
  (define test-intermedcg 
    (assn:assign-add-intermed 
     (gen-optimized-intermed (sem-analyze-tree (k08:parse-port testcg)))))
  (display 
   (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当ての実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
  (define test-ass-itmdcg (assn:gen-assigned-itmd test-intermedcg))
  test-ass-itmdcg)

#;(begin
    (display 
     (format "\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下がアセンブリ生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
    (intermed-prog->code test-ass-itmdcg))



