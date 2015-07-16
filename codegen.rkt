
#lang racket

(require (prefix-in itmd: "intermed.rkt"))
(require (prefix-in assn: "assign-addr.rkt"))
(require rackunit)

;;;;;;;;;;;; アセンブリの一行一行を表す構造体.;;;;;;;;;;;;;

; printcode.rkt を見ればそれぞれがどのようなアセンブリに対応するかが分
; かりやすいはず.

; 命令を表す構造体.op は 'li とか 'addiu のように命令に対応するシンボ
; ル.args は引数のリスト.
(struct instr (op args) #:transparent)

; ラベル (main: とか) を表す構造体.name はラベル名のシンボル.
(struct label (name) #:transparent)

; ディレクティブ (.text とか) を表す構造体
(struct dir (label args) #:transparent)

; コメントを表す構造体.生成するコードにデバッグ用に何かマークを付けた
; いときに使う.
(struct comment (arg) #:transparent)

; コード (i.e., 命令のリスト) であるかどうかを判定する述語.
(define code?
  (listof (or/c instr? label? dir? comment?)))

(define maxlabel 0)
; 新しいラベルを作る関数.
(define (nextlabel)
  (let* ([ret (string->symbol (string-append "L" (number->string maxlabel)))])
    (set! maxlabel (+ maxlabel 1))
    ret))
(define (reset)
  (begin (set! maxlabel 0)))

; 関数フレームを作り,$ra と $fp をフレームに退避し,$fp を適切な値にセッ
; トする命令列を出力する.関数の入り口でこの命令列が実行される.
; localvarsinbytes は関数が使用する局所変数のサイズ.argsinbytes は関数
; にスタック経由で渡す値のサイズ.これらのサイズをもとに $sp や $fp を
; 動かす量を決める.
(define (savecode localvarsinbytes argsinbytes)
  (let* (; 局所変数と $fp と $ra を保存するのに必要なサイズ.
	 [localandfpra (+ localvarsinbytes (* assn:wordsize 2))]
	 ; フレーム全体のサイズ
	 [framesize (+ localandfpra argsinbytes)])
    (list
     ; `($sp $sp ,framesize) は quasiquote と呼ばれる仕組み.('$sp
     ; '$sp '<framesizeの値>) というシンボルのリストに評価される.たと
     ; えば,framesize が 12 だったら ('$sp '$sp '12) というリストにな
     ; る.
     (instr 'subu `($sp $sp ,framesize))
     ; '4($sp) というシンボルを作りたいが,カッコは普通のシンタックスで
     ; はシンボルの一部にできない.そのため,|4($sp)| というように | で
     ; 囲ってやる必要がある.詳細は
     ; http://docs.racket-lang.org/guide/symbols.html?q=symbol#%28tech._symbol%29
     ; を参照のこと.
     (instr 'sw `($ra |4($sp)|))
     (instr 'sw `($fp |0($sp)|))
     ; $fp をセット.
     (instr 'addiu `($fp $sp ,(- localandfpra 4))))))

(test-begin
 (check-equal?
  (savecode 12 4)
  (list
   (instr 'subu `($sp $sp 24))
   (instr 'sw `($ra |4($sp)|))
   (instr 'sw `($fp |0($sp)|))
   (instr 'addiu `($fp $sp 16)))))

; 関数の出口で実行されるべき命令列を出力.$fp と $ra を関数が呼び出され
; た時点での値に戻し (すなわち,退避しておいた値に戻し),$sp を元に戻し,
; $ra に記録されている場所にジャンプする.
(define (restorecode localvarsinbytes argsinbytes)
  (let* ([localandfpra (+ localvarsinbytes 8)]
	 [framesize (+ localandfpra argsinbytes)])
    (list
     ; 退避しておいた $fp を復帰
     (instr 'lw `($fp |0($sp)|))
     ; 退避しておいた $ra を復帰
     (instr 'lw `($ra |4($sp)|))
     ; $sp の値を戻す
     (instr 'addiu `($sp $sp ,framesize))
     ; いまや $ra は関数が呼び出されたときにセットされていた値に戻って
     ; いるので,そこに記録されている場所に jr 命令で戻る.
     (instr 'jr `($ra)))))

(test-begin
 (check-equal?
  (restorecode 12 4)
  (list
   (instr 'lw `($fp |0($sp)|))
   (instr 'lw `($ra |4($sp)|))
   (instr 'addiu `($sp $sp 24))
   (instr 'jr `($ra)))))
		 
; 一時的に使用するレジスタ名の定義
(define reg1 '$t0)
(define reg2 '$t1)
; 関数が返り値を記録するためのレジスタの定義.
(define retreg '$v0)
; フレームポインタとして使うレジスタ名の定義
(define fpreg '$fp)

; assn:ofsid 型の値を受け取って,それを $fp 経由でアクセスするためのア
; ドレス式に変換する関数.正直この辺はもうちょっとシンプルに書く設計が
; あるような気がするが,時間が無いのでこのままで.
(define (addr->sym sym)
  (string->symbol (format "~a($fp)" (assn:obj-off-off sym))))

; assn:ofsid 型の値 dest と中間命令の式 e を受け取って「e を評価して
; dest に結果を書き込む」という動作をする命令列を生成する関数.
(define (intermed-exp->code dest e)
  ;(-> itmd:exp? code?)
  (cond
   #;[(itmd:boolexp? e)
    (let* ([arg (itmd:boolexp-arg e)]
	   ; true を即値の 1,false を 即値の 0 で表現.
	   [val (if arg 1 0)] [symaddr (addr->sym dest)])
      ; 一時レジスタに val を li 命令でロードしてから,
      (list (instr 'li `(,reg1 ,val))
	    ; その値を指定されたアドレスに書き込む
	    (instr 'sw `(,reg1 ,symaddr))))]
   [(itmd:intexp? e)
    ; boolexp と同じように
    (let* ([arg (itmd:intexp-num e)]
	   [val arg]
	   [symaddr (addr->sym dest)])
      (list (instr 'li `(,reg1 ,val))
	    (instr 'sw `(,reg1 ,symaddr))))]
   [(itmd:varexp? e)
    (let* ([arg (itmd:varexp-var e)]
	   [symsrc (addr->sym arg)]
	   [symdest (addr->sym dest)])
      ; symsrc のアドレスに書いてある値を一時レジスタに読み込んで
      (list (instr 'lw `(,reg1 ,symsrc))
	    ; その値を dest の指す先に書き込む
	    (instr 'sw `(,reg1 ,symdest))))]
   [(itmd:aopexp? e)
    (let* ([op (itmd:aopexp-op e)]
	   [sym1 (itmd:aopexp-var1 e)]
	   [sym2 (itmd:aopexp-var2 e)]
	   [sym1 (addr->sym sym1)]
	   [sym2 (addr->sym sym2)]
	   [symdest (addr->sym dest)])
      ; sym1 と sym2 の指す先の値を一時レジスタに読み込んで
      (list (instr 'lw `(,reg1 ,sym1))
	    (instr 'lw `(,reg2 ,sym2))
	    ; その値を op で計算し,
	    (instr op `(,reg1 ,reg1 ,reg2))
	    ; その結果を dest の指す先に書き込む
	    (instr 'sw `(,reg1 ,symdest))))]
   [(itmd:relopexp? e)
    (error 'intermed-exp->code "Not implemented.")
   ]))

; 中間命令をアセンブリに変換. localvarsinbytes と argsinbytes は
; return を変換する際に $sp や $fp を操作するために必要.
(define (intermed-stmt->code localvarsinbytes argsinbytes s)
  ;(-> number? number? itmd:stmt? code?)
  (cond
   [(itmd:emptystmt? s)
    (list (instr 'nop '()))]
   [(itmd:writestmt? s)
    ; *dest = src に相当する命令列は...
    (let* ([dest (itmd:writestmt-dest s)]
	   [symdest (addr->sym dest)]
	   [destderef (string->symbol (format "0(~a)" (symbol->string reg2)))]
	   [src (itmd:writestmt-src s)]
	   [symsrc (addr->sym src)])
      ; ... src の格納場所に書いてある値を reg1 に読み込んで
      (list (instr 'lw `(,reg1 ,symsrc))
	    ; dest の格納場所に書いてある値を reg2 に読み込んで
	    (instr 'lw `(,reg2 ,symdest))
	    ; reg2 の値をアドレスと思ってそのアドレスに reg1 を書き込む
	    (instr 'sw `(,reg1 ,destderef))))]
   [(itmd:readstmt? s)
    ; 実装せよ.
    (error 'intermed-stmt->code "Not implemented.")]
   [(itmd:letstmt? s)
    (let* ([dest (itmd:letstmt-var s)]
	   [exp (itmd:letstmt-exp s)])
      (intermed-exp->code dest exp))]
   ; 条件分岐の命令列
   [(itmd:ifstmt? s)
    (let* ([sym (itmd:ifstmt-var s)]
	   [sym (addr->sym sym)]
	   [stmts1 (itmd:ifstmt-stmt1 s)]
	   [stmts2 (itmd:ifstmt-stmt2 s)]
	   ; then 節,else 節の命令列を作っておく
	   [code1 (flatten (map (lambda (stmt) (intermed-stmt->code localvarsinbytes argsinbytes stmt)) stmts1))]
	   [code2 (flatten (map (lambda (stmt) (intermed-stmt->code localvarsinbytes argsinbytes stmt)) stmts2))]
	   ; 条件分岐の実装に必要なラベルを2つ作っておく
	   [label1 (nextlabel)]
	   [label2 (nextlabel)])
      (flatten (list 
		; sym の記憶領域に格納されている値が...
		(instr 'lw `(,reg1 ,sym))
		; ...0 (i.e., false) ならば,label1 に飛ぶ.そうでない
		; ならば次以降の命令を実行
		(instr 'beqz `(,reg1 ,label1))
		; sym が true だったときに実行される命令 (i.e., then 節)
		code1
		; label2 に飛ぶ.
		(instr 'j `(,label2))
		(label label1)
		; sym が false だったときに実行される命令 (i.e., else 節)
		code2
		; then 節に対応する命令の末尾でここに飛んでくる.
		(label label2))))]
   [(itmd:whilestmt? s)
    ; 実装せよ
    (error 'intermed-stmt->code "Not implemented.")]
   [(itmd:returnstmt? s)
    (error 'intermed-stmt->code "Not implemented.")]
   [(itmd:callstmt? s)
    (error 'intermed-stmt->code "Not implemented.")]
   [(itmd:printstmt? s)
    (let* ([src (itmd:printstmt-var s)]
	   [symsrc (addr->sym src)])
      (list (instr 'li `($v0 1))
	    (instr 'lw `(,reg1 ,symsrc))
	    (instr 'move `($a0 ,reg1))
	    (instr 'syscall '())))]
   ))

(define/contract (intermed-fundef->code fd)
  (-> itmd:fundef? code?)
  (let* #;([f (itmd:fundef-f fd)]
	 [localvarsize (itmd:fundef-localvarsize fd)]
	 [args (itmd:fundef-args fd)]
	 [argsinbytes (* assn:wordsize (length args))]
	 [stmts (itmd:fundef-stmts fd)]
	 [code (flatten (map (lambda (stmt) (intermed-stmt->code localvarsize argsinbytes stmt)) stmts))])
    ()
    #;(flatten 
     (list
      (label f)
      ; 関数の頭に $sp 等を操作するコードをつける
      (savecode localvarsize argsinbytes)
      code
      ; これはいらん気がする
      (restorecode localvarsize argsinbytes)))
    '()))


(define/contract (intermed-prog->code p)
  (-> itmd:prog? code?)
  (let* ([fds (itmd:prog-fundefs p)]
	 [localvarsize (itmd:prog-localvarsize p)]
	 [main (itmd:prog-mainstmts p)]
	 [fdscode (flatten (map intermed-fundef->code fds))]
	 [maincode (flatten (map (lambda (s) (intermed-stmt->code localvarsize 0 s)) main))])
    (flatten
     (list
      (dir '.text '())
      (dir '.globl `(main))
      fdscode
      (label 'main)
      (savecode localvarsize 0)
      maincode
      ; これはいらん気がする
      (restorecode localvarsize 0)))))
 
;;; TEST ;;;
(test-begin
 (check-equal?
  (addr->sym (assn:ofsid 4))
  '|4($fp)|))

(test-begin
 (check-equal?
  (intermed-exp->code (assn:ofsid 4) (itmd:boolexp #t))
  (list (instr 'li '($t0 0))
	(instr 'sw '($t0 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-exp->code (assn:ofsid 4) (itmd:boolexp #f))
  (list (instr 'li '($t0 1))
	(instr 'sw '($t0 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-exp->code (assn:ofsid 4) (itmd:intexp 9))
  (list (instr 'li '($t0 9))
	(instr 'sw '($t0 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-exp->code (assn:ofsid 4) (itmd:varexp (assn:ofsid 8)))
  (list (instr 'lw '($t0 |8($fp)|))
	(instr 'sw '($t0 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 0 0 (itmd:skipstmt))
  (list (instr 'nop '()))))

(test-begin
 (check-equal?
  (intermed-stmt->code 4 0 (itmd:letstmt (assn:ofsid 4) (itmd:intexp 2)))
  (list (instr 'li '($t0 2))
	(instr 'sw '($t0 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 8 0 (itmd:printstmt (assn:ofsid 8)))
  (list (instr 'li '($v0 1))
	(instr 'lw '($t0 |8($fp)|))
	(instr 'move '($a0 $t0))
	(instr 'syscall '()))))

(test-begin
 (check-equal?
  (intermed-stmt->code 12 0 (itmd:letstmt (assn:ofsid 4) (itmd:aopexp 'add (assn:ofsid 8) (assn:ofsid 12))))
  (list (instr 'lw `(,reg1 |8($fp)|))
	(instr 'lw `(,reg2 |12($fp)|))
	(instr 'add `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 12 0 (itmd:letstmt (assn:ofsid 4) (itmd:aopexp 'sub (assn:ofsid 4) (assn:ofsid 12))))
  (list (instr 'lw `(,reg1 |4($fp)|))
	(instr 'lw `(,reg2 |12($fp)|))
	(instr 'sub `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 8 0 (itmd:letstmt (assn:ofsid 4) (itmd:aopexp 'mul (assn:ofsid 8) (assn:ofsid 0))))
  (list (instr 'lw `(,reg1 |8($fp)|))
	(instr 'lw `(,reg2 |0($fp)|))
	(instr 'mul `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 8 0 (itmd:letstmt (assn:ofsid 4) (itmd:aopexp 'div (assn:ofsid 8) (assn:ofsid 0))))
  (list (instr 'lw `(,reg1 |8($fp)|))
	(instr 'lw `(,reg2 |0($fp)|))
	(instr 'div `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 8 0 (itmd:letstmt (assn:ofsid 4) (itmd:aopexp 'rem (assn:ofsid 8) (assn:ofsid 0))))
  (list (instr 'lw `(,reg1 |8($fp)|))
	(instr 'lw `(,reg2 |0($fp)|))
	(instr 'rem `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 48 0 (itmd:letstmt (assn:ofsid 4) (itmd:relopexp 'lt (assn:ofsid 24) (assn:ofsid 48))))
  (list (instr 'lw `(,reg1 |24($fp)|))
	(instr 'lw `(,reg2 |48($fp)|))
	(instr 'slt `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 48 0 (itmd:letstmt (assn:ofsid 4) (itmd:relopexp 'eq (assn:ofsid 24) (assn:ofsid 48))))
  (list (instr 'lw `(,reg1 |24($fp)|))
	(instr 'lw `(,reg2 |48($fp)|))
	(instr 'seq `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 48 0 (itmd:letstmt (assn:ofsid 4) (itmd:relopexp 'ge (assn:ofsid 24) (assn:ofsid 48))))
  (list (instr 'lw `(,reg1 |24($fp)|))
	(instr 'lw `(,reg2 |48($fp)|))
	(instr 'sge `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 48 0 (itmd:letstmt (assn:ofsid 4) (itmd:relopexp 'gt (assn:ofsid 24) (assn:ofsid 48))))
  (list (instr 'lw `(,reg1 |24($fp)|))
	(instr 'lw `(,reg2 |48($fp)|))
	(instr 'sgt `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 48 0 (itmd:letstmt (assn:ofsid 4) (itmd:relopexp 'le (assn:ofsid 24) (assn:ofsid 48))))
  (list (instr 'lw `(,reg1 |24($fp)|))
	(instr 'lw `(,reg2 |48($fp)|))
	(instr 'sle `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 48 0 (itmd:letstmt (assn:ofsid 4) (itmd:relopexp 'ne (assn:ofsid 24) (assn:ofsid 48))))
  (list (instr 'lw `(,reg1 |24($fp)|))
	(instr 'lw `(,reg2 |48($fp)|))
	(instr 'sne `(,reg1 ,reg1 ,reg2))
	(instr 'sw `(,reg1 |4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-stmt->code 24 0 (itmd:writestmt (assn:ofsid 4) (assn:ofsid 24)))
  (let ([symreg2deref (string->symbol (format "0(~a)" (symbol->string reg2)))])
    (list (instr 'lw `(,reg1 |24($fp)|))
	  (instr 'lw `(,reg2 |4($fp)|))
	  (instr 'sw `(,reg1 ,symreg2deref))))))

(test-begin
 (let* ([symreg1deref (string->symbol (format "0(~a)" (symbol->string reg1)))])
   (check-equal?
    (intermed-stmt->code 24 0 (itmd:readstmt (assn:ofsid 4) (assn:ofsid 24)))
    (list (instr 'lw `(,reg1 |24($fp)|))
	  (instr 'lw `(,reg1 ,symreg1deref))
	  (instr 'sw `(,reg1 |4($fp)|))))))

(test-begin
 (let*
     ([code1 (intermed-stmt->code 
	      12 0
	      (itmd:ifstmt 
	       (assn:ofsid 4) 
	       (list (itmd:letstmt (assn:ofsid 8) (itmd:varexp (assn:ofsid 4))))
	       (list (itmd:letstmt (assn:ofsid 12) (itmd:varexp (assn:ofsid 4))))))]
      [_ (reset)]
      [label1 (nextlabel)]
      [label2 (nextlabel)]
      [code2 (list 
	      (instr 'lw `(,reg1 |4($fp)|))
	      (instr 'beqz `(,reg1 ,label1))
	      (instr 'lw `(,reg1 |4($fp)|))
	      (instr 'sw `(,reg1 |8($fp)|))
	      (instr 'j `(,label2))
	      (label label1)
	      (instr 'lw `(,reg1 |4($fp)|))
	      (instr 'sw `(,reg1 |12($fp)|))
	      (label label2))]
      [_ (reset)])
   (check-equal? code1 code2)))

(test-begin
 (let*
     ([code1 (intermed-stmt->code
	      4 0
	      (itmd:whilestmt
	       (assn:ofsid 4)
	       (list (itmd:skipstmt))))]
      [_ (reset)]
      [label1 (nextlabel)]
      [label2 (nextlabel)]
      [code2 (list
	      (instr 'lw `(,reg1 |4($fp)|))
	      (instr 'beqz `(,reg1 ,label2))
	      (label label1)
	      (instr 'nop `())
	      (instr 'lw `(,reg1 |4($fp)|))
	      (instr 'beqz `(,reg1 ,label2))
	      (instr 'j `(,label1))
	      (label label2))]
      [_ (reset)])
   (check-equal? code1 code2)))

(test-begin
 (check-equal?
  (intermed-stmt->code 4 0 (itmd:returnstmt (assn:ofsid -4)))
  (flatten
   (list (instr 'lw `(,reg1 |-4($fp)|))
	 (instr 'move `(,retreg ,reg1))
	 (restorecode 4 0)
	 (instr 'jr `($ra))))))

(test-begin
 (check-equal?
  (intermed-stmt->code 8 0 (itmd:callstmt (assn:ofsid -4) 'f `(,(assn:ofsid -4) ,(assn:ofsid -8))))
  ;; Notice that the parameter is stored below the current stack pointer (see README).
  ;; First paramter at lower memory.
  (list (instr 'lw `(,reg1 |-4($fp)|))
	(instr 'sw `(,reg1 |-8($sp)|))
	(instr 'lw `(,reg1 |-8($fp)|))
	(instr 'sw `(,reg1 |-4($sp)|))
	(instr 'jal `(f))
	(instr 'sw `(,retreg |-4($fp)|)))))

(test-begin
 (check-equal?
  (intermed-fundef->code 
   (itmd:fundef 
    'f 4 `(,(assn:ofsid 4) ,(assn:ofsid 8))
    (list (itmd:letstmt (assn:ofsid -4) (itmd:varexp (assn:ofsid 8))))))
  (flatten (list 
	    (label 'f)
	    (savecode 4 8)
	    (instr 'lw `(,reg1 |8($fp)|))
	    (instr 'sw `(,reg1 |-4($fp)|))
	    (restorecode 4 8)))))

#; prog
   
	
(provide (all-defined-out))
