#lang racket
(require (prefix-in k08: "kadai08.rkt"))
(require (prefix-in itmd: "intermed.rkt"))
(require (prefix-in assn: "assign-addr.rkt"))
(require "gen-intermed.rkt")
(require "semantic-analy.rkt")
(require "myenv.rkt")
(require rackunit)
(require racket/trace)

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
		 
; 一時的に使用するレジスタ名の定義
(define reg1 '$t0)
(define reg2 '$t1)
; 関数が返り値を記録するためのレジスタの定義.
(define retreg '$v0)
; フレームポインタとして使うレジスタ名の定義
(define fpreg '$fp)

;obj-offを受け取って,それを $fp 経由でアクセスするための
;アドレス式に変換する関数.
;引数
;構造体obj-off
;戻り値
;シンボル '|77($fp)| など
(define (addr->sym sym)
  (cond ((assn:obj-off? sym) (string->symbol (format "~a($fp)" (assn:obj-off-off sym))))
        ((and (obj? sym) (array_base? (obj-name sym))) 
         "これは大域変数の配列のベースアドレスであるので後で実装.")
        ((and (obj? sym) (type_array? (obj-type sym)))
         (type_array-size (obj-type sym)))
        (else (error (format "debug\n~a\n" sym)))))

; obj-off構造体destと中間命令の式 e を受け取って「e を評価して
; dest に結果を書き込む」という動作をする命令列を生成する関数.
;引数
;obj-off構造体と  
;中間命令の式
;intexp,varexp,aopexp,relopexp
;戻り値
;アセンブリ
(define (intermed-exp->code dest e)
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
	   [dest (addr->sym dest)])
      ; sym1 と sym2 の指す先の値を一時レジスタに読み込んで
      (list (instr 'lw `(,reg1 ,sym1))
	    (instr 'lw `(,reg2 ,sym2))
	    ; その値を op で計算し,
	    (instr op `(,reg1 ,reg1 ,reg2))
	    ; その結果を dest の指す先に書き込む
	    (instr 'sw `(,reg1 ,dest))))]
   [(itmd:relopexp? e)
    (let* ((op (string->symbol (format "s~a" (itmd:relopexp-op e))))
           (var1 (itmd:relopexp-var1 e))
           (var2 (itmd:relopexp-var2 e))
           (var1 (addr->sym var1))
           (var2 (addr->sym var2))
           (dest (addr->sym dest)))
      (list (instr 'lw `(,reg1 ,var1))
            (instr 'lw `(,reg2 ,var2))
            (instr op `(,reg1 ,reg1 ,reg2))
            (instr 'sw `(,reg1 ,dest))))]
   (else (error (format "debug\n~a\n" e)))))

; 中間命令をアセンブリに変換. localvarsinbytes と argsinbytes は
; return を変換する際に $sp や $fp を操作するために必要.
(define (intermed-stmt->code localvarsinbytes argsinbytes s)
  (cond
    ;
    ;(#t (error (format "debag\n~a\n" s)))
    ;
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
   ; 条件分岐の命令列
   [(itmd:ifstmt? s)
    (let* ([sym (itmd:ifstmt-var s)]
	   [sym (addr->sym sym)]
	   [stmts1 (let* ((stmt1 (itmd:ifstmt-stmt1 s)))
                     (cond ((itmd:compdstmt? stmt1)
                            (itmd:compdstmt-stmts stmt1))
                           ((itmd:emptystmt? stmt1)
                            (list itmd:emptystmt))
                           ((assn:obj-off? stmt1)
                            (list stmt1))
                           (else (error (format "debag\n~a\n" stmt1)))))]
	   [stmts2 (let* ((stmt2 (itmd:ifstmt-stmt2 s)))
                     (cond ((itmd:compdstmt? stmt2)
                            (itmd:compdstmt-stmts stmt2))
                           ((itmd:emptystmt? stmt2)
                            (list itmd:emptystmt))
                           ((assn:obj-off? stmt2)
                            (list stmt2))
                           (else (error (format "debag\n~a\n" stmt2)))))]
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
    (let* ([var (itmd:whilestmt-var s)]
           [var (addr->sym var)]
           [stmt (itmd:compdstmt-stmts (itmd:whilestmt-stmt s))]
           [code1 
            (flatten 
             (map (lambda (st)(intermed-stmt->code localvarsinbytes argsinbytes st)) stmt))]
           [label1 (nextlabel)]
           [label2 (nextlabel)]
           )
      (flatten (list 
                (label label1)
                (instr 'lw `(,reg1 ,var))
                (instr 'beqz `(,reg1 ,label2))
                code1
                (label label1) 
                (label label2)
                )))]
   [(itmd:returnstmt? s)
   (let* ((var (itmd:returnstmt-var s))
          (var (addr->sym var)))
     (list (instr 'lw `(,reg1 ,var))
           (instr 'move `(,retreg ,reg1))
           (restorecode localvarsinbytes argsinbytes)
           (instr 'jr `($ra)))
     )]
   [(itmd:callstmt? s)
   (let* 
       #;[(func (itmd:callstmt-f s))
          (sourvars (itmd:callstmt-vars s))
          (sourvars (addr->sym sourvars))
          (dest (itmd:callstmt-dest s))
          (dest (addr->sym dest))]
     ((meanigless '()))
     #;(list (instr 'lw `(,reg1 ,sourvars))
           (instr 'sw `(,reg1 ,dest))
           (instr 'lw `(,reg1 ,dest))
           (instr 'sw `(,reg1 ,sourvars))
           (instr 'jal `(,func))
           (instr 'sw `(,retreg)))
     ;あとから実装
     '())]
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
       (list (map (lambda (x) (intermed-stmt->code localvarsinbytes argsinbytes x)) stmts))))]
   [else (error (format "debug\n~a\n" s))]
   ))
(trace intermed-stmt->code)

(define (intermed-fundef->code fd)
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
    '()
    ))

;引数
;itme-and-stack
(define (intermed-prog->code it-and-st)
  (let* ((itmd (assn:itmd-and-stack-it it-and-st))
         ;fdsはmani関数以外のfundefのlist
         [fds (filter (lambda (x) (itmd:fundef? x)) itmd)]
	 [localvarsize (* assn:wordsize (length (filter (lambda (x) (itmd:vardecl? x)) itmd)))]
         ;mainはmain関数のfundef
	 [main (car 
                (filter 
                 (lambda (x) (equal? 'main (obj-name (itmd:fundef-var x)))) 
                 fds))]
	 [fdscode 
          (flatten (map intermed-fundef->code fds))]
         ;
         ;(debag (error (format "debag\n~a\n" (itmd:compdstmt-stmts (itmd:fundef-body main)))))
         ;
	 [maincode 
          (flatten (map 
                    (lambda (s) (intermed-stmt->code localvarsize 0 s)) 
                    (itmd:compdstmt-stmts (itmd:fundef-body main))))])
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

;テスト
(define p (open-input-file "test01.c"))
(port-count-lines! p)
(define test-itmd
  (assn:gen-assigned-itmd
   (assn:assign-add-intermed 
    (gen-optimized-intermed (sem-analyze-tree (k08:parse-port p))))))

(intermed-prog->code test-itmd)


(provide (all-defined-out))
