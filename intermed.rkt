#lang racket
(provide (all-defined-out))
;;;; プログラムは vardecl とfundef のリスト
;; 変数宣言
(struct vardecl (var) #:transparent)
;; 関数定義
(struct fundef (var parms body) #:transparent) 
; vardecl のリスト
;;;; 文
;; 空文: ;
(struct emptystmt () #:transparent)
;; 変数への代入: <var> = <exp>;
(struct letstmt (var exp) #:transparent)
;; メモリへの書き込み: *<dest> = <src>;
(struct writestmt (dest src) #:transparent)
;; メモリ参照: <dest> = *<src>;
(struct readstmt (dest src) #:transparent)
;; 条件分岐: if(<var>) <stmt1> <stmt2>
(struct ifstmt (var stmt1 stmt2) #:transparent)
;; 繰り返し: while(<var>) <stmt1>
(struct whilestmt (var stmt) #:transparent)
;; 関数呼出し: <dest> = <f>(<var1>, <var2>, <var3>, ...);
(struct callstmt (dest f vars) #:transparent) ; vars はvar のリスト
;; リターン: return <var>;
(struct returnstmt (var) #:transparent)
;; 値の出力: print(<var>);
(struct printstmt (var) #:transparent)
;; 複文: compdstmt
(struct compdstmt (decls stmts) #:transparent) ; decls は
; vardecl のリスト
; stmts は文のリスト
;;;; 式
;; 変数参照
(struct varexp (var) #:transparent)
;; 整数即値
(struct intexp (num) #:transparent)
;; 算術演算
(struct aopexp (op var1 var2) #:transparent)
;; 比較演算
(struct relopexp (op var1 var2) #:transparent)
;; アドレス取得: &<var>
(struct addrexp (var) #:transparent)