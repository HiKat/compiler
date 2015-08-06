#コンパイラ  
計算機科学実験3Bソフトウェアで作成したC言語のサブセット言語のSmall Cのコンパイラ.  
実装言語はRacket  
[Racket公式ドキュメント](http://docs.racket-lang.org/)  
#Small CのBNF（現在作成中）    
(opt付きはなくても良い)  
\<program> := \<external-declaration> | \<program>\<external-declaration>  
\<external-declaration> := \<declaration> | \<function-prototyep> | \<function-definition>  
\<declaration> := \<type-specifier> \<declrator-list> ;  
\<declarator-list> := \<declarator> | \<declarator-list> , \<declarator>  
\<declarator> := \<direct-declarator>  | * \<direct-declaraotr>  
\<direct-declrator> := \<identifier>  | \<identifier> [ \<constant> ]  
\<function-prototype> := \<type-specfier> \<function-declarator> ;  
\<function-declarator> := \<identifier> ( \<parameter-type-list>opt ) | * \<identifier> ( \<parameter-type-list>opt )  
\<function-defintion> := \<type-specifier> \<function-declarator> \<compound-statement>  
\<parameter-type-list> := \<parameter-declaration> | \<parameter-type-list> , \<parameter-declaration>  
\<parameter-declaration> := \<type-specifier> \<parameter-declarator>  
\<parameter-declarator> := \<identifer> | * \<identifier>  
\<type-specifer> := int | void  
\<statement> := ; | \<expression> ; | \<compound-statement> | if ( \<expression> ) \<statement> | if ( \<expression> ) \<statement> else \<statement> | while ( \<expression> ) \<statement> | for ( \<expression>opt ; \<expression>opt ; \<expression>opt ) \<statement> | return \<expression>opt ;  




#実行方法  
compiler.rkt内の関数compileの引数にファイル名を入れて実行。MIPSアセンブリを戻り値として返す。実行はMIPSシュミレーターで（QTspimなど）  

```lisp:compiler.rkt<!--Racket言語が対応していないので臨時でlispで-->
(compile "basic/arith.sc")
```

テストファイルはbasic以下に。（コンパイラとして最低限の実装）その他advance以下のファイルはほとんど通らない。またerror以下のテストファイルは

```lisp:compiler.rkt<!--Racket言語が対応していないので臨時でlispで-->
(compile "error/name02.sc")
(compile "error/name04.sc")
(compile "error/name05.sc")
(compile "error/name07.sc")
(compile "error/name08.sc")
(compile "error/name09.sc")
(compile "error/name10.sc")
(compile "error/name11.sc")
(compile "error/shape01.sc")
(compile "error/shape03.sc")
(compile "error/type01.sc")
(compile "error/type02.sc")
(compile "error/type03.sc")
(compile "error/type04.sc")
(compile "error/type05.sc")
(compile "error/type06.sc")
(compile "error/type08.sc")
(compile "error/type09.sc")
(compile "error/type10.sc")
(compile "error/type11.sc")
(compile "error/type12.sc")
(compile "error/type13.sc")
(compile "error/type14.sc")
(compile "error/type15.sc")
(compile "error/type16.sc")
(compile "error/type17.sc")
(compile "error/type18.sc")
```

についてはエラーを検出する。（ただしエラーメッセージが適切で無いものも。）またその他のエラーついては明白に式の形がおかしいものなどもコンパイラの側が「main関数がない」といった本質的ではない部分でエラーを検出してしまっている。  

#留意点  
関数フレーム作成時は引数をレジスタを介さず全てメモリ経由で引き渡している。  
作成期間は一ヶ月ほどなのでバグだらけ（仮に時間かけても完璧なものが作れるかは謎だけど）  
頑張ったほうだと思う。    










