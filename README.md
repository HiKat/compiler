#コンパイラ  
計算機科学実験3Bソフトウェアで作成したC言語のサブセット言語のSmall Cのコンパイラ.  
実装言語はRacket  
[Racket公式ドキュメント](http://docs.racket-lang.org/)  
#Small CのBNF  
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
\<statement> := ; | <expression> ; | <compound-statement> | if ( <expression> ) <statement> | if ( <expression> ) <statement> else <statement> | while ( <expression> ) <statement> | for ( <expression>opt ; <expression>opt ; <expression>opt ) <statement> | return <expression>opt ;  




#現在作成中  






