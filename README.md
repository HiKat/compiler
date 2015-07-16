# compiler
##意味解析部  
###analy-declaration_st(チェック部分開発途中)  
;(stx:declaration_st...) と  
;__分析に使う環境env__と   
;__current-lev__  
;を受け取って  
;(stx;declaration_st type-spec (list (obj...) (obj...)...))  
;を返す.  
;同時にlistの形で環境に追加.  
;同時に環境のチェックも行う.  

###analy-func_proto_st  
;(stx:func_proto_st...)  
;を受け取って  
;(stx:func_proto_st (stx:spec_st...)     
;                   (stx:func_declarator/_ast/_st 関数名  
;                                (list obj...)))  
;を返し    
;同時に関数プロトタイプのobject(obj name 0 'proto type)を  
;環境に登録.  
;パラメータのobject(obj name 1 'parm type)の(list obj...)を  
;パラメータ専用の環境をまず初期化してから登録  

###analy-func_def_st  
;stx:func_def_stを  
;引数に取り   
;(stx:func_def_st stx:spec_st   
;                 (func_declarator_st '関数宣言のオブジェクト'  
;                                     'パラメータのオブジェクトのlist')  
;                 compound-statement)  
;(compound-statement部分については関数analy-compound_stに任せる.)  
;を返す.  
;同時にパラメータのオブジェクトをパラメータ専用の環境に追加、チェック  
;同時に関数宣言のオブジェクトを環境に追加、チェック 

###analy-compound_st  
;stx:compound_stか  
;stx:compound_dec_stか  
;stx:compound_sta_stか  
;stx:compound_null_stと 
;lev  
;を受け取って    
;(stx:compound_st declaration-list statement-list)  
;を返す.    
;ただし   
;declaration-listが無いときは'nodecl  
;statement-listが無いときは'nostat  
;を入れる  
;同時に  
;意味解析開始時にcurrent-levをひとつ上げる  
;終了時に1つ下げる  

###analy-compdecl  
;__analy-declaration_stの派生__  
;(list* (stx:declaration_st...)...)と  
;lev（解析中のcompound-statementのブロックレベル）  
;を引数に取り  
;(list* obj)　　
;を返す関数  
;_analy-declarationとは違って外部の大域の環境を更新しない._  
;compound-statementの意味解析結果の環境としては(list* obj)を直接使用することとする.  

###analy-compstate  
;levと  
;envと  
;compound_stなどに入るstatementを  
;引数に取り  
;それぞれのobjを  
;返す関数
;同時にenvをもとにstatement内の定義をチェックする.  










