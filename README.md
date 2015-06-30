# compiler
##意味解析部
###analy-declaration_st(チェック部分開発途中)  
(stx:declaration_st...)  
を受け取って  
(stx;declaration_st type-spec (list (obj...) (obj...)...))  
を返す.  
同時にlistの形で環境に追加.  
同時に環境のチェックも行う.  

###analy-func_proto_st(チェック部分開発途中)  
(stx:func_proto_st...)  
を受け取って  
(stx:func_proto_st (stx:spec_st...)     
                   (stx:func_declarator/_ast/_st 関数名  
                                (list obj...)))  
を返し    
同時に関数プロトタイプのobject(obj name 0 'proto type)を  
環境に登録.  
パラメータのobject(obj name 1 'parm type)の(list obj...)を  
パラメータ専用の環境をまず初期化してから登録  

###analy-func_def_st(チェック部分、compound-statement引き渡し開発途中)
stx:func_def_stを  
引数に取り   
(stx:func_def_st stx:spec_st   
                 (func_declarator_st '関数宣言のオブジェクト'  
                                     'パラメータのオブジェクトのlist')  
                 compound-statement)  
(compound-statement部分については関数analy-compoundに任せる.)  
を返す.  
同時にパラメータのオブジェクトをパラメータ専用の環境に追加、チェック  
同時に関数宣言のオブジェクトを環境に追加、チェック  



