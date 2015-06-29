#lang racket
; このモジュールを使うモジュールは,上で define された関数等を参照できる旨を宣言.
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;データ型定義;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct declaration_st (type-spec declarator-list)#:transparent)
;例) (int (id1 id2 *id3))

(struct func_declarator_st (name para-list pos)#:transparent)
(struct func_declarator_null_st (name pos)#:transparent)
(struct func_declarator_ast_st (name para-list pos)#:transparent)
(struct func_declarator_ast_null_st (name pos)#:transparent)
;例)nameは関数名、para-listは引数のリスト.
;意味解析後はfunc_declaration_stとfunc_declaration_ast_stに
;集約される. parameterなしはpara-listに代わりにシンボル'noparaが入る.
;意味解析後はname部にプロトタイプのオブジェクトが、para-listにパラメータのオブジェクトのリストが入る.

(struct func_proto_st (type-spec func-declarator-st)#:transparent)
(struct func_def_st (type-spec func-declarator-st compound-state-list)#:transparent)
;例) (int funcname1 (id1 id2 *id3) compound_list) compound_listの中身は構造体declaration_listとstatement_listからなる.

(struct declarator_st (var)#:transparent)
(struct declarator_ast_st (var)#:transparent)
;declaratorの定義
;declaration_listは構造体declarationがリストになったもの.以下で定義.
;(struct declaration_st2 (type-spec para-list)#:transparent);;;;;;
;例) (int (id1 id2 *id3 id4[5]))

(struct para_declaration_st (type-spec para)#:transparent)
;例) (int id1)

(struct exp_st (exp)#:transparent)
;式を格納する構造体.

(struct assign_exp_st (dest src pos)#:transparent)
;例)代入を表す構造体. x = 3なら(x 3)

(struct logic_exp_st (log-ope op1 op2 pos)#:transparent)
;例) (or a b)もしくは(and a b)

(struct rel_exp_st (rel-ope op1 op2 pos)#:transparent)
;例) rel_opeは'equal 'not 'less 'and_less 'more 'and_moreで (less a b)など

(struct alge_exp_st (alge-ope op1 op2 pos)#:transparent)
;例) alge_opeは'add 'sub 'mul 'divで('add a b)など

(struct id_st (name pos)#:transparent)
(struct id_ast_st (name pos)#:transparent)
;終端記号 identifierを表す構造体.

(struct array_st (name num pos)#:transparent);宣言時.posはnameの位置.
(struct array_var_st (name num pos)#:transparent);式の中で用いる.posはnameの位置.
;終端記号となりうる配列を表す構造体.

(struct spec_st (type pos)#:transparent)
;データ型を表す構造体

(struct unary_exp_st (mark op pos)#:transparent)
;postfix_expを表す構造体.markは'minus、'ast、'amp

(struct constant_st (cons pos)#:transparent)
;定数を表す構造体.

(struct null_statement_st (null)#:transparent)
;セミコロンのみからなるstatementを表す構造体

(struct exp_with_semi_st (exp)#:transparent)
;expressionとセミコロンからなる式を表す構造体.

(struct exp_in_paren_st (exp)#:transparent)
;()で囲まれたexpression

(struct if_st (cond-exp state pos)#:transparent);else無し.posはifの位置.
(struct if_else_st (cond-exp state else-state if-pos else-pos)#:transparent);elseあり
;if文を表す構造体.

(struct while_st (cond-exp statement pos)#:transparent);posはwhileの位置.
;while文を表す構造体.

(struct for_0_st (cond-exp1 cond-exp2 cond-exp3 statement pos)#:transparent)
(struct for_1_st (cond-exp1 cond-exp2 statement pos)#:transparent)
(struct for_2_st (cond-exp1 cond-exp2 statement pos)#:transparent)
(struct for_3_st (cond-exp1 cond-exp2 statement pos)#:transparent)
(struct for_4_st (cond-exp1 statement pos)#:transparent)
(struct for_5_st (cond-exp1 statement pos)#:transparent)
(struct for_6_st (cond-exp1 statement pos)#:transparent)
(struct for_7_st (statement pos)#:transparent)
;for文を表す構造体.0〜7の数字によってnullの位置が構文木作成の時点でわかる.

(struct return_st (exp pos)#:transparent);posはreturnの位置.
(struct return_null_st (exp pos)#:transparent)
;return文を表す構造体.

(struct compound_st (declaration-list statement-list)#:transparent)
(struct compound_dec_st (declaration-list)#:transparent)
(struct compound_sta_st (statement-list)#:transparent)
(struct compound_null_st (null)#:transparent)
;compound_statementを表す構造体.


(struct func_st (name para)#:transparent)
(struct func_nopara_st (name)#:transparent)
;関数呼び出しを表す構造体



;;注意!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;compound_sta_st内の各評価式はconsで接続される.
;statementはcompound_sta_stのconsで繋がれる.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
