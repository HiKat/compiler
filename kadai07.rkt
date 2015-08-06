#lang racket
(require parser-tools/lex 
         (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)
(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;データ型定義;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;programはexternal-declarationのリスト.
;external_declarationはdeclaartion、func-proto、func-defからなるリスト
(struct declaration_st (type-spec declarator-list)#:transparent)
;例) (int (id1 id2 *id3))

(struct func_declarator_st (name para-list)#:transparent)
(struct func_declarator_null_st (name)#:transparent)
(struct func_declarator_ast_st (name para-list)#:transparent)
(struct func_declarator_ast_null_st (name)#:transparent)
;例)nameは関数名、para-listは引数のリスト.

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;syn-to-codeは構文木として構造体を文字列に変換する.

;dec-list-to-codeは構造体(declaration_st-declarator-list x)を文字列に変換する際の補助関数.
;para-list-to-codeは構造体(func-declarator_st-para-list x)を文字列に変換する際のの補助関数.
;arg-list-to-codeは構造体(func_st-para x)を文字列に変換する際の補助関数.
;上記の関数はカンマの数などがわかっていないときの処理に使用する.

(define (syn-to-code x)
  (cond ((struct? x) (cond ((declaration_st? x) (string-append (syn-to-code (declaration_st-type-spec x))
                                                               " "
                                                               (dec-list-to-code (declaration_st-declarator-list x))  
                                                               ";"))
                           
                           ((func_declarator_st? x) (string-append (symbol->string (func_declarator_st-name x))
                                                                   "("
                                                                   (para-list-to-code (func_declarator_st-para-list x));;;;!!!!!!!!!!
                                                                   ")"))                                                                                                                   
                           ;(struct func_declarator_st (name para-list)#:transparent)
                           
                           ((func_declarator_null_st? x) (string-append (symbol->string (func_declarator_null_st-name x))
                                                                        "()"))                        
                           ;(struct func_declarator_null_st (name)#:transparent)
                           
                           ((func_declarator_ast_st? x) (string-append "*"
                                                                       (syn-to-code (func_declarator_ast_st-name x))
                                                                       (syn-to-code (func_declarator_ast_st-para-list x))))
                           ;(struct func_declarator_ast_st (name para-list)#:transparent)
                           
                           ((func_declarator_ast_null_st? x) (string-append "*" 
                                                                            (syn-to-code (func_declarator_ast_null_st-name x))))
                           ;(struct func_declarator_ast_null_st (name)#:transparent)
                           
                           ((func_proto_st? x) (string-append (syn-to-code (func_proto_st-type-spec x))
                                                              " "
                                                              (syn-to-code (func_proto_st-func-declarator-st x))
                                                              ";"))
                           ;(struct func_proto_st (type-spec func-declarator-st)#:transparent)
                           
                           ((func_def_st? x) (string-append (syn-to-code (func_def_st-type-spec x))
                                                            " "
                                                            (syn-to-code (func_def_st-func-declarator-st x))
                                                            (syn-to-code (func_def_st-compound-state-list x))))
                           ;(struct func_def_st (type-spec func-declarator-st compound-state-list)#:transparent)
                           
                           ((declarator_st? x) (syn-to-code (declarator_st-var x)))
                           ;(struct declarator_st (var)#:transparent)
                           
                           ((declarator_ast_st? x) (string-append "*" (syn-to-code (declarator_ast_st-var x))))
                           ;(struct declarator_ast_st (var)#:transparent)
                           
                           ((para_declaration_st? x) (string-append (syn-to-code (para_declaration_st-type-spec x))
                                                                    " "
                                                                    (syn-to-code (para_declaration_st-para x))))
                           ;(struct para_declaration_st (type-spec para)#:transparent)
                           
                           ((exp_st? x) (syn-to-code (exp_st-exp x)))
                           ;(struct exp_st (exp)#:transparent)
                           
                           ((assign_exp_st? x) (string-append (syn-to-code (assign_exp_st-dest x))
                                                              " = "
                                                              (syn-to-code (assign_exp_st-src x))))
                           ;(struct assign_exp_st (dest src pos)#:transparent)
                           
                           ((logic_exp_st? x) (string-append (syn-to-code (logic_exp_st-op1 x))
                                                             (cond ((eq? (logic_exp_st-log-ope x) 'or) " || ")
                                                                   ((eq? (logic_exp_st-log-ope x) 'and) " && "))
                                                             (syn-to-code (logic_exp_st-op2 x))))
                           ;(struct logic_exp_st (log-ope op1 op2 pos)#:transparent)
                           
                           ((rel_exp_st? x) (string-append (syn-to-code (rel_exp_st-op1 x))
                                                           (cond ((eq? (rel_exp_st-rel-ope x) 'equal) " == ")
                                                                 ((eq? (rel_exp_st-rel-ope x) 'not) " != ")
                                                                 ((eq? (rel_exp_st-rel-ope x) 'less) " < ")
                                                                 ((eq? (rel_exp_st-rel-ope x) 'and_less) " <= ")
                                                                 ((eq? (rel_exp_st-rel-ope x) 'more) " > ")
                                                                 ((eq? (rel_exp_st-rel-ope x) 'and_more) " >= "))                                                           
                                                           (syn-to-code (rel_exp_st-op2 x))))
                           ;(struct rel_exp_st (rel-ope op1 op2 pos)#:transparent)
                           
                           ((alge_exp_st? x) (string-append (syn-to-code (alge_exp_st-op1 x))
                                                            (cond ((eq? (alge_exp_st-alge-ope x) 'add) " + ")
                                                                  ((eq? (alge_exp_st-alge-ope x) 'sub) " - ")
                                                                  ((eq? (alge_exp_st-alge-ope x) 'mul) " * ")
                                                                  ((eq? (alge_exp_st-alge-ope x) 'div) " / "))
                                                            (syn-to-code (alge_exp_st-op2 x))))
                           ;(struct alge_exp_st (alge-ope op1 op2 pos)#:transparent)
                           
                           ((id_st? x) (symbol->string (id_st-name x)))
                           ;(struct id_st (name pos)#:transparent)                           
                           ((id_ast_st? x) (string-append "*" (symbol->string (id_ast_st-name x))))
                           ;(struct id_ast_st (name pos)#:transparent)
                           
                           ((array_st? x) (string-append (symbol->string (array_st-name x)) "[" (number->string (array_st-num x)) "]"))
                           ;(struct array_st (name num pos)#:transparent);宣言時.posはnameの位置.
                           ((array_var_st? x) (string-append (syn-to-code (array_var_st-name x)) "[" (syn-to-code (array_var_st-num x)) "]"))
                           ;(struct array_var_st (name num pos)#:transparent);式の中で用いる.posはnameの位置.
                           
                           ((spec_st? x) (symbol->string (spec_st-type x)))
                           ;(struct spec_st (type pos)#:transparent)
                           
                           ((unary_exp_st? x) (string-append (cond ((eq? (unary_exp_st-mark x) 'minus) "-")
                                                                   ((eq? (unary_exp_st-mark x) 'ast) "*")
                                                                   ((eq? (unary_exp_st-mark x) 'amp) "&"))
                                                             (unary_exp_st-op x)))
                           ;(struct unary_exp_st (mark op pos)#:transparent)
                           
                           ((constant_st? x) (number->string (constant_st-cons x)))
                           ;(struct constant_st (cons pos)#:transparent)
                           
                           ((null_statement_st? x) ";")
                           ;(struct null_statement_st (null))
                           
                           ((exp_with_semi_st? x) (string-append (syn-to-code (exp_with_semi_st-exp x)) ";"))
                           ;(struct exp_with_semi_st (exp)#:transparent)
                           
                           ((exp_in_paren_st? x) (string-append "(" (syn-to-code (exp_in_paren_st-exp x)) ")"))
                           ;(struct exp_in_paren_st (exp)#:transparent)
                           
                           ((if_st? x) (string-append "if ("
                                                      (syn-to-code (if_st-cond-exp x))
                                                      ")"
                                                      (syn-to-code (if_st-state x))))
                           ;(struct if_st (cond-exp state pos)#:transparent);else無し.posはifの位置.
                           ((if_else_st? x) (string-append "if("
                                                           (syn-to-code (if_else_st-cond-exp x))
                                                           ")"
                                                           (syn-to-code (if_else_st-state x))
                                                           "else"
                                                           (syn-to-code (if_else_st-else-state x))))
                           ;(struct if_else_st (cond-exp state else-state if-pos else-pos)#:transparent);elseあり
                           
                           ((while_st? x) (string-append "while("
                                                         (syn-to-code (while_st-cond-exp x))
                                                         ")"
                                                         (syn-to-code (while_st-statement x))))
                           ;(struct while_st (cond-exp statement pos)#:transparent);posはwhileの位置.
                           
                           ((for_0_st? x) (string-append "for("
                                                         (syn-to-code (for_0_st-cond-exp1 x)) ";"
                                                         (syn-to-code (for_0_st-cond-exp2 x)) ";"
                                                         (syn-to-code (for_0_st-cond-exp3 x)) 
                                                         ")" (syn-to-code (for_0_st-statement x))))
                           ;(struct for_0_st (cond-exp1 cond-exp2 cond-exp3 statement pos)#:transparent)
                           
                           ((for_1_st? x) (string-append "for(;"
                                                         (syn-to-code (for_1_st-cond-exp1 x)) ";"
                                                         (syn-to-code (for_1_st-cond-exp2 x)) ")"
                                                         (syn-to-code (for_1_st-statement x))))
                           ;(struct for_1_st (cond-exp1 cond-exp2 statement pos)#:transparent)
                           
                           ((for_2_st? x) (string-append "for("
                                                         (syn-to-code (for_2_st-cond-exp1 x)) ";;"
                                                         (syn-to-code (for_2_st-cond-exp2 x)) ")"
                                                         (syn-to-code (for_2_st-statement x))))
                           ;(struct for_2_st (cond-exp1 cond-exp2 statement pos)#:transparent)
                           
                           ((for_3_st? x) (string-append "for("
                                                         (syn-to-code (for_3_st-cond-exp1 x)) ";"
                                                         (syn-to-code (for_3_st-cond-exp2 x)) ";)"
                                                         (syn-to-code (for_3_st-statement x))))
                           ;(struct for_3_st (cond-exp1 cond-exp2 statement pos)#:transparent)
                           
                           ((for_4_st? x) (string-append "for("
                                                         (syn-to-code (for_4_st-cond-exp1 x)) ";;)"
                                                         (syn-to-code (for_4_st-statement x))))
                           ;(struct for_4_st (cond-exp1 statement pos)#:transparent)
                           
                           ((for_5_st? x) (string-append "for(;"
                                                         (syn-to-code (for_5_st-cond-exp1 x)) ";)"
                                                         (syn-to-code (for_5_st-statement x))))
                           ;(struct for_5_st (cond-exp1 statement pos)#:transparent)
                           
                           ((for_6_st? x) (string-append "for(;;"
                                                         (syn-to-code (for_6_st-cond-exp1 x)) ")"
                                                         (syn-to-code (for_6_st-statement x))))
                           ;(struct for_6_st (cond-exp1 statement pos)#:transparent)
                           
                           ((for_7_st? x) (string-append "for(;;)"
                                                         (syn-to-code (for_7_st-statement x))))
                           ;(struct for_7_st (statement pos)#:transparent)
                           
                           ((return_st? x) (string-append "return"
                                                          (syn-to-code (return_st-exp x))
                                                          ";"))
                           ;(struct return_st (exp pos)#:transparent);posはreturnの位置.
                           
                           ((return_null_st? x) (string-append "return"
                                                               (syn-to-code (return_null_st-exp x))
                                                               ";"))
                           ;(struct return_null_st (exp pos)#:transparent)
                           
                           ((compound_st? x) (string-append "{"
                                                            (syn-to-code (compound_st-declaration-list x))
                                                            (syn-to-code (compound_st-statement-list x))
                                                            "}"))                                                                         
                           ;(struct compound_st (declaration-list statement-list)#:transparent)
                           
                           ((compound_dec_st? x) (string-append "{" (syn-to-code (compound_dec_st-declaration-list x)) "}"))
                           ;(struct compound_dec_st (declaration-list)#:transparent)
                           
                           ((compound_sta_st? x) (string-append 
                                                  "{" 
                                                  (syn-to-code (compound_sta_st-statement-list x)) ;;;;;;
                                                  "}"))                     
                           ;(struct compound_sta_st (statement-list)#:transparent)
                           
                           ((compound_null_st? x) "{}")                        
                           ;(struct compound_null_st (null)#:transparent)
                           
                           ((func_st? x) (string-append (symbol->string (func_st-name x))
                                                        "("
                                                        (arg-list-to-code (func_st-para x))
                                                        ")"))                                          
                           ;(struct func_st (name para)#:transparent)
                           
                           ((func_nopara_st? x) (string-append (symbol->string (func_nopara_st-name x))
                                                               "("
                                                               ")"))  
                           ;(struct func_nopara_st (name)#:transparent)
                           
                           
                           
                           
                           ;(#t "error: unknown syntax")
                           ))
        
        
        (else (string-append (syn-to-code (car x)) 
                             " " 
                             (syn-to-code (cdr x))))))

(define (dec-list-to-code x) 
  (cond ((struct? x) (syn-to-code x))
        (else (string-append (dec-list-to-code (car x))
                             ", "
                             (syn-to-code (cdr x))))))

(define (para-list-to-code x)
  (cond ((struct? x) (syn-to-code x))
        (else (string-append (para-list-to-code (car x))
                             ", "
                             (syn-to-code (cdr x))))))

(define (arg-list-to-code x)
  (cond ((struct? x) (syn-to-code x))
        (else (string-append (arg-list-to-code (car x))
                             ", "
                             (syn-to-code (cdr x))))))




