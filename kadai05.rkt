#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;データ型定義;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-empty-tokens tokens-without-value
  (+ * & - / =
     l_small_paren r_small_paren ;()
     l_mid_paren r_mid_paren ;[]
     l_big_paren r_big_paren ;{}
     int void 
     if while for else
     or and equal not;||、&&、==、!=
     less and_less;<、<=
     more and_more;>、>=
     return
     semicolon comma 
     EOF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-tokens tokens-with-value
  (NUM VAR))

(define-lex-trans uinteger
  (syntax-rules () ((_ d) (:+ d))))

(define-lex-abbrevs
  (digit   (char-range "0" "9"))
  (number  (uinteger digit))
  (identifier-char (:or (char-range "a" "z")
                        (char-range "A" "Z")
                        "_"))
  (identifier (:: identifier-char
                  (:* (:or identifier-char
                           digit)))))

(define sub-program-lexer
  (lexer-src-pos
   ("(" (token-l_small_paren))
   (")" (token-r_small_paren))
   ("[" (token-l_mid_paren))
   ("]" (token-r_mid_paren))
   ("{" (token-l_big_paren))
   ("}" (token-r_big_paren))
   ("int" (token-int))
   ("void" (token-void))
   ("if" (token-if))
   ("while" (token-while))
   ("for" (token-for))
   ("else" (token-else))
   ("||" (token-or))
   ("&&" (token-and))
   ("==" (token-equal))
   ("!=" (token-not))
   ("<" (token-less))
   ("<=" (token-and_less))
   (">" (token-more))
   (">=" (token-and_more))
   ("return" (token-return))
   (";" (token-semicolon))
   ("," (token-comma))
   ("+" (token-+))
   ("*" (token-*))
   ("&" (token-&))
   ("-" (token--))
   ("/" (token-/))
   ("=" (token-=))
   (number (token-NUM (string->number lexeme)))
   (identifier (token-VAR (string->symbol lexeme)))
   (whitespace (return-without-pos (sub-program-lexer input-port)))
   ((eof) (token-EOF))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define program-parser
  (parser
   (start program);開始記号に当たる非終端記号
   (end EOF);入力の終端に達した時に字句解析器が返すトークン
   (src-pos);位置情報を含むオブジェクトを返す
   (debug "siple-parser.tbl")
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
             (error "parse error:" tok-name tok-value)))
   (tokens tokens-with-value tokens-without-value)
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;BNFの指定;;;;;;;;;;;;;;;;;;;;;;;;;;
   (grammar
    (program ((external_declaration) $1)
             ((program external_declaration) (cons $1 $2)))
    (external_declaration ((declaration) $1)
                          ((function_prototype) $1)
                          ((function_definition) $1))   
    (declaration ((type_specifier declarator_list semicolon) (declaration_st  $1 $2)))
    (declarator_list ((declarator) $1)
                      ((declarator_list comma declarator)(cons $1 $3)))
    (declarator ((direct_declarator) (declarator_st $1))
                ((* direct_declarator) (declarator_ast_st $2)))
    
    (direct_declarator ((VAR) (id_st $1 $1-start-pos));構造体id_stを作成.
                       ((VAR l_mid_paren NUM r_mid_paren)(array_st $1 $3 $1-start-pos)));構造体array_stを作成.
    
    (function_prototype ((type_specifier function_declarator semicolon)(func_proto_st $1 $2)));構造体func_proto_stを作成.
    (function_declarator ((VAR l_small_paren parameter_type_list r_small_paren)(func_declarator_st $1 $3));構造体func_declarator_stを作成.
                         ((VAR l_small_paren r_small_paren)(func_declarator_null_st $1));構造体func_declarator_null_stを作成.
                         ((* VAR l_small_paren parameter_type_list r_small_paren)(func_declarator_ast_st (id_ast_st $2 $2-start-pos) $4))
                         ((* VAR l_small_paren r_small_paren)(func_declarator_ast_null_st (id_ast_st $2 $2-start-pos))));構造体id_ast_stを作成.
    
    (function_definition ((type_specifier function_declarator compound_statement)(func_def_st $1 $2 $3)));構造体func_def_stを作成.
    (parameter_type_list ((parameter_declaration) $1)
                         ((parameter_type_list comma parameter_declaration)(cons $1 $3)))
    (parameter_declaration ((type_specifier parameter_declarator)(para_declaration_st $1 $2)))
    (parameter_declarator ((VAR) (id_st $1 $1-start-pos));構造体id_stを作成.
                          ((* VAR)(id_ast_st $2 $2-start-pos)));構造体id_ast_stを作成.
    (type_specifier ((int) (spec_st 'int $1-start-pos));構造体spec_stを作成.
                    ((void) (spec_st 'void $1-start-pos)));構造体spec_stを作成.
    (statement ((semicolon) (null_statement_st 'null))
               ((expression semicolon)(exp_with_semi_st $1));構造体exp-stを作成.
               ((compound_statement) $1)
               ((if l_small_paren expression r_small_paren statement)(if_st $3 $5 $1-start-pos));構造体if-stを作成.
               ((if l_small_paren expression r_small_paren statement else statement)(if_else_st $3 $5 $7 $1-start-pos $6-start-pos));構造体if_else_stを作成.
               ((while l_small_paren expression r_small_paren statement)(while_st $3 $5 $1-start-pos));構造体while-stを作成.
               ((for l_small_paren expression semicolon expression semicolon expression r_small_paren statement)(for_0_st $3 $5 $7 $9 $1-start-pos));構造体for_0_stを作成.
               ((for l_small_paren            semicolon expression semicolon expression r_small_paren statement)(for_1_st $4 $6 $8 $1-start-pos));構造体for_1_stを作成.
               ((for l_small_paren expression semicolon            semicolon expression r_small_paren statement)(for_2_st $3 $6 $8 $1-start-pos));構造体for_2_stを作成.
               ((for l_small_paren expression semicolon expression semicolon            r_small_paren statement)(for_3_st $3 $5 $8 $1-start-pos));構造体for_3_stを作成.
               ((for l_small_paren expression semicolon            semicolon            r_small_paren statement)(for_4_st $3 $7 $1-start-pos));構造体for_4_stを作成.
               ((for l_small_paren            semicolon expression semicolon            r_small_paren statement)(for_5_st $4 $7 $1-start-pos));構造体for_5_stを作成.
               ((for l_small_paren            semicolon            semicolon expression r_small_paren statement)(for_6_st $5 $7 $1-start-pos));構造体for_6_stを作成.
               ((for l_small_paren            semicolon            semicolon            r_small_paren statement)(for_7_st $6 $1-start-pos));構造体for_7_stを作成.
               ((return expression semicolon)(return_st $2 $1-start-pos));構造体return_stを作成.
               ((return semicolon)(return_null_st 'null $1-start-pos)));構造体return-null-stを作成.
    (compound_statement ((l_big_paren declaration_list statement_list r_big_paren)(compound_st $2 $3));構造体compound_stを作成.
                        ((l_big_paren declaration_list r_big_paren)(compound_dec_st $2));構造体compound_dec_stを作成.
                        ((l_big_paren statement_list r_big_paren)(compound_sta_st $2));構造体copound_sta_stを作成.
                        ((l_big_paren r_big_paren)(compound_null_st 'null)));構造体compound_null_stを作成.
    (declaration_list ((declaration) $1)
                      ((declaration_list declaration)(cons $1 $2)))
    (statement_list ((statement) $1)
                    ((statement_list statement)(cons $1 $2)))
    (expression ((assign_expr) $1)
                ((expression comma assign_expr)(cons $1 $3)))
    (assign_expr ((logical_OR_expr) $1)
                 ((logical_OR_expr = assign_expr)(assign_exp_st $1 $3 $2-start-pos)));構造体assign_exp_stを作成.
    (logical_OR_expr ((logical_AND_expr) $1)
                     ((logical_OR_expr or logical_AND_expr)(logic_exp_st 'or $1 $3 $2-start-pos)));構造体logic_exp_stを作成.
    (logical_AND_expr ((equality_expr) $1)
                      ((logical_AND_expr and equality_expr)(logic_exp_st 'and $1 $3 $2-start-pos)));構造体logic_exp_stを作成.
    (equality_expr ((relational_expr) $1)
                   ((equality_expr equal relational_expr)(rel_exp_st 'equal $1 $3 $2-start-pos));構造体rel_exp_stを作成.
                   ((equality_expr not relational_expr)(rel_exp_st 'not $1 $3 $2-start-pos)));構造体rel_exp_stを作成.
    
    (relational_expr ((add_expr) $1)
                     ((relational_expr less add_expr)(rel_exp_st 'less $1 $3 $2-start-pos));構造体rel_exp_stを作成.
                     ((relational_expr more add_expr)(rel_exp_st 'more $1 $3 $2-start-pos));構造体rel_exp_stを作成.
                     ((relational_expr and_less add_expr)(rel_exp_st 'and_less $1 $3 $2-start-pos));構造体rel_exp_stを作成.
                     ((relational_expr and_more add_expr)(rel_exp_st 'and_more $1 $3 $2-start-pos)));構造体rel_exp_stを作成.
    
    (add_expr ((mult_expr) $1)
              ((add_expr + mult_expr)(alge_exp_st 'add $1 $3 $2-start-pos));構造体alge_exp_stを作成.
              ((add_expr - mult_expr)(alge_exp_st 'sub $1 $3 $2-start-pos)));構造体alge_exp_stを作成.
    (mult_expr ((unary_expr) $1)
               ((mult_expr * unary_expr)(alge_exp_st 'mul $1 $3 $2-start-pos));構造体alge_exp_stを作成.
               ((mult_expr / unary_expr)(alge_exp_st 'div $1 $3 $2-start-pos)));構造体alge_exp_stを作成.
    (unary_expr ((postfix_expr) $1)
                ((- unary_expr)(unary_exp_st 'minus $2 $1-start-pos));構造体unary-exp-stを作成.
                ((& unary_expr)(unary_exp_st 'amp $2 $1-start-pos));構造体unary-exp-stを作成.
                ((* unary_expr)(unary_exp_st 'ast $2 $1-start-pos)));構造体unary-exp-stを作成.
    
    (postfix_expr ((primary_expr) $1)
                  ((postfix_expr l_mid_paren expression r_mid_paren)(array_var_st $1 $3  $1-start-pos));構造体array_var_stを作成.
                  ((VAR l_small_paren argument_expression_list r_small_paren)(func_st $1 $3));構造体func_stを作成.
                  ((VAR l_small_paren r_small_paren)(func_nopara_st $1)));構造体func_nopara_stを作成.
    (primary_expr ((VAR)(id_st $1 $1-start-pos));構造体id_stを作成.
                  ((NUM) (constant_st $1 $1-start-pos))
                  ((l_small_paren expression r_small_paren) (exp_in_paren_st $2)));構造体exp_stを作成.
    (argument_expression_list ((assign_expr) $1)
                              ((argument_expression_list comma assign_expr)(cons $1 $3)))
    )
   )
  )  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-string s)
  (let ((p (open-input-string s)))
  (program-parser (lambda () (sub-program-lexer p)))))

(define (parse-port p)
  (program-parser (lambda () (sub-program-lexer p))))


(define p (open-input-file "kadai01.sc"))
(port-count-lines! p)
(parse-port p)





