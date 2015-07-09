#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         (prefix-in stx: "mysyntax.rkt")
         (prefix-in k07u: "kadai07upgrade.rkt")
         )
(provide (all-defined-out))

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
    (program ((program_with_print) 
              (cons 
               (stx:func_proto_st
                (stx:spec_st 'void 'print-proto)
                (stx:func_declarator_st 'print 
                                        (stx:para_declaration_st 
                                         (stx:spec_st 'int 'print-proto)                         
                                         (stx:id_st 'v 'print-proto))
                                        'print-proto)) 
               $1)));プログラムの冒頭に組込み関数 printのプロトタイプ宣言をつける.
    
    (program_with_print ((external_declaration) $1)
                        ((program_with_print external_declaration) (cons $1 $2)))
    
    #;(stx:func_proto_st
       (stx:spec_st 'int 'print-proto)
       (stx:func_declarator_st 'print 
                               (stx:para_declaration_st (stx:spec_st 'int 'print-proto) 
                                                        (stx:id_st 'i 'print-proto))))
    
    (external_declaration ((declaration) $1)
                          ((function_prototype) $1)
                          ((function_definition) $1))   
    (declaration ((type_specifier declarator_list semicolon) 
                  (stx:declaration_st  $1 $2)))
    (declarator_list ((declarator) $1)
                     ((declarator_list comma declarator)(cons $1 $3)))
    (declarator ((direct_declarator) 
                 (stx:declarator_st $1))
                ((* direct_declarator)
                 (stx:declarator_ast_st $2)
                 ))
    
    (direct_declarator ((VAR) (stx:id_st $1 $1-start-pos))
                       ((VAR l_mid_paren NUM r_mid_paren)
                        (stx:array_st $1 $3 $1-start-pos)))
    
    (function_prototype ((type_specifier function_declarator semicolon)
                         (stx:func_proto_st $1 $2)))
    (function_declarator ((VAR l_small_paren parameter_type_list r_small_paren)
                          (stx:func_declarator_st $1 $3 $1-start-pos))
                         ((VAR l_small_paren r_small_paren)
                          (stx:func_declarator_null_st $1 $1-start-pos))
                         ((* VAR l_small_paren parameter_type_list r_small_paren)
                          (stx:func_declarator_ast_st $2 $4 $2-start-pos))
                         ((* VAR l_small_paren r_small_paren)
                          (stx:func_declarator_ast_null_st $2 $2-start-pos)))
    
    (function_definition ((type_specifier function_declarator compound_statement)
                          (stx:func_def_st $1 $2 $3)))
    (parameter_type_list ((parameter_declaration) $1)
                         ((parameter_type_list comma parameter_declaration)(cons $1 $3)))
    (parameter_declaration ((type_specifier parameter_declarator)
                            (stx:para_declaration_st $1 $2)))
    (parameter_declarator ((VAR) (stx:id_st $1 $1-start-pos))
                          ((* VAR)(stx:id_ast_st $2 $2-start-pos)))
    (type_specifier ((int) (stx:spec_st 'int $1-start-pos))
                    ((void) (stx:spec_st 'void $1-start-pos)))
    (statement ((semicolon) (stx:null_statement_st 'null))
               ((expression semicolon)$1)
               ((compound_statement) $1)
               ((if l_small_paren expression r_small_paren statement)
                ;シンタックスシュガー
                (stx:if_else_st $3 
                                $5 
                                (stx:null_statement_st 'null) 
                                $1-start-pos 'syntax-sygar))
               ((if l_small_paren expression r_small_paren statement else statement)
                (stx:if_else_st $3 $5 $7 $1-start-pos $6-start-pos))
               ((while l_small_paren expression r_small_paren statement)
                (stx:while_st $3 $5 $1-start-pos))        
               ((for l_small_paren expression 
                  semicolon expression 
                  semicolon expression 
                  r_small_paren statement)
                ;シンタックスシュガー   
                (stx:compound_sta_st 
                 (cons 
                  $3
                  (stx:while_st $5 
                                (stx:compound_sta_st (cons $9 $7)) 
                                'syntax-sugar))))     
               ((for l_small_paren            
                  semicolon expression 
                  semicolon expression 
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:while_st $4 
                              (stx:compound_sta_st (cons $8 $6))
                              'syntax-sugar))
               ((for l_small_paren expression 
                  semicolon            
                  semicolon expression 
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:compound_sta_st
                 (cons
                  $3
                  (stx:while_st (stx:null_statement_st 'null) 
                                (stx:compound_sta_st (cons $8 $6))
                                'syntax-sugar))))
               ((for l_small_paren expression 
                  semicolon expression 
                  semicolon            
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:compound_sta_st
                 (cons
                  $3
                  (stx:while_st $5
                                $8
                                'syntax-sugar))))
               ((for l_small_paren expression 
                  semicolon            
                  semicolon            
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:compound_sta_st
                 (cons 
                  $3
                  (stx:while_st (stx:null_statement_st 'null)
                                $7
                                'syntax-sugar))))
               ((for l_small_paren            
                  semicolon expression 
                  semicolon            
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:while_st $4
                              $7
                              'syntax-sugat))
               ((for l_small_paren            
                  semicolon            
                  semicolon expression 
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:while_st (stx:null_statement_st 'null)
                              (stx:compound_sta_st (cons $7 $5))))
               ((for l_small_paren            
                  semicolon            
                  semicolon            
                  r_small_paren statement)
                ;シンタックスシュガー
                (stx:while_st (stx:null_statement_st 'null)
                              $6
                              'syntax-sugar))
               
               ((return expression semicolon)(stx:return_st $2 $1-start-pos))
               ((return semicolon)(stx:return_st 'noreturn $1-start-pos)))
    (compound_statement ((l_big_paren declaration_list statement_list r_big_paren)
                         (stx:compound_st $2 $3))
                        ((l_big_paren declaration_list r_big_paren)
                         (stx:compound_dec_st $2))
                        ((l_big_paren statement_list r_big_paren)
                         (stx:compound_sta_st $2))
                        ((l_big_paren r_big_paren)
                         (stx:compound_null_st 'null)))
    (declaration_list ((declaration) $1)
                      ((declaration_list declaration)(cons $1 $2)))
    (statement_list ((statement) $1)
                    ((statement_list statement)(cons $1 $2)))
    (expression ((assign_expr) $1)
                ((expression comma assign_expr)(cons $1 $3)))
    (assign_expr ((logical_OR_expr) $1)
                 ((logical_OR_expr = assign_expr)
                  (stx:assign_exp_st $1 $3 $2-start-pos)))
    (logical_OR_expr ((logical_AND_expr) $1)
                     ((logical_OR_expr or logical_AND_expr)
                      (stx:logic_exp_st 'or $1 $3 $2-start-pos)))
    (logical_AND_expr ((equality_expr) $1)
                      ((logical_AND_expr and equality_expr)
                       (stx:logic_exp_st 'and $1 $3 $2-start-pos)))
    (equality_expr ((relational_expr) $1)
                   ((equality_expr equal relational_expr)
                    (stx:rel_exp_st 'equal $1 $3 $2-start-pos))
                   ((equality_expr not relational_expr)
                    (stx:rel_exp_st 'not $1 $3 $2-start-pos)))
    
    (relational_expr ((add_expr) $1)
                     ((relational_expr less add_expr)
                      (stx:rel_exp_st 'less $1 $3 $2-start-pos))
                     ((relational_expr more add_expr)
                      (stx:rel_exp_st 'more $1 $3 $2-start-pos))
                     ((relational_expr and_less add_expr)
                      (stx:rel_exp_st 'and_less $1 $3 $2-start-pos))
                     ((relational_expr and_more add_expr)
                      (stx:rel_exp_st 'adn_more $1 $3 $2-start-pos)))
    
    (add_expr ((mult_expr) $1)
              ((add_expr + mult_expr)
               (stx:alge_exp_st 'add $1 $3 $2-start-pos))
              ((add_expr - mult_expr)
               (stx:alge_exp_st 'sub $1 $3 $2-start-pos)))
    (mult_expr ((unary_expr) $1)
               ((mult_expr * unary_expr)
                (stx:alge_exp_st 'mul $1 $3 $2-start-pos))
               ((mult_expr / unary_expr)
                (stx:alge_exp_st 'div $1 $3 $2-start-pos)))
    (unary_expr ((postfix_expr) $1)
                ((- unary_expr)
                 ;シンタックスシュガー
                 (stx:alge_exp_st 'sub 
                                  (stx:constant_st 0 'syntax-sugar) 
                                  $2 
                                  $1-start-pos))
                ((& unary_expr)
                 (if (stx:exp_in_paren_st? $2) 
                     (if (stx:unary_exp_st? (stx:exp_in_paren_st-exp $2)) 
                         (if (equal? 'ast 
                                     (stx:unary_exp_st-mark (stx:exp_in_paren_st-exp $2)))
                             ;間接参照式のシンタックスシュガー
                             (stx:unary_exp_st-op (stx:exp_in_paren_st-exp $2))
                             (stx:unary_exp_st 'amp $2 $1-start-pos))
                         (stx:unary_exp_st 'amp $2 $1-start-pos))
                     (if (stx:unary_exp_st? $2)
                         (if (equal? 'ast (stx:unary_exp_st-mark $2)) 
                             (stx:unary_exp_st-op $2) 
                             (stx:unary_exp_st 'amp $2 $1-start-pos))
                         (stx:unary_exp_st 'amp $2 $1-start-pos))))
                ((* unary_expr)
                 (stx:unary_exp_st 'ast $2 $1-start-pos)))
    
    (postfix_expr ((primary_expr) $1)
                  ((postfix_expr l_mid_paren expression r_mid_paren)
                   ;(stx:array_var_st $1 $3  $1-start-pos)
                   ;配列参照式のシンタックスシュガー
                   (stx:unary_exp_st 'ast 
                                     (stx:exp_in_paren_st 
                                      (stx:alge_exp_st 'add $1 $3 'syntax-sugar))
                                     'syntax-sugar))
                  ((VAR l_small_paren argument_expression_list r_small_paren)
                   (stx:func_st $1 $3))
                  ((VAR l_small_paren r_small_paren)
                   (stx:func_st $1 'nopara)))
    (primary_expr ((VAR)(stx:id_st $1 $1-start-pos))
                  ((NUM) (stx:constant_st $1 $1-start-pos))
                  ((l_small_paren expression r_small_paren) 
                   (stx:exp_in_paren_st $2)))
    (argument_expression_list ((assign_expr) $1)
                              ((argument_expression_list comma assign_expr)(cons $1 $3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-string s)
  (let ((p (open-input-string s)))
    (program-parser (lambda () (sub-program-lexer p)))))

(define (parse-port p)
  (program-parser (lambda () (sub-program-lexer p))))
#;(begin
  (define p9999 (open-input-file "test01.c"))
  (port-count-lines! p9999)
  (parse-port p9999))




