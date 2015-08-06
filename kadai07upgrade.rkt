#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         (prefix-in stx: "mysyntax.rkt")
         (prefix-in k05u: "kadai05upgrade.rkt"))
(provide (all-defined-out))


;syn-to-codeは構文木として構造体を文字列に変換する.

;dec-list-to-codeは構造体(declaration_st-declarator-list x)を文字列に変換する際の補助関数.
;para-list-to-codeは構造体(func-declarator_st-para-list x)を文字列に変換する際のの補助関数.
;arg-list-to-codeは構造体(func_st-para x)を文字列に変換する際の補助関数.
;上記の関数はカンマの数などがわかっていないときの処理に使用する.

(define (syn-to-code x)
  (cond ((struct? x) 
         (cond 
           ((stx:declaration_st? x) (string-append (syn-to-code (stx:declaration_st-type-spec x))
                                                   " "
                                                   (dec-list-to-code (stx:declaration_st-declarator-list x))  
                                                   ";"))
           
           ((stx:func_declarator_st? x) (string-append (symbol->string (stx:func_declarator_st-name x))
                                                       "("
                                                       (para-list-to-code (stx:func_declarator_st-para-list x));;;;!!!!!!!!!!
                                                       ")"))                                                                                                                   
           ;(struct func_declarator_st (name para-list)#:transparent)
           
           ((stx:func_declarator_null_st? x) (string-append (symbol->string (stx:func_declarator_null_st-name x))
                                                            "()"))                        
           ;(struct func_declarator_null_st (name)#:transparent)
           
           ((stx:func_declarator_ast_st? x) (string-append "*"
                                                           (syn-to-code (stx:func_declarator_ast_st-name x))
                                                           (syn-to-code (stx:func_declarator_ast_st-para-list x))))
           ;(struct func_declarator_ast_st (name para-list)#:transparent)
           
           ((stx:func_declarator_ast_null_st? x) (string-append "*" 
                                                                (syn-to-code (stx:func_declarator_ast_null_st-name x))))
           ;(struct func_declarator_ast_null_st (name)#:transparent)
           
           ((stx:func_proto_st? x) (string-append (syn-to-code (stx:func_proto_st-type-spec x))
                                                  " "
                                                  (syn-to-code (stx:func_proto_st-func-declarator-st x))
                                                  ";"))
           ;(struct func_proto_st (type-spec func-declarator-st)#:transparent)
           
           ((stx:func_def_st? x) (string-append (syn-to-code (stx:func_def_st-type-spec x))
                                                " "
                                                (syn-to-code (stx:func_def_st-func-declarator-st x))
                                                (syn-to-code (stx:func_def_st-compound-state-list x))))
           ;(struct func_def_st (type-spec func-declarator-st compound-state-list)#:transparent)
           
           ((stx:declarator_st? x) (syn-to-code (stx:declarator_st-var x)))
           ;(struct declarator_st (var)#:transparent)
           
           ((stx:declarator_ast_st? x) (string-append "*" (syn-to-code (stx:declarator_ast_st-var x))))
           ;(struct declarator_ast_st (var)#:transparent)
           
           ((stx:para_declaration_st? x) (string-append (syn-to-code (stx:para_declaration_st-type-spec x))
                                                        " "
                                                        (syn-to-code (stx:para_declaration_st-para x))))
           ;(struct para_declaration_st (type-spec para)#:transparent)
           
           ((stx:exp_st? x) (syn-to-code (stx:exp_st-exp x)))
           ;(struct exp_st (exp)#:transparent)
           
           ((stx:assign_exp_st? x) (string-append (syn-to-code (stx:assign_exp_st-dest x))
                                                  " = "
                                                  (syn-to-code (stx:assign_exp_st-src x))))
           ;(struct assign_exp_st (dest src pos)#:transparent)
           
           ((stx:logic_exp_st? x) (string-append (syn-to-code (stx:logic_exp_st-op1 x))
                                                 (cond ((eq? (stx:logic_exp_st-log-ope x) 'or) " || ")
                                                       ((eq? (stx:logic_exp_st-log-ope x) 'and) " && "))
                                                 (syn-to-code (stx:logic_exp_st-op2 x))))
           ;(struct logic_exp_st (log-ope op1 op2 pos)#:transparent)
           
           ((stx:rel_exp_st? x) (string-append (syn-to-code (stx:rel_exp_st-op1 x))
                                               (cond ((eq? (stx:rel_exp_st-rel-ope x) 'equal) " == ")
                                                     ((eq? (stx:rel_exp_st-rel-ope x) 'not) " != ")
                                                     ((eq? (stx:rel_exp_st-rel-ope x) 'less) " < ")
                                                     ((eq? (stx:rel_exp_st-rel-ope x) 'and_less) " <= ")
                                                     ((eq? (stx:rel_exp_st-rel-ope x) 'more) " > ")
                                                     ((eq? (stx:rel_exp_st-rel-ope x) 'and_more) " >= "))                                                           
                                               (syn-to-code (stx:rel_exp_st-op2 x))))
           ;(struct rel_exp_st (rel-ope op1 op2 pos)#:transparent)
           
           ((stx:alge_exp_st? x) (string-append (syn-to-code (stx:alge_exp_st-op1 x))
                                                (cond ((eq? (stx:alge_exp_st-alge-ope x) 'add) " + ")
                                                      ((eq? (stx:alge_exp_st-alge-ope x) 'sub) " - ")
                                                      ((eq? (stx:alge_exp_st-alge-ope x) 'mul) " * ")
                                                      ((eq? (stx:alge_exp_st-alge-ope x) 'div) " / "))
                                                (syn-to-code (stx:alge_exp_st-op2 x))))
           ;(struct alge_exp_st (alge-ope op1 op2 pos)#:transparent)
           
           ((stx:id_st? x) (symbol->string (stx:id_st-name x)))
           ;(struct id_st (name pos)#:transparent)                           
           ((stx:id_ast_st? x) (string-append "*" (symbol->string (stx:id_ast_st-name x))))
           ;(struct id_ast_st (name pos)#:transparent)
           
           ((stx:array_st? x) (string-append (symbol->string (stx:array_st-name x)) "[" (number->string (stx:array_st-num x)) "]"))
           ;(struct array_st (name num pos)#:transparent);宣言時.posはnameの位置.
           ((stx:array_var_st? x) (string-append (syn-to-code (stx:array_var_st-name x)) "[" (syn-to-code (stx:array_var_st-num x)) "]"))
           ;(struct array_var_st (name num pos)#:transparent);式の中で用いる.posはnameの位置.
           
           ((stx:spec_st? x) (symbol->string (stx:spec_st-type x)))
           ;(struct spec_st (type pos)#:transparent)
           
           ((stx:unary_exp_st? x) (string-append (cond ((eq? (stx:unary_exp_st-mark x) 'minus) "-")
                                                       ((eq? (stx:unary_exp_st-mark x) 'ast) "*")
                                                       ((eq? (stx:unary_exp_st-mark x) 'amp) "&"))
                                                 (stx:unary_exp_st-op x)))
           ;(struct unary_exp_st (mark op pos)#:transparent)
           
           ((stx:constant_st? x) (number->string (stx:constant_st-cons x)))
           ;(struct constant_st (cons pos)#:transparent)
           
           ((stx:null_statement_st? x) ";")
           ;(struct null_statement_st (null))
           
           ((stx:exp_with_semi_st? x) (string-append (syn-to-code (stx:exp_with_semi_st-exp x)) ";"))
           ;(struct exp_with_semi_st (exp)#:transparent)
           
           ((stx:exp_in_paren_st? x) (string-append "(" (syn-to-code (stx:exp_in_paren_st-exp x)) ")"))
           ;(struct exp_in_paren_st (exp)#:transparent)
           
           ((stx:if_st? x) (string-append "if ("
                                          (syn-to-code (stx:if_st-cond-exp x))
                                          ")"
                                          (syn-to-code (stx:if_st-state x))))
           ;(struct if_st (cond-exp state pos)#:transparent);else無し.posはifの位置.
           ((stx:if_else_st? x) (string-append "if("
                                               (syn-to-code (stx:if_else_st-cond-exp x))
                                               ")"
                                               (syn-to-code (stx:if_else_st-state x))
                                               "else"
                                               (syn-to-code (stx:if_else_st-else-state x))))
           ;(struct if_else_st (cond-exp state else-state if-pos else-pos)#:transparent);elseあり
           
           ((stx:while_st? x) (string-append "while("
                                             (syn-to-code (stx:while_st-cond-exp x))
                                             ")"
                                             (syn-to-code (stx:while_st-statement x))))
           ;(struct while_st (cond-exp statement pos)#:transparent);posはwhileの位置.
           
           ((stx:for_0_st? x) (string-append "for("
                                             (syn-to-code (stx:for_0_st-cond-exp1 x)) ";"
                                             (syn-to-code (stx:for_0_st-cond-exp2 x)) ";"
                                             (syn-to-code (stx:for_0_st-cond-exp3 x)) 
                                             ")" (syn-to-code (stx:for_0_st-statement x))))
           ;(struct for_0_st (cond-exp1 cond-exp2 cond-exp3 statement pos)#:transparent)
           
           ((stx:for_1_st? x) (string-append "for(;"
                                             (syn-to-code (stx:for_1_st-cond-exp1 x)) ";"
                                             (syn-to-code (stx:for_1_st-cond-exp2 x)) ")"
                                             (syn-to-code (stx:for_1_st-statement x))))
           ;(struct for_1_st (cond-exp1 cond-exp2 statement pos)#:transparent)
           
           ((stx:for_2_st? x) (string-append "for("
                                             (syn-to-code (stx:for_2_st-cond-exp1 x)) ";;"
                                             (syn-to-code (stx:for_2_st-cond-exp2 x)) ")"
                                             (syn-to-code (stx:for_2_st-statement x))))
           ;(struct for_2_st (cond-exp1 cond-exp2 statement pos)#:transparent)
           
           ((stx:for_3_st? x) (string-append "for("
                                             (syn-to-code (stx:for_3_st-cond-exp1 x)) ";"
                                             (syn-to-code (stx:for_3_st-cond-exp2 x)) ";)"
                                             (syn-to-code (stx:for_3_st-statement x))))
           ;(struct for_3_st (cond-exp1 cond-exp2 statement pos)#:transparent)
           
           ((stx:for_4_st? x) (string-append "for("
                                             (syn-to-code (stx:for_4_st-cond-exp1 x)) ";;)"
                                             (syn-to-code (stx:for_4_st-statement x))))
           ;(struct for_4_st (cond-exp1 statement pos)#:transparent)
           
           ((stx:for_5_st? x) (string-append "for(;"
                                             (syn-to-code (stx:for_5_st-cond-exp1 x)) ";)"
                                             (syn-to-code (stx:for_5_st-statement x))))
           ;(struct for_5_st (cond-exp1 statement pos)#:transparent)
           
           ((stx:for_6_st? x) (string-append "for(;;"
                                             (syn-to-code (stx:for_6_st-cond-exp1 x)) ")"
                                             (syn-to-code (stx:for_6_st-statement x))))
           ;(struct for_6_st (cond-exp1 statement pos)#:transparent)
           
           ((stx:for_7_st? x) (string-append "for(;;)"
                                             (syn-to-code (stx:for_7_st-statement x))))
           ;(struct for_7_st (statement pos)#:transparent)
           
           ((stx:return_st? x) (string-append "return"
                                              (syn-to-code (stx:return_st-exp x))
                                              ";"))
           ;(struct return_st (exp pos)#:transparent);posはreturnの位置.
           
           ((stx:return_null_st? x) (string-append "return"
                                                   (syn-to-code (stx:return_null_st-exp x))
                                                   ";"))
           ;(struct return_null_st (exp pos)#:transparent)
           
           ((stx:compound_st? x) (string-append "{"
                                                (syn-to-code (stx:compound_st-declaration-list x))
                                                (syn-to-code (stx:compound_st-statement-list x))
                                                "}"))                                                                         
           ;(struct compound_st (declaration-list statement-list)#:transparent)
           
           ((stx:compound_dec_st? x) (string-append "{" (syn-to-code (stx:compound_dec_st-declaration-list x)) "}"))
           ;(struct compound_dec_st (declaration-list)#:transparent)
           
           ((stx:compound_sta_st? x) (string-append 
                                      "{" 
                                      (syn-to-code (stx:compound_sta_st-statement-list x)) ;;;;;;
                                      "}"))                     
           ;(struct compound_sta_st (statement-list)#:transparent)
           
           ((stx:compound_null_st? x) "{}")                        
           ;(struct compound_null_st (null)#:transparent)
           
           ((stx:func_st? x) (string-append (symbol->string (stx:func_st-name x))
                                            "("
                                            (arg-list-to-code (stx:func_st-para x))
                                            ")"))                                          
           ;(struct func_st (name para)#:transparent)
           
           ((stx:func_nopara_st? x) (string-append (symbol->string (stx:func_nopara_st-name x))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;テストコード
;(define p (open-input-file "test.c"))
;(port-count-lines! p)
;(syn-to-code (k05u:parse-port p))