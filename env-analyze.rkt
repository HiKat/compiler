#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))

;構造体の型の参照は(stx:spec_st-type s)

(define env env:initial-env)
;大域変数にするとcompoundstatementを出たときにenvを捨てられない.

(struct obj (name lev kind type)#:transparent)
;(struct obj (name)#:transparent)

(define (analy-declaration_st env lv st) "under const")

(define (analy-func_declarator_st env lv st)
  (let* ((name (stx:func_declarator_st-name st)))
    (env:extend-env (obj name env lv st))))

(define (analy-func_declarator_null_st env lv st)
  (let* ((name (stx:func_declarator_null_st-name st)))
    (env:extend-env (obj name env lv st))))

(define (analy-func_declarator_ast_st env lv st)
  (let* ((name (stx:func_declarator_ast_st-name st)))
    (env:extend-env (obj name env lv st))))

(define (analy-func_declarator_ast_null_st env lv st)
  (let* ((name (stx:func_declarator_ast_null_st-name st)))
    (env:extend-env (obj name env lv st))))

(define (anly-func_proto_st env lv st)
  (let* ((name (stx:func_declarator_st-name 
                (stx:func_proto_st-func-declarator-st st))))
    (env:extend-env (obj name) env)))

(define (analy-func_def_st env lv st)
  (let* ((name (stx:func_declarator_st-name 
                (stx:func_def_st-func-declarator-st st))))
    (env:extend-env (obj name) env)))

(define (analy-declarator_st env lv st)
  (let* ((name (stx:id_st-name (stx:declarator_st-var st))))
    (env:extend-env (obj name) env)))

(define (analy-declarator_ast_st env lv st)
  (let* ((name (stx:id_st-name (stx:declarator_st-var st))))
    (env:extend-env (obj name) env)))

(define (analy-id_st st)
  (let* ((name (stx:id_st-name env lv st)))
    (env:extend-env (obj name) env)))

(define (analy-id_ast_st st)
  (let* ((name (stx:id_ast_st-name env lv st)))
    (env:extend-env (obj name) env)))

(define (analy-array_st st)
  (let* ((name (stx:array_st-name env lv st)))
    (env:extend-env (obj name) env)))

;構文木を引数に取りその意味解析を行う関数
(define (analyze-tree t)
  (cond ((#t)("always"))
        ((cons? t) ("analyze cons"))
        ((struct? t) ("analyze st"))
        (else #t)))

(define (analyze-st st)
  (cond ((stx:func_declarator_st? st) #t)
        ((stx:func_declarator_null_st? st) #t)
        ((stx:func_declarator_ast_st? st) #t)
        ((stx:func_declarator_ast_null_st? st) #t)
        ((stx:func_proto_st? st) #t)
        ((stx:func_def_st? st) #t)
        ((stx:funcdeclarator_ast_st? st) #t)
        ((stx:para_declaration_st? st) #t)
        ((stx:exp_st? st) #t)
        ((stx:assign_exp_st? st) #t)
        ((stx:logic_exp_st? st) #t)
        ((stx:rel_exp_st? st) #t)
        ((stx:alge_exp_st? st) #t)
        ((stx:id_st? st) #t)
        ((stx:array_st? st) #t)
        ((stx:array_var_st? st) #t)
        ((stx:spec_st? st) #t)
        ((stx:unary_exp_st? st) #t)
        ((stx:constant_st? st) #t)
        ((stx:null_statement_st? st) #t)
        ((stx:exp_with_semi_st? st) #t)
        ((stx:exp_in_paren_st? st) #t)
        ((stx:if_st? st) #t)
        ((stx:if_else_st? st) #t)
        ((stx:while_st? st) #t)
        ((stx:for_0_st? st) #t)
        ((stx:for_1_st? st) #t)
        ((stx:for_2_st? st) #t)
        ((stx:for_3_st? st) #t)
        ((stx:for_4_st? st) #t)
        ((stx:for_5_st? st) #t)
        ((stx:for_6_st? st) #t)
        ((stx:for_7_st? st) #t)
        ((stx:return_st? st) #t)
        ((stx:return_null_st? st) #t)
        ((stx:compound_st? st) #t)
        ((stx:compound_dec_st? st) #t)
        ((stx:compound_sta_st? st) #t)
        ((stx:compound_null_st? st) #t)
        ((stx:func_st? st) #t)
        ((stx:func_nopara_st? st) #t)))
        
        
        
        
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
;テスト
env
(define p (open-input-file "test.c"))
(port-count-lines! p)
(k08:parse-port p)