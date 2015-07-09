#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require (prefix-in in: "intermed.rkt"))
(require "semantic-analy.rkt")
(provide (all-defined-out))

(define temp 0)
(define temp-name 'temp0)
;一時変数を作成する関数.
;引数無し
;返り値
;新しい一時変数名
(define (make-temp)
  (let* ((new-temp (+ 1 temp))
         (new-name (string->symbol (string-append "temp" (number->string temp)))))
    (set! temp new-temp)
    (set! temp-name new-name)
    (obj new-name 'temp 'temp 'temp 'temp)))

(define (ref-temp)
  (obj temp-name 'temp 'temp 'temp 'temp))

;代入する際に冗長な表現を削除する関数.
;引数はletstmt
;戻り値は適切な letstmt
(define (correct-let ls)
  (cond ;(letstmt var1 (letstmt var2 exp))を(letstmt var1 exp)に 
    ((in:letstmt? (in:letstmt-exp ls))
     (in:letstmt (in:letstmt-var ls) (in:letstmt-exp (in:letstmt-exp ls))))
    (else ls)))


;引数
;抽象構文木
;戻り値
;中間命令列
(define (gen-intermed st) (flatten (map syn-to-inter st)))

;引数
;抽象構文構造体
;戻り値
;中間命令構造体
;constant_stの扱いは??
(define (syn-to-inter st) 
  (cond
    ((stx:declaration_st? st) 
     (let* (;declarator-listはobjのlistもしくはobj単体 
            (decl-ls (stx:declaration_st-declarator-list st)))
       (cond ((obj? decl-ls) (in:vardecl (decl-ls)))
             ((list? decl-ls) (map (lambda (x) (in:vardecl x)) decl-ls))
             (error (format "\n check syn-to-code! ~a\n" st)))))
    ((stx:func_def_st? st) (let* ((fun-dec (stx:func_def_st-func-declarator-st st))
                                  (fun-obj (stx:func_declarator_st-name fun-dec))
                                  ;fun-para-listはパラメータのobjのlistもしくはその単体.
                                  (fun-para-list (stx:func_declarator_st-para-list fun-dec))
                                  (fun-body (stx:func_def_st-compound-state-list st)))
                             (in:fundef fun-obj 
                                        (cond ((equal? 'nopara fun-para-list) '())
                                              (else (map (lambda (x) (in:vardecl x)) fun-para-list)))
                                        (syn-to-inter fun-body)))) 
    ((stx:func_proto_st? st) '())
    ((stx:assign_exp_st? st) 
     (let* ((dest (stx:assign_exp_st-dest st))
            (src (stx:assign_exp_st-src st)))
       ;ここは(syn-to-inter dest)にする必要はないはず.
       (correct-let (in:letstmt dest (syn-to-inter src)))))
    ((stx:logic_exp_st? st) 
     (let* ((op (stx:logic_exp_st-log-ope st))
            (op1 (stx:logic_exp_st-op1 st))
            (op2 (stx:logic_exp_st-op2 st))
            (temp1 (make-temp))
            (temp2 (make-temp))
            (temp3 (make-temp)))
       (cond ((equal? 'or op)
              (list (correct-let (in:letstmt temp1 (syn-to-inter op1)))
                    (correct-let (in:letstmt temp2 (syn-to-inter op2)))
                    (in:ifstmt temp1 
                               (correct-let 
                                (in:letstmt temp3 (in:intexp 1))) 
                                (in:ifstmt temp2 
                                           (in:intexp 1) 
                                           (correct-let 
                                            (in:letstmt temp3 (in:intexp 0)))))))
             ((equal? 'and op)
              (list (correct-let 
                     (in:letstmt temp1 (syn-to-inter op1)))
                    (correct-let 
                     (in:letstmt temp2 (syn-to-inter op2)))
                    (in:ifstmt temp1 
                               (in:ifstmt temp2 
                                          (correct-let 
                                           (in:letstmt temp3 (in:intexp 1))) 
                                          (correct-let (in:letstmt temp3 (in:intexp 0)))) 
                               (correct-let 
                                (in:letstmt temp3 (in:intexp 0)))))))))                              
    ((stx:rel_exp_st? st) 
     (let* ((op (stx:rel_exp_st-rel-ope st))
            (op (cond ((equal? 'not op) '!=)
                      ((equal? 'equal op) '==)
                      ((equal? 'less op) '<)
                      ((equal? 'and_less op) '=<)
                      ((equal? 'more op) '>)
                      ((equal? 'and_more op) '=<)))
            (op1 (stx:rel_exp_st-op1 st))
            (op2 (stx:rel_exp_st-op2 st))
            (temp1 (make-temp))
            (temp2 (make-temp))
            (temp3 (make-temp)))
       (list (correct-let (in:letstmt temp1 (syn-to-inter op1)))
             (correct-let (in:letstmt temp2 (syn-to-inter op2)))
             (correct-let
              (in:letstmt temp3 (in:relopexp op temp1 temp2))))))       
    ((stx:alge_exp_st? st) 
     (let* ((op (stx:alge_exp_st-alge-ope st))
            (op1 (stx:alge_exp_st-op1 st))
            (op2 (stx:alge_exp_st-op2 st))
            (temp1 (make-temp))
            (temp2 (make-temp))
            (temp3 (make-temp)))
       (list (correct-let (in:letstmt temp1 (syn-to-inter op1)))
             (correct-let (in:letstmt temp2 (syn-to-inter op2)))
             (correct-let 
              (in:letstmt temp3 (in:aopexp op temp1 temp2))))))
    ((stx:unary_exp_st? st) '())
    ((stx:constant_st? st) 
     (let* ((num (stx:constant_st-cons st))
            (temp (make-temp)))
       (correct-let 
        (in:letstmt temp (in:intexp num)))))
    ((stx:null_statement_st? st) (in:emptystmt))
    ((stx:exp_in_paren_st? st) (syn-to-inter (stx:exp_in_paren_st-exp st)))
    ((stx:if_else_st? st) 
     (let* ((var (stx:if_else_st-cond-exp st))
            (stmt1 (stx:if_else_st-state st))
            (stmt2 (stx:if_else_st-else-state st))
            (temp (make-temp)))
       (list (syn-to-inter var)
             (in:ifstmt (ref-temp) 
                        (syn-to-inter stmt1)
                        (syn-to-inter stmt2)))))
    ((stx:while_st? st) 
     (let* ((var (stx:while_st-cond-exp st))
            (stmt (stx:while_st-statement st))
            (temp (make-temp)))
       (list (syn-to-inter var)
             (in:whilestmt (ref-temp) (syn-to-inter stmt)))))
    ((stx:sem_return_st? st) 
     (let* ((ret (stx:sem_return_st-exp st))
            (temp (make-temp)))
       (list (correct-let 
              (in:letstmt temp (syn-to-inter ret)))
             (in:returnstmt temp))))
    ((stx:compound_st? st) 
     (let* ((decls (stx:compound_st-declaration-list st))
            (stmts (stx:compound_st-statement-list st)))
       (in:compdstmt (cond ((equal? 'nodecl decls) '())
                           (else (syn-to-inter decls)))
                     (cond ((equal? 'nostat stmts) '())
                           (else (syn-to-inter stmts))))))           
    ((stx:func_st? st) '())
    ((list? st) (flatten (map syn-to-inter st)))
    ((obj? st) (in:varexp st))
    (else (error (format "\n check syn-to-code! ~a\n" st)))))



;テスト
(define p (open-input-file "test01.c"))
(port-count-lines! p)
(display 
 (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が中間命令生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(gen-intermed (sem-analyze-tree (k08:parse-port p)))