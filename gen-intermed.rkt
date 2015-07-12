#lang racket

;一時変数の再使用、最適化の余地あり.
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
;(define temp-name 'temp0) 
(define intermed-code (list '()))
(define temp-decl (list '()))
(define intermed-decllist (list '()))
(define int-load '())
;一時変数を作成する関数.
;引数無し
;返り値
;新しい一時変数名
;一時変数の宣言文を局所的に行う場合
(define (make-temp)
  (let* ((new-temp (+ 1 temp))
         (new-name (string->symbol (string-append "temp" (number->string temp))))
         (new-obj (obj new-name 'temp 'temp 'temp 'temp)))
    (set! temp new-temp)
    ;(set! temp-name new-name)
    ;(set! intermed-code (flatten (append intermed-code (list (in:vardecl new-obj)))))
    (set! temp-decl (flatten (append temp-decl (list (in:vardecl new-obj)))))
    new-obj))


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

(define (gen-intermed st) 
  (let* ((out (map syn-to-inter st))
         (temp-decl-space temp-decl))
    (flatten (list int-load temp-decl-space out))))

;引数
;抽象構文構造体
;戻り値
;中間命令構造体
;
(define (syn-to-inter st) 
  (cond
    ((stx:declaration_st? st) 
     (let* (;declarator-listはobjのlistもしくはobj単体 
            (decl-ls (stx:declaration_st-declarator-list st))
            (meaningless (set! intermed-code '())))
       (cond ((obj? decl-ls) 
              (in:vardecl (decl-ls)))
             ((list? decl-ls) 
              (flatten (append intermed-code (map (lambda (x) (in:vardecl x)) decl-ls))))
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
       (begin
         #;(set! intermed-code 
               (append intermed-code
                       (list (cond ((stx:unary_exp_st? dest) (in:writestmt dest src))
                                   ((stx:unary_exp_st? src) (in:readstmt dest src))
                                   (else (in:letstmt dest (syn-to-inter src)))))))
         (cond ((stx:unary_exp_st? dest) (in:writestmt (syn-to-inter dest) src))
               ((stx:unary_exp_st? src) (in:readstmt (syn-to-inter dest) src))
               (else (in:letstmt (syn-to-inter dest) (syn-to-inter src)))))
         ;dest
         ))
    ((stx:logic_exp_st? st) 
     (let* ((op (stx:logic_exp_st-log-ope st))
            (op1 (stx:logic_exp_st-op1 st))
            (op2 (stx:logic_exp_st-op2 st))
            (temp1 (syn-to-inter op1))
            (temp2 (syn-to-inter op2))
            (temp3 (syn-to-inter (make-temp))))
       (cond ((equal? 'or op)
              (begin
                (set! 
                 intermed-code
                 (append 
                  intermed-code 
                  (flatten (list (in:ifstmt temp1 
                                            (correct-let 
                                             (in:letstmt temp3 (in:intexp 1))) 
                                            (in:ifstmt temp2 
                                                       (in:intexp 1) 
                                                       (correct-let 
                                                        (in:letstmt temp3 (in:intexp 0)))))))))
                temp3))
             ((equal? 'and op)
              (begin 
                (set! intermed-code
                      (append intermed-code
                              (flatten (list (in:ifstmt temp1 
                                                        (in:ifstmt temp2 
                                                                   (correct-let 
                                                                    (in:letstmt temp3 (in:intexp 1))) 
                                                                   (correct-let (in:letstmt temp3 (in:intexp 0)))) 
                                                        (correct-let 
                                                         (in:letstmt temp3 (in:intexp 0))))))))
                temp3)))))                              
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
            (temp1 (syn-to-inter op1))
            (temp2 (syn-to-inter op2))
            (temp3 (syn-to-inter (make-temp))))
       (begin
         (set! intermed-code
               (append 
                intermed-code
                (flatten (list (correct-let
                                (in:letstmt temp3 (in:relopexp op temp1 temp2)))))))
         temp3)))
    ((stx:alge_exp_st? st) 
     (let* ((op (stx:alge_exp_st-alge-ope st))
            (op (cond ((equal? 'add op) '+)
                      ((equal? 'sub op) '-)
                      ((equal? 'mul op) '*)
                      ((equal? 'div op) '/)))
            (op1 (stx:alge_exp_st-op1 st))
            (op2 (stx:alge_exp_st-op2 st))
            (temp1 (syn-to-inter op1))
            (temp2 (syn-to-inter op2))
            (temp3 (syn-to-inter (make-temp)))
            (op1-type (cond ((in:varexp? op1) (cond ((type_pointer? (obj-type (in:varexp-var op1))) 'int-pointer)
                                                    (else 'int)))
                            (else 'int)))
            (op2-type (cond ((in:varexp? op2) (cond ((type_pointer? (obj-type (in:varexp-var op1))) 'int-pointer)
                                                    (else 'int)))
                            (else 'int))))
       (cond ((and (equal? 'int-pointer op1-type) (equal? 'int op2-type)) 
              (begin
                (set! intermed-code
                      (append 
                       intermed-code
                       (flatten (list (correct-let 
                                       (in:letstmt temp3 (in:aopexp op temp1 (in:aopexp 'mul (in:intexp 4) temp2))))))))
                temp3))
             ((and (equal? 'int-pointer op2-type) (equal? 'int op1-type)) 
              (begin
                (set! intermed-code
                      (append 
                       intermed-code
                       (flatten (list (correct-let 
                                       (in:letstmt temp3 temp1 (in:aopexp op temp2 (in:aopexp 'mul (in:intexp 4)))))))))
                temp3))
             (else
              (begin
                (set! intermed-code
                      (append 
                       intermed-code
                       (flatten (list 
                                       (in:letstmt temp3 (in:aopexp op temp1 temp2))))))
                temp3)))))
    ((stx:unary_exp_st? st) 
     (let* ((op (stx:unary_exp_st-mark st))
            (op1 (stx:unary_exp_st-op st)))
       (cond ((equal? 'amp op) (in:addrexp (syn-to-inter op1)))
             (else st))))
    ((stx:constant_st? st) 
     (let* ((num (stx:constant_st-cons st))
            (temp (syn-to-inter (make-temp))))
       (begin
       (set! intermed-code
             (append 
              intermed-code
              (flatten (list  
                              (in:letstmt temp (in:intexp num))))))
       temp)))
    ((stx:null_statement_st? st) 
     (begin
     (set! intermed-code (append intermed-code (flatten (list (in:emptystmt)))))
     '()))
    ((stx:exp_in_paren_st? st) (syn-to-inter (stx:exp_in_paren_st-exp st)))
    ((stx:if_else_st? st) 
     (let* ((var (stx:if_else_st-cond-exp st))
            (stmt1 (syn-to-inter (stx:if_else_st-state st)))
            (stmt2 (syn-to-inter (stx:if_else_st-else-state st))))
       (in:ifstmt (syn-to-inter var) 
                  stmt1
                  stmt2)))
    ((stx:while_st? st) 
     (let* ((var (stx:while_st-cond-exp st))
            ;(meanignless (set! intermed-code '()))
            (stmt (syn-to-inter (stx:while_st-statement st)))
            ;(stmt-intermed intermed-code)
            )
       (in:whilestmt (syn-to-inter var) stmt)))
    ((stx:sem_return_st? st) 
     (let* ((ret (stx:sem_return_st-exp st)))
       (in:returnstmt (syn-to-inter ret))))
    ((stx:compound_st? st) 
     (let* ((decls (stx:compound_st-declaration-list st))
            (stmts (stx:compound_st-statement-list st))
            (original-intermed intermed-code)
            (original-temp temp)
            (original-temp-decl temp-decl)
            ;中間命令のスタックの初期化
            (meaningless (set! intermed-code '()))
             ;一時変数作成のcounterの初期化
            (meaningless (set! temp 0))
            ;一時変数宣言のスタックを初期化
            (meaningless (set! temp-decl (list '())))
            (decl (cond ((equal? 'nodecl decls) '())
                        (else (flatten (map syn-to-inter decls)))))
            (stmt (cond ((equal? 'nostat stmts) '())
                           (else (flatten (map syn-to-inter stmts)))))
            (stmt-intermed intermed-code)
            (stmt-temp-decl temp-decl)
            (meaningless (set! intermed-code original-intermed))
            (meaningless (set! temp original-temp))
            (meaningless (set! temp-decl original-temp-decl)))
       (in:compdstmt (flatten (append decl stmt-temp-decl)) (flatten (append stmt-intermed stmt)))))          
    ((stx:func_st? st) 
     (let* ((vars (flatten (stx:func_st-para st)));varsは図べて一旦変数に格納してそれを関数呼び出しに入れる.
            (f (stx:func_st-name st))
            (temp (syn-to-inter (make-temp)))
            (let-var (map (lambda (x) (correct-let (in:letstmt (make-temp) (syn-to-inter x)))) vars)))
       (begin 
         (set! 
          intermed-code
          (append 
           intermed-code
           (flatten (list
                     let-var
                     (correct-let 
                      (in:letstmt temp 
                                  (in:callstmt temp 
                                               f 
                                               (map (lambda (x) (in:letstmt-var x)) let-var))))))))
         temp)))
    ((obj? st) (cond ((type_array? (obj-type st))
                      (let* ((name (obj-name st))
                             (lev (obj-lev st))
                             (kind (obj-kind st))
                             (array-type (type_array-type (obj-type st)))
                             (array-size (type_array-size (obj-type st)))
                             (pos (obj-pos st)))
                        (obj name lev kind (type_array array-type (syn-to-inter array-size)) pos))
                      ;(error "ERROR")
                      )
                     (else 
                      (in:varexp st)
                      ;(error "ERROR")
                      )))
    (else (error (format "\n check syn-to-code! ~a\n" st)))))



;テスト
(define p (open-input-file "test01.c"))
(port-count-lines! p)
(display 
 (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が中間命令生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(gen-intermed (sem-analyze-tree (k08:parse-port p)))