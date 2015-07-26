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
;syn-to-interで配列型を拾ったときに使用するスペース
;使うときは必ず初期化.
(define temp-space '())

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

;配列のベースアドレスを格納するための一時変数を作成するための
(define (make-base-temp array-obj)
  (let* ((name (obj-name array-obj))
         (lev (obj-lev array-obj))
         (kind (obj-kind array-obj))
         (new-obj (obj (array_base name lev kind) 'temp 'array-base 'temp 'temp)))
    ;(set! temp-name new-name)
    ;(set! intermed-code (flatten (append intermed-code (list (in:vardecl new-obj)))))
    ;(set! temp-decl (flatten (append temp-decl (list (in:vardecl new-obj)))))
    new-obj))
;上で使う
(struct array_base (name lev kind) #:transparent)


;代入する際に冗長な表現を削除する関数.
;引数はletstmt
;戻り値は適切な letstmt
(define (correct-let ls)
  ;(letstmt var1 (letstmt var2 exp))を(letstmt var1 exp)に 
  (cond ((in:letstmt? ls)
         (cond 
           ((in:letstmt? (in:letstmt-exp ls))
            (in:letstmt (in:letstmt-var ls) (in:letstmt-exp (in:letstmt-exp ls))))
           (else ls)))
        (else ls)))


;引数
;抽象構文木
;戻り値
;中間命令列

(define (gen-intermed st) 
  (let* ((out (map syn-to-inter st))
         (temp-decl-space temp-decl))
    (flatten (list temp-decl-space out))))

;以下で使う
;arには配列のobjが入る.
(struct array_base_add (ar) #:transparent)

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
            ;(meaningless (set! intermed-code '()))
            (meaningless (set! temp-space '())))
       (cond ((obj? decl-ls) 
              (cond ((type_array? (obj-type decl-ls)) 
                     (let* ((base-temp (make-base-temp decl-ls)))
                       (list 
                        (in:vardecl (decl-ls))
                        (in:vardecl base-temp))))
                    (else (in:vardecl (decl-ls)))))
             ((list? decl-ls) 
              (flatten 
               (map (lambda (x) 
                      (cond ((type_array? (obj-type x)) 
                             (let* ((base-temp (make-base-temp x)))
                               (set! intermed-code 
                                     (flatten (append intermed-code 
                                                      (list (in:letstmt base-temp (array_base_add x))))))
                               (list
                                (in:vardecl x)
                                (in:vardecl base-temp))))
                            (else (in:vardecl x)))) 
                    decl-ls)))
             (error (format "\n check syn-to-inter! ~a\n" st)))))
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
         (cond ((and (stx:unary_exp_st? dest) 
                     (not (stx:unary_exp_st? src)))
                (in:writestmt (syn-to-inter dest) (syn-to-inter src)))
               ((and (stx:unary_exp_st? src)
                     (not (not (stx:unary_exp_st? src))))
                (in:readstmt (syn-to-inter dest) (syn-to-inter src)))
               (else (in:letstmt (syn-to-inter dest) 
                                 (cond ((or (in:intexp? (syn-to-inter src))
                                            (in:aopexp? (syn-to-inter src))
                                            (in:relopexp? (syn-to-inter src))
                                            (in:addrexp? (syn-to-inter src)))
                                        (syn-to-inter src))
                                       (else (in:varexp (syn-to-inter src))))))))))
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
                  (flatten 
                   (list 
                    (in:ifstmt temp1 
                               (correct-let 
                                (in:letstmt temp3 (in:intexp 1))) 
                               (in:ifstmt temp2 
                                          (correct-let 
                                           (in:letstmt temp3 (in:intexp 1))) 
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
                                                                   (correct-let 
                                                                    (in:letstmt temp3 (in:intexp 0)))) 
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
            ;配列のサイズtype_arrayの要素sizeに数字がそのまま入っている
            ;場合があるためそのときはconstant_stに入れる.
            (op1 (cond ((number? op1) (stx:constant_st op1 'no))
                       (else op1)))
            (op2 (cond ((number? op2) (stx:constant_st op2 'no))
                       (else op2)))
            (temp1 (syn-to-inter op1))
            (temp2 (syn-to-inter op2))
            (temp3 (syn-to-inter (make-temp)))
            (op1-type (cond ((obj? op1) (cond ((type_pointer? (obj-type op1)) 'int-pointer)
                                              (else 'int)))
                            (else 'int)))
            (op2-type (cond ((obj? op2) (cond ((type_pointer? (obj-type op2)) 'int-pointer)
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
             ((equal? 'ast op) op1))))
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
     (cond 
       ;組み込み関数printのとき
       ((equal? 'print (obj-name (stx:func_st-name st))) 
        (let* ((vars (flatten (stx:func_st-para st))))
          (in:printstmt (syn-to-inter (car vars)))))
       ;それ以外
       (else (let* ((vars (stx:func_st-para st))
                    (f (stx:func_st-name st))
                    (return-type (type_fun-out (obj-type f)))
                    (temp (syn-to-inter (make-temp)))
                    (let-var
                     (cond ((equal? 'nopara vars)'())
                           (else (map 
                                  (lambda (x) 
                                    (correct-let (in:letstmt (make-temp) 
                                                             (cond ((or (in:intexp? (syn-to-inter x))
                                                                        (in:aopexp? (syn-to-inter x))
                                                                        (in:relopexp? (syn-to-inter x))
                                                                        (in:addrexp? (syn-to-inter x)))
                                                                    (syn-to-inter x))
                                                                   (else (in:varexp (syn-to-inter x))))))) 
                                  vars)))))
               (begin 
                 (set! 
                  intermed-code
                  (append 
                   intermed-code
                   (flatten (list
                             let-var
                             (correct-let 
                              (in:callstmt temp 
                                           f 
                                           (map (lambda (x) (in:letstmt-var x)) let-var)))))))
                 (cond ((equal? 'void return-type) '())
                       (else temp)))))))
    ((obj? st) (cond ((type_array? (obj-type st))
                      (let* ((name (obj-name st))
                             (lev (obj-lev st))
                             (kind (obj-kind st))
                             (array-type (type_array-type (obj-type st)))
                             (array-size (type_array-size (obj-type st)))
                             (pos (obj-pos st)))
                        (obj name lev kind 
                             (type_array array-type (syn-to-inter 
                                                     (stx:alge_exp_st 
                                                      'add 
                                                      (obj (array_base name lev kind) 'temp 'array-base 'temp 'temp)
                                                      (stx:alge_exp_st 'mul (stx:constant_st 4 'syntax-sugar-gen-intermed) array-size 'syntax-sugar-gen-intermed)
                                                      'syntax-sugar-gen-intermed))) pos)))
                     (else st)))
    (else (error (format "\n check syn-to-inter!! ~a\n" st)))))



;引数は
;中間命令文の構造体
;compdstmt外側での一時変数の宣言
(define (optimize-cmpd st outer-temp-decls)
  (cond ((in:compdstmt? st)
         (let* ((decls (in:compdstmt-decls st))
                (stmts (in:compdstmt-stmts st))
                (temp-decls 
                 (filter (lambda (x) (equal? 'temp (obj-type (in:vardecl-var x)))) decls))
                (new-var-decls (remove* temp-decls decls))
                (new-temp-decls 
                 ;declsのうちでouter-declに含まれないもの
                 ;本当はdeclsのうち一時変数とプログラム中での宣言を区別して、
                 ;前者は全て残し、後者は最適化する必要がある.
                 (remove* outer-temp-decls temp-decls))
                (new-outer-decls (flatten (append (list outer-temp-decls) (list new-temp-decls)))))
           (in:compdstmt (flatten (append (list new-var-decls) (list new-temp-decls)))
                         (flatten (map (lambda (x) (optimize-cmpd x new-outer-decls)) stmts)))))
        ((in:ifstmt? st)
         (let* ((var (in:ifstmt-var st))
                (stmt1 (in:ifstmt-stmt1 st))
                (stmt2 (in:ifstmt-stmt2 st)))
           (in:ifstmt var 
                      (optimize-cmpd stmt1 outer-temp-decls) 
                      (optimize-cmpd stmt2 outer-temp-decls))))
        ((in:whilestmt? st)
         (let* ((var (in:whilestmt-var st))
                (stmt (in:whilestmt-stmt st)))
           (in:whilestmt var (optimize-cmpd stmt outer-temp-decls))))
        (else st)))

(define (optimize-intermed intermed)
  (map (lambda (x) (cond ((in:fundef? x) 
                          (let* ((var (in:fundef-var x))
                                 (parms (in:fundef-parms x))
                                 (body (optimize-cmpd (in:fundef-body x) '())))
                            (in:fundef var parms body)))
                         (else x)))
       intermed))

(define (gen-optimized-intermed tree)
  (optimize-intermed (gen-intermed tree)))


;テスト
#;(begin
(define p-g-itmd (open-input-file "basic/swap.sc"))
(port-count-lines! p-g-itmd)
(display 
 (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が中間命令生成の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(gen-optimized-intermed (sem-analyze-tree (k08:parse-port p-g-itmd)))
)