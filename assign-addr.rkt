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
(require "gen-intermed.rkt")
(provide (all-defined-out))

;spの位置
(define sp 0)
;関数内で作成されたオフセットとobjの対応.
(define fun-stack '())


;オフセットを追加したdecl構造体obj-off
(struct obj-off (name lev kind type pos off) #:transparent)
;オフセットなしのobjをオフセット付きのdecl構造体obj-offに変換する関数.
(define (convert-obj ob off)
  (let* ((name (obj-name ob))
         (lev (obj-lev ob))
         (kind (obj-kind ob))
         (type (obj-type ob))
         (pos (obj-pos ob))
         (new-obj-off (obj-off name lev kind type pos off)))
    (set! fun-stack (flatten (append fun-stack (list new-obj-off))))
    new-obj-off))

;引数
;vardeclのlist
;iはスタートの位置
;jは次の位置との差(基本的には4)
;戻り値
;offsetを加えたvardeclのlist
(define (convert-iter i j vardecl-list)
  (cond ((equal? '() vardecl-list)  '())
        (else 
         (let* ((ob (in:vardecl-var (car vardecl-list)))
                ;flagは取り出したobjが配列型であるとき1が入る.
                (flag (cond ((type_array? (obj-type ob)) 1)
                            (else 0)))
                ;car-convertはvardeclからなるlistの先頭のみをにオフセットを付加したもの.
                (car-convert (cond ((equal? 1 flag) 
                                    (list (in:vardecl 
                                           (convert-obj 
                                            ob 
                                            (+ i 
                                               (* j
                                                  (- (type_array-size (obj-type ob)) 
                                                     1)))))))
                                   ((equal? 0 flag)
                                    (list (in:vardecl (convert-obj ob i)))))))
           (cond ((equal? 0 flag) (set! sp (+ j i)))
                 ((equal? 1 flag) (set! sp (+ j (+ i 
                                                   (* j
                                                      (- (type_array-size (obj-type ob)) 
                                                         1)))))))
           (flatten 
            (append 
             car-convert
             (list (convert-iter sp j (cdr vardecl-list)))))))))
;引数
;プログラムを中間命令文に変換したもの
;戻り値
;アドレス割り当てを行ったあとのin:fundef
(define (sub-assign-add-intermed st)
  (cond ((in:fundef? st)
         (let* ((meaningless (set! fun-stack '()))
                (def-obj (in:fundef-var st))
                ;vardeclのlist
                (vardecl-list (in:fundef-parms st))
                (meaningless (set! sp 0))
                (convert-vardecl (convert-iter 4 4 vardecl-list))
                (meaningless (set! sp 0))
                (body (in:fundef-body st)))
           (in:fundef def-obj 
                      convert-vardecl
                      (assign-add-cmpd body sp))))
        (else st)))

(define (assign-add-intermed in)
  (map sub-assign-add-intermed in))

;引数
;中間命令の構造体
;iは開始位置
;戻り値
;アドレス行ったあとのin:compdstmt
(define (assign-add-cmpd st i)
  (cond ((in:compdstmt? st)
         (let* ((decls (in:compdstmt-decls st))
                (stmts (in:compdstmt-stmts st)))
           ;(set! sp (+ sp (* -4 (length decls))))
           (in:compdstmt 
            (convert-iter i -4 decls) 
            (map (lambda (x) (assign-add-cmpd x sp)) stmts))))
        ((in:ifstmt? st)
         (let* ((var (in:ifstmt-var st))
                (stmt1 (in:ifstmt-stmt1 st))
                (stmt2 (in:ifstmt-stmt2 st)))
           (in:ifstmt var 
                      (assign-add-cmpd stmt1 sp) 
                      (assign-add-cmpd stmt2 sp))))
        ((in:whilestmt? st)
         (let* ((var (in:whilestmt-var st))
                (stmt (in:whilestmt-stmt st)))
           (in:whilestmt var (assign-add-cmpd stmt sp))))
        (else st)
        ))


;引数
;obj
;戻り値
;off-obj
;objを受け取ってdef-stackの中から対応するobj-offを返す関数
;大域変数の場合はそのままobjを返す.
(define (ref-obj ob)
  (let* ((name (obj-name ob))
         (lev (obj-lev ob))
         (kind (obj-kind ob))
         (type (obj-type ob))
         ;flagにはobjが配列型のとき1が入る.
         (flag (cond ((type_array? type) 1)
                     (else 0)))
         (pos (obj-pos ob)))
    (cond ((equal? 0 lev) ob)
          (else (flatten (list (filter (lambda (x)
                                         (and (equal? name (obj-off-name x))
                                              (equal? lev (obj-off-lev x))
                                              (equal? kind (obj-off-kind x))
                                              ;配列型の際は参照してくるアドレスを工夫する必要がある.
                                              (cond ((equal? 1 flag) #t)
                                                    (else (equal? type (obj-off-type x))))
                                              (equal? pos (obj-off-pos x))))
                                       fun-stack)))))))
                                       
;引数
;中間命令文
;戻り値
;varexp内のobjにオフセットを付加した中間命令文を返す.
(define (ref-add i)
  (cond ((obj? i) (ref-obj i))
        ((in:compdstmt? i)
         (let* ((decls (in:compdstmt-decls i))
                (stmts (in:compdstmt-stmts i)))
           (in:compdstmt decls 
                         (map ref-add stmts))))
        ((in:emptystmt? i) i)
        ((in:letstmt? i) 
         (let* ((var (in:letstmt-var i))
                (exp (in:letstmt-exp i)))
           (in:letstmt (ref-add var) (ref-add exp))))
        ((in:writestmt? i)
         (let* ((dest (in:writestmt-dest i))
                (src (in:writestmt-src i)))
           (in:writestmt (ref-add dest) (ref-add src))))
        ((in:readstmt? i)
         (let* ((dest (in:readstmt-dest i))
                (src (in:readstmt-src i)))
           (in:readstmt (ref-add dest) (ref-add src))))
        ((in:ifstmt? i)
         (let* ((var (in:ifstmt-var i))
                (stmt1 (in:ifstmt-stmt1 i))
                (stmt2 (in:ifstmt-stmt2 i)))
           (in:ifstmt (ref-add var) (ref-add stmt1) (ref-add stmt2))))
        ((in:whilestmt? i)
         (let* ((var (in:whilestmt-var i))
                (stmt (in:whilestmt-stmt i)))
           (in:whilestmt (ref-add var) (ref-add stmt))))
        ((in:callstmt? i)
         (let* ((dest (in:callstmt-dest i))
                 (f (in:callstmt-f i))
                 (vars (in:callstmt-vars i)))
           (in:callstmt (ref-add dest) f (map ref-add vars))))
        ((in:returnstmt? i)
         (let* ((var (in:returnstmt-var i)))
           (in:returnstmt (ref-add var))))
        ((in:printstmt? i)
         (let* ((var (in:printstmt-var i)))
           (in:printstmt (ref-add var))))
        ((in:varexp? i)
         (let* ((var (in:varexp-var i)))
           (in:varexp (ref-add var))))
        ((in:intexp i) i)
        ((in:aopexp? i)
         (let* ((op (in:aopexp-op i))
                (var1 (in:aopexp-var1 i))
                (var2 (in:aopexp-var2 i)))
           (in:aopexp op (ref-add var1) (ref-add var2))))
        ((in:relopexp? i)
         (let* ((op (in:relopexp-op i))
                (var1 (in:relopexp-var1 i))
                (var2 (in:relopexp-var2 i)))
           (in:relopexp op (ref-add var1) (ref-add var2))))
        ((in:addrexp? i)
         (let* ((var (in:addrexp-var i)))
           (in:addrexp (ref-add var))))
        (else (error i))))

;varexpが出現するのはfundef内部のin:compdstmt内のみ
(define (ref-add-intermed i)
  (map (lambda (x) 
         (cond ((in:fundef? x) 
                (let* ((var (in:fundef-var x))
                       (parms (in:fundef-parms x))
                       (body (in:fundef-body x)))
                  (in:fundef var parms (ref-add body))))
               (else x)))
       i))
         
                                   



;テスト
(define test-ass (open-input-file "test01.c"))
(port-count-lines! test-ass)
(display 
 (format "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が相対番地割り当ての実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.\n"))
(ref-add-intermed 
 (assign-add-intermed (gen-optimized-intermed (sem-analyze-tree (k08:parse-port test-ass)))))
