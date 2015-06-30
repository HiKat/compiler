#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require "check-env.rkt")
(provide (all-defined-out))

(struct obj (name lev kind type)#:transparent)
(define current-lev 0)
;env、para-env初期化
(define env '())
(define para-env '())
;compound-statementの解析にのみ用いる環境
(define comp-env '())
;objtypeの要素になりうる構造体.
(struct type-pointer (pointer type) #:transparent)
;compound-statement内に
;declaration-listの有無で'normalか'nodecl
;statement-listの有無で'normalか'nostat
;nは整理番号
;を保存するフラグ構造体
(struct comp_flag (decl stat n))


(define (analy-compound_st st)
  (let* (
         (flag (cond ((stx:compound_st? st) (comp_flag 'normal 'normal 1))
                     ((stx:compound_dec_st? st) (comp_flag 'normal 'nostat 2))
                     ((stx:compound_sta_st? st) (comp_flag 'nodecl 'normal 3))
                     ((stx:compound_null_st? st) (comp_flag 'nodecl 'nostat 4))))
         ;decl-listには(list* stx:declaration_st...)
         (decl-list (cond ((eq? 1 (comp_flag-n flag)) (stx:compound_st-declaration-list st))
                          ((eq? 2 (comp_flag-n flag)) (stx:compound_dec_st-declaration-list st))
                          ((or (eq? 3 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 'nodecl)))
         ;statement-listにはstatementのlist*が入る.
         ;処理する際はmap*で
         (stat-list (cond ((eq? 1 (comp_flag-n flag)) (stx:compound_st-statement-list st))
                          ((eq? 3 (comp_flag-n flag)) (stx:compound_dec_st-declaration-list st))
                          ((or (eq? 2 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 'nostat)))
         ;意味解析開始時にcurrent-levを一つ繰り上げる 
         (this-lev (+ current-lev 1))
         ;decl-listに入るのは(list stx:declaration_st...)
         (decl-list (cond ((eq? 'normal (comp_flag-decl flag)) 
                            ;このときオブジェクトはcomp-envに追加する必要がある.
                            ;(map* analy-compdecl decl-list
                            (map* (lambda (x) (analy-compdecl x this-lev)) decl-list))                  
                           ((eq? 'nodecl (comp_flag-decl flag))
                            'nodecl)))
         ;decl-listから環境を生成する.
         ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!この状態ではcomp-envはobjもしくは(list obj...)のlist構造.
         (comp-env (env:separate-list 
                    (env:make-list-list 
                     (map (lambda (x) (stx:declaration_st-declarator-list x)) decl-list))))
         ;チェックを行う.自身の環境とのチェック、
         ;大域環境とのチェックの2つの環境を参照してチェックを行う必要がある.
         )
    
    ;意味解析終了時にcurrent-levを一つ繰り下げる
    ;(set! this-lev (- current-lev 1))
    (stx:compound_st 
     decl-list
     (cond ((eq? 'normal (comp_flag-stat flag)) 
            ;本当はこのstat-listもここで関数に引き渡してチェックをおこなう.
            ;その際にチェックはこのcompoun-statementのオブジェクトのみが
            ;登録された環境で行う.
            stat-list)
           ((eq? 'nostat (comp_flag-stat flag)) 
            'nostat)))
    ))


(define (analy-compdecl st lev)
  ;;;;
  ;内部定義
  ;(stx:declarator_st...)と
  ;levと  
  ;'intもしくは'void
  ;を引数にとり
  ;obj
  ;を返す関数.
  (define (make-obj-from-decl decl type lev)
    (let* ((id (cond ((stx:declarator_st? decl) 
                      (stx:declarator_st-var decl))
                     ((stx:declarator_ast_st? decl)
                      (stx:declarator_ast_st-var decl))))
           (name (stx:id_st-name id))          
           (flag (cond ((stx:declarator_st? decl) 'nomal)
                       ((stx:declarator_ast_st? decl) 'pointer)))
           (kind 'var)
           (type (cond ((eq? flag 'nomal) type)
                       ((eq? flag 'pointer) (list 'pointer type)))))
      (obj name lev kind type)))
  ;;;;内部定義ここまで
  (let* (;typeに入っているのは (stx:spec_st 'intか'void ポジション)
         (type (stx:declaration_st-type-spec st))
         (declarator-list (stx:declaration_st-declarator-list st))
         ;objのlistを作成する.
         (obj-list (map* 
                    (lambda (x) (make-obj-from-decl x (stx:spec_st-type type) lev))
                    declarator-list)))
    ;意味解析上のエラーがないかは外側でチェックするのでここでは実装しなくて良い.
    (stx:declaration_st type obj-list)))

(define (analy-compstate


                     
         
         


(define test1
  (stx:compound_st
   ;declaration-list
   (cons
    ;declaration-listのcar
    (stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st (stx:id_st 'a 'test)))
    ;declaration-listのcdr
    (stx:declaration_st
     (stx:spec_st 'int 'test)
     (cons
      (cons
       (stx:declarator_ast_st (stx:id_st 'b 'test))
       (stx:declarator_st (stx:id_st 'c 'test)))
      (stx:declarator_st (stx:id_st 'd 'test)))))
   ;statement-list
   (stx:exp_with_semi_st (stx:func_st 'func2 (stx:id_st 'a 'test)))))

(analy-compound_st test1)

                         
                          