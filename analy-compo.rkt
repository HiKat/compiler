#lang racket
(require "myenv.rkt")
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))
(require "mymap.rkt")
(require "check-env.rkt")
(provide (all-defined-out))
;(struct obj (name lev kind type)#:transparent)
(define current-lev 0)
;env、para-env初期化
(define env '())
(define para-env '())
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
                          ((eq? 2 (comp_flag-n flag)) (stx:compound_dec_st-declaration_st st))
                          ((or (eq? 3 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 'nodecl)))
         ;statement-listにはstatementが入る.
         ;???????????????????????ここは返り値で構造体を作った方がいい??????????????????
         (stat-list (cond ((eq? 1 (comp_flag-n flag)) (stx:compound_st-statement-list st))
                          ((eq? 3 (comp_flag-n flag)) (stx:compound_dec_st-declaration_st st))
                          ((or (eq? 2 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 'nostat)))
         ;このcompound-statementの解析にのみ用いる環境
         (comp-env '())
         )
    ;意味解析開始時にcurrent-levを一つ繰り上げる 
    (set! current-lev (+ current-lev 1))
    ;意味解析終了時にcurrent-levを一つ繰り下げる
    (set! current-lev (- current-lev 1))
    (stx:compound_st (cond ((eq? 1 (comp_flag-n flag)) 
                            #t)
                           ((eq? 2 (comp_flag-n flag)) 
                            #t)
                           ((or (eq? 3 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 
                            'nodecl))
                     (cond ((eq? 1 (comp_flag-n flag)) 
                            #t)
                           ((eq? 3 (comp_flag-n flag)) 
                            #t)
                           ((or (eq? 2 (comp_flag-n flag))(eq? 4 (comp_flag-n flag))) 
                            'nostat))
                     
         
         


(define test1
  (stx:compound_st
   ;declaration-list
   (cons
    ;declaration-listのcar
    (stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st (id_st 'a 'test)))
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
                         
                          