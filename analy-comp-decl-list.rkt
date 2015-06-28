#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))

;構造体の型の参照は(stx:spec_st-type s)

(define env 'empty)
(define current-lev 0)
;意味解析を実行中の関数のレベルを入れる変数.

(struct obj (name lv kind type)#:transparent) 

;obj構造体のtypeの要素になりうる構造体.
(struct type-pointer (pointer type))
(struct array (type size))


(define (extract-name-from-declarator_st st)
  (cond ((struct? st)
         (cond ((stx:declarator_st? st) (stx:id_st-name (stx:declarator_st-var st)))
               ((stx:declarator_ast_st? st) (stx:id_st-name (stx:declarator_ast_st-var st)))))
        ((cons? st)
         (cons (extract-name-from-declarator_st (car st))
               (extract-name-from-declarator_st (cdr st))))))



;compound-statement内の(list* declaration_st...)
;もしくはdeclaration_st単体とcomp-levを引数に取り
;objのlist*を返す関数.
(define (analy-comp-decl-list st comp-lev)
  ;;;;;;;;;;;;
  (define (analy-comp-declaration st lev)
    ;;;;;;
    (define (separate-name name lev kind type)
      (cond ((cons? name) (let* ((meaningless 1)) 
                            (cons (separate-name (car name) lev kind type)
                                  (separate-name (cdr name) lev kind type))))
            (else (obj name lev kind type))))
    ;;;;;;
    (let* ((name (extract-name-from-declarator_st (stx:declaration_st-declarator-list st)))
           (lev comp-lev);大域変数もしくはcompound-statement内のどちらか
           (kind 'var)
           (type (stx:spec_st-type (stx:declaration_st-type-spec st))))
      (separate-name name lev kind type)
      ;(set! env (env:extend-env (obj name lev kind type) env))
      ))
  ;;;;;;;;;;;;
  (cond ((cons? st) (cons (analy-comp-decl-list (car st) comp-lev)
                          (analy-comp-decl-list (cdr st) comp-lev)))
        ;analy-comp-declarationはcurrent-levを引数levに換え、
        ;戻り値がobjであるようなanaly-declaration_stを修正した関数
        ((struct? st) (analy-comp-declaration st comp-lev))))



(define test
  (cons
   (stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st (stx:id_st 'a 'test)))
   (stx:declaration_st
    (stx:spec_st 'int 'test)
    (cons
     (stx:declarator_st (stx:id_st 'a 'test))
     (stx:declarator_st (stx:id_st 'b 'test))))))

(define test2
(stx:compound_st
   (stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st 
                                                 (stx:id_st 'a 'test)))
   (stx:compound_dec_st
    (stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st 
                                                  (stx:id_st 'c 'test)))))
  )

(analy-comp-decl-list test 0)
;(analy-compound_st test2)




