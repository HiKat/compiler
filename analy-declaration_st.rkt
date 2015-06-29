#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))

(define env 'empty)

;意味解析を実行中の関数のレベルを入れる変数.
(define current-lev 0)
;意味解析のオブジェクト
(struct obj (name lev kind type)#:transparent)
;obj構造体のtypeの要素になりうる構造体.
(struct type-pointer (pointer type)#:transparent)
(struct array (type size)#:transparent)

;環境操作関数
;myenv.rkt内でも定義
(define (name-env? x e)
  (cond
    [(eq? e env:initial-env) #f]
    [(equal? (obj-name (car e)) x) #t]
    [else (name-env? x (cdr e))]))

(define (extract-env name e)
  (if (eq? e env:initial-env) 
      #f
      (if (equal? (obj-name (car e)) name)
          (car e)
          (extract-env name (cdr e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analy-declaration_st st)
  ;内部定義
  ;関数separate-nameはextract-name-from-declarator_stの返り値
  ;変数のidentifierであるnameがlist*の形であった時にそれを分解して残りの情報を付加して
  ;それぞれのobjを生成する関数.  
   (define (separate-name name lev kind type)
    (cond ((cons? name) (let* ((meaningless 1)) 
                          (separate-name (car name) lev kind type)
                          ;(set! env (env:extend-env (obj (cdr name) lev kind type) env))
                          (separate-name (cdr name) lev kind type)
                          ))
          (else (if (ast_idname? name)
                    (set! env (env:extend-env (obj (ast_idname-name name) lev kind (type-pointer 'pointer type)) env))
                    (set! env (env:extend-env (obj name lev kind type) env))))))
  
  ;ここまで内部定義
  (let* ((name (extract-name-from-declarator_st (stx:declaration_st-declarator-list st)))
         (lev current-lev);大域変数もしくはcompound-statement内のどちらか
         (kind 'var)
         (type (stx:spec_st-type (stx:declaration_st-type-spec st))))
    (separate-name name lev kind type)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ast_idname (name) #:transparent)

(define (extract-name-from-declarator_st st)
  (cond ((struct? st)
         (cond ((stx:declarator_st? st) (stx:id_st-name (stx:declarator_st-var st)))
               ;ポインタ型の定義の際はid_ast_stではなくid_stになってしまっているので
               ;これを後の関数に知らせるために構造体ast_idnameにnameが入った形にする.
               ((stx:declarator_ast_st? st) 
                (ast_idname (stx:id_st-name (stx:declarator_ast_st-var st))))))
        ((cons? st)
         (cons (extract-name-from-declarator_st (car st))
               (extract-name-from-declarator_st (cdr st))))))



;テスト
(define test
  (stx:declaration_st
   (stx:spec_st 'int 'test)
   (cons
    (cons (stx:declarator_st (stx:id_st 'b 'test)) (stx:declarator_st (stx:id_st 'c 'test)))
    (stx:declarator_ast_st (stx:id_st 'd 'test)))))

(analy-declaration_st test)

env