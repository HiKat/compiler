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
(struct type-pointer (pointer type) #:transparent);!!!!!!!!!!!!!!!!!!!
(struct array (type size) #:transparent);!!!!!!!!!!!!!!!!!!

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




(define (analy-func_proto_st st)
  ;内部定義
  ;func_proto_st構造体　！！！！！！！
  ;を受け取ってを引数の型のlist*か
  ;引数の型の単体を返す関数か
  ;シンボル'noparameter
  ;を返す関数.
  (define (analy-para_declaration-list st)
    (let*((declarator (stx:func_proto_st-func-declarator-st st))
          (parameter-list 
           ;parameter-listに入るのは'noparameterか
           ;(list* pada_declaration_st ,,,)か
           ;para_declaration_st単体
           ;あとでここから型を抜き出す必要がある.
           ;型を抜き出す際に注意すべきは
           ;para_declaration-paraが
           ;id_stかid_ast_stかを調べる必要がある.
           (cond ((stx:func_declarator_st? declarator) 
                  (stx:func_declarator_st-para-list declarator))
                 
                 ((stx:func_declarator_null_st? declarator) 
                  'noparameter)
                 
                 ((stx:func_declarator_ast_st? declarator) 
                  (stx:func_declarator_ast_st-para-list declarator))
                 
                 ((stx:func_declarator_ast_null_st? declarator) 
                  'noparameter))))
      (cond ((eq? parameter-list 'noparameter) 'noparameter)
            (else (extract-type-from-para parameter-list)))))
  ;para_declaration_stもしくはそのlist*を受け取って
  ;型のlist*を作成する関数の内部定義
  (define (extract-type-from-para parameter-list)
    (cond ((struct? parameter-list) 
           (cond ((stx:id_ast_st? (stx:para_declaration_st-para parameter-list))
                  (type-pointer 'pointer 
                                (stx:spec_st-type (stx:para_declaration_st-type-spec parameter-list))))
                 ((stx:id_st? (stx:para_declaration_st-para parameter-list)) 
                  (stx:spec_st-type (stx:para_declaration_st-type-spec parameter-list)))))
          ((cons? parameter-list) 
           (cons (extract-type-from-para (car parameter-list))
                 (extract-type-from-para (cdr parameter-list))))))
  ;ここまで内部定義
  (let* ((name (cond ((stx:func_declarator_st? 
                       (stx:func_proto_st-func-declarator-st st)) 
                      (stx:func_declarator_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     
                     ((stx:func_declarator_null_st? 
                       (stx:func_proto_st-func-declarator-st st))
                      (stx:func_declarator_null_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     
                     ((stx:func_declarator_ast_st? 
                       (stx:func_proto_st-func-declarator-st st)) 
                      (stx:func_declarator_ast_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     
                     ((stx:func_declarator_ast_null_st? 
                       (stx:func_proto_st-func-declarator-st st)) 
                      (stx:func_declarator_ast_null_st-name 
                       (stx:func_proto_st-func-declarator-st st)))
                     ))
         (lev 0)
         (kind 'proto)
         ;func_typeは関数の戻り値の型
         (func-type
          (cons 'fun 
                (if (stx:func_declarator_ast_st? (stx:func_proto_st-func-declarator-st st))
                    (type-pointer 'poiner (stx:spec_st-type (stx:func_proto_st-type-spec st)))
                    (stx:spec_st-type (stx:func_proto_st-type-spec st)))))
         (func-inputtype (analy-para_declaration-list st));!!!!!!!!!!!
         (type (cons func-type func-inputtype)))
    (set! current-lev 0)
    (set! env (env:extend-env (obj name lev kind type) env))
    st))


;テスト
(define test1
  (stx:func_proto_st
   (stx:spec_st 'int 'print-proto)
   (stx:func_declarator_st
    'print
    (stx:para_declaration_st (stx:spec_st 'int 'print-proto) (stx:id_st 'i 'print-proto))
    'print-proto)))

(define test2 
  (stx:func_proto_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_st
    'func
    (cons 
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'c 'test)))
    (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'd 'test)))
    'test)))


(analy-func_proto_st test2)
env

