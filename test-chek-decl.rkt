#lang racket
(require (prefix-in env: "myenv.rkt"))
(require (prefix-in stx: "mysyntax.rkt"))
(require (prefix-in k08: "kadai08.rkt"))

;意味解析を実行中の関数のレベルを入れる変数.
(struct obj (name lev kind type)#:transparent)
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

;変数宣言のチェック
;変数宣言を読み取って作成したobjをenvに照らし合わせてチェックする.
;適切なobjであれば#tを返す.
;入れ子になったcompound-statement内では引数としてenvが
;自分のenvと親のenvが必要になる.
(define (check-var obj env para-env)
  (if (name-env? obj env)
      ;環境にすでに同じ名前が存在するとき.
      (cond
        ;同じ名前が関数としてレベル0で宣言されているとき
        ((and(eq? 0 (obj-lev (extract-env (obj-name obj) env)))
             (or (eq? 'fun (obj-kind (extract-env (obj-name obj) env)))
                 (eq? 'proto (obj-kind (extract-env (obj-name obj) env)))))
         (format "ERROR! redifinition of '~a'."(obj-name obj)))
        ;同じ名前が変数として同じレベルで宣言されている時
        ((and (eq? (obj-lev obj) (obj-lev (extract-env (obj-name obj) env)))
              (eq? 'var (obj-kind (extract-env (obj-name obj) env))))
         (format "ERROR! redifinition of '~a'."(obj-name obj)))
        ;同じ名前がパラメータとして宣言されているかどうかを確認.
        ;引数としてとっているのは最も新しいパラメータ宣言のみが登録された環境.
        (else (if (name-env? obj para-env) 
                  ("WARNING! '~a' is alreay defined as a parameter" (obj-name obj))
                  #t)))
      ;環境にすでに同じ名前が存在しないとき.
      ;para-envにもなければ#tを返す.
      (if (name-env? obj para-env) 
          ("WARNING! '~a' is alreay defined as a parameter" (obj-name obj))
          #t)))
;関数のcompound-statement内から環境を見るとき、自分の
;関数のパラメータかそうでないかを見分ける方法はどうやって実装すればいいか.
;パラメータのみの環境を作成して、新しく関数定義を読み込む際に毎回
;環境を初期化する??
;プロトタイプははじく.
;compound-statementを読み込む際は毎回この環境もcheck-var
;の引数にする?
;この方法で実装するなら引数は2つ必要

;パラメータ宣言のチェック.
;引数に取るのは
(define (check-para obj para-env)
  (if (name-env? obj para-env)
      #f
      #t))

;プロトタイプ宣言のチェック.
(define (check-proto obj env)
  (if (name-env? obj env) 
      (cond
        ;同じ名前が関数としてレベル0で宣言されているとき
        ((and(eq? 0 (obj-lev (extract-env (obj-name obj) env)))
             (or (eq? 'fun (obj-kind (extract-env (obj-name obj) env)))
                 (eq? 'proto (obj-kind (extract-env (obj-name obj) env)))))
         (format "ERROR! redifinition of '~a'."(obj-name obj)))
        ;同じ名前が変数として同じレベルで宣言されている時
        ((and (eq? (obj-lev obj) (obj-lev (extract-env (obj-name obj) env)))
              (eq? 'var (obj-kind (extract-env (obj-name obj) env))))
         (format "ERROR! redifinition of '~a'."(obj-name obj)))
        ;同じ名前がパラメータとして宣言されているとき
        (else ("同じ名前がパラメータとして宣言されているとき.")))
      #t))
      
            

  