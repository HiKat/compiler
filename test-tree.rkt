#lang racket
;;;;テスト;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test1
  (stx:func_proto_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_ast_st
    'func
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) 
                              (stx:id_st 'c 'test)))
    'test)))
(define test2
  (stx:func_proto_st 
   (stx:spec_st 'int 'test) 
   (stx:func_declarator_ast_null_st 'func 'test)))
(define test3
  (stx:func_def_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_ast_st
    'functioooooon
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) (stx:id_st 'c 'test)))
    'test)
   (stx:compound_sta_st (stx:null_statement_st 'null))))
(define test4
  (stx:compound_st
   ;;;;declaration-list
   ;(cons
    ;(stx:declaration_st (stx:spec_st 'int 'test) (stx:declarator_st (stx:id_st 'a 'test)))
    (stx:declaration_st
     (stx:spec_st 'int 'test)
     (cons
      (cons
       (stx:declarator_ast_st (stx:id_st 'b 'test))
       (stx:declarator_st (stx:id_st 'c 'test)))
      (stx:declarator_st (stx:id_st 'd 'test))))
    ;)
   ;;;;statement-list
   (stx:exp_with_semi_st (stx:func_st 'func2 (stx:id_st 'a 'test)))))
(define test5
  (stx:compound_st
   ;;;;declaration-list
   (stx:declaration_st
    (stx:spec_st 'int 'test)
    (cons
     (cons
      (stx:declarator_ast_st (stx:id_st 'b 'test))
      (stx:declarator_st (stx:id_st 'c 'test)))
     (stx:declarator_st (stx:id_st 'd 'test))))
   ;;;;statement-list
   (cons
    ;(cons
     (stx:func_st 'func2 (stx:id_st 'a 'test))
     ;;
     #;(stx:if_else_st
      (stx:constant_st 1 'test)
      ;;;;compound-statementのネスト
      (stx:compound_st
       (stx:declaration_st (stx:spec_st 'int 'test) 
                           (stx:declarator_st (stx:id_st 'bddddddddd 'test)))
       (stx:assign_exp_st
        (stx:id_st 'b 'test)
        (stx:constant_st 2 'test)
        'test))
      ;;;;
      (stx:null_statement_st 'null)
      'test
      'syntax-sygar)
     ;)
     ;;
    (stx:assign_exp_st
     (stx:id_st 'a 'test)
     (stx:alge_exp_st
      'add
      (stx:constant_st 1 'test)
      (stx:constant_st 2 'test)
      'test)
     'test))))
(define test6
  (stx:compound_st
   (stx:declaration_st
    (stx:spec_st 'int 'test)
     (cons
      (stx:declarator_ast_st (stx:id_st 'b 'test))
      (stx:declarator_st (stx:id_st 'c 'test))))
   (stx:func_st 'func2 (stx:id_st 'a 'test))))
(define test7
  (stx:compound_st
   (cons
    (stx:declaration_st
     (stx:spec_st 'int 'test)
     (cons
      (cons
       (stx:declarator_ast_st (stx:id_st 'b 'test))
       (stx:declarator_st (stx:id_st 'c 'test)))
      (stx:declarator_st (stx:id_st 'd 'test))))
    (stx:declaration_st (stx:spec_st 'int 'test) 
                        (stx:declarator_st (stx:id_st 'a 'test))))
   (stx:func_st 'func2 (stx:id_st 'a 'test))))
(define test8
  (stx:func_proto_st
   (stx:spec_st 'int 'test)
   (stx:func_declarator_ast_st
    'func
    (cons
     (cons
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_st 'a 'test))
      (stx:para_declaration_st (stx:spec_st 'int 'test) 
                               (stx:id_ast_st 'b 'test)))
     (stx:para_declaration_st (stx:spec_st 'int 'test) 
                              (stx:id_st 'c 'test)))
    'test)))
(define test9
  (stx:declaration_st
   (stx:spec_st 'int 'test)
   ;以下がdeclarator-list
   (cons
    (cons
     (stx:declarator_st (stx:id_st 'a 'test))
     (stx:declarator_ast_st (stx:id_st 'b 'test)))
    (stx:declarator_st (stx:id_st 'c 'test)))))
(define test10
    (stx:declaration_st
   (stx:spec_st 'void 'test)
   ;以下がdeclarator-list
     (stx:declarator_ast_st (stx:id_st 'b 'test))))