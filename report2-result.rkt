#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が課題8の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.
(list*
 (func_proto_st
  (spec_st 'void 'print-proto)
  (func_declarator_st
   'print
   (para_declaration_st (spec_st 'int 'print-proto) (id_st 'v 'print-proto))
   'print-proto))
 (cons
  (cons
   (func_proto_st
    (spec_st 'int (position 1 1 0))
    (func_declarator_st
     'comp_num
     (cons
      (para_declaration_st (spec_st 'int (position 14 1 13)) (id_st 'a (position 18 1 17)))
      (para_declaration_st (spec_st 'int (position 21 1 20)) (id_st 'b (position 25 1 24))))
     (position 5 1 4)))
   (declaration_st
    (spec_st 'int (position 29 2 0))
    (declarator_st (array_st 'sort_array 8 (position 33 2 4)))))
  (func_def_st
   (spec_st 'int (position 50 5 0))
   (func_declarator_st
    'comp_num
    (cons
     (para_declaration_st (spec_st 'int (position 63 5 13)) (id_st 'a (position 67 5 17)))
     (para_declaration_st (spec_st 'int (position 70 5 20)) (id_st 'b (position 74 5 24))))
    (position 54 5 4))
   (compound_sta_st
    (cons
     (cons
      (if_else_st
       (rel_exp_st 'more (id_st 'a (position 85 6 7)) (id_st 'b (position 89 6 11)) (position 87 6 9))
       (return_st (constant_st 0 (position 99 6 21)) (position 92 6 14))
       (null_statement_st 'null)
       (position 82 6 4)
       'syntax-sygar)
      (if_else_st
       (rel_exp_st
        'less
        (id_st 'a (position 109 7 7))
        (id_st 'b (position 113 7 11))
        (position 111 7 9))
       (return_st (constant_st 1 (position 123 7 21)) (position 116 7 14))
       (null_statement_st 'null)
       (position 106 7 4)
       'syntax-sygar))
     (if_else_st
      (rel_exp_st
       'equal
       (id_st 'a (position 133 8 7))
       (id_st 'b (position 138 8 12))
       (position 135 8 9))
      (return_st (constant_st 2 (position 148 8 22)) (position 141 8 15))
      (null_statement_st 'null)
      (position 130 8 4)
      'syntax-sygar)))))
 (func_def_st
  (spec_st 'int (position 155 12 0))
  (func_declarator_null_st 'main (position 159 12 4))
  (compound_st
   (cons
    (cons
     (declaration_st
      (spec_st 'int (position 171 13 4))
      (declarator_st (id_st 'i (position 175 13 8))))
     (declaration_st
      (spec_st 'int (position 182 14 4))
      (declarator_st (id_st 'j (position 186 14 8)))))
    (declaration_st
     (spec_st 'int (position 193 15 4))
     (declarator_st (id_st 'h (position 197 15 8)))))
   (cons
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons
          (cons
           (cons
            (cons
             (assign_exp_st
              (unary_exp_st
               'ast
               (exp_in_paren_st
                (alge_exp_st
                 'add
                 (id_st 'sort_array (position 205 17 4))
                 (constant_st 0 (position 216 17 15))
                 'syntax-sugar))
               'syntax-sugar)
              (constant_st 6 (position 221 17 20))
              (position 219 17 18))
             (assign_exp_st
              (unary_exp_st
               'ast
               (exp_in_paren_st
                (alge_exp_st
                 'add
                 (id_st 'sort_array (position 228 18 4))
                 (constant_st 1 (position 239 18 15))
                 'syntax-sugar))
               'syntax-sugar)
              (constant_st 4 (position 244 18 20))
              (position 242 18 18)))
            (assign_exp_st
             (unary_exp_st
              'ast
              (exp_in_paren_st
               (alge_exp_st
                'add
                (id_st 'sort_array (position 251 19 4))
                (constant_st 2 (position 262 19 15))
                'syntax-sugar))
              'syntax-sugar)
             (constant_st 2 (position 267 19 20))
             (position 265 19 18)))
           (assign_exp_st
            (unary_exp_st
             'ast
             (exp_in_paren_st
              (alge_exp_st
               'add
               (id_st 'sort_array (position 274 20 4))
               (constant_st 3 (position 285 20 15))
               'syntax-sugar))
             'syntax-sugar)
            (constant_st 5 (position 290 20 20))
            (position 288 20 18)))
          (assign_exp_st
           (unary_exp_st
            'ast
            (exp_in_paren_st
             (alge_exp_st
              'add
              (id_st 'sort_array (position 297 21 4))
              (constant_st 4 (position 308 21 15))
              'syntax-sugar))
            'syntax-sugar)
           (constant_st 7 (position 313 21 20))
           (position 311 21 18)))
         (assign_exp_st
          (unary_exp_st
           'ast
           (exp_in_paren_st
            (alge_exp_st
             'add
             (id_st 'sort_array (position 320 22 4))
             (constant_st 5 (position 331 22 15))
             'syntax-sugar))
           'syntax-sugar)
          (constant_st 8 (position 336 22 20))
          (position 334 22 18)))
        (assign_exp_st
         (unary_exp_st
          'ast
          (exp_in_paren_st
           (alge_exp_st
            'add
            (id_st 'sort_array (position 343 23 4))
            (constant_st 6 (position 354 23 15))
            'syntax-sugar))
          'syntax-sugar)
         (constant_st 1 (position 359 23 20))
         (position 357 23 18)))
       (assign_exp_st
        (unary_exp_st
         'ast
         (exp_in_paren_st
          (alge_exp_st
           'add
           (id_st 'sort_array (position 366 24 4))
           (constant_st 7 (position 377 24 15))
           'syntax-sugar))
         'syntax-sugar)
        (constant_st 3 (position 382 24 20))
        (position 380 24 18)))
      (compound_sta_st
       (cons
        (assign_exp_st
         (id_st 'j (position 399 27 8))
         (constant_st 8 (position 403 27 12))
         (position 401 27 10))
        (while_st
         (rel_exp_st
          'more
          (id_st 'j (position 406 27 15))
          (constant_st 1 (position 410 27 19))
          (position 408 27 17))
         (compound_sta_st
          (cons
           (compound_sta_st
            (compound_sta_st
             (cons
              (assign_exp_st
               (id_st 'i (position 437 28 12))
               (constant_st 0 (position 441 28 16))
               (position 439 28 14))
              (while_st
               (rel_exp_st
                'less
                (id_st 'i (position 444 28 19))
                (exp_in_paren_st
                 (alge_exp_st
                  'sub
                  (id_st 'j (position 449 28 24))
                  (constant_st 1 (position 451 28 26))
                  (position 450 28 25)))
                (position 446 28 21))
               (compound_sta_st
                (cons
                 (compound_sta_st
                  (if_else_st
                   (func_st
                    'comp_num
                    (cons
                     (unary_exp_st
                      'ast
                      (exp_in_paren_st
                       (alge_exp_st
                        'add
                        (id_st 'sort_array (position 491 29 24))
                        (alge_exp_st
                         'add
                         (id_st 'i (position 502 29 35))
                         (constant_st 1 (position 504 29 37))
                         (position 503 29 36))
                        'syntax-sugar))
                      'syntax-sugar)
                     (unary_exp_st
                      'ast
                      (exp_in_paren_st
                       (alge_exp_st
                        'add
                        (id_st 'sort_array (position 507 29 40))
                        (id_st 'i (position 518 29 51))
                        'syntax-sugar))
                      'syntax-sugar)))
                   (compound_sta_st
                    (cons
                     (cons
                      (assign_exp_st
                       (id_st 'h (position 540 30 16))
                       (unary_exp_st
                        'ast
                        (exp_in_paren_st
                         (alge_exp_st
                          'add
                          (id_st 'sort_array (position 544 30 20))
                          (alge_exp_st
                           'add
                           (id_st 'i (position 555 30 31))
                           (constant_st 1 (position 557 30 33))
                           (position 556 30 32))
                          'syntax-sugar))
                        'syntax-sugar)
                       (position 542 30 18))
                      (assign_exp_st
                       (unary_exp_st
                        'ast
                        (exp_in_paren_st
                         (alge_exp_st
                          'add
                          (id_st 'sort_array (position 577 31 16))
                          (alge_exp_st
                           'add
                           (id_st 'i (position 588 31 27))
                           (constant_st 1 (position 590 31 29))
                           (position 589 31 28))
                          'syntax-sugar))
                        'syntax-sugar)
                       (unary_exp_st
                        'ast
                        (exp_in_paren_st
                         (alge_exp_st
                          'add
                          (id_st 'sort_array (position 595 31 34))
                          (id_st 'i (position 606 31 45))
                          'syntax-sugar))
                        'syntax-sugar)
                       (position 593 31 32)))
                     (assign_exp_st
                      (unary_exp_st
                       'ast
                       (exp_in_paren_st
                        (alge_exp_st
                         'add
                         (id_st 'sort_array (position 626 32 16))
                         (id_st 'i (position 637 32 27))
                         'syntax-sugar))
                       'syntax-sugar)
                      (id_st 'h (position 642 32 32))
                      (position 640 32 30))))
                   (null_statement_st 'null)
                   (position 479 29 12)
                   'syntax-sygar))
                 (assign_exp_st
                  (id_st 'i (position 455 28 30))
                  (exp_in_paren_st
                   (alge_exp_st
                    'add
                    (id_st 'i (position 460 28 35))
                    (constant_st 1 (position 462 28 37))
                    (position 461 28 36)))
                  (position 457 28 32))))
               'syntax-sugar))))
           (assign_exp_st
            (id_st 'j (position 413 27 22))
            (exp_in_paren_st
             (alge_exp_st
              'sub
              (id_st 'j (position 418 27 27))
              (constant_st 1 (position 420 27 29))
              (position 419 27 28)))
            (position 415 27 24))))
         'syntax-sugar))))
     (assign_exp_st
      (id_st 'i (position 692 37 4))
      (constant_st 0 (position 696 37 8))
      (position 694 37 6)))
    (while_st
     (rel_exp_st
      'more
      (constant_st 8 (position 709 38 10))
      (id_st 'i (position 713 38 14))
      (position 711 38 12))
     (compound_sta_st
      (cons
       (if_else_st
        (rel_exp_st
         'not
         (id_st 'i (position 728 39 11))
         (constant_st 7 (position 733 39 16))
         (position 730 39 13))
        (compound_sta_st
         (func_st
          'print
          (unary_exp_st
           'ast
           (exp_in_paren_st
            (alge_exp_st
             'add
             (id_st 'sort_array (position 751 40 14))
             (id_st 'i (position 762 40 25))
             'syntax-sugar))
           'syntax-sugar)))
        (exp_in_paren_st
         (func_st
          'print
          (unary_exp_st
           'ast
           (exp_in_paren_st
            (alge_exp_st
             'add
             (id_st 'sort_array (position 796 42 19))
             (constant_st 7 (position 807 42 30))
             'syntax-sugar))
           'syntax-sugar)))
        (position 725 39 8)
        (position 785 42 8))
       (assign_exp_st
        (id_st 'i (position 821 43 8))
        (exp_in_paren_st
         (alge_exp_st
          'add
          (id_st 'i (position 826 43 13))
          (constant_st 1 (position 828 43 15))
          (position 827 43 14)))
        (position 823 43 10))))
     (position 703 38 4))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が意味解析の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.
OK! CORRENCT DECLARATIONS IN COMPONUND STATEMENT!
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
(list
 (func_proto_st
  (spec_st 'void 'print-proto)
  (func_declarator_st
   (obj 'print 0 'proto (type_fun 'fun 'void '(int)) 'print-proto)
   (list (obj 'v 1 'parm 'int 'print-proto))
   'print-proto))
 (func_proto_st
  (spec_st 'int (position 1 1 0))
  (func_declarator_st
   (obj 'comp_num 0 'proto (type_fun 'fun 'int '(int int)) (position 5 1 4))
   (list (obj 'a 1 'parm 'int (position 18 1 17)) (obj 'b 1 'parm 'int (position 25 1 24)))
   (position 5 1 4)))
 (declaration_st
  (spec_st 'int (position 29 2 0))
  (list (obj 'sort_array 0 'var (type_array 'int 8) (position 33 2 4))))
 (func_def_st
  (spec_st 'int (position 50 5 0))
  (func_declarator_st
   (obj 'comp_num 0 'fun (type_fun 'fun 'int '(int int)) (position 54 5 4))
   (list (obj 'a 1 'parm 'int (position 67 5 17)) (obj 'b 1 'parm 'int (position 74 5 24)))
   (position 54 5 4))
  (compound_st
   'nodecl
   (list
    (if_else_st
     (rel_exp_st
      'more
      (obj 'a 1 'parm 'int (position 67 5 17))
      (obj 'b 1 'parm 'int (position 74 5 24))
      (position 87 6 9))
     (sem_return_st
      (constant_st 0 (position 99 6 21))
      (position 92 6 14)
      (obj 'comp_num 0 'fun (type_fun 'fun 'int '(int int)) (position 54 5 4)))
     (null_statement_st 'null)
     (position 82 6 4)
     'syntax-sygar)
    (if_else_st
     (rel_exp_st
      'less
      (obj 'a 1 'parm 'int (position 67 5 17))
      (obj 'b 1 'parm 'int (position 74 5 24))
      (position 111 7 9))
     (sem_return_st
      (constant_st 1 (position 123 7 21))
      (position 116 7 14)
      (obj 'comp_num 0 'fun (type_fun 'fun 'int '(int int)) (position 54 5 4)))
     (null_statement_st 'null)
     (position 106 7 4)
     'syntax-sygar)
    (if_else_st
     (rel_exp_st
      'equal
      (obj 'a 1 'parm 'int (position 67 5 17))
      (obj 'b 1 'parm 'int (position 74 5 24))
      (position 135 8 9))
     (sem_return_st
      (constant_st 2 (position 148 8 22))
      (position 141 8 15)
      (obj 'comp_num 0 'fun (type_fun 'fun 'int '(int int)) (position 54 5 4)))
     (null_statement_st 'null)
     (position 130 8 4)
     'syntax-sygar))))
 (func_def_st
  (spec_st 'int (position 155 12 0))
  (func_declarator_st
   (obj 'main 0 'fun (type_fun 'fun 'int 'nopara) (position 159 12 4))
   'nopara
   (position 159 12 4))
  (compound_st
   (list
    (declaration_st
     (spec_st 'int (position 171 13 4))
     (list (obj 'i 2 'var 'int (position 175 13 8))))
    (declaration_st
     (spec_st 'int (position 182 14 4))
     (list (obj 'j 2 'var 'int (position 186 14 8))))
    (declaration_st
     (spec_st 'int (position 193 15 4))
     (list (obj 'h 2 'var 'int (position 197 15 8)))))
   (list
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 0 (position 216 17 15))) (position 33 2 4))
     (constant_st 6 (position 221 17 20))
     (position 219 17 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 1 (position 239 18 15))) (position 33 2 4))
     (constant_st 4 (position 244 18 20))
     (position 242 18 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 2 (position 262 19 15))) (position 33 2 4))
     (constant_st 2 (position 267 19 20))
     (position 265 19 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 3 (position 285 20 15))) (position 33 2 4))
     (constant_st 5 (position 290 20 20))
     (position 288 20 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 4 (position 308 21 15))) (position 33 2 4))
     (constant_st 7 (position 313 21 20))
     (position 311 21 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 5 (position 331 22 15))) (position 33 2 4))
     (constant_st 8 (position 336 22 20))
     (position 334 22 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 6 (position 354 23 15))) (position 33 2 4))
     (constant_st 1 (position 359 23 20))
     (position 357 23 18))
    (assign_exp_st
     (obj 'sort_array 0 'var (type_array 'int (constant_st 7 (position 377 24 15))) (position 33 2 4))
     (constant_st 3 (position 382 24 20))
     (position 380 24 18))
    (compound_st
     'nodecl
     (list
      (assign_exp_st
       (obj 'j 2 'var 'int (position 186 14 8))
       (constant_st 8 (position 403 27 12))
       (position 401 27 10))
      (while_st
       (rel_exp_st
        'more
        (obj 'j 2 'var 'int (position 186 14 8))
        (constant_st 1 (position 410 27 19))
        (position 408 27 17))
       (compound_st
        'nodecl
        (list
         (compound_st
          'nodecl
          (list
           (compound_st
            'nodecl
            (list
             (assign_exp_st
              (obj 'i 2 'var 'int (position 175 13 8))
              (constant_st 0 (position 441 28 16))
              (position 439 28 14))
             (while_st
              (rel_exp_st
               'less
               (obj 'i 2 'var 'int (position 175 13 8))
               (exp_in_paren_st
                (alge_exp_st
                 'sub
                 (obj 'j 2 'var 'int (position 186 14 8))
                 (constant_st 1 (position 451 28 26))
                 (position 450 28 25)))
               (position 446 28 21))
              (compound_st
               'nodecl
               (list
                (compound_st
                 'nodecl
                 (list
                  (if_else_st
                   (func_st
                    (obj 'comp_num 0 'fun (type_fun 'fun 'int '(int int)) (position 54 5 4))
                    (list
                     (obj
                      'sort_array
                      0
                      'var
                      (type_array
                       'int
                       (alge_exp_st
                        'add
                        (id_st 'i (position 502 29 35))
                        (constant_st 1 (position 504 29 37))
                        (position 503 29 36)))
                      (position 33 2 4))
                     (obj
                      'sort_array
                      0
                      'var
                      (type_array 'int (id_st 'i (position 518 29 51)))
                      (position 33 2 4))))
                   (compound_st
                    'nodecl
                    (list
                     (assign_exp_st
                      (obj 'h 2 'var 'int (position 197 15 8))
                      (obj
                       'sort_array
                       0
                       'var
                       (type_array
                        'int
                        (alge_exp_st
                         'add
                         (id_st 'i (position 555 30 31))
                         (constant_st 1 (position 557 30 33))
                         (position 556 30 32)))
                       (position 33 2 4))
                      (position 542 30 18))
                     (assign_exp_st
                      (obj
                       'sort_array
                       0
                       'var
                       (type_array
                        'int
                        (alge_exp_st
                         'add
                         (id_st 'i (position 588 31 27))
                         (constant_st 1 (position 590 31 29))
                         (position 589 31 28)))
                       (position 33 2 4))
                      (obj
                       'sort_array
                       0
                       'var
                       (type_array 'int (id_st 'i (position 606 31 45)))
                       (position 33 2 4))
                      (position 593 31 32))
                     (assign_exp_st
                      (obj
                       'sort_array
                       0
                       'var
                       (type_array 'int (id_st 'i (position 637 32 27)))
                       (position 33 2 4))
                      (obj 'h 2 'var 'int (position 197 15 8))
                      (position 640 32 30))))
                   (null_statement_st 'null)
                   (position 479 29 12)
                   'syntax-sygar)))
                (assign_exp_st
                 (obj 'i 2 'var 'int (position 175 13 8))
                 (exp_in_paren_st
                  (alge_exp_st
                   'add
                   (obj 'i 2 'var 'int (position 175 13 8))
                   (constant_st 1 (position 462 28 37))
                   (position 461 28 36)))
                 (position 457 28 32))))
              'syntax-sugar)))))
         (assign_exp_st
          (obj 'j 2 'var 'int (position 186 14 8))
          (exp_in_paren_st
           (alge_exp_st
            'sub
            (obj 'j 2 'var 'int (position 186 14 8))
            (constant_st 1 (position 420 27 29))
            (position 419 27 28)))
          (position 415 27 24))))
       'syntax-sugar)))
    (assign_exp_st
     (obj 'i 2 'var 'int (position 175 13 8))
     (constant_st 0 (position 696 37 8))
     (position 694 37 6))
    (while_st
     (rel_exp_st
      'more
      (constant_st 8 (position 709 38 10))
      (obj 'i 2 'var 'int (position 175 13 8))
      (position 711 38 12))
     (compound_st
      'nodecl
      (list
       (if_else_st
        (rel_exp_st
         'not
         (obj 'i 2 'var 'int (position 175 13 8))
         (constant_st 7 (position 733 39 16))
         (position 730 39 13))
        (compound_st
         'nodecl
         (list
          (func_st
           (obj 'print 0 'proto (type_fun 'fun 'void '(int)) 'print-proto)
           (list
            (obj
             'sort_array
             0
             'var
             (type_array 'int (id_st 'i (position 762 40 25)))
             (position 33 2 4))))))
        (exp_in_paren_st
         (func_st
          (obj 'print 0 'proto (type_fun 'fun 'void '(int)) 'print-proto)
          (list
           (obj
            'sort_array
            0
            'var
            (type_array 'int (constant_st 7 (position 807 42 30)))
            (position 33 2 4)))))
        (position 725 39 8)
        (position 785 42 8))
       (assign_exp_st
        (obj 'i 2 'var 'int (position 175 13 8))
        (exp_in_paren_st
         (alge_exp_st
          'add
          (obj 'i 2 'var 'int (position 175 13 8))
          (constant_st 1 (position 828 43 15))
          (position 827 43 14)))
        (position 823 43 10))))
     (position 703 38 4))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が形検査の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.
OK! CORRENCT DECLARATIONS IN COMPONUND STATEMENT!
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! THIS PROGRAM IS IN A CORRECT FORM.

;;;;;;;;;;;;;;;;;;;;;;;;;;;以下が型検査の実行結果です;;;;;;;;;;;;;;;;;;;;;;;;.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT FUNCTION PROTOTYPE OF 'comp_num'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT FUNCTION PROTOTYPE OF 'main'.
OK! CRRECT PARAMETERS OF FUNCTION PROTOTYPE.
OK! CORRENCT DECLARATIONS IN COMPONUND STATEMENT!
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! NO DECLARATIONS IN COMPONUND STATEMENT
OK! THIS PROGRAM IS WELL TYPED.
> 