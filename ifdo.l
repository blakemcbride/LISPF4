(FILEHEADER IFDO)
(PRINT '(IF AND DO FUNCTIONS WRITTEN BY BLAKE MCBRIDE))
(PRINT '(VERSION 5))
(DEFINEQ
(COMP2
  [LAMBDA (F F2 P Q)
          (COND ((ATOM P) P)
                [(MATCH (LIST F '(? (0 T NIL Q T))) (CAR P))
                  (CONS (EVAL (LIST F2 (LIST 'QUOTE Q)))
                        (COMP2 F F2 (CDR P]
                ((CONS (COMP2 F F2 (CAR P)) (COMP2 F F2 (CDR P])
  
(COMPILE
  [LAMBDA (A)
          (MAPC A
                '(LAMBDA (A)
                         (PUTD A
                               (COMP2 'DO 'DO-C
                                 (COMP2 'IF 'IF-C
                                   (COMP2 'ITP 'ITP-C (GETD A])
  
(COMPILEN
  (NLAMBDA A (COMPILE A)))
  
(DO
  (NLAMBDA DO-ARG- (EVAL (DO-C DO-ARG-))))
  
(DO-C
  [LAMBDA (ARG)
          (PROG (EXPR (B 1)
                      (E 1)
                      (I 1)
                      RETE
                      (RETV (GENSYM))
                      LVARS AFTER BODY DIR)
                (SELECTQ
                  (CAR ARG)
                  ((WHILE UNTIL)
                    [SETQ EXPR
                          (IF (EQ 'WHILE (CAR ARG))
                              THEN
                              (CADR ARG)
                              ELSE
                              (LIST 'NULL (CADR ARG]
                    (SETQ BODY
                          (PROG NIL
                                (MAP (CDDR ARG)
                                     '(LAMBDA
                                        (ARG)
                                        (SET (SELECTQ
                                               (CAR ARG)
                                               ((RETURN RET) 'RETE)
                                               ((RETURNV RETV) 'RETV)
                                               (LVARS 'LVARS)
                                               (AFTER 'AFTER)
                                               (DO (RETURN (CDR ARG)))
                                               (RETURN ARG))
                                             (CADR ARG)))
                                     'CDDR)))
                    (RETURN (DOWHILE EXPR RETE RETV LVARS AFTER BODY)))
                  (FOR (SETQ BODY
                             (PROG NIL
                                   (MAP (CDR ARG)
                                        '(LAMBDA
                                           (ARG)
                                           (SET (SELECTQ
                                                  (CAR ARG)
                                                  ((COUNTV VAR) 'EXPR)
                                                  ((BEGINNING BEG) 'B)
                                                  ((ENDING END) 'E)
                                                  ((INCREMENT INC) 'I)
                                                  ((RETURN RET) 'RETE)
                                                  ((RETURNV RETV)
                                                    'RETV)
                                                  ((DIRECTION DIR)
                                                    'DIR)
                                                  (LVARS 'LVARS)
                                                  (AFTER 'AFTER)
                                                  (DO (RETURN
                                                        (CDR ARG)))
                                                  (RETURN ARG))
                                                (CADR ARG)))
                                        'CDDR)))
                       (OR EXPR (SETQ EXPR (GENSYM)))
                       (SETQ LVARS (CONS (LIST EXPR B) LVARS))
                       [SETQ AFTER
                             (COND ((NLISTP AFTER)
                                     (LIST 'SETQ EXPR
                                           (LIST
                                             (COND
                                               ((EQ 'DOWN DIR)
                                                 'DIFFERENCE)
                                               ('PLUS))
                                             EXPR I)))
                                   [(EQ 'PROGN (CAR AFTER))
                                     (APPEND AFTER
                                       (CONS (LIST 'SETQ EXPR
                                               (LIST
                                                 (COND
                                                   ((EQ 'DOWN DIR)
                                                     'DIFFERENCE)
                                                   ('PLUS))
                                                 EXPR I]
                                   ((LIST 'PROGN AFTER
                                          (LIST 'SETQ EXPR
                                            (LIST
                                              (COND
                                                ((EQ 'DOWN DIR)
                                                  'DIFFERENCE)
                                                ('PLUS))
                                              EXPR I]
                       (SETQ EXPR
                             (LIST (COND ((EQ 'DOWN DIR) 'GEQ)
                                         ('LEQ))
                                   EXPR E))
                       (RETURN
                         (DOWHILE EXPR RETE RETV LVARS AFTER BODY)))
                  (RETURN (CONS 'PROGN ARG])
  
(DOWHILE
  [LAMBDA (EXPR RET VAR LVARS AFTER LIST)
          (SELECTQ RET
                   (LAST (LIST 'PROG
                               (CONS VAR LVARS)
                               'LOOP
                               (LIST 'COND
                                     (LIST EXPR
                                           (LIST 'SETQ VAR
                                             (CONS 'PROGN LIST))
                                           AFTER
                                           '(GO LOOP))
                                     (LIST (LIST 'RETURN VAR)))
                               'AFTER AFTER
                               '(GO LOOP)
                               'END
                               (LIST 'RETURN VAR)))
                   (LIST (LIST 'PROG
                               (CONS VAR LVARS)
                               'LOOP
                               (LIST 'COND
                                     (LIST EXPR
                                           (LIST 'SETQ VAR
                                             (LIST 'CONS
                                               (CONS 'PROGN LIST)
                                               VAR))
                                           AFTER
                                           '(GO LOOP))
                                     (LIST (LIST 'RETURN VAR)))
                               'AFTER AFTER
                               '(GO LOOP)
                               'END
                               (LIST 'RETURN VAR)))
                   (LIST 'PROG LVARS 'LOOP
                         (LIST 'COND
                               (APPEND
                                 (CONS EXPR LIST)
                                 (LIST AFTER '(GO LOOP)))
                               (LIST (LIST 'RETURN RET)))
                         'AFTER AFTER
                         '(GO LOOP)
                         'END
                         (LIST 'RETURN RET])
  
(IF
  (NLAMBDA IF-ARGS- (EVAL (IF-C IF-ARGS-))))
  
(IF-C
  [LAMBDA (ARG)
          (PROG ((LEN (LENGTH ARG))
                  (IKL '(THEN ELSEIF ELSE THENRET))
                  (STATE 'INIT)
                  TOTALCOL COL)
                (COND [(AND (EQ LEN 2)
                            (NULL (MEMB (CADR ARG) IKL)))
                        (RETURN
                          (LIST 'COND (LIST (CAR ARG) (CADR ARG]
                      [[EVERY ARG
                              '(LAMBDA (XX) (NULL (MEMB XX IKL]
                        (RETURN
                          (LIST 'COND
                                (LIST (CAR ARG) (CADR ARG))
                                (CONS T (CDDR ARG]
                      (T (MAPC (REVERSE ARG)
                               '(LAMBDA
                                  (XX)
                                  (SELECTQ STATE
                                    [INIT (COND
                                            ((MEMB XX IKL)
                                              (SETQ COL NIL)
                                              (SETQ STATE 'THEN))
                                            (T (SETQ STATE 'COL)
                                               (SETQ COL (CONS XX NIL]
                                    [COL (COND
                                           ((MEMB XX IKL)
                                             (SELECTQ XX
                                               (ELSE
                                                 (SETQ STATE 'INIT)
                                                 (SETQ TOTALCOL
                                                   (CONS
                                                     (CONS T COL)
                                                     TOTALCOL)))
                                               (THEN
                                                 (SETQ STATE 'THEN))
                                               NIL))
                                           (T (SETQ COL (CONS XX COL]
                                    (THEN (SETQ STATE 'COMPL)
                                          (SETQ TOTALCOL
                                            (CONS
                                              (CONS XX COL)
                                              TOTALCOL)))
                                    (COMPL (SETQ STATE 'INIT))
                                    NIL)))
                         (RETURN (CONS 'COND TOTALCOL])
  
(ITP
  (NLAMBDA ITP-ARG- (EVAL (ITP-C ITP-ARG-))))
  
(ITP-C
  [LAMBDA (ARG)
          (PROG (L R)
                (RETURN (COND ((ATOM ARG) ARG)
                              ((MATCH '(*) ARG)
                                (ITP-C (CAR ARG)))
                              ((MATCH
                                 '((?? (1 1 NIL L NIL OR)
                                       (LITATOM))
                                    =
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'SETQ (CAR L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    |
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'OR (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    &
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'AND (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    [
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'LESSP (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    ]
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'GREATERP (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    [=
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'LEQ (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    ]=
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'GEQ (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    +
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'PLUS (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    -
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'DIFFERENCE
                                      (ITP-C L)
                                      (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    (?? (1 1 NIL NIL NIL OR)
                                        (ITP-S))
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'TIMES (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    /
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'QUOTIENT (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    \
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'REMAINDER (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 1 NIL L T) (- ^))
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'TIMES (ITP-C L) (ITP-C R)))
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    ^
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'EXPT (ITP-C L) (ITP-C R)))
                              ((MATCH '(- (? (1 T NIL R T))) ARG)
                                (LIST 'MINUS (ITP-C R)))
                              (ARG])
  
(ITP-S
  (LAMBDA (A) (EQ '* A)))
  
)
(PRINT 'IFDOFNS)
(RPAQQ IFDOFNS
       (COMP2 COMPILE COMPILEN DO DO-C DOWHILE IF IF-C ITP ITP-C ITP-S))
(RPAQQ IFDOCOMS (IF AND DO FUNCTIONS WRITTEN BY BLAKE MCBRIDE))
(RPAQ IFDOGENNR 5)
STOP
