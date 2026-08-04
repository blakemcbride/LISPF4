(FILEHEADER PROLOG2)
(PRINT '(PROLOG2 - A SMALL PROLOG FOR LISPF4))
(PRINT '(VERSION 1))

(DEFINEQ

(PVARP
  [LAMBDA (X)
          (COND ((NLISTP X) (AND X (LITATOM X) (EQ (NTHCHAR X 1) '?)))
                (T (EQ (CAR X) '?V])

(PVARS
  [LAMBDA (X L)
          (COND ((PVARP X) (COND ((MEMB X L) L) (T (CONS X L))))
                ((NLISTP X) L)
                (T (PVARS (CDR X) (PVARS (CAR X) L])

(PDEREF
  [LAMBDA (X B)
          (PROG (E)
            LP  (COND ((NULL (PVARP X)) (RETURN X)))
                (SETQ E (ASSOC X B))
                (COND ((NULL E) (RETURN X)))
                (SETQ X (CDR E))
                (GO LP])

(PUNIFY
  [LAMBDA (X Y B)
          (PROG (NB)
                (COND ((EQ B 'PFAIL) (RETURN 'PFAIL)))
                (SETQ X (PDEREF X B))
                (SETQ Y (PDEREF Y B))
                (COND ((EQ X Y) (RETURN B))
                      ((PVARP X) (RETURN (CONS (CONS X Y) B)))
                      ((PVARP Y) (RETURN (CONS (CONS Y X) B)))
                      ((OR (NLISTP X) (NLISTP Y))
                        (RETURN (COND ((EQUAL X Y) B) (T 'PFAIL)))))
                (SETQ NB (PUNIFY (CAR X) (CAR Y) B))
                (COND ((EQ NB 'PFAIL) (RETURN 'PFAIL)))
                (RETURN (PUNIFY (CDR X) (CDR Y) NB])

(PRENAME
  [LAMBDA (X TAG)
          (PROG ((*PMAP* NIL))
                (RETURN (PRENAME1 X TAG])

(PRENAME1
  [LAMBDA (X TAG)
          (PROG (E)
                (COND ((EQ X '!) (RETURN (LIST 'CUT TAG)))
                      ((PVARP X)
                        (SETQ E (ASSOC X *PMAP*))
                        (COND (E (RETURN (CDR E))))
                        (SETQ E (CONS '?V X))
                        (SETQ *PMAP* (CONS (CONS X E) *PMAP*))
                        (RETURN E))
                      ((NLISTP X) (RETURN X)))
                (RETURN (CONS (PRENAME1 (CAR X) TAG) (PRENAME1 (CDR X) TAG])

(PINST
  [LAMBDA (X B)
          (SETQ X (PDEREF X B))
          (COND ((PVARP X) (COND ((NLISTP X) X) (T (CDR X))))
                ((NLISTP X) X)
                (T (CONS (PINST (CAR X) B) (PINST (CDR X) B])

(PPRED
  [LAMBDA (G)
          (COND ((NLISTP G) G) (T (CAR G])

(PSOLVE
  [LAMBDA (GOALS B D)
          (COND ((EQ B 'PFAIL) NIL)
                ((AND *PMAX* (NULL (LESSP *PCOUNT* *PMAX*))) NIL)
                ((NULL GOALS) (SETQ *PCOUNT* (ADD1 *PCOUNT*)) (LIST B))
                ((AND D (LESSP D 1)) (SETQ *PDEEP* T) NIL)
                (T (PSOLVE1 (CAR GOALS) (CDR GOALS) B D])

(PSOLVE1
  [LAMBDA (G REST B D)
          (PROG ((P (PPRED G)) CL ACC C NB MYTAG)
                (COND ((MEMB P *PBUILTINS*) (RETURN (PBUILTIN P G REST B D))))
                (SETQ CL (GETPROP P 'PCLAUSES))
                (SETQ MYTAG (SETQ *PGEN* (ADD1 *PGEN*)))
            LP  (COND ((NULL CL) (RETURN ACC))
                      ((AND *PMAX* (NULL (LESSP *PCOUNT* *PMAX*))) (RETURN ACC)))
                (SETQ C (PRENAME (CAR CL) MYTAG))
                (SETQ NB (PUNIFY G (CAR C) B))
                (COND ((NEQ NB 'PFAIL)
                        (SETQ ACC (APPEND ACC (PSOLVE (APPEND (CDR C) REST)
                                                      NB
                                                      (AND D (SUB1 D)))))))
                (COND (*PCUT* (AND (EQ *PCUT* MYTAG) (SETQ *PCUT* NIL))
                              (RETURN ACC)))
                (SETQ CL (CDR CL))
                (GO LP])

(PBUILTIN
  [LAMBDA (P G REST B D)
          (PROG (R S F)
                (COND ((EQ P 'TRUE) (RETURN (PSOLVE REST B D)))
                      ((EQ P 'FAIL) (RETURN NIL))
                      ((EQ P 'CUT) (SETQ R (PSOLVE REST B D))
                                   (SETQ *PCUT* (CADR G))
                                   (RETURN R))
                      ((EQ P '=)
                        (RETURN (PSOLVE REST (PUNIFY (CADR G) (CADDR G) B) D)))
                      ((EQ P 'NOT) (SETQ S *PCOUNT*)
                                   (SETQ R (PSOLVE (CDR G) B (AND D (SUB1 D))))
                                   (SETQ *PCOUNT* S)
                                   (SETQ *PCUT* NIL)
                                   (RETURN (COND (R NIL) (T (PSOLVE REST B D)))))
                      ((EQ P 'IS) (SETQ F (PINST (CADDR G) B))
                                  (COND ((PVARS F NIL) (RETURN NIL)))
                                  (RETURN (PSOLVE REST (PUNIFY (CADR G) (EVAL F) B) D)))
                      ((EQ P 'LISP) (SETQ F (PINST (CADR G) B))
                                    (COND ((PVARS F NIL) (RETURN NIL)))
                                    (RETURN (COND ((EVAL F) (PSOLVE REST B D))
                                                  (T NIL)))))
                (RETURN NIL])

(PQUERY
  [LAMBDA (GOALS D)
          (PROG ((*PGEN* 0) (*PCOUNT* 0))
                (SETQ *PDEEP* NIL)
                (SETQ *PCUT* NIL)
                (RETURN (PSOLVE GOALS NIL (OR D *PDEPTH*)])

(PASSERT
  [LAMBDA (C)
          (PROG ((P (PPRED (CAR C))))
                (COND ((MEMB P *PBUILTINS*)
                        (PRINT '(CANNOT REDEFINE A BUILTIN PREDICATE))
                        (RETURN NIL)))
                (PUTPROP P 'PCLAUSES (APPEND (GETPROP P 'PCLAUSES) (LIST C)))
                (SETQ *PPREDS* (ADDLIST P *PPREDS*))
                (RETURN P])

(<-
  [NLAMBDA C
           (PASSERT C])

(PCLEAR
  [LAMBDA (P)
          (COND (P (PUTPROP P 'PCLAUSES NIL)
                   (SETQ *PPREDS* (DREMOVE P *PPREDS*))
                   P)
                (T (MAPC *PPREDS* '(LAMBDA (PC-P-) (PUTPROP PC-P- 'PCLAUSES NIL)))
                   (SETQ *PPREDS* NIL)
                   T])

(PLISTING
  [LAMBDA (P)
          (COND ((NULL P) (MAPC *PPREDS* 'PLISTING) T)
                (T (MAPC (GETPROP P 'PCLAUSES)
                         '(LAMBDA (PL-C-) (PRINT (CONS '<- PL-C-))))
                   P])

(PQ
  [NLAMBDA GOALS
           (PROG ((SOLS (PQUERY GOALS NIL))
                  (VS (REVERSE (PVARS GOALS NIL)))
                  (N 0))
                 (COND ((NULL SOLS)
                         (PRINT 'NO)
                         (AND *PDEEP* (PRINT '(DEPTH LIMIT HIT - SEE *PDEPTH*)))
                         (RETURN NIL)))
                 (MAPC SOLS
                       '(LAMBDA (PQ-B-)
                                (SETQ N (ADD1 N))
                                (COND ((NULL VS) (PRINT 'YES))
                                      (T (MAPC VS
                                               '(LAMBDA (PQ-V-)
                                                        (PRIN1 PQ-V-)
                                                        (PRIN1 " = ")
                                                        (PRINT (PINST PQ-V- PQ-B-))))
                                         (TERPRI)))))
                 (AND *PDEEP* (PRINT '(DEPTH LIMIT HIT - MORE SOLUTIONS MAY EXIST)))
                 (AND *PMAX* (NULL (LESSP N *PMAX*))
                      (PRINT '(SOLUTION LIMIT HIT - SEE *PMAX*)))
                 (RETURN N])
)
(PRINT 'PROLOG2FNS)
(RPAQQ PROLOG2FNS
       (PVARP PVARS PDEREF PUNIFY PRENAME PRENAME1 PINST PPRED PSOLVE PSOLVE1 PBUILTIN
              PQUERY PASSERT <- PCLEAR PLISTING PQ))
(RPAQQ PROLOG2VARS (*PGEN* *PMAP* *PCOUNT* *PCUT* *PDEEP* *PDEPTH* *PMAX*
                    *PPREDS* *PBUILTINS*))
(RPAQQ PROLOG2COMS (PROLOG2 - A SMALL PROLOG FOR LISPF4))
(RPAQ PROLOG2GENNR 1)
(RPAQ *PGEN* 0)
(RPAQ *PMAP* NIL)
(RPAQ *PCOUNT* 0)
(RPAQ *PCUT* NIL)
(RPAQ *PDEEP* NIL)
(RPAQ *PDEPTH* 40)
(RPAQ *PMAX* 100)
(RPAQ *PPREDS* NIL)
(RPAQQ *PBUILTINS* (CUT TRUE FAIL = NOT IS LISP))
STOP
