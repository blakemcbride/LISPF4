 (FILEHEADER HISTORY)
 (PRINT '(HISTORY FUNCTIONS))
 (PRINT '(VERSION 2))
 (DEFINEQ
 (LISPX
   (LAMBDA NIL
           (PROG NIL
            LOOP (PRIN0 (EVAL (READ)) T T)
                 (TERPRI)
                 (TERPRI)
                 (GO LOOP))))
   
 (READ
   (LAMBDA NIL
           (PROG (X)
                 (SETQ X ((SUBR . READ)))
                 (AND (LISTP X)
                      (OR (EQ (CAR X) 'REDO) (EQ (CAR X) 'HFIX))
                      (RETURN (EVAL X)))
                 (AND (BOUNDP *HIST)
                      *HIST
                      <OR (NLISTP X)
                          (AND (NEQ '?? (CAR X))
                               (NEQ 'HIST (CAR X>
                      (SETQ *HLIST (TCONC *HLIST X)))
                 (RETURN X))))
   
 (REDO
   (LAMBDA (N1)
           (PROG (LN S)
                 (AND (NULL *HLIST) (RETURN))
                 (SETQ LN (LENGTH (CAR *HLIST)))
                 (AND (NULL N1) (SETQ N1 LN))
                 (AND (OR (NULL (NUMBERP N1))
                          (GREATERP 1 N1)
                          (GREATERP N1 LN))
                      (RETURN '(INVALID PARAMETER)))
                 (SETQ S (CAR (NTH (CAR *HLIST) N1)))
                 (AND *HIST (TCONC *HLIST S))
                 (PRINT S)
                 (RETURN S))))
   
 (??
   <LAMBDA (N1 N2 N3)
           (AND *HLIST
                (PROG <(LN (LENGTH (CAR *HLIST>
                      (COND ((AND (NULL N1) (NULL N2))
                              (SETQ N1 (MAX 1 (DIFFERENCE LN 15)))
                              (SETQ N2 LN)))
                      <COND ((AND (NUMBERP N1) (LESSP N1 0))
                              <SETQ N1
                                    (MAX 1 (ADD1 (PLUS LN N1>
                              <AND (NUMBERP N2)
                                   (SETQ N2
                                         (MIN LN (SUB1 (PLUS N1 N2>
                              (AND (NULL N2) (SETQ N2 LN>
                      (AND (NULL N1) (SETQ N1 LN))
                      (AND (NULL N2) (SETQ N2 N1))
                      (AND (EQ N2 T) (SETQ N2 LN))
                      <AND (OR (NULL (NUMBERP N1))
                               (NULL (NUMBERP N2))
                               (GREATERP 1 N1)
                               (GREATERP N1 LN)
                               (GREATERP N1 N2))
                           (RETURN (PRINT '(INVALID PARAMETER>
                      (PROG ((SHLIST (NTH (CAR *HLIST) N1)))
                       LOOP (AND (NUMBERP N3) (SPACES N3))
                            (PRIN1 N1)
                            (PRIN1 '".  ")
                            (PRIN1 (CAR SHLIST))
                            (TERPRI)
                            (SETQ N1 (ADD1 N1))
                            (SETQ SHLIST (CDR SHLIST))
                            (AND (GREATERP N1 N2) (RETURN (TERPRI)))
                            (GO LOOP>)
   
 (HIST
   (NLAMBDA (*H-P1 *H-N)
            (COND ((EQ *H-P1 'ON) (SETQ *HIST 'ON) 'HIST-ON)
                  ((NULL *HIST) "INVALID PARAMETER: HISTORY MODE IS OFF.")
                  ((EQ *H-P1 'OFF) (SETQ *HIST) 'HIST-OFF)
                  <(EQ *H-P1 'RESET)
                    (PROG <(LN (LENGTH (CAR *HLIST>
                          (COND ((NULL *H-N)
                                  (SETQ *HLIST)
                                  (RETURN 'HIST-RESET))
                                ((AND (NUMBERP *H-N)
                                      (GREATERP LN *H-N)
                                      (GREATERP *H-N -1))
                                  <RPLACA *HLIST
                                    (NTH (CAR *HLIST)
                                         (ADD1
                                           (DIFFERENCE LN (ADD1 *H-N>
                                  (PRIN1 'HIST-RESET)
                                  (PRIN1 '"   LAST ")
                                  (PRIN1 *H-N)
                                  (PRIN1 '" ELEMENTS SAVED")
                                  (RETURN (TERPRI)))
                                (T (PRINT '(INVALID PARAMETER))
                                   (RETURN>
                  (T 'INVALID-PARAMETER))))
   
 (HFIX
   (LAMBDA (N1)
           (PROG (LN S)
                 (AND (NULL *HLIST) (RETURN))
                 (SETQ LN (LENGTH (CAR *HLIST)))
                 (AND (NULL N1) (SETQ N1 LN))
                 (AND (OR (NULL (NUMBERP N1))
                          (GREATERP 1 N1)
                          (GREATERP N1 LN))
                      (RETURN '(INVALID PARAMETER)))
                 <SETQ S
                       (EDITS (COPY (CAR (NTH (CAR *HLIST) N1>
                 (AND *HIST (TCONC *HLIST S))
                 (PRINT S)
                 (RETURN S))))
   
 )
 (PRINT 'HISTORYFNS)
 (RPAQQ HISTORYFNS (LISPX READ REDO ?? HIST HFIX))
 (RPAQQ HISTORYCOMS (HISTORY FUNCTIONS))
 (RPAQ HISTORYGENNR 2)
 (PRINT 'HISTORYVARS)
 (RPAQQ HISTORYVARS (*HIST *HLIST))
 (RPAQQ *HIST NIL)
 (RPAQQ *HLIST NIL)
 STOP
