(FILEHEADER MATCH)
(PRINT '(MATCH WRITTEN BY BLAKE MCBRIDE))
(PRINT '(VERSION 7))
(DEFINEQ
(LMATCH
  (LAMBDA (M-M- M-L- M-F-)
          (PROG (M-R-)
           LP   (AND (NLISTP M-L-) (RETURN M-R-))
                (AND (MATCH M-M- (CAR M-L-))
                     (SETQ M-R- T)
                     (NULL M-F-)
                     (RETURN M-R-))
                (SETQ M-L- (CDR M-L-))
                (GO LP))))
  
(MATCH
  [LAMBDA (M-P- M-D- M-AP-)
          (COND ((AND (NLISTP M-P-) (NLISTP M-D-))
                  (AND (OR (EQ M-P- M-D-)
                           (EQ M-P- '*)
                           (EQ M-P- '**))
                       T))
                ((NLISTP M-P-) NIL)
                [(NLISTP M-D-)
                  (COND ((EQ '? (CAR M-P-))
                          (MATCHA
                            (CADR M-P-)
                            (CADDR M-P-)
                            NIL
                            (CONS M-D-)))
                        ((EQ '?? (CAR M-P-))
                          (MATCHP
                            (CADR M-P-)
                            (CADDR M-P-)
                            NIL
                            (CONS M-D-]
                [(NLISTP (SETQ M-AP- (CAR M-P-)))
                  (COND ((OR (EQUAL M-AP- (CAR M-D-))
                             (EQ M-AP- '*))
                          (MATCH (CDR M-P-) (CDR M-D-)))
                        ((EQ M-AP- '**)
                          (OR (MATCH (CDR M-P-) (CDR M-D-))
                              (MATCH M-P- (CDR M-D-]
                ((EQ (CAR M-AP-) '?)
                  (MATCHA (CADR M-AP-)
                          (CADDR M-AP-)
                          (CDR M-P-)
                          M-D-))
                ((EQ (CAR M-AP-) '??)
                  (MATCHP (CADR M-AP-)
                          (CADDR M-AP-)
                          (CDR M-P-)
                          M-D-))
                ((MATCH M-AP- (CAR M-D-))
                  (MATCH (CDR M-P-) (CDR M-D-])
  
(MATCHA
  [LAMBDA (M-C- M-L- M-P- M-D-)
          (PROG (M-F- M-TO- M-DM- M-V- M-AB-)
                (AND (LITATOM M-L-) (SETQ M-L- (EVAL M-L-)))
                (SETQ M-F- (CAR M-C-))
                (SETQ M-TO- (CADR M-C-))
                (SETQ M-DM- (CADDR M-C-))
                (AND (SETQ M-V- (CAR (CDDDR M-C-))) (SET M-V-))
                (SETQ M-AB- (CADR (CDDDR M-C-)))
                (RETURN (MATCHA1 M-P- M-D-])
  
(MATCHA1
  [LAMBDA (M-P- M-D-)
          (COND ((NULL M-D-) (AND (LESSP M-F- 1) (NULL M-P-)))
                ((AND (LESSP M-F- 1) (EQUAL M-TO- 0))
                  (MATCH M-P- M-D-))
                [(LESSP M-F- 1)
                  (COND ((MATCH M-P- M-D-))
                        ([MATCHAB (MATCHL M-L- (CONS (CAR M-D-]
                          (AND (NUMBERP M-TO-)
                               (SETQ M-TO- (SUB1 M-TO-)))
                          (COND ((MATCHA1 M-P- (CDR M-D-))
                                  [AND M-V-
                                       (SET M-V-
                                            (CONS
                                              (CAR M-D-)
                                              (EVAL M-V-]
                                  T]
                ([MATCHAB (MATCHL M-L- (CONS (CAR M-D-]
                  (SETQ M-F- (SUB1 M-F-))
                  (AND (NUMBERP M-TO-) (SETQ M-TO- (SUB1 M-TO-)))
                  (COND ((MATCHA1 M-P- (CDR M-D-))
                          [AND M-V-
                               (SET M-V-
                                    (CONS (CAR M-D-) (EVAL M-V-]
                          T])
  
(MATCHAB
  (LAMBDA (M-D-) (COND (M-AB- (NULL M-D-)) (T M-D-))))
  
(MATCHL
  (LAMBDA (M-P- M-D-)
          (PROG NIL
           LOOP (AND (NLISTP M-P-) (RETURN))
                (COND ((MATCH (LIST (CAR M-P-)) M-D-)
                        (AND M-DM-
                             (SETQ M-L- (REMOVEF (CAR M-P-) M-L-)))
                        (RETURN T)))
                (SETQ M-P- (CDR M-P-))
                (GO LOOP))))
  
(MATCHP
  [LAMBDA (M-C- M-L- M-P- M-D-)
          (PROG (M-F- M-TO- M-DM- M-V- M-AB- M-AO-)
                (AND (LITATOM M-L-) (SETQ M-L- (EVAL M-L-)))
                (SETQ M-F- (CAR M-C-))
                (SETQ M-TO- (CADR M-C-))
                (SETQ M-DM- (CADDR M-C-))
                (AND (SETQ M-V- (CAR (CDDDR M-C-))) (SET M-V-))
                (SETQ M-AB- (CADR (CDDDR M-C-)))
                (SETQ M-AO- (CADDR (CDDDR M-C-)))
                (RETURN (MATCHP1 M-P- M-D-])
  
(MATCHP1
  [LAMBDA (M-P- M-D-)
          (COND ((NULL M-D-) (AND (LESSP M-F- 1) (NULL M-P-)))
                ((AND (LESSP M-F- 1) (EQUAL M-TO- 0))
                  (MATCH M-P- M-D-))
                [(LESSP M-F- 1)
                  (COND ((MATCH M-P- M-D-))
                        ([MATCHAB (MATCHPL M-L- (CONS (CAR M-D-]
                          (AND (NUMBERP M-TO-)
                               (SETQ M-TO- (SUB1 M-TO-)))
                          (COND ((MATCHP1 M-P- (CDR M-D-))
                                  [AND M-V-
                                       (SET M-V-
                                            (CONS
                                              (CAR M-D-)
                                              (EVAL M-V-]
                                  T]
                ([MATCHAB (MATCHPL M-L- (CONS (CAR M-D-]
                  (SETQ M-F- (SUB1 M-F-))
                  (AND (NUMBERP M-TO-) (SETQ M-TO- (SUB1 M-TO-)))
                  (COND ((MATCHP1 M-P- (CDR M-D-))
                          [AND M-V-
                               (SET M-V-
                                    (CONS (CAR M-D-) (EVAL M-V-]
                          T])
  
(MATCHPL
  [LAMBDA (M-P- M-D-)
          (COND ((EQ 'AND M-AO-) (MATCHPLA M-P- M-D-))
                (T (MATCHPLO M-P- M-D-])
  
(MATCHPLA
  (LAMBDA (M-P- M-D-)
          (PROG NIL
           LOOP (AND (NLISTP M-P-) (RETURN T))
                (COND ((APPLY (CAR M-P-) M-D-)
                        (AND M-DM-
                             (SETQ M-L- (REMOVEF (CAR M-P-) M-L-)))
                        (SETQ M-P- (CDR M-P-))
                        (GO LOOP)))
                (RETURN))))
  
(MATCHPLO
  (LAMBDA (M-P- M-D-)
          (PROG NIL
           LOOP (AND (NLISTP M-P-) (RETURN))
                (COND ((APPLY (CAR M-P-) M-D-)
                        (AND M-DM-
                             (SETQ M-L- (REMOVEF (CAR M-P-) M-L-)))
                        (RETURN T)))
                (SETQ M-P- (CDR M-P-))
                (GO LOOP))))
  
(MREPLACE
  [LAMBDA (M-N- M-O- M-P-)
          (COND ((ATOM M-P-)
                  (COND ((EQ M-P- M-O-) M-N-) (M-P-)))
                [(MATCH M-O- (CAR M-P-))
                  (CONS (COPY M-N-)
                        (MREPLACE M-N- M-O- (CDR M-P-]
                ((CONS (MREPLACE M-N- M-O- (CAR M-P-))
                       (MREPLACE M-N- M-O- (CDR M-P-])
  
(REMOVEF
  [LAMBDA (A B)
          (COND ((NLISTP B) NIL)
                ((EQUAL A (CAR B)) (CDR B))
                (T (CONS (CAR B) (REMOVEF A (CDR B])
  
(REMOVEL
  [LAMBDA (A B)
          (COND ((NLISTP B) NIL)
                ((EQUAL A (CAR B)) (REMOVEL A (CDR B)))
                (T (CONS (CAR B) (REMOVEL A (CDR B])
  
)
(PRINT 'MATCHFNS)
(RPAQQ MATCHFNS
       (LMATCH MATCH MATCHA MATCHA1 MATCHAB MATCHL MATCHP MATCHP1 MATCHPL 
               MATCHPLA MATCHPLO MREPLACE REMOVEF REMOVEL))
(RPAQQ MATCHCOMS (MATCH WRITTEN BY BLAKE MCBRIDE))
(RPAQ MATCHGENNR 7)
STOP
