(FILEHEADER ASTRUCT)
(PRINT '(ASTRUCTURE FUNCTIONS WRITTEN BY BLAKE MCBRIDE))
(PRINT '(VERSION 10))
(DEFINEQ
(ACHANGE
  [LAMBDA (A K V)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'AFRAME)))
                     (AND (NLISTP L) (RETURN))
                     [AND (NULL K)
                          (RETURN (RETURN (RPLACA L V]
                     (AND (NLISTP K) (SETQ K (CONS K)))
                LP   [COND ((NLISTP (SETQ L (CDR L))) (RETURN))
                           ((NLISTP (CAR L)) (GO LP))
                           ((MATCH (CAR K) (CAAR L))
                             (AND (NLISTP (SETQ K (CDR K)))
                                  (RETURN (RPLACA (CAR L) V)))
                             (SETQ L (CAR L]
                     (GO LP])
  
(AGET
  [LAMBDA (A K)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'AFRAME)))
                     (AND (NLISTP L) (RETURN))
                     (AND (NULL K) (RETURN L))
                     (AND (NLISTP K) (SETQ K (CONS K)))
                LP   [COND ((NLISTP (SETQ L (CDR L))) (RETURN))
                           ((NLISTP (CAR L)) (GO LP))
                           ((MATCH (CAR K) (CAAR L))
                             (AND (NLISTP (SETQ K (CDR K)))
                                  (RETURN (CAR L)))
                             (SETQ L (CAR L]
                     (GO LP])
  
(AGETNAMES
  (LAMBDA (A K R)
          [MAPC (CDR (AGET A K))
                '(LAMBDA (A)
                         (AND (LISTP A)
                              (SETQ R (CONS (CAR A) R]
          R))
  
(AMAKE
  (LAMBDA (K V)
          [MAPC (REVERSE K)
                '(LAMBDA (K) (SETQ V (CONS (CONS K V]
          V))
  
(AMEMBER
  [LAMBDA (A K V)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'AFRAME)))
                     (AND (NLISTP L) (RETURN))
                     [AND (NULL K)
                          (RETURN
                            (COND (V (AND (OR (MATCH V (CDR L))
                                              (LMATCH V (CDR L)))
                                          T))
                                  (T]
                     (AND (NLISTP K) (SETQ K (CONS K)))
                LP   [COND ((NLISTP (SETQ L (CDR L))) (RETURN))
                           ((NLISTP (CAR L)) (GO LP))
                           ((MATCH (CAR K) (CAAR L))
                             [AND (NLISTP (SETQ K (CDR K)))
                                  (RETURN
                                    (COND (V (AND (OR (MATCH V
                                                        (CDAR L))
                                                      (LMATCH V
                                                        (CDAR L)))
                                                  T))
                                          (T]
                             (SETQ L (CAR L]
                     (GO LP])
  
(APRINT
  (LAMBDA (A K)
          (AND (LISTP (SETQ A (AGET A K))) (APRINT1 A 0))))
  
(APRINT1
  [LAMBDA (L N)
          (PROG NIL
                (SPACES N)
                (PRINT (CAR L))
                (SETQ N (PLUS 3 N))
           LOOP (COND ((NULL (SETQ L (CDR L))) (RETURN))
                      ((NLISTP L) (SPACES N) (PRINT L) (RETURN))
                      ((NLISTP (CAR L))
                        (SPACES N)
                        (PRINT (CAR L))
                        (GO LOOP))
                      (T (APRINT1
                           (CAR L)
                           (COND ((GREATERP N 20) 0) (N)))
                         (GO LOOP])
  
(APUT
  [LAMBDA (A K V F)
          (AND (LITATOM A)
               (PUTPROP A 'AFRAME
                 (PROG ((R (GETPROP A 'AFRAME)) L)
                       [AND (NULL K)
                            (RETURN
                              (COND ((NLISTP R) (CONS A V))
                                    ((RPLACD R V]
                       (AND (NLISTP K) (SETQ K (CONS K)))
                       [AND (NLISTP (SETQ L R))
                            (RETURN (CONS A (AMAKE K V]
                  LP1  (COND ((NLISTP (CDR L))
                               (RPLACD L
                                 (APPEND (AMAKE K V) (CDR L)))
                               (RETURN R))
                             [(NLISTP (CAR (SETQ L (CDR L]
                             ((MATCH (CAR K) (CAAR L))
                               (AND (LISTP (SETQ K (CDR K)))
                                    (SETQ L (CAR L))
                                    (GO LP1))
                               (RPLACD
                                 (CAR L)
                                 (COND (F (CONS V (CDAR L)))
                                       (T V)))
                               (RETURN R)))
                       (GO LP1])
  
(AREM
  (LAMBDA (X L L1)
          [MAPC L
                '(LAMBDA (A)
                         (OR (MATCH X A) (SETQ L1 (CONS A L1]
          (REVERSE L1)))
  
(AREMOVE
  [LAMBDA (A K V)
          (AND (LITATOM A)
               (PUTPROP A 'AFRAME
                 (PROG ((R (GETPROP A 'AFRAME)) L TMP)
                       (AND (NLISTP R) (RETURN R))
                       [AND (NULL K)
                            (RETURN
                              (RPLACD R
                                (COND (V (AREM V (CDR R)))
                                      (T NIL]
                       (AND (NLISTP K) (SETQ K (CONS K)))
                       (SETQ L (CDR R))
                  LP1  (COND ((NLISTP L) (RETURN R))
                             ((NLISTP (CAR L))
                               (SETQ L (CDR (SETQ TMP L)))
                               (GO LP1))
                             [(MATCH (CAR K) (CAAR L))
                               (AND (LISTP (SETQ K (CDR K)))
                                    (PROGN
                                      [SETQ L
                                            (CDR (SETQ TMP (CAR L]
                                      (GO LP1)))
                               (COND (V (RPLACD
                                          (CAR L)
                                          (AREM V (CDAR L)))
                                        (RETURN R))
                                     (T [AND (NULL TMP)
                                             (RETURN
                                               (RPLACD R (CDDR R]
                                        (RPLACD TMP (CDR L))
                                        (RETURN R]
                             (T (SETQ L (CDR (SETQ TMP L)))
                                (GO LP1])
  
)
(PRINT 'ASTRUCTFNS)
(RPAQQ ASTRUCTFNS
       (ACHANGE AGET AGETNAMES AMAKE AMEMBER APRINT APRINT1 APUT AREM AREMOVE)
       )
(RPAQQ ASTRUCTCOMS (ASTRUCTURE FUNCTIONS WRITTEN BY BLAKE MCBRIDE))
(RPAQ ASTRUCTGENNR 10)
STOP
