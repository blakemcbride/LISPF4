(FILEHEADER STRUCTURE)
(PRINT '(STRUCTURE WRITTEN BY BLAKE MCBRIDE))
(PRINT '(VERSION 15))
(DEFINEQ
(SATTACH
  [LAMBDA (A K A2 K2 F)
          (PROG ((V (SATTACH2 A2 K2)))
                (AND (NLISTP V) (RETURN))
                (RETURN (SPUT A K V F])
  
(SATTACH2
  [LAMBDA (A K)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'FRAME)))
                     (AND (OR (NULL K) (NULL L)) (RETURN L))
                     (AND (NLISTP K) (SETQ K (LIST K)))
                LP   (COND ((MATCH (CAR K) (CAR L))
                             (AND (NLISTP (SETQ K (CDR K)))
                                  (RETURN (CDR L)))
                             (SETQ L (CADR L))
                             (GO LP)))
                     (OR (LISTP (SETQ L (CDDR L))) (RETURN))
                     (GO LP])
  
(SCHANGE
  [LAMBDA (A K S)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'FRAME)))
                     (AND (OR (NULL K) (NULL L)) (RETURN))
                     (AND (NLISTP K) (SETQ K (LIST K)))
                LP   (COND ((MATCH (CAR K) (CAR L))
                             (AND (NLISTP (CDR K))
                                  (RETURN (RPLACA L S)))
                             (SETQ K (CDR K))
                             (SETQ L (CADR L))
                             (GO LP)))
                     (OR (LISTP (SETQ L (CDDR L))) (RETURN))
                     (GO LP])
  
(SCLAMP
  (LAMBDA (A K A2 K2 V F F2)
          (SETQ V (CONS V))
          (SPUT A K V F)
          (SPUT A2 K2 V F2)))
  
(SGET
  [LAMBDA (A K)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'FRAME)))
                     (AND (OR (NULL K) (NULL L)) (RETURN L))
                     (AND (NLISTP K) (SETQ K (LIST K)))
                LP   (COND ((MATCH (CAR K) (CAR L))
                             (AND (NLISTP (SETQ K (CDR K)))
                                  (RETURN (CADR L)))
                             (SETQ L (CADR L))
                             (GO LP)))
                     (OR (LISTP (SETQ L (CDDR L))) (RETURN))
                     (GO LP])
  
(SGETNAMES
  (LAMBDA (A K) (MAPCAR (SGET A K) 'QUOTE 'CDDR)))
  
(SMAKE
  (LAMBDA (K V)
          [MAPC (REVERSE K)
                '(LAMBDA (K) (SETQ V (CONS K (LIST V]
          V))
  
(SMEMBER
  [LAMBDA (A K V)
          (AND (LITATOM A)
               (PROG ((L (GETPROP A 'FRAME)))
                     [AND (NULL L)
                          (RETURN (AND (NULL K) (NULL V]
                     [AND (NULL K)
                          (RETURN
                            (COND (V (AND (OR (MATCH V (CADR L))
                                              (LMATCH V (CADR L)))
                                          T))
                                  (T]
                     (AND (NLISTP K) (SETQ K (LIST K)))
                LP   (COND ((MATCH (CAR K) (CAR L))
                             [AND (NLISTP (SETQ K (CDR K)))
                                  (RETURN
                                    (COND (V (AND (OR (MATCH V
                                                        (CADR L))
                                                      (LMATCH V
                                                        (CADR L)))
                                                  T))
                                          (T]
                             (SETQ L (CADR L))
                             (GO LP)))
                     (OR (LISTP (SETQ L (CDDR L))) (RETURN))
                     (GO LP])
  
(SNEXT
  [LAMBDA (A)
          (PROG ((V (SGET A)))
                (SPUT A NIL (CDDR V))
                (RETURN (AND V (CONS (CAR V) (LIST (CADR V])
  
(SPRINT
  (LAMBDA (A K) (SPRINT2 (SGET A K) 0)))
  
(SPRINT2
  (LAMBDA (L N)
          (PROG NIL
           LOOP (AND (NLISTP L)
                     (PROGN (SPACES N) (PRINT L) (RETURN)))
                (SPACES N)
                (PRINT (CAR L))
                (OR (SETQ L (CDR L)) (RETURN))
                (AND (NLISTP L) (RETURN (SPRINT2 L N)))
                [SPRINT2
                  (CAR L)
                  (COND ((GREATERP N 60) 0) (T (PLUS N 3]
                (OR (SETQ L (CDR L)) (RETURN))
                (GO LOOP))))
  
(SPUT
  [LAMBDA (A K V F)
          (AND (LITATOM A)
               (PUTPROP A 'FRAME
                 (PROG ((R (GETPROP A 'FRAME)) L)
                       (AND (NULL K) (RETURN V))
                       (AND (NLISTP K) (SETQ K (LIST K)))
                       (AND (NLISTP (SETQ L R))
                            (RETURN (SMAKE K V)))
                  LP1  (COND ((MATCH (CAR K) (CAR L))
                               [AND (LISTP (SETQ K (CDR K)))
                                    (COND ((NLISTP (CADR L))
                                            (RPLACA
                                              (CDR L)
                                              (SMAKE K V))
                                            (RETURN R))
                                          (T (SETQ L (CADR L))
                                             (GO LP1]
                               (RPLACA
                                 (CDR L)
                                 (COND (F (CONS V (CADR L)))
                                       (T V)))
                               (RETURN R)))
                       (AND (NLISTP (CDDR L))
                            (PROGN (NCONC (CDR L) (SMAKE K V))
                                   (RETURN R)))
                       (SETQ L (CDDR L))
                       (GO LP1])
  
(SREM
  (LAMBDA (X L L1)
          [MAPC L
                '(LAMBDA (A)
                         (OR (MATCH X A) (SETQ L1 (CONS A L1]
          (REVERSE L1)))
  
(SREMOVE
  [LAMBDA (A K V)
          (AND (LITATOM A)
               (PUTPROP A 'FRAME
                 (PROG ((R (GETPROP A 'FRAME)) L TMP F)
                       (AND (OR (NLISTP (SETQ L R)) (NULL K))
                            (RETURN))
                       (AND (NLISTP K) (SETQ K (LIST K)))
                  LP1  [COND ((MATCH (CAR K) (CAR L))
                               (AND (LISTP (SETQ K (CDR K)))
                                    (PROGN
                                      [SETQ L
                                            (CAR (SETQ TMP (CDR L]
                                      (SETQ F T)
                                      (GO LP1)))
                               (COND (V (RPLACA
                                          (CDR L)
                                          (SREM V (CADR L)))
                                        (RETURN R))
                                     (T (AND (NULL TMP)
                                             (RETURN (CDDR L)))
                                        [COND
                                          (F (RPLACA TMP (CDDR L)))
                                          (T (RPLACD TMP (CDDR L]
                                        (RETURN R]
                       (AND [NLISTP
                              (SETQ L (CDR (SETQ TMP (CDR L]
                            (RETURN R))
                       (SETQ F)
                       (GO LP1])
  
)
(PRINT 'STRUCTUREFNS)
(RPAQQ STRUCTUREFNS
       (SATTACH SATTACH2 SCHANGE SCLAMP SGET SGETNAMES SMAKE SMEMBER SNEXT 
                SPRINT SPRINT2 SPUT SREM SREMOVE))
(RPAQQ STRUCTURECOMS (STRUCTURE WRITTEN BY BLAKE MCBRIDE))
(RPAQ STRUCTUREGENNR 15)
STOP
