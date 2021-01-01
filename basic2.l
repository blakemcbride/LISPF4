(FILEHEADER BASIC2)
(PRINT 'BASIC2-PACKAGE)
(PRINT '(VERSION 11))
(DEFINEQ
(*
  (SUBR . TIMES))
  
(+
  (SUBR . PLUS))
  
(-
  [LAMBDA ARGS
          (COND (((SUBR . EQUAL) 1 ((SUBR . LENGTH) ARGS))
                  ((SUBR . DIFFERENCE) 0 ((SUBR . CAR) ARGS)))
                (T ((SUBR . APPLY) '(SUBR . DIFFERENCE) ARGS])
  
(/
  (SUBR . QUOTIENT))
  
(<
  (SUBR . LESSP))
  
(<=
  (LAMBDA (A B) ((SUBR . NULL) ((SUBR . GREATERP) A B))))
  
(=
  (SUBR . EQUAL))
  
(>
  (SUBR . GREATERP))
  
(>=
  (LAMBDA (A B) ((SUBR . NULL) ((SUBR . LESSP) A B))))
  
(ABS
  (LAMBDA (X) (TIMES X (SIGN X))))
  
(ADD1VAR
  [NLAMBDA (ADD1X) (SET ADD1X (ADD1 (EVAL ADD1X])
  
(ADDPROP
  [LAMBDA (A IND S FLAG)
          (PUTPROP A IND
                   (PROG ((TEMP (GETPROP A IND)))
                         (RETURN
                           (COND ((NLISTP TEMP) (LIST S))
                                 (FLAG (CONS S TEMP))
                                 (T (NCONC1 TEMP S])
  
(APPLY*
  (LAMBDA (FN . L) (APPLYSTK FN L (BINDENV FN))))
  
(ATTACH
  (LAMBDA (X Y)
          (RPLACD Y (CONS (CAR Y) (CDR Y)))
          (RPLACA Y X)))
  
(BOUNDP
  [NLAMBDA (A)
           (OR (NULL (NULL (STRINGP A)))
               (NULL (NULL (NUMBERP A)))
               (AND (LITATOM A) (NEQ (CAR A) 'NOBIND])
  
(CHANGEPROP
  [LAMBDA (A P PR)
          (AND (LITATOM A)
               (PROG ((L (GETPROPLIST A)))
                     (AND (NLISTP L) (RETURN))
                LOOP (AND (EQ P (CAR L))
                          (RPLACA L PR)
                          (RETURN A))
                     (AND (NLISTP (SETQ L (CDR L))) (RETURN))
                     (AND (NLISTP (SETQ L (CDR L))) (RETURN))
                     (GO LOOP])
  
(COPY
  (LAMBDA (X) (SUBPAIR NIL NIL X)))
  
(DEFLIST
  [LAMBDA (L PROP)
          (MAPC L
                '(LAMBDA (X) (PUTPROP (CAR X) PROP (CADR X])
  
(DOCOLLECT
  [LAMBDA (ITEM LST)
          (-*- LMM: "30-SEP-76 13:03:33")
          (COND ((NLISTP LST)
                  (RPLACD (SETQ LST (LIST ITEM)) LST))
                (T (CDR (RPLACD LST (CONS ITEM (CDR LST])
  
(DREMOVE
  [LAMBDA (X L)
          (COND ((ATOM L) NIL)
                [(EQ X (CAR L))
                  (COND ((CDR L)
                          (RPLACA L (CADR L))
                          (RPLACD L (CDDR L))
                          (DREMOVE X L]
                (T (PROG ((Z L))
                    LP   [COND ((ATOM (CDR L)) (RETURN Z))
                               ((EQ X (CADR L))
                                 (RPLACD L (CDDR L)))
                               (T (SETQ L (CDR L]
                         (GO LP])
  
(DREVERSE
  (LAMBDA (L)
          (PROG ((L1 L) Y Z)
           LP   [COND ((ATOM (SETQ Y L))
                        (COND ((OR (NULL Z) (NULL (CDR Z)))
                                (RETURN Z))
                              ((NULL (CDDR Z))
                                (SETQ Y (CAR L1))
                                (RPLACA L1 (CAR Z))
                                (RPLACA Z Y)
                                (RPLACD L1 Z)
                                (RPLACD Z NIL)
                                (RETURN L1))
                              (T (RPLACD
                                   (NTH Z (SUB1 (LENGTH Z)))
                                   Z)
                                 (SETQ Y (CAR L1))
                                 (RPLACA L1 (CAR Z))
                                 (RPLACA Z Y)
                                 (RPLACD L1 (CDR Z))
                                 (RPLACD Z NIL)
                                 (RETURN L1]
                (SETQ L (CDR L))
                (SETQ Z (RPLACD Y Z))
                (GO LP))))
  
(DSORT
  (LAMBDA (DATA COMPAREFN)
          (PROG ((SORTTMP1 DATA) (SORTTMP2 (CONS)))
           L2   (COND ((LISTP SORTTMP1)
                        [SETQ SORTTMP1
                              (CDR (RPLACA SORTTMP1
                                     (LIST (CAR SORTTMP1]
                        (GO L2))
                      ((NLISTP DATA) (RETURN DATA))
                      ((EQ COMPAREFN T) (SETQ COMPAREFN 'ALPHORDER))
                      ((NULL COMPAREFN) (SETQ COMPAREFN 'ALPHORDER)))
           L1   (COND [(NLISTP (CDR DATA))
                        (RETURN
                          (RPLACA
                            (RPLACD DATA (CDAR DATA))
                            (CAAR DATA]
                      (T (SETQ SORTTMP1 DATA)))
           LOOP (AND (LISTP SORTTMP1)
                     (LISTP (CDR SORTTMP1))
                     (PROGN (MERGE--- SORTTMP2
                              (CAR SORTTMP1)
                              (CADR SORTTMP1)
                              COMPAREFN)
                            [RPLACD SORTTMP1
                              (SETQ SORTTMP1
                                    (CDDR (RPLACA SORTTMP1
                                            (CDR SORTTMP2]
                            (GO LOOP)))
                (GO L1))))
  
(DSUBST
  (LAMBDA (NEW OLD SOURCE)
          [COND ((LISTP SOURCE)
                  [COND ((EQUAL OLD (CAR SOURCE))
                          (RPLACA SOURCE (COPY NEW)))
                        (T (DSUBST NEW OLD (CAR SOURCE]
                  (COND ((EQUAL OLD (CDR SOURCE))
                          (RPLACD SOURCE (COPY NEW)))
                        (T (DSUBST NEW OLD (CDR SOURCE]
          SOURCE))
  
(ENDCOLLECT
  [LAMBDA (LST TAIL)
          (COND ((NULL LST) TAIL)
                (T (PROG1 (CDR LST) (RPLACD LST TAIL])
  
(EQLENGTH
  [LAMBDA (X N)
          (-*- GENERATED BY PAATERN MATCH. INCLUDED SO USER CAN LOAD CODE THAT 
             HAS BEEN DWIMIFIED AND OR COMPILED INTO A NONCLISP SYSTEM AND RUN 
             IT.)
          (COND ((LESSP N 0) NIL)
                ((ZEROP N) (NLISTP X))
                (T (AND (LISTP (SETQ X (NTH X N)))
                        (NLISTP (CDR X])
  
(EVERY
  [LAMBDA (X FN1 FN2)
          (PROG NIL
           A    (COND ((NULL X) (RETURN T))
                      ((APPLY FN1 (LIST (CAR X)))
                        [SETQ X
                              (COND ((NULL FN2) (CDR X))
                                    (T (APPLY FN2 (LIST X]
                        (GO A))
                      (T (RETURN])
  
(FIX
  (LAMBDA (N) (CAR (IQUOREM N 1))))
  
(FLOAT
  (LAMBDA (N) (QUOTIENT N 1.)))
  
(GCGAG
  (LAMBDA (X) (SYSFLAG 1 X)))
  
(GEQ
  (LAMBDA (A B) (NULL (LESSP A B))))
  
(GETDQ
  (NLAMBDA (FN) (GETD FN)))
  
(GETLIS
  [LAMBDA (X PROPS)
          (-*- WT: "31-MAY-79 22:25")
          (PROG [(Z (COND ((LITATOM X) (GETPROPLIST X)) (T X]
           LP   (RETURN (COND ((NLISTP Z) NIL)
                              ((MEMB (CAR Z) PROPS) Z)
                              (T [SETQ Z (CDR (LISTP (CDR Z]
                                 (GO LP])
  
(GETPROPLIST
  (LAMBDA (A) (AND (LITATOM A) (CDR A))))
  
(GETTOPVAL
  (LAMBDA (ATM) (AND (LITATOM ATM) (CAR ATM))))
  
(INTERSECTION
  (LAMBDA (X Y)
          (PROG ((R (CONS)) S)
           LP   (COND ((NLISTP X) (RETURN (CAR R)))
                      ([COND [(LITATOM (SETQ S (CAR X)))
                               (AND (MEMB S Y)
                                    (NULL (MEMB S (CAR R]
                             (T (AND (MEMBER S Y)
                                     (NULL (MEMBER S (CAR R]
                        (TCONC R S)))
                (SETQ X (CDR X))
                (GO LP))))
  
(LASTN
  (LAMBDA (L N)
          (PROG (X Y)
                (COND ((NLISTP L) (RETURN NIL))
                      ((NULL (SETQ X (NTH L N))) (RETURN)))
           LP   [COND ((NULL (SETQ X (CDR X)))
                        (RETURN (CONS Y L]
                (SETQ Y (NCONC1 Y (CAR L)))
                (SETQ L (CDR L))
                (GO LP))))
  
(LCONC
  [LAMBDA (P A)
          (PROG NIL
                (OR A (RETURN P))
                [AND (NLISTP P) (RETURN (CONS A (LAST A]
                [AND (NLISTP (CDR P))
                     (RPLACA P A)
                     (RETURN (RPLACD P (LAST A]
                (RPLACD (CDR P) A)
                (RETURN (RPLACD P (LAST A])
  
(LEQ
  (LAMBDA (A B) (NULL (GREATERP A B))))
  
(LISTGET
  (LAMBDA (LST PROP)
          (-*- LIKE GETP BUT WORKS ON LISTS, SEARCHING THEM TWO CDRS AT A TIME.)
          (PROG NIL
           LP   [COND ((NLISTP LST) (RETURN))
                      ((EQ (CAR LST) PROP) (RETURN (CADR LST]
                [SETQ LST (CDR (LISTP (CDR LST]
                (GO LP))))
  
(LISTGET1
  (LAMBDA (LST PROP)
          (-*- USED TO BE CALLED GET. LIKE LISTGET BUT ONLY SEARCHES ONE CDR AT 
             A TIME.)
          (PROG NIL
           LP   [COND ((NLISTP LST) (RETURN))
                      ((EQ (CAR LST) PROP) (RETURN (CADR LST]
                (SETQ LST (CDR LST))
                (GO LP))))
  
(LISTPUT
  (LAMBDA (LST PROP VAL)
          (-*- LIKE PUT BUT WORKS ON LISTS. INVERSE OF LISTGET)
          (PROG ((X (OR (LISTP LST) ("LST is not a list."))) X0)
           LOOP (COND ((NLISTP (CDR X))
                        (-*- ODD PARITY - EITHER
                           (A B C)
                           OR
                           (A B C . D)
                           - DROP THRU AND ADD AT BEGINNING))
                      ((EQ (CAR X) PROP)
                        (-*- FOUND IT)
                        (RPLACA (CDR X) VAL)
                        (RETURN VAL))
                      ([LISTP (SETQ X (CDDR (SETQ X0 X]
                        (GO LOOP))
                      ((NULL X)
                        (-*- RAN OUT WITHOUT FINDING PROP ON EVEN PARITY. ADD AT 
                           END IF X IS NOT NIL, MEANS ENDED IN A NON-LIST 
                           FOLLOWING EVEN PARITY, E.G.
                           (A B . C)
                           SO DROP THROUGH AND ADD AT FRONT.)
                        (RPLACD (CDR X0) (LIST PROP VAL))
                        (RETURN VAL)))
           ADDFRONT
                [RPLNODE LST PROP
                  (CONS VAL (CONS (CAR LST) (CDR LST]
                (RETURN VAL))))
  
(LISTPUT1
  (LAMBDA (LST PROP VAL)
          (-*- USED TO BE CALLED PUTL. LIKE LISTPUT BUT ONLY SEARCHES ONE CDR AT 
             A TIME. INVERSE OF LISTGET1)
          (PROG [(X (OR (LISTP LST) ("LST is not a list."]
           LP   (COND [(NLISTP X)
                        (-*- NOTE NO CHECKS FOR LISTS ENDING IN DOTTED PAIRS.)
                        (RETURN (NCONC LST (LIST PROP VAL]
                      ((EQ (CAR X) PROP)
                        [COND ((CDR X) (RPLACA (CDR X) VAL))
                              (T (RPLACD X (LIST VAL]
                        (RETURN LST)))
                (SETQ X (CDR X))
                (GO LP))))
  
(LOGOUT
  (SUBR . EXIT))
  
(LSUBST
  [LAMBDA (NEW OLD EXPR)
          (-*- LMM "16-FEB-82 22:11")
          (-*- SUBSTITUTES X AS A SEGMENT FOR Y IN Z. E.G. LSUBST
             ((A B) Y (X Y Z))
             IS
             (X A B Z)
             NOT MEANINGFUL FOR Y AN ATOM AND CDR OF A LIST. IF X IS NIL, 
             OPERATION EFFECTIVELY DELETES Y, I.E. PRODUCES A COPY WITHOUT Y 
             IN IT.)
          (COND ((NULL EXPR) NIL)
                ((NLISTP EXPR)
                  (COND ((EQ OLD EXPR) NEW) (T EXPR)))
                [(EQUAL OLD (CAR EXPR))
                  (NCONC (COPY NEW) (LSUBST NEW OLD (CDR EXPR]
                (T (CONS (LSUBST NEW OLD (CAR EXPR))
                         (LSUBST NEW OLD (CDR EXPR])
  
(MAP2C
  (LAMBDA (MAPX MAPY MAPFN1 MAPFN2)
          (PROG NIL
           LP   (COND ((OR (NLISTP MAPX) (NLISTP MAPY)) (RETURN)))
                (APPLY* MAPFN1 (CAR MAPX) (CAR MAPY))
                [COND (MAPFN2
                        (SETQ MAPX (APPLY* MAPFN2 MAPX))
                        (SETQ MAPY (APPLY* MAPFN2 MAPY)))
                      (T (SETQ MAPX (CDR MAPX))
                         (SETQ MAPY (CDR MAPY]
                (GO LP))))
  
(MAP2CAR
  (LAMBDA (MAPX MAPY MAPFN1 MAPFN2)
          (PROG (MAPL MAPE)
           LP   (COND ((OR (NLISTP MAPX) (NLISTP MAPY))
                        (RETURN MAPL)))
                (SETQ MAPE
                      (CONS (APPLY* MAPFN1 (CAR MAPX) (CAR MAPY))
                            MAPE))
                (COND (MAPL (RPLACD (CDR MAPE) (RPLACD MAPE)))
                      (T (SETQ MAPL MAPE)))
                [COND (MAPFN2
                        (SETQ MAPY (APPLY* MAPFN2 MAPY))
                        (SETQ MAPX (APPLY* MAPFN2 MAPX)))
                      (T (SETQ MAPY (CDR MAPY))
                         (SETQ MAPX (CDR MAPX]
                (GO LP))))
  
(MAPCON
  (LAMBDA (MAPX MAPFN1 MAPFN2)
          (PROG (MAPL MAPE MAPY)
           LP   [COND ((NLISTP MAPX) (RETURN MAPL))
                      ((LISTP (SETQ MAPY (APPLY* MAPFN1 MAPX)))
                        [COND (MAPE (RPLACD MAPE MAPY))
                              (T (SETQ MAPL (SETQ MAPE MAPY]
                        (PROG NIL
                         LP   (COND ((SETQ MAPY (CDR MAPE))
                                      (SETQ MAPE MAPY)
                                      (GO LP]
                [SETQ MAPX
                      (COND (MAPFN2 (APPLY* MAPFN2 MAPX))
                            (T (CDR MAPX]
                (GO LP))))
  
(MAPCONC
  (LAMBDA (MAPX MAPFN1 MAPFN2)
          (PROG (MAPL MAPE MAPY)
           LP   [COND ((NLISTP MAPX) (RETURN MAPL))
                      ([LISTP (SETQ MAPY (APPLY* MAPFN1 (CAR MAPX]
                        [COND (MAPE (RPLACD MAPE MAPY))
                              (T (SETQ MAPL (SETQ MAPE MAPY]
                        (PROG NIL
                         LP   (COND ((SETQ MAPY (CDR MAPE))
                                      (SETQ MAPE MAPY)
                                      (GO LP]
                [SETQ MAPX
                      (COND (MAPFN2 (APPLY* MAPFN2 MAPX))
                            (T (CDR MAPX]
                (GO LP))))
  
(MAX
  (LAMBDA A
          (PROG ((B (CAR A)))
           LOOP (OR (SETQ A (CDR A)) (RETURN B))
                (OR (LESSP B (CAR A)) (GO LOOP))
                (SETQ B (CAR A))
                (GO LOOP))))
  
(MERGE
  [LAMBDA (A B COMPAREFN)
          (COND ((NLISTP B) A)
                ((NLISTP A) B)
                (T (ATTACH NIL B)
                   (MERGE---
                     (ATTACH NIL A)
                     (CDR A)
                     (CDR B)
                     (SELECTQ COMPAREFN
                       ((T NIL) 'ALPHORDER)
                       COMPAREFN))
                   (RPLACD (RPLACA A (CADR A)) (CDDR A))
                   (RPLACD (RPLACA B (CAR A)) (CDR A])
  
(MERGE---
  (LAMBDA (MERGEROOT A B COMPAREFN)
          (PROG NIL
           LOOP [COND ((NLISTP A) (RETURN (RPLACD MERGEROOT B)))
                      ((NLISTP B) (RETURN (RPLACD MERGEROOT A)))
                      [(APPLY* COMPAREFN (CAR A) (CAR B))
                        (SETQ A
                              (PROG1 (CDR A)
                                     (RPLACD MERGEROOT (RPLACD A NIL]
                      (T (SETQ B
                               (PROG1
                                 (CDR B)
                                 (RPLACD MERGEROOT (RPLACD B NIL]
                (SETQ MERGEROOT (CDR MERGEROOT))
                (GO LOOP))))
  
(MIN
  (LAMBDA A
          (PROG ((B (CAR A)))
           LOOP (OR (SETQ A (CDR A)) (RETURN B))
                (OR (GREATERP B (CAR A)) (GO LOOP))
                (SETQ B (CAR A))
                (GO LOOP))))
  
(MINUS
  (LAMBDA (X) (DIFFERENCE 0 X)))
  
(MINUSP
  (LAMBDA (X) (LESSP X 0)))
  
(MKATOM
  (LAMBDA (X) (PACK (LIST X))))
  
(MKSTRING
  (LAMBDA (X) (CONCAT X)))
  
(NLEFT
  (LAMBDA (L N TAIL)
          (-*- BVM: " 1-FEB-83 17:20")
          (-*- RETURNS TAIL OF L CONTAINING N ELEMENTS MORE THAN TAIL, E.G. IF 
             TAIL IS NIL
             (THE USUAL CASE)
             NLEFT
             ((A B C D E) 2)
             IS
             (D E)
             %. IF FOO IS
             (A B C D E)
             AND FIE IS
             (CDDDR FOO)
             ,
             (NLEFT FOO 1 FIE)
             IS
             (C D E)
             %.)
          (PROG ((X L) (Y L))
           LP   (COND ((ZEROP N) (GO LP1))
                      ((OR (EQ X TAIL) (NLISTP X)) (RETURN NIL)))
                (SETQ X (CDR X))
                (SUB1VAR N)
                (GO LP)
           LP1  (COND ((OR (EQ X TAIL) (NLISTP X)) (RETURN Y)))
                (SETQ X (CDR X))
                (SETQ Y (CDR Y))
                (GO LP1))))
  
(NOTANY
  (LAMBDA (SOMEX SOMEFN1 SOMEFN2)
          (NULL (SOME SOMEX SOMEFN1 SOMEFN2))))
  
(NOTEVERY
  (LAMBDA (EVERYX EVERYFN1 EVERYFN2)
          (NULL (EVERY EVERYX EVERYFN1 EVERYFN2))))
  
(NTHCHAR
  (LAMBDA (X N F) (CAR (NTH (UNPACK X F) N))))
  
(PROPNAMES
  (LAMBDA (A)
          (AND (LITATOM A) (MAPCAR (GETPROPLIST A) 'QUOTE 'CDDR))))
  
(PUTASSOC
  (LAMBDA (KEY VAL L)
          (PROG NIL
           LOOP (COND ((EQ KEY (CAAR L))
                        (RPLACD (CAR L) VAL)
                        (RETURN VAL))
                      ((NULL (CDR L))
                        (RPLACD L (LIST (CONS KEY VAL)))
                        (RETURN VAL)))
                (SETQ L (CDR L))
                (GO LOOP))))
  
(PUTDQ
  (NLAMBDA (FN DEF) (PUTD FN DEF) FN))
  
(PUTPROPS
  (NLAMBDA (X . L)
           (MAP L
                '(LAMBDA (A) (PUTPROP X (CAR A) (CADR A)))
                'CDDR)))
  
(QUIT
  (SUBR . EXIT))
  
(RATOMS
  [LAMBDA (A FILE RDTBL)
          (PROG (L X)
           B    (COND ((EQ (SETQ X (RATOM FILE RDTBL)) A)
                        (RETURN (CAR L)))
                      ((SETQ L (TCONC L X)) (GO B])
  
(REMAINDER
  [LAMBDA (A B) (DIFFERENCE A (TIMES B (QUOTIENT A B])
  
(REMOVE
  (LAMBDA (X L L1)
          [MAPC L
                '(LAMBDA (A)
                         (OR (EQUAL A X) (SETQ L1 (CONS A L1]
          (REVERSE L1)))
  
(REMPROP
  [LAMBDA (A IND)
          (PROG ((TEMP A))
           LOP  (COND ((EQ (CADR TEMP) IND)
                        (RPLACD TEMP (CDDDR TEMP))
                        (RETURN IND))
                      (TEMP (SETQ TEMP (CDDR TEMP)) (GO LOP])
  
(REMPROPLIST
  [LAMBDA (A L) (MAPC L '(NLAMBDA (B) (REMPROP A B])
  
(RPAQ
  [NLAMBDA (A B) (RPLACA A (EVSTK B (BINDENV A])
  
(RPAQQ
  (NLAMBDA (A B) (RPLACA A B)))
  
(RPLNODE
  (LAMBDA (X A D) (RPLACA X A) (RPLACD X D) X))
  
(RPLNODE2
  (LAMBDA (X Y) (RPLACA X (CAR Y)) (RPLACD X (CDR Y)) X))
  
(RPTQ
  (NLAMBDA (N X)
           (APPLYSTK 'RPT
             (LIST (EVSTK N (BINDENV N)) X)
             (BINDENV N))))
  
(RSTRING
  [LAMBDA (N)
          (SETQ N (OR N 75))
          (MAPC '(%( %) %[ %] %" %' %%) '(LAMBDA (X) (CHTAB X 10)))
          (READ)
          (READPOS 1)
          (PROG ((ST ""))
           LOOP (AND (GEQ N (READPOS))
                     (SETQ ST (CONCAT ST (READC)))
                     (GO LOOP))
                (SETQ ST (REVERSE (UNPACK ST)))
           LOOP2
                (AND (EQ '%  (CAR ST))
                     (SETQ ST (CDR ST))
                     (GO LOOP2))
                (MAP '(%( 2 %) 3 %[ 4 %] 5 %" 6 %' 7 %% 23)
                     '(LAMBDA (X) (CHTAB (CAR X) (CADR X)))
                     'CDDR)
                (RETURN (PACK (REVERSE ST])
  
(SAVEDEF
  [LAMBDA (A P)
          (PROG ((PR (OR P 'EXPR)) (DEF (GETD A)))
                (AND DEF (PUTPROP A PR DEF) (RETURN PR])
  
(SETPROPLIST
  (LAMBDA (A L) (AND (LITATOM A) (RPLACD A L) L)))
  
(SETQQ
  (NLAMBDA L (APPLYSTK 'SET L (BINDENV L))))
  
(SETTOPVAL
  (LAMBDA (ATM VAL)
          (AND (LITATOM ATM) (PROGN (RPLACA ATM VAL) VAL))))
  
(SIGN
  (LAMBDA (X)
          (COND ((ZEROP X) 0) ((LESSP 0 X) 1) (T -1))))
  
(SOME
  [LAMBDA (X FN1 FN2)
          (PROG NIL
           A    (COND ((NULL X) (RETURN))
                      ((APPLY FN1 (LIST (CAR X))) (RETURN X))
                      (T [SETQ X
                               (COND ((NULL FN2) (CDR X))
                                     (T (APPLY FN2 (LIST X]
                         (GO A])
  
(STREQUAL
  (LAMBDA (X Y) (AND (STRINGP X) (STRINGP Y) (EQUAL X Y))))
  
(SUB1VAR
  [NLAMBDA (SUB1X) (SET SUB1X (SUB1 (EVAL SUB1X])
  
(SUBLIS
  (LAMBDA (A S) (SUBPAIR (MAPCAR A 'CAR) (MAPCAR A 'CDR) S)))
  
(SUBSET
  [LAMBDA (L F1 F2)
          (AND (NULL F2) (SETQ F2 'CDR))
          (PROG ((A (REVERSE L)) (ENV (BINDENV L)) TMP R)
           LOOP (COND (A (AND (SETQ TMP
                                    (APPLYSTK F1 (LIST (CAR A)) ENV))
                              (SETQ R (CONS TMP R)))
                         (SETQ A (APPLYSTK F2 (LIST A) ENV))
                         (GO LOOP))
                      ((RETURN R])
  
(SUBST
  (LAMBDA (X Y S) (SUBPAIR (CONS Y) (CONS X) S)))
  
(TCONC
  [LAMBDA (P A)
          (PROG ((B (LIST A)))
                [AND (NLISTP P) (RETURN (CONS B (LAST B]
                [AND (NLISTP (CDR P))
                     (RPLACA P B)
                     (RETURN (RPLACD P (LAST B]
                (RPLACD (CDR P) B)
                (RETURN (RPLACD P B])
  
(UNION
  (LAMBDA (X Y)
          (-*- LMM "31-DEC-78 14:47")
          (PROG (VAL)
           LP   [COND ((NLISTP X) (RETURN (ENDCOLLECT VAL Y)))
                      ([COND ((LITATOM (CAR X))
                               (NOT (MEMB (CAR X) Y)))
                             (T (NOT (MEMBER (CAR X) Y]
                        (SETQ VAL (DOCOLLECT (CAR X) VAL]
                (SETQ X (CDR X))
                (GO LP))))
  
(UNSAVEDEF
  [LAMBDA (A P)
          (PROG ((PR (OR P 'EXPR)) DEF)
                (SETQ DEF (GETPROP A PR))
                (AND DEF (PUTPROP A 'FNCELL DEF) (RETURN PR])
  
)
(PRINT 'BASIC2FNS)
(RPAQQ BASIC2FNS
       (* + - / < <= = > >= ABS ADD1VAR ADDPROP APPLY* ATTACH BOUNDP 
          CHANGEPROP COPY DEFLIST DOCOLLECT DREMOVE DREVERSE DSORT 
          DSUBST ENDCOLLECT EQLENGTH EVERY FIX FLOAT GCGAG GEQ GETDQ GETLIS 
          GETPROPLIST GETTOPVAL INTERSECTION LASTN LCONC LEQ LISTGET LISTGET1 
          LISTPUT LISTPUT1 LOGOUT LSUBST MAP2C MAP2CAR MAPCON MAPCONC MAX 
          MERGE MERGE--- MIN MINUS MINUSP MKATOM MKSTRING NLEFT NOTANY 
          NOTEVERY NTHCHAR PROPNAMES PUTASSOC PUTDQ PUTPROPS QUIT RATOMS 
          REMAINDER REMOVE REMPROP REMPROPLIST RPAQ RPAQQ RPLNODE RPLNODE2 
          RPTQ RSTRING SAVEDEF SETPROPLIST SETQQ SETTOPVAL SIGN SOME STREQUAL 
          SUB1VAR SUBLIS SUBSET SUBST TCONC UNION UNSAVEDEF))
(RPAQQ BASIC2COMS BASIC2-PACKAGE)
(RPAQ BASIC2GENNR 11)
STOP
