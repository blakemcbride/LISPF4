 (FILEHEADER BASIC2)
 (PRINT 'BASIC2-PACKAGE)
 (PRINT '(VERSION 8))
 (DEFINEQ
 (ABS
   (LAMBDA (X) (TIMES X (SIGN X))))
   
 (ADDPROP
   <LAMBDA (A IND S FLAG)
           (PUTPROP A IND
                    (PROG ((TEMP (GETPROP A IND)))
                          (RETURN
                            (COND ((NLISTP TEMP) (LIST S))
                                  (FLAG (CONS S TEMP))
                                  (T (NCONC1 TEMP S>)
   
 (APPLY*
   (LAMBDA (FN . L) (APPLYSTK FN L (BINDENV FN))))
   
 (ATTACH
   (LAMBDA (X Y)
           (RPLACD Y (CONS (CAR Y) (CDR Y)))
           (RPLACA Y X)))
   
 (BOUNDP
   <NLAMBDA (A)
            (OR (NULL (NULL (STRINGP A)))
                (NULL (NULL (NUMBERP A)))
                (AND (LITATOM A) (NEQ (CAR A) 'NOBIND>)
   
 (CHANGEPROP
   <LAMBDA (A P PR)
           (AND (LITATOM A)
                (PROG ((L (GETPROPLIST A)))
                      (AND (NLISTP L) (RETURN))
                 LOOP (AND (EQ P (CAR L))
                           (RPLACA L PR)
                           (RETURN A))
                      (AND (NLISTP (SETQ L (CDR L))) (RETURN))
                      (AND (NLISTP (SETQ L (CDR L))) (RETURN))
                      (GO LOOP>)
   
 (COPY
   (LAMBDA (X) (SUBPAIR NIL NIL X)))
   
 (DEFLIST
   <LAMBDA (L PROP)
           (MAPC L
                 '(LAMBDA (X) (PUTPROP (CAR X) PROP (CADR X>)
   
 (DREMOVE
   <LAMBDA (X L)
           (COND ((ATOM L) NIL)
                 <(EQ X (CAR L))
                   (COND ((CDR L)
                           (RPLACA L (CADR L))
                           (RPLACD L (CDDR L))
                           (DREMOVE X L>
                 (T (PROG ((Z L))
                     LP   <COND ((ATOM (CDR L)) (RETURN Z))
                                ((EQ X (CADR L))
                                  (RPLACD L (CDDR L)))
                                (T (SETQ L (CDR L>
                          (GO LP>)
   
 (DREVERSE
   (LAMBDA (L)
           (PROG ((L1 L) Y Z)
            LP   <COND ((ATOM (SETQ Y L))
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
                                  (RETURN L1>
                 (SETQ L (CDR L))
                 (SETQ Z (RPLACD Y Z))
                 (GO LP))))
   
 (DSORT
   (LAMBDA (DATA COMPAREFN)
           (PROG ((SORTTMP1 DATA) (SORTTMP2 (CONS)))
            L2   (COND ((LISTP SORTTMP1)
                         <SETQ SORTTMP1
                               (CDR (RPLACA SORTTMP1
                                      (LIST (CAR SORTTMP1>
                         (GO L2))
                       ((NLISTP DATA) (RETURN DATA))
                       ((EQ COMPAREFN T) (SETQ COMPAREFN 'ALPHORDER))
                       ((NULL COMPAREFN) (SETQ COMPAREFN 'ALPHORDER)))
            L1   (COND <(NLISTP (CDR DATA))
                         (RETURN
                           (RPLACA
                             (RPLACD DATA (CDAR DATA))
                             (CAAR DATA>
                       (T (SETQ SORTTMP1 DATA)))
            LOOP (AND (LISTP SORTTMP1)
                      (LISTP (CDR SORTTMP1))
                      (PROGN (MERGE--- SORTTMP2
                               (CAR SORTTMP1)
                               (CADR SORTTMP1)
                               COMPAREFN)
                             <RPLACD SORTTMP1
                               (SETQ SORTTMP1
                                     (CDDR (RPLACA SORTTMP1
                                             (CDR SORTTMP2>
                             (GO LOOP)))
                 (GO L1))))
   
 (DSUBST
   (LAMBDA (NEW OLD SOURCE)
           <COND ((LISTP SOURCE)
                   <COND ((EQUAL OLD (CAR SOURCE))
                           (RPLACA SOURCE (COPY NEW)))
                         (T (DSUBST NEW OLD (CAR SOURCE>
                   (COND ((EQUAL OLD (CDR SOURCE))
                           (RPLACD SOURCE (COPY NEW)))
                         (T (DSUBST NEW OLD (CDR SOURCE>
           SOURCE))
   
 (EVERY
   <LAMBDA (X FN1 FN2)
           (PROG NIL
            A    (COND ((NULL X) (RETURN T))
                       ((APPLY FN1 (LIST (CAR X)))
                         <SETQ X
                               (COND ((NULL FN2) (CDR X))
                                     (T (APPLY FN2 (LIST X>
                         (GO A))
                       (T (RETURN>)
   
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
   
 (GETPROPLIST
   (LAMBDA (A) (AND (LITATOM A) (CDR A))))
   
 (GETTOPVAL
   (LAMBDA (ATM) (AND (LITATOM ATM) (CAR ATM))))
   
 (LCONC
   <LAMBDA (P A)
           (PROG NIL
                 (OR A (RETURN P))
                 <AND (NLISTP P) (RETURN (CONS A (LAST A>
                 <AND (NLISTP (CDR P))
                      (RPLACA P A)
                      (RETURN (RPLACD P (LAST A>
                 (RPLACD (CDR P) A)
                 (RETURN (RPLACD P (LAST A>)
   
 (LEQ
   (LAMBDA (A B) (NULL (GREATERP A B))))
   
 (MAX
   (LAMBDA A
           (PROG ((B (CAR A)))
            LOOP (OR (SETQ A (CDR A)) (RETURN B))
                 (OR (LESSP B (CAR A)) (GO LOOP))
                 (SETQ B (CAR A))
                 (GO LOOP))))
   
 (MERGE
   <LAMBDA (A B COMPAREFN)
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
                    (RPLACD (RPLACA B (CAR A)) (CDR A>)
   
 (MERGE---
   (LAMBDA (MERGEROOT A B COMPAREFN)
           (PROG NIL
            LOOP <COND ((NLISTP A) (RETURN (RPLACD MERGEROOT B)))
                       ((NLISTP B) (RETURN (RPLACD MERGEROOT A)))
                       <(APPLY* COMPAREFN (CAR A) (CAR B))
                         (SETQ A
                               (PROG1 (CDR A)
                                      (RPLACD MERGEROOT (RPLACD A NIL>
                       (T (SETQ B
                                (PROG1
                                  (CDR B)
                                  (RPLACD MERGEROOT (RPLACD B NIL>
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
   (NLAMBDA (FN DEF) (PUTD FN DEF)))
   
 (PUTPROPS
   (NLAMBDA (X . L)
            (MAP L
                 '(LAMBDA (A) (PUTPROP X (CAR A) (CADR A)))
                 'CDDR)))
   
 (REMAINDER
   <LAMBDA (A B) (DIFFERENCE A (TIMES B (QUOTIENT A B>)
   
 (REMOVE
   (LAMBDA (X L L1)
           <MAPC L
                 '(LAMBDA (A)
                          (OR (EQUAL A X) (SETQ L1 (CONS A L1>
           (REVERSE L1)))
   
 (REMPROP
   <LAMBDA (A IND)
           (PROG ((TEMP A))
            LOP  (COND ((EQ (CADR TEMP) IND)
                         (RPLACD TEMP (CDDDR TEMP))
                         (RETURN IND))
                       (TEMP (SETQ TEMP (CDDR TEMP)) (GO LOP>)
   
 (REMPROPLIST
   <LAMBDA (A L) (MAPC L '(NLAMBDA (B) (REMPROP A B>)
   
 (RPAQ
   <NLAMBDA (A B) (RPLACA A (EVSTK B (BINDENV A>)
   
 (RPAQQ
   (NLAMBDA (A B) (RPLACA A B)))
   
 (RPTQ
   (NLAMBDA (N X)
            (APPLYSTK 'RPT
              (LIST (EVSTK N (BINDENV N)) X)
              (BINDENV N))))
   
 (RSTRING
   <LAMBDA (N)
           (SETQ N (OR N 75))
           (MAPC '(%( %) %< %> %" %' %%) '(LAMBDA (X) (CHTAB X 10)))
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
                 (MAP '(%( 2 %) 3 %< 4 %> 5 %" 6 %' 7 %% 23)
                      '(LAMBDA (X) (CHTAB (CAR X) (CADR X)))
                      'CDDR)
                 (RETURN (PACK (REVERSE ST>)
   
 (SAVEDEF
   <LAMBDA (A P)
           (PROG ((PR (OR P 'EXPR)) (DEF (GETD A)))
                 (AND DEF (PUTPROP A PR DEF) (RETURN PR>)
   
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
   <LAMBDA (X FN1 FN2)
           (PROG NIL
            A    (COND ((NULL X) (RETURN))
                       ((APPLY FN1 (LIST (CAR X))) (RETURN X))
                       (T <SETQ X
                                (COND ((NULL FN2) (CDR X))
                                      (T (APPLY FN2 (LIST X>
                          (GO A>)
   
 (STREQUAL
   (LAMBDA (X Y) (AND (STRINGP X) (STRINGP Y) (EQUAL X Y))))
   
 (SUBSET
   <LAMBDA (L F1 F2)
           (AND (NULL F2) (SETQ F2 'CDR))
           (PROG ((A (REVERSE L)) (ENV (BINDENV L)) TMP R)
            LOOP (COND (A (AND (SETQ TMP
                                     (APPLYSTK F1 (LIST (CAR A)) ENV))
                               (SETQ R (CONS TMP R)))
                          (SETQ A (APPLYSTK F2 (LIST A) ENV))
                          (GO LOOP))
                       ((RETURN R>)
  
 (SUBST
   (LAMBDA (X Y S)
           (SUBPAIR (CONS Y)(CONS X)S)))
 
 (SUBLIS 
   (LAMBDA (A S)
           (SUBPAIR (MAPCAR A 'CAR)(MAPCAR A 'CDR) S)))
  
 (TCONC
   <LAMBDA (P A)
           (PROG ((B (LIST A)))
                 <AND (NLISTP P) (RETURN (CONS B (LAST B>
                 <AND (NLISTP (CDR P))
                      (RPLACA P B)
                      (RETURN (RPLACD P (LAST B>
                 (RPLACD (CDR P) B)
                 (RETURN (RPLACD P B>)
   
 (UNSAVEDEF
   <LAMBDA (A P)
           (PROG ((PR (OR P 'EXPR)) DEF)
                 (SETQ DEF (GETPROP A PR))
                 (AND DEF (PUTPROP A 'FNCELL DEF) (RETURN PR>)
   
 )
 (PRINT 'BASIC2FNS)
 (RPAQQ BASIC2FNS
        (ABS ADDPROP APPLY* ATTACH BOUNDP CHANGEPROP COPY DEFLIST DREMOVE 
             DREVERSE DSORT DSUBST EVERY FIX FLOAT GCGAG GEQ GETDQ GETPROPLIST 
             GETTOPVAL LCONC LEQ MAX MERGE MERGE--- MIN MINUS MINUSP MKATOM 
             MKSTRING NTHCHAR PROPNAMES PUTASSOC PUTDQ PUTPROPS REMAINDER 
             REMOVE REMPROP REMPROPLIST RPAQ RPAQQ RPTQ RSTRING SAVEDEF 
             SETPROPLIST SETQQ SETTOPVAL SIGN SOME STREQUAL SUBSET SUBST SUBLIS
             TCONC UNSAVEDEF))
 (RPAQQ BASIC2COMS BASIC2-PACKAGE)
 (RPAQ BASIC2GENNR 8)
 STOP
