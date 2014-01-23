 (FILEHEADER STATIC)
 (PRINT '(STATIC VARIABLE FUNCTIONS WRITTEN BY BLAKE MCBRIDE))
 (PRINT '(VERSION 2))
 (DEFINEQ
 (PROGS
   (NLAMBDA $MACRO$
            (EVAL (<LAMBDA (L)
                           (LIST 'PROG
                                 (GETPROP '**STATIC** (CAR L))
                                 (LIST 'RETURN
                                       (LIST 'PROG1
                                             (CONS 'PROG
                                               (APPEND (CDR L) NIL))
                                             (LIST 'SAVE-STATIC (CAR L>
                    $MACRO$))))
   
 (SAVE-STATIC
   <NLAMBDA (N ENV)
            (SETQ ENV (BINDENV N))
            (PUTPROP '**STATIC** N
                     (MAPCAR (GETPROP '**STATIC** N)
                             '(LAMBDA
                                (X)
                                (LIST (CAR X)
                                      (LIST 'QUOTE
                                            (EVSTK (CAR X) ENV>)
   
 (DELETE-STATIC
   (NLAMBDA (N L)
            (COND ((NULL L) (REMPROP '**STATIC** N))
                  (T (AND (ATOM L) (SETQ L (CONS L NIL)))
                     <PUTPROP '**STATIC** N
                       (SUBSET
                         (GETPROP '**STATIC** N)
                         '(LAMBDA
                            (X)
                            (COND ((MEMB (CAR X) L) NIL) (X>
                     N))))
   
 (GET-STATIC
   (LAMBDA (N) (GETPROP '**STATIC** N)))
   
 (ADD-STATIC
   (NLAMBDA (N L)
            <PUTPROP '**STATIC** N
                     (NCONC (GETPROP '**STATIC** N)
                            (MAPCAR L
                              '(LAMBDA
                                 (L)
                                 (COND ((ATOM L) (LIST L NIL))
                                       (T (LIST
                                            (CAR L)
                                            (LIST 'QUOTE
                                              (EVSTK
                                                (CADR L)
                                                (BINDENV N>
            N))
   
 (CREATE-STATIC
   (NLAMBDA (N L)
            <PUTPROP '**STATIC** N
                     (MAPCAR L
                             '(LAMBDA
                                (L)
                                (COND ((ATOM L) (LIST L NIL))
                                      (T (LIST
                                           (CAR L)
                                           (LIST 'QUOTE
                                             (EVSTK
                                               (CADR L)
                                               (BINDENV N>
            N))
   
 )
 (PRINT 'STATICFNS)
 (RPAQQ STATICFNS
        (PROGS SAVE-STATIC DELETE-STATIC GET-STATIC ADD-STATIC CREATE-STATIC))
 (RPAQQ STATICCOMS (STATIC VARIABLE FUNCTIONS WRITTEN BY BLAKE MCBRIDE))
 (RPAQ STATICGENNR 2)
 STOP
