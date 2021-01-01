 (FILEHEADER SCHUM)
 (PRINT '(SCHUM PROGRAMMING SYSTEM))
 (PRINT '(VERSION 2))
 (DEFINEQ
 (ASN
   (LAMBDA NIL
           (SPUSH 'ASN1)
           (SETQ **EXP** (CADDR **EXP**))
           (SETQ **PC** 'SEVAL)))
   
 (ASN1
   (LAMBDA NIL
           (PROG ((B (VARBDG (CADR **EXP**) **ENV**)))
                 (COND (B (VAL:BDG:SET B **VAL**))
                       ((SET (CADR **EXP**) **VAL**)))
                 (SPOP))))
   
 (BDG
   (LAMBDA (VAR VAL) (LIST VAR VAL)))
   
 (BDGS:ENV
   (LAMBDA (E) E))
   
 (BLOCK
   (LAMBDA NIL
           (PROG ((BVARS (CADR **EXP**)) VARS VALS)
            LOOP (COND (BVARS (PUSH (POP BVARS) VARS)
                              (PUSH (POP BVARS) VALS)
                              (GO LOOP)))
                 (SETQ VARS (REVERSE VARS))
                 (SETQ VALS (REVERSE VALS))
                 (SETQ **ENV**
                       (VARSBIND VARS
                         (MAPCAR VALS '(LAMBDA (X) NIL))
                         **ENV**))
                 (SETQ **ARGS** VALS)
                 (SETQ **ARGVALS** (BDGS:ENV **ENV**))
                 (SETQ **PC** 'REC-EVLIS))))
   
 (BODY:LAMBDA-EXP
   (LAMBDA (X) (CADDR X)))
   
 (CLOSURE
   (LAMBDA (FUN ENV) (LIST 'CLOSURE FUN ENV)))
   
 (DEF
   (NLAMBDA L
            (SET (CAR L)
                 (CLOSURE
                   (LAMBDA-EXP (CADR L) (CDDR L))
                   (EMPTY-ENV)))
            (CAR L)))
   
 (EMPTY-ENV
   (LAMBDA NIL NIL))
   
 (ENV
   (LAMBDA (BDGS) BDGS))
   
 (ENV:CLOSURE
   (LAMBDA (X) (CADDR X)))
   
 (ERROR
   (LAMBDA (X) (PRINT X) NIL))
   
 (EVLIS0
   [LAMBDA NIL
           (COND (**ARGS**
                   (SPUSH 'EVLIS1)
                   (SETQ **EXP** (CAR **ARGS**))
                   (SETQ **PC** 'SEVAL))
                 (T (SETQ **ARGVALS** (REVERSE **ARGVALS**))
                    (SETQ **ARGS** (POP **ARGVALS**))
                    (SETQ **PC** 'SCHAPPLY])
   
 (EVLIS1
   (LAMBDA NIL
           (SETQ **ARGVALS** (CONS **VAL** **ARGVALS**))
           (SETQ **ARGS** (CDR **ARGS**))
           (SETQ **PC** 'EVLIS0)))
   
 (EVSEQ
   [LAMBDA NIL
           (COND (**ARGS**
                   (COND ((CDR **ARGS**) (SPUSH 'EVSEQ1)))
                   (SETQ **EXP** (CAR **ARGS**))
                   (SETQ **PC** 'SEVAL))
                 ((SPOP])
   
 (EVSEQ1
   (LAMBDA NIL (SETQ **ARGS** (CDR **ARGS**)) (SETQ **PC** 'EVSEQ)))
   
 (FRAME
   (LAMBDA (EXP ENV PC ARGS ARGVALS SUPER)
           (NCONC (LIST EXP ENV PC ARGS ARGVALS) SUPER)))
   
 (FRAME:LABEL-FUNCTION
   (LAMBDA (X) (CADR X)))
   
 (FUN:APPL
   (LAMBDA (X) (CAR X)))
   
 (FUN:CLOSURE
   (LAMBDA (X) (CADR X)))
   
 (GETL
   [LAMBDA (F)
           (OR (GETPROP F 'SCHUMRES)
               (AND (LISTP (SETQ F (GETD F)))
                    (SELECTQ (CAR F) (NLAMBDA F) (FSUBR F) NIL])
   
 (IF0
   (LAMBDA NIL (SETQ **ARGS** (CDR **EXP**)) (SETQ **PC** 'IF1)))
   
 (IF1
   [LAMBDA NIL
           (COND (**ARGS**
                   (COND ((OR (CDR **ARGS**) (CDAR **ARGS**))
                           (SPUSH 'IF2)))
                   (SETQ **EXP** (CAAR **ARGS**))
                   (SETQ **PC** 'SEVAL))
                 (T (SETQ **VAL** NIL) (SPOP])
   
 (IF2
   [LAMBDA NIL
           (COND (**VAL**
                   (SETQ **ARGS** (CDAR **ARGS**))
                   (SETQ **PC** 'EVSEQ))
                 (T (SETQ **ARGS** (CDR **ARGS**))
                    (SETQ **PC** 'IF1])
   
 (IS-CLOSURE
   (LAMBDA (X) (EQ 'CLOSURE (CAR X))))
   
 (IS-LABEL-FUNCTION
   (LAMBDA (X) (EQ 'LABEL-FUNCTION (CAR X))))
   
 (IS-PRIMOP
   (LAMBDA (F)
           (AND (LISTP (SETQ F (GETD F)))
                (SELECTQ (CAR F) (SUBR F) (LAMBDA F) NIL))))
   
 (LABEL-FUNCTION
   (LAMBDA (FRAME) (LIST 'LABEL-FUNCTION FRAME)))
   
 (LAMBDA-EXP
   (LAMBDA (VARS BODY) (LIST '\ VARS BODY)))
   
 (MAPCAR2
   [LAMBDA (F L1 L2)
           (COND ((NULL L1) NIL)
                 (T (CONS (APPLY F (LIST (CAR L1) (CAR L2)))
                          (MAPCAR2 F (CDR L1) (CDR L2])
   
 (MLOOP
   [LAMBDA NIL
           (PROG NIL
            LOOP (COND (**PC** (APPLY **PC** NIL) (GO LOOP))
                       ((RETURN **VAL**])
   
 (POP
   [NLAMBDA ($POP$)
            (PROG (($POP (EVAL $POP$)))
                  (SET $POP$ (CDR $POP))
                  (RETURN (CAR $POP])
   
 (PUSH
   [NLAMBDA ($PUSH$ $PUSH)
            (CAR (SET $PUSH (CONS (EVAL $PUSH$) (EVAL $PUSH])
   
 (REC-EVLIS
   [LAMBDA NIL
           (COND (**ARGS**
                   (SPUSH 'REC-EVLIS1)
                   (SETQ **EXP** (CAR **ARGS**))
                   (SETQ **PC** 'SEVAL))
                 (T (SETQ **ARGS** (CDDR **EXP**))
                    (SETQ **PC** 'EVSEQ])
   
 (REC-EVLIS1
   (LAMBDA NIL
           (VAL:BDG:SET (POP **ARGVALS**) **VAL**)
           (SETQ **ARGS** (CDR **ARGS**))
           (SETQ **PC** 'REC-EVLIS)))
   
 (SCHAPPLY
   [LAMBDA NIL
           (COND ((ATOM **ARGS**)
                   (SETQ **VAL** (APPLY **ARGS** **ARGVALS**))
                   (SPOP))
                 ((IS-CLOSURE **ARGS**)
                   (SETQ **ENV**
                         (VARSBIND
                           (VARS:LAMBDA-EXP (FUN:CLOSURE **ARGS**))
                           **ARGVALS**
                           (ENV:CLOSURE **ARGS**)))
                   (SETQ **ARGS**
                         (BODY:LAMBDA-EXP (FUN:CLOSURE **ARGS**)))
                   (SETQ **PC** 'EVSEQ))
                 ((IS-LABEL-FUNCTION **ARGS**)
                   (SETQ **VAL** (CAR **ARGVALS**))
                   (SETQ **STACK** (FRAME:LABEL-FUNCTION **ARGS**))
                   (SPOP))
                 ((ERROR "BAD FUNCTION"])
   
 (SCHUM
   [LAMBDA NIL
           (PROG (R)
            LOOP (PRINT 'SCHUM)
                 [SETQ R (PRINT (SCHUMVAL (READ]
                 (AND (NEQ R 'LISP) (GO LOOP])
   
 (SCHUMVAL
   [LAMBDA (**EXP**)
           (PROG ((**ENV** **ENV**)
                   (**PC** 'SEVAL)
                   **ARGS** **ARGVALS**
                   (**STACK** (FRAME NIL NIL NIL NIL NIL NIL)))
                 (RETURN (MLOOP])
   
 (SEVAL
   [LAMBDA NIL
           (PROG (TEMP)
                 (COND ((ATOM **EXP**)
                         (COND ((OR (NUMBERP **EXP**)
                                    (IS-PRIMOP **EXP**))
                                 (SETQ **VAL** **EXP**))
                               ((SETQ TEMP (VARBDG **EXP** **ENV**))
                                 (SETQ **VAL** (VAL:BDG TEMP)))
                               ((ERRORSET
                                  '(SETQ **VAL** (EVAL **EXP**))
                                  NIL))
                               ((ERROR "UNBOUND VARIABLE")))
                         (SPOP))
                       [(AND (ATOM (FUN:APPL **EXP**))
                             (GETL (FUN:APPL **EXP**)))
                         (COND ((SETQ TEMP
                                      (GETPROP
                                        (FUN:APPL **EXP**)
                                        'SCHUMRES))
                                 (SETQ **PC** TEMP))
                               ((GETD (FUN:APPL **EXP**))
                                 (SETQ **VAL** (EVAL **EXP**))
                                 (SPOP]
                       (T (SETQ **ARGVALS** NIL)
                          (SETQ **ARGS** **EXP**)
                          (SETQ **PC** 'EVLIS0])
   
 (SLABEL
   (LAMBDA NIL
           (SETQ **ENV**
                 (VARSBIND
                   (LIST (CADR **EXP**))
                   (LIST (LABEL-FUNCTION **STACK**))
                   **ENV**))
           (SETQ **ARGS** (CDDR **EXP**))
           (SETQ **PC** 'EVSEQ)))
   
 (SPOP
   [LAMBDA NIL
           (COND (**STACK**
                   (SETQ **EXP** (POP **STACK**))
                   (SETQ **ENV** (POP **STACK**))
                   (SETQ **PC** (POP **STACK**))
                   (SETQ **ARGS** (POP **STACK**))
                   (SETQ **ARGVALS** (POP **STACK**)))
                 (T (ERROR "BLEW STACK"])
   
 (SPUSH
   (LAMBDA (TAG)
           (SETQ **STACK**
                 (FRAME **EXP** **ENV** TAG **ARGS** **ARGVALS** **STACK**))))
   
 (VAL:BDG
   (LAMBDA (X) (CADR X)))
   
 (VAL:BDG:SET
   (LAMBDA (X V) (RPLACA (CDR X) V)))
   
 (VARBDG
   (LAMBDA (V E) (ASSOC V E)))
   
 (VARS:ENV
   (LAMBDA (E) (MAPCAR E 'CAR)))
   
 (VARS:LAMBDA-EXP
   (LAMBDA (X) (CADR X)))
   
 (VARSBIND
   [LAMBDA (VARS VALS ENV)
           (COND ((EQUAL (LENGTH VARS) (LENGTH VALS))
                   (NCONC (MAPCAR2 'BDG VARS VALS) ENV))
                 (T (ERROR "WRONG NUMBER OF ARGUMENTS"])
   
 (\
   (NLAMBDA L (CLOSURE (LAMBDA-EXP (CAR L) (CDR L)) **ENV**)))
   
 )
 (PRINT 'SCHUMFNS)
 (RPAQQ SCHUMFNS
        (ASN ASN1 BDG BDGS:ENV BLOCK BODY:LAMBDA-EXP CLOSURE DEF EMPTY-ENV ENV 
             ENV:CLOSURE ERROR EVLIS0 EVLIS1 EVSEQ EVSEQ1 FRAME 
             FRAME:LABEL-FUNCTION FUN:APPL FUN:CLOSURE GETL IF0 IF1 IF2 
             IS-CLOSURE IS-LABEL-FUNCTION IS-PRIMOP LABEL-FUNCTION LAMBDA-EXP 
             MAPCAR2 MLOOP POP PUSH REC-EVLIS REC-EVLIS1 SCHAPPLY SCHUM 
             SCHUMVAL SEVAL SLABEL SPOP SPUSH VAL:BDG VAL:BDG:SET VARBDG 
             VARS:ENV VARS:LAMBDA-EXP VARSBIND \))
 (RPAQQ SCHUMCOMS (SCHUM PROGRAMMING SYSTEM))
 (RPAQ SCHUMGENNR 2)
 (PRINT 'SCHUMVARS)
 (RPAQQ SCHUMVARS
        ((P (SETQ **ENV** (EMPTY-ENV)))
          (PROP SCHUMRES ASN IF0 BLOCK LABEL)))
 (SETQ **ENV** (EMPTY-ENV))
 (DEFLIST '((ASN ASN) (IF0 IF0) (BLOCK BLOCK) (LABEL SLABEL))
          'SCHUMRES)
 STOP
