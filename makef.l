(FILEHEADER MAKEF)
(PRINT '(MAKE FILE PACKAGE MODIFIED BY BLAKE MCBRIDE))
(PRINT '(VERSION 6))
(DEFINEQ
(CLOSE
  (LAMBDA (LFN) (XCALL 2 LFN)))
  
(CURFILE
  (NLAMBDA (FILE)
           (SETQ CURFILE FILE)
           (SETQ CURFILEFNS (PACK (LIST FILE 'FNS)))
           (SETQ CURLIBS (ADDLIST FILE CURLIBS))
           (OR (ERRORSET CURFILEFNS NIL) (SET CURFILEFNS NIL))
           FILE))
  
(FILEHEADER
  (NLAMBDA (A) (APPLY* 'CURFILE A) (PRINT (LIST 'FILEHEADER A))))
  
(LOAD
  <LAMBDA (FILE LDFLG PRINTFLG)
          (PROG (<AL (SUBST LDFLG '* '((LDFLG . *> FILENR UNIT X)
                (OR (SETQ FILENR (OPEN 20 FILE 'OLD 'FORMATTED))
                    (PROGN (PRINTL-SP "CAN NOT OPEN" FILE) (RETURN)))
                (SETQ UNIT (INUNIT FILENR))
                (IOTAB 2 (IOTAB 4))
           LOOP (SELECTQ
                  (SETQ X (READ))
                  ((NIL STOP)
                    (PRINTL-SP FILE '- 'LOADED)
                    (INUNIT UNIT)
                    (CLOSE FILENR)
                    (RETURN FILE))
                  (COND (PRINTFLG (PRINT (EVALA X AL)) (GO LOOP))
                        (T (EVALA X AL) (GO LOOP>)
  
(MAKEF/EVAL
  (LAMBDA (X) (CAR (APPLYSTK 'ERRORSET (LIST X) AL))))
  
(MAKEF/FNSX
  (LAMBDA (L) (PRINTL "(DEFINEQ") (PRETTYPRINT L OPT) (PRINTL ")")))
  
(MAKEF/GETPROPS
  <LAMBDA (L)
          (COND ((NLISTP L) NIL)
                ((MEMB (CAR L) SYSPROPS)
                  (MAKEF/GETPROPS (CDDR L)))
                <(OR NIL* (CADR L))
                  (CONS (CAR L)
                        (CONS (CADR L) (MAKEF/GETPROPS (CDDR L>
                (T (MAKEF/GETPROPS (CDDR L>)
  
(MAKEF/MAPC*
  (LAMBDA (L FN)
          (SELECTQ (AND (CDR L) (NULL (CDDR L)) (CAR L))
                                                   (* (MAPC (MAKEF/EVAL (CADR 
                                                   L)) FN))
                   (MAPC L FN))))
  
(MAKEF/OUT
  (NLAMBDA LL
           (PRIN0 (CONS (CAR LL) (EVLIS (CDR LL))) T OPT)
           (TERPRI)))
  
(MAKEF/PROPX
  <LAMBDA (PROPS L NIL*)
          (COND <(EQ PROPS 'ALL)
                  (MAKEF/MAPC* L
                    '(LAMBDA (X)
                             <SETQ X
                                   (CONS X (MAKEF/GETPROPS (CDR X>
                             (COND ((CDR X)
                                     (PRIN0 (CONS 'PUTPROPS X) T OPT)
                                     (TERPRI>
                ((ATOM PROPS) (MAKEF/PROPX (LIST PROPS) L NIL*))
                (T (MAPC PROPS
                         '(LAMBDA
                            (P RES)
                            <MAKEF/MAPC* L
                              '(LAMBDA
                                 (X)
                                 (AND (OR NIL* (GETPROP X P))
                                      (SETQ RES
                                            (CONS
                                              (LIST X (GETPROP X P))
                                              RES>
                            (AND RES
                                 (MAKEF/OUT DEFLIST
                                   (LIST 'QUOTE (REVERSE RES))
                                   (LIST 'QUOTE P>)
  
(MAKEF/VARSX
  <LAMBDA (L)
          (MAKEF/MAPC* L
            '(LAMBDA (X) (MAKEF/OUT RPAQQ X (MAKEF/EVAL X>)
  
(MAKEFILE
  (LAMBDA (FILE XFILE OPT)
          (PROG ((FNS (PACK (LIST FILE 'FNS)))
                  (VARS (PACK (LIST FILE 'VARS)))
                  (COMS (PACK (LIST FILE 'COMS)))
                  (GEN (PACK (LIST FILE 'GENNR)))
                  UNIT FILENR FNSV VARSV COMSV GENV
                  (AL (BINDENV FILE))
                  GBC DEPTH LENGTH)
                <COND <(NULL (SETQ COMSV (ERRORSET COMS NIL)))
                        (PRINTL-SP "GIVE VALUE TO VARIABLE" COMS)
                        (SET COMS (SETQ COMSV (READ>
                      (T (SETQ COMSV (CAR COMSV>
                (SETQ FNSV (CAR (ERRORSET FNS NIL)))
                (SETQ VARSV (CAR (ERRORSET VARS NIL)))
                (OR FNSV VARSV
                    (PROGN (PRINTL-SP 'BOTH FNS 'AND VARS 'EMPTY)
                           (RETURN)))
                (OR (SETQ FILENR (OPEN 20 XFILE 'NEW 'FORMATTED))
                    (PROGN (PRINTL-SP "CAN NOT OPEN" XFILE) (RETURN)))
                (COND <(CAR (ERRORSET '(NUMBERP (EVAL GEN)) NIL))
                        (SETQ GENV (ADD1 (EVAL GEN>
                      (T (SETQ GENV 0)))
                (SET GEN GENV)
                (REWIND FILENR)
                (SETQ UNIT (OUTUNIT FILENR))
                (SETQ GBC (SYSFLAG 1 NIL))
                (SETQ DEPTH (PRINTLEVEL 150))
                (SETQ LENGTH (PRINTLENGTH 1000))
                (MAKEF/OUT FILEHEADER FILE)
                (MAKEF/OUT PRINT (LIST 'QUOTE COMSV))
                (MAKEF/OUT PRINT (SUBST GENV '&&X ''(VERSION &&X)))
                (COND (FNSV (MAKEF/FNSX (LIST '* (LIST 'QUOTE FNSV)))
                            (MAKEF/OUT PRINT (LIST 'QUOTE FNS))
                            (MAKEF/OUT RPAQQ FNS FNSV)))
                (MAKEF/OUT RPAQQ COMS COMSV)
                (MAKEF/OUT RPAQ GEN GENV)
                <COND (VARSV (MAKEF/OUT PRINT (LIST 'QUOTE VARS))
                             (MAKEF/OUT RPAQQ VARS VARSV)
                             (MAPC VARSV
                                   '(LAMBDA
                                      (X)
                                      (COND ((LITATOM X)
                                              (MAKEF/VARSX (LIST X)))
                                            ((LISTP X)
                                              (SELECTQ
                                                (CAR X)
                                                (FNS (MAKEF/FNSX
                                                       (CDR X)))
                                                (VARS
                                                  (MAKEF/VARSX (CDR X)))
                                                (PROP
                                                  (MAKEF/PROPX
                                                    (CADR X)
                                                    (CDDR X)
                                                    T))
                                                (IFPROP
                                                  (MAKEF/PROPX
                                                    (CADR X)
                                                    (CDDR X)))
                                                (P (MAKEF/MAPC*
                                                     (CDR X)
                                                     'PRINT))
                                                (E (OUTUNIT UNIT)
                                                   (MAKEF/MAPC*
                                                     (CDR X)
                                                     'MAKEF/EVAL)
                                                   (OUTUNIT FILENR))
                                                (PROG1
                                                  (OUTUNIT UNIT)
                                                  (PRINTL "BAD PRETTYCOM:  " X
                                                    )
                                                  (OUTUNIT FILENR>
                (PRINT 'STOP)
                (REWIND FILENR)
                (OUTUNIT UNIT)
                (SYSFLAG 1 GBC)
                (PRINTLEVEL DEPTH)
                (PRINTLENGTH LENGTH)
                (CLOSE FILENR)
                (PRINTL-SP 'MAKEFILE FILE 'COMPLETE.)
                (RETURN FILE))))
  
(OPEN
  (LAMBDA (LFN FILE STATUS FORM)
          (COND ((XCALL 1 (LIST LFN FILE STATUS FORM)))
                ((XCALL 2 LFN) NIL))))
  
(PP
  (NLAMBDA L (PRETTYPRINT L T)))
  
(PRETTYPRINT
  (LAMBDA (L OPT POS)
          <SETQ POS (IOTAB 7 (PLUS 2 (IOTAB 7>
          (MAKEF/MAPC* L
            '(LAMBDA (X)
                     (PRIN1 "(")
                     (PRINT X)
                     (PRIN0 (VIRGINFN X) T OPT)
                     (PRINTL ")")
                     (TERPRI)
                     (PRINTPOS POS)))
          (IOTAB 7 POS)
          L))
  
(SYSIN
  (LAMBDA (FILE)
          (PROG (LFN)
                (OR (SETQ LFN (OPEN 30 FILE 'OLD 'UNFORMATTED))
                    (PROGN (PRINTL-SP "CAN NOT OPEN" FILE) (RETURN)))
                (ROLLIN LFN)
                (CLOSE LFN))))
  
(SYSOUT
  (LAMBDA (FILE)
          (PROG (LFN)
                (OR (SETQ LFN (OPEN 30 FILE 'NEW 'UNFORMATTED))
                    (PROGN (PRINTL-SP "CAN NOT OPEN" FILE) (RETURN)))
                (ROLLOUT LFN)
                (CLOSE LFN)
                (PRINTL-SP FILE 'SAVED)
                (RETURN FILE))))
  
)
(PRINT 'MAKEFFNS)
(RPAQQ MAKEFFNS
       (CLOSE CURFILE FILEHEADER LOAD MAKEF/EVAL MAKEF/FNSX MAKEF/GETPROPS 
              MAKEF/MAPC* MAKEF/OUT MAKEF/PROPX MAKEF/VARSX MAKEFILE OPEN PP 
              PRETTYPRINT SYSIN SYSOUT))
(RPAQQ MAKEFCOMS (MAKE FILE PACKAGE MODIFIED BY BLAKE MCBRIDE))
(RPAQ MAKEFGENNR 6)
(PRINT 'MAKEFVARS)
<RPAQQ MAKEFVARS (CURFILE CURLIBS SYSPROPS (P (RPAQ CURFNS>
(RPAQQ CURFILE CUR)
(RPAQQ CURLIBS (DEBUG2 EDIT CUR MAKEF DEBUG1 FUNC1 IO1 BASIC2))
(RPAQQ SYSPROPS
       (ADVISED BROKEN EDITCHAIN EDITVALUE EXPR FNCELL TRACED VIRGINFN))
(RPAQ CURFNS)
STOP
