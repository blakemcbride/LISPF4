(FILEHEADER DEBUG1)
(PRINT 'DEBUG1-PACKAGE)
(PRINT '(VERSION 5))
(DEFINEQ
(HELP
  (LAMBDA (HELPX HELPY)
          (PROG NIL
                (TERPRI T)
                (PRIN1 "HELP:" T)
                (TERPRI T)
                (PRIN1 HELPX T)
                (TERPRI T)
                (PRIN1 HELPY T)
           LOOP (NLSETQ (PROGN (TERPRI T)
                               (PRIN1 '":" T)
                               (PRINT (EVAL (READ T)) T)))
                (GO LOOP))))
  
(ERSETQ
  (NLAMBDA (ERSETX) (ERRORSET ERSETX T)))
  
(NLSETQ
  (NLAMBDA (NLSETX) (ERRORSET NLSETX NIL)))
  
(*ARGLIST
  (LAMBDA (X) (CADR (GETD X))))
  
(BREAK-BT
  (LAMBDA (AL BTV)
          (PRINTLEVEL
            (PRINTLEVEL *PRINTLEVEL)
            (MAPC (BREAK-SKIP1 (ALIST T)) 'BT-PRINT 'BREAK-SKIP))))
  
(BREAK-EVAL
  [LAMBDA (BREAK-FORM)
          (PROG ((*BRKFLG NIL) BREAK-EVAL)
                (RETURN (COND (*BRKEVAL !VALUE)
                              ((OR (NLISTP BREAK-FORM)
                                   (CAR BREAK-FORM))
                                (EVAL BREAK-FORM))
                              ((APPLY
                                 (CADR BREAK-FORM)
                                 (CDDR BREAK-FORM])
  
(BREAK-SKIP
  [LAMBDA (X)
          (PROG NIL
           LOOP (RETURN (AND X
                             (SELECTQ
                               (CAADR X)
                               (BREAK-EVAL (BREAK-SKIP1 X))
                               (*FORM (CDR X))
                               (COND (BTV (CDR X))
                                     (T (SETQ X (CDR X)) (GO LOOP])
  
(BREAK-SKIP1
  [LAMBDA (X)
          (PROG NIL
           LOOP (COND ((NULL (SETQ X (CDR X))) (RETURN NIL))
                      ((EQ (CAAR X) '*FORM)
                        (SELECTQ
                          (CADAR X)
                          [(BREAK11 SYSERROR)
                            (RETURN (CONS '**BREAK** (CDR X]
                          (GO LOOP)))
                      (T (GO LOOP])
  
(BREAK1
  (NLAMBDA L (APPLYSTK 'BREAK11 L (BINDENV L))))
  
(BREAK11
  [LAMBDA (BRKEXP BRKWHEN BRKFN BRKCOMS SYSERROR)
          ((FSUBR . COND)
            (*BRKFLG ((SUBR . EVAL) BRKEXP))
            (((FSUBR . PROG)
               ((*BR1 ((SUBR . IOTAB) 1 T))
                 (*BR2 ((SUBR . IOTAB) 5 T))
                 (*BRKFLG T))
               (RETURN (PROG (COM *BRKEVAL
                                  (*BRKFLG T)
                                  (AL (BINDENV BRKEXP))
                                  !VALUE)
                             (OR (BREAK-EVAL BRKWHEN)
                                 (RETURN (BREAK-EVAL BRKEXP)))
                             (IOTAB 2 (IOTAB 4))
                        BREAK11LOOP
                             (PRINT (LIST BRKFN 'BROKEN))
                        L    (SETQ COM (COM-READ BRKCOMS :))
                             [SELECTQ COM
                               [?= (OR SYSERROR
                                       (PRVARS1
                                         (*ARGLIST BRKFN)
                                         (PRVARS (*ARGLIST BRKFN) AL]
                               (! (GO* BREAKOUT))
                               (OK (RETURN (BREAK-EVAL BRKEXP)))
                               [GO (RETURN
                                     (PRINT
                                       (BREAK-EVAL BRKEXP)
                                       (PRIN2 BRKFN)
                                       (PRIN1 " = "]
                               [RETURN
                                 (SETQ *BRKEVAL NIL)
                                 (RETURN
                                   (BREAK-EVAL (COM-READ BRKCOMS :]
                               (EVAL (SETQ *BRKEVAL NIL)
                                     (SETQ !VALUE (BREAK-EVAL BRKEXP))
                                     (PRINTL-SP BRKFN 'EVALUATED)
                                     (SETQ *BRKEVAL T))
                               (!EVAL
                                 (SETQ BRKCOMS
                                       (APPEND '(UB EVAL BR) BRKCOMS)))
                               (!OK (SETQ BRKCOMS
                                          (APPEND '(!EVAL OK) BRKCOMS)))
                               (!GO (SETQ BRKCOMS
                                          (APPEND '(!EVAL GO) BRKCOMS)))
                               (UB (UNBREAK1 BRKFN))
                               (BR (REBREAK1 BRKFN))
                               (BT (BREAK-BT AL))
                               (BTV (BREAK-BT AL T))
                               ((LAMBDA *BRKEVAL
                                  (PRINT (BREAK-EVAL COM]
                             (GO L))
                       (IOTAB 1 *BR1)
                       (IOTAB 5 *BR2))
               BREAKOUT
               (GO* BREAK11LOOP)
               (RESET])
  
(BT-PRINT
  [LAMBDA (X)
          (COND ((NLISTP X) (PRINT X))
                ((SELECTQ
                   (CAR X)
                   [*FORM (PRIN2L-SP
                            (CAR X)
                            (SELECTQ
                              (CADR X)
                              (PROG '*PROG)
                              (CDR X]
                   (PRIN2L-SP (CAR X) '= (CDR X])
  
(ERRORB
  (LAMBDA NIL (GO* ERRORSET) (RESET)))
  
(ERRORN
  (LAMBDA NIL (CAR (ERRORSET 'ERRTYPE))))
  
(ERRORSET
  (LAMBDA (ERRFORM ERRFLG)
          (PROG NIL
                (RETURN (LIST (EVAL ERRFORM)))
           ERRORSET
                (RETURN NIL))))
  
(KWOTE
  (LAMBDA (X) (LIST 'QUOTE X)))
  
(PRVARS
  [LAMBDA (L AL)
          (COND ((NULL L) NIL)
                ((ATOM L) (EVSTK L AL))
                (T (CONS (EVSTK (CAR L) AL) (PRVARS (CDR L) AL])
  
(PRVARS1
  [LAMBDA (VARS VALS)
          (COND ((NULL VARS) NIL)
                ((NLISTP VARS) (PRIN2L-SP VARS '= VALS))
                (T (PRIN2L-SP (CAR VARS) '= (CAR VALS))
                   (PRVARS1 (CDR VARS) (CDR VALS])
  
(SYSERROR
  (LAMBDA (ERRTYPE FN ARG FORM *BRKFLG)
          [COND (ERRFLG (OUTUNIT
                          (OUTUNIT T)
                          (ERRORMESS ERRTYPE)
                          (PRIN2L-SP FN '- ARG]
          (GO* ERRORSET)
          (BREAK11 FORM T FN NIL T)))
  
)
(PRINT 'DEBUG1FNS)
(RPAQQ DEBUG1FNS
       (ABC HELP ERSETQ NLSETQ *ARGLIST BREAK-BT BREAK-EVAL BREAK-SKIP 
            BREAK-SKIP1 BREAK1 BREAK11 BT-PRINT ERRORB ERRORN ERRORSET KWOTE 
            PRVARS PRVARS1 SYSERROR))
(RPAQQ DEBUG1COMS DEBUG1-PACKAGE)
(RPAQ DEBUG1GENNR 5)
(PRINT 'DEBUG1VARS)
[RPAQQ DEBUG1VARS
       ((P (RPAQ *PRINTLEVEL 3)
           (RPAQ *BRKFLG NIL)
           (RPAQ ERRFLG T]
(RPAQ *PRINTLEVEL 3)
(RPAQ *BRKFLG NIL)
(RPAQ ERRFLG T)
STOP
