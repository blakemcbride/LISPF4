(FILEHEADER DEBUG2)
(PRINT '(DEBUG PACKAGE 2))
(PRINT '(VERSION 0))
(DEFINEQ
(ADDINNAME
  [LAMBDA (F)
          (COND ((NLISTP F) F)
                ((EQ (CADR FN) 'IN)
                  (PROG (NEW (FN (CAR F)) (IFN (CADDR F)) ND)
                        (OR (GETD FN) (RETURN))
                        (SETQ NEW (INNAME FN IFN))
                        (COND ((MEMB FN (GETPROP IFN 'NAMESCHANGED))
                                (RETURN NEW)))
                        (SETQ ND (SUBST NEW FN (GETD IFN)))
                        (AND (EQUAL ND (GETD IFN)) (RETURN))
                        (OR (GETPROP IFN 'VIRGINFN)
                            (PUTPROP IFN 'VIRGINFN (GETD IFN)))
                        (PUTPROP IFN 'FNCELL ND)
                        (ADDPROP IFN 'NAMESCHANGED FN)
                        (PUTPROP NEW 'FNCELL (GETD FN))
                        (PUTPROP NEW 'ALIAS (CONS IFN FN))
                        (RETURN NEW])
  
(ADVISE
  (LAMBDA (FN . L)
          (PROG (WHEN WHAT
                      (BR (UNBREAK1 FN))
                      [NEW (GETD (SETQ FN (ADDINNAME FN]
                      (TMP (GETPROP FN 'ADVISED)))
                (OR NEW (RETURN NIL))
                (OR L (RETURN NIL))
                (OR (GETPROP FN 'VIRGINFN)
                    (PUTPROP FN 'VIRGINFN NEW))
                [OR TMP (PUTPROP FN 'ADVISED (SETQ TMP (CONS]
                [COND ((CAR TMP))
                      (T (RPLACA TMP NEW)
                         (SETQ NEW (ADVISE-BODY NEW]
                (NCONC1 TMP L)
                (SETQ WHAT (CAR (LAST L)))
                (SETQ WHEN (COND ((CDR L) (CAR L)) ('BEFORE)))
                (PUTPROP FN 'FNCELL NEW)
                (SETQ NEW (CADDR NEW))
                (SETQ TMP
                      (SELECTQ WHEN
                        (BIND (CADR NEW))
                        (AFTER (CADDR NEW) (CAR (NTH NEW 5)))
                        (CADDR NEW)))
                (AND WHAT (NCONC1 TMP WHAT))
                (SETQ ADVISEDFNS (ADDLIST FN ADVISEDFNS))
                (RETURN FN))))
  
(ADVISE-BODY
  (LAMBDA (FN)
          (PROG ((BDY (LAMBDAEQV FN)) FORM)
                (SETQ FORM (CDDR BDY))
                [RPLACA FORM
                        (SUBST (CAR FORM)
                               'FORM
                               '(PROG (!VALUE)
                                      (PROGN)
                                      (SETQ !VALUE FORM)
                                      (PROGN)
                                      (RETURN !VALUE]
                (RETURN BDY))))
  
(BREAK
  [NLAMBDA L (MAPCAR L '(LAMBDA (FN) (BREAK0 FN T])
  
(BREAK-BODY
  (LAMBDA (NEW)
          (PROG ((BDY (LAMBDAEQV NEW)))
                (RPLACA (CDDR BDY)
                        (LIST 'BREAK1 (CADDR BDY) BRKWHEN FN BRKCOMS))
                (RETURN BDY))))
  
(BREAK0
  (LAMBDA (FN BRKWHEN BRKCOMS)
          (SETQ FN (ADDINNAME FN))
          (PROG ((NEW (GETD FN)) (TMP (GETPROP FN 'BROKEN)))
                (OR NEW (RETURN NIL))
                (OR (GETPROP FN 'VIRGINFN)
                    (PUTPROP FN 'VIRGINFN NEW))
                (COND ((NULL TMP)
                        (PUTPROP FN 'BROKEN (LIST NEW BRKWHEN BRKCOMS)))
                      ((NULL (CAR TMP)) (RPLACA TMP NEW))
                      (T (RETURN NIL)))
                (SETQ NEW (BREAK-BODY NEW))
                (PUTPROP FN 'FNCELL NEW)
                (SETQ BROKENFNS (ADDLIST FN BROKENFNS))
                (RETURN FN))))
  
(INNAME
  (LAMBDA (F IN) (PACK (LIST F '-IN- IN))))
  
(LAMBDAEQV
  [LAMBDA (NEW)
          (SELECTQ (CAR NEW)
                   (FUNARG (LIST (CAR NEW)
                                 (LAMBDAEQV (CADR NEW))
                                 (CADDR NEW)))
                   [(LAMBDA NLAMBDA)
                     (LIST (CAR NEW)
                           (CADR NEW)
                           (COND ((CDDDR NEW)
                                   (CONS 'PROGN (CDDR NEW)))
                                 ((CADDR NEW]
                   (LIST (COND ((EQ (CAR NEW) 'SUBR) 'LAMBDA)
                               (T 'NLAMBDA))
                         'ARGS
                         (LIST 'APPLY (LIST 'QUOTE NEW) 'ARGS])
  
(PUTD
  (LAMBDA (FN DEF)
          (COND ((GETD FN) (SAVEDEF FN) (PRIN2L-SP FN 'REDEFINED)))
          (REMPROP FN 'TRACED)
          (REMPROP FN 'VIRGINFN)
          [COND ((GETPROP FN 'ADVISED)
                  (RPLACA (GETPROP FN 'ADVISED]
          [COND ((GETPROP FN 'BROKEN)
                  (RPLACA (GETPROP FN 'BROKEN]
          (PUTPROP FN 'FNCELL DEF)
          (SET CURFILEFNS (ADDLIST FN (EVAL CURFILEFNS)))
          DEF))
  
(READVISE
  [NLAMBDA L
           (MAPCAR (OR L ADVISEDFNS)
                   '(LAMBDA (FN)
                            (SETQ FN (THEINNAME FN))
                            (PROG ((OLD (GETPROP FN 'ADVISED)))
                                  (COND ((AND OLD (NULL (CAR OLD)))
                                          (PUTPROP FN 'ADVISED)
                                          [MAPC
                                            (CDR OLD)
                                            '(LAMBDA
                                               (ADV)
                                               (APPLY 'ADVISED
                                                 (CONS FN ADV]
                                          (RETURN FN))
                                        (T (RETURN NIL])
  
(REBREAK
  (NLAMBDA L (MAPCAR (OR L BROKENFNS) 'REBREAK1)))
  
(REBREAK1
  [LAMBDA (FN)
          (SETQ FN (ADDINNAME FN))
          (PROG ((OLD (GETPROP FN 'BROKEN)))
                (COND ((AND OLD (NULL (CAR OLD)))
                        (BREAK0 FN (CADR OLD) (CADDR OLD))
                        (RETURN FN))
                      (T (RETURN NIL])
  
(REMINNAME
  [LAMBDA (FN TEMP)
          (COND ((NLISTP FN)
                  [COND ((SETQ ALIAS (GETPROP FN 'ALIAS))
                          [PUTPROP
                            (CAR ALIAS)
                            'FNCELL
                            (SUBST (CDR ALIAS)
                                   (INNAME (CDR ALIAS) (CAR ALIAS))
                                   (GETD (CAR ALIAS]
                          (PUTPROP
                            (CAR ALIAS)
                            'NAMESCHANGED
                            (REMOVE
                              (CDR ALIAS)
                              (GETPROP (CAR ALIAS) 'NAMESCHANGED]
                  FN)
                ((EQ (CADR FN) 'IN)
                  (REMINNAME (INNAME (CAR FN) (CADDR FN])
  
(THEINNAME
  [LAMBDA (X)
          (COND ((NLISTP X) X)
                ((EQ (CADR X) 'IN) (INNAME (CAR X) (CADDR X])
  
(TRACE
  [NLAMBDA L
           (MAPCAR L
                   '(LAMBDA (FN IFN)
                            (SETQ IFN (THEINNAME FN))
                            (COND ((GETPROP IFN 'TRACED) NIL)
                                  (T (OR (ADVISE FN
                                           (LIST 'TRACE-PRINT
                                             (KWOTE IFN)
                                             T))
                                         (RETURN))
                                     (PUTPROP IFN 'TRACED T)
                                     (ADVISE FN 'BIND
                                       '(*TRACEN (ADD1 *TRACEN)))
                                     (ADVISE FN 'AFTER
                                       (LIST 'TRACE-PRINT (KWOTE IFN])
  
(TRACE-PRINT
  [LAMBDA (FN FLG)
          (AND (GREATERP *TRACEN 15) (SETQ *TRACEN -1))
          (RPTQ *TRACEN (PROGN (PRIN1 'I) (SPACES 2)))
          (PRINTLEVEL
            (PRINTLEVEL *PRINTLEVEL)
            (COND [FLG (PRIN1 "--]")
                       (SPACES 1)
                       (PRIN2 FN)
                       (SPACES 1)
                       (PRINT (PRVARS (*ARGLIST FN) (BINDENV FN]
                  (T (PRIN1 "[--")
                     (SPACES 1)
                     (PRIN2 FN)
                     (SPACES 1)
                     (PRINT !VALUE])
  
(UNADVISE
  (NLAMBDA L (MAPCAR (OR L ADVISEDFNS) 'UNADVISE1)))
  
(UNADVISE1
  (LAMBDA (FN)
          (SETQ FN (THEINNAME FN))
          (PROG ((OLD (GETPROP FN 'ADVISED)) (BR (UNBREAK1 FN)))
                (OR OLD (RETURN NIL))
                (OR (CAR OLD) (RETURN NIL))
                (PUTPROP FN 'FNCELL (CAR OLD))
                (RPLACA OLD NIL)
                (COND (BR (REBREAK1 FN)) ((REMINNAME FN)))
                (RETURN FN))))
  
(UNBREAK
  (NLAMBDA L (MAPCAR (OR L BROKENFNS) 'UNBREAK1)))
  
(UNBREAK1
  (LAMBDA (FN)
          (SETQ FN (THEINNAME FN))
          (PROG ((OLD (GETPROP FN 'BROKEN)))
                (OR OLD (RETURN NIL))
                (OR (CAR OLD) (RETURN NIL))
                (PUTPROP FN 'FNCELL (CAR OLD))
                (RPLACA OLD NIL)
                (REMINNAME FN)
                (RETURN FN))))
  
(UNTRACE
  [NLAMBDA L
           (MAPCAR [OR L
                       (REMOVE NIL
                         (MAPCAR ADVISEDFNS
                           '(LAMBDA
                              (X)
                              (AND (GETPROP X 'TRACED) X]
                   '(LAMBDA (FN)
                            (REMPROP FN 'TRACED)
                            (UNADVISE1 FN])
  
)
(PRINT 'DEBUG2FNS)
(RPAQQ DEBUG2FNS
       (ADDINNAME ADVISE ADVISE-BODY BREAK BREAK-BODY BREAK0 INNAME LAMBDAEQV 
         PUTD READVISE REBREAK REBREAK1 REMINNAME THEINNAME TRACE TRACE-PRINT 
         UNADVISE UNADVISE1 UNBREAK UNBREAK1 UNTRACE))
(RPAQQ DEBUG2COMS (DEBUG PACKAGE 2))
(RPAQ DEBUG2GENNR 0)
(PRINT 'DEBUG2VARS)
[RPAQQ DEBUG2VARS
       ((P (RPAQ BROKENFNS) (RPAQ *BRKFLG))
         (P (RPAQ ADVISEDFNS)
            (RPAQ !ADVISEFLG)
            (RPAQ *TRACEN -1)
            (RPAQ *PRINTLEVEL 3]
(RPAQ BROKENFNS)
(RPAQ *BRKFLG)
(RPAQ ADVISEDFNS)
(RPAQ !ADVISEFLG)
(RPAQ *TRACEN -1)
(RPAQ *PRINTLEVEL 3)
STOP
