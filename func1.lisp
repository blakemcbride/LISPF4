(FILEHEADER FUNC1)
(PRINT 'FUNC1-PACKAGE)
(PRINT '(VERSION 4))
(DEFINEQ
(DE
  (NLAMBDA L (PUTD (CAR L) (CONS 'LAMBDA (CDR L))) (CAR L)))
  
(DEFINE
  [LAMBDA (X)
          (COND ((NULL X) NIL)
                (T (CONS ((LAMBDA
                            (Y CY)
                            [COND [(ATOM Y)
                                    (HELP (CONS Y
                                            '(- INCORRECT DEFINING FORM]
                                  (T (COND ((GETD CY)
                                             (VIRGINFN CY T)))
                                     (PUTD CY
                                           (COND
                                             [(NULL (CDDR Y))
                                               (COND
                                                 [(EQ (CAADR Y)
                                                      'NLAMDA)
                                                   (CONS 'NLAMBDA
                                                     (CONS
                                                       (CAADR (CADR Y))
                                                       (CDDADR Y]
                                                 (T (CADR Y]
                                             (T (CONS 'LAMBDA (CDR Y]
                            (CAR Y))
                           (CAR X)
                           (CAAR X))
                         (DEFINE (CDR X])
 
(DEFUN
  (NLAMBDA L (PUTD (CAR L) (CONS 'LAMBDA (CDR L))) (CAR L)))

(DF
  (NLAMBDA L (PUTD (CAR L) (CONS 'NLAMBDA (CDR L))) (CAR L)))
  
(DM
  (NLAMBDA L
           [PUTD (CAR L)
                 (LIST 'NLAMBDA '$MACRO$
                       (LIST 'EVAL
                             (LIST (CONS 'LAMBDA
                                         (CONS
                                           (LIST (CADR L))
                                           (CDDR L)))
                                   '$MACRO$]
           (CAR L)))
  
(GETD
  (LAMBDA (FN) (AND (NULL LDFLG) ((SUBR . GETD) FN))))
  
(SAVEDEF
  [LAMBDA (A P)
          (PROG ((PR (OR P 'EXPR)) (DEF (GETD A)))
                (AND DEF (PUTPROP A PR DEF) (RETURN PR])
  
(UNSAVEDEF
  [LAMBDA (A P)
          (PROG ((PR (OR P 'EXPR)) DEF)
                (SETQ DEF (GETPROP A PR))
                (AND DEF (PUTPROP A 'FNCELL DEF) (RETURN PR])
  
(VIRGINFN
  (LAMBDA (X) (OR (GETPROP X 'VIRGINFN) ((SUBR . GETD) X))))
  
)
(PRINT 'FUNC1FNS)
(RPAQQ FUNC1FNS (DE DEFINE DEFUN DF DM GETD SAVEDEF UNSAVEDEF VIRGINFN))
(RPAQQ FUNC1COMS FUNC1-PACKAGE)
(RPAQ FUNC1GENNR 5)
(PRINT 'FUNC1VARS)
[RPAQQ FUNC1VARS
       ((P (RPAQ CURFNS NIL)
           (RPAQ CURFILEFNS 'CURFNS)
           (PUTD 'PUTD
                 '(LAMBDA (FN DEF)
                          (COND ((GETD FN)
                                  (SAVEDEF FN)
                                  (PRINTL-SP FN 'REDEFINED)))
                          (PUTPROP FN 'FNCELL DEF)
                          (SET CURFILEFNS
                               (ADDLIST FN (EVAL CURFILEFNS)))
                          DEF))
           (RPAQ LDFLG]
(RPAQ CURFNS NIL)
(RPAQ CURFILEFNS 'CURFNS)
(PUTD 'PUTD '(LAMBDA (FN DEF) (COND ((GETD FN) (SAVEDEF FN) (PRINTL-SP FN 
'REDEFINED))) (PUTPROP FN 'FNCELL DEF) (SET CURFILEFNS (ADDLIST FN (EVAL 
CURFILEFNS))) DEF))
(RPAQ LDFLG)
STOP
