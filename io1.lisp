(FILEHEADER IO1)
(PRINT 'IO1-PACKAGE)
(PRINT '(VERSION 2))
(DEFINEQ
(-*-
  (FSUBR . QUOTE))
  
(COM-READ
  [NLAMBDA (COMS-- CH)
           (PROG ((COMS (EVAL COMS--)))
                 (COND ((NULL COMS)
                         (SETQ CH (PROMPTTEXT CH))
                         (SETQ COMS (READ))
                         (PROMPTTEXT CH)
                         (RETURN COMS))
                       (T (SET COMS-- (CDR COMS))
                          (RETURN (CAR COMS])
  
(INUNIT
  (LAMBDA (N) (IOTAB 1 N)))
  
(OUTUNIT
  (LAMBDA (N) (IOTAB 5 N)))
  
(PRIN1
  (LAMBDA (X) (PRIN0 X)))
  
(PRIN2
  (LAMBDA (X) (PRIN0 X T)))
  
(PRIN2L-SP
  (LAMBDA L
          (MAPC L '(LAMBDA (X) (PRIN2 X) (SPACES 1)))
          (TERPRI)))
  
(PRINT
  (LAMBDA (X) (PRIN0 X T) (TERPRI) X))
  
(PRINTDEF
  (LAMBDA (X) (PRIN0 X T T) NIL))
  
(PRINTL
  (LAMBDA L (MAPC L 'PRIN1) (TERPRI)))
  
(PRINTL-SP
  (LAMBDA L
          (MAPC L '(LAMBDA (X) (PRIN1 X) (SPACES 1)))
          (TERPRI)))
  
(PRINTLENGTH
  (LAMBDA (N) (IOTAB 9 N)))
  
(PRINTLEVEL
  (LAMBDA (N) (IOTAB 10 N)))
  
(PRINTPOS
  (LAMBDA (N) (IOTAB 6 N)))
  
(READPOS
  (LAMBDA (N) (IOTAB 2 N)))
  
(SPACES
  [LAMBDA (N) (IOTAB 6 (PLUS N (IOTAB 6])
  
)
(PRINT 'IO1FNS)
(RPAQQ IO1FNS                                      (-*- COM-READ INUNIT OUTUNIT 
                                                   PRIN1 PRIN2 PRIN2L-SP PRINT 
                                                   PRINTDEF PRINTL PRINTL-SP 
                                                   PRINTLENGTH PRINTLEVEL 
                                                   PRINTPOS READPOS SPACES))
(RPAQQ IO1COMS IO1-PACKAGE)
(RPAQ IO1GENNR 3)
STOP
