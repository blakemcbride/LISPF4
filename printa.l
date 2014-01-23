(FILEHEADER PRINTA)
(PRINT '(ARRAY PRINT FUNCTION WRITTEN BY BLAKE MCBRIDE))
(PRINT '(VERSION 0))
(DEFINEQ
(PRINTA
  <NLAMBDA (A)
           (PROG ((V (EVSTK A (BINDENV A))) B)
                 (SETQ B (ARRAYSIZE V))
                 (AND (ARRAYP V)
                      (PROGN <RPLACA B
                               (DIFFERENCE
                                 (CAR B)
                                 (PLUS (CADR B) (CADDR B>
                             (PRINTA1 A 'SETI 'ELTI (CADR B) V)
                             (PRINTA1 A 'SETR 'ELTR (CADDR B) V)
                             (PRINTA1 A 'SETA 'ELT (CAR B) V>)
  
(PRINTA1
  <LAMBDA (A S F E V)
          (DO FOR END E VAR I DO
              (PRINT (LIST S A I (LIST 'QUOTE (EVAL (LIST F V I>)
  
)
(PRINT 'PRINTAFNS)
(RPAQQ PRINTAFNS (PRINTA PRINTA1))
(RPAQQ PRINTACOMS (ARRAY PRINT FUNCTION WRITTEN BY BLAKE MCBRIDE))
(RPAQ PRINTAGENNR 0)
STOP
