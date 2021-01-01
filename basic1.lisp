'BASIC1-PACKAGE
(MAPC '(1 2 3 4 5 6 7) '(LAMBDA (X) (SYSFLAG X T]
(IOTAB 4 150)
(IOTAB 8 78)
[PUTPROP 'PUTD 'FNCELL '(LAMBDA (FN DEF) (PUTPROP FN 'FNCELL DEF]
[PUTD 'DEFINEQ '(NLAMBDA L (MAPC L '(LAMBDA (X) (PUTD (CAR X) (CADR X]
[PUTD 'FILEHEADER '(NLAMBDA (A) (PRINT (LIST 'FILEHEADER A]
[PUTD 'READFILE
      '(LAMBDA (FILE)
               (PROG (&&X LFN TEMP)
                     (OR (SETQ LFN (XCALL 1 (LIST 15 FILE 'OLD 'FORMATTED)))
                         (RETURN))
                     (SETQ TEMP (IOTAB 1 LFN))
                LOOP (SELECTQ (SETQ &&X (READ))
                              (STOP (XCALL 2 LFN) (RETURN (IOTAB 1 TEMP)))
                              (EVAL &&X))
                     (GO LOOP]
(PUTD 'PRINT '(LAMBDA (A) (PRIN0 A T) (TERPRI) A))
