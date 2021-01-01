(FILEHEADER PROLOG)
(PRINT '(PROLOG: MODIFIED BY BLAKE MCBRIDE))
(PRINT '(VERSION 2))
(DEFINEQ
(PROVE
  [LAMBDA (GS)
          (RESET* NIL NIL)
          (SEEK (LIST (ALLOCATE)) (LIST (CAR GS])
  
(ULT
  [LAMBDA (M X)
          (COND ((OR (ATOM X) (NULL (EQ (CAR X) '?))) X)
                ((LESSP (CADR X) 7)
                  [PROG ((M X)) (FINAL (IMM M (CADR X]
                  X)
                ((DO UNTIL
                     (LESSP X 7)
                     RET
                     (PROGN (PROG ((M X)) (FINAL (IMM M X)))
                            (RETURN X))
                     LVARS
                     ((X (CADR X)))
                     DO
                     (SETQ X (MINUS X 6))
                     (SETQ M
                           (OR (IMM M 7)
                               (IMM (SETIMM M 7 (ALLOCATE)) 7])
  
(POP
  [LAMBDA (S) (PROG1 (CAR S) (SETQ S (CDR S])
  
(PUSH
  (LAMBDA (V S) (SETQ S (CONS V S))))
  
(SEEK
  [LAMBDA (E C)
          (DO WHILE (AND C (NULL (CAR C))) DO (POP E) (POP C))
          (COND ((NULL C) (FUNCALL *TOPFUN*))
                ((ATOM (CAR C)) (FUNCALL (CAR C) E (CDR C)))
                ((DO])
  
(UNIFY
  [LAMBDA (M X N Y)
          (DO WHILE T DO
              (COND ((AND (EQ (ULT M X) (ULT N Y)) (EQ M N))
                      (RETURN T))
                    ((NULL M) (RETURN (BIND X Y N)))
                    ((NULL N) (RETURN (BIND Y X M)))
                    ((OR (ATOM X) (ATOM Y))
                      (RETURN (EQUAL X Y)))
                    ((NULL (UNIFY M (POP X) N (POP Y)))
                      (RETURN NIL])
  
(CATCH-CUT
  (LAMBDA (V E)
          (AND (NULL (AND (EQ (CAR V) 'CUT) (EQ (CDR V) E)))
               V)))
  
(FINAL
  (LAMBDA (X) (CDR (MEMQ NIL X))))
  
(BIND
  [LAMBDA (X Y N)
          (COND (N (PUSH X *E*) (RPLACD X (CONS N Y)))
                (T (PUSH X *E*) (RPLACD X Y) (RPLACA X *FORWARD])
  
(UNBIND
  (LAMBDA (OE)
          (DO UNTIL
              (EQ *E* OE)
              LVARS
              (X)
              DO
              (SETQ X (CAR *E*))
              (RPLACA X NIL)
              (RPLACD X X)
              (POP *E*))))
  
(RESET*
  (LAMBDA (E N) (UNBIND E) (DEALLOCATE N) NIL))
  
(DEALLOCATE
  (LAMBDA (ON) (DO UNTIL (EQ *N* ON) DO (TRANSFER *N* *FH*))))
  
(ALLOCATE
  [LAMBDA NIL
          (COND (*FH* (TRANSFER *FH* *N*) (SETIMM *N* 7 NIL))
                ((SETQ *N* (SETIMM (MAKRECORD 8) 0 *N*])
  
(TRANSFER
  [LAMBDA (X Y)
          (SETQ X (PROG1 (IMM X 0) (SETQ Y (SETIMM X 0 Y])
  
(MAKRECORD
  [LAMBDA (N)
          (DO FOR END
              (MINUS N 2)
              RET R VAR I LVARS
              ((R (MAKHUNK N)) C)
              DO
              (SETQ C (CONS NIL NIL))
              (SETIMM R I (RPLACD C C])
  
(SETIMM
  (LAMBDA (M X V) (RPLACX X M V)))
  
(IMM
  (LAMBDA (M X) (CXR X M)))
  
(RPLACX
  (LAMBDA (N H Z) (RPLACA (NTH H N) Z) H))
  
(CXR
  (LAMBDA (N H) (CAR (NTH H N))))
  
(MAKHUNK
  (LAMBDA (N) (DO FOR END N RET LIST)))
  
)
(PRINT 'PROLOGFNS)
(RPAQQ PROLOGFNS
       (PROVE ULT POP PUSH SEEK UNIFY CATCH-CUT FINAL BIND UNBIND RESET* 
              DEALLOCATE ALLOCATE TRANSFER MAKRECORD SETIMM IMM RPLACX CXR 
              MAKHUNK))
(RPAQQ PROLOGCOMS (PROLOG: MODIFIED BY BLAKE MCBRIDE))
(RPAQ PROLOGGENNR 2)
(PRINT 'PROLOGVARS)
(RPAQQ PROLOGVARS (*E* *FH* *N* *INF* *FORWARD* *TOPFUN*))
(RPAQQ *E* NIL)
(RPAQQ *FH* NIL)
(RPAQQ *N* NIL)
(RPAQQ *INF* 0)
(RPAQQ *FORWARD* NIL)
[RPAQQ *TOPFUN*
       (LAMBDA NIL (PRIN1 "MORE? ") (AND (NULL (READ)) '(TOP]
STOP
