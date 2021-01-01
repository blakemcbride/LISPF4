(FILEHEADER EDIT)
(PRINT 'EDIT-PACKAGE)
(PRINT '(VERSION 4))
(DEFINEQ
(EDFIND1ST
  [LAMBDA (A S TRC)
          (PROG (TEMP)
                (COND ((SETQ TEMP (MEMB A (CDR S)))
                        (RETURN (CONS TEMP TRC)))
                      ((SETQ TEMP
                             (EDFIND1ST2 A
                               (COND ((EQUAL (CAR S) A) (CDR S))
                                     (S))
                               TRC))
                        (RETURN TEMP)))
           LOOP (SETQ S (CAR TRC))
                (SETQ TRC (CDR TRC))
                (COND ((NULL TRC) (RETURN))
                      ((SETQ TEMP
                             (EDFIND1ST2 A
                               (CDR (MEMB S (CAR TRC)))
                               TRC))
                        (RETURN TEMP))
                      ((GO LOOP])
  
(EDFIND1ST2
  [LAMBDA (A S TRC)
          (PROG (RES)
           LOOP (COND ((NLISTP S) (RETURN))
                      ((EQUAL A (CAR S))
                        (RETURN (CONS (CAR S) TRC)))
                      ((SETQ RES
                             (EDFIND1ST2 A
                               (CAR S)
                               (CONS (CAR S) TRC)))
                        (RETURN RES))
                      ((SETQ S (CDR S)) (GO LOOP])
  
(EDITF
  (NLAMBDA (FN . L)
           (PROG ((ESF (GETPROP FN 'EDIT-SAVE))
                   (VF (VIRGINFN FN))
                   RESULT EXIT-TYPE FN-COPY)
                 (SETQ FN-COPY (COPY (OR ESF VF)))
                 (SETQ RESULT (EDITS-INT (OR ESF VF) FN-COPY L))
                 (SELECTQ EXIT-TYPE
                   (OK (REMPROP FN 'EDIT-SAVE) (PUTD FN RESULT))
                   (SAVE (PUTPROP FN 'EDIT-SAVE RESULT)
                         (PUTD FN FN-COPY))
                   [STOP (COND (ESF (PUTPROP FN 'EDIT-SAVE RESULT))
                               (T (PUTD FN RESULT]
                   NIL)
                 (RETURN FN))))
  
(EDITP
  (NLAMBDA (A . L)
           (PROG (EXIT-TYPE VAL)
                 (SETQ VAL (EDITS-INT (GETPROPLIST A) L))
                 (AND (EQ EXIT-TYPE 'STOP) (SETPROPLIST A VAL))
                 (RETURN VAL))))
  
(EDITPR
  [LAMBDA (X PP DEPTH)
          (PRINTLEVEL
            (PRINTLEVEL DEPTH)
            (COND ((TAILP CL (CADR CTLS))
                    (PRIN1 "--- ")
                    [MAPC X
                          '(LAMBDA
                             (Y)
                             (COND (PP (PRIN0 Y T 0)
                                       (TERPRI)
                                       (SPACES 4))
                                   (T (PRIN2 Y) (SPACES 1]
                    (PRINTL '%)))
                  (T (COND (PP (PRIN0 X T 0) (TERPRI))
                           (T (PRINT X])
  
(EDITS-INT
  (LAMBDA (S S-OLD EDCOM)
          (PROG (CL CTLS TEMP X A B L)
                (AND (NLISTP S)
                     (PRINT "NOT EDITABLE")
                     (SETQ EXIT-TYPE 'CANT-EDIT)
                     (RETURN NIL))
                [AND EDCOM (SETQ EDCOM (APPEND EDCOM '(OK]
           START
           B    (SETQ CL S)
                (SETQ CTLS (LIST CL))
           NEXT (SETQ L (COM-READ EDCOM *))
                (COND ((ATOM L) (GO ATOML)))
                (SETQ X (CAR L))
                (SETQ L (CDR L))
                [COND ((NUMBERP X) (GO NUMCARX))
                      ((GETPROP X 'DEEDITL)
                        (SETQ EDCOM
                              (APPEND
                                (APPLY (GETPROP X 'DEEDITL) (LIST L))
                                EDCOM))
                        (GO NEXT))
                      (T (SELECTQ X
                           (R (DSUBST (CADR L) (CAR L) CL))
                           (N (NCONC CL L))
                           [US (COND ((SETQ TEMP
                                            (COPY
                                              (GETPROP
                                                (CAR L)
                                                'EDITVALUE)))
                                       (SETQ EDCOM
                                             (APPEND
                                               (SUBST TEMP
                                                 (CAR L)
                                                 (CDR L))
                                               EDCOM)))
                                     (T (GO ILLG]
                           (MARK (PUTPROP (CAR L) 'EDITCHAIN CTLS))
                           [\ (COND ((AND (LITATOM (CAR L))
                                          (SETQ TEMP
                                            (GETPROP
                                              (CAR L)
                                              'EDITCHAIN)))
                                      (SETQ CL (CAR TEMP))
                                      (SETQ CTLS TEMP))
                                    (T (GO ILLG]
                           [: (SETQ EDCOM
                                    (CONS 'UP
                                          (CONS (CONS 1 L) EDCOM]
                           [MBD (SETQ EDCOM
                                      (CONS (CONS ': (SUBST CL '* L))
                                            (CONS 1 EDCOM]
                           (XTR (SETQ EDCOM
                                      (APPEND
                                        [CONS
                                          '(MARK LISPF4-XTR)
                                          (APPEND L
                                            '(S LISPF4-XTR
                                                (\ LISPF4-XTR)
                                                (US LISPF4-XTR
                                                    (: LISPF4-XTR]
                                        EDCOM)))
                           (B (SETQ EDCOM
                                    (APPEND
                                      (LIST 'UP (CONS -1 L))
                                      EDCOM)))
                           (A (SETQ EDCOM
                                    (APPEND
                                      (LIST 'UP (CONS -2 L))
                                      EDCOM)))
                           [ESET (COND (L (COND
                                            ((LITATOM (CAR L))
                                              (COND
                                                [(CADR L)
                                                  (PUTPROP
                                                    (CAR L)
                                                    'DEEDITA
                                                    (LIST 'QUOTE
                                                      (CDR L]
                                                (T (REMPROP
                                                     (CAR L)
                                                     'DEEDITA]
                           (GO CONT]
                (GO NEXT)
           CONT (COND ((OR (NULL (NUMBERP (CAR L)))
                           (LESSP (CAR L) 1))
                        (GO ILLG)))
                (OR (SETQ TEMP (NTH CL (CAR L))) (GO EMPTY))
                (SELECTQ X
                  (LO (EDSMASH TEMP (CAAR TEMP) (CDAR TEMP)))
                  (LI (EDSMASH TEMP
                        (CONS (CAR TEMP) (CDR TEMP))
                        NIL))
                  (RO (NCONC (CAR TEMP) (CDR TEMP))
                      (RPLACD TEMP NIL))
                  (RI (OR (NUMBERP (CADR L)) (GO ILLG))
                      (SETQ A (NTH (CAR TEMP) (CADR L)))
                      (OR (CDR A) (GO EMPTY))
                      (RPLACD TEMP (NCONC (CDR A) (CDR TEMP)))
                      (RPLACD A NIL))
                  [BO (EDSMASH TEMP
                        (CAAR TEMP)
                        (NCONC (CDAR TEMP) (CDR TEMP]
                  (BI [SETQ B
                            (CDR (SETQ A
                                       (COND ((NULL
                                                (NUMBERP (CADR L)))
                                               TEMP)
                                             (T (NTH CL (CADR L]
                      (RPLACD A NIL)
                      (EDSMASH TEMP (CONS (CAR TEMP) (CDR TEMP)) B))
                  (GO ILLG))
                (GO NEXT)
           NUMCARX
                [COND ((ZEROP X) (GO ILLG))
                      [(LESSP X 0)
                        (COND ((EQ X -1)
                                [SETQ L
                                      (NCONC L
                                        (CONS (CAR CL) (CDR CL]
                                (EDSMASH CL (CAR L) (CDR L)))
                              ([NLISTP
                                 (SETQ A (NTH CL (MINUS (ADD1 X]
                                (GO EMPTY))
                              (T (RPLACD A (NCONC L (CDR A]
                      [(EQ X 1)
                        (COND [L (EDSMASH CL
                                   (CAR L)
                                   (NCONC (CDR L) (CDR CL]
                              ((NLISTP CL) (GO EMPTY))
                              ((NLISTP (CDR CL))
                                (SETQ TEMP (LENGTH (CADR CTLS)))
                                (SETQ EDCOM
                                      (NCONC
                                        (LIST 0
                                          (SELECTQ TEMP
                                            (1 '(1 NIL))
                                            (CONS TEMP)))
                                        EDCOM)))
                              (T (EDSMASH CL (CADR CL) (CDDR CL]
                      ([NLISTP (SETQ A (NTH CL (SUB1 X]
                        (GO EMPTY))
                      (T (RPLACD A
                           (COND ((CDR A) (NCONC L (CDDR A)))
                                 (T L]
                (GO NEXT)
           ATOML
                (SETQ X L)
                [COND [(NUMBERP X)
                        (COND ((ZEROP X)
                                (OR (CDR CTLS) (GO TOP))
                                (SETQ CTLS (CDR CTLS))
                                (SETQ CL (CAR CTLS)))
                              (T (AND (LESSP X 0)
                                      (SETQ X
                                            (PLUS (LENGTH CL) 1 X)))
                                 (SETQ X (NTH CL X))
                                 (OR (LISTP X) (GO EMPTY))
                                 (SETQ CL (CAR X))
                                 (SETQ CTLS (CONS CL CTLS]
                      (T (SELECTQ X
                           (P (EDITPR CL NIL 2))
                           (PP (EDITPR CL T 2))
                           (OK (SETQ EXIT-TYPE 'OK) (RETURN S))
                           (STOP (SETQ EXIT-TYPE 'STOP)
                                 (RETURN S-OLD))
                           (SAVE (SETQ EXIT-TYPE 'SAVE) (RETURN S))
                           [UP (COND ((TAILP CL (CADR CTLS)))
                                     ((NULL (CDR CTLS)) (GO TOP))
                                     (T (SETQ CTLS (CDR CTLS))
                                        (SETQ CL
                                          (MEMB CL (CAR CTLS)))
                                        (OR (EQ CL (CAR CTLS))
                                            (SETQ CTLS (CONS CL CTLS]
                           (E (SETQ X (EVAL (COM-READ EDCOM *)))
                              (OR EDCOM (PRINT X)))
                           [F (SETQ TEMP
                                    (EDFIND1ST
                                      (COM-READ EDCOM *)
                                      CL CTLS))
                              [COND ((NULL TEMP) (EDMSG "NOT FOUND"))
                                    (T (SETQ CTLS TEMP)
                                       (SETQ CL (CAR TEMP]
                              (COND ((ATOM CL)
                                      (SETQ EDCOM (CONS 'UP EDCOM]
                           (! (GO B))
                           [NX (COND ((TAILP CL (CADR CTLS))
                                       (SETQ CL (CDR CL))
                                       (RPLACA CTLS CL))
                                     (T (SETQ CTLS (CDR CTLS))
                                        [SETQ CL
                                          (CADR (MEMB CL (CAR CTLS]
                                        (SETQ CTLS (CONS CL CTLS]
                           (? (EDITPR CL NIL 100))
                           (?? (EDITPR CL T 100))
                           [S (SETQ A (COM-READ EDCOM *))
                              (COND ((LITATOM A)
                                      (PUTPROP A 'EDITVALUE CL))
                                    (T (GO ILLG]
                           (COND ((GETPROP X 'DEEDITA)
                                   (SETQ EDCOM
                                         (APPEND
                                           (EVAL (GETPROP X 'DEEDITA))
                                           EDCOM)))
                                 (T (GO ILLG]
                (GO NEXT)
           ILLG (EDMSG "ILLEGAL COMMAND")
           TOP  (EDMSG "ON TOP LEVEL")
           EMPTY
                (EDMSG "LIST EMPTY"))))
  
(EDMSG
  (LAMBDA (MSG)
          (PRINTL MSG)
          (READPOS (IOTAB 4))
          (SETQ EDCOM)
          (GO NEXT)))
  
(EDSMASH
  (LAMBDA (X A B) (RPLACA X A) (RPLACD X B)))
  
)
(PRINT 'EDITFNS)
(RPAQQ EDITFNS
       (EDFIND1ST EDFIND1ST2 EDITF EDITP EDITPR EDITS-INT EDMSG EDSMASH))
(RPAQQ EDITCOMS EDIT-PACKAGE)
(RPAQ EDITGENNR 7)
STOP
