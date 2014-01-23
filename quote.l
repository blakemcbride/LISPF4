 (FILEHEADER QUOTE)
 (PRINT '(MACRO QUOTE FUNCTIONS))
 (PRINT '(VERSION 2))
 (DEFINEQ
 (COMBINE-SKELS
   <LAMBDA (LFT RGT SKEL)
           (COND ((AND (ISCONST LFT) (ISCONST RGT))
                   (LIST 'QUOTE SKEL))
                 ((NULL RGT) (LIST 'LIST LFT))
                 <(AND (LISTP RGT) (EQ (CAR RGT) 'LIST))
                   (CONS 'LIST (CONS LFT (CDR RGT>
                 ((LIST 'CONS LFT RGT>)
   
 (ISCONST
   <LAMBDA (X)
           (OR (NULL X)
               (EQ X T)
               (NUMBERP X)
               (AND (LISTP X) (EQ (CAR X) 'QUOTE>)
   
 (MQ
   (NLAMBDA $MACRO$
            (EVAL ((LAMBDA (L) (MQUOTE (CAR L))) $MACRO$))))
   
 (MQUOTE
   <LAMBDA (SKEL)
           (COND ((NULL SKEL) NIL)
                 ((ATOM SKEL) (LIST 'QUOTE SKEL))
                 ((EQ (CAR SKEL) '*UQ) (CADR SKEL))
                 <(AND (LISTP (CAR SKEL)) (EQ (CAAR SKEL) '*SUQ))
                   (LIST 'APPEND (CADAR SKEL) (MQUOTE (CDR SKEL>
                 ((COMBINE-SKELS
                    (MQUOTE (CAR SKEL))
                    (MQUOTE (CDR SKEL))
                    SKEL>)
   
 )
 (PRINT 'QUOTEFNS)
 (RPAQQ QUOTEFNS (COMBINE-SKELS ISCONST MQ MQUOTE))
 (RPAQQ QUOTECOMS (MACRO QUOTE FUNCTIONS))
 (RPAQ QUOTEGENNR 2)
 STOP
