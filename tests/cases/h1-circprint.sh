# H1: PRINTLEVEL bounds the depth of the printed image and PRINTLENGTH the
# length of each level, but nothing used to bound their product.  A structure
# circular through BOTH CAR and CDR runs away in both directions at once and
# emits on the order of PRINTLENGTH ** PRINTLEVEL nodes -- 1000 ** 1000 at the
# defaults.  (TCONC X X) builds exactly that shape in one line: it leaves
# P = (B . B) with B = (P).  Before the fix this printed 809 MB in three
# seconds and stopped only for an interrupt.
#
# Four shapes are checked.  The first two were already bounded (by PRINTLENGTH
# and PRINTLEVEL respectively) and must stay that way; the last two are the
# ones that ran away.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'LISP'
(SYSFLAG 1 NIL)
(PROGN (SETQ X (LIST 1)) (TCONC X X) (QUOTE TCONC-DONE))
(PROGN (SETQ C (LIST 1 2 3)) (RPLACD (CDDR C) C) (QUOTE CDR-CIRCULAR))
(PRINT C)
(PROGN (SETQ A (LIST NIL)) (RPLACA A A) (QUOTE CAR-CIRCULAR))
(PRINT A)
(PROGN (SETQ P (CONS NIL NIL)) (RPLACA P P) (RPLACD P P) (QUOTE BOTH))
(PRINT P)
(QUOTE STILL-ALIVE)
(EXIT)
LISP

# A pipe would let the interpreter run forever against a reader that never
# fills; write to a file so the size below is the real, finished output.
"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

if [ "$st" -ne 0 ]; then
	echo "exit status $st"; tail -5 out.txt; exit 1
fi
if ! grep -q '^_*STILL-ALIVE$' out.txt; then
	echo "the session did not reach the end of the script"
	rc=1
fi

# 100 000 printed items is the budget, and the pretty printer spends about
# 50 bytes on each of them.  Anything under 16 MB means the budget held; the
# pre-fix binary passes 16 MB in well under a second and never stops.
size=`wc -c < out.txt`
if [ "$size" -gt 16000000 ]; then
	echo "output ran away: $size bytes"
	rc=1
fi

[ "$rc" -ne 0 ] && tail -5 out.txt
exit $rc
