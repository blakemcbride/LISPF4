# E17: GARB's "--- List space empty" path reached the reset label by CALLING
# LISPF4 again.  That call never returns, so every list-space exhaustion left
# one more CONS -> GARB -> LISPF4 chain -- and one more 640-byte SAV_PRBUFF --
# on the C stack for the rest of the session.  Under a 1 MB stack the shipped
# binary segfaults after about 1100 exhaustions; GARB now jumps to a setjmp
# armed at the top of LISPF4 instead, so the C stack does not grow at all.
#
# A small -c keeps each cycle short.  FILLIT holds its list in a PROG
# variable, so the cells stay live until the reset clears the A-stack.

runs=1500

{ echo '(PROGN (SYSFLAG 1 NIL) (QUOTE READY))'
  echo '(DE FILLIT NIL (PROG (L) LP (SETQ L (CONS 1 L)) (GO LP)))'
  i=0
  while [ $i -lt $runs ]; do echo '(FILLIT)'; i=`expr $i + 1`; done
  echo '(PRINT (QUOTE SURVIVED))'
  echo '(EXIT)'; } > fill.lsp

#  A 1 MB stack is the point: the leak is only visible against a bound.
( ulimit -s 1024 2>/dev/null
  "$LISPF4" -c20000 "$LISPF4_IMG" < fill.lsp > out.txt 2>&1 )
st=$?
if [ "$st" -ne 0 ]; then
	echo "$runs list-space exhaustions killed the interpreter (exit status $st)"
	echo "recovered from `grep -ac 'List space empty' out.txt` of them first"
	exit 1
fi
grep -q SURVIVED out.txt || {
	echo "the session did not reach the end:"
	tail -5 out.txt
	exit 1
}
n=`grep -ac 'List space empty' out.txt`
if [ "$n" -lt 100 ]; then
	echo "only $n exhaustions happened -- the case is not exercising GARB"
	exit 1
fi
exit 0
