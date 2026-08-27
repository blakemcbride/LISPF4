# E1: PRINAT emitted one ' per nested (QUOTE x) wrapper into PRBUFF with no
# bound at all.  PRBUFF is integer[160] inside /B/, followed by BUFF, IMESS
# and then the PNAME/PNP/HTAB/STACK heap pointers, so 161 quotes corrupted
# the printer's scratch area, 321 the whole system message table, and ~800
# the allocation table -- a SIGSEGV on the shipped -O3 build.  The quotes now
# wrap onto continuation lines the way every other PRBUFF write does.

python3 -c "print('(PRINT (QUOTE %sX))' % (\"'\"*800)); print('(EXIT)')" > q.lsp 2>/dev/null \
	|| awk 'BEGIN{s="";for(i=0;i<800;i++)s=s "'"'"'";print "(PRINT (QUOTE " s "X))";print "(EXIT)"}' > q.lsp

"$LISPF4" "$LISPF4_IMG" < q.lsp > out.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "800 nested quotes killed the interpreter (exit status $st)"
	exit 1
fi
#  The datum must still print, and the trailing atom must survive.
if ! grep -q "X" out.txt; then
	echo "the quoted datum was not printed:"
	cat out.txt
	exit 1
fi

#  The same shape built by nesting rather than by typing.
{ echo '(PROGN (SYSFLAG 1 NIL) (QUOTE READY))'
  echo '(SETQ Q (QUOTE X))'
  echo '(PROG (I) (SETQ I 0) LP (SETQ Q (LIST (QUOTE QUOTE) Q)) (SETQ I (ADD1 I))'
  echo '      (COND ((LESSP I 400) (GO LP))) (RETURN (QUOTE BUILT)))'
  echo '(PROGN (PRINT Q) (QUOTE PRINTED))'
  echo '(EXIT)'; } > n.lsp
"$LISPF4" "$LISPF4_IMG" < n.lsp > out2.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "400 nested (QUOTE ...) killed the interpreter (exit status $st)"
	exit 1
fi
grep -q PRINTED out2.txt || { echo "nested form did not print:"; cat out2.txt; exit 1; }
exit 0
