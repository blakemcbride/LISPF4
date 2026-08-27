# D2: the argument/variable spreading loops pushed one parameter-stack slot
# per element of user data with no bound check, so a form with more arguments
# (or PROG variables) than the stack has slots ran off the end of JACK/JILL.
# Both cases segfault the pre-fix interpreter; now they must report the
# overflow and leave the system usable.

cat > mkl.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (PRINTLEVEL 2) (PRINTLENGTH 3) (QUOTE READY))
(DE MKL (N) (PROG (L) (SETQ L NIL)
             LP (COND ((ZEROP N) (RETURN L)))
                (SETQ L (CONS 1 L)) (SETQ N (SUB1 N)) (GO LP)))
(PROGN (SETQ BIG (MKL 3000)) (QUOTE READY))
(LENGTH (APPLY (QUOTE LIST) BIG))
(PLUS 111 222)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < mkl.lsp > out1.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "APPLY with 3000 arguments died (exit status $st)"
	tail -20 out1.txt
	exit 1
fi
if ! grep -q '333' out1.txt; then
	echo "interpreter did not survive to evaluate (PLUS 111 222):"
	tail -20 out1.txt
	exit 1
fi

# 2999 PROG variables.
awk 'BEGIN {
	printf "(PROGN (SYSFLAG 1 NIL) (PRINTLEVEL 2) (PRINTLENGTH 3) (QUOTE READY))\n"
	printf "(DE F NIL (PROG ("
	for (i = 1; i < 3000; i++) printf "V%d ", i
	printf ") (RETURN 1)))\n(F)\n(PLUS 111 222)\n(EXIT)\n"
}' > prog.lsp

"$LISPF4" "$LISPF4_IMG" < prog.lsp > out2.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "PROG with 2999 variables died (exit status $st)"
	tail -20 out2.txt
	exit 1
fi
if ! grep -q '333' out2.txt; then
	echo "interpreter did not survive to evaluate (PLUS 111 222):"
	tail -20 out2.txt
	exit 1
fi
exit 0
