# D3: EVALA and APPLYA jumped back to the label they were already at when the
# parameter stack ran low, wedging the interpreter in a loop that never
# reaches the SIGINT poll in EVAL.  Small -s makes it reproducible -- 500 is
# the smallest MAIN accepts (G4), and 600 a-list entries still overflow it.

cat > evala.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (PRINTLEVEL 2) (PRINTLENGTH 3) (QUOTE READY))
(DE MKAL (N) (PROG (L) (SETQ L NIL)
              LP (COND ((ZEROP N) (RETURN L)))
                 (SETQ L (CONS (CONS (QUOTE X) 7) L)) (SETQ N (SUB1 N)) (GO LP)))
(PROGN (SETQ AL (MKAL 600)) (QUOTE READY))
(EVALA (QUOTE X) AL)
(APPLYA (QUOTE CAR) (QUOTE ((A))) AL)
(PLUS 111 222)
(EXIT)
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 60 "$LISPF4" -s500 "$LISPF4_IMG" < evala.lsp > out.txt 2>&1
else
	"$LISPF4" -s500 "$LISPF4_IMG" < evala.lsp > out.txt 2>&1
fi
st=$?
if [ "$st" -eq 124 ]; then
	echo "EVALA/APPLYA hung with a nearly full parameter stack"
	exit 1
fi
if [ "$st" -ne 0 ]; then
	echo "died (exit status $st)"
	tail -20 out.txt
	exit 1
fi
if ! grep -q '333' out.txt; then
	echo "interpreter did not survive to evaluate (PLUS 111 222):"
	tail -20 out.txt
	exit 1
fi
exit 0
