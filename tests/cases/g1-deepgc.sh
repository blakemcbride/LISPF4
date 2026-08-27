# G1: MARKL, the collector's fallback mark routine, had no NIL guard.  Every
# collection over a structure deeper than the A-stack walked into cell 1, wrote
# CDR(NIL) = -I, and then indexed CAR/CDR with that negative value.  Any of the
# four collectors reaches it, and so does ordinary consing once the data is deep
# enough.  1200 deep is safe at the default -s 1500; 3000 is not.
#
# MARKL announces itself with "--- Non-recursive GBC called" the first time it
# runs, so the case also checks that the fallback path was actually taken --
# otherwise a future change to the A-stack size could silently stop testing it.

cat > deep.lsp <<'EOF'
(DE DEEP (N) (PROG (X) LOOP (COND ((ZEROP N) (RETURN X)))
              (SETQ X (LIST X)) (SETQ N (SUB1 N)) (GO LOOP)))
(DE DEPTH (X) (PROG (N) (SETQ N 0) LOOP (COND ((NLISTP X) (RETURN N)))
              (SETQ N (ADD1 N)) (SETQ X (CAR X)) (GO LOOP)))
(PROGN (SETQ D (DEEP 3000)) (QUOTE BUILT))
(RECLAIM 0)
(DEPTH D)
(RECLAIM 1)
(DEPTH D)
(RECLAIM 2)
(DEPTH D)
(RECLAIM 3)
(DEPTH D)
(EXIT)
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 120 "$LISPF4" "$LISPF4_IMG" < deep.lsp > out.txt 2>&1
else
	"$LISPF4" "$LISPF4_IMG" < deep.lsp > out.txt 2>&1
fi
st=$?
if [ "$st" -ne 0 ]; then
	echo "collector died on a deep structure (exit status $st)"
	tail -20 out.txt
	exit 1
fi
if ! grep -q 'Non-recursive GBC called' out.txt; then
	echo "MARKL was never reached -- the case no longer tests what it means to"
	tail -20 out.txt
	exit 1
fi
# The depth must survive all four collectors.
n=`grep -c '^_3000$' out.txt`
if [ "$n" -ne 4 ]; then
	echo "expected the 3000-deep list to survive all four collectors, got $n of 4"
	tail -30 out.txt
	exit 1
fi
exit 0
