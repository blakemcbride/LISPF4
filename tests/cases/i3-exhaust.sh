# I3: the reset label L1 re-initialised the stacks and cleared FORM, ARG and
# ARG2, but not ARG3, ALIST, TEMP1, TEMP2, TEMP3, I1CONS or I2CONS -- all of
# which live in ARGS(1..NARGS) and are therefore GC roots.  REVERSE accumulates
# into TEMP1, so an exhaustion on a circular list left TEMP1 heading a chain
# holding every cell in the system.  After the reset the top loop's first CONS
# collected, got nothing back, reported "List space empty" and reset into
# exactly the same state: 549 248 resets and 40 MB of output in 45 seconds,
# immune to SIGINT, kill -9 the only way out.  Clearing the rest of the
# register file at L1 releases the garbage and turns it into one message.

cat > circ.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ CIRC (LIST 1 2 3))
(PROGN (RPLACD (CDDR CIRC) CIRC) (QUOTE CIRCULAR))
(PROGN (REVERSE CIRC) (QUOTE UNREACHABLE))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 45 "$LISPF4" -c20000 "$LISPF4_IMG" < circ.lsp > circ.txt 2>&1
	st=$?
else
	"$LISPF4" -c20000 "$LISPF4_IMG" < circ.lsp > circ.txt 2>&1
	st=$?
fi
if [ "$st" -ne 0 ]; then
	echo "the exhaustion did not terminate (exit status $st)"
	head -20 circ.txt
	exit 1
fi

n=`grep -c 'List space empty' circ.txt`
if [ "$n" -ge 10 ]; then
	echo "the reset left the culprit rooted: $n 'List space empty' resets"
	exit 1
fi
grep -q SURVIVED circ.txt || {
	echo "the interpreter was not usable after the exhaustion:"
	tail -20 circ.txt
	exit 1
}
exit 0
