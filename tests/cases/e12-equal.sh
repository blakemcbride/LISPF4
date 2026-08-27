# E12: EQUAL walks two structures in lockstep with no cycle detection and
# never polled the break flag.  CAR-circular arguments spun forever because
# APUSH2 signals A-stack overflow by leaving the marker 16 in the F-stack and
# restoring JP -- so JP stopped moving and EQUAL, unlike SUBPR and PRIN1,
# never looked at the marker.  Ctrl-C did nothing either; the only way out
# was kill -9.  EQUAL now notices the marker, and polls the break flag so the
# CDR-circular case (which does not grow the stack at all) stays killable.

cat > car.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ X (LIST 1 2 3))
(SETQ Y (LIST 1 2 3))
(PROGN (RPLACA X X) (RPLACA Y Y) (QUOTE DONE))
(PROGN (SETQ Z (EQUAL X Y)) (QUOTE FINISHED))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 60 "$LISPF4" "$LISPF4_IMG" < car.lsp > car.txt 2>&1
	st=$?
else
	"$LISPF4" "$LISPF4_IMG" < car.lsp > car.txt 2>&1
	st=$?
fi
if [ "$st" -ne 0 ]; then
	echo "CAR-circular EQUAL did not terminate (exit status $st)"
	exit 1
fi
grep -q SURVIVED car.txt || {
	echo "CAR-circular EQUAL did not leave the system usable:"
	cat car.txt
	exit 1
}

#  CDR-circular EQUAL still loops -- there is no cycle detection -- but it
#  must now be interruptible, and the interpreter must recover.
cat > cdr.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ X (LIST 1 2 3))
(SETQ Y (LIST 1 2 3))
(PROGN (RPLACD (CDDR X) X) (RPLACD (CDDR Y) Y) (QUOTE DONE))
(PROGN (SETQ Z (EQUAL X Y)) (QUOTE FINISHED))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF

"$LISPF4" "$LISPF4_IMG" < cdr.lsp > cdr.txt 2>&1 &
pid=$!
i=0
while [ $i -lt 20 ]; do
	grep -q DONE cdr.txt 2>/dev/null && break
	i=`expr $i + 1`
	sleep 1
done
kill -INT $pid 2>/dev/null
i=0
while [ $i -lt 30 ]; do
	kill -0 $pid 2>/dev/null || break
	i=`expr $i + 1`
	sleep 1
done
if kill -0 $pid 2>/dev/null; then
	kill -9 $pid 2>/dev/null
	wait $pid 2>/dev/null
	echo "CDR-circular EQUAL ignored SIGINT"
	exit 1
fi
wait $pid
grep -q SURVIVED cdr.txt || {
	echo "CDR-circular EQUAL did not recover after SIGINT:"
	cat cdr.txt
	exit 1
}
exit 0
