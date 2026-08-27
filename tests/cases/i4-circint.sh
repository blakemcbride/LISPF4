# I4: twelve builtins walk a list spine with a bare GOTO and never polled the
# break flag, so a circular argument ran forever and ignored every SIGINT --
# ADDLIST APPEND ASSOC LAST LENGTH MEMB MEMBER PACK REVERSE SASSOC SUBPAIR
# TAILP.  E12 gave EQUAL a poll but nothing else, and for MEMBER and SASSOC
# that made things worse rather than better: EQUAL's poll clears the flag, sets
# IBREAK and returns NIL, which the caller read as "not this element" and
# carried on -- so the first Ctrl-C was swallowed and every later one met a
# flag the previous iteration had already cleared.  Circular structure is not
# exotic here: DOCOLLECT builds one deliberately as its accumulator.
#
# Each builtin below is given a circular list and one SIGINT, and must reach
# the (PRINT 'SURVIVED) that follows it.  REVERSE and APPEND are not in the
# list because they CONS and so end by exhausting list space instead (i3), and
# COPY overflows the A-stack first (i5); those are the same builtins reached
# by a different exit.

run_one() {
	what=$1
	form=$2
	cat > c.lsp <<EOF
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ CIRC (LIST 1 2 3))
(PROGN (RPLACD (CDDR CIRC) CIRC) (PRINT (QUOTE CIRCULAR)) NIL)
(PROGN $form (QUOTE UNREACHABLE))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF
	#  The three set-up forms take milliseconds; the fourth never returns.
	#  Output to a file is block buffered, so waiting for the CIRCULAR
	#  marker to appear would mean waiting for the buffer, not for the loop.
	"$LISPF4" "$LISPF4_IMG" < c.lsp > c.txt 2>&1 &
	pid=$!
	sleep 3
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
		echo "$what ignored SIGINT on a circular list"
		return 1
	fi
	wait $pid
	grep -q SURVIVED c.txt || {
		echo "$what did not leave the system usable after SIGINT:"
		tail -20 c.txt
		return 1
	}
	return 0
}

run_one LENGTH  '(LENGTH CIRC)'            || exit 1
run_one LAST    '(LAST CIRC)'              || exit 1
run_one MEMB    "(MEMB (QUOTE Z) CIRC)"    || exit 1
run_one MEMBER  "(MEMBER (QUOTE Z) CIRC)"  || exit 1
run_one ASSOC   "(ASSOC (QUOTE Z) CIRC)"   || exit 1
run_one SASSOC  "(SASSOC (QUOTE Z) CIRC)"  || exit 1
run_one TAILP   "(TAILP (QUOTE Z) CIRC)"   || exit 1
run_one ADDLIST "(ADDLIST (QUOTE Z) CIRC)" || exit 1
run_one SUBPAIR "(SUBPAIR CIRC (QUOTE (A B C)) (QUOTE Z))" || exit 1
exit 0
