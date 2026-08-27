# K4: I4 gave a break poll to the twelve builtins that walk a list spine with a
# bare GOTO; a census of every such loop in lispf41.c turned up four more that
# are reachable and unbounded, plus GET in lispf42.c.  Each hung and ignored
# SIGINT on a circular argument:
#
#   NCONC, NCONC1              the spine walk at L12480
#   MAP, MAPC                  L12250, which drives its loop through APPLY --
#                              and APPLY, unlike EVAL, had no break poll at
#                              all, so the poll was missed whenever the mapped
#                              function was a SUBR and control never reached
#                              EVAL.  With a LAMBDA the body went through EVAL
#                              and the interrupt worked, which is why this was
#                              easy to miss.  The poll now sits at L1500 and so
#                              covers every APPLY-driven loop.
#   PUTPROP                    L15050
#   GETPROP, GETD              GET's L8.  E4 made it tolerate a malformed
#                              property list; it did not make it terminate on a
#                              circular one, and RPLACD on a literal atom IS
#                              the plist setter, so a ring is easy to build.
#
# Same harness as i4-circint: one SIGINT must get control back.

run_one() {
	what=$1
	form=$2
	setup=$3
	cat > c.lsp <<EOF
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ CIRC (LIST 1 2 3))
(PROGN (RPLACD (CDDR CIRC) CIRC) (PRINT (QUOTE CIRCULAR)) NIL)
$setup
(PROGN $form (QUOTE UNREACHABLE))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF
	#  The set-up forms take milliseconds; the last one never returns.
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

#  A ring for ZZ's property list: (PUTPROP 'ZZ 'A 1) (PUTPROP 'ZZ 'B 2) leaves
#  the plist (A 1 B 2), and RPLACD on the last cell points it back at the head.
RING="(PROGN (PUTPROP (QUOTE ZZ) (QUOTE A) 1)
             (PUTPROP (QUOTE ZZ) (QUOTE B) 2)
             (RPLACD (CDR (CDDDR (QUOTE ZZ))) (CDR (QUOTE ZZ)))
             (QUOTE RING))"

run_one NCONC   "(NCONC CIRC (QUOTE (A)))"          || exit 1
run_one NCONC1  "(NCONC1 CIRC (QUOTE A))"           || exit 1
run_one MAP     "(MAP CIRC (QUOTE NULL))"           || exit 1
run_one MAPC    "(MAPC CIRC (QUOTE NULL))"          || exit 1
run_one MAPC3   "(MAPC CIRC (QUOTE NULL) (QUOTE CDR))" || exit 1
run_one GETPROP "(GETPROP (QUOTE ZZ) (QUOTE C))" "$RING" || exit 1
run_one GETD    "(GETD (QUOTE ZZ))"              "$RING" || exit 1
run_one PUTPROP "(PUTPROP (QUOTE ZZ) (QUOTE C) 3)" "$RING" || exit 1
exit 0
