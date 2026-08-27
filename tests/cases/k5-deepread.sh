# K5: I5 established the rule -- APUSH and FPUSH stop only at 100 % full, while
# EVAL refuses to descend once the A-stack is within MIDDL slots of full, and
# that margin is what leaves room for SYSERROR to run.  A C routine that
# overflows must therefore hand the space back itself.  PRIN1 bounds its own
# depth, EQUAL restores JP and SUBPR was given both; IREAD did neither, so an
# over-deep datum returned with IP where FPUSH left it and JP where APUSH2 left
# it.  L25090 could then never report anything: EVAL failed the margin test
# again, MIDDL halved five times and the session reset with `--- Reset' as the
# entire diagnostic, nothing for ERRORSET to catch and ERRORN still holding
# whatever happened before.  The tail of the datum then arrived at top level as
# a stream of stray close parentheses, printing one NIL each.
#
# The threshold is about 490 levels at the default -s1500 (IREAD spends three
# stack words per level) and scales with -s, so 600 is comfortably over it.
# The datum is split over short lines so that K1 plays no part.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

awk 'BEGIN {
	n = 600
	s = "(NLSETQ (QUOTE "
	for (i = 0; i < n; i++) s = s "("
	for (i = 0; i < n; i++) s = s ")"
	s = s "))"
	while (length(s) > 60) { print substr(s, 1, 60); s = substr(s, 61) }
	print s
	print "(ERRORN)"
	print "(QUOTE STILL-ALIVE)"
	print "(EXIT)"
}' > drv.lsp

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

grep -q 'Stack overflow' out.txt || {
	echo "the over-deep datum was not reported as a stack overflow"
	rc=1
}
#  The banner's own "--- Reset" is the first line of every session; a second
#  one means the interpreter restarted and threw the computation away.
[ "`grep -c -- '--- Reset' out.txt`" -gt 1 ] && {
	echo "the over-deep datum reset the interpreter"
	rc=1
}
grep -q '^:*_*12$' out.txt || {
	echo "(ERRORN) does not report 12 (stack overflow):"
	tail -6 out.txt
	rc=1
}
grep -q '^:*_*STILL-ALIVE$' out.txt || { echo "session did not survive"; rc=1; }

[ "$rc" -ne 0 ] && tail -20 out.txt
exit $rc
