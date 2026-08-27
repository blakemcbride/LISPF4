# K7: LOAD's INUNIT and CLOSE sat inside the STOP arm of its SELECTQ, so any
# other exit -- an error in a form, a SIGINT, an NLSETQ that caught -- left
# both undone.  Two things then went wrong.  The load was abandoned with no
# message saying so, and the READER WAS STILL POINTING AT THE FILE: the top
# level read and evaluated the rest of it as though the user had typed it,
# until SHIFT hit EOF and switched back on its own.  And the logical unit
# leaked; OPEN0 skips units that are already open, so a session spent
# debugging a file that does not load ran out of units and could never open a
# file again.  READFILE (basic1.lisp) had the same shape, and is loaded before
# ERRORSET exists, so it catches by hand: its PROG carries the ERRORSET label
# that SYSERROR's (GO* ERRORSET) looks for.
#
# The detector is the form after the failing one: it must NOT be evaluated.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > bad.lisp <<'LISP'
(SETQ P1 1)
(NOSUCHFN 3)
(SETQ P2 2)
STOP
LISP

cat > drv.lsp <<'LISP'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(NLSETQ (LOAD "bad.lisp"))
(QUOTE AFTER-LOAD)
P1
(NLSETQ P2)
(NLSETQ (READFILE "bad.lisp"))
(QUOTE AFTER-READFILE)
(NLSETQ Q2)
(OPEN0 "bad.lisp" T NIL)
(QUOTE STILL-ALIVE)
(EXIT)
LISP

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

#  P1 was set by the form before the failing one, so the load really started.
grep -q '^:*_*1$' out.txt || {
	echo "the first form of the file was never evaluated"
	rc=1
}
#  P2 comes after the failing form.  NLSETQ of an unbound variable answers NIL;
#  if the top level read the rest of the file, P2 is 2.
grep -q '^:*_*(2)$' out.txt && {
	echo "the form after the failing one was evaluated at top level --"
	echo "the reader was left pointing into the file"
	rc=1
}
grep -q 'ABANDONED' out.txt || {
	echo "neither LOAD nor READFILE said the load was abandoned"
	rc=1
}
#  Nothing else holds a unit, so the first free one must still be 10.
grep -q '^:*_*10$' out.txt || {
	echo "the failed loads leaked their logical units:"
	tail -6 out.txt
	rc=1
}
grep -q '^:*_*STILL-ALIVE$' out.txt || { echo "session did not survive"; rc=1; }

[ "$rc" -ne 0 ] && tail -25 out.txt
exit $rc
