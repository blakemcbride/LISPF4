# M2 (circular): a package variable holding a ring used to be written with a
# literal --- once PRINTLENGTH nodes had gone out, and MAKEFILE said COMPLETE.
# K2 raised PRINTLENGTH to a million, which raised the printer's node budget
# with it, so a three-element ring wrote a 2.3 MB file that exhausted list
# space and reset the interpreter when LOAD read it back.  With read-back-mode
# truncation now an error, the ring is reported ABANDONED and no runaway file
# is produced.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (SETQ CIRC (LIST 1 2 3)) (RPLACD (CDDR CIRC) CIRC) (QUOTE BUILT))
(RPAQQ ZZCOMS NIL)
(CURFILE ZZ)
(RPAQQ ZZVARS (CIRC))
(MAKEFILE (QUOTE ZZ) "circ.lisp" T)
(QUOTE STILL-ALIVE)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

grep -q 'MAKEFILE ZZ COMPLETE' out.txt && {
	echo "MAKEFILE reported COMPLETE on a circular variable"; exit 1; }
grep -q 'MAKEFILE ZZ ABANDONED' out.txt || {
	echo "MAKEFILE did not report the circular variable ABANDONED"
	tail -8 out.txt; exit 1; }

#  The file must not have run away.  The old damage was 2.3 MB; anything under
#  256 KB means the node budget bit long before that.
if [ -f circ.lisp ]; then
	size=`wc -c < circ.lisp`
	[ "$size" -gt 262144 ] && { echo "circ.lisp ran away: $size bytes"; exit 1; }
fi
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; exit 1; }
exit 0
