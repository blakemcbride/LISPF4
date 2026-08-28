# M2: MAKEFILE wrote the file with the printer's display limits in force, so a
# structure deeper than the print level, or longer than the node budget, went
# out with a literal ... or --- for the missing part and MAKEFILE still said
# "COMPLETE."  K2 raised the two numbers rather than removing them, which moved
# the cliff (the effective depth limit is (JP-IP)/5-1, so it depends on -s) but
# did not close it.  Printing FOR READ-BACK (SYSFLAG 6 off, as MAKEFILE sets
# it) is now a failure rather than a graphic: PRIN0 raises, MAKEFILE's ERRORSET
# catches it, and the file is reported ABANDONED.  A structure that fits is
# still written and still says COMPLETE and still round-trips.
#
# -s is pinned: at the default 1500 the 600-level nest is past the ~288-level
# A-stack clamp, so it must be ABANDONED; a small nest must round-trip.  The
# two use different package names (ZZ, YY) so their messages can be told apart.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(DEFINEQ (MKD (LAMBDA (N) (PROG (L) LP (COND ((ZEROP N) (RETURN L)))
                                   (SETQ L (LIST L)) (SETQ N (SUB1 N))
                                   (GO LP)))))
(DEFINEQ (DEPTH (LAMBDA (L) (PROG ((N 0)) LP (COND ((NLISTP L) (RETURN N)))
                                   (SETQ N (ADD1 N)) (SETQ L (CAR L))
                                   (GO LP)))))
(PROGN (SETQ DEEP (MKD 600)) (SETQ OK5 (MKD 5)) (QUOTE BUILT))
(RPAQQ ZZCOMS NIL)
(CURFILE ZZ)
(RPAQQ ZZVARS (DEEP))
(MAKEFILE (QUOTE ZZ) "deep.lisp" T)
(RPAQQ YYCOMS NIL)
(CURFILE YY)
(RPAQQ YYVARS (OK5))
(MAKEFILE (QUOTE YY) "ok5.lisp" T)
(SETQ OK5 NIL)
(LOAD "ok5.lisp")
(DEPTH OK5)
(QUOTE STILL-ALIVE)
(EXIT)
EOF

"$LISPF4" -s1500 "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0
[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

#  The deep one must not be written with a truncation graphic and reported done.
grep -q -- '\.\.\.' deep.lisp 2>/dev/null && {
	echo "the deep file has a literal ... (silent level truncation)"; rc=1; }
grep -q 'MAKEFILE ZZ COMPLETE' out.txt && {
	echo "MAKEFILE reported COMPLETE on a structure it could not write"; rc=1; }
grep -q 'MAKEFILE ZZ ABANDONED' out.txt || {
	echo "MAKEFILE did not report the deep structure ABANDONED"; rc=1; }

#  The small one must round-trip and report COMPLETE.
grep -q 'MAKEFILE YY COMPLETE' out.txt || {
	echo "MAKEFILE did not report the small structure COMPLETE"; rc=1; }
grep -q '^_*5$' out.txt || {
	echo "the 5-level nest did not survive MAKEFILE/LOAD"; rc=1; }
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; rc=1; }

[ "$rc" -ne 0 ] && tail -20 out.txt
exit $rc
