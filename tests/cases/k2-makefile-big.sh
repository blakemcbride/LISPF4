# K2: MAKEFILE wrote the file with (PRINTLEVEL 150) and (PRINTLENGTH 1000) in
# force.  Those are DISPLAY limits -- past them the printer emits the graphic
# --- (length) or ... (level) instead of the rest of the structure -- and the
# package is written through the same printer, so anything past them was not
# written at all.  A 1200-element variable came back 1001 long with the literal
# atom --- as its last element, and MAKEFILE still printed "COMPLETE."
#
# SYSOUT is unaffected (it is binary), so the loss only showed up when someone
# reloaded the text file, possibly much later.  The limits are now raised
# rather than lowered: PRIN1's node budget is max(PRNODES, LEVELL, LEVELM), so
# raising them raises the circular-structure guard with them, and the effective
# print level is separately clamped to the A-stack that is actually free.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'LISP'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(DEFINEQ (MKL (LAMBDA (N) (PROG (L) LP (COND ((ZEROP N) (RETURN L)))
                                   (SETQ L (CONS N L)) (SETQ N (SUB1 N))
                                   (GO LP)))))
(DEFINEQ (MKD (LAMBDA (N) (PROG (L) LP (COND ((ZEROP N) (RETURN L)))
                                   (SETQ L (LIST L)) (SETQ N (SUB1 N))
                                   (GO LP)))))
(PROGN (SETQ BIGL (MKL 1200)) (SETQ DEEP (MKD 200)) (QUOTE BUILT))
(RPAQQ ZZCOMS NIL)
(CURFILE ZZ)
(RPAQQ ZZVARS (BIGL DEEP))
(MAKEFILE (QUOTE ZZ) "zz.lisp" T)
(PROGN (SETQ BIGL NIL) (SETQ DEEP NIL) (QUOTE CLEARED))
(LOAD "zz.lisp")
(LENGTH BIGL)
(LAST BIGL)
(EQUAL DEEP (MKD 200))
(QUOTE STILL-ALIVE)
(EXIT)
LISP

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

[ -f zz.lisp ] || { echo "MAKEFILE wrote no file"; exit 1; }

grep -q -- '---' zz.lisp && {
	echo "the file MAKEFILE wrote has a literal --- in it (length truncation)"
	rc=1
}
grep -q -- '\.\.\.' zz.lisp && {
	echo "the file MAKEFILE wrote has a literal ... in it (level truncation)"
	rc=1
}
grep -q '^_*1200$' out.txt || {
	echo "the 1200-element list did not survive MAKEFILE/LOAD:"
	grep -n 'LENGTH\|^_*[0-9][0-9]*$' out.txt | tail -4
	rc=1
}
grep -q '^_*(1200)$' out.txt || {
	echo "the tail of the reloaded list is not (1200)"
	rc=1
}
grep -q '^_*T$' out.txt || {
	echo "the 200-level nest did not survive MAKEFILE/LOAD"
	rc=1
}
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; rc=1; }

[ "$rc" -ne 0 ] && tail -20 out.txt
exit $rc
