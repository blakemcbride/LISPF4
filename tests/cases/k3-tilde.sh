# K3: the printer's read-back escaping (SYSFLAG 5) escaped character types 1-8
# and 23, but not type 24 -- the rescue character `~' -- in either branch, so
# neither in an atom nor inside a string.  SHIFT acts on `~' wherever it occurs,
# including in the middle of a string literal, so any datum containing one
# printed back as a `--- User break' instead of itself and MAKEFILE wrote a file
# LOAD could not read: the definition was lost and everything after it in the
# file was skipped.
#
# %~ was already handled correctly on input (SHIFT's L1100 gives an escaped
# character type 10), so escaping it on output is all that is needed.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'LISP'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(RPAQQ ZTCOMS NIL)
(CURFILE ZT)
(DEFINEQ (FZ (LAMBDA (X) (CONS (QUOTE A%~B) (CONCAT "p" (QUOTE %~) "q")))))
(PROGN (SETQ SAVED (GETD (QUOTE FZ))) (QUOTE SAVED))
(MAKEFILE (QUOTE ZT) "zt.lisp" T)
(PROGN (PUTD (QUOTE FZ) NIL) (QUOTE CLEARED))
(LOAD "zt.lisp")
(EQUAL SAVED (GETD (QUOTE FZ)))
(FZ 1)
(NCHARS (QUOTE A%~B))
(QUOTE STILL-ALIVE)
(EXIT)
LISP

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

grep -q 'User break' out.txt && {
	echo "reading the file back raised a user break on the rescue character"
	rc=1
}
grep -q '^_*T$' out.txt || {
	echo "the reloaded definition is not EQUAL to the original:"
	sed -n '/zt.lisp/,$p' out.txt | head -8
	rc=1
}
grep -q '^_*(A%~B \. "p%~q")$' out.txt || {
	echo "the reloaded function does not return the original datum"
	rc=1
}
grep -q '^_*3$' out.txt || { echo "A~B is not three characters long"; rc=1; }
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; rc=1; }

[ "$rc" -ne 0 ] && { tail -20 out.txt; echo "--- zt.lisp:"; cat zt.lisp; }
exit $rc
