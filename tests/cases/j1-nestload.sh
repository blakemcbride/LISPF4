# LOAD was not re-entrant.  H5 fixed exactly this in READFILE -- F4_OPEN
# silently closes and reuses a unit that is already open, so a fixed unit does
# not merely fail, it pulls the outer file out from under the reader -- but
# LOAD and MAKEFILE both still took unit 20 by hand.  A file that LOADs another
# file therefore lost everything after that line, with no diagnostic: the inner
# file loaded, the outer one simply stopped.  Both now ask OPEN0 for a free
# unit, which is what READFILE and OPENF have always done.

cat > inner.lisp <<'EOF'
(PRINT 'INNER-STARTED)
(DEFINEQ (INNERFN (LAMBDA NIL 'FROM-INNER)))
(PRINT 'INNER-DONE)
STOP
EOF

cat > outer.lisp <<'EOF'
(PRINT 'OUTER-STARTED)
(LOAD "inner.lisp")
(PRINT 'OUTER-RESUMED)
(DEFINEQ (OUTERFN (LAMBDA NIL 'FROM-OUTER)))
(PRINT 'OUTER-DONE)
STOP
EOF

cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (LOAD "outer.lisp") (QUOTE LOADED))
(LIST (OUTERFN) (INNERFN))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > drv.txt 2>&1 || {
	echo "nested LOAD died"
	cat drv.txt
	exit 1
}
grep -q '(FROM-OUTER FROM-INNER)' drv.txt || {
	echo "a LOAD inside a LOAD swallowed the rest of the outer file:"
	cat drv.txt
	exit 1
}

#  MAKEFILE took the same fixed unit, so writing a file from inside a LOADed
#  file lost the rest of that file too.
cat > mkr.lisp <<'EOF'
(PRINT 'MKR-STARTED)
(CURFILE TP)
(DEFINEQ (TPFN (LAMBDA NIL 'FROM-TP)))
(MAKEFILE 'TP "tp.lisp" T)
(PRINT 'MKR-RESUMED)
(DEFINEQ (MKRFN (LAMBDA NIL 'FROM-MKR)))
(PRINT 'MKR-DONE)
STOP
EOF

cat > drv2.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (LOAD "mkr.lisp") (QUOTE LOADED))
(MKRFN)
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF

"$LISPF4" "$LISPF4_IMG" < drv2.lsp > drv2.txt 2>&1 || {
	echo "MAKEFILE inside a LOAD died"
	cat drv2.txt
	exit 1
}
grep -q 'FROM-MKR' drv2.txt || {
	echo "a MAKEFILE inside a LOAD swallowed the rest of the file:"
	cat drv2.txt
	exit 1
}
[ -s tp.lisp ] || { echo "the nested MAKEFILE wrote nothing"; exit 1; }
exit 0
