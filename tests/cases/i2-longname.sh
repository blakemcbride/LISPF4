# I2: PRINAT split any printed form wider than the right print margin across a
# line break with nothing to say so, and MAKEFILE is the export format, so
# MAKEFILE -> LOAD silently corrupted the data.  A 78-character atom came back
# as two atoms; a 77-character string came back 149 characters long, because
# the reader pads every input line out to the read margin with blanks and the
# blanks landed inside the string.  A name that cannot be made to fit on any
# line is data, not layout: it now overruns the right margin and stays
# readable.  The remaining ceiling is the print buffer, which is above the
# 150-column read margin, so anything the reader can take in one line
# round-trips.

cat > mk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(CURFILE TP)
(DEFINEQ (G (LAMBDA NIL (QUOTE AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA))))
(DEFINEQ (H (LAMBDA NIL "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")))
(PROGN (MAKEFILE (QUOTE TP) "t.lisp" T) (QUOTE WRITTEN))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < mk.lsp > mk.txt 2>&1 || {
	echo "MAKEFILE died"
	cat mk.txt
	exit 1
}
[ -s t.lisp ] || { echo "MAKEFILE wrote nothing"; exit 1; }

cat > chk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (LOAD "t.lisp") (QUOTE LOADED))
(LIST (QUOTE ATOM) (NCHARS (G)) (QUOTE STRING) (NCHARS (H)))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < chk.lsp > chk.txt 2>&1 || {
	echo "LOAD of the file MAKEFILE wrote died"
	cat chk.txt
	exit 1
}
grep -q '(ATOM 100 STRING 100)' chk.txt || {
	echo "MAKEFILE/LOAD did not round-trip a 100-character atom and string:"
	grep -n 'ATOM' chk.txt
	echo "--- t.lisp was ---"
	cat t.lisp
	exit 1
}

#  And the plain printer, which is what MAKEFILE is built on: no line of the
#  printed form may be a fragment of the name.
cat > pr.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (PRINT (QUOTE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX)) (QUOTE PRINTED))
(EXIT)
EOF
"$LISPF4" "$LISPF4_IMG" < pr.lsp > pr.txt 2>&1 || {
	echo "PRINT died"
	cat pr.txt
	exit 1
}
n=`sed -e 's/^_//' -e 's/[ 	]*$//' pr.txt | grep -c '^XXXXXXXXXX*$'`
if [ "$n" -ne 1 ]; then
	echo "PRINT split a 100-character atom over $n lines:"
	sed -e 's/^_//' pr.txt | grep -n '^XXXXXXXXXX*$'
	exit 1
fi
exit 0
