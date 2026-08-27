# I8: when the atom table -- rather than the print-name byte area -- came back
# nearly full from a compacting atom collection, MATOM raised ERRTYP 28, whose
# message is "--- Array index out of bounds".  That is ARRUTL's message and has
# nothing to do with atom space.  37, "--- Bignum/atom space almost exhausted",
# is the right one, and is what the very next line already used for the byte
# half of the same area.  Faithfully translated from Lispf42.f:2311, so it had
# always been there.

have_timeout=
command -v timeout > /dev/null 2>&1 && have_timeout=yes

cat > full.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ L NIL)
(PROG (I) (SETQ I 0) LP (SETQ L (CONS (PACK (LIST (QUOTE A) I)) L))
          (SETQ I (ADD1 I)) (COND ((LESSP I 100000) (GO LP))))
(EXIT)
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 120 "$LISPF4" -a2600 "$LISPF4_IMG" < full.lsp > full.txt 2>&1
	st=$?
else
	"$LISPF4" -a2600 "$LISPF4_IMG" < full.lsp > full.txt 2>&1
	st=$?
fi
if [ "$st" -ne 0 ]; then
	echo "filling atom space did not terminate cleanly (exit status $st)"
	tail -20 full.txt
	exit 1
fi

grep -q 'Bignum/atom space almost exhausted' full.txt || {
	echo "atom space nearly full did not report itself:"
	grep '^---' full.txt | sort -u
	exit 1
}
if grep -q 'Array index out of bounds' full.txt; then
	echo "atom space nearly full still reports ARRUTL's message:"
	grep '^---' full.txt | sort -u
	exit 1
fi

#  The other half of the same area.  At -p9000 the print-name bytes fill while
#  the atom table is still mostly empty; MATOM's hard exit reported 33,
#  "--- Atom space empty. NIL returned", which sends a user to -a when the knob
#  that matters is -p.  Both halves now report 37.
cat > bytes.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ S "ABCDEFGHIJ")
(PROGN (PROG (I) (SETQ I 0) LP (SETQ S (CONCAT S S)) (SETQ I (ADD1 I))
                 (COND ((LESSP I 200) (GO LP)))) (QUOTE FILLED))
(NLSETQ (CONCAT S S))
(EXIT)
EOF

if [ -n "$have_timeout" ]; then
	timeout 120 "$LISPF4" -p9000 "$LISPF4_IMG" < bytes.lsp > bytes.txt 2>&1
	st=$?
else
	"$LISPF4" -p9000 "$LISPF4_IMG" < bytes.lsp > bytes.txt 2>&1
	st=$?
fi
if [ "$st" -ne 0 ]; then
	echo "filling the print-name area did not terminate cleanly (exit status $st)"
	tail -20 bytes.txt
	exit 1
fi
grep -q 'Bignum/atom space almost exhausted' bytes.txt || {
	echo "the print-name area filling did not report itself:"
	grep '^---' bytes.txt | sort -u
	exit 1
}
if grep -q 'Atom space empty' bytes.txt; then
	echo "the print-name area filling still reports the atom table's message:"
	grep '^---' bytes.txt | sort -u
	exit 1
fi
exit 0
