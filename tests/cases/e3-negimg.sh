# E3, the other boundary: the small-integer relocation pass started at
# BIGNUM_new - IDIFF2 instead of at BIGNUM_old, so raising -c left the most
# negative small integers below the start of the pass.  Untouched, and now
# below the new NFREET, they came back as cons cells -- (PRINT N1) printed a
# list of NILs.  Values that no longer fit the smaller SMALLNUM range of a
# larger system saturate, which is the honest answer; values that do fit must
# survive exactly.

cat > mk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ N1 -1073650000)
(SETQ N2 -1000000000)
(SETQ N3 1000000000)
(SETQ N4 -7)
(PROGN (SYSOUT "n.img") (QUOTE SAVED))
(EXIT)
EOF

cat > chk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(LIST (COND ((NUMBERP N1) (QUOTE NUM)) (T (QUOTE NOTNUM))) N2 N3 N4)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < mk.lsp > mk.txt 2>&1 || { echo "SYSOUT failed"; cat mk.txt; exit 1; }
[ -f n.img ] || { echo "no n.img written"; cat mk.txt; exit 1; }

want='(NUM -1000000000 1000000000 -7)'
for opts in "" "-c200000" "-c150000" "-a4000 -p20000" "-c200000 -a4000 -p20000"; do
	"$LISPF4" $opts n.img < chk.lsp > chk.txt 2>&1
	st=$?
	if [ "$st" -ne 0 ]; then
		echo "reload with [$opts] died (exit status $st)"
		cat chk.txt
		exit 1
	fi
	if ! grep -q -- "$want" chk.txt; then
		echo "reload with [$opts] mangled the small integers:"
		echo "  wanted: $want"
		grep '^_(' chk.txt
		exit 1
	fi
done
exit 0
