# E3: ROLLIN relocated the three regions above the atoms with the wrong
# offsets.  Floats ("bignums") were shifted by the cons-cell offset IDIFF1,
# which is only correct while NATOM is unchanged, so reloading an image under
# a different -a silently turned every float into whatever four bytes of
# packed print-name text happened to live at the new index -- and under a
# small enough -a into a small integer outright.  The array pointer part went
# the same way.  Nothing crashed; the data was just wrong.

cat > mk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ X 3.25)
(SETQ Y (LIST 1.5 2.5 X))
(PROGN (SETQ B (ARRAY 15 5 5)) (QUOTE READY))
(SETA B 1 3.75)
(SETR B 1 -0.75)
(PROGN (SYSOUT "f.img") (QUOTE SAVED))
(EXIT)
EOF

cat > chk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(LIST X Y (ELT B 1) (ELTR B 1))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < mk.lsp > mk.txt 2>&1 || { echo "SYSOUT failed"; cat mk.txt; exit 1; }
[ -f f.img ] || { echo "no f.img written"; cat mk.txt; exit 1; }

want='(3.25 (1.5 2.5 3.25) 3.75 -.75)'
for opts in "" "-a4000" "-a2500" "-a4000 -p20000" "-c200000" "-c200000 -a4000 -p20000"; do
	"$LISPF4" $opts f.img < chk.lsp > chk.txt 2>&1
	st=$?
	if [ "$st" -ne 0 ]; then
		echo "reload with [$opts] died (exit status $st)"
		cat chk.txt
		exit 1
	fi
	if ! grep -q -- "$want" chk.txt; then
		echo "reload with [$opts] did not relocate the floats:"
		echo "  wanted: $want"
		grep '^_(' chk.txt
		exit 1
	fi
done
exit 0
