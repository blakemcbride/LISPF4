# D8: move_() relocated CAR/CDR and the ARGS block on ROLLIN but not the
# pointer part of arrays, which lives in PNAME.  Reloading an image with a
# different -c or -a therefore left every array pointer slot aimed at the old
# address space -- a cons came back as the free list, a small integer came
# back off by the NUMADD delta.  garb_ STEP 6 always did this correctly; only
# the image path did not.

cat > mk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (SETQ A (ARRAY 15 5 5)) (QUOTE READY))
(SETA A 1 (QUOTE FOO))
(SETA A 2 (CONS 1 2))
(SETA A 3 99)
(SETI A 1 4242)
(SETR A 1 2.5)
(PROGN (SYSOUT "arr.img") (QUOTE SAVED))
(EXIT)
EOF

cat > chk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(LIST (ELT A 1) (ELT A 2) (ELT A 3) (ELTI A 1) (ELTR A 1))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < mk.lsp > mk.txt 2>&1 || { echo "SYSOUT failed"; cat mk.txt; exit 1; }
[ -f arr.img ] || { echo "no arr.img written"; cat mk.txt; exit 1; }

want='(FOO (1 . 2) 99 4242 2.5)'
for opts in "" "-c200000" "-a4000 -p20000" "-c200000 -a4000 -p20000"; do
	"$LISPF4" $opts arr.img < chk.lsp > chk.txt 2>&1
	st=$?
	if [ "$st" -ne 0 ]; then
		echo "reload with [$opts] died (exit status $st)"
		cat chk.txt
		exit 1
	fi
	if ! grep -q -- "$want" chk.txt; then
		echo "reload with [$opts] did not relocate the array pointer part:"
		echo "  wanted: $want"
		grep '^_(' chk.txt
		exit 1
	fi
done
exit 0
