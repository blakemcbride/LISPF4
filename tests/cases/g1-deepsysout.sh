# G1, through ROLLOUT: SYSOUT runs the compacting collector, so saving an image
# while a deep structure is live took the same crashing path.  Also checks the
# image is readable afterwards and the structure survived the round trip.

cat > deep.lsp <<'EOF'
(DE DEEP (N) (PROG (X) LOOP (COND ((ZEROP N) (RETURN X)))
              (SETQ X (LIST X)) (SETQ N (SUB1 N)) (GO LOOP)))
(PROGN (SETQ D (DEEP 3000)) (QUOTE BUILT))
(SYSOUT "deep.img")
(EXIT)
EOF
cat > back.lsp <<'EOF'
(DE DEPTH (X) (PROG (N) (SETQ N 0) LOOP (COND ((NLISTP X) (RETURN N)))
              (SETQ N (ADD1 N)) (SETQ X (CAR X)) (GO LOOP)))
(DEPTH D)
(EXIT)
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 120 "$LISPF4" "$LISPF4_IMG" < deep.lsp > out.txt 2>&1
else
	"$LISPF4" "$LISPF4_IMG" < deep.lsp > out.txt 2>&1
fi
st=$?
if [ "$st" -ne 0 ]; then
	echo "SYSOUT died with a deep structure live (exit status $st)"
	tail -20 out.txt
	exit 1
fi
if [ ! -f deep.img ]; then
	echo "SYSOUT wrote no image"
	tail -20 out.txt
	exit 1
fi
if command -v timeout > /dev/null 2>&1; then
	timeout 120 "$LISPF4" deep.img < back.lsp > back.txt 2>&1
else
	"$LISPF4" deep.img < back.lsp > back.txt 2>&1
fi
if [ $? -ne 0 ] || ! grep -q '^_3000$' back.txt; then
	echo "the saved image did not reload with the structure intact"
	tail -20 back.txt
	exit 1
fi
exit 0
