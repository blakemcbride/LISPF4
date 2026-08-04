# SYSOUT / SYSIN round-trip.  Guards the image format against Phase 2 changes.

printf '(SETQ ZZZ (QUOTE (A B C)))\n(SYSOUT "round.img")\n(EXIT)\n' > save.lsp
"$LISPF4" "$LISPF4_IMG" < save.lsp > save.out 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "SYSOUT run failed (exit status $st)"; cat save.out; exit 1
fi
if [ ! -s round.img ]; then
	echo "SYSOUT did not produce round.img"; cat save.out; exit 1
fi

printf 'ZZZ\n(EXIT)\n' > load.lsp
"$LISPF4" round.img < load.lsp > load.out 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "reload run failed (exit status $st)"; cat load.out; exit 1
fi
if ! grep -q '^_(A B C)$' load.out; then
	echo "value did not survive the SYSOUT/SYSIN round-trip:"
	cat load.out
	exit 1
fi
exit 0
