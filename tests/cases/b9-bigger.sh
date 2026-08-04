# B9 guard: raising -c above the image's build-time value must keep working.
# move_() relocates pointers on ROLLIN.  This must not regress.

printf '(PLUS 2 3)\n(TIMES 6 7)\n(EXIT)\n' > in.lsp

"$LISPF4" -c200000 "$LISPF4_IMG" < in.lsp > out.txt 2>&1
st=$?

if [ "$st" -ne 0 ]; then
	echo "-c200000 failed (exit status $st)"
	cat out.txt
	exit 1
fi
if ! grep -q '^_5$' out.txt || ! grep -q '^_42$' out.txt; then
	echo "-c200000 produced wrong results:"
	cat out.txt
	exit 1
fi
exit 0
