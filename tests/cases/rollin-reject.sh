# A rejected image must leave the system untouched; a half-read one must stop.
#
# rollin_ checks everything it can (header, configuration, capacity) before it
# modifies any global state, so returning NIL means "nothing was touched" and a
# Lisp-level (ROLLIN N) may safely carry on.  Once the bulk transfer starts,
# a short read leaves the atoms, cells and print names partly overwritten --
# there is no consistent state to return to, so the interpreter stops.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

rc=0

# --- clean rejection: a file that is not an image at all ---
printf '(XCALL 1 (LIST 10 "%s" (QUOTE OLD) (QUOTE UNFORMATTED)))\n(ROLLIN 10)\n(PLUS 40 2)\n(EXIT)\n' \
	"$ROOT/basic1.lisp" > reject.lsp
"$LISPF4" "$LISPF4_IMG" < reject.lsp > reject.out 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "clean rejection: exit status $st (should keep running)"; cat reject.out; rc=1
elif ! grep -q '^_NIL$' reject.out; then
	echo "clean rejection: ROLLIN should have returned NIL"; cat reject.out; rc=1
elif ! grep -q '^_42$' reject.out; then
	echo "clean rejection: the session should still work after a rejected ROLLIN"
	cat reject.out
	rc=1
fi

# --- damaged: a truncated image, whose header and sizes look fine ---
head -c 20000 "$LISPF4_IMG" > trunc.img || exit 1
printf '(XCALL 1 (LIST 10 "trunc.img" (QUOTE OLD) (QUOTE UNFORMATTED)))\n(ROLLIN 10)\n(PLUS 40 2)\n(EXIT)\n' > damaged.lsp
"$LISPF4" "$LISPF4_IMG" < damaged.lsp > damaged.out 2> damaged.err
st=$?
if [ "$st" -ge 128 ]; then
	echo "damaged image: crashed (exit status $st)"; rc=1
elif [ "$st" -eq 0 ]; then
	echo "damaged image: kept running on a partly overwritten heap"
	cat damaged.out
	rc=1
elif ! grep -q 'truncated or unreadable' damaged.err; then
	echo "damaged image: expected the truncation message on stderr"
	cat damaged.err
	rc=1
elif grep -q '^_42$' damaged.out; then
	echo "damaged image: evaluation continued after the failed load"
	cat damaged.out
	rc=1
fi

exit $rc
