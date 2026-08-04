# B8: a file that is not an image at all must be rejected cleanly, not crash.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

"$LISPF4" "$ROOT/basic1.lisp" < /dev/null > out.txt 2>&1
st=$?

if [ "$st" -eq 127 ]; then
	echo "interpreter could not be run (exit status 127)"
	exit 1
fi
if [ "$st" -ge 128 ]; then
	echo "crashed on a non-image file (exit status $st)"
	exit 1
fi
if [ "$st" -eq 0 ]; then
	echo "accepted a non-image file (exit status 0); expected a clean error exit"
	exit 1
fi
exit 0
