# B8: a truncated image file must be rejected, not silently loaded.
# Runs in the driver's work directory; $LISPF4, $LISPF4_IMG, $ROOT are exported.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

head -c 20000 "$LISPF4_IMG" > trunc.img || exit 1

"$LISPF4" trunc.img < /dev/null > out.txt 2>&1
st=$?

if [ "$st" -eq 127 ]; then
	echo "interpreter could not be run (exit status 127)"
	exit 1
fi
if [ "$st" -ge 128 ]; then
	echo "crashed on a truncated image (exit status $st)"
	exit 1
fi
if [ "$st" -eq 0 ]; then
	echo "accepted a truncated image (exit status 0); expected a clean error exit"
	exit 1
fi
exit 0
