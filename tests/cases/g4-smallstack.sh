# G4: -s values the configuration validator accepted (100..150) made HILLW
# negative, so an empty parameter stack tested as full.  The overflow handler
# escalated to "fatal", reset to L1, which recomputed HILLW from HILL -- an
# infinite stream of "Parameter stack owerflow" that never read standard input
# and never exited.  A rejected configuration must exit non-zero and promptly.

for s in 100 120 150 200 499; do
	if command -v timeout > /dev/null 2>&1; then
		printf '(PLUS 1 2)\n(EXIT)\n' | timeout 10 "$LISPF4" -s $s "$LISPF4_IMG" > out.txt 2>&1
	else
		printf '(PLUS 1 2)\n(EXIT)\n' | "$LISPF4" -s $s "$LISPF4_IMG" > out.txt 2>&1
	fi
	st=$?
	if [ "$st" -eq 124 ]; then
		echo "-s $s hung"
		exit 1
	fi
	if [ "$st" -eq 0 ]; then
		echo "-s $s was accepted, but the interpreter cannot run in it"
		exit 1
	fi
	if [ `wc -c < out.txt` -gt 4000 ]; then
		echo "-s $s produced `wc -c < out.txt` bytes -- looks like the error loop"
		exit 1
	fi
done

# The smallest accepted value must actually work.
if command -v timeout > /dev/null 2>&1; then
	printf '(PLUS 1 2)\n(EXIT)\n' | timeout 30 "$LISPF4" -s 500 "$LISPF4_IMG" > ok.txt 2>&1
else
	printf '(PLUS 1 2)\n(EXIT)\n' | "$LISPF4" -s 500 "$LISPF4_IMG" > ok.txt 2>&1
fi
if [ $? -ne 0 ] || ! grep -q '^_3$' ok.txt || grep -q owerflow ok.txt; then
	echo "-s 500 is the documented minimum but does not work:"
	tail -20 ok.txt
	exit 1
fi
exit 0
