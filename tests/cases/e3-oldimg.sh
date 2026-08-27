# E3 compatibility guard: ROLLOUT now appends a two-word trailer recording
# NATOM, because NUMADO pins BIGNUM only to within one and the float shift
# needs it exactly.  An image written before the trailer existed must still
# load -- ROLLIN's probe fails and it falls back to assuming NATOM is
# unchanged -- and the shipped pre-trailer images in Linux/ are the test.

old="$ROOT/Linux/basic.img"
[ -f "$old" ] || exit 0		#  nothing to check on this platform

printf '(PROGN (SYSFLAG 1 NIL) (QUOTE READY))\n(LIST (PLUS 2 3) (QUOTE OK))\n(EXIT)\n' > in.lsp
for opts in "" "-c200000" "-a4000 -p20000"; do
	"$LISPF4" $opts "$old" < in.lsp > out.txt 2>&1
	st=$?
	if [ "$st" -ne 0 ]; then
		echo "pre-trailer image failed to load with [$opts] (exit status $st)"
		cat out.txt
		exit 1
	fi
	grep -q '(5 OK)' out.txt || {
		echo "pre-trailer image with [$opts] gave the wrong answer:"
		cat out.txt
		exit 1
	}
done
exit 0
