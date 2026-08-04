# B17: a file whose last line has no trailing newline must not lose that line.
#
# READFILE (basic1.lisp) reads forms until it sees the atom STOP, which is why
# every package file ends with a STOP line.  If the final line is dropped, the
# STOP is never seen and READFILE keeps reading -- from standard input -- so the
# symptom is that the rest of the session gets swallowed.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

rc=0

check() {
	desc=$1
	src=$2
	out=$3
	printf '(READFILE "%s")\n(LIST AAA BBB)\n(EXIT)\n' "$src" > drv.lsp
	"$LISPF4" "$LISPF4_IMG" < drv.lsp > "$out" 2>&1
	st=$?
	if [ "$st" -ne 0 ]; then
		echo "$desc: exit status $st"; cat "$out"; rc=1
	elif ! grep -q '^_(111 222)$' "$out"; then
		echo "$desc: expected (111 222) -- the file's last line was lost"
		cat "$out"
		rc=1
	fi
}

# Last line (the STOP) has no terminating newline -- this is the bug.
printf '(SETQ AAA 111)\n(SETQ BBB 222)\nSTOP' > nonl.lisp
check "no trailing newline" nonl.lisp out1.txt

# Control: identical file that does end with a newline.
printf '(SETQ AAA 111)\n(SETQ BBB 222)\nSTOP\n' > withnl.lisp
check "trailing newline" withnl.lisp out2.txt

# Standard input with no final newline must still work and still terminate.
printf '(PLUS 7 8)\n(EXIT)' > drv3.lsp
"$LISPF4" "$LISPF4_IMG" < drv3.lsp > out3.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "stdin with no trailing newline: exit status $st"; cat out3.txt; rc=1
elif ! grep -q '^_15$' out3.txt; then
	echo "stdin with no trailing newline: expected 15"
	cat out3.txt
	rc=1
fi

# EOF on standard input must still be reported exactly once (no hang, no loop).
printf '(PLUS 1 1)\n' > drv4.lsp
"$LISPF4" "$LISPF4_IMG" < drv4.lsp > out4.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "EOF on stdin: exit status $st"; cat out4.txt; rc=1
elif ! grep -q 'EOF read from standard input' out4.txt; then
	echo "EOF on stdin: expected the end-of-file message"
	cat out4.txt
	rc=1
fi

exit $rc
