# H5: four of the ten loadable packages call functions another package
# defines, and neither loaded nor documented it.  struct.lisp and
# astruct.lisp use MATCH/LMATCH from match.lisp; prolog.lisp and printa.lisp
# use DO from ifdo.lisp, which itself needs match.lisp.  Loading any of them
# as the documentation describes gave "Undefined function" at first use.
#
# Each file now pulls what it needs, before its own FILEHEADER -- a nested
# FILEHEADER moves CURFILE, so a load placed after it would file the
# package's own functions under the wrong name.
#
# Both documented entry points are exercised.  (LOAD f) opens on the first
# free unit at or above 20; READFILE now asks OPEN0 for a free unit instead
# of always taking 15, which is what makes a READFILE inside a READFILE work
# -- F4_OPEN silently closes and reuses a unit that is already open, so the
# fixed unit did not merely fail, it pulled the outer file out from under
# the reader.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cp "$ROOT/match.lisp" "$ROOT/ifdo.lisp" "$ROOT/struct.lisp" \
   "$ROOT/astruct.lisp" "$ROOT/prolog.lisp" "$ROOT/printa.lisp" . || exit 1

rc=0

# $1 = loader, $2 = package, $3.. = forms that must work afterwards
check() {
	loader=$1; pkg=$2; shift 2
	{
		echo "(SYSFLAG 1 NIL)"
		echo "($loader \"$pkg.lisp\")"
		for form in "$@"; do echo "$form"; done
		echo "(QUOTE STILL-ALIVE)"
		echo "(EXIT)"
	} > drv.lsp

	"$LISPF4" "$LISPF4_IMG" < drv.lsp > "out-$loader-$pkg.txt" 2>&1
	st=$?
	if [ "$st" -ne 0 ]; then
		echo "$loader $pkg: exit status $st"
		rc=1
		return
	fi
	if grep -qE "^--- (Unbound|Undefined)" "out-$loader-$pkg.txt"; then
		echo "$loader $pkg: an undefined function was called"
		grep -A2 -E "^--- (Unbound|Undefined)" "out-$loader-$pkg.txt" | head -6
		rc=1
	fi
	if ! grep -q '^_*STILL-ALIVE$' "out-$loader-$pkg.txt"; then
		echo "$loader $pkg: the session did not reach the end of the script"
		rc=1
	fi
}

array='(PROGN (SETQ A (ARRAY 6 2 2)) (SETA A 1 (QUOTE P1)) (SETI A 1 11) (SETR A 1 1.5) (QUOTE FILLED))'

for loader in LOAD READFILE; do
	check $loader struct  '(SPUT (QUOTE FOO) (QUOTE K) (QUOTE V))' \
	                      '(SGET (QUOTE FOO) (QUOTE K))'
	check $loader astruct '(APUT (QUOTE BAR) (QUOTE K) (QUOTE V))' \
	                      '(AGET (QUOTE BAR) (QUOTE K))'
	# RESET* is written with DO -- POP, PUSH and FINAL all happen to
	# take paths that never reach ifdo.lisp, so without it the case
	# would prove nothing.
	check $loader prolog  '(SETQ E (QUOTE (A B)))' '(POP E)' 'E' \
	                      '(FINAL (QUOTE (A NIL B)))' '(RESET* NIL NIL)'
	check $loader printa  "$array" '(PRINTA A)'
	check $loader ifdo    '(SETQ I 0)' \
	                      '(DO WHILE (LESSP I 5) DO (SETQ I (ADD1 I)))' 'I'
done

# Each package must also come up with its prerequisite really defined, not
# merely with no error raised because nothing reached the call.
{
	echo "(SYSFLAG 1 NIL)"
	echo '(LOAD "printa.lisp")'
	echo "(LIST (NULL (GETD (QUOTE MATCH))) (NULL (GETD (QUOTE LMATCH))) (NULL (GETD (QUOTE DO))))"
	echo "(EXIT)"
} > deps.lsp
"$LISPF4" "$LISPF4_IMG" < deps.lsp > out-deps.txt 2>&1
if ! grep -q '^_*(NIL NIL NIL)$' out-deps.txt; then
	echo "printa.lisp did not bring MATCH, LMATCH and DO with it:"
	grep -E '^_*\((NIL|T)' out-deps.txt
	rc=1
fi

exit $rc
