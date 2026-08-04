# L1: ifdo.lisp must load, and its infix comparison operators must work.
#
# ITP-C translates infix operators.  Four of them used to be spelled [ ] [= ]=,
# which are the super-parenthesis characters -- the reader tore the DEFINEQ
# apart and the package could not be loaded at all.  They are now < > <= >=.
# Also covers prolog.lisp's POP/PUSH, which must mutate the caller's variable.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cp "$ROOT/match.lisp" "$ROOT/ifdo.lisp" "$ROOT/prolog.lisp" . || exit 1

cat > drv.lsp <<'EOF'
(READFILE "match.lisp")
(READFILE "ifdo.lisp")
(IF (EQ 1 1) THEN (QUOTE YES) ELSE (QUOTE NO))
(ITP 1 < 2)
(ITP 5 > 2)
(ITP 3 > 9)
(ITP 2 <= 2)
(ITP 3 >= 9)
(ITP 3 + 4)
(ITP 12 / 4)
(IF (ITP 3 >= 9) THEN (QUOTE GEQ) ELSE (QUOTE NOTGEQ))
(SETQ I 0)
(DO WHILE (LESSP I 5) DO (SETQ I (ADD1 I)))
I
(READFILE "prolog.lisp")
(SETQ E (QUOTE (A B)))
(POP E)
E
(SETQ Q NIL)
(PUSH (QUOTE Z) Q)
Q
(FINAL (QUOTE (A NIL B)))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

if [ "$st" -ne 0 ]; then
	echo "exit status $st"; cat out.txt; exit 1
fi
if grep -qE "^--- (Unbound|Undefined)" out.txt; then
	echo "a package failed to load or called an undefined function:"
	grep -A2 -E "^--- (Unbound|Undefined)" out.txt | head -6
	rc=1
fi

want() {
	if ! grep -q "^_$1\$" out.txt; then
		echo "expected a result line '$1'"
		rc=1
	fi
}
want 'YES'
# ITP is the infix evaluator; IF itself takes a single condition form.
want 'T'          # (ITP 1 < 2)
want 'NIL'        # (ITP 3 > 9)
want '7'          # (ITP 3 + 4)
want '3'          # (ITP 12 / 4)
want 'NOTGEQ'     # (IF (ITP 3 >= 9) THEN ... ELSE ...)
want '5'          # DO WHILE ran to completion
want 'A'          # (POP E) returned the head
want '(B)'        # ...and mutated E
want '(Z)'        # PUSH mutated Q
want '(B)'        # (FINAL '(A NIL B)) -- MEMB, not the undefined MEMQ

[ "$rc" -ne 0 ] && cat out.txt
exit $rc
