# prolog2.lisp: the Prolog interpreter must load and solve.
#
# Each check evaluates to a distinctive atom so that a wrong answer cannot be
# confused with the right answer to some other query.  The cut and negation
# checks compare solution COUNTS, because that is what those features change:
# without the cut MAX yields two answers, and without NOT the filter yields
# three.  LOOP-OK covers the runaway guard -- a left-recursive predicate must
# come back empty-handed rather than exhaust the parameter stack.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cp "$ROOT/prolog2.lisp" . || exit 1

cat > drv.lsp <<'EOF'
(READFILE "prolog2.lisp")

(COND ((EQ (PINST (QUOTE ?X) (PUNIFY (QUOTE (F ?X B)) (QUOTE (F A ?Y)) NIL))
           (QUOTE A))
        (QUOTE UNIFY-OK))
      (T (QUOTE UNIFY-BAD)))
(COND ((EQ (PUNIFY (QUOTE (F A)) (QUOTE (F B)) NIL) (QUOTE PFAIL))
        (QUOTE CLASH-OK))
      (T (QUOTE CLASH-BAD)))

(<- (PARENT TOM BOB))
(<- (PARENT TOM LIZ))
(<- (PARENT BOB ANN))
(<- (PARENT ANN JIM))
(<- (ANC ?X ?Y) (PARENT ?X ?Y))
(<- (ANC ?X ?Z) (PARENT ?X ?Y) (ANC ?Y ?Z))
(COND ((EQ (LENGTH (PQUERY (QUOTE ((ANC TOM ?D))) NIL)) 4) (QUOTE ANC-OK))
      (T (QUOTE ANC-BAD)))
(COND ((PQUERY (QUOTE ((ANC JIM ?D))) NIL) (QUOTE NOANC-BAD))
      (T (QUOTE NOANC-OK)))

(<- (APP NIL ?L ?L))
(<- (APP (?H . ?T) ?L (?H . ?R)) (APP ?T ?L ?R))
(COND ((EQUAL (PINST (QUOTE ?Z) (CAR (PQUERY (QUOTE ((APP (A B) (C D) ?Z))) NIL)))
              (QUOTE (A B C D)))
        (QUOTE APP-OK))
      (T (QUOTE APP-BAD)))
(COND ((EQ (LENGTH (PQUERY (QUOTE ((APP ?X ?Y (A B C)))) NIL)) 4) (QUOTE SPLIT-OK))
      (T (QUOTE SPLIT-BAD)))

(<- (REV NIL NIL))
(<- (REV (?H . ?T) ?R) (REV ?T ?RT) (APP ?RT (?H) ?R))
(COND ((EQUAL (PINST (QUOTE ?R) (CAR (PQUERY (QUOTE ((REV (A B C D) ?R))) NIL)))
              (QUOTE (D C B A)))
        (QUOTE REV-OK))
      (T (QUOTE REV-BAD)))

(<- (MAX ?X ?Y ?X) (LISP (NULL (LESSP ?X ?Y))) !)
(<- (MAX ?X ?Y ?Y))
(COND ((AND (EQ (LENGTH (PQUERY (QUOTE ((MAX 5 3 ?M))) NIL)) 1)
            (EQ (PINST (QUOTE ?M) (CAR (PQUERY (QUOTE ((MAX 5 3 ?M))) NIL))) 5)
            (EQ (PINST (QUOTE ?M) (CAR (PQUERY (QUOTE ((MAX 3 5 ?M))) NIL))) 5))
        (QUOTE CUT-OK))
      (T (QUOTE CUT-BAD)))

(<- (P 1))
(<- (P 2))
(<- (P 3))
(<- (ONE ?N) (P ?N) !)
(COND ((EQ (LENGTH (PQUERY (QUOTE ((ONE ?N))) NIL)) 1) (QUOTE CUTBAR-OK))
      (T (QUOTE CUTBAR-BAD)))

(<- (Q 2))
(<- (NOTQ ?N) (P ?N) (NOT (Q ?N)))
(COND ((EQ (LENGTH (PQUERY (QUOTE ((NOTQ ?N))) NIL)) 2) (QUOTE NOT-OK))
      (T (QUOTE NOT-BAD)))

(<- (FACT 0 1) !)
(<- (FACT ?N ?F) (LISP (GREATERP ?N 0)) (IS ?M (SUB1 ?N)) (FACT ?M ?G)
                 (IS ?F (TIMES ?N ?G)))
(COND ((EQ (PINST (QUOTE ?F) (CAR (PQUERY (QUOTE ((FACT 6 ?F))) NIL))) 720)
        (QUOTE IS-OK))
      (T (QUOTE IS-BAD)))

(<- (LOOPY ?X) (LOOPY ?X))
(COND ((PQUERY (QUOTE ((LOOPY A))) NIL) (QUOTE LOOP-BAD)) (T (QUOTE LOOP-OK)))

(PCLEAR (QUOTE P))
(COND ((PQUERY (QUOTE ((P ?N))) NIL) (QUOTE CLEAR-BAD)) (T (QUOTE CLEAR-OK)))
(PCLEAR)
(COND (*PPREDS* (QUOTE WIPE-BAD)) (T (QUOTE WIPE-OK)))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

if [ "$st" -ne 0 ]; then
	echo "exit status $st"; cat out.txt; exit 1
fi
if grep -qE "^--- (Unbound|Undefined|Illegal)" out.txt; then
	echo "prolog2.lisp failed to load or called an undefined function:"
	grep -A2 -E "^--- (Unbound|Undefined|Illegal)" out.txt | head -6
	rc=1
fi
if grep -q "owerflow" out.txt; then
	echo "the depth guard did not hold -- the parameter stack overflowed"
	rc=1
fi

want() {
	if ! grep -q "^_*$1\$" out.txt; then
		echo "expected a result line '$1'"
		rc=1
	fi
}
want 'UNIFY-OK'
want 'CLASH-OK'
want 'ANC-OK'
want 'NOANC-OK'
want 'APP-OK'
want 'SPLIT-OK'
want 'REV-OK'
want 'CUT-OK'
want 'CUTBAR-OK'
want 'NOT-OK'
want 'IS-OK'
want 'LOOP-OK'
want 'CLEAR-OK'
want 'WIPE-OK'

[ "$rc" -ne 0 ] && cat out.txt
exit $rc
