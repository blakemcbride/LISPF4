# prolog2 under cell GC pressure: every answer must be the right answer.
#
# This was a known failure.  The cause was in the interpreter, not in prolog2:
# UNPACK walks the shared print buffer and conses as it goes, and a collection
# landing inside it destroyed its cursor -- see tests/cases/unpack-gc.sh and
# the note in garb_.  PVARP classifies a term by its first character, so a
# corrupted UNPACK made it answer "not a variable" for a variable, no binding
# was made, and the variable came back unbound.
#
# The same query is run 301 times.  It is a pure function of a database that
# never changes, so all 301 answers must be identical and correct.
#
# The loop is written with PROG/GO rather than a package iterator so that the
# only things under test are the base interpreter and prolog2.lisp.
#
# Do not tidy the driver.  Which iterations collected depended on the exact
# heap state, so cosmetic changes -- even returning a shorter value from
# SWEEP -- shifted the collections and could hide the fault entirely.  This
# wording is one that reproduced it.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cp "$ROOT/prolog2.lisp" . || exit 1

cat > drv.lsp <<'EOF'
(READFILE "prolog2.lisp")
(<- (APP NIL ?L ?L))
(<- (APP (?H . ?T) ?L (?H . ?R)) (APP ?T ?L ?R))
(<- (REV NIL NIL))
(<- (REV (?H . ?T) ?R) (REV ?T ?RT) (APP ?RT (?H) ?R))
(DEFINEQ
(SWEEP
  [LAMBDA (N)
          (PROG ((K 0) (BAD 0) (FB NIL) V)
            LP  (COND ((GREATERP K N) (RETURN (LIST (QUOTE ITERS) K (QUOTE BAD) BAD
                                                    (QUOTE FIRSTBAD) FB))))
                (SETQ K (ADD1 K))
                (SETQ V (PQUERY (QUOTE ((REV (A B C D E F) ?R))) NIL))
                (OR (EQUAL (PINST (QUOTE ?R) (CAR V)) (QUOTE (F E D C B A)))
                    (PROGN (SETQ BAD (ADD1 BAD)) (AND (NULL FB) (SETQ FB K))))
                (GO LP])
)
(PRINT (SWEEP 300))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?

if [ "$st" -ne 0 ]; then
	echo "exit status $st"; cat out.txt; exit 1
fi
if grep -qE "^--- (Unbound|Undefined|Illegal)" out.txt; then
	echo "prolog2.lisp failed to load:"
	grep -A2 -E "^--- (Unbound|Undefined|Illegal)" out.txt | head -6
	exit 1
fi
if grep -q "BAD 0 FIRSTBAD NIL" out.txt; then
	exit 0
fi

echo "a query returned a wrong answer under GC pressure:"
grep -o "(ITERS .*" out.txt | head -1
exit 1
