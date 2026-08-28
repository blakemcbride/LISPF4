# M7: GET-STATIC was a LAMBDA in a package of NLAMBDAs, so it evaluated the
# function name its callers pass unevaluated -- every other entry point
# (CREATE-STATIC, ADD-STATIC, GET-STATIC's siblings) is an NLAMBDA -- and
# (GET-STATIC F1) failed with "--- Unbound variable / EVAL - F1".  It now
# takes its argument unevaluated like the rest of the package.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }
cp "$ROOT/static.lisp" . || exit 1

cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(LOAD "static.lisp")
(CREATE-STATIC F1 ((C 0)))
(NLSETQ (GET-STATIC F1))
(QUOTE STILL-ALIVE)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

# (GET-STATIC F1) with F1 unbound must answer the static list, not raise.
# NLSETQ makes an "Unbound variable" show up as a plain NIL.
grep -q "Unbound variable" out.txt && {
	echo "GET-STATIC evaluated its argument (it is a LAMBDA among NLAMBDAs)"
	tail -8 out.txt
	exit 1
}
grep -q "((C '0))" out.txt || {
	echo "GET-STATIC did not answer the static list:"
	tail -8 out.txt
	exit 1
}
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; exit 1; }
exit 0
