# D20: OPENF was a permanent stub, so the documented OPEN0 builtin could only
# ever answer NIL; f4_open closed the unit's old stream before it knew the new
# one would open; EJECT wrote a blank instead of a form feed.

printf 'hello\nSTOP\n' > t.txt

cat > o0.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (SETQ U (OPEN0 "t.txt" T NIL)) (COND ((NULL U) (QUOTE NOTOPEN)) (T (QUOTE OPENED))))
(PROGN (XCALL 2 U) (QUOTE CLOSED))
(COND ((OPEN0 "no/such/dir/nofile" T NIL) (QUOTE WRONGLYOPENED)) (T (QUOTE REFUSED)))
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < o0.lsp > out.txt 2>&1
st=$?
[ "$st" -eq 0 ] || { echo "died (exit status $st)"; cat out.txt; exit 1; }
grep -q 'OPENED'  out.txt || { echo "OPEN0 could not open an existing file:"; cat out.txt; exit 1; }
grep -q 'REFUSED' out.txt || { echo "OPEN0 claimed to open a nonexistent file:"; cat out.txt; exit 1; }

# EJECT must emit a form feed, not a blank.
printf '(EJECT)\n(EXIT)\n' | "$LISPF4" "$LISPF4_IMG" > ff.txt 2>&1
st=$?
[ "$st" -eq 0 ] || { echo "(EJECT) died (exit status $st)"; cat ff.txt; exit 1; }
if ! od -c ff.txt | grep -q '\\f'; then
	echo "(EJECT) wrote no form feed"
	exit 1
fi
exit 0
