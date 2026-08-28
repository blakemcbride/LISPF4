# M1: K1 made SHIFT stop at the true end of a line for EVERY caller, not just
# the token scanner.  READC then ran on into the NEXT line instead of returning
# the card's blank padding, and RSTRING -- which rewinds the card with
# (READPOS 1) and reads columns 1..N trusting the padding is there -- looped
# forever, taking the session down with "--- EOF read from standard input".
# SHIFT now reads the card (stops at MARGR) for READC/READP and the line
# (stops at RD_LINEEND) for the token scanner, told apart by the entry code.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

#  The free text sits on the same line as (RSTRING), the way the manual shows.
cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ S (RSTRING)) hello there world
(QUOTE AFTER)
(NCHARS S)
(QUOTE STILL-ALIVE)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?

#  Pre-fix the loop never returns; the runner kills it (timeout) or it reads
#  to EOF and exits non-zero.
[ "$st" -ne 0 ] && { echo "exit status $st (RSTRING never returned?)"; tail -5 out.txt; exit 1; }

grep -q 'EOF read from standard input' out.txt && {
	echo "RSTRING ate standard input"
	exit 1
}
grep -q '^_*AFTER$' out.txt || {
	echo "the form after RSTRING was never reached"
	tail -8 out.txt
	exit 1
}
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; exit 1; }
exit 0
