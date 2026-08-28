# M3: RSTRING reclassifies ( ) [ ] " ' and % as ordinary letters so it can
# read a line of free text, and used to put them back only on the success path
# -- the last statement of its PROG.  Any error in between (a non-numeric N, a
# short line, an interrupt) left them reclassified, and the session could not
# be recovered because every command that restores them needs a paren to type.
# RSTRING now runs its body under an ERRORSET label and restores CHTAB on every
# exit.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(NLSETQ (RSTRING (QUOTE NOTANUMBER))) some junk text
(LIST 1 2 3)
(QUOTE STILL-ALIVE)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

#  If ( is still a letter, (LIST 1 2 3) reads as one long atom and never
#  evaluates to (1 2 3).
grep -q '^:*_*(1 2 3)$' out.txt || {
	echo "CHTAB was not restored: (LIST 1 2 3) did not read back"
	tail -10 out.txt
	exit 1
}
grep -q '^:*_*STILL-ALIVE$' out.txt || { echo "session did not survive"; exit 1; }
exit 0
