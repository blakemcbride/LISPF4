# M4: L1, the interpreter reset, restored the input unit (LUNIN) and the prompt
# but not the output unit (LUNUT).  After any unrecoverable error a session
# whose output had been redirected to a file kept writing into the file and the
# terminal showed nothing but prompts -- and a MAKEFILE that runs out of list
# space redirects output for its whole run and resets, which is exactly the
# case.  L1 now restores LUNUT (and turns SYSFLAG 6 back on) beside LUNIN.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (SETQ U (OPEN0 "cap.txt" NIL NIL)) (OUTUNIT U) (QUOTE REDIRECTED))
(RESET)
(QUOTE ON-TERMINAL)
(PLUS 40 2)
(EXIT)
EOF

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

#  The answer after the reset must reach the terminal (out.txt) ...
grep -q '^_*42$' out.txt || {
	echo "output stayed redirected after the reset -- 42 not on the terminal"
	tail -8 out.txt
	exit 1
}
grep -q '^_*ON-TERMINAL$' out.txt || {
	echo "the form after the reset did not print on the terminal"
	exit 1
}
#  ... and not the file.  cap.txt should hold only what was written before it.
[ -f cap.txt ] || { echo "capture file missing"; exit 1; }
grep -q '42' cap.txt && {
	echo "the post-reset answer went into the file, not the terminal"
	cat cap.txt
	exit 1
}
grep -q 'ON-TERMINAL' cap.txt && {
	echo "post-reset output still going to the file"
	exit 1
}
exit 0
