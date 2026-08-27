# G6/F10: ROLLOUT ignored every write error, so a unit that was not open ran a
# full compacting collection and then answered N as though an image had been
# written.  ROLLIN, ROLLOUT and XCALL also accepted the reserved units, so
# (ROLLOUT 6) sprayed a binary image at the terminal and (XCALL 2 6) closed
# standard output while still exiting 0.

cat > t.lsp <<'EOF'
(ROLLOUT 15)
(ROLLIN 15)
(ROLLOUT 6)
(XCALL 2 6)
(XCALL 2 5)
(XCALL 1 (QUOTE (5 "steal.txt" NEW FORMATTED)))
(PRINT (QUOTE STILL-HERE))
(EXIT)
EOF

if command -v timeout > /dev/null 2>&1; then
	timeout 60 "$LISPF4" "$LISPF4_IMG" < t.lsp > out.txt 2>&1
else
	"$LISPF4" "$LISPF4_IMG" < t.lsp > out.txt 2>&1
fi
st=$?
if [ "$st" -ne 0 ]; then
	echo "died (exit status $st)"
	tail -20 out.txt
	exit 1
fi
if ! grep -q 'STILL-HERE' out.txt; then
	echo "the terminal was lost -- a reserved unit was closed or repointed"
	tail -20 out.txt
	exit 1
fi
if [ -f steal.txt ]; then
	echo "(XCALL 1 ...) repointed standard input at a file"
	exit 1
fi
# A binary image at the terminal would show up as NUL bytes in the transcript.
if [ `wc -c < out.txt` -ne `tr -d '\000' < out.txt | wc -c` ]; then
	echo "(ROLLOUT 6) wrote a binary image to standard output"
	exit 1
fi
exit 0
