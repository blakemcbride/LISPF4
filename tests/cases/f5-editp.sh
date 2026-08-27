# F5: EDITP called EDITS-INT with two arguments where it takes three, so EDCOM
# was NIL -- the commands were ignored and the editor prompted at the terminal
# instead -- and S-OLD was the command list, which STOP then installed as the
# atom's property list.  Feeding the commands on stdin is what the old EDITP
# consumed; the fixed one takes them from the call and never reads them, so the
# case checks the property list rather than the transcript.

cat > t.lsp <<'EOF'
(PUTPROP (QUOTE ZZ) (QUOTE PA) (QUOTE (1 2 3)))
(EDITP ZZ P OK)
(GETPROP (QUOTE ZZ) (QUOTE PA))
(PUTPROP (QUOTE YY) (QUOTE QB) (QUOTE (7 8)))
(EDITP YY (2 (9 9 9)) OK)
(GETPROP (QUOTE YY) (QUOTE QB))
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
if ! grep -q '^_(1 2 3)$' out.txt; then
	echo "EDITP destroyed the property list it was editing:"
	tail -20 out.txt
	exit 1
fi
if ! grep -q '^_(9 9 9)$' out.txt; then
	echo "EDITP did not run its commands or did not write the result back:"
	tail -20 out.txt
	exit 1
fi
exit 0
