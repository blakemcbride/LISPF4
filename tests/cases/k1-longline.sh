# K1: the reader was a 150-column card reader.  RDA1 filled RDBUFF from LMARGR
# to MARGR and called that a line, so a physical line LONGER than the margin
# was cut and the surplus picked up by the next call as a fresh line -- and
# SHIFT hands RATOM an end-of-line, which is a token delimiter.  A 140-character
# atom sitting across column 150 therefore arrived as two atoms, with no
# diagnostic and nothing in the printed form to mark the seam.  A line SHORTER
# than the margin was blank padded to it, so a string spanning a newline
# swallowed the padding: "abc\ndef" came back 144 characters long.
#
# MAKEFILE never writes a line over 78 columns (I2), so the system's own files
# were safe; LOAD and READFILE are the documented way to bring in text written
# by anything else, and there is no (IOTAB 4 N) setting at which a 200-column
# line reads correctly -- RDBUFF is a compile-time 160 words.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

#  One 170-column line holding a 140-character atom and a second atom.
awk 'BEGIN {
	s = ""
	for (i = 0; i < 140; i++) s = s "X"
	printf "(SETQ A (QUOTE (%s ABCDEFGHIJ)))\n", s
	print "(SETQ C \"abc"
	print "def\")"
	print "STOP"
}' > long.lisp

cat > drv.lsp <<'LISP'
(LOAD "long.lisp")
(LENGTH A)
(NCHARS (CAR A))
(CADR A)
(NCHARS C)
C
(QUOTE STILL-ALIVE)
(EXIT)
LISP

"$LISPF4" "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

[ "$st" -ne 0 ] && { echo "exit status $st"; tail -5 out.txt; exit 1; }

#  Two elements were written, so two must come back.
grep -q '^_*2$' out.txt || {
	echo "the 170-column line did not read as a two-element list:"
	grep -n 'LENGTH\|^_*[0-9]*$' out.txt | head
	rc=1
}
grep -q '^_*140$' out.txt || {
	echo "the 140-character atom was split at the card boundary"
	rc=1
}
grep -q '^_*ABCDEFGHIJ$' out.txt || {
	echo "the second atom of the list is not ABCDEFGHIJ"
	rc=1
}
#  "abc" + newline + "def" is six characters, not six plus the blank fill.
grep -q '^_*6$' out.txt || {
	echo "the string across a newline absorbed the card's blank padding"
	rc=1
}
grep -q '^_*"abcdef"$' out.txt || {
	echo "the string across a newline did not come back as abcdef"
	rc=1
}
grep -q '^_*STILL-ALIVE$' out.txt || { echo "session did not survive"; rc=1; }

[ "$rc" -ne 0 ] && tail -20 out.txt
exit $rc
