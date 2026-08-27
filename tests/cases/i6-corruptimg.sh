# I6: ROLLIN validated the fifteen header words and F4_READU caught a short
# read, so a truncated image and a text file were both rejected -- but nothing
# looked at the tables.  REHASH then took JB and L straight out of the PNP
# table and read PNAME there, so one flipped byte in a print-name index turned
# into a multi-megabyte offset and a SIGSEGV: 5 of 317 single-byte corruptions
# of basic.img crashed the interpreter rather than being refused.  ROLLIN now
# checks the seven header pointers before it writes anything (L90, "nothing
# was touched") and the PNP/CAR/CDR tables after they are read.
#
# This matters beyond damaged files: ROLLIN is callable from Lisp on any unit,
# and a SYSOUT onto a full disk leaves exactly this kind of half-valid file.

cat > run.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (PRINT (QUOTE LOADED)) (EXIT))
EOF

#  The offset the analysis found: inside the PNP print-name index table.
cp "$LISPF4_IMG" one.img || exit 1
printf '\356' | dd of=one.img bs=1 seek=7850 conv=notrunc 2>/dev/null
"$LISPF4" one.img < run.lsp > one.txt 2>&1
st=$?
if [ "$st" -ge 128 ]; then
	echo "a one-byte corruption of the PNP table killed the interpreter (signal, exit status $st)"
	exit 1
fi
if [ "$st" -eq 0 ]; then
	echo "a corrupt image was accepted without a word"
	cat one.txt
	exit 1
fi

#  And a sweep: no single-byte corruption anywhere in the image may produce a
#  signal.  Loading or being refused are both fine.
sz=`wc -c < "$LISPF4_IMG"`
off=0
bad=0
while [ "$off" -lt "$sz" ]; do
	cp "$LISPF4_IMG" f.img || exit 1
	printf '\377' | dd of=f.img bs=1 seek=$off conv=notrunc 2>/dev/null
	if command -v timeout > /dev/null 2>&1; then
		timeout 30 "$LISPF4" f.img < run.lsp > f.txt 2>&1
	else
		"$LISPF4" f.img < run.lsp > f.txt 2>&1
	fi
	st=$?
	if [ "$st" -ge 128 ]; then
		echo "byte $off set to 0xFF: interpreter died with exit status $st"
		bad=`expr $bad + 1`
	fi
	off=`expr $off + 313`
done
[ "$bad" -eq 0 ] || exit 1
exit 0
