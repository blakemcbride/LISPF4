# I1: an atom's CAR is both its value cell and its type tag, and
# car(x) == LISPF4-ARRAY *is* the definition of "x is an array".  Storing that
# marker atom as a value therefore made an ordinary literal atom answer the
# test, and its print name -- two or three letters -- was then decoded as an
# array header.  Reads were survivable (ARRAYSIZE just answered nonsense); the
# collector was not, because GARB's step 6 *writes* through the pair ARRUTL
# hands back.  SYSOUT segfaulted every time and left the target file 0 bytes
# long, which is the only defect found so far that destroys a file on disk.
#
# Two independent fixes, either of which closes it: RPLACA (which SETQ, SET,
# SETTOPVAL and RPAQ all reach) refuses to store the three marker atoms into a
# literal atom, and ARRUTL validates the bounds it decodes before anyone
# subscripts with them.

cat > mk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ ZQ (QUOTE LISPF4-ARRAY))
(PROGN (SYSOUT "fake.img") (QUOTE SAVED))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF

"$LISPF4" "$LISPF4_IMG" < mk.lsp > mk.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "SETQ of the array marker then SYSOUT died (exit status $st)"
	cat mk.txt
	exit 1
fi
grep -q SURVIVED mk.txt || {
	echo "the session did not survive:"
	cat mk.txt
	exit 1
}
[ -s fake.img ] || {
	echo "SYSOUT left an empty image file"
	ls -l fake.img 2>&1
	exit 1
}

cat > chk.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(PROGN (PRINT (QUOTE RELOADED)) (EXIT))
EOF
"$LISPF4" fake.img < chk.lsp > chk.txt 2>&1 || {
	echo "the image SYSOUT wrote does not reload"
	cat chk.txt
	exit 1
}
grep -q RELOADED chk.txt || {
	echo "the image SYSOUT wrote reloads but does not run:"
	cat chk.txt
	exit 1
}

#  No SYSOUT is needed: a compacting collection reaches it, and so does the
#  bignum collector, which runs by itself as soon as float arithmetic fills
#  the number area.  Under the sanitizer build this is the case that reports.
cat > gc.lsp <<'EOF'
(PROGN (SYSFLAG 1 NIL) (QUOTE READY))
(SETQ BAR (QUOTE LISPF4-ARRAY))
(PROG (I) (SETQ I 0) LP (TIMES 1.5 (ADD1 I)) (SETQ I (ADD1 I))
          (COND ((LESSP I 20000) (GO LP))) (RETURN (QUOTE OK)))
(PROGN (RECLAIM 1) (RECLAIM 2) (RECLAIM 3) (QUOTE COLLECTED))
(PROGN (PRINT (QUOTE SURVIVED)) (EXIT))
EOF
"$LISPF4" "$LISPF4_IMG" < gc.lsp > gc.txt 2>&1
st=$?
if [ "$st" -ne 0 ]; then
	echo "a collection with the array marker stored died (exit status $st)"
	cat gc.txt
	exit 1
fi
grep -q SURVIVED gc.txt || {
	echo "the session did not survive the collection:"
	cat gc.txt
	exit 1
}
exit 0
