# H3: STRALLOC captured a print-name byte offset with GETPN, then called
# MATOM -- which can run the atom-compacting collector and move print names --
# and read through the offset afterwards.  SUBSTRING, twenty lines further
# down in the same file, re-fetches after its MATOM; STRALLOC did not.
#
# This is a GUARD, not a detector.  The offset really did go stale on every
# call (measured with a build that forced GARB(3) at the point MATOM could
# collect), but it never produced a wrong character: STEP 4 compacts print
# names downward and does not erase what it vacates, so a stale offset still
# found the old bytes.  The case pins the behaviour so that a later change to
# the collector -- one that reuses or clears the vacated region -- turns into
# a test failure here rather than into silently corrupt strings.
#
# PROMPTTEXT is here for the same reason: it held its argument in a C local
# across the same MATOM, so the index it later handed to GETPN named an atom
# above the live region once the collector had moved things down.
#
# -a2500 -p3000 is tight enough that the loop below drives a dozen atom-
# compacting collections; the counter in the transcript proves they happened.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'LISP'
(SYSFLAG 1 NIL)
(SETQ BAD 0)
(SETQ N 0)
(PROG NIL
 LP  (SETQ N (ADD1 N))
     (COND ((NULL (EQUAL (STRALLOC 20 "ZQ") "ZZZZZZZZZZZZZZZZZZZZ"))
             (SETQ BAD (ADD1 BAD))))
     (COND ((NULL (EQUAL (STRALLOC 3 (SUBSTRING "abcdef" 4 6)) "ddd"))
             (SETQ BAD (ADD1 BAD))))
     (COND ((NULL (EQUAL (PROMPTTEXT "&") "_")) (SETQ BAD (ADD1 BAD))))
     (COND ((NULL (EQUAL (PROMPTTEXT "_") "&")) (SETQ BAD (ADD1 BAD))))
     (COND ((LESSP N 4000) (GO LP)))
     (RETURN (LIST (QUOTE CALLS) N (QUOTE BAD) BAD)))
(QUOTE STILL-ALIVE)
(EXIT)
LISP

"$LISPF4" -a2500 -p3000 "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?
rc=0

if [ "$st" -ne 0 ]; then
	if grep -q 'not a valid Lisp F4 image' out.txt; then
		echo "-a2500 -p3000 no longer fits basic.img -- raise both, keeping"
		echo "them tight enough that the loop still forces a collection"
	fi
	echo "exit status $st"; tail -5 out.txt; exit 1
fi
if ! grep -q '(CALLS 4000 BAD 0)' out.txt; then
	echo "STRALLOC returned a wrong string:"
	grep 'CALLS' out.txt
	rc=1
fi
if ! grep -q '^_*STILL-ALIVE$' out.txt; then
	echo "the session did not reach the end of the script"
	rc=1
fi
# The last line of the GBC tally is the atom-compacting count.  Without one
# the loop never exercised what the case is about.
if grep -q '(0 0 0 0)' out.txt; then
	echo "no atom-compacting collection happened -- the case proves nothing"
	rc=1
fi

[ "$rc" -ne 0 ] && tail -12 out.txt
exit $rc
