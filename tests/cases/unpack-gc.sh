# UNPACK and NTHCHAR must survive a collection landing inside them.
#
# UNPACK walks the shared print buffer PRBUFF backwards, using PRTPOS as its
# cursor, and conses a character atom at each step.  Any of those allocations
# can trigger a garbage collection, and the collector used to print its
# "--- GBC. Free cells =" message through that same buffer -- flushing it,
# blanking it, and assigning PRTPOS = 12 on the way past.  UNPACK then carried
# on with a destroyed cursor over a destroyed buffer and returned nonsense.
#
# The symptom was quiet: a caller such as (NTHCHAR X 1) got the wrong
# character, so anything classifying atoms by their first character silently
# misclassified one now and then.  It also sprayed the atom's own print name
# into the middle of the collector's message.
#
# garb_ now saves PRTPOS and PRBUFF on entry and restores them at the return,
# and clears the buffer before printing rather than flushing it.
#
# Cell space is squeezed with -c so that collections are frequent enough to
# land inside UNPACK.  Against the pre-fix interpreter this reports about 25
# bad results; it must report none.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

cat > drv.lsp <<'EOF'
(DEFINEQ
(HAM
  [LAMBDA (N)
          (PROG ((K 0) (B1 0) (B2 0) (B3 0) (B4 0))
            LP  (COND ((GREATERP K N)
                        (RETURN (LIST (QUOTE NTHCHAR1) B1 (QUOTE UNPACK1) B2
                                      (QUOTE NTHCHAR2) B3 (QUOTE UNPACK2) B4))))
                (SETQ K (ADD1 K))
                (OR (EQ (NTHCHAR (QUOTE ?ABC) 1) (QUOTE ?)) (SETQ B1 (ADD1 B1)))
                (OR (EQUAL (UNPACK (QUOTE ?AB)) (QUOTE (? A B))) (SETQ B2 (ADD1 B2)))
                (OR (EQ (NTHCHAR (QUOTE ?LONGERNAME) 2) (QUOTE L)) (SETQ B3 (ADD1 B3)))
                (OR (EQUAL (UNPACK (QUOTE ABCDEFGH)) (QUOTE (A B C D E F G H)))
                    (SETQ B4 (ADD1 B4)))
                (GO LP])
)
(PRINT (HAM 6000))
(EXIT)
EOF

"$LISPF4" -c 20000 "$LISPF4_IMG" < drv.lsp > out.txt 2>&1
st=$?

if [ "$st" -ne 0 ]; then
	echo "exit status $st"; cat out.txt; exit 1
fi
if grep -qE "^--- (Unbound|Undefined|Illegal)" out.txt; then
	echo "driver failed to run:"
	grep -A2 -E "^--- (Unbound|Undefined|Illegal)" out.txt | head -6
	exit 1
fi
if grep -q "(NTHCHAR1 0 UNPACK1 0 NTHCHAR2 0 UNPACK2 0)" out.txt; then
	exit 0
fi

echo "UNPACK/NTHCHAR returned wrong results across a collection:"
grep -o "(NTHCHAR1 .*" out.txt | head -1
exit 1
