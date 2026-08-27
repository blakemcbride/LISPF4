# E15: read1() treated a bare CR as end-of-line and left the LF for the next
# f4_start_read, so a CRLF SYSATOMS read as alternating real and blank lines.
# IREAD skips blank lines, so the atom groups still parsed, but MESS's RDA4
# reads exactly MAXMES lines with no such tolerance -- the message table came
# out shifted, three extra atoms were interned, and `lispf4 -x` still exited
# 0, so make would build basic.img on top of the corrupt bare.img.  A
# truncated SYSATOMS was just as quiet.

[ -f "$ROOT/SYSATOMS" ] || { echo "no SYSATOMS in $ROOT"; exit 1; }
[ -f "$ROOT/script.1" ] || { echo "no script.1 in $ROOT"; exit 1; }

#  1. A CRLF SYSATOMS must build the same image as the LF one.
mkdir -p lf crlf
cp "$ROOT/SYSATOMS" lf/SYSATOMS
sed 's/$/\r/' "$ROOT/SYSATOMS" > crlf/SYSATOMS

( cd lf   && "$LISPF4" -x < "$ROOT/script.1" > out.txt 2>&1 ) || {
	echo "LF SYSATOMS failed to build"; cat lf/out.txt; exit 1; }
( cd crlf && "$LISPF4" -x < "$ROOT/script.1" > out.txt 2>&1 )
st=$?
if [ "$st" -ne 0 ]; then
	echo "CRLF SYSATOMS failed to build (exit status $st)"
	cat crlf/out.txt
	exit 1
fi
[ -f lf/bare.img ] && [ -f crlf/bare.img ] || {
	echo "one of the builds wrote no bare.img"; exit 1; }
cmp lf/bare.img crlf/bare.img || {
	echo "a CRLF SYSATOMS built a different image from the LF one"
	exit 1
}

#  2. A truncated SYSATOMS must be refused, loudly, with a non-zero status.
i=1
for n in 5 50 150 174 200; do
	d=t$i; i=`expr $i + 1`
	mkdir -p $d
	head -n $n "$ROOT/SYSATOMS" > $d/SYSATOMS
	( cd $d && "$LISPF4" -x < "$ROOT/script.1" > out.txt 2>&1 )
	st=$?
	if [ "$st" -eq 0 ]; then
		echo "SYSATOMS truncated to $n lines was accepted (exit 0)"
		exit 1
	fi
	if [ -f $d/bare.img ]; then
		echo "SYSATOMS truncated to $n lines still wrote bare.img"
		exit 1
	fi
	tr -d '\000' < $d/out.txt | grep -q SYSATOMS || {
		echo "SYSATOMS truncated to $n lines gave no diagnostic:"
		tr -d '\000' < $d/out.txt
		exit 1
	}
done
exit 0
