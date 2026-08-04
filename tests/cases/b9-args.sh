# B9: degenerate command-line memory sizes must be rejected cleanly, not crash.

[ -x "$LISPF4" ] || { echo "interpreter $LISPF4 is not executable"; exit 1; }

rc=0

check() {
	desc=$1
	shift
	"$LISPF4" "$@" "$LISPF4_IMG" < /dev/null > out.txt 2>&1
	st=$?
	if [ "$st" -eq 127 ]; then
		echo "$desc: interpreter could not be run (exit status 127)"
		rc=1
	elif [ "$st" -ge 128 ]; then
		echo "$desc: crashed (exit status $st)"
		rc=1
	elif [ "$st" -eq 0 ]; then
		echo "$desc: accepted (exit status 0); expected a clean error exit"
		rc=1
	fi
}

check "-c 0"   -c 0
check "-c 10"  -c 10
check "-a 0"   -a 0
check "-s 0"   -s 0
check "-p 0"   -p 0
check "-c abc" -c abc

exit $rc
