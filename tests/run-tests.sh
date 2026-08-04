#!/bin/sh
#
# LISPF4 regression suite.
#
#   ./tests/run-tests.sh              run everything
#   ./tests/run-tests.sh b1 b3        run cases whose name contains b1 or b3
#
# Environment overrides:
#   LISPF4      interpreter to test      (default: ../lispf4)
#   LISPF4_IMG  image to load            (default: ../basic.img)
#   TIMEOUT     seconds per case         (default: 300)
#
# Two kinds of case live in cases/:
#
#   NAME.lsp    fed to the interpreter on stdin.  Always checked for a clean
#               exit (no signal, no timeout).  If NAME.exp exists, the
#               normalised output must match it exactly.
#   NAME.sh     shell script run in a scratch directory with $LISPF4,
#               $LISPF4_IMG and $ROOT exported.  Exit 0 means pass.
#
# A NAME.bug file marks a case as a known failure: it names the bug from
# Bugs1.md that causes it.  Known failures are reported but do not fail the
# run.  When a phase of Plan1.md lands, delete the corresponding .bug file --
# the case then becomes a real regression test.
#
# Exit status: 0 if every case either passed or is a known failure; 1 if any
# case failed unexpectedly, or if a known-failure case has started passing
# (which means its .bug file should be removed).

here=`cd \`dirname "$0"\` && pwd`
ROOT=`cd "$here/.." && pwd`
export ROOT

LISPF4=${LISPF4:-"$ROOT/lispf4"}
LISPF4_IMG=${LISPF4_IMG:-"$ROOT/basic.img"}
TIMEOUT=${TIMEOUT:-300}

# Cases run in scratch subdirectories, so relative paths must be resolved now.
abspath() {
	case "$1" in
		/*) echo "$1" ;;
		*)  echo "`cd \`dirname \"$1\"\` 2>/dev/null && pwd`/`basename \"$1\"`" ;;
	esac
}
LISPF4=`abspath "$LISPF4"`
LISPF4_IMG=`abspath "$LISPF4_IMG"`
export LISPF4 LISPF4_IMG

if [ ! -x "$LISPF4" ]; then
	echo "no interpreter at $LISPF4 -- run: make" >&2
	exit 2
fi
if [ ! -f "$LISPF4_IMG" ]; then
	echo "no image at $LISPF4_IMG -- run: make" >&2
	exit 2
fi

work="$here/.work"
rm -rf "$work"
mkdir -p "$work" || exit 2

pass=0
fail=0
known=0
unexpected_pass=0
failed_list=""
unexpected_list=""

# Does this case name match the filters given on the command line?
selected() {
	name=$1
	shift
	[ $# -eq 0 ] && return 0
	for pat in "$@"; do
		case "$name" in
			*"$pat"*) return 0 ;;
		esac
	done
	return 1
}

run_limited() {
	if [ -n "$have_timeout" ]; then
		timeout "$TIMEOUT" "$@"
	else
		"$@"
	fi
}

have_timeout=
command -v timeout >/dev/null 2>&1 && have_timeout=yes

# Strip everything that varies between runs or builds:
#   - the startup banner (build date, free-cell counts) up to and
#     including the first "--- Reset"
#   - the shutdown block from "Exit from Lisp F4" onwards
#   - "_" and ":" prompt characters at the start of a line
#   - trailing blanks, and blank lines
normalize() {
	sed -e '1,/^--- Reset/d' -e '/^Exit from Lisp F4/,$d' \
	| sed -e 's/^[_:]*//' -e 's/[ 	]*$//' \
	| sed -e '/^$/d'
}

report() {
	# report <status> <name> <detail>
	st=$1; name=$2; detail=$3
	bug=
	[ -f "$here/cases/$name.bug" ] && bug=`cat "$here/cases/$name.bug"`

	if [ "$st" = pass ]; then
		if [ -n "$bug" ]; then
			echo "XPASS $name   (marked as $bug -- remove tests/cases/$name.bug)"
			unexpected_pass=`expr $unexpected_pass + 1`
			unexpected_list="$unexpected_list $name"
		else
			echo "PASS  $name"
			pass=`expr $pass + 1`
		fi
	else
		if [ -n "$bug" ]; then
			echo "known $name   ($bug not yet fixed) $detail"
			known=`expr $known + 1`
		else
			echo "FAIL  $name   $detail"
			fail=`expr $fail + 1`
			failed_list="$failed_list $name"
		fi
	fi
}

run_lsp_case() {
	name=$1
	lsp="$here/cases/$name.lsp"
	out="$work/$name.out"

	( cd "$work" && run_limited "$LISPF4" "$LISPF4_IMG" ) < "$lsp" > "$out" 2>&1
	st=$?

	if [ "$st" -ge 128 ]; then
		report fail "$name" "(killed by signal, exit status $st)"
		return
	fi
	if [ "$st" -eq 124 ] && [ -n "$have_timeout" ]; then
		report fail "$name" "(timed out after ${TIMEOUT}s)"
		return
	fi
	if [ "$st" -ne 0 ]; then
		report fail "$name" "(exit status $st)"
		return
	fi

	if [ -f "$here/cases/$name.exp" ]; then
		normalize < "$out" > "$work/$name.norm"
		if diff -u "$here/cases/$name.exp" "$work/$name.norm" > "$work/$name.diff" 2>&1; then
			report pass "$name" ""
		else
			report fail "$name" "(output differs; see tests/.work/$name.diff)"
		fi
	else
		# No .exp: this is a crash-only case.
		report pass "$name" ""
	fi
}

run_sh_case() {
	name=$1
	sh_file="$here/cases/$name.sh"
	out="$work/$name.out"
	casedir="$work/$name.d"

	mkdir -p "$casedir"
	( cd "$casedir" && run_limited sh "$sh_file" ) > "$out" 2>&1
	st=$?

	if [ "$st" -ge 128 ]; then
		report fail "$name" "(killed by signal, exit status $st)"
	elif [ "$st" -eq 124 ] && [ -n "$have_timeout" ]; then
		report fail "$name" "(timed out after ${TIMEOUT}s)"
	elif [ "$st" -ne 0 ]; then
		report fail "$name" "(`head -n 1 \"$out\"`)"
	else
		report pass "$name" ""
	fi
}

echo "interpreter: $LISPF4"
echo "image:       $LISPF4_IMG"
[ -n "$have_timeout" ] || echo "note: 'timeout' not found; a hung case will block the run"
echo

for f in "$here"/cases/*.lsp "$here"/cases/*.sh; do
	[ -e "$f" ] || continue
	base=`basename "$f"`
	name=`echo "$base" | sed -e 's/\.[a-z]*$//'`
	selected "$name" "$@" || continue
	case "$base" in
		*.lsp) run_lsp_case "$name" ;;
		*.sh)  run_sh_case  "$name" ;;
	esac
done

echo
echo "passed: $pass   failed: $fail   known failures: $known   unexpected passes: $unexpected_pass"
[ -n "$failed_list" ]     && echo "failed:          $failed_list"
[ -n "$unexpected_list" ] && echo "now passing:     $unexpected_list"

if [ "$fail" -ne 0 ] || [ "$unexpected_pass" -ne 0 ]; then
	exit 1
fi
exit 0
