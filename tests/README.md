# LISPF4 regression suite

Built as Phase 0 of `Plan1.md`, and extended with every later finding. Every bug that can
be observed from outside the interpreter has a case here. Each one was verified to **fail
before** its fix and pass after, so the suite is a real detector rather than a description
of current behaviour.

## Running

```sh
make test          # builds if needed, then runs everything
./tests/run-tests.sh               # run directly
./tests/run-tests.sh b1 b3         # only cases whose name contains b1 or b3
```

Against the debug/sanitizer build:

```sh
sudo dnf install libasan libubsan     # Fedora: the runtimes ship separately from gcc
make testdebug
```

The `.so` files under `/usr/lib/gcc/.../` are only linker scripts pointing at
`/usr/lib64/libasan.so.8.0.0`; without the `libasan`/`libubsan` packages the debug build
fails to link. Recommended settings when triaging, since UBSan only warns by default:

```sh
export UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1
export ASAN_OPTIONS=detect_leaks=1
```

Environment overrides: `LISPF4` (interpreter, default `../lispf4`), `LISPF4_IMG` (image,
default `../basic.img`), `TIMEOUT` (seconds per case, default 300).

Scratch output lands in `tests/.work/` (gitignored) — `NAME.out` is the raw session,
`NAME.norm` the normalised form, `NAME.diff` the mismatch.

## Expected result today

```
passed: 108   failed: 0   known failures: 0   unexpected passes: 0
```

Everything found so far is fixed, so there are no `.bug` markers left. The driver exits 0
and fails on any new breakage.

## Case types

| File | Meaning |
|---|---|
| `cases/NAME.lsp` | fed to the interpreter on stdin; always checked for a clean exit (no signal, no timeout) |
| `cases/NAME.exp` | optional; the normalised output must match exactly |
| `cases/NAME.sh`  | shell case, run in a scratch dir with `$LISPF4`, `$LISPF4_IMG`, `$ROOT` exported; exit 0 = pass |
| `cases/NAME.bug` | marks a known failure; contains the bug id from `Bugs1.md` |

A `.lsp` case with no `.exp` is a crash-only test: it asserts the interpreter survives.

## The `.bug` workflow

A case with a `.bug` file is reported as `known` and does **not** fail the run. When a phase
of `Plan1.md` lands, delete the corresponding `.bug` files. If a case marked `.bug` starts
passing and the marker is still there, the driver reports `XPASS` and exits non-zero — so
the markers cannot silently rot.

| Phase | Delete these markers |
|---|---|
| 1 (B3) | `b3-specat.bug` |
| 2 (B1, B2) | `b1-arrays.bug`, `b1-arrays-gc.bug`, `b2-floatgc.bug` |
| 3 (B4, B5, B6) | `b4-xcall-lun.bug`, `b5-rewind.bug`, `b5-iotab-out.bug`, `b6-longname.bug` |
| 4 (B8, B9) | `b8-truncimg.bug`, `b8-textimg.bug`, `b9-args.bug` |

All of these have been removed -- every phase has landed. The table is kept as a record of
which case covers which bug.

## Output normalisation

Interpreter output varies between runs and builds, so before comparing, the driver removes:

- the startup banner up to and including the first `--- Reset` (it carries the build date
  and the free-cell/atom counts);
- everything from `Exit from Lisp F4` onwards (GC statistics);
- leading `_` and `:` prompt characters — note these accumulate, so a 10-line definition
  echoes as `__________NAME`;
- trailing blanks and blank lines.

Later `--- Reset` lines, which indicate an error caused a restart, are preserved: they are
significant.

Because of this, avoid writing cases whose output includes an atom index (an array prints as
`#780`, and that number shifts whenever the image changes). Wrap such expressions, e.g.
`(PROGN (SETQ A (ARRAY 15 5 5)) (QUOTE READY))`. Suppress GC chatter with
`(SYSFLAG 1 NIL)` when a case is expected to collect.

## Coverage notes

- `b7-margin` asserts the *guard*, not the overflow. The Phase 5 fix caps the print margin
  at `iobuff-20`, so `(IOTAB 8 160)` is now rejected outright and the out-of-bounds write in
  `priint_` is unreachable by that route. The case therefore checks that 140 is accepted and
  160 refused, which fails on the pre-fix binary and needs no sanitizer. (An earlier version
  simply printed wide output and passed trivially — it proved nothing.)
- `b6-longname` builds a 2560-character filename with `CONCAT` and crashes on both the `-O3`
  and `-O0` builds, so it is a genuine detector without a sanitizer. An earlier version used
  a 90-character atom, which faulted at `-O3` but not at `-O0` — how far the overrun has to
  reach before it hits an unmapped page depends on the static layout, so overshoot generously
  when writing a buffer-overflow case.
- `rollin-reject` checks both image-load failure modes: a cleanly rejected image leaves the
  system usable (`ROLLIN` returns NIL and the session continues), while a truncated one stops
  the interpreter rather than running on a partly overwritten heap.
- The `f`- and `g`-series cover `Bugs3.md` (F1-F12) and `Bugs4.md` (G1-G7).  Every one of
  the seventeen was verified to fail against `Linux/lispf4` + `Linux/basic.img` -- the last
  shipped build -- and to pass after the fix.  Point `LISPF4`/`LISPF4_IMG` at that pair to
  re-check:  `LISPF4=./Linux/lispf4 LISPF4_IMG=./Linux/basic.img ./tests/run-tests.sh f1- g1-`.
  Note that F5-F9 are `.lisp`-layer defects, so they need the *old image* as well as the old
  interpreter; running them against a freshly built `basic.img` would pass trivially.
- `g1-deepgc` asserts that `--- Non-recursive GBC called` appears in the transcript as well
  as that the session survives.  That message is `MARKL` announcing itself, and `MARKL` is
  the routine the case exists to exercise; without the check, raising the default `-s` would
  quietly turn the case into a no-op.
- `g2-arraybreak` needs the *error* to be one that leaves `IBREAK` set -- an array subscript
  does, an ordinary type error like `(CAR 5)` does not -- and needs enough iterations for a
  collection to land inside the four `CONS` calls of the error entry.  40 000 is comfortably
  more than enough at the default `-c`.
- `b9-bigger` and `sanity-image` are guards, not bug reproductions: raising `-c` above the
  image's build-time value must keep working, and the `SYSOUT`/`SYSIN` round-trip must
  survive the Phase 2 layout work.
- B10–B16 have no cases. They are latent or portability issues with no observable behaviour
  on this platform; Phase 6 was validated by inspection, a `-Wall -Wextra` build, and the
  manual Ctrl-C check described in `Plan1.md` §6.3.
- `b17-nonewline` covers the last-line-without-a-newline bug found while verifying Phase 6.
  It exercises `READFILE`, whose loop terminates on the atom `STOP`; if a file's final line
  is dropped the `STOP` is never seen and `READFILE` carries on reading standard input. Note
  that a `READFILE` test file **must** end with `STOP` or the case will appear to fail for
  an unrelated reason.
- The `k`-series covers `Bugs7.md` (K1-K9). All nine were verified to fail against
  `Linux/lispf4` + `Linux/basic.img` -- the last shipped build -- and to pass after the fix:
  `LISPF4=./Linux/lispf4 LISPF4_IMG=./Linux/basic.img ./tests/run-tests.sh k1- k2-` and so
  on. K2, K3 and K7 are `.lisp`-layer defects, so they need the old *image* as well as the
  old interpreter.
- `k4-circint` extends `i4-circint`'s harness to `NCONC`, `NCONC1`, `MAP`, `MAPC` and to a
  **circular property list** for `GETPROP`/`GETD`/`PUTPROP`. A ring is easy to build there
  because `RPLACD` on a literal atom is the plist setter. `MAP`/`MAPC` need the mapped
  function to be a *SUBR* (`'NULL`, not a LAMBDA): they drive their loop through `APPLY`,
  and with a LAMBDA the body goes through `EVAL`, whose poll was always there.
- `k5-deepread` splits its 600-level datum over 60-column lines so that K1 plays no part,
  and asserts three things -- `--- Stack overflow` in the transcript, `(ERRORN)` = 12, and
  no *second* `--- Reset` (the first is the startup banner's). Without the last one the
  case would pass on a build that reset silently.
- `k6-repeatoflo` fires both escalations: five recursions for the parameter stack
  (`HILLW`, fatal on the third before the fix) and six `(COPY circ)` for the A-stack
  (`MIDDL`, five incidents). It needs *no* `--- Reset` and *no* `Fatal` line, which the
  `.exp` comparison enforces by way of the normaliser keeping later `--- Reset` lines.
- `k7-loadfail`'s real detector is the form *after* the failing one: `(NLSETQ P2)` must
  answer `NIL`, not `(2)`. `(2)` means the top level read the rest of the file itself,
  which is what the abandoned `LOAD` used to leave the reader doing.
- `h3-stralloc` changed with K8: `PROMPTTEXT` answers with a literal atom, and the case's
  two comparisons were written against strings. They passed only because `EQUAL` compared
  print names with no type test.
- The `m`-series covers `Bugs8.md` (M1-M10). M1 and M2 are **regressions from the eighth
  pass**, so their detectors fail against the *current* pre-fix `lispf4` + `basic.img`, not
  the shipped `Linux/` pair: `m1-readc`, `m1-rstring`, `m2-makefile-deep` and
  `m2-makefile-circ`. M3, M4, M5, M6 and M8 are pre-existing and fail against
  `Linux/lispf4` + `Linux/basic.img` (M5/M6 are `.lisp`-layer, so they need the old image
  too). `m7-getstatic` cannot be shown against any shipped artifact -- `static.lisp` is an
  optional package that is *not* in `basic.img`, so the case copies the current, fixed
  file; it was verified by hand against a `GET-STATIC` reverted to a `LAMBDA` (answers
  `NIL` through `NLSETQ` instead of the static list). This is the F5-F9 situation one step
  further out.
- `m2-makefile-deep` pins `-s1500` in its own invocation, and `k2-makefile-big` now does
  too (M10): the effective print level is `(JP-IP)/5-1`, so both depend on `-s`, and a
  change to the default stack would otherwise turn them into different tests. At the pinned
  stack the 600-level nest is past the ~288-level A-stack clamp and must be reported
  `ABANDONED` (M2 makes read-back truncation an error), while a 5-level nest round-trips and
  reports `COMPLETE`. The two structures use different package names (ZZ, YY) so their
  MAKEFILE messages can be told apart.
- `m3-rstring-chtab` and `m4-reset-outunit` both turn on the failure and then require a
  *later* form to behave: `(LIST 1 2 3)` must still read (M3 -- `(` was left a letter), and
  `42` must appear on the terminal and not in the capture file (M4 -- output was left
  redirected). Without the later form neither would prove the recovery.

## Sanitizer status

Last run 2026-08-27, ASan + UBSan + `float-cast-overflow`, strict options. (GCC keeps
`float-cast-overflow` out of the default `-fsanitize=undefined` set and it is what catches
E16, so it is now in `DBGFLAGS`.) **No reports** from any of the following:

- the full 98-case suite;
- a sweep of every SUBR against 16 structurally malformed arguments and 8 malformed second
  arguments -- 12 528 forms. The same sweep finds 64 segfaults on the pre-fix binary;
- 6 000 randomly generated nested forms over the builtin table, including dotted tails at
  every level;
- every E-series reproduction from `Bugs2.md`, run individually;
- every D-series reproduction, run individually: the nine dotted FSUBR forms,
  `(PROG 5 ...)`, `(EVALA 'X 5)`, `(OBLIST 5)`, `(EVSTK 'X -5)`, `(STRALLOC -5 "AB")`,
  `(IOTAB 3 160)`, `(APPLY 'LIST <3000 args>)`, a 2999-variable `PROG`, `EVALA` against a
  600-pair a-list under `-s400`, and `PACK`/`UNPACK`/`NTHCHAR` at 50, 160, 300, 400 and
  2500 characters;
- 40 rounds of `UNPACK` on a 250-character string with an atom-compacting `(RECLAIM 3)`
  after each, which is the case the new print-name walk in `UNPACK` has to survive;
- the complete two-stage image bootstrap (`lispf4 -x <script.1`, then
  `lispf4 bare.img <script.2`, loading all eight packages) — which also produced a
  `basic.img` byte-identical to the `-O3` build;
- all four garbage collectors under load: cell, cell-compacting, bignum-compacting, and
  atom-compacting, including a live array relocated by `arrutl_` inside `garb_` STEP 4
  (confirmed under gdb — that is the hardest of the Phase 2 code paths and the suite alone
  does not reach it);
- deep recursion to parameter-stack overflow, the structure editor (`EDITF`), and roughly
  ten error/break paths;
- loading all nine on-demand packages (`match struct astruct quote static printa schum
  prolog prolog2`), a `MAKEFILE`/`LOAD` round trip, and an `EDITF` session.

Worth re-running after any change to the GC, the array code, or the I/O layer.

## C- and L-series cases

These predate the current `Bugs2.md`; the report they came from has since been replaced by
the E-series analysis, so they are named for their case prefixes rather than for a file.

`c1-array-bounds`, `l2-union`, `l3-define` and `l5-savedef` cover those findings;
`l1-ifdo` covers `ifdo.lisp` loading, `ITP` infix evaluation and `prolog.lisp`'s `POP`/`PUSH`.

Note that the `L*` fixes are in `.lisp` sources, not the interpreter, so running the suite
against the old `Linux/lispf4` binary does **not** show them failing — the shell case copies
the current `.lisp` files. To confirm those cases really detect their bug, point them at the
pre-fix source instead, e.g. `git show HEAD:ifdo.lisp`.

`l5-savedef` is a guard, not a detector: removing a duplicate definition is behaviour-neutral
by design. Likewise `l3-define`'s first three cases — only the fourth (`NLAMDA` spelling)
distinguishes fixed from unfixed.

## D-series cases (second bug-fix pass, 2026-08-27)

Seven cases, one per family of defect. Each was checked against the shipped pre-fix
`Linux/lispf4` + `Linux/basic.img` and fails there, so each is a detector and not a
description. `KnowledgeBase.md` -> *Second bug-fix pass (2026-08-27)* explains the fixes.

| Case | What it pins down | How it fails pre-fix |
|---|---|---|
| `d1-packunpack` | `PACK`/`UNPACK`/`NTHCHAR` past the print margin | `UNPACK` of an 80-character string returns 2 characters; `NTHCHAR` past the margin returns NIL |
| `d2-pdlfull` | parameter-stack bound in the spreading loops | `(APPLY 'LIST <3000 args>)` and a 2999-variable `PROG` both segfault |
| `d3-evala` | `EVALA`/`APPLYA` with a nearly full stack | both spin forever under `-s400`; the case times out |
| `d4-wildptr` | missing upper-bound tests on Lisp pointers | the nine dotted FSUBR forms segfault; so do `(PROG 5 ...)`, `(EVALA 'X 5)` and `(OBLIST 5)`. Also covers `EVSTK` with a negative frame, `(STRALLOC -5 ...)` and `(IOTAB 3 160)` |
| `d8-arrayimg` | `move_` relocating array pointer parts on ROLLIN | reloading an image with `-c200000` returns the free list where a cons was stored |
| `d9-arith` | integer and float overflow, float zero, `ADD1`/`SUB1` | `(TIMES 1.0E30 1.0E30)` hangs the printer; integer overflow wraps; float zero prints as `0`; `(ADD1 1.5)` is refused |
| `d20-io` | `OPEN0` and `EJECT` | `OPEN0` always answers NIL; `(EJECT)` writes a blank |

Two defects from that pass have no in-interpreter symptom, both in the makefiles. Check
them by hand: `make -n lispf4` after `touch Lispf41.f` must not run F2C, and `make -n`
after `touch basic2.lisp` must rebuild `basic.img`. The partial bound checks in
`apop2_`/`apop3_`/`fpop_` and the never-assigned local in `priflo_` are latent; they were
cleared by inspection and by the `-Wall -Wextra` build.

`d1-packunpack` is worth understanding before touching `PACK` again: `UNPACK` and
`NTHCHAR` now have no length limit, but `PACK` returns a *string* rather than a literal
atom past 160 characters, because `ratom_` collects a literal atom in `ABUFF` and that is
how big `ABUFF` is. The case pins both sides of that boundary.

## E-series cases (third bug-fix pass, 2026-08-27)

Twenty cases covering `Bugs2.md` E1-E19. Every one was checked against the shipped pre-fix
`Linux/lispf4` + `Linux/basic.img`: seventeen fail there, which makes them detectors rather
than descriptions. `KnowledgeBase.md` -> *Third bug-fix pass (2026-08-27)* explains the
fixes.

| Case | What it pins down | How it fails pre-fix |
|---|---|---|
| `e1-quotes` | `PRINAT`'s unbounded quote loop | 800 nested quotes segfault; so do 400 built by `LIST` rather than typed |
| `e2-putprop` | `PUT` storing through a malformed plist | `(RPLACD 'FOO (CONS 'BAR 5))` then `(PUTPROP 'FOO 'BAR 1)` segfaults -- an out-of-bounds write at a caller-chosen offset with a caller-chosen value |
| `e3-floatimg` | `ROLLIN` relocating floats by the cell offset | reloading under `-a4000` prints `0.` for `3.25`; under `-a2500` the floats come back as integers |
| `e3-negimg` | `ROLLIN` starting the small-integer pass in the wrong place | reloading under `-c200000` prints a list of NILs for `-1073650000` |
| `e3-oldimg` | image-format compatibility, both directions | guard, not a detector: it passes on both, and must keep doing so |
| `e4-getprop` | `get_` on a malformed property list | two shapes, both segfault |
| `e5-fncell` | EAPPLY's two hand-inlined copies of `get_` | `(RPLACD 'ZZ 5)(ZZ)` segfaults; so does a form that rewrites itself while its arguments are being evaluated |
| `e6-assoc` | `ASSOC`/`SASSOC` walking into a non-pair element | `(ASSOC 'A '(1 2 3))` segfaults; the sweep hit both from 36 argument shapes |
| `e7-evstk` | `EVSTK`/`APPLYSTK` handing an unvalidated value to `GETNUM` | `(EVSTK 'X NIL)` reads `PNAME` ~400 KB below the allocation |
| `e8-lambda` | LAMBDA binding over a dotted argument list | `((LAMBDA (X) X) . 5)` segfaults |
| `e9-progvar` | `(PROG ((X . 5)) ...)` | segfaults |
| `e10-selectq` | `SELECTQ`'s interior clause walk | a dotted tail anywhere in the clause list segfaults |
| `e11-compare` | `GREATERP`/`LESSP`/`ALPHORDER` in single precision | `(GREATERP 16777217 16777216)` answers NIL; `MIN` picks the larger |
| `e12-equal` | `EQUAL` on circular structure | the CAR-circular form never returns and ignores SIGINT |
| `e13-funarg` | `FUNARG` block decoding | two hand-built blocks, both segfault |
| `e14-ppprog` | the `PROG`-label outdent writing at `PRBUFF[-3]` | no crash at `-O3`: the label is written into `RDBUFF` and simply vanishes from the output, which is what the `.exp` catches |
| `e15-crlf` | a CRLF or truncated `SYSATOMS` | the CRLF build produces a different `bare.img` and still exits 0 |
| `e16-exponent` | `(integer)` conversion of a 20-digit exponent | `1E99999999999999999999` prints as `0.` instead of saturating |
| `e17-listspace` | `GARB`'s recursive call to `LISPF4` | 1500 list-space exhaustions under a 1 MB stack segfault after about 1100 |
| `e18-args` | command-line handling | an unknown option exits 0; an option after the image name is dropped |

`e14-ppprog` is the one to understand before touching the pretty-printer: it is an
exact-output case because the defect is a silent write outside the buffer, not a fault.
`e17-listspace` needs `ulimit -s 1024` to show anything -- the leak is only visible against
a bound -- and takes about a second.

`e12-equal` covers what E12 fixed, not everything that was wrong: the CDR-circular half of
the case asserts only that SIGINT gets you out and the interpreter recovers. `EQUAL` still
has no cycle *detection*, but the walk is now bounded -- see `j2-eqcycle`.

## H-series cases (fifth bug-fix pass, 2026-08-27)

Six cases covering `Bugs5.md` H1-H5. Four are detectors and two are guards; the
distinction matters, so it is spelled out per case. `KnowledgeBase.md` -> *Fifth bug-fix
pass (2026-08-27)* explains the fixes.

| Case | What it pins down | How it fails pre-fix |
|---|---|---|
| `h1-circprint` | the printer's total-output budget | `(TCONC X X)` never terminates; the case times out. Also prints a CDR-circular and a CAR-circular structure, which were already bounded and must stay so |
| `h1-circlimits` | that `PRINTLEVEL`/`PRINTLENGTH` still mean what they meant | **guard** -- passes both sides. 3/3 on a doubly circular pair must still give exactly 27 leaves, so the new budget cannot be seen at small limits |
| `h2-margin` | `(IOTAB 7 N)` surviving an error | the margin is 1 after `(NLSETQ (CAR 5))` and after `(ERSETQ (CAR 5))` |
| `h3-stralloc` | `STRALLOC`/`PROMPTTEXT` across an atom-compacting collection | **guard** -- passes both sides. The offset really did go stale on every call, but `GARB` STEP 4 compacts downward and leaves the vacated bytes intact, so no wrong answer was ever produced. The case exists so that a later collector change that reuses or clears that region fails here instead of corrupting strings |
| `h4-arity` | the nine operator wrappers in `basic2.lisp` | `(< 1 3 2)` and `(= 1 1 2)` are `T`, `(/ 100 5 2)` is 20, `(- 10 3 2)` is 7 |
| `h5-packages` | the four packages that call another package's functions | `LOAD`ing or `READFILE`ing `struct`, `astruct`, `prolog` or `printa` alone gives "Undefined function" at first use |

`h3-stralloc` asserts that the run actually collected -- the GBC tally's fourth number
must not be 0. Without that check the case would pass trivially at a roomier `-a`/`-p`
and prove nothing; it currently drives about a dozen atom-compacting collections at
`-a2500 -p3000`.

`h5-packages` is like the `L*` cases: the fix is partly in `.lisp` sources, which the
shell case copies from `$ROOT`. Pointing it at the old interpreter alone shows only the
`READFILE` half (a `READFILE` inside a `READFILE` used to take unit 15 out from under the
outer read). To see the missing-dependency half fail, point `ROOT` at a pre-fix checkout
as well:

```sh
git archive HEAD~1 | tar -x -C /tmp/prefix && (cd /tmp/prefix && make)
LISPF4=/tmp/prefix/lispf4 LISPF4_IMG=/tmp/prefix/basic.img ROOT=/tmp/prefix \
	sh tests/cases/h5-packages.sh
```

`h1-circprint` writes to a file rather than a pipe on purpose: against a pipe whose
reader never fills, a runaway print looks like a hang instead of a size, and `wc -c`
reports whatever the pipe happened to carry.

## I-series cases (sixth bug-fix pass, 2026-08-27)

Eight cases covering `Bugs6.md` I1-I8. Every one was checked against the shipped pre-fix
`Linux/lispf4` + `Linux/basic.img` and fails there, so all eight are detectors.
`KnowledgeBase.md` -> *Sixth bug-fix pass (2026-08-27)* explains the fixes. (I9 is two
stale function names in `Documentation/UsersGuide.txt` and has nothing to run.)

| Case | What it pins down | How it fails pre-fix |
|---|---|---|
| `i1-fakearray` | a forged type tag, and the bounds `ARRUTL` hands back | `(SETQ ZQ 'LISPF4-ARRAY)` then `(SYSOUT "x.img")` segfaults and leaves the target file **0 bytes long** |
| `i2-longname` | `MAKEFILE`/`LOAD` and `PRINT` on a datum wider than a line | a 100-character atom comes back as two atoms; a 100-character string comes back 172 characters long |
| `i3-exhaust` | the register file cleared at the reset label | `(REVERSE circ)` at `-c20000` never stops -- 549 248 resets, 40 MB of output, immune to SIGINT |
| `i4-circint` | the interrupt poll in nine list-walking builtins | each of `LENGTH LAST MEMB MEMBER ASSOC SASSOC TAILP ADDLIST SUBPAIR` on a circular list ignores SIGINT and has to be killed |
| `i5-copyoflo` | `SUBPR` giving back the A-stack it consumed | `(COPY <900 elements>)` aborts with a bare `--- Reset`, no message, and `NLSETQ` cannot catch it |
| `i6-corruptimg` | `ROLLIN` validating the tables, not just the header | one flipped byte at offset 7850 segfaults; 5 of 317 single-byte corruptions do |
| `i7-lowercase` | a lowercase exponent in a float literal | `1.5e3` reads as a literal atom, and the diagnostic then names `1.5E3` -- a number the system prints itself |
| `i8-atomfull` | the message number for a nearly full atom table | reports `--- Array index out of bounds`, which is `ARRUTL`'s message |

`i4-circint` sleeps three seconds before each SIGINT rather than waiting for a marker in
the transcript: output to a file is block buffered, so polling for the marker waits for the
buffer rather than for the loop and made the case eight times slower for no extra
certainty. It runs nine interpreters and takes about 35 seconds.

`REVERSE`, `APPEND`, `PACK` and `COPY` are in the same family as the nine builtins
`i4-circint` drives but are not in its list, because each of them ends on its own before a
SIGINT can land: `REVERSE` and `APPEND` exhaust list space (that is `i3-exhaust`), `PACK`
fills the print-name area, and `COPY` overflows the A-stack (that is `i5-copyoflo`). Their
polls were checked by hand.

`i6-corruptimg` sweeps the whole image at a 313-byte stride and asserts only that no
corruption produces a *signal* -- loading and being refused are both acceptable outcomes.
The fix moves the count from 311 loads / 1 refusal / 5 crashes to 236 loads / 81 refusals /
0 crashes: the extra refusals are corruptions that used to load into a quietly broken
state.

`i2-longname` also asserts that plain `PRINT` emits a 100-character atom on one line. The
fix is "do not split", so a name too wide for the line now overruns the right margin
rather than being broken across two. The remaining ceiling is the print buffer
(`IOBUFF-4`, 156 columns), which is above the 150-column read margin, so anything the
reader can take in one line round-trips.

## J-series cases (seventh bug-fix pass, 2026-08-27)

Three cases, plus a second half added to `i8-atomfull`. These are not from a new analysis:
they are the leftovers that six passes had recorded as known and unfixed. All three fail
on the shipped pre-fix `Linux/lispf4` + `Linux/basic.img`. `KnowledgeBase.md` ->
*Seventh bug-fix pass (2026-08-27)* explains the fixes, and lists what was deliberately
left alone and why.

| Case | What it pins down | How it fails pre-fix |
|---|---|---|
| `j1-nestload` | `LOAD` and `MAKEFILE` asking `OPEN0` for a free unit | a `LOAD` inside a `LOAD` loads the inner file and then silently drops the rest of the outer one; same for a `MAKEFILE` inside a `LOAD` |
| `j2-eqcycle` | `EQUAL`'s per-call node budget, and the three builtins that call it in a loop | `(EQUAL circ1 circ2)` never returns; the case times out |
| `j3-negzero` | the sign of a float zero surviving `PRINT`/`READ` | `-0.0` reads as a float and prints as `0.`, so the sign is lost on the way out |

`j2-eqcycle` is deliberately capped at **four** budget-exhausting calls. `L25090` halves
`MIDDL` on every stack-overflow report and `MIDDL` is only restored at the reset label, so
the fifth report in one session resets it -- 150, 75, 37, 18, 9, reset. That ladder is by
design (it is what lets a report get out when the A-stack really is full, see `i5-copyoflo`)
and predates this pass, but it does mean a case that provokes stack overflows has to count
them.

`j3-negzero` also checks that `(ZEROP -0.0)` is `T` and `(MINUSP -0.0)` is `NIL`; the fix
changes the printed form, not the arithmetic. `f3-floatzero.exp` was updated in the same
pass for the same reason -- its first line now reads `(0. -0. 0. 0. NIL)`. That case still
fails hard on the pre-fix binary, where every float zero came back as the integer 0.

`xcall_`'s four spine tests were made to agree (two used `NATOMP` where the boundary is
`NATOM`) with no case: the looser form admitted an unused atom slot, which is in bounds, so
there is no observable behaviour to detect.

## M-series cases (ninth bug-fix pass, 2026-08-27)

Ten findings from `Bugs8.md`; ten cases (`m9` and `m10` are covered inside `m2-makefile-*`
and the `-s` pins rather than as standalone cases). Three are regressions from the eighth
pass -- see the coverage note above for which pre-fix build each fails against.

| Case | What it pins down |
|---|---|
| `m1-readc` | `(LIST (READC) (READC) (READC) (READC))` alone on a line answers four blanks and the next line still evaluates -- `READC` reads the card, not the line (M1) |
| `m1-rstring` | `(SETQ S (RSTRING))` on a short line returns and the session survives, rather than eating standard input (M1) |
| `m2-makefile-deep` | a 600-level nest through `MAKEFILE` at `-s1500` is `ABANDONED`, not written truncated and reported `COMPLETE`; a 5-level nest round-trips (M2) |
| `m2-makefile-circ` | a circular package variable is `ABANDONED` and does not produce a runaway file that resets on `LOAD` (M2) |
| `m3-rstring-chtab` | an error inside `RSTRING` restores `CHTAB`, so `(LIST 1 2 3)` still reads (M3) |
| `m4-reset-outunit` | `(RESET)` restores `LUNUT`, so a post-reset answer lands on the terminal and not in a redirected file (M4) |
| `m5-alias` | `(BREAK F)`/`(UNBREAK F)` do not clobber a global `ALIAS` (M5) |
| `m6-edits-exittype` | `(EDITS ...)` does not clobber a global `EXIT-TYPE` (M6) |
| `m7-getstatic` | `(GET-STATIC F1)` with `F1` unbound answers the static list rather than raising (M7) |
| `m8-substring` | `SUBSTRING`'s start index refuses `0` the way the end index does -- `(SUBSTRING "abcdef" 0 6)` is `NIL`, not `""` (M8) |

## Variance cases

`v-standard` covers the five conformance fixes from `Variance.md` (`LAST`, `TAILP`, `RPT`/
`RPTQ`, `KWOTE`, `NTHCHAR`) together with their callers (`TCONC`, `LCONC`) and a few
manual-derived examples (`SUBPAIR`, `LSUBST`, `NTH` at 0, negative `SUBSTRING`). It fails on
the shipped pre-fix binary.

The editor is *not* covered by an automated case even though `TAILP` drives its `UP`/`NX`
navigation — driving `EDITF` needs an interactive command stream. It was checked by hand
against a captured baseline; redo that if `TAILP` is ever touched again.

## Adding a case

1. Write `cases/NAME.lsp`; keep every line under 150 characters (the reader's right margin
   truncates beyond that and silently splits tokens).
2. Run `./tests/run-tests.sh NAME`, then look at `tests/.work/NAME.out`.
3. Write `cases/NAME.exp` with the output the *fixed* interpreter should produce.
4. If it reproduces a known bug, add `cases/NAME.bug` naming it.
