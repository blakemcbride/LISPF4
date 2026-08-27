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
passed: 35   failed: 0   known failures: 0   unexpected passes: 0
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

## Sanitizer status

Last run 2026-08-27, ASan + UBSan, strict options. **No reports** from any of the
following:

- the full 35-case suite;
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

## Bugs2 cases

`c1-array-bounds`, `l2-union`, `l3-define` and `l5-savedef` cover the `Bugs2.md` findings;
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
