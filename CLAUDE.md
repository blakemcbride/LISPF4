# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

LISPF4 is an InterLisp interpreter written by Mats Nordstrom (Uppsala, 1980-83) in FORTRAN IV, converted to C by Blake McBride via F2C and then hand-modified. It is a dynamically scoped Lisp with LAMBDA/NLAMBDA/FUNARG, a structure editor, property lists, arrays, and binary image save/load. No GUI. The regression suite lives in `tests/`; run it with `make test`.

**`KnowledgeBase.md` in the repo root is the deep technical reference** — memory layout, atom representation, eval/apply labels, GC, ROLLIN/ROLLOUT format, COMMON block contents, and a function-to-file:line index. Read it before doing anything nontrivial in the C code. Keep it current when you change interpreter internals.

## Build

```bash
make                   # Linux/Mac: lispf4, then bare.img, then basic.img
nmake -f Makefile.win  # Windows/MSVC
make clean             # *.o, *~, core, *.bak
make realclean         # also lispf4 and *.img
make test              # regression suite (tests/)
```

The build is a three-stage bootstrap, and each stage depends on the previous:

1. `lispf41.c` + `lispf42.c` + `auxillary.c` → `lispf4`
2. `./lispf4 -x <script.1` reads `SYSATOMS`, does a ROLLOUT → `bare.img`
3. `./lispf4 bare.img <script.2` loads the `.lisp` packages, does a SYSOUT → `basic.img`

`basic.img` depends on `bare.img`, `script.2` and every `.lisp` file `script.2` loads
(`LISPSRC` in the Makefiles), so editing a `.lisp` file rebuilds the image. Adding a file
to `script.2` means adding it to `LISPSRC` too.

Memory defaults are compile-time (`PARMS` in the Makefiles): `CELLS=100000` (cons cells), `ATOMS=3000`, `STACK=1500`, `ARRAY=5000` (print names/strings/reals/arrays). `LAST_UPDATE_{YEAR,MONTH,DAY}` in the Makefiles feed the `-DYEAR/-DMONTH/-DDAY` startup banner — bump them when releasing.

## Running and testing

```bash
./lispf4 basic.img                    # normal interactive use; (EXIT) to quit
./lispf4 -x                           # bare system from SYSATOMS (system generation only)
./lispf4 -c200000 -a5000 basic.img    # override cells/atoms/stack/pnames at runtime
```

Flags: `-c` cells, `-a` atoms, `-s` stack, `-p` print names, `-x` no image, `-h` usage. The number may be attached (`-c200000`) or separated (`-c 200000`). Options must precede the image file name; a trailing option, an unknown option, and `-x` together with an image file are all errors with a non-zero exit.

`make test` runs the regression suite in `tests/` — see `tests/README.md` for the case
format and for what each case detects. `make testdebug` runs it against the ASan/UBSan
build. Add a case for anything you fix, and check that it *fails* on the pre-fix binary
(`Linux/lispf4` + `Linux/basic.img` is the last shipped one) so it is a real detector.

For a quick one-off, pipe expressions to a fresh interpreter:

```bash
printf '(PLUS 2 3)\n(EXIT)\n' | ./lispf4 basic.img
printf '(GETD (QUOTE EDITS))\n(EXIT)\n' | ./lispf4 basic.img
```

Changing `-c`/`-a`/`-s`/`-p` when loading an existing image works — `move_()` relocates
every stored value on ROLLIN: cons cells, floats, small integers, and the pointer part of
arrays, which lives in `PNAME` rather than in a cell. The float shift needs `NATOM` as it
was at SYSOUT time, which the header does not carry, so `ROLLOUT` appends a two-word
trailer (`ROLLMAGIC`, `NATOM`); images written before that still load, and older
interpreters ignore it. The one thing that cannot survive is a small integer larger than
the new system's `ISMALL` — a bigger `-c` or `-a` means a smaller small-integer range, so
such a value saturates. (`Documentation/README.txt` warns against mixing parameters; that
predates these fixes. `tests/cases/d8-arrayimg.sh`, `e3-floatimg.sh`, `e3-negimg.sh` and
`e3-oldimg.sh` cover the round trips.)

`basic.img` loads an upshift option, so input is case-insensitive there. The raw system (`-x` / `bare.img`) is case-sensitive and all builtins are uppercase.

## Hard constraints

- **Never regenerate the `.c` files from the `.f` files.** `Lispf41.f`, `Lispf42.f`, `F4COM.FOR`, and `lispf4.orig` are reference-only originals. The C has been hand-modified (F2C runtime removed, dynamic allocation added, portability and correctness fixes); re-running F2C destroys all of it. Both Makefiles cancel the inference: the explicit rules are commented out, the suffix rule is gone, and `%.c : %.f` has an empty recipe. That last one matters on macOS and Windows, where the filesystem is case-insensitive and `stat("lispf41.f")` finds `Lispf41.f`.
- `Documentation/` holds the original manuals and PDFs — treat as read-only historical material.
- `Linux/`, `Mac/`, `Windows/` hold **committed** prebuilt `lispf4` + `basic.img` for distribution. They are updated by hand at release time (see commits "Add Mac executables", "Linux and Windows exe update"), not by the build.
- Root-level `lispf4`, `*.o`, `bare.img`, `basic.img` are gitignored build artifacts.

## C source layout

| File | Role |
|---|---|
| `lispf41.c` | `lispf4_()` — the eval/apply loop. Computed GOTOs translated from FORTRAN assigned GOTOs. |
| `lispf42.c` | `main()`, `init1_`/`init2_` (bootstrap), `rollin_`/`rollou_`/`move_` (images), `garb_` (GC), reader, printer, arithmetic, atoms, arrays, strings. |
| `auxillary.c` | Hand-written C replacing the F2C runtime: `getch_`/`putch_` byte packing, `f4_open`/`f4_read`/`f4_write`/`f4_close` logical-unit file I/O, time/date. |
| `f2c.h` | `integer`/`real`/`logical` typedefs. Adjust here for unusual word sizes. |

F2C-derived idioms you must preserve when editing:

- Every FORTRAN-origin function name ends in `_` (`init1_`, `garb_`, `shift_`).
- Arrays are 1-based in the algorithm and 0-based in C — accesses read `[i__ - 1]`.
- COMMON blocks are global structs aliased as `a_1`, `b_1`, `carcdr_1`, `chars_1`, `jaan_1`; `#define` macros reproduce FORTRAN `EQUIVALENCE` (e.g. `#define args ((integer *)&b_1.arg)`).
- `*SETC*` comments mark where `CALL SETCAR/SETCDR` became direct `carcdr_1.car[i-1] = v` assignment.
- The loop shape `for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__)` is deliberate: FORTRAN DO loops run at least once. Don't "fix" it.

## Lisp layer

`script.2` determines what lands in `basic.img`, in this order: `basic1.lisp` (via IOTAB redirect), then READFILE of `basic2 io1 func1 debug1 debug2 edit makef`, then LOAD of `history.lisp`, then `(CURFILE CUR)` and `(SYSOUT "basic.img")`. Order matters — later files redefine earlier ones (`history.lisp` redefines LISPX and READ).

Not in `basic.img`, loadable on demand: `ifdo.lisp` (IF/DO WHILE/FOR, needs `match.lisp`), `match.lisp`, `struct.lisp`, `astruct.lisp`, `prolog.lisp`, `quote.lisp`, `static.lisp`, `printa.lisp`, `schum.lisp`.

`SYSATOMS` defines the builtin function tables read by `init2_()` during bare startup: seven SUBR groups (SUBR0, SUBR1, SUBR11, SUBR2, SUBR3, SUBRN, FSUBR), then individual atoms, then the numbered system messages. Atoms are created in file order, and the eval loop dispatches builtins by comparing an atom's index against the group boundary registers — so **adding or reordering entries shifts every subsequent builtin's dispatch group**. `SYSATOMS` is read only during `-x` startup; existing images carry their own atom table and will not pick up the change. Regenerate from the bottom: `make realclean && make`. `init2_()` now checks every read: a
`SYSATOMS` that runs out mid-table, or whose message section is short of `MAXMES` lines,
prints a diagnostic and exits 1 instead of quietly building an image with a shifted
message table. Keep the file LF-terminated (`.gitattributes` says so); CRLF used to be
exactly that failure.

Super-parenthesis is `]` (closes back to the matching `[`), changed from the original `<>` to match InterLisp.

## InterLisp development model

Development normally happens *inside the image*, not in text files: load an image, edit with `(EDITF FUNC)` (the function must already be defined), `(SYSOUT "file.img")`. Text is the export format, not the source of truth — see `Documentation/DevelopmentProcess.txt`.

The "package" system is grouping only, no namespaces or visibility: `(CURFILE PKG)` sets the current group and all subsequently defined functions join it; `(MAKEFILE 'PKG "file.lisp" T)` writes the group out; `(LOAD "file.lisp")` reads it back. Per-package variables are `pkgFNS`, `pkgVARS`, `pkgCOMS`, `pkgGENNR`; `CURLIBS` lists loaded packages. Every `.lisp` file in this repo follows that format — a `FILEHEADER`, `DEFINEQ` blocks, and an `RPAQQ pkgFNS` list that must stay in sync with the definitions.

I/O uses FORTRAN logical unit numbers throughout: 5 = stdin, 6 = stdout, 4 = SYSATOMS during init, 10-30 for user files. `(XCALL 1 '(unit "file" OLD|NEW FORMATTED|UNFORMATTED))` opens, `(XCALL 2 unit)` closes, `(IOTAB 1 unit)` redirects input.

## Further documentation

`Documentation/README.txt` (build/run/function list), `UsersGuide.txt`, `ImplementationGuide.txt`, `DevelopmentProcess.txt`, plus the original Interlisp and Haraldson PDFs.
