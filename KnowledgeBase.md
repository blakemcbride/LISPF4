# LISPF4 Knowledge Base

## Overview

LISPF4 is an InterLisp interpreter originally written in FORTRAN IV by Mats Nordstrom at Uppsala University, Sweden, in the early 1980s.  It was subsequently converted to C by Blake McBride using the F2C (FORTRAN-to-C) converter, then hand-modified to remove F2C library dependencies and add dynamic memory allocation.

### Contributors (original FORTRAN)

- **Dr. Mats Nordstrom** - primary author
- **Hans Eriksson, Kristina Johansson** - updates
- **Dr. Tore Risch** - updates (marked "TR" in source)
- **Mats Carlsson** - reader, printer, arrays, floating-point numbers
- **Jaan Koort** - stack-variant of the interpreter

### Conversion to C (Blake McBride)

1. Converted FORTRAN to C via F2C
2. Replaced FORTRAN I/O with custom C routines (auxillary.c)
3. Enabled command-line arguments to control startup options
4. Changed from static COMMON block arrays to dynamic allocation (calloc)
5. Made the code more portable across 32/64-bit platforms

### Bug-fix pass (2026-08-03)

See `Bugs1.md` and `Plan1.md`. The most important structural correction: the original
FORTRAN declared `JPNAME` as `INTEGER*2` inside `ARRUTL` and `GARB` but as `INTEGER` in
`ROLLIN`/`ROLLOUT`, while `JBYTES = 4`. F2C reproduced that inconsistency, which silently
truncated array pointer slots and the bignum GC's forwarding pointers to 16 bits. `jpname`
is now `(integer *)` everywhere. Likewise the `SPECAT` statement function's `INTEGER*2`
argument became a narrowing cast that misclassified ordinary values as strings/arrays; the
comparison is now done at full width. **Do not reintroduce `shortint` in these paths.**

### GC/printer collision (2026-08-04)

`garb_()` printed its message through `b_1.prbuff` and assigned `b_1.prtpos`, the same
buffer and cursor `UNPACK` uses while consing. A collection landing inside `UNPACK` made
it return wrong characters, which silently misclassified atoms for any caller of
`NTHCHAR`. Fixed by saving and restoring the printer state across `garb_()`. See
**Garbage Collection** below for the details and the constraint it imposes.

### Second bug-fix pass (2026-08-27)

See `Bugs1.md`, findings D1-D20. The themes worth carrying forward:

- **`PACK`/`UNPACK` no longer go through the line buffer.** Both used to print their
  argument into `PRBUFF` with `nchars_()` and read the characters back out. `PRBUFF` is a
  *line* buffer: `prin1_` calls `terpri_()` as soon as `PRTPOS` passes `MARG`, which
  flushes and clears it, so everything past the print margin was silently dropped -- an
  80-character string unpacked to two characters. They now collect the text in a print
  name the way `CONCAT` does (`IFLG2 = T` makes `terpri_` append each flushed line to
  `PNAME`) and walk that instead. `UNPACK`, and therefore `NTHCHAR`, has no length limit
  at all now; it re-reads the print-name bounds with `getpn_` on every character, because
  the `cons_`/`matom_` in its own loop can collect and move them. `PACK` still hands the
  text to `iread_` through `PRBUFF` so that digits become a number, but only up to
  `IOBUFF` (160) characters -- `ratom_` collects a literal atom in `ABUFF`, which is that
  size. Beyond it `PACK` returns the string it just built rather than a truncation.
- **The parameter stack is bounded everywhere it grows.** `TOPS_FULL()` in `lispf41.c`
  guards every loop that pushes one `JACK`/`JILL` slot per element of user data: SUBR
  argument spreading, LAMBDA binding, extra arguments, `FUNARG` rebuilding, `PROG`
  variables, and the `EVALA`/`APPLYA` a-lists. The EVAL-entry test at L1600 reserves
  `HILLW = HILL-150` slots *per call*, which says nothing about a single call with 3000
  arguments. Each guard discards the half-built frame (`tops = iprev`) before jumping to
  L25095, so `SYSERROR` has room to run and the user gets a recoverable error 15.
- **`EVALA`/`APPLYA` no longer hang.** Both branched back to the label they were already
  at when the stack ran low -- an original FORTRAN typo (`Lispf41.f:1310`) faithfully
  reproduced by F2C. The loop never reached EVAL, so the SIGINT poll never ran either.
- **Missing upper-bound tests.** Nine FSUBRs (`AND OR QUOTE SETQ SELECTQ GO GO* FUNCTION
  PROG`), the `PROG` variable list, the `EVALA`/`APPLYA` a-lists and `OBLIST` tested only
  `<= NATOM` or nothing at all, so a number -- which encodes as roughly `value + NUMADD`
  -- was dereferenced about a gigabyte past `CAR`. The house idiom is
  `if (x <= a_1.natom || x > a_1.nfreet)`; use it on every Lisp pointer before `CAR`/`CDR`.
- **`move_()` relocates arrays.** An array keeps its pointer part in `PNAME`, holding
  ordinary Lisp values. `garb_` STEP 6 always relocated them; `move_`, which does the
  equivalent job on `ROLLIN`, did not, so reloading an image under a different `-c`/`-a`
  silently corrupted every array pointer slot. `move_` now mirrors STEP 6.
- **Arithmetic no longer overflows silently.** `PLUS` and `TIMES` test before accumulating
  and fall into the existing floating path (L16044/L16084) instead of wrapping. `mkreal_`
  clamps a value that is not finite to +-`FLT_MAX`: `priflo_` cannot print an infinity
  (`fmod` of one is a NaN and every comparison against a NaN is false, so its L51 output
  loop never terminates), so `(TIMES 1.0E30 1.0E30)` used to hang the interpreter. That
  one was not in `Bugs1.md`; it was found while checking the D9 guards.
- **Float zero.** `ZEROP` now looks at the value, not just the small-integer 0 encoding,
  and `priflo_` prints `0.` rather than `0` -- the decimal point is the only thing that
  tells a float from an integer on output, so `PRINT` followed by `READ` used to change
  the type. `ADD1`/`SUB1` accept floats through the same `gtreal_`/`IRFLAG` path
  `DIFFERENCE` uses; the rest of the SUBR11 group still wants an index and still refuses.
- **`OPENF` is implemented.** It was a permanent stub returning 0, so the documented
  `OPEN0` builtin could only ever answer NIL. It now opens the file on the first free
  logical unit at or above 10 and returns that unit.

### Third bug-fix pass (2026-08-27)

See `Bugs2.md`, findings E1-E19. The themes worth carrying forward:

- **The house bounds test belongs one level in, too.** D4-D7 applied
  `if (x <= a_1.natom || x > a_1.nfreet)` to the values that reach a builtin *directly*.
  Seven more crashes (E4-E10, E13) were the same omission on values reached one level
  in: the element of a list, the tail of a dotted pair, the second half of a
  property-list entry, the argument list a LAMBDA is spread over. When you touch this
  code, check every `car[X - 1]` / `cdr[X - 1]` where `X` is *not* the value that was
  just tested. A useful sweep is every list-taking builtin against a list of numbers, a
  list of dotted pairs, and a list with a numeric tail -- the previous pass varied the
  arguments, which is why it missed these.
- **A property list is writable from Lisp.** `(RPLACD 'FOO 5)` is legal, so every walk
  of an atom's plist has to cope with a malformed one. `get_` now answers NIL for a
  malformed plist, the two hand-inlined copies of it in EAPPLY (`L1671`, `L1786`) fall
  back to `get_` rather than trusting the shape, and `PUT` refuses instead of storing
  through a value cell that is not a pair -- that store was an out-of-bounds write with a
  caller-chosen offset *and* a caller-chosen value (E2).
- **Every write into `PRBUFF` must be bounded.** `PRINAT`'s quote loop emitted one `'`
  per nested `(QUOTE x)` wrapper with no ceiling but the nesting depth of the datum,
  which runs straight off the end of the `/B/` block and through the `PNAME`/`PNP`/
  `HTAB`/`STACK` heap pointers (E1). It now wraps with `terpri_()` like the literal loop
  at `L300`. The `PROG`-label outdent in `LINEBREAK` is clamped at column 1 for the same
  reason (E14).
- **`ROLLIN` relocation is three regions, not two.** See the ROLLIN/ROLLOUT section
  below; this is E3, and it silently changed the value of saved data.
- **The ordering predicates compare integers as integers.** `GREATERP`, `LESSP` and
  `ALPHORDER` converted both operands to `real` -- a 24-bit mantissa -- while small
  integers run to 1073690323, so every pair differing below the 2**24 granule compared
  equal and `EQUAL`, which has always been exact, disagreed with them. They now take the
  integer path when `gtreal_` returns 0.0 for both operands, and promote the mixed case
  to `doublereal` (E11).
- **`EQUAL` notices A-stack overflow and polls the break flag.** `apush2_` signals
  overflow by leaving the marker 16 in the F-stack and restoring `JP`; `subpr_` and
  `prin1_` both test for it and `equal_` did not, so CAR-circular arguments span forever.
  There is still no cycle detection: a CDR-circular comparison does not grow the stack at
  all and still loops, but the SIGINT poll now makes it recoverable (E12).
- **`GARB` jumps to the reset point instead of calling `LISPF4` again.** The old
  `CALL LISPF4(2)` never returns, so every list-space exhaustion left one
  `cons_ -> garb_ -> lispf4_` chain on the C stack for the rest of the session -- about
  1100 of them exhaust a 1 MB stack. `lispf4_` now arms a `setjmp` (`f4_reset` in
  `lispf4.h`) at entry and `garb_` `longjmp`s to it, falling back to `lspex_()` if
  `LISPF4` has not been entered yet, which it has not when `GARB` runs from `INIT2` (E17).
- **System generation fails loudly.** A CRLF `SYSATOMS` used to build a corrupt image in
  silence: `read1()` treated the CR as end-of-line and left the LF for the next
  `f4_start_read`, so the file read as alternating real and blank lines. `IREAD` skips
  blank lines so the atom groups still parsed, but `MESS`'s `RDA4` reads exactly `MAXMES`
  lines and took on the blanks, shifting every diagnostic the system prints. `read1()`
  now folds CRLF, and `INIT2` refuses a short file rather than reading the rest of the
  atom table off standard input (E15). A `.gitattributes` keeps the checkout LF.

### Fourth bug-fix pass (2026-08-27)

See `Bugs3.md` (F1-F12) and `Bugs4.md` (G1-G7), which were fixed together. The themes
worth carrying forward:

- **Size and depth are an input dimension of their own.** Every earlier pass varied the
  *shape* of arguments -- dotted, malformed, wrong type -- and none varied their size. A
  list nested deeper than the A-stack is the first input that makes `GARB` take its
  fallback marking path, and `MARKL`, that fallback, had never once run to completion:
  it had no NIL guard, so it walked into cell 1, wrote `CDR(NIL) = -I`, and then indexed
  `CAR`/`CDR` with that negative value. Any collection over a structure of ~1500 cells
  crashed, `SYSOUT` included (G1). `MARKL` now applies the same `s <= T` leaf test the
  inline marker at `GARB`'s label 30 has always had, on both the CAR and the CDR side.
  This one was inherited from the FORTRAN (`Lispf42.f:2153`, `2178`), not introduced by
  the conversion.
- **A collection can run with `IBREAK` already set.** The error entry `L2400` allocates
  four cells *before* it clears `IBREAK`, and `ARRUTL` refused to store anything while a
  break was pending -- so a collection triggered from there got no array bounds back,
  left `GARB`'s `static` `IND1`/`LEN` at their stale values, and never marked the array's
  contents, which were then swept while the array still pointed at them (G2). `ARRUTL`
  now refuses only actions 1 and 2 (the ones that read and write an element on Lisp's
  behalf); actions 3 and 4 are bookkeeping and their only callers are `GARB` and `MOVE`,
  which have already established that the argument is an array. `GARB` also zeroes
  `IND1`/`LEN`/`INDS`/`LENS` before each call, the way `MOVE` already did. Anything else
  that keeps state in a `static` across an `ARRUTL` call needs the same care. Note which
  errors leave `IBREAK` set: an array subscript (21, 28), a keyboard interrupt (26), the
  user break character (27), the space warnings in `MATOM`/`MKREAL` (21, 25, 33, 37),
  `GARB0` (34) and the printer's bad-substring path (29). An ordinary type error does
  not, which is why `(CAR 5)` never reproduced it and `(ELT A 99999)` did.
- **A substring's descriptor is ordinary cons cells, and `(CDR s)` hands them to Lisp.**
  `RPLACA`/`RPLACD` refuse the substring *atom* but not its descriptor, so any program
  could choose the byte offset and length that `GETPN` -- the single decoder every string
  operation goes through -- hands out. That was an out-of-bounds read through nine
  builtins and an out-of-bounds *write* through `RPLSTRING` (F1). `GETPN` now requires
  the offset and length to be genuine small integers (the old test was `> NFREET`, which
  a float passes) and bounds the window by `PNP(MAIN+1) - PNP(MAIN)`, the length of the
  string it is a window onto.
- **32-bit `real` where more than 24 bits of mantissa are needed, again.** E11 fixed the
  ordering predicates; the same mistake was still in three more places. `PRIFLO`
  normalised the mantissa by repeated single-precision multiply or divide by ten -- thirty
  roundings to reach `1.0E-30` -- so almost every float with a decimal exponent printed as
  a decimal naming a *different* float (F2); the value-producing arithmetic converted each
  integer operand with `(real)` before combining, and `PLUS`/`TIMES` accumulated in `real`
  too (F4). Both now work in `doublereal` and narrow once at the end. When touching this
  code, sweep every `(real)` cast and every `10.f`.
- **`GTREAL` returning 0.0 does not mean "integer".** It is the marker for "not a float",
  and a float *zero* returns 0.0 as well, so `(PLUS 0.0 1)` came back as the integer 1.
  The type test is `v > BIGNUM` (small integer) versus `NFREET < v <= BIGNUM` (float);
  that is what the arithmetic uses now. The comparison predicates were left on the old
  test on purpose -- a float zero's integer view is exactly 0, so they still order
  correctly.
- **A `FUNARG` is only unwrapped when it is `CAR` of the form.** `L1776` handles
  `((FUNARG f alist) ...)`; a block reached through an atom's function cell went to
  `L1786`/`L1788` instead, where `CDDR` of the funarg block -- the closure's a-list --
  became the body (G3). `L1788` now hands such a value back to the unwrapping loop, which
  is bounded so a self-referential block cannot spin it. `FUNCTION` with an explicitly
  empty variable list also built the two-element `(FUNARG f)`, which nothing can apply;
  it now builds `(FUNARG f NIL)` (G5).
- **A configuration the validator accepts must be one the interpreter can run in.**
  `-s` was allowed down to 100, and `LISPF4` reserves a fixed 150-slot margin
  (`HILLW = HILL - 150`) tested on every `EVAL` -- so at 150 or less an empty parameter
  stack read as full, the overflow handler escalated to "fatal", reset to `L1`, and `L1`
  put `HILLW` back: an infinite stream of "Parameter stack owerflow" that never read
  standard input and never exited (G4). The minimum is now 500, `HILLW` is clamped
  positive as a backstop, and `usage()` says so. `-c`/`-a`/`-p` did not need this because
  `ROLLIN` rejects a configuration too small for the image; the stack is not in the image.
- **Reserved logical units.** `OPENF` has always skipped 4 (`SYSATOMS`), 5 and 6 (the
  terminal) when picking a free unit; `XCALL`, `ROLLIN` and `ROLLOUT` did not, so
  `(XCALL 2 6)` closed standard output and still exited 0, and `(ROLLOUT 6)` sprayed a
  binary image at the terminal (F10, G6). All three now refuse them, and `ROLLIN`/`ROLLOUT`
  additionally require the unit to be open -- `ROLLOUT` ignores every write error, so a
  closed unit used to run a full compacting collection and then report success.
- **The `.lisp` layer's calling conventions have to agree with each other.** `ADVISE` was
  the only LAMBDA in a family of NLAMBDAs, so `(ADVISE FOO ...)` failed while
  `(UNADVISE FOO)` worked (F8). It is now an NLAMBDA wrapper over a LAMBDA worker
  `ADVISE1`, the pattern `BREAK`/`BREAK0` already used; `TRACE` calls `ADVISE1`. Fixing
  that also fixed `READVISE`, which called `(APPLY 'ADVISED ...)` -- a property name, not
  a function -- and so had never worked. The other `.lisp` defects: `EDITP` called
  `EDITS-INT` with two arguments where it takes three, which ignored its commands and
  installed the command list as the atom's property list (F5); `BOUNDP` was an NLAMBDA
  that looked only at the global value cell, so `(BOUNDP 'X)` was always NIL and a
  `PROG`/`LAMBDA` binding reported unbound (F6); `ERRORN` read `ERRTYPE`, which is a
  parameter of `SYSERROR` and therefore gone by the time `ERRORSET` returns, so the number
  is now recorded in the global `LASTERRORN` with `SETTOPVAL` (F7); and `ADDINNAME` tested
  a free `FN` instead of its own parameter `F` (F9).

### Fifth bug-fix pass (2026-08-27)

See `Bugs5.md` (H1-H5). The themes worth carrying forward:

- **Two limits that each bound one axis do not bound the area.** `PRIN1` bounds depth
  with `PRINTLEVEL` (at `L2000`, printing `...`) and the length of each level with
  `PRINTLENGTH` (at `L5150`, printing `---`). Each is correct on its own, and a
  structure circular through CAR alone or CDR alone is bounded by one of them. A
  structure circular through *both* runs away in both directions at once and emits on
  the order of `PRINTLENGTH ** PRINTLEVEL` nodes -- 1000 ** 1000 at the defaults -- so
  `(TCONC X X)`, which leaves `P = (B . B)` with `B = (P)`, printed 809 MB in three
  seconds (H1). The same shape comes out of `LCONC`, `RPLNODE`, `EDSMASH`, `PUTPROP`,
  `ADDPROP`, `LISTPUT`, `LISTPUT1` and `PUTASSOC` given the same deep list twice, and
  out of any `RPLACA`/`RPLACD` pair that closes a cycle both ways. `PRIN1` now also
  carries a per-call node budget (`GLNODE` against `GLBUDG`), reset at the main entry,
  counted at `L2000` and tested beside the length limit at `L5150`. The budget is
  `max(PRNODES, PRINTLENGTH, PRINTLEVEL)` with `PRNODES = 100000` -- roughly the default
  cons-cell count, so nothing an acyclic *unshared* structure can hold reaches it, and
  neither limit can be pre-empted by it on the shape it already bounds. Running out
  prints the same `---` and unwinds, which costs O(depth) more nodes. What is *not*
  fixed: there is no cycle detection, so heavily shared acyclic structure (`Xn = (Xn-1
  Xn-1)`) is still truncated rather than printed, and `LASTDEPTH` (`L500`) remains
  O(PRINTLEVEL x PRINTLENGTH) per call, which a structure both deep and long can still
  make slow. Real cycle detection needs a fourth word per `PRIN1` frame -- the frame's
  own node, which `L5110` overwrites with the tail -- and therefore a change to `IDIV`
  and to the `APUSH3`/`APOP3` pair.
- **A register the error path mutates has to be one the error path restores.** `L2400`
  set `LMARG = 1` so the diagnostic started in column 1 and never put it back, so *any*
  error -- including one `NLSETQ` caught and the program never saw -- silently discarded
  the left print margin set with `(IOTAB 7 N)` (H2). The C assignment is gone; the Lisp
  `SYSERROR` now brackets its own printing with `(IOTAB 7 1)` / `(IOTAB 7 old)`, which is
  what `PRIN1` has always done for itself with its `APUSH2`/`APOP2` of `LMARG`. `MAKEFILE`
  never showed the bug because `MAKEF-OUT` saves and restores `(IOTAB 7)` explicitly.
  The other nine `IOTAB` entries and all seven `SYSFLAG` registers already survived an
  error unchanged.
- **A print-name offset or an atom index in a C local does not survive an allocation.**
  `MATOM` can run `GARB(3)`, which compacts print names and moves atoms; a value held in
  a COMMON register is relocated, a copy in a C local is not. `STRALLOC` took its
  `GETPN` before the `MATOM` and read through the offset after it, and `PROMPTTEXT` held
  its argument's atom index in `II` across the same call (H3). Both were provably stale
  on every call and both still produced correct answers, because `GARB` STEP 4 compacts
  *downward* and never erases what it vacates -- correct by accident, not by
  construction. `STRALLOC` now re-fetches after the `MATOM`, as `SUBSTRING` twenty lines
  below it already did, and `PROMPTTEXT` holds the argument in `TEMP1`, which is a GC
  root. This is the same class as the `UNPACK` bug of 2026-08-04.
- **A family of operators added as a set should agree about arity.** `basic2.lisp`
  defines nine spellings; `+` and `*` were `PLUS`/`TIMES` (SUBRN, genuinely n-ary) while
  `-`, `/`, `<`, `>`, `=`, `<=` and `>=` silently dropped everything past the second
  argument, so `(< 1 3 2)` and `(= 1 1 2)` were both `T` (H4). The six comparisons now
  chain -- `(< a b c)` is `(AND (< a b) (< b c))` -- and `-` and `/` fold left over any
  number of arguments, `-` keeping its one-argument negate. Ignoring extra arguments to
  a SUBR is system-wide InterLisp behaviour (`(CONS 1 2 3)` is `(1 . 2)`) and is not a
  defect; these nine were a defect because they read as n-ary and only two were.
- **A package that calls another package's functions has to say so.** `struct.lisp` and
  `astruct.lisp` call `MATCH`/`LMATCH` from `match.lisp`; `prolog.lisp` and `printa.lisp`
  are written with `DO` from `ifdo.lisp`, which itself needs `match.lisp`. Loading any of
  them as documented gave "Undefined function" at first use (H5). Each now pulls what it
  needs with `(OR (GETD 'MATCH) (READFILE "match.lisp") (PRINT "..."))`, placed **before**
  its own `FILEHEADER`: `FILEHEADER` calls `CURFILE`, so a load placed after it would file
  the package's own functions under the prerequisite's name. `MAKEFILE` does not write
  these lines, so they have to be put back by hand if a package is ever regenerated.
  Making that work needed one more fix: `READFILE` opened a fixed unit 15, and `F4_OPEN`
  silently *closes and reuses* a unit that is already open -- so a `READFILE` inside a
  `READFILE` did not fail, it pulled the outer file out from under the reader. `READFILE`
  now asks `OPEN0` for a free unit, which is what `OPENF` has always done. `LOAD`
  (`makef.lisp`) still takes unit 20 and is still not re-entrant; nesting goes through
  `READFILE`.

---

## File Structure

### C Source Files (the working code)

| File | Lines | Description |
|------|-------|-------------|
| `lispf41.c` | ~3940 | Main eval/apply loop (`lispf4_()`) - the heart of the interpreter |
| `lispf42.c` | ~5423 | Auxiliary routines: `main()`, `init1_()`, `init2_()`, `rollin_()`, `rollou_()`, `move_()`, `garb_()` (GC), reader, printer, arithmetic, atoms, arrays, strings |
| `auxillary.c` | ~190 | Custom C replacements for F2C library: `getch_()`, `putch_()`, file I/O (`f4_open`, `f4_close`, `f4_read`, `f4_write`, etc.), `mslft_()`, `mtime_()`, `mdate_()` |
| `f2c.h` | ~230 | Type definitions: `integer`=`int4`=`int`, `real`=`float4`=`float`, `logical`=`int4`=`int` |
| `lispf4.h` | ~55 | Single set of prototypes for the auxillary.c routines and the SIGINT flag |

### FORTRAN Source Files (reference only - do not re-convert)

| File | Description |
|------|-------------|
| `Lispf41.f` | Original FORTRAN for the eval/apply loop |
| `Lispf42.f` | Original FORTRAN for auxiliary routines |
| `F4COM.FOR` | COMMON block declarations (the original static array sizes) |
| `lispf4.orig` | The entire, untouched, original FORTRAN system |

### Lisp Source Files

| File | Description |
|------|-------------|
| `basic1.lisp` | Basic package 1 (essential - loaded first via IOTAB) |
| `basic2.lisp` | Basic package 2 |
| `io1.lisp` | I/O functions |
| `func1.lisp` | Function definition forms: DE, DF, DEFINEQ, etc. |
| `debug1.lisp` | Debugging: BREAK, ADVISE, TRACE |
| `debug2.lisp` | Debugging: BREAK1, error handling |
| `edit.lisp` | Structure editor (EDITF) |
| `makef.lisp` | MAKEFILE package (save/load groups of functions as text) |
| `history.lisp` | History/redo functions (redefines LISPX and READ) |
| `ifdo.lisp` | IF/DO WHILE/DO FOR package |
| `match.lisp` | Pattern matching package (required by ifdo.lisp, struct.lisp) |
| `struct.lisp` | Named data structures package |
| `astruct.lisp` | Association-list structure package - `A*` functions paralleling struct's `S*`. Despite the name it uses no arrays; `AMAKE` builds nested `CONS` pairs |
| `quote.lisp` | Macro-quote / skeleton building (`MQUOTE`, `MQ`, `COMBINE-SKELS`, `ISCONST`) |
| `static.lisp` | Static (persistent) variables: `CREATE-STATIC`, `ADD-STATIC`, `GET-STATIC`, `SAVE-STATIC`, `DELETE-STATIC` |
| `printa.lisp` | Array printing |
| `schum.lisp` | "SCHUM programming system" - an explicit-control evaluator for a Scheme-like language: `SEVAL`/`SCHAPPLY` driven by a `**PC**` register, with closures, environments and frames |
| `prolog2.lisp` | **Prolog interpreter.** Self-contained, works. See *PROLOG2 package* below and `Documentation/prolog2.txt`. |
| `prolog.lisp` | **Experimental / incomplete - superseded by `prolog2.lisp`.** `SEEK`'s compound-goal `COND` clause is truncated in the source (in the initial commit and every commit since), and there is no clause database and no way to assert a clause, so no query can succeed. Mechanical defects (undefined `MEMQ`/`FUNCALL`, non-mutating `POP`/`PUSH`/`TRANSFER`) were fixed 2026-08-04; the missing logic was not invented, because it is not recoverable from git history and no upstream copy was found. Its primitives (`MAKHUNK`, `MAKRECORD`, `ALLOCATE`, `IMM`, `SETIMM`) do work, though `SETIMM` at index 0 is a no-op since `NTH[x;0]` returns `cons[NIL;x]`. |

### Build Files

| File | Description |
|------|-------------|
| `Makefile` | Build for Linux/Mac — the default, so plain `make` works |
| `Makefile.win` | Build for Windows/MSVC (`nmake -f Makefile.win`) |
| `SYSATOMS` | System atom definitions read at init (7 groups + 22 atoms + messages) |
| `script.1` | Builds `bare.img` from SYSATOMS (runs `./lispf4 -x <script.1`) |
| `script.2` | Builds `basic.img` from bare.img + all .lisp files |

### Image Files (binary, generated)

| File | Description |
|------|-------------|
| `bare.img` | Minimal image with just SYSATOMS loaded |
| `basic.img` | Full image with all standard Lisp packages loaded |

---

## Build Process

```
make           # builds lispf4, bare.img, basic.img
make lispf4    # just the executable
make bare.img  # bare image (needs lispf4 + SYSATOMS)
make basic.img # full image (needs bare.img + .lisp files)
```

Default compile-time parameters (set in Makefile):
```
CELLS=100000    # CAR/CDR array size (cons cells + atoms)
ATOMS=3000      # number of atoms
STACK=1500      # stack + parameter stack size
ARRAY=5000      # print names / strings / reals / arrays
```

Build chain:
1. Compile `lispf41.c`, `lispf42.c`, `auxillary.c` -> link to `lispf4`
2. `./lispf4 -x <script.1` -> reads SYSATOMS, does ROLLOUT -> `bare.img`
3. `./lispf4 bare.img <script.2` -> loads all .lisp files, does SYSOUT -> `basic.img`

---

## Command-Line Options

```
lispf4 [-c N] [-a N] [-s N] [-p N] [-x] [FILE.IMG]

-c N   CAR/CDR cells (default 100000)
-a N   Atoms (default 3000)
-s N   Stack space (default 1500, minimum 500)
-p N   Print names/strings/reals/arrays (default 5000)
-x     No image file (reads SYSATOMS for system generation)
```

The numeric argument can be attached (`-c200000`) or separated by a space (`-c 200000`).

Options must come **before** the image file name; a trailing one used to be dropped in
silence and is now an error. An unrecognised option exits non-zero (`-h`, `-?` and
`--help` are the successful requests for the usage text), and `-x` together with an image
file is refused rather than ignored.

`main()` rejects a degenerate configuration before allocating: `-a` below 100, `-s` below
500, `-p` at or below `-a`+100, `-c` at or below `-a`+1000, or a `-c`+`-a` beyond 1e9.
The `-s` floor is 500 because `LISPF4` reserves a fixed 150-slot margin below the top of
the parameter stack and tests it on every `EVAL`: below about 300 that produces spurious
overflows, and at 150 or less it is an infinite error loop (G4).  `-c`/`-a`/`-p` are
additionally checked against the image by `ROLLIN`, which reports
"does not fit the current memory configuration" and exits 1; the stack is not part of an
image, so nothing but this floor guards it.

Reloading an image under different `-c`/`-a`/`-s`/`-p` works: `move_()` relocates every
stored value, floats and array pointer slots included (see ROLLIN/ROLLOUT below). The one
thing that cannot survive is a small integer larger than the new system's `ISMALL`, which
saturates -- a bigger `-c` or `-a` means a smaller small-integer range.
`Documentation/README.txt` warns against mixing parameters; that warning predates the
relocation fixes.

The `LAST_UPDATE_YEAR`, `LAST_UPDATE_MONTH`, and `LAST_UPDATE_DAY` variables in the Makefiles control the date shown in the startup message "Lisp F4, latest update = ..." They are passed to the compiler as `-DYEAR=`, `-DMONTH=`, `-DDAY=`.

---

## Architecture

### Memory Layout (Address Space)

```
1                                                              MAXINT
|---------|------------|------------|----------------|-------------|
NIL    atoms/strings   cons cells   real numbers    small integers
1      ...NATOM        NATOM+1...   (via PNAME)     (encoded via
                       NFREET                        NUMADD offset)
```

- **NIL** = 1 (the value of nil)
- **T** = a regular atom (pointer value stored in `b_1.t`)
- **Atoms**: indices 1 through NATOM (actual count tracked by NATOMP)
- **Cons cells**: indices NATOM+1 through NFREET
- **Big numbers**: start at `BIGNUM = NFREET + NATOM`
- **Small integers**: encoded as `value + NUMADD` where `NUMADD = MAXINT - ISMALL`

### Data Structures

**COMMON blocks** (FORTRAN) / **global structs** (C):

| Struct | FORTRAN | Contents |
|--------|---------|----------|
| `a_` | `/A/` | System parameters: NFREET, NATOM, NSTACK, NUMADD, BIGNUM, ISMALL, etc. |
| `b_` | `/B/` | Interpreter state: ARG, ALIST, FORM, NIL, special atoms (LAMBDA, QUOTE, EVAL, etc.), I/O buffers, PNAME, PNP, HTAB, STACK |
| `carcdr_` | `/CARCDR/` | CAR[], CDR[] arrays (dynamically allocated), CHTAB[256] (character type table) |
| `chars_` | `/CHARS/` | Character constants: SPACE, LPAR, RPAR, DOT, digits, etc. |
| `jaan_` | `/JAAN/` | Parameter stack: JILL[], JACK[], ENV, TOPS, HILL |
| `prompt_` | | Prompt text |

**Dynamically allocated arrays** (in `main()`):

```c
carcdr_1.car  = calloc(NFREET, sizeof(integer));    // CAR cells
carcdr_1.cdr  = calloc(NFREET, sizeof(integer));    // CDR cells
b_1.pnp       = calloc(NATOM+1, sizeof(integer));   // print name pointers
b_1.htab      = calloc(NHTAB, sizeof(integer));      // hash table (1.5*NATOM)
b_1.stack     = calloc(NSTACK, sizeof(integer));     // eval stack
jaan_1.jill   = calloc(HILL, sizeof(integer));       // parameter stack
jaan_1.jack   = calloc(HILL, sizeof(integer));       // parameter stack
b_1.pname     = calloc(NPNAME+2, sizeof(real));      // print names/strings/reals
```

**Key differences from FORTRAN**:
- FORTRAN used 1-based indexing; C uses 0-based (all accesses use `[i__ - 1]`)
- FORTRAN had fixed-size arrays in COMMON blocks; C uses calloc with runtime sizes
- FORTRAN's `CALL SETCAR(I, val)` / `CALL SETCDR(I, val)` became `carcdr_1.car[i-1] = val` / `carcdr_1.cdr[i-1] = val` (marked with `*SETC*` comments)

### Atom Representation

```
         CAR                CDR
   |----------------|----------------|
   | global value   | property list  |-->...
   |----------------|----------------|
```

- Global value stored in `CAR(atom)`; if unbound, points to NOBIND atom
- Property list stored in `CDR(atom)`
- Print names stored separately in PNAME array, indexed via PNP array
- Function definitions stored as LAMBDA/NLAMBDA expressions under the FNCELL property
- Strings: `CAR(string)` points to STRING atom; substrings: `CAR(substr)` = SUBSTR, `CDR(substr)` = `(sourcestring start . length)`

### Character Handling

- Print names are densely packed byte arrays; JBYTES=4 is the *word* size used for
  array and bignum indexing, not a per-character stride
- A single character is held in the low 8 bits of an `integer`, blank padded above
  (`' '<<8 | ' '<<16 | ' '<<24`). `getcht_`/`setcht_` recover it with `ic % 256`.
- `getch_(vec, ch, i)` - read byte `i` of `vec` and build the padded character in `*ch`
- `putch_(vec, ch, i)` - store the low byte of `*ch` into byte `i` of `vec`
- `f4_read_char` / `f4_write_char` do the same for A1 file I/O
- All four build the value arithmetically, so byte order does not matter.  `f4_read` /
  `f4_write` move four bytes verbatim and are for A4 packed text and raw image words only
- Character type table `CHTAB[256]` maps ASCII values to token types
- Types set by `setcht_()`, queried by `getcht_()`

### Eval/Apply Loop (`lispf4_()` in lispf41.c)

The interpreter uses a stack-based evaluation loop with computed GOTOs (translated from FORTRAN's assigned GOTOs). Key labels:

- **L1010-L1020**: Read-eval-print loop (top level)
- **L1671**: Function lookup - checks FNCELL property, then SUBR boundaries
- **L1500-L1900**: EVAL dispatch
- **L2000-L2600**: APPLY dispatch
- **L3000+**: Built-in function implementations (SUBR0 through FSUBR)

Return point codes (stored on stack, used for dispatch after evaluation):
- Small integers encoding which point in the eval/apply loop to return to

### SYSATOMS File Format

Read by `init2_()` during bare system startup. Contains 7 groups of built-in functions plus individual atoms and messages:

1. **SUBR0** (no args): CLOCK, DATE, EXIT, GENSYM, READ, RESET, TERPRI, TIME, etc.
2. **SUBR1** (1 arg): ADD1, ERRORMESS, RECLAIM, ROLLIN, ROLLOUT, SUB1, REWIND
3. **SUBR11** (1 arg, alternate): ATOM, CAR, CDR, EVAL, LENGTH, NULL, REVERSE, ZEROP, etc.
4. **SUBR2** (2 args): CONS, EQ, RPLACA, RPLACD, GETPROP, ASSOC, APPEND, APPLY, etc.
5. **SUBR3** (3 args): ARRAY, MAP, MAPC, MAPCAR, PUTPROP, SUBSTRING, etc.
6. **SUBRN** (n args): CONCAT, LIST, PLUS, TIMES, SYSFLAG
7. **FSUBR** (special forms): AND, COND, GO, OR, PROG, PROGN, QUOTE, SELECTQ, SETQ, etc.

Then 22 individual atoms (A000, APPLY, EVAL, FNCELL, LAMBDA, NLAMBDA, NOBIND, T, etc.) and 40 error/status messages. Key messages by number:

| # | Message |
|---|---------|
| 1 | --- Unbound variable |
| 3 | --- Compacting GBC. Free cells = |
| 4 | Exit from Lisp F4 |
| 17 | --- Reset |
| 20 | Lisp F4 , latest update = |
| 26 | --- Keyboard interrupt |
| 27 | --- User break |
| 30 | Bye |
| 32 | --- EOF read from standard input |
| 35 | --- GBC. Free cells = |
| 39 | GBC:s (cell compacting num num/atom) = |

### Image Files (ROLLIN/ROLLOUT)

- **ROLLOUT** (`rollou_()`) serializes interpreter state to a binary file
- **ROLLIN** (`rollin_()`) deserializes state from a binary file
- Image contains: configuration info (15 words -- the first 15 words of `/A/`), messages,
  interpreter registers (area), print names, PNP, CAR/CDR arrays, character constants,
  character type table, and a two-word trailer
- The trailer is `ROLLMAGIC` (`0x4C463441`, "LF4A") followed by `NATOM`. It is optional
  on input: an image written before it existed simply ends after the character table, and
  `ROLLIN`'s probe for it is deliberately kept out of the `ierr` chain. Older
  interpreters ignore the two extra words, so images stay readable in both directions --
  `tests/cases/e3-oldimg.sh` guards that against the shipped `Linux/basic.img`.

**Relocation.** There are three regions above the atom indices, each with its own shift:

| region | old range | shift |
|---|---|---|
| cons cells | `(NFREPO, NFRETO]` | `IDIFF1 = NFREET - NFRETO` |
| floats ("bignums") | `(NFRETO, BIGOLD]` | `IDIFF3 = BIGNUM - BIGOLD` |
| small integers | `(BIGOLD, MAXINT]` | decode with `NUMADO`, re-encode with `NUMADD` |

`move_()` makes **one** pass and classifies each value before touching it, so nothing can
be relocated twice whatever the shifts are. It covers `CAR`/`CDR`, the `ARGS` block, and
the pointer part of every array, which lives in `PNAME` rather than in a cell.

`BIGNUM = NFREET + NATOM`, so the float shift is `IDIFF1 + (NATOM_new - NATOM_old)` --
which is why the trailer exists. The header carries `NUMADO` but not `NATOM`, and

```
NUMADD = MAXINT - (MAXINT - BIGNUM - 1)/2
```

inverts only to `2*NUMADO - MAXINT - 1 = BIGNUM_old + (BIGNUM_old mod 2)`: exact as the
*boundary* between floats and small integers (the extra index, when there is one, encodes
nothing), but one too high as a *shift* when `BIGNUM_old` was odd. Without a trailer,
`ROLLIN` assumes `NATOM` is unchanged, which settles the parity bit for every reload that
does not pass a different `-a`.

Before this was fixed (E3) the code made two passes, both with ranges derived from the
*new* `BIGNUM`, and shifted the floats by the cell offset. Reloading under a different
`-a` therefore aimed every float at the wrong `PNAME` slot -- it read back as whatever
four bytes of packed print-name text lived there -- and a different `-c` left the most
negative small integers below the start of the small-integer pass, where they came back
as cons cells. Nothing crashed; the data was just wrong.

A small integer that no longer fits the smaller `ISMALL` of a larger system saturates.
That value genuinely has no encoding in the new configuration; saturating is the honest
answer and it is what `tests/cases/e3-negimg.sh` expects.

### Garbage Collection (`garb_()`)

Mark-and-sweep with compaction:
- Marks all reachable cells starting from atoms, stack, and registers
- Sweeps unmarked cells back to free list
- Compacting GC moves cells to eliminate fragmentation
- Separate GC for big numbers and atoms (shared space)
- GC statistics tracked in `a_1.garbs` (cell), `a_1.cgarbs` (compacting), `a_1.ngarbs` (bignum), `a_1.agarbs` (atom)

**Roots for the mark phase (STEP 1), in order:** `jaan_1.jack[]`/`jaan_1.jill[]`
up to `tops`; the registers `args[1..nargs]` (`ARG`..`I2CONS` — `#define args
((integer *)&b_1.arg)`, and note `#define ires` aliases the same slot); the
A-stack `b_1.stack[jp..nstack]`; every atom's car and cdr up to `natomp`; then
arrays. A value held only in a C local across an allocation is *not* a root —
`cons_()` protects just its own two arguments, by copying them into
`i1cons`/`i2cons` in `garb0_()`.

**`garb_()` must not disturb the printer.** It can be entered from any
allocation, including one made from inside `UNPACK` (lispf41.c, `L12750`),
which walks the shared print buffer `b_1.prbuff` backwards using `b_1.prtpos`
as its cursor and conses a character atom at each step. The collector prints
`--- GBC. Free cells =` through that same buffer and assigns `b_1.prtpos = 12`.
Before the fix this destroyed `UNPACK`'s cursor and buffer whenever a
collection landed inside it: `UNPACK` returned the wrong characters, and the
atom's own print name was flushed into the middle of the collector's message.
`garb_()` now saves `prtpos` and `prbuff` on entry, restores them at its single
return, and blanks the buffer before printing rather than flushing it. Any new
message added to `garb_()` inherits this protection; anything else that keeps
state in `prbuff`/`prtpos` across an allocation needs the same care.
Regression tests: `tests/cases/unpack-gc.sh` (direct) and
`tests/cases/prolog2-gc.sh` (via `PVARP`, which classifies terms with
`NTHCHAR`).

**Marking is depth-bounded, and the fallback matters.** STEP 1 marks
recursively using the A-stack between `IP` and `JP`; when that runs out
(`b_1.ip >= b_1.jp - 1`) it hands the rest to `markl_()`, the non-recursive
Schorr-Waite router, and prints `--- Non-recursive GBC called` the first time
per collection. With the default `-s 1500` a structure of roughly 1500 cells
crosses that line. `markl_()` must apply the same leaf test the inline marker
opens with — `if (s <= b_1.t) goto L50` — on both the CAR and the CDR it is
about to descend into. It did not, so it walked into `NIL` (whose CAR and CDR
are both `NIL`), wrote `CDR(NIL) = -I`, and on the next turn indexed
`carcdr_1.cdr[]` with that negative value: a SIGSEGV on every collection over a
deep structure, `SYSOUT` included. `tests/cases/g1-deepgc.sh` checks both that
the session survives and that `markl_()` was actually reached.

**`garb_()` must not be run out of `ARRUTL`.** An array's pointer part lives in
`PNAME`, so STEP 1, STEP 4 and STEP 6 all have to ask `arrutl_()` where it is.
`arrutl_()` refuses to store anything while `IBREAK` is set — and `IBREAK` *is*
set across the four `cons_()` calls in the error entry `L2400`, which clears it
only afterwards. `garb_()` keeps `ind1`/`len`/`inds`/`lens` in `static` locals,
so a refused query left stale values behind and the array's contents went
unmarked (and, in STEP 6, relocated pointers were written through a stale
index). `arrutl_()` now refuses only actions 1 and 2, which act on Lisp's
behalf; actions 3 and 4 are bookkeeping and always answer. `garb_()` zeroes the
indices before each call as a backstop, the way `move_()` already did. See
`tests/cases/g2-arraybreak.lsp`.

---

## InterLisp Features

- **Dynamic scoping** (not lexical)
- **LAMBDA** - evaluates arguments before binding
- **NLAMBDA** - does not evaluate arguments (like FEXPR)
- **FUNARG** - closure mechanism for limited lexical scoping
- **Super-parentheses**: `]` closes all open parens back to matching `[`
- **Case sensitivity**: raw system is case-sensitive (uppercase); basic.img loads upshift option
- **Structure editor**: `(EDITF funcname)` - edit function definitions interactively
- **Package system**: group functions via CURFILE, save/load with MAKEFILE/LOAD
- **Image save/load**: `(SYSOUT "file.img")` / `(SYSIN "file.img")`

---

## PROLOG2 package (`prolog2.lisp`)

A self-contained Prolog interpreter, added 2026-08-04. New code, not a repair of
`prolog.lisp` — the two share no code and no names and can both be loaded at once.
Needs nothing but the base system: `(READFILE "prolog2.lisp")`. User-facing
documentation is `Documentation/prolog2.txt`; this section covers the internals.

### Representation

| Thing | Encoding |
|---|---|
| Term | An ordinary S-expression, so a Prolog list *is* a Lisp list and `(?H . ?T)` is the head/tail pattern |
| Source variable | An atom whose print name starts with `?` — `?X`, `?ANSWER` |
| Renamed variable | A **fresh cons** `(?V . ?X)`, unique by `EQ`, `CDR` = the original name |
| Clause | `(head . body)`; a fact has an empty body |
| Database | The `PCLAUSES` property of the predicate atom, so predicate names never collide with Lisp functions — a predicate may be called `=` |
| Bindings | An association list of `(variable . term)`. `NIL` is the *empty* binding, so failure must be distinct: the atom **`PFAIL`** |

`ASSOC` compares with `EQ` (verified: `(ASSOC '(A) (LIST (CONS '(A) 1)))` → `NIL`),
which is what makes the fresh-cons variable work.

**Renaming allocates conses rather than interning `PACK`ed atoms.** The first version
built `?X-17` with `PACK`; that consumes one atom per variable per resolution step and
exhausts atom space. `PRENAME` binds `*PMAP*` and maps each distinct source variable of
one clause to one fresh cons, so all its occurrences stay `EQ`.

### Solver

`PSOLVE`/`PSOLVE1` are depth-first and return a **list of binding lists**, one per
solution — not a continuation or coroutine, since LISPF4 has no lexical closures to
build one from. `PSOLVE1` resolves a goal against each clause of its predicate,
replacing the goal by that clause's body ahead of the remaining goals.

**Cut** is real, not clause-local. `PRENAME` rewrites `!` in the body into `(CUT tag)`,
where `tag` identifies the `PSOLVE1` invocation. Executing `(CUT tag)` solves the *rest*
first and sets `*PCUT*` on the way back out — so it prunes on backtracking, not on the
way in. Each `PSOLVE1` stops its clause loop whenever `*PCUT*` is set, and clears it
only if the tag is its own. That is the standard cut barrier: it discards both the
remaining clauses of its own predicate and the alternatives of goals to its left.

Built-ins are dispatched in `PBUILTIN` before any clause lookup; the reserved names are
in `*PBUILTINS*` = `(CUT TRUE FAIL = NOT IS LISP)`, and `PASSERT` refuses a clause whose
head names one. `IS`/`LISP` instantiate first and fail rather than `EVAL` an unbound
variable.

### Globals

| Variable | Purpose |
|---|---|
| `*PDEPTH*` | Resolution steps allowed on one branch (default 40) |
| `*PMAX*` | Solutions collected before stopping (default 100) |
| `*PDEEP*` | Set when a depth limit stopped a search — **must not be `PROG`-bound in `PQUERY`** or the flag is lost on return |
| `*PCUT*` | Pending cut tag |
| `*PGEN*` | Cut-tag counter, `PROG`-bound per query |
| `*PCOUNT*` | Solutions so far, `PROG`-bound per query |
| `*PMAP*` | Per-clause rename map, `PROG`-bound in `PRENAME` |
| `*PPREDS*` | Predicates that currently have clauses |

**`*PDEPTH*` defaults to 40 because each resolution step is a level of Lisp recursion**
and the default parameter stack overflows near 60 (measured). `lispf4 -s 40000` raises
the ceiling in proportion — depth 500 runs fine there. Raising `*PDEPTH*` without also
raising the stack overflows it.

### History: this package found a GC bug

`PVARP` classifies a term by its first character, via `NTHCHAR` → `UNPACK`. No other
package calls `UNPACK` in a tight loop over every term node, which is why prolog2
exposed the collector/print-buffer collision described under **Garbage Collection** —
a variable was occasionally judged not to be one, so no binding was made and it came
back unbound, about once in 150–300 queries. Fixed in `garb_`.

### Tests

`tests/cases/prolog2.sh` covers unification, recursion, `APP` in both directions, cut,
negation, arithmetic and the runaway guard. It is mutation-tested: disabling the cut
rewrite, removing `NOT` from `*PBUILTINS*`, or removing the depth limit each makes it
fail. `tests/cases/prolog2-gc.sh` runs one query 301 times under collection pressure.

---

## Key Function Reference

| Function | File:Line | Description |
|----------|-----------|-------------|
| `main()` | lispf42.c:188 | Entry point, command-line parsing, memory allocation, startup. Rejects a degenerate configuration -- note the `-s` floor of 500 (G4) |
| `init1_()` | lispf42.c:1269 | Machine-dependent initialization (JBYTES, MAXBIG, NUMADD, FUZZ, etc.) |
| `init2_()` | lispf42.c:1362 | Reads SYSATOMS, initializes atoms/hash table/free lists |
| `lispf4_()` | lispf41.c | Main eval/apply loop |
| `rollin_()` | lispf42.c:1606 | Load binary image file |
| `rollou_()` | lispf42.c:1803 | Save binary image file. Ignores write errors, so its callers must check the unit is open first |
| `move_()` | lispf42.c:1910 | Relocate stored values on ROLLIN: CAR/CDR, the ARGS block, **and the pointer part of every array** (mirrors `garb_` STEP 6). One pass; `reloc_` (lispf42.c:1880) classifies each value into cells / floats / small integers first, so nothing is relocated twice |
| `equal_()` | lispf42.c:715 | Structural equality. No cycle detection: it tests `apush2_`'s overflow marker and polls the break flag, so a circular comparison is bounded or killable rather than an unrecoverable hang. A float and an integer of the same value are **not** equal -- except at zero, where `gtreal_` answers 0.0 for both |
| `get_()` | lispf42.c:808 | Property lookup. Answers NIL for a malformed property list -- `RPLACD` on an atom can produce one, and EAPPLY's two hand-inlined copies fall back here |
| `getpn_()` | lispf42.c:852 | **The single decoder for every string operation**: returns the byte offset and length of a litatom/string/substring. A substring's descriptor is three ordinary cons cells that `(CDR s)` hands to Lisp, so this is where the offset and length are validated -- genuine small integers, and inside the string they are a window onto (F1). Answers -1 for anything else |
| `arrutl_()` | lispf42.c:1010 | Array bookkeeping. Actions 1 and 2 (get/set an element) refuse to act while `IBREAK` is set; actions 3 and 4 (bounds, make) must not, because `garb_` and `move_` call them and a refusal leaves their indices stale (G2) |
| `prinat_()` | lispf42.c:2509 | Print one atom, plus the leading `'` for each nested `(QUOTE x)`. Every write into `PRBUFF` here is bounded by `MARG` |
| `priflo_()` | lispf42.c:2725 | Print a float. Normalises and extracts digits in `doublereal`; `FUZZ` turns the truncating digit loop into round-to-nearest and must stay half a unit in the last *printed* place, which is `NDIG` significant digits in E format but fewer in F format when leading zeros eat the budget. L51 assumes a finite value; `mkreal_` guarantees one |
| `ratom_()` | lispf42.c:3233 | Token reader. A literal worth zero is an integer only when it held no `.` and no `E` (F3) |
| `garb_()` | lispf42.c:3724 | Garbage collector (mark-and-sweep with compaction) |
| `markl_()` | lispf42.c:4487 | Non-recursive (Schorr-Waite) marker, used when STEP 1 exhausts the A-stack. Needs the same `s <= T` leaf test the inline marker has, on both CAR and CDR (G1) |
| `shift_()` | lispf42.c:3588 | Character input reader / tokenizer (contains EOF handling at L1300) |
| `lspex_()` | lispf42.c:5094 | Clean exit routine (prints GC stats, calls `exit(0)`) |
| `mess_()` | lispf42.c:5140 | Print system message by number (messages defined in SYSATOMS). Clamps the number, so a Lisp-supplied one cannot index outside `IMESS` |
| `rda1_()` | lispf42.c:5453 | Low-level line reader; sets `ieof=2` on end-of-file |
| `matom_()` | lispf42.c:4695 | Atom creation. `k > 0` interns a literal atom from the `k` bytes in `ABUFF`; `k <= 0` makes an unhashed string of length `-k`. **`ABUFF` is 160 bytes, so that is the hard ceiling on a literal atom.** |
| `openf_()` | lispf42.c:5684 | `OPEN0`: open a file on the first free logical unit >= 10, skipping the reserved 4/5/6 |
| `getcht_()` | lispf42.c:5001 | Query character type table |
| `setcht_()` | lispf42.c:5022 | Set character type table entry |
| `getch_()` | auxillary.c | Read byte -> blank-padded character in an integer |
| `putch_()` | auxillary.c | Store a character's low byte into a byte array |
| `f4_fp()` | auxillary.c | Validated lookup of a logical unit's `FILE*` (NULL if bad/closed) |
| `f4_open()` | auxillary.c:72 | Open file on logical unit |
| `f4_read()` | auxillary.c:184 | Read formatted (text) data |
| `f4_readu()` | auxillary.c:213 | Read unformatted (binary) data |

---

## Lisp Package Evolution (lispf4.orig vs current .lisp files)

The current .lisp files have been significantly enhanced from the originals in lispf4.orig. All current files are internally consistent (every function in RPAQQ pkgFNS has a matching DEFINEQ definition). Key changes:

| Package | Renames | Added Functions | Removed Functions |
|---------|---------|-----------------|-------------------|
| basic2 | `SORT` -> `DSORT` | 64 functions (arithmetic ops, property lists, etc.) | `ARRAY` (was built-in SUBR listed in error) |
| io1 | `*` -> `-*-` | — | — |
| func1 | — | `DEFINE`, `DEFUN`, `DM` | — |
| debug1 | — | `ERSETQ`, `HELP`, `NLSETQ` | — |
| debug2 | `ADVICE-BODY` -> `ADVISE-BODY` | — | — |
| edit | `EDITS` -> `EDITS-INT` (refactored) | `EDITF` rewritten with save/stop/ok, `EDITP`, `EDITS` wrapper added back | Original `EDITF`/`EDITP` replaced |
| makef | `FILELST` -> `CURLIBS` | `FILECREATED`, `SYSIN`, `SYSOUT` | `OPENF`, `SOPENF`, `LOPENF`, `CLOSE-1`, `ROLLIN`, `ROLLOUT` (replaced by XCALL-based approach) |
| history | — | Entirely new package (LISPX, READ, REDO, ??, HIST, HFIX) | — |

The file `editv.lisp` is an experimental file and should be ignored.

### Git History for edit.lisp

```
f6e7533  Renamed .l file to .lisp
0281992  Enhanced EDITOR to allow saving and aborting edits
1b290ce  Changes super-parenthesis from <> to the InterLisp standard []
d1405fa  Initial commit
```

---

## I/O Architecture

### Logical Units

The interpreter uses FORTRAN-style logical unit numbers for I/O:

| Unit | Purpose | C mapping |
|------|---------|-----------|
| 5 (`LUNIN`/`LUNINS`) | Standard input | `stdin` |
| 6 (`LUNUT`/`LUNUTS`) | Standard output | `stdout` |
| 4 (`LUNSYS`) | SYSATOMS file (only during init) | opened/closed by `init2_()` |
| 10-30 | User files | opened via `(XCALL 1 ...)` / `(OPEN ...)` |

`Logical_units[100]` array in auxillary.c maps unit numbers to `FILE*` pointers. Units 5 and 6 are
set up by `setup()`. All access goes through `f4_fp()`, which range-checks the unit number and
returns NULL for a closed unit, so every `f4_*` entry point reports failure instead of faulting.
`b_1.maxlun` (99) is enforced by ROLLIN, ROLLOUT, REWIND, IOTAB **and** XCALL.

### Read Path

1. `shift_()` is the character-level reader/tokenizer
2. It calls `rda1_()` at label L1200 to read a new line from `LUNIN`
3. `rda1_()` calls `f4_read()` in auxillary.c for each character
4. `f4_read()` calls `read1()` which calls `getc()` on the FILE* for the logical unit
5. `read1()` tracks `read_status[lun]` (per unit): 1=reading, 2=at EOL, 3=at EOF
6. On EOF, `rda1_()` sets `ieof=2`, which `shift_()` detects at L1300

### IOTAB

`(IOTAB entry value)` reads and sets the I/O table, which is the `/B/` block from `LUNIN`
onwards (`#define iotab ((integer *)&b_1.lunin)`). It returns the old value.

| Entry | Field | Meaning |
|---|---|---|
| 1 | `LUNIN` | input logical unit (`T` = back to `LUNINS`) |
| 2 | `RDPOS` | read cursor |
| 3 | `LMARGR` | left read margin |
| 4 | `MARGR` | right read margin |
| 5 | `LUNUT` | output logical unit (`T` = back to `LUNUTS`) |
| 6 | `PRTPOS` | print cursor |
| 7 | `LMARG` | left print margin |
| 8 | `MARG` | right print margin |
| 9 | `LEVELL` | print length |
| 10 | `LEVELP` | print level |

This is how `script.2` reads Lisp files: open a file on a unit, then redirect input to it
with `(IOTAB 1 unit)`.

Each entry is clamped: the read margins against `IOBUFF`, the print margins against
`IOBUFF-20` (so `priint_`'s 19-digit scratch area stays inside `PRBUFF`), the unit numbers
against `MAXLUN` -- and a unit that is not open is refused. A left margin may not pass its
right margin: `LMARGR > MARGR` made `shift_` read and discard a whole input line per call
and never yield a character, which swallowed the rest of the session.

---

## Coding Conventions

- All FORTRAN-origin names end with underscore: `init1_()`, `garb_()`, `rollin_()`
- Array accesses use `[i__ - 1]` for 1-to-0 based index conversion
- `*SETC*` comments mark where FORTRAN `CALL SETCAR(I,V)` / `CALL SETCDR(I,V)` was translated to direct array assignment
- FORTRAN loop pattern `for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__)` ensures the loop body runs at least once (FORTRAN DO loop semantics)
- Common block struct aliases: `a_1` = `a_`, `b_1` = `b_`, `carcdr_1` = `carcdr_`, etc.
- `#define` macros alias struct members to match FORTRAN EQUIVALENCE statements (e.g., `#define args ((integer *)&b_1.arg)`)
