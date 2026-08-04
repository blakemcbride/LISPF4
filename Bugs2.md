# LISPF4 — Second Deep Analysis (Bugs2)

> **STATUS: all findings below are FIXED** (2026-08-04). The regression suite
> covers them and reports 24/24 passing. See "Resolution" at the end.

Analysis date: 2026-08-04
Companion documents: `Bugs1.md` (first C-only pass, all fixed), `Plan1.md`

**Scope.** A second pass over the C sources — now that every `Bugs1.md` finding is fixed —
**plus the Lisp sources**, which were not examined the first time. The `.lisp` files are the
former `.l` files (renamed in commit `f6e7533`).

**Method.**

- A LISPF4-aware reader was written for static analysis: it handles the `[`/`]`
  super-parenthesis (`]` closes back to the matching `[`), the `%` escape, and `'` quoting.
  Without all three, the results are noise — the first attempts produced hundreds of false
  positives until quoting and `COND` clause structure were modelled correctly.
- Cross-checks run over all 18 Lisp files: package `FNS` lists against actual definitions,
  calls against builtins (parsed from `SYSATOMS`) plus definitions, call arity against
  definitions, and top-level form structure.
- Every finding below was then **confirmed by running the interpreter**. Where it matters,
  behaviour was compared against the shipped pre-fix binary (`Linux/lispf4`) to establish
  whether a defect is pre-existing.
- The C pass used the now-installed ASan/UBSan build: the full regression suite, the
  complete image bootstrap, all four garbage collectors, 4000 lines of malformed reader
  input, and 1500 hostile calls to the argument-validating paths.

---

## Severity summary

| # | Severity | Where | Bug | Status |
|---|---|---|---|---|
| C1 | **High** | `lispf41.c` / `lispf42.c` | `ARRAY` size arithmetic overflows, defeating its own bounds checks → heap buffer overflow; **segfaults the shipped binary** | CONFIRMED |
| C2 | Medium | `lispf42.c` `garb_` | `htab[s-1]` indexed with no lower bound on `s`; `s == 0` reads `htab[-1]` | CONFIRMED |
| L1 | **High** | `ifdo.lisp` | Package fails to load: `[` and `]` are used as less-than/greater-than operators but are the super-parenthesis characters | CONFIRMED |
| L2 | Medium | `basic2.lisp` | `UNION` calls `NOT`, which is not defined anywhere | CONFIRMED |
| L3 | Medium | `func1.lisp` | `DEFINE`'s NLAMBDA branch: `NLAMDA` typo makes it unreachable, and it calls undefined `CDDADR` | CONFIRMED |
| L4 | Medium | `prolog.lisp` | Package is non-functional: undefined `FUNCALL`/`MEMQ`, `POP`/`PUSH` cannot mutate, truncated `COND` clause | CONFIRMED |
| L5 | Info | `basic2.lisp` / `func1.lisp` | `SAVEDEF` and `UNSAVEDEF` defined twice (identical text) | — |

C1, C2, L1–L4 are all **pre-existing** and independent of the `Bugs1.md` fixes.

---

## C1 — HIGH: `ARRAY` size arithmetic overflows, defeating its bounds checks

**Locations**
- `lispf41.c:2928` — `b_1.temp1 = b_1.temp1 - b_1.temp2 - b_1.temp3;`
- `lispf41.c:2935-2936` — `i__1 = -((b_1.temp1 + 3) * a_1.jbytes + (b_1.temp2 + 1) * a_1.ibytes + (b_1.temp3 + 1) * a_1.bytes - 3);`
- `lispf41.c:2937` — `*ires = matom_(&i__1);`
- `lispf41.c:2946` — `arrutl_(ires, &c__4, &ireg, &c__0, &args[ireg + 4]);`
- overflowing write lands in `arrutl_`, `lispf42.c:1052` (and `lispf42.c:1038`)

**Defect.** `(ARRAY total ints reals)` reads three user-supplied integers into `temp1`,
`temp2`, `temp3`. Each is checked individually for being a number and non-negative, but the
*derived* quantities are not checked for overflow:

1. `temp1 - temp2 - temp3` (line 2928) can wrap. The wrapped result can be positive, so the
   `if (b_1.temp1 >= 0)` sanity test passes for dimensions that are nonsense.
2. The byte-size expression (line 2935) then multiplies by `jbytes`/`ibytes`/`bytes` and can
   wrap again, producing a *small* byte count.
3. `matom_` reserves that small amount and succeeds.
4. `arrutl_` is then called with the **raw, huge** element counts and initialises that many
   elements, running off the end of `b_1.pname`.

So the size actually reserved and the size actually written are computed from different
(inconsistent) values, and the guard between them is bypassed by wraparound.

**Reproduction — segfaults the shipped `-O3` binary and image, no sanitizer needed:**

```
$ ./lispf4 basic.img
(ARRAY 0 2147483647 7)
   Segmentation fault (core dumped)
```

Under ASan the write is pinpointed:

```
ERROR: AddressSanitizer: heap-buffer-overflow
WRITE of size 4 at 0x7e97707e5028
    #0 arrutl_ lispf42.c:1052
    #1 lispf4_ lispf41.c:2946
0x... is located 0 bytes after 20008-byte region        <-- b_1.pname, npname=5000
allocated by main lispf42.c:284
```

UBSan independently flags the arithmetic at `lispf41.c:2928`, `:2935` and `lispf42.c:1038`.

**Note on a related, benign case.** `(ARRAY 32767 0 0)` does *not* overflow the arithmetic;
`matom_` correctly refuses it and `ARRAY` reports "Atom space empty. NIL returned". That
path is sound — it is specifically the wraparound that breaks the guard. (It does, however,
expose C2 below.)

**Suggested fix.** Validate the dimensions against what can actually exist before computing
byte sizes. `a_1.npname` bounds the whole print-name/array/bignum area, so no dimension can
sensibly exceed it:

```c
    for (ireg = 1; ireg <= 3 || ireg == 1; ++ireg) {
        b_1.arg = args[ireg - 1];
        if (b_1.arg <= a_1.nfreet) goto L25030;
        args[ireg + 4] = getnum_(&b_1.arg);
        if (args[ireg + 4] < 0 || args[ireg + 4] > a_1.npname) goto L25030;
    }
```

With all three bounded by `npname` (5000 by default, and `-p` is itself capped at 200000000
by the `Bugs1.md` B9 fix), neither the subtraction at 2928 nor the byte-size expression at
2935 can overflow. Keep the existing `temp1 >= 0` test as well.

---

## C2 — MEDIUM: `garb_` indexes `htab[s-1]` without a lower bound

**Location** — `lispf42.c:3917-3923`, label `L652` (atom-compacting GC, STEP 6):

```c
/* A-GBC */
L652:
    if (s > a_1.natom) {
        goto L654;
    }
    if (b_1.htab[s - 1] < 0) {        /* <-- no check that s >= 1 */
        s = -b_1.htab[s - 1];
    }
```

**Defect.** During an atom-compacting GC (`gbctyp == 3`), STEP 6 walks the interpreter
registers, the stacks and every cell, remapping any value that refers to a moved atom. The
guard rejects `s > natom` but not `s <= 0`, so a slot holding **0** reads `htab[-1]` — one
word before the allocation. If that word happens to be negative, `s` becomes
`-htab[-1]`, a fabricated pointer, which is then written back into the register or cell.

Zero is not an exotic value here: `0` is used as a legitimate marker elsewhere
(`jaan_1.jack[tops-1] = 0` means "this block has no variables", `lispf41.c`), and the
general-purpose registers `temp1`/`temp2`/`temp3` routinely hold small integers.

**Reproduction.** `(ARRAY 32767 0 0)` puts the literal `0` dimensions into `temp2`/`temp3`,
then exhausts atom space and triggers the atom GC:

```
ERROR: AddressSanitizer: heap-buffer-overflow
READ of size 4    #0 garb_ lispf42.c:3922  #1 matom_ lispf42.c:4365
0x... is located 4 bytes before 18000-byte region       <-- b_1.htab, nhtab=4500
```

Under gdb, breaking at `lispf42.c:3922` with `s <= 0`:

```
FIRED: s=0  natom=3000  -> reads htab[-1] (valid range 0..4499)
iret=1                                    -- the value came from the args registers
```

In this instance the word before `htab` held 0, so nothing was corrupted — but that is heap
layout luck, not correctness. The shipped `-O3` binary reports the expected
"Atom space empty" error and continues, so today the symptom is silent, not visible.

**Suggested fix.** One line — atoms occupy `1 .. natom`, so anything outside that range
should fall through to the number/cell cases, which already handle it safely:

```c
L652:
    if (s < b_1.nil || s > a_1.natom) {
        goto L654;
    }
```

---

## L1 — HIGH: `ifdo.lisp` cannot be loaded

**Location** — `ifdo.lisp:245, 251, 257, 263`.

**Defect.** `ITP-C`, the infix-to-prefix translator, matches comparison operators written as
bare tokens. Four of them are `[`, `]`, `[=` and `]=`, meaning less-than, greater-than,
less-or-equal and greater-or-equal:

```lisp
                              ((MATCH
                                 '((? (1 T NIL L T))
                                    [                     <-- line 245, "less than"
                                    (? (1 T NIL R T)))
                                 ARG)
                                (LIST 'LESSP (ITP-C L) (ITP-C R)))
```

But `[` and `]` are LISPF4's **super-parenthesis** characters. The reader treats line 245 as
opening a super-bracket and line 251 as closing every open list back to it, which tears the
enclosing `DEFINEQ` apart. Fragments of function bodies then become top-level forms and get
evaluated.

**Reproduction:**

```
$ printf '(READFILE "match.lisp")\n(READFILE "ifdo.lisp")\n(EXIT)\n' | ./lispf4 basic.img
(FILEHEADER IFDO)
(IF AND DO FUNCTIONS WRITTEN BY BLAKE MCBRIDE)
(VERSION 5)
--- Unbound variable
EVAL - L
(EVAL BROKEN)
```

Structurally, `ifdo.lisp` parses to **25** top-level forms; every other package parses to the
expected 9 (`FILEHEADER`, `PRINT`, `PRINT`, `DEFINEQ`, `PRINT`, `RPAQQ`, `RPAQQ`, `RPAQ`,
`STOP`). Load-testing all 18 files, `ifdo.lisp` is the only one that fails.

**Root cause — a regression.** Commit `1b290ce`, *"Changes super-parenthesis from `<>` to
the InterLisp standard `[]`"*, changed the character definition line in `SYSATOMS`:

```
- ()<>"'..T+-0123456789%^E#
+ ()[]"'..T+-0123456789%^E#
```

That commit converted the other packages' `<`…`>` super-brackets to `[`…`]`, but
`ifdo.l` was already using bare `[` and `]` as *data* — comparison operators — which were
ordinary characters until that moment. They were not escaped, and the package has been
unloadable since.

**Suggested fix — use `<` and `>`.** The same commit *freed* those characters, and they are
the natural spelling anyway. Changing the four tokens to `<`, `>`, `<=`, `>=` restores the
package, verified:

```lisp
(ITP 1 < 2)   => T        (ITP 3 > 9)    => NIL      (ITP 2 <= 2) => T
(ITP 3 >= 9)  => NIL      (ITP 3 + 4)    => 7        (ITP 12 / 4) => 3
(IF (EQ 1 1) THEN (QUOTE YES) ELSE (QUOTE NO))             => YES
(IF (ITP 3 >= 9) THEN (QUOTE GEQ) ELSE (QUOTE NOTGEQ))     => NOTGEQ
(SETQ I 0) (DO WHILE (LESSP I 5) DO (SETQ I (ADD1 I))) I   => 5
(SETQ J 0) (DO UNTIL (EQ J 3) DO (SETQ J (ADD1 J)))   J    => 3
```

**Correction.** An earlier draft of this section claimed `(IF 1 < 2 THEN ...)` works. It does
not, and the apparent successes were coincidences — `IF-C` uses only the single token
immediately before `THEN` as the condition and silently discards the rest, so
`(IF 1 < 2 THEN ...)` evaluates `(COND (2 ...))`, which is true for the wrong reason.
`(IF 3 >= 9 THEN ...)` exposes it by returning the THEN branch when it should not. `ITP` is
the infix entry point (`ITP-C` is its compiler); `IF` takes one condition form. That
`IF-C` quietly ignores surplus condition tokens instead of rejecting them is a separate,
pre-existing robustness wart, unaffected by this fix and not addressed here.

Escaping them instead (`%[`, `%]`, `%[=`, `%]=`) also makes the file load, and was verified
to do so — but it is the worse fix: users would then have to write `%[` in their own source
to use the less-than operator. `<` and `>` need no escaping anywhere.

`printa.lisp` depends on `DO`, so it is affected too, though it loads on its own.

---

## L2 — MEDIUM: `UNION` calls undefined `NOT`

**Location** — `basic2.lisp:674-675`, inside `UNION`:

```lisp
                      ([COND ((LITATOM (CAR X))
                               (NOT (MEMB (CAR X) Y)))
                             (T (NOT (MEMBER (CAR X) Y]
```

`NOT` is not a LISPF4 builtin (`SYSATOMS` has `NULL`, not `NOT`) and is not defined in any
package. `UNION` is listed in `BASIC2FNS`, so it is a supported, documented entry point.

**Reproduction** (identical on the shipped pre-fix binary — pre-existing):

```
(UNION (QUOTE (A B)) (QUOTE (B C)))
--- Undefined function
APPLY - NOT
(APPLY BROKEN)
```

**Suggested fix.** Either replace both calls with `NULL`, which is the LISPF4 builtin with
exactly the required meaning, or add `(NOT (LAMBDA (X) (NULL X)))` to `basic2.lisp` and its
`BASIC2FNS` list. Replacing with `NULL` is the smaller change and needs no package edit.

---

## L3 — MEDIUM: `DEFINE`'s NLAMBDA branch is both unreachable and broken

**Location** — `func1.lisp:19-27`, inside `DEFINE`:

```lisp
                                             [(NULL (CDDR Y))
                                               (COND
                                                 [(EQ (CAADR Y)
                                                      'NLAMDA)          <-- typo
                                                   (CONS 'NLAMBDA
                                                     (CONS
                                                       (CAADR (CADR Y))
                                                       (CDDADR Y]       <-- undefined
                                                 (T (CADR Y]
```

Two defects in one expression:

1. **`'NLAMDA` is missing its `B`.** It occurs exactly once in the entire tree; every other
   occurrence is `NLAMBDA`. A correctly written `(NLAMBDA ...)` definition therefore never
   matches, and the branch is dead.
2. **`CDDADR` does not exist.** `SYSATOMS` provides the C…R combinations only three deep
   (`CAAAR` … `CDDDR`); `CDDADR` is four deep, and nothing defines it.

The branch's intent is clear from the code: convert `(NLAMDA (L) . body)` — a nospread
NLAMBDA whose single argument is wrapped in a list — into `(NLAMBDA L . body)`.

**Reproduction.** Correct spelling silently skips the branch (harmless, but the intended
unwrapping never happens):

```
(DEFINE (QUOTE ((FOO (NLAMBDA L (CAR L))))))
(GETD (QUOTE FOO))   =>  (NLAMBDA L (CAR L))     -- stored verbatim by the fallback
```

The spelling the branch actually tests for reaches the undefined call:

```
(DEFINE (QUOTE ((BAR (NLAMDA (L) (CAR L))))))
--- Undefined function
APPLY - CDDADR
```

**Suggested fix.** Correct the typo to `'NLAMBDA` **and** replace `(CDDADR Y)` with
`(CDDR (CADR Y))`, which is what it means and uses only existing builtins. Fixing only the
typo would turn a dead branch into a live broken one, so both are required together.

---

## L4 — MEDIUM: `prolog.lisp` is non-functional

**Location** — `prolog.lisp`.

Three independent defects:

1. **Undefined functions.** `FUNCALL` (lines 38, 39) and `MEMQ` (line 60) are called but
   defined nowhere and are not builtins — `MEMQ` is a MacLisp/Common Lisp name; the
   InterLisp equivalent here is `MEMB`.
2. **`POP` and `PUSH` cannot work as written.**
   ```lisp
   (POP  [LAMBDA (S)   (PROG1 (CAR S) (SETQ S (CDR S])
   (PUSH (LAMBDA (V S) (SETQ S (CONS V S))))
   ```
   Each `SETQ`s its own parameter, which is a local binding — the caller's variable is
   untouched. They would have to be `NLAMBDA`s (or macros) to have any effect.
3. **Truncated `COND` clause.** `SEEK` ends `... ((DO])`, a clause with a test and no body,
   closed by a super-bracket.

**Reproduction.** The file loads without error (the defects are all at run time):

```
(FINAL (QUOTE (A NIL B)))
--- Undefined function
APPLY - MEMQ

(SETQ S (QUOTE (1 2 3)))
(POP S)   =>  1
S         =>  (1 2 3)          -- unchanged; POP did nothing
```

**Suggested action.** This reads as unfinished work rather than a package with a small bug —
`SEEK` is visibly incomplete. Either finish it, or mark it experimental in `README.md` and
`KnowledgeBase.md` the way `editv.lisp` already is. Note `KnowledgeBase.md` currently lists
it without qualification as "Prolog-like features".

---

## L5 — INFO: duplicate definitions

`SAVEDEF` and `UNSAVEDEF` are defined in both `basic2.lisp` and `func1.lisp`. The two
versions are textually identical, and `func1.lisp` loads second so its copies win — harmless
today, but they can drift. Worth deleting one pair (and the corresponding `FNS` entry).

---

## Checked and found clean

Recording what was verified, so the absence of findings is meaningful:

**Lisp**

- Every package's `pkgFNS` list matches its actual definitions exactly — no declared-but-missing
  or defined-but-undeclared functions, in any of the 18 files.
- All 18 files were load-tested; `ifdo.lisp` is the only one that fails.
- A full `MAKEFILE` → `LOAD` round-trip preserves functions, strings (with quotes), floats,
  quote forms and variables exactly. The `(PRIN1 X T)` calls in `makef.lisp` pass an argument
  `PRIN1` ignores, but `PRETTYPRINT` uses `(PRIN0 ... T OPT)` for the actual output, so file
  fidelity is unaffected.
- Arity mismatches are almost all idiomatic: InterLisp binds missing arguments to `NIL`, so
  under-calling is how optional arguments work. Extra arguments are bound to `-*-` and
  ignored; no case was found where this changes behaviour.
- `match`, `quote`, `static`, `struct`, `astruct`, `printa`, `schum` all load cleanly.

**C** (all under ASan + UBSan, strict options)

- The full 19-case regression suite.
- The complete two-stage image bootstrap, producing a `basic.img` byte-identical to the
  `-O3` build.
- All four collectors under load, including a live array relocated by `arrutl_` inside
  `garb_` STEP 4.
- 4000 lines of malformed reader input (random parens, brackets, quotes, escapes): clean.
- 1500 hostile calls to `XCALL`, `IOTAB`, `ARRAY`, `SETA`, `ELT`, `SUBSTRING`, `RECLAIM`,
  `REWIND` with boundary integers — this is what surfaced C1 and C2.
- `abuff` overflow, left open in `Bugs1.md` as "tight but correct": now settled empirically.
  With `margr` at its maximum of 160, a 160-character atom fills `abuff[159]` exactly, with
  no ASan report. The bound is correct.
- A 65-expression differential test (arithmetic, lists, strings, properties, control flow,
  mapping, sorting) against the shipped pre-fix binary: **byte-identical output**, confirming
  the `Bugs1.md`/`Plan1.md` work introduced no behavioural regression.

---

## Suggested fix order

1. **C1** — a one-line bound in `ARRAY`'s argument loop removes a reproducible segfault of
   the shipped binary.
2. **L1** — four token changes restore an entire package. Highest value per character.
3. **C2** — one-line lower bound in `garb_`.
4. **L2, L3** — small, local Lisp corrections.
5. **L4, L5** — decide whether `prolog.lisp` is supported; drop one copy of the duplicates.

C1 and C2 need no image rebuild. L1–L4 change `.lisp` sources, so `basic.img` must be
regenerated for L2/L3 (which are in loaded packages):

```
make realclean && make && ./tests/run-tests.sh
```

Each fix should get a regression case in `tests/` — `L1` and `C1` in particular are easy to
express and both fail loudly today.

---

## Resolution (2026-08-04)

All findings fixed. Covered by `tests/cases/c1-array-bounds`, `l1-ifdo`, `l2-union`,
`l3-define`, `l5-savedef`; suite is 24/24 on both the `-O3` and the ASan/UBSan build.

| # | Change |
|---|---|
| C1 | `lispf41.c` — each dimension is now bounded by `a_1.npname` in `ARRAY`'s argument loop, so neither the subtraction nor the byte-size expression can wrap. `(ARRAY 0 2147483647 7)` now gives "Illegal argument (subr3)" instead of a segfault; the 385-case ARRAY fuzz corpus went from 2 ASan + 6 UBSan findings to **zero**. |
| C2 | `lispf42.c` — `L652` now tests `s < b_1.nil || s > a_1.natom`, so a slot holding 0 no longer reads `htab[-1]`. |
| L1 | `ifdo.lisp` — the four operator tokens `[ ] [= ]=` became `< > <= >=` (lines 245, 251, 257, 263). The package loads, and `ITP` evaluates infix comparison and arithmetic correctly. |
| L2 | `basic2.lisp` — `UNION` now calls `NULL` instead of the non-existent `NOT`. |
| L3 | `func1.lisp` — the dead `NLAMDA`/`CDDADR` branch was **removed** rather than repaired (see below). |
| L4 | `prolog.lisp` — `MEMQ`→`MEMB`, `FUNCALL`→`APPLY*`, and `POP`/`PUSH` became `NLAMBDA`s that `SET` the caller's variable. **`SEEK` is still incomplete** (see below). |
| L5 | `func1.lisp` — its duplicate `SAVEDEF`/`UNSAVEDEF` (and the `FUNC1FNS` entries) were dropped; `basic2.lisp`'s copies remain, since that package loads first. |

### Why L3 was removed rather than repaired

Correcting the `NLAMDA` typo alone would have made a dead branch live — and *broken* working
code. The branch computes `(CAADR (CADR Y))`, i.e. `CAR` of the definition's argument list.
For the ordinary nospread form `(NLAMBDA L . body)` that argument list is the **atom** `L`,
so the branch would take `CAR` of an atom and fail. It only makes sense for
`(NLAMBDA (L) . body)`, and rewriting that to `(NLAMBDA L . body)` would silently convert a
spread NLAMBDA into a nospread one — a semantic change, not a fix.

The branch was unreachable for every correct input, and errored for the one input it tested
for. Deleting it is behaviour-preserving for all valid forms and removes the undefined call.
Verified: all three `DEFINE` shapes — `(NAME (LAMBDA ...))`, `(NAME (NLAMBDA ...))` and
`(NAME ARGS . BODY)` — still work.

### What was *not* fixed in `prolog.lisp`

`SEEK`'s third `COND` clause is `((DO])` — a test with no body. That is the compound-goal
case at the heart of the resolution loop, and the source is simply truncated. Completing it
would mean inventing Prolog semantics, so it was left alone and the package is now marked
experimental in `README.md` and `KnowledgeBase.md`. The three mechanical defects around it
are fixed and verified (`(POP E)` now returns `A` and leaves `E` as `(B)`; `FINAL` works).

One trap worth recording: the first `POP`/`PUSH` rewrite named the `NLAMBDA` parameter `S`,
which under dynamic scoping **captured** a caller variable also named `S`, so `(POP S)`
silently did nothing. The parameters are now `POP-S-`, `PUSH-V-`, `PUSH-S-`, following the
`IF-ARGS-` convention already used in `ifdo.lisp`.
