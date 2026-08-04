# LISPF4 vs. the Interlisp Reference Manual — Variance Report

> **STATUS: V1–V5 are FIXED** (2026-08-04) and covered by
> `tests/cases/v-standard`. Suite is 25/25 on both the `-O3` and the
> ASan/UBSan build. See "Resolution" at the end.

Analysis date: 2026-08-04
Standard: `Documentation/Interlisp-Oct_1978.pdf` — *Interlisp Reference Manual*, Teitelman et
al., BBN / Xerox, October 1978 (773 pages), treated here as the source of truth for function
names, arguments and behaviour.

Companion documents: `Bugs1.md`, `Bugs2.md` (defect analyses, all fixed), `Plan1.md`.

---

## Method

The PDF is a scan carrying an Adobe "Paper Capture" OCR layer. Its quality is **uneven, and
that shaped the method**:

- The per-section **function indexes OCR cleanly** and give `NAME[ARG1;ARG2;…]` plus the type
  (`SUBR`, `FSUBR`, `NL`, …). 898 distinct function signatures were extracted from them. This
  is the reliable machine-readable ground truth for **names, argument count and argument order**.
- The **body prose is badly degraded** in places (`!l@~`, `Ul3t`, `(COR X Y»`). It also writes
  function names in **lower case** (`tconc[ptr;x]`, `sort[a-list;T]`) — an initial pass that
  searched for upper case found nothing and would have produced a falsely clean report.
  Definitions were located by page, and 605 prose definitions recovered.
- Nothing here rests on OCR alone: **every variance below was confirmed by running the
  interpreter**, and checked against the shipped pre-fix binary (`Linux/lispf4`) to establish
  whether it predates the recent work.

LISPF4's own side was derived mechanically: builtin arity from the `SYSATOMS` group each name
appears in (SUBR0/SUBR1/SUBR11/SUBR2/SUBR3/SUBRN/FSUBR), and Lisp-level arity from the
`LAMBDA`/`NLAMBDA` argument lists in the `.lisp` sources.

## Coverage

| | count |
|---|---|
| Functions documented in the manual | 898 |
| Functions provided by LISPF4 | 312 |
| — of those, documented in the manual | 195 |
| — LISPF4 extensions, not in the manual | 117 |
| Manual functions LISPF4 does not provide | 703 |

The 703 absences are **not** defects: `README.md` states LISPF4 "supports much of the InterLisp
standard", and the 1978 manual documents a very large residential system (file packages,
DWIM/CLISP, the compiler, masterscope, display and network I/O) far beyond this interpreter's
scope. The 117 extensions are mostly LISPF4-specific internals (`APPLYSTK`, `BINDENV`,
`XCALL`, `IOTAB`, `-*-`) and Blake's additions (`<`, `>`, `+`, `-`, `DSORT`, `SYSIN`/`SYSOUT`).

**Where LISPF4 does implement a documented function, agreement is high.** Argument order was
checked by comparing parameter names against the manual's: 39 functions match the manual's
parameter names exactly, 5 more match as a prefix (LISPF4 omitting trailing optional
arguments), and the ~50 remaining differences are cosmetic — the same positions under
different local names, e.g. LISPF4 `ADDPROP(A IND S FLAG)` vs manual `addprop[atm;prop;new;flg]`.

Behavioural testing reproduced the manual's own worked examples exactly:

| Manual example | LISPF4 |
|---|---|
| `subpair[(A C);(X Y);(A B C D)] = (X B Y D)` | `(X B Y D)` |
| `lsubst[(A B);Y;(X Y Z)] = (X A B Z)` | `(X A B Z)` |
| `pack[(A BC DEF G)] = ABCDEFG` | `ABCDEFG` |
| `unpack[ABC] = (A B C)` | `(A B C)` |
| `every[(X Y Z);ATOM] = T` | `T` |
| `nchars["ABC"]=3`, `nchars["ABC";T]=5` | `3`, `5` |
| `last[(A B . C)] = (B . C)` | `(B . C)` |
| `nth[x;0] = cons[NIL;x]` | `(NIL A B C D)` |

`SUBST`, `SUBLIS`, `REMOVE`, `UNION`, `INTERSECTION` (with de-duplication), `NLEFT`, `COPY`,
`SORT`/`DSORT`, `SUBSTRING` (including `NIL` and negative indices), `TAILP` on proper tails,
`ALPHORDER` (numbers before literal atoms), `SUBSET`, `MAPCONC`, `DREMOVE`, `LISTGET`,
`PUTASSOC` and `CHANGEPROP` all match the documented behaviour.

---

## Variances found

Five behavioural variances. **All five are pre-existing** — verified identical on the shipped
`Linux/lispf4` binary — and none was introduced by the `Bugs1.md`/`Bugs2.md` work. None is
currently covered by a regression test.

### V1 — `LAST` of a non-list returns its argument instead of `NIL`

*Manual (§6, p78):* "last[x] … Value is `NIL` if x is not a list."

```
(LAST (QUOTE A))  =>  A        expected NIL
(LAST 5)          =>  5        expected NIL
(LAST NIL)        =>  NIL      correct
```

`lispf41.c:1813` — the `LAST` builtin falls straight through to the common return when the
argument is not a list, handing back `b_1.arg` unchanged:

```c
L11255:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L998;                  /* returns ARG; standard requires NIL */
    }
```

Fix: jump to the `NIL` return (`L3090`) instead of `L998`.

### V2 — `TAILP` misses the zero-`CDR` case

*Manual (§6, p70):* "tailp[x;y] Is x, if x is a tail of y, i.e. x is `EQ` to some number of
cdrs **≥ 0** of y. `NIL` otherwise."

"≥ 0" includes zero cdrs, i.e. `x` `EQ` `y`.

```
(SETQ L (QUOTE (A B C)))
(TAILP (CDR (CDR L)) L)  =>  (C)      correct
(TAILP (CDR L) L)        =>  (B C)    correct
(TAILP L L)              =>  NIL      expected (A B C)
```

`lispf41.c:2809-2814` — the loop takes `CDR` *before* its first comparison, so the zero-cdr
case is never tested:

```c
L12746:
    if (b_1.arg2 <= a_1.natom || b_1.arg2 > a_1.nfreet) goto L3090;
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];      /* cdr taken first */
    if (b_1.arg == b_1.arg2) goto L998;
    goto L12746;
```

Fix: compare before taking the first `CDR`.

### V3 — `RPT` / `RPTQ` return `NIL` instead of the last value

*Manual (§10, p102):* "rpt[rptn;rptf] Evaluates the expression rptf rptn times… **Returns the
value of the last evaluation.** If rptn ≤ 0, rptf is not evaluated, and the value of rpt is
`NIL`." `rptq` is the nlambda, nospread version.

```
(RPT 3 (QUOTE (QUOTE X)))  =>  NIL     expected X
(RPTQ 3 (QUOTE X))         =>  NIL     expected X
```

`lispf41.c`, label `L12650` — the loop exit unconditionally branches to the `NIL` return:

```c
L12650:
    b_1.jp += 2;
    goto L3090;                 /* NIL; standard requires the last value */
```

The zero/negative case is already correct. Fix: keep the last evaluation's result and return
it, returning `NIL` only when the body never ran.

### V4 — `KWOTE` quotes `NIL` and numbers

*Manual (§5, p61):* "kwote[x] Value is an expression which when evaluated yields x. **If x is
`NIL` or a number, this is x itself.** Otherwise `(LIST (QUOTE QUOTE) x)`."

```
(KWOTE (CONS (QUOTE A) (QUOTE B)))  =>  '(A . B)   correct (manual's own example)
(KWOTE NIL)                         =>  'NIL       expected NIL
(KWOTE 5)                           =>  '5         expected 5
```

`debug1.lisp:152` wraps unconditionally:

```lisp
(KWOTE
  (LAMBDA (X) (LIST 'QUOTE X)))
```

Fix:

```lisp
(KWOTE
  (LAMBDA (X)
          (COND ((OR (NULL X) (NUMBERP X)) X)
                (T (LIST 'QUOTE X)))))
```

Note `'NIL` and `'5` still *evaluate* to `NIL` and `5`, so this rarely bites; it matters when
the result is compared or printed.

### V5 — `NTHCHAR` does not accept a negative index

*Manual (§10, p181):* "nthchar[x;n;flg;rdtbl] Value is nth character of pname of x… **n can be
negative, in which case counts from end of pname**, e.g. -1 refers to the last character."

```
(NTHCHAR (QUOTE ABCDE) 2)   =>  B      correct
(NTHCHAR (QUOTE ABCDE) -1)  =>  NIL    expected E
```

`basic2.lisp` defines it in terms of `NTH`:

```lisp
(NTHCHAR
  (LAMBDA (X N F) (CAR (NTH (UNPACK X F) N))))
```

`NTH` itself is correct per the standard — for `n ≤ 0` it returns `cons[NIL;x]`, so `CAR`
yields `NIL`. The negative-index conversion simply was never written. Fix: convert a negative
`N` to `(PLUS (LENGTH lst) N 1)` before the `NTH`.

Note `SUBSTRING` *does* handle negative indices correctly (`(SUBSTRING "ABCDEF" -2 -1)` →
`"EF"`), so this is an inconsistency within LISPF4 as well as a variance from the standard.

---

## Deliberate divergences (not defects)

Recorded so they are not re-reported later:

- **`*` is multiplication, not the comment marker.** In the 1978 standard and in
  `lispf4.orig`, `(* …)` is a comment. LISPF4 renamed the comment marker to `-*-` and
  reassigned `*` to `TIMES`. See the original-source comparison for details.
- **`GETP`/`PUT` are named `GETPROP`/`PUTPROP`.** The manual uses `getp`/`put`; LISPF4's
  system-atom table uses the longer names in the same slots. `GETPROP`/`PUTPROP` are also
  valid Interlisp names.
- **`SORT` is `DSORT`.** LISPF4 renamed the standard `sort[data;comparefn]`; the body is
  unchanged.
- **I/O functions omit their `FILE`/`RDTBL` arguments.** `PRINT[X]`, `PRIN1[X]`, `PRIN2[X]`,
  `SPACES[N]` etc. print to the current output unit, selected with `IOTAB`, rather than taking
  a file argument. This is a systematic, coherent simplification, not an oversight.
- **`PACK` takes an optional second argument** (a `FLG`, as `NCHARS`/`UNPACK` do) where the
  manual's `pack[x]` takes one. A superset, harmless.
- **`REMOVE` has a third parameter** used as an internal accumulator; called with two
  arguments it behaves as documented.

---

## Suggested fix order

V1, V2 and V3 are C-side one-liners in `lispf41.c`; V4 and V5 are small Lisp edits.

1. **V2 (`TAILP`)** and **V1 (`LAST`)** — smallest, and both are plain conformance bugs.
2. **V3 (`RPT`/`RPTQ`)** — needs the loop to carry the last value, so slightly more than a
   one-line change.
3. **V4 (`KWOTE`)**, **V5 (`NTHCHAR`)** — Lisp-only; require regenerating `basic.img`.

Each is easy to express as a regression case; all five fail today and would fail loudly if
reintroduced:

```
make realclean && make && ./tests/run-tests.sh
```

## Confidence and limits

- The **names / arity / argument-order** comparison is comprehensive: all 898 documented
  signatures against all 312 LISPF4 functions.
- The **behavioural** comparison is a sample, not exhaustive: roughly 45 functions were
  executed and compared against documented behaviour, chosen for being widely used or for
  having a worked example in the manual. Functions whose prose did not survive OCR legibly
  were not behaviourally verified; a second pass reading page images directly could extend
  the coverage.
- No claim here rests on OCR text alone — every variance was reproduced in the interpreter.

---

## Resolution (2026-08-04)

All five variances fixed. Each was verified to fail on the shipped pre-fix binary and pass
after; the whole set is covered by `tests/cases/v-standard`.

| # | Change |
|---|---|
| V1 | `lispf41.c` `L11255` — the non-list exit now branches to the `NIL` return instead of falling through to the common return, which handed back the argument. The list-exhausted exit in the loop body is untouched. |
| V2 | `lispf41.c` `L12746` — the `EQ` test moved ahead of the `CDR`, so "some number of cdrs ≥ 0" now includes zero. The list check stays first, so `(TAILP 'A 'A)` still yields `NIL` rather than newly treating an atom as a tail of itself. |
| V3 | `lispf41.c` `L12620`–`L12650` — `RPT` pushes a third A-stack slot holding the last evaluation's value (initialised to `NIL`), updates it on each return, and returns it. Slots are `JP-1`=value, `JP`=form, `JP+1`=count; the pop became `JP += 3`. |
| V4 | `debug1.lisp` — `KWOTE` returns `X` unchanged when it is `NIL` or a number. |
| V5 | `basic2.lisp` — `NTHCHAR` converts a negative `N` to `(PLUS (LENGTH lst) N 1)` before indexing. |

### Why the A-stack for V3

`RPT` evaluates arbitrary user code, so a garbage collection can happen between iterations.
The last value therefore cannot live in a C local — it has to sit somewhere `garb_` scans and
relocates. The A-stack qualifies (STEP 6 walks it from `JP` to `NSTACK`), so the value is
pushed there. Verified: `(RPT 300000 '(CONS 'A 'B))` runs through **3 cell collections** and
still returns an intact `(A . B)`; a float-producing body survives a bignum collection.

### Checking for collateral damage

The concern with V1–V3 is that they change values other code already depends on. Every caller
was identified and checked:

- **`TAILP` drives the editor's `UP` and `NX` navigation** (`edit.lisp:66, 250, 269`, testing
  `(TAILP CL (CADR CTLS))`), so this was the highest risk. A scripted editing session
  exercising descend, `UP`, `NX`, `0` and `P` over two functions produces **byte-identical
  output** before and after.
- **`LAST`** is called by `TCONC` and `LCONC` (`basic2.lisp`). `TCONC` passes `(LIST A)` —
  always a list, so unaffected. `LCONC` guards with `(OR A (RETURN P))`. Both verified to build
  `(1 2)` and `(A B C)` correctly.
- **`RPTQ`** is used once, in `TRACE-PRINT` (`debug2.lisp:199`), purely for its printing side
  effect; its value is discarded. **`KWOTE`** is used twice in `debug2.lisp` on function names
  (atoms), which the fix does not touch — and `'NIL` and `NIL` evaluate identically anyway.
  `TRACE`/`UNTRACE`/`ADVISE`/`UNADVISE` all verified working.
- A 65-expression differential against the shipped pre-fix binary (arithmetic, lists, strings,
  properties, control flow, mapping, sorting) is **byte-identical** — the only behaviour that
  changed is the five intended cases.
- Full suite green on both builds; ASan+UBSan report nothing on the suite, on the `RPT`/GC
  stress, or on the caller tests.

### Correction: `UNADVISE` is *not* broken

An earlier draft of this report claimed `UNADVISE` "appears not to remove advice". **That was
my testing error, not a defect.** `UNADVISE` is a no-spread `NLAMBDA`, so it takes function
names *unevaluated*; I called it as `(UNADVISE (QUOTE AFN))`, which passes the list
`(QUOTE AFN)` as the name to be unadvised, so of course nothing matched.

Called correctly it works, including the no-argument form that unadvises everything on
`ADVISEDFNS`:

```
(ADVISE (QUOTE AFN) (QUOTE BEFORE) (QUOTE (PRIN1 (QUOTE IN))))
(AFN 4)          =>  IN8          advice fires
(UNADVISE AFN)   =>  (AFN)
(AFN 4)          =>  8            advice gone
(UNADVISE)       =>  (BFN NIL)    no-argument form
```

This matches the standard exactly. The manual's index marks it `UNADVISE[X] NL*` and the prose
(§19, p449) reads "unadvise[x] is a no-spread NLAMBDA a la unbreak", whereas
`advise[fn;when;where;what]` carries no `NL` marker and does evaluate its arguments. So the
asymmetry between `ADVISE` and `UNADVISE` — easy to trip over, as I did — is prescribed by
Interlisp, and LISPF4 implements both correctly.
