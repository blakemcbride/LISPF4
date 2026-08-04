# LISPF4 — C Code Bug Analysis (Bugs1)

> **STATUS: all findings below are FIXED** (2026-08-03), by the six phases of
> `Plan1.md`, plus B17 and the three follow-up items at the end. Every
> reproduction in this document now behaves correctly; the regression suite in
> `tests/` covers them and reports 19/19 passing. The
> descriptions are kept in the past-tense sense of "what was wrong" — they
> document the defects and the reproductions that proved them, not the current
> state of the code. See "Resolution" at the end for the per-bug mapping.

Analysis date: 2026-08-03
Scope: `lispf41.c`, `lispf42.c`, `auxillary.c`, `f2c.h` only. The FORTRAN sources
(`Lispf41.f`, `Lispf42.f`, `F4COM.FOR`, `lispf4.orig`) were read **for reference only**,
to establish root causes; every fix proposed here is a C-side change.

Method: full read of all C sources, followed by empirical verification. Every finding
marked **CONFIRMED** was reproduced by running the interpreter. Findings B1–B3 and B8
were reproduced against the **shipped `lispf4` binary and shipped `basic.img`** at the
repo root, not just a debug build. A debug build (`gcc -g -O0`, same `-D` parameters) plus
gdb was used to pinpoint locations. ASan/UBSan runtimes are not installed on this machine,
so bounds violations were confirmed with targeted gdb breakpoints rather than a sanitizer.

---

## Severity summary

| # | Severity | Component | Bug | Status |
|---|---|---|---|---|
| B1 | **Critical** | arrays | `jpname` typed `shortint *`; array pointer slots truncated to 16 bits | CONFIRMED |
| B2 | **Critical** | GC | Compacting bignum GC destroys live floating-point numbers | CONFIRMED |
| B3 | **Critical** | eval core | `(shortint)` cast in the string/array/substring atom test causes false positives | CONFIRMED |
| B4 | **High** | I/O | `XCALL` does not range-check the logical unit number → OOB array access, crash | CONFIRMED |
| B5 | **High** | I/O | `f4_rewind`/`f4_write`/`f4_read*` dereference NULL `FILE*` for unopened units | CONFIRMED |
| B6 | **High** | I/O | `mkcha_` overruns the caller's 50-byte buffer; length argument ignored | CONFIRMED |
| B7 | Medium | printer | `priint_` writes up to 19 elements past `prbuff[160]` | CONFIRMED |
| B8 | Medium | images | Image load has no EOF/size/format validation | CONFIRMED |
| B9 | Medium | startup | Command-line memory sizes unvalidated → crash | CONFIRMED |
| B10 | Low | messages | `mess_` writes back through its argument pointer; no lower bound | by inspection |
| B11 | Low | portability | Character packing assumes little-endian | by inspection |
| B12 | Low | portability | Incompatible prototypes for `getch_`/`putch_` across TUs | by inspection |
| B13 | Low | signals | Signal handler writes non-`volatile sig_atomic_t` globals | by inspection |
| B14 | Low | portability | `toupper()` called with possibly-negative `char` | by inspection |
| B15 | Low | I/O | `read_status` is global across all logical units | by inspection |

B1, B2 and B3 all stem from one root cause, described below.

---

## Root cause shared by B1, B2, B3

The original FORTRAN declares the *same* array and the *same* helper with two different
widths in different subprograms:

| Location (FORTRAN) | Declaration |
|---|---|
| `Lispf42.f:384` (`ARRUTL`) | `INTEGER*2 JPNAME(1)` |
| `Lispf42.f:1709` (`GARB`) | `INTEGER*2 JPNAME(1),JDUMMY` |
| `Lispf42.f:674` (`ROLLIN`) | `INTEGER JPNAME(6006)` |
| `Lispf42.f:770` (`ROLLOUT`) | `INTEGER JPNAME(6006)` |
| `Lispf41.f:17` | `INTEGER*2 JPNAME(1),JDUMMY` |
| `Lispf41.f:25` | `SPECAT(JDUMMY) = JDUMMY.LE.SUBSTR .AND. JDUMMY.GE.ARRAY` |

Meanwhile `INIT1` sets `JBYTES = 4` (`Lispf42.f:501`), i.e. the packing unit is 4 bytes,
not 2. The `INTEGER*2` declarations are leftovers from a half-word-packed host and are
inconsistent with `JBYTES=4` and with `ROLLIN`/`ROLLOUT`.

F2C translated this faithfully, so the C carries the same inconsistency:

```
lispf41.c:157   #define jpname  ((shortint *) b_1.pname)     <-- 2-byte
lispf42.c:862   #define jpname  ((shortint *) b_1.pname)     <-- 2-byte
lispf42.c:3296  #define jpname  ((shortint *) b_1.pname)     <-- 2-byte
lispf42.c:1412  #define jpname  ((integer *) b_1.pname)      <-- 4-byte  (rollin_)
lispf42.c:1574  #define jpname  ((integer *) b_1.pname)      <-- 4-byte  (rollou_)
```

and the `SPECAT` statement function became a narrowing cast to `shortint`:

```
lispf41.c:360, 1714, 1873, 2567, 2591, 2966
lispf42.c:3402, 3602, 4118
    s__1 = (shortint) carcdr_1.car[...];
    if (s__1 <= b_1.substr && s__1 >= b_1.array) ...
```

`shortint` is `short` (`f2c.h:10,18`). Lisp pointers routinely exceed 16 bits: with the
default `-DCELLS=100000` cons cells occupy indices 3001…100000, big numbers start at
`BIGNUM = 103000`, and small integers are encoded as `value + NUMADD` where
`NUMADD = 1073793324`. Truncating any of these to 16 bits is destructive.

**Recommended fix (single change, covers B1 and B2):** make `jpname` `((integer *) b_1.pname)`
in `lispf41.c:157`, `lispf42.c:862` and `lispf42.c:3296`, matching `rollin_`/`rollou_`, and
delete the `(shortint)` casts on every store through it (`lispf41.c:3040`;
`lispf42.c:1007, 1056, 1058, 3699, 3820`). For B3, delete the `s__1` casts and compare
`carcdr_1.car[...]` directly as `integer`.

---

## B1 — CRITICAL: array pointer slots are truncated to 16 bits (`SETA`/`ELT`)

**Locations**
- `lispf41.c:157` — `#define jpname ((shortint *) b_1.pname)`
- `lispf41.c:3040` — `SETA`: `jpname[jndex - 1] = (shortint) b_1.arg3;`
- `lispf41.c:2095` — `ELT`: `*ires = jpname[b_1.temp1 - 1];`
- `lispf42.c:862, 1007, 1056, 1058` — `arrutl_` (array layout/creation)
- `lispf42.c:3296, 3820` — `garb_` (array pointer relocation)

**Defect.** `SETA` stores a full Lisp pointer through a `short *`, discarding the top 16
bits; `ELT` reads it back sign-extended. Additionally, because `jbytes == 4`, indexing
`b_1.pname` (a `real*`, 4 bytes) through a 2-byte pointer addresses the wrong slot:
element *n* lands at byte offset `2*(n-1)` instead of `4*(n-1)`.

Consequently the *pointer* part of an array only works for values that happen to fit in a
signed 16-bit quantity — in practice only low-numbered literal atoms. Every cons cell,
every number, and every atom beyond index 32767 is corrupted. The integer part (`SETI`/`ELTI`,
via `ipname`, `integer *`) and the real part (`SETR`/`ELTR`, via `b_1.pname`, `real *`) are
correctly typed and do work.

**Reproduction** (`./lispf4 basic.img`):

```lisp
(SETQ A (ARRAY 15 5 5))
(SETA A 1 (QUOTE FOO))   (ELT A 1)   ; => FOO      correct (atom index 780 fits in 16 bits)
(SETA A 2 (QUOTE (X Y))) (ELT A 2)   ; => (NIL NIL NIL ... ---)   WRONG: returns the free list
(SETA A 3 99)            (ELT A 3)   ; => ...                     WRONG
(SETI A 1 12345)         (ELTI A 1)  ; => 12345    correct
(SETR A 1 3.25)          (ELTR A 1)  ; => 3.25     correct
```

`(ELT A 2)` walks the free list because the cons pointer (~90000) truncated to a small
index. `(ELT A 3)` returns garbage because the small-integer encoding `99 + NUMADD`
(1073793423) truncated to 16 bits.

**Fix.** As described in "Root cause" above. Note `lispf42.c:1056/1058` also store `a_1.jbp`
(a byte offset that reaches `4 * npname`) through the same cast — that overflows a `short`
as soon as `-p` exceeds ~8000, independently of the indexing problem.

---

## B2 — CRITICAL: the compacting bignum GC destroys live floating-point numbers

**Locations**
- `lispf42.c:3699` — `jpname[j - 1] = (shortint) (itop + a_1.nfreet);` (STEP 5, forwarding pointer)
- `lispf42.c:3862` — `s = jpname[icar - 1];` (STEP 6, pointer relocation)
- `lispf42.c:3296` — the `shortint *` definition of `jpname`

**Defect.** When the bignum/float area is compacted (`garb_` with `gbctyp` 2 or 3, reached
from `mkreal_` at `lispf42.c:4375, 4389` and `matom_` at `lispf42.c:4292`), each moved
number leaves a forwarding pointer `itop + a_1.nfreet` in its old slot. With the default
`CELLS=100000` this value is 100001…103000 — far outside `short` range. It is truncated
(and written at half the correct stride), so STEP 6 relocates every surviving float pointer
to garbage.

The bug only bites when compaction actually has to *move* numbers. If the live floats are
already packed at the top of the bignum area (the common case for short-lived numbers),
nothing moves and the collection looks clean — which is why this has stayed hidden.

**Reproduction.** Interleave live and dead floats so compaction must move them
(`fgc2.lsp`):

```lisp
(DE MKL2 (N)
  (PROG (I L)
        (SETQ I 0) (SETQ L NIL)
   LP   (QUOTIENT (ADD1 I) 9.0)                       ; dead float
        (SETQ L (CONS (QUOTIENT (ADD1 I) 8.0) L))     ; live float
        (SETQ I (ADD1 I))
        (COND ((LESSP I N) (GO LP)))
        (RETURN L)))
(DE BURN (N)
  (PROG (I X)
        (SETQ I 0)
   LP   (SETQ X (QUOTIENT (ADD1 I) 3.0))
        (SETQ I (ADD1 I))
        (COND ((LESSP I N) (GO LP)))
        (RETURN X)))
(SETQ BIG (MKL2 1200))
(LENGTH BIG) (CAR BIG) (CAR (CDR BIG))
(BURN 4000)                                            ; forces compacting bignum GC
(LENGTH BIG) (CAR BIG) (CAR (CDR BIG))
```

Observed with the shipped `./lispf4 basic.img`:

```
before GC:  1200    150.      149.875
--- Big number GBC. Bignum space = 1794
--- Big number GBC. Bignum space = 2394
after  GC:  1200    ...       ...          <-- floats destroyed, list structure intact
```

The list spine survives (LENGTH is still 1200) but every float element is now an invalid
pointer. This is silent data corruption: no error is reported.

**Fix.** As in "Root cause". After changing `jpname` to `integer *`, `lispf42.c:3699`
becomes `jpname[j - 1] = itop + a_1.nfreet;` and stores a full 32-bit forwarding pointer at
the correct stride, matching the read at `lispf42.c:3862`.

---

## B3 — CRITICAL: `(shortint)` cast makes EVAL/ATOM/RPLACA/RPLACD/PUT/SET misbehave

**Locations** — every translation of the `SPECAT` statement function:
- `lispf41.c:360` — `EVAL`, atom case
- `lispf41.c:1714` — `ATOM` / `LITATOM`
- `lispf41.c:1873` — `OBLIST`
- `lispf41.c:2567` — `RPLACA` (and `SET`, which jumps to it at `lispf41.c:2656/2663`)
- `lispf41.c:2591` — `RPLACD`
- `lispf41.c:2966` — `PUT`
- `lispf42.c:3402, 3602` — `garb_` STEP 1 and STEP 4
- `lispf42.c:4118` — `rehash_`

**Defect.** The test asks "is `CAR(atom)` one of the special markers ARRAY, STRING,
SUBSTR?" — in the running system these are atoms 137, 138, 139. The value is first
truncated to a signed 16-bit quantity, so **any** `CAR(atom)` congruent to 137…139 modulo
65536 is misclassified. With the default 100000 cells, cons indices 65673, 65674 and 65675
are ordinary, reachable cells that trigger it.

Effects, depending on the site: `EVAL` returns the *atom itself* instead of its value;
`ATOM` reports T for a list; `RPLACA`/`RPLACD`/`SET`/`PUT` reject a perfectly valid atom
with "Illegal argument"; `OBLIST` silently omits atoms; and in `garb_` a live cell can be
treated as an array, which is a memory-safety hazard.

**Reproduction — no debugger, shipped binary and image** (`trunc2.lsp`):

```lisp
(DE TST NIL
  (PROG (I R)
        (SETQ I 0) (SETQ R NIL)
   LP   (SET (QUOTE GV) (CONS 1 2))
        (SETQ I (ADD1 I))
        (COND ((ATOM GV) (SETQ R (CONS I R))))
        (COND ((LESSP I 40000) (GO LP)))
        (RETURN R)))
(TST)
```

```
$ ./lispf4 basic.img < trunc2.lsp
...
--- Illegal argument (subr2)
SET - GV
(SET BROKEN)
```

A loop that does nothing but repeatedly `SET` a global to a fresh cons fails outright once
the cons lands on cell 65673–65675: `SET` reaches `RPLACA` (`lispf41.c:2567`), the truncated
`CAR(GV)` reads as 137–139, and `SET` concludes `GV` is a string or array.

The `EVAL` variant was confirmed separately under gdb by setting `car[GV] = 65673` just
before `lispf41.c:360`: evaluating `GV` then returned the symbol `GV` rather than the cons.
A conditional breakpoint on the exact false-positive condition also fired unaided during
the loop above (`atom=GV car=65675 (short)=139`).

**Fix.** Delete the `s__1` narrowing at all nine sites and compare the full `integer`:

```c
    icar = carcdr_1.car[b_1.arg - 1];
    if (icar <= b_1.substr && icar >= b_1.array) { ... }
```

(The `shortint s__1;` declarations at `lispf41.c:110`, `lispf42.c:3283`, `lispf42.c:4098`
then become unused.)

---

## B4 — HIGH: `XCALL` does not range-check the logical unit number

**Locations**
- `lispf42.c:5246-5251` — `a1` is taken from Lisp and passed to `f4_open` at `lispf42.c:5306`
  with no bound check
- `lispf42.c:5319-5326` — same for `f4_close`
- `auxillary.c:33` — `static FILE *Logical_units[100];`
- `auxillary.c:41-47, 49-56` — `Logical_units[lun]` indexed unchecked

**Defect.** `b_1.maxlun` is set to 99 (`lispf42.c:1123`) and *is* enforced for `ROLLIN`
(`lispf41.c:1638`), `ROLLOUT` (`lispf41.c:1650`), `REWIND` (`lispf41.c:1666`) and `IOTAB`
(`lispf41.c:2772`) — but not for `XCALL`, which is the documented way to open files. Any
Lisp program can therefore index `Logical_units[]` arbitrarily far out of bounds, reading
and writing a `FILE*` outside the array.

**Reproduction**

```lisp
(XCALL 1 (LIST 100000000 "zz.txt" (QUOTE NEW) (QUOTE FORMATTED)))
```
→ segmentation fault (core dumped).

**Fix.** Validate in `xcall_` before both calls, e.g. `if (a1 < 1 || a1 > b_1.maxlun) goto L10000;`,
and additionally bound-check `lun` inside every `f4_*` entry point in `auxillary.c` as
defence in depth (`maxlun` is 99 and the array has 100 entries, so the array size is fine
once the index is checked).

---

## B5 — HIGH: NULL `FILE*` dereference on a never-opened logical unit

**Locations**
- `auxillary.c:114-119` — `f4_rewind`: `rewind(fp)` with no NULL check
- `auxillary.c:121-131` — `f4_write`: `putc(v[0], fp)` with no NULL check
- `auxillary.c:133-138` — `f4_write_lf`: same
- `auxillary.c:83-100` — `f4_read`: same
- `auxillary.c:102-112` — `f4_readu`: same

`f4_close` (`auxillary.c:49-56`) *does* check, so the omission is inconsistent rather than
deliberate.

**Reproduction**

```lisp
(REWIND 50)                 ; unit 50 never opened  -> segmentation fault
```
```lisp
(IOTAB 5 50) (PLUS 1 2)     ; redirect output to unit 50 -> segmentation fault
```

Both unit numbers are inside the `maxlun` limit, so the range checks that do exist pass;
the units simply have no open file.

**Fix.** Return a failure indication when `Logical_units[lun] == NULL` in each of these
routines, and have the callers surface it as a Lisp error rather than crashing. `IOTAB`
should additionally refuse to select a unit that is not open.

---

## B6 — HIGH: `mkcha_` ignores its buffer-length argument and overruns the caller's buffer

**Locations**
- `lispf42.c:5343` — `int mkcha_(integer *addr__, char *a, ftnlen a_len, int *len)`
- `lispf42.c:5395-5403` — the non-FORTRAN branch:
  ```c
  i__1 = iqqn - iqqr;            /* full print-name length */
  if (len) *len = i__1;
  p = ((char *) b_1.pname) + iqqr - 1;
  for (n = 0 ; n++ < i__1 ; )
          *a++ = *p++;           /* a_len never consulted */
  ```
- Callers `lispf42.c:5260, 5269, 5278` pass `c2`, `c3`, `c4` with `(ftnlen)50`; those buffers
  are declared at `lispf42.c:5227-5232` as `struct { integer fill; char val[50+1]; char fill2[1]; }`
- `lispf42.c:5293-5295` — `c2[len2] = '\0';` then writes at the *actual* length, past the end

The `#ifdef FORTRAN_LIB` branch respects the length (it uses `s_copy`/`s_cat` with
`(ftnlen)50`); only the hand-written C replacement dropped the bound.

**Defect.** The filename, status and format arguments of `XCALL 1` are Lisp atoms or
strings whose print names can be far longer than 50 bytes (atoms up to the 150-column read
margin; strings built with `CONCAT`/`PACK` are effectively unbounded). The excess is
written into the adjacent static buffers and beyond.

**Reproduction** (80-character filename atom, verified under gdb):

```
(XCALL 1 (LIST 10 (QUOTE FFFF...80 F's...) (QUOTE OLD) (QUOTE FORMATTED)))

Breakpoint, mkcha_ (a=0x419244 <c2_st+4>, a_len=50, ...) at lispf42.c:5401
mkcha_: copying 80 bytes into buffer with declared a_len=50
```

**Fix.** Clamp inside `mkcha_`:

```c
    i__1 = iqqn - iqqr;
    if (len) *len = i__1;
    if (i__1 > (int) a_len) i__1 = (int) a_len;   /* honour the declared length */
```

and in `xcall_` clamp the index used for the NUL terminator (`c2[len2]` etc.) to the buffer
size — or better, enlarge `c2`/`c3`/`c4` to a realistic path length and still clamp.

---

## B7 — MEDIUM: `priint_` writes past the end of `prbuff`

**Locations**
- `lispf42.c:2551` — `isi = b_1.prtpos + 19;`
- `lispf42.c:2555` — `b_1.prbuff[isi - 1] = chars_1.ifig[jj - 1];`
- `lispf42.c:2569` — the repositioning loop reads `b_1.prbuff[i__ - 1]` up to `b_1.prtpos + 19`
- `prbuff` is `integer prbuff[160]` (`lispf41.c:40`, `lispf42.c:57`)

**Defect.** `priint_` formats an integer into a 19-element scratch area starting at
`prtpos + 19` without checking that this stays inside `prbuff`. `b_1.marg` (the right
margin) can be raised to `b_1.iobuff` = 160 via `(IOTAB 8 160)` — the limit chosen at
`lispf41.c:2756` — after which `prtpos` can approach 160 and the scratch area runs off the
end into the adjacent `buff[]` field.

**Reproduction** (verified under gdb with a widened margin and a line of 7-digit numbers):

```
priint_ OOB: prtpos=145 isi=164 -> writes prbuff[163] but prbuff has 160 elems (marg=160)
```

At the default `marg` of 78 this cannot happen, so it requires the program to widen the
margin first.

**Fix.** Either cap the `IOTAB` limit for `marg`/`margr` at `iobuff - 20` instead of
`iobuff`, or make `priint_` bail out / call `terpri_()` when `prtpos + 19 > b_1.iobuff`.
Capping in `IOTAB` is the smaller change and also protects the `prinat_` paths.

---

## B8 — MEDIUM: image loading performs no EOF, size, or format validation

**Locations**
- `auxillary.c:102-112` — `f4_readu` calls `getc()` and stores the result without ever
  testing for `EOF`; at end-of-file it silently stores `0xFF` bytes and returns 0
- `lispf42.c:4684-4685` (`dmpin2_`) and `lispf42.c:4773-4774` (`dmpin_`) — discard
  `f4_readu`'s return value
- `lispf42.c:1427-1446` — `rollin_` compares only the first 8 configuration words; there is
  no magic number, version stamp, length, or checksum
- `lispf42.c:303-307` — `main` ignores the result of `f4_open` on the image file

**Defect.** A truncated, corrupt, or simply wrong file is loaded as though it were a valid
image, after which the interpreter runs on fabricated data.

**Reproduction**

```
$ head -c 20000 basic.img > trunc.img
$ ./lispf4 trunc.img
   ... starts up and prints mojibake instead of the system messages, no error
$ ./lispf4 basic1.lisp        # a text file, not an image
   Segmentation fault (core dumped)
```

**Fix.** Have `f4_readu` return a short-read/EOF indication, propagate it through
`dmpin_`/`dmpin2_`, and make `rollin_` fail cleanly (it already has the `L90` "rollin not
possible" path returning NIL). Checking `f4_open`'s return in `main` would also turn a
missing image file into a clear message. A small magic/version header would be a worthwhile
addition, though it changes the image format.

---

## B9 — MEDIUM: command-line memory sizes are not validated

**Locations**
- `lispf42.c:185-236` — `atoi()` results assigned straight to `a_1.nfreet`, `a_1.natom`,
  `a_1.nstack`, `a_1.npname` with no range check
- `lispf42.c:238-257` — the `calloc` *results* are checked, but zero/absurd *sizes* are not
- `lispf42.c:241` — `a_1.nhtab = (integer)(1.5 * (double) a_1.natom);`

**Defect.** Degenerate or negative sizes are accepted and produce a crash rather than a
diagnostic. (`calloc(0, 4)` legitimately returns a non-NULL pointer, so the existing NULL
check does not catch it.)

**Reproduction**

```
$ ./lispf4 -c 0  basic.img      -> segmentation fault
$ ./lispf4 -c 10 basic.img      -> segmentation fault
$ ./lispf4 -a 0  basic.img      -> segmentation fault
```

**Fix.** Enforce sane minimums after parsing, before allocating — the system needs at least
`natom + 1` cells plus working room, so something like: `natom >= 1000`, `nfreet >= natom + 5000`,
`nstack >= 500`, `npname >= 2000`, each also bounded above to avoid overflow in
`a_1.bignum = a_1.nfreet + a_1.natom` and in `1.5 * natom`. Print the usage message and exit
non-zero on violation. Also reject non-numeric arguments (`atoi` returns 0 silently).

---

## Lower-severity findings (by inspection)

### B10 — `mess_` mutates its argument and has no lower bound
`lispf42.c:4615-4626`:
```c
    if (*i__ > a_1.maxmes) { *i__ = 31; }
```
This writes through the caller's pointer. Callers pass addresses of file-scope
`static integer c__NN` "constants" (e.g. `mess_(&c__40)` at `lispf41.c:3885`, exactly at the
`maxmes = 40` boundary). Today nothing exceeds `maxmes`, so no constant is corrupted, but a
future change to `maxmes` or to a message number would permanently rewrite a shared
constant. There is also no `*i__ < 0` guard: a negative value yields negative indices in
`wra4_` (`lispf42.c:5093-5094`), which would read far outside `imess[400]`. The built-in
`SYSERROR` subr (`lispf41.c:3142-3144`) passes a Lisp-supplied number here, though in
`basic.img` it is shadowed by the Lisp-level `SYSERROR`, so I could not reach it from the
top level. Suggested fix: take the message number by value, and clamp to `1 .. maxmes`.

### B11 — character packing assumes little-endian
`auxillary.c:10-23` place the character in byte 0 of the 4-byte word and pad bytes 1-3 with
blanks; `getcht_` (`lispf42.c:4479`) recovers it with `*ic % 256`; `upcase_`
(`auxillary.c:25-31`) steps 4 bytes at a time touching byte 0. On a big-endian target the
character occupies the *high* byte and `% 256` returns the padding blank instead, so
character classification would fail wholesale. All current targets (x86-64 Linux/Windows,
Apple silicon and Intel macOS) are little-endian, so this is latent, but it contradicts the
"highly portable" claim in `Documentation/README.txt`.

### B12 — incompatible prototypes for `getch_`/`putch_` across translation units
`getch_` is *defined* as `int getch_(char *vec, char *ch, int *i)` (`auxillary.c:10`) but
*declared* as variadic `extern int getch_(void *, void *, int *, ...)` at `lispf41.c:131`
and called with four arguments at `lispf41.c:248`; `lispf42.c:784` declares it
`(real *, integer *, integer *)`. `putch_` is declared `(integer1 *, integer *, integer *)`
at `lispf42.c:2585` and `(real *, integer *, integer *)` at `lispf41.c:149`. Calling a
non-variadic function through a variadic declaration is undefined behaviour and can break
under a different ABI or under LTO/CFI. Suggested fix: put one correct prototype for each in
a shared header and include it everywhere.

### B13 — signal handler touches non-atomic globals
`lispf42.c:5409-5416`: `brserv_` assigns `b_1.errtyp` and `b_1.ibreak`, neither declared
`volatile sig_atomic_t`. The compiler is free to cache these in the main loop, so an
interrupt can be missed at `-O3`. `signal()` is also used rather than `sigaction()`; glibc
gives BSD semantics so the handler stays installed, but under System V semantics it would
revert to `SIG_DFL` after the first `SIGINT`.

### B14 — `toupper()` with a possibly-negative argument
`auxillary.c:29`: `buff[x] = toupper(buff[x]);` where `buff` is `char *`. On platforms where
`char` is signed, any byte above 127 is passed as a negative value, which is undefined for
`toupper`. Cast through `unsigned char`.

### B15 — `read_status` is global across all logical units
`auxillary.c:58`: one `static int read_status` serves every unit, so EOF state is shared
between, say, standard input and a file being `READFILE`d. It is reset by `f4_start_read()`
at the top of each line-read, which masks the problem in practice. Note also that
`f4_start_read()` is called from the *write* helpers (`lispf42.c:4871, 4891, 4948, 4966, 5109`),
which is at best misleading. Suggested fix: make the status per-unit.

### B16 — possible one-byte under-read in `rehash_` (unverified)
`lispf42.c:4122-4129`: with `l = pnp[n] - pnp[n-1] == 0`, the index `jb + l - 1` is `jb - 1`
and `getch_` reads `pname[jb - 2]`. Zero-length print names are created by `matom_(&c__0)`
(`lispf42.c:2960`; `lispf41.c:2505, 3121, 3159`). Such atoms are normally skipped by the string/substr/array
test three lines earlier — but that test is the defective one from B3, so the guard is not
reliable. I did not manage to trigger this; noting it as a defensive fix (skip when `l == 0`).

---

## Checked and found correct

To bound the report, these were examined and are *not* bugs:

- `abuff[160]` in `shift_` (`lispf42.c:3178-3179`) has no bound check, but `abup1` is bounded
  by the read margin, and `IOTAB` caps `margr` at `iobuff` (160) — exactly the array size.
  It is tight but correct. It only stays correct while `iobuff` matches `sizeof abuff`, so a
  bound check there would be cheap insurance.
- `mtime_`/`mdate_` (`auxillary.c:150-189`) `sprintf` 6 and 21 bytes into `b_1.buff`, which is
  `integer[160]` = 640 bytes. Safe.
- `PROMPTTEXT` (`lispf41.c:1884-1907`) correctly clamps `prolen` to 80 before filling
  `protxt[80]`.
- `IOTAB` index range (`lispf41.c:2744-2748`) is 1…10 and the `iotab` alias covers exactly
  ten fields (`lunin` … `levelp`). Correct.
- `equal_`'s use of `gtreal_` (`lispf42.c:659`) is consistent for both the small-integer and
  float paths.
- Loading an image with a *larger* `-c` than it was built with works: `move_` relocates
  pointers correctly (verified with `-c200000` against `basic.img`).
- `NCHARS`/`PACK` buffer swapping between `prbuff` and `buff` (`lispf42.c:4523-4529`,
  `lispf41.c:2420-2427`) stays within `marg <= 160`.

---

## Suggested fix order

1. **B3** — one-line-per-site removal of the `(shortint)` casts. Smallest change, fixes
   silent wrong answers in the evaluator core. No image rebuild needed.
2. **B1 + B2** — retype `jpname` to `integer *` in the three `shortint` definitions and drop
   the casts on stores. Fixes arrays and float GC together. `arrutl_`'s index arithmetic
   should be re-checked against `jbytes = 4` after the retype, and array behaviour retested.
3. **B4, B5, B6** — input validation on logical unit numbers and the `mkcha_` length. These
   are straightforward and remove three reproducible crashes reachable from ordinary Lisp.
4. **B8, B9** — validation at startup and image load.
5. **B7, B10–B16** — hardening.

Because B1 and B2 change how data is laid out in `PNAME`, existing `.img` files should be
regenerated after fixing them:

```
make realclean && make
```

A regression test worth adding (all currently fail): the `SETA`/`ELT` round-trip from B1, the
float-GC survival check from B2, and the `SET`-in-a-loop check from B3.

---

## Resolution (2026-08-03)

All sixteen findings are fixed. Phases refer to `Plan1.md`.

| # | Fixed in | Change |
|---|---|---|
| B1 | Phase 2 | `jpname` retyped `shortint *` → `integer *` in `lispf41.c`, `arrutl_` and `garb_`; six `(shortint)` casts dropped |
| B2 | Phase 2 | same retype; the GC forwarding pointer at `lispf42.c` STEP 5 is now a full 32-bit store at the correct stride |
| B3 | Phase 1 | three `shortint s__1` declarations retyped to `integer`, nine narrowing casts removed |
| B4 | Phase 3 | `xcall_` validates the unit against `b_1.maxlun` on both open and close |
| B5 | Phase 3 | all `f4_*` routines go through `f4_fp()`, which range-checks and returns NULL; `rew_` propagates status so `REWIND` errors; `IOTAB` refuses a unit that is not open |
| B6 | Phase 3 | `mkcha_` honours `a_len` and reports the stored length; `XCALL` buffers enlarged to `XCALL_NAMELEN` (255) |
| B7 | Phase 5 | `IOTAB` caps `LMARG`/`MARG` at `iobuff-20`; `priint_` additionally calls `terpri_` if its scratch area would not fit |
| B8 | Phase 4 | `f4_readu` uses `fread` and reports short reads; `dmpin_`/`dmpin2_` propagate; `rollin_` fails cleanly; `main` checks both the open and the load |
| B9 | Phase 4 | `parse_size()` rejects junk/zero/negative/huge; a configuration consistency check runs before `calloc` |
| B10 | Phase 5 | `mess_` clamps a local copy instead of writing through the caller's pointer, and rejects values below 1 |
| B11 | Phase 6 + follow-up | `getch_`/`putch_`/`upcase_` build the packed character arithmetically. The Phase 6 change alone was **incomplete**: `f4_read`/`f4_write` still packed A1 characters by direct byte stores. New `f4_read_char`/`f4_write_char` do it arithmetically and every A1 call site now uses them (see "Follow-up" below) |
| B12 | Phase 6 | new `lispf4.h` holds the single set of prototypes; the conflicting in-body `extern`s were deleted |
| B13 | Phase 6 | the handler now sets only `volatile sig_atomic_t f4_break_pending` and re-arms; polled in `EVAL` and in `prin1_` so long prints stay interruptible |
| B14 | Phase 6 | `toupper` is called through `(unsigned char)` |
| B15 | Phase 6 | `read_status` is per unit; `f4_start_read(lun)` no longer called from the write paths |
| B16 | Phase 5 | `rehash_` clamps a zero-length print name before deriving byte offsets |

### B17 — a file whose last line has no trailing newline loses that line (FIXED)

Found while verifying Phase 6; **pre-existing**, not a regression — the shipped
`Linux/lispf4` binary behaves identically.

`rda1_` (`lispf42.c`) discarded the partially-read line when `f4_read` reported EOF: it
jumped straight to its `L1` exit with `ieof = 2`, and `shift_` then treated the whole line
as absent. This matters because `READFILE` (`basic1.lisp`) reads forms until it sees the
atom `STOP` — which is why every package file ends with a `STOP` line. Lose that line and
`READFILE` never terminates; it carries on reading from standard input and silently
swallows the rest of the session.

```
$ printf '(SETQ AAA 111)\n(SETQ BBB 222)\nSTOP' > nonl.lisp    # no trailing newline
$ printf '(READFILE "nonl.lisp")\n(LIST AAA BBB)\n(EXIT)\n' | ./lispf4 basic.img
   before: no output at all -- READFILE ate the remaining input
   after:  (111 222)
```

**Fix.** The difficulty is telling "the file ended mid-line" from "the file ended exactly at
a line boundary": the old `read1` returned a blank in both cases, so `rda1_` could not tell
a real partial line from the phantom blank produced by the end-of-file probe.

- `auxillary.c` — `read1` now returns `EOF` rather than a blank when end-of-file is reached
  without a character, and `f4_read` returns non-zero only when it stored nothing.
- `lispf42.c` — `rda1_` checks whether the failing position is past `i1`. If it is, the last
  line had no newline: blank-fill the rest of the buffer with `chars_1.space`, hand the line
  over, and report the end-of-file on the *next* call. If nothing was read, report EOF as
  before.

Covered by `tests/cases/b17-nonewline.sh`, which also checks the trailing-newline control
case, standard input without a final newline, and that EOF on standard input is still
reported exactly once. Verified to fail on the pre-fix binary and pass after.

---

## Follow-up items (fixed 2026-08-03)

Three items that surfaced during and after the phased work.

### B11 was only half fixed

Phase 6 made `getch_`/`putch_`/`upcase_` byte-order independent, but the I/O layer still
packed single characters by storing into byte 0:

```c
    v[0] = c;  v[1] = v[2] = v[3] = ' ';     /* f4_read, A1 case */
    putc(v[0], fp);                          /* f4_write, A1 case */
```

On a big-endian host that puts the character in the *high* byte, where `getcht_`'s
`ic % 256` would find a blank instead. So the interpreter was endian-clean while the I/O
underneath it was not — the two halves disagreed.

**Fix.** Two new entry points, `f4_read_char` / `f4_write_char`, assemble and take apart the
word arithmetically (`c | ' '<<8 | ' '<<16 | ' '<<24`). All six A1 call sites now use them:
the read loop and both prompt-echo paths in `rda1_`, both write loops in `wra1_`, and
`eject_`. `f4_read`/`f4_write` keep their byte-for-byte behaviour and are now documented as
A4/binary-only. Verified under gdb that `chars_1.space`, `chars_1.lpar` and `chars_1.ifig[0]`
are `0x20202020`, `0x20202028` and `0x20202030` — the character in the low byte, exactly
what `getch_` builds and `getcht_` recovers.

Still true, and inherent: image files hold raw binary words, so they remain non-portable
between machines of different byte order. That was already the case for every pointer value
stored in them.

### Strict aliasing was left enabled at -O3

The F2C output emulates FORTRAN `EQUIVALENCE` by casting between pointer types —
`#define n ((integer *)equiv_2)` over a `real` array, the `((integer *)&b_1.arg)` aliases of
common block `/B/`, and `jpname`/`ipname` over `b_1.pname`. That is exactly what type-based
alias analysis assumes cannot happen, and `-O3` enables `-fstrict-aliasing`. gcc reported 44
such sites.

**Fix.** `Makefile` now passes `-fno-strict-aliasing` (as `$(ALIAS)`, applied to both the
normal and debug builds). MSVC does not make type-based aliasing assumptions, so
`Makefile.win` needs no equivalent.

### A failed (ROLLIN N) from Lisp continued on a corrupt heap

`main` was fixed in Phase 4 to exit when `rollin_` returns NIL, but the Lisp-callable
`(ROLLIN N)` just returned NIL and carried on — even when the failure happened partway
through the bulk transfer, with the atoms, cells and print names already partly overwritten.

**Fix.** The two failure modes are now distinguished, which is meaningful because *every*
`goto L90` in `rollin_` occurs before the first assignment to global state (verified: the
jumps are at the header read, the eight configuration words, and the three capacity checks;
the first mutation, `a_1.natomp = *natopo`, comes after all of them).

- Rejected before any damage → returns NIL as before. `(ROLLIN N)` may safely carry on, and
  a following `(PLUS 40 2)` still evaluates to 42.
- Short read during the transfer → there is nothing consistent to return to, so it reports
  `Image file is truncated or unreadable; the interpreter state is now incomplete. Stopping.`
  and exits 1, for both `main` and a Lisp-level call.

Covered by `tests/cases/rollin-reject.sh`, which checks both paths including that evaluation
does *not* continue after the damaged case.
