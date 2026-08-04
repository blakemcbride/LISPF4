# LISPF4 — Remediation Plan for Bugs1.md (Plan1)

Plan date: 2026-08-03
Companion document: `Bugs1.md`

**Ground rules**

- All changes are in C only: `lispf41.c`, `lispf42.c`, `auxillary.c`, `f2c.h`, plus one new
  header. The FORTRAN sources (`Lispf41.f`, `Lispf42.f`, `F4COM.FOR`, `lispf4.orig`) are
  reference-only and are **not** to be modified, and F2C is **not** to be re-run.
- Work proceeds in six phases. Each phase is independently buildable and testable; do not
  start a phase until the previous one builds clean and the regression suite passes.
- Phase 0 (test harness) comes first so that every later phase has a pass/fail signal.

**Phase overview**

| Phase | Bugs | Risk | Files touched |
|---|---|---|---|
| 0 | — (regression harness) | none | new `tests/` |
| 1 | B3 | low | `lispf41.c`, `lispf42.c` |
| 2 | B1, B2 | **high** | `lispf41.c`, `lispf42.c` |
| 3 | B4, B5, B6 | medium | `auxillary.c`, `lispf42.c`, `lispf41.c` |
| 4 | B8, B9 | low | `lispf42.c`, `auxillary.c` |
| 5 | B7, B10, B16 | low | `lispf41.c`, `lispf42.c` |
| 6 | B11, B12, B13, B14, B15 | medium | all, plus new header |

---

# Phase 0 — Regression harness

Nothing in the repo currently exercises the interpreter automatically. Build this first;
every bug below gets a test that fails now and passes after its fix.

### 0.1 Create `tests/`

```
tests/
  run-tests.sh          driver
  cases/b1-arrays.lsp   ... one .lsp per case
  cases/b1-arrays.exp   expected significant output
  ...
```

### 0.2 Driver

`tests/run-tests.sh` should, for each `cases/NAME.lsp`:

1. run `../lispf4 ../basic.img < cases/NAME.lsp`, capturing stdout+stderr and the exit status;
2. normalise output — strip the banner (`Lisp F4 , latest update`, the `Space (cells …)` line,
   the `--- Reset` line, the trailing `Exit from Lisp F4` / `GBC:s` / `Bye` block), strip the
   `_` and `:` prompt characters, and collapse runs of blanks — because the banner carries a
   build date and the free-cell counts vary;
3. `diff` against `cases/NAME.exp`;
4. report PASS/FAIL, and treat a non-zero exit or a signal (core dump) as FAIL.

Keep it plain `sh` + `diff`; the project has no test framework and does not need one.

### 0.3 Initial cases (all must FAIL before their fix, PASS after)

| Case | Content | Expected after fix |
|---|---|---|
| `b1-arrays` | the `SETA`/`ELT`/`SETI`/`ELTI`/`SETR`/`ELTR` sequence from Bugs1 §B1 | `FOO`, `(X Y)`, `99`, `12345`, `3.25` |
| `b2-floatgc` | `MKL2`/`BURN` from Bugs1 §B2 | `(CAR BIG)` = `150.` and `(CAR (CDR BIG))` = `149.875` both before **and** after the GC |
| `b3-settrunc` | the `TST` loop from Bugs1 §B3 | returns `NIL`, no `Illegal argument` |
| `b3-eval` | as `b3-settrunc` but checking `(ATOM GV)` stays `NIL` | `NIL` |
| `b4-xcall-lun` | `(XCALL 1 (LIST 100000000 "zz.txt" (QUOTE NEW) (QUOTE FORMATTED)))` | returns `NIL`, exit 0, no crash |
| `b5-rewind` | `(REWIND 50)` | returns `NIL` (or errors), no crash |
| `b5-iotab-out` | `(IOTAB 5 50)` then `(PLUS 1 2)` | rejected as illegal argument, no crash |
| `b6-longname` | `XCALL 1` with an 80-character filename atom | returns `NIL`, no crash, no corruption |
| `b7-margin` | `(IOTAB 8 160)` then print a line of 7-digit numbers | either rejected or printed without OOB |
| `sanity-arith` | `(PLUS 2 3)`, `(TIMES 6 7)`, `(QUOTIENT 22 7.0)`, `(FACTORIAL 10)` | unchanged values — guards against regressions |
| `sanity-editor` | `(GETD (QUOTE EDITS))`, `(GETD (QUOTE DE))` | non-`NIL` |
| `sanity-image` | `(SYSOUT "t.img")` in one run, `(SYSIN "t.img")` + a check in the next | round-trips |

Shell-level cases (driver runs these directly, not through a `.lsp`):

| Case | Command | Expected after fix |
|---|---|---|
| `b8-truncimg` | `head -c 20000 basic.img > t.img; ./lispf4 t.img` | clean error message, non-zero exit, no crash |
| `b8-textimg` | `./lispf4 basic1.lisp` | clean error message, non-zero exit, no crash |
| `b9-args` | `./lispf4 -c 0 basic.img`, `-c 10`, `-a 0`, `-c abc` | usage/error message, non-zero exit, no crash |
| `b9-bigger` | `./lispf4 -c200000 basic.img` | still works (must not regress) |

### 0.4 Debug build target

Add to `Makefile` (leave the default build untouched):

```make
# Debug / diagnostic build.  Sanitizers are used when available.
debug:
	gcc -g -O0 -fsanitize=address,undefined -fno-omit-frame-pointer \
	    -Dstricmp=strcasecmp $(PARMS) \
	    -DYEAR=$(LAST_UPDATE_YEAR) -DMONTH=$(LAST_UPDATE_MONTH) -DDAY=$(LAST_UPDATE_DAY) \
	    -o lispf4dbg lispf41.c lispf42.c auxillary.c -lm
```

Note: the ASan/UBSan runtimes were **not installed** on the machine used for the Bugs1
analysis (`libasan.so.8`, `libubsan.so.1` missing), so this target will not link until
`libasan`/`libubsan` are installed (`dnf install libasan libubsan` on Fedora). If they
cannot be installed, drop `-fsanitize=...` and rely on gdb plus the regression suite — that
is how every finding in Bugs1 was confirmed.

---

# Phase 1 — B3: remove the `(shortint)` narrowing in the SPECAT test

**Why first:** smallest change, no data-layout impact, no image rebuild needed, and it fixes
silent wrong answers in the evaluator core.

### 1.1 Change three declarations

| File:line | From | To |
|---|---|---|
| `lispf41.c:110` | `shortint s__1;` | `integer s__1;` |
| `lispf42.c:3283` | `shortint s__1;` | `integer s__1;` |
| `lispf42.c:4098` | `shortint s__1;` | `integer s__1;` |

Verified: in all three functions `s__1` is used *only* for this test, so retyping is safe.

### 1.2 Remove the nine casts

Drop `(shortint)` from the right-hand side at:

```
lispf41.c:360    s__1 = (shortint) carcdr_1.car[b_1.arg - 1];      (EVAL)
lispf41.c:1714                                                     (ATOM / LITATOM)
lispf41.c:1873   s__1 = (shortint) carcdr_1.car[i__ - 1];          (OBLIST)
lispf41.c:2567                                                     (RPLACA, and SET via it)
lispf41.c:2591                                                     (RPLACD)
lispf41.c:2966   s__1 = (shortint) carcdr_1.car[b_1.temp1 - 1];    (PUT)
lispf42.c:3402   s__1 = (shortint) carcdr_1.car[i__ - 1];          (garb_ STEP 1)
lispf42.c:3602   s__1 = (shortint) carcdr_1.car[n - 1];            (garb_ STEP 4)
lispf42.c:4118   s__1 = (shortint) carcdr_1.car[n - 1];            (rehash_)
```

Leave the comparison lines untouched — they already read correctly once `s__1` is an
`integer`.

### 1.3 Verify

- `b3-settrunc` and `b3-eval` pass.
- `sanity-*` unchanged.
- Behaviour change to be aware of: `rehash_` and `garb_` will now correctly classify atoms
  that were previously misread as strings. Hash tables built on `ROLLIN` may differ from
  before — that is the correction, not a regression. Confirm `sanity-image` still round-trips.

---

# Phase 2 — B1 + B2: retype `jpname` to `integer *`

**These must be done together.** They are one defect with two symptoms (arrays; float GC).
This is the highest-risk phase.

### 2.1 Background — why `integer *` is the correct width

`INIT1` sets `JBYTES = 4`, and the space actually reserved per pointer element is 4 bytes:
`arrutl_` L4100 advances `a_1.jbp` by `(ilen + 2) * jbytes`. The index arithmetic is likewise
in 4-byte words:

```
lword1 = (lbyte1 - 2)/jbytes + 4     -> element 1 of the pointer part
lword2 = (lbyte2 - 2)/ibytes + 2     -> element 1 of the integer part  (ipname, integer *)
lword3 = (lbyte3 - 2)/bytes  + 2     -> element 1 of the real part     (pname,  real *)
llen1  = (lbyte2 - lbyte1)/jbytes - 2
```

Worked example, array starting at byte 5 with 5 pointers / 5 ints / 5 reals:

```
w0 = (5+2)/4 = 1
pointer section = words 2..8  (bytes 5..32);  header words 2,3;  elements words 4..8
lword1 = (5-2)/4 + 4 = 4      -> jpname[3] = word 4 = element 1        correct
jpname[lword1-3] = word 2, jpname[lword1-2] = word 3                    the two header words
lbyte2 = 33, lword2 = (33-2)/4 + 2 = 9 -> word 9 = bytes 33..36         no overlap
```

So with `jpname` as `integer *` every index lands exactly where the allocator reserved
space. With `shortint *` the same indices address half-stride locations *and* truncate the
stored value — which is the bug. Also note `inreal = a_1.bytes / a_1.jbytes` = 1, which is
only meaningful if `jpname` elements are the same width as `pname` elements, i.e. 4 bytes.

### 2.2 Retype the three `shortint` definitions

| File:line | From | To |
|---|---|---|
| `lispf41.c:157` | `#define jpname ((shortint *) b_1.pname)` | `#define jpname ((integer *) b_1.pname)` |
| `lispf42.c:862` | same (in `arrutl_`) | same |
| `lispf42.c:3296` | same (in `garb_`) | same |

`lispf42.c:1412` (`rollin_`) and `lispf42.c:1574` (`rollou_`) are already `(integer *)` and
must stay as they are. After this change all five definitions agree.

### 2.3 Remove the `(shortint)` casts on stores through `jpname`

| File:line | From | To |
|---|---|---|
| `lispf41.c:3040` | `jpname[jndex - 1] = (shortint) b_1.arg3;` | `jpname[jndex - 1] = b_1.arg3;` |
| `lispf42.c:1007` | `jpname[*lword1 - 1] = (shortint) b_1.nil;` | `jpname[*lword1 - 1] = b_1.nil;` |
| `lispf42.c:1056` | `jpname[*lword1 - 3] = (shortint) a_1.jbp;` | `jpname[*lword1 - 3] = a_1.jbp;` |
| `lispf42.c:1058` | `jpname[*lword1 - 2] = (shortint) a_1.jbp;` | `jpname[*lword1 - 2] = a_1.jbp;` |
| `lispf42.c:3699` | `jpname[j - 1] = (shortint) (itop + a_1.nfreet);` | `jpname[j - 1] = itop + a_1.nfreet;` |
| `lispf42.c:3820` | `jpname[ind1 - 1] = (shortint) s;` | `jpname[ind1 - 1] = s;` |

`lispf42.c:1056/1058` additionally stop truncating `a_1.jbp`, which previously overflowed a
`short` for any `-p` above roughly 8000 — an independent latent bug removed by the same edit.

Reads (`lispf41.c:2095` ELT, `lispf42.c:962, 923, 929, 1003, 3434, 3816, 3862`) need no
change; they become correct automatically.

### 2.4 Fix the byte-1 off-by-one in the word-index expressions

`(lbyte1 - 2) / jbytes` relies on truncation of a *negative* numerator when `lbyte1 == 1`,
giving `lword1 = 4` where 3 is correct — placing every element one word high and running one
word into the integer section. Reachable only if an array is the very first object in
`PNAME`, which does not happen in practice, but fix it while the code is open. Rewrite in the
same rounding form already used at L4100:

| File:line | From | To |
|---|---|---|
| `lispf42.c:919` | `*lword1 = (lbyte1 - 2) / a_1.jbytes + 4;` | `*lword1 = (lbyte1 + a_1.jbytes - 2) / a_1.jbytes + 3;` |
| `lispf42.c:928` | `*lword2 = (lbyte2 - 2) / a_1.ibytes + 2;` | `*lword2 = (lbyte2 + a_1.ibytes - 2) / a_1.ibytes + 1;` |
| `lispf42.c:934` | `*lword3 = (lbyte3 - 2) / a_1.bytes + 2;` | `*lword3 = (lbyte3 + a_1.bytes - 2) / a_1.bytes + 1;` |

These are identical to the originals for every start byte ≥ 2 and correct at 1.

### 2.5 Image compatibility

- The **on-disk image format does not change.** `rollin_`/`rollou_` already treat the print-name
  area as 4-byte words, and the number of bytes reserved per array element was always 4.
- Image *contents* do change in one respect: any array stored in an existing image has its
  pointer section written by the old, broken code. Those contents were garbage before, so
  nothing of value is lost — but images should be regenerated:
  ```
  make realclean && make
  ```
- The shipped `basic.img` is built by `script.2`, which loads only the standard `.lisp`
  packages and does not create arrays, so in practice the regenerated image will be
  equivalent. Regenerate anyway.

### 2.6 Verify

- `b1-arrays` passes: `FOO`, `(X Y)`, `99`, `12345`, `3.25`.
- `b2-floatgc` passes: `150.` / `149.875` both before and after the compacting bignum GC.
- Add a wider array test: pointer slots holding atoms, conses, small integers, floats,
  strings and `NIL`, written and read back at indices 1, middle and last; plus
  `(ARRAYSIZE A)` still `(15 5 5)`.
- Add a GC-through-array test: store conses in an array, force a cell GC with `(RECLAIM 1)`,
  and confirm the elements survive — this exercises `garb_` L3434/L3816/L3820.
- Run `b9-bigger` (`-c200000`) and also `-p 20000` to confirm the `a_1.jbp` truncation fix.
- Rebuild images and re-run everything against the regenerated `basic.img`.

---

# Phase 3 — B4, B5, B6: I/O input validation

Removes three reproducible crashes reachable from ordinary Lisp.

### 3.1 B4 — range-check the logical unit in `XCALL`

`b_1.maxlun` (99, set at `lispf42.c:1123`) is enforced for `ROLLIN` (`lispf41.c:1638`),
`ROLLOUT` (1650), `REWIND` (1666) and `IOTAB` (2772) but not for `XCALL`.

In `xcall_`, open path — after `a1 = getnum_(&a1);` (`lispf42.c:5251`):

```c
    if (a1 < 1 || a1 > b_1.maxlun) goto L10000;
```

In `xcall_`, close path — after `a1 = getnum_(x);` (`lispf42.c:5319`):

```c
    if (a1 < 1 || a1 > b_1.maxlun) goto L10000;
```

`L10000` already returns `NIL`, which is the documented failure result for `XCALL`.

Optional hardening: also reject units 5 and 6, which are `stdin`/`stdout`
(`auxillary.c:35-39`); `(XCALL 2 5)` currently `fclose`s standard input.

### 3.2 B5 — no NULL `FILE*` dereferences

In `auxillary.c`, add a single accessor and use it everywhere:

```c
#define F4_MAXLUN  ((int)(sizeof Logical_units / sizeof Logical_units[0]))

static FILE *f4_fp(int lun)
{
        if (lun < 0 || lun >= F4_MAXLUN)
                return NULL;
        return Logical_units[lun];
}
```

Then:

| Function | File:line | Change |
|---|---|---|
| `f4_open` | 41-47 | return 1 if `lun` out of range, before touching the array |
| `f4_close` | 49-56 | use `f4_fp` (already NULL-safe) |
| `f4_read` | 83-100 | `fp = f4_fp(lun); if (!fp) return 1;` |
| `f4_readu` | 102-112 | `fp = f4_fp(lun); if (!fp) return 1;` |
| `f4_rewind` | 114-119 | `fp = f4_fp(lun); if (!fp) return 1;` |
| `f4_write` | 121-131 | `fp = f4_fp(lun); if (!fp) return 1;` |
| `f4_write_lf` | 133-138 | same |

`f4_read` already has a "return 1" convention that `rda1_` (`lispf42.c:5000-5002`) turns into
`ieof = 2`, so a closed input unit degrades to end-of-file rather than crashing.

Propagate the rewind status so `REWIND` reports the error instead of silently doing nothing:

- `rew_` (`lispf42.c:5174-5190`): return `f4_rewind(*lun)` rather than 0.
- `REWIND` (`lispf41.c:1665-1670`): `if (rew_(n) != 0) goto L25000;` — the existing
  "Illegal argument (subr1)" path.

Prevent the `IOTAB` output crash at source. `IOTAB` selects the input unit for `n1 == 1`
and the output unit for `n1 == 5` (`iotab[0]` = `lunin`, `iotab[4]` = `lunut`). Add an
"is it open" query in `auxillary.c`:

```c
int f4_isopen(int lun)          { return f4_fp(lun) != NULL; }
```

and in `IOTAB`, after `n2` passes its range check (`lispf41.c:2781-2783`) and before
`iotab[n1 - 1] = n2;`:

```c
    if ((n1 == 1 || n1 == 5) && !f4_isopen(n2)) goto L25010;
```

### 3.3 B6 — `mkcha_` must honour its length argument

Two changes.

**(a)** Give the buffers a realistic size. At `lispf42.c:5227-5232` replace the three
`char val[50+1]` declarations with a named size:

```c
#define XCALL_NAMELEN 255
    static struct { integer fill; char val[XCALL_NAMELEN+1]; char fill2[3]; } c2_st;
```

and update the three `mkcha_` calls (`lispf42.c:5260, 5269, 5278`) to pass
`(ftnlen)XCALL_NAMELEN` instead of `(ftnlen)50`, plus `o__1.ofnmlen` in the (dead)
`FORTRAN_LIB` branch at `lispf42.c:5282`.

**(b)** Clamp inside `mkcha_`, in the non-`FORTRAN_LIB` branch at `lispf42.c:5395-5403`:

```c
    i__1 = iqqn - iqqr;
    if (i__1 < 0)          i__1 = 0;
    if (i__1 > (int)a_len) i__1 = (int)a_len;   /* honour the declared length */
    if (len) *len = i__1;                       /* report the STORED length */
    p = ((char *) b_1.pname) + iqqr - 1;
    for (n = 0 ; n++ < i__1 ; )
            *a++ = *p++;
```

Setting `*len` to the clamped length is what makes `xcall_`'s
`c2[len2] = '\0'` (`lispf42.c:5293-5295`) safe — that is the second out-of-bounds write and
it must not be overlooked. The `FORTRAN_LIB` branch already respects `a_len` via
`s_copy`/`s_cat` and needs no change.

### 3.4 Verify

`b4-xcall-lun`, `b5-rewind`, `b5-iotab-out`, `b6-longname` all pass with no crash. Add a
positive test that normal file I/O still works end-to-end: `XCALL 1` open on unit 10,
`IOTAB` redirect, `READFILE`, `XCALL 2` close — the `script.2` build path depends on exactly
this, so if it breaks the image build breaks.

---

# Phase 4 — B8, B9: image and startup validation

### 4.1 B8 — detect short reads and failed image loads

**(a)** `f4_readu` (`auxillary.c:102-112`) currently stores `getc()`'s result without testing
for `EOF`. Rewrite:

```c
int f4_readu(int lun, char *v, int n)
{
        FILE *fp = f4_fp(lun);
        size_t want = (n == 4) ? 4 : 1;
        if (!fp) return 1;
        if (fread(v, 1, want, fp) != want) return 1;
        return 0;
}
```

(Preserves the existing "4 bytes if `n == 4`, otherwise 1" behaviour.)

**(b)** Propagate. `dmpin_` (`lispf42.c:4734-4777`) and `dmpin2_` (`4642-4688`) ignore the
return value; make each accumulate failures and return non-zero.

**(c)** `rollin_` (`lispf42.c:1393`) — check each `dmpin_`/`dmpin2_` result and `goto L90` on
failure. `L90` already returns `b_1.nil` and rewinds, so the failure path exists.

**(d)** `main` (`lispf42.c:301-309`) — the two remaining gaps:

```c
    if (f4_open(10, argv[1], "rb") != 0) {
            fprintf(stderr, "Cannot open image file '%s'\n", argv[1]);
            exit(1);
    }
    ixcc = rollin_(&c__10);
    f4_close(10);
    if (ixcc == b_1.nil) {
            fprintf(stderr, "'%s' is not a valid Lisp F4 image, or does not fit "
                            "the current memory configuration\n", argv[1]);
            exit(1);
    }
```

Note the `#ifdef unix` branch at `lispf42.c:302-306` opens the image with mode `"r"` rather
than `"rb"`; on POSIX these are identical, but use `"rb"` unconditionally for clarity.

This `ixcc` check alone fixes the `./lispf4 basic1.lisp` segfault: `rollin_` *already*
rejects the file (its 8-word configuration header will not match) and returns `NIL` — `main`
simply ignored that and ran the interpreter on an uninitialised system. The `f4_readu`
change is what catches the *truncated* image, whose header does match.

**Not doing now:** adding a magic number / version stamp to the image format. It would be a
genuine improvement but breaks every existing `.img`. Revisit separately if desired.

### 4.2 B9 — validate command-line sizes

In `main` (`lispf42.c:185-236`), replace `atoi` with a checked parse:

```c
static int parse_size(const char *s, integer *out)
{
        char *end;
        long v = strtol(s, &end, 10);
        if (end == s || *end != '\0' || v <= 0 || v > 200000000L) return 0;
        *out = (integer) v;
        return 1;
}
```

and fail with the usage message when a parse fails (this also catches `-c abc`, currently
silently taken as 0). Note the existing `else if (argc > 2)` arms silently keep the default
when a flag ends the argument list — make that an error too.

After parsing and before the `calloc` block (`lispf42.c:238`), add a consistency check:

```c
    if (a_1.natom  < 100                  ||
        a_1.nstack < 100                  ||
        a_1.npname < a_1.natom + 100      ||
        a_1.nfreet < a_1.natom + 1000     ||
        (double) a_1.nfreet + a_1.natom > 1.0e9) {     /* bignum/ismall headroom */
            fprintf(stderr, "Invalid memory configuration.\n");
            usage(argv[0]);
            exit(1);
    }
```

Rationale for the last term: `init1_` computes `a_1.bignum = nfreet + natom` and
`a_1.ismall = (maxint - bignum - 1)/2` (`lispf42.c:1154-1155`); keeping the sum well below
`INT_MAX` avoids overflow there and in `a_1.nhtab = 1.5 * natom`. The lower bounds only
exclude degenerate configurations — `rollin_` already rejects a configuration too small for
a particular image (`lispf42.c:1436-1446`), and that check must keep working.

### 4.3 Verify

`b8-truncimg`, `b8-textimg`, `b9-args` produce clean messages and non-zero exit with no
crash; `b9-bigger` still works. Confirm `make realclean && make` still builds
both images — the bootstrap runs `./lispf4 -x <script.1`, which takes the `istart == 0` path
and must not be affected by the new image checks.

---

# Phase 5 — B7, B10, B16: bounds and robustness

### 5.1 B7 — keep `priint_` inside `prbuff`

Two complementary changes; do both.

**(a)** Cap the print margins in `IOTAB`. `priint_` needs `prtpos + 19 <= iobuff`, and
`prtpos` is bounded by `marg`. At `lispf41.c:2756` the limit for every non-unit entry is
`b_1.iobuff`; tighten it for the two print-margin entries — `n1 == 7` (`lmarg`) and
`n1 == 8` (`marg`):

```c
    *n = b_1.iobuff;
    if (n1 == 7 || n1 == 8) *n = b_1.iobuff - 20;   /* leave room for priint_'s 19 digits */
```

This does not disturb the defaults (`marg` is 78). Leave `margr`/`lmargr` (`n1 == 3, 4`)
capped at `iobuff` — they index `rdbuff[160]` and `abuff[160]`, not `prbuff`, and the shipped
default `margr` is 150.

**(b)** Add a hard guard in `priint_` itself so no caller can overrun it regardless of how
the margins were set — before `isi = b_1.prtpos + 19;` (`lispf42.c:2551`):

```c
    if (b_1.prtpos + 19 > b_1.iobuff) terpri_();
```

`terpri_` resets `prtpos` to the left margin, which is within bounds.

### 5.2 B10 — `mess_` must not write through its argument

`lispf42.c:4603-4626`. Work on a local copy and add the missing lower bound:

```c
/* Subroutine */ int mess_(integer *i__)
{
    integer m = *i__;
    ...
    if (m == 0) goto L10;                    /* 0 = read messages from LUNSYS */
    if (m < 1 || m > a_1.maxmes) m = 31;
    nw = a_1.nbmess / a_1.ibytes;
    i2 = nw * m;
    ...
```

No call site changes. This removes the write-back that could permanently corrupt a
file-scope `static integer c__NN` constant (`mess_(&c__40)` at `lispf41.c:3885` sits exactly
on the `maxmes = 40` boundary), and stops a negative message number producing negative
indices in `wra4_` (`lispf42.c:5093-5094`).

### 5.3 B16 — guard the zero-length print name in `rehash_`

`lispf42.c:4122-4129`. Before computing the two derived byte offsets:

```c
    l = b_1.pnp[n] - b_1.pnp[n - 1];
    if (l < 1) l = 1;          /* zero-length pname: don't index before the string */
```

This keeps `jb + l/2` and `jb + l - 1` at or after `jb`. Note this is a defensive fix — the
condition was never triggered in testing, because such atoms are normally skipped by the
string/substr/array test three lines earlier (which Phase 1 has by then made reliable).

### 5.4 Verify

`b7-margin` passes. Re-run the full suite; these changes should be behaviour-neutral for
everything else.

---

# Phase 6 — B11–B15: portability and hygiene

Lowest priority; none of these misbehave on the current targets. Do them as one pass since
they overlap.

### 6.1 B12 — one set of prototypes (do this first; it enables the rest)

Create `lispf4.h`:

```c
#ifndef LISPF4_H
#define LISPF4_H
#include <signal.h>     /* sig_atomic_t, for the break flag below */
#include "f2c.h"

/* Character packing helpers (auxillary.c).  `vec` is a densely packed byte
   array; `ch` is one character in the low 8 bits of an integer, blank padded. */
int  getch_(void *vec, integer *ch, integer *i);
int  putch_(void *vec, integer *ch, integer *i);
int  upcase_(integer *buff, integer *n);

/* Logical-unit I/O (auxillary.c) */
void setup(void);
int  f4_open(int lun, char *file, char *mode);
int  f4_close(int lun);
int  f4_isopen(int lun);
void f4_start_read(int lun);
int  f4_read(int lun, char *v, int n);
int  f4_readu(int lun, char *v, int n);
int  f4_write(int lun, char *v, int n);
int  f4_write_lf(int lun);
int  f4_rewind(int lun);

/* Clock / calendar (auxillary.c) */
integer mslft_(integer *i);
int     mtime_(integer *it);
int     mdate_(integer *it);

/* Interrupt flag (lispf42.c), polled by the evaluator */
extern volatile sig_atomic_t f4_break_pending;
#endif
```

`void *vec` is deliberate: it accepts the `real *`, `char *` and `integer *` arguments
already used at the call sites without editing any of them.

Then: `#include "lispf4.h"` in all three `.c` files, and delete the conflicting in-body
`extern` declarations — in particular `lispf41.c:131` (the variadic `getch_`), `lispf41.c:149`
and `lispf42.c:2585` (`putch_`), `lispf42.c:784` and the other `getch_` declarations, and
`lispf42.c:16-24`. One call site needs a real edit: `lispf41.c:248` passes a fourth argument
(`(ftnlen)1`) that must be dropped.

Scope: roughly 25 `getch_` and 12 `putch_` occurrences across the three files, most of which
are declarations being deleted rather than calls being changed. Compile with `-Wall` after
this; the compiler will now catch any remaining mismatch.

### 6.2 B11 + B14 — endian-neutral character packing

With one prototype in place, make the representation explicit rather than relying on
little-endian byte order (`auxillary.c:10-31`):

```c
#define F4_PAD  ((integer)' ')

int getch_(void *vec, integer *ch, integer *i)
{
        const unsigned char *v = (const unsigned char *)vec;
        *ch = (integer)v[*i - 1]
            | (F4_PAD << 8) | (F4_PAD << 16) | (F4_PAD << 24);
        return 0;
}

int putch_(void *vec, integer *ch, integer *i)
{
        ((unsigned char *)vec)[*i - 1] = (unsigned char)(*ch & 0xFF);
        return 0;
}

int upcase_(integer *buff, integer *n)
{
        integer i;
        for (i = 0; i < *n; i++)
                buff[i] = (buff[i] & ~(integer)0xFF)
                        | (integer)toupper((unsigned char)(buff[i] & 0xFF));
        return 0;
}
```

This matches what `getcht_`/`setcht_` already assume (`*ic % 256`, `lispf42.c:4479, 4496`) on
every byte order, and the `toupper((unsigned char)...)` cast disposes of B14 at the same
time. The packed byte arrays themselves (`PNAME`) are byte-addressed and were always
endian-neutral.

Caution: `upcase_` is called as `upcase_(b_1.abuff, &l)` (`lispf42.c:4207`) where `abuff` is
`integer[160]` — the new signature matches. Verify that upshifting still works
(`basic.img` enables it) with a case-mixed input test.

### 6.3 B13 — signal handling

`lispf42.c:5409-5427`:

```c
volatile sig_atomic_t f4_break_pending = 0;

static void brserv_(int sig)
{
        (void) sig;
        f4_break_pending = 1;
        signal(SIGINT, brserv_);        /* re-arm for System V semantics */
}
```

Poll it where the evaluator already tests for a break — `lispf41.c:341-343`, the top of
`EVAL`:

```c
L1600:
    if (f4_break_pending) {
        f4_break_pending = 0;
        b_1.errtyp = 26;                /* --- Keyboard interrupt */
        b_1.ibreak = TRUE_;
    }
    if (b_1.ibreak) goto L2400;
```

This keeps the handler to a single `sig_atomic_t` write and removes the non-atomic access to
`b_1.errtyp` / `b_1.ibreak` from signal context. Consider `sigaction` with `SA_RESTART`
instead of `signal` as a follow-up.

Test manually: start the interpreter, run a long loop, press Ctrl-C, confirm it breaks to the
prompt rather than being ignored or killing the process.

### 6.4 B15 — per-unit read state

`auxillary.c:58-63`: make the EOL/EOF state per unit and stop resetting it from write paths.

```c
static int read_status[F4_MAXLUN];      /* 1=reading, 2=at eol, 3=at eof */

void f4_start_read(int lun)
{
        if (lun >= 0 && lun < F4_MAXLUN) read_status[lun] = 1;
}
```

`read1` and `f4_read` take the unit and index the array. Call sites:

- keep in the read paths: `rda4_` (`lispf42.c:4724`), `rda1_`'s actual read (`lispf42.c:4996`);
- **remove** from the write paths, where it never belonged: `wra1_` (`lispf42.c:4871, 4891`),
  `wra4_` (`5109`), and `rda1_`'s prompt-echo branches (`4948, 4966`).

Removing them is safe: nothing in the write path consults `read_status`, and after EOF the
next `f4_read` re-attempts `getc`, which returns `EOF` again and re-latches state 3.

### 6.5 Verify

Full suite, plus: a case-mixed input test for `upcase_`, a manual Ctrl-C test, and a
`READFILE` of a multi-line file that ends without a trailing newline (exercises the EOF
path). Build with `-Wall -Wextra` and review new warnings — after Phase 6 the prototypes are
consistent, so the warnings become meaningful.

---

# Cross-cutting: release checklist

After all phases:

1. `make realclean && make` — rebuilds `lispf4`, `bare.img`,
   `basic.img`.
2. `tests/run-tests.sh` — all green.
3. Bump `LAST_UPDATE_YEAR/MONTH/DAY` in **both** `Makefile` and `Makefile.win` (they feed
   the `-DYEAR/-DMONTH/-DDAY` startup banner and are currently 2026/2/26).
4. Rebuild the committed prebuilt binaries and images under `Linux/`, `Mac/`, `Windows/`.
   These are hand-maintained (see commits "Add Mac executables", "Linux and Windows exe
   update") and are **not** produced by the build; shipping Phase 2 without refreshing them
   would leave known-broken arrays and float GC in the distribution.
5. Verify the Windows build separately: `nmake -f Makefile.win`. Phase 3's `stricmp` usage is
   unchanged, but Phase 6's new header and prototypes are the most likely source of MSVC
   diagnostics. Note `sig_atomic_t` needs `<signal.h>`, which `lispf42.c` already includes but
   `lispf41.c` does not.
6. Update `KnowledgeBase.md`: the "Key differences from FORTRAN" and array-representation
   sections describe the `INTEGER*2`-derived layout that Phase 2 removes.
7. Record the fixes in `Bugs1.md` (or a short `Bugs1-resolved.md`) so the reproductions stay
   on record next to their status.

# Risk notes

- **Phase 2 is the one to be careful with.** It changes how array pointer slots and GC
  forwarding pointers are addressed. The mitigations are: the derivation in §2.1, the
  byte-1 correction in §2.4, the array + GC tests in §2.6, and regenerating images.
- **Phase 1 changes `garb_`/`rehash_` classification.** Atoms previously misread as strings
  are now handled correctly; hash tables and GC decisions will differ slightly from before.
  This is the intended correction, but it means Phase 1 should be validated with a
  `SYSOUT`/`SYSIN` round-trip, not just arithmetic tests.
- **Phase 6 touches many lines for little runtime benefit.** If time is short, stop after
  Phase 5: everything reproducible in `Bugs1.md` is fixed by then. Phase 6 addresses latent
  portability and undefined-behaviour issues only.
- Do not let a phase span a release. Each phase leaves the tree in a shippable state.
