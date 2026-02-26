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

---

## File Structure

### C Source Files (the working code)

| File | Lines | Description |
|------|-------|-------------|
| `lispf41.c` | ~3940 | Main eval/apply loop (`lispf4_()`) - the heart of the interpreter |
| `lispf42.c` | ~5423 | Auxiliary routines: `main()`, `init1_()`, `init2_()`, `rollin_()`, `rollou_()`, `move_()`, `garb_()` (GC), reader, printer, arithmetic, atoms, arrays, strings |
| `auxillary.c` | ~190 | Custom C replacements for F2C library: `getch_()`, `putch_()`, file I/O (`f4_open`, `f4_close`, `f4_read`, `f4_write`, etc.), `mslft_()`, `mtime_()`, `mdate_()` |
| `f2c.h` | ~230 | Type definitions: `integer`=`int4`=`int`, `real`=`float4`=`float`, `logical`=`int4`=`int` |

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
| `prolog.lisp` | Prolog-like features |

### Build Files

| File | Description |
|------|-------------|
| `Makefile.unx` | Makefile for Linux/Mac (`make -f Makefile.unx`) |
| `Makefile.win` | Makefile for Windows/MSVC (`nmake -f Makefile.win`) |
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
make -f Makefile.unx           # builds lispf4, bare.img, basic.img
make -f Makefile.unx lispf4    # just the executable
make -f Makefile.unx bare.img  # bare image (needs lispf4 + SYSATOMS)
make -f Makefile.unx basic.img # full image (needs bare.img + .lisp files)
```

Default compile-time parameters (set in Makefile.unx):
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
lispf4 [-cN] [-aN] [-sN] [-pN] [-x] [FILE.IMG]

-cN    CAR/CDR cells (default 100000)
-aN    Atoms (default 3000)
-sN    Stack space (default 1500)
-pN    Print names/strings/reals/arrays (default 5000)
-x     No image file (reads SYSATOMS for system generation)
```

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

- Characters are packed into integers (4 bytes per integer, JBYTES=4)
- `getch_(vec, ch, i)` - extract byte `i` from character vector `vec` into `ch`
- `putch_(vec, ch, i)` - insert byte from `ch` into position `i` of vector `vec`
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

Then 22 individual atoms (A000, APPLY, EVAL, FNCELL, LAMBDA, NLAMBDA, NOBIND, T, etc.) and ~40 error/status messages.

### Image Files (ROLLIN/ROLLOUT)

- **ROLLOUT** (`rollou_()`) serializes interpreter state to a binary file
- **ROLLIN** (`rollin_()`) deserializes state from a binary file
- Image contains: configuration info (15 words), messages, interpreter registers (area), print names, PNP, CAR/CDR arrays, character constants, character type table
- When loading with different memory parameters, `move_()` relocates pointer values

### Garbage Collection (`garb_()`)

Mark-and-sweep with compaction:
- Marks all reachable cells starting from atoms, stack, and registers
- Sweeps unmarked cells back to free list
- Compacting GC moves cells to eliminate fragmentation
- Separate GC for big numbers and atoms (shared space)
- GC statistics tracked in `a_1.garbs` (cell), `a_1.cgarbs` (compacting), `a_1.ngarbs` (bignum), `a_1.agarbs` (atom)

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

## Bugs Found and Fixed

### 1. Pointer Relocation Bug in `move_()` (lispf42.c:1615-1620)

**The bug**: When loading an image file with different memory parameters than it was saved with, the `move_()` function was supposed to add a relocation offset (`diff`) to pointer values in the CAR and CDR arrays. Instead, it was copying values from a shifted array position.

**Wrong code** (original F2C translation):
```c
carcdr_1.car[i__ - 1] = carcdr_1.car[i__ + *diff - 1];  // reads from wrong position
carcdr_1.cdr[i__ - 1] = carcdr_1.cdr[i__ + *diff - 1];  // reads from wrong position
```

**Correct code**:
```c
carcdr_1.car[i__ - 1] += *diff;  // adds offset to the value
carcdr_1.cdr[i__ - 1] += *diff;  // adds offset to the value
```

**Root cause**: This was a latent bug in the original FORTRAN source (`CAR(I)=CAR(I+DIFF)` instead of `CAR(I)=CAR(I)+DIFF`). It was never triggered in FORTRAN because the array sizes were hardcoded constants, so `DIFF` was always 0 and `move_()` was never called. When the C version made memory sizes configurable via command-line arguments, loading images with different parameters caused `DIFF` to be non-zero, exposing the bug.

**Symptoms**: Loading `basic.img` with `-c200000` (instead of default `-c100000`) caused `(GETD 'DE)` to return NIL (function definitions lost). With `-c100001`, it returned garbled data (`FNCELL`).

**Note**: The `args` array relocation on the same function (line 1631) already had the correct `+= *diff` pattern.

### 2. Compilation Warning: Duplicate NULL Check in `main()`

In the memory allocation NULL check (lispf42.c:222-223):
```c
if (!carcdr_1.cdr  ||     // checks cdr
    !carcdr_1.cdr  ||     // should check car
```
The first check should be `!carcdr_1.car`.

### 3. EOF Infinite Loop (pre-existing in FORTRAN)

When stdin reaches EOF, the interpreter enters an infinite loop printing the prompt character (underscore). The original FORTRAN had exit code (`MESS(32)` / `LSPEX`) that was commented out by Tore Risch ("TR"). This is not a bug introduced by the C conversion.

### 4. K&R Function Declarations (compilation issue on modern GCC)

The old-style declarations `double pow(), fmod();` and `double pow(), log10();` conflict with modern GCC which has built-in knowledge of these math functions. Fixed by adding `#include <math.h>` and removing the K&R declarations.

---

## Key Function Reference

| Function | File:Line | Description |
|----------|-----------|-------------|
| `main()` | lispf42.c:148 | Entry point, command-line parsing, memory allocation, startup |
| `init1_()` | lispf42.c:1065 | Machine-dependent initialization (JBYTES, MAXBIG, NUMADD, etc.) |
| `init2_()` | lispf42.c:1152 | Reads SYSATOMS, initializes atoms/hash table/free lists |
| `lispf4_()` | lispf41.c | Main eval/apply loop |
| `rollin_()` | lispf42.c:1369 | Load binary image file |
| `rollou_()` | lispf42.c:1536 | Save binary image file |
| `move_()` | lispf42.c:1592 | Relocate pointers when loading images with different parameters |
| `garb_()` | lispf42.c:3256 | Garbage collector (mark-and-sweep with compaction) |
| `shift_()` | lispf42.c:3124 | Character input reader / tokenizer |
| `matom_()` | lispf42.c:4144 | Atom creation |
| `getcht_()` | lispf42.c:~4445 | Query character type table |
| `setcht_()` | lispf42.c:~4470 | Set character type table entry |
| `getch_()` | auxillary.c:10 | Extract byte from integer array |
| `putch_()` | auxillary.c:19 | Insert byte into integer array |
| `f4_open()` | auxillary.c:41 | Open file on logical unit |
| `f4_read()` | auxillary.c:83 | Read formatted (text) data |
| `f4_readu()` | auxillary.c:102 | Read unformatted (binary) data |

---

## Coding Conventions

- All FORTRAN-origin names end with underscore: `init1_()`, `garb_()`, `rollin_()`
- Array accesses use `[i__ - 1]` for 1-to-0 based index conversion
- `*SETC*` comments mark where FORTRAN `CALL SETCAR(I,V)` / `CALL SETCDR(I,V)` was translated to direct array assignment
- FORTRAN loop pattern `for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__)` ensures the loop body runs at least once (FORTRAN DO loop semantics)
- Common block struct aliases: `a_1` = `a_`, `b_1` = `b_`, `carcdr_1` = `carcdr_`, etc.
- `#define` macros alias struct members to match FORTRAN EQUIVALENCE statements (e.g., `#define args ((integer *)&b_1.arg)`)
