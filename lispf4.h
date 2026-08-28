/*  lispf4.h -- shared declarations for the hand-written C support routines.
 *
 *  These functions were originally declared separately inside each F2C
 *  translated routine, with types that did not agree between translation
 *  units (getch_ was even declared variadic in lispf41.c while being
 *  defined non-variadic in auxillary.c, which is undefined behaviour).
 *  Every declaration now comes from here.
 */

#ifndef LISPF4_H
#define LISPF4_H

#include <signal.h>		/*  sig_atomic_t, for the break flag below  */
#include <setjmp.h>		/*  jmp_buf, for the reset jump below  */
#include "f2c.h"

/*  Character packing.
 *
 *  VEC is a densely packed byte array (PNAME, or a buffer of chars).  CH is
 *  one character held in the low 8 bits of an integer, blank padded in the
 *  upper bytes -- the representation GETCHT/SETCHT assume when they recover
 *  the character with (IC mod 256).  VEC is void * so that the existing call
 *  sites, which pass real *, char * and integer *, need no casts.
 */
int	getch_(void *vec, integer *ch, integer *i);
int	putch_(void *vec, integer *ch, integer *i);
int	upcase_(integer *buff, integer *n);

/*  Logical-unit I/O.  Every routine validates the unit number and returns
 *  non-zero rather than dereferencing a NULL stream.
 */
void	setup(void);
int	f4_open(int lun, char *file, char *mode);
int	f4_close(int lun);
int	f4_isopen(int lun);
void	f4_start_read(int lun);
int	f4_at_line_end(int lun);
int	f4_write_lf(int lun);
int	f4_rewind(int lun);

/*  A1 transfers: one character per word, held in the low 8 bits of an integer
 *  and blank padded above (the representation getch_/getcht_ use).  These
 *  assemble and take apart the word arithmetically, so they do not depend on
 *  byte order -- unlike a direct store into byte 0, which only put the
 *  character where `ic % 256` would find it on a little-endian host.
 */
int	f4_read_char(int lun, integer *ch);
int	f4_write_char(int lun, const integer *ch);

/*  Four-byte transfers, used for A4 packed text (RDA4/WRA4, which read and
 *  write the same representation so it round-trips) and for the raw binary
 *  image words (DMPIN/DMPOUT).  These move bytes verbatim; an image file is
 *  therefore only portable between machines of the same byte order, which was
 *  already true of every pointer value stored in it.
 */
int	f4_read(int lun, char *v, int n);
int	f4_readu(int lun, char *v, int n);
int	f4_write(int lun, char *v, int n);

/*  Clock and calendar.  */
integer	mslft_(integer *i);
int	mtime_(integer *it);
int	mdate_(integer *it);

/*  Set by the SIGINT handler, polled by the interpreter.  A signal handler
 *  may only touch a volatile sig_atomic_t, so the handler no longer writes
 *  B/IBREAK and B/ERRTYP directly.
 */
extern volatile sig_atomic_t f4_break_pending;

/*  The interpreter's reset point, armed at the top of LISPF4 and jumped to
    by GARB when list space is exhausted.  GARB used to reach the reset label
    by CALLING LISPF4 again -- which never returns, so every exhaustion left
    one more CONS -> GARB -> LISPF4 chain on the C stack for the rest of the
    session.  F4_RESET_READY says whether the jump target is live: GARB can
    also run from INIT2, before LISPF4 has been entered at all.  */

extern jmp_buf f4_reset;
extern int     f4_reset_ready;

/*  M2: why the last PRIN1 gave up, when SYSFLAG 6 is NIL and it did --
    1 depth, 2 length or node budget, 3 an array -- and the figure or the
    array that goes with it.  PRIN1 sets them; PRIN0's entry in LISPF4
    turns them into a SYSERROR.  CNAME_ interns a C string as a literal
    atom for that report.  Both live in lispf42.c beside the printer.  */

extern integer f4_prfail;
extern integer f4_prfval;
integer	cname_(const char *s);

#endif	/*  LISPF4_H  */
