/* lispf42.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include "f2c.h"

double	pow(), fmod();
extern time_t start_time;


#define SHOWINT(x)	fprintf(stderr, #x " = %d\n", x)


//#define FORTRAN_LIB

/* Common Block Declarations */

extern struct {
    integer narea, jbytes, ibytes, bytes, chdiv, maxint, maxbig, maxrec, 
	    natomp, nfreep, jbp, numbp, nfreet, numadd, npname, nfreeb, natom,
	     nstack, nhtab, bignum, ismall, dpnp, dpname, ipromp, nathsh, 
	    nbytes, nchtyp, nbmess, maxmes, iresol, ipower;
    real bigmax, fuzz;
    integer garbs, cgarbs, ngarbs, agarbs;
    real rmax;
} a_;

#define a_1 a_

extern struct {
    integer arg, arg2, arg3, alist__, form, temp1, temp2, temp3, i1cons, 
	    i2cons, nargs, nil, a000, apply, eval, fncell, fexpr, funarg, 
	    lambda, array, string, substr, lispx, nlambd, unboun, prog, quote,
	     expr, error, sform, sinter, subarg, dotat, t, subr0, subr11, 
	    subr1, subr2, subr3, subr, fsubr, ip, jp, ipp, jpp, middl, abup1, 
	    cht, chr, numgen, unused, brlev, brflg, iflg1, iflg2, isplft;
    logical ibreak;
    integer errtyp, iobuff, maxlun, lunsys, lunins, lunuts, maxpar, itabwg, 
	    lunin, rdpos, lmargr, margr, lunut, prtpos, lmarg, marg, levell, 
	    levelp, dreg[7], abuff[160], rdbuff[160], prbuff[160], buff[160], 
	    imess[400];
    real *pname;
    integer *pnp, *htab, *stack;
} b_;

#define b_1 b_

extern struct {
    integer *car, *cdr, chtab[256];
} carcdr_;

#define carcdr_1 carcdr_

extern struct {
    integer space, lpar, rpar, ilbchr, irbchr, strchr, iqchr, ubr, dot, itchr,
	     iplus, iminus, ifig[10], atend, softbr, echar, nochar;
} chars_;

#define chars_1 chars_

extern struct {
    integer *jill, *jack, env, tops, hill, hillw;
} jaan_;

#define jaan_1 jaan_

extern struct {
    integer protxt[80], prolen;
} prompt_;

#define prompt_1 prompt_

/* Table of constant values */

static integer c__1 = 1;
static struct { integer fill; char val[0+1]; char fill2[3]; } c_b3_st = { 0,
	"" };
#define c_b3 c_b3_st.val
static struct { integer fill; char val[12+1]; char fill2[3]; } c_b4_st = { 0,
	"LISPF4.IMG  " };
#define c_b4 c_b4_st.val
static struct { integer fill; char val[4+1]; char fill2[3]; } c_b5_st = { 0,
	"OLD " };
#define c_b5 c_b5_st.val
static struct { integer fill; char val[12+1]; char fill2[3]; } c_b6_st = { 0,
	"UNFORMATTED " };
#define c_b6 c_b6_st.val
static integer c__10 = 10;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__3 = 3;
static struct { integer fill; char val[8+1]; char fill2[3]; } c_b98_st = { 0,
	"SYSATOMS" };
#define c_b98 c_b98_st.val
static integer c__15 = 15;
static real c_b239 = 1.f;
static integer c__4 = 4;
static integer c__24 = 24;
static integer c__36 = 36;
static integer c__39 = 39;
static integer c__30 = 30;

/* ******FLOSOR      (AUX ROUTINES FOR INTERPRETER) */
/* *********************************************************************** */
/* *                                                                     * */
/* *                           LISP F4 (FLOATING)                        * */
/* *                           -------                                   * */
/* *                                                                     * */
/* *       THE SYSTEM WAS WRITTEN BY    UPDATED BY                       * */
/* *       DR. MATS NORDSTROM           HANS ERIKSSON AND                * */
/* *                                    KRISTINA JOHANSSON               * */
/* *                                    DR. TORE RISCH                   * */
/* *       READER, PRINTER, ARRAYS, AND FLONUMS                          * */
/* *       WERE ADDED BY                MATS CARLSSON                    * */
/* *       THE STACK-VARIANT OF THE                                      * */
/* *       INTERPRETER WAS WRITTEN BY   JAAN KOORT                       * */
/* *                                                                     * */
/* *       UPMAIL                                                        * */
/* *       STUREGATAN 2A                                                 * */
/* *       S-752 23  UPPSALA                                             * */
/* *       SWEDEN                                                        * */
/* *                                                                     * */
/* *        THE WORK WAS SUPPORTED BY THE SWEDISH BOARD                  * */
/* *        FOR TECHNICAL DEVELOPMENT (STU) NO 76-4253.                  * */
/* *                                                                     * */
/* *********************************************************************** */
/* *IBM SUPPLIED MACHINE CODE*      EXTERNAL BRSERV */
#ifdef FORTRAN_LIB
/* Main program */ int MAIN__(void)
#else
int	main(int argc, char *argv[])
#endif
{
    /* Initialized data */

#ifdef FORTRAN_LIB
    static integer istart = 0;  /*  0=SYSATOMS,  1=LISPF4.IMG  */
#else
     integer istart;  /*  0=SYSATOMS,  1=argv[1]  */
#endif

    /* System generated locals */
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_open(olist *), f_clos(cllist *);

    /* Local variables */
    static integer ixcc;
    extern /* Subroutine */ int init1_(void), init2_(void), brset_(void), 
	    lspex_(void), lispf4_(integer *);
    extern integer rollin_(integer *);

    start_time = time(NULL);
/* -- SET UP INTERRUPT HANDLER */
    brset_();

    a_1.nfreet = 100000;
    a_1.natom = 3000;
    a_1.nstack = 1500;
    jaan_1.hill = 1500;
    a_1.npname = 5000;
#ifndef	FORTRAN_LIB
    while (argc > 1 &&  *argv[1] == '-') {
	    switch (argv[1][1]) {
	    case 'c':  /*  car/cdr cells  */
		    a_1.nfreet = atoi(argv[1]+2);
		    break;
	    case 'a':  /*  atoms  */
		    a_1.natom = atoi(argv[1]+2);
		    break;
	    case 's':  /*  stack space  */
		    a_1.nstack = atoi(argv[1]+2);
		    jaan_1.hill = a_1.nstack;
		    break;
	    case 'p':  /*  print names / strings / reals  / arrays  */
		    a_1.npname = atoi(argv[1]+2);
		    break;
	    }
	    argc--;
	    argv++;
    }
#endif
    carcdr_1.car = (integer *) calloc(a_1.nfreet, sizeof(integer));
    carcdr_1.cdr = (integer *) calloc(a_1.nfreet, sizeof(integer));
    b_1.pnp = (integer *) calloc(a_1.natom+1, sizeof(integer));
    a_1.nhtab = (integer)(1.5 * (double) a_1.natom);
    b_1.htab = (integer *) calloc(a_1.nhtab, sizeof(integer));
    b_1.stack = (integer *) calloc(a_1.nstack, sizeof(integer));
    jaan_1.jill = (integer *) calloc(jaan_1.hill, sizeof(integer));
    jaan_1.jack = (integer *) calloc(jaan_1.hill, sizeof(integer));
    b_1.pname = (real *) calloc(a_1.npname+2, sizeof(real));
    if (!carcdr_1.cdr  ||
	!carcdr_1.cdr  ||
	!b_1.pnp       ||
	!b_1.htab      ||
	!b_1.stack     ||
	!jaan_1.jill   ||
	!jaan_1.jack   ||
	!b_1.pname) {
	    fprintf(stderr, "Out of memory\n");
	    exit(-1);
    }
    istart = argc > 1;  /*  0=SYSATOMS,  1=argv[1]  */


    init1_();
    if (istart != 0) {
	goto L10;
    }
    init2_();
    istart = 1;
    lispf4_(&c__1);
    lspex_();
#ifdef FORTRAN_LIB
    s_stop(c_b3, (ftnlen)0);
#else
    exit(0);
#endif
/* -- IN CASE OF INTERRUPT AND RESTART THE VARIABLE ISTART */
/* -- TELLS IF WE HAVE DONE THE INIT, THEN WE CAN CALL THE INTERPRETER */
/* -- DIRECTLY */
/* -- ON COMPUTERS WHERE YOU SAVE CORE IMAGES (E.G. DEC20) YOU CAN */
/* -- START THE LISPF4 SYSTEM, READ ALL LISPCODE YOU WANT AND THEN */
/* -- EXIT AND SAVE THE CORE IMAGE. THIS WAY YOU DONT HAVE TO USE ROLLFILES */
/* -- */
L10:
#ifdef FORTRAN_LIB
    o__1.oerr = 0;
    o__1.ounit = 10;
    o__1.ofnmlen = 10;
    o__1.ofnm = c_b4;
    o__1.orl = 0;
    o__1.osta = c_b5;
    o__1.oacc = 0;
    o__1.ofm = c_b6;
    o__1.oblnk = 0;
    f_open(&o__1);
    ixcc = rollin_(&c__10);
    cl__1.cerr = 0;
    cl__1.cunit = 10;
    cl__1.csta = 0;
    f_clos(&cl__1);
#else
#ifdef unix
    f4_open(10, argv[1], "r");
#else
    f4_open(10, argv[1], "rb");
#endif
    ixcc = rollin_(&c__10);
    f4_close(10);
#endif
    lispf4_(&c__1);
    lspex_();
#ifdef FORTRAN_LIB
    s_stop(c_b3, (ftnlen)0);
#else
    exit(0);
#endif
    return 0;
} /* MAIN__ */


/* Subroutine */ int apush_(integer *i__)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    --b_1.jp;
    if (b_1.ip >= b_1.jp) {
	goto L2;
    }
    b_1.stack[b_1.jp - 1] = *i__;
    return 0;
L2:
    b_1.stack[b_1.ip - 1] = 16;
    ++b_1.jp;
    return 0;
} /* apush_ */

/* Subroutine */ int apush2_(integer *i__, integer *j)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    b_1.jp += -2;
    if (b_1.ip >= b_1.jp) {
	goto L2;
    }
    b_1.stack[b_1.jp] = *i__;
    b_1.stack[b_1.jp - 1] = *j;
    return 0;
L2:
    b_1.stack[b_1.ip - 1] = 16;
    b_1.jp += 2;
    return 0;
} /* apush2_ */

/* Subroutine */ int apop_(integer *i__)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (b_1.jp > a_1.nstack) {
	goto L2;
    }
    *i__ = b_1.stack[b_1.jp - 1];
    ++b_1.jp;
    return 0;
L2:
    b_1.stack[b_1.ip - 1] = 17;
    b_1.jp = b_1.ip + 1;
    return 0;
} /* apop_ */

/* Subroutine */ int apop2_(integer *i__, integer *j)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (b_1.jp > a_1.nstack) {
	goto L2;
    }
    *i__ = b_1.stack[b_1.jp - 1];
    *j = b_1.stack[b_1.jp];
    b_1.jp += 2;
    return 0;
L2:
    b_1.stack[b_1.ip - 1] = 17;
    b_1.jp = b_1.ip + 1;
    return 0;
} /* apop2_ */

/* Subroutine */ int apush3_(integer *i__, integer *j, integer *k)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    b_1.jp += -3;
    if (b_1.ip >= b_1.jp) {
	goto L2;
    }
    b_1.stack[b_1.jp + 1] = *i__;
    b_1.stack[b_1.jp] = *j;
    b_1.stack[b_1.jp - 1] = *k;
    return 0;
L2:
    b_1.stack[b_1.ip - 1] = 16;
    b_1.jp += 3;
    return 0;
} /* apush3_ */

/* Subroutine */ int apop3_(integer *i__, integer *j, integer *k)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (b_1.jp > a_1.nstack) {
	goto L2;
    }
    *i__ = b_1.stack[b_1.jp - 1];
    *j = b_1.stack[b_1.jp];
    *k = b_1.stack[b_1.jp + 1];
    b_1.jp += 3;
    return 0;
L2:
    b_1.stack[b_1.ip - 1] = 17;
    b_1.jp = b_1.ip + 1;
    return 0;
} /* apop3_ */

/* Subroutine */ int fpush_(integer *i__)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    ++b_1.ip;
    if (b_1.ip >= b_1.jp) {
	goto L2;
    }
/* L1: */
    b_1.stack[b_1.ip - 1] = *i__;
    return 0;
L2:
    --b_1.ip;
    b_1.stack[b_1.ip - 1] = 16;
    return 0;
} /* fpush_ */

/* Subroutine */ int fpop_(integer *i__)
{
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    *i__ = b_1.stack[b_1.ip - 1];
    --b_1.ip;
    return 0;
} /* fpop_ */

integer cons_(integer *i1, integer *i2)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern integer garb0_(integer *, integer *);
    static integer icons;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (a_1.nfreep == b_1.nil) {
	goto L3;
    }
    icons = a_1.nfreep;
    ret_val = icons;
    a_1.nfreep = carcdr_1.cdr[a_1.nfreep - 1];
/* *SETC*      CALL SETCAR(ICONS,I1) */
    carcdr_1.car[icons - 1] = *i1;
/* *SETC*      CALL SETCDR(ICONS,I2) */
    carcdr_1.cdr[icons - 1] = *i2;
    return ret_val;
L3:
    ret_val = garb0_(i1, i2);
    return ret_val;
} /* cons_ */

integer garb0_(integer *i1, integer *i2)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern integer garb_(integer *);
    static integer i__, icons;

/*                                      PERFORM A FREE CELL GARB */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/* L3: */
    b_1.i1cons = *i1;
    b_1.i2cons = *i2;
    i__ = garb_(&c__0);
    icons = a_1.nfreep;
    ret_val = icons;
    a_1.nfreep = carcdr_1.cdr[a_1.nfreep - 1];
/* *SETC*      CALL SETCAR(ICONS,I1CONS) */
    carcdr_1.car[icons - 1] = b_1.i1cons;
/* *SETC*      CALL SETCDR(ICONS,I2CONS) */
    carcdr_1.cdr[icons - 1] = b_1.i2cons;
    if (i__ > b_1.isplft) {
	return ret_val;
    }
    b_1.isplft /= 2;
    b_1.ibreak = TRUE_;
    b_1.errtyp = 34;
    return ret_val;
} /* garb0_ */

integer subpr_(integer *ix, integer *iy, integer *is)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer icdr;
    extern /* Subroutine */ int apop_(integer *);
    extern integer cons_(integer *, integer *);
    static integer i__, j, k;
#define s ((integer *)&b_1.temp2)  /*  ((integer *)&b_1 + 6)  */
    extern integer equal_(integer *, integer *);
    extern /* Subroutine */ int fpush_(integer *), apush_(integer *);
#define res ((integer *)&b_1.temp1)  /*  ((integer *)&b_1 + 5)  */

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    *s = *is;
    fpush_(&c__1);
/* -- MEMB TEST OF S IN IX */
L5:
    j = *ix;
    k = *iy;
L6:
    if (j <= a_1.natom || j > a_1.nfreet) {
	goto L7;
    }
    if (k <= a_1.natom || k > a_1.nfreet) {
	k = b_1.nil;
    }
    if (equal_(&carcdr_1.car[j - 1], s) == b_1.t) {
	goto L8;
    }
    j = carcdr_1.cdr[j - 1];
    k = carcdr_1.cdr[k - 1];
    goto L6;
L7:
    if (j == b_1.nil || j != *s) {
	goto L10;
    }
/* --   (SUBPAIR '(X Y . Z) '(A B C D E) '(X Y Z))= */
/* --     (A B C D E) */
    *res = k;
    goto L20;
L8:
    *res = carcdr_1.car[k - 1];
    goto L20;
L10:
    if (*s > a_1.natom && *s <= a_1.nfreet) {
	goto L30;
    }
/* L16: */
    *res = *s;
L20:
    i__ = b_1.stack[b_1.ip - 1];
    if (i__ > 3) {
	goto L90;
    }
/* L25: */
    --b_1.ip;
    switch (i__) {
	case 1:  goto L50;
	case 2:  goto L35;
	case 3:  goto L40;
    }
L30:
    icdr = carcdr_1.cdr[*s - 1];
    apush_(&icdr);
    fpush_(&c__2);
    *s = carcdr_1.car[*s - 1];
    goto L5;
L35:
    *s = b_1.stack[b_1.jp - 1];
    b_1.stack[b_1.jp - 1] = *res;
    fpush_(&c__3);
    goto L5;
L40:
    apop_(s);
    *res = cons_(s, res);
    goto L20;
L50:
    ret_val = *res;
    return ret_val;
/*             PDL OVERFLOW. LEAVE OFLO ADDRESS (16) IN F-STACK */
L90:
    return ret_val;
} /* subpr_ */

#undef res
#undef s


integer equal_(integer *ii, integer *jj)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int apop2_(integer *, integer *);
    static integer i__, j;
    extern /* Subroutine */ int apush2_(integer *, integer *);
    static integer in, jn;
    extern integer comppn_(integer *, integer *);
    extern doublereal gtreal_(integer *, integer *);
    static integer jpe;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    jpe = b_1.jp;
    i__ = *ii;
    j = *jj;
L10:
    if (i__ == j) {
	goto L50;
    }
    if (i__ <= a_1.natom) {
	goto L80;
    }
    if (i__ > a_1.nfreet) {
	goto L70;
    }
    if (j <= a_1.natom || j > a_1.nfreet) {
	goto L60;
    }
/*             I NOT EQ J. I AND J ARE LISTS */
/* L15: */
    apush2_(&i__, &j);
    i__ = carcdr_1.car[i__ - 1];
    j = carcdr_1.car[j - 1];
    goto L10;
L20:
    apop2_(&j, &i__);
    i__ = carcdr_1.cdr[i__ - 1];
    j = carcdr_1.cdr[j - 1];
    goto L10;
L50:
    if (b_1.jp < jpe) {
	goto L20;
    }
/* L51: */
    ret_val = b_1.t;
    return ret_val;
L60:
    b_1.jp = jpe;
    ret_val = b_1.nil;
    return ret_val;
/*               I = NUMBER */
L70:
    if (j <= a_1.nfreet) {
	goto L60;
    }
    if (gtreal_(&i__, &in) != gtreal_(&j, &jn)) {
	goto L60;
    }
    if (in != jn) {
	goto L60;
    }
    goto L50;
/*                                      I = LITERAL/STRING */
L80:
    if (0 == comppn_(&i__, &j)) {
	goto L50;
    }
    goto L60;
} /* equal_ */

integer get_(integer *j, integer *i__)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer k;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (*j > a_1.nfreet) {
	goto L40;
    }
    k = carcdr_1.cdr[*j - 1];
L8:
    if (k <= a_1.natom || k > a_1.nfreet) {
	goto L40;
    }
    if (carcdr_1.car[k - 1] == *i__) {
	goto L20;
    }
/* L12: */
    k = carcdr_1.cdr[k - 1];
    k = carcdr_1.cdr[k - 1];
    goto L8;
L20:
    k = carcdr_1.cdr[k - 1];
    ret_val = carcdr_1.car[k - 1];
    return ret_val;
L40:
    ret_val = b_1.nil;
    return ret_val;
} /* get_ */

integer getpn_(integer *x, integer *main, integer *jb, integer *ipl)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer l, ii;
    extern integer getnum_(integer *);

/* ----- */
/*     DECODES THE LITATOM/STRING/SUBSTRING X */
/*     MAIN   <-- POINTER TO MAIN STRING, IF SUBSTRING */
/*     JB     <-- BYTE OFFSET TO PRINTNAME */
/*     IPL    <-- BYTE LENGTH OF PRINTNAME */
/*     GETPN <-- -1 = X INVALID, 0 = LITATOM, 1 = STRING/SUBSTRING */
/* ----- */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (*x > a_1.natom) {
	goto L9010;
    }
    *jb = b_1.pnp[*x - 1];
    *ipl = b_1.pnp[*x] - *jb;
    *main = *x;
    if (carcdr_1.car[*x - 1] == b_1.string) {
	goto L9030;
    }
    if (carcdr_1.car[*x - 1] != b_1.substr) {
	goto L9020;
    }
/*                                      TAKE CARE OF THE SUBSTR CASE */
    l = carcdr_1.cdr[*x - 1];
    if (l > a_1.nfreet) {
	goto L9010;
    }
    *main = carcdr_1.car[l - 1];
    if (*main > a_1.natom) {
	goto L9010;
    }
    if (carcdr_1.car[*main - 1] != b_1.string) {
	goto L9010;
    }
    l = carcdr_1.cdr[l - 1];
    if (l > a_1.nfreet) {
	goto L9010;
    }
    if (carcdr_1.car[l - 1] <= a_1.nfreet || carcdr_1.cdr[l - 1] <= 
	    a_1.nfreet) {
	goto L9010;
    }
/*                                      GET BYTE ADDR */
    ii = carcdr_1.car[l - 1];
    *jb = b_1.pnp[*main - 1] + getnum_(&ii) - 1;
/*                                      GET BYTE LENGTH */
    ii = carcdr_1.cdr[l - 1];
    *ipl = getnum_(&ii);
    goto L9030;
/*                                      EXITS. */
L9010:
    ret_val = -1;
    return ret_val;
L9020:
    ret_val = 0;
    return ret_val;
L9030:
    ret_val = 1;
    return ret_val;
} /* getpn_ */

integer comppn_(integer *x, integer *y)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    static integer main, i__;
    extern /* Subroutine */ int getch_(real *, integer *, integer *);
    extern integer getpn_(integer *, integer *, integer *, integer *);
    static integer jb, jb2, ich, jch, min__, ipl, ipl2;

/* ----- */
/*     COMPARES THE PRINTNAMES OF ITS ARGUMENTS */
/*     COMPPN <-- -2 IF X ILLEGAL;  <-- 2 IF Y IS ILLEGAL; */
/*            <-- -1 IF X < Y;      <-- 1 IF X > Y; */
/*            <--  0 IF X = Y; */
/* ----- */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    ret_val = -2;
    if (0 > getpn_(x, &main, &jb, &ipl)) {
	return ret_val;
    }
    ret_val = 2;
    if (0 > getpn_(y, &main, &jb2, &ipl2)) {
	return ret_val;
    }
    ret_val = 0;
    min__ = ipl;
    if (ipl > ipl2) {
	min__ = ipl2;
    }
    if (min__ == 0) {
	goto L20;
    }
/*                                      COMPARE BYTE BY BYTE */
    i__1 = min__;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	getch_(b_1.pname, &ich, &jb);
	getch_(b_1.pname, &jch, &jb2);
	if (ich < jch) {
	    goto L30;
	}
	if (ich > jch) {
	    goto L40;
	}
	++jb;
/* L10: */
	++jb2;
    }
L20:
    if ((i__1 = ipl - ipl2) < 0) {
	goto L30;
    } else if (i__1 == 0) {
	goto L50;
    } else {
	goto L40;
    }
L30:
    ret_val = -1;
    goto L50;
L40:
    ret_val = 1;
L50:
    return ret_val;
} /* comppn_ */

/* Subroutine */ int arrutl_(integer *iptr, integer *iactn, integer *ipart, 
	integer *ifirst, integer *ilen)
{
    /* System generated locals */
    integer i__1;
    static integer equiv_3[3], equiv_6[3];

    /* Local variables */
#define llen (equiv_6)
#define llen1 (equiv_6)
#define llen2 (equiv_6 + 1)
#define llen3 (equiv_6 + 2)
    static integer j;
#define lword (equiv_3)
    static integer lbyte1, lbyte2, lbyte3;
#define lword1 (equiv_3)
#define lword2 (equiv_3 + 1)
#define lword3 (equiv_3 + 2)
#define jpname ((shortint *) b_1.pname)  /*  ((shortint *)&b_1 + 2244)  */
#define ipname ((integer *) b_1.pname)   /*  ((integer *)&b_1 + 1122)  */
    static integer jfirst, ind;

/* ---- */

/* + TAKES CARE OF THE SYSTEM'S ARRAY HANDLING. */
/*   ARRAYS ARE REPRESENTED AS FOLLOWS: */

/*             +--------------+ */
/*             !              ! */
/* PNAME:...(Z * * (POINTERS) Z (INTEGERS) Z (REALS)) (NEXT ATOM)... */
/*          !    !                         !          ! */
/*          !    +-------------------------+          ! */
/* PNP:(IPTR)                                  (IPTR+1) */

/* CAR(IPTR) = ARRAY                    (BYTE POINTERS) */
/* CDR(IPTR) = NIL                      (Z = 0 OR MORE SLACK BYTES) */

/* + PARAMETERS: */

/*   IPTR       LISP POINTER TO ARRAY (AS ABOVE) */

/*   IACTN  = 1 GET ARRAY ELEMENT */
/*          = 2 SET ARRAY ELEMENT */
/*          = 3 SET IFIRST AND ILEN TO ARRAY PART BOUNDS */
/*          = 4 MAKE AN ARRAY PART, INITTED TO IFIRST */

/*   IPART  = 1 POINTER PART IS REFERRED TO */
/*          = 2 INTEGER PART */
/*          = 3 REAL PART */

/*   IFIRST:    ARRAY PART RELATIVE INDEX (IACTN = 1,2) OR */
/*              PNAME RELATIVE INDEX      (IACTN = 3,4) */

/*   ILEN:      PNAME RELATIVE INDEX TO VALUE (IACTN = 1,2) OR */
/*              NO. OF ELEMENTS IN PART (IACTN = 3,4) */

/* + NUMERIC ELEMENT VALUES ARE TRANSMITTED VIA IPNAME/PNAME */

/* ----- */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */

    if (b_1.ibreak) {
	goto L9000;
    }
/*                                      CHECK PARAMETER IPTR */
    if (*iptr < b_1.nil || *iptr > a_1.natom) {
	goto L8010;
    }
    if (carcdr_1.car[*iptr - 1] != b_1.array) {
	goto L8010;
    }
/*                                      GET ARRAY BOUNDS */
    lbyte1 = b_1.pnp[*iptr - 1];
    lbyte1 = abs(lbyte1);
    *lword1 = (lbyte1 - 2) / a_1.jbytes + 4;
    if (*iactn == 4 && *ipart == 1) {
	goto L4000;
    }
    lbyte2 = jpname[*lword1 - 3];
    *llen1 = (lbyte2 - lbyte1) / a_1.jbytes - 2;
    if (*ipart < 2) {
	goto L20;
    }
    *lword2 = (lbyte2 - 2) / a_1.ibytes + 2;
    lbyte3 = jpname[*lword1 - 2];
    *llen2 = (lbyte3 - lbyte2) / a_1.ibytes;
    if (*ipart < 3) {
	goto L20;
    }
    *lword3 = (lbyte3 - 2) / a_1.bytes + 2;
    *llen3 = b_1.pnp[*iptr];
    *llen3 = (abs(*llen3) - lbyte3) / a_1.bytes;
L20:
    switch (*iactn) {
	case 1:  goto L1000;
	case 2:  goto L2000;
	case 3:  goto L3000;
	case 4:  goto L4000;
    }

/* GET ARRAY ELEMENT (CALLED BY ELT,ELTI,ELTR) */

L1000:
    if (*ifirst <= 0 || *ifirst > llen[*ipart - 1]) {
	goto L8020;
    }
    *ilen = lword[*ipart - 1] + *ifirst - 1;
    goto L9000;

/* SET ARRAY ELEMENT (CALLED BY SETA,SETI,SETR) */

L2000:
    if (*ifirst <= 0 || *ifirst > llen[*ipart - 1]) {
	goto L8020;
    }
    ind = lword[*ipart - 1] + *ifirst - 1;
    if (*ipart == 1) {
	jpname[ind - 1] = jpname[*ilen - 1];
    }
    if (*ipart == 2) {
	ipname[ind - 1] = ipname[*ilen - 1];
    }
    if (*ipart == 3) {
	b_1.pname[ind - 1] = b_1.pname[*ilen - 1];
    }
    goto L9000;

/* GET ARRAY BOUNDS (CALLED BY ARRAYSIZE AND GARB (STEPS 1,6)) */

L3000:
    *ifirst = lword[*ipart - 1];
    *ilen = llen[*ipart - 1];
    goto L9000;

/* MAKE AN ARRAY (CALLED BY ARRAY AND GARB (STEP 4)) */
/* REQUIRES THAT WE HAVE ENSURED THAT THERE IS ENOUGH SPACE ALREADY */
/* IFIRST = 0 YIELDS DEFAULT VALUES IN THE NEW ARRAY */

L4000:
    jfirst = *ifirst;
    switch (*ipart) {
	case 1:  goto L4100;
	case 2:  goto L4200;
	case 3:  goto L4300;
    }
/* PTR PART */
L4100:
    i__1 = *ilen + 2;
    a_1.jbp = ((a_1.jbp + a_1.jbytes - 2) / a_1.jbytes + i__1) * a_1.jbytes + 
	    1;
    if (*ilen == 0 || *lword1 == jfirst) {
	goto L4410;
    }
    i__1 = *ilen;
    for (j = 1; j <= i__1 || j == 1; ++j) {
	if (jfirst == 0) {
	    goto L4120;
	}
	jpname[*lword1 - 1] = jpname[jfirst - 1];
	++jfirst;
	goto L4150;
L4120:
	jpname[*lword1 - 1] = (shortint) b_1.nil;
L4150:
	++(*lword1);
    }
    goto L4410;
/* INT PART */
L4200:
    a_1.jbp = ((a_1.jbp + a_1.ibytes - 2) / a_1.ibytes + *ilen) * a_1.ibytes 
	    + 1;
    if (*ilen == 0 || *lword2 == jfirst) {
	goto L4420;
    }
    i__1 = *ilen;
    for (j = 1; j <= i__1 || j == 1; ++j) {
	if (jfirst == 0) {
	    goto L4220;
	}
	ipname[*lword2 - 1] = ipname[jfirst - 1];
	++jfirst;
	goto L4250;
L4220:
	ipname[*lword2 - 1] = 0;
L4250:
	++(*lword2);
    }
    goto L4420;
/* REAL PART */
L4300:
    a_1.jbp = ((a_1.jbp + a_1.bytes - 2) / a_1.bytes + *ilen) * a_1.bytes + 1;
    if (*ilen == 0 || *lword3 == jfirst) {
	goto L4430;
    }
    i__1 = *ilen;
    for (j = 1; j <= i__1 || j == 1; ++j) {
	if (jfirst == 0) {
	    goto L4320;
	}
	b_1.pname[*lword3 - 1] = b_1.pname[jfirst - 1];
	++jfirst;
	goto L4350;
L4320:
	b_1.pname[*lword3 - 1] = 0.f;
L4350:
	++(*lword3);
    }
    goto L4430;
/*                                      SET THE POINTERS */
L4410:
    *lword1 -= *ilen;
    jpname[*lword1 - 3] = (shortint) a_1.jbp;
L4420:
    jpname[*lword1 - 2] = (shortint) a_1.jbp;
L4430:
    b_1.pnp[*iptr] = a_1.jbp;
    goto L9000;
/*                                      EXITS */
/* ARG NOT ARRAY */
L8010:
    b_1.ibreak = TRUE_;
    b_1.errtyp = 21;
    goto L9000;
/* ARRAY INDEX OUT OF BOUNDS */
L8020:
    b_1.ibreak = TRUE_;
    b_1.errtyp = 28;
    b_1.arg = b_1.arg2;
L9000:
    return 0;
} /* arrutl_ */

#undef ipname
#undef jpname
#undef lword3
#undef lword2
#undef lword1
#undef lword
#undef llen3
#undef llen2
#undef llen1
#undef llen


/* Subroutine */ int init1_(void)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    double r_lg10(real *), pow(), log10();
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer i__;

#ifndef FORTRAN_LIB
    setup();
#endif

/*             AFTER CALLING INIT1 YOU MAY CALL ROLLIN INSTEAD OF INIT2 */
/*             THE FOLLOWING VARIABLES ARE MACHINE DEPENDENT AND ARE TO BE */
/*             SET BY THE IMPLEMENTOR OF LISPF4 */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*    a_1.natom = 3000;  */
/*    a_1.nfreet = 100000;  */
/*    a_1.nstack = 1500;  */
/*    a_1.nhtab = 4000;  */
/*    a_1.npname = 5000;  */
/*    jaan_1.hill = 1500;  */
    a_1.jbytes = 4;
    a_1.ibytes = 4;
    a_1.bytes = 4;
    b_1.lunin = 5;
    b_1.lunut = 6;
    b_1.lunsys = 4;
    b_1.maxlun = 99;
    b_1.iobuff = 160;
    a_1.nbytes = 256;
    a_1.chdiv = 16777216;
    a_1.maxbig = 2147483647;
/*           =2**31-1 */
    a_1.maxint = a_1.maxbig;
    a_1.iresol = 8;
    a_1.ipower = 50;
    a_1.fuzz = 5e-8f;
/* THE NEAREST 10**N LOWER THAN MAXBIG */
#ifdef FORTRAN_LIB
    r__1 = (real) a_1.maxbig;
    i__1 = (integer) r_lg10(&r__1) - 1;
    a_1.rmax = (real) pow_ii(&c__10, &i__1);
#else
    i__1 = (int) log10((double)a_1.maxbig) - 1;
    a_1.rmax = pow(10.0, (double) i__1);
#endif

/*             THE FOLLOWING VARIABLES BELONGS TO INIT1 BUT MAY NOT */
/*             BE CHANGED BY VARIOUS IMPLEMENTATIONS */
    b_1.nil = 1;
    a_1.nbmess = 40;
    a_1.maxmes = 40;
    a_1.nchtyp = 26;
    a_1.narea = 82;
/*               NAREA = THE LENGTH OF COMMON /B/ FROM ARG TO DREG(7) */
    a_1.nfreeb = a_1.natom + 1;
    b_1.lunins = b_1.lunin;
    b_1.lunuts = b_1.lunut;
    a_1.bignum = a_1.nfreet + a_1.natom;
    a_1.ismall = (a_1.maxint - a_1.bignum - 1) / 2;
    a_1.numadd = a_1.maxint - a_1.ismall;
    a_1.dpnp = a_1.npname - a_1.natom;
    a_1.dpname = a_1.dpnp - a_1.nfreet;
    a_1.bigmax = (real) a_1.maxbig;
/*                   NOW FOLLOW VARIABLE SETTINGS THAT */
/*                   ONLY HAVE TO BE DONE ONCE PER RUN. */
/* STACK */
    b_1.stack[0] = 17;
/* PNP */
    i__1 = a_1.natom;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L10: */
	b_1.pnp[i__ - 1] = 1;
    }
    a_1.garbs = 0;
    a_1.cgarbs = 0;
    a_1.agarbs = 0;
    a_1.ngarbs = 0;
    return 0;
} /* init1_ */

/* Subroutine */ int init2_(void)
{
    /* Format strings */
    static char fmt_5[] = "(1x,\002CANT'T OPEN 'SYSATOMS'\002)";

    /* System generated locals */
    integer i__1, i__2;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), s_wsfe(cilist *), e_wsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    static integer icar, ieof;
#define args ((integer *)&b_1.arg)  /* ((integer *)&b_1) */
    extern /* Subroutine */ int mess_(integer *);
#define isys ((integer *)&b_1.subr0)  /*  ((integer *)&b_1 + 34)  */
#define isys2 ((integer *)&b_1.a000)  /*  ((integer *)&b_1 + 12)  */
    static integer i__;
    extern integer iread_(integer *);
    extern /* Subroutine */ int setcht_(integer *, integer *);
#define ich ((integer *)&chars_1.space)  /*  ((integer *)&chars_1)  */
    extern /* Subroutine */ int rda1_(integer *, integer *, integer *, 
	    integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___50 = { 0, 0, 0, fmt_5, 0 };


/*             ALL MACHINE INDEPENDENT INITIALIZATIONS ARE DONE HERE */
/*             INIT2 MAY BE REPLACED BY A CALL TO ROLLIN */

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    a_1.natomp = 0;
    b_1.ip = 1;
    b_1.jp = a_1.nstack + 1;
    a_1.jbp = 1;
    a_1.numbp = a_1.npname + 1;
    b_1.abup1 = 0;
    b_1.lmargr = 1;
    b_1.lmarg = 1;
    b_1.margr = 80;
    b_1.marg = 80;
    b_1.levell = 1000;
    b_1.levelp = 1000;
    b_1.maxpar = 3;
    b_1.itabwg = 8;
    b_1.middl = a_1.nstack / 10;
    b_1.unused = -1001;

/*      BELOW IS CODE THAT OPENS SYSATOMS AT RUNTIME SO YOU DON'T NEED */
/*      TO ASSIGN IT. */

#ifdef  FORTRAN_LIB
    o__1.oerr = 1;
    o__1.ounit = b_1.lunsys;
    o__1.ofnmlen = 8;
    o__1.ofnm = c_b98;
    o__1.orl = 0;
    o__1.osta = c_b5;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    i__1 = f_open(&o__1);
#else
    i__1 = f4_open(b_1.lunsys, c_b98, "r");
#endif
    if (i__1 != 0) {
	goto L3;
    }
    goto L4;
L3:
#ifdef FORTRAN_LIB
    io___50.ciunit = b_1.lunuts;
    s_wsfe(&io___50);
    e_wsfe();
    s_stop(c_b3, (ftnlen)0);
#else
    fprintf(stderr, "Can't open 'SYSATOMS'\n");
    exit(1);
#endif



/*             READ SYSTEM CHARACTERS */
L4:
    rda1_(&b_1.lunsys, ich, &c__1, &a_1.nchtyp, &ieof);



/* DEBUG      WRITE(LUNUTS,29)(ICH(IIII),IIII=1,26) */
/* DEBUG29    FORMAT(' ICH=',26A1) */
/*             READ  ()<>"'..T+-0123456789%^E# */
/*             IF YOU GOT THE BCD-CODED VERSION, YOU MUST REPLACE THE */
/*             CHARACTERS FOR PROCENT, LEFT-BRACKET, RIGHT-BRACKET BY */
/*             SOMETHING ELSE */

/*             CLEAR HTAB */
    i__1 = a_1.nhtab;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L30: */
	b_1.htab[i__ - 1] = b_1.unused;
    }

/*             CLEAR CAR AND CDR AND MAKE FREE LISTS */
    a_1.nfreep = b_1.nil;
    i__1 = a_1.nfreeb;
    i__2 = a_1.nfreet;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
/* *SETC*      CALL SETCAR(I,NIL) */
	carcdr_1.car[i__ - 1] = b_1.nil;
/* *SETC*      CALL SETCDR(I,NFREEP) */
	carcdr_1.cdr[i__ - 1] = a_1.nfreep;
/* L180: */
	a_1.nfreep = i__;
    }
/*             INITIALIZE CHTAB */
    i__2 = a_1.nbytes;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
/* L200: */
	carcdr_1.chtab[i__ - 1] = 10;
    }
/*             PUT CHARACTER TYPES IN CHTAB */
    i__2 = a_1.nchtyp;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
/* L220: */
	setcht_(&ich[i__ - 1], &i__);
    }
    i__2 = b_1.iobuff;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
	b_1.buff[i__ - 1] = chars_1.space;
	b_1.rdbuff[i__ - 1] = chars_1.space;
	b_1.prbuff[i__ - 1] = chars_1.space;
/* L225: */
	b_1.abuff[i__ - 1] = chars_1.space;
    }
    b_1.prtpos = 1;
    b_1.rdpos = 1000;
    b_1.cht = 1;

    b_1.nargs = 10;
    b_1.numgen = 0;
    for (i__ = 1; i__ <= 7 || i__ == 1; ++i__) {
/* L250: */
	b_1.dreg[i__ - 1] = b_1.nil;
    }
    b_1.iflg1 = b_1.nil;
    b_1.iflg2 = b_1.nil;
    i__2 = b_1.nargs;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
/* L260: */
	args[i__ - 1] = b_1.nil;
    }
/*             USE LISP READ FUNCTION TO READ LISTS OF FUNCTION NAMES */
/*             FROM LUNSYS THUS CREATING ATOMS FOR THE SYS-FUNCTIONS */

/*             THE INTERVALS SUBR0 - FSUBR ARE EQUIVALENCE TO ISYS */

/*             SUBR0 = LAST FUNCTION WITH 0 ARGS */
/*             SUBR11= LAST FUNCTION WITH 1 NUMERICAL ARG */
/*             SUBR1 = LAST FUNCTION WITH 1 ARG (MAY NOT BE NUM ARG) */
/*             SUBR2 = LAST FUNCTION WITH 2 ARGS */
/*             SUBR3 = LAST FUNCTION WITH 3 ARGS */
/*             SUBR  = LAST FUNCTION WITH N ARGS */
/*             FSUBR= LAST FSUBR */
    b_1.lunin = b_1.lunsys;
    for (i__ = 1; i__ <= 7 || i__ == 1; ++i__) {
	icar = iread_(&c__0);
	isys[i__ - 1] = a_1.natomp;
/* DEBUG      WRITE(LUNUTS,500)NATOMP */
/* DEBUG500   FORMAT(' NATOMP =',I4) */
/* DEBUG      WRITE(LUNUTS,501)(PNAME(IIII),IIII=1,20) */
/* DEBUG501   FORMAT(4(' PNAME ',5A5,/)) */
/* L270: */
    }
/*             DEFINE SOME ATOM-INDEX SEPARATELY */
    for (i__ = 1; i__ <= 22 || i__ == 1; ++i__) {
/* L275: */
	isys2[i__ - 1] = iread_(&c__0);
    }
    b_1.lunin = b_1.lunins;
/*                                      SET CAR OF ATOMS */
    i__2 = b_1.nil;
    i__1 = a_1.natomp;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__) {
/* *SETC*280      CALL SETCAR(I, UNBOUN) */
/* L280: */
	carcdr_1.car[i__ - 1] = b_1.unboun;
    }
/* *SETC*      CALL SETCAR(NIL,NIL) */
    carcdr_1.car[b_1.nil - 1] = b_1.nil;
/* *SETC*      CALL SETCAR(T,T) */
    carcdr_1.car[b_1.t - 1] = b_1.t;
/*             INITIALIZE MESS (HAVE MESS TO READ MESSAGES FROM LUNSYS) */
    mess_(&c__0);
#ifdef FORTRAN_LIB
    cl__1.cerr = 0;
    cl__1.cunit = b_1.lunsys;
    cl__1.csta = 0;
    f_clos(&cl__1);
#else
    f4_close(b_1.lunsys);
#endif
    return 0;
} /* init2_ */

#undef ich
#undef isys2
#undef isys
#undef args


integer rollin_(integer *lun)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;
    static integer equiv_6[15];

    /* Local variables */
#define area  ((integer *)&b_1.arg)  /* ((integer *)&b_1) */
#define cinf (equiv_6)
#define acom  ((integer *)&a_1.narea)  /* ((integer *)&a_1)  */
#define bcom  ((integer *) &chars_1.space)  /*  ((integer *)&chars_1)  */
#define jbpo (equiv_6 + 10)
    extern /* Subroutine */ int move_(integer *, integer *, integer *);
    static integer i__;
    extern /* Subroutine */ int dmpin_(integer *, integer *, integer *, 
	    integer *);
    static integer idiff1, idiff2;
    extern /* Subroutine */ int dmpin2_(integer *, integer *, integer *, 
	    integer *);
#define jpname  ((integer *) b_1.pname)   /*  ((integer *)&b_1 + 1122)   */
#define natopo (equiv_6 + 8)
#define nfrepo (equiv_6 + 9)
#define numbpo (equiv_6 + 11)
#define nfreto (equiv_6 + 12)
#define numado (equiv_6 + 13)
#define npnamo (equiv_6 + 14)
    static integer inreal;
    extern /* Subroutine */ int rehash_(void);
    static integer max__;
    extern /* Subroutine */ int rew_(integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    inreal = a_1.bytes / a_1.jbytes;
    dmpin_(lun, cinf, &c__1, &c__15);
/*             CHECK IF ROLLIN POSSIBLE */
    for (i__ = 1; i__ <= 8 || i__ == 1; ++i__) {
	if (cinf[i__ - 1] != acom[i__ - 1]) {
	    goto L90;
	}
/* L1: */
    }
/*               ATOM SPACE */
    if (*natopo + 1 >= a_1.natom) {
	goto L90;
    }
/*               PRINT NAMES AND FLOATING NUMBERS */
    if ((*jbpo - 2) / a_1.bytes + 2 + *npnamo - *numbpo >= a_1.npname) {
	goto L90;
    }
/*               FREE STORAGE */
    if (*nfreto - *nfrepo >= a_1.nfreet - a_1.nfreeb) {
	goto L90;
    }
/*               ROLLIN POSSIBLE, MOVE POINTERS AND READ. */
    idiff1 = a_1.nfreet - *nfreto;
    idiff2 = a_1.numadd - *numado;
    a_1.natomp = *natopo;
    a_1.nfreep = *nfrepo + idiff1;
    a_1.jbp = *jbpo;
    a_1.numbp = *numbpo + a_1.npname - *npnamo;
    i__1 = a_1.nbmess / a_1.ibytes * a_1.maxmes;
    dmpin_(lun, b_1.imess, &c__1, &i__1);
    dmpin_(lun, area, &c__1, &a_1.narea);
/*                                      STACK IS USED PRIOR TO RESET. */
    b_1.ip = 1;
    b_1.jp = a_1.nstack + 1;
    i__1 = (a_1.jbp - 2) / a_1.jbytes + 1;
    dmpin2_(lun, jpname, &c__1, &i__1);
    i__1 = (a_1.numbp - 1) * inreal + 1;
    i__2 = a_1.npname * inreal;
    dmpin2_(lun, jpname, &i__1, &i__2);
    i__1 = a_1.natomp + 1;
    dmpin2_(lun, b_1.pnp, &c__1, &i__1);
/* *SETC*      CALL  DMPIN2(LUN,CARCDR,NIL,NATOMP) */
    dmpin2_(lun, carcdr_1.car, &b_1.nil, &a_1.natomp);
/* *SETC*C */
    dmpin2_(lun, carcdr_1.cdr, &b_1.nil, &a_1.natomp);
/* *SETC*      CALL  DMPIN2(LUN,CARCDR,NFREEP+1,NFREET) */
    i__1 = a_1.nfreep + 1;
    dmpin2_(lun, carcdr_1.car, &i__1, &a_1.nfreet);
/* *SETC*C */
    i__1 = a_1.nfreep + 1;
    dmpin2_(lun, carcdr_1.cdr, &i__1, &a_1.nfreet);
    dmpin_(lun, bcom, &c__1, &a_1.nchtyp);
    dmpin2_(lun, carcdr_1.chtab, &c__1, &a_1.nbytes);

/*             CHECK IF WE NEED TO MOVE POINTERS */

    if (idiff1 >= 0) {
	goto L22;
    }
/*      CALL FTYPE('MOVING 1') */
    i__1 = a_1.nfreep - idiff1;
    i__2 = a_1.bignum - idiff1;
    move_(&idiff1, &i__1, &i__2);
L22:
    if (idiff2 == 0) {
	goto L24;
    }
/*      CALL FTYPE('MOVING 2') */
    i__1 = a_1.bignum - idiff2;
    move_(&idiff2, &i__1, &a_1.maxint);
L24:
    if (idiff1 <= 0) {
	goto L30;
    }
/*      CALL FTYPE('MOVING 3') */
    i__1 = a_1.nfreep - idiff1;
    i__2 = a_1.bignum - idiff1;
    move_(&idiff1, &i__1, &i__2);

/*     MAKE FREE LIST */

L30:
    max__ = a_1.nfreep;
    a_1.nfreep = b_1.nil;
    i__1 = a_1.nfreeb;
    i__2 = max__;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
/* *SETC*      CALL SETCAR(I,NIL) */
	carcdr_1.car[i__ - 1] = b_1.nil;
/* *SETC*      CALL SETCDR(I,NFREEP) */
	carcdr_1.cdr[i__ - 1] = a_1.nfreep;
/* L35: */
	a_1.nfreep = i__;
    }

/*             REHASH THE ATOMS */

    rehash_();

/*               CLEAR BUFFERS */
    i__2 = b_1.iobuff;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
	b_1.abuff[i__ - 1] = chars_1.space;
	b_1.buff[i__ - 1] = chars_1.space;
	b_1.rdbuff[i__ - 1] = chars_1.space;
	b_1.prbuff[i__ - 1] = chars_1.space;
/* L60: */
    }
    b_1.prtpos = 1;
    b_1.rdpos = 1000;
    b_1.cht = 1;
    b_1.abup1 = 0;
    ret_val = *lun + a_1.numadd;
    goto L91;
L90:
    ret_val = b_1.nil;
L91:
    rew_(lun);
    return ret_val;
} /* rollin_ */

#undef npnamo
#undef numado
#undef nfreto
#undef numbpo
#undef nfrepo
#undef natopo
#undef jpname
#undef jbpo
#undef bcom
#undef acom
#undef cinf
#undef area


/* Subroutine */ int rollou_(integer *lun)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
#define area  ((integer *)&b_1.arg)  /* ((integer *)&b_1)  */
    extern integer garb_(integer *);
#define cinf  ((integer *)&a_1.narea)  /* ((integer *)&a_1)  */
#define bcom  ((integer *) &chars_1.space)  /*  ((integer *)&chars_1)  */
    static integer i__;
    extern /* Subroutine */ int dmpou2_(integer *, integer *, integer *, 
	    integer *);
#define jpname  ((integer *) b_1.pname)   /* ((integer *)&b_1 + 1122)  */
    static integer inreal;
    extern /* Subroutine */ int dmpout_(integer *, integer *, integer *, 
	    integer *), rew_(integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    inreal = a_1.bytes / a_1.jbytes;
/*             CALL COMPACTING GBC */
    i__ = garb_(&c__1);
    dmpout_(lun, cinf, &c__1, &c__15);
    i__1 = a_1.nbmess / a_1.ibytes * a_1.maxmes;
    dmpout_(lun, b_1.imess, &c__1, &i__1);
    dmpout_(lun, area, &c__1, &a_1.narea);
    i__1 = (a_1.jbp - 2) / a_1.jbytes + 1;
    dmpou2_(lun, jpname, &c__1, &i__1);
    i__1 = (a_1.numbp - 1) * inreal + 1;
    i__2 = a_1.npname * inreal;
    dmpou2_(lun, jpname, &i__1, &i__2);
    i__1 = a_1.natomp + 1;
    dmpou2_(lun, b_1.pnp, &c__1, &i__1);
/* *SETC*      CALL DMPOU2(LUN,CARCDR,NIL,NATOMP) */
    dmpou2_(lun, carcdr_1.car, &b_1.nil, &a_1.natomp);
/* *SETC*C */
    dmpou2_(lun, carcdr_1.cdr, &b_1.nil, &a_1.natomp);
/* *SETC*      CALL DMPOU2(LUN,CARCDR,NFREEP+1,NFREET) */
    i__1 = a_1.nfreep + 1;
    dmpou2_(lun, carcdr_1.car, &i__1, &a_1.nfreet);
/* *SETC*C */
    i__1 = a_1.nfreep + 1;
    dmpou2_(lun, carcdr_1.cdr, &i__1, &a_1.nfreet);
    dmpout_(lun, bcom, &c__1, &a_1.nchtyp);
    dmpou2_(lun, carcdr_1.chtab, &c__1, &a_1.nbytes);
    rew_(lun);
    return 0;
} /* rollou_ */

#undef jpname
#undef bcom
#undef cinf
#undef area


/* Subroutine */ int move_(integer *diff, integer *min__, integer *max__)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
#define args  ((integer *)&b_1.arg)  /* ((integer *)&b_1)  */
    static integer i__, j, i1, i2;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */

/*             USED BY ROLLIN TO ADD DIFF TO POINTERS POINTING */
/*             IN (.GT.MIN , .LE.MAX) */
    i1 = b_1.nil;
    i2 = a_1.natomp;
    for (j = 1; j <= 2 || j == 1; ++j) {
	i__1 = i1;
	i__2 = i2;
	for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
/* *SETC*      IF(CAR(I).GT.MIN.AND.CAR(I).LE.MAX) CALL SETCAR(I,CAR(I)+DIFF) */
	    if (carcdr_1.car[i__ - 1] > *min__ && carcdr_1.car[i__ - 1] <= *
		    max__) {
		carcdr_1.car[i__ - 1] = carcdr_1.car[i__ + *diff - 1];
	    }
/* *SETC*      IF(CDR(I).GT.MIN.AND.CDR(I).LE.MAX) CALL SETCDR(I,CDR(I)+DIFF) */
	    if (carcdr_1.cdr[i__ - 1] > *min__ && carcdr_1.cdr[i__ - 1] <= *
		    max__) {
		carcdr_1.cdr[i__ - 1] = carcdr_1.cdr[i__ + *diff - 1];
	    }
/* L10: */
	}
	i1 = a_1.nfreep;
/* L20: */
	i2 = a_1.nfreet;
    }
    i__2 = b_1.nargs;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
	if (args[i__ - 1] > *min__ && args[i__ - 1] <= *max__) {
	    args[i__ - 1] += *diff;
	}
/* L50: */
    }
    return 0;
} /* move_ */

#undef args


/* Subroutine */ int prin1_(integer *s)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ccol, idiv, retc, iret, retu;
    extern /* Subroutine */ int apop2_(integer *, integer *), apop3_(integer *
	    , integer *, integer *);
    static integer i__, x, y, glnkw, gllev, jpold, jpcom, jpsav, x2;
    extern /* Subroutine */ int apush2_(integer *, integer *), apush3_(
	    integer *, integer *, integer *);
    static integer ic, li, gllbef, xx, glcoun, levelm;
    extern /* Subroutine */ int terpri_(void);
    static integer ldepth, lprbrk, llmarg;
    extern /* Subroutine */ int prinat_(integer *, integer *, integer *);
    extern integer get_(integer *, integer *);

/* ----- */

/*     THE GENERAL PRINT ROUTINE */

/* ----- */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */

/* --------------------------------------MAIN ENTRY */
    x = *s;
    apush2_(&b_1.ip, &b_1.lmarg);
/*                                      STACK OVERFLOW PREVENTION */
    if (b_1.stack[b_1.ip - 1] == 16) {
	return 0;
    }
    idiv = 3;
    if (b_1.dreg[1] != b_1.nil) {
	idiv = 5;
    }
    levelm = (b_1.jp - b_1.ip) / idiv - 1;
    if (b_1.levelp < levelm) {
	levelm = b_1.levelp;
    }
    jpold = b_1.jp;
    gllev = 0;
    ccol = (b_1.marg << 1) / 3;
    jpcom = -10;
    x2 = b_1.nil;
    retu = 2;
    goto L1000;
/* --------------------------------------RETURN */
/* --TERMINAL INTERRUPT (FROM UNKWOTE) */
L18:
    terpri_();
/* --FROM PRINT-S */
L20:
    b_1.jp = jpold;
    apop2_(&b_1.lmarg, &b_1.ip);
    return 0;

/* --------------------------------------ENTRY LASTDEPTH(X) */
L500:
    ldepth = gllev;
    retu = 1;
/* --FROM UNKWOTE */
L510:
    if (ldepth == levelm || (x <= a_1.natom || x > a_1.nfreet)) {
	goto L550;
    }
    ++ldepth;
/*                                      X := LAST(X) */
    i__1 = b_1.levell;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	y = carcdr_1.cdr[x - 1];
	if (y == b_1.nil) {
	    goto L530;
	}
	x = y;
	if (y <= a_1.natom || y > a_1.nfreet) {
	    goto L510;
	}
/* L520: */
    }
    goto L550;
/*                                      X := UNKWOTE(CAR(X)) */
L530:
    x = carcdr_1.car[x - 1];
    goto L1000;
/*                                      RETURN(LDEPTH) */
L550:
    iret = ldepth - gllev;
    goto L5020;

/* --------------------------------------ENTRY UNKWOTE(X) */
L1000:
    if (b_1.ibreak) {
	goto L18;
    }
    glcoun = 0;
    if (b_1.dreg[2] == b_1.nil) {
	goto L1010;
    }
L1005:
    if (x <= a_1.natom || x > a_1.nfreet) {
	goto L1010;
    }
    y = carcdr_1.cdr[x - 1];
    if (carcdr_1.car[x - 1] != b_1.quote || (y <= a_1.natom || y > a_1.nfreet)
	    ) {
	goto L1010;
    }
    if (carcdr_1.cdr[y - 1] != b_1.nil) {
	goto L1010;
    }
    ++glcoun;
    x = carcdr_1.car[y - 1];
    goto L1005;
L1010:
    if (retu == 2 || retu == 4) {
	glnkw = glcoun;
    }
    switch (retu) {
	case 1:  goto L510;
	case 2:  goto L2000;
	case 3:  goto L1504;
	case 4:  goto L5105;
    }

/* --------------------------------------ENTRY NCHARS(X) */
L1500:
    jpsav = b_1.jp;
    retu = 3;
/*                                      COUNT THE QUOTES. */
    goto L1000;
/* --FROM UNKWOTE */
L1504:
    iret += glcoun;
/*                                      GONE PAST RIGHT MARGIN ? */
    if (iret > b_1.marg) {
	goto L1580;
    }
/*                                      A LITATOM ? */
    if (x > a_1.natom) {
	goto L1520;
    }
    if (carcdr_1.car[x - 1] == b_1.array) {
	goto L1530;
    }
    iret = iret + (b_1.pnp[x] - b_1.pnp[x - 1]) + 1;
    goto L1590;
/*                                      A NUMBER ? */
L1520:
    if (x > a_1.nfreet) {
	goto L1530;
    }
/*                                      DEEP ENOUGH ? */
    if (gllev + (jpsav - b_1.jp) / 2 < levelm) {
	goto L1550;
    }
L1530:
    iret += 4;
    goto L1590;
/*                                      GO DOWN CAR DIRECTION */
L1550:
    b_1.jp += -2;
    b_1.stack[b_1.jp] = 0;
L1555:
    b_1.stack[b_1.jp - 1] = x;
    x = carcdr_1.car[x - 1];
    goto L1000;
/*                                      ... AND CDR DIRECTION */
L1560:
    x = b_1.stack[b_1.jp - 1];
    if (x < b_1.nil) {
	goto L1585;
    }
    x = carcdr_1.cdr[x - 1];
/*                                      SIMULATE A GRAPHIC  ---  ? */
    ++b_1.stack[b_1.jp];
    if (b_1.stack[b_1.jp] >= b_1.levell) {
	x = a_1.numadd;
    }
    if (x > a_1.natom && x <= a_1.nfreet) {
	goto L1555;
    }
    b_1.stack[b_1.jp - 1] = -1;
    goto L1000;
/*                                      BACKUP */
L1580:
    b_1.jp = jpsav - 2;
L1585:
    b_1.jp += 2;
L1590:
    if ((i__1 = jpsav - b_1.jp) < 0) {
	goto L1580;
    } else if (i__1 == 0) {
	goto L1591;
    } else {
	goto L1560;
    }
/*                                      EXIT */
L1591:
    if (jpcom <= 0) {
	goto L5032;
    } else {
	goto L3001;
    }

/* --------------------------------------ENTRY PRINT-S(X) */

/* --FROM UNKWOTE                        DEEP ENOUGH ? */
L2000:
    if (gllev >= levelm && (x > a_1.natom && x <= a_1.nfreet)) {
	x = -1;
    }
    if (x <= a_1.natom || x > a_1.nfreet) {
	goto L2030;
    }
/*                                      PRINT-L(X,X2) */
/* L2010: */
    if (x2 > a_1.numadd) {
	--x2;
    }
    if (b_1.dreg[1] != b_1.nil) {
	apush2_(&lprbrk, &llmarg);
    }
    apush3_(&li, &x2, &x);
    if (b_1.jp == jpcom) {
	b_1.dreg[1] = b_1.nil;
    }
    goto L5000;
/* --FROM PRINT-L */
L2020:
    if (b_1.jp == jpcom) {
	b_1.dreg[1] = b_1.t;
    }
    apop3_(&x, &x2, &li);
    if (b_1.dreg[1] != b_1.nil) {
	apop2_(&llmarg, &lprbrk);
    }
    gllbef = b_1.t;
    if (b_1.jp - idiv != jpcom) {
	goto L2040;
    }
    jpcom = -10;
    if (b_1.lmarg == 10) {
	goto L2500;
    }
/* --FROM TERPRI2 */
L2025:
    b_1.lmarg = xx;
    goto L2040;
/*                                      PRINAT(X) */
L2030:
    prinat_(&x, &glnkw, &jpold);
    gllbef = b_1.nil;
L2040:
    if (gllev <= 0) {
	goto L20;
    } else {
	goto L5120;
    }

/* --------------------------------------ENTRY TERPRI2 */
L2500:
    for (i__ = 1; i__ <= 2 || i__ == 1; ++i__) {
/* L2510: */
	terpri_();
    }
    if (jpcom <= 0) {
	goto L2025;
    } else {
	goto L3040;
    }

/* --------------------------------------ENTRY LINEBREAK(X) */
L3000:
    if (lprbrk == -1) {
	goto L3030;
    }
/*                                      COMMENT NEXT ? */
    if (x <= a_1.natom || x > a_1.nfreet) {
	goto L3010;
    }
    xx = carcdr_1.car[x - 1];
    if (xx > a_1.natom) {
	goto L3010;
    }
    xx = get_(&xx, &b_1.fncell);
    if (carcdr_1.cdr[xx - 1] != b_1.quote) {
	goto L3010;
    }
/*                                      SWITCH TO COMMENT MODE */
    jpcom = b_1.jp - idiv;
    xx = b_1.lmarg;
    b_1.lmarg = ccol;
    iret = 20 - b_1.marg;
/*                                      CALL NCHARS(X) */
    goto L1500;
/* --FROM NCHARS */
L3001:
    x = b_1.stack[b_1.jp - 1];
    x = carcdr_1.car[x - 1];
    if (iret <= b_1.marg) {
	goto L3025;
    }
    b_1.lmarg = 10;
    goto L2500;
/*                                      NOT COMMENT */
L3010:
    if (li < 2 || lprbrk != 1) {
	goto L3020;
    }
    if (x > a_1.natom && x <= a_1.nfreet) {
	goto L3025;
    }
/*                                      PROG--LABEL */
    terpri_();
    b_1.prtpos = b_1.lmarg - 5;
    goto L3040;
/*                                      NOT PROG. */
L3020:
    if (! (x > a_1.natom && x <= a_1.nfreet || gllbef != b_1.nil)) {
	goto L3030;
    }
L3025:
    if (b_1.prtpos >= b_1.lmarg) {
	terpri_();
    }
    b_1.prtpos = b_1.lmarg;
    goto L3040;
/*                                      (SPACES 1) */
L3030:
    ++b_1.prtpos;
/* --FROM TERPRI2                        RETURN */
L3040:
    goto L5110;

/* --------------------------------------ENTRY PRINT-C */
L3500:
    if (b_1.prtpos >= b_1.marg + 1) {
	terpri_();
    }
    b_1.prbuff[b_1.prtpos - 1] = ic;
    ++b_1.prtpos;
    switch (retc) {
	case 1:  goto L5030;
	case 2:  goto L5210;
	case 3:  goto L5010;
    }

/* --------------------------------------ENTRY PRINT-L */

/* THE VARIABLE LPRBRK CARRIES INFORMATION ABOUT WHEN TO PERFORM */
/* TERPRI AND WHEN NOT.  IT IS TESTED BY LINEBREAK.  IT CONTAINS */
/* THE FOLLOWING INFO: */

/* <0       MEANS THAT LINEBREAK WILL NOT PERFORM ANY TERPRI-ES. */
/*           THIS WILL BE THE CASE IF  E I T H E R */
/*           1) THE EXPRESSION ALMOST CERTAINLY WILL FIT ON LINE,  O R */
/*           2) NO PARENTHESISES WILL BE VISIBLE INSIDE THIS */
/*              EXPRESSION, DUE TO THE CURRENT PRINTLEVEL. */
/* >0  INFO ABOUT CAR(CURRENT EXPRESSION) */
/*    = 0    ATOM */
/*    = 1    THE ATOM PROG */
/*    = 2    A LIST */

L5000:
    li = 0;
    llmarg = b_1.lmarg;
    lprbrk = -1;
/*                                      PRINT ZERO OR MORE '-S */
    ic = chars_1.iqchr;
    retc = 3;
/* --FROM PRINT-C */
L5010:
    if (glnkw == 0) {
	goto L5015;
    }
    --glnkw;
    goto L3500;
L5015:
    ic = chars_1.lpar;
    retc = 1;
    if (x2 != b_1.nil) {
	goto L3500;
    }
/*                                      X2=NIL, CALL LASTDEPTH */
    if (b_1.dreg[1] != b_1.nil) {
	goto L500;
    }
    iret = 0;
    goto L5025;
/* --FROM LASTDEPTH */
L5020:
    if (iret > b_1.maxpar) {
	ic = chars_1.ilbchr;
    }
    if (iret <= b_1.maxpar) {
	iret = 0;
    }
L5025:
    b_1.stack[b_1.jp] = iret + a_1.numadd;
    goto L3500;
/* --FROM PRINT-C                        WE HAVE PRINTED ( OR < */
L5030:
    if (b_1.dreg[1] == b_1.nil) {
	goto L5050;
    }
/*                                      TEST IF X WILL FIT ON LINE */
    if (gllev + 1 >= levelm) {
	goto L5040;
    }
    if (b_1.dreg[6] != b_1.nil) {
	goto L5035;
    }
/*                                      CALL NCHARS(X) */
    x = b_1.stack[b_1.jp - 1];
    iret = b_1.prtpos;
    goto L1500;
/* --FROM NCHARS */
L5032:
    if (iret <= b_1.marg) {
	goto L5040;
    }
L5035:
    x = b_1.stack[b_1.jp - 1];
    x = carcdr_1.car[x - 1];
    lprbrk = 0;
    if (x == b_1.prog) {
	lprbrk = 1;
    }
    if (x > a_1.natom && x <= a_1.nfreet) {
	lprbrk = 2;
    }
L5040:
    b_1.lmarg = b_1.prtpos + 1;
    if (b_1.lmarg > b_1.marg - 3) {
	b_1.lmarg = b_1.marg - 3;
    }
L5050:
    ++gllev;
/*                                      THE 'BIG' LOOP */
L5100:
    x = b_1.stack[b_1.jp - 1];
    x = carcdr_1.car[x - 1];
    retu = 4;
    goto L1000;
/* --FROM UNKWOTE */
L5105:
    if (li > 0) {
	goto L3000;
    }
/* --FROM LINEBREAK                      CALL PRINT-S */
L5110:
    x2 = b_1.stack[b_1.jp - 1];
    b_1.stack[b_1.jp - 1] = carcdr_1.cdr[x2 - 1];
    x2 = b_1.nil;
    if (b_1.stack[b_1.jp - 1] == b_1.nil) {
	x2 = b_1.stack[b_1.jp];
    }
    goto L2000;
/* --FROM PRINT-S */
L5120:
    ++li;
/*                                      PERHAPS MOVE LMARG FURTHER RIGHT */
    if (li != 1 || (lprbrk + 2) / 2 != 1) {
	goto L5130;
    }
    if (b_1.prtpos < (b_1.marg + b_1.itabwg * b_1.lmarg) / (b_1.itabwg + 1)) {
	b_1.lmarg = b_1.prtpos + 1;
    }
/*                                      TAKE NEXT ELEMENT OF LIST */
L5130:
    x = b_1.stack[b_1.jp - 1];
    if (x == b_1.nil) {
	goto L5200;
    }
    if (x > a_1.natom && x <= a_1.nfreet) {
	goto L5150;
    }
/*                                      ATOM IS NEXT */
    if (b_1.prtpos + 2 >= b_1.marg) {
	terpri_();
    }
    b_1.prbuff[b_1.prtpos] = chars_1.dot;
    b_1.prtpos += 3;
    goto L5160;
/*                                      LIST IS NEXT */
L5150:
    if (li < b_1.levell) {
	goto L5100;
    }
    x = -2;
    ++b_1.prtpos;
/*                                      CALL PRINAT */
L5160:
    prinat_(&x, &glnkw, &jpold);
/*                                      PRINT ) OR > */
L5200:
    x2 = b_1.stack[b_1.jp] - a_1.numadd;
    if (x2 < 0 || x2 > 1) {
	goto L5210;
    }
    ic = chars_1.rpar;
    retc = 2;
    if (x2 == 1) {
	ic = chars_1.irbchr;
    }
    goto L3500;
/* --FROM PRINT-C */
L5210:
    --gllev;
    b_1.lmarg = llmarg;
    goto L2020;

} /* prin1_ */

/* Subroutine */ int prinat_(integer *x, integer *nkw, integer *jpold)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static integer main, iret, i__, j, l;
    static logical clear;
    extern /* Subroutine */ int getch_(real *, integer *, integer *);
    extern integer getpn_(integer *, integer *, integer *, integer *);
    static integer ic, jb, jj, oldpos;
    extern integer getcht_(integer *);
    extern /* Subroutine */ int priint_(integer *), priflo_(real *);
    extern doublereal gtreal_(integer *, integer *);
    static integer newpos;
    extern /* Subroutine */ int terpri_(void);
    static integer ipl, isi;

/* ----- */

/*     PRINAT WILL PRINT A LISP ATOM OR: */

/*     X = -1  ==>  THE SYMBOL  ... */
/*       = -2  ==>              --- */

/*     NKW IS THE NUMBER OF '-S TO PRINT */

/*     JPOLD IS A SAVED STACK POINTER, IN CASE OF ERROR */
/* ----- */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */

    clear = b_1.prtpos == 1;
    oldpos = b_1.prtpos;
/* ***                                   DELETED STATEMENT(S). *** */
    ipl = 0;
    if (*nkw == 0) {
	goto L150;
    }
    i__1 = *nkw;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	b_1.prbuff[b_1.prtpos - 1] = chars_1.iqchr;
/* L100: */
	++b_1.prtpos;
    }
L150:
    if (*x >= b_1.nil) {
	goto L200;
    }
/*                                      ... OR --- */
    isi = 3;
    ipl = 3;
    ic = chars_1.dot;
    if (*x == -2) {
	ic = chars_1.iminus;
    }
    goto L300;
L200:
    if (*x <= a_1.natom) {
	goto L295;
    }
/*                                      --- NUMBER --- */
    if (*x > a_1.bignum) {
	i__1 = *x - a_1.numadd;
	priint_(&i__1);
    }
    if (*x <= a_1.bignum) {
	r__1 = gtreal_(x, &iret);
	priflo_(&r__1);
    }
    goto L900;
/*                                      --- LITERAL --- */
/*                                      PICK UP ITS LENGTH */
L295:
    if (carcdr_1.car[*x - 1] != b_1.array) {
	goto L296;
    }
    b_1.prbuff[b_1.prtpos - 1] = chars_1.nochar;
    ++b_1.prtpos;
    priint_(x);
    goto L900;
L296:
    isi = getpn_(x, &main, &jb, &ipl) + 1;
    if (isi == 0) {
	goto L9100;
    }
    if (isi == 2 && b_1.dreg[4] == b_1.nil) {
	isi = 1;
    }
    if (isi == 1) {
	goto L300;
    }
    b_1.prbuff[b_1.prtpos - 1] = chars_1.strchr;
    ++b_1.prtpos;
L300:
    if (ipl == 0) {
	goto L850;
    }
    if (b_1.prtpos > b_1.iobuff - 3) {
	goto L910;
    }
L310:
    if (isi > 2) {
	goto L700;
    }
/*                                      GET CHAR */
    getch_(b_1.pname, &ic, &jb);
    ++jb;
    if (b_1.dreg[4] == b_1.nil) {
	goto L700;
    }
/*                                      GET TYPE */
    jj = getcht_(&ic);
/*                                      % MAY BE NEEDED */
    switch (isi) {
	case 1:  goto L400;
	case 2:  goto L500;
    }
L400:
    if (jj <= 8 || jj == 23) {
	goto L600;
    }
    if (jj == 9 && *x == b_1.dotat) {
	goto L600;
    }
    goto L700;
L500:
    if (jj != 6 && jj != 23) {
	goto L700;
    }
L600:
    b_1.prbuff[b_1.prtpos - 1] = chars_1.atend;
    ++b_1.prtpos;
L700:
    b_1.prbuff[b_1.prtpos - 1] = ic;
    ++b_1.prtpos;
    --ipl;
    goto L300;
/*                                      ATOM NOW STORED. OFLO TEST. */
L850:
    if (isi != 2) {
	goto L900;
    }
    b_1.prbuff[b_1.prtpos - 1] = chars_1.strchr;
    ++b_1.prtpos;
L900:
    if (b_1.prtpos <= b_1.marg + 1) {
	goto L1200;
    }
/*                                      YES, IT HAS OVERFLOWED. */
L910:
    newpos = b_1.prtpos;
/*                                      OUTPUT ANY OLD INFO */
    if (clear) {
	goto L915;
    }
    clear = TRUE_;
    b_1.prtpos = oldpos;
    terpri_();
/*               CLEAR RIGHT MARGIN BY MOVING TEXT LEFT */
L915:
    l = newpos - oldpos;
    b_1.prtpos = b_1.marg + 1 - l;
    if (b_1.prtpos > b_1.lmarg) {
	b_1.prtpos = b_1.lmarg;
    }
    if (b_1.prtpos < 1) {
	b_1.prtpos = 1;
    }
    i__1 = l;
    for (j = 1; j <= i__1 || j == 1; ++j) {
	if (b_1.prtpos <= b_1.marg) {
	    goto L920;
	}
	terpri_();
	b_1.prtpos = 1;
L920:
	if (b_1.prtpos == oldpos) {
	    goto L925;
	}
	b_1.prbuff[b_1.prtpos - 1] = b_1.prbuff[oldpos - 1];
	b_1.prbuff[oldpos - 1] = chars_1.space;
L925:
	++b_1.prtpos;
/* L930: */
	++oldpos;
    }
    oldpos = b_1.prtpos - l;
    if (oldpos < 1) {
	oldpos = 1;
    }
/*                                      PERHAPS JUMP BACK TO THE LOOP */
/* L950: */
    if (ipl > 0) {
	goto L310;
    }
/*                                      READY. RETURN. */
L1200:
    return 0;
/*                                      ERROR: INVALID SUBSTRING */
L9100:
    *x = b_1.nil;
    b_1.errtyp = 29;
    b_1.ibreak = TRUE_;
    goto L295;
} /* prinat_ */

/* Subroutine */ int priflo_(real *r__)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double r_mod(real *, real *);

    /* Local variables */
    static integer ipos0, i__;
    static real o, s, u;
    static integer limit, ie;
    extern /* Subroutine */ int priint_(integer *);
    static integer len, num;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    ie = 0;
    if (*r__ < 0.f) {
	goto L2;
    } else if (*r__ == 0) {
	goto L9;
    } else {
	goto L3;
    }
L2:
    *r__ = -(*r__);
    b_1.prbuff[b_1.prtpos - 1] = chars_1.iminus;
    ++b_1.prtpos;
/*                                      CHOOSE E OR F FORMAT */
L3:
    s = *r__;
    if (*r__ >= 1.f) {
	goto L45;
    }
/*                                      R .LT. 1. */
L41:
    if (-ie >= a_1.ipower || s >= 1.f) {
	goto L44;
    }
    --ie;
    s *= 10.f;
    goto L41;
L44:
    if (-ie <= 3) {
	ie = 0;
    }
    goto L50;
/*                                      R .GE. 1. */
L45:
    if (ie >= a_1.ipower || s < 10.f) {
	goto L46;
    }
    ++ie;
    s /= 10.f;
    goto L45;
L46:
    u = s;
    i__1 = a_1.iresol;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	if (u >= 1.f) {
	    len = i__;
	}
/* L48: */
#ifdef FORTRAN_LIB
	u = r_mod(&u, &c_b239) * 10.f;
#else
	u = fmod((double)u, (double) c_b239) * 10.0;
#endif
    }
/* *** CHANGED BY TR */
    if (ie < a_1.iresol) {
	ie = o;
    }
/*                                      NORMALIZE */
L50:
    if (ie != 0) {
	*r__ = s;
    }
    *r__ *= a_1.fuzz / s + 1.f;
    num = *r__;
    limit = b_1.prtpos + a_1.iresol + 1;
/* OUTPUT NUMBER OF INTEGERS WHICH CAN BE HANDELED BY PRIINT */
/* IF 10**IRESOL GT LARGEST INTEGER PRIINT MUST BE CALLED IN LOOP */
L51:
    if (*r__ < (real) a_1.maxbig) {
	goto L52;
    }
    num = (integer) (*r__ / a_1.rmax);
    priint_(&num);
#ifdef FORTRAN_LIB
    *r__ = r_mod(r__, &a_1.rmax);
#else
    *r__ = fmod((double)*r__, (double)a_1.rmax);
#endif
    goto L51;
L52:
    num = *r__;
    if (num != 0) {
	priint_(&num);
    }
/* ***                                   DELETED STATEMENT(S). *** */
    b_1.prbuff[b_1.prtpos - 1] = chars_1.dot;
    ++b_1.prtpos;
    ipos0 = b_1.prtpos;
L6:
#ifdef FORTRAN_LIB
    *r__ = r_mod(r__, &c_b239) * 10.f;
#else
    *r__ = fmod((double)*r__, (double) c_b239) * 10.0;
#endif
    if (*r__ == 0.f || b_1.prtpos >= limit) {
	goto L7;
    }
    num = *r__;
    b_1.prbuff[b_1.prtpos - 1] = chars_1.ifig[num];
    ++b_1.prtpos;
    if (num > 0) {
	ipos0 = b_1.prtpos;
    }
    goto L6;
/*                                      STRIP ZEROS */
L7:
    if (ipos0 == b_1.prtpos) {
	goto L8;
    }
    --b_1.prtpos;
    b_1.prbuff[b_1.prtpos - 1] = chars_1.space;
    goto L7;
/*                                      EXP. PART */
L8:
    if (ie == 0) {
	return 0;
    }
    b_1.prbuff[b_1.prtpos - 1] = chars_1.echar;
    ++b_1.prtpos;
L9:
    priint_(&ie);
    return 0;
} /* priflo_ */

/* Subroutine */ int priint_(integer *inum)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, k, jj, isi, num;

/*                                      PRINT (-)INTEGER */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    num = *inum;
/* L100: */
    if (num >= 0) {
	goto L101;
    }
    b_1.prbuff[b_1.prtpos - 1] = chars_1.iminus;
    ++b_1.prtpos;
    num = -num;
L101:
    isi = b_1.prtpos + 19;
/*                                      THIS LOOP OUTPUTS DIGITS */
    for (i__ = 1; i__ <= 19 || i__ == 1; ++i__) {
	jj = num % 10 + 1;
	b_1.prbuff[isi - 1] = chars_1.ifig[jj - 1];
	num /= 10;
	if (num == 0) {
	    goto L280;
	}
/* L270: */
	--isi;
    }
L280:
    k = b_1.prtpos + 19;
/*                                      THIS LOOP POSITIONS THEM */
    i__1 = isi;
    i__2 = k;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	b_1.prbuff[b_1.prtpos - 1] = b_1.prbuff[i__ - 1];
	b_1.prbuff[i__ - 1] = chars_1.space;
/* L290: */
	++b_1.prtpos;
    }
    return 0;
} /* priint_ */

/* Subroutine */ int terpri_(void)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, k;
    extern integer matom_(integer *);
    extern /* Subroutine */ int putch_(integer1 *, integer *, integer *);
    extern /* Subroutine */ int wra1_(integer *, integer *, integer *, 
	    integer *);

/*             FIRST TEST IF CALLED FROM PRIN1 */
/*             IF NOT, DO NOT WRITE ON LUNUT */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    k = b_1.prtpos - 1;
    if (b_1.iflg2 == b_1.t) {
	goto L10;
    }
    if (b_1.iflg1 != b_1.nil) {
	goto L20;
    }
    if (k < 1) {
	k = 1;
    }
    wra1_(&b_1.lunut, b_1.prbuff, &c__1, &k);
    i__1 = k;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L1: */
	b_1.prbuff[i__ - 1] = chars_1.space;
    }
/* L3: */
    b_1.prtpos = b_1.lmarg;
    return 0;
/*                                      CALLED BY CONCAT */
L10:
    i__1 = -k;
    if (b_1.nil == matom_(&i__1)) {
	goto L22;
    }
    --a_1.natomp;
/* L11: */
    i__1 = k;
    for (i__ = 1; i__ <= i__1; ++i__) {  /* fix CONCAT not to make minimum number of characters 1  */
	putch_((char *)b_1.pname, &b_1.prbuff[i__ - 1], &a_1.jbp);
	b_1.prbuff[i__ - 1] = chars_1.space;
/* L12: */
	++a_1.jbp;
    }
    b_1.pnp[a_1.natomp] = a_1.jbp;
    b_1.prtpos = b_1.lmarg;
    return 0;
/*                                      CALLED BY NCHARS */
L20:
    b_1.iflg1 += k;
L22:
    i__1 = k;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L24: */
	b_1.prbuff[i__ - 1] = chars_1.space;
    }
    b_1.prtpos = 1;
    return 0;
} /* terpri_ */


/* Subroutine */ int iprint_(integer *i__)
{
    extern /* Subroutine */ int prin1_(integer *), terpri_(void);

    prin1_(i__);
    terpri_();
    return 0;
} /* iprint_ */

integer iread_(integer *n)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    extern integer cons_(integer *, integer *);
    extern /* Subroutine */ int apop2_(integer *, integer *);
    static integer i__;
#define x ((integer *)&b_1.temp1)  /*  ((integer *)&b_1 + 5)  */
    static integer y;
#define brstk  ((integer *)&b_1.temp3)  /*  ((integer *)&b_1 + 7)  */
    extern integer ratom_(integer *, integer *);
    extern /* Subroutine */ int fpush_(integer *);
#define s1  ((integer *)&b_1.temp2)  /*  ((integer *)&b_1 + 6)  */
    extern /* Subroutine */ int apush2_(integer *, integer *);
    static integer it, sn;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/* ----- */
/*     IMPORTANT VARIABLES: */

/*     X         ATOM RETURNED BY  RATOM. */
/*               ALSO USED AS RETURN VALUE FROM RECURSIVE CALLS */
/*     IT        TYPE OF ATOM RETURNED BY  RATOM */
/*     S1        BEGINNING OF SUBLIST (LOCAL TO  LIST-L) */
/*     SN        LAST ELEMENT OF SUBLIST  S1  (LOCAL TO LIST-L) */
/* ----- */
/*                                      INITIALIZE */
    b_1.brlev = a_1.numadd;
    *brstk = b_1.nil;
    b_1.brflg = b_1.nil;
/*                                      STACK RETURN ADDRESS */
    fpush_(&c__1);
    goto L190;
/* --R1 */
/*                                      NORMAL RETURN FROM IREAD */
L100:
    ret_val = *x;
    return ret_val;

/*                                      RECURSIVE RETURN */

L900:
    i__ = b_1.stack[b_1.ip - 1];
    if (i__ > 4) {
	goto L9000;
    }
    --b_1.ip;
    switch (i__) {
	case 1:  goto L100;
	case 2:  goto L220;
	case 3:  goto L310;
	case 4:  goto L340;
    }

/* --------------------------------------RECURSIVE PROCEDURE READ-S */

L190:
    it = ratom_(x, &c__1);
/* DEBUG      WRITE(LUNUTS,191) IT */
/* DEBUG191   FORMAT(' IT AT 190 = ',I4) */
L200:
    switch (it) {
	case 1:  goto L900;
	case 2:  goto L290;
	case 3:  goto L230;
	case 4:  goto L210;
	case 5:  goto L900;
    }
/*            A   (   )   '   . */
L210:
    fpush_(&c__2);
    ++b_1.brlev;
    goto L190;
/* --R2 */
L220:
    --b_1.brlev;
    i__1 = cons_(x, &b_1.nil);
    *x = cons_(&b_1.quote, &i__1);
    goto L900;
/*                                      ') OR '> ENCOUNTERED */
L230:
    ++b_1.brlev;
    goto L900;

/* --------------------------------------RECURSIVE PROCEDURE READ-L */

L290:
    *s1 = b_1.nil;
    sn = b_1.nil;
L300:
    it = ratom_(x, &c__1);
/*                                      E-O-LIST */
    if (it == 3) {
	goto L390;
    }
/*                                      APPLY READ-S TO X */
/*                                      AND SET X = CONS(RESULT,NIL) */
    if (it == 1 || it == 5) {
	goto L320;
    }
    fpush_(&c__3);
    apush2_(s1, &sn);
    goto L200;
/* --R3 */
L310:
    apop2_(&sn, s1);
L320:
    *x = cons_(x, &b_1.nil);
/*                                      JUST INITIALIZE IF AT */
/*                                      BEGINNING OF LIST */
    if (*s1 == b_1.nil) {
	goto L330;
    }
/*                                      TAKE CARE OF TRUE DOT */
    if (it != 5 || carcdr_1.car[*x - 1] != b_1.dotat) {
	goto L360;
    }
    fpush_(&c__4);
    apush2_(s1, &sn);
L330:
    *s1 = *x;
    sn = *x;
    goto L300;
/* --R4 */
L340:
    apop2_(&sn, s1);
    y = carcdr_1.cdr[*x - 1];
    if (y <= a_1.natom || y > a_1.nfreet) {
	goto L350;
    }
    if (carcdr_1.cdr[y - 1] == b_1.nil) {
	*x = carcdr_1.car[y - 1];
    }
/* *SETC*350   CALL SETCDR(SN, X) */
L350:
    carcdr_1.cdr[sn - 1] = *x;
    goto L390;
/*                                      JUST APPEND X TO SN */
/*                                      AND GO ON LOOPING */
/* *SETC*360   CALL SETCDR(SN, X) */
L360:
    carcdr_1.cdr[sn - 1] = *x;
    sn = *x;
    goto L300;
/*                                      X = RETURN(S1) FROM READ-L */
L390:
    *x = *s1;
    goto L900;
/* --------------------------------------ERROR RETURNS */

/*  PDL OVERFLOW */
L9000:
    ret_val = b_1.nil;
    return ret_val;
} /* iread_ */

#undef s1
#undef brstk
#undef x


integer ratom_(integer *x, integer *iop)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    extern integer cons_(integer *, integer *);
    static doublereal d__;
    static integer i__, k;
    static doublereal r__, s;
    static integer idigs;
#define brstk   ((integer *)&b_1.temp3)  /* ((integer *)&b_1 + 7)  */
    static integer isign;
    extern /* Subroutine */ int shift_(integer *);
    extern integer matom_(integer *);
    static real s1;
    static integer isign3, ie, irflag, chtsav;
    extern integer mkreal_(real *);
    static integer new__;
    static doublereal sum[3];
    static real div2;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/* ----- */

/*     IOP = 0        CALLED BY LISPF4 */
/*           1        CALLED BY IREAD - DON'T RETURN <, > */

/*     CHR            CURRENT CHAR */
/*     CHT            TYPE OF CURRENT CHAR */
/*         = -1        EOF */
/*            0        EOL */
/*           >0        TYPE RETURNED FROM GETCHT */
/*     IRFLAG           FLAG FOR RECOGNIZING NUMBERS */
/*     ISIGN           FLAG FOR RECOGNIZING SIGN OF NUMATOM */
/*     ISUM            COUNTER TO PICK UP VALUE OF NUMATOM */

/*     RATOM = 1      ATOM */
/*             2      (     (X = NIL) */
/*             3      )     (X = NIL) */
/*             4      '     (X = NIL) */
/*             5      .     (X = .) !! */
/* ----- */
    ret_val = 1;
    *x = b_1.nil;
    irflag = 1;
    isign = 1;
    if (b_1.brflg == b_1.nil) {
	goto L1010;
    }
    ret_val = 3;
    goto L2040;
/*                                      SKIP DELIMITERS */
L1000:
    shift_(&c__2);
L1010:
    if (b_1.cht <= 1) {
	goto L1000;
    }
    chtsav = b_1.cht;
    if (b_1.cht <= 5) {
	goto L2000;
    }
    if (b_1.cht >= 25) {
	b_1.cht = 10;
    }
    if (b_1.cht >= 13) {
	goto L4101;
    }
    i__ = b_1.cht - 5;
    switch (i__) {
	case 1:  goto L3000;
	case 2:  goto L3200;
	case 3:  goto L5000;
	case 4:  goto L4010;
	case 5:  goto L4100;
	case 6:  goto L4101;
	case 7:  goto L4000;
    }
/*            "    '    UBR  .    A    +    - */

/*                                      BREAK CHARACTERS */
/*                                      (  )  <  > */
/*                                      SET-UP THE RETURN VALUE */
L2000:
    ret_val = b_1.cht;
    if (b_1.cht >= 4) {
	ret_val = b_1.cht - 2;
    }
    if (*iop == 0) {
	goto L5000;
    }
/*                                      CALLED BY IREAD */
/*                                      - KEEP TRACK OF ( ) < > */
    i__ = b_1.cht - 1;
    switch (i__) {
	case 1:  goto L2020;
	case 2:  goto L2040;
	case 3:  goto L2010;
	case 4:  goto L2030;
    }
/* <                                    PUSH PARENTHESIS LEVEL */
L2010:
    *brstk = cons_(&b_1.brlev, brstk);
    b_1.brlev = a_1.numadd;
/* ( */
L2020:
    ++b_1.brlev;
    b_1.cht = 0;
    goto L6000;
/* >                                    BRFLG = T  WILL PREPARE FOR */
/*                                      VIRTUAL )'S */
L2030:
    b_1.brflg = b_1.t;
/* ) */
L2040:
    --b_1.brlev;
    b_1.cht = 0;
    if ((i__1 = b_1.brlev - a_1.numadd) < 0) {
	goto L2045;
    } else if (i__1 == 0) {
	goto L2050;
    } else {
	goto L6000;
    }
/*                                      ) OR > HAS BEEN ENCOUNTERED ON */
/*                                        TOP LEVEL - JUST RETURN NIL */
L2045:
    ret_val = 1;
    goto L2060;
/*                                      ONE SUPER-BRACKED IS MATCHED */
/*                                      - POP PREVIOUS PARENTHESIS LEVEL */
L2050:
    b_1.brlev = carcdr_1.car[*brstk - 1];
    *brstk = carcdr_1.cdr[*brstk - 1];
L2060:
    b_1.brflg = b_1.nil;
    return ret_val;
/* "                                    START OF STRING. READ */
/*                                       ALL OF IT. */
L3000:
    shift_(&c__2);
    b_1.i1cons = matom_(&c__0);
    i__1 = a_1.maxint;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	k = 1;
	if (b_1.cht == 6) {
	    goto L3100;
	}
	if (b_1.cht <= 0) {
	    k = 3;
	}
/* L3090: */
	shift_(&k);
    }
/*                                      WHOLE STRING READ */
L3100:
    shift_(&c__3);
    *x = b_1.i1cons;
    return ret_val;
/* '                                    JUST SET-UP RETURN VALUE */
L3200:
    ret_val = 4;
    if (*iop == 0) {
	goto L5000;
    }
    shift_(&c__2);
    return ret_val;
/* -                                    MAY BE BEGINNING OF NUMBER */
L4000:
    isign = -1;
    goto L4101;
/* .                                    MAY BE BEGINNING OF NUMBER */
L4010:
    irflag = 2;
    goto L4101;
/* A */
L4100:
    irflag = 9;
    goto L4110;
/*                                      THIS LOOP READS UNTIL BREAK-CHAR */
/*                                      - IF THE ATOM CAN BE INTERPRETED */
/* N                                       AS A NUMBER, IT WILL. */
L4101:
    sum[0] = 0.f;
    sum[1] = 0.f;
    sum[2] = 0.f;
    idigs = 0;
    new__ = -13;
    div2 = 1.f;
    isign3 = 1;
    if (b_1.cht < 13) {
	b_1.cht = 13;
    }
L4105:
    sum[irflag - 1] = sum[irflag - 1] * 10.f + (real) (b_1.cht - 13);
L4110:
    shift_(&c__1);
    if (b_1.cht < 9) {
	goto L4500;
    }
    if (irflag > 3) {
	goto L4110;
    }
    if (b_1.cht >= 13 && b_1.cht < 23) {
	goto L4200;
    }
    if (b_1.cht >= 13) {
	b_1.cht += -12;
    }
    i__ = b_1.cht - 8;
    switch (i__) {
	case 1:  goto L4120;
	case 2:  goto L4100;
	case 3:  goto L4140;
	case 4:  goto L4150;
	case 5:  goto L4130;
	case 6:  goto L4100;
    }
/* .                                    ALLOWED IF IRFLAG = 1 */
L4120:
    if (irflag > 1) {
	goto L4100;
    }
    irflag = 2;
    goto L4110;
/* E                                    ALLOWED IF IRFLAG = 1 OR 2 */
L4130:
    if (irflag > 2) {
	goto L4100;
    }
    irflag = 3;
    goto L4110;
/* +                                    ALLOWED IF IRFLAG = 3 */
L4140:
    if (irflag - 3 != 0) {
	goto L4100;
    } else {
	goto L4110;
    }
/* -                                    ALLOWED IF IRFLAG = 3 */
L4150:
    if (irflag != 3) {
	goto L4100;
    }
    isign3 = -1;
    goto L4110;
/* DIGIT */
L4200:
    if (irflag == 1 && sum[0] > 0.f) {
	++idigs;
    }
    if (irflag == 2 && sum[0] + sum[1] == 0.f) {
	--idigs;
    }
    if (irflag == 2) {
	div2 *= 10.f;
    }
    goto L4105;
/*                                      END OF NUMBER */
L4500:
    if (irflag > 3) {
	goto L5100;
    }
    if (chtsav <= 12 && b_1.abup1 == 1) {
	goto L5100;
    }
/*                                      REAL NUMBER */
    r__ = sum[0] + sum[1] / div2;
    *x = a_1.numadd;
    if (r__ == 0.f) {
	goto L6000;
    }
    ie = isign3 * (integer) sum[2];
    if (ie > a_1.ipower - idigs) {
	ie = a_1.ipower - idigs;
    }
    if (ie < -a_1.ipower - idigs) {
	ie = -a_1.ipower - idigs;
    }
L4515:
    if (ie < 0) {
	goto L4520;
    } else if (ie == 0) {
	goto L4590;
    } else {
	goto L4530;
    }
L4520:
    ++ie;
    r__ /= 10.f;
    goto L4515;
L4530:
    --ie;
    r__ *= 10.f;
    goto L4515;
/*                                      SMALL NUMBER */
L4590:
    s = r__;
    if (isign < 0) {
	s = -r__;
    }
    d__ = (doublereal) a_1.ismall;
    if (irflag > 1 || r__ > d__) {
	goto L4591;
    }
    *x = (integer) s + a_1.numadd;
    goto L6000;
L4591:
    s1 = s;
    *x = mkreal_(&s1);
    goto L6000;
/*                                      USER BREAK CHARACTER */
L5000:
    shift_(&c__1);
/*                                      RETURN STRING OR LITATOM */
L5100:
    *x = matom_(&b_1.abup1);
/*                                      SIGNAL IF IT IS A  .  WITHOUT  % */
    if (*x == b_1.dotat && chtsav == 9) {
	ret_val = 5;
    }
L6000:
    b_1.abup1 = 0;
    return ret_val;
} /* ratom_ */

#undef brstk


/* Subroutine */ int shift_(integer *i__)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ieof, j;
    extern integer matom_(integer *), getcht_(integer *);
    extern /* Subroutine */ int rew_(integer *), rda1_(integer *, integer *, 
	    integer *, integer *, integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/* ----- */
/*     I = 1       STORE PREVIOUS CHAR */
/*         2       DON'T STORE IT */
/*         3       STORE IN PNAME */

/*     IFLG2 = T   READ FROM PRBUFF   (CALLED BY PACK) */
/*             NIL READ FROM LUNIN    (CALLED BY RATOM) */
/* ----- */

/* L1000: */
    switch (*i__) {
	case 1:  goto L1001;
	case 2:  goto L1010;
	case 3:  goto L1003;
    }
/*                                      STORE PREVIOUS CHAR */
L1001:
    ++b_1.abup1;
    b_1.abuff[b_1.abup1 - 1] = b_1.chr;
L1010:
    if (b_1.iflg2 != b_1.nil) {
	goto L2000;
    }
    goto L1100;
/*                                      STORE IN PNAME */
L1003:
    i__1 = -b_1.abup1;
    if (b_1.nil == matom_(&i__1)) {
	goto L1100;
    }
    --a_1.natomp;
    b_1.pnp[a_1.natomp] = b_1.pnp[a_1.natomp + 1];
/*                                      NOW CALLED BY RATOM */
L1100:
    if (b_1.rdpos > b_1.margr) {
	goto L1200;
    }
    b_1.chr = b_1.rdbuff[b_1.rdpos - 1];
    ++b_1.rdpos;
/*                                      WAS PREVIOUS CHAR  % */
/*                                       THEN WE MUST HAVE COME */
/*                                        FROM DOWN BELOW. */
    if (b_1.cht != 23) {
	goto L1150;
    }
    b_1.cht = 10;
    j = getcht_(&b_1.chr);
    if (j >= 11 && j < 23) {
	b_1.cht = j;
    }
    return 0;
L1150:
    b_1.cht = getcht_(&b_1.chr);
/*                                      DON'T RETURN IF  %  IS READ */
    if (b_1.cht == 23) {
	goto L1100;
    }
/*                                      INPUT BREAK ? */
    if (b_1.cht != 24) {
	goto L1160;
    }
    b_1.errtyp = 27;
    b_1.ibreak = TRUE_;
    b_1.cht = 10;
L1160:
    return 0;
/*                                      NEW LINE */
/*                                      ** THIS IS THE ONLY CALL TO RDA1 */
L1200:
    rda1_(&b_1.lunin, b_1.rdbuff, &c__1, &b_1.margr, &ieof);
/* DEBUG      WRITE(LUNUTS,1201)(RDBUFF(IIII),IIII=1,6) */
/* DEBUG1201  FORMAT(' RDBUFF=',6A5) */
    if (ieof == 2) {
	goto L1300;
    }
    b_1.rdpos = b_1.lmargr;
    b_1.cht = 0;
    return 0;
/*                                      E-O-FILE */
/* *** CHANGED BY TR */
L1300:
    if (b_1.lunin != b_1.lunins) {
	goto L1350;
    }
/*        CALL MESS(32) */
/*        CALL LSPEX */
/*                                      ... BUT NOT ON LUNINS */
/*                                       SWITCH BACK TO LUNINS */
L1350:
    if (b_1.lunin != b_1.lunins) {
	rew_(&b_1.lunin);
    }
    b_1.lunin = b_1.lunins;
    b_1.cht = -1;
    return 0;
/* ---------------------------------------------------------- */
/*                                      NOW CALLED BY PACK */

/*                                      HERE WE MAY RETURN IN CHT: */
/*                                       SEP. + - . DIGIT LETTER */

/*                                      ARG2 = MAX PRTPOS */

L2000:
    if (b_1.prtpos <= b_1.arg2) {
	goto L2010;
    }
    b_1.cht = 0;
    return 0;
L2010:
    b_1.chr = b_1.prbuff[b_1.prtpos - 1];
    ++b_1.prtpos;
    b_1.cht = 10;
    j = getcht_(&b_1.chr);
    if (j >= 9 && j < 23 || j >= 25) {
	b_1.cht = j;
    }
    return 0;
} /* shift_ */

integer garb_(integer *gbctyp)
{
    /* System generated locals */
    shortint s__1;
    integer ret_val, i__1, i__2;

    /* Local variables */
    static integer icar, icdr, ireg;
#define args   ((integer *)&b_1.arg)  /* ((integer *)&b_1)  */
    static integer inds[3], ibot, lens[3], iret;
    extern /* Subroutine */ int mess_(integer *);
    static integer itop, isum, i__, j, n, s;
    extern /* Subroutine */ int getch_(real *, integer *, integer *), markl_(
	    integer *, integer *, integer *), putch_(integer1 *, integer *, 
	    integer *), lispf4_(integer *);
    static integer jb, is;
#define jpname  ((shortint *) b_1.pname)  /* ((shortint *)&b_1 + 2244)  */
#define ipname  ((integer *) b_1.pname)   /* ((integer *)&b_1 + 1122)   */
    static integer arrlst;
    static integer inreal, ispare;
    extern /* Subroutine */ int arrutl_(integer *, integer *, integer *, 
	    integer *, integer *);
    static integer natomo, numove, messnr;
    extern /* Subroutine */ int rehash_(void), terpri_(void), priint_(integer 
	    *);
    static integer ich, len, ipl, max__, njp, ret, ind1, ind2;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*               GARB(GBCTYP) PERFORM GARBAGE COLLECTION DEPENDING ON GBC */
/*       GBCTYP = 0      NORMAL GBC, LIST CELLS ONLY (CALLED FROM CONS) */
/*                1      LIST COMPACTING (CALLED FROM ROLLOUT) */
/*                2      BIG NUMBERS COMPACTING (CALLED FROM MKNUM) */
/*                3      BIG NUMBERS AND ATOMS  (CALLED FROM MKATOM) */

/* STEP 1: MARK ACTIVE CELLS AND ATOMS BY NEGATIVE CDR. */
/*         IF FLOATING NUMBER GBC, MARK IN PNP. */
/*         IF GBCTYP < 1 GO TO STEP 2. */
/* STEP 3: LIST COMPACTING (GBCTYP = 1,2,3) */
/*         MOVE ACTIVE CAR,CDR TO TOP OF FREE STORAGE AND RESET CDR. */
/*         LEAVE NEW ADRESS IN CDR OF MOVED CELL. */
/*         IF GBCTYP < 2 GO TO STEP 2. */
/* STEP 5: FLOATING NUMBER GBC AND COMPACTING (GBCTYP = 2,3) */
/*         MOVE ACTIVE NUMBERS (MARKED IN PNP) TO TOP OF PNAME. */
/*         LEAVE NEW ADRESS IN OLD (LOWER) PNAME */
/*         RESET PNP */
/*         IF GBCTYP < 3 GO TO STEP 2. */
/* STEP 4: ATOM GBC AND COMPACTING (GBCTYP = 3) */
/*         MOVE ACTIVE ATOMS TO LOWER ATOM AREA (LOWER CAR,CDR) */
/*         LEAVE NEW ADRESS IN (NEGATIVE) HTAB AND RESET CDR. */
/* STEP 2: RESET CDR OF MARKED ATOMS. */
/*         IF GBCTYP < 1 GO TO STEP 7. */
/* STEP 6: RESTORE MOVED POINTERS (GBCTYP = 1,2,3) */
/*         CHECK ALL LIST POINTERS AND CHANGE TO */
/*         NEW VALUE IF MOVED. */
/* STEP 7: CLEAR MEMORY */
/*         MAKE FREE LIST */
/*         GBCTYP = 3: REHASH ALL SAVED ATOMS */
/*         RETURN */

/* STEP 1: */

/*             TRACE FROM ARG,ARG2,ARG3,FORM,ALIST,TEMP1,TEMP2,TEMP3, */
/*                        I1CONS,I2CONS; A-STACK; THE ATOMS. */
/*             ARG - I2CONS = ARGS(1) - ARGS(NARGS) */
    arrlst = b_1.nil;
    inreal = a_1.bytes / a_1.jbytes;
    njp = b_1.jp;
    isum = 0;
    ispare = b_1.nil;
/* THE STACK */
    i__ = 1;
L177:
    s = jaan_1.jack[i__ - 1];
    if (s <= 0) {
	goto L179;
    }
    ret = 6;
    goto L30;
/* R-6 */
L178:
    s = jaan_1.jill[i__ - 1];
    ret = 7;
    goto L30;
/* R-7 */
L179:
    ++i__;
    if (i__ <= jaan_1.tops) {
	goto L177;
    }
/* ARG-I2CONS */
    ret = 1;
    i__ = 1;
L91:
    s = args[i__ - 1];
    goto L30;
L1:
    ++i__;
    if (i__ <= b_1.nargs) {
	goto L91;
    }
/* A-STACK */
/* L13: */
    if (b_1.jp > a_1.nstack) {
	goto L16;
    }
/* L14: */
    ret = 2;
    i__ = b_1.jp;
L92:
    s = b_1.stack[i__ - 1];
    goto L30;
L3:
    ++i__;
    if (i__ <= a_1.nstack) {
	goto L92;
    }
/* ATOMS */
L16:
    i__ = 1;
L93:
    ret = 3;
    s__1 = (shortint) carcdr_1.car[i__ - 1];
    if (s__1 == b_1.string || s__1 == b_1.substr || s__1 == b_1.array || 
	    carcdr_1.cdr[i__ - 1] < 0) {
	goto L5;
    }
    s = carcdr_1.car[i__ - 1];
    goto L30;
L4:
    s = carcdr_1.cdr[i__ - 1];
    ret = 4;
    goto L30;
L5:
    ++i__;
    if (i__ <= a_1.natomp) {
	goto L93;
    }
/* ARRAYS */
    ret = 5;
L6:
    if (arrlst <= b_1.nil) {
	goto L99;
    }
    s = arrlst;
    arrlst = -carcdr_1.cdr[s - 1];
/* *SETC*      CALL SETCDR(S, -NIL) */
    carcdr_1.cdr[s - 1] = -b_1.nil;
    arrutl_(&s, &c__3, &c__1, &ind1, &len);
    ind2 = ind1 + len;
L7:
    if (ind1 == ind2) {
	goto L6;
    }
    s = jpname[ind1 - 1];
    ++ind1;
    goto L30;
L99:
    if (*gbctyp <= 0) {
	goto L200;
    } else {
	goto L300;
    }
/*             MARKL */
/*             RECURSIVE ROUTINE FOR MARKING LISTS */
/*             MARKL MAY CALL SUBROUTINE MARKL (NON-RECURSIVE ROUTINE) */

/*             RETURNPOINTS FOR MARKL */
L20:
    switch (ret) {
	case 1:  goto L1;
	case 2:  goto L3;
	case 3:  goto L4;
	case 4:  goto L5;
	case 5:  goto L7;
	case 6:  goto L178;
	case 7:  goto L179;
    }
/*                                      MARK WHAT  S  POINTS AT */
L30:
    if (s <= b_1.t) {
	goto L50;
    }
    if (s > a_1.nfreet) {
	goto L45;
    }
    icdr = carcdr_1.cdr[s - 1];
    if (icdr < 0) {
	goto L50;
    }
/*                                      AN ARRAY? */
    if (carcdr_1.car[s - 1] != b_1.array || s > a_1.natom) {
	goto L32;
    }
/* *SETC*      CALL SETCDR(S, -ARRLST) */
    carcdr_1.cdr[s - 1] = -arrlst;
    arrlst = s;
    goto L50;
L32:
    if (b_1.ip < b_1.jp - 1) {
	goto L35;
    }
/*                                      RECURSION IN MARKL TOO DEEP */
/*                                      CALL SUBROUTINE MARKL */
    if (isum == 1 || b_1.dreg[0] == b_1.nil) {
	goto L31;
    }
    isum = 1;
    mess_(&c__24);
L31:
    markl_(&s, gbctyp, &arrlst);
    goto L50;
/*                                      STACK SPACE LEFT. START MARKING */
/* *SETC*35    CALL SETCDR(S, -ICDR) */
L35:
    carcdr_1.cdr[s - 1] = -icdr;
    --b_1.jp;
    b_1.stack[b_1.jp - 1] = icdr;
/*                                      MARK CAR */
    s = carcdr_1.car[s - 1];
    goto L30;
/*                                      MARK A NUMBER */
L45:
    if (s > a_1.bignum || *gbctyp < 2) {
	goto L50;
    }
    is = s - a_1.nfreet;
    if (b_1.pnp[is - 1] > 0) {
	b_1.pnp[is - 1] = -b_1.pnp[is - 1];
    }
/*                                      RECURSIVE RETURN */
L50:
    if (b_1.jp == njp) {
	goto L20;
    }
    s = b_1.stack[b_1.jp - 1];
/*                                      MARK CDR */
    ++b_1.jp;
    goto L30;

/* STEP 3: LIST COMPACTING ROUTINE */

L300:
    ibot = a_1.nfreeb;
    itop = a_1.nfreet;
/*             STEP ONE. MOVE ACTIVE CELLS TO THE TOP OF FS */
L301:
    if (carcdr_1.cdr[itop - 1] >= 0) {
	goto L315;
    }
/* *SETC*      CALL SETCDR(ITOP, -CDR(ITOP)) */
    carcdr_1.cdr[itop - 1] = -carcdr_1.cdr[itop - 1];
L306:
    --itop;
    if (ibot - itop >= 0) {
	goto L328;
    } else {
	goto L301;
    }
L315:
    if (carcdr_1.cdr[ibot - 1] < 0) {
	goto L325;
    }
/* L320: */
    ++ibot;
    if (ibot - itop >= 0) {
	goto L330;
    } else {
	goto L315;
    }
/* *SETC*325   CALL SETCAR(ITOP,CAR(IBOT)) */
L325:
    carcdr_1.car[itop - 1] = carcdr_1.car[ibot - 1];
/* *SETC*      CALL SETCDR(ITOP,-CDR(IBOT)) */
    carcdr_1.cdr[itop - 1] = -carcdr_1.cdr[ibot - 1];
/* *SETC*      CALL SETCDR(IBOT,ITOP) */
    carcdr_1.cdr[ibot - 1] = itop;
    ++ibot;
    goto L306;
L328:
    if (carcdr_1.cdr[itop - 1] >= 0) {
	goto L330;
    }
/* *SETC*329   CALL SETCDR(ITOP,-CDR(ITOP)) */
/* L329: */
    carcdr_1.cdr[itop - 1] = -carcdr_1.cdr[itop - 1];
    --itop;
L330:
    a_1.nfreep = itop;
    switch (*gbctyp) {
	case 1:  goto L200;
	case 2:  goto L500;
	case 3:  goto L500;
    }

/* STEP 4: ATOM COMPACTING ROUTINE */

/*       MOVE UNUSED ATOMS. UNUSED IF: */
/*     CDR.GT.0 .AND. CASE(CAR) OF */
/*        (SPECIAL): T;  UNBOUND: CDR.EQ.NIL;  NIL; */

/*       FIRST CLEAR HTAB. HTAB IS USED FOR HOLDING MOVED POINTERS */
L400:
    natomo = a_1.natomp;
    i__1 = a_1.nhtab;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L420: */
	b_1.htab[i__ - 1] = 0;
    }
    n = b_1.t;
L401:
    a_1.natomp = n + 1;
    a_1.jbp = b_1.pnp[a_1.natomp - 1];
/*                                      FIND ATOM TO MOVE */
L402:
    ++n;
    if (n > natomo) {
	goto L405;
    }
    if (carcdr_1.cdr[n - 1] < 0) {
	goto L403;
    }
    s__1 = (shortint) carcdr_1.car[n - 1];
    if (s__1 == b_1.string || s__1 == b_1.substr || s__1 == b_1.array) {
	goto L402;
    }
    if (carcdr_1.car[n - 1] == b_1.unboun && carcdr_1.cdr[n - 1] == b_1.nil) {
	goto L402;
    }
L403:
    if (n == a_1.natomp) {
	goto L401;
    }
/*       MOVE ATOM(N) TO ATOM(NATOMP) AND MARK NEW ADRESS IN -HTAB(N) */
    b_1.htab[n - 1] = -a_1.natomp;
/* *SETC*      CALL SETCAR(NATOMP,CAR(N)) */
    carcdr_1.car[a_1.natomp - 1] = carcdr_1.car[n - 1];
/* *SETC*      CALL SETCDR(NATOMP,CDR(N)) */
    carcdr_1.cdr[a_1.natomp - 1] = carcdr_1.cdr[n - 1];
/*       MOVE PNAME(N) TO PNAME(NATOMP) */
/*       PNAME(NATOMP) STARTS IN (1,JBP), I.E. CURRENT PNAME-POINTERS. */
    jb = b_1.pnp[n - 1];
    ipl = b_1.pnp[n] - jb;
    if (ipl == 0) {
	goto L411;
    }
    if (jb != a_1.jbp) {
	goto L4030;
    }
    a_1.jbp += ipl;
    goto L411;
L4030:
    if (carcdr_1.car[n - 1] != b_1.array) {
	goto L409;
    }
    for (ireg = 1; ireg <= 3 || ireg == 1; ++ireg) {
/* L4031: */
	arrutl_(&n, &c__3, &ireg, &inds[ireg - 1], &lens[ireg - 1]);
    }
    for (ireg = 1; ireg <= 3 || ireg == 1; ++ireg) {
/* L4032: */
	arrutl_(&a_1.natomp, &c__4, &ireg, &inds[ireg - 1], &lens[ireg - 1]);
    }
    goto L419;
L409:
    i__1 = ipl;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	getch_(b_1.pname, &ich, &jb);
	putch_((char *)b_1.pname, &ich, &a_1.jbp);
	++jb;
/* L410: */
	++a_1.jbp;
    }
L411:
    b_1.pnp[a_1.natomp] = a_1.jbp;
/*       END OF MOVING PNAME */
L419:
    ++a_1.natomp;
    goto L402;
/*       ALL ACTIVE ATOMS MOVED TO (1,NATOMP) */
L405:
    --a_1.natomp;
    goto L200;
/*       CONTINUE WITH STEP 5 */

/* STEP 5: BIG NUMBERS COMPACTING */

/*       ALL ACTIVE BIG NUMBERS ARE MARKED NEGATIVE IN PNP. */
/*       OFFSETS FROM/TO INDEX IN CAR,INDEX IN PNP, INDEX IN PNAME ARE: */
/*       S -- I=S-NFREET --PNP(I) --J=I+DPNP -- PNAME(J) */
/*       S -- J=S+DPANME -- PNAME(J) */
L500:
    ibot = 1;
    itop = a_1.natom;
L501:
    if (b_1.pnp[itop - 1] >= 0) {
	goto L503;
    }
    b_1.pnp[itop - 1] = -b_1.pnp[itop - 1];
L502:
    --itop;
    if (ibot >= itop) {
	goto L505;
    }
    goto L501;
L503:
    if (b_1.pnp[ibot - 1] < 0) {
	goto L504;
    }
    ++ibot;
    if (ibot >= itop) {
	goto L506;
    }
    goto L503;
L504:
    i__ = itop + a_1.dpnp;
    j = ibot + a_1.dpnp;
    b_1.pname[i__ - 1] = b_1.pname[j - 1];
    j *= inreal;
    jpname[j - 1] = (shortint) (itop + a_1.nfreet);
    b_1.pnp[ibot - 1] = -b_1.pnp[ibot - 1];
    ++ibot;
    goto L502;
L505:
    if (b_1.pnp[itop - 1] >= 0) {
	goto L506;
    }
    b_1.pnp[itop - 1] = -b_1.pnp[itop - 1];
    --itop;
L506:
    a_1.numbp = itop + a_1.dpnp + 1;
    if (*gbctyp == 3) {
	goto L400;
    }
/*       NOW ALL ACTIVE NUMBERS ARE MOVED TO (PNAME(NUMBP), PNAME(NPNAME) */

/* STEP 2: RESTORE CDR OF ATOMS */

L200:
    i__1 = b_1.nil;
    i__2 = a_1.natomp;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
/* *SETC*       IF (CDR(I).LT.0) CALL SETCDR(I, -CDR(I)) */
	if (carcdr_1.cdr[i__ - 1] < 0) {
	    carcdr_1.cdr[i__ - 1] = -carcdr_1.cdr[i__ - 1];
	}
/* L210: */
    }
    if (*gbctyp < 1) {
	goto L700;
    }

/* STEP 6: RESTORE MOVED POINTERS (GBCTYP 1,2,3) */

/*       CHECK ALL LIST POINTERS AND CHANGE VALUE IF MOVED. */
/* L600: */
    numove = a_1.numbp - a_1.dpname;
/* THE STACK */
/* THE STACK */
    i__ = 1;
L560:
    s = jaan_1.jack[i__ - 1];
    if (s <= 0) {
	goto L581;
    }
    iret = 6;
    goto L650;
/* R-6 */
L570:
    jaan_1.jack[i__ - 1] = s;
    s = jaan_1.jill[i__ - 1];
    iret = 7;
    goto L650;
/* R-7 */
L580:
    jaan_1.jill[i__ - 1] = s;
L581:
    ++i__;
    if (i__ <= jaan_1.tops) {
	goto L560;
    }
/* ARG - I2CONS */
    iret = 1;
    i__ = 1;
L601:
    if (i__ > b_1.nargs) {
	goto L610;
    }
    s = args[i__ - 1];
    goto L650;
/* <-- */
L602:
    args[i__ - 1] = s;
    ++i__;
    goto L601;
/* ASTACK */
L610:
    iret = 2;
    i__ = b_1.jp;
L611:
    if (i__ > a_1.nstack) {
	goto L620;
    }
    s = b_1.stack[i__ - 1];
    goto L650;
/* <-- */
L612:
    b_1.stack[i__ - 1] = s;
    ++i__;
    goto L611;
/* ATOMS,LISTS,ARRAYS */
L620:
    i__ = b_1.nil;
L621:
    if (i__ > a_1.nfreet) {
	goto L710;
    }
    if (i__ == a_1.natomp + 1) {
	i__ = a_1.nfreep + 1;
    }
    s = carcdr_1.car[i__ - 1];
    iret = 3;
    goto L650;
/* <-- */
/* *SETC*622   CALL SETCAR(I, S) */
L622:
    carcdr_1.car[i__ - 1] = s;
    if (s != b_1.array || i__ > a_1.natom) {
	goto L625;
    }
    arrutl_(&i__, &c__3, &c__1, &ind1, &len);
    iret = 5;
L623:
    if (len < 1) {
	goto L625;
    }
    s = jpname[ind1 - 1];
    goto L650;
/* <-- */
L624:
    jpname[ind1 - 1] = (shortint) s;
    ++ind1;
    --len;
    goto L623;
L625:
    s = carcdr_1.cdr[i__ - 1];
    iret = 4;
    goto L650;
/* <-- */
/* *SETC*626   CALL SETCDR(I, S) */
L626:
    carcdr_1.cdr[i__ - 1] = s;
/* L629: */
    ++i__;
    goto L621;

/* CHANGE VALUE OF  S  IF NECESSARY */

L650:
    switch (*gbctyp) {
	case 1:  goto L656;
	case 2:  goto L654;
	case 3:  goto L652;
    }
/* A-GBC */
L652:
    if (s > a_1.natom) {
	goto L654;
    }
    if (b_1.htab[s - 1] < 0) {
	s = -b_1.htab[s - 1];
    }
    goto L660;
/* N-GBC */
L654:
    if (s <= a_1.nfreet) {
	goto L656;
    }
    if (s >= numove) {
	goto L660;
    }
    icar = inreal * (a_1.dpname + s);
    s = jpname[icar - 1];
    goto L660;
/* C-GBC */
L656:
    if (s >= a_1.nfreeb && s <= a_1.nfreep) {
	s = carcdr_1.cdr[s - 1];
    }
L660:
    switch (iret) {
	case 1:  goto L602;
	case 2:  goto L612;
	case 3:  goto L622;
	case 4:  goto L626;
	case 5:  goto L624;
	case 6:  goto L570;
	case 7:  goto L580;
    }

/* STEP 7: CLEAR MEMORY */

/*       NORMAL GBC. MAKE FREE LIST AND RETURN */
L700:
    a_1.nfreep = b_1.nil;
    isum = 0;
    i__2 = a_1.nfreeb;
    i__1 = a_1.nfreet;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__) {
	if (carcdr_1.cdr[i__ - 1] > 0) {
	    goto L701;
	}
/* *SETC*      CALL SETCDR(I,-CDR(I)) */
	carcdr_1.cdr[i__ - 1] = -carcdr_1.cdr[i__ - 1];
	goto L702;
/* *SETC*701   CALL SETCDR(I,NFREEP) */
L701:
	carcdr_1.cdr[i__ - 1] = a_1.nfreep;
	a_1.nfreep = i__;
	++isum;
L702:
	;
    }
    ++a_1.garbs;
    messnr = 35;
    goto L800;
/*       COMPACTING GARB. MAKE FREE LIST */
L710:
    max__ = a_1.nfreep;
    a_1.nfreep = b_1.nil;
    i__1 = a_1.nfreeb;
    i__2 = max__;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
/* *SETC*      CALL SETCDR(I,NFREEP) */
	carcdr_1.cdr[i__ - 1] = a_1.nfreep;
/* L711: */
	a_1.nfreep = i__;
    }
    switch (*gbctyp) {
	case 1:  goto L712;
	case 2:  goto L720;
	case 3:  goto L730;
    }
/*       CELL COMPACTING GARB */
L712:
    isum = a_1.nfreep - a_1.nfreeb + 1;
    ++a_1.cgarbs;
    messnr = 3;
    goto L800;
/*       NUMBER COMPACTING GARB. */
L720:
    isum = a_1.numbp - a_1.dpnp - 2;
    j = a_1.numbp - (a_1.jbp - 2) / a_1.bytes - 3;
    if (j < isum) {
	isum = j;
    }
    ++a_1.ngarbs;
    messnr = 19;
    goto L800;
/*       ATOM COMPACTING GARB */
L730:
    ++a_1.agarbs;
    isum = a_1.natom - a_1.natomp;
    rehash_();
    messnr = 38;

/*       EPILOGUE OF GARB. PRINT MESS IF MESSFLAG ON. */
L800:
    if (b_1.dreg[0] == b_1.nil || b_1.iflg2 != b_1.nil) {
	goto L801;
    }
    terpri_();
    mess_(&messnr);
    i__ = b_1.lunut;
    b_1.lunut = b_1.lunuts;
    b_1.prtpos = 12;
    priint_(&isum);
    terpri_();
    b_1.lunut = i__;
L801:
    if (isum > 15 || *gbctyp >= 1) {
	goto L802;
    }
/* L8011: */
    mess_(&c__36);
/* ----- !!!! ----- */
    lispf4_(&c__2);

/* NOTE!  WE ARE NOT INTERESTED OF A RECURSIVE CALL TO LISPF4, */
/*        BUT JUST TO PERFORM A QUICK JUMP TO THE RESET ADDRESS. */
/*        IF YOUR RUNTIME SYSTEM COMPLAINS CHANGE THE PREVIOUS */
/*        CALL TO: */
/*     CALL LSPEX */

L802:
    ret_val = isum;
    return ret_val;
} /* garb_ */

#undef ipname
#undef jpname
#undef args


/* Subroutine */ int markl_(integer *is, integer *gbctyp, integer *arrlst)
{
    static integer icar, icdr, i__, s;


/*             A NON-RECURSIVE LIST TRAVELING ROUTINE WHICH USES THE */
/*             ALGORITHM DESCRIBED IN CACM AUG 67 (NR 8) */

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*             AT ENTRY IS POINTS TO AN UNMARKED LIST-CELL */
    i__ = b_1.nil;
    s = *is;

/*             FORWARD SCAN */

L1:
    icdr = carcdr_1.cdr[s - 1];
    if (carcdr_1.car[s - 1] != b_1.array || s > a_1.natom) {
	goto L11;
    }
/* *SETC*      CALL SETCDR(S, -ARRLST) */
    carcdr_1.cdr[s - 1] = -(*arrlst);
    *arrlst = s;
    icdr = s;
    goto L2;
L11:
    if (icdr > a_1.nfreet) {
	goto L1024;
    }
/* L12: */
    if (carcdr_1.cdr[icdr - 1] < 0) {
	goto L24;
    }
/* *SETC*13    CALL SETCDR(S,-I) */
/* L13: */
    carcdr_1.cdr[s - 1] = -i__;
    i__ = s;
    s = icdr;
    goto L1;

/*             REVERSE SCAN */

L2:
    if (i__ == b_1.nil) {
	goto L50;
    }
/* L21: */
    s = i__;
    if (carcdr_1.car[s - 1] >= 0) {
	goto L23;
    }
/*             CELL MARKED AS BRANCH-POINT */
/* L22: */
    i__ = -carcdr_1.car[s - 1];
/* *SETC*      CALL SETCAR(S,ICDR) */
    carcdr_1.car[s - 1] = icdr;
    icdr = s;
    goto L2;
/*             NOT A BRANCH-POINT */
L23:
    i__ = -carcdr_1.cdr[s - 1];
/* *SETC*24    CALL SETCDR(S,-ICDR) */
L24:
    carcdr_1.cdr[s - 1] = -icdr;
    icdr = s;
    icar = carcdr_1.car[s - 1];
/* L25: */
    if (icar > a_1.nfreet) {
	goto L1002;
    }
/* L26: */
    if (carcdr_1.cdr[icar - 1] < 0) {
	goto L2;
    }
/*             CAR(S) POINTS TO A SUBLIST */
/* L27: */
    s = icar;
/* *SETC*      CALL SETCAR(ICDR,-I) */
    carcdr_1.car[icdr - 1] = -i__;
    i__ = icdr;
    goto L1;
/*       DIFFERENT ENTRIES FOR NUMBERS */
L1024:
    if (*gbctyp < 2) {
	goto L24;
    }
    if (icdr > a_1.bignum) {
	goto L24;
    }
    *is = icdr - a_1.nfreet;
    if (b_1.pnp[*is - 1] > 0) {
	b_1.pnp[*is - 1] = -b_1.pnp[*is - 1];
    }
    goto L24;
L1002:
    if (*gbctyp < 2) {
	goto L2;
    }
    if (icar > a_1.bignum) {
	goto L2;
    }
    *is = icar - a_1.nfreet;
    if (b_1.pnp[*is - 1] > 0) {
	b_1.pnp[*is - 1] = -b_1.pnp[*is - 1];
    }
    goto L2;
L50:
    return 0;
} /* markl_ */

/* Subroutine */ int rehash_(void)
{
    /* System generated locals */
    shortint s__1;
    integer i__1, i__2;

    /* Local variables */
    static integer hadr, i__, l, n;
    extern integer ihadr_(integer *, integer *, integer *, integer *);
    extern /* Subroutine */ int getch_(real *, integer *, integer *);
    static integer jb, ich1, ich2, ich3;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*       USED TO GET A NEW ENTRY IN HTAB FOR AN EXISTING ATOM N. */
/*       CALLED BY ATOM GBC AND ROLLIN. */
    i__1 = a_1.nhtab;
    for (n = 1; n <= i__1 || n == 1; ++n) {
/* L1: */
	b_1.htab[n - 1] = b_1.unused;
    }
    i__1 = a_1.natomp;
    for (n = 1; n <= i__1 || n == 1; ++n) {
	s__1 = (shortint) carcdr_1.car[n - 1];
	if (s__1 == b_1.string || s__1 == b_1.substr || s__1 == b_1.array) {
	    goto L100;
	}
	jb = b_1.pnp[n - 1];
	getch_(b_1.pname, &ich1, &jb);
	l = b_1.pnp[n] - b_1.pnp[n - 1];
/* L20: */
	i__2 = jb + l / 2;
	getch_(b_1.pname, &ich2, &i__2);
	i__2 = jb + l - 1;
	getch_(b_1.pname, &ich3, &i__2);
	hadr = ihadr_(&ich1, &ich2, &ich3, &a_1.nhtab);
/* L50: */
	i__2 = a_1.nhtab;
	for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
	    if (b_1.htab[hadr - 1] == b_1.unused) {
		goto L52;
	    }
	    ++hadr;
	    if (hadr <= a_1.nhtab) {
		goto L51;
	    }
	    hadr = 1;
L51:
	    ;
	}
L52:
	b_1.htab[hadr - 1] = n;
L100:
	;
    }
    return 0;
} /* rehash_ */

integer ihadr_(integer *ich1, integer *ich2, integer *ich3, integer *nhtab)
{
    /* System generated locals */
    integer ret_val, i__1;

/* -- */
/* -- THIS FUNCTION MAY NEED TO BE REWRITTEN FOR EFFICIENT */
/* -- HASHING ON SOME COMPUTERS (E.G. VAX-11) */
/* -- */
    ret_val = (i__1 = *ich1 / 7 + *ich2 / 3 + *ich3 / 5, abs(i__1)) % *nhtab 
	    + 1;
    return ret_val;
} /* ihadr_ */

integer matom_(integer *k)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    extern integer garb_(integer *);
    static integer hadr, i__, j, l;
    extern integer ihadr_(integer *, integer *, integer *, integer *);
    extern /* Subroutine */ int getch_(real *, integer *, integer *);
    static integer nbnow;
    extern /* Subroutine */ int putch_(integer1 *, integer *, integer *);
    static integer jb;
    extern /* Subroutine */ int upcase_(integer *, integer *);
    static integer imatom, naleft, nbleft, ipl, ich1;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */

/*     K>0:  CREATE A LITERAL ATOM OF THE K BYTES IN ABUFF. */
/*     K<=0: CREATE A STRING OF LENGTH -K.  MOVE ABUP1 BYTES TO IT. */

/*     POINTERS USED: JBP (BYTE POINTER IN PNAME), IS UPDATED. */
/*                    NATOMP (ATOM POINTER), INCREASED BY 1. */
/*                    ABUP1 (POINTER IN ABUFF), IS RESET. */
/*                    NUMBP (BIGNUM POINTER IN PNAME). */

/*       FIRST CALCULATE HASH ADRESS. */
    l = *k;
    if (l > 0) {
	goto L100;
    }
/*               MATOM IS TO MAKE A STRING */
    l = -l;
    hadr = 0;
    goto L30;
/*          MATOM IS TO MAKE A LITATOM */
L100:
    b_1.abup1 = l;
    if (b_1.dreg[3] == b_1.t) {
	upcase_(b_1.abuff, &l);
    }
    hadr = ihadr_(b_1.abuff, &b_1.abuff[l / 2], &b_1.abuff[l - 1], &a_1.nhtab)
	    ;
/*       HASH ADRESS = HADR */
/*       LENGTH OF ATOM = L */
/*       SEARCH FOR EXISTING ATOM OR NEW ENTRY. */
/* L5: */
    i__1 = a_1.natom;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	imatom = b_1.htab[hadr - 1];
	if (imatom == b_1.unused) {
	    goto L30;
	}
	jb = b_1.pnp[imatom - 1];
	ipl = b_1.pnp[imatom] - jb;
	if (l != ipl) {
	    goto L16;
	}
/*               EQUAL LENGTHS. TEST CHARACTERS IN IBUFF - PNAME. */
	i__2 = l;
	for (j = 1; j <= i__2 || j == 1; ++j) {
	    getch_(b_1.pname, &ich1, &jb);
	    if (ich1 != b_1.abuff[j - 1]) {
		goto L16;
	    }
/* L15: */
	    ++jb;
	}
/*               OLD ATOM FOUND. RETURN(IMATOM) */
	goto L60;
/*               ATOM NOT FOUND, TRY NEXT. */
L16:
	++hadr;
	if (hadr > a_1.nhtab) {
	    hadr = 1;
	}
/* L20: */
    }
/*               NEW ATOM TO BE CREATED */
L30:
    if (b_1.ibreak && b_1.errtyp == 33) {
	goto L56;
    }
    naleft = a_1.natom - a_1.natomp - 1;
    nbnow = a_1.bytes * (a_1.numbp - 2) - (a_1.jbp - 1);
    nbleft = nbnow - l;
    if (naleft < 0 || nbleft < 0) {
	goto L50;
    }
    if (b_1.ibreak) {
	goto L35;
    }
    if (naleft == 9 || nbnow >= 50 && nbleft < 50) {
	goto L50;
    }
L35:
    ++a_1.natomp;
    imatom = a_1.natomp;
/* *SETC*      CALL SETCAR(IMATOM, STRING) */
    carcdr_1.car[imatom - 1] = b_1.string;
/* *SETC*      CALL SETCDR(IMATOM, NIL) */
    carcdr_1.cdr[imatom - 1] = b_1.nil;
    b_1.pnp[imatom] = a_1.jbp + l;
/* *SETC*      IF (HADR.GT.0) CALL SETCAR(IMATOM, UNBOUN) */
    if (hadr > 0) {
	carcdr_1.car[imatom - 1] = b_1.unboun;
    }
    if (hadr > 0) {
	b_1.htab[hadr - 1] = imatom;
    }
    if (b_1.abup1 == 0) {
	goto L60;
    }
/*                                      MOVE CHARS TO PNAME */
    i__1 = b_1.abup1;
    for (j = 1; j <= i__1 || j == 1; ++j) {
	putch_((char *)b_1.pname, &b_1.abuff[j - 1], &a_1.jbp);
/* L42: */
	++a_1.jbp;
    }
    goto L60;
/*             SPACE FOR NEW ATOM TOO SMALL. */
/*             PERFORM COMPACTING ATOM GBC. */
L50:
    naleft = garb_(&c__3) - 1;
    nbleft = a_1.bytes * (a_1.numbp - 2) - (a_1.jbp - 1) - l;
    if (naleft < 0 || nbleft < 0) {
	goto L56;
    }
    if (! b_1.ibreak) {
	b_1.errtyp = 0;
    }
    if (naleft < 10) {
	b_1.errtyp = 28;
    }
    if (nbleft < 50) {
	b_1.errtyp = 37;
    }
    if (b_1.errtyp > 0) {
	b_1.ibreak = TRUE_;
    }
    if (hadr <= 0) {
	goto L35;
    } else {
	goto L100;
    }
/*                                      ATOM SPACE EMPTY. NIL RETURNED */
L56:
    b_1.errtyp = 33;
    b_1.ibreak = TRUE_;
    imatom = b_1.nil;
/*               RESET POINTER IN ABUFF AND RETURN(IMATOM) */
L60:
    b_1.abup1 = 0;
    ret_val = imatom;
    return ret_val;
} /* matom_ */


integer mknum_(integer *n)
{
    /* System generated locals */
    integer ret_val;
    real r__1;

    /* Local variables */
    extern integer mkreal_(real *);


/*               ROUTINE FOR MAKING A BIG INTEGER NUMBER. */

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (*n < -a_1.ismall || *n > a_1.ismall) {
	goto L1;
    }
    ret_val = *n + a_1.numadd;
    return ret_val;
L1:
    r__1 = (real) (*n);
    ret_val = mkreal_(&r__1);
    return ret_val;
} /* mknum_ */

integer mkreal_(real *r__)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern integer garb_(integer *);
    static integer i__, nleft;


/*                    ROUTINE FOR MAKING A FLOATING NUMBER */

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*                                      CHECK PRINTNAME SPACE */
/* L1: */
    if (b_1.ibreak && b_1.errtyp == 25) {
	goto L3;
    }
    nleft = a_1.numbp - (a_1.jbp - 2) / a_1.bytes - 4;
    if (nleft >= 0 && nleft != 9) {
	goto L11;
    }
    i__ = garb_(&c__3);
/*                                      CHECK AFTER GARB */
    nleft = a_1.numbp - (a_1.jbp - 2) / a_1.bytes - 4;
    i__ = a_1.numbp - a_1.dpnp - 3;
    if (i__ < nleft) {
	nleft = i__;
    }
    goto L12;
/*                                      CHECK BIGNUM SPACE */
L11:
    nleft = a_1.numbp - a_1.dpnp - 3;
    if (nleft >= 0 && nleft != 9) {
	goto L2;
    }
    nleft = garb_(&c__2) - 1;
L12:
    if (nleft < 0) {
	goto L3;
    }
    if (nleft >= 10) {
	goto L2;
    }
    b_1.errtyp = 21;
    b_1.ibreak = TRUE_;
/*                                      MAKE THE NUMBER */
L2:
    --a_1.numbp;
    b_1.pname[a_1.numbp - 1] = *r__;
    ret_val = a_1.numbp - a_1.dpname;
    return ret_val;
/*                                      BIGNUM SPACE EMPTY. 0 RETURNED */
L3:
    b_1.errtyp = 25;
    b_1.ibreak = TRUE_;
    ret_val = a_1.numadd;
    return ret_val;
} /* mkreal_ */

integer getnum_(integer *i__)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real r__;
    extern doublereal gtreal_(integer *, integer *);
    static integer iretur;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (*i__ > a_1.bignum) {
	goto L1;
    }
    r__ = gtreal_(i__, &iretur);
    ret_val = iretur;
    return ret_val;
L1:
    ret_val = *i__ - a_1.numadd;
    return ret_val;
} /* getnum_ */

doublereal gtreal_(integer *i__, integer *iretur)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static integer j;
    static real r__;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (*i__ > a_1.bignum) {
	goto L1;
    }
    j = *i__ + a_1.dpname;
    r__ = b_1.pname[j - 1];
    *iretur = a_1.maxbig;
    if (r__ < 0.f) {
	*iretur = -(*iretur);
    }
    if (dabs(r__) < a_1.bigmax) {
	*iretur = (integer) r__;
    }
    ret_val = r__;
    return ret_val;
/*                                      SMALL INT. -- DON'T CONVERT */
L1:
    ret_val = 0.f;
    *iretur = *i__ - a_1.numadd;
    return ret_val;
} /* gtreal_ */

integer getcht_(integer *ic)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer i__;

/*     I = IC/CHDIV+1 */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    i__ = *ic % 256 + 1;
    if (*ic < 0) {
	i__ = a_1.nbytes + i__ - 1;
    }
    ret_val = carcdr_1.chtab[i__ - 1];
    return ret_val;
} /* getcht_ */

/* Subroutine */ int setcht_(integer *ic, integer *it)
{
    /* Local variables */
    static integer i__;
#define ich  ((integer *)&chars_1.space)  /*  ((integer *)&chars_1)  */

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*      I = IC/CHDIV+1 */
    i__ = *ic % 256 + 1;
    if (*ic < 0) {
	i__ = a_1.nbytes + i__ - 1;
    }
    carcdr_1.chtab[i__ - 1] = *it;
    ich[*it - 1] = *ic;
    return 0;
} /* setcht_ */

#undef ich


integer nchars_(integer *s, integer *iflg)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    static integer icar;
    extern integer cons_(integer *, integer *);
    extern /* Subroutine */ int prin1_(integer *);
    static integer i__;
    extern /* Subroutine */ int apush_(integer *);
    extern integer mknum_(integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    apush_(&b_1.prtpos);
    i__1 = b_1.marg;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	b_1.buff[i__ - 1] = b_1.prbuff[i__ - 1];
/* L1: */
	b_1.prbuff[i__ - 1] = chars_1.space;
    }
    i__ = b_1.dreg[4];
    b_1.dreg[4] = *iflg;
    b_1.iflg1 = a_1.numadd;
    b_1.prtpos = 1;
L2:
    if (*s <= b_1.nil) {
	goto L4;
    }
    if (*s <= a_1.natom || *s > a_1.nfreet) {
	*s = cons_(s, &b_1.nil);
    }
/* L3: */
    icar = carcdr_1.car[*s - 1];
    prin1_(&icar);
    *s = carcdr_1.cdr[*s - 1];
    goto L2;
L4:
    i__1 = b_1.iflg1 + b_1.prtpos - 1 - a_1.numadd;
    ret_val = mknum_(&i__1);
    b_1.iflg1 = b_1.nil;
    b_1.dreg[4] = i__;
    return ret_val;
/*             AFTER CALLING NCHARS, NCHARS=THE NUMBER OF CHARS IN S, */
/*             PRBUFF=THE PRINTNAME OF S, BUFF=OLD PRBUFF. */
/*             CALLING ROUTINE MUST RESET PRBUFF AND PRTPOS */
} /* nchars_ */

/* Subroutine */ int lspex_(void)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;

    /* Builtin functions */
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    extern integer cons_(integer *, integer *);
    extern /* Subroutine */ int mess_(integer *);
    extern integer mknum_(integer *);
    extern /* Subroutine */ int terpri_(void), iprint_(integer *);

/*             EXIT ROUTINE */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    b_1.lmarg = 12;
    terpri_();
    mess_(&c__4);
    mess_(&c__39);
    b_1.lunut = b_1.lunuts;
    i__2 = mknum_(&a_1.garbs);
    i__4 = mknum_(&a_1.cgarbs);
    i__6 = mknum_(&a_1.ngarbs);
    i__8 = mknum_(&a_1.agarbs);
    i__7 = cons_(&i__8, &b_1.nil);
    i__5 = cons_(&i__6, &i__7);
    i__3 = cons_(&i__4, &i__5);
    i__1 = cons_(&i__2, &i__3);
    iprint_(&i__1);
    mess_(&c__30);
    if (FALSE_) {
	return 0;
    }
    b_1.lmarg = 1;
/*        LINE ABOVE NEEDED IF YOU CAN SAVE CORE IMAGES */
/*        THAT YOU RUN AGAIN */
#ifdef FORTRAN_LIB
    s_stop(c_b3, (ftnlen)0);
#else
    exit(0);
#endif
    return 0;
} /* lspex_ */

/* Subroutine */ int mess_(integer *i__)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer k, i2, i1, i3, nw;
    extern /* Subroutine */ int rda4_(integer *, integer *, integer *, 
	    integer *), wra4_(integer *, integer *, integer *, integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    if (*i__ == 0) {
	goto L10;
    }
    if (*i__ > a_1.maxmes) {
	*i__ = 31;
    }
/* L1: */
    nw = a_1.nbmess / a_1.ibytes;
    i2 = nw * *i__;
    i__1 = i2 + 1 - nw;
    wra4_(&b_1.lunuts, b_1.imess, &i__1, &i2);
    return 0;
/*             READ MESSAGES FROM LUNSYS */
L10:
    i1 = 1;
    i2 = a_1.nbmess / a_1.ibytes;
    i3 = i2;
    i__1 = a_1.maxmes;
    for (k = 1; k <= i__1 || k == 1; ++k) {
	rda4_(&b_1.lunsys, b_1.imess, &i1, &i2);
	i1 += i3;
/* L20: */
	i2 += i3;
    }
    return 0;
} /* mess_ */

/* Subroutine */ int dmpin2_(integer *lun, integer *card, integer *i1, 
	integer *i2)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rsue(cilist *), do_uio(integer *, char *, ftnlen), e_rsue(void);

    /* Local variables */
    static integer j;

    /* Fortran I/O blocks */
    static cilist io___233 = { 0, 0, 0, 0, 0 };


/*             DMPIN2 IS CALLED ONLY FROM ROLLIN. */
/*             CARD MUST HAVE THE SAME DECLARATION AS CAR, CDR ... */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*      I3 = MAXREC/JBYTES */
/*      DO 10 I = I1,I2,I3 */
/*        MAX = I+I3-1 */
/*        IF (MAX.GT.I2) MAX = I2 */
/* 10      READ (LUN) (CARD(J), J = I,MAX) */
    /* Parameter adjustments */
    --card;

    /* Function Body */
/* L10: */
#ifdef FORTRAN_LIB
    io___233.ciunit = *lun;
    s_rsue(&io___233);
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j) {
	do_uio(&c__1, (char *)&card[j], (ftnlen)sizeof(integer));
    }
    e_rsue();
#else
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j)
	f4_readu(*lun, (char *)&card[j], 4);
#endif
    return 0;
} /* dmpin2_ */

/* Subroutine */ int rda4_(integer *lun, integer *card, integer *i1, integer *
	i2)
{
    /* Format strings */
    static char fmt_100[] = "(100a4)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);

    /* Local variables */
    static integer i__;

    /* Fortran I/O blocks */
    static cilist io___235 = { 0, 0, 0, fmt_100, 0 };


/*             RDA4 IS CALLED FROM MESS */
    /* Parameter adjustments */
    --card;

    /* Function Body */
#ifdef FORTRAN_LIB
    io___235.ciunit = *lun;
    s_rsfe(&io___235);
    i__1 = *i1;
    i__2 = *i2;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	do_fio(&c__1, (char *)&card[i__], (ftnlen)sizeof(integer));
    }
    e_rsfe();
#else
    f4_start_read();
    i__1 = *i1;
    i__2 = *i2;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	f4_read(*lun, (char *)&card[i__], 4);
    }
#endif
    return 0;
} /* rda4_ */

/* Subroutine */ int dmpin_(integer *lun, integer *area, integer *i1, integer 
	*i2)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rsue(cilist *), do_uio(integer *, char *, ftnlen), e_rsue(void);

    /* Local variables */
    static integer j;

    /* Fortran I/O blocks */
    static cilist io___237 = { 0, 0, 0, 0, 0 };


/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*      I3 = MAXREC/IBYTES */
/*      DO 10 I = I1,I2,I3 */
/*        MAX = I+I3-1 */
/*        IF (MAX.GT.I2) MAX = I2 */
    /* Parameter adjustments */
    --area;

    /* Function Body */
/* L10: */
#ifdef FORTRAN_LIB
    io___237.ciunit = *lun;
    s_rsue(&io___237);
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j) {
	do_uio(&c__1, (char *)&area[j], (ftnlen)sizeof(integer));
    }
    e_rsue();
#else
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j)
	f4_readu(*lun, (char *)&area[j], 4);
#endif
    return 0;
} /* dmpin_ */

/* Subroutine */ int dmpou2_(integer *lun, integer *line, integer *i1, 
	integer *i2)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsue(cilist *), do_uio(integer *, char *, ftnlen), e_wsue(void);

    /* Local variables */
    static integer j;

    /* Fortran I/O blocks */
    static cilist io___239 = { 0, 0, 0, 0, 0 };


/*             DMPOU2 IS CALLED ONLY FROM ROLLOUT. */
/*            LINE MUST HAVE THE SAME DECLARATION AS CAR, CDR ... */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*     IF YOUR FORTRAN HAS A MAX RECORD LENGTH WHEN WRITING TO FILES */
/*     YOU MAY HAVE TO CHANGE BELOW. REMOVE LINE 10 AND THE COMMENTS */
/*     CHANGE MAXREC TO WHATEVER LENGT IS MAX ON YOUR COMPUTER */
/*      I3 = MAXREC/JBYTES */
/*      DO 10 I = I1,I2,I3 */
/*        MAX = I+I3-1 */
/*        IF (MAX.GT.I2) MAX = I2 */
/* 10      WRITE (LUN) (LINE(J), J = I,MAX) */
    /* Parameter adjustments */
    --line;

    /* Function Body */
/* L10: */
#ifdef FORTRAN_LIB
    io___239.ciunit = *lun;
    s_wsue(&io___239);
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j) {
	do_uio(&c__1, (char *)&line[j], (ftnlen)sizeof(integer));
    }
    e_wsue();
#else
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j)
	f4_write(*lun, (char *)&line[j], 4);
#endif
    return 0;
} /* dmpou2_ */

/* Subroutine */ int wra1_(integer *lun, integer *line, integer *i1, integer *
	i2)
{
    /* Format strings */
    static char fmt_100[] = "(150a1)";
    static char fmt_101[] = "(1x,150a1)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__;

    /* Fortran I/O blocks */
    static cilist io___241 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___243 = { 0, 0, 0, fmt_101, 0 };


/* INSERTED BY TR: */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    /* Parameter adjustments */
    --line;

    /* Function Body */
    if (*lun == b_1.lunuts) {
	goto L1;
    }
#ifdef FORTRAN_LIB
    io___241.ciunit = *lun;
    s_wsfe(&io___241);
    i__1 = *i1;
    i__2 = *i2;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	do_fio(&c__1, (char *)&line[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
#else
    f4_start_read();
    i__1 = *i1;
    i__2 = *i2;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	f4_write(*lun, (char *)&line[i__], 1);
    }
    f4_write_lf(*lun);
#endif
    return 0;
L1:
#ifdef FORTRAN_LIB
    io___243.ciunit = *lun;
    s_wsfe(&io___243);
    i__2 = *i1;
    i__1 = *i2;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__) {
	do_fio(&c__1, (char *)&line[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
#else
    f4_start_read();
    i__2 = *i1;
    i__1 = *i2;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__)
	f4_write(*lun, (char *)&line[i__], 1);
    f4_write_lf(*lun);
#endif
    return 0;
} /* wra1_ */

/* Subroutine */ int rda1_(integer *lun, integer *card, integer *i1, integer *
	i2, integer *ieof)
{
    /* Format strings */
    static char fmt_10[] = "($150a1)";
    static char fmt_101[] = "($150a1)";
    static char fmt_100[] = "(150a1)";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_rsfe(cilist *), e_rsfe(void);

    /* Local variables */
    static integer i__, k;

    /* Fortran I/O blocks */
    static cilist io___244 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___247 = { 0, 0, 0, fmt_101, 0 };
    static cilist io___248 = { 0, 0, 1, fmt_100, 0 };


/*             CALLED FROM SHIFT AND INIT2 */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    /* Parameter adjustments */
    --card;

    /* Function Body */
    *ieof = 1;
    if (*lun != b_1.lunins) {
	goto L40;
    }
    if (b_1.prtpos > 1) {
	goto L20;
    }
#ifdef FORTRAN_LIB
    io___244.ciunit = b_1.lunuts;
    s_wsfe(&io___244);
    i__1 = prompt_1.prolen;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	do_fio(&c__1, (char *)&prompt_1.protxt[i__ - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();
#else
    f4_start_read();
    i__1 = prompt_1.prolen;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__)
	f4_write(b_1.lunuts, (char *)&prompt_1.protxt[i__ - 1], 1);
#endif
/* 10    FORMAT (1X,150A1) */
    goto L40;
L20:
    k = b_1.prtpos - 1;
#ifdef FORTRAN_LIB
    io___247.ciunit = b_1.lunuts;
    s_wsfe(&io___247);
    i__1 = k;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	do_fio(&c__1, (char *)&b_1.prbuff[i__ - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();
#else
    f4_start_read();
    i__1 = k;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__)
	f4_write(b_1.lunuts, (char *)&b_1.prbuff[i__ - 1], 1);
    f4_write_lf(b_1.lunuts);
#endif
/* 101   FORMAT(1X,150A1) */
    i__1 = k;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L21: */
	b_1.prbuff[i__ - 1] = chars_1.space;
    }
    b_1.prtpos = 1;
L40:
#ifdef FORTRAN_LIB
    io___248.ciunit = *lun;
    i__1 = s_rsfe(&io___248);
    if (i__1 != 0) {
	goto L1;
    }
    i__2 = *i1;
    i__3 = *i2;
    for (i__ = i__2; i__ <= i__3 || i__ == i__2; ++i__) {
	i__1 = do_fio(&c__1, (char *)&card[i__], (ftnlen)sizeof(integer));
	if (i__1 != 0) {
	    goto L1;
	}
    }
    i__1 = e_rsfe();
#else
    f4_start_read();
    i__2 = *i1;
    i__3 = *i2;
    for (i__ = i__2; i__ <= i__3 || i__ == i__2; ++i__) {
	i__1 = f4_read(*lun, (char *)&card[i__], 1);
	if (i__1 != 0)
	    goto L1;
    }
#endif
    if (i__1 != 0) {
	goto L1;
    }
    return 0;
L1:
    *ieof = 2;
    return 0;
} /* rda1_ */

integer mpname_(integer *x, integer1 *buffer, integer *max__, integer *ipl)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    static integer main, i__;
    extern /* Subroutine */ int getch_(real *, integer *, integer *);
    extern integer getpn_(integer *, integer *, integer *, integer *);
    extern /* Subroutine */ int putch_(integer1 *, integer *, integer *);
    static integer jb, ich;

/* -- MOVES THE PNAME OF X TO THE BUFFER IN PACKED FORM */
/* -- MPNAME LT 0  ERROR */
/* --        EQ 0  OK */
/* --        GT 0  TRUNCATION */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    /* Parameter adjustments */
    --buffer;

    /* Function Body */
    if (getpn_(x, &main, &jb, ipl) < 0) {
	goto L5;
    }
    i__1 = *max__;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* 2     CALL PUTCH(BUFFER,1H ,I) */
/* L2: */
	putch_(&buffer[1], &chars_1.space, &i__);
    }
    ret_val = 0;
    if (*ipl <= *max__) {
	goto L3;
    }
    ret_val = 1;
    *ipl = *max__;
L3:
    i__1 = *ipl;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	getch_(b_1.pname, &ich, &jb);
	putch_(&buffer[1], &ich, &i__);
/* L4: */
	++jb;
    }
    return ret_val;
L5:
    ret_val = -1;
    return ret_val;
} /* mpname_ */

/* Subroutine */ int wra4_(integer *lun, integer *line, integer *i1, integer *
	i2)
{
    /* Format strings */
    static char fmt_101[] = "(100a4)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__, i3;

    /* Fortran I/O blocks */
    static cilist io___255 = { 0, 0, 0, fmt_101, 0 };


/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    /* Parameter adjustments */
    --line;

    /* Function Body */
    i3 = 1;
    i__1 = *i1;
    i__2 = *i2;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	if (line[i__] != chars_1.space) {
	    i3 = i__;
	}
/* L10: */
    }
#ifdef FORTRAN_LIB
    io___255.ciunit = *lun;
    s_wsfe(&io___255);
    i__2 = *i1;
    i__1 = i3;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__) {
	do_fio(&c__1, (char *)&line[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
#else
    f4_start_read();
    i__2 = *i1;
    i__1 = i3;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__)
	f4_write(*lun, (char *)&line[i__], 4);
    f4_write_lf(*lun);
#endif
/* 101   FORMAT(1X,100A4) */
    return 0;
} /* wra4_ */

/* Subroutine */ int dmpout_(integer *lun, integer *area, integer *i1, 
	integer *i2)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsue(cilist *), do_uio(integer *, char *, ftnlen), e_wsue(void);

    /* Local variables */
    static integer j;

    /* Fortran I/O blocks */
    static cilist io___256 = { 0, 0, 0, 0, 0 };


/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*      I3 = MAXREC/IBYTES */
/*      DO 10 I = I1,I2,I3 */
/*        MAX = I+I3-1 */
/*        IF (MAX.GT.I2) MAX = I2 */
    /* Parameter adjustments */
    --area;

    /* Function Body */
/* L10: */
#ifdef FORTRAN_LIB
    io___256.ciunit = *lun;
    s_wsue(&io___256);
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j) {
	do_uio(&c__1, (char *)&area[j], (ftnlen)sizeof(integer));
    }
    e_wsue();
#else
    i__1 = *i1;
    i__2 = *i2;
    for (j = i__1; j <= i__2 || j == i__1; ++j)
	f4_write(*lun, (char *)&area[j], 4);
#endif
    return 0;
} /* dmpout_ */

doublereal openf_(integer *i__)
{
    /* System generated locals */
    real ret_val;

    *i__ = 0;
    return ret_val;
} /* openf_ */

/* Subroutine */ int rew_(integer *lun)
{
    /* System generated locals */
    alist al__1;

    /* Builtin functions */
#ifdef FORTRAN_LIB
    integer f_rew(alist *);

    al__1.aerr = 0;
    al__1.aunit = *lun;
    f_rew(&al__1);
#else
    f4_rewind(*lun);
#endif
    return 0;
} /* rew_ */

/* Subroutine */ int eject_(integer *lun)
{
    /* Format strings */
    static char fmt_100[] = "(\0021\002)";

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void);

    /* Fortran I/O blocks */
    static cilist io___258 = { 0, 0, 0, fmt_100, 0 };

#ifdef FORTRAN_LIB
    io___258.ciunit = *lun;
    s_wsfe(&io___258);
    e_wsfe();
#else
    f4_write(*lun, "    ", 1);
#endif
    return 0;
} /* eject_ */

integer xcall_(integer *fn, integer *x)
{
    /* System generated locals */
    integer ret_val, i__1;
    int	len2, len3, len4;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), f_clos(cllist *);

    /* Local variables */
    extern /* Subroutine */ int mkcha_(integer *, char *, ftnlen, int *);
    static integer a1, a2, a3, a4;
    static struct { integer fill; char val[50+1]; char fill2[1]; } c2_st;
#define c2 c2_st.val
    static struct { integer fill; char val[50+1]; char fill2[1]; } c3_st;
#define c3 c3_st.val
    static struct { integer fill; char val[50+1]; char fill2[1]; } c4_st;
#define c4 c4_st.val
    extern integer getnum_(integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    switch (*fn) {
	case 1:  goto L1000;
	case 2:  goto L2000;
    }
    goto L10000;
L1000:
    if (*x <= a_1.natom || *x > a_1.nfreet) {
	goto L10000;
    }
    a1 = carcdr_1.car[*x - 1];
    if (a1 <= a_1.nfreet || a1 > a_1.maxint) {
	goto L10000;
    }
    ret_val = a1;
    a1 = getnum_(&a1);
    *x = carcdr_1.cdr[*x - 1];
    if (*x <= a_1.natom || *x > a_1.nfreet) {
	goto L10000;
    }
    a2 = carcdr_1.car[*x - 1];
    if (a2 < b_1.nil || a2 > a_1.natomp) {
	goto L10000;
    }
    mkcha_(&a2, c2, (ftnlen)50, &len2);
    *x = carcdr_1.cdr[*x - 1];
    if (*x <= a_1.natomp || *x > a_1.nfreet) {
	goto L10000;
    }
    a3 = carcdr_1.car[*x - 1];
    if (a3 <= b_1.nil || a3 > a_1.natomp) {
	goto L10000;
    }
    mkcha_(&a3, c3, (ftnlen)50, &len3);
    *x = carcdr_1.cdr[*x - 1];
    if (*x <= a_1.natomp || *x > a_1.nfreet) {
	goto L10000;
    }
    a4 = carcdr_1.car[*x - 1];
    if (a4 <= b_1.nil || a4 > a_1.natomp) {
	goto L10000;
    }
    mkcha_(&a4, c4, (ftnlen)50, &len4);
#ifdef FORTRAN_LIB
    o__1.oerr = 1;
    o__1.ounit = a1;
    o__1.ofnmlen = 50;
    o__1.ofnm = c2;
    o__1.orl = 0;
    o__1.osta = c3;
    o__1.oacc = 0;
    o__1.ofm = c4;
    o__1.oblnk = 0;
    i__1 = f_open(&o__1);
#else
    {
	    char   mode[4];
	    c2[len2] = '\0';
	    c3[len3] = '\0';
	    c4[len4] = '\0';
	    if (!stricmp(c3, "NEW"))
		    strcpy(mode, "w");
	    else
		    strcpy(mode, "r");
#ifndef unix
	    if (!stricmp(c4, "FORMATTED"))
		    strcat(mode, "t");
	    else
		    strcat(mode, "b");
#endif
	    i__1 = f4_open(a1, c2, mode);
    }
#endif
    if (i__1 != 0) {
	goto L10000;
    }

    return ret_val;

L2000:
    if (*x <= a_1.nfreet || *x > a_1.maxint) {
	goto L10000;
    }
    a1 = getnum_(x);
#ifdef FORTRAN_LIB
    cl__1.cerr = 1;
    cl__1.cunit = a1;
    cl__1.csta = 0;
    i__1 = f_clos(&cl__1);
#else
    i__1 = f4_close(a1);
#endif
    if (i__1 != 0) {
	goto L10000;
    }
    ret_val = *x;
    return ret_val;
L10000:
    ret_val = b_1.nil;
    return ret_val;
} /* xcall_ */

#undef c4
#undef c3
#undef c2


/* Subroutine */ int mkcha_(integer *addr__, char *a, ftnlen a_len, int *len)
{
#ifdef FORTRAN_LIB
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3[2];
    char ch__1[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    static integer iqqn, iqqq[50], iqqr, i__;
    extern /* Subroutine */ int getch_(real *, integer *, integer *);

/*     RECUR 5 */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */



    iqqr = b_1.pnp[*addr__ - 1];
    iqqn = b_1.pnp[*addr__];

    i__1 = iqqn - iqqr;
    if (len)
	    *len = i__1;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L60: */
	i__2 = iqqr - 1 + i__;
	getch_(b_1.pname, &iqqq[i__ - 1], &i__2);
    }

/*     A=CHAR(IQQQ(1)/CHDIV) */
    *(unsigned char *)&ch__1[0] = iqqq[0] % 256;
    s_copy(a, ch__1, (ftnlen)50, (ftnlen)1);
    i__2 = iqqn - iqqr;
    for (i__ = 2; i__ <= i__2 || i__ == 2; ++i__) {
/*  70 A=A(1:I-1) // CHAR(IQQQ(I)/CHDIV) */
/* L70: */
/* Writing concatenation */
	i__3[0] = i__ - 1, a__1[0] = a;
	*(unsigned char *)&ch__1[0] = iqqq[i__ - 1] % 256;
	i__3[1] = 1, a__1[1] = ch__1;
	s_cat(a, a__1, i__3, &c__2, (ftnlen)50);
    }
#else

    int	iqqr, iqqn, i__1, n;
    char	*p;

    iqqr = b_1.pnp[*addr__ - 1];
    iqqn = b_1.pnp[*addr__];

    i__1 = iqqn - iqqr;
    if (len)
	    *len = i__1;
    p = ((char *) b_1.pname) + iqqr - 1;
    for (n=0 ; n++ < i__1 ; )
	    *a++ = *p++;
#endif

    return 0;
} /* mkcha_ */

/* Subroutine */ static void brserv_(void)
{
/* --  INTERRUPT HANDLER */
/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    b_1.errtyp = 26;
    b_1.ibreak = TRUE_;
} /* brserv_ */


/* Subroutine */ int brset_(void)
{
/* -- THIS SUBROUTINE IS CALLED INITIALLY AND SHOULD SET UP TERMINAL */
/* -- INTERRUPTS SO THAT THE SUBROUTINE BRSERV IS CALLED WHENEVER THE */
/* -- USER TYPES AN INTERRUPT CHARACTER (E.G. CTRL-H ON DEC, */
/* -- CTRL-C ON VAX11, BREAK ON IBM) */
	signal(SIGINT, brserv_);
    return 0;
} /* brset_ */

