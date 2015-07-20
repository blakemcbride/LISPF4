/* lispf41.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"
#include <stdio.h>


#define SHOWINT(x)	fprintf(stderr, #x " = %d\n", x)


static int shostk_(void);


/* Common Block Declarations */

struct {
    integer narea, jbytes, ibytes, bytes, chdiv, maxint, maxbig, maxrec, 
	    natomp, nfreep, jbp, numbp, nfreet, numadd, npname, nfreeb, natom,
	     nstack, nhtab, bignum, ismall, dpnp, dpname, ipromp, nathsh, 
	    nbytes, nchtyp, nbmess, maxmes, iresol, ipower;
    real bigmax, fuzz;
    integer garbs, cgarbs, ngarbs, agarbs;
    real rmax;
} a_;

#define a_1 a_

struct {
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

struct {
    integer *car, *cdr, chtab[256];
} carcdr_;

#define carcdr_1 carcdr_

struct {
    integer space, lpar, rpar, ilbchr, irbchr, strchr, iqchr, ubr, dot, itchr,
	     iplus, iminus, ifig[10], atend, softbr, echar, nochar;
} chars_;

#define chars_1 chars_

struct {
    integer *jill, *jack, env, tops, hill, hillw;
} jaan_;

#define jaan_1 jaan_

struct {
    integer protxt[80], prolen;
} prompt_;

#define prompt_1 prompt_

/* Table of constant values */

static integer c__20 = 20;
static integer c__22 = 22;
static integer c__17 = 17;
static struct { integer fill; char val[4+1]; char fill2[3]; } c_b8_st = { 0,
	"_   " };
#define c_b8 c_b8_st.val
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__7 = 7;
static integer c__19 = 19;
static integer c__6 = 6;
static integer c__26 = 26;
static integer c__9 = 9;
static integer c__2 = 2;
static integer c_n5 = -5;
static integer c_n10 = -10;
static integer c_n20 = -20;
static integer c__3 = 3;
static integer c__5 = 5;
static integer c__14 = 14;
static integer c__21 = 21;
static integer c__18 = 18;
static integer c__4 = 4;
static integer c__27 = 27;
static integer c__12 = 12;
static integer c__13 = 13;
static integer c__23 = 23;
static integer c__11 = 11;
static integer c__16 = 16;
static integer c__40 = 40;

/* ******HEART       (HEART OF INTERPRETER) */
/* Subroutine */ int lispf4_(integer *iree)
{
    /* System generated locals */
    shortint s__1;
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10;
    static real equiv_2[1];

    /* Local variables */
    extern integer garb_(integer *);
    static integer icar, icdr, main, ireg;
#define args  ((integer *)&b_1.arg)  /* ((integer *)&b_1) */
    extern /* Subroutine */ int apop_(integer *);
    static integer imax;
    extern integer cons_(integer *, integer *);
#define narr (equiv_2)
#define ires  ((integer *)&b_1.arg)  /* ((integer *)&b_1)  */
    extern /* Subroutine */ int mess_(integer *);
    static integer iret;
    extern /* Subroutine */ int apop2_(integer *, integer *), prin1_(integer *
	    );
    static integer i__, j, k, l;
#define n ((integer *)equiv_2)
    extern integer iread_(integer *);
    static real r__, s;
    extern /* Subroutine */ int getch_();
    extern integer xcall_(integer *, integer *), openf_(integer *), subpr_(
	    integer *, integer *, integer *), equal_(integer *, integer *), 
	    getpn_(integer *, integer *, integer *, integer *), ratom_(
	    integer *, integer *);
    static integer index;
#define iotab ((integer *)&b_1.lunin)	/*  ((integer *)&b_1 + 65)  */
#define rargs  ((real *)&b_1.arg)  /* ((real *)&b_1)  */
    extern integer mknum_(integer *);
    extern /* Subroutine */ int fpush_(integer *);
    static integer local, iprev, iargs, k1;
    extern /* Subroutine */ int shift_(integer *);
    extern integer matom_(integer *);
    extern /* Subroutine */ int eject_(integer *), lspex_(void);
    extern integer mslft_(integer *);
    extern /* Subroutine */ int mtime_(integer *), mdate_(integer *), apush_(
	    integer *);
    static integer n1, n2;
    extern /* Subroutine */ int putch_(real *, integer *, integer *);
    static integer jndex, kalle;
    extern /* Subroutine */ int apush2_(integer *, integer *);
    static integer jb, ic, ii, ll, locale, irflag;
    static logical aplyfl, noeval, formfl;
    extern integer getcht_(integer *), comppn_(integer *, integer *), getnum_(
	    integer *), rollin_(integer *);
#define ipname  ((integer *) b_1.pname)   /*  ((integer *)&b_1 + 1122)  */
#define jpname  ((shortint *) b_1.pname)  /*  ((shortint *)&b_1 + 2244)  */
    extern /* Subroutine */ int iprint_(integer *), terpri_(void);
    extern integer mkreal_(real *);
    extern /* Subroutine */ int rollou_(integer *), 
	    arrutl_(integer *, integer *, integer *, integer *, integer *);
    extern doublereal gtreal_(integer *, integer *);
    static integer if41;
    extern integer nchars_(integer *, integer *);
    static integer ich;
    extern /* Subroutine */ int setcht_(integer *, integer *);
    static integer nch, jb2;
    extern integer get_(integer *, integer *);
    static integer ict, min__, ipl, max__, if42;
    extern /* Subroutine */ int rew_(integer *);
    static integer ist, ipl2;

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
/*     THIS IS A MODIFIED LISPF3 THAT USE A STACK */
/*     AND NOT A A-LIST FOR VARIABLE BINDINGS. */

/*               LOCAL DECLARATIONS */
/*               ================== */

/*      SPECAT(JDUMMY) = JDUMMY.EQ.STRING .OR. JDUMMY.EQ.SUBSTR */
/*     * .OR. JDUMMY.EQ.ARRAY */
/* L2: */
    switch (*iree) {
	case 1:  goto L10;
	case 2:  goto L1;
	case 3:  goto L1;
    }
/*          INI STA REE */
L10:
    mess_(&c__20);
    b_1.prtpos = 12;
    i__2 = a_1.numadd + 83;
    i__4 = a_1.numadd + 8;
    i__6 = a_1.numadd + 22;
    i__5 = cons_(&i__6, &b_1.nil);
    i__3 = cons_(&i__4, &i__5);
    i__1 = cons_(&i__2, &i__3);
    iprint_(&i__1);
    mess_(&c__22);
    b_1.prtpos = 12;
    i__ = a_1.numbp - a_1.dpnp - 2;
    j = a_1.numbp - (a_1.jbp - 2) / a_1.bytes - 3;
    if (j < i__) {
	i__ = j;
    }
    i__3 = a_1.nfreep - a_1.nfreeb;
    i__2 = mknum_(&i__3);
    i__5 = a_1.numadd + a_1.ismall;
    i__7 = mknum_(&i__);
    i__10 = a_1.natom - a_1.natomp;
    i__9 = mknum_(&i__10);
    i__8 = cons_(&i__9, &b_1.nil);
    i__6 = cons_(&i__7, &i__8);
    i__4 = cons_(&i__5, &i__6);
    i__1 = cons_(&i__2, &i__4);
    iprint_(&i__1);
/*               RESTART OF INTERPRETER */
/*               ----------------------------- */
L1:
    b_1.ip = 1;
    b_1.jp = a_1.nstack + 1;
    b_1.ipp = 0;
    b_1.jpp = 0;
    mess_(&c__17);
/* *SETC*      CALL SETCAR(NIL,NIL) */
    carcdr_1.car[b_1.nil - 1] = b_1.nil;
/* *SETC*      CALL SETCDR(NIL,NIL) */
    carcdr_1.cdr[b_1.nil - 1] = b_1.nil;
/* *SETC*      CALL SETCAR(T,T) */
    carcdr_1.car[b_1.t - 1] = b_1.t;
    b_1.iflg1 = a_1.numadd;
    terpri_();
    b_1.iflg1 = b_1.nil;
    b_1.iflg2 = b_1.nil;
    b_1.cht = 1;
    b_1.rdpos = 1000;
    b_1.lunin = b_1.lunins;
    b_1.form = b_1.nil;
    jaan_1.hillw = jaan_1.hill - 150;
    b_1.middl = a_1.nstack / 10;
    b_1.isplft = 400;
    b_1.ibreak = FALSE_;
    b_1.arg = b_1.lispx;
    b_1.arg2 = b_1.nil;
    jaan_1.env = 0;
    jaan_1.tops = 0;
    getch_(c_b8, prompt_1.protxt, &c__1, (ftnlen)1);
    prompt_1.prolen = 1;
    goto L1500;

/* RECURSIVE RETURN PROCEDURE */
/* DROP TOP BLOCK */

L997:
    jaan_1.env = -jaan_1.jack[jaan_1.tops - 1];
/*                                      ! RESTORE ENVIRONMENT */
L998:
    jaan_1.tops = jaan_1.jill[jaan_1.tops - 1];
/*                                      ! DROP TOP BLOCK */
L999:
    i__ = b_1.stack[b_1.ip - 1];
    --b_1.ip;

/* L1000: */
    switch (i__) {
	case 1:  goto L15013;
	case 2:  goto L2020;
	case 3:  goto L2620;
	case 4:  goto L2120;
	case 5:  goto L12185;
	case 6:  goto L997;
	case 7:  goto L1020;
	case 8:  goto L1710;
	case 9:  goto L1830;
	case 10:  goto L18060;
	case 11:  goto L20130;
	case 12:  goto L18100;
	case 13:  goto L18170;
	case 14:  goto L12260;
	case 15:  goto L18210;
	case 16:  goto L25090;
	case 17:  goto L25100;
	case 18:  goto L15010;
	case 19:  goto L998;
	case 20:  goto L12150;
	case 21:  goto L12290;
	case 22:  goto L12640;
	case 23:  goto L20095;
	case 24:  goto L1795;
	case 25:  goto L1778;
	case 26:  goto L1799;
	case 27:  goto L18012;
    }

/*           APPLYS EVLIS  EVLAS  EVCON  EVSTK  &POPE  LISPX */
/*           EAPPL  FUNAR  SETQ   PROG   AND    OR */
/*           MAPC   SELEC  ?STOV  ?STUN  APPLA  &POP   EVALA */
/*           MAPC   RPT    PROG   EVARGN EVARG  NOSP   FUNCTION   =27 */

    goto L25130;
/*                                        ! INTERNAL STACK PROBLEM */


/* RECURSIVE FUNCTION LISPX() */
/* ----------------------------------------------------------------------- */
/* USE EVAL IN TOP LEVEL LOOP */

L1010:
    b_1.ibreak = FALSE_;
    b_1.arg = iread_(&c__0);
    fpush_(&c__7);
    goto L1600;

/* --R7         RETURN POSITION FROM EVAL */

L1020:
    iprint_(ires);
    terpri_();
    goto L1010;


/*             RECURSIVE FUNCTION APPLY(ARG,ARG2) */

L1477:
    fpush_(&c__19);
L1500:
    if (b_1.arg == b_1.nil) {
	goto L999;
    }
    l = b_1.arg;
    aplyfl = TRUE_;
    b_1.arg = cons_(&l, &b_1.arg2);
    b_1.form = cons_(&b_1.nil, &b_1.arg);
    goto L1671;
/*                                      ! TO ENTRY EAPPLY(L,ARG2) IN EVAL */

/*               RECURSIVE FUNCTION EVAL(ARG) */
/*               ============================ */

L1600:
    if (b_1.ibreak) {
	goto L2400;
    }
    if (jaan_1.tops > jaan_1.hillw) {
	goto L25095;
    }
    if (b_1.jp <= b_1.ip + b_1.middl) {
	goto L25090;
    }
/*                                     ! THESE ARE THE ONLY */
/*                                     ! STACK-OVERFLOW TESTS NEEDED */
    if (b_1.arg > a_1.natom) {
	goto L1660;
    }
/*                                     ! LIST OR NUMBER */
    if (b_1.arg == b_1.nil || b_1.arg == b_1.t) {
	goto L999;
    }
    s__1 = (shortint) carcdr_1.car[b_1.arg - 1];
    if (s__1 <= b_1.substr && s__1 >= b_1.array) {
	goto L999;
    }

/*            EVAL - ATOM. */
/*            ATOM NOT NIL, T, STRING, */
/*            ARRAY OR NUMBER. CHECK IF BOUND ON STACK */

    local = jaan_1.env;
    if (local <= 0) {
	goto L1650;
    }
    goto L1620;
L1610:
    local = -jaan_1.jack[local - 1];
    if (local <= 0) {
	goto L1650;
    }
L1620:
    locale = jaan_1.jill[local - 1] + 2;
    k = local;
L1630:
    --k;
    if (k < locale) {
	goto L1610;
    }
    if (jaan_1.jack[k - 1] != b_1.arg) {
	goto L1630;
    }
    *ires = jaan_1.jill[k - 1];
    goto L999;


/*           ATOM NOT ON STACK CHECK VALUE CELL. */

L1650:
    if (carcdr_1.car[b_1.arg - 1] == b_1.unboun) {
	goto L2200;
    }
/*                                      ! 2200=FAULTEVAL(ARG) */
    *ires = carcdr_1.car[b_1.arg - 1];
    goto L999;

/*          HERE IF ARG NUMBER OR LIST */
/*          ========================== */

L1660:
    if (b_1.arg > a_1.nfreet) {
	goto L999;
    }
/*                                      ! IT'S A NUMBER */

/*              EVAL - LIST */

    b_1.form = b_1.arg;
    l = carcdr_1.car[b_1.arg - 1];
    b_1.arg2 = carcdr_1.cdr[b_1.arg - 1];

/*            EAPPLY(L,ARG2) . CALLED FROM EVAL AND FROM APPLY. */

/* L1670: */
    aplyfl = FALSE_;
L1671:
    ll = l;
    if (ll > a_1.natom) {
	goto L1720;
    }
/*                                      ! CAR OF FORM IS A LIST */
/*                                      ! HOPEFULLY (LAMBDA ...) */
/*                                      ! OR  (SUBR . ...) */

/* THE FOLLOWING CODE IS FOR AVOIDING CALLS TO GET FOR TESTING IF */
/* THERE IS ANY FUNCTION DEFINITION STORED UNDER THE FNCELL PROPERTY */
    ll = carcdr_1.cdr[l - 1];
    if (ll == b_1.nil) {
	goto L1676;
    }
    if (carcdr_1.car[ll - 1] != b_1.fncell) {
	goto L1677;
    }
    ll = carcdr_1.car[carcdr_1.cdr[ll - 1] - 1];
    goto L1720;
L1677:
    ll = get_(&l, &b_1.fncell);
/*                                      ! IS THERE ANYTHING IN FUNC.CELL? */
    if (ll != b_1.nil) {
	goto L1720;
    }
/*                                      ! THERE WAS SOMETHING */
L1676:
    if (l > b_1.fsubr) {
	goto L2230;
    }
/*                                      ! 2230 = FAULTAPPLY(L) */
    if (l > b_1.subr) {
	goto L18000;
    }
/*                                      ! 18000 = THE MATCH ROUTINE FOR FSUBRS. */
    if (l == b_1.nil) {
	goto L2230;
    }


/*     SUBR CASE */
/*     ========= */


/*      1. PUT THE BOTOM BLOCK */

L1680:
    iprev = jaan_1.tops;
/*                                      ! SAVE OLD TOPS */
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = l;
    jaan_1.jill[jaan_1.tops - 1] = b_1.arg;
/*                                      ! WE PUSH *FORM AND ARG */

/*      2.  SPREAD THE ARGS */

L1690:
    if (b_1.arg2 <= a_1.natom || b_1.arg2 > a_1.nfreet) {
	goto L1695;
    }
/*                                      ! ARG2 HOLDS THE ARGLIST */
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.subarg;
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.car[b_1.arg2 - 1];
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    goto L1690;
/*      3.  PUT TOP BLOCK */

L1695:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -iprev;
/*                                      ! LINK TO PREVIUS BLOCK */
    jaan_1.jill[jaan_1.tops - 1] = iprev + 1;
/*                                      ! LINK TO FORM */
    if (aplyfl) {
	goto L1713;
    }
/*                                      ! DON'T EVALUATE ARGS IF */
/*                                      ! SUBR CALLED BY APPLY */
/*      CALL APUSH(L) INLINE */
/*      JP=JP-1 */
/*      IF(IP .GE. JP) GO TO 2 */
/*      STACK(JP)=L */
/*                                      ! WE SAVE L (THATS THE SUBR) */
/*                                      ! ON THE STACK OTHERVISE */
/*                                      ! IT IS HARD TO KNOW WHAT SUBR */
/*                                      ! WE HAD WHEN WE HAVE EVALUATED */
/*                                      ! THE ARGS TO THE SUBR */
/*           EVAL ARGS TO SUBR */
/*           ================= */
/*           WHEN WE ARRIVE HERE THE TOP BLOCK MAY LOOK LIKE THIS. */
/*           THE GLOBAL VARIABLE TOPS POINTS TO THE TOPBLOCK. */
/*           THE POINTER IN JILL(TOPS) POINTS TO ARGUMENT UNDER */
/*           EVALUATION. */

/*           I--  LINK,,LINK -------------  <---  TOPS */
/*           I    -*-  FOO               I */
/*           I    -*-  BAR               I */
/*           I   *FORM (CONS FOO BAR)  <-I */
/*           I */
/*           I->  LINK,,LINK */

L1697:
    ++jaan_1.jill[jaan_1.tops - 1];
    if (jaan_1.jill[jaan_1.tops - 1] == jaan_1.tops) {
	goto L1712;
    }
/*                                      ! WE HAVE EVALUATED ALL ARGS */
    b_1.arg = jaan_1.jill[jaan_1.jill[jaan_1.tops - 1] - 1];
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 8;
/*                                      !  CALL FPUSH(8) */
    goto L1600;
/* R-8 */
L1710:
    if41 = jaan_1.jill[jaan_1.tops - 1];
    jaan_1.jill[if41 - 1] = *ires;
    goto L1697;

L1712:
/*     OK WE ARE READY NOW WE MUST */
/*     1. COMPUTE INDEX TO FIRST ARG */
/*     2. RESTORE FORM */
/*     3. RESTORE FUNCTION (=L) */
/*     4. FIX LINK TO PREVIUS BLOCK */
/*     5. COMPUTE NUMBER OF ARGS */
    index = -jaan_1.jack[jaan_1.tops - 1];
/*                                      ! THATS TO PREVIUS BLOCK SO WE MUST */
/*                                      ! ADD 2 TO GET THE REAL INDEX */
/*                                      ! BUT WE CAN USE THIS INDEX FIRST */
    if41 = index + 1;
    b_1.form = jaan_1.jill[if41 - 1];
    l = jaan_1.jack[if41 - 1];
    jaan_1.jack[if41 - 1] = b_1.sform;

/*      CALL APOP(L) INLINE */
/*      IF (JP .GT. NSTACK) GO TO 2 */
/*      L=STACK(JP) */
/*      JP=JP+1 */

    jaan_1.jill[jaan_1.tops - 1] = index;
    jaan_1.jack[jaan_1.tops - 1] = 0;
/*                                      ! JACK = 0 SHOWS THIS BLOCK */
/*                                      ! HAS NO VARIABLES */

    index += 2;
    iargs = jaan_1.tops - index;
    goto L2800;
/*                                      ! 2800 THE DISPATCH FOR SUBRS */

/*     SUBR CALLED FROM APPLY */

/*     WE DO THE SAME THING BUT WE DONT HAVE TO */
/*     RESTORE FORM OR FUNCTION */

L1713:
    index = -jaan_1.jack[jaan_1.tops - 1];
    jaan_1.jill[jaan_1.tops - 1] = index;
    jaan_1.jack[jaan_1.tops - 1] = 0;
    index += 2;
    iargs = jaan_1.tops - index;
    goto L2800;
/*                                      ! 2800 THE DISPATCH FOR SUBRS */


/*               APPLYFN2(LL,ARG2) */
/*               APPLIES A LIST FORM ONTO ARG2 */
/*               LL IS (HOPFULLY) ON FORM (LAMBDA (X Y) .....) */
/*                                        OR (SUBR . ...) */
/*               ARG2 IS A LIST OF PARAMETERS */

L1720:
    if (ll <= a_1.natom || ll > a_1.nfreet) {
	goto L2210;
    }
/*                                      ! 2210=FAULTAPPLY(LL) */
    l = carcdr_1.car[ll - 1];
    if (l == b_1.lambda) {
	goto L1730;
    }
    if (l == b_1.nlambd) {
	goto L1722;
    }
/*  (SUBR . X) CASE */
    if (l == b_1.expr) {
	goto L1810;
    }
/*  (FSUBR . X) CASE */
    if (l == b_1.fexpr) {
	goto L1815;
    }
    if (l == b_1.funarg) {
	goto L1818;
    }
    goto L2210;

L1722:
    noeval = TRUE_;
    goto L1731;
L1730:
    noeval = FALSE_;


/*          LAMBDA & NLAMBDA CASE */
/*          ===================== */

/*          1. BOTTOM BLOCK */

L1731:
    iprev = jaan_1.tops;
/*                                      ! THIS IS PREVIUS BLOCK */
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.sform;
    jaan_1.jill[jaan_1.tops - 1] = b_1.arg;
/*                                      ! PUSH FORM ON STACK */
/*           2. FIND THE LAMBDALIST */

/*           IF THE FORM IS (FOO FIE FUM (SETQ BAR 77)) */
/*           AND FOO  = (DE FOO (X Y)(CONS X Y)) */
/*           THEN LL  = (LAMBDA (X Y)(CONS X Y)) */
/*           AND ARG2 = (FIE FUM (SETQ BAR 77)) */

/* L1750: */
    icdr = carcdr_1.cdr[ll - 1];
    icar = carcdr_1.car[icdr - 1];
/*                                      ! ICAR IS THEN (X Y) */
L1760:
    if (icar <= a_1.natom || icar > a_1.nfreet) {
	goto L1770;
    }
/*                                      ! THERE WERE NO LAMBDALIST */
/*                                      ! OR IT IS EXAUSTED */
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = carcdr_1.car[icar - 1];
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.car[b_1.arg2 - 1];
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    icar = carcdr_1.cdr[icar - 1];
    goto L1760;

L1770:
    if (icar != b_1.nil) {
	goto L1789;
    }
/*                                      ! WE HAVE A LAMBDA LIST ON FORMAT */
/*                                      ! (X Y . Z), HALFSPREAD OR L, NOSPREAD */

/*          WE ARE ABOUT TO PUT THE TOPBLOCK */
/*          BUT MAYBE ARE THERE EXTRA ARGUMENTS TO THE FUNCTION */

L1771:
    if (b_1.arg2 <= a_1.natom || b_1.arg2 > a_1.nfreet) {
	goto L1772;
    }
/*                                      ! THERE WERE NO EXTRA ARGS */
/*          SOMEONE HAS CALLED A LAMBDA WITH MORE ARGUMENTS */
/*          THAN DEFINED IN LAMBDALIST. */
/*          WE PUT THE EXTRA ARGS ABOVE THE "REAL" ONES. */
/*          THESE ARGS ALL GET THE NAME -*- */

    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.subarg;
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.car[b_1.arg2 - 1];
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    goto L1771;

/*             NOW WE CAN PUT THE TOP BLOCK */

L1772:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -iprev;
/*                                      ! TO PREVIUS BLOCK */
    jaan_1.jill[jaan_1.tops - 1] = iprev + 1;
/*                                      ! TO FORM */
    if (noeval || aplyfl) {
	goto L1785;
    }
/*                                      ! IT WAS A NLAMBDA */
/*                                      ! OR FUNCTION CALLED BY APPLY */


/*              EVALUATE ALL ARGS */
/*              ================= */

/*              WHEN WE ARRIVE HERE THE TOP BLOCK MAY LOOK LIKE THIS. */
/*              THE GLOBAL VARIABLE TOPS POINTS TO THE TOPBLOCK */

/*              I--  LINK,,LINK -------------------------  <---  TOPS */
/*              I    -*- (SETQ BAR 77)                   I */
/*              I     Y  FUM                             I */
/*              I     X  FIE                             I */
/*              I   *FORM (FOO FIE FUM (SETQ BAR 77))  <-I */
/*              I */
/*              I->  LINK,,LINK */

L1777:
    ++jaan_1.jill[jaan_1.tops - 1];
    if (jaan_1.jill[jaan_1.tops - 1] == jaan_1.tops) {
	goto L1785;
    }
    if41 = jaan_1.jill[jaan_1.tops - 1];
    b_1.arg = jaan_1.jill[if41 - 1];
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 25;
/*                                         !  CALL FPUSH(25) */
    goto L1600;
/* R-25 */
L1778:
    if41 = jaan_1.jill[jaan_1.tops - 1];
    jaan_1.jill[if41 - 1] = *ires;
    goto L1777;


/*          WE HAVE EVALUATED ALL ARGS */
/*          ========================== */

/*          WE MUST NOW */
/*          FIX LINK TO PREVIUS BLOCK */
/*          SAVE THE OLD ENVIRONMENT */
/*          MAKE THIS THE NEW ENVIRONMENT */
/*          RESTORE FORM AND FUNCTION */

L1785:
    index = -jaan_1.jack[jaan_1.tops - 1];
    jaan_1.jill[jaan_1.tops - 1] = index;
    jaan_1.jack[jaan_1.tops - 1] = -jaan_1.env;
    jaan_1.env = jaan_1.tops;
    b_1.form = jaan_1.jill[index];
    l = carcdr_1.car[b_1.form - 1];
L1776:
    if (l <= a_1.natom || l > a_1.nfreet) {
	goto L1786;
    }
    ll = l;
    b_1.temp1 = carcdr_1.car[l - 1];
    if (b_1.temp1 != b_1.funarg) {
	goto L1788;
    }
    b_1.temp1 = carcdr_1.cdr[l - 1];
    l = carcdr_1.car[b_1.temp1 - 1];
    goto L1776;
/* THE FOLLOWING CODE IS FOR AVOIDING CALLS TO GET */
L1786:
    ll = carcdr_1.cdr[l - 1];
/* ASSUME THAT THERE IS ALWAYS A PROPERTY LIST */
    if (carcdr_1.car[ll - 1] != b_1.fncell) {
	goto L1787;
    }
    ll = carcdr_1.car[carcdr_1.cdr[ll - 1] - 1];
    goto L1788;
L1787:
    ll = get_(&l, &b_1.fncell);
L1788:
    b_1.arg = carcdr_1.cdr[ll - 1];
    b_1.arg = carcdr_1.cdr[b_1.arg - 1];

/*           NOW ITS TIME TO CALL EVLAST WITH THE FUNCTION BODY */
/*           AND WHEN WE RETURN FROM THERE GOTO 997 TO POP ENVIRONMENT. */
/*           THIS WE COULD HAVE DONE WITH */
/*           CALL FPUSH(?) */
/*           GOTO 2600 */
/*  R- ? */
/* XXXX       GOTO 997 */

/*      BUT AS FPUSH(6) RETURNS TO 997 WE SAVE ONE RECURSION BY */

    fpush_(&c__6);
    goto L2600;

/*     HALF AND NOSPREAD ARGS */
/*     ====================== */

L1789:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = icar;
    jaan_1.jill[jaan_1.tops - 1] = b_1.arg2;
/*                                         ! PUT THE LAST ARG (HALFSPREAD) */
/*                                         ! OR THE ONLY ARG (NOSPREAD) */
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -iprev;
    jaan_1.jill[jaan_1.tops - 1] = iprev + 1;
    if (noeval || aplyfl) {
	goto L1785;
    }
L1790:
    ++jaan_1.jill[jaan_1.tops - 1];
    if (jaan_1.jill[jaan_1.tops - 1] == jaan_1.tops - 1) {
	goto L1797;
    }
/*                                         ! LAST ARGUMENT */
    if41 = jaan_1.jill[jaan_1.tops - 1];
    b_1.arg = jaan_1.jill[if41 - 1];
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 24;
/*                                         ! CALL FPUSH(24) */
    goto L1600;
/* --R24 */
L1795:
    if41 = jaan_1.jill[jaan_1.tops - 1];
    jaan_1.jill[if41 - 1] = *ires;
    goto L1790;

L1797:
    b_1.arg = jaan_1.jill[jaan_1.tops - 2];
    fpush_(&c__26);
    goto L2000;
/*                                          ! CALL EVLIS WITH LAST OR */
/*                                          ! THE ONLY ARG */
/* R--26 */
L1799:
    jaan_1.jill[jaan_1.tops - 2] = *ires;
    goto L1785;




/*          (SUBR . ATOM) */
L1810:
    l = carcdr_1.cdr[ll - 1];
    if (l > b_1.subr) {
	goto L2210;
    }
/*                                         ! 2210=FAULTAPPLY(LL) */
    if (l == b_1.nil) {
	goto L2230;
    }
    goto L1680;
/*          (FSUBR . ATOM) */
L1815:
    l = carcdr_1.cdr[ll - 1];
    if (l > b_1.fsubr) {
	goto L2210;
    }
    if (l <= b_1.subr) {
	goto L2210;
    }
    goto L18000;
/*                                          ! 18000=MATCH ROUTINE FOR FSUBRS. */


/*               FUNARG */
/*              THIS CODE MUST BE REWRITEN IT WORKS BUT !! */
L1818:
    b_1.temp1 = carcdr_1.cdr[ll - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	b_1.temp1 = b_1.nil;
    }
    l = carcdr_1.car[b_1.temp1 - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	b_1.temp1 = b_1.nil;
    }
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    b_1.temp2 = carcdr_1.car[b_1.temp1 - 1];
/* L1820: */
    iprev = jaan_1.tops;
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.sform;
/*      JILL(TOPS)=TEMP1 !! THIS LINE CHANGED TO BELOW LINE, 20 OCTOBER 84 */
    jaan_1.jill[jaan_1.tops - 1] = ll;
L1821:
    b_1.temp3 = carcdr_1.car[b_1.temp2 - 1];
    if (b_1.temp3 <= a_1.natom || b_1.temp3 > a_1.nfreet) {
	goto L1822;
    }
    ++jaan_1.tops;
    b_1.temp2 = carcdr_1.cdr[b_1.temp2 - 1];
    jaan_1.jack[jaan_1.tops - 1] = carcdr_1.car[b_1.temp3 - 1];
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.cdr[b_1.temp3 - 1];
    goto L1821;
L1822:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -jaan_1.env;
    jaan_1.jill[jaan_1.tops - 1] = iprev;
    jaan_1.env = jaan_1.tops;
    aplyfl = FALSE_;
    fpush_(&c__9);
    goto L1671;

/* --R9 */

L1830:
    k = jaan_1.jill[jaan_1.tops - 1] + 1;
    b_1.temp1 = jaan_1.jill[k - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L2250;
    }
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L2250;
    }
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L2250;
    }
    b_1.temp1 = carcdr_1.car[b_1.temp1 - 1];
    k1 = k;
L1832:
    ++k1;
    if (k1 >= jaan_1.tops) {
	goto L997;
    }
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L2250;
    }
    b_1.temp2 = carcdr_1.car[b_1.temp1 - 1];
    if (b_1.temp2 <= a_1.natom || b_1.temp2 > a_1.nfreet) {
	goto L2250;
    }
    if (carcdr_1.car[b_1.temp2 - 1] != jaan_1.jack[k1 - 1]) {
	goto L2250;
    }
/* *SETC*      CALL SETCDR(TEMP2,JILL(K1)) */
    carcdr_1.cdr[b_1.temp2 - 1] = jaan_1.jill[k1 - 1];
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    goto L1832;



/*             RECURSIVE FUNCTION EVLIS(ARG) */
/* ----------------------------------------------------------------------- */

L2000:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L999;
    }
    b_1.jp += -2;
    b_1.stack[b_1.jp] = b_1.nil;
    b_1.stack[b_1.jp - 1] = b_1.arg;
/*                                        ! CALL APUSH2(NIL,ARG) */
L2010:
    b_1.arg = carcdr_1.car[b_1.arg - 1];
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 2;
/*                                        ! CALL FPUSH(2) */
    goto L1600;

/* --R2         RETURN FROM EVAL(ARG) */

L2020:
    b_1.temp1 = b_1.stack[b_1.jp];
    b_1.stack[b_1.jp] = cons_(ires, &b_1.temp1);
    icdr = b_1.stack[b_1.jp - 1];
    b_1.arg = carcdr_1.cdr[icdr - 1];
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L2030;
    }
    b_1.stack[b_1.jp - 1] = b_1.arg;
    goto L2010;
L2030:
    *ires = b_1.stack[b_1.jp];
    b_1.jp += 2;
/*       DREVERSE IRES BEFORE RETURN. */
    b_1.temp1 = b_1.nil;
L2040:
    b_1.temp2 = carcdr_1.cdr[*ires - 1];
/* *SETC*      CALL SETCDR(IRES,TEMP1) */
    carcdr_1.cdr[*ires - 1] = b_1.temp1;
    if (b_1.temp2 == b_1.nil) {
	goto L999;
    }
    b_1.temp1 = *ires;
    *ires = b_1.temp2;
    goto L2040;


/* ----------------------------------------------------------------------- */
/*             RECURSIVE FUNCTION EVCON */

L2100:
    --b_1.jp;
L2110:
    b_1.stack[b_1.jp - 1] = b_1.arg;
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L2140;
    }
    icar = carcdr_1.car[b_1.arg - 1];
    if (icar <= a_1.natom || icar > a_1.nfreet) {
	goto L25080;
    }
    b_1.arg = carcdr_1.car[icar - 1];
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 4;
    goto L1600;

/* --R4         RETURN FROM EVAL(ARG) */

L2120:
    if (*ires != b_1.nil) {
	goto L2130;
    }
    icdr = b_1.stack[b_1.jp - 1];
    b_1.arg = carcdr_1.cdr[icdr - 1];
    goto L2110;
L2130:
    icar = b_1.stack[b_1.jp - 1];
    icar = carcdr_1.car[icar - 1];
    icar = carcdr_1.cdr[icar - 1];
    ++b_1.jp;
    if (icar == b_1.nil) {
	goto L999;
    }
    b_1.arg = icar;
    goto L2600;
L2140:
    ++b_1.jp;
    *ires = b_1.nil;
    goto L999;



/*               FAULTEVAL AND FAULTAPPLY AS CALLED FROM EVAL-APPLY. */
/*               --------------------------------------------------- */

L2200:
    l = b_1.eval;
    b_1.errtyp = 1;
    b_1.form = b_1.arg;
    goto L2400;

L2210:
    b_1.arg = ll;
    b_1.errtyp = 2;
L2220:
    l = b_1.apply;
    goto L2400;

L2230:
    b_1.arg = l;
    b_1.errtyp = 2;
    goto L2220;

/* L2240: */
    b_1.errtyp = 26;
    b_1.arg = b_1.arg2;
    goto L2400;

/*  FAULTY FUNARG BLOCK */

L2250:
    b_1.errtyp = 31;
    b_1.form = jaan_1.jill[jaan_1.jill[jaan_1.tops - 1]];
    l = carcdr_1.car[b_1.form - 1];
    b_1.arg = b_1.nil;
    goto L2400;

/*       SYSERROR(ERRTYP,L,ARG,FORM) */
/*       --------------------------- */
/*       THIS ENTRY IS USED FOR ALL WEAK ERRORS IN THE SYSTEM. */
/*       WHEN CALLED, ERRTYP=TYPE OF ERROR, L=FAILING FUNCTION, */
/*         ARG=ERRONEOUS FORM, FORM=FORM BEING EVALUATED. */
/*       HERE THE LISP-DEFINED FUNCTION SYSERROR IS CALLED. */
L2400:
    b_1.arg2 = cons_(&b_1.form, &b_1.nil);
    b_1.arg2 = cons_(&b_1.arg, &b_1.arg2);
    b_1.arg2 = cons_(&l, &b_1.arg2);
    i__1 = b_1.errtyp + a_1.numadd;
    b_1.arg2 = cons_(&i__1, &b_1.arg2);
    b_1.arg = b_1.error;
    b_1.ibreak = FALSE_;
    b_1.lmarg = 1;
    goto L1500;
/*       CALL APPLY(SYSERROR, (ERRTYP L ARG FORM)) */




/*             RECURSIVE FUNCTION EVLAST(ARG) */
/* ----------------------------------------------------------------------- */

/*   ON ENTRY HERE ARG HOLDS A LIST OF FORMS THAT SHOULD BE EVALUATED */
/*   (SETQ FOO 77)(...) (....) */
/*   THE LIST IS KEEPT ON TOP OF STACK AND CAR OF IT IS SENT TO EVAL */
/*   WHEN ONLY ONE FORM IS LEFT THIS FORM IS GIVEN TO EVAL BUT BEFORE */
/*   THE TOP OF STACK IS CLEARED AND EVAL IS CALLED NONRECURCIVLY */

L2600:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L999;
    }
    --b_1.jp;
    b_1.stack[b_1.jp - 1] = b_1.arg;
    b_1.temp1 = b_1.arg;
L2610:
    b_1.arg = carcdr_1.car[b_1.temp1 - 1];
    i__ = carcdr_1.cdr[b_1.temp1 - 1];
    if (i__ <= a_1.natom || i__ > a_1.nfreet) {
	goto L2630;
    }
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 3;
    goto L1600;

/* --R3         RETURN FROM EVAL(ARG) */

L2620:
    icar = b_1.stack[b_1.jp - 1];
    b_1.temp1 = carcdr_1.cdr[icar - 1];
    b_1.stack[b_1.jp - 1] = b_1.temp1;
    goto L2610;
L2630:
    ++b_1.jp;
    goto L1600;

/*             MAIN DISPATCH */
/*             ============= */

/*             AT ENTRY , L=THE FUNCTION, IARGS=NR OF ARGS IN */
/*             JILL ,STARTING AT INDEX */


L2800:
    if (l <= b_1.subr0) {
	goto L3000;
    }
    if (l <= b_1.subr1) {
	goto L3020;
    }
    if (l <= b_1.subr2) {
	goto L3140;
    }
    if (l - b_1.subr3 <= 0) {
	goto L3200;
    } else {
	goto L3290;
    }
/* ----------------------------------------------------------------------- */
/* SUBR0  ENTRY */

L3000:
    switch (l) {
	case 1:  goto L3090;
	case 2:  goto L10075;
	case 3:  goto L10080;
	case 4:  goto L10130;
	case 5:  goto L10050;
	case 6:  goto L10060;
	case 7:  goto L10070;
	case 8:  goto L1010;
	case 9:  goto L10090;
	case 10:  goto L10100;
	case 11:  goto L10000;
	case 12:  goto L10105;
	case 13:  goto L1;
	case 14:  goto L10110;
	case 15:  goto L10120;
    }
/*          NIL    BTV* */
/*          CLOCK DATE  EJECT EXIT GENSY LISPX RATOM READ  READC */
/*          READP  RESET TERPRI TIME */
/* ----------------------------------------------------------------------- */
/* SUBR1  ENTRY */

L3020:
    b_1.arg = b_1.nil;
    if (iargs == 0) {
	goto L3050;
    }
/* L3040: */
    b_1.arg = jaan_1.jill[index - 1];

/* SUBR1 JUMP ENTRY */

L3050:
    if (l <= b_1.subr11) {
	goto L3070;
    }
    ll = l - b_1.subr11;
/*             JUMP TO SUBR1 (AND DO NOT TEST ON NUM ARG) */
    switch (ll) {
	case 1:  goto L10040;
	case 2:  goto L11190;
	case 3:  goto L11197;
	case 4:  goto L11200;
	case 5:  goto L11210;
	case 6:  goto L11210;
	case 7:  goto L11210;
	case 8:  goto L11210;
	case 9:  goto L11210;
	case 10:  goto L11210;
	case 11:  goto L11210;
	case 12:  goto L11210;
	case 13:  goto L11210;
	case 14:  goto L11210;
	case 15:  goto L11210;
	case 16:  goto L11210;
	case 17:  goto L11210;
	case 18:  goto L11210;
	case 19:  goto L11215;
	case 20:  goto L11220;
	case 21:  goto L11225;
	case 22:  goto L11227;
	case 23:  goto L11230;
	case 24:  goto L11255;
	case 25:  goto L11260;
	case 26:  goto L11290;
	case 27:  goto L11205;
	case 28:  goto L11310;
	case 29:  goto L11320;
	case 30:  goto L11330;
	case 31:  goto L11340;
	case 32:  goto L998;
	case 33:  goto L11355;
	case 34:  goto L20150;
	case 35:  goto L11360;
	case 36:  goto L11415;
	case 37:  goto L11420;
    }
/*          ALIST ARRAYSIZE ARRAYP ATOM */
/*          CAR   CDR   CAAR  CADR  CDAR  CDDR  CAAAR CAADR CADAR */
/*          CADDR CDAAR CDADR CDDAR CDDDR EVAL EVLIS FIXP FLOATP GETD */
/*          LAST,LENGTH LISTP LITAT NLISTP NULL  NUMBE OBLIS */
/*          PROG1 PROMP RETUR REVER STRINGP ZEROP */

/* SUBR1  TEST ON NUMERICAL ARG */

L3070:
    if (b_1.arg <= a_1.bignum) {
	goto L25000;
    }
    *n = b_1.arg - a_1.numadd;
    ll = l - b_1.subr0;
    switch (ll) {
	case 1:  goto L11000;
	case 2:  goto L11010;
	case 3:  goto L11020;
	case 4:  goto L11050;
	case 5:  goto L11060;
	case 6:  goto L11070;
	case 7:  goto L11080;
    }
/*          ADD1 ERRMS RECLA ROLLI ROLLO SUB1 REWIND */

/* RETURN CHANNELS (CAN BE USED BY ALL SUBR'S AND FSUBR'S EXCEPT SUBRN'S) */

/* TEMP1 */

L3080:
    *ires = b_1.temp1;
    goto L998;

/* PREDICATES */

L3090:
    *ires = b_1.nil;
    goto L998;
L3100:
    *ires = b_1.t;
    goto L998;

/* NUMERICAL VALUED FUNCTIONS */

L3105:
L3110:
    if (irflag != b_1.nil) {
	goto L3120;
    }
    if (*n < -a_1.ismall || *n > a_1.ismall) {
	goto L3130;
    }
    *ires = *n + a_1.numadd;
    goto L998;
L3120:
    *ires = mkreal_(&r__);
    goto L998;
L3130:
    *ires = mknum_(n);
    goto L998;
/* ----------------------------------------------------------------------- */
/* SUBR2  ENTRY */

L3140:
    b_1.arg2 = b_1.nil;
    b_1.arg = b_1.nil;
    if41 = iargs + 1;
    switch (if41) {
	case 1:  goto L3190;
	case 2:  goto L3180;
	case 3:  goto L3170;
    }
L3170:
    b_1.arg2 = jaan_1.jill[index];
L3180:
    b_1.arg = jaan_1.jill[index - 1];

/* SUBR 2 JUMP ENTRY */

L3190:
    ll = l - b_1.subr1;
    switch (ll) {
	case 1:  goto L12000;
	case 2:  goto L12040;
	case 3:  goto L12050;
	case 4:  goto L1477;
	case 5:  goto L12715;
	case 6:  goto L12720;
	case 7:  goto L12090;
	case 8:  goto L12100;
	case 9:  goto L12112;
	case 10:  goto L12114;
	case 11:  goto L12116;
	case 12:  goto L12120;
	case 13:  goto L12130;
	case 14:  goto L12140;
	case 15:  goto L12160;
	case 16:  goto L12190;
	case 17:  goto L12200;
	case 18:  goto L12730;
	case 19:  goto L12205;
	case 20:  goto L12210;
	case 21:  goto L12350;
	case 22:  goto L12390;
	case 23:  goto L12430;
	case 24:  goto L12470;
	case 25:  goto L12500;
	case 26:  goto L12510;
	case 27:  goto L12515;
	case 28:  goto L12520;
	case 29:  goto L12540;
	case 30:  goto L12570;
	case 31:  goto L12600;
	case 32:  goto L12620;
	case 33:  goto L12655;
	case 34:  goto L12660;
	case 35:  goto L12690;
	case 36:  goto L12746;
	case 37:  goto L12750;
	case 38:  goto L12780;
    }
/*            ADDLI ALPHO APPEN APPLY ASSOC   CHTAB CONS  DIFFE ELT */
/*      ELTI */
/*      ELTR  EQ    EQUAL EVALA EVSTK GETP  GREAT IOTAB IQUOR LESSP */
/*      MEMB  MEMBE NCHAR NCONC NCONC1 NEQ  NTH   PACK  QUOTI RPLACA */
/*      RPLACD */
/*      RPT   SASSOC SET   STRAL TAILP UNPAC XCALL */
/* ----------------------------------------------------------------------- */
/* SUBR3  ENTRY */

L3200:
    b_1.arg = b_1.nil;
    b_1.arg2 = b_1.nil;
    b_1.arg3 = b_1.nil;
    if41 = iargs + 1;
    switch (if41) {
	case 1:  goto L3280;
	case 2:  goto L3209;
	case 3:  goto L3207;
	case 4:  goto L3205;
    }
L3205:
    b_1.arg3 = jaan_1.jill[index + 1];
L3207:
    b_1.arg2 = jaan_1.jill[index];
L3209:
    b_1.arg = jaan_1.jill[index - 1];

/* SUBR3  JUMP ENTRY */

L3280:
    ll = l - b_1.subr2;
    switch (ll) {
	case 1:  goto L15000;
	case 2:  goto L15012;
	case 3:  goto L15015;
	case 4:  goto L12330;
	case 5:  goto L12220;
	case 6:  goto L12310;
	case 7:  goto L12340;
	case 8:  goto L12345;
	case 9:  goto L15020;
	case 10:  goto L15040;
	case 11:  goto L15130;
	case 12:  goto L15142;
	case 13:  goto L15144;
	case 14:  goto L15146;
	case 15:  goto L15150;
	case 16:  goto L15160;
	case 17:  goto L15190;
    }
/*            APPLYA APPLYS ARRAY MAP  MAPC  MAPCA MAPLI OPEN0 PRIN0 */
/*     PUT    RPLST SETA  SETI  SETR SUBPAIR SUBSTR SYSERROR */
/* ----------------------------------------------------------------------- */
/* SUBRN  ENTRY */

/*             IT IS HERE UP TO THE SUBRN TO TAKE CARE OF THE ARGS */

L3290:
    ll = l - b_1.subr3;
    switch (ll) {
	case 1:  goto L16000;
	case 2:  goto L16010;
	case 3:  goto L16040;
	case 4:  goto L16070;
	case 5:  goto L16080;
    }
/*          CONCAT LIST  PLUS SYSFLA TIMES */

/* RETURN CHANNELS (3105 ALSO) */
L3300:
    if (b_1.iflg2 - b_1.nil != 0) {
	goto L16005;
    } else {
	goto L998;
    }
/* ----------------------------------------------------------------------- */
/*             SUBR 0 */
/* ----------------------------------------------------------------------- */

/* READC */

L10010:
    shift_(&c__2);
L10000:
    if (b_1.cht <= 0) {
	goto L10010;
    }
    if (b_1.cht < 13 || b_1.cht >= 23) {
	goto L10020;
    }
    *ires = b_1.cht - 13 + a_1.numadd;
    goto L10030;
L10020:
    b_1.abuff[0] = b_1.chr;
    *ires = matom_(&c__1);
L10030:
    shift_(&c__2);
    goto L998;

/* ALIST */

L10040:
    local = jaan_1.env;
    formfl = b_1.arg != b_1.nil;
    b_1.temp1 = cons_(&b_1.nil, &b_1.nil);
    b_1.temp2 = b_1.temp1;
    if (local <= 0) {
	goto L10044;
    }
    goto L10042;
L10041:
    if (! formfl) {
	goto L10037;
    }
    if (jaan_1.jack[k - 1] != b_1.sform) {
	goto L10037;
    }
/* *SETC*      CALL SETCDR(TEMP1,CONS(CONS(JACK(K),JILL(K)),NIL)) */
    i__1 = cons_(&jaan_1.jack[k - 1], &jaan_1.jill[k - 1]);
    carcdr_1.cdr[b_1.temp1 - 1] = cons_(&i__1, &b_1.nil);
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
L10037:
    local = -jaan_1.jack[local - 1];
    if (local <= 0) {
	goto L10044;
    }
L10042:
    locale = jaan_1.jill[local - 1] + 2;
    k = local;
L10043:
    --k;
    if (k <= 0) {
	goto L10044;
    }
    if (k < locale) {
	goto L10041;
    }
/* *SETC*      CALL SETCDR(TEMP1,CONS( CONS(JACK(K),JILL(K)),NIL)) */
    i__1 = cons_(&jaan_1.jack[k - 1], &jaan_1.jill[k - 1]);
    carcdr_1.cdr[b_1.temp1 - 1] = cons_(&i__1, &b_1.nil);
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    goto L10043;
L10044:
    *ires = carcdr_1.cdr[b_1.temp2 - 1];
    goto L998;

/* EJECT */

L10050:
    eject_(&b_1.lunut);
    goto L3090;

/* EXIT */

L10060:
    lspex_();

/* GENSYM */

L10070:
    i__2 = mknum_(&b_1.numgen);
    i__1 = cons_(&i__2, &b_1.nil);
    b_1.arg = cons_(&b_1.a000, &i__1);
    ++b_1.numgen;
    b_1.arg2 = b_1.nil;
    goto L12520;
/*             TO PACK */

/* BTV* */
L10075:
    shostk_();
    *ires = b_1.nil;
    goto L998;

/* CLOCK */

L10080:
    i__1 = mslft_(&i__);
    *ires = mknum_(&i__1);
    goto L998;

/* RATOM */

L10090:
    b_1.brflg = b_1.nil;
    i__ = ratom_(ires, &c__0);
    goto L998;

/* READ */

L10100:
    *ires = iread_(&c__0);
    goto L998;

/* READP */
L10105:
    if (b_1.cht > 1) {
	goto L3100;
    }
    if (b_1.rdpos > b_1.margr) {
	goto L3090;
    }
    shift_(&c__2);
    goto L10105;

/* RESET = GOTO 1 */


/* TERPRI */

L10110:
    terpri_();
    b_1.prtpos = b_1.lmarg;
    goto L3090;

/* TIME */
L10120:
    mtime_(b_1.buff);
/*                            CHANGE 5 IF YOUR TIME FORMAT IS NOT 5 LONG */
    for (i__ = 1; i__ <= 5 || i__ == 1; ++i__) {
	getch_(b_1.buff, &b_1.temp1, &i__);
	b_1.abuff[i__ - 1] = b_1.temp1;
/* L10125: */
    }
/*                    THE TWO 5'S BELOW MUST ALSO BE CHANGED */
    b_1.abup1 = 5;
    *ires = matom_(&c_n5);
    goto L998;

/* DATE */
L10130:
    mdate_(b_1.buff);
/*              YOU CAN CHANGE THE 10 IN THE DO STATEMENT BELOW IF YOU */
/*              HAVE A DATE FORMAT THAT IS NOT 10 CHARACTERS LONG */
    for (i__ = 1; i__ <= 20 || i__ == 1; ++i__) {
	getch_(b_1.buff, &b_1.temp1, &i__);
	b_1.abuff[i__ - 1] = b_1.temp1;
/* L10135: */
    }
/*              THE TWO OCCURENCES OF 10 BELOW MUST ALSO BE CHANGED */
    b_1.abup1 = 20;
    *ires = matom_(&c_n20);
    goto L998;
/* ----------------------------------------------------------------------- */
/*        SUBR 1 (WITH NUMERICAL ARG IN N) */
/* ----------------------------------------------------------------------- */

/* ADD1 */

L11000:
    ++(*n);
    irflag = b_1.nil;
    goto L3110;

/* ERRORMESS */

L11010:
    if (*n < 1 || *n > a_1.maxmes) {
	goto L25000;
    }
    mess_(n);
    goto L998;

/* RECLAIM */

L11020:
    i__1 = b_1.nargs;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
	if (i__ == 4) {
	    goto L11030;
	}
	if (i__ != 5) {
	    args[i__ - 1] = b_1.nil;
	}
L11030:
	;
    }
    if (*n < 0 || *n > 3) {
	goto L25000;
    }
    i__1 = garb_(n);
    *ires = mknum_(&i__1);
    goto L998;

/* ROLLIN */

L11050:
    if (*n < 1 || *n > b_1.maxlun) {
	goto L25000;
    }
    *ires = rollin_(n);
    if (*ires == b_1.nil) {
	goto L998;
    }
    goto L1;

/* ROLLOUT */

L11060:
    if (*n < 1 || *n > b_1.maxlun) {
	goto L25000;
    }
    rollou_(n);
    goto L998;

/* SUB1 */

L11070:
    --(*n);
    irflag = b_1.nil;
    goto L3110;

/* REWIND */

L11080:
    if (*n < 1 || *n > b_1.maxlun) {
	goto L25000;
    }
    rew_(n);
    goto L998;
/* ----------------------------------------------------------------------- */
/*       SUBR 1 (OTHERS) */
/* ----------------------------------------------------------------------- */
/* ALIST 10040 */

/* ARRAYSIZE */

L11190:
    for (i__ = 1; i__ <= 3 || i__ == 1; ++i__) {
/* L11195: */
	arrutl_(&b_1.arg, &c__3, &i__, &j, &args[i__ + 4]);
    }
    if (b_1.ibreak) {
	goto L2400;
    }
    i__2 = b_1.temp1 + b_1.temp2 + b_1.temp3;
    i__1 = mknum_(&i__2);
    i__4 = mknum_(&b_1.temp2);
    i__6 = mknum_(&b_1.temp3);
    i__5 = cons_(&i__6, &b_1.nil);
    i__3 = cons_(&i__4, &i__5);
    *ires = cons_(&i__1, &i__3);
    goto L998;
/* ARRAYP */
L11197:
    if (b_1.arg > a_1.natom) {
	goto L3090;
    }
    if (carcdr_1.car[b_1.arg - 1] == b_1.array) {
	goto L998;
    }
    goto L3090;

/* ATOM */

L11200:
    if (b_1.arg > a_1.nfreet) {
	goto L3100;
    }
L11205:
    if (b_1.arg > a_1.natom) {
	goto L3090;
    }
    s__1 = (shortint) carcdr_1.car[b_1.arg - 1];
    if (s__1 <= b_1.substr && s__1 >= b_1.array) {
	goto L3090;
    }
    goto L3100;

/* C...R */

/*      THE CODE BELOW MUST BE CHANGED IF CAR NO LONGER IS THE */
/*      FIFT NON-NUMERIC SUBR1. */

L11210:
    ll += -3;
L11211:
    if (b_1.arg > a_1.nfreet) {
	goto L25000;
    }
    i__ = ll % 2;
    if (i__ == 0) {
	*ires = carcdr_1.car[b_1.arg - 1];
    }
    if (i__ != 0) {
	*ires = carcdr_1.cdr[b_1.arg - 1];
    }
    ll /= 2;
    if (ll - 1 <= 0) {
	goto L998;
    } else {
	goto L11211;
    }

/* EVAL */
L11215:
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 19;
    goto L1600;

/* EVLIS */

L11220:
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 19;
    goto L2000;

/* FIXP */
L11225:
    if (b_1.arg <= a_1.nfreet) {
	goto L3090;
    }
    if (b_1.arg > a_1.bignum) {
	goto L998;
    }
    goto L3090;

/* FLOATP */
L11227:
    if (b_1.arg <= a_1.nfreet) {
	goto L3090;
    }
    if (b_1.arg <= a_1.bignum) {
	goto L998;
    }
    goto L3090;

/* GETD */

L11230:
    k = get_(&b_1.arg, &b_1.fncell);
    if (k == b_1.nil) {
	goto L11240;
    }
    *ires = k;
    goto L998;
L11240:
    if (b_1.arg > b_1.fsubr) {
	goto L3090;
    }
    if (b_1.arg == b_1.nil) {
	goto L3090;
    }
    if (b_1.arg > b_1.subr) {
	goto L11250;
    }
    *ires = cons_(&b_1.expr, &b_1.arg);
    goto L998;
L11250:
    *ires = cons_(&b_1.fexpr, &b_1.arg);
    goto L998;

/* LAST */
L11255:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L998;
    }
L11256:
    b_1.temp1 = carcdr_1.cdr[b_1.arg - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L998;
    }
    b_1.arg = carcdr_1.cdr[b_1.arg - 1];
    goto L11256;

/* LENGTH */

L11260:
    k = 0;
L11270:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L11280;
    }
    ++k;
    b_1.arg = carcdr_1.cdr[b_1.arg - 1];
    goto L11270;
L11280:
    *ires = mknum_(&k);
    goto L998;

/* LISTP */

L11290:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L3090;
    }
    goto L998;

/* LITATOM = 11205 */

/* NLISTP */

L11310:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L3100;
    }
    goto L3090;

/* NULL */

L11320:
    if (b_1.arg == b_1.nil) {
	goto L3100;
    }
    goto L3090;

/* NUMBERP */

L11330:
    if (b_1.arg <= a_1.nfreet) {
	goto L3090;
    }
    goto L998;

/* OBLIST */

L11340:
    min__ = b_1.arg;
    *ires = b_1.nil;
    i__1 = min__;
    i__2 = a_1.natomp;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	s__1 = (shortint) carcdr_1.car[i__ - 1];
	if (! (s__1 <= b_1.substr && s__1 >= b_1.array)) {
	    *ires = cons_(&i__, ires);
	}
/* L11350: */
    }
    goto L998;

/*                                           !  PROMPT SHOULD BE A COMMON */
/* PROMPTTEXT (STRING) = OLD STRING          !   BLOCK CONTAINING */
/*                                           !    CURRENT PROMPT CH */
L11355:
    if (b_1.arg > a_1.natom) {
	goto L3090;
    }
    ii = b_1.arg;
    i__2 = prompt_1.prolen;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
/* L11356: */
	b_1.abuff[i__ - 1] = prompt_1.protxt[i__ - 1];
    }
    *ires = matom_(&prompt_1.prolen);
    if (ii == b_1.nil) {
	goto L998;
    }
    i__ = getpn_(&ii, &main, &jb, &prompt_1.prolen);
    if (prompt_1.prolen > 80) {
	prompt_1.prolen = 80;
    }
    i__2 = prompt_1.prolen;
    for (i__ = 1; i__ <= i__2 || i__ == 1; ++i__) {
/* L11357: */
	i__1 = jb + i__ - 1;
	getch_(b_1.pname, &prompt_1.protxt[i__ - 1], &i__1);
    }
    goto L998;

/* PROG1 = GOTO 998 */
/* RETURN = 20150 */

/* REVERSE */

L11360:
    iret = 1;
/*          SEQUENCE USED ALSO BY APPEND */
L11370:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L11410;
    }
    b_1.temp1 = b_1.nil;
L11390:
    icar = carcdr_1.car[b_1.arg - 1];
    b_1.temp1 = cons_(&icar, &b_1.temp1);
    b_1.arg = carcdr_1.cdr[b_1.arg - 1];
    if (! (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet)) {
	goto L11390;
    }
/* L11400: */
    *ires = b_1.temp1;
/*            RETURN LIST */
    switch (iret) {
	case 1:  goto L998;
	case 2:  goto L12070;
    }
/*             RETURN ATOM */
L11410:
    switch (iret) {
	case 1:  goto L998;
	case 2:  goto L12060;
    }

L11415:
    if (b_1.arg > a_1.natom) {
	goto L3090;
    }
    if (carcdr_1.car[b_1.arg - 1] == b_1.string) {
	goto L998;
    }
    if (carcdr_1.car[b_1.arg - 1] == b_1.substr) {
	goto L998;
    }
    goto L3090;
/* ZEROP */

L11420:
    if (b_1.arg == a_1.numadd) {
	goto L3100;
    }
    goto L3090;
/* ----------------------------------------------------------------------- */
/*             SUBR 2 */
/* ----------------------------------------------------------------------- */

/* ADDLIST */

L12000:
    i__ = b_1.arg2;
L12010:
    if (i__ <= a_1.natom || i__ > a_1.nfreet) {
	goto L12020;
    }
    if (b_1.arg == carcdr_1.car[i__ - 1]) {
	goto L12030;
    }
    i__ = carcdr_1.cdr[i__ - 1];
    goto L12010;
L12020:
    *ires = cons_(&b_1.arg, &b_1.arg2);
    goto L998;
L12030:
    *ires = b_1.arg2;
    goto L998;

/* ALPHORDER */

/* ** CHANGED BY TR TO BE EQUIVALENT WITH INTERLISP */
L12040:
    if (b_1.arg <= a_1.nfreet || b_1.arg2 <= a_1.nfreet) {
	goto L12041;
    }
    r__ = gtreal_(&b_1.arg, &i__);
    s = gtreal_(&b_1.arg2, &j);
    if (r__ == 0.f) {
	r__ = (real) i__;
    }
    if (s == 0.f) {
	s = (real) j;
    }
    if (r__ - s <= 0.f) {
	goto L3100;
    } else {
	goto L3090;
    }
L12041:
    if (b_1.arg > a_1.nfreet) {
	goto L3100;
    }
    if (b_1.arg2 > a_1.nfreet) {
	goto L3090;
    }
    if (b_1.arg > a_1.natom) {
	goto L3100;
    }
/*            HERE ARG = LITERAL ATOM */
    if (b_1.arg2 > a_1.natom) {
	goto L3100;
    }
/*            HERE ALSO ARG2 = LITERAL ATOM */
    if (comppn_(&b_1.arg, &b_1.arg2) <= 0) {
	goto L3100;
    } else {
	goto L3090;
    }

/* APPEND */

L12050:
    iret = 2;
    goto L11370;
/*             TO REVERSE */
L12060:
    *ires = b_1.arg2;
/*             ATOM RETURNED */
    goto L998;
L12070:
    i__ = carcdr_1.cdr[b_1.arg - 1];
/*             LIST RETURNED */
/* *SETC*      CALL SETCDR(ARG,ARG2) */
    carcdr_1.cdr[b_1.arg - 1] = b_1.arg2;
    if (i__ == b_1.nil) {
	goto L998;
    }
    b_1.arg2 = b_1.arg;
    b_1.arg = i__;
    goto L12070;


/* CONS */

L12090:
    *ires = cons_(&b_1.arg, &b_1.arg2);
    goto L998;

/* DIFFERENCE */

L12100:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    irflag = b_1.nil;
    r__ = gtreal_(&b_1.arg, &i__);
    s = gtreal_(&b_1.arg2, &j);
    if (r__ != 0.f || s != 0.f) {
	goto L12105;
    }
    *n = i__ - j;
    goto L3110;
L12105:
    irflag = b_1.t;
    if (r__ == 0.f) {
	r__ = (real) i__;
    }
    if (s == 0.f) {
	s = (real) j;
    }
    r__ -= s;
    goto L3110;

/* ELT */

L12112:
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    i__1 = getnum_(&b_1.arg2);
    arrutl_(&b_1.arg, &c__1, &c__1, &i__1, &b_1.temp1);
    if (b_1.ibreak) {
	goto L2400;
    }
    *ires = jpname[b_1.temp1 - 1];
    goto L998;

/* ELTI */

L12114:
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    i__1 = getnum_(&b_1.arg2);
    arrutl_(&b_1.arg, &c__1, &c__2, &i__1, &b_1.temp1);
    if (b_1.ibreak) {
	goto L2400;
    }
    *ires = mknum_(&ipname[b_1.temp1 - 1]);
    goto L998;

/* ELTR */

L12116:
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    i__1 = getnum_(&b_1.arg2);
    arrutl_(&b_1.arg, &c__1, &c__3, &i__1, &b_1.temp1);
    if (b_1.ibreak) {
	goto L2400;
    }
    *ires = mkreal_(&b_1.pname[b_1.temp1 - 1]);
    goto L998;

/* EQ */

L12120:
    if (b_1.arg - b_1.arg2 != 0) {
	goto L3090;
    } else {
	goto L3100;
    }

/* EQUAL */

L12130:
    *ires = equal_(&b_1.arg, &b_1.arg2);
    goto L998;

/* EVALA */

L12140:
    iprev = jaan_1.tops;
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.sform;
    jaan_1.jill[jaan_1.tops - 1] = b_1.arg2;
L12141:
    b_1.temp1 = carcdr_1.car[b_1.arg2 - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L12142;
    }
    if (jaan_1.tops > jaan_1.hill - 60) {
	goto L12141;
    }
    ++jaan_1.tops;
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    jaan_1.jack[jaan_1.tops - 1] = carcdr_1.car[b_1.temp1 - 1];
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.cdr[b_1.temp1 - 1];
    goto L12141;
L12142:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = 0;
    jaan_1.jill[jaan_1.tops - 1] = iprev;
    apush_(&jaan_1.env);
    jaan_1.env = jaan_1.tops;
    fpush_(&c__20);
    goto L1600;
/* --R20        RETURN FROM EVAL(ARG) */
L12150:
    apop_(&jaan_1.env);
    jaan_1.tops = jaan_1.jill[jaan_1.tops - 1];
    goto L998;

/* EVSTK */
L12160:
    iprev = jaan_1.tops;
    b_1.temp1 = getnum_(&b_1.arg2);
    if (b_1.temp1 > jaan_1.tops || b_1.temp1 != 0 && jaan_1.jack[b_1.temp1 - 
	    1] > 0) {
	goto L25035;
    }
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -b_1.temp1;
    jaan_1.jill[jaan_1.tops - 1] = iprev;
    apush_(&jaan_1.env);
    jaan_1.env = jaan_1.tops;
    fpush_(&c__5);
    goto L1600;
/* R-5 */
L12185:
    jaan_1.tops = jaan_1.jill[jaan_1.tops - 1];
    apop_(&jaan_1.env);
    goto L998;
/* GETP */

L12190:
    *ires = get_(&b_1.arg, &b_1.arg2);
    goto L998;

/* GREATERP */

L12200:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    r__ = gtreal_(&b_1.arg, &i__);
    s = gtreal_(&b_1.arg2, &j);
    if (r__ == 0.f) {
	r__ = (real) i__;
    }
    if (s == 0.f) {
	s = (real) j;
    }
    if (r__ - s <= 0.f) {
	goto L3090;
    } else {
	goto L3100;
    }

/* IQUOREM */

L12205:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    j = getnum_(&b_1.arg2);
    if (j == 0) {
	goto L25010;
    }
    i__1 = getnum_(&b_1.arg);
    i__3 = i__1 / j;
    i__2 = mknum_(&i__3);
    i__5 = i__1 % j;
    i__4 = mknum_(&i__5);
    *ires = cons_(&i__2, &i__4);
    goto L998;

/* LESSP */

L12210:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    r__ = gtreal_(&b_1.arg, &i__);
    s = gtreal_(&b_1.arg2, &j);
    if (r__ == 0.f) {
	r__ = (real) i__;
    }
    if (s == 0.f) {
	s = (real) j;
    }
    if (r__ - s >= 0.f) {
	goto L3090;
    } else {
	goto L3100;
    }

/* MAPC (ARG, ARG2, ARG3) */

L12220:
    i__ = 1;
L12230:
    --b_1.jp;
    b_1.stack[b_1.jp - 1] = i__;
/*             SEQUENCE USED ALSO BY MAP, MAPCAR, MAPLIST */
L12240:
    b_1.jp += -3;
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L12300;
    }
    b_1.stack[b_1.jp + 1] = b_1.arg2;
    b_1.stack[b_1.jp] = b_1.arg3;
    b_1.stack[b_1.jp - 1] = b_1.arg;
L12250:
    icar = b_1.arg;
    if (b_1.stack[b_1.jp + 2] == 1 || b_1.stack[b_1.jp + 2] == 3) {
	icar = carcdr_1.car[b_1.arg - 1];
    }
    b_1.arg3 = b_1.stack[b_1.jp];
    b_1.arg = b_1.stack[b_1.jp + 1];
    b_1.arg2 = cons_(&icar, &b_1.nil);
    fpush_(&c__14);
    goto L1500;

/* --R14        RETURN FROM APPLY */

L12260:
    if (b_1.stack[b_1.jp + 2] < 3) {
	goto L12270;
    }
    ist = b_1.stack[b_1.jp + 3];
/* *SETC*      CALL SETCDR(IST,CONS(IRES,NIL)) */
    carcdr_1.cdr[ist - 1] = cons_(ires, &b_1.nil);
    b_1.stack[b_1.jp + 3] = carcdr_1.cdr[ist - 1];
L12270:
    b_1.arg2 = b_1.stack[b_1.jp - 1];
    b_1.arg = b_1.stack[b_1.jp];
    if (b_1.arg == b_1.nil) {
	goto L12280;
    }
/*        THIRD ARGUMENT NOT NIL. APPLY STEP FUNCTION */
    b_1.arg2 = cons_(&b_1.arg2, &b_1.nil);
    fpush_(&c__21);
    goto L1500;
L12280:
    b_1.arg = carcdr_1.cdr[b_1.arg2 - 1];

/* --R21        RETURN FROM APPLY(ARG,ARG2) */

L12290:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L12300;
    }
    b_1.stack[b_1.jp - 1] = b_1.arg;
    goto L12250;
L12300:
    b_1.jp += 4;
    if (b_1.stack[b_1.jp - 2] < 3) {
	goto L3090;
    }
    icar = b_1.stack[b_1.jp - 1];
/* *SETC*      CALL SETCDR(ICAR,ARG) */
    carcdr_1.cdr[icar - 1] = b_1.arg;
    icar = b_1.stack[b_1.jp];
    *ires = carcdr_1.cdr[icar - 1];
    b_1.jp += 2;
    goto L998;

/* MAPCAR */

L12310:
    i__ = 3;
L12320:
    b_1.jp += -3;
    b_1.stack[b_1.jp + 1] = cons_(&b_1.nil, &b_1.nil);
    b_1.stack[b_1.jp] = b_1.stack[b_1.jp + 1];
    b_1.stack[b_1.jp - 1] = i__;
    goto L12240;

/* MAP */

L12330:
    i__ = 2;
    goto L12230;

/* MAPLIST */

L12340:
    i__ = 4;
    goto L12320;

/* OPEN0 */

L12345:
    i__2 = cons_(&b_1.arg2, &b_1.arg3);
    i__1 = cons_(&b_1.arg, &i__2);
    i__ = openf_(&i__1);
    if (i__ > 0) {
	*ires = mknum_(&i__);
    }
    if (i__ <= 0) {
	*ires = b_1.nil;
    }
    goto L998;


/* MEMB */

L12350:
    i__ = b_1.arg;
    *ires = b_1.arg2;
L12360:
    if (*ires <= a_1.natom || *ires > a_1.nfreet) {
	goto L3090;
    }
    if (i__ == carcdr_1.car[*ires - 1]) {
	goto L998;
    }
    *ires = carcdr_1.cdr[*ires - 1];
    goto L12360;

/* MEMBER */

L12390:
    i__ = b_1.arg;
    *ires = b_1.arg2;
L12400:
    if (*ires <= a_1.natom || *ires > a_1.nfreet) {
	goto L3090;
    }
    icar = carcdr_1.car[*ires - 1];
    if (equal_(&i__, &icar) != b_1.nil) {
	goto L998;
    }
    *ires = carcdr_1.cdr[*ires - 1];
    goto L12400;

/* NCHARS */

L12430:
    if (b_1.arg > a_1.natom) {
	goto L12431;
    }
    if (b_1.arg2 == b_1.nil && carcdr_1.car[b_1.arg - 1] != b_1.array) {
	goto L12460;
    }
L12431:
    i__1 = cons_(&b_1.arg, &b_1.nil);
    *ires = nchars_(&i__1, &b_1.arg2);
/* 12440 = RETURN ALSO FROM PACK, UNPACK, PUTINT, CONCAT */
L12440:
    apop_(&b_1.prtpos);
    i__1 = b_1.marg;
    for (i__ = 1; i__ <= i__1 || i__ == 1; ++i__) {
/* L12450: */
	b_1.prbuff[i__ - 1] = b_1.buff[i__ - 1];
    }
    goto L998;
L12460:
    if (0 > getpn_(&b_1.arg, &main, &jb, &ipl)) {
	goto L25020;
    }
    *ires = mknum_(&ipl);
    goto L998;

/* NCONC */

L12470:
    i__ = 0;
    b_1.temp1 = b_1.arg;
L12480:
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L12490;
    }
    i__ = b_1.temp1;
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    goto L12480;
L12490:
    if (i__ == 0) {
	goto L12060;
    }
/* *SETC*      CALL SETCDR(I, ARG2) */
    carcdr_1.cdr[i__ - 1] = b_1.arg2;
    goto L998;

/* NCONC1 */

L12500:
    b_1.arg2 = cons_(&b_1.arg2, &b_1.nil);
    goto L12470;

/* NEQ */

L12510:
    if (b_1.arg - b_1.arg2 != 0) {
	goto L3100;
    } else {
	goto L3090;
    }

/* NTH */
L12515:
    if (b_1.arg2 <= a_1.bignum) {
	goto L25020;
    }
    if (b_1.arg2 <= a_1.numadd) {
	goto L12519;
    }
    *n = b_1.arg2 - 1;
L12516:
    if (*n <= a_1.numadd) {
	goto L998;
    }
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L998;
    }
    b_1.arg = carcdr_1.cdr[b_1.arg - 1];
    --(*n);
    goto L12516;
/*     INTERLISP GIVES (CONS NIL ARG) FOR N<=0 */
L12519:
    *ires = cons_(&b_1.nil, &b_1.arg);
    goto L998;

/* PACK */

L12520:
    b_1.arg2 = nchars_(&b_1.arg, &b_1.arg2) - a_1.numadd;
    b_1.arg2 = b_1.prtpos - 1;
/*                                       SAVE CHT,CHR */
/*                                      ALSO FROM GETINT */
/* L12530: */
    if (b_1.arg2 > 0) {
	goto L12535;
    }
    *ires = matom_(&c__0);
    goto L12440;
L12535:
    n1 = b_1.cht;
    n2 = b_1.chr;
    b_1.cht = 0;
    b_1.prtpos = 1;
    b_1.iflg2 = b_1.t;
    *ires = iread_(&c__1);
    b_1.iflg2 = b_1.nil;
    b_1.cht = n1;
    b_1.chr = n2;
    goto L12440;

/* QUOTIENT */

L12540:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    irflag = b_1.nil;
    r__ = gtreal_(&b_1.arg, &i__);
    s = gtreal_(&b_1.arg2, &j);
    if (r__ != 0.f || s != 0.f) {
	goto L12545;
    }
    if (j == 0) {
	goto L25010;
    }
    *n = i__ / j;
    goto L3110;
L12545:
    irflag = b_1.t;
    if (r__ == 0.f) {
	r__ = (real) i__;
    }
    if (s == 0.f) {
	s = (real) j;
    }
    if (s == 0.f) {
	goto L25010;
    }
    r__ /= s;
    goto L3110;

/* RPLACA */

L12570:
    iret = 1;
L12580:
    if (b_1.arg == b_1.nil || b_1.arg == b_1.t) {
	goto L25020;
    }
    if (b_1.arg > a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg > a_1.natom) {
	goto L12590;
    }
    s__1 = (shortint) carcdr_1.car[b_1.arg - 1];
    if (s__1 <= b_1.substr && s__1 >= b_1.array) {
	goto L25020;
    }
/* *SETC*12590 CALL SETCAR(ARG, ARG2) */
L12590:
    carcdr_1.car[b_1.arg - 1] = b_1.arg2;
    if (iret == 2) {
	b_1.arg = b_1.arg2;
    }
    goto L998;

/* RPLACD */

L12600:
    if (b_1.arg == b_1.nil) {
	goto L25020;
    }
    if (b_1.arg > a_1.nfreet) {
	goto L25020;
    }
    if (b_1.arg > a_1.natom) {
	goto L12610;
    }
    s__1 = (shortint) carcdr_1.car[b_1.arg - 1];
    if (s__1 <= b_1.substr && s__1 >= b_1.array) {
	goto L25020;
    }
/* *SETC*12610 CALL SETCDR(ARG, ARG2) */
L12610:
    carcdr_1.cdr[b_1.arg - 1] = b_1.arg2;
    goto L998;

/* RPT */

L12620:
    if (b_1.arg <= a_1.bignum) {
	goto L25020;
    }
    apush_(&b_1.arg);
    *n = b_1.arg;
    b_1.arg = b_1.arg2;
    apush_(&b_1.arg2);
L12630:
    if (*n <= a_1.numadd) {
	goto L12650;
    }
    fpush_(&c__22);
    goto L1600;

/* --R22       RETURN FROM EVAL(ARG) */

L12640:
    *n = b_1.stack[b_1.jp] - 1;
    b_1.stack[b_1.jp] = *n;
    b_1.arg = b_1.stack[b_1.jp - 1];
    goto L12630;
L12650:
    b_1.jp += 2;
    goto L3090;

/* SASSOC */
L12655:
    if (b_1.arg2 <= a_1.natom || b_1.arg2 > a_1.nfreet) {
	goto L12659;
    }
    b_1.temp1 = carcdr_1.car[b_1.arg2 - 1];
    if41 = carcdr_1.car[b_1.temp1 - 1];
    if (equal_(&b_1.arg, &if41) != b_1.nil) {
	goto L12658;
    }
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    goto L12655;
L12658:
    *ires = b_1.temp1;
    goto L998;
L12659:
    *ires = b_1.arg2;
    goto L998;

/* SET */

L12660:
    iret = 2;
    if (b_1.arg > a_1.natom) {
	goto L25020;
    }
    local = jaan_1.env;
    if (local <= 0) {
	goto L12580;
    }
/*                                      ! TO RPLACA */
    goto L12675;
L12670:
    local = -jaan_1.jack[local - 1];
    if (local <= 0) {
	goto L12580;
    }
L12675:
    locale = jaan_1.jill[local - 1] + 2;
    k = local;
L12680:
    --k;
    if (k < locale) {
	goto L12670;
    }
    if (jaan_1.jack[k - 1] != b_1.arg) {
	goto L12680;
    }
    jaan_1.jill[k - 1] = b_1.arg2;
    *ires = b_1.arg2;
    goto L998;
/* STRALLOC */

L12690:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    i__ = getnum_(&b_1.arg);
    if (0 > getpn_(&b_1.arg2, &main, &jb, &ipl)) {
	goto L25010;
    }
    i__1 = -i__;
    *ires = matom_(&i__1);
    if (*ires == b_1.nil || i__ == 0) {
	goto L998;
    }
    getch_(b_1.pname, &ich, &jb);
    i__1 = i__;
    for (j = 1; j <= i__1 || j == 1; ++j) {
	putch_(b_1.pname, &ich, &a_1.jbp);
/* L12710: */
	++a_1.jbp;
    }
    goto L998;

/* ASSOC */

L12715:
    if (b_1.arg2 <= a_1.natom || b_1.arg2 > a_1.nfreet) {
	goto L12719;
    }
    b_1.temp1 = carcdr_1.car[b_1.arg2 - 1];
    if (b_1.arg == carcdr_1.car[b_1.temp1 - 1]) {
	goto L12718;
    }
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    goto L12715;
L12718:
    *ires = b_1.temp1;
    goto L998;
L12719:
    *ires = b_1.arg2;
    goto L998;


/* CHTAB */

L12720:
    if (b_1.arg > a_1.natom) {
	goto L25020;
    }
    jb = b_1.pnp[b_1.arg - 1];
    getch_(b_1.pname, &ich, &jb);
    *ires = getcht_(&ich) + a_1.numadd;
    if (b_1.arg2 == b_1.nil) {
	goto L998;
    }
    *n = b_1.arg2 - a_1.numadd;
    if (*n < 1 || *n > a_1.nchtyp) {
	goto L25010;
    }
    setcht_(&ich, n);
    goto L998;

/* IOTAB */

L12730:
    n1 = b_1.arg - a_1.numadd;
    if (n1 < 1 || n1 > 10) {
	goto L25020;
    }
    *ires = mknum_(&iotab[n1 - 1]);
    if (n1 == 2) {
	--(*ires);
    }
    if (b_1.arg2 == b_1.nil) {
	goto L998;
    }
    *n = b_1.iobuff;
    if (n1 != 1) {
	goto L12735;
    }
    if (b_1.arg2 == b_1.t) {
	b_1.arg2 = b_1.lunins + a_1.numadd;
    }
    goto L12740;
L12735:
    if (n1 != 5) {
	goto L12745;
    }
    if (b_1.arg2 == b_1.t) {
	b_1.arg2 = b_1.lunuts + a_1.numadd;
    }
L12740:
    *n = b_1.maxlun;
L12745:
    if (n1 >= 9) {
	*n = a_1.maxint;
    }
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25010;
    }
    n2 = getnum_(&b_1.arg2);
    if (n2 < 1 || n2 > *n) {
	goto L25010;
    }
    iotab[n1 - 1] = n2;
    if (n1 == 2) {
	shift_(&c__2);
    }
    goto L998;
/* TAILP */
L12746:
    if (b_1.arg2 <= a_1.natom || b_1.arg2 > a_1.nfreet) {
	goto L3090;
    }
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    if (b_1.arg == b_1.arg2) {
	goto L998;
    }
    goto L12746;

/* UNPACK */

L12750:
    i__1 = cons_(&b_1.arg, &b_1.nil);
    nch = nchars_(&i__1, &b_1.arg2);
    *ires = b_1.nil;
L12760:
    --b_1.prtpos;
    if (b_1.prtpos == 0) {
	goto L12440;
    }
/*                               12440 = RETURN TO NCHARS AND RESET PRBUF */
    ic = b_1.prbuff[b_1.prtpos - 1];
    ict = getcht_(&ic);
    if (ict < 13 || ict > 22) {
	goto L12770;
    }
    i__1 = ict - 13 + a_1.numadd;
    *ires = cons_(&i__1, ires);
    goto L12760;
L12770:
    b_1.abuff[0] = ic;
    i__1 = matom_(&c__1);
    *ires = cons_(&i__1, ires);
    goto L12760;

/* XCALL */

L12780:
    if (b_1.arg <= a_1.nfreet) {
	goto L25020;
    }
    i__1 = getnum_(&b_1.arg);
    *ires = xcall_(&i__1, &b_1.arg2);
    goto L998;
/* ----------------------------------------------------------------------- */
/*             SUBR 3 */
/* ----------------------------------------------------------------------- */

/* APPLYA */

L15000:
    iprev = jaan_1.tops;
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.sform;
    jaan_1.jill[jaan_1.tops - 1] = b_1.arg3;
L15001:
    b_1.temp1 = carcdr_1.car[b_1.arg3 - 1];
    if (b_1.temp1 <= a_1.natom || b_1.temp1 > a_1.nfreet) {
	goto L15002;
    }
    if (jaan_1.tops > jaan_1.hill - 60) {
	goto L15001;
    }
    ++jaan_1.tops;
    b_1.arg3 = carcdr_1.cdr[b_1.arg3 - 1];
    jaan_1.jack[jaan_1.tops - 1] = carcdr_1.car[b_1.temp1 - 1];
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.cdr[b_1.temp1 - 1];
    goto L15001;
L15002:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = 0;
    jaan_1.jill[jaan_1.tops - 1] = iprev;
    apush_(&jaan_1.env);
    jaan_1.env = jaan_1.tops;
    fpush_(&c__18);
    goto L1500;

/* --R18    RETURN FROM APPLY */

L15010:
    apop_(&jaan_1.env);
    jaan_1.tops = jaan_1.jill[jaan_1.tops - 1];
    goto L998;
/*  APPLYSTK */
L15012:
    iprev = jaan_1.tops;
    b_1.temp1 = getnum_(&b_1.arg3);
    if (b_1.temp1 > jaan_1.tops || b_1.temp1 != 0 && jaan_1.jack[b_1.temp1 - 
	    1] > 0) {
	goto L25036;
    }
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -b_1.temp1;
    jaan_1.jill[jaan_1.tops - 1] = iprev;
    apush_(&jaan_1.env);
    jaan_1.env = jaan_1.tops;
    fpush_(&c__1);
    goto L1500;
/* R-1 */
L15013:
    jaan_1.tops = jaan_1.jill[jaan_1.tops - 1];
    apop_(&jaan_1.env);
    goto L998;

/* ARRAY */

L15015:
    b_1.i1cons = b_1.arg;
    for (ireg = 1; ireg <= 3 || ireg == 1; ++ireg) {
	b_1.arg = args[ireg - 1];
	if (b_1.arg <= a_1.nfreet) {
	    goto L25030;
	}
	args[ireg + 4] = getnum_(&b_1.arg);
	if (args[ireg + 4] < 0) {
	    goto L25030;
	}
/* L15016: */
    }
    b_1.temp1 = b_1.temp1 - b_1.temp2 - b_1.temp3;
    if (b_1.temp1 >= 0) {
	goto L15017;
    }
    b_1.arg = b_1.i1cons;
    goto L25030;
L15017:
    i__1 = -((b_1.temp1 + 3) * a_1.jbytes + (b_1.temp2 + 1) * a_1.ibytes + (
	    b_1.temp3 + 1) * a_1.bytes - 3);
    *ires = matom_(&i__1);
    if (*ires == b_1.nil) {
	goto L998;
    }
/* *SETC*      CALL SETCAR(IRES, ARRAY) */
    carcdr_1.car[*ires - 1] = b_1.array;
    a_1.jbp = b_1.pnp[*ires - 1];
    for (ireg = 1; ireg <= 3 || ireg == 1; ++ireg) {
/* L15019: */
	arrutl_(ires, &c__4, &ireg, &c__0, &args[ireg + 4]);
    }
    goto L998;

/* PRIN0 */

L15020:
    b_1.temp1 = b_1.dreg[4];
    b_1.temp2 = b_1.dreg[1];
    b_1.temp3 = b_1.dreg[6];
    b_1.dreg[4] = b_1.arg2;
    b_1.dreg[1] = b_1.nil;
    b_1.dreg[6] = b_1.nil;
    if (b_1.arg3 == b_1.nil) {
	goto L15030;
    }
    b_1.dreg[1] = b_1.t;
    if (b_1.arg3 > a_1.nfreet) {
	b_1.dreg[6] = b_1.t;
    }
L15030:
    prin1_(&b_1.arg);
    b_1.dreg[4] = b_1.temp1;
    b_1.dreg[1] = b_1.temp2;
    b_1.dreg[6] = b_1.temp3;
    goto L998;

/* MAPC = 12220 */

/* MAPCAR = 12290 */


/* PUT */

L15040:
    b_1.temp1 = b_1.arg;
    if (b_1.temp1 == b_1.nil || b_1.temp1 > a_1.nfreet) {
	goto L25030;
    }
    s__1 = (shortint) carcdr_1.car[b_1.temp1 - 1];
    if (s__1 <= b_1.substr && s__1 >= b_1.array) {
	goto L25030;
    }
L15050:
    i__ = carcdr_1.cdr[b_1.temp1 - 1];
    if (! (i__ <= a_1.natom || i__ > a_1.nfreet)) {
	goto L15060;
    }
/* *SETC*      CALL SETCDR(TEMP1, CONS(ARG2,CONS(ARG3,NIL))) */
    i__1 = cons_(&b_1.arg3, &b_1.nil);
    carcdr_1.cdr[b_1.temp1 - 1] = cons_(&b_1.arg2, &i__1);
    goto L15070;
L15060:
    b_1.temp1 = i__;
    i__ = carcdr_1.cdr[b_1.temp1 - 1];
    if (carcdr_1.car[b_1.temp1 - 1] != b_1.arg2) {
	goto L15080;
    }
/* *SETC*      CALL SETCAR(I, ARG3) */
    carcdr_1.car[i__ - 1] = b_1.arg3;
L15070:
    *ires = b_1.arg3;
    goto L998;
L15080:
    b_1.temp1 = i__;
    goto L15050;

/* RPLSTRING */

L15130:
    b_1.temp1 = b_1.arg;
    if (1 != getpn_(&b_1.arg, &main, &jb, &ipl)) {
	goto L25030;
    }
    b_1.arg = b_1.arg2;
    if (b_1.arg <= a_1.nfreet) {
	goto L25030;
    }
    i__ = getnum_(&b_1.arg);
    if (i__ <= 0) {
	goto L25030;
    }
    jb += i__ - 1;
    ipl -= i__ - 1;
    if (ipl <= 0) {
	goto L3080;
    }
    b_1.arg = b_1.arg3;
    if (1 != getpn_(&b_1.arg, &main, &jb2, &ipl2)) {
	goto L25030;
    }
/*                                      NEVER RPL OUTSIDE GOAL STRING */
    if (ipl > ipl2) {
	ipl = ipl2;
    }
    if (ipl == 0) {
	goto L3080;
    }
    imax = jb2 + ipl - 1;
    i__1 = jb2;
    i__2 = imax;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	getch_(b_1.pname, &ich, &i__);
	putch_(b_1.pname, &ich, &jb);
/* L15140: */
	++jb;
    }
    goto L3080;

/* SETA */

L15142:
    jndex = (a_1.numbp - 1) * a_1.bytes / a_1.jbytes;
    jpname[jndex - 1] = (shortint) b_1.arg3;
    ireg = 1;
    goto L15148;

/* SETI */

L15144:
    if (b_1.arg3 <= a_1.nfreet) {
	goto L25026;
    }
    jndex = (a_1.numbp - 1) * a_1.bytes / a_1.ibytes;
    ipname[jndex - 1] = getnum_(&b_1.arg3);
    ireg = 2;
    goto L15148;

/* SETR */

L15146:
    if (b_1.arg3 <= a_1.nfreet) {
	goto L25026;
    }
    jndex = a_1.numbp - 1;
    r__ = gtreal_(&b_1.arg3, &i__);
    if (r__ == 0.f) {
	r__ = (real) i__;
    }
    b_1.pname[jndex - 1] = r__;
    ireg = 3;
L15148:
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25025;
    }
    i__2 = getnum_(&b_1.arg2);
    arrutl_(&b_1.arg, &c__2, &ireg, &i__2, &jndex);
    if (b_1.ibreak) {
	goto L2400;
    }
    *ires = b_1.arg3;
    goto L998;

/* SUBPAIR (OLD NEW S) */

L15150:
    *ires = subpr_(&b_1.arg, &b_1.arg2, &b_1.arg3);
    goto L998;

/* SUBSTRING */

L15160:
    if (1 != getpn_(&b_1.arg, &main, &jb, &ipl)) {
	goto L25030;
    }
/*                                      LOOK AT 2ND ARG */
    b_1.temp1 = b_1.arg;
    b_1.arg = b_1.arg2;
    if (b_1.arg2 <= a_1.nfreet) {
	goto L25030;
    }
    b_1.arg2 = getnum_(&b_1.arg2);
    if (b_1.arg2 <= 0) {
	b_1.arg2 = ipl + 1 + b_1.arg2;
    }
    if (b_1.arg2 < 1 || b_1.arg2 > ipl + 1) {
	goto L3090;
    }
/*                                      LOOK AT 3RD ARG */
    b_1.arg = b_1.arg3;
    if (b_1.arg3 == b_1.nil) {
	b_1.arg3 = mknum_(&ipl);
    }
    if (b_1.arg3 <= a_1.nfreet) {
	goto L25030;
    }
    b_1.arg3 = getnum_(&b_1.arg3);
    if (b_1.arg3 < 0) {
	b_1.arg3 = ipl + 1 + b_1.arg3;
    }
    if (b_1.arg3 < b_1.arg2 - 1 || b_1.arg3 > ipl) {
	goto L3090;
    }
/*                                      MAKE THE SUBSTRING */
    *ires = matom_(&c__0);
    i__ = getpn_(&b_1.temp1, &main, &jb, &ipl);
    if (*ires == b_1.nil) {
	goto L998;
    }
/* *SETC*      CALL SETCAR(IRES, SUBSTR) */
    carcdr_1.car[*ires - 1] = b_1.substr;
/* *SETC*      CALL SETCDR(IRES, CONS (MAIN, CONS */
    i__3 = b_1.arg2 + jb - b_1.pnp[main - 1];
    i__1 = mknum_(&i__3);
    i__5 = b_1.arg3 + 1 - b_1.arg2;
    i__4 = mknum_(&i__5);
    i__2 = cons_(&i__1, &i__4);
    carcdr_1.cdr[*ires - 1] = cons_(&main, &i__2);
/* *SETC*     *) */

    goto L998;


/* SYSERROR */

L15190:
    i__2 = b_1.arg - a_1.numadd;
    mess_(&i__2);
    iprint_(&b_1.arg2);
    iprint_(&b_1.arg3);
    goto L1;
/* ----------------------------------------------------------------------- */
/*               SUBR N */
/* ----------------------------------------------------------------------- */

/* CONCAT */

L16000:
    b_1.iflg2 = b_1.t;
    goto L16010;
/*      (FROM LIST)                     ARG NOW = ARGLIST */
L16005:
    b_1.temp1 = matom_(&c__0);
/*                                      TERPRI WILL TEST OFLO */
/*                                      TEMP1 WILL GUARD FROM GARB */
    nch = nchars_(&b_1.arg, &b_1.nil);
    terpri_();
    b_1.iflg2 = b_1.nil;
    *ires = b_1.temp1;
    goto L12440;

/* LIST */

L16010:
    *ires = b_1.nil;
    if (iargs == 0) {
	goto L3300;
    }
    max__ = index + iargs - 1;
    i__2 = index;
    i__1 = max__;
    for (kalle = i__2; kalle <= i__1 || kalle == i__2; ++kalle) {
	i__ = max__ - kalle + index;
	b_1.temp1 = jaan_1.jill[i__ - 1];
/* L16020: */
	*ires = cons_(&b_1.temp1, ires);
    }
    goto L3300;

/* PLUS */

L16040:
    *n = 0;
    irflag = b_1.nil;
    if (iargs == 0) {
	goto L3105;
    }
    max__ = index + iargs - 1;
    i__1 = index;
    i__2 = max__;
    for (i__ = i__1; i__ <= i__2 || i__ == i__1; ++i__) {
	ist = jaan_1.jill[i__ - 1];
	if (ist <= a_1.nfreet) {
	    goto L25040;
	}
	s = gtreal_(&ist, &j);
	if (s != 0.f || irflag != b_1.nil) {
	    goto L16044;
	}
	*n += j;
	goto L16060;
L16044:
	if (irflag != b_1.nil) {
	    goto L16046;
	}
	irflag = b_1.t;
	r__ = (real) (*n);
L16046:
	if (s == 0.f) {
	    s = (real) j;
	}
	r__ += s;
L16060:
	;
    }
    goto L3105;

/* SYSFLAG */

L16070:
    *ires = b_1.nil;
    if (iargs == 0) {
	goto L3300;
    }
    *n = jaan_1.jill[index - 1] - a_1.numadd;
    max__ = iargs + index - 1;
    if (*n < 1 || *n > 7) {
	goto L25040;
    }
    *ires = b_1.dreg[*n - 1];
    if (iargs == 1) {
	goto L3300;
    }
    b_1.temp1 = jaan_1.jill[index];
    if (b_1.temp1 > b_1.t) {
	goto L25040;
    }
    b_1.dreg[*n - 1] = b_1.temp1;
    goto L3300;

/* TIMES */

L16080:
    *n = 1;
    irflag = b_1.nil;
    if (iargs == 0) {
	goto L3105;
    }
    max__ = iargs + index - 1;
    i__2 = index;
    i__1 = max__;
    for (i__ = i__2; i__ <= i__1 || i__ == i__2; ++i__) {
	ist = jaan_1.jill[i__ - 1];
	if (ist <= a_1.nfreet) {
	    goto L25040;
	}
	s = gtreal_(&ist, &j);
	if (s != 0.f || irflag != b_1.nil) {
	    goto L16084;
	}
	*n *= j;
	goto L16090;
L16084:
	if (irflag != b_1.nil) {
	    goto L16086;
	}
	irflag = b_1.t;
	r__ = (real) (*n);
L16086:
	if (s == 0.f) {
	    s = (real) j;
	}
	r__ *= s;
L16090:
	;
    }
    goto L3105;
/* ----------------------------------------------------------------------- */
/*             FSUBR */
/*             THIS IS A SEPARATE ENTRYPOINT ACCESSED DIRECTLY FROM EVAL */
/*               L = FSUBR, ARG2 = ARGUMENT LIST. */
/* ----------------------------------------------------------------------- */
L18000:
    ll = l - b_1.subr;
    b_1.form = b_1.arg;
    b_1.arg = b_1.arg2;
/* L18003: */
    switch (ll) {
	case 1:  goto L18070;
	case 2:  goto L18135;
	case 3:  goto L2100;
	case 4:  goto L18010;
	case 5:  goto L18030;
	case 6:  goto L18020;
	case 7:  goto L18140;
	case 8:  goto L20060;
	case 9:  goto L2600;
	case 10:  goto L18040;
	case 11:  goto L18200;
	case 12:  goto L18050;
    }
/*         AND  BINDENV COND  FUNCTION GO   GO*   OR PROG PROGN */
/*        QUOTE SELECTQ SETQ */

/* COND = GOTO 2100 */

/* FUNCTION */

L18010:
    icar = carcdr_1.car[b_1.arg - 1];
    icdr = carcdr_1.cdr[b_1.arg - 1];
    if (! (icdr <= a_1.natom || icdr > a_1.nfreet)) {
	goto L18011;
    }
    *ires = icar;
    goto L999;
/*                                    ! THE FUNCTION */
L18011:
    apush_(&icar);
    icdr = carcdr_1.car[icdr - 1];
/*                                    ! THE LIST */
    if (icdr <= a_1.natom || icdr > a_1.nfreet) {
	icdr = b_1.nil;
    }
/*      ICDR=CAR(ICDR)   THIS LINE REMOVED   20 October 1984 */
    apush_(&icdr);
    b_1.arg = icdr;
    fpush_(&c__27);
    goto L2000;
/* R--27 */

L18012:
    apop_(&icdr);
    apop_(&icar);
    if (icdr == b_1.nil) {
	goto L18017;
    }
    b_1.temp1 = cons_(&b_1.nil, &b_1.nil);
    b_1.temp2 = b_1.temp1;
L18013:
    if (*ires <= a_1.natom || *ires > a_1.nfreet) {
	goto L18015;
    }
    if41 = carcdr_1.car[icdr - 1];
    if42 = carcdr_1.car[*ires - 1];
/* *SETC*      CALL SETCDR(TEMP1,CONS(CONS(IF41,IF42),NIL)) */
    i__1 = cons_(&if41, &if42);
    carcdr_1.cdr[b_1.temp1 - 1] = cons_(&i__1, &b_1.nil);
    b_1.temp1 = carcdr_1.cdr[b_1.temp1 - 1];
    icdr = carcdr_1.cdr[icdr - 1];
    *ires = carcdr_1.cdr[*ires - 1];
    goto L18013;
L18015:
    b_1.temp2 = carcdr_1.cdr[b_1.temp2 - 1];
    i__2 = cons_(&b_1.temp2, &b_1.nil);
    i__1 = cons_(&icar, &i__2);
    *ires = cons_(&b_1.funarg, &i__1);
    goto L999;
L18017:
    i__1 = cons_(&icar, &b_1.nil);
    *ires = cons_(&b_1.funarg, &i__1);
    goto L999;

/* GO* */

L18020:
    iret = 2;
    b_1.arg = carcdr_1.car[b_1.arg - 1];
    goto L20000;

/* GO */

L18030:
    b_1.arg = carcdr_1.car[b_1.arg - 1];
    iret = 1;
    goto L20000;

/* QUOTE */

L18040:
    *ires = carcdr_1.car[b_1.arg - 1];
    goto L999;

/* SETQ */

L18050:
    icar = carcdr_1.car[b_1.arg - 1];
    apush2_(&l, &icar);
    icdr = carcdr_1.cdr[b_1.arg - 1];
    if (icdr <= a_1.natom || icdr > a_1.nfreet) {
	icdr = b_1.nil;
    }
    b_1.arg = carcdr_1.car[icdr - 1];
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 10;
/*                                          ! CALL FPUSH(10) */
    goto L1600;

/* --R10        RETURN FROM EVAL */

L18060:
    b_1.arg2 = *ires;
    apop2_(&b_1.arg, &l);
    ++jaan_1.tops;
    jaan_1.jill[jaan_1.tops - 1] = jaan_1.tops - 1;
    goto L12660;
/*             TO SET */

/* AND */

L18069:
    b_1.arg = b_1.t;
    goto L999;
L18070:
    if (b_1.arg == b_1.nil) {
	goto L18069;
    }
    apush_(&b_1.arg);
    k = b_1.arg;
L18090:
    b_1.arg = carcdr_1.car[k - 1];
    fpush_(&c__12);
    goto L1600;

/* --R12        RETURN FROM EVAL */

L18100:
    if (*ires <= b_1.nil) {
	goto L18120;
    }
    k = b_1.stack[b_1.jp - 1];
    k = carcdr_1.cdr[k - 1];
    if (! (k <= a_1.natom || k > a_1.nfreet)) {
	goto L18130;
    }
L18120:
    ++b_1.jp;
    goto L999;
L18130:
    b_1.stack[b_1.jp - 1] = k;
    goto L18090;

/* BINDENV */
L18135:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L18139;
    }
    b_1.arg = carcdr_1.car[b_1.arg - 1];
    local = jaan_1.env;
    if (local <= 0) {
	goto L18139;
    }
    goto L18137;
L18136:
    local = -jaan_1.jack[local - 1];
    if (local <= 0) {
	goto L18139;
    }
L18137:
    locale = jaan_1.jill[local - 1] + 2;
    k = local;
L18138:
    --k;
    if (k < locale) {
	goto L18136;
    }
    if (jaan_1.jack[k - 1] != b_1.arg) {
	goto L18138;
    }
    b_1.temp1 = -jaan_1.jack[local - 1];
    *ires = mknum_(&b_1.temp1);
    goto L999;
L18139:
    *ires = a_1.numadd;
    goto L999;


/* OR */

L18140:
    apush_(&b_1.arg);
    k = b_1.arg;
L18160:
    b_1.arg = carcdr_1.car[k - 1];
    fpush_(&c__13);
    goto L1600;

/* --R13        RETURN FROM EVAL */

L18170:
    if (*ires > b_1.nil) {
	goto L18120;
    }
    icdr = b_1.stack[b_1.jp - 1];
    k = carcdr_1.cdr[icdr - 1];
    if (k <= a_1.natom || k > a_1.nfreet) {
	goto L18120;
    }
    b_1.stack[b_1.jp - 1] = k;
    goto L18160;

/* SELECTQ */

L18200:
    b_1.arg = carcdr_1.car[b_1.arg - 1];
    apush_(&b_1.form);
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 15;
    goto L1600;

/* --R15        RETURN FROM EVAL */

L18210:
    apop_(&b_1.form);
    b_1.arg2 = carcdr_1.cdr[b_1.form - 1];
L18220:
    b_1.arg2 = carcdr_1.cdr[b_1.arg2 - 1];
    if (carcdr_1.cdr[b_1.arg2 - 1] == b_1.nil) {
	goto L18280;
    }
/*             TEST IF ARG EQ OR MEMB CAAR (ARG2) */
/*             EQ-TEST */
    icar = carcdr_1.car[b_1.arg2 - 1];
    if (icar <= a_1.natom || icar > a_1.nfreet) {
	goto L25080;
    }
    icar = carcdr_1.car[icar - 1];
    if (b_1.arg == icar) {
	goto L18270;
    }
L18240:
    if (icar <= a_1.natom || icar > a_1.nfreet) {
	goto L18220;
    }
/*             MEMB-TEST */
    if (b_1.arg == carcdr_1.car[icar - 1]) {
	goto L18270;
    }
    icar = carcdr_1.cdr[icar - 1];
    goto L18240;
/*             SUCCESS. SET-UP ARG AND JUMP TO EVLAST */
L18270:
    icar = carcdr_1.car[b_1.arg2 - 1];
    b_1.arg = carcdr_1.cdr[icar - 1];
    goto L2600;
/*             FAILURE. SET-UP ARG AND JUMP TO EVAL */
L18280:
    b_1.arg = carcdr_1.car[b_1.arg2 - 1];
    goto L1600;
/* ----------------------------------------------------------------------- */
/*             PROG FEATURES */

/* PROGN = 2600 (EVLAST) */

/* GO */

L20000:
    i__ = b_1.ipp;
    j = b_1.jpp;
L20010:
    if (i__ <= 0) {
	goto L20050;
    }
    k = b_1.stack[j - 2];
L20020:
    if (k <= a_1.natom || k > a_1.nfreet) {
	goto L20040;
    }
    if (b_1.arg == carcdr_1.car[k - 1]) {
	goto L20030;
    }
    k = carcdr_1.cdr[k - 1];
    goto L20020;
/*               LABEL FOUND. RESET TO PROG STATUS AND */
/*               CONTINUE PROG LOOP. */
L20030:
    b_1.ipp = i__;
    b_1.jpp = j;
    b_1.ip = b_1.ipp + 3;
    b_1.jp = b_1.jpp - 2;
    jaan_1.tops = b_1.stack[b_1.ip - 1];
    jaan_1.env = jaan_1.tops;
    b_1.arg = carcdr_1.cdr[k - 1];
    b_1.stack[b_1.jp - 1] = b_1.arg;
    goto L20110;
/*               LABEL NOT FOUND */
L20040:
    j = b_1.stack[i__ + 1];
    i__ = b_1.stack[i__];
    goto L20010;
/*     PROG LABEL NOT FOUND, TEST IF GO OR GO* */
L20050:
    if (iret == 2) {
	goto L999;
    }
/*              GO*, NORMAL RETURN */
    b_1.errtyp = 5;
    goto L2400;
/*              GO, CALL SYSERROR */

/* PROG */

/*             PROLOGUE OF PROG */
L20060:
    iprev = jaan_1.tops;
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.sform;
    jaan_1.jill[jaan_1.tops - 1] = b_1.form;
    icar = carcdr_1.car[b_1.arg - 1];
/*                                     ! THATS VARLIST */
    b_1.jp += -2;
/*                                     ! MAKE PLACE FOR 2 ITEMS ON STACK */
    b_1.stack[b_1.jp] = carcdr_1.cdr[b_1.arg - 1];
/*                                     ! THE PROGBODY */

/*        SPREAD ARGS ON STACK */
/*        ==================== */

L20070:
    if (icar <= a_1.natom) {
	goto L20093;
    }
/*                                     ! READY */
    b_1.temp1 = carcdr_1.car[icar - 1];
    if (b_1.temp1 <= a_1.natom) {
	goto L20090;
    }
/*                                     ! SET VAR TO NIL */
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = carcdr_1.car[b_1.temp1 - 1];
    b_1.temp2 = carcdr_1.cdr[b_1.temp1 - 1];
    jaan_1.jill[jaan_1.tops - 1] = carcdr_1.car[b_1.temp2 - 1];
    goto L20091;
L20090:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = b_1.temp1;
    jaan_1.jill[jaan_1.tops - 1] = b_1.nil;
L20091:
    icar = carcdr_1.cdr[icar - 1];
    goto L20070;
/*        READY FIX BLOCK */
L20093:
    ++jaan_1.tops;
    jaan_1.jack[jaan_1.tops - 1] = -iprev;
    jaan_1.jill[jaan_1.tops - 1] = iprev + 1;


/*      EVAL PROGVARS */
/*      ============= */

L20094:
    ++jaan_1.jill[jaan_1.tops - 1];
    if (jaan_1.jill[jaan_1.tops - 1] == jaan_1.tops) {
	goto L20099;
    }
/*                                            ! READY */
    b_1.arg = jaan_1.jill[jaan_1.jill[jaan_1.tops - 1] - 1];
    fpush_(&c__23);
    goto L1600;
/* R--23 */
L20095:
    if41 = jaan_1.jill[jaan_1.tops - 1];
    jaan_1.jill[if41 - 1] = *ires;
    goto L20094;
L20099:
    jaan_1.jill[jaan_1.tops - 1] = -jaan_1.jack[jaan_1.tops - 1];
    jaan_1.jack[jaan_1.tops - 1] = -jaan_1.env;
    jaan_1.env = jaan_1.tops;

/*               ENTER PROG. NEW STACK POSITIONS ARE */
/*               CURRENT BODY, BODY, TOPS */
/*               JP            JP+1  JP+2 */

/* L20100: */
    b_1.arg = b_1.stack[b_1.jp];
    b_1.stack[b_1.jp - 1] = b_1.arg;
    b_1.stack[b_1.ip + 2] = jaan_1.tops;
    b_1.stack[b_1.ip + 1] = b_1.jpp;
    b_1.stack[b_1.ip] = b_1.ipp;
    b_1.ip += 3;
    b_1.ipp = b_1.ip - 3;
    b_1.jpp = b_1.jp + 2;
/*               START PROG LOOP */
L20110:
    if (b_1.arg <= a_1.natom || b_1.arg > a_1.nfreet) {
	goto L20150;
    }
    b_1.arg = carcdr_1.car[b_1.arg - 1];
    if (b_1.arg <= a_1.natom) {
	goto L20130;
    }
/*                                           ! A LABEL */
/*             FPUSH(11) */
    ++b_1.ip;
    b_1.stack[b_1.ip - 1] = 11;
    goto L1600;

/* --R11        RETURN FROM EVAL */

L20130:
    icdr = b_1.stack[b_1.jp - 1];
    b_1.arg = carcdr_1.cdr[icdr - 1];
    b_1.stack[b_1.jp - 1] = b_1.arg;
    if (b_1.arg != b_1.nil) {
	goto L20110;
    }
    goto L20170;

/* RETURN */

L20150:
    if (b_1.ipp < 0) {
	goto L25130;
    } else if (b_1.ipp == 0) {
	goto L20160;
    } else {
	goto L20165;
    }
L20160:
    b_1.errtyp = 6;
    goto L2400;
L20165:
    jaan_1.tops = jaan_1.jill[jaan_1.tops - 1];
L20170:
    b_1.ip = b_1.ipp;
    if (b_1.ipp < 0) {
	goto L25130;
    }
    b_1.jp = b_1.jpp;
    jaan_1.tops = b_1.stack[b_1.ip + 2];
    b_1.jpp = b_1.stack[b_1.ip + 1];
    b_1.ipp = b_1.stack[b_1.ip];
    goto L997;
/* ----------------------------------------------------------------------- */

/*             RESET AND ERROR RETURNS. */

/* ----------------------------------------------------------------------- */

/*             ERRORS WHICH APPLY ERROR FUNCTION */

/* SUBR1 ERRORS */

L25000:
    b_1.errtyp = 7;
    goto L2400;

/* SUBR2 ERRORS */

L25010:
    b_1.arg = b_1.arg2;
L25020:
    b_1.errtyp = 8;
    goto L2400;

/* SUBR3 ERRORS */

L25025:
    b_1.arg = b_1.arg2;
    goto L25030;
L25026:
    b_1.arg = b_1.arg3;
L25030:
    b_1.errtyp = 9;
    goto L2400;
/* ENVIRONMENT ERRORS */
L25035:
    b_1.arg = b_1.arg2;
    goto L25037;
L25036:
    b_1.arg = b_1.arg3;
L25037:
    b_1.errtyp = 18;
    goto L2400;

/* SUBRN ERROR ON ENTRY HERE, ARGS ARE ON THE A-STACK */
/*             MAX POINTS TO THE FIRST ARG,JP POINTS TO THE LAST. */
/*             IARGS=NR AF ARGS */
/*             BEFORE CALLING ERRORFUNCTION RESET ARG TO THE LIST OF ARGS */
L25040:
    b_1.errtyp = 10;
/* L25050: */
    b_1.arg = b_1.nil;
    if (iargs == 0) {
	goto L2400;
    }
    i__1 = max__;
    i__2 = index;
    for (i__ = i__1; i__ >= i__2 || i__ == i__1; --i__) {
/* L25070: */
	b_1.arg = cons_(&jaan_1.jill[i__ - 1], &b_1.arg);
    }
    goto L2400;

/* FSUBR ERROR */

L25080:
    l = carcdr_1.car[b_1.form - 1];
    b_1.arg = carcdr_1.cdr[b_1.form - 1];
    b_1.errtyp = 23;
    goto L2400;

/*             ERRORS THAT RESTART THE SYSTEM */

/* PDL FULL */
/* --R16 */

L25090:
    b_1.errtyp = 12;
    if (b_1.middl < 10) {
	goto L1;
    }
    b_1.middl /= 2;
    goto L2400;
L25095:
    b_1.errtyp = 15;
    jaan_1.hillw += 65;
    if (jaan_1.hillw > jaan_1.hill - 20) {
	goto L25096;
    }
    goto L2400;
L25096:
    mess_(&c__11);
    goto L1;

/* PDL EMPTY */
/* --R17 */

L25100:
    b_1.errtyp = 13;
    b_1.form = b_1.nil;
    b_1.arg = b_1.nil;
    l = b_1.lispx;
    goto L2400;

/* INTERNAL TROUBLE (INVALID STACK POINTER) */

L25130:
    mess_(&c__16);
/*                                      DUMMY RETURN */
    if (FALSE_) {
	return 0;
    }
    goto L1;
} /* lispf4_ */

#undef jpname
#undef ipname
#undef rargs
#undef iotab
#undef n
#undef ires
#undef narr
#undef args



/* Subroutine */ static int shostk_(void)
{
    /* Local variables */
    extern /* Subroutine */ int mess_(integer *), prin1_(integer *);
    static integer isav9, k;
#define iotab ((integer *)&b_1.lunin)	/*  ((integer *)&b_1 + 65)  */
    static integer isav10;
    extern /* Subroutine */ int priint_(integer *), terpri_(void), iprint_(
	    integer *);

/* OMMON AND INTEGER DECLARATIONS */
/* OMMON AND INTEGER DECLARATIONS END */
    isav10 = iotab[9];
    isav9 = iotab[8];
/*                                             ! WE SAVE OLD PRINT LEVEL */
/*                                             ! AND LENGTH. WE USE 2 AND 3 */
/*                                             ! IN THIS OUTPUT. */
    iotab[9] = 2;
    iotab[8] = 3;
    k = jaan_1.tops;

    mess_(&c__40);
    b_1.prtpos = 6;
    priint_(&jaan_1.env);
    b_1.prtpos = 17;
    priint_(&jaan_1.tops);
    terpri_();
L1212:
    if (k <= 0) {
	goto L9999;
    }
    b_1.temp2 = jaan_1.jill[k - 1];
    b_1.temp1 = jaan_1.jack[k - 1];
    if (b_1.temp1 <= 0) {
	goto L777;
    }
    b_1.prtpos = 1;
    b_1.prbuff[b_1.prtpos - 1] = chars_1.nochar;
    ++b_1.prtpos;
    priint_(&k);
    b_1.prtpos = 6;
/* L1213: */
    prin1_(&b_1.temp1);
    ++b_1.prtpos;
    iprint_(&b_1.temp2);
    --k;
    goto L1212;
L777:
    if (b_1.ibreak) {
	goto L9999;
    }
    b_1.prtpos = 1;
    b_1.prbuff[b_1.prtpos - 1] = chars_1.nochar;
    ++b_1.prtpos;
    priint_(&k);
    b_1.prtpos = 6;
    b_1.prbuff[b_1.prtpos - 1] = chars_1.ilbchr;
    ++b_1.prtpos;
    priint_(&b_1.temp1);
    b_1.prtpos += 2;
    priint_(&b_1.temp2);
    b_1.prbuff[b_1.prtpos - 1] = chars_1.irbchr;
    ++b_1.prtpos;
    terpri_();
    --k;
    goto L1212;
L9999:
    mess_(&c__14);
    terpri_();
    iotab[9] = isav10;
    iotab[8] = isav9;
    return 0;
} /* shostk_ */

#undef iotab


