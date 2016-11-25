

8/23/00 (revised 2/21/15)

Blake McBride
blake@mcbride.name
http://blake.mcbride.name

This is an Interlisp interpreter originally written in Fortran but
converted to C via the F2C Fortran to C converter.  This release has
been tested under Windows, Linux, and Mac and no longer requires F2C
or its associated runtime library.

I had used a slightly older version of this system back in the early
80's and found it very portable, reliable, functional, and fast enough.
I learned Lisp with it and spent quite a bit of time with it back then.

The author (back in 1983) is Mats Nordstrom from Uppsala, Sweden.  I
kept it around, ported it, enhanced some lisp code, and converted it
into C.  Sometime in the late 90's Mats was kind enough to give me a
copy of the latest version he had (8/22/83).  He also gave me
permission to release it so long as I retained his credits.

Interlisp is a dynamically scoped lisp system.  It has no macro facility
but supports LAMBDA (evaluates function arguments), NLAMBDA (doesn't
evaluate its arguments), and variable number of arguments.  Macros are
not hard to simulate.

The system contains no special optimizations such as P-code but has some
good debugging and editing facilities.  It also contains all the basics
such as floating point numbers, arrays, and property lists.

In all the time I've used it I have never found a bug in the base
system (the part written in Fortran) and it has never crashed on me.

The conversion steps I performed are as follows:

1.  Convert the system to C and got it running via the F2C program.

2.  Replace the Fortran calls to equivalent C calls to get rid of the
need for the Fortran support library.

3.  Enabled the use of command line arguments to control startup
options.

4.  Changed memory usage to allow runtime capacity specifications.

5.  Changed some code to make it a little more portable.  (The system
should be highly portable in general though.)

Since I have modified the converted C code you should not attempt to
go from the Fortran code to C without loosing all of my changes.
The Fortran code as-is will not run with F2C without a few tweaks.

The system successfully builds on 32 and 64 bit machines.  You may
need to make some adjustments in f2c.h

The following files are included in the top directory:

lispf41.f	Fortran source - part 1 of 2
lispf42.f	Fortran source - part 2 of 2
lispf4?.c	C version of Fortran code
auxillary.c	Auxiliary C functions
f4com.for	the Fortran common declarations
SYSATOMS	system atoms needed when generating a new system
Makefile.win	makefile for Windows NT using NMAKE
Makefile.unx	makefile for Linux or Mac
*.l		Lisp source files
script.?	scripts to build the image files
f2c.h		used by the .c files
lisf4.orig	the entire original, untouched system

Under Documentation:

README.txt
UsersGuide.txt
DevelopmentProcess.txt
ImplementationGuide.txt
Haraldson-LISP_details.pdf
Interlisp-Oct_1978.pdf
p501-schorr.pdf

Pre-built executables are located under the Windows, Linux & Mac
directories.


Note that you do not want to re-convert the Fortran into C since I
have hand-modified the C files.  All work should be done in the C
files only.  The Fortran code is for reference only.


Building the system
-------------------

Make lispf4.exe and the two image files by typing:

Window/MSVC:   nmake -f Makefile.win

Linux or Mac:   make -f Makefile.unx 


Running the system
------------------

In addition to loading lisp source files, lispf4 has the ability to
save/load binary images of a running system.  This enables you to load
an entire pre-loaded image in a short time.  Initially, if no image
file is being loaded, the system requires the SYSATOMS file in order
to build its internal system.  If an image file is being loaded the
SYSATOMS file is not needed or used.  Typically, one builds an image
which contains the common lisp functions and uses that as the base
system.

The makefile builds two image files.  BARE.IMG contains only SYSATOMS.
BASIC.IMG contains SYSATOMS and most of the common lisp files.  These
common lisp files flesh out the system and would normally be used.

The command line options are as follows:

	lispf4  [-cN]  [-aN]  [-sN] [-pN]  [FILE.IMG]

	N = a number (no space between the option and N)
	c = car/cdr cells (default 100000)
	a = atoms (default 3000)
	s = stack space (default 1500)
	p = print names / strings / reals / arrays (default 5000)
	FILE.IMG = an image file name

For example Lispf4 can be started in the following ways:

	lispf4

This causes a bare system to startup and SYSATOMS will be loaded.

	lispf4  BASIC.IMG

This causes lispf4 to startup and load BASIC.IMG.  SYSATOMS is not used.

	lispf4 -c200000  BASIC.IMG

This causes lispf4 to set the number of cons cells to 200000 and load
BASIC.IMG.

You cannot mix different startup parameters with different image
files.  In other words, you cannot run the system with one
configuration, save an image, and then load it with a lispf4 which was
started with a different set of parameters.



----------------------------------------------------------------------

The raw system is case sensitive and all the built in functions are in
upper case.  BASIC.IMG has a lisp file loaded which sets an option to
cause lispf4 to up shift all input so the system becomes case
insensitive.

Lispf4 came with the super-parenthesis defined as <>.  I changed that
to [] in order to match InterLisp.

To exit the system just type:   (EXIT)

Once the system is built you can save and load images as follows:
(Note that this will only work if the proper lisp packages which
perform these operations are loaded.  These files are loaded in
BASIC.IMG.)

(SYSOUT "file.img")
(SYSIN  "file.img")

You can also use the makefile lisp package (in MAKEF.L) to load and
save groups of lisp functions in ASCII format as follows:

(CURFILE  name) ; names the group - all functions defined from that point
		  are part of that named group

(MAKEFILE name file T) ; saves all the functions associated with name to file

(LOAD file) ; loads the file

----------------------------------------------------------------------

XCALL
-----

Opening files:

(XCALL 1 '(lfn file status form))

	lfn    = logical file number
	file   = file name
	status = OLD or NEW
	form   = FORMATTED or UNFORMATTED
                 FORMATTED = ASCII files
                 UNFORMATTED = binary or sysout files

Closing files:

(XCALL 2 lfn)

----------------------------------------------------------------------

Supported Interlisp Functions
-----------------------------

primitive functions
-------------------
CAR
CDR
CAR & CDR 3 deep
CONS
RPLACD
RPLACA
QUOTE
COND
SELECTQ
PROG1
PROGN
PROG
GO
RETURN
SET
SETQ
SETQQ
GETTOPVAL
SETTOPVAL

ADD1VAR
EQLENGTH
GETLIS
INTERSECTION
LASTN
LISTGET
LISTGET1
RPLNODE
RPLNODE2
LISTPUT
LISTPUT1
LSUBST
MAP2C
MAP2CAR
MAPCON
MAPCONC
NLEFT
SUB1VAR
NOTANY
NOTEVERY
RATOMS
DOCOLLECT
ENDCOLLECT
UNION


Function Definition
-------------------
GETD
PUTD
PUTDQ
GETDQ
DEFINE
DEFINEQ
SAVEDEF
UNSAVEDEF
EVAL
BOUNDP
APPLY
EVALA
RPT
RPTQ
DE  (define lambda function)
DF  (define nlambda function)
DM  (define macro)



List Manipulation
-----------------
LIST
APPEND
NCONC
NCONC1
TCONC
LCONC
COPY
REVERSE
DREVERSE
SUBPAIR
SUBST
SUBLIS
LAST
NTH
LENGTH
DSORT
ALPHORDER
REMOVE
DREMOVE



Predicates and logical connectives
----------------------------------
ATOM
LITATOM
NUMBERP
STRINGP
LISTP
NLISTP
EQ
NEQ
NULL
EQUAL
AND
OR
MEMB
MEMBER
EVERY
SOME
ASSOC
SASSOC
PUTASSOC


Property Lists
--------------
GETPROPLIST
SETPROPLIST
GETPROP
PUTPROP
PUTPROPS
ADDPROP
REMPROP
REMPROPLIST
CHANGEPROP
PROPNAMES

SYSPROPS is a variable which contains a list of all the system
properties.


Math Functions
--------------
ADD1
SUB1
ZEROP
MINUSP
PLUS
MINUS
DIFFERENCE
TIMES
QUOTIENT
REMIANDER
GREATERP
LESSP
GEQ
LEQ
MIN
MAX
ABS


Print Names & Strings
---------------------
PACK
UNPACK
NCHARS
GENSYM
STRINGP
STREQUAL
MKSTRING
SUBSTRING
CONCAT
RPLSTRING
MKATOM


Map Functions
-------------
FUNCTION
MAP
MAPC
MAPLIST
MAPCAR


Terminal IO
-----------
READ
RATOM
RSTRING
READC

PRIN1
PRIN2
PRINT
SPACES
TERPRI
PRINTLEVEL
PP
PRINTPOS


File IO
-------
OPEN
MAKEFILE
READFILE
LOAD
CLOSE
CURFILE
PP
IOTAB
FILECREATED


System
-------
ROLLIN
ROLLOUT
SYSIN
SYSOUT
GCGAG
RECLAIM


Debugging
---------
BREAK0
BREAK1
BREAK11
UNBREAK
REBREAK
TRACE
UNTRACE

VIRGINFN

ADVISE
UNADVISE
READVISE

HELP
NLSETQ
ERSETQ

----------------------------------------------------------------------

Conditional (IF) package (IFDO.L)
------------------------

Requires MATCH.L

This function is compatible with both the MIT version and the keyword
version at UCB.

  simple summary:
   non-keyword use:
       (IF a b) ==> (COND (a b))
       (IF a b c d e ...) ==> (COND (a b) (t c d e ...))
   with keywords:
       (IF a THEN b) ==> (COND (a b))
       (IF a THENRET) ==> (COND (a))
       (IF a THEN b c d e) ==> (COND (a b c d e))
       (IF a THEN b c  ELSE d) ==> (COND (a b c) (t d))
       (IF a THEN b c  ELSEIF d  THENRET  ELSE g)
               ==> (COND (a b c) (d) (t g))


 In the syntax description below,
    optional parts are surrounded by [ and ],
    + means one or more instances.
    | means 'or'
    <expr> is an lisp expression which isn't a keyword
       The keywords are:  THEN, THENRET, ELSE, ELSEIF.
    <pred> is also a lisp expression which isn't a keyword.
 
 <if-stmt> ::=  <simple-if-stmt>
              | <keyword-if-stmt>
 
 <simple-if-stmt> ::=  (IF <pred> <expr>)
                     | (IF <pred> <expr> <expr>)
 
 <keyword-if-stmt> ::= (IF <pred> <then-clause> [ <else-clause> ] )
 
 <then-clause> ::=  THEN <expr>+
                  | THENRET
 
 <else-clause> ::=  ELSE <expr>+
                  | ELSEIF <pred> <then-clause> [ <else-clause> ]


----------------------------------------------------------------------

DO WHILE/UNTIL Package (IFDO.L)
----------------------

Requires MATCH.L


(DO  WHILE  expr  <OPTION EXPR>  DO  <BODY>)
     UNTIL


RETURN or RET
	LAST = return the result of the last iteration of BODY
	LIST = return a list of all results from each iteration of BODY
	expr = return the result of expr after the last itteration, default NIL

RETURNV or RETV
	litatom = variable name used to store result of BODY (only needed if
		  RET is LAST or LIST) default=(GENSYM)

LVARS = List of local variables and optionall initial values.  RETV is
	automatically added to this list.  LVARS is in PROG variable list
	format.

AFTER expr
	expr = an expression which is executed after each iteration of BODY

DO exp1 exp2 exp3 ...
	The expressions to be evaluated at each iteration (the DO is optional)

BODY =
	expr = any expression
	label = any label (atom) other than LOOP, AFTER, END, or NIL
	(GO label) = branch to label
	(RETURN expr) = leave DO and return expr
	(GO LOOP) = restart the presetn iteration
	(GO AFTER) = end the present iteration
	(GO END) = end the DO


----------------------------------------------------------------------

DO FOR Package (IFDO.L)
--------------

Requires MATCH.L


(DO  FOR  <OPTIONAL EXPRS>  DO  <BODY>)

BEGINNING or BEG
	expr = indicates the beginning value of the DO variable, default=1

ENDING or END
	expr = indicating the ending value of the DO variable, default=1

INCREMENT or INC
	expr = indicates the value added to the DO variable after each
		iteration, default=1.  If DIR is DOWN value is subtracted.

COUNTV or VAR
	litatom = the DO variable name, default=(GENSYM).  This value is added
		to LVARS.

DIRECTION or DIR
	UP = DO variable is incremented (default)
	DOWN = Do variable is decremented

All DO WHILE options are also valid.

----------------------------------------------------------------------

Other DO forms
--------------

(DO expr1 expr2 expr3 ...)

same as (PROGN expr1 expr2 expr3 ...)

(DOWHILE  test-expr  return-type-or-expr  return-var local-vars  after-expr
	BODY)

return-type-or-expr = LAST, LIST, or an expression

return-var = variable to use for LAST or LIST

after-expr = executed after each expression


----------------------------------------------------------------------

MATCH package (MATCH.L)
-------------

(MATCH e1 e2)

returns T if match and NIL otherwise

e2 = the expression to matched

e1 = an expression consisting of the following:

	atom
	expr
	*
	**
	(? (from <to or T> <diminish match list> variable ALL/BUT)  L1)
	(?? (from to <diminish match list> variable  ALL/BUT/(NOT)  AND/OR)
		L2)

	L1 = match list, atom-var, or atom-list
	L2 = atom-var or predicate-list

(LMATCH M L F)

	M = match list
	L = a list of the items to be matched
	F = flag
	    NIL = stop trying match and return T after first match
	    T   = continue match to the end of the list

(Don't ask me what all this means.  I wrote it many years ago and have
 virtually no idea what my notes mean. B.M.)

----------------------------------------------------------------------

Structures package (STRUCT.L)
------------------

Requires MATCH.L

This package enables one to create nested, named data structures.

(SPUT A K V F)

	A = the litatom which will represent the data structure
	K = the key.
	    NIL  = A will be set to V.  Any other structure of A will be
		   overwritten
	    atom = the same as (atom)
	    list = each element of the list will specify the key of sucessive
		   levels of the data structure A
	V = the value.  Any lisp object which will be placed at position K
	F = flag
	    NIL = value V overwrites any previous value for key K
	    T   = value V is CONS'ed to any previous value at K

	The value returned is the entire structure A

(SMEMBER A K V)

	A = the litatom which will represent the data structure
	K = the key.
	    NIL  = Top level.
	    atom = the same as (atom)
	    list = each element of the list will specify the key of sucessive
		   levels of the data structure A
	V = matching value
	    NIL = return T if key K exists, NIL otherwise
	    other = return T if V matches or is an element of the value at K

(SNEXT  A)

	Returns the first key/value list and drops it from A

(SCHANGE A K S)

	A = the structure
	K = key
	S = the new name of the final level indicated by K

	Returns NIL if the change is unsuccessful.

(SGETNAMES A K)

	A = the structure
	K = key
	    NIL  = all keys
	    atom = same as (atom)
	    list = return names at level specified by K

(SREMOVE A K V)

	A = the structure
	K = same as above K
	V = value
	    NIL = remove entire level indicated by K
	    atom or list = remove the atom or list from the value at level K

(SGET A K)

	same arguments

(SCLAMP A K A2 K2 V F F2)

	Sets A at K and A2 at K2 to V and adds them depending on F and F2

(SATTACH A K A2 K2 F)

	Sets A at K to point to the value of A2 at K2.
	The value is added if F is T.
