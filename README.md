
LISPF4 - InterLisp Interpreter
=======================

**NOTE: The original LISPF4, written in FORTRAN IV (included herein as lispf4.orig), always worked flawlessly.  I converted it to 32-bit C via the FORTRAN to C converter.  I think it worked, but didn't use it a lot.
I later made a few adjustments for 64 bits.  At first, I thought it worked, but I now realize it does not.  This is going to take some effort to debug.  I just do not have the time right now.  If someone were to,
I might suggest starting back with the original known-working FORTRAN source.**

LISPF4 is an InterLisp interpreter written by Mats Nordstrom from Uppsala, Sweden, in the early '80s.  It was written in FORTRAN and found by me, over the course of a few years of use, to be quite stable.  I subsequently used the F2C program to convert it to C.  I then gave it the ability to have command-line determination of the memory size.  I have much less experience with the C port in terms of reliability.

The original FORTRAN code required a small assembler piece to provide byte indexing into integers.  This small piece caused no end of trouble when porting to a new compiler.  Since C can do this without assembler, this problem has hopefully been eliminated.

The system supports much of the InterLisp standard, and a few bits of the standard were added by me.  There is no GUI.  Sometime in the late '90s Mats was kind enough to give me a copy of the latest version he had (8/22/83).  He also gave me permission to release it so long as I retained his credits.  Among other things, this distribution includes the entire, untouched, original source code and documentation.

The current status of the C conversion eliminates the need for a FORTRAN compiler or F2C.  It is all C code now.  I've had it working on Windows, Mac, and Linux.

Interlisp is a dynamically scoped Lisp system.  It supports LAMBDA (evaluates function arguments), NLAMBDA (doesn't evaluate its arguments), and a variable number of arguments.  Macros are supported as well.

The system comes with a pretty complete user manual and implementor's manual.  I also include the documentation this system is based on.

Pre-built systems for Linux, Mac, and Windows are also included.

See the [Documentation/README.txt](https://github.com/blakemcbride/LISPF4/blob/master/Documentation/README.txt) file for more complete documentation.

Source code located at:  [https://github.com/blakemcbride/LISPF4](https://github.com/blakemcbride/LISPF4)

Enjoy.

Blake McBride
