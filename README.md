
LISPF4 - InterLisp Interpreter
=======================

LISPF4 is an InterLisp interpreter written by Mats Nordstrom from Uppsala, Sweden in the early 80's.  It was written in FORTRAN, and found by me, over the course of a few years of use, to be quite stable.  I subsequently used the F2C program to convert it to C.  I then gave it the ability to have command-line determination of the memory size.  I have much less experience with the C port in terms of reliability.

The original FORTRAN code required a small assembler piece to provide byte indexing into integers.  This small piece caused no end of trouble when porting to a new compiler.  Since C can do this without assembler, this problem has hopefully been eliminated.

The system supports much of the InterLisp standard, and a few bits of the standard were added by me.  There is no GUI.  Sometime in the late 90's Mats was kind enough to give me a copy of the latest version he had (8/22/83).  He also gave me permission to release it so long as I retained his credits.  Among other things, this distribution includes the entire, untouched, original source code and documentation.

The current status of the C conversion eliminates the need for a FORTRAN compiler or F2C.  It is all C code now.  I've had it working on Windows, Mac, and Linux.

Interlisp is a dynamically scoped lisp system.  It supports LAMBDA (evaluates function arguments), NLAMBDA (doesn't evaluate its arguments), and variable number of arguments.  Macros are supported as well.

The system comes with a pretty complete user manual and implementors manual.  I also include the documentation this system is based on.

Pre-built systems for Linux, Mac, and Windows are also included.

See the Documentation/README.txt file for more complete documentation.

The master copy of this system is located at:  https://github.com/blakemcbride/LISPF4

Enjoy.

Blake McBride
