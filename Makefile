# Makefile for GNU Make on Linux (32 or 64 bit)

# compile for 32 bit machine (works on 64 bit machines too)
#M32 = -m32

LAST_UPDATE_YEAR = 2026
LAST_UPDATE_MONTH = 8
LAST_UPDATE_DAY = 4

# Set defaults
PARMS = -DCELLS=100000 -DATOMS=3000 -DSTACK=1500 -DARRAY=5000

OPT = -O3
#OPT = -g -Og

# The F2C output emulates FORTRAN EQUIVALENCE by casting between pointer types
# -- e.g. "#define n ((integer *)equiv_2)" over a "real" array, and the
# ((integer *)&b_1.arg) aliases of the /B/ common block.  That is precisely the
# access pattern type-based alias analysis assumes cannot happen, and -O2/-O3
# turn it on.  Correctness first: switch it off.  (gcc reports ~44 such sites.)
ALIAS = -fno-strict-aliasing

CFLAGS = -Dstricmp=strcasecmp $(OPT) $(ALIAS) $(M32) $(PARMS) -DYEAR=$(LAST_UPDATE_YEAR) -DMONTH=$(LAST_UPDATE_MONTH) -DDAY=$(LAST_UPDATE_DAY)

.f.c:
	f2c -onetrip -A -h $<


basic.img : bare.img script.2
	./lispf4 bare.img <script.2

bare.img : lispf4 SYSATOMS script.1
	./lispf4 -x <script.1


lispf4 : lispf41.o lispf42.o auxillary.o
	gcc -o $@ $(M32) $(OPT) $^ -lm

lispf41.o lispf42.o auxillary.o : f2c.h lispf4.h

#lispf41.c : lispf41.f
#	f2c -onetrip -A -h $<

#lispf42.c : lispf42.f
#	f2c -onetrip -A -h -E $<


# Regression suite.  See tests/README.md.
test : basic.img
	./tests/run-tests.sh

# Debug build: no optimisation, full symbols, sanitizers where available.
# If libasan/libubsan are not installed the link will fail -- either install
# them (dnf install libasan libubsan) or drop -fsanitize=... below and use gdb.
# Several Bugs1.md findings (notably B7) only show up under the sanitizers.
DBGFLAGS = -g -O0 -fsanitize=address,undefined -fno-omit-frame-pointer

lispf4dbg : lispf41.c lispf42.c auxillary.c f2c.h lispf4.h
	gcc -o $@ $(DBGFLAGS) $(ALIAS) -Dstricmp=strcasecmp $(M32) $(PARMS) \
	    -DYEAR=$(LAST_UPDATE_YEAR) -DMONTH=$(LAST_UPDATE_MONTH) -DDAY=$(LAST_UPDATE_DAY) \
	    lispf41.c lispf42.c auxillary.c -lm

debug : lispf4dbg

# Run the suite against the debug build:  make testdebug
testdebug : lispf4dbg basic.img
	LISPF4=./lispf4dbg ./tests/run-tests.sh

clean:
	rm -f *~ *.o core *.bak
	rm -rf tests/.work

realclean: clean
	rm -f lispf4 lispf4dbg *.img
