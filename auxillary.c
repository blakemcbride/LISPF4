
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include "lispf4.h"


/*  A character lives in the low 8 bits of an integer, blank padded above.
    Building it arithmetically rather than by storing into byte 0 keeps this
    independent of byte order: GETCHT recovers the character with (IC mod
    256), which only agreed with a byte-0 store on a little-endian host.  */

#define	F4_PAD	((integer) ' ')

int getch_(void *vec, integer *ch, integer *i)
{
    const unsigned char *v = (const unsigned char *) vec;

    *ch = (integer) v[*i - 1]
	| (F4_PAD << 8) | (F4_PAD << 16) | (F4_PAD << 24);
    return 0;
}

int putch_(void *vec, integer *ch, integer *i)
{
    ((unsigned char *) vec)[*i - 1] = (unsigned char) (*ch & 0xFF);
    return 0;
}

int upcase_(integer *buff, integer *n)
{
	integer	i;

	for (i = 0 ; i < *n ; i++)
		buff[i] = (buff[i] & ~(integer) 0xFF)
			| (integer) toupper((unsigned char) (buff[i] & 0xFF));
	return 0;
}

static	FILE	*Logical_units[100];

#define	F4_MAXLUN	((int)(sizeof Logical_units / sizeof Logical_units[0]))

/*  Stream for a logical unit, or NULL if the unit number is out of range or
    the unit is not open.  Every entry point below goes through this, so a
    bad unit number can neither index outside the array nor be dereferenced.  */

static	FILE	*f4_fp(int lun)
{
	if (lun < 0  ||  lun >= F4_MAXLUN)
		return NULL;
	return Logical_units[lun];
}

int	f4_isopen(int lun)
{
	return f4_fp(lun) != NULL;
}

void	setup()
{
	Logical_units[5] = stdin;
	Logical_units[6] = stdout;
}

/*  Open the new stream BEFORE closing the old one.  Closing first meant a
    failed reopen silently closed the file the unit already had, leaving the
    caller with a unit that reports "not open" and no way back.  */

int	f4_open(int lun, char *file, char *mode)
{
	FILE	*fp;

	if (lun < 0  ||  lun >= F4_MAXLUN)
		return 1;
	if ((fp = fopen(file, mode)) == NULL)
		return 1;
	if (Logical_units[lun])
		fclose(Logical_units[lun]);
	Logical_units[lun] = fp;
	return 0;
}

int	f4_close(int lun)
{
	if (lun < 0  ||  lun >= F4_MAXLUN)
		return 1;
	if (Logical_units[lun]) {
		fclose(Logical_units[lun]);
		Logical_units[lun] = NULL;
	}
	return 0;
}

/*  End-of-line / end-of-file state, per logical unit.  A single global was
    shared by every unit, so an EOF on one file affected reads from another.  */

static	int	read_status[F4_MAXLUN];  /*  1=do read,  2=at eol, 3=at eof  */

void	f4_start_read(int lun)
{
	if (lun >= 0  &&  lun < F4_MAXLUN)
		read_status[lun] = 1;
}

/*  Has the physical line just read really ended?  RDA1 fills its card to the
    right margin whatever the input looks like, so a short line is blank
    padded and the surplus of a long one is left in the stream -- and SHIFT
    could not tell the two apart.  Non-zero here means the newline (or the
    end of the file) has been passed and everything RDA1 stores from now on
    is padding; zero means the card filled up first and the line goes on.  */

int	f4_at_line_end(int lun)
{
	if (lun < 0  ||  lun >= F4_MAXLUN)
		return 1;
	return read_status[lun] != 1;
}

/*  Next character of the current line, or EOF if end-of-file was reached
    without one.  Returning EOF rather than a blank is what lets f4_read
    distinguish "nothing left to read" from "a line with no terminating
    newline", which RDA1 needs in order not to discard that last line.  */

static	int	read1(FILE *fp, int lun)
{
	int	c;
	if (read_status[lun] == 1) {
		c = getc(fp);
		if (c == '\r') {
			/*  A CRLF file is one line per CRLF, not one real
			    line and one blank.  Treating the CR as the
			    terminator and leaving the LF for the next
			    f4_start_read made every second line read as
			    blank: IREAD tolerates that, but MESS's RDA4
			    reads exactly MAXMES lines, so a CRLF SYSATOMS
			    silently built an image with a shifted message
			    table.  */
			int n = getc(fp);
			if (n != '\n'  &&  n != EOF)
				ungetc(n, fp);
			read_status[lun] = 2;
			c = ' ';
		} else if (c == '\n') {
			read_status[lun] = 2;
			c = ' ';
		} else if (c == EOF  &&  (ferror(fp)  ||  feof(fp))) {
			read_status[lun] = 3;
			return EOF;
		} else if (c == '\t'  ||  c == '\f'  ||  c == '\v')
			c = ' ';
	} else
		c = ' ';
	return c;
}

/*  A1 read: one character into the low 8 bits of an integer, blank padded
    above.  Built arithmetically so the character always lands where
    getcht_ looks for it (ic % 256), whatever the host's byte order.
    Returns non-zero only when no character was stored.  */

int	f4_read_char(int lun, integer *ch)
{
	FILE	*fp = f4_fp(lun);
	int	c;

	if (!fp)
		return 1;
	if (read_status[lun] == 3)
		return 1;
	c = read1(fp, lun);
	if (c == EOF)
		return 1;
	*ch = (integer) (unsigned char) c
	    | (F4_PAD << 8) | (F4_PAD << 16) | (F4_PAD << 24);
	return 0;
}

/*  A1 write: emit the character held in the low 8 bits.  */

int	f4_write_char(int lun, const integer *ch)
{
	FILE	*fp = f4_fp(lun);

	if (!fp)
		return 1;
	putc((int) (*ch & 0xFF), fp);
	return 0;
}

/*  A4 read: four characters packed into one word, byte for byte.  RDA4 and
    WRA4 are the only users and share this representation, so it round-trips.
    NOT for single characters -- use f4_read_char, which is byte-order neutral.
    Returns non-zero only when no character was stored.  */

int	f4_read(int lun, char *v, int n)
{
	FILE	*fp = f4_fp(lun);
	int	c;

	if (!fp)
		return 1;
	if (read_status[lun] == 3)
		return 1;
	c = read1(fp, lun);
	if (c == EOF)
		return 1;
	v[0] = c;
	if (n == 1) {
		v[1] = ' ';
		v[2] = ' ';
		v[3] = ' ';
	} else {
		c = read1(fp, lun);	v[1] = (c == EOF) ? ' ' : c;
		c = read1(fp, lun);	v[2] = (c == EOF) ? ' ' : c;
		c = read1(fp, lun);	v[3] = (c == EOF) ? ' ' : c;
	}
	return 0;
}

/*  Unformatted (binary) read.  Returns non-zero on a short read so that a
    truncated or corrupt image file is detected instead of being loaded as
    a run of 0xFF bytes.  */

int	f4_readu(int lun, char *v, int n)
{
	FILE	*fp = f4_fp(lun);
	size_t	want = (n == 4) ? 4 : 1;

	if (!fp)
		return 1;
	if (fread(v, 1, want, fp) != want)
		return 1;
	return 0;
}

int	f4_rewind(int lun)
{
	FILE	*fp = f4_fp(lun);
	if (!fp)
		return 1;
	rewind(fp);
	return 0;
}

int	f4_write(int lun, char *v, int n)
{
	FILE	*fp = f4_fp(lun);
	if (!fp)
		return 1;
	putc(v[0], fp);
	if (n == 4) {
		putc(v[1], fp);
		putc(v[2], fp);
		putc(v[3], fp);
	}
	return 0;
}

int	f4_write_lf(int lun)
{
	FILE	*fp = f4_fp(lun);
	if (!fp)
		return 1;
	putc('\n', fp);
	return 0;
}

time_t start_time;

integer mslft_(integer *i__)
{
    /* System generated locals */
    integer ret_val;
    ret_val = (integer) (time(NULL) - start_time);
    return ret_val;
} /* mslft_ */

/* Subroutine */ int mtime_(integer *it)
{
	char *p = (char *)it;
	time_t sec = time(NULL);
	struct tm *t = localtime(&sec);
	sprintf(p, "%02d:%02d",
		t->tm_hour,
		t->tm_min);
	return 0;
} /* mtime_ */

/* Subroutine */ int mdate_(integer *it)
{
	static char mname[12][4] = {
		"Jan",
		"Feb",
		"Mar",
		"Apr",
		"May",
		"Jun",
		"Jul",
		"Aug",
		"Sep",
		"Oct",
		"Nov",
		"Dec"
	};
	char *p = (char *)it;
	time_t sec = time(NULL);
	struct tm *t = localtime(&sec);
	sprintf(p, "%02d-%s-%4d %02d:%02d:%02d",
		t->tm_mday,
		mname[t->tm_mon],
		t->tm_year+1900,
		t->tm_hour,
		t->tm_min,
		t->tm_sec);

	return 0;
} /* mdate_ */

