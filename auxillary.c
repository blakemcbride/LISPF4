
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
int getch_(char *vec, char *ch, int *i)
{
    ch[0] = vec[*i-1];
    ch[1] = ' ';
    ch[2] = ' ';
    ch[3] = ' ';
    return 0;
}
int putch_(char *vec, char *ch, int *i)
{
    vec[*i-1] = *ch;
    return 0;
}
int upcase_(char *buff, int *n)
{
	int	i, x;
	for (x=i=0 ; i++ < *n ; x+=4)
		buff[x] = toupper(buff[x]);
	return 0;
}
static	FILE	*Logical_units[100];
void	setup()
{
	Logical_units[5] = stdin;
	Logical_units[6] = stdout;
}
int	f4_open(int lun, char *file, char *mode)
{
	if (Logical_units[lun])
		fclose(Logical_units[lun]);
	Logical_units[lun] = fopen(file, mode);
	return Logical_units[lun] ? 0 : 1;
}
int	f4_close(int lun)
{
	if (Logical_units[lun]) {
		fclose(Logical_units[lun]);
		Logical_units[lun] = NULL;
	}
	return 0;
}
static	int	read_status;  /*  1=do read,  2=at eol, 3=at eof  */
void	f4_start_read()
{
	read_status = 1;
}
static	int	read1(FILE *fp)
{
	int	c;
	if (read_status == 1) {
		c = getc(fp);
		if (c == '\r'  ||  c == '\n') {
			read_status = 2;
			c = ' ';
		} else if (c == EOF  &&  (ferror(fp)  ||  feof(fp))) {
			read_status = 3;
			c = ' ';
		}
	} else
		c = ' ';
	return c;
}
int	f4_read(int lun, int *ci, int n)
{
	FILE	*fp = Logical_units[lun];
	char	*v = (char *) ci;
	int	c;
	if (read_status == 3)
		return 1;
	v[0] = read1(fp);
	if (n == 1) {
		v[1] = ' ';
		v[2] = ' ';
		v[3] = ' ';
	} else {
		v[1] = read1(fp);
		v[2] = read1(fp);
		v[3] = read1(fp);
	}
	return 0;
}
int	f4_readu(int lun, int *ci, int n)
{
	FILE	*fp = Logical_units[lun];
	char	*v = (char *) ci;
	v[0] = getc(fp);
	if (n == 4) {
		v[1] = getc(fp);
		v[2] = getc(fp);
		v[3] = getc(fp);
	}
	return 0;
}
int	f4_rewind(int lun)
{
	FILE	*fp = Logical_units[lun];
	rewind(fp);
	return 0;
}
int	f4_write(int lun, int *ci, int n)
{
	FILE	*fp = Logical_units[lun];
	char	*v = (char *) ci;
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
	FILE	*fp = Logical_units[lun];
	putc('\n', fp);
	return 0;
}
