/*
 * yow.c
 *
 * Print a quotation from Zippy the Pinhead.
 * Qux <Kaufman-David@Yale> March 6, 1986
 *
 * With dynamic memory allocation.
 */

/* Synched up with: FSF 19.28. */

#define DONT_ENCAPSULATE
#include <config.h>

#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <../src/sxe-paths.h>	/* For PATH_DATA.  */

#if __STDC__ || defined(STDC_HEADERS)
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>		/* for time() */
#endif

#define BUFSIZE  80
#define SEP      '\0'

#ifndef YOW_FILE
#define YOW_FILE "yow.lines"
#endif

static void yow(FILE * fp);
static void setup_yow(FILE * fp);

#define xstrncpy(d_,s_,l_)			\
	do {					\
		char* dst_=d_;			\
		dst_[0]='\0';			\
		strncat((dst_),(s_),(l_)-1);	\
	} while(0)


int
main(int argc, char *argv[])
{
	FILE *fp;
	char file[BUFSIZ];

	if (argc > 2 && !strcmp(argv[1], "-f")) {
		assert(argv[2] != NULL);
		xstrncpy(file, argv[2], sizeof(file));
	} else {
#ifdef PATH_DATA
#ifdef vms
		int sz = snprintf(file, sizeof(file), "%s%s", PATH_DATA, YOW_FILE);
#else
		int sz = snprintf(file, sizeof(file), "%s/%s", PATH_DATA, YOW_FILE);
#endif
		assert(sz>=0 && sz<sizeof(file));
#else				/* !PATH_DATA */
		fprintf(stderr,
			"%s: the location of the \"%s\" file was not supplied at compile-time.\n\
You must supply it with the -f command-line option.\n",
			argv[0], YOW_FILE);
		exit(1);
#endif
	}

	if ((fp = fopen(file, "r")) == NULL) {
		perror(file);
		exit(1);
	}

	/* initialize random seed */
	srand((int)(getpid() + time((time_t *) 0)));

	setup_yow(fp);
	yow(fp);
	fclose(fp);
	return 0;
}

static long len = -1;
static long header_len;

#define AVG_LEN 40		/* average length of a quotation */

/* Sets len and header_len */
static void
setup_yow(FILE * fp)
{
	int c;

	/* Get length of file */
	/* Because the header (stuff before the first SEP) can be very long,
	 * thus biasing our search in favor of the first quotation in the file,
	 * we explicitly skip that. */
	while ((c = getc(fp)) != SEP) {
		if (c == EOF) {
			fprintf(stderr, "File contains no separators.\n");
			exit(2);
		}
	}
	header_len = ftell(fp);
	if (header_len > AVG_LEN)
		header_len -= AVG_LEN;	/* allow the first quotation to appear */

	if (fseek(fp, 0L, 2) == -1) {
		perror("fseek 1");
		exit(1);
	}
	len = ftell(fp) - header_len;
}

/* go to a random place in the file and print the quotation there */
static void
yow(FILE * fp)
{
	long offset;
	int c, i = 0;
	char *buf;
	int bufsize;

	offset = rand() % len + header_len;
	if (fseek(fp, offset, 0) == -1) {
		perror("fseek 2");
		exit(1);
	}

	/* Read until SEP, read next line, print it.
	   (Note that we will never print anything before the first separator.)
	   If we hit EOF looking for the first SEP, just recurse. */
	while ((c = getc(fp)) != SEP)
		if (c == EOF) {
			yow(fp);
			return;
		}

	/* Skip leading whitespace, then read in a quotation.
	   If we hit EOF before we find a non-whitespace char, recurse. */
	while (isspace(c = getc(fp))) ;
	if (c == EOF) {
		yow(fp);
		return;
	}

	bufsize = BUFSIZE;
	buf = (char *)malloc(bufsize);
	if (buf == (char *)0) {
		fprintf(stderr, "can't allocate any memory\n");
		exit(3);
	}

	buf[i++] = c;
	while ((c = getc(fp)) != SEP && c != EOF) {
		buf[i++] = c;

		if (i == bufsize - 1) {
			/* Yow! Is this quotation too long yet? */
			bufsize *= 2;
			buf = (char *)realloc(buf, bufsize);
			if (buf == (char *)0) {
				fprintf(stderr, "can't allocate more memory\n");
				exit(3);
			}
		}
	}
	buf[i++] = 0;
	printf("%s\n", buf);
	free(buf);
}
