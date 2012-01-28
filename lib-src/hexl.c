/* Synched up with: FSF 19.28. */

#include <config.h>

#include <stdio.h>
#include <ctype.h>

#if __STDC__ || defined(STDC_HEADERS)
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#endif

#define DEFAULT_GROUPING	0x01
#define DEFAULT_BASE		16

#undef TRUE
#undef FALSE
#define TRUE  (1)
#define FALSE (0)

int base = DEFAULT_BASE, un_flag = FALSE, iso_flag = FALSE, endian = 1;
int group_by = DEFAULT_GROUPING;
char *progname;

void usage(void);

int main(int argc, char *argv[])
{
	register long address;
	char string[18];
	FILE *fp;

	progname = *argv++;
	--argc;

	/*
	 ** -hex              hex dump
	 ** -oct              Octal dump
	 ** -group-by-8-bits
	 ** -group-by-16-bits
	 ** -group-by-32-bits
	 ** -group-by-64-bits
	 ** -iso              iso character set.
	 ** -big-endian       Big Endian
	 ** -little-endian    Little Endian
	 ** -un || -de        from hexl format to binary.
	 ** --                End switch list.
	 ** <filename>        dump filename
	 ** -         (as filename == stdin)
	 */

	while (*argv && *argv[0] == '-' && (*argv)[1]) {
		/* A switch! */
		if (!strcmp(*argv, "--")) {
			--argc;
			argv++;
			break;
		} else if (!strcmp(*argv, "-un") || !strcmp(*argv, "-de")) {
			un_flag = TRUE;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-hex")) {
			base = 16;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-iso")) {
			iso_flag = TRUE;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-oct")) {
			base = 8;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-big-endian")) {
			endian = 1;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-little-endian")) {
			endian = 0;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-group-by-8-bits")) {
			group_by = 0x00;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-group-by-16-bits")) {
			group_by = 0x01;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-group-by-32-bits")) {
			group_by = 0x03;
			--argc;
			argv++;
		} else if (!strcmp(*argv, "-group-by-64-bits")) {
			group_by = 0x07;
			endian = 0;
			--argc;
			argv++;
		} else {
			(void)fprintf(stderr, "%s: invalid switch: \"%s\".\n",
				      progname, *argv);
			usage();
		}
	}

	do {
		if (*argv == NULL)
			fp = stdin;
		else {
			char *filename = *argv++;

			if (!strcmp(filename, "-"))
				fp = stdin;
			else if ((fp = fopen(filename, "r")) == NULL) {
				perror(filename);
				continue;
			}
		}

		if (un_flag) {
			char buf[18];

			for (;;) {
				register int i, c = 0, d;

#define hexchar(x) (isdigit (x) ? x - '0' : x - 'a' + 10)

				if (fread(buf, 1, 10, fp) != 10)
					if (feof(fp))
						break;	/* skip 10 bytes */

				for (i = 0; i < 16; ++i) {
					if ((c = getc(fp)) == ' ' || c == EOF)
						break;

					d = getc(fp);
					c = hexchar(c) * 0x10 + hexchar(d);
					putchar(c);

					if ((i & group_by) == group_by)
						(void)getc(fp);
				}

				if (c == ' ') {
					while ((c = getc(fp)) != '\n'
					       && c != EOF) ;

					if (c == EOF)
						break;
				} else {
					if (i < 16)
						break;

					if (fread(buf, 1, 18, fp)!=18) /* skip 18 bytes */
						if(feof(fp))
							break;
				}
			}
		} else {
			address = 0;
			string[0] = ' ';
			string[17] = '\0';
			for (;;) {
				register int i, c = 0;

				for (i = 0; i < 16; ++i) {
					if ((c = getc(fp)) == EOF) {
						if (!i)
							break;

						fputs("  ", stdout);
						string[i + 1] = '\0';
					} else {
						if (!i)
							(void)printf("%08lx: ",
								     address);

						if (iso_flag)
							string[i + 1] =
							    (c < 0x20
							     || (c >= 0x7F
								 && c <
								 0xa0)) ? '.' :
							    c;
						else
							string[i + 1] =
							    (c < 0x20
							     || c >=
							     0x7F) ? '.' : c;

						(void)printf("%02x", c);
					}

					if ((i & group_by) == group_by)
						putchar(' ');
				}

				if (i)
					puts(string);

				if (c == EOF)
					break;

				address += 0x10;

			}
		}

		if (fp != stdin)
			(void)fclose(fp);

	} while (*argv != NULL);
	return 0;
}

void usage(void)
{
	fprintf(stderr, "Usage: %s [-de] [-iso]\n", progname);
	exit(1);
}
