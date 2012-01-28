/* Generate doc-string file for SXEmacs from source files.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1998, 1999 J. Kean Johnston.
   Copyright (C) 2004 Steve Youngs.

This file is part of SXEmacs.

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: FSF 19.30. */

/* The arguments given to this program are all the C and Lisp source files
 of SXEmacs.  .elc and .el and .c files are allowed.
 A .o file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.

 The results, which go to standard output or to a file
 specified with -a or -o (-a to append, -o to start from nothing),
 are entries containing function or variable names and their documentation.
 Each entry starts with a ^_ character.
 Then comes F for a function or V for a variable.
 Then comes the function or variable name, terminated with a newline.
 Then comes the documentation for that function or variable.

 Added 19.15/20.1:  `-i site-packages' allow installer to dump extra packages
 without modifying Makefiles, etc.

 Big cleanup 2012-01-08  Sebastian Freundt
 */

#define NO_SHORTNAMES		/* Tell config not to load remap.h */
#include <config.h>

#include <stdio.h>
#include <errno.h>
#if __STDC__ || defined(STDC_HEADERS)
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <ctype.h>
#endif

#include <sys/param.h>

/* How long can a source filename be in DOC (including "\037S" at the start
    and "\n" at the end) ? */
#define DOC_MAX_FILENAME_LENGTH 2048
#define IS_DIRECTORY_SEP(arg) ('/' == arg)

/* Can't use the system assert on OS X, it can't find a definition for
   __eprintf on linking */
#define assert(x) ((x) ? (void) 0 : (void) abort ())

#define READ_TEXT "r"
#define READ_BINARY "r"
#define WRITE_BINARY "w"
#define APPEND_BINARY "a"

/* Stdio stream for output to the DOC file.  */
static FILE *outfile;
static char *modname = NULL;

enum {
	el_file,
	elc_file,
	c_file
} Current_file_type;

static void put_filename (const char *filename);
static int scan_file(const char *filename);
static int read_c_string(FILE *, int, int);
static void
write_c_args(FILE * out, const char *func, char *buf, int minargs, int maxargs);
static int scan_c_file(const char *filename, const char *mode);
static void skip_white(FILE *);
static void read_lisp_symbol(FILE *, char *);
static int scan_lisp_file(const char *filename, const char *mode);

#define C_IDENTIFIER_CHAR_P(c)			\
	(('A' <= c && c <= 'Z') ||		\
	 ('a' <= c && c <= 'z') ||		\
	 ('0' <= c && c <= '9') ||		\
	 (c == '_'))

/* Name this program was invoked with.  */
static char *progname;

/* Set to 1 if this was invoked by ellcc */
static int ellcc = 0;

/**
 * Print error message.  `s1' is printf control string, `s2' is arg for it. */
static void
error(const char *s1, const char *s2)
{
	fprintf(stderr, "%s: ", progname);
	fprintf(stderr, s1, s2);
	fprintf(stderr, "\n");
	return;
}

/**
 * Print error message and exit.  */
static void
__attribute__((noreturn))
fatal(const char *s1, const char *s2)
{
	error(s1, s2);
	exit(1);
}

/**
 * Like malloc but get fatal error if memory is exhausted.  */
static void*
xmalloc(unsigned int size)
{
	void *result = malloc(size);
	if (result == NULL) {
		fatal("virtual memory exhausted", 0);
	}
	return result;
}

static char*
next_extra_elc(char *extra_elcs)
{
	static FILE *fp = NULL;
	static char line_buf[BUFSIZ];
	char *p = line_buf + 1;

	if (!fp) {
		if (!extra_elcs) {
			return NULL;
		} else if (!(fp = fopen(extra_elcs, READ_BINARY))) {
			/* It is not an error if this file doesn't exist. */
			return NULL;
		}
		if(!fgets(line_buf, BUFSIZ, fp)) {
			fclose(fp);
			fp = NULL;
			return NULL;
		}
	}

again:
	if (!fgets(line_buf, BUFSIZ, fp)) {
		fclose(fp);
		fp = NULL;
		return NULL;
	}
	line_buf[0] = '\0';
	if (strlen(p) <= 2 || strlen(p) >= (BUFSIZ - 5)) {
		/* reject too short or too long lines */
		goto again;
	}
	p[strlen(p) - 2] = '\0';
	strcat(p, ".elc");

	return p;
}

static void
write_doc_header(void)
{
	char *tmp, *modout = strdup(modname), *modoutC;
	size_t modsz;

	if ((tmp = strrchr(modout, '.')) != NULL) {
		*tmp = '\0';
		tmp = strrchr(modout, '.');
		if (tmp != NULL) {
			*tmp = '\0';
		}
	}
	/* the same for modoutC */
	modoutC = strdup(modout);
	modsz = strlen(modoutC);

	for (size_t i = 0; i < modsz; i++) {
		/* for the C version we have to convert any non-char to _ */
		if (!isdigit(modoutC[i]) && !isalpha(modoutC[i])) {
			modoutC[i] = '_';
		}
	}

	fprintf(outfile, "/* DO NOT EDIT - AUTOMATICALLY GENERATED */\n\n");
	fprintf(outfile, "#include <emodules-ng.h>\n\n");

	/* declare and start the LTX_docs() block */
	fprintf(outfile, "\n\nextern void %s_LTX_docs(void);\n", modoutC);
	fprintf(outfile, "\nvoid\n%s_LTX_docs(void)\n", modoutC);
	return;
}


int
main(int argc, char **argv)
{
	int i;
	int err_count = 0;
	char *extra_elcs = NULL;

	progname = argv[0];

	outfile = stdout;

	/* If first two args are -o FILE, output to FILE.  */
	for (i = 1; i < argc - 1;) {
		if (!strcmp(argv[i], "-o")) {
			outfile = fopen(argv[++i], WRITE_BINARY);
		}
		if (!strcmp(argv[i], "-a")) {
			outfile = fopen(argv[++i], APPEND_BINARY);
		}
		if (!strcmp(argv[i], "-E")) {
			if (modname == NULL) {
				modname = strdup(argv[i+1]);
			}
			ellcc = 1;
			outfile = fopen(argv[++i], WRITE_BINARY);
		}
		if (!strcmp(argv[i], "-d")) {
			if (chdir(argv[++i]) < 0) {
				fatal("Could not change to directory ",argv[i]);
			}
		}

		if (!strcmp(argv[i], "-i")) {
			extra_elcs = argv[++i];
		}

		if (!strcmp(argv[i], "--modname") || !strcmp(argv[i], "-m")) {
			modname = strdup(argv[++i]);
		}
		i++;
	}
	if (outfile == 0) {
		fatal("No output file specified", "");
	}
	if (ellcc) {
		write_doc_header();
		fprintf(outfile, "{\n");
	}

	for (i = 1; i < argc; i++) {
		int j;

		if (argc > i + 1 && !strcmp (argv[i], "-d")) {
			/* XEmacs change; allow more than one chdir. The
			   idea is that the second chdir is to source-lisp,
			   and that any Lisp files not under there have the
			   full path specified.  */
			i += 1;
			if (chdir (argv[i]) < 0) {
				fatal("Could not change to directory ", argv[i]);
			}
			continue;
		} else if (argv[i][0] == '-') {
			i++;
			continue;
		}
		/* Don't process one file twice.  */
		for (j = 1; j < i; j++) {
			if (!strcmp(argv[i], argv[j])) {
				break;
			}
		}
		if (j == i) {
			/* err_count seems to be {mis,un}used */
			err_count += scan_file(argv[i]);
		}
	}

	if (extra_elcs) {
		char *p;

		while ((p = next_extra_elc(extra_elcs)) != NULL) {
			err_count += scan_file(p);
		}
	}

	putc('\n', outfile);
	if (ellcc) {
		fprintf(outfile, "}\n\n");
	}
#ifndef VMS
	exit(err_count > 0);
#endif  /* VMS */
	return err_count > 0;
}

/* Add a source file name boundary in the output file.  */
static void
put_filename (const char *filename)
{
/* XEmacs change; don't strip directory information. */
	/* <= because sizeof includes the nul byte at the end. Not quite
	   right, because it should include the length of the symbol +
	   "\037[VF]" instead of simply 10. */
	assert(sizeof("\037S\n") + strlen(filename) + 10
	       <= DOC_MAX_FILENAME_LENGTH);

	putc (037, outfile);
	putc ('S', outfile);
	fprintf (outfile, "%s\n", filename);
	return;
}

/**
 * Read file FILENAME and output its doc strings to outfile.
 * Return 1 if file is not found, 0 if it is found. **/
static int
scan_file(const char *filename)
{
	int len = strlen(filename);

	if (ellcc == 0 && len > 4 && !strcmp(filename + len - 4, ".elc")) {
		Current_file_type = elc_file;
		return scan_lisp_file(filename, READ_BINARY);
	} else if (ellcc == 0 && len > 3 &&
		   strcmp(filename + len - 3, ".el") == 0) {
		Current_file_type = el_file;
		return scan_lisp_file(filename, READ_TEXT);
	} else {
		Current_file_type = c_file;
		return scan_c_file(filename, READ_TEXT);
	}
	/* not reached */
}

char buf[128];

/**
 * Print a simple return in accordance with printflag and ellcc state*/
static void
pr_char(int printflag, char **p, register int c)
{
	if (printflag > 0) {
		if (ellcc) {
			switch (c) {
			case '\n':
				putc('\\', outfile);
				putc('n', outfile);
			case '"':
				putc('\\', outfile);
			default:
				break;
			}
		}
		putc(c, outfile);
	} else if (printflag < 0) {
		char *tmp = *p;
		*tmp++ = c;
		*p = tmp;
	}
	return;
}

#define MDGET						\
	do {						\
		prevc = c;				\
		c = getc(infile);			\
	} while (0)

/**
 * Skip a C string from INFILE,
 * and return the character that follows the closing ".
 * If printflag is positive, output string contents to outfile.
 * If it is negative, store contents in buf.
 * Convert escape sequences \n and \t to newline and tab;
 * discard \ followed by newline. **/
static int
read_c_string(FILE *infile, int printflag, int c_docstring)
{
	register int prevc = 0;
	register int c = 0;
	char *p = buf;
	int start = -1;

	MDGET;
	while (c != EOF) {
		while ((c_docstring || c != '"') && c != EOF) {
			if (c == '*') {
				int cc = getc(infile);

				if (cc == '/' && prevc != '\n') {
					pr_char(printflag, &p, '\n');
					break;
				} else if (cc == '/') {
					break;
				} else {
					ungetc(cc, infile);
				}
			}

			if (start == 1) {
				pr_char(printflag, &p, '\n');
			}

			if (c == '\\') {
				MDGET;
				if (c == '\n') {
					MDGET;
					start = 1;
					continue;
				}
				if (!c_docstring && c == 'n') {
					c = '\n';
				}
				if (c == 't') {
					c = '\t';
				}
			}
			if (c == '\n') {
				start = 1;
			} else {
				start = 0;
				pr_char(printflag, &p, c);
			}
			MDGET;
		}
		/* look for continuation of string */
		if (Current_file_type == c_file) {
			do {
				MDGET;
			}
			while (isspace(c));
			if (c != '"') {
				break;
			}
		} else {
			MDGET;
			if (c != '"') {
				break;
			}
			/* If we had a "", concatenate the two strings. */
		}
		MDGET;
	}

	if (printflag < 0) {
		*p = 0;
	}
	return c;
}

/**
 * Write to file OUT the argument names of function FUNC, whose text is in BUF.
 * MINARGS and MAXARGS are the minimum and maximum number of arguments. **/

static void
write_c_args(FILE *out, const char *func, char *buff, int minargs, int maxargs)
{
	register char *p;
	int in_ident = 0;
	int just_spaced = 0;
	/* XEmacs - "arguments:" is for parsing the docstring.  FSF's help system
	   doesn't parse the docstring for arguments like we do, so we're also
	   going to omit the function name to preserve compatibility with elisp
	   that parses the docstring.  Finally, not prefixing the arglist with
	   anything is asking for trouble because it's not uncommon to have an
	   unescaped parenthesis at the beginning of a line. --Stig */
	fprintf(out, "arguments: (");

	if (*buff == '(') {
		++buff;
	}

	for (p = buff; *p; p++) {
		char c = *p;

		/* Add support for ANSI prototypes. Hop over
		   "Lisp_Object" string (the only C type allowed in DEFUNs) */
		static char lo[] = "Lisp_Object";
		if ((C_IDENTIFIER_CHAR_P(c) != in_ident) && !in_ident &&
		    (strncmp(p, lo, sizeof(lo) - 1) == 0) &&
		    isspace((unsigned char)(*(p + sizeof(lo) - 1)))) {
			p += (sizeof(lo) - 1);
			while (isspace((unsigned char)(*p))) {
				p++;
			}
			c = *p;
		}

		/* Notice when we start printing a new identifier.  */
		if (C_IDENTIFIER_CHAR_P(c) != in_ident) {
			if (!in_ident) {
				in_ident = 1;
				if (minargs == 0 && maxargs > 0) {
					fprintf(out, "&optional ");
				}
				just_spaced = 1;

				minargs--;
				maxargs--;
			} else {
				in_ident = 0;
			}
		}

		/* Print the C argument list as it would appear in lisp:
		   print underscores as hyphens, and print commas as spaces.
		   Collapse adjacent spaces into one. */
		if (c == '_') {
			c = '-';
		}
		if (c == ',') {
			c = ' ';
		}

		/* If the C argument name ends with `_', change it to ' ', to
		   allow use of C reserved words or global symbols as Lisp
		   args. */
		if (c == '-' && !C_IDENTIFIER_CHAR_P(p[1])) {
			in_ident = 0;
			just_spaced = 0;
		}
		/* If the character is carriage return, escape it for the C
		   compiler. */
		else if (c == '\n') {
			putc('\\', out);
			putc('\n', out);
		} else if (c != ' ' || !just_spaced) {
			if (c >= 'a' && c <= 'z') {
				/* Upcase the letter.  */
				c += 'A' - 'a';
			}
			putc(c, out);
		}

		just_spaced = (c == ' ');
	}
	if (!ellcc) {
		/* XEmacs addition */
		putc('\n', out);
	}
	return;
}

static int
check_comma(FILE *infile, register int commas, int *minargs, int *maxargs)
{
	register int c;

	do {
		c = getc(infile);
	} while (c == ' ' || c == '\n' || c == '\t');
	if (c < 0) {
		return c;
	}
	ungetc(c, infile);
	if (commas == 2) {
		/* pick up minargs */
		if (fscanf(infile, "%d", minargs) != 1) {
			fprintf(stderr, "Failed to read minargs\n");
		}
	} else if (c == 'M' || c == 'U') {
		/* MANY || UNEVALLED */
		*maxargs = -1;
	} else {
		/* pick up maxargs */
		if (fscanf(infile, "%d", maxargs) != 1) {
			fprintf(stderr, "Failed to read maxargs\n");
		}
	}
	return c;
}

/**
 * Read through a c file.  If a .o file is named, the corresponding .c file is
 * read instead.
 * Looks for DEFUN constructs such as are defined in ../src/lisp.h.
 * Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED.  */
static int
scan_c_file(const char *filename, const char *mode)
{
	FILE *infile;
	register int c;
	register int commas;
	register int defunflag;
	register int defvarperbufferflag = 0;
	register int defvarflag;
	int minargs;
	int maxargs;
	size_t l = strlen(filename);
	char f[MAXPATHLEN];

	if (l > sizeof(f)) {
#ifdef ENAMETOOLONG
		errno = ENAMETOOLONG;
#else
		errno = EINVAL;
#endif
		return (0);
	}

	strcpy(f, filename);
	if (f[l - 1] == 'o') {
		f[l - 1] = 'c';
	}
	infile = fopen(f, mode);

	/* No error if non-ex input file */
	if (infile == NULL) {
		perror(f);
		return 0;
	}

	c = '\n';
	while (!feof(infile)) {
		if (c != '\n') {
			c = getc(infile);
			continue;
		}
		c = getc(infile);
		/*
		 * SXEmacs uses proper indentation so we need to
		 * search for `\t' instead of ' ' here.
		 */
		if (c == '\t') {
			while (c == '\t') {
				c = getc(infile);
			}
			if ((c == 'D') &&
			    (c = getc(infile)) == 'E' &&
			    (c = getc(infile)) == 'F' &&
			    (c = getc(infile)) == 'V' &&
			    (c = getc(infile)) == 'A' &&
			    (c = getc(infile)) == 'R' &&
			    (c = getc(infile)) == '_') {
				defvarflag = 1;
				defunflag = 0;

				c = getc(infile);
				/* Note that this business doesn't apply under
				   XEmacs.  DEFVAR_BUFFER_LOCAL in XEmacs
				   behaves normally. */
				defvarperbufferflag = (c == 'P');

				c = getc(infile);
			} else {
				continue;
			}
		} else if (c == 'D' &&
			   (c = getc(infile)) == 'E' &&
			   (c = getc(infile)) == 'F') {
			c = getc(infile);
			defunflag = (c == 'U');
			defvarflag = 0;
			c = getc(infile);
		} else {
			continue;
		}
		while (c != '(') {
			if (c < 0) {
				goto eof;
			}
			c = getc(infile);
		}

		c = getc(infile);
		if (c != '"') {
			continue;
		}
		c = read_c_string(infile, -1, 0);

		if (defunflag) {
			commas = 4;
		} else if (defvarperbufferflag) {
			commas = 2;
		} else if (defvarflag) {
			commas = 1;
		} else {
			/* For DEFSIMPLE and DEFPRED */
			commas = 2;
		}
		for (; commas; c = getc(infile)) {
			if (c == ',') {
				commas--;
				if (defunflag && (commas == 1 || commas == 2)) {
					c = check_comma(
						infile, commas,
						&minargs, &maxargs);
				}
			}
			if (c < 0) {
				goto eof;
			}
		}
		while (c == ' ' || c == '\n' || c == '\t') {
			c = getc(infile);
		}
		if (c == '"') {
			c = read_c_string(infile, 0, 0);
		}
		if (defunflag | defvarflag) {
			while (c != '/') {
				c = getc(infile);
			}
			c = getc(infile);
			while (c == '*') {
				c = getc(infile);
			}
		} else {
			while (c != ',') {
				c = getc(infile);
			}
			c = getc(infile);
		}
		while (c == ' ' || c == '\n' || c == '\t') {
			c = getc(infile);
		}
		if (defunflag | defvarflag) {
			ungetc(c, infile);
		}
		if (defunflag || defvarflag || c == '"') {
			if (ellcc) {
				fprintf(outfile, "\tCDOC%s(\"%s\", \"\\\n",
					defvarflag ? "SYM" : "SUBR", buf);
			} else {
				put_filename (filename); /* XEmacs addition */
				putc(037, outfile);
				putc(defvarflag ? 'V' : 'F', outfile);
				fprintf(outfile, "%s\n", buf);
			}
			c = read_c_string(infile, 1, (defunflag || defvarflag));

			/* If this is a defun, find the arguments and print
			   them.  If this function takes MANY or UNEVALLED args,
			   then the C source won't give the names of the
			   arguments, so we shouldn't bother trying to find
			   them.  */
			if (defunflag && maxargs != -1) {
				char argbuf[1024];
				char *p = argbuf;

				/* Skip into arguments.  */
				while (c != '(') {
					if (c < 0) {
						goto eof;
					}
					c = getc(infile);
				}
				/* Copy arguments into ARGBUF. */
				*p++ = c;
				do {
					c = getc(infile);
					*p++ = (char)(c);
				} while (c != ')');
				*p = '\0';
				/* Output them. */
				if (ellcc) {
					fprintf(outfile, "\\n\\\n\\n\\\n");
				} else {
					fprintf(outfile, "\n\n");
				}
				write_c_args(
					outfile, buf, argbuf, minargs, maxargs);
			}
			if (ellcc) {
				fprintf(outfile, "\\n\");\n\n");
			}
		}
	}
eof:
	fclose(infile);
	return 0;
}

/* Read a file of Lisp code, compiled or interpreted.
 Looks for
  (defun NAME ARGS DOCSTRING ...)
  (defmacro NAME ARGS DOCSTRING ...)
  (autoload (quote NAME) FILE DOCSTRING ...)
  (defvar NAME VALUE DOCSTRING)
  (defconst NAME VALUE DOCSTRING)
  (fset (quote NAME) (make-byte-code ... DOCSTRING ...))
  (fset (quote NAME) #[... DOCSTRING ...])
  (defalias (quote NAME) #[... DOCSTRING ...])
 starting in column zero.
 (quote NAME) may appear as 'NAME as well.

 We also look for #@LENGTH CONTENTS^_ at the beginning of the line.
 When we find that, we save it for the following defining-form,
 and we use that instead of reading a doc string within that defining-form.

 For defun, defmacro, and autoload, we know how to skip over the arglist.
 For defvar, defconst, and fset we skip to the docstring with a kludgy
 formatting convention: all docstrings must appear on the same line as the
 initial open-paren (the one in column zero) and must contain a backslash
 and a double-quote immediately after the initial double-quote.  No newlines
 must appear between the beginning of the form and the first double-quote.
 The only source file that must follow this convention is loaddefs.el; aside
 from that, it is always the .elc file that we look at, and they are no
 problem because byte-compiler output follows this convention.
 The NAME and DOCSTRING are output.
 NAME is preceded by `F' for a function or `V' for a variable.
 An entry is output only if DOCSTRING has \ newline just after the opening "

 Adds the filename a symbol or function was found in before its docstring;
 there's no need for this with the load-history available, but we do it for
 consistency with the C parsing code.
 */

static void
skip_white(FILE *infile)
{
	int c = ' ';
	while (c == ' ' || c == '\t' || c == '\n') {
		c = getc(infile);
	}
	ungetc(c, infile);
	return;
}

static void
read_lisp_symbol(FILE *infile, char *buffer)
{
	int c;
	char *fillp = buffer;

	skip_white(infile);
	while (1) {
		c = getc(infile);
		if (c == '\\') {
			/* FSF has *(++fillp), which is wrong. */
			c = getc(infile);
			if( c < 0 )
				/* IO error... */
				return;
			*fillp++ = (char)(c);
		} else if (c == ' ' ||
			   c == '\t' ||
			   c == '\n' ||
			   c == '(' || c == ')') {
			ungetc(c, infile);
			*fillp = 0;
			break;
		} else {
			*fillp++ = (char)(c);
		}
	}

	if (!buffer[0]) {
		fprintf(stderr, "## expected a symbol, got '%c'\n", c);
	}
	skip_white(infile);
	return;
}

static int
get_dyna_doc(FILE *infile, char **saved_string)
{
	int length = 0;
	int i;
	register int c;

	/* Read the length.  */
	while ((c = getc(infile), c >= '0' && c <= '9')) {
		length *= 10;
		length += c - '0';
	}

	/* The next character is a space that is counted in the length
	   but not part of the doc string.
	   We already read it, so just ignore it.  */
	length--;

	/* Read in the contents.  */
	if (*saved_string != NULL) {
		free(*saved_string);
	}
	*saved_string = xmalloc(length);
	for (i = 0; i < length; i++) {
		c = getc(infile);
		if ( c >= 0 )
			(*saved_string)[i] = (char)(c);
		else {
			(*saved_string)[i] = '\0';
			break;
		}
	}
	/* The last character is a ^_.
	 * That is needed in the .elc file
	 * but it is redundant in DOC.  So get rid of it here.  */
	(*saved_string)[length - 1] = 0;

	/* Skip the newline.  */
	c = getc(infile);
	while (c > 0 && c != '\n') {
		c = getc(infile);
	}
	return c;
}

static int
scan_lisp_file(const char *filename, const char *mode)
{
	FILE *infile;
	register int c;
	char *saved_string = 0;

	infile = fopen(filename, mode);
	if (infile == NULL) {
		perror(filename);
		/* No error */
		return 0;
	}

	c = '\n';
	while (!feof(infile)) {
		char buffer[BUFSIZ];
		char type;

		if (c != '\n') {
			c = getc(infile);
			continue;
		}
		c = getc(infile);
		/* Detect a dynamic doc string and save it for the next
		 * expression. */
		if (c == '#') {
			c = getc(infile);
			if (c == '@') {
				c = get_dyna_doc(infile, &saved_string);
			}
			continue;
		}

		if (c != '(') {
			continue;
		}

		read_lisp_symbol(infile, buffer);

		if (!strcmp(buffer, "defun") || !strcmp(buffer, "defmacro")) {
			type = 'F';
			read_lisp_symbol(infile, buffer);

			/* Skip the arguments: either "nil" or
			 * a list in parens */
			c = getc(infile);
			if (c == 'n') {	/* nil */
				if ((c = getc(infile)) != 'i' ||
				    (c = getc(infile)) != 'l') {
					fprintf(stderr, "\
## unparsable arglist in %s (%s)\n",
						buffer, filename);
					continue;
				}
			} else if (c != '(') {
				fprintf(stderr, "\
## unparsable arglist in %s (%s)\n",
					buffer, filename);
				continue;
			} else {
				while (c != ')') {
					c = getc(infile);
				}
			}
			skip_white(infile);

			/* If the next three characters aren't
			 * `dquote bslash newline' then we're not
			 * reading a docstring. */
			if ((c = getc(infile)) != '"' ||
			    (c = getc(infile)) != '\\' ||
			    (c = getc(infile)) != '\n') {
#ifdef DEBUG
				fprintf(stderr, "\
## non-docstring in %s (%s)\n",
					buffer, filename);
#endif
				continue;
			}

		} else if (!strcmp(buffer, "defvar") ||
			   !strcmp(buffer, "defconst")) {
			char c1 = 0;
			char c2 = 0;

			type = 'V';
			read_lisp_symbol(infile, buffer);

			if (saved_string == 0) {
				/* Skip until the first newline;
				 * remember the two previous chars. */
				while (c != '\n' && c >= 0) {
					/* #### Kludge --
					 * Ignore any ESC x x ISO2022 seqs */
					if (c == 27) {
						(void)getc(infile);
						(void)getc(infile);
						goto nextchar;
					}

					c2 = c1;
					c1 = c;
				nextchar:
					c = getc(infile);
				}

				/* If two previous characters were " and \,
				   this is a doc string.
				   Otherwise, there is none.  */
				if (c2 != '"' || c1 != '\\') {
#ifdef DEBUG
					fprintf(stderr, "\
## non-docstring in %s (%s)\n",
						buffer, filename);
#endif	/* DEBUG */
					continue;
				}
			}

		} else if (!strcmp(buffer, "custom-declare-variable")) {
			char c1 = 0, c2 = 0;
			type = 'V';

			c = getc (infile);
			if (c == '\'') {
				read_lisp_symbol (infile, buffer);
			} else {
				if (c != '(') {
					fprintf(stderr, "\
## unparsable name in custom-declare-variable in %s\n",
						filename);
					continue;
				}
				read_lisp_symbol (infile, buffer);
				if (strcmp (buffer, "quote")) {
					fprintf(stderr, "\
## unparsable name in custom-declare-variable in %s\n",
						filename);
					continue;
				}
				read_lisp_symbol (infile, buffer);
				c = getc (infile);
				if (c != ')') {
					fprintf(stderr, "\
## unparsable quoted name in custom-declare-variable in %s\n",
						filename);
					continue;
				}
			}

			if (saved_string == 0) {
				/* Skip to end of line; remember the two
				   previous chars.  */
				while (c != '\n' && c >= 0) {
					c2 = c1;
					c1 = c;
					/* SXEmacs: shame we can't do this. */
					/* c = getc_skipping_iso2022(infile); */
					(void)getc (infile);
				}

				/* If two previous characters were " and \,
				   this is a doc string.  Otherwise, there is
				   none.  */
				if (c2 != '"' || c1 != '\\') {
#ifdef DEBUG
					fprintf(stderr, "\
## non-docstring in %s (%s)\n",
						buffer, filename);
#endif	/* DEBUG */
					continue;
				}
			}

		} else if (!strcmp(buffer, "fset") ||
			   !strcmp(buffer, "defalias")) {
			char c1 = 0, c2 = 0;
			type = 'F';

			c = getc(infile);
			if (c == '\'') {
				read_lisp_symbol(infile, buffer);
			} else {
				if (c != '(') {
					fprintf(stderr, "\
## unparsable name in fset in %s\n",
						filename);
					continue;
				}
				read_lisp_symbol(infile, buffer);
				if (strcmp(buffer, "quote")) {
					fprintf(stderr, "\
## unparsable name in fset in %s\n",
						filename);
					continue;
				}
				read_lisp_symbol(infile, buffer);
				c = getc(infile);
				if (c != ')') {
					fprintf(stderr, "\
## unparsable quoted name in fset in %s\n",
						filename);
					continue;
				}
			}

			if (saved_string == 0) {
				/* Skip until the first newline;
				 * remember the two previous chars. */
				while (c != '\n' && c >= 0) {
					c2 = c1;
					c1 = c;
					c = getc(infile);
				}

				/* If two previous characters were " and \,
				   this is a doc string.
				   Otherwise, there is none.  */
				if (c2 != '"' || c1 != '\\') {
#ifdef DEBUG
					fprintf(stderr, "\
## non-docstring in %s (%s)\n",
						buffer, filename);
#endif	/* DEBUG */
					continue;
				}
			}

		} else if (!strcmp(buffer, "autoload")) {
			type = 'F';
			c = getc(infile);
			if (c == '\'') {
				read_lisp_symbol(infile, buffer);
			} else {
				if (c != '(') {
					fprintf(stderr, "\
## unparsable name in autoload in %s\n",
						filename);
					continue;
				}
				read_lisp_symbol(infile, buffer);
				if (strcmp(buffer, "quote")) {
					fprintf(stderr, "\
## unparsable name in autoload in %s\n",
						filename);
					continue;
				}
				read_lisp_symbol(infile, buffer);
				c = getc(infile);
				if (c != ')') {
					fprintf(stderr, "\
## unparsable quoted name in autoload in %s\n",
						filename);
					continue;
				}
			}
			skip_white(infile);
			if ((c = getc(infile)) != '\"') {
				fprintf(stderr, "\
## autoload of %s unparsable (%s)\n",
					buffer, filename);
				continue;
			}
			read_c_string(infile, 0, 0);
			skip_white(infile);

			if (saved_string == 0) {
				/* If the next three characters aren't
				   `dquote bslash newline'
				   then we're not reading a docstring.  */
				if ((c = getc(infile)) != '"' ||
				    (c = getc(infile)) != '\\' ||
				    (c = getc(infile)) != '\n') {
#ifdef DEBUG
					fprintf(stderr, "\
## non-docstring in %s (%s)\n",
						buffer, filename);
#endif	/* DEBUG */
					continue;
				}
			}

		} else {
#ifdef DEBUG
			fprintf(stderr, "\
## unrecognized top-level form, %s (%s)\n",
				buffer, filename);
#endif	/* DEBUG */
			continue;
		}

		/* At this point, we should either use the previous
		   dynamic doc string in saved_string
		   or gobble a doc string from the input file.

		   In the latter case, the opening quote (and leading
		   backslash-newline) have already been read.  */
		put_filename (filename); /* XEmacs addition */
		putc(037, outfile);
		putc(type, outfile);
		fprintf(outfile, "%s\n", buffer);
		if (saved_string) {
			fputs(saved_string, outfile);
			/* Don't use one dynamic doc string twice.  */
			free(saved_string);
			saved_string = 0;
		} else {
			read_c_string(infile, 1, 0);
		}
	}
	if (saved_string) {
		/* If this is true then a dynamic doc string was
		   detected without a next expression. We should not
		   emit anything since the input was badly formed,
		   but lets free the string...
		*/
		free(saved_string);
		saved_string = 0;
	}
	fclose(infile);
	return 0;
}

/* make-docfile.c ends here */
