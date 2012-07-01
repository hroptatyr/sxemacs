/* sendmail-like interface to /bin/mail for system V,
   Copyright (C) 1985, 1994 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.28. */

#define NO_SHORTNAMES
#include <config.h>

#if defined (BSD) && !defined (BSD4_1) && !defined (USE_FAKEMAIL)
/* This program is not used in BSD, so just avoid loader complaints.  */
int main(int argc, char *argv[])
{
	return 0;
}
#elif defined (LINUX)
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
	/* Linux /bin/mail, if it exists, is NOT the Unix v7 mail that
	   fakemail depends on!  This causes garbled mail.  Better to
	   output an error message. */
	fprintf(stderr, "Sorry, fakemail does not work on Linux.\n");
	fprintf(stderr, "Make sure you have the sendmail program, and\n");
	fprintf(stderr, "set the Lisp variable `sendmail-program' to point\n");
	fprintf(stderr, "to the path of the sendmail binary.\n");
	return 1;
}
#else				/* not BSD 4.2 (or newer) */

/* These are defined in config in some versions. */

#ifdef static
#undef static
#endif

#ifdef read
#undef read
#undef write
#undef open
#undef close
#endif

#include <stdio.h>
#if __STDC__ || defined(STDC_HEADERS)
#include <stdlib.h>
#include <unistd.h>
#endif
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <pwd.h>

/* Type definitions */

#define boolean int
#define true 1
#define false 0

/* Various lists */

struct line_record {
	char *string;
	struct line_record *continuation;
};
typedef struct line_record *line_list;

struct header_record {
	line_list text;
	struct header_record *next;
	struct header_record *previous;
};
typedef struct header_record *header;

struct stream_record {
	FILE *handle;
	int (*action) (FILE *);
	struct stream_record *rest_streams;
};
typedef struct stream_record *stream_list;

/* A `struct linebuffer' is a structure which holds a line of text.
 * `readline' reads a line from a stream into a linebuffer
 * and works regardless of the length of the line.
 */

struct linebuffer {
	size_t size;
	char *buffer;
};

struct linebuffer lb;

#define new_list()					\
  ((line_list) xmalloc (sizeof (struct line_record)))
#define new_header()				\
  ((header) xmalloc (sizeof (struct header_record)))
#define new_stream()				\
  ((stream_list) xmalloc (sizeof (struct stream_record)))
#define alloc_string(nchars)				\
  ((char *) xmalloc ((nchars) + 1))

/* Global declarations */

#define BUFLEN 1024
#define KEYWORD_SIZE 256
#define FROM_PREFIX "From"
#define MY_NAME "fakemail"
#define NIL ((line_list) NULL)
#define INITIAL_LINE_SIZE 200

#ifndef MAIL_PROGRAM_NAME
#define MAIL_PROGRAM_NAME "/bin/mail"
#endif

#define xstrncpy(d_,s_,l_)			\
	do {					\
		char* dst_=d_;			\
		dst_[0]='\0';			\
		strncat((dst_),(s_),(l_)-1);	\
	} while(0)

static const char *my_name;
static char *the_date;
static char *the_user;
static line_list file_preface;
static stream_list the_streams;
static boolean no_problems = true;

#if !__STDC__ && !defined(STDC_HEADERS)
extern FILE *popen();
extern int fclose(), pclose();
extern char *malloc(), *realloc();
#endif

#if defined(__FreeBSD_version) && __FreeBSD_version >= 400000
#define CURRENT_USER
#endif

#ifdef CURRENT_USER
extern struct passwd *getpwuid();
#if defined(__FreeBSD_version) && __FreeBSD_version >= 400000
extern uid_t geteuid ();
#else
extern unsigned short geteuid();
#endif
static struct passwd *my_entry;
#define cuserid(s)				\
(my_entry = getpwuid ((int) geteuid ()),	\
 my_entry->pw_name)
#endif

/* Utilities */

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

static void error(const char *s1, const char *s2)
{
	printf("%s: ", my_name);
	printf(s1, s2);
	printf("\n");
	no_problems = false;
}

/* Print error message and exit.  */

static void fatal(const char *s1, const char *s2)
{
	error(s1, s2);
	exit(1);
}

/* Like malloc but get fatal error if memory is exhausted.  */

static void *xmalloc(size_t size)
{
	void *result = malloc(size);
	if (result == NULL)
		fatal("virtual memory exhausted", (char *)0);
	return result;
}

static void *xrealloc(void *ptr, size_t size)
{
	void *result = realloc(ptr, size);
	if (result == NULL)
		fatal("virtual memory exhausted", (char *)0);
	return result;
}

/* Initialize a linebuffer for use */

static void init_linebuffer(struct linebuffer *linebuffer)
{
	linebuffer->size = INITIAL_LINE_SIZE;
	linebuffer->buffer = (char *)xmalloc(INITIAL_LINE_SIZE);
}

/* Read a line of text from `stream' into `linebuffer'.
 * Return the length of the line.
 */

static long readline(struct linebuffer *linebuffer, FILE * stream)
{
	char *buffer = linebuffer->buffer;
	char *p = linebuffer->buffer;
	char *end = p + linebuffer->size;

	while (true) {
		int c = getc(stream);
		if (p == end) {
			linebuffer->size *= 2;
			buffer = (char *)xrealloc(buffer, linebuffer->size);
			p = buffer + (p - linebuffer->buffer);
			end = buffer + linebuffer->size;
			linebuffer->buffer = buffer;
		}
		if (c < 0 || c == '\n') {
			*p = 0;
			break;
		}
		*p++ = c;
	}

	return p - buffer;
}

static char *get_keyword(register char *field, char **rest)
{
	static char keyword[KEYWORD_SIZE];
	register char *ptr;
	register char c;

	ptr = &keyword[0];
	c = *field++;
	if ((isspace((int)(unsigned char)c)) || (c == ':'))
		return (char *)NULL;
	*ptr++ = ((islower((int)(unsigned char)c)) ?
		  (toupper((int)(unsigned char)c)) : c);
	while (((c = *field++) != ':') && (!(isspace((int)(unsigned char)c))))
		*ptr++ = ((islower((int)(unsigned char)c)) ?
			  (toupper((int)(unsigned char)c)) : c);
	*ptr++ = '\0';
	while (isspace((int)(unsigned char)c))
		c = *field++;
	if (c != ':')
		return (char *)NULL;
	*rest = field;
	return &keyword[0];
}

static boolean has_keyword(char *field)
{
	char *ignored;
	return (get_keyword(field, &ignored) != (char *)NULL);
}

static char *add_field(line_list the_list, register char *field,
		       register char *where)
{
	register char c;
	while (true) {
		*where++ = ' ';
		while ((c = *field++) != '\0') {
			if (c == '(') {
				while (*field && *field != ')')
					++field;
				if (!(*field++))
					break;	/* no closer */
				if (!(*field))
					break;	/* closerNULL */
				c = *field;
			}
			*where++ =
			    ((c == ',' || c == '>' || c == '<') ? ' ' : c);
		}
		if (the_list == NIL)
			break;
		field = the_list->string;
		the_list = the_list->continuation;
	}
	return where;
}

static line_list make_file_preface(void)
{
	char *the_string, *temp;
	time_t idiotic_interface;
	long prefix_length;
	long user_length;
	long date_length;
	line_list result;
	size_t the_string_len, the_user_len, temp_len;

	prefix_length = strlen(FROM_PREFIX);
	time(&idiotic_interface);
	the_date = ctime(&idiotic_interface);
	/* the_date has an unwanted newline at the end */
	date_length = strlen(the_date) - 1;
	if (the_date[date_length] == '\n')
		the_date[date_length] = '\0';
	temp = cuserid((char *)NULL);
	/* the_user */
	the_user_len = strlen(temp);
	the_user = alloc_string(the_user_len);
	xstrncpy(the_user, temp, the_user_len+1);
	/* alloc the_string */
	the_string_len = 3 + prefix_length + the_user_len + date_length;
	the_string = alloc_string(the_string_len);
	temp_len = the_string_len+1;
	temp = the_string;
	xstrncpy(temp, FROM_PREFIX, temp_len);

	temp = &temp[prefix_length];
	*temp++ = ' ';
	temp_len -= prefix_length + 1;
	xstrncpy(temp, the_user, temp_len);

	temp = &temp[the_user_len];
	*temp++ = ' ';
	temp_len -= the_user_len + 1;
	xstrncpy(temp, the_date, temp_len);

	result = new_list();
	result->string = the_string;
	result->continuation = ((line_list) NULL);
	return result;
}

static void write_line_list(register line_list the_list, FILE * the_stream)
{
	for (;
	     the_list != ((line_list) NULL);
	     the_list = the_list->continuation) {
		fputs(the_list->string, the_stream);
		putc('\n', the_stream);
	}
	return;
}

static int close_the_streams(void)
{
	register stream_list rem;
	for (rem = the_streams;
	     rem != ((stream_list) NULL); rem = rem->rest_streams)
		no_problems = (no_problems &&
			       ((*rem->action) (rem->handle) == 0));
	the_streams = ((stream_list) NULL);
	return (no_problems ? 0 : 1);
}

static void add_a_stream(FILE * the_stream, int (*closing_action) (FILE *))
{
	stream_list old = the_streams;
	the_streams = new_stream();
	the_streams->handle = the_stream;
	the_streams->action = closing_action;
	the_streams->rest_streams = old;
	return;
}

static int my_fclose(FILE * the_file)
{
	putc('\n', the_file);
	fflush(the_file);
	return fclose(the_file);
}

static boolean open_a_file(char *name)
{
	FILE *the_stream = fopen(name, "a");
	if (the_stream != ((FILE *) NULL)) {
		add_a_stream(the_stream, my_fclose);
		if (the_user == (char *)NULL)
			file_preface = make_file_preface();
		write_line_list(file_preface, the_stream);
		return true;
	}
	return false;
}

static void put_string(char *s)
{
	register stream_list rem;
	for (rem = the_streams;
	     rem != ((stream_list) NULL); rem = rem->rest_streams)
		fputs(s, rem->handle);
	return;
}

static void put_line(const char *string)
{
	register stream_list rem;
	for (rem = the_streams;
	     rem != ((stream_list) NULL); rem = rem->rest_streams) {
		const char *s = string;
		int column = 0;

		/* Divide STRING into lines.  */
		while (*s != 0) {
			const char *breakpos;

			/* Find the last char that fits.  */
			for (breakpos = s; *breakpos && column < 78; ++breakpos) {
				if (*breakpos == '\t')
					column += 8;
				else
					column++;
			}
			/* If we didn't reach end of line, break the line.  */
			if (*breakpos) {
				/* Back up to just after the last comma that fits.  */
				while (breakpos != s && breakpos[-1] != ',')
					--breakpos;

				if (breakpos == s) {
					/* If no comma fits, move past the first address anyway.  */
					while (*breakpos != 0
					       && *breakpos != ',')
						++breakpos;
					if (*breakpos != 0)
						/* Include the comma after it.  */
						++breakpos;
				}
			}
			/* Output that much, then break the line.  */
			fwrite(s, 1, breakpos - s, rem->handle);
			column = 8;

			/* Skip whitespace and prepare to print more addresses.  */
			s = breakpos;
			while (*s == ' ' || *s == '\t')
				++s;
			if (*s != 0)
				fputs("\n\t", rem->handle);
		}
		putc('\n', rem->handle);
	}
	return;
}

#define mail_error error

static void setup_files(register line_list the_list, register char *field)
{
	register char *start;
	register char c;
	while (true) {
		while (((c = *field) != '\0') &&
		       ((c == ' ') || (c == '\t') || (c == ',')))
			field += 1;
		if (c != '\0') {
			start = field;
			while (((c = *field) != '\0') &&
			       (c != ' ') && (c != '\t') && (c != ','))
				field += 1;
			*field = '\0';
			if (!open_a_file(start))
				mail_error("Could not open file %s", start);
			*field = c;
			if (c != '\0')
				continue;
		}
		if (the_list == ((line_list) NULL))
			return;
		field = the_list->string;
		the_list = the_list->continuation;
	}
}

static int args_size(header the_header)
{
	register header old = the_header;
	register line_list rem;
	register int size = 0;
	do {
		char *field;
		register char *keyword =
		    get_keyword(the_header->text->string, &field);
		if ((strcmp(keyword, "TO") == 0) || (strcmp(keyword, "CC") == 0)
		    || (strcmp(keyword, "BCC") == 0)) {
			size += 1 + strlen(field);
			for (rem = the_header->text->continuation;
			     rem != NIL; rem = rem->continuation)
				size += 1 + strlen(rem->string);
		}
		the_header = the_header->next;
	} while (the_header != old);
	return size;
}

static void parse_header(header the_header, register char *where)
{
	register header old = the_header;
	do {
		char *field;
		register char *keyword =
		    get_keyword(the_header->text->string, &field);
		if (strcmp(keyword, "TO") == 0)
			where =
			    add_field(the_header->text->continuation, field,
				      where);
		else if (strcmp(keyword, "CC") == 0)
			where =
			    add_field(the_header->text->continuation, field,
				      where);
		else if (strcmp(keyword, "BCC") == 0) {
			where =
			    add_field(the_header->text->continuation, field,
				      where);
			the_header->previous->next = the_header->next;
			the_header->next->previous = the_header->previous;
		} else if (strcmp(keyword, "FCC") == 0)
			setup_files(the_header->text->continuation, field);
		the_header = the_header->next;
	} while (the_header != old);
	*where = '\0';
	return;
}

static header read_header(void)
{
	register header the_header = ((header) NULL);
	register line_list *next_line = ((line_list *) NULL);

	init_linebuffer(&lb);

	do {
		long length;
		register char *line;

		readline(&lb, stdin);
		line = lb.buffer;
		length = strlen(line);
		if (length == 0)
			break;

		if (has_keyword(line)) {
			register header old = the_header;
			the_header = new_header();
			if (old == ((header) NULL)) {
				the_header->next = the_header;
				the_header->previous = the_header;
			} else {
				the_header->previous = old;
				the_header->next = old->next;
				old->next = the_header;
			}
			next_line = &(the_header->text);
		}

		if (next_line == ((line_list *) NULL)) {
			/* Not a valid header */
			exit(1);
		}
		*next_line = new_list();
		(*next_line)->string = alloc_string((size_t) length);
		xstrncpy(((*next_line)->string), length, line);
		next_line = &((*next_line)->continuation);
		*next_line = NIL;

	} while (true);

	return the_header->next;
}

static void write_header(header the_header)
{
	register header old = the_header;
	do {
		register line_list the_list;
		for (the_list = the_header->text;
		     the_list != NIL; the_list = the_list->continuation)
			put_line(the_list->string);
		the_header = the_header->next;
	} while (the_header != old);
	put_line("");
	return;
}

int main(int argc, char *argv[])
{
	char *command_line;
	size_t command_line_len;
	header the_header;
	long name_length;
	char *mail_program_name;
	char buf[BUFLEN + 1];
	register int size;
	FILE *the_pipe;

	mail_program_name = getenv("FAKEMAILER");
	if (!(mail_program_name && *mail_program_name))
		mail_program_name = (char *)MAIL_PROGRAM_NAME;
	name_length = strlen(mail_program_name);

	my_name = MY_NAME;
	the_streams = ((stream_list) NULL);
	the_date = (char *)NULL;
	the_user = (char *)NULL;

	the_header = read_header();
	command_line_len = name_length + args_size(the_header);
	command_line = alloc_string(command_line_len);
	xstrncpy(command_line, command_line_len, mail_program_name);
	parse_header(the_header, &command_line[name_length]);

	the_pipe = popen(command_line, "w");
	if (the_pipe == ((FILE *) NULL))
		fatal("cannot open pipe to real mailer", (char *)NULL);

	add_a_stream(the_pipe, pclose);

	write_header(the_header);

	/* Dump the message itself */

	while (!feof(stdin)) {
		size = fread(buf, 1, BUFLEN, stdin);
		buf[size] = '\0';
		put_string(buf);
	}

	return close_the_streams();
}

#endif				/* not BSD 4.2 (or newer) */
