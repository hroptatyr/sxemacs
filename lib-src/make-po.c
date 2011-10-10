/* Generate .po file from doc-string file.

   Scan specified doc-string file, creating .po format messages for processing
   with msgfmt.  The results go to standard output or to a file specified
   with -a or -o (-a to append, -o to start from nothing).

   Kludge to make up for shortcoming in make-docfile and Snarf-documentation:
   If arg before input filename is -p, we are scanning an add-on
   package, which requires slightly different processing.
*/

#include <stdio.h>
#include <stdlib.h>

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#endif

/* #define BUFSIZE    8192 */
/* #define BUFSIZE    16384 */
#define BUFSIZE    32768
#define NEWSTRING  31		/* Character signalling start of new doc string */
#define LINEEND    "\\n"
#define ENDSTRING  "\"\n"
#define LINEBEGIN  "       \""
#define LINEBREAK  ENDSTRING LINEBEGIN

/* some brain-dead headers define this ... */
#undef FALSE
#undef TRUE
enum boolean { FALSE, TRUE };

/***********************/
/* buffer pseudo-class */
/***********************/

typedef struct _buffer {
	size_t index;		/* current position in buf[] */
	size_t size;		/* size of buf */
	char *buf;
} buffer_struct;

#define BUF_NULL  {0, 0, NULL}

int buf_init(buffer_struct * buffer, size_t size);
void buf_free(buffer_struct * buffer);
void buf_clear(buffer_struct * buffer);
int buf_putc(buffer_struct * buffer, int c);
int buf_print(buffer_struct * buffer, const char *s);

/********************/
/* global variables */
/********************/

FILE *infile = NULL;
FILE *outfile = NULL;
buffer_struct buf = BUF_NULL;

void scan_file(enum boolean package);
void initialize(void);
void clean_exit(int status);
void buf_putc_safe(int c);
void buf_print_safe(const char *s);
void terminate_string(void);

main(int argc, char *argv[])
{
	register int i;
	enum boolean package = FALSE;	/* TRUE if scanning add-on package */

	initialize();

	outfile = stdout;

	/* If first two args are -o FILE, output to FILE. */
	i = 1;
	if (argc > i + 1 && strcmp(argv[i], "-o") == 0) {
		outfile = fopen(argv[++i], "w");
		++i;
	}
	/* ...Or if args are -a FILE, append to FILE. */
	if (argc > i + 1 && strcmp(argv[i], "-a") == 0) {
		outfile = fopen(argv[++i], "a");
		++i;
	}
	if (!outfile) {
		fprintf(stderr, "Unable to open output file %s\n", argv[--i]);
		return 1;
	}

	if (argc > i && !strcmp(argv[i], "-p")) {
		package = TRUE;
		++i;
	}

	infile = fopen(argv[i], "r");
	if (!infile) {
		fprintf(stderr, "Unable to open input file %s\n", argv[i]);
		return 1;
	}

	scan_file(package);
	clean_exit(EXIT_SUCCESS);
}

void scan_file(enum boolean package)
{
	register int c;		/* Character read in */

	fprintf(outfile, "###############\n");
	fprintf(outfile, "# DOC strings #\n");
	fprintf(outfile, "###############\n");

	while (c = getc(infile), !feof(infile)) {
		if (c == NEWSTRING) {
			/* If a string was being processed, terminate it. */
			if (buf.index > 0)
				terminate_string();

			/* Skip function or variable name. */
			while (c != '\n')
				c = getc(infile);
			c = getc(infile);

			/* Begin a new string. */
			fprintf(outfile, "msgid  \"");
			buf_print_safe("msgstr \"");
		}

		if (c == '\n') {
			/* Peek at next character. */
			c = getc(infile);
			ungetc(c, infile);

			/* For add-on (i.e., non-preloaded) documentation, ignore the last
			   carriage return of a string. */
			if (!(package && c == NEWSTRING)) {
				fprintf(outfile, LINEEND);
				buf_print_safe(LINEEND);
			}

			/* If not end of string, continue it on the next line. */
			if (c != NEWSTRING) {
				fprintf(outfile, LINEBREAK);
				buf_print_safe(LINEBREAK);
			}
		} else {

			/* If character is \ or ", precede it by a backslash. */
			if (c == '\\' || c == '\"') {
				putc('\\', outfile);
				buf_putc_safe('\\');
			}

			putc(c, outfile);
			buf_putc_safe(c);
		}
	}
	terminate_string();
}

/* initialize sets up the global variables.
*/
void initialize(void)
{
	if (buf_init(&buf, BUFSIZE) != 0)
		clean_exit(EXIT_FAILURE);
}

/* clean_exit returns any resources and terminates the program.
   An error message is printed if status is EXIT_FAILURE.
*/
void clean_exit(int status)
{
	if (buf.size > 0)
		buf_free(&buf);
	if (outfile)
		fclose(outfile);
	if (infile)
		fclose(infile);

	if (status == EXIT_FAILURE)
		fprintf(stderr, "make-po abnormally terminated\n");
	exit(status);
}

/* buf_putc_safe writes the character c on the global buffer buf,
   checking to make sure that the operation was successful.
*/
void buf_putc_safe(int c)
{
	register int status;

	status = buf_putc(&buf, c);
	if (status == EOF)
		clean_exit(EXIT_FAILURE);
}

/* buf_putc_safe writes the string s on the global buffer buf,
   checking to make sure that the operation was successful.
*/
void buf_print_safe(const char *s)
{
	register int status;

	status = buf_print(&buf, s);
	if (status < 0)
		clean_exit(EXIT_FAILURE);
}

/* terminate_string terminates the current doc string and outputs the buffer.
*/
void terminate_string(void)
{
	fprintf(outfile, ENDSTRING);

	/* Make the "translation" different from the original string. */
	buf_print_safe("_X");

	buf_print_safe(ENDSTRING);
	fprintf(outfile, "%s", buf.buf);
	buf_clear(&buf);
}

/*********************************/
/* buffer pseudo-class functions */
/*********************************/

/* buf_init initializes a buffer to the specified size.
   It returns non-zero if the attempt fails.
*/
int buf_init(buffer_struct * buffer, size_t size)
{
	buffer->buf = malloc(size);
	if (buffer->buf == NULL)
		return 1;

	buffer->size = size;
	buf_clear(buffer);
	return 0;
}

/* buf_free releases the memory allocated for the buffer.
*/
void buf_free(buffer_struct * buffer)
{
	free(buffer->buf);
	buffer->size = 0;
}

/* buf_clear resets a buffer to an empty string.
*/
void buf_clear(buffer_struct * buffer)
{
	buffer->index = 0;
	buffer->buf[0] = '\0';
}

/* buf_putc writes the character c on the buffer.
   It returns the character written, or EOF for error.
*/
int buf_putc(buffer_struct * buffer, int c)
{
	if (buffer->index >= buffer->size)
		return EOF;

	buffer->buf[buffer->index++] = c;
	return c;
}

/* buf_print writes the string s on the buffer.
   It returns the number of characters written, or negative if an error occurred.
*/
int buf_print(buffer_struct * buffer, const char *s)
{
	register int len, sz, msz = buffer->size - buffer->index;

	len = strlen(s);
	if ( len >= msz)
		return -1;

	sz = snprintf(&(buffer->buf[buffer->index]), msz, "%s", s);
	assert(sz>=0 && sz<msz);
	buffer->index += len;
	return len;
}
