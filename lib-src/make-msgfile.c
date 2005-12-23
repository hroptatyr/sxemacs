/*

   PROPOSAL FOR HOW THIS ALL OUGHT TO WORK
   this isn't implemented yet, but this is the plan-in-progress

   In general, it's accepted that the best way to internationalize is for all
   messages to be referred to by a symbolic name (or number) and come out of a
   table or tables, which are easy to change.

   However, with Emacs, we've got the task of internationalizing a huge body
   of existing code, which already contains messages internally.

   For the C code we've got two options:

    - Use a Sun-like gettext() form, which takes an "english" string which
      appears literally in the source, and uses that as a hash key to find
      a translated string;
    - Rip all of the strings out and put them in a table.

   In this case, it's desirable to make as few changes as possible to the C
   code, to make it easier to merge the code with the FSF version of emacs
   which won't ever have these changes made to it.  So we should go with the
   former option.

   The way it has been done (between 19.8 and 19.9) was to use gettext(), but
   *also* to make massive changes to the source code.  The goal now is to use
   gettext() at run-time and yet not require a textual change to every line
   in the C code which contains a string constant.  A possible way to do this
   is described below.

   (gettext() can be implemented in terms of catgets() for non-Sun systems, so
   that in itself isn't a problem.)

   For the Lisp code, we've got basically the same options: put everything in
   a table, or translate things implicitly.

   Another kink that lisp code introduces is that there are thousands of third-
   party packages, so changing the source for all of those is simply not an
   option.

   Is it a goal that if some third party package displays a message which is
   one we know how to translate, then we translate it?  I think this is a
   worthy goal.  It remains to be seen how well it will work in practice.

   So, we should endeavor to minimize the impact on the lisp code.  Certain
   primitive lisp routines (the stuff in lisp/prim/, and especially in
   cmdloop.el and minibuf.el) may need to be changed to know about translation,
   but that's an ideologically clean thing to do because those are considered
   a part of the emacs substrate.

   However, if we find ourselves wanting to make changes to, say, RMAIL, then
   something has gone wrong.  (Except to do things like remove assumptions
   about the order of words within a sentence, or how pluralization works.)

   There are two parts to the task of displaying translated strings to the
   user: the first is to extract the strings which need to be translated from
   the sources; and the second is to make some call which will translate those
   strings before they are presented to the user.

   The old way was to use the same form to do both, that is, GETTEXT() was both
   the tag that we searched for to build a catalog, and was the form which did
   the translation.  The new plan is to separate these two things more: the
   tags that we search for to build the catalog will be stuff that was in there
   already, and the translation will get done in some more centralized, lower
   level place.

   This program (make-msgfile.c) addresses the first part, extracting the
   strings.

   For the emacs C code, we need to recognize the following patterns:

     message ("string" ... )
     error ("string")
     report_file_error ("string" ... )
     signal_simple_error ("string" ... )
     signal_simple_error_2 ("string" ... )

     build_translated_string ("string")
     #### add this and use it instead of build_string() in some places.

     yes_or_no_p ("string" ... )
     #### add this instead of funcalling Qyes_or_no_p directly.

     barf_or_query_if_file_exists	#### restructure this
     check all callers of Fsignal	#### restructure these
     signal_error (Qerror ... )		#### change all of these to error()

     And we also parse out the `interactive' prompts from DEFUN() forms.

     #### When we've got a string which is a candidate for translation, we
     should ignore it if it contains only format directives, that is, if
     there are no alphabetic characters in it that are not a part of a `%'
     directive.  (Careful not to translate either "%s%s" or "%s: ".)

   For the emacs Lisp code, we need to recognize the following patterns:

     (message "string" ... )
     (error "string" ... )
     (format "string" ... )
     (read-from-minibuffer "string" ... )
     (read-shell-command "string" ... )
     (y-or-n-p "string" ... )
     (yes-or-no-p "string" ... )
     (read-file-name "string" ... )
     (temp-minibuffer-message "string")
     (query-replace-read-args "string" ... )

   I expect there will be a lot like the above; basically, any function which
   is a commonly used wrapper around an eventual call to `message' or
   `read-from-minibuffer' needs to be recognized by this program.

     (dgettext "domain-name" "string")		#### do we still need this?

     things that should probably be restructured:
       `princ' in cmdloop.el
       `insert' in debug.el
       face-interactive
       help.el, syntax.el all messed up

   Menu descriptors: one way to extract the strings in menu labels would be
   to teach this program about "^(defvar .*menu\n" forms; that's probably
   kind of hard, though, so perhaps a better approach would be to make this
   program recognize lines of the form

     "string" ... ;###translate

   where the magic token ";###translate" on a line means that the string
   constant on this line should go into the message catalog.  This is analogous
   to the magic ";###autoload" comments, and to the magic comments used in the
   EPSF structuring conventions.

  -----
  So this program manages to build up a catalog of strings to be translated.
  To address the second part of the problem, of actually looking up the
  translations, there are hooks in a small number of low level places in
  emacs.

  Assume the existence of a C function gettext(str) which returns the
  translation of `str' if there is one, otherwise returns `str'.

  - message() takes a char* as its argument, and always filters it through
    gettext() before displaying it.

  - errors are printed by running the lisp function `display-error' which
    doesn't call `message' directly (it princ's to streams), so it must be
    carefully coded to translate its arguments.  This is only a few lines
    of code.

  - Fread_minibuffer_internal() is the lowest level interface to all minibuf
    interactions, so it is responsible for translating the value that will go
    into Vminibuf_prompt.

  - Fpopup_menu filters the menu titles through gettext().

    The above take care of 99% of all messages the user ever sees.

  - The lisp function temp-minibuffer-message translates its arg.

  - query-replace-read-args is funny; it does
      (setq from (read-from-minibuffer (format "%s: " string) ... ))
      (setq to (read-from-minibuffer (format "%s %s with: " string from) ... ))

    What should we do about this?  We could hack query-replace-read-args to
    translate its args, but might this be a more general problem?  I don't
    think we ought to translate all calls to format.  We could just change
    the calling sequence, since this is odd in that the first %s wants to be
    translated but the second doesn't.

  Solving the "translating too much" problem:
  The concern has been raised that in this situation:
   - "Help" is a string for which we know a translation;
   - someone visits a file called Help, and someone does something
     contrived like (error buffer-file-name)
  then we would display the translation of Help, which would not be correct.
  We can solve this by adding a bit to Lisp_String objects which identifies
  them as having been read as literal constants from a .el or .elc file (as
  opposed to having been constructed at run time as it would in the above
  case.)  To solve this:

    - Fmessage() takes a lisp string as its first argument.
      If that string is a constant, that is, was read from a source file
      as a literal, then it calls message() with it, which translates.
      Otherwise, it calls message_no_translate(), which does not translate.

    - Ferror() (actually, Fsignal() when condition is Qerror) works similarly.
*/

/* Scan specified C and Lisp files, extracting the following messages:

     C files:
	GETTEXT (...)
	DEFER_GETTEXT (...)
	DEFUN interactive prompts
     Lisp files:
	(gettext ...)
	(dgettext "domain-name" ...)
	(defer-gettext ...)
	(interactive ...)

  The arguments given to this program are all the C and Lisp source files
  of GNU Emacs.  .el and .c files are allowed.  There is no support for .elc
  files at this time, but they may be specified; the corresponding .el file
  will be used.  Similarly, .o files can also be specified, and the corresponding
  .c file will be used.  This helps the makefile pass the correct list of files.

  The results, which go to standard output or to a file specified with -a or -o
  (-a to append, -o to start from nothing), are quoted strings wrapped in
  gettext(...).  The results can be passed to xgettext to produce a .po message
  file.
*/

#include <stdio.h>
#include <string.h>

#define LINESIZE 256
#define GET_LINE	fgets (line, LINESIZE, infile)
#define CHECK_EOL(p)	if (*(p) == '\0')  (p) = GET_LINE
#define SKIP_BLANKS(p)	while ((*p) == ' ' || (*p) == '\t')  (p)++

enum filetype { C_FILE, LISP_FILE, INVALID_FILE };
/* some brain-dead headers define this ... */
#undef FALSE
#undef TRUE
enum boolean { FALSE, TRUE };

FILE *infile;
FILE *outfile;
char line[LINESIZE];

void scan_file(char *filename);
void process_C_file(void);
void process_Lisp_file(void);
char *copy_up_to_paren(register char *p);
char *copy_quoted_string(register char *p);
enum boolean no_interactive_prompt(register char *q);
char *skip_blanks(register char *p);

main(int argc, char *argv[])
{
	register int i;

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
		return;
	}

	for (; i < argc; i++)
		scan_file(argv[i]);

	return 0;
}

void scan_file(char *filename)
{
	enum filetype type = INVALID_FILE;
	register char *p = filename + strlen(filename);

	if (strcmp(p - 4, ".elc") == 0) {
		*--p = '\0';	/* Use .el file instead */
		type = LISP_FILE;
	} else if (strcmp(p - 3, ".el") == 0)
		type = LISP_FILE;
	else if (strcmp(p - 2, ".o") == 0) {
		*--p = 'c';	/* Use .c file instead */
		type = C_FILE;
	} else if (strcmp(p - 2, ".c") == 0)
		type = C_FILE;

	if (type == INVALID_FILE) {
		fprintf(stderr, "File %s being ignored\n", filename);
		return;
	}
	infile = fopen(filename, "r");
	if (!infile) {
		fprintf(stderr, "Unable to open input file %s\n", filename);
		return;
	}

	fprintf(outfile, "/* %s */\n", filename);
	if (type == C_FILE)
		process_C_file();
	else
		process_Lisp_file();
	fputc('\n', outfile);

	fclose(infile);
}

void process_C_file(void)
{
	register char *p;
	char *gettext, *defun;

	while (p = GET_LINE) {
		gettext = strstr(p, "GETTEXT");
		defun = strstr(p, "DEFUN");
		if (gettext || defun) {
			if (gettext) {
				p = gettext;
				p += 7;	/* Skip over "GETTEXT" */
			} else if (defun) {
				p = defun;
				p += 5;	/* Skip over "DEFUN" */
			}

			p = skip_blanks(p);
			if (*p++ != '(')
				continue;

			if (defun) {
				register int i;

				for (i = 0; i < 5; i++)	/* Skip over commas to doc string */
					while (*p++ != ',')
						CHECK_EOL(p);
				if (*p == '\n')
					p = GET_LINE;
			}

			p = skip_blanks(p);
			if (*p != '\"')	/* Make sure there is a quoted string */
				continue;

			if (defun && no_interactive_prompt(p))
				continue;

			fprintf(outfile, "gettext(");
			if (gettext)
				p = copy_up_to_paren(p);
			else
				p = copy_quoted_string(p);
			fprintf(outfile, ")\n");
		}
	}
}

void process_Lisp_file(void)
{
	register char *p;
	char *gettext, *interactive;
	enum boolean dgettext = FALSE;

	while (p = GET_LINE) {
		gettext = strstr(p, "gettext");
		interactive = strstr(p, "(interactive");
		if (gettext || interactive) {
			if (!interactive)
				p = gettext;
			else if (!gettext)
				p = interactive;
			else if (gettext < interactive) {
				p = gettext;
				interactive = NULL;
			} else {
				p = interactive;
				gettext = NULL;
			}

			if (gettext) {
				if (p > line && *(p - 1) == 'd')
					dgettext = TRUE;
				p += 7;	/* Skip over "gettext" */
			} else
				p += 12;	/* Skip over "(interactive" */

			p = skip_blanks(p);
			if (*p != '\"')	/* Make sure there is a quoted string */
				continue;

			if (dgettext) {	/* Skip first quoted string (domain name) */
				while (*++p != '"') ;	/* null statement */
				++p;
				p = skip_blanks(p);
				if (*p != '\"')	/* Check for second quoted string (message) */
					continue;
			}

			if (interactive && no_interactive_prompt(p))
				continue;

			fprintf(outfile, "gettext(");
			p = copy_up_to_paren(p);
			fprintf(outfile, ")\n");
		}
	}
}

/* Assuming p points to some character beyond an opening parenthesis, copy
   everything to outfile up to but not including the closing parenthesis.
*/
char *copy_up_to_paren(register char *p)
{
	for (;;) {
		SKIP_BLANKS(p);	/* We don't call skip_blanks() in order to */
		CHECK_EOL(p);	/* preserve blanks at the beginning of the line */
		if (*p == ')')
			break;

		if (*p == '\"')
			p = copy_quoted_string(p);
		else
			fputc(*p++, outfile);
	}
	return p;
}

/* Assuming p points to a quote character, copy the quoted string to outfile.
*/
char *copy_quoted_string(register char *p)
{
	do {
		if (*p == '\\')
			fputc(*p++, outfile);
		fputc(*p++, outfile);
		CHECK_EOL(p);
	} while (*p != '\"');

	fputc(*p++, outfile);
	return p;
}

/* Return TRUE if the interactive specification consists only
   of code letters and no prompt.
*/
enum boolean no_interactive_prompt(register char *q)
{
	while (++q, *q == '*' || *q == '@') ;	/* null statement */
	if (*q == '\"')
		return TRUE;
      skip_code_letter:
	if (*++q == '\"')
		return TRUE;
	if (*q == '\\' && *++q == 'n') {
		++q;
		goto skip_code_letter;
	}
	return FALSE;
}

char *skip_blanks(register char *p)
{
	while (*p == ' ' || *p == '\t' || *p == '\n') {
		p++;
		CHECK_EOL(p);
	}
	return p;
}
