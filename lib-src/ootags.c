/* Tags file maker to go with GNU Emacs
   Copyright (C) 1984, 87, 88, 89, 93, 94, 95
   Free Software Foundation, Inc. and Ken Arnold

This file is not considered part of GNU Emacs.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*
 * Authors:
 *	Ctags originally by Ken Arnold.
 *	Fortran added by Jim Kleckner.
 *	Ed Pelegri-Llopart added C typedefs.
 *	Gnu Emacs TAGS format and modifications by RMS?
 *	Sam Kendall added C++.
 *	Francesco Potorti` reorganised C and C++ based on work by Joe Wells.
 *	Regexp tags by Tom Tromey.
 *
 *	Francesco Potorti` (F.Potorti@cnuce.cnr.it) is the current maintainer.
 */

char pot_etags_version[] = "@(#) pot revision number is 12.28";

/* Prototyping magic snarfed from gmalloc.c */
#if defined (__cplusplus) || defined (__STDC__)
#undef	PP
#define	PP(args)	args
#undef	__ptr_t
#define	__ptr_t		void *
#else /* Not C++ or ANSI C.  */
#undef	PP
#define	PP(args)	()
#undef	const
#define	const
#undef	__ptr_t
#define	__ptr_t		char *
#endif /* C++ or ANSI C.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
  /* On some systems, Emacs defines static as nothing for the sake
     of unexec.  We don't want that here since we don't use unexec. */
# undef static
# define ETAGS_REGEXPS		/* use the regexp features */
# define LONG_OPTIONS		/* accept long options */
#endif /* HAVE_CONFIG_H */

#define	TRUE	1
#define	FALSE	0

#ifndef DEBUG
# define DEBUG FALSE
#endif

#ifdef WIN32_NATIVE
# include <stdlib.h>
# include <fcntl.h>
# include <string.h>
# include <io.h>
# define MAXPATHLEN _MAX_PATH
# ifndef HAVE_CONFIG_H
#   define HAVE_GETCWD
# endif /* not HAVE_CONFIG_H */
#endif /* WIN32_NATIVE */

#if !defined (WIN32_NATIVE) && defined (STDC_HEADERS)
#include <stdlib.h>
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# ifdef HAVE_GETCWD
    extern char *getcwd ();
# endif
#endif /* HAVE_UNISTD_H */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#if !defined (S_ISREG) && defined (S_IFREG)
# define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#endif

#ifdef LONG_OPTIONS
# include <getopt.h>
#else
# define getopt_long(argc,argv,optstr,lopts,lind) getopt (argc, argv, optstr)
  extern char *optarg;
  extern int optind, opterr;
#endif /* LONG_OPTIONS */

#ifdef ETAGS_REGEXPS
# include <regex.h>
#endif /* ETAGS_REGEXPS */

/* Define CTAGS to make the program "ctags" compatible with the usual one.
 Leave it undefined to make the program "etags", which makes emacs-style
 tag tables and tags typedefs, #defines and struct/union/enum by default. */
#ifdef CTAGS
# undef  CTAGS
# define CTAGS TRUE
#else
# define CTAGS FALSE
#endif

/* Exit codes for success and failure.  */
#ifdef VMS
# define	GOOD	1
# define	BAD	0
#else
# define	GOOD	0
# define	BAD	1
#endif

/* C extensions. */
#define C_PLPL	0x00001		/* C++ */
#define C_STAR	0x00003		/* C* */
#define C_JAVA	0x00005		/* JAVA */
#define YACC	0x10000		/* yacc file */

#define streq(s,t)	((DEBUG && (s) == NULL && (t) == NULL	\
			  && (abort (), 1)) || !strcmp (s, t))
#define strneq(s,t,n)	((DEBUG && (s) == NULL && (t) == NULL	\
			  && (abort (), 1)) || !strncmp (s, t, n))

#define lowcase(c)	tolower ((char)c)

#define CHARS 256		/* 2^sizeof(char) */
#define CHAR(x)		((unsigned int)x & (CHARS - 1))
#define	iswhite(c)	(_wht[CHAR(c)]) /* c is white */
#define notinname(c)	(_nin[CHAR(c)]) /* c is not in a name */
#define	begtoken(c)	(_btk[CHAR(c)]) /* c can start token */
#define	intoken(c)	(_itk[CHAR(c)]) /* c can be in token */
#define	endtoken(c)	(_etk[CHAR(c)]) /* c ends tokens */

/*#ifdef INFODOCK*/
/*#undef OO_BROWSER*/
/* Due to the way this file is constructed, this unfortunately doesn't */
/* work except for documentation purposes. -slb */
#define OO_BROWSER 1
/*#endif*/

#ifdef OO_BROWSER
#define set_construct(construct) \
  if (!oo_browser_construct) oo_browser_construct = construct
void oo_browser_clear_all_globals(void);
void oo_browser_clear_some_globals(void);
void oo_browser_check_and_clear_structtype(void);
#endif

/*
 *	xnew, xrnew -- allocate, reallocate storage
 *
 * SYNOPSIS:	Type *xnew (int n, Type);
 *		Type *xrnew (OldPointer, int n, Type);
 */
#ifdef chkmalloc
# include "chkmalloc.h"
# define xnew(n,Type)	  ((Type *) trace_malloc (__FILE__, __LINE__, \
						  (n) * sizeof (Type)))
# define xrnew(op,n,Type) ((Type *) trace_realloc (__FILE__, __LINE__, \
						   (op), (n) * sizeof (Type)))
#else
# define xnew(n,Type)	  ((Type *) xmalloc ((n) * sizeof (Type)))
# define xrnew(op,n,Type) ((Type *) xrealloc ((op), (n) * sizeof (Type)))
#endif

typedef int bool;

typedef void Lang_function (FILE *);

typedef struct
{
  char *name;
  Lang_function *function;
  char **suffixes;
  char **interpreters;
} language;

typedef struct node_st
{				/* sorting structure		*/
  char *name;			/* function or type name	*/
#ifdef OO_BROWSER
  short int construct;		/* Construct type for the OO-Browser */
#endif
  char *file;			/* file name			*/
  bool is_func;			/* use pattern or line no	*/
  bool been_warned;		/* set if noticed dup		*/
  int lno;			/* line number tag is on	*/
  long cno;			/* character number line starts on */
  char *pat;			/* search pattern		*/
  struct node_st *left, *right;	/* left and right sons		*/
} node;

#ifdef OO_BROWSER
/* If you add to this array, you must add a corresponding entry to the
   following enum. */
static char *oo_browser_default_classes[] =
  /* Lack of square brackets around some of these entries are intentional. */
  {"null", "class", "method", "[constant]", "[enumeration]", "[enum_label]",
   "extern", "[function]", "[macro]", "objc", "[structure]", "[type]",
   "[union]", "[variable]"};

/* If you add to this enum, you must add a corresponding entry to the
   preceding array. */
enum oo_browser_constructs {C_NULL, C_CLASS, C_METHOD, C_CONSTANT, C_ENUMERATION,
                            C_ENUM_LABEL, C_EXTERN, C_FUNCTION, C_MACRO,
                            C_OBJC, C_STRUCTURE, C_TYPE, C_UNION, C_VARIABLE};

enum oo_browser_constructs oo_browser_construct = C_NULL;
#endif

/*
 * A `linebuffer' is a structure which holds a line of text.
 * `readline_internal' reads a line from a stream into a linebuffer
 * and works regardless of the length of the line.
 * SIZE is the size of BUFFER, LEN is the length of the string in
 * BUFFER after readline reads it.
 */
typedef struct
{
  long size;
  int len;
  char *buffer;
} linebuffer;

extern char *getenv PP ((const char *envvar));

/* Many compilers barf on this:
	Lang_function Asm_labels;
   so let's write it this way */
void Asm_labels PP ((FILE *inf));
void C_entries PP ((int c_ext, FILE *inf));
void default_C_entries PP ((FILE *inf));
void plain_C_entries PP ((FILE *inf));
void Cjava_entries PP ((FILE *inf));
void Cplusplus_entries PP ((FILE *inf));
void Yacc_entries PP ((FILE *inf));
void Cobol_paragraphs PP ((FILE *inf));
void Cstar_entries PP ((FILE *inf));
void Erlang_functions PP ((FILE *inf));
void Fortran_functions PP ((FILE *inf));
void Lisp_functions PP ((FILE *inf));
void Pascal_functions PP ((FILE *inf));
void Perl_functions PP ((FILE *inf));
void Postscript_functions PP ((FILE *inf));
void Prolog_functions PP ((FILE *inf));
void Python_functions PP ((FILE *inf));
void Scheme_functions PP ((FILE *inf));
void TeX_functions PP ((FILE *inf));
void just_read_file PP ((FILE *inf));

void print_language_names PP ((void));
void print_version PP ((void));
void print_help PP ((void));

language *get_language_from_name PP ((char *name));
language *get_language_from_interpreter PP ((char *interpreter));
language *get_language_from_suffix PP ((char *suffix));
int total_size_of_entries PP ((node *np));
long readline PP ((linebuffer *lbp, FILE *stream));
long readline_internal PP ((linebuffer *lbp, FILE *stream));
#ifdef ETAGS_REGEXPS
void analyse_regex PP ((char *regex_arg));
void add_regex PP ((char *regexp_pattern, language *lang));
void free_patterns PP ((void));
#endif /* ETAGS_REGEXPS */
void error PP ((const char *s1, const char *s2));
void suggest_asking_for_help PP ((void));
void fatal PP ((char *s1, char *s2));
void pfatal PP ((char *s1));
void add_node PP ((node *np, node **cur_node_p));

void init PP ((void));
void initbuffer PP ((linebuffer *lbp));
void find_entries PP ((char *file, FILE *inf));
void free_tree PP ((node *np));
void pfnote PP ((char *name, bool is_func, char *linestart, int linelen, int lno, long cno));
void new_pfnote PP ((char *name, int namelen, bool is_func, char *linestart, int linelen, int lno, long cno));
void process_file PP ((char *file));
void put_entries PP ((node *np));
void takeprec PP ((void));

char *concat PP ((char *s1, char *s2, char *s3));
char *skip_spaces PP ((char *cp));
char *skip_non_spaces PP ((char *cp));
char *savenstr PP ((char *cp, int len));
char *savestr PP ((char *cp));
char *etags_strchr PP ((char *sp, int c));
char *etags_strrchr PP ((char *sp, int c));
char *etags_getcwd PP ((void));
char *relative_filename PP ((char *file, char *dir));
char *absolute_filename PP ((char *file, char *dir));
char *absolute_dirname PP ((char *file, char *dir));
bool filename_is_absolute PP ((char *fn));
void canonicalize_filename PP ((char *fn));
void grow_linebuffer PP ((linebuffer *lbp, int toksize));
long *xmalloc PP ((unsigned int size));
long *xrealloc PP ((char *ptr, unsigned int size));


char searchar = '/';		/* use /.../ searches */

char *tagfile;			/* output file */
char *progname;			/* name this program was invoked with */
char *cwd;			/* current working directory */
char *tagfiledir;		/* directory of tagfile */
FILE *tagf;			/* ioptr for tags file */

char *curfile;			/* current input file name */
language *curlang;		/* current language */

int lineno;			/* line number of current line */
long charno;			/* current character number */
long linecharno;		/* charno of start of current line */
char *dbp;			/* pointer to start of current tag */
node *head;			/* the head of the binary tree of tags */

linebuffer lb;			/* the current line */
linebuffer token_name;		/* used by C_entries as a temporary area */
struct
{
  long linepos;
  linebuffer lb;		/* used by C_entries instead of lb */
} lbs[2];

/* boolean "functions" (see init)	*/
bool _wht[CHARS], _nin[CHARS], _itk[CHARS], _btk[CHARS], _etk[CHARS];
char
  /* white chars */
  *white = " \f\t\n\r",
  /* not in a name */
  *nonam = " \f\t\n\r(=,[;",
  /* token ending chars */
  *endtk = " \t\n\r\"'#()[]{}=-+%*/&|^~!<>;,.:?",
  /* token starting chars */
  *begtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$~@",
  /* valid in-token chars */
  *midtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$0123456789";

bool append_to_tagfile;		/* -a: append to tags */
/* The following four default to TRUE for etags, but to FALSE for ctags.  */
bool typedefs;			/* -t: create tags for C typedefs */
bool typedefs_and_cplusplus;	/* -T: create tags for C typedefs, level */
				/* 0 struct/enum/union decls, and C++ */
				/* member functions. */
bool constantypedefs;		/* -d: create tags for C #define, enum */
				/* constants and variables. */
				/* -D: opposite of -d.  Default under ctags. */
bool globals;			/* create tags for global variables */
bool members;			/* create tags for C member variables */
bool update;			/* -u: update tags */
bool vgrind_style;		/* -v: create vgrind style index output */
bool no_warnings;		/* -w: suppress warnings */
bool cxref_style;		/* -x: create cxref style output */
bool cplusplus;			/* .[hc] means C++, not C */
bool noindentypedefs;		/* -I: ignore indentation in C */
#ifdef OO_BROWSER
bool oo_browser_format;		/* -O: OO-Browser tags format */
#endif

#ifdef LONG_OPTIONS
struct option longopts[] =
{
  { "append",			no_argument,	   NULL,     'a'   },
  { "backward-search",		no_argument,	   NULL,     'B'   },
  { "c++",			no_argument,	   NULL,     'C'   },
  { "cxref",			no_argument,	   NULL,     'x'   },
  { "defines",			no_argument,	   NULL,     'd'   },
  { "no-defines",		no_argument,	   NULL,     'D'   },
  { "globals",			no_argument,	   &globals, TRUE  },
  { "no-globals",		no_argument,	   &globals, FALSE },
  { "help",			no_argument,	   NULL,     'h'   },
  { "help",			no_argument,	   NULL,     'H'   },
  { "ignore-indentation",	no_argument,	   NULL,     'I'   },
  { "include",			required_argument, NULL,     'i'   },
  { "language",                 required_argument, NULL,     'l'   },
  { "members",			no_argument,	   &members, TRUE  },
  { "no-members",		no_argument,	   &members, FALSE },
  { "no-warn",			no_argument,	   NULL,     'w'   },
  { "output",			required_argument, NULL,     'o'   },
#ifdef OO_BROWSER
  { "oo-browser",		no_argument,	   NULL,     'O'   },
#endif
#ifdef ETAGS_REGEXPS  
  { "regex",			required_argument, NULL,     'r'   },
  { "no-regex",			no_argument,	   NULL,     'R'   },
#endif /* ETAGS_REGEXPS */  
  { "typedefs",			no_argument,	   NULL,     't'   },
  { "typedefs-and-c++",		no_argument,	   NULL,     'T'   },
  { "update",			no_argument,	   NULL,     'u'   },
  { "version",			no_argument,	   NULL,     'V'   },
  { "vgrind",			no_argument,	   NULL,     'v'   },
  { 0 }
};
#endif /* LONG_OPTIONS */

#ifdef ETAGS_REGEXPS
/* Structure defining a regular expression.  Elements are
   the compiled pattern, and the name string. */
typedef struct pattern
{
  struct pattern *p_next;
  language *language;
  char *regex;
  struct re_pattern_buffer *pattern;
  struct re_registers regs;
  char *name_pattern;
  bool error_signaled;
} pattern;

/* Array of all regexps. */
pattern *p_head = NULL;
#endif /* ETAGS_REGEXPS */

/*
 * Language stuff.
 */

/* Non-NULL if language fixed. */
language *forced_lang = NULL;

/* Assembly code */
char *Asm_suffixes [] = { "a",	/* Unix assembler */
			  "asm", /* Microcontroller assembly */
			  "def", /* BSO/Tasking definition includes  */
			  "inc", /* Microcontroller include files */
			  "ins", /* Microcontroller include files */
			  "s", "sa", /* Unix assembler */
			  "src", /* BSO/Tasking C compiler output */
			  NULL
			};

/* Note that .c and .h can be considered C++, if the --c++ flag was
   given.  That is why default_C_entries is called here. */
char *default_C_suffixes [] =
  { "c", "h", NULL };

char *Cplusplus_suffixes [] =
  { "C", "H", "c++", "cc", "cpp", "cxx", "h++", "hh", "hpp", "hxx",
    "M",			/* Objective C++ */
    "pdb",			/* Postscript with C syntax */
    NULL };

char *Cjava_suffixes [] =
  { "java", NULL };

char *Cobol_suffixes [] =
  { "COB", "cob", NULL };

char *Cstar_suffixes [] =
  { "cs", "hs", NULL };

char *Erlang_suffixes [] =
  { "erl", "hrl", NULL };

char *Fortran_suffixes [] =
  { "F", "f", "f90", "for", NULL };

char *Lisp_suffixes [] =
  { "cl", "clisp", "el", "l", "lisp", "lsp", "ml", NULL };

char *Pascal_suffixes [] =
  { "p", "pas", NULL };

char *Perl_suffixes [] =
  { "pl", "pm", NULL };
char *Perl_interpreters [] =
  { "perl", "@PERL@", NULL };

char *plain_C_suffixes [] =
  { "pc",			/* Pro*C file */
    "m",			/* Objective C file */
    "lm",			/* Objective lex file */
     NULL };

char *Postscript_suffixes [] =
  { "ps", NULL };

char *Prolog_suffixes [] =
  { "prolog", NULL };

char *Python_suffixes [] =
  { "py", NULL };

/* Can't do the `SCM' or `scm' prefix with a version number. */
char *Scheme_suffixes [] =
  { "SCM", "SM", "oak", "sch", "scheme", "scm", "sm", "ss", "t", NULL };

char *TeX_suffixes [] =
  { "TeX", "bib", "clo", "cls", "ltx", "sty", "tex", NULL };

char *Yacc_suffixes [] =
  { "y", "ym", NULL };		/* .ym is Objective yacc file */

/*
 * Table of languages.
 *
 * It is ok for a given function to be listed under more than one
 * name.  I just didn't.
 */

language lang_names [] =
{
  { "asm",     Asm_labels,          Asm_suffixes,         NULL              },
  { "c",       default_C_entries,   default_C_suffixes,   NULL              },
  { "c++",     Cplusplus_entries,   Cplusplus_suffixes,   NULL              },
  { "c*",      Cstar_entries,       Cstar_suffixes,       NULL              },
  { "cobol",   Cobol_paragraphs,    Cobol_suffixes,       NULL              },
  { "erlang",  Erlang_functions,    Erlang_suffixes,      NULL              },
  { "fortran", Fortran_functions,   Fortran_suffixes,     NULL              },
  { "java",    Cjava_entries,       Cjava_suffixes,       NULL              },
  { "lisp",    Lisp_functions,      Lisp_suffixes,        NULL              },
  { "pascal",  Pascal_functions,    Pascal_suffixes,      NULL              },
  { "perl",    Perl_functions,      Perl_suffixes,        Perl_interpreters },
  { "postscript", Postscript_functions, Postscript_suffixes, NULL           },
  { "proc",    plain_C_entries,     plain_C_suffixes,     NULL              },
  { "prolog",  Prolog_functions,    Prolog_suffixes,      NULL              },
  { "python",  Python_functions,    Python_suffixes,      NULL              },
  { "scheme",  Scheme_functions,    Scheme_suffixes,      NULL              },
  { "tex",     TeX_functions,       TeX_suffixes,         NULL              },
  { "yacc",    Yacc_entries,        Yacc_suffixes,        NULL              },
  { "auto", NULL },             /* default guessing scheme */
  { "none", just_read_file },   /* regexp matching only */
  { NULL, NULL }                /* end of list */
};


void
print_language_names ()
{
  language *lang;
  char **ext;

  puts ("\nThese are the currently supported languages, along with the\n\
default file name suffixes:");
  for (lang = lang_names; lang->name != NULL; lang++)
    {
      printf ("\t%s\t", lang->name);
      if (lang->suffixes != NULL)
	for (ext = lang->suffixes; *ext != NULL; ext++)
	  printf (" .%s", *ext);
      puts ("");
    }
  puts ("Where `auto' means use default language for files based on file\n\
name suffix, and `none' means only do regexp processing on files.\n\
If no language is specified and no matching suffix is found,\n\
the first line of the file is read for a sharp-bang (#!) sequence\n\
followed by the name of an interpreter.  If no such sequence is found,\n\
Fortran is tried first; if no tags are found, C is tried next.");
}

#ifndef VERSION
# define VERSION "20"
#endif
void
print_version ()
{
  printf ("%s (GNU Emacs %s)\n", (CTAGS) ? "ctags" : "etags", VERSION);
  puts ("Copyright (C) 1996 Free Software Foundation, Inc. and Ken Arnold");
  puts ("This program is distributed under the same terms as Emacs");

  exit (GOOD);
}

void
print_help ()
{
  printf ("Usage: %s [options] [[regex-option ...] file-name] ...\n\
\n\
These are the options accepted by %s.\n", progname, progname);
#ifdef LONG_OPTIONS
  puts ("You may use unambiguous abbreviations for the long option names.");
#else
  puts ("Long option names do not work with this executable, as it is not\n\
linked with GNU getopt.");
#endif /* LONG_OPTIONS */
  puts ("A - as file name means read names from stdin (one per line).");
  if (!CTAGS)
    printf ("  Absolute names are stored in the output file as they are.\n\
Relative ones are stored relative to the output file's directory.");
  puts ("\n");

  puts ("-a, --append\n\
        Append tag entries to existing tags file.");

  if (CTAGS)
    puts ("-B, --backward-search\n\
        Write the search commands for the tag entries using '?', the\n\
        backward-search command instead of '/', the forward-search command.");

  puts ("-C, --c++\n\
        Treat files whose name suffix defaults to C language as C++ files.");

  if (CTAGS)
    puts ("-d, --defines\n\
        Create tag entries for C #define constants and enum constants, too.");
  else
    puts ("-D, --no-defines\n\
        Don't create tag entries for C #define constants and enum constants.\n\
	This makes the tags file smaller.");

  if (!CTAGS)
    {
      puts ("-i FILE, --include=FILE\n\
        Include a note in tag file indicating that, when searching for\n\
        a tag, one should also consult the tags file FILE after\n\
        checking the current file.");
      puts ("-l LANG, --language=LANG\n\
        Force the following files to be considered as written in the\n\
	named language up to the next --language=LANG option.");
    }

  if (CTAGS)
    puts ("--globals\n\
	Create tag entries for global variables in some languages.");
  else
    puts ("--no-globals\n\
	Do not create tag entries for global variables in some\n\
	languages.  This makes the tags file smaller.");
  puts ("--members\n\
	Create tag entries for member variables in C and derived languages.");

#ifdef ETAGS_REGEXPS
  puts ("-r /REGEXP/, --regex=/REGEXP/ or --regex=@regexfile\n\
        Make a tag for each line matching pattern REGEXP in the\n\
 	following files.  regexfile is a file containing one REGEXP\n\
	per line.  REGEXP is anchored (as if preceded by ^).\n\
	The form /REGEXP/NAME/ creates a named tag.  For example Tcl\n\
	named tags can be created with:\n\
	--regex=/proc[ \\t]+\\([^ \\t]+\\)/\\1/.");
  puts ("-R, --no-regex\n\
        Don't create tags from regexps for the following files.");
#endif /* ETAGS_REGEXPS */
  puts ("-o FILE, --output=FILE\n\
        Write the tags to FILE.");
#ifdef OO_BROWSER
  puts ("-O, --oo-browser\n\
	Generate a specialized tags format used only by the Altrasoft OO-Browser.");
#endif
  puts ("-I, --ignore-indentation\n\
        Don't rely on indentation quite as much as normal.  Currently,\n\
        this means not to assume that a closing brace in the first\n\
        column is the final brace of a function or structure\n\
        definition in C and C++.");

  if (CTAGS)
    {
      puts ("-t, --typedefs\n\
        Generate tag entries for C typedefs.");
      puts ("-T, --typedefs-and-c++\n\
        Generate tag entries for C typedefs, C struct/enum/union tags,\n\
        and C++ member functions.");
      puts ("-u, --update\n\
        Update the tag entries for the given files, leaving tag\n\
        entries for other files in place.  Currently, this is\n\
        implemented by deleting the existing entries for the given\n\
        files and then rewriting the new entries at the end of the\n\
        tags file.  It is often faster to simply rebuild the entire\n\
        tag file than to use this.");
      puts ("-v, --vgrind\n\
        Generates an index of items intended for human consumption,\n\
        similar to the output of vgrind.  The index is sorted, and\n\
        gives the page number of each item.");
      puts ("-w, --no-warn\n\
        Suppress warning messages about entries defined in multiple\n\
        files.");
      puts ("-x, --cxref\n\
        Like --vgrind, but in the style of cxref, rather than vgrind.\n\
        The output uses line numbers instead of page numbers, but\n\
        beyond that the differences are cosmetic; try both to see\n\
        which you like.");
    }

  puts ("-V, --version\n\
        Print the version of the program.\n\
-h, --help\n\
        Print this help message.");

  print_language_names ();

  puts ("");
  puts ("Report bugs to bug-gnu-emacs@prep.ai.mit.edu");

  exit (GOOD);
}


enum argument_type
{
  at_language,
  at_regexp,
  at_filename
};

/* This structure helps us allow mixing of --lang and file names. */
typedef struct
{
  enum argument_type arg_type;
  char *what;
  language *lang;
} argument;

#ifdef VMS			/* VMS specific functions */

#define	EOS	'\0'

/* This is a BUG!  ANY arbitrary limit is a BUG!
   Won't someone please fix this?  */
#define	MAX_FILE_SPEC_LEN	255
typedef struct	{
  short   curlen;
  char    body[MAX_FILE_SPEC_LEN + 1];
} vspec;

/*
 v1.05 nmm 26-Jun-86 fn_exp - expand specification of list of file names
 returning in each successive call the next file name matching the input
 spec. The function expects that each in_spec passed
 to it will be processed to completion; in particular, up to and
 including the call following that in which the last matching name
 is returned, the function ignores the value of in_spec, and will
 only start processing a new spec with the following call.
 If an error occurs, on return out_spec contains the value
 of in_spec when the error occurred.

 With each successive file name returned in out_spec, the
 function's return value is one. When there are no more matching
 names the function returns zero. If on the first call no file
 matches in_spec, or there is any other error, -1 is returned.
*/

#include	<rmsdef.h>
#include	<descrip.h>
#define		OUTSIZE	MAX_FILE_SPEC_LEN
short
fn_exp (out, in)
     vspec *out;
     char *in;
{
  static long context = 0;
  static struct dsc$descriptor_s o;
  static struct dsc$descriptor_s i;
  static bool pass1 = TRUE;
  long status;
  short retval;

  if (pass1)
    {
      pass1 = FALSE;
      o.dsc$a_pointer = (char *) out;
      o.dsc$w_length = (short)OUTSIZE;
      i.dsc$a_pointer = in;
      i.dsc$w_length = (short)strlen(in);
      i.dsc$b_dtype = DSC$K_DTYPE_T;
      i.dsc$b_class = DSC$K_CLASS_S;
      o.dsc$b_dtype = DSC$K_DTYPE_VT;
      o.dsc$b_class = DSC$K_CLASS_VS;
    }
  if ((status = lib$find_file(&i, &o, &context, 0, 0)) == RMS$_NORMAL)
    {
      out->body[out->curlen] = EOS;
      return 1;
    }
  else if (status == RMS$_NMF)
    retval = 0;
  else
    {
      strcpy(out->body, in);
      retval = -1;
    }
  lib$find_file_end(&context);
  pass1 = TRUE;
  return retval;
}

/*
  v1.01 nmm 19-Aug-85 gfnames - return in successive calls the
  name of each file specified by the provided arg expanding wildcards.
*/
char *
gfnames (arg, p_error)
     char *arg;
     bool *p_error;
{
  static vspec filename = {MAX_FILE_SPEC_LEN, "\0"};

  switch (fn_exp (&filename, arg))
    {
    case 1:
      *p_error = FALSE;
      return filename.body;
    case 0:
      *p_error = FALSE;
      return NULL;
    default:
      *p_error = TRUE;
      return filename.body;
    }
}

#ifndef OLD  /* Newer versions of VMS do provide `system'.  */
system (cmd)
     char *cmd;
{
  error ("%s", "system() function not implemented under VMS");
}
#endif

#define	VERSION_DELIM	';'
char *massage_name (s)
     char *s;
{
  char *start = s;

  for ( ; *s; s++)
    if (*s == VERSION_DELIM)
      {
	*s = EOS;
	break;
      }
    else
      *s = lowcase (*s);
  return start;
}
#endif /* VMS */


int
main (int argc, char *argv[])
{
  int i;
  unsigned int nincluded_files;
  char **included_files;
  char *this_file;
  argument *argbuffer;
  int current_arg, file_count;
  linebuffer filename_lb;
#ifdef VMS
  bool got_err;
#endif

#ifdef WIN32_NATIVE
  _fmode = O_BINARY;   /* all of files are treated as binary files */
#endif /* WIN32_NATIVE */

  progname = argv[0];
  nincluded_files = 0;
  included_files = xnew (argc, char *);
  current_arg = 0;
  file_count = 0;

  /* Allocate enough no matter what happens.  Overkill, but each one
     is small. */
  argbuffer = xnew (argc, argument);

#ifdef ETAGS_REGEXPS
  /* Set syntax for regular expression routines. */
  re_set_syntax (RE_SYNTAX_EMACS | RE_INTERVALS);
#endif /* ETAGS_REGEXPS */

  /*
   * If etags, always find typedefs and structure tags.  Why not?
   * Also default is to find macro constants, enum constants and
   * global variables. 
   */
  if (!CTAGS)
    {
      typedefs = typedefs_and_cplusplus = constantypedefs = TRUE;
      globals = TRUE;
      members = FALSE;
    }

  while (1)
    {
      int opt;
      char *optstring;

#ifdef ETAGS_REGEXPS
#ifndef OO_BROWSER
      optstring = "-aCdDf:Il:o:r:RStTi:BuvxwVhH";
#else
      optstring = "-aCdDf:Il:o:r:RStTi:BOuvxwVhH";
#endif
#else
#ifndef OO_BROWSER
      optstring = "-aCdDf:Il:o:StTi:BuvxwVhH";
#else
      optstring = "-aCdDf:Il:o:StTi:BOuvxwVhH";
#endif
#endif /* ETAGS_REGEXPS */

#ifndef LONG_OPTIONS
      optstring = optstring + 1;
#endif /* LONG_OPTIONS */

      opt = getopt_long (argc, argv, optstring, longopts, 0);
      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  /* If getopt returns 0, then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	case 1:
	  /* This means that a file name has been seen.  Record it. */
	  argbuffer[current_arg].arg_type = at_filename;
	  argbuffer[current_arg].what = optarg;
	  ++current_arg;
	  ++file_count;
	  break;

	  /* Common options. */
	case 'a': append_to_tagfile = TRUE;	break;
	case 'C': cplusplus = TRUE;		break;
	case 'd': constantypedefs = TRUE;	break;
	case 'D': constantypedefs = FALSE;	break;
	case 'f':		/* for compatibility with old makefiles */
	case 'o':
	  if (tagfile)
	    {
	      /* convert char to string, to call error with */
	      char buf[2];
	      sprintf (buf, "%c", opt);
	      error ("-%s option may only be given once.", buf);
	      suggest_asking_for_help ();
	    }
	  tagfile = optarg;
	  break;
#ifdef OO_BROWSER
	case 'O':
	  oo_browser_format = TRUE;
	  break;
#endif
	case 'I':
	case 'S':		/* for backward compatibility */
	  noindentypedefs = TRUE;
	  break;
	case 'l':
	  {
	    language *lang = get_language_from_name (optarg);
	    if (lang != NULL)
	      {
		argbuffer[current_arg].lang = lang;
		argbuffer[current_arg].arg_type = at_language;
		++current_arg;
	      }
	  }
	  break;
#ifdef ETAGS_REGEXPS
	case 'r':
	  argbuffer[current_arg].arg_type = at_regexp;
	  argbuffer[current_arg].what = optarg;
	  ++current_arg;
	  break;
	case 'R':
	  argbuffer[current_arg].arg_type = at_regexp;
	  argbuffer[current_arg].what = NULL;
	  ++current_arg;
	  break;
#endif /* ETAGS_REGEXPS */
	case 'V':
	  print_version ();
	  break;
	case 'h':
	case 'H':
	  print_help ();
	  break;
	case 't':
	  typedefs = TRUE;
	  break;
	case 'T':
	  typedefs = typedefs_and_cplusplus = TRUE;
	  break;
#if (!CTAGS)
	  /* Etags options */
	case 'i':
	  included_files[nincluded_files++] = optarg;
	  break;
#else /* CTAGS */
	  /* Ctags options. */
	case 'B': searchar = '?';	break;
	case 'u': update = TRUE;	break;
	case 'v': vgrind_style = TRUE;	/*FALLTHRU*/
	case 'x': cxref_style = TRUE;	break;
	case 'w': no_warnings = TRUE;	break;
#endif /* CTAGS */
	default:
	  suggest_asking_for_help ();
	}
    }

  for (; optind < argc; ++optind)
    {
      argbuffer[current_arg].arg_type = at_filename;
      argbuffer[current_arg].what = argv[optind];
      ++current_arg;
      ++file_count;
    }

  if (nincluded_files == 0 && file_count == 0)
    {
      error ("no input files specified.", 0);
      suggest_asking_for_help ();
    }

  if (tagfile == NULL)
    tagfile = CTAGS ? "tags" : "TAGS";
  cwd = etags_getcwd ();	/* the current working directory */
  if (cwd[strlen (cwd) - 1] != '/')
    {
      char *oldcwd = cwd;
      cwd = concat (oldcwd, "/", "");
      free (oldcwd);
    }
  if (streq (tagfile, "-"))
    tagfiledir = cwd;
  else
    tagfiledir = absolute_dirname (tagfile, cwd);

  init ();			/* set up boolean "functions" */

  initbuffer (&lb);
  initbuffer (&token_name);
  initbuffer (&lbs[0].lb);
  initbuffer (&lbs[1].lb);
  initbuffer (&filename_lb);

  if (!CTAGS)
    {
      if (streq (tagfile, "-"))
	{
	  tagf = stdout;
#ifdef WIN32_NATIVE
	  /* Switch redirected `stdout' to binary mode (setting `_fmode'
	     doesn't take effect until after `stdout' is already open). */
	  if (!isatty (fileno (stdout)))
	    setmode (fileno (stdout), O_BINARY);
#endif /* WIN32_NATIVE */
	}
      else
	tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
      if (tagf == NULL)
	pfatal (tagfile);
    }

  /*
   * Loop through files finding functions.
   */
  for (i = 0; i < current_arg; ++i)
    {
      switch (argbuffer[i].arg_type)
	{
	case at_language:
	  forced_lang = argbuffer[i].lang;
	  break;
#ifdef ETAGS_REGEXPS
	case at_regexp:
	  analyse_regex (argbuffer[i].what);
	  break;
#endif
	case at_filename:
#ifdef VMS
	  while ((this_file = gfnames (argbuffer[i].what, &got_err)) != NULL)
	    {
	      if (got_err)
		{
		  error ("can't find file %s\n", this_file);
		  argc--, argv++;
		}
	      else
		{
		  this_file = massage_name (this_file);
		}
#else
	      this_file = argbuffer[i].what;
#endif
#ifdef OO_BROWSER
	      oo_browser_clear_all_globals();
#endif
	      /* Input file named "-" means read file names from stdin
		 (one per line) and use them. */
	      if (streq (this_file, "-"))
		while (readline_internal (&filename_lb, stdin) > 0)
#ifdef OO_BROWSER
		  {
		    oo_browser_clear_some_globals();
#endif
		  process_file (filename_lb.buffer);
#ifdef OO_BROWSER
		  }
#endif
	      else
		process_file (this_file);
#ifdef VMS
	    }
#endif
	  break;
	}
    }

#ifdef ETAGS_REGEXPS
  free_patterns ();
#endif /* ETAGS_REGEXPS */

  if (!CTAGS)
    {
      while (nincluded_files-- > 0)
	fprintf (tagf, "\f\n%s,include\n", *included_files++);

      fclose (tagf);
      exit (GOOD);
    }

  /* If CTAGS, we are here.  process_file did not write the tags yet,
     because we want them ordered.  Let's do it now. */
  if (cxref_style)
    {
      put_entries (head);
      exit (GOOD);
    }

  if (update)
    {
      char cmd[BUFSIZ];
      for (i = 0; i < current_arg; ++i)
	{
	  if (argbuffer[i].arg_type != at_filename)
	    continue;
	  sprintf (cmd,
		   "mv %s OTAGS;fgrep -v '\t%s\t' OTAGS >%s;rm OTAGS",
		   tagfile, argbuffer[i].what, tagfile);
	  if (system (cmd) != GOOD)
	    fatal ("failed to execute shell command", (char *)NULL);
	}
      append_to_tagfile = TRUE;
    }

  tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
  if (tagf == NULL)
    pfatal (tagfile);
  put_entries (head);
  fclose (tagf);

  if (update)
    {
      char cmd[BUFSIZ];
      sprintf (cmd, "sort %s -o %s", tagfile, tagfile);
      exit (system (cmd));
    }
  return GOOD;
}


/*
 * Return a language given the name.
 */
language *
get_language_from_name (name)
     char *name;
{
  language *lang;

  if (name == NULL)
    error ("empty language name", (char *)NULL);
  else
    {
      for (lang = lang_names; lang->name != NULL; lang++)
	if (streq (name, lang->name))
	  return lang;
      error ("unknown language \"%s\"", name);
    }

  return NULL;
}


/*
 * Return a language given the interpreter name.
 */
language *
get_language_from_interpreter (interpreter)
     char *interpreter;
{
  language *lang;
  char **iname;

  if (interpreter == NULL)
    return NULL;
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->interpreters != NULL)
      for (iname = lang->interpreters; *iname != NULL; iname++)
	if (streq (*iname, interpreter))
	    return lang;

  return NULL;
}



/*
 * Return a language given the file suffix.
 */
language *
get_language_from_suffix (suffix)
     char *suffix;
{
  language *lang;
  char **ext;

  if (suffix == NULL)
    return NULL;
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->suffixes != NULL)
      for (ext = lang->suffixes; *ext != NULL; ext++)
	if (streq (*ext, suffix))
	    return lang;

  return NULL;
}


/*
 * This routine is called on each file argument.
 */
void
process_file (file)
     char *file;
{
  struct stat stat_buf;
  FILE *inf;

  canonicalize_filename (file);
  if (stat (file, &stat_buf) == 0 && !S_ISREG (stat_buf.st_mode))
    {
      error ("skipping %s: it is not a regular file.", file);
      return;
    }
  if (streq (file, tagfile) && !streq (tagfile, "-"))
    {
      error ("skipping inclusion of %s in self.", file);
      return;
    }
  inf = fopen (file, "r");
  if (inf == NULL)
    {
      perror (file);
      return;
    }

  find_entries (file, inf);

  if (!CTAGS)
    {
      char *filename;

      if (filename_is_absolute (file))
	{
	  /* file is an absolute file name.  Canonicalise it. */
	  filename = absolute_filename (file, cwd);
	}
      else
	{
	  /* file is a file name relative to cwd.  Make it relative
	     to the directory of the tags file. */
	  filename = relative_filename (file, tagfiledir);
	}
#ifdef OO_BROWSER
      if (oo_browser_format)
	fprintf (tagf, "\f\n%s\n", filename);
      else
#endif
      fprintf (tagf, "\f\n%s,%d\n", filename, total_size_of_entries (head));
      free (filename);
      put_entries (head);
      free_tree (head);
      head = NULL;
    }
}

/*
 * This routine sets up the boolean pseudo-functions which work
 * by setting boolean flags dependent upon the corresponding character.
 * Every char which is NOT in that string is not a white char.  Therefore,
 * all of the array "_wht" is set to FALSE, and then the elements
 * subscripted by the chars in "white" are set to TRUE.  Thus "_wht"
 * of a char is TRUE if it is the string "white", else FALSE.
 */
void
init ()
{
  register char *sp;
  register int i;

  for (i = 0; i < CHARS; i++)
    iswhite(i) = notinname(i) = begtoken(i) = intoken(i) = endtoken(i) = FALSE;
  for (sp = white; *sp != '\0'; sp++) iswhite (*sp) = TRUE;
  for (sp = nonam; *sp != '\0'; sp++) notinname (*sp) = TRUE;
  for (sp = begtk; *sp != '\0'; sp++) begtoken (*sp) = TRUE;
  for (sp = midtk; *sp != '\0'; sp++) intoken (*sp) = TRUE;
  for (sp = endtk; *sp != '\0'; sp++) endtoken (*sp) = TRUE;
  iswhite('\0') = iswhite('\n');
  notinname('\0') = notinname('\n');
  begtoken('\0') = begtoken('\n');
  intoken('\0') = intoken('\n');
  endtoken('\0') = endtoken('\n');
}

/*
 * This routine opens the specified file and calls the function
 * which finds the function and type definitions.
 */
node *last_node = NULL;

void
find_entries (file, inf)
     char *file;
     FILE *inf;
{
  char *cp;
  language *lang;
  node *old_last_node;

  curfile = savestr (file);

  /* If user specified a language, use it. */
  lang = forced_lang;
  if (lang != NULL && lang->function != NULL)
    {
      curlang = lang;
      lang->function (inf);
      free (curfile);
      fclose (inf);
      return;
    }

  cp = etags_strrchr (file, '.');
  if (cp != NULL)
    {
      cp += 1;
      lang = get_language_from_suffix (cp);
      if (lang != NULL && lang->function != NULL)
	{
	  curlang = lang;
	  lang->function (inf);
	  free (curfile);
	  fclose (inf);
	  return;
	}
    }

  /* Look for sharp-bang as the first two characters. */
  if (readline_internal (&lb, inf) > 0
      && lb.len >= 2
      && lb.buffer[0] == '#'
      && lb.buffer[1] == '!')
    {
      char *lp;

      /* Set lp to point at the first char after the last slash in the
         line or, if no slashes, at the first nonblank.  Then set cp to
	 the first successive blank and terminate the string. */
      lp = etags_strrchr (lb.buffer+2, '/');
      if (lp != NULL)
	lp += 1;
      else
	lp = skip_spaces (lb.buffer + 2);
      cp = skip_non_spaces (lp);
      *cp = '\0';

      if (strlen (lp) > 0)
	{
	  lang = get_language_from_interpreter (lp);
	  if (lang != NULL && lang->function != NULL)
	    {
	      curlang = lang;
	      lang->function (inf);
	      fclose (inf);
	      free (curfile);
	      return;
	    }
	}
    }
  rewind (inf);

  /* Try Fortran. */
  old_last_node = last_node;
  curlang = get_language_from_name ("fortran");
  Fortran_functions (inf);

  /* No Fortran entries found.  Try C. */
  if (old_last_node == last_node)
    {
      rewind (inf);
      curlang = get_language_from_name (cplusplus ? "c++" : "c");
      default_C_entries (inf);
    }
  free (curfile);
  fclose (inf);
  return;
}

/* Record a tag. */
void
pfnote (name, is_func, linestart, linelen, lno, cno)
     char *name;		/* tag name, or NULL if unnamed */
     bool is_func;		/* tag is a function */
     char *linestart;		/* start of the line where tag is */
     int linelen;		/* length of the line where tag is */
     int lno;			/* line number */
     long cno;			/* character number */
{
  register node *np;

  if (CTAGS && name == NULL)
    return;

  np = xnew (1, node);

  /* If ctags mode, change name "main" to M<thisfilename>. */
  if (CTAGS && !cxref_style && streq (name, "main"))
    {
      register char *fp = etags_strrchr (curfile, '/');
      np->name = concat ("M", fp == 0 ? curfile : fp + 1, "");
      fp = etags_strrchr (np->name, '.');
      if (fp && fp[1] != '\0' && fp[2] == '\0')
	fp[0] = 0;
    }
  else
    np->name = name;
  np->been_warned = FALSE;
  np->file = curfile;
  np->is_func = is_func;
  np->lno = lno;
  /* Our char numbers are 0-base, because of C language tradition?
     ctags compatibility?  old versions compatibility?   I don't know.
     Anyway, since emacs's are 1-base we expect etags.el to take care
     of the difference.  If we wanted to have 1-based numbers, we would
     uncomment the +1 below. */
  np->cno = cno /* + 1 */ ;
  np->left = np->right = NULL;
  if (CTAGS && !cxref_style)
    {
      if (strlen (linestart) < 50)
	np->pat = concat (linestart, "$", "");
      else
	np->pat = savenstr (linestart, 50);
    }
  else
    np->pat = savenstr (linestart, linelen);

#ifdef OO_BROWSER
  if (oo_browser_format)
    np->construct = oo_browser_construct;
  oo_browser_construct = C_NULL;
  oo_browser_check_and_clear_structtype();
#endif

  add_node (np, &head);
}

/* Date: Wed, 22 Jan 1997 02:56:31 -0500 [last amended 18 Sep 1997]
 * From: Sam Kendall <kendall@mv.mv.com>
 * Subject: Proposal for firming up the TAGS format specification
 * To: F.Potorti@cnuce.cnr.it
 *
 * pfnote should emit the optimized form [unnamed tag] only if:
 *  1. name does not contain any of the characters " \t\r\n(),;";
 *  2. linestart contains name as either a rightmost, or rightmost but
 *     one character, substring;
 *  3. the character, if any, immediately before name in linestart must
 *     be one of the characters " \t(),;";
 *  4. the character, if any, immediately after name in linestart must
 *     also be one of the characters " \t(),;".
 *
 * The real implementation uses the notinname() macro, which recognises
 * characters slightly different form " \t\r\n(),;".  See the variable
 * `nonam'.
 */
#define traditional_tag_style TRUE
void
new_pfnote (name, namelen, is_func, linestart, linelen, lno, cno)
     char *name;		/* tag name, or NULL if unnamed */
     int namelen;		/* tag length */
     bool is_func;		/* tag is a function */
     char *linestart;		/* start of the line where tag is */
     int linelen;		/* length of the line where tag is */
     int lno;			/* line number */
     long cno;			/* character number */
{
  register char *cp;
  bool named;

  named = TRUE;
  if (!CTAGS)
    {
      for (cp = name; !notinname (*cp); cp++)
	continue;
      if (*cp == '\0')				/* rule #1 */
	{
	  cp = linestart + linelen - namelen;
	  if (notinname (linestart[linelen-1]))
	    cp -= 1;				/* rule #4 */
#ifdef OO_BROWSER
	  if (!oo_browser_format
	      && cp >= linestart		/* rule #2 */
#else
	  if (cp >= linestart			/* rule #2 */
#endif
	      && (cp == linestart
		  || notinname (cp[-1]))	/* rule #3 */
	      && strneq (name, cp, namelen))	/* rule #2 */
	    named = FALSE;	/* use unnamed tag */
	}
    }
  
  if (named)
    name = savenstr (name, namelen);
  else
    name = NULL;
  pfnote (name, is_func, linestart, linelen, lno, cno);
}

/*
 * free_tree ()
 *	recurse on left children, iterate on right children.
 */
void
free_tree (np)
     register node *np;
{
  while (np)
    {
      register node *node_right = np->right;
      free_tree (np->left);
      if (np->name != NULL)
	free (np->name);
      free (np->pat);
      free (np);
      np = node_right;
    }
}

/*
 * add_node ()
 *	Adds a node to the tree of nodes.  In etags mode, we don't keep
 *	it sorted; we just keep a linear list.  In ctags mode, maintain
 *	an ordered tree, with no attempt at balancing.
 *
 *	add_node is the only function allowed to add nodes, so it can
 *	maintain state.
 */
void
add_node (np, cur_node_p)
     node *np, **cur_node_p;
{
  register int dif;
  register node *cur_node = *cur_node_p;

  if (cur_node == NULL)
    {
      *cur_node_p = np;
      last_node = np;
      return;
    }

  if (!CTAGS)
    {
      /* Etags Mode */
      if (last_node == NULL)
	fatal ("internal error in add_node", (char *)NULL);
      last_node->right = np;
      last_node = np;
    }
  else
    {
      /* Ctags Mode */
      dif = strcmp (np->name, cur_node->name);

      /*
       * If this tag name matches an existing one, then
       * do not add the node, but maybe print a warning.
       */
      if (!dif)
	{
	  if (streq (np->file, cur_node->file))
	    {
	      if (!no_warnings)
		{
		  fprintf (stderr, "Duplicate entry in file %s, line %d: %s\n",
			   np->file, lineno, np->name);
		  fprintf (stderr, "Second entry ignored\n");
		}
	    }
	  else if (!cur_node->been_warned && !no_warnings)
	    {
	      fprintf
		(stderr,
		 "Duplicate entry in files %s and %s: %s (Warning only)\n",
		 np->file, cur_node->file, np->name);
	      cur_node->been_warned = TRUE;
	    }
	  return;
	}

      /* Actually add the node */
      add_node (np, dif < 0 ? &cur_node->left : &cur_node->right);
    }
}

#ifdef OO_BROWSER
/* Default class name for the current OO-Browser tag. */
static char *oo_browser_class;
/* Prefix character to use in OO-Browser listings for the current tag. */
static char oo_browser_prefix;
#endif

void
put_entries (node *np)
{
  register char *sp;

  if (np == NULL)
    return;

  /* Output subentries that precede this one */
  put_entries (np->left);

  /* Output this entry */

  if (!CTAGS)
    {
#ifdef OO_BROWSER
      if (oo_browser_format)
        {
          /* Omit C++ `class' and `method' entries as well as Objective-C
             entries from this OO-Browser tags file since the browser handles
             them independently of this file.  Omit `extern' variable declarations
             as they are unused by the OO-Browser. */
          if (np->construct != C_CLASS
              && np->construct != C_METHOD
              && np->construct != C_EXTERN
              && np->construct != C_OBJC)
            {
              oo_browser_class = oo_browser_default_classes[np->construct];
              switch (np->construct)
                {
                case C_CONSTANT:
                case C_ENUMERATION:
                case C_ENUM_LABEL:
                case C_STRUCTURE:
                case C_TYPE:
                case C_UNION:
                case C_VARIABLE:
                  oo_browser_prefix = '=';
                  break;
                case C_FUNCTION:
                case C_MACRO:
                  oo_browser_prefix = '-';
                  break;
                }
              if (np->name != NULL)
                fprintf (tagf, "%s@%c %s@%s\n",
                         oo_browser_class, oo_browser_prefix,
                         np->name, np->pat);
              else
                fprintf (tagf, "%s@%c ???@%s\n",
                         oo_browser_class, oo_browser_prefix, np->pat);
            }
        }
      else
        {
#endif
      if (np->name != NULL)
	fprintf (tagf, "%s\177%s\001%d,%ld\n",
		 np->pat, np->name, np->lno, np->cno);
      else
	fprintf (tagf, "%s\177%d,%ld\n",
		 np->pat, np->lno, np->cno);
#ifdef OO_BROWSER
	}
#endif
    }
  else
    {
      if (np->name == NULL)
	error ("internal error: NULL name in ctags mode.", (char *)NULL);

      if (cxref_style)
	{
	  if (vgrind_style)
	    fprintf (stdout, "%s %s %d\n",
		     np->name, np->file, (np->lno + 63) / 64);
	  else
	    fprintf (stdout, "%-16s %3d %-16s %s\n",
		     np->name, np->lno, np->file, np->pat);
	}
      else
	{
	  fprintf (tagf, "%s\t%s\t", np->name, np->file);

	  if (np->is_func)
	    {			/* a function */
	      putc (searchar, tagf);
	      putc ('^', tagf);

	      for (sp = np->pat; *sp; sp++)
		{
		  if (*sp == '\\' || *sp == searchar)
		    putc ('\\', tagf);
		  putc (*sp, tagf);
		}
	      putc (searchar, tagf);
	    }
	  else
	    {			/* a typedef; text pattern inadequate */
	      fprintf (tagf, "%d", np->lno);
	    }
	  putc ('\n', tagf);
	}
    }

  /* Output subentries that follow this one */
  put_entries (np->right);
}

/* Length of a number's decimal representation. */
int number_len PP ((long num));
int
number_len (num)
     long num;
{
  int len = 1;
  while ((num /= 10) > 0)
    len += 1;
  return len;
}

/*
 * Return total number of characters that put_entries will output for
 * the nodes in the subtree of the specified node.  Works only if
 * we are not ctags, but called only in that case.  This count
 * is irrelevant with the new tags.el, but is still supplied for
 * backward compatibility.
 */
int
total_size_of_entries (np)
     register node *np;
{
  register int total;

  if (np == NULL)
    return 0;

  for (total = 0; np != NULL; np = np->right)
    {
      /* Count left subentries. */
      total += total_size_of_entries (np->left);

      /* Count this entry */
      total += strlen (np->pat) + 1;
      total += number_len ((long) np->lno) + 1 + number_len (np->cno) + 1;
      if (np->name != NULL)
	total += 1 + strlen (np->name);	/* \001name */
    }

  return total;
}

/*
 * The C symbol tables.
 */
enum sym_type
{
  st_none,
  st_C_objprot, st_C_objimpl, st_C_objend,
  st_C_gnumacro,
  st_C_ignore,
  st_C_javastruct,
  st_C_struct, st_C_enum, st_C_define, st_C_typedef, st_C_typespec,
  st_C_const
#ifdef OO_BROWSER
  , st_C_union, st_C_class, st_C_extern, st_C_inline
#endif
};

/* Feed stuff between (but not including) %[ and %] lines to:
      gperf -c -k 1,3 -o -p -r -t
%[
struct C_stab_entry { char *name; int c_ext; enum sym_type type; }
%%
@interface,	0,	st_C_objprot
@protocol,	0,	st_C_objprot
@implementation,0,	st_C_objimpl
@end,		0,	st_C_objend
import,		C_JAVA,	st_C_ignore
package,	C_JAVA,	st_C_ignore
friend,		C_PLPL,	st_C_ignore
extends,  	C_JAVA,	st_C_javastruct
implements,  	C_JAVA,	st_C_javastruct
interface,	C_JAVA, st_C_struct
class,  	C_PLPL,	st_C_class
namespace,	C_PLPL,	st_C_struct
domain, 	C_STAR,	st_C_struct
union,  	0,	st_C_union
struct, 	0,	st_C_struct
enum,    	0,	st_C_enum
typedef, 	0,	st_C_typedef
define,  	0,	st_C_define
inline,		0,	st_C_inline
bool,		C_PLPL,	st_C_typespec
long,    	0,	st_C_typespec
short,   	0,	st_C_typespec
int,     	0,	st_C_typespec
char,    	0,	st_C_typespec
float,   	0,	st_C_typespec
double,  	0,	st_C_typespec
signed,  	0,	st_C_typespec
unsigned,	0,	st_C_typespec
auto,    	0,	st_C_typespec
void,    	0,	st_C_typespec
extern,  	0,	st_C_extern
static,  	0,	st_C_typespec
const,   	0,	st_C_const
volatile,	0,	st_C_typespec
explicit,	C_PLPL,	st_C_typespec
mutable,	C_PLPL,	st_C_typespec
typename,	C_PLPL,	st_C_typespec
# DEFUN used in emacs, the next three used in glibc (SYSCALL only for mach).
DEFUN,		0,	st_C_gnumacro
SYSCALL,	0,	st_C_gnumacro
ENTRY,		0,	st_C_gnumacro
PSEUDO,		0,	st_C_gnumacro
# These are defined inside C functions, so currently they are not met.
# EXFUN used in glibc, DEFVAR_* in emacs.
#EXFUN,		0,	st_C_gnumacro
#DEFVAR_,	0,	st_C_gnumacro
%]
and replace lines between %< and %> with its output. */
/*%<*/
/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -c -k 1,3 -o -p -r -t  */
struct C_stab_entry { char *name; int c_ext; enum sym_type type; };

#define TOTAL_KEYWORDS 41
#define MIN_WORD_LENGTH 3
#define MAX_WORD_LENGTH 15
#define MIN_HASH_VALUE 13
#define MAX_HASH_VALUE 129
/* maximum key range = 117, duplicates = 0 */

static unsigned int
hash (char *str, unsigned int len)
{
  static unsigned char asso_values[] =
    {
     130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
     130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
     130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
     130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
     130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
     130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
     130, 130, 130, 130,  13, 130, 130, 130,  33,  32,
      47, 130, 130, 130, 130, 130, 130, 130, 130, 130,
       5, 130, 130,  20,  32, 130, 130, 130, 130, 130,
     130, 130, 130, 130, 130, 130, 130,  47,  55,   8,
      15,  33,  61,  38, 130,  60, 130, 130,   2,   9,
      10,  62,  59, 130,  28,  27,  50,  19,   3, 130,
     130, 130, 130, 130, 130, 130, 130, 130,
    };
  return len +
    asso_values[(unsigned char) str[2]] +
    asso_values[(unsigned char) str[0]];
}

static struct C_stab_entry *
in_word_set (char *str, unsigned int len)
{
  static struct C_stab_entry wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, 
      {"volatile", 	0,	st_C_typespec},
      {"",}, {"",}, 
      {"long",     	0,	st_C_typespec},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"const",    	0,	st_C_const},
      {"",}, {"",}, {"",}, 
      {"@end", 		0,	st_C_objend},
      {"namespace", 	C_PLPL,	st_C_struct},
      {"",}, 
      {"domain",  	C_STAR,	st_C_struct},
      {"",}, {"",}, 
      {"@interface", 	0,	st_C_objprot},
      {"",}, {"",}, {"",}, 
      {"@implementation", 0,	st_C_objimpl},
      {"",}, {"",}, 
      {"double",   	0,	st_C_typespec},
      {"",}, {"",}, 
      {"PSEUDO", 		0,	st_C_gnumacro},
      {"",}, {"",}, {"",}, 
      {"SYSCALL", 	0,	st_C_gnumacro},
      {"",}, {"",}, 
      {"@protocol", 	0,	st_C_objprot},
      {"",}, {"",}, {"",}, 
      {"unsigned", 	0,	st_C_typespec},
      {"",}, 
      {"enum",     	0,	st_C_enum},
      {"",}, {"",}, 
      {"char",     	0,	st_C_typespec},
      {"class",   	C_PLPL,	st_C_class},
      {"struct",  	0,	st_C_struct},
      {"",}, {"",}, {"",}, {"",}, 
      {"mutable", 	C_PLPL,	st_C_typespec},
      {"void",     	0,	st_C_typespec},
      {"inline", 		0,	st_C_inline},
      {"ENTRY", 		0,	st_C_gnumacro},
      {"",}, 
      {"signed",   	0,	st_C_typespec},
      {"",}, {"",}, 
      {"package", 	C_JAVA,	st_C_ignore},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"static",   	0,	st_C_typespec},
      {"",}, 
      {"define",   	0,	st_C_define},
      {"",}, 
      {"union",   	0,	st_C_union},
      {"DEFUN", 		0,	st_C_gnumacro},
      {"",}, {"",}, {"",}, 
      {"extern",   	0,	st_C_extern},
      {"extends",   	C_JAVA,	st_C_javastruct},
      {"",}, {"",}, {"",}, 
      {"short",    	0,	st_C_typespec},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"explicit", 	C_PLPL,	st_C_typespec},
      {"auto",     	0,	st_C_typespec},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, 
      {"int",      	0,	st_C_typespec},
      {"",}, {"",}, 
      {"typedef",  	0,	st_C_typedef},
      {"typename", 	C_PLPL,	st_C_typespec},
      {"",}, 
      {"interface", 	C_JAVA, st_C_struct},
      {"",}, 
      {"bool", 		C_PLPL,	st_C_typespec},
      {"",}, {"",}, {"",}, 
      {"import", 		C_JAVA,	st_C_ignore},
      {"",}, 
      {"friend", 		C_PLPL,	st_C_ignore},
      {"float",    	0,	st_C_typespec},
      {"implements",   	C_JAVA,	st_C_javastruct},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strncmp (str + 1, s + 1, len - 1))
            return &wordlist[key];
        }
    }
  return 0;
}
/*%>*/

enum sym_type C_symtype PP ((char *str, int len, int c_ext));
enum sym_type
C_symtype (str, len, c_ext)
     char *str;
     int len;
     int c_ext;
{
  register struct C_stab_entry *se = in_word_set (str, len);

  if (se == NULL || (se->c_ext && !(c_ext & se->c_ext)))
    return st_none;
  return se->type;
}

 /*
  * C functions and variables are recognized using a simple
  * finite automaton.  fvdef is its state variable.
  */
enum
{
  fvnone,			/* nothing seen */
  fvnameseen,			/* function or variable name seen */
  fstartlist,			/* func: just after open parenthesis */
  finlist,			/* func: in parameter list */
  flistseen,			/* func: after parameter list */
  fignore,			/* func: before open brace */
  vignore			/* var-like: ignore until ';' */
} fvdef;


 /*
  * typedefs are recognized using a simple finite automaton.
  * typdef is its state variable.
  */
enum
{
  tnone,			/* nothing seen */
  ttypedseen,			/* typedef keyword seen */
  tinbody,			/* inside typedef body */
  tend,				/* just before typedef tag */
  tignore			/* junk after typedef tag */
} typdef;


 /*
  * struct-like structures (enum, struct and union) are recognized
  * using another simple finite automaton.  `structdef' is its state
  * variable.
  */
enum
{
  snone,			/* nothing seen yet */
  skeyseen,			/* struct-like keyword seen */
  stagseen,			/* struct-like tag seen */
  scolonseen,			/* colon seen after struct-like tag */
  sinbody			/* in struct body: recognize member func defs*/
} structdef;

/*
 * When structdef is stagseen, scolonseen, or sinbody, structtag is the
 * struct tag, and structtype is the type of the preceding struct-like
 * keyword.
 */
char *structtag = "<uninited>";
enum sym_type structtype;

#ifdef OO_BROWSER
void
oo_browser_check_and_clear_structtype(void)
{
  /* Allow for multiple enum_label tags. */
  if (structtype != st_C_enum)
    structtype = st_none;
}
#endif

/*
 * When objdef is different from onone, objtag is the name of the class.
 */
char *objtag = "<uninited>";

/*
 * Yet another little state machine to deal with preprocessor lines.
 */
enum
{
  dnone,			/* nothing seen */
  dsharpseen,			/* '#' seen as first char on line */
  ddefineseen,			/* '#' and 'define' seen */
  dignorerest			/* ignore rest of line */
} definedef;

/*
 * State machine for Objective C protocols and implementations.
 * Tom R.Hageman <tom@basil.icce.rug.nl>
 */
enum
{
  onone,			/* nothing seen */
  oprotocol,			/* @interface or @protocol seen */
  oimplementation,		/* @implementations seen */
  otagseen,			/* class name seen */
  oparenseen,			/* parenthesis before category seen */
  ocatseen,			/* category name seen */
  oinbody,			/* in @implementation body */
  omethodsign,			/* in @implementation body, after +/- */
  omethodtag,			/* after method name */
  omethodcolon,			/* after method colon */
  omethodparm,			/* after method parameter */
  oignore			/* wait for @end */
} objdef;

/*
 * Use this structure to keep info about the token read, and how it
 * should be tagged.  Used by the make_C_tag function to build a tag.
 */
typedef struct
{
  bool valid;
  char *str;
  bool named;
  int linelen;
  int lineno;
  long linepos;
  char *buffer;
} token;

token tok;			/* latest token read */

/*
 * Set this to TRUE, and the next token considered is called a function.
 * Used only for GNU emacs's function-defining macros.
 */
bool next_token_is_func;

/*
 * TRUE in the rules part of a yacc file, FALSE outside (parse as C).
 */
bool yacc_rules;

/*
 * methodlen is the length of the method name stored in token_name.
 */
int methodlen;

#ifdef OO_BROWSER
void
oo_browser_clear_all_globals(void)
{
  /* Initialize globals so there is no carry over between files. */
  oo_browser_construct = C_NULL;
  fvdef = fvnone; typdef = tnone; structdef = snone;
  definedef = dnone; objdef = onone;
  structtype = st_none;
  next_token_is_func = yacc_rules = FALSE;
}

void
oo_browser_clear_some_globals(void)
{
  oo_browser_construct = C_NULL;
  structtype = st_none;
}
#endif

/*
 * consider_token ()
 *	checks to see if the current token is at the start of a
 *	function or variable, or corresponds to a typedef, or
 * 	is a struct/union/enum tag, or #define, or an enum constant.
 *
 *	*IS_FUNC gets TRUE iff the token is a function or #define macro
 *	with args.  C_EXT is which language we are looking at.
 *
 *	In the future we will need some way to adjust where the end of
 *	the token is; for instance, implementing the C++ keyword
 *	`operator' properly will adjust the end of the token to be after
 *	whatever follows `operator'.
 *
 * Globals
 *	fvdef			IN OUT
 *	structdef		IN OUT
 *	definedef		IN OUT
 *	typdef			IN OUT
 *	objdef			IN OUT
 *	next_token_is_func	IN OUT
 */
bool consider_token PP ((char *str, int len, int c, int c_ext,
			 int cblev, int parlev, bool *is_func_or_var));
bool
consider_token (str, len, c, c_ext, cblev, parlev, is_func_or_var)
     register char *str;	/* IN: token pointer */
     register int len;		/* IN: token length */
     register int c;		/* IN: first char after the token */
     int c_ext;			/* IN: C extensions mask */
     int cblev;			/* IN: curly brace level */
     int parlev;		/* IN: parenthesis level */
     bool *is_func_or_var;	/* OUT: function or variable found */
{
  enum sym_type toktype = C_symtype (str, len, c_ext);

#ifdef OO_BROWSER
  switch (toktype)
    {
      case st_C_struct:
        set_construct(C_STRUCTURE);
        break;
      case st_C_union:
        set_construct(C_UNION);
        break;
      case st_C_class:
        set_construct(C_CLASS);
        break;
      case st_C_enum:
        set_construct(C_ENUMERATION);
        break;
      case st_C_typedef:
        set_construct(C_TYPE);
        break;
      case st_C_extern:
        set_construct(C_EXTERN);
        break;
      case st_C_inline:
        set_construct(C_FUNCTION);
        break;
    }
#endif

  /*
   * Advance the definedef state machine.
   */
  switch (definedef)
    {
    case dnone:
      /* We're not on a preprocessor line. */
      break;
    case dsharpseen:
      if (toktype == st_C_define)
	{
	  definedef = ddefineseen;
	}
      else
	{
	  definedef = dignorerest;
	}
      return FALSE;
    case ddefineseen:
      /*
       * Make a tag for any macro, unless it is a constant
       * and constantypedefs is FALSE.
       */
      definedef = dignorerest;
#ifndef OO_BROWSER
      *is_func_or_var = (c == '(');
#else
      {
        char *p = str + len * sizeof(char);

        if (*p == '(')
          /* This must be a macro since there is no
             whitespace between the opening parenthesis
             and the definition name. */
          *is_func_or_var = TRUE;
        else
          {
            *is_func_or_var = FALSE;

            /* Handle possible whitespace between macro tag and opening
               parenthesis and ensure this is an actual macro.
               -- Bob Weiner, Altrasoft, 11/19/1997 */
            while (*p && isspace(*p)) p++;
            if (*p) c = *p;

            /* Skip over nested parentheses. */
            if (c == '(')
              {
                short depth = 1;

                while (*++p && depth > 0 && *p != '\n')
                  {
                    switch (*p)
                      {
                      case '(':
                        depth++; break;
                      case ')':
                        depth--; break;
                      }
                  }

                /* If this is a macro, we have just passed
                   the arguments and there will be more on
                   the line before the NULL character that marks
                   the end of the line token. */
                while (*p == ' ' || *p == '\t') p++;
                if (*p) *is_func_or_var = TRUE;
              }
          }
      }

      set_construct((*is_func_or_var) ? C_MACRO : C_CONSTANT);
#endif
      if (!*is_func_or_var && !constantypedefs)
	return FALSE;
      else
	return TRUE;
    case dignorerest:
      return FALSE;
    default:
      error ("internal error: definedef value.", (char *)NULL);
    }

  /*
   * Now typedefs
   */
  switch (typdef)
    {
    case tnone:
      if (toktype == st_C_typedef)
	{
	  if (typedefs)
	    typdef = ttypedseen;
	  fvdef = fvnone;
	  return FALSE;
	}
      break;
    case ttypedseen:
      switch (toktype)
	{
	case st_C_const:
          set_construct(C_CONSTANT);
          /* fall through */
	case st_none:
	case st_C_typespec:
#ifdef OO_BROWSER
	case st_C_extern:
#endif
	  typdef = tend;
	  break;
	case st_C_struct:
	case st_C_enum:
#ifdef OO_BROWSER
	case st_C_union:
	case st_C_class:
#endif
	  break;
	}
      /* Do not return here, so the structdef stuff has a chance. */
      break;
    case tend:
      switch (toktype)
	{
	case st_C_const:
          set_construct(C_CONSTANT);
          /* fall through */
	case st_C_typespec:
	case st_C_struct:
	case st_C_enum:
#ifdef OO_BROWSER
	case st_C_extern:
	case st_C_union:
	case st_C_class:
#endif
	  return FALSE;
	}
      return TRUE;
    }

  /*
   * This structdef business is currently only invoked when cblev==0.
   * It should be recursively invoked whatever the curly brace level,
   * and a stack of states kept, to allow for definitions of structs
   * within structs.
   *
   * This structdef business is NOT invoked when we are ctags and the
   * file is plain C.  This is because a struct tag may have the same
   * name as another tag, and this loses with ctags.
   */
  switch (toktype)
    {
    case st_C_javastruct:
      if (structdef == stagseen)
        structdef = scolonseen;
      return FALSE;
    case st_C_struct:
    case st_C_enum:
#ifdef OO_BROWSER
    case st_C_union:
    case st_C_class:
    case st_C_extern:
#endif
      if (typdef == ttypedseen
	  || (typedefs_and_cplusplus && cblev == 0 && structdef == snone))
	{
	  structdef = skeyseen;
	  structtype = toktype;
	}
      return FALSE;
    }

  if (structdef == skeyseen)
    {
      /* Save the tag for struct/union/class, for functions and variables
	 that may be defined inside. */
#ifndef OO_BROWSER
      if (structtype == st_C_struct)
#else
      if (structtype == st_C_struct
	  || structtype == st_C_union
	  || structtype == st_C_class)
#endif
	structtag = savenstr (str, len);
      else
	structtag = "<enum>";
      structdef = stagseen;
      return TRUE;
    }

  /* Avoid entering fvdef stuff if typdef is going on. */
  if (typdef != tnone)
    {
      definedef = dnone;
      return FALSE;
    }

  /* Detect GNU macros.

     DEFUN note for writers of emacs C code:
      The DEFUN macro, used in emacs C source code, has a first arg
     that is a string (the lisp function name), and a second arg that
     is a C function name.  Since etags skips strings, the second arg
     is tagged.  This is unfortunate, as it would be better to tag the
     first arg.  The simplest way to deal with this problem would be
     to name the tag with a name built from the function name, by
     removing the initial 'F' character and substituting '-' for '_'.
     Anyway, this assumes that the conventions of naming lisp
     functions will never change.  Currently, this method is not
     implemented, so writers of emacs code are recommended to put the
     first two args of a DEFUN on the same line. */
  if (definedef == dnone && toktype == st_C_gnumacro)
    {
      next_token_is_func = TRUE;
      return FALSE;
    }
  if (next_token_is_func)
    {
      next_token_is_func = FALSE;
      fvdef = fignore;
      *is_func_or_var = TRUE;
      return TRUE;
    }

  /* Detect Objective C constructs. */
  switch (objdef)
    {
    case onone:
      switch (toktype)
	{
	case st_C_objprot:
#ifdef OO_BROWSER
	  set_construct(C_OBJC);
#endif
	  objdef = oprotocol;
	  return FALSE;
	case st_C_objimpl:
#ifdef OO_BROWSER
	  set_construct(C_OBJC);
#endif
	  objdef = oimplementation;
	  return FALSE;
	}
      break;
    case oimplementation:
      /* Save the class tag for functions or variables defined inside. */
      objtag = savenstr (str, len);
      objdef = oinbody;
      return FALSE;
    case oprotocol:
      /* Save the class tag for categories. */
      objtag = savenstr (str, len);
      objdef = otagseen;
      *is_func_or_var = TRUE;
      return TRUE;
    case oparenseen:
      objdef = ocatseen;
      *is_func_or_var = TRUE;
      return TRUE;
    case oinbody:
      break;
    case omethodsign:
      if (parlev == 0)
	{
	  objdef = omethodtag;
	  methodlen = len;
	  grow_linebuffer (&token_name, methodlen + 1);
	  strncpy (token_name.buffer, str, len);
	  token_name.buffer[methodlen] = '\0';
	  token_name.len = methodlen;
	  return TRUE;
	}
      return FALSE;
    case omethodcolon:
      if (parlev == 0)
	objdef = omethodparm;
      return FALSE;
    case omethodparm:
      if (parlev == 0)
	{
	  objdef = omethodtag;
	  methodlen += len;
	  grow_linebuffer (&token_name, methodlen + 1);
	  strncat (token_name.buffer, str, len);
	  token_name.len = methodlen;
	  return TRUE;
	}
      return FALSE;
    case oignore:
      if (toktype == st_C_objend)
	{
	  /* Memory leakage here: the string pointed by objtag is
	     never released, because many tests would be needed to
	     avoid breaking on incorrect input code.  The amount of
	     memory leaked here is the sum of the lengths of the
	     class tags.
	  free (objtag); */
	  objdef = onone;
	}
      return FALSE;
    }

  /* A function, variable or enum constant? */
  switch (toktype)
    {
    case st_C_const:
      set_construct(C_CONSTANT);
      /* fall through */
    case st_C_typespec:
#ifdef OO_BROWSER
    case st_C_extern:
#endif
      if (fvdef != finlist && fvdef != fignore  && fvdef != vignore)
        fvdef = fvnone;		/* should be useless */
      return FALSE;
    case st_C_ignore:
      fvdef = vignore;
      return FALSE;
    case st_none:
      if (constantypedefs && structdef == sinbody && structtype == st_C_enum)
#ifdef OO_BROWSER
        {
	  oo_browser_construct = C_ENUM_LABEL;
#endif
	return TRUE;
#ifdef OO_BROWSER
	}
#endif
      if (fvdef == fvnone)
	{
	  fvdef = fvnameseen;	/* function or variable */
	  *is_func_or_var = TRUE;
	  return TRUE;
	}
    }

  return FALSE;
}

/*
 * C_entries ()
 *	This routine finds functions, variables, typedefs,
 * 	#define's, enum constants and struct/union/enum definitions in
 * 	#C syntax and adds them to the list.
 */
#define current_lb_is_new (newndx == curndx)
#define switch_line_buffers() (curndx = 1 - curndx)

#define curlb (lbs[curndx].lb)
#define othlb (lbs[1-curndx].lb)
#define newlb (lbs[newndx].lb)
#define curlinepos (lbs[curndx].linepos)
#define othlinepos (lbs[1-curndx].linepos)
#define newlinepos (lbs[newndx].linepos)

#define CNL_SAVE_DEFINEDEF()						\
do {									\
  curlinepos = charno;							\
  lineno++;								\
  linecharno = charno;							\
  charno += readline (&curlb, inf);					\
  lp = curlb.buffer;							\
  quotednl = FALSE;							\
  newndx = curndx;							\
} while (0)

#define CNL()								\
do {									\
  CNL_SAVE_DEFINEDEF();							\
  if (savetok.valid)							\
    {									\
      tok = savetok;							\
      savetok.valid = FALSE;						\
    }									\
  definedef = dnone;							\
} while (0)


void make_C_tag PP ((bool isfun));
void
make_C_tag (isfun)
     bool isfun;
{
  /* This function should never be called when tok.valid is FALSE, but
     we must protect against invalid input or internal errors. */
  if (tok.valid)
    {
      if (traditional_tag_style)
	{
	  /* This was the original code.  Now we call new_pfnote instead,
	     which uses the new method for naming tags (see new_pfnote). */
	  char *name = NULL;

	  if (CTAGS || tok.named)
	    name = savestr (token_name.buffer);
	  pfnote (name, isfun,
		  tok.buffer, tok.linelen, tok.lineno, tok.linepos);
	}
      else
	new_pfnote (token_name.buffer, token_name.len, isfun,
		    tok.buffer, tok.linelen, tok.lineno, tok.linepos);
      tok.valid = FALSE;
    }
  else if (DEBUG)
    abort ();
}


void
C_entries (c_ext, inf)
     int c_ext;			/* extension of C */
     FILE *inf;			/* input file */
{
  register char c;		/* latest char read; '\0' for end of line */
  register char *lp;		/* pointer one beyond the character `c' */
  int curndx, newndx;		/* indices for current and new lb */
  register int tokoff;		/* offset in line of start of current token */
  register int toklen;		/* length of current token */
  char *qualifier;		/* string used to qualify names */
  int qlen;			/* length of qualifier */
  int cblev;			/* current curly brace level */
  int parlev;			/* current parenthesis level */
  bool incomm, inquote, inchar, quotednl, midtoken;
  bool cplpl, cjava;
  token savetok;		/* token saved during preprocessor handling */


  tokoff = toklen = 0;		/* keep compiler quiet */
  curndx = newndx = 0;
  lineno = 0;
  charno = 0;
  lp = curlb.buffer;
  *lp = 0;

  fvdef = fvnone; typdef = tnone; structdef = snone;
  definedef = dnone; objdef = onone;
  next_token_is_func = yacc_rules = FALSE;
  midtoken = inquote = inchar = incomm = quotednl = FALSE;
  tok.valid = savetok.valid = FALSE;
  cblev = 0;
  parlev = 0;
  cplpl = (c_ext & C_PLPL) == C_PLPL;
  cjava = (c_ext & C_JAVA) == C_JAVA;
  if (cjava)
    { qualifier = "."; qlen = 1; }
  else
    { qualifier = "::"; qlen = 2; }

  while (!feof (inf))
    {
      c = *lp++;
      if (c == '\\')
	{
	  /* If we're at the end of the line, the next character is a
	     '\0'; don't skip it, because it's the thing that tells us
	     to read the next line.  */
	  if (*lp == '\0')
	    {
	      quotednl = TRUE;
	      continue;
	    }
	  lp++;
	  c = ' ';
	}
      else if (incomm)
	{
	  switch (c)
	    {
	    case '*':
	      if (*lp == '/')
		{
		  c = *lp++;
		  incomm = FALSE;
		}
	      break;
	    case '\0':
	      /* Newlines inside comments do not end macro definitions in
		 traditional cpp. */
	      CNL_SAVE_DEFINEDEF ();
	      break;
	    }
	  continue;
	}
      else if (inquote)
	{
	  switch (c)
	    {
	    case '"':
	      inquote = FALSE;
	      break;
	    case '\0':
	      /* Newlines inside strings do not end macro definitions
		 in traditional cpp, even though compilers don't
		 usually accept them. */
	      CNL_SAVE_DEFINEDEF ();
	      break;
	    }
	  continue;
	}
      else if (inchar)
	{
	  switch (c)
	    {
	    case '\0':
	      /* Hmmm, something went wrong. */
	      CNL ();
	      /* FALLTHRU */
	    case '\'':
	      inchar = FALSE;
	      break;
	    }
	  continue;
	}
      else
	switch (c)
	  {
	  case '"':
	    inquote = TRUE;
	    if (fvdef != finlist && fvdef != fignore && fvdef !=vignore)
	      fvdef = fvnone;
	    continue;
	  case '\'':
	    inchar = TRUE;
	    if (fvdef != finlist && fvdef != fignore && fvdef !=vignore)
	      fvdef = fvnone;
	    continue;
	  case '/':
	    if (*lp == '*')
	      {
		lp++;
		incomm = TRUE;
		continue;
	      }
	    else if (/* cplpl && */ *lp == '/')
	      {
		c = '\0';
		break;
	      }
	    else
	      break;
	  case '%':
	    if ((c_ext & YACC) && *lp == '%')
	      {
		/* entering or exiting rules section in yacc file */
		lp++;
		definedef = dnone; fvdef = fvnone;
		typdef = tnone; structdef = snone;
		next_token_is_func = FALSE;
		midtoken = inquote = inchar = incomm = quotednl = FALSE;
		cblev = 0;
		yacc_rules = !yacc_rules;
		continue;
 	      }
	    else
	      break;
	  case '#':
	    if (definedef == dnone)
	      {
		char *cp;
		bool cpptoken = TRUE;

		/* Look back on this line.  If all blanks, or nonblanks
		   followed by an end of comment, this is a preprocessor
		   token. */
		for (cp = newlb.buffer; cp < lp-1; cp++)
		  if (!iswhite (*cp))
		    {
		      if (*cp == '*' && *(cp+1) == '/')
			{
			  cp++;
			  cpptoken = TRUE;
			}
		      else
			cpptoken = FALSE;
		    }
		if (cpptoken)
		  definedef = dsharpseen;
	      } /* if (definedef == dnone) */

	    continue;
	  } /* switch (c) */


      /* Consider token only if some complicated conditions are satisfied. */
      if ((definedef != dnone
	   || (cblev == 0 && structdef != scolonseen)
	   || (cblev == 1 && cplpl && structdef == sinbody)
	   || (structdef == sinbody && structtype == st_C_enum))
	  && typdef != tignore
	  && definedef != dignorerest
	  && fvdef != finlist)
	{
	  if (midtoken)
	    {
	      if (endtoken (c))
		{
		  if (c == ':' && cplpl && *lp == ':' && begtoken(*(lp + 1)))
		    {
		      /*
		       * This handles :: in the middle, but not at the
		       * beginning of an identifier.
		       */
		      lp += 2;
		      toklen += 3;
#ifdef OO_BROWSER
		      set_construct(C_METHOD);
#endif
		    }
		  else
		    {
		      bool funorvar = FALSE;

		      if (yacc_rules
			  || consider_token (newlb.buffer + tokoff, toklen, c,
					     c_ext, cblev, parlev, &funorvar))
			{
			  tok.named = FALSE;
			  if (structdef == sinbody
			      && definedef == dnone
			      && funorvar)
			    /* function or var defined in C++ class body */
			    {
			      int len = strlen (structtag) + qlen + toklen;
			      grow_linebuffer (&token_name, len + 1);
			      strcpy (token_name.buffer, structtag);
			      strcat (token_name.buffer, qualifier);
			      strncat (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      token_name.len = len;
			      tok.named = TRUE;
#ifdef OO_BROWSER
			      oo_browser_construct = C_METHOD;
#endif
			    }
			  else if (objdef == ocatseen)
			    /* Objective C category */
			    {
			      int len = strlen (objtag) + 2 + toklen;
			      grow_linebuffer (&token_name, len + 1);
			      strcpy (token_name.buffer, objtag);
			      strcat (token_name.buffer, "(");
			      strncat (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      strcat (token_name.buffer, ")");
			      token_name.len = len;
			      tok.named = TRUE;
#ifdef OO_BROWSER
			      oo_browser_construct = C_OBJC;
#endif
			    }
			  else if (objdef == omethodtag
				   || objdef == omethodparm)
			    /* Objective C method */
			    {
			      tok.named = TRUE;
#ifdef OO_BROWSER
			      oo_browser_construct = C_OBJC;
#endif
			    }
			  else
			    {
			      grow_linebuffer (&token_name, toklen + 1);
			      strncpy (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      token_name.buffer[toklen] = '\0';
			      token_name.len = toklen;
			      /* Name macros. */
			      tok.named
                                = (structdef == stagseen
                                   || typdef == tend
#ifdef OO_BROWSER
                                   /* Also name #define constants,
                                      enumerations and enum_labels.
                                      Conditionalize `funorvar' reference
                                      here or #defines will appear without
                                      their #names.
                                      -- Bob Weiner, Altrasoft, 4/25/1998 */
                                   || ((oo_browser_format || funorvar)
                                       && definedef == dignorerest)
                                   || (oo_browser_format
                                       && (oo_browser_construct == C_ENUMERATION
                                           || oo_browser_construct == C_ENUM_LABEL))
#else
                                   || (funorvar
                                       && definedef == dignorerest)
#endif
                                   );
			    }
			  tok.lineno = lineno;
			  tok.linelen = tokoff + toklen + 1;
			  tok.buffer = newlb.buffer;
			  tok.linepos = newlinepos;
			  tok.valid = TRUE;

			  if (definedef == dnone
			      && (fvdef == fvnameseen
				  || structdef == stagseen
				  || typdef == tend
				  || objdef != onone))
			    {
			      if (current_lb_is_new)
				switch_line_buffers ();
			    }
			  else
			    make_C_tag (funorvar);
			}
		      midtoken = FALSE;
		    }
		} /* if (endtoken (c)) */
	      else if (intoken (c))
		{
		  toklen++;
		  continue;
		}
	    } /* if (midtoken) */
	  else if (begtoken (c))
	    {
	      switch (definedef)
		{
		case dnone:
		  switch (fvdef)
		    {
		    case fstartlist:
		      fvdef = finlist;
		      continue;
		    case flistseen:
#ifdef OO_BROWSER
		      set_construct(C_MACRO);
#endif
		      make_C_tag (TRUE); /* a function */
		      fvdef = fignore;
		      break;
		    case fvnameseen:
		      fvdef = fvnone;
		      break;
		    }
		  if (structdef == stagseen && !cjava)
		    structdef = snone;
		  break;
		case dsharpseen:
		  savetok = tok;
		}
	      if (!yacc_rules || lp == newlb.buffer + 1)
		{
		  tokoff = lp - 1 - newlb.buffer;
		  toklen = 1;
		  midtoken = TRUE;
		}
	      continue;
	    } /* if (begtoken) */
	} /* if must look at token */


      /* Detect end of line, colon, comma, semicolon and various braces
	 after having handled a token.*/
      switch (c)
	{
	case ':':
	  if (definedef != dnone)
	    break;
	  switch (objdef)
	    {
	    case  otagseen:
	      objdef = oignore;
	      make_C_tag (TRUE); /* an Objective C class */
	      break;
	    case omethodtag:
	    case omethodparm:
	      objdef = omethodcolon;
	      methodlen += 1;
	      grow_linebuffer (&token_name, methodlen + 1);
	      strcat (token_name.buffer, ":");
	      token_name.len = methodlen;
	      break;
	    }
	  if (structdef == stagseen)
	    structdef = scolonseen;
	  else
	    switch (fvdef)
	      {
	      case fvnameseen:
		if (yacc_rules)
		  {
		    make_C_tag (FALSE); /* a yacc function */
		    fvdef = fignore;
		  }
		break;
	      case fstartlist:
		fvdef = fvnone;
		break;
	      }
	  break;
	case ';':
	  if (definedef != dnone)
	    break;
	  if (cblev == 0)
	    switch (typdef)
	      {
	      case tend:
#ifdef OO_BROWSER
		set_construct(C_TYPE);
#endif
		make_C_tag (FALSE); /* a typedef */
		/* FALLTHRU */
	      default:
		typdef = tnone;
	      }
	  switch (fvdef)
	    {
	    case fignore:
	      break;
	    case fvnameseen:
	      if ((globals && cblev == 0) || (members && cblev == 1))
#ifndef OO_BROWSER
		make_C_tag (FALSE); /* a variable */
#else
/*	      if (constantypedefs && structdef == snone)*/
		{
		  tok.named = TRUE;
		  switch (structtype)
		    {
		      case st_C_enum:
			set_construct(C_ENUMERATION);
			break;
		      case st_C_class:
			set_construct(C_CLASS);
			break;
		      default:
			set_construct(C_VARIABLE);
			break;
		    }
		  make_C_tag (FALSE);
		  /* Force reset of st_C_enum structtype value. */
		  structtype = st_none;
		}
#endif
	      /* FALLTHRU */
	    default:
	      fvdef = fvnone;
	      /* The following instruction invalidates the token.
		 Probably the token should be invalidated in all
		 other cases  where some state machine is reset. */
	      tok.valid = FALSE;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case ',':
	  if (definedef != dnone)
	    break;
	  switch (objdef)
	    {
	    case omethodtag:
	    case omethodparm:
	      make_C_tag (TRUE); /* an Objective C method */
	      objdef = oinbody;
	      break;
	    }
	  switch (fvdef)
	    {
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fvnameseen:
	      if ((globals && cblev == 0) || (members && cblev == 1))
		make_C_tag (FALSE); /* a variable */
	      break;
	    default:
	      fvdef = fvnone;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case '[':
	  if (definedef != dnone)
	    break;
	  if (cblev == 0 && typdef == tend)
	    {
#ifdef OO_BROWSER
	      set_construct(C_TYPE);
#endif
	      typdef = tignore;
	      make_C_tag (FALSE);	/* a typedef */
	      break;
	    }
	  switch (fvdef)
	    {
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fvnameseen:
#ifndef OO_BROWSER
	      if ((globals && cblev == 0) || (members && cblev == 1))
		make_C_tag (FALSE); /* a variable */
#else
	      if (constantypedefs && structdef == snone)
	        {
		  tok.named = TRUE;
		  switch (structtype)
		    {
		      case st_C_enum:
			set_construct(C_ENUMERATION);
			break;
		      case st_C_class:
			set_construct(C_CLASS);
			break;
		      default:
			set_construct(C_VARIABLE);
			break;
		    }
		  make_C_tag (FALSE);
		  /* Force reset of st_C_enum structtype value. */
		  structtype = st_none;
		}
#endif
	      /* FALLTHRU */
	    default:
	      fvdef = fvnone;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case '(':
	  if (definedef != dnone)
	    break;
	  if (objdef == otagseen && parlev == 0)
	    objdef = oparenseen;
	  switch (fvdef)
	    {
	    case fvnone:
	      switch (typdef)
		{
		case ttypedseen:
		case tend:
		  if (tok.valid && *lp != '*')
		    {
		      /* This handles constructs like:
			 typedef void OperatorFun (int fun); */
		      typdef = tignore;
#ifdef OO_BROWSER
		      set_construct(C_TYPE);
#endif
		      make_C_tag (FALSE);
		    }
		  break;
		} /* switch (typdef) */
	      break;
	    case fvnameseen:
	      fvdef = fstartlist;
	      break;
	    case flistseen:
	      fvdef = finlist;
	      break;
	    }
	  parlev++;
	  break;
	case ')':
	  if (definedef != dnone)
	    break;
	  if (objdef == ocatseen && parlev == 1)
	    {
	      make_C_tag (TRUE); /* an Objective C category */
	      objdef = oignore;
	    }
	  if (--parlev == 0)
	    {
	      switch (fvdef)
		{
		case fstartlist:
		case finlist:
		  fvdef = flistseen;
		  break;
		}
	      if (cblev == 0 && typdef == tend)
		{
#ifdef OO_BROWSER
		  set_construct(C_TYPE);
#endif
		  typdef = tignore;
		  make_C_tag (FALSE); /* a typedef */
		}
	    }
	  else if (parlev < 0)	/* can happen due to ill-conceived #if's. */
	    parlev = 0;
	  break;
	case '{':
	  if (definedef != dnone)
	    break;
	  if (typdef == ttypedseen)
	    typdef = tinbody;
	  switch (structdef)
	    {
	    case skeyseen:	/* unnamed struct */
	      structdef = sinbody;
	      structtag = "_anonymous_";
	      break;
	    case stagseen:
	    case scolonseen:	/* named struct */
	      structdef = sinbody;
	      make_C_tag (FALSE);	/* a struct */
	      break;
	    }
	  switch (fvdef)
	    {
	    case flistseen:
#ifdef OO_BROWSER
	      set_construct(C_FUNCTION);
	      /* Ensure function name is recorded.
		 -- Bob Weiner, Altrasoft */
	      tok.named = TRUE;
#endif
	      make_C_tag (TRUE); /* a function */
	      /* FALLTHRU */
	    case fignore:
	      fvdef = fvnone;
	      break;
	    case fvnone:
	      switch (objdef)
		{
		case otagseen:
		  make_C_tag (TRUE); /* an Objective C class */
		  objdef = oignore;
		  break;
		case omethodtag:
		case omethodparm:
		  make_C_tag (TRUE); /* an Objective C method */
		  objdef = oinbody;
		  break;
		default:
		  /* Neutralize `extern "C" {' grot. */
		  if (cblev == 0 && structdef == snone && typdef == tnone)
		    cblev = -1;
		}
	    }
	  cblev++;
	  break;
	case '*':
	  if (definedef != dnone)
	    break;
	  if (fvdef == fstartlist)
	    fvdef = fvnone;	/* avoid tagging `foo' in `foo (*bar()) ()' */
	  break;
	case '}':
	  if (definedef != dnone)
	    break;
	  if (!noindentypedefs && lp == newlb.buffer + 1)
	    {
	      cblev = 0;	/* reset curly brace level if first column */
	      parlev = 0;	/* also reset paren level, just in case... */
	    }
	  else if (cblev > 0)
	    cblev--;
	  if (cblev == 0)
	    {
	      if (typdef == tinbody)
		typdef = tend;
	      /* Memory leakage here: the string pointed by structtag is
	         never released, because I fear to miss something and
	         break things while freeing the area.  The amount of
	         memory leaked here is the sum of the lengths of the
	         struct tags.
	      if (structdef == sinbody)
		free (structtag); */

	      structdef = snone;
	      structtag = "<error>";
#ifdef OO_BROWSER
	      /* Next line added to avoid any state carryover between
		 functions. -- Bob Weiner, Altrasoft, 11/19/1997 */
	      fvdef = fvnone; oo_browser_construct = C_NULL;
#endif
	    }
	  break;
	case '=':
	  if (definedef != dnone)
	    break;
#ifdef OO_BROWSER
	  {
	    int is_method = 0;
#endif
	  switch (fvdef)
	    {
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fvnameseen:
	      if ((globals && cblev == 0) || (members && cblev == 1))
#ifndef OO_BROWSER
		make_C_tag (FALSE); /* a variable */
#else
		{
		  tok.named = TRUE;
                  switch (structtype)
                    {
                      case st_C_enum:
                        set_construct(C_ENUMERATION);
                        break;
                      case st_C_class:
                        set_construct(C_CLASS);
                        break;
                      default:
                        /* a global variable */
                        set_construct(C_VARIABLE);
                        break;
                    }

                  /* ootags categorizes each tag found whereas etags doesn't.
                     Set the is_method flag if this tag has been marked as
                     such by an earlier section of code.
                     -- Steve Baur, Altrasoft, 5/7/1998 */
		  is_method = (oo_browser_construct == C_METHOD);

		  make_C_tag (FALSE);
                  /* Force reset of st_C_enum structtype value. */
                  structtype = st_none;
		}
#endif
	      /* FALLTHRU */
	    default:
#ifdef OO_BROWSER
	      fvdef = is_method ? fignore : vignore;
#else
	      fvdef = vignore;
#endif
	    }
#ifdef OO_BROWSER
	  }
#endif
	  break;
	case '+':
	case '-':
	  if (objdef == oinbody && cblev == 0)
	    {
	      objdef = omethodsign;
	      break;
	    }
	  /* FALLTHRU */
	case '#': case '~': case '&': case '%': case '/': case '|':
	case '^': case '!': case '<': case '>': case '.': case '?': case ']':
	  if (definedef != dnone)
	    break;
#ifdef OO_BROWSER
	  if (!cplpl)
	    {
#endif
              /* The above characters cannot follow a function tag in C, so
                 unmark this as a function entry.  For C++, these characters
                 may follow an `operator' function construct, so skip the
                 unmarking conditional below.
                 -- Steve Baur, Altrasoft, 5/7/1998 */
              if (fvdef != finlist && fvdef != fignore && fvdef != vignore)
                fvdef = fvnone;
#ifdef OO_BROWSER
	    }
#endif
	  break;
	case '\0':
	  if (objdef == otagseen)
	    {
	      make_C_tag (TRUE); /* an Objective C class */
	      objdef = oignore;
	    }
	  /* If a macro spans multiple lines don't reset its state. */
	  if (quotednl)
	    CNL_SAVE_DEFINEDEF ();
	  else
	    CNL ();
	  break;
	} /* switch (c) */

    } /* while not eof */
}

/*
 * Process either a C++ file or a C file depending on the setting
 * of a global flag.
 */
void
default_C_entries (inf)
     FILE *inf;
{
  C_entries (cplusplus ? C_PLPL : 0, inf);
}

/* Always do plain ANSI C. */
void
plain_C_entries (inf)
     FILE *inf;
{
  C_entries (0, inf);
}

/* Always do C++. */
void
Cplusplus_entries (inf)
     FILE *inf;
{
  C_entries (C_PLPL, inf);
}

/* Always do Java. */
void
Cjava_entries (inf)
     FILE *inf;
{
  C_entries (C_JAVA, inf);
}

/* Always do C*. */
void
Cstar_entries (inf)
     FILE *inf;
{
  C_entries (C_STAR, inf);
}

/* Always do Yacc. */
void
Yacc_entries (inf)
     FILE *inf;
{
  C_entries (YACC, inf);
}

/* A useful macro. */  
#define LOOP_ON_INPUT_LINES(file_pointer, line_buffer, char_pointer)	\
  for (lineno = charno = 0;	/* loop initialization */		\
       !feof (file_pointer)	/* loop test */				\
       && (lineno++,		/* instructions at start of loop */	\
	   linecharno = charno,						\
	   charno += readline (&line_buffer, file_pointer),		\
	   char_pointer = lb.buffer,					\
	   TRUE);							\
      )


/*
 * Read a file, but do no processing.  This is used to do regexp
 * matching on files that have no language defined.
 */
void
just_read_file (inf)
     FILE *inf;
{
  register char *dummy;

  LOOP_ON_INPUT_LINES (inf, lb, dummy)
    continue;
}

/* Fortran parsing */

bool tail PP ((char *cp));
bool
tail (cp)
     char *cp;
{
  register int len = 0;

  while (*cp && lowcase(*cp) == lowcase(dbp[len]))
    cp++, len++;
  if (*cp == '\0' && !intoken(dbp[len]))
    {
      dbp += len;
      return TRUE;
    }
  return FALSE;
}

void
takeprec ()
{
  dbp = skip_spaces (dbp);
  if (*dbp != '*')
    return;
  dbp++;
  dbp = skip_spaces (dbp);
  if (strneq (dbp, "(*)", 3))
    {
      dbp += 3;
      return;
    }
  if (!isdigit (*dbp))
    {
      --dbp;			/* force failure */
      return;
    }
  do
    dbp++;
  while (isdigit (*dbp));
}

void getit PP ((FILE *inf));
void
getit (inf)
     FILE *inf;
{
  register char *cp;

  dbp = skip_spaces (dbp);
  if (*dbp == '\0')
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      if (dbp[5] != '&')
	return;
      dbp += 6;
      dbp = skip_spaces (dbp);
    }
  if (!isalpha (*dbp)
      && *dbp != '_'
      && *dbp != '$')
    return;
  for (cp = dbp + 1; *cp && intoken (*cp); cp++)
    continue;
  pfnote ((CTAGS) ? savenstr (dbp, cp-dbp) : NULL, TRUE,
	  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
}


void
Fortran_functions (inf)
     FILE *inf;
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (*dbp == '%')
	dbp++;			/* Ratfor escape to fortran */
      dbp = skip_spaces (dbp);
      if (*dbp == '\0')
	continue;
      switch (lowcase (*dbp))
	{
	case 'i':
	  if (tail ("integer"))
	    takeprec ();
	  break;
	case 'r':
	  if (tail ("real"))
	    takeprec ();
	  break;
	case 'l':
	  if (tail ("logical"))
	    takeprec ();
	  break;
	case 'c':
	  if (tail ("complex") || tail ("character"))
	    takeprec ();
	  break;
	case 'd':
	  if (tail ("double"))
	    {
	      dbp = skip_spaces (dbp);
	      if (*dbp == '\0')
		continue;
	      if (tail ("precision"))
		break;
	      continue;
	    }
	  break;
	}
      dbp = skip_spaces (dbp);
      if (*dbp == '\0')
	continue;
      switch (lowcase (*dbp))
	{
	case 'f':
	  if (tail ("function"))
	    getit (inf);
	  continue;
	case 's':
	  if (tail ("subroutine"))
	    getit (inf);
	  continue;
	case 'e':
	  if (tail ("entry"))
	    getit (inf);
	  continue;
	case 'p':
	  if (tail ("program"))
	    {
	      getit (inf);
	      continue;
	    }
	  if (tail ("procedure"))
	    getit (inf);
	  continue;
	}
    }
}

/*
 * Bob Weiner, Motorola Inc., 4/3/94
 * Unix and microcontroller assembly tag handling
 * look for '^[a-zA-Z_.$][a-zA_Z0-9_.$]*[: ^I^J]'
 */
void
Asm_labels (FILE *inf)
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      /* If first char is alphabetic or one of [_.$], test for colon
	 following identifier. */
      if (isalpha (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	{
 	  /* Read past label. */
	  cp++;
 	  while (isalnum (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	    cp++;
 	  if (*cp == ':' || isspace (*cp))
 	    {
 	      /* Found end of label, so copy it and add it to the table. */
 	      pfnote ((CTAGS) ? savenstr(lb.buffer, cp-lb.buffer) : NULL, TRUE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
 	    }
 	}
    }
}

/*
 * Perl support by Bart Robinson <lomew@cs.utah.edu>
 *              enhanced by Michael Ernst <mernst@alum.mit.edu>
 * Perl sub names: look for /^sub[ \t\n]+[^ \t\n{]+/
 * Perl variable names: /^(my|local).../
 */
void
Perl_functions (inf)
     FILE *inf;
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (*cp++ == 's'
	  && *cp++ == 'u'
	  && *cp++ == 'b' && isspace (*cp++))
	{
	  cp = skip_spaces (cp);
 	  if (*cp != '\0')
 	    {
 	      while (*cp != '\0'
		     && !isspace (*cp) && *cp != '{' && *cp != '(')
		cp++;
	      pfnote ((CTAGS) ? savenstr(lb.buffer, cp-lb.buffer) : NULL, TRUE,
 		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
 	    }	      
 	}
       else if (globals		/* only if tagging global vars is enabled */
		&& ((cp = lb.buffer,
		     *cp++ == 'm'
		     && *cp++ == 'y')
		    || (cp = lb.buffer,
			*cp++ == 'l'
			&& *cp++ == 'o'
			&& *cp++ == 'c'
			&& *cp++ == 'a'
			&& *cp++ == 'l'))
		&& (*cp == '(' || isspace (*cp)))
 	{
 	  /* After "my" or "local", but before any following paren or space. */
 	  char *varname = NULL;

 	  cp = skip_spaces (cp);
 	  if (*cp == '$' || *cp == '@' || *cp == '%')
 	    {
 	      char* varstart = ++cp;
 	      while (isalnum (*cp) || *cp == '_')
 		cp++;
 	      varname = savenstr (varstart, cp-varstart);
 	    }
 	  else
 	    {
 	      /* Should be examining a variable list at this point;
 		 could insist on seeing an open parenthesis. */
 	      while (*cp != '\0' && *cp != ';' && *cp != '=' &&  *cp != ')')
 		cp++;
 	    }
 
 	  /* Perhaps I should back cp up one character, so the TAGS table
 	     doesn't mention (and so depend upon) the following char. */
 	  pfnote ((CTAGS) ? savenstr (lb.buffer, cp-lb.buffer) : varname,
 		  FALSE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}

/*
 * Python support by Eric S. Raymond <esr@thyrsus.com>
 * Look for /^def[ \t\n]+[^ \t\n(:]+/ or /^class[ \t\n]+[^ \t\n(:]+/
 */
void
Python_functions (inf)
     FILE *inf;
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (*cp++ == 'd'
	  && *cp++ == 'e'
	  && *cp++ == 'f' && isspace (*cp++))
	{
	  cp = skip_spaces (cp);
	  while (*cp != '\0' && !isspace (*cp) && *cp != '(' && *cp != ':')
	    cp++;
	  pfnote ((char *) NULL, TRUE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}

      cp = lb.buffer;
      if (*cp++ == 'c'
	  && *cp++ == 'l'
	  && *cp++ == 'a'
	  && *cp++ == 's'
	  && *cp++ == 's' && isspace (*cp++))
	{
	  cp = skip_spaces (cp);
	  while (*cp != '\0' && !isspace (*cp) && *cp != '(' && *cp != ':')
	    cp++;
	  pfnote ((char *) NULL, TRUE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}

/* Idea by Corny de Souza
 * Cobol tag functions
 * We could look for anything that could be a paragraph name.
 * i.e. anything that starts in column 8 is one word and ends in a full stop.
 */
void
Cobol_paragraphs (inf)
     FILE *inf;
{
  register char *bp, *ep;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (lb.len < 9)
	continue;
      bp += 8;

      /* If eoln, compiler option or comment ignore whole line. */
      if (bp[-1] != ' ' || !isalnum (bp[0]))
        continue;

      for (ep = bp; isalnum (*ep) || *ep == '-'; ep++)
	continue;
      if (*ep++ == '.')
	pfnote ((CTAGS) ? savenstr (bp, ep-bp) : NULL, TRUE,
		lb.buffer, ep - lb.buffer + 1, lineno, linecharno);
    }
}

/* Added by Mosur Mohan, 4/22/88 */
/* Pascal parsing                */

/*
 *  Locates tags for procedures & functions.  Doesn't do any type- or
 *  var-definitions.  It does look for the keyword "extern" or
 *  "forward" immediately following the procedure statement; if found,
 *  the tag is skipped.
 */
void
Pascal_functions (inf)
     FILE *inf;
{
  linebuffer tline;		/* mostly copied from C_entries */
  long save_lcno;
  int save_lineno, save_len;
  char c, *cp, *namebuf;

  bool				/* each of these flags is TRUE iff: */
    incomment,			/* point is inside a comment */
    inquote,			/* point is inside '..' string */
    get_tagname,		/* point is after PROCEDURE/FUNCTION
				   keyword, so next item = potential tag */
    found_tag,			/* point is after a potential tag */
    inparms,			/* point is within parameter-list */
    verify_tag;			/* point has passed the parm-list, so the
				   next token will determine whether this
				   is a FORWARD/EXTERN to be ignored, or
				   whether it is a real tag */

  save_lcno = save_lineno = save_len = 0; /* keep compiler quiet */
  namebuf = NULL;		/* keep compiler quiet */
  lineno = 0;
  charno = 0;
  dbp = lb.buffer;
  *dbp = '\0';
  initbuffer (&tline);

  incomment = inquote = FALSE;
  found_tag = FALSE;		/* have a proc name; check if extern */
  get_tagname = FALSE;		/* have found "procedure" keyword    */
  inparms = FALSE;		/* found '(' after "proc"            */
  verify_tag = FALSE;		/* check if "extern" is ahead        */

  
  while (!feof (inf))		/* long main loop to get next char */  
    {
      c = *dbp++;
      if (c == '\0')		/* if end of line */
	{
	  lineno++;
	  linecharno = charno;
	  charno += readline (&lb, inf);
	  dbp = lb.buffer;
	  if (*dbp == '\0')
	    continue;
	  if (!((found_tag && verify_tag)
		|| get_tagname))
	    c = *dbp++;		/* only if don't need *dbp pointing
				   to the beginning of the name of
				   the procedure or function */
	}
      if (incomment)
	{
	  if (c == '}')		/* within { } comments */
	    incomment = FALSE;
	  else if (c == '*' && *dbp == ')') /* within (* *) comments */
	    {
	      dbp++;
	      incomment = FALSE;
	    }
	  continue;
	}
      else if (inquote)
	{
	  if (c == '\'')
	    inquote = FALSE;
	  continue;
	}
      else
	switch (c)
	  {
	  case '\'':
	    inquote = TRUE;	/* found first quote */
	    continue;
	  case '{':		/* found open { comment */
	    incomment = TRUE;
	    continue;
	  case '(':
	    if (*dbp == '*')	/* found open (* comment */
	      {
		incomment = TRUE;
		dbp++;
	      }
	    else if (found_tag)	/* found '(' after tag, i.e., parm-list */
	      inparms = TRUE;
	    continue;
	  case ')':		/* end of parms list */
	    if (inparms)
	      inparms = FALSE;
	    continue;
	  case ';':
	    if (found_tag && !inparms) /* end of proc or fn stmt */
	      {
		verify_tag = TRUE;
		break;
	      }
	    continue;
	  }
      if (found_tag && verify_tag && (*dbp != ' '))
	{
	  /* check if this is an "extern" declaration */
	  if (*dbp == '\0')
	    continue;
	  if (lowcase (*dbp == 'e'))
	    {
	      if (tail ("extern"))	/* superfluous, really! */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  else if (lowcase (*dbp) == 'f')
	    {
	      if (tail ("forward"))	/*  check for forward reference */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  if (found_tag && verify_tag) /* not external proc, so make tag */
	    {
	      found_tag = FALSE;
	      verify_tag = FALSE;
	      pfnote (namebuf, TRUE,
		      tline.buffer, save_len, save_lineno, save_lcno);
	      continue;
	    }
	}
      if (get_tagname)		/* grab name of proc or fn */
	{
	  if (*dbp == '\0')
	    continue;

	  /* save all values for later tagging */
	  grow_linebuffer (&tline, lb.len + 1);
	  strcpy (tline.buffer, lb.buffer);
	  save_lineno = lineno;
	  save_lcno = linecharno;

	  /* grab block name */
	  for (cp = dbp + 1; *cp != '\0' && !endtoken (*cp); cp++)
	    continue;
	  namebuf = (CTAGS) ? savenstr (dbp, cp-dbp) : NULL;
	  dbp = cp;		/* set dbp to e-o-token */
	  save_len = dbp - lb.buffer + 1;
	  get_tagname = FALSE;
	  found_tag = TRUE;
	  continue;

	  /* and proceed to check for "extern" */
	}
      else if (!incomment && !inquote && !found_tag)
	{
	  /* check for proc/fn keywords */
	  switch (lowcase (c))
	    {
	    case 'p':
	      if (tail ("rocedure"))	/* c = 'p', dbp has advanced */
		get_tagname = TRUE;
	      continue;
	    case 'f':
	      if (tail ("unction"))
		get_tagname = TRUE;
	      continue;
	    }
	}
    }				/* while not eof */

  free (tline.buffer);
}

/*
 * lisp tag functions
 *  look for (def or (DEF, quote or QUOTE
 */
int L_isdef PP ((char *strp));
int
L_isdef (strp)
     register char *strp;
{
  return ((strp[1] == 'd' || strp[1] == 'D')
	  && (strp[2] == 'e' || strp[2] == 'E')
	  && (strp[3] == 'f' || strp[3] == 'F'));
}
int L_isquote PP ((char *strp));
int
L_isquote (strp)
     register char *strp;
{
  return ((*++strp == 'q' || *strp == 'Q')
	  && (*++strp == 'u' || *strp == 'U')
	  && (*++strp == 'o' || *strp == 'O')
	  && (*++strp == 't' || *strp == 'T')
	  && (*++strp == 'e' || *strp == 'E')
	  && isspace (*++strp));
}

void L_getit PP ((void));
void
L_getit ()
{
  register char *cp;

  if (*dbp == '\'')		/* Skip prefix quote */
    dbp++;
  else if (*dbp == '(')
  {
    if (L_isquote (dbp))
      dbp += 7;			/* Skip "(quote " */
    else
      dbp += 1;			/* Skip "(" before name in (defstruct (foo)) */
    dbp = skip_spaces (dbp);
  }

  for (cp = dbp /*+1*/;
       *cp != '\0' && *cp != '(' && *cp != ' ' && *cp != ')';
       cp++)
    continue;
  if (cp == dbp)
    return;

  pfnote ((CTAGS) ? savenstr (dbp, cp-dbp) : NULL, TRUE,
	  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
}

void
Lisp_functions (inf)
     FILE *inf;
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (dbp[0] == '(')
	{
	  if (L_isdef (dbp))
	    {
	      dbp = skip_non_spaces (dbp);
	      dbp = skip_spaces (dbp);
	      L_getit ();
	    }
	  else
	    {
	      /* Check for (foo::defmumble name-defined ... */
	      do
		dbp++;
	      while (*dbp != '\0' && !isspace (*dbp)
		     && *dbp != ':' && *dbp != '(' && *dbp != ')');
	      if (*dbp == ':')
		{
		  do
		    dbp++;
		  while (*dbp == ':');

		  if (L_isdef (dbp - 1))
		    {
		      dbp = skip_non_spaces (dbp);
		      dbp = skip_spaces (dbp);
		      L_getit ();
		    }
		}
	    }
	}
    }
}

/*
 * Postscript tag functions
 * Just look for lines where the first character is '/'
 * Richard Mlynarik <mly@adoc.xerox.com>
 */
void 
Postscript_functions (inf)
     FILE *inf;
{
  register char *bp, *ep;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (bp[0] == '/')
	{
	  for (ep = bp+1;
	       *ep != '\0' && *ep != ' ' && *ep != '{';
	       ep++)
	    continue;
	  pfnote ((CTAGS) ? savenstr (bp, ep-bp) : NULL, TRUE,
		  lb.buffer, ep - lb.buffer + 1, lineno, linecharno);
	}
    }
}


/*
 * Scheme tag functions
 * look for (def... xyzzy
 * look for (def... (xyzzy
 * look for (def ... ((...(xyzzy ....
 * look for (set! xyzzy
 */

void get_scheme PP ((void));

void
Scheme_functions (inf)
     FILE *inf;
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (dbp[0] == '('
	  && (dbp[1] == 'D' || dbp[1] == 'd')
	  && (dbp[2] == 'E' || dbp[2] == 'e')
	  && (dbp[3] == 'F' || dbp[3] == 'f'))
	{
	  dbp = skip_non_spaces (dbp);
	  /* Skip over open parens and white space */
	  while (isspace (*dbp) || *dbp == '(')
	    dbp++;
	  get_scheme ();
	}
      if (dbp[0] == '('
	  && (dbp[1] == 'S' || dbp[1] == 's')
	  && (dbp[2] == 'E' || dbp[2] == 'e')
	  && (dbp[3] == 'T' || dbp[3] == 't')
	  && (dbp[4] == '!' || dbp[4] == '!')
	  && (isspace (dbp[5])))
	{
	  dbp = skip_non_spaces (dbp);
	  dbp = skip_spaces (dbp);
	  get_scheme ();
	}
    }
}

void
get_scheme ()
{
  register char *cp;

  if (*dbp == '\0')
    return;
  /* Go till you get to white space or a syntactic break */
  for (cp = dbp + 1;
       *cp != '\0' && *cp != '(' && *cp != ')' && !isspace (*cp);
       cp++)
    continue;
  pfnote ((CTAGS) ? savenstr (dbp, cp-dbp) : NULL, TRUE,
	  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
}

/* Find tags in TeX and LaTeX input files.  */

/* TEX_toktab is a table of TeX control sequences that define tags.
   Each TEX_tabent records one such control sequence.
   CONVERT THIS TO USE THE Stab TYPE!! */
struct TEX_tabent
{
  char *name;
  int len;
};

struct TEX_tabent *TEX_toktab = NULL;	/* Table with tag tokens */

/* Default set of control sequences to put into TEX_toktab.
   The value of environment var TEXTAGS is prepended to this.  */

char *TEX_defenv = "\
:chapter:section:subsection:subsubsection:eqno:label:ref:cite:bibitem\
:part:appendix:entry:index";

void TEX_mode PP ((FILE *inf));
struct TEX_tabent *TEX_decode_env PP ((char *evarname, char *defenv));
int TEX_Token PP ((char *cp));

char TEX_esc = '\\';
char TEX_opgrp = '{';
char TEX_clgrp = '}';

/*
 * TeX/LaTeX scanning loop.
 */
void
TeX_functions (inf)
     FILE *inf;
{
  char *cp, *lasthit;
  register int i;

  /* Select either \ or ! as escape character.  */
  TEX_mode (inf);

  /* Initialize token table once from environment. */
  if (!TEX_toktab)
    TEX_toktab = TEX_decode_env ("TEXTAGS", TEX_defenv);

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      lasthit = cp;
      /* Look at each esc in line. */
      while ((cp = etags_strchr (cp, TEX_esc)) != NULL)
	{
	  if (*++cp == '\0')
	    break;
	  linecharno += cp - lasthit;
	  lasthit = cp;
	  i = TEX_Token (lasthit);
	  if (i >= 0)
	    {
	      /* We seem to include the TeX command in the tag name.
	      register char *p;
	      for (p = lasthit + TEX_toktab[i].len;
		   *p != '\0' && *p != TEX_clgrp;
		   p++)
		continue; */
	      pfnote (/*savenstr (lasthit, p-lasthit)*/ (char *)NULL, TRUE,
		      lb.buffer, lb.len, lineno, linecharno);
	      break;		/* We only tag a line once */
	    }
	}
    }
}

#define TEX_LESC '\\'
#define TEX_SESC '!'
#define TEX_cmt  '%'

/* Figure out whether TeX's escapechar is '\\' or '!' and set grouping
   chars accordingly. */
void
TEX_mode (inf)
     FILE *inf;
{
  int c;

  while ((c = getc (inf)) != EOF)
    {
      /* Skip to next line if we hit the TeX comment char. */
      if (c == TEX_cmt)
	while (c != '\n')
	  c = getc (inf);
      else if (c == TEX_LESC || c == TEX_SESC )
	break;
    }

  if (c == TEX_LESC)
    {
      TEX_esc = TEX_LESC;
      TEX_opgrp = '{';
      TEX_clgrp = '}';
    }
  else
    {
      TEX_esc = TEX_SESC;
      TEX_opgrp = '<';
      TEX_clgrp = '>';
    }
  rewind (inf);
}

/* Read environment and prepend it to the default string.
   Build token table. */
struct TEX_tabent *
TEX_decode_env (evarname, defenv)
     char *evarname;
     char *defenv;
{
  register char *env, *p;

  struct TEX_tabent *tab;
  int size, i;

  /* Append default string to environment. */
  env = getenv (evarname);
  if (!env)
    env = defenv;
  else
    {
      char *oldenv = env;
      env = concat (oldenv, defenv, "");
      free (oldenv);
    }

  /* Allocate a token table */
  for (size = 1, p = env; p;)
    if ((p = etags_strchr (p, ':')) && *++p != '\0')
      size++;
  /* Add 1 to leave room for null terminator.  */
  tab = xnew (size + 1, struct TEX_tabent);

  /* Unpack environment string into token table. Be careful about */
  /* zero-length strings (leading ':', "::" and trailing ':') */
  for (i = 0; *env;)
    {
      p = etags_strchr (env, ':');
      if (!p)			/* End of environment string. */
	p = env + strlen (env);
      if (p - env > 0)
	{			/* Only non-zero strings. */
	  tab[i].name = savenstr (env, p - env);
	  tab[i].len = strlen (tab[i].name);
	  i++;
	}
      if (*p)
	env = p + 1;
      else
	{
	  tab[i].name = NULL;	/* Mark end of table. */
	  tab[i].len = 0;
	  break;
	}
    }
  return tab;
}

/* If the text at CP matches one of the tag-defining TeX command names,
   return the pointer to the first occurrence of that command in TEX_toktab.
   Otherwise return -1.
   Keep the capital `T' in `token' for dumb truncating compilers
   (this distinguishes it from `TEX_toktab' */
int
TEX_Token (cp)
     char *cp;
{
  int i;

  for (i = 0; TEX_toktab[i].len > 0; i++)
    if (strneq (TEX_toktab[i].name, cp, TEX_toktab[i].len))
      return i;
  return -1;
}

/*
 * Prolog support (rewritten) by Anders Lindgren, Mar. 96
 *
 * Assumes that the predicate starts at column 0.
 * Only the first clause of a predicate is added. 
 */
int prolog_pred PP ((char *s, char *last));
void prolog_skip_comment PP ((linebuffer *plb, FILE *inf));
int prolog_atom PP ((char *s, int pos));

void
Prolog_functions (inf)
     FILE *inf;
{
  char *cp, *last;
  int len;
  int allocated;

  allocated = 0;
  len = 0;
  last = NULL;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (cp[0] == '\0')	/* Empty line */
	continue;
      else if (isspace (cp[0])) /* Not a predicate */
	continue;
      else if (cp[0] == '/' && cp[1] == '*')	/* comment. */
	prolog_skip_comment (&lb, inf);
      else if ((len = prolog_pred (cp, last)) > 0)
	{
	  /* Predicate.  Store the function name so that we only
	     generate a tag for the first clause.  */
	  if (last == NULL)
	    last = xnew(len + 1, char);
	  else if (len + 1 > allocated)
	    last = xrnew (last, len + 1, char);
	  allocated = len + 1;
	  strncpy (last, cp, len);
	  last[len] = '\0';
	}
    }
}


void
prolog_skip_comment (plb, inf)
     linebuffer *plb;
     FILE *inf;
{
  char *cp;

  do
    {
      for (cp = plb->buffer; *cp != '\0'; cp++)
	if (cp[0] == '*' && cp[1] == '/')
	  return;
      lineno++;
      linecharno += readline (plb, inf);
    }
  while (!feof(inf));
}

/*
 * A predicate definition is added if it matches:
 *     <beginning of line><Prolog Atom><whitespace>(
 *
 * It is added to the tags database if it doesn't match the
 * name of the previous clause header.
 *
 * Return the size of the name of the predicate, or 0 if no header
 * was found.
 */
int
prolog_pred (s, last)
     char *s;
     char *last;		/* Name of last clause. */
{
  int pos;
  int len;

  pos = prolog_atom (s, 0);
  if (pos < 1)
    return 0;

  len = pos;
  pos = skip_spaces (s + pos) - s;

  if ((s[pos] == '(') || (s[pos] == '.'))
    {
      if (s[pos] == '(')
	pos++;

      /* Save only the first clause. */
      if (last == NULL
	  || len != (int) strlen (last)
	  || !strneq (s, last, len))
	{
	  pfnote ((CTAGS) ? savenstr (s, len) : NULL, TRUE,
		  s, pos, lineno, linecharno);
	  return len;
	}
    }
  return 0;
}

/*
 * Consume a Prolog atom.
 * Return the number of bytes consumed, or -1 if there was an error.
 *
 * A prolog atom, in this context, could be one of:
 * - An alphanumeric sequence, starting with a lower case letter.
 * - A quoted arbitrary string. Single quotes can escape themselves.
 *   Backslash quotes everything.
 */
int
prolog_atom (s, pos)
     char *s;
     int pos;
{
  int origpos;

  origpos = pos;

  if (islower(s[pos]) || (s[pos] == '_'))
    {
      /* The atom is unquoted. */
      pos++;
      while (isalnum(s[pos]) || (s[pos] == '_'))
	{
	  pos++;
	}
      return pos - origpos;
    }
  else if (s[pos] == '\'')
    {
      pos++;

      while (1) 
	{
	  if (s[pos] == '\'')
	    {
	      pos++;
	      if (s[pos] != '\'')
		break;
	      pos++;		/* A double quote */
	    }
	  else if (s[pos] == '\0')
	    /* Multiline quoted atoms are ignored. */
	    return -1;
	  else if (s[pos] == '\\')
	    {
	      if (s[pos+1] == '\0')
		return -1;
	      pos += 2;
	    }
	  else
	    pos++;
	}
      return pos - origpos;
    }
  else
    return -1;
}

/* 
 * Support for Erlang  --  Anders Lindgren, Feb 1996.
 *
 * Generates tags for functions, defines, and records.
 *
 * Assumes that Erlang functions start at column 0.
 */
int erlang_func PP ((char *s, char *last));
void erlang_attribute PP ((char *s));
int erlang_atom PP ((char *s, int pos));

void
Erlang_functions (inf)
     FILE *inf;
{
  char *cp, *last;
  int len;
  int allocated;

  allocated = 0;
  len = 0;
  last = NULL;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (cp[0] == '\0')	/* Empty line */
	continue;
      else if (isspace (cp[0])) /* Not function nor attribute */
	continue;
      else if (cp[0] == '%')	/* comment */
	continue;
      else if (cp[0] == '"')	/* Sometimes, strings start in column one */
	continue;
      else if (cp[0] == '-') 	/* attribute, e.g. "-define" */
	{
	  erlang_attribute (cp);
	  last = NULL;
	}
      else if ((len = erlang_func (cp, last)) > 0)
	{
	  /* 
	   * Function.  Store the function name so that we only
	   * generates a tag for the first clause.
	   */
	  if (last == NULL)
	    last = xnew (len + 1, char);
	  else if (len + 1 > allocated)
	    last = xrnew (last, len + 1, char);
	  allocated = len + 1;
	  strncpy (last, cp, len);
	  last[len] = '\0';
	}
    }
}


/*
 * A function definition is added if it matches:
 *     <beginning of line><Erlang Atom><whitespace>(
 *
 * It is added to the tags database if it doesn't match the
 * name of the previous clause header.
 *
 * Return the size of the name of the function, or 0 if no function
 * was found.
 */
int
erlang_func (s, last)
     char *s;
     char *last;		/* Name of last clause. */
{
  int pos;
  int len;

  pos = erlang_atom (s, 0);
  if (pos < 1)
    return 0;

  len = pos;
  pos = skip_spaces (s + pos) - s;

  /* Save only the first clause. */
  if (s[pos++] == '('
      && (last == NULL
	  || len != (int) strlen (last)
	  || !strneq (s, last, len)))
	{
	  pfnote ((CTAGS) ? savenstr (s, len) : NULL, TRUE,
		  s, pos, lineno, linecharno);
	  return len;
	}

  return 0;
}


/*
 * Handle attributes.  Currently, tags are generated for defines 
 * and records.
 *
 * They are on the form:
 * -define(foo, bar).
 * -define(Foo(M, N), M+N).
 * -record(graph, {vtab = notable, cyclic = true}).
 */
void
erlang_attribute (s)
     char *s;
{
  int pos;
  int len;

  if (strneq (s, "-define", 7) || strneq (s, "-record", 7))
    {
      pos = skip_spaces (s + 7) - s;
      if (s[pos++] == '(') 
	{
	  pos = skip_spaces (s + pos) - s;
	  len = erlang_atom (s, pos);
	  if (len != 0)
	    pfnote ((CTAGS) ? savenstr (& s[pos], len) : NULL, TRUE,
		    s, pos + len, lineno, linecharno);
	}
    }
  return;
}


/*
 * Consume an Erlang atom (or variable).
 * Return the number of bytes consumed, or -1 if there was an error.
 */
int
erlang_atom (s, pos)
     char *s;
     int pos;
{
  int origpos;

  origpos = pos;

  if (isalpha (s[pos]) || s[pos] == '_')
    {
      /* The atom is unquoted. */
      pos++;
      while (isalnum (s[pos]) || s[pos] == '_')
	pos++;
      return pos - origpos;
    }
  else if (s[pos] == '\'')
    {
      pos++;

      while (1) 
	{
	  if (s[pos] == '\'')
	    {
	      pos++;
	      break;
	    }
	  else if (s[pos] == '\0')
	    /* Multiline quoted atoms are ignored. */
	    return -1;
	  else if (s[pos] == '\\')
	    {
	      if (s[pos+1] == '\0')
		return -1;
	      pos += 2;
	    }
	  else
	    pos++;
	}
      return pos - origpos;
    }
  else
    return -1;
}

#ifdef ETAGS_REGEXPS

/* Take a string like "/blah/" and turn it into "blah", making sure
   that the first and last characters are the same, and handling
   quoted separator characters.  Actually, stops on the occurrence of
   an unquoted separator.  Also turns "\t" into a Tab character.
   Returns pointer to terminating separator.  Works in place.  Null
   terminates name string. */
char * scan_separators PP ((char *name));
char *
scan_separators (name)
     char *name;
{
  char sep = name[0];
  char *copyto = name;
  bool quoted = FALSE;

  for (++name; *name != '\0'; ++name)
    {
      if (quoted)
	{
	  if (*name == 't')
	    *copyto++ = '\t';
	  else if (*name == sep)
	    *copyto++ = sep;
	  else
	    {
	      /* Something else is quoted, so preserve the quote. */
	      *copyto++ = '\\';
	      *copyto++ = *name;
	    }
	  quoted = FALSE;
	}
      else if (*name == '\\')
	quoted = TRUE;
      else if (*name == sep)
	break;
      else
	*copyto++ = *name;
    }

  /* Terminate copied string. */
  *copyto = '\0';
  return name;
}

/* Look at the argument of --regex or --no-regex and do the right
   thing.  Same for each line of a regexp file. */
void
analyse_regex (regex_arg)
     char *regex_arg;
{
  if (regex_arg == NULL)
    free_patterns ();		/* --no-regex: remove existing regexps */

  /* A real --regexp option or a line in a regexp file. */
  switch (regex_arg[0])
    {
      /* Comments in regexp file or null arg to --regex. */
    case '\0':
    case ' ':
    case '\t':
      break;

      /* Read a regex file.  This is recursive and may result in a
	 loop, which will stop when the file descriptors are exhausted. */
    case '@':
      {
	FILE *regexfp;
	linebuffer regexbuf;
	char *regexfile = regex_arg + 1;

	/* regexfile is a file containing regexps, one per line. */
	regexfp = fopen (regexfile, "r");
	if (regexfp == NULL)
	  {
	    pfatal (regexfile);
	    return;
	  }
	initbuffer (&regexbuf);
	while (readline_internal (&regexbuf, regexfp) > 0)
	  analyse_regex (regexbuf.buffer);
	free (regexbuf.buffer);
	fclose (regexfp);
      }
      break;

      /* Regexp to be used for a specific language only. */
    case '{':
      {
	language *lang;
	char *lang_name = regex_arg + 1;
	char *cp;

	for (cp = lang_name; *cp != '}'; cp++)
	  if (*cp == '\0')
	    {
	      error ("unterminated language name in regex: %s", regex_arg);
	      return;
	    }
	*cp = '\0';
	lang = get_language_from_name (lang_name);
	if (lang == NULL)
	  return;
	add_regex (cp + 1, lang);
      }
      break;

      /* Regexp to be used for any language. */
    default:
      add_regex (regex_arg, NULL);
      break;
    }
}

/* Turn a name, which is an ed-style (but Emacs syntax) regular
   expression, into a real regular expression by compiling it. */
void
add_regex (regexp_pattern, lang)
     char *regexp_pattern;
     language *lang;
{
  char *name;
  const char *err;
  struct re_pattern_buffer *patbuf;
  pattern *pp;


  if (regexp_pattern[strlen(regexp_pattern)-1] != regexp_pattern[0])
    {
      error ("%s: unterminated regexp", regexp_pattern);
      return;
    }
  name = scan_separators (regexp_pattern);
  if (regexp_pattern[0] == '\0')
    {
      error ("null regexp", (char *)NULL);
      return;
    }
  (void) scan_separators (name);

  patbuf = xnew (1, struct re_pattern_buffer);
  patbuf->translate = NULL;
  patbuf->fastmap = NULL;
  patbuf->buffer = NULL;
  patbuf->allocated = 0;

  err = re_compile_pattern (regexp_pattern, strlen (regexp_pattern), patbuf);
  if (err != NULL)
    {
      error ("%s while compiling pattern", err);
      return;
    }

  pp = p_head;
  p_head = xnew (1, pattern);
  p_head->regex = savestr (regexp_pattern);
  p_head->p_next = pp;
  p_head->language = lang;
  p_head->pattern = patbuf;
  p_head->name_pattern = savestr (name);
  p_head->error_signaled = FALSE;
}

/*
 * Do the substitutions indicated by the regular expression and
 * arguments.
 */
char * substitute PP ((char *in, char *out, struct re_registers *regs));
char *
substitute (in, out, regs)
     char *in, *out;
     struct re_registers *regs;
{
  char *result, *t;
  int size, dig, diglen;

  result = NULL;
  size = strlen (out);

  /* Pass 1: figure out how much to allocate by finding all \N strings. */
  if (out[size - 1] == '\\')
    fatal ("pattern error in \"%s\"", out);
  for (t = etags_strchr (out, '\\');
       t != NULL;
       t = etags_strchr (t + 2, '\\'))
    if (isdigit (t[1]))
      {
	dig = t[1] - '0';
	diglen = regs->end[dig] - regs->start[dig];
	size += diglen - 2;
      }
    else
      size -= 1;

  /* Allocate space and do the substitutions. */
  result = xnew (size + 1, char);

  for (t = result; *out != '\0'; out++)
    if (*out == '\\' && isdigit (*++out))
      {
	/* Using "dig2" satisfies my debugger.  Bleah. */
	dig = *out - '0';
	diglen = regs->end[dig] - regs->start[dig];
	strncpy (t, in + regs->start[dig], diglen);
	t += diglen;
      }
    else
      *t++ = *out;
  *t = '\0';

  if (DEBUG && (t > result + size || t - result != strlen (result)))
    abort ();

  return result;
}

/* Deallocate all patterns. */
void
free_patterns ()
{
  pattern *pp;
  while (p_head != NULL)
    {
      pp = p_head->p_next;
      free (p_head->regex);
      free (p_head->name_pattern);
      free (p_head);
      p_head = pp;
    }
  return;
}

#endif /* ETAGS_REGEXPS */
/* Initialize a linebuffer for use */
void
initbuffer (lbp)
     linebuffer *lbp;
{
  lbp->size = 200;
  lbp->buffer = xnew (200, char);
}

/*
 * Read a line of text from `stream' into `lbp', excluding the
 * newline or CR-NL, if any.  Return the number of characters read from
 * `stream', which is the length of the line including the newline.
 *
 * On DOS or Windows we do not count the CR character, if any, before the
 * NL, in the returned length; this mirrors the behavior of emacs on those
 * platforms (for text files, it translates CR-NL to NL as it reads in the
 * file).
 */
long
readline_internal (lbp, stream)
     linebuffer *lbp;
     register FILE *stream;
{
  char *buffer = lbp->buffer;
  register char *p = lbp->buffer;
  register char *pend;
  int chars_deleted;

  pend = p + lbp->size;		/* Separate to avoid 386/IX compiler bug.  */

  while (1)
    {
      register int c = getc (stream);
      if (p == pend)
	{
	  /* We're at the end of linebuffer: expand it. */
	  lbp->size *= 2;
	  buffer = xrnew (buffer, lbp->size, char);
	  p += buffer - lbp->buffer;
	  pend = buffer + lbp->size;
	  lbp->buffer = buffer;
	}
      if (c == EOF)
	{
	  *p = '\0';
	  chars_deleted = 0;
	  break;
	}
      if (c == '\n')
	{
	  if (p > buffer && p[-1] == '\r')
	    {
	      p -= 1;
#ifdef WIN32_NATIVE
	     /* Assume CRLF->LF translation will be performed by Emacs
		when loading this file, so CRs won't appear in the buffer.
		It would be cleaner to compensate within Emacs;
		however, Emacs does not know how many CRs were deleted
		before any given point in the file.  */
	      chars_deleted = 1;
#else
	      chars_deleted = 2;
#endif
	    }
	  else
	    {
	      chars_deleted = 1;
	    }
	  *p = '\0';
	  break;
	}
      *p++ = c;
    }
  lbp->len = p - buffer;

  return lbp->len + chars_deleted;
}

/*
 * Like readline_internal, above, but in addition try to match the
 * input line against relevant regular expressions.
 */
long
readline (lbp, stream)
     linebuffer *lbp;
     FILE *stream;
{
  /* Read new line. */
  long result = readline_internal (lbp, stream);
#ifdef ETAGS_REGEXPS
  int match;
  pattern *pp;

  /* Match against relevant patterns. */
  if (lbp->len > 0)
    for (pp = p_head; pp != NULL; pp = pp->p_next)
      {
	/* Only use generic regexps or those for the current language. */
	if (pp->language != NULL && pp->language != curlang)
	  continue;

	match = re_match (pp->pattern, lbp->buffer, lbp->len, 0, &pp->regs);
	switch (match)
	  {
	  case -2:
	    /* Some error. */
	    if (!pp->error_signaled)
	      {
		error ("error while matching \"%s\"", pp->regex);
		pp->error_signaled = TRUE;
	      }
	    break;
	  case -1:
	    /* No match. */
	    break;
	  default:
	    /* Match occurred.  Construct a tag. */
	    if (pp->name_pattern[0] != '\0')
	      {
		/* Make a named tag. */
		char *name = substitute (lbp->buffer,
					 pp->name_pattern, &pp->regs);
		if (name != NULL)
		  pfnote (name, TRUE, lbp->buffer, match, lineno, linecharno);
	      }
	    else
	      {
		/* Make an unnamed tag. */
		pfnote ((char *)NULL, TRUE,
			lbp->buffer, match, lineno, linecharno);
	      }
	    break;
	  }
      }
#endif /* ETAGS_REGEXPS */
  
  return result;
}

/*
 * Return a pointer to a space of size strlen(cp)+1 allocated
 * with xnew where the string CP has been copied.
 */
char *
savestr (cp)
     char *cp;
{
  return savenstr (cp, strlen (cp));
}

/*
 * Return a pointer to a space of size LEN+1 allocated with xnew where
 * the string CP has been copied for at most the first LEN characters.
 */
char *
savenstr (cp, len)
     char *cp;
     int len;
{
  register char *dp;

  dp = xnew (len + 1, char);
  strncpy (dp, cp, len);
  dp[len] = '\0';
  return dp;
}

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 *
 * Identical to System V strrchr, included for portability.
 */
char *
etags_strrchr (sp, c)
     register char *sp;
     register int c;
{
  register char *r;

  r = NULL;
  do
    {
      if (*sp == c)
	r = sp;
  } while (*sp++);
  return r;
}


/*
 * Return the ptr in sp at which the character c first
 * appears; NULL if not found
 *
 * Identical to System V strchr, included for portability.
 */
char *
etags_strchr (sp, c)
     register char *sp;
     register int c;
{
  do
    {
      if (*sp == c)
	return sp;
    } while (*sp++);
  return NULL;
}

/* Skip spaces, return new pointer. */
char *
skip_spaces (cp)
     char *cp;
{
  while (isspace (*cp))		/* isspace('\0')==FALSE */
    cp++;
  return cp;
}

/* Skip non spaces, return new pointer. */
char *
skip_non_spaces (cp)
     char *cp;
{
  while (!iswhite (*cp))	/* iswhite('\0')==TRUE */
    cp++;
  return cp;
}

/* Print error message and exit.  */
void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (BAD);
}

void
pfatal (s1)
     char *s1;
{
  perror (s1);
  exit (BAD);
}

void
suggest_asking_for_help ()
{
  fprintf (stderr, "\tTry `%s %s' for a complete list of options.\n",
	   progname,
#ifdef LONG_OPTIONS
	   "--help"
#else
	   "-h"
#endif
	   );
  exit (BAD);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */
void
error (s1, s2)
     const char *s1, *s2;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Return a newly-allocated string whose contents
   concatenate those of s1, s2, s3.  */
char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = xnew (len1 + len2 + len3 + 1, char);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  result[len1 + len2 + len3] = '\0';

  return result;
}

/* Does the same work as the system V getcwd, but does not need to
   guess the buffer size in advance. */
char *
etags_getcwd ()
{
#ifdef HAVE_GETCWD
  int bufsize = 200;
  char *path = xnew (bufsize, char);

  while (getcwd (path, bufsize) == NULL)
    {
      if (errno != ERANGE)
	pfatal ("getcwd");
      bufsize *= 2;
      free (path);
      path = xnew (bufsize, char);
    }

  canonicalize_filename (path);
  return path;

#else /* not HAVE_GETCWD */
  linebuffer path;
  FILE *pipe;

  initbuffer (&path);
  pipe = (FILE *) popen ("pwd 2>/dev/null", "r");
  if (pipe == NULL || readline_internal (&path, pipe) == 0)
    pfatal ("pwd");
  pclose (pipe);

  return path.buffer;
#endif /* not HAVE_GETCWD */
}

/* Return a newly allocated string containing the file name of FILE
   relative to the absolute directory DIR (which should end with a slash). */
char *
relative_filename (file, dir)
     char *file, *dir;
{
  char *fp, *dp, *afn, *res;
  int i;

  /* Find the common root of file and dir (with a trailing slash). */
  afn = absolute_filename (file, cwd);
  fp = afn;
  dp = dir;
  while (*fp++ == *dp++)
    continue;
  fp--, dp--;			/* back to the first differing char */
  do				/* look at the equal chars until '/' */
    fp--, dp--;
  while (*fp != '/');

  /* Build a sequence of "../" strings for the resulting relative file name. */
  i = 0;
  while ((dp = etags_strchr (dp + 1, '/')) != NULL)
    i += 1;
  res = xnew (3*i + strlen (fp + 1) + 1, char);
  res[0] = '\0';
  while (i-- > 0)
    strcat (res, "../");

  /* Add the file name relative to the common root of file and dir. */
  strcat (res, fp + 1);
  free (afn);

  return res;
}

/* Return a newly allocated string containing the absolute file name
   of FILE given DIR (which should end with a slash). */
char *
absolute_filename (file, dir)
     char *file, *dir;
{
  char *slashp, *cp, *res;

  if (filename_is_absolute (file))
    res = savestr (file);
#ifdef WIN32_NATIVE
  /* We don't support non-absolute file names with a drive
     letter, like `d:NAME' (it's too much hassle).  */
  else if (file[1] == ':')
    fatal ("%s: relative file names with drive letters not supported", file);
#endif
  else
    res = concat (dir, file, "");

  /* Delete the "/dirname/.." and "/." substrings. */
  slashp = etags_strchr (res, '/');
  while (slashp != NULL && slashp[0] != '\0')
    {
      if (slashp[1] == '.')
	{
	  if (slashp[2] == '.'
	      && (slashp[3] == '/' || slashp[3] == '\0'))
	    {
	      cp = slashp;
	      do
		cp--;
	      while (cp >= res && !filename_is_absolute (cp));
	      if (cp < res)
		cp = slashp;	/* the absolute name begins with "/.." */
#ifdef WIN32_NATIVE
	      /* Under Windows we get `d:/NAME' as absolute
		 file name, so the luser could say `d:/../NAME'.
		 We silently treat this as `d:/NAME'.  */
	      else if (cp[0] != '/')
		cp = slashp;
#endif
	      strcpy (cp, slashp + 3);
	      slashp = cp;
	      continue;
	    }
	  else if (slashp[2] == '/' || slashp[2] == '\0')
	    {
	      strcpy (slashp, slashp + 2);
	      continue;
	    }
	}

      slashp = etags_strchr (slashp + 1, '/');
    }
  
  if (res[0] == '\0')
    return savestr ("/");
  else
    return res;
}

/* Return a newly allocated string containing the absolute
   file name of dir where FILE resides given DIR (which should
   end with a slash). */
char *
absolute_dirname (file, dir)
     char *file, *dir;
{
  char *slashp, *res;
  char save;

  canonicalize_filename (file);
  slashp = etags_strrchr (file, '/');
  if (slashp == NULL)
    return savestr (dir);
  save = slashp[1];
  slashp[1] = '\0';
  res = absolute_filename (file, dir);
  slashp[1] = save;

  return res;
}

/* Whether the argument string is an absolute file name.  The argument
   string must have been canonicalized with canonicalize_filename. */
bool
filename_is_absolute (fn)
     char *fn;
{
  return (fn[0] == '/'
#ifdef WIN32_NATIVE
	  || (isalpha(fn[0]) && fn[1] == ':' && fn[2] == '/')
#endif
	  );
}

/* Translate backslashes into slashes.  Works in place. */
void
canonicalize_filename (fn)
     register char *fn;
{
#ifdef WIN32_NATIVE
  for (; *fn != '\0'; fn++)
    if (*fn == '\\')
      *fn = '/';
#else
  /* No action. */
#endif
}

/* Increase the size of a linebuffer. */
void
grow_linebuffer (lbp, toksize)
     linebuffer *lbp;
     int toksize;
{
  while (lbp->size < toksize)
    lbp->size *= 2;
  lbp->buffer = xrnew (lbp->buffer, lbp->size, char);
}

/* Like malloc but get fatal error if memory is exhausted.  */
long *
xmalloc (size)
     unsigned int size;
{
  long *result = (long *) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)NULL);
  return result;
}

long *
xrealloc (ptr, size)
     char *ptr;
     unsigned int size;
{
  long *result =  (long *) realloc (ptr, size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)NULL);
  return result;
}
