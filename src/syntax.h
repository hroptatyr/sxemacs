/* Declarations having to do with XEmacs syntax tables.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

This file is part of SXEmacs

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

#ifndef INCLUDED_syntax_h_
#define INCLUDED_syntax_h_

#include "chartab.h"

/* A syntax table is a type of char table.

The low 7 bits of the integer is a code, as follows. The 8th bit is
used as the prefix bit flag (see below).

The values in a syntax table are either integers or conses of
integers and chars.  The lowest 7 bits of the integer are the syntax
class.  If this is Sinherit, then the actual syntax value needs to
be retrieved from the standard syntax table.

Since the logic involved in finding the actual integer isn't very
complex, you'd think the time required to retrieve it is not a
factor.  If you thought that, however, you'd be wrong, due to the
high number of times (many per character) that the syntax value is
accessed in functions such as scan_lists().  To speed this up,
we maintain a mirror syntax table that contains the actual
integers.  We can do this successfully because syntax tables are
now an abstract type, where we control all access.
*/

enum syntaxcode {
	Swhitespace,		/* whitespace character */
	Spunct,			/* random punctuation character */
	Sword,			/* word constituent */
	Ssymbol,		/* symbol constituent but not word constituent */
	Sopen,			/* a beginning delimiter */
	Sclose,			/* an ending delimiter */
	Squote,			/* a prefix character like Lisp ' */
	Sstring,		/* a string-grouping character like Lisp " */
	Smath,			/* delimiters like $ in TeX. */
	Sescape,		/* a character that begins a C-style escape */
	Scharquote,		/* a character that quotes the following character */
	Scomment,		/* a comment-starting character */
	Sendcomment,		/* a comment-ending character */
	Sinherit,		/* use the standard syntax table for this character */
	Scomment_fence,		/* Starts/ends comment which is delimited on the
				   other side by a char with the same syntaxcode.  */
	Sstring_fence,		/* Starts/ends string which is delimited on the
				   other side by a char with the same syntaxcode.  */
	Smax			/* Upper bound on codes that are meaningful */
};

enum syntaxcode charset_syntax(struct buffer *buf, Lisp_Object charset,
			       int *multi_p_out);

/* Return the syntax code for a particular character and mirror table. */

#define SYNTAX_CODE_UNSAFE(table, c) \
	((enum syntaxcode) (int)XINT(CHAR_TABLE_VALUE_UNSAFE (table, c)))

extern_inline enum syntaxcode SYNTAX_CODE(Lisp_Char_Table * table, Emchar c);
extern_inline enum syntaxcode SYNTAX_CODE(Lisp_Char_Table * table, Emchar c)
{
	return SYNTAX_CODE_UNSAFE(table, c);
}

#define SYNTAX_UNSAFE(table, c) \
  ((enum syntaxcode) (SYNTAX_CODE_UNSAFE (table, c) & 0177))

#define SYNTAX_FROM_CODE(code) ((enum syntaxcode) ((code) & 0177))
#define SYNTAX(table, c) SYNTAX_FROM_CODE (SYNTAX_CODE (table, c))

extern_inline int WORD_SYNTAX_P(Lisp_Char_Table * table, Emchar c);
extern_inline int WORD_SYNTAX_P(Lisp_Char_Table * table, Emchar c)
{
	return SYNTAX(table, c) == Sword;
}

/* OK, here's a graphic diagram of the format of the syntax values:

   Bit number:

 [ 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 ]
 [ 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 ]

   <-----> <-----> <-------------> <-------------> ^  <----------->
    ELisp  unused  |comment bits |     unused      |   syntax code
     tag           | | | | | | | |                 |
    stuff          | | | | | | | |                 |
		   | | | | | | | |                 |
		   | | | | | | | |                 `--> prefix flag
		   | | | | | | | |
		   | | | | | | | `--> comment end style B, second char
		   | | | | | | `----> comment end style A, second char
		   | | | | | `------> comment end style B, first char
		   | | | | `--------> comment end style A, first char
		   | | | `----------> comment start style B, second char
		   | | `------------> comment start style A, second char
		   | `--------------> comment start style B, first char
		   `----------------> comment start style A, first char

  In a 64-bit integer, there would be 32 more unused bits between
  the tag and the comment bits.

  Clearly, such a scheme will not work for Mule, because the matching
  paren could be any character and as such requires 19 bits, which
  we don't got.

  Remember that under Mule we use char tables instead of vectors.
  So what we do is use another char table for the matching paren
  and store a pointer to it in the first char table. (This frees
  code from having to worry about passing two tables around.)
*/

/* The prefix flag bit for backward-prefix-chars is now put into bit 7. */

#define SYNTAX_PREFIX_UNSAFE(table, c) \
  ((SYNTAX_CODE_UNSAFE (table, c) >> 7) & 1)
#define SYNTAX_PREFIX(table, c) \
  ((SYNTAX_CODE (table, c) >> 7) & 1)

/* Bits 23-16 are used to implement up to two comment styles
   in a single buffer. They have the following meanings:

  1. first of a one or two character comment-start sequence of style a.
  2. first of a one or two character comment-start sequence of style b.
  3. second of a two-character comment-start sequence of style a.
  4. second of a two-character comment-start sequence of style b.
  5. first of a one or two character comment-end sequence of style a.
  6. first of a one or two character comment-end sequence of style b.
  7. second of a two-character comment-end sequence of style a.
  8. second of a two-character comment-end sequence of style b.

From the internals manual:

Syntax codes are implemented as bitfields in an int.  Bits 0-6 contain
the syntax code itself, bit 7 is a special prefix flag used for Lisp,
and bits 16-23 contain comment syntax flags.  From the Lisp programmer's
point of view, there are 11 flags: 2 styles X 2 characters X @{start,
end@} flags for two-character comment delimiters, 2 style flags for
one-character comment delimiters, and the prefix flag.

Internally, however, the characters used in multi-character delimiters
will have non-comment-character syntax classes (@emph{e.g.}, the
@samp{/} in C's @samp{/}@samp{*} comment-start delimiter has ``punctuation''
\(here meaning ``operator-like'') class in C modes).  Thus in a mixed
comment style, such as C++'s @samp{//} to end of line, is represented by
giving @samp{/} the ``punctuation'' class and the ``style b first
character of start sequence'' and ``style b second character of start
sequence'' flags.  The fact that class is @emph{not} punctuation allows
the syntax scanner to recognize that this is a multi-character
delimiter.  The @samp{newline} character is given (single-character)
``comment-end'' @emph{class} and the ``style b first character of end
sequence'' @emph{flag}.  The ``comment-end'' class allows the scanner to
determine that no second character is needed to terminate the comment.
 */

#define SYNTAX_COMMENT_BITS(c) \
  ((SYNTAX_CODE (mirrortab, c) >> 16) &0xff)

#define SYNTAX_FIRST_OF_START_A  0x80
#define SYNTAX_FIRST_OF_START_B  0x40
#define SYNTAX_SECOND_OF_START_A 0x20
#define SYNTAX_SECOND_OF_START_B 0x10
#define SYNTAX_FIRST_OF_END_A    0x08
#define SYNTAX_FIRST_OF_END_B    0x04
#define SYNTAX_SECOND_OF_END_A   0x02
#define SYNTAX_SECOND_OF_END_B   0x01

#define SYNTAX_COMMENT_STYLE_A   0xaa
#define SYNTAX_COMMENT_STYLE_B   0x55
#define SYNTAX_FIRST_CHAR_START  0xc0
#define SYNTAX_FIRST_CHAR_END    0x0c
#define SYNTAX_FIRST_CHAR        0xcc
#define SYNTAX_SECOND_CHAR_START 0x30
#define SYNTAX_SECOND_CHAR_END   0x03
#define SYNTAX_SECOND_CHAR       0x33

/* #### These are now more or less equivalent to
   SYNTAX_COMMENT_MATCH_START ...*/
/* a and b must be first and second start chars for a common type */
#define SYNTAX_START_P(a, b)                                     \
  (((SYNTAX_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START) >> 2)    \
   & (SYNTAX_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_START))

/* ... and  SYNTAX_COMMENT_MATCH_END */
/* a and b must be first and second end chars for a common type */
#define SYNTAX_END_P(a, b)                                       \
  (((SYNTAX_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END) >> 2)      \
   & (SYNTAX_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_END))

#define SYNTAX_STYLES_MATCH_START_P(a, b, mask)			\
  ((SYNTAX_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START & (mask))	\
   && (SYNTAX_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_START & (mask)))

#define SYNTAX_STYLES_MATCH_END_P(a, b, mask)			\
  ((SYNTAX_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END & (mask))	\
   && (SYNTAX_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_END & (mask)))

#define SYNTAX_STYLES_MATCH_1CHAR_P(a, mask)	\
  ((SYNTAX_COMMENT_BITS (a) & (mask)))

#define STYLE_FOUND_P(a, b, startp, style)		\
  ((SYNTAX_COMMENT_BITS (a) &				\
    ((startp) ? SYNTAX_FIRST_CHAR_START :		\
     SYNTAX_FIRST_CHAR_END) & (style))			\
   && (SYNTAX_COMMENT_BITS (b) &			\
    ((startp) ? SYNTAX_SECOND_CHAR_START :		\
     SYNTAX_SECOND_CHAR_END) & (style)))

#define SYNTAX_COMMENT_MASK_START(a, b)			\
  ((STYLE_FOUND_P (a, b, 1, SYNTAX_COMMENT_STYLE_A)	\
    ? SYNTAX_COMMENT_STYLE_A				\
    : (STYLE_FOUND_P (a, b, 1, SYNTAX_COMMENT_STYLE_B)	\
	 ? SYNTAX_COMMENT_STYLE_B			\
	 : 0)))

#define SYNTAX_COMMENT_MASK_END(a, b)			\
  ((STYLE_FOUND_P (a, b, 0, SYNTAX_COMMENT_STYLE_A)	\
   ? SYNTAX_COMMENT_STYLE_A				\
   : (STYLE_FOUND_P (a, b, 0, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B				\
      : 0)))

#define STYLE_FOUND_1CHAR_P(a, style)	\
  ((SYNTAX_COMMENT_BITS (a) & (style)))

#define SYNTAX_COMMENT_1CHAR_MASK(a)			\
  ((STYLE_FOUND_1CHAR_P (a, SYNTAX_COMMENT_STYLE_A)	\
   ? SYNTAX_COMMENT_STYLE_A				\
   : (STYLE_FOUND_1CHAR_P (a, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B				\
	 : 0)))

EXFUN(Fchar_syntax, 2);
EXFUN(Fforward_word, 2);

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
extern Lisp_Object Vstandard_syntax_table;

/* This array, indexed by a character, contains the syntax code which
   that character signifies (as a char).
   For example, (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern const unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it. */

extern unsigned char syntax_code_spec[];

Lisp_Object scan_lists(struct buffer *buf, Bufpos from, int count,
		       int depth, int sexpflag, int no_error);
int char_quoted(struct buffer *buf, Bufpos pos);

/* NOTE: This does not refer to the mirror table, but to the
   syntax table itself. */
Lisp_Object syntax_match(Lisp_Object table, Emchar ch);

extern int no_quit_in_re_search;
extern struct buffer *regex_emacs_buffer;

/* Target text (string or buffer), used for syntax-table properties. */
extern Lisp_Object regex_match_object;

void update_syntax_table(Lisp_Char_Table * ct);

/* The syntax table cache */

/*
   The *-single-property-change versions turn out to be unbearably slow.
   Do not enable them in a production or distribution version.
*/
#define NEXT_SINGLE_PROPERTY_CHANGE		0
#define PREVIOUS_SINGLE_PROPERTY_CHANGE		0

/* Test instruments, used in macros below.
   Define SYNTAX_CACHE_STATISTICS to enable them. */
/* #undef SYNTAX_CACHE_STATISTICS */

#ifdef SYNTAX_CACHE_STATISTICS
#define SYNTAX_CACHE_STATISTICS_REPORT_INTERVAL	100000

enum syntax_cache_statistics_functions {
	scs_no_function = -1,
	scs_find_context = 0,
	scs_find_defun_start,
	scs_scan_words,
	scs_Fforward_comment,
	scs_scan_lists,
	scs_Fbackward_prefix_characters,
	scs_scan_sexps_forward,
	scs_number_of_functions
};

/* keep this in synch with syntax.c */
extern char *syntax_cache_statistics_function_names[scs_number_of_functions];

struct syntax_cache_statistics {
	/* inits + misses_hi + misses_lo + #HITS = total_updates */
	int total_updates;
	int inits;
	int misses_lo;
	int misses_hi;
	int min_length;
	int max_length;
	double mean_length;
	double mean_length_on_miss;
	enum syntax_cache_statistics_functions this_function;
	int functions[scs_number_of_functions];
};

extern struct syntax_cache_statistics scs_statistics;

#define SCS_STATISTICS_SET_FUNCTION(fndx) scs_statistics.this_function = fndx
/* used in macros below */
#define SYNTAX_CACHE_STATISTICS_COUNT_INIT scs_statistics.inits++

#else

#define SCS_STATISTICS_SET_FUNCTION(fndx)
#define SYNTAX_CACHE_STATISTICS_COUNT_INIT

#endif				/* SYNTAX_CACHE_STATISTICS */

/* Theory of the syntax table cache

   This cache cooperates with but is conceptually different from the
   mirror table.  The mirror table precomputes (and caches, if you like)
   the syntax codes for characters in a given syntax table, taking into
   account possible inheritance from a table given by a parent text object.
   The syntax table cache checks for overriding tables defined by
   _subobjects_.

   This implementation defines the "subobjects" by _extent properties_.
   We may restrict them to _text_ properties.  There are two lookup
   styles for the cache, "single code" and "full table".  In the "single
   code" style, a given syntax code, kept in the `syntax_code' member, is
   applied to the entire range (#### check this).  In the "full table"
   style, a syntax table kept in the `current_syntax_table' member is
   checked for each character in the range.  If the flag `use_code' is
   non-zero, the "single code" is used, otherwise the "full table".

   The cache is valid for the range `[prev_change, next_change)' in the
   text object (buffer or string) `object'.

   If the current position is outside the range valid for the cache, the
   cache is updated by checking for the text property `syntax-table'.  If
   present, its value is either a syntax code or a syntax table, and the
   appropriate member and `use_code' are updated accordingly.  If absent
   or nil, the default syntax table from the `buffer' member is used.  The
   extent of the property is used to reinitialize the cache's validity
   range.  (We would like to improve this by checking the property value
   against `old_prop', and if the same, extend the validity range of the
   cache by the extent of the property.)

   Note: the values Qt and Qnil for `object' are not supported in this
   implementation.  GNU Emacs uses them for reasons not yet (####) clear.
*/

extern int lookup_syntax_properties;

struct syntax_cache {
	int use_code;		/* Whether to use syntax_code
				   or current_syntax_table. */
	struct buffer *buffer;	/* The buffer providing the default
				   syntax table to the cache. */
	Lisp_Object object;	/* The buffer or string the current
				   syntax cache applies to. */
	enum syntaxcode syntax_code;	/* Syntax code of current char. */
	Lisp_Object current_syntax_table;	/* Syntax table for current pos. */
	Lisp_Object old_prop;	/* Syntax-table prop at prev pos. */

	Bufpos next_change;	/* Position of the next extent
				   change. */
	Bufpos prev_change;	/* Position of the previous
				   extent change. */
};
extern struct syntax_cache syntax_cache;

/*
   The macros below handle the internal structure of the cache.
   ALWAYS USE THE MACROS TO MANIPULATE THE CACHE.

   o Use the SETUP_SYNTAX_CACHE* macros to set the object and buffer members.

     OBJECT is either a Lisp buffer or a Lisp string.  BUFFER is a
     pointer to struct buffer.  If OBJECT is a buffer, it must refer to
     BUFFER.  If OBJECT is a string, then BUFFER will supply the default
     syntax table when the `syntax-table' property is nil.

     For convenience and backward compatibility, the values Qt and Qnil are
     accepted for OBJECT.  These are taken to refer to the current buffer,
     and that substitution is made immediately.  The value Qt is treated
     specially in the *BYTE_TO_CHAR macros below.  This appears (####) to
     be a GNU kludge related to `enable-multibyte-characters' and was used
     only in dired.c.

     FROM is the starting character position in OBJECT.
     COUNT is currently used only as a flag.  If positive, we are proceeding
     forward through OBJECT, otherwise in reverse.

   o All other members are updated using the update_syntax_cache
     function, normally wrapped in the UPDATE_SYNTAX_CACHE* macros.
*/

void update_syntax_cache(int pos, int count);

/* in one example the high misses vastly outweigh the low ones
   seems plausible, since we typically are moving forward through the buffer */
#define UPDATE_SYNTAX_CACHE_INTERNAL(pos, dir)	\
   ((lookup_syntax_properties &&		\
     (pos >= syntax_cache.next_change ||		\
      pos < syntax_cache.prev_change))		\
    ? (update_syntax_cache ((pos), dir), 1)	\
    : 0)

/* In the current implementation, all of the following are identical. */
/* Make syntax cache state good for CHARPOS, assuming it is
   currently good for a position before CHARPOS.  */
#define UPDATE_SYNTAX_CACHE_FORWARD(pos) UPDATE_SYNTAX_CACHE_INTERNAL(pos, 1)

/* Make syntax cache state good for CHARPOS, assuming it is
   currently good for a position after CHARPOS.  */
#define UPDATE_SYNTAX_CACHE_BACKWARD(pos) UPDATE_SYNTAX_CACHE_INTERNAL(pos, -1)

/* Make syntax cache state good for CHARPOS */
#define UPDATE_SYNTAX_CACHE(pos) UPDATE_SYNTAX_CACHE_INTERNAL(pos, 0)

#define SYNTAX_FROM_CACHE(table, c)			\
   SYNTAX_FROM_CODE (SYNTAX_CODE_FROM_CACHE (table, c))

#define SYNTAX_CODE_FROM_CACHE(table, c)				\
  ( syntax_cache.use_code						\
      ? syntax_cache.syntax_code					\
      : SYNTAX_CODE (XCHAR_TABLE (syntax_cache.current_syntax_table),	\
		     c)							\
 )

/* Convert the byte offset BYTEPOS into a character position,
   for the object recorded in syntax_cache with SETUP_SYNTAX_CACHE*.

   The value is meant for use in the UPDATE_SYNTAX_CACHE... macros.
   These macros do nothing when lookup_syntax_properties is 0,
   so we return 0 in that case, for speed.

   The default case does no conversion; this seems (####) to be an
   evil hangover from GNU Emacs. */
#define SYNTAX_CACHE_OBJECT_BYTE_TO_CHAR(obj, buf, bytepos)	\
  (! lookup_syntax_properties					\
   ? 0								\
   : STRINGP (obj)						\
   ? bytecount_to_charcount (XSTRING_DATA (obj), bytepos)	\
   : (BUFFERP (obj) || NILP (obj))				\
   ? bytind_to_bufpos (buf, bytepos + BI_BUF_BEGV (buf))	\
   : (bytepos))

#define SYNTAX_CACHE_BYTE_TO_CHAR(bytepos)				       \
  SYNTAX_CACHE_OBJECT_BYTE_TO_CHAR (syntax_cache.object, syntax_cache.buffer,  \
				    (bytepos))

#define SETUP_SYNTAX_CACHE(FROM, COUNT)				\
  SETUP_SYNTAX_CACHE_FOR_BUFFER (current_buffer, (FROM), (COUNT))

#define SETUP_SYNTAX_CACHE_FOR_BUFFER(BUFFER, FROM, COUNT)	\
  SETUP_SYNTAX_CACHE_FOR_OBJECT (Qnil, (BUFFER), (FROM), (COUNT))

#define SETUP_SYNTAX_CACHE_FOR_OBJECT(OBJECT, BUFFER, FROM, COUNT)	\
  do {									\
    syntax_cache.buffer = (BUFFER);					\
    syntax_cache.object = (OBJECT);					\
    if (NILP (syntax_cache.object))					\
      {									\
	XSETBUFFER (syntax_cache.object, syntax_cache.buffer);		\
      }									\
    else if (EQ (syntax_cache.object, Qt))				\
      {									\
	XSETBUFFER (syntax_cache.object, syntax_cache.buffer);		\
      }									\
    else if (STRINGP (syntax_cache.object))				\
      {									\
	/* do nothing */;						\
      }									\
    else if (BUFFERP (syntax_cache.object))				\
      {									\
	syntax_cache.buffer = XBUFFER (syntax_cache.object);		\
      }									\
    else								\
      {									\
	/* OBJECT must be buffer/string/t/nil */			\
	assert(0);							\
      }									\
    syntax_cache.current_syntax_table					\
      = syntax_cache.buffer->mirror_syntax_table;			\
    syntax_cache.use_code = 0;						\
    if (lookup_syntax_properties)					\
      {									\
	SYNTAX_CACHE_STATISTICS_COUNT_INIT;				\
	update_syntax_cache ((FROM) + ((COUNT) > 0 ? 0 : -1), (COUNT));	\
      }									\
  } while (0)

#define SYNTAX_CODE_PREFIX(c) \
  ((c >> 7) & 1)

#define SYNTAX_CODE_COMMENT_BITS(c) \
  ((c >> 16) &0xff)

#define SYNTAX_CODES_START_P(a, b)                                    \
  (((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START) >> 2)    \
   & (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_START))

#define SYNTAX_CODES_END_P(a, b)                                    \
  (((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END) >> 2)    \
   & (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_END))

#define SYNTAX_CODES_COMMENT_MASK_START(a, b)			\
  (SYNTAX_CODES_MATCH_START_P (a, b, SYNTAX_COMMENT_STYLE_A)	\
   ? SYNTAX_COMMENT_STYLE_A					\
   : (SYNTAX_CODES_MATCH_START_P (a, b, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B					\
      : 0))
#define SYNTAX_CODES_COMMENT_MASK_END(a, b)			\
  (SYNTAX_CODES_MATCH_END_P (a, b, SYNTAX_COMMENT_STYLE_A)	\
   ? SYNTAX_COMMENT_STYLE_A					\
   : (SYNTAX_CODES_MATCH_END_P (a, b, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B					\
      : 0))

#define SYNTAX_CODE_START_FIRST_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START)

#define SYNTAX_CODE_START_SECOND_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_SECOND_CHAR_START)

#define SYNTAX_CODE_END_FIRST_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END)

#define SYNTAX_CODE_END_SECOND_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_SECOND_CHAR_END)

#define SYNTAX_CODES_MATCH_START_P(a, b, mask)				\
  ((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START & (mask))	\
   && (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_START & (mask)))

#define SYNTAX_CODES_MATCH_END_P(a, b, mask)				\
  ((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END & (mask))	\
   && (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_END & (mask)))

#define SYNTAX_CODE_MATCHES_1CHAR_P(a, mask)	\
  ((SYNTAX_CODE_COMMENT_BITS (a) & (mask)))

#define SYNTAX_CODE_COMMENT_1CHAR_MASK(a)			\
  ((SYNTAX_CODE_MATCHES_1CHAR_P (a, SYNTAX_COMMENT_STYLE_A)	\
    ? SYNTAX_COMMENT_STYLE_A					\
    : (SYNTAX_CODE_MATCHES_1CHAR_P (a, SYNTAX_COMMENT_STYLE_B)	\
       ? SYNTAX_COMMENT_STYLE_B					\
       : 0)))

#if 0
/* These are the things that need to be #defined away to create a
   no syntax-table property version. */

/* This should be entirely encapsulated in macros
#define update_syntax_cache(pos, count)
*/
#define lookup_syntax_properties 0

#define SETUP_SYNTAX_CACHE(FROM, COUNT)
#define SETUP_SYNTAX_CACHE_FOR_BUFFER(BUFFER, FROM, COUNT)
#define SETUP_SYNTAX_CACHE_FOR_OBJECT(OBJECT, BUFFER, FROM, COUNT)
#define UPDATE_SYNTAX_CACHE_FORWARD(pos)
#define UPDATE_SYNTAX_CACHE_BACKWARD(pos)
#define UPDATE_SYNTAX_CACHE(pos)

#define SYNTAX_FROM_CACHE SYNTAX
#define SYNTAX_CODE_FROM_CACHE SYNTAX_CODE

#define SYNTAX_CACHE_BYTE_TO_CHAR(x) 0

/* cache statistics */
#define SCS_STATISTICS_SET_FUNCTION(fndx)
#define SYNTAX_CACHE_STATISTICS_COUNT_INIT

#endif				/* 0 */
#endif				/* INCLUDED_syntax_h_ */
