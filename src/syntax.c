/* XEmacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985-1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.28. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "syntax.h"
#include "extents.h"

/* Here is a comment from Ken'ichi HANDA <handa@etl.go.jp>
   explaining the purpose of the Sextword syntax category:

Japanese words are not separated by spaces, which makes finding word
boundaries very difficult.  Theoretically it's impossible without
using natural language processing techniques.  But, by defining
pseudo-words as below (much simplified for letting you understand it
easily) for Japanese, we can have a convenient forward-word function
for Japanese.

	A Japanese word is a sequence of characters that consists of
	zero or more Kanji characters followed by zero or more
	Hiragana characters.

Then, the problem is that now we can't say that a sequence of
word-constituents makes up a WORD.  For instance, both Hiragana "A"
and Kanji "KAN" are word-constituents but the sequence of these two
letters can't be a single word.

So, we introduced Sextword for Japanese letters.  A character of
Sextword is a word-constituent but a word boundary may exist between
two such characters.  */

/* Mule 2.4 doesn't seem to have Sextword - I'm removing it -- mrb */
/* Recovered by tomo */

#define ST_COMMENT_STYLE 0x101
#define ST_STRING_STYLE  0x102

Lisp_Object Qsyntax_table;
int lookup_syntax_properties;

Lisp_Object Qsyntax_table_p;

int words_include_escapes;

int parse_sexp_ignore_comments;

/* The following two variables are provided to tell additional information
   to the regex routines.  We do it this way rather than change the
   arguments to re_search_2() in an attempt to maintain some call
   compatibility with other versions of the regex code. */

/* Tell the regex routines not to QUIT.  Normally there is a QUIT
   each iteration in re_search_2(). */
int no_quit_in_re_search;

/* Tell the regex routines which buffer to access for SYNTAX() lookups
   and the like. */
struct buffer *regex_emacs_buffer;

/* In Emacs, this is the string or buffer in which we
   are matching.  It is used for looking up syntax properties.	*/
Lisp_Object regex_match_object;

Lisp_Object Vstandard_syntax_table;

Lisp_Object Vsyntax_designator_chars_string;

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
{
  int depth;		/* Depth at end of parsing */
  Emchar instring;	/* -1 if not within string, else desired terminator */
  int incomment;	/* Nonzero if within a comment at end of parsing */
  int comstyle;		/* comment style a=0, or b=1, or ST_COMMENT_STYLE */
  int quoted;		/* Nonzero if just after an escape char at end of
			   parsing */
  Bufpos thislevelstart;/* Char number of most recent start-of-expression
                           at current level */
  Bufpos prevlevelstart;/* Char number of start of containing expression */
  Bufpos location;	/* Char number at which parsing stopped */
  int mindepth;		/* Minimum depth seen while scanning  */
  Bufpos comstr_start;	/* Position just after last comment/string starter
			   (if the 'syntax-table text property is not
			   supported, used only for comment starts) */
  Lisp_Object levelstarts;	/* Char numbers of starts-of-expression
                                   of levels (starting from outermost).  */
};

/* These variables are a cache for finding the start of a defun.
   find_start_pos    is the place for which the defun start was found.
   find_start_value  is the defun start position found for it.
   find_start_buffer is the buffer it was found in.
   find_start_begv   is the BEGV value when it was found.
   find_start_modiff is the value of MODIFF when it was found.  */

static Bufpos find_start_pos;
static Bufpos find_start_value;
static struct buffer *find_start_buffer;
static Bufpos find_start_begv;
static int find_start_modiff;

/* Find a defun-start that is the last one before POS (or nearly the last).
   We record what we find, so that another call in the same area
   can return the same value right away.  */

static Bufpos
find_defun_start (struct buffer *buf, Bufpos pos)
{
  Bufpos tem;

  /* Use previous finding, if it's valid and applies to this inquiry.  */
  if (buf == find_start_buffer
      /* Reuse the defun-start even if POS is a little farther on.
	 POS might be in the next defun, but that's ok.
	 Our value may not be the best possible, but will still be usable.  */
      && pos <= find_start_pos + 1000
      && pos >= find_start_value
      && BUF_BEGV (buf) == find_start_begv
      && BUF_MODIFF (buf) == find_start_modiff)
    return find_start_value;

  /* Back up to start of line.  */
  tem = find_next_newline (buf, pos, -1);

  SCS_STATISTICS_SET_FUNCTION (scs_find_defun_start);
  SETUP_SYNTAX_CACHE (tem, 1);
  while (tem > BUF_BEGV (buf))
    {
      UPDATE_SYNTAX_CACHE_BACKWARD(tem);

      /* Open-paren at start of line means we found our defun-start.  */
      if (SYNTAX_FROM_CACHE (mirrortab, BUF_FETCH_CHAR (buf, tem)) == Sopen)
	break;
      /* Move to beg of previous line.  */
      tem = find_next_newline (buf, tem, -2);
    }

  /* Record what we found, for the next try.  */
  find_start_value  = tem;
  find_start_buffer = buf;
  find_start_modiff = BUF_MODIFF (buf);
  find_start_begv   = BUF_BEGV (buf);
  find_start_pos    = pos;

  return find_start_value;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, 1, 1, 0, /*
Return t if OBJECT is a syntax table.
Any vector of 256 elements will do.
*/
       (object))
{
  return (CHAR_TABLEP (object)
	  && XCHAR_TABLE_TYPE (object) == CHAR_TABLE_TYPE_SYNTAX)
    ? Qt : Qnil;
}

static Lisp_Object
check_syntax_table (Lisp_Object obj, Lisp_Object default_)
{
  if (NILP (obj))
    obj = default_;
  while (NILP (Fsyntax_table_p (obj)))
    obj = wrong_type_argument (Qsyntax_table_p, obj);
  return obj;
}

DEFUN ("syntax-table", Fsyntax_table, 0, 1, 0, /*
Return the current syntax table.
This is the one specified by the current buffer, or by BUFFER if it
is non-nil.
*/
       (buffer))
{
  return decode_buffer (buffer, 0)->syntax_table;
}

DEFUN ("standard-syntax-table", Fstandard_syntax_table, 0, 0, 0, /*
Return the standard syntax table.
This is the one used for new buffers.
*/
       ())
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, 0, 1, 0, /*
Return a new syntax table which is a copy of SYNTAX-TABLE.
SYNTAX-TABLE defaults to the standard syntax table.
*/
       (syntax_table))
{
  if (NILP (Vstandard_syntax_table))
    return Fmake_char_table (Qsyntax);

  syntax_table = check_syntax_table (syntax_table, Vstandard_syntax_table);
  return Fcopy_char_table (syntax_table);
}

DEFUN ("set-syntax-table", Fset_syntax_table, 1, 2, 0, /*
Select SYNTAX-TABLE as the new syntax table for BUFFER.
BUFFER defaults to the current buffer if omitted.
*/
       (syntax_table, buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  syntax_table = check_syntax_table (syntax_table, Qnil);
  buf->syntax_table = syntax_table;
  buf->mirror_syntax_table = XCHAR_TABLE (syntax_table)->mirror_table;
  /* Indicate that this buffer now has a specified syntax table.  */
  buf->local_var_flags |= XINT (buffer_local_flags.syntax_table);
  return syntax_table;
}

/* The current syntax state */
struct syntax_cache syntax_cache;


/* 
   Update syntax_cache to an appropriate setting for position POS

   The sign of COUNT gives the relative position of POS wrt the
   previously valid interval.  (not currently used)

   `syntax_cache.*_change' are the next and previous positions at
   which syntax_code and c_s_t will need to be recalculated.

   #### Currently this code uses 'get-char-property', which will
   return the "last smallest" extent at a given position. In cases
   where overlapping extents are defined, this code will simply use
   whatever is returned by get-char-property.

   It might be worth it at some point to merge provided syntax tables
   outward to the current buffer.

   sjt sez:
   This implementation has to rather inefficient, since it looks at
   next-extent-change, and a heavily font-locked buffer will be rife
   with irrelevant extents.  We could do a sledgehammer check on this
   by looking at the distribution of extent lengths.  Also count up
   cache hits and misses.

   If we assume that syntax-table is a _text_ property (which also
   deals with the issue of overlapping syntax-table properties), then
   the following strategy recommends itself
     o give the syntax cache a `valid' flag, to be reset whenever a
       syntax-table property is added, changed, or removed; this could
       be done by setting syntax_cache's prev_change > next_change
       (but not compatible with using extents/markers here); if it's a
       Lisp variable, doing it in Lisp shouldn't be too inefficient
     o lazily initialize the cache whenever the object being examined
       differs from the object the cache currently refers to
     o by using {previous,next-single-property-change} we should be
       able to get much bigger cache intervals (in most cases, the
       whole buffer)
     o cache markers instead of positions so the mere insertion or
       deletion of text doesn't invalidate the cache, only if it
       involves a syntax-table property (we could also cache the
       extents carrying the syntax-table text-property; that gives us
       another check for invalid cache).

   If I understand this correctly, we need to invalidate the cache in the
   following cases:
     o If the referenced object changes (it's a global cache)
     o If there are insertions or deletions of text (the positions are
       absolute; fix: use markers or an extent instead?)
     o If the syntax-table property is altered == added and different or
       removed and the same (fix: probably computable from range overlap,
       but is it worth it?  would interact with ins/del); this includes
       detachment of extents with the same value (but only the boundary
       extents, as otherwise the range coalesces across the deletion point)
       and attachment of extents with a different value
   Note: the above looks a lot like what Ben has implemented in 21.5, but
   he goes one better by making the cache buffer-local.

   Note: cperl mode uses the text property API, not extents/overlays.
*/

#ifdef SYNTAX_CACHE_STATISTICS
struct syntax_cache_statistics scs_statistics =
  { 0, 0, 0, 0, -1, -1, 0.0, 0.0, scs_no_function};

char* syntax_cache_statistics_function_names[scs_number_of_functions] = {
  "find_context",
  "find_defun_start",
  "scan_words",
  "Fforward_comment",
  "scan_lists",
  "Fbackward_prefix_characters",
  "scan_sexps_forward"
};
#endif /* SYNTAX_CACHE_STATISTICS */

void
update_syntax_cache (int pos, int count)
{
  Lisp_Object tmp_table;

#ifdef SYNTAX_CACHE_STATISTICS
  if (scs_statistics.total_updates == 0)
    {
      int i;
      for (i = 0; i < scs_number_of_functions; ++i)
	scs_statistics.functions[i] = 0;
    }
  if (syntax_cache.prev_change > syntax_cache.next_change)
    scs_statistics.inits++;
  else if (pos < syntax_cache.prev_change)
    scs_statistics.misses_lo++;
  else if (pos >= syntax_cache.next_change)
    scs_statistics.misses_hi++;
#endif /* SYNTAX_CACHE_STATISTICS */

  /* #### Since font-lock undoes any narrowing, maybe the BUF_ZV and
     BUF_BEGV below should be BUF_Z and BUF_BEG respectively? */
  if (BUFFERP (syntax_cache.object))
    {
      int get_change_before = pos + 1;

      tmp_table = Fget_char_property (make_int(pos), Qsyntax_table,
				      syntax_cache.object, Qnil);
#if NEXT_SINGLE_PROPERTY_CHANGE
      /* #### shouldn't we be using BUF_BEGV here? */
      syntax_cache.next_change =
	XINT (Fnext_single_property_change
	      (make_int (pos > 0 ? pos : 1), Qsyntax_table,
	       syntax_cache.object, make_int (BUF_ZV (syntax_cache.buffer))));
#else
      syntax_cache.next_change =
	XINT (Fnext_extent_change (make_int (pos > 0 ? pos : 1),
				   syntax_cache.object));
#endif

      /* #### shouldn't we be using BUF_BEGV here? */
      if (get_change_before < 1)
	get_change_before = 1;
      else if (get_change_before > BUF_ZV (syntax_cache.buffer))
	get_change_before = BUF_ZV (syntax_cache.buffer);

#if PREVIOUS_SINGLE_PROPERTY_CHANGE
      /* #### shouldn't we be using BUF_BEGV here? */
      syntax_cache.prev_change =
	XINT (Fprevious_single_property_change
	      (make_int (get_change_before), Qsyntax_table,
	       syntax_cache.object, make_int(1)));
#else
      syntax_cache.prev_change =
	XINT (Fprevious_extent_change (make_int (get_change_before),
					   syntax_cache.object));
#endif
    }
  else if (STRINGP (syntax_cache.object))
    {
      int get_change_before = pos + 1;

      tmp_table = Fget_char_property (make_int(pos), Qsyntax_table,
				      syntax_cache.object, Qnil);
#if NEXT_SINGLE_PROPERTY_CHANGE
      /* #### shouldn't we be using BUF_BEGV here? */
      syntax_cache.next_change =
	XINT (Fnext_single_property_change
	      (make_int (pos >= 0 ? pos : 0), Qsyntax_table,
	       syntax_cache.object,
	       make_int(XSTRING_LENGTH(syntax_cache.object))));
#else
      syntax_cache.next_change =
	XINT (Fnext_extent_change (make_int (pos >= 0 ? pos : 0),
				   syntax_cache.object));
#endif

      if (get_change_before < 0)
	get_change_before = 0;
      else if (get_change_before > XSTRING_LENGTH(syntax_cache.object))
	get_change_before = XSTRING_LENGTH(syntax_cache.object);

#if PREVIOUS_SINGLE_PROPERTY_CHANGE
      syntax_cache.prev_change =
	XINT (Fprevious_single_property_change
	      (make_int (get_change_before), Qsyntax_table,
	       syntax_cache.object, make_int(0)));
#else
      syntax_cache.prev_change =
	XINT (Fprevious_extent_change (make_int (get_change_before),
				       syntax_cache.object));
#endif
    }
  else
    {
      tmp_table = Qnil;	/* silence compiler */
      /* Always aborts.  #### Is there another sensible thing to do here? */
      assert (BUFFERP (syntax_cache.object) || STRINGP (syntax_cache.object));
    }

  if (EQ (Fsyntax_table_p (tmp_table), Qt))
    {
      syntax_cache.use_code = 0;
      syntax_cache.current_syntax_table =
	XCHAR_TABLE (tmp_table)->mirror_table;
    } 
  else if (CONSP (tmp_table) && INTP (XCAR (tmp_table)))
    {
      syntax_cache.use_code = 1;
      syntax_cache.syntax_code = (enum syntaxcode) XINT (XCAR (tmp_table));
    }
  else 
    {
      syntax_cache.use_code = 0;
      syntax_cache.current_syntax_table =
	syntax_cache.buffer->mirror_syntax_table;
    }

#ifdef SYNTAX_CACHE_STATISTICS
  {
    int length = syntax_cache.next_change - syntax_cache.prev_change;
    int misses = scs_statistics.misses_lo +
      scs_statistics.misses_hi + scs_statistics.inits;
      
    if (scs_statistics.min_length == -1 || scs_statistics.min_length > length)
      scs_statistics.min_length = length;
    if (scs_statistics.max_length == -1 || scs_statistics.max_length < length)
	  scs_statistics.max_length = length;
    scs_statistics.mean_length_on_miss =
      ((misses - 1) * scs_statistics.mean_length_on_miss + length) / misses;
  }

  scs_statistics.mean_length
    = scs_statistics.total_updates*scs_statistics.mean_length
      + syntax_cache.next_change - syntax_cache.prev_change;
  scs_statistics.total_updates++;
  scs_statistics.mean_length /= scs_statistics.total_updates;

  if (scs_statistics.this_function != scs_no_function)
    {
      scs_statistics.functions[scs_statistics.this_function]++;
      scs_statistics.this_function = scs_no_function;
    }

  if (!(scs_statistics.total_updates % SYNTAX_CACHE_STATISTICS_REPORT_INTERVAL))
    {
      fprintf (stderr, "Syntax cache stats:\n  ");
      fprintf (stderr, "updates %d, inits %d, misses low %d, misses high %d,",
	       scs_statistics.total_updates, scs_statistics.inits,
	       scs_statistics.misses_lo, scs_statistics.misses_hi);
      fprintf (stderr, "\n ");

#define REPORT_FUNCTION(i)				\
  fprintf (stderr, " %s %d,",				\
	   syntax_cache_statistics_function_names[i],	\
	   scs_statistics.functions[i]);

      REPORT_FUNCTION(scs_find_context);
      REPORT_FUNCTION(scs_find_defun_start);
      REPORT_FUNCTION(scs_scan_words);
      REPORT_FUNCTION(scs_Fforward_comment);
      fprintf (stderr, "\n ");
      REPORT_FUNCTION(scs_scan_lists);
      REPORT_FUNCTION(scs_Fbackward_prefix_characters);
      REPORT_FUNCTION(scs_scan_sexps_forward);
#undef REPORT_FUNCTION

      fprintf (stderr, "\n  min length %d, max length %d,",
	       scs_statistics.min_length, scs_statistics.max_length);
      fprintf (stderr, "\n  mean length %.1f, mean length on miss %.1f\n",
	       scs_statistics.mean_length,
	       scs_statistics.mean_length_on_miss);
    }
#endif /* SYNTAX_CACHE_STATISTICS */
}


/* Convert a letter which signifies a syntax code
   into the code it signifies.
   This is used by modify-syntax-entry, and other things. */

const unsigned char syntax_spec_code[0400] =
{ 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  (char) Swhitespace, 0377, (char) Sstring, 0377,
      (char) Smath, 0377, 0377, (char) Squote,
  (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
  (char) Sinherit, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A ... */
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
  0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
  0377, 0377, 0377, 0377, (char) Sstring_fence, 0377, 0377, 0377
};

const unsigned char syntax_code_spec[] =  " .w_()'\"$\\/<>@!|";

DEFUN ("syntax-designator-chars", Fsyntax_designator_chars, 0, 0, 0, /*
Return a string of the recognized syntax designator chars.
The chars are ordered by their internal syntax codes, which are
numbered starting at 0.
*/
       ())
{
  return Vsyntax_designator_chars_string;
}

DEFUN ("char-syntax", Fchar_syntax, 1, 2, 0, /*
Return the syntax code of CHARACTER, described by a character.
For example, if CHARACTER is a word constituent,
the character `?w' is returned.
The characters that correspond to various syntax codes
are listed in the documentation of `modify-syntax-entry'.
Optional second argument SYNTAX-TABLE defaults to the current buffer's
syntax table.
*/
       (character, syntax_table))
{
  Lisp_Char_Table *mirrortab;

  if (NILP (character))
    {
      character = make_char ('\000');
    }
  CHECK_CHAR_COERCE_INT (character);
  syntax_table = check_syntax_table (syntax_table, current_buffer->syntax_table);
  mirrortab = XCHAR_TABLE (XCHAR_TABLE (syntax_table)->mirror_table);
  return make_char (syntax_code_spec[(int) SYNTAX (mirrortab, XCHAR (character))]);
}

#ifdef MULE

enum syntaxcode
charset_syntax (struct buffer *buf, Lisp_Object charset, int *multi_p_out)
{
  *multi_p_out = 1;
  /* #### get this right */
  return Spunct;
}

#endif

Lisp_Object
syntax_match (Lisp_Object syntax_table, Emchar ch)
{
  Lisp_Object code = XCHAR_TABLE_VALUE_UNSAFE (syntax_table, ch);
  Lisp_Object code2 = code;

  if (CONSP (code))
    code2 = XCAR (code);
  if (SYNTAX_FROM_CODE (XINT (code2)) == Sinherit)
    code = XCHAR_TABLE_VALUE_UNSAFE (Vstandard_syntax_table, ch);

  return CONSP (code) ? XCDR (code) : Qnil;
}

DEFUN ("matching-paren", Fmatching_paren, 1, 2, 0, /*
Return the matching parenthesis of CHARACTER, or nil if none.
Optional second argument SYNTAX-TABLE defaults to the current buffer's
syntax table.
*/
       (character, syntax_table))
{
  Lisp_Char_Table *mirrortab;
  int code;

  CHECK_CHAR_COERCE_INT (character);
  syntax_table = check_syntax_table (syntax_table, current_buffer->syntax_table);
  mirrortab = XCHAR_TABLE (XCHAR_TABLE (syntax_table)->mirror_table);
  code = SYNTAX (mirrortab, XCHAR (character));
  if (code == Sopen || code == Sclose || code == Sstring)
    return syntax_match (syntax_table, XCHAR (character));
  return Qnil;
}



#ifdef MULE
/* Return 1 if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order, else return 0.
   There is no word boundary between two word-constituent ASCII
   characters.  */
#define WORD_BOUNDARY_P(c1, c2)			\
  (!(CHAR_ASCII_P (c1) && CHAR_ASCII_P (c2))	\
   && word_boundary_p (c1, c2))

extern int word_boundary_p (Emchar c1, Emchar c2);
#endif

/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

Bufpos
scan_words (struct buffer *buf, Bufpos from, int count)
{
  Bufpos limit = count > 0 ? BUF_ZV (buf) : BUF_BEGV (buf);
  Emchar ch0, ch1;
  enum syntaxcode code;

  SCS_STATISTICS_SET_FUNCTION (scs_scan_words);
  SETUP_SYNTAX_CACHE_FOR_BUFFER (buf, from, count);

  /* #### is it really worth it to hand expand both cases? JV */
  while (count > 0)
    {
      QUIT;

      while (1)
	{
	  if (from == limit)
	    return 0;

	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  ch0 = BUF_FETCH_CHAR (buf, from);
	  code = SYNTAX_FROM_CACHE (mirrortab, ch0);

	  from++;
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}

      QUIT;

      while (from != limit)
	{
	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  ch1 = BUF_FETCH_CHAR (buf, from);
	  code = SYNTAX_FROM_CACHE (mirrortab, ch1);
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword
#ifdef MULE
		|| WORD_BOUNDARY_P (ch0, ch1)
#endif
		)
	      break;
#ifdef MULE
	  ch0 = ch1;
#endif
	  from++;
	}
      count--;
    }

  while (count < 0)
    {
      QUIT;

      while (1)
	{
	  if (from == limit)
	    return 0;

	  UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
	  ch1 = BUF_FETCH_CHAR (buf, from - 1);
	  code = SYNTAX_FROM_CACHE (mirrortab, ch1);
	  from--;

	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}

      QUIT;

      while (from != limit)
	{
	  UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
	  ch0 = BUF_FETCH_CHAR (buf, from - 1);
	  code = SYNTAX_FROM_CACHE (mirrortab, ch0);

	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword
#ifdef MULE
		|| WORD_BOUNDARY_P (ch0, ch1)
#endif
		)
	      break;
#ifdef MULE
	  ch1 = ch0;
#endif
	  from--;
	}
      count++;
    }

  return from;
}

DEFUN ("forward-word", Fforward_word, 0, 2, "_p", /*
Move point forward COUNT words (backward if COUNT is negative).
Normally t is returned, but if an edge of the buffer is reached,
point is left there and nil is returned.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.

COUNT defaults to 1, and BUFFER defaults to the current buffer.
*/
       (count, buffer))
{
  Bufpos val;
  struct buffer *buf = decode_buffer (buffer, 0);
  EMACS_INT n;

  if (NILP (count))
    n = 1;
  else
    {
      CHECK_INT (count);
      n = XINT (count);
    }

  val = scan_words (buf, BUF_PT (buf), n);
  if (val)
    {
      BUF_SET_PT (buf, val);
      return Qt;
    }
  else
    {
      BUF_SET_PT (buf, n > 0 ? BUF_ZV (buf) : BUF_BEGV (buf));
      return Qnil;
    }
}

static void scan_sexps_forward (struct buffer *buf,
				struct lisp_parse_state *,
				Bufpos from, Bufpos end,
				int targetdepth, int stopbefore,
				Lisp_Object oldstate,
				int commentstop);

static int
find_start_of_comment (struct buffer *buf, Bufpos from, Bufpos stop,
		       int comstyle)
{
  Emchar c;
  enum syntaxcode code;

  /* Look back, counting the parity of string-quotes,
     and recording the comment-starters seen.
     When we reach a safe place, assume that's not in a string;
     then step the main scan to the earliest comment-starter seen
     an even number of string quotes away from the safe place.

     OFROM[I] is position of the earliest comment-starter seen
     which is I+2X quotes from the comment-end.
     PARITY is current parity of quotes from the comment end.  */
  int parity = 0;
  Emchar my_stringend = 0;
  int string_lossage = 0;
  Bufpos comment_end = from;
  Bufpos comstart_pos = 0;
  int comstart_parity = 0;
  int styles_match_p = 0;
  /* mask to match comment styles against; for ST_COMMENT_STYLE, this
     will get set to SYNTAX_COMMENT_STYLE_B, but never get checked */
  int mask = comstyle ? SYNTAX_COMMENT_STYLE_B : SYNTAX_COMMENT_STYLE_A;

  /* At beginning of range to scan, we're outside of strings;
     that determines quote parity to the comment-end.  */
  while (from != stop)
    {
      int syncode;

      /* Move back and examine a character.  */
      from--;
      UPDATE_SYNTAX_CACHE_BACKWARD (from);

      c = BUF_FETCH_CHAR (buf, from);
      syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
      code = SYNTAX_FROM_CODE (syncode);

      /* is this a 1-char comment end sequence? if so, try
	 to see if style matches previously extracted mask */
      if (code == Sendcomment)
	{
	  /* MT had SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode) & mask
	     but (as a Boolean) that's just a complicated way to write: */
	  styles_match_p = SYNTAX_CODE_MATCHES_1CHAR_P (syncode, mask);
	}

      /* or are we looking at a 1-char comment start sequence
	 of the style matching mask? */
      else if (code == Scomment)
	{
	  styles_match_p = SYNTAX_CODE_MATCHES_1CHAR_P (syncode, mask);
	}

      /* otherwise, is this a 2-char comment end or start sequence? */
      else if (from > stop)
	do
	  {
	    /* 2-char comment end sequence? */
	    if (SYNTAX_CODE_END_SECOND_P (syncode))
	      {
		int prev_syncode;
		UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
		prev_syncode =
		  SYNTAX_CODE_FROM_CACHE (mirrortab,
					  BUF_FETCH_CHAR (buf, from - 1));

		if (SYNTAX_CODES_END_P (prev_syncode, syncode))
		  {
		    code = Sendcomment;
		    styles_match_p =
		      SYNTAX_CODES_MATCH_END_P (prev_syncode, syncode, mask);
		    from--;
		    UPDATE_SYNTAX_CACHE_BACKWARD (from);
		    c = BUF_FETCH_CHAR (buf, from);

		    /* Found a comment-end sequence, so skip past the
		       check for a comment-start */
		    break;
		  }
	      }

	    /* 2-char comment start sequence? */
	    if (SYNTAX_CODE_START_SECOND_P (syncode))
	      {
		int prev_syncode;
		UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
		prev_syncode =
		  SYNTAX_CODE_FROM_CACHE (mirrortab,
					  BUF_FETCH_CHAR (buf, from - 1));

		if (SYNTAX_CODES_START_P (prev_syncode, syncode))
		  {
		    code = Scomment;
		    styles_match_p =
		      SYNTAX_CODES_MATCH_START_P (prev_syncode, syncode, mask);
		    from--;
		    UPDATE_SYNTAX_CACHE_BACKWARD (from);
		    c = BUF_FETCH_CHAR (buf, from);
		  }
	      }
	  } while (0);

      /* Ignore escaped characters.  */
      if (char_quoted (buf, from))
	continue;

      /* Track parity of quotes.  */
      if (code == Sstring)
	{
	  parity ^= 1;
	  if (my_stringend == 0)
	    my_stringend = c;
	  /* If we have two kinds of string delimiters.
	     There's no way to grok this scanning backwards.  */
	  else if (my_stringend != c)
	    string_lossage = 1;
	}

      if (code == Sstring_fence || code == Scomment_fence)
	{
	  parity ^= 1;
	  if (my_stringend == 0)
	    my_stringend =
	      code == Sstring_fence ? ST_STRING_STYLE : ST_COMMENT_STYLE;
	  /* If we have two kinds of string delimiters.
	     There's no way to grok this scanning backwards.  */
	  else if (my_stringend != (code == Sstring_fence 
				    ? ST_STRING_STYLE : ST_COMMENT_STYLE))
	    string_lossage = 1;
	}

      /* Record comment-starters according to that
	 quote-parity to the comment-end.  */
      if (code == Scomment && styles_match_p)
	{
	  comstart_parity = parity;
	  comstart_pos = from;
	}

      /* If we find another earlier comment-ender,
	 any comment-starts earlier than that don't count
	 (because they go with the earlier comment-ender).  */
      if (code == Sendcomment && styles_match_p)
	break;

      /* Assume a defun-start point is outside of strings.  */
      if (code == Sopen
	  && (from == stop || BUF_FETCH_CHAR (buf, from - 1) == '\n'))
	break;
    }

  if (comstart_pos == 0)
    from = comment_end;
  /* If the earliest comment starter
     is followed by uniform paired string quotes or none,
     we know it can't be inside a string
     since if it were then the comment ender would be inside one.
     So it does start a comment.  Skip back to it.  */
  else if (comstart_parity == 0 && !string_lossage)
    from = comstart_pos;
  else
    {
      /* We had two kinds of string delimiters mixed up
	 together.  Decode this going forwards.
	 Scan fwd from the previous comment ender
	 to the one in question; this records where we
	 last passed a comment starter.  */

      struct lisp_parse_state state;
      scan_sexps_forward (buf, &state, find_defun_start (buf, comment_end),
			  comment_end - 1, -10000, 0, Qnil, 0);
      if (state.incomment)
	from = state.comstr_start;
      else
	/* We can't grok this as a comment; scan it normally.  */
	from = comment_end;
      UPDATE_SYNTAX_CACHE_FORWARD (from - 1);
    }
  return from;
}

static Bufpos
find_end_of_comment (struct buffer *buf, Bufpos from, Bufpos stop, int comstyle)
{
  int c;
  int syncode;
  enum syntaxcode code, next_code;
  /* mask to match comment styles against; for ST_COMMENT_STYLE, this
     will get set to SYNTAX_COMMENT_STYLE_B, but never get checked */
  int mask = comstyle ? SYNTAX_COMMENT_STYLE_B : SYNTAX_COMMENT_STYLE_A;

  /* This is only called by functions which have already set up the
     syntax_cache and are keeping it up-to-date */
  while (1)
    {
      if (from == stop)
	{
	  return -1;
	}

      UPDATE_SYNTAX_CACHE_FORWARD (from);
      c = BUF_FETCH_CHAR (buf, from);
      syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
      code = SYNTAX_FROM_CODE (syncode);

      from++;
      UPDATE_SYNTAX_CACHE_FORWARD (from);

      /* At end of current generic comment? */
      if (comstyle == ST_COMMENT_STYLE)
 	{
	  if (code == Scomment_fence)
	    break;		/* matched */
	  else
	    continue;		/* Ignore other styles in generic comments */
	}
      /* At end of current one-character comment of specified style? */
      else if (code == Sendcomment &&
	       SYNTAX_CODE_MATCHES_1CHAR_P (syncode, mask))
	  {
	    /* pre-MT code effectively does from-- here, that seems wrong */
	    break;
	  }

      /* At end of current two-character comment of specified style? */
      c = BUF_FETCH_CHAR (buf, from);
      next_code = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
      if (from < stop && SYNTAX_CODES_MATCH_END_P (syncode, next_code, mask))
	{
	  from++;
	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  break;
	}
    }
  return from;
}


/* #### between FSF 19.23 and 19.28 there are some changes to the logic
   in this function (and minor changes to find_start_of_comment(),
   above, which is part of Fforward_comment() in FSF).  Attempts to port
   that logic made this function break, so I'm leaving it out.  If anyone
   ever complains about this function not working properly, take a look
   at those changes.  --ben */

DEFUN ("forward-comment", Fforward_comment, 0, 2, 0, /*
Move forward across up to COUNT comments, or backwards if COUNT is negative.
Stop scanning if we find something other than a comment or whitespace.
Set point to where scanning stops.
If COUNT comments are found as expected, with nothing except whitespace
between them, return t; otherwise return nil.
Point is set in either case.
COUNT defaults to 1, and BUFFER defaults to the current buffer.
*/
       (count, buffer))
{
  Bufpos from;
  Bufpos stop;
  Emchar c;
  enum syntaxcode code;
  int syncode;
  EMACS_INT n;
  struct buffer *buf = decode_buffer (buffer, 0);

  if (NILP (count))
    n = 1;
  else
    {
      CHECK_INT (count);
      n = XINT (count);
    }

  from = BUF_PT (buf);

  SCS_STATISTICS_SET_FUNCTION (scs_Fforward_comment);
  SETUP_SYNTAX_CACHE (from, n);
  while (n > 0)
    {
      QUIT;

      stop = BUF_ZV (buf);
      while (from < stop)
	{
	  int comstyle = 0;     /* Code for comment style: 0 for A, 1 for B,
				   or ST_COMMENT_STYLE */

	  if (char_quoted (buf, from))
	    {
	      from++;
	      continue;
	    }

	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
	  code = SYNTAX_FROM_CODE (syncode);

	  if (code == Scomment)
	    {
	      /* we have encountered a single character comment start
		 sequence, and we are ignoring all text inside comments.
		 we must record the comment style this character begins
		 so that later, only a comment end of the same style actually
		 ends the comment section */
	      comstyle =
		SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  else if (code == Scomment_fence)
	    {
	      from++;
	      code = Scomment;
	      comstyle = ST_COMMENT_STYLE;
 	    }

	  else if (from < stop && SYNTAX_CODE_START_FIRST_P (syncode))
	    {
	      int next_syncode;
	      UPDATE_SYNTAX_CACHE_FORWARD (from + 1);
	      next_syncode =
		SYNTAX_CODE_FROM_CACHE (mirrortab, 
					BUF_FETCH_CHAR (buf, from + 1));

	      if (SYNTAX_CODES_START_P (syncode, next_syncode))
		{
		  /* we have encountered a 2char comment start sequence and we
		     are ignoring all text inside comments. we must record
		     the comment style this sequence begins so that later,
		     only a comment end of the same style actually ends
		     the comment section */
		  code = Scomment;
		  comstyle =
		    SYNTAX_CODES_COMMENT_MASK_START (syncode, next_syncode)
		    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from++;
		}
	    }

	  if (code == Scomment)
	    {
	      Bufpos newfrom = find_end_of_comment (buf, from, stop, comstyle);
	      if (newfrom < 0)
		{
		  /* we stopped because from==stop */
		  BUF_SET_PT (buf, stop);
		  return Qnil;
		}
	      from = newfrom;

	      /* We have skipped one comment.  */
	      break;
	    }
	  else if (code != Swhitespace
		   && code != Sendcomment
		   && code != Scomment )
	    {
	      BUF_SET_PT (buf, from);
	      return Qnil;
	    }
	  from++;
	}

      /* End of comment reached */
      n--;
    }

  while (n < 0)
    {
      QUIT;

      stop = BUF_BEGV (buf);
      while (from > stop)
	{
          int comstyle = 0;     /* Code for comment style: 0 for A, 1 for B,
				   or ST_COMMENT_STYLE */

	  from--;
	  if (char_quoted (buf, from))
	    {
	      from--;
	      continue;
	    }

	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
	  code = SYNTAX_FROM_CODE (syncode);

	  if (code == Sendcomment)
	    {
	      /* we have found a single char end comment. we must record
		 the comment style encountered so that later, we can match
		 only the proper comment begin sequence of the same style */
	      comstyle =
		SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  else if (code == Scomment_fence)
	    {
	      code = Sendcomment;
	      comstyle = ST_COMMENT_STYLE;
	    }

	  else if (from > stop
		   /* #### This seems logical but it's not in 21.4.9 */
		   /* && !char_quoted (buf, from - 1) */
		   && SYNTAX_CODE_END_SECOND_P (syncode))
	    {
	      int prev_syncode;
	      UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
	      prev_syncode =
		SYNTAX_CODE_FROM_CACHE (mirrortab,
					BUF_FETCH_CHAR (buf, from - 1));
	      if (SYNTAX_CODES_END_P (prev_syncode, syncode))
		{
		  /* We must record the comment style encountered so that
		     later, we can match only the proper comment begin
		     sequence of the same style.  */
		  code = Sendcomment;
		  comstyle =
		    SYNTAX_CODES_COMMENT_MASK_END (prev_syncode, syncode)
		    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from--;
		}
	    }

	  if (code == Sendcomment)
 	    {
 	      from = find_start_of_comment (buf, from, stop, comstyle);
 	      break;
            }

	  else if (code != Swhitespace
		   && code != Scomment
		   && code != Sendcomment)
	    {
	      BUF_SET_PT (buf, from + 1);
	      return Qnil;
	    }
	}

      n++;
    }

  BUF_SET_PT (buf, from);
  return Qt;
}


Lisp_Object
scan_lists (struct buffer *buf, Bufpos from, int count, int depth,
	    int sexpflag, int noerror)
{
  Bufpos stop;
  Emchar c;
  int quoted;
  int mathexit = 0;
  enum syntaxcode code;
  int syncode;
  int min_depth = depth;    /* Err out if depth gets less than this. */

  if (depth > 0) min_depth = 0;

  SCS_STATISTICS_SET_FUNCTION (scs_scan_lists);
  SETUP_SYNTAX_CACHE_FOR_BUFFER (buf, from, count);
  while (count > 0)
    {
      QUIT;

      stop = BUF_ZV (buf);
      while (from < stop)
	{
          int comstyle = 0;     /* mask for finding matching comment style */
	  Emchar stringterm = '\0'; /* Used by Sstring case in switch */

	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
	  code = SYNTAX_FROM_CODE (syncode);
	  from++;

	  /* a 1-char comment start sequence */
	  if (code == Scomment && parse_sexp_ignore_comments)
	    {
	      comstyle = SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode) ==
		SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  /* else, a 2-char comment start sequence? */
	  else if (from < stop
		   && SYNTAX_CODE_START_FIRST_P (syncode)
		   && parse_sexp_ignore_comments)
	    {
	      int next_syncode;
	      UPDATE_SYNTAX_CACHE_FORWARD (from);
	      next_syncode =
		SYNTAX_CODE_FROM_CACHE (mirrortab, BUF_FETCH_CHAR (buf, from));

	      if (SYNTAX_CODES_START_P (syncode, next_syncode))
		{
		  /* we have encountered a comment start sequence and we
		     are ignoring all text inside comments. we must record
		     the comment style this sequence begins so that later,
		     only a comment end of the same style actually ends
		     the comment section */
		  code = Scomment;
		  comstyle =
		    SYNTAX_CODES_COMMENT_MASK_START (syncode, next_syncode)
		    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from++;
		}
	    }
	  UPDATE_SYNTAX_CACHE_FORWARD (from);

	  if (SYNTAX_CODE_PREFIX (syncode))
	    continue;

	  switch (code)
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop) goto lose;
	      from++;
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it. */
	      while (from < stop)
		{
		  UPDATE_SYNTAX_CACHE_FORWARD (from);
		  switch (SYNTAX_FROM_CACHE (mirrortab,
					     BUF_FETCH_CHAR (buf, from)))
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		      if (from == stop) goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  from++;
		}
	      goto done;

	    case Scomment_fence:
	      comstyle = ST_COMMENT_STYLE;
	      /* falls through! */
	    case Scomment:
	      if (!parse_sexp_ignore_comments)
		break;
	      UPDATE_SYNTAX_CACHE_FORWARD (from);
	      {
		Bufpos newfrom =
		  find_end_of_comment (buf, from, stop, comstyle);
		if (newfrom < 0)
		  {
		    /* we stopped because from == stop in search forward */
		    from = stop;
		    if (depth == 0)
		      goto done;
		    goto lose;
		  }
		from = newfrom;
	      }
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == BUF_FETCH_CHAR (buf, from))
		from++;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;

	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	    if (!--depth) goto done;
	    if (depth < min_depth)
	      {
		if (noerror)
		  return Qnil;
		error ("Containing expression ends prematurely");
	      }
	    break;

	    case Sstring:
	      {
		/* XEmacs change: call syntax_match on character */
		Emchar ch = BUF_FETCH_CHAR (buf, from - 1);
		Lisp_Object stermobj =
		  syntax_match (syntax_cache.current_syntax_table, ch);

		if (CHARP (stermobj))
		  stringterm = XCHAR (stermobj);
		else
		  stringterm = ch;
	      }
	      /* falls through! */
	    case Sstring_fence:
	      while (1)
		{
		  if (from >= stop)
		    goto lose;
		  UPDATE_SYNTAX_CACHE_FORWARD (from);
		  c = BUF_FETCH_CHAR (buf, from);
		  if (code == Sstring
		      ? c == stringterm
		      : SYNTAX_FROM_CACHE (mirrortab, c) == Sstring_fence)
		    break;

		  switch (SYNTAX_FROM_CACHE (mirrortab, c))
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		      break;
		    default:
		      break;
		    }
		  from++;
		}
	      from++;
	      if (!depth && sexpflag) goto done;
	      break;

            default:
              break;
	    }
	}

      /* Reached end of buffer.  Error if within object,
	 return nil if between */
      if (depth) goto lose;

      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      QUIT;

      stop = BUF_BEGV (buf);
      while (from > stop)
	{
          int comstyle = 0;     /* mask for finding matching comment style */
	  Emchar stringterm = '\0'; /* used by case Sstring in switch below */

	  from--;
	  UPDATE_SYNTAX_CACHE_BACKWARD (from);
          quoted = char_quoted (buf, from);
	  if (quoted)
	    {
	    from--;
	      UPDATE_SYNTAX_CACHE_BACKWARD (from);
	    }

	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
	  code = SYNTAX_FROM_CODE (syncode);

	  if (code == Sendcomment && parse_sexp_ignore_comments)
	    {
	      /* we have found a single char end comment. we must record
		 the comment style encountered so that later, we can match
		 only the proper comment begin sequence of the same style */
	      comstyle = SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  else if (from > stop
		   && SYNTAX_CODE_END_SECOND_P (syncode)
		   && !char_quoted (buf, from - 1)
		   && parse_sexp_ignore_comments)
	    {
	      int prev_syncode;
	      UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
	      prev_syncode = SYNTAX_CODE_FROM_CACHE
		(mirrortab, BUF_FETCH_CHAR (buf, from - 1));

	      if (SYNTAX_CODES_END_P (prev_syncode, syncode))
		{
		  /* we must record the comment style encountered so that
		     later, we can match only the proper comment begin
		     sequence of the same style */
		  code = Sendcomment;
		  comstyle =
		    SYNTAX_CODES_COMMENT_MASK_END (prev_syncode, syncode)
		    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from--;
		}
	    }

	  if (SYNTAX_CODE_PREFIX (syncode))
	    continue;

	  switch (quoted ? Sword : code)
	    {
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished after
		 passing it. */
	      while (from > stop)
		{
		  /* enum syntaxcode syncode; */
		  UPDATE_SYNTAX_CACHE_BACKWARD (from);
		  quoted = char_quoted (buf, from - 1);

		  if (quoted)
		    from--;
		  if (! (quoted
                         || (syncode =
			     SYNTAX_FROM_CACHE (mirrortab,
						BUF_FETCH_CHAR (buf, from - 1)))
			 == Sword
			 || syncode == Ssymbol
			 || syncode == Squote))
            	    goto done2;
		  from--;
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == BUF_FETCH_CHAR (buf, from - 1))
		from--;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;

	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	    if (!--depth) goto done2;
	    if (depth < min_depth)
	      {
		if (noerror)
		  return Qnil;
		error ("Containing expression ends prematurely");
	      }
	    break;

	    case Scomment_fence:
	      comstyle = ST_COMMENT_STYLE;
	      /* falls through! */
	    case Sendcomment:
	      if (parse_sexp_ignore_comments)
		from = find_start_of_comment (buf, from, stop, comstyle);
	      break;

	    case Sstring:
              {
		/* XEmacs change: call syntax_match() on character */
                Emchar ch = BUF_FETCH_CHAR (buf, from);
		Lisp_Object stermobj =
		  syntax_match (syntax_cache.current_syntax_table, ch);
		if (CHARP (stermobj))
		  stringterm = XCHAR (stermobj);
		else
		  stringterm = ch;
	      }

	      /* falls through! */
	    case Sstring_fence:
	      while (1)
		{
		  if (from == stop) goto lose;

		  UPDATE_SYNTAX_CACHE_BACKWARD (from - 1);
		  c = BUF_FETCH_CHAR (buf, from - 1);
		  if ((code == Sstring
		       ? c == stringterm
		       : SYNTAX_FROM_CACHE (mirrortab, c) == Sstring_fence)
		      && !char_quoted (buf, from - 1))
		    {
		      break;
		    }

		  from--;
		}
	      from--;
	      if (!depth && sexpflag) goto done2;
	      break;
            }
	}

      /* Reached start of buffer.  Error if within object,
	 return nil if between */
      if (depth) goto lose;

      return Qnil;

    done2:
      count++;
    }


  return (make_int (from));

lose:
  if (!noerror)
    error ("Unbalanced parentheses");
  return Qnil;
}

int
char_quoted (struct buffer *buf, Bufpos pos)
{
  enum syntaxcode code;
  Bufpos beg = BUF_BEGV (buf);
  int quoted = 0;
  Bufpos startpos = pos;

  while (pos > beg)
    {
      UPDATE_SYNTAX_CACHE_BACKWARD (pos - 1);
      code = SYNTAX_FROM_CACHE (mirrortab, BUF_FETCH_CHAR (buf, pos - 1));

      if (code != Scharquote && code != Sescape)
	break;
      pos--;
      quoted = !quoted;
    }

  UPDATE_SYNTAX_CACHE (startpos);
  return quoted;
}

DEFUN ("scan-lists", Fscan_lists, 3, 5, 0, /*
Scan from character number FROM by COUNT lists.
Returns the character number of the position thus found.

If DEPTH is nonzero, paren depth begins counting from that value,
only places where the depth in parentheses becomes zero
are candidates for stopping; COUNT such places are counted.
Thus, a positive value for DEPTH means go out levels.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
and the depth is wrong, an error is signaled.
If the depth is right but the count is not used up, nil is returned.

If optional arg BUFFER is non-nil, scanning occurs in that buffer instead
of in the current buffer.

If optional arg NOERROR is non-nil, scan-lists will return nil instead of
signalling an error.
*/
       (from, count, depth, buffer, noerror))
{
  struct buffer *buf;

  CHECK_INT (from);
  CHECK_INT (count);
  CHECK_INT (depth);
  buf = decode_buffer (buffer, 0);

  return scan_lists (buf, XINT (from), XINT (count), XINT (depth), 0,
		     !NILP (noerror));
}

DEFUN ("scan-sexps", Fscan_sexps, 2, 4, 0, /*
Scan from character number FROM by COUNT balanced expressions.
If COUNT is negative, scan backwards.
Returns the character number of the position thus found.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings
but before count is used up, nil is returned.

If optional arg BUFFER is non-nil, scanning occurs in that buffer instead
of in the current buffer.

If optional arg NOERROR is non-nil, scan-sexps will return nil instead of
signalling an error.
*/
       (from, count, buffer, noerror))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  CHECK_INT (from);
  CHECK_INT (count);

  return scan_lists (buf, XINT (from), XINT (count), 0, 1, !NILP (noerror));
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, 0, 1, 0, /*
Move point backward over any number of chars with prefix syntax.
This includes chars with "quote" or "prefix" syntax (' or p).

Optional arg BUFFER defaults to the current buffer.
*/
       (buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  Bufpos beg = BUF_BEGV (buf);
  Bufpos pos = BUF_PT (buf);
#ifndef emacs
  Lisp_Char_Table *mirrortab = XCHAR_TABLE (buf->mirror_syntax_table);
#endif
  Emchar c = '\0'; /* initialize to avoid compiler warnings */


  SCS_STATISTICS_SET_FUNCTION (scs_Fbackward_prefix_characters);
  SETUP_SYNTAX_CACHE_FOR_BUFFER (buf, pos, -1);

  while (pos > beg && !char_quoted (buf, pos - 1)
	 /* Previous statement updates syntax table.  */
	 && (SYNTAX_FROM_CACHE (mirrortab, c = BUF_FETCH_CHAR (buf, pos - 1)) == Squote
	     /* equivalent to SYNTAX_PREFIX (mirrortab, c) */
	     || SYNTAX_CODE_PREFIX (SYNTAX_CODE_FROM_CACHE (mirrortab, c))))
    pos--;

  BUF_SET_PT (buf, pos);

  return Qnil;
}

/* Parse forward from FROM to END,
   assuming that FROM has state OLDSTATE (nil means FROM is start of function),
   and return a description of the state of the parse at END.
   If STOPBEFORE is nonzero, stop at the start of an atom.
   If COMMENTSTOP is 1, stop at the start of a comment; if it is -1,
     stop at the start of a comment or a string */

static void
scan_sexps_forward (struct buffer *buf, struct lisp_parse_state *stateptr,
		    Bufpos from, Bufpos end,
		    int targetdepth, int stopbefore,
		    Lisp_Object oldstate,
		    int commentstop)
{
  struct lisp_parse_state state;

  enum syntaxcode code;
  struct level { int last, prev; };
  struct level levelstart[100];
  struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  int boundary_stop = commentstop == -1;
  Lisp_Object tem;

  SCS_STATISTICS_SET_FUNCTION (scs_scan_sexps_forward);
  SETUP_SYNTAX_CACHE (from, 1);
  if (NILP (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
      state.comstyle = 0;	/* comment style a by default */
      state.comstr_start = -1;	/* no comment/string seen.  */
    }
  else
    {
      tem = Fcar (oldstate);    /* elt 0, depth */
      if (!NILP (tem))
	depth = XINT (tem);
      else
	depth = 0;

      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 3, instring */
      state.instring = ( !NILP (tem) 
			 ? ( INTP (tem) ? XINT (tem) : ST_STRING_STYLE) 
			 : -1);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 4, incomment */
      state.incomment = !NILP (tem);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 5, follows-quote */
      start_quoted = !NILP (tem);

      /* if the eighth element of the list is nil, we are in comment style
	 a; if it is t, we are in comment style b; if it is 'syntax-table,
	 we are in a generic comment */
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      /* The code below was changed radically for syntax-table properties.
         A reasonable place to look if a bug manifests. */
      tem = Fcar (oldstate);    /* elt 7, comment style a/b/fence */
      state.comstyle = NILP (tem) ? 0 : ( EQ (tem, Qsyntax_table)
					  ? ST_COMMENT_STYLE : 1 );

      oldstate = Fcdr (oldstate); /* elt 8, start of last comment/string */
      tem = Fcar (oldstate);
      state.comstr_start = NILP (tem) ? -1 : XINT (tem);

      /* elt 9, char numbers of starts-of-expression of levels
         (starting from outermost). */
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 9, intermediate data for
				   continuation of parsing (subject
				   to change). */
      while (!NILP (tem))	/* >= second enclosing sexps.  */
	{
	  curlevel->last = XINT (Fcar (tem));
	  if (++curlevel == endlevel)
	    error ("Nesting too deep for parser");
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  tem = Fcdr (tem);
	}
      /* end radical change section */
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  /* Enter the loop at a place appropriate for initial state. */

  if (state.incomment) goto startincomment;
  if (state.instring >= 0)
    {
      if (start_quoted) goto startquotedinstring;
      goto startinstring;
    }
  if (start_quoted) goto startquoted;

  while (from < end)
    {
      Emchar c;
      int syncode;

      QUIT;

      UPDATE_SYNTAX_CACHE_FORWARD (from);
      c = BUF_FETCH_CHAR (buf, from);
      syncode = SYNTAX_CODE_FROM_CACHE (mirrortab, c);
      code = SYNTAX_FROM_CODE (syncode);
      from++;

	  /* record the comment style we have entered so that only the
	     comment-ender sequence (or single char) of the same style
	     actually terminates the comment section. */
      if (code == Scomment)
	{
	  state.comstyle =
	    SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
	    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	  state.comstr_start = from - 1;
	}

      /* a generic comment delimiter? */
      else if (code == Scomment_fence)
	{
	  state.comstyle = ST_COMMENT_STYLE;
	  state.comstr_start = from - 1;
	  code = Scomment;
	}

      else if (from < end &&
	       SYNTAX_CODE_START_FIRST_P (syncode))
	{
	  int next_syncode;
	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  next_syncode =
	    SYNTAX_CODE_FROM_CACHE (mirrortab, BUF_FETCH_CHAR (buf, from));

	  if (SYNTAX_CODES_START_P (syncode, next_syncode))
	    {
	      code = Scomment;
	      state.comstyle =
		SYNTAX_CODES_COMMENT_MASK_START (syncode, next_syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	      state.comstr_start = from - 1;
	      from++;
	      UPDATE_SYNTAX_CACHE_FORWARD (from);
	    }
	}

      if (SYNTAX_CODE_PREFIX (syncode))
	continue;
      switch (code)
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	startquoted:
	  if (from == end) goto endquoted;
	  from++;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	symstarted:
	  while (from < end)
	    {
	      UPDATE_SYNTAX_CACHE_FORWARD (from);
	      switch (SYNTAX_FROM_CACHE (mirrortab, BUF_FETCH_CHAR (buf, from)))
		{
		case Scharquote:
		case Sescape:
		  from++;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      from++;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	case Scomment:
	  state.incomment = 1;
	  if (commentstop || boundary_stop) goto done;
	startincomment:
	  if (commentstop == 1)
	    goto done;
	  UPDATE_SYNTAX_CACHE_FORWARD (from);
	  {
	    Bufpos newfrom = find_end_of_comment (buf, from, end, state.comstyle);
	    if (newfrom < 0)
	      {
		/* we terminated search because from == end */
		from = end;
		goto done;
	      }
	    from = newfrom;
	  }
	  state.incomment = 0;
	  state.comstyle = 0;		     /* reset the comment style */
	  if (boundary_stop) goto done;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = from - 1;
	  if (++curlevel == endlevel)
	    error ("Nesting too deep for parser");
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (targetdepth == depth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (targetdepth == depth) goto done;
	  break;

	case Sstring:
	case Sstring_fence:
	  state.comstr_start = from - 1;
            if (stopbefore) goto stop; /* this arg means stop at sexp start */
            curlevel->last = from - 1;
	  if (code == Sstring_fence)
	    {
	      state.instring = ST_STRING_STYLE;
	    }
	  else
	    {
	      /* XEmacs change: call syntax_match() on character */
	      Emchar ch = BUF_FETCH_CHAR (buf, from - 1);
	      Lisp_Object stermobj =
		syntax_match (syntax_cache.current_syntax_table, ch);

	      if (CHARP (stermobj))
		state.instring = XCHAR (stermobj);
	      else
		state.instring = ch;
	    }
	  if (boundary_stop) goto done;
	startinstring:
	  while (1)
	    {
	      enum syntaxcode temp_code;

	      if (from >= end) goto done;

	      UPDATE_SYNTAX_CACHE_FORWARD (from);
	      c = BUF_FETCH_CHAR (buf, from);
	      temp_code = SYNTAX_FROM_CACHE (mirrortab, c);

	      if (
		  state.instring != ST_STRING_STYLE &&
		  temp_code == Sstring &&
		  c == state.instring) break;

	      switch (temp_code)
		{
		case Sstring_fence:
		  if (state.instring == ST_STRING_STYLE)
		    goto string_end;
		  break;
		case Scharquote:
		case Sescape:
                  {
                    from++;
                  startquotedinstring:
                    if (from >= end) goto endquoted;
                    break;
                  }
                default:
                  break;
		}
	      from++;
	    }
	string_end:
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  from++;
	  if (boundary_stop) goto done;
	  break;

	case Smath:
	  break;

        case Swhitespace:
        case Spunct:
        case Squote:
        case Sendcomment:
	case Scomment_fence:
	case Sinherit:
        case Smax:
          break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from--;    /* We have just fetched the char that starts it; */
  goto done; /* but return the position before it. */

 endquoted:
  state.quoted = 1;
 done:
  state.depth = depth;
  state.mindepth = mindepth;
  state.thislevelstart = curlevel->prev;
  state.prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state.location = from;
  state.levelstarts = Qnil;
  while (--curlevel >= levelstart)
    state.levelstarts = Fcons (make_int (curlevel->last),
			       state.levelstarts);

  *stateptr = state;
}

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, 2, 7, 0, /*
Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.
If fifth arg OLDSTATE is omitted or nil,
 parsing assumes that FROM is the beginning of a function.
Value is a list of nine elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (It is the character that will terminate the string,
     or t if the string should be terminated by an explicit
     `syntax-table' property.)
 4. t if inside a comment.
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. nil if in comment style a, or not in a comment; t if in comment style b;
    `syntax-table' if given by an explicit `syntax-table' property.
 8. character address of start of last comment or string; nil if none.
 9. Intermediate data for continuation of parsing (subject to change).
If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a nine-element list like what this function returns.
It is used to initialize the state of the parse.  Its second and third
elements are ignored.
Sixth arg COMMENTSTOP non-nil means stop at the start of a comment. If it
is `syntax-table', stop after the start of a comment or a string, or after
the end of a comment or string.
*/
       (from, to, targetdepth, stopbefore, oldstate, commentstop, buffer))
{
  struct lisp_parse_state state;
  int target;
  Bufpos start, end;
  struct buffer *buf = decode_buffer (buffer, 0);
  Lisp_Object val;

  if (!NILP (targetdepth))
    {
      CHECK_INT (targetdepth);
      target = XINT (targetdepth);
    }
  else
    target = -100000;		/* We won't reach this depth */

  get_buffer_range_char (buf, from, to, &start, &end, 0);
  scan_sexps_forward (buf, &state, start, end,
		      target, !NILP (stopbefore), oldstate,
		      (NILP (commentstop)
		       ? 0 : (EQ (commentstop, Qsyntax_table) ? -1 : 1)));
  BUF_SET_PT (buf, state.location);

  /* reverse order */
  val = Qnil;
  val = Fcons (state.levelstarts, val);
  val = Fcons ((state.incomment || (state.instring >= 0))
	       ? make_int (state.comstr_start) : Qnil, val);
  val = Fcons (state.comstyle  ? (state.comstyle == ST_COMMENT_STYLE
				  ? Qsyntax_table : Qt) : Qnil, val);
  val = Fcons (make_int (state.mindepth),   val);
  val = Fcons (state.quoted    ? Qt : Qnil, val);
  val = Fcons (state.incomment ? Qt : Qnil, val);
  val = Fcons (state.instring < 0
	       ? Qnil
	       : (state.instring == ST_STRING_STYLE
		  ? Qt : make_int (state.instring)), val);
  val = Fcons (state.thislevelstart < 0 ? Qnil : make_int (state.thislevelstart), val);
  val = Fcons (state.prevlevelstart < 0 ? Qnil : make_int (state.prevlevelstart), val);
  val = Fcons (make_int (state.depth), val);

  return val;
}


/* Updating of the mirror syntax table.

   Each syntax table has a corresponding mirror table in it.
   Whenever we make a change to a syntax table, we call
   update_syntax_table() on it.

   #### We really only need to map over the changed range.

   If we change the standard syntax table, we need to map over
   all tables because any of them could be inheriting from the
   standard syntax table.

   When `set-syntax-table' is called, we set the buffer's mirror
   syntax table as well.
   */

struct cmst_arg
{
  Lisp_Object mirrortab;
  int check_inherit;
};

static int
cmst_mapfun (struct chartab_range *range, Lisp_Object val, void *arg)
{
  struct cmst_arg *closure = (struct cmst_arg *) arg;

  if (CONSP (val))
    val = XCAR (val);
  if (SYNTAX_FROM_CODE (XINT (val)) == Sinherit
      && closure->check_inherit)
    {
      struct cmst_arg recursive;

      recursive.mirrortab = closure->mirrortab;
      recursive.check_inherit = 0;
      map_char_table (XCHAR_TABLE (Vstandard_syntax_table), range,
				   cmst_mapfun, &recursive);
    }
  else
    put_char_table (XCHAR_TABLE (closure->mirrortab), range, val);
  return 0;
}

static void
update_just_this_syntax_table (Lisp_Char_Table *ct)
{
  struct chartab_range range;
  struct cmst_arg arg;

  arg.mirrortab = ct->mirror_table;
  arg.check_inherit = (CHAR_TABLEP (Vstandard_syntax_table)
		       && ct != XCHAR_TABLE (Vstandard_syntax_table));
  range.type = CHARTAB_RANGE_ALL;
  map_char_table (ct, &range, cmst_mapfun, &arg);
}

/* Called from chartab.c when a change is made to a syntax table.
   If this is the standard syntax table, we need to recompute
   *all* syntax tables (yuck).  Otherwise we just recompute this
   one. */

void
update_syntax_table (Lisp_Char_Table *ct)
{
  /* Don't be stymied at startup. */
  if (CHAR_TABLEP (Vstandard_syntax_table)
      && ct == XCHAR_TABLE (Vstandard_syntax_table))
    {
      Lisp_Object syntab;

      for (syntab = Vall_syntax_tables; !NILP (syntab);
	   syntab = XCHAR_TABLE (syntab)->next_table)
	update_just_this_syntax_table (XCHAR_TABLE (syntab));
    }
  else
    update_just_this_syntax_table (ct);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_syntax (void)
{
  defsymbol (&Qsyntax_table_p, "syntax-table-p");
  defsymbol (&Qsyntax_table, "syntax-table");

  DEFSUBR (Fsyntax_table_p);
  DEFSUBR (Fsyntax_table);
  DEFSUBR (Fstandard_syntax_table);
  DEFSUBR (Fcopy_syntax_table);
  DEFSUBR (Fset_syntax_table);
  DEFSUBR (Fsyntax_designator_chars);
  DEFSUBR (Fchar_syntax);
  DEFSUBR (Fmatching_paren);
  /* DEFSUBR (Fmodify_syntax_entry); now in Lisp. */
  /* DEFSUBR (Fdescribe_syntax); now in Lisp. */

  DEFSUBR (Fforward_word);

  DEFSUBR (Fforward_comment);
  DEFSUBR (Fscan_lists);
  DEFSUBR (Fscan_sexps);
  DEFSUBR (Fbackward_prefix_chars);
  DEFSUBR (Fparse_partial_sexp);
}

void
vars_of_syntax (void)
{
  DEFVAR_BOOL ("parse-sexp-ignore-comments", &parse_sexp_ignore_comments /*
Non-nil means `forward-sexp', etc., should treat comments as whitespace.
*/ );
  parse_sexp_ignore_comments = 0;

  DEFVAR_BOOL ("lookup-syntax-properties", &lookup_syntax_properties /*
Non-nil means `forward-sexp', etc., look up character syntax in the
table that is the value of the `syntax-table' text property, if non-nil.
The value of this property should be either a syntax table, or a cons
of the form (SYNTAXCODE . MATCHCHAR), SYNTAXCODE being the numeric
syntax code, MATCHCHAR being nil or the character to match (which is
relevant only for open/close type.
*/ );
  lookup_syntax_properties = 0;	/* #### default off until optimized */

  DEFVAR_BOOL ("words-include-escapes", &words_include_escapes /*
Non-nil means `forward-word', etc., should treat escape chars part of words.
*/ );
  words_include_escapes = 0;

  no_quit_in_re_search = 0;
}

static void
define_standard_syntax (const char *p, enum syntaxcode syn)
{
  for (; *p; p++)
    Fput_char_table (make_char (*p), make_int (syn), Vstandard_syntax_table);
}

void
complex_vars_of_syntax (void)
{
  Emchar i;
  const char *p;
  /* Set this now, so first buffer creation can refer to it. */
  /* Make it nil before calling copy-syntax-table
     so that copy-syntax-table will know not to try to copy from garbage */
  Vstandard_syntax_table = Qnil;
  Vstandard_syntax_table = Fcopy_syntax_table (Qnil);
  staticpro (&Vstandard_syntax_table);

  Vsyntax_designator_chars_string = make_string_nocopy (syntax_code_spec,
							Smax);
  staticpro (&Vsyntax_designator_chars_string);

  fill_char_table (XCHAR_TABLE (Vstandard_syntax_table), make_int (Spunct));

  for (i = 0; i <= 32; i++)	/* Control 0 plus SPACE */
    Fput_char_table (make_char (i), make_int (Swhitespace),
		     Vstandard_syntax_table);
  for (i = 127; i <= 159; i++)	/* DEL plus Control 1 */
    Fput_char_table (make_char (i), make_int (Swhitespace),
		     Vstandard_syntax_table);

  define_standard_syntax ("abcdefghijklmnopqrstuvwxyz"
			  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			  "0123456789"
			  "$%", Sword);
  define_standard_syntax ("\"", Sstring);
  define_standard_syntax ("\\", Sescape);
  define_standard_syntax ("_-+*/&|<>=", Ssymbol);
  define_standard_syntax (".,;:?!#@~^'`", Spunct);

  for (p = "()[]{}"; *p; p+=2)
    {
      Fput_char_table (make_char (p[0]),
		       Fcons (make_int (Sopen), make_char (p[1])),
		       Vstandard_syntax_table);
      Fput_char_table (make_char (p[1]),
		       Fcons (make_int (Sclose), make_char (p[0])),
		       Vstandard_syntax_table);
    }
}
