/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Rewritten by mly to use varargs.h.
   Rewritten from scratch by Ben Wing (February 1995) for Mule; expanded
   to full printf spec.

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

/* Synched up with: Rewritten.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "lstream.h"

static const char * const valid_flags = "-+ #0";
static const char * const valid_converters = "dic" "ouxX" "feEgG" "sS";
static const char * const int_converters = "dic";
static const char * const unsigned_int_converters = "ouxX";
static const char * const double_converters = "feEgG";
static const char * const string_converters = "sS";

typedef struct printf_spec printf_spec;
struct printf_spec
{
  int argnum; /* which argument does this spec want?  This is one-based:
		 The first argument given is numbered 1, the second
		 is 2, etc.  This is to handle %##$x-type specs. */
  int minwidth;
  int precision;
  unsigned int minus_flag:1;
  unsigned int plus_flag:1;
  unsigned int space_flag:1;
  unsigned int number_flag:1;
  unsigned int zero_flag:1;
  unsigned int h_flag:1;
  unsigned int l_flag:1;
  unsigned int forwarding_precision:1;
  char converter; /* converter character or 0 for dummy marker
		     indicating literal text at the end of the
		     specification */
  Bytecount text_before; /* position of the first character of the
			    block of literal text before this spec */
  Bytecount text_before_len; /* length of that text */
};

typedef union printf_arg printf_arg;
union printf_arg
{
  long l;
  unsigned long ul;
  double d;
  Bufbyte *bp;
};

/* We maintain a list of all the % specs in the specification,
   along with the offset and length of the block of literal text
   before each spec.  In addition, we have a "dummy" spec that
   represents all the literal text at the end of the specification.
   Its converter is 0. */

typedef struct
{
  Dynarr_declare (struct printf_spec);
} printf_spec_dynarr;

typedef struct
{
  Dynarr_declare (union printf_arg);
} printf_arg_dynarr;

/* Append STRING (of length LEN bytes) to STREAM.
   MINLEN is the minimum field width.
   If MINUS_FLAG is set, left-justify the string in its field;
    otherwise, right-justify.
   If ZERO_FLAG is set, pad with 0's; otherwise pad with spaces.
   If MAXLEN is non-negative, the string is first truncated on the
    right to that many characters.

   Note that MINLEN and MAXLEN are Charcounts but LEN is a Bytecount. */

static void
doprnt_1 (Lisp_Object stream, const Bufbyte *string, Bytecount len,
	  Charcount minlen, Charcount maxlen, int minus_flag, int zero_flag)
{
  Lstream *lstr = XLSTREAM (stream);
  Charcount cclen = bytecount_to_charcount (string, len);
  int to_add = minlen - cclen;

  /* Padding at beginning to right-justify ... */
  if (!minus_flag)
    while (to_add-- > 0)
      Lstream_putc (lstr, zero_flag ? '0' : ' ');

  if (0 <= maxlen && maxlen < cclen)
    len = charcount_to_bytecount (string, maxlen);
  Lstream_write (lstr, string, len);

  /* Padding at end to left-justify ... */
  if (minus_flag)
    while (to_add-- > 0)
      Lstream_putc (lstr, zero_flag ? '0' : ' ');
}

static const Bufbyte *
parse_off_posnum (const Bufbyte *start, const Bufbyte *end, int *returned_num)
{
  Bufbyte arg_convert[100];
  REGISTER Bufbyte *arg_ptr = arg_convert;

  *returned_num = -1;
  while (start != end && isdigit (*start))
    {
      if ((size_t) (arg_ptr - arg_convert) >= sizeof (arg_convert) - 1)
	error ("Format converter number too large");
      *arg_ptr++ = *start++;
    }
  *arg_ptr = '\0';
  if (arg_convert != arg_ptr)
    *returned_num = atoi ((char *) arg_convert);
  return start;
}

#define NEXT_ASCII_BYTE(ch)					\
  do {								\
    if (fmt == fmt_end)						\
      error ("Premature end of format string");			\
    ch = *fmt;							\
    if (ch >= 0200)						\
      error ("Non-ASCII character in format converter spec");	\
    fmt++;							\
  } while (0)

#define RESOLVE_FLAG_CONFLICTS(spec)				\
  do {								\
    if (spec.space_flag && spec.plus_flag)			\
      spec.space_flag = 0;					\
    if (spec.zero_flag && spec.space_flag)			\
      spec.zero_flag = 0;					\
  } while (0)

static printf_spec_dynarr *
parse_doprnt_spec (const Bufbyte *format, Bytecount format_length)
{
  const Bufbyte *fmt = format;
  const Bufbyte *fmt_end = format + format_length;
  printf_spec_dynarr *specs = Dynarr_new (printf_spec);
  int prev_argnum = 0;

  while (1)
    {
      struct printf_spec spec;
      const Bufbyte *text_end;
      Bufbyte ch;

      xzero (spec);
      if (fmt == fmt_end)
	return specs;
      text_end = (Bufbyte *) memchr (fmt, '%', fmt_end - fmt);
      if (!text_end)
	text_end = fmt_end;
      spec.text_before = fmt - format;
      spec.text_before_len = text_end - fmt;
      fmt = text_end;
      if (fmt != fmt_end)
	{
	  fmt++; /* skip over % */

	  /* A % is special -- no arg number.  According to ANSI specs,
	     field width does not apply to %% conversion. */
	  if (fmt != fmt_end && *fmt == '%')
	    {
	      spec.converter = '%';
	      Dynarr_add (specs, spec);
	      fmt++;
	      continue;
	    }

	  /* Is there a field number specifier? */
	  {
	    const Bufbyte *ptr;
	    int fieldspec;

	    ptr = parse_off_posnum (fmt, fmt_end, &fieldspec);
	    if (fieldspec > 0 && ptr != fmt_end && *ptr == '$')
	      {
		/* There is a format specifier */
		prev_argnum = fieldspec;
		fmt = ptr + 1;
	      }
	    else
	      prev_argnum++;
	    spec.argnum = prev_argnum;
	  }

	  /* Parse off any flags */
	  NEXT_ASCII_BYTE (ch);
	  while (strchr (valid_flags, ch))
	    {
	      switch (ch)
		{
		case '-': spec.minus_flag  = 1; break;
		case '+': spec.plus_flag   = 1; break;
		case ' ': spec.space_flag  = 1; break;
		case '#': spec.number_flag = 1; break;
		case '0': spec.zero_flag   = 1; break;
		default: abort ();
		}
	      NEXT_ASCII_BYTE (ch);
	    }

	  /* Parse off the minimum field width */
	  fmt--; /* back up */

	  /*
	   * * means the field width was passed as an argument.
	   * Mark the current spec as one that forwards its
	   * field width and flags to the next spec in the array.
	   * Then create a new spec and continue with the parsing.
	   */
	  if (fmt != fmt_end && *fmt == '*')
	    {
	      spec.converter = '*';
	      RESOLVE_FLAG_CONFLICTS(spec);
	      Dynarr_add (specs, spec);
	      xzero (spec);
	      spec.argnum = ++prev_argnum;
	      fmt++;
	    }
	  else
	    {
	      fmt = parse_off_posnum (fmt, fmt_end, &spec.minwidth);
	      if (spec.minwidth == -1)
		spec.minwidth = 0;
	    }

	  /* Parse off any precision specified */
	  NEXT_ASCII_BYTE (ch);
	  if (ch == '.')
	    {
	      /*
	       * * means the precision was passed as an argument.
	       * Mark the current spec as one that forwards its
	       * fieldwidth, flags and precision to the next spec in
	       * the array.  Then create a new spec and continue
	       * with the parse.
	       */
	      if (fmt != fmt_end && *fmt == '*')
		{
		  spec.converter = '*';
		  spec.forwarding_precision = 1;
		  RESOLVE_FLAG_CONFLICTS(spec);
		  Dynarr_add (specs, spec);
		  xzero (spec);
		  spec.argnum = ++prev_argnum;
		  fmt++;
		}
	      else
		{
		  fmt = parse_off_posnum (fmt, fmt_end, &spec.precision);
		  if (spec.precision == -1)
		    spec.precision = 0;
		}
	      NEXT_ASCII_BYTE (ch);
	    }
	  else
	    /* No precision specified */
	    spec.precision = -1;

	  /* Parse off h or l flag */
	  if (ch == 'h' || ch == 'l')
	    {
	      if (ch == 'h')
		spec.h_flag = 1;
	      else
		spec.l_flag = 1;
	      NEXT_ASCII_BYTE (ch);
	    }

	  if (!strchr (valid_converters, ch))
	    error ("Invalid converter character %c", ch);
	  spec.converter = ch;
	}

      RESOLVE_FLAG_CONFLICTS(spec);
      Dynarr_add (specs, spec);
    }

  RETURN_NOT_REACHED(specs) /* suppress compiler warning */
}

static int
get_args_needed (printf_spec_dynarr *specs)
{
  int args_needed = 0;
  REGISTER int i;

  /* Figure out how many args are needed.  This may be less than
     the number of specs because a spec could be %% or could be
     missing (literal text at end of format string) or there
     could be specs where the field number is explicitly given.
     We just look for the maximum argument number that's referenced. */

  for (i = 0; i < Dynarr_length (specs); i++)
    {
      char ch = Dynarr_at (specs, i).converter;
      if (ch && ch != '%')
	{
	  int argnum = Dynarr_at (specs, i).argnum;
	  if (argnum > args_needed)
	    args_needed = argnum;
	}
    }

  return args_needed;
}

static printf_arg_dynarr *
get_doprnt_args (printf_spec_dynarr *specs, va_list vargs)
{
  printf_arg_dynarr *args = Dynarr_new (printf_arg);
  union printf_arg arg;
  REGISTER int i;
  int args_needed = get_args_needed (specs);

  xzero (arg);
  for (i = 1; i <= args_needed; i++)
    {
      int j;
      char ch;
      struct printf_spec *spec = 0;

      for (j = 0; j < Dynarr_length (specs); j++)
	{
	  spec = Dynarr_atp (specs, j);
	  if (spec->argnum == i)
	    break;
	}

      if (j == Dynarr_length (specs))
	error ("No conversion spec for argument %d", i);

      ch = spec->converter;

      if (strchr (int_converters, ch))
	{
	  if (spec->l_flag)
	    arg.l = va_arg (vargs, long);
	  else
	    /* int even if ch == 'c' or spec->h_flag:
	       "the type used in va_arg is supposed to match the
	       actual type **after default promotions**."
	       Hence we read an int, not a short, if spec->h_flag. */
	    arg.l = va_arg (vargs, int);
	}
      else if (strchr (unsigned_int_converters, ch))
	{
	  if (spec->l_flag)
	    arg.ul = va_arg (vargs, unsigned long);
	  else
	    /* unsigned int even if ch == 'c' or spec->h_flag */
	    arg.ul = (unsigned long) va_arg (vargs, unsigned int);
	}
      else if (strchr (double_converters, ch))
	arg.d = va_arg (vargs, double);
      else if (strchr (string_converters, ch))
	arg.bp = va_arg (vargs, Bufbyte *);
      else abort ();

      Dynarr_add (args, arg);
    }

  return args;
}

/* Generate output from a format-spec FORMAT, of length FORMAT_LENGTH.
   Output goes in BUFFER, which has room for BUFSIZE bytes.
   If the output does not fit, truncate it to fit.
   Returns the number of bytes stored into BUFFER.
   LARGS or VARGS points to the arguments, and NARGS says how many.
   if LARGS is non-zero, it should be a pointer to NARGS worth of
   Lisp arguments.  Otherwise, VARGS should be a va_list referring
   to the arguments. */

static Bytecount
emacs_doprnt_1 (Lisp_Object stream, const Bufbyte *format_nonreloc,
		Lisp_Object format_reloc, Bytecount format_length,
		int nargs,
		/* #### Gag me, gag me, gag me */
		const Lisp_Object *largs, va_list vargs)
{
  printf_spec_dynarr *specs = 0;
  printf_arg_dynarr *args = 0;
  REGISTER int i;
  int init_byte_count = Lstream_byte_count (XLSTREAM (stream));

  if (!NILP (format_reloc))
    {
      format_nonreloc = XSTRING_DATA (format_reloc);
      format_length = XSTRING_LENGTH (format_reloc);
    }
  if (format_length < 0)
    format_length = (Bytecount) strlen ((const char *) format_nonreloc);

  specs = parse_doprnt_spec (format_nonreloc, format_length);
  if (largs)
    {
      /* allow too many args for string, but not too few */
      if (nargs < get_args_needed (specs))
	signal_error (Qwrong_number_of_arguments,
		      list3 (Qformat,
			     make_int (nargs),
			     !NILP (format_reloc) ? format_reloc :
			     make_string (format_nonreloc, format_length)));
    }
  else
    {
      args = get_doprnt_args (specs, vargs);
    }

  for (i = 0; i < Dynarr_length (specs); i++)
    {
      struct printf_spec *spec = Dynarr_atp (specs, i);
      char ch;

      /* Copy the text before */
      if (!NILP (format_reloc)) /* refetch in case of GC below */
	format_nonreloc = XSTRING_DATA (format_reloc);

      doprnt_1 (stream, format_nonreloc + spec->text_before,
		spec->text_before_len, 0, -1, 0, 0);

      ch = spec->converter;

      if (!ch)
	continue;

      if (ch == '%')
	{
	  doprnt_1 (stream, (Bufbyte *) &ch, 1, 0, -1, 0, 0);
	  continue;
	}

      /* The char '*' as converter means the field width, precision
         was specified as an argument.  Extract the data and forward
         it to the next spec, to which it will apply.  */
      if (ch == '*')
	{
	  struct printf_spec *nextspec = Dynarr_atp (specs, i + 1);
	  Lisp_Object obj = largs[spec->argnum - 1];

	  if (INTP (obj))
	    {
	      if (spec->forwarding_precision)
		{
		  nextspec->precision = XINT (obj);
		  nextspec->minwidth = spec->minwidth;
		}
	      else
		{
		  nextspec->minwidth = XINT (obj);
		  if (XINT (obj) < 0)
		    {
		      spec->minus_flag = 1;
		      nextspec->minwidth = - nextspec->minwidth;
		    }
		}
	      nextspec->minus_flag  = spec->minus_flag;
	      nextspec->plus_flag   = spec->plus_flag;
	      nextspec->space_flag  = spec->space_flag;
	      nextspec->number_flag = spec->number_flag;
	      nextspec->zero_flag   = spec->zero_flag;
	    }
	  continue;
	}

      if (largs && (spec->argnum < 1 || spec->argnum > nargs))
	error ("Invalid repositioning argument %d", spec->argnum);

      else if (ch == 'S' || ch == 's')
	{
	  Bufbyte *string;
	  Bytecount string_len;

	  if (!largs)
	    {
	      string = Dynarr_at (args, spec->argnum - 1).bp;
	      /* error() can be called with null string arguments.
		 E.g., in fileio.c, the return value of strerror()
		 is never checked.  We'll print (null), like some
		 printf implementations do.  Would it be better (and safe)
		 to signal an error instead?  Or should we just use the
                 empty string?  -dkindred@cs.cmu.edu 8/1997
	      */
	      if (!string)
		string = (Bufbyte *) "(null)";
	      string_len = strlen ((char *) string);
	    }
	  else
	    {
	      Lisp_Object obj = largs[spec->argnum - 1];
	      Lisp_String *ls;

	      if (ch == 'S')
		{
		  /* For `S', prin1 the argument and then treat like
		     a string.  */
		  ls = XSTRING (Fprin1_to_string (obj, Qnil));
		}
	      else if (STRINGP (obj))
		ls = XSTRING (obj);
	      else if (SYMBOLP (obj))
		ls = XSYMBOL (obj)->name;
	      else
		{
		  /* convert to string using princ. */
		  ls = XSTRING (Fprin1_to_string (obj, Qt));
		}
	      string = string_data (ls);
	      string_len = string_length (ls);
	    }

	  doprnt_1 (stream, string, string_len, spec->minwidth,
		    spec->precision, spec->minus_flag, spec->zero_flag);
	}

      else
	{
	  /* Must be a number. */
	  union printf_arg arg;

	  if (!largs)
	    {
	      arg = Dynarr_at (args, spec->argnum - 1);
	    }
	  else
	    {
	      Lisp_Object obj = largs[spec->argnum - 1];
	      if (CHARP (obj))
		obj = make_int (XCHAR (obj));
	      if (!INT_OR_FLOATP (obj))
		{
		  error ("format specifier %%%c doesn't match argument type",
			 ch);
		}
	      else if (strchr (double_converters, ch))
		arg.d = XFLOATINT (obj);
	      else
		{
		  if (FLOATP (obj))
		    obj = Ftruncate (obj);

		  if (strchr (unsigned_int_converters, ch))
		    arg.ul = (unsigned long) XINT (obj);
		  else
		    arg.l = XINT (obj);
		}
	    }


	  if (ch == 'c')
	    {
	      Emchar a;
	      Bytecount charlen;
	      Bufbyte charbuf[MAX_EMCHAR_LEN];

	      a = (Emchar) arg.l;

	      if (!valid_char_p (a))
		error ("invalid character value %d to %%c spec", a);

	      charlen = set_charptr_emchar (charbuf, a);
	      doprnt_1 (stream, charbuf, charlen, spec->minwidth,
			-1, spec->minus_flag, spec->zero_flag);
	    }
	  else
	    {
	      /* ASCII Decimal representation uses 2.4 times as many
		 bits as machine binary.  */
	      char *text_to_print =
		alloca_array (char, 32 +
			      max (spec->minwidth,
				   (EMACS_INT)
				    max (sizeof (double), sizeof (long)) * 3 +
				   max (spec->precision, 0)));
	      char constructed_spec[100];
	      char *p = constructed_spec;

	      /* Mostly reconstruct the spec and use sprintf() to
		 format the string. */

	      *p++ = '%';
	      if (spec->plus_flag)   *p++ = '+';
	      if (spec->space_flag)  *p++ = ' ';
	      if (spec->number_flag) *p++ = '#';
	      if (spec->minus_flag)  *p++ = '-';
	      if (spec->zero_flag)   *p++ = '0';

	      if (spec->minwidth >= 0)
		p = long_to_string (p, spec->minwidth);
	      if (spec->precision >= 0)
		{
		  *p++ = '.';
		  p = long_to_string (p, spec->precision);
		}
	      
	      if (strchr (double_converters, ch))
		{
		  *p++ = ch;
		  *p++ = '\0';
		  sprintf (text_to_print, constructed_spec, arg.d);
		}
	      else
		{
		  *p++ = 'l';	/* Always use longs with sprintf() */
		  *p++ = ch;
		  *p++ = '\0';

		  if (strchr (unsigned_int_converters, ch))
		    sprintf (text_to_print, constructed_spec, arg.ul);
		  else
		    sprintf (text_to_print, constructed_spec, arg.l);
		}

	      doprnt_1 (stream, (Bufbyte *) text_to_print,
			strlen (text_to_print), 0, -1, 0, 0);
	    }
	}
    }

  /* #### will not get freed if error */
  if (specs)
    Dynarr_free (specs);
  if (args)
    Dynarr_free (args);
  return Lstream_byte_count (XLSTREAM (stream)) - init_byte_count;
}

/* You really don't want to know why this is necessary... */
static Bytecount
emacs_doprnt_2 (Lisp_Object stream, const Bufbyte *format_nonreloc,
		Lisp_Object format_reloc, Bytecount format_length, int nargs,
		const Lisp_Object *largs, ...)
{
  va_list vargs;
  Bytecount val;
  va_start (vargs, largs);
  val = emacs_doprnt_1 (stream, format_nonreloc, format_reloc,
			format_length, nargs, largs, vargs);
  va_end (vargs);
  return val;
}

/*********************** external entry points ***********************/

#ifdef I18N3
  /* A note about I18N3 translating: the format string should get
     translated, but not under all circumstances.  When the format
     string is a Lisp string, what should happen is that Fformat()
     should format the untranslated args[0] and return that, and also
     call Fgettext() on args[0] and, if that is different, format it
     and store it in the `string-translatable' property of
     the returned string.  See Fgettext(). */
#endif

/* Send formatted output to STREAM.  The format string comes from
   either FORMAT_NONRELOC (of length FORMAT_LENGTH; -1 means use
   strlen() to determine the length) or from FORMAT_RELOC, which
   should be a Lisp string.  Return the number of bytes written
   to the stream.

   DO NOT pass the data from a Lisp string as the FORMAT_NONRELOC
   parameter, because this function can cause GC. */

Bytecount
emacs_doprnt_c (Lisp_Object stream, const Bufbyte *format_nonreloc,
		Lisp_Object format_reloc, Bytecount format_length,
		...)
{
  int val;
  va_list vargs;

  va_start (vargs, format_length);
  val = emacs_doprnt_1 (stream, format_nonreloc, format_reloc,
			format_length, 0, 0, vargs);
  va_end (vargs);
  return val;
}

/* Like emacs_doprnt_c but the args come in va_list format. */

Bytecount
emacs_doprnt_va (Lisp_Object stream, const Bufbyte *format_nonreloc,
		 Lisp_Object format_reloc, Bytecount format_length,
		 va_list vargs)
{
  return emacs_doprnt_1 (stream, format_nonreloc, format_reloc,
			 format_length, 0, 0, vargs);
}

/* Like emacs_doprnt_c but the args are Lisp objects instead of
   C arguments.  This causes somewhat different behavior from
   the above two functions (which should act like printf).
   See `format' for a description of this behavior. */

Bytecount
emacs_doprnt_lisp (Lisp_Object stream, const Bufbyte *format_nonreloc,
		   Lisp_Object format_reloc, Bytecount format_length,
		   int nargs, const Lisp_Object *largs)
{
  return emacs_doprnt_2 (stream, format_nonreloc, format_reloc,
			 format_length, nargs, largs);
}

/* Like the previous function but takes a variable number of arguments. */

Bytecount
emacs_doprnt_lisp_2 (Lisp_Object stream, const Bufbyte *format_nonreloc,
		     Lisp_Object format_reloc, Bytecount format_length,
		     int nargs, ...)
{
  va_list vargs;
  int i;
  Lisp_Object *foo = alloca_array (Lisp_Object, nargs);

  va_start (vargs, nargs);
  for (i = 0; i < nargs; i++)
    foo[i] = va_arg (vargs, Lisp_Object);
  va_end (vargs);

  return emacs_doprnt_2 (stream, format_nonreloc, format_reloc,
			 format_length, nargs, foo);
}

/* The following four functions work like the above three but
   return their output as a Lisp string instead of sending it
   to a stream. */

Lisp_Object
emacs_doprnt_string_c (const Bufbyte *format_nonreloc,
		       Lisp_Object format_reloc, Bytecount format_length,
		       ...)
{
  va_list vargs;
  Lisp_Object obj;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;

  GCPRO1 (stream);
  va_start (vargs, format_length);
  emacs_doprnt_1 (stream, format_nonreloc, format_reloc,
		  format_length, 0, 0, vargs);
  va_end (vargs);
  Lstream_flush (XLSTREAM (stream));
  obj = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		     Lstream_byte_count (XLSTREAM (stream)));
  UNGCPRO;
  Lstream_delete (XLSTREAM (stream));
  return obj;
}

Lisp_Object
emacs_doprnt_string_va (const Bufbyte *format_nonreloc,
			Lisp_Object format_reloc, Bytecount format_length,
			va_list vargs)
{
  /* I'm fairly sure that this function cannot actually GC.
     That can only happen when the arguments to emacs_doprnt_1() are
     Lisp objects rather than C args. */
  Lisp_Object obj;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;

  GCPRO1 (stream);
  emacs_doprnt_1 (stream, format_nonreloc, format_reloc,
		  format_length, 0, 0, vargs);
  Lstream_flush (XLSTREAM (stream));
  obj = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		     Lstream_byte_count (XLSTREAM (stream)));
  UNGCPRO;
  Lstream_delete (XLSTREAM (stream));
  return obj;
}

Lisp_Object
emacs_doprnt_string_lisp (const Bufbyte *format_nonreloc,
			  Lisp_Object format_reloc, Bytecount format_length,
			  int nargs, const Lisp_Object *largs)
{
  Lisp_Object obj;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;

  GCPRO1 (stream);
  emacs_doprnt_2 (stream, format_nonreloc, format_reloc,
		  format_length, nargs, largs);
  Lstream_flush (XLSTREAM (stream));
  obj = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		     Lstream_byte_count (XLSTREAM (stream)));
  UNGCPRO;
  Lstream_delete (XLSTREAM (stream));
  return obj;
}

Lisp_Object
emacs_doprnt_string_lisp_2 (const Bufbyte *format_nonreloc,
			    Lisp_Object format_reloc, Bytecount format_length,
			    int nargs, ...)
{
  Lisp_Object obj;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;
  va_list vargs;
  int i;
  Lisp_Object *foo = alloca_array (Lisp_Object, nargs);

  va_start (vargs, nargs);
  for (i = 0; i < nargs; i++)
    foo[i] = va_arg (vargs, Lisp_Object);
  va_end (vargs);

  GCPRO1 (stream);
  emacs_doprnt_2 (stream, format_nonreloc, format_reloc,
		  format_length, nargs, foo);
  Lstream_flush (XLSTREAM (stream));
  obj = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		     Lstream_byte_count (XLSTREAM (stream)));
  UNGCPRO;
  Lstream_delete (XLSTREAM (stream));
  return obj;
}
