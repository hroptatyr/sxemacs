/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Rewritten by mly to use varargs.h.
   Rewritten from scratch by Ben Wing (February 1995) for Mule; expanded
   to full printf spec.

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Rewritten.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "lstream.h"

static const char *const valid_flags = "-+ #0";
static const char *const valid_converters = "dic" "oxX" "feEgG" "sS"
#if defined(HAVE_MPZ) || defined(HAVE_MPQ)
	"ZQ"
#endif
#if defined(HAVE_MPF) || defined(HAVE_MPFR)
	"FR"
#endif
#if defined(HAVE_PSEUG)
	"B"
#endif
#if defined(HAVE_MPC)
	"C"
#endif
	;
static const char *const int_converters = "dic";
static const char *const base_converters = "oxX";
static const char *const double_converters = "feEgG";
static const char *const string_converters = "sS";
#if defined(HAVE_MPZ) || defined(HAVE_MPQ)
static const char *const bigz_converters = "ZQ";
#endif
#if defined(HAVE_MPF) || defined(HAVE_MPFR)
static const char *const bigf_converters = "FR";
#endif
#if defined(HAVE_PSEUG)
static const char *const bigg_converters = "B";
#endif
#if defined(HAVE_MPC)
static const char *const bigc_converters = "C";
#endif


typedef struct printf_spec printf_spec;
struct printf_spec {
	int argnum;		/* which argument does this spec want?  This is one-based:
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
	char converter;		/* converter character or 0 for dummy marker
				   indicating literal text at the end of the
				   specification */
	Bytecount text_before;	/* position of the first character of the
				   block of literal text before this spec */
	Bytecount text_before_len;	/* length of that text */
};

typedef union printf_arg printf_arg;
union printf_arg {
	long l;
	unsigned long ul;
	double d;
	Bufbyte *bp;
	Lisp_Object obj;
};

/* We maintain a list of all the % specs in the specification,
   along with the offset and length of the block of literal text
   before each spec.  In addition, we have a "dummy" spec that
   represents all the literal text at the end of the specification.
   Its converter is 0. */

typedef struct {
	Dynarr_declare(struct printf_spec);
} printf_spec_dynarr;

typedef struct {
	Dynarr_declare(union printf_arg);
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
doprnt_1(Lisp_Object stream, const Bufbyte * string, Bytecount len,
	 Charcount minlen, Charcount maxlen, int minus_flag, int zero_flag)
{
	Lstream *lstr = XLSTREAM(stream);
	Charcount cclen = bytecount_to_charcount(string, len);
	int to_add = minlen - cclen;

	/* Padding at beginning to right-justify ... */
	if (!minus_flag)
		while (to_add-- > 0)
			Lstream_putc(lstr, zero_flag ? '0' : ' ');

	if (0 <= maxlen && maxlen < cclen)
		len = charcount_to_bytecount(string, maxlen);
	Lstream_write(lstr, string, len);

	/* Padding at end to left-justify ... */
	if (minus_flag)
		while (to_add-- > 0)
			Lstream_putc(lstr, zero_flag ? '0' : ' ');
}

static const Bufbyte *parse_off_posnum(const Bufbyte * start,
				       const Bufbyte * end, int *returned_num)
{
	Bufbyte arg_convert[100];
	REGISTER Bufbyte *arg_ptr = arg_convert;

	*returned_num = -1;
	while (start != end && isdigit(*start)) {
		if ((size_t) (arg_ptr - arg_convert) >= sizeof(arg_convert) - 1)
			error("Format converter number too large");
		*arg_ptr++ = *start++;
	}
	*arg_ptr = '\0';
	if (arg_convert != arg_ptr)
		*returned_num = atoi((char *)arg_convert);
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

static printf_spec_dynarr *parse_doprnt_spec(const Bufbyte * format,
					     Bytecount format_length)
{
	const Bufbyte *fmt = format;
	const Bufbyte *fmt_end = format + format_length;
	printf_spec_dynarr *specs = Dynarr_new(printf_spec);
	int prev_argnum = 0;

	while (1) {
		struct printf_spec spec;
		const Bufbyte *text_end;
		Bufbyte ch;

		xzero(spec);
		if (fmt == fmt_end)
			return specs;
		text_end = (Bufbyte *) memchr(fmt, '%', fmt_end - fmt);
		if (!text_end)
			text_end = fmt_end;
		spec.text_before = fmt - format;
		spec.text_before_len = text_end - fmt;
		fmt = text_end;
		if (fmt != fmt_end) {
			fmt++;	/* skip over % */

			/* A % is special -- no arg number.  According to ANSI specs,
			   field width does not apply to %% conversion. */
			if (fmt != fmt_end && *fmt == '%') {
				spec.converter = '%';
				Dynarr_add(specs, spec);
				fmt++;
				continue;
			}

			/* Is there a field number specifier? */
			{
				const Bufbyte *ptr;
				int fieldspec;

				ptr =
				    parse_off_posnum(fmt, fmt_end, &fieldspec);
				if (fieldspec > 0 && ptr != fmt_end
				    && *ptr == '$') {
					/* There is a format specifier */
					prev_argnum = fieldspec;
					fmt = ptr + 1;
				} else
					prev_argnum++;
				spec.argnum = prev_argnum;
			}

			/* Parse off any flags */
			NEXT_ASCII_BYTE(ch);
			while (strchr(valid_flags, ch)) {
				switch (ch) {
				case '-':
					spec.minus_flag = 1;
					break;
				case '+':
					spec.plus_flag = 1;
					break;
				case ' ':
					spec.space_flag = 1;
					break;
				case '#':
					spec.number_flag = 1;
					break;
				case '0':
					spec.zero_flag = 1;
					break;
				default:
					abort();
				}
				NEXT_ASCII_BYTE(ch);
			}

			/* Parse off the minimum field width */
			fmt--;	/* back up */

			/*
			 * * means the field width was passed as an argument.
			 * Mark the current spec as one that forwards its
			 * field width and flags to the next spec in the array.
			 * Then create a new spec and continue with the parsing.
			 */
			if (fmt != fmt_end && *fmt == '*') {
				spec.converter = '*';
				RESOLVE_FLAG_CONFLICTS(spec);
				Dynarr_add(specs, spec);
				xzero(spec);
				spec.argnum = ++prev_argnum;
				fmt++;
			} else {
				fmt =
				    parse_off_posnum(fmt, fmt_end,
						     &spec.minwidth);
				if (spec.minwidth == -1)
					spec.minwidth = 0;
			}

			/* Parse off any precision specified */
			NEXT_ASCII_BYTE(ch);
			if (ch == '.') {
				/*
				 * * means the precision was passed as an argument.
				 * Mark the current spec as one that forwards its
				 * fieldwidth, flags and precision to the next spec in
				 * the array.  Then create a new spec and continue
				 * with the parse.
				 */
				if (fmt != fmt_end && *fmt == '*') {
					spec.converter = '*';
					spec.forwarding_precision = 1;
					RESOLVE_FLAG_CONFLICTS(spec);
					Dynarr_add(specs, spec);
					xzero(spec);
					spec.argnum = ++prev_argnum;
					fmt++;
				} else {
					fmt =
					    parse_off_posnum(fmt, fmt_end,
							     &spec.precision);
					if (spec.precision == -1)
						spec.precision = 0;
				}
				NEXT_ASCII_BYTE(ch);
			} else
				/* No precision specified */
				spec.precision = -1;

			/* Parse off h or l flag */
			if (ch == 'h' || ch == 'l') {
				if (ch == 'h')
					spec.h_flag = 1;
				else
					spec.l_flag = 1;
				NEXT_ASCII_BYTE(ch);
			}

			if (!strchr(valid_converters, ch))
				error("Invalid converter character %c", ch);
			spec.converter = ch;
		}

		RESOLVE_FLAG_CONFLICTS(spec);
		Dynarr_add(specs, spec);
	}

	RETURN_NOT_REACHED(specs)	/* suppress compiler warning */
}

static int get_args_needed(printf_spec_dynarr * specs)
{
	int args_needed = 0;
	REGISTER int i;

	/* Figure out how many args are needed.  This may be less than
	   the number of specs because a spec could be %% or could be
	   missing (literal text at end of format string) or there
	   could be specs where the field number is explicitly given.
	   We just look for the maximum argument number that's referenced. */

	for (i = 0; i < Dynarr_length(specs); i++) {
		char ch = Dynarr_at(specs, i).converter;
		if (ch && ch != '%') {
			int argnum = Dynarr_at(specs, i).argnum;
			if (argnum > args_needed)
				args_needed = argnum;
		}
	}

	return args_needed;
}

static printf_arg_dynarr *get_doprnt_args(printf_spec_dynarr * specs,
					  va_list vargs)
{
	printf_arg_dynarr *args = Dynarr_new(printf_arg);
	union printf_arg arg;
	REGISTER int i;
	int args_needed = get_args_needed(specs);

	xzero(arg);
	for (i = 1; i <= args_needed; i++) {
		int j;
		char ch;
		struct printf_spec *spec = 0;

		for (j = 0; j < Dynarr_length(specs); j++) {
			spec = Dynarr_atp(specs, j);
			if (spec->argnum == i)
				break;
		}

		if (j == Dynarr_length(specs))
			error("No conversion spec for argument %d", i);

		ch = spec->converter;

		if (strchr(int_converters, ch)) {
			if (spec->l_flag)
				arg.l = va_arg(vargs, long);
			else
				/* int even if ch == 'c' or spec->h_flag: "the
				   type used in va_arg is supposed to match the
				   actual type **after default promotions**."
				   Hence we read an int, not a short, if
				   spec->h_flag. */
				arg.l = va_arg(vargs, int);
		} else if (strchr(base_converters, ch)) {
			if (spec->l_flag)
				arg.l = va_arg(vargs, int);
			else
				/* unsigned int even if ch == 'c' or spec->h_flag */
				arg.l =
				    va_arg(vargs, int);
		} else if (strchr(double_converters, ch))
			arg.d = va_arg(vargs, double);
		else if (strchr(string_converters, ch))
			arg.bp = va_arg(vargs, Bufbyte *);
#if defined(HAVE_MPZ) || defined(HAVE_MPQ)
		else if (strchr(bigz_converters, ch))
			arg.obj = va_arg(vargs, Lisp_Object);
#endif
#if defined(HAVE_MPF) || defined(HAVE_MPFR)
		else if (strchr(bigf_converters, ch))
			arg.obj = va_arg(vargs, Lisp_Object);
#endif
		else
			abort();

		Dynarr_add(args, arg);
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


/* we divide the emacs_doprnt_1 into readable chunks */

static void emacs_doprnt_number(
	Lisp_Object, const Lisp_Object *,
	printf_arg_dynarr *, struct printf_spec *, char);


#define DOPRNT_AND_FREE(b, l) do					\
{									\
	doprnt_1(stream, b, l, 0, -1, 0, 0);				\
	free(b);							\
} while (0)

static void
emacs_doprnt_number(Lisp_Object stream,
		    const Lisp_Object *largs,
		    printf_arg_dynarr *args,
		    struct printf_spec *spec,
		    char ch)
{
	/* Must be a number. */
	union printf_arg arg;
	Lisp_Object obj;

	if (!largs) {
		arg = Dynarr_at(args, spec->argnum - 1);
		obj = Qnil;
	} else {
		obj = largs[spec->argnum - 1];
		if (CHARP(obj))
			obj = make_int(XCHAR(obj));
		if (MARKERP(obj))
			obj = make_int(marker_position(obj));
	}

#ifdef WITH_NUMBER_TYPES
	if (!NUMBERP(obj) && !NILP(obj)) {
		error("format specifier %%%c "
		      "doesn't match argument type", ch);
	}
#else
	if (!INT_OR_FLOATP(obj) && !NILP(obj)) {
		error("format specifier %%%c "
		      "doesn't match argument type", ch);
	}
#endif

	if (NILP(obj)) {

	} else if (ch == 'c') {
		/* always convert to int if we deal with characters */
#ifdef WITH_NUMBER_TYPES
		obj = Fcoerce_number(obj, Qint, Qnil);
#else
		obj = Ftruncate(obj);
#endif

	} else if (strchr(int_converters, ch) && (ch != 'c')) {
#ifdef WITH_NUMBER_TYPES
		obj = Fcoerce_number(obj, Qinteger, Qnil);
#else
		obj = Ftruncate(obj);
#endif

	} else if (strchr(base_converters, ch)) {
		/* must that really be int?
		 * The ENT libraries have support for printing floats
		 * or fractions in hex and octal
		 */
#ifdef WITH_NUMBER_TYPES
		obj = Fcoerce_number(obj, Qinteger, Qnil);
#else
		obj = Ftruncate(obj);
#endif

	} else if (strchr(double_converters, ch)) {
#ifdef WITH_NUMBER_TYPES
		obj = Fcoerce_number(obj, Qfloat, Qnil);
#else
		obj = make_float(XFLOATINT(obj));
#endif

#if defined(HAVE_MPZ)
	} else if (ch == 'Z') {
		obj = Fcoerce_number(obj, Qbigz, Qnil);

#endif /* HAVE_MPQ */
#if defined(HAVE_MPQ)
	} else if (ch == 'Q') {
		obj = Fcoerce_number(obj, Qbigq, Qnil);

#endif /* HAVE_MPQ */
#if defined(HAVE_MPFR)
	} else if (ch == 'F') {
		obj = Fcoerce_number(obj, Qbigfr, Qnil);

#elif defined(HAVE_MPF)
	} else if (ch == 'F') {
		obj = Fcoerce_number(obj, Qbigf, Qnil);

#endif /* HAVE_MPFR || HAVE_MPF */
#if defined(HAVE_MPFR) || defined(HAVE_MPF)
	} else if (ch == 'R') {
		obj = Fcoerce_number(obj, Qreal, Qnil);

		if (FLOATP(obj)) {
			ch = 'f';
		}

#endif
#if defined(HAVE_PSEUG)
	} else if (strchr(bigg_converters, ch)) {
		obj = Fcoerce_number(obj, Qbigg, Qnil);

#endif
#if defined(HAVE_MPC)
	} else if (strchr(bigc_converters, ch)) {
		obj = Fcoerce_number(obj, Qbigc, Qnil);

#endif
	}

	if (0) {
		;

	} else if ((NILP(obj) || INTP(obj)) && ch == 'c') {
		Emchar a;
		Bytecount charlen;
		Bufbyte charbuf[MAX_EMCHAR_LEN];

		if (NILP(obj))
			a = (Emchar)arg.l;
		else
			a = (Emchar)XINT(obj);

		if (!valid_char_p(a))
			error("invalid character value %d to %%c spec", a);

		charlen = set_charptr_emchar(charbuf, a);
		doprnt_1(stream, charbuf, charlen,
			 spec->minwidth, -1, spec->minus_flag,
			 spec->zero_flag);
		return;

	} else if ((NILP(obj) || FLOATP(obj)) &&
		   strchr(double_converters, ch)) {

		/* ASCII Decimal representation uses 2.4 times as many
		   bits as machine binary.  */
		char *text_to_print;
		char constructed_spec[100];
		char *p = constructed_spec;

		text_to_print = alloca_array(
			char, 32 + max(spec->minwidth,
				       (EMACS_INT)max(
					       sizeof(double), sizeof(long))
				       * 3 + max(spec->precision, 0)));

		/* Mostly reconstruct the spec and use sprintf() to
		   format the string. */

		*p++ = '%';
		if (spec->plus_flag)
			*p++ = '+';
		if (spec->space_flag)
			*p++ = ' ';
		if (spec->number_flag)
			*p++ = '#';
		if (spec->minus_flag)
			*p++ = '-';
		if (spec->zero_flag)
			*p++ = '0';

		if (spec->minwidth >= 0) {
			long_to_string(p, spec->minwidth);
			p += strlen (p);
		}
		if (spec->precision >= 0) {
			*p++ = '.';
			long_to_string(p, spec->precision);
			p += strlen (p);
		}

		*p++ = ch;
		*p++ = '\0';
		if (NILP(obj))
			sprintf(text_to_print, constructed_spec,
				arg.d);
		else
			sprintf(text_to_print, constructed_spec,
				XFLOAT_DATA(obj));

		doprnt_1(stream, (Bufbyte *)text_to_print,
			 strlen(text_to_print), 0, -1, 0, 0);
		return;

	} else if ((NILP(obj) || INTP(obj)) && (ch != 'c')) {
		/* ASCII Decimal representation uses 2.4 times as many
		   bits as machine binary.  */
		char *text_to_print;
		int ttp_len;
		int cruft_len;
		EMACS_INT XINTobj;
		char constructed_spec[100];
		char *p = constructed_spec;

		text_to_print = alloca_array(
			char, 32 + max((EMACS_INT)spec->minwidth,
				       (EMACS_INT)sizeof(long)) * 3 +
			max(spec->precision, 0));

		*p++ = '%';
		*p++ = 'l';	/* use long */
		*p++ = ch;
		*p++ = '\0';

		if (NILP(obj))
			XINTobj = arg.l;
		else
			XINTobj = XINT(obj);

		if (strchr(base_converters, ch) && XINTobj < 0) {
			sprintf(text_to_print, constructed_spec, -XINTobj);
			/* We cannot simply use sprintf,
			 * sprintf would return a two-complement
			 * on negative numbers
			 */
			ttp_len = strlen(text_to_print);
			memmove(text_to_print+1, text_to_print, ttp_len);
			/* insert the sign */
			text_to_print[0] = '-';
			ttp_len++;
			cruft_len = 1;
		} else {
			sprintf(text_to_print, constructed_spec, XINTobj);
			ttp_len = strlen(text_to_print);
			cruft_len = 0;
		}

		if (ttp_len < spec->precision) {
			memmove(text_to_print+spec->precision-ttp_len,
				text_to_print, ttp_len);
			memset(text_to_print, '0',
			       spec->precision-ttp_len);
			ttp_len = spec->precision;
		}

		/* now maybe print the signed or spaced version */
		if ((spec->plus_flag ||
		     (spec->space_flag &&
		      strchr(base_converters, ch) &&
		      !spec->number_flag) ||
		     (spec->space_flag &&
		      !strchr(base_converters, ch))) && XINT(obj) >= 0) {
			memmove(text_to_print+1, text_to_print, ttp_len);
			ttp_len++;
			if (spec->plus_flag)
				text_to_print[0] = '+';
			else if (spec->space_flag)
				text_to_print[0] = ' ';
			cruft_len++;
		}

		if (spec->number_flag && strchr(base_converters, ch)) {
			memmove(text_to_print+2, text_to_print, ttp_len);
			text_to_print[0] = '0';
			/* the idea behind that is to just swap the
			 * leading zero with a # et voila the number
			 * can be read in again
			 */
			switch (ch) {
			case 'o':
				text_to_print[1] = 'o';
				break;
			case 'x':
			case 'X':
				text_to_print[1] = 'x';
				break;
			default:
				/* unknown */
				text_to_print[1] = 'u';
				break;
			}
			ttp_len += 2;
			cruft_len += 2;
		}

		/* care about spec->minwidth */
		if (ttp_len < spec->minwidth) {
			if (spec->minus_flag)
				memset(text_to_print+ttp_len, ' ',
				       spec->minwidth-ttp_len);
			else {
				if (spec->zero_flag && spec->precision <= 0) {
					/* regard the signum as cruft */
					if (XINTobj < 0)
						cruft_len++;
					/* dont move the cruft around */
					memmove(text_to_print+cruft_len+
						spec->minwidth-ttp_len,
						text_to_print+cruft_len,
						ttp_len-cruft_len);
					memset(text_to_print+cruft_len, '0',
					       spec->minwidth-ttp_len);
				} else {
					memmove(text_to_print+
						spec->minwidth-ttp_len,
						text_to_print, ttp_len);
					memset(text_to_print, ' ',
					       spec->minwidth-ttp_len);
				}
			}
			ttp_len = spec->minwidth;
		}

		doprnt_1(stream, (Bufbyte*)text_to_print, ttp_len, 0, -1, 0, 0);
		return;

#ifdef HAVE_MPZ
	} else if (BIGZP(obj)) {
		Bufbyte *text_to_print;
		int ttp_len;
		int cruft_len;
		int base;
		int sign;

		switch (ch) {
		case 'o':
			base = 8;
			break;
		case 'x':
		case 'X':
			base = 16;
			break;
		default:
			base = 10;
		}

		sign = bigz_sign(XBIGZ_DATA(obj));
		text_to_print =
			(Bufbyte*)bigz_to_string(XBIGZ_DATA(obj), base);
		ttp_len = strlen((char*)text_to_print);
		cruft_len = 0;

		if (ttp_len < spec->precision) {
			XREALLOC_ARRAY(text_to_print, Bufbyte, spec->precision);
			memmove(text_to_print+spec->precision-ttp_len,
				text_to_print, ttp_len);
			memset(text_to_print, '0',
			       spec->precision-ttp_len);
			ttp_len = spec->precision;
		}

		/* now maybe print the signed or spaced version */
		if ((spec->plus_flag ||
		     (spec->space_flag &&
		      strchr(base_converters, ch) &&
		      !spec->number_flag) ||
		     (spec->space_flag &&
		      !strchr(base_converters, ch))) && sign>=0) {
			XREALLOC_ARRAY(text_to_print, Bufbyte, ttp_len + 1);
			memmove(text_to_print+1, text_to_print, ttp_len);
			ttp_len++;
			if (spec->plus_flag)
				text_to_print[0] = '+';
			if (spec->space_flag)
				text_to_print[0] = ' ';
			cruft_len++;
		}

		if (spec->number_flag && strchr(base_converters, ch)) {
			XREALLOC_ARRAY(text_to_print, Bufbyte, ttp_len+2);
			memmove(text_to_print+2, text_to_print, ttp_len);
			text_to_print[0] = '0';
			switch (ch) {
			case 'o':
				text_to_print[1] = 'o';
				break;
			case 'x':
			case 'X':
				text_to_print[1] = 'x';
				break;
			default:
				/* unknown */
				text_to_print[1] = 'u';
				break;
			}
			ttp_len += 2;
			cruft_len += 2;
		}

		/* special case %X, MPZ does not upcase hex chars,
		 * so we have to do it here
		 */
		if (ch == 'X') {
			char *q;
			for (q = (char*)text_to_print; *q != '\0'; q++) {
				if (strchr("abcdef", *q))
					*q -= 32;
			}
		}

		/* care about spec->minwidth */
		if (ttp_len < spec->minwidth) {
			XREALLOC_ARRAY(text_to_print, Bufbyte, spec->minwidth);
			if (spec->minus_flag)
				memset(text_to_print+ttp_len, ' ',
				       spec->minwidth-ttp_len);
			else {
				if (spec->zero_flag && spec->precision <= 0) {
					/* regard the signum as cruft */
					if (sign < 0)
						cruft_len++;
					/* dont move the cruft around */
					memmove(text_to_print+cruft_len+
						spec->minwidth-ttp_len,
						text_to_print+cruft_len,
						ttp_len-cruft_len);
					memset(text_to_print+cruft_len, '0',
					       spec->minwidth-ttp_len);
				} else {
					memmove(text_to_print+
						spec->minwidth-ttp_len,
						text_to_print, ttp_len);
					memset(text_to_print, ' ',
					       spec->minwidth-ttp_len);
				}
			}
			ttp_len = spec->minwidth;
		}

		DOPRNT_AND_FREE(text_to_print, ttp_len);
		return;
#endif	/* HAVE_MPZ */
#ifdef HAVE_MPQ
	} else if (BIGQP(obj)) {
		Bufbyte *text_to_print;
		int ttp_len;
		int base;

		switch (ch) {
		case 'o':
			base = 8;
			break;
		case 'x':
		case 'X':
			base = 16;
			break;
		default:
			base = 10;
		}

		text_to_print =
			(Bufbyte*)bigq_to_string(XBIGQ_DATA(obj), base);
		ttp_len = strlen((char*)text_to_print);

		/* now maybe print the signed or spaced version */
		if ((spec->plus_flag || spec->space_flag) &&
		    (bigq_sign(XBIGQ_DATA(obj))>=0)) {
			XREALLOC_ARRAY(text_to_print, Bufbyte,
				       ttp_len + 1);
			memmove(text_to_print+1, text_to_print, ttp_len);
			ttp_len++;
			if (spec->plus_flag)
				text_to_print[0] = '+';
			if (spec->space_flag)
				text_to_print[0] = ' ';
		}

		/* care about spec->minwidth */
		if (ttp_len < spec->minwidth) {
			XREALLOC_ARRAY(text_to_print, Bufbyte, spec->minwidth);
			if (spec->minus_flag)
				memset(text_to_print+ttp_len, ' ',
				       spec->minwidth-ttp_len);
			else {
				memmove(text_to_print+spec->minwidth-ttp_len,
					text_to_print, ttp_len);
				if (spec->zero_flag && spec->precision <= 0)
					memset(text_to_print, '0',
					       spec->minwidth-ttp_len);
				else
					memset(text_to_print, ' ',
					       spec->minwidth-ttp_len);
			}
			ttp_len = spec->minwidth;
		}

		DOPRNT_AND_FREE(text_to_print, ttp_len);
		return;
#endif	/* HAVE_MPZ */
#ifdef HAVE_MPFR
	} else if (BIGFRP(obj)) {
		Bufbyte *text_to_print;
		int ttp_len;
		long preradix_len, postradix_len;
		int base;

		switch (ch) {
		case 'o':
			base = 8;
			break;
		case 'x':
		case 'X':
			base = 16;
			break;
		default:
			base = 10;
		}

		text_to_print =
			(Bufbyte*)bigfr_to_string(XBIGFR_DATA(obj), base);
		ttp_len = strlen((char*)text_to_print);

		/* if obj is an infinite point or not-a-number dont care about
		 * precision flags,
		 * also dont care about space or plus flag since the infinities
		 * always carry their sign, and not-a-number cannot have a sign
		 */
		if (bigfr_nan_p(XBIGFR_DATA(obj)) ||
		    bigfr_inf_p(XBIGFR_DATA(obj))) {
			DOPRNT_AND_FREE(text_to_print, ttp_len);
			return;
		}

		/* examine the lengths of digits before and after
		 * the decimal dot
		 */
		if ((preradix_len = (long)strchr((char *)text_to_print, '.'))) {
			preradix_len = preradix_len - (long)text_to_print;
			postradix_len = ttp_len - preradix_len - 1;
		} else {
			preradix_len = ttp_len;
			postradix_len = 0;
		}

		/* now cut unwanted places after the decimal dot */
		if (postradix_len > spec->precision &&
		    spec->precision >= 0) {
			text_to_print[ttp_len -
				      postradix_len +
				      spec->precision] = '\0';
			ttp_len = ttp_len - postradix_len + spec->precision;
			if (spec->precision == 0) {
				text_to_print[ttp_len] = '\0';
				ttp_len--;
			}

		/* now extend to wanted places after the decimal dot */
		} else if (postradix_len < spec->precision &&
			   postradix_len > 0) {
			XREALLOC_ARRAY(text_to_print, Bufbyte,
				       ttp_len - postradix_len +
				       spec->precision);
			text_to_print[preradix_len] = '.';
			memset(text_to_print+ttp_len, '0',
			       spec->precision - postradix_len);
			ttp_len = ttp_len - postradix_len + spec->precision;

		/* now extend to wanted places, insert a decimal dot first */
		} else if (postradix_len < spec->precision &&
			   postradix_len == 0) {
			XREALLOC_ARRAY(text_to_print, Bufbyte,
				       ttp_len + spec->precision + 1);
			text_to_print[preradix_len] = '.';
			memset(text_to_print+preradix_len+1, '0',
			       spec->precision);
			ttp_len = ttp_len + spec->precision + 1;
		}

		/* now maybe print the signed or spaced version */
		if ((spec->plus_flag || spec->space_flag) &&
		    (bigfr_sign(XBIGFR_DATA(obj))>=0)) {
			XREALLOC_ARRAY(text_to_print, Bufbyte,
				       ttp_len + 1);
			memmove(text_to_print+1, text_to_print, ttp_len);
			ttp_len++;
			if (spec->plus_flag)
				text_to_print[0] = '+';
			if (spec->space_flag)
				text_to_print[0] = ' ';
		}

		/* care about spec->minwidth */
		if (ttp_len < spec->minwidth) {
			XREALLOC_ARRAY(text_to_print, Bufbyte, spec->minwidth);
			if (spec->minus_flag)
				memset(text_to_print+ttp_len, ' ',
				       spec->minwidth-ttp_len);
			else {
				memmove(text_to_print+spec->minwidth-ttp_len,
					text_to_print, ttp_len);
				if (spec->zero_flag && spec->precision <= 0)
					memset(text_to_print, '0',
					       spec->minwidth-ttp_len);
				else
					memset(text_to_print, ' ',
					       spec->minwidth-ttp_len);
			}
			ttp_len = spec->minwidth;
		}

		DOPRNT_AND_FREE(text_to_print, ttp_len);
		return;
#endif	/* HAVE_MPFR */
#ifdef HAVE_PSEUG
	} else if (BIGGP(obj)) {

		int old_argnum, old_plus_flag, old_space_flag;
		Lisp_Object *modobj = alloca_array(Lisp_Object, 1);

		/* Actually, %a.bB is a rewrite for %a.bd%+a.bd */

		old_argnum = spec->argnum;
		old_plus_flag = spec->plus_flag;
		old_space_flag = spec->space_flag;

		/* rewrite the real part */
		spec->argnum = 1;
		modobj[0] = Freal_part(obj);
		emacs_doprnt_number(stream, modobj, args, spec, 'Z');

		/* rewrite the imaginary part */
		spec->argnum = 1;
		spec->plus_flag = 1;
		spec->space_flag = 0;
		modobj[0] = Fimaginary_part(obj);
		emacs_doprnt_number(stream, modobj, args, spec, 'Z');
		/* print the imaginary unit now */
		doprnt_1(stream, (Bufbyte*)"i", 1, 1, -1, 0, 0);

		spec->argnum = old_argnum;
		spec->plus_flag = old_plus_flag;
		spec->space_flag = old_space_flag;
		return;
#endif	/* HAVE_PSEUG */
#ifdef HAVE_MPC
	} else if (BIGCP(obj)) {

		int old_argnum, old_plus_flag, old_space_flag;
		Lisp_Object *modobj = alloca_array(Lisp_Object, 1);

		/* Actually, %a.bC is a rewrite for %a.bF%+a.bF */

		old_argnum = spec->argnum;
		old_plus_flag = spec->plus_flag;
		old_space_flag = spec->space_flag;

		/* rewrite the real part */
		spec->argnum = 1;
		modobj[0] = Freal_part(obj);
		emacs_doprnt_number(stream, modobj, args, spec, 'F');

		/* rewrite the imaginary part */
		spec->argnum = 1;
		spec->plus_flag = 1;
		spec->space_flag = 0;
		modobj[0] = Fimaginary_part(obj);
		emacs_doprnt_number(stream, modobj, args, spec, 'F');
		/* print the imaginary unit now */
		doprnt_1(stream, (Bufbyte*)"i", 1, 1, -1, 0, 0);

		spec->argnum = old_argnum;
		spec->plus_flag = old_plus_flag;
		spec->space_flag = old_space_flag;
		return;
#endif	/* HAVE_MPC */
	}
}


static Bytecount
emacs_doprnt_1(Lisp_Object stream, const Bufbyte * format_nonreloc,
	       Lisp_Object format_reloc, Bytecount format_length, int nargs,
	       /* #### Gag me, gag me, gag me */
	       const Lisp_Object * largs, va_list vargs)
{
	printf_spec_dynarr *specs = 0;
	printf_arg_dynarr *args = 0;
	REGISTER int i;
	int init_byte_count = Lstream_byte_count(XLSTREAM(stream));

	if (!NILP(format_reloc)) {
		format_nonreloc = XSTRING_DATA(format_reloc);
		format_length = XSTRING_LENGTH(format_reloc);
	}
	if (format_length < 0)
		format_length =
		    (Bytecount) strlen((const char *)format_nonreloc);

	specs = parse_doprnt_spec(format_nonreloc, format_length);
	if (largs) {
		/* allow too many args for string, but not too few */
		if (nargs < get_args_needed(specs))
			signal_error(Qwrong_number_of_arguments,
				     list3(Qformat,
					   make_int(nargs),
					   !NILP(format_reloc) ? format_reloc :
					   make_string(format_nonreloc,
						       format_length)));
	} else {
		args = get_doprnt_args(specs, vargs);
	}

	for (i = 0; i < Dynarr_length(specs); i++) {
		struct printf_spec *spec = Dynarr_atp(specs, i);
		char ch;

		/* Copy the text before */
		if (!NILP(format_reloc))	/* refetch in case of GC below */
			format_nonreloc = XSTRING_DATA(format_reloc);

		doprnt_1(stream, format_nonreloc + spec->text_before,
			 spec->text_before_len, 0, -1, 0, 0);

		ch = spec->converter;

		if (!ch)
			continue;

		if (ch == '%') {
			doprnt_1(stream, (Bufbyte *) & ch, 1, 0, -1, 0, 0);
			continue;
		}

		/* The char '*' as converter means the field width, precision
		   was specified as an argument.  Extract the data and forward
		   it to the next spec, to which it will apply.  */
		if (ch == '*') {
			struct printf_spec *nextspec = Dynarr_atp(specs, i + 1);
			Lisp_Object obj = largs[spec->argnum - 1];

			if (INTP(obj)) {
				if (spec->forwarding_precision) {
					nextspec->precision = XINT(obj);
					nextspec->minwidth = spec->minwidth;
				} else {
					nextspec->minwidth = XINT(obj);
					if (XINT(obj) < 0) {
						spec->minus_flag = 1;
						nextspec->minwidth =
						    -nextspec->minwidth;
					}
				}
				nextspec->minus_flag = spec->minus_flag;
				nextspec->plus_flag = spec->plus_flag;
				nextspec->space_flag = spec->space_flag;
				nextspec->number_flag = spec->number_flag;
				nextspec->zero_flag = spec->zero_flag;
			}
			continue;
		}

		if (largs && (spec->argnum < 1 || spec->argnum > nargs))
			error("Invalid repositioning argument %d",
			      spec->argnum);

		else if (ch == 'S' || ch == 's') {
			Bufbyte *string;
			Bytecount string_len;

			if (!largs) {
				string = Dynarr_at(args, spec->argnum - 1).bp;
				/* error() can be called with null string
				   arguments.  E.g., in fileio.c, the return
				   value of strerror() is never checked.  We'll
				   print (null), like some printf
				   implementations do.  Would it be better (and
				   safe) to signal an error instead?  Or should
				   we just use the empty string?
				   -dkindred@cs.cmu.edu 8/1997
				 */
				if (!string)
					string = (Bufbyte *) "(null)";
				string_len = strlen((char *)string);
			} else {
				Lisp_Object obj = largs[spec->argnum - 1];
				Lisp_String *ls;

				if (ch == 'S') {
					/* For `S', prin1 the argument and
					 * then treat like a string.
					 */
					ls = XSTRING(Fprin1_to_string
						     (obj, Qnil));
				} else if (STRINGP(obj))
					ls = XSTRING(obj);
				else if (SYMBOLP(obj))
					ls = XSYMBOL(obj)->name;
				else {
					/* convert to string using princ. */
					ls = XSTRING(Fprin1_to_string(obj, Qt));
				}
				string = string_data(ls);
				string_len = string_length(ls);
			}

			doprnt_1(stream, string, string_len, spec->minwidth,
				 spec->precision, spec->minus_flag,
				 spec->zero_flag);
		} else {
			/* Must be a number. */
			emacs_doprnt_number(stream, largs, args, spec, ch);
		}
	}

	/* #### will not get freed if error */
	if (specs)
		Dynarr_free(specs);
	if (args)
		Dynarr_free(args);
	return Lstream_byte_count(XLSTREAM(stream)) - init_byte_count;
}

/* You really don't want to know why this is necessary... */
static Bytecount
emacs_doprnt_2(Lisp_Object stream, const Bufbyte * format_nonreloc,
	       Lisp_Object format_reloc, Bytecount format_length, int nargs,
	       const Lisp_Object * largs, ...)
{
	va_list vargs;
	Bytecount val;
	va_start(vargs, largs);
	val = emacs_doprnt_1(stream, format_nonreloc, format_reloc,
			     format_length, nargs, largs, vargs);
	va_end(vargs);
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
emacs_doprnt_c(Lisp_Object stream, const Bufbyte * format_nonreloc,
	       Lisp_Object format_reloc, Bytecount format_length, ...)
{
	int val;
	va_list vargs;

	va_start(vargs, format_length);
	val = emacs_doprnt_1(stream, format_nonreloc, format_reloc,
			     format_length, 0, 0, vargs);
	va_end(vargs);
	return val;
}

/* Like emacs_doprnt_c but the args come in va_list format. */

Bytecount
emacs_doprnt_va(Lisp_Object stream, const Bufbyte * format_nonreloc,
		Lisp_Object format_reloc, Bytecount format_length,
		va_list vargs)
{
	return emacs_doprnt_1(stream, format_nonreloc, format_reloc,
			      format_length, 0, 0, vargs);
}

/* Like emacs_doprnt_c but the args are Lisp objects instead of
   C arguments.  This causes somewhat different behavior from
   the above two functions (which should act like printf).
   See `format' for a description of this behavior. */

Bytecount
emacs_doprnt_lisp(Lisp_Object stream, const Bufbyte * format_nonreloc,
		  Lisp_Object format_reloc, Bytecount format_length,
		  int nargs, const Lisp_Object * largs)
{
	return emacs_doprnt_2(stream, format_nonreloc, format_reloc,
			      format_length, nargs, largs);
}

/* Like the previous function but takes a variable number of arguments. */

Bytecount
emacs_doprnt_lisp_2(Lisp_Object stream, const Bufbyte * format_nonreloc,
		    Lisp_Object format_reloc, Bytecount format_length,
		    int nargs, ...)
{
	va_list vargs;
	int i;
	Lisp_Object *foo = alloca_array(Lisp_Object, nargs);

	va_start(vargs, nargs);
	for (i = 0; i < nargs; i++)
		foo[i] = va_arg(vargs, Lisp_Object);
	va_end(vargs);

	return emacs_doprnt_2(stream, format_nonreloc, format_reloc,
			      format_length, nargs, foo);
}

/* The following four functions work like the above three but
   return their output as a Lisp string instead of sending it
   to a stream. */

Lisp_Object
emacs_doprnt_string_c(const Bufbyte * format_nonreloc,
		      Lisp_Object format_reloc, Bytecount format_length, ...)
{
	va_list vargs;
	Lisp_Object obj;
	Lisp_Object stream = make_resizing_buffer_output_stream();
	struct gcpro gcpro1;

	GCPRO1(stream);
	va_start(vargs, format_length);
	emacs_doprnt_1(stream, format_nonreloc, format_reloc,
		       format_length, 0, 0, vargs);
	va_end(vargs);
	Lstream_flush(XLSTREAM(stream));
	obj = make_string(resizing_buffer_stream_ptr(XLSTREAM(stream)),
			  Lstream_byte_count(XLSTREAM(stream)));
	UNGCPRO;
	Lstream_delete(XLSTREAM(stream));
	return obj;
}

Lisp_Object
emacs_doprnt_string_va(const Bufbyte * format_nonreloc,
		       Lisp_Object format_reloc, Bytecount format_length,
		       va_list vargs)
{
	/* I'm fairly sure that this function cannot actually GC.
	   That can only happen when the arguments to emacs_doprnt_1() are
	   Lisp objects rather than C args. */
	Lisp_Object obj;
	Lisp_Object stream = make_resizing_buffer_output_stream();
	struct gcpro gcpro1;

	GCPRO1(stream);
	emacs_doprnt_1(stream, format_nonreloc, format_reloc,
		       format_length, 0, 0, vargs);
	Lstream_flush(XLSTREAM(stream));
	obj = make_string(resizing_buffer_stream_ptr(XLSTREAM(stream)),
			  Lstream_byte_count(XLSTREAM(stream)));
	UNGCPRO;
	Lstream_delete(XLSTREAM(stream));
	return obj;
}

Lisp_Object
emacs_doprnt_string_lisp(const Bufbyte * format_nonreloc,
			 Lisp_Object format_reloc, Bytecount format_length,
			 int nargs, const Lisp_Object * largs)
{
	Lisp_Object obj;
	Lisp_Object stream = make_resizing_buffer_output_stream();
	struct gcpro gcpro1;

	GCPRO1(stream);
	emacs_doprnt_2(stream, format_nonreloc, format_reloc,
		       format_length, nargs, largs);
	Lstream_flush(XLSTREAM(stream));
	obj = make_string(resizing_buffer_stream_ptr(XLSTREAM(stream)),
			  Lstream_byte_count(XLSTREAM(stream)));
	UNGCPRO;
	Lstream_delete(XLSTREAM(stream));
	return obj;
}

Lisp_Object
emacs_doprnt_string_lisp_2(const Bufbyte * format_nonreloc,
			   Lisp_Object format_reloc, Bytecount format_length,
			   int nargs, ...)
{
	Lisp_Object obj;
	Lisp_Object stream = make_resizing_buffer_output_stream();
	struct gcpro gcpro1;
	va_list vargs;
	int i;
	Lisp_Object *foo = alloca_array(Lisp_Object, nargs);

	va_start(vargs, nargs);
	for (i = 0; i < nargs; i++)
		foo[i] = va_arg(vargs, Lisp_Object);
	va_end(vargs);

	GCPRO1(stream);
	emacs_doprnt_2(stream, format_nonreloc, format_reloc,
		       format_length, nargs, foo);
	Lstream_flush(XLSTREAM(stream));
	obj = make_string(resizing_buffer_stream_ptr(XLSTREAM(stream)),
			  Lstream_byte_count(XLSTREAM(stream)));
	UNGCPRO;
	Lstream_delete(XLSTREAM(stream));
	return obj;
}
