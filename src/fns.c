/* Random utility Lisp functions.
   Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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


/* Synched up with: Mule 2.0, FSF 19.30. */

/* This file has been Mule-ized. */

/* Note: FSF 19.30 has bool vectors.  We have bit vectors. */

/* Hacked on for Mule by Ben Wing, December 1994, January 1995. */

#include <config.h>

/* Note on some machines this defines `vector' as a typedef,
   so make sure we don't use that name in this file.  */
#undef vector
#define vector *****

#include "lisp.h"

#include "sysfile.h"

#include "buffer.h"
#include "bytecode.h"
#include "ui/device.h"
#include "events/events.h"
#include "extents.h"
#include "ui/frame.h"
#include "systime.h"
#include "ui/insdel.h"
#include "lstream.h"
/* for the categorial views */
#include "category.h"
#include "seq.h"
/* for all the map* funs */
#include "map.h"


/* NOTE: This symbol is also used in lread.c */
#define FEATUREP_SYNTAX

Lisp_Object Qstring_lessp, Qstring_greaterp;
Lisp_Object Qidentity;

static int internal_old_equal(Lisp_Object, Lisp_Object, int);
Lisp_Object safe_copy_tree(Lisp_Object arg, Lisp_Object vecp, int depth);
int internal_equalp(Lisp_Object, Lisp_Object, int);

static Lisp_Object mark_bit_vector(Lisp_Object obj)
{
	return Qnil;
}

static void
print_bit_vector(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	size_t i;
	Lisp_Bit_Vector *v = XBIT_VECTOR(obj);
	size_t len = bit_vector_length(v);
	size_t last = len;

	if (INTP(Vprint_length))
		last = min((EMACS_INT) len, XINT(Vprint_length));
	write_c_string("#*", printcharfun);
	for (i = 0; i < last; i++) {
		if (bit_vector_bit(v, i))
			write_c_string("1", printcharfun);
		else
			write_c_string("0", printcharfun);
	}

	if (last != len)
		write_c_string("...", printcharfun);
}

static int bit_vector_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Bit_Vector *v1 = XBIT_VECTOR(obj1);
	Lisp_Bit_Vector *v2 = XBIT_VECTOR(obj2);

	return ((bit_vector_length(v1) == bit_vector_length(v2)) &&
		!memcmp(v1->bits, v2->bits,
			BIT_VECTOR_LONG_STORAGE(bit_vector_length(v1)) *
			sizeof(long)));
}

static unsigned long bit_vector_hash(Lisp_Object obj, int depth)
{
	Lisp_Bit_Vector *v = XBIT_VECTOR(obj);
	return HASH2(bit_vector_length(v),
		     memory_hash(v->bits,
				 BIT_VECTOR_LONG_STORAGE(bit_vector_length(v)) *
				 sizeof(long)));
}

static size_t size_bit_vector(const void *lheader)
{
	const Lisp_Bit_Vector *v = (const Lisp_Bit_Vector *) lheader;
	return FLEXIBLE_ARRAY_STRUCT_SIZEOF(Lisp_Bit_Vector, unsigned long,
					    bits,
					    BIT_VECTOR_LONG_STORAGE
					    (bit_vector_length(v)));
}

static const struct lrecord_description bit_vector_description[] = {
	{XD_LISP_OBJECT, offsetof(Lisp_Bit_Vector, next)},
	{XD_END}
};

DEFINE_BASIC_LRECORD_SEQUENCE_IMPLEMENTATION("bit-vector", bit_vector,
					     mark_bit_vector, print_bit_vector,
					     0, bit_vector_equal,
					     bit_vector_hash,
					     bit_vector_description,
					     size_bit_vector, Lisp_Bit_Vector);

DEFUN("identity", Fidentity, 1, 1, 0,	/*
Return the argument unchanged.
*/
      (arg))
{
	return arg;
}

extern long get_random(void);
extern void seed_random(long arg);

DEFUN("random", Frandom, 0, 1, 0,	/*
Return a pseudo-random number.
All integers representable in Lisp are equally likely.
On most systems, this is 31 bits' worth.

With positive integer argument LIMIT, return random number 
in interval [0,LIMIT). LIMIT can be a big integer, in which
case the range of possible values is extended.

With argument t, set the random number seed from the 
current time and pid.
*/
      (limit))
{
	EMACS_INT val;
	unsigned long denominator;

	if (EQ(limit, Qt))
		seed_random(getpid() + time(NULL));
	if (NATNUMP(limit) && !ZEROP(limit)) {
		/* Try to take our random number from the higher bits of VAL,
		   not the lower, since (says Gentzel) the low bits of `random'
		   are less random than the higher ones.  We do this by using the
		   quotient rather than the remainder.  At the high end of the RNG
		   it's possible to get a quotient larger than limit; discarding
		   these values eliminates the bias that would otherwise appear
		   when using a large limit.  */
		denominator = ((unsigned long)1 << INT_VALBITS) / XINT(limit);
		do
			val = get_random() / denominator;
		while (val >= XINT(limit));
	} else if (ZEROP(limit)) {
		return wrong_type_argument(Qpositivep, limit);
#if defined HAVE_MPZ && defined WITH_GMP
	} else if (BIGZP(limit)) {
		bigz bz;
		Lisp_Object result;

		if (bigz_sign(XBIGZ_DATA(limit)) <= 0)
			return wrong_type_argument(Qpositivep, limit);

		bigz_init(bz);

		bigz_random(bz, XBIGZ_DATA(limit));
		result = ent_mpz_downgrade_maybe(bz);

		bigz_fini(bz);
		return result;
#endif	/* HAVE_MPZ */
	} else
		val = get_random();

	return make_int(val);
}

#if defined(WITH_GMP) && defined(HAVE_MPZ)
DEFUN("randomb", Frandomb, 1, 1, 0,	/*
Return a uniform pseudo-random number in the range [0, 2^LIMIT).
*/
      (limit))
{
	bigz bz;
	unsigned long limui;
	Lisp_Object result;

	CHECK_INTEGER(limit);

	if (NILP(Fnonnegativep(limit)))
		return wrong_type_argument(Qnonnegativep, limit);
	else if (INTP(limit))
		limui = XINT(limit);
	else if (BIGZP(limit) && bigz_fits_ulong_p(XBIGZ_DATA(limit)))
		limui = bigz_to_ulong(XBIGZ_DATA(limit));
	else
		return wrong_type_argument(Qintegerp, limit);

	bigz_init(bz);

	mpz_urandomb(bz, random_state, limui);
	result = make_bigz_bz(bz);

	bigz_fini(bz);
	return result;
}
#endif	/* HAVE_MPZ */


/* Random data-structure functions */

#ifdef LOSING_BYTECODE

/* #### Delete this shit */

/* Charcount is a misnomer here as we might be dealing with the
   length of a vector or list, but emphasizes that we're not dealing
   with Bytecounts in strings */
static Charcount length_with_bytecode_hack(Lisp_Object seq)
{
	if (!COMPILED_FUNCTIONP(seq))
		return XINT(Flength(seq));
	else {
		Lisp_Compiled_Function *f = XCOMPILED_FUNCTION(seq);

		return (f->flags.interactivep ? COMPILED_INTERACTIVE :
			f->flags.domainp ? COMPILED_DOMAIN :
			COMPILED_DOC_STRING)
		    + 1;
	}
}

#endif				/* LOSING_BYTECODE */

void check_losing_bytecode(const char *function, Lisp_Object seq)
{
	if (COMPILED_FUNCTIONP(seq))
		error_with_frob
		    (seq,
		     "As of 20.3, `%s' no longer works with compiled-function objects",
		     function);
}

DEFUN("length", Flength, 1, 1, 0,	/*
Return the length of vector, bit vector, list or string SEQUENCE.
*/
      (sequence))
{
#if 1
/* that's whither we have to get */
	if (LIKELY(!NILP(sequence))) {
		return make_int(seq_length((seq_t)sequence));
	} else {
		return Qzero;
	}
#elif 0
retry:
	if (LIKELY(STRINGP(sequence) ||
		   CONSP(sequence) ||
		   VECTORP(sequence) ||
		   DLLISTP(sequence) ||
		   BIT_VECTORP(sequence))) {
		return make_int(seq_length(sequence));
	} else if (NILP(sequence)) {
		return Qzero;
	} else {
		check_losing_bytecode("length", sequence);
		sequence = wrong_type_argument(Qsequencep, sequence);
		goto retry;
	}
#else
retry:
	if (STRINGP(sequence))
		return make_int(XSTRING_CHAR_LENGTH(sequence));
	else if (CONSP(sequence)) {
		return make_int(seq_length(sequence));
	} else if (VECTORP(sequence))
		return make_int(seq_length(sequence));
	else if (DLLISTP(sequence))
		return make_int(XDLLIST_SIZE(sequence));
	else if (NILP(sequence))
		return Qzero;
	else if (BIT_VECTORP(sequence))
		return make_int(bit_vector_length(XBIT_VECTOR(sequence)));
	else {
		check_losing_bytecode("length", sequence);
		sequence = wrong_type_argument(Qsequencep, sequence);
		goto retry;
	}
#endif
}

DEFUN("safe-length", Fsafe_length, 1, 1, 0,	/*
Return the length of a list, but avoid error or infinite loop.
This function never gets an error.  If LIST is not really a list,
it returns 0.  If LIST is circular, it returns a finite value
which is at least the number of distinct elements.
*/
      (list))
{
	Lisp_Object hare, tortoise;
	size_t len;

	for (hare = tortoise = list, len = 0;
	     CONSP(hare) && (!EQ(hare, tortoise) || len == 0);
	     hare = XCDR(hare), len++) {
		if (len & 1)
			tortoise = XCDR(tortoise);
	}

	return make_int(len);
}

/*** string functions. ***/

DEFUN("string-equal", Fstring_equal, 2, 2, 0,	/*
Return t if two strings have identical contents.
Case is significant.  Text properties are ignored.
\(Under SXEmacs, `equal' also ignores text properties and extents in
strings, but this is not the case under FSF Emacs 19.  In FSF Emacs 20
`equal' is the same as in SXEmacs, in that respect.)
Symbols are also allowed; their print names are used instead.
*/
      (string1, string2))
{
	Bytecount len;
	Lisp_String *p1, *p2;

	if (SYMBOLP(string1))
		p1 = XSYMBOL(string1)->name;
	else {
		CHECK_STRING(string1);
		p1 = XSTRING(string1);
	}

	if (SYMBOLP(string2))
		p2 = XSYMBOL(string2)->name;
	else {
		CHECK_STRING(string2);
		p2 = XSTRING(string2);
	}

	return (((len = string_length(p1)) == string_length(p2)) &&
		!memcmp(string_data(p1), string_data(p2), len)) ? Qt : Qnil;
}

DEFUN("string-lessp", Fstring_lessp, 2, 2, 0,	/*
Return t if first arg string is less than second in lexicographic order.
If I18N2 support (but not Mule support) was compiled in, ordering is
determined by the locale. (Case is significant for the default C locale.)
In all other cases, comparison is simply done on a character-by-
character basis using the numeric value of a character. (Note that
this may not produce particularly meaningful results under Mule if
characters from different charsets are being compared.)

Symbols are also allowed; their print names are used instead.

The reason that the I18N2 locale-specific collation is not used under
Mule is that the locale model of internationalization does not handle
multiple charsets and thus has no hope of working properly under Mule.
What we really should do is create a collation table over all built-in
charsets.  This is extremely difficult to do from scratch, however.

Unicode is a good first step towards solving this problem.  In fact,
it is quite likely that a collation table exists (or will exist) for
Unicode.  When Unicode support is added to SXEmacs/Mule, this problem
may be solved.
*/
      (string1, string2))
{
	Lisp_String *p1, *p2;
	Charcount end, len2;
	int i;

	if (SYMBOLP(string1))
		p1 = XSYMBOL(string1)->name;
	else {
		CHECK_STRING(string1);
		p1 = XSTRING(string1);
	}

	if (SYMBOLP(string2))
		p2 = XSYMBOL(string2)->name;
	else {
		CHECK_STRING(string2);
		p2 = XSTRING(string2);
	}

	end = string_char_length(p1);
	len2 = string_char_length(p2);
	if (end > len2)
		end = len2;

#if defined (I18N2) && !defined (MULE)
	/* There is no hope of this working under Mule.  Even if we converted
	   the data into an external format so that strcoll() processed it
	   properly, it would still not work because strcoll() does not
	   handle multiple locales.  This is the fundamental flaw in the
	   locale model. */
	{
		Bytecount bcend = charcount_to_bytecount(string_data(p1), end);
		/* Compare strings using collation order of locale. */
		/* Need to be tricky to handle embedded nulls. */

		for (i = 0; i < bcend;
		     i += strlen((char *)string_data(p1) + i) + 1) {
			int val = strcoll((char *)string_data(p1) + i,
					  (char *)string_data(p2) + i);
			if (val < 0)
				return Qt;
			if (val > 0)
				return Qnil;
		}
	}
#else				/* not I18N2, or MULE */
	{
		Bufbyte *ptr1 = string_data(p1);
		Bufbyte *ptr2 = string_data(p2);

		/* #### It is not really necessary to do this: We could compare
		   byte-by-byte and still get a reasonable comparison, since this
		   would compare characters with a charset in the same way.  With
		   a little rearrangement of the leading bytes, we could make most
		   inter-charset comparisons work out the same, too; even if some
		   don't, this is not a big deal because inter-charset comparisons
		   aren't really well-defined anyway. */
		for (i = 0; i < end; i++) {
			if (charptr_emchar(ptr1) != charptr_emchar(ptr2))
				return charptr_emchar(ptr1) <
				    charptr_emchar(ptr2) ? Qt : Qnil;
			INC_CHARPTR(ptr1);
			INC_CHARPTR(ptr2);
		}
	}
#endif				/* not I18N2, or MULE */
	/* Can't do i < len2 because then comparison between "foo" and "foo^@"
	   won't work right in I18N2 case */
	return end < len2 ? Qt : Qnil;
}

DEFUN("string-greaterp", Fstring_greaterp, 2, 2, 0, /*
Return t if first arg string is greater than second in lexicographic order.
If I18N2 support (but not Mule support) was compiled in, ordering is
determined by the locale. (Case is significant for the default C locale.)
In all other cases, comparison is simply done on a character-by-
character basis using the numeric value of a character. (Note that
this may not produce particularly meaningful results under Mule if
characters from different charsets are being compared.)

Symbols are also allowed; their print names are used instead.

The reason that the I18N2 locale-specific collation is not used under
Mule is that the locale model of internationalization does not handle
multiple charsets and thus has no hope of working properly under Mule.
What we really should do is create a collation table over all built-in
charsets.  This is extremely difficult to do from scratch, however.

Unicode is a good first step towards solving this problem.  In fact,
it is quite likely that a collation table exists (or will exist) for
Unicode.  When Unicode support is added to SXEmacs/Mule, this problem
may be solved.
*/
      (string1, string2))
{
	return Fstring_lessp(string2, string1);
}

DEFUN("string-modified-tick", Fstring_modified_tick, 1, 1, 0,	/*
Return STRING's tick counter, incremented for each change to the string.
Each string has a tick counter which is incremented each time the contents
of the string are changed (e.g. with `aset').  It wraps around occasionally.
*/
      (string))
{
	Lisp_String *s;

	CHECK_STRING(string);
	s = XSTRING(string);
	if (CONSP(s->plist) && INTP(XCAR(s->plist)))
		return XCAR(s->plist);
	else
		return Qzero;
}

void bump_string_modiff(Lisp_Object str)
{
	Lisp_String *s = XSTRING(str);
	Lisp_Object *ptr = &s->plist;

#ifdef I18N3
	/* #### remove the `string-translatable' property from the string,
	   if there is one. */
#endif
	/* skip over extent info if it's there */
	if (CONSP(*ptr) && EXTENT_INFOP(XCAR(*ptr)))
		ptr = &XCDR(*ptr);
	if (CONSP(*ptr) && INTP(XCAR(*ptr)))
		XSETINT(XCAR(*ptr), 1 + XINT(XCAR(*ptr)));
	else
		*ptr = Fcons(make_int(1), *ptr);
}

enum concat_target_type { c_cons, c_string, c_vector, c_bit_vector, c_dllist };
static Lisp_Object concat(int nargs, Lisp_Object * args,
			  enum concat_target_type target_type,
			  int last_special);

Lisp_Object concat2(Lisp_Object string1, Lisp_Object string2)
{
	Lisp_Object args[2];
	args[0] = string1;
	args[1] = string2;
	return concat(2, args, c_string, 0);
}

Lisp_Object
concat3(Lisp_Object string1, Lisp_Object string2, Lisp_Object string3)
{
	Lisp_Object args[3];
	args[0] = string1;
	args[1] = string2;
	args[2] = string3;
	return concat(3, args, c_string, 0);
}

Lisp_Object vconcat2(Lisp_Object vec1, Lisp_Object vec2)
{
	Lisp_Object args[2];
	args[0] = vec1;
	args[1] = vec2;
	return concat(2, args, c_vector, 0);
}

Lisp_Object vconcat3(Lisp_Object vec1, Lisp_Object vec2, Lisp_Object vec3)
{
	Lisp_Object args[3];
	args[0] = vec1;
	args[1] = vec2;
	args[2] = vec3;
	return concat(3, args, c_vector, 0);
}

DEFUN("append", Fappend, 0, MANY, 0,	/*
Concatenate all the arguments and make the result a list.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector, bit vector, or string.
The last argument is not copied, just used as the tail of the new list.
Also see: `nconc'.
*/
      (int nargs, Lisp_Object * args))
{
	return concat(nargs, args, c_cons, 1);
}

DEFUN("concat", Fconcat, 0, MANY, 0,	/*
Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument may be a string or a list or vector of characters.

As of XEmacs 21.0, this function does NOT accept individual integers
as arguments.  Old code that relies on, for example, (concat "foo" 50)
returning "foo50" will fail.  To fix such code, either apply
`int-to-string' to the integer argument, or use `format'.
*/
      (int nargs, Lisp_Object * args))
{
	return concat(nargs, args, c_string, 0);
}

DEFUN("vconcat", Fvconcat, 0, MANY, 0,	/*
Concatenate all the arguments and make the result a vector.
The result is a vector whose elements are the elements of all the arguments.
Each argument may be a list, vector, bit vector, or string.
*/
      (int nargs, Lisp_Object * args))
{
	return concat(nargs, args, c_vector, 0);
}

DEFUN("bvconcat", Fbvconcat, 0, MANY, 0,	/*
Concatenate all the arguments and make the result a bit vector.
The result is a bit vector whose elements are the elements of all the
arguments.  Each argument may be a list, vector, bit vector, or string.
*/
      (int nargs, Lisp_Object * args))
{
	return concat(nargs, args, c_bit_vector, 0);
}

/* Copy a (possibly dotted) list.  LIST must be a cons.
   Can't use concat (1, &alist, c_cons, 0) - doesn't handle dotted lists. */
static Lisp_Object copy_list(Lisp_Object list)
{
	Lisp_Object list_copy = Fcons(XCAR(list), XCDR(list));
	Lisp_Object last = list_copy;
	Lisp_Object hare, tortoise;
	size_t len;

	for (tortoise = hare = XCDR(list), len = 1;
	     CONSP(hare); hare = XCDR(hare), len++) {
		XCDR(last) = Fcons(XCAR(hare), XCDR(hare));
		last = XCDR(last);

		if (len < CIRCULAR_LIST_SUSPICION_LENGTH)
			continue;
		if (len & 1)
			tortoise = XCDR(tortoise);
		if (EQ(tortoise, hare))
			signal_circular_list_error(list);
	}

	return list_copy;
}

DEFUN("copy-list", Fcopy_list, 1, 1, 0,	/*
Return a copy of list LIST, which may be a dotted list.
The elements of LIST are not copied; they are shared
with the original.
*/
      (list))
{
      again:
	if (NILP(list))
		return list;
	if (CONSP(list))
		return copy_list(list);

	list = wrong_type_argument(Qlistp, list);
	goto again;
}

DEFUN("copy-sequence", Fcopy_sequence, 1, 1, 0,	/*
Return a copy of list, dllist, vector, bit vector or string SEQUENCE.
The elements of a list or vector are not copied; they are shared
with the original. SEQUENCE may be a dotted list.
*/
      (sequence))
{
      again:
	if (NILP(sequence))
		return sequence;
	if (CONSP(sequence))
		return copy_list(sequence);
	if (DLLISTP(sequence))
		return Fcopy_dllist(sequence);
	if (STRINGP(sequence))
		return concat(1, &sequence, c_string, 0);
	if (VECTORP(sequence))
		return concat(1, &sequence, c_vector, 0);
	if (BIT_VECTORP(sequence))
		return concat(1, &sequence, c_bit_vector, 0);

	check_losing_bytecode("copy-sequence", sequence);
	sequence = wrong_type_argument(Qsequencep, sequence);
	goto again;
}

struct merge_string_extents_struct {
	Lisp_Object string;
	Bytecount entry_offset;
	Bytecount entry_length;
};

static Lisp_Object
concat(int nargs, Lisp_Object * args,
       enum concat_target_type target_type, int last_special)
{
	Lisp_Object val;
	Lisp_Object tail = Qnil;
	int toindex;
	int argnum;
	Lisp_Object last_tail;
	Lisp_Object prev;
	struct merge_string_extents_struct *args_mse = 0;
	Bufbyte *string_result = NULL;
	Bufbyte *string_result_ptr = NULL;
	struct gcpro gcpro1;
	int speccount = specpdl_depth();
        Charcount total_length;
        

	/* The modus operandi in Emacs is "caller gc-protects args".
	   However, concat is called many times in Emacs on freshly
	   created stuff.  So we help those callers out by protecting
	   the args ourselves to save them a lot of temporary-variable
	   grief. */

	GCPROn(args, nargs);

#ifdef I18N3
	/* #### if the result is a string and any of the strings have a string
	   for the `string-translatable' property, then concat should also
	   concat the args but use the `string-translatable' strings, and store
	   the result in the returned string's `string-translatable' property. */
#endif
	if (target_type == c_string)
                XMALLOC_OR_ALLOCA(args_mse, nargs, struct merge_string_extents_struct);

	/* In append, the last arg isn't treated like the others */
	if (last_special && nargs > 0) {
		nargs--;
		last_tail = args[nargs];
	} else
		last_tail = Qnil;

	/* Check and coerce the arguments. */
	for (argnum = 0; argnum < nargs; argnum++) {
		Lisp_Object seq = args[argnum];
		if (LISTP(seq) || DLLISTP(seq)) ;
		else if (VECTORP(seq) || STRINGP(seq) || BIT_VECTORP(seq)) ;
#ifdef LOSING_BYTECODE
		else if (COMPILED_FUNCTIONP(seq))
			/* Urk!  We allow this, for "compatibility"... */
			;
#endif
#if 0				/* removed for XEmacs 21 */
		else if (INTP(seq))
			/* This is too revolting to think about but maintains
			   compatibility with FSF (and lots and lots of old code). */
			args[argnum] = Fnumber_to_string(seq);
#endif
		else {
			check_losing_bytecode("concat", seq);
			args[argnum] = wrong_type_argument(Qsequencep, seq);
		}

		if (args_mse) {
			if (STRINGP(seq))
				args_mse[argnum].string = seq;
			else
				args_mse[argnum].string = Qnil;
		}
	}

	{
		/* Charcount is a misnomer here as we might be dealing with the
		   length of a vector or list, but emphasizes that we're not dealing
		   with Bytecounts in strings */
                /* Charcount total_length; */

		for (argnum = 0, total_length = 0; argnum < nargs; argnum++) {
#ifdef LOSING_BYTECODE
			Charcount thislen =
			    length_with_bytecode_hack(args[argnum]);
#else
			Charcount thislen = XINT(Flength(args[argnum]));
#endif
			total_length += thislen;
		}

		switch (target_type) {
		case c_cons:
                        if (total_length == 0) {
				/* In append, if all but last arg are nil,
				   return last arg */
                                XMALLOC_UNBIND(args_mse, nargs, speccount);
				RETURN_UNGCPRO(last_tail);
                        }
			val = Fmake_list(make_int(total_length), Qnil);
			break;
		case c_dllist:
                        if (total_length == 0) {
				/* In append, if all but last arg are nil,
				   return last arg */
                                XMALLOC_UNBIND(args_mse, nargs, speccount);
				RETURN_UNGCPRO(last_tail);
                        }
			val = Fmake_list(make_int(total_length), Qnil);
			break;
		case c_vector:
			val = make_vector(total_length, Qnil);
			break;
		case c_bit_vector:
			val = make_bit_vector(total_length, Qzero);
			break;
		case c_string:
			/* We don't make the string yet because we don't know
			   the actual number of bytes.  This loop was formerly
			   written to call Fmake_string() here and then call
			   set_string_char() for each char.  This seems logical
			   enough but is waaaaaaaay slow -- set_string_char()
			   has to scan the whole string up to the place where
			   the substitution is called for in order to find the
			   place to change, and may have to do some realloc()ing
			   in order to make the char fit properly.  O(N^2)
			   yuckage. */
			val = Qnil;
                        XMALLOC_ATOMIC_OR_ALLOCA( string_result, 
						  total_length * MAX_EMCHAR_LEN,
						  Bufbyte );
			string_result_ptr = string_result;
			break;
		default:
			val = Qnil;
			abort();
		}
	}

	if (CONSP(val))
		tail = val, toindex = -1;	/* -1 in toindex is flag we are
						   making a list */
	else
		toindex = 0;

	prev = Qnil;

	for (argnum = 0; argnum < nargs; argnum++) {
		Charcount thisleni = 0;
		Charcount thisindex = 0;
		Lisp_Object seq = args[argnum];
		Bufbyte *string_source_ptr = 0;
		Bufbyte *string_prev_result_ptr = string_result_ptr;

		if (!CONSP(seq)) {
#ifdef LOSING_BYTECODE
			thisleni = length_with_bytecode_hack(seq);
#else
			thisleni = XINT(Flength(seq));
#endif
		}
		if (STRINGP(seq))
			string_source_ptr = XSTRING_DATA(seq);

		while (1) {
			Lisp_Object elt;

			/* We've come to the end of this arg, so exit. */
			if (NILP(seq))
				break;

			/* Fetch next element of `seq' arg into `elt' */
			if (CONSP(seq)) {
				elt = XCAR(seq);
				seq = XCDR(seq);
			} else {
				if (thisindex >= thisleni)
					break;

				if (STRINGP(seq)) {
					elt =
					    make_char(charptr_emchar
						      (string_source_ptr));
					INC_CHARPTR(string_source_ptr);
				} else if (VECTORP(seq))
					elt = XVECTOR_DATA(seq)[thisindex];
				else if (BIT_VECTORP(seq))
					elt =
					    make_int(bit_vector_bit
						     (XBIT_VECTOR(seq),
						      thisindex));
				else
					elt = Felt(seq, make_int(thisindex));
				thisindex++;
			}

			/* Store into result */
			if (toindex < 0) {
				/* toindex negative means we are making a list */
				XCAR(tail) = elt;
				prev = tail;
				tail = XCDR(tail);
			} else if (VECTORP(val))
				XVECTOR_DATA(val)[toindex++] = elt;
			else if (BIT_VECTORP(val)) {
				CHECK_BIT(elt);
				set_bit_vector_bit(XBIT_VECTOR(val), toindex++,
						   XINT(elt));
			} else {
				CHECK_CHAR_COERCE_INT(elt);
                                if(string_result_ptr != NULL) {
					string_result_ptr +=
						set_charptr_emchar(string_result_ptr,
								   XCHAR(elt));
				} else {
					abort();
				}
			}
		}
		if (args_mse) {
			args_mse[argnum].entry_offset =
			    string_prev_result_ptr - string_result;
			args_mse[argnum].entry_length =
			    string_result_ptr - string_prev_result_ptr;
		}
	}

	/* Now we finally make the string. */
	if (target_type == c_string) {
		val =
		    make_string(string_result,
				string_result_ptr - string_result);
		if (args_mse != NULL) {
			for (argnum = 0; argnum < nargs; argnum++) {
				if (STRINGP(args_mse[argnum].string))
					copy_string_extents(val,
							    args_mse[argnum].string,
							    args_mse[argnum].
							    entry_offset, 0,
							    args_mse[argnum].
							    entry_length);
			}
			XMALLOC_UNBIND(string_result,
				       total_length * MAX_EMCHAR_LEN, speccount);
			XMALLOC_UNBIND(args_mse, nargs, speccount);
		} else {
			abort();
		}
	}

	if (!NILP(prev))
		XCDR(prev) = last_tail;

	RETURN_UNGCPRO(val);
}

DEFUN("copy-alist", Fcopy_alist, 1, 1, 0,	/*
Return a copy of ALIST.
This is an alist which represents the same mapping from objects to objects,
but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however.
Elements of ALIST that are not conses are also shared.
*/
      (alist))
{
	Lisp_Object tail;

	if (NILP(alist))
		return alist;
	CHECK_CONS(alist);

	alist = concat(1, &alist, c_cons, 0);
	for (tail = alist; CONSP(tail); tail = XCDR(tail)) {
		Lisp_Object car = XCAR(tail);

		if (CONSP(car))
			XCAR(tail) = Fcons(XCAR(car), XCDR(car));
	}
	return alist;
}

DEFUN("copy-tree", Fcopy_tree, 1, 2, 0,	/*
Return a copy of a list and substructures.
The argument is copied, and any lists contained within it are copied
recursively.  Circularities and shared substructures are not preserved.
Second arg VECP causes vectors to be copied, too.  Strings and bit vectors
are not copied.
*/
      (arg, vecp))
{
	return safe_copy_tree(arg, vecp, 0);
}

Lisp_Object safe_copy_tree(Lisp_Object arg, Lisp_Object vecp, int depth)
{
	if (depth > 200)
		signal_simple_error("Stack overflow in copy-tree", arg);

	if (CONSP(arg)) {
		Lisp_Object rest;
		rest = arg = Fcopy_sequence(arg);
		while (CONSP(rest)) {
			Lisp_Object elt = XCAR(rest);
			QUIT;
			if (CONSP(elt) || VECTORP(elt))
				XCAR(rest) =
				    safe_copy_tree(elt, vecp, depth + 1);
			if (VECTORP(XCDR(rest)))	/* hack for (a b . [c d]) */
				XCDR(rest) =
				    safe_copy_tree(XCDR(rest), vecp, depth + 1);
			rest = XCDR(rest);
		}
	} else if (VECTORP(arg) && !NILP(vecp)) {
		int i = XVECTOR_LENGTH(arg);
		int j;
		arg = Fcopy_sequence(arg);
		for (j = 0; j < i; j++) {
			Lisp_Object elt = XVECTOR_DATA(arg)[j];
			QUIT;
			if (CONSP(elt) || VECTORP(elt))
				XVECTOR_DATA(arg)[j] =
				    safe_copy_tree(elt, vecp, depth + 1);
		}
	}
	return arg;
}

DEFUN("substring", Fsubstring, 2, 3, 0,	/*
Return the substring of STRING starting at START and ending before END.
END may be nil or omitted; then the substring runs to the end of STRING.
If START or END is negative, it counts from the end.
Relevant parts of the string-extent-data are copied to the new string.
*/
      (string, start, end)) 
{
	Charcount ccstart, ccend;
	Bytecount bstart, blen;
	Lisp_Object val;

	CHECK_STRING(string);
	CHECK_INT(start);
	get_string_range_char(string, start, end, &ccstart, &ccend,
			      GB_HISTORICAL_STRING_BEHAVIOR);
	bstart = charcount_to_bytecount(XSTRING_DATA(string), ccstart);
	blen =
	    charcount_to_bytecount(XSTRING_DATA(string) + bstart,
				   ccend - ccstart);
	val = make_string(XSTRING_DATA(string) + bstart, blen);
	/* Copy any applicable extent information into the new string. */
	copy_string_extents(val, string, 0, bstart, blen);
	return val;
}

DEFUN("subseq", Fsubseq, 2, 3, 0,	/*
Return the subsequence of SEQUENCE starting at START and ending before END.
END may be omitted; then the subsequence runs to the end of SEQUENCE.
If START or END is negative, it counts from the end.
The returned subsequence is always of the same type as SEQUENCE.
If SEQUENCE is a string, relevant parts of the string-extent-data
are copied to the new string.
*/
      (sequence, start, end))
{
	EMACS_INT len, s, e;

	if (STRINGP(sequence))
		return Fsubstring(sequence, start, end);

	len = XINT(Flength(sequence));

	CHECK_INT(start);
	s = XINT(start);
	if (s < 0)
		s = len + s;

	if (NILP(end))
		e = len;
	else {
		CHECK_INT(end);
		e = XINT(end);
		if (e < 0)
			e = len + e;
	}

	if (!(0 <= s && s <= e && e <= len))
		args_out_of_range_3(sequence, make_int(s), make_int(e));

	if (VECTORP(sequence)) {
		Lisp_Object result = make_vector(e - s, Qnil);
		EMACS_INT i;
		Lisp_Object *in_elts = XVECTOR_DATA(sequence);
		Lisp_Object *out_elts = XVECTOR_DATA(result);

		for (i = s; i < e; i++)
			out_elts[i - s] = in_elts[i];
		return result;
	} else if (LISTP(sequence)) {
		Lisp_Object result = Qnil;
		EMACS_INT i;

		sequence = Fnthcdr(make_int(s), sequence);

		for (i = s; i < e; i++) {
			result = Fcons(Fcar(sequence), result);
			sequence = Fcdr(sequence);
		}

		return Fnreverse(result);
	} else if (BIT_VECTORP(sequence)) {
		Lisp_Object result = make_bit_vector(e - s, Qzero);
		EMACS_INT i;

		for (i = s; i < e; i++)
			set_bit_vector_bit(XBIT_VECTOR(result), i - s,
					   bit_vector_bit(XBIT_VECTOR(sequence),
							  i));
		return result;
	} else {
		abort();	/* unreachable, since Flength (sequence) did not get
				   an error */
		return Qnil;
	}
}

DEFUN("nthcdr", Fnthcdr, 2, 2, 0,	/*
Take cdr N times on LIST, and return the result.
*/
      (n, list))
{
	REGISTER size_t i;
	REGISTER Lisp_Object tail = list;
	CHECK_NATNUM(n);
	for (i = XINT(n); i; i--) {
		if (CONSP(tail))
			tail = XCDR(tail);
		else if (NILP(tail))
			return Qnil;
		else {
			tail = wrong_type_argument(Qlistp, tail);
			i++;
		}
	}
	return tail;
}

DEFUN("nth", Fnth, 2, 2, 0,	/*
Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned.
*/
      (n, list))
{
	return Fcar(Fnthcdr(n, list));
}

DEFUN("elt", Felt, 2, 2, 0,	/*
Return element of SEQUENCE at index N.
*/
      (sequence, n))
{
retry:
	if (!(INTP(n) || CHARP(n))) {
		n = wrong_type_argument(Qinteger_or_char_p, n);
		goto retry;
	}

	if (LISTP(sequence)) {
		Lisp_Object tem = Fnthcdr(n, sequence);
		/* #### Utterly, completely, fucking disgusting.
		 * #### The whole point of "elt" is that it operates on
		 * #### sequences, and does error- (bounds-) checking.
		 */
		if (CONSP(tem))
			return XCAR(tem);
		else
#if 1
			/* This is The Way It Has Always Been. */
			return Qnil;
#else
			/* This is The Way Mly and Cltl2 say It Should Be. */
			args_out_of_range(sequence, n);
#endif
	} else if (DLLISTP(sequence)) {
		dllist_item_t elm = NULL;
		int rev = 0;
		REGISTER size_t i;
		EMACS_INT rn = ent_int(n);

		if (rn < 0) {
			args_out_of_range(sequence, n);
			return Qnil;
		}

		if (rn * 2 < (EMACS_INT)XDLLIST_SIZE(sequence)) {
			/* start at the front */
			elm = XDLLIST_FIRST(sequence);
			i = rn;
		} else {
			/* start at the end */
			elm = XDLLIST_LAST(sequence);
			rev = 1;
			i = XDLLIST_SIZE(sequence) - rn - 1;
		}

		for (; i > 0 && elm != NULL; i--)
			if (rev == 0)
				elm = elm->next;
			else
				elm = elm->prev;

		if (elm)
			return (Lisp_Object)elm->item;
		else
			return Qnil;

	} else if (STRINGP(sequence) ||
		   VECTORP(sequence) || BIT_VECTORP(sequence))
		return Faref(sequence, n);
#ifdef LOSING_BYTECODE
	else if (COMPILED_FUNCTIONP(sequence)) {
		EMACS_INT idx = ent_int(n);
		if (idx < 0) {
		      lose:
			args_out_of_range(sequence, n);
		}
		/* Utter perversity */
		{
			Lisp_Compiled_Function *f =
			    XCOMPILED_FUNCTION(sequence);
			switch (idx) {
			case COMPILED_ARGLIST:
				return compiled_function_arglist(f);
			case COMPILED_INSTRUCTIONS:
				return compiled_function_instructions(f);
			case COMPILED_CONSTANTS:
				return compiled_function_constants(f);
			case COMPILED_STACK_DEPTH:
				return compiled_function_stack_depth(f);
			case COMPILED_DOC_STRING:
				return compiled_function_documentation(f);
			case COMPILED_DOMAIN:
				return compiled_function_domain(f);
			case COMPILED_INTERACTIVE:
				if (f->flags.interactivep)
					return compiled_function_interactive(f);
				/* if we return nil, can't tell interactive with no args
				   from noninteractive. */
				goto lose;
			default:
				goto lose;
			}
		}
	}
#endif				/* LOSING_BYTECODE */
	else {
		check_losing_bytecode("elt", sequence);
		sequence = wrong_type_argument(Qsequencep, sequence);
		goto retry;
	}
}

DEFUN("last", Flast, 1, 2, 0,	/*
Return the tail of list LIST, of length N (default 1).
LIST may be a dotted list, but not a circular list.
Optional argument N must be a non-negative integer.
If N is zero, then the atom that terminates the list is returned.
If N is greater than the length of LIST, then LIST itself is returned.
*/
      (list, n))
{
	EMACS_INT int_n, count;
	Lisp_Object retval, tortoise, hare;

	if (DLLISTP(list))
		return Fdllist_rac(list);

	CHECK_LIST(list);

	if (NILP(n))
		int_n = 1;
	else {
		CHECK_NATNUM(n);
		int_n = XINT(n);
	}

	for (retval = tortoise = hare = list, count = 0;
	     CONSP(hare);
	     hare = XCDR(hare),
	     (int_n-- <= 0 ? ((void)(retval = XCDR(retval))) : (void)0),
	     count++) {
		if (count < CIRCULAR_LIST_SUSPICION_LENGTH)
			continue;

		if (count & 1)
			tortoise = XCDR(tortoise);
		if (EQ(hare, tortoise))
			signal_circular_list_error(list);
	}

	return retval;
}

DEFUN("nbutlast", Fnbutlast, 1, 2, 0,	/*
Modify LIST to remove the last N (default 1) elements.
If LIST has N or fewer elements, nil is returned and LIST is unmodified.
*/
      (list, n))
{
	EMACS_INT int_n;

	CHECK_LIST(list);

	if (NILP(n))
		int_n = 1;
	else {
		CHECK_NATNUM(n);
		int_n = XINT(n);
	}

	{
		Lisp_Object last_cons = list;

		EXTERNAL_LIST_LOOP_1(list) {
			if (int_n-- < 0)
				last_cons = XCDR(last_cons);
		}

		if (int_n >= 0)
			return Qnil;

		XCDR(last_cons) = Qnil;
		return list;
	}
}

DEFUN("butlast", Fbutlast, 1, 2, 0,	/*
Return a copy of LIST with the last N (default 1) elements removed.
If LIST has N or fewer elements, nil is returned.
*/
      (list, n))
{
	EMACS_INT int_n;

	CHECK_LIST(list);

	if (NILP(n))
		int_n = 1;
	else {
		CHECK_NATNUM(n);
		int_n = XINT(n);
	}

	{
		Lisp_Object retval = Qnil;
		Lisp_Object tail = list;

		EXTERNAL_LIST_LOOP_1(list) {
			if (--int_n < 0) {
				retval = Fcons(XCAR(tail), retval);
				tail = XCDR(tail);
			}
		}

		return Fnreverse(retval);
	}
}

DEFUN("member", Fmember, 2, 2, 0,	/*
Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_3(list_elt, list, tail) {
		if (internal_equal(elt, list_elt, 0))
			return tail;
	}
	return Qnil;
}

DEFUN("old-member", Fold_member, 2, 2, 0,	/*
Return non-nil if ELT is an element of LIST.  Comparison done with `old-equal'.
The value is actually the tail of LIST whose car is ELT.
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_3(list_elt, list, tail) {
		if (internal_old_equal(elt, list_elt, 0))
			return tail;
	}
	return Qnil;
}

DEFUN("memq", Fmemq, 2, 2, 0,	/*
Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_3(list_elt, list, tail) {
		if (EQ_WITH_EBOLA_NOTICE(elt, list_elt))
			return tail;
	}
	return Qnil;
}

DEFUN("old-memq", Fold_memq, 2, 2, 0,	/*
Return non-nil if ELT is an element of LIST.  Comparison done with `old-eq'.
The value is actually the tail of LIST whose car is ELT.
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_3(list_elt, list, tail) {
		if (HACKEQ_UNSAFE(elt, list_elt))
			return tail;
	}
	return Qnil;
}

Lisp_Object memq_no_quit(Lisp_Object elt, Lisp_Object list)
{
	LIST_LOOP_3(list_elt, list, tail) {
		if (EQ_WITH_EBOLA_NOTICE(elt, list_elt))
			return tail;
	}
	return Qnil;
}

DEFUN("assoc", Fassoc, 2, 2, 0,	/*
Return non-nil if KEY is `equal' to the car of an element of ALIST.
The value is actually the element of ALIST whose car equals KEY.
*/
      (key, alist))
{
	/* This function can GC. */
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (internal_equal(key, elt_car, 0))
			return elt;
	}
	return Qnil;
}

DEFUN("old-assoc", Fold_assoc, 2, 2, 0,	/*
Return non-nil if KEY is `old-equal' to the car of an element of ALIST.
The value is actually the element of ALIST whose car equals KEY.
*/
      (key, alist))
{
	/* This function can GC. */
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (internal_old_equal(key, elt_car, 0))
			return elt;
	}
	return Qnil;
}

Lisp_Object assoc_no_quit(Lisp_Object key, Lisp_Object alist)
{
	int speccount = specpdl_depth();
	specbind(Qinhibit_quit, Qt);
	return unbind_to(speccount, Fassoc(key, alist));
}

DEFUN("assq", Fassq, 2, 2, 0,	/*
Return non-nil if KEY is `eq' to the car of an element of ALIST.
The value is actually the element of ALIST whose car is KEY.
Elements of ALIST that are not conses are ignored.
*/
      (key, alist))
{
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (EQ_WITH_EBOLA_NOTICE(key, elt_car))
			return elt;
	}
	return Qnil;
}

DEFUN("old-assq", Fold_assq, 2, 2, 0,	/*
Return non-nil if KEY is `old-eq' to the car of an element of ALIST.
The value is actually the element of ALIST whose car is KEY.
Elements of ALIST that are not conses are ignored.
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
      (key, alist))
{
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (HACKEQ_UNSAFE(key, elt_car))
			return elt;
	}
	return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object assq_no_quit(Lisp_Object key, Lisp_Object alist)
{
	/* This cannot GC. */
	LIST_LOOP_2(elt, alist) {
		Lisp_Object elt_car = XCAR(elt);
		if (EQ_WITH_EBOLA_NOTICE(key, elt_car))
			return elt;
	}
	return Qnil;
}

DEFUN("rassoc", Frassoc, 2, 2, 0,	/*
Return non-nil if VALUE is `equal' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr equals VALUE.
*/
      (value, alist))
{
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (internal_equal(value, elt_cdr, 0))
			return elt;
	}
	return Qnil;
}

DEFUN("old-rassoc", Fold_rassoc, 2, 2, 0,	/*
Return non-nil if VALUE is `old-equal' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr equals VALUE.
*/
      (value, alist))
{
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (internal_old_equal(value, elt_cdr, 0))
			return elt;
	}
	return Qnil;
}

DEFUN("rassq", Frassq, 2, 2, 0,	/*
Return non-nil if VALUE is `eq' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr is VALUE.
*/
      (value, alist))
{
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (EQ_WITH_EBOLA_NOTICE(value, elt_cdr))
			return elt;
	}
	return Qnil;
}

DEFUN("old-rassq", Fold_rassq, 2, 2, 0,	/*
Return non-nil if VALUE is `old-eq' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr is VALUE.
*/
      (value, alist))
{
	EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, alist) {
		if (HACKEQ_UNSAFE(value, elt_cdr))
			return elt;
	}
	return Qnil;
}

/* Like Frassq, but caller must ensure that ALIST is properly
   nil-terminated and ebola-free. */
Lisp_Object rassq_no_quit(Lisp_Object value, Lisp_Object alist)
{
	LIST_LOOP_2(elt, alist) {
		Lisp_Object elt_cdr = XCDR(elt);
		if (EQ_WITH_EBOLA_NOTICE(value, elt_cdr))
			return elt;
	}
	return Qnil;
}

DEFUN("delete", Fdelete, 2, 2, 0,	/*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (delete element foo))' to be sure
of changing the value of `foo'.
Also see: `remove'.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(list_elt, list,
				     (internal_equal(elt, list_elt, 0)));
	return list;
}

DEFUN("old-delete", Fold_delete, 2, 2, 0,	/*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `old-equal'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (old-delete element foo))' to be sure
of changing the value of `foo'.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(list_elt, list,
				     (internal_old_equal(elt, list_elt, 0)));
	return list;
}

DEFUN("delq", Fdelq, 2, 2, 0,	/*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `eq'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (delq element foo))' to be sure of
changing the value of `foo'.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(list_elt, list,
				     (EQ_WITH_EBOLA_NOTICE(elt, list_elt)));
	return list;
}

DEFUN("old-delq", Fold_delq, 2, 2, 0,	/*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `old-eq'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (old-delq element foo))' to be sure of
changing the value of `foo'.
*/
      (elt, list))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(list_elt, list,
				     (HACKEQ_UNSAFE(elt, list_elt)));
	return list;
}

/* Like Fdelq, but caller must ensure that LIST is properly
   nil-terminated and ebola-free. */

Lisp_Object delq_no_quit(Lisp_Object elt, Lisp_Object list)
{
	LIST_LOOP_DELETE_IF(list_elt, list,
			    (EQ_WITH_EBOLA_NOTICE(elt, list_elt)));
	return list;
}

/* Be VERY careful with this.  This is like delq_no_quit() but
   also calls free_cons() on the removed conses.  You must be SURE
   that no pointers to the freed conses remain around (e.g.
   someone else is pointing to part of the list).  This function
   is useful on internal lists that are used frequently and where
   the actual list doesn't escape beyond known code bounds. */

Lisp_Object delq_no_quit_and_free_cons(Lisp_Object elt, Lisp_Object list)
{
	REGISTER Lisp_Object tail = list;
	REGISTER Lisp_Object prev = Qnil;

	while (!NILP(tail)) {
		REGISTER Lisp_Object tem = XCAR(tail);
		if (EQ(elt, tem)) {
			Lisp_Object cons_to_free = tail;
			if (NILP(prev))
				list = XCDR(tail);
			else
				XCDR(prev) = XCDR(tail);
			tail = XCDR(tail);
			free_cons(XCONS(cons_to_free));
		} else {
			prev = tail;
			tail = XCDR(tail);
		}
	}
	return list;
}

DEFUN("remassoc", Fremassoc, 2, 2, 0,	/*
Delete by side effect any elements of ALIST whose car is `equal' to KEY.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassoc key foo))' to be sure of changing
the value of `foo'.
*/
      (key, alist))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(elt, alist,
				     (CONSP(elt) &&
				      internal_equal(key, XCAR(elt), 0)));
	return alist;
}

Lisp_Object remassoc_no_quit(Lisp_Object key, Lisp_Object alist)
{
	int speccount = specpdl_depth();
	specbind(Qinhibit_quit, Qt);
	return unbind_to(speccount, Fremassoc(key, alist));
}

DEFUN("remassq", Fremassq, 2, 2, 0,	/*
Delete by side effect any elements of ALIST whose car is `eq' to KEY.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `eq' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassq key foo))' to be sure of changing
the value of `foo'.
*/
      (key, alist))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(elt, alist,
				     (CONSP(elt) &&
				      EQ_WITH_EBOLA_NOTICE(key, XCAR(elt))));
	return alist;
}

/* no quit, no errors; be careful */

Lisp_Object remassq_no_quit(Lisp_Object key, Lisp_Object alist)
{
	LIST_LOOP_DELETE_IF(elt, alist,
			    (CONSP(elt) &&
			     EQ_WITH_EBOLA_NOTICE(key, XCAR(elt))));
	return alist;
}

DEFUN("remrassoc", Fremrassoc, 2, 2, 0,	/*
Delete by side effect any elements of ALIST whose cdr is `equal' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassoc value foo))' to be sure of changing
the value of `foo'.
*/
      (value, alist))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(elt, alist,
				     (CONSP(elt) &&
				      internal_equal(value, XCDR(elt), 0)));
	return alist;
}

DEFUN("remrassq", Fremrassq, 2, 2, 0,	/*
Delete by side effect any elements of ALIST whose cdr is `eq' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `eq' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassq value foo))' to be sure of changing
the value of `foo'.
*/
      (value, alist))
{
	EXTERNAL_LIST_LOOP_DELETE_IF(elt, alist,
				     (CONSP(elt) &&
				      EQ_WITH_EBOLA_NOTICE(value, XCDR(elt))));
	return alist;
}

/* Like Fremrassq, fast and unsafe; be careful */
Lisp_Object remrassq_no_quit(Lisp_Object value, Lisp_Object alist)
{
	LIST_LOOP_DELETE_IF(elt, alist,
			    (CONSP(elt) &&
			     EQ_WITH_EBOLA_NOTICE(value, XCDR(elt))));
	return alist;
}

DEFUN("nreverse", Fnreverse, 1, 1, 0,	/*
Reverse LIST by destructively modifying cdr pointers.
Return the beginning of the reversed list.
Also see: `reverse'.
*/
      (list))
{
	struct gcpro gcpro1, gcpro2;
	REGISTER Lisp_Object prev = Qnil;
	REGISTER Lisp_Object tail = list;

	/* We gcpro our args; see `nconc' */
	GCPRO2(prev, tail);
	while (!NILP(tail)) {
		REGISTER Lisp_Object next;
		CONCHECK_CONS(tail);
		next = XCDR(tail);
		XCDR(tail) = prev;
		prev = tail;
		tail = next;
	}
	UNGCPRO;
	return prev;
}

DEFUN("reverse", Freverse, 1, 1, 0,	/*
Reverse LIST, copying.  Return the beginning of the reversed list.
See also the function `nreverse', which is used more often.
*/
      (list))
{
	Lisp_Object reversed_list = Qnil;
	EXTERNAL_LIST_LOOP_2(elt, list) {
		reversed_list = Fcons(elt, reversed_list);
	}
	return reversed_list;
}

static Lisp_Object list_merge(Lisp_Object org_l1, Lisp_Object org_l2,
			      Lisp_Object lisp_arg,
			      int (*pred_fn) (Lisp_Object, Lisp_Object,
					      Lisp_Object lisp_arg));

Lisp_Object
list_sort(Lisp_Object list,
	  Lisp_Object lisp_arg,
	  int (*pred_fn) (Lisp_Object, Lisp_Object, Lisp_Object lisp_arg))
{
	struct gcpro gcpro1, gcpro2, gcpro3;
	Lisp_Object back, tem;
	Lisp_Object front = list;
	Lisp_Object len = Flength(list);

	if (XINT(len) < 2)
		return list;

	len = make_int(XINT(len) / 2 - 1);
	tem = Fnthcdr(len, list);
	back = Fcdr(tem);
	Fsetcdr(tem, Qnil);

	GCPRO3(front, back, lisp_arg);
	front = list_sort(front, lisp_arg, pred_fn);
	back = list_sort(back, lisp_arg, pred_fn);
	UNGCPRO;
	return list_merge(front, back, lisp_arg, pred_fn);
}

static int
merge_pred_function(Lisp_Object obj1, Lisp_Object obj2, Lisp_Object pred)
{
	Lisp_Object tmp;

	/* prevents the GC from happening in call2 */
	int speccount = specpdl_depth();
/* Emacs' GC doesn't actually relocate pointers, so this probably
   isn't strictly necessary */
	record_unwind_protect(restore_gc_inhibit,
			      make_int(gc_currently_forbidden));
	gc_currently_forbidden = 1;
	tmp = call2(pred, obj1, obj2);
	unbind_to(speccount, Qnil);

	if (NILP(tmp))
		return -1;
	else
		return 1;
}

DEFUN("sort", Fsort, 2, 2, 0,	/*
Sort LIST, stably, comparing elements using PREDICATE.
Returns the sorted list.  LIST is modified by side effects.
PREDICATE is called with two elements of LIST, and should return T
if the first element is "less" than the second.
*/
      (list, predicate))
{
	return list_sort(list, predicate, merge_pred_function);
}

Lisp_Object merge(Lisp_Object org_l1, Lisp_Object org_l2, Lisp_Object pred)
{
	return list_merge(org_l1, org_l2, pred, merge_pred_function);
}

static Lisp_Object
list_merge(Lisp_Object org_l1, Lisp_Object org_l2,
	   Lisp_Object lisp_arg,
	   int (*pred_fn) (Lisp_Object, Lisp_Object, Lisp_Object lisp_arg))
{
	Lisp_Object value;
	Lisp_Object tail;
	Lisp_Object tem;
	Lisp_Object l1, l2;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	l1 = org_l1;
	l2 = org_l2;
	tail = Qnil;
	value = Qnil;

	/* It is sufficient to protect org_l1 and org_l2.
	   When l1 and l2 are updated, we copy the new values
	   back into the org_ vars.  */

	GCPRO4(org_l1, org_l2, lisp_arg, value);

	while (1) {
		if (NILP(l1)) {
			UNGCPRO;
			if (NILP(tail))
				return l2;
			Fsetcdr(tail, l2);
			return value;
		}
		if (NILP(l2)) {
			UNGCPRO;
			if (NILP(tail))
				return l1;
			Fsetcdr(tail, l1);
			return value;
		}

		if (((*pred_fn) (Fcar(l2), Fcar(l1), lisp_arg)) < 0) {
			tem = l1;
			l1 = Fcdr(l1);
			org_l1 = l1;
		} else {
			tem = l2;
			l2 = Fcdr(l2);
			org_l2 = l2;
		}
		if (NILP(tail))
			value = tem;
		else
			Fsetcdr(tail, tem);
		tail = tem;
	}
}

/************************************************************************/
/*	  	        property-list functions				*/
/************************************************************************/

/* For properties of text, we need to do order-insensitive comparison of
   plists.  That is, we need to compare two plists such that they are the
   same if they have the same set of keys, and equivalent values.
   So (a 1 b 2) would be equal to (b 2 a 1).

   NIL_MEANS_NOT_PRESENT is as in `plists-eq' etc.
   LAXP means use `equal' for comparisons.
 */
int
plists_differ(Lisp_Object a, Lisp_Object b, int nil_means_not_present,
	      int laxp, int depth)
{
	int eqp = (depth == -1);	/* -1 as depth means use eq, not equal. */
	int la, lb, m, i, fill;
	Lisp_Object *keys, *vals;
	char *flags;
	Lisp_Object rest;
	int speccount = specpdl_depth();

	if (NILP(a) && NILP(b))
		return 0;

	Fcheck_valid_plist(a);
	Fcheck_valid_plist(b);

	la = XINT(Flength(a));
	lb = XINT(Flength(b));
	m = (la > lb ? la : lb);
	fill = 0;
	XMALLOC_OR_ALLOCA(keys, m, Lisp_Object);
	XMALLOC_OR_ALLOCA(vals, m, Lisp_Object);
	XMALLOC_ATOMIC_OR_ALLOCA(flags, m, char);

	/* First extract the pairs from A. */
	for (rest = a; !NILP(rest); rest = XCDR(XCDR(rest))) {
		Lisp_Object k = XCAR(rest);
		Lisp_Object v = XCAR(XCDR(rest));
		/* Maybe be Ebolified. */
		if (nil_means_not_present && NILP(v))
			continue;
		keys[fill] = k;
		vals[fill] = v;
		flags[fill] = 0;
		fill++;
	}
	/* Now iterate over B, and stop if we find something that's not in A,
	   or that doesn't match.  As we match, mark them. */
	for (rest = b; !NILP(rest); rest = XCDR(XCDR(rest))) {
		Lisp_Object k = XCAR(rest);
		Lisp_Object v = XCAR(XCDR(rest));
		/* Maybe be Ebolified. */
		if (nil_means_not_present && NILP(v))
			continue;
		for (i = 0; i < fill; i++) {
			if (!laxp ? EQ(k, keys[i]) :
			    internal_equal(k, keys[i], depth)) {
				if (eqp
				    /* We narrowly escaped being Ebolified
				       here. */
				    ? !EQ_WITH_EBOLA_NOTICE(v, vals[i])
				    : !internal_equal(v, vals[i], depth))
					/* a property in B has a different value
					   than in A */
					goto MISMATCH;
				flags[i] = 1;
				break;
			}
		}
		if (i == fill)
			/* there are some properties in B that are not in A */
			goto MISMATCH;
	}
	/* Now check to see that all the properties in A were also in B */
	for (i = 0; i < fill; i++)
		if (flags[i] == 0)
			goto MISMATCH;

	XMALLOC_UNBIND(flags, m, speccount);
	XMALLOC_UNBIND(vals, m, speccount);
	XMALLOC_UNBIND(keys, m, speccount);
	/* Ok. */
	return 0;

MISMATCH:
	XMALLOC_UNBIND(flags, m, speccount);
	XMALLOC_UNBIND(vals, m, speccount);
	XMALLOC_UNBIND(keys, m, speccount);
	return 1;
}

DEFUN("plists-eq", Fplists_eq, 2, 3, 0,	/*
Return non-nil if property lists A and B are `eq'.
A property list is an alternating list of keywords and values.
This function does order-insensitive comparisons of the property lists:
For example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
Comparison between values is done using `eq'.  See also `plists-equal'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
a nil value is ignored.  This feature is a virus that has infected
old Lisp implementations, but should not be used except for backward
compatibility.
*/
      (a, b, nil_means_not_present))
{
	return (plists_differ(a, b, !NILP(nil_means_not_present), 0, -1)
		? Qnil : Qt);
}

DEFUN("plists-equal", Fplists_equal, 2, 3, 0,	/*
Return non-nil if property lists A and B are `equal'.
A property list is an alternating list of keywords and values.  This
function does order-insensitive comparisons of the property lists: For
example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
Comparison between values is done using `equal'.  See also `plists-eq'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
a nil value is ignored.  This feature is a virus that has infected
old Lisp implementations, but should not be used except for backward
compatibility.
*/
      (a, b, nil_means_not_present))
{
	return (plists_differ(a, b, !NILP(nil_means_not_present), 0, 1)
		? Qnil : Qt);
}

DEFUN("lax-plists-eq", Flax_plists_eq, 2, 3, 0,	/*
Return non-nil if lax property lists A and B are `eq'.
A property list is an alternating list of keywords and values.
This function does order-insensitive comparisons of the property lists:
For example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
Comparison between values is done using `eq'.  See also `plists-equal'.
A lax property list is like a regular one except that comparisons between
keywords is done using `equal' instead of `eq'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
a nil value is ignored.  This feature is a virus that has infected
old Lisp implementations, but should not be used except for backward
compatibility.
*/
      (a, b, nil_means_not_present))
{
	return (plists_differ(a, b, !NILP(nil_means_not_present), 1, -1)
		? Qnil : Qt);
}

DEFUN("lax-plists-equal", Flax_plists_equal, 2, 3, 0,	/*
Return non-nil if lax property lists A and B are `equal'.
A property list is an alternating list of keywords and values.  This
function does order-insensitive comparisons of the property lists: For
example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
Comparison between values is done using `equal'.  See also `plists-eq'.
A lax property list is like a regular one except that comparisons between
keywords is done using `equal' instead of `eq'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
a nil value is ignored.  This feature is a virus that has infected
old Lisp implementations, but should not be used except for backward
compatibility.
*/
      (a, b, nil_means_not_present))
{
	return (plists_differ(a, b, !NILP(nil_means_not_present), 1, 1)
		? Qnil : Qt);
}

/* Return the value associated with key PROPERTY in property list PLIST.
   Return nil if key not found.  This function is used for internal
   property lists that cannot be directly manipulated by the user.
   */

Lisp_Object internal_plist_get(Lisp_Object plist, Lisp_Object property)
{
	Lisp_Object tail;

	for (tail = plist; !NILP(tail); tail = XCDR(XCDR(tail))) {
		if (EQ(XCAR(tail), property))
			return XCAR(XCDR(tail));
	}

	return Qunbound;
}

/* Set PLIST's value for PROPERTY to VALUE.  Analogous to
   internal_plist_get(). */

void
internal_plist_put(Lisp_Object * plist, Lisp_Object property, Lisp_Object value)
{
	Lisp_Object tail;

	for (tail = *plist; !NILP(tail); tail = XCDR(XCDR(tail))) {
		if (EQ(XCAR(tail), property)) {
			XCAR(XCDR(tail)) = value;
			return;
		}
	}

	*plist = Fcons(property, Fcons(value, *plist));
}

int internal_remprop(Lisp_Object * plist, Lisp_Object property)
{
	Lisp_Object tail, prev;

	for (tail = *plist, prev = Qnil; !NILP(tail); tail = XCDR(XCDR(tail))) {
		if (EQ(XCAR(tail), property)) {
			if (NILP(prev))
				*plist = XCDR(XCDR(tail));
			else
				XCDR(XCDR(prev)) = XCDR(XCDR(tail));
			return 1;
		} else
			prev = tail;
	}

	return 0;
}

/* Called on a malformed property list.  BADPLACE should be some
   place where truncating will form a good list -- i.e. we shouldn't
   result in a list with an odd length. */

static Lisp_Object
bad_bad_bunny(Lisp_Object * plist, Lisp_Object * badplace, Error_behavior errb)
{
	if (ERRB_EQ(errb, ERROR_ME))
		return Fsignal(Qmalformed_property_list,
			       list2(*plist, *badplace));
	else {
		if (ERRB_EQ(errb, ERROR_ME_WARN)) {
			warn_when_safe_lispobj
			    (Qlist, Qwarning,
			     list2(build_string
				   ("Malformed property list -- list has been truncated"),
				   *plist));
			*badplace = Qnil;
		}
		return Qunbound;
	}
}

/* Called on a circular property list.  BADPLACE should be some place
   where truncating will result in an even-length list, as above.
   If doesn't particularly matter where we truncate -- anywhere we
   truncate along the entire list will break the circularity, because
   it will create a terminus and the list currently doesn't have one.
*/

static Lisp_Object
bad_bad_turtle(Lisp_Object * plist, Lisp_Object * badplace, Error_behavior errb)
{
	if (ERRB_EQ(errb, ERROR_ME))
		return Fsignal(Qcircular_property_list, list1(*plist));
	else {
		if (ERRB_EQ(errb, ERROR_ME_WARN)) {
			warn_when_safe_lispobj
			    (Qlist, Qwarning,
			     list2(build_string
				   ("Circular property list -- list has been truncated"),
				   *plist));
			*badplace = Qnil;
		}
		return Qunbound;
	}
}

/* Advance the tortoise pointer by two (one iteration of a property-list
   loop) and the hare pointer by four and verify that no malformations
   or circularities exist.  If so, return zero and store a value into
   RETVAL that should be returned by the calling function.  Otherwise,
   return 1.  See external_plist_get().
 */

static int
advance_plist_pointers(Lisp_Object * plist,
		       Lisp_Object ** tortoise, Lisp_Object ** hare,
		       Error_behavior errb, Lisp_Object * retval)
{
	int i;
	Lisp_Object *tortsave = *tortoise;

	/* Note that our "fixing" may be more brutal than necessary,
	   but it's the user's own problem, not ours, if they went in and
	   manually fucked up a plist. */

	for (i = 0; i < 2; i++) {
		/* This is a standard iteration of a defensive-loop-checking
		   loop.  We just do it twice because we want to advance past
		   both the property and its value.

		   If the pointer indirection is confusing you, remember that
		   one level of indirection on the hare and tortoise pointers
		   is only due to pass-by-reference for this function.  The other
		   level is so that the plist can be fixed in place. */

		/* When we reach the end of a well-formed plist, **HARE is
		   nil.  In that case, we don't do anything at all except
		   advance TORTOISE by one.  Otherwise, we advance HARE
		   by two (making sure it's OK to do so), then advance
		   TORTOISE by one (it will always be OK to do so because
		   the HARE is always ahead of the TORTOISE and will have
		   already verified the path), then make sure TORTOISE and
		   HARE don't contain the same non-nil object -- if the
		   TORTOISE and the HARE ever meet, then obviously we're
		   in a circularity, and if we're in a circularity, then
		   the TORTOISE and the HARE can't cross paths without
		   meeting, since the HARE only gains one step over the
		   TORTOISE per iteration. */

		if (!NILP(**hare)) {
			Lisp_Object *haresave = *hare;
			if (!CONSP(**hare)) {
				*retval = bad_bad_bunny(plist, haresave, errb);
				return 0;
			}
			*hare = &XCDR(**hare);
			/* In a non-plist, we'd check here for a nil value for
			 **HARE, which is OK (it just means the list has an
			 odd number of elements).  In a plist, it's not OK
			 for the list to have an odd number of elements. */
			if (!CONSP(**hare)) {
				*retval = bad_bad_bunny(plist, haresave, errb);
				return 0;
			}
			*hare = &XCDR(**hare);
		}

		*tortoise = &XCDR(**tortoise);
		if (!NILP(**hare) && EQ(**tortoise, **hare)) {
			*retval = bad_bad_turtle(plist, tortsave, errb);
			return 0;
		}
	}

	return 1;
}

/* Return the value of PROPERTY from PLIST, or Qunbound if
   property is not on the list.

   PLIST is a Lisp-accessible property list, meaning that it
   has to be checked for malformations and circularities.

   If ERRB is ERROR_ME, an error will be signalled.  Otherwise, the
   function will never signal an error; and if ERRB is ERROR_ME_WARN,
   on finding a malformation or a circularity, it issues a warning and
   attempts to silently fix the problem.

   A pointer to PLIST is passed in so that PLIST can be successfully
   "fixed" even if the error is at the beginning of the plist. */

Lisp_Object
external_plist_get(Lisp_Object * plist, Lisp_Object property,
		   int laxp, Error_behavior errb)
{
	Lisp_Object *tortoise = plist;
	Lisp_Object *hare = plist;

	while (!NILP(*tortoise)) {
		Lisp_Object *tortsave = tortoise;
		Lisp_Object retval;

		/* We do the standard tortoise/hare march.  We isolate the
		   grungy stuff to do this in advance_plist_pointers(), though.
		   To us, all this function does is advance the tortoise
		   pointer by two and the hare pointer by four and make sure
		   everything's OK.  We first advance the pointers and then
		   check if a property matched; this ensures that our
		   check for a matching property is safe. */

		if (!advance_plist_pointers
		    (plist, &tortoise, &hare, errb, &retval))
			return retval;

		if (!laxp ? EQ(XCAR(*tortsave), property)
		    : internal_equal(XCAR(*tortsave), property, 0))
			return XCAR(XCDR(*tortsave));
	}

	return Qunbound;
}

/* Set PLIST's value for PROPERTY to VALUE, given a possibly
   malformed or circular plist.  Analogous to external_plist_get(). */

void
external_plist_put(Lisp_Object * plist, Lisp_Object property,
		   Lisp_Object value, int laxp, Error_behavior errb)
{
	Lisp_Object *tortoise = plist;
	Lisp_Object *hare = plist;

	while (!NILP(*tortoise)) {
		Lisp_Object *tortsave = tortoise;
		Lisp_Object retval;

		/* See above */
		if (!advance_plist_pointers
		    (plist, &tortoise, &hare, errb, &retval))
			return;

		if (!laxp ? EQ(XCAR(*tortsave), property)
		    : internal_equal(XCAR(*tortsave), property, 0)) {
			XCAR(XCDR(*tortsave)) = value;
			return;
		}
	}

	*plist = Fcons(property, Fcons(value, *plist));
}

int
external_remprop(Lisp_Object * plist, Lisp_Object property,
		 int laxp, Error_behavior errb)
{
	Lisp_Object *tortoise = plist;
	Lisp_Object *hare = plist;

	while (!NILP(*tortoise)) {
		Lisp_Object *tortsave = tortoise;
		Lisp_Object retval;

		/* See above */
		if (!advance_plist_pointers
		    (plist, &tortoise, &hare, errb, &retval))
			return 0;

		if (!laxp ? EQ(XCAR(*tortsave), property)
		    : internal_equal(XCAR(*tortsave), property, 0)) {
			/* Now you see why it's so convenient to have that level
			   of indirection. */
			*tortsave = XCDR(XCDR(*tortsave));
			return 1;
		}
	}

	return 0;
}

DEFUN("plist-get", Fplist_get, 2, 3, 0,	/*
Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...).
PROPERTY is usually a symbol.
This function returns the value corresponding to the PROPERTY,
or DEFAULT if PROPERTY is not one of the properties on the list.
*/
      (plist, property, default_))
{
	Lisp_Object value = external_plist_get(&plist, property, 0, ERROR_ME);
	return UNBOUNDP(value) ? default_ : value;
}

DEFUN("plist-put", Fplist_put, 3, 3, 0,	/*
Change value in PLIST of PROPERTY to VALUE.
PLIST is a property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2 ...).
PROPERTY is usually a symbol and VALUE is any object.
If PROPERTY is already a property on the list, its value is set to VALUE,
otherwise the new PROPERTY VALUE pair is added.
The new plist is returned; use `(setq x (plist-put x property value))'
to be sure to use the new value.  PLIST is modified by side effect.
*/
      (plist, property, value))
{
	external_plist_put(&plist, property, value, 0, ERROR_ME);
	return plist;
}

DEFUN("plist-remprop", Fplist_remprop, 2, 2, 0,	/*
Remove from PLIST the property PROPERTY and its value.
PLIST is a property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2 ...).
PROPERTY is usually a symbol.
The new plist is returned; use `(setq x (plist-remprop x property))'
to be sure to use the new value.  PLIST is modified by side effect.
*/
      (plist, property))
{
	external_remprop(&plist, property, 0, ERROR_ME);
	return plist;
}

DEFUN("plist-member", Fplist_member, 2, 2, 0,	/*
Return t if PROPERTY has a value specified in PLIST.
*/
      (plist, property))
{
	Lisp_Object value = Fplist_get(plist, property, Qunbound);
	return UNBOUNDP(value) ? Qnil : Qt;
}

DEFUN("check-valid-plist", Fcheck_valid_plist, 1, 1, 0,	/*
Given a plist, signal an error if there is anything wrong with it.
This means that it's a malformed or circular plist.
*/
      (plist))
{
	Lisp_Object *tortoise;
	Lisp_Object *hare;

      start_over:
	tortoise = &plist;
	hare = &plist;
	while (!NILP(*tortoise)) {
		Lisp_Object retval;

		/* See above */
		if (!advance_plist_pointers(&plist, &tortoise, &hare, ERROR_ME,
					    &retval))
			goto start_over;
	}

	return Qnil;
}

DEFUN("valid-plist-p", Fvalid_plist_p, 1, 1, 0,	/*
Given a plist, return non-nil if its format is correct.
If it returns nil, `check-valid-plist' will signal an error when given
the plist; that means it's a malformed or circular plist.
*/
      (plist))
{
	Lisp_Object *tortoise;
	Lisp_Object *hare;

	tortoise = &plist;
	hare = &plist;
	while (!NILP(*tortoise)) {
		Lisp_Object retval;

		/* See above */
		if (!advance_plist_pointers
		    (&plist, &tortoise, &hare, ERROR_ME_NOT, &retval))
			return Qnil;
	}

	return Qt;
}

DEFUN("canonicalize-plist", Fcanonicalize_plist, 1, 2, 0,	/*
Destructively remove any duplicate entries from a plist.
In such cases, the first entry applies.

If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
a nil value is removed.  This feature is a virus that has infected
old Lisp implementations, but should not be used except for backward
compatibility.

The new plist is returned.  If NIL-MEANS-NOT-PRESENT is given, the
return value may not be EQ to the passed-in value, so make sure to
`setq' the value back into where it came from.
*/
      (plist, nil_means_not_present))
{
	Lisp_Object head = plist;

	Fcheck_valid_plist(plist);

	while (!NILP(plist)) {
		Lisp_Object prop = Fcar(plist);
		Lisp_Object next = Fcdr(plist);

		CHECK_CONS(next);	/* just make doubly sure we catch any errors */
		if (!NILP(nil_means_not_present) && NILP(Fcar(next))) {
			if (EQ(head, plist))
				head = Fcdr(next);
			plist = Fcdr(next);
			continue;
		}
		/* external_remprop returns 1 if it removed any property.
		   We have to loop till it didn't remove anything, in case
		   the property occurs many times. */
		while (external_remprop(&XCDR(next), prop, 0, ERROR_ME))
			DO_NOTHING;
		plist = Fcdr(next);
	}

	return head;
}

DEFUN("lax-plist-get", Flax_plist_get, 2, 3, 0,	/*
Extract a value from a lax property list.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
PROPERTY is usually a symbol.
This function returns the value corresponding to PROPERTY,
or DEFAULT if PROPERTY is not one of the properties on the list.
*/
      (lax_plist, property, default_))
{
	Lisp_Object value =
	    external_plist_get(&lax_plist, property, 1, ERROR_ME);
	return UNBOUNDP(value) ? default_ : value;
}

DEFUN("lax-plist-put", Flax_plist_put, 3, 3, 0,	/*
Change value in LAX-PLIST of PROPERTY to VALUE.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
PROPERTY is usually a symbol and VALUE is any object.
If PROPERTY is already a property on the list, its value is set to
VALUE, otherwise the new PROPERTY VALUE pair is added.
The new plist is returned; use `(setq x (lax-plist-put x property value))'
to be sure to use the new value.  LAX-PLIST is modified by side effect.
*/
      (lax_plist, property, value))
{
	external_plist_put(&lax_plist, property, value, 1, ERROR_ME);
	return lax_plist;
}

DEFUN("lax-plist-remprop", Flax_plist_remprop, 2, 2, 0,	/*
Remove from LAX-PLIST the property PROPERTY and its value.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
PROPERTY is usually a symbol.
The new plist is returned; use `(setq x (lax-plist-remprop x property))'
to be sure to use the new value.  LAX-PLIST is modified by side effect.
*/
      (lax_plist, property))
{
	external_remprop(&lax_plist, property, 1, ERROR_ME);
	return lax_plist;
}

DEFUN("lax-plist-member", Flax_plist_member, 2, 2, 0,	/*
Return t if PROPERTY has a value specified in LAX-PLIST.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
*/
      (lax_plist, property))
{
	return UNBOUNDP(Flax_plist_get(lax_plist, property, Qunbound)) ? Qnil :
	    Qt;
}

DEFUN("canonicalize-lax-plist", Fcanonicalize_lax_plist, 1, 2, 0,	/*
Destructively remove any duplicate entries from a lax plist.
In such cases, the first entry applies.

If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
a nil value is removed.  This feature is a virus that has infected
old Lisp implementations, but should not be used except for backward
compatibility.

The new plist is returned.  If NIL-MEANS-NOT-PRESENT is given, the
return value may not be EQ to the passed-in value, so make sure to
`setq' the value back into where it came from.
*/
      (lax_plist, nil_means_not_present))
{
	Lisp_Object head = lax_plist;

	Fcheck_valid_plist(lax_plist);

	while (!NILP(lax_plist)) {
		Lisp_Object prop = Fcar(lax_plist);
		Lisp_Object next = Fcdr(lax_plist);

		CHECK_CONS(next);	/* just make doubly sure we catch any errors */
		if (!NILP(nil_means_not_present) && NILP(Fcar(next))) {
			if (EQ(head, lax_plist))
				head = Fcdr(next);
			lax_plist = Fcdr(next);
			continue;
		}
		/* external_remprop returns 1 if it removed any property.
		   We have to loop till it didn't remove anything, in case
		   the property occurs many times. */
		while (external_remprop(&XCDR(next), prop, 1, ERROR_ME))
			DO_NOTHING;
		lax_plist = Fcdr(next);
	}

	return head;
}

/* In C because the frame props stuff uses it */

DEFUN("destructive-alist-to-plist", Fdestructive_alist_to_plist, 1, 1, 0,	/*
Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is destroyed in the process of constructing the plist.
See also `alist-to-plist'.
*/
      (alist))
{
	Lisp_Object head = alist;
	while (!NILP(alist)) {
		/* remember the alist element. */
		Lisp_Object el = Fcar(alist);

		Fsetcar(alist, Fcar(el));
		Fsetcar(el, Fcdr(el));
		Fsetcdr(el, Fcdr(alist));
		Fsetcdr(alist, el);
		alist = Fcdr(Fcdr(alist));
	}

	return head;
}

DEFUN("get", Fget, 2, 3, 0,	/*
Return the value of OBJECT's PROPERTY property.
This is the last VALUE stored with `(put OBJECT PROPERTY VALUE)'.
If there is no such property, return optional third arg DEFAULT
\(which defaults to `nil').  OBJECT can be a symbol, string, extent,
face, or glyph.  See also `put', `remprop', and `object-plist'.
*/
      (object, property, default_))
{
	/* Various places in emacs call Fget() and expect it not to quit,
	   so don't quit. */
	Lisp_Object val;

	if (LRECORDP(object) && XRECORD_LHEADER_IMPLEMENTATION(object)->getprop)
		val =
		    XRECORD_LHEADER_IMPLEMENTATION(object)->getprop(object,
								    property);
	else
		signal_simple_error("Object type has no properties", object);

	return UNBOUNDP(val) ? default_ : val;
}

DEFUN("put", Fput, 3, 3, 0,	/*
Set OBJECT's PROPERTY to VALUE.
It can be subsequently retrieved with `(get OBJECT PROPERTY)'.
OBJECT can be a symbol, face, extent, or string.
For a string, no properties currently have predefined meanings.
For the predefined properties for extents, see `set-extent-property'.
For the predefined properties for faces, see `set-face-property'.
See also `get', `remprop', and `object-plist'.
*/
      (object, property, value))
{
	CHECK_LISP_WRITEABLE(object);

	if (LRECORDP(object) && XRECORD_LHEADER_IMPLEMENTATION(object)->putprop) {
		if (!XRECORD_LHEADER_IMPLEMENTATION(object)->putprop
		    (object, property, value))
			signal_simple_error("Can't set property on object",
					    property);
	} else
		signal_simple_error("Object type has no settable properties",
				    object);

	return value;
}

DEFUN("remprop", Fremprop, 2, 2, 0,	/*
Remove, from OBJECT's property list, PROPERTY and its corresponding value.
OBJECT can be a symbol, string, extent, face, or glyph.  Return non-nil
if the property list was actually modified (i.e. if PROPERTY was present
in the property list).  See also `get', `put', and `object-plist'.
*/
      (object, property))
{
	int ret = 0;

	CHECK_LISP_WRITEABLE(object);

	if (LRECORDP(object) && XRECORD_LHEADER_IMPLEMENTATION(object)->remprop) {
		ret =
		    XRECORD_LHEADER_IMPLEMENTATION(object)->remprop(object,
								    property);
		if (ret == -1)
			signal_simple_error("Can't remove property from object",
					    property);
	} else
		signal_simple_error("Object type has no removable properties",
				    object);

	return ret ? Qt : Qnil;
}

DEFUN("object-plist", Fobject_plist, 1, 1, 0,	/*
Return a property list of OBJECT's properties.
For a symbol, this is equivalent to `symbol-plist'.
OBJECT can be a symbol, string, extent, face, or glyph.
Do not modify the returned property list directly;
this may or may not have the desired effects.  Use `put' instead.
*/
      (object))
{
	if (LRECORDP(object) && XRECORD_LHEADER_IMPLEMENTATION(object)->plist)
		return XRECORD_LHEADER_IMPLEMENTATION(object)->plist(object);
	else
		signal_simple_error("Object type has no properties", object);

	return Qnil;
}

int internal_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	if (depth > 200)
		error("Stack overflow in equal");
	QUIT;
	if (EQ_WITH_EBOLA_NOTICE(obj1, obj2))
		return 1;
	/* Note that (equal 20 20.0) should be nil */
	if (XTYPE(obj1) != XTYPE(obj2))
		return 0;
	if (LRECORDP(obj1)) {
		const struct lrecord_implementation
		*imp1 = XRECORD_LHEADER_IMPLEMENTATION(obj1),
		    *imp2 = XRECORD_LHEADER_IMPLEMENTATION(obj2);

		return (imp1 == imp2) &&
		    /* EQ-ness of the objects was noticed above */
		    (imp1->equal && (imp1->equal) (obj1, obj2, depth));
	}

	return 0;
}

int
internal_equalp (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	if (depth > 200)
		error ("Stack overflow in equalp");
	QUIT;
	if (EQ_WITH_EBOLA_NOTICE (obj1, obj2))
		return 1;

	if (NUMBERP(obj1) && NUMBERP(obj2)) {
		return ent_binrel(ASE_BINARY_REL_EQUALP, obj1, obj2);
	}

	if (CHARP(obj1) && CHARP(obj2))
		return XCHAR(obj1) == XCHAR(obj2);
	if (XTYPE(obj1) != XTYPE(obj2))
		return 0;
	if (LRECORDP(obj1)) {
		const struct lrecord_implementation
			*imp1 = XRECORD_LHEADER_IMPLEMENTATION (obj1),
			*imp2 = XRECORD_LHEADER_IMPLEMENTATION (obj2);
		
		/* #### not yet implemented properly, needs another flag to specify
		   equalp-ness */
		return (imp1 == imp2) &&
			/* EQ-ness of the objects was noticed above */
			(imp1->equal && (imp1->equal) (obj1, obj2, depth));
	}

	return 0;
}


/* Note that we may be calling sub-objects that will use
   internal_equal() (instead of internal_old_equal()).  Oh well.
   We will get an Ebola note if there's any possibility of confusion,
   but that seems unlikely. */

static int internal_old_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	if (depth > 200)
		error("Stack overflow in equal");
	QUIT;
	if (HACKEQ_UNSAFE(obj1, obj2))
		return 1;
	/* Note that (equal 20 20.0) should be nil */
	if (XTYPE(obj1) != XTYPE(obj2))
		return 0;

	return internal_equal(obj1, obj2, depth);
}

DEFUN("equal", Fequal, 2, 2, 0,	/*
Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
Conses are compared by comparing the cars and the cdrs.
Vectors and strings are compared element by element.
Numbers are compared by value.  Symbols must match exactly.
*/
      (object1, object2))
{
	return internal_equal(object1, object2, 0) ? Qt : Qnil;
}

DEFUN("old-equal", Fold_equal, 2, 2, 0,	/*
Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
\(Note, however, that an exception is made for characters and integers;
this is known as the "char-int confoundance disease." See `eq' and
`old-eq'.)
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
      (object1, object2))
{
	return internal_old_equal(object1, object2, 0) ? Qt : Qnil;
}

DEFUN("fillarray", Ffillarray, 2, 2, 0,	/*
Destructively modify ARRAY by replacing each element with ITEM.
ARRAY is a vector, bit vector, or string.
*/
      (array, item))
{
      retry:
	if (STRINGP(array)) {
		Lisp_String *s = XSTRING(array);
		Bytecount old_bytecount = string_length(s);
		Bytecount new_bytecount;
		Bytecount item_bytecount;
		Bufbyte item_buf[MAX_EMCHAR_LEN];
		Bufbyte *p;
		Bufbyte *end;

		CHECK_CHAR_COERCE_INT(item);
		CHECK_LISP_WRITEABLE(array);

		item_bytecount = set_charptr_emchar(item_buf, XCHAR(item));
		new_bytecount = item_bytecount * string_char_length(s);

		resize_string(s, -1, new_bytecount - old_bytecount);

		for (p = string_data(s), end = p + new_bytecount;
		     p < end; p += item_bytecount)
			memcpy(p, item_buf, item_bytecount);
		*p = '\0';

		bump_string_modiff(array);
	} else if (VECTORP(array)) {
		Lisp_Object *p = XVECTOR_DATA(array);
		size_t len = XVECTOR_LENGTH(array);
		CHECK_LISP_WRITEABLE(array);
		while (len--)
			*p++ = item;
	} else if (BIT_VECTORP(array)) {
		Lisp_Bit_Vector *v = XBIT_VECTOR(array);
		size_t len = bit_vector_length(v);
		int bit;
		CHECK_BIT(item);
		bit = XINT(item);
		CHECK_LISP_WRITEABLE(array);
		while (len--)
			set_bit_vector_bit(v, len, bit);
	} else {
		array = wrong_type_argument(Qarrayp, array);
		goto retry;
	}
	return array;
}

Lisp_Object nconc2(Lisp_Object arg1, Lisp_Object arg2)
{
	Lisp_Object args[2] = {arg1, arg2};
	struct gcpro gcpro1;

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(bytecode_nconc2(args));
}

Lisp_Object bytecode_nconc2(Lisp_Object * args)
{
      retry:

	if (CONSP(args[0])) {
		/* (setcdr (last args[0]) args[1]) */
		Lisp_Object tortoise, hare;
		size_t count;

		for (hare = tortoise = args[0], count = 0;
		     CONSP(XCDR(hare)); hare = XCDR(hare), count++) {
			if (count < CIRCULAR_LIST_SUSPICION_LENGTH)
				continue;

			if (count & 1)
				tortoise = XCDR(tortoise);
			if (EQ(hare, tortoise))
				signal_circular_list_error(args[0]);
		}
		XCDR(hare) = args[1];
		return args[0];
	} else if (NILP(args[0])) {
		return args[1];
	} else {
		args[0] = wrong_type_argument(args[0], Qlistp);
		goto retry;
	}
}

DEFUN("nconc", Fnconc, 0, MANY, 0,	/*
Concatenate any number of lists by altering them.
Only the last argument is not altered, and need not be a list.
Also see: `append'.
If the first argument is nil, there is no way to modify it by side
effect; therefore, write `(setq foo (nconc foo list))' to be sure of
changing the value of `foo'.
*/
      (int nargs, Lisp_Object * args))
{
	int argnum = 0;
	struct gcpro gcpro1;

	/* The modus operandi in Emacs is "caller gc-protects args".
	   However, nconc (particularly nconc2 ()) is called many times
	   in Emacs on freshly created stuff (e.g. you see the idiom
	   nconc2 (Fcopy_sequence (foo), bar) a lot).  So we help those
	   callers out by protecting the args ourselves to save them
	   a lot of temporary-variable grief. */

	GCPROn(args, nargs);

	while (argnum < nargs) {
		Lisp_Object val;
	retry:
		val = args[argnum];
		if (CONSP(val)) {
			/* `val' is the first cons, which will be our return
			 * value.
			 * `last_cons' will be the cons cell to mutate.  */
			Lisp_Object last_cons = val;
			Lisp_Object tortoise = val;

			for (argnum++; argnum < nargs; argnum++) {
				Lisp_Object next = args[argnum];
			      retry_next:
				if (CONSP(next) || argnum == nargs - 1) {
					/* (setcdr (last val) next) */
					size_t count;

					for (count = 0;
					     CONSP(XCDR(last_cons));
					     last_cons =
					     XCDR(last_cons), count++) {
						if (count <
						    CIRCULAR_LIST_SUSPICION_LENGTH)
							continue;

						if (count & 1)
							tortoise =
							    XCDR(tortoise);
						if (EQ(last_cons, tortoise))
							signal_circular_list_error
							    (args[argnum - 1]);
					}
					XCDR(last_cons) = next;
				} else if (NILP(next)) {
					continue;
				} else {
					next =
					    wrong_type_argument(Qlistp, next);
					goto retry_next;
				}
			}
			RETURN_UNGCPRO(val);
		} else if (NILP(val))
			argnum++;
		else if (argnum == nargs - 1)	/* last arg? */
			RETURN_UNGCPRO(val);
		else {
			args[argnum] = wrong_type_argument(Qlistp, val);
			goto retry;
		}
	}
	RETURN_UNGCPRO(Qnil);	/* No non-nil args provided. */
}


DEFUN("replace-list", Freplace_list, 2, 2, 0,	/*
Destructively replace the list OLD with NEW.
This is like (copy-sequence NEW) except that it reuses the
conses in OLD as much as possible.  If OLD and NEW are the same
length, no consing will take place.
*/
      (old, new))
{
	Lisp_Object tail, oldtail = old, prevoldtail = Qnil;

	EXTERNAL_LIST_LOOP(tail, new) {
		if (!NILP(oldtail)) {
			CHECK_CONS(oldtail);
			XCAR(oldtail) = XCAR(tail);
		} else if (!NILP(prevoldtail)) {
			XCDR(prevoldtail) = Fcons(XCAR(tail), Qnil);
			prevoldtail = XCDR(prevoldtail);
		} else
			old = oldtail = Fcons(XCAR(tail), Qnil);

		if (!NILP(oldtail)) {
			prevoldtail = oldtail;
			oldtail = XCDR(oldtail);
		}
	}

	if (!NILP(prevoldtail))
		XCDR(prevoldtail) = Qnil;
	else
		old = Qnil;

	return old;
}

/* #### this function doesn't belong in this file! */

#ifdef HAVE_GETLOADAVG
#ifdef HAVE_SYS_LOADAVG_H
#include <sys/loadavg.h>
#endif
#else
int getloadavg(double loadavg[], int nelem);	/* Defined in getloadavg.c */
#endif

DEFUN("load-average", Fload_average, 0, 1, 0,	/*
Return list of 1 minute, 5 minute and 15 minute load averages.
Each of the three load averages is multiplied by 100,
then converted to integer.

When USE-FLOATS is non-nil, floats will be used instead of integers.
These floats are not multiplied by 100.

If the 5-minute or 15-minute load averages are not available, return a
shortened list, containing only those averages which are available.

On some systems, this won't work due to permissions on /dev/kmem,
in which case you can't use this.
*/
      (use_floats))
{
	double load_ave[3];
	int loads = getloadavg(load_ave, countof(load_ave));
	Lisp_Object ret = Qnil;

	if (loads == -2)
		error("load-average not implemented for this operating system");
	else if (loads < 0)
		signal_simple_error("Could not get load-average",
				    lisp_strerror(errno));

	while (loads-- > 0) {
		Lisp_Object load = (NILP(use_floats) ?
				    make_int((int)(100.0 * load_ave[loads]))
				    : make_float(load_ave[loads]));
		ret = Fcons(load, ret);
	}
	return ret;
}

Lisp_Object Vfeatures;

DEFUN("featurep", Ffeaturep, 1, 1, 0,	/*
Return non-nil if feature FEXP is present in this Emacs.
Use this to conditionalize execution of lisp code based on the
presence or absence of emacs or environment extensions.
FEXP can be a symbol, a number, or a list.
If it is a symbol, that symbol is looked up in the `features' variable,
and non-nil will be returned if found.
If it is a number, the function will return non-nil if this Emacs
has an equal or greater version number than FEXP.
If it is a list whose car is the symbol `and', it will return
non-nil if all the features in its cdr are non-nil.
If it is a list whose car is the symbol `or', it will return non-nil
if any of the features in its cdr are non-nil.
If it is a list whose car is the symbol `not', it will return
non-nil if the feature is not present.

Examples:

(featurep 'sxemacs)
=> ; Non-nil on SXEmacs.

(featurep '(and sxemacs gnus))
=> ; Non-nil on SXEmacs with Gnus loaded.

(featurep '(or tty-frames (and emacs 19.30)))
=> ; Non-nil if this Emacs supports TTY frames.

(featurep '(or (and xemacs 19.15) (and emacs 19.34)))
=> ; Non-nil on XEmacs 19.15 and later, or FSF Emacs 19.34 and later.

(featurep '(and xemacs 21.02))
=> ; Non-nil on XEmacs 21.2 and later.

NOTE: The advanced arguments of this function (anything other than a
symbol) are not yet supported by FSF Emacs.  If you feel they are useful
for supporting multiple Emacs variants, lobby Richard Stallman at
<bug-gnu-emacs@gnu.org>.
*/
      (fexp))
{
#ifndef FEATUREP_SYNTAX
	CHECK_SYMBOL(fexp);
	return NILP(Fmemq(fexp, Vfeatures)) ? Qnil : Qt;
#else				/* FEATUREP_SYNTAX */
	static double featurep_emacs_version;

	/* Brute force translation from Erik Naggum's lisp function. */
	if (SYMBOLP(fexp)) {
		/* Original definition */
		return NILP(Fmemq(fexp, Vfeatures)) ? Qnil : Qt;
	} else if (INTP(fexp) || FLOATP(fexp)) {
		double d = extract_float(fexp);

		if (featurep_emacs_version == 0.0) {
			featurep_emacs_version = XINT(Vemacs_major_version) +
			    (XINT(Vemacs_minor_version) / 100.0);
		}
		return featurep_emacs_version >= d ? Qt : Qnil;
	} else if (CONSP(fexp)) {
		Lisp_Object tem = XCAR(fexp);
		if (EQ(tem, Qnot)) {
			Lisp_Object negate;

			tem = XCDR(fexp);
			negate = Fcar(tem);
			if (!NILP(tem))
				return NILP(call1(Qfeaturep, negate)) ? Qt :
				    Qnil;
			else
				return Fsignal(Qinvalid_read_syntax,
					       list1(tem));
		} else if (EQ(tem, Qand)) {
			tem = XCDR(fexp);
			/* Use Fcar/Fcdr for error-checking. */
			while (!NILP(tem) && !NILP(call1(Qfeaturep, Fcar(tem)))) {
				tem = Fcdr(tem);
			}
			return NILP(tem) ? Qt : Qnil;
		} else if (EQ(tem, Qor)) {
			tem = XCDR(fexp);
			/* Use Fcar/Fcdr for error-checking. */
			while (!NILP(tem) && NILP(call1(Qfeaturep, Fcar(tem)))) {
				tem = Fcdr(tem);
			}
			return NILP(tem) ? Qnil : Qt;
		} else {
			return Fsignal(Qinvalid_read_syntax, list1(XCDR(fexp)));
		}
	} else {
		return Fsignal(Qinvalid_read_syntax, list1(fexp));
	}
}
#endif				/* FEATUREP_SYNTAX */

DEFUN("provide", Fprovide, 1, 1, 0,	/*
Announce that FEATURE is a feature of the current Emacs.
This function updates the value of the variable `features'.
*/
      (feature))
{
	Lisp_Object tem;
	CHECK_SYMBOL(feature);
	if (!NILP(Vautoload_queue))
		Vautoload_queue =
		    Fcons(Fcons(Vfeatures, Qnil), Vautoload_queue);
	tem = Fmemq(feature, Vfeatures);
	if (NILP(tem))
		Vfeatures = Fcons(feature, Vfeatures);
	LOADHIST_ATTACH(Fcons(Qprovide, feature));
	return feature;
}

DEFUN("require", Frequire, 1, 2, 0,	/*
If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name.
*/
      (feature, filename))
{
	Lisp_Object tem;

	CHECK_SYMBOL(feature);
	tem = Fmemq(feature, Vfeatures);
	LOADHIST_ATTACH(Fcons(Qrequire, feature));

	if (!NILP(tem)) {
		return feature;
	} else {
		int speccount = specpdl_depth();

		/* Value saved here is to be restored into Vautoload_queue */
		record_unwind_protect(un_autoload, Vautoload_queue);
		Vautoload_queue = Qt;

		/* defined in code-files.el */
		call4(Qload, NILP(filename) ? Fsymbol_name(feature) : filename,
		      Qnil, Qt, Qnil);

		tem = Fmemq(feature, Vfeatures);
		if (NILP(tem))
			error("Required feature %s was not provided",
			      string_data(XSYMBOL(feature)->name));

		/* Once loading finishes, don't undo it.  */
		Vautoload_queue = Qt;
		return unbind_to(speccount, feature);
	}
}

DEFUN("revoke", Frevoke, 1, 1, 0,	/*
Announce that FEATURE is no longer a feature of the current Emacs.
*/
      (feature))
{
	CHECK_SYMBOL(feature);
	if (!NILP(Vautoload_queue))
		Vautoload_queue =
		    Fcons(Fcons(Vfeatures, Qnil), Vautoload_queue);

	if (LIKELY(CONSP(Vfeatures) && EQ(XCAR(Vfeatures), feature))) {
		/* special case where feature is the head of 'features */
		Vfeatures = XCDR(Vfeatures);
		return feature;
	}
	for (Lisp_Object tmp = Vfeatures;
	     CONSP(tmp) && CONSP(XCDR(tmp));
	     tmp = XCDR(tmp)) {
		if (EQ(XCAR(XCDR(tmp)), feature)) {
			XCDR(tmp) = XCDR(XCDR(tmp));
		}
		return feature;
	}
	return Qnil;
}

/* base64 encode/decode functions.

   Originally based on code from GNU recode.  Ported to FSF Emacs by
   Lars Magne Ingebrigtsen and Karl Heuer.  Ported to XEmacs and
   subsequently heavily hacked by Hrvoje Niksic.  */

#define MIME_LINE_LENGTH 72

#define IS_ASCII(Character) \
  ((Character) < 128)
#define IS_BASE64(Character) \
  (IS_ASCII (Character) && base64_char_to_value[Character] >= 0)

/* Table of characters coding the 64 values.  */
static char base64_value_to_char[64] = {
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',	/*  0- 9 */
	'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',	/* 10-19 */
	'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',	/* 20-29 */
	'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',	/* 30-39 */
	'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',	/* 40-49 */
	'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7',	/* 50-59 */
	'8', '9', '+', '/'	/* 60-63 */
};

/* Table of base64 values for first 128 characters.  */
static short base64_char_to_value[128] = {
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,	/*   0-  9 */
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,	/*  10- 19 */
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,	/*  20- 29 */
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,	/*  30- 39 */
	-1, -1, -1, 62, -1, -1, -1, 63, 52, 53,	/*  40- 49 */
	54, 55, 56, 57, 58, 59, 60, 61, -1, -1,	/*  50- 59 */
	-1, -1, -1, -1, -1, 0, 1, 2, 3, 4,	/*  60- 69 */
	5, 6, 7, 8, 9, 10, 11, 12, 13, 14,	/*  70- 79 */
	15, 16, 17, 18, 19, 20, 21, 22, 23, 24,	/*  80- 89 */
	25, -1, -1, -1, -1, -1, -1, 26, 27, 28,	/*  90- 99 */
	29, 30, 31, 32, 33, 34, 35, 36, 37, 38,	/* 100-109 */
	39, 40, 41, 42, 43, 44, 45, 46, 47, 48,	/* 110-119 */
	49, 50, 51, -1, -1, -1, -1, -1	/* 120-127 */
};

/* The following diagram shows the logical steps by which three octets
   get transformed into four base64 characters.

		 .--------.  .--------.  .--------.
		 |aaaaaabb|  |bbbbcccc|  |ccdddddd|
		 `--------'  `--------'  `--------'
                    6   2      4   4       2   6
	       .--------+--------+--------+--------.
	       |00aaaaaa|00bbbbbb|00cccccc|00dddddd|
	       `--------+--------+--------+--------'

	       .--------+--------+--------+--------.
	       |AAAAAAAA|BBBBBBBB|CCCCCCCC|DDDDDDDD|
	       `--------+--------+--------+--------'

   The octets are divided into 6 bit chunks, which are then encoded into
   base64 characters.  */

#define ADVANCE_INPUT(c, stream)				\
 ((ec = Lstream_get_emchar (stream)) == -1 ? 0 :		\
  ((ec > 255) ?							\
   (signal_simple_error ("Non-ascii character in base64 input",	\
			 make_char (ec)), 0)			\
   : (c = (Bufbyte)ec), 1))

static Bytind base64_encode_1(Lstream * istream, Bufbyte * to, int line_break)
{
	EMACS_INT counter = 0;
	Bufbyte *e = to;
	Emchar ec;
	unsigned int value;

	while (1) {
		Bufbyte c;
		if (!ADVANCE_INPUT(c, istream))
			break;

		/* Wrap line every 76 characters.  */
		if (line_break) {
			if (counter < MIME_LINE_LENGTH / 4)
				counter++;
			else {
				*e++ = '\n';
				counter = 1;
			}
		}

		/* Process first byte of a triplet.  */
		*e++ = base64_value_to_char[0x3f & c >> 2];
		value = (0x03 & c) << 4;

		/* Process second byte of a triplet.  */
		if (!ADVANCE_INPUT(c, istream)) {
			*e++ = base64_value_to_char[value];
			*e++ = '=';
			*e++ = '=';
			break;
		}

		*e++ = base64_value_to_char[value | (0x0f & c >> 4)];
		value = (0x0f & c) << 2;

		/* Process third byte of a triplet.  */
		if (!ADVANCE_INPUT(c, istream)) {
			*e++ = base64_value_to_char[value];
			*e++ = '=';
			break;
		}

		*e++ = base64_value_to_char[value | (0x03 & c >> 6)];
		*e++ = base64_value_to_char[0x3f & c];
	}

	return e - to;
}

#undef ADVANCE_INPUT

/* Get next character from the stream, except that non-base64
   characters are ignored.  This is in accordance with rfc2045.  EC
   should be an Emchar, so that it can hold -1 as the value for EOF.  */
#define ADVANCE_INPUT_IGNORE_NONBASE64(ec, stream, streampos) do {	\
  ec = Lstream_get_emchar (stream);					\
  ++streampos;								\
  /* IS_BASE64 may not be called with negative arguments so check for	\
     EOF first. */							\
  if (ec < 0 || IS_BASE64 (ec) || ec == '=')				\
    break;								\
} while (1)

#define STORE_BYTE(pos, val, ccnt) do {					\
  pos += set_charptr_emchar (pos, (Emchar)((unsigned char)(val)));	\
  ++ccnt;								\
} while (0)

static Bytind
base64_decode_1(Lstream * istream, Bufbyte * to, Charcount * ccptr)
{
	Charcount ccnt = 0;
	Bufbyte *e = to;
	EMACS_INT streampos = 0;

	while (1) {
		Emchar ec;
		unsigned long value;

		/* Process first byte of a quadruplet.  */
		ADVANCE_INPUT_IGNORE_NONBASE64(ec, istream, streampos);
		if (ec < 0)
			break;
		if (ec == '=')
			signal_simple_error
			    ("Illegal `=' character while decoding base64",
			     make_int(streampos));
		value = base64_char_to_value[ec] << 18;

		/* Process second byte of a quadruplet.  */
		ADVANCE_INPUT_IGNORE_NONBASE64(ec, istream, streampos);
		if (ec < 0)
			error("Premature EOF while decoding base64");
		if (ec == '=')
			signal_simple_error
			    ("Illegal `=' character while decoding base64",
			     make_int(streampos));
		value |= base64_char_to_value[ec] << 12;
		STORE_BYTE(e, value >> 16, ccnt);

		/* Process third byte of a quadruplet.  */
		ADVANCE_INPUT_IGNORE_NONBASE64(ec, istream, streampos);
		if (ec < 0)
			error("Premature EOF while decoding base64");

		if (ec == '=') {
			ADVANCE_INPUT_IGNORE_NONBASE64(ec, istream, streampos);
			if (ec < 0)
				error("Premature EOF while decoding base64");
			if (ec != '=')
				signal_simple_error
				    ("Padding `=' expected but not found while decoding base64",
				     make_int(streampos));
			continue;
		}

		value |= base64_char_to_value[ec] << 6;
		STORE_BYTE(e, 0xff & value >> 8, ccnt);

		/* Process fourth byte of a quadruplet.  */
		ADVANCE_INPUT_IGNORE_NONBASE64(ec, istream, streampos);
		if (ec < 0)
			error("Premature EOF while decoding base64");
		if (ec == '=')
			continue;

		value |= base64_char_to_value[ec];
		STORE_BYTE(e, 0xff & value, ccnt);
	}

	*ccptr = ccnt;
	return e - to;
}

#undef ADVANCE_INPUT
#undef ADVANCE_INPUT_IGNORE_NONBASE64
#undef STORE_BYTE

DEFUN("base64-encode-region", Fbase64_encode_region, 2, 3, "r",	/*
Base64-encode the region between START and END.
Return the length of the encoded text.
Optional third argument NO-LINE-BREAK means do not break long lines
into shorter lines.
*/
      (start, end, no_line_break))
{
	Bufbyte *encoded;
	Bytind encoded_length;
	Charcount allength, length;
	struct buffer *buf = current_buffer;
	Bufpos begv, zv, old_pt = BUF_PT(buf);
	Lisp_Object input;
	int speccount = specpdl_depth();

	get_buffer_range_char(buf, start, end, &begv, &zv, 0);
	barf_if_buffer_read_only(buf, begv, zv);

	/* We need to allocate enough room for encoding the text.
	   We need 33 1/3% more space, plus a newline every 76
	   characters, and then we round up. */
	length = zv - begv;
	allength = length + length / 3 + 1;
	allength += allength / MIME_LINE_LENGTH + 1 + 6;

	input = make_lisp_buffer_input_stream(buf, begv, zv, 0);
	/* We needn't multiply allength with MAX_EMCHAR_LEN because all the
	   base64 characters will be single-byte.  */
	XMALLOC_ATOMIC_OR_ALLOCA(encoded, allength, Bufbyte);
	encoded_length = base64_encode_1(XLSTREAM(input), encoded,
					 NILP(no_line_break));
	if (encoded_length > allength) {
		abort();
	}
	Lstream_delete(XLSTREAM(input));

	/* Now we have encoded the region, so we insert the new contents
	   and delete the old.  (Insert first in order to preserve markers.)  */
	buffer_insert_raw_string_1(buf, begv, encoded, encoded_length, 0);
	XMALLOC_UNBIND(encoded, allength, speccount);
	buffer_delete_range(buf, begv + encoded_length, zv + encoded_length, 0);

	/* Simulate FSF Emacs implementation of this function: if point was
	   in the region, place it at the beginning.  */
	if (old_pt >= begv && old_pt < zv) {
		BUF_SET_PT(buf, begv);
	}

	/* We return the length of the encoded text. */
	return make_int(encoded_length);
}

DEFUN("base64-encode-string", Fbase64_encode_string, 1, 2, 0,	/*
Base64 encode STRING and return the result.
Optional argument NO-LINE-BREAK means do not break long lines
into shorter lines.
*/
      (string, no_line_break))
{
	Charcount allength, length;
	Bytind encoded_length;
	Bufbyte *encoded;
	Lisp_Object input, result;
	int speccount = specpdl_depth();

	CHECK_STRING(string);

	length = XSTRING_CHAR_LENGTH(string);
	allength = length + length / 3 + 1;
	allength += allength / MIME_LINE_LENGTH + 1 + 6;

	input = make_lisp_string_input_stream(string, 0, -1);
	XMALLOC_ATOMIC_OR_ALLOCA(encoded, allength, Bufbyte);
	encoded_length = base64_encode_1(XLSTREAM(input), encoded,
					 NILP(no_line_break));
	if (encoded_length > allength) {
		abort();
	}
	Lstream_delete(XLSTREAM(input));
	result = make_string(encoded, encoded_length);
	XMALLOC_UNBIND(encoded, allength, speccount);
	return result;
}

DEFUN("base64-decode-region", Fbase64_decode_region, 2, 2, "r",	/*
Base64-decode the region between START and END.
Return the length of the decoded text.
If the region can't be decoded, return nil and don't modify the buffer.
Characters out of the base64 alphabet are ignored.
*/
      (start, end))
{
	struct buffer *buf = current_buffer;
	Bufpos begv, zv, old_pt = BUF_PT(buf);
	Bufbyte *decoded;
	Bytind decoded_length;
	Charcount length, cc_decoded_length;
	Lisp_Object input;
	int speccount = specpdl_depth();

	get_buffer_range_char(buf, start, end, &begv, &zv, 0);
	barf_if_buffer_read_only(buf, begv, zv);

	length = zv - begv;

	input = make_lisp_buffer_input_stream(buf, begv, zv, 0);
	/* We need to allocate enough room for decoding the text. */
	XMALLOC_ATOMIC_OR_ALLOCA(decoded, length * MAX_EMCHAR_LEN, Bufbyte);
	decoded_length =
		base64_decode_1(XLSTREAM(input), decoded, &cc_decoded_length);
	if (decoded_length > length * MAX_EMCHAR_LEN) {
		abort();
	}
	Lstream_delete(XLSTREAM(input));

	/* Now we have decoded the region, so we insert the new contents
	   and delete the old.  (Insert first in order to preserve markers.)  */
	BUF_SET_PT(buf, begv);
	buffer_insert_raw_string_1(buf, begv, decoded, decoded_length, 0);
	XMALLOC_UNBIND(decoded, length * MAX_EMCHAR_LEN, speccount);
	buffer_delete_range(buf, begv + cc_decoded_length,
			    zv + cc_decoded_length, 0);

	/* Simulate FSF Emacs implementation of this function: if point was
	   in the region, place it at the beginning.  */
	if (old_pt >= begv && old_pt < zv) {
		BUF_SET_PT(buf, begv);
	}

	return make_int(cc_decoded_length);
}

DEFUN("base64-decode-string", Fbase64_decode_string, 1, 1, 0,	/*
Base64-decode STRING and return the result.
Characters out of the base64 alphabet are ignored.
*/
      (string))
{
	Bufbyte *decoded;
	Bytind decoded_length;
	Charcount length, cc_decoded_length;
	Lisp_Object input, result;
	int speccount = specpdl_depth();

	CHECK_STRING(string);

	length = XSTRING_CHAR_LENGTH(string);
	/* We need to allocate enough room for decoding the text. */
	XMALLOC_ATOMIC_OR_ALLOCA(decoded, length * MAX_EMCHAR_LEN, Bufbyte);

	input = make_lisp_string_input_stream(string, 0, -1);
	decoded_length = base64_decode_1(XLSTREAM(input), decoded,
					 &cc_decoded_length);
	if (decoded_length > length * MAX_EMCHAR_LEN) {
		abort();
	}
	Lstream_delete(XLSTREAM(input));

	result = make_string(decoded, decoded_length);
	XMALLOC_UNBIND(decoded, length * MAX_EMCHAR_LEN, speccount);
	return result;
}

/* base16 encode/decode functions. */
static Bytind
base16_encode_1(Lstream * istream, int length, Bufbyte * to, int max)
{
	Emchar ec;
	int i, sz;

	for (i=0; i < length; i++) {
		ec = Lstream_get_emchar (istream);
		sz = snprintf((char *)to+2*i, 3, "%02x", ec);
		assert( sz >= 0 && sz < 3);
		max -= sz;
		assert(max >= 0);
	}

	return 1;
}
static Bytind
base16_decode_1(Lstream * istream, int length, Bufbyte * to)
{
	Emchar ec;
	Emchar high = 0, low = 0;
	int high_set_p = 0, ignore_p = 0;
	int i = 0;

	/* high and low perform flip flop operation */
	while (1) {
		ec = Lstream_get_emchar (istream);
		if (ec < 0)
			break;
		if (isdigit(ec))
			low = ec - '0';
		else if (isupper(ec))
			low = ec - 'A' + 10;
		else if (islower(ec))
			low = ec - 'a' + 10;
		else 
			ignore_p = 1;

		if (low < 0 || low >= 16)
			ignore_p = 1;

		if (!ignore_p) {
			if (!high_set_p) {
				high = low;
				high_set_p = 1;
			} else {
				to[i] = high*16+low;
				i++;
				high_set_p = 0;
			}
		} else
			ignore_p = 0;
	}

	return i;
}
DEFUN("base16-encode-string", Fbase16_encode_string, 1, 1, 0, /*
Base16 encode (i.e. hex dump) STRING and return the result.
Optional argument NO-LINE-BREAK means do not break long lines
into shorter lines.
*/
      (string))
{
	Charcount length;
	Bufbyte *encoded;
	Lisp_Object input, result;
	int sz;
	int speccount = specpdl_depth();

	CHECK_STRING(string);

	length = XSTRING_CHAR_LENGTH(string);
	sz = 2 * length;
	input = make_lisp_string_input_stream(string, 0, -1);
	XMALLOC_ATOMIC_OR_ALLOCA(encoded, sz+1, Bufbyte);
	base16_encode_1(XLSTREAM(input), length, encoded, sz);
	Lstream_delete(XLSTREAM(input));
	result = make_string(encoded, sz);
	XMALLOC_UNBIND(encoded, sz+1, speccount);

	XSTRING(result)->plist = XSTRING(string)->plist;

	return result;
}

DEFUN("base16-decode-string", Fbase16_decode_string, 1, 1, 0, /*
Base16-decode (i.e. read hex data from) STRING and return the result.
Characters out of the base16 alphabet are ignored.
*/
      (string))
{
	Bufbyte *decoded;
	Bytind decoded_length;
	Charcount length;
	Lisp_Object input, result;
	int speccount = specpdl_depth();

	CHECK_STRING(string);

	length = XSTRING_CHAR_LENGTH(string);
	/* We need to allocate enough room for decoding the text. */
	XMALLOC_ATOMIC_OR_ALLOCA(decoded, length, Bufbyte);

	input = make_lisp_string_input_stream(string, 0, -1);
	decoded_length = base16_decode_1(XLSTREAM(input), length, decoded);
	Lstream_delete(XLSTREAM(input));

	/* this result might be raw, we declare it binary */
	result = make_ext_string((char *)decoded, decoded_length, Qbinary);
	XMALLOC_UNBIND(decoded, length, speccount);

	XSTRING(result)->plist = XSTRING(string)->plist;

	return result;
}

Lisp_Object Qyes_or_no_p;

DEFUN("foobar", Ffoobar, 2, 2, 0, /*
*/
      (n, b))
{
	return make_int(__nbits_right_of(XINT(n), XINT(b)));
}

void syms_of_fns(void)
{
	INIT_LRECORD_IMPLEMENTATION(bit_vector);

	defsymbol(&Qstring_lessp, "string-lessp");
	defsymbol(&Qstring_greaterp, "string-greaterp");
	defsymbol(&Qidentity, "identity");
	defsymbol(&Qyes_or_no_p, "yes-or-no-p");

	DEFSUBR(Ffoobar);

	DEFSUBR(Fidentity);
	DEFSUBR(Frandom);
#if defined(WITH_GMP) && defined(HAVE_MPZ)
	DEFSUBR(Frandomb);
#endif
	DEFSUBR(Flength);
	DEFSUBR(Fsafe_length);
	DEFSUBR(Fstring_equal);
	DEFSUBR(Fstring_lessp);
	DEFSUBR(Fstring_greaterp);
	DEFSUBR(Fstring_modified_tick);
	DEFSUBR(Fappend);
	DEFSUBR(Fconcat);
	DEFSUBR(Fvconcat);
	DEFSUBR(Fbvconcat);
	DEFSUBR(Fcopy_list);
	DEFSUBR(Fcopy_sequence);
	DEFSUBR(Fcopy_alist);
	DEFSUBR(Fcopy_tree);
	DEFSUBR(Fsubstring);
	DEFSUBR(Fsubseq);
	DEFSUBR(Fnthcdr);
	DEFSUBR(Fnth);
	DEFSUBR(Felt);
	DEFSUBR(Flast);
	DEFSUBR(Fbutlast);
	DEFSUBR(Fnbutlast);
	DEFSUBR(Fmember);
	DEFSUBR(Fold_member);
	DEFSUBR(Fmemq);
	DEFSUBR(Fold_memq);
	DEFSUBR(Fassoc);
	DEFSUBR(Fold_assoc);
	DEFSUBR(Fassq);
	DEFSUBR(Fold_assq);
	DEFSUBR(Frassoc);
	DEFSUBR(Fold_rassoc);
	DEFSUBR(Frassq);
	DEFSUBR(Fold_rassq);
	DEFSUBR(Fdelete);
	DEFSUBR(Fold_delete);
	DEFSUBR(Fdelq);
	DEFSUBR(Fold_delq);
	DEFSUBR(Fremassoc);
	DEFSUBR(Fremassq);
	DEFSUBR(Fremrassoc);
	DEFSUBR(Fremrassq);
	DEFSUBR(Fnreverse);
	DEFSUBR(Freverse);
	DEFSUBR(Fsort);
	DEFSUBR(Fplists_eq);
	DEFSUBR(Fplists_equal);
	DEFSUBR(Flax_plists_eq);
	DEFSUBR(Flax_plists_equal);
	DEFSUBR(Fplist_get);
	DEFSUBR(Fplist_put);
	DEFSUBR(Fplist_remprop);
	DEFSUBR(Fplist_member);
	DEFSUBR(Fcheck_valid_plist);
	DEFSUBR(Fvalid_plist_p);
	DEFSUBR(Fcanonicalize_plist);
	DEFSUBR(Flax_plist_get);
	DEFSUBR(Flax_plist_put);
	DEFSUBR(Flax_plist_remprop);
	DEFSUBR(Flax_plist_member);
	DEFSUBR(Fcanonicalize_lax_plist);
	DEFSUBR(Fdestructive_alist_to_plist);
	DEFSUBR(Fget);
	DEFSUBR(Fput);
	DEFSUBR(Fremprop);
	DEFSUBR(Fobject_plist);
	DEFSUBR(Fequal);
	DEFSUBR(Fold_equal);
	DEFSUBR(Ffillarray);
	DEFSUBR(Fnconc);
	DEFSUBR(Freplace_list);
	DEFSUBR(Fload_average);
	DEFSUBR(Ffeaturep);
	DEFSUBR(Frequire);
	DEFSUBR(Fprovide);
	DEFSUBR(Frevoke);
	DEFSUBR(Fbase64_encode_region);
	DEFSUBR(Fbase64_encode_string);
	DEFSUBR(Fbase64_decode_region);
	DEFSUBR(Fbase64_decode_string);
	DEFSUBR(Fbase16_encode_string);
	DEFSUBR(Fbase16_decode_string);

#if 1
	map_LTX_init();
#endif
}

void init_provide_once(void)
{
	DEFVAR_LISP("features", &Vfeatures	/*
A list of symbols which are the features of the executing emacs.
Used by `featurep' and `require', and altered by `provide'.
						 */ );
	Vfeatures = Qnil;

	Fprovide(intern("base64"));
	Fprovide(intern("base16"));

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
/* it's fuck ugly to define that here :( */
	Fprovide(intern("bdwgc"));
#endif
}
