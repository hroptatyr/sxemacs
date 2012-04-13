/* Primitive operations on Lisp data types for SXEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 2000 Ben Wing.

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


/* Synched up with: Mule 2.0, FSF 19.30.  Some of FSF's data.c is in
   SXEmacs' symbols.c. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "syssignal.h"
#include "dynacat.h"
#include "ent/ent.h"

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message;
Lisp_Object Qerror, Qquit, Qsyntax_error, Qinvalid_read_syntax;
Lisp_Object Qlist_formation_error, Qstructure_formation_error;
Lisp_Object Qmalformed_list, Qmalformed_property_list;
Lisp_Object Qcircular_list, Qcircular_property_list;
Lisp_Object Qinvalid_argument, Qwrong_type_argument, Qargs_out_of_range;
Lisp_Object Qwrong_number_of_arguments, Qinvalid_function, Qno_catch;
Lisp_Object Qinternal_error, Qinvalid_state, Qinvalid_constant;
Lisp_Object Qvoid_variable, Qcyclic_variable_indirection;
Lisp_Object Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qinvalid_operation, Qinvalid_change, Qout_of_memory;
Lisp_Object Qsetting_constant, Qprinting_unreadable_object;
Lisp_Object Qediting_error, Qconversion_error, Qtext_conversion_error;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qio_error, Qend_of_file;
Lisp_Object Qarith_error, Qrange_error, Qdomain_error, Qstack_overflow;
Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
Lisp_Object Qintegerp, Qnatnump, Qnonnegativep, Qpositivep, Qsymbolp;
Lisp_Object Qlistp, Qtrue_list_p, Qweak_listp;
Lisp_Object Qconsp, Qsubrp;
Lisp_Object Qcharacterp, Qstringp, Qarrayp, Qsequencep, Qvectorp, Qdictp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qbufferp;
Lisp_Object Qinteger_or_char_p, Qinteger_char_or_marker_p;
Lisp_Object Qnumberp, Qnumber_char_or_marker_p;
Lisp_Object Qbit_vectorp, Qbitp, Qcdr;

Lisp_Object Qfloatp;

#ifdef DEBUG_SXEMACS

int debug_issue_ebola_notices;

Fixnum debug_ebola_backtrace_length;

int eq_with_ebola_notice(Lisp_Object obj1, Lisp_Object obj2)
{
	if (debug_issue_ebola_notices
	    && ((CHARP(obj1) && INTP(obj2)) || (CHARP(obj2) && INTP(obj1)))) {
		/* #### It would be really nice if this were a proper warning
		   instead of brain-dead print to Qexternal_debugging_output.  */
		write_c_string
		    ("Comparison between integer and character is constant nil (",
		     Qexternal_debugging_output);
		Fprinc(obj1, Qexternal_debugging_output);
		write_c_string(" and ", Qexternal_debugging_output);
		Fprinc(obj2, Qexternal_debugging_output);
		write_c_string(")\n", Qexternal_debugging_output);
		debug_short_backtrace(debug_ebola_backtrace_length);
	}
	return EQ(obj1, obj2);
}

#endif				/* DEBUG_SXEMACS */

Lisp_Object wrong_type_argument(Lisp_Object predicate, Lisp_Object value)
{
	/* This function can GC */
	REGISTER Lisp_Object tem;
	do {
		value = Fsignal(Qwrong_type_argument, list2(predicate, value));
		tem = call1(predicate, value);
	}
	while (NILP(tem));
	return value;
}

DOESNT_RETURN dead_wrong_type_argument(Lisp_Object predicate, Lisp_Object value)
{
	signal_error(Qwrong_type_argument, list2(predicate, value));
}

DEFUN("wrong-type-argument", Fwrong_type_argument, 2, 2, 0,	/*
Signal an error until the correct type value is given by the user.
This function loops, signalling a continuable `wrong-type-argument' error
with PREDICATE and VALUE as the data associated with the error and then
calling PREDICATE on the returned value, until the value gotten satisfies
PREDICATE.  At that point, the gotten value is returned.
*/
      (predicate, value))
{
	return wrong_type_argument(predicate, value);
}

DOESNT_RETURN c_write_error(Lisp_Object obj)
{
	signal_simple_error("Attempt to modify read-only object (c)", obj);
}

DOESNT_RETURN lisp_write_error(Lisp_Object obj)
{
	signal_simple_error("Attempt to modify read-only object (lisp)", obj);
}

DOESNT_RETURN args_out_of_range(Lisp_Object a1, Lisp_Object a2)
{
	signal_error(Qargs_out_of_range, list2(a1, a2));
}

DOESNT_RETURN
args_out_of_range_3(Lisp_Object a1, Lisp_Object a2, Lisp_Object a3)
{
	signal_error(Qargs_out_of_range, list3(a1, a2, a3));
}

void check_int_range(EMACS_INT val, EMACS_INT min, EMACS_INT max)
{
	if (val < min || val > max)
		args_out_of_range_3(make_int(val), make_int(min),
				    make_int(max));
}

/* On some machines, XINT needs a temporary location.
   Here it is, in case it is needed.  */

EMACS_INT sign_extend_temp;

/* On a few machines, XINT can only be done by calling this.  */
/* SXEmacs:  only used by m/convex.h */
EMACS_INT sign_extend_lisp_int(EMACS_INT num);
EMACS_INT sign_extend_lisp_int(EMACS_INT num)
{
	if (num & (1L << (INT_VALBITS - 1)))
		return num | ((-1L) << INT_VALBITS);
	else
		return num & (EMACS_INT) ((1UL << INT_VALBITS) - 1);
}

/* Data type predicates */

DEFUN("eq", Feq, 2, 2, 0,	/*
Return t if the two args are the same Lisp object.
*/
      (object1, object2))
{
	return EQ_WITH_EBOLA_NOTICE(object1, object2) ? Qt : Qnil;
}

DEFUN("old-eq", Fold_eq, 2, 2, 0,	/*
Return t if the two args are (in most cases) the same Lisp object.

Special kludge: A character is considered `old-eq' to its equivalent integer
even though they are not the same object and are in fact of different
types.  This is ABSOLUTELY AND UTTERLY HORRENDOUS but is necessary to
preserve byte-code compatibility with v19.  This kludge is known as the
\"char-int confoundance disease\" and appears in a number of other
functions with `old-foo' equivalents.

Do not use this function!
*/
      (object1, object2))
{
	/* #### blasphemy */
	return HACKEQ_UNSAFE(object1, object2) ? Qt : Qnil;
}

DEFUN("null", Fnull, 1, 1, 0,	/*
Return t if OBJECT is nil.
*/
      (object))
{
	return NILP(object) ? Qt : Qnil;
}

DEFUN("consp", Fconsp, 1, 1, 0,	/*
Return t if OBJECT is a cons cell.  `nil' is not a cons cell.

A cons cell is a Lisp object (an area in memory) comprising two pointers
called the CAR and the CDR.  Each of these pointers can point to any other
Lisp object.  The common Lisp data type, the list, is a specially-structured
series of cons cells.

See the documentation for `cons' or the Lisp manual for more details on what
a cons cell is.
*/
      (object))
{
	return CONSP(object) ? Qt : Qnil;
}

DEFUN("atom", Fatom, 1, 1, 0,	/*
Return t if OBJECT is not a cons cell.  `nil' is not a cons cell.

A cons cell is a Lisp object (an area in memory) comprising two pointers
called the CAR and the CDR.  Each of these pointers can point to any other
Lisp object.  The common Lisp data type, the list, is a specially-structured
series of cons cells.

See the documentation for `cons' or the Lisp manual for more details on what
a cons cell is.
*/
      (object))
{
	return CONSP(object) ? Qnil : Qt;
}

DEFUN("listp", Flistp, 1, 1, 0,	/*
Return t if OBJECT is a list.  `nil' is a list.

A list is implemented as a series of cons cells structured such that the CDR
of each cell either points to another cons cell or to `nil', the special
Lisp value for both Boolean false and the empty list.
*/
      (object))
{
	return LISTP(object) ? Qt : Qnil;
}

DEFUN("nlistp", Fnlistp, 1, 1, 0,	/*
Return t if OBJECT is not a list.  `nil' is a list.

A list is implemented as a series of cons cells structured such that the CDR
of each cell either points to another cons cell or to `nil', the special
Lisp value for both Boolean false and the empty list.
*/
      (object))
{
	return LISTP(object) ? Qnil : Qt;
}

DEFUN("true-list-p", Ftrue_list_p, 1, 1, 0,	/*
Return t if OBJECT is an acyclic, nil-terminated (ie, not dotted), list.

A list is implemented as a series of cons cells structured such that the CDR
of each cell either points to another cons cell or to `nil', the special
Lisp value for both Boolean false and the empty list.
*/
      (object))
{
	return TRUE_LIST_P(object) ? Qt : Qnil;
}

DEFUN("symbolp", Fsymbolp, 1, 1, 0,	/*
Return t if OBJECT is a symbol.
*/
      (object))
{
	return SYMBOLP(object) ? Qt : Qnil;
}

DEFUN("keywordp", Fkeywordp, 1, 1, 0,	/*
Return t if OBJECT is a keyword.

A symbol is a Lisp object with a name. It can optionally have any and all of
a value, a property list and an associated function.
*/
      (object))
{
	return KEYWORDP(object) ? Qt : Qnil;
}

DEFUN("vectorp", Fvectorp, 1, 1, 0,	/*
Return t if OBJECT is a vector.
*/
      (object))
{
	return VECTORP(object) ? Qt : Qnil;
}

DEFUN("bit-vector-p", Fbit_vector_p, 1, 1, 0,	/*
Return t if OBJECT is a bit vector.
*/
      (object))
{
	return BIT_VECTORP(object) ? Qt : Qnil;
}

DEFUN("stringp", Fstringp, 1, 1, 0,	/*
Return t if OBJECT is a string.
*/
      (object))
{
	return STRINGP(object) ? Qt : Qnil;
}

DEFUN("arrayp", Farrayp, 1, 1, 0,	/*
Return t if OBJECT is an array (string, vector, or bit vector).
*/
      (object))
{
	return (VECTORP(object) || STRINGP(object) || BIT_VECTORP(object))
	    ? Qt : Qnil;
}

DEFUN("sequencep", Fsequencep, 1, 1, 0,	/*
Return t if OBJECT is a sequence (list, dllist or array).
*/
      (object))
{
	return (LISTP(object) || DLLISTP(object) ||
		VECTORP(object) || STRINGP(object) || BIT_VECTORP(object))
	    ? Qt : Qnil;
}

DEFUN("markerp", Fmarkerp, 1, 1, 0,	/*
Return t if OBJECT is a marker (editor pointer).
*/
      (object))
{
	return MARKERP(object) ? Qt : Qnil;
}

DEFUN("subrp", Fsubrp, 1, 1, 0,	/*
Return t if OBJECT is a built-in function.
*/
      (object))
{
	return SUBRP(object) ? Qt : Qnil;
}

DEFUN("subr-min-args", Fsubr_min_args, 1, 1, 0,	/*
Return minimum number of args built-in function SUBR may be called with.
*/
      (subr))
{
	CHECK_SUBR(subr);
	return make_int(XSUBR(subr)->min_args);
}

DEFUN("subr-max-args", Fsubr_max_args, 1, 1, 0,	/*
Return maximum number of args built-in function SUBR may be called with,
or nil if it takes an arbitrary number of arguments or is a special form.
*/
      (subr))
{
	int nargs;
	CHECK_SUBR(subr);
	nargs = XSUBR(subr)->max_args;
	if (nargs == MANY || nargs == UNEVALLED)
		return Qnil;
	else
		return make_int(nargs);
}

DEFUN("subr-interactive", Fsubr_interactive, 1, 1, 0,	/*
Return the interactive spec of the subr object SUBR, or nil.
If non-nil, the return value will be a list whose first element is
`interactive' and whose second element is the interactive spec.
*/
      (subr))
{
	const char *prompt;
	CHECK_SUBR(subr);
	prompt = XSUBR(subr)->prompt;
	return prompt ? list2(Qinteractive, build_string(prompt)) : Qnil;
}

DEFUN("characterp", Fcharacterp, 1, 1, 0,	/*
Return t if OBJECT is a character.
Unlike in XEmacs v19 and FSF Emacs, a character is its own primitive type.
Any character can be converted into an equivalent integer using
`char-int'.  To convert the other way, use `int-char'; however,
only some integers can be converted into characters.  Such an integer
is called a `char-int'; see `char-int-p'.

Some functions that work on integers (e.g. the comparison functions
<, <=, =, /=, etc. and the arithmetic functions +, -, *, etc.)
accept characters and implicitly convert them into integers.  In
general, functions that work on characters also accept char-ints and
implicitly convert them into characters.  WARNING: Neither of these
behaviors is very desirable, and they are maintained for backward
compatibility with old E-Lisp programs that confounded characters and
integers willy-nilly.  These behaviors may change in the future; therefore,
do not rely on them.  Instead, use the character-specific functions such
as `char='.
*/
      (object))
{
	return CHARP(object) ? Qt : Qnil;
}

DEFUN("char-to-int", Fchar_to_int, 1, 1, 0,	/*
Convert CHARACTER into an equivalent integer.
The resulting integer will always be non-negative.  The integers in
the range 0 - 255 map to characters as follows:

   0 - 31          Control set 0
  32 - 127        ASCII
 128 - 159       Control set 1
 160 - 255       Right half of ISO-8859-1

If support for Mule does not exist, these are the only valid character
values.  When Mule support exists, the values assigned to other characters
may vary depending on the particular version of SXEmacs, the order in which
character sets were loaded, etc., and you should not depend on them.
*/
      (character))
{
	CHECK_CHAR(character);
	return make_int(XCHAR(character));
}

DEFUN("int-to-char", Fint_to_char, 1, 1, 0,	/*
Convert integer INTEGER into the equivalent character.
Not all integers correspond to valid characters; use `char-int-p' to
determine whether this is the case.  If the integer cannot be converted,
nil is returned.
*/
      (integer))
{
	CHECK_INT(integer);
	if (CHAR_INTP(integer))
		return make_char(XINT(integer));
	else
		return Qnil;
}

DEFUN("char-int-p", Fchar_int_p, 1, 1, 0,	/*
Return t if OBJECT is an integer that can be converted into a character.
See `char-int'.
*/
      (object))
{
	return CHAR_INTP(object) ? Qt : Qnil;
}

DEFUN("char-or-char-int-p", Fchar_or_char_int_p, 1, 1, 0,	/*
Return t if OBJECT is a character or an integer that can be converted into one.
*/
      (object))
{
	return CHAR_OR_CHAR_INTP(object) ? Qt : Qnil;
}

DEFUN("char-or-string-p", Fchar_or_string_p, 1, 1, 0,	/*
Return t if OBJECT is a character (or a char-int) or a string.
It is semi-hateful that we allow a char-int here, as it goes against
the name of this function, but it makes the most sense considering the
other steps we take to maintain compatibility with the old character/integer
confoundedness in older versions of E-Lisp.
*/
      (object))
{
	return CHAR_OR_CHAR_INTP(object) || STRINGP(object) ? Qt : Qnil;
}

#ifdef WITH_NUMBER_TYPES
/* In this case, integerp is defined in number.c. */
DEFUN("intp", Fintp, 1, 1, 0, /*
Return t if OBJECT is an ordinary integer.
*/
      (object))
{
	return INTP(object) ? Qt : Qnil;
}
/* stay compatible to XE 21.5 */
DEFUN("fixnump", Ffixnump, 1, 1, 0, /*
Return t if OBJECT is an ordinary integer.
*/
      (object))
{
	return INTP(object) ? Qt : Qnil;
}
#else  /* !WITH_NUMBER_TYPES */
DEFUN("integerp", Fintegerp, 1, 1, 0,	/*
Return t if OBJECT is an integer.
*/
      (object))
{
	return INTP(object) ? Qt : Qnil;
}
#endif	/* WITH_NUMBER_TYPES */

DEFUN("integer-or-marker-p", Finteger_or_marker_p, 1, 1, 0,	/*
Return t if OBJECT is an integer or a marker (editor pointer).
*/
      (object))
{
	return INTP(object) || MARKERP(object) ? Qt : Qnil;
}

DEFUN("integer-or-char-p", Finteger_or_char_p, 1, 1, 0,	/*
Return t if OBJECT is an integer or a character.
*/
      (object))
{
	return INTP(object) || CHARP(object) ? Qt : Qnil;
}

DEFUN("integer-char-or-marker-p", Finteger_char_or_marker_p, 1, 1, 0,	/*
Return t if OBJECT is an integer, character or a marker (editor pointer).
*/
      (object))
{
	return INTP(object) || CHARP(object) || MARKERP(object) ? Qt : Qnil;
}

DEFUN("natnump", Fnatnump, 1, 1, 0,	/*
Return t if OBJECT is a nonnegative integer.
*/
      (object))
{
	return (NATNUMP(object)
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		|| (BIGZP(object) &&
		    bigz_sign(XBIGZ_DATA(object)) >= 0)
#endif
		) ? Qt : Qnil;
}

DEFUN ("nonnegativep", Fnonnegativep, 1, 1, 0, /*
Return t if OBJECT is a nonnegative number.

We call a number object non-negative iff it is comparable
and its value is not less than 0.
*/
       (object))
{
	return NATNUMP(object)
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		|| (BIGZP(object) &&
		    bigz_sign(XBIGZ_DATA(object)) >= 0)
#endif	/* HAVE_MPZ */
#if defined HAVE_MPQ && defined WITH_GMP
		|| (BIGQP(object) &&
		    bigq_sign(XBIGQ_DATA(object)) >= 0)
#endif	/* HAVE_MPQ */
#ifdef HAVE_FPFLOAT
		|| (FLOATP(object) &&
		    (double)XFLOAT_DATA(object) >= 0.0)
#endif	/* HAVE_FPFLOAT */
#if defined HAVE_MPF && defined WITH_GMP
		|| (BIGFP(object) &&
		    bigf_sign(XBIGF_DATA(object)) >= 0)
#endif	/* HAVE_MPF */
#if defined HAVE_MPFR && defined WITH_MPFR
		|| (BIGFRP(object) &&
		    bigfr_sign(XBIGFR_DATA(object)) >= 0)
#endif	/* HAVE_MPFR */
		? Qt : Qnil;
}

DEFUN("bitp", Fbitp, 1, 1, 0,	/*
Return t if OBJECT is a bit (0 or 1).
*/
      (object))
{
	return BITP(object) ? Qt : Qnil;
}

DEFUN("numberp", Fnumberp, 1, 1, 0,	/*
Return t if OBJECT is a number (floating point or integer).
*/
      (object))
{
#if defined(WITH_NUMBER_TYPES)
	return NUMBERP(object) ? Qt : Qnil;
#else
	return INT_OR_FLOATP(object) ? Qt : Qnil;
#endif
}

DEFUN("number-or-marker-p", Fnumber_or_marker_p, 1, 1, 0,	/*
Return t if OBJECT is a number or a marker.
*/
      (object))
{
	return INT_OR_FLOATP(object) || MARKERP(object) ? Qt : Qnil;
}

DEFUN("number-char-or-marker-p", Fnumber_char_or_marker_p, 1, 1, 0,	/*
Return t if OBJECT is a number, character or a marker.
*/
      (object))
{
	return (INT_OR_FLOATP(object) || CHARP(object) || MARKERP(object))
	    ? Qt : Qnil;
}

#ifdef HAVE_FPFLOAT
DEFUN("floatp", Ffloatp, 1, 1, 0,	/*
Return t if OBJECT is a floating point number.
*/
      (object))
{
	return FLOATP(object) ? Qt : Qnil;
}
#endif				/* HAVE_FPFLOAT */

DEFUN("type-of", Ftype_of, 1, 1, 0,	/*
Return a symbol representing the type of OBJECT.
*/
      (object))
{
	switch (XTYPE(object)) {
	case Lisp_Type_Record:
		if (XRECORD_LHEADER_IMPLEMENTATION(object)->
		    lrecord_type_index != lrecord_type_dynacat)
			return intern(
				XRECORD_LHEADER_IMPLEMENTATION(object)->name);
		else if (SYMBOLP(XDYNACAT_TYPE(object)))
			return XDYNACAT_TYPE(object);
		else
			return Qundecided;

	case Lisp_Type_Char:
		return Qcharacter;

		/* list all cases */
	case Lisp_Type_Int_Even:
	case Lisp_Type_Int_Odd:
	default:
		return Qinteger;
	}
}

/* Extract and set components of lists */

DEFUN("car", Fcar, 1, 1, 0,	/*
Return the car of CONS.  If CONS is nil, return nil.

The car of a list or a dotted pair is its first element.
Error if CONS is not nil and not a cons cell.  See also `car-safe'.
*/
      (cons))
{
	while (1) {
		if (CONSP(cons))
			return XCAR(cons);
		else if (NILP(cons))
			return Qnil;
		else
			cons = wrong_type_argument(Qlistp, cons);
	}
}

DEFUN("car-safe", Fcar_safe, 1, 1, 0,	/*
Return the car of OBJECT if it is a cons cell, or else nil.

The car of a list or a dotted pair is its first element.
*/
      (object))
{
	return CONSP(object) ? XCAR(object) : Qnil;
}

DEFUN("cdr", Fcdr, 1, 1, 0,	/*
Return the cdr of CONS.  If CONS is nil, return nil.

The cdr of a list is the list without its first element.  The cdr of a
dotted pair (A . B) is the second element, B.

Error if arg is not nil and not a cons cell.  See also `cdr-safe'.
*/
      (cons))
{
	while (1) {
		if (CONSP(cons))
			return XCDR(cons);
		else if (NILP(cons))
			return Qnil;
		else
			cons = wrong_type_argument(Qlistp, cons);
	}
}

DEFUN("cdr-safe", Fcdr_safe, 1, 1, 0,	/*
Return the cdr of OBJECT if it is a cons cell, else nil.

The cdr of a list is the list without its first element.  The cdr of a
dotted pair (A . B) is the second element, B.
*/
      (object))
{
	return CONSP(object) ? XCDR(object) : Qnil;
}

DEFUN("setcar", Fsetcar, 2, 2, 0,	/*
Set the car of CONS-CELL to be NEWCAR.  Return NEWCAR.

The car of a list or a dotted pair is its first element.
*/
      (cons_cell, newcar))
{
	if (!CONSP(cons_cell))
		cons_cell = wrong_type_argument(Qconsp, cons_cell);

	XCAR(cons_cell) = newcar;
	return newcar;
}

DEFUN("setcdr", Fsetcdr, 2, 2, 0,	/*
Set the cdr of CONS-CELL to be NEWCDR.  Return NEWCDR.

The cdr of a list is the list without its first element.  The cdr of a
dotted pair (A . B) is the second element, B.
*/
      (cons_cell, newcdr))
{
	if (!CONSP(cons_cell))
		cons_cell = wrong_type_argument(Qconsp, cons_cell);

	XCDR(cons_cell) = newcdr;
	return newcdr;
}

/* Find the function at the end of a chain of symbol function indirections.

   If OBJECT is a symbol, find the end of its function chain and
   return the value found there.  If OBJECT is not a symbol, just
   return it.  If there is a cycle in the function chain, signal a
   cyclic-function-indirection error.

   This is like Findirect_function when VOID_FUNCTION_ERRORP is true.
   When VOID_FUNCTION_ERRORP is false, no error is signaled if the end
   of the chain ends up being Qunbound. */
Lisp_Object indirect_function(Lisp_Object object, int void_function_errorp)
{
#define FUNCTION_INDIRECTION_SUSPICION_LENGTH 16
	Lisp_Object tortoise, hare;
	int count;

	for (hare = tortoise = object, count = 0;
	     SYMBOLP(hare); hare = XSYMBOL(hare)->function, count++) {
		if (count < FUNCTION_INDIRECTION_SUSPICION_LENGTH)
			continue;

		if (count & 1)
			tortoise = XSYMBOL(tortoise)->function;
		if (EQ(hare, tortoise))
			return Fsignal(Qcyclic_function_indirection,
				       list1(object));
	}

	if (void_function_errorp && UNBOUNDP(hare))
		return signal_void_function_error(object);

	return hare;
}

DEFUN("indirect-function", Findirect_function, 1, 1, 0,	/*
Return the function at the end of OBJECT's function chain.
If OBJECT is a symbol, follow all function indirections and return
the final function binding.
If OBJECT is not a symbol, just return it.
Signal a void-function error if the final symbol is unbound.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.
*/
      (object))
{
	return indirect_function(object, 1);
}

/* Extract and set vector and string elements */

DEFUN("aref", Faref, 2, 2, 0,	/*
Return the element of ARRAY at index INDEX.
ARRAY may be a vector, bit vector, or string.  INDEX starts at 0.
*/
      (array, index_))
{
	EMACS_INT idx;
	EMACS_INT alen;

retry:
	/* frob the position INDEX */
	if (INTP(index_))
		idx = XINT(index_);
	else if (CHARP(index_))
		idx = XCHAR(index_);	/* yuck! */
	else {
		index_ = wrong_type_argument(Qinteger_or_char_p, index_);
		goto retry;
	}

	/* frob the length of ARRAY */
	if (VECTORP(array))
		alen = XVECTOR_LENGTH(array);
	else if (BIT_VECTORP(array))
		alen = bit_vector_length(XBIT_VECTOR(array));
	else if (STRINGP(array))
		alen = XSTRING_CHAR_LENGTH(array);
	else
		alen = 0;

	if (idx < 0 || idx >= alen)
		goto range_error;

	if (VECTORP(array))
		return XVECTOR_DATA(array)[idx];
	else if (BIT_VECTORP(array))
		return make_int(bit_vector_bit(XBIT_VECTOR(array), idx));
	else if (STRINGP(array))
		return make_char(string_char(XSTRING(array), idx));
#ifdef LOSING_BYTECODE
	else if (COMPILED_FUNCTIONP(array)) {
		/* Weird, gross compatibility kludge */
		return Felt(array, index_);
	}
#endif
	else {
		check_losing_bytecode("aref", array);
		array = wrong_type_argument(Qarrayp, array);
		goto retry;
	}

range_error:
	args_out_of_range(array, index_);
	return Qnil;		/* not reached */
}

DEFUN("aset", Faset, 3, 3, 0,	/*
Store into the element of ARRAY at index INDEX the value NEWVAL.
ARRAY may be a vector, bit vector, or string.  INDEX starts at 0.
*/
      (array, index_, newval))
{
	EMACS_INT idx;
	EMACS_INT alen;

retry:
	/* frob the INDEX position */
	if (INTP(index_))
		idx = XINT(index_);
	else if (CHARP(index_))
		idx = XCHAR(index_);	/* yuck! */
	else {
		index_ = wrong_type_argument(Qinteger_or_char_p, index_);
		goto retry;
	}

	/* frob the length of ARRAY */
	if (VECTORP(array))
		alen = XVECTOR_LENGTH(array);
	else if (BIT_VECTORP(array))
		alen = bit_vector_length(XBIT_VECTOR(array));
	else if (STRINGP(array))
		alen = XSTRING_CHAR_LENGTH(array);
	else
		alen = 0;

	if (idx < 0 || idx >= alen)
		goto range_error;

	if (VECTORP(array)) {
		XVECTOR_DATA(array)[idx] = newval;
	} else if (BIT_VECTORP(array)) {
		CHECK_BIT(newval);
		set_bit_vector_bit(XBIT_VECTOR(array), idx, !ZEROP(newval));
	} else if (STRINGP(array)) {
		CHECK_CHAR_COERCE_INT(newval);
		set_string_char(XSTRING(array), idx, XCHAR(newval));
		bump_string_modiff(array);
	} else {
		array = wrong_type_argument(Qarrayp, array);
		goto retry;
	}

	return newval;

range_error:
	args_out_of_range(array, index_);
	return Qnil;		/* not reached */
}

/**********************************************************************/
/*                       Arithmetic functions                         */
/**********************************************************************/
typedef struct {
	int int_p;
	union {
		EMACS_INT ival;
		double dval;
	} c;
} int_or_double;

#ifndef WITH_NUMBER_TYPES
static void
number_char_or_marker_to_int_or_double(Lisp_Object obj, int_or_double * p)
{
      retry:
	p->int_p = 1;
	if (INTP(obj))
		p->c.ival = XINT(obj);
	else if (CHARP(obj))
		p->c.ival = XCHAR(obj);
	else if (MARKERP(obj))
		p->c.ival = marker_position(obj);
#ifdef HAVE_FPFLOAT
	else if (FLOATP(obj))
		p->c.dval = XFLOAT_DATA(obj), p->int_p = 0;
#endif
	else {
		obj = wrong_type_argument(Qnumber_char_or_marker_p, obj);
		goto retry;
	}
}

static double number_char_or_marker_to_double(Lisp_Object obj)
{
      retry:
	if (INTP(obj))
		return (double)XINT(obj);
	else if (CHARP(obj))
		return (double)XCHAR(obj);
	else if (MARKERP(obj))
		return (double)marker_position(obj);
#ifdef HAVE_FPFLOAT
	else if (FLOATP(obj))
		return XFLOAT_DATA(obj);
#endif
	else {
		obj = wrong_type_argument(Qnumber_char_or_marker_p, obj);
		goto retry;
	}
}
#endif

static EMACS_INT integer_char_or_marker_to_int(Lisp_Object obj)
{
      retry:
	if (INTP(obj))
		return XINT(obj);
	else if (CHARP(obj))
		return XCHAR(obj);
	else if (MARKERP(obj))
		return marker_position(obj);
	else {
		obj = wrong_type_argument(Qinteger_char_or_marker_p, obj);
		goto retry;
	}
}


/* Convert between a 32-bit value and a cons of two 16-bit values.
   This is used to pass 32-bit integers to and from the user.
   Use time_to_lisp() and lisp_to_time() for time values.

   If you're thinking of using this to store a pointer into a Lisp Object
   for internal purposes (such as when calling record_unwind_protect()),
   try using make_opaque_ptr()/get_opaque_ptr() instead. */
Lisp_Object word_to_lisp(unsigned int item)
{
	return Fcons(make_int(item >> 16), make_int(item & 0xffff));
}

unsigned int lisp_to_word(Lisp_Object item)
{
	if (INTP(item))
		return XINT(item);
	else {
		Lisp_Object top = Fcar(item);
		Lisp_Object bot = Fcdr(item);
		CHECK_INT(top);
		CHECK_INT(bot);
		return (XINT(top) << 16) | (XINT(bot) & 0xffff);
	}
}

DEFUN("number-to-string", Fnumber_to_string, 1, 1, 0,	/*
Convert NUMBER to a string by printing it in decimal.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.
*/
      (number))
{
	char buffer[VALBITS];

#ifdef WITH_NUMBER_TYPES
	CHECK_NUMBER(number);
#else
	CHECK_INT_OR_FLOAT(number);
#endif

#ifdef HAVE_FPFLOAT
	if (FLOATP(number)) {
		char pigbuf[350];	/* see comments in float_to_string */

		float_to_string(pigbuf, XFLOAT_DATA(number), sizeof(pigbuf));
		return build_string(pigbuf);
	}
#endif  /* HAVE_FPFLOAT */
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (BIGZP(number)) {
		char *str = bigz_to_string(XBIGZ_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_MPZ */
#if defined HAVE_MPQ && defined WITH_GMP
	if (BIGQP(number)) {
		char *str = (char *)bigq_to_string(XBIGQ_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_MPQ */
#if defined HAVE_MPF && defined WITH_GMP
	if (BIGFP(number)) {
		char *str = (char *)bigf_to_string(XBIGF_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_MPF */
#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number)) {
		char *str = (char*)bigfr_to_string(XBIGFR_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_MPFR */
#if defined HAVE_PSEUG && defined WITH_PSEUG
	if (BIGGP(number)) {
		char *str = (char *)bigg_to_string(XBIGG_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_PSEUG */
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	if (BIGCP(number)) {
		char *str = (char *)bigc_to_string(XBIGC_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_MPC */
#if defined HAVE_QUATERN && defined WITH_QUATERN
	if (QUATERNP(number)) {
		char *str = (char*)quatern_to_string(XQUATERN_DATA(number), 10);
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}
#endif	/* HAVE_QUATERN */
	if (INDEFP(number)) {
		char *str = (char *)indef_to_string(XINDEF_DATA(number));
		Lisp_Object retval = build_string(str);
		xfree(str);
		return retval;
	}

	long_to_string(buffer, XINT(number), sizeof(buffer));
	return build_string(buffer);
}

#if !defined HAVE_MPZ || !(defined WITH_GMP || defined WITH_MP)
static int digit_to_number(int character, int base)
{
	/* Assumes ASCII */
	int digit = ((character >= '0' && character <= '9') ? character - '0' :
		     (character >= 'a'
		      && character <=
		      'z') ? character - 'a' + 10 : (character >= 'A'
						     && character <=
						     'Z') ? character - 'A' +
		     10 : -1);

	return digit >= base ? -1 : digit;
}
#endif	/* HAVE_MPZ */

DEFUN("string-to-number", Fstring_to_number, 1, 2, 0,	/*
Convert STRING to a number by parsing it as a number in base BASE.
This parses both integers and floating point numbers.
It ignores leading spaces and tabs.

If BASE is nil or omitted, base 10 is used.
BASE must be an integer between 2 and 16 (inclusive).
Floating point numbers always use base 10.

If STRING is a float, the variable `read-real-as' decides how to
interpret that float.
*/
      (string, base))
{
	char *p;
	int b;

	CHECK_STRING(string);

	if (NILP(base))
		b = 10;
	else {
		CHECK_INT(base);
		b = XINT(base);
		check_int_range(b, 2, 16);
	}

	p = (char *)XSTRING_DATA(string);

	/* Skip any whitespace at the front of the number.  Some versions of
	   atoi do this anyway, so we might as well make Emacs lisp consistent.  */
	while (*p == ' ' || *p == '\t')
		p++;

#if defined HAVE_PSEUG && defined WITH_PSEUG
	if (isgaussian_string(p))
		return read_bigg_string(p);
#endif	/* HAVE_PSEUG */

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	if (isbigc_string(p))
		return read_bigc_string(p);
#endif	/* HAVE_MPC */

#if defined HAVE_MPFR && defined WITH_MPFR
	if (isfloat_string(p) && b == 10) {
		if (!(default_real_precision) || Vread_real_as != Qbigfr)
			return make_float(str_to_fpfloat((const char*)p));
		else
			return read_bigfr_string(p);
	}
#elif defined HAVE_MPF && defined WITH_GMP
	if (isfloat_string(p) && b == 10) {
		if (!(default_real_precision) || Vread_real_as != Qbigf)
			return make_float(str_to_fpfloat((const char*)p));
		else
			return read_bigf_string(p);
	}
#elif defined HAVE_FPFLOAT
	if (isfloat_string(p) && b == 10)
		return make_float(str_to_fpfloat(p));
#endif	/* HAVE_MPFR || HAVE_MPFR || HAVE_FPFLOAT */

	if (ase_resc_elm_pred_f && ase_resc_elm_f &&
	    ase_resc_elm_pred_f(p))
		return ase_resc_elm_f(p);

#if defined HAVE_QUATERN && defined WITH_QUATERN
	if (isquatern_string(p))
		return read_quatern_string(p);
#endif	/* HAVE_QUATERN */

#if defined HAVE_MPQ && defined WITH_GMP
	if (strchr (p, '/') != NULL) {
#if 0
		return read_bigq_string(p);
#else
		/* do we even need fractions in different bases? */
		Bufbyte *end, save;
		bigq bq;
		Lisp_Object result;

		if (*p == '+')
			p++;

		end = (Bufbyte*)p;
		if (*end == '-')
			end++;
		while ((*end >= '0' && *end <= '9') ||
		       (b > 10 && *end >= 'a' && *end <= 'a' + b - 11) ||
		       (b > 10 && *end >= 'A' && *end <= 'A' + b - 11))
			end++;
		if (*end == '/') {
			end++;
			if (*end == '-')
				end++;
			while ((*end >= '0' && *end <= '9') ||
			       (b > 10 && *end >= 'a' &&
				*end <= 'a' + b - 11) ||
			       (b > 10 && *end >= 'A' &&
				*end <= 'A' + b - 11))
				end++;
		}
		save = *end;
		*end = '\0';

		bigq_init(bq);

		bigq_set_string(bq, (const char *) p, b);
		*end = save;
		bigq_canonicalize(bq);

		result = make_bigq_bq(bq);

		bigq_fini(bq);
		return result;
#endif	/* 1 */
	}
#endif /* HAVE_MPQ */

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	{
		Bufbyte *end, save;
		Lisp_Object retval;

		if (*p == '+')
			p++;
		end = (Bufbyte*)p;
		if (*end == '-')
			end++;
		while ((*end >= '0' && *end <= '9') ||
		       (b > 10 && *end >= 'a' && *end <= 'a' + b - 11) ||
		       (b > 10 && *end >= 'A' && *end <= 'A' + b - 11))
			end++;
		save = *end;
		*end = '\0';
		if (*p == '\0')
			retval = make_int(0);
		else {
			bigz bz;
			bigz_init(bz);
			bigz_set_string(bz, (const char *)p, b);
			retval = ent_mpz_downgrade_maybe(bz);
			bigz_fini(bz);
		}
		*end = save;
		return retval;
	}

#else  /* !HAVE_MPZ */

	if (b == 10) {
		/* Use the system-provided functions for base 10. */
#if   SIZEOF_EMACS_INT == SIZEOF_INT
		return make_int(atoi(p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG
		return make_int(atol(p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG_LONG_INT
		return make_int(atoll(p));
#endif
	} else {
		int negative = 1;
		EMACS_INT v = 0;

		if (*p == '-') {
			negative = -1;
			p++;
		} else if (*p == '+')
			p++;
		while (1) {
			int digit = digit_to_number(*p++, b);
			if (digit < 0)
				break;
			v = v * b + digit;
		}
		return make_int(negative * v);
	}
#endif /* HAVE_MPZ */
}


DEFUN("logand", Flogand, 0, MANY, 0,	/*
Return bitwise-and of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
*/
      (int nargs, Lisp_Object * args))
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	REGISTER int i;
	Lisp_Object result, other;
	ase_object_type_t nt1, nt2;

	if (nargs == 0)
		return make_int(~0);

	result = args[0];
	if (!(INTEGERP(result) || CHARP(result) || MARKERP(result)))
		result = wrong_type_argument(Qintegerp, result);

	if (nargs == 1)
		return make_int(ent_int(result));

	for (i = 1; i < nargs; i++) {
		other = args[i];
		if (!(INTEGERP(other) || CHARP(other) || MARKERP(other)))
			other = wrong_type_argument(Qintegerp, other);

		nt1 = ase_optable_index(result);
		nt2 = ase_optable_index(other);

		if (nt1 == INT_T && nt2 == INT_T) {
			result = make_int(ent_int(result) & ent_int(other));
		} else if (nt1 == INT_T && nt2 == BIGZ_T) {
			bigz_set_long(ent_scratch_bigz, ent_int(result));
			bigz_and(ent_scratch_bigz,
				 ent_scratch_bigz,
				 XBIGZ_DATA(other));
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		} else if (nt1 == BIGZ_T && nt2 == INT_T) {
			bigz_set_long(ent_scratch_bigz, ent_int(other));
			bigz_and(ent_scratch_bigz,
				 XBIGZ_DATA(result),
				 ent_scratch_bigz);
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		} else if (nt1 == BIGZ_T && nt2 == BIGZ_T) {
			bigz_and(ent_scratch_bigz,
				 XBIGZ_DATA(result),
				 XBIGZ_DATA(other));
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		}
	}
	return result;

#else /* !HAVE_MPZ */
	EMACS_INT bits = ~0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end)
		bits &= integer_char_or_marker_to_int(*args++);

	return make_int(bits);
#endif	/* HAVE_MPZ */
}

DEFUN("logior", Flogior, 0, MANY, 0,	/*
Return bitwise-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
*/
      (int nargs, Lisp_Object * args))
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	REGISTER int i;
	Lisp_Object result, other;
	ase_object_type_t nt1, nt2;

	if (nargs == 0)
		return make_int(0);

	result = args[0];
	if (!(INTEGERP(result) || CHARP(result) || MARKERP(result)))
		result = wrong_type_argument(Qintegerp, result);

	if (nargs == 1)
		return make_int(ent_int(result));

	for (i = 1; i < nargs; i++) {
		other = args[i];
		if (!(INTEGERP(other) || CHARP(other) || MARKERP(other)))
			other = wrong_type_argument(Qintegerp, other);

		nt1 = ase_optable_index(result);
		nt2 = ase_optable_index(other);

		if (nt1 == INT_T && nt2 == INT_T) {
			result = make_int(ent_int(result) | ent_int(other));
		} else if (nt1 == INT_T && nt2 == BIGZ_T) {
			bigz_set_long(ent_scratch_bigz, ent_int(result));
			bigz_ior(ent_scratch_bigz,
				 ent_scratch_bigz,
				 XBIGZ_DATA(other));
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		} else if (nt1 == BIGZ_T && nt2 == INT_T) {
			bigz_set_long(ent_scratch_bigz, ent_int(other));
			bigz_ior(ent_scratch_bigz,
				 XBIGZ_DATA(result),
				 ent_scratch_bigz);
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		} else if (nt1 == BIGZ_T && nt2 == BIGZ_T) {
			bigz_ior(ent_scratch_bigz,
				 XBIGZ_DATA(result),
				 XBIGZ_DATA(other));
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		}
	}
	return result;

#else /* !HAVE_MPZ */

	EMACS_INT bits = 0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end)
		bits |= integer_char_or_marker_to_int(*args++);

	return make_int(bits);
#endif	/* HAVE_MPZ */
}

DEFUN("logxor", Flogxor, 0, MANY, 0,	/*
Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
*/
      (int nargs, Lisp_Object * args))
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	REGISTER int i;
	Lisp_Object result, other;
	ase_object_type_t nt1, nt2;

	if (nargs == 0)
		return make_int(0);

	result = args[0];
	if (!(INTEGERP(result) || CHARP(result) || MARKERP(result)))
		result = wrong_type_argument(Qintegerp, result);

	if (nargs == 1)
		return make_int(ent_int(result));

	for (i = 1; i < nargs; i++) {
		other = args[i];
		if (!(INTEGERP(other) || CHARP(other) || MARKERP(other)))
			other = wrong_type_argument(Qintegerp, other);

		nt1 = ase_optable_index(result);
		nt2 = ase_optable_index(other);

		if (nt1 == INT_T && nt2 == INT_T) {
			result = make_int(ent_int(result) ^ ent_int(other));
		} else if (nt1 == INT_T && nt2 == BIGZ_T) {
			bigz_set_long(ent_scratch_bigz, ent_int(result));
			bigz_xor(ent_scratch_bigz,
				 ent_scratch_bigz,
				 XBIGZ_DATA(other));
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		} else if (nt1 == BIGZ_T && nt2 == INT_T) {
			bigz_set_long(ent_scratch_bigz, ent_int(other));
			bigz_xor(ent_scratch_bigz,
				 XBIGZ_DATA(result),
				 ent_scratch_bigz);
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		} else if (nt1 == BIGZ_T && nt2 == BIGZ_T) {
			bigz_xor(ent_scratch_bigz,
				 XBIGZ_DATA(result),
				 XBIGZ_DATA(other));
			result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		}
	}
	return result;

#else  /* !HAVE_MPZ */

	EMACS_INT bits = 0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end)
		bits ^= integer_char_or_marker_to_int(*args++);

	return make_int(bits);
#endif	/* HAVE_MPZ */
}

DEFUN("lognot", Flognot, 1, 1, 0,	/*
Return the bitwise complement of NUMBER.
NUMBER may be an integer, marker or character converted to integer.
*/
      (number))
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (BIGZP(number)) {
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);

		bigz_not(bz, XBIGZ_DATA(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
	} else {
		return make_int(~integer_char_or_marker_to_int(number));
	}
#else  /* HAVE_MPZ */
	return make_int(~integer_char_or_marker_to_int(number));
#endif	/* HAVE_MPZ */
}

/* Note, ANSI *requires* the presence of the fmod() library routine.
   If your system doesn't have it, complain to your vendor, because
   that is a bug. */

#ifndef HAVE_FMOD
double fmod(double f1, double f2)
{
	if (f2 < 0.0)
		f2 = -f2;
	return f1 - f2 * floor(f1 / f2);
}
#endif				/* ! HAVE_FMOD */

DEFUN("ash", Fash, 2, 2, 0,	/*
Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, the sign bit is duplicated.
*/
      (value, count))
{
	CHECK_INT_COERCE_CHAR(value);
	CONCHECK_INT(count);

	return make_int(XINT(count) > 0 ?
			XINT(value) << XINT(count) :
			XINT(value) >> -XINT(count));
}

DEFUN("lsh", Flsh, 2, 2, 0,	/*
Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, zeros are shifted in on the left.
*/
      (value, count))
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	Lisp_Object result;

	result = Qzero;
	value = Fcoerce_number(value, Qinteger, Qnil);
	CONCHECK_INT(count);

	switch (ase_optable_index(value)) {
	case INT_T:
		if (XREALINT(count) <= 0)
			return make_int(XREALINT(value) >> -XREALINT(count));
		/* Use bignums to avoid overflow */
		bigz_set_long(ent_scratch_bigz, XREALINT(value));
		bigz_lshift(ent_scratch_bigz,
			    ent_scratch_bigz, XREALINT(count));

		result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		break;
	case BIGZ_T:
		if (XREALINT(count) <= 0) {
			bigz_rshift(ent_scratch_bigz,
				    XBIGZ_DATA(value),
				    -XREALINT(count));
		} else {
			bigz_lshift(ent_scratch_bigz,
				    XBIGZ_DATA(value),
				    XREALINT(count));
		}
		result = ent_mpz_downgrade_maybe(ent_scratch_bigz);
		break;
	case INDEF_T:
		if (COMPARABLE_INDEF_P(value))
			result = value;
		else if (COMPARABLE_INDEF_P(count) &&
			 XINDEF_DATA(count) == POS_INFINITY)
			result = make_indef(POS_INFINITY);
		else if (COMPARABLE_INDEF_P(count) &&
			 XINDEF_DATA(count) == NEG_INFINITY)
			result = Qzero;
		break;
	default:
		result = Qzero;
	}

	return result;

#else  /* HAVE_MPZ */
	CHECK_INT_COERCE_CHAR(value);
	CONCHECK_INT(count);

	return make_int(XINT(count) > 0 ?
			XUINT(value) << XINT(count) :
			XUINT(value) >> -XINT(count));
#endif	/* HAVE_MPZ */
}

/* Number theoretic functions */

#if defined WITH_GMP && defined HAVE_MPZ

/* why do we put this cruft here, actually? Is not it better to have a separate
 * number-fns.c or the like?
 */

DEFUN("primep", Fprimep, 1, 2, 0,	/*
Return `nil' if NUMBER is known to be composite, return `t' if
NUMBER is definitely prime and return 'probably-prime if
NUMBER seems to be prime but it is not certain.

If optional argument CERTAINTY-THRESHOLD is non-nil, it should be a
natural number to indicate how many probabilistic primality tests must
be passed in order to have certainty about the primality of NUMBER.
The default is 8.
*/
      (number, certainty_threshold))
{
	Lisp_Object bznumber;
	int result;

	if (INDEFP(number))
		return Qnil;

	bznumber = Fcoerce_number(number, Qbigz, Qnil);
	if (NILP(certainty_threshold))
		result = mpz_probab_prime_p(XBIGZ_DATA(bznumber), 8);
	else if (NATNUMP(certainty_threshold))
		result = mpz_probab_prime_p(XBIGZ_DATA(bznumber),
					    XINT(certainty_threshold));
	else
		result = wrong_type_argument(Qnatnump, certainty_threshold);

	if (result == 0)
		return Qnil;
	else if (result == 1)
		return intern("probably-prime");
	else if (result == 2)
		return Qt;
	else
		return intern("unknown-test-result");
}

DEFUN("next-prime", Fnext_prime, 1, 1, 0,	/*
Return the next prime number greater than NUMBER.
*/
      (number))
{
	Lisp_Object bznumber;

	if (INDEFP(number)) {
		return number;
	}

	bznumber = Fcoerce_number(number, Qbigz, Qnil);
	mpz_nextprime(ent_scratch_bigz, XBIGZ_DATA(bznumber));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}


DEFUN("factorial", Ffactorial, 1, 1, 0,	/*
Return the factorial of NUMBER.
*/
      (number))
{
	bigz bz;
	Lisp_Object result;

	if (INDEFP(number) &&
	    XINDEF_DATA(number) == POS_INFINITY)
		return number;

	if (!INTP(number)) {
		number = wrong_type_argument(Qintegerp, number);
		return Qzero;
	}
	if (!NATNUMP(number)) {
		number = wrong_type_argument(Qnatnump, number);
		return Qzero;
	}

	bigz_init(bz);

	mpz_fac_ui(bz, XUINT(number));
	result = make_bigz_bz(bz);

	bigz_fini(bz);
	return result;
}

DEFUN("binomial-coefficient", Fbinomial_coefficient, 2, 2, 0, /*
Return the binomial coefficient, N over K.
*/
      (n, k))
{
	bigz bz;
	unsigned long kui;
	Lisp_Object result;

	CHECK_INTEGER(n);
	CHECK_INTEGER(k);

	if (NILP(Fnonnegativep(k)))
		return wrong_type_argument(Qnonnegativep, k);
	else if (INTP(k))
		kui = XINT(k);
	else if (BIGZP(k))
		kui = bigz_to_ulong(XBIGZ_DATA(k));
	else
		return wrong_type_argument(Qintegerp, k);

	n = Fcoerce_number(n, Qbigz, Qnil);

	bigz_init(bz);
	mpz_bin_ui(bz, XBIGZ_DATA(n), kui);
	result = make_bigz_bz(bz);

	bigz_fini(bz);
	return result;
}

DEFUN("remove-factor", Fremove_factor, 2, 2, 0,	/*
Remove all occurences of FACTOR in NUMBER and return a cons cell
with NUMBER divided by a maximal power of FACTOR in the car and
the exponent in the cdr.
FACTOR must be non-negative and greater than 1.
*/
      (factor, number))
{
	Lisp_Object bznumber, bzfactor;
	bigz bz;
	Lisp_Object result;
	unsigned long occur;

	if (INDEFP(factor) && INDEFP(number)) {
		if (XINDEF_DATA(factor) == POS_INFINITY)
			return Fcons(factor, factor);
		else
			return wrong_type_argument(Qnonnegativep, factor);
	}
	if (INDEFP(factor)) {
		if (XINDEF_DATA(factor) == POS_INFINITY)
			return Fcons(number, Qzero);
		else
			return wrong_type_argument(Qnonnegativep, factor);
	}
	if (INDEFP(number)) {
		if (INFINITYP(number))
			return Fcons(number, make_indef(POS_INFINITY));
		else
			return wrong_type_argument(Qnumberp, number);
	}

	bigz_init(bz);

	bznumber = Fcoerce_number(number, Qbigz, Qnil);
	bzfactor = Fcoerce_number(factor, Qbigz, Qnil);

	bigz_set_long(bz, 1L);
	if (bigz_eql(XBIGZ_DATA(bzfactor), bz)) {
		/* factor is one, which is always in a prime decomposition */
		bigz_fini(bz);
		return Fcons(bznumber, make_indef(POS_INFINITY));
	}
	bigz_set_long(bz, -1L);
	if (bigz_eql(XBIGZ_DATA(bzfactor), bz)) {
		/* factor is -1, which is always in a prime decomposition
		 * (it is a unit), but as such it occurs only pairwise, that's
		 * why we return 0 as exponent here
		 */
		bigz_fini(bz);
		return Fcons(bznumber, Qzero);
	}
	bigz_set_long(bz, 0L);
	if (bigz_eql(XBIGZ_DATA(bzfactor), bz)) {
		/* factor is zero, which is never in a prime decomposition */
		bigz_fini(bz);
		return Fcons(bznumber, Qzero);
	}
	if (bigz_lt(XBIGZ_DATA(bzfactor), bz)) {
		/* factor is negative, which is bad if number is positive */
		bigz_neg(XBIGZ_DATA(bzfactor), XBIGZ_DATA(bzfactor));
		occur = mpz_remove(bz, XBIGZ_DATA(bznumber),
				   XBIGZ_DATA(bzfactor));
		/* negate the result, iff the exponent is odd */
		if ((occur % 2) != 0)
			bigz_neg(bz, bz);
		result = make_bigz_bz(bz);
	} else {
		occur = mpz_remove(bz,
				   XBIGZ_DATA(bznumber),
				   XBIGZ_DATA(bzfactor));
		result = make_bigz_bz(bz);
	}

	bigz_fini(bz);
	return Fcons(result, make_integer((signed long)occur));
}

DEFUN("fibonacci", Ffibonacci, 1, 1, 0, /*
Return the NUMBERth Fibonacci number.
To compute both, the NUMBERth and (NUMBER-1)th Fibonacci
number use `fibonacci2' instead.
*/
      (number))
{
	bigz bz;
	unsigned long n;
	Lisp_Object result;

	CHECK_INTEGER(number);

	if (NILP(Fnonnegativep(number)))
		return wrong_type_argument(Qnonnegativep, number);
	else if (INTP(number))
		n = XINT(number);
	else if (BIGZP(number))
		n = bigz_to_ulong(XBIGZ_DATA(number));
	else
		return wrong_type_argument(Qintegerp, number);

	bigz_init(bz);
	mpz_fib_ui(bz, n);
	result = make_bigz_bz(bz);

	bigz_fini(bz);
	return result;
}

DEFUN("fibonacci2", Ffibonacci2, 1, 1, 0, /*
Return a cons with the NUMBERth and (NUMBER-1)th Fibonacci number.
To compute a series of Fibonacci numbers starting at index
NUMBER, use this function and recursively compute the rest.
*/
      (number))
{
	bigz bzn, bznsub1;
	unsigned long n;
	Lisp_Object result;

	CHECK_INTEGER(number);

	if (NILP(Fnonnegativep(number)))
		return wrong_type_argument(Qnonnegativep, number);
	else if (INTP(number))
		n = XINT(number);
	else if (BIGZP(number))
		n = bigz_to_ulong(XBIGZ_DATA(number));
	else
		return wrong_type_argument(Qintegerp, number);

	bigz_init(bzn);
	bigz_init(bznsub1);
	mpz_fib2_ui(bzn, bznsub1, n);
	result = Fcons(make_bigz_bz(bzn),
		       make_bigz_bz(bznsub1));

	bigz_fini(bzn);
	bigz_fini(bznsub1);
	return result;
}

DEFUN("lucas", Flucas, 1, 1, 0, /*
Return the NUMBERth Lucas number.
To compute both, the NUMBERth and (NUMBER-1)th Lucas
number use `lucas2' instead.
*/
      (number))
{
	bigz bz;
	unsigned long n;
	Lisp_Object result;

	CHECK_INTEGER(number);

	if (NILP(Fnonnegativep(number)))
		return wrong_type_argument(Qnonnegativep, number);
	else if (INTP(number))
		n = XINT(number);
	else if (BIGZP(number))
		n = bigz_to_ulong(XBIGZ_DATA(number));
	else
		return wrong_type_argument(Qintegerp, number);

	bigz_init(bz);
	mpz_lucnum_ui(bz, n);
	result = make_bigz_bz(bz);

	bigz_fini(bz);
	return result;
}

DEFUN("lucas2", Flucas2, 1, 1, 0, /*
Return a cons with the NUMBERth and (NUMBER-1)th Lucas number.
To compute a series of Lucas numbers starting at index
NUMBER, use this function and recursively compute the rest.
*/
      (number))
{
	bigz bzn, bznsub1;
	unsigned long n;
	Lisp_Object result;

	CHECK_INTEGER(number);

	if (NILP(Fnonnegativep(number)))
		return wrong_type_argument(Qnonnegativep, number);
	else if (INTP(number))
		n = XINT(number);
	else if (BIGZP(number))
		n = bigz_to_ulong(XBIGZ_DATA(number));
	else
		return wrong_type_argument(Qintegerp, number);

	bigz_init(bzn);
	bigz_init(bznsub1);
	mpz_lucnum2_ui(bzn, bznsub1, n);
	result = Fcons(make_bigz_bz(bzn),
		       make_bigz_bz(bznsub1));

	bigz_fini(bzn);
	bigz_fini(bznsub1);
	return result;
}

DEFUN("divisiblep", Fdivisiblep, 2, 2, 0, /*
Return t if NUMBER is divisible by D, nil otherwise.
*/
      (number, d))
{
	CHECK_INTEGER(number);
	CHECK_INTEGER(d);

	number = Fcoerce_number(number, Qbigz, Qnil);
	if (INTP(d))
		return mpz_divisible_ui_p(XBIGZ_DATA(number), XINT(d))
			? Qt : Qnil;
	else if (BIGZP(d))
		return mpz_divisible_p(XBIGZ_DATA(number), XBIGZ_DATA(d))
			? Qt : Qnil;
	else
		return wrong_type_argument(Qintegerp, d);
}

DEFUN("congruentp", Fcongruentp, 3, 3, 0, /*
Return t if NUMBER is congruent to C modulo M, nil otherwise.
*/
      (number, c, m))
{
	CHECK_INTEGER(number);
	CHECK_INTEGER(c);
	CHECK_INTEGER(m);

	number = Fcoerce_number(number, Qbigz, Qnil);
	if (INTP(c) && INTP(m))
		return mpz_congruent_ui_p(XBIGZ_DATA(number), XINT(c), XINT(m))
			? Qt : Qnil;
	else {
		c = Fcoerce_number(c, Qbigz, Qnil);
		m = Fcoerce_number(m, Qbigz, Qnil);
		return mpz_congruent_p(XBIGZ_DATA(number),
				       XBIGZ_DATA(c), XBIGZ_DATA(m))
			? Qt : Qnil;
	}
}

DEFUN("perfect-power-p", Fperfect_power_p, 1, 1, 0, /*
Return t if NUMBER is a perfect power, nil otherwise.
An integer NUMBER is said to be a perfect power if there
exist integers, a and b, such that a^b = NUMBER.
*/
      (number))
{
	CHECK_INTEGER(number);

	number = Fcoerce_number(number, Qbigz, Qnil);

	return mpz_perfect_power_p(XBIGZ_DATA(number)) ? Qt : Qnil;
}

DEFUN("perfect-square-p", Fperfect_square_p, 1, 1, 0, /*
Return t if NUMBER is a perfect square, nil otherwise.
An integer NUMBER is said to be a perfect square if there
exists an integer b such that b^2 = NUMBER.
*/
      (number))
{
	CHECK_INTEGER(number);

	number = Fcoerce_number(number, Qbigz, Qnil);

	return mpz_perfect_square_p(XBIGZ_DATA(number)) ? Qt : Qnil;
}

DEFUN("integral-sqrt", Fintegral_sqrt, 1, 1, 0, /*
Return a cons with the integral square root of NUMBER
in the car and the remainder in the cdr.
An integral square root is a number b and a remainder c
such that b*b + c = NUMBER.
*/
      (number))
{
	bigz bzsqrt, bzrem;
	Lisp_Object result;

	CHECK_INTEGER(number);

	number = Fcoerce_number(number, Qbigz, Qnil);

	bigz_init(bzsqrt);
	bigz_init(bzrem);
	mpz_sqrtrem(bzsqrt, bzrem, XBIGZ_DATA(number));

	result = Fcons(make_bigz_bz(bzsqrt), make_bigz_bz(bzrem));

	bigz_fini(bzsqrt);
	bigz_fini(bzrem);
	return result;
}

#endif	/* WITH_GMP && HAVE_MPZ */

DEFUN("zero-divisor-p", Fzero_divisor_p, 1, 1, 0, /*
Return t if NUMBER is a zero-divisor, nil otherwise.
That is, if there exists another non-zero number B, such that
  NUMBER * B = 0
*/
      (number))
{
	Lisp_Object result;

	CHECK_NUMBER(number);

	switch (ase_optable_index(number)) {
	default:
		result = Qnil;
	}
	return result;
}

#if defined WITH_ECM && defined HAVE_ECM &&	\
	defined HAVE_MPZ && defined WITH_GMP
DEFUN("factorise", Ffactorise, 1, 3, 0,	/*
Return the factorisation of NUMBER.
If optional arument B1 is non-nil, it should be a float used as
stage 1 boundary.
Second optional argument method can be 'ecm, 'p-1 'p+1.
*/
      (number, b1, method))
{
	int status;
	unsigned long expt;
	long factor_l;
	bigz bz;
	bigz bznumber;
	Lisp_Object bzn;
	Lisp_Object result = Qnil;
	double sb1;
	ecm_params p;

	bzn = Fcoerce_number(number, Qbigz, Qnil);
	bigz_init(bz);

	bigz_init(bznumber);
	bigz_set(bznumber, XBIGZ_DATA(bzn));

	if (NILP(b1))
		sb1 = 200.0;
	else
		sb1 = extract_float(b1);

	ecm_init(p);
	if (0) {
	} else if (method == intern("p-1")) {
		p->method = ECM_PM1;
	} else if (method == intern("p+1")) {
		p->method = ECM_PP1;
	} else {
		p->method = ECM_ECM;
	}

	status = 1;
	while (status > 0) {
		status = ecm_factor(bz, bznumber, sb1, p);

		factor_l = bigz_to_long(bz);
		if (factor_l == 1 || factor_l == -1)
			status = 0;
		if (status > 0 && factor_l != 0) {
			expt = mpz_remove(bznumber, bznumber, bz);
			result = Fcons(Fcons(make_bigz_bz(bz),
					     make_int(expt)),
				       result);
		}
	}

	ecm_clear(p);
	bigz_fini(bznumber);
	bigz_fini(bz);

	return result;
}
#endif	/* WITH_ECM && HAVE_ECM */

#if defined(WITH_GMP) && (defined(HAVE_MPZ) || defined(HAVE_MPQ))
DEFUN("gcd", Fgcd, 0, MANY, 0,	/*
Return the greatest common divisor of the arguments.
*/
      (int nargs, Lisp_Object *args))
{
	REGISTER int i;

	if (nargs == 0)
		return Qzero;
	else if (nargs == 1)
		return args[0];
	else {
		bigz bz;
		bigz bznum;
		bigz bzden;
		Lisp_Object bzn;
		bigz_init(bz);
		bigz_init(bznum);
		bigz_init(bzden);

		bzn = args[0];
		switch (ase_optable_index(bzn)) {
		case INT_T:
			bzn = Fcoerce_number(bzn, Qbigz, Qnil);
			bigz_set(bz, XBIGZ_DATA(bzn));
			bigz_set_long(bzden, 1L);
			break;
		case BIGZ_T:
			bigz_set(bz, XBIGZ_DATA(bzn));
			bigz_set_long(bzden, 1L);
			break;
		case BIGQ_T:
			bigz_set(bz, XBIGQ_NUMERATOR(bzn));
			bigz_set(bzden, XBIGQ_DENOMINATOR(bzn));
			break;

		/* no gcd defined for the rest */
		default:
			bigz_fini(bz);
			bigz_fini(bznum);
			bigz_fini(bzden);
			return Qzero;
			break;
		}

		for (i = 1; i < nargs; i++) {
			bzn = args[i];

			switch (ase_optable_index(bzn)) {
			case INT_T:
				bzn = Fcoerce_number(bzn, Qbigz, Qnil);
				bigz_set(bznum, XBIGZ_DATA(bzn));
				break;
			case BIGZ_T:
				bigz_set(bznum, XBIGZ_DATA(bzn));
				break;
			case BIGQ_T:
				bigz_mul(bzden, bzden, XBIGQ_DENOMINATOR(bzn));
				bigz_set(bznum, XBIGQ_NUMERATOR(bzn));
				break;

				/* no gcd defined for the rest */
			default:
				bigz_fini(bz);
				bigz_fini(bznum);
				bigz_fini(bzden);
				return Qzero;
				break;
			}

			bigz_gcd(bz, bz, bznum);
		}
		if (bigz_fits_long_p(bzden) &&
		    bigz_to_long(bzden) == 1L) {
			bzn = make_bigz_bz(bz);
		} else {
			bzn = make_bigq_bz(bz, bzden);
		}
		bigz_fini(bz);
		bigz_fini(bznum);
		bigz_fini(bzden);
		return bzn;
	}
	/* NOT REACHED */
	return Qzero;
}

DEFUN("xgcd", Fxgcd, 0, MANY, 0,	/*
Return the extended gcd of the arguments.
The result is a list of integers, where the car is the actual gcd
and the cdr consists of coefficients, s1, ..., sn, such that
s1*arg1 + s2*arg2 + ... + sn*argn = gcd.
*/
      (int nargs, Lisp_Object *args))
{
	REGISTER int i, j;

	if (nargs == 0)
		return list1(Qzero);
	else if (nargs == 1)
		return list2(args[0], make_int(1L));
	else {
		bigz bz;
		bigz bs;
		bigz bt;
		bigz bznum;
		bigz bzden;
		Lisp_Object bzn;
		Lisp_Object *qargs = alloca_array(Lisp_Object, nargs+1);
		bigz_init(bz);
		bigz_init(bznum);
		bigz_init(bzden);
		bigz_init(bs);
		bigz_init(bt);

		bzn = args[0];
		switch (ase_optable_index(bzn)) {
		case INT_T:
			bzn = Fcoerce_number(bzn, Qbigz, Qnil);
			bigz_set(bz, XBIGZ_DATA(bzn));
			bigz_set_long(bzden, 1L);
			break;
		case BIGZ_T:
			bigz_set(bz, XBIGZ_DATA(bzn));
			bigz_set_long(bzden, 1L);
			break;
		case BIGQ_T:
			bigz_set(bz, XBIGQ_NUMERATOR(bzn));
			bigz_set(bzden, XBIGQ_DENOMINATOR(bzn));
			break;

		/* no gcd defined for the rest */
		default:
			bigz_fini(bz);
			bigz_fini(bznum);
			bigz_fini(bzden);
			bigz_fini(bs);
			bigz_fini(bt);
			return list1(Qzero);
			break;
		}

		qargs[1] = make_bigz(1L);
		for (i = 1; i < nargs; i++) {
			bzn = args[i];

			switch (ase_optable_index(bzn)) {
			case INT_T:
				bzn = Fcoerce_number(bzn, Qbigz, Qnil);
				bigz_set(bznum, XBIGZ_DATA(bzn));
				break;
			case BIGZ_T:
				bigz_set(bznum, XBIGZ_DATA(bzn));
				break;
			/* multiply across fractions */
			case BIGQ_T:
				bigz_set(bznum, XBIGQ_NUMERATOR(bzn));
				bigz_mul(bznum, bznum, bzden);
				bigz_mul(bzden, bzden, XBIGQ_DENOMINATOR(bzn));
				bigz_mul(bz, bz, XBIGQ_DENOMINATOR(bzn));
				break;

				/* no gcd defined for the rest */
			default:
				bigz_fini(bz);
				bigz_fini(bznum);
				bigz_fini(bzden);
				bigz_fini(bs);
				bigz_fini(bt);
				return list1(Qzero);
				break;
			}

			mpz_gcdext(bz, bs, bt, bz, bznum);
			for (j = i; j > 0; j--) {
				bigz_mul(XBIGZ_DATA(qargs[j]),
					 XBIGZ_DATA(qargs[j]),
					 bs);
			}
			qargs[i+1] = make_bigz_bz(bt);
		}
		if (bigz_fits_long_p(bzden) &&
		    bigz_to_long(bzden) == 1L) {
			qargs[0] = make_bigz_bz(bz);
		} else {
			qargs[0] = make_bigq_bz(bz, bzden);
		}
		bigz_fini(bz);
		bigz_fini(bznum);
		bigz_fini(bzden);
		bigz_fini(bs);
		bigz_fini(bt);
		return Flist(nargs+1, qargs);
	}
	/* NOT REACHED */
	return Qzero;
}

DEFUN("lcm", Flcm, 0, MANY, 0,	/*
Return the least common multiple of the arguments.
*/
      (int nargs, Lisp_Object *args))
{
	REGISTER int i;

	if (nargs == 0)
		return Qzero;
	else if (nargs == 1)
		return args[0];
	else {
		bigz bz;
		bigz bznum;
		bigz bzden;
		Lisp_Object bzn;
		bigz_init(bz);
		bigz_init(bznum);
		bigz_init(bzden);

		bzn = args[0];
		switch (ase_optable_index(bzn)) {
		case INT_T:
			bzn = Fcoerce_number(bzn, Qbigz, Qnil);
			bigz_set(bz, XBIGZ_DATA(bzn));
			bigz_set_long(bzden, 1L);
			break;
		case BIGZ_T:
			bigz_set(bz, XBIGZ_DATA(bzn));
			bigz_set_long(bzden, 1L);
			break;
		case BIGQ_T:
			bigz_set(bz, XBIGQ_NUMERATOR(bzn));
			bigz_set(bzden, XBIGQ_DENOMINATOR(bzn));
			break;

		/* no lcm defined for the rest */
		default:
			bigz_fini(bz);
			bigz_fini(bznum);
			bigz_fini(bzden);
			return Qzero;
			break;
		}

		for (i = 1; i < nargs; i++) {
			bzn = args[i];

			switch (ase_optable_index(bzn)) {
			case INT_T:
				bzn = Fcoerce_number(bzn, Qbigz, Qnil);
				bigz_set(bznum, XBIGZ_DATA(bzn));
				break;
			case BIGZ_T:
				bigz_set(bznum, XBIGZ_DATA(bzn));
				break;
			/* multiply across fractions */
			case BIGQ_T:
				bigz_set(bznum, XBIGQ_NUMERATOR(bzn));
				bigz_mul(bznum, bznum, bzden);
				bigz_mul(bzden, bzden, XBIGQ_DENOMINATOR(bzn));
				bigz_mul(bz, bz, XBIGQ_DENOMINATOR(bzn));
				break;

			/* no gcd defined for the rest */
			default:
				bigz_fini(bz);
				bigz_fini(bznum);
				bigz_fini(bzden);
				return Qzero;
				break;
			}
			bigz_lcm(bz, bz, bznum);
		}
		if (bigz_fits_long_p(bzden) &&
		    bigz_to_long(bzden) == 1L) {
			bzn = make_bigz_bz(bz);
		} else {
			bzn = make_bigq_bz(bz, bzden);
		}
		bigz_fini(bz);
		bigz_fini(bznum);
		bigz_fini(bzden);
		return bzn;
	}
	/* NOT REACHED */
	return Qzero;
}
#endif	/* WITH_GMP && (HAVE_MPZ || HAVE_MPQ) */


/************************************************************************/
/*                              weak lists                              */
/************************************************************************/

/* A weak list is like a normal list except that elements automatically
   disappear when no longer in use, i.e. when no longer GC-protected.
   The basic idea is that we don't mark the elements during GC, but
   wait for them to be marked elsewhere.  If they're not marked, we
   remove them.  This is analogous to weak hash tables; see the explanation
   there for more info. */

static Lisp_Object Vall_weak_lists;	/* Gemarke es nicht!!! */

static Lisp_Object encode_weak_list_type(enum weak_list_type type);

static Lisp_Object mark_weak_list(Lisp_Object obj)
{
	return Qnil;		/* nichts ist gemarkt */
	/* avoid some warning */
	return (obj == Qnil);
}

static void
print_weak_list(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	if (print_readably)
		error("printing unreadable object #<weak-list>");

	write_c_string("#<weak-list ", printcharfun);
	print_internal(encode_weak_list_type(XWEAK_LIST(obj)->type),
		       printcharfun, 0);
	write_c_string(" ", printcharfun);
	print_internal(XWEAK_LIST(obj)->list, printcharfun, escapeflag);
	write_c_string(">", printcharfun);
}

static int weak_list_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	struct weak_list *w1 = XWEAK_LIST(obj1);
	struct weak_list *w2 = XWEAK_LIST(obj2);

	return ((w1->type == w2->type) &&
		internal_equal(w1->list, w2->list, depth + 1));
}

static unsigned long weak_list_hash(Lisp_Object obj, int depth)
{
	struct weak_list *w = XWEAK_LIST(obj);

	return HASH2((unsigned long)w->type, internal_hash(w->list, depth + 1));
}

Lisp_Object make_weak_list(enum weak_list_type type)
{
	Lisp_Object result;
	struct weak_list *wl =
	    alloc_lcrecord_type(struct weak_list, &lrecord_weak_list);

	wl->list = Qnil;
	wl->type = type;
	XSETWEAK_LIST(result, wl);
	wl->next_weak = Vall_weak_lists;
	Vall_weak_lists = result;
	return result;
}

static const struct lrecord_description weak_list_description[] = {
	{XD_LISP_OBJECT, offsetof(struct weak_list, list)},
	{XD_LO_LINK, offsetof(struct weak_list, next_weak)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION("weak-list", weak_list,
			      mark_weak_list, print_weak_list,
			      0, weak_list_equal, weak_list_hash,
			      weak_list_description, struct weak_list);
/*
   -- we do not mark the list elements (either the elements themselves
      or the cons cells that hold them) in the normal marking phase.
   -- at the end of marking, we go through all weak lists that are
      marked, and mark the cons cells that hold all marked
      objects, and possibly parts of the objects themselves.
      (See alloc.c, "after-mark".)
   -- after that, we prune away all the cons cells that are not marked.

   WARNING WARNING WARNING WARNING WARNING:

   The code in the following two functions is *unbelievably* tricky.
   Don't mess with it.  You'll be sorry.

   Linked lists just majorly suck, d'ya know?
*/

int finish_marking_weak_lists(void)
{
	Lisp_Object rest;
	int did_mark = 0;

	for (rest = Vall_weak_lists;
	     !NILP(rest); rest = XWEAK_LIST(rest)->next_weak) {
		Lisp_Object rest2;
		enum weak_list_type type = XWEAK_LIST(rest)->type;

		if (!marked_p(rest))
			/* The weak list is probably garbage.  Ignore it. */
			continue;

		for (rest2 = XWEAK_LIST(rest)->list;
		     /* We need to be trickier since we're inside of GC;
			use CONSP instead of !NILP in case of user-visible
			imperfect lists */
		     CONSP(rest2); rest2 = XCDR(rest2)) {
			Lisp_Object elem;
			/* If the element is "marked" (meaning depends on the type
			   of weak list), we need to mark the cons containing the
			   element, and maybe the element itself (if only some part
			   was already marked). */
			int need_to_mark_cons = 0;
			int need_to_mark_elem = 0;

			/* If a cons is already marked, then its car is already marked
			   (either because of an external pointer or because of
			   a previous call to this function), and likewise for all
			   the rest of the elements in the list, so we can stop now. */
			if (marked_p(rest2))
				break;

			elem = XCAR(rest2);

			switch (type) {
			case WEAK_LIST_SIMPLE:
				if (marked_p(elem))
					need_to_mark_cons = 1;
				break;

			case WEAK_LIST_ASSOC:
				if (!CONSP(elem)) {
					/* just leave bogus elements there */
					need_to_mark_cons = 1;
					need_to_mark_elem = 1;
				} else if (marked_p(XCAR(elem)) &&
					   marked_p(XCDR(elem))) {
					need_to_mark_cons = 1;
					/* We still need to mark elem, because it's
					   probably not marked. */
					need_to_mark_elem = 1;
				}
				break;

			case WEAK_LIST_KEY_ASSOC:
				if (!CONSP(elem)) {
					/* just leave bogus elements there */
					need_to_mark_cons = 1;
					need_to_mark_elem = 1;
				} else if (marked_p(XCAR(elem))) {
					need_to_mark_cons = 1;
					/* We still need to mark elem and XCDR (elem);
					   marking elem does both */
					need_to_mark_elem = 1;
				}
				break;

			case WEAK_LIST_VALUE_ASSOC:
				if (!CONSP(elem)) {
					/* just leave bogus elements there */
					need_to_mark_cons = 1;
					need_to_mark_elem = 1;
				} else if (marked_p(XCDR(elem))) {
					need_to_mark_cons = 1;
					/* We still need to mark elem and XCAR (elem);
					   marking elem does both */
					need_to_mark_elem = 1;
				}
				break;

			case WEAK_LIST_FULL_ASSOC:
				if (!CONSP(elem)) {
					/* just leave bogus elements there */
					need_to_mark_cons = 1;
					need_to_mark_elem = 1;
				} else if (marked_p(XCAR(elem)) ||
					   marked_p(XCDR(elem))) {
					need_to_mark_cons = 1;
					/* We still need to mark elem and XCAR (elem);
					   marking elem does both */
					need_to_mark_elem = 1;
				}
				break;

			default:
				abort();
			}

			if (need_to_mark_elem && !marked_p(elem)) {
				mark_object(elem);
				did_mark = 1;
			}

			/* We also need to mark the cons that holds the elem or
			   assoc-pair.  We do *not* want to call (mark_object) here
			   because that will mark the entire list; we just want to
			   mark the cons itself.
			 */
			if (need_to_mark_cons) {
				Lisp_Cons *c = XCONS(rest2);
				if (!CONS_MARKED_P(c)) {
					MARK_CONS(c);
					did_mark = 1;
				}
			}
		}

		/* In case of imperfect list, need to mark the final cons
		   because we're not removing it */
		if (!NILP(rest2) && !marked_p(rest2)) {
			mark_object(rest2);
			did_mark = 1;
		}
	}

	return did_mark;
}

void prune_weak_lists(void)
{
	Lisp_Object rest, prev = Qnil;

	for (rest = Vall_weak_lists;
	     !NILP(rest); rest = XWEAK_LIST(rest)->next_weak) {
		if (!(marked_p(rest))) {
			/* This weak list itself is garbage.  Remove it from the list. */
			if (NILP(prev))
				Vall_weak_lists = XWEAK_LIST(rest)->next_weak;
			else
				XWEAK_LIST(prev)->next_weak =
				    XWEAK_LIST(rest)->next_weak;
		} else {
			Lisp_Object rest2, prev2 = Qnil;
			Lisp_Object tortoise;
			int go_tortoise = 0;

			for (rest2 = XWEAK_LIST(rest)->list, tortoise = rest2;
			     /* We need to be trickier since we're inside of GC;
				use CONSP instead of !NILP in case of user-visible
				imperfect lists */
			     CONSP(rest2);) {
				/* It suffices to check the cons for marking,
				   regardless of the type of weak list:

				   -- if the cons is pointed to somewhere else,
				   then it should stay around and will be marked.
				   -- otherwise, if it should stay around, it will
				   have been marked in finish_marking_weak_lists().
				   -- otherwise, it's not marked and should disappear.
				 */
				if (!marked_p(rest2)) {
					/* bye bye :-( */
					if (NILP(prev2))
						XWEAK_LIST(rest)->list =
						    XCDR(rest2);
					else
						XCDR(prev2) = XCDR(rest2);
					rest2 = XCDR(rest2);
					/* Ouch.  Circularity checking is even trickier
					   than I thought.  When we cut out a link
					   like this, we can't advance the turtle or
					   it'll catch up to us.  Imagine that we're
					   standing on floor tiles and moving forward --
					   what we just did here is as if the floor
					   tile under us just disappeared and all the
					   ones ahead of us slid one tile towards us.
					   In other words, we didn't move at all;
					   if the tortoise was one step behind us
					   previously, it still is, and therefore
					   it must not move. */
				} else {
					prev2 = rest2;

					/* Implementing circularity checking is trickier here
					   than in other places because we have to guarantee
					   that we've processed all elements before exiting
					   due to a circularity. (In most places, an error
					   is issued upon encountering a circularity, so it
					   doesn't really matter if all elements are processed.)
					   The idea is that we process along with the hare
					   rather than the tortoise.  If at any point in
					   our forward process we encounter the tortoise,
					   we must have already visited the spot, so we exit.
					   (If we process with the tortoise, we can fail to
					   process cases where a cons points to itself, or
					   where cons A points to cons B, which points to
					   cons A.) */

					rest2 = XCDR(rest2);
					if (go_tortoise)
						tortoise = XCDR(tortoise);
					go_tortoise = !go_tortoise;
					if (EQ(rest2, tortoise))
						break;
				}
			}

			prev = rest;
		}
	}
}

static enum weak_list_type decode_weak_list_type(Lisp_Object symbol)
{
	CHECK_SYMBOL(symbol);
	if (EQ(symbol, Qsimple))
		return WEAK_LIST_SIMPLE;
	if (EQ(symbol, Qassoc))
		return WEAK_LIST_ASSOC;
	if (EQ(symbol, Qold_assoc))
		return WEAK_LIST_ASSOC;	/* EBOLA ALERT! */
	if (EQ(symbol, Qkey_assoc))
		return WEAK_LIST_KEY_ASSOC;
	if (EQ(symbol, Qvalue_assoc))
		return WEAK_LIST_VALUE_ASSOC;
	if (EQ(symbol, Qfull_assoc))
		return WEAK_LIST_FULL_ASSOC;

	signal_simple_error("Invalid weak list type", symbol);
	return WEAK_LIST_SIMPLE;	/* not reached */
}

static Lisp_Object encode_weak_list_type(enum weak_list_type type)
{
	switch (type) {
	case WEAK_LIST_SIMPLE:
		return Qsimple;
	case WEAK_LIST_ASSOC:
		return Qassoc;
	case WEAK_LIST_KEY_ASSOC:
		return Qkey_assoc;
	case WEAK_LIST_VALUE_ASSOC:
		return Qvalue_assoc;
	case WEAK_LIST_FULL_ASSOC:
		return Qfull_assoc;
	default:
		abort();
	}

	return Qnil;		/* not reached */
}

DEFUN("weak-list-p", Fweak_list_p, 1, 1, 0,	/*
Return non-nil if OBJECT is a weak list.
*/
      (object))
{
	return WEAK_LISTP(object) ? Qt : Qnil;
}

DEFUN("make-weak-list", Fmake_weak_list, 0, 1, 0,	/*
Return a new weak list object of type TYPE.
A weak list object is an object that contains a list.  This list behaves
like any other list except that its elements do not count towards
garbage collection -- if the only pointer to an object is inside a weak
list (other than pointers in similar objects such as weak hash tables),
the object is garbage collected and automatically removed from the list.
This is used internally, for example, to manage the list holding the
children of an extent -- an extent that is unused but has a parent will
still be reclaimed, and will automatically be removed from its parent's
list of children.

Optional argument TYPE specifies the type of the weak list, and defaults
to `simple'.  Recognized types are

  `simple'       Objects in the list disappear if not pointed to.
  `assoc'        Objects in the list disappear if they are conses
		 and either the car or the cdr of the cons is not
		 pointed to.
  `key-assoc'    Objects in the list disappear if they are conses
		 and the car is not pointed to.
  `value-assoc'  Objects in the list disappear if they are conses
		 and the cdr is not pointed to.
  `full-assoc'   Objects in the list disappear if they are conses
		 and neither the car nor the cdr is pointed to.
*/
      (type))
{
	if (NILP(type))
		type = Qsimple;

	return make_weak_list(decode_weak_list_type(type));
}

DEFUN("weak-list-type", Fweak_list_type, 1, 1, 0,	/*
Return the type of the given weak-list object.
*/
      (weak))
{
	CHECK_WEAK_LIST(weak);
	return encode_weak_list_type(XWEAK_LIST(weak)->type);
}

DEFUN("weak-list-list", Fweak_list_list, 1, 1, 0,	/*
Return the list contained in a weak-list object.
*/
      (weak))
{
	CHECK_WEAK_LIST(weak);
	return XWEAK_LIST_LIST(weak);
}

DEFUN("set-weak-list-list", Fset_weak_list_list, 2, 2, 0,	/*
Change the list contained in a weak-list object.
*/
      (weak, new_list))
{
	CHECK_WEAK_LIST(weak);
	XWEAK_LIST_LIST(weak) = new_list;
	return new_list;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

static SIGTYPE arith_error(int signo)
{
	EMACS_REESTABLISH_SIGNAL(signo, arith_error);
	EMACS_UNBLOCK_SIGNAL(signo);
	signal_error(Qarith_error, Qnil);
}

void init_data_very_early(void)
{
	/* Don't do this if just dumping out.
	   We don't want to call `signal' in this case
	   so that we don't have trouble with dumping
	   signal-delivering routines in an inconsistent state.  */
#ifndef CANNOT_DUMP
	if (!initialized)
		return;
#endif				/* CANNOT_DUMP */
	signal(SIGFPE, arith_error);
#ifdef uts
	signal(SIGEMT, arith_error);
#endif				/* uts */
}

void
init_errors_once_early (void)
{
	DEFSYMBOL (Qerror_conditions);
	DEFSYMBOL (Qerror_message);

	/* We declare the errors here because some other deferrors depend
	   on some of the errors below. */

	/* ERROR is used as a signaler for random errors for which nothing
	   else is right */

	DEFERROR (Qerror, "error", Qnil);
	DEFERROR_STANDARD (Qquit, Qnil);

	DEFERROR_STANDARD (Qinvalid_argument, Qerror);

	DEFERROR_STANDARD (Qsyntax_error, Qinvalid_argument);
	DEFERROR_STANDARD (Qinvalid_read_syntax, Qsyntax_error);
	DEFERROR_STANDARD (Qstructure_formation_error, Qsyntax_error);
	DEFERROR_STANDARD (Qlist_formation_error, Qstructure_formation_error);
	DEFERROR_STANDARD (Qmalformed_list, Qlist_formation_error);
	DEFERROR_STANDARD (Qmalformed_property_list, Qmalformed_list);
	DEFERROR_STANDARD (Qcircular_list, Qlist_formation_error);
	DEFERROR_STANDARD (Qcircular_property_list, Qcircular_list);

	DEFERROR_STANDARD (Qwrong_type_argument, Qinvalid_argument);
	DEFERROR_STANDARD (Qargs_out_of_range, Qinvalid_argument);
	DEFERROR_STANDARD (Qwrong_number_of_arguments, Qinvalid_argument);
	DEFERROR_STANDARD (Qinvalid_function, Qinvalid_argument);
	DEFERROR_STANDARD (Qinvalid_constant, Qinvalid_argument);
	DEFERROR (Qno_catch, "No catch for tag", Qinvalid_argument);

	DEFERROR_STANDARD (Qinvalid_state, Qerror);
	DEFERROR (Qvoid_function, "Symbol's function definition is void",
		  Qinvalid_state);
	DEFERROR (Qcyclic_function_indirection,
		  "Symbol's chain of function indirections contains a loop",
		  Qinvalid_state);
	DEFERROR (Qvoid_variable, "Symbol's value as variable is void",
		  Qinvalid_state);
	DEFERROR (Qcyclic_variable_indirection,
		  "Symbol's chain of variable indirections contains a loop",
		  Qinvalid_state);
	DEFERROR_STANDARD (Qstack_overflow, Qinvalid_state);
	DEFERROR_STANDARD (Qinternal_error, Qinvalid_state);
	DEFERROR_STANDARD (Qout_of_memory, Qinvalid_state);

	DEFERROR_STANDARD (Qinvalid_operation, Qerror);
	DEFERROR_STANDARD (Qinvalid_change, Qinvalid_operation);
	DEFERROR (Qsetting_constant, "Attempt to set a constant symbol",
		  Qinvalid_change);
	DEFERROR_STANDARD (Qprinting_unreadable_object, Qinvalid_operation);
	DEFERROR (Qunimplemented, "Feature not yet implemented", Qinvalid_operation);

	DEFERROR_STANDARD (Qediting_error, Qinvalid_operation);
	DEFERROR_STANDARD (Qbeginning_of_buffer, Qediting_error);
	DEFERROR_STANDARD (Qend_of_buffer, Qediting_error);
	DEFERROR (Qbuffer_read_only, "Buffer is read-only", Qediting_error);

	DEFERROR (Qio_error, "IO Error", Qinvalid_operation);
	DEFERROR_STANDARD (Qfile_error, Qio_error);
	DEFERROR (Qend_of_file, "End of file or stream", Qfile_error);
	DEFERROR_STANDARD (Qconversion_error, Qio_error);
	DEFERROR_STANDARD (Qtext_conversion_error, Qconversion_error);

	DEFERROR (Qarith_error, "Arithmetic error", Qinvalid_operation);
	DEFERROR (Qrange_error, "Arithmetic range error", Qarith_error);
	DEFERROR (Qdomain_error, "Arithmetic domain error", Qarith_error);
	DEFERROR (Qsingularity_error, "Arithmetic singularity error", Qdomain_error);
	DEFERROR (Qoverflow_error, "Arithmetic overflow error", Qdomain_error);
	DEFERROR (Qunderflow_error, "Arithmetic underflow error", Qdomain_error);
}

void syms_of_data(void)
{
	INIT_LRECORD_IMPLEMENTATION(weak_list);

	DEFSYMBOL(Qquote);
	DEFSYMBOL(Qlambda);
	DEFSYMBOL(Qlistp);
	DEFSYMBOL(Qtrue_list_p);
	DEFSYMBOL(Qconsp);
	DEFSYMBOL(Qsubrp);
	DEFSYMBOL(Qsymbolp);
	DEFSYMBOL(Qintegerp);
	DEFSYMBOL(Qcharacterp);
	DEFSYMBOL(Qnatnump);
	DEFSYMBOL(Qnonnegativep);
	DEFSYMBOL(Qpositivep);
	DEFSYMBOL(Qstringp);
	DEFSYMBOL(Qarrayp);
	DEFSYMBOL(Qsequencep);
	DEFSYMBOL(Qdictp);
	DEFSYMBOL(Qbufferp);
	DEFSYMBOL(Qbitp);
	DEFSYMBOL_MULTIWORD_PREDICATE(Qbit_vectorp);
	DEFSYMBOL(Qvectorp);
	DEFSYMBOL(Qchar_or_string_p);
	DEFSYMBOL(Qmarkerp);
	DEFSYMBOL(Qinteger_or_marker_p);
	DEFSYMBOL(Qinteger_or_char_p);
	DEFSYMBOL(Qinteger_char_or_marker_p);
	DEFSYMBOL(Qnumberp);
	DEFSYMBOL(Qnumber_char_or_marker_p);
	DEFSYMBOL(Qcdr);
	DEFSYMBOL_MULTIWORD_PREDICATE(Qweak_listp);

#ifdef HAVE_FPFLOAT
	DEFSYMBOL(Qfloatp);
#endif				/* HAVE_FPFLOAT */

	DEFSUBR(Fwrong_type_argument);

	DEFSUBR(Feq);
	DEFSUBR(Fold_eq);
	DEFSUBR(Fnull);
	Ffset(intern("not"), intern("null"));
	DEFSUBR(Flistp);
	DEFSUBR(Fnlistp);
	DEFSUBR(Ftrue_list_p);
	DEFSUBR(Fconsp);
	DEFSUBR(Fatom);
	DEFSUBR(Fchar_or_string_p);
	DEFSUBR(Fcharacterp);
	DEFSUBR(Fchar_int_p);
	DEFSUBR(Fchar_to_int);
	DEFSUBR(Fint_to_char);
	DEFSUBR(Fchar_or_char_int_p);
	DEFSUBR(Fintp);
	DEFSUBR(Ffixnump);
	DEFSUBR(Finteger_or_marker_p);
	DEFSUBR(Finteger_or_char_p);
	DEFSUBR(Finteger_char_or_marker_p);
	DEFSUBR(Fnumberp);
	DEFSUBR(Fnumber_or_marker_p);
	DEFSUBR(Fnumber_char_or_marker_p);
#ifdef HAVE_FPFLOAT
	DEFSUBR(Ffloatp);
#endif				/* HAVE_FPFLOAT */
	DEFSUBR(Fnatnump);
	DEFSUBR(Fnonnegativep);
	DEFSUBR(Fsymbolp);
	DEFSUBR(Fkeywordp);
	DEFSUBR(Fstringp);
	DEFSUBR(Fvectorp);
	DEFSUBR(Fbitp);
	DEFSUBR(Fbit_vector_p);
	DEFSUBR(Farrayp);
	DEFSUBR(Fsequencep);
	DEFSUBR(Fmarkerp);
	DEFSUBR(Fsubrp);
	DEFSUBR(Fsubr_min_args);
	DEFSUBR(Fsubr_max_args);
	DEFSUBR(Fsubr_interactive);
	DEFSUBR(Ftype_of);
	DEFSUBR(Fcar);
	DEFSUBR(Fcdr);
	DEFSUBR(Fcar_safe);
	DEFSUBR(Fcdr_safe);
	DEFSUBR(Fsetcar);
	DEFSUBR(Fsetcdr);
	DEFSUBR(Findirect_function);
	DEFSUBR(Faref);
	DEFSUBR(Faset);

	DEFSUBR(Fnumber_to_string);
	DEFSUBR(Fstring_to_number);

	DEFSUBR(Flogand);
	DEFSUBR(Flogior);
	DEFSUBR(Flogxor);
	DEFSUBR(Flsh);
	DEFSUBR(Fash);
	DEFSUBR(Flognot);
#if defined(WITH_GMP) && defined(HAVE_MPZ)
	DEFSUBR(Fprimep);
	DEFSUBR(Fnext_prime);
	DEFSUBR(Fgcd);
	DEFSUBR(Fxgcd);
	DEFSUBR(Flcm);
	DEFSUBR(Ffactorial);
	DEFSUBR(Fbinomial_coefficient);
	DEFSUBR(Fremove_factor);
	DEFSUBR(Ffibonacci);
	DEFSUBR(Ffibonacci2);
	DEFSUBR(Flucas);
	DEFSUBR(Flucas2);
	DEFSUBR(Fdivisiblep);
	DEFSUBR(Fcongruentp);
	DEFSUBR(Fperfect_power_p);
	DEFSUBR(Fperfect_square_p);
	DEFSUBR(Fintegral_sqrt);
#if defined HAVE_ECM && defined WITH_ECM
	DEFSUBR(Ffactorise);	/* some day maybe */
#endif	/* WITH_ECM && HAVE_ECM */
#endif	/* WITH_GMP && HAVE_MPZ */
	DEFSUBR(Fzero_divisor_p);
	DEFSUBR(Fweak_list_p);
	DEFSUBR(Fmake_weak_list);
	DEFSUBR(Fweak_list_type);
	DEFSUBR(Fweak_list_list);
	DEFSUBR(Fset_weak_list_list);
}

void vars_of_data(void)
{
	/* This must not be staticpro'd */
	Vall_weak_lists = Qnil;
	dump_add_weak_object_chain(&Vall_weak_lists);

#ifdef DEBUG_SXEMACS
	DEFVAR_BOOL("debug-issue-ebola-notices", &debug_issue_ebola_notices	/*
If non-zero, note when your code may be suffering from char-int confoundance.
That is to say, if SXEmacs encounters a usage of `eq', `memq', `equal',
etc. where an int and a char with the same value are being compared,
it will issue a notice on stderr to this effect, along with a backtrace.
In such situations, the result would be different in XEmacs 19 versus
XEmacs 20, and you probably don't want this.

Note that in order to see these notices, you have to byte compile your
code under XEmacs 20 -- any code byte-compiled under XEmacs 19 will
have its chars and ints all confounded in the byte code, making it
impossible to accurately determine Ebola infection.
										 */ );

	debug_issue_ebola_notices = 0;

	DEFVAR_INT("debug-ebola-backtrace-length", &debug_ebola_backtrace_length	/*
Length (in stack frames) of short backtrace printed out in Ebola notices.
See `debug-issue-ebola-notices'.
											 */ );
	debug_ebola_backtrace_length = 32;

#endif				/* DEBUG_SXEMACS */
}
