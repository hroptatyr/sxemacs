/* Primitive operations on Lisp data types for SXEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 2000 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30.  Some of FSF's data.c is in
   SXEmacs' symbols.c. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "syssignal.h"

#ifdef LISP_FLOAT_TYPE
/* Need to define a differentiating symbol -- see sysfloat.h */
# define THIS_FILENAME data_c
# include "sysfloat.h"
#endif				/* LISP_FLOAT_TYPE */

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message;
Lisp_Object Qerror, Qquit, Qsyntax_error, Qinvalid_read_syntax;
Lisp_Object Qlist_formation_error;
Lisp_Object Qmalformed_list, Qmalformed_property_list;
Lisp_Object Qcircular_list, Qcircular_property_list;
Lisp_Object Qinvalid_argument, Qwrong_type_argument, Qargs_out_of_range;
Lisp_Object Qwrong_number_of_arguments, Qinvalid_function, Qno_catch;
Lisp_Object Qinternal_error, Qinvalid_state;
Lisp_Object Qvoid_variable, Qcyclic_variable_indirection;
Lisp_Object Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qinvalid_operation, Qinvalid_change;
Lisp_Object Qsetting_constant;
Lisp_Object Qediting_error;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qio_error, Qend_of_file;
Lisp_Object Qarith_error, Qrange_error, Qdomain_error;
Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
Lisp_Object Qintegerp, Qnatnump, Qsymbolp;
Lisp_Object Qlistp, Qtrue_list_p, Qweak_listp;
Lisp_Object Qconsp, Qsubrp;
Lisp_Object Qcharacterp, Qstringp, Qarrayp, Qsequencep, Qvectorp;
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
				 */
      (object))
{
	return CONSP(object) ? Qt : Qnil;
}

DEFUN("atom", Fatom, 1, 1, 0,	/*
Return t if OBJECT is not a cons cell.  `nil' is not a cons cell.
				 */
      (object))
{
	return CONSP(object) ? Qnil : Qt;
}

DEFUN("listp", Flistp, 1, 1, 0,	/*
Return t if OBJECT is a list.  `nil' is a list.
				 */
      (object))
{
	return LISTP(object) ? Qt : Qnil;
}

DEFUN("nlistp", Fnlistp, 1, 1, 0,	/*
Return t if OBJECT is not a list.  `nil' is a list.
					 */
      (object))
{
	return LISTP(object) ? Qnil : Qt;
}

DEFUN("true-list-p", Ftrue_list_p, 1, 1, 0,	/*
Return t if OBJECT is an acyclic, nil-terminated (ie, not dotted), list.
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
Return t if OBJECT is a sequence (list or array).
					 */
      (object))
{
	return (LISTP(object) ||
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
      (object)) {
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
      (integer)) {
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

DEFUN("integerp", Fintegerp, 1, 1, 0,	/*
Return t if OBJECT is an integer.
					 */
      (object))
{
	return INTP(object) ? Qt : Qnil;
}

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
	return NATNUMP(object) ? Qt : Qnil;
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
	return INT_OR_FLOATP(object) ? Qt : Qnil;
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

#ifdef LISP_FLOAT_TYPE
DEFUN("floatp", Ffloatp, 1, 1, 0,	/*
Return t if OBJECT is a floating point number.
					 */
      (object))
{
	return FLOATP(object) ? Qt : Qnil;
}
#endif				/* LISP_FLOAT_TYPE */

DEFUN("type-of", Ftype_of, 1, 1, 0,	/*
Return a symbol representing the type of OBJECT.
					 */
      (object))
{
	switch (XTYPE(object)) {
	case Lisp_Type_Record:
		return intern(XRECORD_LHEADER_IMPLEMENTATION(object)->name);

	case Lisp_Type_Char:
		return Qcharacter;

	default:
		return Qinteger;
	}
}

/* Extract and set components of lists */

DEFUN("car", Fcar, 1, 1, 0,	/*
Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `car-safe'.
				 */
      (list))
{
	while (1) {
		if (CONSP(list))
			return XCAR(list);
		else if (NILP(list))
			return Qnil;
		else
			list = wrong_type_argument(Qlistp, list);
	}
}

DEFUN("car-safe", Fcar_safe, 1, 1, 0,	/*
Return the car of OBJECT if it is a cons cell, or else nil.
					 */
      (object))
{
	return CONSP(object) ? XCAR(object) : Qnil;
}

DEFUN("cdr", Fcdr, 1, 1, 0,	/*
Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.
				 */
      (list))
{
	while (1) {
		if (CONSP(list))
			return XCDR(list);
		else if (NILP(list))
			return Qnil;
		else
			list = wrong_type_argument(Qlistp, list);
	}
}

DEFUN("cdr-safe", Fcdr_safe, 1, 1, 0,	/*
Return the cdr of OBJECT if it is a cons cell, else nil.
					 */
      (object))
{
	return CONSP(object) ? XCDR(object) : Qnil;
}

DEFUN("setcar", Fsetcar, 2, 2, 0,	/*
Set the car of CONS-CELL to be NEWCAR.  Return NEWCAR.
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

      retry:

	if (INTP(index_))
		idx = XINT(index_);
	else if (CHARP(index_))
		idx = XCHAR(index_);	/* yuck! */
	else {
		index_ = wrong_type_argument(Qinteger_or_char_p, index_);
		goto retry;
	}

	if (idx < 0)
		goto range_error;

	if (VECTORP(array)) {
		if (idx >= XVECTOR_LENGTH(array))
			goto range_error;
		return XVECTOR_DATA(array)[idx];
	} else if (BIT_VECTORP(array)) {
		if (idx >= bit_vector_length(XBIT_VECTOR(array)))
			goto range_error;
		return make_int(bit_vector_bit(XBIT_VECTOR(array), idx));
	} else if (STRINGP(array)) {
		if (idx >= XSTRING_CHAR_LENGTH(array))
			goto range_error;
		return make_char(string_char(XSTRING(array), idx));
	}
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

      retry:

	if (INTP(index_))
		idx = XINT(index_);
	else if (CHARP(index_))
		idx = XCHAR(index_);	/* yuck! */
	else {
		index_ = wrong_type_argument(Qinteger_or_char_p, index_);
		goto retry;
	}

	if (idx < 0)
		goto range_error;

	if (VECTORP(array)) {
		if (idx >= XVECTOR_LENGTH(array))
			goto range_error;
		XVECTOR_DATA(array)[idx] = newval;
	} else if (BIT_VECTORP(array)) {
		if (idx >= bit_vector_length(XBIT_VECTOR(array)))
			goto range_error;
		CHECK_BIT(newval);
		set_bit_vector_bit(XBIT_VECTOR(array), idx, !ZEROP(newval));
	} else if (STRINGP(array)) {
		CHECK_CHAR_COERCE_INT(newval);
		if (idx >= XSTRING_CHAR_LENGTH(array))
			goto range_error;
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
#ifdef LISP_FLOAT_TYPE
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
#ifdef LISP_FLOAT_TYPE
	else if (FLOATP(obj))
		return XFLOAT_DATA(obj);
#endif
	else {
		obj = wrong_type_argument(Qnumber_char_or_marker_p, obj);
		goto retry;
	}
}

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

#define ARITHCOMPARE_MANY(op)					\
{								\
  int_or_double iod1, iod2, *p = &iod1, *q = &iod2;		\
  Lisp_Object *args_end = args + nargs;				\
								\
  number_char_or_marker_to_int_or_double (*args++, p);		\
								\
  while (args < args_end)					\
    {								\
      number_char_or_marker_to_int_or_double (*args++, q);	\
								\
      if (!((p->int_p && q->int_p) ?				\
	    (p->c.ival op q->c.ival) :				\
	    ((p->int_p ? (double) p->c.ival : p->c.dval) op	\
	     (q->int_p ? (double) q->c.ival : q->c.dval))))	\
	return Qnil;						\
								\
      { /* swap */ int_or_double *r = p; p = q; q = r; }	\
    }								\
  return Qt;							\
}

DEFUN("=", Feqlsign, 1, MANY, 0,	/*
Return t if all the arguments are numerically equal.
The arguments may be numbers, characters or markers.
					 */
      (int nargs, Lisp_Object * args))
{
	ARITHCOMPARE_MANY( ==)
}

DEFUN("<", Flss, 1, MANY, 0,	/*
Return t if the sequence of arguments is monotonically increasing.
The arguments may be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	ARITHCOMPARE_MANY(<)
}

DEFUN(">", Fgtr, 1, MANY, 0,	/*
Return t if the sequence of arguments is monotonically decreasing.
The arguments may be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	ARITHCOMPARE_MANY(>)
}

DEFUN("<=", Fleq, 1, MANY, 0,	/*
Return t if the sequence of arguments is monotonically nondecreasing.
The arguments may be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	ARITHCOMPARE_MANY(<=)
}

DEFUN(">=", Fgeq, 1, MANY, 0,	/*
Return t if the sequence of arguments is monotonically nonincreasing.
The arguments may be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	ARITHCOMPARE_MANY(>=)
}

DEFUN("/=", Fneq, 1, MANY, 0,	/*
Return t if no two arguments are numerically equal.
The arguments may be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	Lisp_Object *args_end = args + nargs;
	Lisp_Object *p, *q;

	/* Unlike all the other comparisons, this is an N*N algorithm.
	   We could use a hash table for nargs > 50 to make this linear. */
	for (p = args; p < args_end; p++) {
		int_or_double iod1, iod2;
		number_char_or_marker_to_int_or_double(*p, &iod1);

		for (q = p + 1; q < args_end; q++) {
			number_char_or_marker_to_int_or_double(*q, &iod2);

			if (!((iod1.int_p && iod2.int_p) ?
			      (iod1.c.ival != iod2.c.ival) :
			      ((iod1.int_p ? (double)iod1.c.ival : iod1.c.
				dval) !=
			       (iod2.int_p ? (double)iod2.c.ival : iod2.c.
				dval))))
				return Qnil;
		}
	}
	return Qt;
}

DEFUN("zerop", Fzerop, 1, 1, 0,	/*
Return t if NUMBER is zero.
				 */
      (number))
{
      retry:
	if (INTP(number))
		return EQ(number, Qzero) ? Qt : Qnil;
#ifdef LISP_FLOAT_TYPE
	else if (FLOATP(number))
		return XFLOAT_DATA(number) == 0.0 ? Qt : Qnil;
#endif				/* LISP_FLOAT_TYPE */
	else {
		number = wrong_type_argument(Qnumberp, number);
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

	CHECK_INT_OR_FLOAT(number);

#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number)) {
		char pigbuf[350];	/* see comments in float_to_string */

		float_to_string(pigbuf, XFLOAT_DATA(number));
		return build_string(pigbuf);
	}
#endif				/* LISP_FLOAT_TYPE */

	long_to_string(buffer, XINT(number));
	return build_string(buffer);
}

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

DEFUN("string-to-number", Fstring_to_number, 1, 2, 0,	/*
Convert STRING to a number by parsing it as a number in base BASE.
This parses both integers and floating point numbers.
It ignores leading spaces and tabs.

If BASE is nil or omitted, base 10 is used.
BASE must be an integer between 2 and 16 (inclusive).
Floating point numbers always use base 10.
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

#ifdef LISP_FLOAT_TYPE
	if (isfloat_string(p) && b == 10)
		return make_float(atof(p));
#endif				/* LISP_FLOAT_TYPE */

	if (b == 10) {
		/* Use the system-provided functions for base 10. */
#if   SIZEOF_EMACS_INT == SIZEOF_INT
		return make_int(atoi(p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG
		return make_int(atol(p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG_LONG
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
}

DEFUN("+", Fplus, 0, MANY, 0,	/*
Return sum of any number of arguments.
The arguments should all be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT iaccum = 0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end) {
		int_or_double iod;
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p)
			iaccum += iod.c.ival;
		else {
			double daccum = (double)iaccum + iod.c.dval;
			while (args < args_end)
				daccum +=
				    number_char_or_marker_to_double(*args++);
			return make_float(daccum);
		}
	}

	return make_int(iaccum);
}

DEFUN("-", Fminus, 1, MANY, 0,	/*
Negate number or subtract numbers, characters or markers.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.
				 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT iaccum;
	double daccum;
	Lisp_Object *args_end = args + nargs;
	int_or_double iod;

	number_char_or_marker_to_int_or_double(*args++, &iod);
	if (iod.int_p)
		iaccum = nargs > 1 ? iod.c.ival : -iod.c.ival;
	else {
		daccum = nargs > 1 ? iod.c.dval : -iod.c.dval;
		goto do_float;
	}

	while (args < args_end) {
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p)
			iaccum -= iod.c.ival;
		else {
			daccum = (double)iaccum - iod.c.dval;
			goto do_float;
		}
	}

	return make_int(iaccum);

      do_float:
	for (; args < args_end; args++)
		daccum -= number_char_or_marker_to_double(*args);
	return make_float(daccum);
}

DEFUN("*", Ftimes, 0, MANY, 0,	/*
Return product of any number of arguments.
The arguments should all be numbers, characters or markers.
				 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT iaccum = 1;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end) {
		int_or_double iod;
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p)
			iaccum *= iod.c.ival;
		else {
			double daccum = (double)iaccum * iod.c.dval;
			while (args < args_end)
				daccum *=
				    number_char_or_marker_to_double(*args++);
			return make_float(daccum);
		}
	}

	return make_int(iaccum);
}

DEFUN("/", Fquo, 1, MANY, 0,	/*
Return first argument divided by all the remaining arguments.
The arguments must be numbers, characters or markers.
With one argument, reciprocates the argument.
				 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT iaccum;
	double daccum;
	Lisp_Object *args_end = args + nargs;
	int_or_double iod;

	if (nargs == 1)
		iaccum = 1;
	else {
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p)
			iaccum = iod.c.ival;
		else {
			daccum = iod.c.dval;
			goto divide_floats;
		}
	}

	while (args < args_end) {
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p) {
			if (iod.c.ival == 0)
				goto divide_by_zero;
			iaccum /= iod.c.ival;
		} else {
			if (iod.c.dval == 0)
				goto divide_by_zero;
			daccum = (double)iaccum / iod.c.dval;
			goto divide_floats;
		}
	}

	return make_int(iaccum);

      divide_floats:
	for (; args < args_end; args++) {
		double dval = number_char_or_marker_to_double(*args);
		if (dval == 0)
			goto divide_by_zero;
		daccum /= dval;
	}
	return make_float(daccum);

      divide_by_zero:
	Fsignal(Qarith_error, Qnil);
	return Qnil;		/* not reached */
}

DEFUN("max", Fmax, 1, MANY, 0,	/*
Return largest of all the arguments.
All arguments must be numbers, characters or markers.
The value is always a number; markers and characters are converted
to numbers.
				 */
      (int nargs, Lisp_Object * args)) {
	EMACS_INT imax;
	double dmax;
	Lisp_Object *args_end = args + nargs;
	int_or_double iod;

	number_char_or_marker_to_int_or_double(*args++, &iod);
	if (iod.int_p)
		imax = iod.c.ival;
	else {
		dmax = iod.c.dval;
		goto max_floats;
	}

	while (args < args_end) {
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p) {
			if (imax < iod.c.ival)
				imax = iod.c.ival;
		} else {
			dmax = (double)imax;
			if (dmax < iod.c.dval)
				dmax = iod.c.dval;
			goto max_floats;
		}
	}

	return make_int(imax);

      max_floats:
	while (args < args_end) {
		double dval = number_char_or_marker_to_double(*args++);
		if (dmax < dval)
			dmax = dval;
	}
	return make_float(dmax);
}

DEFUN("min", Fmin, 1, MANY, 0,	/*
Return smallest of all the arguments.
All arguments must be numbers, characters or markers.
The value is always a number; markers and characters are converted
to numbers.
				 */
      (int nargs, Lisp_Object * args)) {
	EMACS_INT imin;
	double dmin;
	Lisp_Object *args_end = args + nargs;
	int_or_double iod;

	number_char_or_marker_to_int_or_double(*args++, &iod);
	if (iod.int_p)
		imin = iod.c.ival;
	else {
		dmin = iod.c.dval;
		goto min_floats;
	}

	while (args < args_end) {
		number_char_or_marker_to_int_or_double(*args++, &iod);
		if (iod.int_p) {
			if (imin > iod.c.ival)
				imin = iod.c.ival;
		} else {
			dmin = (double)imin;
			if (dmin > iod.c.dval)
				dmin = iod.c.dval;
			goto min_floats;
		}
	}

	return make_int(imin);

      min_floats:
	while (args < args_end) {
		double dval = number_char_or_marker_to_double(*args++);
		if (dmin > dval)
			dmin = dval;
	}
	return make_float(dmin);
}

DEFUN("logand", Flogand, 0, MANY, 0,	/*
Return bitwise-and of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
					 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT bits = ~0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end)
		bits &= integer_char_or_marker_to_int(*args++);

	return make_int(bits);
}

DEFUN("logior", Flogior, 0, MANY, 0,	/*
Return bitwise-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
					 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT bits = 0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end)
		bits |= integer_char_or_marker_to_int(*args++);

	return make_int(bits);
}

DEFUN("logxor", Flogxor, 0, MANY, 0,	/*
Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
					 */
      (int nargs, Lisp_Object * args))
{
	EMACS_INT bits = 0;
	Lisp_Object *args_end = args + nargs;

	while (args < args_end)
		bits ^= integer_char_or_marker_to_int(*args++);

	return make_int(bits);
}

DEFUN("lognot", Flognot, 1, 1, 0,	/*
Return the bitwise complement of NUMBER.
NUMBER may be an integer, marker or character converted to integer.
					 */
      (number))
{
	return make_int(~integer_char_or_marker_to_int(number));
}

DEFUN("%", Frem, 2, 2, 0,	/*
Return remainder of first arg divided by second.
Both must be integers, characters or markers.
				 */
      (number1, number2))
{
	EMACS_INT ival1 = integer_char_or_marker_to_int(number1);
	EMACS_INT ival2 = integer_char_or_marker_to_int(number2);

	if (ival2 == 0)
		Fsignal(Qarith_error, Qnil);

	return make_int(ival1 % ival2);
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

DEFUN("mod", Fmod, 2, 2, 0,	/*
Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers, characters or markers.
If either argument is a float, a float will be returned.
				 */
      (x, y))
{
	int_or_double iod1, iod2;
	number_char_or_marker_to_int_or_double(x, &iod1);
	number_char_or_marker_to_int_or_double(y, &iod2);

#ifdef LISP_FLOAT_TYPE
	if (!iod1.int_p || !iod2.int_p) {
		double dval1 = iod1.int_p ? (double)iod1.c.ival : iod1.c.dval;
		double dval2 = iod2.int_p ? (double)iod2.c.ival : iod2.c.dval;
		if (dval2 == 0)
			goto divide_by_zero;
		dval1 = fmod(dval1, dval2);

		/* If the "remainder" comes out with the wrong sign, fix it.  */
		if (dval2 < 0 ? dval1 > 0 : dval1 < 0)
			dval1 += dval2;

		return make_float(dval1);
	}
#endif				/* LISP_FLOAT_TYPE */
	{
		EMACS_INT ival;
		if (iod2.c.ival == 0)
			goto divide_by_zero;

		ival = iod1.c.ival % iod2.c.ival;

		/* If the "remainder" comes out with the wrong sign, fix it.  */
		if (iod2.c.ival < 0 ? ival > 0 : ival < 0)
			ival += iod2.c.ival;

		return make_int(ival);
	}

      divide_by_zero:
	Fsignal(Qarith_error, Qnil);
	return Qnil;		/* not reached */
}

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
	CHECK_INT_COERCE_CHAR(value);
	CONCHECK_INT(count);

	return make_int(XINT(count) > 0 ?
			XUINT(value) << XINT(count) :
			XUINT(value) >> -XINT(count));
}

DEFUN("1+", Fadd1, 1, 1, 0,	/*
Return NUMBER plus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
				 */
      (number))
{
      retry:

	if (INTP(number))
		return make_int(XINT(number) + 1);
	if (CHARP(number))
		return make_int(XCHAR(number) + 1);
	if (MARKERP(number))
		return make_int(marker_position(number) + 1);
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number))
		return make_float(XFLOAT_DATA(number) + 1.0);
#endif				/* LISP_FLOAT_TYPE */

	number = wrong_type_argument(Qnumber_char_or_marker_p, number);
	goto retry;
}

DEFUN("1-", Fsub1, 1, 1, 0,	/*
Return NUMBER minus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
				 */
      (number))
{
      retry:

	if (INTP(number))
		return make_int(XINT(number) - 1);
	if (CHARP(number))
		return make_int(XCHAR(number) - 1);
	if (MARKERP(number))
		return make_int(marker_position(number) - 1);
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number))
		return make_float(XFLOAT_DATA(number) - 1.0);
#endif				/* LISP_FLOAT_TYPE */

	number = wrong_type_argument(Qnumber_char_or_marker_p, number);
	goto retry;
}

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

void init_errors_once_early(void)
{
	DEFSYMBOL(Qerror_conditions);
	DEFSYMBOL(Qerror_message);

	/* We declare the errors here because some other deferrors depend
	   on some of the errors below. */

	/* ERROR is used as a signaler for random errors for which nothing
	   else is right */

	DEFERROR(Qerror, "error", Qnil);
	DEFERROR_STANDARD(Qquit, Qnil);

	DEFERROR(Qunimplemented, "Feature not yet implemented", Qerror);
	DEFERROR_STANDARD(Qsyntax_error, Qerror);
	DEFERROR_STANDARD(Qinvalid_read_syntax, Qsyntax_error);
	DEFERROR_STANDARD(Qlist_formation_error, Qsyntax_error);

	/* Generated by list traversal macros */
	DEFERROR_STANDARD(Qmalformed_list, Qlist_formation_error);
	DEFERROR_STANDARD(Qmalformed_property_list, Qmalformed_list);
	DEFERROR_STANDARD(Qcircular_list, Qlist_formation_error);
	DEFERROR_STANDARD(Qcircular_property_list, Qcircular_list);

	DEFERROR_STANDARD(Qinvalid_argument, Qerror);
	DEFERROR_STANDARD(Qwrong_type_argument, Qinvalid_argument);
	DEFERROR_STANDARD(Qargs_out_of_range, Qinvalid_argument);
	DEFERROR_STANDARD(Qwrong_number_of_arguments, Qinvalid_argument);
	DEFERROR_STANDARD(Qinvalid_function, Qinvalid_argument);
	DEFERROR(Qno_catch, "No catch for tag", Qinvalid_argument);

	DEFERROR_STANDARD(Qinternal_error, Qerror);

	DEFERROR(Qinvalid_state,
		 "Properties or values have been set incorrectly", Qerror);
	DEFERROR(Qvoid_function, "Symbol's function definition is void",
		 Qinvalid_state);
	DEFERROR(Qcyclic_function_indirection,
		 "Symbol's chain of function indirections contains a loop",
		 Qinvalid_state);
	DEFERROR(Qvoid_variable, "Symbol's value as variable is void",
		 Qinvalid_state);
	DEFERROR(Qcyclic_variable_indirection,
		 "Symbol's chain of variable indirections contains a loop",
		 Qinvalid_state);

	DEFERROR(Qinvalid_operation,
		 "Operation not allowed or error during operation", Qerror);
	DEFERROR(Qinvalid_change,
		 "Attempt to set properties or values incorrectly",
		 Qinvalid_operation);
	DEFERROR(Qsetting_constant, "Attempt to set a constant symbol",
		 Qinvalid_change);

	DEFERROR(Qediting_error, "Invalid operation during editing",
		 Qinvalid_operation);
	DEFERROR_STANDARD(Qbeginning_of_buffer, Qediting_error);
	DEFERROR_STANDARD(Qend_of_buffer, Qediting_error);
	DEFERROR(Qbuffer_read_only, "Buffer is read-only", Qediting_error);

	DEFERROR(Qio_error, "IO Error", Qinvalid_operation);
	DEFERROR(Qend_of_file, "End of file or stream", Qio_error);

	DEFERROR(Qarith_error, "Arithmetic error", Qinvalid_operation);
	DEFERROR(Qrange_error, "Arithmetic range error", Qarith_error);
	DEFERROR(Qdomain_error, "Arithmetic domain error", Qarith_error);
	DEFERROR(Qsingularity_error, "Arithmetic singularity error",
		 Qdomain_error);
	DEFERROR(Qoverflow_error, "Arithmetic overflow error", Qdomain_error);
	DEFERROR(Qunderflow_error, "Arithmetic underflow error", Qdomain_error);
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
	DEFSYMBOL(Qstringp);
	DEFSYMBOL(Qarrayp);
	DEFSYMBOL(Qsequencep);
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

#ifdef LISP_FLOAT_TYPE
	DEFSYMBOL(Qfloatp);
#endif				/* LISP_FLOAT_TYPE */

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
	DEFSUBR(Fintegerp);
	DEFSUBR(Finteger_or_marker_p);
	DEFSUBR(Finteger_or_char_p);
	DEFSUBR(Finteger_char_or_marker_p);
	DEFSUBR(Fnumberp);
	DEFSUBR(Fnumber_or_marker_p);
	DEFSUBR(Fnumber_char_or_marker_p);
#ifdef LISP_FLOAT_TYPE
	DEFSUBR(Ffloatp);
#endif				/* LISP_FLOAT_TYPE */
	DEFSUBR(Fnatnump);
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
	DEFSUBR(Feqlsign);
	DEFSUBR(Flss);
	DEFSUBR(Fgtr);
	DEFSUBR(Fleq);
	DEFSUBR(Fgeq);
	DEFSUBR(Fneq);
	DEFSUBR(Fzerop);
	DEFSUBR(Fplus);
	DEFSUBR(Fminus);
	DEFSUBR(Ftimes);
	DEFSUBR(Fquo);
	DEFSUBR(Frem);
	DEFSUBR(Fmod);
	DEFSUBR(Fmax);
	DEFSUBR(Fmin);
	DEFSUBR(Flogand);
	DEFSUBR(Flogior);
	DEFSUBR(Flogxor);
	DEFSUBR(Flsh);
	DEFSUBR(Fash);
	DEFSUBR(Fadd1);
	DEFSUBR(Fsub1);
	DEFSUBR(Flognot);

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
