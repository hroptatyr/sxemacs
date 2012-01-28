/* Generic selection processing for SXEmacs
   Copyright (C) 1999 Free Software Foundation, Inc.
   Copyright (C) 1999 Andy Piper.

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


/* Synched up with: Not synched with FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "extents.h"
#include "console.h"
#include "objects.h"

#include "frame.h"
#include "opaque.h"
#include "select.h"

/* X Atoms */
Lisp_Object QPRIMARY, QSECONDARY, QSTRING, QINTEGER, QCLIPBOARD, QTIMESTAMP,
    QTEXT, QDELETE, QMULTIPLE, QINCR, QEMACS_TMP, QTARGETS, QATOM, QNULL,
    QATOM_PAIR, QCOMPOUND_TEXT;

/* Selection strategy symbols */
Lisp_Object Qreplace_all, Qreplace_existing;

/* "Selection owner couldn't convert selection" */
Lisp_Object Qselection_conversion_error;

/* A couple of Lisp functions */
Lisp_Object Qselect_convert_in, Qselect_convert_out, Qselect_coerce;

/* These are alists whose CARs are selection-types (whose names are the same
   as the names of X Atoms or Windows clipboard formats) and whose CDRs are
   the names of Lisp functions to call to convert the given Emacs selection
   value to a string representing the given selection type.  This is for
   elisp-level extension of the emacs selection handling.
 */
Lisp_Object Vselection_converter_out_alist;
Lisp_Object Vselection_converter_in_alist;
Lisp_Object Vselection_coercion_alist;
Lisp_Object Vselection_appender_alist;
Lisp_Object Vselection_buffer_killed_alist;
Lisp_Object Vselection_coercible_types;

Lisp_Object Vlost_selection_hooks;

/* This is an association list whose elements are of the form
     ( selection-name selection-value selection-timestamp )
   selection-name is a lisp symbol, whose name is the name of an X Atom.
   selection-value is a list of cons pairs that emacs owns for that selection.
     Each pair consists of (type . value), where type is nil or a
     selection data type, and value is any type of Lisp object.
   selection-timestamp is the time at which emacs began owning this selection,
     as a cons of two 16-bit numbers (making a 32 bit time).
   If there is an entry in this alist, then it can be assumed that emacs owns
    that selection.
   The only (eq) parts of this list that are visible from elisp are the
    selection-values.
 */
Lisp_Object Vselection_alist;

/* Given a selection-name and desired type, this looks up our local copy of
   the selection value and converts it to the type. */
static Lisp_Object
get_local_selection(Lisp_Object selection_symbol, Lisp_Object target_type)
{
	Lisp_Object local_value =
	    assq_no_quit(selection_symbol, Vselection_alist);

	if (!NILP(local_value)) {
		Lisp_Object value_list = XCAR(XCDR(local_value));
		Lisp_Object value;

		/* First try to find an entry of the appropriate type */
		value = assq_no_quit(target_type, value_list);

		if (!NILP(value))
			return XCDR(value);
	}

	return Qnil;
}

/* #### Should perhaps handle 'MULTIPLE. The code below is now completely
   broken due to a re-organization of get_local_selection, but I've left
   it here should anyone show an interest - ajh */
#if 0
else
if (CONSP(target_type) && XCAR(target_type) == QMULTIPLE) {
Lisp_Object pairs = XCDR(target_type);
int len = XVECTOR_LENGTH(pairs);
int i;
	  /* If the target is MULTIPLE, then target_type looks like
	     (MULTIPLE . [[SELECTION1 TARGET1] [SELECTION2 TARGET2] ... ])
	     We modify the second element of each pair in the vector and
	     return it as [[SELECTION1 <value1>] [SELECTION2 <value2>] ... ]
	   */
for (i = 0; i < len; i++) {
	Lisp_Object pair = XVECTOR_DATA(pairs)[i];
	XVECTOR_DATA(pair)[1] =
	    x_get_local_selection(XVECTOR_DATA(pair)[0], XVECTOR_DATA(pair)[1]);
}
return pairs;
}
#endif

DEFUN("own-selection-internal", Fown_selection_internal, 2, 5, 0,	/*
Give the selection SELECTION-NAME the value SELECTION-VALUE.
SELECTION-NAME is a symbol, typically PRIMARY, SECONDARY, or CLIPBOARD.
SELECTION-VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on selection-converter-out-alist know about.
Optional arg HOW-TO-ADD specifies how the selection will be combined
with any existing selection(s) - see `own-selection' for more
information.
Optional arg DATA-TYPE is a window-system-specific type.
Optional arg DEVICE specifies the device on which to assert the selection.
It defaults to the selected device.
*/
      (selection_name, selection_value, how_to_add, data_type, device))
{
	Lisp_Object selection_time, selection_data, prev_value = Qnil,
	    value_list = Qnil;
	Lisp_Object prev_real_value = Qnil;
	struct gcpro gcpro1;
	int owned_p = 0;

	CHECK_SYMBOL(selection_name);
	if (NILP(selection_value))
		error("selection-value may not be nil.");

	if (NILP(device))
		device = Fselected_device(Qnil);

	if (!EQ(how_to_add, Qappend) && !EQ(how_to_add, Qt)
	    && !EQ(how_to_add, Qreplace_existing)
	    && !EQ(how_to_add, Qreplace_all) && !NILP(how_to_add))
		error("how-to-add must be nil, append, replace_all, "
		      "replace_existing or t.");

#ifdef MULE
	if (NILP(data_type))
		data_type = QCOMPOUND_TEXT;
#else
	if (NILP(data_type))
		data_type = QSTRING;
#endif

	/* Examine the how-to-add argument */
	if (EQ(how_to_add, Qreplace_all) || NILP(how_to_add)) {
		Lisp_Object local_selection_data = assq_no_quit(selection_name,
								Vselection_alist);

		if (!NILP(local_selection_data)) {
			owned_p = 1;
			/* Don't use Fdelq() as that may QUIT;. */
			if (EQ(local_selection_data, Fcar(Vselection_alist)))
				Vselection_alist = Fcdr(Vselection_alist);
			else {
				Lisp_Object rest;
				for (rest = Vselection_alist; !NILP(rest);
				     rest = Fcdr(rest))
					if (EQ
					    (local_selection_data,
					     Fcar(XCDR(rest)))) {
						XCDR(rest) = Fcdr(XCDR(rest));
						break;
					}
			}
		}
	} else {
		/* Look for a previous value */
		prev_value = assq_no_quit(selection_name, Vselection_alist);

		if (!NILP(prev_value)) {
			owned_p = 1;
			value_list = XCAR(XCDR(prev_value));
		}

		if (!NILP(value_list))
			prev_real_value = assq_no_quit(data_type, value_list);
	}

	/* Append values if necessary */
	if (!NILP(value_list)
	    && (EQ(how_to_add, Qappend) || EQ(how_to_add, Qt))) {
		/* Did we have anything of this type previously? */
		if (!NILP(prev_real_value)) {
			if ((NILP(data_type) && STRINGP(selection_value)
			     && STRINGP(XCDR(prev_real_value)))
			    || !NILP(data_type)) {
				Lisp_Object function = assq_no_quit(data_type,
								    Vselection_appender_alist);

				if (NILP(function))
					error
					    ("cannot append selections of supplied types.");

				function = XCDR(function);

				selection_value = call4(function,
							selection_name,
							data_type,
							XCDR(prev_real_value),
							selection_value);

				if (NILP(selection_value))
					error
					    ("cannot append selections of supplied types.");
			} else
				error
				    ("cannot append selections of supplied types.");
		}

		selection_data = Fcons(data_type, selection_value);
		value_list = Fcons(selection_data, value_list);
	}

	if (!NILP(prev_real_value)) {
		Lisp_Object rest;	/* We know it isn't the CAR, so it's easy. */

		/* Delete the old type entry from the list */
		for (rest = value_list; !NILP(rest); rest = Fcdr(rest))
			if (EQ(prev_real_value, Fcar(XCDR(rest)))) {
				XCDR(rest) = Fcdr(XCDR(rest));
				break;
			}
	} else {
		value_list = Fcons(Fcons(data_type, selection_value),
				   value_list);
	}

	/* Complete the local cache update; note that we destructively
	   modify the current list entry if there is one */
	if (NILP(prev_value)) {
		selection_data = list3(selection_name, value_list, Qnil);
		Vselection_alist = Fcons(selection_data, Vselection_alist);
	} else {
		selection_data = prev_value;
		Fsetcar(XCDR(selection_data), value_list);
	}

	GCPRO1(selection_data);

	/* have to do device specific stuff last so that methods can access the
	   selection_alist */
	if (HAS_DEVMETH_P(XDEVICE(device), own_selection))
		selection_time = DEVMETH(XDEVICE(device), own_selection,
					 (selection_name, selection_value,
					  how_to_add, data_type, owned_p));
	else
		selection_time = Qnil;

	Fsetcar(XCDR(XCDR(selection_data)), selection_time);

	UNGCPRO;

	return selection_value;
}

DEFUN("register-selection-data-type", Fregister_selection_data_type, 1, 2, 0,	/*
Register a new selection data type DATA-TYPE, optionally on the specified
DEVICE. Returns the device-specific data type identifier, or nil if the
device does not support this feature or the registration fails.
*/
      (data_type, device))
{
	/* Check arguments */
	CHECK_STRING(data_type);

	if (NILP(device))
		device = Fselected_device(Qnil);

	if (HAS_DEVMETH_P(XDEVICE(device), register_selection_data_type))
		return DEVMETH(XDEVICE(device), register_selection_data_type,
			       (data_type));
	else
		return Qnil;
}

DEFUN("selection-data-type-name", Fselection_data_type_name, 1, 2, 0,	/*
Retrieve the name of the specified selection data type DATA-TYPE, optionally
on the specified DEVICE. Returns either a string or a symbol on success, and
nil if the device does not support this feature or the type is not known.
*/
      (data_type, device))
{
	if (NILP(device))
		device = Fselected_device(Qnil);

	if (HAS_DEVMETH_P(XDEVICE(device), selection_data_type_name))
		return DEVMETH(XDEVICE(device), selection_data_type_name,
			       (data_type));
	else
		return Qnil;
}

DEFUN("available-selection-types", Favailable_selection_types, 1, 2, 0,	/*
Retrieve a list of currently available types of selection associated with
the given SELECTION-NAME, optionally on the specified DEVICE. This list
does not take into account any possible conversions that might take place,
so it should be taken as a minimal estimate of what is available.
*/
      (selection_name, device))
{
	Lisp_Object types = Qnil, rest;
	struct gcpro gcpro1;

	CHECK_SYMBOL(selection_name);

	if (NILP(device))
		device = Fselected_device(Qnil);

	GCPRO1(types);

	/* First check the device */
	if (HAS_DEVMETH_P(XDEVICE(device), available_selection_types))
		types = DEVMETH(XDEVICE(device), available_selection_types,
				(selection_name));

	/* Now look in the list */
	rest = assq_no_quit(selection_name, Vselection_alist);

	if (NILP(rest)) {
		UNGCPRO;

		return types;
	}

	/* Examine the types and cons them onto the front of the list */
	for (rest = XCAR(XCDR(rest)); !NILP(rest); rest = XCDR(rest)) {
		Lisp_Object value = XCDR(XCAR(rest));
		Lisp_Object type = XCAR(XCAR(rest));

		types = Fcons(type, types);

		if ((STRINGP(value) || EXTENTP(value))
		    && (NILP(type) || EQ(type, QSTRING)
			|| EQ(type, QTEXT) || EQ(type, QCOMPOUND_TEXT)))
			types =
			    Fcons(QTEXT,
				  Fcons(QCOMPOUND_TEXT, Fcons(QSTRING, types)));
		else if (INTP(value) && NILP(type))
			types = Fcons(QINTEGER, types);
		else if (SYMBOLP(value) && NILP(type))
			types = Fcons(QATOM, types);
	}

	UNGCPRO;

	return types;
}

/* remove a selection from our local copy
 */
void handle_selection_clear(Lisp_Object selection_symbol)
{
	Lisp_Object local_selection_data = assq_no_quit(selection_symbol,
							Vselection_alist);

	/* Well, we already believe that we don't own it, so that's just fine. */
	if (NILP(local_selection_data))
		return;

	/* Otherwise, we're really honest and truly being told to drop it.
	   Don't use Fdelq() as that may QUIT;.
	 */
	if (EQ(local_selection_data, Fcar(Vselection_alist)))
		Vselection_alist = Fcdr(Vselection_alist);
	else {
		Lisp_Object rest;
		for (rest = Vselection_alist; !NILP(rest); rest = Fcdr(rest))
			if (EQ(local_selection_data, Fcar(XCDR(rest)))) {
				XCDR(rest) = Fcdr(XCDR(rest));
				break;
			}
	}

	/* Let random lisp code notice that the selection has been stolen.
	 */
	{
		Lisp_Object rest;
		Lisp_Object val = Vlost_selection_hooks;
		if (!UNBOUNDP(val) && !NILP(val)) {
			if (CONSP(val) && !EQ(XCAR(val), Qlambda))
				for (rest = val; !NILP(rest); rest = Fcdr(rest))
					call1(Fcar(rest), selection_symbol);
			else
				call1(val, selection_symbol);
		}
	}
}

DEFUN("disown-selection-internal", Fdisown_selection_internal, 1, 3, 0,	/*
If we own the named selection, then disown it (make there be no selection).
*/
      (selection_name, selection_time, device))
{
	if (NILP(assq_no_quit(selection_name, Vselection_alist)))
		return Qnil;	/* Don't disown the selection when we're not the owner. */

	if (NILP(device))
		device = Fselected_device(Qnil);

	MAYBE_DEVMETH(XDEVICE(device), disown_selection,
		      (selection_name, selection_time));

	handle_selection_clear(selection_name);

	return Qt;
}

DEFUN("selection-owner-p", Fselection_owner_p, 0, 1, 0,	/*
Return t if the current emacs process owns SELECTION.
SELECTION should be the name of the selection in question, typically one of
the symbols PRIMARY, SECONDARY, or CLIPBOARD.  (For convenience, the symbol
nil is the same as PRIMARY, and t is the same as SECONDARY.)
*/
      (selection))
{
	CHECK_SYMBOL(selection);
	if (EQ(selection, Qnil))
		selection = QPRIMARY;
	else if (EQ(selection, Qt))
		selection = QSECONDARY;

	return NILP(Fassq(selection, Vselection_alist)) ? Qnil : Qt;
}

DEFUN("selection-exists-p", Fselection_exists_p, 0, 3, 0,	/*
Whether there is currently an owner for SELECTION.
SELECTION should be the name of the selection in question, typically one of
the symbols PRIMARY, SECONDARY, or CLIPBOARD.  (For convenience, the symbol
nil is the same as PRIMARY, and t is the same as SECONDARY.)
Optionally, the window-system DATA-TYPE and the DEVICE may be specified.
*/
      (selection, data_type, device))
{
	CHECK_SYMBOL(selection);
	if (NILP(data_type)
	    && !NILP(Fselection_owner_p(selection)))
		return Qt;

	if (NILP(device))
		device = Fselected_device(Qnil);

	return HAS_DEVMETH_P(XDEVICE(device), selection_exists_p) ?
	    DEVMETH(XDEVICE(device), selection_exists_p, (selection, data_type))
	    : Qnil;
}

/* Get the timestamp of the given selection */
DEFUN("get-selection-timestamp", Fget_selection_timestamp, 1, 1, 0,	/*
Return the timestamp associated with the specified SELECTION, if it exists.
Note that the timestamp is a device-specific object, and may not actually be
visible from Lisp.
*/
      (selection))
{
	Lisp_Object local_value = assq_no_quit(selection, Vselection_alist);

	if (!NILP(local_value))
		return XCAR(XCDR(XCDR(local_value)));

	return Qnil;
}

/* Request the selection value from the owner.  If we are the owner,
   simply return our selection value.  If we are not the owner, this
   will block until all of the data has arrived.
 */
DEFUN("get-selection-internal", Fget_selection_internal, 2, 3, 0,	/*
Return text selected from some window-system window.
SELECTION is a symbol, typically PRIMARY, SECONDARY, or CLIPBOARD.
TARGET-TYPE is the type of data desired, typically STRING or COMPOUND_TEXT.
Under Mule, if the resultant data comes back as 8-bit data in type
TEXT or COMPOUND_TEXT, it will be decoded as Compound Text.
*/
      (selection, target_type, device))
{
	/* This function can GC */
	Lisp_Object val = Qnil;
	struct gcpro gcpro1, gcpro2;
	GCPRO2(target_type, val);
	CHECK_SYMBOL(selection);

	if (NILP(device))
		device = Fselected_device(Qnil);

#ifdef MULE
	if (NILP(target_type))
		target_type = QCOMPOUND_TEXT;
#else
	if (NILP(target_type))
		target_type = QSTRING;
#endif

#if 0				/* #### MULTIPLE doesn't work yet and probably never will */
	if (CONSP(target_type) && XCAR(target_type) == QMULTIPLE) {
		CHECK_VECTOR(XCDR(target_type));
		/* So we don't destructively modify this... */
		target_type = copy_multiple_data(target_type);
	}
#endif

	/* Used to check that target_type was a symbol. This is no longer
	   necessarily the case, because the type might be registered with
	   the device (in which case target_type would be a device-specific
	   identifier - probably an integer) - ajh */

	val = get_local_selection(selection, target_type);

	if (!NILP(val)) {
		/* If we get something from the local cache, we may need to convert
		   it slightly - to do this, we call select-coerce */
		val = call3(Qselect_coerce, selection, target_type, val);
	} else if (HAS_DEVMETH_P(XDEVICE(device), get_foreign_selection)) {
		/* Nothing in the local cache; try the window system */
		val = DEVMETH(XDEVICE(device), get_foreign_selection,
			      (selection, target_type));
	}

	if (NILP(val)) {
		/* Still nothing. Try coercion. */

		/* Try looking in selection-coercible-types to see if any of
		   them are present for this selection. We try them *in order*;
		   the first for which a conversion succeeds gets returned. */
		EXTERNAL_LIST_LOOP_2(element, Vselection_coercible_types) {
			val = get_local_selection(selection, element);

			if (NILP(val))
				continue;

			val =
			    call3(Qselect_coerce, selection, target_type, val);

			if (!NILP(val))
				break;
		}
	}

	/* Used to call clean_local_selection here... but that really belonged
	   in Lisp (so the equivalent is now built-in to the INTEGER conversion
	   function select-convert-from-integer) - ajh */

	UNGCPRO;
	return val;
}

/* These are convenient interfaces to the lisp code in select.el;
   this way we can rename them easily rather than having to hunt everywhere.
   Also, this gives us access to get_local_selection so that convert_out
   can retrieve the internal selection value automatically if passed a
   value of Qnil. */
Lisp_Object
select_convert_in(Lisp_Object selection, Lisp_Object type, Lisp_Object value)
{
	return call3(Qselect_convert_in, selection, type, value);
}

Lisp_Object
select_coerce(Lisp_Object selection, Lisp_Object type, Lisp_Object value)
{
	return call3(Qselect_coerce, selection, type, value);
}

Lisp_Object
select_convert_out(Lisp_Object selection, Lisp_Object type, Lisp_Object value)
{
	if (NILP(value))
		value = get_local_selection(selection, type);

	if (NILP(value)) {
		/* Try looking in selection-coercible-types to see if any of
		   them are present for this selection. We try them *in order*;
		   the first for which a conversion succeeds gets returned. */
		EXTERNAL_LIST_LOOP_2(element, Vselection_coercible_types) {
			Lisp_Object ret;

			value = get_local_selection(selection, element);

			if (NILP(value))
				continue;

			ret =
			    call3(Qselect_convert_out, selection, type, value);

			if (!NILP(ret))
				return ret;
		}

		return Qnil;
	}

	return call3(Qselect_convert_out, selection, type, value);
}

/* Gets called from kill-buffer; this lets us dispose of buffer-dependent
   selections (or alternatively make them independent of the buffer) when
   it gets vaped. */
void select_notify_buffer_kill(Lisp_Object buffer)
{
	Lisp_Object rest;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/* For each element of Vselection_alist */
	for (rest = Vselection_alist; !NILP(rest);) {
		Lisp_Object selection, values, prev = Qnil;

		selection = XCAR(rest);

		for (values = XCAR(XCDR(selection));
		     !NILP(values); values = XCDR(values)) {
			Lisp_Object value, handler_fn;

			/* Extract the (type . value) pair. */
			value = XCAR(values);

			/* Find the handler function (if any). */
			handler_fn = Fcdr(Fassq(XCAR(value),
						Vselection_buffer_killed_alist));

			if (!NILP(handler_fn)) {
				Lisp_Object newval;

				/* Protect ourselves, just in case some tomfool calls
				   own-selection from with the buffer-killed handler, then
				   causes a GC. Just as a note, *don't do this*. */
				GCPRO3(rest, values, value);

				newval =
				    call4(handler_fn, XCAR(selection),
					  XCAR(value), XCDR(value), buffer);

				UNGCPRO;

				/* Set or delete the value (by destructively modifying
				   the list). */
				if (!NILP(newval)) {
					Fsetcdr(value, newval);

					prev = values;
				} else {
					if (NILP(prev))
						Fsetcar(XCDR(selection),
							XCDR(values));
					else
						Fsetcdr(prev, XCDR(values));
				}
			} else
				prev = values;
		}

		/* If we have no values for this selection */
		if (NILP(XCAR(XCDR(selection)))) {
			/* Move on to the next element *first* */
			rest = XCDR(rest);

			/* Protect it and disown this selection */
			GCPRO1(rest);

			Fdisown_selection_internal(XCAR(selection), Qnil, Qnil);

			UNGCPRO;
		} else
			rest = XCDR(rest);
	}
}

void syms_of_select(void)
{
	DEFSUBR(Fown_selection_internal);
	DEFSUBR(Fget_selection_internal);
	DEFSUBR(Fget_selection_timestamp);
	DEFSUBR(Fselection_exists_p);
	DEFSUBR(Fdisown_selection_internal);
	DEFSUBR(Fselection_owner_p);
	DEFSUBR(Favailable_selection_types);
	DEFSUBR(Fregister_selection_data_type);
	DEFSUBR(Fselection_data_type_name);

	/* Lisp Functions */
	defsymbol(&Qselect_convert_in, "select-convert-in");
	defsymbol(&Qselect_convert_out, "select-convert-out");
	defsymbol(&Qselect_coerce, "select-coerce");

	/* X Atoms */
	defsymbol(&QPRIMARY, "PRIMARY");
	defsymbol(&QSECONDARY, "SECONDARY");
	defsymbol(&QSTRING, "STRING");
	defsymbol(&QINTEGER, "INTEGER");
	defsymbol(&QCLIPBOARD, "CLIPBOARD");
	defsymbol(&QTIMESTAMP, "TIMESTAMP");
	defsymbol(&QTEXT, "TEXT");
	defsymbol(&QDELETE, "DELETE");
	defsymbol(&QMULTIPLE, "MULTIPLE");
	defsymbol(&QINCR, "INCR");
	defsymbol(&QEMACS_TMP, "_EMACS_TMP_");
	defsymbol(&QTARGETS, "TARGETS");
	defsymbol(&QATOM, "ATOM");
	defsymbol(&QATOM_PAIR, "ATOM_PAIR");
	defsymbol(&QCOMPOUND_TEXT, "COMPOUND_TEXT");
	defsymbol(&QNULL, "NULL");

	/* Selection strategies */
	defsymbol(&Qreplace_all, "replace-all");
	defsymbol(&Qreplace_existing, "replace-existing");

	DEFERROR_STANDARD(Qselection_conversion_error, Qio_error);
}

void vars_of_select(void)
{
	Vselection_alist = Qnil;
	staticpro(&Vselection_alist);

	DEFVAR_LISP("selection-converter-alist", &Vselection_converter_out_alist	/*
An alist associating selection-types (such as STRING and TIMESTAMP) with
functions.  This is an alias for `selection-converter-out-alist', and should
be considered obsolete.  Use the new name instead. */ );

	DEFVAR_LISP("selection-converter-out-alist", &Vselection_converter_out_alist	/*
An alist associating selection-types (such as STRING and TIMESTAMP) with
functions.  These functions will be called with three args: the name
of the selection (typically PRIMARY, SECONDARY, or CLIPBOARD); a
desired type to which the selection should be converted; and the local
selection value (whatever had been passed to `own-selection').

The return type of these functions depends upon the device in question;
for X, the return value should be one of:

-- nil (the conversion could not be done)
-- a cons of a symbol and any of the following values; the symbol
explicitly specifies the type that will be sent.
-- a string (If the type is not specified, then if Mule support exists,
the string will be converted to Compound Text and sent in
the 'COMPOUND_TEXT format; otherwise (no Mule support),
the string will be left as-is and sent in the 'STRING
format.  If the type is specified, the string will be
left as-is (or converted to binary format under Mule).
In all cases, 8-bit data it sent.)
-- a character (With Mule support, will be converted to Compound Text
whether or not a type is specified.  If a type is not
specified, a type of 'STRING or 'COMPOUND_TEXT will be
sent, as for strings.)
-- the symbol 'NULL (Indicates that there is no meaningful return value.
Empty 32-bit data with a type of 'NULL will be sent.)
-- a symbol (Will be converted into an atom.  If the type is not specified,
a type of 'ATOM will be sent.)
-- an integer (Will be converted into a 16-bit or 32-bit integer depending
on the value.  If the type is not specified, a type of
'INTEGER will be sent.)
-- a cons (HIGH . LOW) of integers (Will be converted into a 32-bit integer.
If the type is not specified, a type of
'INTEGER will be sent.)
-- a vector of symbols (Will be converted into a list of atoms.  If the type
is not specified, a type of 'ATOM will be sent.)
-- a vector of integers (Will be converted into a list of 16-bit integers.
If the type is not specified, a type of 'INTEGER
will be sent.)
-- a vector of integers and/or conses (HIGH . LOW) of integers
(Will be converted into a list of 16-bit integers.
If the type is not specified, a type of 'INTEGER
will be sent.)
											 */ );
	Vselection_converter_out_alist = Qnil;

	DEFVAR_LISP("selection-converter-in-alist", &Vselection_converter_in_alist	/*
An alist associating selection-types (such as STRING and TIMESTAMP) with
functions.  These functions will be called with three args: the name
of the selection (typically PRIMARY, SECONDARY or CLIPBOARD); the
type from which the selection should be converted; and the selection
value.  These functions should return a suitable representation of the
value, or nil to indicate that the conversion was not possible.

See also `selection-converter-out-alist'.
											*/ );
	Vselection_converter_in_alist = Qnil;

	DEFVAR_LISP("selection-coercion-alist", &Vselection_coercion_alist	/*
An alist associating selection-types (such as STRING and TIMESTAMP) with
functions.  These functions will be called with three args; the name
of the selection (typically PRIMARY, SECONDARY or CLIPBOARD); the type
from which the selection should be converted, and the selection value.
The value passed will be *exactly the same value* that was given to
`own-selection'; it should be converted into something suitable for
return to a program calling `get-selection' with the appropriate
parameters.

See also `selection-converter-in-alist' and `selection-converter-out-alist'.
*/ );
	Vselection_coercion_alist = Qnil;

	DEFVAR_LISP("selection-appender-alist", &Vselection_appender_alist	/*
An alist associating selection-types (such as STRING and TIMESTAMP) with
functions.  These functions will be called with four args; the name
of the selection (typically PRIMARY, SECONDARY or CLIPBOARD); the type
of the selection; and two selection values.  The functions are expected to
return a value representing the catenation of the two values, or nil to
indicate that this was not possible.
*/ );
	Vselection_appender_alist = Qnil;

	DEFVAR_LISP("selection-buffer-killed-alist", &Vselection_buffer_killed_alist	/*
An alist associating selection-types (such as STRING and TIMESTAMP) with
functions.  These functions will be called whenever a buffer is killed,
with four args: the name of the selection (typically PRIMARY, SECONDARY
or CLIPBOARD); the type of the selection; the value of the selection; and
the buffer that has just been killed.  These functions should return a new
selection value, or nil to indicate that the selection value should be
deleted.
*/ );
	Vselection_buffer_killed_alist = Qnil;

	DEFVAR_LISP("selection-coercible-types", &Vselection_coercible_types	/*
A list of selection types that are coercible---that is, types that may be
automatically converted to another type. Selection values with types in this
list may be subject to conversion attempts to other types.
*/
		    );
	Vselection_coercible_types = Qnil;

	DEFVAR_LISP("lost-selection-hooks", &Vlost_selection_hooks	/*
A function or functions to be called after we have been notified
that we have lost the selection.  The function(s) will be called with one
argument, a symbol naming the selection (typically PRIMARY, SECONDARY, or
CLIPBOARD).
									 */ );
	Vlost_selection_hooks = Qunbound;
}
