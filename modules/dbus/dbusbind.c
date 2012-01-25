/* dbusbind.c -- Elisp bindings for D-Bus. */

/*
 * Time-stamp: <Wednesday Jan 25, 2012 13:32:16 steve>
 * Created:    <2012-01-03>
 * Maintainer: Steve Youngs <steve@sxemacs.org>
 * Homepage:   http://www.sxemacs.org/
 */

/*
 * Copyright (C) 2007-2011 Free Software Foundation, Inc.
 * Copyright (C) 2012 Steve Youngs
 * Copyright (C) 2012 Nelson Ferreira
 */

/*
 * This file is part of SXEmacs.

 * SXEmacs is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * SXEmacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <config.h>

#ifdef HAVE_DBUS
#include <stdio.h>
#include <dbus/dbus.h>
#include <setjmp.h>
#include "lisp.h"
#include "ui/frame.h"
#include "process.h"
#include "elhash.h"
#include "dbusbind.h"

/*
 * #include "termhooks.h"
 * #include "keyboard.h"
 */


/*
 * Stuff lifted from GNU/Emacs to let this work
 * This stuff needs to be ported and then removed!!
 */

/* Internal version of Fsignal that never returns.
   Used for anything but Qquit (which can return from Fsignal).  */

void
xsignal (Lisp_Object error_symbol, Lisp_Object data)
{
	Fsignal (error_symbol, data);
	abort ();
}

/* Like xsignal, but takes 0, 1, 2, or 3 args instead of a list.  */

void
xsignal0 (Lisp_Object error_symbol)
{
	xsignal (error_symbol, Qnil);
}

void
xsignal1 (Lisp_Object error_symbol, Lisp_Object arg)
{
	xsignal (error_symbol, list1 (arg));
}

void
xsignal2 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2)
{
	xsignal (error_symbol, list2 (arg1, arg2));
}

void
xsignal3 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
	xsignal (error_symbol, list3 (arg1, arg2, arg3));
}

Lisp_Object
format2 (const char *string1, Lisp_Object arg0, Lisp_Object arg1)
{
	Lisp_Object args[3];
	args[0] = build_string (string1);
	args[1] = arg0;
	args[2] = arg1;
	return Fformat (3, args);
}

/* End lifted from GNU */

/* Whether we are reading a D-Bus event.  */
static int xd_in_read_queued_messages = 0;


/* This was a macro.  On Solaris 2.11 it was said to compile for
   hours, when optimization is enabled.  So we have transferred it into
   a function.  */
/* Determine the DBusType of a given Lisp symbol.  OBJECT must be one
   of the predefined D-Bus type symbols.  */
static int
xd_symbol_to_dbus_type (Lisp_Object object)
{
	return
		((EQ (object, QCdbus_type_byte)) ? DBUS_TYPE_BYTE
		 : (EQ (object, QCdbus_type_boolean)) ? DBUS_TYPE_BOOLEAN
		 : (EQ (object, QCdbus_type_int16)) ? DBUS_TYPE_INT16
		 : (EQ (object, QCdbus_type_uint16)) ? DBUS_TYPE_UINT16
		 : (EQ (object, QCdbus_type_int32)) ? DBUS_TYPE_INT32
		 : (EQ (object, QCdbus_type_uint32)) ? DBUS_TYPE_UINT32
		 : (EQ (object, QCdbus_type_int64)) ? DBUS_TYPE_INT64
		 : (EQ (object, QCdbus_type_uint64)) ? DBUS_TYPE_UINT64
		 : (EQ (object, QCdbus_type_double)) ? DBUS_TYPE_DOUBLE
		 : (EQ (object, QCdbus_type_string)) ? DBUS_TYPE_STRING
		 : (EQ (object, QCdbus_type_object_path)) ? DBUS_TYPE_OBJECT_PATH
		 : (EQ (object, QCdbus_type_signature)) ? DBUS_TYPE_SIGNATURE
#ifdef DBUS_TYPE_UNIX_FD
		 : (EQ (object, QCdbus_type_unix_fd)) ? DBUS_TYPE_UNIX_FD
#endif
		 : (EQ (object, QCdbus_type_array)) ? DBUS_TYPE_ARRAY
		 : (EQ (object, QCdbus_type_variant)) ? DBUS_TYPE_VARIANT
		 : (EQ (object, QCdbus_type_struct)) ? DBUS_TYPE_STRUCT
		 : (EQ (object, QCdbus_type_dict_entry)) ? DBUS_TYPE_DICT_ENTRY
		 : DBUS_TYPE_INVALID);
}

/* Append to SIGNATURE a copy of X, making sure SIGNATURE does
   not become too long.  */
static void
xd_signature_cat (char *signature, char const *x)
{
	int siglen = strlen (signature);
	int xlen = strlen (x);
	if (DBUS_MAXIMUM_SIGNATURE_LENGTH - xlen <= siglen)
		string_overflow ();
	strcat (signature, x);
}

/* Compute SIGNATURE of OBJECT.  It must have a form that it can be
   used in dbus_message_iter_open_container.  DTYPE is the DBusType
   the object is related to.  It is passed as argument, because it
   cannot be detected in basic type objects, when they are preceded by
   a type symbol.  PARENT_TYPE is the DBusType of a container this
   signature is embedded, or DBUS_TYPE_INVALID.  It is needed for the
   check that DBUS_TYPE_DICT_ENTRY occurs only as array element.  */
static void
xd_signature (char *signature, unsigned int dtype, unsigned int parent_type, Lisp_Object object)
{
	unsigned int subtype;
	Lisp_Object elt;
	char const *subsig;
	int subsiglen;
	char x[DBUS_MAXIMUM_SIGNATURE_LENGTH];

	elt = object;

	switch (dtype)
	{
	case DBUS_TYPE_BYTE:
	case DBUS_TYPE_UINT16:
	case DBUS_TYPE_UINT32:
	case DBUS_TYPE_UINT64:
#ifdef DBUS_TYPE_UNIX_FD
	case DBUS_TYPE_UNIX_FD:
#endif
		CHECK_NATNUM (object);
		sprintf (signature, "%c", dtype);
		break;

	case DBUS_TYPE_BOOLEAN:
		if (!EQ (object, Qt) && !EQ (object, Qnil))
			wrong_type_argument (intern ("booleanp"), object);
		sprintf (signature, "%c", dtype);
		break;

	case DBUS_TYPE_INT16:
	case DBUS_TYPE_INT32:
	case DBUS_TYPE_INT64:
		CHECK_NUMBER (object);
		sprintf (signature, "%c", dtype);
		break;

	case DBUS_TYPE_DOUBLE:
		CHECK_FLOAT (object);
		sprintf (signature, "%c", dtype);
		break;

	case DBUS_TYPE_STRING:
	case DBUS_TYPE_OBJECT_PATH:
	case DBUS_TYPE_SIGNATURE:
		CHECK_STRING (object);
		sprintf (signature, "%c", dtype);
		break;

	case DBUS_TYPE_ARRAY:
		/* Check that all list elements have the same D-Bus type.  For
		   complex element types, we just check the container type, not
		   the whole element's signature.  */
		CHECK_CONS (object);

		/* Type symbol is optional.  */
		if (EQ (QCdbus_type_array, CAR_SAFE (elt)))
			elt = XD_NEXT_VALUE (elt);

		/* If the array is empty, DBUS_TYPE_STRING is the default
		   element type.  */
		if (NILP (elt))
		{
			subtype = DBUS_TYPE_STRING;
			subsig = DBUS_TYPE_STRING_AS_STRING;
		}
		else
		{
			subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
			xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
			subsig = x;
		}

		/* If the element type is DBUS_TYPE_SIGNATURE, and this is the
		   only element, the value of this element is used as he array's
		   element signature.  */
		if ((subtype == DBUS_TYPE_SIGNATURE)
		    && STRINGP (CAR_SAFE (XD_NEXT_VALUE (elt)))
		    && NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
			subsig = SSDATA (CAR_SAFE (XD_NEXT_VALUE (elt)));

		while (!NILP (elt))
		{
			if (subtype != XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt)))
				wrong_type_argument (intern ("D-Bus"), CAR_SAFE (elt));
			elt = CDR_SAFE (XD_NEXT_VALUE (elt));
		}

		subsiglen = snprintf (signature, DBUS_MAXIMUM_SIGNATURE_LENGTH,
				      "%c%s", dtype, subsig);
		if (! (0 <= subsiglen && subsiglen < DBUS_MAXIMUM_SIGNATURE_LENGTH))
			string_overflow ();
		break;

	case DBUS_TYPE_VARIANT:
		/* Check that there is exactly one list element.  */
		CHECK_CONS (object);

		elt = XD_NEXT_VALUE (elt);
		subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
		xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));

		if (!NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
			wrong_type_argument (intern ("D-Bus"),
					     CAR_SAFE (CDR_SAFE (XD_NEXT_VALUE (elt))));

		sprintf (signature, "%c", dtype);
		break;

	case DBUS_TYPE_STRUCT:
		/* A struct list might contain any number of elements with
		   different types.  No further check needed.  */
		CHECK_CONS (object);

		elt = XD_NEXT_VALUE (elt);

		/* Compose the signature from the elements.  It is enclosed by
		   parentheses.  */
		sprintf (signature, "%c", DBUS_STRUCT_BEGIN_CHAR );
		while (!NILP (elt))
		{
			subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
			xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
			xd_signature_cat (signature, x);
			elt = CDR_SAFE (XD_NEXT_VALUE (elt));
		}
		xd_signature_cat (signature, DBUS_STRUCT_END_CHAR_AS_STRING);
		break;

	case DBUS_TYPE_DICT_ENTRY:
		/* Check that there are exactly two list elements, and the first
		   one is of basic type.  The dictionary entry itself must be an
		   element of an array.  */
		CHECK_CONS (object);

		/* Check the parent object type.  */
		if (parent_type != DBUS_TYPE_ARRAY)
			wrong_type_argument (intern ("D-Bus"), object);

		/* Compose the signature from the elements.  It is enclosed by
		   curly braces.  */
		sprintf (signature, "%c", DBUS_DICT_ENTRY_BEGIN_CHAR);

		/* First element.  */
		elt = XD_NEXT_VALUE (elt);
		subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
		xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
		xd_signature_cat (signature, x);

		if (!XD_BASIC_DBUS_TYPE (subtype))
			wrong_type_argument (intern ("D-Bus"), CAR_SAFE (XD_NEXT_VALUE (elt)));

		/* Second element.  */
		elt = CDR_SAFE (XD_NEXT_VALUE (elt));
		subtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (elt));
		xd_signature (x, subtype, dtype, CAR_SAFE (XD_NEXT_VALUE (elt)));
		xd_signature_cat (signature, x);

		if (!NILP (CDR_SAFE (XD_NEXT_VALUE (elt))))
			wrong_type_argument (intern ("D-Bus"),
					     CAR_SAFE (CDR_SAFE (XD_NEXT_VALUE (elt))));

		/* Closing signature.  */
		xd_signature_cat (signature, DBUS_DICT_ENTRY_END_CHAR_AS_STRING);
		break;

	default:
		wrong_type_argument (intern ("D-Bus"), object);
	}

	XD_DEBUG_MESSAGE ("%s", signature);
}

/* Append C value, extracted from Lisp OBJECT, to iteration ITER.
   DTYPE must be a valid DBusType.  It is used to convert Lisp
   objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
#define pI "l"
static void
xd_append_arg (unsigned int dtype, Lisp_Object object, DBusMessageIter *iter)
{
	char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];
	DBusMessageIter subiter;

	if (XD_BASIC_DBUS_TYPE (dtype))
		switch (dtype)
		{
		case DBUS_TYPE_BYTE:
			CHECK_NATNUM (object);
			{
				unsigned char val = XFASTINT (object) & 0xFF;
				XD_DEBUG_MESSAGE ("%c %d", dtype, val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_BOOLEAN:
		{
			dbus_bool_t val = (NILP (object)) ? FALSE : TRUE;
			XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
			if (!dbus_message_iter_append_basic (iter, dtype, &val))
				XD_SIGNAL2 (build_string ("Unable to append argument"), object);
			return;
		}

		case DBUS_TYPE_INT16:
			CHECK_NUMBER (object);
			{
				dbus_int16_t val = XINT (object);
				XD_DEBUG_MESSAGE ("%c %d", dtype, (int) val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_UINT16:
			CHECK_NATNUM (object);
			{
				dbus_uint16_t val = XFASTINT (object);
				XD_DEBUG_MESSAGE ("%c %u", dtype, (unsigned int) val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_INT32:
			CHECK_NUMBER (object);
			{
				dbus_int32_t val = XINT (object);
				XD_DEBUG_MESSAGE ("%c %d", dtype, val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_UINT32:
#ifdef DBUS_TYPE_UNIX_FD
		case DBUS_TYPE_UNIX_FD:
#endif
			CHECK_NATNUM (object);
			{
				dbus_uint32_t val = XFASTINT (object);
				XD_DEBUG_MESSAGE ("%c %u", dtype, val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_INT64:
			CHECK_NUMBER (object);
			{
				dbus_int64_t val = XINT (object);
				XD_DEBUG_MESSAGE ("%c %d", dtype, (int) val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_UINT64:
			CHECK_NATNUM (object);
			{
				dbus_uint64_t val = XFASTINT (object);
				XD_DEBUG_MESSAGE ("%c %"pI"d", dtype, XFASTINT (object));
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_DOUBLE:
			CHECK_FLOAT (object);
			{
				double val = XFLOAT_DATA (object);
				XD_DEBUG_MESSAGE ("%c %f", dtype, val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}

		case DBUS_TYPE_STRING:
		case DBUS_TYPE_OBJECT_PATH:
		case DBUS_TYPE_SIGNATURE:
			CHECK_STRING (object);
			{
				/* We need to send a valid UTF-8 string.  We could encode `object'
				   but by not encoding it, we guarantee it's valid utf-8, even if
				   it contains eight-bit-bytes.  Of course, you can still send
				   manually-crafted junk by passing a unibyte string.  */
				char *val = SSDATA (object);
				XD_DEBUG_MESSAGE ("%c %s", dtype, val);
				if (!dbus_message_iter_append_basic (iter, dtype, &val))
					XD_SIGNAL2 (build_string ("Unable to append argument"), object);
				return;
			}
		}

	else /* Compound types.  */
	{

		/* All compound types except array have a type symbol.  For
		   array, it is optional.  Skip it.  */
		if (!XD_BASIC_DBUS_TYPE (XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object))))
			object = XD_NEXT_VALUE (object);

		/* Open new subiteration.  */
		switch (dtype)
		{
		case DBUS_TYPE_ARRAY:
			/* An array has only elements of the same type.  So it is
			   sufficient to check the first element's signature
			   only.  */

			if (NILP (object))
				/* If the array is empty, DBUS_TYPE_STRING is the default
				   element type.  */
				strcpy (signature, DBUS_TYPE_STRING_AS_STRING);

			else
				/* If the element type is DBUS_TYPE_SIGNATURE, and this is
				   the only element, the value of this element is used as
				   the array's element signature.  */
				if ((XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object))
				     == DBUS_TYPE_SIGNATURE)
				    && STRINGP (CAR_SAFE (XD_NEXT_VALUE (object)))
				    && NILP (CDR_SAFE (XD_NEXT_VALUE (object))))
				{
					strcpy (signature, SSDATA (CAR_SAFE (XD_NEXT_VALUE (object))));
					object = CDR_SAFE (XD_NEXT_VALUE (object));
				}

				else
					xd_signature (signature,
						      XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object)),
						      dtype, CAR_SAFE (XD_NEXT_VALUE (object)));

			XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
					  SDATA (format2 ("%s", object, Qnil)));
			if (!dbus_message_iter_open_container (iter, dtype,
							       signature, &subiter))
				XD_SIGNAL3 (build_string ("Cannot open container"),
					    make_number (dtype), build_string (signature));
			break;

		case DBUS_TYPE_VARIANT:
			/* A variant has just one element.  */
			xd_signature (signature, XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object)),
				      dtype, CAR_SAFE (XD_NEXT_VALUE (object)));

			XD_DEBUG_MESSAGE ("%c %s %s", dtype, signature,
					  SDATA (format2 ("%s", object, Qnil)));
			if (!dbus_message_iter_open_container (iter, dtype,
							       signature, &subiter))
				XD_SIGNAL3 (build_string ("Cannot open container"),
					    make_number (dtype), build_string (signature));
			break;

		case DBUS_TYPE_STRUCT:
		case DBUS_TYPE_DICT_ENTRY:
			/* These containers do not require a signature.  */
			XD_DEBUG_MESSAGE ("%c %s", dtype,
					  SDATA (format2 ("%s", object, Qnil)));
			if (!dbus_message_iter_open_container (iter, dtype, NULL, &subiter))
				XD_SIGNAL2 (build_string ("Cannot open container"),
					    make_number (dtype));
			break;
		}

		/* Loop over list elements.  */
		while (!NILP (object))
		{
			dtype = XD_OBJECT_TO_DBUS_TYPE (CAR_SAFE (object));
			object = XD_NEXT_VALUE (object);

			xd_append_arg (dtype, CAR_SAFE (object), &subiter);

			object = CDR_SAFE (object);
		}

		/* Close the subiteration.  */
		if (!dbus_message_iter_close_container (iter, &subiter))
			XD_SIGNAL2 (build_string ("Cannot close container"),
				    make_number (dtype));
	}
}

/* Retrieve C value from a DBusMessageIter structure ITER, and return
   a converted Lisp object.  The type DTYPE of the argument of the
   D-Bus message must be a valid DBusType.  Compound D-Bus types
   result always in a Lisp list.  */
static Lisp_Object
xd_retrieve_arg (unsigned int dtype, DBusMessageIter *iter)
{

	switch (dtype)
	{
	case DBUS_TYPE_BYTE:
	{
		unsigned int val;
		dbus_message_iter_get_basic (iter, &val);
		val = val & 0xFF;
		XD_DEBUG_MESSAGE ("%c %d", dtype, val);
		return make_number (val);
	}

	case DBUS_TYPE_BOOLEAN:
	{
		dbus_bool_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %s", dtype, (val == FALSE) ? "false" : "true");
		return (val == FALSE) ? Qnil : Qt;
	}

	case DBUS_TYPE_INT16:
	{
		dbus_int16_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %d", dtype, val);
		return make_number (val);
	}

	case DBUS_TYPE_UINT16:
	{
		dbus_uint16_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %d", dtype, val);
		return make_number (val);
	}

	case DBUS_TYPE_INT32:
	{
		dbus_int32_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %d", dtype, val);
		return make_fixnum_or_float (val);
	}

	case DBUS_TYPE_UINT32:
#ifdef DBUS_TYPE_UNIX_FD
	case DBUS_TYPE_UNIX_FD:
#endif
	{
		dbus_uint32_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %d", dtype, val);
		return make_fixnum_or_float (val);
	}

	case DBUS_TYPE_INT64:
	{
		dbus_int64_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %d", dtype, (int) val);
		return make_fixnum_or_float (val);
	}

	case DBUS_TYPE_UINT64:
	{
		dbus_uint64_t val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %d", dtype, (int) val);
		return make_fixnum_or_float (val);
	}

	case DBUS_TYPE_DOUBLE:
	{
		double val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %f", dtype, val);
		return make_float (val);
	}

	case DBUS_TYPE_STRING:
	case DBUS_TYPE_OBJECT_PATH:
	case DBUS_TYPE_SIGNATURE:
	{
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		XD_DEBUG_MESSAGE ("%c %s", dtype, val);
		return build_string (val);
	}

	case DBUS_TYPE_ARRAY:
	case DBUS_TYPE_VARIANT:
	case DBUS_TYPE_STRUCT:
	case DBUS_TYPE_DICT_ENTRY:
	{
		Lisp_Object result;
		struct gcpro gcpro1;
		DBusMessageIter subiter;
		int subtype;
		result = Qnil;
		GCPRO1 (result);
		dbus_message_iter_recurse (iter, &subiter);
		while ((subtype = dbus_message_iter_get_arg_type (&subiter))
		       != DBUS_TYPE_INVALID)
		{
			result = Fcons (xd_retrieve_arg (subtype, &subiter), result);
			dbus_message_iter_next (&subiter);
		}
		XD_DEBUG_MESSAGE ("%c %s", dtype, SDATA (format2 ("%s", result, Qnil)));
		RETURN_UNGCPRO (Fnreverse (result));
	}

	default:
		XD_DEBUG_MESSAGE ("DBusType '%c' not supported", dtype);
		return Qnil;
	}
}

/* Initialize D-Bus connection.  BUS is either a Lisp symbol, :system
   or :session, or a string denoting the bus address.  It tells which
   D-Bus to initialize.  If RAISE_ERROR is non-zero, signal an error
   when the connection cannot be initialized.  */
static DBusConnection *
xd_initialize (Lisp_Object bus, int raise_error)
{
	DBusConnection *connection;
	DBusError derror;

	/* Parameter check.  */
	if (!STRINGP (bus))
	{
		CHECK_SYMBOL (bus);
		if (!(EQ (bus, QCdbus_system_bus) || EQ (bus, QCdbus_session_bus)))
		{
			if (raise_error)
				XD_SIGNAL2 (build_string ("Wrong bus name"), bus);
			else
				return NULL;
		}

		/* We do not want to have an autolaunch for the session bus.  */
		if (EQ (bus, QCdbus_session_bus)
		    && getenv ("DBUS_SESSION_BUS_ADDRESS") == NULL)
		{
			if (raise_error)
				XD_SIGNAL2 (build_string ("No connection to bus"), bus);
			else
				return NULL;
		}
	}

	/* Open a connection to the bus.  */
	dbus_error_init (&derror);

	if (STRINGP (bus))
		connection = dbus_connection_open (SSDATA (bus), &derror);
	else
		if (EQ (bus, QCdbus_system_bus))
			connection = dbus_bus_get (DBUS_BUS_SYSTEM, &derror);
		else
			connection = dbus_bus_get (DBUS_BUS_SESSION, &derror);

	if (dbus_error_is_set (&derror))
	{
		if (raise_error)
			XD_ERROR (derror);
		else
			connection = NULL;
	}

	/* If it is not the system or session bus, we must register
	   ourselves.  Otherwise, we have called dbus_bus_get, which has
	   configured us to exit if the connection closes - we undo this
	   setting.  */
	if (connection != NULL)
	{
		if (STRINGP (bus))
			dbus_bus_register (connection, &derror);
		else
			dbus_connection_set_exit_on_disconnect (connection, FALSE);
	}

	if (dbus_error_is_set (&derror))
	{
		if (raise_error)
			XD_ERROR (derror);
		else
			connection = NULL;
	}

	if (connection == NULL && raise_error)
		XD_SIGNAL2 (build_string ("No connection to bus"), bus);

	/* Cleanup.  */
	dbus_error_free (&derror);

	/* Return the result.  */
	return connection;
}

/* Return the file descriptor for WATCH, -1 if not found.  */
static int
xd_find_watch_fd (DBusWatch *watch)
{
#if HAVE_DBUS_WATCH_GET_UNIX_FD
	/* TODO: Reverse these on Win32, which prefers the opposite.  */
	int fd = dbus_watch_get_unix_fd (watch);
	if (fd == -1)
		fd = dbus_watch_get_socket (watch);
#else
	int fd = dbus_watch_get_fd (watch);
#endif
	return fd;
}

/* Prototype.  */
static void
xd_read_queued_messages (int fd, void *data, int for_read);

/* Start monitoring WATCH for possible I/O.  */
static dbus_bool_t
xd_add_watch (DBusWatch *watch, void *data)
{
	unsigned int flags = dbus_watch_get_flags (watch);
	int fd = xd_find_watch_fd (watch);

	XD_DEBUG_MESSAGE ("fd %d, write %d, enabled %d",
			  fd, flags & DBUS_WATCH_WRITABLE,
			  dbus_watch_get_enabled (watch));

	if (fd == -1)
		return FALSE;

	if (dbus_watch_get_enabled (watch))
	{
		if (flags & DBUS_WATCH_WRITABLE)
			add_write_fd (fd, xd_read_queued_messages, data);
		if (flags & DBUS_WATCH_READABLE)
			add_read_fd (fd, xd_read_queued_messages, data);
	}
	return TRUE;
}

/* Stop monitoring WATCH for possible I/O.
   DATA is the used bus, either a string or QCdbus_system_bus or
   QCdbus_session_bus.  */
static void
xd_remove_watch (DBusWatch *watch, void *data)
{
	unsigned int flags = dbus_watch_get_flags (watch);
	int fd = xd_find_watch_fd (watch);

	XD_DEBUG_MESSAGE ("fd %d", fd);

	if (fd == -1)
		return;

	/* Unset session environment.  */
	if (XSYMBOL (QCdbus_session_bus) == data)
	{
		XD_DEBUG_MESSAGE ("unsetenv DBUS_SESSION_BUS_ADDRESS");
		unsetenv ("DBUS_SESSION_BUS_ADDRESS");
	}

	if (flags & DBUS_WATCH_WRITABLE)
		delete_write_fd (fd);
	if (flags & DBUS_WATCH_READABLE)
		delete_read_fd (fd);
}

/* Toggle monitoring WATCH for possible I/O.  */
static void
xd_toggle_watch (DBusWatch *watch, void *data)
{
	if (dbus_watch_get_enabled (watch))
		xd_add_watch (watch, data);
	else
		xd_remove_watch (watch, data);
}

DEFUN("dbus-init-bus", Fdbus_init_bus, 1, 1, 0, /*
Initialize connection to D-Bus BUS.
*/
      (bus))
{
	DBusConnection *connection;
	void *busp;

	/* Check parameter.  */
	if (SYMBOLP (bus))
		busp = XSYMBOL (bus);
	else if (STRINGP (bus))
		busp = XSTRING (bus);
	else
		wrong_type_argument (intern ("D-Bus"), bus);

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Add the watch functions.  We pass also the bus as data, in order
	   to distinguish between the buses in xd_remove_watch.  */
	if (!dbus_connection_set_watch_functions (connection,
						  xd_add_watch,
						  xd_remove_watch,
						  xd_toggle_watch,
						  busp, NULL))
		XD_SIGNAL1 (build_string ("Cannot add watch functions"));

	/* Add bus to list of registered buses.  */
	Vdbus_registered_buses =  Fcons (bus, Vdbus_registered_buses);

	/* We do not want to abort.  */
	putenv ((char *) "DBUS_FATAL_WARNINGS=0");

	/* Return.  */
	return Qnil;
}

DEFUN("dbus-close-bus", Fdbus_close_bus, 1, 1, 0, /*
Close connection to D-Bus BUS.
*/
      (bus))
{
	DBusConnection *connection;

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Decrement reference count to the bus.  */
	dbus_connection_unref (connection);

	/* Remove bus from list of registered buses.  */
	Vdbus_registered_buses = Fdelete (bus, Vdbus_registered_buses);

	/* Return.  */
	return Qnil;
}

DEFUN("dbus-get-unique-name", Fdbus_get_unique_name, 1, 1, 0, /*
Return the unique name of Emacs registered at D-Bus BUS.
*/
      (bus))
{
	DBusConnection *connection;
	const char *name;

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Request the name.  */
	name = dbus_bus_get_unique_name (connection);
	if (name == NULL)
		XD_SIGNAL1 (build_string ("No unique name available"));

	/* Return.  */
	return build_string (name);
}

#define pD

DEFUN("dbus-call-method", Fdbus_call_method, 5, MANY, 0, /*
Call METHOD on the D-Bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide METHOD.

If the parameter `:timeout' is given, the following integer TIMEOUT
specifies the maximum number of milliseconds the method call must
return.  The default value is 25,000.  If the method call doesn't
return in time, a D-Bus error is raised.

All other arguments ARGS are passed to METHOD as arguments.  They are
converted into D-Bus types via the following rules:

t and nil => DBUS_TYPE_BOOLEAN
number    => DBUS_TYPE_UINT32
integer   => DBUS_TYPE_INT32
float     => DBUS_TYPE_DOUBLE
string    => DBUS_TYPE_STRING
list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type symbol.  For details about
type symbols, see Info node `(dbus)Type Conversion'.

`dbus-call-method' returns the resulting values of METHOD as a list of
Lisp objects.  The type conversion happens the other direction as for
input arguments.  It follows the mapping rules:

DBUS_TYPE_BOOLEAN     => t or nil
DBUS_TYPE_BYTE        => number
DBUS_TYPE_UINT16      => number
DBUS_TYPE_INT16       => integer
DBUS_TYPE_UINT32      => number or float
DBUS_TYPE_UNIX_FD     => number or float
DBUS_TYPE_INT32       => integer or float
DBUS_TYPE_UINT64      => number or float
DBUS_TYPE_INT64       => integer or float
DBUS_TYPE_DOUBLE      => float
DBUS_TYPE_STRING      => string
DBUS_TYPE_OBJECT_PATH => string
DBUS_TYPE_SIGNATURE   => string
DBUS_TYPE_ARRAY       => list
DBUS_TYPE_VARIANT     => list
DBUS_TYPE_STRUCT      => list
DBUS_TYPE_DICT_ENTRY  => list

Example:

\(dbus-call-method
:session "org.gnome.seahorse" "/org/gnome/seahorse/keys/openpgp"
"org.gnome.seahorse.Keys" "GetKeyField"
"openpgp:657984B8C7A966DD" "simple-name")

=> (t ("Philip R. Zimmermann"))

If the result of the METHOD call is just one value, the converted Lisp
object is returned instead of a list containing this single Lisp object.

\(dbus-call-method
:system "org.freedesktop.Hal" "/org/freedesktop/Hal/devices/computer"
"org.freedesktop.Hal.Device" "GetPropertyString"
"system.kernel.machine")

=> "i686"

usage: (dbus-call-method BUS SERVICE PATH INTERFACE METHOD &optional :timeout TIMEOUT &rest ARGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service, path, interface, method;
	Lisp_Object result;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
	DBusConnection *connection;
	DBusMessage *dmessage;
	DBusMessage *reply;
	DBusMessageIter iter;
	DBusError derror;
	unsigned int dtype;
	int timeout = -1;
	int i = 5;
	char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

	/* Check parameters.  */
	bus = args[0];
	service = args[1];
	path = args[2];
	interface = args[3];
	method = args[4];

	CHECK_STRING (service);
	CHECK_STRING (path);
	CHECK_STRING (interface);
	CHECK_STRING (method);
	GCPRO5 (bus, service, path, interface, method);

	XD_DEBUG_MESSAGE ("%s %s %s %s",
			  SDATA (service),
			  SDATA (path),
			  SDATA (interface),
			  SDATA (method));

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Create the message.  */
	dmessage = dbus_message_new_method_call (SSDATA (service),
						 SSDATA (path),
						 SSDATA (interface),
						 SSDATA (method));
	UNGCPRO;
	if (dmessage == NULL)
		XD_SIGNAL1 (build_string ("Unable to create a new message"));

	/* Check for timeout parameter.  */
	if ((i+2 <= nargs) && (EQ ((args[i]), QCdbus_timeout)))
	{
		CHECK_NATNUM (args[i+1]);
		timeout = XFASTINT (args[i+1]);
		i = i+2;
	}

	/* Initialize parameter list of message.  */
	dbus_message_iter_init_append (dmessage, &iter);

	/* Append parameters to the message.  */
	for (; i < nargs; ++i)
	{
		dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
		if (XD_DBUS_TYPE_P (args[i]))
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i+1]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s %s", i - 4,
					  SDATA (format2 ("%s", args[i], Qnil)),
					  SDATA (format2 ("%s", args[i+1], Qnil)));
			++i;
		}
		else
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s", i - 4,
					  SDATA (format2 ("%s", args[i], Qnil)));
		}

		/* Check for valid signature.  We use DBUS_TYPE_INVALID as
		   indication that there is no parent type.  */
		xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

		xd_append_arg (dtype, args[i], &iter);
	}

	/* Send the message.  */
	dbus_error_init (&derror);
	reply = dbus_connection_send_with_reply_and_block (connection,
							   dmessage,
							   timeout,
							   &derror);

	if (dbus_error_is_set (&derror))
		XD_ERROR (derror);

	if (reply == NULL)
		XD_SIGNAL1 (build_string ("No reply"));

	XD_DEBUG_MESSAGE ("Message sent");

	/* Collect the results.  */
	result = Qnil;
	GCPRO1 (result);

	if (dbus_message_iter_init (reply, &iter))
	{
		/* Loop over the parameters of the D-Bus reply message.  Construct a
		   Lisp list, which is returned by `dbus-call-method'.  */
		while ((dtype = dbus_message_iter_get_arg_type (&iter))
		       != DBUS_TYPE_INVALID)
		{
			result = Fcons (xd_retrieve_arg (dtype, &iter), result);
			dbus_message_iter_next (&iter);
		}
	}
	else
	{
		/* No arguments: just return nil.  */
	}

	/* Cleanup.  */
	dbus_error_free (&derror);
	dbus_message_unref (dmessage);
	dbus_message_unref (reply);

	/* Return the result.  If there is only one single Lisp object,
	   return it as-it-is, otherwise return the reversed list.  */
	if (XFASTINT (Flength (result)) == 1)
		RETURN_UNGCPRO (CAR_SAFE (result));
	else
		RETURN_UNGCPRO (Fnreverse (result));
}

DEFUN("dbus-call-method-asynchronously", Fdbus_call_method_asynchronously,
       6, MANY, 0, /*
Call METHOD on the D-Bus BUS asynchronously.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name to be used.  PATH is the D-Bus
object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide METHOD.

HANDLER is a Lisp function, which is called when the corresponding
return message has arrived.  If HANDLER is nil, no return message will
be expected.

If the parameter `:timeout' is given, the following integer TIMEOUT
specifies the maximum number of milliseconds the method call must
return.  The default value is 25,000.  If the method call doesn't
return in time, a D-Bus error is raised.

All other arguments ARGS are passed to METHOD as arguments.  They are
converted into D-Bus types via the following rules:

t and nil => DBUS_TYPE_BOOLEAN
number    => DBUS_TYPE_UINT32
integer   => DBUS_TYPE_INT32
float     => DBUS_TYPE_DOUBLE
string    => DBUS_TYPE_STRING
list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type symbol.  For details about
type symbols, see Info node `(dbus)Type Conversion'.

Unless HANDLER is nil, the function returns a key into the hash table
`dbus-registered-objects-table'.  The corresponding entry in the hash
table is removed, when the return message has been arrived, and
HANDLER is called.

Example:

\(dbus-call-method-asynchronously
:system "org.freedesktop.Hal" "/org/freedesktop/Hal/devices/computer"
"org.freedesktop.Hal.Device" "GetPropertyString" 'message
"system.kernel.machine")

=> (:system 2)

-| i686

usage: (dbus-call-method-asynchronously BUS SERVICE PATH INTERFACE METHOD HANDLER &optional :timeout TIMEOUT &rest ARGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service, path, interface, method, handler;
	Lisp_Object result;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
	DBusConnection *connection;
	DBusMessage *dmessage;
	DBusMessageIter iter;
	unsigned int dtype;
	dbus_uint32_t serial;
	int timeout = -1;
	int i = 6;
	char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

	/* Check parameters.  */
	bus = args[0];
	service = args[1];
	path = args[2];
	interface = args[3];
	method = args[4];
	handler = args[5];

	CHECK_STRING (service);
	CHECK_STRING (path);
	CHECK_STRING (interface);
	CHECK_STRING (method);
	if (!NILP (handler) && !FUNCTIONP (handler))
		wrong_type_argument (Qinvalid_function, handler);
	GCPRO6 (bus, service, path, interface, method, handler);

	XD_DEBUG_MESSAGE ("%s %s %s %s",
			  SDATA (service),
			  SDATA (path),
			  SDATA (interface),
			  SDATA (method));

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Create the message.  */
	dmessage = dbus_message_new_method_call (SSDATA (service),
						 SSDATA (path),
						 SSDATA (interface),
						 SSDATA (method));
	if (dmessage == NULL)
		XD_SIGNAL1 (build_string ("Unable to create a new message"));

	/* Check for timeout parameter.  */
	if ((i+2 <= nargs) && (EQ ((args[i]), QCdbus_timeout)))
	{
		CHECK_NATNUM (args[i+1]);
		timeout = XFASTINT (args[i+1]);
		i = i+2;
	}

	/* Initialize parameter list of message.  */
	dbus_message_iter_init_append (dmessage, &iter);

	/* Append parameters to the message.  */
	for (; i < nargs; ++i)
	{
		dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
		if (XD_DBUS_TYPE_P (args[i]))
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i+1]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s %s", i - 4,
					  SDATA (format2 ("%s", args[i], Qnil)),
					  SDATA (format2 ("%s", args[i+1], Qnil)));
			++i;
		}
		else
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s", i - 4,
					  SDATA (format2 ("%s", args[i], Qnil)));
		}

		/* Check for valid signature.  We use DBUS_TYPE_INVALID as
		   indication that there is no parent type.  */
		xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

		xd_append_arg (dtype, args[i], &iter);
	}

	if (!NILP (handler))
	{
		/* Send the message.  The message is just added to the outgoing
		   message queue.  */
		if (!dbus_connection_send_with_reply (connection, dmessage,
						      NULL, timeout))
			XD_SIGNAL1 (build_string ("Cannot send message"));

		/* The result is the key in Vdbus_registered_objects_table.  */
		serial = dbus_message_get_serial (dmessage);
		result = list2 (bus, make_fixnum_or_float (serial));

		/* Create a hash table entry.  */
		Fputhash (result, handler, Vdbus_registered_objects_table);
	}
	else
	{
		/* Send the message.  The message is just added to the outgoing
		   message queue.  */
		if (!dbus_connection_send (connection, dmessage, NULL))
			XD_SIGNAL1 (build_string ("Cannot send message"));

		result = Qnil;
	}

	XD_DEBUG_MESSAGE ("Message sent");

	/* Cleanup.  */
	dbus_message_unref (dmessage);

	/* Return the result.  */
	RETURN_UNGCPRO (result);
}

DEFUN("dbus-method-return-internal", Fdbus_method_return_internal,
      3, MANY, 0, /*
Return for message SERIAL on the D-Bus BUS.
This is an internal function, it shall not be used outside dbus.el.

usage: (dbus-method-return-internal BUS SERIAL SERVICE &rest ARGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service;
	struct gcpro gcpro1, gcpro2;
	DBusConnection *connection;
	DBusMessage *dmessage;
	DBusMessageIter iter;
	dbus_uint32_t serial;
	unsigned int ui_serial, dtype;
	int i;
	char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

	/* Check parameters.  */
	bus = args[0];
	service = args[2];

	CHECK_DBUS_SERIAL_GET_SERIAL (args[1], serial);
	CHECK_STRING (service);
	GCPRO2 (bus, service);

	ui_serial = serial;
	XD_DEBUG_MESSAGE ("%u %s ", ui_serial, SSDATA (service));

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Create the message.  */
	dmessage = dbus_message_new (DBUS_MESSAGE_TYPE_METHOD_RETURN);
	if ((dmessage == NULL)
	    || (!dbus_message_set_reply_serial (dmessage, serial))
	    || (!dbus_message_set_destination (dmessage, SSDATA (service))))
	{
		UNGCPRO;
		XD_SIGNAL1 (build_string ("Unable to create a return message"));
	}

	UNGCPRO;

	/* Initialize parameter list of message.  */
	dbus_message_iter_init_append (dmessage, &iter);

	/* Append parameters to the message.  */
	for (i = 3; i < nargs; ++i)
	{
		dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
		if (XD_DBUS_TYPE_P (args[i]))
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i+1]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s %s", i - 2,
					  SDATA (format2 ("%s", args[i], Qnil)),
					  SDATA (format2 ("%s", args[i+1], Qnil)));
			++i;
		}
		else
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s", i - 2,
					  SDATA (format2 ("%s", args[i], Qnil)));
		}

		/* Check for valid signature.  We use DBUS_TYPE_INVALID as
		   indication that there is no parent type.  */
		xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

		xd_append_arg (dtype, args[i], &iter);
	}

	/* Send the message.  The message is just added to the outgoing
	   message queue.  */
	if (!dbus_connection_send (connection, dmessage, NULL))
		XD_SIGNAL1 (build_string ("Cannot send message"));

	XD_DEBUG_MESSAGE ("Message sent");

	/* Cleanup.  */
	dbus_message_unref (dmessage);

	/* Return.  */
	return Qt;
}

DEFUN("dbus-method-error-internal", Fdbus_method_error_internal,
       3, MANY, 0, /*
Return error message for message SERIAL on the D-Bus BUS.
This is an internal function, it shall not be used outside dbus.el.

usage: (dbus-method-error-internal BUS SERIAL SERVICE &rest ARGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service;
	struct gcpro gcpro1, gcpro2;
	DBusConnection *connection;
	DBusMessage *dmessage;
	DBusMessageIter iter;
	dbus_uint32_t serial;
	unsigned int ui_serial, dtype;
	int i;
	char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

	/* Check parameters.  */
	bus = args[0];
	service = args[2];

	CHECK_DBUS_SERIAL_GET_SERIAL (args[1], serial);
	CHECK_STRING (service);
	GCPRO2 (bus, service);

	ui_serial = serial;
	XD_DEBUG_MESSAGE ("%u %s ", ui_serial, SSDATA (service));

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Create the message.  */
	dmessage = dbus_message_new (DBUS_MESSAGE_TYPE_ERROR);
	if ((dmessage == NULL)
	    || (!dbus_message_set_error_name (dmessage, DBUS_ERROR_FAILED))
	    || (!dbus_message_set_reply_serial (dmessage, serial))
	    || (!dbus_message_set_destination (dmessage, SSDATA (service))))
	{
		UNGCPRO;
		XD_SIGNAL1 (build_string ("Unable to create a error message"));
	}

	UNGCPRO;

	/* Initialize parameter list of message.  */
	dbus_message_iter_init_append (dmessage, &iter);

	/* Append parameters to the message.  */
	for (i = 3; i < nargs; ++i)
	{
		dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
		if (XD_DBUS_TYPE_P (args[i]))
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i+1]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s %s", i - 2,
					  SDATA (format2 ("%s", args[i], Qnil)),
					  SDATA (format2 ("%s", args[i+1], Qnil)));
			++i;
		}
		else
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s", i - 2,
					  SDATA (format2 ("%s", args[i], Qnil)));
		}

		/* Check for valid signature.  We use DBUS_TYPE_INVALID as
		   indication that there is no parent type.  */
		xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

		xd_append_arg (dtype, args[i], &iter);
	}

	/* Send the message.  The message is just added to the outgoing
	   message queue.  */
	if (!dbus_connection_send (connection, dmessage, NULL))
		XD_SIGNAL1 (build_string ("Cannot send message"));

	XD_DEBUG_MESSAGE ("Message sent");

	/* Cleanup.  */
	dbus_message_unref (dmessage);

	/* Return.  */
	return Qt;
}

DEFUN("dbus-send-signal", Fdbus_send_signal, 5, MANY, 0, /*
Send signal SIGNAL on the D-Bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name SIGNAL is sent from.  PATH is the
D-Bus object path SERVICE is registered at.  INTERFACE is an interface
offered by SERVICE.  It must provide signal SIGNAL.

All other arguments ARGS are passed to SIGNAL as arguments.  They are
converted into D-Bus types via the following rules:

t and nil => DBUS_TYPE_BOOLEAN
number    => DBUS_TYPE_UINT32
integer   => DBUS_TYPE_INT32
float     => DBUS_TYPE_DOUBLE
string    => DBUS_TYPE_STRING
list      => DBUS_TYPE_ARRAY

All arguments can be preceded by a type symbol.  For details about
type symbols, see Info node `(dbus)Type Conversion'.

Example:

\(dbus-send-signal
:session "org.gnu.Emacs" "/org/gnu/Emacs"
"org.gnu.Emacs.FileManager" "FileModified" "/home/albinus/.emacs")

usage: (dbus-send-signal BUS SERVICE PATH INTERFACE SIGNAL &rest ARGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service, path, interface, signal;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
	DBusConnection *connection;
	DBusMessage *dmessage;
	DBusMessageIter iter;
	unsigned int dtype;
	int i;
	char signature[DBUS_MAXIMUM_SIGNATURE_LENGTH];

	/* Check parameters.  */
	bus = args[0];
	service = args[1];
	path = args[2];
	interface = args[3];
	signal = args[4];

	CHECK_STRING (service);
	CHECK_STRING (path);
	CHECK_STRING (interface);
	CHECK_STRING (signal);
	GCPRO5 (bus, service, path, interface, signal);

	XD_DEBUG_MESSAGE ("%s %s %s %s",
			  SDATA (service),
			  SDATA (path),
			  SDATA (interface),
			  SDATA (signal));

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Create the message.  */
	dmessage = dbus_message_new_signal (SSDATA (path),
					    SSDATA (interface),
					    SSDATA (signal));
	UNGCPRO;
	if (dmessage == NULL)
		XD_SIGNAL1 (build_string ("Unable to create a new message"));

	/* Initialize parameter list of message.  */
	dbus_message_iter_init_append (dmessage, &iter);

	/* Append parameters to the message.  */
	for (i = 5; i < nargs; ++i)
	{
		dtype = XD_OBJECT_TO_DBUS_TYPE (args[i]);
		if (XD_DBUS_TYPE_P (args[i]))
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i+1]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s %s", i - 4,
					  SDATA (format2 ("%s", args[i], Qnil)),
					  SDATA (format2 ("%s", args[i+1], Qnil)));
			++i;
		}
		else
		{
			XD_DEBUG_VALID_LISP_OBJECT_P (args[i]);
			XD_DEBUG_MESSAGE ("Parameter%"pD"d %s", i - 4,
					  SDATA (format2 ("%s", args[i], Qnil)));
		}

		/* Check for valid signature.  We use DBUS_TYPE_INVALID as
		   indication that there is no parent type.  */
		xd_signature (signature, dtype, DBUS_TYPE_INVALID, args[i]);

		xd_append_arg (dtype, args[i], &iter);
	}

	/* Send the message.  The message is just added to the outgoing
	   message queue.  */
	if (!dbus_connection_send (connection, dmessage, NULL))
		XD_SIGNAL1 (build_string ("Cannot send message"));

	XD_DEBUG_MESSAGE ("Signal sent");

	/* Cleanup.  */
	dbus_message_unref (dmessage);

	/* Return.  */
	return Qt;
}

/* Read one queued incoming message of the D-Bus BUS.
   BUS is either a Lisp symbol, :system or :session, or a string denoting
   the bus address.  */

/* 
 * FIXME: Gotta fix this.
 * We kinda need it to get anything useful to happen. --SY.
 */
#if 0
static void
xd_read_message_1 (DBusConnection *connection, Lisp_Object bus)
{
	Lisp_Object args, key, value;
	struct gcpro gcpro1;
	struct input_event event;
	DBusMessage *dmessage;
	DBusMessageIter iter;
	unsigned int dtype;
	int mtype;
	dbus_uint32_t serial;
	unsigned int ui_serial;
	const char *uname, *path, *interface, *member;

	dmessage = dbus_connection_pop_message (connection);

	/* Return if there is no queued message.  */
	if (dmessage == NULL)
		return;

	/* Collect the parameters.  */
	args = Qnil;
	GCPRO1 (args);

	/* Loop over the resulting parameters.  Construct a list.  */
	if (dbus_message_iter_init (dmessage, &iter))
	{
		while ((dtype = dbus_message_iter_get_arg_type (&iter))
		       != DBUS_TYPE_INVALID)
		{
			args = Fcons (xd_retrieve_arg (dtype, &iter), args);
			dbus_message_iter_next (&iter);
		}
		/* The arguments are stored in reverse order.  Reorder them.  */
		args = Fnreverse (args);
	}

	/* Read message type, message serial, unique name, object path,
	   interface and member from the message.  */
	mtype = dbus_message_get_type (dmessage);
	ui_serial = serial =
		((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
		 || (mtype == DBUS_MESSAGE_TYPE_ERROR))
		? dbus_message_get_reply_serial (dmessage)
		: dbus_message_get_serial (dmessage);
	uname = dbus_message_get_sender (dmessage);
	path = dbus_message_get_path (dmessage);
	interface = dbus_message_get_interface (dmessage);
	member = dbus_message_get_member (dmessage);

	XD_DEBUG_MESSAGE ("Event received: %s %u %s %s %s %s %s",
			  (mtype == DBUS_MESSAGE_TYPE_INVALID)
			  ? "DBUS_MESSAGE_TYPE_INVALID"
			  : (mtype == DBUS_MESSAGE_TYPE_METHOD_CALL)
			  ? "DBUS_MESSAGE_TYPE_METHOD_CALL"
			  : (mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
			  ? "DBUS_MESSAGE_TYPE_METHOD_RETURN"
			  : (mtype == DBUS_MESSAGE_TYPE_ERROR)
			  ? "DBUS_MESSAGE_TYPE_ERROR"
			  : "DBUS_MESSAGE_TYPE_SIGNAL",
			  ui_serial, uname, path, interface, member,
			  SDATA (format2 ("%s", args, Qnil)));

	if ((mtype == DBUS_MESSAGE_TYPE_METHOD_RETURN)
	    || (mtype == DBUS_MESSAGE_TYPE_ERROR))
	{
		/* Search for a registered function of the message.  */
		key = list2 (bus, make_fixnum_or_float (serial));
		value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

		/* There shall be exactly one entry.  Construct an event.  */
		if (NILP (value))
			goto cleanup;

		/* Remove the entry.  */
		Fremhash (key, Vdbus_registered_objects_table);

		/* Construct an event.  */
		EVENT_INIT (event);
		event.kind = DBUS_EVENT;
		event.frame_or_window = Qnil;
		event.arg = Fcons (value, args);
	}

	else /* (mtype != DBUS_MESSAGE_TYPE_METHOD_RETURN)  */
	{
		/* Vdbus_registered_objects_table requires non-nil interface and
		   member.  */
		if ((interface == NULL) || (member == NULL))
			goto cleanup;

		/* Search for a registered function of the message.  */
		key = list3 (bus, build_string (interface), build_string (member));
		value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

		/* Loop over the registered functions.  Construct an event.  */
		while (!NILP (value))
		{
			key = CAR_SAFE (value);
			/* key has the structure (UNAME SERVICE PATH HANDLER).  */
			if (((uname == NULL)
			     || (NILP (CAR_SAFE (key)))
			     || (strcmp (uname, SSDATA (CAR_SAFE (key))) == 0))
			    && ((path == NULL)
				|| (NILP (CAR_SAFE (CDR_SAFE (CDR_SAFE (key)))))
				|| (strcmp (path,
					    SSDATA (CAR_SAFE (CDR_SAFE (CDR_SAFE (key)))))
				    == 0))
			    && (!NILP (CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (key)))))))
			{
				EVENT_INIT (event);
				event.kind = DBUS_EVENT;
				event.frame_or_window = Qnil;
				event.arg
					= Fcons (CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (key)))), args);
				break;
			}
			value = CDR_SAFE (value);
		}

		if (NILP (value))
			goto cleanup;
	}

	/* Add type, serial, uname, path, interface and member to the event.  */
	event.arg = Fcons ((member == NULL ? Qnil : build_string (member)),
			   event.arg);
	event.arg = Fcons ((interface == NULL ? Qnil : build_string (interface)),
			   event.arg);
	event.arg = Fcons ((path == NULL ? Qnil : build_string (path)),
			   event.arg);
	event.arg = Fcons ((uname == NULL ? Qnil : build_string (uname)),
			   event.arg);
	event.arg = Fcons (make_fixnum_or_float (serial), event.arg);
	event.arg = Fcons (make_number (mtype), event.arg);

	/* Add the bus symbol to the event.  */
	event.arg = Fcons (bus, event.arg);

	/* Store it into the input event queue.  */
	kbd_buffer_store_event (&event);

	XD_DEBUG_MESSAGE ("Event stored: %s",
			  SDATA (format2 ("%s", event.arg, Qnil)));

	/* Cleanup.  */
cleanup:
	dbus_message_unref (dmessage);

	UNGCPRO;
}
#endif

/* Read queued incoming messages of the D-Bus BUS.
   BUS is either a Lisp symbol, :system or :session, or a string denoting
   the bus address.  */
static Lisp_Object
xd_read_message (Lisp_Object bus)
{
	/* Open a connection to the bus.  */
	DBusConnection *connection = xd_initialize (bus, TRUE);

	/* Non blocking read of the next available message.  */
	dbus_connection_read_write (connection, 0);

	while (dbus_connection_get_dispatch_status (connection)
	       != DBUS_DISPATCH_COMPLETE)
		xd_read_message_1 (connection, bus);
	return Qnil;
}

/* Callback called when something is ready to read or write.  */
static void
xd_read_queued_messages (int fd, void *data, int for_read)
{
	Lisp_Object busp = Vdbus_registered_buses;
	Lisp_Object bus = Qnil;

	/* Find bus related to fd.  */
	if (data != NULL)
		while (!NILP (busp))
		{
			if ((SYMBOLP (CAR_SAFE (busp)) && XSYMBOL (CAR_SAFE (busp)) == data)
			    || (STRINGP (CAR_SAFE (busp)) && XSTRING (CAR_SAFE (busp)) == data))
				bus = CAR_SAFE (busp);
			busp = CDR_SAFE (busp);
		}

	if (NILP (bus))
		return;

	/* We ignore all Lisp errors during the call.  */
	xd_in_read_queued_messages = 1;
	internal_catch (Qdbus_error, xd_read_message, bus, 0);
	xd_in_read_queued_messages = 0;
}

DEFUN("dbus-register-service", Fdbus_register_service, 2, MANY, 0, /*
Register known name SERVICE on the D-Bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name that should be registered.  It must
be a known name.

FLAGS are keywords, which control how the service name is registered.
The following keywords are recognized:

`:allow-replacement': Allow another service to become the primary
owner if requested.

`:replace-existing': Request to replace the current primary owner.

`:do-not-queue': If we can not become the primary owner do not place
us in the queue.

The function returns a keyword, indicating the result of the
operation.  One of the following keywords is returned:

`:primary-owner': Service has become the primary owner of the
requested name.

`:in-queue': Service could not become the primary owner and has been
placed in the queue.

`:exists': Service is already in the queue.

`:already-owner': Service is already the primary owner.

Example:

\(dbus-register-service :session dbus-service-emacs)

=> :primary-owner.

\(dbus-register-service
:session "org.freedesktop.TextEditor"
dbus-service-allow-replacement dbus-service-replace-existing)

=> :already-owner.

usage: (dbus-register-service BUS SERVICE &rest FLAGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service;
	DBusConnection *connection;
	int i;
	unsigned int value;
	unsigned int flags = 0;
	int result;
	DBusError derror;

	bus = args[0];
	service = args[1];

	/* Check parameters.  */
	CHECK_STRING (service);

	/* Process flags.  */
	for (i = 2; i < nargs; ++i) {
		value = ((EQ (args[i], QCdbus_request_name_replace_existing))
			 ? DBUS_NAME_FLAG_REPLACE_EXISTING
			 : (EQ (args[i], QCdbus_request_name_allow_replacement))
			 ? DBUS_NAME_FLAG_ALLOW_REPLACEMENT
			 : (EQ (args[i], QCdbus_request_name_do_not_queue))
			 ? DBUS_NAME_FLAG_DO_NOT_QUEUE
			 : -1);
		if (value == -1)
			XD_SIGNAL2 (build_string ("Unrecognized name request flag"), args[i]);
		flags |= value;
	}

	/* Open a connection to the bus.  */
	connection = xd_initialize (bus, TRUE);

	/* Request the known name from the bus.  */
	dbus_error_init (&derror);
	result = dbus_bus_request_name (connection, SSDATA (service), flags,
					&derror);
	if (dbus_error_is_set (&derror))
		XD_ERROR (derror);

	/* Cleanup.  */
	dbus_error_free (&derror);

	/* Return object.  */
	switch (result)
	{
	case DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER:
		return QCdbus_request_name_reply_primary_owner;
	case DBUS_REQUEST_NAME_REPLY_IN_QUEUE:
		return QCdbus_request_name_reply_in_queue;
	case DBUS_REQUEST_NAME_REPLY_EXISTS:
		return QCdbus_request_name_reply_exists;
	case DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER:
		return QCdbus_request_name_reply_already_owner;
	default:
		/* This should not happen.  */
		XD_SIGNAL2 (build_string ("Could not register service"), service);
	}
}

DEFUN("dbus-register-signal", Fdbus_register_signal, 6, MANY, 0, /*
Register for signal SIGNAL on the D-Bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name used by the sending D-Bus object.
It can be either a known name or the unique name of the D-Bus object
sending the signal.  When SERVICE is nil, related signals from all
D-Bus objects shall be accepted.

PATH is the D-Bus object path SERVICE is registered.  It can also be
nil if the path name of incoming signals shall not be checked.

INTERFACE is an interface offered by SERVICE.  It must provide SIGNAL.
HANDLER is a Lisp function to be called when the signal is received.
It must accept as arguments the values SIGNAL is sending.

All other arguments ARGS, if specified, must be strings.  They stand
for the respective arguments of the signal in their order, and are
used for filtering as well.  A nil argument might be used to preserve
the order.

INTERFACE, SIGNAL and HANDLER must not be nil.  Example:

\(defun my-signal-handler (device)
(message "Device %s added" device))

\(dbus-register-signal
:system "org.freedesktop.Hal" "/org/freedesktop/Hal/Manager"
"org.freedesktop.Hal.Manager" "DeviceAdded" 'my-signal-handler)

=> ((:system "org.freedesktop.Hal.Manager" "DeviceAdded")
("org.freedesktop.Hal" "/org/freedesktop/Hal/Manager" my-signal-handler))

`dbus-register-signal' returns an object, which can be used in
`dbus-unregister-object' for removing the registration.

usage: (dbus-register-signal BUS SERVICE PATH INTERFACE SIGNAL HANDLER &rest ARGS)
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object bus, service, path, interface, signal, handler;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
	Lisp_Object uname, key, key1, value;
	DBusConnection *connection;
	int i;
	char rule[DBUS_MAXIMUM_MATCH_RULE_LENGTH];
	int rulelen;
	DBusError derror;

	/* Check parameters.  */
	bus = args[0];
	service = args[1];
	path = args[2];
	interface = args[3];
	signal = args[4];
	handler = args[5];

	if (!NILP (service)) CHECK_STRING (service);
	if (!NILP (path)) CHECK_STRING (path);
	CHECK_STRING (interface);
	CHECK_STRING (signal);
	if (!FUNCTIONP (handler))
		wrong_type_argument (Qinvalid_function, handler);
	GCPRO6 (bus, service, path, interface, signal, handler);

	/* Retrieve unique name of service.  If service is a known name, we
	   will register for the corresponding unique name, if any.  Signals
	   are sent always with the unique name as sender.  Note: the unique
	   name of "org.freedesktop.DBus" is that string itself.  */
	if ((STRINGP (service))
	    && (SBYTES (service) > 0)
	    && (strcmp (SSDATA (service), DBUS_SERVICE_DBUS) != 0)
	    && (strncmp (SSDATA (service), ":", 1) != 0))
		uname = call2 (intern ("dbus-get-name-owner"), bus, service);
	else
		uname = service;

	/* Create a matching rule if the unique name exists (when no
	   wildcard).  */
	if (NILP (uname) || (SBYTES (uname) > 0))
	{
		/* Open a connection to the bus.  */
		connection = xd_initialize (bus, TRUE);

		/* Create a rule to receive related signals.  */
		rulelen = snprintf (rule, sizeof rule,
				    "type='signal',interface='%s',member='%s'",
				    SDATA (interface),
				    SDATA (signal));
		if (! (0 <= rulelen && rulelen < sizeof rule))
			string_overflow ();

		/* Add unique name and path to the rule if they are non-nil.  */
		if (!NILP (uname))
		{
			int len = snprintf (rule + rulelen, sizeof rule - rulelen,
					    ",sender='%s'", SDATA (uname));
			if (! (0 <= len && len < sizeof rule - rulelen))
				string_overflow ();
			rulelen += len;
		}

		if (!NILP (path))
		{
			int len = snprintf (rule + rulelen, sizeof rule - rulelen,
					    ",path='%s'", SDATA (path));
			if (! (0 <= len && len < sizeof rule - rulelen))
				string_overflow ();
			rulelen += len;
		}

		/* Add arguments to the rule if they are non-nil.  */
		for (i = 6; i < nargs; ++i)
			if (!NILP (args[i]))
			{
				int len;
				CHECK_STRING (args[i]);
				len = snprintf (rule + rulelen, sizeof rule - rulelen,
						",arg%"pD"d='%s'", i - 6, SDATA (args[i]));
				if (! (0 <= len && len < sizeof rule - rulelen))
					string_overflow ();
				rulelen += len;
			}

		/* Add the rule to the bus.  */
		dbus_error_init (&derror);
		dbus_bus_add_match (connection, rule, &derror);
		if (dbus_error_is_set (&derror))
		{
			UNGCPRO;
			XD_ERROR (derror);
		}

		/* Cleanup.  */
		dbus_error_free (&derror);

		XD_DEBUG_MESSAGE ("Matching rule \"%s\" created", rule);
	}

	/* Create a hash table entry.  */
	key = list3 (bus, interface, signal);
	key1 = list5 (uname, service, path, handler, build_string (rule));
	value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

	if (NILP (Fmember (key1, value)))
		Fputhash (key, Fcons (key1, value), Vdbus_registered_objects_table);

	/* Return object.  */
	RETURN_UNGCPRO (list2 (key, list3 (service, path, handler)));
}

DEFUN("dbus-register-method", Fdbus_register_method, 6, 7, 0, /*
Register for method METHOD on the D-Bus BUS.

BUS is either a Lisp symbol, `:system' or `:session', or a string
denoting the bus address.

SERVICE is the D-Bus service name of the D-Bus object METHOD is
registered for.  It must be a known name (See discussion of
DONT-REGISTER-SERVICE below).

PATH is the D-Bus object path SERVICE is registered (See discussion of
DONT-REGISTER-SERVICE below).  INTERFACE is the interface offered by
SERVICE.  It must provide METHOD.  HANDLER is a Lisp function to be
called when a method call is received.  It must accept the input
arguments of METHOD.  The return value of HANDLER is used for
composing the returning D-Bus message.

When DONT-REGISTER-SERVICE is non-nil, the known name SERVICE is not
registered.  This means that other D-Bus clients have no way of
noticing the newly registered method.  When interfaces are constructed
incrementally by adding single methods or properties at a time,
DONT-REGISTER-SERVICE can be used to prevent other clients from
discovering the still incomplete interface.
*/
      (bus, service, path, interface, method, handler,
       dont_register_service))
{
	Lisp_Object key, key1, value;
	Lisp_Object args[2] = { bus, service };

	/* Check parameters.  */
	CHECK_STRING (service);
	CHECK_STRING (path);
	CHECK_STRING (interface);
	CHECK_STRING (method);
	if (!FUNCTIONP (handler))
		wrong_type_argument (Qinvalid_function, handler);
	/* TODO: We must check for a valid service name, otherwise there is
	   a segmentation fault.  */

	/* Request the name.  */
	if (NILP (dont_register_service))
		Fdbus_register_service (2, args);

	/* Create a hash table entry.  We use nil for the unique name,
	   because the method might be called from anybody.  */
	key = list3 (bus, interface, method);
	key1 = list4 (Qnil, service, path, handler);
	value = Fgethash (key, Vdbus_registered_objects_table, Qnil);

	if (NILP (Fmember (key1, value)))
		Fputhash (key, Fcons (key1, value), Vdbus_registered_objects_table);

	/* Return object.  */
	return list2 (key, list3 (service, path, handler));
}


void
syms_of_dbusbind (void)
{

	defsymbol(&Qdbus_init_bus, "dbus-init-bus");
	DEFSUBR(Fdbus_init_bus);

	defsymbol(&Qdbus_close_bus, "dbus-close-bus");
	DEFSUBR(Fdbus_close_bus);

	defsymbol(&Qdbus_get_unique_name, "dbus-get-unique-name");
	DEFSUBR(Fdbus_get_unique_name);

	defsymbol(&Qdbus_call_method, "dbus-call-method");
	DEFSUBR(Fdbus_call_method);

	defsymbol(&Qdbus_call_method_asynchronously, "dbus-call-method-asynchronously");
	DEFSUBR(Fdbus_call_method_asynchronously);

	defsymbol(&Qdbus_method_return_internal, "dbus-method-return-internal");
	DEFSUBR(Fdbus_method_return_internal);

	defsymbol(&Qdbus_method_error_internal, "dbus-method-error-internal");
	DEFSUBR(Fdbus_method_error_internal);

	defsymbol(&Qdbus_send_signal, "dbus-send-signal");
	DEFSUBR(Fdbus_send_signal);

	defsymbol(&Qdbus_register_service, "dbus-register-service");
	DEFSUBR(Fdbus_register_service);

	defsymbol(&Qdbus_register_signal, "dbus-register-signal");
	DEFSUBR(Fdbus_register_signal);

	defsymbol(&Qdbus_register_method, "dbus-register-method");
	DEFSUBR(Fdbus_register_method);

	defsymbol(&Qdbus_error, "dbus-error");
	Fput (Qdbus_error, Qerror_conditions,
	      list2 (Qdbus_error, Qerror));
	Fput (Qdbus_error, Qerror_message,
	      make_pure_c_string ("D-Bus error"));

	defsymbol(&QCdbus_system_bus, ":system");
	defsymbol(&QCdbus_session_bus, ":session");
	defsymbol(&QCdbus_request_name_allow_replacement, ":allow-replacement");
	defsymbol(&QCdbus_request_name_replace_existing, ":replace-existing");
	defsymbol(&QCdbus_request_name_do_not_queue, ":do-not-queue");
	defsymbol(&QCdbus_request_name_reply_primary_owner, ":primary-owner");
	defsymbol(&QCdbus_request_name_reply_exists, ":exists");
	defsymbol(&QCdbus_request_name_reply_in_queue, ":in-queue");
	defsymbol(&QCdbus_request_name_reply_already_owner, ":already-owner");
	defsymbol(&QCdbus_timeout, ":timeout");
	defsymbol(&QCdbus_type_byte, ":byte");
	defsymbol(&QCdbus_type_boolean, ":boolean");
	defsymbol(&QCdbus_type_int16, ":int16");
	defsymbol(&QCdbus_type_uint16, ":uint16");
	defsymbol(&QCdbus_type_int32, ":int32");
	defsymbol(&QCdbus_type_uint32, ":uint32");
	defsymbol(&QCdbus_type_int64, ":int64");
	defsymbol(&QCdbus_type_uint64, ":uint64");
	defsymbol(&QCdbus_type_double, ":double");
	defsymbol(&QCdbus_type_string, ":string");
	defsymbol(&QCdbus_type_object_path, ":object-path");
	defsymbol(&QCdbus_type_signature, ":signature");

#ifdef DBUS_TYPE_UNIX_FD
	defsymbol(&QCdbus_type_unix_fd, ":unix-fd");
#endif

	defsymbol(&QCdbus_type_array, ":array");
	defsymbol(&QCdbus_type_variant, ":variant");
	defsymbol(&QCdbus_type_struct, ":struct");
	defsymbol(&QCdbus_type_dict_entry, ":dict-entry");

	DEFVAR_LISP ("dbus-registered-buses", &Vdbus_registered_buses /*
List of D-Bus buses we are polling for messages.
*/);
	Vdbus_registered_buses = Qnil;

	DEFVAR_LISP ("dbus-registered-objects-table",
		     &Vdbus_registered_objects_table /*
Hash table of registered functions for D-Bus.

There are two different uses of the hash table: for accessing
registered interfaces properties, targeted by signals or method calls,
and for calling handlers in case of non-blocking method call returns.

In the first case, the key in the hash table is the list (BUS
INTERFACE MEMBER).  BUS is either a Lisp symbol, `:system' or
`:session', or a string denoting the bus address.  INTERFACE is a
string which denotes a D-Bus interface, and MEMBER, also a string, is
either a method, a signal or a property INTERFACE is offering.  All
arguments but BUS must not be nil.

The value in the hash table is a list of quadruple lists
\((UNAME SERVICE PATH OBJECT) (UNAME SERVICE PATH OBJECT) ...).
SERVICE is the service name as registered, UNAME is the corresponding
unique name.  In case of registered methods and properties, UNAME is
nil.  PATH is the object path of the sending object.  All of them can
be nil, which means a wildcard then.  OBJECT is either the handler to
be called when a D-Bus message, which matches the key criteria,
arrives (methods and signals), or a cons cell containing the value of
the property.

For signals, there is also a fifth element RULE, which keeps the match
string the signal is registered with.

In the second case, the key in the hash table is the list (BUS
SERIAL).  BUS is either a Lisp symbol, `:system' or `:session', or a
string denoting the bus address.  SERIAL is the serial number of the
non-blocking method call, a reply is expected.  Both arguments must
not be nil.  The value in the hash table is HANDLER, the function to
be called when the D-Bus reply message arrives.
*/);
	{
		Lisp_Object args[2];
		args[0] = Q_test;
		args[1] = Qequal;
		Vdbus_registered_objects_table = Fmake_hash_table (2, args);
	}

	DEFVAR_LISP ("dbus-debug", &Vdbus_debug /*
If non-nil, debug messages of D-Bus bindings are raised.
*/);
#ifdef DBUS_DEBUG
	Vdbus_debug = Qt;
	/* We can also set environment variable DBUS_VERBOSE=1 in order to
	   see more traces.  This requires libdbus-1 to be configured with
	   --enable-verbose-mode.  */
#else
	Vdbus_debug = Qnil;
#endif

	Fprovide(intern("dbusbind"));

}

#endif /* HAVE_DBUS */

/* dbusbind.c ends here */
