/* dbusbind.h -- Definitions used in and for dbusbind.c  */

/* 
 * Copyright (C) 2012 Steve Youngs
 */

/*
 * Time-stamp: <Monday Jan 23, 2012 02:10:12 steve>
 * Created:    <2012-01-22>  
 * Author:     Steve Youngs <steve@sxemacs.org>  
 * Maintainer: Steve Youngs <steve@sxemacs.org>  
 * Homepage:   http://www.sxemacs.org/  
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

/*
 * Commentary:
 *
 *   Herein lives 2 categories of stuff...
 *
 *      1) Stuff to keep dbusbind.c happy.
 *      2) Stuff lifted from GNU/Emacs to allow for #1
 *
 *   Some of the things from #2 may turn out to be useful in other
 *   areas of SXEmacs, and if/when that happens feel free to move it
 *   from here to a more appropiate place like lisp.h.
 */


/* Code: */
#ifndef _DBUSBIND_H
#define _DBUSBIND_H

/* The category 2 stuff (things that might one day live elsewhere) */
/* From GNU/Emacs */
/* Take the car or cdr of something whose type is not known.  */
#define CAR(c)					\
 (CONSP ((c)) ? XCAR ((c))			\
  : NILP ((c)) ? Qnil				\
  : wrong_type_argument (Qlistp, (c)))

#define CDR(c)					\
 (CONSP ((c)) ? XCDR ((c))			\
  : NILP ((c)) ? Qnil				\
  : wrong_type_argument (Qlistp, (c)))

/* Take the car or cdr of something whose type is not known.  */
#define CAR_SAFE(c)				\
  (CONSP ((c)) ? XCAR ((c)) : Qnil)

#define CDR_SAFE(c)				\
  (CONSP ((c)) ? XCDR ((c)) : Qnil)

/* Convenience macros for dealing with Lisp strings.  */

#define SDATA(string)		(XSTRING (string)->data + 0)
#define SREF(string, index)	(SDATA (string)[index] + 0)
#define SSET(string, index, new) (SDATA (string)[index] = (new))
#define SCHARS(string)		(XSTRING (string)->size + 0)
#define SBYTES(string)		(STRING_BYTES (XSTRING (string)) + 0)

/* Avoid "differ in sign" warnings.  */
#define SSDATA(x)  ((char *) SDATA (x))

#define STRING_SET_CHARS(string, newsize) \
    (XSTRING (string)->size = (newsize))

#define STRING_COPYIN(string, index, new, count) \
    memcpy (SDATA (string) + index, new, count)

/* For integers known to be positive, XFASTINT provides fast retrieval
   and XSETFASTINT provides fast storage.  This takes advantage of the
   fact that Lisp_Int is 0.  */
#define XFASTINT(a) ((a) + 0)
#define XSETFASTINT(a, b) ((a) = (b))



/* Return a fixnum or float, depending on whether VAL fits in a Lisp
   fixnum.  */
#define make_fixnum_or_float(val) \
   (NUMBER_FITS_IN_AN_EMACS_INT(val) ? make_float(val) : make_int(val))

#define make_number(N)  make_int(N)

#define NO_RETURN
extern void xsignal (Lisp_Object, Lisp_Object) NO_RETURN;
extern void xsignal0 (Lisp_Object) NO_RETURN;
extern void xsignal1 (Lisp_Object, Lisp_Object) NO_RETURN;
extern void xsignal2 (Lisp_Object, Lisp_Object, Lisp_Object) NO_RETURN;
extern void xsignal3 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object) NO_RETURN;

extern Lisp_Object format2 (const char *, Lisp_Object, Lisp_Object);

/* End From GNU/Emacs */

/* Evilness:
 * Fmake_hash_table shouldn't be used within C.  I'm only
 * doing this so dbusbind.c will compile.
 */
EXFUN(Fmake_hash_table, MANY);

/* The category 1 stuff  */
static Lisp_Object Qdbus_init_bus;
static Lisp_Object Qdbus_close_bus;
static Lisp_Object Qdbus_get_unique_name;
static Lisp_Object Qdbus_call_method;
static Lisp_Object Qdbus_call_method_asynchronously;
static Lisp_Object Qdbus_method_return_internal;
static Lisp_Object Qdbus_method_error_internal;
static Lisp_Object Qdbus_send_signal;
static Lisp_Object Qdbus_register_service;
static Lisp_Object Qdbus_register_signal;
static Lisp_Object Qdbus_register_method;

/* D-Bus error symbol.  */
static Lisp_Object Qdbus_error;

/* Lisp symbols of the system and session buses.  */
static Lisp_Object QCdbus_system_bus, QCdbus_session_bus;

/* Lisp symbol for method call timeout.  */
static Lisp_Object QCdbus_timeout;

/* Lisp symbols for name request flags.  */
static Lisp_Object QCdbus_request_name_allow_replacement;
static Lisp_Object QCdbus_request_name_replace_existing;
static Lisp_Object QCdbus_request_name_do_not_queue;

/* Lisp symbols for name request replies.  */
static Lisp_Object QCdbus_request_name_reply_primary_owner;
static Lisp_Object QCdbus_request_name_reply_in_queue;
static Lisp_Object QCdbus_request_name_reply_exists;
static Lisp_Object QCdbus_request_name_reply_already_owner;

/* Lisp symbols of D-Bus types.  */
static Lisp_Object QCdbus_type_byte, QCdbus_type_boolean;
static Lisp_Object QCdbus_type_int16, QCdbus_type_uint16;
static Lisp_Object QCdbus_type_int32, QCdbus_type_uint32;
static Lisp_Object QCdbus_type_int64, QCdbus_type_uint64;
static Lisp_Object QCdbus_type_double, QCdbus_type_string;
static Lisp_Object QCdbus_type_object_path, QCdbus_type_signature;
#ifdef DBUS_TYPE_UNIX_FD
static Lisp_Object QCdbus_type_unix_fd;
#endif
static Lisp_Object QCdbus_type_array, QCdbus_type_variant;
static Lisp_Object QCdbus_type_struct, QCdbus_type_dict_entry;
static Lisp_Object Vdbus_debug, Vdbus_registered_buses;
static Lisp_Object Vdbus_registered_objects_table;
Lisp_Object Q_test;

#define string_overflow(args...)    error("Maximum string size exceeded")

/* We use "xd_" and "XD_" as prefix for all internal symbols, because
   we don't want to poison other namespaces with "dbus_".  */

/* Raise a signal.  If we are reading events, we cannot signal; we
   throw to xd_read_queued_messages then.  */
#define XD_SIGNAL1(arg)					\
	do {						\
		if (xd_in_read_queued_messages)		\
			Fthrow (Qdbus_error, Qnil);	\
		else					\
			xsignal1 (Qdbus_error, arg);	\
	} while (0)

#define XD_SIGNAL2(arg1, arg2)					\
	do {							\
		if (xd_in_read_queued_messages)			\
			Fthrow (Qdbus_error, Qnil);		\
		else						\
			xsignal2 (Qdbus_error, arg1, arg2);	\
	} while (0)

#define XD_SIGNAL3(arg1, arg2, arg3)					\
	do {								\
		if (xd_in_read_queued_messages)				\
			Fthrow (Qdbus_error, Qnil);			\
		else							\
			xsignal3 (Qdbus_error, arg1, arg2, arg3);	\
	} while (0)

/* Raise a Lisp error from a D-Bus ERROR.  */
#define XD_ERROR(error)							\
	do {								\
		/* Remove the trailing newline.  */			\
		char const *mess = error.message;			\
		char const *nl = strchr (mess, '\n');			\
		Lisp_Object err = make_string (mess, nl ? nl - mess : strlen (mess)); \
		dbus_error_free (&error);				\
		XD_SIGNAL1 (err);					\
	} while (0)

/* Macros for debugging.  In order to enable them, build with
   "MYCPPFLAGS='-DDBUS_DEBUG -Wall' make".  */
#ifdef DBUS_DEBUG
#define XD_DEBUG_MESSAGE(...)				\
	do {						\
		char s[1024];				\
		snprintf (s, sizeof s, __VA_ARGS__);	\
		printf ("%s: %s\n", __func__, s);	\
		message ("%s: %s", __func__, s);	\
	} while (0)
#define XD_DEBUG_VALID_LISP_OBJECT_P(object)				\
	do {								\
		if (!valid_lisp_object_p (object))			\
		{							\
			XD_DEBUG_MESSAGE ("%d Assertion failure", __LINE__); \
			XD_SIGNAL1 (build_string ("Assertion failure")); \
		}							\
	} while (0)

#else /* !DBUS_DEBUG */
#define XD_DEBUG_MESSAGE(...)					\
	do {							\
		if (!NILP (Vdbus_debug))			\
		{						\
			char s[1024];				\
			snprintf (s, 1023, __VA_ARGS__);	\
			message ("%s: %s", __func__, s);	\
		}						\
	} while (0)
#define XD_DEBUG_VALID_LISP_OBJECT_P(object)
#endif

/* Check whether TYPE is a basic DBusType.  */
#ifdef DBUS_TYPE_UNIX_FD
#define XD_BASIC_DBUS_TYPE(type)		\
	((type ==  DBUS_TYPE_BYTE)		\
	 || (type ==  DBUS_TYPE_BOOLEAN)	\
	 || (type ==  DBUS_TYPE_INT16)		\
	 || (type ==  DBUS_TYPE_UINT16)		\
	 || (type ==  DBUS_TYPE_INT32)		\
	 || (type ==  DBUS_TYPE_UINT32)		\
	 || (type ==  DBUS_TYPE_INT64)		\
	 || (type ==  DBUS_TYPE_UINT64)		\
	 || (type ==  DBUS_TYPE_DOUBLE)		\
	 || (type ==  DBUS_TYPE_STRING)		\
	 || (type ==  DBUS_TYPE_OBJECT_PATH)	\
	 || (type ==  DBUS_TYPE_SIGNATURE)	\
	 || (type ==  DBUS_TYPE_UNIX_FD))
#else
#define XD_BASIC_DBUS_TYPE(type)		\
	((type ==  DBUS_TYPE_BYTE)		\
	 || (type ==  DBUS_TYPE_BOOLEAN)	\
	 || (type ==  DBUS_TYPE_INT16)		\
	 || (type ==  DBUS_TYPE_UINT16)		\
	 || (type ==  DBUS_TYPE_INT32)		\
	 || (type ==  DBUS_TYPE_UINT32)		\
	 || (type ==  DBUS_TYPE_INT64)		\
	 || (type ==  DBUS_TYPE_UINT64)		\
	 || (type ==  DBUS_TYPE_DOUBLE)		\
	 || (type ==  DBUS_TYPE_STRING)		\
	 || (type ==  DBUS_TYPE_OBJECT_PATH)	\
	 || (type ==  DBUS_TYPE_SIGNATURE))
#endif

/* Check whether a Lisp symbol is a predefined D-Bus type symbol.  */
#define XD_DBUS_TYPE_P(object)						\
	(SYMBOLP (object) && ((xd_symbol_to_dbus_type (object) != DBUS_TYPE_INVALID)))

/* Determine the DBusType of a given Lisp OBJECT.  It is used to
   convert Lisp objects, being arguments of `dbus-call-method' or
   `dbus-send-signal', into corresponding C values appended as
   arguments to a D-Bus message.  */
#define XD_OBJECT_TO_DBUS_TYPE(object)					\
	((EQ (object, Qt) || EQ (object, Qnil)) ? DBUS_TYPE_BOOLEAN	\
	 : (NATNUMP (object)) ? DBUS_TYPE_UINT32			\
	 : (INTEGERP (object)) ? DBUS_TYPE_INT32			\
	 : (FLOATP (object)) ? DBUS_TYPE_DOUBLE				\
	 : (STRINGP (object)) ? DBUS_TYPE_STRING			\
	 : (XD_DBUS_TYPE_P (object)) ? xd_symbol_to_dbus_type (object)	\
	 : (CONSP (object))						\
	 ? ((XD_DBUS_TYPE_P (CAR_SAFE (object)))			\
	    ? ((XD_BASIC_DBUS_TYPE (xd_symbol_to_dbus_type (CAR_SAFE (object)))) \
	       ? DBUS_TYPE_ARRAY					\
	       : xd_symbol_to_dbus_type (CAR_SAFE (object)))		\
	    : DBUS_TYPE_ARRAY)						\
	 : DBUS_TYPE_INVALID)

/* Return a list pointer which does not have a Lisp symbol as car.  */
#define XD_NEXT_VALUE(object)						\
	((XD_DBUS_TYPE_P (CAR_SAFE (object))) ? CDR_SAFE (object) : object)

/* Check whether X is a valid dbus serial number.  If valid, set
   SERIAL to its value.  Otherwise, signal an error. */
#define CHECK_DBUS_SERIAL_GET_SERIAL(x, serial)				\
	do								\
	{								\
		dbus_uint32_t DBUS_SERIAL_MAX = -1;			\
		if (NATNUMP (x) && XINT (x) <= DBUS_SERIAL_MAX)		\
			serial = XINT (x);				\
		else if (EMACS_INT_MAX < DBUS_SERIAL_MAX		\
			 && FLOATP (x)					\
			 && 0 <= XFLOAT_DATA (x)			\
			 && XFLOAT_DATA (x) <= DBUS_SERIAL_MAX)		\
			serial = XFLOAT_DATA (x);			\
		else							\
			XD_SIGNAL2 (build_string ("Invalid dbus serial"), x); \
	}								\
	while (0)



#endif



/* dbusbind.h ends here */
