/* Tooltalk support for Emacs.
   Copyright (C) 1993, 1994 Sun Microsystems, Inc.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

/* Synched up with: Not in FSF. */

/* Written by John Rose <john.rose@eng.sun.com>.
   Heavily modified and cleaned up by Ben Wing <ben@xemacs.org>. */

#include <config.h>
#include "lisp.h"

#include <X11/Xlib.h>

#include "buffer.h"
#include "elhash.h"
#include "process.h"
#include "tooltalk.h"
#include "syssignal.h"

Lisp_Object Vtooltalk_fd;

#ifdef TT_DEBUG
static FILE *tooltalk_log_file;
#endif

static Lisp_Object
  Vtooltalk_message_handler_hook,
  Vtooltalk_pattern_handler_hook,
  Vtooltalk_unprocessed_message_hook;

static Lisp_Object
  Qtooltalk_message_handler_hook,
  Qtooltalk_pattern_handler_hook,
  Qtooltalk_unprocessed_message_hook;

static Lisp_Object
  Qreceive_tooltalk_message,
  Qtt_address,
  Qtt_args_count,
  Qtt_arg_bval,
  Qtt_arg_ival,
  Qtt_arg_mode,
  Qtt_arg_type,
  Qtt_arg_val,
  Qtt_class,
  Qtt_category,
  Qtt_disposition,
  Qtt_file,
  Qtt_gid,
  Qtt_handler,
  Qtt_handler_ptype,
  Qtt_object,
  Qtt_op,
  Qtt_opnum,
  Qtt_otype,
  Qtt_scope,
  Qtt_sender,
  Qtt_sender_ptype,
  Qtt_session,
  Qtt_state,
  Qtt_status,
  Qtt_status_string,
  Qtt_uid,
  Qtt_callback,
  Qtt_plist,
  Qtt_prop,

  Qtt_reject,                /* return-tooltalk-message */
  Qtt_reply,
  Qtt_fail,

  Q_TT_MODE_UNDEFINED,       /* enum Tt_mode */
  Q_TT_IN,
  Q_TT_OUT,
  Q_TT_INOUT,
  Q_TT_MODE_LAST,

  Q_TT_SCOPE_NONE,            /* enum Tt_scope */
  Q_TT_SESSION,
  Q_TT_FILE,
  Q_TT_BOTH,
  Q_TT_FILE_IN_SESSION,

  Q_TT_CLASS_UNDEFINED,       /* enum Tt_class */
  Q_TT_NOTICE,
  Q_TT_REQUEST,
  Q_TT_CLASS_LAST,

  Q_TT_CATEGORY_UNDEFINED,    /* enum Tt_category */
  Q_TT_OBSERVE,
  Q_TT_HANDLE,
  Q_TT_CATEGORY_LAST,

  Q_TT_PROCEDURE,             /* typedef enum Tt_address */
  Q_TT_OBJECT,
  Q_TT_HANDLER,
  Q_TT_OTYPE,
  Q_TT_ADDRESS_LAST,

  Q_TT_CREATED,               /* enum Tt_state */
  Q_TT_SENT,
  Q_TT_HANDLED,
  Q_TT_FAILED,
  Q_TT_QUEUED,
  Q_TT_STARTED,
  Q_TT_REJECTED,
  Q_TT_STATE_LAST,

  Q_TT_DISCARD,              /* enum Tt_disposition */
  Q_TT_QUEUE,
  Q_TT_START;

static Lisp_Object Tooltalk_Message_plist_str, Tooltalk_Pattern_plist_str;

Lisp_Object Qtooltalk_error;

/* Used to GCPRO tooltalk message and pattern objects while
   they're sitting inside of some active tooltalk message or pattern.
   There may not be any other pointers to these objects. */
Lisp_Object Vtooltalk_message_gcpro, Vtooltalk_pattern_gcpro;


/*                                     */
/* machinery for tooltalk-message type */
/*                                     */

Lisp_Object Qtooltalk_messagep;

struct Lisp_Tooltalk_Message
{
  struct lcrecord_header header;
  Lisp_Object plist_sym, callback;
  Tt_message m;
};

static Lisp_Object
mark_tooltalk_message (Lisp_Object obj)
{
  mark_object (XTOOLTALK_MESSAGE (obj)->callback);
  return XTOOLTALK_MESSAGE (obj)->plist_sym;
}

static void
print_tooltalk_message (Lisp_Object obj, Lisp_Object printcharfun,
			int escapeflag)
{
  Lisp_Tooltalk_Message *p = XTOOLTALK_MESSAGE (obj);

  char buf[200];

  if (print_readably)
    error ("printing unreadable object #<tooltalk_message 0x%x>",
	   p->header.uid);

  sprintf (buf, "#<tooltalk_message id:0x%lx 0x%x>", (long) (p->m), p->header.uid);
  write_c_string (buf, printcharfun);
}

DEFINE_LRECORD_IMPLEMENTATION ("tooltalk-message", tooltalk_message,
                               mark_tooltalk_message, print_tooltalk_message,
                               0, 0, 0, 0,
			       Lisp_Tooltalk_Message);

static Lisp_Object
make_tooltalk_message (Tt_message m)
{
  Lisp_Object val;
  Lisp_Tooltalk_Message *msg =
    alloc_lcrecord_type (Lisp_Tooltalk_Message, &lrecord_tooltalk_message);

  msg->m = m;
  msg->callback = Qnil;
  msg->plist_sym = Fmake_symbol (Tooltalk_Message_plist_str);
  XSETTOOLTALK_MESSAGE (val, msg);
  return val;
}

Tt_message
unbox_tooltalk_message (Lisp_Object msg)
{
  CHECK_TOOLTALK_MESSAGE (msg);
  return XTOOLTALK_MESSAGE (msg)->m;
}

DEFUN ("tooltalk-message-p", Ftooltalk_message_p, 1, 1, 0, /*
Return non-nil if OBJECT is a tooltalk message.
*/
       (object))
{
  return TOOLTALK_MESSAGEP (object) ? Qt : Qnil;
}




/*                                     */
/* machinery for tooltalk-pattern type */
/*                                     */

Lisp_Object Qtooltalk_patternp;

struct Lisp_Tooltalk_Pattern
{
  struct lcrecord_header header;
  Lisp_Object plist_sym, callback;
  Tt_pattern p;
};

static Lisp_Object
mark_tooltalk_pattern (Lisp_Object obj)
{
  mark_object (XTOOLTALK_PATTERN (obj)->callback);
  return XTOOLTALK_PATTERN (obj)->plist_sym;
}

static void
print_tooltalk_pattern (Lisp_Object obj, Lisp_Object printcharfun,
			int escapeflag)
{
  Lisp_Tooltalk_Pattern *p = XTOOLTALK_PATTERN (obj);

  char buf[200];

  if (print_readably)
    error ("printing unreadable object #<tooltalk_pattern 0x%x>",
	   p->header.uid);

  sprintf (buf, "#<tooltalk_pattern id:0x%lx 0x%x>", (long) (p->p), p->header.uid);
  write_c_string (buf, printcharfun);
}

DEFINE_LRECORD_IMPLEMENTATION ("tooltalk-pattern", tooltalk_pattern,
                               mark_tooltalk_pattern, print_tooltalk_pattern,
                               0, 0, 0, 0,
			       Lisp_Tooltalk_Pattern);

static Lisp_Object
make_tooltalk_pattern (Tt_pattern p)
{
  Lisp_Tooltalk_Pattern *pat =
    alloc_lcrecord_type (Lisp_Tooltalk_Pattern, &lrecord_tooltalk_pattern);
  Lisp_Object val;

  pat->p = p;
  pat->callback = Qnil;
  pat->plist_sym = Fmake_symbol (Tooltalk_Pattern_plist_str);

  XSETTOOLTALK_PATTERN (val, pat);
  return val;
}

static Tt_pattern
unbox_tooltalk_pattern (Lisp_Object pattern)
{
  CHECK_TOOLTALK_PATTERN (pattern);
  return XTOOLTALK_PATTERN (pattern)->p;
}

DEFUN ("tooltalk-pattern-p", Ftooltalk_pattern_p, 1, 1, 0, /*
Return non-nil if OBJECT is a tooltalk pattern.
*/
       (object))
{
  return TOOLTALK_PATTERNP (object) ? Qt : Qnil;
}




static int
tooltalk_constant_value (Lisp_Object s)
{
  if (INTP (s))
    return XINT (s);
  else if (SYMBOLP (s))
    return XINT (XSYMBOL (s)->value);
  else
    return 0;   /* should never occur */
}

static void
check_status (Tt_status st)
{
  if (tt_is_err (st))
    signal_error (Qtooltalk_error,
		  Fcons (build_string (tt_status_message (st)), Qnil));
}

DEFUN ("receive-tooltalk-message", Freceive_tooltalk_message, 0, 2, 0, /*
Run tt_message_receive().
This function is the process handler for the ToolTalk connection process.
*/
       (ignore1, ignore2))
{
  /* This function can GC */
  Tt_message mess = tt_message_receive ();
  Lisp_Object message_ = make_tooltalk_message (mess);
  struct gcpro gcpro1;

  GCPRO1 (message_);
  if (mess != NULL && !NILP (Vtooltalk_unprocessed_message_hook))
    va_run_hook_with_args (Qtooltalk_unprocessed_message_hook, 1, message_);
  UNGCPRO;

  /* see comment in event-stream.c about this return value. */
  return Qzero;
}

static Tt_callback_action
tooltalk_message_callback (Tt_message m, Tt_pattern p)
{
  /* This function can GC */
  Lisp_Object cb;
  Lisp_Object message_;
  Lisp_Object pattern;
  struct gcpro gcpro1, gcpro2;

#ifdef TT_DEBUG
  int i, j;

  fprintf (tooltalk_log_file, "message_cb: %d\n", m);
  fprintf (tooltalk_log_file, "op: %s (", tt_message_op (m));
  for (j = tt_message_args_count (m), i = 0; i < j; i++) {
    fprintf (tooltalk_log_file, "%s \"%s\"", tt_message_arg_type (m, i),
	    tt_message_arg_val (m, i));
    fprintf (tooltalk_log_file, "%s", i == j-1 ? ")" : ", ");
  }
  fprintf (tooltalk_log_file, "\n\n");
  fflush (tooltalk_log_file);
#endif

  VOID_TO_LISP (message_, tt_message_user (m, TOOLTALK_MESSAGE_KEY));
  pattern = make_tooltalk_pattern (p);
  cb = XTOOLTALK_MESSAGE (message_)->callback;
  GCPRO2 (message_, pattern);
  if (!NILP (Vtooltalk_message_handler_hook))
    va_run_hook_with_args (Qtooltalk_message_handler_hook, 2,
			   message_, pattern);

  if ((SYMBOLP (cb) && EQ (Qt, Ffboundp (cb))) ||
      (CONSP (cb) && EQ (Qlambda, Fcar (cb)) &&
       !NILP (Flistp (Fcar (Fcdr (cb))))))
    call2 (cb, message_, pattern);
  UNGCPRO;

  tt_message_destroy (m);
  Fremhash (message_, Vtooltalk_message_gcpro);

  return TT_CALLBACK_PROCESSED;
}

static Tt_callback_action
tooltalk_pattern_callback (Tt_message m, Tt_pattern p)
{
  /* This function can GC */
  Lisp_Object cb;
  Lisp_Object message_;
  Lisp_Object pattern;
  struct gcpro gcpro1, gcpro2;

#ifdef TT_DEBUG
  int i, j;

  fprintf (tooltalk_log_file, "pattern_cb: %d\n", m);
  fprintf (tooltalk_log_file, "op: %s (", tt_message_op (m));
  for (j = tt_message_args_count (m), i = 0; i < j; i++) {
    fprintf (tooltalk_log_file, "%s \"%s\"", tt_message_arg_type (m, i),
	    tt_message_arg_val (m, i));
    fprintf (tooltalk_log_file, "%s", i == j-1 ? ")" : ", ");
  }
  fprintf (tooltalk_log_file, "\n\n");
  fflush (tooltalk_log_file);
#endif

  message_ = make_tooltalk_message (m);
  VOID_TO_LISP (pattern, tt_pattern_user (p, TOOLTALK_PATTERN_KEY));
  cb = XTOOLTALK_PATTERN (pattern)->callback;
  GCPRO2 (message_, pattern);
  if (!NILP (Vtooltalk_pattern_handler_hook))
    va_run_hook_with_args (Qtooltalk_pattern_handler_hook, 2,
			   message_, pattern);

  if (SYMBOLP (cb) && EQ (Qt, Ffboundp (cb)))
    call2 (cb, message_, pattern);
  UNGCPRO;

  tt_message_destroy (m);
  return TT_CALLBACK_PROCESSED;
}

static Lisp_Object
tt_mode_symbol (Tt_mode n)
{
  switch (n)
    {
    case TT_MODE_UNDEFINED:	return Q_TT_MODE_UNDEFINED;
    case TT_IN:			return Q_TT_IN;
    case TT_OUT:		return Q_TT_OUT;
    case TT_INOUT:		return Q_TT_INOUT;
    case TT_MODE_LAST:		return Q_TT_MODE_LAST;
    default:			return Qnil;
    }
}

static Lisp_Object
tt_scope_symbol (Tt_scope n)
{
  switch (n)
    {
    case TT_SCOPE_NONE:		return Q_TT_SCOPE_NONE;
    case TT_SESSION:		return Q_TT_SESSION;
    case TT_FILE:		return Q_TT_FILE;
    case TT_BOTH:		return Q_TT_BOTH;
    case TT_FILE_IN_SESSION:	return Q_TT_FILE_IN_SESSION;
    default:			return Qnil;
    }
}


static Lisp_Object
tt_class_symbol (Tt_class n)
{
  switch (n)
    {
    case TT_CLASS_UNDEFINED:	return Q_TT_CLASS_UNDEFINED;
    case TT_NOTICE:		return Q_TT_NOTICE;
    case TT_REQUEST:		return Q_TT_REQUEST;
    case TT_CLASS_LAST:		return Q_TT_CLASS_LAST;
    default:			return Qnil;
    }
}

/*
 * This is not being used.  Is that a mistake or is this function
 * simply not necessary?
 */
#if 0
static Lisp_Object
tt_category_symbol (Tt_category n)
{
  switch (n)
    {
    case TT_CATEGORY_UNDEFINED:	return Q_TT_CATEGORY_UNDEFINED;
    case TT_OBSERVE:		return Q_TT_OBSERVE;
    case TT_HANDLE:		return Q_TT_HANDLE;
    case TT_CATEGORY_LAST:	return Q_TT_CATEGORY_LAST;
    default:			return Qnil;
    }
}
#endif /* 0 */

static Lisp_Object
tt_address_symbol (Tt_address n)
{
  switch (n)
    {
    case TT_PROCEDURE:		return Q_TT_PROCEDURE;
    case TT_OBJECT:		return Q_TT_OBJECT;
    case TT_HANDLER:		return Q_TT_HANDLER;
    case TT_OTYPE:		return Q_TT_OTYPE;
    case TT_ADDRESS_LAST:	return Q_TT_ADDRESS_LAST;
    default:			return Qnil;
    }
}

static Lisp_Object
tt_state_symbol (Tt_state n)
{
  switch (n)
    {
    case TT_CREATED:		return Q_TT_CREATED;
    case TT_SENT:		return Q_TT_SENT;
    case TT_HANDLED:		return Q_TT_HANDLED;
    case TT_FAILED:		return Q_TT_FAILED;
    case TT_QUEUED:		return Q_TT_QUEUED;
    case TT_STARTED:		return Q_TT_STARTED;
    case TT_REJECTED:		return Q_TT_REJECTED;
    case TT_STATE_LAST:		return Q_TT_STATE_LAST;
    default:			return Qnil;
    }
}

static Lisp_Object
tt_build_string (char *s)
{
  return build_string (s ? s : "");
}

static Lisp_Object
tt_opnum_string (int n)
{
  char buf[32];

  sprintf (buf, "%u", n);
  return build_string (buf);
}

static Lisp_Object
tt_message_arg_ival_string (Tt_message m, int n)
{
  char buf[32];
  int value;

  check_status (tt_message_arg_ival (m, n, &value));
  long_to_string (buf, value);
  return build_string (buf);
}

static Lisp_Object
tt_message_arg_bval_vector (Tt_message m, int n)
{
  /* !!#### This function has not been Mule-ized */
  Bufbyte *value;
  int len = 0;

  check_status (tt_message_arg_bval (m, n, &value, &len));

  return make_string (value, len);
}

DEFUN ("get-tooltalk-message-attribute", Fget_tooltalk_message_attribute,
       2, 3, 0, /*
Return the indicated Tooltalk message attribute.  Attributes are
identified by symbols with the same name (underscores and all) as the
suffix of the Tooltalk tt_message_<attribute> function that extracts the value.
String attribute values are copied, enumerated type values (except disposition)
are converted to symbols - e.g. TT_HANDLER is 'TT_HANDLER, uid and gid are
represented by fixnums (small integers), opnum is converted to a string,
and disposition is converted to a fixnum.  We convert opnum (a C int) to a
string, e.g. 123 => "123" because there's no guarantee that opnums will fit
within the range of Lisp integers.

Use the 'plist attribute instead of the C API 'user attribute
for user defined message data.  To retrieve the value of a message property
specify the indicator for argn.  For example to get the value of a property
called 'rflag, use
   (get-tooltalk-message-attribute message 'plist 'rflag)

To get the value of a message argument use one of the 'arg_val (strings),
'arg_ival (integers), or 'arg_bval (strings with embedded nulls), attributes.
For example to get the integer value of the third argument:

   (get-tooltalk-message-attribute message 'arg_ival 2)

As you can see, argument numbers are zero based.  The type of each argument
can be retrieved with the 'arg_type attribute; however, Tooltalk doesn't
define any semantics for the string value of 'arg_type.  Conventionally
"string" is used for strings and "int" for 32 bit integers.  Note that
Emacs Lisp stores the lengths of strings explicitly (unlike C) so treating the
value returned by 'arg_bval like a string is fine.
*/
       (message_, attribute, argn))
{
  Tt_message m = unbox_tooltalk_message (message_);
  int n = 0;

  CHECK_SYMBOL (attribute);
  if (EQ (attribute, (Qtt_arg_bval))  ||
      EQ (attribute, (Qtt_arg_ival))  ||
      EQ (attribute, (Qtt_arg_mode))  ||
      EQ (attribute, (Qtt_arg_type))  ||
      EQ (attribute, (Qtt_arg_val)))
    {
      CHECK_INT (argn);
      n = XINT (argn);
    }

  if (!VALID_TOOLTALK_MESSAGEP (m))
    return Qnil;

  else if (EQ (attribute, Qtt_arg_bval))
    return tt_message_arg_bval_vector (m, n);

  else if (EQ (attribute, Qtt_arg_ival))
    return tt_message_arg_ival_string (m, n);

  else if (EQ (attribute, Qtt_arg_mode))
    return tt_mode_symbol (tt_message_arg_mode (m, n));

  else if (EQ (attribute, Qtt_arg_type))
    return tt_build_string (tt_message_arg_type (m, n));

  else if (EQ (attribute, Qtt_arg_val))
    return tt_message_arg_bval_vector (m, n);

  else if (EQ (attribute, Qtt_args_count))
    return make_int (tt_message_args_count (m));

  else if (EQ (attribute, Qtt_address))
    return tt_address_symbol (tt_message_address (m));

  else if (EQ (attribute, Qtt_class))
    return tt_class_symbol (tt_message_class (m));

  else if (EQ (attribute, Qtt_disposition))
    return make_int (tt_message_disposition (m));

  else if (EQ (attribute, Qtt_file))
    return tt_build_string (tt_message_file (m));

  else if (EQ (attribute, Qtt_gid))
    return make_int (tt_message_gid (m));

  else if (EQ (attribute, Qtt_handler))
    return tt_build_string (tt_message_handler (m));

  else if (EQ (attribute, Qtt_handler_ptype))
    return tt_build_string (tt_message_handler_ptype (m));

  else if (EQ (attribute, Qtt_object))
    return tt_build_string (tt_message_object (m));

  else if (EQ (attribute, Qtt_op))
    return tt_build_string (tt_message_op (m));

  else if (EQ (attribute, Qtt_opnum))
    return tt_opnum_string (tt_message_opnum (m));

  else if (EQ (attribute, Qtt_otype))
    return tt_build_string (tt_message_otype (m));

  else if (EQ (attribute, Qtt_scope))
    return tt_scope_symbol (tt_message_scope (m));

  else if (EQ (attribute, Qtt_sender))
    return tt_build_string (tt_message_sender (m));

  else if (EQ (attribute, Qtt_sender_ptype))
    return tt_build_string (tt_message_sender_ptype (m));

  else if (EQ (attribute, Qtt_session))
    return tt_build_string (tt_message_session (m));

  else if (EQ (attribute, Qtt_state))
    return tt_state_symbol (tt_message_state (m));

  else if (EQ (attribute, Qtt_status))
    return make_int (tt_message_status (m));

  else if (EQ (attribute, Qtt_status_string))
    return tt_build_string (tt_message_status_string (m));

  else if (EQ (attribute, Qtt_uid))
    return make_int (tt_message_uid (m));

  else if (EQ (attribute, Qtt_callback))
    return XTOOLTALK_MESSAGE (message_)->callback;

  else if (EQ (attribute, Qtt_prop))
    return Fget (XTOOLTALK_MESSAGE (message_)->plist_sym, argn, Qnil);

  else if (EQ (attribute, Qtt_plist))
    return Fcopy_sequence (Fsymbol_plist
			   (XTOOLTALK_MESSAGE (message_)->plist_sym));

  else
    signal_simple_error ("Invalid value for `get-tooltalk-message-attribute'",
			 attribute);

  return Qnil;
}

DEFUN ("set-tooltalk-message-attribute", Fset_tooltalk_message_attribute,
       3, 4, 0, /*
Initialize one Tooltalk message attribute.

Attribute names and values are the same as for
`get-tooltalk-message-attribute'.  A property list is provided for user
data (instead of the 'user message attribute); see
`get-tooltalk-message-attribute'.

The value of callback should be the name of a function of one argument.
It will be applied to the message and matching pattern each time the state of the
message changes.  This is usually used to notice when the messages state has
changed to TT_HANDLED (or TT_FAILED), so that reply argument values
can be used.

If one of the argument attributes is specified, 'arg_val, 'arg_ival, or
'arg_bval then argn must be the number of an already created argument.
New arguments can be added to a message with add-tooltalk-message-arg.
*/
       (value, message_, attribute, argn))
{
  Tt_message m = unbox_tooltalk_message (message_);
  int n = 0;
  Tt_status (*fun_str) (Tt_message, const char *) = 0;

  CHECK_SYMBOL (attribute);

  if (EQ (attribute, (Qtt_arg_bval))  ||
      EQ (attribute, (Qtt_arg_ival))  ||
      EQ (attribute, (Qtt_arg_val)))
    {
      CHECK_INT (argn);
      n = XINT (argn);
    }

  if (!VALID_TOOLTALK_MESSAGEP (m))
    return Qnil;

  if (EQ (attribute, Qtt_address))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_message_address_set (m, (Tt_address) tooltalk_constant_value (value));
    }
  else if (EQ (attribute, Qtt_class))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_message_class_set (m, (Tt_class) tooltalk_constant_value (value));
    }
  else if (EQ (attribute, Qtt_disposition))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_message_disposition_set (m, ((Tt_disposition)
				      tooltalk_constant_value (value)));
    }
  else if (EQ (attribute, Qtt_scope))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_message_scope_set (m, (Tt_scope) tooltalk_constant_value (value));
    }
  else if (EQ (attribute, Qtt_file))
    fun_str = tt_message_file_set;
  else if (EQ (attribute, Qtt_handler_ptype))
    fun_str = tt_message_handler_ptype_set;
  else if (EQ (attribute, Qtt_handler))
    fun_str = tt_message_handler_set;
  else if (EQ (attribute, Qtt_object))
    fun_str = tt_message_object_set;
  else if (EQ (attribute, Qtt_op))
    fun_str = tt_message_op_set;
  else if (EQ (attribute, Qtt_otype))
    fun_str = tt_message_otype_set;
  else if (EQ (attribute, Qtt_sender_ptype))
    fun_str = tt_message_sender_ptype_set;
  else if (EQ (attribute, Qtt_session))
    fun_str = tt_message_session_set;
  else if (EQ (attribute, Qtt_status_string))
    fun_str = tt_message_status_string_set;
  else if (EQ (attribute, Qtt_arg_bval))
    {
      Extbyte *value_ext;
      Extcount value_ext_len;
      CHECK_STRING (value);
      TO_EXTERNAL_FORMAT (LISP_STRING, value,
			  ALLOCA, (value_ext, value_ext_len),
			  Qnative);
      tt_message_arg_bval_set (m, n, (unsigned char *) value_ext, value_ext_len);
    }
  else if (EQ (attribute, Qtt_arg_ival))
    {
      CHECK_INT (value);
      tt_message_arg_ival_set (m, n, XINT (value));
    }
  else if (EQ (attribute, Qtt_arg_val))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_message_arg_val_set (m, n, value_ext);
    }
  else if (EQ (attribute, Qtt_status))
    {
      CHECK_INT (value);
      tt_message_status_set (m, XINT (value));
    }
  else if (EQ (attribute, Qtt_callback))
    {
      CHECK_SYMBOL (value);
      XTOOLTALK_MESSAGE (message_)->callback = value;
    }
  else if (EQ (attribute, Qtt_prop))
    {
      return Fput (XTOOLTALK_MESSAGE (message_)->plist_sym, argn, value);
    }
  else
    signal_simple_error ("Invalid value for `set-tooltalk-message-attribute'",
			 attribute);

  if (fun_str)
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      (*fun_str) (m, value_ext);
    }

  return Qnil;
}

DEFUN ("return-tooltalk-message", Freturn_tooltalk_message, 1, 2, 0, /*
Send a reply to this message.  The second argument can be
'reply, 'reject or 'fail; the default is 'reply.  Before sending
a reply all message arguments whose mode is TT_INOUT or TT_OUT should
have been filled in - see set-tooltalk-message-attribute.
*/
       (message_, mode))
{
  Tt_message m = unbox_tooltalk_message (message_);

  if (NILP (mode))
    mode = Qtt_reply;
  else
    CHECK_SYMBOL (mode);

  if (!VALID_TOOLTALK_MESSAGEP (m))
    return Qnil;
  else if (EQ (mode, Qtt_reply))
    tt_message_reply (m);
  else if (EQ (mode, Qtt_reject))
    tt_message_reject (m);
  else if (EQ (mode, Qtt_fail))
    tt_message_fail (m);

  return Qnil;
}

DEFUN ("create-tooltalk-message", Fcreate_tooltalk_message, 0, 1, 0, /*
Create a new tooltalk message.
The messages session attribute is initialized to the default session.
Other attributes can be initialized with `set-tooltalk-message-attribute'.
`make-tooltalk-message' is the preferred to create and initialize a message.

Optional arg NO-CALLBACK says don't add a C-level callback at all.
Normally don't do that; just don't specify the Lisp callback when
calling `make-tooltalk-message'.
*/
       (no_callback))
{
  Tt_message m = tt_message_create ();
  Lisp_Object message_ = make_tooltalk_message (m);
  if (NILP (no_callback))
    {
      tt_message_callback_add (m, tooltalk_message_callback);
    }
  tt_message_session_set (m, tt_default_session ());
  tt_message_user_set (m, TOOLTALK_MESSAGE_KEY, LISP_TO_VOID (message_));
  return message_;
}

DEFUN ("destroy-tooltalk-message", Fdestroy_tooltalk_message, 1, 1, 0, /*
Apply tt_message_destroy() to the message.
It's not necessary to destroy messages after they've been processed by
a message or pattern callback; the Lisp/Tooltalk callback machinery does
this for you.
*/
       (message_))
{
  Tt_message m = unbox_tooltalk_message (message_);

  if (VALID_TOOLTALK_MESSAGEP (m))
    /* #### Should we call Fremhash() here?  It seems that
       a common paradigm is

       (send-tooltalk-message)
       (destroy-tooltalk-message)

       which would imply that destroying a sent ToolTalk message
       doesn't actually destroy it; when a response is sent back,
       the callback for the message will still be called.

       But then maybe not: Maybe it really does destroy it,
       and the reason for that paradigm is that the author
       of `send-tooltalk-message' didn't really know what he
       was talking about when he said that it's a good idea
       to call `destroy-tooltalk-message' after sending it. */
    tt_message_destroy (m);

  return Qnil;
}


DEFUN ("add-tooltalk-message-arg", Fadd_tooltalk_message_arg, 3, 4, 0, /*
Append one new argument to the message.
MODE must be one of TT_IN, TT_INOUT, or TT_OUT; VTYPE must be a string;
and VALUE can be a string or an integer.   Tooltalk doesn't
define any semantics for VTYPE, so only the participants in the
protocol you're using need to agree what types mean (if anything).
Conventionally "string" is used for strings and "int" for 32 bit integers.
Arguments can initialized by providing a value or with
`set-tooltalk-message-attribute'.  The latter is necessary if you
want to initialize the argument with a string that can contain
embedded nulls (use 'arg_bval).
*/
       (message_, mode, vtype, value))
{
  Tt_message m = unbox_tooltalk_message (message_);
  Tt_mode n;

  CHECK_STRING (vtype);
  CHECK_TOOLTALK_CONSTANT (mode);

  n = (Tt_mode) tooltalk_constant_value (mode);

  if (!VALID_TOOLTALK_MESSAGEP (m))
    return Qnil;
  {
    const char *vtype_ext;

    LISP_STRING_TO_EXTERNAL (vtype, vtype_ext, Qnative);
    if (NILP (value))
      tt_message_arg_add (m, n, vtype_ext, NULL);
    else if (STRINGP (value))
      {
	const char *value_ext;
	LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
	tt_message_arg_add (m, n, vtype_ext, value_ext);
      }
    else if (INTP (value))
      tt_message_iarg_add (m, n, vtype_ext, XINT (value));
  }

  return Qnil;
}

DEFUN ("send-tooltalk-message", Fsend_tooltalk_message, 1, 1, 0, /*
Send the message on its way.
Once the message has been sent it's almost always a good idea to get rid of
it with `destroy-tooltalk-message'.
*/
       (message_))
{
  Tt_message m = unbox_tooltalk_message (message_);

  if (VALID_TOOLTALK_MESSAGEP (m))
    {
      tt_message_send (m);
      Fputhash (message_, Qnil, Vtooltalk_message_gcpro);
    }

  return Qnil;
}

DEFUN ("create-tooltalk-pattern", Fcreate_tooltalk_pattern, 0, 0, 0, /*
Create a new Tooltalk pattern.
Its session attribute is initialized to be the default session.
*/
       ())
{
  Tt_pattern p = tt_pattern_create ();
  Lisp_Object pattern = make_tooltalk_pattern (p);

  tt_pattern_callback_add (p, tooltalk_pattern_callback);
  tt_pattern_session_add (p, tt_default_session ());
  tt_pattern_user_set (p, TOOLTALK_PATTERN_KEY, LISP_TO_VOID (pattern));

  return pattern;
}


DEFUN ("destroy-tooltalk-pattern", Fdestroy_tooltalk_pattern, 1, 1, 0, /*
Apply tt_pattern_destroy() to the pattern.
This effectively unregisters the pattern.
*/
       (pattern))
{
  Tt_pattern p = unbox_tooltalk_pattern (pattern);

  if (VALID_TOOLTALK_PATTERNP (p))
    {
      tt_pattern_destroy (p);
      Fremhash (pattern, Vtooltalk_pattern_gcpro);
    }

  return Qnil;
}


DEFUN ("add-tooltalk-pattern-attribute", Fadd_tooltalk_pattern_attribute, 3, 3, 0, /*
Add one value to the indicated pattern attribute.
All Tooltalk pattern attributes are supported except 'user.  The names
of attributes are the same as the Tooltalk accessors used to set them
less the "tooltalk_pattern_" prefix and the "_add" ...
*/
       (value, pattern, attribute))
{
  Tt_pattern p = unbox_tooltalk_pattern (pattern);

  CHECK_SYMBOL (attribute);

  if (!VALID_TOOLTALK_PATTERNP (p))
    return Qnil;

  else if (EQ (attribute, Qtt_category))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_pattern_category_set (p, ((Tt_category)
				   tooltalk_constant_value (value)));
    }
  else if (EQ (attribute, Qtt_address))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_pattern_address_add (p, ((Tt_address)
				  tooltalk_constant_value (value)));
    }
  else if (EQ (attribute, Qtt_class))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_pattern_class_add (p, (Tt_class) tooltalk_constant_value (value));
    }
  else if (EQ (attribute, Qtt_disposition))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_pattern_disposition_add (p, ((Tt_disposition)
				      tooltalk_constant_value (value)));
    }
  else if (EQ (attribute, Qtt_file))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_file_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_object))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_object_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_op))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_op_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_otype))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_otype_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_scope))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_pattern_scope_add (p, (Tt_scope) tooltalk_constant_value (value));
    }
  else if (EQ (attribute, Qtt_sender))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_sender_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_sender_ptype))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_sender_ptype_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_session))
    {
      const char *value_ext;
      CHECK_STRING (value);
      LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
      tt_pattern_session_add (p, value_ext);
    }
  else if (EQ (attribute, Qtt_state))
    {
      CHECK_TOOLTALK_CONSTANT (value);
      tt_pattern_state_add (p, (Tt_state) tooltalk_constant_value (value));
    }
  else if (EQ (attribute, Qtt_callback))
    {
      CHECK_SYMBOL (value);
      XTOOLTALK_PATTERN (pattern)->callback = value;
    }

  return Qnil;
}


DEFUN ("add-tooltalk-pattern-arg", Fadd_tooltalk_pattern_arg, 3, 4, 0, /*
Add one fully specified argument to a tooltalk pattern.
Mode must be one of TT_IN, TT_INOUT, or TT_OUT, type must be a string.
Value can be an integer, string or nil.  If value is an integer then
an integer argument (tt_pattern_iarg_add) added otherwise a string argument
is added.  At present there's no way to add a binary data argument.
*/
     (pattern, mode, vtype, value))
{
  Tt_pattern p = unbox_tooltalk_pattern (pattern);
  Tt_mode n;

  CHECK_STRING (vtype);
  CHECK_TOOLTALK_CONSTANT (mode);

  n = (Tt_mode) tooltalk_constant_value (mode);

  if (!VALID_TOOLTALK_PATTERNP (p))
    return Qnil;

  {
    const char *vtype_ext;

    LISP_STRING_TO_EXTERNAL (vtype, vtype_ext, Qnative);
    if (NILP (value))
      tt_pattern_arg_add (p, n, vtype_ext, NULL);
    else if (STRINGP (value))
      {
	const char *value_ext;
	LISP_STRING_TO_EXTERNAL (value, value_ext, Qnative);
	tt_pattern_arg_add (p, n, vtype_ext, value_ext);
      }
    else if (INTP (value))
      tt_pattern_iarg_add (p, n, vtype_ext, XINT (value));
  }

  return Qnil;
}


DEFUN ("register-tooltalk-pattern", Fregister_tooltalk_pattern, 1, 1, 0, /*
Emacs will begin receiving messages that match this pattern.
*/
       (pattern))
{
  Tt_pattern p = unbox_tooltalk_pattern (pattern);

  if (VALID_TOOLTALK_PATTERNP (p) && tt_pattern_register (p) == TT_OK)
    {
      Fputhash (pattern, Qnil, Vtooltalk_pattern_gcpro);
      return Qt;
    }
  else
    return Qnil;
}


DEFUN ("unregister-tooltalk-pattern", Funregister_tooltalk_pattern, 1, 1, 0, /*
Emacs will stop receiving messages that match this pattern.
*/
       (pattern))
{
  Tt_pattern p = unbox_tooltalk_pattern (pattern);

  if (VALID_TOOLTALK_PATTERNP (p))
    {
      tt_pattern_unregister (p);
      Fremhash (pattern, Vtooltalk_pattern_gcpro);
    }

  return Qnil;
}


DEFUN ("tooltalk-pattern-prop-get", Ftooltalk_pattern_prop_get, 2, 2, 0, /*
Return the value of PROPERTY in tooltalk pattern PATTERN.
This is the last value set with `tooltalk-pattern-prop-set'.
*/
       (pattern, property))
{
  CHECK_TOOLTALK_PATTERN (pattern);
  return Fget (XTOOLTALK_PATTERN (pattern)->plist_sym, property, Qnil);
}


DEFUN ("tooltalk-pattern-prop-set", Ftooltalk_pattern_prop_set, 3, 3, 0, /*
Set the value of PROPERTY to VALUE in tooltalk pattern PATTERN.
It can be retrieved with `tooltalk-pattern-prop-get'.
*/
       (pattern, property, value))
{
  CHECK_TOOLTALK_PATTERN (pattern);
  return Fput (XTOOLTALK_PATTERN (pattern)->plist_sym, property, value);
}


DEFUN ("tooltalk-pattern-plist-get", Ftooltalk_pattern_plist_get, 1, 1, 0, /*
Return the a list of all the properties currently set in PATTERN.
*/
       (pattern))
{
  CHECK_TOOLTALK_PATTERN (pattern);
  return
    Fcopy_sequence (Fsymbol_plist (XTOOLTALK_PATTERN (pattern)->plist_sym));
}

DEFUN ("tooltalk-default-procid", Ftooltalk_default_procid, 0, 0, 0, /*
Return current default process identifier for your process.
*/
       ())
{
  char *procid = tt_default_procid ();
  return procid ? build_string (procid) : Qnil;
}

DEFUN ("tooltalk-default-session", Ftooltalk_default_session, 0, 0, 0, /*
Return current default session identifier for the current default procid.
*/
       ())
{
  char *session = tt_default_session ();
  return session ? build_string (session) : Qnil;
}

static void
init_tooltalk (void)
{
  /* This function can GC */
  char *retval;
  Lisp_Object lp;
  Lisp_Object fil;


  /* tt_open() messes with our signal handler flags (at least when no
     ttsessions is running on the machine), therefore we save the
     actions and restore them after the call */
#ifdef HAVE_SIGPROCMASK
  {
    struct sigaction ActSIGQUIT;
    struct sigaction ActSIGINT;
    struct sigaction ActSIGCHLD;
    sigaction (SIGQUIT, NULL, &ActSIGQUIT);
    sigaction (SIGINT, NULL, &ActSIGINT);
    sigaction (SIGCHLD, NULL, &ActSIGCHLD);
#endif
  retval = tt_open ();
#ifdef HAVE_SIGPROCMASK
    sigaction (SIGQUIT, &ActSIGQUIT, NULL);
    sigaction (SIGINT, &ActSIGINT, NULL);
    sigaction (SIGCHLD, &ActSIGCHLD, NULL);
  }
#endif


  if (tt_ptr_error (retval) != TT_OK)
    return;

  Vtooltalk_fd = make_int (tt_fd ());

  tt_session_join (tt_default_session ());

  lp = connect_to_file_descriptor (build_string ("tooltalk"), Qnil,
				   Vtooltalk_fd, Vtooltalk_fd);
  if (!NILP (lp))
    {
      /* Don't ask the user for confirmation when exiting Emacs */
      Fprocess_kill_without_query (lp, Qnil);
      XSETSUBR (fil, &SFreceive_tooltalk_message);
      set_process_filter (lp, fil, 1);
    }
  else
    {
      tt_close ();
      Vtooltalk_fd = Qnil;
      return;
    }

#if defined (SOLARIS2)
  /* Apparently the tt_message_send_on_exit() function does not exist
     under SunOS 4.x or IRIX 5 or various other non-Solaris-2 systems.
     No big deal if we don't do the following under those systems. */
  {
    Tt_message exit_msg = tt_message_create ();

    tt_message_op_set (exit_msg, "emacs-aborted");
    tt_message_scope_set (exit_msg, TT_SESSION);
    tt_message_class_set (exit_msg, TT_NOTICE);
    tt_message_send_on_exit (exit_msg);
    tt_message_destroy (exit_msg);
  }
#endif
}

DEFUN ("tooltalk-open-connection", Ftooltalk_open_connection, 0, 0, 0, /*
Opens a connection to the ToolTalk server.
Returns t if successful, nil otherwise.
*/
       ())
{
  if (!NILP (Vtooltalk_fd))
    error ("Already connected to ToolTalk.");
  if (noninteractive)
    error ("Can't connect to ToolTalk in batch mode.");
  init_tooltalk ();
  return NILP (Vtooltalk_fd) ? Qnil : Qt;
}


void
syms_of_tooltalk (void)
{
  INIT_LRECORD_IMPLEMENTATION (tooltalk_message);
  INIT_LRECORD_IMPLEMENTATION (tooltalk_pattern);

  defsymbol (&Qtooltalk_messagep, "tooltalk-message-p");
  DEFSUBR (Ftooltalk_message_p);
  defsymbol (&Qtooltalk_patternp, "tooltalk-pattern-p");
  DEFSUBR (Ftooltalk_pattern_p);
  defsymbol (&Qtooltalk_message_handler_hook, "tooltalk-message-handler-hook");
  defsymbol (&Qtooltalk_pattern_handler_hook, "tooltalk-pattern-handler-hook");
  defsymbol (&Qtooltalk_unprocessed_message_hook,
	     "tooltalk-unprocessed-message-hook");

  DEFSUBR (Freceive_tooltalk_message);
  DEFSUBR (Fcreate_tooltalk_message);
  DEFSUBR (Fdestroy_tooltalk_message);
  DEFSUBR (Fadd_tooltalk_message_arg);
  DEFSUBR (Fget_tooltalk_message_attribute);
  DEFSUBR (Fset_tooltalk_message_attribute);
  DEFSUBR (Fsend_tooltalk_message);
  DEFSUBR (Freturn_tooltalk_message);
  DEFSUBR (Fcreate_tooltalk_pattern);
  DEFSUBR (Fdestroy_tooltalk_pattern);
  DEFSUBR (Fadd_tooltalk_pattern_attribute);
  DEFSUBR (Fadd_tooltalk_pattern_arg);
  DEFSUBR (Fregister_tooltalk_pattern);
  DEFSUBR (Funregister_tooltalk_pattern);
  DEFSUBR (Ftooltalk_pattern_plist_get);
  DEFSUBR (Ftooltalk_pattern_prop_set);
  DEFSUBR (Ftooltalk_pattern_prop_get);
  DEFSUBR (Ftooltalk_default_procid);
  DEFSUBR (Ftooltalk_default_session);
  DEFSUBR (Ftooltalk_open_connection);

  defsymbol (&Qreceive_tooltalk_message, "receive-tooltalk-message");
  defsymbol (&Qtt_address, "address");
  defsymbol (&Qtt_args_count, "args_count");
  defsymbol (&Qtt_arg_bval, "arg_bval");
  defsymbol (&Qtt_arg_ival, "arg_ival");
  defsymbol (&Qtt_arg_mode, "arg_mode");
  defsymbol (&Qtt_arg_type, "arg_type");
  defsymbol (&Qtt_arg_val, "arg_val");
  defsymbol (&Qtt_class, "class");
  defsymbol (&Qtt_category, "category");
  defsymbol (&Qtt_disposition, "disposition");
  defsymbol (&Qtt_file, "file");
  defsymbol (&Qtt_gid, "gid");
  defsymbol (&Qtt_handler, "handler");
  defsymbol (&Qtt_handler_ptype, "handler_ptype");
  defsymbol (&Qtt_object, "object");
  defsymbol (&Qtt_op, "op");
  defsymbol (&Qtt_opnum, "opnum");
  defsymbol (&Qtt_otype, "otype");
  defsymbol (&Qtt_scope, "scope");
  defsymbol (&Qtt_sender, "sender");
  defsymbol (&Qtt_sender_ptype, "sender_ptype");
  defsymbol (&Qtt_session, "session");
  defsymbol (&Qtt_state, "state");
  defsymbol (&Qtt_status, "status");
  defsymbol (&Qtt_status_string, "status_string");
  defsymbol (&Qtt_uid, "uid");
  defsymbol (&Qtt_callback, "callback");
  defsymbol (&Qtt_prop, "prop");
  defsymbol (&Qtt_plist, "plist");
  defsymbol (&Qtt_reject, "reject");
  defsymbol (&Qtt_reply, "reply");
  defsymbol (&Qtt_fail, "fail");

  DEFERROR (Qtooltalk_error, "ToolTalk error", Qio_error);
}

void
vars_of_tooltalk (void)
{
  Fprovide (intern ("tooltalk"));

  DEFVAR_LISP ("tooltalk-fd", &Vtooltalk_fd /*
File descriptor returned by tt_initialize; nil if not connected to ToolTalk.
*/ );
  Vtooltalk_fd = Qnil;

  DEFVAR_LISP ("tooltalk-message-handler-hook",
	      &Vtooltalk_message_handler_hook /*
List of functions to be applied to each ToolTalk message reply received.
This will always occur as a result of our sending a request message.
Functions will be called with two arguments, the message and the
corresponding pattern.  This hook will not be called if the request
message was created without a C-level callback function (see
`tooltalk-unprocessed-message-hook').
*/ );
  Vtooltalk_message_handler_hook = Qnil;

  DEFVAR_LISP ("tooltalk-pattern-handler-hook",
	      &Vtooltalk_pattern_handler_hook /*
List of functions to be applied to each pattern-matching ToolTalk message.
This is all messages except those handled by `tooltalk-message-handler-hook'.
Functions will be called with two arguments, the message and the
corresponding pattern.
*/ );
  Vtooltalk_pattern_handler_hook = Qnil;

  DEFVAR_LISP ("tooltalk-unprocessed-message-hook",
	      &Vtooltalk_unprocessed_message_hook /*
List of functions to be applied to each unprocessed ToolTalk message.
Unprocessed messages are messages that didn't match any patterns.
*/ );
  Vtooltalk_unprocessed_message_hook = Qnil;

  Tooltalk_Message_plist_str = build_string ("Tooltalk Message plist");
  Tooltalk_Pattern_plist_str = build_string ("Tooltalk Pattern p plist");

  staticpro(&Tooltalk_Message_plist_str);
  staticpro(&Tooltalk_Pattern_plist_str);

#define MAKE_CONSTANT(name) do { \
    defsymbol (&Q_ ## name, #name); \
    Fset (Q_ ## name, make_int (name)); \
  } while (0)

  MAKE_CONSTANT (TT_MODE_UNDEFINED);
  MAKE_CONSTANT (TT_IN);
  MAKE_CONSTANT (TT_OUT);
  MAKE_CONSTANT (TT_INOUT);
  MAKE_CONSTANT (TT_MODE_LAST);

  MAKE_CONSTANT (TT_SCOPE_NONE);
  MAKE_CONSTANT (TT_SESSION);
  MAKE_CONSTANT (TT_FILE);
  MAKE_CONSTANT (TT_BOTH);
  MAKE_CONSTANT (TT_FILE_IN_SESSION);

  MAKE_CONSTANT (TT_CLASS_UNDEFINED);
  MAKE_CONSTANT (TT_NOTICE);
  MAKE_CONSTANT (TT_REQUEST);
  MAKE_CONSTANT (TT_CLASS_LAST);

  MAKE_CONSTANT (TT_CATEGORY_UNDEFINED);
  MAKE_CONSTANT (TT_OBSERVE);
  MAKE_CONSTANT (TT_HANDLE);
  MAKE_CONSTANT (TT_CATEGORY_LAST);

  MAKE_CONSTANT (TT_PROCEDURE);
  MAKE_CONSTANT (TT_OBJECT);
  MAKE_CONSTANT (TT_HANDLER);
  MAKE_CONSTANT (TT_OTYPE);
  MAKE_CONSTANT (TT_ADDRESS_LAST);

  MAKE_CONSTANT (TT_CREATED);
  MAKE_CONSTANT (TT_SENT);
  MAKE_CONSTANT (TT_HANDLED);
  MAKE_CONSTANT (TT_FAILED);
  MAKE_CONSTANT (TT_QUEUED);
  MAKE_CONSTANT (TT_STARTED);
  MAKE_CONSTANT (TT_REJECTED);
  MAKE_CONSTANT (TT_STATE_LAST);

  MAKE_CONSTANT (TT_DISCARD);
  MAKE_CONSTANT (TT_QUEUE);
  MAKE_CONSTANT (TT_START);

#undef MAKE_CONSTANT

  staticpro (&Vtooltalk_message_gcpro);
  staticpro (&Vtooltalk_pattern_gcpro);
  Vtooltalk_message_gcpro =
    make_lisp_hash_table (10, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
  Vtooltalk_pattern_gcpro =
    make_lisp_hash_table (10, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
}
