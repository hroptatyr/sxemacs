/* GTK selection processing for XEmacs
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* Synched up with: Not synched with FSF. */

/* Authorship:

   Written by Kevin Gallo for FSF Emacs.
   Rewritten for mswindows by Jonathan Harris, December 1997 for 21.0.
   Rewritten for GTK by William Perry, April 2000 for 21.1
 */


#include <config.h>
#include "lisp.h"
#include "events.h"
#include "buffer.h"
#include "device.h"
#include "console-gtk.h"
#include "select.h"
#include "opaque.h"
#include "frame.h"

int lisp_to_time (Lisp_Object, time_t *);
static Lisp_Object Vretrieved_selection;
static gboolean waiting_for_selection;
Lisp_Object Vgtk_sent_selection_hooks;

static Lisp_Object atom_to_symbol (struct device *d, GdkAtom atom);
static GdkAtom symbol_to_gtk_atom (struct device *d, Lisp_Object sym, int only_if_exists);

static void lisp_data_to_selection_data (struct device *,
					 Lisp_Object obj,
					 unsigned char **data_ret,
					 GdkAtom *type_ret,
					 unsigned int *size_ret,
					 int *format_ret);
static Lisp_Object selection_data_to_lisp_data (struct device *,
						Extbyte *data,
						size_t size,
						GdkAtom type,
						int format);

/* Set the selection data to GDK_NONE and NULL data, meaning we were
** unable to do what they wanted.
*/
static void
gtk_decline_selection_request (GtkSelectionData *data)
{
  gtk_selection_data_set (data, GDK_NONE, 0, NULL, 0);
}

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.
 */
struct _selection_closure
{
  GtkSelectionData *data;
  gboolean successful;
};

static Lisp_Object
gtk_selection_request_lisp_error (Lisp_Object closure)
{
  struct _selection_closure *cl = (struct _selection_closure *)
    get_opaque_ptr (closure);

  free_opaque_ptr (closure);
  if (cl->successful == TRUE)
    return Qnil;
  gtk_decline_selection_request (cl->data);
  return Qnil;
}

/* This provides the current selection to a requester.
**
** This is connected to the selection_get() signal of the application
** shell in device-gtk.c:gtk_init_device().
**
** This is radically different than the old selection code (21.1.x),
** but has been modeled after the X code, and appears to work.
**
** WMP Feb 12 2001
*/
void
emacs_gtk_selection_handle (GtkWidget *widget,
			    GtkSelectionData *selection_data,
			    guint info,
			    guint time_stamp,
			    gpointer data)
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;
  Lisp_Object temp_obj;
  Lisp_Object selection_symbol;
  Lisp_Object target_symbol = Qnil;
  Lisp_Object converted_selection = Qnil;
  guint32 local_selection_time;
  Lisp_Object successful_p = Qnil;
  int count;
  struct device *d = decode_gtk_device (Qnil);
  struct _selection_closure *cl = NULL;

  GCPRO2 (converted_selection, target_symbol);

  selection_symbol = atom_to_symbol (d, selection_data->selection);
  target_symbol = atom_to_symbol (d, selection_data->target);

#if 0 /* #### MULTIPLE doesn't work yet */
  if (EQ (target_symbol, QMULTIPLE))
    target_symbol = fetch_multiple_target (selection_data);
#endif

  temp_obj = Fget_selection_timestamp (selection_symbol);

  if (NILP (temp_obj))
    {
      /* We don't appear to have the selection. */
      gtk_decline_selection_request (selection_data);

      goto DONE_LABEL;
    }

  local_selection_time = * (guint32 *) XOPAQUE_DATA (temp_obj);

  if (time_stamp != GDK_CURRENT_TIME &&
      local_selection_time > time_stamp)
    {
      /* Someone asked for the selection, and we have one, but not the one
	 they're looking for. */
      gtk_decline_selection_request (selection_data);
      goto DONE_LABEL;
    }

  converted_selection = select_convert_out (selection_symbol,
					    target_symbol, Qnil);

  /* #### Is this the right thing to do? I'm no X expert. -- ajh */
  if (NILP (converted_selection))
    {
      /* We don't appear to have a selection in that data type. */
      gtk_decline_selection_request (selection_data);
      goto DONE_LABEL;
    }

  count = specpdl_depth ();

  cl = (struct _selection_closure *) xmalloc (sizeof (*cl));
  cl->data = selection_data;
  cl->successful = FALSE;

  record_unwind_protect (gtk_selection_request_lisp_error,
			 make_opaque_ptr (cl));

  {
    unsigned char *data;
    unsigned int size;
    int format;
    GdkAtom type;
    lisp_data_to_selection_data (d, converted_selection,
				 &data, &type, &size, &format);

    gtk_selection_data_set (selection_data, type, format, data, size);
    successful_p = Qt;
    /* Tell x_selection_request_lisp_error() it's cool. */
    cl->successful = TRUE;
    xfree (data);
  }

  unbind_to (count, Qnil);

 DONE_LABEL:

  if (cl) xfree (cl);

  UNGCPRO;

  /* Let random lisp code notice that the selection has been asked for. */
  {
    Lisp_Object val = Vgtk_sent_selection_hooks;
    if (!UNBOUNDP (val) && !NILP (val))
      {
	Lisp_Object rest;
	if (CONSP (val) && !EQ (XCAR (val), Qlambda))
	  for (rest = val; !NILP (rest); rest = Fcdr (rest))
	    call3 (Fcar (rest), selection_symbol, target_symbol, successful_p);
	else
	  call3 (val, selection_symbol, target_symbol, successful_p);
      }
  }
}


void
emacs_gtk_selection_clear_event_handle (GtkWidget *widget,
                                        GdkEventSelection *event,
                                        gpointer data)
{
  GdkAtom selection = event->selection;
  guint32 changed_owner_time = event->time;
  struct device *d = decode_gtk_device (Qnil);

  Lisp_Object selection_symbol, local_selection_time_lisp;
  guint32 local_selection_time;

  selection_symbol = atom_to_symbol (d, selection);

  local_selection_time_lisp = Fget_selection_timestamp (selection_symbol);

  /* We don't own the selection, so that's fine. */
  if (NILP (local_selection_time_lisp))
    return;

  local_selection_time = *(guint32 *) XOPAQUE_DATA (local_selection_time_lisp);

  /* This SelectionClear is for a selection that we no longer own, so we can
     disregard it.  (That is, we have reasserted the selection since this
     request was generated.)
   */
  if (changed_owner_time != GDK_CURRENT_TIME &&
      local_selection_time > changed_owner_time)
    return;

  handle_selection_clear (selection_symbol);
}



static GtkWidget *reading_selection_reply;
static GdkAtom reading_which_selection;
static int selection_reply_timed_out;

/* Gets the current selection owned by another application */
void
emacs_gtk_selection_received (GtkWidget *widget,
			      GtkSelectionData *selection_data,
			      gpointer user_data)
{
  waiting_for_selection = FALSE;
  Vretrieved_selection = Qnil;

  reading_selection_reply = NULL;

  signal_fake_event ();

  if (selection_data->length < 0)
    {
      return;
    }

  Vretrieved_selection =
    selection_data_to_lisp_data (NULL,
				 selection_data->data,
				 selection_data->length,
				 selection_data->type,
				 selection_data->format);
}

static int
selection_reply_done (void *ignore)
{
  return !reading_selection_reply;
}

/* Do protocol to read selection-data from the server.
   Converts this to lisp data and returns it.
 */
static Lisp_Object
gtk_get_foreign_selection (Lisp_Object selection_symbol,
			   Lisp_Object target_type)
{
  /* This function can GC */
  struct device *d = decode_gtk_device (Qnil);
  GtkWidget *requestor = DEVICE_GTK_APP_SHELL (d);
  guint32 requestor_time = DEVICE_GTK_MOUSE_TIMESTAMP (d);
  GdkAtom selection_atom = symbol_to_gtk_atom (d, selection_symbol, 0);
  int speccount;
  GdkAtom type_atom = symbol_to_gtk_atom (d, (CONSP (target_type) ?
					      XCAR (target_type) : target_type), 0);

  gtk_selection_convert (requestor, selection_atom, type_atom,
			 requestor_time);

  signal_fake_event ();

  /* Block until the reply has been read. */
  reading_selection_reply = requestor;
  reading_which_selection = selection_atom;
  selection_reply_timed_out = 0;

  speccount = specpdl_depth ();

#if 0
  /* add a timeout handler */
  if (gtk_selection_timeout > 0)
    {
      Lisp_Object id = Fadd_timeout (make_int (x_selection_timeout),
				     Qx_selection_reply_timeout_internal,
				     Qnil, Qnil);
      record_unwind_protect (Fdisable_timeout, id);
    }
#endif

  /* This is ^Gable */
  wait_delaying_user_input (selection_reply_done, 0);

  if (selection_reply_timed_out)
    error ("timed out waiting for reply from selection owner");

  unbind_to (speccount, Qnil);

  /* otherwise, the selection is waiting for us on the requested property. */
  return select_convert_in (selection_symbol,
			    target_type,
			    Vretrieved_selection);
}


#if 0
static void
gtk_get_window_property (struct device *d, GtkWidget *window, GdkAtom property,
			 Extbyte **data_ret, int *bytes_ret,
			 GdkAtom *actual_type_ret, int *actual_format_ret,
			 unsigned long *actual_size_ret, int delete_p)
{
  size_t total_size;
  unsigned long bytes_remaining;
  int offset = 0;
  unsigned char *tmp_data = 0;
  int result;
  int buffer_size = SELECTION_QUANTUM (display);
  if (buffer_size > MAX_SELECTION_QUANTUM) buffer_size = MAX_SELECTION_QUANTUM;

  /* First probe the thing to find out how big it is. */
  result = XGetWindowProperty (display, window, property,
			       0, 0, False, AnyPropertyType,
			       actual_type_ret, actual_format_ret,
			       actual_size_ret,
			       &bytes_remaining, &tmp_data);
  if (result != Success)
    {
      *data_ret = 0;
      *bytes_ret = 0;
      return;
    }
  XFree ((char *) tmp_data);

  if (*actual_type_ret == None || *actual_format_ret == 0)
    {
      if (delete_p) XDeleteProperty (display, window, property);
      *data_ret = 0;
      *bytes_ret = 0;
      return;
    }

  total_size = bytes_remaining + 1;
  *data_ret = (Extbyte *) xmalloc (total_size);

  /* Now read, until we've gotten it all. */
  while (bytes_remaining)
    {
#if 0
      int last = bytes_remaining;
#endif
      result =
	XGetWindowProperty (display, window, property,
			    offset/4, buffer_size/4,
			    (delete_p ? True : False),
			    AnyPropertyType,
			    actual_type_ret, actual_format_ret,
			    actual_size_ret, &bytes_remaining, &tmp_data);
#if 0
      stderr_out ("<< read %d\n", last-bytes_remaining);
#endif
      /* If this doesn't return Success at this point, it means that
	 some clod deleted the selection while we were in the midst of
	 reading it.  Deal with that, I guess....
       */
      if (result != Success) break;
      *actual_size_ret *= *actual_format_ret / 8;
      memcpy ((*data_ret) + offset, tmp_data, *actual_size_ret);
      offset += *actual_size_ret;
      XFree ((char *) tmp_data);
    }
  *bytes_ret = offset;
}


static void
receive_incremental_selection (Display *display, Window window, Atom property,
			       /* this one is for error messages only */
			       Lisp_Object target_type,
			       unsigned int min_size_bytes,
			       Extbyte **data_ret, int *size_bytes_ret,
			       Atom *type_ret, int *format_ret,
			       unsigned long *size_ret)
{
  /* This function can GC */
  int offset = 0;
  int prop_id;
  *size_bytes_ret = min_size_bytes;
  *data_ret = (Extbyte *) xmalloc (*size_bytes_ret);
#if 0
  stderr_out ("\nread INCR %d\n", min_size_bytes);
#endif
  /* At this point, we have read an INCR property, and deleted it (which
     is how we ack its receipt: the sending window will be selecting
     PropertyNotify events on our window to notice this).

     Now, we must loop, waiting for the sending window to put a value on
     that property, then reading the property, then deleting it to ack.
     We are done when the sender places a property of length 0.
   */
  prop_id = expect_property_change (display, window, property,
				    PropertyNewValue);
  while (1)
    {
      Extbyte *tmp_data;
      int tmp_size_bytes;
      wait_for_property_change (prop_id);
      /* expect it again immediately, because x_get_window_property may
	 .. no it won't, I don't get it.
	 .. Ok, I get it now, the Xt code that implements INCR is broken.
       */
      prop_id = expect_property_change (display, window, property,
					PropertyNewValue);
      x_get_window_property (display, window, property,
			     &tmp_data, &tmp_size_bytes,
			     type_ret, format_ret, size_ret, 1);

      if (tmp_size_bytes == 0) /* we're done */
	{
#if 0
	  stderr_out ("  read INCR done\n");
#endif
	  unexpect_property_change (prop_id);
	  if (tmp_data) xfree (tmp_data);
	  break;
	}
#if 0
      stderr_out ("  read INCR %d\n", tmp_size_bytes);
#endif
      if (*size_bytes_ret < offset + tmp_size_bytes)
	{
#if 0
	  stderr_out ("  read INCR realloc %d -> %d\n",
		   *size_bytes_ret, offset + tmp_size_bytes);
#endif
	  *size_bytes_ret = offset + tmp_size_bytes;
	  *data_ret = (Extbyte *) xrealloc (*data_ret, *size_bytes_ret);
	}
      memcpy ((*data_ret) + offset, tmp_data, tmp_size_bytes);
      offset += tmp_size_bytes;
      xfree (tmp_data);
    }
}


static Lisp_Object
gtk_get_window_property_as_lisp_data (struct device *d,
				      GtkWidget *window,
				      GdkAtom property,
				      /* next two for error messages only */
				      Lisp_Object target_type,
				      GdkAtom selection_atom)
{
  /* This function can GC */
  Atom actual_type;
  int actual_format;
  unsigned long actual_size;
  Extbyte *data = NULL;
  int bytes = 0;
  Lisp_Object val;
  struct device *d = get_device_from_display (display);

  x_get_window_property (display, window, property, &data, &bytes,
			 &actual_type, &actual_format, &actual_size, 1);
  if (! data)
    {
      if (XGetSelectionOwner (display, selection_atom))
	/* there is a selection owner */
	signal_error
	  (Qselection_conversion_error,
	   Fcons (build_string ("selection owner couldn't convert"),
		  Fcons (x_atom_to_symbol (d, selection_atom),
			 actual_type ?
			 list2 (target_type, x_atom_to_symbol (d, actual_type)) :
			 list1 (target_type))));
      else
	signal_error (Qerror,
		      list2 (build_string ("no selection"),
			     x_atom_to_symbol (d, selection_atom)));
    }

  if (actual_type == DEVICE_XATOM_INCR (d))
    {
      /* Ok, that data wasn't *the* data, it was just the beginning. */

      unsigned int min_size_bytes = * ((unsigned int *) data);
      xfree (data);
      receive_incremental_selection (display, window, property, target_type,
				     min_size_bytes, &data, &bytes,
				     &actual_type, &actual_format,
				     &actual_size);
    }

  /* It's been read.  Now convert it to a lisp object in some semi-rational
     manner. */
  val = selection_data_to_lisp_data (d, data, bytes,
				     actual_type, actual_format);

  xfree (data);
  return val;
}
#endif


static GdkAtom
symbol_to_gtk_atom (struct device *d, Lisp_Object sym, int only_if_exists)
{
  if (NILP (sym))		return GDK_SELECTION_PRIMARY;
  if (EQ (sym, Qt))		return GDK_SELECTION_SECONDARY;
  if (EQ (sym, QPRIMARY))	return GDK_SELECTION_PRIMARY;
  if (EQ (sym, QSECONDARY))	return GDK_SELECTION_SECONDARY;

  {
    const char *nameext;
    LISP_STRING_TO_EXTERNAL (Fsymbol_name (sym), nameext, Qctext);
    return gdk_atom_intern (nameext, only_if_exists ? TRUE : FALSE);
  }
}

static Lisp_Object
atom_to_symbol (struct device *d, GdkAtom atom)
{
  if (atom == GDK_SELECTION_PRIMARY) return (QPRIMARY);
  if (atom == GDK_SELECTION_SECONDARY) return (QSECONDARY);

  {
    char *intstr;
    char *str = gdk_atom_name (atom);

    if (! str) return Qnil;

    TO_INTERNAL_FORMAT (C_STRING, str,
			C_STRING_ALLOCA, intstr,
			Qctext);
    g_free (str);
    return intern (intstr);
  }
}

/* #### These are going to move into Lisp code(!) with the aid of
        some new functions I'm working on - ajh */

/* These functions convert from the selection data read from the server into
   something that we can use from elisp, and vice versa.

	Type:	Format:	Size:		Elisp Type:
	-----	-------	-----		-----------
	*	8	*		String
	ATOM	32	1		Symbol
	ATOM	32	> 1		Vector of Symbols
	*	16	1		Integer
	*	16	> 1		Vector of Integers
	*	32	1		if <=16 bits: Integer
					if > 16 bits: Cons of top16, bot16
	*	32	> 1		Vector of the above

   When converting a Lisp number to C, it is assumed to be of format 16 if
   it is an integer, and of format 32 if it is a cons of two integers.

   When converting a vector of numbers from Elisp to C, it is assumed to be
   of format 16 if every element in the vector is an integer, and is assumed
   to be of format 32 if any element is a cons of two integers.

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above.

   NOTE: Under Mule, when someone shoves us a string without a type, we
   set the type to 'COMPOUND_TEXT and automatically convert to Compound
   Text.  If the string has a type, we assume that the user wants the
   data sent as-is so we just do "binary" conversion.
 */


static Lisp_Object
selection_data_to_lisp_data (struct device *d,
			     Extbyte *data,
			     size_t size,
			     GdkAtom type,
			     int format)
{
  if (type == gdk_atom_intern ("NULL", 0))
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness. */
  else if (format == 8)
    return make_ext_string (data, size,
			    ((type == gdk_atom_intern ("TEXT", FALSE)) ||
			     (type == gdk_atom_intern ("COMPOUND_TEXT", FALSE)))
			    ? Qctext : Qbinary);

  /* Convert a single atom to a Lisp Symbol.
     Convert a set of atoms to a vector of symbols. */
  else if (type == gdk_atom_intern ("ATOM", FALSE))
    {
      if (size == sizeof (GdkAtom))
	return atom_to_symbol (d, *((GdkAtom *) data));
      else
	{
	  int i;
	  int len = size / sizeof (GdkAtom);
	  Lisp_Object v = Fmake_vector (make_int (len), Qzero);
	  for (i = 0; i < len; i++)
	    Faset (v, make_int (i), atom_to_symbol (d, ((GdkAtom *) data) [i]));
	  return v;
	}
    }

  /* Convert a single 16 or small 32 bit number to a Lisp Int.
     If the number is > 16 bits, convert it to a cons of integers,
     16 bits in each half.
   */
  else if (format == 32 && size == sizeof (long))
    return word_to_lisp (((unsigned long *) data) [0]);
  else if (format == 16 && size == sizeof (short))
    return make_int ((int) (((unsigned short *) data) [0]));

  /* Convert any other kind of data to a vector of numbers, represented
     as above (as an integer, or a cons of two 16 bit integers).

     #### Perhaps we should return the actual type to lisp as well.

	(x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	==> [4 4]

     and perhaps it should be

	(x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	==> (SPAN . [4 4])

     Right now the fact that the return type was SPAN is discarded before
     lisp code gets to see it.
   */
  else if (format == 16)
    {
      int i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < (int) size / 4; i++)
	{
	  int j = (int) ((unsigned short *) data) [i];
	  Faset (v, make_int (i), make_int (j));
	}
      return v;
    }
  else
    {
      int i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < (int) size / 4; i++)
	{
	  unsigned long j = ((unsigned long *) data) [i];
	  Faset (v, make_int (i), word_to_lisp (j));
	}
      return v;
    }
}


static void
lisp_data_to_selection_data (struct device *d,
			     Lisp_Object obj,
			     unsigned char **data_ret,
			     GdkAtom *type_ret,
			     unsigned int *size_ret,
			     int *format_ret)
{
  Lisp_Object type = Qnil;

  if (CONSP (obj) && SYMBOLP (XCAR (obj)))
    {
      type = XCAR (obj);
      obj = XCDR (obj);
      if (CONSP (obj) && NILP (XCDR (obj)))
	obj = XCAR (obj);
    }

  if (EQ (obj, QNULL) || (EQ (type, QNULL)))
    {				/* This is not the same as declining */
      *format_ret = 32;
      *size_ret = 0;
      *data_ret = 0;
      type = QNULL;
    }
  else if (STRINGP (obj))
    {
      const Extbyte *extval;
      Extcount extvallen;

      TO_EXTERNAL_FORMAT (LISP_STRING, obj,
			  ALLOCA, (extval, extvallen),
			  (NILP (type) ? Qctext : Qbinary));
      *format_ret = 8;
      *size_ret = extvallen;
      *data_ret = (unsigned char *) xmalloc (*size_ret);
      memcpy (*data_ret, extval, *size_ret);
#ifdef MULE
      if (NILP (type)) type = QCOMPOUND_TEXT;
#else
      if (NILP (type)) type = QSTRING;
#endif
    }
  else if (CHARP (obj))
    {
      Bufbyte buf[MAX_EMCHAR_LEN];
      Bytecount len;
      const Extbyte *extval;
      Extcount extvallen;

      *format_ret = 8;
      len = set_charptr_emchar (buf, XCHAR (obj));
      TO_EXTERNAL_FORMAT (DATA, (buf, len),
			  ALLOCA, (extval, extvallen),
			  Qctext);
      *size_ret = extvallen;
      *data_ret = (unsigned char *) xmalloc (*size_ret);
      memcpy (*data_ret, extval, *size_ret);
#ifdef MULE
      if (NILP (type)) type = QCOMPOUND_TEXT;
#else
      if (NILP (type)) type = QSTRING;
#endif
    }
  else if (SYMBOLP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (GdkAtom) + 1);
      (*data_ret) [sizeof (GdkAtom)] = 0;
      (*(GdkAtom **) data_ret) [0] = symbol_to_gtk_atom (d, obj, 0);
      if (NILP (type)) type = QATOM;
    }
  else if (INTP (obj) &&
	   XINT (obj) <= 0x7FFF &&
	   XINT (obj) >= -0x8000)
    {
      *format_ret = 16;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (short) + 1);
      (*data_ret) [sizeof (short)] = 0;
      (*(short **) data_ret) [0] = (short) XINT (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (INTP (obj) || CONSP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (unsigned char *) xmalloc (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(unsigned long **) data_ret) [0] = lisp_to_word (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (VECTORP (obj))
    {
      /* Lisp Vectors may represent a set of ATOMs;
	 a set of 16 or 32 bit INTEGERs;
	 or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
       */
      int i;

      if (SYMBOLP (XVECTOR_DATA (obj) [0]))
	/* This vector is an ATOM set */
	{
	  if (NILP (type)) type = QATOM;
	  *size_ret = XVECTOR_LENGTH (obj);
	  *format_ret = 32;
	  *data_ret = (unsigned char *) xmalloc ((*size_ret) * sizeof (GdkAtom));
	  for (i = 0; i < (int) (*size_ret); i++)
	    if (SYMBOLP (XVECTOR_DATA (obj) [i]))
	      (*(GdkAtom **) data_ret) [i] =
		symbol_to_gtk_atom (d, XVECTOR_DATA (obj) [i], 0);
	    else
              signal_error (Qerror, /* Qselection_error */
                            list2 (build_string
		   ("all elements of the vector must be of the same type"),
                                   obj));
	}
#if 0 /* #### MULTIPLE doesn't work yet */
      else if (VECTORP (XVECTOR_DATA (obj) [0]))
	/* This vector is an ATOM_PAIR set */
	{
	  if (NILP (type)) type = QATOM_PAIR;
	  *size_ret = XVECTOR_LENGTH (obj);
	  *format_ret = 32;
	  *data_ret = (unsigned char *)
	    xmalloc ((*size_ret) * sizeof (Atom) * 2);
	  for (i = 0; i < *size_ret; i++)
	    if (VECTORP (XVECTOR_DATA (obj) [i]))
	      {
		Lisp_Object pair = XVECTOR_DATA (obj) [i];
		if (XVECTOR_LENGTH (pair) != 2)
		  signal_error (Qerror,
                                list2 (build_string
       ("elements of the vector must be vectors of exactly two elements"),
				  pair));

		(*(GdkAtom **) data_ret) [i * 2] =
		  symbol_to_gtk_atom (d, XVECTOR_DATA (pair) [0], 0);
		(*(GdkAtom **) data_ret) [(i * 2) + 1] =
		  symbol_to_gtk_atom (d, XVECTOR_DATA (pair) [1], 0);
	      }
	    else
	      signal_error (Qerror,
                            list2 (build_string
		   ("all elements of the vector must be of the same type"),
                                   obj));
	}
#endif
      else
	/* This vector is an INTEGER set, or something like it */
	{
	  *size_ret = XVECTOR_LENGTH (obj);
	  if (NILP (type)) type = QINTEGER;
	  *format_ret = 16;
	  for (i = 0; i < (int) (*size_ret); i++)
	    if (CONSP (XVECTOR_DATA (obj) [i]))
	      *format_ret = 32;
	    else if (!INTP (XVECTOR_DATA (obj) [i]))
	      signal_error (Qerror, /* Qselection_error */
                            list2 (build_string
	("all elements of the vector must be integers or conses of integers"),
                                   obj));

	  *data_ret = (unsigned char *) xmalloc (*size_ret * (*format_ret/8));
	  for (i = 0; i < (int) (*size_ret); i++)
	    if (*format_ret == 32)
	      (*((unsigned long **) data_ret)) [i] =
		lisp_to_word (XVECTOR_DATA (obj) [i]);
	    else
	      (*((unsigned short **) data_ret)) [i] =
		(unsigned short) lisp_to_word (XVECTOR_DATA (obj) [i]);
	}
    }
  else
    signal_error (Qerror, /* Qselection_error */
                  list2 (build_string ("unrecognized selection data"),
                         obj));

  *type_ret = symbol_to_gtk_atom (d, type, 0);
}



static Lisp_Object
gtk_own_selection (Lisp_Object selection_name, Lisp_Object selection_value,
		   Lisp_Object how_to_add, Lisp_Object selection_type)
{
  struct device *d = decode_gtk_device (Qnil);
  GtkWidget *selecting_window = GTK_WIDGET (DEVICE_GTK_APP_SHELL (d));
  Lisp_Object selection_time;
  /* Use the time of the last-read mouse or keyboard event.
     For selection purposes, we use this as a sleazy way of knowing what the
     current time is in server-time.  This assumes that the most recently read
     mouse or keyboard event has something to do with the assertion of the
     selection, which is probably true.
     */
  guint32 thyme = DEVICE_GTK_MOUSE_TIMESTAMP (d);
  GdkAtom selection_atom;

  CHECK_SYMBOL (selection_name);
  selection_atom = symbol_to_gtk_atom (d, selection_name, 0);

  gtk_selection_owner_set (selecting_window,
			   selection_atom,
			   thyme);

  /* We do NOT use time_to_lisp() here any more, like we used to.
     That assumed equivalence of time_t and Time, which is not
     necessarily the case (e.g. under OSF on the Alphas, where
     Time is a 64-bit quantity and time_t is a 32-bit quantity).

     Opaque pointers are the clean way to go here.
  */
  selection_time = make_opaque (&thyme, sizeof (thyme));

  return selection_time;
}

static void
gtk_disown_selection (Lisp_Object selection, Lisp_Object timeval)
{
  struct device *d = decode_gtk_device (Qnil);
  GdkAtom selection_atom;
  guint32 timestamp;

  CHECK_SYMBOL (selection);
  selection_atom = symbol_to_gtk_atom (d, selection, 0);

  if (NILP (timeval))
    timestamp = DEVICE_GTK_MOUSE_TIMESTAMP (d);
  else
    {
      time_t the_time;
      lisp_to_time (timeval, &the_time);
      timestamp = (guint32) the_time;
    }

  gtk_selection_owner_set (NULL, selection_atom, timestamp);
}

static Lisp_Object
gtk_selection_exists_p (Lisp_Object selection,
			Lisp_Object selection_type)
{
  struct device *d = decode_gtk_device (Qnil);
  
  return (gdk_selection_owner_get (symbol_to_gtk_atom (d, selection, 0)) ? Qt : Qnil);
}


 
/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_select_gtk (void)
{
}

void
console_type_create_select_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, own_selection);
  CONSOLE_HAS_METHOD (gtk, disown_selection);
  CONSOLE_HAS_METHOD (gtk, selection_exists_p);
  CONSOLE_HAS_METHOD (gtk, get_foreign_selection);
}

void
vars_of_select_gtk (void)
{
  staticpro (&Vretrieved_selection);
  Vretrieved_selection = Qnil;

  DEFVAR_LISP ("gtk-sent-selection-hooks", &Vgtk_sent_selection_hooks /*
A function or functions to be called after we have responded to some
other client's request for the value of a selection that we own.  The
function(s) will be called with four arguments:
  - the name of the selection (typically PRIMARY, SECONDARY, or CLIPBOARD);
  - the name of the selection-type which we were requested to convert the
    selection into before sending (for example, STRING or LENGTH);
  - and whether we successfully transmitted the selection.
We might have failed (and declined the request) for any number of reasons,
including being asked for a selection that we no longer own, or being asked
to convert into a type that we don't know about or that is inappropriate.
This hook doesn't let you change the behavior of emacs's selection replies,
it merely informs you that they have happened.
*/ );
  Vgtk_sent_selection_hooks = Qunbound;
}
