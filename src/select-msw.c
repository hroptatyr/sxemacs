/* mswindows selection processing for XEmacs
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
   Hacked by Alastair Houghton, July 2000 for enhanced clipboard support.
*/

#include <config.h>
#include "lisp.h"
#include "frame.h"
#include "select.h"
#include "opaque.h"
#include "file-coding.h"
#include "buffer.h"

#include "console-msw.h"

/* A list of handles that we must release. Not accessible from Lisp. */
static Lisp_Object Vhandle_alist;

/* Test if this is an X symbol that we understand */
static int
x_sym_p (Lisp_Object value)
{
  if (NILP (value) || INTP (value))
    return 0;

  /* Check for some of the X symbols */
  if (EQ (value, QSTRING))		return 1;
  if (EQ (value, QTEXT))		return 1;
  if (EQ (value, QCOMPOUND_TEXT))	return 1;

  return 0;
}

/* This converts a Lisp symbol to an MS-Windows clipboard format.
   We have symbols for all predefined clipboard formats, but that
   doesn't mean we support them all ;-)
   The name of this function is actually a lie - it also knows about
   integers and strings... */
static UINT
symbol_to_ms_cf (Lisp_Object value)
{
  /* If it's NIL, we're in trouble. */
  if (NILP (value))			return 0;

  /* If it's an integer, assume it's a format ID */
  if (INTP (value))			return (UINT) (XINT (value));

  /* If it's a string, register the format(!) */
  if (STRINGP (value))
    return RegisterClipboardFormat (XSTRING_DATA (value));

  /* Check for Windows clipboard format symbols */
  if (EQ (value, QCF_TEXT))		return CF_TEXT;
  if (EQ (value, QCF_BITMAP))		return CF_BITMAP;
  if (EQ (value, QCF_METAFILEPICT))	return CF_METAFILEPICT;
  if (EQ (value, QCF_SYLK))		return CF_SYLK;
  if (EQ (value, QCF_DIF))		return CF_DIF;
  if (EQ (value, QCF_TIFF))		return CF_TIFF;
  if (EQ (value, QCF_OEMTEXT))		return CF_OEMTEXT;
  if (EQ (value, QCF_DIB))		return CF_DIB;
#ifdef CF_DIBV5
  if (EQ (value, QCF_DIBV5))		return CF_DIBV5;
#endif
  if (EQ (value, QCF_PALETTE))		return CF_PALETTE;
  if (EQ (value, QCF_PENDATA))		return CF_PENDATA;
  if (EQ (value, QCF_RIFF))		return CF_RIFF;
  if (EQ (value, QCF_WAVE))		return CF_WAVE;
  if (EQ (value, QCF_UNICODETEXT))	return CF_UNICODETEXT;
  if (EQ (value, QCF_ENHMETAFILE))	return CF_ENHMETAFILE;
  if (EQ (value, QCF_HDROP))		return CF_HDROP;
  if (EQ (value, QCF_LOCALE))		return CF_LOCALE;
  if (EQ (value, QCF_OWNERDISPLAY))	return CF_OWNERDISPLAY;
  if (EQ (value, QCF_DSPTEXT))		return CF_DSPTEXT;
  if (EQ (value, QCF_DSPBITMAP))	return CF_DSPBITMAP;
  if (EQ (value, QCF_DSPMETAFILEPICT))	return CF_DSPMETAFILEPICT;
  if (EQ (value, QCF_DSPENHMETAFILE))	return CF_DSPENHMETAFILE;

  return 0;
}

/* This converts an MS-Windows clipboard format to its corresponding
   Lisp symbol, or a Lisp integer otherwise. */
static Lisp_Object
ms_cf_to_symbol (UINT format)
{
  switch (format)
    {
    case CF_TEXT:		return QCF_TEXT;
    case CF_BITMAP:		return QCF_BITMAP;
    case CF_METAFILEPICT:	return QCF_METAFILEPICT;
    case CF_SYLK:		return QCF_SYLK;
    case CF_DIF:		return QCF_DIF;
    case CF_TIFF:		return QCF_TIFF;
    case CF_OEMTEXT:		return QCF_OEMTEXT;
    case CF_DIB:		return QCF_DIB;
#ifdef CF_DIBV5
    case CF_DIBV5:		return QCF_DIBV5;
#endif
    case CF_PALETTE:		return QCF_PALETTE;
    case CF_PENDATA:		return QCF_PENDATA;
    case CF_RIFF:		return QCF_RIFF;
    case CF_WAVE:		return QCF_WAVE;
    case CF_UNICODETEXT:	return QCF_UNICODETEXT;
    case CF_ENHMETAFILE:	return QCF_ENHMETAFILE;
    case CF_HDROP:		return QCF_HDROP;
    case CF_LOCALE:		return QCF_LOCALE;
    case CF_OWNERDISPLAY:	return QCF_OWNERDISPLAY;
    case CF_DSPTEXT:		return QCF_DSPTEXT;
    case CF_DSPBITMAP:		return QCF_DSPBITMAP;
    case CF_DSPMETAFILEPICT:	return QCF_DSPMETAFILEPICT;
    case CF_DSPENHMETAFILE:	return QCF_DSPENHMETAFILE;
    default:			return make_int ((int) format);
    }
}

/* Test if the specified clipboard format is auto-released by the OS. If
   not, we must remember the handle on Vhandle_alist, and free it if
   the clipboard is emptied or if we set data with the same format. */
static int
cf_is_autofreed (UINT format)
{
  switch (format)
    {
      /* This list comes from the SDK documentation */
    case CF_DSPENHMETAFILE:
    case CF_DSPMETAFILEPICT:
    case CF_ENHMETAFILE:
    case CF_METAFILEPICT:
    case CF_BITMAP:
    case CF_DSPBITMAP:
    case CF_PALETTE:
    case CF_DIB:
#ifdef CF_DIBV5
    case CF_DIBV5:
#endif
    case CF_DSPTEXT:
    case CF_OEMTEXT:
    case CF_TEXT:
    case CF_UNICODETEXT:
      return TRUE;

    default:
      return FALSE;
    }
}

/* Do protocol to assert ourself as a selection owner.

   Under mswindows, we:

   * Only set the clipboard if (eq selection-name 'CLIPBOARD)

   * Check if an X atom name has been passed. If so, convert to CF_TEXT
     (or CF_UNICODETEXT) remembering to perform LF -> CR-LF conversion.

   * Otherwise assume the data is formatted appropriately for the data type
     that was passed.

   Then set the clipboard as necessary.
*/
static Lisp_Object
mswindows_own_selection (Lisp_Object selection_name,
			 Lisp_Object selection_value,
			 Lisp_Object how_to_add,
			 Lisp_Object selection_type,
			 int owned_p /* Not used */)
{
  HGLOBAL 	hValue = NULL;
  UINT		cfType;
  int		is_X_type = FALSE;
  Lisp_Object	cfObject;
  Lisp_Object	data = Qnil;
  int		size;
  void		*src, *dst;
  struct frame  *f = NULL;

  /* Only continue if we're trying to set the clipboard - mswindows doesn't
     use the same selection model as X */
  if (!EQ (selection_name, QCLIPBOARD))
    return Qnil;

  /* If this is one of the X-style atom name symbols, or NIL, convert it
     as appropriate */
  if (NILP (selection_type) || x_sym_p (selection_type))
    {
      /* Should COMPOUND_TEXT map to CF_UNICODETEXT? */
      cfType = CF_TEXT;
      cfObject = QCF_TEXT;
      is_X_type = TRUE;
    }
  else
    {
      cfType = symbol_to_ms_cf (selection_type);

      /* Only continue if we can figure out a clipboard type */
      if (!cfType)
	return Qnil;

      cfObject = selection_type;
    }

  /* Convert things appropriately */
  data = select_convert_out (selection_name,
			     cfObject,
			     selection_value);

  if (NILP (data))
    return Qnil;

  if (CONSP (data))
    {
      if (!EQ (XCAR (data), cfObject))
	cfType = symbol_to_ms_cf (XCAR (data));

      if (!cfType)
	return Qnil;

      data = XCDR (data);
    }

  /* We support opaque or string values, but we only mention string
     values for now... */
  if (!OPAQUEP (data)
      && !STRINGP (data))
    return Qnil;

  /* Compute the data length */
  if (OPAQUEP (data))
    size = XOPAQUE_SIZE (data);
  else
    size = XSTRING_LENGTH (data) + 1;

  /* Find the frame */
  f = selected_frame ();

  /* Open the clipboard */
  if (!OpenClipboard (FRAME_MSWINDOWS_HANDLE (f)))
    return Qnil;

  /* Allocate memory */
  hValue = GlobalAlloc (GMEM_DDESHARE | GMEM_MOVEABLE, size);

  if (!hValue)
    {
      CloseClipboard ();

      return Qnil;
    }

  /* Copy the data */
  if (OPAQUEP (data))
    src = XOPAQUE_DATA (data);
  else
    src = XSTRING_DATA (data);

  dst = GlobalLock (hValue);

  if (!dst)
    {
      GlobalFree (hValue);
      CloseClipboard ();

      return Qnil;
    }

  memcpy (dst, src, size);

  GlobalUnlock (hValue);

  /* Empty the clipboard if we're replacing everything */
  if (NILP (how_to_add) || EQ (how_to_add, Qreplace_all))
    {
      if (!EmptyClipboard ())
	{
	  CloseClipboard ();
	  GlobalFree (hValue);

	  return Qnil;
	}
    }

  /* Append is currently handled in select.el; perhaps this should change,
     but it only really makes sense for ordinary text in any case... */

  SetClipboardData (cfType, hValue);

  if (!cf_is_autofreed (cfType))
    {
      Lisp_Object alist_elt = Qnil, rest;
      Lisp_Object cfType_int = make_int (cfType);

      /* First check if there's an element in the alist for this type
	 already. */
      alist_elt = assq_no_quit (cfType_int, Vhandle_alist);

      /* Add an element to the alist */
      Vhandle_alist = Fcons (Fcons (cfType_int, make_opaque_ptr (hValue)),
			     Vhandle_alist);

      if (!NILP (alist_elt))
	{
	  /* Free the original handle */
	  GlobalFree ((HGLOBAL) get_opaque_ptr (XCDR (alist_elt)));

	  /* Remove the original one (adding first makes life easier, because
	     we don't have to special case this being the first element)      */
	  for (rest = Vhandle_alist; !NILP (rest); rest = Fcdr (rest))
	    if (EQ (cfType_int, Fcar (XCDR (rest))))
	      {
		XCDR (rest) = Fcdr (XCDR (rest));
		break;
	      }
	}
    }

  CloseClipboard ();

  /* #### Should really return a time, though this is because of the
     X model (by the looks of things) */
  return Qnil;
}

static Lisp_Object
mswindows_available_selection_types (Lisp_Object selection_name)
{
  Lisp_Object	types = Qnil;
  UINT		format = 0;
  struct frame  *f = NULL;

  if (!EQ (selection_name, QCLIPBOARD))
    return Qnil;

  /* Find the frame */
  f = selected_frame ();

  /* Open the clipboard */
  if (!OpenClipboard (FRAME_MSWINDOWS_HANDLE (f)))
    return Qnil;

  /* #### ajh - Should there be an unwind-protect handler around this?
                It could (well it probably won't, but it's always better to
		be safe) run out of memory and leave the clipboard open... */

  while ((format = EnumClipboardFormats (format)))
    types = Fcons (ms_cf_to_symbol (format), types);

  /* Close it */
  CloseClipboard ();

  return types;
}

static Lisp_Object
mswindows_register_selection_data_type (Lisp_Object type_name)
{
  /* Type already checked in select.c */
  const char *name = XSTRING_DATA (type_name);
  UINT	      format;

  format = RegisterClipboardFormat (name);

  if (format)
    return make_int ((int) format);
  else
    return Qnil;
}

static Lisp_Object
mswindows_selection_data_type_name (Lisp_Object type_id)
{
  UINT		format;
  int		numchars;
  char		name_buf[128];

  /* If it's an integer, convert to a symbol if appropriate */
  if (INTP (type_id))
    type_id = ms_cf_to_symbol (XINT (type_id));

  /* If this is a symbol, return it */
  if (SYMBOLP (type_id))
    return type_id;

  /* Find the format code */
  format = symbol_to_ms_cf (type_id);

  if (!format)
    return Qnil;

  /* Microsoft, stupid Microsoft */
  numchars = GetClipboardFormatName (format, name_buf, 128);

  if (numchars)
    {
      Lisp_Object name;

      /* Do this properly - though we could support UNICODE (UCS-2) if
         MULE could hack it. */
      name = make_ext_string (name_buf, numchars,
			      Fget_coding_system (Qraw_text));

      return name;
    }

  return Qnil;
}

static Lisp_Object
mswindows_get_foreign_selection (Lisp_Object selection_symbol,
				 Lisp_Object target_type)
{
  HGLOBAL	hValue = NULL;
  UINT		cfType;
  Lisp_Object	cfObject = Qnil, ret = Qnil, value = Qnil;
  int		is_X_type = FALSE;
  int		size;
  void		*data;
  struct frame  *f = NULL;
  struct gcpro	gcpro1;

  /* Only continue if we're trying to read the clipboard - mswindows doesn't
     use the same selection model as X */
  if (!EQ (selection_symbol, QCLIPBOARD))
    return Qnil;

  /* If this is one of the X-style atom name symbols, or NIL, convert it
     as appropriate */
  if (NILP (target_type) || x_sym_p (target_type))
    {
      /* Should COMPOUND_TEXT map to CF_UNICODETEXT? */
      cfType = CF_TEXT;
      cfObject = QCF_TEXT;
      is_X_type = TRUE;
    }
  else
    {
      cfType = symbol_to_ms_cf (target_type);

      /* Only continue if we can figure out a clipboard type */
      if (!cfType)
	return Qnil;

      cfObject = ms_cf_to_symbol (cfType);
    }

  /* Find the frame */
  f = selected_frame ();

  /* Open the clipboard */
  if (!OpenClipboard (FRAME_MSWINDOWS_HANDLE (f)))
    return Qnil;

  /* Read the clipboard */
  hValue = GetClipboardData (cfType);

  if (!hValue)
    {
      CloseClipboard ();

      return Qnil;
    }

  /* Find the data */
  size = GlobalSize (hValue);
  data = GlobalLock (hValue);

  if (!data)
    {
      CloseClipboard ();

      return Qnil;
    }

  /* Place it in a Lisp string */
  TO_INTERNAL_FORMAT (DATA, (data, size),
		      LISP_STRING, ret,
		      Qbinary);

  GlobalUnlock (data);
  CloseClipboard ();

  GCPRO1 (ret);

  /* Convert this to the appropriate type. If we can't find anything,
     then we return a cons of the form (DATA-TYPE . STRING), where the
     string contains the raw binary data. */
  value = select_convert_in (selection_symbol,
			     cfObject,
			     ret);

  UNGCPRO;

  if (NILP (value))
    return Fcons (cfObject, ret);
  else
    return value;
}

static void
mswindows_disown_selection (Lisp_Object selection, Lisp_Object timeval)
{
  if (EQ (selection, QCLIPBOARD))
    {
      BOOL success = OpenClipboard (NULL);
      if (success)
	{
	  success = EmptyClipboard ();
	  /* Close it regardless of whether empty worked. */
	  if (!CloseClipboard ())
	    success = FALSE;
	}

      /* #### return success ? Qt : Qnil; */
    }
}

void
mswindows_destroy_selection (Lisp_Object selection)
{
  /* Do nothing if this isn't for the clipboard. */
  if (!EQ (selection, QCLIPBOARD))
    return;

  /* Right. We need to delete everything in Vhandle_alist. */
  {
    LIST_LOOP_2 (elt, Vhandle_alist)
      GlobalFree ((HGLOBAL) get_opaque_ptr (XCDR (elt)));
  }

  Vhandle_alist = Qnil;
}

static Lisp_Object
mswindows_selection_exists_p (Lisp_Object selection,
			      Lisp_Object selection_type)
{
  /* We used to be picky about the format, but now we support anything. */
  if (EQ (selection, QCLIPBOARD))
    {
      if (NILP (selection_type))
	return CountClipboardFormats () ? Qt : Qnil;
      else
	return IsClipboardFormatAvailable (symbol_to_ms_cf (selection_type))
	  ? Qt : Qnil;
    }
  else
    return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_select_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, own_selection);
  CONSOLE_HAS_METHOD (mswindows, disown_selection);
  CONSOLE_HAS_METHOD (mswindows, selection_exists_p);
  CONSOLE_HAS_METHOD (mswindows, get_foreign_selection);
  CONSOLE_HAS_METHOD (mswindows, available_selection_types);
  CONSOLE_HAS_METHOD (mswindows, register_selection_data_type);
  CONSOLE_HAS_METHOD (mswindows, selection_data_type_name);
}

void
syms_of_select_mswindows (void)
{
}

void
vars_of_select_mswindows (void)
{
  /* Initialise Vhandle_alist */
  Vhandle_alist = Qnil;
  staticpro (&Vhandle_alist);
}
