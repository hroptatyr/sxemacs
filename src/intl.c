/* Various functions for internationalizing XEmacs
   Copyright (C) 1993, 1994, 1995 Board of Trustees, University of Illinois.

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

/* This stuff is far, far from working. */

#include <config.h>
#include "lisp.h"

#include "bytecode.h"
#include "device.h"

#if defined (HAVE_X_WINDOWS) && defined (HAVE_X11_XLOCALE_H)
#include <X11/Xlocale.h>
#else
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#endif

#ifdef I18N4
#include <X11/Xlib.h>

unsigned long input_method_event_mask;
Atom wc_atom;

/* init_input -- Set things up for i18n level 4 input.
*/
void
init_input (const char *res_name, const char *res_class, Display *display)
{
  XIMStyles *styles;
  unsigned short i;

  input_method = 0;
  input_method_style = 0;
  initial_input_context = 0;
  input_method_event_mask = 0;

  input_method = XOpenIM (display, NULL,
			  (char *) res_name, (char *) res_class);

  if (!input_method)
    {
      stderr_out ("WARNING: XOpenIM() failed...no input server\n");
      return;
    }

  /* Query input method for supported input styles and pick one.
     Right now, we choose a style which supports root-window preediting. */
  XGetIMValues (input_method, XNQueryInputStyle, &styles, NULL);
  for (i = 0; i < styles->count_styles; i++)
    {
      if (styles->supported_styles[i] == (XIMPreeditNothing|XIMStatusNothing))
	{
	  input_method_style= styles->supported_styles[i];
	  break;
	}
    }

  if (!input_method_style)
    {
      stderr_out ("WARNING: Could not find suitable input style.\n");
      return;
    }

  initial_input_context = XCreateIC (input_method,
				     XNInputStyle, input_method_style,
				     NULL);
  if (!initial_input_context)
    {
      stderr_out ("WARNING: Could not create input context.\n");
      return;
    }

  XGetICValues (initial_input_context,
		XNFilterEvents, &input_method_event_mask,
		NULL);

  /* Get a new atom for wide character client messages. */
  wc_atom = XInternAtom (display, "Wide Character Event", False);
}


/*static widechar_string composed_input_buf = EMPTY_WIDECHAR_STRING;*/

#define XIM_Composed_Text_BUFSIZE 64
typedef struct XIM_Composed_Text {
  int size;
  wchar_t data [XIM_Composed_Text_BUFSIZE];
} XIM_Composed_Text;
static XIM_Composed_Text composed_input_buf = {XIM_Composed_Text_BUFSIZE, {0}};
/*static wcidechar composed_input_buf [64] = {0};*/
Window main_window;  /* Convenient way to refer to main Era window. */

/* x_get_composed_input -- Process results of input method composition.

   This function copies the results of the input method composition to
   composed_input_buf.  Then for each character, a custom event of type
   wc_atom is sent with the character as its data.

   It is probably more efficient to copy the composition results to some
   allocated memory and send a single event pointing to that memory.
   That would cut down on the event processing as well as allow quick
   insertion into the buffer of the whole string.  It might require some
   care, though, to avoid fragmenting memory through the allocation and
   freeing of many small chunks.  Maybe the existing system for
   (single-byte) string allocation can be used, multiplying the length by
   sizeof (wchar_t) to get the right size.
*/
void
x_get_composed_input (XKeyPressedEvent *x_key_event, XIC context,
		      Display *display)
{
  KeySym keysym;
  Status status;
  int len;
  int i;
  XClientMessageEvent new_event;

 retry:
  len = XwcLookupString (context, x_key_event, composed_input_buf.data,
			 composed_input_buf.size, &keysym, &status);
  switch (status)
    {
    case XBufferOverflow:
      /* GROW_WC_STRING (&composed_input_buf, 32); mrb */
      goto retry;
    case XLookupChars:
      break;
    default:
      abort ();
    }

  new_event.type = ClientMessage;
  new_event.display = x_key_event->display;
  new_event.window = x_key_event->window;
  new_event.message_type = wc_atom;
  new_event.format = 32;  /* 32-bit wide data */
  new_event.data.l[2] = new_event.data.l[3] = new_event.data.l[4] = 0L;
  new_event.data.l[0] = x_key_event->time;
  for (i = 0; i < len; i++) {
    new_event.data.l[1] = ((wchar_t *) composed_input_buf.data)[i];
    XSendEvent (display, main_window, False, 0L, (XEvent *) &new_event);
  }
}
#endif /* I18N4 */


Lisp_Object Qdefer_gettext;

DEFUN ("ignore-defer-gettext", Fignore_defer_gettext, 1, 1, 0, /*
If OBJECT is of the form (defer-gettext "string"), return the string.
The purpose of the defer-gettext symbol is to identify strings which
are translated when they are referenced instead of when they are defined.
*/
       (object))
{
  if (CONSP (object)
      && SYMBOLP (Fcar (object))
      && EQ (Fcar (object), Qdefer_gettext))
    return Fcar (Fcdr (object));
  else
    return object;
}

DEFUN ("gettext", Fgettext, 1, 1, 0, /*
Look up STRING in the default message domain and return its translation.
This function does nothing if I18N3 was not enabled when Emacs was compiled.
*/
       (string))
{
#ifdef I18N3
  /* #### What should happen here is:

     1) If the string has no `string-translatable' property or its value
        is nil, no translation takes place.  The `string-translatable' property
	only gets added when a constant string is read in from a .el or .elc
	file, to avoid excessive translation.  (The user can also explicitly
	add this property to a string.)
     2) If the string's `string-translatable' property is a string,
	that string should be returned.  `format' add this property.
	This allows translation to take place at the proper time but
	avoids excessive translation if the string is not destined for
	a translating stream.  (See print_internal().)
     3) If gettext() returns the same string, then Fgettext() should return
        the same object, minus the 'string-translatable' property. */

  if (STRINGP (string)) {
#ifdef DEBUG_XEMACS
    stderr_out ("\nFgettext (%s) called.\n", XSTRING_DATA (string));
#endif
    return build_string (gettext ((char *) XSTRING_DATA (string)));
  } else {
    return string;
  }
#else
  return string;
#endif
}

#ifdef I18N3

/* #### add the function `force-gettext', perhaps in Lisp.  This
   ignores the `string-translatable' property and simply calls gettext()
   on the string.  Add the functions `set-string-translatable' and
   `set-stream-translating'. */

#endif

DEFUN ("dgettext", Fdgettext, 2, 2, 0, /*
Look up STRING in the specified message domain and return its translation.
This function does nothing if I18N3 was not enabled when Emacs was compiled.
*/
       (domain, string))
{
  CHECK_STRING (domain);
  CHECK_STRING (string);
#ifdef I18N3
  return build_string (dgettext ((char *) XSTRING_DATA (domain),
				 (char *) XSTRING_DATA (string)));
#else
  return string;
#endif
}

DEFUN ("bind-text-domain", Fbind_text_domain, 2, 2, 0, /*
Associate a pathname with a message domain.
Here's how the path to message files is constructed under SunOS 5.0:
  {pathname}/{LANG}/LC_MESSAGES/{domain}.mo
This function does nothing if I18N3 was not enabled when Emacs was compiled.
*/
       (domain, pathname))
{
  CHECK_STRING (domain);
  CHECK_STRING (pathname);
#ifdef I18N3
  return build_string (bindtextdomain ((char *) XSTRING_DATA (domain),
				       (char *) XSTRING_DATA (pathname)));
#else
  return Qnil;
#endif
}

extern int load_in_progress;

DEFUN ("set-domain", Fset_domain, 1, 1, 0, /*
Specify the domain used for translating messages in this source file.
The domain declaration may only appear at top-level, and should precede
all function and variable definitions.

The presence of this declaration in a compiled file effectively sets the
domain of all functions and variables which are defined in that file.
Bug: it has no effect on source (.el) files, only compiled (.elc) files.
*/
       (domain_name))
{
  CHECK_STRING (domain_name);
  if (load_in_progress)
    return (domain_name);
  else
    return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
init_intl_very_early (void)
{
#if defined (I18N2) || defined (I18N3) || defined (I18N4)
  setlocale (LC_ALL, "");
  setlocale(LC_NUMERIC, "C");
#endif

#ifdef I18N3
  textdomain ("emacs");
#endif
}

void
syms_of_intl (void)
{
  /* defer-gettext is defined as a symbol because when it is used in menu
     specification strings, it is not evaluated as a function by
     menu_item_descriptor_to_widget_value(). */
  defsymbol (&Qdefer_gettext, "defer-gettext");

  DEFSUBR (Fignore_defer_gettext);
  DEFSUBR (Fgettext);
  DEFSUBR (Fdgettext);
  DEFSUBR (Fbind_text_domain);
  DEFSUBR (Fset_domain);
}

void
vars_of_intl (void)
{
#ifdef I18N2
  Fprovide (intern ("i18n2"));
#endif
#ifdef I18N3
  Fprovide (intern ("i18n3"));
#endif
}
