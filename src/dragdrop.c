/* Drag'n'Drop definitions
   created 03-may-98 by Oliver Graf <ograf@fga.de>
   Copyright (C) 1998 Oliver Graf <ograf@fga.de>

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

/* This file should be Mule-ized. */

/* A short introduction to the new Drag'n'Drop Model:

   Currently only drops from OffiX are implemented.

   A drop generates a extended misc-user-event, as defined in events.[ch].
   This event contains the same as a eval and a button event.
   The function of a drop is set to 'dragdrop-drop-dispatch' which will be
   defined in ../lisp/dragdrop.el.
   The object of the misc-user-event has the following format:
              ( TYPE . DATA )
   TYPE  is one of 'dragdrop-MIME and 'dragdrop-URL
   DATA  - if TYPE is 'dragdrop-URL, DATA is a list of valid URL strings. It
	   is always a list, also if only one URL string is within it.
	 - if TYPE is 'dragdrop-MIME, DATA is a list of MIME elements.
	   Each can be a string or a list.
	   if it is a string it is the pure MIME data complete with header
	   and body.
	   if it is a list it should look like
	      ( MIME-TYPE MIME-ENCODING MIME-DATA )
	   MIME-TYPE list of type and key.value conses. Same as in tm-view
	   MIME-ENC  the same (a string in this case)
	   MIME-DATA is a string
*/

#include <config.h>
#include "lisp.h"
#include "dragdrop.h"

/* The supported protocol list */
Lisp_Object Vdragdrop_protocols;

/* Drag'n'Drop data types known by XEmacs */
Lisp_Object Qdragdrop_MIME;
Lisp_Object Qdragdrop_URL;

/* External defined functions to handle Drag'n'Drop */
Lisp_Object Qdragdrop_drop_dispatch;

/* from wget -- thanxx Hrvoje */
/* A list of unsafe characters for encoding, as per RFC1738.  '@' and
   ':' (not listed in RFC) were added because of user/password
   encoding, and \033 for safe printing.  */

#define URL_UNSAFE " <>\"#%{}|\\^~[]`@:\033"

/* HEX digit -> ASCII char */
#define HEXD2ASC(x) (((x) < 10) ? ((x) + '0') : ((x) - 10 + 'A'))

/* Encodes the unsafe characters (listed in URL_UNSAFE) in a given
   string, returning a malloc-ed %XX encoded string.
   if method is != NULL it is prepended to the string. */
char *
dnd_url_hexify_string (const char *s, const char *m)
{
  const char *b;
  char *p, *res;
  int i;

  b = s;
  for (i = 0; *s; s++, i++)
    if (strchr (URL_UNSAFE, *s))
      i += 2; /* Two more characters (hex digits) */
  if (m)
    {
      res = (char *)xmalloc (i + 1 + strlen (m));
      strcpy (res, m);
      p = res + strlen (m);
    }
  else
    {
      res = (char *)xmalloc (i + 1);
      p = res;
    }
  for (s = b; *s; s++)
    if (strchr (URL_UNSAFE, *s))
      {
	const unsigned char c = *s;
	*p++ = '%';
	*p++ = HEXD2ASC (c >> 4);
	*p++ = HEXD2ASC (c & 0xf);
      }
    else
      *p++ = *s;
  *p = '\0';
  return res;
}

void
syms_of_dragdrop (void)
{
  defsymbol (&Qdragdrop_MIME, "dragdrop-MIME");
  defsymbol (&Qdragdrop_URL,  "dragdrop-URL");
  defsymbol (&Qdragdrop_drop_dispatch, "dragdrop-drop-dispatch");
}

void
vars_of_dragdrop (void)
{
  Fprovide (intern ("dragdrop-api"));

  DEFVAR_CONST_LISP ("dragdrop-protocols", &Vdragdrop_protocols /*
A list of supported Drag'n'drop protocols.
Each element is the feature symbol of the protocol.
*/ );
  
  Vdragdrop_protocols = Qnil;

#ifdef HAVE_MS_WINDOWS
  Vdragdrop_protocols = Fcons ( Qmswindows , Vdragdrop_protocols );
#endif
#ifdef HAVE_CDE
  Vdragdrop_protocols = Fcons ( intern ("cde") , Vdragdrop_protocols );
#endif
#ifdef HAVE_OFFIX_DND
  Vdragdrop_protocols = Fcons ( intern ("offix") , Vdragdrop_protocols );
#endif
#ifdef HAVE_GTK
  Vdragdrop_protocols = Fcons ( Qgtk , Vdragdrop_protocols );
#endif
}
