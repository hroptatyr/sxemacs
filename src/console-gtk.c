/* Console functions for X windows.
   Copyright (C) 1996 Ben Wing.

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

/* Authorship:

   Ben Wing: January 1996, for 19.14.
   William Perry: April 2000, for 21.1 (Gtk version)
 */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "process.h" /* canonicalize_host_name */
#include "redisplay.h" /* for display_arg */

DEFINE_CONSOLE_TYPE (gtk);

static int
gtk_initially_selected_for_input (struct console *con)
{
  return 1;
}

/* Remember, in all of the following functions, we have to verify
   the integrity of our input, because the generic functions don't. */

static Lisp_Object
gtk_device_to_console_connection (Lisp_Object connection, Error_behavior errb)
{
  /* Strip the trailing .# off of the connection, if it's there. */

  if (NILP (connection))
    return Qnil;
  else
    {
	connection = build_string ("gtk");
    }
  return connection;
}

static Lisp_Object
gtk_semi_canonicalize_console_connection (Lisp_Object connection,
					  Error_behavior errb)
{
  struct gcpro gcpro1;

  GCPRO1 (connection);

  connection = build_string ("gtk");

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
gtk_canonicalize_console_connection (Lisp_Object connection, Error_behavior errb)
{
  Lisp_Object hostname = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (connection, hostname);

  connection = build_string ("gtk");

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
gtk_semi_canonicalize_device_connection (Lisp_Object connection,
				         Error_behavior errb)
{
  struct gcpro gcpro1;

  GCPRO1 (connection);

  connection = build_string("gtk");

  RETURN_UNGCPRO (connection);
}

static Lisp_Object
gtk_canonicalize_device_connection (Lisp_Object connection, Error_behavior errb)
{
  struct gcpro gcpro1;

  GCPRO1 (connection);
  connection = build_string("gtk");

  RETURN_UNGCPRO (connection);
}

void
console_type_create_gtk (void)
{
  INITIALIZE_CONSOLE_TYPE (gtk, "gtk", "console-gtk-p");

  CONSOLE_HAS_METHOD (gtk, semi_canonicalize_console_connection);
  CONSOLE_HAS_METHOD (gtk, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (gtk, semi_canonicalize_device_connection);
  CONSOLE_HAS_METHOD (gtk, canonicalize_device_connection);
  CONSOLE_HAS_METHOD (gtk, device_to_console_connection);
  CONSOLE_HAS_METHOD (gtk, initially_selected_for_input);
  /* CONSOLE_HAS_METHOD (gtk, delete_console); */
}

void
reinit_console_type_create_gtk (void)
{
  REINITIALIZE_CONSOLE_TYPE (gtk);
}
