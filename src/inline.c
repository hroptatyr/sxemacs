/* Repository for inline functions
   Copyright (C) 1995 Sun Microsystems, Inc.

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


/* Synched up with: Not in FSF. */

/* The purpose of this file is so that there is at least one actual
   definition of each inline function.  This is needed under GCC.  The
   reason is that under GCC we declare our inline functions `inline
   extern', which causes the inlined version to get used only for
   inlining, and in other cases to generate an external reference to
   the function.  This is more efficient than declaring our inline
   functions `inline static', which (in many cases) would cause a separate
   version of the function to get inserted into every source file that
   included the corresponding header file.  See internals.texi.

   Some compilers that recognize `inline' may not do the same
   `inline extern' business, so on those we just do `inline static'.
   */

/* Note to maintainers: This file contains a list of all header files
   that use the INLINE macro, either directly, or by using DECLARE_LRECORD.
   i.e. the output of ``grep -l -w 'DECLARE_LRECORD|INLINE_HEADER' *.h'' */

#define DONT_EXTERN_INLINE_HEADER_FUNCTIONS

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "bytecode.h"
#include "casetab.h"
#include "chartab.h"
<<<<<<< HEAD
#include "console.h"
#include "device.h"
#include "elhash.h"
#include "events.h"
#include "extents.h"
#include "faces.h"
#include "frame.h"
#include "glyphs.h"
#include "gui.h"
#include "keymap.h"
#include "lstream.h"
#include "objects.h"
=======
#include "ui/console.h"
#include "ui/device.h"
#include "elhash.h"
#include "events/events.h"
#include "extents.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/glyphs.h"
#include "ui/gui.h"
#include "ui/keymap.h"
#include "lstream.h"
#include "ui/objects.h"
>>>>>>> master
#include "opaque.h"
#include "process.h"
#include "rangetab.h"
#include "specifier.h"
#include "syntax.h"
<<<<<<< HEAD
#include "window.h"
=======
#include "ui/window.h"
>>>>>>> master
#include "bloom.h"
#include "dllist.h"

#ifdef HAVE_LDAP
#include "eldap.h"
#endif

#ifdef HAVE_POSTGRESQL
#include "postgresql.h"
#endif

#ifdef HAVE_TOOLBARS
<<<<<<< HEAD
#include "toolbar.h"
=======
#include "ui/toolbar.h"
>>>>>>> master
#endif

#ifdef HAVE_DATABASE
#include "database.h"
#endif

#ifdef HAVE_X_WINDOWS
<<<<<<< HEAD
#include "glyphs-x.h"
#include "gui-x.h"
#endif

#ifdef HAVE_GTK
#include "console-gtk.h"
#include "ui-gtk.h"
#endif

#ifdef FILE_CODING
<<<<<<< HEAD
#include "file-coding.h"
=======
#include "mule/file-coding.h"
>>>>>>> origin/master
#endif

#ifdef TOOLTALK
#include "tooltalk.h"
=======
#include "ui/X11/glyphs-x.h"
#include "ui/X11/gui-x.h"
#endif

#ifdef HAVE_GTK
#include "ui/Gtk/console-gtk.h"
#include "ui/Gtk/ui-gtk.h"
#endif

#ifdef FILE_CODING
#include "mule/file-coding.h"
>>>>>>> master
#endif

#ifdef HAVE_OPENSSL
#include "openssl.h"
#endif

#ifdef WITH_NUMBER_TYPES
<<<<<<< HEAD
<<<<<<< HEAD
#include "ent.h"
=======
#include "ent/ent.h"
>>>>>>> origin/master
=======
#include "ent/ent.h"
>>>>>>> master
#endif

#ifdef HAVE_LIBFFI
#include "effi.h"
#endif

#include "sound.h"
#include "media.h"
