/* Repository for inline functions
   Copyright (C) 1995 Sun Microsystems, Inc.

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
#include "opaque.h"
#include "process.h"
#include "rangetab.h"
#include "specifier.h"
#include "syntax.h"
#include "window.h"

#ifdef HAVE_LDAP
#include "eldap.h"
#endif

#ifdef HAVE_POSTGRESQL
#include "postgresql.h"
#endif

#ifdef HAVE_TOOLBARS
#include "toolbar.h"
#endif

#ifdef HAVE_DATABASE
#include "database.h"
#endif

#ifdef HAVE_X_WINDOWS
#include "glyphs-x.h"
#include "gui-x.h"
#endif

#ifdef HAVE_MS_WINDOWS
#include "console-msw.h"
#endif

#ifdef HAVE_GTK
#include "console-gtk.h"
#include "ui-gtk.h"
#endif

#ifdef FILE_CODING
#include "file-coding.h"
#endif

#ifdef TOOLTALK
#include "tooltalk.h"
#endif
