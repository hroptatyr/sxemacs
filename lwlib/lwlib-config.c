/* Flags indicating how lwlib was compiled.
   Copyright (C) 1994 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

The Lucid Widget Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* This is a kludge to make sure emacs can only link against a version of
   lwlib that was compiled in the right way.  Emacs references symbols which
   correspond to the way it thinks lwlib was compiled, and if lwlib wasn't
   compiled in that way, then somewhat meaningful link errors will result.
   The alternatives to this range from obscure link errors, to obscure
   runtime errors that look a lot like bugs. */

#include <config.h>
#include "lwlib.h"

#include <X11/Xlib.h>		/* to get XlibSpecificationRelease */
#ifdef NEED_MOTIF
#include <Xm/Xm.h>		/* to get XmVersion */
#endif	/* NEED_MOTIF */

#if (XlibSpecificationRelease == 4)
int lwlib_uses_x11r4;
#elif (XlibSpecificationRelease == 5)
int lwlib_uses_x11r5;
#elif (XlibSpecificationRelease == 6)
int lwlib_uses_x11r6;
#else
int lwlib_uses_unknown_x11;
#endif

#ifdef NEED_MOTIF
int lwlib_uses_motif;
#if (XmVersion >= 1002 )
int lwlib_uses_motif_1_2;
#else
int lwlib_does_not_use_motif_1_2;
#endif
#else
int lwlib_does_not_use_motif;
int lwlib_does_not_use_motif_1_2;
#endif	/* NEED_MOTIF */

#ifdef LWLIB_MENUBARS_LUCID
int lwlib_menubars_lucid;
#else
# ifdef LWLIB_MENUBARS_MOTIF
int lwlib_menubars_motif;
# else
int lwlib_does_not_support_menubars;
# endif
#endif

#ifdef LWLIB_SCROLLBARS_LUCID
int lwlib_scrollbars_lucid;
#else
# ifdef LWLIB_SCROLLBARS_MOTIF
int lwlib_scrollbars_motif;
# else
#  ifdef LWLIB_SCROLLBARS_ATHENA
int lwlib_scrollbars_athena;
#  else
int lwlib_does_not_support_scrollbars;
#  endif
# endif
#endif

#ifdef LWLIB_DIALOGS_MOTIF
int lwlib_dialogs_motif;
#else
# ifdef LWLIB_DIALOGS_ATHENA
int lwlib_dialogs_athena;
# else
int lwlib_does_not_support_dialogs;
# endif
#endif

#ifdef LWLIB_WIDGETS_MOTIF
int lwlib_widgets_motif;
#else
# ifdef LWLIB_WIDGETS_ATHENA
int lwlib_widgets_athena;
# else
int lwlib_does_not_support_widgets;
# endif
#endif
