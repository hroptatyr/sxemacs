/* Copyright (C) 2000 Free Software Foundation, Inc.

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

   Created May 2000 by Andy Piper.
   Windows-Mule stuff added by Ben Wing.
*/

#ifndef INCLUDED_syscommctrl_h_
#define INCLUDED_syscommctrl_h_

#if !defined (CYGWIN_VERSION_DLL_MAJOR) || CYGWIN_VERSION_DLL_MAJOR > 20
/* Appears to be missing in Cygwin b20.1; requisite includes are in
   Windows32/Messages.h and get included automatically with windows.h */
#include <commctrl.h>
#endif

#ifndef TB_SETIMAGELIST
#define TB_SETIMAGELIST (WM_USER + 48)
#endif
#ifndef TB_GETIMAGELIST
#define TB_GETIMAGELIST (WM_USER + 49)
#endif
#ifndef TB_SETDISABLEDIMAGELIST
#define TB_SETDISABLEDIMAGELIST (WM_USER + 54)
#endif
#ifndef TB_GETDISABLEDIMAGELIST
#define TB_GETDISABLEDIMAGELIST (WM_USER + 55)
#endif
#ifndef TB_SETPADDING
#define TB_SETPADDING   (WM_USER + 87)
#endif
#ifndef TBSTYLE_FLAT
#define TBSTYLE_FLAT 0x800
#endif
#ifndef TCS_BOTTOM
#define TCS_BOTTOM 0x0002
#endif
#ifndef TCS_VERTICAL
#define TCS_VERTICAL 0x0080
#endif
#ifndef PBS_SMOOTH
#define PBS_SMOOTH              0x01
#endif

#ifndef ICC_BAR_CLASSES
#define ICC_BAR_CLASSES 4
#endif

#endif /* INCLUDED_syscommctrl_h_ */
