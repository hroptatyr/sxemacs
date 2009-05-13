/* Copyright (C) 2000 Free Software Foundation, Inc.

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

/* Authorship:

   Created May 2000 by Andy Piper.
   Windows-Mule stuff added by Ben Wing.
*/

#ifndef INCLUDED_syscommctrl_h_
#define INCLUDED_syscommctrl_h_


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

#endif				/* INCLUDED_syscommctrl_h_ */
