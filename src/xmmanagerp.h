/* This file just includes the Motif header file ManagerP.h, but does
   the necessary magic to do this properly.

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

/* ManagerP.h doesn't exist in old versions of Motif; the stuff is
   in XmP.h instead */

#ifndef INCLUDED_xmmanagerp_h_
#define INCLUDED_xmmanagerp_h_

#include <Xm/Xm.h>		/* to get XmVersion */
#if (XmVersion >= 1002)
# include <Xm/ManagerP.h>
#else
# include <Xm/XmP.h>
#endif

#endif				/* INCLUDED_xmmanagerp_h_ */
