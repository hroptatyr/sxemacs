/* Commonly-used symbols
   Copyright (C) 1995 Sun Microsystems.
   Copyright (C) 1995, 1996, 2000 Ben Wing.

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

/* See general-slots.h.
*/

#include <config.h>
#include "lisp.h"

#define SYMBOL(fou) Lisp_Object fou
#define SYMBOL_KEYWORD(la_cle_est_fou) Lisp_Object la_cle_est_fou
#define SYMBOL_GENERAL(tout_le_monde, est_fou) Lisp_Object tout_le_monde

#include "general-slots.h"

#undef SYMBOL
#undef SYMBOL_KEYWORD
#undef SYMBOL_GENERAL

void
syms_of_general (void)
{
#define SYMBOL(loco) DEFSYMBOL (loco)
#define SYMBOL_KEYWORD(meshugeneh) DEFKEYWORD (meshugeneh)
#define SYMBOL_GENERAL(vachement, fou) defsymbol (&vachement, fou)

#include "general-slots.h"

#undef SYMBOL
#undef SYMBOL_KEYWORD
#undef SYMBOL_GENERAL
}
