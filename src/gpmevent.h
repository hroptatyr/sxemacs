/* GPM (General purpose mouse) support
   Copyright (C) 1997 William M. Perry <wmperry@gnu.org>
   Copyright (C) 1999 Free Software Foundation, Inc.

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

#ifndef INCLUDED_gpmevent_h_
#define INCLUDED_gpmevent_h_

int handle_gpm_read (Lisp_Event *event, struct console *con, int fd);
void connect_to_gpm (struct console *con);

#endif /* INCLUDED_gpmevent_h_ */
