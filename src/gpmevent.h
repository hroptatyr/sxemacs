/* GPM (General purpose mouse) support
   Copyright (C) 1997 William M. Perry <wmperry@gnu.org>
   Copyright (C) 1999 Free Software Foundation, Inc.

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


#ifndef INCLUDED_gpmevent_h_
#define INCLUDED_gpmevent_h_

int handle_gpm_read(Lisp_Event * event, struct console *con, int fd);
void connect_to_gpm(struct console *con);

#endif				/* INCLUDED_gpmevent_h_ */
