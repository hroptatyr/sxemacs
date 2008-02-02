/* Header file for database functions
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


/* This file is only necessary to get inline handling correct.
   See inline.c  */

#ifndef INCLUDED_database_h_
#define INCLUDED_database_h_

typedef struct Lisp_Database Lisp_Database;
DECLARE_LRECORD(database, Lisp_Database);

#endif				/* INCLUDED_database_h_ */
