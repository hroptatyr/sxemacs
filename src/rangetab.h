/* XEmacs routines to deal with range tables.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

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

/* Extracted from rangetab.c by O. Galibert, 1998. */

#ifndef INCLUDED_rangetab_h_
#define INCLUDED_rangetab_h_

typedef struct range_table_entry range_table_entry;
struct range_table_entry {
	EMACS_INT first;
	EMACS_INT last;
	Lisp_Object val;
};

typedef struct {
	Dynarr_declare(range_table_entry);
} range_table_entry_dynarr;

struct Lisp_Range_Table {
	struct lcrecord_header header;
	range_table_entry_dynarr *entries;
};
typedef struct Lisp_Range_Table Lisp_Range_Table;

DECLARE_LRECORD(range_table, Lisp_Range_Table);
#define XRANGE_TABLE(x) XRECORD (x, range_table, Lisp_Range_Table)
#define XSETRANGE_TABLE(x, p) XSETRECORD (x, p, range_table)
#define RANGE_TABLEP(x) RECORDP (x, range_table)
#define CHECK_RANGE_TABLE(x) CHECK_RECORD (x, range_table)

#endif				/* INCLUDED_rangetab_h_ */
