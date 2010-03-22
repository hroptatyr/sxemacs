/* Portable data dumper for XEmacs.
   Copyright (C) 1999-2000 Olivier Galibert

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

#ifndef INCLUDED_dumper_h
#define INCLUDED_dumper_h

void pdump_objects_unmark(void);
extern void pdump(const char *);
extern int pdump_load(const char *);
extern unsigned int dump_id;

#endif				/* INCLUDED_dumper_h */
