/* XEmacs routines to deal with case tables.
   Copyright (C) 2000 Yoshiki Hayashi.
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

#ifndef INCLUDED_casetab_h_
#define INCLUDED_casetab_h_

struct Lisp_Case_Table {
	struct lcrecord_header header;
	Lisp_Object downcase_table;
	Lisp_Object upcase_table;
	Lisp_Object case_canon_table;
	Lisp_Object case_eqv_table;
};
typedef struct Lisp_Case_Table Lisp_Case_Table;

DECLARE_LRECORD(case_table, Lisp_Case_Table);
#define XCASE_TABLE(x) XRECORD (x, case_table, Lisp_Case_Table)
#define XSETCASE_TABLE(x, p) XSETRECORD (x, p, case_table)
#define CASE_TABLEP(x) RECORDP (x, case_table)
#define CHECK_CASE_TABLE(x) CHECK_RECORD (x, case_table)
#define CONCHECK_CASE_TABLE(x) CONCHECK_RECORD (x, case_table)

#define CASE_TABLE_DOWNCASE(ct) ((ct)->downcase_table)
#define CASE_TABLE_UPCASE(ct) ((ct)->upcase_table)
#define CASE_TABLE_CANON(ct) ((ct)->case_canon_table)
#define CASE_TABLE_EQV(ct) ((ct)->case_eqv_table)
#define XCASE_TABLE_DOWNCASE(ct) (XCASE_TABLE (ct)->downcase_table)
#define XCASE_TABLE_UPCASE(ct) (XCASE_TABLE (ct)->upcase_table)
#define XCASE_TABLE_CANON(ct) (XCASE_TABLE (ct)->case_canon_table)
#define XCASE_TABLE_EQV(ct) (XCASE_TABLE (ct)->case_eqv_table)

#define SET_CASE_TABLE_DOWNCASE(ct, p) ((ct)->downcase_table = p)
#define SET_CASE_TABLE_UPCASE(ct, p) ((ct)->upcase_table = p)
#define SET_CASE_TABLE_CANON(ct, p) ((ct)->case_canon_table = p)
#define SET_CASE_TABLE_EQV(ct, p) ((ct)->case_eqv_table = p)
#define XSET_CASE_TABLE_DOWNCASE(ct, p)	\
  SET_CASE_TABLE_DOWNCASE (XCASE_TABLE (ct), p)
#define XSET_CASE_TABLE_UPCASE(ct, p)	\
  SET_CASE_TABLE_UPCASE (XCASE_TABLE (ct), p)
#define XSET_CASE_TABLE_CANON(ct, p)	\
  SET_CASE_TABLE_CANON (XCASE_TABLE (ct),  p)
#define XSET_CASE_TABLE_EQV(ct, p)	\
  SET_CASE_TABLE_EQV (XCASE_TABLE (ct),  p)

extern Lisp_Object Vstandard_case_table;

#endif				/* INCLUDED_casetab_h_ */
