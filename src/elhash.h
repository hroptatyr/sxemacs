/* Lisp interface to hash tables -- include file.
   Copyright (C) 1995, 1996 Ben Wing.

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

#ifndef INCLUDED_elhash_h_
#define INCLUDED_elhash_h_

typedef struct Lisp_Hash_Table Lisp_Hash_Table;

DECLARE_LRECORD (hash_table, Lisp_Hash_Table);

#define XHASH_TABLE(x) XRECORD (x, hash_table, Lisp_Hash_Table)
#define XSETHASH_TABLE(x, p) XSETRECORD (x, p, hash_table)
#define HASH_TABLEP(x) RECORDP (x, hash_table)
#define CHECK_HASH_TABLE(x) CHECK_RECORD (x, hash_table)
#define CONCHECK_HASH_TABLE(x) CONCHECK_RECORD (x, hash_table)

enum hash_table_weakness
{
  HASH_TABLE_NON_WEAK,
  HASH_TABLE_KEY_WEAK,
  HASH_TABLE_VALUE_WEAK,
  HASH_TABLE_KEY_VALUE_WEAK,
  HASH_TABLE_KEY_CAR_WEAK,
  HASH_TABLE_VALUE_CAR_WEAK,
  HASH_TABLE_KEY_CAR_VALUE_WEAK,
  HASH_TABLE_WEAK
};

enum hash_table_test
{
  HASH_TABLE_EQ,
  HASH_TABLE_EQL,
  HASH_TABLE_EQUAL
};

extern const struct lrecord_description hash_table_description[];

EXFUN (Fcopy_hash_table, 1);
EXFUN (Fhash_table_count, 1);
EXFUN (Fgethash, 3);
EXFUN (Fputhash, 3);
EXFUN (Fremhash, 2);
EXFUN (Fclrhash, 1);

typedef unsigned long hashcode_t;
typedef int (*hash_table_test_function_t) (Lisp_Object obj1, Lisp_Object obj2);
typedef unsigned long (*hash_table_hash_function_t) (Lisp_Object obj);
typedef int (*maphash_function_t) (Lisp_Object key, Lisp_Object value,
				   void* extra_arg);

Lisp_Object make_standard_lisp_hash_table (enum hash_table_test test,
					   size_t size,
					   double rehash_size,
					   double rehash_threshold,
					   enum hash_table_weakness weakness);

Lisp_Object make_general_lisp_hash_table (hash_table_hash_function_t hash_function,
					  hash_table_test_function_t test_function,
					  size_t size,
					  double rehash_size,
					  double rehash_threshold,
					  enum hash_table_weakness weakness);

Lisp_Object make_lisp_hash_table (size_t size,
				  enum hash_table_weakness weakness,
				  enum hash_table_test test);

void elisp_maphash (maphash_function_t function,
		    Lisp_Object hash_table, void *extra_arg);

void elisp_map_remhash (maphash_function_t predicate,
			Lisp_Object hash_table, void *extra_arg);

int finish_marking_weak_hash_tables (void);
void prune_weak_hash_tables (void);

void pdump_reorganize_hash_table (Lisp_Object);

#endif /* INCLUDED_elhash_h_ */
