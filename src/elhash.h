/* Lisp interface to hash tables -- include file.
   Copyright (C) 1995, 1996 Ben Wing.

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

#ifndef INCLUDED_elhash_h_
#define INCLUDED_elhash_h_

typedef struct hash_table_s *hash_table_t;
typedef struct hentry_s *hentry_t;

extern void elhash_reinit(void);

typedef int (*hash_table_test_f)(Lisp_Object obj1, Lisp_Object obj2);
typedef long unsigned int (*hash_table_hash_f)(Lisp_Object obj);
typedef int (*maphash_f)(Lisp_Object key, Lisp_Object value, void*);

DECLARE_LRECORD(hash_table, struct hash_table_s);

#define XHASH_TABLE(x)		XRECORD(x, hash_table, struct hash_table_s)
#define XSETHASH_TABLE(x, p)	XSETRECORD(x, p, hash_table)
#define HASH_TABLEP(x)		RECORDP(x, hash_table)
#define CHECK_HASH_TABLE(x)	CHECK_RECORD(x, hash_table)
#define CONCHECK_HASH_TABLE(x)	CONCHECK_RECORD(x, hash_table)

enum hash_table_weakness {
	HASH_TABLE_NON_WEAK,
	HASH_TABLE_KEY_WEAK,
	HASH_TABLE_VALUE_WEAK,
	HASH_TABLE_KEY_VALUE_WEAK,
	HASH_TABLE_KEY_CAR_WEAK,
	HASH_TABLE_VALUE_CAR_WEAK,
	HASH_TABLE_KEY_CAR_VALUE_WEAK,
	HASH_TABLE_WEAK
};

enum hash_table_test {
	HASH_TABLE_EQ,
	HASH_TABLE_EQL,
	HASH_TABLE_EQUAL
};


struct hentry_s {
	Lisp_Object key;
	Lisp_Object value;
};

struct hash_table_s {
	struct lcrecord_header header;

	/* the sequence category */
	void *si;
	/* the dict (aset) category */
	void *di;

	size_t size;
	size_t count;
	size_t rehash_count;
	fpfloat rehash_size;
	fpfloat rehash_threshold;
	size_t golden_ratio;
	hash_table_hash_f hash_function;
	hash_table_test_f test_function;
	hentry_t hentries;
	enum hash_table_weakness weakness;
	Lisp_Object next_weak;	/* Used to chain together all of the weak
				   hash tables.  Don't mark through this. */
};

#if 0
#define HENTRY_CLEAR_P(hentry) ((*(EMACS_UINT*)(&((hentry)->key))) == 0)
#else
extern_inline int
HENTRY_CLEAR_P(const hentry_t h);
extern_inline int
HENTRY_CLEAR_P(const hentry_t h)
{
	return h->key == Qnull_pointer;
}
#endif
#define CLEAR_HENTRY(hentry)   \
  ((*(EMACS_UINT*)(&((hentry)->key)))   = 0, \
   (*(EMACS_UINT*)(&((hentry)->value))) = 0)

extern const struct lrecord_description hash_table_description[];

EXFUN(Fcopy_hash_table, 1);
EXFUN(Fhash_table_count, 1);
EXFUN(Fgethash, 3);
EXFUN(Fputhash, 3);
EXFUN(Fremhash, 2);
EXFUN(Fclrhash, 1);

Lisp_Object make_standard_lisp_hash_table(enum hash_table_test test,
					  size_t size,
					  fpfloat rehash_size,
					  fpfloat rehash_threshold,
					  enum hash_table_weakness weakness);

Lisp_Object make_general_lisp_hash_table(
	hash_table_hash_f hash_function,
	hash_table_test_f test_function,
	size_t size, fpfloat rehash_size, fpfloat rehash_threshold,
	enum hash_table_weakness weakness);

Lisp_Object make_lisp_hash_table(
	size_t size,
	enum hash_table_weakness weakness,
	enum hash_table_test test);

void elisp_maphash(maphash_f function, Lisp_Object ht, void *extra_arg);
void elisp_map_remhash(maphash_f predicate, Lisp_Object ht, void *extra_arg);

int finish_marking_weak_hash_tables(void);
void prune_weak_hash_tables(void);

void pdump_reorganize_hash_table(Lisp_Object);

#endif				/* INCLUDED_elhash_h_ */
