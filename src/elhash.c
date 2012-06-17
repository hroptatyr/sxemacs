/* Implementation of the hash table lisp object type.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1997 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"
#include "bytecode.h"
#include "elhash.h"
/* for the category subsystem */
#include "category.h"
#include "seq.h"
#include "dict.h"
#include "ent/ent.h"

Lisp_Object Qhash_tablep;
static Lisp_Object Qhashtable, Qhash_table;
static Lisp_Object Qweakness, Qvalue, Qkey_or_value, Qkey_and_value;
static Lisp_Object Vall_weak_hash_tables;
static Lisp_Object Qrehash_size, Qrehash_threshold;
static Lisp_Object Q_size, Q_weakness;
Lisp_Object Q_test;
static Lisp_Object Q_rehash_size, Q_rehash_threshold;

/* obsolete as of 19990901 in xemacs-21.2 */
static Lisp_Object Qweak, Qkey_weak, Qvalue_weak, Qkey_or_value_weak;
static Lisp_Object Qnon_weak, Q_type;


#define HASH_TABLE_DEFAULT_SIZE 16
#define HASH_TABLE_DEFAULT_REHASH_SIZE 1.3
#define HASH_TABLE_MIN_SIZE 10

#define HASH_CODE(key, ht)						\
  ((((ht)->hash_function ? (ht)->hash_function (key) : LISP_HASH (key))	\
    * (ht)->golden_ratio)						\
   % (ht)->size)

#define KEYS_EQUAL_P(key1, key2, testfun) \
  (EQ (key1, key2) || ((testfun) && (testfun) (key1, key2)))

#define LINEAR_PROBING_LOOP(probe, entries, size)		\
  for (;							\
       !HENTRY_CLEAR_P (probe) ||				\
	 (probe == entries + size ?				\
	  (probe = entries, !HENTRY_CLEAR_P (probe)) : 0);	\
       probe++)

#ifndef ERROR_CHECK_HASH_TABLE
# ifdef ERROR_CHECK_TYPECHECK
#  define ERROR_CHECK_HASH_TABLE 1
# else
#  define ERROR_CHECK_HASH_TABLE 0
# endif
#endif

#if ERROR_CHECK_HASH_TABLE
static void
check_hash_table_invariants(hash_table_t ht)
{
	assert(ht->count < ht->size);
	assert(ht->count <= ht->rehash_count);
	assert(ht->rehash_count < ht->size);
	assert((fpfloat)ht->count * ht->rehash_threshold - 1 <=
	       (fpfloat)ht->rehash_count);
	assert(HENTRY_CLEAR_P(ht->hentries + ht->size));
}
#else
#define check_hash_table_invariants(ht)
#endif

/* We use linear probing instead of double hashing, despite its lack
   of blessing by Knuth and company, because, as a result of the
   increasing discrepancy between CPU speeds and memory speeds, cache
   behavior is becoming increasingly important, e.g:

   For a trivial loop, the penalty for non-sequential access of an array is:
    - a factor of 3-4 on Pentium Pro 200 Mhz
    - a factor of 10  on Ultrasparc  300 Mhz */

/* Return a suitable size for a hash table, with at least SIZE slots. */
static size_t
hash_table_size(size_t requested_size)
{
	/* Return some prime near, but greater than or equal to, SIZE.
	   Decades from the time of writing, someone will have a system large
	   enough that the list below will be too short... */
	static const size_t primes[] = {
		29, 41, 59, 79, 107, 149, 197, 263, 347, 457, 599, 787, 1031,
		1361, 1777, 2333, 3037, 3967, 5167, 6719, 8737, 11369, 14783,
		19219, 24989, 32491, 42257, 54941, 71429, 92861, 120721, 156941,
		204047, 265271, 344857, 448321, 582821, 757693, 985003, 1280519,
		1664681, 2164111, 2813353, 3657361, 4754591, 6180989, 8035301,
		10445899, 13579681, 17653589, 22949669, 29834603, 38784989,
		50420551, 65546729, 85210757, 110774011, 144006217, 187208107,
		243370577, 316381771, 411296309, 534685237, 695090819,
		    903618083,
		1174703521, 1527114613, 1985248999, 2580823717UL, 3355070839UL
	};
	/* We've heard of binary search. */
	int low, high;
	for (low = 0, high = countof(primes) - 1; high - low > 1;) {
		/* Loop Invariant: size < primes [high] */
		int mid = (low + high) / 2;
		if (primes[mid] < requested_size)
			low = mid;
		else
			high = mid;
	}
	return primes[high];
}

static int lisp_object_eql_equal(Lisp_Object obj1, Lisp_Object obj2)
{
	return EQ(obj1, obj2) || (FLOATP(obj1)
				  && internal_equal(obj1, obj2, 0));
}

static hcode_t lisp_object_eql_hash(Lisp_Object obj)
{
	return FLOATP(obj) ? internal_hash(obj, 0) : LISP_HASH(obj);
}

static int lisp_object_equal_equal(Lisp_Object obj1, Lisp_Object obj2)
{
	return internal_equal(obj1, obj2, 0);
}

static hcode_t lisp_object_equal_hash(Lisp_Object obj)
{
	return internal_hash(obj, 0);
}

static Lisp_Object mark_hash_table(Lisp_Object obj)
{
	hash_table_t ht = XHASH_TABLE(obj);

	/* If the hash table is weak, we don't want to mark the keys and
	   values (we scan over them after everything else has been marked,
	   and mark or remove them as necessary).  */
	if (ht->weakness == HASH_TABLE_NON_WEAK) {
		for (hentry_t e = ht->hentries, sentinel = e + ht->size;
		     e < sentinel; e++) {
			if (!HENTRY_CLEAR_P(e)) {
				mark_object(e->key);
				mark_object(e->value);
			}
		}
	}
	return Qnil;
}

/* Equality of hash tables.  Two hash tables are equal when they are of
   the same weakness and test function, they have the same number of
   elements, and for each key in the hash table, the values are `equal'.

   This is similar to Common Lisp `equalp' of hash tables, with the
   difference that CL requires the keys to be compared with the test
   function, which we don't do.  Doing that would require consing, and
   consing is a bad idea in `equal'.  Anyway, our method should provide
   the same result -- if the keys are not equal according to the test
   function, then Fgethash() in hash_table_equal_mapper() will fail.  */
static int
hash_table_equal(Lisp_Object hash_table1, Lisp_Object hash_table2, int depth)
{
	hash_table_t ht1 = XHASH_TABLE(hash_table1);
	hash_table_t ht2 = XHASH_TABLE(hash_table2);

	if ((ht1->test_function != ht2->test_function) ||
	    (ht1->weakness != ht2->weakness) || (ht1->count != ht2->count)) {
		return 0;
	}
	depth++;

	for (hentry_t e = ht1->hentries, sntl = e + ht1->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			/* Look up the key in the other hash table, and compare
			   the values. */
			Lisp_Object value_in_other =
				Fgethash(e->key, hash_table2, Qunbound);
			if (UNBOUNDP(value_in_other) ||
			    !internal_equal(e->value, value_in_other, depth)) {
				return 0;	/* Give up */
			}
		}
	}
	return 1;
}

/* This is not a great hash function, but it _is_ correct and fast.
   Examining all entries is too expensive, and examining a random
   subset does not yield a correct hash function. */
static hcode_t hash_table_hash(Lisp_Object hash_table, int depth)
{
	return XHASH_TABLE(hash_table)->count;
}

/* Printing hash tables.

   This is non-trivial, because we use a readable structure-style
   syntax for hash tables.  This means that a typical hash table will be
   readably printed in the form of:

   #s(hash-table size 2 data (key1 value1 key2 value2))

   The supported hash table structure keywords and their values are:
   `test'             (eql (or nil), eq or equal)
   `size'             (a natnum or nil)
   `rehash-size'      (a float)
   `rehash-threshold' (a float)
   `weakness'         (nil, key, value, key-and-value, or key-or-value)
   `data'             (a list)

   If `print-readably' is nil, then a simpler syntax is used, for example

   #<hash-table size 2/13 data (key1 value1 key2 value2) 0x874d>

   The data is truncated to four pairs, and the rest is shown with
   `...'.  This printer does not cons.  */

/* Print the data of the hash table.  This maps through a Lisp
   hash table and prints key/value pairs using PRINTCHARFUN.  */
static void
print_hash_table_data(hash_table_t  ht, Lisp_Object printcharfun)
{
	int count = 0;

	write_c_string(" data (", printcharfun);

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			if (count > 0)
				write_c_string(" ", printcharfun);
			if (!print_readably && count > 3) {
				write_c_string("...", printcharfun);
				break;
			}
			print_internal(e->key, printcharfun, 1);
			write_c_string(" ", printcharfun);
			print_internal(e->value, printcharfun, 1);
			count++;
		}
	}
	write_c_string(")", printcharfun);
}

static void
print_hash_table(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	hash_table_t ht = XHASH_TABLE(obj);

	write_c_string(print_readably ? "#s(hash-table" : "#<hash-table",
		       printcharfun);

	/* These checks have a kludgy look to them, but they are safe.
	   Due to nature of hashing, you cannot use arbitrary
	   test functions anyway.  */
	if (!ht->test_function)
		write_c_string(" test eq", printcharfun);
	else if (ht->test_function == lisp_object_equal_equal)
		write_c_string(" test equal", printcharfun);
	else if (ht->test_function == lisp_object_eql_equal)
		DO_NOTHING;
	else
		abort();

	if (ht->count || !print_readably) {
		if (print_readably)
			write_fmt_str(printcharfun, " size %lu", (unsigned long)ht->count);
		else
			write_fmt_str(printcharfun, " size %lu/%lu",
				      (unsigned long)ht->count,
				      (unsigned long)ht->size);
	}

	if (ht->weakness != HASH_TABLE_NON_WEAK) {
		write_fmt_str(printcharfun, " weakness %s",
			      (ht->weakness == HASH_TABLE_WEAK ? "key-and-value" :
			       ht->weakness == HASH_TABLE_KEY_WEAK ? "key" :
			       ht->weakness == HASH_TABLE_VALUE_WEAK ? "value" :
			       ht->weakness ==
			       HASH_TABLE_KEY_VALUE_WEAK ? "key-or-value" :
			       "you-d-better-not-see-this"));
	}

	if (ht->count)
		print_hash_table_data(ht, printcharfun);

	if (print_readably)
		write_c_string(")", printcharfun);
	else
		write_fmt_str(printcharfun, " 0x%x>", ht->header.uid);
}

static void finalize_hash_table(void *header, int for_disksave)
{
	if (!for_disksave) {
		hash_table_t ht = (hash_table_t ) header;

		xfree(ht->hentries);
		ht->hentries = 0;
	}
}

static const struct lrecord_description hentry_description_1[] = {
	{XD_LISP_OBJECT, offsetof(struct hentry_s, key)},
	{XD_LISP_OBJECT, offsetof(struct hentry_s, value)},
	{XD_END}
};

static const struct struct_description hentry_description = {
	sizeof(struct hentry_s),
	hentry_description_1
};

const struct lrecord_description hash_table_description[] = {
	{XD_SIZE_T, offsetof(struct hash_table_s, size)},
	{XD_STRUCT_PTR, offsetof(struct hash_table_s, hentries),
	 XD_INDIRECT(0, 1), &hentry_description},
	{XD_LO_LINK, offsetof(struct hash_table_s, next_weak)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION(
	"hash-table", hash_table,
	mark_hash_table, print_hash_table, finalize_hash_table,
	hash_table_equal, hash_table_hash, hash_table_description,
	struct hash_table_s);

static hash_table_t
xhash_table(Lisp_Object hash_table)
{
	if (!gc_in_progress)
		CHECK_HASH_TABLE(hash_table);
	check_hash_table_invariants(XHASH_TABLE(hash_table));
	return XHASH_TABLE(hash_table);
}

/************************************************************************/
/*			 Creation of Hash Tables			*/
/************************************************************************/

/* Creation of hash tables, without error-checking. */
static void compute_hash_table_derived_values(hash_table_t  ht)
{
	ht->rehash_count = (size_t)
	    ((fpfloat)ht->size * ht->rehash_threshold);
	ht->golden_ratio = (size_t)
	    ((fpfloat)ht->size * (.6180339887 / (fpfloat)sizeof(Lisp_Object)));
}

Lisp_Object
make_standard_lisp_hash_table(
	enum hash_table_test test,
	size_t size, fpfloat rehash_size, fpfloat rehash_threshold,
	enum hash_table_weakness weakness)
{
	hash_table_hash_f hash_function = 0;
	hash_table_test_f test_function = 0;

	switch (test) {
	case HASH_TABLE_EQ:
		test_function = 0;
		hash_function = 0;
		break;

	case HASH_TABLE_EQL:
		test_function = lisp_object_eql_equal;
		hash_function = lisp_object_eql_hash;
		break;

	case HASH_TABLE_EQUAL:
		test_function = lisp_object_equal_equal;
		hash_function = lisp_object_equal_hash;
		break;

	default:
		abort();
	}

	return make_general_lisp_hash_table(hash_function, test_function,
					    size, rehash_size, rehash_threshold,
					    weakness);
}

Lisp_Object
make_general_lisp_hash_table(
	hash_table_hash_f hash_function, hash_table_test_f test_function,
	size_t size, fpfloat rehash_size, fpfloat rehash_threshold,
	enum hash_table_weakness weakness)
{
	Lisp_Object hash_table;
	hash_table_t ht = alloc_lcrecord_type(
		struct hash_table_s, &lrecord_hash_table);

	/* the categories are actually seq and dict, but use the per-type
	   implementation for a start */
	ht->header.lheader.morphisms = (1<<cat_mk_lc);

	ht->test_function = test_function;
	ht->hash_function = hash_function;
	ht->weakness = weakness;

	ht->rehash_size = rehash_size > 1.0
		? rehash_size
		: HASH_TABLE_DEFAULT_REHASH_SIZE;

	ht->rehash_threshold = rehash_threshold > 0.0
		? rehash_threshold
		: size > 4096 && !ht->test_function ? 0.7 : 0.6;

	if (size < HASH_TABLE_MIN_SIZE)
		size = HASH_TABLE_MIN_SIZE;
	ht->size = hash_table_size(
		(size_t)(((fpfloat)size / ht->rehash_threshold) + 1.0));
	ht->count = 0;

	compute_hash_table_derived_values(ht);

	/* We leave room for one never-occupied sentinel hentry at the end. */
	ht->hentries = xnew_array_and_zero(struct hentry_s, ht->size + 1);

	XSETHASH_TABLE(hash_table, ht);

	if (weakness == HASH_TABLE_NON_WEAK) {
		ht->next_weak = Qunbound;
	} else {
		ht->next_weak = Vall_weak_hash_tables,
			Vall_weak_hash_tables = hash_table;
	}
	return hash_table;
}

Lisp_Object
make_lisp_hash_table(size_t size,
		     enum hash_table_weakness weakness,
		     enum hash_table_test test)
{
	return make_standard_lisp_hash_table(test, size, -1.0, -1.0, weakness);
}

/* Pretty reading of hash tables.

   Here we use the existing structures mechanism (which is,
   unfortunately, pretty cumbersome) for validating and instantiating
   the hash tables.  The idea is that the side-effect of reading a
   #s(hash-table PLIST) object is creation of a hash table with desired
   properties, and that the hash table is returned.  */

/* Validation functions: each keyword provides its own validation
   function.  The errors should maybe be continuable, but it is
   unclear how this would cope with ERRB.  */
static int
hash_table_size_validate(Lisp_Object keyword, Lisp_Object value,
			 Error_behavior errb)
{
#ifdef WITH_NUMBER_TYPES
	if (!NILP(Fnonnegativep(value)))
		return 1;

	maybe_signal_error(Qwrong_type_argument, list2(Qnonnegativep, value),
			   Qhash_table, errb);
#else  /* !WITH_NUMBER_TYPES */
	if (NATNUMP(value))
		return 1;

	maybe_signal_error(Qwrong_type_argument, list2(Qnatnump, value),
			   Qhash_table, errb);
#endif	/* WITH_NUMBER_TYPES */
	return 0;
}

static size_t decode_hash_table_size(Lisp_Object obj)
{
#ifdef WITH_NUMBER_TYPES
	return NILP(obj) ? HASH_TABLE_DEFAULT_SIZE :
		XINT(Fcoerce_number(obj, Qint, Qnil));
#else
	return NILP(obj) ? HASH_TABLE_DEFAULT_SIZE : XINT(obj);
#endif
}

static int
hash_table_weakness_validate(Lisp_Object keyword, Lisp_Object value,
			     Error_behavior errb)
{
	if (EQ(value, Qnil))
		return 1;
	if (EQ(value, Qt))
		return 1;
	if (EQ(value, Qkey))
		return 1;
	if (EQ(value, Qkey_and_value))
		return 1;
	if (EQ(value, Qkey_or_value))
		return 1;
	if (EQ(value, Qvalue))
		return 1;

	/* Following values are obsolete as of 19990901 in xemacs-21.2 */
	if (EQ(value, Qnon_weak))
		return 1;
	if (EQ(value, Qweak))
		return 1;
	if (EQ(value, Qkey_weak))
		return 1;
	if (EQ(value, Qkey_or_value_weak))
		return 1;
	if (EQ(value, Qvalue_weak))
		return 1;

	maybe_signal_simple_error("Invalid hash table weakness",
				  value, Qhash_table, errb);
	return 0;
}

static enum hash_table_weakness decode_hash_table_weakness(Lisp_Object obj)
{
	if (EQ(obj, Qnil))
		return HASH_TABLE_NON_WEAK;
	if (EQ(obj, Qt))
		return HASH_TABLE_WEAK;
	if (EQ(obj, Qkey_and_value))
		return HASH_TABLE_WEAK;
	if (EQ(obj, Qkey))
		return HASH_TABLE_KEY_WEAK;
	if (EQ(obj, Qkey_or_value))
		return HASH_TABLE_KEY_VALUE_WEAK;
	if (EQ(obj, Qvalue))
		return HASH_TABLE_VALUE_WEAK;

	/* Following values are obsolete as of 19990901 in xemacs-21.2 */
	if (EQ(obj, Qnon_weak))
		return HASH_TABLE_NON_WEAK;
	if (EQ(obj, Qweak))
		return HASH_TABLE_WEAK;
	if (EQ(obj, Qkey_weak))
		return HASH_TABLE_KEY_WEAK;
	if (EQ(obj, Qkey_or_value_weak))
		return HASH_TABLE_KEY_VALUE_WEAK;
	if (EQ(obj, Qvalue_weak))
		return HASH_TABLE_VALUE_WEAK;

	signal_simple_error("Invalid hash table weakness", obj);
	return HASH_TABLE_NON_WEAK;	/* not reached */
}

static int
hash_table_test_validate(Lisp_Object keyword, Lisp_Object value,
			 Error_behavior errb)
{
	if (EQ(value, Qnil))
		return 1;
	if (EQ(value, Qeq))
		return 1;
	if (EQ(value, Qequal))
		return 1;
	if (EQ(value, Qeql))
		return 1;

	maybe_signal_simple_error("Invalid hash table test",
				  value, Qhash_table, errb);
	return 0;
}

static enum hash_table_test decode_hash_table_test(Lisp_Object obj)
{
	if (EQ(obj, Qnil))
		return HASH_TABLE_EQL;
	if (EQ(obj, Qeq))
		return HASH_TABLE_EQ;
	if (EQ(obj, Qequal))
		return HASH_TABLE_EQUAL;
	if (EQ(obj, Qeql))
		return HASH_TABLE_EQL;

	signal_simple_error("Invalid hash table test", obj);
	return HASH_TABLE_EQ;	/* not reached */
}

static int
hash_table_rehash_size_validate(Lisp_Object keyword, Lisp_Object value,
				Error_behavior errb)
{
	if (!FLOATP(value)) {
		maybe_signal_error(Qwrong_type_argument, list2(Qfloatp, value),
				   Qhash_table, errb);
		return 0;
	}

	{
		fpfloat rehash_size = XFLOAT_DATA(value);
		if (rehash_size <= 1.0) {
			maybe_signal_simple_error
			    ("Hash table rehash size must be greater than 1.0",
			     value, Qhash_table, errb);
			return 0;
		}
	}

	return 1;
}

static fpfloat decode_hash_table_rehash_size(Lisp_Object rehash_size)
{
	return NILP(rehash_size) ? -1.0 : XFLOAT_DATA(rehash_size);
}

static int
hash_table_rehash_threshold_validate(Lisp_Object keyword, Lisp_Object value,
				     Error_behavior errb)
{
	if (!FLOATP(value)) {
		maybe_signal_error(Qwrong_type_argument, list2(Qfloatp, value),
				   Qhash_table, errb);
		return 0;
	}

	{
		fpfloat rehash_threshold = XFLOAT_DATA(value);
		if (rehash_threshold <= 0.0 || rehash_threshold >= 1.0) {
			maybe_signal_simple_error
			    ("Hash table rehash threshold must be between 0.0 and 1.0",
			     value, Qhash_table, errb);
			return 0;
		}
	}

	return 1;
}

static fpfloat decode_hash_table_rehash_threshold(Lisp_Object rehash_threshold)
{
	return NILP(rehash_threshold) ? -1.0 : XFLOAT_DATA(rehash_threshold);
}

static int
hash_table_data_validate(Lisp_Object keyword, Lisp_Object value,
			 Error_behavior errb)
{
	int len;

	GET_EXTERNAL_LIST_LENGTH(value, len);

	if (len & 1) {
		maybe_signal_simple_error
		    ("Hash table data must have alternating key/value pairs",
		     value, Qhash_table, errb);
		return 0;
	}
	return 1;
}

/* The actual instantiation of a hash table.  This does practically no
   error checking, because it relies on the fact that the paranoid
   functions above have error-checked everything to the last details.
   If this assumption is wrong, we will get a crash immediately (with
   error-checking compiled in), and we'll know if there is a bug in
   the structure mechanism.  So there.  */
static Lisp_Object hash_table_instantiate(Lisp_Object plist)
{
	Lisp_Object hash_table;
	Lisp_Object test = Qnil;
	Lisp_Object size = Qnil;
	Lisp_Object rehash_size = Qnil;
	Lisp_Object rehash_threshold = Qnil;
	Lisp_Object weakness = Qnil;
	Lisp_Object data = Qnil;

	while (!NILP(plist)) {
		Lisp_Object key, value;
		key = XCAR(plist);
		plist = XCDR(plist);
		value = XCAR(plist);
		plist = XCDR(plist);

		if (EQ(key, Qtest))
			test = value;
		else if (EQ(key, Qsize))
			size = value;
		else if (EQ(key, Qrehash_size))
			rehash_size = value;
		else if (EQ(key, Qrehash_threshold))
			rehash_threshold = value;
		else if (EQ(key, Qweakness))
			weakness = value;
		else if (EQ(key, Qdata))
			data = value;
		else if (EQ(key, Qtype))	/*obsolete */
			weakness = value;
		else
			abort();
	}

	/* Create the hash table.  */
	hash_table = make_standard_lisp_hash_table
	    (decode_hash_table_test(test),
	     decode_hash_table_size(size),
	     decode_hash_table_rehash_size(rehash_size),
	     decode_hash_table_rehash_threshold(rehash_threshold),
	     decode_hash_table_weakness(weakness));

	/* I'm not sure whether this can GC, but better safe than sorry.  */
	{
		struct gcpro gcpro1;
		GCPRO1(hash_table);

		/* And fill it with data.  */
		while (!NILP(data)) {
			Lisp_Object key, value;
			key = XCAR(data);
			data = XCDR(data);
			value = XCAR(data);
			data = XCDR(data);
			Fputhash(key, value, hash_table);
		}
		UNGCPRO;
	}

	return hash_table;
}

static void
structure_type_create_hash_table_structure_name(Lisp_Object structure_name)
{
	struct structure_type *st;

	st = define_structure_type(structure_name, 0, hash_table_instantiate);
	define_structure_type_keyword(st, Qtest, hash_table_test_validate);
	define_structure_type_keyword(st, Qsize, hash_table_size_validate);
	define_structure_type_keyword(st, Qrehash_size,
				      hash_table_rehash_size_validate);
	define_structure_type_keyword(st, Qrehash_threshold,
				      hash_table_rehash_threshold_validate);
	define_structure_type_keyword(st, Qweakness,
				      hash_table_weakness_validate);
	define_structure_type_keyword(st, Qdata, hash_table_data_validate);

	/* obsolete as of 19990901 in xemacs-21.2 */
	define_structure_type_keyword(st, Qtype, hash_table_weakness_validate);
}

/* Create a built-in Lisp structure type named `hash-table'.
   We make #s(hashtable ...) equivalent to #s(hash-table ...),
   for backward compatibility.
   This is called from emacs.c.  */
void structure_type_create_hash_table(void)
{
	structure_type_create_hash_table_structure_name(Qhash_table);
	structure_type_create_hash_table_structure_name(Qhashtable);	/* compat */
}

/************************************************************************/
/*		Definition of Lisp-visible methods			*/
/************************************************************************/

DEFUN("hash-table-p", Fhash_table_p, 1, 1, 0,	/*
Return t if OBJECT is a hash table, else nil.
*/
      (object))
{
	return HASH_TABLEP(object) ? Qt : Qnil;
}

DEFUN("make-hash-table", Fmake_hash_table, 0, MANY, 0,	/*
Return a new empty hash table object.
Use Common Lisp style keywords to specify hash table properties.
(make-hash-table &key test size rehash-size rehash-threshold weakness)

Keyword :test can be `eq', `eql' (default) or `equal'.
Comparison between keys is done using this function.
If speed is important, consider using `eq'.
When hash table keys may be strings, you will likely need to use `equal'.

Keyword :size specifies the number of keys likely to be inserted.
This number of entries can be inserted without enlarging the hash table.

Keyword :rehash-size must be a float greater than 1.0, and specifies
the factor by which to increase the size of the hash table when enlarging.

Keyword :rehash-threshold must be a float between 0.0 and 1.0,
and specifies the load factor of the hash table which triggers enlarging.

Non-standard keyword :weakness can be `nil' (default), `t', `key-and-value',
`key', `value' or `key-or-value'. `t' is an alias for `key-and-value'.

A key-and-value-weak hash table, also known as a fully-weak or simply
as a weak hash table, is one whose pointers do not count as GC
referents: for any key-value pair in the hash table, if the only
remaining pointer to either the key or the value is in a weak hash
table, then the pair will be removed from the hash table, and the key
and value collected.  A non-weak hash table (or any other pointer)
would prevent the object from being collected.

A key-weak hash table is similar to a fully-weak hash table except that
a key-value pair will be removed only if the key remains unmarked
outside of weak hash tables.  The pair will remain in the hash table if
the key is pointed to by something other than a weak hash table, even
if the value is not.

A value-weak hash table is similar to a fully-weak hash table except
that a key-value pair will be removed only if the value remains
unmarked outside of weak hash tables.  The pair will remain in the
hash table if the value is pointed to by something other than a weak
hash table, even if the key is not.

A key-or-value-weak hash table is similar to a fully-weak hash table except
that a key-value pair will be removed only if the value and the key remain
unmarked outside of weak hash tables.  The pair will remain in the
hash table if the value or key are pointed to by something other than a weak
hash table, even if the other is not.
*/
      (int nargs, Lisp_Object * args))
{
	int i = 0;
	Lisp_Object test = Qnil;
	Lisp_Object size = Qnil;
	Lisp_Object rehash_size = Qnil;
	Lisp_Object rehash_threshold = Qnil;
	Lisp_Object weakness = Qnil;

	while (i + 1 < nargs) {
		Lisp_Object keyword = args[i++];
		Lisp_Object value = args[i++];

		if (EQ(keyword, Q_test))
			test = value;
		else if (EQ(keyword, Q_size))
			size = value;
		else if (EQ(keyword, Q_rehash_size))
			rehash_size = value;
		else if (EQ(keyword, Q_rehash_threshold))
			rehash_threshold = value;
		else if (EQ(keyword, Q_weakness))
			weakness = value;
		else if (EQ(keyword, Q_type))	/*obsolete */
			weakness = value;
		else
			signal_simple_error
			    ("Invalid hash table property keyword", keyword);
	}

	if (i < nargs)
		signal_simple_error("Hash table property requires a value",
				    args[i]);

#define VALIDATE_VAR(var) \
if (!NILP (var)) hash_table_##var##_validate (Q##var, var, ERROR_ME);

	VALIDATE_VAR(test);
	VALIDATE_VAR(size);
	VALIDATE_VAR(rehash_size);
	VALIDATE_VAR(rehash_threshold);
	VALIDATE_VAR(weakness);

	return make_standard_lisp_hash_table
	    (decode_hash_table_test(test),
	     decode_hash_table_size(size),
	     decode_hash_table_rehash_size(rehash_size),
	     decode_hash_table_rehash_threshold(rehash_threshold),
	     decode_hash_table_weakness(weakness));
}

DEFUN("copy-hash-table", Fcopy_hash_table, 1, 1, 0,	/*
Return a new hash table containing the same keys and values as HASH-TABLE.
The keys and values will not themselves be copied.
*/
      (hash_table))
{
	const hash_table_t ht_old = xhash_table(hash_table);
	hash_table_t ht = alloc_lcrecord_type(
		struct hash_table_s, &lrecord_hash_table);

	copy_lcrecord(ht, ht_old);

	ht->hentries = xnew_array(struct hentry_s, ht_old->size + 1);
	memcpy(ht->hentries, ht_old->hentries,
	       (ht_old->size + 1) * sizeof(struct hentry_s));

	/* the categories are actually seq and dict, but use the per-type
	   implementation for a start */
	ht->header.lheader.morphisms = (1<<cat_mk_lc);

	XSETHASH_TABLE(hash_table, ht);

	if (!EQ(ht->next_weak, Qunbound)) {
		ht->next_weak = Vall_weak_hash_tables,
			Vall_weak_hash_tables = hash_table;
	}
	return hash_table;
}

static void
resize_hash_table(hash_table_t  ht, size_t new_size)
{
	hentry_t old_entries, new_entries;
	size_t old_size;

	old_size = ht->size;
	ht->size = new_size;

	old_entries = ht->hentries;

	ht->hentries = xnew_array_and_zero(struct hentry_s, new_size + 1);
	new_entries = ht->hentries;

	compute_hash_table_derived_values(ht);

	for (hentry_t e = old_entries, sentinel = e + old_size;
	     e < sentinel; e++)
		if (!HENTRY_CLEAR_P(e)) {
			hentry_t probe = new_entries + HASH_CODE(e->key, ht);
			LINEAR_PROBING_LOOP(probe, new_entries, new_size);
			*probe = *e;
		}

	if (!DUMPEDP(old_entries)) {
		xfree(old_entries);
	}
}

/* After a hash table has been saved to disk and later restored by the
   portable dumper, it contains the same objects, but their addresses
   and thus their HASH_CODEs have changed. */
void
pdump_reorganize_hash_table(Lisp_Object hash_table)
{
	const hash_table_t ht = xhash_table(hash_table);
	hentry_t new_entries =
		xnew_array_and_zero(struct hentry_s, ht->size + 1);

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			hentry_t probe = new_entries + HASH_CODE(e->key, ht);
			LINEAR_PROBING_LOOP(probe, new_entries, ht->size);
			*probe = *e;
		}
	}
	memcpy(ht->hentries, new_entries, ht->size * sizeof(struct hentry_s));

	xfree(new_entries);
	return;
}

static void enlarge_hash_table(hash_table_t  ht)
{
	size_t new_size =
	    hash_table_size((size_t) ((fpfloat)ht->size * ht->rehash_size));
	resize_hash_table(ht, new_size);
}

static hentry_t
find_hentry(Lisp_Object key, const hash_table_t  ht)
{
	hash_table_test_f test_function = ht->test_function;
	hentry_t entries = ht->hentries;
	hentry_t probe = entries + HASH_CODE(key, ht);

	LINEAR_PROBING_LOOP(probe, entries, ht->size)
	    if (KEYS_EQUAL_P(probe->key, key, test_function))
		break;

	return probe;
}

static inline Lisp_Object
dict_ht_get(hash_table_t ht, Lisp_Object key, Lisp_Object _default)
{
	const hentry_t e = find_hentry(key, ht);

	return HENTRY_CLEAR_P(e) ? _default : e->value;
}

DEFUN("gethash", Fgethash, 2, 3, 0,	/*
Find hash value for KEY in HASH-TABLE.
If there is no corresponding value, return DEFAULT (which defaults to nil).
*/
      (key, hash_table, default_))
{
	return dict_ht_get(xhash_table(hash_table), key, default_);
}

static inline Lisp_Object
dict_ht_put(hash_table_t ht, Lisp_Object key, Lisp_Object value)
{
	hentry_t e = find_hentry(key, ht);

	if (!HENTRY_CLEAR_P(e))
		return e->value = value;

	e->key = key;
	e->value = value;

	if (++ht->count >= ht->rehash_count) {
		enlarge_hash_table(ht);
	}

	return value;
}

DEFUN("puthash", Fputhash, 3, 3, 0,	/*
Hash KEY to VALUE in HASH-TABLE.
*/
      (key, value, hash_table))
{
	return dict_ht_put(xhash_table(hash_table), key, value);
}

/* Remove hentry pointed at by PROBE.
   Subsequent entries are removed and reinserted.
   We don't use tombstones - too wasteful.  */
static void remhash_1(hash_table_t  ht, hentry_t  entries, hentry_t  probe)
{
	size_t size = ht->size;
	CLEAR_HENTRY(probe);
	probe++;
	ht->count--;

	LINEAR_PROBING_LOOP(probe, entries, size) {
		Lisp_Object key = probe->key;
		hentry_t probe2 = entries + HASH_CODE(key, ht);
		LINEAR_PROBING_LOOP(probe2, entries, size)
		    if (EQ(probe2->key, key))
			/* hentry at probe doesn't need to move. */
			goto continue_outer_loop;
		/* Move hentry from probe to new home at probe2. */
		*probe2 = *probe;
		CLEAR_HENTRY(probe);
	      continue_outer_loop:continue;
	}
}

static inline Lisp_Object
dict_ht_remove(hash_table_t ht, Lisp_Object key)
{
	hentry_t e = find_hentry(key, ht);

	if (HENTRY_CLEAR_P(e)) {
		return Qnil;
	}

	remhash_1(ht, ht->hentries, e);
	return Qt;
}

DEFUN("remhash", Fremhash, 2, 2, 0,	/*
Remove the entry for KEY from HASH-TABLE.
Do nothing if there is no entry for KEY in HASH-TABLE.
*/
      (key, hash_table))
{
	return dict_ht_remove(xhash_table(hash_table), key);
}

DEFUN("clrhash", Fclrhash, 1, 1, 0,	/*
Remove all entries from HASH-TABLE, leaving it empty.
*/
      (hash_table))
{
	hash_table_t ht = xhash_table(hash_table);

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		CLEAR_HENTRY(e);
	}
	ht->count = 0;

	return hash_table;
}

/************************************************************************/
/*			    Accessor Functions				*/
/************************************************************************/

DEFUN("hash-table-count", Fhash_table_count, 1, 1, 0,	/*
Return the number of entries in HASH-TABLE.
*/
      (hash_table))
{
	return make_int(xhash_table(hash_table)->count);
}

DEFUN("hash-table-test", Fhash_table_test, 1, 1, 0,	/*
Return the test function of HASH-TABLE.
This can be one of `eq', `eql' or `equal'.
*/
      (hash_table))
{
	hash_table_test_f fun = xhash_table(hash_table)->test_function;

	return (fun == lisp_object_eql_equal ? Qeql :
		fun == lisp_object_equal_equal ? Qequal : Qeq);
}

static size_t
dict_ht_size(const hash_table_t ht)
{
	return ht->count;
}

DEFUN("hash-table-size", Fhash_table_size, 1, 1, 0,	/*
Return the size of HASH-TABLE.
This is the current number of slots in HASH-TABLE, whether occupied or not.
*/
      (hash_table))
{
	return make_int(xhash_table(hash_table)->size);
}

DEFUN("hash-table-rehash-size", Fhash_table_rehash_size, 1, 1, 0,	/*
Return the current rehash size of HASH-TABLE.
This is a float greater than 1.0; the factor by which HASH-TABLE
is enlarged when the rehash threshold is exceeded.
*/
      (hash_table))
{
	return make_float(xhash_table(hash_table)->rehash_size);
}

DEFUN("hash-table-rehash-threshold", Fhash_table_rehash_threshold, 1, 1, 0,	/*
Return the current rehash threshold of HASH-TABLE.
This is a float between 0.0 and 1.0; the maximum `load factor' of HASH-TABLE,
beyond which the HASH-TABLE is enlarged by rehashing.
*/
      (hash_table))
{
	return make_float(xhash_table(hash_table)->rehash_threshold);
}

DEFUN("hash-table-weakness", Fhash_table_weakness, 1, 1, 0,	/*
Return the weakness of HASH-TABLE.
This can be one of `nil', `key-and-value', `key-or-value', `key' or `value'.
*/
      (hash_table))
{
	switch (xhash_table(hash_table)->weakness) {
	case HASH_TABLE_WEAK:
		return Qkey_and_value;
	case HASH_TABLE_KEY_WEAK:
		return Qkey;
	case HASH_TABLE_KEY_VALUE_WEAK:
		return Qkey_or_value;
	case HASH_TABLE_VALUE_WEAK:
		return Qvalue;

		/* all the rest */
	case HASH_TABLE_NON_WEAK:
	case HASH_TABLE_KEY_CAR_WEAK:
	case HASH_TABLE_VALUE_CAR_WEAK:
	case HASH_TABLE_KEY_CAR_VALUE_WEAK:

	default:
		return Qnil;
	}
}

/* obsolete as of 19990901 in xemacs-21.2 */
DEFUN("hash-table-type", Fhash_table_type, 1, 1, 0,	/*
Return the type of HASH-TABLE.
This can be one of `non-weak', `weak', `key-weak' or `value-weak'.
*/
      (hash_table))
{
	switch (xhash_table(hash_table)->weakness) {
	case HASH_TABLE_WEAK:
		return Qweak;
	case HASH_TABLE_KEY_WEAK:
		return Qkey_weak;
	case HASH_TABLE_KEY_VALUE_WEAK:
		return Qkey_or_value_weak;
	case HASH_TABLE_VALUE_WEAK:
		return Qvalue_weak;

		/* the bloody rest */
	case HASH_TABLE_NON_WEAK:
	case HASH_TABLE_KEY_CAR_WEAK:
	case HASH_TABLE_VALUE_CAR_WEAK:
	case HASH_TABLE_KEY_CAR_VALUE_WEAK:

	default:
		return Qnon_weak;
	}
}

/************************************************************************/
/*			    Mapping Functions				*/
/************************************************************************/
DEFUN("maphash", Fmaphash, 2, 2, 0,	/*
Map FUNCTION over entries in HASH-TABLE, calling it with two args,
each key and value in HASH-TABLE.

FUNCTION may not modify HASH-TABLE, with the one exception that FUNCTION
may remhash or puthash the entry currently being processed by FUNCTION.
*/
      (function, hash_table))
{
	const hash_table_t ht = xhash_table(hash_table);

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			Lisp_Object args[3], key;
		      again:
			key = e->key;
			args[0] = function;
			args[1] = key;
			args[2] = e->value;
			Ffuncall(countof(args), args);
			/* Has FUNCTION done a remhash? */
			if (!EQ(key, e->key) && !HENTRY_CLEAR_P(e))
				goto again;
		}
	}
	return Qnil;
}

/* #### If the Lisp function being called does a puthash and this
   #### causes the hash table to be resized, the results will be quite
   #### random and we will likely crash.  To fix this, either set a
   #### flag in the hash table while we're mapping and signal an error
   #### when new entries are added, or fix things to make this
   #### operation work properly, like this: Store two hash tables in
   #### each hash table object -- the second one is written to when
   #### you do a puthash inside of a mapping operation, and the
   #### various operations need to check both hash tables for entries.
   #### As soon as the last maphash over a particular hash table
   #### object terminates, the entries in the second table are added
   #### to the first (using an unwind-protect). --ben */

/* Map *C* function FUNCTION over the elements of a lisp hash table. */
void
elisp_maphash(maphash_f function, Lisp_Object hash_table, void *extra_arg)
{
	const hash_table_t ht = XHASH_TABLE(hash_table);

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			Lisp_Object key;
		again:
			key = e->key;
			if (function(key, e->value, extra_arg)) {
				return;
			}
			/* Has FUNCTION done a remhash? */
			if (!EQ(key, e->key) && !HENTRY_CLEAR_P(e)) {
				goto again;
			}
		}
	}
}

/* Remove all elements of a lisp hash table satisfying *C* predicate
   PREDICATE. */
void
elisp_map_remhash(maphash_f predicate, Lisp_Object hash_table, void *extra_arg)
{
	hash_table_t ht = XHASH_TABLE(hash_table);
	hentry_t entries;

	for (hentry_t e = entries = ht->hentries, sentinel = e + ht->size;
	     e < sentinel; e++) {
		if (!HENTRY_CLEAR_P(e)) {
		again:
			if (predicate(e->key, e->value, extra_arg)) {
				remhash_1(ht, entries, e);
				if (!HENTRY_CLEAR_P(e)) {
					goto again;
				}
			}
		}
	}
}

/************************************************************************/
/*		   garbage collecting weak hash tables			*/
/************************************************************************/
#define MARK_OBJ(obj)				\
	do {					\
		Lisp_Object mo_obj = (obj);	\
		if (!marked_p (mo_obj))  {	\
			mark_object (mo_obj);	\
			did_mark = 1;		\
		}				\
	} while (0)

/* Complete the marking for semi-weak hash tables. */
int finish_marking_weak_hash_tables(void)
{
	Lisp_Object hash_table;
	int did_mark = 0;

	for (hash_table = Vall_weak_hash_tables;
	     !NILP(hash_table);
	     hash_table = XHASH_TABLE(hash_table)->next_weak) {
		const hash_table_t ht = XHASH_TABLE(hash_table);
		hentry_t e = ht->hentries;
		const hentry_t sentinel = e + ht->size;

		if (!marked_p(hash_table)) {
			/* The hash table is probably garbage.  Ignore it. */
			continue;
		}

		/* Now, scan over all the pairs.  For all pairs that are
		   half-marked, we may need to mark the other half if we're
		   keeping this pair. */
		switch (ht->weakness) {
		case HASH_TABLE_KEY_WEAK:
			for (; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
					if (marked_p(e->key)) {
						MARK_OBJ(e->value);
					}
				}
			}
			break;

		case HASH_TABLE_VALUE_WEAK:
			for (; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
					if (marked_p(e->value)) {
						MARK_OBJ(e->key);
					}
				}
			}
			break;

		case HASH_TABLE_KEY_VALUE_WEAK:
			for (; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
					if (marked_p(e->value)) {
						MARK_OBJ(e->key);
					} else if (marked_p(e->key)) {
						MARK_OBJ(e->value);
					}
				}
			}
			break;

		case HASH_TABLE_KEY_CAR_WEAK:
			for (; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
					if (!CONSP(e->key)
					    || marked_p(XCAR(e->key))) {
						MARK_OBJ(e->key);
						MARK_OBJ(e->value);
					}
				}
			}
			break;

			/* We seem to be sprouting new weakness types at an
			   alarming rate. At least this is not externally
			   visible - and in fact all of these KEY_CAR_* types
			   are only used by the glyph code. */
		case HASH_TABLE_KEY_CAR_VALUE_WEAK:
			for (; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
					if (!CONSP(e->key)
					    || marked_p(XCAR(e->key))) {
						MARK_OBJ(e->key);
						MARK_OBJ(e->value);
					} else if (marked_p(e->value)) {
						MARK_OBJ(e->key);
					}
				}
			}
			break;

		case HASH_TABLE_VALUE_CAR_WEAK:
			for (; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
					if (!CONSP(e->value)
					    || marked_p(XCAR(e->value))) {
						MARK_OBJ(e->key);
						MARK_OBJ(e->value);
					}
				}
			}
			break;

			/* all the rest */
		case HASH_TABLE_NON_WEAK:
		case HASH_TABLE_WEAK:
		default:
			break;
		}
	}

	return did_mark;
}

void prune_weak_hash_tables(void)
{
	Lisp_Object hash_table, prev = Qnil;

	for (hash_table = Vall_weak_hash_tables; !NILP(hash_table);
	     hash_table = XHASH_TABLE(hash_table)->next_weak) {
		if (!marked_p(hash_table)) {
			/* This hash table itself is garbage.  Remove it from
			   the list. */
			if (NILP(prev))
				Vall_weak_hash_tables =
				    XHASH_TABLE(hash_table)->next_weak;
			else
				XHASH_TABLE(prev)->next_weak =
				    XHASH_TABLE(hash_table)->next_weak;
		} else {
			/* Now, scan over all the pairs.  Remove all of the pairs
			   in which the key or value, or both, is unmarked
			   (depending on the weakness of the hash table). */
			hash_table_t ht = XHASH_TABLE(hash_table);
			hentry_t entries = ht->hentries;
			hentry_t sentinel = entries + ht->size;

			for (hentry_t e = entries; e < sentinel; e++) {
				if (!HENTRY_CLEAR_P(e)) {
				again:
					if (!marked_p(e->key) ||
					    !marked_p(e->value)) {
						remhash_1(ht, entries, e);
						if (!HENTRY_CLEAR_P(e)) {
							goto again;

						}
					}
				}
			}
			prev = hash_table;
		}
	}
	return;
}

/* Return a hash value for an array of Lisp_Objects of size SIZE. */

hcode_t
internal_array_hash(const Lisp_Object *arr, size_t size, int depth)
{
	hcode_t hash = 0;
	depth++;

	if (size <= 5) {
		for (size_t i = 0; i < size; i++) {
			hash = HASH2(hash, internal_hash(arr[i], depth));
		}
		return hash;
	}

	/* just pick five elements scattered throughout the array.
	   A slightly better approach would be to offset by some
	   noise factor from the points chosen below. */
	for (int i = 0; i < 5; i++) {
		hash = HASH2(hash, internal_hash(arr[i * size / 5], depth));
	}
	return hash;
}

/* Return a hash value for a Lisp_Object.  This is for use when hashing
   objects with the comparison being `equal' (for `eq', you can just
   use the Lisp_Object itself as the hash value).  You need to make a
   tradeoff between the speed of the hash function and how good the
   hashing is.  In particular, the hash function needs to be FAST,
   so you can't just traipse down the whole tree hashing everything
   together.  Most of the time, objects will differ in the first
   few elements you hash.  Thus, we only go to a short depth (5)
   and only hash at most 5 elements out of a vector.  Theoretically
   we could still take 5^5 time (a big big number) to compute a
   hash, but practically this won't ever happen. */

hcode_t
internal_hash(const Lisp_Object obj, int depth)
{
	if (depth > 5)
		return 0;
	if (CONSP(obj) && !CONSP(XCDR(obj))) {
		/* special case for '(a . b) conses */
		return HASH2(internal_hash(XCAR(obj), depth + 1),
			     internal_hash(XCDR(obj), depth + 1));
	} else if (CONSP(obj)) {
		/* no point in worrying about tail recursion, since we're not
		   going very deep */
		Lisp_Object o = obj;
		/* unroll */
		hcode_t hash = internal_hash(XCAR(o), depth+1);

		o = XCDR(o);
		for (int s = 1; s < 6 && CONSP(o); o = XCDR(o), s++) {
			hcode_t h = internal_hash(XCAR(o), depth+1);
			hash = HASH3(hash, h, s);
		}
		return hash;
	}
	if (STRINGP(obj)) {
		return hash_string(XSTRING_DATA(obj), XSTRING_LENGTH(obj));
	}
	if (LRECORDP(obj)) {
		const struct lrecord_implementation
		*imp = XRECORD_LHEADER_IMPLEMENTATION(obj);
		if (imp->hash)
			return imp->hash(obj, depth);
	}

	return LISP_HASH(obj);
}

DEFUN("sxhash", Fsxhash, 1, 1, 0,	/*
Return a hash value for OBJECT.
\(equal obj1 obj2) implies (= (sxhash obj1) (sxhash obj2)).
*/
      (object))
{
	return make_int(internal_hash(object, 0));
}


/* the seq/dict implementation */
/* iterator stuff, only needed for dict so make it static */
static void
ht_iter_init(dict_t d, dict_iter_t di)
{
	const hash_table_t ht = (hash_table_t)d;
	di->dict = d;

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			di->data = e;
			return;
		}
	}
	di->data = NULL;
	return;
}

static void
ht_iter_fini(dict_iter_t di)
{
	di->dict = di->data = NULL;
	return;
}

static void
ht_diter_next(dict_iter_t di, Lisp_Object *key, Lisp_Object *val)
{
	hentry_t e = di->data;
	const hash_table_t ht = (hash_table_t)di->dict;

	if (UNLIKELY(e == NULL)) {
		*key = *val = Qnull_pointer;
		return;
	}

	*key = e->key;
	*val = e->value;

	/* wind to next hentry */
	for (const hentry_t last = ht->hentries + ht->size; ++e < last;) {
		if (!HENTRY_CLEAR_P(e)) {
			di->data = e;
			return;
		}
	}
	di->data = NULL;
	return;
}

static void
ht_siter_next(seq_iter_t si, void **elm)
{
	hentry_t e = si->data;
	const hash_table_t ht = (hash_table_t)si->seq;

	if (UNLIKELY(e == NULL)) {
		*elm = Qnull_pointer;
		return;
	}

	*elm = (void*)e->key;

	/* wind to next hentry */
	for (const hentry_t last = ht->hentries + ht->size; ++e < last;) {
		if (!HENTRY_CLEAR_P(e)) {
			si->data = e;
			return;
		}
	}
	si->data = NULL;
	return;
}

static void
ht_iter_reset(seq_iter_t si)
{
	const hash_table_t ht = (hash_table_t)si->seq;

	for (hentry_t e = ht->hentries, sntl = e + ht->size; e < sntl; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			si->data = e;
			return;
		}
	}
	si->data = NULL;
	return;
}

static size_t
ht_explode(void *restrict tgt[], size_t ntgt, seq_t s)
{
	volatile size_t i = 0;
	const hash_table_t ht = (hash_table_t)s;

	for (hentry_t e = ht->hentries, sntl = e + ht->size;
	     e < sntl && i < ntgt; e++) {
		if (!HENTRY_CLEAR_P(e)) {
			tgt[i++] = (void*)e->key;
		}
	}
	return i;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

static struct seq_impl_s __shash_table = {
	.length_f = (seq_length_f)dict_ht_size,
	.iter_init_f = (seq_iter_init_f)ht_iter_init,
	.iter_next_f = ht_siter_next,
	.iter_fini_f = (seq_iter_fini_f)ht_iter_fini,
	.iter_reset_f = ht_iter_reset,
	.explode_f = ht_explode,
};

static struct dict_impl_s __dhash_table = {
	.size_f = (dict_size_f)dict_ht_size,
	.put_f = (dict_put_f)dict_ht_put,
	.get_f = (dict_get_f)dict_ht_get,
	.remove_f = (dict_remove_f)dict_ht_remove,
	.iter_init_f = ht_iter_init,
	.iter_next_f = ht_diter_next,
	.iter_fini_f = ht_iter_fini,
};

/* deal with seq interface */
const seq_impl_t seq_hash_table = &__shash_table;
/* deal with dict interface */
const dict_impl_t dict_hash_table = &__dhash_table;

void syms_of_elhash(void)
{
	INIT_LRECORD_IMPLEMENTATION(hash_table);

	DEFSUBR(Fhash_table_p);
	DEFSUBR(Fmake_hash_table);
	DEFSUBR(Fcopy_hash_table);
	DEFSUBR(Fgethash);
	DEFSUBR(Fremhash);
	DEFSUBR(Fputhash);
	DEFSUBR(Fclrhash);
	DEFSUBR(Fmaphash);
	DEFSUBR(Fhash_table_count);
	DEFSUBR(Fhash_table_test);
	DEFSUBR(Fhash_table_size);
	DEFSUBR(Fhash_table_rehash_size);
	DEFSUBR(Fhash_table_rehash_threshold);
	DEFSUBR(Fhash_table_weakness);
	DEFSUBR(Fhash_table_type);	/* obsolete */
	DEFSUBR(Fsxhash);
#if 0
	DEFSUBR(Finternal_hash_value);
#endif

	defsymbol(&Qhash_tablep, "hash-table-p");
	defsymbol(&Qhash_table, "hash-table");
	defsymbol(&Qhashtable, "hashtable");
	defsymbol(&Qweakness, "weakness");
	defsymbol(&Qvalue, "value");
	defsymbol(&Qkey_or_value, "key-or-value");
	defsymbol(&Qkey_and_value, "key-and-value");
	defsymbol(&Qrehash_size, "rehash-size");
	defsymbol(&Qrehash_threshold, "rehash-threshold");

	defsymbol(&Qweak, "weak");	/* obsolete */
	defsymbol(&Qkey_weak, "key-weak");	/* obsolete */
	defsymbol(&Qkey_or_value_weak, "key-or-value-weak");	/* obsolete */
	defsymbol(&Qvalue_weak, "value-weak");	/* obsolete */
	defsymbol(&Qnon_weak, "non-weak");	/* obsolete */

	defkeyword(&Q_test, ":test");
	defkeyword(&Q_size, ":size");
	defkeyword(&Q_rehash_size, ":rehash-size");
	defkeyword(&Q_rehash_threshold, ":rehash-threshold");
	defkeyword(&Q_weakness, ":weakness");
	defkeyword(&Q_type, ":type");	/* obsolete */
}

void
elhash_reinit(void)
{
	morphisms[lrecord_type_hash_table].seq_impl = seq_hash_table;
	morphisms[lrecord_type_hash_table].aset_impl = dict_hash_table;
	return;
}

void vars_of_elhash(void)
{
	/* This must NOT be staticpro'd */
	Vall_weak_hash_tables = Qnil;
	dump_add_weak_object_chain(&Vall_weak_hash_tables);
}

/* elhash.c ends here */
