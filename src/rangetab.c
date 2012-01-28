/* SXEmacs routines to deal with range tables.
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

/* Written by Ben Wing, August 1995. */

#include <config.h>
#include "lisp.h"
#include "rangetab.h"

Lisp_Object Qrange_tablep;
Lisp_Object Qrange_table;

/************************************************************************/
/*                            Range table object                        */
/************************************************************************/

/* We use a sorted array of ranges.

   #### We should be using the gap array stuff from extents.c.  This
   is not hard but just requires moving that stuff out of that file. */

static Lisp_Object mark_range_table(Lisp_Object obj)
{
	Lisp_Range_Table *rt = XRANGE_TABLE(obj);
	int i;

	for (i = 0; i < Dynarr_length(rt->entries); i++)
		mark_object(Dynarr_at(rt->entries, i).val);
	return Qnil;
}

static void
print_range_table(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Range_Table *rt = XRANGE_TABLE(obj);
	int i;

	write_c_string("#s(range-table data (", printcharfun);
	for (i = 0; i < Dynarr_length(rt->entries); i++) {
		struct range_table_entry *rte = Dynarr_atp(rt->entries, i);
		if (i > 0)
			write_c_string(" ", printcharfun);
		if (rte->first == rte->last)
			write_fmt_str(printcharfun, "%ld ", (long)(rte->first));
		else
			write_fmt_str(printcharfun, "(%ld %ld) ",
				      (long)(rte->first),
				      (long)(rte->last));
		print_internal(rte->val, printcharfun, 1);
	}
	write_c_string("))", printcharfun);
}

static int range_table_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Range_Table *rt1 = XRANGE_TABLE(obj1);
	Lisp_Range_Table *rt2 = XRANGE_TABLE(obj2);
	int i;

	if (Dynarr_length(rt1->entries) != Dynarr_length(rt2->entries))
		return 0;

	for (i = 0; i < Dynarr_length(rt1->entries); i++) {
		struct range_table_entry *rte1 = Dynarr_atp(rt1->entries, i);
		struct range_table_entry *rte2 = Dynarr_atp(rt2->entries, i);

		if (rte1->first != rte2->first
		    || rte1->last != rte2->last
		    || !internal_equal(rte1->val, rte2->val, depth + 1))
			return 0;
	}

	return 1;
}

static unsigned long
range_table_entry_hash(struct range_table_entry *rte, int depth)
{
	return HASH3(rte->first, rte->last, internal_hash(rte->val, depth + 1));
}

static unsigned long range_table_hash(Lisp_Object obj, int depth)
{
	Lisp_Range_Table *rt = XRANGE_TABLE(obj);
	int i;
	int size = Dynarr_length(rt->entries);
	unsigned long hash = size;

	/* approach based on internal_array_hash(). */
	if (size <= 5) {
		for (i = 0; i < size; i++)
			hash = HASH2(hash,
				     range_table_entry_hash(Dynarr_atp
							    (rt->entries, i),
							    depth));
		return hash;
	}

	/* just pick five elements scattered throughout the array.
	   A slightly better approach would be to offset by some
	   noise factor from the points chosen below. */
	for (i = 0; i < 5; i++)
		hash =
		    HASH2(hash,
			  range_table_entry_hash(Dynarr_atp
						 (rt->entries, i * size / 5),
						 depth));
	return hash;
}

static const struct lrecord_description rte_description_1[] = {
	{XD_LISP_OBJECT, offsetof(range_table_entry, val)},
	{XD_END}
};

static const struct struct_description rte_description = {
	sizeof(range_table_entry),
	rte_description_1
};

static const struct lrecord_description rted_description_1[] = {
	XD_DYNARR_DESC(range_table_entry_dynarr, &rte_description),
	{XD_END}
};

static const struct struct_description rted_description = {
	sizeof(range_table_entry_dynarr),
	rted_description_1
};

static const struct lrecord_description range_table_description[] = {
	{XD_STRUCT_PTR, offsetof(Lisp_Range_Table, entries), 1,
	 &rted_description},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION("range-table", range_table,
			      mark_range_table, print_range_table, 0,
			      range_table_equal, range_table_hash,
			      range_table_description, Lisp_Range_Table);

/************************************************************************/
/*                        Range table operations                        */
/************************************************************************/

#ifdef ERROR_CHECK_TYPECHECK

static void verify_range_table(Lisp_Range_Table * rt)
{
	int i;

	for (i = 0; i < Dynarr_length(rt->entries); i++) {
		struct range_table_entry *rte = Dynarr_atp(rt->entries, i);
		assert(rte->last >= rte->first);
		if (i > 0)
			assert(Dynarr_at(rt->entries, i - 1).last < rte->first);
	}
}

#else

#define verify_range_table(rt)

#endif

/* Look up in a range table without the Dynarr wrapper.
   Used also by the unified range table format. */

static Lisp_Object
get_range_table(EMACS_INT pos, int nentries, struct range_table_entry *tab,
		Lisp_Object default_)
{
	int left = 0, right = nentries;

	/* binary search for the entry.  Based on similar code in
	   extent_list_locate(). */
	while (left != right) {
		/* RIGHT might not point to a valid entry (i.e. it's at the end
		   of the list), so NEWPOS must round down. */
		unsigned int newpos = (left + right) >> 1;
		struct range_table_entry *entry = tab + newpos;
		if (pos > entry->last)
			left = newpos + 1;
		else if (pos < entry->first)
			right = newpos;
		else
			return entry->val;
	}

	return default_;
}

DEFUN("range-table-p", Frange_table_p, 1, 1, 0,	/*
Return non-nil if OBJECT is a range table.
*/
      (object))
{
	return RANGE_TABLEP(object) ? Qt : Qnil;
}

DEFUN("make-range-table", Fmake_range_table, 0, 0, 0,	/*
Return a new, empty range table.
You can manipulate it using `put-range-table', `get-range-table',
`remove-range-table', and `clear-range-table'.
*/
      ())
{
	Lisp_Object obj;
	Lisp_Range_Table *rt = alloc_lcrecord_type(Lisp_Range_Table,
						   &lrecord_range_table);
	rt->entries = Dynarr_new(range_table_entry);
	XSETRANGE_TABLE(obj, rt);
	return obj;
}

DEFUN("copy-range-table", Fcopy_range_table, 1, 1, 0,	/*
Return a new range table which is a copy of RANGE-TABLE.
It will contain the same values for the same ranges as RANGE-TABLE.
The values will not themselves be copied.
*/
      (range_table))
{
	Lisp_Range_Table *rt, *rtnew;
	Lisp_Object obj;

	CHECK_RANGE_TABLE(range_table);
	rt = XRANGE_TABLE(range_table);

	rtnew = alloc_lcrecord_type(Lisp_Range_Table, &lrecord_range_table);
	rtnew->entries = Dynarr_new(range_table_entry);

	Dynarr_add_many(rtnew->entries, Dynarr_atp(rt->entries, 0),
			Dynarr_length(rt->entries));
	XSETRANGE_TABLE(obj, rtnew);
	return obj;
}

DEFUN("get-range-table", Fget_range_table, 2, 3, 0,	/*
Find value for position POS in RANGE-TABLE.
If there is no corresponding value, return DEFAULT (defaults to nil).
*/
      (pos, range_table, default_))
{
	Lisp_Range_Table *rt;

	CHECK_RANGE_TABLE(range_table);
	rt = XRANGE_TABLE(range_table);

	CHECK_INT_COERCE_CHAR(pos);

	return get_range_table(XINT(pos), Dynarr_length(rt->entries),
			       Dynarr_atp(rt->entries, 0), default_);
}

void
put_range_table(Lisp_Object table, EMACS_INT first,
		EMACS_INT last, Lisp_Object val)
{
	int i;
	int insert_me_here = -1;
	Lisp_Range_Table *rt = XRANGE_TABLE(table);

	/* Now insert in the proper place.  This gets tricky because
	   we may be overlapping one or more existing ranges and need
	   to fix them up. */

	/* First delete all sections of any existing ranges that overlap
	   the new range. */
	for (i = 0; i < Dynarr_length(rt->entries); i++) {
		struct range_table_entry *entry = Dynarr_atp(rt->entries, i);
		/* We insert before the first range that begins at or after the
		   new range. */
		if (entry->first >= first && insert_me_here < 0)
			insert_me_here = i;
		if (entry->last < first)
			/* completely before the new range. */
			continue;
		if (entry->first > last)
			/* completely after the new range.  No more possibilities of
			   finding overlapping ranges. */
			break;
		if (entry->first < first && entry->last <= last) {
			/* looks like:

			   [ NEW ]
			   [ EXISTING ]

			 */
			/* truncate the end off of it. */
			entry->last = first - 1;
		} else if (entry->first < first && entry->last > last)
			/* looks like:

			   [ NEW ]
			   [ EXISTING ]

			 */
			/* need to split this one in two. */
		{
			struct range_table_entry insert_me_too;

			insert_me_too.first = last + 1;
			insert_me_too.last = entry->last;
			insert_me_too.val = entry->val;
			entry->last = first - 1;
			Dynarr_insert_many(rt->entries, &insert_me_too, 1,
					   i + 1);
		} else if (entry->last > last) {
			/* looks like:

			   [ NEW ]
			   [ EXISTING ]

			 */
			/* truncate the start off of it. */
			entry->first = last + 1;
		} else {
			/* existing is entirely within new. */
			Dynarr_delete_many(rt->entries, i, 1);
			i--;	/* back up since everything shifted one to the left. */
		}
	}

	/* Someone asked us to delete the range, not insert it. */
	if (UNBOUNDP(val))
		return;

	/* Now insert the new entry, maybe at the end. */

	if (insert_me_here < 0)
		insert_me_here = i;

	{
		struct range_table_entry insert_me;

		insert_me.first = first;
		insert_me.last = last;
		insert_me.val = val;

		Dynarr_insert_many(rt->entries, &insert_me, 1, insert_me_here);
	}

	/* Now see if we can combine this entry with adjacent ones just
	   before or after. */

	if (insert_me_here > 0) {
		struct range_table_entry *entry = Dynarr_atp(rt->entries,
							     insert_me_here -
							     1);
		if (EQ(val, entry->val) && entry->last == first - 1) {
			entry->last = last;
			Dynarr_delete_many(rt->entries, insert_me_here, 1);
			insert_me_here--;
			/* We have morphed into a larger range.  Update our records
			   in case we also combine with the one after. */
			first = entry->first;
		}
	}

	if (insert_me_here < Dynarr_length(rt->entries) - 1) {
		struct range_table_entry *entry = Dynarr_atp(rt->entries,
							     insert_me_here +
							     1);
		if (EQ(val, entry->val) && entry->first == last + 1) {
			entry->first = first;
			Dynarr_delete_many(rt->entries, insert_me_here, 1);
		}
	}
}

DEFUN("put-range-table", Fput_range_table, 4, 4, 0,	/*
Set the value for range (START, END) to be VALUE in RANGE-TABLE.
*/
      (start, end, value, range_table))
{
	EMACS_INT first, last;

	CHECK_RANGE_TABLE(range_table);
	CHECK_INT_COERCE_CHAR(start);
	first = XINT(start);
	CHECK_INT_COERCE_CHAR(end);
	last = XINT(end);
	if (first > last)
		signal_simple_error_2("start must be <= end", start, end);

	put_range_table(range_table, first, last, value);
	verify_range_table(XRANGE_TABLE(range_table));
	return Qnil;
}

DEFUN("remove-range-table", Fremove_range_table, 3, 3, 0,	/*
Remove the value for range (START, END) in RANGE-TABLE.
*/
      (start, end, range_table))
{
	return Fput_range_table(start, end, Qunbound, range_table);
}

DEFUN("clear-range-table", Fclear_range_table, 1, 1, 0,	/*
Flush RANGE-TABLE.
*/
      (range_table))
{
	CHECK_RANGE_TABLE(range_table);
	Dynarr_reset(XRANGE_TABLE(range_table)->entries);
	return Qnil;
}

DEFUN("map-range-table", Fmap_range_table, 2, 2, 0,	/*
Map FUNCTION over entries in RANGE-TABLE, calling it with three args,
the beginning and end of the range and the corresponding value.

Results are guaranteed to be correct (i.e. each entry processed
exactly once) if FUNCTION modifies or deletes the current entry
\(i.e. passes the current range to `put-range-table' or
`remove-range-table'), but not otherwise.
*/
      (function, range_table))
{
	Lisp_Range_Table *rt;
	int i;

	CHECK_RANGE_TABLE(range_table);
	CHECK_FUNCTION(function);

	rt = XRANGE_TABLE(range_table);

	/* Do not "optimize" by pulling out the length computation below!
	   FUNCTION may have changed the table. */
	for (i = 0; i < Dynarr_length(rt->entries); i++) {
		struct range_table_entry *entry = Dynarr_atp(rt->entries, i);
		EMACS_INT first, last;
		Lisp_Object args[4];
		int oldlen;

	      again:
		first = entry->first;
		last = entry->last;
		oldlen = Dynarr_length(rt->entries);
		args[0] = function;
		args[1] = make_int(first);
		args[2] = make_int(last);
		args[3] = entry->val;
		Ffuncall(countof(args), args);
		/* Has FUNCTION removed the entry? */
		if (oldlen > Dynarr_length(rt->entries)
		    && i < Dynarr_length(rt->entries)
		    && (first != entry->first || last != entry->last))
			goto again;
	}

	return Qnil;
}

/************************************************************************/
/*                         Range table read syntax                      */
/************************************************************************/

static int
rangetab_data_validate(Lisp_Object keyword, Lisp_Object value,
		       Error_behavior errb)
{
	Lisp_Object rest;

	/* #### should deal with errb */
	EXTERNAL_LIST_LOOP(rest, value) {
		Lisp_Object range = XCAR(rest);
		rest = XCDR(rest);
		if (!CONSP(rest))
			signal_simple_error("Invalid list format", value);
		if (!INTP(range) && !CHARP(range)
		    && !(CONSP(range) && CONSP(XCDR(range))
			 && NILP(XCDR(XCDR(range)))
			 && (INTP(XCAR(range)) || CHARP(XCAR(range)))
			 && (INTP(XCAR(XCDR(range)))
			     || CHARP(XCAR(XCDR(range))))))
			signal_simple_error("Invalid range format", range);
	}

	return 1;
}

static Lisp_Object rangetab_instantiate(Lisp_Object data)
{
	Lisp_Object rangetab = Fmake_range_table();

	if (!NILP(data)) {
		data = Fcar(Fcdr(data));	/* skip over 'data keyword */
		while (!NILP(data)) {
			Lisp_Object range = Fcar(data);
			Lisp_Object val = Fcar(Fcdr(data));

			data = Fcdr(Fcdr(data));
			if (CONSP(range))
				Fput_range_table(Fcar(range), Fcar(Fcdr(range)),
						 val, rangetab);
			else
				Fput_range_table(range, range, val, rangetab);
		}
	}

	return rangetab;
}

/************************************************************************/
/*                         Unified range tables                         */
/************************************************************************/

/* A "unified range table" is a format for storing range tables
   as contiguous blocks of memory.  This is used by the regexp
   code, which needs to use range tables to properly handle []
   constructs in the presence of extended characters but wants to
   store an entire compiled pattern as a contiguous block of memory.

   Unified range tables are designed so that they can be placed
   at an arbitrary (possibly mis-aligned) place in memory.
   (Dealing with alignment is a pain in the ass.)

   WARNING: No provisions for garbage collection are currently made.
   This means that there must not be any Lisp objects in a unified
   range table that need to be marked for garbage collection.
   Good candidates for objects that can go into a range table are

   -- numbers and characters (do not need to be marked)
   -- nil, t (marked elsewhere)
   -- charsets and coding systems (automatically marked because
				   they are in a marked list,
				   and can't be removed)

   Good but slightly less so:

   -- symbols (could be uninterned, but that is not likely)

   Somewhat less good:

   -- buffers, frames, devices (could get deleted)

   It is expected that you work with range tables in the normal
   format and then convert to unified format when you are done
   making modifications.  As such, no functions are provided
   for modifying a unified range table.  The only operations
   you can do to unified range tables are

   -- look up a value
   -- retrieve all the ranges in an iterative fashion

*/

/* The format of a unified range table is as follows:

   -- The first byte contains the number of bytes to skip to find the
      actual start of the table.  This deals with alignment constraints,
      since the table might want to go at any arbitrary place in memory.
   -- The next three bytes contain the number of bytes to skip (from the
      *first* byte) to find the stuff after the table.  It's stored in
      little-endian format because that's how God intended things.  We don't
      necessarily start the stuff at the very end of the table because
      we want to have at least ALIGNOF (EMACS_INT) extra space in case
      we have to move the range table around. (It appears that some
      architectures don't maintain alignment when reallocing.)
   -- At the prescribed offset is a struct unified_range_table, containing
      some number of `struct range_table_entry' entries. */

struct unified_range_table {
	int nentries;
	struct range_table_entry first;
};

/* Return size in bytes needed to store the data in a range table. */

int unified_range_table_bytes_needed(Lisp_Object rangetab)
{
	return (sizeof(struct range_table_entry) *
		(Dynarr_length(XRANGE_TABLE(rangetab)->entries) - 1) +
		sizeof(struct unified_range_table) +
		/* ALIGNOF a struct may be too big. */
		/* We have four bytes for the size numbers, and an extra
		   four or eight bytes for making sure we get the alignment
		   OK. */
		ALIGNOF(EMACS_INT) + 4);
}

/* Convert a range table into unified format and store in DEST,
   which must be able to hold the number of bytes returned by
   range_table_bytes_needed(). */

void unified_range_table_copy_data(Lisp_Object rangetab, void *dest)
{
	/* We cast to the above structure rather than just casting to
	   char * and adding sizeof(int), because that will lead to
	   mis-aligned data on the Alpha machines. */
	struct unified_range_table *un;
	range_table_entry_dynarr *rted = XRANGE_TABLE(rangetab)->entries;
	int total_needed = unified_range_table_bytes_needed(rangetab);
	void *new_dest = ALIGN_PTR((char *)dest + 4, ALIGNOF(EMACS_INT));

	*(char *)dest = (char)((char *)new_dest - (char *)dest);
	*((unsigned char *)dest + 1) = total_needed & 0xFF;
	total_needed >>= 8;
	*((unsigned char *)dest + 2) = total_needed & 0xFF;
	total_needed >>= 8;
	*((unsigned char *)dest + 3) = total_needed & 0xFF;
	un = (struct unified_range_table *)new_dest;
	un->nentries = Dynarr_length(rted);
	memcpy(&un->first, Dynarr_atp(rted, 0),
	       sizeof(struct range_table_entry) * Dynarr_length(rted));
}

/* Return number of bytes actually used by a unified range table. */

int unified_range_table_bytes_used(const void *unrangetab)
{
	return ((*((const unsigned char*)unrangetab + 1))
		+ ((*((const unsigned char*)unrangetab + 2)) << 8)
		+ ((*((const unsigned char*)unrangetab + 3)) << 16));
}

/* Make sure the table is aligned, and move it around if it's not. */
static void
align_the_damn_table(void *unrangetab)
{
	const void *cur_dest = (char*)unrangetab + *(char*)unrangetab;
#if SXE_LONGBITS == 64
	if ((((long)cur_dest) & 7) != 0)
#else
	if ((((int)cur_dest) & 3) != 0)
#endif
	{
		int count = (unified_range_table_bytes_used(unrangetab) - 4
			     - ALIGNOF(EMACS_INT));
		/* Find the proper location, just like above. */
		void *new_dest = ALIGN_PTR((char*)unrangetab + 4,
					   ALIGNOF(EMACS_INT));
		/* memmove() works in the presence of overlapping data. */
		memmove(new_dest, cur_dest, count);
		*(char*)unrangetab =
			(char)((char*)new_dest - (char*)unrangetab);
	}
}

/* Look up a value in a unified range table. */

Lisp_Object
unified_range_table_lookup(void *unrangetab, EMACS_INT pos,
			   Lisp_Object default_)
{
	void *new_dest;
	struct unified_range_table *un;

	align_the_damn_table(unrangetab);
	new_dest = (char *)unrangetab + *(char *)unrangetab;
	un = (struct unified_range_table *)new_dest;

	return get_range_table(pos, un->nentries, &un->first, default_);
}

/* Return number of entries in a unified range table. */

int
unified_range_table_nentries(void *unrangetab)
{
	void *new_dest;
	struct unified_range_table *un;

	align_the_damn_table(unrangetab);
	new_dest = (char *)unrangetab + *(char *)unrangetab;
	un = (struct unified_range_table *)new_dest;
	return un->nentries;
}

/* Return the OFFSETth range (counting from 0) in UNRANGETAB. */
void
unified_range_table_get_range(void *unrangetab, int offset,
			      EMACS_INT * min, EMACS_INT * max,
			      Lisp_Object * val)
{
	void *new_dest;
	struct unified_range_table *un;
	struct range_table_entry *tab;

	align_the_damn_table(unrangetab);
	new_dest = (char *)unrangetab + *(char *)unrangetab;
	un = (struct unified_range_table *)new_dest;

	assert(offset >= 0 && offset < un->nentries);
	tab = (&un->first) + offset;
	*min = tab->first;
	*max = tab->last;
	*val = tab->val;
}

/************************************************************************/
/*                            Initialization                            */
/************************************************************************/

void syms_of_rangetab(void)
{
	INIT_LRECORD_IMPLEMENTATION(range_table);

	defsymbol(&Qrange_tablep, "range-table-p");
	defsymbol(&Qrange_table, "range-table");

	DEFSUBR(Frange_table_p);
	DEFSUBR(Fmake_range_table);
	DEFSUBR(Fcopy_range_table);
	DEFSUBR(Fget_range_table);
	DEFSUBR(Fput_range_table);
	DEFSUBR(Fremove_range_table);
	DEFSUBR(Fclear_range_table);
	DEFSUBR(Fmap_range_table);
}

void structure_type_create_rangetab(void)
{
	struct structure_type *st;

	st = define_structure_type(Qrange_table, 0, rangetab_instantiate);

	define_structure_type_keyword(st, Qdata, rangetab_data_validate);
}
