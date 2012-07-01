/* Copyright (c) 1994, 1995 Free Software Foundation.
   Copyright (c) 1995 Ben Wing.

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

#ifndef INCLUDED_extents_h_
#define INCLUDED_extents_h_

DECLARE_LRECORD(extent, struct extent);
#define XEXTENT(x) XRECORD (x, extent, struct extent)
#define XSETEXTENT(x, p) XSETRECORD (x, p, extent)
#define EXTENTP(x) RECORDP (x, extent)
#define CHECK_EXTENT(x) CHECK_RECORD (x, extent)
#define CONCHECK_EXTENT(x) CONCHECK_RECORD (x, extent)

/* the layouts for glyphs (extent->flags.glyph_layout).  Must fit in 2 bits. */
typedef enum glyph_layout {
	GL_TEXT,
	GL_OUTSIDE_MARGIN,
	GL_INSIDE_MARGIN,
	GL_WHITESPACE
} glyph_layout;

struct extent {
	struct lrecord_header lheader;

	Memind start;
	Memind end;
	Lisp_Object object;	/* A buffer, string, Qnil (extent detached from no
				   buffer), Qt (destroyed extent) */

	/* Extent properties are conceptually a plist, but the most common
	   props are implemented as bits instead of conses.  */
	struct {
		Lisp_Object face;

		/* These flags are simply an optimization for common
		   boolean properties which go onto the extent's
		   property list.  Any of them would work if done in
		   the normal way, but the space savings of doing
		   these in this way is significant.  Note that if you
		   add a flag, there are numerous places in extents.c
		   that need to know about it.

		   Another consideration is that some of these
		   properties are accessed during redisplay, so it's
		   good for access to them to be fast (a bit reference
		   instead of a search down a plist).

		   `begin_glyph_layout' and `end_glyph_layout' are
		   unusual in that they have 4 states instead of 2.

		   Other special extent properties are stored in an
		   auxiliary structure that sits at the beginning of
		   the plist.  The has_aux flag indicates whether this
		   structure exists.  The has_parent flag is an
		   optimization indicating whether the extent has a
		   parent (this could also be determined by looking in
		   the aux structure). */

		 enum_field(glyph_layout) begin_glyph_layout:2;
		/*  2 text, margins, or whitespace */
		 enum_field(glyph_layout) end_glyph_layout:2;
		/*  4 text, margins, or whitespace */
		unsigned int has_parent:1;	/*  5 extent has a parent          */
		unsigned int has_aux:1;	/*  6 extent has an aux. structure */
		unsigned int start_open:1;	/*  7 insertion behavior at start  */
		unsigned int end_open:1;	/*  8 insertion behavior at end    */
		unsigned int unique:1;	/*  9 there may be only one attached  */
		unsigned int duplicable:1;	/* 10 copied to strings by kill/undo  */
		unsigned int detachable:1;	/* 11 extent detaches if text deleted */
		unsigned int internal:1;	/* 12 used by map-extents etc.        */
		unsigned int in_red_event:1;	/* 13 An event has been spawned for
						   initial redisplay.
						   (not exported to lisp) */
		unsigned int unused16:1;	/* 16 unused bits                   */
		/* --- Adding more flags will cause the extent struct to grow by another
		   word.  It's not clear that this would make a difference, however,
		   because on 32-bit machines things tend to get allocated in chunks
		   of 4 bytes. */
	} flags;
	/* The plist may have an auxiliary structure as its first element */
	Lisp_Object plist;
};

/* Basic properties of an extent (not affected by the extent's parent) */
#define extent_object(e) ((e)->object)
#define extent_start(e) ((e)->start + 0)
#define extent_end(e) ((e)->end + 0)
#define set_extent_start(e, val) ((void) ((e)->start = (val)))
#define set_extent_end(e, val) ((void) ((e)->end = (val)))
#define extent_endpoint(e, endp) ((endp) ? extent_end (e) : extent_start (e))
#define set_extent_endpoint(e, val, endp) \
  ((endp) ? set_extent_end (e, val) : set_extent_start (e, val))
#define extent_detached_p(e) (extent_start (e) < 0)

/* Additional information that may be present in an extent.  The idea is
   that fast access is provided to this information, but since (hopefully)
   most extents won't have this set on them, we usually don't need to
   have this structure around and thus the size of an extent is smaller. */

typedef struct extent_auxiliary extent_auxiliary;
struct extent_auxiliary {
	struct lcrecord_header header;

	Lisp_Object begin_glyph;
	Lisp_Object end_glyph;
	Lisp_Object parent;
	/* We use a weak list here.  Originally I didn't do this and
	   depended on having the extent's finalization method remove
	   itself from its parent's children list.  This runs into
	   lots and lots of problems though because everything is in
	   a really really bizarre state when an extent's finalization
	   method is called (it happens in sweep_extents() by way of
	   ADDITIONAL_FREE_extent()) and it's extremely difficult to
	   avoid getting hosed by just-freed objects. */
	Lisp_Object children;
	Lisp_Object invisible;
	Lisp_Object read_only;
	Lisp_Object mouse_face;
	Lisp_Object initial_redisplay_function;
	Lisp_Object before_change_functions, after_change_functions;
	int priority;
};

extern struct extent_auxiliary extent_auxiliary_defaults;

DECLARE_LRECORD(extent_auxiliary, struct extent_auxiliary);
#define XEXTENT_AUXILIARY(x) \
  XRECORD (x, extent_auxiliary, struct extent_auxiliary)
#define XSETEXTENT_AUXILIARY(x, p) XSETRECORD (x, p, extent_auxiliary)
#define EXTENT_AUXILIARYP(x) RECORDP (x, extent_auxiliary)
#define CHECK_EXTENT_AUXILIARY(x) CHECK_RECORD (x, extent_auxiliary)
#define CONCHECK_EXTENT_AUXILIARY(x) CONCHECK_RECORD (x, extent_auxiliary)

struct extent_info {
	struct lcrecord_header header;

	struct extent_list_s *extents;
	struct extent_stack_s *soe;
};

DECLARE_LRECORD(extent_info, struct extent_info);
#define XEXTENT_INFO(x) XRECORD (x, extent_info, struct extent_info)
#define XSETEXTENT_INFO(x, p) XSETRECORD (x, p, extent_info)
#define EXTENT_INFOP(x) RECORDP (x, extent_info)
#define CHECK_EXTENT_INFO(x) CHECK_RECORD (x, extent_info)
#define CONCHECK_EXTENT_INFO(x) CONCHECK_RECORD (x, extent_info)

void flush_cached_extent_info(Lisp_Object extent_info);

/* A "normal" field is one that is stored in the `struct flags' structure
   in an extent.  an "aux" field is one that is stored in the extent's
   auxiliary structure.

   The functions below that have `extent_no_chase' in their name operate
   on an extent directly (ignoring its parent), and should normally
   only be used on extents known not to have a parent.  The other
   versions chase down any parent links. */

#define extent_no_chase_normal_field(e, field) ((e)->flags.field)

extern_inline struct extent_auxiliary *extent_aux_or_default(EXTENT e);
extern_inline struct extent_auxiliary *extent_aux_or_default(EXTENT e)
{
	return e->flags.has_aux ?
	    XEXTENT_AUXILIARY(XCAR(e->plist)) : &extent_auxiliary_defaults;
}

#define extent_no_chase_aux_field(e, field) (extent_aux_or_default(e)->field)

#define extent_normal_field(e, field)				\
  extent_no_chase_normal_field (extent_ancestor (e), field)

#define extent_aux_field(e, field)				\
  extent_no_chase_aux_field (extent_ancestor (e), field)

#define set_extent_no_chase_aux_field(e, field, value) do {	\
  EXTENT sencaf_e = (e);					\
  if (! sencaf_e->flags.has_aux)				\
    allocate_extent_auxiliary (sencaf_e);			\
  XEXTENT_AUXILIARY (XCAR (sencaf_e->plist))->field = (value);\
} while (0)

#define set_extent_no_chase_normal_field(e, field, value)	\
  extent_no_chase_normal_field (e, field) = (value)

#define set_extent_aux_field(e, field, value)			\
  set_extent_no_chase_aux_field (extent_ancestor (e), field, value)

#define set_extent_normal_field(e, field, value)		\
  set_extent_ancestor_normal_field (extent_no_chase (e), field, value)

/* The `parent' and `children' fields are not affected by any
   parent links.  We don't provide any settors for these fields
   because they need special handling and it's cleaner just to
   do this in the particular functions that need to do this. */

#define extent_parent(e)	extent_no_chase_aux_field (e, parent)
#define extent_children(e)	extent_no_chase_aux_field (e, children)

#define extent_begin_glyph(e)	extent_aux_field (e, begin_glyph)
#define extent_end_glyph(e)	extent_aux_field (e, end_glyph)
#define extent_priority(e)	extent_aux_field (e, priority)
#define extent_invisible(e)	extent_aux_field (e, invisible)
#define extent_read_only(e)	extent_aux_field (e, read_only)
#define extent_mouse_face(e)	extent_aux_field (e, mouse_face)
#define extent_initial_redisplay_function(e)	extent_aux_field (e, initial_redisplay_function)
#define extent_before_change_functions(e) extent_aux_field (e, before_change_functions)
#define extent_after_change_functions(e)  extent_aux_field (e, after_change_functions)

#define set_extent_begin_glyph(e, value)	\
  set_extent_aux_field (e, begin_glyph, value)
#define set_extent_end_glyph(e, value)		\
  set_extent_aux_field (e, end_glyph, value)
#define set_extent_priority(e, value)		\
  set_extent_aux_field (e, priority, value)
#define set_extent_invisible_1(e, value)	\
  set_extent_aux_field (e, invisible, value)
#define set_extent_read_only(e, value)		\
  set_extent_aux_field (e, read_only, value)
#define set_extent_mouse_face(e, value)		\
  set_extent_aux_field (e, mouse_face, value)
/* Use Fset_extent_initial_redisplay_function unless you know what you're doing */
#define set_extent_initial_redisplay_function(e, value) \
  set_extent_aux_field (e, initial_redisplay_function, value)
#define set_extent_before_change_functions(e, value)	\
  set_extent_aux_field (e, before_change_functions, value)
#define set_extent_after_change_functions(e, value)	\
  set_extent_aux_field (e, after_change_functions, value)

#define extent_face(e)		     extent_normal_field (e, face)
#define extent_begin_glyph_layout(e) extent_normal_field (e, begin_glyph_layout)
#define extent_end_glyph_layout(e)   extent_normal_field (e, end_glyph_layout)
#define extent_start_open_p(e)	     extent_normal_field (e, start_open)
#define extent_end_open_p(e)	     extent_normal_field (e, end_open)
#define extent_unique_p(e)	     extent_normal_field (e, unique)
#define extent_duplicable_p(e)	     extent_normal_field (e, duplicable)
#define extent_detachable_p(e)	     extent_normal_field (e, detachable)
#define extent_internal_p(e)	     extent_normal_field (e, internal)
#define extent_in_red_event_p(e)     extent_normal_field (e, in_red_event)

extern_inline Lisp_Object *extent_no_chase_plist_addr(EXTENT e);
extern_inline Lisp_Object *extent_no_chase_plist_addr(EXTENT e)
{
	return e->flags.has_aux ? &XCDR(e->plist) : &e->plist;
}

#define extent_no_chase_plist(e) (*extent_no_chase_plist_addr (e))

#define extent_plist_addr(e) extent_no_chase_plist_addr (extent_ancestor (e))
#define extent_plist_slot(e) extent_no_chase_plist (extent_ancestor (e))

/* flags for map_extents() and friends */
#define ME_END_CLOSED (1 << 0)
#define ME_START_OPEN (1 << 1)
#define ME_ALL_EXTENTS_CLOSED (1 << 2)
#define ME_ALL_EXTENTS_OPEN (2 << 2)
#define ME_ALL_EXTENTS_CLOSED_OPEN (3 << 2)
#define ME_ALL_EXTENTS_OPEN_CLOSED (4 << 2)
#define ME_ALL_EXTENTS_MASK (7 << 2)
#define ME_START_IN_REGION (1 << 5)
#define ME_END_IN_REGION (2 << 5)
#define ME_START_AND_END_IN_REGION (3 << 5)
#define ME_START_OR_END_IN_REGION (4 << 5)
#define ME_IN_REGION_MASK (7 << 5)
#define ME_NEGATE_IN_REGION (1 << 8)
/* the following flags are internal-only */
#define ME_INCLUDE_INTERNAL (1 << 9)
#define ME_MIGHT_THROW (1 << 10)
#define ME_MIGHT_MODIFY_TEXT (1 << 11)
#define ME_MIGHT_MODIFY_EXTENTS (1 << 12)
#define ME_MIGHT_MOVE_SOE (1 << 13)
#define ME_MIGHT_CALL_ELISP (ME_MIGHT_THROW | ME_MIGHT_MODIFY_TEXT | \
			     ME_MIGHT_MODIFY_EXTENTS | ME_MIGHT_MOVE_SOE)

#define EXTENT_LIVE_P(e)	(!EQ (extent_object (e), Qt))

#define CHECK_LIVE_EXTENT(x) do {			\
  CHECK_EXTENT (x);					\
  if (!EXTENT_LIVE_P (XEXTENT (x)))			\
    dead_wrong_type_argument (Qextent_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_EXTENT(x) do {			\
  CONCHECK_EXTENT (x);					\
  if (!EXTENT_LIVE_P (XEXTENT (x)))			\
    x = wrong_type_argument (Qextent_live_p, (x));	\
} while (0)

EXFUN(Fdetach_extent, 1);
EXFUN(Fextent_end_position, 1);
EXFUN(Fextent_object, 1);
EXFUN(Fextent_start_position, 1);
EXFUN(Fmake_extent, 3);
EXFUN(Fnext_single_property_change, 4);
EXFUN(Fprevious_single_property_change, 4);
EXFUN(Fset_extent_endpoints, 4);
EXFUN(Fnext_extent_change, 2);
EXFUN(Fprevious_extent_change, 2);
EXFUN(Fset_extent_parent, 2);
EXFUN(Fget_char_property, 4);

extern int inside_undo;
extern int in_modeline_generation;

struct extent_fragment *extent_fragment_new(Lisp_Object buffer_or_string,
					    struct frame *frm);
face_index extent_fragment_update(struct window *w, struct extent_fragment *ef,
				  /* Note this is in Bytinds */
				  Bytind pos, Lisp_Object last_glyph);
void extent_fragment_delete(struct extent_fragment *ef);

#ifdef emacs			/* things other than emacs want the structs */

/* from alloc.c */
struct extent *allocate_extent(void);

/* from extents.c */
EXTENT extent_ancestor_1(EXTENT e);

/* extent_ancestor() chases all the parent links until there aren't any
   more.  extent_ancestor_1() does the same thing but it a function;
   the following optimizes the most common case. */
extern_inline EXTENT extent_ancestor(EXTENT e);
extern_inline EXTENT extent_ancestor(EXTENT e)
{
	return e->flags.has_parent ? extent_ancestor_1(e) : e;
}

void allocate_extent_auxiliary(EXTENT ext);
void init_buffer_extents(struct buffer *b);
void uninit_buffer_extents(struct buffer *b);
typedef int (*map_extents_fun) (EXTENT extent, void *arg);
void map_extents(Bufpos from, Bufpos to, map_extents_fun fn,
		 void *arg, Lisp_Object obj, EXTENT after, unsigned int flags);

/* Note the following five functions are NOT in Bufpos's */
void adjust_extents(Lisp_Object object, Memind from, Memind to, int amount);
void adjust_extents_for_deletion(Lisp_Object object, Bytind from,
				 Bytind to, int gapsize,
				 int numdel, int movegapsize);
void verify_extent_modification(Lisp_Object object, Bytind from,
				Bytind to, Lisp_Object inhibit_read_only_value);
void process_extents_for_insertion(Lisp_Object object,
				   Bytind opoint, Bytecount length);
void process_extents_for_deletion(Lisp_Object object, Bytind from,
				  Bytind to, int destroy_them);
void report_extent_modification(Lisp_Object, Bufpos, Bufpos, int);

void set_extent_glyph(EXTENT extent, Lisp_Object glyph, int endp,
		      glyph_layout layout);

void add_string_extents(Lisp_Object string, struct buffer *buf,
			Bytind opoint, Bytecount length);
void splice_in_string_extents(Lisp_Object string, struct buffer *buf,
			      Bytind opoint, Bytecount length, Bytecount pos);
void copy_string_extents(Lisp_Object new_string,
			 Lisp_Object old_string,
			 Bytecount new_pos, Bytecount old_pos,
			 Bytecount length);

void detach_all_extents(Lisp_Object object);
void set_extent_endpoints(EXTENT extent, Bytind s, Bytind e,
			  Lisp_Object object);

#ifdef ERROR_CHECK_EXTENTS
void sledgehammer_extent_check(Lisp_Object obj);
#endif

#ifdef MEMORY_USAGE_STATS
int compute_buffer_extent_usage(struct buffer *b,
				struct overhead_stats *ovstats);
#endif

#endif				/* emacs */

#endif				/* INCLUDED_extents_h_ */
