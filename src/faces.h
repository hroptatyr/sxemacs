/* Face data structures.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing

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

#ifndef INCLUDED_faces_h_
#define INCLUDED_faces_h_

#include "buffer.h"		/* for NUM_LEADING_BYTES */

/* a Lisp_Face is the C object corresponding to a face.  There is one
   of these per face.  It basically contains all of the specifiers for
   the built-in face properties, plus the plist of user-specified
   properties. */

struct Lisp_Face {
	struct lcrecord_header header;

	Lisp_Object name;
	Lisp_Object doc_string;
	unsigned int dirty:1;	/* Set whenever a face property is changed on
				   a face. */

	Lisp_Object foreground;
	Lisp_Object background;
	Lisp_Object font;

	Lisp_Object display_table;
	Lisp_Object background_pixmap;

	Lisp_Object underline;
	Lisp_Object strikethru;
	Lisp_Object highlight;
	Lisp_Object dim;
	Lisp_Object blinking;
	Lisp_Object reverse;

	Lisp_Object plist;

	Lisp_Object charsets_warned_about;
};

/*

   A face cache element caches the results of instantiating the
   properties of a face in a particular window. (Instantiation can
   take a long time so this is very important.) Each window contains
   an array of face cache elements (called the "face cache"), one for
   each face that has been seen in the window so far.

   Some tricky stuff is done to make sure the face cache does not
   become inconsistent:

   1) Switching buffers in a window clears the face cache for that
      window, because this can change the way any property is
      instantiated in the window.
   2) Setting a face property causes that face to be marked as
      dirty.  This causes various stuff to happen to make sure
      the appropriate face cache elements are invalidated.
      (#### Actually this doesn't work quite right, and errs
      too much on the side of invalidating unnecessary stuff.)

   There are also face cache elements for "merged faces", which are the
   result of merging all the faces that overlap a particular buffer
   position.  The merging is always done in the context of a particular
   domain (specifically, a window), and the face cache element is
   specific to a particular window. (Face cache elements are contained
   in an array that is attached to each struct_window.) The reason that
   the merging takes place in the context of a particular window has
   to do with the way the merging works:

   1) All extents overlying the buffer position are sorted by descending
      priority.
   2) The property of a particular merged face comes from the highest-
      priority face that specifies a value for that particular property.
   3) The way to determine whether a face specifies a value for a
      particular property is to instantiate that face's property in
      the window in question with the no-fallback option set, to
      see if we got anything.

   For Mule, things get a bit trickier because there can be multiple
   fonts per face/window combination -- the charset is an argument
   to specifier-instance.

   We have two possible data structure representations:

   1) Keep the original "one font per face cache element" representation
      and use a different face cache element for each charset.
   2) Allow multiple fonts to be in a single face cache element.

   I've chosen to use (2) -- the data structure gets more complicated
   but the algorithms for maintaining face cache elements end up
   simpler.
 */

#define NUM_STATIC_CACHEL_FACES 4

typedef struct face_cachel face_cachel;
struct face_cachel {
	/* There are two kinds of cachels; those created from a single face
	   and those created by merging more than one face.  In the former
	   case, the FACE element specifies the face used.  In the latter
	   case, the MERGED_FACES_STATIC and MERGED_FACES elements specify
	   the faces used for merging by giving the indices of the
	   corresponding single-face cachels.

	   Formerly we didn't bother to keep track of the faces used for
	   merging.  We do now because we need to do so because there is no
	   other way to properly handle multiple charsets for Mule in the
	   presence of display tables short of always computing the values
	   for all charsets, which is very expensive.  Instead, we use a
	   lazy scheme where we only compute the font for a particular charset
	   when it is needed. (The exception is the font for the ASCII charset.
	   We always compute it, just like the other attributes, because
	   many places in the C code refer to the font of the ASCII charset
	   and expect it to always be there.)

	   We store the first four faces in a static array, and use a
	   Dynarr for the rest.  This has the advantage that the space used
	   is small but the Dynarr will almost never be created, so we
	   won't spend much time in malloc()/free().

	   The order of the faces here is decreasing extent priority. */
	Lisp_Object face;
	int merged_faces_static[NUM_STATIC_CACHEL_FACES];
	int_dynarr *merged_faces;
	int nfaces;

	/* The values stored here are computed by calling specifier_instance()
	   on the appropriate specifiers.  This means that we will have either
	   a value computed from the face itself or a value computed from the
	   default face.  We need to distinguish the two so that merging works
	   properly -- a value that comes from the default face is treated
	   as "unspecified" during merging and is overridden by lower-priority
	   faces.  This is what the _specified flags below are for. */

	Lisp_Object foreground;
	Lisp_Object background;
	/* There are currently 128 possible charsets under Mule.  For the
	   moment we just take the easy way out and allocate space for each
	   of them.  This avoids messing with Dynarrs.

	   #### We should look into this and probably clean it up
	   to use Dynarrs.  This may be a big space hog as is. */
	Lisp_Object font[NUM_LEADING_BYTES];

	Lisp_Object display_table;
	Lisp_Object background_pixmap;

	unsigned int underline:1;
	unsigned int strikethru:1;
	unsigned int highlight:1;
	unsigned int dim:1;
	unsigned int blinking:1;
	unsigned int reverse:1;

	/* Used when merging to tell if the above field represents an actual
	   value of this face or a fallback value. */
	/* #### Of course we should use a bit array or something. */
	unsigned char font_specified[NUM_LEADING_BYTES];
	unsigned int foreground_specified:1;
	unsigned int background_specified:1;
	unsigned int display_table_specified:1;
	unsigned int background_pixmap_specified:1;

	unsigned int strikethru_specified:1;
	unsigned int underline_specified:1;
	unsigned int highlight_specified:1;
	unsigned int dim_specified:1;
	unsigned int blinking_specified:1;
	unsigned int reverse_specified:1;

	/* The updated flag is set after we calculate the values for the
	   face cachel and cleared whenever a face changes, to indicate
	   that the values stored here might be wrong.  The next time
	   we go to access the values, we recompute them; if any values
	   change, we set the DIRTY flag, which tells the output routines
	   that a face value has in fact changed and the sections of text
	   using this face need to be redrawn.

	   It is trickier with fonts because we don't automatically
	   recompute the fonts but do it only when it is necessary.
	   (The ASCII font is an exception, of course; see above).

	   In the case of fonts, we maintain a separate updated flag
	   for each font.  Whenever we need to access the font for
	   a particular charset, we recalculate it if either its
	   value is Qunbound (meaning it's never been computed at all)
	   or the updated flag is not set.  We set the dirty flag if
	   the value is not the same as before and the previous value
	   was not Qunbound.

	   #### Note that we don't yet deal with the case of the new
	   value being Qunbound, as could happen if no fonts of the
	   right sort are available on the system.  In this case, the
	   whole program will just crash.  For the moment, this is
	   OK (for debugging purposes) but we should fix this by
	   storing a "blank font" if the instantiation fails. */
	unsigned int dirty:1;
	unsigned int updated:1;
	/* #### Of course we should use a bit array or something. */
	unsigned char font_updated[NUM_LEADING_BYTES];
};

DECLARE_LRECORD(face, Lisp_Face);
#define XFACE(x) XRECORD (x, face, Lisp_Face)
#define XSETFACE(x, p) XSETRECORD (x, p, face)
#define FACEP(x) RECORDP (x, face)
#define CHECK_FACE(x) CHECK_RECORD (x, face)

Lisp_Object ensure_face_cachel_contains_charset(struct face_cachel *cachel,
						Lisp_Object domain,
						Lisp_Object charset);
void ensure_face_cachel_complete(struct face_cachel *cachel,
				 Lisp_Object domain, unsigned char *charsets);
void update_face_cachel_data(struct face_cachel *cachel,
			     Lisp_Object domain, Lisp_Object face);
void face_cachel_charset_font_metric_info(struct face_cachel *cachel,
					  unsigned char *charsets,
					  struct font_metric_info *fm);
void mark_face_cachels(face_cachel_dynarr * elements);
void mark_face_cachels_as_clean(struct window *w);
void mark_face_cachels_as_not_updated(struct window *w);
void reset_face_cachel(struct face_cachel *inst);
void reset_face_cachels(struct window *w);
face_index get_builtin_face_cache_index(struct window *w, Lisp_Object face);
#ifdef MEMORY_USAGE_STATS
int compute_face_cachel_usage(face_cachel_dynarr * face_cachels,
			      struct overhead_stats *ovstats);
#endif				/* MEMORY_USAGE_STATS */

EXFUN(Fface_name, 1);
EXFUN(Ffind_face, 1);
EXFUN(Fget_face, 1);

extern Lisp_Object Qstrikethru, Vbuilt_in_face_specifiers, Vdefault_face;
extern Lisp_Object Vleft_margin_face, Vpointer_face, Vright_margin_face;
extern Lisp_Object Vtext_cursor_face, Vvertical_divider_face;
extern Lisp_Object Vtoolbar_face, Vgui_element_face, Vwidget_face;

void mark_all_faces_as_clean(void);
void init_frame_faces(struct frame *f);
void init_device_faces(struct device *d);
void init_global_faces(struct device *d);
face_index get_extent_fragment_face_cache_index(struct window *w,
						struct extent_fragment *ef);
void update_frame_face_values(struct frame *f);
void face_property_was_changed(Lisp_Object face, Lisp_Object property,
			       Lisp_Object locale);
void default_face_font_info(Lisp_Object domain, int *ascent,
			    int *descent, int *height, int *width,
			    int *proportional_p);
void default_face_height_and_width(Lisp_Object domain, int *height, int *width);
void default_face_height_and_width_1(Lisp_Object domain,
				     int *height, int *width);

#define FACE_CACHEL_FONT(cachel, charset) \
  (cachel->font[XCHARSET_LEADING_BYTE (charset) - 128])

#define WINDOW_FACE_CACHEL(window, index) \
  Dynarr_atp ((window)->face_cachels, index)

#define FACE_CACHEL_FINDEX_UNSAFE(cachel, offset)			     \
  ((offset) < NUM_STATIC_CACHEL_FACES					     \
   ? (cachel)->merged_faces_static[offset]				     \
   : Dynarr_at ((cachel)->merged_faces, (offset) - NUM_STATIC_CACHEL_FACES))

#define WINDOW_FACE_CACHEL_FACE(window, index)				\
  (WINDOW_FACE_CACHEL (window, index)->face)
#define WINDOW_FACE_CACHEL_FOREGROUND(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->foreground)
#define WINDOW_FACE_CACHEL_BACKGROUND(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->background)
/* #### This can be referenced by various functions,
   but face_cachels isn't initialized for the stream device.
   Since it doesn't need the value we just return nil here to avoid
   blowing up in multiple places. */
#define WINDOW_FACE_CACHEL_FONT(window, index, charset)			\
  ((window)->face_cachels					\
   ? FACE_CACHEL_FONT (WINDOW_FACE_CACHEL (window, index), charset)	\
   : Qnil)
#define WINDOW_FACE_CACHEL_DISPLAY_TABLE(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->display_table)
#define WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP(window, index)		\
  (WINDOW_FACE_CACHEL (window, index)->background_pixmap)
#define WINDOW_FACE_CACHEL_DIRTY(window, index)				\
  (WINDOW_FACE_CACHEL (window, index)->dirty)
#define WINDOW_FACE_CACHEL_UNDERLINE_P(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->underline)
#define WINDOW_FACE_CACHEL_HIGHLIGHT_P(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->highlight)
#define WINDOW_FACE_CACHEL_DIM_P(window, index)				\
  (WINDOW_FACE_CACHEL (window, index)->dim)
#define WINDOW_FACE_CACHEL_BLINKING_P(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->blinking)
#define WINDOW_FACE_CACHEL_REVERSE_P(window, index)			\
  (WINDOW_FACE_CACHEL (window, index)->reverse)

#define FACE_PROPERTY_SPECIFIER(face, property) Fget (face, property, Qnil)

#define FACE_PROPERTY_INSTANCE_1(face, property, domain, errb, no_fallback, depth)	\
  specifier_instance (FACE_PROPERTY_SPECIFIER (face, property), Qunbound, \
		      domain, errb, 1, no_fallback, depth)

#define FACE_PROPERTY_INSTANCE(face, property, domain, no_fallback, depth)	\
  FACE_PROPERTY_INSTANCE_1 (face, property, domain, ERROR_ME_NOT, \
			    no_fallback, depth)

Lisp_Object face_property_matching_instance(Lisp_Object face,
					    Lisp_Object property,
					    Lisp_Object charset,
					    Lisp_Object domain,
					    Error_behavior errb,
					    int no_fallback, Lisp_Object depth);

#define FACE_PROPERTY_SPEC_LIST(face, property, locale)			\
  Fspecifier_spec_list (FACE_PROPERTY_SPECIFIER (face, property),	\
			locale, Qnil, Qnil)
#define SET_FACE_PROPERTY(face, property, value, locale, tag, how_to_add) \
  Fadd_spec_to_specifier (FACE_PROPERTY_SPECIFIER (face, property),	\
			  value, locale, tag, how_to_add)

#define FACE_FOREGROUND(face, domain)					\
  FACE_PROPERTY_INSTANCE (face, Qforeground, domain, 0, Qzero)
#define FACE_BACKGROUND(face, domain)					\
  FACE_PROPERTY_INSTANCE (face, Qbackground, domain, 0, Qzero)
#define FACE_FONT(face, domain, charset)				\
  face_property_matching_instance (face, Qfont, charset, domain,	\
				   ERROR_ME_NOT, 0, Qzero)
#define FACE_DISPLAY_TABLE(face, domain)				\
  FACE_PROPERTY_INSTANCE (face, Qdisplay_table, domain, 0, Qzero)
#define FACE_BACKGROUND_PIXMAP(face, domain)				\
  FACE_PROPERTY_INSTANCE (face, Qbackground_pixmap, domain, 0, Qzero)
#define FACE_UNDERLINE_P(face, domain)					\
  (!NILP (FACE_PROPERTY_INSTANCE (face, Qunderline, domain, 0, Qzero)))
#define FACE_STRIKETHRU_P(face, domain)					\
  (!NILP (FACE_PROPERTY_INSTANCE (face, Qstrikethru, domain, 0, Qzero)))
#define FACE_HIGHLIGHT_P(face, domain)					\
  (!NILP (FACE_PROPERTY_INSTANCE (face, Qhighlight, domain, 0, Qzero)))
#define FACE_DIM_P(face, domain)					\
  (!NILP (FACE_PROPERTY_INSTANCE (face, Qdim, domain, 0, Qzero)))
#define FACE_BLINKING_P(face, domain)					\
  (!NILP (FACE_PROPERTY_INSTANCE (face, Qblinking, domain, 0, Qzero)))
#define FACE_REVERSE_P(face, domain)					\
  (!NILP (FACE_PROPERTY_INSTANCE (face, Qreverse, domain, 0, Qzero)))

#endif				/* INCLUDED_faces_h_ */
