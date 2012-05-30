/* Generic glyph/image implementation + display tables
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996, 2000 Ben Wing
   Copyright (C) 1995 Sun Microsystems
   Copyright (C) 1998, 1999, 2000 Andy Piper

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

/* Written by Ben Wing and Chuck Thompson. Heavily modified /
   rewritten by Andy Piper. */

#include <config.h>
#include "lisp.h"

#include <mem/blocktype.h>
#include "buffer.h"
#include "chartab.h"
#include "device.h"
#include "elhash.h"
#include "faces.h"
#include "frame.h"
#include "glyphs.h"
#include "insdel.h"
#include "objects.h"
#include "opaque.h"
#include "rangetab.h"
#include "redisplay.h"
#include "specifier.h"
#include "window.h"
#include <ent/ent-float.h>

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

Lisp_Object Qimage_conversion_error;

Lisp_Object Qglyphp, Qcontrib_p, Qbaseline;
Lisp_Object Qbuffer_glyph_p, Qpointer_glyph_p, Qicon_glyph_p;
Lisp_Object Qnothing_image_instance_p, Qtext_image_instance_p;
Lisp_Object Qmono_pixmap_image_instance_p;
Lisp_Object Qcolor_pixmap_image_instance_p;
Lisp_Object Qpointer_image_instance_p;
Lisp_Object Qsubwindow_image_instance_p;
Lisp_Object Qwidget_image_instance_p;
Lisp_Object Qconst_glyph_variable;
Lisp_Object Qmono_pixmap, Qcolor_pixmap, Qsubwindow;
Lisp_Object Q_file, Q_data, Q_face, Q_pixel_width, Q_pixel_height;
Lisp_Object Qformatted_string;
Lisp_Object Vcurrent_display_table;
Lisp_Object Vtruncation_glyph, Vcontinuation_glyph, Voctal_escape_glyph;
Lisp_Object Vcontrol_arrow_glyph, Vinvisible_text_glyph, Vhscroll_glyph;
Lisp_Object Vsxemacs_logo;
Lisp_Object Vthe_nothing_vector;
Lisp_Object Vimage_instantiator_format_list;
Lisp_Object Vimage_instance_type_list;
Lisp_Object Vglyph_type_list;

int disable_animated_pixmaps;

DEFINE_IMAGE_INSTANTIATOR_FORMAT(nothing);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(inherit);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(string);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(formatted_string);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(subwindow);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(text);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(pointer);

Lisp_Object Q_mask_file, Q_mask_data, Q_hotspot_x, Q_hotspot_y;
Lisp_Object Q_foreground, Q_background;

#ifdef HAVE_WINDOW_SYSTEM
DEFINE_IMAGE_INSTANTIATOR_FORMAT(xbm);
Lisp_Object Qxbm;

#ifndef BitmapSuccess
#define BitmapSuccess           0
#define BitmapOpenFailed        1
#define BitmapFileInvalid       2
#define BitmapNoMemory          3
#endif
#endif

#ifdef HAVE_XFACE
DEFINE_IMAGE_INSTANTIATOR_FORMAT(xface);
Lisp_Object Qxface;
#endif

#ifdef HAVE_XPM
DEFINE_IMAGE_INSTANTIATOR_FORMAT(xpm);
Lisp_Object Qxpm;
Lisp_Object Q_color_symbols;
#endif

typedef struct image_instantiator_format_entry image_instantiator_format_entry;
struct image_instantiator_format_entry {
	Lisp_Object symbol;
	Lisp_Object device;
	struct image_instantiator_methods *meths;
};

typedef struct {
	Dynarr_declare(struct image_instantiator_format_entry);
} image_instantiator_format_entry_dynarr;

/* This contains one entry per format, per device it's defined on. */
image_instantiator_format_entry_dynarr
    *the_image_instantiator_format_entry_dynarr;

static Lisp_Object allocate_image_instance(Lisp_Object governing_domain,
					   Lisp_Object parent,
					   Lisp_Object instantiator);
static void image_validate(Lisp_Object instantiator);
static void glyph_property_was_changed(Lisp_Object glyph,
				       Lisp_Object property,
				       Lisp_Object locale);
static void set_image_instance_dirty_p(Lisp_Object instance, int dirty);
static void register_ignored_expose(struct frame *f, int x, int y, int width,
				    int height);
static void cache_subwindow_instance_in_frame_maybe(Lisp_Object instance);
static void update_image_instance(Lisp_Object image_instance,
				  Lisp_Object instantiator);
/* Unfortunately windows and X are different. In windows BeginPaint()
   will prevent WM_PAINT messages being generated so it is unnecessary
   to register exposures as they will not occur. Under X they will
   always occur. */
int hold_ignored_expose_registration;

EXFUN(Fimage_instance_type, 1);
EXFUN(Fglyph_type, 1);
EXFUN(Fnext_window, 4);

/****************************************************************************
 *                          Image Instantiators                             *
 ****************************************************************************/

struct image_instantiator_methods *decode_device_ii_format(Lisp_Object device,
							   Lisp_Object format,
							   Error_behavior errb)
{
	int i;

	if (!SYMBOLP(format)) {
		if (ERRB_EQ(errb, ERROR_ME))
			CHECK_SYMBOL(format);
		return 0;
	}

	for (i = 0;
	     i < Dynarr_length(the_image_instantiator_format_entry_dynarr);
	     i++) {
		if (EQ
		    (format,
		     Dynarr_at(the_image_instantiator_format_entry_dynarr,
			       i).symbol)) {
			Lisp_Object d =
			    Dynarr_at
			    (the_image_instantiator_format_entry_dynarr,
			     i).device;
			if ((NILP(d) && NILP(device))
			    ||
			    (!NILP(device) &&
			     EQ(CONSOLE_TYPE(XCONSOLE
					     (DEVICE_CONSOLE(XDEVICE(device)))),
				d)))
				return
				    Dynarr_at
				    (the_image_instantiator_format_entry_dynarr,
				     i).meths;
		}
	}

	maybe_signal_simple_error("Invalid image-instantiator format", format,
				  Qimage, errb);

	return 0;
}

struct image_instantiator_methods *decode_image_instantiator_format(Lisp_Object
								    format,
								    Error_behavior
								    errb)
{
	return decode_device_ii_format(Qnil, format, errb);
}

static int
valid_image_instantiator_format_p(Lisp_Object format, Lisp_Object locale)
{
	int i;
	struct image_instantiator_methods *meths =
	    decode_image_instantiator_format(format, ERROR_ME_NOT);
	Lisp_Object contype = Qnil;
	/* mess with the locale */
	if (!NILP(locale) && SYMBOLP(locale))
		contype = locale;
	else {
		struct console *console = decode_console(locale);
		contype = console ? CONSOLE_TYPE(console) : locale;
	}
	/* nothing is valid in all locales */
	if (EQ(format, Qnothing))
		return 1;
	/* reject unknown formats */
	else if (NILP(contype) || !meths)
		return 0;

	for (i = 0; i < Dynarr_length(meths->consoles); i++)
		if (EQ(contype, Dynarr_at(meths->consoles, i).symbol))
			return 1;
	return 0;
}

DEFUN("valid-image-instantiator-format-p", Fvalid_image_instantiator_format_p, 1, 2, 0,	/*
Given an IMAGE-INSTANTIATOR-FORMAT, return non-nil if it is valid.
If LOCALE is non-nil then the format is checked in that locale.
If LOCALE is nil the current console is used.

Valid formats are some subset of 'nothing, 'string, 'formatted-string,
'xpm, 'xbm, 'xface, 'gif, 'jpeg, 'png, 'tiff, 'cursor-font, 'font,
'autodetect, 'subwindow, 'inherit, 'mswindows-resource, 'bmp,
'native-layout, 'layout, 'label, 'tab-control, 'tree-view,
'progress-gauge, 'scrollbar, 'combo-box, 'edit-field, 'button,
'widget, 'pointer, and 'text, depending on how SXEmacs was compiled.
*/
      (image_instantiator_format, locale))
{
	return valid_image_instantiator_format_p(image_instantiator_format,
						 locale) ? Qt : Qnil;
}

DEFUN("image-instantiator-format-list", Fimage_instantiator_format_list, 0, 0, 0,	/*
Return a list of valid image-instantiator formats.
*/
      ())
{
	return Fcopy_sequence(Vimage_instantiator_format_list);
}

void
add_entry_to_device_ii_format_list(Lisp_Object device, Lisp_Object symbol,
				   struct image_instantiator_methods *meths)
{
	struct image_instantiator_format_entry entry;

	entry.symbol = symbol;
	entry.device = device;
	entry.meths = meths;
	Dynarr_add(the_image_instantiator_format_entry_dynarr, entry);
	if (NILP(memq_no_quit(symbol, Vimage_instantiator_format_list)))
		Vimage_instantiator_format_list =
		    Fcons(symbol, Vimage_instantiator_format_list);
}

void add_entry_to_image_instantiator_format_list(Lisp_Object symbol, struct
						 image_instantiator_methods
						 *meths)
{
	add_entry_to_device_ii_format_list(Qnil, symbol, meths);
}

static Lisp_Object *get_image_conversion_list(Lisp_Object console_type)
{
	return &decode_console_type(console_type,
				    ERROR_ME)->image_conversion_list;
}

DEFUN("set-console-type-image-conversion-list", Fset_console_type_image_conversion_list, 2, 2, 0,	/*
Set the image-conversion-list for consoles of the given CONSOLE-TYPE.
The image-conversion-list specifies how image instantiators that
are strings should be interpreted.  Each element of the list should be
a list of two elements (a regular expression string and a vector) or
a list of three elements (the preceding two plus an integer index into
the vector).  The string is converted to the vector associated with the
first matching regular expression.  If a vector index is specified, the
string itself is substituted into that position in the vector.

Note: The conversion above is applied when the image instantiator is
added to an image specifier, not when the specifier is actually
instantiated.  Therefore, changing the image-conversion-list only affects
newly-added instantiators.  Existing instantiators in glyphs and image
specifiers will not be affected.
*/
      (console_type, list))
{
	Lisp_Object tail;
	Lisp_Object *imlist = get_image_conversion_list(console_type);

	/* Check the list to make sure that it only has valid entries. */

	EXTERNAL_LIST_LOOP(tail, list) {
		Lisp_Object mapping = XCAR(tail);

		/* Mapping form should be (STRING VECTOR) or (STRING VECTOR INTEGER) */
		if (!CONSP(mapping) ||
		    !CONSP(XCDR(mapping)) ||
		    (!NILP(XCDR(XCDR(mapping))) &&
		     (!CONSP(XCDR(XCDR(mapping))) ||
		      !NILP(XCDR(XCDR(XCDR(mapping)))))))
			signal_simple_error("Invalid mapping form", mapping);
		else {
			Lisp_Object mapexp = XCAR(mapping);
			Lisp_Object typevec = XCAR(XCDR(mapping));
			Lisp_Object pos = Qnil;
			Lisp_Object newvec;
			struct gcpro gcpro1;

			CHECK_STRING(mapexp);
			CHECK_VECTOR(typevec);
			if (!NILP(XCDR(XCDR(mapping)))) {
				pos = XCAR(XCDR(XCDR(mapping)));
				CHECK_INT(pos);
				if (XINT(pos) < 0 ||
				    XINT(pos) >= XVECTOR_LENGTH(typevec))
					args_out_of_range_3
					    (pos, Qzero,
					     make_int(XVECTOR_LENGTH(typevec) -
						      1));
			}

			newvec = Fcopy_sequence(typevec);
			if (INTP(pos))
				XVECTOR_DATA(newvec)[XINT(pos)] = mapexp;
			GCPRO1(newvec);
			image_validate(newvec);
			UNGCPRO;
		}
	}

	*imlist = Fcopy_tree(list, Qt);
	return list;
}

DEFUN("console-type-image-conversion-list", Fconsole_type_image_conversion_list, 1, 1, 0,	/*
Return the image-conversion-list for devices of the given CONSOLE-TYPE.
The image-conversion-list specifies how to interpret image string
instantiators for the specified console type.  See
`set-console-type-image-conversion-list' for a description of its syntax.
*/
      (console_type))
{
	return Fcopy_tree(*get_image_conversion_list(console_type), Qt);
}

/* Process a string instantiator according to the image-conversion-list for
   CONSOLE_TYPE.  Returns a vector. */

static Lisp_Object
process_image_string_instantiator(Lisp_Object data,
				  Lisp_Object console_type, int dest_mask)
{
	Lisp_Object tail;

	LIST_LOOP(tail, *get_image_conversion_list(console_type)) {
		Lisp_Object mapping = XCAR(tail);
		Lisp_Object mapexp = XCAR(mapping);
		Lisp_Object typevec = XCAR(XCDR(mapping));

		/* if the result is of a type that can't be instantiated
		   (e.g. a string when we're dealing with a pointer glyph),
		   skip it. */
		if (!(dest_mask &
		      IIFORMAT_METH(decode_image_instantiator_format
				    (INSTANTIATOR_TYPE(typevec), ERROR_ME),
				    possible_dest_types, ())))
			continue;
		if (fast_string_match(mapexp, 0, data, 0, -1, 0, ERROR_ME, 0) >=
		    0) {
			if (!NILP(XCDR(XCDR(mapping)))) {
				int pos = XINT(XCAR(XCDR(XCDR(mapping))));
				Lisp_Object newvec = Fcopy_sequence(typevec);
				XVECTOR_DATA(newvec)[pos] = data;
				return newvec;
			} else
				return typevec;
		}
	}

	/* Oh well. */
	signal_simple_error("Unable to interpret glyph instantiator", data);

	return Qnil;
}

Lisp_Object
find_keyword_in_vector_or_given(Lisp_Object vector, Lisp_Object keyword,
				Lisp_Object default_)
{
	Lisp_Object *elt;
	int instantiator_len;

	elt = XVECTOR_DATA(vector);
	instantiator_len = XVECTOR_LENGTH(vector);

	elt++;
	instantiator_len--;

	while (instantiator_len > 0) {
		if (EQ(elt[0], keyword))
			return elt[1];
		elt += 2;
		instantiator_len -= 2;
	}

	return default_;
}

Lisp_Object find_keyword_in_vector(Lisp_Object vector, Lisp_Object keyword)
{
	return find_keyword_in_vector_or_given(vector, keyword, Qnil);
}

static Lisp_Object
find_instantiator_differences(Lisp_Object new, Lisp_Object old)
{
	Lisp_Object alist = Qnil;
	Lisp_Object *elt = XVECTOR_DATA(new);
	Lisp_Object *old_elt = XVECTOR_DATA(old);
	int len = XVECTOR_LENGTH(new);
	struct gcpro gcpro1;

	/* If the vector length has changed then consider everything
	   changed. We could try and figure out what properties have
	   disappeared or been added, but this code is only used as an
	   optimization anyway so lets not bother. */
	if (len != XVECTOR_LENGTH(old))
		return new;

	GCPRO1(alist);

	for (len -= 2; len >= 1; len -= 2) {
		/* Keyword comparisons can be done with eq, the value must be
		   done with equal.
		   #### Note that this does not optimize re-ordering. */
		if (!EQ(elt[len], old_elt[len])
		    || !internal_equal(elt[len + 1], old_elt[len + 1], 0))
			alist = Fcons(Fcons(elt[len], elt[len + 1]), alist);
	}

	{
		Lisp_Object result = alist_to_tagged_vector(elt[0], alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}
}

DEFUN("set-instantiator-property", Fset_instantiator_property, 3, 3, 0,	/*
Destructively set the property KEYWORD of INSTANTIATOR to VALUE.
If the property is not set then it is added to a copy of the
instantiator and the new instantiator returned.
Use `set-glyph-image' on glyphs to register instantiator changes.
*/
      (instantiator, keyword, value))
{
	Lisp_Object *elt;
	int len;

	CHECK_VECTOR(instantiator);
	if (!KEYWORDP(keyword))
		signal_simple_error("instantiator property must be a keyword",
				    keyword);

	elt = XVECTOR_DATA(instantiator);
	len = XVECTOR_LENGTH(instantiator);

	for (len -= 2; len >= 1; len -= 2) {
		if (EQ(elt[len], keyword)) {
			elt[len + 1] = value;
			break;
		}
	}

	/* Didn't find it so add it. */
	if (len < 1) {
		Lisp_Object alist = Qnil, result;
		struct gcpro gcpro1;

		GCPRO1(alist);
		alist = tagged_vector_to_alist(instantiator);
		alist = Fcons(Fcons(keyword, value), alist);
		result = alist_to_tagged_vector(elt[0], alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}

	return instantiator;
}

void check_valid_string(Lisp_Object data)
{
	CHECK_STRING(data);
}

void check_valid_vector(Lisp_Object data)
{
	CHECK_VECTOR(data);
}

void check_valid_face(Lisp_Object data)
{
	Fget_face(data);
}

void check_valid_int(Lisp_Object data)
{
	CHECK_INT(data);
}

void file_or_data_must_be_present(Lisp_Object instantiator)
{
	if (NILP(find_keyword_in_vector(instantiator, Q_file)) &&
	    NILP(find_keyword_in_vector(instantiator, Q_data)))
		signal_simple_error("Must supply either :file or :data",
				    instantiator);
}

void data_must_be_present(Lisp_Object instantiator)
{
	if (NILP(find_keyword_in_vector(instantiator, Q_data)))
		signal_simple_error("Must supply :data", instantiator);
}

static void face_must_be_present(Lisp_Object instantiator)
{
	if (NILP(find_keyword_in_vector(instantiator, Q_face)))
		signal_simple_error("Must supply :face", instantiator);
}

/* utility function useful in retrieving data from a file. */

Lisp_Object make_string_from_file(Lisp_Object file)
{
	/* This function can call lisp */
	int count = specpdl_depth();
	Lisp_Object temp_buffer;
	struct gcpro gcpro1;
	Lisp_Object data;

	specbind(Qinhibit_quit, Qt);
	record_unwind_protect(Fset_buffer, Fcurrent_buffer());
	temp_buffer = Fget_buffer_create(build_string(" *pixmap conversion*"));
	GCPRO1(temp_buffer);
	set_buffer_internal(XBUFFER(temp_buffer));
	Ferase_buffer(Qnil);
	specbind(intern("format-alist"), Qnil);
	Finsert_file_contents_internal(file, Qnil, Qnil, Qnil, Qnil, Qnil,
				       Qnil);
	data = Fbuffer_substring(Qnil, Qnil, Qnil);
	unbind_to(count, Qnil);
	UNGCPRO;
	return data;
}

/* The following two functions are provided to make it easier for
   the normalize methods to work with keyword-value vectors.
   Hash tables are kind of heavyweight for this purpose.
   (If vectors were resizable, we could avoid this problem;
   but they're not.) An alternative approach that might be
   more efficient but require more work is to use a type of
   assoc-Dynarr and provide primitives for deleting elements out
   of it. (However, you'd also have to add an unwind-protect
   to make sure the Dynarr got freed in case of an error in
   the normalization process.) */

Lisp_Object tagged_vector_to_alist(Lisp_Object vector)
{
	Lisp_Object *elt = XVECTOR_DATA(vector);
	int len = XVECTOR_LENGTH(vector);
	Lisp_Object result = Qnil;

	assert(len & 1);
	for (len -= 2; len >= 1; len -= 2)
		result = Fcons(Fcons(elt[len], elt[len + 1]), result);

	return result;
}

Lisp_Object alist_to_tagged_vector(Lisp_Object tag, Lisp_Object alist)
{
	int len = 1 + 2 * XINT(Flength(alist));
	Lisp_Object *elt = alloca_array(Lisp_Object, len);
	int i;
	Lisp_Object rest;

	i = 0;
	elt[i++] = tag;
	LIST_LOOP(rest, alist) {
		Lisp_Object pair = XCAR(rest);
		elt[i] = XCAR(pair);
		elt[i + 1] = XCDR(pair);
		i += 2;
	}

	return Fvector(len, elt);
}

#ifdef ERROR_CHECK_GLYPHS
static int
check_instance_cache_mapper(Lisp_Object key, Lisp_Object value,
			    void *flag_closure)
{
	/* This function can GC */
	/* value can be nil; we cache failures as well as successes */
	if (!NILP(value)) {
		Lisp_Object window;
		VOID_TO_LISP(window, flag_closure);
		assert(EQ(XIMAGE_INSTANCE_DOMAIN(value), window));
	}

	return 0;
}

void check_window_subwindow_cache(struct window *w)
{
	Lisp_Object window;

	XSETWINDOW(window, w);

	assert(!NILP(w->subwindow_instance_cache));
	elisp_maphash(check_instance_cache_mapper,
		      w->subwindow_instance_cache, LISP_TO_VOID(window));
}

void check_image_instance_structure(Lisp_Object instance)
{
	/* Weird nothing images exist at startup when the console is
	   deleted. */
	if (!NOTHING_IMAGE_INSTANCEP(instance)) {
		assert(DOMAIN_LIVE_P(instance));
		assert(VECTORP(XIMAGE_INSTANCE_INSTANTIATOR(instance)));
	}
	if (WINDOWP(XIMAGE_INSTANCE_DOMAIN(instance)))
		check_window_subwindow_cache
		    (XWINDOW(XIMAGE_INSTANCE_DOMAIN(instance)));
}
#endif

/* Determine what kind of domain governs the image instance.
   Verify that the given domain is at least as specific, and extract
   the governing domain from it. */
static Lisp_Object
get_image_instantiator_governing_domain(Lisp_Object instantiator,
					Lisp_Object domain)
{
	int governing_domain;

	struct image_instantiator_methods *meths =
	    decode_image_instantiator_format(INSTANTIATOR_TYPE(instantiator),
					     ERROR_ME);
	governing_domain = IIFORMAT_METH_OR_GIVEN(meths, governing_domain, (),
						  GOVERNING_DOMAIN_DEVICE);

	if (governing_domain == GOVERNING_DOMAIN_WINDOW
	    && NILP(DOMAIN_WINDOW(domain)))
		signal_simple_error_2
		    ("Domain for this instantiator must be resolvable to a window",
		     instantiator, domain);
	else if (governing_domain == GOVERNING_DOMAIN_FRAME
		 && NILP(DOMAIN_FRAME(domain)))
		signal_simple_error_2
		    ("Domain for this instantiator must be resolvable to a frame",
		     instantiator, domain);

	if (governing_domain == GOVERNING_DOMAIN_WINDOW)
		domain = DOMAIN_WINDOW(domain);
	else if (governing_domain == GOVERNING_DOMAIN_FRAME)
		domain = DOMAIN_FRAME(domain);
	else if (governing_domain == GOVERNING_DOMAIN_DEVICE)
		domain = DOMAIN_DEVICE(domain);
	else
		abort();

	return domain;
}

Lisp_Object
normalize_image_instantiator(Lisp_Object instantiator,
			     Lisp_Object contype, Lisp_Object dest_mask)
{
	if (IMAGE_INSTANCEP(instantiator))
		return instantiator;

	if (STRINGP(instantiator))
		instantiator =
		    process_image_string_instantiator(instantiator, contype,
						      XINT(dest_mask));
	/* Subsequent validation will pick this up. */
	if (!VECTORP(instantiator))
		return instantiator;
	/* We have to always store the actual pixmap data and not the
	   filename even though this is a potential memory pig.  We have to
	   do this because it is quite possible that we will need to
	   instantiate a new instance of the pixmap and the file will no
	   longer exist (e.g. w3 pixmaps are almost always from temporary
	   files). */
	{
		struct gcpro gcpro1;
		struct image_instantiator_methods *meths;

		GCPRO1(instantiator);

		meths =
		    decode_image_instantiator_format(INSTANTIATOR_TYPE
						     (instantiator), ERROR_ME);
		RETURN_UNGCPRO(IIFORMAT_METH_OR_GIVEN
			       (meths, normalize,
				(instantiator, contype, dest_mask),
				instantiator));
	}
}

static Lisp_Object
instantiate_image_instantiator(Lisp_Object governing_domain,
			       Lisp_Object domain,
			       Lisp_Object instantiator,
			       Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			       int dest_mask, Lisp_Object glyph)
{
	Lisp_Object ii = allocate_image_instance(governing_domain,
						 IMAGE_INSTANCEP(domain) ?
						 domain : glyph, instantiator);
	Lisp_Image_Instance *p = XIMAGE_INSTANCE(ii);
	struct image_instantiator_methods *meths, *device_meths;
	struct gcpro gcpro1;

	GCPRO1(ii);
	if (!valid_image_instantiator_format_p(INSTANTIATOR_TYPE(instantiator),
					       DOMAIN_DEVICE(governing_domain)))
		signal_simple_error
		    ("Image instantiator format is invalid in this locale.",
		     instantiator);

	meths =
	    decode_image_instantiator_format(INSTANTIATOR_TYPE(instantiator),
					     ERROR_ME);
	MAYBE_IIFORMAT_METH(meths, instantiate,
			    (ii, instantiator, pointer_fg, pointer_bg,
			     dest_mask, domain));

	/* Now do device specific instantiation. */
	device_meths = decode_device_ii_format(DOMAIN_DEVICE(governing_domain),
					       INSTANTIATOR_TYPE(instantiator),
					       ERROR_ME_NOT);

	if (!HAS_IIFORMAT_METH_P(meths, instantiate)
	    && (!device_meths
		|| !HAS_IIFORMAT_METH_P(device_meths, instantiate)))
		signal_simple_error
		    ("Don't know how to instantiate this image instantiator?",
		     instantiator);

	/* In general native window system methods will require sane
	   geometry values, thus the instance needs to have been laid-out
	   before they get called. */
	image_instance_layout(ii, XIMAGE_INSTANCE_WIDTH(ii),
			      XIMAGE_INSTANCE_HEIGHT(ii),
			      IMAGE_UNCHANGED_GEOMETRY,
			      IMAGE_UNCHANGED_GEOMETRY, domain);

	MAYBE_IIFORMAT_METH(device_meths, instantiate,
			    (ii, instantiator, pointer_fg, pointer_bg,
			     dest_mask, domain));
	/* Do post instantiation. */
	MAYBE_IIFORMAT_METH(meths, post_instantiate,
			    (ii, instantiator, domain));
	MAYBE_IIFORMAT_METH(device_meths, post_instantiate,
			    (ii, instantiator, domain));

	/* We're done. */
	IMAGE_INSTANCE_INITIALIZED(p) = 1;
	/* Now that we're done verify that we really are laid out. */
	if (IMAGE_INSTANCE_LAYOUT_CHANGED(p))
		image_instance_layout(ii, XIMAGE_INSTANCE_WIDTH(ii),
				      XIMAGE_INSTANCE_HEIGHT(ii),
				      IMAGE_UNCHANGED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY, domain);

	/* We *must* have a clean image at this point. */
	IMAGE_INSTANCE_TEXT_CHANGED(p) = 0;
	IMAGE_INSTANCE_SIZE_CHANGED(p) = 0;
	IMAGE_INSTANCE_LAYOUT_CHANGED(p) = 0;
	IMAGE_INSTANCE_DIRTYP(p) = 0;

	assert(XIMAGE_INSTANCE_HEIGHT(ii) >= 0
	       && XIMAGE_INSTANCE_WIDTH(ii) >= 0);

	ERROR_CHECK_IMAGE_INSTANCE(ii);

	RETURN_UNGCPRO(ii);
}

/****************************************************************************
 *                          Image-Instance Object                           *
 ****************************************************************************/

Lisp_Object Qimage_instancep;

static Lisp_Object mark_image_instance(Lisp_Object obj)
{
	Lisp_Image_Instance *i = XIMAGE_INSTANCE(obj);

	/* #### I want to check the instance here, but there are way too
	   many instances of the instance being marked while the domain is
	   dead. For instance you can get marked through an event when using
	   callback_ex. */
#if 0
	ERROR_CHECK_IMAGE_INSTANCE(obj);
#endif

	mark_object(i->name);
	mark_object(i->instantiator);
	/* Is this legal in marking? We may get in the situation where the
	   domain has been deleted - making the instance unusable. It seems
	   better to remove the domain so that it can be finalized. */
	if (!DOMAIN_LIVE_P(i->domain))
		i->domain = Qnil;
	else
		mark_object(i->domain);

	/* We don't mark the glyph reference since that would create a
	   circularity preventing GC. Ditto the instantiator. */
	switch (IMAGE_INSTANCE_TYPE(i)) {
	case IMAGE_TEXT:
		mark_object(IMAGE_INSTANCE_TEXT_STRING(i));
		break;
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
		mark_object(IMAGE_INSTANCE_PIXMAP_FILENAME(i));
		mark_object(IMAGE_INSTANCE_PIXMAP_MASK_FILENAME(i));
		mark_object(IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(i));
		mark_object(IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(i));
		mark_object(IMAGE_INSTANCE_PIXMAP_FG(i));
		mark_object(IMAGE_INSTANCE_PIXMAP_BG(i));
		break;

	case IMAGE_WIDGET:
		mark_object(IMAGE_INSTANCE_WIDGET_TYPE(i));
		mark_object(IMAGE_INSTANCE_WIDGET_PROPS(i));
		mark_object(IMAGE_INSTANCE_SUBWINDOW_FACE(i));
		mark_object(IMAGE_INSTANCE_WIDGET_ITEMS(i));
		mark_object(IMAGE_INSTANCE_LAYOUT_CHILDREN(i));
		mark_object(IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(i));
		mark_object(IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(i));
		mark_object(IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(i));
	case IMAGE_SUBWINDOW:
		break;

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_POINTER:
	default:
		break;
	}

	/* The image may have been previously finalized (yes that's weird,
	   see Fdelete_frame() and mark_window_as_deleted()), in which case
	   the domain will be nil, so cope with this. */
	if (!NILP(IMAGE_INSTANCE_DEVICE(i)))
		MAYBE_DEVMETH(XDEVICE(IMAGE_INSTANCE_DEVICE(i)),
			      mark_image_instance, (i));

	return i->device;
}

static void
print_image_instance(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(obj);

	if (print_readably)
		error("printing unreadable object #<image-instance 0x%x>",
		      ii->header.uid);
	write_c_string("#<image-instance (", printcharfun);
	print_internal(Fimage_instance_type(obj), printcharfun, 0);
	write_c_string(") ", printcharfun);
	if (!NILP(ii->name)) {
		print_internal(ii->name, printcharfun, 1);
		write_c_string(" ", printcharfun);
	}
	write_c_string("on ", printcharfun);
	print_internal(ii->domain, printcharfun, 0);
	write_c_string(" ", printcharfun);
	switch (IMAGE_INSTANCE_TYPE(ii)) {
	case IMAGE_NOTHING:
		break;

	case IMAGE_TEXT:
		print_internal(IMAGE_INSTANCE_TEXT_STRING(ii), printcharfun, 1);
		break;

	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		if (STRINGP(IMAGE_INSTANCE_PIXMAP_FILENAME(ii))) {
			char *s;
			Lisp_Object filename =
			    IMAGE_INSTANCE_PIXMAP_FILENAME(ii);
			s = strrchr((char *)XSTRING_DATA(filename), '/');
			if (s)
				print_internal(build_string(s + 1),
					       printcharfun, 1);
			else
				print_internal(filename, printcharfun, 1);
		}
		if (IMAGE_INSTANCE_PIXMAP_DEPTH(ii) > 1)
			write_fmt_str(printcharfun, " %dx%dx%d",
				      IMAGE_INSTANCE_PIXMAP_WIDTH(ii),
				      IMAGE_INSTANCE_PIXMAP_HEIGHT(ii),
				      IMAGE_INSTANCE_PIXMAP_DEPTH(ii));
		else
			write_fmt_str(printcharfun, " %dx%d", IMAGE_INSTANCE_PIXMAP_WIDTH(ii),
				      IMAGE_INSTANCE_PIXMAP_HEIGHT(ii));
		if (!NILP(IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(ii)) ||
		    !NILP(IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(ii))) {
			write_c_string(" @", printcharfun);
			if (!NILP(IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(ii))) {
				write_fmt_str(printcharfun,"%ld",
					      XINT(IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(ii)));
			} else
				write_c_string("??", printcharfun);
			write_c_string(",", printcharfun);
			if (!NILP(IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(ii))) {
				write_fmt_str(printcharfun,"%ld",
					      XINT(IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(ii)));
			} else
				write_c_string("??", printcharfun);
		}
		if (!NILP(IMAGE_INSTANCE_PIXMAP_FG(ii)) ||
		    !NILP(IMAGE_INSTANCE_PIXMAP_BG(ii))) {
			write_c_string(" (", printcharfun);
			if (!NILP(IMAGE_INSTANCE_PIXMAP_FG(ii))) {
				print_internal
				    (XCOLOR_INSTANCE
				     (IMAGE_INSTANCE_PIXMAP_FG(ii))->name,
				     printcharfun, 0);
			}
			write_c_string("/", printcharfun);
			if (!NILP(IMAGE_INSTANCE_PIXMAP_BG(ii))) {
				print_internal
				    (XCOLOR_INSTANCE
				     (IMAGE_INSTANCE_PIXMAP_BG(ii))->name,
				     printcharfun, 0);
			}
			write_c_string(")", printcharfun);
		}
		break;

	case IMAGE_WIDGET:
		print_internal(IMAGE_INSTANCE_WIDGET_TYPE(ii), printcharfun, 0);

		if (GUI_ITEMP(IMAGE_INSTANCE_WIDGET_ITEM(ii))) {
			write_c_string(" ", printcharfun);
			print_internal(IMAGE_INSTANCE_WIDGET_TEXT(ii),
				       printcharfun, 1);
		}

		if (!NILP(IMAGE_INSTANCE_WIDGET_FACE(ii))) {
			write_c_string(" face=", printcharfun);
			print_internal
			    (IMAGE_INSTANCE_WIDGET_FACE(ii), printcharfun, 0);
		}
		/* fallthrough */

	case IMAGE_SUBWINDOW:
		write_fmt_str(printcharfun, " %dx%d", IMAGE_INSTANCE_WIDTH(ii),
			      IMAGE_INSTANCE_HEIGHT(ii));

		/* This is stolen from frame.c.  Subwindows are strange in that they
		   are specific to a particular frame so we want to print in their
		   description what that frame is. */

		write_c_string(" on #<", printcharfun);
		{
			struct frame *f = XFRAME(IMAGE_INSTANCE_FRAME(ii));

			if (!FRAME_LIVE_P(f))
				write_c_string("dead", printcharfun);
			else
				write_c_string(DEVICE_TYPE_NAME
					       (XDEVICE(FRAME_DEVICE(f))),
					       printcharfun);
		}
		write_c_string("-frame>", printcharfun);
		write_hex_ptr( IMAGE_INSTANCE_SUBWINDOW_ID(ii), printcharfun);

		break;

	case IMAGE_UNKNOWN:
	default:
		abort();
	}

	MAYBE_DEVMETH(DOMAIN_XDEVICE(ii->domain), print_image_instance,
		      (ii, printcharfun, escapeflag));
	write_fmt_str(printcharfun, " 0x%x>", ii->header.uid);
}

static void finalize_image_instance(void *header, int for_disksave)
{
	Lisp_Image_Instance *i = (Lisp_Image_Instance *) header;

	/* objects like this exist at dump time, so don't bomb out. */
	if (IMAGE_INSTANCE_TYPE(i) == IMAGE_NOTHING
	    || NILP(IMAGE_INSTANCE_DEVICE(i)))
		return;
	if (for_disksave)
		finalose(i);

	/* We can't use the domain here, because it might have
	   disappeared. */
	MAYBE_DEVMETH(XDEVICE(IMAGE_INSTANCE_DEVICE(i)),
		      finalize_image_instance, (i));

	/* Make sure we don't try this twice. */
	IMAGE_INSTANCE_DEVICE(i) = Qnil;
}

static int image_instance_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Image_Instance *i1 = XIMAGE_INSTANCE(obj1);
	Lisp_Image_Instance *i2 = XIMAGE_INSTANCE(obj2);

	ERROR_CHECK_IMAGE_INSTANCE(obj1);
	ERROR_CHECK_IMAGE_INSTANCE(obj2);

	if (!EQ(IMAGE_INSTANCE_DOMAIN(i1), IMAGE_INSTANCE_DOMAIN(i2))
	    || IMAGE_INSTANCE_TYPE(i1) != IMAGE_INSTANCE_TYPE(i2)
	    || IMAGE_INSTANCE_WIDTH(i1) != IMAGE_INSTANCE_WIDTH(i2)
	    || IMAGE_INSTANCE_MARGIN_WIDTH(i1) !=
	    IMAGE_INSTANCE_MARGIN_WIDTH(i2)
	    || IMAGE_INSTANCE_HEIGHT(i1) != IMAGE_INSTANCE_HEIGHT(i2)
	    || IMAGE_INSTANCE_XOFFSET(i1) != IMAGE_INSTANCE_XOFFSET(i2)
	    || IMAGE_INSTANCE_YOFFSET(i1) != IMAGE_INSTANCE_YOFFSET(i2))
		return 0;
	if (!internal_equal(IMAGE_INSTANCE_NAME(i1), IMAGE_INSTANCE_NAME(i2),
			    depth + 1))
		return 0;
	if (!internal_equal(IMAGE_INSTANCE_INSTANTIATOR(i1),
			    IMAGE_INSTANCE_INSTANTIATOR(i2), depth + 1))
		return 0;

	switch (IMAGE_INSTANCE_TYPE(i1)) {
	case IMAGE_NOTHING:
		break;

	case IMAGE_TEXT:
		if (!internal_equal(IMAGE_INSTANCE_TEXT_STRING(i1),
				    IMAGE_INSTANCE_TEXT_STRING(i2), depth + 1))
			return 0;
		break;

	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		if (!(IMAGE_INSTANCE_PIXMAP_DEPTH(i1) ==
		      IMAGE_INSTANCE_PIXMAP_DEPTH(i2) &&
		      IMAGE_INSTANCE_PIXMAP_SLICE(i1) ==
		      IMAGE_INSTANCE_PIXMAP_SLICE(i2) &&
		      EQ(IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(i1),
			 IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(i2)) &&
		      EQ(IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(i1),
			 IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(i2)) &&
		      internal_equal(IMAGE_INSTANCE_PIXMAP_FILENAME(i1),
				     IMAGE_INSTANCE_PIXMAP_FILENAME(i2),
				     depth + 1) &&
		      internal_equal(IMAGE_INSTANCE_PIXMAP_MASK_FILENAME(i1),
				     IMAGE_INSTANCE_PIXMAP_MASK_FILENAME(i2),
				     depth + 1)))
			return 0;
		break;

	case IMAGE_WIDGET:
		if (!(EQ(IMAGE_INSTANCE_WIDGET_TYPE(i1),
			 IMAGE_INSTANCE_WIDGET_TYPE(i2))
		      && IMAGE_INSTANCE_SUBWINDOW_ID(i1) ==
		      IMAGE_INSTANCE_SUBWINDOW_ID(i2)
		      &&
		      EQ(IMAGE_INSTANCE_WIDGET_FACE(i1),
			 IMAGE_INSTANCE_WIDGET_TYPE(i2))
		      && internal_equal(IMAGE_INSTANCE_WIDGET_ITEMS(i1),
					IMAGE_INSTANCE_WIDGET_ITEMS(i2),
					depth + 1)
		      && internal_equal(IMAGE_INSTANCE_LAYOUT_CHILDREN(i1),
					IMAGE_INSTANCE_LAYOUT_CHILDREN(i2),
					depth + 1)
		      && internal_equal(IMAGE_INSTANCE_WIDGET_PROPS(i1),
					IMAGE_INSTANCE_WIDGET_PROPS(i2),
					depth + 1)
		      && internal_equal(IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(i1),
					IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(i2),
					depth + 1)
		      && internal_equal(IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(i1),
					IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(i2),
					depth + 1)
		    ))
			return 0;
		break;

	case IMAGE_SUBWINDOW:
		if (!(IMAGE_INSTANCE_SUBWINDOW_ID(i1) ==
		      IMAGE_INSTANCE_SUBWINDOW_ID(i2)))
			return 0;
		break;

	case IMAGE_UNKNOWN:
	default:
		abort();
	}

	return DEVMETH_OR_GIVEN(DOMAIN_XDEVICE(i1->domain),
				image_instance_equal, (i1, i2, depth), 1);
}

/* Image instance domain manipulators. We can't error check in these
   otherwise we get into infinite recursion. */
Lisp_Object image_instance_device(Lisp_Object instance)
{
	return XIMAGE_INSTANCE_DEVICE(instance);
}

Lisp_Object image_instance_frame(Lisp_Object instance)
{
	return XIMAGE_INSTANCE_FRAME(instance);
}

Lisp_Object image_instance_window(Lisp_Object instance)
{
	return DOMAIN_WINDOW(XIMAGE_INSTANCE_DOMAIN(instance));
}

int image_instance_live_p(Lisp_Object instance)
{
	return DOMAIN_LIVE_P(XIMAGE_INSTANCE_DOMAIN(instance));
}

static unsigned long image_instance_hash(Lisp_Object obj, int depth)
{
	Lisp_Image_Instance *i = XIMAGE_INSTANCE(obj);
	unsigned long hash = HASH5(LISP_HASH(IMAGE_INSTANCE_DOMAIN(i)),
				   IMAGE_INSTANCE_WIDTH(i),
				   IMAGE_INSTANCE_MARGIN_WIDTH(i),
				   IMAGE_INSTANCE_HEIGHT(i),
				   internal_hash(IMAGE_INSTANCE_INSTANTIATOR(i),
						 depth + 1));

	ERROR_CHECK_IMAGE_INSTANCE(obj);

	switch (IMAGE_INSTANCE_TYPE(i)) {
	case IMAGE_NOTHING:
		break;

	case IMAGE_TEXT:
		hash = HASH2(hash, internal_hash(IMAGE_INSTANCE_TEXT_STRING(i),
						 depth + 1));
		break;

	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		hash = HASH4(hash, IMAGE_INSTANCE_PIXMAP_DEPTH(i),
			     IMAGE_INSTANCE_PIXMAP_SLICE(i),
			     internal_hash(IMAGE_INSTANCE_PIXMAP_FILENAME(i),
					   depth + 1));
		break;

	case IMAGE_WIDGET:
		/* We need the hash to be equivalent to what should be
		   displayed. */
		hash = HASH5(hash,
			     LISP_HASH(IMAGE_INSTANCE_WIDGET_TYPE(i)),
			     internal_hash(IMAGE_INSTANCE_WIDGET_PROPS(i),
					   depth + 1),
			     internal_hash(IMAGE_INSTANCE_WIDGET_ITEMS(i),
					   depth + 1),
			     internal_hash(IMAGE_INSTANCE_LAYOUT_CHILDREN(i),
					   depth + 1));
	case IMAGE_SUBWINDOW:
		hash = HASH2(hash, (EMACS_INT) IMAGE_INSTANCE_SUBWINDOW_ID(i));
		break;

	case IMAGE_UNKNOWN:
	default:
		abort();
	}

	{
		Lisp_Object tmp = image_instance_device(obj);
		return HASH2(hash, DEVMETH_OR_GIVEN(
				     XDEVICE(tmp), image_instance_hash,
				     (i, depth), 0));
	}
}

DEFINE_LRECORD_IMPLEMENTATION("image-instance", image_instance,
			      mark_image_instance, print_image_instance,
			      finalize_image_instance, image_instance_equal,
			      image_instance_hash, 0, Lisp_Image_Instance);

static Lisp_Object
allocate_image_instance(Lisp_Object governing_domain, Lisp_Object parent,
			Lisp_Object instantiator)
{
	Lisp_Image_Instance *lp =
	    alloc_lcrecord_type(Lisp_Image_Instance, &lrecord_image_instance);
	Lisp_Object val;

	zero_lcrecord(lp);
	/* It's not possible to simply keep a record of the domain in which
	   the instance was instantiated. This is because caching may mean
	   that the domain becomes invalid but the instance remains
	   valid. However, the only truly relevant domain is the domain in
	   which the instance is cached since this is the one that will be
	   common to the instances. */
	lp->domain = governing_domain;
	/* The cache domain is not quite sufficient since the domain can get
	   deleted before the image instance does. We need to know the
	   domain device in order to finalize the image instance
	   properly. We therefore record the device also. */
	lp->device = DOMAIN_DEVICE(governing_domain);
	lp->type = IMAGE_NOTHING;
	lp->name = Qnil;
	lp->x_offset = 0;
	lp->y_offset = 0;
	lp->width = IMAGE_UNSPECIFIED_GEOMETRY;
	lp->margin_width = 0;
	lp->height = IMAGE_UNSPECIFIED_GEOMETRY;
	lp->parent = parent;
	lp->instantiator = instantiator;
	/* So that layouts get done. */
	lp->layout_changed = 1;
	lp->initialized = 0;

	XSETIMAGE_INSTANCE(val, lp);
	MARK_GLYPHS_CHANGED;

	return val;
}

static enum image_instance_type
decode_image_instance_type(Lisp_Object type, Error_behavior errb)
{
	if (ERRB_EQ(errb, ERROR_ME))
		CHECK_SYMBOL(type);

	if (EQ(type, Qnothing))
		return IMAGE_NOTHING;
	if (EQ(type, Qtext))
		return IMAGE_TEXT;
	if (EQ(type, Qmono_pixmap))
		return IMAGE_MONO_PIXMAP;
	if (EQ(type, Qcolor_pixmap))
		return IMAGE_COLOR_PIXMAP;
	if (EQ(type, Qpointer))
		return IMAGE_POINTER;
	if (EQ(type, Qsubwindow))
		return IMAGE_SUBWINDOW;
	if (EQ(type, Qwidget))
		return IMAGE_WIDGET;

	maybe_signal_simple_error("Invalid image-instance type", type,
				  Qimage, errb);

	return IMAGE_UNKNOWN;	/* not reached */
}

static Lisp_Object encode_image_instance_type(enum image_instance_type type)
{
	switch (type) {
	case IMAGE_NOTHING:
		return Qnothing;
	case IMAGE_TEXT:
		return Qtext;
	case IMAGE_MONO_PIXMAP:
		return Qmono_pixmap;
	case IMAGE_COLOR_PIXMAP:
		return Qcolor_pixmap;
	case IMAGE_POINTER:
		return Qpointer;
	case IMAGE_SUBWINDOW:
		return Qsubwindow;
	case IMAGE_WIDGET:
		return Qwidget;

	case IMAGE_UNKNOWN:
	default:
		abort();
	}

	return Qnil;		/* not reached */
}

static int decode_image_instance_type_list(Lisp_Object list)
{
	Lisp_Object rest;
	int mask = 0;

	if (NILP(list))
		return ~0;

	if (!CONSP(list)) {
		enum image_instance_type type =
		    decode_image_instance_type(list, ERROR_ME);
		return image_instance_type_to_mask(type);
	}

	EXTERNAL_LIST_LOOP(rest, list) {
		enum image_instance_type type =
		    decode_image_instance_type(XCAR(rest), ERROR_ME);
		mask |= image_instance_type_to_mask(type);
	}

	return mask;
}

static Lisp_Object encode_image_instance_type_list(int mask)
{
	int count = 0;
	Lisp_Object result = Qnil;

	while (mask) {
		count++;
		if (mask & 1)
			result = Fcons(encode_image_instance_type
				       ((enum image_instance_type)count),
				       result);
		mask >>= 1;
	}

	return Fnreverse(result);
}

DOESNT_RETURN
incompatible_image_types(Lisp_Object instantiator, int given_dest_mask,
			 int desired_dest_mask)
{
	signal_error(Qerror, list2(emacs_doprnt_string_lisp_2((const Bufbyte *)
							      "No compatible image-instance types given: wanted one of %s, got %s",
							      Qnil, -1, 2,
							      encode_image_instance_type_list
							      (desired_dest_mask),
							      encode_image_instance_type_list
							      (given_dest_mask)),
				   instantiator));
}

static int valid_image_instance_type_p(Lisp_Object type)
{
	return !NILP(memq_no_quit(type, Vimage_instance_type_list));
}

DEFUN("valid-image-instance-type-p", Fvalid_image_instance_type_p, 1, 1, 0,	/*
Given an IMAGE-INSTANCE-TYPE, return non-nil if it is valid.
Valid types are some subset of 'nothing, 'text, 'mono-pixmap, 'color-pixmap,
'pointer, 'subwindow, and 'widget, depending on how SXEmacs was compiled.
*/
      (image_instance_type))
{
	return valid_image_instance_type_p(image_instance_type) ? Qt : Qnil;
}

DEFUN("image-instance-type-list", Fimage_instance_type_list, 0, 0, 0,	/*
Return a list of valid image-instance types.
*/
      ())
{
	return Fcopy_sequence(Vimage_instance_type_list);
}

Error_behavior decode_error_behavior_flag(Lisp_Object noerror)
{
	if (NILP(noerror))
		return ERROR_ME;
	else if (EQ(noerror, Qt))
		return ERROR_ME_NOT;
	else
		return ERROR_ME_WARN;
}

Lisp_Object encode_error_behavior_flag(Error_behavior errb)
{
	if (ERRB_EQ(errb, ERROR_ME))
		return Qnil;
	else if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qt;
	else {
		assert(ERRB_EQ(errb, ERROR_ME_WARN));
		return Qwarning;
	}
}

/* Recurse up the hierarchy looking for the topmost glyph. This means
   that instances in layouts will inherit face properties from their
   parent. */
Lisp_Object image_instance_parent_glyph(Lisp_Image_Instance * ii)
{
	if (IMAGE_INSTANCEP(IMAGE_INSTANCE_PARENT(ii))) {
		return image_instance_parent_glyph
		    (XIMAGE_INSTANCE(IMAGE_INSTANCE_PARENT(ii)));
	}
	return IMAGE_INSTANCE_PARENT(ii);
}

static Lisp_Object
make_image_instance_1(Lisp_Object data, Lisp_Object domain,
		      Lisp_Object dest_types)
{
	Lisp_Object ii;
	struct gcpro gcpro1;
	int dest_mask;
	Lisp_Object governing_domain;

	if (IMAGE_INSTANCEP(data))
		signal_simple_error("Image instances not allowed here", data);
	image_validate(data);
	domain = decode_domain(domain);
	/* instantiate_image_instantiator() will abort if given an
	   image instance ... */
	dest_mask = decode_image_instance_type_list(dest_types);
	data = normalize_image_instantiator(data,
					    DEVICE_TYPE(DOMAIN_XDEVICE(domain)),
					    make_int(dest_mask));
	GCPRO1(data);
	/* After normalizing the data, it's always either an image instance (which
	   we filtered out above) or a vector. */
	if (EQ(INSTANTIATOR_TYPE(data), Qinherit))
		signal_simple_error("Inheritance not allowed here", data);
	governing_domain =
	    get_image_instantiator_governing_domain(data, domain);
	ii = instantiate_image_instantiator(governing_domain, domain, data,
					    Qnil, Qnil, dest_mask, Qnil);
	RETURN_UNGCPRO(ii);
}

DEFUN("make-image-instance", Fmake_image_instance, 1, 4, 0,	/*
Return a new `image-instance' object.

Image-instance objects encapsulate the way a particular image (pixmap,
etc.) is displayed on a particular device.  In most circumstances, you
do not need to directly create image instances; use a glyph instead.
However, it may occasionally be useful to explicitly create image
instances, if you want more control over the instantiation process.

DATA is an image instantiator, which describes the image; see
`make-image-specifier' for a description of the allowed values.

DEST-TYPES should be a list of allowed image instance types that can
be generated.  The recognized image instance types are

'nothing
Nothing is displayed.
'text
Displayed as text.  The foreground and background colors and the
font of the text are specified independent of the pixmap.  Typically
these attributes will come from the face of the surrounding text,
unless a face is specified for the glyph in which the image appears.
'mono-pixmap
Displayed as a mono pixmap (a pixmap with only two colors where the
foreground and background can be specified independent of the pixmap;
typically the pixmap assumes the foreground and background colors of
the text around it, unless a face is specified for the glyph in which
the image appears).
'color-pixmap
Displayed as a color pixmap.
'pointer
Used as the mouse pointer for a window.
'subwindow
A child window that is treated as an image.  This allows (e.g.)
another program to be responsible for drawing into the window.
'widget
A child window that contains a window-system widget, e.g. a push
button, text field, or slider.

The DEST-TYPES list is unordered.  If multiple destination types are
possible for a given instantiator, the "most natural" type for the
instantiator's format is chosen. (For XBM, the most natural types are
`mono-pixmap', followed by `color-pixmap', followed by `pointer'.  For
the other normal image formats, the most natural types are
`color-pixmap', followed by `mono-pixmap', followed by `pointer'.  For
the string and formatted-string formats, the most natural types are
`text', followed by `mono-pixmap' (not currently implemented),
followed by `color-pixmap' (not currently implemented).  For MS
Windows resources, the most natural type for pointer resources is
`pointer', and for the others it's `color-pixmap'.  The other formats
can only be instantiated as one type. (If you want to control more
specifically the order of the types into which an image is
instantiated, just call `make-image-instance' repeatedly until it
succeeds, passing less and less preferred destination types each
time.)

See `make-image-specifier' for a description of the different image
instantiator formats.

If DEST-TYPES is omitted, all possible types are allowed.

DOMAIN specifies the domain to which the image instance will be attached.
This domain is termed the \"governing domain\".  The type of the governing
domain depends on the image instantiator format. (Although, more correctly,
it should probably depend on the image instance type.) For example, pixmap
image instances are specific to a device, but widget image instances are
specific to a particular SXEmacs window because in order to display such a
widget when two windows onto the same buffer want to display the widget,
two separate underlying widgets must be created. (That's because a widget
is actually a child window-system window, and all window-system windows have
a unique existence on the screen.) This means that the governing domain for
a pixmap image instance will be some device (most likely, the only existing
device), whereas the governing domain for a widget image instance will be
some SXEmacs window.

If you specify an overly general DOMAIN (e.g. a frame when a window was
wanted), an error is signaled.  If you specify an overly specific DOMAIN
\(e.g. a window when a device was wanted), the corresponding general domain
is fetched and used instead.  For `make-image-instance', it makes no
difference whether you specify an overly specific domain or the properly
general domain derived from it.  However, it does matter when creating an
image instance by instantiating a specifier or glyph (e.g. with
`glyph-image-instance'), because the more specific domain causes spec lookup
to start there and proceed to more general domains. (It would also matter
when creating an image instance with an instantiator format of `inherit',
but we currently disallow this. #### We should fix this.)

If omitted, DOMAIN defaults to the selected window.

NOERROR controls what happens when the image cannot be generated.
If nil, an error message is generated.  If t, no messages are
generated and this function returns nil.  If anything else, a warning
message is generated and this function returns nil.
*/
      (data, domain, dest_types, noerror))
{
	Error_behavior errb = decode_error_behavior_flag(noerror);

	return call_with_suspended_errors((lisp_fn_t) make_image_instance_1,
					  Qnil, Qimage, errb,
					  3, data, domain, dest_types);
}

DEFUN("image-instance-p", Fimage_instance_p, 1, 1, 0,	/*
Return non-nil if OBJECT is an image instance.
*/
      (object))
{
	return IMAGE_INSTANCEP(object) ? Qt : Qnil;
}

DEFUN("image-instance-type", Fimage_instance_type, 1, 1, 0,	/*
Return the type of the given image instance.
The return value will be one of 'nothing, 'text, 'mono-pixmap,
'color-pixmap, 'pointer, or 'subwindow.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);
	return encode_image_instance_type(XIMAGE_INSTANCE_TYPE(image_instance));
}

DEFUN("image-instance-name", Fimage_instance_name, 1, 1, 0,	/*
Return the name of the given image instance.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	return XIMAGE_INSTANCE_NAME(image_instance);
}

DEFUN("image-instance-domain", Fimage_instance_domain, 1, 1, 0,	/*
Return the governing domain of the given image instance.
The governing domain of an image instance is the domain that the image
instance is specific to.  It is NOT necessarily the domain that was
given to the call to `specifier-instance' that resulted in the creation
of this image instance.  See `make-image-instance' for more information
on governing domains.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	return XIMAGE_INSTANCE_DOMAIN(image_instance);
}

DEFUN("image-instance-string", Fimage_instance_string, 1, 1, 0,	/*
Return the string of the given image instance.
This will only be non-nil for text image instances and widgets.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	if (XIMAGE_INSTANCE_TYPE(image_instance) == IMAGE_TEXT)
		return XIMAGE_INSTANCE_TEXT_STRING(image_instance);
	else if (XIMAGE_INSTANCE_TYPE(image_instance) == IMAGE_WIDGET)
		return XIMAGE_INSTANCE_WIDGET_TEXT(image_instance);
	else
		return Qnil;
}

DEFUN("image-instance-property", Fimage_instance_property, 2, 2, 0,	/*
Return the given property of the given image instance.
Returns nil if the property or the property method do not exist for
the image instance in the domain.
*/
      (image_instance, prop))
{
	Lisp_Image_Instance *ii;
	Lisp_Object type, ret;
	struct image_instantiator_methods *meths;

	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);
	CHECK_SYMBOL(prop);
	ii = XIMAGE_INSTANCE(image_instance);

	/* ... then try device specific methods ... */
	type = encode_image_instance_type(IMAGE_INSTANCE_TYPE(ii));
	meths = decode_device_ii_format(image_instance_device(image_instance),
					type, ERROR_ME_NOT);
	if (meths && HAS_IIFORMAT_METH_P(meths, property)
	    &&
	    !UNBOUNDP(ret =
		      IIFORMAT_METH(meths, property, (image_instance, prop)))) {
		return ret;
	}
	/* ... then format specific methods ... */
	meths = decode_device_ii_format(Qnil, type, ERROR_ME_NOT);
	if (meths && HAS_IIFORMAT_METH_P(meths, property)
	    &&
	    !UNBOUNDP(ret =
		      IIFORMAT_METH(meths, property, (image_instance, prop)))) {
		return ret;
	}
	/* ... then fail */
	return Qnil;
}

DEFUN("image-instance-file-name", Fimage_instance_file_name, 1, 1, 0,	/*
Return the file name from which IMAGE-INSTANCE was read, if known.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return XIMAGE_INSTANCE_PIXMAP_FILENAME(image_instance);

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-mask-file-name", Fimage_instance_mask_file_name, 1, 1, 0,	/*
Return the file name from which IMAGE-INSTANCE's mask was read, if known.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return XIMAGE_INSTANCE_PIXMAP_MASK_FILENAME(image_instance);

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-depth", Fimage_instance_depth, 1, 1, 0,	/*
Return the depth of the image instance.
This is 0 for a bitmap, or a positive integer for a pixmap.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return make_int(XIMAGE_INSTANCE_PIXMAP_DEPTH(image_instance));

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-height", Fimage_instance_height, 1, 1, 0,	/*
Return the height of the image instance, in pixels.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
		return make_int(XIMAGE_INSTANCE_HEIGHT(image_instance));

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-width", Fimage_instance_width, 1, 1, 0,	/*
Return the width of the image instance, in pixels.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
		return make_int(XIMAGE_INSTANCE_WIDTH(image_instance));

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-hotspot-x", Fimage_instance_hotspot_x, 1, 1, 0,	/*
Return the X coordinate of the image instance's hotspot, if known.
This is a point relative to the origin of the pixmap.  When an image is
used as a mouse pointer, the hotspot is the point on the image that sits
over the location that the pointer points to.  This is, for example, the
tip of the arrow or the center of the crosshairs.
This will always be nil for a non-pointer image instance.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return XIMAGE_INSTANCE_PIXMAP_HOTSPOT_X(image_instance);

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-hotspot-y", Fimage_instance_hotspot_y, 1, 1, 0,	/*
Return the Y coordinate of the image instance's hotspot, if known.
This is a point relative to the origin of the pixmap.  When an image is
used as a mouse pointer, the hotspot is the point on the image that sits
over the location that the pointer points to.  This is, for example, the
tip of the arrow or the center of the crosshairs.
This will always be nil for a non-pointer image instance.
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return XIMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(image_instance);

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	case IMAGE_WIDGET:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-foreground", Fimage_instance_foreground, 1, 1, 0,	/*
Return the foreground color of IMAGE-INSTANCE, if applicable.
This will be a color instance or nil. (It will only be non-nil for
colorized mono pixmaps and for pointers.)
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return XIMAGE_INSTANCE_PIXMAP_FG(image_instance);

	case IMAGE_WIDGET:
		return
		    FACE_FOREGROUND(XIMAGE_INSTANCE_WIDGET_FACE(image_instance),
				    XIMAGE_INSTANCE_FRAME(image_instance));

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	default:
		return Qnil;
	}
}

DEFUN("image-instance-background", Fimage_instance_background, 1, 1, 0,	/*
Return the background color of IMAGE-INSTANCE, if applicable.
This will be a color instance or nil. (It will only be non-nil for
colorized mono pixmaps and for pointers.)
*/
      (image_instance))
{
	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	switch (XIMAGE_INSTANCE_TYPE(image_instance)) {
	case IMAGE_MONO_PIXMAP:
	case IMAGE_COLOR_PIXMAP:
	case IMAGE_POINTER:
		return XIMAGE_INSTANCE_PIXMAP_BG(image_instance);

	case IMAGE_WIDGET:
		return
		    FACE_BACKGROUND(XIMAGE_INSTANCE_WIDGET_FACE(image_instance),
				    XIMAGE_INSTANCE_FRAME(image_instance));

	case IMAGE_UNKNOWN:
	case IMAGE_NOTHING:
	case IMAGE_TEXT:
	case IMAGE_SUBWINDOW:
	default:
		return Qnil;
	}
}

DEFUN("colorize-image-instance", Fcolorize_image_instance, 3, 3, 0,	/*
Make the image instance be displayed in the given colors.
This function returns a new image instance that is exactly like the
specified one except that (if possible) the foreground and background
colors and as specified.  Currently, this only does anything if the image
instance is a mono pixmap; otherwise, the same image instance is returned.
*/
      (image_instance, foreground, background))
{
	Lisp_Object new;
	Lisp_Object device;

	CHECK_IMAGE_INSTANCE(image_instance);
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);
	CHECK_COLOR_INSTANCE(foreground);
	CHECK_COLOR_INSTANCE(background);

	device = image_instance_device(image_instance);
	if (!HAS_DEVMETH_P(XDEVICE(device), colorize_image_instance))
		return image_instance;

	/* #### There should be a copy_image_instance(), which calls a
	   device-specific method to copy the window-system subobject. */
	new = allocate_image_instance(XIMAGE_INSTANCE_DOMAIN(image_instance),
				      Qnil, Qnil);
	copy_lcrecord(XIMAGE_INSTANCE(new), XIMAGE_INSTANCE(image_instance));
	/* note that if this method returns non-zero, this method MUST
	   copy any window-system resources, so that when one image instance is
	   freed, the other one is not hosed. */
	if (!DEVMETH(XDEVICE(device), colorize_image_instance, (new, foreground,
								background)))
		return image_instance;
	return new;
}

/************************************************************************/
/*                              Geometry calculations                   */
/************************************************************************/

/* Find out desired geometry of the image instance. If there is no
   special function then just return the width and / or height. */
void
image_instance_query_geometry(Lisp_Object image_instance,
			      int *width, int *height,
			      enum image_instance_geometry disp,
			      Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object type;
	struct image_instantiator_methods *meths;
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	type = encode_image_instance_type(IMAGE_INSTANCE_TYPE(ii));
	meths = decode_device_ii_format(Qnil, type, ERROR_ME_NOT);

	if (meths && HAS_IIFORMAT_METH_P(meths, query_geometry)) {
		IIFORMAT_METH(meths, query_geometry,
			      (image_instance, width, height, disp, domain));
	} else {
		if (width)
			*width = IMAGE_INSTANCE_WIDTH(ii);
		if (height)
			*height = IMAGE_INSTANCE_HEIGHT(ii);
	}
}

/* Layout the image instance using the provided dimensions. Layout
   widgets are going to do different kinds of calculations to
   determine what size to give things so we could make the layout
   function relatively simple to take account of that. An alternative
   approach is to consider separately the two cases, one where you
   don't mind what size you have (normal widgets) and one where you
   want to specify something (layout widgets). */
void
image_instance_layout(Lisp_Object image_instance,
		      int width, int height,
		      int xoffset, int yoffset, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object type;
	struct image_instantiator_methods *meths;

	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	/* Nothing is as nothing does. */
	if (NOTHING_IMAGE_INSTANCEP(image_instance))
		return;

	/* We don't want carefully calculated offsets to be mucked up by
	   random layouts. */
	if (xoffset != IMAGE_UNCHANGED_GEOMETRY)
		XIMAGE_INSTANCE_XOFFSET(image_instance) = xoffset;
	if (yoffset != IMAGE_UNCHANGED_GEOMETRY)
		XIMAGE_INSTANCE_YOFFSET(image_instance) = yoffset;

	/* If geometry is unspecified then get some reasonable values for it. */
	if (width == IMAGE_UNSPECIFIED_GEOMETRY
	    || height == IMAGE_UNSPECIFIED_GEOMETRY) {
		int dwidth = IMAGE_UNSPECIFIED_GEOMETRY;
		int dheight = IMAGE_UNSPECIFIED_GEOMETRY;
		/* Get the desired geometry. */
		image_instance_query_geometry(image_instance,
					      &dwidth, &dheight,
					      IMAGE_DESIRED_GEOMETRY, domain);
		/* Compare with allowed geometry. */
		if (width == IMAGE_UNSPECIFIED_GEOMETRY)
			width = dwidth;
		if (height == IMAGE_UNSPECIFIED_GEOMETRY)
			height = dheight;
	}

	/* If we don't have sane values then we cannot layout at this point and
	   must just return. */
	if (width == IMAGE_UNSPECIFIED_GEOMETRY
	    || height == IMAGE_UNSPECIFIED_GEOMETRY)
		return;

	/* At this point width and height should contain sane values. Thus
	   we set the glyph geometry and lay it out. */
	if (IMAGE_INSTANCE_WIDTH(ii) != width
	    || IMAGE_INSTANCE_HEIGHT(ii) != height) {
		IMAGE_INSTANCE_SIZE_CHANGED(ii) = 1;
	}

	IMAGE_INSTANCE_WIDTH(ii) = width;
	IMAGE_INSTANCE_HEIGHT(ii) = height;

	type = encode_image_instance_type(IMAGE_INSTANCE_TYPE(ii));
	meths = decode_device_ii_format(Qnil, type, ERROR_ME_NOT);

	MAYBE_IIFORMAT_METH(meths, layout,
			    (image_instance, width, height, xoffset, yoffset,
			     domain));
	/* Do not clear the dirty flag here - redisplay will do this for
	   us at the end. */
	IMAGE_INSTANCE_LAYOUT_CHANGED(ii) = 0;
}

/* Update an image instance from its changed instantiator. */
static void
update_image_instance(Lisp_Object image_instance, Lisp_Object instantiator)
{
	struct image_instantiator_methods *meths;
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);

	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	if (NOTHING_IMAGE_INSTANCEP(image_instance))
		return;

	assert(!internal_equal(IMAGE_INSTANCE_INSTANTIATOR(ii), instantiator, 0)
	       ||
	       (internal_equal(IMAGE_INSTANCE_INSTANTIATOR(ii), instantiator, 0)
		&& internal_equal(IMAGE_INSTANCE_INSTANTIATOR(ii), instantiator,
				  -10)));

	/* If the instantiator is identical then do nothing. We must use
	   equal here because the specifier code copies the instantiator. */
	if (!internal_equal(IMAGE_INSTANCE_INSTANTIATOR(ii), instantiator, 0)) {
		/* Extract the changed properties so that device / format
		   methods only have to cope with these. We assume that
		   normalization has already been done. */
		Lisp_Object diffs = find_instantiator_differences(instantiator,
								  IMAGE_INSTANCE_INSTANTIATOR
								  (ii));
		Lisp_Object type =
		    encode_image_instance_type(IMAGE_INSTANCE_TYPE(ii));
		struct gcpro gcpro1;
		GCPRO1(diffs);

		/* try device specific methods first ... */
		meths =
		    decode_device_ii_format(image_instance_device
					    (image_instance), type,
					    ERROR_ME_NOT);
		MAYBE_IIFORMAT_METH(meths, update, (image_instance, diffs));
		/* ... then format specific methods ... */
		meths = decode_device_ii_format(Qnil, type, ERROR_ME_NOT);
		MAYBE_IIFORMAT_METH(meths, update, (image_instance, diffs));

		/* Instance and therefore glyph has changed so mark as dirty.
		   If we don't do this output optimizations will assume the
		   glyph is unchanged. */
		set_image_instance_dirty_p(image_instance, 1);
		/* Structure has changed. */
		IMAGE_INSTANCE_LAYOUT_CHANGED(ii) = 1;

		UNGCPRO;
	}
	/* We should now have a consistent instantiator so keep a record of
	   it. It is important that we don't actually update the window
	   system widgets here - we must do that when redisplay tells us
	   to.

	   #### should we delay doing this until the display is up-to-date
	   also? */
	IMAGE_INSTANCE_INSTANTIATOR(ii) = instantiator;
}

/*
 * Mark image instance in W as dirty if (a) W's faces have changed and
 * (b) GLYPH_OR_II instance in W is a string.
 *
 * Return non-zero if instance has been marked dirty.
 */
int invalidate_glyph_geometry_maybe(Lisp_Object glyph_or_ii, struct window *w)
{
	if (XFRAME(WINDOW_FRAME(w))->faces_changed) {
		Lisp_Object image = glyph_or_ii;

		if (GLYPHP(glyph_or_ii)) {
			Lisp_Object window;
			XSETWINDOW(window, w);
			image =
			    glyph_image_instance(glyph_or_ii, window,
						 ERROR_ME_NOT, 1);
		}

		if (TEXT_IMAGE_INSTANCEP(image)) {
			Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image);
			IMAGE_INSTANCE_DIRTYP(ii) = 1;
			IMAGE_INSTANCE_LAYOUT_CHANGED(ii) = 1;
			if (GLYPHP(glyph_or_ii))
				XGLYPH_DIRTYP(glyph_or_ii) = 1;
			return 1;
		}
	}

	return 0;
}

/************************************************************************/
/*                              error helpers                           */
/************************************************************************/
DOESNT_RETURN signal_image_error(const char *reason, Lisp_Object frob)
{
	signal_error(Qimage_conversion_error,
		     list2(build_translated_string(reason), frob));
}

DOESNT_RETURN
signal_image_error_2(const char *reason, Lisp_Object frob0, Lisp_Object frob1)
{
	signal_error(Qimage_conversion_error,
		     list3(build_translated_string(reason), frob0, frob1));
}

/****************************************************************************
 *                                  nothing                                 *
 ****************************************************************************/

static int nothing_possible_dest_types(void)
{
	return IMAGE_NOTHING_MASK;
}

static void
nothing_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
		    Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		    int dest_mask, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);

	if (dest_mask & IMAGE_NOTHING_MASK) {
		IMAGE_INSTANCE_TYPE(ii) = IMAGE_NOTHING;
		IMAGE_INSTANCE_HEIGHT(ii) = 0;
		IMAGE_INSTANCE_WIDTH(ii) = 0;
	} else
		incompatible_image_types(instantiator, dest_mask,
					 IMAGE_NOTHING_MASK);
}

/****************************************************************************
 *                                  inherit                                 *
 ****************************************************************************/

static void inherit_validate(Lisp_Object instantiator)
{
	face_must_be_present(instantiator);
}

static Lisp_Object
inherit_normalize(Lisp_Object inst, Lisp_Object console_type,
		  Lisp_Object dest_mask)
{
	Lisp_Object face;

	assert(XVECTOR_LENGTH(inst) == 3);
	face = XVECTOR_DATA(inst)[2];
	if (!FACEP(face))
		inst = vector3(Qinherit, Q_face, Fget_face(face));
	return inst;
}

static int inherit_possible_dest_types(void)
{
	return IMAGE_MONO_PIXMAP_MASK;
}

static void
inherit_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
		    Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		    int dest_mask, Lisp_Object domain)
{
	/* handled specially in image_instantiate */
	abort();
}

/****************************************************************************
 *                                  string                                  *
 ****************************************************************************/

static void string_validate(Lisp_Object instantiator)
{
	data_must_be_present(instantiator);
}

static int string_possible_dest_types(void)
{
	return IMAGE_TEXT_MASK;
}

/* Called from autodetect_instantiate() */
void
string_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, Lisp_Object domain)
{
	Lisp_Object string = find_keyword_in_vector(instantiator, Q_data);
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);

	/* Should never get here with a domain other than a window. */
	assert(!NILP(string) && WINDOWP(DOMAIN_WINDOW(domain)));
	if (dest_mask & IMAGE_TEXT_MASK) {
		IMAGE_INSTANCE_TYPE(ii) = IMAGE_TEXT;
		IMAGE_INSTANCE_TEXT_STRING(ii) = string;
	} else
		incompatible_image_types(instantiator, dest_mask,
					 IMAGE_TEXT_MASK);
}

/* Sort out the size of the text that is being displayed. Calculating
   it dynamically allows us to change the text and still see
   everything. Note that the following methods are for text not string
   since that is what the instantiated type is. The first method is a
   helper that is used elsewhere for calculating text geometry. */
void
query_string_geometry(Lisp_Object string, Lisp_Object face,
		      int *width, int *height, int *descent, Lisp_Object domain)
{
	struct font_metric_info fm;
	unsigned char charsets[NUM_LEADING_BYTES];
	struct face_cachel frame_cachel;
	struct face_cachel *cachel;
	Lisp_Object frame = DOMAIN_FRAME(domain);

	CHECK_STRING(string);

	/* Compute height */
	if (height) {
		/* Compute string metric info */
		find_charsets_in_bufbyte_string(charsets,
						XSTRING_DATA(string),
						XSTRING_LENGTH(string));

		/* Fallback to the default face if none was provided. */
		if (!NILP(face)) {
			reset_face_cachel(&frame_cachel);
			update_face_cachel_data(&frame_cachel, frame, face);
			cachel = &frame_cachel;
		} else {
			cachel = WINDOW_FACE_CACHEL(DOMAIN_XWINDOW(domain),
						    DEFAULT_INDEX);
		}

		ensure_face_cachel_complete(cachel, domain, charsets);
		face_cachel_charset_font_metric_info(cachel, charsets, &fm);

		*height = fm.ascent + fm.descent;
		/* #### descent only gets set if we query the height as well. */
		if (descent)
			*descent = fm.descent;
	}

	/* Compute width */
	if (width) {
		if (!NILP(face))
			*width =
			    redisplay_frame_text_width_string(XFRAME(frame),
							      face, 0, string,
							      0, -1);
		else
			*width =
			    redisplay_frame_text_width_string(XFRAME(frame),
							      Vdefault_face, 0,
							      string, 0, -1);
	}
}

Lisp_Object
query_string_font(Lisp_Object string, Lisp_Object face, Lisp_Object domain)
{
	unsigned char charsets[NUM_LEADING_BYTES];
	struct face_cachel frame_cachel;
	struct face_cachel *cachel;
	int i;
	Lisp_Object frame = DOMAIN_FRAME(domain);

	/* Compute string font info */
	find_charsets_in_bufbyte_string(charsets,
					XSTRING_DATA(string),
					XSTRING_LENGTH(string));

	reset_face_cachel(&frame_cachel);
	update_face_cachel_data(&frame_cachel, frame, face);
	cachel = &frame_cachel;

	ensure_face_cachel_complete(cachel, domain, charsets);

	for (i = 0; i < NUM_LEADING_BYTES; i++) {
		if (charsets[i]) {
			Lisp_Object tmp =
				CHARSET_BY_LEADING_BYTE(i + MIN_LEADING_BYTE);
			return FACE_CACHEL_FONT(cachel, tmp);
		}
	}

	return Qnil;		/* NOT REACHED */
}

static void
text_query_geometry(Lisp_Object image_instance,
		    int *width, int *height,
		    enum image_instance_geometry disp, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	int descent = 0;

	query_string_geometry(IMAGE_INSTANCE_TEXT_STRING(ii),
			      IMAGE_INSTANCE_FACE(ii),
			      width, height, &descent, domain);

	/* The descent gets set as a side effect of querying the
	   geometry. */
	IMAGE_INSTANCE_TEXT_DESCENT(ii) = descent;
}

/* set the properties of a string */
static void text_update(Lisp_Object image_instance, Lisp_Object instantiator)
{
	Lisp_Object val = find_keyword_in_vector(instantiator, Q_data);

	if (!NILP(val)) {
		CHECK_STRING(val);
		XIMAGE_INSTANCE_TEXT_STRING(image_instance) = val;
	}
}

/****************************************************************************
 *                             formatted-string                             *
 ****************************************************************************/

static void formatted_string_validate(Lisp_Object instantiator)
{
	data_must_be_present(instantiator);
}

static int formatted_string_possible_dest_types(void)
{
	return IMAGE_TEXT_MASK;
}

static void
formatted_string_instantiate(Lisp_Object image_instance,
			     Lisp_Object instantiator,
			     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			     int dest_mask, Lisp_Object domain)
{
	/* #### implement this */
	warn_when_safe(Qunimplemented, Qnotice,
		       "`formatted-string' not yet implemented; assuming `string'");

	string_instantiate(image_instance, instantiator,
			   pointer_fg, pointer_bg, dest_mask, domain);
}

/************************************************************************/
/*                        pixmap file functions                         */
/************************************************************************/

/* If INSTANTIATOR refers to inline data, return Qnil.
   If INSTANTIATOR refers to data in a file, return the full filename
   if it exists; otherwise, return a cons of (filename).

   FILE_KEYWORD and DATA_KEYWORD are symbols specifying the
   keywords used to look up the file and inline data,
   respectively, in the instantiator.  Normally these would
   be Q_file and Q_data, but might be different for mask data. */

Lisp_Object
potential_pixmap_file_instantiator(Lisp_Object instantiator,
				   Lisp_Object file_keyword,
				   Lisp_Object data_keyword,
				   Lisp_Object console_type)
{
	Lisp_Object file;
	Lisp_Object data;

	assert(VECTORP(instantiator));

	data = find_keyword_in_vector(instantiator, data_keyword);
	file = find_keyword_in_vector(instantiator, file_keyword);

	if (!NILP(file) && NILP(data)) {
		Lisp_Object retval = MAYBE_LISP_CONTYPE_METH
		    (decode_console_type(console_type, ERROR_ME),
		     locate_pixmap_file, (file));

		if (!NILP(retval))
			return retval;
		else
			return Fcons(file, Qnil);	/* should have been file */
	}

	return Qnil;
}

Lisp_Object
simple_image_type_normalize(Lisp_Object inst, Lisp_Object console_type,
			    Lisp_Object image_type_tag)
{
	/* This function can call lisp */
	Lisp_Object file = Qnil;
	struct gcpro gcpro1, gcpro2;
	Lisp_Object alist = Qnil;

	GCPRO2(file, alist);

	/* Now, convert any file data into inline data.  At the end of this,
	   `data' will contain the inline data (if any) or Qnil, and `file'
	   will contain the name this data was derived from (if known) or
	   Qnil.

	   Note that if we cannot generate any regular inline data, we
	   skip out. */

	file = potential_pixmap_file_instantiator(inst, Q_file, Q_data,
						  console_type);

	if (CONSP(file))	/* failure locating filename */
		signal_double_file_error("Opening pixmap file",
					 "no such file or directory",
					 Fcar(file));

	if (NILP(file))		/* no conversion necessary */
		RETURN_UNGCPRO(inst);

	alist = tagged_vector_to_alist(inst);

	{
		Lisp_Object data = make_string_from_file(file);
		alist = remassq_no_quit(Q_file, alist);
		/* there can't be a :data at this point. */
		alist = Fcons(Fcons(Q_file, file),
			      Fcons(Fcons(Q_data, data), alist));
	}

	{
		Lisp_Object result =
		    alist_to_tagged_vector(image_type_tag, alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}
}

#ifdef HAVE_WINDOW_SYSTEM
/**********************************************************************
 *                             XBM                                    *
 **********************************************************************/

/* Check if DATA represents a valid inline XBM spec (i.e. a list
   of (width height bits), with checking done on the dimensions).
   If not, signal an error. */

static void check_valid_xbm_inline(Lisp_Object data)
{
	Lisp_Object width, height, bits;

	if (!CONSP(data) ||
	    !CONSP(XCDR(data)) ||
	    !CONSP(XCDR(XCDR(data))) || !NILP(XCDR(XCDR(XCDR(data)))))
		signal_simple_error("Must be list of 3 elements", data);

	width = XCAR(data);
	height = XCAR(XCDR(data));
	bits = XCAR(XCDR(XCDR(data)));

	CHECK_STRING(bits);

	if (!NATNUMP(width))
		signal_simple_error("Width must be a natural number", width);

	if (!NATNUMP(height))
		signal_simple_error("Height must be a natural number", height);

	if (((XINT(width) * XINT(height)) / 8) > XSTRING_CHAR_LENGTH(bits))
		signal_simple_error("data is too short for width and height",
				    vector3(width, height, bits));
}

/* Validate method for XBM's. */

static void xbm_validate(Lisp_Object instantiator)
{
	file_or_data_must_be_present(instantiator);
}

/* Given a filename that is supposed to contain XBM data, return
   the inline representation of it as (width height bits).  Return
   the hotspot through XHOT and YHOT, if those pointers are not 0.
   If there is no hotspot, XHOT and YHOT will contain -1.

   If the function fails:

   -- if OK_IF_DATA_INVALID is set and the data was invalid,
      return Qt.
   -- maybe return an error, or return Qnil.
 */

#ifdef HAVE_X_WINDOWS
#include <X11/Xlib.h>
#else
#define XFree(data) free(data)
#endif

Lisp_Object
bitmap_to_lisp_data(Lisp_Object name, int *xhot, int *yhot,
		    int ok_if_data_invalid)
{
	unsigned int w, h;
	Extbyte *data;
	int result;
	const char *filename_ext;

	LISP_STRING_TO_EXTERNAL(name, filename_ext, Qfile_name);
	result = read_bitmap_data_from_file(filename_ext, &w, &h,
					    (unsigned char **)((void*)&data),
					    xhot, yhot);

	if (result == BitmapSuccess) {
		Lisp_Object retval;
		int len = (w + 7) / 8 * h;

		retval = list3(make_int(w), make_int(h),
			       make_ext_string(data, len, Qbinary));
		XFree(data);
		return retval;
	}

	switch (result) {
	case BitmapOpenFailed:
		{
			/* should never happen */
			signal_double_file_error("Opening bitmap file",
						 "no such file or directory",
						 name);
		}
	case BitmapFileInvalid:
		{
			if (ok_if_data_invalid)
				return Qt;
			signal_double_file_error("Reading bitmap file",
						 "invalid data in file", name);
		}
	case BitmapNoMemory:
		{
			signal_double_file_error("Reading bitmap file",
						 "out of memory", name);
		}
	default:
		{
			signal_double_file_error_2("Reading bitmap file",
						   "unknown error code",
						   make_int(result), name);
		}
	}

	return Qnil;		/* not reached */
}

Lisp_Object
xbm_mask_file_munging(Lisp_Object alist, Lisp_Object file,
		      Lisp_Object mask_file, Lisp_Object console_type)
{
	/* This is unclean but it's fairly standard -- a number of the
	   bitmaps in /usr/include/X11/bitmaps use it -- so we support
	   it. */
	if (NILP(mask_file)
	    /* don't override explicitly specified mask data. */
	    && NILP(assq_no_quit(Q_mask_data, alist))
	    && !NILP(file)) {
		mask_file = MAYBE_LISP_CONTYPE_METH
		    (decode_console_type(console_type, ERROR_ME),
		     locate_pixmap_file, (concat2(file, build_string("Mask"))));
		if (NILP(mask_file))
			mask_file = MAYBE_LISP_CONTYPE_METH
			    (decode_console_type(console_type, ERROR_ME),
			     locate_pixmap_file,
			     (concat2(file, build_string("msk"))));
	}

	if (!NILP(mask_file)) {
		Lisp_Object mask_data = bitmap_to_lisp_data(mask_file, 0, 0, 0);
		alist = remassq_no_quit(Q_mask_file, alist);
		/* there can't be a :mask-data at this point. */
		alist = Fcons(Fcons(Q_mask_file, mask_file),
			      Fcons(Fcons(Q_mask_data, mask_data), alist));
	}

	return alist;
}

/* Normalize method for XBM's. */

static Lisp_Object
xbm_normalize(Lisp_Object inst, Lisp_Object console_type, Lisp_Object dest_mask)
{
	Lisp_Object file = Qnil, mask_file = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	Lisp_Object alist = Qnil;

	GCPRO3(file, mask_file, alist);

	/* Now, convert any file data into inline data for both the regular
	   data and the mask data.  At the end of this, `data' will contain
	   the inline data (if any) or Qnil, and `file' will contain
	   the name this data was derived from (if known) or Qnil.
	   Likewise for `mask_file' and `mask_data'.

	   Note that if we cannot generate any regular inline data, we
	   skip out. */

	file = potential_pixmap_file_instantiator(inst, Q_file, Q_data,
						  console_type);
	mask_file = potential_pixmap_file_instantiator(inst, Q_mask_file,
						       Q_mask_data,
						       console_type);

	if (CONSP(file))	/* failure locating filename */
		signal_double_file_error("Opening bitmap file",
					 "no such file or directory",
					 Fcar(file));

	if (NILP(file) && NILP(mask_file))	/* no conversion necessary */
		RETURN_UNGCPRO(inst);

	alist = tagged_vector_to_alist(inst);

	if (!NILP(file)) {
		int xhot, yhot;
		Lisp_Object data = bitmap_to_lisp_data(file, &xhot, &yhot, 0);
		alist = remassq_no_quit(Q_file, alist);
		/* there can't be a :data at this point. */
		alist = Fcons(Fcons(Q_file, file),
			      Fcons(Fcons(Q_data, data), alist));

		if (xhot != -1 && NILP(assq_no_quit(Q_hotspot_x, alist)))
			alist = Fcons(Fcons(Q_hotspot_x, make_int(xhot)),
				      alist);
		if (yhot != -1 && NILP(assq_no_quit(Q_hotspot_y, alist)))
			alist = Fcons(Fcons(Q_hotspot_y, make_int(yhot)),
				      alist);
	}

	alist = xbm_mask_file_munging(alist, file, mask_file, console_type);

	{
		Lisp_Object result = alist_to_tagged_vector(Qxbm, alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}
}

static int xbm_possible_dest_types(void)
{
	return
	    IMAGE_MONO_PIXMAP_MASK |
	    IMAGE_COLOR_PIXMAP_MASK | IMAGE_POINTER_MASK;
}

#endif

#ifdef HAVE_XFACE
/**********************************************************************
 *                             X-Face                                 *
 **********************************************************************/

static void xface_validate(Lisp_Object instantiator)
{
	file_or_data_must_be_present(instantiator);
}

static Lisp_Object
xface_normalize(Lisp_Object inst, Lisp_Object console_type,
		Lisp_Object dest_mask)
{
	/* This function can call lisp */
	Lisp_Object file = Qnil, mask_file = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	Lisp_Object alist = Qnil;

	GCPRO3(file, mask_file, alist);

	/* Now, convert any file data into inline data for both the regular
	   data and the mask data.  At the end of this, `data' will contain
	   the inline data (if any) or Qnil, and `file' will contain
	   the name this data was derived from (if known) or Qnil.
	   Likewise for `mask_file' and `mask_data'.

	   Note that if we cannot generate any regular inline data, we
	   skip out. */

	file = potential_pixmap_file_instantiator(inst, Q_file, Q_data,
						  console_type);
	mask_file = potential_pixmap_file_instantiator(inst, Q_mask_file,
						       Q_mask_data,
						       console_type);

	if (CONSP(file))	/* failure locating filename */
		signal_double_file_error("Opening bitmap file",
					 "no such file or directory",
					 Fcar(file));

	if (NILP(file) && NILP(mask_file))	/* no conversion necessary */
		RETURN_UNGCPRO(inst);

#ifdef HAVE_WINDOW_SYSTEM
	alist = tagged_vector_to_alist(inst);

	{
		Lisp_Object data = make_string_from_file(file);
		alist = remassq_no_quit(Q_file, alist);
		/* there can't be a :data at this point. */
		alist = Fcons(Fcons(Q_file, file),
			      Fcons(Fcons(Q_data, data), alist));
	}

	alist = xbm_mask_file_munging(alist, file, mask_file, console_type);

	{
		Lisp_Object result = alist_to_tagged_vector(Qxface, alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}
#else
	RETURN_UNGCPRO(Qnil);
#endif
}

static int xface_possible_dest_types(void)
{
	return
	    IMAGE_MONO_PIXMAP_MASK |
	    IMAGE_COLOR_PIXMAP_MASK | IMAGE_POINTER_MASK;
}

#endif				/* HAVE_XFACE */

#ifdef HAVE_XPM

/**********************************************************************
 *                             XPM                                    *
 **********************************************************************/

Lisp_Object pixmap_to_lisp_data(Lisp_Object name, int ok_if_data_invalid)
{
	char **data;
	int result;
	char *fname = 0;

	LISP_STRING_TO_EXTERNAL(name, fname, Qfile_name);
	result = XpmReadFileToData(fname, &data);

	if (result == XpmSuccess) {
		Lisp_Object retval = Qnil;
		struct buffer *old_buffer = current_buffer;
		Lisp_Object temp_buffer =
		    Fget_buffer_create(build_string(" *pixmap conversion*"));
		int elt;
		int height, width, ncolors;
		struct gcpro gcpro1, gcpro2, gcpro3;
		int speccount = specpdl_depth();

		GCPRO3(name, retval, temp_buffer);

		specbind(Qinhibit_quit, Qt);
		set_buffer_internal(XBUFFER(temp_buffer));
		Ferase_buffer(Qnil);

		buffer_insert_c_string(current_buffer, "/* XPM */\r");
		buffer_insert_c_string(current_buffer,
				       "static char *pixmap[] = {\r");

		sscanf(data[0], "%d %d %d", &height, &width, &ncolors);
		for (elt = 0; elt <= width + ncolors; elt++) {
			buffer_insert_c_string(current_buffer, "\"");
			buffer_insert_c_string(current_buffer, data[elt]);

			if (elt < width + ncolors)
				buffer_insert_c_string(current_buffer, "\",\r");
			else
				buffer_insert_c_string(current_buffer,
						       "\"};\r");
		}

		retval = Fbuffer_substring(Qnil, Qnil, Qnil);
		XpmFree(data);

		set_buffer_internal(old_buffer);
		unbind_to(speccount, Qnil);

		RETURN_UNGCPRO(retval);
	}

	switch (result) {
	case XpmFileInvalid:
		{
			if (ok_if_data_invalid)
				return Qt;
			signal_image_error("invalid XPM data in file", name);
		}
	case XpmNoMemory:
		{
			signal_double_file_error("Reading pixmap file",
						 "out of memory", name);
		}
	case XpmOpenFailed:
		{
			/* should never happen? */
			signal_double_file_error("Opening pixmap file",
						 "no such file or directory",
						 name);
		}
	default:
		{
			signal_double_file_error_2("Parsing pixmap file",
						   "unknown error code",
						   make_int(result), name);
			break;
		}
	}

	return Qnil;		/* not reached */
}

static void check_valid_xpm_color_symbols(Lisp_Object data)
{
	Lisp_Object rest;

	for (rest = data; !NILP(rest); rest = XCDR(rest)) {
		if (!CONSP(rest) ||
		    !CONSP(XCAR(rest)) ||
		    !STRINGP(XCAR(XCAR(rest))) ||
		    (!STRINGP(XCDR(XCAR(rest))) &&
		     !COLOR_SPECIFIERP(XCDR(XCAR(rest)))))
			signal_simple_error("Invalid color symbol alist", data);
	}
}

static void xpm_validate(Lisp_Object instantiator)
{
	file_or_data_must_be_present(instantiator);
}

Lisp_Object Vxpm_color_symbols;

Lisp_Object evaluate_xpm_color_symbols(void)
{
	Lisp_Object rest, results = Qnil;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(rest, results);
	for (rest = Vxpm_color_symbols; !NILP(rest); rest = XCDR(rest)) {
		Lisp_Object name, value, cons;

		CHECK_CONS(rest);
		cons = XCAR(rest);
		CHECK_CONS(cons);
		name = XCAR(cons);
		CHECK_STRING(name);
		value = XCDR(cons);
		CHECK_CONS(value);
		value = XCAR(value);
		value = Feval(value);
		if (NILP(value))
			continue;
		if (!STRINGP(value) && !COLOR_SPECIFIERP(value))
			signal_simple_error
			    ("Result from xpm-color-symbols eval must be nil, string, or color",
			     value);
		results = Fcons(Fcons(name, value), results);
	}
	UNGCPRO;		/* no more evaluation */
	return results;
}

static Lisp_Object
xpm_normalize(Lisp_Object inst, Lisp_Object console_type, Lisp_Object dest_mask)
{
	Lisp_Object file = Qnil;
	Lisp_Object color_symbols;
	struct gcpro gcpro1, gcpro2;
	Lisp_Object alist = Qnil;

	GCPRO2(file, alist);

	/* Now, convert any file data into inline data.  At the end of this,
	   `data' will contain the inline data (if any) or Qnil, and
	   `file' will contain the name this data was derived from (if
	   known) or Qnil.

	   Note that if we cannot generate any regular inline data, we
	   skip out. */

	file = potential_pixmap_file_instantiator(inst, Q_file, Q_data,
						  console_type);

	if (CONSP(file))	/* failure locating filename */
		signal_double_file_error("Opening pixmap file",
					 "no such file or directory",
					 Fcar(file));

	color_symbols = find_keyword_in_vector_or_given(inst, Q_color_symbols,
							Qunbound);

	if (NILP(file) && !UNBOUNDP(color_symbols))
		/* no conversion necessary */
		RETURN_UNGCPRO(inst);

	alist = tagged_vector_to_alist(inst);

	if (!NILP(file)) {
		Lisp_Object data = pixmap_to_lisp_data(file, 0);
		alist = remassq_no_quit(Q_file, alist);
		/* there can't be a :data at this point. */
		alist = Fcons(Fcons(Q_file, file),
			      Fcons(Fcons(Q_data, data), alist));
	}

	if (UNBOUNDP(color_symbols)) {
		color_symbols = evaluate_xpm_color_symbols();
		alist = Fcons(Fcons(Q_color_symbols, color_symbols), alist);
	}

	{
		Lisp_Object result = alist_to_tagged_vector(Qxpm, alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}
}

static int xpm_possible_dest_types(void)
{
	return
	    IMAGE_MONO_PIXMAP_MASK |
	    IMAGE_COLOR_PIXMAP_MASK | IMAGE_POINTER_MASK;
}

#endif				/* HAVE_XPM */

/****************************************************************************
 *                         Image Specifier Object                           *
 ****************************************************************************/

DEFINE_SPECIFIER_TYPE(image);

static void image_create(Lisp_Object obj)
{
	Lisp_Specifier *image = XIMAGE_SPECIFIER(obj);

	IMAGE_SPECIFIER_ALLOWED(image) = ~0;	/* all are allowed */
	IMAGE_SPECIFIER_ATTACHEE(image) = Qnil;
	IMAGE_SPECIFIER_ATTACHEE_PROPERTY(image) = Qnil;
}

static void image_mark(Lisp_Object obj)
{
	Lisp_Specifier *image = XIMAGE_SPECIFIER(obj);

	mark_object(IMAGE_SPECIFIER_ATTACHEE(image));
	mark_object(IMAGE_SPECIFIER_ATTACHEE_PROPERTY(image));
}

static int instantiator_eq_equal(Lisp_Object obj1, Lisp_Object obj2)
{
	if (EQ(obj1, obj2))
		return 1;

	else if (CONSP(obj1) && CONSP(obj2)) {
		return instantiator_eq_equal(XCAR(obj1), XCAR(obj2))
		    && instantiator_eq_equal(XCDR(obj1), XCDR(obj2));
	}
	return 0;
}

static hcode_t instantiator_eq_hash(Lisp_Object obj)
{
	if (CONSP(obj)) {
		/* no point in worrying about tail recursion, since we're not
		   going very deep */
		return HASH2(instantiator_eq_hash(XCAR(obj)),
			     instantiator_eq_hash(XCDR(obj)));
	}
	return LISP_HASH(obj);
}

/* We need a special hash table for storing image instances. */
Lisp_Object make_image_instance_cache_hash_table(void)
{
	return make_general_lisp_hash_table
		(instantiator_eq_hash, instantiator_eq_equal,
		 30, -1.0, -1.0, HASH_TABLE_KEY_CAR_VALUE_WEAK);
}

static Lisp_Object image_instantiate_cache_result(Lisp_Object locative)
{
	/* locative = (instance instantiator . subtable)

	   So we are using the instantiator as the key and the instance as
	   the value. Since the hashtable is key-weak this means that the
	   image instance will stay around as long as the instantiator stays
	   around. The instantiator is stored in the `image' slot of the
	   glyph, so as long as the glyph is marked the instantiator will be
	   as well and hence the cached image instance also. */
	Fputhash(XCAR(XCDR(locative)), XCAR(locative), XCDR(XCDR(locative)));
	free_cons(XCONS(XCDR(locative)));
	free_cons(XCONS(locative));
	return Qnil;
}

/* Given a specification for an image, return an instance of
   the image which matches the given instantiator and which can be
   displayed in the given domain. */

static Lisp_Object
image_instantiate(Lisp_Object specifier, Lisp_Object matchspec,
		  Lisp_Object domain, Lisp_Object instantiator,
		  Lisp_Object depth)
{
	Lisp_Object glyph =
	    IMAGE_SPECIFIER_ATTACHEE(XIMAGE_SPECIFIER(specifier));
	int dest_mask = XIMAGE_SPECIFIER_ALLOWED(specifier);
	int pointerp = dest_mask & image_instance_type_to_mask(IMAGE_POINTER);

	if (IMAGE_INSTANCEP(instantiator)) {
		/* make sure that the image instance's governing domain and type are
		   matching. */
		Lisp_Object governing_domain =
		    XIMAGE_INSTANCE_DOMAIN(instantiator);

		if ((DEVICEP(governing_domain)
		     && EQ(governing_domain, DOMAIN_DEVICE(domain)))
		    || (FRAMEP(governing_domain)
			&& EQ(governing_domain, DOMAIN_FRAME(domain)))
		    || (WINDOWP(governing_domain)
			&& EQ(governing_domain, DOMAIN_WINDOW(domain)))) {
			int mask =
			    image_instance_type_to_mask(XIMAGE_INSTANCE_TYPE
							(instantiator));
			if (mask & dest_mask)
				return instantiator;
			else
				signal_simple_error
				    ("Type of image instance not allowed here",
				     instantiator);
		} else
			signal_simple_error_2("Wrong domain for image instance",
					      instantiator, domain);
	}
	/* How ugly !! An image instanciator that uses a kludgy syntax to snarf in
	   face properties. There's a design flaw here. -- didier */
	else if (VECTORP(instantiator)
		 && EQ(INSTANTIATOR_TYPE(instantiator), Qinherit)) {
		assert(XVECTOR_LENGTH(instantiator) == 3);
		return (FACE_PROPERTY_INSTANCE
			(Fget_face(XVECTOR_DATA(instantiator)[2]),
			 Qbackground_pixmap, domain, 1, depth));
	} else {
		Lisp_Object instance = Qnil;
		Lisp_Object subtable = Qnil;
		/* #### Should this be GCPRO'd? */
		Lisp_Object hash_key = Qnil;
		Lisp_Object pointer_fg = Qnil;
		Lisp_Object pointer_bg = Qnil;
		Lisp_Object governing_domain =
		    get_image_instantiator_governing_domain(instantiator,
							    domain);
		struct gcpro gcpro1;

		GCPRO1(instance);

		/* We have to put subwindow, widget and text image instances in
		   a per-window cache so that we can see the same glyph in
		   different windows. We use governing_domain to determine the type
		   of image_instance that will be created. */

		if (pointerp) {
			pointer_fg = FACE_FOREGROUND(Vpointer_face, domain);
			pointer_bg = FACE_BACKGROUND(Vpointer_face, domain);
			hash_key = list4(glyph, INSTANTIATOR_TYPE(instantiator),
					 pointer_fg, pointer_bg);
		} else
			/* We cannot simply key on the glyph since fallbacks could use
			   the same glyph but have a totally different instantiator
			   type. Thus we key on the glyph and the type (but not any
			   other parts of the instantiator. */
			hash_key =
			    list2(glyph, INSTANTIATOR_TYPE(instantiator));

		/* First look in the device cache. */
		if (DEVICEP(governing_domain)) {
			subtable = Fgethash(make_int(dest_mask),
					    XDEVICE(governing_domain)->
					    image_instance_cache, Qunbound);
			if (UNBOUNDP(subtable)) {
				/* For the image instance cache, we do comparisons with
				   EQ rather than with EQUAL, as we do for color and
				   font names.  The reasons are:

				   1) pixmap data can be very long, and thus the hashing
				   and comparing will take awhile.

				   2) It's not so likely that we'll run into things that
				   are EQUAL but not EQ (that can happen a lot with
				   faces, because their specifiers are copied around);
				   but pixmaps tend not to be in faces.

				   However, if the image-instance could be a pointer, we
				   have to use EQUAL because we massaged the
				   instantiator into a cons3 also containing the
				   foreground and background of the pointer face.  */
				subtable =
				    make_image_instance_cache_hash_table();

				Fputhash(make_int(dest_mask), subtable,
					 XDEVICE(governing_domain)->
					 image_instance_cache);
				instance = Qunbound;
			} else {
				instance =
				    Fgethash(hash_key, subtable, Qunbound);
			}
		} else if (WINDOWP(governing_domain)) {
			/* Subwindows have a per-window cache and have to be treated
			   differently. */
			instance =
			    Fgethash(hash_key,
				     XWINDOW(governing_domain)->
				     subwindow_instance_cache, Qunbound);
		} else
			abort();	/* We're not allowed anything else currently. */

		/* If we don't have an instance at this point then create
		   one. */
		if (UNBOUNDP(instance)) {
			Lisp_Object locative = noseeum_cons(Qnil,
							    noseeum_cons
							    (hash_key,
							     DEVICEP
							     (governing_domain)
							     ? subtable :
							     XWINDOW
							     (governing_domain)
							     ->
							     subwindow_instance_cache));
			int speccount = specpdl_depth();

			/* Make sure we cache the failures, too.  Use an
			   unwind-protect to catch such errors.  If we fail, the
			   unwind-protect records nil in the hash table.  If we
			   succeed, we change the car of the locative to the
			   resulting instance, which gets recorded instead. */
			record_unwind_protect(image_instantiate_cache_result,
					      locative);
			instance =
			    instantiate_image_instantiator(governing_domain,
							   domain, instantiator,
							   pointer_fg,
							   pointer_bg,
							   dest_mask, glyph);

			/* We need a per-frame cache for redisplay. */
			cache_subwindow_instance_in_frame_maybe(instance);

			Fsetcar(locative, instance);
#ifdef ERROR_CHECK_GLYPHS
			if (image_instance_type_to_mask
			    (XIMAGE_INSTANCE_TYPE(instance))
			    & (IMAGE_SUBWINDOW_MASK | IMAGE_WIDGET_MASK))
				assert(EQ(XIMAGE_INSTANCE_FRAME(instance),
					  DOMAIN_FRAME(domain)));
#endif
			unbind_to(speccount, Qnil);
#ifdef ERROR_CHECK_GLYPHS
			if (image_instance_type_to_mask
			    (XIMAGE_INSTANCE_TYPE(instance))
			    & (IMAGE_SUBWINDOW_MASK | IMAGE_WIDGET_MASK))
				assert(EQ(Fgethash(hash_key,
						   XWINDOW(governing_domain)
						   ->subwindow_instance_cache,
						   Qunbound), instance));
#endif
		} else if (NILP(instance))
			signal_simple_error
			    ("Can't instantiate image (probably cached)",
			     instantiator);
		/* We found an instance. However, because we are using the glyph
		   as the hash key instead of the instantiator, the current
		   instantiator may not be the same as the original. Thus we
		   must update the instance based on the new
		   instantiator. Preserving instance identity like this is
		   important to stop excessive window system widget creation and
		   deletion - and hence flashing. */
		else {
			/* #### This function should be able to cope with *all*
			   changes to the instantiator, but currently only copes
			   with the most used properties. This means that it is
			   possible to make changes that don't get reflected in the
			   display. */
			update_image_instance(instance, instantiator);
			free_list(hash_key);
		}

#ifdef ERROR_CHECK_GLYPHS
		if (image_instance_type_to_mask(XIMAGE_INSTANCE_TYPE(instance))
		    & (IMAGE_SUBWINDOW_MASK | IMAGE_WIDGET_MASK))
			assert(EQ(XIMAGE_INSTANCE_FRAME(instance),
				  DOMAIN_FRAME(domain)));
#endif
		ERROR_CHECK_IMAGE_INSTANCE(instance);
		RETURN_UNGCPRO(instance);
	}

	abort();
	return Qnil;		/* not reached */
}

/* Validate an image instantiator. */

static void image_validate(Lisp_Object instantiator)
{
	if (IMAGE_INSTANCEP(instantiator) || STRINGP(instantiator))
		return;
	else if (VECTORP(instantiator)) {
		Lisp_Object *elt = XVECTOR_DATA(instantiator);
		int instantiator_len = XVECTOR_LENGTH(instantiator);
		struct image_instantiator_methods *meths;
		Lisp_Object already_seen = Qnil;
		struct gcpro gcpro1;
		int i;

		if (instantiator_len < 1)
			signal_simple_error("Vector length must be at least 1",
					    instantiator);

		meths = decode_image_instantiator_format(elt[0], ERROR_ME);
		if (!(instantiator_len & 1))
			signal_simple_error
			    ("Must have alternating keyword/value pairs",
			     instantiator);

		GCPRO1(already_seen);

		for (i = 1; i < instantiator_len; i += 2) {
			Lisp_Object keyword = elt[i];
			Lisp_Object value = elt[i + 1];
			int j;

			CHECK_SYMBOL(keyword);
			if (!SYMBOL_IS_KEYWORD(keyword))
				signal_simple_error
				    ("Symbol must begin with a colon", keyword);

			for (j = 0; j < Dynarr_length(meths->keywords); j++)
				if (EQ
				    (keyword,
				     Dynarr_at(meths->keywords, j).keyword))
					break;

			if (j == Dynarr_length(meths->keywords))
				signal_simple_error("Unrecognized keyword",
						    keyword);

			if (!Dynarr_at(meths->keywords, j).multiple_p) {
				if (!NILP(memq_no_quit(keyword, already_seen)))
					signal_simple_error
					    ("Keyword may not appear more than once",
					     keyword);
				already_seen = Fcons(keyword, already_seen);
			}

			(Dynarr_at(meths->keywords, j).validate) (value);
		}

		UNGCPRO;

		MAYBE_IIFORMAT_METH(meths, validate, (instantiator));
	} else
		signal_simple_error("Must be string or vector", instantiator);
}

static void image_after_change(Lisp_Object specifier, Lisp_Object locale)
{
	Lisp_Object attachee =
	    IMAGE_SPECIFIER_ATTACHEE(XIMAGE_SPECIFIER(specifier));
	Lisp_Object property =
	    IMAGE_SPECIFIER_ATTACHEE_PROPERTY(XIMAGE_SPECIFIER(specifier));
	if (FACEP(attachee)) {
		face_property_was_changed(attachee, property, locale);
		if (BUFFERP(locale))
			XBUFFER(locale)->buffer_local_face_property = 1;
	} else if (GLYPHP(attachee))
		glyph_property_was_changed(attachee, property, locale);
}

void
set_image_attached_to(Lisp_Object obj, Lisp_Object face_or_glyph,
		      Lisp_Object property)
{
	Lisp_Specifier *image = XIMAGE_SPECIFIER(obj);

	IMAGE_SPECIFIER_ATTACHEE(image) = face_or_glyph;
	IMAGE_SPECIFIER_ATTACHEE_PROPERTY(image) = property;
}

static Lisp_Object
image_going_to_add(Lisp_Object specifier, Lisp_Object locale,
		   Lisp_Object tag_set, Lisp_Object instantiator)
{
	Lisp_Object possible_console_types = Qnil;
	Lisp_Object rest;
	Lisp_Object retlist = Qnil;
	struct gcpro gcpro1, gcpro2;

	LIST_LOOP(rest, Vconsole_type_list) {
		Lisp_Object contype = XCAR(rest);
		if (!NILP(memq_no_quit(contype, tag_set)))
			possible_console_types =
			    Fcons(contype, possible_console_types);
	}

	if (XINT(Flength(possible_console_types)) > 1)
		/* two conflicting console types specified */
		return Qnil;

	if (NILP(possible_console_types))
		possible_console_types = Vconsole_type_list;

	GCPRO2(retlist, possible_console_types);

	LIST_LOOP(rest, possible_console_types) {
		Lisp_Object contype = XCAR(rest);
		Lisp_Object newinst = call_with_suspended_errors
		    ((lisp_fn_t) normalize_image_instantiator,
		     Qnil, Qimage, ERROR_ME_NOT, 3, instantiator, contype,
		     make_int(XIMAGE_SPECIFIER_ALLOWED(specifier)));

		if (!NILP(newinst)) {
			Lisp_Object newtag;
			if (NILP(memq_no_quit(contype, tag_set)))
				newtag = Fcons(contype, tag_set);
			else
				newtag = tag_set;
			retlist = Fcons(Fcons(newtag, newinst), retlist);
		}
	}

	UNGCPRO;

	return retlist;
}

/* Copy an image instantiator. We can't use Fcopy_tree since widgets
   may contain circular references which would send Fcopy_tree into
   infloop death. */
static Lisp_Object image_copy_vector_instantiator(Lisp_Object instantiator)
{
	int i;
	struct image_instantiator_methods *meths;
	Lisp_Object *elt;
	int instantiator_len;

	CHECK_VECTOR(instantiator);

	instantiator = Fcopy_sequence(instantiator);
	elt = XVECTOR_DATA(instantiator);
	instantiator_len = XVECTOR_LENGTH(instantiator);

	meths = decode_image_instantiator_format(elt[0], ERROR_ME);

	for (i = 1; i < instantiator_len; i += 2) {
		int j;
		Lisp_Object keyword = elt[i];
		Lisp_Object value = elt[i + 1];

		/* Find the keyword entry. */
		for (j = 0; j < Dynarr_length(meths->keywords); j++) {
			if (EQ(keyword, Dynarr_at(meths->keywords, j).keyword))
				break;
		}

		/* Only copy keyword values that should be copied. */
		if (Dynarr_at(meths->keywords, j).copy_p
		    && (CONSP(value) || VECTORP(value))) {
			elt[i + 1] = Fcopy_tree(value, Qt);
		}
	}

	return instantiator;
}

static Lisp_Object image_copy_instantiator(Lisp_Object arg)
{
	if (CONSP(arg)) {
		Lisp_Object rest;
		rest = arg = Fcopy_sequence(arg);
		while (CONSP(rest)) {
			Lisp_Object elt = XCAR(rest);
			if (CONSP(elt))
				XCAR(rest) = Fcopy_tree(elt, Qt);
			else if (VECTORP(elt))
				XCAR(rest) =
				    image_copy_vector_instantiator(elt);
			if (VECTORP(XCDR(rest)))	/* hack for (a b . [c d]) */
				XCDR(rest) = Fcopy_tree(XCDR(rest), Qt);
			rest = XCDR(rest);
		}
	} else if (VECTORP(arg)) {
		arg = image_copy_vector_instantiator(arg);
	}
	return arg;
}

DEFUN("image-specifier-p", Fimage_specifier_p, 1, 1, 0,	/*
Return non-nil if OBJECT is an image specifier.
See `make-image-specifier' for a description of image instantiators.
*/
      (object))
{
	return IMAGE_SPECIFIERP(object) ? Qt : Qnil;
}

/****************************************************************************
 *                             Glyph Object                                 *
 ****************************************************************************/

static Lisp_Object mark_glyph(Lisp_Object obj)
{
	Lisp_Glyph *glyph = XGLYPH(obj);

	mark_object(glyph->image);
	mark_object(glyph->contrib_p);
	mark_object(glyph->baseline);
	mark_object(glyph->face);

	return glyph->plist;
}

static void
print_glyph(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Glyph *glyph = XGLYPH(obj);
	if (print_readably)
		error("printing unreadable object #<glyph 0x%x>",
		      glyph->header.uid);

	write_c_string("#<glyph (", printcharfun);
	print_internal(Fglyph_type(obj), printcharfun, 0);
	write_c_string(") ", printcharfun);
	print_internal(glyph->image, printcharfun, 1);
	write_fmt_str(printcharfun, "0x%x>", glyph->header.uid);
}

/* Glyphs are equal if all of their display attributes are equal.  We
   don't compare names or doc-strings, because that would make equal
   be eq.

   This isn't concerned with "unspecified" attributes, that's what
   #'glyph-differs-from-default-p is for. */
static int glyph_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Glyph *g1 = XGLYPH(obj1);
	Lisp_Glyph *g2 = XGLYPH(obj2);

	depth++;

	return (internal_equal(g1->image, g2->image, depth) &&
		internal_equal(g1->contrib_p, g2->contrib_p, depth) &&
		internal_equal(g1->baseline, g2->baseline, depth) &&
		internal_equal(g1->face, g2->face, depth) &&
		!plists_differ(g1->plist, g2->plist, 0, 0, depth + 1));
}

static unsigned long glyph_hash(Lisp_Object obj, int depth)
{
	depth++;

	/* No need to hash all of the elements; that would take too long.
	   Just hash the most common ones. */
	return HASH2(internal_hash(XGLYPH(obj)->image, depth),
		     internal_hash(XGLYPH(obj)->face, depth));
}

static Lisp_Object glyph_getprop(Lisp_Object obj, Lisp_Object prop)
{
	Lisp_Glyph *g = XGLYPH(obj);

	if (EQ(prop, Qimage))
		return g->image;
	if (EQ(prop, Qcontrib_p))
		return g->contrib_p;
	if (EQ(prop, Qbaseline))
		return g->baseline;
	if (EQ(prop, Qface))
		return g->face;

	return external_plist_get(&g->plist, prop, 0, ERROR_ME);
}

static int glyph_putprop(Lisp_Object obj, Lisp_Object prop, Lisp_Object value)
{
	if (EQ(prop, Qimage) || EQ(prop, Qcontrib_p) || EQ(prop, Qbaseline))
		return 0;

	if (EQ(prop, Qface)) {
		XGLYPH(obj)->face = Fget_face(value);
		return 1;
	}

	external_plist_put(&XGLYPH(obj)->plist, prop, value, 0, ERROR_ME);
	return 1;
}

static int glyph_remprop(Lisp_Object obj, Lisp_Object prop)
{
	if (EQ(prop, Qimage) || EQ(prop, Qcontrib_p) || EQ(prop, Qbaseline))
		return -1;

	if (EQ(prop, Qface)) {
		XGLYPH(obj)->face = Qnil;
		return 1;
	}

	return external_remprop(&XGLYPH(obj)->plist, prop, 0, ERROR_ME);
}

static Lisp_Object glyph_plist(Lisp_Object obj)
{
	Lisp_Glyph *glyph = XGLYPH(obj);
	Lisp_Object result = glyph->plist;

	result = cons3(Qface, glyph->face, result);
	result = cons3(Qbaseline, glyph->baseline, result);
	result = cons3(Qcontrib_p, glyph->contrib_p, result);
	result = cons3(Qimage, glyph->image, result);

	return result;
}

static const struct lrecord_description glyph_description[] = {
	{XD_LISP_OBJECT, offsetof(Lisp_Glyph, image)},
	{XD_LISP_OBJECT, offsetof(Lisp_Glyph, contrib_p)},
	{XD_LISP_OBJECT, offsetof(Lisp_Glyph, baseline)},
	{XD_LISP_OBJECT, offsetof(Lisp_Glyph, face)},
	{XD_LISP_OBJECT, offsetof(Lisp_Glyph, plist)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("glyph", glyph,
					 mark_glyph, print_glyph, 0,
					 glyph_equal, glyph_hash,
					 glyph_description, glyph_getprop,
					 glyph_putprop, glyph_remprop,
					 glyph_plist, Lisp_Glyph);

Lisp_Object
allocate_glyph(enum glyph_type type,
	       void (*after_change) (Lisp_Object glyph, Lisp_Object property,
				     Lisp_Object locale))
{
	/* This function can GC */
	Lisp_Object obj = Qnil;
	Lisp_Glyph *g = alloc_lcrecord_type(Lisp_Glyph, &lrecord_glyph);

	g->type = type;
	g->image = Fmake_specifier(Qimage);	/* This function can GC */
	g->dirty = 0;
	switch (g->type) {
	case GLYPH_BUFFER:
		XIMAGE_SPECIFIER_ALLOWED(g->image) =
		    IMAGE_NOTHING_MASK | IMAGE_TEXT_MASK
		    | IMAGE_MONO_PIXMAP_MASK | IMAGE_COLOR_PIXMAP_MASK
		    | IMAGE_SUBWINDOW_MASK | IMAGE_WIDGET_MASK;
		break;
	case GLYPH_POINTER:
		XIMAGE_SPECIFIER_ALLOWED(g->image) =
		    IMAGE_NOTHING_MASK | IMAGE_POINTER_MASK;
		break;
	case GLYPH_ICON:
		XIMAGE_SPECIFIER_ALLOWED(g->image) =
		    IMAGE_NOTHING_MASK | IMAGE_MONO_PIXMAP_MASK
		    | IMAGE_COLOR_PIXMAP_MASK;
		break;
	case GLYPH_UNKNOWN:
	default:
		abort();
	}

	/* I think Fmake_specifier can GC.  I think set_specifier_fallback can GC. */
	/* We're getting enough reports of odd behavior in this area it seems */
	/* best to GCPRO everything. */
	{
		Lisp_Object tem1 = list1(Fcons(Qnil, Vthe_nothing_vector));
		Lisp_Object tem2 = list1(Fcons(Qnil, Qt));
		Lisp_Object tem3 = list1(Fcons(Qnil, Qnil));
		struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

		GCPRO4(obj, tem1, tem2, tem3);

		set_specifier_fallback(g->image, tem1);
		g->contrib_p = Fmake_specifier(Qboolean);
		set_specifier_fallback(g->contrib_p, tem2);
		/* #### should have a specifier for the following */
		g->baseline = Fmake_specifier(Qgeneric);
		set_specifier_fallback(g->baseline, tem3);
		g->face = Qnil;
		g->plist = Qnil;
		g->after_change = after_change;
		XSETGLYPH(obj, g);

		set_image_attached_to(g->image, obj, Qimage);
		UNGCPRO;
	}

	return obj;
}

static enum glyph_type decode_glyph_type(Lisp_Object type, Error_behavior errb)
{
	if (NILP(type))
		return GLYPH_BUFFER;

	if (ERRB_EQ(errb, ERROR_ME))
		CHECK_SYMBOL(type);

	if (EQ(type, Qbuffer))
		return GLYPH_BUFFER;
	if (EQ(type, Qpointer))
		return GLYPH_POINTER;
	if (EQ(type, Qicon))
		return GLYPH_ICON;

	maybe_signal_simple_error("Invalid glyph type", type, Qimage, errb);

	return GLYPH_UNKNOWN;
}

static int valid_glyph_type_p(Lisp_Object type)
{
	return !NILP(memq_no_quit(type, Vglyph_type_list));
}

DEFUN("valid-glyph-type-p", Fvalid_glyph_type_p, 1, 1, 0,	/*
Given a GLYPH-TYPE, return non-nil if it is valid.
Valid types are `buffer', `pointer', and `icon'.
*/
      (glyph_type))
{
	return valid_glyph_type_p(glyph_type) ? Qt : Qnil;
}

DEFUN("glyph-type-list", Fglyph_type_list, 0, 0, 0,	/*
Return a list of valid glyph types.
*/
      ())
{
	return Fcopy_sequence(Vglyph_type_list);
}

DEFUN("make-glyph-internal", Fmake_glyph_internal, 0, 1, 0,	/*
Create and return a new uninitialized glyph of type TYPE.

TYPE specifies the type of the glyph; this should be one of `buffer',
`pointer', or `icon', and defaults to `buffer'.  The type of the glyph
specifies in which contexts the glyph can be used, and controls the
allowable image types into which the glyph's image can be
instantiated.

`buffer' glyphs can be used as the begin-glyph or end-glyph of an
extent, in the modeline, and in the toolbar.  Their image can be
instantiated as `nothing', `mono-pixmap', `color-pixmap', `text',
and `subwindow'.

`pointer' glyphs can be used to specify the mouse pointer.  Their
image can be instantiated as `pointer'.

`icon' glyphs can be used to specify the icon used when a frame is
iconified.  Their image can be instantiated as `mono-pixmap' and
`color-pixmap'.
*/
      (type))
{
	enum glyph_type typeval = decode_glyph_type(type, ERROR_ME);
	return allocate_glyph(typeval, 0);
}

DEFUN("glyphp", Fglyphp, 1, 1, 0,	/*
Return non-nil if OBJECT is a glyph.

A glyph is an object used for pixmaps, widgets and the like.  It is used
in begin-glyphs and end-glyphs attached to extents, in marginal and textual
annotations, in overlay arrows (overlay-arrow-* variables), in toolbar
buttons, and the like.  Much more detailed information can be found at
`make-glyph'.  Its image is described using an image specifier --
see `make-image-specifier'.  See also `make-image-instance' for further
information.
*/
      (object))
{
	return GLYPHP(object) ? Qt : Qnil;
}

DEFUN("glyph-type", Fglyph_type, 1, 1, 0,	/*
Return the type of the given glyph.
The return value will be one of 'buffer, 'pointer, or 'icon.
*/
      (glyph))
{
	CHECK_GLYPH(glyph);
	switch (XGLYPH_TYPE(glyph)) {
	case GLYPH_UNKNOWN:
	default:
		abort();
		break;
	case GLYPH_BUFFER:
		return Qbuffer;
	case GLYPH_POINTER:
		return Qpointer;
	case GLYPH_ICON:
		return Qicon;
	}
	return Qnil;
}

Lisp_Object
glyph_image_instance(Lisp_Object glyph, Lisp_Object domain,
		     Error_behavior errb, int no_quit)
{
	Lisp_Object specifier = GLYPH_IMAGE(XGLYPH(glyph));

	/* This can never return Qunbound.  All glyphs have 'nothing as
	   a fallback. */
	Lisp_Object image_instance = specifier_instance(specifier, Qunbound,
							domain, errb, no_quit,
							0,
							Qzero);
	assert(!UNBOUNDP(image_instance));
	ERROR_CHECK_IMAGE_INSTANCE(image_instance);

	return image_instance;
}

static Lisp_Object
glyph_image_instance_maybe(Lisp_Object glyph_or_image, Lisp_Object window)
{
	Lisp_Object instance = glyph_or_image;

	if (GLYPHP(glyph_or_image))
		instance =
		    glyph_image_instance(glyph_or_image, window, ERROR_ME_NOT,
					 1);

	return instance;
}

/*****************************************************************************
 glyph_width

 Return the width of the given GLYPH on the given WINDOW.
 Calculations are done based on recursively querying the geometry of
 the associated image instances.
 ****************************************************************************/
unsigned short glyph_width(Lisp_Object glyph_or_image, Lisp_Object domain)
{
	Lisp_Object instance = glyph_image_instance_maybe(glyph_or_image,
							  domain);
	if (!IMAGE_INSTANCEP(instance))
		return 0;

	if (XIMAGE_INSTANCE_NEEDS_LAYOUT(instance))
		image_instance_layout(instance, IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY, domain);

	return XIMAGE_INSTANCE_WIDTH(instance);
}

DEFUN("glyph-width", Fglyph_width, 1, 2, 0,	/*
Return the width of GLYPH on WINDOW.
This may not be exact as it does not take into account all of the context
that redisplay will.
*/
      (glyph, window))
{
	XSETWINDOW(window, decode_window(window));
	CHECK_GLYPH(glyph);

	return make_int(glyph_width(glyph, window));
}

unsigned short glyph_ascent(Lisp_Object glyph_or_image, Lisp_Object domain)
{
	Lisp_Object instance = glyph_image_instance_maybe(glyph_or_image,
							  domain);
	if (!IMAGE_INSTANCEP(instance))
		return 0;

	if (XIMAGE_INSTANCE_NEEDS_LAYOUT(instance))
		image_instance_layout(instance, IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY, domain);

	if (XIMAGE_INSTANCE_TYPE(instance) == IMAGE_TEXT)
		return XIMAGE_INSTANCE_TEXT_ASCENT(instance);
	else
		return XIMAGE_INSTANCE_HEIGHT(instance);
}

unsigned short glyph_descent(Lisp_Object glyph_or_image, Lisp_Object domain)
{
	Lisp_Object instance = glyph_image_instance_maybe(glyph_or_image,
							  domain);
	if (!IMAGE_INSTANCEP(instance))
		return 0;

	if (XIMAGE_INSTANCE_NEEDS_LAYOUT(instance))
		image_instance_layout(instance, IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY, domain);

	if (XIMAGE_INSTANCE_TYPE(instance) == IMAGE_TEXT)
		return XIMAGE_INSTANCE_TEXT_DESCENT(instance);
	else
		return 0;
}

/* strictly a convenience function. */
unsigned short glyph_height(Lisp_Object glyph_or_image, Lisp_Object domain)
{
	Lisp_Object instance = glyph_image_instance_maybe(glyph_or_image,
							  domain);

	if (!IMAGE_INSTANCEP(instance))
		return 0;

	if (XIMAGE_INSTANCE_NEEDS_LAYOUT(instance))
		image_instance_layout(instance, IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNSPECIFIED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY,
				      IMAGE_UNCHANGED_GEOMETRY, domain);

	return XIMAGE_INSTANCE_HEIGHT(instance);
}

DEFUN("glyph-ascent", Fglyph_ascent, 1, 2, 0,	/*
Return the ascent value of GLYPH on WINDOW.
This may not be exact as it does not take into account all of the context
that redisplay will.
*/
      (glyph, window))
{
	XSETWINDOW(window, decode_window(window));
	CHECK_GLYPH(glyph);

	return make_int(glyph_ascent(glyph, window));
}

DEFUN("glyph-descent", Fglyph_descent, 1, 2, 0,	/*
Return the descent value of GLYPH on WINDOW.
This may not be exact as it does not take into account all of the context
that redisplay will.
*/
      (glyph, window))
{
	XSETWINDOW(window, decode_window(window));
	CHECK_GLYPH(glyph);

	return make_int(glyph_descent(glyph, window));
}

/* This is redundant but I bet a lot of people expect it to exist. */
DEFUN("glyph-height", Fglyph_height, 1, 2, 0,	/*
Return the height of GLYPH on WINDOW.
This may not be exact as it does not take into account all of the context
that redisplay will.
*/
      (glyph, window))
{
	XSETWINDOW(window, decode_window(window));
	CHECK_GLYPH(glyph);

	return make_int(glyph_height(glyph, window));
}

static void
set_glyph_dirty_p(Lisp_Object glyph_or_image, Lisp_Object window, int dirty)
{
	Lisp_Object instance = glyph_or_image;

	if (!NILP(glyph_or_image)) {
		if (GLYPHP(glyph_or_image)) {
			instance = glyph_image_instance(glyph_or_image, window,
							ERROR_ME_NOT, 1);
			XGLYPH_DIRTYP(glyph_or_image) = dirty;
		}

		if (!IMAGE_INSTANCEP(instance))
			return;

		XIMAGE_INSTANCE_DIRTYP(instance) = dirty;
	}
}

static void set_image_instance_dirty_p(Lisp_Object instance, int dirty)
{
	if (IMAGE_INSTANCEP(instance)) {
		XIMAGE_INSTANCE_DIRTYP(instance) = dirty;
		/* Now cascade up the hierarchy. */
		set_image_instance_dirty_p(XIMAGE_INSTANCE_PARENT(instance),
					   dirty);
	} else if (GLYPHP(instance)) {
		XGLYPH_DIRTYP(instance) = dirty;
	}
}

/* #### do we need to cache this info to speed things up? */

Lisp_Object glyph_baseline(Lisp_Object glyph, Lisp_Object domain)
{
	if (!GLYPHP(glyph))
		return Qnil;
	else {
		Lisp_Object retval =
		    specifier_instance_no_quit(GLYPH_BASELINE(XGLYPH(glyph)),
					       /* #### look into ERROR_ME_NOT */
					       Qunbound, domain, ERROR_ME_NOT,
					       0, Qzero);
		if (!NILP(retval) && !INTP(retval))
			retval = Qnil;
		else if (INTP(retval)) {
			if (XINT(retval) < 0)
				retval = Qzero;
			if (XINT(retval) > 100)
				retval = make_int(100);
		}
		return retval;
	}
}

Lisp_Object glyph_face(Lisp_Object glyph, Lisp_Object domain)
{
	/* #### Domain parameter not currently used but it will be */
	return GLYPHP(glyph) ? GLYPH_FACE(XGLYPH(glyph)) : Qnil;
}

int glyph_contrib_p(Lisp_Object glyph, Lisp_Object domain)
{
	if (!GLYPHP(glyph))
		return 0;
	else
		return !NILP(specifier_instance_no_quit
			     (GLYPH_CONTRIB_P(XGLYPH(glyph)), Qunbound, domain,
			      /* #### look into ERROR_ME_NOT */
			      ERROR_ME_NOT, 0, Qzero));
}

static void
glyph_property_was_changed(Lisp_Object glyph, Lisp_Object property,
			   Lisp_Object locale)
{
	if (XGLYPH(glyph)->after_change)
		(XGLYPH(glyph)->after_change) (glyph, property, locale);
}

void
glyph_query_geometry(Lisp_Object glyph_or_image, int *width, int *height,
		     enum image_instance_geometry disp, Lisp_Object domain)
{
	Lisp_Object instance = glyph_or_image;

	if (GLYPHP(glyph_or_image))
		instance =
		    glyph_image_instance(glyph_or_image, domain, ERROR_ME_NOT,
					 1);

	image_instance_query_geometry(instance, width, height, disp, domain);
}

void
glyph_do_layout(Lisp_Object glyph_or_image, int width, int height,
		int xoffset, int yoffset, Lisp_Object domain)
{
	Lisp_Object instance = glyph_or_image;

	if (GLYPHP(glyph_or_image))
		instance =
		    glyph_image_instance(glyph_or_image, domain, ERROR_ME_NOT,
					 1);

	image_instance_layout(instance, width, height, xoffset, yoffset,
			      domain);
}

/*****************************************************************************
 *                     glyph cachel functions	     *
 *****************************************************************************/

/* #### All of this is 95% copied from face cachels.  Consider
  consolidating.

  Why do we need glyph_cachels? Simply because a glyph_cachel captures
  per-window information about a particular glyph. A glyph itself is
  not created in any particular context, so if we were to rely on a
  glyph to tell us about its dirtiness we would not be able to reset
  the dirty flag after redisplaying it as it may exist in other
  contexts. When we have redisplayed we need to know which glyphs to
  reset the dirty flags on - the glyph_cachels give us a nice list we
  can iterate through doing this.  */
void mark_glyph_cachels(glyph_cachel_dynarr * elements)
{
	int elt;

	if (!elements)
		return;

	for (elt = 0; elt < Dynarr_length(elements); elt++) {
		struct glyph_cachel *cachel = Dynarr_atp(elements, elt);
		mark_object(cachel->glyph);
	}
}

static void
update_glyph_cachel_data(struct window *w, Lisp_Object glyph,
			 struct glyph_cachel *cachel)
{
	if (!cachel->updated || NILP(cachel->glyph) || !EQ(cachel->glyph, glyph)
	    || XGLYPH_DIRTYP(cachel->glyph)
	    || XFRAME(WINDOW_FRAME(w))->faces_changed) {
		Lisp_Object window, instance;

		XSETWINDOW(window, w);

		cachel->glyph = glyph;
		/* Speed things up slightly by grabbing the glyph instantiation
		   and passing it to the size functions. */
		instance = glyph_image_instance(glyph, window, ERROR_ME_NOT, 1);

		if (!IMAGE_INSTANCEP(instance))
			return;

		/* Mark text instance of the glyph dirty if faces have changed,
		   because its geometry might have changed. */
		invalidate_glyph_geometry_maybe(instance, w);

		/* #### Do the following 2 lines buy us anything? --kkm */
		XGLYPH_DIRTYP(glyph) = XIMAGE_INSTANCE_DIRTYP(instance);
		cachel->dirty = XGLYPH_DIRTYP(glyph);
		cachel->width = glyph_width(instance, window);
		cachel->ascent = glyph_ascent(instance, window);
		cachel->descent = glyph_descent(instance, window);
	}

	cachel->updated = 1;
}

static void add_glyph_cachel(struct window *w, Lisp_Object glyph)
{
	struct glyph_cachel new_cachel;

	xzero(new_cachel);
	new_cachel.glyph = Qnil;

	update_glyph_cachel_data(w, glyph, &new_cachel);
	Dynarr_add(w->glyph_cachels, new_cachel);
}

glyph_index get_glyph_cachel_index(struct window *w, Lisp_Object glyph)
{
	int elt;

	if (noninteractive)
		return 0;

	for (elt = 0; elt < Dynarr_length(w->glyph_cachels); elt++) {
		struct glyph_cachel *cachel = Dynarr_atp(w->glyph_cachels, elt);

		if (EQ(cachel->glyph, glyph) && !NILP(glyph)) {
			update_glyph_cachel_data(w, glyph, cachel);
			return elt;
		}
	}

	/* If we didn't find the glyph, add it and then return its index. */
	add_glyph_cachel(w, glyph);
	return elt;
}

void reset_glyph_cachels(struct window *w)
{
	Dynarr_reset(w->glyph_cachels);
	get_glyph_cachel_index(w, Vcontinuation_glyph);
	get_glyph_cachel_index(w, Vtruncation_glyph);
	get_glyph_cachel_index(w, Vhscroll_glyph);
	get_glyph_cachel_index(w, Vcontrol_arrow_glyph);
	get_glyph_cachel_index(w, Voctal_escape_glyph);
	get_glyph_cachel_index(w, Vinvisible_text_glyph);
}

void mark_glyph_cachels_as_not_updated(struct window *w)
{
	int elt;

	/* We need to have a dirty flag to tell if the glyph has changed.
	   We can check to see if each glyph variable is actually a
	   completely different glyph, though. */
#define FROB(glyph_obj, gindex)						\
  update_glyph_cachel_data (w, glyph_obj,				\
			      Dynarr_atp (w->glyph_cachels, gindex))

	FROB(Vcontinuation_glyph, CONT_GLYPH_INDEX);
	FROB(Vtruncation_glyph, TRUN_GLYPH_INDEX);
	FROB(Vhscroll_glyph, HSCROLL_GLYPH_INDEX);
	FROB(Vcontrol_arrow_glyph, CONTROL_GLYPH_INDEX);
	FROB(Voctal_escape_glyph, OCT_ESC_GLYPH_INDEX);
	FROB(Vinvisible_text_glyph, INVIS_GLYPH_INDEX);
#undef FROB

	for (elt = 0; elt < Dynarr_length(w->glyph_cachels); elt++) {
		Dynarr_atp(w->glyph_cachels, elt)->updated = 0;
	}
}

/* Unset the dirty bit on all the glyph cachels that have it. */
void mark_glyph_cachels_as_clean(struct window *w)
{
	int elt;
	Lisp_Object window;
	XSETWINDOW(window, w);
	for (elt = 0; elt < Dynarr_length(w->glyph_cachels); elt++) {
		struct glyph_cachel *cachel = Dynarr_atp(w->glyph_cachels, elt);
		cachel->dirty = 0;
		set_glyph_dirty_p(cachel->glyph, window, 0);
	}
}

#if defined MEMORY_USAGE_STATS && !(defined HAVE_BDWGC && defined EF_USE_BDWGC)

int
compute_glyph_cachel_usage(glyph_cachel_dynarr * glyph_cachels,
			   struct overhead_stats *ovstats)
{
	int total = 0;

	if (glyph_cachels)
		total += Dynarr_memory_usage(glyph_cachels, ovstats);

	return total;
}

#endif				/* MEMORY_USAGE_STATS */

/*****************************************************************************
 *                     subwindow cachel functions	     *
 *****************************************************************************/
/* Subwindows are curious in that you have to physically unmap them to
   not display them. It is problematic deciding what to do in
   redisplay. We have two caches - a per-window instance cache that
   keeps track of subwindows on a window, these are linked to their
   instantiator in the hashtable and when the instantiator goes away
   we want the instance to go away also. However we also have a
   per-frame instance cache that we use to determine if a subwindow is
   obscuring an area that we want to clear. We need to be able to flip
   through this quickly so a hashtable is not suitable hence the
   subwindow_cachels. This is a weak list so unreference instances
   will get deleted properly. */

/* redisplay in general assumes that drawing something will erase
   what was there before. unfortunately this does not apply to
   subwindows that need to be specifically unmapped in order to
   disappear. we take a brute force approach - on the basis that its
   cheap - and unmap all subwindows in a display line */

/* Put new instances in the frame subwindow cache. This is less costly than
   doing it every time something gets mapped, and deleted instances will be
   removed automatically. */
static void cache_subwindow_instance_in_frame_maybe(Lisp_Object instance)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(instance);
	if (!NILP(DOMAIN_FRAME(IMAGE_INSTANCE_DOMAIN(ii)))) {
		struct frame *f = DOMAIN_XFRAME(IMAGE_INSTANCE_DOMAIN(ii));
		XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE(f))
		    = Fcons(instance,
			    XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE(f)));
	}
}

/* Unmap and finalize all subwindow instances in the frame cache. This
   is necessary because GC will not guarantee the order things get
   deleted in and moreover, frame finalization deletes the window
   system windows before deleting SXEmacs windows, and hence
   subwindows.  */
int
unmap_subwindow_instance_cache_mapper(Lisp_Object key, Lisp_Object value,
				      void *finalize)
{
	/* value can be nil; we cache failures as well as successes */
	if (!NILP(value)) {
		struct frame *f = XFRAME(XIMAGE_INSTANCE_FRAME(value));
		unmap_subwindow(value);
		if (finalize) {
			/* In case GC doesn't catch up fast enough, remove from the frame
			   cache also. Otherwise code that checks the sanity of the instance
			   will fail. */
			XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE(f))
			    = delq_no_quit(value,
					   XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE
							   (f)));
			finalize_image_instance(XIMAGE_INSTANCE(value), 0);
		}
	}
	return 0;
}

static void finalize_all_subwindow_instances(struct window *w)
{
	if (!NILP(w->next))
		finalize_all_subwindow_instances(XWINDOW(w->next));
	if (!NILP(w->vchild))
		finalize_all_subwindow_instances(XWINDOW(w->vchild));
	if (!NILP(w->hchild))
		finalize_all_subwindow_instances(XWINDOW(w->hchild));

	elisp_maphash(unmap_subwindow_instance_cache_mapper,
		      w->subwindow_instance_cache, (void *)1);
}

void free_frame_subwindow_instances(struct frame *f)
{
	/* Make sure all instances are finalized. We have to do this via the
	   instance cache since some instances may be extant but not
	   displayed (and hence not in the frame cache). */
	finalize_all_subwindow_instances(XWINDOW(f->root_window));
}

/* Unmap all instances in the frame cache. */
void reset_frame_subwindow_instance_cache(struct frame *f)
{
	Lisp_Object rest;

	LIST_LOOP(rest, XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE(f))) {
		Lisp_Object value = XCAR(rest);
		unmap_subwindow(value);
	}
}

/*****************************************************************************
 *                              subwindow exposure ignorance                    *
 *****************************************************************************/
/* when we unmap subwindows the associated window system will generate
   expose events. This we do not want as redisplay already copes with
   the repainting necessary. Worse, we can get in an endless cycle of
   redisplay if we are not careful. Thus we keep a per-frame list of
   expose events that are going to come and ignore them as
   required. */

struct expose_ignore_blocktype {
	Blocktype_declare(struct expose_ignore);
} *the_expose_ignore_blocktype;

int
check_for_ignored_expose(struct frame *f, int x, int y, int width, int height)
{
	struct expose_ignore *ei, *prev;
	/* the ignore list is FIFO so we should generally get a match with
	   the first element in the list */
	for (ei = f->subwindow_exposures, prev = 0; ei; ei = ei->next) {
		/* Checking for exact matches just isn't good enough as we
		   might get exposures for partially obscured subwindows, thus
		   we have to check for overlaps. Being conservative, we will
		   check for exposures wholly contained by the subwindow - this
		   might give us what we want. */
		if (ei->x <= (unsigned)x && ei->y <= (unsigned)y
		    && ei->x + ei->width >= (unsigned)(x + width)
		    && ei->y + ei->height >= (unsigned)(y + height)) {
#ifdef DEBUG_WIDGETS
			stderr_out
			    ("ignored %d+%d, %dx%d for exposure %d+%d, %dx%d\n",
			     x, y, width, height, ei->x, ei->y, ei->width,
			     ei->height);
#endif
			if (!prev)
				f->subwindow_exposures = ei->next;
			else
				prev->next = ei->next;

			if (ei == f->subwindow_exposures_tail)
				f->subwindow_exposures_tail = prev;

			Blocktype_free(the_expose_ignore_blocktype, ei);
			return 1;
		}
		prev = ei;
	}
	return 0;
}

static void
register_ignored_expose(struct frame *f, int x, int y, int width, int height)
{
	if (!hold_ignored_expose_registration) {
		struct expose_ignore *ei;

		ei = Blocktype_alloc(the_expose_ignore_blocktype);

		ei->next = NULL;
		ei->x = x;
		ei->y = y;
		ei->width = width;
		ei->height = height;

		/* we have to add the exposure to the end of the list, since we
		   want to check the oldest events first. for speed we keep a record
		   of the end so that we can add right to it. */
		if (f->subwindow_exposures_tail) {
			f->subwindow_exposures_tail->next = ei;
		}
		if (!f->subwindow_exposures) {
			f->subwindow_exposures = ei;
		}
		f->subwindow_exposures_tail = ei;
	}
}

/****************************************************************************
 find_matching_subwindow

 See if there is a subwindow that completely encloses the requested
 area.
 ****************************************************************************/
int find_matching_subwindow(struct frame *f, int x, int y, int width,
			    int height)
{
	Lisp_Object rest;

	LIST_LOOP(rest, XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE(f))) {
		Lisp_Image_Instance *ii = XIMAGE_INSTANCE(XCAR(rest));

		if (IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(ii)
		    &&
		    IMAGE_INSTANCE_DISPLAY_X(ii) <= (unsigned)x
		    &&
		    IMAGE_INSTANCE_DISPLAY_Y(ii) <= (unsigned)y
		    && IMAGE_INSTANCE_DISPLAY_X(ii)
		    + IMAGE_INSTANCE_DISPLAY_WIDTH(ii) >= (unsigned)(x + width)
		    && IMAGE_INSTANCE_DISPLAY_Y(ii)
		    + IMAGE_INSTANCE_DISPLAY_HEIGHT(ii) >=
		    (unsigned)(y + height)) {
			return 1;
		}
	}
	return 0;
}

/*****************************************************************************
 *                              subwindow functions                          *
 *****************************************************************************/

/* Update the displayed characteristics of a subwindow. This function
   should generally only get called if the subwindow is actually
   dirty. */
void redisplay_subwindow(Lisp_Object subwindow)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(subwindow);
	int count = specpdl_depth();

	/* The update method is allowed to call eval.  Since it is quite
	   common for this function to get called from somewhere in
	   redisplay we need to make sure that quits are ignored.  Otherwise
	   Fsignal will abort. */
	specbind(Qinhibit_quit, Qt);

	ERROR_CHECK_IMAGE_INSTANCE(subwindow);

	if (WIDGET_IMAGE_INSTANCEP(subwindow)) {
		if (image_instance_changed(subwindow))
			redisplay_widget(subwindow);
		/* Reset the changed flags. */
		IMAGE_INSTANCE_WIDGET_FACE_CHANGED(ii) = 0;
		IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED(ii) = 0;
		IMAGE_INSTANCE_WIDGET_ACTION_OCCURRED(ii) = 0;
		IMAGE_INSTANCE_TEXT_CHANGED(ii) = 0;
	} else if (IMAGE_INSTANCE_TYPE(ii) == IMAGE_SUBWINDOW
		   && !NILP(IMAGE_INSTANCE_FRAME(ii))) {
		MAYBE_DEVMETH(DOMAIN_XDEVICE(ii->domain),
			      redisplay_subwindow, (ii));
	}

	IMAGE_INSTANCE_SIZE_CHANGED(ii) = 0;
	/* This function is typically called by redisplay just before
	   outputting the information to the screen. Thus we record a hash
	   of the output to determine whether on-screen is the same as
	   recorded structure. This approach has limitations in there is a
	   good chance that hash values will be different for the same
	   visual appearance. However, we would rather that then the other
	   way round - it simply means that we will get more displays than
	   we might need. We can get better hashing by making the depth
	   negative - currently it will recurse down 7 levels. */
	IMAGE_INSTANCE_DISPLAY_HASH(ii) = internal_hash(subwindow,
							IMAGE_INSTANCE_HASH_DEPTH);

	unbind_to(count, Qnil);
}

/* Determine whether an image_instance has changed structurally and
   hence needs redisplaying in some way.

   #### This should just look at the instantiator differences when we
   get rid of the stored items altogether. In fact we should probably
   store the new instantiator as well as the old - as we do with
   gui_items currently - and then pick-up the new on the next
   redisplay. This would obviate the need for any of this trickery
   with hashcodes. */
int image_instance_changed(Lisp_Object subwindow)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(subwindow);

	if (internal_hash(subwindow, IMAGE_INSTANCE_HASH_DEPTH) !=
	    IMAGE_INSTANCE_DISPLAY_HASH(ii))
		return 1;
	/* #### I think there is probably a bug here. This gets called for
	   layouts - and yet the pending items are always nil for
	   layouts. We are saved by layout optimization, but I'm undecided
	   as to what the correct fix is. */
	else if (WIDGET_IMAGE_INSTANCEP(subwindow)
		 && (!internal_equal(IMAGE_INSTANCE_WIDGET_ITEMS(ii),
				     IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii), 0)
		     || !NILP(IMAGE_INSTANCE_LAYOUT_CHILDREN(ii))
		     || IMAGE_INSTANCE_WIDGET_ACTION_OCCURRED(ii)))
		return 1;
	else
		return 0;
}

/* Update all the subwindows on a frame. */
void update_widget_instances(Lisp_Object frame)
{
	struct frame *f;
	Lisp_Object rest;

	/* Its possible for the preceding callback to have deleted the
	   frame, so cope with this. */
	if (!FRAMEP(frame) || !FRAME_LIVE_P(XFRAME(frame)))
		return;

	CHECK_FRAME(frame);
	f = XFRAME(frame);

	/* If we get called we know something has changed. */
	LIST_LOOP(rest, XWEAK_LIST_LIST(FRAME_SUBWINDOW_CACHE(f))) {
		Lisp_Object widget = XCAR(rest);

		if (XIMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(widget)
		    && image_instance_changed(widget)) {
			set_image_instance_dirty_p(widget, 1);
			MARK_FRAME_GLYPHS_CHANGED(f);
		}
	}
}

/* remove a subwindow from its frame */
void unmap_subwindow(Lisp_Object subwindow)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(subwindow);
	struct frame *f;

	ERROR_CHECK_IMAGE_INSTANCE(subwindow);

	if (!(image_instance_type_to_mask(IMAGE_INSTANCE_TYPE(ii))
	      & (IMAGE_WIDGET_MASK | IMAGE_SUBWINDOW_MASK))
	    || !IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(ii))
		return;

#ifdef DEBUG_WIDGETS
	stderr_out("unmapping subwindow %p\n", IMAGE_INSTANCE_SUBWINDOW_ID(ii));
#endif
	f = XFRAME(IMAGE_INSTANCE_FRAME(ii));

	/* make sure we don't get expose events */
	register_ignored_expose(f, IMAGE_INSTANCE_DISPLAY_X(ii),
				IMAGE_INSTANCE_DISPLAY_Y(ii),
				IMAGE_INSTANCE_DISPLAY_WIDTH(ii),
				IMAGE_INSTANCE_DISPLAY_HEIGHT(ii));
	IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(ii) = 0;

	MAYBE_DEVMETH(XDEVICE(IMAGE_INSTANCE_DEVICE(ii)),
		      unmap_subwindow, (ii));
}

/* show a subwindow in its frame */
void map_subwindow(Lisp_Object subwindow, int x, int y,
		   struct display_glyph_area *dga)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(subwindow);

	ERROR_CHECK_IMAGE_INSTANCE(subwindow);

	if (!(image_instance_type_to_mask(IMAGE_INSTANCE_TYPE(ii))
	      & (IMAGE_WIDGET_MASK | IMAGE_SUBWINDOW_MASK)))
		return;

#ifdef DEBUG_WIDGETS
	stderr_out("mapping subwindow %p, %dx%d@%d+%d\n",
		   IMAGE_INSTANCE_SUBWINDOW_ID(ii),
		   dga->width, dga->height, x, y);
#endif
	(void)XFRAME(IMAGE_INSTANCE_FRAME(ii));
	IMAGE_INSTANCE_DISPLAY_X(ii) = x;
	IMAGE_INSTANCE_DISPLAY_Y(ii) = y;
	IMAGE_INSTANCE_DISPLAY_WIDTH(ii) = dga->width;
	IMAGE_INSTANCE_DISPLAY_HEIGHT(ii) = dga->height;

	MAYBE_DEVMETH(DOMAIN_XDEVICE(ii->domain),
		      map_subwindow, (ii, x, y, dga));
	IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(ii) = 1;
}

static int subwindow_possible_dest_types(void)
{
	return IMAGE_SUBWINDOW_MASK;
}

int subwindow_governing_domain(void)
{
	return GOVERNING_DOMAIN_WINDOW;
}

/* Partially instantiate a subwindow. */
void
subwindow_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
		      Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		      int dest_mask, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object device = image_instance_device(image_instance);
	Lisp_Object frame = DOMAIN_FRAME(domain);
	Lisp_Object width = find_keyword_in_vector(instantiator, Q_pixel_width);
	Lisp_Object height =
	    find_keyword_in_vector(instantiator, Q_pixel_height);

	if (NILP(frame))
		signal_simple_error("No selected frame", device);

	if (!(dest_mask & IMAGE_SUBWINDOW_MASK))
		incompatible_image_types(instantiator, dest_mask,
					 IMAGE_SUBWINDOW_MASK);

	ii->data = 0;
	IMAGE_INSTANCE_SUBWINDOW_ID(ii) = 0;
	IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(ii) = 0;

	if (INTP(width)) {
		int w = 1;
		if (XINT(width) > 1)
			w = XINT(width);
		IMAGE_INSTANCE_WIDTH(ii) = w;
		IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii) = 0;
	}

	if (INTP(height)) {
		int h = 1;
		if (XINT(height) > 1)
			h = XINT(height);
		IMAGE_INSTANCE_HEIGHT(ii) = h;
		IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii) = 0;
	}
}

#ifdef HAVE_X_WINDOWS
extern void x_subwindow_query_geometry(Lisp_Object image_instance,
				       int *width, int *height);

static void
subwindow_query_geometry(Lisp_Object image_instance, int *width,
			 int *height, enum image_instance_geometry disp,
			 Lisp_Object domain)
{
	if (IMAGE_INSTANCE_INITIALIZED(XIMAGE_INSTANCE(image_instance)))
	{
		/* Query real size of subwindow */
		x_subwindow_query_geometry(image_instance, width, height);
	} else {
		/* Set them in case of initial layout instantiation */
		if (width)
			*width = 20;
		if (height)
			*height = 20;
	}
}
#else
/* This is just a backup in case no-one has assigned a suitable geometry.
   #### It should really query the enclose window for geometry. */
static void
subwindow_query_geometry(Lisp_Object image_instance, int *width,
			 int *height, enum image_instance_geometry disp,
			 Lisp_Object domain)
{
	if (width)
		*width = 20;
	if (height)
		*height = 20;
}
#endif  /* HAVE_X_WINDOWS */

DEFUN("subwindowp", Fsubwindowp, 1, 1, 0,	/*
Return non-nil if OBJECT is a subwindow.
*/
      (object))
{
	CHECK_IMAGE_INSTANCE(object);
	return (XIMAGE_INSTANCE_TYPE(object) == IMAGE_SUBWINDOW) ? Qt : Qnil;
}

DEFUN("image-instance-subwindow-id", Fimage_instance_subwindow_id, 1, 1, 0,	/*
Return the window id of SUBWINDOW as a number.
*/
      (subwindow))
{
	CHECK_SUBWINDOW_IMAGE_INSTANCE(subwindow);
	return make_int((EMACS_INT) XIMAGE_INSTANCE_SUBWINDOW_ID(subwindow));
}

DEFUN("resize-subwindow", Fresize_subwindow, 1, 3, 0,	/*
Resize SUBWINDOW to WIDTH x HEIGHT.
If a value is nil that parameter is not changed.
*/
      (subwindow, width, height))
{
	int neww, newh;
	Lisp_Image_Instance *ii;

	CHECK_SUBWINDOW_IMAGE_INSTANCE(subwindow);
	ii = XIMAGE_INSTANCE(subwindow);

	if (NILP(width))
		neww = IMAGE_INSTANCE_WIDTH(ii);
	else
		neww = XINT(width);

	if (NILP(height))
		newh = IMAGE_INSTANCE_HEIGHT(ii);
	else
		newh = XINT(height);

	/* The actual resizing gets done asynchronously by
	   update_subwindow. */
	IMAGE_INSTANCE_HEIGHT(ii) = newh;
	IMAGE_INSTANCE_WIDTH(ii) = neww;
	IMAGE_INSTANCE_SIZE_CHANGED(ii) = 1;

	return subwindow;
}

DEFUN("force-subwindow-map", Fforce_subwindow_map, 1, 1, 0,	/*
Generate a Map event for SUBWINDOW.
*/
      (subwindow))
{
	CHECK_SUBWINDOW_IMAGE_INSTANCE(subwindow);
#if 0
	map_subwindow(subwindow, 0, 0);
#endif
	return subwindow;
}

/*****************************************************************************
 *                              display tables                               *
 *****************************************************************************/

/* Get the display tables for use currently on window W with face
   FACE.  #### This will have to be redone.  */

void
get_display_tables(struct window *w, face_index findex,
		   Lisp_Object * face_table, Lisp_Object * window_table)
{
	Lisp_Object tem;
	tem = WINDOW_FACE_CACHEL_DISPLAY_TABLE(w, findex);
	if (UNBOUNDP(tem))
		tem = Qnil;
	if (!LISTP(tem))
		tem = noseeum_cons(tem, Qnil);
	*face_table = tem;
	tem = w->display_table;
	if (UNBOUNDP(tem))
		tem = Qnil;
	if (!LISTP(tem))
		tem = noseeum_cons(tem, Qnil);
	*window_table = tem;
}

Lisp_Object
display_table_entry(Emchar ch, Lisp_Object face_table, Lisp_Object window_table)
{
	Lisp_Object tail;

	/* Loop over FACE_TABLE, and then over WINDOW_TABLE. */
	for (tail = face_table; 1; tail = XCDR(tail)) {
		Lisp_Object table;
		if (NILP(tail)) {
			if (!NILP(window_table)) {
				tail = window_table;
				window_table = Qnil;
			} else
				return Qnil;
		}
		table = XCAR(tail);

		if (VECTORP(table)) {
			if (ch < XVECTOR_LENGTH(table)
			    && !NILP(XVECTOR_DATA(table)[ch]))
				return XVECTOR_DATA(table)[ch];
			else
				continue;
		} else if (CHAR_TABLEP(table)
			   && XCHAR_TABLE_TYPE(table) == CHAR_TABLE_TYPE_CHAR) {
			return get_char_table(ch, XCHAR_TABLE(table));
		} else if (CHAR_TABLEP(table)
			   && XCHAR_TABLE_TYPE(table) ==
			   CHAR_TABLE_TYPE_GENERIC) {
			Lisp_Object gotit =
			    get_char_table(ch, XCHAR_TABLE(table));
			if (!NILP(gotit))
				return gotit;
			else
				continue;
		} else if (RANGE_TABLEP(table)) {
			Lisp_Object gotit =
			    Fget_range_table(make_char(ch), table, Qnil);
			if (!NILP(gotit))
				return gotit;
			else
				continue;
		} else
			abort();
	}
}

/*****************************************************************************
 *                              timeouts for animated glyphs                      *
 *****************************************************************************/
static Lisp_Object Qglyph_animated_timeout_handler;

DEFUN("glyph-animated-timeout-handler", Fglyph_animated_timeout_handler, 1, 1, 0,	/*
Callback function for updating animated images.
Don't use this.
*/
      (arg))
{
	CHECK_WEAK_LIST(arg);

	if (!NILP(XWEAK_LIST_LIST(arg)) && !NILP(XCAR(XWEAK_LIST_LIST(arg)))) {
		Lisp_Object value = XCAR(XWEAK_LIST_LIST(arg));

		if (IMAGE_INSTANCEP(value)) {
			Lisp_Image_Instance *ii = XIMAGE_INSTANCE(value);

			if (COLOR_PIXMAP_IMAGE_INSTANCEP(value)
			    &&
			    IMAGE_INSTANCE_PIXMAP_MAXSLICE(ii) > 1
			    && !disable_animated_pixmaps) {
				Lisp_Object tmp;
				/* Increment the index of the image slice we are
				   currently viewing. */
				IMAGE_INSTANCE_PIXMAP_SLICE(ii) =
				    (IMAGE_INSTANCE_PIXMAP_SLICE(ii) + 1)
				    % IMAGE_INSTANCE_PIXMAP_MAXSLICE(ii);
				/* We might need to kick redisplay at this point
				   - but we also might not. */
				tmp = image_instance_device(value);
				MARK_DEVICE_FRAMES_GLYPHS_CHANGED(XDEVICE(tmp));
				/* Cascade dirtiness so that we can have an
				   animated glyph in a layout for instance. */
				set_image_instance_dirty_p(value, 1);
			}
		}
	}
	return Qnil;
}

Lisp_Object add_glyph_animated_timeout(EMACS_INT tickms, Lisp_Object image)
{
	Lisp_Object ret = Qnil;

	if (tickms > 0 && IMAGE_INSTANCEP(image)) {
		double ms = ((double)tickms) / 1000.0;
		struct gcpro gcpro1;
		Lisp_Object holder = make_weak_list(WEAK_LIST_SIMPLE);

		GCPRO1(holder);
		XWEAK_LIST_LIST(holder) = Fcons(image, Qnil);

		ret = Fadd_timeout(make_float(ms),
				   Qglyph_animated_timeout_handler,
				   holder, make_float(ms));

		UNGCPRO;
	}
	return ret;
}

void disable_glyph_animated_timeout(int i)
{
	Lisp_Object id;
	XSETINT(id, i);

	Fdisable_timeout(id);
}

/*****************************************************************************
 *                              initialization                               *
 *****************************************************************************/

void syms_of_glyphs(void)
{
	INIT_LRECORD_IMPLEMENTATION(glyph);
	INIT_LRECORD_IMPLEMENTATION(image_instance);

	/* image instantiators */

	DEFSUBR(Fimage_instantiator_format_list);
	DEFSUBR(Fvalid_image_instantiator_format_p);
	DEFSUBR(Fset_console_type_image_conversion_list);
	DEFSUBR(Fconsole_type_image_conversion_list);

	DEFKEYWORD(Q_face);
	DEFKEYWORD(Q_pixel_height);
	DEFKEYWORD(Q_pixel_width);

#ifdef HAVE_XPM
	DEFKEYWORD(Q_color_symbols);
#endif

	DEFKEYWORD(Q_mask_file);
	DEFKEYWORD(Q_mask_data);
	DEFKEYWORD(Q_hotspot_x);
	DEFKEYWORD(Q_hotspot_y);
	DEFKEYWORD(Q_foreground);
	DEFKEYWORD(Q_background);

	/* image specifiers */

	DEFSUBR(Fimage_specifier_p);
	/* Qimage in general.c */

	/* image instances */

	defsymbol(&Qimage_instancep, "image-instance-p");

	DEFSYMBOL(Qnothing_image_instance_p);
	DEFSYMBOL(Qtext_image_instance_p);
	DEFSYMBOL(Qmono_pixmap_image_instance_p);
	DEFSYMBOL(Qcolor_pixmap_image_instance_p);
	DEFSYMBOL(Qpointer_image_instance_p);
	DEFSYMBOL(Qwidget_image_instance_p);
	DEFSYMBOL(Qsubwindow_image_instance_p);

	DEFSUBR(Fmake_image_instance);
	DEFSUBR(Fimage_instance_p);
	DEFSUBR(Fimage_instance_type);
	DEFSUBR(Fvalid_image_instance_type_p);
	DEFSUBR(Fimage_instance_type_list);
	DEFSUBR(Fimage_instance_name);
	DEFSUBR(Fimage_instance_domain);
	DEFSUBR(Fimage_instance_string);
	DEFSUBR(Fimage_instance_file_name);
	DEFSUBR(Fimage_instance_mask_file_name);
	DEFSUBR(Fimage_instance_depth);
	DEFSUBR(Fimage_instance_height);
	DEFSUBR(Fimage_instance_width);
	DEFSUBR(Fimage_instance_hotspot_x);
	DEFSUBR(Fimage_instance_hotspot_y);
	DEFSUBR(Fimage_instance_foreground);
	DEFSUBR(Fimage_instance_background);
	DEFSUBR(Fimage_instance_property);
	DEFSUBR(Fcolorize_image_instance);
	/* subwindows */
	DEFSUBR(Fsubwindowp);
	DEFSUBR(Fimage_instance_subwindow_id);
	DEFSUBR(Fresize_subwindow);
	DEFSUBR(Fforce_subwindow_map);

	/* Qnothing defined as part of the "nothing" image-instantiator
	   type. */
	/* Qtext defined in general.c */
	DEFSYMBOL(Qmono_pixmap);
	DEFSYMBOL(Qcolor_pixmap);
	/* Qpointer defined in general.c */

	/* glyphs */

	DEFSYMBOL(Qglyphp);
	DEFSYMBOL(Qcontrib_p);
	DEFSYMBOL(Qbaseline);

	DEFSYMBOL(Qbuffer_glyph_p);
	DEFSYMBOL(Qpointer_glyph_p);
	DEFSYMBOL(Qicon_glyph_p);

	DEFSYMBOL(Qconst_glyph_variable);

	DEFSUBR(Fglyph_type);
	DEFSUBR(Fvalid_glyph_type_p);
	DEFSUBR(Fglyph_type_list);
	DEFSUBR(Fglyphp);
	DEFSUBR(Fmake_glyph_internal);
	DEFSUBR(Fglyph_width);
	DEFSUBR(Fglyph_ascent);
	DEFSUBR(Fglyph_descent);
	DEFSUBR(Fglyph_height);
	DEFSUBR(Fset_instantiator_property);

	/* Qbuffer defined in general.c. */
	/* Qpointer defined above */

	/* Unfortunately, timeout handlers must be lisp functions. This is
	   for animated glyphs. */
	DEFSYMBOL(Qglyph_animated_timeout_handler);
	DEFSUBR(Fglyph_animated_timeout_handler);

	/* Errors */
	DEFERROR_STANDARD(Qimage_conversion_error, Qio_error);
}

static const struct lrecord_description image_specifier_description[] = {
	{XD_LISP_OBJECT,
	 specifier_data_offset + offsetof(struct image_specifier, attachee)},
	{XD_LISP_OBJECT,
	 specifier_data_offset + offsetof(struct image_specifier,
					  attachee_property)},
	{XD_END}
};

void specifier_type_create_image(void)
{
	/* image specifiers */

	INITIALIZE_SPECIFIER_TYPE_WITH_DATA(image, "image", "imagep");

	SPECIFIER_HAS_METHOD(image, create);
	SPECIFIER_HAS_METHOD(image, mark);
	SPECIFIER_HAS_METHOD(image, instantiate);
	SPECIFIER_HAS_METHOD(image, validate);
	SPECIFIER_HAS_METHOD(image, after_change);
	SPECIFIER_HAS_METHOD(image, going_to_add);
	SPECIFIER_HAS_METHOD(image, copy_instantiator);
}

void reinit_specifier_type_create_image(void)
{
	REINITIALIZE_SPECIFIER_TYPE(image);
}

static const struct lrecord_description iike_description_1[] = {
	{XD_LISP_OBJECT, offsetof(ii_keyword_entry, keyword)},
	{XD_END}
};

static const struct struct_description iike_description = {
	sizeof(ii_keyword_entry),
	iike_description_1
};

static const struct lrecord_description iiked_description_1[] = {
	XD_DYNARR_DESC(ii_keyword_entry_dynarr, &iike_description),
	{XD_END}
};

static const struct struct_description iiked_description = {
	sizeof(ii_keyword_entry_dynarr),
	iiked_description_1
};

static const struct lrecord_description iife_description_1[] = {
	{XD_LISP_OBJECT, offsetof(image_instantiator_format_entry, symbol)},
	{XD_LISP_OBJECT, offsetof(image_instantiator_format_entry, device)},
	{XD_STRUCT_PTR, offsetof(image_instantiator_format_entry, meths), 1,
	 &iim_description},
	{XD_END}
};

static const struct struct_description iife_description = {
	sizeof(image_instantiator_format_entry),
	iife_description_1
};

static const struct lrecord_description iifed_description_1[] = {
	XD_DYNARR_DESC(image_instantiator_format_entry_dynarr,
		       &iife_description),
	{XD_END}
};

static const struct struct_description iifed_description = {
	sizeof(image_instantiator_format_entry_dynarr),
	iifed_description_1
};

static const struct lrecord_description iim_description_1[] = {
	{XD_LISP_OBJECT, offsetof(struct image_instantiator_methods, symbol)},
	{XD_LISP_OBJECT, offsetof(struct image_instantiator_methods, device)},
	{XD_STRUCT_PTR, offsetof(struct image_instantiator_methods, keywords),
	 1, &iiked_description},
	{XD_STRUCT_PTR, offsetof(struct image_instantiator_methods, consoles),
	 1, &cted_description},
	{XD_END}
};

const struct struct_description iim_description = {
	sizeof(struct image_instantiator_methods),
	iim_description_1
};

void image_instantiator_format_create(void)
{
	/* image instantiators */

	the_image_instantiator_format_entry_dynarr =
	    Dynarr_new(image_instantiator_format_entry);

	Vimage_instantiator_format_list = Qnil;
	staticpro(&Vimage_instantiator_format_list);

	dump_add_root_struct_ptr(&the_image_instantiator_format_entry_dynarr,
				 &iifed_description);

	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(nothing, "nothing");

	IIFORMAT_HAS_METHOD(nothing, possible_dest_types);
	IIFORMAT_HAS_METHOD(nothing, instantiate);

	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(inherit, "inherit");

	IIFORMAT_HAS_METHOD(inherit, validate);
	IIFORMAT_HAS_METHOD(inherit, normalize);
	IIFORMAT_HAS_METHOD(inherit, possible_dest_types);
	IIFORMAT_HAS_METHOD(inherit, instantiate);

	IIFORMAT_VALID_KEYWORD(inherit, Q_face, check_valid_face);

	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(string, "string");

	IIFORMAT_HAS_METHOD(string, validate);
	IIFORMAT_HAS_SHARED_METHOD(string, governing_domain, subwindow);
	IIFORMAT_HAS_METHOD(string, possible_dest_types);
	IIFORMAT_HAS_METHOD(string, instantiate);

	IIFORMAT_VALID_KEYWORD(string, Q_data, check_valid_string);
	/* Do this so we can set strings. */
	/* #### Andy, what is this?  This is a bogus format and should not be
	   visible to the user. */
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(text, "text");
	IIFORMAT_HAS_METHOD(text, update);
	IIFORMAT_HAS_METHOD(text, query_geometry);

	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(formatted_string,
					     "formatted-string");

	IIFORMAT_HAS_METHOD(formatted_string, validate);
	IIFORMAT_HAS_METHOD(formatted_string, possible_dest_types);
	IIFORMAT_HAS_METHOD(formatted_string, instantiate);
	IIFORMAT_VALID_KEYWORD(formatted_string, Q_data, check_valid_string);

	/* Do this so pointers have geometry. */
	/* #### Andy, what is this?  This is a bogus format and should not be
	   visible to the user. */
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(pointer, "pointer");
	IIFORMAT_HAS_SHARED_METHOD(pointer, query_geometry, subwindow);

	/* subwindows */
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(subwindow, "subwindow");
	IIFORMAT_HAS_METHOD(subwindow, possible_dest_types);
	IIFORMAT_HAS_METHOD(subwindow, governing_domain);
	IIFORMAT_HAS_METHOD(subwindow, instantiate);
	IIFORMAT_HAS_METHOD(subwindow, query_geometry);
	IIFORMAT_VALID_KEYWORD(subwindow, Q_pixel_width, check_valid_int);
	IIFORMAT_VALID_KEYWORD(subwindow, Q_pixel_height, check_valid_int);

#ifdef HAVE_WINDOW_SYSTEM
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(xbm, "xbm");

	IIFORMAT_HAS_METHOD(xbm, validate);
	IIFORMAT_HAS_METHOD(xbm, normalize);
	IIFORMAT_HAS_METHOD(xbm, possible_dest_types);

	IIFORMAT_VALID_KEYWORD(xbm, Q_data, check_valid_xbm_inline);
	IIFORMAT_VALID_KEYWORD(xbm, Q_file, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xbm, Q_mask_data, check_valid_xbm_inline);
	IIFORMAT_VALID_KEYWORD(xbm, Q_mask_file, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xbm, Q_hotspot_x, check_valid_int);
	IIFORMAT_VALID_KEYWORD(xbm, Q_hotspot_y, check_valid_int);
	IIFORMAT_VALID_KEYWORD(xbm, Q_foreground, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xbm, Q_background, check_valid_string);
#endif				/* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_XFACE
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(xface, "xface");

	IIFORMAT_HAS_METHOD(xface, validate);
	IIFORMAT_HAS_METHOD(xface, normalize);
	IIFORMAT_HAS_METHOD(xface, possible_dest_types);

	IIFORMAT_VALID_KEYWORD(xface, Q_data, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xface, Q_file, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xface, Q_hotspot_x, check_valid_int);
	IIFORMAT_VALID_KEYWORD(xface, Q_hotspot_y, check_valid_int);
	IIFORMAT_VALID_KEYWORD(xface, Q_foreground, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xface, Q_background, check_valid_string);
#endif

#ifdef HAVE_XPM
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(xpm, "xpm");

	IIFORMAT_HAS_METHOD(xpm, validate);
	IIFORMAT_HAS_METHOD(xpm, normalize);
	IIFORMAT_HAS_METHOD(xpm, possible_dest_types);

	IIFORMAT_VALID_KEYWORD(xpm, Q_data, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xpm, Q_file, check_valid_string);
	IIFORMAT_VALID_KEYWORD(xpm, Q_color_symbols,
			       check_valid_xpm_color_symbols);
#endif				/* HAVE_XPM */
}

void reinit_vars_of_glyphs(void)
{
	the_expose_ignore_blocktype =
	    Blocktype_new(struct expose_ignore_blocktype);

	hold_ignored_expose_registration = 0;
}

void vars_of_glyphs(void)
{
	reinit_vars_of_glyphs();

	Vthe_nothing_vector = vector1(Qnothing);
	staticpro(&Vthe_nothing_vector);

	/* image instances */

	Vimage_instance_type_list = Fcons(Qnothing,
					  list6(Qtext, Qmono_pixmap,
						Qcolor_pixmap, Qpointer,
						Qsubwindow, Qwidget));
	staticpro(&Vimage_instance_type_list);

	/* glyphs */

	Vglyph_type_list = list3(Qbuffer, Qpointer, Qicon);
	staticpro(&Vglyph_type_list);

	/* The octal-escape glyph, control-arrow-glyph and
	   invisible-text-glyph are completely initialized in glyphs.el */

	DEFVAR_LISP("octal-escape-glyph", &Voctal_escape_glyph	/*
What to prefix character codes displayed in octal with.
								 */ );
	Voctal_escape_glyph =
	    allocate_glyph(GLYPH_BUFFER, redisplay_glyph_changed);

	DEFVAR_LISP("control-arrow-glyph", &Vcontrol_arrow_glyph	/*
What to use as an arrow for control characters.
									 */ );
	Vcontrol_arrow_glyph = allocate_glyph(GLYPH_BUFFER,
					      redisplay_glyph_changed);

	DEFVAR_LISP("invisible-text-glyph", &Vinvisible_text_glyph	/*
What to use to indicate the presence of invisible text.
This is the glyph that is displayed when an ellipsis is called for
\(see `selective-display-ellipses' and `buffer-invisibility-spec').
Normally this is three dots ("...").
									 */ );
	Vinvisible_text_glyph = allocate_glyph(GLYPH_BUFFER,
					       redisplay_glyph_changed);

	/* Partially initialized in glyphs.el */
	DEFVAR_LISP("hscroll-glyph", &Vhscroll_glyph	/*
What to display at the beginning of horizontally scrolled lines.
							 */ );
	Vhscroll_glyph = allocate_glyph(GLYPH_BUFFER, redisplay_glyph_changed);
#ifdef HAVE_WINDOW_SYSTEM
	Fprovide(Qxbm);
#endif
#ifdef HAVE_XPM
	Fprovide(Qxpm);

	DEFVAR_LISP("xpm-color-symbols", &Vxpm_color_symbols	/*
Definitions of logical color-names used when reading XPM files.
Elements of this list should be of the form (COLOR-NAME FORM-TO-EVALUATE).
The COLOR-NAME should be a string, which is the name of the color to define;
the FORM should evaluate to a `color' specifier object, or a string to be
passed to `make-color-instance'.  If a loaded XPM file references a symbolic
color called COLOR-NAME, it will display as the computed color instead.

The default value of this variable defines the logical color names
\"foreground\" and \"background\" to be the colors of the `default' face.
								 */ );
	Vxpm_color_symbols = Qnil;	/* initialized in x-faces.el */
#endif				/* HAVE_XPM */
#ifdef HAVE_XFACE
	Fprovide(Qxface);
#endif

	DEFVAR_BOOL("disable-animated-pixmaps", &disable_animated_pixmaps	/*
Whether animated pixmaps should be animated.
Default is t.
										 */ );
	disable_animated_pixmaps = 0;
}

void specifier_vars_of_glyphs(void)
{
	/* #### Can we GC here? The set_specifier_* calls definitely need */
	/* protection. */
	/* display tables */

	DEFVAR_SPECIFIER("current-display-table", &Vcurrent_display_table	/*
*The display table currently in use.
This is a specifier; use `set-specifier' to change it.

Display tables are used to control how characters are displayed.  Each
time that redisplay processes a character, it is looked up in all the
display tables that apply (obtained by calling `specifier-instance' on
`current-display-table' and any overriding display tables specified in
currently active faces).  The first entry found that matches the
character determines how the character is displayed.  If there is no
matching entry, the default display method is used. (Non-control
characters are displayed as themselves and control characters are
displayed according to the buffer-local variable `ctl-arrow'.  Control
characters are further affected by `control-arrow-glyph' and
`octal-escape-glyph'.)

Each instantiator in this specifier and the display-table specifiers
in faces is a display table or a list of such tables.  If a list, each
table will be searched in turn for an entry matching a particular
character.  Each display table is one of

-- a vector, specifying values for characters starting at 0
-- a char table, either of type `char' or `generic'
-- a range table

Each entry in a display table should be one of

-- nil (this entry is ignored and the search continues)
-- a character (use this character; if it happens to be the same as
the original character, default processing happens, otherwise
redisplay attempts to display this character directly;
#### At some point recursive display-table lookup will be
implemented.)
-- a string (display each character in the string directly;
#### At some point recursive display-table lookup will be
implemented.)
-- a glyph (display the glyph;
#### At some point recursive display-table lookup will be
implemented when a string glyph is being processed.)
-- a cons of the form (format "STRING") where STRING is a printf-like
spec used to process the character. #### Unfortunately no
formatting directives other than %% are implemented.
-- a vector (each element of the vector is processed recursively;
in such a case, nil elements in the vector are simply ignored)

#### At some point in the near future, display tables are likely to
be expanded to include other features, such as referencing characters
in particular fonts and allowing the character search to continue
all the way up the chain of specifier instantiators.  These features
are necessary to properly display Unicode characters.
										 */ );
	Vcurrent_display_table = Fmake_specifier(Qdisplay_table);
	set_specifier_fallback(Vcurrent_display_table,
			       list1(Fcons(Qnil, Qnil)));
	set_specifier_caching(Vcurrent_display_table,
			      offsetof(struct window, display_table),
			      some_window_value_changed, 0, 0, 0);
}

void complex_vars_of_glyphs(void)
{
	/* Partially initialized in glyphs-x.c, glyphs.el */
	DEFVAR_LISP("truncation-glyph", &Vtruncation_glyph	/*
What to display at the end of truncated lines.
								 */ );
	Vtruncation_glyph =
	    allocate_glyph(GLYPH_BUFFER, redisplay_glyph_changed);

	/* Partially initialized in glyphs-x.c, glyphs.el */
	DEFVAR_LISP("continuation-glyph", &Vcontinuation_glyph	/*
What to display at the end of wrapped lines.
								 */ );
	Vcontinuation_glyph =
	    allocate_glyph(GLYPH_BUFFER, redisplay_glyph_changed);

	/* Partially initialized in glyphs-x.c, glyphs.el */
	DEFVAR_LISP("sxemacs-logo", &Vsxemacs_logo	/*
The glyph used to display the SXEmacs logo at startup.
							 */ );
	Vsxemacs_logo = allocate_glyph(GLYPH_BUFFER, 0);
}
