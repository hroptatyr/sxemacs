/* Generic glyph data structures + display tables
   Copyright (C) 1994 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing

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

#ifndef INCLUDED_glyphs_h_
#define INCLUDED_glyphs_h_

#include "specifier.h"
#include "gui.h"

/************************************************************************/
/*			Image Instantiators				*/
/************************************************************************/

struct image_instantiator_methods;

/* Remember the distinction between image instantiator formats and
   image instance types.  Here's an approximate mapping:

  image instantiator format	image instance type
  -------------------------	-------------------
  nothing			nothing
  string			text
  formatted-string		text
  xbm				mono-pixmap, color-pixmap, pointer
  xpm				color-pixmap, mono-pixmap, pointer
  xface				mono-pixmap, color-pixmap, pointer
  gif				color-pixmap
  jpeg				color-pixmap
  png				color-pixmap
  tiff				color-pixmap
  bmp				color-pixmap
  cursor-font			pointer
  mswindows-resource		pointer, color-pixmap
  font				pointer
  subwindow			subwindow
  inherit			mono-pixmap
  autodetect			mono-pixmap, color-pixmap, pointer, text
  button			widget
  edit-field			widget
  combo-box			widget
  progress-gauge		widget
  tab-control			widget
  tree-view			widget
  scrollbar			widget
  label				widget
  layout			widget
  native-layout			widget
*/

/* These are methods specific to a particular format of image instantiator
   (e.g. xpm, string, etc.). */

typedef struct ii_keyword_entry ii_keyword_entry;
struct ii_keyword_entry {
	Lisp_Object keyword;
	void (*validate) (Lisp_Object data);
	int multiple_p;
	int copy_p;
};

typedef struct {
	Dynarr_declare(ii_keyword_entry);
} ii_keyword_entry_dynarr;

extern const struct struct_description iim_description;

enum image_instance_geometry {
	IMAGE_GEOMETRY,
	IMAGE_DESIRED_GEOMETRY,
	IMAGE_MIN_GEOMETRY,
	IMAGE_MAX_GEOMETRY
};

#define IMAGE_UNSPECIFIED_GEOMETRY -1
#define IMAGE_UNCHANGED_GEOMETRY -2

#define DEFAULT_WIDGET_BORDER_WIDTH 2
#define DEFAULT_WIDGET_SPACING 3
#define DEFAULT_WIDGET_SHADOW_WIDTH 2

enum governing_domain {
	GOVERNING_DOMAIN_WINDOW,
	GOVERNING_DOMAIN_FRAME,
	GOVERNING_DOMAIN_DEVICE
};

struct image_instantiator_methods {
	Lisp_Object symbol;

	Lisp_Object device;	/* sometimes used */

	ii_keyword_entry_dynarr *keywords;
	/* consoles this ii is supported on */
	console_type_entry_dynarr *consoles;
	/* Implementation specific methods: */

	/* Validate method: Given an instantiator vector, signal an error if
	   it's invalid for this image-instantiator format.  Note that this
	   validation only occurs after all the keyword-specific validation
	   has already been performed.  This is chiefly useful for making
	   sure that certain required keywords are present. */
	void (*validate_method) (Lisp_Object instantiator);

	/* Normalize method: Given an instantiator, convert it to the form
	   that should be used in a glyph, for devices of type CONSOLE_TYPE.
	   Signal an error if conversion fails. */
	 Lisp_Object(*normalize_method) (Lisp_Object instantiator,
					 Lisp_Object console_type,
					 Lisp_Object dest_mask);

	/* Governing domain method: Return an int indicating what type of
	   domain an instance in this format is governed by. */
	int (*governing_domain_method) (void);

	/* Possible-dest-types method: Return a mask indicating what dest types
	   are compatible with this format. */
	int (*possible_dest_types_method) (void);

	/* Instantiate method: Given an instantiator and a partially
	   filled-in image instance, complete the filling-in.  Return
	   non-zero if the instantiation succeeds, 0 if it fails.
	   This must be present. */
	void (*instantiate_method) (Lisp_Object image_instance,
				    Lisp_Object instantiator,
				    Lisp_Object pointer_fg,
				    Lisp_Object pointer_bg,
				    int dest_mask, Lisp_Object domain);
	/* Post instantiate method: finish instantiation of the image
	   instance. */
	void (*post_instantiate_method) (Lisp_Object image_instance,
					 Lisp_Object instantiator,
					 Lisp_Object domain);
	/* Property method: Given an image instance, return device specific
	   properties. */
	 Lisp_Object(*property_method) (Lisp_Object image_instance,
					Lisp_Object property);
	/* Set-property method: Given an image instance, set device specific
	   properties. */
	 Lisp_Object(*set_property_method) (Lisp_Object image_instance,
					    Lisp_Object property,
					    Lisp_Object val);
	/* Asynchronously update properties. */
	void (*update_method) (Lisp_Object image_instance,
			       Lisp_Object instantiator);
	void (*redisplay_method) (Lisp_Object image_instance);

	/* Find out the desired geometry, as given by disp, of this image
	   instance. Actual geometry is stored in the appropriate slots in the
	   image instance. */
	void (*query_geometry_method) (Lisp_Object image_instance,
				       int *width, int *height,
				       enum image_instance_geometry disp,
				       Lisp_Object domain);

	/* Layout the instance and its children bounded by the provided
	   dimensions. Returns success or failure. */
	int (*layout_method) (Lisp_Object image_instance,
			      int width, int height, int xoffset, int yoffset,
			      Lisp_Object domain);
};

/***** Calling an image-instantiator method *****/

#define HAS_IIFORMAT_METH_P(mstruc, m) (((mstruc)->m##_method) != 0)
#define IIFORMAT_METH(mstruc, m, args) (((mstruc)->m##_method) args)

/* Call a void-returning specifier method, if it exists */
#define MAYBE_IIFORMAT_METH(mstruc, m, args)			\
do {								\
  struct image_instantiator_methods *MIM_mstruc = (mstruc);	\
  if (MIM_mstruc && HAS_IIFORMAT_METH_P (MIM_mstruc, m))	\
    IIFORMAT_METH (MIM_mstruc, m, args);			\
} while (0)

#define MAYBE_IIFORMAT_DEVMETH(device, mstruc, m, args)	\
do {							\
  struct image_instantiator_methods *MID_mstruc =	\
    decode_ii_device (device, mstruc);			\
  if (MID_mstruc)					\
    MAYBE_IIFORMAT_METH(MID_mstruc, m, args);		\
} while (0)

/* Call a specifier method, if it exists; otherwise return
   the specified value */

#define IIFORMAT_METH_OR_GIVEN(mstruc, m, args, given)	\
  ((mstruc && HAS_IIFORMAT_METH_P (mstruc, m)) ?		\
   IIFORMAT_METH (mstruc, m, args) : (given))

/***** Defining new image-instantiator types *****/

#define DECLARE_IMAGE_INSTANTIATOR_FORMAT(format)		\
extern struct image_instantiator_methods *format##_image_instantiator_methods

#define DEFINE_IMAGE_INSTANTIATOR_FORMAT(format)		\
struct image_instantiator_methods *format##_image_instantiator_methods

#define INITIALIZE_IMAGE_INSTANTIATOR_FORMAT_NO_SYM(format, obj_name)	\
do {									\
  format##_image_instantiator_methods =					\
    xnew_and_zero (struct image_instantiator_methods);			\
  format##_image_instantiator_methods->symbol = Q##format;		\
  format##_image_instantiator_methods->device = Qnil;			\
  format##_image_instantiator_methods->keywords =			\
    Dynarr_new (ii_keyword_entry);					\
  format##_image_instantiator_methods->consoles =			\
    Dynarr_new (console_type_entry);					\
  add_entry_to_image_instantiator_format_list				\
    (Q##format, format##_image_instantiator_methods);			\
  dump_add_root_struct_ptr (&format##_image_instantiator_methods,	\
			    &iim_description);				\
} while (0)

#define INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(format, obj_name)	\
do {								\
  defsymbol (&Q##format, obj_name);				\
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT_NO_SYM(format, obj_name);\
} while (0)

/* Declare that image-instantiator format FORMAT has method M; used in
   initialization routines */
#define IIFORMAT_HAS_METHOD(format, m) \
  (format##_image_instantiator_methods->m##_method = format##_##m)

#define IIFORMAT_HAS_SHARED_METHOD(format, m, type) \
  (format##_image_instantiator_methods->m##_method = type##_##m)

/* Declare that KEYW is a valid keyword for image-instantiator format
   FORMAT.  VALIDATE_FUN if a function that returns whether the data
   is valid.  The keyword may not appear more than once. */
#define IIFORMAT_VALID_GENERIC_KEYWORD(format, keyw, validate_fun, copy, multi) \
  do {								\
    struct ii_keyword_entry entry;				\
								\
    entry.keyword = keyw;					\
    entry.validate = validate_fun;				\
    entry.multiple_p = multi;					\
    entry.copy_p = copy;					\
    Dynarr_add (format##_image_instantiator_methods->keywords,	\
		entry);						\
  } while (0)

#define IIFORMAT_VALID_KEYWORD(format, keyw, validate_fun)	\
IIFORMAT_VALID_GENERIC_KEYWORD(format, keyw, validate_fun, 1, 0)

/* Same as IIFORMAT_VALID_KEYWORD except that the keyword may
   appear multiple times. */
#define IIFORMAT_VALID_MULTI_KEYWORD(format, keyw, validate_fun)	\
IIFORMAT_VALID_GENERIC_KEYWORD(format, keyw, validate_fun, 1, 1)

/* Same as IIFORMAT_VALID_KEYWORD except that the argument is not
   copied by the specifier functions. This is necessary for things
   like callbacks etc. */
#define IIFORMAT_VALID_NONCOPY_KEYWORD(format, keyw, validate_fun)	\
IIFORMAT_VALID_GENERIC_KEYWORD(format, keyw, validate_fun, 0, 0)

/* Declare that image-instantiator format FORMAT is supported on
   CONSOLE type. */
#define IIFORMAT_VALID_CONSOLE(console, format)			\
  do {								\
    struct console_type_entry entry;				\
								\
    entry.symbol = Q##console;					\
    entry.meths = console##_console_methods;			\
    Dynarr_add (format##_image_instantiator_methods->consoles,	\
		entry);						\
  } while (0)

#define IIFORMAT_VALID_CONSOLE2(con1, con2, format)		\
  IIFORMAT_VALID_CONSOLE (con1, format);			\
  IIFORMAT_VALID_CONSOLE (con2, format);

#define DEFINE_DEVICE_IIFORMAT(type, format)	\
DECLARE_IMAGE_INSTANTIATOR_FORMAT(format);	\
struct image_instantiator_methods *type##_##format##_image_instantiator_methods

#define INITIALIZE_DEVICE_IIFORMAT(type, format)				\
do {										\
  type##_##format##_image_instantiator_methods =				\
    xnew_and_zero (struct image_instantiator_methods);				\
  type##_##format##_image_instantiator_methods->symbol = Q##format;		\
  type##_##format##_image_instantiator_methods->device = Q##type;		\
  type##_##format##_image_instantiator_methods->keywords =			\
    Dynarr_new (ii_keyword_entry);						\
  add_entry_to_device_ii_format_list						\
    (Q##type, Q##format, type##_##format##_image_instantiator_methods);		\
  IIFORMAT_VALID_CONSOLE(type,format);						\
  dump_add_root_struct_ptr (&type##_##format##_image_instantiator_methods,	\
			    &iim_description);					\
} while (0)

/* Declare that image-instantiator format FORMAT has method M; used in
   initialization routines */
#define IIFORMAT_HAS_DEVMETHOD(type, format, m) \
  (type##_##format##_image_instantiator_methods->m##_method = type##_##format##_##m)
#define IIFORMAT_HAS_SHARED_DEVMETHOD(type, format, m, fromformat) \
  (type##_##format##_image_instantiator_methods->m##_method = type##_##fromformat##_##m)

#define IIFORMAT_INHERITS_DEVMETHOD(type, from, format, m) \
  (type##_##format##_image_instantiator_methods->m##_method = from##_##format##_##m)
#define IIFORMAT_INHERITS_SHARED_DEVMETHOD(type, from, format, m, fromformat) \
  (type##_##format##_image_instantiator_methods->m##_method = from##_##fromformat##_##m)

#define INSTANTIATOR_TYPE(inst) (XVECTOR_DATA ((inst))[0])

struct image_instantiator_methods *decode_device_ii_format(Lisp_Object device,
							   Lisp_Object format,
							   Error_behavior errb);
struct image_instantiator_methods *decode_image_instantiator_format(Lisp_Object
								    format,
								    Error_behavior
								    errb);

void add_entry_to_image_instantiator_format_list(Lisp_Object symbol,
						 struct
						 image_instantiator_methods
						 *meths);
void add_entry_to_device_ii_format_list(Lisp_Object device, Lisp_Object symbol,
					struct image_instantiator_methods
					*meths);
Lisp_Object find_keyword_in_vector(Lisp_Object vector, Lisp_Object keyword);
Lisp_Object find_keyword_in_vector_or_given(Lisp_Object vector,
					    Lisp_Object keyword,
					    Lisp_Object default_);
Lisp_Object simple_image_type_normalize(Lisp_Object inst,
					Lisp_Object console_type,
					Lisp_Object image_type_tag);
Lisp_Object potential_pixmap_file_instantiator(Lisp_Object instantiator,
					       Lisp_Object file_keyword,
					       Lisp_Object data_keyword,
					       Lisp_Object console_type);
void check_valid_string(Lisp_Object data);
void check_valid_int(Lisp_Object data);
void check_valid_face(Lisp_Object data);
void check_valid_vector(Lisp_Object data);
void check_valid_item_list(Lisp_Object items);

void initialize_subwindow_image_instance(Lisp_Image_Instance *);
void subwindow_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
			   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			   int dest_mask, Lisp_Object domain);
int subwindow_governing_domain(void);
void widget_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			int dest_mask, Lisp_Object domain);
void image_instance_query_geometry(Lisp_Object image_instance,
				   int *width, int *height,
				   enum image_instance_geometry disp,
				   Lisp_Object domain);
void image_instance_layout(Lisp_Object image_instance,
			   int width, int height, int xoffset, int yoffset,
			   Lisp_Object domain);
int layout_layout(Lisp_Object image_instance,
		  int width, int height, int xoffset, int yoffset,
		  Lisp_Object domain);
int invalidate_glyph_geometry_maybe(Lisp_Object glyph_or_ii, struct window *w);
Lisp_Object make_image_instance_cache_hash_table(void);

DECLARE_DOESNT_RETURN(incompatible_image_types(Lisp_Object instantiator,
					       int given_dest_mask,
					       int desired_dest_mask));
DECLARE_DOESNT_RETURN(signal_image_error(const char *, Lisp_Object));
DECLARE_DOESNT_RETURN(signal_image_error_2
		      (const char *, Lisp_Object, Lisp_Object));

/************************************************************************/
/*			Image Specifier Object				*/
/************************************************************************/

DECLARE_SPECIFIER_TYPE(image);
#define XIMAGE_SPECIFIER(x) XSPECIFIER_TYPE (x, image)
#define XSETIMAGE_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, image)
#define IMAGE_SPECIFIERP(x) SPECIFIER_TYPEP (x, image)
#define CHECK_IMAGE_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, image)
#define CONCHECK_IMAGE_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, image)

void set_image_attached_to(Lisp_Object obj, Lisp_Object face_or_glyph,
			   Lisp_Object property);

struct image_specifier {
	int allowed;
	Lisp_Object attachee;	/* face or glyph this is attached to, or nil */
	Lisp_Object attachee_property;	/* property of that face or glyph */
};

#define IMAGE_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, image)
#define IMAGE_SPECIFIER_ALLOWED(g) (IMAGE_SPECIFIER_DATA (g)->allowed)
#define IMAGE_SPECIFIER_ATTACHEE(g) (IMAGE_SPECIFIER_DATA (g)->attachee)
#define IMAGE_SPECIFIER_ATTACHEE_PROPERTY(g) \
  (IMAGE_SPECIFIER_DATA (g)->attachee_property)

#define XIMAGE_SPECIFIER_ALLOWED(g) \
  IMAGE_SPECIFIER_ALLOWED (XIMAGE_SPECIFIER (g))

/************************************************************************/
/*			Image Instance Object				*/
/************************************************************************/

DECLARE_LRECORD(image_instance, Lisp_Image_Instance);
#define XIMAGE_INSTANCE(x) XRECORD (x, image_instance, Lisp_Image_Instance)
#define XSETIMAGE_INSTANCE(x, p) XSETRECORD (x, p, image_instance)
#define IMAGE_INSTANCEP(x) RECORDP (x, image_instance)
#define CHECK_IMAGE_INSTANCE(x) CHECK_RECORD (x, image_instance)
#define CONCHECK_IMAGE_INSTANCE(x) CONCHECK_RECORD (x, image_instance)

#ifdef ERROR_CHECK_GLYPHS
void check_image_instance_structure(Lisp_Object instance);
void check_window_subwindow_cache(struct window *w);
#define ERROR_CHECK_IMAGE_INSTANCE(ii) \
  check_image_instance_structure (ii)
#define ERROR_CHECK_SUBWINDOW_CACHE(w) \
  check_window_subwindow_cache (w)
#else
#define ERROR_CHECK_IMAGE_INSTANCE(ii)
#define ERROR_CHECK_SUBWINDOW_CACHE(w)
#endif

enum image_instance_type {
	IMAGE_UNKNOWN,
	IMAGE_NOTHING,
	IMAGE_TEXT,
	IMAGE_MONO_PIXMAP,
	IMAGE_COLOR_PIXMAP,
	IMAGE_POINTER,
	IMAGE_SUBWINDOW,
	IMAGE_WIDGET
};

#define IMAGE_NOTHING_MASK (1 << 0)
#define IMAGE_TEXT_MASK (1 << 1)
#define IMAGE_MONO_PIXMAP_MASK (1 << 2)
#define IMAGE_COLOR_PIXMAP_MASK (1 << 3)
#define IMAGE_POINTER_MASK (1 << 4)
#define IMAGE_SUBWINDOW_MASK (1 << 5)
#define IMAGE_WIDGET_MASK (1 << 6)

/* This depends on the fact that enums are assigned consecutive
   integers starting at 0. (Remember that IMAGE_UNKNOWN is the
   first enum.) I'm fairly sure this behavior is ANSI-mandated,
   so there should be no portability problems here. */
#define image_instance_type_to_mask(type) \
  ((int) (1 << ((int) (type) - 1)))

#define IMAGE_INSTANCE_TYPE_P(ii, type) \
(IMAGE_INSTANCEP (ii) && XIMAGE_INSTANCE_TYPE (ii) == type)

#define NOTHING_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_NOTHING)
#define TEXT_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_TEXT)
#define MONO_PIXMAP_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_MONO_PIXMAP)
#define COLOR_PIXMAP_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_COLOR_PIXMAP)
#define POINTER_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_POINTER)
#define SUBWINDOW_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_SUBWINDOW)
#define WIDGET_IMAGE_INSTANCEP(ii) \
     IMAGE_INSTANCE_TYPE_P (ii, IMAGE_WIDGET)

#define CHECK_NOTHING_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);					\
  if (!NOTHING_IMAGE_INSTANCEP (x))				\
    x = wrong_type_argument (Qnothing_image_instance_p, (x));	\
} while (0)

#define CHECK_TEXT_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);					\
  if (!TEXT_IMAGE_INSTANCEP (x))				\
    x = wrong_type_argument (Qtext_image_instance_p, (x));	\
} while (0)

#define CHECK_MONO_PIXMAP_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);						\
  if (!MONO_PIXMAP_IMAGE_INSTANCEP (x))					\
    x = wrong_type_argument (Qmono_pixmap_image_instance_p, (x));	\
} while (0)

#define CHECK_COLOR_PIXMAP_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);						\
  if (!COLOR_PIXMAP_IMAGE_INSTANCEP (x))				\
    x = wrong_type_argument (Qcolor_pixmap_image_instance_p, (x));	\
} while (0)

#define CHECK_POINTER_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);					\
  if (!POINTER_IMAGE_INSTANCEP (x))				\
    x = wrong_type_argument (Qpointer_image_instance_p, (x));	\
} while (0)

#define CHECK_SUBWINDOW_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);					\
  if (!SUBWINDOW_IMAGE_INSTANCEP (x)				\
      && !WIDGET_IMAGE_INSTANCEP (x))				\
    x = wrong_type_argument (Qsubwindow_image_instance_p, (x));	\
} while (0)

#define CHECK_WIDGET_IMAGE_INSTANCE(x) do {			\
  CHECK_IMAGE_INSTANCE (x);					\
  if (!WIDGET_IMAGE_INSTANCEP (x))				\
    x = wrong_type_argument (Qwidget_image_instance_p, (x));	\
} while (0)

struct Lisp_Image_Instance {
	struct lcrecord_header header;
	Lisp_Object domain;	/* The domain in which we were cached. */
	Lisp_Object device;	/* The device of the domain. Recorded
				   since the domain may get deleted
				   before us. */
	Lisp_Object name;
	/* The glyph from which we were instantiated. This is a weak
	   reference. */
	Lisp_Object parent;
	/* The instantiator from which we were instantiated. */
	Lisp_Object instantiator;
	enum image_instance_type type;
	unsigned int x_offset, y_offset;	/* for layout purposes */
	int width, height, margin_width;
	unsigned long display_hash;	/* Hash value representing the structure
					   of the image_instance when it was
					   last displayed. */
	unsigned int dirty:1;
	unsigned int size_changed:1;
	unsigned int text_changed:1;
	unsigned int layout_changed:1;
	unsigned int optimize_output:1;	/* For outputting layouts. */
	unsigned int initialized:1;	/* When we're fully done. */
	unsigned int wants_initial_focus:1;

	union {
		struct {
			unsigned int descent;
			Lisp_Object string;
		} text;
		struct {
			unsigned int depth;
			unsigned int slice, maxslice, timeout;
			Lisp_Object hotspot_x, hotspot_y;	/* integer or Qnil */
			Lisp_Object filename;	/* string or Qnil */
			Lisp_Object mask_filename;	/* string or Qnil */
			Lisp_Object fg, bg;	/* foreground and background colors,
						   if this is a colorized mono-pixmap
						   or a pointer */
			Lisp_Object auxdata;	/* list or Qnil: any additional data
						   to be seen from lisp */
			void *mask;	/* mask that can be seen from all windowing systems */
		} pixmap;	/* used for pointers as well */
		struct {
			void *subwindow;	/* specific devices can use this as necessary */
			struct {	/* We need these so we can do without
					   subwindow_cachel */
				unsigned int x, y;
				unsigned int width, height;
			} display_data;
			unsigned int being_displayed:1;	/* used to detect when needs
							   to be unmapped */
			unsigned int v_resize:1;	/* Whether the vsize is allowed to change. */
			unsigned int h_resize:1;	/* Whether the hsize is allowed to change. */
			unsigned int orientation:1;	/* Vertical or horizontal. */
			unsigned int h_justification:2;	/* left, right or center. */
			unsigned int v_justification:2;	/* top, bottom or center. */
			/* Face for colors and font. We specify this here because we
			   want people to be able to put :face in the instantiator
			   spec. Using glyph-face is more inconvenient, although more
			   general. */
			Lisp_Object face;
			Lisp_Object type;
			Lisp_Object props;	/* properties or border */
			Lisp_Object items;	/* a list of displayed gui_items */
			Lisp_Object pending_items;	/* gui_items that should be displayed */
			Lisp_Object children;	/* a list of children */
			Lisp_Object width;	/* dynamic width spec. */
			Lisp_Object height;	/* dynamic height spec. */
			/* Change flags to augment dirty. */
			unsigned int face_changed:1;
			unsigned int items_changed:1;
			unsigned int action_occurred:1;
		} subwindow;
	} u;

	/* console-type- and image-type-specific data */
	void *data;
};

/* Layout bit-fields. */
#define LAYOUT_HORIZONTAL	0
#define LAYOUT_VERTICAL	1

#define LAYOUT_JUSTIFY_LEFT 0
#define LAYOUT_JUSTIFY_TOP 0
#define LAYOUT_JUSTIFY_RIGHT 1
#define LAYOUT_JUSTIFY_BOTTOM 1
#define LAYOUT_JUSTIFY_CENTER 2

#define IMAGE_INSTANCE_HASH_DEPTH 0

/* Accessor macros. */
#define IMAGE_INSTANCE_DOMAIN(i) ((i)->domain)
#define IMAGE_INSTANCE_DOMAIN_LIVE_P(i) (DOMAIN_LIVE_P ((i)->domain))
#define IMAGE_INSTANCE_DEVICE(i) ((i)->device)
#define IMAGE_INSTANCE_FRAME(i) (DOMAIN_FRAME ((i)->domain))
#define IMAGE_INSTANCE_NAME(i) ((i)->name)
#define IMAGE_INSTANCE_PARENT(i) ((i)->parent)
#define IMAGE_INSTANCE_INSTANTIATOR(i) ((i)->instantiator)
#define IMAGE_INSTANCE_GLYPH(i) (image_instance_parent_glyph(i))
#define IMAGE_INSTANCE_TYPE(i) ((i)->type)
#define IMAGE_INSTANCE_XOFFSET(i) ((i)->x_offset)
#define IMAGE_INSTANCE_YOFFSET(i) ((i)->y_offset)
#define IMAGE_INSTANCE_WIDTH(i) ((i)->width)
#define IMAGE_INSTANCE_MARGIN_WIDTH(i) ((i)->margin_width)
#define IMAGE_INSTANCE_HEIGHT(i) ((i)->height)
#define IMAGE_INSTANCE_INITIALIZED(i) ((i)->initialized)
#define IMAGE_INSTANCE_DISPLAY_HASH(i) ((i)->display_hash)
#define IMAGE_INSTANCE_PIXMAP_TYPE_P(i)			\
 ((IMAGE_INSTANCE_TYPE (i) == IMAGE_MONO_PIXMAP)	\
  || (IMAGE_INSTANCE_TYPE (i) == IMAGE_COLOR_PIXMAP))
#define IMAGE_INSTANCE_DIRTYP(i) ((i)->dirty)
#define IMAGE_INSTANCE_NEEDS_LAYOUT(i) \
  ((IMAGE_INSTANCE_DIRTYP (i) && IMAGE_INSTANCE_LAYOUT_CHANGED (i)) \
   || (FRAMEP (IMAGE_INSTANCE_FRAME (i)) \
       && XFRAME (IMAGE_INSTANCE_FRAME (i))->size_changed))
#if 0
/* converted to inline for clarity */
#define IMAGE_INSTANCE_FACE(i)				\
	(GLYPHP (IMAGE_INSTANCE_GLYPH (i)) ?		\
	 XGLYPH_FACE (IMAGE_INSTANCE_GLYPH (i)) : Qnil)
#else  /* !0 */
static inline Lisp_Object
IMAGE_INSTANCE_FACE(Lisp_Image_Instance *i)
	__attribute__((always_inline));
#endif
#define IMAGE_INSTANCE_WANTS_INITIAL_FOCUS(i) ((i)->wants_initial_focus)

/* Changed flags */
#define IMAGE_INSTANCE_TEXT_CHANGED(i) ((i)->text_changed)
#define IMAGE_INSTANCE_SIZE_CHANGED(i) ((i)->size_changed)
#define IMAGE_INSTANCE_WIDGET_FACE_CHANGED(i) \
  ((i)->u.subwindow.face_changed)
#define IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED(i) \
  ((i)->u.subwindow.items_changed)
#define IMAGE_INSTANCE_WIDGET_ACTION_OCCURRED(i) \
  ((i)->u.subwindow.action_occurred)
#define IMAGE_INSTANCE_LAYOUT_CHANGED(i) ((i)->layout_changed)
#define IMAGE_INSTANCE_OPTIMIZE_OUTPUT(i) ((i)->optimize_output)

/* Text properties */
#define IMAGE_INSTANCE_TEXT_STRING(i) ((i)->u.text.string)
#define IMAGE_INSTANCE_TEXT_WIDTH(i) \
  IMAGE_INSTANCE_WIDTH(i)
#define IMAGE_INSTANCE_TEXT_HEIGHT(i) \
  IMAGE_INSTANCE_HEIGHT(i)
#define IMAGE_INSTANCE_TEXT_DESCENT(i) ((i)->u.text.descent)
#define IMAGE_INSTANCE_TEXT_ASCENT(i) \
  (IMAGE_INSTANCE_TEXT_HEIGHT(i) - IMAGE_INSTANCE_TEXT_DESCENT(i))

/* Pixmap properties */
#define IMAGE_INSTANCE_PIXMAP_WIDTH(i) \
  IMAGE_INSTANCE_WIDTH(i)
#define IMAGE_INSTANCE_PIXMAP_HEIGHT(i) \
  IMAGE_INSTANCE_HEIGHT(i)
#define IMAGE_INSTANCE_PIXMAP_DEPTH(i) ((i)->u.pixmap.depth)
#define IMAGE_INSTANCE_PIXMAP_FILENAME(i) ((i)->u.pixmap.filename)
#define IMAGE_INSTANCE_PIXMAP_MASK_FILENAME(i) ((i)->u.pixmap.mask_filename)
#define IMAGE_INSTANCE_PIXMAP_HOTSPOT_X(i) ((i)->u.pixmap.hotspot_x)
#define IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(i) ((i)->u.pixmap.hotspot_y)
#define IMAGE_INSTANCE_PIXMAP_FG(i) ((i)->u.pixmap.fg)
#define IMAGE_INSTANCE_PIXMAP_BG(i) ((i)->u.pixmap.bg)
#define IMAGE_INSTANCE_PIXMAP_AUXDATA(i) ((i)->u.pixmap.auxdata)
#define IMAGE_INSTANCE_PIXMAP_MASK(i) ((i)->u.pixmap.mask)
#define IMAGE_INSTANCE_PIXMAP_SLICE(i) ((i)->u.pixmap.slice)
#define IMAGE_INSTANCE_PIXMAP_MAXSLICE(i) ((i)->u.pixmap.maxslice)
#define IMAGE_INSTANCE_PIXMAP_TIMEOUT(i) ((i)->u.pixmap.timeout)

/* Subwindow properties */
#define IMAGE_INSTANCE_SUBWINDOW_ID(i) ((i)->u.subwindow.subwindow)
/* Display data. */
#define IMAGE_INSTANCE_DISPLAY_X(i) ((i)->u.subwindow.display_data.x)
#define IMAGE_INSTANCE_DISPLAY_Y(i) ((i)->u.subwindow.display_data.y)
#define IMAGE_INSTANCE_DISPLAY_WIDTH(i) \
  ((i)->u.subwindow.display_data.width)
#define IMAGE_INSTANCE_DISPLAY_HEIGHT(i) \
  ((i)->u.subwindow.display_data.height)
#define IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(i) \
((i)->u.subwindow.being_displayed)
#define IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(i) \
((i)->u.subwindow.v_resize)
#define IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(i) \
((i)->u.subwindow.h_resize)
#define IMAGE_INSTANCE_SUBWINDOW_ORIENT(i) \
((i)->u.subwindow.orientation)
#define IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(i) \
((i)->u.subwindow.h_justification)
#define IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(i) \
((i)->u.subwindow.v_justification)
#define IMAGE_INSTANCE_SUBWINDOW_RIGHT_JUSTIFIED(i) \
 (IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(i) == LAYOUT_JUSTIFY_RIGHT)
#define IMAGE_INSTANCE_SUBWINDOW_LEFT_JUSTIFIED(i) \
 (IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(i) == LAYOUT_JUSTIFY_LEFT)
#define IMAGE_INSTANCE_SUBWINDOW_TOP_JUSTIFIED(i) \
 (IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(i) == LAYOUT_JUSTIFY_TOP)
#define IMAGE_INSTANCE_SUBWINDOW_BOTTOM_JUSTIFIED(i) \
 (IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(i) == LAYOUT_JUSTIFY_BOTTOM)
#define IMAGE_INSTANCE_SUBWINDOW_H_CENTERED(i) \
 (IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(i) == LAYOUT_JUSTIFY_CENTER)
#define IMAGE_INSTANCE_SUBWINDOW_V_CENTERED(i) \
 (IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(i) == LAYOUT_JUSTIFY_CENTER)
#define IMAGE_INSTANCE_SUBWINDOW_LOGICAL_LAYOUT(i) \
 (IMAGE_INSTANCE_SUBWINDOW_ORIENT (i) \
  == LAYOUT_VERTICAL && !IMAGE_INSTANCE_SUBWINDOW_V_CENTERED (i))

#define IMAGE_INSTANCE_SUBWINDOW_FACE(i) \
((i)->u.subwindow.face)

/* Widget properties */
#define IMAGE_INSTANCE_WIDGET_WIDTH(i) \
  IMAGE_INSTANCE_WIDTH(i)
#define IMAGE_INSTANCE_WIDGET_HEIGHT(i) \
  IMAGE_INSTANCE_HEIGHT(i)
#define IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(i) ((i)->u.subwindow.width)
#define IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(i) ((i)->u.subwindow.height)
#define IMAGE_INSTANCE_WIDGET_TYPE(i) ((i)->u.subwindow.type)
#define IMAGE_INSTANCE_WIDGET_PROPS(i) ((i)->u.subwindow.props)
#define SET_IMAGE_INSTANCE_WIDGET_FACE(i,f) \
 ((i)->u.subwindow.face = f)
#if 0
/* converted to an inline for better readability */
#define IMAGE_INSTANCE_WIDGET_FACE(i)				\
  (!NILP ((i)->u.subwindow.face) ? (i)->u.subwindow.face :	\
  !NILP (IMAGE_INSTANCE_FACE (i)) ? IMAGE_INSTANCE_FACE (i) :	\
  Vwidget_face)
#else  /* !0 */
static inline Lisp_Object
IMAGE_INSTANCE_WIDGET_FACE(Lisp_Image_Instance *i)
	__attribute__((always_inline));
#endif	/* 0 */

#define IMAGE_INSTANCE_WIDGET_ITEMS(i) ((i)->u.subwindow.items)
#define IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(i) \
  ((i)->u.subwindow.pending_items)
#define IMAGE_INSTANCE_WIDGET_ITEM(i)		\
  (CONSP (IMAGE_INSTANCE_WIDGET_ITEMS (i)) ?	\
   XCAR (IMAGE_INSTANCE_WIDGET_ITEMS (i)) :	\
   IMAGE_INSTANCE_WIDGET_ITEMS (i))
#define IMAGE_INSTANCE_WIDGET_TEXT(i) \
   XGUI_ITEM (IMAGE_INSTANCE_WIDGET_ITEM (i))->name

/* Layout properties */
#define IMAGE_INSTANCE_LAYOUT_CHILDREN(i) ((i)->u.subwindow.children)
#define IMAGE_INSTANCE_LAYOUT_BORDER(i) ((i)->u.subwindow.props)

#define XIMAGE_INSTANCE_DOMAIN(i) \
  IMAGE_INSTANCE_DOMAIN (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DOMAIN_LIVE_P(i) \
  IMAGE_INSTANCE_DOMAIN_LIVE_P (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DEVICE(i) \
  IMAGE_INSTANCE_DEVICE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_FRAME(i) \
  IMAGE_INSTANCE_FRAME (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_NAME(i) \
  IMAGE_INSTANCE_NAME (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GLYPH(i) \
  IMAGE_INSTANCE_GLYPH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PARENT(i) \
  IMAGE_INSTANCE_PARENT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_INSTANTIATOR(i) \
  IMAGE_INSTANCE_INSTANTIATOR (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_TYPE(i) \
  IMAGE_INSTANCE_TYPE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DISPLAY_HASH(i) \
  IMAGE_INSTANCE_DISPLAY_HASH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_XOFFSET(i) \
  IMAGE_INSTANCE_XOFFSET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_YOFFSET(i) \
  IMAGE_INSTANCE_YOFFSET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DIRTYP(i) \
  IMAGE_INSTANCE_DIRTYP (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_NEEDS_LAYOUT(i) \
  IMAGE_INSTANCE_NEEDS_LAYOUT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDTH(i) \
  IMAGE_INSTANCE_WIDTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_MARGIN_WIDTH(i) \
  IMAGE_INSTANCE_MARGIN_WIDTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_HEIGHT(i) \
  IMAGE_INSTANCE_HEIGHT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_INITIALIZED(i) \
  IMAGE_INSTANCE_INITIALIZED (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_FACE(i) \
  IMAGE_INSTANCE_FACE (XIMAGE_INSTANCE (i))

#define XIMAGE_INSTANCE_TEXT_STRING(i) \
  IMAGE_INSTANCE_TEXT_STRING (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_TEXT_WIDTH(i) \
  IMAGE_INSTANCE_TEXT_WIDTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_TEXT_HEIGHT(i) \
  IMAGE_INSTANCE_TEXT_HEIGHT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_TEXT_ASCENT(i) \
  IMAGE_INSTANCE_TEXT_ASCENT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_TEXT_DESCENT(i) \
  IMAGE_INSTANCE_TEXT_DESCENT (XIMAGE_INSTANCE (i))

#define XIMAGE_INSTANCE_PIXMAP_WIDTH(i) \
  IMAGE_INSTANCE_PIXMAP_WIDTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_HEIGHT(i) \
  IMAGE_INSTANCE_PIXMAP_HEIGHT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_DEPTH(i) \
  IMAGE_INSTANCE_PIXMAP_DEPTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_FILENAME(i) \
  IMAGE_INSTANCE_PIXMAP_FILENAME (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_MASK_FILENAME(i) \
  IMAGE_INSTANCE_PIXMAP_MASK_FILENAME (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_HOTSPOT_X(i) \
  IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_HOTSPOT_Y(i) \
  IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_FG(i) \
  IMAGE_INSTANCE_PIXMAP_FG (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_BG(i) \
  IMAGE_INSTANCE_PIXMAP_BG (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_MASK(i) \
  IMAGE_INSTANCE_PIXMAP_MASK (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_SLICE(i) \
  IMAGE_INSTANCE_PIXMAP_SLICE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_MAXSLICE(i) \
  IMAGE_INSTANCE_PIXMAP_MAXSLICE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_PIXMAP_TIMEOUT(i) \
  IMAGE_INSTANCE_PIXMAP_TIMEOUT (XIMAGE_INSTANCE (i))

#define XIMAGE_INSTANCE_WIDGET_WIDTH(i) \
  IMAGE_INSTANCE_WIDGET_WIDTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_HEIGHT(i) \
  IMAGE_INSTANCE_WIDGET_HEIGHT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_WIDTH_SUBR(i) \
  IMAGE_INSTANCE_WIDGET_WIDTH_SUBR (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(i) \
  IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_TYPE(i) \
  IMAGE_INSTANCE_WIDGET_TYPE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_PROPS(i) \
  IMAGE_INSTANCE_WIDGET_PROPS (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_FACE(i) \
  IMAGE_INSTANCE_WIDGET_FACE (XIMAGE_INSTANCE (i))
#define XSET_IMAGE_INSTANCE_WIDGET_FACE(i) \
  SET_IMAGE_INSTANCE_WIDGET_FACE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_ITEM(i) \
  IMAGE_INSTANCE_WIDGET_ITEM (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_ITEMS(i) \
  IMAGE_INSTANCE_WIDGET_ITEMS (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_PENDING_ITEMS(i) \
  IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_TEXT(i) \
  IMAGE_INSTANCE_WIDGET_TEXT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_WIDGET_ACTION_OCCURRED(i) \
  IMAGE_INSTANCE_WIDGET_ACTION_OCCURRED (XIMAGE_INSTANCE (i))

#define XIMAGE_INSTANCE_LAYOUT_CHILDREN(i) \
  IMAGE_INSTANCE_LAYOUT_CHILDREN (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_LAYOUT_BORDER(i) \
  IMAGE_INSTANCE_LAYOUT_BORDER (XIMAGE_INSTANCE (i))

#define XIMAGE_INSTANCE_SUBWINDOW_ID(i) \
  IMAGE_INSTANCE_SUBWINDOW_ID (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DISPLAY_X(i) \
  IMAGE_INSTANCE_DISPLAY_X (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DISPLAY_Y(i) \
  IMAGE_INSTANCE_DISPLAY_Y (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DISPLAY_WIDTH(i) \
  IMAGE_INSTANCE_DISPLAY_WIDTH (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_DISPLAY_HEIGHT(i) \
  IMAGE_INSTANCE_DISPLAY_HEIGHT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP(i) \
  IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_SUBWINDOW_ORIENT(i) \
  IMAGE_INSTANCE_SUBWINDOW_ORIENT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_SUBWINDOW_JUSTIFY(i) \
  IMAGE_INSTANCE_SUBWINDOW_JUSTIFY (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_SUBWINDOW_FACE(i) \
  IMAGE_INSTANCE_SUBWINDOW_FACE (XIMAGE_INSTANCE (i))

#define MARK_IMAGE_INSTANCE_CHANGED(i) \
  (IMAGE_INSTANCE_DIRTYP (i) = 1);

Lisp_Object image_instance_device(Lisp_Object instance);
Lisp_Object image_instance_frame(Lisp_Object instance);
Lisp_Object image_instance_window(Lisp_Object instance);
int image_instance_live_p(Lisp_Object instance);

#ifdef HAVE_XPM
Lisp_Object evaluate_xpm_color_symbols(void);
Lisp_Object pixmap_to_lisp_data(Lisp_Object name, int ok_if_data_invalid);
#endif				/* HAVE_XPM */
#ifdef HAVE_WINDOW_SYSTEM
Lisp_Object bitmap_to_lisp_data(Lisp_Object name, int *xhot, int *yhot,
				int ok_if_data_invalid);
int read_bitmap_data_from_file(const char *filename, unsigned int *width,
			       unsigned int *height, unsigned char **datap,
			       int *x_hot, int *y_hot);
Lisp_Object xbm_mask_file_munging(Lisp_Object alist, Lisp_Object file,
				  Lisp_Object mask_file,
				  Lisp_Object console_type);
#endif

/************************************************************************/
/*				Glyph Object				*/
/************************************************************************/

enum glyph_type {
	GLYPH_UNKNOWN,
	GLYPH_BUFFER,
	GLYPH_POINTER,
	GLYPH_ICON
};

struct Lisp_Glyph {
	struct lcrecord_header header;

	enum glyph_type type;

	/* specifiers: */
	Lisp_Object image;	/* the actual image */
	Lisp_Object contrib_p;	/* whether to figure into line height */
	Lisp_Object baseline;	/* percent above baseline */

	Lisp_Object face;	/* if non-nil, face to use when displaying */

	Lisp_Object plist;
	void (*after_change) (Lisp_Object glyph, Lisp_Object property,
			      Lisp_Object locale);

	unsigned int dirty:1;	/* So that we can selectively
				   redisplay changed glyphs. */
};
typedef struct Lisp_Glyph Lisp_Glyph;

DECLARE_LRECORD(glyph, Lisp_Glyph);
#define XGLYPH(x) XRECORD (x, glyph, Lisp_Glyph)
#define XSETGLYPH(x, p) XSETRECORD (x, p, glyph)
#define GLYPHP(x) RECORDP (x, glyph)
#define CHECK_GLYPH(x) CHECK_RECORD (x, glyph)
#define CONCHECK_GLYPH(x) CONCHECK_RECORD (x, glyph)

#define CHECK_BUFFER_GLYPH(x) do {			\
  CHECK_GLYPH (x);					\
  if (XGLYPH (x)->type != GLYPH_BUFFER)			\
    x = wrong_type_argument (Qbuffer_glyph_p, (x));	\
} while (0)

#define CHECK_POINTER_GLYPH(x) do {			\
  CHECK_GLYPH (x);					\
  if (XGLYPH (x)->type != GLYPH_POINTER)		\
    x = wrong_type_argument (Qpointer_glyph_p, (x));	\
} while (0)

#define CHECK_ICON_GLYPH(x) do {			\
  CHECK_GLYPH (x);					\
  if (XGLYPH (x)->type != GLYPH_ICON)			\
    x = wrong_type_argument (Qicon_glyph_p, (x));	\
} while (0)

#define GLYPH_TYPE(g) ((g)->type)
#define GLYPH_IMAGE(g) ((g)->image)
#define GLYPH_CONTRIB_P(g) ((g)->contrib_p)
#define GLYPH_BASELINE(g) ((g)->baseline)
#define GLYPH_FACE(g) ((g)->face)
#define GLYPH_DIRTYP(g) ((g)->dirty)

#define XGLYPH_TYPE(g) GLYPH_TYPE (XGLYPH (g))
#define XGLYPH_IMAGE(g) GLYPH_IMAGE (XGLYPH (g))
#define XGLYPH_CONTRIB_P(g) GLYPH_CONTRIB_P (XGLYPH (g))
#define XGLYPH_BASELINE(g) GLYPH_BASELINE (XGLYPH (g))
#define XGLYPH_FACE(g) GLYPH_FACE (XGLYPH (g))
#define XGLYPH_DIRTYP(g) GLYPH_DIRTYP (XGLYPH (g))

#define MARK_GLYPH_CHANGED(g) (GLYPH_DIRTYP (g) = 1);

extern Lisp_Object Qxpm, Qxface, Qetched_in, Qetched_out, Qbevel_in, Qbevel_out;
extern Lisp_Object Q_data, Q_file, Q_color_symbols, Qconst_glyph_variable;
extern Lisp_Object Qxbm, Qedit_field, Qgroup, Qlabel, Qcombo_box, Qscrollbar;
extern Lisp_Object Qtree_view, Qtab_control, Qprogress_gauge, Q_border;
extern Lisp_Object Q_mask_file, Q_mask_data, Q_hotspot_x, Q_hotspot_y;
extern Lisp_Object Q_foreground, Q_background, Q_face, Q_descriptor, Q_group;
extern Lisp_Object Q_width, Q_height, Q_pixel_width, Q_pixel_height, Q_text;
extern Lisp_Object Q_items, Q_properties, Q_image, Qimage_conversion_error;
extern Lisp_Object Q_orientation, Q_margin_width;
extern Lisp_Object Vcontinuation_glyph, Vcontrol_arrow_glyph, Vhscroll_glyph;
extern Lisp_Object Vinvisible_text_glyph, Voctal_escape_glyph,
    Vtruncation_glyph;
extern Lisp_Object Vsxemacs_logo;

unsigned short glyph_width(Lisp_Object glyph, Lisp_Object domain);
unsigned short glyph_ascent(Lisp_Object glyph, Lisp_Object domain);
unsigned short glyph_descent(Lisp_Object glyph, Lisp_Object domain);
unsigned short glyph_height(Lisp_Object glyph, Lisp_Object domain);
Lisp_Object glyph_baseline(Lisp_Object glyph, Lisp_Object domain);
Lisp_Object glyph_face(Lisp_Object glyph, Lisp_Object domain);
int glyph_contrib_p(Lisp_Object glyph, Lisp_Object domain);
Lisp_Object glyph_image_instance(Lisp_Object glyph,
				 Lisp_Object domain,
				 Error_behavior errb, int no_quit);
void file_or_data_must_be_present(Lisp_Object instantiator);
void data_must_be_present(Lisp_Object instantiator);
Lisp_Object make_string_from_file(Lisp_Object file);
Lisp_Object tagged_vector_to_alist(Lisp_Object vector);
Lisp_Object alist_to_tagged_vector(Lisp_Object tag, Lisp_Object alist);
void string_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			int dest_mask, Lisp_Object domain);
int tab_control_order_only_changed(Lisp_Object image_instance);
Lisp_Object allocate_glyph(enum glyph_type type,
			   void (*after_change) (Lisp_Object glyph,
						 Lisp_Object property,
						 Lisp_Object locale));
Lisp_Object normalize_image_instantiator(Lisp_Object instantiator,
					 Lisp_Object contype,
					 Lisp_Object dest_mask);
void glyph_query_geometry(Lisp_Object glyph_or_image, int *width, int *height,
			  enum image_instance_geometry disp,
			  Lisp_Object domain);
void glyph_do_layout(Lisp_Object glyph_or_image, int width, int height,
		     int xoffset, int yoffset, Lisp_Object domain);
void query_string_geometry(Lisp_Object string, Lisp_Object face,
			   int *width, int *height, int *descent,
			   Lisp_Object domain);
Lisp_Object query_string_font(Lisp_Object string,
			      Lisp_Object face, Lisp_Object domain);
Lisp_Object add_glyph_animated_timeout(EMACS_INT tickms, Lisp_Object device);
void disable_glyph_animated_timeout(int i);

/************************************************************************/
/*				Glyph Cachels				*/
/************************************************************************/

typedef struct glyph_cachel glyph_cachel;
struct glyph_cachel {
	Lisp_Object glyph;

	unsigned int dirty:1;	/* I'm copying faces here. I'm not
				   sure why we need two dirty
				   flags. Maybe because an image
				   instance can be dirty and so we
				   need to frob this in the same way
				   as other image instance properties.  */
	unsigned int updated:1;

	unsigned short width;
	unsigned short ascent;
	unsigned short descent;
};

#define CONT_GLYPH_INDEX	(glyph_index) 0
#define TRUN_GLYPH_INDEX	(glyph_index) 1
#define HSCROLL_GLYPH_INDEX	(glyph_index) 2
#define CONTROL_GLYPH_INDEX	(glyph_index) 3
#define OCT_ESC_GLYPH_INDEX	(glyph_index) 4
#define INVIS_GLYPH_INDEX	(glyph_index) 5

#ifdef ERROR_CHECK_GLYPHS

#include "window.h"

extern_inline int GLYPH_CACHEL_WIDTH(struct window *window, int ind);
extern_inline int GLYPH_CACHEL_WIDTH(struct window *window, int ind)
{
	int wid = Dynarr_atp(window->glyph_cachels, ind)->width;
	assert(wid >= 0 && wid < 10000);
	return wid;
}
extern_inline int GLYPH_CACHEL_ASCENT(struct window *window, int ind);
extern_inline int GLYPH_CACHEL_ASCENT(struct window *window, int ind)
{
	int wid = Dynarr_atp(window->glyph_cachels, ind)->ascent;
	assert(wid >= 0 && wid < 10000);
	return wid;
}
extern_inline int GLYPH_CACHEL_DESCENT(struct window *window, int ind);
extern_inline int GLYPH_CACHEL_DESCENT(struct window *window, int ind)
{
	int wid = Dynarr_atp(window->glyph_cachels, ind)->descent;
	assert(wid >= 0 && wid < 10000);
	return wid;
}

#else				/* not ERROR_CHECK_GLYPHS */

#define GLYPH_CACHEL_WIDTH(window, ind)		\
  Dynarr_atp (window->glyph_cachels, ind)->width
#define GLYPH_CACHEL_ASCENT(window, ind)		\
  Dynarr_atp (window->glyph_cachels, ind)->ascent
#define GLYPH_CACHEL_DESCENT(window, ind)		\
  Dynarr_atp (window->glyph_cachels, ind)->descent

#endif				/* not ERROR_CHECK_GLYPHS */

#define GLYPH_CACHEL(window, ind)			\
  Dynarr_atp (window->glyph_cachels, ind)
#define GLYPH_CACHEL_GLYPH(window, ind)		\
  Dynarr_atp (window->glyph_cachels, ind)->glyph
#define GLYPH_CACHEL_DIRTYP(window, ind)		\
  Dynarr_atp (window->glyph_cachels, ind)->dirty

void mark_glyph_cachels(glyph_cachel_dynarr * elements);
void mark_glyph_cachels_as_not_updated(struct window *w);
void mark_glyph_cachels_as_clean(struct window *w);
void reset_glyph_cachels(struct window *w);
glyph_index get_glyph_cachel_index(struct window *w, Lisp_Object glyph);

#ifdef MEMORY_USAGE_STATS
int compute_glyph_cachel_usage(glyph_cachel_dynarr * glyph_cachels,
			       struct overhead_stats *ovstats);
#endif				/* MEMORY_USAGE_STATS */

/************************************************************************/
/*				Display Tables				*/
/************************************************************************/

Lisp_Object display_table_entry(Emchar, Lisp_Object, Lisp_Object);
void get_display_tables(struct window *, face_index,
			Lisp_Object *, Lisp_Object *);

/****************************************************************************
 *                            Subwindow Object                              *
 ****************************************************************************/

void unmap_subwindow(Lisp_Object subwindow);
void map_subwindow(Lisp_Object subwindow, int x, int y,
		   struct display_glyph_area *dga);
int find_matching_subwindow(struct frame *f, int x, int y, int width,
			    int height);
void redisplay_widget(Lisp_Object widget);
void update_widget_instances(Lisp_Object frame);
void redisplay_subwindow(Lisp_Object subwindow);
Lisp_Object image_instance_parent_glyph(struct Lisp_Image_Instance *);
int image_instance_changed(Lisp_Object image);
void free_frame_subwindow_instances(struct frame *f);
void reset_frame_subwindow_instance_cache(struct frame *f);
int unmap_subwindow_instance_cache_mapper(Lisp_Object key,
					  Lisp_Object value, void *finalize);

struct expose_ignore {
	unsigned int x, y;
	unsigned int width, height;
	struct expose_ignore *next;
};

int check_for_ignored_expose(struct frame *f, int x, int y, int width,
			     int height);
extern int hold_ignored_expose_registration;

#define ROUND_UP(arg, unit) (((int)((arg) + (unit) - 1) / (int)(unit)) * (int)(unit))


/* inlines */
static inline Lisp_Object
IMAGE_INSTANCE_FACE(Lisp_Image_Instance *i)
{
	Lisp_Object tmp = IMAGE_INSTANCE_GLYPH(i);

	if (GLYPHP(tmp)) {
		return XGLYPH_FACE(tmp);
	} else {
		return Qnil;
	}
}

static inline Lisp_Object
IMAGE_INSTANCE_WIDGET_FACE(Lisp_Image_Instance *i)
{
	extern Lisp_Object Vwidget_face;
	Lisp_Object res;

	if (!NILP(res = (i)->u.subwindow.face)) {
		return res;
	} else if (!NILP(res = IMAGE_INSTANCE_FACE(i))) {
		return res;
	} else {
		return Vwidget_face;
	}
}

#endif  /* INCLUDED_glyphs_h_ */
