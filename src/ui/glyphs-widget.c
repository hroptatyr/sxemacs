/* Widget-specific glyph objects.
   Copyright (C) 1998, 1999, 2000, 2002 Andy Piper.

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

/* written by Andy Piper <andy@xemacs.org> */

#include <config.h>
#include "lisp.h"
#include "lstream.h"
#include "console.h"
#include "device.h"
#include "faces.h"
#include "glyphs.h"
#include "objects.h"
#include "bytecode.h"
#include "window.h"
#include "buffer.h"
#include "frame.h"
#include "insdel.h"
#include "opaque.h"

DEFINE_IMAGE_INSTANTIATOR_FORMAT(button);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(combo_box);
Lisp_Object Qcombo_box;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(edit_field);
Lisp_Object Qedit_field;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(scrollbar);
Lisp_Object Qscrollbar;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(widget);
DEFINE_IMAGE_INSTANTIATOR_FORMAT(label);
Lisp_Object Qlabel;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(progress_gauge);
Lisp_Object Qprogress_gauge;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(tree_view);
Lisp_Object Qtree_view;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(tab_control);
Lisp_Object Qtab_control;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(layout);
Lisp_Object Qlayout;
DEFINE_IMAGE_INSTANTIATOR_FORMAT(native_layout);
Lisp_Object Qnative_layout;

Lisp_Object Qetched_in, Qetched_out, Qbevel_in, Qbevel_out;
Lisp_Object Qmake_glyph;
Lisp_Object Vwidget_border_width;

static int widget_border_width(Lisp_Object domain);
static int widget_spacing(Lisp_Object domain);
#define BORDER_FIDDLE_FACTOR 10
#ifdef DEBUG_WIDGETS
int debug_widget_instances;
#endif

/* TODO:
   - tooltips for controls, especially buttons.
   - keyboard traversal.
   - lisp configurable layout.
 */

/* In MS-Windows normal windows work in pixels, dialog boxes work in
   dialog box units. Why? sigh. We could reuse the metrics for dialogs
   if this were not the case. As it is we have to position things
   pixel wise. I'm not even sure that X has this problem at least for
   buttons in groups. */
static int widget_possible_dest_types(void)
{
	return IMAGE_WIDGET_MASK;
}

static void check_valid_instantiator(Lisp_Object data)
{
	Lisp_Object glyph = data;
	if (SYMBOLP(data))
		glyph = XSYMBOL(data)->value;

	if (!CONSP(glyph) && !VECTORP(glyph))
		invalid_argument("instantiator item must be a vector", data);
}

static void check_valid_orientation(Lisp_Object data)
{
	if (!EQ(data, Qhorizontal)
	    && !EQ(data, Qvertical))
		invalid_argument("unknown orientation for layout", data);
}

static void check_valid_tab_orientation(Lisp_Object data)
{
	if (!EQ(data, Qtop)
	    && !EQ(data, Qbottom)
	    && !EQ(data, Qleft)
	    && !EQ(data, Qright))
		invalid_argument("unknown orientation for tab control", data);
}

static void check_valid_justification(Lisp_Object data)
{
	if (!EQ(data, Qleft)
	    && !EQ(data, Qright)
	    && !EQ(data, Qtop)
	    && !EQ(data, Qbottom)
	    && !EQ(data, Qcenter))
		invalid_argument("unknown justification for layout", data);
}

static void check_valid_border(Lisp_Object data)
{
	if (!EQ(data, Qt) && !EQ(data, Qetched_in) && !EQ(data, Qetched_out)
	    && !EQ(data, Qbevel_in) && !EQ(data, Qbevel_out)
	    && !GLYPHP(data) && !VECTORP(data))
		invalid_argument("unknown border style for layout", data);
}

static void check_valid_anything(Lisp_Object data)
{
}

static void check_valid_callback(Lisp_Object data)
{
	if (!SYMBOLP(data)
	    && !COMPILED_FUNCTIONP(data)
	    && !CONSP(data)) {
		invalid_argument(":callback must be a function or expression",
				 data);
	}
}

static void check_valid_int_or_function(Lisp_Object data)
{
	if (!INTP(data) && !CONSP(data) && !SYMBOLP(data))
		invalid_argument("must be an integer or expresssion", data);
}

static void check_valid_symbol(Lisp_Object data)
{
	CHECK_SYMBOL(data);
}

static void check_valid_string_or_vector(Lisp_Object data)
{
	if (!STRINGP(data) && !VECTORP(data))
		invalid_argument(":descriptor must be a string or a vector",
				 data);
}

void check_valid_item_list(Lisp_Object items)
{
	Lisp_Object rest;

	CHECK_LIST(items);
	EXTERNAL_LIST_LOOP(rest, items) {
		if (STRINGP(XCAR(rest)))
			CHECK_STRING(XCAR(rest));
		else if (VECTORP(XCAR(rest)))
			gui_parse_item_keywords(XCAR(rest));
		else if (LISTP(XCAR(rest)))
			check_valid_item_list(XCAR(rest));
		else
			invalid_argument
			    ("Items must be vectors, lists or strings", items);
	}
}

static void check_valid_instantiator_list(Lisp_Object data)
{
	Lisp_Object rest;

	CHECK_LIST(data);
	EXTERNAL_LIST_LOOP(rest, data) {
		check_valid_instantiator(XCAR(rest));
	}
}

static Lisp_Object glyph_instantiator_to_glyph(Lisp_Object sym)
{
	/* This function calls lisp. */
	Lisp_Object glyph = sym;
	struct gcpro gcpro1;

	GCPRO1(glyph);
	/* if we have a symbol get at the actual data */
	if (SYMBOLP(glyph))
		glyph = XSYMBOL(glyph)->value;

	if (CONSP(glyph))
		glyph = Feval(glyph);

	/* Be really helpful to the user. */
	if (VECTORP(glyph)) {
		glyph = call1(Qmake_glyph, glyph);
	}

	/* substitute the new glyph */
	RETURN_UNGCPRO(glyph);
}

static void
substitute_keyword_value(Lisp_Object inst, Lisp_Object key, Lisp_Object val)
{
	int i;
	/* substitute the new glyph */
	for (i = 0; i < XVECTOR_LENGTH(inst); i++) {
		if (EQ(key, XVECTOR_DATA(inst)[i])) {
			XVECTOR_DATA(inst)[i + 1] = val;
			break;
		}
	}
}

/* Determine the border with of the widget. */
static int widget_border_width(Lisp_Object domain)
{
	/* #### FIXME -- need to use specifiers (Vwidget_border_width) for
	   some portion of this. */
	if (HAS_DEVMETH_P(DOMAIN_XDEVICE(domain), widget_border_width))
		return DEVMETH(DOMAIN_XDEVICE(domain), widget_border_width, ());
	else
		return DEFAULT_WIDGET_BORDER_WIDTH;
}

static int widget_instance_border_width(Lisp_Image_Instance * ii)
{
	return widget_border_width(IMAGE_INSTANCE_DOMAIN(ii));
}

/* #### Its not clear to me what the value of logical_unit_height should
   be, or whether it should even depend on the current
   image_instance. It really should probably only depend on the
   default widget face and the domain, however you can envisage users
   wanting different logical units for nested layouts - so using the
   properties of the current lahyout is probably not so dumb. */
static int
logical_unit_height(Lisp_Object text, Lisp_Object face, Lisp_Object domain)
{
	int charheight = 0;
	query_string_geometry(text, face, 0, &charheight, 0, domain);
	/* For the returned value to be useful it needs to be big enough to
	   accomodate the largest single-height widget. This is currently
	   the edit-field. */
	return charheight + 2 * widget_spacing(domain)
	    + 4 * widget_border_width(domain);
}

static int widget_logical_unit_height(Lisp_Image_Instance * ii)
{
	return logical_unit_height(NILP(IMAGE_INSTANCE_WIDGET_TEXT(ii)) ?
				   NILP(IMAGE_INSTANCE_NAME(ii)) ?
				   Fsymbol_name(Qwidget)
				   : IMAGE_INSTANCE_NAME(ii)
				   : IMAGE_INSTANCE_WIDGET_TEXT(ii),
				   IMAGE_INSTANCE_WIDGET_FACE(ii),
				   IMAGE_INSTANCE_DOMAIN(ii));
}

/* Wire widget property invocations to specific widgets. The problem
   we are solving here is that when instantiators get converted to
   instances they lose some type information (they just become
   subwindows or widgets for example). For widgets we need to preserve
   this type information so that we can do widget specific operations
   on the instances. This is encoded in the widget type
   field. widget_property gets invoked by decoding the primary type
   (Qwidget), <widget>_property then invokes based on the secondary
   type (Qedit_field for example). It is debatable whether we should
   wire things in this generalised way rather than treating widgets
   specially in image_instance_property. */
static Lisp_Object widget_property(Lisp_Object image_instance, Lisp_Object prop)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	struct image_instantiator_methods *meths;
#if 0				/* The usefulness of this is dubious. */
	/* first see if its a general property ... */
	if (!NILP(Fplist_member(IMAGE_INSTANCE_WIDGET_PROPS(ii), prop)))
		return Fplist_get(IMAGE_INSTANCE_WIDGET_PROPS(ii), prop, Qnil);
#endif
	/* .. then try device specific methods ... */
	meths = decode_device_ii_format(image_instance_device(image_instance),
					IMAGE_INSTANCE_WIDGET_TYPE(ii),
					ERROR_ME_NOT);
	if (meths && HAS_IIFORMAT_METH_P(meths, property))
		return IIFORMAT_METH(meths, property, (image_instance, prop));
	/* ... then format specific methods ... */
	meths = decode_device_ii_format(Qnil, IMAGE_INSTANCE_WIDGET_TYPE(ii),
					ERROR_ME_NOT);
	if (meths && HAS_IIFORMAT_METH_P(meths, property))
		return IIFORMAT_METH(meths, property, (image_instance, prop));
	/* ... then fail */
	return Qunbound;
}

/* Update the displayed properties of a widget.

   #### This has been adapted from the original set_property functions
   and thus reuses the state management of that. A better solution is
   to simply re-parse the instantiator when items need updating. This
   make comparing differences much simpler and obviates the need for a
   lot of the state variables.

   #### property is still a valid function since we have to be able to
   extract information from the actual widget.

   #### update_widget should probably be re-written to use the
   instantiator. We probably want to keep a record of the differences
   also to make this easy. We would also need a pending_instantiator
   so that changes could be delayed. */
static void widget_update(Lisp_Object image_instance, Lisp_Object instantiator)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	struct image_instantiator_methods *meths;
	struct gcpro gcpro1;

	Lisp_Object text = find_keyword_in_vector(instantiator, Q_text);
	Lisp_Object desc = find_keyword_in_vector(instantiator, Q_descriptor);
	Lisp_Object items = find_keyword_in_vector(instantiator, Q_items);
	Lisp_Object descriptor_item = Qnil;

	GCPRO1(descriptor_item);

	/* Pick up any generic properties that we might need to keep hold
	   of.
	   #### This is potentially bogus because it is changing the items
	   in place rather than in the pending items. */
	if (!NILP(text)) {
		IMAGE_INSTANCE_WIDGET_TEXT(ii) = text;
		IMAGE_INSTANCE_TEXT_CHANGED(ii) = 1;
	}

	/* Retrieve the gui item information. This is easy if we have been
	   provided with a vector, more difficult if we have just been given
	   keywords.

	   #### This is inconsistent with instantiation in that you have to
	   have the :descriptor keyword for updates in order to recognise
	   changes. */
	if (VECTORP(desc)) {
		descriptor_item = gui_parse_item_keywords_no_errors(desc);
	} else {
		/* Since we are updating the instantiator could be incomplete
		   and hence the gui item descriptor not well formed. We
		   therefore try updating and discard the results if nothing
		   changed. */
		descriptor_item = copy_gui_item(IMAGE_INSTANCE_WIDGET_ITEM(ii));
		if (!update_gui_item_keywords(descriptor_item, instantiator))
			descriptor_item = Qnil;
	}

	/* Record new items for update. *_redisplay will do the
	   rest. */
	if (!EQ(IMAGE_INSTANCE_WIDGET_TYPE(ii), Qlayout)
	    && !EQ(IMAGE_INSTANCE_WIDGET_TYPE(ii), Qnative_layout)) {
		if (!NILP(items)) {
			if (NILP(descriptor_item))
				descriptor_item =
				    IMAGE_INSTANCE_WIDGET_ITEM(ii);

			check_valid_item_list(items);
#ifdef DEBUG_WIDGET_OUTPUT
			stderr_out("items for widget %p updated\n",
				   IMAGE_INSTANCE_SUBWINDOW_ID(ii));
#endif
			/* Don't set the actual items since we might decide not to use
			   the new ones (because nothing has really changed). If we did
			   set them and didn't use them then we would get into whole
			   heaps of trouble when the old items get GC'd. */
			descriptor_item =
			    Fcons(descriptor_item,
				  parse_gui_item_tree_children(items));
		}
		/* If the descriptor was updated but not the items we need to fill
		   in the `new' items. */
		else if (!NILP(descriptor_item)
			 && CONSP(IMAGE_INSTANCE_WIDGET_ITEMS(ii))) {
			descriptor_item = Fcons
			    (descriptor_item,
			     copy_gui_item_tree(XCDR
						(IMAGE_INSTANCE_WIDGET_ITEMS
						 (ii))));
		}
	}

	if (!NILP(descriptor_item)) {
		IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii) = descriptor_item;
		IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED(ii) = 1;
	}

	UNGCPRO;

	/* Now try device specific methods first ... */
	meths = decode_device_ii_format(image_instance_device(image_instance),
					IMAGE_INSTANCE_WIDGET_TYPE(ii),
					ERROR_ME_NOT);
	MAYBE_IIFORMAT_METH(meths, update, (image_instance, instantiator));
	/* ... then format specific methods ... */
	meths = decode_device_ii_format(Qnil, IMAGE_INSTANCE_WIDGET_TYPE(ii),
					ERROR_ME_NOT);
	MAYBE_IIFORMAT_METH(meths, update, (image_instance, instantiator));
#if 0				/* The usefulness of this is dubious. */
	/* we didn't do any device specific properties, so shove the property in our plist. */
	IMAGE_INSTANCE_WIDGET_PROPS(ii)
	    = Fplist_put(IMAGE_INSTANCE_WIDGET_PROPS(ii), prop, val);
#endif
}

/* Like the rest of redisplay, we want widget updates to occur
   asynchronously. Thus toolkit specific methods for setting
   properties must be called by redisplay instead of by *_update. Thus
   *_update records the change and this function actually implements
   it. We want to be slightly clever about this however by supplying
   format specific functions for the updates instead of lumping them
   all into this function. Note that there is no need for format
   generic functions. This is not the same as widget_update! */
void redisplay_widget(Lisp_Object widget)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(widget);
	struct image_instantiator_methods *meths;

	if (!WIDGET_IMAGE_INSTANCEP(widget)
	    || EQ(IMAGE_INSTANCE_WIDGET_TYPE(ii), Qlayout)
	    || EQ(IMAGE_INSTANCE_WIDGET_TYPE(ii), Qnative_layout))
		return;

	/* Device-format specific methods - e.g. x_tab_control_redisplay () */
	meths = decode_device_ii_format(image_instance_device(widget),
					IMAGE_INSTANCE_WIDGET_TYPE(ii),
					ERROR_ME_NOT);
	MAYBE_IIFORMAT_METH(meths, redisplay, (widget));

	/* Device generic methods - e.g. x_redisplay_widget (). We must
	   update the widget's size as it may have been changed by the the
	   layout routines. We also do this here so that explicit resizing
	   from lisp does not result in synchronous updates. Do this last so
	   that format-specific methods have an opportunity to prevent
	   wholesale changes - e.g. rebuilding tabs. */
	MAYBE_DEVMETH(DOMAIN_XDEVICE(IMAGE_INSTANCE_DOMAIN(ii)),
		      redisplay_widget, (ii));

	/* Pick up the items we recorded earlier. */
	if (IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED(ii)) {
		IMAGE_INSTANCE_WIDGET_ITEMS(ii) =
		    IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii);
		IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii) = Qnil;
	}
}

/* Determine the spacing of the widget. */
static int widget_spacing(Lisp_Object domain)
{
	if (HAS_DEVMETH_P(DOMAIN_XDEVICE(domain), widget_spacing))
		return DEVMETH(DOMAIN_XDEVICE(domain), widget_spacing, (0));
	else
		return DEFAULT_WIDGET_SPACING;
}

/* Query for a widgets desired geometry. If no type specific method is
   provided then use the widget text to calculate sizes. */
static void
widget_query_geometry(Lisp_Object image_instance,
		      int *width, int *height,
		      enum image_instance_geometry disp, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	struct image_instantiator_methods *meths;
	Lisp_Object dynamic_width = Qnil;
	Lisp_Object dynamic_height = Qnil;

	/* First just set up what we already have. */
	if (width)
		*width = IMAGE_INSTANCE_WIDTH(ii);
	if (height)
		*height = IMAGE_INSTANCE_HEIGHT(ii);

	if (IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii)
	    || IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii)) {
		/* .. then try device specific methods ... */
		meths =
		    decode_device_ii_format(image_instance_device
					    (image_instance),
					    IMAGE_INSTANCE_WIDGET_TYPE(ii),
					    ERROR_ME_NOT);
		if (meths && HAS_IIFORMAT_METH_P(meths, query_geometry))
			IIFORMAT_METH(meths, query_geometry, (image_instance,
							      width, height,
							      disp, domain));
		else {
			/* ... then format specific methods ... */
			meths =
			    decode_device_ii_format(Qnil,
						    IMAGE_INSTANCE_WIDGET_TYPE
						    (ii), ERROR_ME_NOT);
			if (meths && HAS_IIFORMAT_METH_P(meths, query_geometry))
				IIFORMAT_METH(meths, query_geometry,
					      (image_instance, width, height,
					       disp, domain));
			else {
				int w, h;

				/* Then if we are allowed to resize the widget, make the
				   size the same as the text dimensions. */
				query_string_geometry(IMAGE_INSTANCE_WIDGET_TEXT
						      (ii),
						      IMAGE_INSTANCE_WIDGET_FACE
						      (ii), &w, &h, 0, domain);
				/* Adjust the size for borders. */
				if (width && IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii))
					*width =
					    w +
					    2 *
					    widget_instance_border_width(ii);
				if (height && IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii))
					*height =
					    h +
					    2 *
					    widget_instance_border_width(ii);
			}
		}
		/* Finish off with dynamic sizing. */
		if (!NILP(IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(ii))) {
			dynamic_width =
			    Feval(IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(ii));
			if (INTP(dynamic_width) && width )
				*width = XINT(dynamic_width);
		}
		if (!NILP(IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(ii))) {
			dynamic_height =
			    Feval(IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(ii));
			if (INTP(dynamic_height) && height)
				*height = XINT(dynamic_height);
		}
	}
}

static int
widget_layout(Lisp_Object image_instance,
	      int width, int height, int xoffset, int yoffset,
	      Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	struct image_instantiator_methods *meths;

	/* .. then try device specific methods ... */
	meths = decode_device_ii_format(image_instance_device(image_instance),
					IMAGE_INSTANCE_WIDGET_TYPE(ii),
					ERROR_ME_NOT);
	if (meths && HAS_IIFORMAT_METH_P(meths, layout))
		return IIFORMAT_METH(meths, layout, (image_instance,
						     width, height, xoffset,
						     yoffset, domain));
	else {
		/* ... then format specific methods ... */
		meths =
		    decode_device_ii_format(Qnil,
					    IMAGE_INSTANCE_WIDGET_TYPE(ii),
					    ERROR_ME_NOT);
		if (meths && HAS_IIFORMAT_METH_P(meths, layout))
			return IIFORMAT_METH(meths, layout, (image_instance,
							     width, height,
							     xoffset, yoffset,
							     domain));
	}
	return 1;
}

static void widget_validate(Lisp_Object instantiator)
{
	Lisp_Object desc = find_keyword_in_vector(instantiator, Q_descriptor);

	if (NILP(desc))
		syntax_error("Must supply :descriptor", instantiator);

	if (VECTORP(desc))
		gui_parse_item_keywords(desc);

	if (!NILP(find_keyword_in_vector(instantiator, Q_width))
	    && !NILP(find_keyword_in_vector(instantiator, Q_pixel_width)))
		syntax_error("Must supply only one of :width and :pixel-width",
			     instantiator);

	if (!NILP(find_keyword_in_vector(instantiator, Q_height))
	    && !NILP(find_keyword_in_vector(instantiator, Q_pixel_height)))
		syntax_error
		    ("Must supply only one of :height and :pixel-height",
		     instantiator);
}

static void combo_box_validate(Lisp_Object instantiator)
{
	widget_validate(instantiator);
	if (NILP(find_keyword_in_vector(instantiator, Q_items)))
		syntax_error("Must supply item list", instantiator);
}

/* we need to convert things like glyphs to images, eval expressions
   etc.*/
static Lisp_Object
widget_normalize(Lisp_Object inst, Lisp_Object console_type,
		 Lisp_Object dest_mask)
{
	/* This function can call lisp */
	Lisp_Object glyph = find_keyword_in_vector(inst, Q_image);

	/* we need to eval glyph if its an expression, we do this for the
	   same reasons we normalize file to data.

	   #### should just normalize the data. */
	if (!NILP(glyph)) {
		substitute_keyword_value(inst, Q_image,
					 glyph_instantiator_to_glyph(glyph));
	}

	return inst;
}

static void
initialize_widget_image_instance(Lisp_Image_Instance * ii, Lisp_Object type)
{
	/*  initialize_subwindow_image_instance (ii); */
	IMAGE_INSTANCE_WIDGET_TYPE(ii) = type;
	IMAGE_INSTANCE_WIDGET_PROPS(ii) = Qnil;
	SET_IMAGE_INSTANCE_WIDGET_FACE(ii, Qnil);
	IMAGE_INSTANCE_WIDGET_ITEMS(ii) = allocate_gui_item();
	IMAGE_INSTANCE_LAYOUT_CHILDREN(ii) = Qnil;
	IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii) = Qnil;
	IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(ii) = Qnil;
	IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(ii) = Qnil;
	IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii) = 1;
	IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii) = 1;
	IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) = LAYOUT_HORIZONTAL;
	IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(ii) = 0;
	IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(ii) = 0;
}

/* Instantiate a button widget. Unfortunately instantiated widgets are
   particular to a frame since they need to have a parent. It's not
   like images where you just select the image into the context you
   want to display it in and BitBlt it. So image instances can have a
   many-to-one relationship with things you see, whereas widgets can
   only be one-to-one (i.e. per frame) */
void
widget_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, Lisp_Object domain)
{
	/* #### practically all of this should be moved to widget_update()
	   so that users can dynamically change all possible widget
	   properties. */
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object face = find_keyword_in_vector(instantiator, Q_face);
	Lisp_Object height = find_keyword_in_vector(instantiator, Q_height);
	Lisp_Object width = find_keyword_in_vector(instantiator, Q_width);
	Lisp_Object pixwidth =
	    find_keyword_in_vector(instantiator, Q_pixel_width);
	Lisp_Object pixheight =
	    find_keyword_in_vector(instantiator, Q_pixel_height);
	Lisp_Object desc = find_keyword_in_vector(instantiator, Q_descriptor);
	Lisp_Object glyph = find_keyword_in_vector(instantiator, Q_image);
	Lisp_Object items = find_keyword_in_vector(instantiator, Q_items);
	Lisp_Object orient =
	    find_keyword_in_vector(instantiator, Q_orientation);
	Lisp_Object mwidth =
	    find_keyword_in_vector(instantiator, Q_margin_width);
	Lisp_Object ifocus =
	    find_keyword_in_vector(instantiator, Q_initial_focus);
	int pw = 0, ph = 0, tw = 0, th = 0;

	/* this just does pixel type sizing */
	subwindow_instantiate(image_instance, instantiator, pointer_fg,
			      pointer_bg, dest_mask, domain);

	if (!(dest_mask & IMAGE_WIDGET_MASK))
		incompatible_image_types(instantiator, dest_mask,
					 IMAGE_WIDGET_MASK);

	initialize_widget_image_instance(ii, XVECTOR_DATA(instantiator)[0]);

	IMAGE_INSTANCE_TYPE(ii) = IMAGE_WIDGET;

	/* retrieve the fg and bg colors */
	if (!NILP(face))
		SET_IMAGE_INSTANCE_WIDGET_FACE(ii, Fget_face(face));

	/* Retrieve the gui item information. This is easy if we have been
	   provided with a vector, more difficult if we have just been given
	   keywords. Note that standard gui descriptor shortcuts will not work
	   because of keyword parsing.

	   #### This is bogus in that descriptor and items share the same slot,
	   we should rationalize. */
	if (VECTORP(desc)) {
		IMAGE_INSTANCE_WIDGET_ITEMS(ii) =
		    gui_parse_item_keywords_no_errors(desc);
	} else {
		/* big cheat - we rely on the fact that a gui item looks like an instantiator */
		IMAGE_INSTANCE_WIDGET_ITEMS(ii) =
		    widget_gui_parse_item_keywords(instantiator);
	}

	/* Pick up the orientation before we do our first layout. */
	if (EQ(orient, Qleft) || EQ(orient, Qright) || EQ(orient, Qvertical))
		IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) = LAYOUT_VERTICAL;

	/* parse more gui items out of the properties */
	if (!NILP(items) && !EQ(IMAGE_INSTANCE_WIDGET_TYPE(ii), Qlayout)
	    && !EQ(IMAGE_INSTANCE_WIDGET_TYPE(ii), Qnative_layout)) {
		IMAGE_INSTANCE_WIDGET_ITEMS(ii) =
		    Fcons(IMAGE_INSTANCE_WIDGET_ITEMS(ii),
			  parse_gui_item_tree_children(items));
	}

	/* Normalize size information. We now only assign sizes if the user
	   gives us some explicitly, or there are some constraints that we
	   can't change later on. Otherwise we postpone sizing until query
	   geometry gets called. */
	if (!NILP(pixwidth)) {	/* pixwidth takes precendent */
		if (!INTP(pixwidth))
			IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(ii) = pixwidth;
		else {
			pw = XINT(pixwidth);
			IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii) = 0;
		}
	} else if (!NILP(width)) {
		tw = XINT(width);
		IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii) = 0;
	}

	if (!NILP(pixheight)) {
		if (!INTP(pixheight))
			IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(ii) = pixheight;
		else {
			ph = XINT(pixheight);
			IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii) = 0;
		}
	} else if (!NILP(height) && XINT(height) > 1) {
		th = XINT(height);
		IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii) = 0;
	}

	/* Taking the default face information when the user has specified
	   size in characters is probably as good as any since the widget
	   face is more likely to be proportional and thus give inadequate
	   results. Using character sizes can only ever be approximate
	   anyway. :height is measured in logical characters which take into
	   account the borders and spacing on widgets. */
	if (tw) {
		int charwidth;
		default_face_font_info(domain, 0, 0, 0, &charwidth, 0);
		pw = ROUND_UP(charwidth * tw +
			      4 * widget_instance_border_width(ii), charwidth);
	}

	/* For heights the widget face is more appropriate. */
	if (th == 1) {
		int charheight;
		if (!NILP(IMAGE_INSTANCE_WIDGET_TEXT(ii))) {
			query_string_geometry(IMAGE_INSTANCE_WIDGET_TEXT(ii),
					      IMAGE_INSTANCE_WIDGET_FACE(ii),
					      0, &charheight, 0, domain);
		} else {
			default_face_font_info(domain, 0, 0, &charheight, 0, 0);
		}
		ph = (charheight + 2 * widget_instance_border_width(ii)) * th;
	}
	/* For heights > 1 use logical units. */
	else if (th > 1) {
		ph = widget_logical_unit_height(ii) * th;
	}

	/* for a widget with an image pick up the dimensions from that */
	if (!NILP(glyph)) {
		if (!pw)
			pw = glyph_width(glyph,
					 image_instance) +
			    2 * widget_instance_border_width(ii);
		if (!ph)
			ph = glyph_height(glyph,
					  image_instance) +
			    2 * widget_instance_border_width(ii);
		IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii) = 0;
		IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii) = 0;
	}

	/* Pick up the margin width. */
	if (!NILP(mwidth))
		IMAGE_INSTANCE_MARGIN_WIDTH(ii) = XINT(mwidth);

	IMAGE_INSTANCE_WANTS_INITIAL_FOCUS(ii) = !NILP(ifocus);

	/* Layout for the layout widget is premature at this point since the
	   children will not have been instantiated. We can't instantiate
	   them until the device instantiation method for the layout has
	   been executed. We do however want to record any specified
	   dimensions. */
	if (pw)
		IMAGE_INSTANCE_WIDTH(ii) = pw;
	if (ph)
		IMAGE_INSTANCE_HEIGHT(ii) = ph;
}

static void
widget_post_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object domain)
{
#ifdef DEBUG_WIDGETS
	debug_widget_instances++;
	stderr_out("instantiated ");
	debug_print(instantiator);
	stderr_out("%d widgets instantiated\n", debug_widget_instances);
#endif
}

/* Get the geometry of a button control. We need to adjust the size
   depending on the type of button. */
static void
button_query_geometry(Lisp_Object image_instance,
		      int *width, int *height,
		      enum image_instance_geometry disp, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	int w, h;
	query_string_geometry(IMAGE_INSTANCE_WIDGET_TEXT(ii),
			      IMAGE_INSTANCE_WIDGET_FACE(ii),
			      &w, &h, 0, domain);
	/* Adjust the size for borders. */
	if (IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii)) {
		*width = w + 3 * widget_instance_border_width(ii);

		if (EQ(XGUI_ITEM(IMAGE_INSTANCE_WIDGET_ITEM(ii))->style, Qradio)
		    ||
		    EQ(XGUI_ITEM(IMAGE_INSTANCE_WIDGET_ITEM(ii))->style,
		       Qtoggle))
			/* This is an approximation to the size of the actual button bit. */
			*width += 12;
	}
	if (IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii))
		*height = h + 3 * widget_instance_border_width(ii);
}

/* Get the geometry of an edit field. */
static void
edit_field_query_geometry(Lisp_Object image_instance,
			  int *width, int *height,
			  enum image_instance_geometry disp, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	int w, h;
	query_string_geometry(IMAGE_INSTANCE_WIDGET_TEXT(ii),
			      IMAGE_INSTANCE_WIDGET_FACE(ii),
			      &w, &h, 0, domain);
	/* Adjust the size for borders. */
	if (IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii))
		*width = w + 4 * widget_instance_border_width(ii);
	if (IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii))
		*height = h + 4 * widget_instance_border_width(ii);
}

/* tree-view geometry - get the height right */
static void
tree_view_query_geometry(Lisp_Object image_instance,
			 int *width, int *height,
			 enum image_instance_geometry disp, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object items = IMAGE_INSTANCE_WIDGET_ITEMS(ii);

	if (*width) {
		/* #### what should this be. reconsider when X has tree views. */
		query_string_geometry(IMAGE_INSTANCE_WIDGET_TEXT(ii),
				      IMAGE_INSTANCE_WIDGET_FACE(ii),
				      width, 0, 0, domain);
	}
	if (*height) {
		int len, h;
		/* #### widget face would be better here. */
		default_face_font_info(domain, 0, 0, &h, 0, 0);
		GET_LIST_LENGTH(items, len);
		*height = len * h;
	}
}

/* Get the geometry of a tab control. This is based on the number of
   items and text therin in the tab control. */
static void
tab_control_query_geometry(Lisp_Object image_instance,
			   int *width, int *height,
			   enum image_instance_geometry disp,
			   Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object items = XCDR(IMAGE_INSTANCE_WIDGET_ITEMS(ii));
	Lisp_Object rest;
	int tw = 0, th = 0;

	LIST_LOOP(rest, items) {
		int h, w;

		query_string_geometry(XGUI_ITEM(XCAR(rest))->name,
				      IMAGE_INSTANCE_WIDGET_FACE(ii),
				      &w, &h, 0, domain);
		tw += 5 * widget_instance_border_width(ii);	/* some bias */
		tw += w;
		th = max(th, h + 2 * widget_instance_border_width(ii));
	}

	/* Fixup returned values depending on orientation. */
	if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii)) {
		if (height)
			*height = tw;
		if (width)
			*width = th;
	} else {
		if (height)
			*height = th;
		if (width)
			*width = tw;
	}
}

/* Determine whether only the order has changed for a tab. */
int tab_control_order_only_changed(Lisp_Object image_instance)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	int found = 0, len, pending_len;
	Lisp_Object rest;

	/* Degenerate case. */
	if (NILP(IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii)))
		return 1;

	/* See whether we just need a change in order. */
	GET_LIST_LENGTH(IMAGE_INSTANCE_WIDGET_ITEMS(ii), len);
	GET_LIST_LENGTH(IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii), pending_len);
	if (len == pending_len) {
		LIST_LOOP(rest, XCDR(IMAGE_INSTANCE_WIDGET_ITEMS(ii))) {
			Lisp_Object pending_rest;
			found = 0;
			LIST_LOOP(pending_rest,
				  XCDR(IMAGE_INSTANCE_WIDGET_PENDING_ITEMS(ii)))
			{
				if (gui_item_equal_sans_selected(XCAR(rest),
								 XCAR
								 (pending_rest),
								 0)) {
					found = 1;
					break;
				}
			}
			if (!found)
				break;
		}
	}
	return found;
}

/*****************************************************************************
 *                              widget layout                               *
 *****************************************************************************/
/* We need to cascade normalization.*/
static Lisp_Object
layout_normalize(Lisp_Object inst, Lisp_Object console_type,
		 Lisp_Object dest_mask)
{
	/* This function can call lisp */
	struct gcpro gcpro1, gcpro2;
	Lisp_Object alist = Qnil, new_items = Qnil, border;
	/* This function can call lisp */
	Lisp_Object items;

	GCPRO2(alist, new_items);
	alist = tagged_vector_to_alist(inst);
	items = assq_no_quit(Q_items, alist);

	/* We need to normalize sub-objects. */
	if (!NILP(items)) {
		Lisp_Object rest;
		LIST_LOOP(rest, XCDR(items)) {
			/* Substitute the new instantiator */
			new_items =
			    Fcons(normalize_image_instantiator
				  (XCAR(rest), console_type, dest_mask),
				  new_items);
		}
		new_items = Fnreverse(new_items);
		Fsetcdr(items, new_items);
	}
	/* Normalize the border spec. */
	border = assq_no_quit(Q_border, alist);
	if (!NILP(border) && VECTORP(XCDR(border))) {
		Fsetcdr(border, normalize_image_instantiator(XCDR(border),
							     console_type,
							     dest_mask));
	}

	{
		Lisp_Object result =
		    alist_to_tagged_vector(XVECTOR_DATA(inst)[0],
					   alist);
		free_alist(alist);
		RETURN_UNGCPRO(result);
	}
}

/* Update the instances in the layout. */
static void layout_update(Lisp_Object image_instance, Lisp_Object instantiator)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object items = find_keyword_in_vector(instantiator, Q_items);
	Lisp_Object border_inst =
	    find_keyword_in_vector(instantiator, Q_border);
	Lisp_Object justify = find_keyword_in_vector(instantiator, Q_justify);
	Lisp_Object hjustify =
	    find_keyword_in_vector(instantiator, Q_horizontally_justify);
	Lisp_Object vjustify =
	    find_keyword_in_vector(instantiator, Q_vertically_justify);
	Lisp_Object border = Qnil;
	Lisp_Object children = IMAGE_INSTANCE_LAYOUT_CHILDREN(ii);
	int structure_changed = 0;
	struct gcpro gcpro1;

	/* Pick up horizontal justification, left is the default. */
	if (!NILP(hjustify)) {
		if (EQ(hjustify, Qright) || EQ(hjustify, Qbottom))
			IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_RIGHT;
		else if (EQ(hjustify, Qcenter))
			IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_CENTER;
	}
	/* If not set use general justification. */
	else if (!NILP(justify)) {
		if (EQ(justify, Qright) || EQ(justify, Qbottom))
			IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_RIGHT;
		else if (EQ(justify, Qcenter))
			IMAGE_INSTANCE_SUBWINDOW_H_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_CENTER;
	}

	/* Pick up vertical justification, top is the default. */
	if (!NILP(vjustify)) {
		if (EQ(vjustify, Qright) || EQ(vjustify, Qbottom))
			IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_BOTTOM;
		else if (EQ(vjustify, Qcenter))
			IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_CENTER;
	}
	/* If not set use general justification. */
	else if (!NILP(justify)) {
		if (EQ(justify, Qright) || EQ(justify, Qbottom))
			IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_BOTTOM;
		else if (EQ(justify, Qcenter))
			IMAGE_INSTANCE_SUBWINDOW_V_JUSTIFY(ii) =
			    LAYOUT_JUSTIFY_CENTER;
	}

	/* We want to avoid consing if we can. This is quite awkward because
	   we have to deal with the border as well as the items. */
	GCPRO1(border);

	if (INTP(IMAGE_INSTANCE_LAYOUT_BORDER(ii))) {
		border = XCAR(children);
		children = XCDR(children);
	}
#ifdef DEBUG_WIDGET_OUTPUT
	stderr_out("layout updated\n");
#endif
	/* Update the border. */
	if (!NILP(border_inst)) {
		if (VECTORP(border_inst)) {
			/* We are going to be sneaky here and add the border text as
			   just another child, the layout and output routines don't know
			   this and will just display at the offsets we prescribe. */
			if (!NILP(border))
				call3(Qset_glyph_image, border, border_inst,
				      IMAGE_INSTANCE_DOMAIN(ii));
			else {
				border =
				    Fcons(call1(Qmake_glyph, border_inst),
					  Qnil);
				structure_changed = 1;
			}
			IMAGE_INSTANCE_LAYOUT_BORDER(ii) = make_int(0);
		} else {
			if (!NILP(border)) {
				border = Qnil;
				structure_changed = 1;
			}
			if (EQ(border_inst, Qt))
				IMAGE_INSTANCE_LAYOUT_BORDER(ii) = Qetched_in;
			else
				IMAGE_INSTANCE_LAYOUT_BORDER(ii) = border_inst;
		}
	}

	/* Pick up the sub-widgets. */
	if (!NILP(items)) {
		int len1, len2;
		GET_LIST_LENGTH(items, len1);
		GET_LIST_LENGTH(children, len2);
		/* The structure hasn't changed so just update the images. */
		if (!structure_changed && len1 == len2) {
			/* Pick up the sub-widgets. */
			for (; !NILP(children);
			     children = XCDR(children), items = XCDR(items)) {
				call3(Qset_glyph_image, XCAR(children),
				      XCAR(items), IMAGE_INSTANCE_DOMAIN(ii));
			}
		}
		/* The structure has changed so start over. */
		else {
			/* Instantiate any new glyphs. */
			for (; !NILP(items); items = XCDR(items)) {
				/* #### We really want to use call_with_suspended_errors
				   here, but it won't allow us to call lisp. */
				border =
				    Fcons(call1(Qmake_glyph, XCAR(items)),
					  border);
			}
			IMAGE_INSTANCE_LAYOUT_CHILDREN(ii) = Fnreverse(border);
		}
	}
	UNGCPRO;
}

static void
layout_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object orient =
	    find_keyword_in_vector(instantiator, Q_orientation);

#ifdef DEBUG_WIDGET_OUTPUT
	stderr_out("layout instantiated\n");
#endif
	/* Do widget type instantiation first. */
	widget_instantiate(image_instance, instantiator, pointer_fg, pointer_bg,
			   dest_mask, domain);

	if (NILP(orient)) {
		IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) = LAYOUT_VERTICAL;
	}

	/* Get child glyphs and finish instantiation. We can't do image
	   instance children yet as we might not have a containing
	   window. */
	layout_update(image_instance, instantiator);
}

static void
layout_post_instantiate(Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object domain)
{
}

/* Layout widget. Sizing commentary: we have a number of problems that
   we would like to address. Some consider some of these more
   important than others. It used to be that size information was
   determined at instantiation time and was then fixed forever
   after. Generally this is not what we want. Users want size to be
   "big enough" to accommodate whatever they are trying to show and
   this is dependent on text length, lines, font metrics etc. Of
   course these attributes can change dynamically and so the size
   should changed dynamically also. Only in a few limited cases should
   the size be fixed and remain fixed. Of course this actually means
   that we don't really want to specify the size *at all* for most
   widgets - we want it to be discovered dynamically. Thus we can
   envisage the following scenarios:

   1. A button is sized to accommodate its text, the text changes and the
   button should change size also.

   2. A button is given an explicit size. Its size should never change.

   3. Layout is put inside an area. The size of the area changes, the
   layout should change with it.

   4. A button grows to accommodate additional text. The whitespace
   around it should be modified to cope with the new layout
   requirements.

   5. A button grows. The area surrounding it should grow also if
   possible.

   What metrics are important?
   1. Actual width and height.

   2. Whether the width and height are what the widget actually wants, or
   whether it can grow or shrink.

   Text glyphs are particularly troublesome since their metrics depend
   on the context in which they are being viewed. For instance they
   can appear differently depending on the window face, frame face or
   glyph face. In order to simplify this text glyphs can now only have
   a glyph-face or image-instance face. All other glyphs are
   essentially fixed in appearance. Perhaps the problem is that text
   glyphs are cached on a device basis like most other glyphs. Instead
   they should be cached per-window and then the instance would be
   fixed and we wouldn't have to mess around with font metrics and the
   rest.

   Another sizing problem is alignment. We provide layout widgets that
   allow users to stack widgets vertically or horizontally. These
   layouts also allow the widgets to be centered (space evenly
   distributed), left or right justified (fixed spacing widgets
   stacked against the left, righ, top or bottom edge). Unfortunately
   this doesn't allow widgets in different layouts to be aligned. For
   instance how should the search dialog be organized for alignment?
   The obvious choice of two vertical columns does not work since the
   size of individual widgets will affect where they get placed. The
   same is true for several rows of widgets. To solve this problem we
   introduce the notion of `logical_unit_height'. This is a size
   quantity that is designed to be big enough to accomodate the
   largest `single height unit'. The function
   widget_logical_unit_height() determines the value of this in
   pixels. It is dependent on the widget face and some combination of
   spacing and border-width. Thus if users specify left or right
   justification in a vertical layout they get something in logical
   units. To simplify this the functions
   `widget-logical-to-character-height' and
   `widget-logical-to-character-width' allow conversion between
   characters and logical units so that frames can be sized
   appropriately. */

/* Query the geometry of a layout widget. */
static void
layout_query_geometry(Lisp_Object image_instance, int *width,
		      int *height, enum image_instance_geometry disp,
		      Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object items = IMAGE_INSTANCE_LAYOUT_CHILDREN(ii), rest;
	int maxph = 0, maxpw = 0, nitems = 0, ph_adjust = 0;
	int gheight, gwidth, luh;

	/* If we are not initialized then we won't have any children. */
	if (!IMAGE_INSTANCE_INITIALIZED(ii))
		return;

	/* First just set up what we already have. */
	if (width)
		*width = IMAGE_INSTANCE_WIDTH(ii);
	if (height)
		*height = IMAGE_INSTANCE_HEIGHT(ii);

	/* If we are not allowed to dynamically size then return. */
	if (!IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii)
	    && !IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii))
		return;

	luh = widget_logical_unit_height(ii);

	/* Pick up the border text if we have one. */
	if (INTP(IMAGE_INSTANCE_LAYOUT_BORDER(ii))) {
		glyph_query_geometry(XCAR(items), &gwidth, &gheight, disp,
				     image_instance);
		ph_adjust = gheight;
		/* Include text width in vertical layouts. */
		if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) == LAYOUT_VERTICAL)
			maxpw = gwidth + BORDER_FIDDLE_FACTOR;
		items = XCDR(items);
	}

	/* Flip through the items to work out how much stuff we have to display */
	LIST_LOOP(rest, items) {
		Lisp_Object glyph = XCAR(rest);
		glyph_query_geometry(glyph, &gwidth, &gheight, disp,
				     image_instance);

		nitems++;
		if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) == LAYOUT_HORIZONTAL) {
			maxph = max(maxph, gheight);
			maxpw += gwidth;
		} else {
			maxpw = max(maxpw, gwidth);
			maxph += gheight;
		}
	}

	/* Work out minimum space we need to fit all the items. This could
	   have been fixed by the user. */
	if (width && IMAGE_INSTANCE_SUBWINDOW_H_RESIZEP(ii)) {
		if (!NILP(IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(ii))) {
			Lisp_Object dynamic_width =
			    Feval(IMAGE_INSTANCE_WIDGET_WIDTH_SUBR(ii));
			if (INTP(dynamic_width))
				*width = XINT(dynamic_width);
		} else if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) ==
			   LAYOUT_HORIZONTAL) {
			*width =
			    maxpw +
			    ((nitems + 1) * widget_instance_border_width(ii) +
			     IMAGE_INSTANCE_MARGIN_WIDTH(ii)) * 2;
		} else {
			*width =
			    maxpw + 2 * (widget_instance_border_width(ii) * 2 +
					 IMAGE_INSTANCE_MARGIN_WIDTH(ii));
		}
	}

	/* Work out vertical spacings. */
	if (height && IMAGE_INSTANCE_SUBWINDOW_V_RESIZEP(ii)) {
		if (!NILP(IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(ii))) {
			Lisp_Object dynamic_height =
			    Feval(IMAGE_INSTANCE_WIDGET_HEIGHT_SUBR(ii));
			if (INTP(dynamic_height))
				*height = XINT(dynamic_height);
		} else if (IMAGE_INSTANCE_SUBWINDOW_LOGICAL_LAYOUT(ii)) {
			*height = nitems * luh + ph_adjust;
		} else if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) ==
			   LAYOUT_VERTICAL) {
			*height =
			    maxph +
			    ((nitems + 1) * widget_instance_border_width(ii) +
			     IMAGE_INSTANCE_MARGIN_WIDTH(ii)) * 2 + ph_adjust;
		} else {
			*height =
			    maxph + (2 * widget_instance_border_width(ii) +
				     IMAGE_INSTANCE_MARGIN_WIDTH(ii)) * 2 +
			    ph_adjust;
		}
	}
#ifdef DEBUG_WIDGET_OUTPUT
	stderr_out("layout wants %dx%d\n", *width, *height);
#endif
}

int
layout_layout(Lisp_Object image_instance,
	      int width, int height, int xoffset, int yoffset,
	      Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object rest;
	Lisp_Object items = IMAGE_INSTANCE_LAYOUT_CHILDREN(ii);
	int x, y, maxph = 0, maxpw = 0, nitems = 0,
	    horiz_spacing, vert_spacing, ph_adjust = 0;
	int gheight, gwidth;
	/* See comments in widget_logical_unit_height(). */
	int luh = widget_logical_unit_height(ii);

	/* If we are not initialized then we won't have any children. */
	if (!IMAGE_INSTANCE_INITIALIZED(ii))
		return 0;

#ifdef DEBUG_WIDGET_OUTPUT
	stderr_out("layout output %dx%d\n", width, height);
#endif

	/* Pick up the border text if we have one. A border can have the
	   values Qetched_in, Qetched_out, Qbevel_in, Qbevel_out or an
	   integer. The first four just affect the display properties of the
	   border that is drawn. The last is an offset and implies that the
	   first item in the list of subcontrols is a text control that
	   should be displayed on the border. */
	if (INTP(IMAGE_INSTANCE_LAYOUT_BORDER(ii))) {
		Lisp_Object border = XCAR(items);
		items = XCDR(items);
		glyph_query_geometry(border, &gwidth, &gheight,
				     IMAGE_DESIRED_GEOMETRY, image_instance);
		/* The vertical offset for subsequent items is the full height
		   of the border glyph. */
		ph_adjust = gheight;
		/* The offset for the border is half the glyph height. */
		IMAGE_INSTANCE_LAYOUT_BORDER(ii) = make_int(gheight / 2);

		/* #### Really, what should this be? */
		glyph_do_layout(border, gwidth, gheight, BORDER_FIDDLE_FACTOR,
				0, image_instance);
	}

	/* Flip through the items to work out how much stuff we have to display. */
	LIST_LOOP(rest, items) {
		Lisp_Object glyph = XCAR(rest);

		glyph_query_geometry(glyph, &gwidth, &gheight,
				     IMAGE_DESIRED_GEOMETRY, image_instance);
		nitems++;
		if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii)
		    == LAYOUT_HORIZONTAL) {
			maxph = max(maxph, gheight);
			maxpw += gwidth;
		} else {
			maxpw = max(maxpw, gwidth);
			maxph += gheight;
		}
	}

	/* work out spacing between items and bounds of the layout */
	if (width < maxpw)
		/* The user wants a smaller space than the largest item, so we
		   just provide default spacing and will let the output routines
		   clip. */
		horiz_spacing = widget_spacing(IMAGE_INSTANCE_DOMAIN(ii));
	else if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii)
		 == LAYOUT_HORIZONTAL)
		/* We have a larger area to display in so distribute the space
		   evenly. */
		horiz_spacing = (width - (maxpw +
					  IMAGE_INSTANCE_MARGIN_WIDTH(ii) * 2))
		    / (nitems + 1);
	else
		horiz_spacing = (width - maxpw) / 2
		    - IMAGE_INSTANCE_MARGIN_WIDTH(ii);

	/* We are trying here to get widgets to line up when they are left
	   or right justified vertically. This means that we must position
	   widgets on logical unit boundaries, even though their height may
	   be greater or less than a logical unit. In order to avoid
	   clipping we need to determine how big the widget wants to be and
	   then allocate as many logical units as necessary in order to
	   accommodate it. */
	if (height < maxph)
		vert_spacing = widget_spacing(IMAGE_INSTANCE_DOMAIN(ii)) * 2;
	else if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii)
		 == LAYOUT_VERTICAL) {
		if (!IMAGE_INSTANCE_SUBWINDOW_V_CENTERED(ii))
			vert_spacing =
			    widget_spacing(IMAGE_INSTANCE_DOMAIN(ii)) * 2;
		else
			vert_spacing = (height - (maxph + ph_adjust +
						  IMAGE_INSTANCE_MARGIN_WIDTH
						  (ii) * 2))
			    / (nitems + 1);
	} else
		vert_spacing = (height - (maxph + ph_adjust)) / 2
		    - IMAGE_INSTANCE_MARGIN_WIDTH(ii);

	y = yoffset =
	    vert_spacing + ph_adjust + IMAGE_INSTANCE_MARGIN_WIDTH(ii);
	x = horiz_spacing + IMAGE_INSTANCE_MARGIN_WIDTH(ii);

	/* Now flip through putting items where we want them, paying
	   attention to justification. Make sure we don't mess with the
	   border glyph. */
	LIST_LOOP(rest, items) {
		Lisp_Object glyph = XCAR(rest);

		glyph_query_geometry(glyph, &gwidth, &gheight,
				     IMAGE_DESIRED_GEOMETRY, image_instance);

		if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) == LAYOUT_HORIZONTAL) {
			if (IMAGE_INSTANCE_SUBWINDOW_BOTTOM_JUSTIFIED(ii))
				y = height - (gheight + vert_spacing);
			else if (IMAGE_INSTANCE_SUBWINDOW_V_CENTERED(ii))
				y = (height - gheight) / 2;
		} else {
			if (IMAGE_INSTANCE_SUBWINDOW_RIGHT_JUSTIFIED(ii))
				x = width - (gwidth + horiz_spacing);
			else if (IMAGE_INSTANCE_SUBWINDOW_H_CENTERED(ii))
				x = (width - gwidth) / 2;
		}

		/* Now layout subwidgets if they require it. */
		glyph_do_layout(glyph, gwidth, gheight, x, y, image_instance);

		if (IMAGE_INSTANCE_SUBWINDOW_ORIENT(ii) == LAYOUT_HORIZONTAL) {
			x += (gwidth + horiz_spacing);
		} else {
			y += (gheight + vert_spacing);
			if (!IMAGE_INSTANCE_SUBWINDOW_V_CENTERED(ii)) {
				/* justified, vertical layout, try and align on logical unit
				   boundaries. */
				y = ROUND_UP(y - yoffset, luh) + yoffset;
			}
		}

	}
	return 1;
}

/* Get the glyphs that comprise a layout. These are created internally
   and so are otherwise inaccessible to lisp. We need some way of getting
   properties from the widgets that comprise a layout and this is the
   simplest way of doing it.

   #### Eventually we should allow some more intelligent access to
   sub-widgets. */
static Lisp_Object layout_property(Lisp_Object image_instance, Lisp_Object prop)
{
	/* This function can GC. */
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	if (EQ(prop, Q_items)) {
		if (INTP(IMAGE_INSTANCE_LAYOUT_BORDER(ii)) &&
		    CONSP(IMAGE_INSTANCE_LAYOUT_CHILDREN(ii)))
			return Fcopy_sequence(XCDR
					      (IMAGE_INSTANCE_LAYOUT_CHILDREN
					       (ii)));
		else
			return
			    Fcopy_sequence(IMAGE_INSTANCE_LAYOUT_CHILDREN(ii));
	}
	return Qunbound;
}

/* Layout subwindows if they are real subwindows. */
static int
native_layout_layout(Lisp_Object image_instance,
		     int width, int height, int xoffset, int yoffset,
		     Lisp_Object domain)
{
	Lisp_Image_Instance *ii = XIMAGE_INSTANCE(image_instance);
	Lisp_Object rest;

	/* The first time this gets called, the layout will be only
	   partially instantiated. The children get done in
	   post_instantiate. */
	if (!IMAGE_INSTANCE_INITIALIZED(ii))
		return 0;

	/* Defining this overrides the default layout_layout so we first have to call that to get
	   suitable instances and values set up. */
	layout_layout(image_instance, width, height, xoffset, yoffset, domain);

	LIST_LOOP(rest, IMAGE_INSTANCE_LAYOUT_CHILDREN(ii)) {
		struct display_glyph_area dga;
		dga.xoffset = 0;
		dga.yoffset = 0;
		dga.width = IMAGE_INSTANCE_WIDTH(ii);
		dga.height = IMAGE_INSTANCE_HEIGHT(ii);

		map_subwindow(XCAR(rest),
			      IMAGE_INSTANCE_XOFFSET(ii),
			      IMAGE_INSTANCE_YOFFSET(ii), &dga);
	}
	return 1;
}

DEFUN("widget-logical-to-character-width", Fwidget_logical_to_character_width, 1, 3, 0,	/*
Convert the width in logical widget units to characters.
Logical widget units do not take into account adjusments made for
layout borders, so this adjusment is approximated.
*/
      (width, face, domain))
{
	int w, neww, charwidth;
	int border_width = DEFAULT_WIDGET_BORDER_WIDTH;

	if (NILP(domain))
		domain = Fselected_frame(Qnil);

	CHECK_INT(width);
	w = XINT(width);

	if (HAS_DEVMETH_P(DOMAIN_XDEVICE(domain), widget_border_width))
		border_width =
		    DEVMETH(DOMAIN_XDEVICE(domain), widget_border_width, ());

	default_face_font_info(domain, 0, 0, 0, &charwidth, 0);
	neww =
	    ROUND_UP(charwidth * w + 4 * border_width +
		     2 * widget_spacing(domain), charwidth) / charwidth;

	return make_int(neww);
}

DEFUN("widget-logical-to-character-height", Fwidget_logical_to_character_height, 1, 3, 0,	/*
Convert the height in logical widget units to characters.
Logical widget units do not take into account adjusments made for
layout borders, so this adjustment is approximated.

If the components of a widget layout are justified to the top or the
bottom then they are aligned in terms of `logical units'. This is a
size quantity that is designed to be big enough to accomodate the
largest `single height' widget. It is dependent on the widget face and
some combination of spacing and border-width. Thus if you specify top
or bottom justification in a vertical layout the subcontrols are laid
out one per logical unit. This allows adjoining layouts to have
identical alignment for their subcontrols.

Since frame sizes are measured in characters, this function allows you
to do appropriate conversion between logical units and characters.
*/
      (height, face, domain))
{
	int h, newh, charheight;

	CHECK_INT(height);
	if (NILP(domain))
		domain = Fselected_frame(Qnil);

	h = XINT(height);

	default_face_font_info(domain, 0, 0, &charheight, 0, 0);
	newh = ROUND_UP(logical_unit_height(Fsymbol_name(Qwidget),
					    Vwidget_face, domain) * h,
			charheight)
	    / charheight;

	return make_int(newh);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_glyphs_widget(void)
{
	DEFSYMBOL(Qetched_in);
	DEFSYMBOL(Qetched_out);
	DEFSYMBOL(Qbevel_in);
	DEFSYMBOL(Qbevel_out);
	DEFSYMBOL(Qmake_glyph);

	DEFSUBR(Fwidget_logical_to_character_height);
	DEFSUBR(Fwidget_logical_to_character_width);
}

#define VALID_GUI_KEYWORDS(type) do {					      \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_active, check_valid_anything);      \
  IIFORMAT_VALID_KEYWORD (type, Q_suffix, check_valid_anything);	      \
  IIFORMAT_VALID_KEYWORD (type, Q_keys, check_valid_string);		      \
  IIFORMAT_VALID_KEYWORD (type, Q_style, check_valid_symbol);		      \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_selected, check_valid_anything);    \
  IIFORMAT_VALID_KEYWORD (type, Q_filter, check_valid_anything);	      \
  IIFORMAT_VALID_KEYWORD (type, Q_config, check_valid_symbol);		      \
  IIFORMAT_VALID_KEYWORD (type, Q_included, check_valid_anything);	      \
  IIFORMAT_VALID_KEYWORD (type, Q_initial_focus, check_valid_anything);	      \
  IIFORMAT_VALID_KEYWORD (type, Q_key_sequence, check_valid_string);	      \
  IIFORMAT_VALID_KEYWORD (type, Q_accelerator, check_valid_string);	      \
  IIFORMAT_VALID_KEYWORD (type, Q_label, check_valid_anything);		      \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_callback, check_valid_callback);    \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_callback_ex, check_valid_callback); \
  IIFORMAT_VALID_NONCOPY_KEYWORD (type, Q_descriptor,			      \
				  check_valid_string_or_vector);	      \
} while (0)

#define VALID_WIDGET_KEYWORDS(type) do {				      \
  IIFORMAT_VALID_KEYWORD (type, Q_width, check_valid_int);		      \
  IIFORMAT_VALID_KEYWORD (type, Q_height, check_valid_int);		      \
  IIFORMAT_VALID_KEYWORD (type, Q_pixel_width, check_valid_int_or_function);  \
  IIFORMAT_VALID_KEYWORD (type, Q_pixel_height, check_valid_int_or_function); \
  IIFORMAT_VALID_KEYWORD (type, Q_face, check_valid_face);		      \
} while (0)

static void image_instantiator_widget(void)
{				/* we only do this for properties */
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT_NO_SYM(widget, "widget");
	IIFORMAT_HAS_METHOD(widget, property);
	IIFORMAT_HAS_METHOD(widget, update);
	IIFORMAT_HAS_METHOD(widget, query_geometry);
	IIFORMAT_HAS_METHOD(widget, layout);
}

static void image_instantiator_buttons(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(button, "button");
	IIFORMAT_HAS_SHARED_METHOD(button, validate, widget);
	IIFORMAT_HAS_SHARED_METHOD(button, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(button, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(button, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(button, normalize, widget);
	IIFORMAT_HAS_SHARED_METHOD(button, governing_domain, subwindow);
	IIFORMAT_HAS_METHOD(button, query_geometry);
	IIFORMAT_VALID_KEYWORD(button, Q_image, check_valid_instantiator);
	VALID_WIDGET_KEYWORDS(button);
	VALID_GUI_KEYWORDS(button);
}

static void image_instantiator_edit_fields(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(edit_field, "edit-field");
	IIFORMAT_HAS_SHARED_METHOD(edit_field, validate, widget);
	IIFORMAT_HAS_SHARED_METHOD(edit_field, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(edit_field, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(edit_field, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(edit_field, governing_domain, subwindow);
	IIFORMAT_HAS_METHOD(edit_field, query_geometry);
	VALID_WIDGET_KEYWORDS(edit_field);
	VALID_GUI_KEYWORDS(edit_field);
}

static void image_instantiator_combo_box(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(combo_box, "combo-box");
	IIFORMAT_HAS_METHOD(combo_box, validate);
	IIFORMAT_HAS_SHARED_METHOD(combo_box, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(combo_box, governing_domain, subwindow);

	VALID_GUI_KEYWORDS(combo_box);

	IIFORMAT_VALID_KEYWORD(combo_box, Q_width, check_valid_int);
	IIFORMAT_VALID_KEYWORD(combo_box, Q_height, check_valid_int);
	IIFORMAT_VALID_KEYWORD(combo_box, Q_pixel_width,
			       check_valid_int_or_function);
	IIFORMAT_VALID_KEYWORD(combo_box, Q_face, check_valid_face);
	IIFORMAT_VALID_KEYWORD(combo_box, Q_items, check_valid_item_list);
}

static void image_instantiator_scrollbar(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(scrollbar, "scrollbar");
	IIFORMAT_HAS_SHARED_METHOD(scrollbar, validate, widget);
	IIFORMAT_HAS_SHARED_METHOD(scrollbar, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(scrollbar, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(scrollbar, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(scrollbar, governing_domain, subwindow);
	VALID_GUI_KEYWORDS(scrollbar);

	IIFORMAT_VALID_KEYWORD(scrollbar, Q_pixel_width,
			       check_valid_int_or_function);
	IIFORMAT_VALID_KEYWORD(scrollbar, Q_pixel_height,
			       check_valid_int_or_function);
	IIFORMAT_VALID_KEYWORD(scrollbar, Q_face, check_valid_face);
}

static void image_instantiator_progress_guage(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(progress_gauge, "progress-gauge");
	IIFORMAT_HAS_SHARED_METHOD(progress_gauge, validate, widget);
	IIFORMAT_HAS_SHARED_METHOD(progress_gauge, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(progress_gauge, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(progress_gauge, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(progress_gauge, governing_domain, subwindow);
	VALID_WIDGET_KEYWORDS(progress_gauge);
	VALID_GUI_KEYWORDS(progress_gauge);

	IIFORMAT_VALID_KEYWORD(progress_gauge, Q_value, check_valid_int);
}

static void image_instantiator_tree_view(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(tree_view, "tree-view");
	IIFORMAT_HAS_SHARED_METHOD(tree_view, validate, combo_box);
	IIFORMAT_HAS_SHARED_METHOD(tree_view, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(tree_view, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(tree_view, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(tree_view, governing_domain, subwindow);
	IIFORMAT_HAS_METHOD(tree_view, query_geometry);
	VALID_WIDGET_KEYWORDS(tree_view);
	VALID_GUI_KEYWORDS(tree_view);
	IIFORMAT_VALID_KEYWORD(tree_view, Q_items, check_valid_item_list);
}

static void image_instantiator_tab_control(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(tab_control, "tab-control");
	IIFORMAT_HAS_SHARED_METHOD(tab_control, validate, combo_box);
	IIFORMAT_HAS_SHARED_METHOD(tab_control, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(tab_control, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(tab_control, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(tab_control, governing_domain, subwindow);
	IIFORMAT_HAS_METHOD(tab_control, query_geometry);
	VALID_WIDGET_KEYWORDS(tab_control);
	VALID_GUI_KEYWORDS(tab_control);
	IIFORMAT_VALID_KEYWORD(tab_control, Q_orientation,
			       check_valid_tab_orientation);
	IIFORMAT_VALID_KEYWORD(tab_control, Q_items, check_valid_item_list);
}

static void image_instantiator_labels(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(label, "label");
	IIFORMAT_HAS_SHARED_METHOD(label, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(label, instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(label, post_instantiate, widget);
	IIFORMAT_HAS_SHARED_METHOD(label, governing_domain, subwindow);
	VALID_WIDGET_KEYWORDS(label);
	IIFORMAT_VALID_KEYWORD(label, Q_descriptor, check_valid_string);
}

#define VALID_LAYOUT_KEYWORDS(layout)					   \
  VALID_WIDGET_KEYWORDS (layout);					   \
  IIFORMAT_VALID_KEYWORD (layout, Q_orientation, check_valid_orientation); \
  IIFORMAT_VALID_KEYWORD (layout, Q_justify, check_valid_justification);   \
  IIFORMAT_VALID_KEYWORD (layout, Q_vertically_justify, check_valid_justification);   \
  IIFORMAT_VALID_KEYWORD (layout, Q_horizontally_justify, check_valid_justification);   \
  IIFORMAT_VALID_KEYWORD (layout, Q_border, check_valid_border);	   \
  IIFORMAT_VALID_KEYWORD (layout, Q_margin_width, check_valid_int);	   \
  IIFORMAT_VALID_KEYWORD (layout, Q_items,				   \
			  check_valid_instantiator_list)

static void image_instantiator_layout(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(layout, "layout");
	IIFORMAT_HAS_SHARED_METHOD(layout, possible_dest_types, widget);
	IIFORMAT_HAS_METHOD(layout, instantiate);
	IIFORMAT_HAS_METHOD(layout, post_instantiate);
	IIFORMAT_HAS_SHARED_METHOD(layout, governing_domain, subwindow);
	IIFORMAT_HAS_METHOD(layout, normalize);
	IIFORMAT_HAS_METHOD(layout, query_geometry);
	IIFORMAT_HAS_METHOD(layout, layout);
	IIFORMAT_HAS_METHOD(layout, update);
	IIFORMAT_HAS_METHOD(layout, property);

	VALID_GUI_KEYWORDS(layout);
	VALID_LAYOUT_KEYWORDS(layout);
}

static void image_instantiator_native_layout(void)
{
	INITIALIZE_IMAGE_INSTANTIATOR_FORMAT(native_layout, "native-layout");
	IIFORMAT_HAS_SHARED_METHOD(native_layout, possible_dest_types, widget);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, instantiate, layout);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, post_instantiate, layout);
	IIFORMAT_HAS_METHOD(native_layout, layout);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, governing_domain, subwindow);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, normalize, layout);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, query_geometry, layout);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, layout, layout);
	IIFORMAT_HAS_SHARED_METHOD(native_layout, property, layout);

	VALID_GUI_KEYWORDS(native_layout);
	VALID_LAYOUT_KEYWORDS(native_layout);
}

void image_instantiator_format_create_glyphs_widget(void)
{
	image_instantiator_widget();
	image_instantiator_buttons();
	image_instantiator_edit_fields();
	image_instantiator_combo_box();
	image_instantiator_scrollbar();
	image_instantiator_progress_guage();
	image_instantiator_tree_view();
	image_instantiator_tab_control();
	image_instantiator_labels();
	image_instantiator_layout();
	image_instantiator_native_layout();
}

void reinit_vars_of_glyphs_widget(void)
{
#ifdef DEBUG_WIDGETS
	debug_widget_instances = 0;
#endif
}

void vars_of_glyphs_widget(void)
{
	reinit_vars_of_glyphs_widget();
}

void specifier_vars_of_glyphs_widget(void)
{
	DEFVAR_SPECIFIER("widget-border-width", &Vwidget_border_width	/*
*Border width of widgets.
This is a specifier; use `set-specifier' to change it.
									 */ );
	Vwidget_border_width = Fmake_specifier(Qnatnum);
}
