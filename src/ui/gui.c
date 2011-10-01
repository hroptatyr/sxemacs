/* Generic GUI code. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/* This file not quite Mule-ized yet but will be when merged with my
   Mule workspace. --ben */

#include <config.h>
#include "lisp.h"
#include "gui.h"
#include "elhash.h"
#include "buffer.h"
#include "bytecode.h"

Lisp_Object Qmenu_no_selection_hook;
Lisp_Object Vmenu_no_selection_hook;

static Lisp_Object parse_gui_item_tree_list(Lisp_Object list);
Lisp_Object find_keyword_in_vector(Lisp_Object vector, Lisp_Object keyword);

#ifdef HAVE_POPUPS

/* count of menus/dboxes currently up */
int popup_up_p;

DEFUN("popup-up-p", Fpopup_up_p, 0, 0, 0,	/*
Return t if a popup menu or dialog box is up, nil otherwise.
See `popup-menu' and `popup-dialog-box'.
*/
      ())
{
	return popup_up_p ? Qt : Qnil;
}
#endif				/* HAVE_POPUPS */

int separator_string_p(const Bufbyte * s)
{
	const Bufbyte *p;
	Bufbyte first;

	if (!s || s[0] == '\0')
		return 0;
	first = s[0];
	if (first != '-' && first != '=')
		return 0;
	for (p = s; *p == first; p++) ;

	return (*p == '!' || *p == ':' || *p == '\0');
}

/* Massage DATA to find the correct function and argument.  Used by
   popup_selection_callback() and the msw code. */
void get_gui_callback(Lisp_Object data, Lisp_Object * fn, Lisp_Object * arg)
{
	if (EQ(data, Qquit)) {
		*fn = Qeval;
		*arg = list3(Qsignal, list2(Qquote, Qquit), Qnil);
		Vquit_flag = Qt;
	} else if (SYMBOLP(data)
		   || (COMPILED_FUNCTIONP(data)
		       && XCOMPILED_FUNCTION(data)->flags.interactivep)
		   || (CONSP(data) && (EQ(XCAR(data), Qlambda))
		       && !NILP(Fassq(Qinteractive, Fcdr(Fcdr(data)))))) {
		*fn = Qcall_interactively;
		*arg = data;
	} else if (CONSP(data)) {
		*fn = Qeval;
		*arg = data;
	} else {
		*fn = Qeval;
		*arg = list3(Qsignal,
			     list2(Qquote, Qerror),
			     list2(Qquote, list2(build_translated_string
						 ("illegal callback"), data)));
	}
}

/*
 * Add a value VAL associated with keyword KEY into PGUI_ITEM
 * structure. If KEY is not a keyword, or is an unknown keyword, then
 * error is signaled.
 */
int
gui_item_add_keyval_pair(Lisp_Object gui_item,
			 Lisp_Object key, Lisp_Object val, Error_behavior errb)
{
	Lisp_Gui_Item *pgui_item = XGUI_ITEM(gui_item);
	int retval = 0;

	if (!KEYWORDP(key))
		syntax_error_2("Non-keyword in gui item", key, pgui_item->name);

	if (EQ(key, Q_descriptor)) {
		if (!EQ(pgui_item->name, val)) {
			retval = 1;
			pgui_item->name = val;
		}
	}
#define FROB(slot) \
  else if (EQ (key, Q_##slot))			\
  {						\
    if (!EQ (pgui_item->slot, val))			\
      {						\
	retval = 1;				\
	pgui_item->slot   = val;			\
      }						\
  }
	FROB(suffix)
	    FROB(active)
	    FROB(included)
	    FROB(config)
	    FROB(filter)
	    FROB(style)
	    FROB(selected)
	    FROB(keys)
	    FROB(callback)
	    FROB(callback_ex)
	    FROB(value)
#undef FROB
	    else if (EQ(key, Q_key_sequence)) ;	/* ignored for FSF compatibility */
	else if (EQ(key, Q_label)) ;	/* ignored for 21.0 implement in 21.2  */
	else if (EQ(key, Q_accelerator)) {
		if (!EQ(pgui_item->accelerator, val)) {
			retval = 1;
			if (SYMBOLP(val) || CHARP(val))
				pgui_item->accelerator = val;
			else if (ERRB_EQ(errb, ERROR_ME))
				syntax_error("Bad keyboard accelerator", val);
		}
	} else if (ERRB_EQ(errb, ERROR_ME))
		syntax_error_2("Unknown keyword in gui item", key,
			       pgui_item->name);
	return retval;
}

void gui_item_init(Lisp_Object gui_item)
{
	Lisp_Gui_Item *lp = XGUI_ITEM(gui_item);

	lp->name = Qnil;
	lp->callback = Qnil;
	lp->callback_ex = Qnil;
	lp->suffix = Qnil;
	lp->active = Qt;
	lp->included = Qt;
	lp->config = Qnil;
	lp->filter = Qnil;
	lp->style = Qnil;
	lp->selected = Qnil;
	lp->keys = Qnil;
	lp->accelerator = Qnil;
	lp->value = Qnil;
}

Lisp_Object allocate_gui_item(void)
{
	Lisp_Gui_Item *lp =
	    alloc_lcrecord_type(Lisp_Gui_Item, &lrecord_gui_item);
	Lisp_Object val;

	zero_lcrecord(lp);
	XSETGUI_ITEM(val, lp);

	gui_item_init(val);

	return val;
}

/*
 * ITEM is a lisp vector, describing a menu item or a button. The
 * function extracts the description of the item into the PGUI_ITEM
 * structure.
 */
static Lisp_Object
make_gui_item_from_keywords_internal(Lisp_Object item, Error_behavior errb)
{
	int length, plist_p, start;
	Lisp_Object *contents;
	Lisp_Object gui_item = allocate_gui_item();
	Lisp_Gui_Item *pgui_item = XGUI_ITEM(gui_item);

	CHECK_VECTOR(item);
	length = XVECTOR_LENGTH(item);
	contents = XVECTOR_DATA(item);

	if (length < 1)
		syntax_error
		    ("GUI item descriptors must be at least 1 elts long", item);

	/* length 1:                  [ "name" ]
	   length 2:          [ "name" callback ]
	   length 3:          [ "name" callback active-p ]
	   or [ "name" keyword  value  ]
	   length 4:          [ "name" callback active-p suffix ]
	   or [ "name" callback keyword  value  ]
	   length 5+:         [ "name" callback [ keyword value ]+ ]
	   or [ "name" [ keyword value ]+ ]
	 */
	plist_p = (length > 2 && (KEYWORDP(contents[1])
				  || KEYWORDP(contents[2])));

	pgui_item->name = contents[0];
	if (length > 1 && !KEYWORDP(contents[1])) {
		pgui_item->callback = contents[1];
		start = 2;
	} else
		start = 1;

	if (!plist_p && length > 2)
		/* the old way */
	{
		pgui_item->active = contents[2];
		if (length == 4)
			pgui_item->suffix = contents[3];
	} else
		/* the new way */
	{
		int i;
		if ((length - start) & 1)
			syntax_error
			    ("GUI item descriptor has an odd number of keywords and values",
			     item);

		for (i = start; i < length;) {
			Lisp_Object key = contents[i++];
			Lisp_Object val = contents[i++];
			gui_item_add_keyval_pair(gui_item, key, val, errb);
		}
	}
	return gui_item;
}

/* This will only work with descriptors in the new format. */
Lisp_Object widget_gui_parse_item_keywords(Lisp_Object item)
{
	int i, length;
	Lisp_Object *contents;
	Lisp_Object gui_item = allocate_gui_item();
	Lisp_Object desc = find_keyword_in_vector(item, Q_descriptor);

	CHECK_VECTOR(item);
	length = XVECTOR_LENGTH(item);
	contents = XVECTOR_DATA(item);

	if (!NILP(desc) && !STRINGP(desc) && !VECTORP(desc))
		syntax_error("Invalid GUI item descriptor", item);

	if (length & 1) {
		if (!SYMBOLP(contents[0]))
			syntax_error("Invalid GUI item descriptor", item);
		contents++;	/* Ignore the leading symbol. */
		length--;
	}

	for (i = 0; i < length;) {
		Lisp_Object key = contents[i++];
		Lisp_Object val = contents[i++];
		gui_item_add_keyval_pair(gui_item, key, val, ERROR_ME_NOT);
	}

	return gui_item;
}

/* Update a gui item from a partial descriptor. */
int update_gui_item_keywords(Lisp_Object gui_item, Lisp_Object item)
{
	int i, length, retval = 0;
	Lisp_Object *contents;

	CHECK_VECTOR(item);
	length = XVECTOR_LENGTH(item);
	contents = XVECTOR_DATA(item);

	if (length & 1) {
		if (!SYMBOLP(contents[0]))
			syntax_error("Invalid GUI item descriptor", item);
		contents++;	/* Ignore the leading symbol. */
		length--;
	}

	for (i = 0; i < length;) {
		Lisp_Object key = contents[i++];
		Lisp_Object val = contents[i++];
		if (gui_item_add_keyval_pair(gui_item, key, val, ERROR_ME_NOT))
			retval = 1;
	}
	return retval;
}

Lisp_Object gui_parse_item_keywords(Lisp_Object item)
{
	return make_gui_item_from_keywords_internal(item, ERROR_ME);
}

Lisp_Object gui_parse_item_keywords_no_errors(Lisp_Object item)
{
	return make_gui_item_from_keywords_internal(item, ERROR_ME_NOT);
}

/* convert a gui item into plist properties */
void gui_add_item_keywords_to_plist(Lisp_Object plist, Lisp_Object gui_item)
{
	Lisp_Gui_Item *pgui_item = XGUI_ITEM(gui_item);

	if (!NILP(pgui_item->callback))
		Fplist_put(plist, Q_callback, pgui_item->callback);
	if (!NILP(pgui_item->callback_ex))
		Fplist_put(plist, Q_callback_ex, pgui_item->callback_ex);
	if (!NILP(pgui_item->suffix))
		Fplist_put(plist, Q_suffix, pgui_item->suffix);
	if (!NILP(pgui_item->active))
		Fplist_put(plist, Q_active, pgui_item->active);
	if (!NILP(pgui_item->included))
		Fplist_put(plist, Q_included, pgui_item->included);
	if (!NILP(pgui_item->config))
		Fplist_put(plist, Q_config, pgui_item->config);
	if (!NILP(pgui_item->filter))
		Fplist_put(plist, Q_filter, pgui_item->filter);
	if (!NILP(pgui_item->style))
		Fplist_put(plist, Q_style, pgui_item->style);
	if (!NILP(pgui_item->selected))
		Fplist_put(plist, Q_selected, pgui_item->selected);
	if (!NILP(pgui_item->keys))
		Fplist_put(plist, Q_keys, pgui_item->keys);
	if (!NILP(pgui_item->accelerator))
		Fplist_put(plist, Q_accelerator, pgui_item->accelerator);
	if (!NILP(pgui_item->value))
		Fplist_put(plist, Q_value, pgui_item->value);
}

/*
 * Decide whether a GUI item is active by evaluating its :active form
 * if any
 */
int gui_item_active_p(Lisp_Object gui_item)
{
	/* This function can call lisp */

	/* Shortcut to avoid evaluating Qt each time */
	return (EQ(XGUI_ITEM(gui_item)->active, Qt)
		|| !NILP(Feval(XGUI_ITEM(gui_item)->active)));
}

/* set menu accelerator key to first underlined character in menu name */
Lisp_Object gui_item_accelerator(Lisp_Object gui_item)
{
	Lisp_Gui_Item *pgui = XGUI_ITEM(gui_item);

	if (!NILP(pgui->accelerator))
		return pgui->accelerator;

	else
		return gui_name_accelerator(pgui->name);
}

Lisp_Object gui_name_accelerator(Lisp_Object nm)
{
	Bufbyte *name = XSTRING_DATA(nm);

	while (*name) {
		if (*name == '%') {
			++name;
			if (!(*name))
				return Qnil;
			if (*name == '_' && *(name + 1)) {
				Emchar accelerator = charptr_emchar(name + 1);
				/* #### bogus current_buffer dependency */
				return
				    make_char(DOWNCASE
					      (current_buffer, accelerator));
			}
		}
		INC_CHARPTR(name);
	}
	return make_char(DOWNCASE(current_buffer,
				  charptr_emchar(XSTRING_DATA(nm))));
}

/*
 * Decide whether a GUI item is selected by evaluating its :selected form
 * if any
 */
int gui_item_selected_p(Lisp_Object gui_item)
{
	/* This function can call lisp */

	/* Shortcut to avoid evaluating Qt each time */
	return (EQ(XGUI_ITEM(gui_item)->selected, Qt)
		|| !NILP(Feval(XGUI_ITEM(gui_item)->selected)));
}

Lisp_Object gui_item_list_find_selected(Lisp_Object gui_item_list)
{
	/* This function can GC. */
	Lisp_Object rest;
	LIST_LOOP(rest, gui_item_list) {
		if (gui_item_selected_p(XCAR(rest)))
			return XCAR(rest);
	}
	return XCAR(gui_item_list);
}

/*
 * Decide whether a GUI item is included by evaluating its :included
 * form if given, and testing its :config form against supplied CONFLIST
 * configuration variable
 */
int gui_item_included_p(Lisp_Object gui_item, Lisp_Object conflist)
{
	/* This function can call lisp */
	Lisp_Gui_Item *pgui_item = XGUI_ITEM(gui_item);

	/* Evaluate :included first. Shortcut to avoid evaluating Qt each time */
	if (!EQ(pgui_item->included, Qt)
	    && NILP(Feval(pgui_item->included)))
		return 0;

	/* Do :config if conflist is given */
	if (!NILP(conflist) && !NILP(pgui_item->config)
	    && NILP(Fmemq(pgui_item->config, conflist)))
		return 0;

	return 1;
}

static DOESNT_RETURN signal_too_long_error(Lisp_Object name)
{
	syntax_error("GUI item produces too long displayable string", name);
}

#ifdef HAVE_WINDOW_SYSTEM
/*
 * Format "left flush" display portion of an item into BUF, guarded by
 * maximum buffer size BUF_LEN. BUF_LEN does not count for terminating
 * null character, so actual maximum size of buffer consumed is
 * BUF_LEN + 1 bytes. If buffer is not big enough, then error is
 * signaled.
 * Return value is the offset to the terminating null character into the
 * buffer.
 */
unsigned int
gui_item_display_flush_left(Lisp_Object gui_item, char *buf, Bytecount buf_len)
{
	/* This function can call lisp */
	char *p = buf;
	Bytecount len;
	Lisp_Gui_Item *pgui_item = XGUI_ITEM(gui_item);

	/* Copy item name first */
	CHECK_STRING(pgui_item->name);
	len = XSTRING_LENGTH(pgui_item->name);
	if (len > buf_len)
		signal_too_long_error(pgui_item->name);
	memcpy(p, XSTRING_DATA(pgui_item->name), len);
	p += len;

	/* Add space and suffix, if there is a suffix.
	 * If suffix is not string evaluate it */
	if (!NILP(pgui_item->suffix)) {
		Lisp_Object suffix = pgui_item->suffix;
		/* Shortcut to avoid evaluating suffix each time */
		if (!STRINGP(suffix)) {
			suffix = Feval(suffix);
			CHECK_STRING(suffix);
		}

		len = XSTRING_LENGTH(suffix);
		if (p + len + 1 > buf + buf_len)
			signal_too_long_error(pgui_item->name);
		*(p++) = ' ';
		memcpy(p, XSTRING_DATA(suffix), len);
		p += len;
	}
	*p = '\0';
	return p - buf;
}

/*
 * Format "right flush" display portion of an item into BUF, guarded by
 * maximum buffer size BUF_LEN. BUF_LEN does not count for terminating
 * null character, so actual maximum size of buffer consumed is
 * BUF_LEN + 1 bytes. If buffer is not big enough, then error is
 * signaled.
 * Return value is the offset to the terminating null character into the
 * buffer.
 */
unsigned int
gui_item_display_flush_right(Lisp_Object gui_item, char *buf, Bytecount buf_len)
{
	Lisp_Gui_Item *pgui_item = XGUI_ITEM(gui_item);
	*buf = 0;

#ifdef HAVE_MENUBARS
	/* Have keys? */
	if (!menubar_show_keybindings)
		return 0;
#endif

	/* Try :keys first */
	if (!NILP(pgui_item->keys)) {
		CHECK_STRING(pgui_item->keys);
		if (XSTRING_LENGTH(pgui_item->keys) + 1 > buf_len)
			signal_too_long_error(pgui_item->name);
		memcpy(buf, XSTRING_DATA(pgui_item->keys),
		       XSTRING_LENGTH(pgui_item->keys) + 1);
		return XSTRING_LENGTH(pgui_item->keys);
	}

	/* See if we can derive keys out of callback symbol */
	if (SYMBOLP(pgui_item->callback)) {
		char buf2[1024];	/* #### */
		Bytecount len;

		where_is_to_char(pgui_item->callback, buf2);
		len = strlen(buf2);
		if (len > buf_len)
			signal_too_long_error(pgui_item->name);
		strcpy(buf, buf2);
		return len;
	}

	/* No keys - no right flush display */
	return 0;
}
#endif				/* HAVE_WINDOW_SYSTEM */

static Lisp_Object mark_gui_item(Lisp_Object obj)
{
	Lisp_Gui_Item *p = XGUI_ITEM(obj);

	mark_object(p->name);
	mark_object(p->callback);
	mark_object(p->callback_ex);
	mark_object(p->config);
	mark_object(p->suffix);
	mark_object(p->active);
	mark_object(p->included);
	mark_object(p->config);
	mark_object(p->filter);
	mark_object(p->style);
	mark_object(p->selected);
	mark_object(p->keys);
	mark_object(p->accelerator);
	mark_object(p->value);

	return Qnil;
}

static unsigned long gui_item_hash(Lisp_Object obj, int depth)
{
	Lisp_Gui_Item *p = XGUI_ITEM(obj);

	return HASH2(HASH6(internal_hash(p->name, depth + 1),
			   internal_hash(p->callback, depth + 1),
			   internal_hash(p->callback_ex, depth + 1),
			   internal_hash(p->suffix, depth + 1),
			   internal_hash(p->active, depth + 1),
			   internal_hash(p->included, depth + 1)),
		     HASH6(internal_hash(p->config, depth + 1),
			   internal_hash(p->filter, depth + 1),
			   internal_hash(p->style, depth + 1),
			   internal_hash(p->selected, depth + 1),
			   internal_hash(p->keys, depth + 1),
			   internal_hash(p->value, depth + 1)));
}

int gui_item_id_hash(Lisp_Object hashtable, Lisp_Object gitem, int slot)
{
	int hashid = gui_item_hash(gitem, 0);
	int id = GUI_ITEM_ID_BITS(hashid, slot);
	while (!NILP(Fgethash(make_int(id), hashtable, Qnil))) {
		id = GUI_ITEM_ID_BITS(id + 1, slot);
	}
	return id;
}

int gui_item_equal_sans_selected(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Gui_Item *p1 = XGUI_ITEM(obj1);
	Lisp_Gui_Item *p2 = XGUI_ITEM(obj2);

	if (!(internal_equal(p1->name, p2->name, depth + 1)
	      && internal_equal(p1->callback, p2->callback, depth + 1)
	      && internal_equal(p1->callback_ex, p2->callback_ex, depth + 1)
	      && EQ(p1->suffix, p2->suffix)
	      && EQ(p1->active, p2->active)
	      && EQ(p1->included, p2->included)
	      && EQ(p1->config, p2->config)
	      && EQ(p1->filter, p2->filter)
	      && EQ(p1->style, p2->style)
	      && EQ(p1->accelerator, p2->accelerator)
	      && EQ(p1->keys, p2->keys)
	      && EQ(p1->value, p2->value)))
		return 0;
	return 1;
}

static int gui_item_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Gui_Item *p1 = XGUI_ITEM(obj1);
	Lisp_Gui_Item *p2 = XGUI_ITEM(obj2);

	if (!(gui_item_equal_sans_selected(obj1, obj2, depth)
	      && EQ(p1->selected, p2->selected)))
		return 0;
	return 1;
}

static void
print_gui_item(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Gui_Item *g = XGUI_ITEM(obj);
	if (print_readably)
		error("printing unreadable object #<gui-item 0x%x>",
		      g->header.uid);
	write_fmt_string(printcharfun, "#<gui-item 0x%x>", g->header.uid);
}

Lisp_Object copy_gui_item(Lisp_Object gui_item)
{
	Lisp_Object ret = allocate_gui_item();
	Lisp_Gui_Item *lp, *g = XGUI_ITEM(gui_item);

	lp = XGUI_ITEM(ret);
	lp->name = g->name;
	lp->callback = g->callback;
	lp->callback_ex = g->callback_ex;
	lp->suffix = g->suffix;
	lp->active = g->active;
	lp->included = g->included;
	lp->config = g->config;
	lp->filter = g->filter;
	lp->style = g->style;
	lp->selected = g->selected;
	lp->keys = g->keys;
	lp->accelerator = g->accelerator;
	lp->value = g->value;

	return ret;
}

Lisp_Object copy_gui_item_tree(Lisp_Object arg)
{
	if (CONSP(arg)) {
		Lisp_Object rest = arg = Fcopy_sequence(arg);
		while (CONSP(rest)) {
			XCAR(rest) = copy_gui_item_tree(XCAR(rest));
			rest = XCDR(rest);
		}
		return arg;
	} else if (GUI_ITEMP(arg))
		return copy_gui_item(arg);
	else
		return arg;
}

/* parse a glyph descriptor into a tree of gui items.

   The gui_item slot of an image instance can be a single item or an
   arbitrarily nested hierarchy of item lists. */

static Lisp_Object parse_gui_item_tree_item(Lisp_Object entry)
{
	Lisp_Object ret = entry;
	struct gcpro gcpro1;

	GCPRO1(ret);

	if (VECTORP(entry)) {
		ret = gui_parse_item_keywords_no_errors(entry);
	} else if (STRINGP(entry)) {
		CHECK_STRING(entry);
	} else
		syntax_error("item must be a vector or a string", entry);

	RETURN_UNGCPRO(ret);
}

Lisp_Object parse_gui_item_tree_children(Lisp_Object list)
{
	Lisp_Object rest, ret = Qnil, sub = Qnil;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(ret, sub);
	CHECK_CONS(list);
	/* recursively add items to the tree view */
	LIST_LOOP(rest, list) {
		if (CONSP(XCAR(rest)))
			sub = parse_gui_item_tree_list(XCAR(rest));
		else
			sub = parse_gui_item_tree_item(XCAR(rest));

		ret = Fcons(sub, ret);
	}
	/* make the order the same as the items we have parsed */
	RETURN_UNGCPRO(Fnreverse(ret));
}

static Lisp_Object parse_gui_item_tree_list(Lisp_Object list)
{
	Lisp_Object ret;
	struct gcpro gcpro1;
	CHECK_CONS(list);
	/* first one can never be a list */
	ret = parse_gui_item_tree_item(XCAR(list));
	GCPRO1(ret);
	ret = Fcons(ret, parse_gui_item_tree_children(XCDR(list)));
	RETURN_UNGCPRO(ret);
}

static void finalize_gui_item(void *header, int for_disksave)
{
}

DEFINE_LRECORD_IMPLEMENTATION("gui-item", gui_item,
			      mark_gui_item, print_gui_item,
			      finalize_gui_item, gui_item_equal,
			      gui_item_hash, 0, Lisp_Gui_Item);

void syms_of_gui(void)
{
	INIT_LRECORD_IMPLEMENTATION(gui_item);

	DEFSYMBOL(Qmenu_no_selection_hook);

#ifdef HAVE_POPUPS
	DEFSUBR(Fpopup_up_p);
#endif
}

void vars_of_gui(void)
{
	DEFVAR_LISP("menu-no-selection-hook", &Vmenu_no_selection_hook	/*
Function or functions to call when a menu or dialog box is dismissed
without a selection having been made.
									 */ );
	Vmenu_no_selection_hook = Qnil;
}
