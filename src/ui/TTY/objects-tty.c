/* TTY-specific Lisp objects.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 2007, 2008 Nelson Ferreira

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

#include "console-tty.h"
#include "ui/insdel.h"
#include "objects-tty.h"
#ifdef MULE
#include "ui/device.h"
#include "mule/mule-charset.h"
#endif

/* for CHECK_INT_OR_FLOAT */
#include "ent/ent.h"

#include "skiplist.h"

/* Term => ( color => '(index bold) ) */
Lisp_Object Vterm_color_alias_slist;

/* Term => ( [r g b] => '(index bold) ) */
Lisp_Object Vterm_color_map_slist;


Lisp_Object Qx_nearest_color;

static inline Lisp_Object
get_term_color_alias_slist( Lisp_Object sym_term, unsigned create )
	__attribute__((always_inline));

static inline Lisp_Object
get_term_color_alias_slist( Lisp_Object sym_term, unsigned create )
{
	Lisp_Object color_slist = Qnil;

	if ( EQ(Vterm_color_alias_slist,Qnil) ) {
		Vterm_color_alias_slist = make_skiplist();
	} else {
		assert(SKIPLISTP(Vterm_color_alias_slist));
		color_slist = get_skiplist(XSKIPLIST(Vterm_color_alias_slist),
					   sym_term, Qnil);
	}
	if ( EQ(color_slist,Qnil) ) {
		if ( create ) {
			color_slist = make_skiplist();
		}
	} else {
		assert(SKIPLISTP(color_slist));
	}
	return color_slist;
}

static inline Lisp_Object
get_term_color_map_slist( Lisp_Object sym_term, unsigned create )
	__attribute__((always_inline));
static inline Lisp_Object
get_term_color_map_slist( Lisp_Object sym_term, unsigned create )
{
	Lisp_Object map_slist = Qnil;

	if ( EQ(Vterm_color_map_slist,Qnil) ) {
		Vterm_color_map_slist = make_skiplist();
	} else {
		assert(SKIPLISTP(Vterm_color_map_slist));
		map_slist = get_skiplist(XSKIPLIST(Vterm_color_map_slist),
					 sym_term, Qnil);
	}
	if ( EQ(map_slist,Qnil) ) {
		if ( create ) {
			map_slist = make_skiplist();
		}
	} else {
		assert(SKIPLISTP(map_slist));
	}
	return map_slist;
}



DEFUN("register-tty-color-index", Fregister_tty_color_index, 6, 7, 0, /*
Register COLOR as recognized by TERM with index IDX and RED, GREEN and BLUE
components.
RED, GREEN and BLUE is expected to be in the range 0 through 65535
*/
      (term,color,idx,red,green,blue,bold))
{
	Lisp_Object sym_term = Qnil;
	Lisp_Object sym_color = Qnil;
	Lisp_Object rgb_tuple = Qnil;
	Lisp_Object idx_tuple = Qnil;
	Lisp_Object map_slist = Qnil;
	Lisp_Object color_slist = Qnil;

	/* Validating the parameter types */
	CHECK_INT(idx);
	CHECK_INT_OR_FLOAT(red);
	CHECK_INT_OR_FLOAT(green);
	CHECK_INT_OR_FLOAT(blue);
	CHECK_SYMBOL(bold);

	/* term and color can be given as symbols or strings.
	   if given as strings we auto-intern them
	*/
	if( RECORD_TYPEP(term, lrecord_type_symbol) ) {
		sym_term = term;
	} else if ( RECORD_TYPEP(term, lrecord_type_string) ) {
		sym_term = Fintern(term,Qnil);
	} else {
		dead_wrong_type_argument(Qstringp,term);
	}
	if( RECORD_TYPEP(color, lrecord_type_symbol) ) {
		sym_color = color;
	} else if ( RECORD_TYPEP(color, lrecord_type_string) ) {
		sym_color = Fintern(color,Qnil);
	} else {
		dead_wrong_type_argument(Qstringp,color);
	}

	color_slist = get_term_color_alias_slist(sym_term, 1);
	map_slist = get_term_color_map_slist(sym_term, 1);

	/* Updating the skiplists
	 */
	rgb_tuple = make_vector(3,Qnil);
	Faset(rgb_tuple, make_int(0), red);
	Faset(rgb_tuple, make_int(1), green);
	Faset(rgb_tuple, make_int(2), blue);

	{       /* Build the index tuple */
		Lisp_Object list_args[2];

		list_args[0] = idx;
		list_args[1] = bold;
		idx_tuple = Flist(2, list_args);
	}

	/* Add the color alias */
	put_skiplist( XSKIPLIST(color_slist),sym_color,idx_tuple);
	put_skiplist( XSKIPLIST(Vterm_color_alias_slist),
		      term, color_slist);
	/* Add the index rgb */
	put_skiplist( XSKIPLIST(map_slist),rgb_tuple,idx_tuple);
	put_skiplist( XSKIPLIST(Vterm_color_map_slist),
		      term, map_slist);
	return Qnil;
}


static Lisp_Object tty_slist_keyname_accum( Lisp_Object key, Lisp_Object val, void* accum)
{
	Lisp_Object *result = (Lisp_Object*)accum;
	*result = Fcons(Fsymbol_name(key), *result);
	return *result;
}

static Lisp_Object tty_slist_key_accum( Lisp_Object key, Lisp_Object val, void* accum)
{
	Lisp_Object *result = (Lisp_Object*)accum;
	*result = Fcons(key, *result);
	return *result;
}

static Lisp_Object nearest_color_slist( Lisp_Object sym_color, Lisp_Object slist )
{
	Lisp_Object rgblist = Qnil;
	Lisp_Object result = Qnil;

	map2_skiplist(XSKIPLIST(slist), tty_slist_key_accum, &rgblist );
	result = call2_trapping_errors( "Error_nearest_color",
					Qx_nearest_color,
					sym_color, rgblist );
	return result;
}


DEFUN("find-tty-color", Ffind_tty_color, 1, 3, 0,	/*
Look up COLOR in the list of registered TTY colors for DEVICE or NEAREST.
DEVICE defaults to the selected device if omitted.
If it is found, return a list (INDEX BOLD) used to set the foreground to the color.
If it is not found, return nil.
If NEAREST is non-nil and an exact match was not found, find and return
the nearest available color.
*/
      (color,device,nearest))
{
	Lisp_Object result = Qnil;
	Lisp_Object console = Qnil;
	Lisp_Object color_slist = Qnil;
	Lisp_Object default_color_slist = Qnil;
	Lisp_Object default_term_slist = Qnil;
	Lisp_Object this_term_slist = Qnil;
	Lisp_Object sym_color =Qnil;
	Lisp_Object sym_term = Qnil;

	if ( EQ(device,Qnil) ) {
		device = Fselected_device(Qnil);
	}
	console=Fdevice_console(device);
	if ( ! CONSOLEP(console) || ! CONSOLE_TYPE_P (XCONSOLE (console), tty) )
		dead_wrong_type_argument(Qconsolep,console);


	if( RECORD_TYPEP(color, lrecord_type_symbol) ) {
		sym_color = color;
	} else if ( RECORD_TYPEP(color, lrecord_type_string) ) {
		sym_color = Fintern(color,Qnil);
	} else {
		dead_wrong_type_argument(Qstringp,color);
	}

	/* First check if there is a term-specific map */
	this_term_slist = color_slist = CONSOLE_TTY_DATA(console)->term_cmap;
	if ( ! EQ(color_slist,Qnil) ) {
		result = Fget_skiplist(color_slist, sym_color, Qnil);
	}
	if ( EQ(result,Qnil) ) {
		/* If not, let's try the term */
		default_term_slist = color_slist =
			get_term_color_alias_slist(
				CONSOLE_TTY_DATA(console)->terminal_type,0);
		if ( ! EQ(color_slist,Qnil) ) {
			result = Fget_skiplist(color_slist, sym_color, Qnil);
		}
	}
	if ( EQ(result,Qnil) ) {
		/* Last resort: the default based on the colors */
		char term_name[32];
		int sz = snprintf(term_name,sizeof(term_name),"default-%d-color",
				  CONSOLE_TTY_DATA(console)->maxcolors);
		assert(sz >= 0 && (size_t)sz < sizeof(term_name));
		sym_term = Fintern(make_string((Bufbyte*)term_name,strlen(term_name)),Qnil);
		default_color_slist = color_slist = get_term_color_alias_slist(sym_term,0);
		if ( ! EQ(color_slist,Qnil) ) {
			result = Fget_skiplist(color_slist, sym_color, Qnil);
		}
	}
	if ( EQ(result,Qnil) && ! EQ(nearest, Qnil) ) {
		Lisp_Object color_alist = Qnil;
		Lisp_Object default_color_alist = Qnil;
		Lisp_Object default_term_alist = Qnil;
		Lisp_Object this_term_alist = Qnil;
		Lisp_Object nearest_rgb = Qnil;
		Lisp_Object args[2];

		/* Lets build the supreme list :) Starting with the
		 * default color, then terminal name, then specific
		 * tty.  This way we get actual override of color defs
		 * in the more specific definitions.
		 */
		/* NOTE: sym_term was filled above... */
		default_color_alist = color_alist = get_term_color_map_slist(sym_term,0);
		args[0] = color_alist;
		default_term_alist = args[1] =
			get_term_color_map_slist(CONSOLE_TTY_DATA(console)->terminal_type,0);
		if ( ! EQ(args[1],Qnil) ) {
			color_alist = Fnconc(2,args);
			args[0] = color_alist;
		}
		this_term_alist = args[1] = CONSOLE_TTY_DATA(console)->term_crgb;
		if ( ! EQ(args[1],Qnil) ) {
			color_alist = Fnconc(2,args);
		}
		if ( ! EQ(color_alist,Qnil) ) {
			nearest_rgb = nearest_color_slist( sym_color, color_alist );
		}
		if ( ! EQ(nearest_rgb, Qnil) ) {
			/* Let's find out where this result comes from ;-) */
			int cached = 0;
			if ( ! EQ(this_term_alist, Qnil) ) {
				result = Fget_skiplist(this_term_alist, nearest_rgb, Qnil);
				if ( ! EQ(result, Qnil) ) {
					put_skiplist( XSKIPLIST(this_term_slist),sym_color,result);
					CONSOLE_TTY_DATA(console)->term_cmap = this_term_slist;
					cached = 1;
				}
			}
			if ( ! cached && ! EQ(default_term_alist, Qnil) &&
			     ! EQ(default_term_slist, Qnil) ) {
				result = Fget_skiplist(default_term_alist, nearest_rgb, Qnil);
				if ( ! EQ(result, Qnil) ) {
					put_skiplist( XSKIPLIST(default_term_slist),sym_color,result);
					put_skiplist( XSKIPLIST(Vterm_color_alias_slist),
						      CONSOLE_TTY_DATA(console)->terminal_type,
						      default_term_slist);
					cached = 1;
				}
			}
			if ( ! cached && ! EQ(default_color_alist, Qnil) &&
			     ! EQ(default_color_slist, Qnil) ) {
				result = Fget_skiplist(default_color_alist, nearest_rgb, Qnil);
				if ( ! EQ(result, Qnil) ) {
					put_skiplist( XSKIPLIST(default_color_slist),sym_color,result);
					put_skiplist( XSKIPLIST(Vterm_color_alias_slist),
						      sym_term, default_color_slist);
					cached = 1;
				}
			}
		}
	}
	return result;
}



DEFUN("tty-registered-color-list", Ftty_registered_color_list, 0, 1, 0,	/*
Return a list of the registered TTY colors FOR DEVICE.
DEVICE defaults to the selected device if omitted.
*/
      (device))
{
	Lisp_Object result = Qnil;
	Lisp_Object console = Qnil;
	Lisp_Object color_slist = Qnil;

	if ( ! EQ(device,Qnil) ) {
		device = Fselected_device(Qnil);
	}
	console=Fdevice_console(device);
	/* First check if there is a term-specific map */
	color_slist = CONSOLE_TTY_DATA(console)->term_cmap;
	if ( EQ(color_slist,Qnil) ) {
		/* If not, let's try the term */
		color_slist = get_term_color_alias_slist(
			CONSOLE_TTY_DATA(console)->terminal_type,0);
	}
	if ( EQ(color_slist,Qnil) ) {
		/* Last resort: the default based on the colors */
		char term_name[32];
		Lisp_Object sym_term;
		int sz = snprintf(term_name,sizeof(term_name),"default-%d-color",
				  CONSOLE_TTY_DATA(console)->maxcolors);
		assert(sz>=0 && (size_t)sz < sizeof(term_name));
		sym_term = Fintern(make_string((Bufbyte*)term_name,strlen(term_name)),Qnil);
		color_slist = get_term_color_alias_slist(sym_term,0);
	}
	if ( ! EQ(color_slist,Qnil) ) {
		map2_skiplist(XSKIPLIST(color_slist), tty_slist_keyname_accum, &result );
	}
	return result;
}


static int
tty_initialize_color_instance(Lisp_Color_Instance * c, Lisp_Object name,
			      Lisp_Object device, Error_behavior errb)
{
/* This function does not GC */
	Lisp_Object result;
	Lisp_Object sym_color;

	if( RECORD_TYPEP(name, lrecord_type_symbol) ) {
		sym_color = name;
	} else if ( RECORD_TYPEP(name, lrecord_type_string) ) {
		sym_color = Fintern(name,Qnil);
	} else {
		dead_wrong_type_argument(Qstringp,name);
	}

	result = Ffind_tty_color( sym_color, device, Qt);

	if (NILP(result)) {
		c->data = NULL;
		return 0;
	}

	/* Don't allocate the data until we're sure that we will succeed. */
	c->data = xnew(struct tty_color_instance_data);
	if ( ! c->data ) {
		return 0;
	}

	COLOR_INSTANCE_TTY_SYMBOL(c) = sym_color;
	return 1;
}

static void tty_mark_color_instance(Lisp_Color_Instance * c)
{
	if ( TTY_COLOR_INSTANCE_DATA(c) )
		mark_object(COLOR_INSTANCE_TTY_SYMBOL(c));
}

static void
tty_print_color_instance(Lisp_Color_Instance * c,
			 Lisp_Object printcharfun, int escapeflag)
{
}

static void tty_finalize_color_instance(Lisp_Color_Instance * c)
{
	if (c->data)
		xfree(c->data);
}

static int
tty_color_instance_equal(Lisp_Color_Instance * c1,
			 Lisp_Color_Instance * c2, int depth)
{
	return (EQ(COLOR_INSTANCE_TTY_SYMBOL(c1),
		   COLOR_INSTANCE_TTY_SYMBOL(c2)));
}

static unsigned long tty_color_instance_hash(Lisp_Color_Instance * c, int depth)
{
	return LISP_HASH(COLOR_INSTANCE_TTY_SYMBOL(c));
}

static int tty_valid_color_name_p(struct device *d, Lisp_Object color)
{
	return (!NILP(Ffind_tty_color( color, wrap_object(d), Qt)));
}

static int
tty_initialize_font_instance(Lisp_Font_Instance * f, Lisp_Object name,
			     Lisp_Object device, Error_behavior errb)
{
	Bufbyte *str = XSTRING_DATA(name);
	Lisp_Object charset = Qnil;

	if (strncmp((const char *)str, "normal", 6))
		return 0;
	str += 6;
	if (*str) {
#ifdef MULE
		if (*str != '/')
			return 0;
		str++;
		charset = Ffind_charset(intern((const char *)str));
		if (NILP(charset))
			return 0;
#else
		return 0;
#endif
	}

	/* Don't allocate the data until we're sure that we will succeed. */
	f->data = xnew(struct tty_font_instance_data);
	FONT_INSTANCE_TTY_CHARSET(f) = charset;
#ifdef MULE
	if (CHARSETP(charset))
		f->width = XCHARSET_COLUMNS(charset);
	else
#endif
		f->width = 1;

	f->proportional_p = 0;
	f->ascent = f->height = 1;
	f->descent = 0;

	return 1;
}

static void tty_mark_font_instance(Lisp_Font_Instance * f)
{
	mark_object(FONT_INSTANCE_TTY_CHARSET(f));
}

static void
tty_print_font_instance(Lisp_Font_Instance * f,
			Lisp_Object printcharfun, int escapeflag)
{
}

static void tty_finalize_font_instance(Lisp_Font_Instance * f)
{
	if (f->data)
		xfree(f->data);
}

static Lisp_Object tty_list_fonts(Lisp_Object pattern, Lisp_Object device)
{
	return list1(build_string("normal"));
}

#ifdef MULE

static int
tty_font_spec_matches_charset(struct device *d, Lisp_Object charset,
			      const Bufbyte * nonreloc, Lisp_Object reloc,
			      Bytecount offset, Bytecount length)
{
	const Bufbyte *the_nonreloc = nonreloc;

	if (!the_nonreloc)
		the_nonreloc = XSTRING_DATA(reloc);
	fixup_internal_substring(nonreloc, reloc, offset, &length);

	assert(length>=0);
	if( length<0 ) {
		abort();
		return -1;
	}

	the_nonreloc += offset;

	if (UNBOUNDP(charset))
		return !memchr(the_nonreloc, '/', length);
	the_nonreloc = (const Bufbyte *)memchr(the_nonreloc, '/', length);
	if (!the_nonreloc)
		return 0;
	the_nonreloc++;
	{
		Lisp_String *s = symbol_name(XSYMBOL(XCHARSET_NAME(charset)));
		return !strcmp((const char *)the_nonreloc,
			       (const char *)string_data(s));
	}
}

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
tty_find_charset_font(Lisp_Object device, Lisp_Object font, Lisp_Object charset)
{
	Bufbyte *fontname = XSTRING_DATA(font);

	if (strchr((const char *)fontname, '/')) {
		if (tty_font_spec_matches_charset(XDEVICE(device), charset, 0,
						  font, 0, -1))
			return font;
		return Qnil;
	}

	if (UNBOUNDP(charset))
		return font;

	return concat3(font, build_string("/"),
		       Fsymbol_name(XCHARSET_NAME(charset)));
}

#endif				/* MULE */

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_objects_tty(void)
{
	DEFSUBR(Fregister_tty_color_index);
	DEFSUBR(Ffind_tty_color);
	DEFSUBR(Ftty_registered_color_list);
	defsymbol(&Qx_nearest_color, "x-nearest-color");
}

void console_type_create_objects_tty(void)
{
	/* object methods */
	CONSOLE_HAS_METHOD(tty, initialize_color_instance);
	CONSOLE_HAS_METHOD(tty, mark_color_instance);
	CONSOLE_HAS_METHOD(tty, print_color_instance);
	CONSOLE_HAS_METHOD(tty, finalize_color_instance);
	CONSOLE_HAS_METHOD(tty, color_instance_equal);
	CONSOLE_HAS_METHOD(tty, color_instance_hash);
	CONSOLE_HAS_METHOD(tty, valid_color_name_p);

	CONSOLE_HAS_METHOD(tty, initialize_font_instance);
	CONSOLE_HAS_METHOD(tty, mark_font_instance);
	CONSOLE_HAS_METHOD(tty, print_font_instance);
	CONSOLE_HAS_METHOD(tty, finalize_font_instance);
	CONSOLE_HAS_METHOD(tty, list_fonts);
#ifdef MULE
	CONSOLE_HAS_METHOD(tty, font_spec_matches_charset);
	CONSOLE_HAS_METHOD(tty, find_charset_font);
#endif
}

void vars_of_objects_tty(void)
{
	DEFVAR_LISP("term-color-alias-slist", &Vterm_color_alias_slist	/*
Term => ( color => '(index bold) )
								 */ );
	DEFVAR_LISP("term-color-map-slist", &Vterm_color_map_slist	/*
Term => ( [r g b] => '(index bold) )
								 */ );
	/*
	staticpro(&Vterm_color_alias_slist);
	staticpro(&Vterm_color_map_slist);
	*/
	Vterm_color_alias_slist = Qnil;
	Vterm_color_map_slist = Qnil;
}
