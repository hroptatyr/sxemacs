/* Generic object functions.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
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

#ifndef INCLUDED_objects_h_
#define INCLUDED_objects_h_

#include "specifier.h"

void finalose(void *ptr);

/*****************************************************************************
 *                        Color Specifier Object                             *
 *****************************************************************************/

struct color_specifier {
	Lisp_Object face;	/* face this is attached to, or nil */
	Lisp_Object face_property;	/* property of that face */
};

#define COLOR_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, color)
#define COLOR_SPECIFIER_FACE(g) (COLOR_SPECIFIER_DATA (g)->face)
#define COLOR_SPECIFIER_FACE_PROPERTY(g) \
  (COLOR_SPECIFIER_DATA (g)->face_property)

DECLARE_SPECIFIER_TYPE(color);
#define XCOLOR_SPECIFIER(x) XSPECIFIER_TYPE (x, color)
#define XSETCOLOR_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, color)
#define COLOR_SPECIFIERP(x) SPECIFIER_TYPEP (x, color)
#define CHECK_COLOR_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, color)
#define CONCHECK_COLOR_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, color)

void set_color_attached_to(Lisp_Object obj, Lisp_Object face,
			   Lisp_Object property);

/*****************************************************************************
 *                         Font Specifier Object                             *
 *****************************************************************************/

struct font_specifier {
	Lisp_Object face;	/* face this is attached to, or nil */
	Lisp_Object face_property;	/* property of that face */
};

#define FONT_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, font)
#define FONT_SPECIFIER_FACE(g) (FONT_SPECIFIER_DATA (g)->face)
#define FONT_SPECIFIER_FACE_PROPERTY(g) \
  (FONT_SPECIFIER_DATA (g)->face_property)

DECLARE_SPECIFIER_TYPE(font);
#define XFONT_SPECIFIER(x) XSPECIFIER_TYPE (x, font)
#define XSETFONT_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, font)
#define FONT_SPECIFIERP(x) SPECIFIER_TYPEP (x, font)
#define CHECK_FONT_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, font)
#define CONCHECK_FONT_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, font)

void set_font_attached_to(Lisp_Object obj, Lisp_Object face,
			  Lisp_Object property);

/*****************************************************************************
 *                       Face Boolean Specifier Object                       *
 *****************************************************************************/

struct face_boolean_specifier {
	Lisp_Object face;	/* face this is attached to, or nil */
	Lisp_Object face_property;	/* property of that face */
};

#define FACE_BOOLEAN_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, face_boolean)
#define FACE_BOOLEAN_SPECIFIER_FACE(g) (FACE_BOOLEAN_SPECIFIER_DATA (g)->face)
#define FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY(g) \
  (FACE_BOOLEAN_SPECIFIER_DATA (g)->face_property)

DECLARE_SPECIFIER_TYPE(face_boolean);
extern Lisp_Object Qface_boolean;
#define XFACE_BOOLEAN_SPECIFIER(x) XSPECIFIER_TYPE (x, face_boolean)
#define XSETFACE_BOOLEAN_SPECIFIER(x, p) \
  XSETSPECIFIER_TYPE (x, p, face_boolean)
#define FACE_BOOLEAN_SPECIFIERP(x) SPECIFIER_TYPEP (x, face_boolean)
#define CHECK_FACE_BOOLEAN_SPECIFIER(x) \
  CHECK_SPECIFIER_TYPE (x, face_boolean)
#define CONCHECK_FACE_BOOLEAN_SPECIFIER(x) \
  CONCHECK_SPECIFIER_TYPE (x, face_boolean)

void set_face_boolean_attached_to(Lisp_Object obj, Lisp_Object face,
				  Lisp_Object property);

/****************************************************************************
 *                           Color Instance Object                          *
 ****************************************************************************/

EXFUN(Fmake_color_instance, 3);

extern Lisp_Object Vthe_null_color_instance;

struct Lisp_Color_Instance {
	struct lcrecord_header header;
	Lisp_Object name;
	Lisp_Object device;

	/* console-type-specific data */
	void *data;
};

DECLARE_LRECORD(color_instance, Lisp_Color_Instance);
#define XCOLOR_INSTANCE(x) XRECORD (x, color_instance, Lisp_Color_Instance)
#define XSETCOLOR_INSTANCE(x, p) XSETRECORD (x, p, color_instance)
#define COLOR_INSTANCEP(x) RECORDP (x, color_instance)
#define CHECK_COLOR_INSTANCE(x) CHECK_RECORD (x, color_instance)
#define CONCHECK_COLOR_INSTANCE(x) CONCHECK_RECORD (x, color_instance)

#define COLOR_INSTANCE_NAME(c)   ((c)->name)
#define COLOR_INSTANCE_DEVICE(c) ((c)->device)

/****************************************************************************
 *                            Font Instance Object                          *
 ****************************************************************************/

DECLARE_LRECORD(font_instance, Lisp_Font_Instance);
#define XFONT_INSTANCE(x) XRECORD (x, font_instance, Lisp_Font_Instance)
#define XSETFONT_INSTANCE(x, p) XSETRECORD (x, p, font_instance)
#define FONT_INSTANCEP(x) RECORDP (x, font_instance)
#define CHECK_FONT_INSTANCE(x) CHECK_RECORD (x, font_instance)
#define CONCHECK_FONT_INSTANCE(x) CONCHECK_RECORD (x, font_instance)

#ifdef MULE
int font_spec_matches_charset(struct device *d, Lisp_Object charset,
			      const Bufbyte * nonreloc,
			      Lisp_Object reloc, Bytecount offset,
			      Bytecount length);
#endif

EXFUN(Fmake_font_instance, 3);
EXFUN(Ffont_instance_name, 1);
EXFUN(Ffont_instance_p, 1);
EXFUN(Ffont_instance_truename, 1);

extern Lisp_Object Vthe_null_font_instance;

struct Lisp_Font_Instance {
	struct lcrecord_header header;
	Lisp_Object name;
	Lisp_Object device;

	unsigned short ascent;	/* extracted from `font', or made up */
	unsigned short descent;
	unsigned short width;
	unsigned short height;
	int proportional_p;

	/* console-type-specific data */
	void *data;
};

#define FONT_INSTANCE_NAME(f)	 ((f)->name)
#define FONT_INSTANCE_DEVICE(f)	 ((f)->device)
#define FONT_INSTANCE_ASCENT(f)	 ((f)->ascent)
#define FONT_INSTANCE_DESCENT(f) ((f)->descent)
#define FONT_INSTANCE_WIDTH(f)	 ((f)->width)
#define FONT_INSTANCE_HEIGHT(f)	 ((f)->height)

#endif				/* INCLUDED_objects_h_ */
