/* Define generic menubar support.
   Copyright (C) 1995 Board of Trustees, University of Illinois.

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

/* #### Still needs some device-abstraction work. */

#ifndef INCLUDED_menubar_h_
#define INCLUDED_menubar_h_

#ifdef HAVE_MENUBARS
#include "gui.h"
void update_frame_menubars(struct frame *f);
void free_frame_menubars(struct frame *f);
Lisp_Object menu_parse_submenu_keywords(Lisp_Object desc, Lisp_Object gui_item);
Lisp_Object current_frame_menubar(const struct frame *f);

EXFUN(Fmenu_find_real_submenu, 2);

extern Lisp_Object Vmenu_accelerator_prefix;
extern Lisp_Object Vmenu_accelerator_modifiers;
extern Lisp_Object Vmenu_accelerator_enabled;
extern Lisp_Object Vmenu_accelerator_map;

extern Lisp_Object Qmenu_force;
extern Lisp_Object Qmenu_fallback;

extern Lisp_Object Qmenu_quit;
extern Lisp_Object Qmenu_up;
extern Lisp_Object Qmenu_down;
extern Lisp_Object Qmenu_left;
extern Lisp_Object Qmenu_right;
extern Lisp_Object Qmenu_select;
extern Lisp_Object Qmenu_escape;

/* #### kluuuuuuuuuuuuuuuuuuuuuuuuuuuudge!
   The author of the accelerator code didn't know what the hell he was doing.
   Someone needs to abstract this properly. */
#if defined(HAVE_X_WINDOWS) && defined(LWLIB_MENUBARS_LUCID)
extern int x_kludge_lw_menu_active(void);
struct command_builder;
Lisp_Object command_builder_find_menu_accelerator(struct command_builder
						  *builder);
Lisp_Object command_builder_operate_menu_accelerator(struct command_builder
						     *builder);

extern int in_menu_callback;
#endif

#endif				/* HAVE_MENUBARS */

#endif				/* INCLUDED_menubar_h_ */
