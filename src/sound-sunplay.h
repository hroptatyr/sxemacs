/* sound-sunplay.h - play a sound natively

   Copyright (C) 2006 Sebastian Freundt

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_sound_sunplay_h_
#define INCLUDED_sound_sunplay_h_

#include "sound.h"

struct sound_native_data {
	int driver_id;
};
typedef struct sound_native_data sound_native_data;

extern char *sound_native_subprint(Lisp_Object);
extern sound_native_data *sound_native_create(Lisp_Object);
extern void sound_native_finish(sound_native_data*);

extern int sound_native_play_stream(Lisp_Object);

extern Lisp_Object Qsunplay;		/* initialised in sound.c */


#define DEVICE_CONNECTED_TO_NATIVE_P(x) 1	/* FIXME: better check */

#endif	/* INCLUDED_sound_sunplay_h_ */
