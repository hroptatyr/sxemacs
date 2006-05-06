/* sound-polyp.h - play a sound over the Polypaudio Server

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

#ifndef INCLUDED_sound_polyp_h_
#define INCLUDED_sound_polyp_h_

#include "sound.h"
#if POLYP_VERSION == 8
#  include <polyp/polypaudio.h>
#  include <polyp/mainloop.h>
#  include <polyp/mainloop-signal.h>
#  include <polyp/simple.h>
#  include <polyp/error.h>
#elif POLYP_VERSION == 7
#  include <polyp/polyplib.h>
#  include <polyp/mainloop.h>
#  include <polyp/mainloop-signal.h>
#  include <polyp/polyplib-simple.h>
#  include <polyp/polyplib-error.h>
#endif

struct sound_polyp_data {
	char *stream_name;
	char *client_name;
	char *polyp_device;
	char *polyp_server;
	int driver_id;
};
typedef struct sound_polyp_data sound_polyp_data;

extern char *sound_polyp_subprint(Lisp_Object);
extern sound_polyp_data *sound_polyp_create(Lisp_Object);
extern void sound_polyp_finish(sound_polyp_data*);

extern int sound_polyp_play_stream(media_subthread*);

extern Lisp_Object Qpolyp;		/* initialised in sound.c */


#define DEVICE_CONNECTED_TO_POLYP_P(x) 1	/* FIXME: better check */

#endif	/* INCLUDED_sound_polyp_h_ */
