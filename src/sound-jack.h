/* sound-jack.h - play a sound over the Jackaudio Server

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

#ifndef INCLUDED_sound_jack_h_
#define INCLUDED_sound_jack_h_

#include "sound.h"
#include <jack/jack.h>

struct sound_jack_data {
	char *stream_name;
	char *client_name;
	char *jack_device;
	char *jack_server;
	int driver_id;
};
typedef struct sound_jack_data sound_jack_data;

extern char *sound_jack_subprint(Lisp_Object);
extern sound_jack_data *sound_jack_create(Lisp_Object);
extern void sound_jack_finish(sound_jack_data*);

extern int sound_jack_play_stream(media_subthread*);

extern Lisp_Object Qjack;		/* initialised in sound.c */


#define DEVICE_CONNECTED_TO_JACK_P(x) 1	/* FIXME: better check */

#endif	/* INCLUDED_sound_jack_h_ */
