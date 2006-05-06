/* sound-esd.c - play a sound over the esound daemon

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

#ifndef INCLUDED_sound_esd_h_
#define INCLUDED_sound_esd_h_

#include "sound.h"
#include "media-native.h"
#include <esd.h>

struct sound_esd_data {
	/* host which runs the daemon */
	char *server;
	/* port number */
	uint16_t port;
};
typedef struct sound_esd_data sound_esd_data;

extern char *sound_esd_subprint(Lisp_Object);
extern sound_esd_data *sound_esd_create(Lisp_Object);
extern void sound_esd_finish(sound_esd_data*);

extern int sound_esd_play_stream(media_subthread*);

extern Lisp_Object Qesd;		/* initialised in sound.c */


#define DEVICE_CONNECTED_TO_ESD_P(x) 1	/* FIXME: better check */

#endif	/* INCLUDED_sound_esd_h_ */
