/* sound-alsa.c - play a sound over the alsa server

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

#ifndef INCLUDED_sound_alsa_h_
#define INCLUDED_sound_alsa_h_

#include "sound.h"
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/conf.h>
#include <alsa/global.h>
#include <alsa/pcm.h>
#include <alsa/error.h>

struct sound_alsa_data {
};
typedef struct sound_alsa_data sound_alsa_data;

extern sound_alsa_data *sound_alsa_create(Lisp_Object);
extern void sound_alsa_finish(sound_alsa_data*);

extern int sound_alsa_play_stream(media_subthread*);

extern Lisp_Object Qalsa;		/* initialised in sound.c */


#define DEVICE_CONNECTED_TO_ALSA_P(x) 1	/* FIXME: better check */

#endif	/* INCLUDED_sound_alsa_h_ */
