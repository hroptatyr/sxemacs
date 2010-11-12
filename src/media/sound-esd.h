/* sound-esd.c - play a sound over the esound daemon

   Copyright (C) 2006 Sebastian Freundt

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

#ifndef INCLUDED_sound_esd_h_
#define INCLUDED_sound_esd_h_

#include "sound.h"
#include "media-internal.h"
#include <esd.h>

typedef struct sound_esd_data_s sound_esd_data_t;
typedef struct sound_esd_aj_data_s sound_esd_aj_data_t;

extern Lisp_Object Qesd;		/* initialised in sound.c */

struct sound_esd_data_s {
	/* host which runs the daemon */
	Lisp_Object server;
	int desc;
	int lock;
	int keep_open;
	int sock;		/* nah, we use the other sock */
};

struct sound_esd_aj_data_s {
	/* stream props */
	size_t samplerate;	/* rate used on the device */
	size_t samplewidth;	/* width used on the device */
	unsigned int channels;	/* channels used on the device */
	size_t framesize;	/* resulting framesize */
	int sock;		/* socket fd */
	int flags;		/* ESD flags */
	/* coercion stuff */
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	mtype_audio_properties *mtap;
};

#define DEVICE_CONNECTED_TO_ESD_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_esd);

#endif	/* INCLUDED_sound_esd_h_ */
