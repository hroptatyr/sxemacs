/* sound-arts.c - play a sound over the arts server

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

#ifndef INCLUDED_sound_arts_h_
#define INCLUDED_sound_arts_h_

#include "sound.h"
#include <artsc.h>

typedef struct sound_arts_data_s sound_arts_data_t;
typedef struct sound_arts_aj_data_s sound_arts_aj_data_t;

struct sound_arts_data_s {
	int lock;
	sxe_mutex_t mtx;
};

struct sound_arts_aj_data_s {
	arts_stream_t as;

	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	mtype_audio_properties *mtap;
	sxe_mse_volume_args *volargs;
	sxe_mse_rerate_args *rrargs;

	int framesize;
};

extern Lisp_Object Qarts;		/* initialised in sound.c */

#define DEVICE_CONNECTED_TO_ARTS_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_arts);

#ifdef ALL_DEBUG_FLAGS
#undef ARTS_DEBUG_FLAG
#define ARTS_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_arts_h_ */
