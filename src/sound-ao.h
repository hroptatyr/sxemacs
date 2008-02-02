/* sound-ao.c - play a sound over the libao devices

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

#ifndef INCLUDED_sound_ao_h_
#define INCLUDED_sound_ao_h_

#include "sound.h"
#include <ao/ao.h>

typedef struct sound_ao_data_s sound_ao_data_t;
typedef struct sound_ao_aj_data_s sound_ao_aj_data_t;

struct sound_ao_data_s {
	ao_device *ad;
	ao_option *options;
	ao_sample_format *fmt;
	int driver_id;
};

struct sound_ao_aj_data_s {
	ao_device *dev;

	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	mtype_audio_properties *mtap;
	sxe_mse_volume_args *volargs;
	sxe_mse_rerate_args *rrargs;
	int framesize;
};

extern Lisp_Object Qao;		/* initialised in sound.c */

#define DEVICE_CONNECTED_TO_AO_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_ao);

#ifdef ALL_DEBUG_FLAGS
#undef AO_DEBUG_FLAG
#define AO_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_ao_h_ */
