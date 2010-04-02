/* sound-nas.c - play a sound over NAS

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

/* based largely on the ao_nas mplayer plugin by Tobias Diedrich */

#ifndef INCLUDED_sound_nas_h_
#define INCLUDED_sound_nas_h_

#include "sound.h"
#include <audio/audiolib.h>

#define NAS_FRAG_SIZE 4096

typedef struct sound_nas_data_s sound_nas_data_t;
typedef struct sound_nas_aj_data_s sound_nas_aj_data_t;

struct sound_nas_data_s {
	AuServer *aud;
	size_t proposed_buffer_size;
};

struct sound_nas_aj_data_s {
	sound_nas_data_t *snd;
	AuFlowID flow;
	AuDeviceID dev;
	AuFixedPoint gain;

	size_t samplerate;	/* rate used on the device */
	unsigned int channels;	/* channels used on the device */
	unsigned int framesize;
	int resolution;
	int volume;

	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	mtype_audio_properties *mtap;
	sxe_mse_volume_args *volargs;
	sxe_mse_rerate_args *rrargs;

	size_t writepos;
	size_t readpos;
	size_t overfill;
	int underrun;
	size_t buffer_size;

	media_thread_play_state mtp;
};

extern void nas_wait_for_sounds(void);

extern Lisp_Object Qnas;		/* initialised in sound.c */

DECLARE_AUDIO_DEVICE(sound_nas);

#ifdef ALL_DEBUG_FLAGS
#undef NAS_DEBUG_FLAG
#define NAS_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_nas_h_ */
