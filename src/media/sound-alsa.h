/* sound-alsa.c - play a sound over the alsa server

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

#ifndef INCLUDED_sound_alsa_h_
#define INCLUDED_sound_alsa_h_

#include "sound.h"
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/conf.h>
#include <alsa/global.h>
#include <alsa/pcm.h>
#include <alsa/error.h>

typedef struct sound_alsa_data_s sound_alsa_data_t;
typedef struct sound_alsa_aj_data_s sound_alsa_aj_data_t;

struct sound_alsa_data_s {
	Lisp_Object device;
	int keep_open;
	snd_pcm_t *handle;
	snd_pcm_hw_params_t *hwparams;
	int lock;
	sxe_mutex_t mtx;
};

struct sound_alsa_aj_data_s {
	/* stream props */
	unsigned int samplerate;	/* rate used on the device */
	unsigned int channels;	/* channels used on the device */
	size_t framesize;	/* resulting framesize */
	snd_pcm_format_t format;
	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	mtype_audio_properties *mtap;
};

extern Lisp_Object Qalsa;		/* initialised in sound.c */

#define DEVICE_CONNECTED_TO_ALSA_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_alsa);

#ifdef ALL_DEBUG_FLAGS
#undef ALSA_DEBUG_FLAG
#define ALSA_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_alsa_h_ */
