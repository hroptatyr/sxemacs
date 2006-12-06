/* sound-pulse.h - play a sound over the PulseAudio Server

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

#ifndef INCLUDED_sound_pulse_h_
#define INCLUDED_sound_pulse_h_

#include "sound.h"
#include <pulse/pulseaudio.h>
#include "semaphore.h"

struct sound_pulse_data {
	Lisp_Object stream; /* media has to deal with this actually */
	Lisp_Object client;
	Lisp_Object sink;
	Lisp_Object source;
	Lisp_Object server;
	int ml_running_p;
	int ml_threaded_p;
	pa_threaded_mainloop *tml;
	pa_mainloop *ml;
	pa_mainloop_api *mlapi;
	pa_context *ctx;
	const pa_sink_info *sink_info;
	sxe_semaphore_t ctxsem;
};
typedef struct sound_pulse_data sound_pulse_data;

struct sound_pulse_subthread_data {
	int volume;
	pa_cvolume chanvol;
	struct pa_sample_spec sampspec;
	struct pa_channel_map chanmap;
	pa_buffer_attr buffattr;
	pa_stream *stream;
	pa_context *ctx;

	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	sxe_mse_rerate_args *rrargs;
};
typedef struct sound_pulse_subthread_data sound_pulse_subthread_data;

extern Lisp_Object Qpulse;		/* initialised in sound.c */

#define DEVICE_CONNECTED_TO_PULSE_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_pulse);

#ifdef ALL_DEBUG_FLAGS
#undef PULSE_DEBUG_FLAG
#define PULSE_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_pulse_h_ */
