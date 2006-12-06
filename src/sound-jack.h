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
#include "semaphore.h"
#include <jack/jack.h>

#define MAX_CHANS 6

typedef struct sound_jack_data sound_jack_data;
typedef struct sound_jack_subthread_data sound_jack_subthread_data;
typedef size_t(*sound_jack_demux_fun)(float*, char*, size_t, int,
				      sound_jack_subthread_data*);

struct sound_jack_data {
	Lisp_Object options;
	Lisp_Object server;
	Lisp_Object client;
	int num_ports;
};

struct sound_jack_subthread_data {
	jack_client_t *client;
	jack_port_t *ports[MAX_CHANS];
	const char **port_ptrs;
	int num_ports;
	int channels;
	int underrun;
	int paused;
	int volume;
	float fvol;
	float ratetrafo;
	/* thread management */
	sxe_semaphore_t *sem;
	/* buffer management */
	size_t overfill;	/* in frames */
	size_t ringbufcnt;
	size_t readpos;
	size_t writepos;
	sound_jack_demux_fun demux_fun;
	/* stream props */
	size_t framesize;
	size_t samplerate;
};

extern Lisp_Object Qjack;		/* initialised in sound.c */

#define DEVICE_CONNECTED_TO_JACK_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_jack);

#ifdef ALL_DEBUG_FLAGS
#undef JACK_DEBUG_FLAG
#define JACK_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_jack_h_ */
