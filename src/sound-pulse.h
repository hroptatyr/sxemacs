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

typedef struct sound_pulse_data_s *sound_pulse_data_t;
typedef struct sound_pulse_aj_data_s *sound_pulse_aj_data_t;


extern Lisp_Object Qpulse;		/* initialised in sound.c */

#define DEVICE_CONNECTED_TO_PULSE_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_pulse);

#ifdef ALL_DEBUG_FLAGS
#undef PULSE_DEBUG_FLAG
#define PULSE_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_pulse_h_ */
