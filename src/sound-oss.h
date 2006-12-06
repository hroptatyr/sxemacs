/* sound-linuxplay.h - play a sound natively

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

#ifndef INCLUDED_sound_oss_h_
#define INCLUDED_sound_oss_h_

#include "sound.h"
#if HAVE_MACHINE_SOUNDCARD_H
#include <machine/soundcard.h>
#elif HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#elif HAVE_LINUX_SOUNDCARD_H
#include <linux/soundcard.h>
#else
#include <soundcard.h>
#endif

typedef struct sound_oss_data sound_oss_data;
typedef struct sound_oss_subthread_data sound_oss_subthread_data;

extern Lisp_Object Qoss;		/* initialised in sound.c */

struct sound_oss_data {
	Lisp_Object device;
	int desc;
	int lock;
	int keep_open;
	int device_fd;
};

struct sound_oss_subthread_data {
	int paused;
	int volume;
	/* stream props */
	size_t samplerate;	/* rate used on the device */
	unsigned int channels;	/* channels used on the device */
	unsigned int framesize;
	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	mtype_audio_properties *mtap;
};

#define DEVICE_CONNECTED_TO_OSS_P(x) 1	/* FIXME: better check */

DECLARE_AUDIO_DEVICE(sound_oss);

#ifdef ALL_DEBUG_FLAGS
#undef OSS_DEBUG_FLAG
#define OSS_DEBUG_FLAG
#endif

#endif	/* INCLUDED_sound_oss_h_ */
