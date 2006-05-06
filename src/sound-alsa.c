/* sound-alsa.c - play a sound over the alsa

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "lisp.h"

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "media.h"
#include "sound-alsa.h"

Lisp_Object Qalsa;

#define SOUND_ALSA_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec of 48kHz, 32bit, 2ch */
#define MYSELF ADRIVER_ALSA

sound_alsa_data *sound_alsa_create(Lisp_Object alsa_options)
{
#if 0
	/* result */
	sound_alsa_data *ad;
#endif

	return NULL;
}

void sound_alsa_finish(sound_alsa_data *alsa_dev)
{
#if 0
	return;
#endif
}


int sound_alsa_play_stream(media_subthread *mst)
{
	/* stream stuff */
	mtype_audio_properties *mtap;
	Lisp_Media_Stream *ms;
	media_substream *mss;
	Lisp_Media_Thread *mt;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_alsa_data *sad = NULL;
	/* alsa stuff */
	snd_pcm_t *ad;
	snd_pcm_hw_params_t *hwparams;
	snd_pcm_format_t format = SND_PCM_FORMAT_S16_LE;
	/* buffering */
	char *buffer;
	uint32_t len;
	int resolution;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;

	/* unpack device */
	sad = get_audio_device_data(device);
	lad = get_audio_device(device);

	/* cannot use alsa on incomplete or corrupt audio devices */
	if (lad == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio media */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* trigger alsa, the device name should be an option */
	if ((snd_pcm_open(&ad, "default", SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
		message(GETTEXT("Opening ALSA failed."));
		return 0;
	}

	snd_pcm_hw_params_alloca(&hwparams);

	if ((snd_pcm_hw_params_any(ad, hwparams)) < 0) {
		message(GETTEXT("Setting up alsa hardware failed."));
		snd_pcm_hw_free(ad);
		snd_pcm_close(ad);
		return 0;
	}

	if (snd_pcm_hw_params_set_access(ad, hwparams,
					 SND_PCM_ACCESS_RW_INTERLEAVED) < 0) {
		message(GETTEXT("Setting up access failed."));
		snd_pcm_hw_free(ad);
		snd_pcm_close(ad);
		return 0;
	}

	if ((snd_pcm_hw_params_set_format(ad, hwparams, format)) < 0) {
		message(GETTEXT("Setting up format failed."));
		snd_pcm_hw_free(ad);
		snd_pcm_close(ad);
		return 0;
	}

	if (snd_pcm_hw_params_set_rate(ad, hwparams, mtap->samplerate, 0) < 0) {
		message(GETTEXT("Setting up rate failed."));
		snd_pcm_hw_free(ad);
		snd_pcm_close(ad);
		return 0;
	}

	if (snd_pcm_hw_params_set_channels(ad, hwparams, mtap->channels) < 0) {
		message(GETTEXT("Setting up channels failed."));
		snd_pcm_hw_free(ad);
		snd_pcm_close(ad);
		return 0;
	}

	if ((snd_pcm_hw_params(ad, hwparams)) < 0 ||
	    (snd_pcm_prepare(ad)) < 0) {
		message(GETTEXT("Setting up ALSA failed."));
		snd_pcm_hw_free(ad);
		snd_pcm_close(ad);
		return 0;
	}

	/* rewind it ... */
	media_substream_rewind(mss);

	/* ... and play it */
	buffer = xmalloc(SOUND_ALSA_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		snd_pcm_writei(ad, buffer, len);

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* -- Close and shutdown -- */
	xfree(buffer);
	snd_pcm_hw_free(ad);
	snd_pcm_close(ad);
	return 1;
}

#undef MYSELF

/* sound-alsa.c ends here */
