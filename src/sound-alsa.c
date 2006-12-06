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

#define MYSELF ADRIVER_ALSA

#define __ALSA_DEBUG__(args...)		fprintf(stderr, "ALSA " args)
#ifndef ALSA_DEBUG_FLAG
#define ALSA_DEBUG(args...)
#else
#define ALSA_DEBUG(args...)		__ALSA_DEBUG__(args)
#endif
#define ALSA_DEBUG_HW(args...)		ALSA_DEBUG("[hardware]: " args)
#define ALSA_DEBUG_S(args...)		ALSA_DEBUG("[stream]: " args)
#define ALSA_DEBUG_COE(args...)		ALSA_DEBUG("[coerce]: " args)
#define ALSA_CRITICAL(args...)		__ALSA_DEBUG__("CRITICAL: " args)


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_alsa);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_alsa);


static Lisp_Object
sound_alsa_mark(ad_device_data *devdata)
{
	sound_alsa_data *sad = devdata;

	mark_object(sad->device);

	return Qnil;
}

static void
sound_alsa_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_alsa_data *sad = NULL;
	char *temp = alloca(48);

	sad = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sad == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :device ", pcfun);
	if (NILP(sad->device))
		write_c_string("#default", pcfun);
	else
		print_internal(sad->device, pcfun, ef);

	if (sad->keep_open) {
		write_c_string(" :keep-open t", pcfun);
		snprintf(temp, 47, " :handle 0x%x", (unsigned int)sad->handle);
		write_c_string(temp, pcfun);
	} else
		write_c_string(" :keep-open nil", pcfun);

	snprintf(temp, 47, " :params 0x%x", (unsigned int)sad->hwparams);
	write_c_string(temp, pcfun);

	return;
}


static int
sound_alsa_open_device(sound_alsa_data *sad)
{
	if (sad->handle == NULL) {
		const char *dev;
		if (NILP(sad->device))
			dev = "default";
		else
			dev = (char*)XSTRING_DATA(sad->device);

		return snd_pcm_open(&sad->handle, dev,
				    SND_PCM_STREAM_PLAYBACK, 0);
	}

	return 0;
}

static int
sound_alsa_init_hardware(sound_alsa_data *sad)
{
	int err = 0;

	if (sad->hwparams == NULL || sad->handle == NULL)
		return -1;

	/* check if we can configure this device */
	err = snd_pcm_hw_params_any(sad->handle, sad->hwparams);
	if (err < 0)
		return err;

	err = snd_pcm_hw_params_set_access(
		sad->handle, sad->hwparams, SND_PCM_ACCESS_RW_INTERLEAVED);

	return err;
}

static void
sound_alsa_finish(ad_device_data *data)
{
	sound_alsa_data *sad = data;

	if (sad->hwparams)
		snd_pcm_hw_params_free(sad->hwparams);
	sad->hwparams = NULL;

	if (sad->handle) {
		snd_pcm_hw_free(sad->handle);
		snd_pcm_close(sad->handle);
	}
	sad->handle = NULL;

	ALSA_DEBUG("audio-device finished.\n");

	return;
}

static ad_device_data *
sound_alsa_create(Lisp_Object alsa_options)
{
	/* result */
	sound_alsa_data *sad = NULL;
	int keep_open = 0;
	int err = 0;
	/* option keywords */
	Lisp_Object opt_device;
	Lisp_Object opt_keepopen;

	/* parse options */
	opt_device = Fplist_get(alsa_options, Q_device, Qnil);
	if (!NILP(opt_device) && !STRINGP(opt_device)) {
		wrong_type_argument(Qstringp, opt_device);
		return NULL;
	}

	opt_keepopen = Fplist_get(alsa_options, Q_keep_open, Qnil);
	if (!NILP(opt_keepopen))
		keep_open = 1;

	/* initialise and fill */
	sad = xnew_and_zero(sound_alsa_data);
	sad->device = opt_device;
	sad->keep_open = keep_open;

	/* Open the device */
	if ((err = sound_alsa_open_device(sad)) < 0) {
		xfree(sad);
		error(GETTEXT("audio-alsa: "
			      "Opening ALSA device failed: %s."),
			snd_strerror(err));
		return NULL;
	}

	snd_pcm_hw_params_malloc(&sad->hwparams);

	if ((err = sound_alsa_init_hardware(sad)) < 0) {
		sound_alsa_finish(sad);
		xfree(sad);
		error(GETTEXT("Error: audio-alsa: "
			      "Opening ALSA device failed: %s."),
		      snd_strerror(err));
		return NULL;
	}

	if (!keep_open) {
		snd_pcm_hw_free(sad->handle);
		snd_pcm_close(sad->handle);
		sad->handle = NULL;
	}

	return (ad_device_data*)sad;
}


static int
sound_alsa_prepare_device(sound_alsa_data *sad, sound_alsa_subthread_data *sasd)
{
	/* alsa stuff */
	snd_pcm_state_t state;
	snd_pcm_format_t tmpfmt[] = {
		SND_PCM_FORMAT_S32, SND_PCM_FORMAT_S24,
		SND_PCM_FORMAT_S16,
		SND_PCM_FORMAT_FLOAT,
		SND_PCM_FORMAT_U8,
		SND_PCM_FORMAT_UNKNOWN };
	int err = 0, i, num_tmpfmt = sizeof(tmpfmt)/sizeof(snd_pcm_format_t);

	sasd->format = SND_PCM_FORMAT_UNKNOWN;
	sasd->channels = 0;

	switch (sasd->mtap->channels) {
	case 1:
		if (snd_pcm_hw_params_test_channels(
			    sad->handle, sad->hwparams, 1) == 0) {
			sasd->channels = 1;
			ALSA_DEBUG_HW("Using MONO.\n");
		} else if (!snd_pcm_hw_params_test_channels(
				   sad->handle, sad->hwparams, 2)) {
			sasd->channels = 2;
			ADD_MEDIA_SAMPLE_EFFECT(
				sasd->coe_chain, sasd->coe_ch_cnt,
				MEDIA_SAMPLE_EFFECT(sxe_mse_1ch_to_2ch), NULL);
				ALSA_DEBUG_HW("MONO->STEREO coerce.\n");
		}
		break;
	case 2:
		if (snd_pcm_hw_params_test_channels(
			    sad->handle, sad->hwparams, 2) == 0) {
			sasd->channels = 2;
			ALSA_DEBUG_HW("Using STEREO.\n");
		} else if (!snd_pcm_hw_params_test_channels(
				   sad->handle, sad->hwparams, 1)) {
			sasd->channels = 1;
			ADD_MEDIA_SAMPLE_EFFECT(
				sasd->coe_chain, sasd->coe_ch_cnt,
				MEDIA_SAMPLE_EFFECT(sxe_mse_2ch_to_1ch), NULL);
			ALSA_DEBUG_HW("STEREO->MONO coerce.\n");
		}
		break;

		/* more channels here */
	}

	if (sasd->channels == 0 ||
	    (err = snd_pcm_hw_params_set_channels(
		     sad->handle, sad->hwparams, sasd->channels)) < 0) {
		message(GETTEXT("audio-alsa: "
				"Setting channels failed: %s."),
			snd_strerror(err));
		return -1;
	}

	/* now we try to set a useful format */
	ALSA_DEBUG("trying %d formats\n", num_tmpfmt);
	i = 0;
	while (i < num_tmpfmt &&
	       (snd_pcm_hw_params_test_format(
			 sad->handle, sad->hwparams, tmpfmt[i])))
		i++;
	if (i == num_tmpfmt) {
		ALSA_DEBUG_HW("Setting sample format failed :(.\n");
		/* warning? */
		message(GETTEXT("audio-alsa: "
				"Setting sample format failed."));
		return -1;
	}

	switch ((sasd->format = tmpfmt[i])) {
	case SND_PCM_FORMAT_U8:
		sasd->msf = sxe_msf_U8;
		sasd->framesize = sasd->channels * sizeof(uint8_t);
		ALSA_DEBUG_HW("Using U8.\n");
		break;
	case SND_PCM_FORMAT_S16:
		sasd->msf = sxe_msf_S16;
		sasd->framesize = sasd->channels * sizeof(int16_t);
		ALSA_DEBUG_HW("Using S16.\n");
		break;
	case SND_PCM_FORMAT_S24:
		sasd->msf = sxe_msf_S24;
		sasd->framesize = sasd->channels * sizeof(int32_t);
		ALSA_DEBUG_HW("Using S24.\n");
		break;
	case SND_PCM_FORMAT_S32:
		sasd->msf = sxe_msf_S32;
		sasd->framesize = sasd->channels * sizeof(int32_t);
		ALSA_DEBUG_HW("Using S32.\n");
		break;
	case SND_PCM_FORMAT_FLOAT:
		sasd->msf = sxe_msf_FLT;
		sasd->framesize = sasd->channels * sizeof(float);
		ALSA_DEBUG_HW("Using FLT.\n");
		break;
	default:
		break;
	}

	/* now set the format */
	if ((err = snd_pcm_hw_params_set_format(
		     sad->handle, sad->hwparams, sasd->format)) < 0) {
		ALSA_DEBUG_HW("Setting sample format failed: %s.\n",
			      snd_strerror(err));
		/* warning? */
		message(GETTEXT("audio-alsa: "
				"Setting sample format failed: %s."),
			snd_strerror(err));
		return -1;
	}

	sasd->samplerate = sasd->mtap->samplerate;
	if ((err = snd_pcm_hw_params_set_rate_near(
		     sad->handle, sad->hwparams, &sasd->samplerate, 0)) < 0) {
		ALSA_DEBUG_HW("Setting sample rate failed: %s.\n",
			      snd_strerror(err));
		/* warning? */
		message(GETTEXT("audio-alsa: "
				"Setting sample rate failed: %s."),
			snd_strerror(err));
		return -1;
	}
	/* we could feed the coerce chain with the rerate module */
	if (sasd->samplerate != sasd->mtap->samplerate)
		ALSA_DEBUG_HW("had to adapt samplerate, old: %d, new: %d.\n",
			      sasd->mtap->samplerate, sasd->samplerate);

	ALSA_DEBUG_HW("Using samplerate: %d.\n", sasd->samplerate);

	/* now set all the params */
	if ((err = snd_pcm_hw_params(sad->handle, sad->hwparams)) < 0) {
		message(GETTEXT("audio-alsa: "
				"Setting parameters failed: %s."),
			snd_strerror(err));
		return -1;
	}

	if ((err = snd_pcm_prepare(sad->handle)) < 0) {
		message(GETTEXT("audio-alsa: "
				"Cannot prepare ALSA device: %s."),
			snd_strerror(err));
		return -1;
	}

	if ((state = snd_pcm_state(sad->handle)) != SND_PCM_STATE_PREPARED) {
		message(GETTEXT("audio-alsa: "
				"Cannot prepare ALSA device."));
		return -1;
	}

	return 1;
}

static int
sound_alsa_close_device(sound_alsa_data *sad)
{
	sad->lock = 0;
	if (sad->handle == NULL)
		return 0;

	if (sad->keep_open)
		return 0;

	/* close /dev/dsp */
	snd_pcm_hw_free(sad->handle);
	snd_pcm_close(sad->handle);

	ALSA_DEBUG_HW("device handle closed\n");
	sad->handle = NULL;

	return 0;
}

static int
sound_alsa_play(media_subthread *mst)
{
	/* stream stuff */
	Lisp_Media_Stream *ms;
	media_substream *mss;
	Lisp_Media_Thread *mt;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_alsa_data *sad = NULL;
	/* buffering */
	size_t len = 0, tmplen = 0;
	sxe_media_sample_t *tmpbuf;
	int err = 0, i, resolution;
	/* subthread stuff */
	sound_alsa_subthread_data _sasd, *sasd = &_sasd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;

	SOUND_UNPACK_MT(mt, mst, device, ms, mss, lad, sad, sasd->mtap);

	if (sad->lock) {
		message(GETTEXT("audio-alsa: "
				"Device locked."));
		/* this lock is unnecessary, we _could_ write concurrently
		   provided that the concurrent media streams have set the ALSA
		   device to the exact same hardware parameters.
		   In cleartext this means, we have to convert all sample data
		   to a common format, e.g. 48000Hz/STEREO/FLT or the like.
		   However, I'm tired at the moment so I leave this lock here.
		*/
		return 0;
	}

	sad->lock = 1;

	/* trigger alsa, the device name should be an option */
	if ((err = sound_alsa_open_device(sad)) < 0) {
		ALSA_DEBUG_HW("Opening ALSA device failed: %s.",
			      snd_strerror(err));
		sad->handle = NULL;
		/* warning? */
		message(GETTEXT("audio-alsa: "
				"Opening ALSA device failed: %s."),
			snd_strerror(err));
		sound_alsa_close_device(sad);
		return 0;
	}

	if ((err = sound_alsa_init_hardware(sad)) < 0) {
		ALSA_DEBUG_HW("Device not configurable: %s.\n",
			      snd_strerror(err));
		/* warning? */
		message(GETTEXT("audio-alsa: "
				"Cannot access ALSA device: %s."),
			snd_strerror(err));
		sound_alsa_close_device(sad);
		return 0;
	}

	/* init the sasd */
	sasd->samplerate = sasd->channels = 0;
	sasd->framesize = 0;
	sasd->coe_ch_cnt = 0;

	if ((err = sound_alsa_prepare_device(sad, sasd)) < 0) {
		sound_alsa_close_device(sad);
		return 0;
	}

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sasd->coe_chain, sasd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = sasd->channels;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sasd->coe_chain, sasd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = sasd->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;

	ALSA_DEBUG_COE("have %d coerce functions in my chain.\n",
		       sasd->coe_ch_cnt);

	/* rewind it ... */
	media_stream_meth(ms, rewind)(mss);

	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* ... and play it */
	mst->buffer = xmalloc(SOUND_MAX_AUDIO_FRAME_SIZE);
	resolution = (sasd->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	tmpbuf = (sxe_media_sample_t*)mst->buffer;

	while (mt->play_state != MTPSTATE_STOP) {

		mtp = mt->play_state;
		switch (mtp) {
		case MTPSTATE_RUN:
			len = media_stream_meth(ms, read)(
				mss, mst->buffer, resolution);
			if (!len) {
				ALSA_DEBUG_S("finished\n");
				mt->play_state = MTPSTATE_STOP;
				break;
			}

			/* set up the volume args */
			volargs->volume[0] = volargs->volume[1] =
				mst->up->volume;
			/* set up the rerate args */
			rrargs->tweak = mst->up->ratetrafo;

			/* coerce the stuff, tmplen is in samples */
			tmplen = sasd->channels*len;
			for (i = 0; i < sasd->coe_ch_cnt; i++) {
				ALSA_DEBUG_COE("calling coerce "
					       "%d on b:0x%x l:%d\n",
					       i, (unsigned int)tmpbuf, tmplen);
				tmplen = CALL_MEDIA_SAMPLE_EFFECT(
					sasd->coe_chain, i,
					tmpbuf, tmpbuf, tmplen);
			}

			/* bring back to S16 or U8 */
			MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(sasd->msf)(
				mst->buffer, mst->buffer, tmplen);

			snd_pcm_writei(sad->handle, mst->buffer, len);
			break;
		case MTPSTATE_PAUSE:
			ALSA_DEBUG("sleeping for %d\n", resolution);
			memset(mst->buffer, 0, resolution*sasd->framesize);
			snd_pcm_writei(sad->handle, mst->buffer,
				       resolution);
			break;
		default:
			ALSA_DEBUG("ACK, quit\n");
			mt->play_state = MTPSTATE_STOP;
			break;
		}
	}

#if 0
	snd_pcm_drain(ad);
#endif
	mt->state = MTSTATE_FINISHED;

	/* -- Close and shutdown -- */
	if (mst->buffer)
		xfree(mst->buffer);
	mst->buffer = NULL;

	sound_alsa_close_device(sad);

	return 1;
}

#undef MYSELF

/* sound-alsa.c ends here */
