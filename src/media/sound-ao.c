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
#include "sound-ao.h"

Lisp_Object Qao;

#define MYSELF ADRIVER_AO

#define __AO_DEBUG__(args...)		fprintf(stderr, "AO " args)
#ifndef AO_DEBUG_FLAG
#define AO_DEBUG(args...)
#else
#define AO_DEBUG(args...)		__AO_DEBUG__(args)
#endif
#define AO_DEBUG_C(args...)		AO_DEBUG("[connection]: " args)
#define AO_DEBUG_S(args...)		AO_DEBUG("[stream]: " args)
#define AO_DEBUG_COE(args...)		AO_DEBUG("[coerce]: " args)
#define AO_DEBUG_AJ(args...)		AO_DEBUG("[audio-job]: " args)
#define AO_CRITICAL(args...)		__AO_DEBUG__("CRITICAL: " args)


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_ao);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_ao);


static Lisp_Object
sound_ao_mark(ad_device_data *devdata)
{
	return Qnil;
}

static void
sound_ao_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_ao_data_t *saod = NULL;

	saod = get_audio_device_data(device);
	/* cannot use AO on incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || saod == NULL) {
		write_c_string("VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	} 

	/* info about the connected output plugin */
	write_c_string(" :driver \"", pcfun);
	write_c_string(ao_driver_info(saod->driver_id)->short_name, pcfun);
	write_c_string("\"", pcfun);

	if (ef);
	return;
}

static ad_device_data *
sound_ao_create(Lisp_Object ao_options)
{
	int driver;
	ao_device *device;
	ao_option *options;
	ao_sample_format *fmt;
	/* result */
	sound_ao_data_t *aod;
	/* option keywords */
	Lisp_Object opt_driver;
	char *optext_driver = NULL;

	/* parse options */
	opt_driver = Fplist_get(ao_options, intern(":driver"), Qnil);
	if (!NILP(opt_driver) && !STRINGP(opt_driver)) {
		wrong_type_argument(Qstringp, opt_driver);
		return NULL;
	} else if (STRINGP(opt_driver))
		optext_driver = (char*)XSTRING_DATA(opt_driver);

	/* -- initialise -- */
	ao_initialize();
	fmt = xmalloc(sizeof(ao_sample_format));

	/* -- Setup for driver -- */
	if (optext_driver != NULL)
		driver = ao_driver_id(optext_driver);
	else
		driver = ao_default_driver_id();

	/* just some generics */
	fmt->channels = 2;
	fmt->rate = 44100;
	fmt->bits = 16;
	fmt->byte_format = AO_FMT_LITTLE;

	options = NULL;

	/* -- Open driver -- */
	device = ao_open_live(driver, fmt, options);
	if (device == NULL) {
		message(GETTEXT("audio-ao: Unsupported driver."));
		xfree(fmt);
		aod = NULL;
	} else {
		aod = xnew_and_zero(sound_ao_data_t);

		aod->ad = device;
		aod->options = NULL;
		aod->fmt = fmt;
		aod->driver_id = driver;
	}

	return aod;
}

static void
sound_ao_finish(ad_device_data *data)
{
	sound_ao_data_t *ao_dev = data;
	if (ao_dev != NULL) {
		ao_close(ao_dev->ad);
		xfree(ao_dev->fmt);
		ao_shutdown();
	}
	return;
}


static int
ao_push(audio_job_t aj, int nframes)
{
	size_t len = 0, tmplen = 0;
	sxe_media_sample_t *tmpbuf;
	sound_ao_aj_data_t *sasd = audio_job_device_data(aj);
	int i;

	tmpbuf = (sxe_media_sample_t*)(aj->buffer);
	len = media_stream_meth(aj->substream->up, read)(
		aj->substream, aj->buffer, nframes);

	/* set up the volume args */
	sasd->volargs->volume[0] = sasd->volargs->volume[1] = aj->volume;
	/* set up the rerate args */
	sasd->rrargs->tweak = aj->ratetrafo;

	/* coerce the stuff */
	tmplen = sasd->mtap->channels*len;
	for (i = 0; i < sasd->coe_ch_cnt; i++) {
		AO_DEBUG_COE("calling coerce "
			      "%d on b:0x%x l:%d\n",
			      i, (unsigned int)tmpbuf, tmplen);
		tmplen = CALL_MEDIA_SAMPLE_EFFECT(
			sasd->coe_chain, i, tmpbuf, tmpbuf, tmplen);
	}
	/* bring back to S16 or U8 */
	MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(sasd->msf)(
		aj->buffer, aj->buffer, tmplen);

	ao_play(sasd->dev, aj->buffer,
		tmplen * sasd->framesize / sasd->mtap->channels);

	return tmplen;
}


#ifdef EF_USE_ASYNEQ
static inline void
sound_ao_change_volume(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->volume = args->volume_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_ao_change_rate(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->ratetrafo = args->rate_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_ao_change_state(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	switch (args->state_args) {
	case aj_pause:
		AO_DEBUG_AJ("->pause state\n");
		aj->play_state = MTPSTATE_PAUSE;
		break;
	case aj_resume:
		AO_DEBUG_AJ("->resume state\n");
		aj->play_state = MTPSTATE_RUN;
		break;
	case aj_start:
		AO_DEBUG_AJ("->start state\n");
		break;
	case aj_stop:
		AO_DEBUG_AJ("->stop state\n");
		aj->play_state = MTPSTATE_STOP;
		break;

	case no_audio_job_change_states:
	default:
		AO_DEBUG_AJ("->unknown state\n");
		break;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_ao_handle_aj_events(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_ao_handle_aj_events(audio_job_t aj)
{
	sound_ao_aj_data_t *sasd;
	audio_job_event_t ev = NULL;

#if 0
	assert(audio_job_queue(aj));
#endif

	SXE_MUTEX_LOCK(&aj->mtx);
	sasd = audio_job_device_data(aj);
	if ((ev = eq_noseeum_dequeue(audio_job_queue(aj))) == NULL) {
		SXE_MUTEX_UNLOCK(&aj->mtx);
		return;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);

	AO_DEBUG_AJ("Event 0x%lx\n", (long unsigned int)ev);
	switch (audio_job_event_kind(ev)) {
	case aj_change_state:
		AO_DEBUG_AJ("change state event\n");
		sound_ao_change_state(aj, &audio_job_event_args(ev));
		break;
	case aj_change_volume:
		AO_DEBUG_AJ("change volume event\n");
		sound_ao_change_volume(aj, &audio_job_event_args(ev));
		break;
	case aj_change_rate:
		AO_DEBUG_AJ("change rate event\n");
		sound_ao_change_rate(aj, &audio_job_event_args(ev));
		break;

	case no_audio_job_event_kinds:
	default:
		AO_CRITICAL("unknown event\n");
		break;
	}
	free_audio_job_event(ev);
}
#endif	/* EF_USE_ASYNEQ */
static int
sound_ao_play(audio_job_t aj)
{
	/* stream stuff */
	Lisp_Media_Stream *ms;
	media_substream *mss;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_ao_data_t *saod = NULL;
	/* libao stuff */
	ao_sample_format *format;
	ao_option *options;
	int driver;
	/* buffering */
	int resolution;
	/* subthread stuff */
	sound_ao_aj_data_t _sasd, *sasd = &_sasd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;
	/* cache stuff */
	int alloced_myself = 0;

	SOUND_UNPACK_MT(aj, device, ms, mss, lad, saod, sasd->mtap);

	/* setup for the driver */
	driver = saod->driver_id;
	format = saod->fmt;
	options = saod->options;

	/* setup format specs from audio props */
	format->channels = sasd->mtap->channels;
	format->rate = sasd->mtap->samplerate;
	format->bits = 16; /* HARDCODED, was: mtap->samplewidth; */
	format->byte_format = AO_FMT_LITTLE; /* hmpf */

	/* open the driver */
	sasd->dev = ao_open_live(driver, format, options);
	if (sasd->dev == NULL) {
		message(GETTEXT("audio: Unsupported device."));
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return 0;
	}

	/* fill the subthread data structure */
	sasd->msf = MEDIA_SAMPLE_FORMAT(sxe_msf_S16);
	sasd->framesize = sasd->mtap->channels * sizeof(int16_t);
	sasd->coe_ch_cnt = 0;
	audio_job_device_data(aj) = sasd;

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sasd->coe_chain, sasd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = sasd->mtap->channels;
	sasd->volargs = volargs;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sasd->coe_chain, sasd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = sasd->mtap->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;
	sasd->rrargs = rrargs;

	/* rewind it ... */
	media_stream_meth(ms, rewind)(mss);

	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* ... and play it */
	SXE_MUTEX_LOCK(&aj->mtx);
<<<<<<< HEAD
=======
	mtp = aj->play_state;
>>>>>>> master
	if (aj->buffer_alloc_size < SOUND_MAX_AUDIO_FRAME_SIZE) {
		alloced_myself = 1;
		aj->buffer = xmalloc_atomic(SOUND_MAX_AUDIO_FRAME_SIZE);
		aj->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
	resolution = (sasd->mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;

<<<<<<< HEAD
	while (aj->play_state != MTPSTATE_STOP) {
=======
	while (mtp != MTPSTATE_STOP) {
>>>>>>> master

#ifdef EF_USE_ASYNEQ
		/* events are there? */
		if (audio_job_queue(aj)) {
			sound_ao_handle_aj_events(aj);
		}
#endif

		SXE_MUTEX_LOCK(&aj->mtx);
<<<<<<< HEAD
		mtp = aj->play_state;
		SXE_MUTEX_UNLOCK(&aj->mtx);
		switch (mtp) {
=======
		switch (aj->play_state) {
>>>>>>> master
		case MTPSTATE_RUN:
			if (!ao_push(aj, resolution))
				aj->play_state = MTPSTATE_STOP;
			break;
		case MTPSTATE_PAUSE:
<<<<<<< HEAD
			AO_DEBUG("sleeping for %d\n", resolution);
			usleep(resolution);
			break;
=======
		  /* must sleep resolution outside of lock */
>>>>>>> master

		case MTPSTATE_UNKNOWN:
		case MTPSTATE_STOP:
		case NUMBER_OF_MEDIA_THREAD_PLAY_STATES:
		default:
			AO_DEBUG("ACK, quit\n");
<<<<<<< HEAD
			SXE_MUTEX_LOCK(&aj->mtx);
			aj->play_state = MTPSTATE_STOP;
			SXE_MUTEX_UNLOCK(&aj->mtx);
=======
			aj->play_state = MTPSTATE_STOP;
			break;
		}
		mtp = aj->play_state;
		SXE_MUTEX_UNLOCK(&aj->mtx);
		if (mtp == MTPSTATE_PAUSE) {
			AO_DEBUG("sleeping for %d\n", resolution);
			usleep(resolution);
>>>>>>> master
			break;
		}
	}

	/* -- Close and shutdown -- */
	SXE_MUTEX_LOCK(&aj->mtx);
	if (alloced_myself && aj->buffer) {
		xfree(aj->buffer);
	}
	aj->buffer = NULL;
	aj->buffer_alloc_size = 0;
	SXE_MUTEX_UNLOCK(&aj->mtx);

<<<<<<< HEAD
	if (sasd && sasd->dev) {
=======
	/* sasd is always != NULL here per its initialization */
	if (sasd->dev) {
>>>>>>> master
		ao_close(sasd->dev);
		sasd->dev = NULL;
	}
	sasd = NULL;

	return 1;
}

#undef MYSELF

/* sound-ao.c ends here */
