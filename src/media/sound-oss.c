/** sound-oss.c - play a sound file on using the (deprecated) OSS
 **
 ** Copyright (C) 2006 Sebastian Freundt
 **
 ** Copyright (C) 1995,96 by Markus Gutschke (gutschk@math.uni-muenster.de)
 ** This is version 1.3 of linuxplay.c, with platform-independent functions
 ** moved to a different file by Robert Bihlmeyer <robbe@orcus.priv.at>.
 **
 ** Parts of this code were inspired by sunplay.c, which is copyright 1989 by
 ** Jef Poskanzer and 1991,92 by Jamie Zawinski; c.f. sunplay.c for further
 ** information.
 **
 ** Permission to use, copy, modify, and distribute this software and its
 ** documentation for any purpose and without fee is hereby granted, provided
 ** that the above copyright notice appear in all copies and that both that
 ** copyright notice and this permission notice appear in supporting
 ** documentation.  This software is provided "as is" without express or
 ** implied warranty.
 **
 */

/* Synched up with: Not in FSF. */

/* XEmacs beta testers say:  undef this by default. */
#undef NOVOLUMECTRLFORMULAW	/* Changing the volume for uLaw-encoded
				   samples sounds very poor; possibly,
				   this is true only for the PC-Snd
				   driver, so undefine this symbol at your
				   discretion */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include "lisp.h"
#include "sysfile.h"

#include "media.h"
#include "sound-oss.h"

Lisp_Object Qoss;		/* cannot be Qnative */

#define MYSELF ADRIVER_OSS

#define __OSS_DEBUG__(args...)		fprintf(stderr, "OSS " args)
#ifndef OSS_DEBUG_FLAG
#define OSS_DEBUG(args...)
#else
#define OSS_DEBUG(args...)		__OSS_DEBUG__(args)
#endif
#define OSS_DEBUG_HW(args...)		OSS_DEBUG("[hardware]: " args)
#define OSS_DEBUG_S(args...)		OSS_DEBUG("[stream]: " args)
#define OSS_DEBUG_COE(args...)		OSS_DEBUG("[coerce]: " args)
#define OSS_DEBUG_AJ(args...)		OSS_DEBUG("[audio-job]: " args)
#define OSS_CRITICAL(args...)		__OSS_DEBUG__("CRITICAL: " args)

#define audio_dev "/dev/dsp"


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_oss);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_oss);


static Lisp_Object
sound_oss_mark(ad_device_data *devdata)
{
	sound_oss_data_t *sod = devdata;

	mark_object(sod->device);

	return Qnil;
}

static void
sound_oss_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_oss_data_t *sod = NULL;

	sod = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sod == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :device ", pcfun);
	if (NILP(sod->device))
		write_c_string("\"/dev/dsp\"", pcfun);
	else
		print_internal(sod->device, pcfun, ef);

	if (sod->lock) {
		write_c_string(" :busy t", pcfun);
	} else
		write_c_string(" :busy nil", pcfun);

	if (sod->keep_open) {
		write_c_string(" :keep-open t", pcfun);
	} else
		write_c_string(" :keep-open nil", pcfun);

	return;
}


static int
sound_oss_open_device(sound_oss_data_t *sod)
{
	if (sod->device_fd < 0) {
		/* open /dev/dsp */
		if ((sod->device_fd = open(audio_dev, O_WRONLY, 0)) < 0)
			return sod->device_fd;
	}

	return 0;
}

static int
sound_oss_close_device(sound_oss_data_t *sod)
{
	sod->lock = 0;
	if (sod->device_fd < 0)
		return 0;

	if (sod->keep_open)
		return 0;

	/* close /dev/dsp */
	ioctl(sod->device_fd, SNDCTL_DSP_SYNC, NULL);
	ioctl(sod->device_fd, SNDCTL_DSP_RESET, NULL);

	OSS_DEBUG_HW("device file closed\n");
	close(sod->device_fd);
	sod->device_fd = -1;

	return 0;
}

static int
sound_oss_init_device(sound_oss_data_t *sod, sound_oss_aj_data_t *sosd)
{
	int tmp[] = {
		AFMT_S16_LE, AFMT_S16_BE, AFMT_U8,
		AFMT_QUERY };
	int i, num_tmp = sizeof(tmp)/sizeof(int), fd = sod->device_fd;

	if (ioctl(sod->device_fd, SNDCTL_DSP_SYNC, NULL) < 0) {
		OSS_DEBUG_HW("SNDCTL_DSP_SYNC failed.\n");
		return -1;
	}

	/* Initialize sound hardware with preferred parameters */

	/* try to set channels */
	sosd->channels = (sosd->mtap->channels) -1;
	if (ioctl(sod->device_fd, SNDCTL_DSP_STEREO, &sosd->channels) < 0) {
		OSS_DEBUG_HW("cannot set channels\n");
		return -1;
	}

	if (++sosd->channels != sosd->mtap->channels) {
		if (sosd->channels == 1) {
			/* mono, source is stereo */
			ADD_MEDIA_SAMPLE_EFFECT(
				sosd->coe_chain, sosd->coe_ch_cnt,
				MEDIA_SAMPLE_EFFECT(sxe_mse_2ch_to_1ch), NULL);
				OSS_DEBUG_COE("STEREO->MONO coerce.\n");
		} else if (sosd->channels == 2) {
			/* stereo, source is mono */
			ADD_MEDIA_SAMPLE_EFFECT(
				sosd->coe_chain, sosd->coe_ch_cnt,
				MEDIA_SAMPLE_EFFECT(sxe_mse_1ch_to_2ch), NULL);
				OSS_DEBUG_COE("MONO->STEREO coerce.\n");
		} else {
			/* bullshit */
			OSS_DEBUG_HW("Hardware supports %d channels, "
				     "source has %d channels.\n",
				     sosd->channels, sosd->mtap->channels);
			sosd->channels = 0;
			return -1;
		}
	}

	/* we try some sample formats */
	i = 0;
	OSS_DEBUG("trying %d formats\n", num_tmp);
	while (i < num_tmp && ioctl(fd, SNDCTL_DSP_SAMPLESIZE, &tmp[i]) < 0) {
		i++;
	}

	if (i == num_tmp) {
		OSS_DEBUG_HW("Your soundcard is bullshit.\n");
		return -1;
	}

	switch (tmp[i]) {
	case AFMT_U8:
		OSS_DEBUG_HW("Using U8.\n");
		sosd->msf = sxe_msf_U8;
		sosd->framesize = sosd->channels * sizeof(uint8_t);
		break;
        case AFMT_S16_LE:
	case AFMT_S16_BE:
		OSS_DEBUG_HW("Using S16.\n");
		sosd->msf = sxe_msf_S16;
		sosd->framesize = sosd->channels * sizeof(int16_t);
	        break;
	default:
		OSS_DEBUG_HW(".oO{ I must not be here }\n");
		sosd->framesize = 0;
		return -1;
	        break;
	}

	/* The PCSP driver does not support reading of the sampling rate via the
	   SOUND_PCM_READ_RATE ioctl; determine "the_speed" here */
	sosd->samplerate = sosd->mtap->samplerate;
	if (ioctl(sod->device_fd, SNDCTL_DSP_SPEED, &sosd->samplerate) < 0 ||
	    sosd->samplerate > 1.02 * sosd->mtap->samplerate ||
	    sosd->samplerate < 0.98 * sosd->mtap->samplerate) {
		OSS_DEBUG_HW("OSS cannot set rate: %d\n",
			     sosd->samplerate);
		/* actually we should use the rerate effect */
		return -1;
	}

#if 0				/* stupid mixer device */
	/* Use the mixer device for setting the playback volume */
	if (sod->device_fd > 0) {
		int vol = 100;
		vol |= 256 * 100;
		/* Do not signal an error, if volume control is unavailable! */
		ioctl(sod->device_fd, SOUND_MIXER_WRITE_PCM, &vol);
	}
#endif

	return 1;
}

static ad_device_data *
sound_oss_create(Lisp_Object oss_options)
{
	/* result */
	sound_oss_data_t *sod = NULL;
	int keep_open = 0;
	/* option keywords */
	Lisp_Object opt_device;
	Lisp_Object opt_keepopen;

	/* parse options */
	opt_device = Fplist_get(oss_options, Q_device, Qnil);
	if (!NILP(opt_device) && !STRINGP(opt_device)) {
		wrong_type_argument(Qstringp, opt_device);
		return NULL;
	}

	opt_keepopen = Fplist_get(oss_options, Q_keep_open, Qnil);
	if (!NILP(opt_keepopen))
		keep_open = 1;

	/* initialise and fill */
	sod = xnew_and_zero(sound_oss_data_t);
	sod->device = opt_device;
	sod->keep_open = keep_open;
	sod->device_fd = -1;
	SXE_MUTEX_INIT(&sod->mtx);

	/* Open the device */


	if (!keep_open) {
		sod->device_fd = -1;
	}

	return (ad_device_data*)sod;
}

static void
sound_oss_finish(ad_device_data *data)
{
	sound_oss_data_t *sod = data;

	sod->lock = 1;
	sod->keep_open = 0;
	sound_oss_close_device(sod);
	SXE_MUTEX_FINI(&sod->mtx);

	OSS_DEBUG("audio-device finished.\n");

	return;
}

#ifdef EF_USE_ASYNEQ
static inline void
sound_oss_change_volume(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->volume = args->volume_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_oss_change_rate(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->ratetrafo = args->rate_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_oss_change_state(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	switch (args->state_args) {
	case aj_pause:
		OSS_DEBUG_AJ("->pause state\n");
		aj->play_state = MTPSTATE_PAUSE;
		break;
	case aj_resume:
		OSS_DEBUG_AJ("->resume state\n");
		aj->play_state = MTPSTATE_RUN;
		break;
	case aj_start:
		OSS_DEBUG_AJ("->start state\n");
		break;
	case aj_stop:
		OSS_DEBUG_AJ("->stop state\n");
		aj->play_state = MTPSTATE_STOP;
		break;
	case no_audio_job_change_states:
	default:
		OSS_DEBUG_AJ("->unknown state\n");
		break;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_oss_handle_aj_events(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_oss_handle_aj_events(audio_job_t aj)
{
	sound_oss_aj_data_t *sasd;
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

	OSS_DEBUG_AJ("Event 0x%lx\n", (long unsigned int)ev);
	switch (audio_job_event_kind(ev)) {
	case aj_change_state:
		OSS_DEBUG_AJ("change state event\n");
		sound_oss_change_state(aj, &audio_job_event_args(ev));
		break;
	case aj_change_volume:
		OSS_DEBUG_AJ("change volume event\n");
		sound_oss_change_volume(aj, &audio_job_event_args(ev));
		break;
	case aj_change_rate:
		OSS_DEBUG_AJ("change rate event\n");
		sound_oss_change_rate(aj, &audio_job_event_args(ev));
		break;
	case no_audio_job_event_kinds:
	default:
		OSS_CRITICAL("unknown event\n");
		break;
	}
	free_audio_job_event(ev);
}
#endif	/* EF_USE_ASYNEQ */

static int
sound_oss_play(audio_job_t aj)
{
	/* stream stuff */
	Lisp_Media_Stream *ms;
	media_substream *mss;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_oss_data_t *sod = NULL;
	int fd;
	/* buffering */
	size_t len, tmplen;
	sxe_media_sample_t *tmpbuf;
	char *bptr = NULL;
	size_t natlen;
	int32_t written;
	int resolution, i;
	/* subthread stuff */
	sound_oss_aj_data_t _sosd, *sosd = &_sosd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;
	/* cache stuff */
	int alloced_myself = 0;

	SOUND_UNPACK_MT(aj, device, ms, mss, lad, sod, sosd->mtap);

	SXE_MUTEX_LOCK(&sod->mtx);
	if (sod->lock) {
		OSS_DEBUG_HW("Device locked.\n");
		message(GETTEXT("audio-oss: "
				"Device locked."));
		/* this lock is probably unnecessary */
		SXE_MUTEX_UNLOCK(&sod->mtx);
		return 0;
	}

	sod->lock = 1;

	/* okay, njsf said /dev/dsp writing is not mt safe,
	 * also i hate OSS, so let's block everything here :) -hroptatyr
	 */
#if defined(HAVE_THREADS) && 0
	pthread_mutex_lock(&mss->substream_mutex);
#endif

	if (sound_oss_open_device(sod) < 0) {
		OSS_DEBUG_HW("Opening device failed.\n");
		sod->device_fd = -1;
		/* warning? */
		message(GETTEXT("audio-oss: "
				"Opening OSS device failed."));
		sound_oss_close_device(sod);
		SXE_MUTEX_UNLOCK(&sod->mtx);
		return 0;
	}

	/* init the sosd */
	sosd->paused = sosd->volume = 0;
	sosd->samplerate = sosd->channels = 0;
	sosd->coe_ch_cnt = 0;

	if (sound_oss_init_device(sod, sosd) < 0) {
		OSS_DEBUG_HW("Device not configurable.\n");
		/* warning? */
		message(GETTEXT("audio-oss: "
				"Cannot access OSS device."));
		sound_oss_close_device(sod);
		SXE_MUTEX_UNLOCK(&sod->mtx);
		return 0;
	}

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sosd->coe_chain, sosd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = sosd->channels;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sosd->coe_chain, sosd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = sosd->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;

	OSS_DEBUG_COE("have %d coerce functions in my chain.\n",
		      sosd->coe_ch_cnt);

	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;
	SXE_MUTEX_UNLOCK(&sod->mtx);

	/* rewind the stream */
	media_stream_meth(ms, rewind)(mss);

	/* play chunks of the stream */
	SXE_MUTEX_LOCK(&aj->mtx);
	if (aj->buffer_alloc_size < SOUND_MAX_AUDIO_FRAME_SIZE) {
		alloced_myself = 1;
		aj->buffer = xmalloc_atomic(SOUND_MAX_AUDIO_FRAME_SIZE);
		aj->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	}
	tmpbuf = (sxe_media_sample_t*)aj->buffer;
	resolution = (sosd->mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	fd = sod->device_fd;
	natlen = 0;
	SXE_MUTEX_UNLOCK(&aj->mtx);

	while (aj->play_state != MTPSTATE_STOP) {

#ifdef EF_USE_ASYNEQ
		if (audio_job_queue(aj)) {
			sound_oss_handle_aj_events(aj);
		}
#endif

		SXE_MUTEX_LOCK(&aj->mtx);
		mtp = aj->play_state;
		SXE_MUTEX_UNLOCK(&aj->mtx);
		switch (mtp) {
		case MTPSTATE_RUN:
			if (natlen > 0)
				goto write_buf;

			/* otherwise we simply fetch a new bunch of samples */
			len = media_stream_meth(ms, read)(
				mss, aj->buffer, resolution);
			if (!len) {
				OSS_DEBUG_S("finished\n");
				SXE_MUTEX_LOCK(&aj->mtx);
				aj->play_state = MTPSTATE_STOP;
				SXE_MUTEX_UNLOCK(&aj->mtx);
				break;
			}
			/* set up the volume args */
			volargs->volume[0] = volargs->volume[1] =
				aj->volume;
			/* set up the rerate args */
			rrargs->tweak = aj->ratetrafo;

			/* coerce the stuff */
			tmplen = sosd->channels*len;
			for (i = 0; i < sosd->coe_ch_cnt; i++) {
				OSS_DEBUG_COE("calling coerce "
					      "%d on b:0x%x l:%d\n",
					      i, (unsigned int)tmpbuf, tmplen);
				tmplen = CALL_MEDIA_SAMPLE_EFFECT(
					sosd->coe_chain, i,
					tmpbuf, tmpbuf, tmplen);
			}

			/* bring back to S16 or U8 */
			MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(sosd->msf)(
				aj->buffer, aj->buffer, tmplen);

			/* convert tmplen back to number of frames */
			natlen = tmplen * sosd->framesize / sosd->channels;
			bptr = aj->buffer;

		write_buf:
			OSS_DEBUG_S("remaining cruft: %d bytes\n", natlen);
			if ((written = write(fd, bptr, natlen)) < 0) {
				OSS_DEBUG_S("ERROR in write()\n");
				natlen = 0;
			} else if (written) {
				natlen -= written;
				bptr += written;
			} else {
				natlen = 0;
				ioctl(fd, SNDCTL_DSP_SYNC, NULL);
			}
			break;
		case MTPSTATE_PAUSE:
			OSS_DEBUG("sleeping for %d\n", resolution);
			usleep(resolution);
			break;
		case MTPSTATE_UNKNOWN:
		case MTPSTATE_STOP:
		case NUMBER_OF_MEDIA_THREAD_PLAY_STATES:
		default:
			OSS_DEBUG("ACK, quit\n");
			SXE_MUTEX_LOCK(&aj->mtx);
			aj->play_state = MTPSTATE_STOP;
			SXE_MUTEX_UNLOCK(&aj->mtx);
			break;
		}
	}

	/* Now cleanup all used resources */
	bptr = NULL;
	SXE_MUTEX_LOCK(&aj->mtx);
	if (alloced_myself && aj->buffer) {
		xfree(aj->buffer);
	}
	aj->buffer = NULL;
	aj->buffer_alloc_size = 0;
	SXE_MUTEX_UNLOCK(&aj->mtx);

	sound_oss_close_device(sod);

#if defined(HAVE_THREADS) && 0
	pthread_mutex_unlock(&mss->substream_mutex);
#endif

	return 1;
}

#undef MYSELF
