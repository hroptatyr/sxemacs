/* sound-esd.c - play a sound over ESD

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
#include "sound-esd.h"

/* the name given to ESD - I think this should identify ourselves */
#define ESD_NAME "SXEmacs"

#define MYSELF ADRIVER_ESD

Lisp_Object Qesd;

#define __ESD_DEBUG__(args...)		fprintf(stderr, "ESD " args)
#ifndef ESD_DEBUG_FLAG
#define ESD_DEBUG(args...)
#else
#define ESD_DEBUG(args...)		__ESD_DEBUG__(args)
#endif
#define ESD_DEBUG_HW(args...)		ESD_DEBUG("[hardware]: " args)
#define ESD_DEBUG_S(args...)		ESD_DEBUG("[stream]: " args)
#define ESD_DEBUG_COE(args...)		ESD_DEBUG("[coerce]: " args)
#define ESD_DEBUG_AJ(args...)		ESD_DEBUG("[audio-job]: " args)
#define ESD_CRITICAL(args...)		__ESD_DEBUG__("CRITICAL: " args)


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_esd);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_esd);


static Lisp_Object
sound_esd_mark(ad_device_data *devdata)
{
	sound_esd_data_t *sed = devdata;

	if (sed == NULL)
		return Qnil;

	mark_object(sed->server);

	return Qnil;
}

static void
sound_esd_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_esd_data_t *sed = NULL;

	sed = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sed == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :device ", pcfun);
	if (NILP(sed->server))
		write_c_string("#default", pcfun);
	else
		print_internal(sed->server, pcfun, ef);

	return;
}


static int
sound_esd_close_device(sound_esd_data_t *sed)
{
	sed->lock = 0;

	ESD_DEBUG_HW("socket closed\n");

	return 0;
}

static int
sound_esd_init_socket(sound_esd_data_t *sed, sound_esd_aj_data_t *sesd)
{
	/* convert header information into ESD flags */
	sesd->flags = 0;
	sesd->flags = ESD_STREAM | ESD_PLAY;
	sesd->flags |= ESD_BITS16;
	sesd->samplewidth = 16;
	sesd->channels = sesd->mtap->channels;
	sesd->framesize = sesd->channels * sizeof(int16_t);
	sesd->samplerate = sesd->mtap->samplerate;

	switch (sesd->mtap->channels) {
	case 1:
		sesd->flags |= ESD_MONO;
		break;
	case 2:
		sesd->flags |= ESD_STEREO;
		break;
	case 5:
		sesd->flags |= ESD_STEREO;
		ADD_MEDIA_SAMPLE_EFFECT(
			sesd->coe_chain, sesd->coe_ch_cnt,
			MEDIA_SAMPLE_EFFECT(sxe_mse_5ch_to_2ch), NULL);
		break;
	default:
		message(GETTEXT("audio-esd: "
				"%d channels - only 1 or 2 supported"),
			sesd->mtap->channels);
		return -1;
	}

	return 1;
}

static ad_device_data *
sound_esd_create(Lisp_Object esd_options)
{
	/* result */
	sound_esd_data_t *sed = NULL;
	/* option keywords */
	Lisp_Object opt_server;
	Lisp_Object opt_port;

	/* parse options */
	opt_server = Fplist_get(esd_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	}

	opt_port = Fplist_get(esd_options, intern(":port"), Qnil);
	if (!NILP(opt_port) && !NATNUMP(opt_port)) {
		wrong_type_argument(Qnatnump, opt_port);
		return NULL;
	}

	/* initialise and fill */
	sed = xnew_and_zero(sound_esd_data_t);
	sed->server = opt_server;

	return (ad_device_data*)sed;
}

static void
sound_esd_finish(ad_device_data *data)
{
	sound_esd_data_t *sed = data;

	sed->lock = 1;
	sed->keep_open = 0;
	sound_esd_close_device(sed);

	ESD_DEBUG("audio-device finished.\n");

	return;
}

#ifdef EF_USE_ASYNEQ
static inline void
sound_esd_change_volume(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->volume = args->volume_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_esd_change_rate(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->ratetrafo = args->rate_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_esd_change_state(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	switch (args->state_args) {
	case aj_pause:
		ESD_DEBUG_AJ("->pause state\n");
		aj->play_state = MTPSTATE_PAUSE;
		break;
	case aj_resume:
		ESD_DEBUG_AJ("->resume state\n");
		aj->play_state = MTPSTATE_RUN;
		break;
	case aj_start:
		ESD_DEBUG_AJ("->start state\n");
		break;
	case aj_stop:
		ESD_DEBUG_AJ("->stop state\n");
		aj->play_state = MTPSTATE_STOP;
		break;
	case no_audio_job_change_states:
	default:
		ESD_DEBUG_AJ("->unknown state\n");
		break;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_esd_handle_aj_events(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_esd_handle_aj_events(audio_job_t aj)
{
	sound_esd_aj_data_t *sasd;
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

	ESD_DEBUG_AJ("Event 0x%lx\n", (long unsigned int)ev);
	switch (audio_job_event_kind(ev)) {
	case aj_change_state:
		ESD_DEBUG_AJ("change state event\n");
		sound_esd_change_state(aj, &audio_job_event_args(ev));
		break;
	case aj_change_volume:
		ESD_DEBUG_AJ("change volume event\n");
		sound_esd_change_volume(aj, &audio_job_event_args(ev));
		break;
	case aj_change_rate:
		ESD_DEBUG_AJ("change rate event\n");
		sound_esd_change_rate(aj, &audio_job_event_args(ev));
		break;

	case no_audio_job_event_kinds:
	default:
		ESD_CRITICAL("unknown event\n");
		break;
	}
	free_audio_job_event(ev);
}
#endif	/* EF_USE_ASYNEQ */

static int
sound_esd_play(audio_job_t aj)
{
	/* stream stuff */
	Lisp_Media_Stream *ms;
	media_substream *mss;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_esd_data_t *sed = NULL;
	/* buffering */
	int i;
	size_t len, tmplen;
	sxe_media_sample_t *tmpbuf;
	/* esd socket stuff */
	ssize_t wrtn;
	int resolution;
	char *hoststr = NULL;
	/* subthread stuff */
	sound_esd_aj_data_t _sesd, *sesd = &_sesd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;
	/* cache stuff */
	int alloced_myself = 0;

	/* unpack the media thread */
	SOUND_UNPACK_MT(aj, device, ms, mss, lad, sed, sesd->mtap);

	/* this lock is totally unnecessary */
	sed->lock = 1;

	/* init the sesd */
	sesd->samplerate = sesd->samplewidth = sesd->channels = 0;
	sesd->framesize = 0;
	sesd->coe_ch_cnt = 0;

	/* init the socket */
	sound_esd_init_socket(sed, sesd);

	/* hm, use the given options */
	hoststr = (NILP(sed->server) ? NULL : (char*)XSTRING_DATA(sed->server));
	sesd->sock = esd_play_stream(
		sesd->flags, sesd->samplerate, hoststr, "SXEmacs");
	if (sesd->sock < 0)
		return 0;

	/* rewind the stream */
	media_stream_meth(ms, rewind)(mss);

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sesd->coe_chain, sesd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = sesd->channels;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sesd->coe_chain, sesd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = sesd->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;

	ESD_DEBUG_COE("have %d coerce functions in my chain.\n",
		      sesd->coe_ch_cnt);

	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* play chunks of the stream */
	SXE_MUTEX_LOCK(&aj->mtx);
	if (aj->buffer_alloc_size < SOUND_MAX_AUDIO_FRAME_SIZE) {
		alloced_myself = 1;
		aj->buffer = xmalloc_atomic(SOUND_MAX_AUDIO_FRAME_SIZE);
		aj->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
	tmpbuf = (sxe_media_sample_t*)aj->buffer;
	resolution = (sesd->mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;

	while (aj->play_state != MTPSTATE_STOP) {

#ifdef EF_USE_ASYNEQ
		/* handle job events */
		if (audio_job_queue(aj)) {
			sound_esd_handle_aj_events(aj);
		}
#endif

		SXE_MUTEX_LOCK(&aj->mtx);
		mtp = aj->play_state;
		SXE_MUTEX_UNLOCK(&aj->mtx);
		switch (mtp) {
		case MTPSTATE_RUN:
			len = media_stream_meth(ms, read)(
				mss, aj->buffer, resolution);
			if (!len) {
				ESD_DEBUG_S("finished\n");
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

			/* coerce the stuff,
			   tmplen becomes the number of samples */
			tmplen = sesd->channels*len;
			for (i = 0; i < sesd->coe_ch_cnt; i++) {
				ESD_DEBUG_COE("calling coerce "
					      "%d on b:0x%x l:%d\n",
					      i, (unsigned int)tmpbuf, tmplen);
				tmplen = CALL_MEDIA_SAMPLE_EFFECT(
					sesd->coe_chain, i,
					tmpbuf, tmpbuf, tmplen);
			}

			/* bring back to S16 */
			MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(sxe_msf_S16)(
				aj->buffer, aj->buffer, tmplen);

			wrtn = write(
				sesd->sock, aj->buffer,
				tmplen*sizeof(int16_t));
			if (wrtn < 0) {
				ESD_DEBUG_S("writing to socket failed: %d.\n",
					    wrtn);
				SXE_MUTEX_LOCK(&aj->mtx);
				aj->play_state = MTPSTATE_STOP;
				SXE_MUTEX_UNLOCK(&aj->mtx);
			}
			break;
		case MTPSTATE_PAUSE:
			ESD_DEBUG("sleeping for %d\n", resolution);
			usleep(resolution);
			break;

		case MTPSTATE_UNKNOWN:
		case MTPSTATE_STOP:
		case NUMBER_OF_MEDIA_THREAD_PLAY_STATES:
		default:
			ESD_DEBUG("ACK, quit\n");
			SXE_MUTEX_LOCK(&aj->mtx);
			aj->play_state = MTPSTATE_STOP;
			SXE_MUTEX_UNLOCK(&aj->mtx);
			break;
		}
	}

	/* close and shutdown */
	SXE_MUTEX_LOCK(&aj->mtx);
	if (alloced_myself && aj->buffer) {
		xfree(aj->buffer);
	}
	aj->buffer = NULL;
	aj->buffer_alloc_size = 0;
	SXE_MUTEX_UNLOCK(&aj->mtx);

	sound_esd_close_device(sed);
	/* we better close the socket now */
	close(sesd->sock);

	return 1;
}

#undef MYSELF

/* sound-esd.c ends here */
