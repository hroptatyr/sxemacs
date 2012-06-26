/* sound-pulse.c - play a sound over the Pulse Audio Server

  Copyright (C) 2006, 2007, 2008 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

  * This file is part of SXEmacs.
  *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  *
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  *
  * 2. Redistributions in binary form must reproduce the above copyright
  *    notice, this list of conditions and the following disclaimer in the
  *    documentation and/or other materials provided with the distribution.
  *
  * 3. Neither the name of the author nor the names of any contributors
  *    may be used to endorse or promote products derived from this
  *    software without specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
  * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
  * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
  * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  */

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
#include "sound.h"
#include "sound-pulse.h"

Lisp_Object Qpulse;

#define MYSELF ADRIVER_PULSE

#define __PULSE_DEBUG__(args...)	fprintf(stderr, "PULSE " args)
#ifndef PULSE_DEBUG_FLAG
#define PULSE_DEBUG(args...)
#else
#define PULSE_DEBUG(args...)		__PULSE_DEBUG__(args)
#endif
#define PULSE_DEBUG_CTX(args...)	PULSE_DEBUG("[connection]: " args)
#define PULSE_DEBUG_ML(args...)		PULSE_DEBUG("[mainloop]: " args)
#define PULSE_DEBUG_MX(args...)		PULSE_DEBUG("[semaphore]: " args)
#define PULSE_DEBUG_S(args...)		PULSE_DEBUG("[stream]: " args)
#define PULSE_DEBUG_PT(args...)		PULSE_DEBUG("[pthread]: " args)
#define PULSE_DEBUG_COE(args...)	PULSE_DEBUG("[coerce]: " args)
#define PULSE_DEBUG_AJ(args...)		PULSE_DEBUG("[audio-job]: " args)
#define PULSE_DEBUG_CACHE(args...)	PULSE_DEBUG("[cache]: " args)
#define PULSE_CRITICAL(args...)		__PULSE_DEBUG__("CRITICAL: " args)


static int sound_pulse_init_mainloop(ad_device_data*);
static void
time_ev_cb(pa_mainloop_api*, pa_time_event*, const struct timeval*, void*);

struct sound_pulse_data_s {
	Lisp_Object stream; /* media has to deal with this actually */
	Lisp_Object client;
	Lisp_Object role;
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
	struct sxe_semaphore_s ctxsem;
};

struct sound_pulse_aj_data_s {
	int volume;
	pa_cvolume chanvol;
	struct pa_sample_spec sampspec;
	struct pa_channel_map chanmap;
	pa_buffer_attr buffattr;
	pa_stream *stream;
	pa_context *ctx;

	/* event trickery */
	pa_time_event *time_ev;

	/* coercion stuff */
	media_sample_format_t *msf;
	int coe_ch_cnt;
	audio_coerce_chain_t coe_chain[4];
	sxe_mse_rerate_args *rrargs;
};


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_pulse);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_pulse);


static Lisp_Object
sound_pulse_mark(ad_device_data *devdata)
{
	sound_pulse_data_t spd = (sound_pulse_data_t)devdata;
	if (spd == NULL)
		return Qnil;

	mark_object(spd->stream);
	mark_object(spd->client);
	mark_object(spd->sink);
	mark_object(spd->source);
	mark_object(spd->server);
	return Qnil;
}

static void
sound_pulse_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_pulse_data_t spd = NULL;
	pa_context_state_t st;

	spd = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || spd == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :server ", pcfun);
	if (NILP(spd->server))
		write_c_string("#default", pcfun);
	else
		print_internal(spd->server, pcfun, ef);

	write_c_string(" :sink ", pcfun);
	if (NILP(spd->sink)) {
		write_c_string("#default", pcfun);
	} else {
		print_internal(spd->sink, pcfun, ef);
	}

#if 0
	if (spd->sink_info) {
		write_c_string(" (", pcfun);
		write_c_string(spd->sink_info->description, pcfun);
		write_c_string(")", pcfun);
	} else {
		write_c_string(" (sink does not exist)", pcfun);
	}
#endif

	write_c_string(" :source ", pcfun);
	if (NILP(spd->source))
		write_c_string("#default", pcfun);
	else
		print_internal(spd->source, pcfun, ef);

	write_c_string(" :server-state ", pcfun);

	if (!spd->ctx) {
		write_c_string("#b0rked", pcfun);
		return;
	}

	st = pa_context_get_state(spd->ctx);
	switch ((unsigned int)st) {
	case PA_CONTEXT_CONNECTING:
	case PA_CONTEXT_AUTHORIZING:
	case PA_CONTEXT_SETTING_NAME:
		write_c_string("#busy", pcfun);
		break;

	case PA_CONTEXT_READY:
		write_c_string("#connected", pcfun);
		break;

	case PA_CONTEXT_TERMINATED:
		write_c_string("#terminated", pcfun);
		break;

	case PA_CONTEXT_FAILED:
	default:
		write_c_string("#failed", pcfun);
		break;
	}

	write_c_string(" :api ", pcfun);
	if (spd->ml_threaded_p) {
		write_fmt_str(pcfun, "#threaded :mainloop 0x%lx",
				 (long unsigned int)spd->tml);
	} else {
		write_fmt_str(pcfun, "#non-threaded :mainloop 0x%lx",
			      (long unsigned int)spd->ml);
	}
	return;
}


static ad_device_data *
sound_pulse_create(Lisp_Object pulse_options)
{
	/* result */
	sound_pulse_data_t spd = NULL;
	/* option keywords */
	Lisp_Object opt_server = Qnil;
	Lisp_Object opt_sink = Qnil;
	Lisp_Object opt_source = Qnil;
	Lisp_Object opt_client = Qnil;
	Lisp_Object opt_role = Qnil;
	Lisp_Object opt_stream = Qnil;
	Lisp_Object opt_immediate = Qnil;
	Lisp_Object opt_threaded = Qnil;
	Lisp_Object opt_force = Qnil;

	/* parse options */
	opt_server = Fplist_get(pulse_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	}

	opt_sink = Fplist_get(pulse_options, intern(":sink"), Qnil);
	if (!NILP(opt_sink) && !STRINGP(opt_sink)) {
		wrong_type_argument(Qstringp, opt_sink);
		return NULL;
	}

	opt_client = Fplist_get(pulse_options, intern(":client"), Qnil);
	if (!NILP(opt_client) && !STRINGP(opt_client)) {
		wrong_type_argument(Qstringp, opt_client);
		return NULL;
	}

	opt_role = Fplist_get(pulse_options, intern(":role"), Qnil);
	if (!NILP(opt_role) && !STRINGP(opt_role)) {
		wrong_type_argument(Qstringp, opt_role);
		return NULL;
	}

	opt_stream = Fplist_get(pulse_options, intern(":stream"), Qnil);
	if (!NILP(opt_stream) && !STRINGP(opt_stream)) {
		wrong_type_argument(Qstringp, opt_stream);
		return NULL;
	}

	opt_immediate = Fplist_get(pulse_options, intern(":immediate"), Qt);
	opt_threaded = Fplist_get(pulse_options, intern(":threaded"), Qt);
	opt_force = Fplist_get(pulse_options, intern(":force"), Qnil);

	/* initialise and fill */
	spd = xnew_and_zero(struct sound_pulse_data_s);
	spd->server = opt_server;
	spd->client = opt_client;
	spd->role = opt_role;
	spd->sink = opt_sink;
	spd->source = opt_source;
	spd->stream = opt_stream;

	spd->ml_threaded_p = !NILP(opt_threaded);

	if (NILP(opt_immediate))
		return (ad_device_data*)spd;

	sound_pulse_init_mainloop(spd);

	if (spd->ml_running_p == 0) /* yeah, finish immediately */
		sound_pulse_finish(spd);

	if (NILP(opt_force) && spd->ml_running_p == 0) {
		xfree(spd);
		spd = NULL;
		error(GETTEXT("Error: audio-pulse: "
			      "Initialising connection to PulseAudio failed."));
		return NULL;
	}

	return (ad_device_data*)spd;
}

static void
sound_pulse_finish(ad_device_data *data)
{
	sound_pulse_data_t spd = data;
	if (spd == NULL)
		return;

	if (spd != NULL && 0) {
		spd->stream = Qnil;
		spd->server = Qnil;
		spd->client = Qnil;
		spd->role = Qnil;
		spd->sink = Qnil;
		spd->source = Qnil;
	}

	spd->ml_running_p = 0;

	/* setting this to 0 is not enough? :\ */
	if (spd->ctx) {
		PULSE_DEBUG_CTX("DRAIN.\n");
		pa_context_drain(spd->ctx, NULL, NULL);
		PULSE_DEBUG_CTX("STOP.\n");
		pa_context_disconnect(spd->ctx);
		pa_context_unref(spd->ctx);
		SXE_SEMAPH_FINI(&(spd->ctxsem));
	}
	spd->ctx = NULL;

	PULSE_DEBUG_ML("Is there a mainloop awake?\n");
	if (spd->ml_threaded_p) {
		if (spd->tml) {
			PULSE_DEBUG_ML("STOP.\n");
			pa_threaded_mainloop_stop(spd->tml);
			PULSE_DEBUG_ML("FREE. bye bye\n");
			pa_threaded_mainloop_free(spd->tml);
		}
		spd->tml = NULL;
	} else {
		if (spd->ml) {
			;
			PULSE_DEBUG_ML("FREE. bye bye (non-threaded)\n");
			pa_mainloop_free(spd->ml);
		}
		spd->ml = NULL;
	}

	PULSE_DEBUG_CTX("finish.\n");

	spd->tml = NULL;
	spd->ml = NULL;
	return;
}


/** Wait until the specified operation completes */
#define wait_for_operation(_m, _o)					\
	do {								\
		struct pa_operation *__o = (_o);			\
		while (pa_operation_get_state(__o) ==			\
		       PA_OPERATION_RUNNING)				\
			pa_mainloop_iterate((_m), 1, NULL);		\
		pa_operation_unref(__o);				\
	} while(0)

/** Wait until no further actions are pending on the connection context */
#define wait_for_completion(_c, _m)					\
	do {								\
		while (pa_context_is_pending(_c))			\
			pa_mainloop_iterate((_m), 1, NULL);		\
	} while(0)


static void
my_pa_free_cb(void *data)
{
	/* dummy callback which ought to free DATA,
	 * however, we want to recycle the memory as much as possible,
	 * we free it ourselves */
	PULSE_DEBUG_S("my_pa_free_cb called 0x%lx\n",
		      (long unsigned int)data);
	return;
}


static inline pa_volume_t
_sound_pulse_change_volume(
	pa_cvolume *cvolp, pa_context *ctx,
	pa_stream *stream, pa_volume_t volume)
	__attribute__((always_inline));
static inline pa_volume_t
_sound_pulse_change_volume(
	pa_cvolume *cvolp, pa_context *ctx,
	pa_stream *stream, pa_volume_t volume)
{
	pa_operation *o;

	pa_cvolume_set(cvolp, cvolp->channels,
		       (volume*PA_VOLUME_NORM)/MEDIA_SAMPLE_VOLUME_NORM);

	if (!(o = pa_context_set_sink_input_volume(
		      ctx, pa_stream_get_index(stream), cvolp, NULL, NULL))) {
		PULSE_CRITICAL("Cannot set volume.\n");
	} else {
		pa_operation_unref(o);
	}
	return volume;
}

/* handling of events from the audio job's private queue */
#ifdef EF_USE_ASYNEQ
static inline void
sound_pulse_stop(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_pulse_stop(audio_job_t aj)
{
	sound_pulse_aj_data_t spsd;
	pa_stream *stream;

	SXE_MUTEX_LOCK(&aj->mtx);
	spsd = audio_job_device_data(aj);
	stream = spsd->stream;
	PULSE_DEBUG_S("FLUSH. (0x%lx)\n", (long unsigned int)aj);
	pa_operation_unref(pa_stream_flush(stream, NULL, NULL));
	pa_stream_disconnect(stream);
	aj->play_state = MTPSTATE_STOP;
	SXE_MUTEX_UNLOCK(&aj->mtx);
	return;
}

static inline void
sound_pulse_pause(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_pulse_pause(audio_job_t aj)
{
	sound_pulse_aj_data_t spsd;
	sound_pulse_data_t spd;
	pa_stream *stream;
	struct timeval tv;

	SXE_MUTEX_LOCK(&aj->mtx);
	spd = get_audio_device_data(aj->device);
	spsd = audio_job_device_data(aj);
	stream = spsd->stream;
	PULSE_DEBUG_S("CORK. (0x%lx)\n", (long unsigned int)aj);
	pa_operation_unref(pa_stream_cork(stream, 1, NULL, NULL));
	aj->play_state = MTPSTATE_PAUSE;

	if (audio_job_queue(aj)) {
		spsd->time_ev = spd->mlapi->
			time_new(spd->mlapi, &tv, time_ev_cb, aj);
	}

	SXE_MUTEX_UNLOCK(&aj->mtx);
	return;
}

static inline void
sound_pulse_resume(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_pulse_resume(audio_job_t aj)
{
	sound_pulse_aj_data_t spsd;
	sound_pulse_data_t spd;
	pa_stream *stream;

	SXE_MUTEX_LOCK(&aj->mtx);
	spd = get_audio_device_data(aj->device);
	spsd = audio_job_device_data(aj);
	stream = spsd->stream;
	PULSE_DEBUG_S("UNCORK. (0x%lx)\n", (long unsigned int)aj);

	if (spsd->time_ev) {
		spd->mlapi->time_free(spsd->time_ev);
		spsd->time_ev = NULL;
	}

	pa_operation_unref(pa_stream_cork(stream, 0, NULL, NULL));
	aj->play_state = MTPSTATE_RUN;
	SXE_MUTEX_UNLOCK(&aj->mtx);
	return;
}

static inline void
sound_pulse_change_volume(audio_job_t aj, audio_job_event_args_t args)
{
	sound_pulse_aj_data_t spsd;
	pa_volume_t new_vol = args->volume_args;

	PULSE_DEBUG_AJ("->%d\n", (int)new_vol);

	SXE_MUTEX_LOCK(&aj->mtx);
	spsd = audio_job_device_data(aj);
	spsd->volume = _sound_pulse_change_volume(
		&spsd->chanvol, spsd->ctx, spsd->stream, new_vol);
	aj->volume = (int)(spsd->volume);
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_pulse_change_rate(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->ratetrafo = args->rate_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_pulse_change_state(audio_job_t aj, audio_job_event_args_t args)
{
	switch (args->state_args) {
	case aj_pause:
		PULSE_DEBUG_AJ("->pause state\n");
		sound_pulse_pause(aj);
		break;
	case aj_resume:
		PULSE_DEBUG_AJ("->resume state\n");
		sound_pulse_resume(aj);
		break;
	case aj_start:
		PULSE_DEBUG_AJ("->start state\n");
		break;
	case aj_stop:
		PULSE_DEBUG_AJ("->stop state\n");
		sound_pulse_stop(aj);
		break;

	case no_audio_job_change_states:
	default:
		PULSE_DEBUG_AJ("->unknown state\n");
		break;
	}
}

static inline void
sound_pulse_handle_aj_events(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_pulse_handle_aj_events(audio_job_t aj)
{
	sound_pulse_aj_data_t spsd;
	pa_stream *stream;
	audio_job_event_t ev = NULL;
	event_queue_t eq;

#if defined EF_USE_ASYNEQ
	if (audio_job_queue(aj)) {
		assert((long unsigned int)audio_job_queue(aj) != 0xB16B00B5);
	}
#endif

	PULSE_DEBUG_AJ("handle aj events called on 0x%lx\n",
		       (long unsigned int)aj);

	SXE_MUTEX_LOCK(&aj->mtx);
	spsd = audio_job_device_data(aj);
	eq = audio_job_queue(aj);
	SXE_MUTEX_UNLOCK(&aj->mtx);
	stream = spsd->stream;
	if ((ev = eq_noseeum_dequeue(eq)) == NULL) {
		return;
	}
	if (stream == NULL || pa_stream_get_state(stream) != PA_STREAM_READY) {
		return;
	}

	PULSE_DEBUG_AJ("Event 0x%lx\n", (long unsigned int)ev);
	switch (audio_job_event_kind(ev)) {
	case aj_change_state:
		PULSE_DEBUG_AJ("change state event\n");
		sound_pulse_change_state(aj, &audio_job_event_args(ev));
		break;
	case aj_change_volume:
		PULSE_DEBUG_AJ("change volume event\n");
		sound_pulse_change_volume(aj, &audio_job_event_args(ev));
		break;
	case aj_change_rate:
		PULSE_DEBUG_AJ("change rate event\n");
		sound_pulse_change_rate(aj, &audio_job_event_args(ev));
		break;

	case no_audio_job_event_kinds:
	default:
		PULSE_CRITICAL("unknown event\n");
		break;
	}
	free_audio_job_event(ev);
}

static void
time_ev_cb(pa_mainloop_api *m, pa_time_event *e,
	   const struct timeval *tv, void *userdata)
{
	/* check an audio-job's queue for incoming events */
	struct timeval next;	/* for rescheduling this construction */
	audio_job_t aj = userdata;
	sound_pulse_aj_data_t spsd = audio_job_device_data(aj);;

	sound_pulse_handle_aj_events(userdata);

	if (spsd->time_ev != NULL) {
		/* reschedule the thing */
		pa_gettimeofday(&next);
		pa_timeval_add(&next, 25000);
		m->time_restart(e, &next);
	}
}
#endif	/* EF_USE_ASYNEQ */

/* This is called whenever stream draining has completed */
static void
stream_drain_complete(pa_stream *stream, int success, void *userdata)
{
	audio_job_t aj = userdata;

	PULSE_DEBUG_S("drain_complete cb called (0x%lx)\n",
		      (long unsigned int)aj);

	if (UNLIKELY(!success)) {
		PULSE_CRITICAL("Failed to drain stream 0x%lx\n",
			       (long unsigned int)aj);
	}

	pa_stream_disconnect(stream);
	PULSE_DEBUG_S("leaving stream_drain cb (0x%lx)\n",
		      (long unsigned int)aj);
	return;
}

/* This is called whenever new data may be written to the stream */
static void
stream_write_callback(pa_stream *stream, size_t length, void *userdata)
{
	audio_job_t aj = NULL;
	media_substream *mss = NULL;
	int resolution __attribute__((unused));
	int channels;
	media_thread_play_state mtp;
	size_t len, tmplen;
	size_t real_frm_sz __attribute__((unused));
	long int i;
	/* pulse and subthread stuff */
	sound_pulse_aj_data_t spsd;
	pa_context *ctx __attribute__((unused)) = NULL;
	sxe_media_sample_t *tmpbuf;

	assert(stream && length);

	if (((aj = userdata) == NULL) ||
	    ((mss = aj->substream) == NULL)) {
		PULSE_CRITICAL("This idiot of mainloop gave us wrong data: "
			       "aj:0x%lx mss:0x%lx\n",
			       (long unsigned int)aj,
			       (long unsigned int)mss);

		PULSE_DEBUG_S("FLUSH.\n");
		pa_operation_unref(
			pa_stream_flush(
				stream, stream_drain_complete, aj));
		pa_stream_disconnect(stream);
		return;
	}

	PULSE_DEBUG_S("stream_write cb called on 0x%lx\n",
		      (long unsigned int)aj);
	if (audio_job_queue(aj)) {
		PULSE_DEBUG_S("checking queue\n");
		sound_pulse_handle_aj_events(userdata);
	}
	SXE_MUTEX_LOCK(&aj->mtx);
	spsd = audio_job_device_data(aj);
	resolution = aj->resolution;
	real_frm_sz = SOUND_MAX_SAMPLE_WIDTH * (channels = aj->channels);
	ctx = spsd->ctx;

#ifndef EF_USE_ASYNEQ
	/* Set the playback volume of the stream */
	if (aj->volume != spsd->volume) {
		spsd->volume = _sound_pulse_change_volume(
			&spsd->chanvol, ctx, stream, (pa_volume_t)aj->volume);
	}
#endif	/* !EF_USE_ASYNEQ */

	mtp = aj->play_state;
	switch (mtp) {
	case MTPSTATE_RUN:
		/* set up length, rerate args and tmpbuf */
		tmplen = length * (spsd->rrargs->tweak = aj->ratetrafo);
		/* It's unlikely but I hit these constraints personally once so
		   let's handle them here.
		   Problem is that a request of 10000 samples from ffmpeg might
		   yield 11560 samples because the decode is not that
		   fine-tunable because a muxed'n'encoded packet likely (if not
		   always) contains a bunch of raw samples (==
		   demuxed'n'decoded).
		   Thence we add an extra 25% threshold here to be maximum
		   sure not to ask for our own suicide.  Of course the media
		   driver itself ought to make sure not to flood the buffer
		   accidentally.  This however is not possible at the moment,
		   look at the signature of the media_stream_meth to get the
		   enlightenment.
		*/
		if (UNLIKELY(tmplen > 4 * aj->buffer_alloc_size / 5)) {
			tmplen = 4 * aj->buffer_alloc_size / 5;
		}
		tmplen /= aj->framesize;
		tmpbuf = (sxe_media_sample_t*)aj->buffer;
		PULSE_DEBUG_S("asking for %u frames, fetching %u instead "
			      "(0x%lx)\n",
			      length/aj->framesize, tmplen,
			      (long unsigned int)aj);
		len = media_stream_meth(mss->up, read)(mss, aj->buffer, tmplen);
		PULSE_DEBUG_S("reader sent %u frames down the pipe\n", len);
		/* result `len' has number of frames */
		if (len == 0) {
			tmplen = 0;
			break;
		}

		/* coerce the stuff, tmplen is in samples */
		tmplen = channels*len;
		for (i = 0; i < spsd->coe_ch_cnt; i++) {
			PULSE_DEBUG_COE("calling coerce "
					"%lu on b:0x%lx l:%lu\n",
					(long unsigned int)i,
					(long unsigned int)tmpbuf,
					(long unsigned int)tmplen);
			tmplen = CALL_MEDIA_SAMPLE_EFFECT(
				spsd->coe_chain, i, tmpbuf, tmpbuf, tmplen);
		}

		MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(spsd->msf)(
			aj->buffer, aj->buffer, tmplen);

		tmplen = tmplen * sizeof(int16_t);

		break;
	case MTPSTATE_PAUSE:
#if !defined EF_USE_ASYNEQ
		/* fill buffer with zeros */
		tmplen = (len = length) * channels * sizeof(int16_t);
		memset(aj->buffer, 0, tmplen);
		break;
#endif

	case MTPSTATE_UNKNOWN:
	case NUMBER_OF_MEDIA_THREAD_PLAY_STATES:
	case MTPSTATE_STOP:
	default:
		PULSE_DEBUG_S("MTPSTATE == STOP, "
			      "hence leaving stream_write cb\n");
		SXE_MUTEX_UNLOCK(&aj->mtx);
		return;
	}

	if (LIKELY(len > 0)) {
		PULSE_DEBUG_S("writing %u frames == %u samples == %u bytes "
			      "(0x%lx)\n",
			      tmplen/aj->framesize,
			      tmplen*aj->channels / aj->framesize,
			      tmplen,
			      (long unsigned int)aj);
		pa_stream_write(stream, aj->buffer, tmplen,
				my_pa_free_cb, 0, PA_SEEK_RELATIVE);
	}

	if (tmplen < length) {
		PULSE_DEBUG_S("DRAIN. (0x%lx)\n", (long unsigned int)aj);
		aj->play_state = MTPSTATE_STOP;
		pa_operation_unref(
			pa_stream_drain(
				stream, stream_drain_complete, aj));
	}

	SXE_MUTEX_UNLOCK(&aj->mtx);
	PULSE_DEBUG_S("leaving stream_write cb\n");
	return;
}

/* This routine is called whenever the stream state changes */
static void
stream_state_callback(pa_stream *s, void *userdata)
{
	sxe_semaphore_t sem = userdata;
	pa_context_state_t st;

	assert(s);

	st = pa_stream_get_state(s);
	switch ((unsigned int)st) {
	case PA_STREAM_CREATING:
		PULSE_DEBUG_S("CREATING.\n");
		break;
	case PA_STREAM_TERMINATED:
		PULSE_DEBUG_S("TERMINATED.\n");
		PULSE_DEBUG_PT("trigger local semaphore\n");
		SXE_SEMAPH_TRIGGER(sem);
		break;

	case PA_STREAM_READY:
		PULSE_DEBUG_S("READY.\n");
		break;

	case PA_STREAM_FAILED:
	default:
		PULSE_DEBUG_S("FAILED.\n");
		PULSE_DEBUG_PT("trigger local semaphore\n");
		SXE_SEMAPH_TRIGGER(sem);
		break;
	}
	return;
}


static int
sound_pulse_play(audio_job_t aj)
{
	/* stream stuff */
	mtype_audio_properties *mtap;
	Lisp_Media_Stream *ms;
	media_substream *mss;
	/* device stuff */
	Lisp_Object device;
	sound_pulse_data_t spd = NULL;
	Lisp_Audio_Device *lad = NULL;
	/* pulse stuff */
	struct sound_pulse_aj_data_s _spsd, *spsd = &_spsd;
	struct pa_sample_spec *ssp = &spsd->sampspec;
	struct pa_channel_map *cmapp = &spsd->chanmap;
	pa_buffer_attr *bap = &spsd->buffattr;
	pa_stream *stream = NULL;
	int r = 0;
	char *stream_name = NULL;
	char *sink_name = NULL;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;
	/* semaphore stuff */
	struct sxe_semaphore_s _sem, *sem = &_sem;
	/* cache stuff */
	int alloced_myself = 0;

	/* unpack the media thread */
	SOUND_UNPACK_MT(aj, device, ms, mss, lad, spd, mtap);

	/* Set up a new main loop - if it's not already running */
	if (!(r = spd->ml_running_p)) {
		PULSE_DEBUG_ML("trying to restart main loop...\n");
		r = sound_pulse_init_mainloop(spd);
		PULSE_DEBUG_ML("result was: %d\n", r);
	}

	if (r == 0 || spd->ml_running_p == 0) {
		warn_when_safe(Qpulse, Qerror,
			       GETTEXT("Failed to (re)connect to server."));
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		goto finish;
	}

	/* prepare pulse sample specs */
	ssp->rate = mtap->samplerate;
	ssp->channels = mtap->channels;

	/* prepare pulse channel map and output format */
	pa_channel_map_init_auto(cmapp, ssp->channels, PA_CHANNEL_MAP_ALSA);
	ssp->format = PA_SAMPLE_S16NE;

	if (spd->ml_running_p && spd->ml_threaded_p) {
		PULSE_DEBUG_ML("lock.\n");
		pa_threaded_mainloop_lock(spd->tml);
	}

	stream_name = (NILP(spd->stream) ? "SXEmacs stream" :
		       (char*)XSTRING_DATA(spd->stream));
	sink_name = (NILP(spd->sink) ? NULL :
		     (char*)XSTRING_DATA(spd->sink));

	/* create the stream */
	PULSE_DEBUG_S("Creating stream...\n");
	if (!spd->ml_running_p ||
	    !(stream = pa_stream_new(spd->ctx, stream_name, ssp, cmapp))) {
		PULSE_CRITICAL("SXEmacs critical: [pulse] stream is NULL. "
			       "Mainloop not running?!\n");
		goto finish;
	}
	PULSE_DEBUG_S("Created stream 0x%lx\n", (long unsigned int)stream);

	/* obviously our device is alive
	 * This does NOT mean that you can hear the sounds */
	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* rewind the media substream */
	media_stream_meth(ms, rewind)(mss);
	SXE_MUTEX_LOCK(&aj->mtx);
	if (aj->buffer_alloc_size == 0) {
		PULSE_DEBUG_CACHE("worker buffer (sz:%ld) too small, "
				  "reallocate (sz:%ld)\n",
				  (long int)aj->buffer_alloc_size,
				  (long int)SOUND_MAX_AUDIO_FRAME_SIZE);
		alloced_myself = 1;
		aj->buffer = xmalloc_atomic(SOUND_MAX_AUDIO_FRAME_SIZE);
		aj->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	}
	aj->resolution = mtap->samplerate * mtap->channels;
	aj->framesize = mtap->channels * sizeof(int16_t);
	aj->channels = mtap->channels;
	SXE_MUTEX_UNLOCK(&aj->mtx);

	/* initialise buffer attributes */
	bap->maxlength = mtap->samplerate * aj->framesize;
	bap->tlength = 9 * bap->maxlength / 10;
	bap->minreq = bap->tlength / 10;
	bap->prebuf = bap->tlength / SOUND_MAX_SAMPLE_WIDTH;
	bap->fragsize = bap->minreq;

	pa_cvolume_set(&spsd->chanvol, ssp->channels,
		       ((pa_volume_t)aj->volume*PA_VOLUME_NORM)/
		       MEDIA_SAMPLE_VOLUME_NORM);
	spsd->ctx = spd->ctx;
	spsd->stream = stream;
	spsd->time_ev = NULL;
	audio_job_device_data(aj) = spsd;

	/* initialise our thread semaphore */
	PULSE_DEBUG_PT("init pulse local semaphore\n");
	SXE_SEMAPH_INIT(sem);

	/* coercion stuff */
	spsd->msf = MEDIA_SAMPLE_FORMAT(sxe_msf_S16);
	spsd->coe_ch_cnt = 0;
	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		spsd->coe_chain, spsd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = mtap->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;
	spsd->rrargs = rrargs;

	PULSE_DEBUG_S("setting up stream.\n");
	pa_stream_set_state_callback(stream, stream_state_callback, sem);
	pa_stream_set_write_callback(stream, stream_write_callback, aj);

	/* we better check if ml is still running */
	if (!spd->ml_running_p)
		goto finish;
	pa_stream_connect_playback(
		stream, sink_name, bap, 0, &spsd->chanvol, NULL);

	if (spd->ml_running_p && spd->ml_threaded_p) {
		WITH_SXE_SEMAPH_SYNCH(
			sem,
			PULSE_DEBUG_ML("unlock.\n");
			pa_threaded_mainloop_unlock(spd->tml);
			PULSE_DEBUG_MX("play.\n"););
		PULSE_DEBUG_PT("ACK, shall finish.\n");
	} else if (spd->ml_running_p) {
		/* iterate by hand */
		pa_stream_state_t state;
		while (spd->ml_running_p) {
			pa_mainloop_iterate(spd->ml, 1, NULL);
			state = pa_stream_get_state(stream);
			if (!(state == PA_STREAM_READY ||
			      state == PA_STREAM_CREATING)) {
				break;
			}
		}
	}

	/* ... and outta here */
	PULSE_DEBUG_S("finish.\n");

	/* close and shutdown */
finish:
	SXE_MUTEX_LOCK(&aj->mtx);
	if (alloced_myself && aj->buffer) {
		PULSE_DEBUG_S("freeing stream buffer.\n");
		xfree(aj->buffer);
	}
	aj->buffer = NULL;
	aj->buffer_alloc_size = 0;

	/* device data could be finished already, hence we refetch it */
	spd = get_audio_device_data(aj->device);
	SXE_MUTEX_UNLOCK(&aj->mtx);

	if (spd && spd->ml_running_p && spd->ml_threaded_p) {
		PULSE_DEBUG_ML("lock.\n");
		pa_threaded_mainloop_lock(spd->tml);
	}

#ifdef EF_USE_ASYNEQ
	/* kick those timers */
	if (spsd->time_ev != NULL) {
		spd->mlapi->time_free(spsd->time_ev);
	}
	spsd->time_ev = NULL;
#endif

#if 1
	if (stream) {
		PULSE_DEBUG_S("unref.\n");
		pa_stream_unref(stream);
		stream = NULL;
	}
#endif

	if (spd && spd->ml_running_p && spd->ml_threaded_p) {
		PULSE_DEBUG_ML("unlock.\n");
		pa_threaded_mainloop_unlock(spd->tml);
	}

	SXE_MUTEX_LOCK(&aj->mtx);
	audio_job_device_data(aj) = NULL;
	SXE_MUTEX_UNLOCK(&aj->mtx);

	/* finish the pulse device data if pulse is not running anymore */
	if (spd && !spd->ml_running_p) {
		SXE_MUTEX_LOCK(&aj->mtx);
		XAUDIO_DEVICE_STATE(aj->device) = ASTATE_DEAD;
		SXE_MUTEX_UNLOCK(&aj->mtx);
		sound_pulse_finish(spd);
		return 0;
	}

	/* deinitialise our thread semaphore */
	PULSE_DEBUG_PT("deinit pulse local semaphore\n");
	SXE_SEMAPH_FINI(sem);

	return 1;
}


#define CHECK_DEAD_GOTO(label)						\
	do {								\
		if (!spd->ctx ||					\
		    pa_context_get_state(spd->ctx) != PA_CONTEXT_READY) { \
			PULSE_CRITICAL("Uh oh, operation failed.\n");	\
			goto label;					\
		}							\
	} while(0)

static void
sink_info_callback(pa_context *c, const pa_sink_info *i, int eol, void *data)
{
	sound_pulse_data_t spd = data;

	if (eol < 0) {
		PULSE_DEBUG_CTX("No sink info\n");
		spd->sink_info = NULL;
	} else if (i == NULL) {
		;
	} else {
		PULSE_DEBUG_CTX("Name: %s, Index: %d\nDescr: %s\n",
				i->name, i->index, i->description);
		spd->sink_info = NULL;
	}

	return;
}

/* This is called whenever the context status changes */
static void
context_state_callback(pa_context *c, void *userdata)
{
	sound_pulse_data_t spd = userdata;
	pa_operation *o;
	char *sink_name = NULL;

	assert(c);

	switch (pa_context_get_state(c)) {
	case PA_CONTEXT_UNCONNECTED:
	case PA_CONTEXT_CONNECTING:
	case PA_CONTEXT_AUTHORIZING:
	case PA_CONTEXT_SETTING_NAME:
		PULSE_DEBUG_CTX("CONN/AUTH.\n");
		spd->ml_running_p = 0;
		break;

	case PA_CONTEXT_READY:
		PULSE_DEBUG_CTX("ESTA.\n");
		sink_name = (NILP(spd->sink) ? NULL :
			     (char*)XSTRING_DATA(spd->sink));

		/* query sinks */
		if (!(o = pa_context_get_sink_info_by_name(
			      spd->ctx, sink_name,
			      sink_info_callback, spd))) {
			PULSE_DEBUG_CTX("Hm, no sink info ... what now?\n");
		} else {
			;
		}

		pa_operation_unref(o);
		spd->ml_running_p = 1;
		pa_threaded_mainloop_signal(spd->tml, 0);
		break;

	case PA_CONTEXT_TERMINATED:
		PULSE_DEBUG_CTX("DEAD.\n");
		spd->ml_running_p = 0;
		pa_threaded_mainloop_signal(spd->tml, 0);
		break;

	case PA_CONTEXT_FAILED:
	default:
		PULSE_DEBUG_CTX("FAIL.\n");
		spd->ml_running_p = 0;
		PULSE_DEBUG_CTX("triggering semaphore.\n");
		pa_threaded_mainloop_signal(spd->tml, 0);
		break;
	}
	return;
}

/* mainloop in an extra thread API */
static int
sound_pulse_init_mainloop(ad_device_data *devdata)
{
	/* device stuff */
	sound_pulse_data_t spd = (sound_pulse_data_t)devdata;
	/* some predeclarations to avoid ugly trigraphs */
	const char *client =
		(NILP(spd->client) ? "SXEmacs" :
		 (const char*)XSTRING_DATA(spd->client));
	const char *server =
		(NILP(spd->server) ? NULL :
		 (const char*)XSTRING_DATA(spd->server));
	const char *role =
		(NILP(spd->role) ? "event" :
		 (const char*)XSTRING_DATA(spd->role));
	int threadedp = spd->ml_threaded_p;

	/* cannot use Pulse on incomplete or corrupt audio devices */
	if (spd == NULL)
		return 0;

	SXE_SEMAPH_INIT(&(spd->ctxsem));

	setenv("PULSE_PROP_media.role", role, 1);

	/* Set up a new main loop */
	if (threadedp)
		spd->tml = pa_threaded_mainloop_new();
	else
		spd->ml = pa_mainloop_new();

	if (!spd->tml && !spd->ml) {
		message(GETTEXT("audio-pulse: "
				"Failed to connect to server."));
		return 0;
	}

	if (threadedp)
		spd->mlapi = pa_threaded_mainloop_get_api(spd->tml);
	else
		spd->mlapi = pa_mainloop_get_api(spd->ml);

	/* Create a new connection context */
	if (!(spd->ctx = pa_context_new(spd->mlapi, client))) {
		message(GETTEXT("audio-pulse: "
				"pa_context_new() failed."));
		spd->ml = NULL;
		spd->tml = NULL;
		return 0;
	}

	/* initialise callback semaphore */
	if (threadedp) {
		spd->ml_running_p = 0;
		pa_context_set_state_callback(
			spd->ctx, context_state_callback, spd);

		/* Connect the context */
		if (pa_context_connect(spd->ctx, server, 0, NULL) < 0) {
			PULSE_DEBUG_ML("failed to connect to server\n");
			goto fail;
		}

		PULSE_DEBUG_ML("locking mainloop\n");
		pa_threaded_mainloop_lock(spd->tml);

		if (pa_threaded_mainloop_start(spd->tml) < 0) {
			PULSE_DEBUG_ML("failed to start mainloop\n");
			goto unlock_and_fail;
		}

		/* Wait until the context is ready */
		pa_threaded_mainloop_wait(spd->tml);

		if (pa_context_get_state(spd->ctx) != PA_CONTEXT_READY) {
			PULSE_DEBUG_ML("failed to connect to server\n");
			goto unlock_and_fail;
		}
	} else {
		/* Connect the context */
		pa_context_connect(spd->ctx, server, 0, NULL);
		/* iterate manually */
		while (1) {
			pa_context_state_t st;

			pa_mainloop_iterate(spd->ml, 1, NULL);
			st = pa_context_get_state(spd->ctx);

			switch ((unsigned int)st) {
			case PA_CONTEXT_CONNECTING:
			case PA_CONTEXT_AUTHORIZING:
			case PA_CONTEXT_SETTING_NAME:
				spd->ml_running_p = 0;
				break;

			case PA_CONTEXT_READY:
				PULSE_DEBUG_CTX("ESTA.\n");
				PULSE_DEBUG_CTX("READY.\n");
				return (spd->ml_running_p = 1);

			case PA_CONTEXT_TERMINATED:
				PULSE_DEBUG_CTX("DEAD.\n");
				return (spd->ml_running_p = 0);

			case PA_CONTEXT_FAILED:
			default:
				PULSE_DEBUG_CTX("FAIL.\n");
				return (spd->ml_running_p = 0);
			}
		}
	}

	/* clean up */
	PULSE_DEBUG_ML("READY: %d.\n", spd->ml_running_p);

unlock_and_fail:
	if (threadedp && spd->tml) {
		pa_threaded_mainloop_unlock(spd->tml);
	}
fail:
	/* indicate success */
	return spd->ml_running_p;
}

/* DEFUN("pulse-get-sink-info-list", Fpulse_get_sink_info_list, 1, 1, 0,  */


#undef MYSELF

/* sound-pulse.c ends here */
