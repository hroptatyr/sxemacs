/* sound-pulse.c - play a sound over the Pulse Audio Server

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
#define PULSE_CRITICAL(args...)		__PULSE_DEBUG__("CRITICAL: " args)


static int sound_pulse_init_mainloop(ad_device_data*);


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_pulse);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_pulse);


static Lisp_Object
sound_pulse_mark(ad_device_data *devdata)
{
	sound_pulse_data *spd = (sound_pulse_data*)devdata;
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
	sound_pulse_data *spd = NULL;
	char *temp = alloca(256);

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

	switch (pa_context_get_state(spd->ctx)) {
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
		write_c_string("#threaded", pcfun);
		snprintf(temp, 255, " :mainloop 0x%x", (unsigned int)spd->tml);
	} else {
		write_c_string("#non-threaded", pcfun);
		snprintf(temp, 255, " :mainloop 0x%x", (unsigned int)spd->ml);
	}

	write_c_string(temp, pcfun);

	return;
}


static ad_device_data *
sound_pulse_create(Lisp_Object pulse_options)
{
	/* result */
	sound_pulse_data *spd = NULL;
	/* option keywords */
	Lisp_Object opt_server = Qnil;
	Lisp_Object opt_sink = Qnil;
	Lisp_Object opt_source = Qnil;
	Lisp_Object opt_client = Qnil;
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

	opt_stream = Fplist_get(pulse_options, intern(":stream"), Qnil);
	if (!NILP(opt_stream) && !STRINGP(opt_stream)) {
		wrong_type_argument(Qstringp, opt_stream);
		return NULL;
	}

	opt_immediate = Fplist_get(pulse_options, intern(":immediate"), Qt);
	opt_threaded = Fplist_get(pulse_options, intern(":threaded"), Qt);
	opt_force = Fplist_get(pulse_options, intern(":force"), Qnil);

	/* initialise and fill */
	spd = xnew_and_zero(sound_pulse_data);
	spd->server = opt_server;
	spd->client = opt_client;
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
	sound_pulse_data *spd = data;
	if (spd == NULL)
		return;

	if (spd != NULL && 0) {
		spd->stream = Qnil;
		spd->server = Qnil;
		spd->client = Qnil;
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
	return;
}

/* This is called whenever new data may be written to the stream */
static void
stream_write_callback(pa_stream *stream, size_t length, void *userdata)
{
	size_t len;
	media_subthread *mst = NULL;
	media_substream *mss = NULL;
	int resolution;
	media_thread_play_state mtp;
	size_t tmplen;
	int channels;
	int curvol;
	int i, sillen;
	/* pulse and subthread stuff */
	sound_pulse_subthread_data *spsd;
	pa_context *ctx = NULL;
	sxe_media_sample_t *tmpbuf;

	assert(stream && length);

	if (((mst = (media_subthread*)userdata) == NULL) ||
	    (!MEDIA_THREADP(wrap_object(mst->up))) ||
	    ((mss = mst->substream) == NULL)) {
		PULSE_CRITICAL("This idiot of mainloop gave us wrong data: "
			       "mst:0x%x mt:0x%x mss:0x%x (mt is mt: %d)\n",
			       (unsigned int)mst, (unsigned int)mst->up,
			       (unsigned int)mss,
			       !MEDIA_THREADP(wrap_object(mst->up)));

		PULSE_DEBUG_S("FLUSH.\n");
		pa_operation_unref(pa_stream_flush(stream, NULL, NULL));
		pa_stream_disconnect(stream);
		return;
	}

	spsd = media_subthread_device_data(mst);
	resolution = mst->resolution;
	channels = mst->channels;
	ctx = spsd->ctx;

	/* Set the playback volume of the stream */
	if ((curvol = mst->up->volume) != spsd->volume) {
		pa_operation *o;
		pa_cvolume *cvolp = &spsd->chanvol;

		pa_cvolume_set(cvolp, cvolp->channels,
			       ((pa_volume_t)curvol*PA_VOLUME_NORM)/
			       MEDIA_SAMPLE_VOLUME_NORM);

		if (!(o = pa_context_set_sink_input_volume(
			      ctx, pa_stream_get_index(stream),
			      cvolp, NULL, NULL)))
			PULSE_CRITICAL("Cannot set volume.\n");
		else
			pa_operation_unref(o);
		spsd->volume = curvol;
        }

	mtp = mst->up->play_state;
	switch (mtp) {
	case MTPSTATE_RUN:
		/* set up length, rerate args and tmpbuf */
		tmplen = length * (spsd->rrargs->tweak = mst->up->ratetrafo);
		tmpbuf = (sxe_media_sample_t*)mst->buffer;
		PULSE_DEBUG_S("asking for %d frames, fetching %d instead "
			      "(0x%x@0x%x)\n",
			      length, tmplen,
			      (unsigned int)mst, (unsigned int)mst->up);
		len = media_stream_meth(mss->up, read)(
			mss, mst->buffer, tmplen);
		/* result `len' has number of frames */

		/* coerce the stuff, tmplen is in samples */
		tmplen = channels*len;
		for (i = 0; i < spsd->coe_ch_cnt; i++) {
			PULSE_DEBUG_COE("calling coerce "
					"%d on b:0x%x l:%d\n",
					i, (unsigned int)tmpbuf, tmplen);
			tmplen = CALL_MEDIA_SAMPLE_EFFECT(
				spsd->coe_chain, i,
				tmpbuf, tmpbuf, tmplen);
		}

		MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(spsd->msf)(
			mst->buffer, mst->buffer, tmplen);

		tmplen = tmplen * sizeof(int16_t);

		break;
	case MTPSTATE_PAUSE:
		/* what about this cork thingie? */
		/* fill buffer with zeros */
		tmplen = (len = length) * channels * sizeof(int16_t);
		memset(mst->buffer, 0, tmplen);
		break;
	case MTPSTATE_STOP:
	default:
		PULSE_DEBUG_S("FLUSH. (0x%x@0x%x)\n",
			      (unsigned int)mst, (unsigned int)mst->up);
		pa_operation_unref(pa_stream_flush(stream, NULL, NULL));
		pa_stream_disconnect(stream);
		return;
	}

	sillen = length*channels*sizeof(int16_t) - tmplen;
	if (sillen > 0)
		memset(mst->buffer+tmplen, 0, sillen);
	else
		sillen = 0;

	if (len > 0) {
		PULSE_DEBUG_S("writing %u frames == %u samples == %u bytes "
			      "plus %u bytes of silence "
			      "(0x%x@0x%x)\n",
			      tmplen/channels/sizeof(int16_t),
			      tmplen/sizeof(int16_t), tmplen, sillen,
			      (unsigned int)mst, (unsigned int)mst->up);
		pa_stream_write(stream, mst->buffer, tmplen+sillen,
				my_pa_free_cb, 0, PA_SEEK_RELATIVE);
	} else {
		PULSE_DEBUG_S("FLUSH. (0x%x@0x%x)\n",
			      (unsigned int)mst, (unsigned int)mst->up);
		pa_operation_unref(pa_stream_flush(stream, NULL, NULL));
		pa_stream_disconnect(stream);
	}

	return;
}

/* This routine is called whenever the stream state changes */
static void
stream_state_callback(pa_stream *s, void *userdata)
{
	sxe_semaphore_t *sem = userdata;

	assert(s);

	switch (pa_stream_get_state(s)) {
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
sound_pulse_play(media_subthread *mst)
{
	/* stream stuff */
	mtype_audio_properties *mtap;
	Lisp_Media_Stream *ms;
	media_substream *mss;
	Lisp_Media_Thread *mt;
	/* device stuff */
	Lisp_Object device;
	sound_pulse_data *spd = NULL;
	Lisp_Audio_Device *lad = NULL;
	/* pulse stuff */
	sound_pulse_subthread_data _spsd, *spsd = &_spsd;
	struct pa_sample_spec *ssp = &spsd->sampspec;
	struct pa_channel_map *cmapp = &spsd->chanmap;
	pa_cvolume *cvp = &spsd->chanvol;
	pa_buffer_attr *bap = &spsd->buffattr;
	pa_stream *stream = NULL;
	int r = 0;
	char *stream_name = NULL;
	char *sink_name = NULL;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;
	/* semaphore stuff */
	sxe_semaphore_t _sem, *sem = &_sem;

	/* unpack the media thread */
	SOUND_UNPACK_MT(mt, mst, device, ms, mss, lad, spd, mtap);

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
	if (!spd->ml_running_p ||
	    !(stream = pa_stream_new(spd->ctx, stream_name, ssp, cmapp))) {
		PULSE_CRITICAL("SXEmacs critical: [pulse] stream is NULL. "
			       "Mainloop not running?!\n");
		goto finish;
	}

	/* obviously our device is alive
	 * This does NOT mean that you can hear the sounds */
	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* rewind the media substream */
	media_stream_meth(ms, rewind)(mss);
	mst->buffer = xmalloc(SOUND_MAX_AUDIO_FRAME_SIZE);
	mst->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	mst->resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mst->framesize = mtap->channels * sizeof(int16_t);
	mst->channels = mtap->channels;

	/* initialise buffer attributes */
        bap->tlength = mst->resolution;
        bap->maxlength = SOUND_MAX_AUDIO_FRAME_SIZE;
        bap->minreq = bap->fragsize = bap->prebuf = mst->resolution;

	pa_cvolume_set(cvp, ssp->channels,
		       ((pa_volume_t)mst->up->volume * PA_VOLUME_NORM) / 10000);

	spsd->volume = -1;	/* such that it's set during callback */
	spsd->ctx = spd->ctx;
	spsd->stream = stream;
	media_subthread_device_data(mst) = spsd;

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
	pa_stream_set_write_callback(stream, stream_write_callback, mst);

	/* we better check if ml is still running */
	if (!spd->ml_running_p)
		goto finish;
	pa_stream_connect_playback(stream, sink_name, bap, 0, cvp, NULL);

	if (spd->ml_running_p && spd->ml_threaded_p) {
		PULSE_DEBUG_ML("unlock.\n");
		pa_threaded_mainloop_unlock(spd->tml);
		PULSE_DEBUG_MX("play.\n");
		SXE_SEMAPH_SYNCH(sem);
		PULSE_DEBUG_PT("ACK, shall finish.\n");
	} else if (spd->ml_running_p) {
		/* iterate by hand */
		pa_stream_state_t state;
		while (spd->ml_running_p) {
			pa_mainloop_iterate(spd->ml, 1, NULL);
			state = pa_stream_get_state(stream);
			if (!(state == PA_STREAM_CREATING ||
			      state == PA_STREAM_READY))
				break;
		}
	}

	/* ... and outta here */
	PULSE_DEBUG_S("finish.\n");

	/* close and shutdown */
finish:
	if (mst->buffer) {
		PULSE_DEBUG_S("freeing stream buffer.\n");
		xfree(mst->buffer);
	}
	mst->buffer = NULL;
	mst->buffer_alloc_size = 0;

	/* device data could be finished already, hence we refetch it */
	spd = get_audio_device_data(mst->up->device);
	if (spd && spd->ml_running_p && spd->ml_threaded_p) {
		PULSE_DEBUG_ML("lock.\n");
		pa_threaded_mainloop_lock(spd->tml);
	}

	if (stream) {
		PULSE_DEBUG_S("unref.\n");
		pa_stream_unref(stream);
		stream = NULL;
	}

	if (spd && spd->ml_running_p && spd->ml_threaded_p) {
		PULSE_DEBUG_ML("unlock.\n");
		pa_threaded_mainloop_unlock(spd->tml);
	}

	media_subthread_device_data(mst) = NULL;

	/* finish the pulse device data if pulse is not running anymore */
	if (spd && !spd->ml_running_p) {
		XAUDIO_DEVICE_STATE(mst->up->device) = ASTATE_DEAD;
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
	sound_pulse_data *spd = data;

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
	sound_pulse_data *spd = userdata;
	sxe_semaphore_t *sem = &(spd->ctxsem);
	pa_operation *o;
	char *sink_name = NULL;

	assert(c);

	switch (pa_context_get_state(c)) {
        case PA_CONTEXT_CONNECTING:
        case PA_CONTEXT_AUTHORIZING:
        case PA_CONTEXT_SETTING_NAME:
		spd->ml_running_p = 0;
		break;
        
        case PA_CONTEXT_READY:
		PULSE_DEBUG_CTX("ESTA.\n");

		sink_name = (NILP(spd->sink) ? NULL :
			     (char*)XSTRING_DATA(spd->sink));

		/* query sinks */
		if (!(o = pa_context_get_sink_info_by_name(
			      spd->ctx, sink_name, sink_info_callback, spd))) {
			PULSE_DEBUG_CTX("Hm, no sink info ... what now?\n");
		} else {
			;
		}

		pa_operation_unref(o);

		spd->ml_running_p = 1;
		SXE_SEMAPH_TRIGGER(sem);
		break;
            
        case PA_CONTEXT_TERMINATED:
		PULSE_DEBUG_CTX("DEAD.\n");
		spd->ml_running_p = 0;
		SXE_SEMAPH_TRIGGER(sem);
		break;

        case PA_CONTEXT_FAILED:
        default:
		PULSE_DEBUG_CTX("FAIL.\n");
		spd->ml_running_p = 0;
		SXE_SEMAPH_TRIGGER(sem);
		break;
	}
	return;
}

/* mainloop in an extra thread API */
static int
sound_pulse_init_mainloop(ad_device_data *devdata)
{
	/* device stuff */
	sound_pulse_data *spd = (sound_pulse_data*)devdata;
	/* thread stuff */
	sxe_semaphore_t *sem = &(spd->ctxsem);
	/* some predeclarations to avoid ugly trigraphs */
	const char *client = 
		(NILP(spd->client) ? "SXEmacs" :
		 (const char*)XSTRING_DATA(spd->client));
	const char *server =
		(NILP(spd->server) ? NULL :
		 (const char*)XSTRING_DATA(spd->server));
	int threadedp = spd->ml_threaded_p;

	/* cannot use Pulse on incomplete or corrupt audio devices */
	if (spd == NULL)
		return 0;

	SXE_SEMAPH_INIT(&(spd->ctxsem));

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
		pa_context_set_state_callback(
			spd->ctx, context_state_callback, spd);
	}

	/* Connect the context */
	pa_context_connect(spd->ctx, server, 0, NULL);

	PULSE_DEBUG_ML("START.\n");
	if (threadedp) {
		pa_threaded_mainloop_start(spd->tml);
		PULSE_DEBUG_MX("mainloop context.\n");
		SXE_SEMAPH_SYNCH(sem);
	} else {
		/* iterate manually */
		while (1) {
			pa_mainloop_iterate(spd->ml, 1, NULL);
			switch (pa_context_get_state(spd->ctx)) {
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

	/* indicate success */
	return spd->ml_running_p;
}

/* DEFUN("pulse-get-sink-info-list", Fpulse_get_sink_info_list, 1, 1, 0,  */


#undef MYSELF

/* sound-pulse.c ends here */
