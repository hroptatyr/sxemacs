/* sound-polyp.c - play a sound over the Polyp Audio Server

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
#include "sound-polyp.h"

#define POLYP_USE_SIMPLE_API 1

Lisp_Object Qpolyp;

#define SOUND_POLYP_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec @ 48kHz 32bit 2ch */
#define MYSELF ADRIVER_POLYP


char *sound_polyp_subprint(Lisp_Object device)
{
	sound_polyp_data *spd = NULL;
	char *out = xmalloc(64);

	spd = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || spd == NULL) {
		memcpy(out, "VOID\000", 5);
	} else
		memcpy(out, "\000", 1);

	/* now that we are here, mark AO device as dead if so */
	if (XAUDIO_DEVICE_DRIVER(device) == MYSELF && spd == NULL)
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
	else
		XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* info about the connected output plugin */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || spd == NULL);
	else {
		char *temp = alloca(48);
		snprintf(temp, 47, " :server \"%s\" :device \"%s\"",
			 (spd->polyp_server ? spd->polyp_server : "localhost"),
			 (spd->polyp_device ?
			  spd->polyp_device : "default_sink"));
		strncat(out, temp, 47);
	}

	/* info about the general state */
	strcat(out, " :state ");
	switch (XAUDIO_DEVICE_STATE(device)) {
	case ASTATE_UNDECIDED:
	case ASTATE_DEAD:
	case ASTATE_SUSPENDED:
		strcat(out, "#dead");
		break;
	case ASTATE_ALIVE:
		strcat(out, "#ready");
		break;
	default:
		strcat(out, "#unknown");
		break;
	}

	return out;
}


sound_polyp_data *sound_polyp_create(Lisp_Object polyp_options)
{
	char *server = NULL;
	char *client = NULL;
	char *device = NULL;
	char *stream = NULL;
	/* result */
	sound_polyp_data *spd = NULL;
	/* option keywords */
	Lisp_Object opt_server;
	Lisp_Object opt_device;
	Lisp_Object opt_client;
	Lisp_Object opt_stream;

	/* parse options */
	opt_server = Fplist_get(polyp_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	} else if (STRINGP(opt_server)) {
		server = xmalloc(64);
		strncpy(server, (char*)XSTRING_DATA(opt_server), 63);
	}

	opt_device = Fplist_get(polyp_options, intern(":device"), Qnil);
	if (!NILP(opt_device) && !STRINGP(opt_device)) {
		wrong_type_argument(Qstringp, opt_device);
		return NULL;
	} else if (STRINGP(opt_device)){
		device = xmalloc(64);
		strncpy(device, (char*)XSTRING_DATA(opt_device), 63);
	}

	opt_client = Fplist_get(polyp_options, intern(":client"), Qnil);
	if (!NILP(opt_client) && !STRINGP(opt_client)) {
		wrong_type_argument(Qstringp, opt_client);
		return NULL;
	} else if (STRINGP(opt_client)){
		client = xmalloc(64);
		strncpy(client, (char*)XSTRING_DATA(opt_client), 63);
	}

	opt_stream = Fplist_get(polyp_options, intern(":stream"), Qnil);
	if (!NILP(opt_stream) && !STRINGP(opt_stream)) {
		wrong_type_argument(Qstringp, opt_stream);
		return NULL;
	} else if (STRINGP(opt_stream)){
		stream = xmalloc(64);
		strncpy(stream, (char*)XSTRING_DATA(opt_stream), 63);
	}

	/* initialise and fill */
	spd = xnew_and_zero(sound_polyp_data);
	spd->polyp_server = server;
	spd->polyp_device = device;
	spd->client_name = client;
	spd->stream_name = stream;

	return spd;
}

void sound_polyp_finish(sound_polyp_data *spd)
{
	if (spd != NULL) {
		if (spd->polyp_server)
			xfree(spd->polyp_server);
		if (spd->polyp_device)
			xfree(spd->polyp_device);
		if (spd->client_name)
			xfree(spd->client_name);
		if (spd->stream_name)
			xfree(spd->stream_name);
	}
	return;
}


#if POLYP_USE_SIMPLE_API
int sound_polyp_play_stream(media_subthread *mst)
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
	sound_polyp_data *spd = NULL;
	/* polyp stuff */
	struct pa_sample_spec *ss;
	struct pa_simple *s = NULL;
	/* buffering */
	char *buffer = NULL;
	uint32_t len;
	int resolution;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;;

	/* unpack device */
	lad = get_audio_device(device);
	spd = get_audio_device_data(device);

	/* cannot use Polyp on incomplete or corrupt audio devices */
	if (lad == NULL || spd == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* cannot use Polyp to play non-audio stuff */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* prepare polyp sample specs */
	ss = xnew_and_zero(struct pa_sample_spec);
	ss->rate = mtap->samplerate;
	ss->channels = mtap->channels;

	if (mtap->samplewidth <= 16)
		ss->format = PA_SAMPLE_S16NE;
	else
		ss->format = PA_SAMPLE_FLOAT32NE;

	/* Create a new playback stream */
	if ((s = pa_simple_new(
		     /* server */
		     spd->polyp_server,
		     /* client name */
		     (spd->client_name ? spd->client_name : "SXEmacs"),
		     /* direction */
		     PA_STREAM_PLAYBACK,
		     /* device */
		     spd->polyp_device,
		     /* stream name */
		     (spd->stream_name ? spd->stream_name : "SXEmacs stream"),
		     /* sample_spec */
		     ss,
		     /* pa attributes */
		     NULL,
#if POLYP_VERSION < 8
		     /* volume */
		     PA_VOLUME_NORM,
#endif
		     /* error ptr */
		     NULL)));

	else {
		message(GETTEXT("audio-polyp: "
				"Connecting to polyp server failed."));
		goto finish;
	}

	/* rewind it ... */
	media_substream_rewind(mss);

	/* ... and play it */
	buffer = xmalloc(SOUND_POLYP_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		if (pa_simple_write(s, buffer, len*mtap->framesize, NULL) < 0) {
			message(GETTEXT("audio-polyp: "
					"Writing to polyp server failed."));
			goto finish;
		}

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* Make sure that every single sample was played */
	if (pa_simple_drain(s, NULL) < 0) {
		message(GETTEXT("audio-polyp: "
				"Draining failed."));
		goto finish;
	}

	/* flush everything */
	if (pa_simple_flush(s, NULL) < 0) {
		message(GETTEXT("audio-polyp: "
				"Flushing failed."));
		goto finish;
	}

	/* close and shutdown */
finish:
	if (ss)
		xfree(ss);
	if (s)
		pa_simple_free(s);
	if (buffer)
		xfree(buffer);
	return 1;
}

#else  /* POLYP_USE_SIMPLE_API */

int sound_polyp_play_stream(media_subthread *mst)
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
	sound_polyp_data *spd = NULL;
	/* polyp stuff */
	struct pa_sample_spec *ss;
	pa_mainloop* m = NULL;
	pa_mainloop_api *mainloop_api = NULL;
	pa_context *context = NULL;
	pa_stream *stream = NULL;
	int r = 0, quit = 0, drained = 0;
	/* buffering */
	char *buffer = NULL;
	int resolution;
	uint32_t len;
	size_t tmplen;

	inline void quit_api(void);
	inline void drain_all(void);
	inline void wait_for_operation(struct pa_operation*);
	inline void wait_for_completion(void);

	/** Wait until the specified operation completes */
	void wait_for_operation(struct pa_operation *o)
	{
		assert(o && m);

		while (pa_operation_get_state(o) == PA_OPERATION_RUNNING)
			pa_mainloop_iterate(m, 1, NULL);

		pa_operation_unref(o);
	}

	/** Wait until no further actions are pending on the connection context */
	void wait_for_completion(void)
	{
		assert(context && m);

		while (pa_context_is_pending(context))
			pa_mainloop_iterate(m, 1, NULL);
	}

	/* mainloop api quit function */
	void quit_api(void)
	{
		if (quit)
			return;

		mainloop_api->quit(mainloop_api, 1);
		quit = 1;
	}

	void drain_all(void)
	{
		if (drained)
			return;

		wait_for_operation(
			pa_stream_drain(stream, NULL, NULL));
		drained = 1;
	}

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;;

	/* unpack device */
	lad = get_audio_device(device);
	spd = get_audio_device_data(device);

	/* cannot use Polyp on incomplete or corrupt audio devices */
	if (lad == NULL || spd == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* cannot use Polyp to play non-audio stuff */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* prepare polyp sample specs */
	ss = xnew_and_zero(struct pa_sample_spec);
	ss->rate = mtap->samplerate;
	ss->channels = mtap->channels;

	if (mtap->samplewidth <= 16)
		ss->format = PA_SAMPLE_S16NE;
	else
		ss->format = PA_SAMPLE_FLOAT32NE;

	buffer = xmalloc(SOUND_POLYP_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;

	/* Set up a new main loop */
	if (!(m = pa_mainloop_new())) {
		message(GETTEXT("audio-polyp: "
				"Connecting to polyp server failed."));
		goto finish;
	}

	mainloop_api = pa_mainloop_get_api(m);

	r = 0;
    
	/* Create a new connection context */
	if (!(context = pa_context_new(mainloop_api,
				       (spd->client_name ?
					spd->client_name :
					"SXEmacs")))) {
		message(GETTEXT("audio-polyp: "
				"pa_context_new() failed."));
		goto finish;
	}

	/* pa_context_set_state_callback(context, context_state_callback, NULL);
	 */

	/* Connect the context */
	pa_context_connect(context, spd->polyp_server, 0, NULL);

	wait_for_completion();

	if (pa_context_get_state(context) != PA_CONTEXT_READY) {
		message(GETTEXT("audio-polyp: "
				"Connecting to polyp server failed. %s"),
			pa_strerror(pa_context_errno(context)));
		goto finish;
	}

	/* create the stream */
	stream = pa_stream_new(context, (spd->stream_name ?
					 spd->stream_name :
					 "SXEmacs stream"), ss, NULL);
	assert(stream);

	pa_stream_connect_playback(stream, spd->polyp_device,
				   NULL, PA_STREAM_INTERPOLATE_LATENCY,
				   NULL, NULL);

	wait_for_completion();

	if (pa_stream_get_state(stream) != PA_STREAM_READY) {
		message(GETTEXT("audio-polyp: "
				"Failed to connect to server: %s."),
			pa_strerror(pa_context_errno(context)));
		goto finish;
	}

	/* rewind the media substream */
	media_substream_rewind(mss);

	mtp = mt->play_state;
	while (r >= 0 && quit == 0 &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		if (mtp != mt->play_state) {
			switch (mt->play_state) {
			case MTPSTATE_RUN:
				/* fprintf(stderr, "Uncork!\n"); */
				wait_for_operation(
					pa_stream_cork(stream, 0, NULL, NULL));
				break;
			case MTPSTATE_PAUSE:
				/* fprintf(stderr, "Cork!\n"); */
				wait_for_operation(
					pa_stream_cork(stream, 1, NULL, NULL));
				break;
			case MTPSTATE_STOP:
			default:
				drain_all();
				break;
			}
			mtp = mt->play_state;
		}
		while (len > 0) {
			while (!(tmplen = pa_stream_writable_size(stream)) &&
			       r >= 0)
				r = pa_mainloop_iterate(m, 1, NULL);

			if (r < 0)
				break;

			if (tmplen > len)
				tmplen = len;

			pa_stream_write(stream, buffer,
					tmplen*mtap->framesize, NULL,
					0, PA_SEEK_RELATIVE);
			len -= tmplen;
		}
	}

	/* fprintf(stderr, "Draining.\n"); */

	/* close and shutdown */
finish:
	if (ss)
		xfree(ss);
	if (stream) {
		drain_all();
		pa_stream_unref(stream);
		stream = NULL;
	}

	if (context) {
		pa_context_unref(context);
		context = NULL;
	}

	if (m) {
		pa_mainloop_free(m);
		m = NULL;
	}
	return 1;
}
#endif	/* POLYP_USE_SIMPLE_API */

#undef MYSELF

/* sound-polyp.c ends here */
