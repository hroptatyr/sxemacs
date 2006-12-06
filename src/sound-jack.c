/* sound-jack.c - play a sound over the Jack Audio Server

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
#include "sound-jack.h"

Lisp_Object Qjack;

#define MYSELF ADRIVER_JACK
#define SAMPLE_MAX_32BIT  2147483647.0f
#define SAMPLE_MAX_24BIT  8388607.0f
#define SAMPLE_MAX_16BIT  32767.0f
#define SAMPLE_MAX_8BIT   255.0f

static JMP_BUF jack_server_sig;
static int sound_jack_process(jack_nframes_t, void*);
static void sound_jack_shutdown_cbfun(void*);
static void sound_jack_error(const char*);
static void sound_jack_silence(float**, int, sound_jack_subthread_data*);
static int sound_jack_write(float**, jack_nframes_t, media_subthread*);
static size_t demux_internal();

#define __JACK_DEBUG__(args...)	fprintf(stderr, "JACK " args)
#ifndef JACK_DEBUG_FLAG
#define JACK_DEBUG(args...)
#else
#define JACK_DEBUG(args...)		__JACK_DEBUG__(args)
#endif
#define JACK_DEBUG_S(args...)		JACK_DEBUG("[stream]: " args)
#define JACK_CRITICAL(args...)		__JACK_DEBUG__("CRITICAL: " args)


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_jack);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_jack);


static Lisp_Object
sound_jack_mark(ad_device_data *devdata)
{
	sound_jack_data *sjd = devdata;

	mark_object(sjd->options);
	mark_object(sjd->server);

	return Qnil;
}

static void
sound_jack_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_jack_data *sjd = NULL;

	sjd = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sjd == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :server ", pcfun);
	if (NILP(sjd->server))
		write_c_string("#default", pcfun);
	else
		print_internal(sjd->server, pcfun, ef);

	write_c_string(" :client ", pcfun);
	if (NILP(sjd->client))
		write_c_string("SXEmacs", pcfun);
	else
		print_internal(sjd->client, pcfun, ef);

	return;
}


static ad_device_data *
sound_jack_create(Lisp_Object jack_options)
{
	/* result */
	sound_jack_data *sjd = NULL;
	/* option keywords */
	Lisp_Object opt_server = Qnil;
	Lisp_Object opt_client = Qnil;

	/* parse options */
	opt_server = Fplist_get(jack_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	}

	opt_client = Fplist_get(jack_options, intern(":client"), Qnil);
	if (!NILP(opt_client) && !STRINGP(opt_client)) {
		wrong_type_argument(Qstringp, opt_client);
		return NULL;
	}

	/* initialise and fill */
	sjd = xnew_and_zero(sound_jack_data);
	sjd->options = jack_options;
	sjd->server = opt_server;
	sjd->client = opt_client;
	sjd->num_ports = 0;

	return (ad_device_data*)sjd;
}

static void
sound_jack_finish(ad_device_data *data)
{
	sound_jack_data *sjd = data;
	if (sjd != NULL) {
		;
	}

	return;
}


static ad_device_data *
sound_jack_subthread_create(void)
{
	/* result */
	sound_jack_subthread_data *sjsd = NULL;
	/* jack stuff */
	jack_client_t *client = NULL;
	int port_flags = 0;
	const char **ports = NULL;
	int i;

	/* Create a new playback client */
	client = jack_client_open("SXEmacs", 0, NULL);
	if (!client) {
		message(GETTEXT("audio-jack: "
				"cannot open server."));
		return NULL;
	}

	/* initialise and fill */
	sjsd = xnew_and_zero(sound_jack_subthread_data);
	sjsd->client = client;

	/* list matching ports */
	port_flags |= JackPortIsInput;
	port_flags |= JackPortIsPhysical;
	ports = jack_get_ports(client, NULL, NULL, port_flags);
	for (sjsd->num_ports = 0; ports && ports[sjsd->num_ports];
	     sjsd->num_ports++);	/* just count */
	if (!sjsd->num_ports) {
		message(GETTEXT("audio-jack: "
				"no physical ports available."));
		xfree(sjsd);
		return NULL;
	}

	JACK_DEBUG("initialised %d ports\n", sjsd->num_ports);

	/* if (mtap->channels > sjsd->num_ports); */

	/* create out output ports */
	for (i = 0; i < sjsd->num_ports; i++) {
		char pname[30];
		snprintf(pname, 30, "SXEmacs out_%d", i);
		sjsd->ports[i] = jack_port_register(
			client, pname,
			JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
		if (!sjsd->ports[i]) {
			message(GETTEXT("audio-jack: "
					"not enough ports available."));
			xfree(sjsd);
			return NULL;
		}
	}

	sjsd->port_ptrs = ports;

	return (ad_device_data*)sjsd;
}

static int
sound_jack_play(media_subthread *mst)
{
	/* stream stuff */
	mtype_audio_properties *mtap;
	Lisp_Media_Stream *ms;
	media_substream *mss;
	Lisp_Media_Thread *mt;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_jack_data *sjd = NULL;
	sound_jack_subthread_data *sjsd = NULL;
	int i;
	sxe_semaphore_t _sem, *sem = &_sem;

	SOUND_UNPACK_MT(mt, mst, device, ms, mss, lad, sjd, mtap);

	/* get jack device */
	JACK_DEBUG("creating subthread.\n");
	if ((sjsd = sound_jack_subthread_create()) == NULL) {
		message(GETTEXT("audio-jack: "
				"cannot create connection to jack server."));
		return 0;
	}

	/* initialise our callback semaphore */
	SXE_SEMAPH_INIT(sem);
	/* ... and store it */
	sjsd->sem = sem;

	if (SETJMP(jack_server_sig)) {
		JACK_CRITICAL("Caught a lethal signal.\n");
		warn_when_safe(
			Qjack, Qerror,
			GETTEXT("Jack daemon died or "
				"send us a lethal signal! "
				"This instance might have become a total mess! "
				"Consider a restart."));
		return 1;
	}

	/* rewind the stream */
	media_stream_meth(ms, rewind)(mss);
	mst->buffer = xmalloc(SOUND_MAX_AUDIO_FRAME_SIZE);
	mst->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	mst->resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;

	media_subthread_data(mst) = sjsd;
	sjsd->samplerate = mtap->samplerate;
	sjsd->framesize = (sjsd->channels = mtap->channels) * sizeof(int32_t);
	sjsd->demux_fun = demux_internal;

	sjsd->ringbufcnt = SOUND_MAX_AUDIO_FRAME_SIZE >> 10;
	sjsd->readpos = sjsd->writepos = sjsd->overfill = 0;

	JACK_DEBUG("setting callback.\n");
	jack_set_process_callback(sjsd->client, sound_jack_process, mst);
	jack_set_error_function(sound_jack_error);
	jack_on_shutdown(sjsd->client, sound_jack_shutdown_cbfun, mst);

	/* now activate */
	if (jack_activate(sjsd->client)) {
		message(GETTEXT("audio-jack: "
				"activate failed."));
		goto finish;
	}

	for (i = 0; i < sjsd->num_ports; i++) {
		if (jack_connect(sjsd->client,
				 jack_port_name(sjsd->ports[i]),
				 sjsd->port_ptrs[i])) {
			message(GETTEXT("audio-jack: "
					"connecting failed."));
			goto finish;
		}
	}

	JACK_DEBUG("SEMAPHORE WAIT: 0x%x@0x%x@0x%x\n",
		   (unsigned int)sem, (unsigned int)sjsd, (unsigned int)mst);
	SXE_SEMAPH_SYNCH(sem);

	/* close and shutdown */
finish:
	JACK_DEBUG("finish.\n");
	if (sjsd->client) {
		jack_client_t *cli = sjsd->client;
		JACK_DEBUG("DISC.\n");
		sjsd->client = NULL;
		for (i = 0; i < sjsd->num_ports; i++) {
			jack_disconnect(cli,
					jack_port_name(sjsd->ports[i]),
					sjsd->port_ptrs[i]);
		}
		JACK_DEBUG("CLOSE.\n");
		jack_client_close(cli);
	}

	SXE_SEMAPH_FINI(sem);

	if (mst->buffer)
		xfree(mst->buffer);
	mst->buffer = NULL;

	if (sjsd)
		xfree(sjsd);
	sjsd = NULL;

	mt->state = MTSTATE_FINISHED;

	return 1;
}


/* pull one channel out of a multi-channel stream */
static size_t
demux_internal(float *dst, char *src,
	       size_t n, int chan, sound_jack_subthread_data *f)
{
	/* dst: destination buffer */
	/* src: source buffer */
	/* n: number of samples */
	/* chan: channel to frob */
	int off = sizeof(int32_t)*chan;
	int sr = f->samplerate;
	int fs = f->framesize;
	int i;

	for (i = 0; i < n; i++) {
		/* compute frame number to serve */
		int frame = i * sr/48000 * f->ratetrafo;
		int srcp = frame*fs + off;
		dst[i] = ((float)(*(int32_t*)(src+srcp)) / SAMPLE_MAX_24BIT)
			* f->fvol;
	}

	/* actually read frames were n * samplerate / 48000 */
	return n * sr/48000 * f->ratetrafo;
}

/**
 * error callback
 **/
static void
sound_jack_error(const char *errmsg)
{
	JACK_CRITICAL("Strange things happened.\n"
		      "Error message: %s\n",
		      errmsg);

	LONGJMP(jack_server_sig, 1);

	return;
}

/* shutdown callback */
static void
sound_jack_shutdown_cbfun(void *arg)
{
	JACK_CRITICAL("Shutdown: 0x%x\n", (unsigned int)arg);

	LONGJMP(jack_server_sig, 1);

	return;
}


/**
 * JACK Callback function
 * - nframes number of frames to fill into buffers
 * - arg contains the media subthread to operate on
 */
static int
sound_jack_process(jack_nframes_t nframes, void *userdata)
{
	media_subthread *mst = NULL;
	sound_jack_subthread_data *sjsd = NULL;
	media_substream *mss = NULL;
	char *buffer = NULL;
	media_thread_play_state mtp;
	Lisp_Media_Thread *mt;
	float *bufs[MAX_CHANS];
	int curvol, i;

	mst = (media_subthread*)userdata;
	mt = mst->up;
	mss = mst->substream;
	sjsd = (sound_jack_subthread_data*)media_subthread_data(mst);
	buffer = mst->buffer;

	/* Set the playback volume of the stream */
	if ((curvol = mt->volume) != sjsd->volume) {
		sjsd->fvol = (float)curvol / MEDIA_SAMPLE_VOLUME_NORM;
		sjsd->volume = curvol;
        }

	if (mt->ratetrafo != sjsd->ratetrafo) {
		sjsd->ratetrafo = mt->ratetrafo;
	}

	for (i = 0; i < sjsd->num_ports; i++)
		bufs[i] = jack_port_get_buffer(sjsd->ports[i], nframes);

	mtp = mt->play_state;
	switch (mtp) {
	case MTPSTATE_RUN:
		sound_jack_write(bufs, nframes, mst);
		break;
	case MTPSTATE_PAUSE:
		sound_jack_silence(bufs, nframes, sjsd);
		break;
	case MTPSTATE_STOP:
	default:
		JACK_DEBUG_S("DRAIN.\n");
		SXE_SEMAPH_TRIGGER(sjsd->sem);
		break;
	}
	return 0;
}

/**
 * fill the buffers with silence
 * - bufs num_bufs float buffers, each will contain the data of one channel
 * - cnt number of samples in each buffer
 * - num_bufs number of buffers
 */
static void
sound_jack_silence(float **bufs, int cnt, sound_jack_subthread_data *sjsd)
{
	int i, j;
	for (i = 0; i < cnt; i++)
		for (j = 0; j < sjsd->num_ports; j++)
			bufs[j][i] = 0.0f;
}

static int
sound_jack_write(float **bufs, jack_nframes_t nframes, media_subthread *mst)
{
	sound_jack_subthread_data *sjsd = media_subthread_data(mst);
	int i, j;
	size_t len = 0, tmplen = 0, tmpcnt = 0;
	sound_jack_demux_fun dmx = sjsd->demux_fun;
	ms_read_fun read = media_stream_meth(mst->substream->up, read);

	/* cleanup buffer, move the frame overfill to the beginning */
	if (sjsd->writepos < mst->buffer_alloc_size >> 1 &&
	    sjsd->underrun < 4) {
		memcpy(mst->buffer, mst->buffer+sjsd->readpos,
		       sjsd->writepos - sjsd->readpos);
		sjsd->writepos -= sjsd->readpos;
		sjsd->readpos = 0;

		len = read(mst->substream, mst->buffer+sjsd->writepos, nframes);
		if (len == -1)
			len = 0;

		sjsd->overfill += len;
		sjsd->writepos += len*sjsd->framesize;

		/* increase underrun counter to detect the end */
		if (len == 0)
			sjsd->underrun++;
	} else if (sjsd->overfill > nframes << 2 ||
		   sjsd->underrun >= 4) {
		JACK_DEBUG("having a rest\n");
	} else {
		JACK_DEBUG("resetting write position.\n");
		memcpy(mst->buffer, mst->buffer+sjsd->readpos,
		       sjsd->writepos - sjsd->readpos);
		sjsd->writepos -= sjsd->readpos;
		sjsd->readpos = 0;
	}

	JACK_DEBUG_S("req:%d p, got:%d p, readpos %d, writepos %d, "
		     "overfill: %d, framesize: %d, sr: %d\n",
		     nframes, len, sjsd->readpos, sjsd->writepos,
		     sjsd->overfill, sjsd->framesize, sjsd->samplerate);

	/* care for buffer underruns */
	if (nframes > sjsd->overfill)
		tmplen = sjsd->overfill;
	else
		tmplen = nframes;

	/* output, i loops over the number of ports we have to fill,
	 * while j loops over the actual channels */
	for (i = 0, j = 0; i < sjsd->num_ports; i++) {
		tmpcnt = dmx(
			bufs[i], mst->buffer+sjsd->readpos, tmplen, j, sjsd);
		if (j < sjsd->channels)
			j++;
	}
	/* fill the rest with silence */
	tmplen = nframes - tmplen;
	if (tmplen > 0) {
		JACK_DEBUG_S("fill up with silence\n");
		sound_jack_silence(bufs, tmplen, sjsd);
	}

	/* modify readposition and overfill */
	sjsd->readpos += tmpcnt * sjsd->framesize;
	sjsd->overfill -= tmpcnt;

	/* detect end of track */
	if (sjsd->underrun >= 4 && sjsd->overfill < 64)
		mst->up->play_state = MTPSTATE_STOP;

	return 0;
}


#undef MYSELF

/* sound-jack.c ends here */
