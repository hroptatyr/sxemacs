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

#define SOUND_JACK_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec @ 48kHz 32bit 2ch */
#define MYSELF ADRIVER_JACK

#define MAX_CHANS 6
static jack_port_t *ports[MAX_CHANS];
static int num_ports; /* Number of used ports == number of channels */
static volatile int underrun = 0; /* signals if an underrun occured */
static volatile int read_pos;
static char *buffer = NULL;

static int sound_jack_fill_buffer(float**, int, int);
static void sound_jack_silence(float**, int, int);
static int sound_jack_process(jack_nframes_t, void*);


char *sound_jack_subprint(Lisp_Object device)
{
	return NULL;
}


int sound_jack_play_stream(media_subthread *mst)
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
	sound_jack_data *saod = NULL;
	/* jack stuff */
	jack_client_t *client = NULL;
	int port_flags = 0;
	const char **matching_ports = NULL;
	int i;
	/* buffering */
	uint32_t len;
	int resolution;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;

	/* unpack device */
	lad = get_audio_device(device);
	saod = get_audio_device_data(device);

	/* cannot use Jack on incomplete or corrupt audio devices */
	if (lad == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* cannot use Jack to play non-audio stuff */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* Create a new playback client */
	client = jack_client_new("SXEmacs" /* client_name */);
	if (!client) {
		message(GETTEXT("audio-jack: "
				"cannot open server."));
		return 0;
	}
	jack_set_process_callback(client, sound_jack_process, 0);

	/* list matching ports */
	port_flags |= JackPortIsInput;
	port_flags |= JackPortIsPhysical;
	matching_ports = jack_get_ports(client, NULL, NULL, port_flags);
	for (num_ports = 0; matching_ports && matching_ports[num_ports];
	     num_ports++);	/* just count */
	if (!num_ports) {
		message(GETTEXT("audio-jack: "
				"no physical ports available."));
		goto finish;
	}
	if (mtap->channels > num_ports);

	// create out output ports
	for (i = 0; i < num_ports; i++) {
		char pname[30];
		snprintf(pname, 30, "SXEmacs out_%d", i);
		ports[i] = jack_port_register(client, pname,
					      JACK_DEFAULT_AUDIO_TYPE,
					      JackPortIsOutput, 0);
		if (!ports[i]) {
			message(GETTEXT("audio-jack: "
					"not enough ports available."));
			goto finish;
		}
	}
	if (jack_activate(client)) {
		message(GETTEXT("audio-jack: "
				"activate failed."));
		goto finish;
	}
	for (i = 0; i < num_ports; i++) {
		if (jack_connect(client,
				 jack_port_name(ports[i]), matching_ports[i])) {
			message(GETTEXT("audio-jack: "
					"connecting failed."));
			goto finish;
		}
	}

	/* rewind it ... */
	media_substream_rewind(mss);

	/* ... and play it */
	buffer = xmalloc(SOUND_JACK_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		;

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* close and shutdown */
finish:
	xfree(matching_ports);
	if (client)
		jack_client_close(client);
	if (buffer)
		xfree(buffer);
	return 1;
}


/**
 * \brief read data from buffer and splitting it into channels
 * \param bufs num_bufs float buffers, each will contain the data of one channel
 * \param cnt number of samples to read per channel
 * \param num_bufs number of channels to split the data into
 * \return number of samples read per channel, equals cnt unless there was too
 *         little data in the buffer
 *
 * Assumes the data in the buffer is of type float, the number of bytes
 * read is res * num_bufs * sizeof(float), where res is the return value.
 */
static int
sound_jack_fill_buffer(float **bufs, int cnt, int num_bufs)
{
	int first_len = SOUND_JACK_MAX_AUDIO_FRAME_SIZE - read_pos;
	int i, j;

	for (i = 0; i < cnt; i++) {
		for (j = 0; j < num_bufs; j++) {
			bufs[j][i] = *((float*)(&buffer[read_pos]));
			read_pos = (read_pos + sizeof(float)) %
				SOUND_JACK_MAX_AUDIO_FRAME_SIZE;
		}
	}
	return cnt;
}

/**
 * \brief fill the buffers with silence
 * \param bufs num_bufs float buffers, each will contain the data of one channel
 * \param cnt number of samples in each buffer
 * \param num_bufs number of buffers
 */
static void
sound_jack_silence(float **bufs, int cnt, int num_bufs)
{
	int i, j;
	for (i = 0; i < cnt; i++)
		for (j = 0; j < num_bufs; j++)
			bufs[j][i] = 0;
}

/**
 * \brief JACK Callback function
 * \param nframes number of frames to fill into buffers
 * \param arg unused
 * \return currently always 0
 *
 * Write silence into buffers if paused or an underrun occured
 */
static int
sound_jack_process(jack_nframes_t nframes, void *arg)
{
	float *bufs[MAX_CHANS];
	int i;

	for (i = 0; i < num_ports; i++)
		bufs[i] = jack_port_get_buffer(ports[i], nframes);
	if (!underrun)
		if (sound_jack_fill_buffer(bufs, nframes, num_ports) < nframes)
			underrun = 1;
	if (underrun)
		sound_jack_silence(bufs, nframes, num_ports);
	return 0;
}

#undef MYSELF

/* sound-jack.c ends here */
