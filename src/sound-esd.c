/* sound-esd.c - play a sound over ESD

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
#include "sound-esd.h"

/* the name given to ESD - I think this should identify ourselves */
#define ESD_NAME "SXEmacs"
#define SOUND_ESD_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec of 48kHz, 32bit, 2ch */

#define MYSELF ADRIVER_ESD

Lisp_Object Qesd;

char *sound_esd_subprint(Lisp_Object device)
{
	sound_esd_data *sed = NULL;
	char *out = xmalloc(64);

	sed = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sed == NULL) {
		memcpy(out, "VOID\000", 5);
	} else
		memcpy(out, "\000", 1);

	/* now that we are here, mark AO device as dead if so */
	if (XAUDIO_DEVICE_DRIVER(device) == MYSELF && sed == NULL)
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
	else
		XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* info about the connected output plugin */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sed == NULL);
	else {
		char *temp = alloca(48);
		snprintf(temp, 47, " :server \"%s\" :port %d",
			 (sed->server ? sed->server : "localhost"),
			 (sed->port ? sed->port : 16001));
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


sound_esd_data *sound_esd_create(Lisp_Object esd_options)
{
	char *server = NULL;
	uint16_t port = 0;
	/* result */
	sound_esd_data *ed = NULL;
	/* option keywords */
	Lisp_Object opt_server;
	Lisp_Object opt_port;

	/* parse options */
	opt_server = Fplist_get(esd_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	} else if (STRINGP(opt_server)) {
		server = xmalloc(64);
		strncpy(server, (char*)XSTRING_DATA(opt_server), 63);
	}

	opt_port = Fplist_get(esd_options, intern(":port"), Qnil);
	if (!NILP(opt_port) && !NATNUMP(opt_port)) {
		wrong_type_argument(Qnatnump, opt_port);
		return NULL;
	} else if (NATNUMP(opt_port))
		port = XINT(opt_port);

	/* initialise and fill */
	ed = xnew_and_zero(sound_esd_data);
	ed->server = server;
	ed->port = port;

	return ed;
}

void sound_esd_finish(sound_esd_data *esd_dev)
{
	if (esd_dev != NULL) {
		if (esd_dev->server)
			xfree(esd_dev->server);
	}
	return;
}


int sound_esd_play_stream(media_subthread *mst)
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
	sound_esd_data *sed = NULL;
	/* buffering */
	char *buffer;
	uint32_t len;
	/* esd socket stuff */
	ssize_t wrtn;
	int flags, sock;
	int resolution;
	char *hoststr = NULL;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;

	/* unpack device */
	lad = get_audio_device(device);
	sed = get_audio_device_data(device);

	/* cannot use ESD on incomplete or corrupt audio devices */
	if (lad == NULL || sed == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* convert header information into ESD flags */
	flags = ESD_STREAM | ESD_PLAY;
	if (mtap->samplewidth <= 8)
		flags |= ESD_BITS8;
	else if (mtap->samplewidth <= 16)
		flags |= ESD_BITS16;
	else {
		message(GETTEXT("audio-esd: "
				"byte format unimplemented"));
		return 0;
	}

	switch (mtap->channels) {
	case 1:
		flags |= ESD_MONO;
		break;
	case 2:
		flags |= ESD_STEREO;
		break;
	default:
		message(GETTEXT("audio-esd: "
				"%d channels - only 1 or 2 supported"),
			mtap->channels);
		return 0;
	}

	/* hm, use the given options */
	if (sed->server || sed->port) {
		hoststr = alloca(256);
		snprintf(hoststr, 255, "%s:%d", sed->server, sed->port);
	}
	sock = esd_play_stream(flags, mtap->samplerate, hoststr, "sxemacs");
	if (sock < 0)
		return 0;

	/* rewind the stream */
	media_substream_rewind(mss);

	/* play chunks of the stream */
	buffer = xmalloc(SOUND_ESD_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		if ((wrtn = write(sock, buffer, len*mtap->framesize)) < 0)
			break;

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* close and shutdown */
	xfree(buffer);
	close(sock);
	return 1;
}

#undef MYSELF

/* sound-esd.c ends here */
