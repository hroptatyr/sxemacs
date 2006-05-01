/* sound-arts.c - play a sound over the arts

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
#include "sound-arts.h"

Lisp_Object Qarts;

#define SOUND_ARTS_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec of 48kHz, 32bit, 2ch */
#define MYSELF ADRIVER_ARTS

sound_arts_data *sound_arts_create(Lisp_Object arts_options)
{
#if 0
	/* result */
	sound_arts_data *ad;
#endif

	return NULL;
}

void sound_arts_finish(sound_arts_data *arts_dev)
{
	return;
}


/*
  (setq aod (make-audio-device 'arts))
  (setq s (make-media-stream "/usr/local/share/sounds/KDE_Door.ogg"))
  (setq r (make-media-stream "/usr/local/share/sounds/KDE_Error.wav"))
  (play-media-stream s aod)
  (play-media-stream r aod)
*/
int sound_arts_play_stream(media_subthread *mst)
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
	sound_arts_data *sad = NULL;
	/* artsc stuff */
	arts_stream_t *as;
	/* buffering */
	char *buffer;
	uint32_t len;
	int resolution;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;

	/* unpack device */
	sad = get_audio_device_data(device);
	lad = get_audio_device(device);

	/* cannot use aRts on incomplete or corrupt audio devices */
	/* no? Okay, I could, but since aRts takes so long to wake up
	 * we better let it sleep -hroptatyr
	 */
	if (lad == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio media */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* trigger arts */
	if (arts_init() == 0)
		as = arts_play_stream(mtap->samplerate, mtap->samplewidth,
				      mtap->channels, "SXEmacs");
	else {
		message(GETTEXT("Connecting to aRts daemon failed."));
		return 0;
	}

	/* rewind it ... */
	media_substream_rewind(mss);

	/* ... and play it */
	buffer = xmalloc(SOUND_ARTS_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		arts_write(as, buffer, len*mtap->framesize);

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* -- Close and shutdown -- */
	xfree(buffer);
	arts_close_stream(as);
	arts_free();
	return 1;
}

#undef MYSELF

/* sound-arts.c ends here */
