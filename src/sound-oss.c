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
#include SOUNDCARD_H_FILE	/* Path computed by configure */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include "lisp.h"
#include "sysfile.h"

#include "media-native.h"
#include "sound-oss.h"

Lisp_Object Qoss;		/* cannot be Qnative */

#define MYSELF ADRIVER_OSS

#if 0				/* dont need no stinking signals */
static SIGTYPE(*sighup_handler) (int);
static SIGTYPE(*sigint_handler) (int);
#endif

#if 0
static int mix_fd;
static int audio_vol;
static int audio_fd;
#endif
#define audio_dev "/dev/dsp"



static int
sound_oss_audio_init(int audio_fd, int speed, int channels, int width)
{
	int i;

	if (ioctl(audio_fd, SNDCTL_DSP_SYNC, NULL) < 0) {
		message("SNDCTL_DSP_SYNC");
		return 0;
	}

	/* Initialize sound hardware with preferred parameters */

	/* try to set channels */
	switch (channels) {
	case 1:
		i = 0;
		break;
	case 2:
		i = 1;
		break;
	default:
		message("unsupported number of channels");
		return 0;
	}

	if (ioctl(audio_fd, SNDCTL_DSP_STEREO, &i) < 0 || i+1 != channels) {
		message("OSS cannot set channels");
		return 0;
	}

	/* To eliminate the need for a swap buffer, we set the device
	   to use whatever byte format the client selected. */
	switch (width) {
	case 8:
		i = AFMT_S8;
		break;
        case 16:
		i = AFMT_S16_LE;
	        break;
	default:
		message("unsupported number of bits");
		return 0;
	}

	if (ioctl(audio_fd, SNDCTL_DSP_SAMPLESIZE, &i) < 0) {
		message("OSS cannot set sample size");
		return 0;
	}

	/* The PCSP driver does not support reading of the sampling rate via the
	   SOUND_PCM_READ_RATE ioctl; determine "the_speed" here */
	i = speed;
	if (ioctl(audio_fd, SNDCTL_DSP_SPEED, &i) < 0
	    || i > 1.02 * speed || i < 0.98 * speed) {
		message("OSS cannot set rate");
		return 0;
	}

#if 0
	/* Use the mixer device for setting the playback volume */
	if (audio_fd > 0) {
		int vol = 100;
		vol |= 256 * 100;
		/* Do not signal an error, if volume control is unavailable! */
		ioctl(audio_fd, SOUND_MIXER_WRITE_PCM, &vol);
	}
#endif

	return 1;
}


int sound_oss_play_stream(media_subthread *mst)
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
	sound_oss_data *saod = NULL;
	/* native stuff */
	int audio_fd = -1;
	/* buffering */
	char *buffer = NULL;
	char *bptr;
	uint32_t len;
	int32_t natlen;
	int32_t written;
	int resolution;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;

	/* unpack device */
	lad = get_audio_device(device);
	saod = get_audio_device_data(device);

	/* cannot use AO on incomplete or corrupt audio devices */
	if (lad == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* open /dev/dsp */
	if ((audio_fd = open(audio_dev, O_WRONLY, 0)) < 0)
		return 0;

	/* okay, njsf said /dev/dsp writing is not mt safe,
	 * also i hate OSS, so let's block everything here :) -hroptatyr
	 */

#ifdef HAVE_THREADS
	pthread_mutex_lock(&mss->substream_mutex);
#endif

	if (!sound_oss_audio_init(audio_fd,
				  mtap->samplerate,
				  mtap->channels,
				  mtap->samplewidth))
		goto END_OF_PLAY;

	/* rewind the stream */
	media_substream_rewind(mss);

	/* play chunks of the stream */
	buffer = xmalloc(SOUND_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {

		natlen = len * mtap->framesize;
		bptr = buffer;

		while (natlen > 0) {
			if ((written = write(audio_fd, bptr, natlen)) < 0) {
				message("error in write");
				natlen = 0;
			} else if (written) {
				natlen -= written;
				bptr += written;
			} else {
				natlen = 0;
				ioctl(audio_fd, SNDCTL_DSP_SYNC, NULL);
			}
		}

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

END_OF_PLAY:
	/* Now cleanup all used resources */
	if (buffer)
		free(buffer);

	ioctl(audio_fd, SNDCTL_DSP_SYNC, NULL);
	ioctl(audio_fd, SNDCTL_DSP_RESET, NULL);

	close(audio_fd);
	audio_fd = -1;

#ifdef HAVE_THREADS
	pthread_mutex_unlock(&mss->substream_mutex);
#endif

	return 1;
}

#undef MYSELF
