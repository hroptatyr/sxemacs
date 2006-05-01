/* sound-ao.c - play a sound over the libao devices

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
#include "sound-ao.h"

Lisp_Object Qao;

#define MYSELF ADRIVER_AO
#define SOUND_AO_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec of 48kHz, 32bit, stereo */


char *sound_ao_subprint(Lisp_Object device)
{
	sound_ao_data *saod = NULL;
	char *dr_name;
	char *out;

	saod = get_audio_device_data(device);
	/* cannot use AO on incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || saod == NULL) {
		dr_name = xmalloc(10);
		memcpy(dr_name, "VOID", 4);
		dr_name[4] = '\0';
	} else
		dr_name = ao_driver_info(saod->driver_id)->short_name;

	/* now that we are here, mark AO device as dead if so */
	if (XAUDIO_DEVICE_DRIVER(device) == MYSELF && saod == NULL)
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
	else
		XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	out = xmalloc(40+strlen(dr_name));

	/* info about the connected output plugin */
	strcpy(out, " :driver ");
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || saod == NULL);
	else
		strcat(out, "\"");
	strcat(out, dr_name);
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || saod == NULL);
	else
		strcat(out, "\"");

	/* info about the general state of AO */
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

sound_ao_data *sound_ao_create(Lisp_Object ao_options)
{
	int driver;
	ao_device *device;
	ao_option *options;
	ao_sample_format *fmt;
	/* result */
	sound_ao_data *aod;
	/* option keywords */
	Lisp_Object opt_driver;
	char *optext_driver = NULL;

	/* parse options */
	opt_driver = Fplist_get(ao_options, intern(":driver"), Qnil);
	if (!NILP(opt_driver) && !STRINGP(opt_driver)) {
		wrong_type_argument(Qstringp, opt_driver);
		return NULL;
	} else if (STRINGP(opt_driver))
		optext_driver = (char*)XSTRING_DATA(opt_driver);

	/* -- initialise -- */
	ao_initialize();
	fmt = malloc(sizeof(ao_sample_format));

	/* -- Setup for driver -- */
	if (optext_driver != NULL)
		driver = ao_driver_id(optext_driver);
	else
		driver = ao_default_driver_id();

	/* just some generics */
	fmt->channels = 2;
	fmt->rate = 44100;
	fmt->bits = 16;
	fmt->byte_format = AO_FMT_LITTLE;

	options = NULL;

	/* -- Open driver -- */
	device = ao_open_live(driver, fmt, options);
	if (device == NULL) {
		message(GETTEXT("audio-ao: Unsupported driver."));
		xfree(fmt);
		aod = NULL;
	} else {
		aod = xnew_and_zero(sound_ao_data);

		aod->ad = device;
		aod->options = NULL;
		aod->fmt = fmt;
		aod->driver_id = driver;
	}

	return aod;
}

void sound_ao_finish(sound_ao_data *ao_dev)
{
	if (ao_dev != NULL) {
		ao_close(ao_dev->ad);
		free(ao_dev->fmt);
		ao_shutdown();
	}
	return;
}


int sound_ao_play_stream(media_subthread *mst)
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
	sound_ao_data *saod = NULL;
	/* libao stuff */
	ao_device *dev;
	ao_sample_format *format;
	ao_option *options;
	int driver;
	/* buffering */
	char *buffer;
	uint32_t len;
	int resolution;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;;

	/* unpack device */
	lad = get_audio_device(device);
	saod = get_audio_device_data(device);

	/* cannot use AO on incomplete or corrupt audio devices */
	if (lad == NULL || saod == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;

	/* setup for the driver */
	driver = saod->driver_id;
	format = saod->fmt;
	options = saod->options;

	/* setup format specs from audio props */
	format->channels = mtap->channels;
	format->rate = mtap->samplerate;
	format->bits = mtap->samplewidth;
	format->byte_format = AO_FMT_LITTLE; /* hmpf */

	/* open the driver */
	dev = ao_open_live(driver, format, options);
	if (dev == NULL) {
		message(GETTEXT("audio: Unsupported device."));
		return 0;
	}

	/* rewind it ... */
	media_substream_rewind(mss);

	/* ... and play it */
	buffer = xmalloc(SOUND_AO_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {
		ao_play(dev, buffer, len*mtap->framesize);

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* -- Close and shutdown -- */
	xfree(buffer);
	ao_close(dev);
	return 1;
}

#undef MYSELF

/* sound-ao.c ends here */
