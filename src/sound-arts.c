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

#define MYSELF ADRIVER_ARTS

#define __ARTS_DEBUG__(args...)		fprintf(stderr, "ARTS " args)
#ifndef ARTS_DEBUG_FLAG
#define ARTS_DEBUG(args...)
#else
#define ARTS_DEBUG(args...)		__ARTS_DEBUG__(args)
#endif
#define ARTS_DEBUG_C(args...)		ARTS_DEBUG("[connection]: " args)
#define ARTS_DEBUG_S(args...)		ARTS_DEBUG("[stream]: " args)
#define ARTS_DEBUG_COE(args...)		ARTS_DEBUG("[coerce]: " args)
#define ARTS_CRITICAL(args...)		__ARTS_DEBUG__("CRITICAL: " args)


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_arts);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_arts);


static Lisp_Object
sound_arts_mark(ad_device_data *devdata)
{
	sound_arts_data *sad = devdata;

	sad = NULL;;

	return Qnil;
}

static void
sound_arts_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_arts_data *sad = NULL;

	sad = get_audio_device_data(device);

	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sad == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */


	return;
}


static ad_device_data *
sound_arts_create(Lisp_Object arts_options)
{
	sound_arts_data *sad;
	Lisp_Object opt_server = Qnil;

	/* parse options */
	opt_server = Fplist_get(arts_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	}

	/* -- initialise -- */
	sad = xnew_and_zero(sound_arts_data);

	ARTS_DEBUG_C("created: 0x%x\n", (unsigned int)sad);

	return sad;
}

static void
sound_arts_finish(ad_device_data *data)
{
	sound_arts_data *sad = data;

	ARTS_DEBUG("finishing ARTS: 0x%x\n",
		   (unsigned int)sad);

	ARTS_DEBUG("audio-device finished.\n");

	return;
}


static int
arts_push(media_subthread *mst, int nframes)
{
	size_t len = 0, tmplen = 0;
	sxe_media_sample_t *tmpbuf;
	sound_arts_subthread_data *sasd = media_subthread_data(mst);
	int i;

	tmpbuf = (sxe_media_sample_t*)(mst->buffer);
	len = media_stream_meth(mst->substream->up, read)(
		mst->substream, mst->buffer, nframes);

	/* set up the volume args */
	sasd->volargs->volume[0] = sasd->volargs->volume[1] =
		mst->up->volume;
	/* set up the rerate args */
	sasd->rrargs->tweak = mst->up->ratetrafo;

	/* coerce the stuff */
	tmplen = sasd->mtap->channels*len;
	for (i = 0; i < sasd->coe_ch_cnt; i++) {
		ARTS_DEBUG_COE("calling coerce "
			      "%d on b:0x%x l:%d\n",
			      i, (unsigned int)tmpbuf, tmplen);
		tmplen = CALL_MEDIA_SAMPLE_EFFECT(
			sasd->coe_chain, i, tmpbuf, tmpbuf, tmplen);
	}
	/* bring back to S16 or U8 */
	MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(sasd->msf)(
		mst->buffer, mst->buffer, tmplen);

	ARTS_DEBUG_S("writing %u samples...\n", tmplen);
	i = 0;
	tmplen *= sasd->framesize/sasd->mtap->channels;
	while (i < 500 &&	/* very arbitrary choice */
	       !(len = arts_write(sasd->as, mst->buffer, tmplen))) {
		i++;
		usleep(100);
	}
	ARTS_DEBUG_S("ACK (written %u)\n", len);

	return len;
}

static int
sound_arts_play(media_subthread *mst)
{
	/* stream stuff */
	Lisp_Media_Stream *ms;
	media_substream *mss;
	Lisp_Media_Thread *mt;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_arts_data *sad = NULL;
	/* buffering */
	int resolution;
	/* subthread stuff */
	sound_arts_subthread_data _sasd, *sasd = &_sasd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;

	SOUND_UNPACK_MT(mt, mst, device, ms, mss, lad, sad, sasd->mtap);

	if (sad->lock) {
		ARTS_DEBUG_C("Device locked.\n");
		message(GETTEXT("audio-arts: "
				"Device locked."));
		/* this lock is probably unnecessary */
		return 0;
	}

	sad->lock = 1;

	/* trigger arts */
	if (arts_init() == 0) {
		char tmp[16];
		snprintf(tmp, 15, "SXEmacs%lx", pthread_self());
		sasd->as = arts_play_stream(sasd->mtap->samplerate,
					    16 /* HARDCODED */,
					    sasd->mtap->channels, tmp);
		arts_stream_set(sasd->as, ARTS_P_BLOCKING, 0);
	} else {
		message(GETTEXT("Connecting to aRts daemon failed."));
		sad->lock = 0;
		return 0;
	}

	/* fill the subthread data structure */
	sasd->msf = MEDIA_SAMPLE_FORMAT(sxe_msf_S16);
	sasd->framesize = sasd->mtap->channels * sizeof(int16_t);
	sasd->coe_ch_cnt = 0;
	media_subthread_data(mst) = sasd;

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sasd->coe_chain, sasd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = sasd->mtap->channels;
	sasd->volargs = volargs;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sasd->coe_chain, sasd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = sasd->mtap->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;
	sasd->rrargs = rrargs;

	/* rewind it ... */
	media_stream_meth(ms, rewind)(mss);

	mst->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	mst->buffer = xmalloc(mst->buffer_alloc_size);
	resolution = (sasd->mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;

	while (mt->play_state != MTPSTATE_STOP) {

		mtp = mt->play_state;
		switch (mtp) {
		case MTPSTATE_RUN:
			if (!arts_push(mst, resolution))
				mt->play_state = MTPSTATE_STOP;
			break;
		case MTPSTATE_PAUSE:
			ARTS_DEBUG("sleeping for %d\n", resolution);
			usleep(resolution);
			break;
		default:
			ARTS_DEBUG("ACK, quit\n");
			mt->play_state = MTPSTATE_STOP;
			break;
		}
	}

	/* -- Close and shutdown -- */
	if (mst->buffer)
		xfree(mst->buffer);
	mst->buffer = NULL;

	if (sasd && sasd->as) {
		arts_close_stream(sasd->as);
		sasd->as = NULL;
	}
	sasd = NULL;

	arts_free();
	sad->lock = 0;
	return 1;
}

#undef MYSELF

/* sound-arts.c ends here */
