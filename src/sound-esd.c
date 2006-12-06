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

#define MYSELF ADRIVER_ESD

Lisp_Object Qesd;

#define __ESD_DEBUG__(args...)		fprintf(stderr, "ESD " args)
#ifndef ESD_DEBUG_FLAG
#define ESD_DEBUG(args...)
#else
#define ESD_DEBUG(args...)		__ESD_DEBUG__(args)
#endif
#define ESD_DEBUG_HW(args...)		ESD_DEBUG("[hardware]: " args)
#define ESD_DEBUG_S(args...)		ESD_DEBUG("[stream]: " args)
#define ESD_DEBUG_COE(args...)		ESD_DEBUG("[coerce]: " args)
#define ESD_CRITICAL(args...)		__ESD_DEBUG__("CRITICAL: " args)


DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_esd);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_esd);


static Lisp_Object
sound_esd_mark(ad_device_data *devdata)
{
	sound_esd_data *sed = devdata;

	if (sed == NULL)
		return Qnil;

	mark_object(sed->server);

	return Qnil;
}

static void
sound_esd_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_esd_data *sed = NULL;

	sed = get_audio_device_data(device);
	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || sed == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :device ", pcfun);
	if (NILP(sed->server))
		write_c_string("#default", pcfun);
	else
		print_internal(sed->server, pcfun, ef);

	return;
}


static int
sound_esd_close_device(sound_esd_data *sed)
{
	sed->lock = 0;

	ESD_DEBUG_HW("socket closed\n");

	return 0;
}

static int
sound_esd_init_socket(sound_esd_data *sed, sound_esd_subthread_data *sesd)
{
	/* convert header information into ESD flags */
	sesd->flags = 0;
	sesd->flags = ESD_STREAM | ESD_PLAY;
	sesd->flags |= ESD_BITS16;
	sesd->samplewidth = 16;
	sesd->channels = sesd->mtap->channels;
	sesd->framesize = sesd->channels * sizeof(int16_t);
	sesd->samplerate = sesd->mtap->samplerate;

	switch (sesd->mtap->channels) {
	case 1:
		sesd->flags |= ESD_MONO;
		break;
	case 2:
		sesd->flags |= ESD_STEREO;
		break;
	case 5:
		sesd->flags |= ESD_STEREO;
		ADD_MEDIA_SAMPLE_EFFECT(
			sesd->coe_chain, sesd->coe_ch_cnt,
			MEDIA_SAMPLE_EFFECT(sxe_mse_5ch_to_2ch), NULL);
		break;
	default:
		message(GETTEXT("audio-esd: "
				"%d channels - only 1 or 2 supported"),
			sesd->mtap->channels);
		return -1;
	}

	return 1;
}

static ad_device_data *
sound_esd_create(Lisp_Object esd_options)
{
	/* result */
	sound_esd_data *sed = NULL;
	/* option keywords */
	Lisp_Object opt_server;
	Lisp_Object opt_port;

	/* parse options */
	opt_server = Fplist_get(esd_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	}

	opt_port = Fplist_get(esd_options, intern(":port"), Qnil);
	if (!NILP(opt_port) && !NATNUMP(opt_port)) {
		wrong_type_argument(Qnatnump, opt_port);
		return NULL;
	}

	/* initialise and fill */
	sed = xnew_and_zero(sound_esd_data);
	sed->server = opt_server;

	return (ad_device_data*)sed;
}

static void
sound_esd_finish(ad_device_data *data)
{
	sound_esd_data *sed = data;

	sed->lock = 1;
	sed->keep_open = 0;
	sound_esd_close_device(sed);

	ESD_DEBUG("audio-device finished.\n");

	return;
}


static int
sound_esd_play(media_subthread *mst)
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
	sound_esd_data *sed = NULL;
	/* buffering */
	int i;
	size_t len, tmplen;
	sxe_media_sample_t *tmpbuf;
	/* esd socket stuff */
	ssize_t wrtn;
	int resolution;
	char *hoststr = NULL;
	/* subthread stuff */
	sound_esd_subthread_data _sesd, *sesd = &_sesd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;

	/* unpack the media thread */
	SOUND_UNPACK_MT(mt, mst, device, ms, mss, lad, sed, sesd->mtap);

	/* this lock is totally unnecessary */
	sed->lock = 1;

	/* init the sesd */
	sesd->samplerate = sesd->samplewidth = sesd->channels = 0;
	sesd->framesize = 0;
	sesd->coe_ch_cnt = 0;

	/* init the socket */
	sound_esd_init_socket(sed, sesd);

	/* hm, use the given options */
	hoststr = (NILP(sed->server) ? NULL : (char*)XSTRING_DATA(sed->server));
	sesd->sock = esd_play_stream(
		sesd->flags, sesd->samplerate, hoststr, "SXEmacs");
	if (sesd->sock < 0)
		return 0;

	/* rewind the stream */
	media_stream_meth(ms, rewind)(mss);

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sesd->coe_chain, sesd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = sesd->channels;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		sesd->coe_chain, sesd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = sesd->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;

	ESD_DEBUG_COE("have %d coerce functions in my chain.\n",
		      sesd->coe_ch_cnt);

	XAUDIO_DEVICE_STATE(device) = ASTATE_ALIVE;

	/* play chunks of the stream */
	mst->buffer = xmalloc(SOUND_MAX_AUDIO_FRAME_SIZE);
	tmpbuf = (sxe_media_sample_t*)mst->buffer;
	resolution = (sesd->mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;

	while (mt->play_state != MTPSTATE_STOP) {

		mtp = mt->play_state;
		switch (mtp) {
		case MTPSTATE_RUN:
			len = media_stream_meth(ms, read)(
				mss, mst->buffer, resolution);
			if (!len) {
				ESD_DEBUG_S("finished\n");
				mt->play_state = MTPSTATE_STOP;
				break;
			}

			/* set up the volume args */
			volargs->volume[0] = volargs->volume[1] =
				mst->up->volume;
			/* set up the rerate args */
			rrargs->tweak = mst->up->ratetrafo;

			/* coerce the stuff,
			   tmplen becomes the number of samples */
			tmplen = sesd->channels*len;
			for (i = 0; i < sesd->coe_ch_cnt; i++) {
				ESD_DEBUG_COE("calling coerce "
					      "%d on b:0x%x l:%d\n",
					      i, (unsigned int)tmpbuf, tmplen);
				tmplen = CALL_MEDIA_SAMPLE_EFFECT(
					sesd->coe_chain, i,
					tmpbuf, tmpbuf, tmplen);
			}

			/* bring back to S16 */
			MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(sxe_msf_S16)(
				mst->buffer, mst->buffer, tmplen);

			wrtn = write(
				sesd->sock, mst->buffer,
				tmplen*sizeof(int16_t));
			if (wrtn < 0) {
				ESD_DEBUG_S("writing to socket failed: %d.\n",
					    wrtn);
				mt->play_state = MTPSTATE_STOP;
			}
			break;
		case MTPSTATE_PAUSE:
			ESD_DEBUG("sleeping for %d\n", resolution);
			usleep(resolution);
			break;
		default:
			ESD_DEBUG("ACK, quit\n");
			mt->play_state = MTPSTATE_STOP;
			break;
		}
	}

	/* close and shutdown */
	if (mst->buffer)
		xfree(mst->buffer);
	mst->buffer = NULL;

	sound_esd_close_device(sed);
	/* we better close the socket now */
	close(sesd->sock);

	return 1;
}

#undef MYSELF

/* sound-esd.c ends here */
