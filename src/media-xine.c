/* media-xine.c - analyse audio files or streams via xine

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

#include <config.h>
#include "lisp.h"

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "media-xine.h"
#include <xine/xine_internal.h>
#include <xine/audio_out.h>

static uint32_t media_xine_sread_audio(media_substream*, void*, uint32_t);
static void media_xine_srewind_audio(media_substream*);

#define MYSELF MDRIVER_XINE



void media_xine_close_context(media_xine_context *mxc)
{
	if (mxc) {
		if (mxc->stream)
			xine_dispose(mxc->stream);
		if (mxc->audio_port)
			xine_close_audio_driver(mxc->context, mxc->audio_port);
		if (mxc->video_port)
			xine_close_video_driver(mxc->context, mxc->video_port);
		if (mxc->context)
			xine_exit(mxc->context);
	}
	xfree(mxc);
}

/* main analysis function */
void media_xine_analyse_stream(Lisp_Media_Stream *ms)
{
	/* stream stuff */
	media_substream *mss;
	mtype_audio_properties *mtap;
	char *name = NULL;
	/* xine stuff */
	xine_t *xc = NULL;
	xine_audio_port_t *xapt = NULL;
	xine_video_port_t *xvpt = NULL;
	xine_stream_t *xst = NULL;
	/* our container */
	media_xine_context *mxc = NULL;

	/* initialise */
	xc = xine_new();
	xine_init(xc);
	if (xc == NULL) {
		fprintf(stderr, "xc does not work\n");
		return;
	}

	xapt = xine_open_audio_driver(xc, "none", NULL);
	xvpt = xine_open_video_driver(xc, NULL, XINE_VISUAL_TYPE_NONE, NULL);
	if (xapt == NULL || xvpt == NULL) {
		fprintf(stderr, "xapt/xvpt does not work\n");
		if (xapt != NULL)
			xine_close_audio_driver(xc, xapt);
		if (xvpt != NULL)
			xine_close_video_driver(xc, xvpt);
		xine_exit(xc);
		return;
	}

	xst = xine_stream_new (xc, xapt, xvpt);
	if (xst == NULL) {
		fprintf(stderr, "xst does not work\n");
		xine_close_audio_driver(xc, xapt);
		xine_close_video_driver(xc, xvpt);
		xine_exit(xc);
		return;
	}

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;
		int succ = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		file = mkfp->filename;

		succ = xine_open(xst, file);

		if (!succ) {
			fprintf(stderr, "open does not work\n");
			xine_dispose(xst);
			xine_close_audio_driver(xc, xapt);
			xine_close_video_driver(xc, xvpt);
			xine_exit(xc);
			return;
		}
		break;
	}
	case MKIND_STRING: {
		/* not yet handable */
		break;
	}
	default:
		break;
	}

	/* ah, we survived, now create a substream */
	mss = make_media_substream_append(ms);

	/* retrieve the signal information */
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = xine_get_stream_info(
		xst, XINE_STREAM_INFO_AUDIO_CHANNELS);
	mtap->samplerate = xine_get_stream_info(
		xst, XINE_STREAM_INFO_AUDIO_SAMPLERATE);
	mtap->samplewidth = xine_get_stream_info(
		xst, XINE_STREAM_INFO_AUDIO_BITS);
	mtap->framesize = mtap->channels * mtap->samplewidth / 8;
	mtap->bitrate = xine_get_stream_info(
		xst, XINE_STREAM_INFO_AUDIO_BITRATE);

	/* copy in some name */
	name = xmalloc(48);
	strncpy(name, xine_get_meta_info(xst, XINE_META_INFO_AUDIOCODEC), 47);
	mtap->name = name;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_substream_sread(mss) = media_xine_sread_audio;
	media_substream_srewind(mss) = media_xine_srewind_audio;

	/* keep the xine_stream_t context */
	mxc = xnew_and_zero(media_xine_context);
	mxc->context = xc;
	mxc->audio_port = xapt;
	mxc->video_port = xvpt;
	mxc->stream = xst;
	media_stream_data(ms) = mxc;
	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;
}


static uint32_t
media_xine_sread_audio(media_substream *mss, void *outbuf, uint32_t length)
{
/* read at most `length' frames into `outbuf' */
	/* stream stuff */
	Lisp_Media_Stream *ms = mss->up;
	mtype_audio_properties *mtap;
	/* libxine */
	media_xine_context *mxc = NULL;
	xine_audio_frame_t *xaft = NULL;
	buf_element_t *buf = NULL;
	audio_decoder_t *xdec = NULL;
	/* buffering */
	int16_t *buffer;
	int size;
	uint16_t framesize;
	/* result */
	uint32_t bufseek = 0;
	int declen, dec;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;

	/* fetch the format context */
	mxc = media_stream_data(ms);

	if (!mxc) {
		fprintf(stderr, "mxc was NULL\n");
		return 0;
	}

	/* unpack the substream */
	mtap = media_substream_type_properties(mss).aprops;

	/* fetch framesize */
	if (mtap)
		framesize = mtap->framesize;
	else
		return 0;

	/* initialise packets and buffer */
	buffer = xmalloc(MEDIA_MAX_AUDIO_FRAME_SIZE);

	buf = mxc->stream->audio_fifo->get(mxc->stream->audio_fifo);

	fprintf(stderr, "buf is %d\n", buf);

	xdec = _x_get_audio_decoder(mxc->stream, (buf->type >> 16) & 0xff);

	fprintf(stderr, "buf == 0? %d, xdec == 0? %d\n",
		buf == NULL, xdec == NULL);

	/* read a frame and decode it */
	while (bufseek <= length*framesize && buf && xdec) {

		xdec->decode_data(xdec, buf);

		size = buf->size;

		fprintf(stderr, "sz:%d\n", size);

		memcpy(outbuf+bufseek, (char*)buf->content, size);
		bufseek += size;

		buf = buf->next;
	}

	/* shutdown */
	xfree(buffer);

	//bufseek = ip->read(ip, outbuf, length*framesize);

	if (bufseek >= 0)
		return bufseek/framesize;
	else
		return 0;
}

static void
media_xine_srewind_audio(media_substream *mss)
{
/* rewind the stream to the first frame */
#if 0
	/* libst stuff */
	ft_t ft;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;
	if (media_stream_type(ms) != MTYPE_AUDIO)
		return;

	/* fetch the SNDFILE context and our audio props */
	if (!(ft = media_stream_data(ms)))
		return;

	st_seek(ft, 0, SEEK_SET);
#endif
}

#undef MYSELF
