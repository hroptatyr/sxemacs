/* media-sox.c - analyse audio files or streams via sox

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

#include "media-sox.h"

static uint32_t media_sox_sread_audio(media_substream*, void*, uint32_t);
static void media_sox_srewind_audio(media_substream*);

#define MYSELF MDRIVER_SOX

/* called from util.c::st_fail() */
void cleanup(void) 
{
}


void media_sox_analyse_stream(Lisp_Media_Stream *ms)
{
	media_substream *mss;
	mtype_audio_properties *mtap;
	char *name = NULL;
	/* libsndfile stuff */
	ft_t ft = NULL;
	st_signalinfo_t *stinfo = NULL;

	/* initialise */
	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		file = mkfp->filename;
		ft = st_open_read(file, NULL, NULL);
		break;
	}
	case MKIND_STRING: {
		/* not yet handable */
		break;
	}
	default:
		break;
	}

	if (!ft)
		return;

	/* retrieve the signal information */
	stinfo = &ft->info;

	/* create a substream */
	mss = make_media_substream_append(ms);
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = stinfo->channels;
	mtap->samplerate = stinfo->rate;

	/* try to find a read function */
	switch (stinfo->size) {
	case ST_SIZE_8BIT:
		mtap->samplewidth = 8;
		mtap->framesize = mtap->channels;
		break;
	case ST_SIZE_16BIT:
		mtap->samplewidth = 16;
		mtap->framesize = mtap->channels * 2;
		break;
#ifdef ST_SIZE_24BIT
	case ST_SIZE_24BIT:
		mtap->samplewidth = 24;
		mtap->framesize = mtap->channels * 3;
		break;
#endif
	case ST_SIZE_32BIT:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		break;
	case ST_SIZE_64BIT:
		mtap->samplewidth = 64;
		mtap->framesize = mtap->channels * 8;
#if 0
		/* copy in some name */
		name = xmalloc(20);
		memcpy(name, "PCM", 3);
		name[3] = '\0';
		break;
#endif
        default:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
#if 0
		/* copy in some name */
		name = xmalloc(20);
		memcpy(name, "PCM32", 5);
		name[5] = '\0';
#endif
		break;
	}
	mtap->name = name;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_substream_sread(mss) = media_sox_sread_audio;
	media_substream_srewind(mss) = media_sox_srewind_audio;

	/* keep the sox handle */
	media_stream_data(ms) = ft;
	media_substream_data(mss) = ft;

	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;
}


static uint32_t
media_sox_sread_audio(media_substream *mss, void *outbuf, uint32_t length)
{
/* read at most `length' frames into `outbuf' */
	/* stream stuff */
	Lisp_Media_Stream *ms = mss->up;
	mtype_audio_properties *mtap;
	/* libst stuff */
	ft_t ft;
	st_ssize_t samples;
	st_ssize_t samples_cnt;
	st_sample_t *buf;
	st_sample_t *bptr;
	int width;
	int16_t *optr;
	uint16_t framesize;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;

	/* fetch the context and our audio props */
	if (!(ft = media_stream_data(ms)))
		return 0;
	if (!(mtap = media_substream_type_properties(mss).aprops))
		return 0;

	framesize = mtap->framesize;

	/* since sox is using 32bit internally we have to `rescale' the samples
	 * before putting them into outbuf :( */
	width = mtap->samplewidth;
	buf = xmalloc(length*sizeof(st_sample_t));
	samples = st_read(ft, buf, length);
	bptr = buf;
	optr = (int16_t*)outbuf;
	for (samples_cnt = 0; samples_cnt < samples; samples_cnt++) {
		optr[samples_cnt] = (int16_t)(bptr[samples_cnt] >> 16);
	}

	xfree(buf);
	return samples;
}

static void
media_sox_srewind_audio(media_substream *mss)
{
/* rewind the stream to the first frame */
	Lisp_Media_Stream *ms = mss->up;
	/* libst stuff */
	ft_t ft;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return;

	/* fetch the SNDFILE context and our audio props */
	if (!(ft = media_stream_data(ms)))
		return;

	st_seek(ft, 0, SEEK_SET);
}

#undef MYSELF
