/* media-mad.c - analyse audio files or streams

   Copyright (C) 2006 Sebastian Freundt

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


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

#include "sysfile.h"

#include "media-mad.h"

#define MYSELF MDRIVER_MAD

Lisp_Object Qmad;

#define __MAD_DEBUG__(args...)		fprintf(stderr, "MAD " args)
#ifndef MAD_DEBUG_FLAG
#define MAD_DEBUG(args...)
#else
#define MAD_DEBUG(args...)		__MAD_DEBUG__(args)
#endif
#define MAD_DEBUG_S(args...)		MAD_DEBUG("[stream]: " args)
#define MAD_DEBUG_DEC(args...)		MAD_DEBUG("[decoder]: " args)
#define MAD_CRITICAL(args...)		__MAD_DEBUG__("CRITICAL: " args)


DECLARE_MEDIA_DRIVER_OPEN_METH(media_mad);
DECLARE_MEDIA_DRIVER_CLOSE_METH(media_mad);
DECLARE_MEDIA_DRIVER_READ_METH(media_mad);
DECLARE_MEDIA_DRIVER_REWIND_METH(media_mad);

DEFINE_MEDIA_DRIVER_CUSTOM(media_mad,
			   media_mad_open, media_mad_close,
			   NULL, NULL,
			   media_mad_read, NULL,
			   media_mad_rewind, NULL);

DECLARE_MEDIA_SAMPLE_FORMAT(sxe_msf_MAD);


static void
media_mad_close(ms_driver_data_t data)
{
	mad_decoder_t *madd = data;

	MAD_DEBUG_DEC("closing decoder handle: 0x%x\n",
		      (unsigned int)madd);
	if (madd->synth)
		xfree(madd->synth);
	madd->synth = NULL;

	if (madd->stream)
		xfree(madd->stream);
	madd->stream = NULL;

	if (madd->frame)
		xfree(madd->frame);
	madd->frame = NULL;

	if (madd->fp)
		fclose(madd->fp);
	madd->fp = NULL;

	if (madd->sd)
		xfree(madd->sd);
	madd->sd = NULL;

	return;
}

static ms_driver_data_t
media_mad_open(Lisp_Media_Stream *ms)
{
	media_substream *mss;
	mtype_audio_properties *mtap;
	/* libmad stuff */
	mad_decoder_t *madd = NULL;
	media_data *sd = NULL;

	/* initialise */
	sd = xnew_and_zero(media_data);
	madd = xnew_and_zero(mad_decoder_t);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file = NULL;
		/* file stuff */
		FILE *f = NULL;
		size_t file_len = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		TO_EXTERNAL_FORMAT(LISP_STRING, mkfp->filename,
				   ALLOCA, (file, file_len), Qnil);
		if ( file != NULL ) {
			/* read in the file */
			f = fopen(file, "rb");
		}
		if (!f) {
			media_stream_set_meths(ms, NULL);
			media_stream_driver(ms) = MDRIVER_UNKNOWN;
			xfree(sd);
			xfree(madd);
			return NULL;
		}
		fseek(f, 0, SEEK_END);
		file_len = ftell(f);
		if ( file_len < 0) {
			fclose(f);
			media_stream_set_meths(ms, NULL);
			media_stream_driver(ms) = MDRIVER_UNKNOWN;
			xfree(sd);
			xfree(madd);
			return NULL;
		}
		fseek(f, 0, SEEK_SET);

		/* now copy into media_data structure */
		sd->data = xmalloc_atomic(file_len+1);
		sd->seek = 0;
		sd->length = file_len;

		/* THIS IS VEEEEEERY BAD! */
		if (fread(sd->data, sizeof(char), file_len, f) < file_len)
			MAD_DEBUG("could not read complete file\n");
		madd->fp = f;

		/* store the filesize */
		mkfp->filesize = file_len;
		break;
	}
	case MKIND_STRING: {
		mkind_string_properties *mksp = NULL;

		/* prepare our user_data */
		mksp = media_stream_kind_properties(ms).sprops;
		sd->length = mksp->size;
		sd->seek = 0;
		sd->data = mksp->stream_data;

		break;
	}

	case MKIND_UNKNOWN:
	case MKIND_FIFO:
	case MKIND_STREAM:
	case NUMBER_OF_MEDIA_KINDS:
	default:
		break;
	}

	/* now initialise our decoder struct */
	madd->synth = xnew_and_zero(struct mad_synth);
	madd->stream = xnew_and_zero(struct mad_stream);
	madd->frame = xnew_and_zero(struct mad_frame);

	mad_synth_init(madd->synth);
	mad_stream_init(madd->stream);
	mad_frame_init(madd->frame);

	mad_stream_buffer(madd->stream, (unsigned char*)sd->data, sd->length);
	madd->have_frame = (mad_frame_decode(madd->frame, madd->stream) == 0);
	if (!madd->have_frame) {
		media_stream_set_meths(ms, NULL);
		media_stream_driver(ms) = MDRIVER_UNKNOWN;
		media_mad_close(madd);
		xfree(madd);
		xfree(sd);
		return NULL;
	}
	madd->sd = sd;

	/* create a substream */
	mss = make_media_substream_append(ms);
	
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = (madd->frame->header.mode ==
			  MAD_MODE_SINGLE_CHANNEL) ? 1 : 2;
	mtap->samplerate = madd->frame->header.samplerate;
	mtap->bitrate = madd->frame->header.bitrate;
	mtap->samplewidth = 16;
	mtap->framesize = mtap->channels * 2;
	mtap->msf = sxe_msf_MAD;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_stream_set_meths(ms, media_mad);

	/* keep the SNDFILE context */
	media_stream_data(ms) = madd;
	media_substream_data(mss) = madd;
	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;

	return madd;
}


/* utility to scale and round samples to 16 bits */

DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(sxe_msf_MAD);

static void
sxe_msf_MAD_up(void *d, void *s, size_t len)
{
	/* convert MAD samples to internal format (S24in32) */
	int i;
	int32_t *dst = d;
	mad_fixed_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("upsampling MAD->internal: %u samples\n", len);

	for (i = len-1; i >= 0; i--) {
		src[i] += (1L << (MAD_F_FRACBITS - 24));
		if (src[i] >= MAD_F_ONE)
			src[i] = MAD_F_ONE - 1;
		else if (src[i] < -MAD_F_ONE)
			src[i] = -MAD_F_ONE;

		dst[i] = (int32_t)(src[i] >> (MAD_F_FRACBITS + 1 - 24));
	}

	return;
}

static void
sxe_msf_MAD_down(void *d, void *s, size_t len)
{
	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("downsampling internal->MAD: %u samples\n", len);

	MAD_CRITICAL(".oO{ Which Id10T called this?! }\n");

	return;
}


static size_t
media_mad_read(media_substream *mss, void *outbuf, size_t length)
{
/* read at most `length' frames into `outbuf' */
	/* stream stuff */
	Lisp_Media_Stream *ms = mss->up;
	mtype_audio_properties *mtap;
	/* libsndfile stuff */
	mad_decoder_t *madd = NULL;
	struct mad_stream *mads = NULL;
	struct mad_frame *madf = NULL;
	struct mad_synth *mady = NULL;
	/* buffering */
	size_t size = 0;
	size_t framesize;
	mad_fixed_t *tmpbuf = (mad_fixed_t*)outbuf;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;


	/* fetch the decoder context and our audio props */
	if (!(madd = media_stream_data(ms)))
		return 0;
	if (!(mtap = media_substream_type_properties(mss).aprops))
		return 0;

	/* fetch framesize */
	framesize = mtap->framesize;

	/* prepare mad */
	mads = madd->stream;
	mady = madd->synth;
	madf = madd->frame;

	MAD_DEBUG("req:%d\n", length);
	while (size <= length) {
		unsigned int nchannels, nframes;
		mad_fixed_t const *left_ch, *right_ch;
		struct mad_pcm *pcm = &mady->pcm;

		if (!madd->have_frame) {
			MAD_DEBUG_DEC("no frames atm, fetching new ones\n");
			madd->have_frame = (mad_frame_decode(madf, mads) == 0);
		}
		if (!madd->have_frame) {
			MAD_DEBUG_DEC("no frames there :(\n");
			return size;
		}

		mad_synth_frame(mady, madf);

		nchannels = pcm->channels;
		nframes  = pcm->length;
		left_ch   = pcm->samples[0];
		right_ch  = pcm->samples[1];

		size += nframes;
	  
		MAD_DEBUG_S("frames: %d, samples: %d, size:%d\n",
			    nframes, nframes*nchannels, size);

		if (nchannels == 2) {
			while (nframes--) {
				*tmpbuf++ = *left_ch++;
				*tmpbuf++ = *right_ch++;
			}
		} else if (nchannels == 1) {
			while (nframes--)
				*tmpbuf++ = *left_ch++;
		}

		madd->have_frame = 0;
	}

	/* upsample the thing */
	MEDIA_SAMPLE_FORMAT_UPSAMPLE(sxe_msf_MAD)(
		outbuf, outbuf, size*mtap->channels);

	return size;
}

static void
media_mad_rewind(media_substream *mss)
{
/* rewind the stream to the first frame */
	/* libavformat */
	mad_decoder_t *madd;
	Lisp_Media_Stream *ms = mss->up;
	unsigned char *sdd = NULL;
	size_t sdl = 0;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;

	/* fetch the format context */
	if (!(madd = media_stream_data(ms)))
		return;

	sdd = (unsigned char*)madd->sd->data;
	sdl = madd->sd->length;

	/* ... and reset the stream */
	madd->have_frame=0;
	mad_synth_init(madd->synth);
	mad_stream_init(madd->stream);
	mad_frame_init(madd->frame);

	mad_stream_buffer(madd->stream, sdd, sdl);

	return;
}

#undef MYSELF
