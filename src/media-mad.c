/* media-mad.c - analyse audio files or streams

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

#include "buffer.h"
#include "sysdep.h"
#include "sysfile.h"

#include "media-mad.h"

static uint32_t media_mad_sread_audio(media_substream*, void*, uint32_t);
static void media_mad_srewind_audio(media_substream*);

#define MYSELF MDRIVER_MAD

typedef struct mad_decoder_s {

  struct mad_synth  synth; 
  struct mad_stream stream;
  struct mad_frame  frame;
  
  int have_frame;

  int               output_sampling_rate;
  int               output_open;
  int               output_mode;

} mad_decoder_t;


void media_mad_analyse_stream(Lisp_Media_Stream *ms)
{
	media_substream *mss;
	mtype_audio_properties *mtap;
	char *name;
	/* libmad stuff */
	mad_decoder_t *madd = NULL;
	struct mad_stream *mads = NULL;
	struct mad_frame *madf = NULL;
	sound_data *sd = NULL;

	/* initialise */
	sd = xnew_and_zero(sound_data);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;
		/* file stuff */
		FILE *f;
		size_t file_len = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		file = mkfp->filename;

		/* read in the file */
		f = fopen(file, "rb");
		if (!f) {
			xfree(sd);
			return;
		}
		fseek(f, 0, SEEK_END);
		file_len = ftell(f);
		fseek(f, 0, SEEK_SET);

		/* now copy into sound_data structure */
		sd->data = xmalloc(file_len+1);
		sd->seek = 0;
		sd->length = file_len;
		if (fread(sd->data, sizeof(char), file_len, f) < file_len);
		fclose(f);

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
	default:
		break;
	}

	/* now initialise our decoder struct */
	madd = xnew_and_zero(mad_decoder_t);
	mads = &madd->stream;
	madf = &madd->frame;

	mad_synth_init(&madd->synth);
	mad_stream_init(mads);
	mad_frame_init(madf);

	mad_stream_buffer(mads, sd->data, sd->length);
	madd->have_frame = (mad_frame_decode(madf, mads) == 0);
	if (!madd->have_frame) {
		xfree(madd);
		return;
	}
	xfree(sd);

	/* create a substream */
	mss = make_media_substream_append(ms);
	
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = (madf->header.mode ==
			  MAD_MODE_SINGLE_CHANNEL) ? 1 : 2;
	mtap->samplerate = madf->header.samplerate;
	mtap->bitrate = madf->header.bitrate;
	mtap->samplewidth = 16;
	mtap->framesize = mtap->channels * 2;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_substream_sread(mss) = media_mad_sread_audio;
	media_substream_srewind(mss) = media_mad_srewind_audio;

	/* keep the SNDFILE context */
	media_stream_data(ms) = madd;
	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;
}


/* utility to scale and round samples to 16 bits */
static inline signed int
scale(mad_fixed_t sample)
{
	/* round */
	sample += (1L << (MAD_F_FRACBITS - 16));

	/* clip */
	if (sample >= MAD_F_ONE)
		sample = MAD_F_ONE - 1;
	else if (sample < -MAD_F_ONE)
		sample = -MAD_F_ONE;

	/* quantize */
	return sample >> (MAD_F_FRACBITS + 1 - 16);
}

static uint32_t
media_mad_sread_audio(media_substream *mss, void *outbuf, uint32_t length)
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
	u_int32_t size = 0;
	uint16_t framesize;

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
	mads = &madd->stream;
	mady = &madd->synth;
	madf = &madd->frame;

	while (size <= length*framesize) {
		unsigned int         nchannels, nsamples;
		mad_fixed_t const   *left_ch, *right_ch;
		struct mad_pcm      *pcm = &mady->pcm;
		uint16_t            *output = (uint16_t*)outbuf;

		if (!madd->have_frame)
			madd->have_frame = (mad_frame_decode(madf, mads) == 0);
		if (!madd->have_frame)
			return size/framesize;

		mad_synth_frame(mady, madf);

		nchannels = pcm->channels;
		nsamples  = pcm->length;
		left_ch   = pcm->samples[0];
		right_ch  = pcm->samples[1];

		size += framesize*nsamples;
		outbuf += framesize*nsamples;
	  
#ifdef MEDIA_MAD_DEBUG
		fprintf(stderr, "samples: %d, size:%d\n",
			nsamples, size);
#endif
		while (nsamples--) {
			/* output sample(s) in 16-bit signed little-endian PCM */
			*output++ = scale(*left_ch++);
	    
			if (nchannels == 2) 
				*output++ = scale(*right_ch++);
		}

		madd->have_frame = 0;
	}

	return size/framesize;
}

static void
media_mad_srewind_audio(media_substream *mss)
{
/* rewind the stream to the first frame */
	/* libavformat */
	mad_decoder_t *madd;
	Lisp_Media_Stream *ms = mss->up;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;

	/* fetch the format context */
	if (!(madd = media_stream_data(ms)))
		return;

	/* ... and reset the stream */
	madd->have_frame=0;
	mad_synth_init(&madd->synth);
	mad_stream_init(&madd->stream);
	mad_frame_init(&madd->frame);
}

#undef MYSELF
