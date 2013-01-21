/* media-sox.c - analyse audio files or streams via sox

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

#include <config.h>
#include "lisp.h"

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

/*
lisp.h defined UNUSED which also gets defined in some versions of SoX
in an incompatible fashion.  We don't need that macro here...
*/
#undef UNUSED
#include "media-sox.h"

#define MYSELF MDRIVER_SOX

Lisp_Object Qsox;

#define __SOX_DEBUG__(args...)		fprintf(stderr, "SOX " args)
#ifndef SOX_DEBUG_FLAG
#define SOX_DEBUG(args...)
#else
#define SOX_DEBUG(args...)		__SOX_DEBUG__(args)
#endif
#define SOX_DEBUG_S(args...)		SOX_DEBUG("[stream]: " args)
#define SOX_CRITICAL(args...)		__SOX_DEBUG__("CRITICAL: " args)


DECLARE_MEDIA_DRIVER_OPEN_METH(media_sox);
DECLARE_MEDIA_DRIVER_CLOSE_METH(media_sox);
DECLARE_MEDIA_DRIVER_READ_METH(media_sox);
DECLARE_MEDIA_DRIVER_REWIND_METH(media_sox);

DEFINE_MEDIA_DRIVER_CUSTOM(media_sox,
			   media_sox_open, media_sox_close,
			   NULL, NULL,
			   media_sox_read, NULL,
			   media_sox_rewind, NULL);


/* called from util.c::st_fail() */
void cleanup(void)
{
}


static ms_driver_data_t
media_sox_open(Lisp_Media_Stream *ms)
{
	media_substream *mss;
	mtype_audio_properties *mtap;
	char *name = NULL;
	/* libsndfile stuff */
	sxe_sox_t ft = NULL;
	sxe_sox_signalinfo_t *stinfo = NULL;

	/* initialise */
	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file = NULL;
		int file_len __attribute__((unused)) = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		TO_EXTERNAL_FORMAT(LISP_STRING, mkfp->filename,
				   ALLOCA, (file, file_len), Qnil);
		if( file != NULL ) {
#if defined HAVE_SOX_OPEN_READ_3ARGS
			    ft = sxe_sox_open_read(file, NULL, NULL);
#elif defined HAVE_SOX_OPEN_READ_4ARGS
			    ft = sxe_sox_open_read(file, NULL, NULL, NULL);
#else
# error You shouldnt be here.  Wake up before you try to compile me.
#endif
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

	if (!ft) {
		media_stream_set_meths(ms, NULL);
		media_stream_driver(ms) = MDRIVER_UNKNOWN;
		return NULL;
	}

	/* retrieve the signal information */
#if defined HAVE_STRUCT_SOX_FORMAT_T
	stinfo = &ft->signal;
#else
#  error "What's the matter with you?! How did you reach this?"
#endif

	/* create a substream */
	mss = make_media_substream_append(ms);
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = stinfo->channels;
	mtap->samplerate = stinfo->rate;

	/* try to find a read function */
#if defined HAVE_SOX_SIGNALINFO_T_PRECISION
	mtap->samplewidth = stinfo->precision;
	mtap->framesize = mtap->channels * (stinfo->precision / 8);
#else
	switch (stinfo->size) {
	case SXE_SIZE_8BIT:
		mtap->samplewidth = 8;
		mtap->framesize = mtap->channels;
		break;
	case SXE_SIZE_16BIT:
		mtap->samplewidth = 16;
		mtap->framesize = mtap->channels * 2;
		break;
	case SXE_SIZE_24BIT:
		mtap->samplewidth = 24;
		mtap->framesize = mtap->channels * 3;
		break;
	case SXE_SIZE_32BIT:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		break;
	case SXE_SIZE_64BIT:
		mtap->samplewidth = 64;
		mtap->framesize = mtap->channels * 8;
		break;
	default:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		break;
	}
#endif
	/* since SoX internally uses S32, we bang from S32 */
	mtap->msf = sxe_msf_S32;
#if 0
	mtap->samplewidth = 8 * sizeof(float);
	mtap->framesize = mtap->channels * sizeof(float);
#endif

	mtap->name = name;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_stream_set_meths(ms, media_sox);

	/* keep the sox handle */
	media_stream_data(ms) = ft;
	media_substream_data(mss) = ft;

	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;

	return ft;
}

static void
media_sox_close(ms_driver_data_t arg)
{
	sxe_sox_t ft = arg;
	sxe_sox_close(ft);

	SOX_DEBUG("closing SoX handle: 0x%lx\n",
		  (unsigned long int)ft);

	return;
}


static size_t
media_sox_read(media_substream *mss, void *outbuf, size_t length)
{
/* read at most `length' frames into `outbuf' */
	/* stream stuff */
	Lisp_Media_Stream *ms = mss->up;
	mtype_audio_properties *mtap;
	/* libsox stuff */
	sxe_sox_t ft;
	sxe_sox_ssize_t samples;
	sxe_sox_sample_t *bptr;
	uint16_t framesize __attribute__((unused));
	media_sample_format_t *fmt;

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
	fmt = mtap->msf;

	bptr = (sxe_sox_sample_t*)outbuf;
	samples = sxe_sox_read(ft, bptr, mtap->channels*length);

	SOX_DEBUG_S("SoX handle: 0x%lx read %zd samples\n",
                    (unsigned long int)ft, samples);

	if (samples < 0)
		return 0;

	/* always convert to internal format */
	MEDIA_SAMPLE_FORMAT_UPSAMPLE(fmt)(outbuf, outbuf, samples);

	/* return number of frames */
	return samples/mtap->channels;
}

static void
media_sox_rewind(media_substream *mss)
{
        /* rewind the stream to the first frame */
	Lisp_Media_Stream *ms = mss->up;
	/* libsox stuff */
	sxe_sox_t ft;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return;

	/* fetch the SNDFILE context and our audio props */
	if (!(ft = media_stream_data(ms)))
		return;

	SOX_DEBUG_S("rewind stream 0x%lx\n", (unsigned long int)ft);

        /* Unfortunately it seems sex_sox_seek is broken, so we are
           closing and reopening the stream,
	*/
#ifdef SXE_SOX_CAN_SEEK
        if( sxe_sox_seek(ft, 0, SOX_SEEK_SET) == 0 ) {
		return;
	}
	SOX_DEBUG_S("rewind stream 0x%lx failed, trying reopen\n",
		    (unsigned long int)ft);
#endif
	sxe_sox_close(ft);
	ft = NULL;
	{
		mkind_file_properties *mkfp = NULL;
		const char *file = NULL;
		int file_len __attribute__((unused)) = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		TO_EXTERNAL_FORMAT(LISP_STRING, mkfp->filename,
				   ALLOCA, (file, file_len), Qnil);
		if( file != NULL ) {
#if defined HAVE_SOX_OPEN_READ_3ARGS
			ft = sxe_sox_open_read(file, NULL, NULL);
#elif defined HAVE_SOX_OPEN_READ_4ARGS
			ft = sxe_sox_open_read(file, NULL, NULL, NULL);
#else
# error You shouldnt be here.  Wake up before you try to compile me.
#endif
		}
	}
	if (!ft) {
		media_stream_set_meths(ms, NULL);
		media_stream_driver(ms) = MDRIVER_UNKNOWN;
		SOX_DEBUG_S("could not reopen stream");
		return;
	}
	media_stream_data(ms) = ft;
	media_substream_data(mss) = ft;
}

#undef MYSELF
