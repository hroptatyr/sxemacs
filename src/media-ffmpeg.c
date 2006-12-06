/* media-ffmpeg.c - analyse audio files or streams via ffmpeg

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

#include "media-ffmpeg.h"

static int media_ffmpeg_bitrate(AVCodecContext*);
static AVFormatContext *media_ffmpeg_open_file(const char*);
AVFormatContext *media_ffmpeg_open_data(const char*, uint32_t);

static void media_ffmpeg_analyse_audio(media_substream*, AVFormatContext*, int);
static void media_ffmpeg_analyse_video(media_substream*, AVFormatContext*, int);

#define MYSELF MDRIVER_FFMPEG

Lisp_Object Qffmpeg;

#define __FFMPEG_DEBUG__(args...)	fprintf(stderr, "FFMPEG " args)
#ifndef FFMPEG_DEBUG_FLAG
#define FFMPEG_DEBUG(args...)
#else
#define FFMPEG_DEBUG(args...)		__FFMPEG_DEBUG__(args)
#endif
#define FFMPEG_DEBUG_AVF(args...)	FFMPEG_DEBUG("[avformat]: " args)
#define FFMPEG_DEBUG_AVC(args...)	FFMPEG_DEBUG("[avcodec]: " args)
#define FFMPEG_DEBUG_AVS(args...)	FFMPEG_DEBUG("[stream]: " args)
#define FFMPEG_CRITICAL(args...)	__FFMPEG_DEBUG__("CRITICAL: " args)


DECLARE_MEDIA_DRIVER_OPEN_METH(media_ffmpeg);
DECLARE_MEDIA_DRIVER_CLOSE_METH(media_ffmpeg);
DECLARE_MEDIA_DRIVER_PRINT_METH(media_ffmpeg);
DECLARE_MEDIA_DRIVER_READ_METH(media_ffmpeg);
DECLARE_MEDIA_DRIVER_REWIND_METH(media_ffmpeg);

DEFINE_MEDIA_DRIVER_CUSTOM(media_ffmpeg,
			   media_ffmpeg_open, media_ffmpeg_close,
			   media_ffmpeg_print, NULL,
			   media_ffmpeg_read, NULL,
			   media_ffmpeg_rewind, NULL);


static int
media_ffmpeg_bitrate(AVCodecContext *enc)
{
	int bitrate;

        /* for PCM codecs, compute bitrate directly */
        switch(enc->codec_id) {
        case CODEC_ID_PCM_S32LE:
        case CODEC_ID_PCM_S32BE:
        case CODEC_ID_PCM_U32LE:
        case CODEC_ID_PCM_U32BE:
            bitrate = enc->sample_rate * enc->channels * 32;
            break;
        case CODEC_ID_PCM_S24LE:
        case CODEC_ID_PCM_S24BE:
        case CODEC_ID_PCM_U24LE:
        case CODEC_ID_PCM_U24BE:
        case CODEC_ID_PCM_S24DAUD:
            bitrate = enc->sample_rate * enc->channels * 24;
            break;
        case CODEC_ID_PCM_S16LE:
        case CODEC_ID_PCM_S16BE:
        case CODEC_ID_PCM_U16LE:
        case CODEC_ID_PCM_U16BE:
            bitrate = enc->sample_rate * enc->channels * 16;
            break;
        case CODEC_ID_PCM_S8:
        case CODEC_ID_PCM_U8:
        case CODEC_ID_PCM_ALAW:
        case CODEC_ID_PCM_MULAW:
            bitrate = enc->sample_rate * enc->channels * 8;
            break;
        default:
            bitrate = enc->bit_rate;
            break;
        }
	return bitrate;
}

char *media_ffmpeg_streaminfo(Lisp_Media_Stream *ms)
{
	AVFormatContext *avfc = NULL;
	char *out;
	int chars_left = 4095;

	avfc = media_stream_data(ms);
	out = xmalloc(chars_left+1);
	out[0] = '\0';

	/* cannot use ffmpeg on corrupt streams */
	if (media_stream_driver(ms) != MYSELF || avfc == NULL)
		return out;

	if (avfc->author && *avfc->author) {
		strcat(out, " :author \"");
		strncat(out, avfc->author, chars_left);
		strcat(out, "\"");
		chars_left -= 560;
	}
	if (avfc->title && *avfc->title) {
		strcat(out, " :title: \"");
		strncat(out, avfc->title, chars_left);
		strcat(out, "\"");
		chars_left -= 560;
	}
	if (avfc->year) {
		char year[12];
		strcat(out, " :year ");
		snprintf(year, 12, "%d", avfc->year);
		strncat(out, year, chars_left);
		chars_left -= 24;
	}

	return out;
}

static void
media_ffmpeg_print(Lisp_Object ms, Lisp_Object pcfun, int ef)
{
	return;
}

static AVFormatContext*
media_ffmpeg_open_file(const char *file)
{
	AVFormatContext *avfc = av_alloc_format_context();

	/* open the file */
	if (av_open_input_file(&avfc, file, NULL, 0, NULL) < 0) {
		FFMPEG_DEBUG_AVF("opening file failed.\n");
		if (avfc)
			xfree(avfc);
		return NULL;
	}

	/* Retrieve stream information */
	if (av_find_stream_info(avfc) < 0) {
		FFMPEG_DEBUG_AVS("opening stream inside file failed.\n");
		av_close_input_file(avfc);
		if (avfc)
			xfree(avfc);
		return NULL;
	}

	return avfc;
}


static int
media_ffmpeg_vio_open(URLContext *h, const char *filename, int flags)
{
	return 0;
}

static int
media_ffmpeg_vio_read(URLContext *h, unsigned char *buf, int size)
{
	media_data *sd = (media_data*)h->priv_data;

	FFMPEG_DEBUG_AVS("reading %d bytes to 0x%x, respecting seek %ld\n",
			 size, (unsigned int)buf, sd->seek);

	if (sd->length <= sd->seek) {
		FFMPEG_DEBUG_AVS("eof\n");
		return -1;
	}

	memcpy(buf, sd->data+sd->seek, size);
	sd->seek += size;

	return size;
}

static int
media_ffmpeg_vio_write(URLContext *h, unsigned char *buf, int size)
{
	return -1;
}

static offset_t
media_ffmpeg_vio_seek(URLContext *h, offset_t pos, int whence)
{
	media_data *sd = (media_data*)h->priv_data;
    
	FFMPEG_DEBUG_AVS("seeking to %ld via %d\n", (long int)pos, whence);

	switch (whence) {
	case SEEK_SET:
		sd->seek = pos;
		break;
	case SEEK_CUR:
		sd->seek = sd->seek+pos;
		break;
	case SEEK_END:
		sd->seek = sd->length+pos;
		break;
	}
	return sd->seek;
}

static int
media_ffmpeg_vio_close(URLContext *h)
{
	if (h->priv_data)
		xfree(h->priv_data);
	h->priv_data = NULL;
	return 0;
}

/* this is a memory-i/o protocol in case we have to deal with string data */
static URLProtocol media_ffmpeg_protocol = {
	"SXEmff",
	media_ffmpeg_vio_open,
	media_ffmpeg_vio_read,
	media_ffmpeg_vio_write,
	media_ffmpeg_vio_seek,
	media_ffmpeg_vio_close,
};

/** Size of probe buffer, for guessing file type from file contents. */
#define PROBE_BUF_MIN 2048
#define PROBE_BUF_MAX 131072

AVFormatContext*
media_ffmpeg_open_data(const char *data, const uint32_t size)
{
	AVFormatContext *avfc = av_alloc_format_context();
	AVProbeData *pd = NULL;
	ByteIOContext *bioctx = NULL;
	AVInputFormat *fmt = NULL;
	char file[] = "SXEmff:SXEmacs.mp3\000";
	media_data *sd = NULL;

	/* register our virtual i/o */
	register_protocol(&media_ffmpeg_protocol);

	/* initialise our media_data */
	sd = xnew_and_zero(media_data);
	sd->length = size;
	sd->seek = 0;
	sd->data = (char*)data;

	/* register ffmpeg byteio */
	bioctx = xnew_and_zero(ByteIOContext);
	url_fopen(bioctx, file, URL_RDONLY);
	/* erm, register us at the byteio context */
	((URLContext*)(bioctx->opaque))->priv_data = sd;

	/* take a probe */
	pd = xnew_and_zero(AVProbeData);
 	pd->filename = file;
 	pd->buf = NULL;
 	pd->buf_size = 0;

	pd->buf = (void*)sd->data;
	pd->buf_size = PROBE_BUF_MIN;
	fmt = av_probe_input_format(pd, 1);

	/* if still no format found, error */
	if (!fmt) {
		xfree(pd);
		xfree(bioctx);
		xfree(sd);
		xfree(avfc);
		return NULL;
	}

	/* open the file */
	if (av_open_input_stream(&avfc, bioctx, file, fmt, NULL) < 0) {
		xfree(pd);
		xfree(bioctx);
		xfree(sd);
		xfree(avfc);
		return NULL;
	}

	/* Retrieve stream information */
	if (av_find_stream_info(avfc) < 0) {
		xfree(pd);
		xfree(bioctx);
		xfree(sd);
		xfree(avfc);
		return NULL;
	}

	return avfc;
}

static void
media_ffmpeg_close(ms_driver_data *data)
{
	AVFormatContext *avfc = (AVFormatContext*)data;
	FFMPEG_DEBUG_AVF("closing AVFormatContext: 0x%x\n",
			 (unsigned int)avfc);
	if (avfc && avfc->iformat)
		av_close_input_file(avfc);
}

static void
media_ffmpeg_analyse_audio(media_substream *mss, AVFormatContext *avfc, int st)
{
	mtype_audio_properties *mtap;
	const char *name = NULL;
	const char *codec_name = NULL;
	/* libavformat cruft */
	AVStream *avst;
	AVCodecContext *avcc;

	/* unpack the stream and codec context from the container, again */
	avst = avfc->streams[st];
	avcc = avst->codec;

	/* initialise */
	mtap = xnew_and_zero(mtype_audio_properties);

	/* copy the name */
	if (avfc && avfc->iformat)
		name = avfc->iformat->name;
	if (avcc && avcc->codec)
		codec_name = avcc->codec->name;

	mtap->name = name;
	mtap->codec_name = codec_name;
	mtap->channels = avcc->channels;
	mtap->samplerate = avcc->sample_rate;
	mtap->bitrate = media_ffmpeg_bitrate(avcc);

	/* samplewidth and framesize */
	switch (avcc->sample_fmt) {
	case SAMPLE_FMT_U8:
		mtap->samplewidth = 8;
		mtap->framesize = mtap->channels * 1;
		mtap->msf = sxe_msf_U8;
		break;
	case SAMPLE_FMT_S16:
		mtap->samplewidth = 16;
		mtap->framesize = mtap->channels * 2;
		mtap->msf = sxe_msf_S16;
		break;
	case SAMPLE_FMT_S24:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		mtap->msf = sxe_msf_S24;
		break;
	case SAMPLE_FMT_S32:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		mtap->msf = sxe_msf_S32;
		break;
	case SAMPLE_FMT_FLT:
		mtap->samplewidth = 8*sizeof(float);
		mtap->framesize = mtap->channels * sizeof(float);
		mtap->msf = sxe_msf_FLT;
		break;
	default:
		mtap->samplewidth = 0;
		break;
	}
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_substream_data(mss) = (void*)st;
}

static void
media_ffmpeg_analyse_video(media_substream *mss, AVFormatContext *avfc, int st)
{
	mtype_video_properties *mtvp;
	const char *name = NULL;
	const char *codec_name = NULL;
	/* libavformat cruft */
	AVStream *avst;
	AVCodecContext *avcc;

	/* unpack the stream and codec context from the container, again */
	avst = avfc->streams[st];
	avcc = avst->codec;

	/* initialise */
	mtvp = xnew_and_zero(mtype_video_properties);

	/* copy the name */
	if (avfc && avfc->iformat)
		name = avfc->iformat->name;
	if (avcc && avcc->codec)
		codec_name = avcc->codec->name;

	mtvp->name = name;
	mtvp->codec_name = codec_name;
	mtvp->bitrate = avcc->bit_rate;
	mtvp->width = avcc->width;
	mtvp->height = avcc->height;
	mtvp->aspect_num = avcc->sample_aspect_ratio.num;
	mtvp->aspect_den = avcc->sample_aspect_ratio.den;

	mtvp->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).vprops = mtvp;
}

/* main analysis function */
static ms_driver_data *
media_ffmpeg_open(Lisp_Media_Stream *ms)
{
	/* stream stuff */
	media_substream *mss;
	/* libavformat stuff */
	AVFormatContext *avfc = NULL;
	AVStream *avst = NULL;
	AVCodecContext *avcc = NULL;
	AVCodec *avc = NULL;
	int st = -1;
	
	/* initialise */
	av_register_all();

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;
		int file_len = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		TO_EXTERNAL_FORMAT(LISP_STRING, mkfp->filename,
				   ALLOCA, (file, file_len), Qnil);
		avfc = media_ffmpeg_open_file(file);
		if (!avfc) {
			media_stream_set_meths(ms, NULL);
			media_stream_driver(ms) = MDRIVER_UNKNOWN;
			return NULL;
		}

		/* store the filesize */
		mkfp->filesize = avfc->file_size;
		break;
	}
	case MKIND_STRING: {
		mkind_string_properties *mksp = NULL;
		const char *data;
		uint32_t size;

		/* open the file */
		mksp = media_stream_kind_properties(ms).sprops;
		data = mksp->stream_data;
		size = mksp->size;
		avfc = media_ffmpeg_open_data(data, size);

		if (!avfc) {
			media_stream_set_meths(ms, NULL);
			media_stream_driver(ms) = MDRIVER_UNKNOWN;
			return NULL;
		}
		break;
	}
	default:
		break;
	}

	/* check if there is at least one usable stream */
	for (st = 0; st < avfc->nb_streams; st++) {
		avst = avfc->streams[st];
		avcc = avst->codec;
		if (avcc &&
		    avcc->codec_id != CODEC_ID_NONE &&
		    avcc->codec_type != CODEC_TYPE_DATA &&
		    (avc = avcodec_find_decoder(avcc->codec_id)) &&
		    (avc && (avcodec_open(avcc, avc) >= 0))) {

			/* create a substream */
			mss = make_media_substream_append(ms);

			switch (avcc->codec_type) {
			case CODEC_TYPE_VIDEO:
				/* assign substream props */
				media_substream_type(mss) = MTYPE_VIDEO;
				media_ffmpeg_analyse_video(mss, avfc, st);
				break;
			case CODEC_TYPE_AUDIO:
				/* assign substream props */
				media_substream_type(mss) = MTYPE_AUDIO;
				media_ffmpeg_analyse_audio(mss, avfc, st);
				/* set some stream handlers */
				media_stream_set_meths(ms, media_ffmpeg);
				break;
			case CODEC_TYPE_DATA:
				media_substream_type(mss) = MTYPE_IMAGE;
				break;
			default:
				media_substream_type(mss) = MTYPE_UNKNOWN;
				break;
			}
		}
	}

	/* keep the format context */
	media_stream_data(ms) = avfc;

	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;

	return avfc;
}


static size_t
media_ffmpeg_read(media_substream *mss, void *outbuf, size_t length)
{
/* read at most `length' frames into `outbuf' */
/* convert them to internal format */
	/* stream stuff */
	Lisp_Media_Stream *ms = mss->up;
	mtype_audio_properties *mtap;
	media_sample_format_t *fmt;
	/* libavformat */
	AVFormatContext *avfc;
	AVStream *avst;
	AVCodecContext *avcc;
	AVCodec *avc;
	AVPacket *pkt;
	/* buffering */
	int size;
	uint16_t framesize;
	/* result */
	uint32_t bufseek = 0;
	int declen, dec;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;

	/* fetch the format context */
	avfc = media_stream_data(ms);

	if (!avfc)
		return 0;

	avst = avfc->streams[(int)mss->substream_data];
	avcc = avst->codec;
	avc = avcc->codec;

	/* unpack the substream */
	if ((mtap = media_substream_type_properties(mss).aprops) == NULL) {
		FFMPEG_DEBUG_AVS("stream information missing. Uh Oh.\n");
		return 0;
	}

	/* fetch audio info */
	framesize = mtap->framesize;
	fmt = mtap->msf;

	/* initialise packets and buffer */
#if 0
	pkt = xnew_and_zero(AVPacket);
#else
	pkt = alloca(sizeof(AVPacket));
#endif
	av_init_packet(pkt);
	/* do not cache locally any longer */
	/* buffer = xmalloc(MEDIA_MAX_AUDIO_FRAME_SIZE); */

	/* read a frame and decode it */
	while (bufseek <= length*framesize && av_read_frame(avfc, pkt) >= 0) {

		dec = pkt->size;
		/* decode the demuxed packet */
		declen = avcodec_decode_audio(avcc, outbuf+bufseek, &size,
					      pkt->data, pkt->size);

		if (dec == declen && dec > 0 && size > 0) {
			FFMPEG_DEBUG_AVF("pts:%lld dts:%lld psz:%d s:%d d:%d\n",
					 pkt->pts, pkt->dts, pkt->size,
					 size, declen);

			/* memcpy(outbuf+bufseek, (char*)buffer, size); */
			bufseek += size;
		}
	}

	/* convert the pig */
	size = bufseek/framesize;
	MEDIA_SAMPLE_FORMAT_UPSAMPLE(fmt)(outbuf, outbuf, size*mtap->channels);

	/* shutdown */
#if 0
	av_free_packet(pkt);
#endif

	return size;
}


static void
media_ffmpeg_rewind(media_substream *mss)
{
/* rewind the stream to the first frame */
	/* libavformat */
	AVFormatContext *avfc;
	Lisp_Media_Stream *ms = mss->up;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;

	/* fetch the format context */
	if (!(avfc = media_stream_data(ms)))
		return;

	/* ... and reset the stream */
	av_seek_frame(avfc, -1, 0, AVSEEK_FLAG_BACKWARD);
}


Lisp_Object media_ffmpeg_available_formats(void)
{
	Lisp_Object formats;
	AVInputFormat *avif;

	formats = Qnil;

	av_register_all();
	avif = first_iformat;

	while (avif) {
		if (avif->name) {
			const Lisp_Object fmtname =
				Fintern(build_string(avif->name), Qnil);
			formats = Fcons(fmtname, formats);
		}
		avif = avif->next;
	}

	return formats;
}

#undef MYSELF
