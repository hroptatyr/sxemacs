/* media-ffmpeg.c - analyse audio files or streams via ffmpeg

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
#if defined HAVE_STDINT_H
# include <stdint.h>
#endif	/* HAVE_STDINT_H */

#include "buffer.h"
#include "sysdep.h"
#include "sysfile.h"

#include "media-ffmpeg.h"

static int media_ffmpeg_bitrate(AVCodecContext*);
static AVFormatContext *media_ffmpeg_open_file(const char*);
AVFormatContext *media_ffmpeg_open_data(char*, size_t);

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

DECLARE_MEDIA_DRIVER_OPEN_METH(new_media_ffmpeg);
DECLARE_MEDIA_DRIVER_READ_METH(new_media_ffmpeg);
DECLARE_MEDIA_DRIVER_REWIND_METH(new_media_ffmpeg);

DEFINE_MEDIA_DRIVER_CUSTOM(new_media_ffmpeg,
			   new_media_ffmpeg_open, NULL,
			   NULL, NULL,
			   new_media_ffmpeg_read, NULL,
			   new_media_ffmpeg_rewind, NULL);


static int
media_ffmpeg_bitrate(AVCodecContext *enc)
{
	int bitrate;

        /* for PCM codecs, compute bitrate directly */
        switch ((unsigned int)enc->codec_id) {
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
	out = xmalloc_atomic(chars_left+1);
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
#if defined HAVE_AVFORMAT_ALLOC_CONTEXT
	AVFormatContext *avfc = avformat_alloc_context();
#elif defined HAVE_AV_ALLOC_FORMAT_CONTEXT
	/* deprecated already, but `people' like Horst still use this */
	AVFormatContext *avfc = av_alloc_format_context();
#else
# error "Your ffmpeg library is too old.  Adopt a new one."
#endif	/* HAVE_AVFORMAT_ALLOC_CONTEXT */

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

	if ((long int)sd->length <= sd->seek) {
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

static int64_t
media_ffmpeg_vio_seek(URLContext *h, int64_t pos, int whence)
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
	default:
		/* be prolific */
		abort();
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
media_ffmpeg_open_data(char *data, size_t size)
{
#if defined HAVE_AVFORMAT_ALLOC_CONTEXT
	AVFormatContext *avfc = avformat_alloc_context();
#elif defined HAVE_AV_ALLOC_FORMAT_CONTEXT
	/* deprecated already, but `people' like Horst still use this */
	AVFormatContext *avfc = av_alloc_format_context();
#else
# error "Your ffmpeg library is too old.  Adopt a new one."
#endif	/* HAVE_AVFORMAT_ALLOC_CONTEXT */
	AVProbeData *pd = NULL;
	ByteIOContext *bioctx = NULL;
	AVInputFormat *fmt = NULL;
	char file[] = "SXEmff:SXEmacs.mp3\000";
	media_data *sd = NULL;

	/* register our virtual i/o */
#if defined HAVE_AV_REGISTER_PROTOCOL
	av_register_protocol(&media_ffmpeg_protocol);
#elif defined HAVE_REGISTER_PROTOCOL
	register_protocol(&media_ffmpeg_protocol);
#else
# error "Get a recent ffmpeg or get a life."
#endif

	/* initialise our media_data */
	sd = xnew_and_zero(media_data);
	sd->length = size;
	sd->seek = 0;
	sd->data = data;

	/* register ffmpeg byteio */
	bioctx = xnew_and_zero(ByteIOContext);
#if defined FFMPEG_URL_FOPEN_BIOCTX_STAR_STAR
	url_fopen(&bioctx, file, URL_RDONLY);
#elif defined FFMPEG_URL_FOPEN_BIOCTX_STAR
	url_fopen(bioctx, file, URL_RDONLY);
#endif
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
media_ffmpeg_close(ms_driver_data_t data)
{
	AVFormatContext *avfc = (AVFormatContext*)data;
	FFMPEG_DEBUG_AVF("closing AVFormatContext: 0x%lx\n",
			 (long unsigned int)avfc);
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
#if defined SAMPLE_FMT_S24
	case SAMPLE_FMT_S24:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		mtap->msf = sxe_msf_S24;
		break;
#endif	/* SAMPLE_FMT_S24 */
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
	case SAMPLE_FMT_NONE:
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
	media_substream_data(mss) = (void*)st;
}

/* main analysis function */
static ms_driver_data_t
media_ffmpeg_open(Lisp_Media_Stream *ms)
{
	/* stream stuff */
	media_substream *mss;
	/* libavformat stuff */
	AVFormatContext *avfc = NULL;
	AVStream *avst = NULL;
	AVCodecContext *avcc = NULL;
	AVCodec *avc = NULL;
	
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
		char *data;
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
	case MKIND_UNKNOWN:
	case MKIND_FIFO:
	case MKIND_STREAM:
	case NUMBER_OF_MEDIA_KINDS:
	default:
		break;
	}

	/* check if there is at least one usable stream */
	for (size_t st = 0; st < avfc->nb_streams; st++) {
		avst = avfc->streams[st];
		avcc = avst->codec;
		if (avcc &&
		    avcc->codec_id != CODEC_ID_NONE &&
		    avcc->codec_type != CODEC_TYPE_DATA &&
		    (avc = avcodec_find_decoder(avcc->codec_id)) &&
		    (avc && (avcodec_open(avcc, avc) >= 0))) {

			/* create a substream */
			mss = make_media_substream_append(ms);

			switch ((unsigned int)avcc->codec_type) {
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


static inline void
handle_packet(AVFormatContext *avfc, AVPacket *pkt)
{
#if 0
	AVFrame picture;
	AVStream *st;
	int ret, got_picture;

	st = avfc->streams[pkt->stream_index];

	/* XXX: allocate picture correctly */
	avcodec_get_frame_defaults(&picture);

	ret = avcodec_decode_video(
		st->codec, &picture, &got_picture, pkt->data, pkt->size);

	if (!got_picture) {
		/* no picture yet */
		goto discard_packet;
	}
#endif

	FFMPEG_DEBUG_AVF("got video frame\n");

#if 0				/* not yet */
discard_packet:
#endif
	av_free_packet(pkt);
	return;
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
	AVPacket pkt;
	/* buffering */
	/* the size we present here, is _not_ the size we want, however
	 * ffmpeg is quite pedantic about the buffer size,
	 * we just pass the least possible value here to please him */
	int size = AVCODEC_MAX_AUDIO_FRAME_SIZE;
	uint16_t framesize;
	/* result */
	long int bufseek = 0, si = -1;
	int declen, dec, rf_status = 0;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;

	/* fetch the format context */
	avfc = media_stream_data(ms);

	if (!avfc)
		return 0;

	si = (long int)mss->substream_data;
	avst = avfc->streams[si];
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

	/* initialise the packet */
	pkt.pts = pkt.dts = pkt.size = 0;
	FFMPEG_DEBUG_AVF("initialised packet: "
			 "pts:%lld dts:%lld psz:%d\n",
			 (long long int)pkt.pts,
			 (long long int)pkt.dts,
			 pkt.size);

	/* read a frame and decode it */
	while ((size_t)bufseek <= length*framesize &&
	       (rf_status = av_read_frame(avfc, &pkt)) >= 0) {
		if (pkt.stream_index != si) {
			FFMPEG_DEBUG_AVF("SKIP reason: "
					 "sought after stream %ld, got %ld\n",
					 (long int)si,
					 (long int)pkt.stream_index);
			handle_packet(avfc, &pkt);
			continue;
		}

		FFMPEG_DEBUG_AVF("read frame: "
				 "pts:%lld dts:%lld psz:%d\n",
				 (long long int)pkt.pts,
				 (long long int)pkt.dts,
				 pkt.size);

		dec = pkt.size;
		/* decode the demuxed packet */
#ifdef HAVE_AVCODEC_DECODE_AUDIO2
/* prefer decode_audio2() if available */
		size = AVCODEC_MAX_AUDIO_FRAME_SIZE;
		declen = avcodec_decode_audio2(
			avcc, (void*)((char*)outbuf+bufseek),
			&size, pkt.data, pkt.size);
#elif defined HAVE_AVCODEC_DECODE_AUDIO
		declen = avcodec_decode_audio(
			avcc, (void*)((char*)outbuf+bufseek),
			&size, pkt.data, pkt.size);
#else
		abort();
#endif

		if (dec > 0 && size > 0) {
			FFMPEG_DEBUG_AVF("pts:%lld dts:%lld psz:%d s:%d d:%d\n",
					 (long long int)pkt.pts,
					 (long long int)pkt.dts,
					 pkt.size, size, declen);

			/* memcpy(outbuf+bufseek, (char*)buffer, size); */
			bufseek += size;
		}

		FFMPEG_DEBUG_AVF("packet state: "
				 "pts:%lld dts:%lld psz:%d\n",
				 (long long int)pkt.pts,
				 (long long int)pkt.dts,
				 (int)pkt.size);
		av_free_packet(&pkt);
	}
	av_free_packet(&pkt);

	FFMPEG_DEBUG_AVF("finished reading, bufseek=%ld, rf_status=%ld\n",
			 (long int)bufseek, (long int)rf_status);

	/* convert the pig */
	size = bufseek/framesize;
	MEDIA_SAMPLE_FORMAT_UPSAMPLE(fmt)(outbuf, outbuf, size*mtap->channels);

	/* shutdown */
	return size;
}

static void
media_ffmpeg_rewind(media_substream *mss)
{
/* rewind the stream to the first frame */
	/* libavformat */
	AVFormatContext *avfc;
	AVStream *avst;
	Lisp_Media_Stream *ms = mss->up;
	int64_t start_time;
	int res = 0;
	long int si;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;

	FFMPEG_DEBUG_AVF("rewind substream 0x%lx\n",
			 (long unsigned int)mss);

	/* fetch the format context */
	if (!(avfc = media_stream_data(ms)))
		return;

	si = (long int)mss->substream_data;
	avst = avfc->streams[si];
	if ((start_time = avst->start_time) < 0) {
		start_time = 0;
	}

	FFMPEG_DEBUG_AVF("rewind (idx:%ld) to %lld\n",
			 si, (long long int)start_time);

	/* ... and reset the stream */
	res = av_seek_frame(avfc, si, AV_NOPTS_VALUE, AVSEEK_FLAG_BACKWARD);

	if (res >= 0) {
		FFMPEG_DEBUG_AVF("rewind succeeded\n");
		return;
	} else {
		FFMPEG_DEBUG_AVF("rewind exitted with %d\n", res);
		return;
	}
}


/* NOTE:
 * the size must be big enough to compensate the hardware audio buffersize size
 */
#define SAMPLE_ARRAY_SIZE (2*65536)
#define VIDEO_PICTURE_QUEUE_SIZE 1
#define SUBPICTURE_QUEUE_SIZE 4
#define AUDIO_DIFF_AVG_NB   20
#define SDL_AUDIO_BUFFER_SIZE 1024

#define MAX_VIDEOQ_SIZE (5 * 256 * 1024)
#define MAX_AUDIOQ_SIZE (5 * 16 * 1024)
#define MAX_SUBTITLEQ_SIZE (5 * 16 * 1024)

#define FF_ALLOC_EVENT   0
#define FF_REFRESH_EVENT 1
#define FF_QUIT_EVENT    2

typedef struct PacketQueue {
	AVPacketList *first_pkt, *last_pkt;
	int nb_packets;
	int size;
	int abort_request;
	struct sxe_semaphore_s sem;
} PacketQueue;

typedef struct VideoPicture {
	/* presentation time stamp for this picture */
	double pts;
	void *bmp;
	int width, height; /* source height & width */
	int allocated;
} VideoPicture;

typedef struct SubPicture {
	double pts; /* presentation time stamp for this picture */
	AVSubtitle sub;
} SubPicture;

typedef struct VideoState {
	void *parse_tid;
	void *video_tid;
	AVInputFormat *iformat;
	int no_background;
	int abort_request;
	int paused;
	int last_paused;
	int seek_req;
	int seek_flags;
	int64_t seek_pos;
	AVFormatContext *ic;
	int dtg_active_format;

	int audio_stream;

	int av_sync_type;
	double external_clock; /* external clock base */
	int64_t external_clock_time;

	double audio_clock;
	double audio_diff_cum; /* used for AV difference average computation */
	double audio_diff_avg_coef;
	double audio_diff_threshold;
	int audio_diff_avg_count;
	AVStream *audio_st;
	PacketQueue audioq;
	int audio_hw_buf_size;
	/* samples output by the codec. we reserve more space for avsync
	   compensation */
	uint8_t audio_buf[(AVCODEC_MAX_AUDIO_FRAME_SIZE * 3) / 2]
	/* fixme, this is very gcc centric, what's aligned(x) in icc? */
	__attribute__((aligned(16)));
	unsigned int audio_buf_size; /* in bytes */
	int audio_buf_index; /* in bytes */
	AVPacket audio_pkt;
	uint8_t *audio_pkt_data;
	int audio_pkt_size;

	int show_audio; /* if true, display audio samples */
	int16_t sample_array[SAMPLE_ARRAY_SIZE];
	int sample_array_index;
	int last_i_start;

	void *subtitle_tid;
	int subtitle_stream;
	int subtitle_stream_changed;
	AVStream *subtitle_st;
	PacketQueue subtitleq;
	SubPicture subpq[SUBPICTURE_QUEUE_SIZE];
	int subpq_size, subpq_rindex, subpq_windex;
	struct sxe_semaphore_s subpq_sem;

	double frame_timer;
	double frame_last_pts;
	double frame_last_delay;
	/* pts of last decoded frame / predicted pts of next decoded frame */
	double video_clock;
	int video_stream;
	AVStream *video_st;
	PacketQueue videoq;
	/* current displayed pts (different from video_clock
	 * if frame fifos are used */
	double video_current_pts;
	/* time (av_gettime) at which we updated video_current_pts - used
	 * to have running video pts */
	int64_t video_current_pts_time;
	VideoPicture pictq[VIDEO_PICTURE_QUEUE_SIZE];
	int pictq_size, pictq_rindex, pictq_windex;
	struct sxe_semaphore_s pictq_sem;

	/* QETimer *video_timer; */
	char *filename;
	size_t filelen;

	int width, height, xleft, ytop;
} VideoState;

/* since we have only one decoding thread, we can use a global
   variable instead of a thread local variable */
static VideoState *global_video_state;
AVPacket flush_pkt;

/* packet queue handling */
static void
packet_queue_init(PacketQueue *q)
{
	memset(q, 0, sizeof(PacketQueue));
	SXE_SEMAPH_INIT(&q->sem);
}

static void
packet_queue_flush(PacketQueue *q)
{
	AVPacketList *pkt, *pkt1;

	SXE_SEMAPH_LOCK(&q->sem);
	for (pkt = q->first_pkt; pkt != NULL; pkt = pkt1) {
		pkt1 = pkt->next;
		av_free_packet(&pkt->pkt);
		av_freep(&pkt);
	}
	q->last_pkt = NULL;
	q->first_pkt = NULL;
	q->nb_packets = 0;
	q->size = 0;
	SXE_SEMAPH_UNLOCK(&q->sem);
}

static void
packet_queue_end(PacketQueue *q)
{
	packet_queue_flush(q);
	SXE_SEMAPH_FINI(&q->sem);
}

static int
packet_queue_put(PacketQueue *q, AVPacket *pkt)
{
	AVPacketList *pkt1;

	/* duplicate the packet */
	if (pkt!=&flush_pkt && av_dup_packet(pkt) < 0)
		return -1;

	pkt1 = av_malloc(sizeof(AVPacketList));
	if (!pkt1)
		return -1;
	pkt1->pkt = *pkt;
	pkt1->next = NULL;


	SXE_SEMAPH_LOCK(&q->sem);

	if (!q->last_pkt)

		q->first_pkt = pkt1;
	else
		q->last_pkt->next = pkt1;
	q->last_pkt = pkt1;
	q->nb_packets++;
	q->size += pkt1->pkt.size;
	/* XXX: should duplicate packet data in DV case */
	SXE_SEMAPH_SIGNAL(&q->sem);
	SXE_SEMAPH_UNLOCK(&q->sem);
	return 0;
}

static void
packet_queue_abort(PacketQueue *q)
{
	SXE_SEMAPH_LOCK(&q->sem);

	q->abort_request = 1;

	SXE_SEMAPH_SIGNAL(&q->sem);
	SXE_SEMAPH_UNLOCK(&q->sem);
}

/* return < 0 if aborted, 0 if no packet and > 0 if packet.  */
static int
packet_queue_get(PacketQueue *q, AVPacket *pkt, int block)
	__attribute__((unused));

static int
packet_queue_get(PacketQueue *q, AVPacket *pkt, int block)
{
	AVPacketList *pkt1;
	int ret;

	SXE_SEMAPH_LOCK(&q->sem);

	for(;;) {
		if (q->abort_request) {
			ret = -1;
			break;
		}

		pkt1 = q->first_pkt;
		if (pkt1) {
			q->first_pkt = pkt1->next;
			if (!q->first_pkt)
				q->last_pkt = NULL;
			q->nb_packets--;
			q->size -= pkt1->pkt.size;
			*pkt = pkt1->pkt;
			av_free(pkt1);
			ret = 1;
			break;
		} else if (!block) {
			ret = 0;
			break;
		} else {
			SXE_SEMAPH_WAIT(&q->sem);
		}
	}
	SXE_SEMAPH_UNLOCK(&q->sem);
	return ret;
}

static uint64_t global_video_pkt_pts = AV_NOPTS_VALUE;

static int
my_get_buffer(struct AVCodecContext *c, AVFrame *pic)
{
	int ret= avcodec_default_get_buffer(c, pic);
	uint64_t *pts= av_malloc(sizeof(uint64_t));
	*pts= global_video_pkt_pts;
	pic->opaque= pts;
	return ret;
}

static void
my_release_buffer(struct AVCodecContext *c, AVFrame *pic)
{
	if(pic) av_freep(&pic->opaque);
	avcodec_default_release_buffer(c, pic);
}


static int
stream_component_open(VideoState *is, int stream_index, Lisp_Media_Stream *ms)
{
	/* stream stuff */
	media_substream *mss = NULL;
	AVFormatContext *ic = is->ic;
	AVCodecContext *enc;
	AVCodec *codec;

	if (stream_index < 0 || (size_t)stream_index >= ic->nb_streams) {
		return -1;
	}
	enc = ic->streams[stream_index]->codec;

	/* prepare audio output */
	if (enc->codec_type == CODEC_TYPE_AUDIO) {
#if 0
		wanted_spec.freq = enc->sample_rate;
		wanted_spec.format = AUDIO_S16SYS;
#endif
		/* hack for AC3. XXX: suppress that */
		if (enc->channels > 2)
			enc->channels = 2;
#if 0
		wanted_spec.channels = enc->channels;
		wanted_spec.silence = 0;
		wanted_spec.samples = SDL_AUDIO_BUFFER_SIZE;
		wanted_spec.callback = sdl_audio_callback;
		wanted_spec.userdata = is;
#endif
		is->audio_hw_buf_size = 0 /* spec.size */;
	}

	codec = avcodec_find_decoder(enc->codec_id);
	enc->debug_mv = 0 /* debug_mv */;
	enc->debug = 0 /* debug */;
	enc->workaround_bugs = 0 /* workaround_bugs */;
	enc->lowres = 0 /* lowres */;
	if (0 /* lowres */)
		enc->flags |= CODEC_FLAG_EMU_EDGE;
	enc->idct_algo = FF_IDCT_AUTO; /* idct; */
	if (0 /* fast */)
		enc->flags2 |= CODEC_FLAG2_FAST;
	enc->skip_frame = AVDISCARD_DEFAULT; /* skip_frame; */
	enc->skip_idct = AVDISCARD_DEFAULT; /* skip_idct; */
	enc->skip_loop_filter = AVDISCARD_DEFAULT; /* skip_loop_filter; */
#if 0
	enc->error_resilience = FF_ER_CAREFUL; /* error_resilience; */
#endif
	enc->error_concealment = 3; /* error_concealment; */
	if (!codec ||
	    avcodec_open(enc, codec) < 0)
		return -1;
	if (1 /* thread_count */ > 1)
		avcodec_thread_init(enc, 1 /*thread_count*/);
	enc->thread_count= 1 /* thread_count */;

	/* create a substream */
	mss = make_media_substream_append(ms);

	switch ((unsigned int)enc->codec_type) {
	case CODEC_TYPE_AUDIO:
		is->audio_stream = stream_index;
		is->audio_st = ic->streams[stream_index];
		is->audio_buf_size = 0;
		is->audio_buf_index = 0;

		/* init averaging filter */
		is->audio_diff_avg_coef = exp(log(0.01) / AUDIO_DIFF_AVG_NB);
		is->audio_diff_avg_count = 0;
		/* since we do not have a precise anough audio fifo fullness,
		   we correct audio sync only if larger than this threshold */
		is->audio_diff_threshold =
			2.0 * SDL_AUDIO_BUFFER_SIZE / enc->sample_rate;

		memset(&is->audio_pkt, 0, sizeof(is->audio_pkt));
		packet_queue_init(&is->audioq);

		media_substream_type(mss) = MTYPE_AUDIO;
		media_ffmpeg_analyse_audio(mss, is->ic, stream_index);
		break;
	case CODEC_TYPE_VIDEO:
		is->video_stream = stream_index;
		is->video_st = ic->streams[stream_index];

		is->frame_last_delay = 40e-3;
		{
			int64_t tmp = av_gettime();
			is->frame_timer = (double)tmp / 1000000.0f;
		}
		is->video_current_pts_time = av_gettime();

		packet_queue_init(&is->videoq);
		is->video_tid = 0 /* SDL_CreateThread(video_thread, is) */;

		enc->    get_buffer=     my_get_buffer;
		enc->release_buffer= my_release_buffer;

		media_substream_type(mss) = MTYPE_VIDEO;
		media_ffmpeg_analyse_video(mss, is->ic, stream_index);
		break;
	case CODEC_TYPE_SUBTITLE:
		is->subtitle_stream = stream_index;
		is->subtitle_st = ic->streams[stream_index];
		packet_queue_init(&is->subtitleq);

		is->subtitle_tid = 0 /*SDL_CreateThread(subtitle_thread, is)*/;
		break;
	default:
		break;
	}
	return 0;
}

static void
stream_component_close(VideoState *is, int stream_index)
{
	AVFormatContext *ic = is->ic;
	AVCodecContext *enc;

	if (stream_index < 0 || (size_t)stream_index >= ic->nb_streams) {
		return;
	}
	enc = ic->streams[stream_index]->codec;

	switch ((unsigned int)enc->codec_type) {
	case CODEC_TYPE_AUDIO:
		packet_queue_abort(&is->audioq);
#if 0
		SDL_CloseAudio();
#endif
		packet_queue_end(&is->audioq);
		break;
	case CODEC_TYPE_VIDEO:
		packet_queue_abort(&is->videoq);

		/* note: we also signal this mutex to make sure we deblock the
		   video thread in all cases */
		SXE_SEMAPH_LOCK(&is->pictq_sem);
		SXE_SEMAPH_SIGNAL(&is->pictq_sem);
		SXE_SEMAPH_UNLOCK(&is->pictq_sem);
#if 0
		SDL_WaitThread(is->video_tid, NULL);
#endif
		packet_queue_end(&is->videoq);
		break;
	case CODEC_TYPE_SUBTITLE:
		packet_queue_abort(&is->subtitleq);

		/* note: we also signal this mutex to make sure we deblock the
		   video thread in all cases */
		SXE_SEMAPH_LOCK(&is->subpq_sem);
		is->subtitle_stream_changed = 1;

		SXE_SEMAPH_SIGNAL(&is->subpq_sem);
		SXE_SEMAPH_UNLOCK(&is->subpq_sem);
#if 0
		SDL_WaitThread(is->subtitle_tid, NULL);
#endif
		packet_queue_end(&is->subtitleq);
		break;
	default:
		break;
	}

	avcodec_close(enc);
	switch ((unsigned int)enc->codec_type) {
	case CODEC_TYPE_AUDIO:
		is->audio_st = NULL;
		is->audio_stream = -1;
		break;
	case CODEC_TYPE_VIDEO:
		is->video_st = NULL;
		is->video_stream = -1;
		break;
	case CODEC_TYPE_SUBTITLE:
		is->subtitle_st = NULL;
		is->subtitle_stream = -1;
		break;
	default:
		break;
	}
}

static void
dump_stream_info(const AVFormatContext *s)
{
	if (s->track != 0)
		fprintf(stderr, "Track: %d\n", s->track);
	if (s->title[0] != '\0')
		fprintf(stderr, "Title: %s\n", s->title);
	if (s->author[0] != '\0')
		fprintf(stderr, "Author: %s\n", s->author);
	if (s->copyright[0] != '\0')
		fprintf(stderr, "Copyright: %s\n", s->copyright);
	if (s->comment[0] != '\0')
		fprintf(stderr, "Comment: %s\n", s->comment);
	if (s->album[0] != '\0')
		fprintf(stderr, "Album: %s\n", s->album);
	if (s->year != 0)
		fprintf(stderr, "Year: %d\n", s->year);
	if (s->genre[0] != '\0')
		fprintf(stderr, "Genre: %s\n", s->genre);
}

enum {
	AV_SYNC_AUDIO_MASTER, /* default choice */
	AV_SYNC_VIDEO_MASTER,
	AV_SYNC_EXTERNAL_CLOCK, /* synchronize to an external clock */
};

static VideoState *
stream_open(char *filename, size_t filelen)
{
	VideoState *is = xnew(VideoState);
	AVFormatParameters params, *ap = &params;
	int err = 0;

	is->filename = filename;
	is->filelen = filelen;
	is->iformat = av_find_input_format("fmt");
	is->ytop = 0;
	is->xleft = 0;

	/* initialise some semaphores */
	SXE_SEMAPH_INIT(&is->pictq_sem);
	SXE_SEMAPH_INIT(&is->subpq_sem);

	is->av_sync_type = AV_SYNC_AUDIO_MASTER;
	is->parse_tid = 0; /* SDL_CreateThread(decode_thread, is); */

	memset(ap, 0, sizeof(*ap));
	/* we force a pause when starting an RTSP stream */
	ap->initial_pause = 1; 

	ap->width = 0; /* frame_width; */
	ap->height= 0; /* frame_height; */
	ap->time_base= (AVRational){1, 25};
	ap->pix_fmt = PIX_FMT_NONE; /* frame_pix_fmt; */

	err = av_open_input_file(&is->ic, is->filename, is->iformat, 0, ap);
	if (UNLIKELY(err < 0)) {
		FFMPEG_DEBUG_AVF("Could not open \"%s\" (errno %d)\n",
				 is->filename, err);
		goto fail;
	}

	return is;

fail:
	xfree(is);
	return NULL;
}

/* main analysis function */
static ms_driver_data_t
new_media_ffmpeg_open(Lisp_Media_Stream *ms)
{
	/* the final result */
	VideoState *vs = NULL;
	int err = 0, use_play = 0;
	int wanted_audio_stream = 0;
	int wanted_video_stream = 0;
	int video_index = -1, audio_index = -1;
	mkind_file_properties *mkfp = NULL;
	char *file;
	int file_len = 0;
	
	/* initialise */
	av_register_all();

	if (media_stream_kind(ms) != MKIND_FILE) {
		return NULL;
	}

	/* open the file */
	mkfp = media_stream_kind_properties(ms).fprops;
	TO_EXTERNAL_FORMAT(LISP_STRING, mkfp->filename,
			   MALLOC, (file, file_len), Qnil);
	if (UNLIKELY((vs = stream_open(file, file_len)) == NULL)) {
		media_stream_set_meths(ms, NULL);
		media_stream_driver(ms) = MDRIVER_UNKNOWN;
		return NULL;
	}

#ifdef CONFIG_RTSP_DEMUXER
	use_play = (ic->iformat == &rtsp_demuxer);
#else
	use_play = 0;
#endif

	if (1 /* genpts */) {
		vs->ic->flags |= AVFMT_FLAG_GENPTS;
	}

	if (!use_play) {
		err = av_find_stream_info(vs->ic);
		if (err < 0) {
			FFMPEG_DEBUG_AVF("\"%s\": "
					 "could not find codec parameters\n",
					 vs->filename);
			goto fail;
		}
		/* FIXME hack,
		 * ffplay maybe should not use url_feof() to test for the end */
#if defined FFMPEG_URL_FOPEN_BIOCTX_STAR_STAR
		vs->ic->pb->eof_reached = 0;
#elif defined FFMPEG_URL_FOPEN_BIOCTX_STAR
		vs->ic->pb.eof_reached = 0;
#endif
	}

	/* now we can begin to play (RTSP stream only) */
	av_read_play(vs->ic);

	if (use_play) {
		err = av_find_stream_info(vs->ic);
		if (err < 0) {
			FFMPEG_DEBUG_AVF("\"%s\": "
					 "could not find codec parameters\n",
					 vs->filename);
			goto fail;
		}
	}

	for (size_t i = 0; i < vs->ic->nb_streams; i++) {
		AVCodecContext *enc = vs->ic->streams[i]->codec;
		switch ((unsigned int)enc->codec_type) {
		case CODEC_TYPE_AUDIO:
			if ((audio_index < 0 || wanted_audio_stream-- > 0)) {
				audio_index = i;
			}
			break;
		case CODEC_TYPE_VIDEO:
			if ((video_index < 0 || wanted_video_stream-- > 0)) {
				video_index = i;
			}
			break;
		default:
			break;
		}
	}
	if (1 /* show_status */) {
		dump_format(vs->ic, 0, vs->filename, 0);
		dump_stream_info(vs->ic);
	}

	/* open the streams */
	if (audio_index >= 0) {
		stream_component_open(vs, audio_index, ms);
	}

	if (video_index >= 0) {
		stream_component_open(vs, video_index, ms);
	} else {
		vs->show_audio = 1;
	}

	if (vs->video_stream < 0 && vs->audio_stream < 0) {
		FFMPEG_DEBUG_AVF("\"%s\": could not open codecs\n",
				 vs->filename);
		goto fail;
	}

	/* keep the context */
	media_stream_data(ms) = vs;
	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;

	return vs;

fail:
	xfree(vs);
	return NULL;
}

static size_t
new_media_ffmpeg_read(media_substream *mss, void *outbuf, size_t length)
{
	VideoState *is = media_stream_data(mss->up); /* was arg */
	int ret;
	AVPacket pkt1, *pkt = &pkt1;
#if 0
	/* stuff normally set on the command line */
	int64_t start_time = AV_NOPTS_VALUE;
#endif
	av_init_packet(&flush_pkt);
	FFMPEG_CRITICAL("read\n");

#if 0
	/* if seeking requested, we execute it */
	if (start_time != AV_NOPTS_VALUE) {
		int64_t timestamp;

		timestamp = start_time;
		/* add the stream start time */
		if (ic->start_time != AV_NOPTS_VALUE)
			timestamp += ic->start_time;
		ret = av_seek_frame(ic, -1, timestamp, AVSEEK_FLAG_BACKWARD);
		if (ret < 0) {
			fprintf(stderr, "%s: could not seek to position %0.3f\n",
				is->filename, (double)timestamp / AV_TIME_BASE);
		}
	}
#endif

	for(;;) {
		if (is->abort_request) {
			FFMPEG_DEBUG_AVF("\n");
			break;
		}
		if (is->paused != is->last_paused) {
			is->last_paused = is->paused;
			if (is->paused)
				av_read_pause(is->ic);
			else
				av_read_play(is->ic);
		}
#ifdef CONFIG_RTSP_DEMUXER
		if (is->paused && is->ic->iformat == &rtsp_demuxer) {
			/* wait 10 ms to avoid trying to get another packet */
			/* XXX: horrible */
			SDL_Delay(10);
			continue;
		}
#endif
		if (is->seek_req) {
			int stream_index= -1;
			int64_t seek_target= is->seek_pos;

			if (is->video_stream >= 0)
				stream_index = is->video_stream;
			else if (is->audio_stream >= 0)
				stream_index = is->audio_stream;
			else if (is->subtitle_stream >= 0)
				stream_index = is->subtitle_stream;

			if (stream_index >= 0) {
				seek_target = av_rescale_q(
					seek_target, AV_TIME_BASE_Q,
					is->ic->streams[stream_index]->
					time_base);
			}

			ret = av_seek_frame(is->ic, stream_index,
					    seek_target, is->seek_flags);
			if (ret < 0) {
				FFMPEG_DEBUG_AVS("\"%s: \""
						 "error while seeking\n",
						 is->ic->filename);
			} else {
				if (is->audio_stream >= 0) {
					packet_queue_flush(&is->audioq);
					packet_queue_put(&is->audioq, &flush_pkt);
				}
				if (is->subtitle_stream >= 0) {
					packet_queue_flush(&is->subtitleq);
					packet_queue_put(&is->subtitleq, &flush_pkt);
				}
				if (is->video_stream >= 0) {
					packet_queue_flush(&is->videoq);
					packet_queue_put(&is->videoq, &flush_pkt);
				}
			}
			is->seek_req = 0;
		}

		/* if the queue are full, no need to read more */
		if (is->audioq.size > MAX_AUDIOQ_SIZE ||
		    is->videoq.size > MAX_VIDEOQ_SIZE ||
		    is->subtitleq.size > MAX_SUBTITLEQ_SIZE ||
#if defined FFMPEG_URL_FOPEN_BIOCTX_STAR_STAR
		    url_feof(is->ic->pb)
#elif defined FFMPEG_URL_FOPEN_BIOCTX_STAR
		    url_feof(&is->ic->pb)
#endif
			) {
			/* wait 10 ms */
			usleep(10);
			continue;
		}
		ret = av_read_frame(is->ic, pkt);
		if (ret < 0) {
			if (url_ferror(
#if defined FFMPEG_URL_FOPEN_BIOCTX_STAR_STAR
				    is->ic->pb
#elif defined FFMPEG_URL_FOPEN_BIOCTX_STAR
				    &is->ic->pb
#endif
				    ) == 0) {
				usleep(100); /* wait for user event */
				continue;
			} else
				break;
		}
		if (pkt->stream_index == is->audio_stream) {
			packet_queue_put(&is->audioq, pkt);
		} else if (pkt->stream_index == is->video_stream) {
			packet_queue_put(&is->videoq, pkt);
		} else if (pkt->stream_index == is->subtitle_stream) {
			packet_queue_put(&is->subtitleq, pkt);
		} else {
			av_free_packet(pkt);
		}
	}
	/* wait until the end */
	while (!is->abort_request) {
		usleep(100);
	}

	ret = 0;
	/* disable interrupting */
	global_video_state = NULL;

	/* close each stream */
	if (is->audio_stream >= 0)
		stream_component_close(is, is->audio_stream);
	if (is->video_stream >= 0)
		stream_component_close(is, is->video_stream);
	if (is->subtitle_stream >= 0)
		stream_component_close(is, is->subtitle_stream);
	if (is->ic) {
		av_close_input_file(is->ic);
		is->ic = NULL; /* safety */
	}
	url_set_interrupt_cb(NULL);

	if (ret != 0) {
#if 0
		SDL_Event event;

		event.type = FF_QUIT_EVENT;
		event.user.data1 = is;
		SDL_PushEvent(&event);
#endif
	}
	return 0;
}

static void
new_media_ffmpeg_rewind(media_substream *mss)
{
/* rewind the stream to the first frame */
	/* libavformat */
	AVFormatContext *avfc;
	AVStream *avst;
	Lisp_Media_Stream *ms = mss->up;
	int64_t start_time;
	int res = 0;
	long int si;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;

	FFMPEG_DEBUG_AVF("rewind substream 0x%lx\n",
			 (long unsigned int)mss);

	/* fetch the format context */
	if (!(avfc = media_stream_data(ms)))
		return;

	si = (long int)mss->substream_data;
	avst = avfc->streams[si];
	if ((start_time = avst->start_time) < 0) {
		start_time = 0;
	}

	FFMPEG_DEBUG_AVF("rewind (idx:%ld) to %lld\n",
			 si, (long long int)start_time);

	/* ... and reset the stream */
	res = av_seek_frame(avfc, -1, start_time, AVSEEK_FLAG_BACKWARD);

	if (res >= 0) {
		FFMPEG_DEBUG_AVF("rewind succeeded\n");
		return;
	} else {
		FFMPEG_DEBUG_AVF("rewind exitted with %d\n", res);
		return;
	}
}


Lisp_Object
media_ffmpeg_available_formats(void)
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
