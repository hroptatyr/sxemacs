/* Media functions.
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

#ifndef INCLUDED_media_h_
#define INCLUDED_media_h_

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"
#include "opaque.h"

#ifdef FILE_CODING
#include "mule/file-coding.h"
#endif

typedef enum media_drivers media_driver;
typedef enum media_types media_type;
typedef enum media_kinds media_kind;

typedef struct mtype_audio_properties mtype_audio_properties;
typedef struct mtype_video_properties mtype_video_properties;
typedef struct mkind_file_properties mkind_file_properties;
typedef struct mkind_string_properties mkind_string_properties;

typedef struct Lisp_Media_Stream Lisp_Media_Stream;
typedef struct media_substream media_substream;

typedef void *ms_driver_data_t;
typedef ms_driver_data_t(*ms_open_fun)(Lisp_Media_Stream*);
typedef void(*ms_close_fun)(ms_driver_data_t);
typedef void(*ms_print_fun)(Lisp_Object stream, Lisp_Object, int);
typedef Lisp_Object(*ms_mark_fun)(ms_driver_data_t driver_data);
typedef size_t(*ms_read_fun)(media_substream*, void*, size_t);
typedef size_t(*ms_write_fun)(media_substream*, void*, size_t);
typedef void(*ms_rewind_fun)(media_substream*);
typedef void(*ms_seek_fun)(media_substream*, size_t);

typedef struct ms_meths ms_meths;
typedef struct media_data media_data;

/* audio coercion */
typedef int32_t sxe_media_sample_t;
typedef void* audio_coerce_args;
typedef size_t(*audio_coerce_fun)(sxe_media_sample_t *dst,
				  sxe_media_sample_t *src,
				  size_t len, void *args);
typedef void(*audio_resample_fun)(void *dst, void *src, size_t len);
typedef struct media_sample_format_s media_sample_format_t;
typedef struct audio_coerce_chain_s audio_coerce_chain_t;


enum media_drivers {
	MDRIVER_UNKNOWN,
	MDRIVER_INTERNAL,
	MDRIVER_SNDFILE,
	MDRIVER_FFMPEG,
	MDRIVER_MAD,
	MDRIVER_SOX,
	MDRIVER_XINE,
	MDRIVER_GSTREAMER,
	NUMBER_OF_MEDIA_DRIVERS
};

enum media_types {
	MTYPE_UNKNOWN,
	MTYPE_AUDIO,
	MTYPE_VIDEO,
	MTYPE_IMAGE,
	NUMBER_OF_MEDIA_TYPES
};

enum media_kinds {
	MKIND_UNKNOWN,
	MKIND_FILE,
	MKIND_STRING,		/* better name? */
	MKIND_FIFO,
	MKIND_STREAM,
	NUMBER_OF_MEDIA_KINDS
};

/* type properties */
struct mtype_audio_properties {
	const char *name;	/* name of the codec or whatever */
	const char *codec_name;	/* name of the codec or whatever */
	uint16_t channels;
	uint16_t samplewidth;	/* in bits */
	uint32_t samplerate;
	uint16_t framesize;	/* == channels * samplewidth (in bytes) */
	media_sample_format_t *msf; /* function for upsampling */
	int endianness;
	int bitrate;
};

struct mtype_video_properties {
	const char *name;	/* name of the codec or whatever */
	const char *codec_name;	/* name of the codec or whatever */
	int bitrate;
	int width;
	int height;
	int aspect_num, aspect_den;
	uint16_t framesize;
	int endianness;
};

/* kind properties */
struct mkind_file_properties {
	Lisp_Object filename;
	uint32_t filesize;
};

struct mkind_string_properties {
	char *name;
	char *stream_data;
	uint32_t size;
};


struct media_substream {
	/* navigation for the doubly linked list */
	media_substream *next;
	media_substream *prev;
	Lisp_Media_Stream *up;

	/* data and handling issues */
	media_type type; 

	union {
		mtype_audio_properties *aprops;
		mtype_video_properties *vprops;
	} type_properties;

#ifdef HAVE_THREADS
	pthread_mutex_t substream_mutex;
#endif

	/* anonymous data, used by the various backends */
	/* do not use it, it is not freed by gc */
	ms_driver_data_t substream_data;
};

#define media_substream_type(mss) (mss)->type
#define media_substream_type_properties(mss) (mss)->type_properties
#define media_substream_data(mss) (mss)->substream_data
/* make it look like funs */
#define media_substream_read(mss, b, r) (media_substream_sread(mss)(mss, b, r))
#define media_substream_rewind(mss) (media_substream_srewind(mss)(mss))

#define media_substream_next(mss) (mss)->next
#define media_substream_prev(mss) (mss)->prev
#define media_substream_up(mss) (mss)->up


/* now stream driver dependent stuff */
struct ms_meths {
	ms_open_fun open;
	ms_close_fun close;
	ms_print_fun print;
	ms_mark_fun mark;
	ms_read_fun read;
	ms_write_fun write;
	ms_rewind_fun rewind;
	ms_seek_fun seek;
};

#define DECLARE_MEDIA_DRIVER(_name)					\
	extern ms_meths _name[1]
#define DECLARE_MEDIA_DRIVER_OPEN_METH(_name)				\
	static ms_driver_data_t _name##_open(Lisp_Media_Stream*)
#define DECLARE_MEDIA_DRIVER_CLOSE_METH(_name)				\
	static void _name##_close(ms_driver_data_t)
#define DECLARE_MEDIA_DRIVER_PRINT_METH(_name)				\
	static void _name##_print(Lisp_Object, Lisp_Object, int);
#define DECLARE_MEDIA_DRIVER_MARK_METH(_name)				\
	static Lisp_Object _name##_mark(ms_driver_data_t);
#define DECLARE_MEDIA_DRIVER_READ_METH(_name)				\
	static size_t _name##_read(media_substream*, void*, size_t);
#define DECLARE_MEDIA_DRIVER_WRITE_METH(_name)				\
	static size_t _name##_write(media_substream*, void*, size_t);
#define DECLARE_MEDIA_DRIVER_REWIND_METH(_name)				\
	static void _name##_rewind(media_substream*);
#define DECLARE_MEDIA_DRIVER_SEEK_METH(_name)				\
	static void _name##_seek(media_substream*, size_t);
#define DECLARE_MEDIA_DRIVER_SIMPLE_METHS(_name)			\
	DECLARE_MEDIA_DRIVER_OPEN_METH(_name);				\
	DECLARE_MEDIA_DRIVER_CLOSE_METH(_name);				\
	DECLARE_MEDIA_DRIVER_PRINT_METH(_name);				\
	DECLARE_MEDIA_DRIVER_MARK_METH(_name);				\
	DECLARE_MEDIA_DRIVER_READ_METH(_name);				\
	DECLARE_MEDIA_DRIVER_WRITE_METH(_name);				\
	DECLARE_MEDIA_DRIVER_REWIND_METH(_name)				\
	DECLARE_MEDIA_DRIVER_SEEK_METH(_name);				\
	/* we currently have no write support so define a nop here */	\
	static size_t							\
	_name##_write(media_substream *v01d, void *buf, size_t size)	\
	{								\
		return 0;						\
	}								\
	/* we currently have no seek support so define a nop here */	\
	static void							\
	_name##_seek(media_substream *v01d, size_t position)		\
	{								\
		return;							\
	}

#define DEFINE_MEDIA_DRIVER_CUSTOM(_n, _o, _c, _p, _m, _r, _w, _rw, _s)	\
	ms_meths _n[1] = { { (_o), (_c), (_p), (_m), (_r), (_w), (_rw), (_s) } }
#define DEFINE_MEDIA_DRIVER(_name, _o, _c, _p, _m, _r, _w, _rw, _s)	\
	DEFINE_MEDIA_DRIVER_CUSTOM((_name),				\
				   (_name##_##_o),			\
				   (_name##_##_c),			\
				   (_name##_##_p),			\
				   (_name##_##_m),			\
				   (_name##_##_r),			\
				   (_name##_##_w),			\
				   (_name##_##_rw),			\
				   (_name##_##_s))
#define DEFINE_MEDIA_DRIVER_SIMPLE(_name)				\
	DEFINE_MEDIA_DRIVER(_name,					\
			    open, close,				\
			    print, mark,				\
			    read, write,				\
			    rewind, seek)
#define DEFINE_MEDIA_DRIVER_EMPTY(_name)				\
	DEFINE_MEDIA_DRIVER_CUSTOM((_name),				\
				   NULL, NULL,				\
				   NULL, NULL,				\
				   NULL, NULL,				\
				   NULL, NULL)
#define SET_MEDIA_DRIVER_FUNCTION_CUSTOM(_driver, _slot, _function)	\
	(_driver->_slot = _function)
#define SET_MEDIA_DRIVER_FUNCTION(_driver, _function)			\
	SET_MEDIA_DRIVER_FUNCTION_CUSTOM(_driver,			\
					 _function,			\
					 _driver##_##_function)

#define media_stream_set_meths(_ms, _m) ((_ms)->meths = _m)
#define media_stream_meths(_ms)	((_ms)->meths)
#define media_stream_meth(_ms, _m) (media_stream_meths(_ms)->_m)
#define XMEDIA_STREAM_SET_METHS(_ms, _m) (media_stream_set_meths(	\
						  XMEDIA_STREAM(_ms), _m))
#define XMEDIA_STREAM_METHS(_ms) (media_stream_meths(XMEDIA_STREAM(_ms)))
#define XMEDIA_STREAM_METH(_ms, _m) (media_stream_meth(XMEDIA_STREAM(_ms), _m))


/* Media streams are actually containers, we use doubly linked lists with media
   substreams inside */
struct Lisp_Media_Stream {
	struct lcrecord_header lheader;

	media_substream *first;	/* pointer to first substream */
	media_substream *last;	/* pointer to last substream */

	media_kind kind; 
	media_driver driver;

	union {
		mkind_file_properties *fprops;
		mkind_string_properties *sprops;
	} kind_properties;

	/* methods for the stream */
	const ms_meths *meths;

	/* anonymous data, used by the various backends */
	void *stream_data;
};

extern Lisp_Object Qmedia_streamp;
extern Lisp_Object Qunknown;

DECLARE_LRECORD(media_stream, Lisp_Media_Stream);
#define XMEDIA_STREAM(x) XRECORD(x, media_stream, Lisp_Media_Stream)
#define XSETMEDIA_STREAM(x, p) XSETRECORD(x, p, media_stream)
#define wrap_media_stream(p) wrap_object(p)
#define MEDIA_STREAMP(x) RECORDP(x, media_stream)
#define CHECK_MEDIA_STREAM(x) CHECK_RECORD(x, media_stream)
#define CONCHECK_MEDIA_STREAM(x) CONCHECK_RECORD(x, media_stream)

#define media_stream_first(ms) (ms)->first
#define media_stream_last(ms) (ms)->last
#define media_stream_driver(ms) (ms)->driver
#define media_stream_kind(ms) (ms)->kind
#define media_stream_kind_properties(ms) (ms)->kind_properties
#define media_stream_data(ms) (ms)->stream_data
#define media_stream_1sub(ms) media_stream_first(ms)
#define media_stream_nsub(ms) media_stream_last(ms)
#define XMEDIA_STREAM_FIRST(x) media_stream_first(XMEDIA_STREAM(x))
#define XMEDIA_STREAM_LAST(x) media_stream_last(XMEDIA_STREAM(x))
#define XMEDIA_STREAM_DRIVER(x) media_stream_driver(XMEDIA_STREAM(x))
#define XMEDIA_STREAM_KIND(x) media_stream_kind(XMEDIA_STREAM(x))
#define XMEDIA_STREAM_DATA(x) media_stream_data(XMEDIA_STREAM(x))
#define XMEDIA_STREAM_1SUB(x) media_stream_1sub(XMEDIA_STREAM(x))
#define XMEDIA_STREAM_NSUB(x) media_stream_nsub(XMEDIA_STREAM(x))


extern Lisp_Object make_media_stream(void);
extern media_substream *make_media_substream(void);
extern media_substream *make_media_substream_append(Lisp_Media_Stream*);
extern media_substream *make_media_substream_prepend(Lisp_Media_Stream*);

extern Lisp_Object Q_data, Q_file, Q_url;

EXFUN(Fmake_media_stream, 3);

/* A struct for virtual-I/O */
struct media_data {
	size_t length;
	long seek;
	char *data;
};

/******************/
/* audio coercion */
/******************/
struct media_sample_format_s {
	audio_resample_fun upsample;
	audio_resample_fun downsample;
};

struct audio_coerce_chain_s {
	audio_coerce_fun fun;
	audio_coerce_args args;
	audio_coerce_chain_t *next;
	audio_coerce_chain_t *last;
};

#define DECLARE_MEDIA_SAMPLE_FORMAT(_name)	\
	extern media_sample_format_t _name[1]
#define DECLARE_MEDIA_SAMPLE_FORMAT_UP(_name)	\
	static void _name##_up(void*, void*, size_t);
#define DECLARE_MEDIA_SAMPLE_FORMAT_DOWN(_name)	\
	static void _name##_down(void*, void*, size_t);
#define DECLARE_MEDIA_SAMPLE_FORMAT_SIMPLE(_name)	\
	DECLARE_MEDIA_SAMPLE_FORMAT_UP(_name);	\
	DECLARE_MEDIA_SAMPLE_FORMAT_DOWN(_name);

#define DEFINE_MEDIA_SAMPLE_FORMAT_CUSTOM(_name, _up, _down)		\
	media_sample_format_t _name[1] = { { (_up), (_down) } }
#define DEFINE_MEDIA_SAMPLE_FORMAT(_name, _up, _down)			\
	DEFINE_MEDIA_SAMPLE_FORMAT_CUSTOM((_name),			\
					  (_name##_##_up),		\
					  (_name##_##_down))
#define DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(_name)			\
	DECLARE_MEDIA_SAMPLE_FORMAT_SIMPLE(_name)			\
	DEFINE_MEDIA_SAMPLE_FORMAT(_name, up, down)
#define DEFINE_MEDIA_SAMPLE_FORMAT_EMPTY(_name)				\
	DEFINE_MEDIA_SAMPLE_FORMAT_CUSTOM((_name), NULL, NULL)

#define MEDIA_SAMPLE_FORMAT_UPSAMPLE(_msf) (_msf)->upsample
#define MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(_msf) (_msf)->downsample

#define MEDIA_SAMPLE_FORMAT(_msf) (_msf)


/* `effects' */
#define DECLARE_MEDIA_SAMPLE_EFFECT(_name, args...)	\
	extern audio_coerce_fun _name[1];		\
	typedef struct {				\
			args				\
	} _name##_args;

#define DEFINE_MEDIA_SAMPLE_EFFECT(_name, _fun)		\
	static size_t _fun(sxe_media_sample_t *,	\
			   sxe_media_sample_t *,	\
			   size_t, void *);		\
	audio_coerce_fun _name[1] = { (_fun) }

#define MEDIA_SAMPLE_EFFECT(_mse) (_mse)[0]

#define MEDIA_SAMPLE_VOLUME_NORM 0x7f
#define MEDIA_SAMPLE_VOLUME_MAX  0xff
#define MEDIA_SAMPLE_VOLUME_MIN  0x00
#define MEDIA_MAX_AUDIO_CHANNELS 16 /* maximal number of audio channels */

#define PUT_MEDIA_SAMPLE_EFFECT(_chain, _idx, _mse, _mse_arg)	\
	do {							\
		(&(_chain[(_idx)]))->fun = (_mse);		\
		(&(_chain[(_idx)]))->args = (_mse_arg);		\
	} while (0)
#define ADD_MEDIA_SAMPLE_EFFECT(_chain, _idx, _mse, _mse_arg)		\
	do {								\
		PUT_MEDIA_SAMPLE_EFFECT(_chain, _idx, _mse, _mse_arg);	\
		(_idx)++;						\
	} while (0)
#define GET_MEDIA_SAMPLE_EFFECT_FUN(_chain, _idx)			\
	(&(_chain[(_idx)]))->fun
#define GET_MEDIA_SAMPLE_EFFECT_ARGS(_chain, _idx)			\
	(&(_chain[(_idx)]))->args
#define CALL_MEDIA_SAMPLE_EFFECT(_chain, _idx, _dst, _src, _len)	\
	GET_MEDIA_SAMPLE_EFFECT_FUN(_chain, _idx)(			\
		(_dst), (_src), (_len),					\
		GET_MEDIA_SAMPLE_EFFECT_ARGS(_chain, _idx))

/* some predefined sample formats */
DECLARE_MEDIA_SAMPLE_FORMAT(sxe_msf_U8);
DECLARE_MEDIA_SAMPLE_FORMAT(sxe_msf_S16);
DECLARE_MEDIA_SAMPLE_FORMAT(sxe_msf_FLT);
DECLARE_MEDIA_SAMPLE_FORMAT(sxe_msf_S32);
DECLARE_MEDIA_SAMPLE_FORMAT(sxe_msf_S24); /* format internally used */

/* some predefined effects */
DECLARE_MEDIA_SAMPLE_EFFECT(sxe_mse_1ch_to_2ch);
DECLARE_MEDIA_SAMPLE_EFFECT(sxe_mse_2ch_to_1ch,
			    int chan;);	/* which channel to keep */
DECLARE_MEDIA_SAMPLE_EFFECT(sxe_mse_5ch_to_2ch,
			    int chan1;
			    int chan2;);
DECLARE_MEDIA_SAMPLE_EFFECT(sxe_mse_volume,
			    int num_channels;
			    uint8_t volume[MEDIA_MAX_AUDIO_CHANNELS];);
DECLARE_MEDIA_SAMPLE_EFFECT(sxe_mse_rerate,
			    size_t srcrate;
			    size_t tgtrate;
			    float tweak;
			    int num_channels;);

#define SXE_MAX_S32  2147483647.0f
#define SXE_MAX_S24  8388607.0f
#define SXE_MAX_S16  32767.0f
#define SXE_MAX_S8   127.0f
#define SXE_MAX_U8   255.0f

#define MEDIA_MAX_AUDIO_FRAME_SIZE 384000 /* 2 secs of 48kHz, 32bit, stereo */

#ifdef ALL_DEBUG_FLAGS
#undef MEDIA_DEBUG_FLAG
#define MEDIA_DEBUG_FLAG
#endif

#define __MEDIA_DEBUG__(args...)	fprintf(stderr, "MEDIA " args)
#ifndef MEDIA_DEBUG_FLAG
#define MEDIA_DEBUG(args...)
#else
#define MEDIA_DEBUG(args...)		__MEDIA_DEBUG__(args)
#endif
#define MEDIA_DEBUG_FMT(args...)	MEDIA_DEBUG("[format] " args)
#define MEDIA_DEBUG_COE(args...)	MEDIA_DEBUG("[coerce] " args)
#define MEDIA_CRITICAL(args...)		__MEDIA_DEBUG__("CRITICAL: " args)

#endif	/* INCLUDED_media_h_ */
