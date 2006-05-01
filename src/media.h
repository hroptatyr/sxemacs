/* Media functions.
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

#ifndef INCLUDED_media_h_
#define INCLUDED_media_h_

enum media_drivers {
	MDRIVER_UNDECIDED,
	MDRIVER_NATIVE,
	MDRIVER_SNDFILE,
	MDRIVER_FFMPEG,
	MDRIVER_MAD,
	MDRIVER_SOX,
	MDRIVER_XINE,
	MDRIVER_GSTREAMER,
	NUMBER_OF_MEDIA_DRIVERS
};
typedef enum media_drivers media_driver;

enum media_types {
	MTYPE_UNDECIDED,
	MTYPE_AUDIO,
	MTYPE_VIDEO,
	MTYPE_IMAGE,
	NUMBER_OF_MEDIA_TYPES
};
typedef enum media_types media_type;

enum media_kinds {
	MKIND_UNDECIDED,
	MKIND_FILE,
	MKIND_STRING,		/* better name? */
	MKIND_FIFO,
	MKIND_STREAM,
	NUMBER_OF_MEDIA_KINDS
};
typedef enum media_kinds media_kind;

/* type properties */
struct mtype_audio_properties {
	const char *name;	/* name of the codec or whatever */
	const char *codec_name;	/* name of the codec or whatever */
	uint16_t channels;
	uint16_t samplerate;
	uint16_t samplewidth;	/* in bits */
	uint16_t framesize;	/* == channels * samplewidth */
	int endianness;
	int bitrate;
};
typedef struct mtype_audio_properties mtype_audio_properties;

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
typedef struct mtype_video_properties mtype_video_properties;

/* kind properties */
struct mkind_file_properties {
	char *filename;
	uint32_t filesize;
};
typedef struct mkind_file_properties mkind_file_properties;

struct mkind_string_properties {
	char *name;
	char *stream_data;
	uint32_t size;
};
typedef struct mkind_string_properties mkind_string_properties;


/* forward declarations */
typedef struct Lisp_Media_Stream Lisp_Media_Stream;
typedef struct media_substream media_substream;

typedef uint32_t(*sread_function)(media_substream*, void*, uint32_t);
typedef void(*srewind_function)(media_substream*);

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

	/* a read function for the substream,
	   request to read `length' many frames into `outbuf'
	   return the number of read frames
	*/
	sread_function sread;
	/* a rewind function for the substream,
	   i.e. start at the first frame on next sread request
	*/
	srewind_function srewind;

#ifdef HAVE_THREADS
	pthread_mutex_t substream_mutex;
#endif

	/* anonymous data, used by the various backends */
	/* do not use it, it is not freed by gc */
	void *substream_data;
};

#define media_substream_type(mss) (mss)->type
#define media_substream_type_properties(mss) (mss)->type_properties
#define media_substream_sread(mss) (mss)->sread
#define media_substream_srewind(mss) (mss)->srewind
#define media_substream_data(mss) (mss)->substream_data
/* make it look like funs */
#define media_substream_read(mss, b, r) (media_substream_sread(mss)(mss, b, r))
#define media_substream_rewind(mss) (media_substream_srewind(mss)(mss))

#define media_substream_next(mss) (mss)->next
#define media_substream_prev(mss) (mss)->prev
#define media_substream_up(mss) (mss)->up



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

	/* anonymous data, used by the various backends */
	void *stream_data;
};

extern Lisp_Object Qmedia_streamp;

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

extern Lisp_Object Q_data, Q_file;

EXFUN(Fmake_media_stream, 2);

/* A struct for virtual-I/O */
struct sound_data {
	size_t length;
	long seek;
	char *data;
};
typedef struct sound_data sound_data;

#define MEDIA_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec of 48kHz, 32bit, stereo */

#endif	/* INCLUDED_media_h_ */
