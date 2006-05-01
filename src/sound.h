/* Sound functions.
   Copyright (C) 1992, 1993, 1994 Lucid Inc.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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

#ifndef INCLUDED_sound_h_
#define INCLUDED_sound_h_

#include "media.h"

enum audio_drivers {
	ADRIVER_UNDECIDED,
	ADRIVER_OSS,
	ADRIVER_NAS,
	ADRIVER_ESD,
	ADRIVER_POLYP,
	ADRIVER_ARTS,
	ADRIVER_JACK,
	ADRIVER_ALSA,
	ADRIVER_AO,
	NUMBER_OF_AUDIO_DRIVERS
};
typedef enum audio_drivers audio_driver;

enum audio_states {
	ASTATE_UNDECIDED,
	ASTATE_ALIVE,
	ASTATE_SUSPENDED,
	ASTATE_DEAD,
	NUMBER_OF_AUDIO_STATES
};
typedef enum audio_states audio_state;

extern Lisp_Object Vdefault_audio_device;

struct Lisp_Audio_Device {
	struct lcrecord_header lheader;
	audio_driver driver;
	audio_state state;
	void *device_data;
};
typedef struct Lisp_Audio_Device Lisp_Audio_Device;

DECLARE_LRECORD(audio_device, Lisp_Audio_Device);
#define XAUDIO_DEVICE(x) XRECORD(x, audio_device, Lisp_Audio_Device)
#define XSETAUDIO_DEVICE(x, p) XSETRECORD(x, p, audio_device)
#define wrap_audio_device(p) wrap_object(p)
#define AUDIO_DEVICEP(x) RECORDP(x, audio_device)
#define CHECK_AUDIO_DEVICE(x) CHECK_RECORD(x, audio_device)
#define CONCHECK_AUDIO_DEVICE(x) CONCHECK_RECORD(x, audio_device)

#define audio_device_driver(ad) (ad)->driver
#define audio_device_state(ad) (ad)->state
#define audio_device_data(ad) (ad)->device_data
#define XAUDIO_DEVICE_DRIVER(x) audio_device_driver(XAUDIO_DEVICE(x))
#define XAUDIO_DEVICE_STATE(x) audio_device_state(XAUDIO_DEVICE(x))
#define XAUDIO_DEVICE_DATA(x) audio_device_data(XAUDIO_DEVICE(x))

extern audio_driver decode_audio_type(Lisp_Object);
extern audio_driver decode_audio_device(Lisp_Object);
extern void *get_audio_device_data(Lisp_Object);
extern Lisp_Audio_Device *get_audio_device(Lisp_Object);
extern Lisp_Object make_audio_device(Lisp_Object);

#define SOUND_MAX_AUDIO_FRAME_SIZE 192000 /* 1 sec of 48kHz, 32bit, stereo */

/* 
 * Threads are the containers for the streams. Streams are stored
 * (along with devices) inside threads, while substreams are stored inside
 * subthreads. In source/sink language, a thread is the cable to plug a source
 * (stream) to a sink (device).
 * 
 * This brings us to:
 * 
 *                      up  +========+  up
 *                  ,-----> | Thread | <-----,
 *                 /        +--------+        \
 *                /         | Stream |         \
 *               /          | Device |          \
 *              /           | State  |           \
 *             /            | PState |            \
 *            /             | Result |             \
 *           /              +========+              \
 *           |                   ^                  |
 *     first |                   | up               | last
 *           v                   |                  v
 *    +==========+    next +==========+    next +==========+
 *    |subthread1| <-----> |subthread2| <-----> |subthread3|
 *    +----------+ prev    +----------+ prev    +----------+
 *    |substream1|         |substream2|         |substream3|
 *    |pthread_t1|	   |pthread_t2|         |pthread_t3|
 *    |privdata1 |	   |privdata2 |	        |privdata3 |
 *    +==========+	   +==========+	        +==========+
 * 
 * Note: It is yet _not_ possible to specify different devices for each
 * subthread. This will require another split of the device structure into
 * a device+subdevice tree.
 */


/* some defs for the pthreaded version */

enum media_thread_states {
	MTSTATE_UNDECIDED,
	MTSTATE_RUNNING,
	MTSTATE_FINISHED,
	NUMBER_OF_MEDIA_THREAD_STATES
};
typedef enum media_thread_states media_thread_state;

enum media_thread_play_states {
	MTPSTATE_UNDECIDED,
	MTPSTATE_RUN,
	MTPSTATE_PAUSE,
	MTPSTATE_STOP,
	NUMBER_OF_MEDIA_THREAD_PLAY_STATES
};
typedef enum media_thread_play_states media_thread_play_state;

typedef struct media_subthread media_subthread;
typedef struct Lisp_Media_Thread Lisp_Media_Thread;
struct media_subthread {
	/* navigation issues, set by append/prepend constructors */
	media_subthread *next;
	media_subthread *prev;
	Lisp_Media_Thread *up;
	
	/* data and handling info, not set by constructors */
	media_substream *substream;
#ifdef HAVE_THREADS
	pthread_t thread;
#endif

	/* anonymous data, used by the various backends */
	/* do not use it, it is not freed by gc */
	void *subthread_data;
};

#define media_subthread_next(mt) (mt)->next
#define media_subthread_prev(mt) (mt)->prev
#define media_subthread_up(mt) (mt)->up


#define MTPSTATE_REACT_TIME 20000	/* in usec */

struct Lisp_Media_Thread {
	struct lcrecord_header lheader;
	Lisp_Object stream;
	Lisp_Object device;
	media_thread_state state;
	media_thread_play_state play_state;
	Lisp_Object result;
	Lisp_Object sentinel;

	/* navigation issues */
	media_subthread *first;
	media_subthread *last;
};

DECLARE_LRECORD(media_thread, Lisp_Media_Thread);
#define XMEDIA_THREAD(x) XRECORD(x, media_thread, Lisp_Media_Thread)
#define XSETMEDIA_THREAD(x, p) XSETRECORD(x, p, media_thread)
#define wrap_media_thread(p) wrap_object(p)
#define MEDIA_THREADP(x) RECORDP(x, media_thread)
#define CHECK_MEDIA_THREAD(x) CHECK_RECORD(x, media_thread)
#define CONCHECK_MEDIA_THREAD(x) CONCHECK_RECORD(x, media_thread)

#define media_thread_first(mt) (mt)->first
#define media_thread_last(mt) (mt)->first

extern Lisp_Object make_media_thread(Lisp_Object, Lisp_Object);

#endif	/* INCLUDED_sound_h_ */
