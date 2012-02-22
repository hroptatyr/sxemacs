/* New Generation Sound Functions.
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

#ifndef INCLUDED_sound_h_
#define INCLUDED_sound_h_

#include "media.h"
#include "semaphore.h"
#ifdef EF_USE_ASYNEQ
#include "events/event-queue.h"
#endif

extern Lisp_Object Q_device, Q_keep_open, Q_server, Q_client;


typedef enum audio_drivers audio_driver;
typedef enum audio_states audio_state;
typedef enum media_thread_states media_thread_state;
typedef enum media_thread_play_states media_thread_play_state;
typedef struct ad_meths ad_meths;
typedef struct Lisp_Audio_Device Lisp_Audio_Device;

typedef enum media_thread_states media_thread_state_t;
typedef enum media_thread_play_states media_thread_play_state_t;
typedef struct audio_job_s *audio_job_t;

typedef void ad_device_data;
typedef ad_device_data*(*ad_create_fun)(Lisp_Object options);
typedef void(*ad_finish_fun)(ad_device_data*);
typedef void(*ad_print_fun)(Lisp_Object device, Lisp_Object, int);
typedef Lisp_Object(*ad_mark_fun)(ad_device_data *device_data);
typedef int(*ad_play_fun)(audio_job_t);
typedef int(*ad_record_fun)(audio_job_t);


enum audio_drivers {
	ADRIVER_UNKNOWN,
	ADRIVER_OSS,
	ADRIVER_NAS,
	ADRIVER_ESD,
	ADRIVER_POLYP,
	ADRIVER_PULSE,
	ADRIVER_JACK,
	ADRIVER_ALSA,
	ADRIVER_AO,
	NUMBER_OF_AUDIO_DRIVERS
};

enum audio_states {
	ASTATE_UNKNOWN,
	ASTATE_ALIVE,
	ASTATE_SUSPENDED,
	ASTATE_DEAD,
	NUMBER_OF_AUDIO_STATES
};


#ifdef ALL_DEBUG_FLAGS
#undef SOUND_DEBUG_FLAG
#define SOUND_DEBUG_FLAG
#endif

#define __SOUND_DEBUG__(args...)	fprintf(stderr, "SOUND " args)
#ifndef SOUND_DEBUG_FLAG
#define SOUND_DEBUG(args...)
#else
#define SOUND_DEBUG(args...)		__SOUND_DEBUG__(args)
#endif
#define SOUND_DEBUG_DEV(args...)	SOUND_DEBUG("[audio-device]: " args)
#define SOUND_DEBUG_MT(args...)		SOUND_DEBUG("[media-thread]: " args)
#define SOUND_DEBUG_MW(args...)		SOUND_DEBUG("[media-workers]: " args)
#define SOUND_DEBUG_MW_ENQ(args...)		\
	SOUND_DEBUG_MW("[enqueue]: st:0x%x, " args)
#define SOUND_DEBUG_MW_DEQ(args...)		\
	SOUND_DEBUG_MW("[dequeue]: st:0x%x, " args)
#define SOUND_DEBUG_PT(args...)		SOUND_DEBUG("[pthread]: " args)
#define SOUND_DEBUG_AJ(args...)		SOUND_DEBUG("[audio-job]: " args)
#define SOUND_CRITICAL(args...)		__SOUND_DEBUG__("CRITICAL: " args)

/* now device driver dependent stuff */
struct ad_meths {
	ad_create_fun create;
	ad_finish_fun finish;
	ad_print_fun print;
	ad_mark_fun mark;
	ad_play_fun play;
	ad_record_fun record;
};

extern Lisp_Object Vdefault_audio_device;

struct Lisp_Audio_Device {
	struct lcrecord_header lheader;
	audio_driver driver;
	audio_state state;
	const ad_meths *meths;
	ad_device_data *device_data;
	pthread_mutex_t device_data_mtx;
};

DECLARE_LRECORD(audio_device, Lisp_Audio_Device);
#define XAUDIO_DEVICE(x) XRECORD(x, audio_device, Lisp_Audio_Device)
#define XSETAUDIO_DEVICE(x, p) XSETRECORD(x, p, audio_device)
#define wrap_audio_device(p) wrap_object(p)
#define AUDIO_DEVICEP(x) RECORDP(x, audio_device)
#define CHECK_AUDIO_DEVICE(x) CHECK_RECORD(x, audio_device)
#define CONCHECK_AUDIO_DEVICE(x) CONCHECK_RECORD(x, audio_device)

#define audio_device_driver(ad) ((ad)->driver)
#define audio_device_state(ad) ((ad)->state)
#define audio_device_data(ad) ((ad)->device_data)
#define audio_device_set_meths(_ad, _m) ((_ad)->meths = _m)
#define audio_device_meths(_ad)	((_ad)->meths)
#define audio_device_meth(_ad, _m) (audio_device_meths(_ad)->_m)
#define XAUDIO_DEVICE_DRIVER(x) (audio_device_driver(XAUDIO_DEVICE(x)))
#define XAUDIO_DEVICE_STATE(x) (audio_device_state(XAUDIO_DEVICE(x)))
#define XAUDIO_DEVICE_DATA(x) (audio_device_data(XAUDIO_DEVICE(x)))
#define XAUDIO_DEVICE_SET_METHS(_d, _m) (audio_device_set_meths(_d, _m))
#define XAUDIO_DEVICE_METHS(_d)	(audio_device_meths(XAUDIO_DEVICE(_d)))
#define XAUDIO_DEVICE_METH(_d, _m) (audio_device_meth(XAUDIO_DEVICE(_d), _m))

extern audio_driver decode_audio_type(Lisp_Object);
extern audio_driver decode_audio_device(Lisp_Object);
extern void *get_audio_device_data(Lisp_Object);
extern Lisp_Audio_Device *get_audio_device(Lisp_Object);

EXFUN(Fmake_audio_device, MANY);
EXFUN(Faudio_device_p, 1);
#if 0
EXFUN(Fdelete_audio_device, 1);	/* too dangerous at the moment */
#endif

#define DECLARE_AUDIO_DEVICE(_name)					\
	extern ad_meths _name[1]
#define DECLARE_AUDIO_DEVICE_SIMPLE_METHS(_name)			\
	static ad_device_data *_name##_create(Lisp_Object);		\
	static void _name##_finish(ad_device_data*);			\
	static void _name##_print(Lisp_Object, Lisp_Object, int);	\
	static Lisp_Object _name##_mark(ad_device_data*);		\
	static int _name##_play(audio_job_t);				\
	static int _name##_record(audio_job_t);				\
	/* we currently have no record support so define a nop here */	\
	static int							\
	_name##_record(audio_job_t SXE_UNUSED(foo))			\
	{								\
		return 0;						\
	}

#define DEFINE_AUDIO_DEVICE_CUSTOM(_na, _cr, _fi, _pr, _ma, _pl, _re)	\
	ad_meths _na[1] = { { (_cr), (_fi), (_pr), (_ma), (_pl), (_re) } }
#define DEFINE_AUDIO_DEVICE(_name, _crea, _fini, _prin, _mark, _play, _reco) \
	DEFINE_AUDIO_DEVICE_CUSTOM((_name),				\
				   (_name##_##_crea),			\
				   (_name##_##_fini),			\
				   (_name##_##_prin),			\
				   (_name##_##_mark),			\
				   (_name##_##_play),			\
				   (_name##_##_reco))
#define DEFINE_AUDIO_DEVICE_SIMPLE(_name)				\
	DEFINE_AUDIO_DEVICE(_name,					\
			    create, finish, print, mark, play, record)
#define DEFINE_AUDIO_DEVICE_EMPTY(_name)				\
	DEFINE_AUDIO_DEVICE_CUSTOM((_name),				\
				   NULL, NULL, NULL, NULL, NULL, NULL)
#define DEFINE_AUDIO_DEVICE_EMPTY_BUT_PLAY(_name, _play)		\
	DEFINE_AUDIO_DEVICE_CUSTOM((_name),				\
				   NULL, NULL, NULL, NULL, (_play), NULL)
#define SET_AUDIO_DEVICE_FUNCTION_CUSTOM(_device, _slot, _function)	\
	(_device->_slot = _function)
#define SET_AUDIO_DEVICE_FUNCTION(_device, _function)			\
	SET_AUDIO_DEVICE_FUNCTION_CUSTOM(_device,			\
					 _function,			\
					 _device##_##_function)


/* maximal number of channels, 8ch should be okay */
#define SOUND_MAX_CHANNELS	8
/* maximal sample width */
#define SOUND_MAX_SAMPLE_WIDTH	sizeof(uint32_t)
/* maximal sampling frequency, could be 96 kHz though */
#define SOUND_MAX_SAMPLE_FREQ	48000
/* 1 sec of sound at extreme values */
#define SOUND_MAX_AUDIO_FRAME_SIZE		\
	SOUND_MAX_SAMPLE_FREQ * SOUND_MAX_SAMPLE_WIDTH * SOUND_MAX_CHANNELS

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
	MTSTATE_UNKNOWN,
	MTSTATE_RUNNING,
	MTSTATE_FINISHED,
	NUMBER_OF_MEDIA_THREAD_STATES
};

enum media_thread_play_states {
	MTPSTATE_UNKNOWN,
	MTPSTATE_RUN,
	MTPSTATE_PAUSE,
	MTPSTATE_STOP,
	NUMBER_OF_MEDIA_THREAD_PLAY_STATES
};

struct audio_job_s {
	/* data and handling info */
	Lisp_Object stream;
	Lisp_Object device;
	media_substream *substream;

	/* state information */
	media_thread_state state;
	media_thread_play_state play_state;
#if defined(HAVE_THREADS)
	sxe_mutex_t mtx;
#endif
#if defined(EF_USE_ASYNEQ)
	event_queue_t queue;
#endif

	/* anonymous data, used by the various backends */
	/* do not use it, it is not freed by gc */
	void *job_device_data;
	void *job_stream_data;

	/* cache buffer */
	char *buffer;
	size_t buffer_alloc_size;

	/* audio-specific job options */
	int resolution;
	int framesize;
	int channels;
	int volume;
	uint8_t chanvol[MEDIA_MAX_AUDIO_CHANNELS];
	float ratetrafo;

	/* communication objects */
	Lisp_Object result;
	Lisp_Object sentinel;
};

#define MTPSTATE_REACT_TIME 20000	/* in usec */

enum sxe_finalise_conds {
	SXE_SMPH_SHALL_START,
	SXE_SMPH_HAVE_STARTED,
	SXE_SMPH_SHALL_FINISH,
	SXE_SMPH_HAVE_FINISHED
};

#define audio_job(_x)			(((audio_job_t)worker_job_data(_x)))
#define audio_job_stream(_x)		(audio_job(_x)->stream)
#define audio_job_device(_x)		(audio_job(_x)->device)
#define audio_job_sentinel(_x)		(audio_job(_x)->sentinel)
#define audio_job_device_data(_aj)	(_aj)->job_device_data
#define audio_job_stream_data(_aj)	(_aj)->job_stream_data
#define audio_job_queue(_aj)		(_aj)->queue
#define XAUDIO_JOB(_x)			((audio_job_t)(XWORKER_JOB_DATA(_x)))
#define XAUDIO_JOB_STREAM(_x)		audio_job_stream(XWORKER_JOB(_x))
#define XAUDIO_JOB_DEVICE(_x)		audio_job_device(XWORKER_JOB(_x))
#define XAUDIO_JOB_SENTINEL(_x)		audio_job_sentinel(XWORKER_JOB(_x))
#define XAUDIO_JOB_QUEUE(_x)		(audio_job_queue(XAUDIO_JOB(_x)))
#define AUDIO_JOBP(_x)							\
	(WORKER_JOBP(_x) && XWORKER_JOB_HANDLER(_x) == &audio_job_handler)
#define CHECK_AUDIO_JOB(_x)						\
	do {								\
		if (!AUDIO_JOBP(_x))					\
			dead_wrong_type_argument(Qaudio_jobp, _x);	\
	} while (0)
#define CONCHECK_AUDIO_JOB(_x)						\
	do {								\
		if (!AUDIO_JOBP(_x))					\
			return wrong_type_argument(Qaudio_jobp, _x);	\
	} while (0)

#if 0
#define SXE_WORKERS_SMPH_INIT(_mt)					\
	SOUND_DEBUG_MW("initialising state semaphore: 0x%x\n",		\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_INIT(&(_mt)->state_smph);
#define SXE_WORKERS_SMPH_FINI(_mt)					\
	SOUND_DEBUG_MW("finishing state semaphore: 0x%x\n",		\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_FINI(&(_mt)->state_smph);
#define SXE_WORKERS_SMPH_LOCK(_mt)					\
	SOUND_DEBUG_MW("locking mutex of state semaphore: 0x%x\n",	\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_LOCK(&(_mt)->state_smph);
#define SXE_WORKERS_SMPH_UNLOCK(_mt)					\
	SOUND_DEBUG_MW("unlocking mutex of state semaphore: 0x%x\n",	\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_UNLOCK(&(_mt)->state_smph);

#define SXE_WORKERS_SMPH_SIGNAL(_mt, _state)				\
	SOUND_DEBUG_MW("signalling "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_SIGNAL(&(_mt)->state_smph, _state);			\
	SOUND_DEBUG_MW("signalled "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));
#define SXE_WORKERS_SMPH_BROADCAST(_mt, _state)				\
	SOUND_DEBUG_MW("broadcasting "#_state": 0x%x\n",		\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_BROADCAST(&(_mt)->state_smph, _state);		\
	SOUND_DEBUG_MW("broadcast "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));
#define SXE_WORKERS_SMPH_WAIT(_mt, _state)				\
	SOUND_DEBUG_MW("waiting for "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_WAIT(&(_mt)->state_smph, _state);			\
	SOUND_DEBUG_MW("ACK, got "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));

#define SXE_WORKERS_SMPH_SYNCH(_mt, _state)				\
	SOUND_DEBUG_MW("waiting for "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_SYNCH(&(_mt)->state_smph, _state);			\
	SOUND_DEBUG_MW("ACK, got "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));
#define WITH_SXE_WORKERS_SMPH_SYNCH(_mt, _state, args...)		\
	SOUND_DEBUG_MW("waiting for "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));				\
	WITH_SXE_MSEMAPH_SYNCH(&(_mt)->state_smph, _state, args);	\
	SOUND_DEBUG_MW("ACK, got "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));
#define SXE_WORKERS_SMPH_TRIGGER(_mt, _state)				\
	SOUND_DEBUG_MW("triggering "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_TRIGGER(&(_mt)->state_smph, _state);		\
	SOUND_DEBUG_MW("triggered "#_state": 0x%x\n",			\
		       (unsigned int)(_mt));
#define SXE_WORKERS_SMPH_TRIGGER_ALL(_mt, _state)			\
	SOUND_DEBUG_MW("triggering all "#_state": 0x%x\n",		\
		       (unsigned int)(_mt));				\
	SXE_MSEMAPH_TRIGGER_ALL(&(_mt)->state_smph, _state);		\
	SOUND_DEBUG_MW("triggered all "#_state": 0x%x\n",		\
		       (unsigned int)(_mt));
#endif

/* convenience macros, dunno how sensible they are */
#define SOUND_UNPACK_MT(aj, dev, ms, mss, ad, cad, mtap)		\
	/* unpack the media thread */					\
	dev = (aj)->device;						\
	ms = XMEDIA_STREAM((aj)->stream);				\
	mss = aj->substream;						\
									\
	/* unpack device */						\
	ad = get_audio_device(dev);					\
	cad = get_audio_device_data(dev);				\
									\
	/* cannot use incomplete or corrupt audio devices */		\
	if ((ad) == NULL || cad == NULL ||				\
	    media_stream_meth((ms), rewind) == NULL ||			\
	    media_stream_meth((ms), read) == NULL ||			\
	    XAUDIO_DEVICE_DRIVER(dev) != MYSELF)			\
		return 0;						\
									\
	/* cannot use most devices to play non-audio stuff */		\
	if (media_substream_type(mss) != MTYPE_AUDIO)			\
		return 0;						\
	mtap = media_substream_type_properties(mss).aprops;


#ifdef EF_USE_ASYNEQ
/* for the communication with (async'ly) running audio jobs */
typedef struct audio_job_event_s *audio_job_event_t;
typedef enum audio_job_event_kinds audio_job_event_kind_t;
typedef enum audio_job_change_states audio_job_change_state_t;
typedef int audio_job_change_volume_t;
typedef float audio_job_change_rate_t;
typedef union audio_job_event_args_u *audio_job_event_args_t;

enum audio_job_change_states {
	aj_pause,
	aj_resume,
	aj_start,
	aj_stop,
	no_audio_job_change_states
};

union audio_job_event_args_u {
	audio_job_change_state_t state_args;
	audio_job_change_volume_t volume_args;
	audio_job_change_rate_t rate_args;
};

enum audio_job_event_kinds {
	aj_change_state,
	aj_change_volume,
	aj_change_rate,
	no_audio_job_event_kinds
};

struct audio_job_event_s {
	audio_job_event_kind_t kind;
	union audio_job_event_args_u args;
	int free_after_use;
};

#define audio_job_event_kind(_x)	((_x)->kind)
#define audio_job_event_args(_x)	((_x)->args)
#define audio_job_event_fau(_x)		((_x)->free_after_use)

/* some often used events */
extern struct audio_job_event_s pause_event;
extern struct audio_job_event_s resume_event;
extern struct audio_job_event_s start_event;
extern struct audio_job_event_s stop_event;
extern struct audio_job_event_s change_volume_event;
extern struct audio_job_event_s change_rate_event;

extern_inline audio_job_event_t
make_audio_job_event(audio_job_event_kind_t kind);

extern_inline audio_job_event_t
make_audio_job_event(audio_job_event_kind_t kind)
{
	audio_job_event_t aje = xnew(struct audio_job_event_s);
	audio_job_event_kind(aje) = kind;
	audio_job_event_fau(aje) = 1;
	return aje;
}

static inline void
free_audio_job_event(audio_job_event_t aje)
{
	if (audio_job_event_fau(aje)) {
		xfree(aje);
	}
	return;
}

#endif	/* EF_USE_ASYNEQ */

#endif	/* INCLUDED_sound_h_ */
