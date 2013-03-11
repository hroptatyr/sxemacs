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


/* Inspired by XEmacs' sound.c written by Jamie Zawinski */

/* Synched up with: Not in FSF. */

#include <config.h>
#include <time.h>
#include "lisp.h"

#include "syssignal.h"

#include "buffer.h"

#include "ui/device.h"
#include "ui/redisplay.h"
#include "sysdep.h"

#include "sysfile.h"
#include "opaque.h"
#include "semaphore.h"

#include "media.h"
#include "sound.h"

Fixnum bell_volume;
Fixnum bell_inhibit_time;
Lisp_Object Vsound_alist;
Lisp_Object Vsynchronous_sounds;
Lisp_Object Vnative_sound_only_on_console;
Lisp_Object Q_volume, Q_pitch, Q_duration, Q_sound;
Lisp_Object Q_device, Q_server, Q_client, Q_keep_open;
Lisp_Object Qplay_sound;

#ifdef HAVE_AO_SOUND
#include "sound-ao.h"
#endif
#ifdef HAVE_POLYP_SOUND
#include "sound-polyp.h"
#endif
#ifdef HAVE_PULSE_SOUND
#include "sound-pulse.h"
#endif
#ifdef HAVE_ESD_SOUND
#include "sound-esd.h"
#endif
#ifdef HAVE_NAS_SOUND
#include "sound-nas.h"
#endif
#ifdef HAVE_JACK_SOUND
#include "sound-jack.h"
#endif
#ifdef HAVE_ALSA_SOUND
#include "sound-alsa.h"
#endif
#ifdef HAVE_OSS_SOUND
#include "sysproc.h"
#include "sound-oss.h"
#endif

/* for CHECK_NUMBER and COMPARABLEP */
#include "ent/ent.h"

Lisp_Object Qaudio_devicep;
Lisp_Object Qaudio_jobp;
Lisp_Object Vdefault_audio_device;

static audio_job_t make_audio_job(Lisp_Object, Lisp_Object, Lisp_Object);
static Lisp_Object make_audio_asyneq_job(audio_job_t);
static inline void finish_audio_job_data(audio_job_t);
static inline void
exec_sentinel(void *job, Lisp_Object, Lisp_Object, Lisp_Object);

#ifdef EF_USE_ASYNEQ
#include "events/worker-asyneq.h"

/*****************************************************************/
/*			Audio Jobs				 */
/*****************************************************************/
/* sound-mst handler */

static void
mark_audio_job(worker_job_t job)
{
	audio_job_t aj;

	aj = audio_job(job);
	if (aj == NULL) {
		return;
	}

	SOUND_DEBUG_AJ("Marking audio job 0x%lx (job 0x%lx)\n",
		       (long unsigned int)aj,
		       (long unsigned int)job);
	SXE_MUTEX_LOCK(&aj->mtx);
	mark_object(aj->stream);
	mark_object(aj->device);
	mark_object(aj->result);
	mark_object(aj->sentinel);
	SXE_MUTEX_UNLOCK(&aj->mtx);
	return;
}

static void
print_audio_job(worker_job_t job, Lisp_Object pcf)
{
	audio_job_t aj = audio_job(job);
	SXE_MUTEX_LOCK(&aj->mtx);
	write_fmt_string(pcf, " carrying  #<audio-job 0x%lx>", (long unsigned int)aj);
	SXE_MUTEX_UNLOCK(&aj->mtx);
	return;
}

static void
finish_audio_job(worker_job_t job)
{
	audio_job_t aj;

	lock_worker_job(job);
	aj = audio_job(job);

	SOUND_DEBUG_AJ("Finishing audio job 0x%lx (job 0x%lx)\n",
		       (long unsigned int)aj,
		       (long unsigned int)job);
	if (aj) {
		finish_audio_job_data(aj);
	}
	worker_job_data(job) = NULL;
	unlock_worker_job(job);
	return;
}

static void
audio_job_handle(worker_job_t job)
{
	/* thread-safe */
	/* usually called from aux threads */
	audio_job_t aj;
	Lisp_Object device;
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	Lisp_Object ljob = (Lisp_Object)job;
#endif	/* !BDWGC */
	int(*playfun)(audio_job_t);
	struct gcpro gcpro1;

	GCPRO1(ljob);
	lock_worker_job(job);
	aj = audio_job(job);
	SXE_MUTEX_LOCK(&aj->mtx);
	SOUND_DEBUG_AJ("inherit scratch buffer 0x%lx (sz=%ld)\n",
		       (long unsigned int)worker_job_buffer(job),
		       (long int)worker_job_buffer_alloc_size(job));
	aj->buffer = worker_job_buffer(job);
	aj->buffer_alloc_size = worker_job_buffer_alloc_size(job);
	device = audio_job_device(job);
	SOUND_DEBUG_MT("starting thread 0x%lx@0x%lx\n",
		       (long unsigned int)aj,
		       (long unsigned int)job);
	aj->play_state = MTPSTATE_RUN;

	SOUND_DEBUG_AJ("fetching play function\n");
	playfun = XAUDIO_DEVICE(device)->meths->play;
	SOUND_DEBUG_AJ("fetched 0x%lx\n", (long unsigned int)playfun);

	SXE_MUTEX_UNLOCK(&aj->mtx);
	SOUND_DEBUG_AJ("calling play function\n");
	(void)playfun(aj);
	SOUND_DEBUG_AJ("play function finished\n");
	unlock_worker_job(job);
	UNGCPRO;
	return;
}

static void
audio_job_started(worker_job_t job)
{
	if (NILP(audio_job_sentinel(job) /* sentinel */)) {
		return;
	}
	/* called from main thread */
	exec_sentinel(job, audio_job_stream(job), intern("started"),
		      audio_job_sentinel(job));
	return;
}

static void
audio_job_finished(worker_job_t job)
{
	if (NILP(audio_job_sentinel(job) /* sentinel */)) {
		return;
	}
	/* called from main thread */
	exec_sentinel(job, audio_job_stream(job), intern("finished"),
		      audio_job_sentinel(job));
	return;
}

static struct work_handler_s audio_job_handler = {
	mark_audio_job, print_audio_job, finish_audio_job,
	audio_job_handle, audio_job_started, audio_job_finished
};

static Lisp_Object
make_audio_asyneq_job(audio_job_t aj)
{
	/* create a job digestible by the asyneq */
	Lisp_Object job = Qnil;
	struct gcpro gcpro1;
	worker_job_t j;

	GCPRO1(job);
	j = make_worker_job(&audio_job_handler);
	job = wrap_object(j);
	XWORKER_JOB_DATA(job) = aj;
	/* the scratch buffer thingie */
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->buffer = XWORKER_JOB_BUFFER(job);
	aj->buffer_alloc_size = XWORKER_JOB_BUFFER_ALLOC_SIZE(job);
	/* generate an event queue for job control */
	audio_job_queue(aj) = make_noseeum_event_queue();
	SXE_MUTEX_UNLOCK(&aj->mtx);
	UNGCPRO;
	return job;
}


DEFUN("set-audio-job-sentinel", Fset_audio_job_sentinel, 2, 2, 0, /*
Give JOB the sentinel SENTINEL; `nil' for none.
The sentinel is called as a function whenever the stream state changes.

The function should take three (optional four) arguments
  (JOB STREAM STATE &optional OLD-STATE)
where
- JOB is the worker job object currently coping with the stream,
- STREAM is bound to the stream object, and
- STATE is one of 'unknown, 'started, 'paused, 'stopped, 'finished
  and indicates the current state of the job
- OLD-STATE is again one of the above state symbols but indicates
  the previous state of the job.
*/
      (job, sentinel))
{
	CHECK_AUDIO_JOB(job);
	XAUDIO_JOB_SENTINEL(job) = sentinel;
	return sentinel;
}

DEFUN("play-media-stream&", Fplay_media_streamX,
      1, 4, 0, /*
Play the media stream STREAM on an audio device DEVICE.

Optional second argument DEVICE must be an audio device
created by `make-audio-device'.
If omitted DEVICE defaults to the value of `default-audio-device'.

Optional third argument SENTINEL specifies a lisp function to be
called whenever the stream state changes.  The function should
take three (optional four) arguments
  (JOB STREAM STATE &optional OLD-STATE)
where
- JOB is the worker job object currently coping with the stream,
- STREAM is bound to the stream object, and
- STATE is one of 'unknown, 'started, 'paused, 'stopped and indicates
  the current state of the job
- OLD-STATE is again one of the above state symbols but indicates
  the previous state of the job.

See also `set-media-thread-sentinel'.

Optional fourth argument VOLUME specifies an intial value for
the playback volume.
*/
      (stream, device, sentinel, volume))
{
	audio_job_t aj;
	Lisp_Object job = Qnil;
	int vol;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	CHECK_MEDIA_STREAM(stream);

	if (NILP(device))
		device = Vdefault_audio_device;
	else
		CHECK_AUDIO_DEVICE(device);

	/* hm, it's useful to stop here if default-audio-device is nil,
	 * i merely spit out a warning and return nil, that should suffice
	 */
	if (NILP(device)) {
		warn_when_safe(
			Qdevice, Qnotice,
			GETTEXT("play-media-stream: "
				"no device specified, "
				"consider setting `default-audio-device'."));
		return Qnil;
	}

	if (!NILP(volume)) {
		CHECK_NUMBER(volume);
		volume = Fcoerce_number(volume, Qint, Qnil);
		vol = XINT(volume);
		if (vol < MEDIA_SAMPLE_VOLUME_MIN)
			vol = 0;
		else if (vol > MEDIA_SAMPLE_VOLUME_MAX)
			vol = MEDIA_SAMPLE_VOLUME_MAX;
	} else
		vol = MEDIA_SAMPLE_VOLUME_NORM;

	GCPRO4(job, device, stream, sentinel);
	/* create the job data object */
	aj = make_audio_job(stream, device, sentinel);
	if (aj == Qnull_pointer) {
		UNGCPRO;
		return Qnil;
	}
	aj->volume = vol;
	aj->ratetrafo = 1.0;
	/* now prepare the job to dispatch */
	job = make_audio_asyneq_job(aj);
	/* add some props */
	/* ... and dispatch it */
	eq_enqueue(delegate_eq, job);
	/* brag about new jobs in the queue */
	eq_queue_trigger_all(delegate_eq);
	UNGCPRO;
	return job;
}
#endif	/* EF_USE_ASYNEQ */


/* media thread sentinels */
static Lisp_Object
exec_sentinel_unwind(Lisp_Object SXE_UNUSED(datum))
{
	return Qnil;
}

static inline void
exec_sentinel(void *job, Lisp_Object stream,
	      Lisp_Object state, Lisp_Object sentinel)
{
	/* This function can GC */
	/* called from main thread */
	Lisp_Object funcell[4] = {sentinel, (Lisp_Object)job, stream, state};
	int speccount = specpdl_depth();
	struct gcpro gcpro1;

	GCPROn(funcell, countof(funcell));

	record_unwind_protect(exec_sentinel_unwind, Qnil);
	/* call the funcell */
	Ffuncall(countof(funcell), funcell);
	/* reset to previous state */
	restore_match_data();
	UNGCPRO;
	unbind_to(speccount, Qnil);
	return;
}

static inline audio_job_t
allocate_audio_job(void)
{
	audio_job_t aj = xnew(struct audio_job_s);
	SOUND_DEBUG_AJ("allocated: 0x%lx\n", (long unsigned int)aj);
	return aj;
}

static audio_job_t
make_audio_job(Lisp_Object stream, Lisp_Object device, Lisp_Object sentinel)
{
	audio_job_t aj = NULL;
	media_substream *mss;

	/* traverse the substreams, try to find the first audio stream */
	for (mss = XMEDIA_STREAM_FIRST(stream);
	     mss && media_substream_type(mss) != MTYPE_AUDIO;
	     mss = media_substream_next(mss));

	if (mss == NULL) {
		/* throw error */
		return Qnull_pointer;
	}

	aj = allocate_audio_job();
	aj->stream = stream;
	aj->device = device;
	aj->substream = mss;
	aj->result = Qnil;
	aj->sentinel = sentinel;

	aj->state = MTSTATE_UNKNOWN;
	aj->play_state = MTPSTATE_UNKNOWN;
	SXE_MUTEX_INIT(&aj->mtx);
#ifdef EF_USE_ASYNEQ
	audio_job_queue(aj) = NULL;
#endif

	aj->job_device_data = NULL;
	aj->job_stream_data = NULL;

	aj->buffer = NULL;
	aj->buffer_alloc_size = 0;

	SOUND_DEBUG_AJ("created: 0x%lx stream 0x%lx device 0x%lx sentinel 0x%lx\n",
		       (long unsigned int)aj, (long unsigned int)stream,
                       (long unsigned int)device, (long unsigned int) sentinel);
	return aj;
}

static inline void
finish_audio_job_data(audio_job_t aj)
{
	SOUND_DEBUG_AJ("finishing: 0x%lx\n", (long unsigned int)aj);
	SXE_MUTEX_LOCK(&aj->mtx);
	if (audio_job_device_data(aj)) {
		SOUND_DEBUG_AJ("audio-job device data still alive. Bug?\n");
		audio_job_device_data(aj) = NULL;
	}
	if (audio_job_stream_data(aj)) {
		SOUND_DEBUG_AJ("audio-job stream data still alive. Bug?\n");
		audio_job_stream_data(aj) = NULL;
	}

	if (aj->buffer) {
		SOUND_CRITICAL("strange, buffer is non-NULL: 0x%lx\n",
			       (long unsigned int)aj->buffer);
		/* xfree(aj->buffer); */
	}
	aj->buffer_alloc_size = 0;

#ifdef EF_USE_ASYNEQ
	if (audio_job_queue(aj)) {
		SOUND_DEBUG_AJ("finishing audio job queue\n");
		free_event_queue(audio_job_queue(aj));
	}
	audio_job_queue(aj) = (void*)0xB16B00B5;
#endif  /* EF_USE_ASYNEQ */

	aj->resolution = 0;
	aj->framesize = 0;
	aj->channels = 0;
	aj->volume = 0;
	SXE_MUTEX_UNLOCK(&aj->mtx);
	SXE_MUTEX_FINI(&aj->mtx);

	SOUND_DEBUG_AJ("finished: 0x%lx\n", (long unsigned int)aj);
	xfree(aj);
}

#if defined __GNUC__
static int
sound_DO_NOT_play_stream(audio_job_t mst)
	__attribute__((unused));
#endif
static int
sound_DO_NOT_play_stream(audio_job_t mst)
{
	/* just a dummy function */
	return 0;
}

DEFUN("play-media-stream-synchronously", Fplay_media_stream_synchronously,
      1, 4, 0, /*
Play the media stream STREAM on an audio device synchronously.
This function disregards the value of `synchronous-sounds',
instead streams will always be played in synchronous mode.

Optional second argument DEVICE must be an audio device
created by `make-audio-device'.
If omitted DEVICE defaults to the value of `default-audio-device'.

Optional third argument SENTINEL specifies a lisp function to be
called after the stream playback finishes.  The function should
take one argument (STREAM) where STREAM is bound to the
media stream which finished.  See `set-media-thread-sentinel'.

Optional fourth argument VOLUME specifies an intial value for
the playback volume.
*/
      (stream, device, sentinel, volume))
{
	audio_job_t aj;
	int vol;

	CHECK_MEDIA_STREAM(stream);

	if (NILP(device))
		device = Vdefault_audio_device;
	else
		CHECK_AUDIO_DEVICE(device);

	/* hm, it's useful to stop here if default-audio-device is nil,
	 * i merely spit out a warning and return nil, that should suffice
	 */
	if (NILP(device)) {
		warn_when_safe(
			Qdevice, Qnotice,
			GETTEXT("play-media-stream: "
				"no device specified, "
				"consider setting `default-audio-device'."));
		return Qnil;
	}

	if (!NILP(volume)) {
		CHECK_NUMBER(volume);
		volume = Fcoerce_number(volume, Qint, Qnil);
		vol = XINT(volume);
		if (vol < MEDIA_SAMPLE_VOLUME_MIN)
			vol = 0;
		else if (vol > MEDIA_SAMPLE_VOLUME_MAX)
			vol = MEDIA_SAMPLE_VOLUME_MAX;
	} else {
		vol = MEDIA_SAMPLE_VOLUME_NORM;
	}

	aj = make_audio_job(stream, device, Qnil);
	if (aj == Qnull_pointer) {
		SOUND_DEBUG_AJ("audio job is void ... cancelling play\n");
		return Qnil;
	}
	aj->volume = vol;
	aj->ratetrafo = 1.0;
	aj->play_state = MTPSTATE_RUN;

#if defined EF_USE_ASYNEQ
	aj->queue = NULL;
#endif

	SOUND_DEBUG_AJ("sync calling play meth\n");
	XAUDIO_DEVICE(device)->meths->play(aj);

	if (!NILP(sentinel)) {
		exec_sentinel(
			(void*)Qnil, stream, intern("finished"), sentinel);
	}

	finish_audio_job_data(aj);

	return Qt;
}


#ifdef EF_USE_ASYNEQ
struct audio_job_event_s pause_event = {aj_change_state, {aj_pause}, 0};
struct audio_job_event_s resume_event = {aj_change_state, {aj_resume}, 0};
struct audio_job_event_s start_event = {aj_change_state, {aj_start}, 0};
struct audio_job_event_s stop_event = {aj_change_state, {aj_stop}, 0};
struct audio_job_event_s volnorm_event = {
	aj_change_volume, {MEDIA_SAMPLE_VOLUME_NORM}, 0};
struct audio_job_event_s volmute_event = {
	aj_change_volume, {MEDIA_SAMPLE_VOLUME_MIN}, 0};

DEFUN("pause-audio-job", Fpause_audio_job, 1, 1, 0, /*
Pause the audio job JOB.
Optionally JOB can be 'all in which case all running
media threads are paused.
*/
      (job))
{
	if (!EQ(job, Qall)) {
		CHECK_AUDIO_JOB(job);
		/* connect to job's queue and place a PAUSE event there */
		if (XAUDIO_JOB_QUEUE(job)) {
			eq_noseeum_enqueue(XAUDIO_JOB_QUEUE(job), &pause_event);
		}
	}
	return Qt;
}

DEFUN("resume-audio-job", Fresume_audio_job, 1, 1, 0, /*
Resume a paused audio job JOB.
Optionally JOB can be 'all in which case all paused
media threads are resumed.
*/
      (job))
{
	if (!EQ(job, Qall)) {
		CHECK_AUDIO_JOB(job);
		/* connect to job's queue and place a RESUME event there */
		if (XAUDIO_JOB_QUEUE(job)) {
			eq_noseeum_enqueue(XAUDIO_JOB_QUEUE(job),
					   &resume_event);
		}
	}
	return Qt;
}

DEFUN("stop-audio-job", Fstop_audio_job, 1, 1, 0, /*
Stop a audio job JOB.
Optionally JOB can be 'all in which case all media threads
are stopped.
*/
      (job))
{
	if (!EQ(job, Qall)) {
		CHECK_AUDIO_JOB(job);
		/* connect to job's queue and place a STOP event there */
		if (XAUDIO_JOB_QUEUE(job)) {
			eq_noseeum_enqueue(XAUDIO_JOB_QUEUE(job), &stop_event);
		}
	}
	return Qt;
}

DEFUN("set-audio-job-volume", Fset_audio_job_volume, 1, 2, 0, /*
Set the volume of the audio job JOB to VOLUME.

JOB is assumed to be a media thread object with an audio substream.
Optionally JOB can be 'all in which case the volume change
applies to all (currently handled) media threads.

VOLUME is either a comparable number (see `comparablep') or
a vector of comparable numbers.
In the former case VOLUME sets the master volume of all channels.
In the latter case VOLUME sets the volumes channelwise.

Any volume value is coerced to an integer.
A volume of 128 is the norm.
A volume of 0 is muting the respective channels.
Volumes greater than 128 cause an amplification of the stream,
255 is the maximal volume value.  Note that clipping may occur.
*/
      (job, volume))
{
	int vol = 0;
	Lisp_Object tmpv = Qnil;

	CHECK_AUDIO_JOB(job);
	if (volume == Qt) {
		if (XAUDIO_JOB_QUEUE(job)) {
			eq_noseeum_enqueue(
				XAUDIO_JOB_QUEUE(job), &volnorm_event);
		}
		return make_int(MEDIA_SAMPLE_VOLUME_NORM);
	} else if (volume == Qnil) {
		if (XAUDIO_JOB_QUEUE(job)) {
			eq_noseeum_enqueue(
				XAUDIO_JOB_QUEUE(job), &volmute_event);
		}
		return make_int(MEDIA_SAMPLE_VOLUME_NORM);
	} else if (COMPARABLEP(volume)) {
		volume = Fcoerce_number(volume, Qint, Qnil);
		vol = XINT(volume);

		if (vol < 0)
			vol = MEDIA_SAMPLE_VOLUME_MIN;
		else if (vol > MEDIA_SAMPLE_VOLUME_MAX)
			vol = MEDIA_SAMPLE_VOLUME_MAX;
	} else if (VECTORP(volume)) {
		tmpv = XVECTOR_DATA(volume)[0];
		tmpv = Fcoerce_number(tmpv, Qint, Qnil);
		vol = XINT(tmpv);

		if (vol < 0)
			vol = MEDIA_SAMPLE_VOLUME_MIN;
		else if (vol > MEDIA_SAMPLE_VOLUME_MAX)
			vol = MEDIA_SAMPLE_VOLUME_MAX;
	} else {
		return wrong_type_argument(Qnumberp, volume);
	}

	/* place an VOLCHANGE event in job's queue */
	if (XAUDIO_JOB_QUEUE(job)) {
		audio_job_event_t aje = make_audio_job_event(aj_change_volume);
		audio_job_event_args(aje).volume_args = vol;
		eq_noseeum_enqueue(XAUDIO_JOB_QUEUE(job), aje);
	}
	return volume;
}

DEFUN("audio-job-volume", Faudio_job_volume, 1, 1, 0, /*
Return the current volume of audio job JOB.
*/
      (job))
{
	CHECK_AUDIO_JOB(job);

	return make_int(XAUDIO_JOB(job)->volume);
}

DEFUN("set-audio-job-rate", Fset_audio_job_rate, 1, 2, 0, /*
Set the rate of audio job JOB to RATE.

If RATE is `t' or `nil', reset the rate to 1.0.
*/
      (job, rate))
{
	float ratetrafo;

	CHECK_AUDIO_JOB(job);
	if (rate == Qt || rate == Qnil) {
		ratetrafo = 1.0;
	} else if (COMPARABLEP(rate)) {
		rate = Fcoerce_number(rate, Qfloat, Qnil);
		ratetrafo = XFLOAT_DATA(rate);

		if (ratetrafo <= 0.5)
			ratetrafo = 0.5;
		else if (ratetrafo > 2.0)
			ratetrafo = 2.0;
	} else {
		return wrong_type_argument(Qnumberp, rate);
	}

	/* place a rate change event in job's queue */
	if (XAUDIO_JOB_QUEUE(job)) {
		audio_job_event_t aje = make_audio_job_event(aj_change_rate);
		audio_job_event_args(aje).rate_args = ratetrafo;
		eq_noseeum_enqueue(XAUDIO_JOB_QUEUE(job), aje);
	}
	return rate;
}

DEFUN("audio-job-rate", Faudio_job_rate, 1, 1, 0, /*
Return the current rate of audio job JOB.
*/
      (job))
{
	CHECK_AUDIO_JOB(job);

	return make_float(XAUDIO_JOB(job)->ratetrafo);
}
#endif	/* EF_USE_ASYNEQ */


DEFUN("ding", Fding, 0, 3, 0,	/*
Beep, or flash the frame.
Also, unless an argument is given,
terminate any keyboard macro currently executing.
When called from lisp, the second argument is what sound to make, and
the third argument is the device to make it in (defaults to the selected
device), but may also be an audio device created by `make-audio-device'.
*/
      (arg, sound, device))
{
	static time_t last_bell_time;
	static struct device *last_bell_device;
	time_t now;
	struct device *d = decode_device(device);
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(arg, sound, device);

	/* XSETDEVICE(device, d); */
	now = time(0);

	if (NILP(arg) && !NILP(Vexecuting_macro))
		/* Stop executing a keyboard macro. */
		error
		    ("Keyboard macro terminated by a command ringing the bell");

	if (d == last_bell_device && now - last_bell_time < bell_inhibit_time) {
		return Qnil;
	} else if (!NILP(Vvisible_bell) && DEVMETH(d, flash, (d))) {
		;
	} else if (NILP(sound)) {
		DEVMETH(d, ring_bell, (d, bell_volume, -1, -1));
	} else if (!NILP(Ffboundp(Qplay_sound))) {
		call3(Qplay_sound, sound, Qnil, device);
	}

	last_bell_time = now;
	last_bell_device = d;
	RETURN_UNGCPRO(Qnil);
}


/* LEGACY */
DEFUN("device-sound-enabled-p", Fdevice_sound_enabled_p, 0, 1, 0,	/*
Return t if DEVICE is able to play sound.  Defaults to selected device.
*/
      (device))
{
	if (DEVICEP(device)) {
#ifdef HAVE_NAS_SOUND
		if (DEVICE_CONNECTED_TO_NAS_P(decode_device(device)))
			return Qt;
#endif
#ifdef HAVE_OSS_SOUND
		if (DEVICE_ON_CONSOLE_P(decode_device(device)))
			return Qt;
#endif
	}

	if (NILP(device))
		device = Vdefault_audio_device;

	if (!NILP(device) &&
	    AUDIO_DEVICEP(device) &&
	    XAUDIO_DEVICE_STATE(device) != ASTATE_DEAD)
		return Qt;

	return Qnil;
}

/* LEGACY */
DEFUN("wait-for-sounds", Fwait_for_sounds, 0, 1, 0,	/*
Wait for all sounds to finish playing on DEVICE.
*/
      (device))
{
	return Qnil;
}

/* LEGACY */
DEFUN("connected-to-nas-p", Fconnected_to_nas_p, 0, 1, 0,	/*
Return t if connected to NAS server for sounds on DEVICE.
*/
      (device))
{
#ifdef HAVE_NAS_SOUND
	return DEVICE_CONNECTED_TO_NAS_P(decode_device(device)) ? Qt : Qnil;
#else
	return Qnil;
#endif
}


/*****************************************************************/
/*			audio device hack			 */
/*****************************************************************/
/* Indeed the console->device->frame->window structure is not what I'd call
 * applicable to audio devices. That is why this seamless fake here exists :)
 * -hroptatyr
 */
static Lisp_Object
audio_device_mark(Lisp_Object obj)
{
	if (XAUDIO_DEVICE_METH(obj, mark))
		return XAUDIO_DEVICE_METH(obj, mark)(
			XAUDIO_DEVICE_DATA(obj));

	return Qnil;
}

static void
audio_device_finalise(void *header, int for_disksave)
{
	Lisp_Audio_Device *ad = (Lisp_Audio_Device*)header;

	SOUND_DEBUG_DEV("GCor asked me to finalise: 0x%lx\n",
			(long unsigned int)ad);

	if ( ad == NULL )
		return;

	if (audio_device_data(ad) &&
	    audio_device_meth(ad, finish))
		audio_device_meth(ad, finish)(audio_device_data(ad));

	if (audio_device_data(ad))
		xfree(audio_device_data(ad));
	audio_device_data(ad) = NULL;

	/* avoid some warning */
	if (for_disksave);
}

static void
audio_device_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	write_c_string("#<audio-device :type ", printcharfun);


	switch (XAUDIO_DEVICE_DRIVER(obj)) {
	case ADRIVER_OSS:
		write_c_string("oss", printcharfun);
		break;

	case ADRIVER_NAS:
		write_c_string("nas", printcharfun);
		break;

	case ADRIVER_ESD:
		write_c_string("esd", printcharfun);
		break;

	case ADRIVER_POLYP:
		write_c_string("polyp", printcharfun);
		break;

	case ADRIVER_PULSE:
		write_c_string("pulse", printcharfun);
		break;

	case ADRIVER_AO:
		write_c_string("ao", printcharfun);
		break;

	case ADRIVER_JACK:
		write_c_string("jack", printcharfun);
		break;

	case ADRIVER_ALSA:
		write_c_string("alsa", printcharfun);
		break;

	case NUMBER_OF_AUDIO_DRIVERS:
	case ADRIVER_UNKNOWN:
	default:
		write_c_string("unknown", printcharfun);
		break;
	}

	if (XAUDIO_DEVICE_METH(obj, print)) {
		XAUDIO_DEVICE_METH(obj, print)(obj, printcharfun, escapeflag);
	}

	/* info about the general state */
	write_c_string(" :device-state ", printcharfun);
	switch (XAUDIO_DEVICE_STATE(obj)) {
	case ASTATE_DEAD:
	case ASTATE_SUSPENDED:
		write_c_string("#dead", printcharfun);
		break;
	case ASTATE_ALIVE:
		write_c_string("#ready", printcharfun);
		break;

	case NUMBER_OF_AUDIO_STATES:
	case ASTATE_UNKNOWN:
	default:
		write_c_string("#unknown", printcharfun);
		break;
	}

	write_c_string(">", printcharfun);
}

static int
audio_device_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return Qnil;

	/* less warnings */
	if (depth || obj1 || obj2);
}

static unsigned long
audio_device_hash (Lisp_Object obj, int SXE_UNUSED(depth))
{
	return (unsigned long)obj;
	/* audio_device_hashcode(XAUDIO_DEVICE_DATA(obj)); */
}

static const struct lrecord_description audio_device_description[] = {
	{ XD_INT, offsetof(Lisp_Audio_Device, driver) },
	{ XD_INT, offsetof(Lisp_Audio_Device, state) },
	{ XD_OPAQUE_PTR, offsetof(Lisp_Audio_Device, device_data) },
	{ XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION("audio_device", audio_device,
			      audio_device_mark, audio_device_print,
			      audio_device_finalise,
			      audio_device_equal, audio_device_hash,
			      audio_device_description,
			      Lisp_Audio_Device);

static Lisp_Audio_Device *
audio_device_allocate(void)
{
	Lisp_Audio_Device *ad;

	ad = alloc_lcrecord_type(Lisp_Audio_Device, &lrecord_audio_device);
	return ad;
}

audio_driver decode_audio_type(Lisp_Object type)
{
	audio_driver ad = ADRIVER_UNKNOWN;

	if (0);
#ifdef HAVE_OSS_SOUND
	else if (EQ(type, Qoss))
		ad = ADRIVER_OSS;
#endif
#ifdef HAVE_NAS_SOUND
	else if (EQ(type, Qnas))
		ad = ADRIVER_NAS;
#endif
#ifdef HAVE_ESD_SOUND
	else if (EQ(type, Qesd))
		ad = ADRIVER_ESD;
#endif
#ifdef HAVE_POLYP_SOUND
	else if (EQ(type, Qpolyp))
		ad = ADRIVER_POLYP;
#endif
#ifdef HAVE_PULSE_SOUND
	else if (EQ(type, Qpulse))
		ad = ADRIVER_PULSE;
#endif
#ifdef HAVE_AO_SOUND
	else if (EQ(type, Qao))
		ad = ADRIVER_AO;
#endif
#ifdef HAVE_JACK_SOUND
	else if (EQ(type, Qjack))
		ad = ADRIVER_JACK;
#endif
#ifdef HAVE_ALSA_SOUND
	else if (EQ(type, Qalsa))
		ad = ADRIVER_ALSA;
#endif

	return ad;
}

audio_driver decode_audio_device(Lisp_Object device)
{
	struct device *d = NULL;
	audio_driver ad = ADRIVER_UNKNOWN;

	if (NILP(device) && !NILP(Vdefault_audio_device))
		device = Vdefault_audio_device;

	if (AUDIO_DEVICEP(device)) {
		ad = XAUDIO_DEVICE_DRIVER(device);

	} else if (DEVICEP(device) || NILP(device)) {
		d = decode_device(device);

		if (0);
#ifdef HAVE_NAS_SOUND
		else if (DEVICE_CONNECTED_TO_NAS_P(d))
			ad = ADRIVER_NAS;
#endif
#ifdef HAVE_AO_SOUND
		else if (DEVICE_CONNECTED_TO_AO_P(d))
			ad = ADRIVER_AO;
#endif
#ifdef HAVE_PULSE_SOUND
		else if (DEVICE_CONNECTED_TO_PULSE_P(d))
			ad = ADRIVER_PULSE;
#endif
#ifdef HAVE_POLYP_SOUND
		else if (DEVICE_CONNECTED_TO_POLYP_P(d))
			ad = ADRIVER_POLYP;
#endif
#ifdef HAVE_ESD_SOUND
		else if (DEVICE_CONNECTED_TO_ESD_P(d))
			ad = ADRIVER_ESD;
#endif
#ifdef HAVE_ALSA_SOUND
		else if (DEVICE_CONNECTED_TO_ALSA_P(d))
			ad = ADRIVER_ALSA;
#endif
#ifdef HAVE_OSS_SOUND
		else if (NILP(Vnative_sound_only_on_console) ||
			 DEVICE_ON_CONSOLE_P(d))
			ad = ADRIVER_OSS;
#endif
	}

	return ad;
}

void *get_audio_device_data(Lisp_Object device)
{
	if (AUDIO_DEVICEP(device))
		return XAUDIO_DEVICE_DATA(device);
	else
		return NULL;
}

Lisp_Audio_Device *get_audio_device(Lisp_Object device)
{
	if (AUDIO_DEVICEP(device))
		return XAUDIO_DEVICE(device);
	else
		return NULL;
}


static Lisp_Object
make_audio_device(Lisp_Object type)
{
	Lisp_Audio_Device *ad;
	Lisp_Object lad;

	CHECK_SYMBOL(type);

	ad = audio_device_allocate();
	audio_device_driver(ad) = decode_audio_type(type);
	audio_device_data(ad) = NULL;
	XSETAUDIO_DEVICE(lad, ad);
	XAUDIO_DEVICE_STATE(lad) = ASTATE_UNKNOWN;

	return lad;
}

DEFUN("make-audio-device", Fmake_audio_device, 1, MANY, 0,	/*
DRIVER &rest DEVICE-OPTIONS

Create a new device to output audio via DRIVER.
DRIVER should be a symbol out of 'oss, 'nas, 'esd, 'pulse,
'jack, 'alsa, or 'ao.

The rest arguments may be used to pass options to the selected
output driver. These should be `:keyword value' pairs.

Valid keywords for ALSA are:
:device - the name of the hardware interface (default: "default"),
  you may want to try "plughw:0,0" first
:keep-open - whether to exclusively reserve the device.
  Note this may prevent other applications from using the device.

Valid keywords for (deprecated) OSS are:
:device - the name of the hardware interface (default: "/dev/dsp")
:keep-open - whether to exclusively reserve the device.
  Note this may prevent other applications from using the device.

Valid keywords for ESD are:
:server - to use a distant ESD daemon (e.g. "my.machine.box:16001")
The default for ESD output is to use a locally running daemon and
to connect to it via unix domain sockets.

Valid keywords for Pulse are:
:server - the host name to connect to (default: "localhost")
:sink - the name of the sink to connect to (e.g. "output")
:source - the name of the source to record from (e.g. "mic_in")
:client - how to call the client on the server (default "SXEmacs")
:role - a one-word description of the "type" of media to be played
  on the server. It can be one of:
          "video" - for movie/video streams
          "music" - for music streams (like mp3's, oga's etc)
           "game" - for audio from games
          "event" - for event sounds (this is the default)
          "phone" - for VoIP and Instant Messaging audio
      "animation" - for animations
     "production" - for audio production applications
           "a11y" - for accessibility applications
:stream - how to call the stream on the server (e.g. "fancy-sound")
:immediate - connect to sink immediately and keep the connection
  alive as long as the audio device exists (default `t')
:threaded - initiate a threaded mainloop (default `t')
:force - if non-nil the device object is created even though the
  pulse mainloop could not be started; if `nil' any mainloop failure
  results in an error.  This can be useful if you want to have an
  audio device object although the server is not (yet) up or not
  (yet) accepting connections from you. (default `nil')

Valid keywords for Jack are:
:server - the jack server to connect to (default "default")
:client - how to call the client on the server (default "SXEmacs")

Valid keywords for AO are:
:driver - the name of the output driver (e.g. "alsa", "esd", etc.)
:options - a list of AO suboptions (see AO documentation)
The default for AO output is to pass nothing and entirely use the
system and user configuration files.

Valid keywords for NAS are:
:server - the NAS server to connect to.  This can be either:
  - an X display string like "localhost:0.0", the X display string
    the current frame is on can be obtained by the function
    `device-connection'
  - or a SXEmacs device name like "localhost-11-0" which can be
    obtained by `device-name'
  - or a SXEmacs device object, obtainable by `frame-device', like
    #<x-device on "localhost:11.0" 0xee4>
If the :server keyword is omitted SXEmacs tries to determine a
sensible default in this order:
  - use the frame device of the current frame
  - use the frame device of the initial frame
  - use the display specified in $AUDIOSERVER
  - use the display specified in $DISPLAY
  - try "localhost:0.0"

*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object ad, driver, device_options = Qnil;
	audio_driver dev_driver;
	ad_device_data *device_data = NULL;

	driver = args[0];

	CHECK_SYMBOL(driver);
	ad = make_audio_device(driver);

	dev_driver = XAUDIO_DEVICE_DRIVER(ad);

	switch (dev_driver) {
	case ADRIVER_NAS:
#ifdef HAVE_NAS_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_nas;
		break;
#endif
	case ADRIVER_ALSA:
#ifdef HAVE_ALSA_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_alsa;
		break;
#endif
	case ADRIVER_OSS:
#ifdef HAVE_OSS_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_oss;
		break;
#endif
	case ADRIVER_AO:
#ifdef HAVE_AO_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_ao;
		break;
#endif
	case ADRIVER_POLYP:
	case ADRIVER_PULSE:
#ifdef HAVE_PULSE_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_pulse;
		break;
#endif
	case ADRIVER_ESD:
#ifdef HAVE_ESD_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_esd;
		break;
#endif
	case ADRIVER_JACK:
#ifdef HAVE_JACK_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_jack;
		break;
#endif
	case ADRIVER_UNKNOWN:
	case NUMBER_OF_AUDIO_DRIVERS:
	default:
		return Qnil;
		break;
	}

	{
		Lisp_Object tmp = Flist(nargs, args);
		device_options = XCDR(tmp);
	}

	if (XAUDIO_DEVICE_METH(ad, create) &&
	    !(device_data = XAUDIO_DEVICE_METH(ad, create)(device_options))) {
		XAUDIO_DEVICE_STATE(ad) = ASTATE_DEAD;
	}
	XAUDIO_DEVICE_DATA(ad) = device_data;
	return ad;
}

DEFUN("audio-device-p", Faudio_device_p, 1, 1, 0, /*
Return non-nil if OBJECT is an audio-device.
*/
      (object))
{
	if (AUDIO_DEVICEP(object))
		return Qt;
	else
		return Qnil;
}

#if 0
DEFUN("delete-audio-device", Fdelete_audio_device, 1, 1, 0, /*
Deinitialise the audio device DEVICE and free its resources.
*/
      (device))
{
	CHECK_AUDIO_DEVICE(device);

	audio_device_finalise(XAUDIO_DEVICE(device), 0);
	return device;
}
#endif


void syms_of_sound(void)
{
	INIT_LRECORD_IMPLEMENTATION(audio_device);

	defkeyword(&Q_volume, ":volume");
	defkeyword(&Q_pitch, ":pitch");
	defkeyword(&Q_duration, ":duration");
	defkeyword(&Q_sound, ":sound");

	/* located in sound.el */
	defsymbol(&Qplay_sound, "play-sound");

#ifdef HAVE_NAS_SOUND
	defsymbol(&Qnas, "nas");
#endif
#ifdef HAVE_ESD_SOUND
	defsymbol(&Qesd, "esd");
#endif
#ifdef HAVE_POLYP_SOUND
	defsymbol(&Qpolyp, "polyp");
#endif
#ifdef HAVE_PULSE_SOUND
	defsymbol(&Qpulse, "pulse");
#endif
#ifdef HAVE_AO_SOUND
	defsymbol(&Qao, "ao");
#endif
#ifdef HAVE_ALSA_SOUND
	defsymbol(&Qalsa, "alsa");
#endif
#ifdef HAVE_JACK_SOUND
	defsymbol(&Qjack, "jack");
#endif
#ifdef HAVE_OSS_SOUND
	defsymbol(&Qoss, "oss");
#endif
	defsymbol(&Qaudio_devicep, "audio-device-p");
	defsymbol(&Qaudio_jobp, "audio-job-p");

	/* some more symbols */
	defsymbol(&Q_device, ":device");
	defsymbol(&Q_keep_open, ":keep-open");
	defsymbol(&Q_server, ":server");
	defsymbol(&Q_client, ":client");

	DEFSUBR(Fplay_media_stream_synchronously);
#ifdef EF_USE_ASYNEQ
	DEFSUBR(Fplay_media_streamX);
	DEFSUBR(Fset_audio_job_sentinel);
	DEFSUBR(Fpause_audio_job);
	DEFSUBR(Fresume_audio_job);
	DEFSUBR(Fstop_audio_job);
	DEFSUBR(Fset_audio_job_volume);
	DEFSUBR(Faudio_job_volume);
	DEFSUBR(Fset_audio_job_rate);
	DEFSUBR(Faudio_job_rate);
#endif

	DEFSUBR(Fding);
	DEFSUBR(Fwait_for_sounds);
	DEFSUBR(Fconnected_to_nas_p);
	DEFSUBR(Fdevice_sound_enabled_p);

	/* audio device fake */
	DEFSUBR(Fmake_audio_device);
	DEFSUBR(Faudio_device_p);
#if 0
	DEFSUBR(Fdelete_audio_device); /* too dangerous atm */
#endif
}

void vars_of_sound(void)
{
#ifdef HAVE_OSS_SOUND
#  if 0
	Fprovide(intern("native-sound")); /* for compatibility */
	/* transition time is over! */
#  endif
	Fprovide(intern("oss-sound"));
#endif
#ifdef HAVE_NAS_SOUND
	Fprovide(intern("nas-sound"));
#endif
#ifdef HAVE_ESD_SOUND
	Fprovide(intern("esd-sound"));
#endif
#ifdef HAVE_POLYP_SOUND
	Fprovide(intern("polyp-sound"));
#endif
#ifdef HAVE_PULSE_SOUND
	Fprovide(intern("pulse-sound"));
#endif
#ifdef HAVE_AO_SOUND
	Fprovide(intern("ao-sound"));
#endif
#ifdef HAVE_ALSA_SOUND
	Fprovide(intern("alsa-sound"));
#endif
	Fprovide(intern("audio"));

	DEFVAR_INT("bell-volume", &bell_volume	/*
*How loud to be, from 0 to 255, where 127 is the norm (100%).
Values above raise the volume and values below lower it.
						 */ );
	bell_volume = 63;

	DEFVAR_INT("bell-inhibit-time", &bell_inhibit_time	/*
*Don't ring the bell on the same device more than once within this many seconds.
								 */ );
	bell_inhibit_time = 0;

	DEFVAR_LISP("sound-alist", &Vsound_alist	/*
An alist associating names with sounds.
When `beep' or `ding' is called with one of the name symbols, the associated
sound will be generated instead of the standard beep.

Each element of `sound-alist' is a list describing a sound.
The first element of the list is the name of the sound being defined.
Subsequent elements of the list are alternating keyword/value pairs:

Keyword: Value:
-------  -----
sound    A string of raw sound data (deprecated), or the name of another
	 sound to play.   The symbol `t' here means use the default X beep.
volume   An integer from 0-100, defaulting to `bell-volume'
pitch    If using the default X beep, the pitch (Hz) to generate.
duration If using the default X beep, the duration (milliseconds).
stream   A media stream object containing the sound.

You should probably add things to this list by calling the function
load-sound-file.

Note: SXEmacs must be built with sound support for your system.  Not all
systems support sound.
Note: The pitch, duration, and volume options are available everywhere,
but many X servers ignore the `pitch' option.

The following beep-types are used by SXEmacs itself:

auto-save-error  when an auto-save does not succeed
command-error    when the emacs command loop catches an error
undefined-key    when you type a key that is undefined
undefined-click  when you use an undefined mouse-click combination
no-completion    during completing-read
y-or-n-p         when you type something other than 'y' or 'n'
yes-or-no-p      when you type something other than 'yes' or 'no'
default          used when nothing else is appropriate.

Other lisp packages may use other beep types, but these are the ones that
the C kernel of Emacs uses.
							 */ );
	Vsound_alist = Qnil;

	DEFVAR_LISP("synchronous-sounds", &Vsynchronous_sounds	/*
Play sounds synchronously, if non-nil.
Only applies if SXEmacs has been compiled with a threading library.
Otherwise, sounds are always played synchronously.
								 */ );
	Vsynchronous_sounds = Qt;

	DEFVAR_LISP("native-sound-only-on-console", &Vnative_sound_only_on_console	/*
Non-nil value means play sounds only if SXEmacs is running
on the system console.
Nil means always play sounds, even if running on a non-console tty
or a secondary X display.

This variable only applies to native sound support.
											 */ );
	Vnative_sound_only_on_console = Qt;

	DEFVAR_LISP("default-audio-device", &Vdefault_audio_device	/*
Default audio device to use.
									 */ );
	Vdefault_audio_device = Qnil;
}

/* sound.c ends here */
