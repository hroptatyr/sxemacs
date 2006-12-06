/* New Generation Sound Functions.
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

/* Inspired by XEmacs' sound.c written by Jamie Zawinski */

/* Synched up with: Not in FSF. */

#include <config.h>
#include <time.h>
#include "lisp.h"

#include "syssignal.h"

#include "buffer.h"
#ifdef HAVE_X_WINDOWS
#include "console-x.h"
#endif

#include "device.h"
#include "redisplay.h"
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
#ifdef HAVE_ARTS_SOUND
#include "sound-arts.h"
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

Lisp_Object Qaudio_devicep;
Lisp_Object Qmedia_threadp;
Lisp_Object Vdefault_audio_device;

#if defined(HAVE_THREADS)
#include "emodptr.h"
#ifndef USLEEP_MAGIC
#define USLEEP_MAGIC(args...)	usleep(5000)
#endif

typedef struct {
	sxe_thread_t thread;
	sxe_mutex_t privmutex;
	Lisp_Object handle;
} sxe_media_worker_t;

typedef struct {
	/* the array of workers */
	sxe_media_worker_t *media_worker;
	sxe_mutex_t media_worker_mutex;
	int worker_counter;
	sxe_mutex_t media_worker_counter_mutex;
	int num_media_workers;

	/* the pre-queue */
	sxe_semaphore_t media_dequeue_smph;
	Lisp_Dllist *media_queue;
} sxe_media_workers_t;

static sxe_media_workers_t media_workers;
static Fixnum num_media_workers;
Lisp_Object Vmedia_workers;
#endif	/* HAVE_THREADS */

static Lisp_Media_Thread *finish_media_thread(Lisp_Media_Thread*);
static media_subthread *finish_media_subthread(media_subthread*);

static void exec_sentinel(Lisp_Object);


/* media thread sentinels */
static Lisp_Object exec_sentinel_unwind(Lisp_Object datum)
{
	Lisp_Cons *d = XCONS(datum);
	/* dont restore the sentinel */
	/* XMEDIA_THREAD(d->car)->sentinel = d->cdr; */
	free_cons(d);
	return Qnil;
}

static void exec_sentinel(Lisp_Object mthread)
{
	/* This function can GC */
	int speccount = specpdl_depth();
	Lisp_Media_Thread *mt = XMEDIA_THREAD(mthread);
	Lisp_Object sentinel = mt->sentinel;

	if (NILP(sentinel))
		return;

	/* Zilch the sentinel while it's running, to avoid recursive
	   invocations. */
	mt->sentinel = Qnil;
	record_unwind_protect(exec_sentinel_unwind,
			      noseeum_cons(mthread, sentinel));
	/* We used to bind inhibit-quit to t here, but call2_trapping_errors()
	   does that for us. */
	running_asynch_code = 1;
	call1_trapping_errors("Error in media-thread sentinel",
			      sentinel, mthread);
	running_asynch_code = 0;
	restore_match_data();

	unbind_to(speccount, Qnil);
}

DEFUN("set-media-thread-sentinel", Fset_media_thread_sentinel, 2, 2, 0, /*
Give MEDIA-THREAD the sentinel SENTINEL; `nil' for none.
The sentinel is called as a function when the stream finishes.
The function should take two arguments (STREAM STATE) where 
STREAM is bound to the media stream which finished and STATE
is a symbol (currently the only valid symbol is 'finished).
									*/
      (media_thread, sentinel))
{
	CHECK_MEDIA_THREAD(media_thread);
	XMEDIA_THREAD(media_thread)->sentinel = sentinel;
	return sentinel;
}


static int
sound_DO_NOT_play_stream(media_subthread *mst)
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
	Lisp_Object media_thread;
	Lisp_Media_Thread *mt;
	media_subthread *mst;
	int vol;

	/* this function can GC */
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
	GCPRO4(stream, device, sentinel, media_thread);

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

	media_thread = make_media_thread(stream, device);
	mt = XMEDIA_THREAD(media_thread);
	mt->play_state = MTPSTATE_RUN;
	mt->volume = vol;
	mt->ratetrafo = 1.0;

	/* hm, what to play in the synchronous case? :\ */
	/* i've decided to traverse all subthreads, if they're of type audio,
	   then fine, play'em, if not just return nil. */
	mst = mt->first;
	while (mst) {
		SXE_MUTEX_LOCK(&mst->mtx);
		if (media_substream_type(mst->substream) == MTYPE_AUDIO &&
		    (XAUDIO_DEVICE(device)->meths->play)(mst)) {
			SXE_MUTEX_UNLOCK(&mst->mtx);
			UNGCPRO;
			break;
		}
		SXE_MUTEX_UNLOCK(&mst->mtx);
		QUIT;
		mst = mst->next;
	}

	UNGCPRO;
	if (!NILP(mt->sentinel)) {
		Fset_media_thread_sentinel(wrap_media_thread(mt), sentinel);
		exec_sentinel(wrap_media_thread(mt));
	}

	finish_media_thread(mt);

	return Qt;
}

#ifdef HAVE_THREADS
static void sound_th_blksig()
{
        EMACS_BLOCK_SIGNAL(SIGINT);	/* ANSI */
        EMACS_BLOCK_SIGNAL(SIGILL);	/* ANSI */
        EMACS_BLOCK_SIGNAL(SIGABRT);	/* ANSI */
        EMACS_BLOCK_SIGNAL(SIGFPE);	/* ANSI */
        EMACS_BLOCK_SIGNAL(SIGSEGV);	/* ANSI */
        EMACS_BLOCK_SIGNAL(SIGTERM);	/* ANSI */
        
#ifdef SIGHUP
        EMACS_BLOCK_SIGNAL(SIGHUP);	/* POSIX */
#endif
#ifdef SIGQUIT
        EMACS_BLOCK_SIGNAL(SIGQUIT);	/* POSIX */
#endif
#ifdef SIGTRAP
        EMACS_BLOCK_SIGNAL(SIGTRAP);	/* POSIX */
#endif
#ifdef SIGUSR1
        EMACS_BLOCK_SIGNAL(SIGUSR1);	/* POSIX */
#endif
#ifdef SIGUSR2
        EMACS_BLOCK_SIGNAL(SIGUSR2);	/* POSIX */
#endif
#ifdef SIGPIPE
        EMACS_BLOCK_SIGNAL(SIGPIPE);	/* POSIX */
#endif
#ifdef SIGALRM
        EMACS_BLOCK_SIGNAL(SIGALRM);	/* POSIX */
#endif
#ifdef SIGCHLD
        EMACS_BLOCK_SIGNAL(SIGCHLD);	/* POSIX */
#endif
#ifdef SIGCONT
        EMACS_BLOCK_SIGNAL(SIGCONT);	/* POSIX */
#endif
#ifdef SIGSTOP
        EMACS_BLOCK_SIGNAL(SIGSTOP);	/* POSIX */
#endif
#ifdef SIGTSTP
        EMACS_BLOCK_SIGNAL(SIGTSTP);	/* POSIX */
#endif
#ifdef SIGTTIN
        EMACS_BLOCK_SIGNAL(SIGTTIN);	/* POSIX */
#endif
#ifdef SIGTTOU
        EMACS_BLOCK_SIGNAL(SIGTTOU);	/* POSIX */
#endif

#ifdef SIGBUS
        EMACS_BLOCK_SIGNAL(SIGBUS);	/* XPG5 */
#endif
#ifdef SIGPOLL
        EMACS_BLOCK_SIGNAL(SIGPOLL);	/* XPG5 */
#endif
#ifdef SIGPROF
        EMACS_BLOCK_SIGNAL(SIGPROF);	/* XPG5 */
#endif
#ifdef SIGSYS
        EMACS_BLOCK_SIGNAL(SIGSYS);	/* XPG5 */
#endif
#ifdef SIGURG
        EMACS_BLOCK_SIGNAL(SIGURG);	/* XPG5 */
#endif
#ifdef SIGXCPU
        EMACS_BLOCK_SIGNAL(SIGXCPU);	/* XPG5 */
#endif
#ifdef SIGXFSZ
        EMACS_BLOCK_SIGNAL(SIGXFSZ);	/* XPG5 */
#endif
#ifdef SIGVTALRM
        EMACS_BLOCK_SIGNAL(SIGVTALRM);	/* XPG5 */
#endif

#ifdef SIGIO
        EMACS_BLOCK_SIGNAL(SIGIO);	/* BSD 4.2 */
#endif
#ifdef SIGWINCH
        EMACS_BLOCK_SIGNAL(SIGWINCH);	/* BSD 4.3 */
#endif

#ifdef SIGEMT
        EMACS_BLOCK_SIGNAL(SIGEMT);
#endif
#ifdef SIGINFO
        EMACS_BLOCK_SIGNAL(SIGINFO);
#endif
#ifdef SIGHWE
        EMACS_BLOCK_SIGNAL(SIGHWE);
#endif
#ifdef SIGPRE
        EMACS_BLOCK_SIGNAL(SIGPRE);
#endif
#ifdef SIGUME
        EMACS_BLOCK_SIGNAL(SIGUME);
#endif
#ifdef SIGDLK
        EMACS_BLOCK_SIGNAL(SIGDLK);
#endif
#ifdef SIGCPULIM
        EMACS_BLOCK_SIGNAL(SIGCPULIM);
#endif
#ifdef SIGIOT
        EMACS_BLOCK_SIGNAL(SIGIOT);
#endif
#ifdef SIGLOST
        EMACS_BLOCK_SIGNAL(SIGLOST);
#endif
#ifdef SIGSTKFLT
        EMACS_BLOCK_SIGNAL(SIGSTKFLT);
#endif
#ifdef SIGUNUSED
        EMACS_BLOCK_SIGNAL(SIGUNUSED);
#endif
#ifdef SIGDANGER
        EMACS_BLOCK_SIGNAL(SIGDANGER);	/* AIX */
#endif
#ifdef SIGMSG
        EMACS_BLOCK_SIGNAL(SIGMSG);
#endif
#ifdef SIGSOUND
        EMACS_BLOCK_SIGNAL(SIGSOUND);
#endif
#ifdef SIGRETRACT
        EMACS_BLOCK_SIGNAL(SIGRETRACT);
#endif
#ifdef SIGGRANT
        EMACS_BLOCK_SIGNAL(SIGGRANT);
#endif
#ifdef SIGPWR
        EMACS_BLOCK_SIGNAL(SIGPWR);
#endif

}

extern void enqueue_magic_eval_event(void(*)(Lisp_Object), Lisp_Object);

static void *
sound_queue_th(void *queue_id)
{
	int qid = (int)queue_id;
	media_subthread *mst = NULL;
	Lisp_Object lmt = Qnil;
	Lisp_Media_Thread *mt = NULL;

        sound_th_blksig();

	while (1) {
		SXE_SEMAPH_SYNCH(&media_workers.media_dequeue_smph);

		SXE_LOCK_GC_MUTEX;
		SOUND_DEBUG_PT("dequeuing thread: 0x%x\n",
			       (unsigned int)pthread_self());

		SXE_MUTEX_LOCK(&media_workers.media_worker_mutex);
		if (media_workers.media_queue == NULL) {
			/* awww ... we gotta exit :( */
			SXE_MUTEX_UNLOCK(&media_workers.media_worker_mutex);
			SXE_UNLOCK_GC_MUTEX;
			pthread_exit(NULL);
			break;
		}

		SOUND_DEBUG_MW_DEQ("stack had %d elements\n",
				   (unsigned int)NULL,
				   dllist_size(media_workers.media_queue));
		/* we need noseeum_dllist_* funs */
		mst = (void*)dllist_pop_car(media_workers.media_queue);
		SOUND_DEBUG_MW_DEQ("stack has %d elements\n",
				   (unsigned int)mst,
				   dllist_size(media_workers.media_queue));
		SXE_MUTEX_UNLOCK(&media_workers.media_worker_mutex);

		/* SOUND_DEBUG("th %d handles.\n", qid); */
		/* mst->thread = qid; */

		if (mst == (void*)Qnil) {
			SOUND_CRITICAL("No thread on the queue. Uh oh.\n");
			continue;
		}

		/* wrap mst->up */
		mt = mst->up;
		lmt = wrap_media_thread(mt);

		SXE_MUTEX_LOCK(&media_workers.media_worker[qid].privmutex);
		SOUND_DEBUG_PT("escrowing handle 0x%x in queue %d.\n",
			       (unsigned int)mt, qid);
		media_workers.media_worker[qid].handle =
			wrap_media_thread(mt);

		SXE_UNLOCK_GC_MUTEX;
		SXE_WORKERS_SMPH_LOCK(mt);
		USLEEP_MAGIC();
		SXE_WORKERS_SMPH_SIGNAL(mt, SXE_SMPH_HAVE_STARTED);
		SXE_WORKERS_SMPH_WAIT(mt, SXE_SMPH_SHALL_START);
		SXE_WORKERS_SMPH_UNLOCK(mt);

		switch (media_substream_type(mst->substream)) {
		case MTYPE_AUDIO:
			SOUND_DEBUG_PT("locking private subthread mutex...");
			SXE_MUTEX_LOCK(&mst->mtx);
			SOUND_DEBUG_PT("done\n");
			SOUND_DEBUG_MW("announce successful start\n");
#if 1				/* use 0 to emulate Steve's 'puter */
			if (MEDIA_THREADP(lmt) &&
			    MEDIA_STREAMP(mst->substream->up)) {
				XAUDIO_DEVICE(mt->device)->meths->play(mst);
			}
#endif
			SOUND_DEBUG_PT("unlocking private subthread mutex...");
			SXE_MUTEX_UNLOCK(&mst->mtx);
			SOUND_DEBUG_PT("done\n");
			break;
		case MTYPE_VIDEO:
			SOUND_DEBUG("playing video.\n");
			SOUND_DEBUG_MW("announce successful start\n");
			break;
		default:
			break;
		}

		if (MEDIA_THREADP(lmt) &&
		    MEDIA_STREAMP(mst->substream->up) &&
		    !NILP(mt->sentinel) &&
		    !gc_in_progress) {
			struct gcpro ngcpro1;

			NGCPRO1(lmt);
			SXE_LOCK_GC_MUTEX;
			exec_sentinel(lmt);
			SXE_UNLOCK_GC_MUTEX;
			NUNGCPRO;
		}

		finish_media_thread(mt);
		SOUND_DEBUG_PT("flushing handle 0x%x in queue %d.\n",
			       (unsigned int)media_workers.
			       media_worker[qid].handle, qid);
		media_workers.media_worker[qid].handle = Qnil;

		SXE_MUTEX_UNLOCK(&media_workers.media_worker[qid].privmutex);

		SOUND_DEBUG_PT("enqueuing thread: 0x%x\n",
			       (unsigned int)pthread_self());
		WITH_SXE_MUTEX(&media_workers.media_worker_counter_mutex,
			       media_workers.worker_counter--);
	}

	return NULL;
}

static void
media_workers_mark(Lisp_Object obj)
{
	int i;
	SOUND_DEBUG_MW("MARK.\n");
	for (i=0; i < media_workers.num_media_workers; i++) {
		Lisp_Object obj = media_workers.media_worker[i].handle;
		if (!NILP(obj))
			SOUND_DEBUG_MW("marking %d: 0x%x\n",
				       i, (unsigned int)obj);
		mark_object(obj);
	}
	return;
}

DEFUN("init-asynchronousity", Finit_asynchronousity, 0, 0, 0, /*
Initialise queue listener threads.
The number of spawned worker threads can be controlled by
`number-of-media-workers'.
When called the complementary variable `synchronous-sounds' is
set to `nil'.
							       */
      ())
{
	pthread_attr_t attr;
	int i;

	/* cannot acquire a lock here, since the according mutex may
	 * be yet undefined
	 */
	if (EMODPTRP(Vmedia_workers) ||
	    num_media_workers <= 0)
		return Qnil;

	/* set synchronous-sounds to `nil', because initting asynchronous
	   sounds implies we want to use them, too */
	Vsynchronous_sounds = Qnil;

	media_workers.media_worker =
		xnew_array(sxe_media_worker_t, num_media_workers);
	media_workers.num_media_workers = num_media_workers;
	Vmedia_workers = make_emodptr(&media_workers);
	set_emodptr_marker(Vmedia_workers, media_workers_mark);

	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	SXE_MUTEX_INIT(&media_workers.media_worker_mutex);
	SXE_MUTEX_INIT(&media_workers.media_worker_counter_mutex);
	SXE_SEMAPH_INIT(&media_workers.media_dequeue_smph);

	for (i=0; i < media_workers.num_media_workers; i++) {
		SXE_MUTEX_INIT(&media_workers.media_worker[i].privmutex);
		pthread_create(&media_workers.media_worker[i].thread, &attr,
			       sound_queue_th, (void*)i);
		media_workers.media_worker[i].handle = Qnil;
	}

	WITH_SXE_MUTEX(&media_workers.media_worker_counter_mutex,
		       media_workers.worker_counter = 0);
	WITH_SXE_MUTEX(&media_workers.media_worker_mutex,
		       media_workers.media_queue = noseeum_make_dllist());

	pthread_attr_destroy(&attr);

	return Qt;
}

DEFUN("uninit-asynchronousity", Funinit_asynchronousity, 0, 0, 0, /*
Stop all queue listener threads.
Depending on whether there are busy threads this may block the
main execution loop until all worker threads are non-busy.
When called the complementary variable `synchronous-sounds' is
set to `t'.
								   */
      ())
{
	int i;

	/* cannot acquire lock on this one, since it may be dead already */
	if (!EMODPTRP(Vmedia_workers) ||
	    media_workers.media_queue == NULL ||
	    media_workers.num_media_workers <= 0)
		return Qnil;

	/* lock _all_ private mutexes, this means queue threads which still
	   play stuff will block the whole SXE */
	for (i = 0; i < media_workers.num_media_workers; i++)
		SXE_MUTEX_LOCK(&media_workers.media_worker[i].privmutex);

	WITH_SXE_MUTEX(&media_workers.media_worker_mutex,
		       noseeum_free_dllist(media_workers.media_queue);
		       media_workers.media_queue = NULL);

	/* now signal the stopping sequence to all threads */
	for (i = 0; i < media_workers.num_media_workers; i++) {
		SXE_SEMAPH_TRIGGER_ALL(&media_workers.media_dequeue_smph);
	}

	/* join the threads, gather their private mutexes and destroy them */
	for (i=0; i < media_workers.num_media_workers; i++) {
		pthread_join(media_workers.media_worker[i].thread, NULL);
		SXE_MUTEX_UNLOCK(&media_workers.media_worker[i].privmutex);
		SXE_MUTEX_FINI(&media_workers.media_worker[i].privmutex);
	}

	WITH_SXE_MUTEX(&media_workers.media_worker_counter_mutex,
		       media_workers.worker_counter = 0);

	/* gather the global mutexes and semaphores */
	SXE_SEMAPH_FINI(&media_workers.media_dequeue_smph);
	SXE_MUTEX_FINI(&media_workers.media_worker_mutex);
	SXE_MUTEX_FINI(&media_workers.media_worker_counter_mutex);

	/* free the mutex and sxe_thread_t arrays */
	set_emodptr(Vmedia_workers, NULL);
	Vmedia_workers = Qnil;

	/* reset synchronous-sounds to `t', because uninitting asynchronous
	   sounds implies we want to not use them anymore */
	Vsynchronous_sounds = Qt;

	return Qt;
}

static void
trigger_media_play(void)
{
/* signal one of the waiting threads to start playback */
	WITH_SXE_MUTEX(
		&media_workers.media_worker_mutex,
		if (media_workers.media_queue == NULL) {
			SOUND_CRITICAL("uh oh\n");
			return;
		}
		);

	WITH_SXE_MUTEX(&media_workers.media_worker_counter_mutex,
		       media_workers.worker_counter++);

	SXE_SEMAPH_TRIGGER(&media_workers.media_dequeue_smph);

	return;
}

static int
queue_has_free_slot_p(void)
{
	int result = 0;

	WITH_SXE_MUTEX(&media_workers.media_worker_counter_mutex,
		       result = media_workers.worker_counter);

	return (result < media_workers.num_media_workers);
}

static int
enqueue_stream(media_subthread *mst)
{
/* enqueue the substream within MST to the global media queue
 * iff there are free handler slots, i.e. a thread is waiting for 
 * unqueuing the thing
 */
	if (!queue_has_free_slot_p()) {
		SOUND_DEBUG_MW("no free worker, flushing: 0x%x\n",
			       (unsigned int)mst);
		return 0;
	}

	WITH_SXE_MUTEX(
		&media_workers.media_worker_mutex,
		SOUND_DEBUG_MW_ENQ("stack had %d elements\n",
				   (unsigned int)mst,
				   dllist_size(media_workers.media_queue));
		/* fuck, we really need noseeum_dllist_* funs :| */
		dllist_append(media_workers.media_queue, mst);
		SOUND_DEBUG_MW_ENQ("stack now has %d elements\n",
				   (unsigned int)NULL,
				   dllist_size(media_workers.media_queue)));

	return 1;
}

DEFUN("play-media-stream-asynchronously", Fplay_media_stream_asynchronously,
      1, 4, 0, /*
Play the media stream STREAM on an audio device asynchronously.
Return a media-thread object which can be used to interact with
the worker thread which handles STREAM.
This function disregards the value of `synchronous-sounds',
instead streams will always be played in asynchronous mode,
provided the worker threads have been initialised.
See `init-asynchronousity'.

Optional second argument DEVICE must be an audio device created
by `make-audio-device'.
If omitted DEVICE defaults to the value of `default-audio-device'.

Optional third argument SENTINEL specifies a lisp function to be
called after the stream playback finishes.  The function should
take one argument (STREAM) where STREAM is bound to the
media stream which finished.  See `set-media-thread-sentinel'.

Optional fourth argument VOLUME specifies an initial value for
the playback volume.
	       */
      (stream, device, sentinel, volume))
{
	Lisp_Object lmt;
	Lisp_Media_Thread *mt;
	media_subthread *mst;
	/* pthread stuff */
	int thret = -1;
	int vol = 0;

	/* This function can GC */
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
	GCPRO4(stream, device, sentinel, lmt);

	CHECK_MEDIA_STREAM(stream);

	if (NILP(device))
		device = Vdefault_audio_device;
	else
		CHECK_AUDIO_DEVICE(device);

	if (media_workers.media_queue == NULL) {
		UNGCPRO;
		return Qnil;
	}

	/* hm, it's useful to stop here if default-audio-device is nil,
	 * i merely spit out a warning and return nil, that should suffice
	 */
	if (NILP(device)) {
		warn_when_safe(
			Qdevice, Qnotice,
			GETTEXT("play-media-stream: "
				"no device specified, "
				"consider setting `default-audio-device'."));
		UNGCPRO;
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

	if (!queue_has_free_slot_p()) {
		SOUND_DEBUG_PT("no slave threads\n");
		warn_when_safe(
			Qdevice, Qnotice,
			GETTEXT("play-media-stream: "
				"all playback slots are busy, "
				"come back later."));
		UNGCPRO;
		return Qnil;
	}

	lmt = make_media_thread(stream, device);
	mt = XMEDIA_THREAD(lmt);
	mt->play_state = MTPSTATE_RUN;
	mt->volume = vol;
	mt->ratetrafo = 1.0;
	mst = mt->first;

	if (mst == NULL) {
		SOUND_DEBUG_MST("weird subthread state in thread: 0x%x\n",
				(unsigned int)mt);
		finish_media_thread(mt);
	}

	while (mst) {
		if (enqueue_stream(mst)) {
			WITH_SXE_WORKERS_SMPH_SYNCH(
				mt, SXE_SMPH_HAVE_STARTED,
				trigger_media_play();
				thret = 0);
		}

		mst = mst->next;
	}

	if (thret == 0) {
		mt->state = MTSTATE_RUNNING;
	}

	if (!NILP(sentinel))
		Fset_media_thread_sentinel(lmt, sentinel);

	USLEEP_MAGIC();
	SXE_WORKERS_SMPH_TRIGGER_ALL(mt, SXE_SMPH_SHALL_START);

	RETURN_UNGCPRO(lmt);
}

DEFUN("join-media-thread", Fjoin_media_thread, 1, 1, 0, /*
Join a media thread previously started by `play-media-thread-async'.
							 */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	return Qt;
}

DEFUN("pause-media-thread", Fpause_media_thread, 1, 1, 0, /*
Pause the media thread THREAD.
Optionally THREAD can be 'all in which case all running
media threads are paused.
							  */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	XMEDIA_THREAD(thread)->play_state = MTPSTATE_PAUSE;
	return Qt;
}

DEFUN("resume-media-thread", Fresume_media_thread, 1, 1, 0, /*
Resume a paused media thread THREAD.
Optionally THREAD can be 'all in which case all paused
media threads are resumed.
							    */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	XMEDIA_THREAD(thread)->play_state = MTPSTATE_RUN;
	return Qt;
}

DEFUN("stop-media-thread", Fstop_media_thread, 1, 1, 0, /*
Stop a media thread THREAD.
Optionally THREAD can be 'all in which case all media threads
are stopped.

Stopping THREAD will unleash the respective worker threads.
This is an irreversible action.
							*/
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	XMEDIA_THREAD(thread)->play_state = MTPSTATE_STOP;
	XMEDIA_THREAD(thread)->state = MTSTATE_FINISHED;
	return Fjoin_media_thread(thread);
}

DEFUN("media-thread-set-volume", Fmedia_thread_set_volume, 2, 2, 0, /*
Set the volume of the media thread THREAD to VOLUME.

THREAD is a media thread object with an audio substream.
Optionally THREAD can be 'all in which case the volume change
applies to all media threads.

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
      (thread, volume))
{
	int i, vol = 0;
	Lisp_Object tmpv = Qnil;

	CHECK_MEDIA_THREAD(thread);
	if (volume == Qt) {
		vol = MEDIA_SAMPLE_VOLUME_NORM;
	} else if (volume == Qnil) {
		vol = MEDIA_SAMPLE_VOLUME_MIN;
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

	XMEDIA_THREAD(thread)->volume = vol;
	for (i = 0; i < MEDIA_MAX_AUDIO_CHANNELS; i++)
		XMEDIA_THREAD(thread)->chanvol[i] = vol;

	return volume;
}

DEFUN("media-thread-volume", Fmedia_thread_volume, 1, 1, 0, /*
Return the current volume of media thread THREAD.
							    */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	return make_int(XMEDIA_THREAD(thread)->volume);
}

DEFUN("media-thread-set-rate", Fmedia_thread_set_rate, 2, 2, 0, /*
Set the rate of media thread THREAD to RATE.
								*/
      (thread, rate))
{
	float ratetrafo;

	CHECK_MEDIA_THREAD(thread);
	if (rate == Qt) {
		ratetrafo = 1.0;
	} else if (rate == Qnil) {
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

	XMEDIA_THREAD(thread)->ratetrafo = ratetrafo;

	return rate;
}

DEFUN("media-thread-rate", Fmedia_thread_rate, 1, 1, 0, /*
Return the current rate of media thread THREAD.
								*/
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	return make_float(XMEDIA_THREAD(thread)->ratetrafo);
}
#endif	/* HAVE_THREADS */

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

	if (d == last_bell_device && now - last_bell_time < bell_inhibit_time)
		return Qnil;
	else if (!NILP(Vvisible_bell) && DEVMETH(d, flash, (d))) ;
	else if (NILP(sound))
		DEVMETH(d, ring_bell, (d, bell_volume, -1, -1));
	else
		call3(Qplay_sound, sound, Qnil, device);

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
/* 			audio device hack			 */
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

	SOUND_DEBUG_DEV("GCor asked me to finalise: 0x%x\n",
			(unsigned int)ad);

	if (audio_device_data(ad) &&
	    audio_device_meth(ad, finish))
		audio_device_meth(ad, finish)(audio_device_data(ad));

	if (audio_device_data(ad))
		xfree(audio_device_data(ad));
	audio_device_data(ad) = NULL;

	/* avoid some warning */
	if (for_disksave || ad == NULL);
}

static void
audio_device_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	write_c_string("#<audio-device :type ", printcharfun);


	switch (XAUDIO_DEVICE_DRIVER(obj)) {
	case ADRIVER_OSS:
		write_c_string("oss", printcharfun);
		break;

	case ADRIVER_ARTS:
		write_c_string("arts", printcharfun);
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

	case ADRIVER_UNKNOWN:
	default:
		write_c_string("unknown", printcharfun);
		break;
	}

	if (XAUDIO_DEVICE_METH(obj, print))
		XAUDIO_DEVICE_METH(obj, print)(obj, printcharfun, escapeflag);

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
audio_device_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)obj;
	/* audio_device_hashcode(XAUDIO_DEVICE_DATA(obj)); */

	/* less warnings */
	if (depth);
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
#ifdef HAVE_ARTS_SOUND
	else if (EQ(type, Qarts))
		ad = ADRIVER_ARTS;
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
#ifdef HAVE_ARTS_SOUND
		else if (DEVICE_CONNECTED_TO_ARTS_P(d))
			ad = ADRIVER_ARTS;
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
'jack, 'alsa, 'arts or 'ao.

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
:stream - how to call the stream on the server (e.g. "fancy-sound")
:immediate - connect to sink immediately and keep the connection
  alive as long as the audio device exists (default `t')
:threaded - initiate a threaded mainloop (default `t')
:force - if non-`nil' the device object is created even though the
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

Valid keywords for aRts are:
none at the moment

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
	case ADRIVER_ARTS:
#ifdef HAVE_ARTS_SOUND
		XAUDIO_DEVICE_METHS(ad) = sound_arts;
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
	default:
		return Qnil;
		break;
	}

	device_options = XCDR(Flist(nargs, args));

	if (XAUDIO_DEVICE_METH(ad, create) &&
	    !(device_data = XAUDIO_DEVICE_METH(ad, create)(device_options)))
		XAUDIO_DEVICE_STATE(ad) = ASTATE_DEAD;

	XAUDIO_DEVICE_DATA(ad) = device_data;
	
	return ad;
}

DEFUN("audio-device-p", Faudio_device_p, 1, 1, 0, /*
Return non-`nil' if OBJECT is an audio-device, `nil' otherwise.
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


/*****************************************************************/
/* 			Media Threads				 */
/*****************************************************************/
/* Media Threads (used for example to play sounds asynchronously)
 */
static Lisp_Object
media_thread_mark(Lisp_Object obj)
{
	Lisp_Media_Thread *mt = XMEDIA_THREAD(obj);

	mark_object(mt->stream);
	mark_object(mt->device);
	mark_object(mt->result);
	mark_object(mt->sentinel);

	return Qnil;
}

static void
media_thread_finalise(void *header, int for_disksave)
{
	Lisp_Media_Thread *mt = (Lisp_Media_Thread*)header;

	SOUND_DEBUG_MT("GCor asked me to finalise: 0x%x\n",
		       (unsigned int)mt);

	if (mt->state == MTSTATE_FINISHED &&
	    mt->first == NULL &&
	    mt->last == NULL) {
		SXE_WORKERS_SMPH_FINI(mt);
		return;
	}

	/* .oO{better have pthreads around} */
#if PeanutHorst_WINS_NOBEL_PRIZE
	SOUND_DEBUG_PT("triggering the finalise semaphore with SHALL_FINISH "
		       "at 0x%x\n", (unsigned int)mt);
	SXE_WORKERS_SMPH_TRIGGER_ALL(mt, SXE_SMPH_SHALL_FINISH);
#endif
	SOUND_DEBUG_PT("waiting for finalise semaphore with HAVE_FINISHED "
		       "at 0x%x\n", (unsigned int)mt);
	WITH_SXE_WORKERS_SMPH_SYNCH(
		mt, SXE_SMPH_HAVE_FINISHED,
		mt->play_state = MTPSTATE_STOP);
	SXE_WORKERS_SMPH_FINI(mt);

	/* avoid some warning */
	if (for_disksave);
}

static void
media_subthread_print(media_subthread *mst,
		      Lisp_Object printcharfun, int escapeflag)
{
	char *str = alloca(64);

	snprintf(str, 63, " #<subthread 0x%x>", (unsigned int)mst->thread);

	write_c_string(str, printcharfun);
}

static void
media_thread_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Media_Thread *mt = XMEDIA_THREAD(obj);
	media_subthread *mst;

	write_c_string("#<media-thread ", printcharfun);

	switch (mt->state) {
	case MTSTATE_RUNNING:
		write_c_string(":running", printcharfun);
		break;
	case MTSTATE_FINISHED:
		write_c_string(":finished (result ", printcharfun);
		print_internal(mt->result, printcharfun, escapeflag);
		write_c_string(")", printcharfun);
		break;
	case MTSTATE_UNKNOWN:
	default:
		write_c_string(":unknown-state", printcharfun);
		break;
	}

	mst = media_thread_first_subthread(mt);
	while (mst) {
		media_subthread_print(mst, printcharfun, escapeflag);
		mst = media_subthread_next(mst);
	}

	write_c_string(">", printcharfun);
}

static int
media_thread_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return Qnil;

	/* less warnings */
	if (depth || obj1 || obj2);
}

static unsigned long
media_thread_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)obj;
	/* media_thread_hashcode(XMEDIA_THREAD_DATA(obj)); */

	/* less warnings */
	if (depth);
}

static const struct lrecord_description media_thread_description[] = {
	{ XD_LISP_OBJECT, offsetof(Lisp_Media_Thread, stream) },
	{ XD_LISP_OBJECT, offsetof(Lisp_Media_Thread, device) },
        { XD_INT, offsetof(Lisp_Media_Thread, state) },
        { XD_INT, offsetof(Lisp_Media_Thread, play_state) },
        { XD_LISP_OBJECT, offsetof(Lisp_Media_Thread, result) },
        { XD_LISP_OBJECT, offsetof(Lisp_Media_Thread, sentinel) },
        { XD_OPAQUE_PTR, offsetof(Lisp_Media_Thread, first) },
        { XD_OPAQUE_PTR, offsetof(Lisp_Media_Thread, last) },
	{ XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION("media_thread", media_thread,
			      media_thread_mark, media_thread_print,
			      media_thread_finalise,
			      media_thread_equal, media_thread_hash,
			      media_thread_description,
			      Lisp_Media_Thread);

static Lisp_Media_Thread *
media_thread_allocate(void)
{
	Lisp_Media_Thread *mt;

	mt = alloc_lcrecord_type(Lisp_Media_Thread, &lrecord_media_thread);
	return mt;
}


static media_subthread *
make_media_subthread(void)
{
	media_subthread *mst;

	mst = xnew_and_zero(media_subthread);
	mst->next = NULL;
	mst->prev = NULL;
	mst->up = NULL;
	mst->substream = NULL;
#if defined(HAVE_THREADS)
	mst->thread = pthread_self();
	SXE_MUTEX_INIT(&mst->mtx);
#endif

	SOUND_DEBUG_MST("created: 0x%x\n", (unsigned int)mst);

	return mst;
}

static media_subthread *
finish_media_subthread(media_subthread *mst)
{
#ifdef HAVE_THREADS
	SOUND_DEBUG_PT("Waiting for: 0x%x\n",
		       (unsigned int)mst->thread);
	SOUND_DEBUG_PT("f_m_s: locking private subthread mutex...");
	SXE_MUTEX_LOCK(&mst->mtx);
	SOUND_DEBUG_PT("f_m_s: done\n");
	SOUND_DEBUG_PT("f_m_s: unlocking private subthread mutex...");
	SXE_MUTEX_UNLOCK(&mst->mtx);
	SOUND_DEBUG_PT("f_m_s: done\n");
	SXE_MUTEX_FINI(&mst->mtx);
	mst->thread = 0;
#endif

	mst->up = NULL;
	mst->substream = NULL;
	mst->prev = NULL;
	mst->next = NULL;

	if (media_subthread_device_data(mst)) {
		SOUND_DEBUG_MST("subthread device data still alive. Bug?\n");
		media_subthread_device_data(mst) = NULL;
	}
	if (media_subthread_stream_data(mst)) {
		SOUND_DEBUG_MST("subthread stream data still alive. Bug?\n");
		media_subthread_stream_data(mst) = NULL;
	}

	if (mst->buffer) {
		SOUND_CRITICAL("strange, buffer is non-NULL: 0x%x\n",
			       (unsigned int)mst->buffer);
		/* xfree(mst->buffer); */
	}
	mst->buffer_alloc_size = 0;

	mst->resolution = 0;
	mst->framesize = 0;
	mst->channels = 0;
	mst->volume = 0;

	SOUND_DEBUG_MST("finished: 0x%x\n", (unsigned int)mst);

	return mst;
}

static media_subthread *
make_media_subthread_append(Lisp_Media_Thread *mt)
{
	media_subthread *mst;

	SOUND_DEBUG_MT("0x%x, creating subthread\n", (unsigned int)mt);

	mst = make_media_subthread();

	/* set next/prev */
	mst->next = NULL;
	if (!(mt->last)) {
		mst->prev = NULL;
		mt->first = mst;
	} else {
		mst->prev = mt->last;
		(mt->last)->next = mst;
	}

	mt->last = mst;
	mst->up = mt;

	SOUND_DEBUG_MST("appended: 0x%x\n", (unsigned int)mst);

	return mst;
}

static media_subthread *
make_media_subthread_prepend(Lisp_Media_Thread *mt)
{
	media_subthread *mst;

	SOUND_DEBUG_MT("0x%x, creating subthread\n", (unsigned int)mt);

	mst = make_media_subthread();

	/* set next/prev */
	mst->prev = NULL;
	if (!(mt->first)) {
		mst->next = NULL;
		mt->last = mst;
	} else {
		mst->next = mt->first;
		(mt->first)->prev = mst;
	}

	mt->first = mst;
	mst->up = mt;

	SOUND_DEBUG_MST("prepended: 0x%x\n", (unsigned int)mst);

	return mst;
}

Lisp_Object make_media_thread(Lisp_Object stream, Lisp_Object device)
{
	Lisp_Media_Thread *mt;
	Lisp_Object lmt;
	media_substream *mss;

	mt = media_thread_allocate();
	mt->stream = stream;
	mt->device = device;
	mt->state = MTSTATE_UNKNOWN;
	mt->result = Qnil;
	mt->sentinel = Qnil;
	/* navigation */
	mt->first = NULL;
	mt->last = NULL;

	/* traverse the substreams */
	mss = XMEDIA_STREAM_FIRST(stream);
	while (mss) {
		media_subthread *mst = make_media_subthread_append(mt);

		mst->substream = mss;
		mss = media_substream_next(mss);
	}

	SXE_WORKERS_SMPH_INIT(mt);

	XSETMEDIA_THREAD(lmt, mt);

	SOUND_DEBUG_MT("created: 0x%x\n", (unsigned int)mt);

	return lmt;
}

static Lisp_Media_Thread *
finish_media_thread(Lisp_Media_Thread *mt)
{
	media_subthread *mst = NULL, *tmpmst = NULL;

	if (!MEDIA_THREADP(wrap_object(mt))) {
		SOUND_CRITICAL("Strange voodoo happened: "
			       "Should finish a media-thread which is none: "
			       "mt:0x%x\n",
			       (unsigned int)mt);
		return NULL;
	}

	/* traverse the subthreads and finish them, too */
	/* what if some subthreads are still active?! :| */
	mst = media_thread_first_subthread(mt);
	while (mst) {
		tmpmst = mst;
		mst = media_subthread_next(tmpmst);
		finish_media_subthread(tmpmst);
		SOUND_DEBUG_MST("freeing: 0x%x\n", (unsigned int)tmpmst);
		xfree(tmpmst);
		SOUND_DEBUG_MST("freed: 0x%x\n", (unsigned int)tmpmst);
	}

	mt->stream = Qnil;
	mt->device = Qnil;
	mt->state = MTSTATE_FINISHED;

	mt->first = NULL;
	mt->last = NULL;

	SXE_WORKERS_SMPH_TRIGGER(mt, SXE_SMPH_HAVE_FINISHED);

	SOUND_DEBUG_MT("finished: 0x%x\n", (unsigned int)mt);

	return mt;
}


void syms_of_sound(void)
{
	INIT_LRECORD_IMPLEMENTATION(audio_device);
	INIT_LRECORD_IMPLEMENTATION(media_thread);

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
#ifdef HAVE_ARTS_SOUND
	defsymbol(&Qarts, "arts");
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
	defsymbol(&Qmedia_threadp, "media-thread-p");

/* some more symbols */
	defsymbol(&Q_device, ":device");
	defsymbol(&Q_keep_open, ":keep-open");
	defsymbol(&Q_server, ":server");
	defsymbol(&Q_client, ":client");

#ifdef HAVE_THREADS
	DEFSUBR(Finit_asynchronousity);
	DEFSUBR(Funinit_asynchronousity);
	DEFSUBR(Fplay_media_stream_asynchronously);
#endif
	DEFSUBR(Fplay_media_stream_synchronously);
	DEFSUBR(Fset_media_thread_sentinel);
	DEFSUBR(Fjoin_media_thread);
	DEFSUBR(Fpause_media_thread);
	DEFSUBR(Fresume_media_thread);
	DEFSUBR(Fstop_media_thread);
	DEFSUBR(Fmedia_thread_set_volume);
	DEFSUBR(Fmedia_thread_volume);
	DEFSUBR(Fmedia_thread_set_rate);
	DEFSUBR(Fmedia_thread_rate);
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
#ifdef HAVE_ARTS_SOUND
	Fprovide(intern("arts-sound"));
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

	DEFVAR_CONST_LISP("media-thread-workers", &Vmedia_workers	/*
Internal queue, DO NOT FIDDLE WITH IT!!!!
Oh, do fiddle with it if you can't get enough if these cute core files. :)
									*/ );
	Vmedia_workers = Qnil;

	DEFVAR_INT("number-of-media-queues", &num_media_workers /*
Number of worker threads spawned as queue listeners.
								*/ );
	DEFVAR_INT("number-of-media-workers", &num_media_workers /*
Number of worker threads spawned as queue listeners.
								 */ );
	num_media_workers = 4;
}
