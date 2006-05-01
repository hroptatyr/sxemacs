/* Sound functions.
   Copyright (C) 1992, 1993, 1994 Lucid Inc.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

/* Originally written by Jamie Zawinski.
   Hacked on quite a bit by various others. */

#include <config.h>
#include <time.h>
#include "lisp.h"

#ifdef HAVE_THREADS
#include <pthread.h>
#endif

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

#include "media.h"
#include "sound.h"

Fixnum bell_volume;
Fixnum bell_inhibit_time;
Lisp_Object Vsound_alist;
Lisp_Object Vsynchronous_sounds;
Lisp_Object Vnative_sound_only_on_console;
Lisp_Object Q_volume, Q_pitch, Q_duration, Q_sound;
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
#ifdef HAVE_ESD_SOUND
#include "sound-esd.h"
#endif
#ifdef HAVE_NAS_SOUND
#include "sound-nas.h"
extern char *nas_init_play(Display *);
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

#define SOUNDQNATIVE Qoss
#endif

Lisp_Object Qaudio_devicep;
Lisp_Object Qmedia_threadp;
Lisp_Object Vdefault_audio_device;

#ifdef HAVE_THREADS
#define MEDIA_THREADS_RUN_DETACHED 0
#define MEDIA_USE_QUEUE 1

#if MEDIA_USE_QUEUE
pthread_mutex_t media_queue_mutex;
pthread_mutex_t media_dequeue_mutex;
pthread_cond_t media_dequeue_cond;
/* the different threads listening to the queue */
pthread_t *media_queue;
pthread_mutex_t *media_queue_privmutex;
int queue_counter;
pthread_mutex_t media_queue_counter_mutex;
/* the queue itself */
Lisp_Object Vmedia_queue;
/* the number of spawned queue threads */
Lisp_Object Vnumber_of_media_queues;
int num_media_queues;
#endif

#if MEDIA_THREADS_RUN_DETACHED
static void media_thread_join(Lisp_Media_Thread*);
#else
static Lisp_Object media_thread_join(Lisp_Media_Thread*);
#endif
#endif	/* HAVE_THREADS */

static void exec_sentinel(Lisp_Object, Lisp_Object);



/* media thread sentinels */
static Lisp_Object exec_sentinel_unwind(Lisp_Object datum)
{
	Lisp_Cons *d = XCONS(datum);
	XMEDIA_THREAD(d->car)->sentinel = d->cdr;
	free_cons(d);
	return Qnil;
}

static void exec_sentinel(Lisp_Object mthread, Lisp_Object state)
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
	call2_trapping_errors("Error in media-thread sentinel",
			      sentinel, mthread, state);
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

typedef int(*play_stream_function)(media_subthread*);

static play_stream_function
sound_play_stream(Lisp_Media_Thread *mt)
{
	audio_driver ad;

	/* try to determine what the user gave us as DEVICE */
	ad = decode_audio_device(mt->device);

	switch (ad) {
	case ADRIVER_NAS:
#ifdef HAVE_NAS_SOUND
		return sound_nas_play_stream;
#endif	/* HAVE_NAS_SOUND */

	case ADRIVER_AO:
#ifdef HAVE_AO_SOUND
		return sound_ao_play_stream;
#endif	/* HAVE_AO_SOUND */

	case ADRIVER_POLYP:
#ifdef HAVE_POLYP_SOUND
		return sound_polyp_play_stream;
#endif	/* HAVE_POLYP_SOUND */

	case ADRIVER_ESD:
#ifdef HAVE_ESD_SOUND
		return sound_esd_play_stream;
#endif	/* HAVE_ESD_SOUND */

	case ADRIVER_ARTS:
#ifdef HAVE_ARTS_SOUND
		return sound_arts_play_stream;
#endif	/* HAVE_ARTS_SOUND */

	case ADRIVER_JACK:
#ifdef HAVE_JACK_SOUND
		return sound_jack_play_stream;
#endif	/* HAVE_JACK_SOUND */

	case ADRIVER_ALSA:
#ifdef HAVE_ALSA_SOUND
		return sound_alsa_play_stream;
#endif	/* HAVE_ALSA_SOUND */

	case ADRIVER_OSS:
#ifdef HAVE_OSS_SOUND
		return sound_oss_play_stream;
#endif	/* HAVE_OSS_SOUND */

	case ADRIVER_UNDECIDED:
	default:
		return sound_DO_NOT_play_stream;
	}
}

DEFUN("play-media-stream-synchronously", Fplay_media_stream_synchronously,
      1, 3, 0, /*
Play the media stream STREAM on DEVICE.

DEVICE must be an audio device created by `make-audio-device'.
DEVICE defaults to `default-audio-device'.

Optional third argument SENTINEL specifies a lisp function to be
called after the stream playback finishes.  The function should
take two arguments (STREAM STATE) where STREAM is bound to the
media stream which finished and STATE is a symbol (currently the
only valid symbol is 'finished).  See `set-media-thread-sentinel'.
	       */
      (stream, device, sentinel))
{
	Lisp_Object media_thread;
	Lisp_Media_Thread *mt;
	media_subthread *mst;

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
			Qdevice, Qerror,
			GETTEXT("play-media-stream: "
				"no device specified, "
				"consider setting `default-audio-device'."));
		return Qnil;
	}

	media_thread = make_media_thread(stream, device);
	mt = XMEDIA_THREAD(media_thread);
	mt->play_state = MTPSTATE_RUN;

	/* hm, what to play in the synchronous case? :\ */
	/* i've decided to traverse all subthreads, if they're of type audio,
	   then fine, play'em, if not just return nil. */
	mst = mt->first;
	while (mst) {
		if (media_substream_type(mst->substream) == MTYPE_AUDIO &&
		    sound_play_stream(mt)(mst)) {
			UNGCPRO;
			return Qt;
		}
		QUIT;
		mst = mst->next;
	}

	UNGCPRO;
	if (!NILP(mt->sentinel)) {
		Fset_media_thread_sentinel(wrap_media_thread(mt), sentinel);
		exec_sentinel(wrap_media_thread(mt), intern("finished"));
	}
	return Qt;
}

#ifdef HAVE_THREADS
static void *
sound_test_th(void *foo)
{
	fprintf(stderr, "\nthread entered\n");
	usleep(1000000);
	fprintf(stderr, "thread about to leave\n");
	return NULL;
}


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



#if MEDIA_USE_QUEUE

static void *
sound_queue_th(void *queue_id)
{
	int qid = (int)queue_id;
	Lisp_Object opqmst = Qnil;
	media_subthread *mst = NULL;
	struct gcpro gcpro1;

        sound_th_blksig();

	GCPRO1(opqmst);

	while (1) {
		pthread_mutex_lock(&media_dequeue_mutex);
		pthread_cond_wait(
			&media_dequeue_cond, &media_dequeue_mutex);
		pthread_mutex_unlock(&media_dequeue_mutex);

		pthread_mutex_lock(&media_queue_mutex);
		if (NILP(Vmedia_queue)) {
			/* awww ... we gotta exit :( */
			pthread_mutex_unlock(&media_queue_mutex);
			pthread_exit(NULL);
			break;
		}

		opqmst = dllist_pop_car(XDLLIST(Vmedia_queue));
		pthread_mutex_unlock(&media_queue_mutex);

		/* fprintf(stderr, "th %d handles.\n", qid); */
		/* mst->thread = qid; */

		if (NILP(opqmst))
			continue;
		else {
			mst = get_opaque_ptr(opqmst);
			free_opaque_ptr(opqmst);
		}

		pthread_mutex_lock(&media_queue_privmutex[qid]);

		switch (media_substream_type(mst->substream)) {
		case MTYPE_AUDIO:
			if (MEDIA_THREADP(mst->up) &&
			    MEDIA_STREAMP(mst->substream->up))
				sound_play_stream(mst->up)(mst);
			break;
		case MTYPE_VIDEO:
			fprintf(stderr, "playing video.\n");
			break;
		default:
			break;
		}

		if (MEDIA_THREADP(mst->up) &&
		    MEDIA_STREAMP(mst->substream->up) &&
		    !NILP(mst->up->sentinel))
			exec_sentinel(
				wrap_media_thread(mst->up), intern("finished"));

		pthread_mutex_unlock(&media_queue_privmutex[qid]);

		pthread_mutex_lock(&media_queue_counter_mutex);
		queue_counter--;
		pthread_mutex_unlock(&media_queue_counter_mutex);
	}

	UNGCPRO;
	return NULL;
}

#else  /* !MEDIA_USE_QUEUE */

static void *
sound_play_stream_th(media_subthread *mst)
{
	Lisp_Media_Thread *mt;
	int result = 0;

	mt = mst->up;

	result = sound_play_stream(mt)(mst);

	if (!NILP(mt->sentinel))
		exec_sentinel(wrap_media_thread(mt), intern("finished"));

#if MEDIA_THREADS_RUN_DETACHED
	pthread_exit(NULL);
#else
	if (result)
		pthread_exit((void*)Qt);
	else
		pthread_exit((void*)Qnil);
#endif

	/* just to make gcc smile */
	return NULL;
}


typedef void*(*media_pthread_fun)(void*);

static media_pthread_fun
media_play_substream(media_subthread *mst)
{
	media_substream *mss = mst->substream;

        sound_th_blksig();

	switch (media_substream_type(mss)) {
	case MTYPE_AUDIO:
		return (media_pthread_fun)sound_play_stream_th;
		break;
	case MTYPE_VIDEO:
		return (media_pthread_fun)NULL;
		break;
	default:
		break;
	}

	return (media_pthread_fun)NULL;
}

#endif	/* MEDIA_USE_QUEUE */

DEFUN("test-threads", Ftest_threads, 0, 0, 0, /*
					       */
      ())
{
	pthread_t thid;
	pthread_attr_t attr;

	pthread_attr_init(&attr);

	pthread_create(&thid, &attr, sound_test_th, (void*)NULL);

	pthread_attr_destroy(&attr);

	return Qt;
}

#if MEDIA_USE_QUEUE
DEFUN("init-asynchronousity", Finit_asynchronousity, 0, 0, 0, /*
Initialise queue listener threads.
The call to this function is mandatory on systems with a broken
pthread library.
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
	if (DLLISTP(Vmedia_queue))
		return Qnil;

	CHECK_NATNUM(Vnumber_of_media_queues);
	num_media_queues = XINT(Vnumber_of_media_queues);
	if (num_media_queues <= 0)
		return Qnil;

	/* set synchronous-sounds to `nil', because initting asynchronous
	   sounds implies we want to use them, too */
	Vsynchronous_sounds = Qnil;

	media_queue_privmutex = xnew_array(pthread_mutex_t, num_media_queues);
	media_queue = xnew_array(pthread_t, num_media_queues);

	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	pthread_mutex_init(&media_queue_mutex, NULL);
	pthread_mutex_init(&media_dequeue_mutex, NULL);
	pthread_cond_init(&media_dequeue_cond, NULL);

	pthread_mutex_init(&media_queue_counter_mutex, NULL);

	for (i=0; i < num_media_queues; i++) {
		pthread_mutex_init(&media_queue_privmutex[i], NULL);
		pthread_create(&media_queue[i], &attr,
			       sound_queue_th, (void*)i);
	}

	pthread_mutex_lock(&media_queue_counter_mutex);
	queue_counter = 0;
	pthread_mutex_unlock(&media_queue_counter_mutex);
	pthread_mutex_lock(&media_queue_mutex);
	Vmedia_queue = make_dllist();
	pthread_mutex_unlock(&media_queue_mutex);

	pthread_attr_destroy(&attr);

	return Qt;
}

DEFUN("uninit-asynchronousity", Funinit_asynchronousity, 0, 0, 0, /*
Stop all queue listener threads.
Depending on whether there are busy threads this may block the
main execution loop until all threads are non-busy.
When called the complementary variable `synchronous-sounds' is
set to `t'.
								   */
      ())
{
	int i;

	/* cannot acquire lock on this one, since it may be dead already */
	if (!DLLISTP(Vmedia_queue))
		return Qnil;

	if (num_media_queues <= 0)
		return Qnil;

	/* lock _all_ private mutexes, this means queue threads which still
	   play stuff will block the whole SXE */
	for (i = 0; i < num_media_queues; i++)
		pthread_mutex_lock(&media_queue_privmutex[i]);

	pthread_mutex_lock(&media_queue_mutex);
	Vmedia_queue = Qnil;
	pthread_mutex_unlock(&media_queue_mutex);

	/* now signal the stopping sequence to all threads */
	for (i = 0; i < num_media_queues; i++) {
		pthread_mutex_lock(&media_dequeue_mutex);
		pthread_cond_broadcast(&media_dequeue_cond);
		pthread_mutex_unlock(&media_dequeue_mutex);
	}

	/* join the threads, gather their private mutexes and destroy them */
	for (i=0; i < num_media_queues; i++) {
		pthread_join(media_queue[i], (void**)NULL);
		pthread_mutex_unlock(&media_queue_privmutex[i]);
		pthread_mutex_destroy(&media_queue_privmutex[i]);
	}

	pthread_mutex_lock(&media_queue_counter_mutex);
	queue_counter = 0;
	pthread_mutex_unlock(&media_queue_counter_mutex);

	/* gather the global mutexes and semaphores */
	pthread_mutex_destroy(&media_queue_mutex);
	pthread_mutex_destroy(&media_dequeue_mutex);
	pthread_cond_destroy(&media_dequeue_cond);
	pthread_mutex_destroy(&media_queue_counter_mutex);

	/* free the mutex and pthread_t arrays */
	xfree(media_queue_privmutex);
	xfree(media_queue);

	/* reset this */
	num_media_queues = 0;

	/* reset synchronous-sounds to `t', because uninitting asynchronous
	   sounds implies we want to not use them anymore */
	Vsynchronous_sounds = Qt;

	return Qt;
}

static void
trigger_media_play(void)
{
/* signal one of the waiting threads to start playback */
	pthread_mutex_lock(&media_queue_mutex);
	if (!DLLISTP(Vmedia_queue))
		return;
	pthread_mutex_unlock(&media_queue_mutex);

	pthread_mutex_lock(&media_queue_counter_mutex);
	queue_counter++;
	pthread_mutex_unlock(&media_queue_counter_mutex);

	pthread_mutex_lock(&media_dequeue_mutex);
	pthread_cond_signal(&media_dequeue_cond);
	pthread_mutex_unlock(&media_dequeue_mutex);

	return;
}

static int
queue_has_free_slot_p(void)
{
	int result = 0;

	pthread_mutex_lock(&media_queue_counter_mutex);
	result = queue_counter;
	pthread_mutex_unlock(&media_queue_counter_mutex);

	return (result < num_media_queues);
}

static int
enqueue_stream(media_subthread *mst)
{
/* enqueue the substream within MST to the global media queue
 * iff there are free handler slots, i.e. a thread is waiting for 
 * unqueuing the thing
 */
	if (!queue_has_free_slot_p())
		return 0;

	pthread_mutex_lock(&media_queue_mutex);
	dllist_append(XDLLIST(Vmedia_queue),
		      make_opaque_ptr(mst));
	pthread_mutex_unlock(&media_queue_mutex);

	return 1;
}
#endif

DEFUN("play-media-stream-asynchronously", Fplay_media_stream_asynchronously,
      1, 3, 0, /*
Play the media stream STREAM on DEVICE _asynchronously_.
Return a media-thread object which can be used to interact with
the thread. See `pause-media-thread', `resume-media-thread' and
`stop-media-thread'.

Note: If the return value is not bound to a variable, the stream
will be stopped during the next garbage collection.

DEVICE must be an audio device created by `make-audio-device'.
DEVICE defaults to `default-audio-device'.

Optional third argument SENTINEL specifies a lisp function to be
called after the stream playback finishes.  The function should
take two arguments (STREAM STATE) where STREAM is bound to the
media stream which finished and STATE is a symbol (currently the
only valid symbol is 'finished).  See `set-media-thread-sentinel'.
	       */
      (stream, device, sentinel))
{
	Lisp_Object lmt;
	Lisp_Media_Thread *mt;
	media_subthread *mst;
	/* pthread stuff */
	int thret = -1;
#if !MEDIA_USE_QUEUE
	media_pthread_fun mpf;
	pthread_attr_t attr;
#endif

	/* This function can GC */
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
	GCPRO4(stream, device, sentinel, lmt);

	CHECK_MEDIA_STREAM(stream);

	if (NILP(device))
		device = Vdefault_audio_device;
	else
		CHECK_AUDIO_DEVICE(device);

#if MEDIA_USE_QUEUE
	if (!DLLISTP(Vmedia_queue)) {
		UNGCPRO;
		return Qnil;
	}
#endif


	/* hm, it's useful to stop here if default-audio-device is nil,
	 * i merely spit out a warning and return nil, that should suffice
	 */
	if (NILP(device)) {
		warn_when_safe(
			Qdevice, Qerror,
			GETTEXT("play-media-stream: "
				"no device specified, "
				"consider setting `default-audio-device'."));
		UNGCPRO;
		return Qnil;
	}

#if MEDIA_USE_QUEUE

	if (!queue_has_free_slot_p()) {
		warn_when_safe(
			Qdevice, Qnotice,
			GETTEXT("play-media-stream: "
				"all playback slots are busy, "
				"come back later."));
		UNGCPRO;
		return Qnil;
	}

#else  /* !MEDIA_USE_QUEUE */
	pthread_attr_init(&attr);
#if MEDIA_THREADS_RUN_DETACHED
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
#else
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
#endif
#endif

	lmt = make_media_thread(stream, device);
	mt = XMEDIA_THREAD(lmt);
	mt->play_state = MTPSTATE_RUN;
	mst = mt->first;
	while (mst) {

#if MEDIA_USE_QUEUE

		if (enqueue_stream(mst)) {
			trigger_media_play();
			thret = 0;
		}

#else
		mpf = media_play_substream(mst);

		if (mpf)
			thret = thret &&
				pthread_create(&mst->thread, &attr,
					       mpf, (void*)mst);


#endif

		mst = mst->next;
	}

#if !MEDIA_USE_QUEUE
	pthread_attr_destroy(&attr);
#endif

	if (thret == 0) {
		mt->state = MTSTATE_RUNNING;
	}

	if (!NILP(sentinel))
		Fset_media_thread_sentinel(lmt, sentinel);

	RETURN_UNGCPRO(lmt);
}
#endif

DEFUN("join-media-thread", Fjoin_media_thread, 1, 1, 0, /*
Join a media thread previously started by `play-media-thread-async'.
							 */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

#if MEDIA_THREADS_RUN_DETACHED || MEDIA_USE_QUEUE
	return Qt;
#else
	return media_thread_join(XMEDIA_THREAD(thread));
#endif
}

DEFUN("pause-media-thread", Fpause_media_thread, 1, 1, 0, /*
Pause a media thread previously started by `play-media-thread-async'.
							  */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	XMEDIA_THREAD(thread)->play_state = MTPSTATE_PAUSE;
	return Qt;
}

DEFUN("resume-media-thread", Fresume_media_thread, 1, 1, 0, /*
Pause a media thread previously started by `play-media-thread-async'.
							    */
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	XMEDIA_THREAD(thread)->play_state = MTPSTATE_RUN;
	return Qt;
}

DEFUN("stop-media-thread", Fstop_media_thread, 1, 1, 0, /*
Stop a media thread previously started by `play-media-thread-async'.
							*/
      (thread))
{
	CHECK_MEDIA_THREAD(thread);

	XMEDIA_THREAD(thread)->play_state = MTPSTATE_STOP;
	XMEDIA_THREAD(thread)->state = MTSTATE_FINISHED;
	return Fjoin_media_thread(thread);
}


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
	else
		call3(Qplay_sound, sound, Qnil, device);

	last_bell_time = now;
	last_bell_device = d;
	RETURN_UNGCPRO(Qnil);
}

DEFUN("wait-for-sounds", Fwait_for_sounds, 0, 1, 0,	/*
Wait for all sounds to finish playing on DEVICE.
							 */
      (device))
{
#ifdef HAVE_NAS_SOUND
	struct device *d = decode_device(device);
	if (DEVICE_CONNECTED_TO_NAS_P(d)) {
		/* #### somebody fix this to be device-dependent. */
		nas_wait_for_sounds();
	}
#endif
	return Qnil;
}

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

#ifdef HAVE_NAS_SOUND

static void init_nas_sound(struct device *d)
{
#ifdef HAVE_X_WINDOWS
	if (DEVICE_X_P(d)) {
		char *err_message = nas_init_play(DEVICE_X_DISPLAY(d));
		DEVICE_CONNECTED_TO_NAS_P(d) = !err_message;
		/* Print out the message? */
	}
#endif				/* HAVE_X_WINDOWS */
}

#endif				/* HAVE_NAS_SOUND */

#ifdef HAVE_OSS_SOUND

static void init_oss_sound(struct device *d)
{
	if (DEVICE_TTY_P(d) || DEVICE_STREAM_P(d))
		DEVICE_ON_CONSOLE_P(d) = 1;
#ifdef HAVE_X_WINDOWS
	else {
		/* When running on a machine with native sound support, we
		   cannot use digitized sounds as beeps unless emacs is running
		   on the same machine that $DISPLAY points to, and $DISPLAY
		   points to frame 0 of that machine.
		*/

		Display *display = DEVICE_X_DISPLAY(d);
		char *dpy = DisplayString(display);
		char *tail = (char *)strchr(dpy, ':');
		if (!tail || strncmp(tail, ":0", 2))
			DEVICE_ON_CONSOLE_P(d) = 0;
		else {
			char dpyname[255], localname[255];

			/* some systems can't handle SIGIO or SIGALARM in
			   gethostbyname.
			*/
			stop_interrupts();
			strncpy(dpyname, dpy, tail - dpy);
			dpyname[tail - dpy] = 0;
			if (!*dpyname ||
			    !strcmp(dpyname, "unix") ||
			    !strcmp(dpyname, "localhost"))
				DEVICE_ON_CONSOLE_P(d) = 1;
			else if (gethostname(localname, sizeof(localname)))
				DEVICE_ON_CONSOLE_P(d) = 0; /* can't find
							       hostname? */
			else {
				/* We have to call gethostbyname() on the result
				   of gethostname() because the two aren't
				   guaranteed to be the same name for the same
				   host: on some losing systems, one is a FQDN
				   and the other is not.  Here in the wide
				   wonderful world of Unix it's rocket science
				   to obtain the local hostname in a portable
				   fashion.

				   And don't forget, gethostbyname() reuses the
				   structure it returns, so we have to copy the
				   fucker before calling it again.

				   Thank you master, may I have another.
				*/
				struct hostent *h = gethostbyname(dpyname);
				if (!h)
					DEVICE_ON_CONSOLE_P(d) = 0;
				else {
					char hn[255];
					struct hostent *l;
					strcpy(hn, h->h_name);
					l = gethostbyname(localname);
					DEVICE_ON_CONSOLE_P(d) = (l
								  &&
								  !(strcmp
								    (l->h_name,
								     hn)));
				}
			}
			start_interrupts();
		}
	}
#endif	/* HAVE_X_WINDOWS */
}

#endif	/* HAVE_OSS_SOUND */

void init_device_sound(struct device *d)
{
#ifdef HAVE_NAS_SOUND
	init_nas_sound(d);
#endif

#ifdef HAVE_OSS_SOUND
	init_oss_sound(d);
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
	if (obj);

	return Qnil;
}

static void
audio_device_finalise(void *header, int for_disksave)
{
	Lisp_Audio_Device *ad = (Lisp_Audio_Device*)header;

	switch (audio_device_driver(ad)) {
	case ADRIVER_NAS:
#ifdef HAVE_NAS_SOUND
		if (audio_device_data(ad))
			sound_nas_finish(audio_device_data(ad));
#endif
		break;
	case ADRIVER_ARTS:
#ifdef HAVE_ARTS_SOUND
		if (audio_device_data(ad))
			sound_arts_finish(audio_device_data(ad));
#endif
		break;
	case ADRIVER_ALSA:
#ifdef HAVE_ALSA_SOUND
		if (audio_device_data(ad))
			sound_alsa_finish(audio_device_data(ad));
#endif
		break;
	case ADRIVER_AO:
#ifdef HAVE_AO_SOUND
		if (audio_device_data(ad))
			sound_ao_finish(audio_device_data(ad));
#endif
		break;
	case ADRIVER_POLYP:
#ifdef HAVE_POLYP_SOUND
		if (audio_device_data(ad))
			sound_polyp_finish(audio_device_data(ad));
#endif
		break;
	case ADRIVER_ESD:
#ifdef HAVE_ESD_SOUND
		if (audio_device_data(ad))
			sound_esd_finish(audio_device_data(ad));
#endif
		break;
	default:
		break;
	}

	if (audio_device_data(ad))
		xfree(audio_device_data(ad));

	/* avoid some warning */
	if (for_disksave || ad == NULL);
}

static void
audio_device_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char *subinfo = NULL;

	write_c_string("#<audio-device :type ", printcharfun);

	switch (XAUDIO_DEVICE_DRIVER(obj)) {
	case ADRIVER_OSS:
		write_c_string("oss", printcharfun);
#ifdef HAVE_OSS_SOUND
		/* subinfo = sound_oss_subprint(obj); */
#endif
		break;

	case ADRIVER_ARTS:
		write_c_string("aRts", printcharfun);
#ifdef HAVE_ARTS_SOUND
		/* subinfo = sound_arts_subprint(obj); */
#endif
		break;

	case ADRIVER_NAS:
		write_c_string("nas", printcharfun);
#ifdef HAVE_NAS_SOUND
		/* subinfo = sound_nas_subprint(obj); */
#endif
		break;

	case ADRIVER_ESD:
		write_c_string("esd", printcharfun);
#ifdef HAVE_ESD_SOUND
		subinfo = sound_esd_subprint(obj);
#endif
		break;

	case ADRIVER_POLYP:
		write_c_string("polyp", printcharfun);
#ifdef HAVE_POLYP_SOUND
		subinfo = sound_polyp_subprint(obj);
#endif
		break;

	case ADRIVER_AO:
		write_c_string("ao", printcharfun);
#ifdef HAVE_AO_SOUND
		subinfo = sound_ao_subprint(obj);
#endif
		break;

	case ADRIVER_JACK:
		write_c_string("jack", printcharfun);
#ifdef HAVE_JACK_SOUND
		/* subinfo = sound_jack_subprint(obj); */
#endif
		break;

	case ADRIVER_ALSA:
		write_c_string("alsa", printcharfun);
#ifdef HAVE_ALSA_SOUND
		/* subinfo = sound_alsa_subprint(obj); */
#endif
		break;

	case ADRIVER_UNDECIDED:
	default:
		write_c_string("unknown", printcharfun);
		break;
	}

	if (subinfo) {
		write_c_string(subinfo, printcharfun);
		xfree(subinfo);
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
	audio_driver ad = ADRIVER_UNDECIDED;

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
#ifdef HAVE_AO_SOUND
	else if (EQ(type, Qao))
		ad = ADRIVER_AO;
#endif
#ifdef HAVE_ARTS_SOUND
	else if (EQ(type, Qarts))
		ad = ADRIVER_ARTS;
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
	audio_driver ad = ADRIVER_UNDECIDED;

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


Lisp_Object make_audio_device(Lisp_Object type)
{
	Lisp_Audio_Device *ad;
	Lisp_Object lad;

	CHECK_SYMBOL(type);

	ad = audio_device_allocate();
	audio_device_driver(ad) = decode_audio_type(type);
	audio_device_data(ad) = NULL;
	XSETAUDIO_DEVICE(lad, ad);

	return lad;
}

DEFUN("make-audio-device", Fmake_audio_device, 0, 2, 0,	/*
Create a new device to output audio via DRIVER.
DRIVER should be a symbol out of 'oss, 'nas, 'esd, 'polyp,
'jack, 'alsa, 'arts or 'ao.

The rest arguments may be used to pass options to the selected
output driver. These should be `:keyword value' pairs.

Valid keywords for ESD are:
:server - to use a distant ESD daemon (e.g. "my.machine.box")
:port - the port number of a distant daemon (e.g. 16001)
The default for ESD output is to use a locally running daemon and
to connect to it via unix domain sockets.

Valid keywords for Polyp are:
:server - the host name to connect to (default: "localhost")
:device - the name of the sink/source to connect to (e.g. "output1")
:client - how to call the client on the server (default "SXEmacs")
:stream - how to call the stream on the server (e.g. "fancy-sound")

Valid keywords for AO are:
:driver - the name of the output driver (e.g. "alsa", "esd", etc.)
:options - a list of AO suboptions (see AO documentation)
The default for AO output is to pass nothing and entirely use the
system and user configuration files.
								*/
      (driver, device_options))
{
	Lisp_Object ad;
	audio_driver dev_driver;
	void *device_data = NULL;

	CHECK_SYMBOL(driver);
	ad = make_audio_device(driver);

	dev_driver = XAUDIO_DEVICE_DRIVER(ad);

	switch (dev_driver) {
	case ADRIVER_NAS:
#ifdef HAVE_NAS_SOUND
		device_data = sound_nas_create(device_options);
		break;
#endif
	case ADRIVER_ARTS:
#ifdef HAVE_ARTS_SOUND
		device_data = sound_arts_create(device_options);
		break;
#endif
	case ADRIVER_ALSA:
#ifdef HAVE_ALSA_SOUND
		device_data = sound_alsa_create(device_options);
		break;
#endif
	case ADRIVER_AO:
#ifdef HAVE_AO_SOUND
		device_data = sound_ao_create(device_options);
		if (device_data == NULL)
			XAUDIO_DEVICE_STATE(ad) = ASTATE_DEAD;
		break;
#endif
	case ADRIVER_POLYP:
#ifdef HAVE_POLYP_SOUND
		device_data = sound_polyp_create(device_options);
		if (device_data == NULL)
			XAUDIO_DEVICE_STATE(ad) = ASTATE_DEAD;
		break;
#endif
	case ADRIVER_ESD:
#ifdef HAVE_ESD_SOUND
		device_data = sound_esd_create(device_options);
		if (device_data == NULL)
			XAUDIO_DEVICE_STATE(ad) = ASTATE_DEAD;
		break;
#endif
	default:
		break;
	}

	XAUDIO_DEVICE_DATA(ad) = device_data;
	
	return ad;
}

DEFUN("audio-device-p", Faudio_device_p, 1, 1, 0, /*
Return non-`nil' if object is an audio-device, `nil' otherwise.
						  */
      (object))
{
	if (AUDIO_DEVICEP(object))
		return Qt;
	else
		return Qnil;
}


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

#if MEDIA_THREADS_RUN_DETACHED
static void
#else
static Lisp_Object
#endif
media_thread_join(Lisp_Media_Thread *mt)
{
	Lisp_Object result = Qnil;
	int thret = -1;
	media_subthread *mst;

	mst = media_thread_first(mt);

#if defined(HAVE_THREADS) && !MEDIA_THREADS_RUN_DETACHED
	/* we use detached streams, no need to join therefore */
	while (mst) {
		if (mst->thread) {
			thret = thret &&
				pthread_join(mst->thread,
					     (void**)((void*)&result));
			mst->thread = 0;
		}
		mst = media_subthread_next(mst);
	}
#endif

	if (thret == 0) {
		mt->result = result;
		mt->state = MTSTATE_FINISHED;
	}

#if MEDIA_THREADS_RUN_DETACHED
	return;
#else
	return result;
#endif
}

static void
media_thread_finalise(void *header, int for_disksave)
{
	Lisp_Media_Thread *mt = (Lisp_Media_Thread*)header;

	mt->play_state = MTPSTATE_STOP;
	media_thread_join(mt);

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
		write_c_string(") ", printcharfun);
		break;
	case MTSTATE_UNDECIDED:
	default:
		write_c_string(":unknown-state", printcharfun);
		break;
	}

	mst = media_thread_first(mt);
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
	mst->thread = 0;

	return mst;
}

static media_subthread *
make_media_subthread_append(Lisp_Media_Thread *mt)
{
	media_subthread *mst;

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

	return mst;
}

static media_subthread *
make_media_subthread_prepend(Lisp_Media_Thread *mt)
{
	media_subthread *mst;

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
	mt->state = MTSTATE_UNDECIDED;
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

	XSETMEDIA_THREAD(lmt, mt);

	return lmt;
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
#ifdef HAVE_AO_SOUND
	defsymbol(&Qao, "ao");
#endif
#ifdef HAVE_ARTS_SOUND
	defsymbol(&Qarts, "arts");
#endif
#ifdef HAVE_ALSA_SOUND
	defsymbol(&Qalsa, "alsa");
#endif
#ifdef HAVE_OSS_SOUND
	defsymbol(&Qoss, "oss");
#endif
	defsymbol(&Qaudio_devicep, "audio-device-p");
	defsymbol(&Qmedia_threadp, "media-thread-p");

#ifdef HAVE_THREADS
	DEFSUBR(Ftest_threads);
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
	DEFSUBR(Fding);
	DEFSUBR(Fwait_for_sounds);
	DEFSUBR(Fconnected_to_nas_p);
	DEFSUBR(Fdevice_sound_enabled_p);

	/* audio device fake */
	DEFSUBR(Fmake_audio_device);
	DEFSUBR(Faudio_device_p);
}

void vars_of_sound(void)
{
#ifdef HAVE_OSS_SOUND
	Fprovide(intern("native-sound")); /* for compatibility */
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
*How loud to be, from 0 to 100.
						 */ );
	bell_volume = 50;

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

#if MEDIA_USE_QUEUE

	DEFVAR_LISP("media-queue", &Vmedia_queue	/*
Internal queue, DO NOT FIDDLE WITH IT!!!!
Oh, do fiddle with it if you can't get enough if these cute core files. :)
							*/ );
	Vmedia_queue = Qnil;

	DEFVAR_LISP("number-of-media-queues", &Vnumber_of_media_queues	/*
Number of threads spawned as queue listeners.
									*/ );
	Vnumber_of_media_queues = make_int(4);

#endif	/* MEDIA_USE_QUEUE */


#if defined (HAVE_OSS_SOUND) && defined (hp9000s800) && 0
/* we do not support hpplay anymore */
	{
		void vars_of_hpplay(void);
		vars_of_hpplay();
	}
#endif
}
