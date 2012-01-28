/*** worker-asyneq.c -- worker threads for asyneq feature
 *
 * Copyright (C) 2006-2008  Sebastian Freundt
 *
 * Author:  Sebastian Freundt <hroptatyr@sxemacs.org>
 *
 * This file is part of SXEmacs.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of any contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ***/

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include "syssignal.h"
#include "worker-asyneq.h"
#define INCLUDE_EVENTS_H_PRIVATE_SPHERE
#include "events.h"

event_queue_t delegate_eq = Qnull_pointer;
static Lisp_Object Vdelegate_eq;
static void eq_worker_th_blksig(void);

static struct work_handler_s eat_yerself = {
	NULL,			/* markfun */
	NULL,			/* printer */
	NULL			/* finaliser */
};


void
init_workers(int nthreads, sxe_thread_f handler)
{
	pthread_attr_t attr;
	int i;

	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	for (i=0; i < nthreads; i++) {
		eq_worker_t eqw = eq_make_worker();
		/* value taken from SOUND_MAX_AUDIO_FRAME_SIZE */
		resize_worker_scratch(eqw, 48000*6*sizeof(uint32_t));
		xthread_create(
			&eq_worker_thread(eqw), &attr, handler, eqw);
		dllist_append(workers, eqw);
	}

	pthread_attr_destroy(&attr);
	return;
}

void
fini_worker(eq_worker_t eqw)
{
	xthread_join(eq_worker_thread(eqw), NULL);
	SXE_MUTEX_UNLOCK(&eq_worker_mtx(eqw));
	eq_free_worker(eqw);
}


extern event_queue_t asyneq;

void
eq_worker_eaten_myself(eq_worker_t eqw)
{
	Lisp_Object emev = Qnil;
	struct gcpro gcpro1;

	GCPRO1(emev);
	emev = make_empty_event();
	XEVENT(emev)->event_type = eaten_myself_event;
	XEVENT(emev)->event.eaten_myself.worker = eqw;
	eq_enqueue(asyneq, emev);
	UNGCPRO;
	return;
}

void
eq_worker_work_started(Lisp_Object job)
{
	Lisp_Object wsev = Qnil;
	struct gcpro gcpro1;

	GCPRO1(wsev);
	wsev = make_empty_event();
	XEVENT(wsev)->event_type = work_started_event;
	XEVENT(wsev)->event.work_started.job = job;
	eq_enqueue(asyneq, wsev);
	UNGCPRO;
	return;
}

void
eq_worker_work_finished(Lisp_Object job)
{
	Lisp_Object wfev = Qnil;
	struct gcpro gcpro1;

	GCPRO1(wfev);
	wfev = make_empty_event();
	XEVENT(wfev)->event_type = work_finished_event;
	XEVENT(wfev)->event.work_finished.job = job;
	eq_enqueue(asyneq, wfev);
	UNGCPRO;
	return;
}

void
eq_delegate_work(event_queue_t eq)
{
	int cur = eq_queue_size(eq);
	while (cur--) {
		eq_queue_trigger(eq);
	}
	return;
}


static void
eq_worker_th_blksig(void)
{
	EMACS_BLOCK_SIGNAL(SIGINT);	/* ANSI */
	EMACS_BLOCK_SIGNAL(SIGILL);	/* ANSI */
	EMACS_BLOCK_SIGNAL(SIGABRT);	/* ANSI */
	EMACS_BLOCK_SIGNAL(SIGFPE);	/* ANSI */
	EMACS_BLOCK_SIGNAL(SIGSEGV);	/* ANSI */
	EMACS_BLOCK_SIGNAL(SIGTERM);	/* ANSI */

#if defined SIGHUP
	EMACS_BLOCK_SIGNAL(SIGHUP);	/* POSIX */
#endif
#if defined SIGQUIT
	EMACS_BLOCK_SIGNAL(SIGQUIT);	/* POSIX */
#endif
#if defined SIGTRAP
	EMACS_BLOCK_SIGNAL(SIGTRAP);	/* POSIX */
#endif
#if defined SIGUSR1
	EMACS_BLOCK_SIGNAL(SIGUSR1);	/* POSIX */
#endif
#if defined SIGUSR2
	EMACS_BLOCK_SIGNAL(SIGUSR2);	/* POSIX */
#endif
#if defined SIGPIPE
	EMACS_BLOCK_SIGNAL(SIGPIPE);	/* POSIX */
#endif
#if defined SIGALRM
	EMACS_BLOCK_SIGNAL(SIGALRM);	/* POSIX */
#endif
#if defined SIGCHLD
	EMACS_BLOCK_SIGNAL(SIGCHLD);	/* POSIX */
#endif
#if defined SIGCONT
	EMACS_BLOCK_SIGNAL(SIGCONT);	/* POSIX */
#endif
#if defined SIGSTOP
	EMACS_BLOCK_SIGNAL(SIGSTOP);	/* POSIX */
#endif
#if defined SIGTSTP
	EMACS_BLOCK_SIGNAL(SIGTSTP);	/* POSIX */
#endif
#if defined SIGTTIN
	EMACS_BLOCK_SIGNAL(SIGTTIN);	/* POSIX */
#endif
#if defined SIGTTOU
	EMACS_BLOCK_SIGNAL(SIGTTOU);	/* POSIX */
#endif

#if defined SIGBUS
	EMACS_BLOCK_SIGNAL(SIGBUS);	/* XPG5 */
#endif
#if defined SIGPOLL
	EMACS_BLOCK_SIGNAL(SIGPOLL);	/* XPG5 */
#endif
#if defined SIGPROF
	EMACS_BLOCK_SIGNAL(SIGPROF);	/* XPG5 */
#endif
#if defined SIGSYS
	EMACS_BLOCK_SIGNAL(SIGSYS);	/* XPG5 */
#endif
#if defined SIGURG
	EMACS_BLOCK_SIGNAL(SIGURG);	/* XPG5 */
#endif
#if defined SIGXCPU
	EMACS_BLOCK_SIGNAL(SIGXCPU);	/* XPG5 */
#endif
#if defined SIGXFSZ
	EMACS_BLOCK_SIGNAL(SIGXFSZ);	/* XPG5 */
#endif
#if defined SIGVTALRM
	EMACS_BLOCK_SIGNAL(SIGVTALRM);	/* XPG5 */
#endif

#if defined SIGIO
	EMACS_BLOCK_SIGNAL(SIGIO);	/* BSD 4.2 */
#endif
#if defined SIGWINCH
	EMACS_BLOCK_SIGNAL(SIGWINCH);	/* BSD 4.3 */
#endif

#if defined SIGEMT
	EMACS_BLOCK_SIGNAL(SIGEMT);
#endif
#if defined SIGINFO
	EMACS_BLOCK_SIGNAL(SIGINFO);
#endif
#if defined SIGHWE
	EMACS_BLOCK_SIGNAL(SIGHWE);
#endif
#if defined SIGPRE
	EMACS_BLOCK_SIGNAL(SIGPRE);
#endif
#if defined SIGUME
	EMACS_BLOCK_SIGNAL(SIGUME);
#endif
#if defined SIGDLK
	EMACS_BLOCK_SIGNAL(SIGDLK);
#endif
#if defined SIGCPULIM
	EMACS_BLOCK_SIGNAL(SIGCPULIM);
#endif
#if defined SIGIOT
	EMACS_BLOCK_SIGNAL(SIGIOT);
#endif
#if defined SIGLOST
# if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	EMACS_BLOCK_SIGNAL(SIGLOST);
# endif	 /* BDWGC case */
#endif
#if defined SIGSTKFLT
	EMACS_BLOCK_SIGNAL(SIGSTKFLT);
#endif
#if defined SIGUNUSED
	EMACS_BLOCK_SIGNAL(SIGUNUSED);
#endif
#if defined SIGDANGER
	EMACS_BLOCK_SIGNAL(SIGDANGER);	/* AIX */
#endif
#if defined SIGMSG
	EMACS_BLOCK_SIGNAL(SIGMSG);
#endif
#if defined SIGSOUND
	EMACS_BLOCK_SIGNAL(SIGSOUND);
#endif
#if defined SIGRETRACT
	EMACS_BLOCK_SIGNAL(SIGRETRACT);
#endif
#if defined SIGGRANT
	EMACS_BLOCK_SIGNAL(SIGGRANT);
#endif
#if defined SIGPWR
# if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	EMACS_BLOCK_SIGNAL(SIGPWR);
# endif	 /* BDWGC case */
#endif
}

static void *
eq_worker_th(void *eqwptr)
{
	Lisp_Object ljob = Qnil;
	worker_job_t job = NULL;
	eq_worker_t eqw = eqwptr;
	work_handler_t hdl;
	struct gcpro gcpro1;

	eq_worker_th_blksig();

	GCPRO1(ljob);
listen:
	eq_queue_synch(delegate_eq);

	EQUEUE_DEBUG_WORKER("dequeuing thread: 0x%lx\n",
			    (long unsigned int)pthread_self());

	/* fetch one event now */
refetch:
	ljob = Qnil;
	eq_dequeue_pro(&ljob, delegate_eq);
	if (NILP(ljob)) {
		EQUEUE_DEBUG_WORKER("No event on the queue. "
				    "Who dared to wake me up?! >8(\n");
		goto listen;
	}

	eq_lock_meself(eqw);
	job = XWORKER_JOB(ljob);
	hdl = XWORKER_JOB_HANDLER(ljob);
	EQUEUE_DEBUG_WORKER("escrowing event 0x%lx in worker 0x%lx.\n",
			    (long unsigned int)job,
			    (long unsigned int)eqw);

	/* maybe it's a eat-yourself ticket? */
	if (hdl == &eat_yerself) {
		/* awww ... we gotta exit :( */
		EQUEUE_DEBUG_WORKER(
			"Worker 0x%lx commits suicide...\n",
			(long unsigned int)eqw);
		eq_unlock_meself(eqw);
		eq_worker_eaten_myself(eqw);
		UNGCPRO;
		pthread_exit(NULL);
		return NULL;
	}

	/* help the job a bit with local resources */
	EQUEUE_DEBUG_SCRATCH("inherit scratch buffer 0x%lx of size %ld\n",
			     (long unsigned int)eq_worker_scratch(eqw),
			     eq_worker_scratch_alloc_size(eqw));
	worker_job_buffer(job) = eq_worker_scratch(eqw);
	worker_job_buffer_alloc_size(job) = eq_worker_scratch_alloc_size(eqw);

	/* generate a started event and update job state */
	worker_job_state(job) = WORKER_JOB_RUNNING;
	eq_worker_work_started(ljob);

	/* ... otherwise handle the event */
	work_handler(hdl)(job);

	/* generate a `finished' event,
	 * sentinel code shall be injected in the routine
	 * called by eq_worker_handle_event() */
	worker_job_state(job) = WORKER_JOB_FINISHED;
	eq_worker_work_finished(ljob);

	eq_unlock_meself(eqw);

	EQUEUE_DEBUG_WORKER("enqueuing thread: 0x%lx\n",
			    (long unsigned int)pthread_self());
	goto refetch;
	/* not reached */
	return NULL;
}

DEFUN("init-workers", Finit_workers, 1, 1, 0, /*
Initialise NUMBER-OF-WORKERS worker threads.
If called repeatedly this function does NOT add more workers
use `add-workers' instead.
*/
      (number_of_workers))
{
	CHECK_NATNUM(number_of_workers);
	if (dllist_get_size(workers) <= 1) {
		init_workers(XINT(number_of_workers), eq_worker_th);
	}
	return Qt;
}


DEFUN("add-workers", Fadd_workers, 1, 1, 0, /*
Add NUMBER-OF-WORKERS worker threads.
*/
      (number_of_workers))
{
	CHECK_NATNUM(number_of_workers);
	init_workers(XINT(number_of_workers), eq_worker_th);
	return Qt;
}

DEFUN("remove-workers", Fremove_workers, 0, 1, 0, /*
Stop NUMBER-OF-WORKERS worker threads.  By default stop all.
Depending on whether there are busy this operation may block the
main execution loop until all worker threads are non-busy.
*/
      (number_of_workers))
{
	Lisp_Object job = Qnil;
	int i, noev = 0;	/* how many eat_yerself events to send? */
	struct gcpro gcpro1;

	if (NILP(number_of_workers)) {
		noev = dllist_get_size(workers)-1;
	} else {
		CHECK_NATNUM(number_of_workers);
		noev = XINT(number_of_workers);
	}

	GCPRO1(job);
	for (i = 0; i < noev; i++) {
		job = wrap_object(make_worker_job(&eat_yerself));
		eq_enqueue(delegate_eq, job);
	}
	eq_queue_trigger_all(delegate_eq);
	UNGCPRO;
	return job;
}

DEFUN("trigger-workers", Ftrigger_workers, 0, 0, 0, /*
Trigger all worker threads.
*/
      ())
{
	eq_queue_trigger_all(delegate_eq);
	return Qt;
}

DEFUN("running-workers", Frunning_workers, 0, 0, 0, /*
Return the number of currently running worker threads,
the main thread excluded.
*/
      ())
{
	return make_int(dllist_get_size(workers)-1);
}


void syms_of_worker_asyneq(void)
{
	DEFSUBR(Finit_workers);
	DEFSUBR(Fadd_workers);
	DEFSUBR(Fremove_workers);
	DEFSUBR(Ftrigger_workers);
	DEFSUBR(Frunning_workers);
}

void reinit_vars_of_worker_asyneq(void)
{
	/* the delegate queue in case of multiple threads */
	delegate_eq = make_event_queue();
	XSETEVENT_QUEUE(Vdelegate_eq, delegate_eq);
	staticpro_nodump(&Vdelegate_eq);
}

void vars_of_worker_asyneq(void)
{
	Fprovide(intern("asyneq"));
}

/* workers.c ends here */
