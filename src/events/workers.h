/*
  workers.h -- worker threads
  Copyright (C) 2006, 2007, 2008 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

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
  */

/* Inspired by XEmacs' events.c written by Jamie Zawinski */

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_workers_h_
#define INCLUDED_workers_h_

#include "semaphore.h"

#ifdef ALL_DEBUG_FLAGS
#undef WORKERS_DEBUG_FLAG
#define WORKERS_DEBUG_FLAG
#endif

#define __WORKERS_DEBUG__(args...)	fprintf(stderr, "workers " args)
#ifndef WORKERS_DEBUG_FLAG
#define WORKERS_DEBUG(args...)
#else
#define WORKERS_DEBUG(args...)		__WORKERS_DEBUG__(args)
#endif
#define WORKERS_DEBUG_WORKER(args...)	WORKERS_DEBUG("[worker]: " args)
#define WORKERS_DEBUG_SCRATCH(args...)	WORKERS_DEBUG("[scratch]: " args)
#define WORKERS_CRITICAL(args...)	__WORKERS_DEBUG__("CRITICAL: " args)

typedef struct eq_worker_s *eq_worker_t;
typedef struct worker_job_s *worker_job_t;
typedef struct work_handler_s *work_handler_t;
typedef enum worker_job_state_e worker_job_state_t;

extern Lisp_Object Qworker_jobp;
extern Lisp_Object Qunknown, Qrunning, Qfinished, Qqueued;


struct eq_worker_s {
	sxe_thread_t thread;
	sxe_mutex_t mtx;
	struct gcpro *gcprolist;

	/* things that taste like ice cream */
	void *scratch;
	size_t scratch_alloc_size;
};

/* worker magic */
#define eq_worker_thread(_x)	((_x)->thread)
#define eq_worker_mtx(_x)	((_x)->mtx)
#define eq_worker_gcprolist(_x)	((_x)->gcprolist)
#define eq_worker_scratch(_x)	((_x)->scratch)
#define eq_worker_scratch_alloc_size(_x)	((_x)->scratch_alloc_size)

extern eq_worker_t eq_make_worker(void);
extern void eq_free_worker(eq_worker_t);
extern void resize_worker_scratch(eq_worker_t eqw, size_t new_size);

extern_inline void eq_lock_meself(eq_worker_t eqw);
extern_inline void eq_unlock_meself(eq_worker_t eqw);
extern_inline void lock_worker_job(worker_job_t job);
extern_inline void unlock_worker_job(worker_job_t job);

extern_inline void
eq_lock_meself(eq_worker_t eqw)
{
	SXE_MUTEX_LOCK(&eq_worker_mtx(eqw));
}

extern_inline void
eq_unlock_meself(eq_worker_t eqw)
{
	SXE_MUTEX_UNLOCK(&eq_worker_mtx(eqw));
}


enum worker_job_state_e {
	WORKER_JOB_UNKNOWN,
	WORKER_JOB_QUEUED,
	WORKER_JOB_RUNNING,
	WORKER_JOB_FINISHED,
	NUMBER_OF_WORKER_JOB_STATES
};

struct worker_job_s {
	struct lcrecord_header lheader;
	Lisp_Object queue;	/* used to be event_queue_t */
	work_handler_t handler;
	void *data;
#if !defined(EF_USE_POM) && defined(HAVE_THREADS)
	sxe_mutex_t mtx;
#endif
	worker_job_state_t state;

	Lisp_Object result;
	Lisp_Object plist;

	/* some support from the underlying worker */
	void *buffer;
	size_t buffer_alloc_size;
};

DECLARE_LRECORD(worker_job, struct worker_job_s);
#define XWORKER_JOB(x)		XRECORD(x, worker_job, struct worker_job_s)
#define XSETWORKER_JOB(x, p)	XSETRECORD(x, p, worker_job)
#define wrap_worker_job(p)	wrap_object(p)
#define WORKER_JOBP(x)		RECORDP(x, worker_job)
#define CHECK_WORKER_JOB(x)	CHECK_RECORD(x, worker_job)
#define CONCHECK_WORKER_JOB(x)	CONCHECK_RECORD(x, worker_job)

#define worker_job_handler(_x)	((_x)->handler)
#define worker_job_queue(_x)	((_x)->queue)
#define worker_job_data(_x)	((_x)->data)
#define worker_job_state(_x)	((_x)->state)
#define worker_job_result(_x)	((_x)->result)
#define worker_job_plist(_x)	((_x)->plist)
#if defined(EF_USE_POM)
#define worker_job_mtx(_x)	(XRECORD_MTX(_x))
#else
#define worker_job_mtx(_x)	((_x)->mtx)
#endif
#define worker_job_buffer(_x)	((_x)->buffer)
#define worker_job_buffer_alloc_size(_x)	((_x)->buffer_alloc_size)
#define XWORKER_JOB_HANDLER(_x)	worker_job_handler(XWORKER_JOB(_x))
#define XWORKER_JOB_QUEUE(_x)	worker_job_queue(XWORKER_JOB(_x))
#define XWORKER_JOB_DATA(_x)	worker_job_data(XWORKER_JOB(_x))
#define XWORKER_JOB_STATE(_x)	worker_job_state(XWORKER_JOB(_x))
#define XWORKER_JOB_RESULT(_x)	worker_job_result(XWORKER_JOB(_x))
#define XWORKER_JOB_PLIST(x)	worker_job_plist(XWORKER_JOB(x))
#define XWORKER_JOB_MTX(_x)	worker_job_mtx(XWORKER_JOB(_x))
#define XWORKER_JOB_BUFFER(_x)	(worker_job_buffer(XWORKER_JOB(_x)))
#define XWORKER_JOB_BUFFER_ALLOC_SIZE(_x)	\
	(worker_job_buffer_alloc_size(XWORKER_JOB(_x)))

extern worker_job_t make_noseeum_worker_job(work_handler_t handler);
extern void free_noseeum_worker_job(worker_job_t job);
extern worker_job_t make_worker_job(work_handler_t handler);
extern Lisp_Object make_worker_job_ts(Lisp_Object*, work_handler_t handler);

extern_inline void
lock_worker_job(worker_job_t job)
{
	SXE_MUTEX_LOCK(&worker_job_mtx(job));
}

extern_inline void
unlock_worker_job(worker_job_t job)
{
	SXE_MUTEX_UNLOCK(&worker_job_mtx(job));
}


typedef void(*work_handler_mark_f)(worker_job_t);
typedef void(*work_handler_print_f)(worker_job_t, Lisp_Object pcf);
typedef void(*work_handler_finalise_f)(worker_job_t);
typedef void(*work_handler_f)(worker_job_t);
typedef void(*work_started_f)(worker_job_t);
typedef void(*work_finished_f)(worker_job_t);

struct work_handler_s {
	work_handler_mark_f marker;
	work_handler_print_f printer;
	work_handler_finalise_f finaliser;
	work_handler_f handler;
	work_started_f started;
	work_finished_f finished;
};

#define work_handler_marker(_x)		((_x)->marker)
#define work_handler_printer(_x)	((_x)->printer)
#define work_handler_finaliser(_x)	((_x)->finaliser)
#define work_handler(_x)		((_x)->handler)
#define work_started(_x)		((_x)->started)
#define work_finished(_x)		((_x)->finished)
#define work_handler_handle(_x, _y)	work_handler(_x)(_y)

#endif	/* INCLUDED_workers_h_ */
