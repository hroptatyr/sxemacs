/*
  workers.c -- worker threads
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

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include "workers.h"

Lisp_Object Qworker_jobp;
Lisp_Object Qunknown, Qrunning, Qfinished, Qqueued;


/* worker sorcery */
static inline void
eq_make_worker_scratch(eq_worker_t eqw, size_t size)
{
	if (size > 0) {
		eq_worker_scratch(eqw) = xmalloc_atomic(size);
		eq_worker_scratch_alloc_size(eqw) = size;
		return;
	} else {
		eq_worker_scratch(eqw) = NULL;
		eq_worker_scratch_alloc_size(eqw) = 0;
		return;
	}
}

eq_worker_t
eq_make_worker(void)
{
	eq_worker_t res = xnew_and_zero(struct eq_worker_s);

	SXE_MUTEX_INIT(&eq_worker_mtx(res));
	eq_worker_gcprolist(res) = NULL;
	eq_make_worker_scratch(res, 0);
	return res;
}

static inline void
eq_free_worker_scratch(eq_worker_t eqw)
{
	if (!eq_worker_scratch(eqw)) {
		return;
	}
	/* clean sweep */
	memset(eq_worker_scratch(eqw), 0, eq_worker_scratch_alloc_size(eqw));
	xfree(eq_worker_scratch(eqw));
	eq_worker_scratch(eqw) = NULL;
	eq_worker_scratch_alloc_size(eqw) = 0;
	return;
}

void
eq_free_worker(eq_worker_t eqw)
{
	if (eqw == NULL)
		return;

	SXE_MUTEX_FINI(&eq_worker_mtx(eqw));
	eq_free_worker_scratch(eqw);
	xfree(eqw);
	return;
}

void
resize_worker_scratch(eq_worker_t eqw, size_t new_size)
{
	eq_lock_meself(eqw);
	eq_free_worker_scratch(eqw);
	WORKERS_DEBUG_SCRATCH("resize to %ld\n", (long int)new_size);
	eq_make_worker_scratch(eqw, new_size);
	eq_unlock_meself(eqw);
	return;
}


/* worker jobs machinery */
static inline void
init_worker_job_mtx(worker_job_t job)
{
	SXE_MUTEX_INIT(&worker_job_mtx(job));
}

static inline void
fini_worker_job_mtx(worker_job_t job)
{
	SXE_MUTEX_FINI(&worker_job_mtx(job));
}

worker_job_t
make_noseeum_worker_job(work_handler_t handler)
{
	worker_job_t res = xnew(struct worker_job_s);

	worker_job_queue(res) = Qnil;
	worker_job_handler(res) = handler;
	init_worker_job_mtx(res);
	worker_job_result(res) = Qnil;
	worker_job_plist(res) = Qnil;
	return res;
}

worker_job_t
make_worker_job(work_handler_t handler)
{
	worker_job_t res =
		alloc_lcrecord_type(struct worker_job_s, &lrecord_worker_job);

	worker_job_queue(res) = Qnil;
	worker_job_handler(res) = handler;
	worker_job_buffer(res) = NULL;
	worker_job_buffer_alloc_size(res) = 0;
#ifndef EF_USE_POM
	/* we have to care ourselves about mutex initialisation */
	init_worker_job_mtx(res);
#endif
	worker_job_state(res) = WORKER_JOB_UNKNOWN;
	worker_job_result(res) = Qnil;
	worker_job_plist(res) = Qnil;
	return res;
}

Lisp_Object
make_worker_job_ts(Lisp_Object *into, work_handler_t handler)
{
	worker_job_t job;

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	lock_allocator();
#endif
	job = make_worker_job(handler);
	*into = (Lisp_Object)job;
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	unlock_allocator();
#endif
	return *into;
}

static Lisp_Object
worker_job_mark(Lisp_Object obj)
{
	work_handler_t hdl;
	worker_job_t job = XWORKER_JOB(obj);

	lock_worker_job(job);
	if ((hdl = worker_job_handler(job)) &&
	    (work_handler_marker(hdl))) {
		work_handler_marker(hdl)(job);
	}
	mark_object(worker_job_result(job));
	mark_object(worker_job_plist(job));
	unlock_worker_job(job);
	return worker_job_queue(job);
}

static void
worker_job_finalise(void *header, int SXE_UNUSED(for_disksave))
{
	work_handler_t hdl;
	worker_job_t job = header;
	if ((hdl = worker_job_handler(job)) &&
	    (work_handler_finaliser(hdl))) {
		work_handler_finaliser(hdl)(job);
	}
}

static void
worker_job_print(Lisp_Object obj, Lisp_Object pcf, int escapeflag)
{
	work_handler_t hdl;
	worker_job_t job = XWORKER_JOB(obj);

	lock_worker_job(job);
	write_c_string("#<worker-job", pcf);
	if ((hdl = worker_job_handler(job)) &&
	    (work_handler_printer(hdl))) {
		work_handler_printer(hdl)(job, pcf);
	}
	switch (worker_job_state(job)) {
	case WORKER_JOB_QUEUED:
		write_c_string(" :state 'queued>", pcf);
		break;
	case WORKER_JOB_RUNNING:
		write_c_string(" :state 'running>", pcf);
		break;
	case WORKER_JOB_FINISHED:
		write_c_string(" :state 'finished :result ", pcf);
		print_internal(worker_job_result(job), pcf, escapeflag);
		write_c_string(">", pcf);
		break;
	case WORKER_JOB_UNKNOWN:
	case NUMBER_OF_WORKER_JOB_STATES:
	default:
		write_c_string(" :state 'unknown>", pcf);
		break;
	}
	unlock_worker_job(job);
}

static int
worker_job_equal(Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return (obj1 == obj2);
}

static unsigned long
worker_job_hash (Lisp_Object obj, int SXE_UNUSED(depth))
{
	return (unsigned long)obj;
}

static Lisp_Object
worker_job_getprop(Lisp_Object obj, Lisp_Object property)
{
	Lisp_Object result = Qnil;
	worker_job_t job = XWORKER_JOB(obj);

	lock_worker_job(job);
	result = external_plist_get(
		&worker_job_plist(job), property, 0, ERROR_ME);
	unlock_worker_job(job);
	return result;
}

static int
worker_job_putprop(Lisp_Object obj, Lisp_Object property, Lisp_Object value)
{
	worker_job_t job = XWORKER_JOB(obj);

	lock_worker_job(job);
	external_plist_put(
		&worker_job_plist(job), property, value, 0, ERROR_ME);
	unlock_worker_job(job);
	return 1;
}

static int
worker_job_remprop(Lisp_Object obj, Lisp_Object property)
{
	Lisp_Object result = Qnil;
	worker_job_t job = XWORKER_JOB(obj);

	lock_worker_job(job);
	result = external_remprop(
		&worker_job_plist(job), property, 0, ERROR_ME);
	unlock_worker_job(job);
	return result;
}

DEFUN("worker-job-plist", Fworker_job_plist, 1, 1, 0, /*
Return the property list of WORKER-JOB.
*/
      (worker_job))
{
	Lisp_Object result = Qnil;
	worker_job_t job;

	CHECK_WORKER_JOB(worker_job);
	job = XWORKER_JOB(worker_job);

	lock_worker_job(job);
	result = worker_job_plist(job);
	unlock_worker_job(job);
	return result;
}

static const struct lrecord_description worker_job_description[] = {
	{XD_LISP_OBJECT, offsetof(struct worker_job_s, queue)},
	{XD_OPAQUE_DATA_PTR, offsetof(struct worker_job_s, handler)},
	{XD_OPAQUE_PTR, offsetof(struct worker_job_s, data)},
#if !defined(EF_USE_POM) && defined(HAVE_THREADS)
	{XD_OPAQUE_PTR, offsetof(struct worker_job_s, mtx)},
#endif
	{XD_LISP_OBJECT, offsetof(struct worker_job_s, result)},
	{XD_LISP_OBJECT, offsetof(struct worker_job_s, plist)},
	{XD_INT, offsetof(struct worker_job_s, state)},

	{XD_OPAQUE_PTR, offsetof(struct worker_job_s, buffer)},
	{XD_SIZE_T, offsetof(struct worker_job_s, buffer_alloc_size)},

	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS(
	"worker_job", worker_job,
	worker_job_mark, worker_job_print,
	worker_job_finalise,
	worker_job_equal, worker_job_hash,
	worker_job_description,
	worker_job_getprop, worker_job_putprop,
	worker_job_remprop, Fworker_job_plist,
	struct worker_job_s);

void
free_noseeum_worker_job(worker_job_t job)
{
	worker_job_finalise(job, 0);
	fini_worker_job_mtx(job);
	xfree(job);
	return;
}

DEFUN("get-worker-job-result", Fget_worker_job_result, 1, 1, 0, /*
Return the result slot of JOB.
*/
      (job))
{
	Lisp_Object res;

	CHECK_WORKER_JOB(job);

	lock_worker_job(XWORKER_JOB(job));
	res = XWORKER_JOB_RESULT(job);
	unlock_worker_job(XWORKER_JOB(job));
	return res;
}

DEFUN("worker-job-p", Fworker_job_p, 1, 1, 0, /*
Return non-nil iff JOB is a valid worker-job object.
*/
      (job))
{
	if (WORKER_JOBP(job)) {
		return Qt;
	} else {
		return Qnil;
	}
}

DEFUN("get-worker-job-state", Fget_worker_job_state, 1, 1, 0, /*
Return the state of JOB, will be one of
'unknown  when the job is in an unknown state
'queued   when the job is on the queue but not yet processed
'running  when the job is currently processed
'finished when the job has finished
*/
      (job))
{
	Lisp_Object result;

	CHECK_WORKER_JOB(job);

	lock_worker_job(XWORKER_JOB(job));
	switch (XWORKER_JOB_STATE(job)) {
	case WORKER_JOB_QUEUED:
		result = Qqueued;
		break;
	case WORKER_JOB_RUNNING:
		result = Qrunning;
		break;
	case WORKER_JOB_FINISHED:
		result = Qfinished;
		break;
	case WORKER_JOB_UNKNOWN:
	case NUMBER_OF_WORKER_JOB_STATES:
	default:
		result = Qunknown;
		break;
	}
	unlock_worker_job(XWORKER_JOB(job));

	return result;
}


void syms_of_workers(void)
{
	INIT_LRECORD_IMPLEMENTATION(worker_job);

	DEFSUBR(Fget_worker_job_result);
	DEFSUBR(Fget_worker_job_state);
	DEFSUBR(Fworker_job_p);
	DEFSUBR(Fworker_job_plist);
	defsymbol(&Qworker_jobp, "worker-job-p");

	DEFSYMBOL(Qunknown);
	DEFSYMBOL(Qrunning);
	DEFSYMBOL(Qfinished);
	DEFSYMBOL(Qqueued);
}

void vars_of_workers(void)
{
	Fprovide(intern("workers"));
}

/* workers.c ends here */
