/*** worker-asyneq.h -- worker threads for asyneq feature
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

/* Inspired by XEmacs' events.c written by Jamie Zawinski */

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_worker_asyneq_h_
#define INCLUDED_worker_asyneq_h_

#include "semaphore.h"
#include "workers.h"
#include "event-queue.h"

extern event_queue_t delegate_eq;


extern void init_workers(int nthreads, sxe_thread_f handler);
extern void fini_worker(eq_worker_t);

extern void eq_worker_eaten_myself(eq_worker_t eqw);
extern void eq_worker_work_started(Lisp_Object job);
extern void eq_worker_work_finished(Lisp_Object job);
extern void eq_delegate_work(event_queue_t eq);

extern void asyneq_handle_event(event_queue_t);
extern void asyneq_handle_non_command_event(event_queue_t eq);

#endif	/* INCLUDED_workers_h_ */
