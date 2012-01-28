/*
  event-queue.h -- New Generation Event Queue
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

#ifndef INCLUDED_event_queue_h_
#define INCLUDED_event_queue_h_

#include "semaphore.h"

#ifdef ALL_DEBUG_FLAGS
#undef EQUEUE_DEBUG_FLAG
#define EQUEUE_DEBUG_FLAG
#endif

#define __EQUEUE_DEBUG__(args...)	fprintf(stderr, "event-queue " args)
#ifndef EQUEUE_DEBUG_FLAG
#define EQUEUE_DEBUG(args...)
#else
#define EQUEUE_DEBUG(args...)		__EQUEUE_DEBUG__(args)
#endif
#define EQUEUE_CRITICAL(args...)	__EQUEUE_DEBUG__("CRITICAL: " args)
#define EQUEUE_DEBUG_WORKER(args...)	EQUEUE_DEBUG("[worker] " args)
#define EQUEUE_DEBUG_SCRATCH(args...)	EQUEUE_DEBUG("[scratch] " args)

typedef struct event_queue_s *event_queue_t;
typedef struct event_prique_s *event_prique_t;

/* a prominent instance */
extern event_queue_t asyneq;


/* ordinary FIFO-queue */
struct event_queue_s {
	struct lcrecord_header lheader;
	struct sxe_semaphore_s queue_sem;
	dllist_t queue;
};

/* priority queue, pronounced: prick :) */
struct event_prique_s {
	struct lcrecord_header lheader;
	struct sxe_semaphore_s prique_sem;
	/* since ase's heap implementation is not global we use a void* here */
	void *prique;
};

DECLARE_LRECORD(event_queue, struct event_queue_s);
#define eq_queue_sem(_x)	((_x)->queue_sem)
#define eq_queue(_x)		((_x)->queue)
#define XEVENT_QUEUE(x)		XRECORD(x, event_queue, struct event_queue_s)
#define XSETEVENT_QUEUE(x, p)	XSETRECORD(x, p, event_queue)
#define EVENT_QUEUE_P(x)	RECORDP(x, event_queue)

DECLARE_LRECORD(event_prique, struct event_prique_s);
#define eq_prique_sem(_x)	((_x)->prique_sem)
#define eq_prique(_x)		((_x)->prique)
#define XEVENT_PRIQUE(x)	XRECORD(x, event_prique, struct event_prique_s)
#define XSETEVENT_PRIQUE(x, p)	XSETRECORD(x, p, event_prique)
#define EVENT_PRIQUE_P(x)	RECORDP(x, event_prique)


extern event_queue_t make_noseeum_event_queue(void);
extern event_queue_t make_event_queue(void);
extern void free_event_queue(event_queue_t);
extern event_prique_t make_event_prique(void);
extern void free_event_prique(event_prique_t);

extern_inline void eq_enqueue(event_queue_t, Lisp_Object);
extern_inline Lisp_Object eq_dequeue(event_queue_t);
extern_inline void eq_noseeum_enqueue(event_queue_t, void*);
extern_inline void *eq_noseeum_dequeue(event_queue_t);
extern void eq_enqueue_event_chain(event_queue_t, Lisp_Object);
extern_inline size_t eq_queue_size(event_queue_t);
extern_inline int eq_queue_empty_p(event_queue_t);
extern_inline void eq_queue_synch(event_queue_t);
extern_inline void eq_queue_trigger(event_queue_t);
extern_inline void eq_queue_trigger_all(event_queue_t);

/* special purpose */
extern_inline void eq_dequeue_pro(Lisp_Object*, event_queue_t);


extern_inline void
eq_enqueue(event_queue_t eq, Lisp_Object ev)
{
	dllist_append(eq_queue(eq), (void*)ev);
	return;
}

extern_inline Lisp_Object
eq_dequeue(event_queue_t eq)
{
	return (Lisp_Object)(Lisp_Object*)dllist_pop_car(eq_queue(eq));
}

extern_inline void
eq_dequeue_pro(Lisp_Object *result, event_queue_t eq)
{
	dllist_pop_and_pro_car(result, eq_queue(eq));
	return;
}

extern_inline void
eq_noseeum_enqueue(event_queue_t eq, void *ev)
{
	dllist_append(eq_queue(eq), ev);
	return;
}

extern_inline void*
eq_noseeum_dequeue(event_queue_t eq)
{
	return (void*)dllist_pop_car(eq_queue(eq));
}

extern_inline size_t
eq_queue_size(event_queue_t eq)
{
	return dllist_get_size(eq_queue(eq));
}

extern_inline int
eq_queue_empty_p(event_queue_t eq)
{
	return eq_queue_size(eq) == 0;
}

#define RETURN_FROM_EQ_TRAVERSE(_eq, _retval)			\
	RETURN_FROM_DLLIST_TRAVERSE(eq_queue(_eq), _retval)
#define EQ_TRAVERSE(_eq, _var, args...)				\
	WITH_DLLIST_TRAVERSE(					\
		eq_queue(_eq),					\
		_var = (Lisp_Object)dllist_item;		\
		args;						\
		)

extern_inline void
eq_queue_synch(event_queue_t eq)
{
	SXE_SEMAPH_SYNCH(&eq_queue_sem(eq));
}

extern_inline void
eq_queue_trigger(event_queue_t eq)
{
	SXE_SEMAPH_TRIGGER(&eq_queue_sem(eq));
}

extern_inline void
eq_queue_trigger_all(event_queue_t eq)
{
	SXE_SEMAPH_TRIGGER_ALL(&eq_queue_sem(eq));
}

#endif	/* INCLUDED_event_queue_h_ */
