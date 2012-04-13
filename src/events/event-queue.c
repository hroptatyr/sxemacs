/*
  event-queue.c -- New Generation Event Queue
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

#include <config.h>
#include "lisp.h"
#include "event-queue.h"
#define INCLUDE_EVENTS_H_PRIVATE_SPHERE
#include "events.h"


static void
finalise_event_queue(void *obj, int for_disksave)
{
	event_queue_t eq = obj;

	if (eq == NULL || for_disksave) {
		return;
	}

	SXE_SEMAPH_FINI(&(eq_queue_sem(eq)));
	free_noseeum_dllist(eq_queue(eq));
	return;
}

static Lisp_Object
mark_event_queue(Lisp_Object obj)
{
	event_queue_t eq = XEVENT_QUEUE(obj);
	WITH_DLLIST_TRAVERSE(
		eq_queue(eq),
		if (dllist_item)
			mark_object((Lisp_Object)dllist_item));
	return Qnil;
}

event_queue_t
make_noseeum_event_queue(void)
{
	event_queue_t res = xnew(struct event_queue_s);

	SXE_SEMAPH_INIT(&(eq_queue_sem(res)));
	eq_queue(res) = make_noseeum_dllist();

	return res;
}

static inline event_queue_t
allocate_event_queue(void)
{
	event_queue_t res =
		alloc_lcrecord_type(struct event_queue_s, &lrecord_event_queue);
	return res;
}

event_queue_t
make_event_queue(void)
{
	event_queue_t res = allocate_event_queue();

	SXE_SEMAPH_INIT(&(eq_queue_sem(res)));
	eq_queue(res) = make_noseeum_dllist();

	return res;
}

void
free_event_queue(event_queue_t eq)
{
	finalise_event_queue(eq, 0);
	xfree(eq);
	return;
}

DEFINE_LRECORD_IMPLEMENTATION("event-queue", event_queue,
			      mark_event_queue, NULL,
			      finalise_event_queue,
			      NULL, NULL, 0, struct event_queue_s);

/* priques */
static void
finalise_event_prique(void *obj, int for_disksave)
{
	event_prique_t ep = obj;
	if (ep == NULL)
		return;

	SXE_SEMAPH_FINI(&(eq_prique_sem(ep)));
	free_noseeum_dllist(eq_prique(ep));
	return;
}

static Lisp_Object
mark_event_prique(Lisp_Object obj)
{
	return Qnil;
}

event_prique_t
make_event_prique(void)
{
	event_prique_t res = xnew(struct event_prique_s);

	SXE_SEMAPH_INIT(&(eq_prique_sem(res)));
	eq_prique(res) = NULL;

	return res;
}

void
free_event_prique(event_prique_t eq)
{
	finalise_event_prique(eq, 0);
	xfree(eq);
	return;
}

DEFINE_LRECORD_IMPLEMENTATION("event-prique", event_prique,
			      mark_event_prique, NULL,
			      finalise_event_prique,
			      NULL, NULL, 0, struct event_prique_s);

/* stuff with private data aboard */
extern void
eq_enqueue_event_chain(event_queue_t eq, Lisp_Object ec)
{
	while (!NILP(ec)) {
		eq_enqueue(eq, ec);
		ec = XEVENT_NEXT(ec);
	}
	return;
}



/************************************************************************/
/*                            initialisation                            */
/************************************************************************/

void syms_of_event_queue(void)
{
	INIT_LRECORD_IMPLEMENTATION(event_queue);
	INIT_LRECORD_IMPLEMENTATION(event_prique);
}

/* event-queue.c ends here */
