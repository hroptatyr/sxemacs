/* Why the hell is SXEmacs so fucking slow?
   Copyright (C) 1996 Ben Wing.
   Copyright (C) 1998 Free Software Foundation, Inc.

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


#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "bytecode.h"
#include "elhash.h"
#include "hash.h"

#include "syssignal.h"
#include "systime.h"

#ifndef HAVE_SETITIMER
#error Sorry charlie.  We need a scalpel and all we have is a lawnmower.
#endif

/* We implement our own profiling scheme so that we can determine
   things like which Lisp functions are occupying the most time.  Any
   standard OS-provided profiling works on C functions, which is
   somewhat useless.

   The basic idea is simple.  We set a profiling timer using setitimer
   (ITIMER_PROF), which generates a SIGPROF every so often.  (This
   runs not in real time but rather when the process is executing or
   the system is running on behalf of the process.) When the signal
   goes off, we see what we're in, and add 1 to the count associated
   with that function.

   It would be nice to use the Lisp allocation mechanism etc. to keep
   track of the profiling information, but we can't because that's not
   safe, and trying to make it safe would be much more work than it's
   worth.

   Jan 1998: In addition to this, I have added code to remember call
   counts of Lisp funcalls.  The profile_increase_call_count()
   function is called from Ffuncall(), and serves to add data to
   Vcall_count_profile_table.  This mechanism is much simpler and
   independent of the SIGPROF-driven one.  It uses the Lisp allocation
   mechanism normally, since it is not called from a handler.  It may
   even be useful to provide a way to turn on only one profiling
   mechanism, but I haven't done so yet.  --hniksic */

static struct hash_table *big_profile_table;
Lisp_Object Vcall_count_profile_table;

Fixnum default_profiling_interval;

int profiling_active;

/* The normal flag in_display is used as a critical-section flag
   and is not set the whole time we're in redisplay. */
int profiling_redisplay_flag;

static Lisp_Object QSin_redisplay;
static Lisp_Object QSin_garbage_collection;
static Lisp_Object QSprocessing_events_at_top_level;
static Lisp_Object QSunknown;

/* We use inside_profiling to prevent the handler from writing to
   the table while another routine is operating on it.  We also set
   inside_profiling in case the timeout between signal calls is short
   enough to catch us while we're already in there. */
static volatile int inside_profiling;

/* Increase the value of OBJ in Vcall_count_profile_table hash table.
   If the hash table is nil, create it first.  */
void profile_increase_call_count(Lisp_Object obj)
{
	Lisp_Object count;

	if (NILP(Vcall_count_profile_table))
		Vcall_count_profile_table =
		    make_lisp_hash_table(100, HASH_TABLE_NON_WEAK,
					 HASH_TABLE_EQ);

	count = Fgethash(obj, Vcall_count_profile_table, Qzero);
	if (!INTP(count))
		count = Qzero;
	Fputhash(obj, make_int(1 + XINT(count)), Vcall_count_profile_table);
}

static SIGTYPE sigprof_handler(int signo)
{
	/* Don't do anything if we are shutting down, or are doing a maphash
	   or clrhash on the table. */
	if (!inside_profiling && !preparing_for_armageddon) {
		Lisp_Object fun;

		/* If something below causes an error to be signaled, we'll
		   not correctly reset this flag.  But we'll be in worse shape
		   than that anyways, since we'll longjmp back to the last
		   condition case. */
		inside_profiling = 1;

		if (profiling_redisplay_flag)
			fun = QSin_redisplay;
		else if (gc_in_progress)
			fun = QSin_garbage_collection;
		else if (backtrace_list) {
			fun = *backtrace_list->function;

			if (!SYMBOLP(fun)
			    && !COMPILED_FUNCTIONP(fun)
			    && !SUBRP(fun)
			    && !CONSP(fun))
				fun = QSunknown;
		} else
			fun = QSprocessing_events_at_top_level;

		{
			/* #### see comment about memory allocation in
			   #### start-profiling.
			   Allocating memory in a signal handler is BAD BAD BAD.
			   If you are using the non-mmap rel-alloc code, you
			   might lose because of this.  Even worse, if the
			   memory allocation fails, the `error' generated whacks
			   everything hard. */
			long count;
			void *vval;

			if (gethash(LISP_TO_VOID(fun), big_profile_table,
				    (void*)&vval)) {
				count = (long)vval;
			} else {
				count = 0;
			}
			count++;
			vval = (void *)count;
			puthash(LISP_TO_VOID(fun), vval, big_profile_table);
		}

		inside_profiling = 0;
	}
}

DEFUN("start-profiling", Fstart_profiling, 0, 1, 0,	/*
Start profiling, with profile queries every MICROSECS.
If MICROSECS is nil or omitted, the value of `default-profiling-interval'
is used.

You can retrieve the recorded profiling info using `get-profiling-info'.

Starting and stopping profiling does not clear the currently recorded
info.  Thus you can start and stop as many times as you want and everything
will be properly accumulated.
*/
      (microsecs))
{
	/* This function can GC */
	int msecs;
	struct itimerval foo;

	/* #### The hash code can safely be called from a signal handler
	   except when it has to grow the hash table.  In this case, it calls
	   realloc(), which is not (in general) re-entrant.  We'll just be
	   sleazy and make the table large enough that it (hopefully) won't
	   need to be realloc()ed. */
	if (!big_profile_table)
		big_profile_table = make_hash_table(10000);

	if (NILP(microsecs))
		msecs = default_profiling_interval;
	else {
		CHECK_NATNUM(microsecs);
		msecs = XINT(microsecs);
	}
	if (msecs <= 0)
		msecs = 1000;

	signal(SIGPROF, sigprof_handler);
	foo.it_value.tv_sec = 0;
	foo.it_value.tv_usec = msecs;
	EMACS_NORMALIZE_TIME(foo.it_value);
	foo.it_interval = foo.it_value;
	profiling_active = 1;
	inside_profiling = 0;
	qxe_setitimer(ITIMER_PROF, &foo, 0);
	return Qnil;
}

DEFUN("stop-profiling", Fstop_profiling, 0, 0, 0,	/*
Stop profiling.
*/
      ())
{
	/* This function does not GC */
	struct itimerval foo;

	foo.it_value.tv_sec = 0;
	foo.it_value.tv_usec = 0;
	foo.it_interval = foo.it_value;
	qxe_setitimer(ITIMER_PROF, &foo, 0);
	profiling_active = 0;
	signal(SIGPROF, fatal_error_signal);
	return Qnil;
}

static Lisp_Object profile_lock_unwind(Lisp_Object ignore)
{
	inside_profiling = 0;
	return Qnil;
}

struct get_profiling_info_closure {
	Lisp_Object accum;
};

static int
get_profiling_info_maphash(const void *void_key,
			   void *void_val, void *void_closure)
{
	/* This function does not GC */
	Lisp_Object key;
	struct get_profiling_info_closure *closure
	    = (struct get_profiling_info_closure *)void_closure;
	EMACS_INT val;

	CVOID_TO_LISP(key, void_key);
	val = (EMACS_INT) void_val;

	closure->accum = Fcons(Fcons(key, make_int(val)), closure->accum);
	return 0;
}

DEFUN("get-profiling-info", Fget_profiling_info, 0, 0, 0,	/*
Return the profiling info as an alist.
*/
      ())
{
	/* This function does not GC */
	struct get_profiling_info_closure closure;

	closure.accum = Qnil;
	if (big_profile_table) {
		int count = specpdl_depth();
		record_unwind_protect(profile_lock_unwind, Qnil);
		inside_profiling = 1;
		maphash(get_profiling_info_maphash, big_profile_table,
			&closure);
		unbind_to(count, Qnil);
	}
	return closure.accum;
}

static int
mark_profiling_info_maphash(const void *void_key,
			    void *void_val, void *void_closure)
{
	Lisp_Object key;

	CVOID_TO_LISP(key, void_key);
	mark_object(key);
	return 0;
}

void mark_profiling_info(void)
{
	/* This function does not GC */
	if (big_profile_table) {
		inside_profiling = 1;
		maphash(mark_profiling_info_maphash, big_profile_table, 0);
		inside_profiling = 0;
	}
}

DEFUN("clear-profiling-info", Fclear_profiling_info, 0, 0, "",	/*
Clear out the recorded profiling info.
*/
      ())
{
	/* This function does not GC */
	if (big_profile_table) {
		inside_profiling = 1;
		clrhash(big_profile_table);
		inside_profiling = 0;
	}
	if (!NILP(Vcall_count_profile_table))
		Fclrhash(Vcall_count_profile_table);
	return Qnil;
}

DEFUN("profiling-active-p", Fprofiling_active_p, 0, 0, 0,	/*
Return non-nil if profiling information is currently being recorded.
*/
      ())
{
	return profiling_active ? Qt : Qnil;
}

void syms_of_profile(void)
{
	DEFSUBR(Fstart_profiling);
	DEFSUBR(Fstop_profiling);
	DEFSUBR(Fget_profiling_info);
	DEFSUBR(Fclear_profiling_info);
	DEFSUBR(Fprofiling_active_p);
}

void vars_of_profile(void)
{
	DEFVAR_INT("default-profiling-interval", &default_profiling_interval	/*
										   Default CPU time in microseconds between profiling sampling.
										   Used when the argument to `start-profiling' is nil or omitted.
										   Note that the time in question is CPU time (when the program is executing
										   or the kernel is executing on behalf of the program) and not real time.
										 */ );
	default_profiling_interval = 1000;

	DEFVAR_LISP("call-count-profile-table", &Vcall_count_profile_table	/*
										   The table where call-count information is stored by the profiling primitives.
										   This is a hash table whose keys are funcallable objects, and whose
										   values are their call counts (integers).
										 */ );
	Vcall_count_profile_table = Qnil;

	inside_profiling = 0;

	QSin_redisplay = build_string("(in redisplay)");
	staticpro(&QSin_redisplay);
	QSin_garbage_collection = build_string("(in garbage collection)");
	staticpro(&QSin_garbage_collection);
	QSunknown = build_string("(unknown)");
	staticpro(&QSunknown);
	QSprocessing_events_at_top_level =
	    build_string("(processing events at top level)");
	staticpro(&QSprocessing_events_at_top_level);
}
