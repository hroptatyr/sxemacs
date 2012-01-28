/* Copyright (C) 1985, 86, 87, 93, 94, 96 Free Software Foundation, Inc.

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


/* Synced with FSF 20.2 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include <sxe-paths.h>

#include "sysfile.h"
#include "sysdir.h"
#include "syspwd.h"
#include "syssignal.h"		/* for kill */

Lisp_Object Qask_user_about_supersession_threat;
Lisp_Object Qask_user_about_lock;
int inhibit_clash_detection;

#ifdef CLASH_DETECTION

/* The strategy: to lock a file FN, create a symlink .#FN in FN's
   directory, with link data `user@host.pid'.  This avoids a single
   mount (== failure) point for lock files.

   When the host in the lock data is the current host, we can check if
   the pid is valid with kill.

   Otherwise, we could look at a separate file that maps hostnames to
   reboot times to see if the remote pid can possibly be valid, since we
   don't want Emacs to have to communicate via pipes or sockets or
   whatever to other processes, either locally or remotely; rms says
   that's too unreliable.  Hence the separate file, which could
   theoretically be updated by daemons running separately -- but this
   whole idea is unimplemented; in practice, at least in our
   environment, it seems such stale locks arise fairly infrequently, and
   Emacs' standard methods of dealing with clashes suffice.

   We use symlinks instead of normal files because (1) they can be
   stored more efficiently on the filesystem, since the kernel knows
   they will be small, and (2) all the info about the lock can be read
   in a single system call (readlink).  Although we could use regular
   files to be useful on old systems lacking symlinks, nowadays
   virtually all such systems are probably single-user anyway, so it
   didn't seem worth the complication.

   Similarly, we don't worry about a possible 14-character limit on
   file names, because those are all the same systems that don't have
   symlinks.

   This is compatible with the locking scheme used by Interleaf (which
   has contributed this implementation for Emacs), and was designed by
   Ethan Jacobson, Kimbo Mundy, and others.

   --karl@cs.umb.edu/karl@hq.ileaf.com.  */

/* Note that muleization is provided by using mule-encapsulated
   versions of the system calls we use like symlink(), unlink(), etc... */

/* Here is the structure that stores information about a lock.  */

typedef struct {
	char *user;
	char *host;
	unsigned long pid;
} lock_info_type;

/* When we read the info back, we might need this much more,
   enough for decimal representation plus null.  */
#define LOCK_PID_MAX (4 * sizeof (unsigned long))

/* Free the two dynamically-allocated pieces in PTR.  */
#define FREE_LOCK_INFO(i) do { xfree ((i).user); xfree ((i).host); } while (0)

/* Write the name of the lock file for FN into LFNAME.  Length will be
   that of FN plus two more for the leading `.#' plus one for the null.  */
#define MAKE_LOCK_NAME(lock, file) \
  (lock = (char *) alloca (XSTRING_LENGTH (file) + 2 + 1), \
   fill_in_lock_file_name ((Bufbyte *) (lock), (file)))

static void fill_in_lock_file_name(Bufbyte * lockfile, Lisp_Object fn)
{
	Bufbyte *file_name = XSTRING_DATA(fn);
	Bufbyte *p;
	size_t dirlen;

	for (p = file_name + XSTRING_LENGTH(fn) - 1;
	     p > file_name && !IS_ANY_SEP(p[-1]); p--) ;
	dirlen = p - file_name;

	memcpy(lockfile, file_name, dirlen);
	p = lockfile + dirlen;
	*(p++) = '.';
	*(p++) = '#';
	memcpy(p, file_name + dirlen, XSTRING_LENGTH(fn) - dirlen + 1);
}

/* Lock the lock file named LFNAME.
   If FORCE is nonzero, we do so even if it is already locked.
   Return 1 if successful, 0 if not.  */

static int lock_file_1(char *lfname, int force)
{
	/* Does not GC. */
	int err;
	char *lock_info_str;
	char *host_name;
	char *user_name = user_login_name(NULL);
	int sz, maxlen;

	if (user_name == NULL)
		user_name = "";

	if (STRINGP(Vsystem_name))
		host_name = (char *)XSTRING_DATA(Vsystem_name);
	else
		host_name = "";

	maxlen = strlen(user_name) + strlen(host_name)
		+ LOCK_PID_MAX + 5;
	lock_info_str = (char *)alloca(maxlen);

	sz = snprintf(lock_info_str, maxlen, "%s@%s.%lu", user_name, host_name,
		      (unsigned long)getpid());
	assert(sz>=0 && sz < maxlen);

	err = symlink(lock_info_str, lfname);
	if (err != 0 && errno == EEXIST && force) {
		unlink(lfname);
		err = symlink(lock_info_str, lfname);
	}

	return err == 0;
}

/* Return 0 if nobody owns the lock file LFNAME or the lock is obsolete,
   1 if another process owns it (and set OWNER (if non-null) to info),
   2 if the current process owns it,
   or -1 if something is wrong with the locking mechanism.  */

static int current_lock_owner(lock_info_type * owner, char *lfname)
{
	/* Does not GC. */
	int len, ret;
	int local_owner = 0;
	char *at, *dot;
	char *lfinfo = 0;
	int bufsize = 50;
	/* Read arbitrarily-long contents of symlink.  Similar code in
	   file-symlink-p in fileio.c.  */
	do {
		bufsize *= 2;
		lfinfo = (char *)xrealloc(lfinfo, bufsize);
		len = readlink(lfname, lfinfo, bufsize);
	}
	while (len >= bufsize);

	/* If nonexistent lock file, all is well; otherwise, got strange error. */
	if (len == -1) {
		xfree(lfinfo);
		return errno == ENOENT ? 0 : -1;
	}

	/* Link info exists, so `len' is its length.  Null terminate.  */
	lfinfo[len] = 0;

	/* Even if the caller doesn't want the owner info, we still have to
	   read it to determine return value, so allocate it.  */
	if (!owner) {
		owner = (lock_info_type *) alloca(sizeof(lock_info_type));
		local_owner = 1;
	}

	/* Parse USER@HOST.PID.  If can't parse, return -1.  */
	/* The USER is everything before the first @.  */
	at = strchr(lfinfo, '@');
	dot = strrchr(lfinfo, '.');
	if (!at || !dot) {
		xfree(lfinfo);
		return -1;
	}
	len = at - lfinfo;
	owner->user = (char *)xmalloc_atomic(len + 1);
	strncpy(owner->user, lfinfo, len);
	owner->user[len] = 0;

	/* The PID is everything after the last `.'.  */
	owner->pid = atoi(dot + 1);

	/* The host is everything in between.  */
	len = dot - at - 1;
	owner->host = (char *)xmalloc_atomic(len + 1);
	strncpy(owner->host, at + 1, len);
	owner->host[len] = 0;

	/* We're done looking at the link info.  */
	xfree(lfinfo);

	/* On current host?  */
	if (STRINGP(Fsystem_name())
	    && strcmp(owner->host, (char *)XSTRING_DATA(Fsystem_name())) == 0) {
		if (owner->pid == (unsigned long)getpid())
			ret = 2;	/* We own it.  */
		else if (owner->pid > 0
			 && (kill(owner->pid, 0) >= 0 || errno == EPERM))
			ret = 1;	/* An existing process on this machine owns it.  */
		/* The owner process is dead or has a strange pid (<=0), so try to
		   zap the lockfile.  */
		else if (unlink(lfname) < 0)
			ret = -1;
		else
			ret = 0;
	} else {		/* If we wanted to support the check for stale locks on remote machines,
				   here's where we'd do it.  */
		ret = 1;
	}

	/* Avoid garbage.  */
	if (local_owner || ret <= 0) {
		FREE_LOCK_INFO(*owner);
	}
	return ret;
}

/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return positive if some other process owns the lock, and info about
     that process in CLASHER.
   Return -1 if cannot lock for any other reason.  */

static int lock_if_free(lock_info_type * clasher, char *lfname)
{
	/* Does not GC. */
	if (lock_file_1(lfname, 0) == 0) {
		int locker;

		if (errno != EEXIST)
			return -1;

		locker = current_lock_owner(clasher, lfname);
		if (locker == 2) {
			FREE_LOCK_INFO(*clasher);
			return 0;	/* We ourselves locked it.  */
		} else if (locker == 1)
			return 1;	/* Someone else has it.  */

		return -1;	/* Something's wrong.  */
	}
	return 0;
}

/* lock_file locks file FN,
   meaning it serves notice on the world that you intend to edit that file.
   This should be done only when about to modify a file-visiting
   buffer previously unmodified.
   Do not (normally) call this for a buffer already modified,
   as either the file is already locked, or the user has already
   decided to go ahead without locking.

   When this returns, either the lock is locked for us,
   or the user has said to go ahead without locking.

   If the file is locked by someone else, this calls
   ask-user-about-lock (a Lisp function) with two arguments,
   the file name and info about the user who did the locking.
   This function can signal an error, or return t meaning
   take away the lock, or return nil meaning ignore the lock.  */

void lock_file(Lisp_Object fn)
{
	/* This function can GC.  GC checked 7-11-00 ben */
	/* dmoore - and can destroy current_buffer and all sorts of other
	   mean nasty things with pointy teeth.  If you call this make sure
	   you protect things right. */
	/* Somebody updated the code in this function and removed the previous
	   comment.  -slb */

	register Lisp_Object attack, orig_fn;
	register char *lfname, *locker;
	lock_info_type lock_info;
	struct gcpro gcpro1, gcpro2, gcpro3;
	Lisp_Object old_current_buffer;
	Lisp_Object subject_buf;
	int sz, max_sz;

	if (inhibit_clash_detection)
		return;

	XSETBUFFER(old_current_buffer, current_buffer);
	subject_buf = Qnil;
	GCPRO3(fn, subject_buf, old_current_buffer);
	orig_fn = fn;
	fn = Fexpand_file_name(fn, Qnil);

	/* Create the name of the lock-file for file fn */
	MAKE_LOCK_NAME(lfname, fn);

	/* See if this file is visited and has changed on disk since it was
	   visited.  */
	{
		subject_buf = get_truename_buffer(orig_fn);
		if (!NILP(subject_buf)
		    && NILP(Fverify_visited_file_modtime(subject_buf))
		    && !NILP(Ffile_exists_p(fn)))
			call1_in_buffer(XBUFFER(subject_buf),
					Qask_user_about_supersession_threat,
					fn);
	}

	/* Try to lock the lock. */
	if (current_buffer != XBUFFER(old_current_buffer)
	    || lock_if_free(&lock_info, lfname) <= 0)
		/* Return now if we have locked it, or if lock creation failed
		   or current buffer is killed. */
		goto done;

	/* Else consider breaking the lock */
	max_sz = strlen(lock_info.user) + strlen(lock_info.host)
		+ LOCK_PID_MAX + 9;
	locker = (char *)alloca(max_sz);
	sz = snprintf(locker, max_sz, "%s@%s (pid %lu)",
		      lock_info.user, lock_info.host,
		      lock_info.pid);
	assert(sz>=0 && sz < max_sz);
	FREE_LOCK_INFO(lock_info);

	attack = call2_in_buffer(BUFFERP(subject_buf) ? XBUFFER(subject_buf) :
				 current_buffer, Qask_user_about_lock, fn,
				 build_string(locker));
	if (!NILP(attack) && current_buffer == XBUFFER(old_current_buffer))
		/* User says take the lock */
	{
		lock_file_1(lfname, 1);
		goto done;
	}
	/* User says ignore the lock */
      done:
	UNGCPRO;
}

void unlock_file(Lisp_Object fn)
{
	/* This can GC */
	register char *lfname;
	struct gcpro gcpro1;

	GCPRO1(fn);

	fn = Fexpand_file_name(fn, Qnil);

	MAKE_LOCK_NAME(lfname, fn);

	if (current_lock_owner(0, lfname) == 2)
		unlink(lfname);

	UNGCPRO;
}

void unlock_all_files(void)
{
	register Lisp_Object tail;

	for (tail = Vbuffer_alist; CONSP(tail); tail = XCDR(tail)) {
		struct buffer *b = XBUFFER(XCDR(XCAR(tail)));
		if (STRINGP(b->file_truename)
		    && BUF_SAVE_MODIFF(b) < BUF_MODIFF(b))
			unlock_file(b->file_truename);
	}
}

DEFUN("lock-buffer", Flock_buffer, 0, 1, 0,	/*
Lock FILE, if current buffer is modified.
FILE defaults to current buffer's visited file,
or else nothing is done if current buffer isn't visiting a file.
*/
      (file))
{
	if (NILP(file))
		file = current_buffer->file_truename;
	CHECK_STRING(file);
	if (BUF_SAVE_MODIFF(current_buffer) < BUF_MODIFF(current_buffer)
	    && !NILP(file))
		lock_file(file);
	return Qnil;
}

DEFUN("unlock-buffer", Funlock_buffer, 0, 0, 0,	/*
Unlock the file visited in the current buffer,
if it should normally be locked.
*/
      ())
{
	/* This function can GC */
	/* dmoore - and can destroy current_buffer and all sorts of other
	   mean nasty things with pointy teeth.  If you call this make sure
	   you protect things right. */

	if (BUF_SAVE_MODIFF(current_buffer) < BUF_MODIFF(current_buffer)
	    && STRINGP(current_buffer->file_truename))
		unlock_file(current_buffer->file_truename);
	return Qnil;
}

/* Unlock the file visited in buffer BUFFER.  */

void unlock_buffer(struct buffer *buffer)
{
	/* This function can GC */
	/* dmoore - and can destroy current_buffer and all sorts of other
	   mean nasty things with pointy teeth.  If you call this make sure
	   you protect things right. */
	if (BUF_SAVE_MODIFF(buffer) < BUF_MODIFF(buffer)
	    && STRINGP(buffer->file_truename))
		unlock_file(buffer->file_truename);
}

DEFUN("file-locked-p", Ffile_locked_p, 0, 1, 0,	/*
Return nil if the FILENAME is not locked,
t if it is locked by you, else a string of the name of the locker.
*/
      (filename))
{
	Lisp_Object ret;
	register char *lfname;
	int owner;
	lock_info_type locker;
	struct gcpro gcpro1;

	GCPRO1(filename);

	filename = Fexpand_file_name(filename, Qnil);

	MAKE_LOCK_NAME(lfname, filename);

	owner = current_lock_owner(&locker, lfname);
	if (owner <= 0)
		ret = Qnil;
	else if (owner == 2)
		ret = Qt;
	else
		ret = build_string(locker.user);

	if (owner > 0)
		FREE_LOCK_INFO(locker);

	UNGCPRO;

	return ret;
}

/* Initialization functions.  */

void syms_of_filelock(void)
{
	/* This function can GC */
	DEFSUBR(Funlock_buffer);
	DEFSUBR(Flock_buffer);
	DEFSUBR(Ffile_locked_p);

	defsymbol(&Qask_user_about_supersession_threat,
		  "ask-user-about-supersession-threat");
	defsymbol(&Qask_user_about_lock, "ask-user-about-lock");
}

void vars_of_filelock(void)
{
	DEFVAR_BOOL("inhibit-clash-detection", &inhibit_clash_detection	/*
Non-nil inhibits creation of lock file to detect clash.
									 */ );
	inhibit_clash_detection = 0;
}

#endif				/* CLASH_DETECTION */
