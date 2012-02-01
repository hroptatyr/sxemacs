/* Definitions for asynchronous process control in SXEmacs.
   Copyright (C) 1985, 1992, 1993, 1994 Free Software Foundation, Inc.

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


#ifndef INCLUDED_process_h_
#define INCLUDED_process_h_

#if defined (NO_SUBPROCESSES)
#undef XPROCESS
#undef CHECK_PROCESS
#undef XSETPROCESS
#define PROCESSP(x) 0
#define PROCESS_LIVE_P(x) 0
#define Fprocess_status(x) Qnil
#define Fget_process(x) Qnil
#define Fget_buffer_process(x) Qnil
#define kill_buffer_processes(x) 0
#define close_process_descs() 0
#define init_sxemacs_process() 0
void wait_without_blocking(void);

#else				/* not NO_SUBPROCESSES */

/* struct Lisp_Process is defined in procimpl.h; only process-*.c need
   to know about the guts of it. */

enum PROCESS_TYPES {
	PROCESS_TYPE_PROC,
	PROCESS_TYPE_NETWORK,
	PROCESS_TYPE_MULTICAST,
	PROCESS_TYPE_SSL,
	PROCESS_TYPE_NETWORK_SERVER_LISTEN,
	/* last entry holds the number of process types */
	PROCESS_TYPES_COUNT
};

DECLARE_LRECORD(process, Lisp_Process);
#define XPROCESS(x) XRECORD (x, process, Lisp_Process)
#define XSETPROCESS(x, p) XSETRECORD (x, p, process)
#define PROCESSP(x) RECORDP (x, process)
#define CHECK_PROCESS(x) CHECK_RECORD (x, process)
#define PROCESS_LIVE_P(x) (EQ ((x)->status_symbol, Qrun))
#define PROCESS_READABLE_P(x) (!NILP ((x)->pipe_instream))

#define CHECK_LIVE_PROCESS(x) do {			\
  CHECK_PROCESS (x);					\
  if (! PROCESS_LIVE_P (XPROCESS (x)))			\
    dead_wrong_type_argument (Qprocess_live_p, (x));	\
} while (0)

#define CHECK_READABLE_PROCESS(x) do {			\
  CHECK_PROCESS (x);					\
  if (! PROCESS_READABLE_P (XPROCESS (x)))		\
    dead_wrong_type_argument (Qprocess_readable_p, (x));	\
} while (0)

#ifdef emacs

EXFUN(Fprocess_kill_without_query, 2);
EXFUN(Fprocess_id, 1);

Lisp_Object connect_to_file_descriptor(Lisp_Object name,
				       Lisp_Object buffer,
				       Lisp_Object infd, Lisp_Object outfd);
int connected_via_filedesc_p(Lisp_Process * p);
void kill_buffer_processes(Lisp_Object buffer);
void close_process_descs(void);

void set_process_filter(Lisp_Object proc,
			Lisp_Object filter, int filter_does_read);

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
extern volatile int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
extern const char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
extern int synch_process_retcode;

void update_process_status(Lisp_Object p,
			   Lisp_Object status_symbol,
			   int exit_code, int core_dumped);

void get_process_streams(Lisp_Process * p,
			 Lisp_Object * instr, Lisp_Object * outstr);
int get_process_selected_p(Lisp_Process * p);
void set_process_selected_p(Lisp_Process * p, int selected_p);

Lisp_Process *get_process_from_usid(USID usid);

#ifdef HAVE_SOCKETS
int network_connection_p(Lisp_Object process);
#else
#define network_connection_p(x) 0
#endif

extern Lisp_Object Qclosed, Qmulticast, Qopen, Qrun, Qstop, Qtcp, Qudp;
extern Lisp_Object Vprocess_connection_type, Vprocess_list;

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is done while Emacs is waiting for keyboard input.  */
void status_notify(void);
void kick_status_notify(void);

void deactivate_process(Lisp_Object proc);

void
 child_setup(int in, int out, int err,
	     char **new_argv, const char *current_dir);

Charcount read_process_output(Lisp_Object proc);

const char *signal_name(int signum);

Lisp_Object canonicalize_host_name(Lisp_Object host);

#endif				/* not NO_SUBPROCESSES */

/* The name of the file open to get a null file, or a data sink.
   MS-DOS, and OS/2 redefine this.  */
#ifndef NULL_DEVICE
#define NULL_DEVICE "/dev/null"
#endif

/* A string listing the possible suffixes used for executable files,
   separated by colons.  MS-DOS, and OS/2 redefine this.  */
#ifndef EXEC_SUFFIXES
#define EXEC_SUFFIXES ""
#endif

#endif				/* emacs */

#endif				/* INCLUDED_process_h_ */
