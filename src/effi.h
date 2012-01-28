/*
 * effi.h --- Header for Foreign Function Interface.
 *
 * Copyright (C) 2004 Evgeny Zajcev.
 * Copyright (C) 2008 Steve Youngs.
 *
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

#ifndef _SXEMACS_EFFI_H_
#define _SXEMACS_EFFI_H_

#ifndef FFI_STACK_SIZE
#define FFI_STACK_SIZE 4096
#endif  /* FFI_STACK_SIZE */

/** Errors **/
#define EFFI_OK  0
#define EFFI_ARG 1
#define EFFI_ERR 2

struct Lisp_EffiObject {
#ifdef SXEMACS
	struct lcrecord_header header;
#else
	struct old_lcrecord_header header;
#endif	/* SXEMACS */
	Lisp_Object type;               /* type of ffi object */
	Lisp_Object size;               /* size of ffi object type */

	Lisp_Object plist;              /* properties list */

	/* Foreign stuff */
	int fotype;
#define EFFI_FOT_NONE  0                /* fop is not used */
#define EFFI_FOT_BIND  2                /* fop is reference to foreign data */
#define EFFI_FOT_FUNC  3                /* fop is pointer to function */

	/*
	 * Declared as union just for the style, there no problem to
	 * always use fop.generic to access pointer.
	 */
	union {
		void *ptr;              /* pointer to foreign data */
		void *fun;              /* pointer to foreign function */
		void *generic;          /* generic storer */
	} fop;

	size_t storage_size;            /* size of storage */
	char fostorage[0];              /* storage */
};

#define EFFIO_HAS_FOP(fo) ((fo)->fotype != EFFI_FOT_NONE)

typedef struct Lisp_EffiObject Lisp_EffiObject;

DECLARE_LRECORD(ffiobject, Lisp_EffiObject);
#define XEFFIO(x) XRECORD (x, ffiobject, Lisp_EffiObject)
#ifdef SXEMACS
#  define XSETEFFIO(x, p) XSETRECORD (x, p, ffiobject)
#else
#  define wrap_effio(p) wrap_record (p, ffiobject)
#endif	/* SXEMACS */
#define EFFIOP(x) RECORDP (x, ffiobject)
#define CHECK_EFFIO(x) CHECK_RECORD (x, ffiobject)
#define CONCHECK_EFFIO(x) CONCHECK_RECORD (x, ffiobject)

EXFUN(Fffi_slot_offset, 2);
EXFUN(Fffi_size_of_type, 1);
EXFUN(Fffi_plist, 1);

#ifdef EF_USE_ASYNEQ
#include "events/event-queue.h"

typedef struct ffi_job_s *ffi_job_t;
struct ffi_job_s {
	sxe_mutex_t mtx;
	event_queue_t queue;

	/* the foreign function and its args */
	Lisp_Object fof;
	int fof_nargs;
	Lisp_Object *fof_args;

	/* the sentinel and its args */
	Lisp_Object sntnl;
	int sntnl_nargs;
	Lisp_Object *sntnl_args;

	/* the result fo */
	Lisp_Object retfo;
	Lisp_Object result;
};

#define ffi_job(_x)		(((ffi_job_t)worker_job_data(_x)))
#define ffi_job_sentinel(_x)	(ffi_job(_x)->sntnl)
#define ffi_job_result(_x)	(ffi_job(_x)->result)
#define XFFI_JOB(_x)		((ffi_job_t)(XWORKER_JOB_DATA(_x)))
#define FFI_JOBP(_x)							\
	(WORKER_JOBP(_x) && XWORKER_JOB_HANDLER(_x) == &ffi_job_handler)
#define CHECK_FFI_JOB(_x)						\
	do {								\
		if (!FFI_JOBP(_x))					\
			dead_wrong_type_argument(Qffi_jobp, _x);	\
	} while (0)
#define CONCHECK_FFI_JOB(_x)						\
	do {								\
		if (!FFI_JOBP(_x))					\
			return wrong_type_argument(Qffi_jobp, _x);	\
	} while (0)

#endif	/* EF_USE_ASYNEQ */

#endif /* _SXEMACS_FFI_H_ */
