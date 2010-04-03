/** postgresql.c -- elisp binding to libpq.so
 *
 * Copyright (C) 2000 Electrotechnical Laboratory, JAPAN.
 * Copyright (C) 2005-2008 Sebastian Freundt <hroptatyr@sxemacs.org>
 *
 * Original author:  SL Baur <steve@beopen.com>
 *
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
 *
 * This file is part of SXEmacs.
 */

#ifndef INCLUDED_postgresql_h_
#define INCLUDED_postgresql_h_ 1

#include LIBPQ_FE_H_FILE	/* main PostgreSQL header file */

#define BLCKSZ 8192		/* size of a default Postgres disk block */
/*
  This file contains the GCC bug workaround code for the private
  LRECORD types.
*/

/* PGconn is an opaque object and we need to be able to store them in
   Lisp code because libpq supports multiple connections.
*/
struct Lisp_PGconn {
	struct lcrecord_header header;
	PGconn *pgconn;
	Lisp_Object notice_processor;
};
typedef struct Lisp_PGconn Lisp_PGconn;

DECLARE_LRECORD(pgconn, Lisp_PGconn);

#define XPGCONN(x) XRECORD (x, pgconn, Lisp_PGconn)
#define XSETPGCONN(x, p) XSETRECORD (x, p, pgconn)
#define PGCONNP(x) RECORDP (x, pgconn)
#define CHECK_PGCONN(x) CHECK_RECORD (x, pgconn)
#define CONCHECK_PGCONN(x) CONCHECK_RECORD (x, pgconn)

/****/

/* PGresult is an opaque object and we need to be able to store them in
   Lisp code.
*/
struct Lisp_PGresult {
	struct lcrecord_header header;
	PGresult *pgresult;
};
typedef struct Lisp_PGresult Lisp_PGresult;

DECLARE_LRECORD(pgresult, Lisp_PGresult);

#define XPGRESULT(x) XRECORD (x, pgresult, Lisp_PGresult)
#define XSETPGRESULT(x, p) XSETRECORD (x, p, pgresult)
#define PGRESULTP(x) RECORDP (x, pgresult)
#define CHECK_PGRESULT(x) CHECK_RECORD (x, pgresult)
#define CONCHECK_PGRESULT(x) CONCHECK_RECORD (x, pgresult)

#endif	/* INCLUDED_postgresql_h_ */
