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
/*

Implementation notes (written by Steve Baur):
0. Supported PostgreSQL versions
   This code was developed against libpq-8.0.1 and libpq-8.1.0-CVS.  Earlier
   versions do work.  V7 support is more complete than V6.5 support.
   V8 support is currently experimental.
1. Mule
   Non-ASCII databases have been tested on 6.5, 7.3, 7.4 and 8.0.
2. Asynchronous Operation
   Starting with libpq-7.0, an asynchronous interface is offered.  This
   binding supports the asynchronous calls to a limited extent.  Since the
   SXEmacs 22.1 core does not support a sensible interface to add managed but
   unreadable (by SXEmacs) file descriptors to the main select code, polling
   is required to drive the asynchronous calls.  XtAppAddInput would work
   fine, but we want to be able to use the database when running strictly in
   tty mode.
3. Completeness
   Various calls have been deliberately not exported to Lisp.  The
   unexported calls are either left-over backwards compatibility code that
   aren't needed, calls that cannot be implemented sensibly, or calls that
   cannot be implemented safely.  A list of all global functions in libpq
   but not exported to Lisp is below.
4. Policy
   This interface tries very hard to not set any policy towards how database
   code in Emacs Lisp will be written.
5. Documentation
   For full lisp programming documentation, see the SXEmacs Lisp Reference
   Manual.  For PostgreSQL documentation, see the PostgreSQL distribution.

TODO (in rough order of priority):
1. The large object interface needs work with Emacs buffers in addition
   to files.  Need two functions buffer->large_object, and large_object->
   buffer.
- PQexecParams
- PQprepare, PQexecPrepared
- PQresultErrorField

- PQftable, PQftablecol, PQfformat

- PQsendQueryParams
- PQsendPrepare, PQsendQueryPrepared
- PQgetCancel, PQfreeCancel, PQcancel, PQrequestCancel

- PQputCopyData, PQputCopyEnd, PQgetCopyData

- notice receivers
*/

/*
  Unimplemented functions: [TODO]
  PQsetNoticeProcessor - done 2005/05/03 hroptatyr

  Unsupported functions:
  PQsetdbLogin -- This function is deprecated, has a subset of the
   functionality of PQconnectdb, and is better done in Lisp.
   -- and I say, no it is not! I hate the single string interface!
  PQsetdb -- Same as for PQsetdbLogin
   -- same as my comment there :)
  PQsocket -- Abstraction error, file descriptors should not be leaked
   into Lisp code
  PQprint -- print to a file descriptor, deprecated, better done in Lisp
  PQdisplayTuples -- deprecated
  PQprintTuples -- really, really deprecated
  PQmblen -- Returns the length in bytes of multibyte character encoded
   string.
  PQtrace -- controls debug print tracing to a tty.
  PQuntrace -- Ditto.  I don't see any way to do this sensibly.
  PQoidStatus -- deprecated and nearly identical to PQoidValue
  PQfn -- "Fast path" interface; This is a trapdoor into system internals
    and can be a potential security hole.
    Most users will not need this feature.

  lo_open (large object) [*]
  lo_close (large object) [*]
  lo_read (large object) [*]
  lo_write (large object) [*]
  lo_lseek (large object) [*]
  lo_creat (large object) [*]
  lo_tell (large object) [*]
  lo_unlink (large object) [*]
*/

#include <config.h>
#include "lisp.h"
#include "sysdep.h"
#include "buffer.h"
#include "postgresql.h"

#ifdef HAVE_OPENSSL
#include "openssl.h"
#endif

#ifdef MULE
#define PG_OS_CODING Fget_coding_system(Vpg_coding_system)
#else
#define PG_OS_CODING Qnative
#endif
Lisp_Object Vpg_coding_system;

#define CHECK_CONNECTION_ALIVE(P)				\
	if (P == NULL ||					\
	    PQstatus(P) != CONNECTION_OK) {			\
		char *e = "bad value";				\
		if (P) {					\
			e = PQerrorMessage(P);			\
		}						\
		error("dead connection [%s]", e);		\
	}
#define CHECK_CONNECTION_ELIGIBLE(P)				\
	if (P == NULL ||					\
	    PQstatus(P) == CONNECTION_BAD) {			\
		char *e = "bad value";				\
		if (P) {					\
			e = PQerrorMessage(P);			\
		}						\
		error("dead connection [%s]", e);		\
	}
#define PUKE_IF_NULL(p)				\
	if (p == NULL) {			\
		error ("bad value");		\
	}

static Lisp_Object VXPGHOST;
static Lisp_Object VXPGUSER;
static Lisp_Object VXPGOPTIONS;
static Lisp_Object VXPGPORT;
static Lisp_Object VXPGTTY;	/* This needs to be blanked! */
static Lisp_Object VXPGDATABASE;
static Lisp_Object VXPGREALM;
#ifdef MULE
static Lisp_Object VXPGCLIENTENCODING;
#endif				/* MULE */

/* Other variables:
   PGAUTHTYPE -- not used after PostgreSQL 6.5
   PGGEQO
   PGCOSTINDEX
   PGCOSTHEAP
   PGTZ
   PGDATESTYLE
*/
#ifndef HAVE_POSTGRESQLV7
static Lisp_Object VXPGAUTHTYPE;
#endif
static Lisp_Object VXPGGEQO, VXPGCOSTINDEX, VXPGCOSTHEAP, VXPGTZ, VXPGDATESTYLE;

static Lisp_Object Qpostgresql;
static Lisp_Object Q_pg_connection_ok, Q_pg_connection_bad;
static Lisp_Object Q_pg_connection_started, Q_pg_connection_made;
static Lisp_Object Q_pg_connection_awaiting_response, Q_pg_connection_auth_ok;
static Lisp_Object Q_pg_connection_setenv;

/* trans statuses */
static Lisp_Object Q_pg_trans_idle, Q_pg_trans_active, Q_pg_trans_intrans;
static Lisp_Object Q_pg_trans_inerror, Q_pg_trans_unknown;

static Lisp_Object Q_pq_db, Q_pq_user, Q_pq_pass, Q_pq_host, Q_pq_port;
static Lisp_Object Q_pq_tty;
static Lisp_Object Q_pq_options, Q_pq_error_message, Q_pq_backend_pid;
static Lisp_Object Q_pq_status, Q_pq_transaction_status, Q_pq_parameter_status;
static Lisp_Object Q_pq_protocol_version, Q_pq_server_version;
#ifdef HAVE_OPENSSL
static Lisp_Object Q_pq_getssl;
#endif

static Lisp_Object Q_pgres_empty_query, Q_pgres_command_ok, Q_pgres_tuples_ok;
static Lisp_Object Q_pgres_copy_out, Q_pgres_copy_in, Q_pgres_bad_response;
static Lisp_Object Q_pgres_nonfatal_error, Q_pgres_fatal_error;

static Lisp_Object Q_pgres_polling_failed, Q_pgres_polling_reading;
static Lisp_Object Q_pgres_polling_writing, Q_pgres_polling_ok;
static Lisp_Object Q_pgres_polling_active;
/****/

/* PGconn is an opaque object and we need to be able to store them in
   Lisp code because libpq supports multiple connections.
*/
Lisp_Object Qpgconnp;

static Lisp_Object make_pgconn(Lisp_PGconn * pgconn)
{
	Lisp_Object lisp_pgconn;
	XSETPGCONN(lisp_pgconn, pgconn);
	return lisp_pgconn;
}

static Lisp_Object
mark_pgconn(Lisp_Object obj)
{
	return Qnil;
}

static void
print_pgconn(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	PGconn *P;
	ConnStatusType cst;
	char *host = "", *db = "", *user = "", *port = "";

	P = (XPGCONN(obj))->pgconn;

	if (P == NULL) {
		/* this may happen since we allow PQfinish() to be called */
		write_c_string("#<PGconn DEAD>", printcharfun);
	} else if ((cst = PQstatus(P)) == CONNECTION_OK) {
		if (!(host = PQhost(P)))
			host = "";
		port = PQport(P);
		db = PQdb(P);
		if (!(user = PQuser(P))) {
			user = "";
		}
		write_fmt_string(printcharfun, "#<PGconn %s:%s %s/%s>",
				 (!strlen(host) ?
				  "localhost" : host) /* evil! */,
				 port, user, db);
	} else if (cst == CONNECTION_BAD) {
		write_c_string("#<PGconn BAD>", printcharfun);
	} else {
		write_c_string("#<PGconn connecting>", printcharfun);
	}
	return;
}

static Lisp_PGconn*
allocate_pgconn(void)
{
	Lisp_PGconn *pgconn =
		alloc_lcrecord_type(Lisp_PGconn, &lrecord_pgconn);
	pgconn->pgconn = (PGconn *) NULL;
	return pgconn;
}

static void
finalize_pgconn(void *header, int for_disksave)
{
	Lisp_PGconn *pgconn = (Lisp_PGconn *) header;

	if (for_disksave) {
		signal_simple_error
			("Can't dump an emacs containing PGconn objects",
			 make_pgconn(pgconn));
	}

	if (pgconn->pgconn) {
		PQfinish(pgconn->pgconn);
		pgconn->pgconn = (PGconn *) NULL;
	}
}

DEFINE_LRECORD_IMPLEMENTATION("pgconn", pgconn,
			      mark_pgconn, print_pgconn, finalize_pgconn,
			      NULL, NULL, 0, Lisp_PGconn);


/* PGresult is an opaque object and we need to be able to store them in
   Lisp code.
*/
Lisp_Object Qpgresultp;

static Lisp_Object
make_pgresult(Lisp_PGresult * pgresult)
{
	Lisp_Object lisp_pgresult;
	XSETPGRESULT(lisp_pgresult, pgresult);
	return lisp_pgresult;
}

static Lisp_Object
mark_pgresult(Lisp_Object obj)
{
	return Qnil;
}

#define RESULT_TUPLES_FMT "#<PGresult %s[%d] - %s>"
#define RESULT_CMD_TUPLES_FMT "#<PGresult %s[%s] - %s>"
#define RESULT_DEFAULT_FMT "#<PGresult %s - %s>"

static void
print_pgresult(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	PGresult *res;

	res = (XPGRESULT(obj))->pgresult;

	if (res) {
		switch (PQresultStatus(res)) {
		case PGRES_TUPLES_OK:
			/* Add number of tuples of result to output */
			write_fmt_string(printcharfun, RESULT_TUPLES_FMT,
					 PQresStatus(PQresultStatus(res)),
					 PQntuples(res), PQcmdStatus(res));
			break;
		case PGRES_COMMAND_OK:
			/* Add number of tuples affected by output-less
			   command */
			if (!strlen(PQcmdTuples(res)))
				goto notuples;
			write_fmt_string(printcharfun, RESULT_CMD_TUPLES_FMT,
					 PQresStatus(PQresultStatus(res)),
					 PQcmdTuples(res), PQcmdStatus(res));
			break;
		default:
		notuples:
			/* No counts to print */
			write_fmt_string(printcharfun, RESULT_DEFAULT_FMT,
					 PQresStatus(PQresultStatus(res)),
					 PQcmdStatus(res));
			break;
		}
	} else {
		write_c_string("#<PGresult DEAD>", printcharfun);
	}
	return;
}

#undef RESULT_TUPLES_FMT
#undef RESULT_CMD_TUPLES_FMT
#undef RESULT_DEFAULT_FMT

static Lisp_PGresult*
allocate_pgresult(void)
{
	Lisp_PGresult *pgresult = alloc_lcrecord_type(
		Lisp_PGresult, &lrecord_pgresult);
	pgresult->pgresult = (PGresult *) NULL;
	return pgresult;
}

static void
finalize_pgresult(void *header, int for_disksave)
{
	Lisp_PGresult *pgresult = (Lisp_PGresult *) header;

	if (for_disksave) {
		signal_simple_error
		    ("Can't dump an emacs containing PGresult objects",
		     make_pgresult(pgresult));
	}

	if (pgresult->pgresult) {
		PQclear(pgresult->pgresult);
		pgresult->pgresult = (PGresult *) NULL;
	}
}

DEFINE_LRECORD_IMPLEMENTATION("pgresult", pgresult,
			      mark_pgresult, print_pgresult, finalize_pgresult,
			      NULL, NULL, 0, Lisp_PGresult);

/***********************/

/* Notice Processor Stuff */
/* Okay, let's imagine how such a notice processor wants to look like.
 * The only sensible thing I can imagine is a defun (like a sentinel for
 * processes)
 */
static void
sxemacs_notice_processor(Lisp_PGconn *conn, const char *msg)
{
	/* (Lisp_PGconn *)conn; */

	/* void *arg is my sentinel function */
	Lisp_Object sentinel = conn->notice_processor;

	if (NILP(sentinel))
		warn_when_safe(Qpostgresql, Qnotice, "%s", msg);
	else {
		running_asynch_code = 1;
		call2_trapping_errors("Error in notice processor",
			  sentinel, make_pgconn(conn),
			  build_string(msg));
		running_asynch_code = 0;
	}
}

/* HOWTO evoke notices:
 *   (let ((res (pq-exec <conn> "SELECT * FROM <sometable> LIMIT 0")))
 *     (pq-get-is-null res 0 0))
 * should result in:
 * msg <- `row number 0 is out of range 0..-1'
 */
DEFUN("pq-set-notice-processor", Fpq_set_notice_processor, 2, 2, 0, /*
Give CONN the notice processor SENTINEL; nil for none.
The notice processor is called as a function whenever the pq backend
has notices.
It gets two arguments: the connection, and a message string.
*/
      (conn, sentinel))
{
	CHECK_PGCONN(conn);

	XPGCONN(conn)->notice_processor = sentinel;

	return sentinel;
}


/* There are four ways (as of PostgreSQL v7) to connect to a database.
   Two of them, PQsetdb and PQsetdbLogin, are deprecated.  Both of those
   routines take a number of positional parameters and are better done in Lisp.
   Note that PQconnectStart does not exist prior to v7.
*/

DEFUN("pq-conn-defaults", Fpq_conn_defaults, 0, 0, 0,	/*
Return a connection default structure.
*/
      ())
{
	/* This function can GC */
	PQconninfoOption *pcio;
	Lisp_Object temp, temp1;
	int i;

	/* WHAT A FOOKING MESS! */
	pcio = PQconndefaults();
	if (!pcio)
		return Qnil;	/* can never happen in libpq-7.0 */
	temp = list1(Fcons(build_ext_string(pcio[0].keyword, PG_OS_CODING),
			   Fcons(build_ext_string(pcio[0].envvar, PG_OS_CODING),
				 Fcons(build_ext_string
				       (pcio[0].compiled, PG_OS_CODING),
				       Fcons(build_ext_string
					     (pcio[0].val, PG_OS_CODING),
					     Fcons(build_ext_string
						   (pcio[0].label,
						    PG_OS_CODING),
						   Fcons(build_ext_string
							 (pcio[0].dispchar,
							  PG_OS_CODING),
							 Fcons(make_int
							       (pcio[0].
								dispsize),
							       Qnil))))))));

	for (i = 1; pcio[i].keyword; i++) {
		temp1 =
		    list1(Fcons
			  (build_ext_string(pcio[i].keyword, PG_OS_CODING),
			   Fcons(build_ext_string(pcio[i].envvar, PG_OS_CODING),
				 Fcons(build_ext_string
				       (pcio[i].compiled, PG_OS_CODING),
				       Fcons(build_ext_string
					     (pcio[i].val, PG_OS_CODING),
					     Fcons(build_ext_string
						   (pcio[i].label,
						    PG_OS_CODING),
						   Fcons(build_ext_string
							 (pcio[i].dispchar,
							  PG_OS_CODING),
							 Fcons(make_int
							       (pcio[i].
								dispsize),
							       Qnil))))))));
		{
			Lisp_Object args[2];
			args[0] = temp;
			args[1] = temp1;
			/* Fappend GCPROs its arguments */
			temp = Fappend(2, args);
		}
	}

	return temp;
}

/* PQconnectdb Makes a new connection to a backend.
PGconn *PQconnectdb(const char *conninfo)
*/

DEFUN("pq-connectdb", Fpq_connectdb, 1, 1, 0,	/*
Open and return a new database connection using the parameters from the
string CONNINFO.

Unlike `pq-set-db-login' below, the parameter set can be extended without
changing the function signature, so use of this function (or its nonblocking
analogues `pq-connect-start' and `pq-connect-poll') is preferred for new
application programming.

The passed string can be empty to use all default parameters, or it can
contain one or more parameter settings separated by whitespace.  Each
parameter setting is in the form `keyword = value'.  Spaces around the equal
sign are optional.  To write an empty value or a value containing spaces,
surround it with single quotes, e.g., `keyword = \'a value\''.  Single
quotes and backslashes within the value must be escaped with a backslash,
i.e., \\\' and \\\\.

The currently recognized parameter key words are:

- host

Name of host to connect to.  If this begins with a slash, it specifies
Unix-domain communication rather than TCP/IP communication; the value is the
name of the directory in which the socket file is stored.

The default behaviour when `host' is not specified is to connect to a
Unix-domain socket in /tmp (or whatever socket directory was specified when
PostgreSQL was built).

On machines without Unix-domain sockets, the default is to connect to
localhost.


- hostaddr

Numeric IP address of host to connect to.  This should be in the standard
IPv4 address format, e.g., 172.28.40.9.

If your machine supports IPv6, you can also use those addresses.

TCP/IP communication is always used when a nonempty string is specified for
this parameter.

Using `hostaddr' instead of `host' allows the application to avoid a host
name look-up, which may be important in applications with time constraints.
However, Kerberos authentication requires the host name.

The following therefore applies:
  - If `host' is specified without `hostaddr' a host name lookup occurs.
  - If `hostaddr' is specified without `host' the value for `hostaddr' gives
    the remote address.
  - When Kerberos is used, a reverse name query occurs to obtain the host
    name for Kerberos.
  - If both `host' and `hostaddr' are specified, the value for `hostaddr'
    gives the remote address; the value for `host' is ignored, unless
    Kerberos is used, in which case that value is used for Kerberos
    authentication.

Note: Authentication is likely to fail if libpq is passed a host name that
is not the name of the machine at `hostaddr'.
Also, `host' rather than `hostaddr' is used to identify the connection in
~/.pgpass.


Without either a host name or host address, libpq will connect using a local
Unix-domain socket; or on machines without Unix-domain sockets, it will
attempt to connect to localhost.


- port

Port number to connect to at the server host, or socket file name extension
for Unix-domain connections.


- dbname

The database name.
Defaults to be the same as the user name.


- user

PostgreSQL user name to connect as.
Defaults to be the same as the operating system name of the user running the
application.


- password

Password to be used if the server demands password authentication.


- connect_timeout

Maximum wait for connection, in seconds (write as a decimal integer string).
Zero or not specified means wait indefinitely.
It is not recommended to use a timeout of less than 2 seconds.


- options

Command-line options to be sent to the server.


- tty

Ignored (formerly, this specified where to send server debug output).


- sslmode

This option determines whether or with what priority an SSL connection will
be negotiated with the server.
There are four modes:
  - `disable' will attempt only an unencrypted SSL connection;
  - `allow' will negotiate, trying first a non-SSL connection, then if that
    fails, trying an SSL connection;
  - `prefer' (the default) will negotiate, trying first an SSL connection,
    then if that fails, trying a regular non-SSL connection;
  - `require' will try only an SSL connection.

Note: If PostgreSQL is compiled without SSL support, using option require
will cause an error, while options allow and prefer will be accepted but
libpq will not in fact attempt an SSL connection.

- requiressl

This option is deprecated in favour of the sslmode setting.

If set to 1, an SSL connection to the server is required (this is equivalent
to sslmode require). libpq will then refuse to connect if the server does
not accept an SSL connection.
If set to 0 (default), libpq will negotiate the connection type with the
server (equivalent to sslmode prefer).

This option is only available if PostgreSQL is compiled with SSL support.


- service

Service name to use for additional parameters.
It specifies a service name in pg_service.conf that holds additional
connection parameters. This allows applications to specify only a service
name so connection parameters can be centrally maintained.
See share/pg_service.conf.sample in the installation directory for
information on how to set up the file.

General note:
If any parameter is unspecified, then the corresponding environment variable
is checked. If the environment variable is not set either, then the
indicated built-in defaults are used.
*/
      (conninfo))
{
	PGconn *P;
	Lisp_PGconn *lisp_pgconn;
	char *error_message = "Out of Memory?";
	char *c_conninfo;
	/* the result */
	Lisp_Object conn;

	CHECK_STRING(conninfo);

	TO_EXTERNAL_FORMAT(LISP_STRING, conninfo,
			   C_STRING_ALLOCA, c_conninfo, Qnative);
	P = PQconnectdb(c_conninfo);
	if (P && (PQstatus(P) == CONNECTION_OK)) {
		lisp_pgconn = allocate_pgconn();
		lisp_pgconn->pgconn = P;
		lisp_pgconn->notice_processor = Qnil;
		conn = make_pgconn(lisp_pgconn);
		PQsetNoticeProcessor
			(P,
			 (PQnoticeProcessor)sxemacs_notice_processor,
			 /* this is stupid, but libpq wants a void pointer */
			 (Lisp_PGconn *)lisp_pgconn);
		return conn;
	} else {
		/* Connection failed.  Destroy the connection and signal an
		 * error. */
		char buf[BLCKSZ];
		xstrncpy(buf, error_message, sizeof(buf));
		if (P) {
			/* storage for the error message gets erased when
			 * call PQfinish */
			/* so we must temporarily stash it somewhere */
			xstrncpy(buf, PQerrorMessage(P), sizeof(buf));
			PQfinish(P);
		}
		error("libpq: %s", buf);
	}
}

/* PQconnectStart Makes a new asynchronous connection to a backend.
PGconn *PQconnectStart(const char *conninfo)
*/

#ifdef HAVE_POSTGRESQLV7
DEFUN("pq-connect-start", Fpq_connect_start, 1, 1, 0,	/*
Make a new asynchronous connection to a PostgreSQL backend.

See `pq-connectdb' for a complete description of conninfo.
*/
      (conninfo))
{
	PGconn *P;
	Lisp_PGconn *lisp_pgconn;
	char *error_message = "Out of Memory?";
	char *c_conninfo;
	/* the result */
	Lisp_Object conn;

	CHECK_STRING(conninfo);
	TO_EXTERNAL_FORMAT(LISP_STRING, conninfo,
			   C_STRING_ALLOCA, c_conninfo, Qnative);
	P = PQconnectStart(c_conninfo);

	if (P && (PQstatus(P) != CONNECTION_BAD)) {
		lisp_pgconn = allocate_pgconn();
		lisp_pgconn->pgconn = P;
		lisp_pgconn->notice_processor = Qnil;
		conn = make_pgconn(lisp_pgconn);
		PQsetNoticeProcessor
			(P,
			 (PQnoticeProcessor)sxemacs_notice_processor,
			 (void *)&conn);
		return conn;
	} else {
		/* capture the error message before destroying the object */
		char buf[BLCKSZ];
		xstrncpy(buf, error_message, sizeof(buf));
		if (P) {
			xstrncpy(buf, PQerrorMessage(P), sizeof(buf));
			PQfinish(P);
		}
		error("libpq: %s", buf);
	}
	return Qnil;
}

DEFUN("pq-connect-poll", Fpq_connect_poll, 1, 1, 0,	/*
Poll an asynchronous connection for completion
*/
      (conn))
{
	PGconn *P;
	PostgresPollingStatusType polling_status;

	CHECK_PGCONN(conn);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ELIGIBLE(P);

	polling_status = PQconnectPoll(P);
	switch (polling_status) {
	case PGRES_POLLING_FAILED:
		/* Something Bad has happened */
		{
			char *e = PQerrorMessage(P);
			error("libpq: %s", e);
		}
	case PGRES_POLLING_OK:
		return Q_pgres_polling_ok;
	case PGRES_POLLING_READING:
		return Q_pgres_polling_reading;
	case PGRES_POLLING_WRITING:
		return Q_pgres_polling_writing;
	case PGRES_POLLING_ACTIVE:
		return Q_pgres_polling_active;
	default:
		/* they've added a new field we don't know about */
		error("Help!  Unknown status code %08x from backend!",
		      polling_status);
	}
}

#ifdef MULE
DEFUN("pq-client-encoding", Fpq_client_encoding, 1, 1, 0,	/*
Return client coding system.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return make_int(PQclientEncoding(P));
}

DEFUN("pq-set-client-encoding", Fpq_set_client_encoding, 2, 2, 0,	/*
Set client coding system.
*/
      (conn, encoding))
{
	PGconn *P;
	int rc;
	char *c_encoding;

	CHECK_PGCONN(conn);
	CHECK_STRING(encoding);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	TO_EXTERNAL_FORMAT(LISP_STRING, encoding,
			   C_STRING_ALLOCA, c_encoding, Qnative);

	if ((rc = PQsetClientEncoding(P, c_encoding)) < 0)
		error("bad encoding");
	else
		return make_int(rc);
}

#endif
#endif				/* HAVE_POSTGRESQLV7 */

/* PQfinish Close the connection to the backend. Also frees memory
       used by the PGconn object.
void PQfinish(PGconn *conn)
*/
DEFUN("pq-finish", Fpq_finish, 1, 1, 0,	/*
Close the connection to the backend.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	PUKE_IF_NULL(P);

	PQfinish(P);
	/* #### PQfinish deallocates the PGconn structure, so we now have a
	   dangling pointer. */
	/* Genocided all @'s ... */
	(XPGCONN(conn))->pgconn = (PGconn *) NULL;	/* You feel DEAD inside */
	return Qnil;
}

DEFUN("pq-clear", Fpq_clear, 1, 1, 0,	/*
Forcibly erase a PGresult object.
*/
      (res))
{
	PGresult *R;

	CHECK_PGRESULT(res);
	R = (XPGRESULT(res))->pgresult;
	PUKE_IF_NULL(R);

	PQclear(R);
	/* Genocided all @'s ... */
	(XPGRESULT(res))->pgresult = (PGresult *) NULL;	/* You feel DEAD inside */

	return Qnil;
}

DEFUN("pq-is-busy", Fpq_is_busy, 1, 1, 0,	/*
Return t if PQgetResult would block waiting for input.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return PQisBusy(P) ? Qt : Qnil;
}

DEFUN("pq-consume-input", Fpq_consume_input, 1, 1, 0,	/*
Consume any available input from the backend.
Returns nil if something bad happened.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return PQconsumeInput(P) ? Qt : Qnil;
}

/* PQreset Reset the communication port with the backend.
void PQreset(PGconn *conn)
*/
DEFUN("pq-reset", Fpq_reset, 1, 1, 0,	/*
Reset the connection to the backend.
This function will close the connection to the backend and attempt to
reestablish a new connection to the same postmaster, using all the same
parameters previously used.  This may be useful for error recovery if a
working connection is lost.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	PUKE_IF_NULL(P);
	/* we can resurrect a BAD connection, but not a dead one. */

	PQreset(P);

	return Qnil;
}

#ifdef HAVE_POSTGRESQLV7
DEFUN("pq-reset-start", Fpq_reset_start, 1, 1, 0,	/*
Reset connection to the backend asynchronously.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	if (PQresetStart(P))
		return Qt;
	{
		char *e = PQerrorMessage(P);
		error("libpq: %s", e);
	}
}

DEFUN("pq-reset-poll", Fpq_reset_poll, 1, 1, 0,	/*
Poll an asynchronous reset for completion.
*/
      (conn))
{
	PGconn *P;
	PostgresPollingStatusType polling_status;

	CHECK_PGCONN(conn);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ELIGIBLE(P);

	polling_status = PQresetPoll(P);
	switch (polling_status) {
	case PGRES_POLLING_FAILED:
		/* Something Bad has happened */
		{
			char *e = PQerrorMessage(P);
			error("libpq: %s", e);
		}
	case PGRES_POLLING_OK:
		return Q_pgres_polling_ok;
	case PGRES_POLLING_READING:
		return Q_pgres_polling_reading;
	case PGRES_POLLING_WRITING:
		return Q_pgres_polling_writing;
	case PGRES_POLLING_ACTIVE:
		return Q_pgres_polling_active;
	default:
		/* they've added a new field we don't know about */
		error("Help!  Unknown status code %08x from backend!",
		      polling_status);
	}
}
#endif

DEFUN("pq-request-cancel", Fpq_request_cancel, 1, 1, 0,	/*
Attempt to request cancellation of the current operation.

The return value is t if the cancel request was successfully
dispatched, nil if not (in which case conn->errorMessage is set).
Note: successful dispatch is no guarantee that there will be any effect at
the backend.  The application must read the operation result as usual.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return PQrequestCancel(P) ? Qt : Qnil;
}

/* accessor function for the PGconn object */
DEFUN("pq-connection-status", Fpq_connection_status, 2, 2, 0,	/*
Accessor function for the PGconn object.
Currently recognized symbols for the field:
:pq-db                  Database name
:pq-user                Database user name
:pq-pass                Database user's password
:pq-host                Hostname of PostgreSQL backend connected to
:pq-port                TCP port number of connection
:pq-tty                 Debugging TTY (not used in Emacs)
:pq-options             Additional backend options
:pq-status              Connection status (either OK or BAD)
:pq-transaction-status  Current in-transaction status of the server
:pq-parameter-status    Current parameter setting of the server
:pq-protocol-version    Frontend/Backend protocol
:pq-server-version      Integer representing the backend version
:pq-error-message       Last error message from the backend
:pq-backend-pid         Process ID of backend process
:pq-getssl              SSL session used in the connection
*/
      (conn, field))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	PUKE_IF_NULL(P);	/* BAD connections still have state to query */

	if (EQ(field, Q_pq_db)) {
		/* PQdb Returns the database name of the connection.
		   char *PQdb(PGconn *conn)
		 */
		return build_ext_string(PQdb(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_user)) {
		/* PQuser Returns the user name of the connection.
		   char *PQuser(PGconn *conn)
		 */
		return build_ext_string(PQuser(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_pass)) {
		/* PQpass Returns the password of the connection.
		   char *PQpass(PGconn *conn)
		 */
		return build_ext_string(PQpass(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_host)) {
		/* PQhost Returns the server host name of the connection.
		   char *PQhost(PGconn *conn)
		 */
		return build_ext_string(PQhost(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_port)) {
		char *p;
		/* PQport Returns the port of the connection.
		   char *PQport(PGconn *conn)
		 */
		if ((p = PQport(P)))
			return make_int(atoi(p));
		else
			return make_int(-1);
	} else if (EQ(field, Q_pq_tty)) {
		/* PQtty Returns the debug tty of the connection.
		   char *PQtty(PGconn *conn)
		 */
		return build_ext_string(PQtty(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_options)) {
		/* PQoptions Returns the backend options used in the connection.
		   char *PQoptions(PGconn *conn)
		 */
		return build_ext_string(PQoptions(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_status)) {
		ConnStatusType cst;
		/* PQstatus Returns the status of the connection. The status can be
		   CONNECTION_OK or CONNECTION_BAD.
		   ConnStatusType PQstatus(PGconn *conn)
		 */
		switch ((cst = PQstatus(P))) {
		case CONNECTION_OK:
			return Q_pg_connection_ok;
		case CONNECTION_BAD:
			return Q_pg_connection_bad;
#ifdef HAVE_POSTGRESQLV7
		case CONNECTION_STARTED:
			return Q_pg_connection_started;
		case CONNECTION_MADE:
			return Q_pg_connection_made;
		case CONNECTION_AWAITING_RESPONSE:
			return Q_pg_connection_awaiting_response;
		case CONNECTION_AUTH_OK:
			return Q_pg_connection_auth_ok;
		case CONNECTION_SETENV:
			return Q_pg_connection_setenv;
#endif				/* HAVE_POSTGRESQLV7 */
		default:
			/* they've added a new field we don't know about */
			error("Help!  Unknown connection status code %08x "
			      "from backend!",
			      cst);
		}
	} else if (EQ(field, Q_pq_transaction_status)) {
		PGTransactionStatusType ts;
		switch ((ts = PQtransactionStatus(P))) {
		case PQTRANS_IDLE:
			return Q_pg_trans_idle;
		case PQTRANS_ACTIVE:
			return Q_pg_trans_active;
		case PQTRANS_INTRANS:
			return Q_pg_trans_intrans;
		case PQTRANS_INERROR:
			return Q_pg_trans_inerror;
		case PQTRANS_UNKNOWN:
			return Q_pg_trans_unknown;
		default:
			/* they've added a new field we don't know about */
			error("Help!  Unknown transaction status code %08x "
			      "from backend!",
			      ts);
		}
	} else if (EQ(field, Q_pq_parameter_status)) {
		return Qnil;
	} else if (EQ(field, Q_pq_protocol_version)) {
		return make_int(PQprotocolVersion(P));
#if HAVE_PQSERVERVERSION
	} else if (EQ(field, Q_pq_server_version)) {
		return make_int(PQserverVersion(P));
#else
	} else if (EQ(field, Q_pq_server_version)) {
		char *vstr = xstrdup(PQparameterStatus(P,"server_version"));
		char *tmp, previous;
		int major, minor, patch;

		if ( vstr == NULL )
			return Qnil;
		tmp = strtok(vstr,".");
		major = atoi(tmp);
		tmp = strtok(NULL,".");
		minor = atoi(tmp);
		tmp = strtok(NULL,".");
		patch = atoi(tmp);
		xfree(vstr);
		return make_int(major*10000+minor*100+patch);
#endif
	} else if (EQ(field, Q_pq_error_message)) {
		/* PQerrorMessage Returns the error message most recently
		 * generated by an operation on the connection.
		 * char *PQerrorMessage(PGconn* conn);
		 */
		return build_ext_string(PQerrorMessage(P), PG_OS_CODING);
	} else if (EQ(field, Q_pq_backend_pid)) {
		/* PQbackendPID Returns the process ID of the backend server
		 * handling this connection.
		 * int PQbackendPID(PGconn *conn);
		 */
		return make_int(PQbackendPID(P));
#ifdef HAVE_OPENSSL
	} else if (EQ(field, Q_pq_getssl)) {
		/* PQgetssl Returns the SSL structure used in the connection,
		 * or NULL if SSL is not in use.
		 * SSL *PQgetssl(PGconn *conn);
		 */
		SSL *ssl_conn;
		ssl_conn = (SSL*)PQgetssl(P);
		if (ssl_conn == NULL)
			return Qnil; /* meaning: no SSL in use */
		else {
			Lisp_SSL_CONN *pqssl = allocate_ssl_conn();
			pqssl->ssl_conn = ssl_conn;
			pqssl->parent = conn;
			pqssl->protected_p = 1;
			return make_ssl_conn(pqssl);
			/* Should we use a copy of the SSL session here?
			 * Otherwise it's safe to obtain a nice segfault by:
			 *   (setq m (pq-pgconn foo \'pq::getssl))
			 *   (setq m \'something-else)
			 *   M-x garbage-collect RET
			 *   (pq-send-query foo ...)
			 * You will see SXE dump in ssl*_write or the like
			 * since the _original_ session handle has been gc'd
			 *
			 * Nah, _for the moment_ I assume our users to be
			 * smart enough to rethink twice before they do
			 * something like this.
			 */
		}
#endif
	}

	/* else */
	message("bad PGconn accessor");
	return Qnil;
}

DEFUN("pq-connection-alive-p", Fpq_connection_alive_p, 1, 1, 0, /*
Return non-nil when CONN is considered alive.

This is roughly the same as calling (pq-connection-status CONN :status)
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);

	P = (XPGCONN(conn))->pgconn;

	if (PQstatus(P) == CONNECTION_OK) {
		return Qt;
	}
	return Qnil;
}

DEFUN("pq-connection-p", Fpq_connection_p, 1, 1, 0, /*
Return non-nil if OBJECT is a pq connection object.
*/
      (object))
{
	return PGCONNP(object) ? Qt : Qnil;
}

/* Query functions */
DEFUN("pq-exec", Fpq_exec, 2, 2, 0,	/*
Submit a query to Postgres and wait for the result.
*/
      (conn, query))
{
	PGconn *P;
	Lisp_PGresult *lisp_pgresult;
	PGresult *R;
	char *c_query;

	CHECK_PGCONN(conn);
	CHECK_STRING(query);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	TO_EXTERNAL_FORMAT(LISP_STRING, query,
			   C_STRING_ALLOCA, c_query, Qnative);

	R = PQexec(P, c_query);
	{
		char *tag, buf[BLCKSZ];

		if (!R)
			error("query: out of memory");
		else
			switch (PQresultStatus(R)) {
			case PGRES_BAD_RESPONSE:
				tag = "bad response [%s]";
				goto err;
			case PGRES_NONFATAL_ERROR:
				tag = "non-fatal error [%s]";
				goto err;
			case PGRES_FATAL_ERROR:
				tag = "fatal error [%s]";
			      err:
				xstrncpy(buf, PQresultErrorMessage(R),
					sizeof(buf));
				PQclear(R);
				error(tag, buf);
			 /*NOTREACHED*/ default:
				break;
			}
	}

	lisp_pgresult = allocate_pgresult();
	lisp_pgresult->pgresult = R;

	return make_pgresult(lisp_pgresult);
}

DEFUN("pq-send-query", Fpq_send_query, 2, 2, 0,	/*
Submit a query to Postgres and don't wait for the result.
Returns: t if successfully submitted
nil if error (conn->errorMessage is set)
*/
      (conn, query))
{
	PGconn *P;
	char *c_query;

	CHECK_PGCONN(conn);
	CHECK_STRING(query);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	TO_EXTERNAL_FORMAT(LISP_STRING, query,
			   C_STRING_ALLOCA, c_query, Qnative);

	if (PQsendQuery(P, c_query))
		return Qt;
	else
		error("async query: %s", PQerrorMessage(P));
}

DEFUN("pq-result-p", Fpq_result_p, 1, 1, 0, /*
Return non-nil if OBJECT is a pq query result object.
*/
      (object))
{
	return PGRESULTP(object) ? Qt : Qnil;
}

DEFUN("pq-get-result", Fpq_get_result, 1, 1, 0,	/*
Retrieve an asynchronous result from a query.
NIL is returned when no more query work remains.
*/
      (conn))
{
	PGconn *P;
	Lisp_PGresult *lisp_pgresult;
	PGresult *R;

	CHECK_PGCONN(conn);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	R = PQgetResult(P);
	if (!R)
		return Qnil;	/* not an error, there's no more data to get */

	{
		char *tag, buf[BLCKSZ];

		switch (PQresultStatus(R)) {
		case PGRES_BAD_RESPONSE:
			tag = "bad response [%s]";
			goto err;
		case PGRES_NONFATAL_ERROR:
			tag = "non-fatal error [%s]";
			goto err;
		case PGRES_FATAL_ERROR:
			tag = "fatal error [%s]";
		      err:
			xstrncpy(buf, PQresultErrorMessage(R), sizeof(buf));
			PQclear(R);
			error(tag, buf);
		 /*NOTREACHED*/ default:
			break;
		}
	}

	lisp_pgresult = allocate_pgresult();
	lisp_pgresult->pgresult = R;

	return make_pgresult(lisp_pgresult);
}

DEFUN("pq-result-status", Fpq_result_status, 1, 1, 0,	/*
Return result status of the query.
*/
      (result))
{
	PGresult *R;
	ExecStatusType est;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	switch ((est = PQresultStatus(R))) {
	case PGRES_EMPTY_QUERY:
		return Q_pgres_empty_query;
	case PGRES_COMMAND_OK:
		return Q_pgres_command_ok;
	case PGRES_TUPLES_OK:
		return Q_pgres_tuples_ok;
	case PGRES_COPY_OUT:
		return Q_pgres_copy_out;
	case PGRES_COPY_IN:
		return Q_pgres_copy_in;
	case PGRES_BAD_RESPONSE:
		return Q_pgres_bad_response;
	case PGRES_NONFATAL_ERROR:
		return Q_pgres_nonfatal_error;
	case PGRES_FATAL_ERROR:
		return Q_pgres_fatal_error;
	default:
		/* they've added a new field we don't know about */
		error("Help!  Unknown exec status code %08x from backend!",
		      est);
	}
}

DEFUN("pq-result-status-string", Fpq_result_status_string, 1, 1, 0,	/*
Return stringified result status of the query.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return build_ext_string(PQresStatus(PQresultStatus(R)), PG_OS_CODING);
}

/* Sundry PGresult accessor functions */
DEFUN("pq-result-error-message", Fpq_result_error_message, 1, 1, 0,	/*
Return last message associated with the query.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return build_ext_string(PQresultErrorMessage(R), PG_OS_CODING);
}

DEFUN("pq-ntuples", Fpq_ntuples, 1, 1, 0,	/*
Return the number of tuples (instances) in the query result.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return make_int(PQntuples(R));
}

DEFUN("pq-nfields", Fpq_nfields, 1, 1, 0,	/*
Return the number of fields (attributes) in each tuple of the query result.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return make_int(PQnfields(R));
}

DEFUN("pq-binary-tuples", Fpq_binary_tuples, 1, 1, 0,	/*
Return t if the query result contains binary data, nil otherwise.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return (PQbinaryTuples(R)) ? Qt : Qnil;
}

DEFUN("pq-fname", Fpq_fname, 2, 2, 0,	/*
Return the field (attribute) name associated with the given field index.
Field indices start at 0.
*/
      (result, field_index))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(field_index);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return build_ext_string(PQfname(R, XINT(field_index)), PG_OS_CODING);
}

DEFUN("pq-fnumber", Fpq_fnumber, 2, 2, 0,	/*
Return the number of fields (attributes) in each tuple of the query result.
*/
      (result, field_name))
{
	PGresult *R;
	char *c_field_name;

	CHECK_PGRESULT(result);
	CHECK_STRING(field_name);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	TO_EXTERNAL_FORMAT(LISP_STRING, field_name,
			   C_STRING_ALLOCA, c_field_name, Qnative);

	return make_int(PQfnumber(R, c_field_name));
}

DEFUN("pq-ftype", Fpq_ftype, 2, 2, 0,	/*
Return the field type associated with the given field index.
The integer returned is the internal coding of the type.  Field indices
start at 0.
*/
      (result, field_num))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(field_num);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return make_int(PQftype(R, XINT(field_num)));
}

DEFUN("pq-fsize", Fpq_fsize, 2, 2, 0,	/*
Return the field size in bytes associated with the given field index.
Field indices start at 0.
*/
      (result, field_index))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(field_index);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return make_int(PQftype(R, XINT(field_index)));
}

DEFUN("pq-fmod", Fpq_fmod, 2, 2, 0,	/*
Return the type modifier associated with a field.
Field indices start at 0.
*/
      (result, field_index))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(field_index);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return make_int(PQfmod(R, XINT(field_index)));
}

DEFUN("pq-get-value", Fpq_get_value, 3, 3, 0,	/*
Return a single field (attribute) value of one tuple of a PGresult.
Tuple and field indices start at 0.
*/
      (result, tup_num, field_num))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(tup_num);
	CHECK_INT(field_num);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return build_ext_string(PQgetvalue(R, XINT(tup_num), XINT(field_num)),
				PG_OS_CODING);
}

DEFUN("pq-get-length", Fpq_get_length, 3, 3, 0,	/*
Returns the length of a field value in bytes.
If result is binary, i.e. a result of a binary portal, then the
length returned does NOT include the size field of the varlena.  (The
data returned by PQgetvalue doesn't either.)
*/
      (result, tup_num, field_num))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(tup_num);
	CHECK_INT(field_num);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return make_int(PQgetlength(R, XINT(tup_num), XINT(field_num)));
}

DEFUN("pq-get-is-null", Fpq_get_is_null, 3, 3, 0,	/*
Returns the null status of a field value.
*/
      (result, tup_num, field_num))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	CHECK_INT(tup_num);
	CHECK_INT(field_num);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return PQgetisnull(R, XINT(tup_num), XINT(field_num)) ? Qt : Qnil;
}

DEFUN("pq-cmd-status", Fpq_cmd_status, 1, 1, 0,	/*
Returns the command status string from the SQL command that generated the result.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return build_ext_string(PQcmdStatus(R), PG_OS_CODING);
}

DEFUN("pq-cmd-tuples", Fpq_cmd_tuples, 1, 1, 0,	/*
Returns the number of rows affected by the SQL command.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

	return build_ext_string(PQcmdTuples(R), PG_OS_CODING);
}

DEFUN("pq-oid-value", Fpq_oid_value, 1, 1, 0,	/*
Returns the object id of the tuple inserted.
*/
      (result))
{
	PGresult *R;

	CHECK_PGRESULT(result);
	R = (XPGRESULT(result))->pgresult;
	PUKE_IF_NULL(R);

#ifdef HAVE_POSTGRESQLV7
	return make_int(PQoidValue(R));
#else
	/* Use the old interface */
	return make_int(atoi(PQoidStatus(R)));
#endif
}

#ifdef HAVE_POSTGRESQLV7
DEFUN("pq-set-nonblocking", Fpq_set_nonblocking, 2, 2, 0,	/*
Sets the PGconn's database connection non-blocking if the arg is TRUE
or makes it non-blocking if the arg is FALSE, this will not protect
you from PQexec(), you'll only be safe when using the non-blocking API.

Needs to be called only on a connected database connection.
*/
      (conn, arg))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return make_int(PQsetnonblocking(P, !NILP(arg)));
}

DEFUN("pq-is-nonblocking", Fpq_is_nonblocking, 1, 1, 0,	/*
Return the blocking status of the database connection.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return PQisnonblocking(P) ? Qt : Qnil;
}

DEFUN("pq-flush", Fpq_flush, 1, 1, 0,	/*
Force the write buffer to be written (or at least try).
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return make_int(PQflush(P));
}
#endif

DEFUN("pq-notifies", Fpq_notifies, 1, 1, 0,	/*
Return the latest async notification that has not yet been handled.
If there has been a notification, then a list of two elements will be returned.
The first element contains the relation name being notified, the second
element contains the backend process ID number.  nil is returned if there
aren't any notifications to process.
*/
      (conn))
{
	/* This function cannot GC */
	PGconn *P;
	PGnotify *PGN;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	PGN = PQnotifies(P);
	if (!PGN)
		return Qnil;
	else {
		Lisp_Object temp;

		temp = list2(build_ext_string(PGN->relname, PG_OS_CODING),
			     make_int(PGN->be_pid));
		xfree(PGN);
		return temp;
	}
}

#if defined (HAVE_POSTGRESQLV7) && defined(MULE)
DEFUN("pq-env-2-encoding", Fpq_env_2_encoding, 0, 0, 0,	/*
Get encoding id from environment variable PGCLIENTENCODING.
*/
      ())
{
	return make_int(PQenv2encoding());
}
#endif				/* MULE */

DEFUN("pq-lo-import", Fpq_lo_import, 2, 2, 0,	/*
*/
      (conn, filename))
{
	PGconn *P;
	char *c_filename;

	CHECK_PGCONN(conn);
	CHECK_STRING(filename);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	TO_EXTERNAL_FORMAT(LISP_STRING, filename,
			   C_STRING_ALLOCA, c_filename, Qfile_name);

	return make_int((int)lo_import(P, c_filename));
}

DEFUN("pq-lo-export", Fpq_lo_export, 3, 3, 0,	/*
*/
      (conn, oid, filename))
{
	PGconn *P;
	char *c_filename;

	CHECK_PGCONN(conn);
	CHECK_INT(oid);
	CHECK_STRING(filename);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	TO_EXTERNAL_FORMAT(LISP_STRING, filename,
			   C_STRING_ALLOCA, c_filename, Qfile_name);

	return make_int((int)lo_export(P, XINT(oid), c_filename));
}

DEFUN("pq-make-empty-pgresult", Fpq_make_empty_pgresult, 2, 2, 0,	/*
Make an empty PGresult object with the given status.
*/
      (conn, status))
{
	PGconn *P;
	Lisp_PGresult *lpgr;
	PGresult *R;
	ExecStatusType est;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);	/* needed here? */

	if (EQ(status, Q_pgres_empty_query))
		est = PGRES_EMPTY_QUERY;
	else if (EQ(status, Q_pgres_command_ok))
		est = PGRES_COMMAND_OK;
	else if (EQ(status, Q_pgres_tuples_ok))
		est = PGRES_TUPLES_OK;
	else if (EQ(status, Q_pgres_copy_out))
		est = PGRES_COPY_OUT;
	else if (EQ(status, Q_pgres_copy_in))
		est = PGRES_COPY_IN;
	else if (EQ(status, Q_pgres_bad_response))
		est = PGRES_BAD_RESPONSE;
	else if (EQ(status, Q_pgres_nonfatal_error))
		est = PGRES_NONFATAL_ERROR;
	else if (EQ(status, Q_pgres_fatal_error))
		est = PGRES_FATAL_ERROR;
	else
		signal_simple_error("bad status symbol", status);

	R = PQmakeEmptyPGresult(P, est);
	if (!R)
		error("out of memory?");

	lpgr = allocate_pgresult();
	lpgr->pgresult = R;

	return make_pgresult(lpgr);
}

#ifdef HAVE_POSTGRESQLV7
/* actually I don't know when this made its way to libpq
 * I just assume 7.4 here
 * Bite me, if that's wrong ;P
 */
DEFUN("pq-escape-string", Fpq_escape_string, 1, 1, 0, /*
Return an SQL-suited escaped version of STRING.
*/
      (string))
{
	char *result;
	int result_len;
	/* buffers for our args */
	char *string_ext;
	int string_len;

	TO_EXTERNAL_FORMAT(LISP_STRING, string,
			   C_STRING_ALLOCA, string_ext, PG_OS_CODING);
	string_len = (int)XSTRING_CHAR_LENGTH(string);

	result = (char *)xmalloc_atomic(4*XSTRING_LENGTH(string));

	result_len = PQescapeString(result, string_ext, string_len);

	return make_ext_string(result, result_len, PG_OS_CODING);
}

DEFUN("pq-escape-bytea", Fpq_escape_bytea, 1, 1, 0, /*
Return an SQL-suited escaped version of binary DATA.
*/
      (data))
{
	char *result;
	size_t result_len;
	/* buffers for our args */
	char *data_ext;
	int data_len;

	TO_EXTERNAL_FORMAT(LISP_STRING, data,
			   C_STRING_ALLOCA, data_ext, PG_OS_CODING);
	data_len = (int)XSTRING_CHAR_LENGTH(data);

	result = (char*)PQescapeBytea(
		(unsigned char*)data_ext, data_len, &result_len);

	if (result == NULL)
		return Qnil;
	else
		return make_ext_string(result,result_len-1,PG_OS_CODING);
}

DEFUN("pq-unescape-bytea", Fpq_unescape_bytea, 1, 1, 0, /*
Return the unescaped form of DATA (which may be binary).
Such binary data may result from a BYTEA column.

Note: Of course, escaped SQL strings are elisp-escaped again
so you may have to use `pq-unescape-bytea' twice.
*/
      (data))
{
	char *result;
	size_t result_len;
	/* buffers for our args */
	char *data_ext;

	TO_EXTERNAL_FORMAT(LISP_STRING, data,
			   C_STRING_ALLOCA, data_ext, PG_OS_CODING);

	result = (char*)PQunescapeBytea(
		(unsigned char*)data_ext, &result_len);

	if (result == NULL)
		return Qnil;
	else
		return make_ext_string(result,result_len,PG_OS_CODING);
}
#endif

DEFUN("pq-get-line", Fpq_get_line, 1, 1, 0,	/*
Retrieve a line from server in copy in operation.
The return value is a dotted pair where the cons cell is an integer code:
-1: Copying is complete
0: A record is complete
1: A record is incomplete, it will be continued in the next `pq-get-line'
operation.
and the cdr cell is returned string data.

The copy operation is complete when the value `\.' (backslash dot) is
returned.
*/
      (conn))
{
	char buffer[BLCKSZ];	/* size of a Postgres disk block */
	PGconn *P;
	int ret;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	ret = PQgetline(P, buffer, sizeof(buffer));

	return Fcons(make_int(ret), build_ext_string(buffer, PG_OS_CODING));
}

DEFUN("pq-put-line", Fpq_put_line, 2, 2, 0,	/*
Send a line to the server in copy out operation.

Returns t if the operation succeeded, nil otherwise.
*/
      (conn, string))
{
	PGconn *P;
	char *c_string;

	CHECK_PGCONN(conn);
	CHECK_STRING(string);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);
	TO_EXTERNAL_FORMAT(LISP_STRING, string,
			   C_STRING_ALLOCA, c_string, Qnative);

	return !PQputline(P, c_string) ? Qt : Qnil;
}

DEFUN("pq-get-line-async", Fpq_get_line_async, 1, 1, 0,	/*
Get a line from the server in copy in operation asynchronously.

This routine is for applications that want to do "COPY <rel> to stdout"
asynchronously, that is without blocking.  Having issued the COPY command
and gotten a PGRES_COPY_OUT response, the app should call PQconsumeInput
and this routine until the end-of-data signal is detected.  Unlike
PQgetline, this routine takes responsibility for detecting end-of-data.

On each call, PQgetlineAsync will return data if a complete newline-
terminated data line is available in libpq's input buffer, or if the
incoming data line is too long to fit in the buffer offered by the caller.
Otherwise, no data is returned until the rest of the line arrives.

If -1 is returned, the end-of-data signal has been recognized (and removed
from libpq's input buffer).  The caller *must* next call PQendcopy and
then return to normal processing.

RETURNS:
-1    if the end-of-copy-data marker has been recognized
0         if no data is available
>0    the number of bytes returned.
The data returned will not extend beyond a newline character.  If possible
a whole line will be returned at one time.  But if the buffer offered by
the caller is too small to hold a line sent by the backend, then a partial
data line will be returned.  This can be detected by testing whether the
last returned byte is '\n' or not.
The returned string is *not* null-terminated.
*/
      (conn))
{
	PGconn *P;
	char buffer[BLCKSZ];
	int ret;

	CHECK_PGCONN(conn);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	ret = PQgetlineAsync(P, buffer, sizeof(buffer));

	if (ret == -1)
		return Qt;	/* done! */
	else if (!ret)
		return Qnil;	/* no data yet */
	else
		return Fcons(make_int(ret),
			     make_ext_string((Extbyte *) buffer, ret,
					     PG_OS_CODING));
}

DEFUN("pq-put-nbytes", Fpq_put_nbytes, 2, 2, 0,	/*
Asynchronous copy out.
*/
      (conn, data))
{
	/* NULs are not allowed.  I don't think this matters at this time. */
	PGconn *P;
	char *c_data;

	CHECK_PGCONN(conn);
	CHECK_STRING(data);

	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);
	TO_EXTERNAL_FORMAT(LISP_STRING, data, C_STRING_ALLOCA, c_data, Qnative);

	return !PQputnbytes(P, c_data, strlen(c_data)) ? Qt : Qnil;
}

DEFUN("pq-end-copy", Fpq_end_copy, 1, 1, 0,	/*
End a copying operation.
*/
      (conn))
{
	PGconn *P;

	CHECK_PGCONN(conn);
	P = (XPGCONN(conn))->pgconn;
	CHECK_CONNECTION_ALIVE(P);

	return PQendcopy(P) ? Qt : Qnil;
}


void
syms_of_postgresql(void)
{
	INIT_LRECORD_IMPLEMENTATION(pgconn);
	INIT_LRECORD_IMPLEMENTATION(pgresult);

	defsymbol(&Qpostgresql, "postgresql");

	/* opaque exported types */
	defsymbol(&Qpgconnp, "pgconnp");
	defsymbol(&Qpgresultp, "pgresultp");

	/* connection status types
	 * now implemented as keywords */
	DEFKEYWORD(Q_pg_connection_ok);
	DEFKEYWORD(Q_pg_connection_bad);
	DEFKEYWORD(Q_pg_connection_started);
	DEFKEYWORD(Q_pg_connection_made);
	DEFKEYWORD(Q_pg_connection_awaiting_response);
	DEFKEYWORD(Q_pg_connection_auth_ok);
	DEFKEYWORD(Q_pg_connection_setenv);

	/* transaction status types */
	DEFKEYWORD(Q_pg_trans_idle);
	DEFKEYWORD(Q_pg_trans_active);
	DEFKEYWORD(Q_pg_trans_intrans);
	DEFKEYWORD(Q_pg_trans_inerror);
	DEFKEYWORD(Q_pg_trans_unknown);

	/* Fields of PGconn */
	DEFKEYWORD(Q_pq_db);
	DEFKEYWORD(Q_pq_user);
	DEFKEYWORD(Q_pq_pass);
	DEFKEYWORD(Q_pq_host);
	DEFKEYWORD(Q_pq_port);
	DEFKEYWORD(Q_pq_tty);
	DEFKEYWORD(Q_pq_options);
	DEFKEYWORD(Q_pq_status);
	DEFKEYWORD(Q_pq_transaction_status);
	DEFKEYWORD(Q_pq_parameter_status);
	DEFKEYWORD(Q_pq_protocol_version);
	DEFKEYWORD(Q_pq_server_version);
	DEFKEYWORD(Q_pq_error_message);
	DEFKEYWORD(Q_pq_backend_pid);
#ifdef HAVE_OPENSSL
	DEFKEYWORD(Q_pq_getssl);
#endif

	/* Query status results */
	DEFKEYWORD(Q_pgres_empty_query);
	DEFKEYWORD(Q_pgres_command_ok);
	DEFKEYWORD(Q_pgres_tuples_ok);
	DEFKEYWORD(Q_pgres_copy_out);
	DEFKEYWORD(Q_pgres_copy_in);
	DEFKEYWORD(Q_pgres_bad_response);
	DEFKEYWORD(Q_pgres_nonfatal_error);
	DEFKEYWORD(Q_pgres_fatal_error);

	/* Poll status results */
	DEFKEYWORD(Q_pgres_polling_failed);
	DEFKEYWORD(Q_pgres_polling_reading);
	DEFKEYWORD(Q_pgres_polling_writing);
	DEFKEYWORD(Q_pgres_polling_ok);
	DEFKEYWORD(Q_pgres_polling_active);

#ifdef HAVE_POSTGRESQLV7
	DEFSUBR(Fpq_connect_start);
	DEFSUBR(Fpq_connect_poll);
#ifdef MULE
	DEFSUBR(Fpq_client_encoding);
	DEFSUBR(Fpq_set_client_encoding);
#endif				/* MULE */
#endif				/* HAVE_POSTGRESQLV7 */
	DEFSUBR(Fpq_set_notice_processor);

	DEFSUBR(Fpq_connection_p);
	DEFSUBR(Fpq_conn_defaults);
	DEFSUBR(Fpq_connectdb);
	DEFSUBR(Fpq_finish);
	DEFSUBR(Fpq_clear);
	DEFSUBR(Fpq_is_busy);
	DEFSUBR(Fpq_consume_input);

#ifdef HAVE_POSTGRESQLV7
	DEFSUBR(Fpq_escape_string);
	DEFSUBR(Fpq_escape_bytea);
	DEFSUBR(Fpq_unescape_bytea);
#endif

	DEFSUBR(Fpq_reset);
#ifdef HAVE_POSTGRESQLV7
	DEFSUBR(Fpq_reset_start);
	DEFSUBR(Fpq_reset_poll);
#endif
	DEFSUBR(Fpq_request_cancel);
	DEFSUBR(Fpq_connection_status);
	DEFSUBR(Fpq_connection_alive_p);

	DEFSUBR(Fpq_exec);
	DEFSUBR(Fpq_send_query);
	DEFSUBR(Fpq_result_p);
	DEFSUBR(Fpq_get_result);
	DEFSUBR(Fpq_result_status);
	DEFSUBR(Fpq_result_status_string);
	DEFSUBR(Fpq_result_error_message);
	DEFSUBR(Fpq_ntuples);
	DEFSUBR(Fpq_nfields);
	DEFSUBR(Fpq_binary_tuples);
	DEFSUBR(Fpq_fname);
	DEFSUBR(Fpq_fnumber);
	DEFSUBR(Fpq_ftype);
	DEFSUBR(Fpq_fsize);
	DEFSUBR(Fpq_fmod);
	/***/
	DEFSUBR(Fpq_get_value);
	DEFSUBR(Fpq_get_length);
	DEFSUBR(Fpq_get_is_null);
	DEFSUBR(Fpq_cmd_status);
	DEFSUBR(Fpq_cmd_tuples);
	DEFSUBR(Fpq_oid_value);

#ifdef HAVE_POSTGRESQLV7
	DEFSUBR(Fpq_set_nonblocking);
	DEFSUBR(Fpq_is_nonblocking);
	DEFSUBR(Fpq_flush);
#endif
	DEFSUBR(Fpq_notifies);

#if defined (HAVE_POSTGRESQLV7) && defined(MULE)
	DEFSUBR(Fpq_env_2_encoding);
#endif

	DEFSUBR(Fpq_lo_import);
	DEFSUBR(Fpq_lo_export);

	DEFSUBR(Fpq_make_empty_pgresult);

	/* copy in/out functions */
	DEFSUBR(Fpq_get_line);
	DEFSUBR(Fpq_put_line);
	DEFSUBR(Fpq_get_line_async);
	DEFSUBR(Fpq_put_nbytes);
	DEFSUBR(Fpq_end_copy);
}

void vars_of_postgresql(void)
{
	Fprovide(Qpostgresql);
#ifdef HAVE_POSTGRESQLV7
	Fprovide(intern("postgresqlv7"));
#endif
	Vpg_coding_system = Qnative;
	DEFVAR_LISP("pg-coding-system", &Vpg_coding_system	/*
Default Postgres client coding system.
								*/ );

	DEFVAR_LISP("pg:host", &VXPGHOST	/*
Default PostgreSQL server name.
If not set, the server running on the local host is used.  The
initial value is set from the PGHOST environment variable.
						 */ );

	DEFVAR_LISP("pg:user", &VXPGUSER	/*
Default PostgreSQL user name.
This value is used when connecting to a database for authentication.
The initial value is set from the PGUSER environment variable.
						 */ );

	DEFVAR_LISP("pg:options", &VXPGOPTIONS	/*
Default PostgreSQL user name.
This value is used when connecting to a database for authentication.
The initial value is set from the PGUSER environment variable.
						 */ );

	DEFVAR_LISP("pg:port", &VXPGPORT	/*
Default port to connect to PostgreSQL backend.
This value is used when connecting to a database.
The initial value is set from the PGPORT environment variable.
						 */ );

	DEFVAR_LISP("pg:tty", &VXPGTTY	/*
Default debugging TTY.
There is no useful setting of this variable in the XEmacs Lisp API.
The initial value is set from the PGTTY environment variable.
					 */ );

	DEFVAR_LISP("pg:database", &VXPGDATABASE	/*
Default database to connect to.
The initial value is set from the PGDATABASE environment variable.
							 */ );

	DEFVAR_LISP("pg:realm", &VXPGREALM	/*
Default kerberos realm to use for authentication.
The initial value is set from the PGREALM environment variable.
						 */ );

#ifdef MULE
	/* It's not clear whether this is any use.  My intent is to
	   autodetect the coding system from the database. */
	DEFVAR_LISP("pg:client-encoding", &VXPGCLIENTENCODING	/*
Default client encoding to use.
The initial value is set from the PGCLIENTENCODING environment variable.
								 */ );
#endif

#if !defined(HAVE_POSTGRESQLV7)
	DEFVAR_LISP("pg:authtype", &VXPGAUTHTYPE	/*
Default authentication to use.
The initial value is set from the PGAUTHTYPE environment variable.

WARNING:  This variable has gone away in versions of PostgreSQL newer
than 6.5.
							 */ );
#endif

	DEFVAR_LISP("pg:geqo", &VXPGGEQO	/*
Genetic Query Optimizer options.
The initial value is set from the PGGEQO environment variable.
						 */ );

	DEFVAR_LISP("pg:cost-index", &VXPGCOSTINDEX	/*
Default cost index options.
The initial value is set from the PGCOSTINDEX environment variable.
							 */ );

	DEFVAR_LISP("pg:cost-heap", &VXPGCOSTHEAP	/*
Default cost heap options.
The initial value is set from the PGCOSTHEAP environment variable.
							 */ );

	DEFVAR_LISP("pg:tz", &VXPGTZ	/*
Default timezone to use.
The initial value is set from the PGTZ environment variable.
					 */ );

	DEFVAR_LISP("pg:date-style", &VXPGDATESTYLE	/*
Default date style to use.
The initial value is set from the PGDATESTYLE environment variable.
							 */ );
}

/* These initializations should not be done at dump-time. */
void
init_postgresql_from_environment(void)
{
	char *p;

	if ((p = getenv("PGHOST"))) {
		VXPGHOST = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGHOST = Qnil;
	}

	if ((p = getenv("PGUSER"))) {
		VXPGUSER = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGUSER = Qnil;
	}

	if ((p = getenv("PGOPTIONS"))) {
		VXPGOPTIONS = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGOPTIONS = Qnil;
	}

	if ((p = getenv("PGPORT"))) {
		VXPGPORT = make_int(atoi(p));
	} else {
		VXPGPORT = Qnil;
	}

	if ((p = getenv("PGTTY"))) {
		VXPGTTY = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGTTY = Qnil;
	}

	if ((p = getenv("PGDATABASE"))) {
		VXPGDATABASE = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGDATABASE = Qnil;
	}

	if ((p = getenv("PGREALM"))) {
		VXPGREALM = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGREALM = Qnil;
	}

#ifdef MULE
	/* It's not clear whether this is any use.  My intent is to
	   autodetect the coding system from the database. */
	if ((p = getenv("PGCLIENTENCODING"))) {
		VXPGCLIENTENCODING = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGCLIENTENCODING = Qnil;
	}
#endif

#if !defined(HAVE_POSTGRESQLV7)
	if ((p = getenv("PGAUTHTYPE"))) {
		VXPGAUTHTYPE = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGAUTHTYPE = Qnil;
	}
#endif

	if ((p = getenv("PGGEQO"))) {
		VXPGGEQO = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGGEQO = Qnil;
	}

	if ((p = getenv("PGCOSTINDEX"))) {
		VXPGCOSTINDEX = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGCOSTINDEX = Qnil;
	}

	if ((p = getenv("PGCOSTHEAP"))) {
		VXPGCOSTHEAP = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGCOSTHEAP = Qnil;
	}

	if ((p = getenv("PGTZ"))) {
		VXPGTZ = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGTZ = Qnil;
	}

	if ((p = getenv("PGDATESTYLE"))) {
		VXPGDATESTYLE = build_ext_string(p, PG_OS_CODING);
	} else {
		VXPGDATESTYLE = Qnil;
	}
}

/* postgresql.c ends here */
