/*
  postgresql.c -- Emacs Lisp binding to libpq.so
  Copyright (C) 2000 Electrotechnical Laboratory, JAPAN.
  Licensed to the Free Software Foundation.

  Author:  SL Baur <steve@beopen.com>
  Maintainer:  SL Baur <steve@beopen.com>

Please send patches to this file to me first before submitting them to
xemacs-patches.


KNOWN PROBLEMS (Last update 15-March-2000)
+  None.

Implementation notes:
0. Supported PostgreSQL versions
   This code was developed against libpq-6.5.3 and libpq-7.0-beta1.  Earlier
   versions may work.  V7 support is more complete than V6.5 support.
1. Mule
   Non-ASCII databases have been tested on both 6.5 and 7.0.
2. Asynchronous Operation
   Starting with libpq-7.0, an asynchronous interface is offered.  This
   binding supports the asynchronous calls to a limited extent.  Since the
   XEmacs 21.2 core does not support a sensible interface to add managed but
   unreadable (by XEmacs) file descriptors to the main select code, polling
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
   For full lisp programming documentation, see the XEmacs Lisp Reference
   Manual.  For PostgreSQL documentation, see the PostgreSQL distribution.

TODO (in rough order of priority):
1. Asynchronous notifies need to be implemented to the extent they can be.
2. The large object interface needs work with Emacs buffers in addition
   to files.  Need two functions buffer->large_object, and large_object->
   buffer.
*/

/*
  Unimplemented functions: [TODO]
  PQsetNoticeProcessor

  Implemented, but undocumented functions: [TODO]
  PQgetline (copy in/out)
  PQputline (copy in/out)
  PQgetlineAsync (copy in/out Asynch.)
  PQputnbytes (copy in/out Asynch.)
  PQendcopy (copy in/out)

  Unsupported functions:
  PQsetdbLogin -- This function is deprecated, has a subset of the
   functionality of PQconnectdb, and is better done in Lisp.
  PQsetdb -- Same as for PQsetdbLogin
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
  PQfn -- "Fast path" interface
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

/* This must be portable with XEmacs 21.1 so long as it is the official
   released version of XEmacs and provides the basis of InfoDock.  The
   interface to lcrecord handling has changed with 21.2, so unfortunately
   we will need a few snippets of backwards compatibility code.
*/
#if (EMACS_MAJOR_VERSION == 21) && (EMACS_MINOR_VERSION < 2)
#define RUNNING_XEMACS_21_1 1
#endif

/* #define POSTGRES_LO_IMPORT_IS_VOID 1 */

#include "lisp.h"
#include "sysdep.h"
#include "buffer.h"
#include "postgresql.h"

#ifdef RUNNING_XEMACS_21_1 /* handle interface changes */
#define PG_OS_CODING FORMAT_FILENAME
#define TO_EXTERNAL_FORMAT(a,from,b,to,c) GET_C_STRING_EXT_DATA_ALLOCA(from,FORMAT_FILENAME,to)
#else
#ifdef MULE
#define PG_OS_CODING Fget_coding_system(Vpg_coding_system)
#else
#define PG_OS_CODING Qnative
#endif
Lisp_Object Vpg_coding_system;
#endif

#define CHECK_LIVE_CONNECTION(P) { \
	if (!P || (PQstatus (P) != CONNECTION_OK)) { \
		char *e = "bad value"; \
		if (P) e = PQerrorMessage (P); \
		error ("dead connection [%s]", e); \
	} }
#define PUKE_IF_NULL(p) { \
	if (!p) error ("bad value"); \
	}

static Lisp_Object VXPGHOST;
static Lisp_Object VXPGUSER;
static Lisp_Object VXPGOPTIONS;
static Lisp_Object VXPGPORT;
static Lisp_Object VXPGTTY; /* This needs to be blanked! */
static Lisp_Object VXPGDATABASE;
static Lisp_Object VXPGREALM;
#ifdef MULE
static Lisp_Object VXPGCLIENTENCODING;
#endif /* MULE */

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
static Lisp_Object Qpg_connection_ok, Qpg_connection_bad;
static Lisp_Object Qpg_connection_started, Qpg_connection_made;
static Lisp_Object Qpg_connection_awaiting_response, Qpg_connection_auth_ok;
static Lisp_Object Qpg_connection_setenv;

static Lisp_Object Qpqdb, Qpquser, Qpqpass, Qpqhost, Qpqport, Qpqtty;
static Lisp_Object Qpqoptions, Qpqstatus, Qpqerrormessage, Qpqbackendpid;

static Lisp_Object Qpgres_empty_query, Qpgres_command_ok, Qpgres_tuples_ok;
static Lisp_Object Qpgres_copy_out, Qpgres_copy_in, Qpgres_bad_response;
static Lisp_Object Qpgres_nonfatal_error, Qpgres_fatal_error;

static Lisp_Object Qpgres_polling_failed, Qpgres_polling_reading;
static Lisp_Object Qpgres_polling_writing, Qpgres_polling_ok;
static Lisp_Object Qpgres_polling_active;
/****/

/* PGconn is an opaque object and we need to be able to store them in
   Lisp code because libpq supports multiple connections.
*/
Lisp_Object Qpgconnp;

static Lisp_Object
make_pgconn (Lisp_PGconn *pgconn)
{
  Lisp_Object lisp_pgconn;
  XSETPGCONN (lisp_pgconn, pgconn);
  return lisp_pgconn;
}

static Lisp_Object
#ifdef RUNNING_XEMACS_21_1
mark_pgconn (Lisp_Object obj, void (*markobj) (Lisp_Object))
#else
mark_pgconn (Lisp_Object obj)
#endif
{
  return Qnil;
}

static void
print_pgconn (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[256];
  PGconn *P;
  ConnStatusType cst;
  char *host="", *db="", *user="", *port="";

  P = (XPGCONN (obj))->pgconn;

  if (!P) /* this may happen since we allow PQfinish() to be called */
    strcpy (buf, "#<PGconn DEAD>"); /* evil! */
  else if ((cst = PQstatus (P)) == CONNECTION_OK)
    {
      if (!(host = PQhost (P)))
	host = "";
      port = PQport (P);
      db = PQdb (P);
      if (!(user = PQuser (P)))
	user = "";
      sprintf (buf, "#<PGconn %s:%s %s/%s>", /* evil! */
	      !strlen (host) ? "localhost" : host,
	      port,
	      user,
	      db);
    }
  else if (cst == CONNECTION_BAD)
    strcpy (buf, "#<PGconn BAD>"); /* evil! */
  else
    strcpy (buf, "#<PGconn connecting>"); /* evil! */

  if (print_readably)
    error ("printing unreadable object %s", buf);
  else
    write_c_string (buf, printcharfun);
}

static Lisp_PGconn *
allocate_pgconn (void)
{
#ifdef RUNNING_XEMACS_21_1
  Lisp_PGconn *pgconn = alloc_lcrecord_type (Lisp_PGconn,
					     lrecord_pgconn);
#else
  Lisp_PGconn *pgconn = alloc_lcrecord_type (Lisp_PGconn,
					     &lrecord_pgconn);
#endif
  pgconn->pgconn = (PGconn *)NULL;
  return pgconn;
}

static void
finalize_pgconn (void *header, int for_disksave)
{
  Lisp_PGconn *pgconn = (Lisp_PGconn *)header;

  if (for_disksave)
    signal_simple_error ("Can't dump an emacs containing PGconn objects",
                         make_pgconn (pgconn));

  if (pgconn->pgconn)
    {
      PQfinish (pgconn->pgconn);
      pgconn->pgconn = (PGconn *)NULL;
    }
}

#ifdef RUNNING_XEMACS_21_1
DEFINE_LRECORD_IMPLEMENTATION ("pgconn", pgconn,
			       mark_pgconn, print_pgconn, finalize_pgconn,
			       NULL, NULL,
			       Lisp_PGconn);
#else
DEFINE_LRECORD_IMPLEMENTATION ("pgconn", pgconn,
			       mark_pgconn, print_pgconn, finalize_pgconn,
			       NULL, NULL,
			       0,
			       Lisp_PGconn);
#endif
/****/

/* PGresult is an opaque object and we need to be able to store them in
   Lisp code.
*/
Lisp_Object Qpgresultp;

static Lisp_Object
make_pgresult (Lisp_PGresult *pgresult)
{
  Lisp_Object lisp_pgresult;
  XSETPGRESULT (lisp_pgresult, pgresult);
  return lisp_pgresult;
}

static Lisp_Object
#ifdef RUNNING_XEMACS_21_1
mark_pgresult (Lisp_Object obj, void (*markobj) (Lisp_Object))
#else
mark_pgresult (Lisp_Object obj)
#endif
{
  return Qnil;
}

#define RESULT_TUPLES_FMT "#<PGresult %s[%d] - %s>"
#define RESULT_CMD_TUPLES_FMT "#<PGresult %s[%s] - %s>"
#define RESULT_DEFAULT_FMT "#<PGresult %s - %s>"
static void
print_pgresult (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[1024];
  PGresult *res;

  res = (XPGRESULT (obj))->pgresult;

  if (res)
    {
      switch (PQresultStatus (res))
	{
	case PGRES_TUPLES_OK:
	  /* Add number of tuples of result to output */
	  sprintf (buf, RESULT_TUPLES_FMT, /* evil! */
		   PQresStatus (PQresultStatus (res)),
		   PQntuples (res),
		   PQcmdStatus (res));
	  break;
	case PGRES_COMMAND_OK:
	  /* Add number of tuples affected by output-less command */
	  if (!strlen (PQcmdTuples (res))) goto notuples;
	  sprintf (buf, RESULT_CMD_TUPLES_FMT, /* evil! */
		   PQresStatus (PQresultStatus (res)),
		   PQcmdTuples (res),
		   PQcmdStatus (res));
	  break;
	default:
notuples:
	  /* No counts to print */
	  sprintf (buf, RESULT_DEFAULT_FMT, /* evil! */
		   PQresStatus (PQresultStatus (res)),
		   PQcmdStatus (res));
	  break;
	}
    }
  else
    strcpy (buf, "#<PGresult DEAD>"); /* evil! */

  if (print_readably)
    error ("printing unreadable object %s", buf);
  else
    write_c_string (buf, printcharfun);
}

#undef RESULT_TUPLES_FMT
#undef RESULT_CMD_TUPLES_FMT
#undef RESULT_DEFAULT_FMT

static Lisp_PGresult *
allocate_pgresult (void)
{
#ifdef RUNNING_XEMACS_21_1
  Lisp_PGresult *pgresult = alloc_lcrecord_type (Lisp_PGresult,
						 lrecord_pgresult);
#else
  Lisp_PGresult *pgresult = alloc_lcrecord_type (Lisp_PGresult,
						 &lrecord_pgresult);
#endif
  pgresult->pgresult = (PGresult *)NULL;
  return pgresult;
}

static void
finalize_pgresult (void *header, int for_disksave)
{
  Lisp_PGresult *pgresult = (Lisp_PGresult *)header;

  if (for_disksave)
    signal_simple_error ("Can't dump an emacs containing PGresult objects",
                         make_pgresult (pgresult));

  if (pgresult->pgresult)
    {
      PQclear (pgresult->pgresult);
      pgresult->pgresult = (PGresult *)NULL;
    }
}

#ifdef RUNNING_XEMACS_21_1
DEFINE_LRECORD_IMPLEMENTATION ("pgresult", pgresult,
			       mark_pgresult, print_pgresult, finalize_pgresult,
			       NULL, NULL,
			       Lisp_PGresult);
#else
DEFINE_LRECORD_IMPLEMENTATION ("pgresult", pgresult,
			       mark_pgresult, print_pgresult, finalize_pgresult,
			       NULL, NULL,
			       0,
			       Lisp_PGresult);
#endif

/***********************/

/* notices */
static void
xemacs_notice_processor (void *arg, const char *msg)
{
  warn_when_safe (Qpostgresql, Qnotice, "%s", msg);
}

/* There are four ways (as of PostgreSQL v7) to connect to a database.
   Two of them, PQsetdb and PQsetdbLogin, are deprecated.  Both of those
   routines take a number of positional parameters and are better done in Lisp.
   Note that PQconnectStart does not exist prior to v7.
*/

DEFUN ("pq-conn-defaults", Fpq_conn_defaults, 0, 0, 0, /*
Return a connection default structure.
*/
       ())
{
  /* This function can GC */
  PQconninfoOption *pcio;
  Lisp_Object temp, temp1;
  int i;

  pcio = PQconndefaults();
  if (!pcio) return Qnil; /* can never happen in libpq-7.0 */
  temp = list1 (Fcons (build_ext_string (pcio[0].keyword, PG_OS_CODING),
		       Fcons (build_ext_string (pcio[0].envvar, PG_OS_CODING),
			      Fcons (build_ext_string (pcio[0].compiled, PG_OS_CODING),
				     Fcons (build_ext_string (pcio[0].val, PG_OS_CODING),
					    Fcons (build_ext_string (pcio[0].label, PG_OS_CODING),
						   Fcons (build_ext_string (pcio[0].dispchar, PG_OS_CODING),
							  Fcons (make_int (pcio[0].dispsize), Qnil))))))));

  for (i = 1; pcio[i].keyword; i++)
    {
      temp1 = list1 (Fcons (build_ext_string (pcio[i].keyword, PG_OS_CODING),
			    Fcons (build_ext_string (pcio[i].envvar, PG_OS_CODING),
				   Fcons (build_ext_string (pcio[i].compiled, PG_OS_CODING),
					  Fcons (build_ext_string (pcio[i].val, PG_OS_CODING),
						 Fcons (build_ext_string (pcio[i].label, PG_OS_CODING),
							Fcons (build_ext_string (pcio[i].dispchar, PG_OS_CODING),
							       Fcons (make_int (pcio[i].dispsize), Qnil))))))));
      {
	Lisp_Object args[2];
	args[0] = temp;
	args[1] = temp1;
	/* Fappend GCPROs its arguments */
	temp = Fappend (2, args);
      }
    }

  return temp;
}

/* PQconnectdb Makes a new connection to a backend.
PGconn *PQconnectdb(const char *conninfo)
*/

DEFUN ("pq-connectdb", Fpq_connectdb, 1, 1, 0, /*
Make a new connection to a PostgreSQL backend.
*/
	(conninfo))
{
  PGconn *P;
  Lisp_PGconn *lisp_pgconn;
  char *error_message = "Out of Memory?";
  char *c_conninfo;

  CHECK_STRING (conninfo);

  TO_EXTERNAL_FORMAT(LISP_STRING, conninfo,
		     C_STRING_ALLOCA, c_conninfo, Qnative);
  P = PQconnectdb (c_conninfo);
  if (P && (PQstatus (P) == CONNECTION_OK))
    {
      (void)PQsetNoticeProcessor (P, xemacs_notice_processor, NULL);
      lisp_pgconn = allocate_pgconn();
      lisp_pgconn->pgconn = P;
      return make_pgconn (lisp_pgconn);
    }
  else
    {
      /* Connection failed.  Destroy the connection and signal an error. */
      char buf[BLCKSZ];
      strcpy (buf, error_message);
      if (P)
	{
	  /* storage for the error message gets erased when call PQfinish */
	  /* so we must temporarily stash it somewhere */
	  strncpy (buf, PQerrorMessage (P), sizeof (buf));
	  buf[sizeof (buf) - 1] = '\0';
	  PQfinish (P);
	}
      error ("libpq: %s", buf);
    }
}

/* PQconnectStart Makes a new asynchronous connection to a backend.
PGconn *PQconnectStart(const char *conninfo)
*/

#ifdef HAVE_POSTGRESQLV7
DEFUN ("pq-connect-start", Fpq_connect_start, 1, 1, 0, /*
Make a new asynchronous connection to a PostgreSQL backend.
*/
	(conninfo))
{
  PGconn *P;
  Lisp_PGconn *lisp_pgconn;
  char *error_message = "Out of Memory?";
  char *c_conninfo;

  CHECK_STRING (conninfo);
  TO_EXTERNAL_FORMAT (LISP_STRING, conninfo,
		      C_STRING_ALLOCA, c_conninfo, Qnative);
  P = PQconnectStart (c_conninfo);

  if (P && (PQstatus (P) != CONNECTION_BAD))
    {
      (void)PQsetNoticeProcessor (P, xemacs_notice_processor, NULL);
      lisp_pgconn = allocate_pgconn();
      lisp_pgconn->pgconn = P;

      return make_pgconn (lisp_pgconn);
    }
  else
    {
      /* capture the error message before destroying the object */
      char buf[BLCKSZ];
      strcpy (buf, error_message);
      if (P)
	{
	  strncpy (buf, PQerrorMessage (P), sizeof (buf));
	  buf[sizeof (buf) - 1] = '\0';
	  PQfinish (P);
	}
      error ("libpq: %s", buf);
    }
}

DEFUN ("pq-connect-poll", Fpq_connect_poll, 1, 1, 0, /*
Poll an asynchronous connection for completion
*/
	(conn))
{
  PGconn *P;
  PostgresPollingStatusType polling_status;

  CHECK_PGCONN (conn);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  polling_status = PQconnectPoll (P);
  switch (polling_status)
    {
    case PGRES_POLLING_FAILED:
      /* Something Bad has happened */
      {
	char *e = PQerrorMessage (P);
	error ("libpq: %s", e);
      }
    case PGRES_POLLING_OK:
      return Qpgres_polling_ok;
    case PGRES_POLLING_READING:
      return Qpgres_polling_reading;
    case PGRES_POLLING_WRITING:
      return Qpgres_polling_writing;
    case PGRES_POLLING_ACTIVE:
      return Qpgres_polling_active;
    default:
      /* they've added a new field we don't know about */
      error ("Help!  Unknown status code %08x from backend!", polling_status);
    }
}

#ifdef MULE
DEFUN ("pq-client-encoding", Fpq_client_encoding, 1, 1, 0, /*
Return client coding system.
*/
       (conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return make_int (PQclientEncoding (P));
}

DEFUN ("pq-set-client-encoding", Fpq_set_client_encoding, 2, 2, 0, /*
Set client coding system.
*/
       (conn, encoding))
{
  PGconn *P;
  int rc;
  char *c_encoding;

  CHECK_PGCONN (conn);
  CHECK_STRING (encoding);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  TO_EXTERNAL_FORMAT (LISP_STRING, encoding,
		      C_STRING_ALLOCA, c_encoding, Qnative);

  if ((rc = PQsetClientEncoding (P, c_encoding)) < 0)
    error ("bad encoding");
  else
    return make_int (rc);
}

#endif
#endif /* HAVE_POSTGRESQLV7 */

/* PQfinish Close the connection to the backend. Also frees memory
       used by the PGconn object.
void PQfinish(PGconn *conn)
*/
DEFUN ("pq-finish", Fpq_finish, 1, 1, 0, /*
Close the connection to the backend.
*/
	(conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  PUKE_IF_NULL (P);

  PQfinish (P);
  /* #### PQfinish deallocates the PGconn structure, so we now have a
     dangling pointer. */
  /* Genocided all @'s ... */
  (XPGCONN (conn))->pgconn = (PGconn *)NULL; /* You feel DEAD inside */
  return Qnil;
}

DEFUN ("pq-clear", Fpq_clear, 1, 1, 0, /*
Forcibly erase a PGresult object.
*/
       (res))
{
  PGresult *R;

  CHECK_PGRESULT (res);
  R = (XPGRESULT (res))->pgresult;
  PUKE_IF_NULL (R);

  PQclear (R);
  /* Genocided all @'s ... */
  (XPGRESULT (res))->pgresult = (PGresult *)NULL; /* You feel DEAD inside */

  return Qnil;
}

DEFUN ("pq-is-busy", Fpq_is_busy, 1, 1, 0, /*
Return t if PQgetResult would block waiting for input.
*/
	(conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return PQisBusy (P) ? Qt : Qnil;
}

DEFUN ("pq-consume-input", Fpq_consume_input, 1, 1, 0, /*
Consume any available input from the backend.
Returns nil if something bad happened.
*/
	(conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return PQconsumeInput (P) ? Qt : Qnil;
}

/* PQreset Reset the communication port with the backend.
void PQreset(PGconn *conn)
*/
DEFUN ("pq-reset", Fpq_reset, 1, 1, 0, /*
Reset the connection to the backend.
This function will close the connection to the backend and attempt to
reestablish a new connection to the same postmaster, using all the same
parameters previously used.  This may be useful for error recovery if a
working connection is lost.
*/
	(conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  PUKE_IF_NULL (P);/* we can resurrect a BAD connection, but not a dead one. */

  PQreset (P);

  return Qnil;
}

#ifdef HAVE_POSTGRESQLV7
DEFUN ("pq-reset-start", Fpq_reset_start, 1, 1, 0, /*
Reset connection to the backend asynchronously.
*/
       (conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  if (PQresetStart (P)) return Qt;
  {
    char *e = PQerrorMessage (P);
    error ("libpq: %s", e);
  }
}

DEFUN ("pq-reset-poll", Fpq_reset_poll, 1, 1, 0, /*
Poll an asynchronous reset for completion.
*/
	(conn))
{
  PGconn *P;
  PostgresPollingStatusType polling_status;

  CHECK_PGCONN (conn);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  polling_status = PQresetPoll (P);
  switch (polling_status)
    {
    case PGRES_POLLING_FAILED:
      /* Something Bad has happened */
      {
	char *e = PQerrorMessage (P);
	error ("libpq: %s", e);
      }
    case PGRES_POLLING_OK:
      return Qpgres_polling_ok;
    case PGRES_POLLING_READING:
      return Qpgres_polling_reading;
    case PGRES_POLLING_WRITING:
      return Qpgres_polling_writing;
    case PGRES_POLLING_ACTIVE:
      return Qpgres_polling_active;
    default:
      /* they've added a new field we don't know about */
      error ("Help!  Unknown status code %08x from backend!", polling_status);
    }
}
#endif

DEFUN ("pq-request-cancel", Fpq_request_cancel, 1, 1, 0, /*
Attempt to request cancellation of the current operation.

The return value is t if the cancel request was successfully
dispatched, nil if not (in which case conn->errorMessage is set).
Note: successful dispatch is no guarantee that there will be any effect at
the backend.  The application must read the operation result as usual.
*/
       (conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return PQrequestCancel (P) ? Qt : Qnil;
}

/* accessor function for the PGconn object */
DEFUN ("pq-pgconn", Fpq_pgconn, 2, 2, 0, /*
Accessor function for the PGconn object.
Currently recognized symbols for the field:
pq::db            Database name
pq::user          Database user name
pq::pass          Database user's password
pq::host          Hostname of PostgreSQL backend connected to
pq::port          TCP port number of connection
pq::tty           Debugging TTY (not used in Emacs)
pq::options       Additional backend options
pq::status        Connection status (either OK or BAD)
pq::error-message Last error message from the backend
pq::backend-pid   Process ID of backend process
*/
	(conn, field))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  PUKE_IF_NULL (P); /* BAD connections still have state to query */

  if (EQ(field, Qpqdb))
    /* PQdb Returns the database name of the connection.
       char *PQdb(PGconn *conn)
     */
    return build_ext_string (PQdb(P), PG_OS_CODING);
  else if (EQ (field, Qpquser))
    /* PQuser Returns the user name of the connection.
       char *PQuser(PGconn *conn)
     */
    return build_ext_string (PQuser(P), PG_OS_CODING);
  else if (EQ (field, Qpqpass))
    /* PQpass Returns the password of the connection.
       char *PQpass(PGconn *conn)
     */
    return build_ext_string (PQpass(P), PG_OS_CODING);
  else if (EQ (field, Qpqhost))
    /* PQhost Returns the server host name of the connection.
       char *PQhost(PGconn *conn)
     */
    return build_ext_string (PQhost(P), PG_OS_CODING);
  else if (EQ (field, Qpqport))
    {
      char *p;
      /* PQport Returns the port of the connection.
         char *PQport(PGconn *conn)
       */
      if ((p = PQport(P)))
	return make_int(atoi(p));
      else
        return make_int(-1);
    }
  else if (EQ (field, Qpqtty))
    /* PQtty Returns the debug tty of the connection.
       char *PQtty(PGconn *conn)
     */
    return build_ext_string (PQtty(P), PG_OS_CODING);
  else if (EQ (field, Qpqoptions))
  /* PQoptions Returns the backend options used in the connection.
     char *PQoptions(PGconn *conn)
   */
    return build_ext_string (PQoptions(P), PG_OS_CODING);
  else if (EQ (field, Qpqstatus))
    {
      ConnStatusType cst;
      /* PQstatus Returns the status of the connection. The status can be
	 CONNECTION_OK or CONNECTION_BAD.
	 ConnStatusType PQstatus(PGconn *conn)
      */
      switch ((cst = PQstatus (P)))
	{
	case CONNECTION_OK: return Qpg_connection_ok;
	case CONNECTION_BAD: return Qpg_connection_bad;
#ifdef HAVE_POSTGRESQLV7
	case CONNECTION_STARTED: return Qpg_connection_started;
	case CONNECTION_MADE: return Qpg_connection_made;
	case CONNECTION_AWAITING_RESPONSE: return Qpg_connection_awaiting_response;
	case CONNECTION_AUTH_OK: return Qpg_connection_auth_ok;
	case CONNECTION_SETENV: return Qpg_connection_setenv;
#endif /* HAVE_POSTGRESQLV7 */
	default:
	  /* they've added a new field we don't know about */
	  error ("Help!  Unknown connection status code %08x from backend!", cst);
	}
    }
  else if (EQ (field, Qpqerrormessage))
    /* PQerrorMessage Returns the error message most recently generated
       by an operation on the connection.
       char *PQerrorMessage(PGconn* conn);
     */
    return build_ext_string (PQerrorMessage(P), PG_OS_CODING);
  else if (EQ (field, Qpqbackendpid))
    /* PQbackendPID Returns the process ID of the backend server handling
       this connection.
       int PQbackendPID(PGconn *conn);
     */
    return make_int (PQbackendPID(P));
  else
    error ("bad PGconn accessor");
}

/* Query functions */
DEFUN ("pq-exec", Fpq_exec, 2, 2, 0, /*
Submit a query to Postgres and wait for the result.
*/
	(conn, query))
{
  PGconn *P;
  Lisp_PGresult *lisp_pgresult;
  PGresult *R;
  char *c_query;

  CHECK_PGCONN (conn);
  CHECK_STRING (query);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  TO_EXTERNAL_FORMAT (LISP_STRING, query,
		      C_STRING_ALLOCA, c_query, Qnative);

  R = PQexec (P, c_query);
  {
    char *tag, buf[BLCKSZ];

    if (!R) error ("query: out of memory");
    else
      switch (PQresultStatus (R))
	{
	case PGRES_BAD_RESPONSE:
	  tag = "bad response [%s]";
	  goto err;
	case PGRES_NONFATAL_ERROR:
	  tag = "non-fatal error [%s]";
	  goto err;
	case PGRES_FATAL_ERROR:
	  tag = "fatal error [%s]";
err:
	  strncpy (buf, PQresultErrorMessage (R), sizeof (buf));
	  buf [sizeof (buf) - 1] = '\0';
	  PQclear (R);
	  error (tag, buf);
	  /*NOTREACHED*/
	default:
	  break;
	}
  }

  lisp_pgresult = allocate_pgresult ();
  lisp_pgresult->pgresult = R;

  return make_pgresult (lisp_pgresult);
}

DEFUN ("pq-send-query", Fpq_send_query, 2, 2, 0, /*
Submit a query to Postgres and don't wait for the result.
Returns: t if successfully submitted
         nil if error (conn->errorMessage is set)
*/
	(conn, query))
{
  PGconn *P;
  char *c_query;

  CHECK_PGCONN (conn);
  CHECK_STRING (query);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  TO_EXTERNAL_FORMAT (LISP_STRING, query,
		      C_STRING_ALLOCA, c_query, Qnative);

  if (PQsendQuery (P, c_query)) return Qt;
  else error ("async query: %s", PQerrorMessage (P));
}

DEFUN ("pq-get-result", Fpq_get_result, 1, 1, 0, /*
Retrieve an asynchronous result from a query.
NIL is returned when no more query work remains.
*/
	(conn))
{
  PGconn *P;
  Lisp_PGresult *lisp_pgresult;
  PGresult *R;

  CHECK_PGCONN (conn);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  R = PQgetResult (P);
  if (!R) return Qnil; /* not an error, there's no more data to get */

  {
    char *tag, buf[BLCKSZ];

    switch (PQresultStatus (R))
      {
      case PGRES_BAD_RESPONSE:
	tag = "bad response [%s]";
	goto err;
      case PGRES_NONFATAL_ERROR:
	tag = "non-fatal error [%s]";
	goto err;
      case PGRES_FATAL_ERROR:
	tag = "fatal error [%s]";
err:
	strncpy (buf, PQresultErrorMessage (R), sizeof (buf));
	buf[sizeof (buf) - 1] = '\0';
	PQclear (R);
	error (tag, buf);
	/*NOTREACHED*/
      default:
	break;
      }
  }

  lisp_pgresult = allocate_pgresult();
  lisp_pgresult->pgresult = R;

  return make_pgresult (lisp_pgresult);
}

DEFUN ("pq-result-status", Fpq_result_status, 1, 1, 0, /*
Return result status of the query.
*/
	(result))
{
  PGresult *R;
  ExecStatusType est;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  switch ((est = PQresultStatus (R))) {
  case PGRES_EMPTY_QUERY: return Qpgres_empty_query;
  case PGRES_COMMAND_OK: return Qpgres_command_ok;
  case PGRES_TUPLES_OK: return Qpgres_tuples_ok;
  case PGRES_COPY_OUT: return Qpgres_copy_out;
  case PGRES_COPY_IN: return Qpgres_copy_in;
  case PGRES_BAD_RESPONSE: return Qpgres_bad_response;
  case PGRES_NONFATAL_ERROR: return Qpgres_nonfatal_error;
  case PGRES_FATAL_ERROR: return Qpgres_fatal_error;
  default:
    /* they've added a new field we don't know about */
    error ("Help!  Unknown exec status code %08x from backend!", est);
  }
}

DEFUN ("pq-res-status", Fpq_res_status, 1, 1, 0, /*
Return stringified result status of the query.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return build_ext_string (PQresStatus (PQresultStatus (R)), PG_OS_CODING);
}

/* Sundry PGresult accessor functions */
DEFUN ("pq-result-error-message", Fpq_result_error_message, 1, 1, 0, /*
Return last message associated with the query.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return build_ext_string (PQresultErrorMessage (R), PG_OS_CODING);
}

DEFUN ("pq-ntuples", Fpq_ntuples, 1, 1, 0, /*
Return the number of tuples (instances) in the query result.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return make_int (PQntuples (R));
}

DEFUN ("pq-nfields", Fpq_nfields, 1, 1, 0, /*
Return the number of fields (attributes) in each tuple of the query result.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return make_int (PQnfields (R));
}

DEFUN ("pq-binary-tuples", Fpq_binary_tuples, 1, 1, 0, /*
Return t if the query result contains binary data, nil otherwise.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return (PQbinaryTuples (R)) ? Qt : Qnil;
}

DEFUN ("pq-fname", Fpq_fname, 2, 2, 0, /*
Return the field (attribute) name associated with the given field index.
Field indices start at 0.
*/
	(result, field_index))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (field_index);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return build_ext_string (PQfname (R, XINT (field_index)), PG_OS_CODING);
}

DEFUN ("pq-fnumber", Fpq_fnumber, 2, 2, 0, /*
Return the number of fields (attributes) in each tuple of the query result.
*/
	(result, field_name))
{
  PGresult *R;
  char *c_field_name;

  CHECK_PGRESULT (result);
  CHECK_STRING (field_name);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  TO_EXTERNAL_FORMAT (LISP_STRING, field_name,
		      C_STRING_ALLOCA, c_field_name, Qnative);

  return make_int (PQfnumber (R, c_field_name));
}

DEFUN ("pq-ftype", Fpq_ftype, 2, 2, 0, /*
Return the field type associated with the given field index.
The integer returned is the internal coding of the type.  Field indices
start at 0.
*/
	(result, field_num))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (field_num);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return make_int (PQftype (R, XINT (field_num)));
}

DEFUN ("pq-fsize", Fpq_fsize, 2, 2, 0, /*
Return the field size in bytes associated with the given field index.
Field indices start at 0.
*/
	(result, field_index))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (field_index);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return make_int (PQftype (R, XINT (field_index)));
}

DEFUN ("pq-fmod", Fpq_fmod, 2, 2, 0, /*
Return the type modifier associated with a field.
Field indices start at 0.
*/
	(result, field_index))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (field_index);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return make_int (PQfmod (R, XINT (field_index)));
}

DEFUN ("pq-get-value", Fpq_get_value, 3, 3, 0, /*
Return a single field (attribute) value of one tuple of a PGresult.
Tuple and field indices start at 0.
*/
	(result, tup_num, field_num))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (tup_num);
  CHECK_INT (field_num);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return build_ext_string (PQgetvalue (R, XINT (tup_num), XINT (field_num)),
			   PG_OS_CODING);
}

DEFUN ("pq-get-length", Fpq_get_length, 3, 3, 0, /*
Returns the length of a field value in bytes.
If result is binary, i.e. a result of a binary portal, then the
length returned does NOT include the size field of the varlena.  (The
data returned by PQgetvalue doesn't either.)
*/
	(result, tup_num, field_num))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (tup_num);
  CHECK_INT (field_num);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return make_int (PQgetlength (R, XINT (tup_num), XINT (field_num)));
}

DEFUN ("pq-get-is-null", Fpq_get_is_null, 3, 3, 0, /*
Returns the null status of a field value.
*/
	(result, tup_num, field_num))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  CHECK_INT (tup_num);
  CHECK_INT (field_num);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return PQgetisnull (R, XINT (tup_num), XINT (field_num)) ? Qt : Qnil;
}

DEFUN ("pq-cmd-status", Fpq_cmd_status, 1, 1, 0, /*
Returns the command status string from the SQL command that generated the result.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return build_ext_string (PQcmdStatus (R), PG_OS_CODING);
}

DEFUN ("pq-cmd-tuples", Fpq_cmd_tuples, 1, 1, 0, /*
Returns the number of rows affected by the SQL command.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

  return build_ext_string (PQcmdTuples (R), PG_OS_CODING);
}

DEFUN ("pq-oid-value", Fpq_oid_value, 1, 1, 0, /*
Returns the object id of the tuple inserted.
*/
	(result))
{
  PGresult *R;

  CHECK_PGRESULT (result);
  R = (XPGRESULT (result))->pgresult;
  PUKE_IF_NULL (R);

#ifdef HAVE_POSTGRESQLV7
  return make_int (PQoidValue (R));
#else
  /* Use the old interface */
  return make_int (atoi (PQoidStatus (R)));
#endif
}

#ifdef HAVE_POSTGRESQLV7
DEFUN ("pq-set-nonblocking", Fpq_set_nonblocking, 2, 2, 0, /*
Sets the PGconn's database connection non-blocking if the arg is TRUE
or makes it non-blocking if the arg is FALSE, this will not protect
you from PQexec(), you'll only be safe when using the non-blocking API.

Needs to be called only on a connected database connection.
*/
       (conn, arg))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return make_int (PQsetnonblocking (P, !NILP (arg)));
}

DEFUN ("pq-is-nonblocking", Fpq_is_nonblocking, 1, 1, 0, /*
Return the blocking status of the database connection.
*/
       (conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return PQisnonblocking (P) ? Qt : Qnil;
}

DEFUN ("pq-flush", Fpq_flush, 1, 1, 0, /*
Force the write buffer to be written (or at least try).
*/
       (conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return make_int (PQflush (P));
}
#endif

DEFUN ("pq-notifies", Fpq_notifies, 1, 1, 0, /*
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

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  PGN = PQnotifies (P);
  if (!PGN)
    return Qnil;
  else
  {
    Lisp_Object temp;

    temp = list2 (build_ext_string (PGN->relname, PG_OS_CODING), make_int (PGN->be_pid));
    free ((void *)PGN);
    return temp;
  }
}

#if defined (HAVE_POSTGRESQLV7) && defined(MULE)
DEFUN ("pq-env-2-encoding", Fpq_env_2_encoding, 0, 0, 0, /*
Get encoding id from environment variable PGCLIENTENCODING.
*/
       ())
{
  return make_int (PQenv2encoding ());
}
#endif /* MULE */

DEFUN ("pq-lo-import", Fpq_lo_import, 2, 2, 0, /*
*/
       (conn, filename))
{
  PGconn *P;
  char *c_filename;

  CHECK_PGCONN (conn);
  CHECK_STRING (filename);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  TO_EXTERNAL_FORMAT (LISP_STRING, filename,
		      C_STRING_ALLOCA, c_filename,
		      Qfile_name);

  return make_int ((int)lo_import (P, c_filename));
}

DEFUN ("pq-lo-export", Fpq_lo_export, 3, 3, 0, /*
*/
       (conn, oid, filename))
{
  PGconn *P;
  char *c_filename;

  CHECK_PGCONN (conn);
  CHECK_INT (oid);
  CHECK_STRING (filename);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  TO_EXTERNAL_FORMAT (LISP_STRING, filename,
		      C_STRING_ALLOCA, c_filename, Qfile_name);

  return make_int ((int)lo_export (P, XINT (oid), c_filename));
}

DEFUN ("pq-make-empty-pgresult", Fpq_make_empty_pgresult, 2, 2, 0, /*
Make an empty PGresult object with the given status.
*/
       (conn, status))
{
  PGconn *P;
  Lisp_PGresult *lpgr;
  PGresult *R;
  ExecStatusType est;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P); /* needed here? */

  if (EQ (status, Qpgres_empty_query)) est = PGRES_EMPTY_QUERY;
  else if (EQ (status, Qpgres_command_ok)) est = PGRES_COMMAND_OK;
  else if (EQ (status, Qpgres_tuples_ok)) est = PGRES_TUPLES_OK;
  else if (EQ (status, Qpgres_copy_out)) est = PGRES_COPY_OUT;
  else if (EQ (status, Qpgres_copy_in)) est = PGRES_COPY_IN;
  else if (EQ (status, Qpgres_bad_response)) est = PGRES_BAD_RESPONSE;
  else if (EQ (status, Qpgres_nonfatal_error)) est = PGRES_NONFATAL_ERROR;
  else if (EQ (status, Qpgres_fatal_error)) est = PGRES_FATAL_ERROR;
  else signal_simple_error ("bad status symbol", status);

  R = PQmakeEmptyPGresult (P, est);
  if (!R) error ("out of memory?");

  lpgr = allocate_pgresult ();
  lpgr->pgresult = R;

  return make_pgresult (lpgr);
}

DEFUN ("pq-get-line", Fpq_get_line, 1, 1, 0, /*
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
  char buffer[BLCKSZ]; /* size of a Postgres disk block */
  PGconn *P;
  int ret;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  ret = PQgetline (P, buffer, sizeof (buffer));

  return Fcons (make_int (ret), build_ext_string (buffer, PG_OS_CODING));
}

DEFUN ("pq-put-line", Fpq_put_line, 2, 2, 0, /*
Send a line to the server in copy out operation.

Returns t if the operation succeeded, nil otherwise.
*/
       (conn, string))
{
  PGconn *P;
  char *c_string;

  CHECK_PGCONN (conn);
  CHECK_STRING (string);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);
  TO_EXTERNAL_FORMAT (LISP_STRING, string,
		      C_STRING_ALLOCA, c_string, Qnative);

  return !PQputline (P, c_string) ? Qt : Qnil;
}

DEFUN ("pq-get-line-async", Fpq_get_line_async, 1, 1, 0, /*
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

  CHECK_PGCONN (conn);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  ret = PQgetlineAsync (P, buffer, sizeof (buffer));

  if (ret == -1) return Qt; /* done! */
  else if (!ret) return Qnil; /* no data yet */
  else return Fcons (make_int (ret),
		     make_ext_string ((Extbyte *) buffer, ret, PG_OS_CODING));
}

DEFUN ("pq-put-nbytes", Fpq_put_nbytes, 2, 2, 0, /*
Asynchronous copy out.
*/
       (conn, data))
{
  /* NULs are not allowed.  I don't think this matters at this time. */
  PGconn *P;
  char *c_data;

  CHECK_PGCONN (conn);
  CHECK_STRING (data);

  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);
  TO_EXTERNAL_FORMAT (LISP_STRING, data,
		      C_STRING_ALLOCA, c_data, Qnative);

  return !PQputnbytes (P, c_data, strlen (c_data)) ? Qt : Qnil;
}

DEFUN ("pq-end-copy", Fpq_end_copy, 1, 1, 0, /*
End a copying operation.
*/
       (conn))
{
  PGconn *P;

  CHECK_PGCONN (conn);
  P = (XPGCONN (conn))->pgconn;
  CHECK_LIVE_CONNECTION (P);

  return PQendcopy (P) ? Qt : Qnil;
}

void
syms_of_postgresql(void)
{
#ifndef RUNNING_XEMACS_21_1
  INIT_LRECORD_IMPLEMENTATION (pgconn);
  INIT_LRECORD_IMPLEMENTATION (pgresult);
#endif
  defsymbol (&Qpostgresql, "postgresql");

  /* opaque exported types */
  defsymbol (&Qpgconnp, "pgconnp");
  defsymbol (&Qpgresultp, "pgresultp");

  /* connection status types */
  defsymbol (&Qpg_connection_ok, "pg::connection-ok");
  defsymbol (&Qpg_connection_bad, "pg::connection-bad");
  defsymbol (&Qpg_connection_started, "pg::connection-started");
  defsymbol (&Qpg_connection_made, "pg::connection-made");
  defsymbol (&Qpg_connection_awaiting_response, "pg::connection-awaiting-response");
  defsymbol (&Qpg_connection_auth_ok, "pg::connection-auth-ok");
  defsymbol (&Qpg_connection_setenv, "pg::connection-setenv");

  /* Fields of PGconn */
  defsymbol (&Qpqdb, "pq::db");
  defsymbol (&Qpquser, "pq::user");
  defsymbol (&Qpqpass, "pq::pass");
  defsymbol (&Qpqhost, "pq::host");
  defsymbol (&Qpqport, "pq::port");
  defsymbol (&Qpqtty, "pq::tty");
  defsymbol (&Qpqoptions, "pq::options");
  defsymbol (&Qpqstatus, "pq::status");
  defsymbol (&Qpqerrormessage, "pq::error-message");
  defsymbol (&Qpqbackendpid, "pq::backend-pid");

  /* Query status results */
  defsymbol (&Qpgres_empty_query, "pgres::empty-query");
  defsymbol (&Qpgres_command_ok, "pgres::command-ok");
  defsymbol (&Qpgres_tuples_ok, "pgres::tuples-ok");
  defsymbol (&Qpgres_copy_out, "pgres::copy-out");
  defsymbol (&Qpgres_copy_in, "pgres::copy-in");
  defsymbol (&Qpgres_bad_response, "pgres::bad-response");
  defsymbol (&Qpgres_nonfatal_error, "pgres::nonfatal-error");
  defsymbol (&Qpgres_fatal_error, "pgres::fatal-error");

  /* Poll status results */
  defsymbol (&Qpgres_polling_failed, "pgres::polling-failed");
  defsymbol (&Qpgres_polling_reading, "pgres::polling-reading");
  defsymbol (&Qpgres_polling_writing, "pgres::polling-writing");
  defsymbol (&Qpgres_polling_ok, "pgres::polling-ok");
  defsymbol (&Qpgres_polling_active, "pgres::polling-active");

#ifdef HAVE_POSTGRESQLV7
  DEFSUBR (Fpq_connect_start);
  DEFSUBR (Fpq_connect_poll);
#ifdef MULE
  DEFSUBR (Fpq_client_encoding);
  DEFSUBR (Fpq_set_client_encoding);
#endif /* MULE */
#endif /* HAVE_POSTGRESQLV7 */
  DEFSUBR (Fpq_conn_defaults);
  DEFSUBR (Fpq_connectdb);
  DEFSUBR (Fpq_finish);
  DEFSUBR (Fpq_clear);
  DEFSUBR (Fpq_is_busy);
  DEFSUBR (Fpq_consume_input);

  DEFSUBR (Fpq_reset);
#ifdef HAVE_POSTGRESQLV7
  DEFSUBR (Fpq_reset_start);
  DEFSUBR (Fpq_reset_poll);
#endif
  DEFSUBR (Fpq_request_cancel);
  DEFSUBR (Fpq_pgconn);

  DEFSUBR (Fpq_exec);
  DEFSUBR (Fpq_send_query);
  DEFSUBR (Fpq_get_result);
  DEFSUBR (Fpq_result_status);
  DEFSUBR (Fpq_res_status);
  DEFSUBR (Fpq_result_error_message);
  DEFSUBR (Fpq_ntuples);
  DEFSUBR (Fpq_nfields);
  DEFSUBR (Fpq_binary_tuples);
  DEFSUBR (Fpq_fname);
  DEFSUBR (Fpq_fnumber);
  DEFSUBR (Fpq_ftype);
  DEFSUBR (Fpq_fsize);
  DEFSUBR (Fpq_fmod);
  /***/
  DEFSUBR (Fpq_get_value);
  DEFSUBR (Fpq_get_length);
  DEFSUBR (Fpq_get_is_null);
  DEFSUBR (Fpq_cmd_status);
  DEFSUBR (Fpq_cmd_tuples);
  DEFSUBR (Fpq_oid_value);

#ifdef HAVE_POSTGRESQLV7
  DEFSUBR (Fpq_set_nonblocking);
  DEFSUBR (Fpq_is_nonblocking);
  DEFSUBR (Fpq_flush);
#endif
  DEFSUBR (Fpq_notifies);

#if defined (HAVE_POSTGRESQLV7) && defined(MULE)
  DEFSUBR (Fpq_env_2_encoding);
#endif

  DEFSUBR (Fpq_lo_import);
  DEFSUBR (Fpq_lo_export);

  DEFSUBR (Fpq_make_empty_pgresult);

  /* copy in/out functions */
  DEFSUBR (Fpq_get_line);
  DEFSUBR (Fpq_put_line);
  DEFSUBR (Fpq_get_line_async);
  DEFSUBR (Fpq_put_nbytes);
  DEFSUBR (Fpq_end_copy);
}

void
vars_of_postgresql(void)
{
  Fprovide (Qpostgresql);
#ifdef HAVE_POSTGRESQLV7
  Fprovide (intern ("postgresqlv7"));
#endif
#ifndef RUNNING_XEMACS_21_1
  Vpg_coding_system = Qnative;
  DEFVAR_LISP ("pg-coding-system", &Vpg_coding_system /*
Default Postgres client coding system.
*/ );
#endif

  DEFVAR_LISP ("pg:host", &VXPGHOST /*
Default PostgreSQL server name.
If not set, the server running on the local host is used.  The
initial value is set from the PGHOST environment variable.
*/ );

  DEFVAR_LISP ("pg:user", &VXPGUSER /*
Default PostgreSQL user name.
This value is used when connecting to a database for authentication.
The initial value is set from the PGUSER environment variable.
*/ );

  DEFVAR_LISP ("pg:options", &VXPGOPTIONS /*
Default PostgreSQL user name.
This value is used when connecting to a database for authentication.
The initial value is set from the PGUSER environment variable.
*/ );

  DEFVAR_LISP ("pg:port", &VXPGPORT /*
Default port to connect to PostgreSQL backend.
This value is used when connecting to a database.
The initial value is set from the PGPORT environment variable.
*/ );

  DEFVAR_LISP ("pg:tty", &VXPGTTY /*
Default debugging TTY.
There is no useful setting of this variable in the XEmacs Lisp API.
The initial value is set from the PGTTY environment variable.
*/ );

  DEFVAR_LISP ("pg:database", &VXPGDATABASE /*
Default database to connect to.
The initial value is set from the PGDATABASE environment variable.
*/ );

  DEFVAR_LISP ("pg:realm", &VXPGREALM /*
Default kerberos realm to use for authentication.
The initial value is set from the PGREALM environment variable.
*/ );

#ifdef MULE
  /* It's not clear whether this is any use.  My intent is to
     autodetect the coding system from the database. */
  DEFVAR_LISP ("pg:client-encoding", &VXPGCLIENTENCODING /*
Default client encoding to use.
The initial value is set from the PGCLIENTENCODING environment variable.
*/ );
#endif

#if !defined(HAVE_POSTGRESQLV7)
  DEFVAR_LISP ("pg:authtype", &VXPGAUTHTYPE /*
Default authentication to use.
The initial value is set from the PGAUTHTYPE environment variable.

WARNING:  This variable has gone away in versions of PostgreSQL newer
than 6.5.
*/ );
#endif

  DEFVAR_LISP ("pg:geqo", &VXPGGEQO /*
Genetic Query Optimizer options.
The initial value is set from the PGGEQO environment variable.
*/ );

  DEFVAR_LISP ("pg:cost-index", &VXPGCOSTINDEX /*
Default cost index options.
The initial value is set from the PGCOSTINDEX environment variable.
*/ );

  DEFVAR_LISP ("pg:cost-heap", &VXPGCOSTHEAP /*
Default cost heap options.
The initial value is set from the PGCOSTHEAP environment variable.
*/ );

  DEFVAR_LISP ("pg:tz", &VXPGTZ /*
Default timezone to use.
The initial value is set from the PGTZ environment variable.
*/ );

  DEFVAR_LISP ("pg:date-style", &VXPGDATESTYLE /*
Default date style to use.
The initial value is set from the PGDATESTYLE environment variable.
*/ );
}

/* These initializations should not be done at dump-time. */
void
init_postgresql_from_environment(void)
{
  char *p;

  if ((p = getenv ("PGHOST")))
    {
      VXPGHOST = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGHOST = Qnil;
    }

  if ((p = getenv ("PGUSER")))
    {
      VXPGUSER = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGUSER = Qnil;
    }

  if ((p = getenv ("PGOPTIONS")))
    {
      VXPGOPTIONS = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGOPTIONS = Qnil;
    }

  if ((p = getenv ("PGPORT")))
    {
      VXPGPORT = make_int (atoi (p));
    }
  else
    {
      VXPGPORT = Qnil;
    }

  if ((p = getenv ("PGTTY")))
    {
      VXPGTTY = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGTTY = Qnil;
    }

  if ((p = getenv ("PGDATABASE")))
    {
      VXPGDATABASE = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGDATABASE = Qnil;
    }

  if ((p = getenv ("PGREALM")))
    {
      VXPGREALM = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGREALM = Qnil;
    }

#ifdef MULE
  /* It's not clear whether this is any use.  My intent is to
     autodetect the coding system from the database. */
  if ((p = getenv ("PGCLIENTENCODING")))
    {
      VXPGCLIENTENCODING = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGCLIENTENCODING = Qnil;
    }
#endif

#if !defined(HAVE_POSTGRESQLV7)
  if ((p = getenv ("PGAUTHTYPE")))
    {
      VXPGAUTHTYPE = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGAUTHTYPE = Qnil;
    }
#endif

  if ((p = getenv ("PGGEQO")))
    {
      VXPGGEQO = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGGEQO = Qnil;
    }

  if ((p = getenv ("PGCOSTINDEX")))
    {
      VXPGCOSTINDEX = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGCOSTINDEX = Qnil;
    }

  if ((p = getenv ("PGCOSTHEAP")))
    {
      VXPGCOSTHEAP = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGCOSTHEAP = Qnil;
    }

  if ((p = getenv ("PGTZ")))
    {
      VXPGTZ = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGTZ = Qnil;
    }

  if ((p = getenv ("PGDATESTYLE")))
    {
      VXPGDATESTYLE = build_ext_string (p, PG_OS_CODING);
    }
  else
    {
      VXPGDATESTYLE = Qnil;
    }
}

