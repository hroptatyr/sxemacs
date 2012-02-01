/* Database access routines
   Copyright (C) 1996, William M. Perry

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


/* Synched up with: Not in FSF. */

/* Written by Bill Perry */
/* Substantially rewritten by Martin Buchholz */
/* db 2.x support added by Andreas Jaeger */

#include <config.h>
#include "lisp.h"
#include "sysfile.h"
#include "buffer.h"
#include <errno.h>

#ifndef HAVE_DATABASE
#error HAVE_DATABASE not defined!!
#endif

#include "database.h"		/* Our include file */

#ifdef HAVE_BERKELEY_DB
/* Work around Berkeley DB's use of int types which are defined
   slightly differently in the not quite yet standard <inttypes.h>.
   See db.h for details of why we're resorting to this... */
/* glibc 2.1 doesn't have this problem with DB 2.x */
#if !(defined __GLIBC__ && __GLIBC_MINOR__ >= 1)
#ifdef HAVE_INTTYPES_H
#define __BIT_TYPES_DEFINED__
#include <inttypes.h>
#ifndef __FreeBSD__
typedef uint8_t u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
#ifdef WE_DONT_NEED_QUADS
typedef uint64_t u_int64_t;
#endif
#endif				/* WE_DONT_NEED_QUADS */
#endif				/* HAVE_INTTYPES_H */
#endif				/* !(defined __GLIBC__ && __GLIBC_MINOR__ >= 1) */
/* Berkeley DB wants __STDC__ to be defined; else if does `#define const' */
#if ! defined (__STDC__) && ! defined(__cplusplus)
#define __STDC__ 0
#endif
#if defined(DB_H_FILE)
#include DB_H_FILE		/* Berkeley db's header file */
#endif
#ifndef DB_VERSION_MAJOR
# define DB_VERSION_MAJOR 1
#endif				/* DB_VERSION_MAJOR */
#ifndef DB_VERSION_MINOR
# define DB_VERSION_MINOR 0
#endif				/* DB_VERSION_MINOR */
Lisp_Object Qberkeley_db;
Lisp_Object Qhash, Qbtree, Qrecno, Qunknown;
#if DB_VERSION_MAJOR > 2
Lisp_Object Qqueue;
#endif
#endif				/* HAVE_BERKELEY_DB */

#ifdef HAVE_DBM
#include <ndbm.h>
Lisp_Object Qdbm;
#endif				/* HAVE_DBM */

#ifdef MULE
/* #### The following should be settable on a per-database level.
   But the whole coding-system infrastructure should be rewritten someday.
   We really need coding-system aliases. -- martin */
Lisp_Object Vdatabase_coding_system;
#endif

Lisp_Object Qdatabasep;

typedef struct {
	Lisp_Object(*get_subtype) (Lisp_Database *);
	Lisp_Object(*get_type) (Lisp_Database *);
	Lisp_Object(*get) (Lisp_Database *, Lisp_Object);
	int (*put) (Lisp_Database *, Lisp_Object, Lisp_Object, Lisp_Object);
	int (*rem) (Lisp_Database *, Lisp_Object);
	void (*map) (Lisp_Database *, Lisp_Object);
	void (*close) (Lisp_Database *);
	 Lisp_Object(*last_error) (Lisp_Database *);
} DB_FUNCS;

struct Lisp_Database {
	struct lcrecord_header header;
	Lisp_Object fname;
	int mode;
	int access_;
	int dberrno;
	int live_p;
#ifdef HAVE_DBM
	DBM *dbm_handle;
#endif
#ifdef HAVE_BERKELEY_DB
	DB *db_handle;
#endif
	DB_FUNCS *funcs;
#ifdef MULE
	Lisp_Object coding_system;
#endif
};

#define XDATABASE(x) XRECORD (x, database, Lisp_Database)
#define XSETDATABASE(x, p) XSETRECORD (x, p, database)
#define DATABASEP(x) RECORDP (x, database)
#define CHECK_DATABASE(x) CHECK_RECORD (x, database)
#define CONCHECK_DATABASE(x) CONCHECK_RECORD (x, database)
#define DATABASE_LIVE_P(x) (x->live_p)

#define CHECK_LIVE_DATABASE(db) do {					\
  CHECK_DATABASE (db);							\
  if (!DATABASE_LIVE_P (XDATABASE(db)))					\
    signal_simple_error ("Attempting to access closed database", db);	\
} while (0)

static Lisp_Database *allocate_database(void)
{
	Lisp_Database *db =
	    alloc_lcrecord_type(Lisp_Database, &lrecord_database);

	db->fname = Qnil;
	db->live_p = 0;
#ifdef HAVE_BERKELEY_DB
	db->db_handle = NULL;
#endif
#ifdef HAVE_DBM
	db->dbm_handle = NULL;
#endif
	db->access_ = 0;
	db->mode = 0;
	db->dberrno = 0;
#ifdef MULE
	db->coding_system = Fget_coding_system(Qbinary);
#endif
	return db;
}

static Lisp_Object mark_database(Lisp_Object object)
{
	Lisp_Database *db = XDATABASE(object);
	return db->fname;
}

static void
print_database(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Database *db = XDATABASE(obj);
	Lisp_Object tmp1, tmp2;

	if (print_readably) {
		error("printing unreadable object #<database 0x%x>",
		      db->header.uid);
	}
	write_c_string("#<database \"", printcharfun);
	print_internal(db->fname, printcharfun, 0);

	tmp1 = db->funcs->get_type(db);
	tmp2 = db->funcs->get_subtype(db);
	write_fmt_string(printcharfun,
			 "\" (%s/%s/%s) 0x%x>",
			 (char *)string_data(XSYMBOL(tmp1)->name),
			 (char *)string_data(XSYMBOL(tmp2)->name),
			 (!DATABASE_LIVE_P(db) ? "closed" :
			  (db->access_ & O_WRONLY) ? "writeonly" :
			  (db->access_ & O_RDWR) ? "readwrite" : "readonly"),
			 db->header.uid);
}

static void finalize_database(void *header, int for_disksave)
{
	Lisp_Database *db = (Lisp_Database *) header;

	if (for_disksave) {
		Lisp_Object object;
		XSETDATABASE(object, db);

		signal_simple_error
		    ("Can't dump an emacs containing database objects", object);
	}
	db->funcs->close(db);
}

DEFINE_LRECORD_IMPLEMENTATION("database", database,
			      mark_database, print_database,
			      finalize_database, 0, 0, 0, Lisp_Database);

DEFUN("close-database", Fclose_database, 1, 1, 0,	/*
Close database DATABASE.
*/
      (database))
{
	Lisp_Database *db;
	CHECK_LIVE_DATABASE(database);
	db = XDATABASE(database);
	db->funcs->close(db);
	db->live_p = 0;
	return Qnil;
}

DEFUN("database-type", Fdatabase_type, 1, 1, 0,	/*
Return the type of database DATABASE.
*/
      (database))
{
	CHECK_DATABASE(database);

	return XDATABASE(database)->funcs->get_type(XDATABASE(database));
}

DEFUN("database-subtype", Fdatabase_subtype, 1, 1, 0,	/*
Return the subtype of database DATABASE, if any.
*/
      (database))
{
	CHECK_DATABASE(database);

	return XDATABASE(database)->funcs->get_subtype(XDATABASE(database));
}

DEFUN("database-live-p", Fdatabase_live_p, 1, 1, 0,	/*
Return t if OBJECT is an active database.
*/
      (object))
{
	return DATABASEP(object) && DATABASE_LIVE_P(XDATABASE(object)) ?
	    Qt : Qnil;
}

DEFUN("database-file-name", Fdatabase_file_name, 1, 1, 0,	/*
Return the filename associated with the database DATABASE.
*/
      (database))
{
	CHECK_DATABASE(database);

	return XDATABASE(database)->fname;
}

DEFUN("databasep", Fdatabasep, 1, 1, 0,	/*
Return t if OBJECT is a database.
*/
      (object))
{
	return DATABASEP(object) ? Qt : Qnil;
}

#ifdef HAVE_DBM
static void dbm_map(Lisp_Database * db, Lisp_Object func)
{
	datum keydatum, valdatum;
	Lisp_Object key, val;

	for (keydatum = dbm_firstkey(db->dbm_handle);
	     keydatum.dptr != NULL; keydatum = dbm_nextkey(db->dbm_handle)) {
		valdatum = dbm_fetch(db->dbm_handle, keydatum);
		key =
		    make_string((unsigned char *)keydatum.dptr, keydatum.dsize);
		val =
		    make_string((unsigned char *)valdatum.dptr, valdatum.dsize);
		call2(func, key, val);
	}
}

static Lisp_Object dbm_get(Lisp_Database * db, Lisp_Object key)
{
	datum keydatum, valdatum;

	keydatum.dptr = (char *)XSTRING_DATA(key);
	keydatum.dsize = XSTRING_LENGTH(key);
	valdatum = dbm_fetch(db->dbm_handle, keydatum);

	return (valdatum.dptr
		? make_string((unsigned char *)valdatum.dptr, valdatum.dsize)
		: Qnil);
}

static int
dbm_put(Lisp_Database * db,
	Lisp_Object key, Lisp_Object val, Lisp_Object replace)
{
	datum keydatum, valdatum;

	valdatum.dptr = (char *)XSTRING_DATA(val);
	valdatum.dsize = XSTRING_LENGTH(val);
	keydatum.dptr = (char *)XSTRING_DATA(key);
	keydatum.dsize = XSTRING_LENGTH(key);

	return !dbm_store(db->dbm_handle, keydatum, valdatum,
			  NILP(replace) ? DBM_INSERT : DBM_REPLACE);
}

static int dbm_remove(Lisp_Database * db, Lisp_Object key)
{
	datum keydatum;

	keydatum.dptr = (char *)XSTRING_DATA(key);
	keydatum.dsize = XSTRING_LENGTH(key);

	return dbm_delete(db->dbm_handle, keydatum);
}

static Lisp_Object dbm_type(Lisp_Database * db)
{
	return Qdbm;
}

static Lisp_Object dbm_subtype(Lisp_Database * db)
{
	return Qnil;
}

static Lisp_Object dbm_lasterr(Lisp_Database * db)
{
	return lisp_strerror(db->dberrno);
}

static void dbm_closeit(Lisp_Database * db)
{
	if (db->dbm_handle) {
		dbm_close(db->dbm_handle);
		db->dbm_handle = NULL;
	}
}

static DB_FUNCS ndbm_func_block = {
	dbm_subtype,
	dbm_type,
	dbm_get,
	dbm_put,
	dbm_remove,
	dbm_map,
	dbm_closeit,
	dbm_lasterr
};
#endif				/* HAVE_DBM */

#ifdef HAVE_BERKELEY_DB
static Lisp_Object berkdb_type(Lisp_Database * db)
{
	return Qberkeley_db;
}

static Lisp_Object berkdb_subtype(Lisp_Database * db)
{
	if (!db->db_handle)
		return Qnil;

	switch (db->db_handle->type) {
	case DB_BTREE:
		return Qbtree;
	case DB_HASH:
		return Qhash;
	case DB_RECNO:
		return Qrecno;
#if DB_VERSION_MAJOR > 2
	case DB_QUEUE:
		return Qqueue;
#endif
#ifdef HAVE_DB_UNKNOWN
	case DB_UNKNOWN:
#endif
	default:
		return Qunknown;
	}
}

static Lisp_Object berkdb_lasterr(Lisp_Database * db)
{
	return lisp_strerror(db->dberrno);
}

static Lisp_Object berkdb_get(Lisp_Database * db, Lisp_Object key)
{
	DBT keydatum, valdatum;
	int status = 0;

	/* DB Version 2 requires DBT's to be zeroed before use. */
	xzero(keydatum);
	xzero(valdatum);

	keydatum.data = XSTRING_DATA(key);
	keydatum.size = XSTRING_LENGTH(key);

#if DB_VERSION_MAJOR == 1
	status = db->db_handle->get(db->db_handle, &keydatum, &valdatum, 0);
#else
	status =
	    db->db_handle->get(db->db_handle, NULL, &keydatum, &valdatum, 0);
#endif				/* DB_VERSION_MAJOR */

	if (!status)
		/* #### Not mule-ized! will crash! */
		return make_string((Bufbyte *) valdatum.data, valdatum.size);

#if DB_VERSION_MAJOR == 1
	db->dberrno = (status == 1) ? -1 : errno;
#else
	db->dberrno = (status < 0) ? -1 : errno;
#endif				/* DB_VERSION_MAJOR */

	return Qnil;
}

static int
berkdb_put(Lisp_Database * db,
	   Lisp_Object key, Lisp_Object val, Lisp_Object replace)
{
	DBT keydatum, valdatum;
	int status = 0;

	/* DB Version 2 requires DBT's to be zeroed before use. */
	xzero(keydatum);
	xzero(valdatum);

	keydatum.data = XSTRING_DATA(key);
	keydatum.size = XSTRING_LENGTH(key);
	valdatum.data = XSTRING_DATA(val);
	valdatum.size = XSTRING_LENGTH(val);
#if DB_VERSION_MAJOR == 1
	status = db->db_handle->put(db->db_handle, &keydatum, &valdatum,
				    NILP(replace) ? R_NOOVERWRITE : 0);
	db->dberrno = (status == 1) ? -1 : errno;
#else
	status = db->db_handle->put(db->db_handle, NULL, &keydatum, &valdatum,
				    NILP(replace) ? DB_NOOVERWRITE : 0);
	db->dberrno = (status < 0) ? -1 : errno;
#endif				/* DV_VERSION_MAJOR = 2 */

	return status;
}

static int berkdb_remove(Lisp_Database * db, Lisp_Object key)
{
	DBT keydatum;
	int status;

	/* DB Version 2 requires DBT's to be zeroed before use. */
	xzero(keydatum);

	keydatum.data = XSTRING_DATA(key);
	keydatum.size = XSTRING_LENGTH(key);

#if DB_VERSION_MAJOR == 1
	status = db->db_handle->del(db->db_handle, &keydatum, 0);
#else
	status = db->db_handle->del(db->db_handle, NULL, &keydatum, 0);
#endif				/* DB_VERSION_MAJOR */

	if (!status)
		return 0;

#if DB_VERSION_MAJOR == 1
	db->dberrno = (status == 1) ? -1 : errno;
#else
	db->dberrno = (status < 0) ? -1 : errno;
#endif				/* DB_VERSION_MAJOR */

	return 1;
}

static void berkdb_map(Lisp_Database * db, Lisp_Object func)
{
	DBT keydatum, valdatum;
	Lisp_Object key, val;
	DB *dbp = db->db_handle;
	int status;

	xzero(keydatum);
	xzero(valdatum);

#if DB_VERSION_MAJOR == 1
	for (status = dbp->seq(dbp, &keydatum, &valdatum, R_FIRST);
	     status == 0;
	     status = dbp->seq(dbp, &keydatum, &valdatum, R_NEXT)) {
		/* #### Needs mule-izing */
		key = make_string((Bufbyte *) keydatum.data, keydatum.size);
		val = make_string((Bufbyte *) valdatum.data, valdatum.size);
		call2(func, key, val);
	}
#else
	{
		DBC *dbcp;

#if DB_VERSION_MAJOR > 2 || DB_VERSION_MINOR >=6
		status = dbp->cursor(dbp, NULL, &dbcp, 0);
#else
		status = dbp->cursor(dbp, NULL, &dbcp);
#endif
		for (status = dbcp->c_get(dbcp, &keydatum, &valdatum, DB_FIRST);
		     status == 0;
		     status = dbcp->c_get(dbcp, &keydatum, &valdatum, DB_NEXT))
		{
			/* #### Needs mule-izing */
			key =
			    make_string((Bufbyte *) keydatum.data,
					keydatum.size);
			val =
			    make_string((Bufbyte *) valdatum.data,
					valdatum.size);
			call2(func, key, val);
		}
		dbcp->c_close(dbcp);
	}
#endif				/* DB_VERSION_MAJOR */
}

static void berkdb_close(Lisp_Database * db)
{
	if (db->db_handle) {
#if DB_VERSION_MAJOR == 1
		db->db_handle->sync(db->db_handle, 0);
		db->db_handle->close(db->db_handle);
#else
		db->db_handle->sync(db->db_handle, 0);
		db->db_handle->close(db->db_handle, 0);
#endif				/* DB_VERSION_MAJOR */
		db->db_handle = NULL;
	}
}

static DB_FUNCS berk_func_block = {
	berkdb_subtype,
	berkdb_type,
	berkdb_get,
	berkdb_put,
	berkdb_remove,
	berkdb_map,
	berkdb_close,
	berkdb_lasterr
};
#endif				/* HAVE_BERKELEY_DB */

DEFUN("database-last-error", Fdatabase_last_error, 0, 1, 0,	/*
Return the last error associated with DATABASE.
*/
      (database))
{
	if (NILP(database))
		return lisp_strerror(errno);

	CHECK_DATABASE(database);

	return XDATABASE(database)->funcs->last_error(XDATABASE(database));
}

DEFUN("open-database", Fopen_database, 1, 5, 0,	/*
Return a new database object opened on FILE.
Optional arguments TYPE and SUBTYPE specify the database type.
Optional argument ACCESS specifies the access rights, which may be any
combination of 'r' 'w' and '+', for read, write, and creation flags.
Optional argument MODE gives the permissions to use when opening FILE,
and defaults to 0755.
*/
      (file, type, subtype, access_, mode))
{
	/* This function can GC */
	int modemask;
	int accessmask = 0;
	Lisp_Database *db = NULL;
	char *filename;
	struct gcpro gcpro1, gcpro2;

	CHECK_STRING(file);
	GCPRO2(file, access_);
	file = Fexpand_file_name(file, Qnil);
	UNGCPRO;

	TO_EXTERNAL_FORMAT(LISP_STRING, file,
			   C_STRING_ALLOCA, filename, Qfile_name);

	if (NILP(access_)) {
		accessmask = O_RDWR | O_CREAT;
	} else {
		char *acc;
		CHECK_STRING(access_);
		acc = (char *)XSTRING_DATA(access_);

		if (strchr(acc, '+'))
			accessmask |= O_CREAT;

		{
			char *rp = strchr(acc, 'r');
			char *wp = strchr(acc, 'w');
			if (rp && wp)
				accessmask |= O_RDWR;
			else if (wp)
				accessmask |= O_WRONLY;
			else
				accessmask |= O_RDONLY;
		}
	}

	if (NILP(mode)) {
		modemask = 0755;	/* rwxr-xr-x */
	} else {
		CHECK_INT(mode);
		modemask = XINT(mode);
	}

#ifdef HAVE_DBM
	if (NILP(type) || EQ(type, Qdbm)) {
		DBM *dbase = dbm_open(filename, accessmask, modemask);
		if (!dbase)
			return Qnil;

		db = allocate_database();
		db->dbm_handle = dbase;
		db->funcs = &ndbm_func_block;
		goto db_done;
	}
#endif				/* HAVE_DBM */

#ifdef HAVE_BERKELEY_DB
	if (NILP(type) || EQ(type, Qberkeley_db)) {
		DBTYPE real_subtype;
		DB *dbase;
#if DB_VERSION_MAJOR != 1
		int status;
#endif

		if (EQ(subtype, Qhash) || NILP(subtype))
			real_subtype = DB_HASH;
		else if (EQ(subtype, Qbtree))
			real_subtype = DB_BTREE;
		else if (EQ(subtype, Qrecno))
			real_subtype = DB_RECNO;
#if DB_VERSION_MAJOR > 2
		else if (EQ(subtype, Qqueue))
			real_subtype = DB_QUEUE;
#endif
		else
			signal_simple_error("Unsupported subtype", subtype);

#if DB_VERSION_MAJOR == 1
		dbase =
		    dbopen(filename, accessmask, modemask, real_subtype, NULL);
		if (!dbase)
			return Qnil;
#else
		/* Berkeley DB Version 2 has only the accessmask DB_CREATE and DB_RDONLY,
		   other flags shouldn't be set */
		if (NILP(access_))
			accessmask = DB_CREATE;
		else {
			char *acc;
			CHECK_STRING(access_);
			acc = (char *)XSTRING_DATA(access_);
			accessmask = 0;

			if (strchr(acc, '+'))
				accessmask |= DB_CREATE;

			if (strchr(acc, 'r') && !strchr(acc, 'w'))
				accessmask |= DB_RDONLY;
		}
#if DB_VERSION_MAJOR == 2
		status = db_open(filename, real_subtype, accessmask,
				 modemask, NULL, NULL, &dbase);
		if (status)
			return Qnil;
#else
		status = db_create(&dbase, NULL, 0);
		if (status)
			return Qnil;
#if DB_VERSION_MAJOR < 4 || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR < 1)
		status = dbase->open(dbase, filename, NULL,
				     real_subtype, accessmask, modemask);
#else				/* DB_VERSION >= 4.1 */
		/* DB_AUTO_COMMIT requires transaction support, don't try it */
		status = dbase->open(dbase, NULL, filename, NULL, real_subtype,
				     accessmask, modemask);
#endif				/* DB_VERSION < 4.1 */
		if (status) {
			dbase->close(dbase, 0);
			return Qnil;
		}
#endif				/* DB_VERSION_MAJOR > 2 */
		/* Normalize into system specific file modes. Only for printing */
		accessmask = accessmask & DB_RDONLY ? O_RDONLY : O_RDWR;
#endif				/* DB_VERSION_MAJOR */

		db = allocate_database();
		db->db_handle = dbase;
		db->funcs = &berk_func_block;
		goto db_done;
	}
#endif				/* HAVE_BERKELEY_DB */

	signal_simple_error("Unsupported database type", type);
	return Qnil;

      db_done:
	db->live_p = 1;
	db->fname = file;
	db->mode = modemask;
	db->access_ = accessmask;

	{
		Lisp_Object retval;
		XSETDATABASE(retval, db);
		return retval;
	}
}

DEFUN("put-database", Fput_database, 3, 4, 0,	/*
Store KEY and VALUE in DATABASE.
If optional fourth arg REPLACE is non-nil,
replace any existing entry in the database.
*/
      (key, value, database, replace))
{
	CHECK_LIVE_DATABASE(database);
	CHECK_STRING(key);
	CHECK_STRING(value);
	{
		Lisp_Database *db = XDATABASE(database);
		int status = db->funcs->put(db, key, value, replace);
		return status ? Qt : Qnil;
	}
}

DEFUN("remove-database", Fremove_database, 2, 2, 0,	/*
Remove KEY from DATABASE.
*/
      (key, database))
{
	CHECK_LIVE_DATABASE(database);
	CHECK_STRING(key);
	{
		Lisp_Database *db = XDATABASE(database);
		int status = db->funcs->rem(db, key);
		return status ? Qt : Qnil;
	}
}

DEFUN("get-database", Fget_database, 2, 3, 0,	/*
Return value for KEY in DATABASE.
If there is no corresponding value, return DEFAULT (defaults to nil).
*/
      (key, database, default_))
{
	CHECK_LIVE_DATABASE(database);
	CHECK_STRING(key);
	{
		Lisp_Database *db = XDATABASE(database);
		Lisp_Object retval = db->funcs->get(db, key);
		return NILP(retval) ? default_ : retval;
	}
}

DEFUN("map-database", Fmapdatabase, 2, 2, 0,	/*
Map FUNCTION over entries in DATABASE, calling it with two args,
each key and value in the database.
*/
      (function, database))
{
	CHECK_LIVE_DATABASE(database);

	XDATABASE(database)->funcs->map(XDATABASE(database), function);

	return Qnil;
}

void syms_of_database(void)
{
	INIT_LRECORD_IMPLEMENTATION(database);

	defsymbol(&Qdatabasep, "databasep");
#ifdef HAVE_DBM
	defsymbol(&Qdbm, "dbm");
#endif
#ifdef HAVE_BERKELEY_DB
	defsymbol(&Qberkeley_db, "berkeley-db");
	defsymbol(&Qhash, "hash");
	defsymbol(&Qbtree, "btree");
	defsymbol(&Qrecno, "recno");
#if DB_VERSION_MAJOR > 2
	defsymbol(&Qqueue, "queue");
#endif
	defsymbol(&Qunknown, "unknown");
#endif

	DEFSUBR(Fopen_database);
	DEFSUBR(Fdatabasep);
	DEFSUBR(Fmapdatabase);
	DEFSUBR(Fput_database);
	DEFSUBR(Fget_database);
	DEFSUBR(Fremove_database);
	DEFSUBR(Fdatabase_type);
	DEFSUBR(Fdatabase_subtype);
	DEFSUBR(Fdatabase_last_error);
	DEFSUBR(Fdatabase_live_p);
	DEFSUBR(Fdatabase_file_name);
	DEFSUBR(Fclose_database);
}

void vars_of_database(void)
{
#ifdef HAVE_DBM
	Fprovide(Qdbm);
#endif
#ifdef HAVE_BERKELEY_DB
	Fprovide(Qberkeley_db);
#endif

#if 0				/* #### implement me! */
#ifdef MULE
	DEFVAR_LISP("database-coding-system", &Vdatabase_coding_system	/*
Coding system used to convert data in database files.
									 */ );
	Vdatabase_coding_system = Qnil;
#endif
#endif				/* 0 */
}
