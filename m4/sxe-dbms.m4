dnl sxe-dbms.m4 -- Database stuff


AC_DEFUN([_SXE_CHECK_POSTGRESQL], [dnl
	## (re)defines have_postgresql
	## defines HAVE_POSTGRESQL

	## compute pgsql specific compiler/linker flags
	PGSQL_CPPFLAGS="-I$pgsql_incdir"
	PGSQL_LDFLAGS="-L$pgsql_libdir ${pgsql_ldflags}"
	## do not add pgsql_libs here, as they concern the server only
	PGSQL_LIBS="-lpq"

	## backup values for excursion
	SXE_DUMP_LIBS
	save_DB_LIBS="$DB_LIBS"
	save_DB_LDFLAGS="$DB_LDFLAGS"
	save_DB_CPPFLAGS="$DB_CPPFLAGS"

	## prepend temporarily
	DB_CPPFLAGS="$PGSQL_CPPFLAGS $DB_CPPFLAGS"
	DB_LDFLAGS="$PGSQL_LDFLAGS $DB_LDFLAGS"
	DB_LIBS="$PGSQL_LIBS $DB_LIBS"

	## add DB_* to corresponding compiler vars temporarily
	CPPFLAGS="$CPPFLAGS $DB_CPPFLAGS"
	LDFLAGS="$LDFLAGS $DB_LDFLAGS $DB_LIBS"

	## check for necessary header
	AC_CHECK_HEADERS([libpq-fe.h])

	## check for necessary functions
	AC_CHECK_LIB([pq], [PQconnectdb])
	AC_CHECK_LIB([pq], [PQconnectStart])
	AC_CHECK_FUNC(PQserverVersion,
		[have_PQserverVersion=yes
		 AC_DEFINE([HAVE_PQSERVERVERSION], [1],
			 [Defined when PQserverVersion() is available])],
		[have_PQserverVersion=no])


	if test "$ac_cv_header_libpq_fe_h" = "yes"; then
		libpq_fe_h_file="$pgsql_incdir/libpq-fe.h"
		AC_DEFINE_UNQUOTED([LIBPQ_FE_H_FILE], ["libpq-fe.h"],
			[name of the main libpq interface header file])
	fi
	if test "$ac_cv_lib_pq_PQconnectStart" = "yes"; then
		have_postgresqlv7="yes"
		AC_DEFINE([HAVE_POSTGRESQLV7], [1], [Description here!])
	fi

	## final evaluation
	if test "$ac_cv_header_libpq_fe_h" = "yes" -a \
		"$ac_cv_lib_pq_PQconnectdb" = "yes"; then
		have_postgresql="yes"
		AC_DEFINE([HAVE_POSTGRESQL], [1], [Description here!])
		## keep the DB_* vars
	else
		have_postgresql="no"
		## restore all values of the DB_* vars
		DB_LIBS="$save_DB_LIBS"
		DB_LDFLAGS="$save_DB_LDFLAGS"
		DB_CPPFLAGS="$save_DB_CPPFLAGS"
	fi
	## restore our global compiler state
	SXE_RESTORE_LIBS

])dnl _SXE_CHECK_POSTGRESQL

AC_DEFUN([SXE_CHECK_POSTGRESQL], [dnl
	AC_MSG_CHECKING([for PostgreSQL])
	AC_MSG_RESULT([])
	AC_CHECK_PROG([have_pg_config], [pg_config], [yes], [no])

	if test "$have_pg_config" = "yes"; then
		AC_PATH_PROG([PG_CONFIG], [pg_config], [:])
		pgsql_incdir=$(${PG_CONFIG} --includedir)
		pgsql_libdir=$(${PG_CONFIG} --libdir)
		pgsql_ldflags=$(${PG_CONFIG} --ldflags)

		if test -n "${PG_CONFIG}" -a -f "${PG_CONFIG}"; then
			pgsql_libs="$(${PG_CONFIG} --libs)"
		elif test -f "${pgsql_libdir}/libpq.so"; then
			pgsql_libs=$(${LDD} ${pgsql_libdir}/libpq.so | \
				grep "=> /" | grep -v "=> /lib" | \
				sed -e "s,.*/lib\(.*\)\.so.*,-l\1," | \
				tr "\n" " ")
		else
			## doesnt matter otherwise
			pgsql_libs=""
		fi
		if test "$have_openssl" = "no" -a -n "$(echo ${pgsql_libs} | grep ssl)"; then
			if test "$with_openssl" = "no"; then
				AC_MSG_WARN([Your PostgreSQL seems to require OpenSSL.])
				AC_MSG_WARN([Sadly OpenSSL is not available or is misconfigured,])
				AC_MSG_WARN([and '--with-openssl=no' was passed. Disabling PostgreSQL])
				have_postgresql="no"
			else
				AC_MSG_WARN([Your PostgreSQL seems to require OpenSSL.])
				AC_MSG_WARN([Sadly OpenSSL is not available or is misconfigured.])
				AC_MSG_WARN([Performing the actual check anyway, cross your fingers!])
				AC_MSG_WARN([If you still encounter problems disable using '--with-postgresql=no'.])
				AC_MSG_WARN([NOTE: '--with-openssl=no' will also implictly disable PostgreSQL in this configuration.])
			fi
		fi
	else
		AC_MSG_WARN([Unable to find pg_config, if you want PostgreSQL support])
		AC_MSG_WARN([in SXEmacs, check your PATH environment variable])
		AC_MSG_WARN([Performing the actual check anyway, cross your fingers!])
	fi
	if test "$have_postgresql" != "no"; then
		_SXE_CHECK_POSTGRESQL
	fi
	if test "$have_postgresql" = "yes"; then
		SXE_ADD_DB_OBJS([postgresql.o])
		AC_SUBST([DB_CPPFLAGS])
		AC_SUBST([DB_LDFLAGS])
		AC_SUBST([DB_LIBS])
	else
		AC_MSG_WARN([Your PostgreSQL installation seems broken or is too old.])
	fi
])dnl SXE_CHECK_POSTGRESQL


AC_DEFUN([SXE_CHECK_LDAP], [dnl
	AC_MSG_CHECKING([for LDAP])
	AC_MSG_RESULT([])

	ldap_libs=
	AC_CHECK_HEADER(ldap.h, [], [have_ldap=no])
	AC_CHECK_HEADER(lber.h, [], [have_ldap=no])
	AC_CHECK_LIB(ldap, [ldap_search], [have_ldap=yes])
	dnl Check for other libraries we need to link with to get the main routines.
	if test "$have_ldap" != "yes"; then
		AC_CHECK_LIB(ldap, [ldap_open], [
			have_ldap=yes
			have_ldap_lber=yes], [], [-llber])
	fi
	if test "$habe_ldap" != "yes"; then
		AC_CHECK_LIB(ldap, [ldap_open],	[
			have_ldap=yes
			have_ldap_lber=yes
			have_ldap_krb=yes], [], [-llber -lkrb])
	fi
	if test "$have_ldap" != "yes"; then
		AC_CHECK_LIB(ldap, [ldap_open],	[
			have_ldap=yes
			have_ldap_lber=yes
			have_ldap_krb=yes
			have_ldap_des=yes], [], [-llber -lkrb -ldes])
	fi
	dnl Recently, we need -lber even though the main routines are elsewhere,
	dnl because otherwise be get link errors w.r.t. ber_pvt_opt_on.  So just
	dnl check for that (it's a variable not a fun but that doesn't seem to
	dnl matter in these checks)  and stick in -lber if so.  Can't hurt (even to
	dnl stick it in always shouldn't hurt, I don't think) ... #### Someone who
	dnl #### understands LDAP needs to fix this properly.
	if test "$have_ldap_lber" != "yes"; then
		AC_CHECK_LIB(lber, [ber_pvt_opt_on], [have_ldap_lber=yes])
	fi
])dnl SXE_CHECK_LDAP


AC_DEFUN([SXE_CHECK_NDBM], [dnl
	SXE_CHECK_HEADERS([ndbm.h])
	if test "$ac_cv_header_ndbm_h" = "yes"; then
		sxe_cv_feat_ndbm="yes"
	else
		sxe_cv_feat_ndbm="no"
	fi
])dnl SXE_CHECK_NDBM

AC_DEFUN([SXE_CHECK_GDBM], [dnl
	AC_REQUIRE([SXE_CHECK_NDBM])

	if test "$sxe_cv_feat_ndbm" = "yes"; then
		AC_CHECK_LIB([gdbm], [dbm_open])
		AC_CHECK_LIB([gdbm_compat], [dbm_open], [:], [:], [-lgdbm])

		if test "$ac_cv_lib_gdbm_dbm_open" = "yes"; then
			sxe_cv_feat_gdbm="yes"
			libdbm="-lgdbm"
		elif test "$ac_cv_lib_gdbm_compat_dbm_open" = "yes"; then
			sxe_cv_feat_gdbm="yes"
			libdbm="-lgdbm_compat -lgdbm"
		else
			sxe_cv_feat_gdbm="no"
		fi
	fi
])dnl SXE_CHECK_GDBM

AC_DEFUN([SXE_CHECK_DBM], [dnl
	AC_REQUIRE([SXE_CHECK_NDBM])

	if test "$sxe_cv_feat_ndbm" = "yes"; then
		AC_CHECK_LIB([dbm], [dbm_open])

		if test "$ac_cv_lib_dbm_dbm_open" = "yes"; then
			sxe_cv_feat_dbm="yes"
			libdbm="-ldbm"
		else
			sxe_cv_feat_dbm="no"
		fi
	fi
])dnl SXE_CHECK_DBM

AC_DEFUN([SXE_CHECK_BERKDB], [dnl
	SXE_MSG_CHECKING([for BerkeleyDB header file])

	for header in "db/db.h" "db.h"; do
		AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#if !(defined __GLIBC__ && __GLIBC_MINOR__ >= 1)
#ifdef HAVE_INTTYPES_H
#define __BIT_TYPES_DEFINED__
#include <inttypes.h>
typedef uint8_t  u_int8_t;
typedef uint16_t u_int16_t;
typedef uint32_t u_int32_t;
#ifdef WE_DONT_NEED_QUADS
typedef uint64_t u_int64_t;
#endif
#endif
#endif
#include <$header>
		]])], [db_h_file="$header"; break])
	done

	if test -z "$db_h_file"; then
		SXE_MSG_RESULT([no])
		sxe_cv_feat_berkdb="no"
	else
		SXE_MSG_RESULT([$db_h_file])
		sxe_cv_feat_berkdb="yes"
	fi

	SXE_MSG_CHECKING([for Berkeley DB version])
	AC_EGREP_CPP([yes], [
#include <$db_h_file>
#if DB_VERSION_MAJOR > 1
yes
#endif
			], [AC_EGREP_CPP([yes], [
#include <$db_h_file>
#if DB_VERSION_MAJOR > 2
yes
#endif
				], [AC_EGREP_CPP([yes], [
#include <$db_h_file>
#if DB_VERSION_MAJOR > 3
yes
#endif
					], [
						SXE_MSG_RESULT([4])
						dbfunc="db_create"
						dbver="4"], [
					SXE_MSG_RESULT([3])
					dbfunc="db_create"
					dbver="3"])], [
				SXE_MSG_RESULT([2])
				dbfunc="db_open"
				dbver="2"])], [
			SXE_MSG_RESULT([1])
			dbfunc="dbopen"
			dbver="1"])
	AC_CHECK_FUNC([$dbfunc], [
		sxe_cv_feat_berkdb="yes"
		need_libdb="no"], [
			AC_CHECK_LIB([db], [$dbfunc], [
				sxe_cv_feat_berkdb="yes"
				need_libdb="yes"])])

	dnl Berk db 4.1 decorates public functions with version information
	if test "$sxe_cv_feat_berkdb" != "yes" -a "$dbver" = "4"; then
		rm -f $tempcname
		echo "#include <$db_h_file>" > $tempcname
		echo "configure___ dbfunc=db_create" >> $tempcname
		define(TAB, [	])dnl
		eval $($CPP -Isrc $tempcname \
			| sed -n -e "s/[[ TAB]]*=[[ TAB\"]]*/='/" \
				-e "s/[[ TAB\"]]*\$/'/" -e "s/^configure___//p")
		rm -f $tempcname
		AC_MSG_WARN([db_create is really $dbfunc])
		AC_CHECK_LIB([db], [$dbfunc], [
			sxe_cv_feat_berkdb="yes"
			need_libdb="yes"])
	fi

	if test "$sxe_cv_feat_berkdb" = "yes"; then
		if test -n "$db_h_file"; then
			AC_DEFINE_UNQUOTED([DB_H_FILE],
				["$db_h_file"], [Description here!])
		fi
		AC_DEFINE([HAVE_BERKELEY_DB], [1], [Description here!])
		if test "$need_libdb" = "yes"; then
			SXE_PREPEND([-ldb], [DB_LIBS])
		fi
	else
		sxe_cv_feat_berkdb="no"
	fi
	if test "$sxe_cv_feat_berkdb" = "yes"; then
		AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include DB_H_FILE
main() {
	int type = DB_UNKNOWN;
	return 0;
}
		]])], [have_db_unknown="yes"])
		if test "$have_db_unknown" = "yes"; then
			AC_DEFINE_UNQUOTED([HAVE_DB_UNKNOWN], [1],
				[Berkley DB has DB_UNKNOWN type])
		fi
	fi
])dnl SXE_CHECK_BERKDB

AC_DEFUN([SXE_CHECK_NDBM_BERKDB_CONFLICT], [dnl
	SXE_MSG_CHECKING([whether ndbm and berkdb can coexist])

	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include DB_H_FILE
#include <ndbm.h>

int
main(void)
{
	return 0;
}
		]])],
		[sxe_cv_db_ndbm_berkdb_conflict="no"],
		[sxe_cv_db_ndbm_berkdb_conflict="yes"])

	if test "$sxe_cv_db_ndbm_berkdb_conflict" = "no"; then
		SXE_MSG_RESULT([yes])
	else
		SXE_MSG_RESULT([no])
		AC_MSG_WARN([disabling (g)dbm support again])
	fi

])dnl SXE_CHECK_NDBM_BERKDB_CONFLICT

dnl sxe-dbms.m4 ends here
