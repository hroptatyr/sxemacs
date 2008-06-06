dnl sxe-dbms.m4 -- Database stuff


AC_DEFUN([_SXE_CHECK_POSTGRESQL], [dnl
	## (re)defines have_postgresql
	## defines HAVE_POSTGRESQL

	## compute pgsql specific compiler/linker flags
	PGSQL_CPPFLAGS="-I$pgsql_incdir"
	PGSQL_LDFLAGS="-L$pgsql_libdir"
	PGSQL_LIBS="-lpq $pgsql_libs"

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
		pgsql_incdir=$($PG_CONFIG --includedir)
		pgsql_libdir=$($PG_CONFIG --libdir)

		if test -n "$PG_CONFIG" -a -f "$PG_CONFIG"; then
			pgsql_libs="$($PG_CONFIG --ldflags) $($PG_CONFIG --libs)"
		elif test -f "$pgsql_libdir/libpq.so"; then
			pgsql_libs=$($LDD $pgsql_libdir/libpq.so | \
				grep "=> /" | grep -v "=> /lib" | \
				sed -e "s,.*/lib\(.*\)\.so.*,-l\1," | \
				tr "\n" " ")
		else
			## doesnt matter otherwise
			pgsql_libs=""
		fi
		_SXE_CHECK_POSTGRESQL
	else
		AC_MSG_WARN([Unable to find pg_config, if you want PostgreSQL support])
		AC_MSG_WARN([in SXEmacs, check your PATH environment variable])
		AC_MSG_WARN([Performing the actual check anyway, cross your fingers!])
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

dnl sxe-dbms.m4 ends here
