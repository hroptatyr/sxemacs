/* Definitions for the LDAP client interface for XEmacs.
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


#ifndef INCLUDED_eldap_h_
#define INCLUDED_eldap_h_

#include <lber.h>
#include <ldap.h>

/*
 * The following structure records pertinent information about a
 * LDAP connection.
 */

struct Lisp_LDAP {
	/* lcrecord header */
	struct lcrecord_header header;
	/* The LDAP connection handle used by the LDAP API */
	LDAP *ld;
	/* Name of the host we connected to */
	Lisp_Object host;
};
typedef struct Lisp_LDAP Lisp_LDAP;

DECLARE_LRECORD(ldap, Lisp_LDAP);
#define XLDAP(x) XRECORD (x, ldap, Lisp_LDAP)
#define XSETLDAP(x, p) XSETRECORD (x, p, ldap)
#define LDAPP(x) RECORDP (x, ldap)
#define CHECK_LDAP(x) CHECK_RECORD (x, ldap)
#define CONCHECK_LDAP(x) CONCHECK_RECORD (x, ldap)

#define CHECK_LIVE_LDAP(ldap) do {					\
  CHECK_LDAP (ldap);							\
  if (!XLDAP (ldap)->ld)						\
    signal_simple_error ("Attempting to access closed LDAP connection",	\
			 ldap);						\
} while (0)

Lisp_Object Fldapp(Lisp_Object object);
Lisp_Object Fldap_host(Lisp_Object ldap);
Lisp_Object Fldap_status(Lisp_Object ldap);
Lisp_Object Fldap_open(Lisp_Object host, Lisp_Object ldap_plist);
Lisp_Object Fldap_close(Lisp_Object ldap);
Lisp_Object Fldap_search_basic(Lisp_Object ldap,
			       Lisp_Object filter,
			       Lisp_Object base,
			       Lisp_Object scope,
			       Lisp_Object attrs,
			       Lisp_Object attrsonly,
			       Lisp_Object withdn, Lisp_Object verbose);
Lisp_Object Fldap_add(Lisp_Object ldap, Lisp_Object dn, Lisp_Object entry);
Lisp_Object Fldap_modify(Lisp_Object ldap, Lisp_Object dn, Lisp_Object entry);
Lisp_Object Fldap_delete(Lisp_Object ldap, Lisp_Object dn);

#endif				/* INCLUDED_eldap_h_ */
