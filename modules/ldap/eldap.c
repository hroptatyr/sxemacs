/* LDAP client interface for SXEmacs.
   Copyright (C) 1998 Free Software Foundation, Inc.

This file is part of SXEmacs.

SXEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

SXEmacs is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Not in FSF. */

/* Author: Oscar Figueiredo */

/* This file provides lisp primitives for access to an LDAP library
   conforming to the API defined in RFC 1823.
   It has been tested with:
   - UMich LDAP 3.3 (http://www.umich.edu/~dirsvcs/ldap/)
   - Netscape's LDAP SDK 1.0 (http://developer.netscape.com) */

#include <emodules.h>

#if defined (HAVE_LDAP)
/* The entire file is within this conditional */

#include "eldap.h"
#include <lber.h>
#include <ldap.h>

#ifdef HAVE_NS_LDAP
#define HAVE_LDAP_SET_OPTION 1
#define HAVE_LDAP_GET_ERRNO 1
#else
#undef HAVE_LDAP_SET_OPTION
#undef HAVE_LDAP_GET_ERRNO
#endif

static Lisp_Object Vldap_default_base;
static Lisp_Object Vldap_default_host;

/* ldap-search-internal plist keywords */
static Lisp_Object Qhost, Qfilter, Qattributes, Qattrsonly, Qbase, Qscope,
    Qauth, Qbinddn, Qpasswd, Qderef, Qtimelimit, Qsizelimit;
/* Search scope limits */
static Lisp_Object Qbase, Qonelevel, Qsubtree;
/* Authentication methods */
#ifdef LDAP_AUTH_KRBV41
static Lisp_Object Qkrbv41;
#endif
#ifdef LDAP_AUTH_KRBV42
static Lisp_Object Qkrbv42;
#endif
/* Deref policy */
static Lisp_Object Qnever, Qalways, Qfind;

DEFUN("ldap-search-internal", Fldap_search_internal, 1, 1, 0,	/*
Perform a search on a LDAP server.

SEARCH-PLIST is a property list describing the search request.
Valid keys in that list are:

  `host' is a string naming one or more (blank separated) LDAP servers
  to to try to connect to. Each host name may optionally be of the
  form host:port.

  `filter' is a filter string for the search as described in RFC 1558

  `attributes' is a list of strings indicating which attributes to
  retrieve for each matching entry. If nil return all available
  attributes.

  `attrsonly' if non-nil indicates that only the attributes are
  retrieved, not the associated values.

  `base' is the base for the search as described in RFC 1779.

  `scope' is one of the three symbols `subtree', `base' or `onelevel'.

  `auth' is the authentication method to use, possible values depend
  on the LDAP library XEmacs was compiled with: `simple', `krbv41' and
  `krbv42'.

  `binddn' is the distinguished name of the user to bind as (in RFC
  1779 syntax).

  `passwd' is the password to use for simple authentication.

  `deref' is one of the symbols `never', `always', `search' or `find'.

  `timelimit' is the timeout limit for the connection in seconds.

  `sizelimit' is the maximum number of matches to return.

The function returns a list of matching entries.  Each entry is itself
an alist of attribute/values.
*/
      (search_plist))
{
	/* This function calls lisp */

	/* Vars for query */
	LDAP *ld;
	LDAPMessage *res, *e;
	BerElement *ptr;
	char *a;
	int i, rc, err;

	char *ldap_host = NULL;
	char *ldap_filter = NULL;
	char **ldap_attributes = NULL;
	int ldap_attrsonly = 0;
	char *ldap_base = NULL;
	int ldap_scope = LDAP_SCOPE_SUBTREE;
	int ldap_auth = LDAP_AUTH_SIMPLE;
	char *ldap_binddn = NULL;
	char *ldap_passwd = NULL;
	int ldap_deref = LDAP_DEREF_NEVER;
	int ldap_timelimit = 0;
	int ldap_sizelimit = 0;

	char **vals = NULL;
	int matches;

	Lisp_Object list, entry, result, keyword, value;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

	list = entry = result = keyword = value = Qnil;
	GCPRO5(list, entry, result, keyword, value);

	EXTERNAL_PROPERTY_LIST_LOOP(list, keyword, value, search_plist) {
		/* Host */
		if (EQ(keyword, Qhost)) {
			CHECK_STRING(value);
			ldap_host = alloca(XSTRING_LENGTH(value) + 1);
			strcpy(ldap_host, (char *)XSTRING_DATA(value));
		}
		/* Filter */
		else if (EQ(keyword, Qfilter)) {
			CHECK_STRING(value);
			ldap_filter = alloca(XSTRING_LENGTH(value) + 1);
			strcpy(ldap_filter, (char *)XSTRING_DATA(value));
		}
		/* Attributes */
		else if (EQ(keyword, Qattributes)) {
			if (!NILP(value)) {
				Lisp_Object attr_left = value;
				struct gcpro ngcpro1;

				NGCPRO1(attr_left);
				CHECK_CONS(value);

				ldap_attributes =
				    alloca((XINT(Flength(value)) +
					    1) * sizeof(char *));

				for (i = 0; !NILP(attr_left); i++) {
					CHECK_STRING(XCAR(attr_left));
					ldap_attributes[i] =
					    alloca(XSTRING_LENGTH
						   (XCAR(attr_left)) + 1);
					strcpy(ldap_attributes[i],
					       (char
						*)(XSTRING_DATA(XCAR
								(attr_left))));
					attr_left = XCDR(attr_left);
				}
				ldap_attributes[i] = NULL;
				NUNGCPRO;
			}
		}
		/* Attributes Only */
		else if (EQ(keyword, Qattrsonly)) {
			CHECK_SYMBOL(value);
			ldap_attrsonly = NILP(value) ? 0 : 1;
		}
		/* Base */
		else if (EQ(keyword, Qbase)) {
			if (!NILP(value)) {
				CHECK_STRING(value);
				ldap_base = alloca(XSTRING_LENGTH(value) + 1);
				strcpy(ldap_base, (char *)XSTRING_DATA(value));
			}
		}
		/* Scope */
		else if (EQ(keyword, Qscope)) {
			CHECK_SYMBOL(value);

			if (EQ(value, Qbase))
				ldap_scope = LDAP_SCOPE_BASE;
			else if (EQ(value, Qonelevel))
				ldap_scope = LDAP_SCOPE_ONELEVEL;
			else if (EQ(value, Qsubtree))
				ldap_scope = LDAP_SCOPE_SUBTREE;
			else
				signal_simple_error("Invalid scope", value);
		}
		/* Authentication method */
		else if (EQ(keyword, Qauth)) {
			CHECK_SYMBOL(value);

			if (EQ(value, Qsimple))
				ldap_auth = LDAP_AUTH_SIMPLE;
#ifdef LDAP_AUTH_KRBV41
			else if (EQ(value, Qkrbv41))
				ldap_auth = LDAP_AUTH_KRBV41;
#endif
#ifdef LDAP_AUTH_KRBV42
			else if (EQ(value, Qkrbv42))
				ldap_auth = LDAP_AUTH_KRBV42;
#endif
			else
				signal_simple_error
				    ("Invalid authentication method", value);
		}
		/* Bind DN */
		else if (EQ(keyword, Qbinddn)) {
			if (!NILP(value)) {
				CHECK_STRING(value);
				ldap_binddn = alloca(XSTRING_LENGTH(value) + 1);
				strcpy(ldap_binddn,
				       (char *)XSTRING_DATA(value));
			}
		}
		/* Password */
		else if (EQ(keyword, Qpasswd)) {
			if (!NILP(value)) {
				CHECK_STRING(value);
				ldap_passwd = alloca(XSTRING_LENGTH(value) + 1);
				strcpy(ldap_passwd,
				       (char *)XSTRING_DATA(value));
			}
		}
		/* Deref */
		else if (EQ(keyword, Qderef)) {
			CHECK_SYMBOL(value);
			if (EQ(value, Qnever))
				ldap_deref = LDAP_DEREF_NEVER;
			else if (EQ(value, Qsearch))
				ldap_deref = LDAP_DEREF_SEARCHING;
			else if (EQ(value, Qfind))
				ldap_deref = LDAP_DEREF_FINDING;
			else if (EQ(value, Qalways))
				ldap_deref = LDAP_DEREF_ALWAYS;
			else
				signal_simple_error("Invalid deref value",
						    value);
		}
		/* Timelimit */
		else if (EQ(keyword, Qtimelimit)) {
			if (!NILP(value)) {
				CHECK_INT(value);
				ldap_timelimit = XINT(value);
			}
		}
		/* Sizelimit */
		else if (EQ(keyword, Qsizelimit)) {
			if (!NILP(value)) {
				CHECK_INT(value);
				ldap_sizelimit = XINT(value);
			}
		}
	}

	/* Use ldap-default-base if no default base was given */
	if (ldap_base == NULL && !NILP(Vldap_default_base)) {
		CHECK_STRING(Vldap_default_base);
		ldap_base = alloca(XSTRING_LENGTH(Vldap_default_base) + 1);
		strcpy(ldap_base, (char *)XSTRING_DATA(Vldap_default_base));
	}

	/* Use ldap-default-host if no host was given */
	if (ldap_host == NULL && !NILP(Vldap_default_host)) {
		CHECK_STRING(Vldap_default_host);
		ldap_host = alloca(XSTRING_LENGTH(Vldap_default_host) + 1);
		strcpy(ldap_host, (char *)XSTRING_DATA(Vldap_default_host));
	}

	if (ldap_filter == NULL)
		error("Empty search filter");

	/* Garbage collect before connecting (if using UMich lib).
	   This is ugly, I know, but without this, the UMich LDAP library 3.3
	   frequently reports "Can't contact LDAP server".  I really need to
	   check what happens inside that lib. Anyway this should be harmless to
	   XEmacs and makes things work. */
#if defined (HAVE_UMICH_LDAP)
	garbage_collect_1();
#endif

	/* Connect to the server and bind */
	message("Connecting to %s...", ldap_host);
	if ((ld = ldap_open(ldap_host, LDAP_PORT)) == NULL)
		signal_simple_error("Failed connecting to host",
				    build_string(ldap_host));

#if HAVE_LDAP_SET_OPTION
	if (ldap_set_option(ld, LDAP_OPT_DEREF, (void *)&ldap_deref) !=
	    LDAP_SUCCESS)
		error("Failed to set deref option");
	if (ldap_set_option(ld, LDAP_OPT_TIMELIMIT, (void *)&ldap_timelimit) !=
	    LDAP_SUCCESS)
		error("Failed to set timelimit option");
	if (ldap_set_option(ld, LDAP_OPT_SIZELIMIT, (void *)&ldap_sizelimit) !=
	    LDAP_SUCCESS)
		error("Failed to set sizelimit option");
	if (ldap_set_option(ld, LDAP_OPT_REFERRALS, LDAP_OPT_ON) !=
	    LDAP_SUCCESS)
		error("Failed to set referral option");
#else				/* HAVE_LDAP_SET_OPTION */
	ld->ld_deref = ldap_deref;
	ld->ld_timelimit = ldap_timelimit;
	ld->ld_sizelimit = ldap_sizelimit;
#ifdef LDAP_REFERRALS
	ld->ld_options = LDAP_OPT_REFERRALS;
#else				/* LDAP_REFERRALS */
	ld->ld_options = 0;
#endif				/* LDAP_REFERRALS */
#endif				/* HAVE_LDAP_SET_OPTION */

	message("Binding to %s...", ldap_host);
	if ((err =
	     (ldap_bind_s(ld, ldap_binddn, ldap_passwd, ldap_auth))) !=
	    LDAP_SUCCESS)
		signal_simple_error("Failed binding to the server",
				    build_string(ldap_err2string(err)));

	/* Perform the search */
	message("Searching with LDAP on %s...", ldap_host);
	if (ldap_search(ld, ldap_base, ldap_scope, ldap_filter,
			ldap_attributes, ldap_attrsonly) == -1) {
		ldap_unbind(ld);
#if HAVE_LDAP_GET_ERRNO
		signal_simple_error("Error during LDAP search",
				    build_string(ldap_err2string
						 (ldap_get_lderrno
						  (ld, NULL, NULL))));
#else
		signal_simple_error("Error during LDAP search",
				    build_string(ldap_err2string
						 (ld->ld_errno)));
#endif
	}

	/* Build the results list */
	matches = 0;

	while ((rc = ldap_result(ld, LDAP_RES_ANY, 0, NULL, &res))
	       == LDAP_RES_SEARCH_ENTRY) {
		matches++;
		e = ldap_first_entry(ld, res);
		message("Parsing results... %d", matches);
		entry = Qnil;
		for (a = ldap_first_attribute(ld, e, &ptr);
		     a != NULL; a = ldap_next_attribute(ld, e, ptr)) {
			list = Fcons(build_string(a), Qnil);
			vals = ldap_get_values(ld, e, a);
			if (vals != NULL) {
				for (i = 0; vals[i] != NULL; i++) {
					list = Fcons(build_string(vals[i]),
						     list);
				}
			}
			entry = Fcons(Fnreverse(list), entry);
			ldap_value_free(vals);
		}
		result = Fcons(Fnreverse(entry), result);
		ldap_msgfree(res);
	}

	if (rc == -1) {
#if HAVE_LDAP_GET_ERRNO
		signal_simple_error("Error retrieving result",
				    build_string(ldap_err2string
						 (ldap_get_lderrno
						  (ld, NULL, NULL))));
#else
		signal_simple_error("Error retrieving result",
				    build_string(ldap_err2string
						 (ld->ld_errno)));
#endif
	}

	if ((rc = ldap_result2error(ld, res, 0)) != LDAP_SUCCESS) {
#if HAVE_LDAP_GET_ERRNO
		signal_simple_error("Error on result",
				    build_string(ldap_err2string
						 (ldap_get_lderrno
						  (ld, NULL, NULL))));
#else
		signal_simple_error("Error on result",
				    build_string(ldap_err2string
						 (ld->ld_errno)));
#endif
	}

	ldap_msgfree(res);
	ldap_unbind(ld);
	message("Done.");

	result = Fnreverse(result);
	clear_message();

	UNGCPRO;
	return result;
}

void syms_of_ldap(void)
{
	DEFSUBR(Fldap_search_internal);

	defsymbol(&Qhost, "host");
	defsymbol(&Qfilter, "filter");
	defsymbol(&Qattributes, "attributes");
	defsymbol(&Qattrsonly, "attrsonly");
	defsymbol(&Qbase, "base");
	defsymbol(&Qscope, "scope");
	defsymbol(&Qauth, "auth");
	defsymbol(&Qbinddn, "binddn");
	defsymbol(&Qpasswd, "passwd");
	defsymbol(&Qderef, "deref");
	defsymbol(&Qtimelimit, "timelimit");
	defsymbol(&Qsizelimit, "sizelimit");
	defsymbol(&Qbase, "base");
	defsymbol(&Qonelevel, "onelevel");
	defsymbol(&Qsubtree, "subtree");
#ifdef LDAP_AUTH_KRBV41
	defsymbol(&Qkrbv41, "krbv41");
#endif
#ifdef LDAP_AUTH_KRBV42
	defsymbol(&Qkrbv42, "krbv42");
#endif
	defsymbol(&Qnever, "never");
	defsymbol(&Qalways, "always");
	defsymbol(&Qfind, "find");
}

void vars_of_ldap(void)
{
	Fprovide(intern("ldap-internal"));

	DEFVAR_LISP("ldap-default-host", &Vldap_default_host	/*
								   Default LDAP host.
								 */ );

	DEFVAR_LISP("ldap-default-base", &Vldap_default_base	/*
								   Default base for LDAP searches.
								   This is a string using the syntax of RFC 1779.
								   For instance, "o=ACME, c=US" limits the search to the
								   Acme organization in the United States.
								 */ );

	Vldap_default_host = Qnil;
	Vldap_default_base = Qnil;
}

#endif				/* HAVE_LDAP */
