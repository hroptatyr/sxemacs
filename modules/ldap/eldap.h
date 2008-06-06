/* Definitions for the LDAP client interface for SXEmacs.
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

#ifndef _XEMACS_ELDAP_H_
#define _XEMACS_ELDAP_H_

#ifdef HAVE_LDAP

#ifdef emacs

Lisp_Object Fldap_search_internal(Lisp_Object search_plist);

#endif				/* emacs */

#endif				/* HAVE_LDAP */

#endif				/* _XEMACS_ELDAP_H_ */
