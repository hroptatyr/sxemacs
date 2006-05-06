/*
  openssl.h -- Emacs Lisp binding to OpenSSL ciphers and digests
  Copyright (C) 2005 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

 * This file is part of SXEmacs.
 * 
 * SXEmacs is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with SXEmacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef INCLUDED_openssl_h_
#define INCLUDED_openssl_h_ 1

/* this is to determine what has been configured */
#include <openssl/opensslconf.h>

#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/hmac.h>

/* special asymmetric crypto systems */
#ifndef OPENSSL_NO_RSA
#include <openssl/rsa.h>
#endif

#ifndef OPENSSL_NO_DSA
#include <openssl/dsa.h>
#endif

#ifndef OPENSSL_NO_EC
#include <openssl/ec.h>
#include <openssl/ecdh.h>
#include <openssl/ecdsa.h>
#endif

#ifndef OPENSSL_NO_DH
#include <openssl/dh.h>
#endif

#ifdef HAVE_SOCKETS
#  if !defined(OPENSSL_NO_SSL2) || !defined(OPENSSL_NO_SSL3)
#    include <openssl/x509.h>
#    include <openssl/pem.h>
#    include <openssl/ssl.h>
#endif
#endif

#endif /* INCLUDED_openssl_h_ */


/* opaque EVP_PKEY object structure */
struct Lisp_EVP_PKEY {
	struct lcrecord_header header;
	EVP_PKEY *evp_pkey;
};
typedef struct Lisp_EVP_PKEY Lisp_EVP_PKEY;

DECLARE_LRECORD(evp_pkey, Lisp_EVP_PKEY);

#define XEVPPKEY(x) XRECORD (x, evp_pkey, Lisp_EVP_PKEY)
#define XSETEVPPKEY(x, p) XSETRECORD (x, p, evp_pkey)
#define EVPPKEYP(x) RECORDP (x, evp_pkey)
#define CHECK_EVPPKEY(x) CHECK_RECORD (x, evp_pkey)


#ifdef HAVE_SOCKETS
#  if !defined(OPENSSL_NO_SSL2) || !defined(OPENSSL_NO_SSL3)
/* opaque SSL_CONN object structure
 * this is just an ssl-ish wrap around the process object
 */
struct Lisp_SSL_CONN {
	struct lcrecord_header header;
	SSL_METHOD *ssl_meth;
	SSL_CTX *ssl_ctx;
	SSL *ssl_conn;
	Lisp_Object parent;
	EMACS_INT *infd,*outfd;
	BIO *ssl_bio;
	int connected_p;
	int protected_p;
};
typedef struct Lisp_SSL_CONN Lisp_SSL_CONN;

Lisp_Object make_ssl_conn(Lisp_SSL_CONN *ssl_conn);
Lisp_SSL_CONN *allocate_ssl_conn(void);
void finalize_ssl_conn(void *header, int for_disksave);

DECLARE_LRECORD(ssl_conn, Lisp_SSL_CONN);

#define XSSLCONN(x) XRECORD (x, ssl_conn, Lisp_SSL_CONN)
#define XSETSSLCONN(x, p) XSETRECORD (x, p, ssl_conn)
#define SSLCONNP(x) RECORDP (x, ssl_conn)
#define CHECK_SSLCONN(x) CHECK_RECORD (x, ssl_conn)

#endif	/* !OPENSSL_NO_SSL2 || !OPENSSL_NO_SSL3 */
#endif	/* HAVE_SOCKETS */
