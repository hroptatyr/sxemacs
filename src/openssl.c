/*
  openssl.c -- Emacs Lisp binding to OpenSSL ciphers and digests
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

/* Copyright (C) 1995-1998 Eric Young (eay@cryptsoft.com)
 * All rights reserved.
 *
 * This package is an SSL implementation written
 * by Eric Young (eay@cryptsoft.com).
 * The implementation was written so as to conform with Netscapes SSL.
 * 
 * This library is free for commercial and non-commercial use as long as
 * the following conditions are aheared to.  The following conditions
 * apply to all code found in this distribution, be it the RC4, RSA,
 * lhash, DES, etc., code; not just the SSL code.  The SSL documentation
 * included with this distribution is covered by the same copyright terms
 * except that the holder is Tim Hudson (tjh@cryptsoft.com).
 * 
 * Copyright remains Eric Young's, and as such any Copyright notices in
 * the code are not to be removed.
 * If this package is used in a product, Eric Young should be given attribution
 * as the author of the parts of the library used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    "This product includes cryptographic software written by
 *     Eric Young (eay@cryptsoft.com)"
 *    The word 'cryptographic' can be left out if the rouines from the library
 *    being used are not cryptographic related :-).
 * 4. If you include any Windows specific code (or a derivative thereof) from 
 *    the apps directory (application code) you must include an acknowledgement:
 *    "This product includes software written by Tim Hudson (tjh@cryptsoft.com)"
 * 
 * THIS SOFTWARE IS PROVIDED BY ERIC YOUNG ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 * The licence and distribution terms for any publically available version or
 * derivative of this code cannot be changed.  i.e. this code cannot simply be
 * copied and put under another distribution licence
 * [including the GNU Public Licence.]
 */

/* General overview:
 * openssl provides an assortment of cryptographic routines and interfaces
 * to access them.
 * This API hook attempts to bring them all as pure as possible into SXE
 * elisp. This in turn means that the feature 'openssl is NOT a higher
 * level crypto library for elisp. Personally I consider implementing the
 * latter one, too, based on the API provided by this feature.
 *
 *
 * * Detailed overview:
 *  Currently provided routines:
 *  - all of openssl message digest algorithms (md)
 *  - all of openssl message authentication algorithms (hmac)
 *  - all of openssl (pseudo) random number generators (prng)
 *  - all of openssl symmetric block and stream cipher algorithms (cipher)
 *  - basic functionality of openssl asymmetric crypto-systems (pkey)
 *  - all of openssl envelope handling (hybrid)
 *  - all of EVP interface functionality minus `engine' support
 *  - all of PEM interface functionality
 *  - (very) premature SSL client
 * 
 *  In addition, we are trying hard to provide not only an exact elisp
 *  copy of openssl, but also a _comprehensive_ one
 *
 * 
 * * src/openssl.c: functions overview:
 *
 * - General
 *  ossl-version - version info
 *  ossl-available-digests - list of available message digests
 *  ossl-available-ciphers - list of available ciphers
 *
 * - PRNG
 *  ossl-rand-bytes - generation of (pseudo) randomness
 *
 * - MD
 *  ossl-digest - gateway to digest functions
 *
 * - HMAC
 *  ossl-hmac - gateway to message authentication codes
 *
 * - CIPHER
 *  ossl-bytes-to-key - key generation for symmetric ciphers
 *  ossl-encrypt - gateway to symmetric cipher encryption
 *  ossl-decrypt - gateway to symmetric cipher decryption
 *
 * - PKEY
 * + General
 *  ossl-pkey-p - discriminator of public keys
 *  ossl-pkey-size - selector of public key sizes
 *  ossl-pkey-get-public - strip the private data
 *  Lisp_EVP_PKEY - lrecord object to store public keys
 * + RSA
 *  ossl-rsa-generate-key - constructor of RSA public keys
 *  ossl-rsa-pkey-p - discriminator of RSA public keys
 *  ossl-rsa-subkey-p - comparator of two keys
 * + DSA
 *  ossl-dsa-generate-key - constructor of DSA public keys
 *  ossl-dsa-pkey-p - discriminator of DSA public keys
 *  ossl-dsa-subkey-p - comparator of two keys
 * + EC
 *  ossl-ec-generate-key - constructor of EC public keys
 *  ossl-ec-pkey-p - discriminator of EC public keys
 * + DH
 *  ossl-dh-pkey-p - discriminator of DH public keys
 *
 * - HYBRID
 *  ossl-seal - gateway to public key hybrid (envelope) encryption
 *  ossl-open - gateway to public key hybrid (envelope) decryption
 *
 * - SIGN
 *  ossl-sign - gateway to public key signature
 *  ossl-verify - gateway to public key signature verification
 *
 * - PEM
 *  ossl-pem-read-public-key
 *  ossl-pem-read-key
 *  ossl-pem-write-public-key
 *  ossl-pem-write-key
 *
 * - SSL (it is highly likely to change entirely)
 *  ossl-connect-ssl
 *  ossl-finish-ssl
 *  ossl-get-certificate
 *  ossl-bio-open-network-stream
 *  ossl-send
 *  ossl-receive
 *
 * 
 * * Todo (internally):
 *  - implement the usage of engines
 *  - implement X.509 stuff
 *  - implement actual SSL (somehow the name openssl is derived from that ;P)
 *  - determine a safe way to incorporate EC (elliptic curves) systems
 *
 *
 * * Roadmap:
 *  1. Implement basic C stuff, mostly for accessing the structures
 *     which is evil and insecure if done with an elisp interface
 *  2. Implement higher level API functions (without the guts of the actual
 *     OpenSSL libcrypto implementation)
 *  3. Implement highest level user functions for actual daily consumption
 *     (e.g. keyrings, import/export of keys, stuff like that)
 *  4. Build an API (called CERTS) on top of that which transparently
 *     brings security functions to elisp-libraries
 *     Goals:
 *     - install a master password system a la firefox
 *     - implement an opaque lisp type for storing security relevant stuff
 *     - securify parts of the obarray against other parts of it
 *       (useful e.g. for erbot which otherwise brags your secrets to the
 *        world)
 *
 *
 * * Bugs:
 *  - any function using or needing random data assumes you have /dev/urandom
 *
 *
 * * Examples:
 *
 *  - PRNG:
 *    (ossl-rand-bytes 8)
 *    (base16-encode-string (ossl-rand-bytes 16))
 *
 *  - MD:
 *    (ossl-available-digests)
 *
 *    (ossl-digest 'MD5 "test")
 *    (base16-encode-string (ossl-digest 'MD5 "test"))
 *    ;; compare to
 *    (md5 "test")
 *
 *    (base64-encode-string (ossl-digest 'SHA1 "test"))
 *
 *    (base16-encode-string (ossl-digest 'RIPEMD160 "test"))
 *
 *  - HMAC:
 *    (ossl-hmac 'md5 "testmess" "testpass")
 *
 *    (base16-encode-string (ossl-hmac 'dsa-sha1 "testmess" "testpass"))
 *
 *  - CIPHER:
 *    ;; retrieve a list of available cipher algorithms first
 *    (ossl-available-ciphers)
 *
 *    ;; generate a key/iv pair (iv = initialisation vector)
 *    ;; from a password
 *    (ossl-bytes-to-key 'AES-256-ECB 'RIPEMD160 nil "password" 1)
 *
 *    ;; use a key/iv pair to initiate an encryption
 *    (setq key (ossl-bytes-to-key 'BF-CBC 'DSA-SHA1 "somesalt" "somepass" 24))
 *    (setq enc (ossl-encrypt 'BF-CBC "a test string" (car key) (cdr key)))
 *    ;; of course we can decrypt it again
 *    (ossl-decrypt 'BF-CBC enc (car key) (cdr key))
 *    ;; in contrast:
 *    (ossl-decrypt 'BF-ECB enc (car key) (cdr key))
 *      ;; this one yields an error since BF-CBC is not BF-ECB
 *
 *  - PKEY:
 *  + General:
 *    ;; SOMETHING HERE
 *
 *  + RSA:
 *    ;; generate an rsa key of size 2048
 *    (setq pkey (ossl-rsa-generate-key 2048 17))
 *    (ossl-rsa-pkey-p pkey)
 *
 *    ;; generate an rsa key of size 1024 and flush the private data
 *    (setq k1 (ossl-rsa-generate-key 1024 17))
 *    (setq k2 (ossl-rsa-get-public k1))
 *    (setq k2 (ossl-pkey-get-public k1))
 *    ;; now check if k2 fits into k1 (i.e. if the public data is the same)
 *    (ossl-rsa-subkey-p k2 k1)
 *
 *  + DSA:
 *    ;; generate a dsa key of size 1024 (dsa is digital signature algo)
 *    ;; Note: I dont restrict the size, but it has to be <=1024 if
 *    ;; used to actually sign something
 *    (setq pkey (ossl-dsa-generate-key 1024))
 *    (ossl-dsa-pkey-p pkey)
 *
 *    ;; now generate a dsa key again and flush the private data
 *    ;; k2 can then only be used to verify signatures
 *    (setq k1 (ossl-dsa-generate-key 1024))
 *    (setq k2 (ossl-dsa-get-public k1))
 *    (setq k2 (ossl-pkey-get-public k1))
 *    ;; check if k2 is a public copy of k1
 *    (ossl-dsa-subkey-p k2 k1)
 *
 *  + EC:
 *  Note: For these functions you must have enabled EC in your OpenSSL lib
 *    (setq pkey (ossl-ec-generate-key))
 *    (ossl-ec-pkey-p pkey)
 *    ;; generate an ec (elliptic curve) key
 *    ;; Note: this is probably disabled in your openssl
 *    (when (featurep 'openssl-ec) 
 *      (setq pkey (ossl-ec-generate-key))
 *      (ossl-ec-pkey-p pkey))
 *
 *  + DH:
 *  Note: For these functions you must have enabled DH in your OpenSSL lib
 *    ;; not yet
 *
 *  - HYBRID 
 *    (setq key (ossl-rsa-generate-key 2048 3))
 *    (setq enc (ossl-seal 'AES-256-ECB "a tight secret" key))
 *    (ossl-open 'AES-256-ECB (car enc) key (cadr enc) (caddr enc))
 *    ;; behold also:
 *    (ossl-open 'AES-256-ECB (car enc) key (cadr enc) "some other iv!!!")
 *      ;; this one is okay, too! since AES-256-ECB needs no IV
 *    ;; but:
 *    (setq key (ossl-rsa-generate-key 2048 3))
 *    (ossl-open 'AES-256-ECB (car enc) key (cadr enc) (caddr enc))
 *      ;; this yields probably an error since now key holds another key!
 *
 *  - SIGN
 *    (setq key (ossl-dsa-generate-key 1024))
 *    (setq sig (ossl-sign 'DSA-SHA1 "this is MY msg" key))
 *    (ossl-verify 'DSA-SHA1 "this is MY msg" sig key)
 *    ;; and behold:
 *    (ossl-verify 'DSA-SHA1 "this is not MY msg" sig key)
 *
 *    (setq key (ossl-rsa-generate-key 2048 3))
 *    (setq sig1 (ossl-sign 'RSA-MD5 "this is MY msg" key))
 *    (setq sig2 (ossl-sign 'RSA-MD5 "this is MY other msg" key))
 *    (ossl-verify 'RSA-MD5 "this is MY msg" sig1 key)
 *    ;; and behold:
 *    (ossl-verify 'RSA-SHA1 "this is MY msg" sig2 key)
 *
 *    (setq key (ossl-ec-generate-key))
 *    (setq sig (ossl-sign 'ecdsa-with-SHA1 "this is MY msg" key))
 *    (ossl-verify 'ecdsa-with-SHA1 "this is MY msg" sig key)
 *
 *  - PEM
 *    (setq key (ossl-rsa-generate-key 1024 3))
 *    (ossl-pem-write-key "/tmp/pkey1.pem" key)
 *    (ossl-pem-write-key "/tmp/pkey2.pem" key 'AES-256-ECB "somepass")
 *    (ossl-pem-write-public-key "/tmp/pkeyp.pem" key)
 *
 *  - SSL
 *    ;; no examples yet
 *    ;; in fact the code only works with specially prepared SSL servers
 *    ;; in any of my past tests, I couldnt ever get a connection
 *    ;; to www.redhat.com:443 (https) :(
 *    ;; also, I'd like the SSL network stream to act transparently, i.e.
 *    ;; process-send-string should use SSL_write if an SSL connection
 *    ;; has been established
 *
 */

#include <config.h>

#include "lisp.h"

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"

#ifdef HAVE_SOCKETS
#include "events.h"
#include "process.h"
#include "procimpl.h"
#endif

#include "openssl.h"

#ifdef FILE_CODING
#include "file-coding.h"
#endif

#ifdef HAVE_POSTGRESQL
#include "postgresql.h"
#endif

#define OSSL_CODING Qbinary

#define OSSL_STRING_LENGTH (int)XSTRING_CHAR_LENGTH

static Lisp_Object Qopenssl;

int ossl_pkey_has_public_data(EVP_PKEY *pkey);
int ossl_pkey_has_private_data(EVP_PKEY *pkey);

int rsa_pkey_p(EVP_PKEY *pkey);
#ifndef OPENSSL_NO_RSA
int rsa_pkey_has_public_data(RSA *rsakey);
int rsa_pkey_has_private_data(RSA *rsakey);
#endif

int dsa_pkey_p(EVP_PKEY *pkey);
#ifndef OPENSSL_NO_DSA
int dsa_pkey_has_public_data(DSA *dsakey);
int dsa_pkey_has_private_data(DSA *dsakey);
DSA *dsa_get_public(EVP_PKEY *pk);
#endif

int ec_pkey_p(EVP_PKEY *pkey);
#ifndef OPENSSL_NO_EC
int ec_pkey_has_public_data(EC_KEY *ec_key);
int ec_pkey_has_private_data(EC_KEY *ec_key);
EC_KEY *ec_get_public(EVP_PKEY *pk);
#endif

int dh_pkey_p(EVP_PKEY *pkey);
#ifndef OPENSSL_NO_DH
int dh_pkey_has_public_data(DH *dh_key);
int dh_pkey_has_private_data(DH *dh_key);
DH *dh_get_public(EVP_PKEY *pk);
#endif


/*
 *
 * AUXILIARY
 * 
 */
DEFUN("ossl-version", Fossl_version, 0, 0, 0, /*
Return a descriptive version number of the OpenSSL in use.
					      */
      ())
{
	return build_string(SSLeay_version(SSLEAY_VERSION));
}


DEFUN("ossl-available-digests", Fossl_available_digests, 0, 0, 0, /*
Return a list of digest algorithms in the underlying crypto library.
This yields a plain list of symbols.
								  */
      ())
{
	int nid;
	Lisp_Object digests;

	OpenSSL_add_all_digests();

	digests = Qnil;

	/*  is there a better way to get the size of the nid list? */
	for (nid = 10000; nid >= 0; --nid) {
		const EVP_MD *digest = EVP_get_digestbynid(nid);
		if (digest) {
			const Lisp_Object dgstname =
				Fmake_symbol(build_string(OBJ_nid2sn(nid)));
			digests = Fcons(dgstname,digests);
		}
	}

	EVP_cleanup();

	return digests;
}


DEFUN("ossl-available-ciphers", Fossl_available_ciphers, 0, 0, 0, /*
Return a list of cipher algorithms in the underlying crypto library.
This yields a plain list of symbols.
								  */
      ())
{
	int nid;
	Lisp_Object ciphers;

	OpenSSL_add_all_ciphers();

	ciphers = Qnil;

	/* is there a better way to get the size of the nid list? */
	for (nid = 10000; nid >= 0; --nid) {
		const EVP_CIPHER *cipher = EVP_get_cipherbynid(nid);
		if (cipher) {
			const Lisp_Object ciphname =
				Fmake_symbol(build_string(OBJ_nid2sn(nid)));
			ciphers = Fcons(ciphname,ciphers);
		}
	}

	EVP_cleanup();

	return ciphers;
}


/*
 * 
 * PRNG
 * 
 */
DEFUN("ossl-rand-bytes", Fossl_rand_bytes, 1, 1, 0, /*
Return COUNT bytes of randomness.

Note: You probably want to put a wrapping encoder function
(like `base16-encode-string') around it, since this returns
binary string data.
						    */
      (count))
{
	char *outbuf;
	Lisp_Object l_outbuf;
	int count_ext;

	CHECK_NATNUM(count);
	count_ext = (int)XINT(count);

	/* now allocate some output buffer externally */
	outbuf = (char *)malloc(count_ext);

	if (!RAND_bytes((unsigned char*)outbuf, count_ext)) {
		error ("RAND_bytes did not have enough seed "
		       "to perform operation");
		return Qnil;
	}

	l_outbuf = make_ext_string(outbuf, count_ext, OSSL_CODING);
	free(outbuf);
	return l_outbuf;
}


/*
 *
 * DIGEST HANDLING
 *
 */
DEFUN("ossl-digest", Fossl_digest, 2, 2, 0,	/*
Return the message digest of STRING computed by DIGEST.
DIGEST may be one of the OpenSSL digests you have compiled.
See `ossl-available-digests'.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
						*/
      (digest, string))
{
	EVP_MD_CTX mdctx;
	const EVP_MD *md;
	char md_value[EVP_MAX_MD_SIZE];
	unsigned int md_len;

	CHECK_SYMBOL(digest);
	CHECK_STRING(string);

	OpenSSL_add_all_digests();
	md = EVP_get_digestbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	EVP_MD_CTX_init(&mdctx);
	EVP_DigestInit_ex(&mdctx, md, NULL);
	EVP_DigestUpdate(&mdctx,(char *)XSTRING_DATA(string),
			 XSTRING_LENGTH(string));
	EVP_DigestFinal_ex(&mdctx, (unsigned char *)md_value, &md_len);
	EVP_MD_CTX_cleanup(&mdctx);

	EVP_cleanup();

	return make_ext_string(md_value, md_len, OSSL_CODING);
}


/* 
 *
 * HMAC (aka keyed hashes)
 * 
 */
DEFUN("ossl-hmac", Fossl_hmac, 3, 3, 0, /*
Return the message authentication code of MSG
using the hash function DIGEST and the key PASSWORD.

Note: You probably want to put a wrapping encoder function
(like `base16-encode-string') around it, since this returns
binary string data.
					*/
      (digest, msg, password))
{
	const EVP_MD *md;
	HMAC_CTX hmacctx;

	/* buffer for the ciphertext */
	char outbuf[EVP_MAX_MD_SIZE];
	unsigned int outlen;
	/* buffer for external password */
	char *password_ext;
	int password_len;
	/* buffer for external message */
	char *msg_ext;
	int msg_len;

	CHECK_SYMBOL(digest);
	CHECK_STRING(msg);
	CHECK_STRING(password);

	OpenSSL_add_all_digests();
	md = (EVP_MD *)EVP_get_digestbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, password,
			    C_STRING_ALLOCA, password_ext, OSSL_CODING);
	password_len = OSSL_STRING_LENGTH(password);

	TO_EXTERNAL_FORMAT (LISP_STRING, msg,
			    C_STRING_ALLOCA, msg_ext, OSSL_CODING);
	msg_len = OSSL_STRING_LENGTH(msg);

	HMAC_CTX_init(&hmacctx);
	HMAC_Init(&hmacctx,password_ext,password_len,md);
	HMAC_Update(&hmacctx,(unsigned char *)msg_ext,msg_len);
	HMAC_Final(&hmacctx,(unsigned char *)outbuf,&outlen);
	HMAC_CTX_cleanup(&hmacctx);

	EVP_cleanup();

	return make_ext_string(outbuf, outlen, OSSL_CODING);
}


/* 
 * 
 * SYMMETRIC CIPHER
 * 
 */
DEFUN("ossl-bytes-to-key", Fossl_bytes_to_key, 5, 5, 0, /*
Derive a key and IV (initialisation vector) suitable for a cipher.
Return a cons of \(key . iv\).

PASSWORD is an arbitrary string which is hashed to
derive a unique key and IV.

CIPHER is the cipher to derive the key and IV for.

DIGEST is the message digest to use.

SALT is used as a salt in the derivation,
use nil here to indicate that no salt is used.

COUNT is the iteration count to use, this indicates
how often the hash algorithm is called recursively.
							*/
      (cipher, digest, salt, password, count))
{
	const EVP_MD *md;
	const EVP_CIPHER *ciph;
	const char *salt_ext;

	char *password_ext;
	int password_len;

	char key[EVP_MAX_KEY_LENGTH];
	char iv[EVP_MAX_IV_LENGTH];

	CHECK_STRING(password);
	CHECK_SYMBOL(cipher);
	CHECK_SYMBOL(digest);
	CHECK_NATNUM(count);


	if (!XINT(count))
		error ("count has to be a non-zero positive integer");

	OpenSSL_add_all_algorithms();
	md = EVP_get_digestbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(digest)->name));
	ciph = EVP_get_cipherbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	if (NILP(salt)) {
		salt_ext = NULL;
	} else {
		CHECK_STRING(salt);
		TO_EXTERNAL_FORMAT (LISP_STRING, salt,
				    C_STRING_ALLOCA, salt_ext, OSSL_CODING);
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, password,
			    C_STRING_ALLOCA, password_ext, OSSL_CODING);
	password_len = OSSL_STRING_LENGTH(password);

	EVP_BytesToKey(ciph, md, (unsigned char *)salt_ext,
		       (unsigned char *)password_ext, password_len,
		       XINT(count),
		       (unsigned char *)key,
		       (unsigned char *)iv);

	EVP_cleanup();

	return Fcons(make_ext_string(key, EVP_MAX_KEY_LENGTH, OSSL_CODING),
		     make_ext_string(iv, EVP_MAX_IV_LENGTH, OSSL_CODING));
}


DEFUN("ossl-encrypt", Fossl_encrypt, 4, 4, 0,	/*
Return the cipher of STRING computed by CIPHER.
CIPHER may be one of the OpenSSL cipher algorithms
you have compiled.

See `ossl-available-ciphers'.

Note: You probably want to put a wrapping encoder function
(like `base16-encode-string') around it, since this returns
binary string data.
						*/
      (cipher, string, key, iv))
{
	/* buffer for the external string */
	char *string_ext;
	int string_len;
	/* buffer for the ciphertext */
	char *outbuf;
	int outlen;
	Lisp_Object l_outbuf;
	/* buffer for key */
	char *key_ext;
	/* buffer for iv */
	char *iv_ext;

	/* declarations for the cipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX ciphctx;

	int tmplen;


	CHECK_SYMBOL(cipher);
	CHECK_STRING(string);
	CHECK_STRING(key);
	CHECK_STRING(iv);


	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	if (!string_len)
		error ("string must be of non-zero positive length.");

	OpenSSL_add_all_algorithms();
	/* ENGINE_load_builtin_engines(); */
	/* atm, no support for different engines */
	ciph = EVP_get_cipherbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	/* now allocate some output buffer externally
	 * this one has to be at least EVP_CIPHER_block_size bigger
	 * since block algorithms merely operate blockwise
	 */
	outbuf = (char *)malloc(XSTRING_LENGTH(string) +
				EVP_CIPHER_block_size(ciph));

	TO_EXTERNAL_FORMAT (LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT (LISP_STRING, iv,
			    C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	EVP_CIPHER_CTX_init(&ciphctx);
	if (!EVP_EncryptInit(&ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		error ("error in EncryptInit");
	}
	if (!EVP_EncryptUpdate(&ciphctx,
			       (unsigned char *)outbuf, &outlen,
			       (unsigned char *)string_ext, string_len)) {
		EVP_cleanup();
		error ("error in EncryptUpdate");
	}
	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_EncryptFinal(&ciphctx,
			      (unsigned char *)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		error ("error in EncryptFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(&ciphctx);

	l_outbuf = make_ext_string(outbuf, outlen, OSSL_CODING);
	free(outbuf);

	EVP_cleanup();

	return l_outbuf;
}

DEFUN("ossl-decrypt", Fossl_decrypt, 4, 4, 0,	/*
Return the cipher of STRING computed by CIPHER.
CIPHER may be one of the OpenSSL cipher algorithms
you have compiled.

See `ossl-available-ciphers'.
						*/
      (cipher, string, key, iv))
{
	/* buffer for the external string */
	char *string_ext;
	int string_len;
	/* buffer for the deciphered text */
	char *outbuf;
	int outlen;
	Lisp_Object l_outbuf;
	/* buffer for key */
	char *key_ext;
	/* buffer for iv */
	char *iv_ext;

	/* declarations for the decipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX ciphctx;

	int tmplen;


	CHECK_SYMBOL(cipher);
	CHECK_STRING(string);
	CHECK_STRING(key);
	CHECK_STRING(iv);


	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	if (!string_len) 
		error ("string must be of non-zero positive length.");

	OpenSSL_add_all_algorithms();
	/* ENGINE_load_builtin_engines(); */
	/* atm, no support for different engines */
	ciph = EVP_get_cipherbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	/* now allocate some output buffer externally */
	outbuf = (char *)malloc(XSTRING_LENGTH(string));

	TO_EXTERNAL_FORMAT (LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT (LISP_STRING, iv,
			    C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	EVP_CIPHER_CTX_init(&ciphctx);
	if (!EVP_DecryptInit(&ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		error ("error in DecryptInit");
	}
	if (!EVP_DecryptUpdate(&ciphctx,
			       (unsigned char *)outbuf, &outlen,
			       (unsigned char *)string_ext,string_len)) {
		EVP_cleanup();
		error ("error in DecryptUpdate");
	}
	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_DecryptFinal(&ciphctx,
			      (unsigned char *)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		error ("error in DecryptFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(&ciphctx);

	l_outbuf = make_ext_string(outbuf, outlen, OSSL_CODING);
	free(outbuf);

	EVP_cleanup();

	return l_outbuf;
}


/* 
 * 
 * ASYMMETRIC CIPHER
 * 
 */
/* This is an opaque object for storing PKEYs in lisp */
Lisp_Object Qevp_pkeyp;

static Lisp_Object
make_evp_pkey(Lisp_EVP_PKEY * evp_pkey)
{
	Lisp_Object lisp_evp_pkey;
	XSETEVPPKEY(lisp_evp_pkey, evp_pkey);
	return lisp_evp_pkey;
}

static Lisp_Object
mark_evp_pkey(Lisp_Object obj)
{
	return Qnil;
}

static void
print_evp_pkey(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char buf[256];
	EVP_PKEY *pkey;

	pkey = (XEVPPKEY(obj))->evp_pkey;

	if (rsa_pkey_p(pkey))
		strcpy(buf, "#<OpenSSL RSA"); 
	else if (dsa_pkey_p(pkey))
		strcpy(buf, "#<OpenSSL DSA"); 
	else if (ec_pkey_p(pkey))
		strcpy(buf, "#<OpenSSL EC"); 
	else
		sprintf(buf, "#<OpenSSL <%2d>",
			EVP_PKEY_type(pkey->type));

	if (ossl_pkey_has_private_data(pkey))
		strcat(buf, " private/public key");
	else if (ossl_pkey_has_public_data(pkey))
		strcat(buf, " public key");
	else
		strcat(buf, " empty key");

	if (EVP_PKEY_size(pkey) > 0)
		sprintf(buf+strlen(buf), ", size %d", EVP_PKEY_size(pkey)*8);

	strcat(buf, ">");

	if (print_readably)
		error("printing unreadable object %s", buf);
	else
		write_c_string(buf, printcharfun);
}

static Lisp_EVP_PKEY *
allocate_evp_pkey(void)
{
	Lisp_EVP_PKEY *evp_pkey =
		alloc_lcrecord_type(Lisp_EVP_PKEY, &lrecord_evp_pkey);
	evp_pkey->evp_pkey = EVP_PKEY_new();
	return evp_pkey;
}

static void
finalize_evp_pkey(void *header, int for_disksave)
{
	Lisp_EVP_PKEY *evp_pkey = (Lisp_EVP_PKEY *) header;

	if (evp_pkey->evp_pkey) {
		EVP_PKEY_free(evp_pkey->evp_pkey);
		evp_pkey->evp_pkey = (EVP_PKEY *) NULL;
	}
}

DEFINE_LRECORD_IMPLEMENTATION("evp_pkey", evp_pkey,
			      mark_evp_pkey, print_evp_pkey, finalize_evp_pkey,
			      NULL, NULL, 0, Lisp_EVP_PKEY);


DEFUN("ossl-pkey-p", Fossl_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of RSA type.
					    */
      (pkey))
{
	if (EVPPKEYP(pkey))
		return Qt;
	else
		return Qnil;
}

DEFUN("ossl-pkey-size", Fossl_pkey_size, 1, 1, 0, /*
Return the size a public key PKEY in bits. 
						  */
      (pkey))
{
	EVP_PKEY *pk;

	CHECK_EVPPKEY(pkey);

	pk = (XEVPPKEY(pkey))->evp_pkey;

	return make_int(EVP_PKEY_size(pk)*8);
}

int
ossl_pkey_has_public_data(EVP_PKEY *pkey)
{
	if (rsa_pkey_p(pkey)) {
#ifndef OPENSSL_NO_RSA
		return rsa_pkey_has_public_data((pkey->pkey).rsa);
#else
		return 0;
#endif
	} else if (dsa_pkey_p(pkey)) {
#ifndef OPENSSL_NO_DSA
		return dsa_pkey_has_public_data((pkey->pkey).dsa);
#else
		return 0;
#endif
	} else if (ec_pkey_p(pkey)) {
#ifndef OPENSSL_NO_EC
		return ec_pkey_has_public_data((pkey->pkey).eckey);
#else
		return 0;
#endif
	} else if (dh_pkey_p(pkey)) {
#ifndef OPENSSL_NO_DH
		return dh_pkey_has_public_data((pkey->pkey).dh);
#else
		return 0;
#endif
	} else
		return 0;
}
int
ossl_pkey_has_private_data(EVP_PKEY *pkey)
{
	if (rsa_pkey_p(pkey)) {
#ifndef OPENSSL_NO_RSA
		return rsa_pkey_has_private_data((pkey->pkey).rsa);
#else
		return 0;
#endif
	} else if (dsa_pkey_p(pkey)) {
#ifndef OPENSSL_NO_DSA
		return dsa_pkey_has_private_data((pkey->pkey).dsa);
#else
		return 0;
#endif
	} else if (ec_pkey_p(pkey)) {
#ifndef OPENSSL_NO_EC
		return ec_pkey_has_private_data((pkey->pkey).eckey);
#else
		return 0;
#endif
	} else if (dh_pkey_p(pkey)) {
#ifndef OPENSSL_NO_DH
		return dh_pkey_has_private_data((pkey->pkey).dh);
#else
		return 0;
#endif
	} else
		return 0;
}

DEFUN("ossl-pkey-get-public", Fossl_pkey_get_public, 1, 1, 0, /*
Return a copy of PKEY stripped by the private data.

This function is not (yet) native OpenSSL.
								      */
      (pkey))
{
	EVP_PKEY *pk;
	EVP_PKEY *pkout;
	Lisp_EVP_PKEY *lisp_pkout;

	CHECK_EVPPKEY(pkey);

	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (!(ossl_pkey_has_public_data(pk)))
		error ("key must have public data");

	pkout = EVP_PKEY_new();
	if (rsa_pkey_p(pk)) {
#ifndef OPENSSL_NO_RSA
		EVP_PKEY_assign_RSA(pkout, RSAPublicKey_dup((pk->pkey).rsa));
#endif
	} else if (dsa_pkey_p(pk)) {
#ifndef OPENSSL_NO_DSA
		EVP_PKEY_assign_DSA(pkout, dsa_get_public(pk));
#endif
	} else if (ec_pkey_p(pk)) {
#ifndef OPENSSL_NO_EC
		EVP_PKEY_assign_EC_KEY(pkout, ec_get_public(pk));
#endif
	} else
		error ("no method to strip private data yet");

	lisp_pkout = allocate_evp_pkey();
	lisp_pkout->evp_pkey = pkout;

	return make_evp_pkey(lisp_pkout);
}

/* RSA */
int
rsa_pkey_p(EVP_PKEY *pkey)
{
	int type;

	type = EVP_PKEY_type(pkey->type);

#ifndef OPENSSL_NO_RSA
	return ((type == EVP_PKEY_RSA) ||
		(type == EVP_PKEY_RSA2));
#else
	return 0;
#endif
}
#ifndef OPENSSL_NO_RSA
int
rsa_pkey_has_public_data(RSA *rsakey)
{
	return (!(rsakey->n == NULL) &&
		!(rsakey->e == NULL));
}
int
rsa_pkey_has_private_data(RSA *rsakey)
{
	return (rsa_pkey_has_public_data(rsakey) &&
		!(rsakey->d == NULL));
}

DEFUN("ossl-rsa-generate-key", Fossl_rsa_generate_key, 2, 2, 0, /*
Return an RSA public key with of length BITS and exponent EXP.
								*/
      (bits, exp))
{
	EVP_PKEY *pkey;
	RSA *rsakey;
	Lisp_EVP_PKEY *lisp_pkey;


	CHECK_NATNUM(bits);
	CHECK_NATNUM(exp);


	if (!XINT(bits))
		error ("modulus size must be a non-zero positive integer");
	if (!(XINT(exp) % 2))
		error ("exponent must be an odd positive integer");

	pkey = EVP_PKEY_new();
	rsakey = RSA_generate_key(XINT(bits), XINT(exp), NULL, NULL);
	EVP_PKEY_assign_RSA(pkey, rsakey);

	lisp_pkey = allocate_evp_pkey();
	lisp_pkey->evp_pkey = pkey;

	return make_evp_pkey(lisp_pkey);
}

DEFUN("ossl-rsa-pkey-p", Fossl_rsa_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of RSA type.
						    */
      (pkey))
{
	EVP_PKEY *pk;

	if (!EVPPKEYP(pkey))
		return Qnil;

	pk = (XEVPPKEY(pkey))->evp_pkey;

	if (rsa_pkey_p(pk))
		return Qt;
	else
		return Qnil;
}

DEFUN("ossl-rsa-subkey-p", Fossl_rsa_subkey_p, 2, 2, 0, /*
Return t iff PKEY1 is a subkey of PKEY2.
I.e. if PKEY1 has the same public key data as PKEY2 and
PKEY2 has all private data.

This function is not native OpenSSL.
							*/
      (pkey1, pkey2))
{
	EVP_PKEY *pk1;
	EVP_PKEY *pk2;
	RSA *rk1;
	RSA *rk2;

	CHECK_EVPPKEY(pkey1);
	CHECK_EVPPKEY(pkey2);

	pk1 = (XEVPPKEY(pkey1))->evp_pkey;
	pk2 = (XEVPPKEY(pkey2))->evp_pkey;

	/* perform a type check first */
	if (!rsa_pkey_p(pk1))
		error ("pkey1 must be of RSA type");
	if (!rsa_pkey_p(pk2))
		error ("pkey2 must be of RSA type");
	
	rk1 = (pk1->pkey).rsa;
	rk2 = (pk2->pkey).rsa;

	if (rsa_pkey_has_private_data(rk2) &&
	    rsa_pkey_has_public_data(rk1) &&
	    (!BN_cmp(rk1->n, rk2->n)) &&
	    (!BN_cmp(rk1->e, rk2->e)))
		return Qt;
	else
		return Qnil;
}
#endif /* OPENSSL_NO_RSA */


/* DSA */
int
dsa_pkey_p(EVP_PKEY *pkey)
{
	int type;

	type = EVP_PKEY_type(pkey->type);

#ifndef OPENSSL_NO_DSA
	return ((type == EVP_PKEY_DSA) ||
		(type == EVP_PKEY_DSA1) ||
		(type == EVP_PKEY_DSA2) ||
		(type == EVP_PKEY_DSA3) ||
		(type == EVP_PKEY_DSA4));
#else
	return 0;
#endif
}
#ifndef OPENSSL_NO_DSA
int
dsa_pkey_has_public_data(DSA *dsakey)
{
	return (!(dsakey->p == NULL) &&
		!(dsakey->q == NULL) &&
		!(dsakey->g == NULL) &&
		!(dsakey->pub_key == NULL));
}
int
dsa_pkey_has_private_data(DSA *dsakey)
{
	return (dsa_pkey_has_public_data(dsakey) &&
		!(dsakey->priv_key == NULL));
}

DEFUN("ossl-dsa-generate-key", Fossl_dsa_generate_key, 1, 2, 0, /*
Return a DSA public key with of length BITS seeded with (optional) SEED.
								*/
      (bits, seed))
{
	EVP_PKEY *pkey;
	DSA *dsakey;
	Lisp_EVP_PKEY *lisp_pkey;
	char *seed_ext;
	int seed_len;
	int counter_ret;
	int h_ret;


	CHECK_NATNUM(bits);


	if (!XINT(bits))
		error ("prime number size must be a non-zero positive integer");

	if (NILP(seed)) {
		seed_ext = NULL;
		seed_len = 0;
	} else {
		CHECK_STRING(seed);
		TO_EXTERNAL_FORMAT (LISP_STRING, seed,
				    C_STRING_ALLOCA, seed_ext, OSSL_CODING);
		seed_len = OSSL_STRING_LENGTH(seed);
	}

	pkey = EVP_PKEY_new();
	dsakey = DSA_generate_parameters(XINT(bits),
					 (unsigned char*)seed_ext, seed_len,
					 &counter_ret, (void*)&h_ret,
					 NULL, NULL);
	if (!DSA_generate_key(dsakey))
		error ("error during generation of DSA key");

	EVP_PKEY_assign_DSA(pkey, dsakey);

	lisp_pkey = allocate_evp_pkey();
	lisp_pkey->evp_pkey = pkey;

	return make_evp_pkey(lisp_pkey);
}

DEFUN("ossl-dsa-pkey-p", Fossl_dsa_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of RSA type.
						    */
      (pkey))
{
	EVP_PKEY *pk;

	if (!EVPPKEYP(pkey))
		return Qnil;

	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (dsa_pkey_p(pk))
		return Qt;
	else
		return Qnil;
}

DSA *
dsa_get_public(EVP_PKEY *pk)
{
	DSA *key;

	key = DSA_new();
	memcpy(key, (pk->pkey).dsa, sizeof(DSA));

	/* now kill the private data */
	key->priv_key = NULL;

	return key;
}

DEFUN("ossl-dsa-subkey-p", Fossl_dsa_subkey_p, 2, 2, 0, /*
Return t iff PKEY1 is a subkey of PKEY2.
I.e. if PKEY1 has the same public key data as PKEY2 and
PKEY2 has all private data.

This function is not native OpenSSL.
							*/
      (pkey1, pkey2))
{
	EVP_PKEY *pk1;
	EVP_PKEY *pk2;
	DSA *dk1;
	DSA *dk2;

	CHECK_EVPPKEY(pkey1);
	CHECK_EVPPKEY(pkey2);

	pk1 = (XEVPPKEY(pkey1))->evp_pkey;
	pk2 = (XEVPPKEY(pkey2))->evp_pkey;

	/* perform a type check first */
	if (!dsa_pkey_p(pk1))
		error ("pkey1 must be of DSA type");
	if (!dsa_pkey_p(pk2))
		error ("pkey2 must be of DSA type");
	
	dk1 = (pk1->pkey).dsa;
	dk2 = (pk2->pkey).dsa;

	if (dsa_pkey_has_private_data(dk2) &&
	    dsa_pkey_has_public_data(dk1) &&
	    (!BN_cmp(dk1->p, dk2->p)) &&
	    (!BN_cmp(dk1->q, dk2->q)) &&
	    (!BN_cmp(dk1->g, dk2->g)) &&
	    (!BN_cmp(dk1->pub_key, dk2->pub_key)))
		return Qt;
	else
		return Qnil;
}
#endif /* OPENSSL_NO_DSA */


/* EC */
int
ec_pkey_p(EVP_PKEY *pkey)
{
	int type;

	type = EVP_PKEY_type(pkey->type);

#ifndef OPENSSL_NO_EC
	return (type == EVP_PKEY_EC);
#else
	return 0;
#endif
}
#ifndef OPENSSL_NO_EC
int
ec_pkey_has_public_data(EC_KEY *ec_key)
{
	return (!(ec_key->group == NULL) &&
		!(ec_key->pub_key == NULL));
}
int
ec_pkey_has_private_data(EC_KEY *ec_key)
{
	return (ec_pkey_has_public_data(ec_key) &&
		!(ec_key->priv_key == NULL));
}

DEFUN("ossl-ec-generate-key", Fossl_ec_generate_key, 0, 0, 0, /*
Return a EC public key on the named curve secp192k1.
							      */
      ())
{
	EVP_PKEY *pkey;
	EC_KEY *eckey = EC_KEY_new();
	Lisp_EVP_PKEY *lisp_pkey;

	pkey = EVP_PKEY_new();
	eckey->group = EC_GROUP_new_by_nid (NID_secp192k1);

	if (eckey->group == NULL)
		error ("error during generation of EC group.");

	if (!EC_KEY_generate_key(eckey))
		error ("error during generation of EC key");

	EVP_PKEY_assign_EC_KEY(pkey, eckey);

	lisp_pkey = allocate_evp_pkey();
	lisp_pkey->evp_pkey = pkey;

	return make_evp_pkey(lisp_pkey);
}

DEFUN("ossl-ec-pkey-p", Fossl_ec_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of RSA type.
						  */
      (pkey))
{
	EVP_PKEY *pk;
	int type;

	if (!EVPPKEYP(pkey))
		return Qnil;

	pk = (XEVPPKEY(pkey))->evp_pkey;
	type = EVP_PKEY_type(pk->type);
	if (type == EVP_PKEY_EC)
		return Qt;
	else
		return Qnil;
}

EC_KEY *
ec_get_public(EVP_PKEY *pk)
{
	EC_KEY *key;

	key = EC_KEY_dup((pk->pkey).eckey);

	/* now kill the private data */
	key->priv_key = NULL;

	return key;
}
#endif /* OPENSSL_NO_EC */


/* DH */
int
dh_pkey_p(EVP_PKEY *pkey)
{
	int type;

	type = EVP_PKEY_type(pkey->type);

#ifndef OPENSSL_NO_DH
	return (type == EVP_PKEY_DH);
#else
	return 0;
#endif
}
#ifndef OPENSSL_NO_DH
int
dh_pkey_has_public_data(DH *dhkey)
{
	return (!(dhkey->p == NULL) &&
		!(dhkey->g == NULL) &&
		!(dhkey->pub_key == NULL));
}
int
dh_pkey_has_private_data(DH *dhkey)
{
	return (dh_pkey_has_public_data(dhkey) &&
		!(dhkey->priv_key == NULL));
}

DEFUN("ossl-dh-pkey-p", Fossl_dh_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of DH type.
						    */
      (pkey))
{
	EVP_PKEY *pk;

	if (!EVPPKEYP(pkey))
		return Qnil;

	pk = (XEVPPKEY(pkey))->evp_pkey;

	if (dh_pkey_p(pk))
		return Qt;
	else
		return Qnil;
}

#endif /* OPENSSL_NO_DH */


/* more general access functions */
DEFUN("ossl-seal", Fossl_seal, 3, 3, 0, /*
Return an envelope derived from encrypting STRING by CIPHER under PKEY
with the hybride technique.

That is, create a random key/iv pait for the symmetric encryption with 
CIPHER and encrypt that key/iv asymmetrically with the provided public
key.

The envelope return is a list 
\(encrypted_string encrypted_key encrypted_iv\)
where
`encrypted_string' is the (symmetrically) encrypted message
`encrypted_key' is the (asymmetrically) encrypted random key
`encrypted_iv' is the (asymmetrically) encrypted random iv

Note: You probably want to put a wrapping encoder function
(like `base16-encode-string') around it, since this returns
binary string data.
					*/
      (cipher, string, pkey))
{
	/* declarations for the cipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX ciphctx;
	/* declarations for the pkey */
	EVP_PKEY *pk[1];
	int npubk;
	unsigned char *ekey[1];
	int ekey_len[1];
	Lisp_Object l_ekey;
	/* buffer for the generated IV */
	char iv[EVP_MAX_IV_LENGTH];
	Lisp_Object l_iv;
	/* buffer for output */
	unsigned char *outbuf;
	unsigned int outlen;
	Lisp_Object l_outbuf;
	/* buffer for external string data */
	char *string_ext;
	int string_len;

	int tmplen;


	CHECK_SYMBOL(cipher);
	CHECK_STRING(string);
	CHECK_EVPPKEY(pkey);


	pk[0] = (XEVPPKEY(pkey))->evp_pkey;
	if (!ossl_pkey_has_public_data(pk[0]))
		error ("cannot seal, key has no public key data");
	npubk = 1;

	ekey[0] = (unsigned char *)malloc(EVP_PKEY_size(pk[0]));

	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	OpenSSL_add_all_algorithms();
	ciph = EVP_get_cipherbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	/* now allocate some output buffer externally
	 * this one has to be at least EVP_CIPHER_block_size bigger
	 * since block algorithms merely operate blockwise
	 */
	outbuf = (unsigned char *)malloc(XSTRING_LENGTH(string) +
					 EVP_CIPHER_block_size(ciph));

	EVP_CIPHER_CTX_init(&ciphctx);
	if (!(EVP_SealInit(&ciphctx, ciph,
			   (unsigned char **)&ekey, (int *)&ekey_len,
			   (unsigned char *)&iv,
			   (EVP_PKEY **)&pk, npubk)==npubk)) {
		EVP_cleanup();
		error ("error in SealInit");
	}
	if (!EVP_SealUpdate(&ciphctx, outbuf, (int *)&outlen,
			    (unsigned char*)string_ext, string_len)) {
		EVP_cleanup();
		error ("error in SealUpdate");
	}
	if (!EVP_SealFinal(&ciphctx, (unsigned char*)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		error ("error in SealFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(&ciphctx);

	l_outbuf = make_ext_string((char *)outbuf, outlen, OSSL_CODING);
	l_ekey = make_ext_string((char *)ekey[0],ekey_len[0], OSSL_CODING);
	l_iv = make_ext_string(iv,EVP_CIPHER_iv_length(ciph), OSSL_CODING);
	free(outbuf);
	free(ekey[0]);
	EVP_cleanup();

	return list3(l_outbuf, l_ekey, l_iv);
}


DEFUN("ossl-open", Fossl_open, 4, 5, 0, /*
Return the deciphered message STRING from an envelope
obtained by `ossl-seal'.

CIPHER is the cipher to use (the same as in `ossl-seal')
STRING is the encrypted message
PKEY is the private key
EKEY is the encrypted random key
EIV is the encrypted iv
					*/
      (cipher, string, pkey, ekey, eiv))
{
	/* declarations for the cipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX ciphctx;
	/* declarations for the pkey */
	EVP_PKEY *pk;
	/* buffer for external ekey data */
	char *ekey_ext;
	int ekey_len;
	/* buffer for external eiv data */
	char *eiv_ext;
	/* buffer for output */
	unsigned char *outbuf;
	unsigned int outlen;
	Lisp_Object l_outbuf;
	/* buffer for external string data */
	char *string_ext;
	int string_len;

	int tmplen;


	CHECK_SYMBOL(cipher);
	CHECK_STRING(string);
	CHECK_EVPPKEY(pkey);
	CHECK_STRING(ekey);


	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (!ossl_pkey_has_private_data(pk))
		error ("cannot open, key has no private key data");

	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);
	TO_EXTERNAL_FORMAT (LISP_STRING, ekey,
			    C_STRING_ALLOCA, ekey_ext, OSSL_CODING);
	ekey_len = OSSL_STRING_LENGTH(ekey);

	OpenSSL_add_all_algorithms();
	ciph = EVP_get_cipherbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	if (NILP(eiv))
		eiv_ext = NULL;
	else {
		CHECK_STRING(eiv);
		TO_EXTERNAL_FORMAT (LISP_STRING, eiv,
				    C_STRING_ALLOCA, eiv_ext, OSSL_CODING);
	}

	/* now allocate some output buffer externally */
	outbuf = (unsigned char *)malloc(XSTRING_LENGTH(string));

	EVP_CIPHER_CTX_init(&ciphctx);
	if (!EVP_OpenInit(&ciphctx, ciph,
			  (unsigned char*)ekey_ext,
			  (unsigned int)ekey_len,
			  (unsigned char*)eiv_ext, pk)) {
		EVP_cleanup();
		error ("error in OpenInit");
	}
	if (!EVP_OpenUpdate(&ciphctx, outbuf, (int *)&outlen,
			    (unsigned char*)string_ext,
			    (unsigned int)string_len)) {
		EVP_cleanup();
		error ("error in OpenUpdate");
	}
	if (!EVP_OpenFinal(&ciphctx, outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		error ("error in OpenFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(&ciphctx);

	l_outbuf = make_ext_string((char *)outbuf, outlen, OSSL_CODING);
	free(outbuf);

	EVP_cleanup();

	return l_outbuf;
}


DEFUN("ossl-sign", Fossl_sign, 3, 3, 0, /*
Return a signature obtained by signing STRING under DIGEST with PKEY.

That is, hash the message STRING with the message digest DIGEST and
encrypt the result with the private key PKEY.

Note: Due to some relationship between the public key system and the
message digest you cannot use all digests with all private keys.
The most certain results will be achieved using
RSA keys with RSA-* digests, DSA keys with DSA-* digests.

See `ossl-available-digests'.

Note: You probably want to put a wrapping encoder function
(like `base16-encode-string') around it, since this returns
binary string data.
					*/
      (digest, string, pkey))
{
	/* declarations for the cipher */
	const EVP_MD *md;
	EVP_MD_CTX mdctx;
	/* declarations for the pkey */
	EVP_PKEY *pk;
	/* buffer for output */
	unsigned char *outbuf;
	unsigned int outlen;
	Lisp_Object l_outbuf;
	/* buffer for external string data */
	char *string_ext;
	int string_len;


	CHECK_SYMBOL(digest);
	CHECK_STRING(string);
	CHECK_EVPPKEY(pkey);


	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (!ossl_pkey_has_private_data(pk))
		error ("cannot sign, key has no private key data");

	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	OpenSSL_add_all_algorithms();
	md = EVP_get_digestbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	/* now allocate some output buffer externally */
	outbuf = (unsigned char *)malloc(EVP_PKEY_size(pk));

	EVP_MD_CTX_init(&mdctx);
	if (!(EVP_SignInit(&mdctx, md))) {
		EVP_cleanup();
		error ("error in SignInit");
	}
	if (!EVP_SignUpdate(&mdctx, string_ext, string_len)) {
		EVP_cleanup();
		error ("error in SignUpdate");
	}
	if (!EVP_SignFinal(&mdctx, outbuf, &outlen, pk)) {
		EVP_cleanup();
		error ("error in SignFinal");
	}
	EVP_MD_CTX_cleanup(&mdctx);

	l_outbuf = make_ext_string((char *)outbuf, outlen, OSSL_CODING);
	free(outbuf);

	EVP_cleanup();

	return l_outbuf;
}

DEFUN("ossl-verify", Fossl_verify, 4, 4, 0, /*
Return t iff SIG is a valid signature of STRING under DIGEST made by PKEY.

That is, hash the message STRING with the message digest DIGEST, then
decrypt the signature SIG with the public key PKEY.
Compare the results and return t iff both hashes are equal.

DIGEST is the digest to use (the same as in `ossl-sign')
STRING is the message
SIG is the signature of message
PKEY is the public key
					    */
      (digest, string, sig, pkey))
{
	/* declarations for the cipher */
	const EVP_MD *md;
	EVP_MD_CTX mdctx;
	/* declarations for the pkey */
	EVP_PKEY *pk;
	/* buffer for external signature data */
	char *sig_ext;
	int sig_len;
	/* buffer for external string data */
	char *string_ext;
	int string_len;

	int result;


	CHECK_SYMBOL(digest);
	CHECK_STRING(string);
	CHECK_STRING(sig);
	CHECK_EVPPKEY(pkey);


	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (!ossl_pkey_has_public_data(pk))
		error ("cannot verify, key has no public key data");

	OpenSSL_add_all_algorithms();
	md = EVP_get_digestbyname(
		(char *)XSTRING_DATA(
			(Lisp_Object)XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);
	TO_EXTERNAL_FORMAT (LISP_STRING, sig,
			    C_STRING_ALLOCA, sig_ext, OSSL_CODING);
	sig_len = OSSL_STRING_LENGTH(sig);

	EVP_MD_CTX_init(&mdctx);
	if (!EVP_VerifyInit(&mdctx, md)) {
		EVP_cleanup();
		error ("error in VerifyInit");
	}
	if (!EVP_VerifyUpdate(&mdctx, string_ext, string_len)) {
		EVP_cleanup();
		error ("error in VerifyUpdate");
	}
	result = EVP_VerifyFinal(&mdctx, (unsigned char*)sig_ext, sig_len, pk);
	if (result == -1) {
		EVP_cleanup();
		error ("error in VerifyFinal");
	}
	EVP_MD_CTX_cleanup(&mdctx);

	EVP_cleanup();

	if (result)
		return Qt;
	else
		return Qnil;
}


/*
 *
 * PEM
 *
 */
DEFUN("ossl-pem-read-public-key", Fossl_pem_read_public_key, 1, 1, 0, /*
Return a key (the public part) stored in a PEM structure from FILE.
							*/
      (file))
{
	/* declarations for the pkey */
	EVP_PKEY *pk;
	Lisp_EVP_PKEY *lisp_pkey;
	/* output file */
	FILE *fp;


	CHECK_STRING(file);


	if ((fp = fopen((char *)XSTRING_DATA(file),"r")) == NULL)
		error ("error opening file.");

	pk = PEM_read_PUBKEY(fp, NULL, 0, NULL);
	if (pk == NULL)
		error ("error reading PEM file.");

	fclose(fp);

	lisp_pkey = allocate_evp_pkey();
	lisp_pkey->evp_pkey = pk;
	return make_evp_pkey(lisp_pkey);
}

DEFUN("ossl-pem-read-key", Fossl_pem_read_key, 1, 2, 0, /*
Return a key stored in a PEM structure from FILE.
If the (private part of the) key is protected with a password
provide (optional) PASSWORD.
							*/
      (file, password))
{
	/* declarations for the pkey */
	EVP_PKEY *pk;
	Lisp_EVP_PKEY *lisp_pkey;
	/* output file */
	FILE *fp;
	/* password pointer */
	char *pass;


	CHECK_STRING(file);


	if ((fp = fopen((char *)XSTRING_DATA(file),"r")) == NULL)
		error ("error opening file.");

	if (NILP(password))
		pass = NULL;
	else
		pass = (char *)XSTRING_DATA(password);

	pk = PEM_read_PrivateKey(fp, NULL, 0, (char *)password);
	if (pk == NULL)
		error ("error reading PEM file.");

	fclose(fp);

	lisp_pkey = allocate_evp_pkey();
	lisp_pkey->evp_pkey = pk;
	return make_evp_pkey(lisp_pkey);
}

DEFUN("ossl-pem-write-public-key", Fossl_pem_write_public_key, 2, 2, 0, /*
Write PKEY (the public part) in a PEM structure to FILE.
									*/
      (file, pkey))
{
	/* declarations for the pkey */
	EVP_PKEY *pk;
	/* output file */
	FILE *fp;


	CHECK_STRING(file);
	CHECK_EVPPKEY(pkey);


	pk = (XEVPPKEY(pkey))->evp_pkey;

	if ((fp = fopen((char *)XSTRING_DATA(file),"w")) == NULL) {
		error ("error opening file.");
	}

	if (!PEM_write_PUBKEY(fp, pk)) {
		fclose(fp);
		error ("error writing PEM file.");
	}

	fclose(fp);

	return file;
}

DEFUN("ossl-pem-write-key", Fossl_pem_write_key, 2, 5, 0, /*
Write PKEY in a PEM structure to FILE. The key itself is
protected by (optional) CIPHER with PASSWORD.

CIPHER can be set to nil and the key will not be encrypted.
PASSWORD is ignored in this case.

Optional STYLE argument can be 'traditional or 'pkcs8
and, if omitted, defaults to 'traditional.
							  */
      (file, pkey, cipher, password, style))
{
	const EVP_CIPHER *ciph;
	/* declarations for the pkey */
	EVP_PKEY *pk;
	/* output file */
	FILE *fp;
	/* password pointer */
	char *pass;
	/* the symbols for style */
	Lisp_Object Qtraditional;
	Lisp_Object Qpkcs8;


	CHECK_STRING(file);
	CHECK_EVPPKEY(pkey);

	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (!ossl_pkey_has_private_data(pk))
		return Fossl_pem_write_public_key(file,pkey);

	CHECK_SYMBOL(cipher);
	CHECK_SYMBOL(style);

	/* initialise symbols used in style arg */
	defsymbol(&Qtraditional, "traditional");
	defsymbol(&Qpkcs8, "pkcs8");


	OpenSSL_add_all_algorithms();
	
	if (NILP(cipher)) {
		ciph = NULL;
		pass = NULL;
	} else 
		ciph = EVP_get_cipherbyname(
			(char *)XSTRING_DATA(
				(Lisp_Object)XSYMBOL(cipher)->name));
		
	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	if (NILP(password)) {
		ciph = NULL;
		pass = NULL;
	} else {
		CHECK_STRING(password);
		pass = (char *)XSTRING_DATA(password);
	}

	if ((fp = fopen((char *)XSTRING_DATA(file),"w")) == NULL) {
		EVP_cleanup();
		error ("error opening file.");
	}

	if (NILP(style) || EQ(style,Qtraditional)) {
		if (!PEM_write_PrivateKey(fp, pk, ciph, NULL, 0, 0, pass)) {
			EVP_cleanup();
			fclose(fp);
			error ("error writing PEM file.");
		}
	} else if (EQ(style,Qpkcs8)) {
		if (!PEM_write_PKCS8PrivateKey(fp, pk, ciph, NULL, 0, 0, pass)) {
			EVP_cleanup();
			fclose(fp);
			error ("error writing PEM file.");
		}
	} else {
		EVP_cleanup();
		fclose(fp);
		error ("style not supported.");
	}

	fclose(fp);
	EVP_cleanup();

	return file;
}


/*
 *
 * SSL
 *
 */
/* This is an opaque object for storing PKEYs in lisp */
Lisp_Object Qssl_connp;

Lisp_Object
make_ssl_conn(Lisp_SSL_CONN *ssl_conn)
{
	Lisp_Object lisp_ssl_conn;
	XSETSSLCONN(lisp_ssl_conn, ssl_conn);
	return lisp_ssl_conn;
}

static Lisp_Object
mark_ssl_conn(Lisp_Object obj)
{
	return Qnil;
}

static void
print_ssl_conn(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char buf[1024];
	Lisp_SSL_CONN *ssl_conn;
	SSL *conn;
	Lisp_Object parent;

	ssl_conn = XSSLCONN(obj);
	conn = ssl_conn->ssl_conn;
	parent = ssl_conn->parent;

	strcpy(buf, "#<OpenSSL socket layer");
	strcat(buf, ": ");
	if (conn == NULL) 
		strcat(buf, "dead");
	else
		strcat(buf, SSL_get_version(conn));

#ifdef HAVE_SOCKETS
	if (PROCESSP(parent) && network_connection_p(parent)) {
		Lisp_Process *p = XPROCESS(parent);
		strcat(buf, " on top of ");
		strcat(buf, (char *)XSTRING_DATA(Fcar(p->pid)));
		strcat(buf, ":");
		strcat(buf, (char *)XSTRING_DATA(Fcdr(p->pid)));
	}
#endif	/* HAVE_SOCKETS */

#ifdef HAVE_POSTGRESQL
	if (PGCONNP(parent) &&
	    PQstatus(XPGCONN(parent)->pgconn) == CONNECTION_OK) {
		PGconn *P = XPGCONN(parent)->pgconn;
		strcat(buf, " on top of ");
		strcat(buf, PQhost(P));
		strcat(buf, ":");
		strcat(buf, PQport(P));
		strcat(buf, " ");
		strcat(buf, PQdb(P));
		strcat(buf, "/");
		strcat(buf, PQuser(P));
	}
#endif	/* HAVE_POSTGRESQL */
	strcat(buf, ">");

	if (print_readably)
		error("printing unreadable object %s", buf);
	else
		write_c_string(buf, printcharfun);
}

Lisp_SSL_CONN *
allocate_ssl_conn(void)
{
	SSL_METHOD *meth=NULL;
	SSL_CTX *ctx=NULL;
	SSL *conn=NULL;
	Lisp_SSL_CONN *ssl_conn =
		alloc_lcrecord_type(Lisp_SSL_CONN, &lrecord_ssl_conn);

	SSL_library_init();

#if !defined(OPENSSL_NO_SSL2) && !defined(OPENSSL_NO_SSL3)
	meth=SSLv23_client_method();
#elif !defined(OPENSSL_NO_SSL3)
	meth=SSLv3_client_method();
#elif !defined(OPENSSL_NO_SSL2)
	meth=SSLv2_client_method();
#endif

	ctx = SSL_CTX_new(meth);
	conn = SSL_new(ctx);

	ssl_conn->ssl_meth = meth;
	ssl_conn->ssl_ctx = ctx;
	ssl_conn->ssl_conn = conn;
	ssl_conn->ssl_bio = NULL;

	/* the network process stuff */
	ssl_conn->parent = Qnil;
	ssl_conn->infd = NULL;
	ssl_conn->outfd = NULL;

	ssl_conn->connected_p = 0;
	ssl_conn->protected_p = 0;

	return ssl_conn;
}

void
finalize_ssl_conn(void *header, int for_disksave)
{
	Lisp_SSL_CONN *ssl_conn = (Lisp_SSL_CONN *) header;

	if (!(ssl_conn->ssl_conn == NULL)) {
		if (ssl_conn->connected_p)
			SSL_shutdown(ssl_conn->ssl_conn);
		SSL_free(ssl_conn->ssl_conn);
		ssl_conn->ssl_conn = NULL;
	}
	if (!(ssl_conn->ssl_ctx == NULL)) {
		SSL_CTX_free(ssl_conn->ssl_ctx);
		ssl_conn->ssl_ctx = NULL;
	}
	if (!(ssl_conn->ssl_bio == NULL)) {
		BIO_free_all(ssl_conn->ssl_bio);
		ssl_conn->ssl_bio = NULL;
	}
	/* we leave the process alive, it's not our fault, but
	 * we nullify its pointer
	 */
	ssl_conn->parent = Qnil;
	ssl_conn->infd = NULL;
	ssl_conn->outfd = NULL;

	ssl_conn->connected_p = 0;
	ssl_conn->protected_p = 0;
}

DEFINE_LRECORD_IMPLEMENTATION("ssl_conn", ssl_conn,
			      mark_ssl_conn, print_ssl_conn, finalize_ssl_conn,
			      NULL, NULL, 0, Lisp_SSL_CONN);

static int
get_process_infd(Lisp_Process * p)
{
	Lisp_Object instr, outstr;
	get_process_streams(p, &instr, &outstr);
	assert(!NILP(instr));
	return filedesc_stream_fd(XLSTREAM(instr));
}
static int
get_process_outfd(Lisp_Process * p)
{
	Lisp_Object instr, outstr;
	get_process_streams(p, &instr, &outstr);
	assert(!NILP(outstr));
	return filedesc_stream_fd(XLSTREAM(outstr));
}

DEFUN("ossl-connect-ssl", Fossl_connect_ssl, 1, 1, 0, /*
						       */
      (process))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CTX *ctx=NULL;
	SSL_METHOD *meth=NULL;
	/* network stream stuff */
	int *infd, *outfd;
	/* aux */
	int connected, err;
	/* the result */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_PROCESS(process);

	/* Make sure the process is really alive.  */
	if (!EQ(XPROCESS(process)->status_symbol, Qrun))
		error("network stream %s not alive",
		      XSTRING_DATA(XPROCESS(process)->name));
	/* Make sure the process is a network stream. */
	if (!network_connection_p(process))
		error("process %s is not a network stream",
		      XSTRING_DATA(XPROCESS(process)->name));

	/* start preparing the conn object */
	SSL_library_init();
	SSL_load_error_strings();

	meth=SSLv23_method();

	ctx=SSL_CTX_new(meth);
	if (ctx == NULL)
		error ("SSL context initialisation failed");

	/* now initialise a new connection context */
	conn=SSL_new(ctx);
	if (conn == NULL)
		error ("SSL connection initialisation failed");

/* 	printf("SSL V: %d\n",
 * 	       SSL_version(conn));
 */

	/* re-organise the fds */
	infd = (int *)get_process_infd(XPROCESS(process));
	outfd = (int *)get_process_outfd(XPROCESS(process));

	/* now allocate this stuff, pump it and return */
	lisp_ssl_conn = allocate_ssl_conn();
	lisp_ssl_conn->ssl_meth = meth;
	lisp_ssl_conn->ssl_ctx = ctx;
	lisp_ssl_conn->ssl_conn = conn;
	lisp_ssl_conn->parent = process;
	lisp_ssl_conn->infd = infd;
	lisp_ssl_conn->outfd = outfd;

	/* somehow we gotta link the network-process with the ss-layer
	 * otherwise it'd be easy to open a network stream then
	 * a ss-layer on top of it and then via `delete-process'
	 * all the work is void while the ss-layer still exists
	 */


	/* now finally, perform a test connection */
	/* give it a file descriptor to use */
	SSL_set_rfd(conn,(int)infd);
	SSL_set_wfd(conn,(int)outfd);

	/* perform handshake */
	connected = SSL_connect(conn);

	if (connected > 0) {
		lisp_ssl_conn->connected_p = 1;
		return make_ssl_conn(lisp_ssl_conn);
	}

	if (connected == 0) {
		finalize_ssl_conn(lisp_ssl_conn, 0);
		error ("handshake failed");
	}

	err = SSL_get_error(conn, connected);
	switch (err) {
	case SSL_ERROR_SSL:
		finalize_ssl_conn(lisp_ssl_conn, 0);
		error("setting up connection failed: severe SSL error.");
		break;
	case SSL_ERROR_WANT_READ:
		finalize_ssl_conn(lisp_ssl_conn, 0);
		error("setting up connection failed: WANT_READ.");
		break;
	case SSL_ERROR_WANT_WRITE:
		finalize_ssl_conn(lisp_ssl_conn, 0);
		error("setting up connection failed: WANT_WRITE.");
		break;
	default:
		finalize_ssl_conn(lisp_ssl_conn, 0);
		error("setting up connection failed.");
		break;
	}
	/* never reached */
	return Qnil;
}

DEFUN("ossl-process-send-string", Fossl_process_send_string, 2, 2, 0, /*
								       */
      (ssl_conn,string))
{
	/* network stream stuff */
	SSL *conn;

	CHECK_SSLCONN(ssl_conn);

	conn = XSSLCONN(ssl_conn)->ssl_conn;

	SSL_write(conn, XSTRING_DATA(string), XSTRING_LENGTH(string));

	return Qt;
}

DEFUN("ossl-finish-ssl", Fossl_finish_ssl, 1, 1, 0, /*
						     */
      (ssl_conn))
{
	CHECK_SSLCONN(ssl_conn);

	finalize_ssl_conn(XSSLCONN(ssl_conn), 0);
	return ssl_conn;
}

DEFUN("ossl-get-certificate", Fossl_get_certificate, 1, 1, 0, /*
							       */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	STACK_OF(X509) *cert;
	/* network stream stuff */
	Lisp_SSL_CONN *lisp_ssl_conn;
	/* aux */
	int i;
	/* the result */
	Lisp_Object certs;
	Lisp_Object issuers;


	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);


	conn = lisp_ssl_conn->ssl_conn;
	cert = SSL_get_peer_cert_chain(conn);

	certs = Qnil;
	issuers = Qnil;
	if (!(cert == NULL)) {
		for (i=sk_X509_num(cert); i>0; i--) {
			X509_NAME *sub =
				X509_get_subject_name(sk_X509_value(cert,i-1));
			const Lisp_Object certline =
				build_string(
					X509_NAME_oneline(sub,NULL,0));
			certs = Fcons(certline,certs);

			X509_NAME *iss =
				X509_get_issuer_name(sk_X509_value(cert,i-1));
			const Lisp_Object issline =
				build_string(
					X509_NAME_oneline(iss,NULL,0));
			issuers = Fcons(issline, issuers);
		}
	}

	return Fcons(certs, issuers);
}

DEFUN("ossl-bio-open-network-stream", Fossl_bio_open_network_stream, 2, 2, 0, /*
									       */
      (host, service))
{
	BIO *sbio;
	SSL_METHOD *meth=NULL;
	SSL_CTX *ctx;
	SSL *conn;
	/* the result */
	Lisp_SSL_CONN *lisp_ssl_conn;


	CHECK_STRING(host);
	CHECK_STRING(service);


	OpenSSL_add_all_algorithms();

	/* We would seed the PRNG here if the platform didn't
	 * do it automatically
	 */

#if !defined(OPENSSL_NO_SSL2) && !defined(OPENSSL_NO_SSL3)
	meth = SSLv23_client_method();
#elif !defined(OPENSSL_NO_SSL3)
	meth = SSLv3_client_method();
#elif !defined(OPENSSL_NO_SSL2)
	meth = SSLv2_client_method();
#endif

	ctx = SSL_CTX_new(meth);

	/* We'd normally set some stuff like the verify paths and
	 * mode here because as things stand this will connect to
	 * any server whose certificate is signed by any CA.
	 */

	sbio = BIO_new_ssl_connect(ctx);

	BIO_get_ssl(sbio, &conn);

	if(!conn)
		error ("can't locate SSL pointer");

	/* Don't want any retries */
	SSL_set_mode(conn, SSL_MODE_AUTO_RETRY);

	/* We might want to do other things with ssl here */

	BIO_set_conn_hostname(sbio, XSTRING_DATA(host));
	BIO_set_conn_port(sbio, XSTRING_DATA(service));

	if(BIO_do_connect(sbio) <= 0)
		error ("error connecting to server");

	if(BIO_do_handshake(sbio) <= 0)
		error ("error establishing SSL connection");


	/* now allocate this stuff, pump it and return */
	lisp_ssl_conn = allocate_ssl_conn();
	lisp_ssl_conn->ssl_meth = meth;
	lisp_ssl_conn->ssl_ctx = ctx;
	lisp_ssl_conn->ssl_conn = conn;
	lisp_ssl_conn->parent = Qnil;
	lisp_ssl_conn->infd = NULL;
	lisp_ssl_conn->outfd = NULL;
	lisp_ssl_conn->ssl_bio = sbio;

	return make_ssl_conn(lisp_ssl_conn);
}

DEFUN("ossl-bio-send", Fossl_send, 2, 2, 0, /*
					     */
      (ssl_conn, string))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CTX *ctx=NULL;
	SSL_METHOD *meth = NULL;
	/* in case of bio */
	BIO *sbio = NULL;
	/* the connection */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);

	meth = lisp_ssl_conn->ssl_meth;
	ctx = lisp_ssl_conn->ssl_ctx;
	conn = lisp_ssl_conn->ssl_conn;
	sbio = lisp_ssl_conn->ssl_bio;

	if (sbio == NULL) {
		error ("operation is allowed on bio-ssl objects only (atm)");
	} else {
		BIO_puts(sbio, (char *)XSTRING_DATA(string));
		BIO_flush(sbio);
	}

	return Qt;
}

DEFUN("ossl-bio-receive", Fossl_receive, 1, 1, 0, /*
					 */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CTX *ctx=NULL;
	SSL_METHOD *meth = NULL;
	/* aux */
	int len;
	/* some tmp buffer */
	char tmpbuf[1024];
	/* in case of bio */
	BIO *sbio = NULL;
	/* the connection */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);

	meth = lisp_ssl_conn->ssl_meth;
	ctx = lisp_ssl_conn->ssl_ctx;
	conn = lisp_ssl_conn->ssl_conn;
	sbio = lisp_ssl_conn->ssl_bio;

	if (sbio == NULL) {
		error ("operation is allowed on bio-ssl objects only (atm)");
	} else {
		len = BIO_read(sbio, tmpbuf, 1024);
	}

	if (len <= 0)
		return Qnil;
	else
		return make_ext_string(tmpbuf, len, OSSL_CODING);
}




/*
 *
 * Initialisation stuff
 *
 */
void syms_of_openssl(void)
{
	INIT_LRECORD_IMPLEMENTATION(evp_pkey);
	INIT_LRECORD_IMPLEMENTATION(ssl_conn);

	defsymbol(&Qopenssl, "openssl");
	defsymbol(&Qevp_pkeyp, "ossl-pkey-p");

	DEFSUBR(Fossl_version);
	DEFSUBR(Fossl_available_digests);
	DEFSUBR(Fossl_available_ciphers);

	DEFSUBR(Fossl_rand_bytes);

	DEFSUBR(Fossl_digest);

	DEFSUBR(Fossl_hmac);

	DEFSUBR(Fossl_bytes_to_key);
	DEFSUBR(Fossl_encrypt);
	DEFSUBR(Fossl_decrypt);

	/* general pkey */
	DEFSUBR(Fossl_pkey_p);
	DEFSUBR(Fossl_pkey_size);
	DEFSUBR(Fossl_pkey_get_public);

#ifndef OPENSSL_NO_RSA
	/* RSA */
	DEFSUBR(Fossl_rsa_generate_key);
	DEFSUBR(Fossl_rsa_pkey_p);
	DEFSUBR(Fossl_rsa_subkey_p);
#endif /* OPENSSL_NO_RSA */
#ifndef OPENSSL_NO_DSA
	/* DSA */
	DEFSUBR(Fossl_dsa_generate_key);
	DEFSUBR(Fossl_dsa_pkey_p);
	DEFSUBR(Fossl_dsa_subkey_p);
#endif /* OPENSSL_NO_DSA */
#ifndef OPENSSL_NO_EC
	/* EC */
	DEFSUBR(Fossl_ec_generate_key);
	DEFSUBR(Fossl_ec_pkey_p);
#endif /* OPENSSL_NO_EC */
#ifndef OPENSSL_NO_DH
	/* DH */
	/* DEFSUBR(Fossl_ec_generate_key); */
	DEFSUBR(Fossl_dh_pkey_p);
#endif
	DEFSUBR(Fossl_seal);
	DEFSUBR(Fossl_open);

	DEFSUBR(Fossl_sign);
	DEFSUBR(Fossl_verify);

/* PEM */
	DEFSUBR(Fossl_pem_read_public_key);
	DEFSUBR(Fossl_pem_read_key);
	DEFSUBR(Fossl_pem_write_public_key);
	DEFSUBR(Fossl_pem_write_key);

/* SSL */
#ifdef HAVE_SOCKETS
	DEFSUBR(Fossl_connect_ssl);
	DEFSUBR(Fossl_process_send_string);
	DEFSUBR(Fossl_finish_ssl);
	DEFSUBR(Fossl_get_certificate);
	DEFSUBR(Fossl_bio_open_network_stream);
	DEFSUBR(Fossl_send);
	DEFSUBR(Fossl_receive);
#endif
}

void vars_of_openssl(void)
{
	Fprovide(Qopenssl);

#ifndef OPENSSL_NO_RSA
	Fprovide(intern("openssl-rsa"));
#endif
#ifndef OPENSSL_NO_DSA
	Fprovide(intern("openssl-dsa"));
#endif
#ifndef OPENSSL_NO_EC
	Fprovide(intern("openssl-ec"));
#endif
#ifndef OPENSSL_NO_DH
	Fprovide(intern("openssl-dh"));
#endif
#ifdef HAVE_SOCKETS
#  if !defined(OPENSSL_NO_SSL2) || !defined(OPENSSL_NO_SSL3)
	Fprovide(intern("openssl-ssl"));
#  endif
#endif
}
