/*
  openssl.c -- Emacs Lisp binding to OpenSSL ciphers and digests
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

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
 *  - all of openssl (pseudo) random number generators (rand)
 *  - all of openssl symmetric block and stream cipher algorithms (cipher)
 *  - basic functionality of openssl asymmetric crypto-systems (pkey)
 *  - all of openssl envelope handling (hybrid)
 *  - all of EVP interface functionality minus `engine' support
 *  - all of PEM interface functionality
 *  - a simple SSL client
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
 *  ossl-digest-bits - effective length of the digest in bits
 *  ossl-cipher-bits - effective length of the key in bits
 *
 * - RAND
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
 *  ossl-connect - constructor for SSL connection objects
 *  ossl-finish - destructor of SSL connection objects
 *  ossl-pending - predicate if data is available for read
 *  ossl-read - 
 *  ossl-write -
 *  ossl-x509-get-subject
 *  ossl-x509-get-issuer
 *  ossl-x509-get-pubkey
 *  ossl-sslcipher-version
 *  ossl-sslcipher-name
 *  ossl-sslcipher-bits
 *
 * 
 * * Todo (internally):
 *  - implement the usage of engines
 *  - implement X.509 stuff
 *  - make TLS/SSL version selectable by user instead of #ifdef'fing it
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
 *  - RAND:
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
 *    (setq p (open-network-stream "tmp" "tmp" "www.redhat.com" "443"))
 *    (setq m (ossl-connect p))
 *    (ossl-x509-get-subject m)
 *    (ossl-x509-get-issuer m)
 *    (ossl-x509-get-pubkey m)
 *    (ossl-cipher-get-version m)
 *    (ossl-cipher-get-name m)
 *    (ossl-finish m)
 *
 */

#include <config.h>

#include "lisp.h"

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"
#include "opaque.h"

#ifdef HAVE_SOCKETS
#include "events/events.h"
#include "process.h"
#include "procimpl.h"
#endif

#include <errno.h>

#include "openssl.h"

#ifdef FILE_CODING
#include "mule/file-coding.h"
#endif

#ifdef HAVE_POSTGRESQL
#include "database/postgresql.h"
#endif

#define OSSL_CODING Qbinary

#define OSSL_STRING_LENGTH XSTRING_CHAR_LENGTH

static Lisp_Object Qopenssl;

#define __OSSL_DEBUG__(args...)		fprintf(stderr, "OSSL " args)
#ifndef OSSL_DEBUG_FLAG
#define OSSL_DEBUG(args...)
#else
#define OSSL_DEBUG(args...)		__OSSL_DEBUG__(args)
#endif
#define OSSL_DEBUG_CTX(args...)		OSSL_DEBUG("[connection]: " args)
#define OSSL_CRITICAL(args...)		__OSSL_DEBUG__("CRITICAL: " args)


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
int ec_curve_by_name(char *name);
#endif

int dh_pkey_p(EVP_PKEY *pkey);
#ifndef OPENSSL_NO_DH
int dh_pkey_has_public_data(DH *dh_key);
int dh_pkey_has_private_data(DH *dh_key);
DH *dh_get_public(EVP_PKEY *pk);
#endif

#ifdef OSSL_DEBUG_FLAG
static long ossl_bio_dump_callback(BIO*, int, const char*, int, long, long);
#endif
static int ossl_ssl_proselytise_process(Lisp_Object, Lisp_Object);
static int ossl_ssl_unproselytise_process(Lisp_Object, Lisp_Object);
int ossl_ssl_inject_ca(Lisp_Object, Lisp_Object);
int ossl_ssl_inject_ca_file(Lisp_Object, Lisp_Object);
int ossl_ssl_inject_ca_path(Lisp_Object, Lisp_Object);
int ossl_ssl_inject_cert(Lisp_Object, Lisp_Object, Lisp_Object);
int ossl_ssl_inject_cert_file(Lisp_Object, Lisp_Object, Lisp_Object);

Lisp_Object Qssl2, Qssl23, Qssl3, Qtls1;

extern Lisp_Object Qfile_readable_p;
extern Lisp_Object Qfile_writable_p;

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

	digests = Qnil;

	OpenSSL_add_all_digests();

	/*  is there a better way to get the size of the nid list? */
	for (nid = 10000; nid >= 0; --nid) {
		const EVP_MD *digest = EVP_get_digestbynid(nid);
		if (digest) {
			digests = Fcons(intern(OBJ_nid2sn(nid)), digests);
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
			ciphers = Fcons(intern(OBJ_nid2sn(nid)), ciphers);
		}
	}

	EVP_cleanup();

	return ciphers;
}


#define ossl_digest_fun(var, fun)					\
{									\
	int __kl;							\
	const EVP_MD *__md;						\
									\
	OpenSSL_add_all_digests();					\
									\
	__md = EVP_get_digestbyname(					\
		(char *)string_data(XSYMBOL(var)->name));		\
									\
	if (!__md) {							\
		EVP_cleanup();						\
		return -1;						\
	} 								\
									\
	__kl = fun(__md);						\
									\
	EVP_cleanup();							\
									\
	return __kl;							\
} while (0);

static int
ossl_digest_size(Lisp_Object digest)
{
	ossl_digest_fun(digest, EVP_MD_size);
}

static int
ossl_digest_block_size(Lisp_Object digest)
{
	ossl_digest_fun(digest, EVP_MD_block_size);
}

DEFUN("ossl-digest-size", Fossl_digest_size, 1, 1, 0, /*
Return the hash length of DIGEST in bytes.
*/
      (digest))
{
	int size = ossl_digest_size(digest);

	if (size < 0)
		error ("no such cipher");

	return make_int(size);
}

/* deprecated */
DEFUN("ossl-digest-bits", Fossl_digest_bits, 1, 1, 0, /*
Return the number of effective output bits of DIGEST.
*/
      (digest))
{
	int size = ossl_digest_size(digest);

	if (size < 0)
		error ("no such digest");

	return make_int(size*8);
}

DEFUN("ossl-digest-block-size", Fossl_digest_block_size, 1, 1, 0, /*
Return the block size of DIGEST in bytes.
*/
      (digest))
{
	int size = ossl_digest_block_size(digest);

	if (size < 0)
		error ("no such digest");

	return make_int(size);
}


#define ossl_cipher_fun(var, fun)					\
{									\
	int __kl;							\
	const EVP_CIPHER *__ciph;					\
									\
	OpenSSL_add_all_ciphers();					\
									\
	__ciph = EVP_get_cipherbyname(					\
		(char *)string_data(XSYMBOL(var)->name));		\
									\
	if (!__ciph) {							\
		EVP_cleanup();						\
		return -1;						\
	} 								\
									\
	__kl = fun(__ciph);						\
									\
	EVP_cleanup();							\
									\
	return __kl;							\
} while (0);

static int
ossl_cipher_key_length(Lisp_Object cipher)
{
	ossl_cipher_fun(cipher, EVP_CIPHER_key_length);
}

static int
ossl_cipher_iv_length(Lisp_Object cipher)
{
	ossl_cipher_fun(cipher, EVP_CIPHER_iv_length);
}

static int
ossl_cipher_block_size(Lisp_Object cipher)
{
	ossl_cipher_fun(cipher, EVP_CIPHER_block_size);
}

static int
ossl_cipher_mode(Lisp_Object cipher)
{
	ossl_cipher_fun(cipher, EVP_CIPHER_mode);
}

DEFUN("ossl-cipher-key-length", Fossl_cipher_key_length, 1, 1, 0, /*
Return the effective key length of CIPHER in bytes.
*/
      (cipher))
{
	int size = ossl_cipher_key_length(cipher);

	if (size < 0)
		error ("no such cipher");

	return make_int(size);
}

/* deprecated */
DEFUN("ossl-cipher-bits", Fossl_cipher_bits, 1, 1, 0, /*
Return the effective key size of CIPHER in bits.
*/
      (cipher))
{
	int size = ossl_cipher_key_length(cipher);

	if (size < 0)
		error ("no such cipher");

	return make_int(size*8);
}

DEFUN("ossl-cipher-iv-length", Fossl_cipher_iv_length, 1, 1, 0, /*
Return the initialisation vector length of CIPHER in bytes.
*/
      (cipher))
{
	int size = ossl_cipher_iv_length(cipher);

	if (size < 0)
		error ("no such cipher");

	return make_int(size);
}

DEFUN("ossl-cipher-block-size", Fossl_cipher_block_size, 1, 1, 0, /*
Return the block size of CIPHER in bytes.
*/
      (cipher))
{
	int size = ossl_cipher_block_size(cipher);

	if (size < 0)
		error ("no such cipher");

	return make_int(size);
}

DEFUN("ossl-cipher-mode", Fossl_cipher_mode, 1, 1, 0, /*
Return the operation mode of CIPHER.
*/
      (cipher))
{
	Lisp_Object result = Qnil;
	int mode = ossl_cipher_mode(cipher);

	if (mode < 0)
		error ("no such cipher");

	switch (mode) {
	case EVP_CIPH_STREAM_CIPHER:
		result = intern("stream");
		break;
	case EVP_CIPH_ECB_MODE:
		result = intern("ecb");
		break;
	case EVP_CIPH_CBC_MODE:
		result = intern("cbc");
		break;
	case EVP_CIPH_CFB_MODE:
		result = intern("cfb");
		break;
	case EVP_CIPH_OFB_MODE:
		result = intern("ofb");
		break;
	default:
		result = intern("cbc");
		break;
	}

	return result;
}


/*
 * 
 * RAND
 * 
 */
DEFUN("ossl-rand-bytes", Fossl_rand_bytes, 1, 1, 0, /*
Return COUNT bytes of randomness.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (count))
{
	char *outbuf;
	Lisp_Object l_outbuf;
	int count_ext;

	int speccount = specpdl_depth(), res;

	CHECK_NATNUM(count);
	count_ext = (int)XINT(count);

	/* now allocate some output buffer externally */
	XMALLOC_ATOMIC_OR_ALLOCA(outbuf, count_ext, char);

	res = RAND_bytes((unsigned char*)outbuf, count_ext);
	if (!res) {
		error("RAND_bytes did not have enough seed "
		      "to perform operation");
		return Qnil;
	} else if (res < 0) {
		error("RAND_bytes failed");
		return Qnil;
	}

	l_outbuf = make_ext_string(outbuf, count_ext, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, count_ext, speccount);

	return l_outbuf;
}

DEFUN("ossl-rand-bytes-egd", Fossl_rand_bytes_egd, 1, 2, 0, /*
Return COUNT bytes of randomness from an EGD socket.
By default use the socket /var/run/egd-pool.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (count, egd))
{
	/* This function can GC */
	char *outbuf;
	Lisp_Object l_outbuf;
	int count_ext;
	int speccount = specpdl_depth(), res;
	/* gc cruft */
	struct gcpro gcpro1, gcpro2;

	GCPRO2(count, egd);

	CHECK_NATNUM(count);
	if (!NILP(egd)) {
		CHECK_STRING(egd);
		egd = Fexpand_file_name(egd, Qnil);
		if (NILP(Ffile_exists_p(egd)))
			egd = Qnil;
	}
	count_ext = XINT(count);

	/* now allocate some output buffer externally */
	XMALLOC_ATOMIC_OR_ALLOCA(outbuf, count_ext, char);

	if (!NILP(egd)) {
		res = RAND_query_egd_bytes((char*)XSTRING_DATA(egd),
					   (unsigned char*)outbuf, count_ext);
	} else {
		res = RAND_query_egd_bytes("/var/run/egd-pool",
					   (unsigned char*)outbuf, count_ext);
	}
	if (!res) {
		UNGCPRO;
		error("RAND_query_egd_bytes did not have enough seed "
		      "to perform operation");
		return Qnil;
	} else if (res < 0) {
		UNGCPRO;
		error("RAND_query_egd_bytes failed");
		return Qnil;
	}

	l_outbuf = make_ext_string(outbuf, count_ext, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, count_ext, speccount);

	UNGCPRO;
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
	EVP_MD_CTX *mdctx;
	const EVP_MD *md;
	char md_value[EVP_MAX_MD_SIZE];
	unsigned int md_len;

	CHECK_SYMBOL(digest);
	CHECK_STRING(string);

	OpenSSL_add_all_digests();
	md = EVP_get_digestbyname(
		(char *)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	mdctx = xnew(EVP_MD_CTX);
	EVP_MD_CTX_init(mdctx);
	EVP_DigestInit_ex(mdctx, md, NULL);
	EVP_DigestUpdate(mdctx,(char*)XSTRING_DATA(string),
			 XSTRING_LENGTH(string));
	EVP_DigestFinal_ex(mdctx, (unsigned char *)md_value, &md_len);
	EVP_MD_CTX_cleanup(mdctx);

	EVP_cleanup();
	xfree(mdctx);

	return make_ext_string(md_value, md_len, OSSL_CODING);
}

DEFUN("ossl-digest-file", Fossl_digest_file, 2, 2, 0,	/*
Return the message digest of the contents of FILE computed by DIGEST.
DIGEST may be one of the OpenSSL digests you have compiled.
See `ossl-available-digests'.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (digest, file))
{
	EVP_MD_CTX *mdctx;
	const EVP_MD *md;
	unsigned char md_value[EVP_MAX_MD_SIZE];
	unsigned int md_len, md_blocksize;
	ssize_t n;
	/* input file */
	FILE *fp;


	CHECK_SYMBOL(digest);
	CHECK_STRING(file);


	file = Fexpand_file_name(file, Qnil);

	if (((fp = fopen((char *)XSTRING_DATA(file),"rb")) == NULL) ||
	    (fseek(fp, 0, SEEK_SET)))
		return wrong_type_argument(Qfile_readable_p, file);


	OpenSSL_add_all_digests();
	md = EVP_get_digestbyname(
		(char *)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	mdctx = xnew(EVP_MD_CTX);
	EVP_MD_CTX_init(mdctx);
	md_blocksize = (unsigned int)(EVP_MD_block_size(md) / 8);

	EVP_DigestInit_ex(mdctx, md, NULL);

	/* we reuse md_value here for streaming over fp */
	do {
		n = fread(md_value, 1, EVP_MAX_MD_SIZE, fp);
		if (n < 0) {
			EVP_cleanup();
			fclose(fp);
			xfree(mdctx);
			error("file corrupted");
			return Qnil;
		}
		EVP_DigestUpdate(mdctx, md_value, n);
   	} while (n > 0);

	EVP_DigestFinal_ex(mdctx, md_value, &md_len);
	EVP_MD_CTX_cleanup(mdctx);

	EVP_cleanup();
	xfree(mdctx);
	fclose(fp);

	return make_ext_string((char *)md_value, md_len, OSSL_CODING);
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
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (digest, msg, password))
{
	const EVP_MD *md;
	HMAC_CTX *hmacctx;

	/* buffer for the ciphertext */
	unsigned char outbuf[EVP_MAX_MD_SIZE];
	unsigned int outlen;
	/* buffer for external password */
	char *password_ext;
	unsigned int password_len;
#if 0   /* why? */
	/* buffer for external message */
	char *msg_ext;
	unsigned int msg_len;
#endif

	CHECK_SYMBOL(digest);
	CHECK_STRING(msg);
	CHECK_STRING(password);

	OpenSSL_add_all_digests();
	md = EVP_get_digestbyname((char*)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, password,
			    C_STRING_ALLOCA, password_ext, OSSL_CODING);
	password_len = OSSL_STRING_LENGTH(password);

#if 0   /* i wonder why */
	TO_EXTERNAL_FORMAT (LISP_STRING, msg,
			    C_STRING_ALLOCA, msg_ext, OSSL_CODING);
	msg_len = OSSL_STRING_LENGTH(msg);
#endif

	hmacctx = xnew(HMAC_CTX);
	HMAC_CTX_init(hmacctx);
	HMAC_Init(hmacctx, password_ext, password_len, md);
	HMAC_Update(hmacctx, (unsigned char*)XSTRING_DATA(msg),
		    XSTRING_LENGTH(msg));
	HMAC_Final(hmacctx, outbuf, &outlen);
	HMAC_CTX_cleanup(hmacctx);
	xfree(hmacctx);

	EVP_cleanup();

	return make_ext_string((char*)outbuf, outlen, OSSL_CODING);
}

DEFUN("ossl-hmac-file", Fossl_hmac_file, 3, 3, 0, /*
Return the message authentication code of the contents of FILE
using the hash function DIGEST and the key PASSWORD.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (digest, file, password))
{
	const EVP_MD *md;
	HMAC_CTX *hmacctx;

	/* buffer for the ciphertext */
	unsigned char outbuf[EVP_MAX_MD_SIZE];
	unsigned int outlen;
	ssize_t n;
	/* buffer for external password */
	char *password_ext;
	unsigned int password_len;
	/* input file */
	FILE *fp;

	CHECK_SYMBOL(digest);
	CHECK_STRING(file);
	CHECK_STRING(password);

	file = Fexpand_file_name(file, Qnil);

	if (((fp = fopen((char *)XSTRING_DATA(file),"rb")) == NULL) ||
	    (fseek(fp, 0, SEEK_SET))) {
		if (fp)
			fclose(fp);
		return wrong_type_argument(Qfile_readable_p, file);
	}


	OpenSSL_add_all_digests();
	md = EVP_get_digestbyname((char*)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, password,
			    C_STRING_ALLOCA, password_ext, OSSL_CODING);
	password_len = OSSL_STRING_LENGTH(password);

	hmacctx = xnew(HMAC_CTX);
	HMAC_CTX_init(hmacctx);
	HMAC_Init(hmacctx, password_ext, password_len, md);

	/* we reuse md_value here for streaming over fp */
	do {
		n = fread(outbuf, 1, EVP_MAX_MD_SIZE, fp);
		if (n < 0) {
			EVP_cleanup();
			fclose(fp);
			xfree(hmacctx);
			error("file corrupted");
			return Qnil;
		}
		HMAC_Update(hmacctx, outbuf, n);
   	} while (n > 0);

	HMAC_Final(hmacctx, outbuf, &outlen);
	HMAC_CTX_cleanup(hmacctx);
	xfree(hmacctx);

	EVP_cleanup();
	fclose(fp);

	return make_ext_string((char*)outbuf, outlen, OSSL_CODING);
}


/* 
 * 
 * SYMMETRIC CIPHER
 * 
 */
DEFUN("ossl-bytes-to-key", Fossl_bytes_to_key, 5, 5, 0, /*
Derive a key and initialisation vector (iv) suitable for a cipher.
Return a string KEY being the key. The initialisation vector is
put into KEY's property list as 'iv.

CIPHER \(a symbol\) is the cipher to derive the key and IV for.
Valid ciphers can be obtained by `ossl-available-ciphers'.

DIGEST \(a symbol\) is the message digest to use.
Valid digests can be obtained by `ossl-available-digests'.

SALT \(string or `nil'\) is used as a salt in the derivation.
Use `nil' here to indicate that no salt is used.

PASSWORD is an arbitrary string which is processed to derive a
unique key and IV.

COUNT \(a positive integer\) is the iteration count to use. This
indicates how often the hash algorithm is called recursively.

Note: You probably want to put a wrapping encoder function 
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (cipher, digest, salt, password, count))
{
	const EVP_MD *md;
	const EVP_CIPHER *ciph;
	const char *salt_ext;

	char *password_ext;
	unsigned int password_len;

	char key[EVP_MAX_KEY_LENGTH];
	char iv[EVP_MAX_IV_LENGTH];

	Lisp_Object result;

	CHECK_STRING(password);
	CHECK_SYMBOL(cipher);
	CHECK_SYMBOL(digest);
	CHECK_NATNUM(count);


	if (!XINT(count))
		error ("count has to be a non-zero positive integer");

	OpenSSL_add_all_algorithms();
	md = EVP_get_digestbyname(
		(char *)string_data(XSYMBOL(digest)->name));
	ciph = EVP_get_cipherbyname(
		(char *)string_data(XSYMBOL(cipher)->name));

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
		salt_ext = NULL;
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, password,
			    C_STRING_ALLOCA, password_ext, OSSL_CODING);
	password_len = OSSL_STRING_LENGTH(password);

	EVP_BytesToKey(ciph, md, (const unsigned char *)salt_ext,
		       (const unsigned char *)password_ext, password_len,
		       XINT(count),
		       (unsigned char *)key,
		       (unsigned char *)iv);

	EVP_cleanup();

	result = make_ext_string(key, EVP_CIPHER_key_length(ciph), OSSL_CODING);
	Fput(result, intern("iv"),
	     make_ext_string(iv, EVP_CIPHER_iv_length(ciph), OSSL_CODING));

	return result;
}


DEFUN("ossl-encrypt", Fossl_encrypt, 3, 4, 0,	/*
Return the cipher of STRING computed by CIPHER under KEY.

CIPHER \(a symbol\) may be one of the OpenSSL cipher algorithms
you have compiled. See `ossl-available-ciphers'.

STRING is the text to be encrypted.

KEY should be a key generated suitably for this cipher, for example 
by `ossl-bytes-to-key'.

Optional fourth argument IV should be an initialisation vector
suitable for this cipher. Normally the initialisation vector from
KEY's property list is used. However, if IV is
non-nil, use this IV instead.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (cipher, string, key, iv))
{
	/* buffer for the external string */
	char *string_ext;
	unsigned int string_len;
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
	EVP_CIPHER_CTX *ciphctx;

	int tmplen;
	int speccount = specpdl_depth();
	Charcount alloclen;

	/* frob the IV from the plist of key maybe */
	if (NILP(iv))
		iv = Fget(key, intern("iv"), Qnil);

	CHECK_SYMBOL(cipher);
	CHECK_STRING(string);
	CHECK_STRING(key);
	CHECK_STRING(iv);

	TO_EXTERNAL_FORMAT(LISP_STRING, string,
			   C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	if (string_len <= 0)
		error ("string must be of non-zero positive length.");

	OpenSSL_add_all_algorithms();
	/* ENGINE_load_builtin_engines(); */
	/* atm, no support for different engines */
	ciph = EVP_get_cipherbyname(
		(char *)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	/* now allocate some output buffer externally
	 * this one has to be at least EVP_CIPHER_block_size bigger
	 * since block algorithms merely operate blockwise
	 */
	alloclen = XSTRING_LENGTH(string) + EVP_CIPHER_block_size(ciph);
	XMALLOC_ATOMIC_OR_ALLOCA(outbuf, alloclen, char);

	TO_EXTERNAL_FORMAT(LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT(LISP_STRING, iv,
			   C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xnew(EVP_CIPHER_CTX);
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_EncryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		xfree(ciphctx);
		error ("error in EncryptInit");
	}
	if (!EVP_EncryptUpdate(ciphctx,
			       (unsigned char *)outbuf, &outlen,
			       (unsigned char *)string_ext, string_len)) {
		EVP_cleanup();
		xfree(ciphctx);
		error ("error in EncryptUpdate");
	}
	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_EncryptFinal(ciphctx,
			      (unsigned char *)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		xfree(ciphctx);
		error ("error in EncryptFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	l_outbuf = make_ext_string(outbuf, outlen, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	xfree(ciphctx);

	return l_outbuf;
}

DEFUN("ossl-encrypt-file", Fossl_encrypt_file, 3, 5, 0,	/*
Return the encrypted contents of FILE computed by CIPHER under KEY.

CIPHER \(a symbol\) may be one of the OpenSSL cipher algorithms
you have compiled. See `ossl-available-ciphers'.

FILE is the file to be encrypted.

Third argument KEY should be a key generated suitably for this
cipher, for example by `ossl-bytes-to-key'.

Optional fourth argument IV should be an initialisation vector
suitable for this cipher. Normally the initialisation vector from
KEY's property list is used. However, if IV is
non-nil, use this IV instead.

Optional fifth argument OUTFILE may specify a file to have the
encrypted data redirected.

Note: You probably want to put a wrapping encoder function
\(like `base16-encode-string'\) around it, since this returns
binary string data.
*/
      (cipher, file, key, iv, outfile))
{
	/* buffer for the external string */
	unsigned char string_in[1024];
	ssize_t string_len;
	unsigned int block_len;
	unsigned long file_size;
	/* buffer for the ciphertext */
	unsigned char *outbuf;
	unsigned char *obp;
	int outlen;
	Lisp_Object l_outbuf;
	/* buffer for key */
	char *key_ext;
	/* buffer for iv */
	char *iv_ext;

	/* input file */
	FILE *fp;
	/* output file */
	FILE *of;

	/* declarations for the cipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX *ciphctx;

	int tmplen;
	int speccount = specpdl_depth();
	Charcount alloclen;

	/* frob the IV from the plist of key maybe */
	if (NILP(iv))
		iv = Fget(key, intern("iv"), Qnil);

	CHECK_SYMBOL(cipher);
	CHECK_STRING(file);
	CHECK_STRING(key);
	CHECK_STRING(iv);

	if (!NILP(outfile)) {
		CHECK_STRING(outfile);
		outfile = Fexpand_file_name(outfile, Qnil);
		if ((of = fopen((char *)XSTRING_DATA(outfile),"wb")) == NULL)
			return wrong_type_argument(Qfile_writable_p, outfile);
	} else {
		of = NULL;
	}

	file = Fexpand_file_name(file, Qnil);
	if (((fp = fopen((char *)XSTRING_DATA(file),"rb")) == NULL) ||
	    (fseek(fp, 0, SEEK_SET))) {
		if (fp)
			fclose(fp);
		if (of)
			fclose(of);
		return wrong_type_argument(Qfile_readable_p, file);
	}

	fseek(fp, 0, SEEK_END);
	file_size = ftell(fp); 
	fseek(fp, 0, SEEK_SET);


	OpenSSL_add_all_algorithms();
	/* ENGINE_load_builtin_engines(); */
	/* atm, no support for different engines */
	ciph = EVP_get_cipherbyname(
		(char *)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		fclose(fp);
		if (of)
			fclose(of);
		error ("no such cipher");
	}

	/* now allocate some output buffer externally
	 * this one has to be at least EVP_CIPHER_block_size bigger
	 * since block algorithms merely operate blockwise
	 */
	block_len = EVP_CIPHER_block_size(ciph);
	if (UNLIKELY(of != NULL)) {
		alloclen = 2048;
	} else {
		alloclen = file_size + block_len;
	}
	XMALLOC_ATOMIC_OR_ALLOCA(outbuf, alloclen, unsigned char);

	TO_EXTERNAL_FORMAT(LISP_STRING, key,
			   C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT(LISP_STRING, iv,
			   C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xnew(EVP_CIPHER_CTX);
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_EncryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		fclose(fp);
		if (of)
			fclose(of);
		xfree(ciphctx);
		error("error in EncryptInit");
	}

	obp = outbuf;
	outlen = 0;
	do {
		string_len = fread(string_in, 1, 1024, fp);
		if (string_len < 0) {
			EVP_cleanup();
			fclose(fp);
			if (of)
				fclose(of);
			xfree(ciphctx);
			error("file corrupted");
			return Qnil;
		}

		tmplen = 0;
		if (string_len > 0 &&
		    !EVP_EncryptUpdate(ciphctx,
				       obp, &tmplen,
				       string_in, string_len)) {
			EVP_cleanup();
			fclose(fp);
			if (of)
				fclose(of);
			xfree(ciphctx);
			error("error in EncryptUpdate");
		}

		if (of)
			fwrite(obp, 1, tmplen, of);
		else
			obp += tmplen;

		outlen += tmplen;
	} while (string_len > 0);

	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_EncryptFinal(ciphctx, obp, &tmplen)) {
		EVP_cleanup();
		fclose(fp);
		if (of)
			fclose(of);
		xfree(ciphctx);
		error("error in EncryptFinal");
	}

	if (of)
		fwrite(obp, 1, tmplen, of);

	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	if (UNLIKELY(of != NULL)) {
		l_outbuf = outfile;
	} else {
		l_outbuf = make_ext_string((char*)outbuf, outlen, OSSL_CODING);
	}
	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	xfree(ciphctx);
	fclose(fp);
	if (of)
		fclose(of);

	return l_outbuf;
}
/* testcase:
 (setq k (ossl-bytes-to-key 'AES-256-OFB 'SHA1 nil "password" 1))
 (ossl-encrypt-file 'AES-256-OFB "~/.gnus" k nil "/tmp/gnus-enc")
 (ossl-decrypt-file 'AES-256-OFB "/tmp/gnus-enc" k nil "/tmp/gnus-dec")
*/


DEFUN("ossl-decrypt", Fossl_decrypt, 3, 4, 0,	/*
Return the deciphered version of STRING computed by CIPHER under KEY.

CIPHER \(a symbol\) may be one of the OpenSSL cipher algorithms
you have compiled. See `ossl-available-ciphers'.

STRING is the text to be decrypted.

KEY should be a key generated suitably for this
cipher, for example by `ossl-bytes-to-key'.

Optional fourth argument IV should be an initialisation vector
suitable for this cipher. Normally the initialisation vector from
KEY's property list is used. However, if IV is
non-nil, use this IV instead.
*/
      (cipher, string, key, iv))
{
	/* buffer for the external string */
	char *string_ext;
	unsigned int string_len;
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
	EVP_CIPHER_CTX *ciphctx;

	int tmplen;
	int speccount = specpdl_depth();
	Charcount alloclen;

	/* frob the IV from the plist of key maybe */
	if (NILP(iv))
		iv = Fget(key, intern("iv"), Qnil);

	CHECK_SYMBOL(cipher);
	CHECK_STRING(string);
	CHECK_STRING(key);
	CHECK_STRING(iv);

	TO_EXTERNAL_FORMAT(LISP_STRING, string,
			   C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	if (!string_len) 
		error ("string must be of non-zero positive length.");

	OpenSSL_add_all_algorithms();
	/* ENGINE_load_builtin_engines(); */
	/* atm, no support for different engines */
	ciph = EVP_get_cipherbyname(
		(char *)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	/* now allocate some output buffer externally */
	alloclen = XSTRING_LENGTH(string);
	XMALLOC_ATOMIC_OR_ALLOCA(outbuf, alloclen, char);

	TO_EXTERNAL_FORMAT (LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT (LISP_STRING, iv,
			    C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xnew(EVP_CIPHER_CTX);
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_DecryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		xfree(ciphctx);
		error ("error in DecryptInit");
	}
	if (!EVP_DecryptUpdate(ciphctx,
			       (unsigned char *)outbuf, &outlen,
			       (unsigned char *)string_ext,string_len)) {
		EVP_cleanup();
		xfree(ciphctx);
		error ("error in DecryptUpdate");
	}
	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_DecryptFinal(ciphctx,
			      (unsigned char *)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		xfree(ciphctx);
		error ("error in DecryptFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	l_outbuf = make_ext_string(outbuf, outlen, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	xfree(ciphctx);

	return l_outbuf;
}

DEFUN("ossl-decrypt-file", Fossl_decrypt_file, 3, 5, 0,	/*
Return the deciphered version of FILE computed by CIPHER under KEY.

CIPHER \(a symbol\) may be one of the OpenSSL cipher algorithms
you have compiled. See `ossl-available-ciphers'.

FILE is the file to be decrypted.

Third argument KEY should be a key generated suitably for this
cipher, for example by `ossl-bytes-to-key'.

Optional fourth argument IV should be an initialisation vector
suitable for this cipher. Normally the initialisation vector from
KEY's property list is used. However, if IV is
non-nil, use this IV instead.

Optional fifth argument OUTFILE may specify a file to have the
encrypted data redirected.
*/
      (cipher, file, key, iv, outfile))
{
	/* buffer for the external string */
	unsigned char string_in[1024];
	ssize_t string_len;
	unsigned int block_len;
	unsigned long file_size;
	/* buffer for the deciphered text */
	unsigned char *outbuf;
	unsigned char *obp;
	int outlen;
	Lisp_Object l_outbuf;
	/* buffer for key */
	char *key_ext;
	/* buffer for iv */
	char *iv_ext;

	/* input file */
	FILE *fp;
	/* output file */
	FILE *of;

	/* declarations for the decipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX *ciphctx;

	int tmplen;
	int speccount = specpdl_depth();
	Charcount alloclen;

	/* frob the IV from the plist of key maybe */
	if (NILP(iv))
		iv = Fget(key, intern("iv"), Qnil);

	CHECK_SYMBOL(cipher);
	CHECK_STRING(file);
	CHECK_STRING(key);
	CHECK_STRING(iv);

	if (!NILP(outfile)) {
		CHECK_STRING(outfile);
		outfile = Fexpand_file_name(outfile, Qnil);
		if ((of = fopen((char *)XSTRING_DATA(outfile),"wb")) == NULL)
			return wrong_type_argument(Qfile_writable_p, outfile);
	} else {
		of = NULL;
	}

	file = Fexpand_file_name(file, Qnil);
	if (((fp = fopen((char *)XSTRING_DATA(file),"rb")) == NULL) ||
	    (fseek(fp, 0, SEEK_SET))) {
		if (fp)
			fclose(fp);
		if (of)
			fclose(of);
		return wrong_type_argument(Qfile_readable_p, file);
	}

	fseek(fp, 0, SEEK_END);
	file_size = ftell(fp); 
	fseek(fp, 0, SEEK_SET);


	OpenSSL_add_all_algorithms();
	/* ENGINE_load_builtin_engines(); */
	/* atm, no support for different engines */
	ciph = EVP_get_cipherbyname(
		(char *)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		fclose(fp);
		if (of)
			fclose(of);
		error ("no such cipher");
	}

	/* now allocate some output buffer externally */
	block_len = EVP_CIPHER_block_size(ciph);
	if (UNLIKELY(of != NULL)) {
		alloclen = 2048;
	} else {
		alloclen = file_size + block_len;
	}
	XMALLOC_ATOMIC_OR_ALLOCA(outbuf, alloclen, unsigned char);

	TO_EXTERNAL_FORMAT (LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT (LISP_STRING, iv,
			    C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xnew(EVP_CIPHER_CTX);
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_DecryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		fclose(fp);
		if (of)
			fclose(of);
		xfree(ciphctx);
		error ("error in DecryptInit");
	}

	obp = outbuf;
	outlen = 0;
	do {
		string_len = fread(string_in, 1, 1024, fp);
		if (string_len < 0) {
			EVP_cleanup();
			fclose(fp);
			if (of)
				fclose(of);
			xfree(ciphctx);
			error("file corrupted");
			return Qnil;
		}

		tmplen = 0;
		if (string_len > 0 &&
		    !EVP_DecryptUpdate(ciphctx,
				       obp, &tmplen,
				       string_in, string_len)) {
			EVP_cleanup();
			fclose(fp);
			if (of)
				fclose(of);
			xfree(ciphctx);
			error ("error in DecryptUpdate");
		}

		if (of)
			fwrite(obp, 1, tmplen, of);
		else
			obp += tmplen;

		outlen += tmplen;
	} while (string_len > 0);

	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_DecryptFinal(ciphctx, obp, &tmplen)) {
		EVP_cleanup();
		fclose(fp);
		if (of)
			fclose(of);
		xfree(ciphctx);
		error ("error in DecryptFinal");
	}

	if (of)
		fwrite(obp, 1, tmplen, of);

	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	if (UNLIKELY(of != NULL)) {
		l_outbuf = outfile;
	} else {
		l_outbuf = make_ext_string((char*)outbuf, outlen, OSSL_CODING);
	}
	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	xfree(ciphctx);
	fclose(fp);
	if (of)
		fclose(of);

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
mark_evp_pkey(Lisp_Object obj)
{
	/* avoid some warning */
	if (obj);
	return Qnil;
}

static void
print_evp_pkey(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char buf[256];
	EVP_PKEY *pkey;
	X509 *x509;

	pkey = XEVPPKEY(obj)->evp_pkey;
	x509 = XEVPPKEY(obj)->x509;

	write_c_string("#<OpenSSL", printcharfun);

	if (x509) {
		X509_NAME *iss = X509_get_issuer_name(x509);
		X509_NAME *sub = X509_get_subject_name(x509);
		write_c_string(" X509 Certificate", printcharfun); 
		write_c_string(" iss:", printcharfun);
		write_c_string(X509_NAME_oneline(sub, NULL, 0), printcharfun);
		write_c_string(" sub:", printcharfun);
		write_c_string(X509_NAME_oneline(iss, NULL, 0), printcharfun);
	}

	if (pkey) {
		if (x509)
			write_c_string(";", printcharfun);

		if (rsa_pkey_p(pkey))
			write_c_string(" RSA", printcharfun); 
		else if (dsa_pkey_p(pkey))
			write_c_string(" DSA", printcharfun); 
		else if (ec_pkey_p(pkey))
			write_c_string(" EC", printcharfun); 

		if (ossl_pkey_has_private_data(pkey))
			write_c_string(" private/public key", printcharfun); 
		else if (ossl_pkey_has_public_data(pkey))
			write_c_string(" public key", printcharfun); 
		else
			write_c_string(" empty key", printcharfun); 

		if (EVP_PKEY_size(pkey) > 0) {
			snprintf(buf, 256, ", size %d", EVP_PKEY_size(pkey)*8);
			write_c_string(buf, printcharfun); 
		}
	}

	write_c_string(">", printcharfun); 

	/* avoid some warning */
	if (escapeflag);
}

static Lisp_EVP_PKEY *
allocate_evp_pkey(void)
{
	Lisp_EVP_PKEY *evp_pkey =
		alloc_lcrecord_type(Lisp_EVP_PKEY, &lrecord_evp_pkey);
	evp_pkey->evp_pkey = NULL;
	evp_pkey->x509 = NULL;
	return evp_pkey;
}

static void
finalise_evp_pkey(void *header, int for_disksave)
{
	Lisp_EVP_PKEY *evp_pkey = (Lisp_EVP_PKEY *) header;

	if (evp_pkey->evp_pkey) {
		EVP_PKEY_free(evp_pkey->evp_pkey);
		evp_pkey->evp_pkey = NULL;
	}
	if (evp_pkey->x509) {
		X509_free(evp_pkey->x509);
		evp_pkey->x509 = NULL;
	}

	/* avoid some warning */
	if (for_disksave);
}

DEFINE_LRECORD_IMPLEMENTATION("evp_pkey", evp_pkey,
			      mark_evp_pkey, print_evp_pkey,
			      finalise_evp_pkey,
			      NULL, NULL, 0,
			      Lisp_EVP_PKEY);

static Lisp_Object
make_evp_pkey(EVP_PKEY *pkey, X509 *x509)
{
	Lisp_EVP_PKEY *lisp_pkey = allocate_evp_pkey();

	lisp_pkey->evp_pkey = pkey;
	lisp_pkey->x509 = x509;

	return wrap_evppkey(lisp_pkey);
}

static Lisp_Object
make_evp_pkey_pk(EVP_PKEY *pkey)
{
	return make_evp_pkey(pkey, NULL);
}

static Lisp_Object
make_evp_pkey_x509(X509 *x509)
{
	return make_evp_pkey(X509_get_pubkey(x509), x509);
}

DEFUN("ossl-pkey-p", Fossl_pkey_p, 1, 1, 0, /*
Return t iff OBJECT is a pkey, nil otherwise.
*/
      (object))
{
	if (EVPPKEYP(object))
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
		return ec_pkey_has_public_data((pkey->pkey).ec);
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
		return ec_pkey_has_private_data((pkey->pkey).ec);
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

DEFUN("ossl-pkey-private-p", Fossl_pkey_private_p, 1, 1, 0, /*
Return non-nil if PKEY contains private data.

This function is not native OpenSSL.
*/
      (pkey))
{
	EVP_PKEY *pk;

	if (!(EVPPKEYP(pkey)))
		return Qnil;

	pk = (XEVPPKEY(pkey))->evp_pkey;

	if (ossl_pkey_has_private_data(pk))
		return Qt;

	return Qnil;
}

DEFUN("ossl-pkey-get-public", Fossl_pkey_get_public, 1, 1, 0, /*
Return a copy of PKEY stripped by the private data.

This function is not native OpenSSL.
*/
      (pkey))
{
	EVP_PKEY *pk;
	EVP_PKEY *pkout;

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

	return make_evp_pkey_pk(pkout);
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
Return an RSA public key with of length BITS and exponent EXPO.
*/
      (bits, expo))
{
	EVP_PKEY *pkey;
	RSA *rsakey;

	CHECK_NATNUM(bits);
	CHECK_NATNUM(expo);


	if (!XINT(bits))
		error ("modulus size must be a non-zero positive integer");
	if (!(XINT(expo) % 2))
		error ("exponent must be an odd positive integer");

	pkey = EVP_PKEY_new();
	rsakey = RSA_generate_key(XINT(bits), XINT(expo), NULL, NULL);
	EVP_PKEY_assign_RSA(pkey, rsakey);

	return make_evp_pkey_pk(pkey);
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
	char *seed_ext;
	int seed_len;
	int counter_ret;
	unsigned_long h_ret;


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
					 &counter_ret, &h_ret,
					 NULL, NULL);
	if (!DSA_generate_key(dsakey))
		error ("error during generation of DSA key");

	EVP_PKEY_assign_DSA(pkey, dsakey);

	return make_evp_pkey_pk(pkey);
}

DEFUN("ossl-dsa-pkey-p", Fossl_dsa_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of DSA type.
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
	return (!(EC_KEY_get0_group(ec_key) == NULL) &&
		!(EC_KEY_get0_public_key(ec_key) == NULL));
}
int
ec_pkey_has_private_data(EC_KEY *ec_key)
{
	return (ec_pkey_has_public_data(ec_key) &&
		!(EC_KEY_get0_private_key(ec_key) == NULL));
}

DEFUN("ossl-ec-available-curves", Fossl_ec_available_curves, 0, 0, 0, /*
Return a list of builtin elliptic curves.
*/
      ())
{
	EC_builtin_curve *curves = NULL;
	size_t crv_len = 0, n = 0;
	Lisp_Object lcurves;

	lcurves = Qnil;

	crv_len = EC_get_builtin_curves(NULL, 0);
	curves = OPENSSL_malloc(sizeof(EC_builtin_curve) * crv_len);

	if (curves == NULL)
		error ("no curves defined");

	if (!EC_get_builtin_curves(curves, crv_len)) {
		OPENSSL_free(curves);
		error ("error during initialisation of curves");
	}

	for (n = 0; n < crv_len; n++) {
		int nid = curves[n].nid;
		lcurves = Fcons(intern(OBJ_nid2sn(nid)), lcurves);
	}

	OPENSSL_free(curves);

	return lcurves;
}

int
ec_curve_by_name(char *name)
{
	return OBJ_sn2nid(name);
}

DEFUN("ossl-ec-generate-key", Fossl_ec_generate_key, 1, 1, 0, /*
Return a EC public key on CURVE.
CURVE may be any symbol from `ossl-ec-available-curves'.

At the moment we do not support creating custom curves.
*/
      (curve))
{
	EVP_PKEY *pkey;
	EC_KEY *eckey = EC_KEY_new();

	CHECK_SYMBOL(curve);

	pkey = EVP_PKEY_new();
	eckey = EC_KEY_new_by_curve_name(
		ec_curve_by_name((char *)string_data(XSYMBOL(curve)->name)));

	if ((eckey == NULL)) {
		error ("no such curve");
	}

	if (!EC_KEY_generate_key(eckey))
		error ("error during generation of EC key");

	EVP_PKEY_assign_EC_KEY(pkey, eckey);

	return make_evp_pkey_pk(pkey);
}

DEFUN("ossl-ec-pkey-p", Fossl_ec_pkey_p, 1, 1, 0, /*
Return t iff PKEY is of EC type.
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

	key = EC_KEY_dup((pk->pkey).ec);

	/* now kill the private data */
	EC_KEY_set_private_key(key, NULL);

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
with the hybrid technique.

That is, create a random key/iv pair for the symmetric encryption with 
CIPHER and encrypt that key/iv asymmetrically with the provided public
key.

The envelope returned is a list 
\(encrypted_string encrypted_key encrypted_iv\)
where
`encrypted_string' is the (symmetrically) encrypted message
`encrypted_key' is the (asymmetrically) encrypted random key
`encrypted_iv' is the (asymmetrically) encrypted random iv

Note: You probably want to put a wrapping encoder function
(like `base16-encode-string') around it, since this function
returns binary string data.
*/
      (cipher, string, pkey))
{
	/* declarations for the cipher */
	const EVP_CIPHER *ciph;
	EVP_CIPHER_CTX ciphctx;
	/* declarations for the pkey */
	EVP_PKEY *pk[1];
	int npubk;
	unsigned char *ekey;
	int ekey_len;
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
	if (!ossl_pkey_has_public_data(pk[0])) {
		error ("cannot seal, key has no public key data");
	}
	npubk = 1;

	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	OpenSSL_add_all_algorithms();
	ciph = EVP_get_cipherbyname((char*)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
		return Qnil;
	}

	/* alloc ekey buffer */
	ekey = (unsigned char*)xmalloc_atomic(EVP_PKEY_size(pk[0]));

	/* now allocate some output buffer externally
	 * this one has to be at least EVP_CIPHER_block_size bigger
	 * since block algorithms merely operate blockwise
	 */
	outbuf = (unsigned char *)xmalloc_atomic(XSTRING_LENGTH(string) +
						 EVP_CIPHER_block_size(ciph));

	EVP_CIPHER_CTX_init(&ciphctx);
	if (!(EVP_SealInit(&ciphctx, ciph,
			   &ekey, &ekey_len,
			   (unsigned char *)&iv,
			   (EVP_PKEY **)&pk, npubk)==npubk)) {
		EVP_cleanup();
		xfree(outbuf);
		xfree(ekey);
		error ("error in SealInit");
		return Qnil;
	}
	if (!EVP_SealUpdate(&ciphctx, outbuf, (int *)&outlen,
			    (unsigned char*)string_ext, string_len)) {
		EVP_cleanup();
		xfree(outbuf);
		xfree(ekey);
		error ("error in SealUpdate");
		return Qnil;
	}
	if (!EVP_SealFinal(&ciphctx, (unsigned char*)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		xfree(outbuf);
		xfree(ekey);
		error ("error in SealFinal");
		return Qnil;
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(&ciphctx);

	l_outbuf = make_ext_string((char *)outbuf, outlen, OSSL_CODING);
	l_ekey = make_ext_string((char *)ekey, ekey_len, OSSL_CODING);
	l_iv = make_ext_string(iv,EVP_CIPHER_iv_length(ciph), OSSL_CODING);
	xfree(outbuf);
	xfree(ekey);
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
	ciph = EVP_get_cipherbyname((char*)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
		return Qnil;
	}

	if (NILP(eiv)) {
		eiv_ext = NULL;
	} else {
		CHECK_STRING(eiv);
		TO_EXTERNAL_FORMAT (LISP_STRING, eiv,
				    C_STRING_ALLOCA, eiv_ext, OSSL_CODING);
	}

	/* now allocate some output buffer externally */
	outbuf = (unsigned char *)xmalloc_atomic(XSTRING_LENGTH(string));

	EVP_CIPHER_CTX_init(&ciphctx);
	if (!EVP_OpenInit(&ciphctx, ciph,
			  (unsigned char*)ekey_ext,
			  (unsigned int)ekey_len,
			  (unsigned char*)eiv_ext, pk)) {
		EVP_cleanup();
		xfree(outbuf);
		error ("error in OpenInit");
		return Qnil;
	}
	if (!EVP_OpenUpdate(&ciphctx, outbuf, (int *)&outlen,
			    (unsigned char*)string_ext,
			    (unsigned int)string_len)) {
		EVP_cleanup();
		xfree(outbuf);
		error ("error in OpenUpdate");
		return Qnil;
	}
	if (!EVP_OpenFinal(&ciphctx, outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		xfree(outbuf);
		error ("error in OpenFinal");
		return Qnil;
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(&ciphctx);

	l_outbuf = make_ext_string((char *)outbuf, outlen, OSSL_CODING);
	xfree(outbuf);

	EVP_cleanup();

	return l_outbuf;
}


DEFUN("ossl-sign", Fossl_sign, 3, 3, 0, /*
Return a signature obtained by signing STRING under DIGEST with PKEY.

That is, hash the message STRING with the message digest DIGEST and
encrypt the result with the private key PKEY.

Note: Due to some relationship between the public key system and the
message digest you cannot use every digest algorithm with every 
private key type.
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
	if (!ossl_pkey_has_private_data(pk)) {
		error ("cannot sign, key has no private key data");
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, string,
			    C_STRING_ALLOCA, string_ext, OSSL_CODING);
	string_len = OSSL_STRING_LENGTH(string);

	OpenSSL_add_all_algorithms();
	md = EVP_get_digestbyname((char*)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
		return Qnil;
	}

	/* now allocate some output buffer externally */
	outbuf = (unsigned char *)xmalloc_atomic(EVP_PKEY_size(pk));

	EVP_MD_CTX_init(&mdctx);
	if (!(EVP_SignInit(&mdctx, md))) {
		EVP_cleanup();
		xfree(outbuf);
		error ("error in SignInit");
		return Qnil;
	}
	if (!EVP_SignUpdate(&mdctx, string_ext, string_len)) {
		EVP_cleanup();
		xfree(outbuf);
		error ("error in SignUpdate");
		return Qnil;
	}
	if (!EVP_SignFinal(&mdctx, outbuf, &outlen, pk)) {
		EVP_cleanup();
		xfree(outbuf);
		error ("error in SignFinal");
		return Qnil;
	}
	EVP_MD_CTX_cleanup(&mdctx);

	l_outbuf = make_ext_string((char *)outbuf, outlen, OSSL_CODING);
	xfree(outbuf);

	EVP_cleanup();

	return l_outbuf;
}

DEFUN("ossl-verify", Fossl_verify, 4, 4, 0, /*
Return t iff SIG is a valid signature of STRING under DIGEST obtained by PKEY.

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
	md = EVP_get_digestbyname((char*)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
		return Qnil;
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
		return Qnil;
	}
	if (!EVP_VerifyUpdate(&mdctx, string_ext, string_len)) {
		EVP_cleanup();
		error ("error in VerifyUpdate");
		return Qnil;
	}
	result = EVP_VerifyFinal(&mdctx, (unsigned char*)sig_ext, sig_len, pk);
	if (result == -1) {
		EVP_cleanup();
		error ("error in VerifyFinal");
		return Qnil;
	}
	EVP_MD_CTX_cleanup(&mdctx);

	EVP_cleanup();

	return result ? Qt : Qnil;
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
	X509 *pk509;

	/* output file */
	FILE *fp;

	CHECK_STRING(file);

	file = Fexpand_file_name(file, Qnil);

	if ((fp = fopen((char *)XSTRING_DATA(file), "r")) == NULL)
		error ("error opening file.");

	pk509 = PEM_read_X509(fp, NULL, NULL, NULL);
	pk = PEM_read_PUBKEY(fp, NULL, NULL, NULL);

	fclose(fp);

	return make_evp_pkey(pk, pk509);
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
	/* output file */
	FILE *fp;
	/* password pointer */
	char *pass;

	CHECK_STRING(file);

	file = Fexpand_file_name(file, Qnil);

	if ((fp = fopen((char *)XSTRING_DATA(file), "r")) == NULL)
		error ("error opening file.");

	if (NILP(password)) {
		pass = NULL;
	} else {
		CHECK_STRING(password);
		pass = (char *)XSTRING_DATA(password);
	}

	pk = PEM_read_PrivateKey(fp, NULL, NULL, pass);
	fclose(fp);
	if (pk == NULL) {
		/* now maybe it is a public key only */
		return Fossl_pem_read_public_key(file);
	}

	return make_evp_pkey_pk(pk);
}

DEFUN("ossl-pem-write-public-key", Fossl_pem_write_public_key, 2, 2, 0, /*
Write PKEY (the public part) in a PEM structure to FILE.
*/
      (file, pkey))
{
	/* declarations for the pkey */
	EVP_PKEY *pk;
	X509 *pk509;
	/* output file */
	FILE *fp;

	CHECK_STRING(file);
	CHECK_EVPPKEY(pkey);

	file = Fexpand_file_name(file, Qnil);

	pk = XEVPPKEY(pkey)->evp_pkey;
	pk509 = XEVPPKEY(pkey)->x509;

	if ((fp = fopen((char *)XSTRING_DATA(file), "w")) == NULL)
		error ("error opening file.");

	if (!PEM_write_PUBKEY(fp, pk)) {
		fclose(fp);
		error ("error writing PEM file.");
	}

	fclose(fp);

	return file;
}

DEFUN("ossl-pem-write-key", Fossl_pem_write_key, 2, 4, 0, /*
Write PKEY in a PEM structure to FILE. The key itself is
protected by (optional) CIPHER with PASSWORD.

CIPHER can be set to nil and the key will not be encrypted.
PASSWORD is ignored in this case.
*/
      (file, pkey, cipher, password))
{
	const EVP_CIPHER *ciph;
	/* declarations for the pkey */
	EVP_PKEY *pk;
	X509 *pk509;
	/* output file */
	FILE *fp;
	/* password pointer */
	char *pass;

	CHECK_STRING(file);
	CHECK_EVPPKEY(pkey);

	file = Fexpand_file_name(file, Qnil);

	pk = XEVPPKEY(pkey)->evp_pkey;
	pk509 = XEVPPKEY(pkey)->x509;

	if (!ossl_pkey_has_private_data(pk))
		return Fossl_pem_write_public_key(file, pkey);

	CHECK_SYMBOL(cipher);

	OpenSSL_add_all_algorithms();
	
	if (NILP(cipher)) {
		ciph = NULL;
		pass = NULL;
	} else {
		ciph = EVP_get_cipherbyname(
			(char *)string_data(XSYMBOL(cipher)->name));
		if (!ciph) {
			EVP_cleanup();
			error ("no such cipher");
		}
	}

	if (NILP(password)) {
		ciph = NULL;
		pass = NULL;
	} else {
		CHECK_STRING(password);
		pass = (char *)XSTRING_DATA(password);
	}

	if ((fp = fopen((char *)XSTRING_DATA(file), "w")) == NULL) {
		EVP_cleanup();
		error ("error opening file.");
	}

	if (!PEM_write_PKCS8PrivateKey(fp, pk, ciph, NULL, 0, NULL, pass)) {
		EVP_cleanup();
		fclose(fp);
		error ("error writing PEM file.");
	}

	EVP_cleanup();
	fclose(fp);

	return file;
}

static long
ossl_pem_pkey_cb(BIO *bio, int cmd, const char *argp,
		 int argi, long argl, long ret)
{
	Lisp_Object key;
	void *foo = BIO_get_callback_arg(bio);

	if (!(key = (Lisp_Object)foo)) {
		return ret;
	}

	if (BIO_CB_RETURN & cmd) {
		return ret;
	}

	switch (cmd) {
	case BIO_CB_WRITE:
		key = concat2(key, make_ext_string(argp, argi, OSSL_CODING));
		BIO_set_callback_arg(bio, (void*)key);
		break;
	default:
		return ret;
	}
	return ret;
}

DEFUN("ossl-pem-public-key",Fossl_pem_public_key, 1, 1, 0, /*
Return PKEY as PEM encoded string.
*/
      (pkey))
{
	/* This function can GC */
	/* declarations for the pkey */
	EVP_PKEY *pk;
	Lisp_Object result;
	/* bio stuff */
        BIO *b;
	/* gc stuff */
	struct gcpro gcpro1;

	GCPRO1(pkey);

	CHECK_EVPPKEY(pkey);

	pk = (XEVPPKEY(pkey))->evp_pkey;

	if (!(b = BIO_new(BIO_s_null()))) {
		UNGCPRO;
		error("cannot open memory buffer");
                return Qnil;
	}

	result = build_string("");
	BIO_set_callback(b, ossl_pem_pkey_cb);
	BIO_set_callback_arg(b, (void*)result);

	if (!PEM_write_bio_PUBKEY(b, pk)) {
		EVP_cleanup();
		BIO_free(b);
		UNGCPRO;
		error ("error creating PEM string");
		return Qnil;
	}

	{
		void *foo = BIO_get_callback_arg(b);
		if (!(result = (Lisp_Object)foo)) {
			result = Qnil;
		}
	}

	BIO_free(b);

	UNGCPRO;
	return result;
}

DEFUN("ossl-pem-key",Fossl_pem_key, 1, 3, 0, /*
Return PKEY as PEM encoded string.   The key itself is
protected by (optional) CIPHER with PASSWORD.

CIPHER can be set to nil and the key will not be encrypted.
PASSWORD is ignored in this case.
*/
      (pkey, cipher, password))
{
	/* This function can GC */
	/* declarations for the pkey */
	EVP_PKEY *pk;
	Lisp_Object result;
	const EVP_CIPHER *ciph;
	char *pass;
	/* bio stuff */
        BIO *b;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(pkey, cipher, password);

	CHECK_EVPPKEY(pkey);

	pk = (XEVPPKEY(pkey))->evp_pkey;

	if (!ossl_pkey_has_private_data(pk)) {
		UNGCPRO;
		return Fossl_pem_public_key(pkey);
	}

	CHECK_SYMBOL(cipher);

	OpenSSL_add_all_algorithms();
	
	if (NILP(cipher)) {
		ciph = NULL;
		pass = NULL;
	} else {
		ciph = EVP_get_cipherbyname(
			(char *)string_data(XSYMBOL(cipher)->name));
		if (!ciph) {
			EVP_cleanup();
			UNGCPRO;
			error ("no such cipher");
			return Qnil;
		}
	}

	if (NILP(password)) {
		ciph = NULL;
		pass = NULL;
	} else {
		CHECK_STRING(password);
		pass = (char *)XSTRING_DATA(password);
	}

	if (!(b = BIO_new(BIO_s_null()))) {
		UNGCPRO;
		error("cannot open memory buffer");
                return Qnil;
	}

	result = build_string("");
	BIO_set_callback(b, ossl_pem_pkey_cb);
	BIO_set_callback_arg(b, (void*)result);

	if (!PEM_write_bio_PKCS8PrivateKey(b, pk, ciph, NULL, 0, NULL, pass)) {
		EVP_cleanup();
		BIO_free(b);
		UNGCPRO;
		error ("error creating PEM string");
		return Qnil;
	}

	{
		void *foo = BIO_get_callback_arg(b);

		if (!(result = (Lisp_Object)foo)) {
			result = Qnil;
		}
	}

	BIO_free(b);

	UNGCPRO;
	return result;
}


/*
 *
 * SSL
 * The SSL support in this API is sorta high level since having
 * server hellos, handshakes and stuff like that is not what you want
 * to do in elisp.
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
	mark_object(XSSLCONN(obj)->parent);
	mark_object(XSSLCONN(obj)->pipe_instream);
	mark_object(XSSLCONN(obj)->pipe_outstream);
#ifdef FILE_CODING
	mark_object(XSSLCONN(obj)->coding_instream);
	mark_object(XSSLCONN(obj)->coding_outstream);
#endif

	return Qnil;
}

static void
print_ssl_conn(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	SSL *conn;
	Lisp_Object parent;

	conn = XSSLCONN(obj)->ssl_conn;
	parent = XSSLCONN(obj)->parent;

	write_c_string("#<OpenSSL socket layer: ", printcharfun);
	if (conn == NULL) 
		write_c_string("dead", printcharfun);
	else
		write_c_string(SSL_get_version(conn), printcharfun);

#ifdef HAVE_SOCKETS
	if (PROCESSP(parent)) {
		write_c_string(" on top of ", printcharfun);
		print_internal(parent, printcharfun, escapeflag);
	}
#endif	/* HAVE_SOCKETS */

#ifdef HAVE_POSTGRESQL
	if (PGCONNP(parent) &&
	    PQstatus(XPGCONN(parent)->pgconn) == CONNECTION_OK) {
		write_c_string(" on top of ", printcharfun);
		print_internal(parent, printcharfun, escapeflag);
	}
#endif	/* HAVE_POSTGRESQL */
	write_c_string(">", printcharfun);
}

Lisp_SSL_CONN *
allocate_ssl_conn(void)
{
	Lisp_SSL_CONN *ssl_conn =
		alloc_lcrecord_type(Lisp_SSL_CONN, &lrecord_ssl_conn);

	/* the network process stuff */
	ssl_conn->parent = Qnil;
	ssl_conn->infd = -1;
	ssl_conn->outfd = -1;

	ssl_conn->connected_p = 0;
	ssl_conn->protected_p = 0;

	ssl_conn->pipe_instream = Qnil;
	ssl_conn->pipe_outstream = Qnil;
#if FILE_CODING
	ssl_conn->coding_instream = Qnil;
	ssl_conn->coding_outstream = Qnil;
#endif

	return ssl_conn;
}

static void
finalise_ssl_conn(void *header, int for_disksave)
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
	ssl_conn->ssl_bio = NULL;

	if (PROCESSP(ssl_conn->parent)) {
		XPROCESS(ssl_conn->parent)->process_type = PROCESS_TYPE_NETWORK;
		XPROCESS(ssl_conn->parent)->process_type_data = Qnil;
	}
	/* we leave the process alive, it's not our fault, but
	 * we nullify its pointer
	 */
	ssl_conn->parent = Qnil;
	ssl_conn->infd = -1;
	ssl_conn->outfd = -1;

	ssl_conn->connected_p = 0;
	ssl_conn->protected_p = 0;

	/* free the lstream resources */
#if 0				/* will lead to problems */
	if (LSTREAMP(ssl_conn->pipe_instream))
		Lstream_delete(XLSTREAM(ssl_conn->pipe_instream));
	if (LSTREAMP(ssl_conn->pipe_outstream))
		Lstream_delete(XLSTREAM(ssl_conn->pipe_outstream));
#endif
	ssl_conn->pipe_instream = Qnil;
	ssl_conn->pipe_outstream = Qnil;
#if FILE_CODING
#if 0				/* will lead to problems */
	if (LSTREAMP(ssl_conn->coding_instream))
		Lstream_delete(XLSTREAM(ssl_conn->coding_instream));
	if (LSTREAMP(ssl_conn->coding_outstream))
		Lstream_delete(XLSTREAM(ssl_conn->coding_outstream));
#endif
	ssl_conn->coding_instream = Qnil;
	ssl_conn->coding_outstream = Qnil;
#endif

	/* avoid some warning */
	if (for_disksave);
}

DEFINE_LRECORD_IMPLEMENTATION("ssl_conn", ssl_conn,
			      mark_ssl_conn, print_ssl_conn,
			      finalise_ssl_conn,
			      NULL, NULL, 0, Lisp_SSL_CONN);

static int
ssl_conn_alive_p(Lisp_SSL_CONN *ssl_conn)
{
	return ssl_conn->connected_p;
}

static int
get_process_infd(Lisp_Process * p)
{
	Lisp_Object instr, outstr;
	get_process_streams(p, &instr, &outstr);
	return Lstream_get_fd(XLSTREAM(instr));
}
static int
get_process_outfd(Lisp_Process * p)
{
	Lisp_Object instr, outstr;
	get_process_streams(p, &instr, &outstr);
	return Lstream_get_fd(XLSTREAM(outstr));
}

static int
event_stream_ssl_create_stream_pair(
	SSL *conn,
	Lisp_Object *instream, Lisp_Object *outstream, int flags)
{
	*instream = make_ssl_input_stream(conn, flags);
	*outstream = make_ssl_output_stream(conn, flags);

	return 0;
}

static void
init_ssl_io_handles(Lisp_SSL_CONN *s, int flags)
{
	event_stream_ssl_create_stream_pair(
		s->ssl_conn, &s->pipe_instream, &s->pipe_outstream, flags);

#ifdef FILE_CODING
	s->coding_instream = make_decoding_input_stream(
		XLSTREAM(s->pipe_instream), Fget_coding_system(
			Vcoding_system_for_read));
	Lstream_set_character_mode(XLSTREAM(s->coding_instream));
	s->coding_outstream = make_encoding_output_stream(
		XLSTREAM(s->pipe_outstream), Fget_coding_system(
			Vcoding_system_for_write));
#endif /* FILE_CODING */
}

/* Advanced step-by-step initialisation */
#define OSSL_CHECK_PROCESS(process)					\
{									\
	/* Make sure the process is really alive.  */			\
	if (!EQ(XPROCESS(process)->status_symbol, Qrun))		\
		error("Network stream %s not alive",			\
		      XSTRING_DATA(XPROCESS(process)->name));		\
	/* Make sure the process is a network stream. */		\
	if (!network_connection_p(process))				\
		error("Process %s is not a network stream",		\
		      XSTRING_DATA(XPROCESS(process)->name));		\
} while (0);

#ifdef OSSL_DEBUG_FLAG
static long
ossl_bio_dump_callback(BIO *bio, int cmd, const char *argp,
		  int argi, long argl, long ret)
{
	BIO *out;

	out=(BIO *)BIO_get_callback_arg(bio);
	if (out == NULL) return(ret);

	if (cmd == (BIO_CB_READ|BIO_CB_RETURN))
	{
		BIO_printf(out,"read from %p [%p] (%d bytes => %ld (0x%lX))\n",
			   (void *)bio,argp,argi,ret,ret);
		BIO_dump(out,argp,(int)ret);
		return(ret);
	}
	else if (cmd == (BIO_CB_WRITE|BIO_CB_RETURN))
	{
		BIO_printf(out,"write to %p [%p] (%d bytes => %ld (0x%lX))\n",
			   (void *)bio,argp,argi,ret,ret);
		BIO_dump(out,argp,(int)ret);
	}
	return(ret);
}
#endif

static Lisp_Object
ossl_ssl_prepare_cmeth(Lisp_Object method)
{
	SSL_METHOD *meth = NULL;
	Lisp_SSL_CONN *lisp_ssl_conn;

	/* start preparing the conn object */
	SSL_library_init();
	SSL_load_error_strings();

	if (0);
	else if (EQ(method, Qssl2))
		meth = (SSL_METHOD *)SSLv2_client_method();
	else if (EQ(method, Qssl3))
		meth = (SSL_METHOD *)SSLv3_client_method();
	else if (EQ(method, Qssl23))
		meth = (SSL_METHOD *)SSLv23_client_method();
	else if (EQ(method, Qtls1))
		meth = (SSL_METHOD *)TLSv1_client_method();
	else
		meth = (SSL_METHOD *)TLSv1_client_method();

	if (!RAND_status())
		error("OSSL: not enough random data");

	/* now allocate this stuff, pump it and return */
	lisp_ssl_conn = allocate_ssl_conn();
	lisp_ssl_conn->ssl_meth = meth;
	lisp_ssl_conn->ssl_ctx = NULL;
	lisp_ssl_conn->ssl_conn = NULL;
	lisp_ssl_conn->ssl_bio = NULL;

	return make_ssl_conn(lisp_ssl_conn);
}

static Lisp_Object
ossl_ssl_prepare_smeth(Lisp_Object method)
{
	SSL_METHOD *meth = NULL;
	Lisp_SSL_CONN *lisp_ssl_conn;

	/* start preparing the conn object */
	SSL_library_init();
	SSL_load_error_strings();

	if (0);
	else if (EQ(method, Qssl2))
		meth = (SSL_METHOD *)SSLv2_server_method();
	else if (EQ(method, Qssl3))
		meth = (SSL_METHOD *)SSLv3_server_method();
	else if (EQ(method, Qssl23))
		meth = (SSL_METHOD *)SSLv23_server_method();
	else if (EQ(method, Qtls1))
		meth = (SSL_METHOD *)TLSv1_server_method();
	else
		meth = (SSL_METHOD *)SSLv23_server_method();

	if (!RAND_status())
		error("OSSL: not enough random data");

	/* now allocate this stuff, pump it and return */
	lisp_ssl_conn = allocate_ssl_conn();
	lisp_ssl_conn->ssl_meth = meth;
	lisp_ssl_conn->ssl_ctx = NULL;
	lisp_ssl_conn->ssl_conn = NULL;
	lisp_ssl_conn->ssl_bio = NULL;

	return make_ssl_conn(lisp_ssl_conn);
}

static Lisp_Object
ossl_ssl_prepare_ctx(Lisp_Object ssl_conn)
{
	/* SSL connection stuff */
	SSL_CTX *ctx = NULL;
	Lisp_SSL_CONN *lisp_ssl_conn = XSSLCONN(ssl_conn);

	ctx = SSL_CTX_new(lisp_ssl_conn->ssl_meth);
	if (ctx == NULL)
		error("OSSL: context initialisation failed");

	/* OpenSSL contains code to work-around lots of bugs and flaws in
	 * various SSL-implementations. SSL_CTX_set_options() is used to enabled
	 * those work-arounds. The man page for this option states that
	 * SSL_OP_ALL enables all the work-arounds and that "It is usually safe
	 * to use SSL_OP_ALL to enable the bug workaround options if
	 * compatibility with somewhat broken implementations is desired."
	 */
	SSL_CTX_set_options(ctx, SSL_OP_ALL);

	lisp_ssl_conn->ssl_ctx = ctx;

	return ssl_conn;
}

static Lisp_Object
ossl_ssl_prepare(Lisp_Object ssl_conn, void(*fun)(SSL*))
{
	/* SSL connection stuff */
	SSL *conn = NULL;
	BIO *bio = NULL;
#ifdef OSSL_DEBUG_FLAG
	BIO *bio_c_out = NULL;
#endif
	Lisp_SSL_CONN *lisp_ssl_conn = XSSLCONN(ssl_conn);

	/* now initialise a new connection context */
	conn = SSL_new(lisp_ssl_conn->ssl_ctx);
	if (conn == NULL || fun == NULL)
		error("OSSL: connection initialisation failed");

	/* always renegotiate */
	SSL_set_mode(conn, SSL_MODE_AUTO_RETRY);

	/* initialise the main connection BIO */
	bio = BIO_new(BIO_s_socket());

#ifdef OSSL_DEBUG_FLAG
	/* this is a debug BIO which pukes tons of stuff to stderr */
	bio_c_out = BIO_new_fp(stderr, BIO_NOCLOSE);
	BIO_set_callback(bio, ossl_bio_dump_callback);
	BIO_set_callback_arg(bio, bio_c_out);
#endif

	/* connect SSL with the bio */
	SSL_set_bio(conn, bio, bio);
	/* turn into client or server */
	fun(conn);

	/* now allocate this stuff, pump it and return */
	lisp_ssl_conn->ssl_conn = conn;
	lisp_ssl_conn->ssl_bio = bio;

	/* create lstream handles */
	init_ssl_io_handles(lisp_ssl_conn, STREAM_NETWORK_CONNECTION);

	return ssl_conn;
}

/* Injection of CA certificates */
int ossl_ssl_inject_ca(Lisp_Object ssl_conn, Lisp_Object cacert)
{
	SSL_CTX *ctx;
	EVP_PKEY *cert;
	X509 *xc509;

	ctx = XSSLCONN(ssl_conn)->ssl_ctx;
	cert = XEVPPKEY(cacert)->evp_pkey;
	xc509 = XEVPPKEY(cacert)->x509;

	if (cert && !xc509) {
		xc509 = X509_new();
		X509_set_pubkey(xc509, cert);
		XEVPPKEY(cacert)->x509 = xc509;
	} else if (xc509);
	else
		return 0;

	/* what about coding system issues? */
	if (!SSL_CTX_add_client_CA(ctx, xc509))
		return 0;
	else
		return -1;
}

int ossl_ssl_inject_ca_file(Lisp_Object ssl_conn, Lisp_Object cafile)
{
	SSL_CTX *ctx;

	ctx = XSSLCONN(ssl_conn)->ssl_ctx;

	/* what about coding system issues? */
	if (!SSL_CTX_load_verify_locations(
		    ctx, (char*)XSTRING_DATA(cafile), NULL))
		return 0;
	else
		return -1;
}

int ossl_ssl_inject_ca_path(Lisp_Object ssl_conn, Lisp_Object capath)
{
	SSL_CTX *ctx;

	ctx = XSSLCONN(ssl_conn)->ssl_ctx;

	/* what about coding system issues? */
	if (!SSL_CTX_load_verify_locations(
		    ctx, NULL, (char*)XSTRING_DATA(capath)))
		return 0;
	else
		return -1;
}

int ossl_ssl_inject_cert(Lisp_Object ssl_conn,
			 Lisp_Object cert, Lisp_Object key)
{
	SSL_CTX *ctx;
	EVP_PKEY *pkey;
	EVP_PKEY *xcert;
	X509 *xc509;

	ctx = XSSLCONN(ssl_conn)->ssl_ctx;
	pkey = XEVPPKEY(key)->evp_pkey;
	xcert = XEVPPKEY(cert)->evp_pkey;
	xc509 = XEVPPKEY(cert)->x509;

	if (xcert && !xc509) {
		xc509 = X509_new();
		X509_set_pubkey(xc509, xcert);
		XEVPPKEY(cert)->x509 = xc509;
	} else if (xc509);
	else
		return 0;

	if (SSL_CTX_use_certificate(ctx, xc509) <= 0)
		return 0;

	if (SSL_CTX_use_PrivateKey(ctx, pkey) <= 0)
		return 0;
	if (!SSL_CTX_check_private_key(ctx))
		return 0;

	return -1;
}

int ossl_ssl_inject_cert_file(Lisp_Object ssl_conn,
			      Lisp_Object cert, Lisp_Object key)
{
	SSL_CTX *ctx;

	ctx = XSSLCONN(ssl_conn)->ssl_ctx;

	if (SSL_CTX_use_certificate_file(
		    ctx, (char*)XSTRING_DATA(cert), SSL_FILETYPE_PEM) <= 0)
		return 0;
	if (SSL_CTX_use_PrivateKey_file(
		    ctx, (char*)XSTRING_DATA(key), SSL_FILETYPE_PEM) <= 0)
		return 0;
	if (!SSL_CTX_check_private_key(ctx))
		return 0;

	return -1;
}

Lisp_Object ossl_ssl_handshake(Lisp_Object ssl_conn, Lisp_Object process)
{
	/* This function can GC */
	/* SSL connection stuff */
	SSL *conn = NULL;
	BIO *bio = NULL;
#if 0 && defined(OSSL_DEBUG_FLAG)
	BIO *bio_c_out = NULL;
#endif
	int ret, err, infd, outfd;

	struct gcpro gcpro1, gcpro2;

	/* Make sure we have a process, the alive check should be done in the
	   function calling this here */
	CHECK_PROCESS(process);

	GCPRO2(ssl_conn, process);

	/* set the alternate one */
	event_stream_unselect_process(XPROCESS(process));

#ifdef HAVE_MULE
	/* just announce that we are very binary */
	Fset_process_coding_system(process, Qbinary, Qbinary);
#endif

	/* initialise the process' buffer for type-specific data,
	 * we will store process input there */
	XPROCESS(process)->process_type_data = Qnil;

	/* retrieve the sockets of the process */
	infd = get_process_infd(XPROCESS(process));
	outfd = get_process_outfd(XPROCESS(process));

	/* push data to ssl_conn */
	XSSLCONN(ssl_conn)->parent = process;
	XSSLCONN(ssl_conn)->infd = infd;
	XSSLCONN(ssl_conn)->outfd = outfd;

	/* frob vars from ssl_conn */
	conn = XSSLCONN(ssl_conn)->ssl_conn;
	bio = XSSLCONN(ssl_conn)->ssl_bio;

	/* initialise the main connection BIO */
	BIO_set_fd(bio, infd, 0);

	/* now perform the actual handshake
	 * this is a loop because of the genuine openssl concept to not handle
	 * non-blocking I/O correctly */
	for (;;) {
		struct timeval to;

		ret = SSL_do_handshake(conn);
		err = SSL_get_error(conn, ret);

		/* perform select() with timeout
		 * 1 second at the moment */
		to.tv_sec = 1;
		to.tv_usec = 0;

		if (err == SSL_ERROR_NONE) {
			break;
		} else if (err == SSL_ERROR_WANT_READ) {
			fd_set read_fds;
			OSSL_DEBUG("WANT_READ\n");

			FD_ZERO(&read_fds);
			FD_SET(infd, &read_fds);

			/* wait for socket to be readable */
			if (!(ret = select(infd+1, &read_fds, 0, NULL, &to))) {
				UNGCPRO;
				finalise_ssl_conn(XSSLCONN(ssl_conn), 0);
				error("timeout during handshake");
				return Qnil;
			}
		} else if (err == SSL_ERROR_WANT_WRITE) {
			fd_set write_fds;
			OSSL_DEBUG("WANT_WRITE\n");
			FD_ZERO(&write_fds);
			FD_SET(outfd, &write_fds);

			/* wait for socket to be writable */
			if (!(ret = select(infd+1, &write_fds, 0, NULL, &to))) {
				UNGCPRO;
				finalise_ssl_conn(XSSLCONN(ssl_conn), 0);
				error("timeout during handshake");
				return Qnil;
			}
		} else if (err == SSL_ERROR_SSL) {
			/* close down the process object */
			Fdelete_process(process);

			UNGCPRO;
			finalise_ssl_conn(XSSLCONN(ssl_conn), 0);
			error("handshake failed");
			return Qnil;
		} else {
			OSSL_CRITICAL("\nUnknown error: %d\n"
				      "Please report: "
				      "sxemacs-devel@sxemacs.org\n\n", err);

#if 0
			/* we used to check whether the connection is
			   still alive, but this was perhaps a bad idea */
			try = BIO_read(bio, buf, 2);
			if ((try == 0) ||
			    (try < 0 && !BIO_should_retry(bio))) {
				/* Handle closed connection */
				XPROCESS(process)->exit_code = 256;
				XPROCESS(process)->status_symbol = Qexit;
			}
#else
			/* close down the process object */
			Fdelete_process(process);
#endif

			UNGCPRO;
			finalise_ssl_conn(XSSLCONN(ssl_conn), 0);
			error("unknown handshake error");
			return Qnil;
		}
	}

	/* marry the socket layer now */
	ossl_ssl_proselytise_process(ssl_conn, process);

	/* declare the whole pig connected */
	XSSLCONN(ssl_conn)->connected_p = 1;

	event_stream_select_process(XPROCESS(process));

	UNGCPRO;
	return ssl_conn;
}

DEFUN("ossl-ssl-inject-cert", Fossl_ssl_inject_cert, 2, 3, 0, /*
Add CERT as the local certificate of SSL-CONN.
Optional argument KEY specifies a key file or evp-pkey, if
CERT does not contain it.

Both, CERT and KEY may be either a filename pointing to a
PEM-encoded certificate and key respectively, or may be an
evp-pkey object.
*/
      (ssl_conn, cert, key))
{
	/* This function can GC */
	int (*fun)(Lisp_Object, Lisp_Object, Lisp_Object) = NULL;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(ssl_conn, cert, key);

	CHECK_SSLCONN(ssl_conn);
	if (!NILP(cert))
		if (!STRINGP(cert))
			CHECK_EVPPKEY(cert);
	if (!NILP(key))
		if (!STRINGP(key))
			CHECK_EVPPKEY(key);

	/* certificate and key preparation */
	if (STRINGP(cert)) {
		cert = Fexpand_file_name(cert, Qnil);
		if (NILP(Ffile_readable_p(cert)))
			cert = Qnil;
	}

	if (STRINGP(key)) {
		key = Fexpand_file_name(key, Qnil);
		if (NILP(Ffile_readable_p(key)))
			key = Qnil;
	}

	if (STRINGP(cert) && NILP(key))
		key = cert;
	else if (EVPPKEYP(cert) && NILP(key))
		key = cert;

	/* certificate and key injection */
	if (!NILP(cert) && !NILP(key) &&
	    STRINGP(cert) && STRINGP(key))
		fun = ossl_ssl_inject_cert_file;
	else if (!NILP(cert) && !NILP(key) &&
		 EVPPKEYP(cert) && EVPPKEYP(key))
		fun = ossl_ssl_inject_cert;

	if (fun && fun(ssl_conn, cert, key)) {
		UNGCPRO;
		return Qt;
	} else {
		UNGCPRO;
		return Qnil;
	}
}

DEFUN("ossl-ssl-inject-ca", Fossl_ssl_inject_ca, 2, 2, 0, /*
Add CA to the pile of certificate authorities of SSL-CONN.
Also force a \(re\)verification of the remote peer certificate
against CA.  Return `t' if the injection was successful,
`nil' otherwise.

CA may be either a file name pointing to a PEM-encoded
CA certificate, or may be a directory containing a valid
bunch of CA certificates according to OpenSSL's CA path
layout, or may also be an evp-pkey object.
*/
      (ssl_conn, ca))
{
	/* This function can GC */
	int (*fun)(Lisp_Object, Lisp_Object) = NULL;
	SSL *conn = NULL;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(ssl_conn, ca);

	CHECK_SSLCONN(ssl_conn);
	if (!NILP(ca))
		if (!STRINGP(ca))
			CHECK_EVPPKEY(ca);

	if (STRINGP(ca)) {
		ca = Fexpand_file_name(ca, Qnil);
		if (NILP(Ffile_readable_p(ca)))
			ca = Qnil;
	}

	if (!NILP(ca) && STRINGP(ca)) {
		if (NILP(Ffile_directory_p(ca)))
			fun = ossl_ssl_inject_ca_file;
		else
			fun = ossl_ssl_inject_ca_path;
	} else if (!NILP(ca) && EVPPKEYP(ca))
		fun = ossl_ssl_inject_ca;

	if (fun && fun(ssl_conn, ca) &&
	    (conn = XSSLCONN(ssl_conn)->ssl_conn)) {
		ssl_verify_cert_chain(conn, SSL_get_peer_cert_chain(conn));
		UNGCPRO;
		return Qt;
	}

	UNGCPRO;
	return Qnil;
}

DEFUN("ossl-ssl-handshake", Fossl_ssl_handshake, 1, 6, 0, /*
Perform a handshake on the network connection PROCESS.

Return a ssl-conn object, or `nil' if the handshake failed.
In the latter case, most likely the remote site cannot handle
the specified method, requires a client certificate, or cannot
handle ssl at all.

Optional argument METHOD indicates the SSL connection method,
it can be one of `tls1' \(default\), `ssl23', `ssl2', or `ssl3'.

Optional argument CA indicates a CA certificate.
See `ossl-ssl-inject-ca'.

Optional arguments CERT and KEY indicate a peer certificate
and possibly a separate key file respectively.
See `ossl-ssl-inject-peer-cert'.

Optional argument SERVERP indicates whether to perform the
handshake as a server if non-nil, and as a client otherwise.
Note: In case of a handshake as server it is mandatory to provide
a valid certificate and a corresponding key.
*/
      (process, method, ca, cert, key, serverp))
{
	/* This function can GC */
	/* the result(s) */
	Lisp_Object ssl_conn, result;

	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;

	GCPRO6(process, method, ca, cert, key, serverp);

	/* Make sure the process is really alive.  */
	CHECK_PROCESS(process);
	OSSL_CHECK_PROCESS(process);

	/* create a ssl_conn object first */
	if (NILP(serverp))
		ssl_conn = ossl_ssl_prepare_cmeth(method);
	else
		ssl_conn = ossl_ssl_prepare_smeth(method);

	/* create the context */
	ossl_ssl_prepare_ctx(ssl_conn);

	/* certificate and key preparation */
	Fossl_ssl_inject_cert(ssl_conn, cert, key);
	/* certificate authority preparation */
	Fossl_ssl_inject_ca(ssl_conn, ca);

	/* prepare for handshake */
	if (NILP(serverp))
		ossl_ssl_prepare(ssl_conn, SSL_set_connect_state);
	else
		ossl_ssl_prepare(ssl_conn, SSL_set_accept_state);

	result = ossl_ssl_handshake(ssl_conn, process);

	UNGCPRO;
	return result;
}

DEFUN("ossl-ssl-connect", Fossl_ssl_connect, 0, MANY, 0, /*
Perform a TLS or SSL handshake, return a ssl-conn object on
success, or `nil' if the handshake failed.
In the latter case, most likely the remote site cannot handle
the specified method, requires a client certificate, or cannot
handle ssl at all.

:process
:method
:cafile
:capath
:key
:cert

 PROCESS.
Optional argument METHOD indicates the SSL connection method,
it can be one of `tls1' \(default\), `ssl23', `ssl2', or `ssl3'.
*/
      (int nargs, Lisp_Object *args))
{
	int i;

	for (i = 0; i < nargs; i++);

	return Qnil;
}

static void
ossl_swap_process_streams(Lisp_SSL_CONN *s, Lisp_Process *p)
{
	Lisp_Object in, out;

	in = p->pipe_instream;
	out = p->pipe_outstream;

	p->pipe_instream = s->pipe_instream;
	p->pipe_outstream = s->pipe_outstream;

	s->pipe_instream = in;
	s->pipe_outstream = out;

#ifdef FILE_CODING
	in = p->coding_instream;
	out = p->coding_outstream;

	p->coding_instream = s->coding_instream;
	p->coding_outstream = s->coding_outstream;

	s->coding_instream = in;
	s->coding_outstream = out;
#endif
}

static int
ossl_ssl_proselytise_process(Lisp_Object ssl_conn, Lisp_Object process)
{
	Lisp_Process *p = XPROCESS(process);
	Lisp_SSL_CONN *s = XSSLCONN(ssl_conn);

	event_stream_unselect_process(p);

	/* put the streams we have in the ssl-conn object into the process
	   object; actually these swap their places */
	if (p->process_type != PROCESS_TYPE_SSL)
		ossl_swap_process_streams(s, p);

	/* somehow we gotta link the network-process with the ss-layer
	 * otherwise it'd be easy to open a network stream then
	 * a ss-layer on top of it and then via `delete-process'
	 * all the work is void while the ss-layer still exists
	 */
	p->process_type = PROCESS_TYPE_SSL;
	p->process_type_data = ssl_conn;

	event_stream_select_process(p);

	return 0;
}

static int
ossl_ssl_unproselytise_process(Lisp_Object ssl_conn, Lisp_Object process)
{
	Lisp_Process *p = XPROCESS(process);
	Lisp_SSL_CONN *s = XSSLCONN(ssl_conn);

	/* put the streams we have in the ssl-conn object into the process
	   object (they should be the former process streams) */
	if (p->process_type == PROCESS_TYPE_SSL)
		ossl_swap_process_streams(s, p);

	/* somehow we gotta link the network-process with the ss-layer
	 * otherwise it'd be easy to open a network stream then
	 * a ss-layer on top of it and then via `delete-process'
	 * all the work is void while the ss-layer still exists
	 */
	XPROCESS(process)->process_type = PROCESS_TYPE_NETWORK;
	XPROCESS(process)->process_type_data = Qnil;

	return 0;
}

DEFUN("ossl-ssl-proselytise-process", Fossl_ssl_proselytise_process,
      1, 1, 0, /*
Convert the underlying process of SSL-CONN into a secure
network connection object.
*/
      (ssl_conn))
{
	Lisp_Object process;

	CHECK_SSLCONN(ssl_conn);

	process = XSSLCONN(ssl_conn)->parent;
	if (!PROCESSP(process)) {
		error("no process associated with this connection");
		return Qnil;
	}

	/* Make sure the process is really alive.  */
	OSSL_CHECK_PROCESS(process);

	ossl_ssl_proselytise_process(ssl_conn, process);

	return process;
}

DEFUN("ossl-ssl-unproselytise-process", Fossl_ssl_unproselytise_process,
      1, 1, 0, /*
Convert the underlying process of SSL-CONN into an ordinary
network connection object.
*/
      (ssl_conn))
{
	Lisp_Object process;

	CHECK_SSLCONN(ssl_conn);

	process = XSSLCONN(ssl_conn)->parent;
	if (!PROCESSP(process)) {
		error("no process associated with this connection");
		return Qnil;
	}

	/* Make sure the process is really alive.  */
	OSSL_CHECK_PROCESS(process);

	/* Castrate the process and make it a network process again */
	ossl_ssl_unproselytise_process(ssl_conn, process);

	return process;
}

DEFUN("ossl-ssl-finish", Fossl_ssl_finish, 1, 1, 0, /*
Finish an SSL connection SSL-CONN.

Note: This may also finish the network connection.
*/
      (ssl_conn))
{
	Lisp_Object process;

	CHECK_SSLCONN(ssl_conn);

	if (XSSLCONN(ssl_conn)->protected_p)
		error ("Cannot finish protected SSL connection");

	process = XSSLCONN(ssl_conn)->parent;
	if (PROCESSP(process))
		ossl_ssl_unproselytise_process(ssl_conn, process);

	finalise_ssl_conn(XSSLCONN(ssl_conn), 0);
	return ssl_conn;
}

DEFUN("ossl-ssl-read", Fossl_ssl_read, 2, 2, 0, /*
Return the cleartext of STRING which is assumed to be a complete
block of data sent through SSL-CONN.
*/
      (ssl_conn, string))
{
	/* network stream stuff */
	SSL *conn;
	Lisp_Object process;
	/* the result */
	Lisp_Object result = Qnil;

	CHECK_SSLCONN(ssl_conn);
	CHECK_STRING(string);

	if (!ssl_conn_alive_p(XSSLCONN(ssl_conn)))
		error("SSL connection dead");

	conn = XSSLCONN(ssl_conn)->ssl_conn;
	process = XSSLCONN(ssl_conn)->parent;

	/* Make sure the process is really alive.  */
	OSSL_CHECK_PROCESS(process);

	return result;
}

DEFUN("ossl-ssl-write", Fossl_ssl_write, 2, 2, 0, /*
Send STRING to the tunnel SSL-CONN.
*/
      (ssl_conn, string))
{
	/* network stream stuff */
	SSL *conn;
	Lisp_Object process, proc_filter;
	Lstream *out;
	/* aux */
	int ret;

	CHECK_SSLCONN(ssl_conn);
	CHECK_STRING(string);

	if (!ssl_conn_alive_p(XSSLCONN(ssl_conn)))
		error("SSL connection dead");

	conn = XSSLCONN(ssl_conn)->ssl_conn;
	process = XSSLCONN(ssl_conn)->parent;

	/* Make sure the process is really alive.  */
	OSSL_CHECK_PROCESS(process);

	switch (XPROCESS(process)->process_type) {
	case PROCESS_TYPE_NETWORK:
		/* ssl streams reside in ssl-conn object atm */
		out = XLSTREAM(DATA_OUTSTREAM(XSSLCONN(ssl_conn)));
		break;
	case PROCESS_TYPE_SSL:
		/* ssl streams reside in process object, snarf from there */
		out = XLSTREAM(DATA_OUTSTREAM(XPROCESS(process)));
		break;
	default:
		out = NULL;
		error("unable to write");
	}

	/* store the original process filter */
	proc_filter = XPROCESS(process)->filter;

	ret = Lstream_write(out, XSTRING_DATA(string), XSTRING_LENGTH(string));
	Lstream_flush(out);

	switch (SSL_get_error(conn, ret)) {
	case SSL_ERROR_NONE:
		break;
	case SSL_ERROR_WANT_WRITE:
		error("Connection wants write");
	case SSL_ERROR_WANT_READ:
		error("Connection wants read");
	default:
		error("Severe SSL connection error");
	}

	/* restore the original process filter */
	return (SSL_pending(conn) == 0) ? Qt : Qnil;
}

/* convenience functions */
DEFUN("ossl-ssl-parent", Fossl_ssl_parent, 1, 1, 0, /*
Return the underlying parent layer of SSL-CONN.
*/
      (ssl_conn))
{
	CHECK_SSLCONN(ssl_conn);

	return XSSLCONN(ssl_conn)->parent;
}

DEFUN("ossl-ssl-cert", Fossl_ssl_cert, 1, 1, 0, /*
Return the local peer's certificate of SSL-CONN if present,
`nil' otherwise.
*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn = NULL;
	X509 *cert = NULL;

	CHECK_SSLCONN(ssl_conn);

	conn = XSSLCONN(ssl_conn)->ssl_conn;
	cert = SSL_get_certificate(conn);

	if (cert)
		return make_evp_pkey_x509(cert);
	else
		return Qnil;
}

DEFUN("ossl-ssl-peer-cert", Fossl_ssl_peer_cert, 1, 1, 0, /*
Return the remote peer's certificate of SSL-CONN if present,
`nil' otherwise.
*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn = NULL;
	X509 *cert = NULL;

	CHECK_SSLCONN(ssl_conn);

	conn = XSSLCONN(ssl_conn)->ssl_conn;
	cert = SSL_get_peer_certificate(conn);

	if (cert)
		return make_evp_pkey_x509(cert);
	else
		return Qnil;
}

DEFUN("ossl-ssl-peer-cert-chain", Fossl_ssl_peer_cert_chain, 1, 1, 0, /*
Return the certificate chain of SSL-CONN as a list of
evp-pkey objects.
*/
      (ssl_conn))
{
	int i;
	/* SSL connection stuff */
	SSL *conn = NULL;
	STACK_OF(X509) *sk;
	/* result cruft */
	Lisp_Object result = Qnil;

	CHECK_SSLCONN(ssl_conn);

	conn = XSSLCONN(ssl_conn)->ssl_conn;
	sk = SSL_get_peer_cert_chain(conn);

	if (sk == NULL)
		return result;

	for (i=0; i<sk_X509_num(sk); i++) {
		X509 *cert = sk_X509_value(sk, i);

		result = Fcons(make_evp_pkey_x509(cert), result);
	}

	return result;
}

#if 0
DEFUN("ossl-ssl-cert-store", Fossl_ssl_cert_store, 1, 1, 0, /*
Return the X509 cert store of SSL-CONN.
*/
      (ssl_conn))
{
	X509_STORE *sto = NULL;

	return Qnil;
}
#endif

#if 0				/* just thoughts */
int	SSL_get_verify_mode(const SSL *s);
int	SSL_get_verify_depth(const SSL *s);
#endif

DEFUN("ossl-ssl-verify-certificate", Fossl_ssl_verify_certificate,
      1, 1, 0, /*
Return a verify code of SSL-CONN.

The result is a cons cell with the numeric verify code in
the car and a verbose string in the cdr.
*/
      (ssl_conn))
{
	int vrc;
	/* SSL connection stuff */
	SSL *conn = NULL;
	/* result cruft */
	Lisp_Object result = Qnil;

	CHECK_SSLCONN(ssl_conn);

	conn = XSSLCONN(ssl_conn)->ssl_conn;	
	vrc = SSL_get_verify_result(conn);

	result = Fcons(
		make_int(vrc),
		build_string(X509_verify_cert_error_string(vrc)));

	return result;
}

DEFUN("ossl-ssl-cipher-version", Fossl_ssl_cipher_version, 1, 1, 0, /*
Return the protocol version of the tunnel SSL-CONN.
*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	const SSL_CIPHER *ciph;
	/* network stream stuff */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);

	conn = lisp_ssl_conn->ssl_conn;
	if (conn == NULL)
		return Qnil;

	ciph = SSL_get_current_cipher(conn);

	if (!(ciph == NULL))
		return Fmake_symbol(
			build_string(SSL_CIPHER_get_version(ciph)));
	else
		return Qnil;
}

DEFUN("ossl-ssl-cipher-name", Fossl_ssl_cipher_name, 1, 1, 0, /*
Return the name of the current cipher used in the tunnel SSL-CONN.
*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	const SSL_CIPHER *ciph;
	/* network stream stuff */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);

	conn = lisp_ssl_conn->ssl_conn;
	if (conn == NULL)
		return Qnil;

	ciph = SSL_get_current_cipher(conn);

	if (!(ciph == NULL))
		return intern(SSL_CIPHER_get_name(ciph));
	else
		return Qnil;
}

DEFUN("ossl-ssl-cipher-names", Fossl_ssl_cipher_names, 1, 1, 0, /*
Return the names of all supported ciphers in the tunnel SSL-CONN.
*/
      (ssl_conn))
{
	int i;
	/* SSL connection stuff */
	SSL *conn=NULL;
	STACK_OF(SSL_CIPHER) *ciphs;
	Lisp_Object result = Qnil;

	CHECK_SSLCONN(ssl_conn);

	conn = XSSLCONN(ssl_conn)->ssl_conn;
	if (conn == NULL)
		return Qnil;

	ciphs = SSL_get_ciphers(conn);

	for (i=sk_SSL_CIPHER_num(ciphs)-1; i>=0; i--) {
		SSL_CIPHER *ciph = sk_SSL_CIPHER_value(ciphs, i);

		result = Fcons(intern(SSL_CIPHER_get_name(ciph)), result);
	}

	return result;
}

DEFUN("ossl-ssl-cipher-bits", Fossl_ssl_cipher_bits, 1, 1, 0, /*
Return the number of effective bits of the current cipher in SSL-CONN.
*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	const SSL_CIPHER *ciph;
	int alg_bits, strength_bits;
	/* network stream stuff */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);

	conn = lisp_ssl_conn->ssl_conn;
	if (conn == NULL)
		return Qnil;

	ciph = SSL_get_current_cipher(conn);

	if (!(ciph == NULL)) {
		strength_bits = SSL_CIPHER_get_bits(ciph, &alg_bits);
		/* what do we want to do with alg_bits? */
		return make_int(strength_bits);
	} else
		return Qnil;
}

DEFUN("ossl-ssl-cipher-description", Fossl_ssl_cipher_description, 1, 1, 0, /*
Return a description of the current cipher used in the tunnel SSL-CONN.
*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	const SSL_CIPHER *ciph;
	/* network stream stuff */
	Lisp_SSL_CONN *lisp_ssl_conn;

	CHECK_SSLCONN(ssl_conn);
	lisp_ssl_conn = XSSLCONN(ssl_conn);

	conn = lisp_ssl_conn->ssl_conn;
	if (conn == NULL)
		return Qnil;

	ciph = SSL_get_current_cipher(conn);

	if (!(ciph == NULL))
		return build_string(SSL_CIPHER_description(ciph, NULL, 0));
	else
		return Qnil;
}


/* X509 cert handling */
DEFUN("ossl-x509-subject", Fossl_x509_subject, 1, 1, 0, /*
Return the certificate subject of CERT (an evp-pkey object).

This will return a string in LDAP syntax.
*/
      (cert))
{
	X509 *pk509;

	CHECK_EVPPKEY(cert);

	pk509 = XEVPPKEY(cert)->x509;

	if (pk509) {
		X509_NAME *sub = X509_get_subject_name(pk509);
		return build_string(X509_NAME_oneline(sub, NULL, 0));
	} else
		return Qnil;
}

DEFUN("ossl-x509-issuer", Fossl_x509_issuer, 1, 1, 0, /*
Return the certificate issuer of CERT (an evp-pkey object),
that is the organisation which signed the certificate.

This will return a string in LDAP syntax.
*/
      (cert))
{
	X509 *pk509;

	CHECK_EVPPKEY(cert);

	pk509 = XEVPPKEY(cert)->x509;

	if (pk509) {
		X509_NAME *iss = X509_get_issuer_name(pk509);
		return build_string(X509_NAME_oneline(iss, NULL, 0));
	} else
		return Qnil;
}

DEFUN("ossl-x509-serial", Fossl_x509_serial, 1, 1, 0, /*
Return the certificate serial of CERT (an evp-pkey object).
*/
      (cert))
{
	X509 *pk509;

	CHECK_EVPPKEY(cert);

	pk509 = XEVPPKEY(cert)->x509;

	if (pk509) {
		ASN1_INTEGER *ser = X509_get_serialNumber(pk509);
		return make_integer(ASN1_INTEGER_get(ser));
	} else
		return Qnil;
}

DEFUN("ossl-x509-not-before", Fossl_x509_not_before, 1, 1, 0, /*
Return the certificate valid-not-before time of CERT.
*/
      (cert))
{
	X509 *pk509;

	CHECK_EVPPKEY(cert);

	pk509 = XEVPPKEY(cert)->x509;

	if (pk509) {
		ASN1_TIME *nbf = X509_get_notBefore(pk509);
		return build_string((char*)nbf->data);
	} else
		return Qnil;
}

DEFUN("ossl-x509-not-after", Fossl_x509_not_after, 1, 1, 0, /*
Return the certificate valid-not-after time of CERT.
*/
      (cert))
{
	X509 *pk509;

	CHECK_EVPPKEY(cert);

	pk509 = XEVPPKEY(cert)->x509;

	if (pk509) {
		ASN1_TIME *nbf = X509_get_notAfter(pk509);
		return build_string((char*)nbf->data);
	} else
		return Qnil;
}

DEFUN("ossl-x509-signature-type", Fossl_x509_signature_type, 1, 1, 0, /*
Return the signature type of CERT.
*/
      (cert))
{
	X509 *pk509;

	CHECK_EVPPKEY(cert);

	pk509 = XEVPPKEY(cert)->x509;

	if (pk509) {
		int ty = X509_get_signature_type(pk509);
		Lisp_Object result = Qnil;

		switch (ty) {
		case EVP_PKEY_NONE:
			result = intern("none");
			break;
#ifndef OPENSSL_NO_RSA
		case EVP_PKEY_RSA:
			result = intern("rsa");
			break;
		case EVP_PKEY_RSA2:
			result = intern("rsa2");
			break;
#endif
#ifndef OPENSSL_NO_DSA
		case EVP_PKEY_DSA:
			result = intern("dsa");
			break;
		case EVP_PKEY_DSA1:
			result = intern("dsa1");
			break;
		case EVP_PKEY_DSA2:
			result = intern("dsa2");
			break;
		case EVP_PKEY_DSA3:
			result = intern("dsa3");
			break;
		case EVP_PKEY_DSA4:
			result = intern("dsa4");
			break;
#endif
#ifndef OPENSSL_NO_DH
		case EVP_PKEY_DH:
			result = intern("dh");
			break;
#endif
#ifndef OPENSSL_NO_EC
		case EVP_PKEY_EC:
			result = intern("ec");
			break;
#endif
		default:
			result = intern("unknown");
			break;
		}

		return result;
	} else
		return Qnil;
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
	DEFSUBR(Fossl_digest_size);
	DEFSUBR(Fossl_digest_bits);
	DEFSUBR(Fossl_digest_block_size);
	DEFSUBR(Fossl_cipher_key_length);
	DEFSUBR(Fossl_cipher_bits);
	DEFSUBR(Fossl_cipher_iv_length);
	DEFSUBR(Fossl_cipher_block_size);
	DEFSUBR(Fossl_cipher_mode);

	DEFSUBR(Fossl_rand_bytes);
	DEFSUBR(Fossl_rand_bytes_egd);

	DEFSUBR(Fossl_digest);
	DEFSUBR(Fossl_digest_file);

	DEFSUBR(Fossl_hmac);
	DEFSUBR(Fossl_hmac_file);

	DEFSUBR(Fossl_bytes_to_key);
	DEFSUBR(Fossl_encrypt);
	DEFSUBR(Fossl_encrypt_file);
	DEFSUBR(Fossl_decrypt);
	DEFSUBR(Fossl_decrypt_file);

	/* general pkey */
	DEFSUBR(Fossl_pkey_p);
	DEFSUBR(Fossl_pkey_size);
	DEFSUBR(Fossl_pkey_private_p);
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
	DEFSUBR(Fossl_ec_available_curves);
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
	DEFSUBR(Fossl_pem_public_key);
	DEFSUBR(Fossl_pem_key);

/* SSL */
	defsymbol(&Qssl_connp, "ossl-ssl-conn-p");
	defsymbol(&Qssl2, "ssl2");
	defsymbol(&Qssl23, "ssl23");
	defsymbol(&Qssl3, "ssl3");
	defsymbol(&Qtls1, "tls1");
#ifdef HAVE_SOCKETS
	DEFSUBR(Fossl_ssl_handshake);
	DEFSUBR(Fossl_ssl_inject_ca);
	DEFSUBR(Fossl_ssl_inject_cert);
	DEFSUBR(Fossl_ssl_proselytise_process);
	DEFSUBR(Fossl_ssl_unproselytise_process);
	DEFSUBR(Fossl_ssl_connect);
	DEFSUBR(Fossl_ssl_finish);
	DEFSUBR(Fossl_ssl_read);
	DEFSUBR(Fossl_ssl_write);
	DEFSUBR(Fossl_ssl_parent);
	DEFSUBR(Fossl_ssl_cert);
	DEFSUBR(Fossl_ssl_peer_cert);
	DEFSUBR(Fossl_ssl_peer_cert_chain);
	DEFSUBR(Fossl_ssl_verify_certificate);
	DEFSUBR(Fossl_ssl_cipher_version);
	DEFSUBR(Fossl_ssl_cipher_name);
	DEFSUBR(Fossl_ssl_cipher_names);
	DEFSUBR(Fossl_ssl_cipher_bits);
	DEFSUBR(Fossl_ssl_cipher_description);
#endif

	DEFSUBR(Fossl_x509_subject);
	DEFSUBR(Fossl_x509_issuer);
	DEFSUBR(Fossl_x509_serial);
	DEFSUBR(Fossl_x509_not_before);
	DEFSUBR(Fossl_x509_not_after);
	DEFSUBR(Fossl_x509_signature_type);
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
	Fprovide(intern("openssl-ssl"));
#endif
}
