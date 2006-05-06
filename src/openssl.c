/*
  openssl.c -- Emacs Lisp binding to OpenSSL ciphers and digests
  Copyright (C) 2005, 2006 Sebastian Freundt

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

#define OSSL_STRING_LENGTH XSTRING_CHAR_LENGTH

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
int ec_curve_by_name(char *name);
#endif

int dh_pkey_p(EVP_PKEY *pkey);
#ifndef OPENSSL_NO_DH
int dh_pkey_has_public_data(DH *dh_key);
int dh_pkey_has_private_data(DH *dh_key);
DH *dh_get_public(EVP_PKEY *pk);
#endif


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

	OpenSSL_add_all_digests();

	digests = Qnil;

	/*  is there a better way to get the size of the nid list? */
	for (nid = 10000; nid >= 0; --nid) {
		const EVP_MD *digest = EVP_get_digestbynid(nid);
		if (digest) {
			const Lisp_Object dgstname =
				Fintern(build_string(OBJ_nid2sn(nid)), Qnil);
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
				Fintern(build_string(OBJ_nid2sn(nid)), Qnil);
			ciphers = Fcons(ciphname,ciphers);
		}
	}

	EVP_cleanup();

	return ciphers;
}


DEFUN("ossl-digest-bits", Fossl_digest_bits, 1, 1, 0, /*
Return the number of effective output bits of DIGEST.
						      */
      (digest))
{
	int kl;
	const EVP_MD *md;

	OpenSSL_add_all_digests();

	md = EVP_get_digestbyname(
		(char *)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	kl = EVP_MD_size(md);

	EVP_cleanup();

	return make_int(kl*8);
}

DEFUN("ossl-cipher-bits", Fossl_cipher_bits, 1, 1, 0, /*
Return the number of effective bits of CIPHER.
						      */
      (cipher))
{
	int kl;
	const EVP_CIPHER *ciph;

	OpenSSL_add_all_ciphers();

	ciph = EVP_get_cipherbyname(
		(char *)string_data(XSYMBOL(cipher)->name));

	if (!ciph) {
		EVP_cleanup();
		error ("no such cipher");
	}

	kl = EVP_CIPHER_key_length(ciph);

	EVP_cleanup();

	return make_int(kl*8);
}



static Lisp_Object free_malloced_ptr(Lisp_Object unwind_obj)
{
	void *ptr = (void *)get_opaque_ptr(unwind_obj);
	xfree(ptr);
	free_opaque_ptr(unwind_obj);
	return Qnil;
}

/* Don't use alloca for regions larger than this, lest we overflow
   the stack.  */
#define MAX_ALLOCA 65536

/* We need to setup proper unwinding, because there is a number of
   ways these functions can blow up, and we don't want to have memory
   leaks in those cases.  */
#define XMALLOC_OR_ALLOCA(ptr, len, type) do {				\
  size_t XOA_len = (len);						\
  if (XOA_len > MAX_ALLOCA) {						\
	  ptr = xnew_array (type, XOA_len);				\
	  record_unwind_protect (free_malloced_ptr,			\
				 make_opaque_ptr ((void *)ptr));	\
  }									\
  else									\
    ptr = alloca_array (type, XOA_len);					\
} while (0)

#define XMALLOC_UNBIND(ptr, len, speccount) do {			\
  if ((len) > MAX_ALLOCA)						\
    unbind_to (speccount, Qnil);					\
} while (0)


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

	int speccount = specpdl_depth();

	CHECK_NATNUM(count);
	count_ext = (int)XINT(count);

	/* now allocate some output buffer externally */
	XMALLOC_OR_ALLOCA(outbuf, count_ext, char);

	if (!RAND_bytes((unsigned char*)outbuf, count_ext)) {
		error ("RAND_bytes did not have enough seed "
		       "to perform operation");
		return Qnil;
	}

	l_outbuf = make_ext_string(outbuf, count_ext, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, count_ext, speccount);

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

	mdctx = xmalloc(sizeof(EVP_MD_CTX));
	EVP_MD_CTX_init(mdctx);
	EVP_DigestInit_ex(mdctx, md, NULL);
	EVP_DigestUpdate(mdctx,(char*)XSTRING_DATA(string),
			 XSTRING_LENGTH(string));
	EVP_DigestFinal_ex(mdctx, (unsigned char *)md_value, &md_len);
	EVP_MD_CTX_cleanup(mdctx);

	EVP_cleanup();
	free(mdctx);

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
	unsigned int md_len, md_blocksize, n;
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

	mdctx = xmalloc(sizeof(EVP_MD_CTX));
	EVP_MD_CTX_init(mdctx);
	md_blocksize = (unsigned int)(EVP_MD_block_size(md) / 8);

	EVP_DigestInit_ex(mdctx, md, NULL);

	/* we reuse md_value here for streaming over fp */
	do {
		n = fread(md_value, 1, EVP_MAX_MD_SIZE, fp);
		if (n < 0) {
			EVP_cleanup();
			fclose(fp);
			free(mdctx);
			error("file corrupted");
			return Qnil;
		}
		EVP_DigestUpdate(mdctx, md_value, n);
   	} while (n > 0);

	EVP_DigestFinal_ex(mdctx, md_value, &md_len);
	EVP_MD_CTX_cleanup(mdctx);

	EVP_cleanup();
	free(mdctx);
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
	md = (EVP_MD *)EVP_get_digestbyname(
		(char *)string_data(XSYMBOL(digest)->name));

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

	hmacctx = xmalloc(sizeof(HMAC_CTX));
	HMAC_CTX_init(hmacctx);
	HMAC_Init(hmacctx, password_ext, password_len, md);
	HMAC_Update(hmacctx, (unsigned char*)XSTRING_DATA(msg),
		    XSTRING_LENGTH(msg));
	HMAC_Final(hmacctx, outbuf, &outlen);
	HMAC_CTX_cleanup(hmacctx);
	free(hmacctx);

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
	unsigned int outlen, n;
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
	    (fseek(fp, 0, SEEK_SET)))
		return wrong_type_argument(Qfile_readable_p, file);


	OpenSSL_add_all_digests();
	md = (EVP_MD *)EVP_get_digestbyname(
		(char *)string_data(XSYMBOL(digest)->name));

	if (!md) {
		EVP_cleanup();
		error ("no such digest");
	}

	TO_EXTERNAL_FORMAT (LISP_STRING, password,
			    C_STRING_ALLOCA, password_ext, OSSL_CODING);
	password_len = OSSL_STRING_LENGTH(password);

	hmacctx = xmalloc(sizeof(HMAC_CTX));
	HMAC_CTX_init(hmacctx);
	HMAC_Init(hmacctx, password_ext, password_len, md);

	/* we reuse md_value here for streaming over fp */
	do {
		n = fread(outbuf, 1, EVP_MAX_MD_SIZE, fp);
		if (n < 0) {
			EVP_cleanup();
			fclose(fp);
			free(hmacctx);
			error("file corrupted");
			return Qnil;
		}
		HMAC_Update(hmacctx, outbuf, n);
   	} while (n > 0);

	HMAC_Final(hmacctx, outbuf, &outlen);
	HMAC_CTX_cleanup(hmacctx);
	free(hmacctx);

	EVP_cleanup();

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
non-`nil', use this IV instead.

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
	XMALLOC_OR_ALLOCA(outbuf, alloclen, char);

	TO_EXTERNAL_FORMAT(LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT(LISP_STRING, iv,
			   C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xmalloc(sizeof(EVP_CIPHER_CTX));
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_EncryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in EncryptInit");
	}
	if (!EVP_EncryptUpdate(ciphctx,
			       (unsigned char *)outbuf, &outlen,
			       (unsigned char *)string_ext, string_len)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in EncryptUpdate");
	}
	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_EncryptFinal(ciphctx,
			      (unsigned char *)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in EncryptFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	l_outbuf = make_ext_string(outbuf, outlen, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	free(ciphctx);

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
non-`nil', use this IV instead.

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
	unsigned int string_len;
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
	    (fseek(fp, 0, SEEK_SET)))
		return wrong_type_argument(Qfile_readable_p, file);

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
		error ("no such cipher");
	}

	/* now allocate some output buffer externally
	 * this one has to be at least EVP_CIPHER_block_size bigger
	 * since block algorithms merely operate blockwise
	 */
	block_len = EVP_CIPHER_block_size(ciph);
	if (of)
		alloclen = 2048;
	else
		alloclen = file_size + block_len;
	XMALLOC_OR_ALLOCA(outbuf, alloclen, unsigned char);

	TO_EXTERNAL_FORMAT(LISP_STRING, key,
			   C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT(LISP_STRING, iv,
			   C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xmalloc(sizeof(EVP_CIPHER_CTX));
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_EncryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		free(ciphctx);
		error("error in EncryptInit");
	}

	obp = outbuf;
	outlen = 0;
	do {
		string_len = fread(string_in, 1, 1024, fp);
		if (string_len < 0) {
			EVP_cleanup();
			fclose(fp);
			free(ciphctx);
			error("file corrupted");
			return Qnil;
		}

		tmplen = 0;
		if (string_len > 0 &&
		    !EVP_EncryptUpdate(ciphctx,
				       obp, &tmplen,
				       string_in, string_len)) {
			EVP_cleanup();
			free(ciphctx);
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
		free(ciphctx);
		error("error in EncryptFinal");
	}

	if (of)
		fwrite(obp, 1, tmplen, of);

	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	if (of)
		l_outbuf = outfile;
	else
		l_outbuf = make_ext_string((char*)outbuf, outlen, OSSL_CODING);

	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	free(ciphctx);
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
non-`nil', use this IV instead.
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
	XMALLOC_OR_ALLOCA(outbuf, alloclen, char);

	TO_EXTERNAL_FORMAT (LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT (LISP_STRING, iv,
			    C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xmalloc(sizeof(EVP_CIPHER_CTX));
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_DecryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in DecryptInit");
	}
	if (!EVP_DecryptUpdate(ciphctx,
			       (unsigned char *)outbuf, &outlen,
			       (unsigned char *)string_ext,string_len)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in DecryptUpdate");
	}
	/* Buffer passed to EVP_EncryptFinal() must be after data just
	 * encrypted to avoid overwriting it.
	 */
	if (!EVP_DecryptFinal(ciphctx,
			      (unsigned char *)outbuf+outlen, &tmplen)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in DecryptFinal");
	}
	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	l_outbuf = make_ext_string(outbuf, outlen, OSSL_CODING);
	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	free(ciphctx);

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
non-`nil', use this IV instead.

Optional fifth argument OUTFILE may specify a file to have the
encrypted data redirected.
						*/
      (cipher, file, key, iv, outfile))
{
	/* buffer for the external string */
	unsigned char string_in[1024];
	unsigned int string_len;
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
	    (fseek(fp, 0, SEEK_SET)))
		return wrong_type_argument(Qfile_readable_p, file);

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
		error ("no such cipher");
	}

	/* now allocate some output buffer externally */
	block_len = EVP_CIPHER_block_size(ciph);
	if (of)
		alloclen = 2048;
	else
		alloclen = file_size + block_len;
	XMALLOC_OR_ALLOCA(outbuf, alloclen, unsigned char);

	TO_EXTERNAL_FORMAT (LISP_STRING, key,
			    C_STRING_ALLOCA, key_ext, OSSL_CODING);
	TO_EXTERNAL_FORMAT (LISP_STRING, iv,
			    C_STRING_ALLOCA, iv_ext, OSSL_CODING);

	ciphctx = xmalloc(sizeof(EVP_CIPHER_CTX));
	EVP_CIPHER_CTX_init(ciphctx);
	if (!EVP_DecryptInit(ciphctx, ciph,
			     (unsigned char *)key_ext,
			     (unsigned char *)iv_ext)) {
		EVP_cleanup();
		free(ciphctx);
		error ("error in DecryptInit");
	}

	obp = outbuf;
	outlen = 0;
	do {
		string_len = fread(string_in, 1, 1024, fp);
		if (string_len < 0) {
			EVP_cleanup();
			fclose(fp);
			free(ciphctx);
			error("file corrupted");
			return Qnil;
		}

		tmplen = 0;
		if (string_len > 0 &&
		    !EVP_DecryptUpdate(ciphctx,
				       obp, &tmplen,
				       string_in, string_len)) {
			EVP_cleanup();
			free(ciphctx);
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
		free(ciphctx);
		error ("error in DecryptFinal");
	}

	if (of)
		fwrite(obp, 1, tmplen, of);

	/* added probable padding space to the length of the output buffer */
	outlen += tmplen;
	EVP_CIPHER_CTX_cleanup(ciphctx);

	if (of)
		l_outbuf = outfile;
	else
		l_outbuf = make_ext_string((char*)outbuf, outlen, OSSL_CODING);

	XMALLOC_UNBIND(outbuf, alloclen, speccount);

	EVP_cleanup();
	free(ciphctx);
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
make_evp_pkey(Lisp_EVP_PKEY * evp_pkey)
{
	Lisp_Object lisp_evp_pkey;
	XSETEVPPKEY(lisp_evp_pkey, evp_pkey);
	return lisp_evp_pkey;
}

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

	/* avoid some warning */
	if (escapeflag);
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

	/* avoid some warning */
	if (for_disksave);
}

DEFINE_LRECORD_IMPLEMENTATION("evp_pkey", evp_pkey,
			      mark_evp_pkey, print_evp_pkey, finalize_evp_pkey,
			      NULL, NULL, 0, Lisp_EVP_PKEY);

static Lisp_Object
make_evp_pkey_raw(EVP_PKEY *pkey)
{
	Lisp_EVP_PKEY *lisp_pkey;

	lisp_pkey = allocate_evp_pkey();
	lisp_pkey->evp_pkey = pkey;

	return make_evp_pkey(lisp_pkey);
}	


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

This function is not (yet) native OpenSSL.
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

	return make_evp_pkey_raw(pkout);
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

	CHECK_NATNUM(bits);
	CHECK_NATNUM(exp);


	if (!XINT(bits))
		error ("modulus size must be a non-zero positive integer");
	if (!(XINT(exp) % 2))
		error ("exponent must be an odd positive integer");

	pkey = EVP_PKEY_new();
	rsakey = RSA_generate_key(XINT(bits), XINT(exp), NULL, NULL);
	EVP_PKEY_assign_RSA(pkey, rsakey);

	return make_evp_pkey_raw(pkey);
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

	return make_evp_pkey_raw(pkey);
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
		const Lisp_Object crvname =
			Fintern(build_string(OBJ_nid2sn(nid)), Qnil);
		lcurves = Fcons(crvname,lcurves);
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

At the moment we do not support creating own curves.
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

	return make_evp_pkey_raw(pkey);
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
with the hybride technique.

That is, create a random key/iv pair for the symmetric encryption with 
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
		(char *)string_data(XSYMBOL(cipher)->name));

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
		(char *)string_data(XSYMBOL(cipher)->name));

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
		(char *)string_data(XSYMBOL(digest)->name));

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
		(char *)string_data(XSYMBOL(digest)->name));

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
	/* output file */
	FILE *fp;
	/* the expanded file name */
	Lisp_Object file_expanded;


	CHECK_STRING(file);


	file_expanded = Fexpand_file_name(file,Qnil);

	if ((fp = fopen((char *)XSTRING_DATA(file_expanded),"r")) == NULL)
		error ("error opening file.");

	pk = PEM_read_PUBKEY(fp, NULL, NULL, NULL);
	if (pk == NULL)
		error ("error reading PEM file.");

	fclose(fp);

	return make_evp_pkey_raw(pk);
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
	/* the expanded file name */
	Lisp_Object file_expanded;


	CHECK_STRING(file);


	file_expanded = Fexpand_file_name(file,Qnil);

	if ((fp = fopen((char *)XSTRING_DATA(file_expanded),"r")) == NULL)
		error ("error opening file.");

	if (NILP(password))
		pass = NULL;
	else {
		CHECK_STRING(password);
		pass = (char *)XSTRING_DATA(password);
	}

	pk = PEM_read_PrivateKey(fp, NULL, NULL, pass);
	fclose(fp);
	if (pk == NULL) {
		/* now maybe it is a public key only */
		return Fossl_pem_read_public_key(file);
	}

	return make_evp_pkey_raw(pk);
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
	/* the expanded file name */
	Lisp_Object file_expanded;


	CHECK_STRING(file);
	CHECK_EVPPKEY(pkey);


	file_expanded = Fexpand_file_name(file,Qnil);

	pk = (XEVPPKEY(pkey))->evp_pkey;

	if ((fp = fopen((char *)XSTRING_DATA(file_expanded),"w")) == NULL)
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
	/* output file */
	FILE *fp;
	/* password pointer */
	char *pass;
	/* the expanded file name */
	Lisp_Object file_expanded;


	CHECK_STRING(file);
	CHECK_EVPPKEY(pkey);


	file_expanded = Fexpand_file_name(file,Qnil);

	pk = (XEVPPKEY(pkey))->evp_pkey;
	if (!ossl_pkey_has_private_data(pk))
		return Fossl_pem_write_public_key(file,pkey);

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

	if ((fp = fopen((char *)XSTRING_DATA(file_expanded),"w")) == NULL) {
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

DEFUN("ossl-pem-key",Fossl_pem_key, 1, 1, 0, /*
Return PKEY as PEM encoded string.
					     */
      (pkey))
{
	/* declarations for the pkey */
	EVP_PKEY *pk;


	CHECK_EVPPKEY(pkey);


	pk = (XEVPPKEY(pkey))->evp_pkey;

	PKCS8_PRIV_KEY_INFO *p8inf;
	p8inf = EVP_PKEY2PKCS8(pk);

	return make_ext_string(
		(char*)((p8inf->pkey)->value.printablestring),
		256, OSSL_CODING);
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
	/* avoid some warning */
	if (obj);

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


	/* avoid some warning */
	if (escapeflag);

#ifdef HAVE_SOCKETS
	if (PROCESSP(parent)) { /* && network_connection_p(parent)) { */
		Lisp_Process *p = XPROCESS(parent);
		strcat(buf, " on top of ");
		strcat(buf, (char *)XSTRING_DATA(p->name));
/* 		strcat(buf, (char *)XSTRING_DATA(Fcdr(p->pid)));
 * 		strcat(buf, ":");
 * 		strcat(buf, (char *)XSTRING_DATA(Fcar(p->pid)));
 */
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
	meth=(SSL_METHOD *)SSLv23_client_method();
#elif !defined(OPENSSL_NO_SSL3)
	meth=(SSL_METHOD *)SSLv3_client_method();
#elif !defined(OPENSSL_NO_SSL2)
	meth=(SSL_METHOD *)SSLv2_client_method();
#else
	meth=(SSL_METHOD *)TLSv1_client_method();
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
	if (PROCESSP(ssl_conn->parent)) {
		XPROCESS(ssl_conn->parent)->process_type = PROCESS_TYPE_NETWORK;
		XPROCESS(ssl_conn->parent)->process_type_data = Qnil;
	}
	/* we leave the process alive, it's not our fault, but
	 * we nullify its pointer
	 */
	ssl_conn->parent = Qnil;
	ssl_conn->infd = NULL;
	ssl_conn->outfd = NULL;

	ssl_conn->connected_p = 0;
	ssl_conn->protected_p = 0;

	/* avoid some warning */
	if (for_disksave);
}

DEFINE_LRECORD_IMPLEMENTATION("ssl_conn", ssl_conn,
			      mark_ssl_conn, print_ssl_conn, finalize_ssl_conn,
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

DEFUN("ossl-connect", Fossl_connect, 1, 2, 0, /*
Return an SSL connection object on top of PROCESS.
Establishing a connection performs a handshake with the
other peer.

If optional argument TIMEOUT is non-nil, it should be a
positive integer to indicate (in seconds) how long to wait
for a successful handshake.
Default: 2 seconds.
					      */
      (process, timeout))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CTX *ctx=NULL;
	SSL_METHOD *meth=NULL;
	/* network stream stuff */
	EMACS_INT *infd, *outfd;
	/* aux */
	int ok=0, ret, err, timeout_ext;
	struct timeval tout;
	fd_set read_fds, write_fds;
	/* the result(s) */
	Lisp_SSL_CONN *lisp_ssl_conn;
	Lisp_Object result;


	CHECK_PROCESS(process);

	if (NILP(timeout))
		timeout_ext = 2;
	else {
		CHECK_NATNUM(timeout);
		timeout_ext = XINT(timeout);
	}


	/* Make sure the process is really alive.  */
	if (!EQ(XPROCESS(process)->status_symbol, Qrun))
		error("Network stream %s not alive",
		      XSTRING_DATA(XPROCESS(process)->name));
	/* Make sure the process is a network stream. */
	if (!network_connection_p(process))
		error("Process %s is not a network stream",
		      XSTRING_DATA(XPROCESS(process)->name));

	/* start preparing the conn object */
	SSL_library_init();
	SSL_load_error_strings();

#if !defined(OPENSSL_NO_SSL2) && !defined(OPENSSL_NO_SSL3)
	meth=(SSL_METHOD *)SSLv23_client_method();
#elif !defined(OPENSSL_NO_SSL3)
	meth=(SSL_METHOD *)SSLv3_client_method();
#elif !defined(OPENSSL_NO_SSL2)
	meth=(SSL_METHOD *)SSLv2_client_method();
#else
	meth=(SSL_METHOD *)TLSv1_client_method();
#endif

	ctx=SSL_CTX_new(meth);
	if (ctx == NULL)
		error ("SSL context initialisation failed");

	/* now initialise a new connection context */
	conn=SSL_new(ctx);
	if (conn == NULL)
		error ("SSL connection initialisation failed");

	/* re-organise the fds */
	infd = (EMACS_INT*)get_process_infd(XPROCESS(process));
	outfd = (EMACS_INT*)get_process_outfd(XPROCESS(process));

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
	XPROCESS(process)->process_type = PROCESS_TYPE_SSL;
	XPROCESS(process)->process_type_data = Qnil;

	/* now finally, perform a test connection */
	/* give it a file descriptor to use */
	SSL_set_rfd(conn,*infd);
	SSL_set_wfd(conn,*outfd);

	/* dont read events on process, otherwise encrypted data is
	 * read off the channel and not available anymore
	 */
	//event_stream_unselect_process(XPROCESS(process));


	/* perform handshake */
	/* set timeout first */
	tout.tv_sec = timeout_ext;
	tout.tv_usec = 0;

	SSL_set_connect_state(conn);
	while (!ok) {
		ret = SSL_connect(conn);
		err = SSL_get_error(conn, ret);

		switch (err) {
		case SSL_ERROR_NONE:
			ok = 1;
			break;
		case SSL_ERROR_WANT_READ:
			FD_ZERO(&read_fds);
			FD_SET(*infd, &read_fds);

			/* wait for socket to be readable */
			ret = select(*infd+1, &read_fds, 0, NULL, &tout);
			if (!ret)
				break;	/* re-issue the read */

			ret = select(*infd+1,&read_fds, 0, NULL, NULL);
			if (ret < 0)
				error("Timeout during read");
			break;
		case SSL_ERROR_WANT_WRITE:
			FD_ZERO(&write_fds);
			FD_SET(*outfd, &write_fds);

			/* wait for socket to be wriable */
			ret = select(*outfd+1, 0, &write_fds, NULL, &tout);
			if (!ret)
				break;	/* re-issue the write */

			ret = select(*outfd+1, 0, &write_fds, NULL, NULL);
			if (ret < 0)
				error("Timeout during write");
			break;
		default:
			error("Severe SSL connection error");
		}
	}

	lisp_ssl_conn->connected_p = 1;
	result = make_ssl_conn(lisp_ssl_conn);
	XPROCESS(process)->process_type_data = result;

	return result;
}

/* EXTREEEEEEEEEEMELY DANGEROUS */
/* DEFUN("set-process-type", Fset_process_type, 2, 2, 0, / *
 * 						       * /
 *       (process, type))
 * {
 * 	XPROCESS(process)->process_type = XINT(type);
 * 	return type;
 * }
 */

DEFUN("ossl-finish", Fossl_finish, 1, 1, 0, /*
Finish an SSL connection SSL-CONN.

Note: This may also finish the network connection.
					    */
      (ssl_conn))
{
	CHECK_SSLCONN(ssl_conn);

	if (XSSLCONN(ssl_conn)->protected_p)
		error ("Cannot finish protected SSL connection");

	finalize_ssl_conn(XSSLCONN(ssl_conn), 0);
	return ssl_conn;
}

DEFUN("ossl-pending", Fossl_pending, 1, 2, 0, /*
Return the number of pending bytes which have not yet been read.

Note: Depending on whether you use a process buffer or a process
filter this function may always return 0 because SXEmacs in these
cases grabs everything on the wire automagically.
					       */
      (ssl_conn, timeout))
{
	/* network stream stuff */
	SSL *conn;
	/* aux */
	int pending;
	EMACS_INT *infd;
	struct timeval tout;
	int timeout_ext;
	fd_set read_fds;


	CHECK_SSLCONN(ssl_conn);

	if (!ssl_conn_alive_p(XSSLCONN(ssl_conn)))
		error("SSL connection dead");

	if (NILP(timeout))
		timeout_ext = 2;
	else {
		CHECK_NATNUM(timeout);
		timeout_ext = XINT(timeout);
	}


	conn = XSSLCONN(ssl_conn)->ssl_conn;
	infd = XSSLCONN(ssl_conn)->infd;
	/* pending = SSL_pending(conn); */

	tout.tv_sec = timeout_ext;
	tout.tv_usec = 0;

	FD_ZERO(&read_fds);
	FD_SET(*infd, &read_fds);
	
	/* wait for socket to be readable */
	pending= select(*infd+1, &read_fds, 0, NULL, &tout);

	if (pending < 0)
		return Qnil;
	else
		return Qt;
}

DEFUN("ossl-read", Fossl_read, 1, 3, 0, /*
Read out everything from the tunnel SSL-CONN.

If optional argument SYNCHRONOUSP is non-`nil' wait for data to read.

If optional argument TIMEOUT is non-`nil' it should be a number
indicating (in asynchronous mode) how long at most to wait for data
to arrive.
					*/
      (ssl_conn, synchronousp, timeout))
{
	/* network stream stuff */
	SSL *conn;
	/* aux */
	int err, ret;
	EMACS_INT *infd, *outfd;
	int pending;
	char buf[1024];
	struct timeval tout;
	int timeout_ext;
	fd_set read_fds;
	int sync_p;
	/* the result */
	Lisp_Object result;


	CHECK_SSLCONN(ssl_conn);

	if (!ssl_conn_alive_p(XSSLCONN(ssl_conn)))
		error("SSL connection dead");

	if (NILP(synchronousp))
		sync_p = 0;
	else 
		sync_p = 1;

	if (NILP(timeout))
		timeout_ext = 2;
	else {
		CHECK_NATNUM(timeout);
		timeout_ext = XINT(timeout);
	}


	conn = XSSLCONN(ssl_conn)->ssl_conn;
	infd = XSSLCONN(ssl_conn)->infd;
	outfd = XSSLCONN(ssl_conn)->outfd;
	result = build_string("");

	tout.tv_sec = timeout_ext;
	tout.tv_usec = 0;

	pending = 1;
	while (pending) {
		ret = SSL_read(conn, buf, 1024);
		err = SSL_get_error(conn, ret);

		switch (err) {
		case SSL_ERROR_NONE:
			result = concat2(result,
					 make_ext_string(buf, ret, OSSL_CODING));
			pending = SSL_pending(conn);
			break;
		case SSL_ERROR_WANT_WRITE:
			error("Connection wants write");
		case SSL_ERROR_WANT_READ:
			FD_ZERO(&read_fds);
			FD_SET(*infd, &read_fds);

			/* wait for socket to be readable */
			ret = select(*infd+1, &read_fds, 0, NULL, &tout);
			if (!ret)
				break;	/* re-issue the read */

			ret = select(*infd+1,&read_fds, 0, NULL, NULL);
/* 			if (ret < 0)
 * 				error("Timeout during read");
 */
			result = Qnil;
			/* now, in case of synchronous read, stay in the while */
			pending = sync_p;
			break;
		case SSL_ERROR_ZERO_RETURN:
			Fossl_finish(ssl_conn);
			return Qnil;
		default:
			error("Severe SSL connection error: %d", err);
		}
	}

	return result;
}

DEFUN("ossl-write", Fossl_write, 2, 2, 0, /*
Send STRING to the tunnel SSL-CONN.
					  */
      (ssl_conn, string))
{
	/* network stream stuff */
	SSL *conn;
	/* aux */
	int ret;


	CHECK_SSLCONN(ssl_conn);
	CHECK_STRING(string);

	if (!ssl_conn_alive_p(XSSLCONN(ssl_conn)))
		error("SSL connection dead");


	conn = XSSLCONN(ssl_conn)->ssl_conn;

	ret = SSL_write(conn, XSTRING_DATA(string), XSTRING_LENGTH(string));

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

	return make_int(SSL_pending(conn));
}


DEFUN("ossl-x509-get-subject", Fossl_x509_get_subject, 1, 1, 0, /*
Return the other peer's certificate subject.

This will return a string in LDAP syntax.
								*/
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	X509 *cert=NULL;


	CHECK_SSLCONN(ssl_conn);


	conn = XSSLCONN(ssl_conn)->ssl_conn;
	cert = SSL_get_peer_certificate(conn);

	if (!(cert == NULL)) {
		X509_NAME *subject = X509_get_subject_name(cert);
		X509_free(cert);
		return build_string(X509_NAME_oneline(subject,NULL,0));
	} else
		return Qnil;
}

DEFUN("ossl-x509-get-issuer", Fossl_x509_get_issuer, 1, 1, 0, /*
Return the other peer's certificate issuer, that is the organisation
who certified the other peer.

This will return a string in LDAP syntax.
							       */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	X509 *cert;


	CHECK_SSLCONN(ssl_conn);


	conn = XSSLCONN(ssl_conn)->ssl_conn;
	cert = SSL_get_peer_certificate(conn);

	if (!(cert == NULL)) {
		X509_NAME *issuer = X509_get_issuer_name(cert);
		X509_free(cert);
		return build_string(X509_NAME_oneline(issuer,NULL,0));
	} else
		return Qnil;
}

DEFUN("ossl-x509-get-pubkey", Fossl_x509_get_pubkey, 1, 1, 0, /*
Return the other peer's public key.

This will return a pkey object.
							      */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	X509 *cert=NULL;


	CHECK_SSLCONN(ssl_conn);


	conn = XSSLCONN(ssl_conn)->ssl_conn;
	cert = SSL_get_peer_certificate(conn);

	if (!(cert == NULL)) {
		EVP_PKEY *pkey = X509_get_pubkey(cert);
		X509_free(cert);
		return make_evp_pkey_raw(pkey);
	} else
		return Qnil;
}

DEFUN("ossl-sslcipher-version", Fossl_sslcipher_version, 1, 1, 0, /*
Return the protocol version of the tunnel SSL-CONN.
								    */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CIPHER *ciph;
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

DEFUN("ossl-sslcipher-name", Fossl_sslcipher_name, 1, 1, 0, /*
Return the name of the cipher used in the tunnel SSL-CONN.
							      */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CIPHER *ciph;
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
			build_string(SSL_CIPHER_get_name(ciph)));
	else
		return Qnil;
}

DEFUN("ossl-sslcipher-bits", Fossl_sslcipher_bits, 1, 1, 0, /*
Return the number of effective bits of the encryption key in SSL-CONN.
							      */
      (ssl_conn))
{
	/* SSL connection stuff */
	SSL *conn=NULL;
	SSL_CIPHER *ciph;
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
	DEFSUBR(Fossl_digest_bits);
	DEFSUBR(Fossl_cipher_bits);

	DEFSUBR(Fossl_rand_bytes);

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
	DEFSUBR(Fossl_pem_key);

/* SSL */
	defsymbol(&Qssl_connp, "ossl-ssl-conn-p");
#ifdef HAVE_SOCKETS
	DEFSUBR(Fossl_connect);
	DEFSUBR(Fossl_finish);
	DEFSUBR(Fossl_pending);
	DEFSUBR(Fossl_read);
	DEFSUBR(Fossl_write);
	DEFSUBR(Fossl_x509_get_subject);
	DEFSUBR(Fossl_x509_get_issuer);
	DEFSUBR(Fossl_x509_get_pubkey);
	DEFSUBR(Fossl_sslcipher_version);
	DEFSUBR(Fossl_sslcipher_name);
	DEFSUBR(Fossl_sslcipher_bits);
#endif

	/* JUST FOR DEBUGGING PURPOSES */
	/* DEFSUBR(Fset_process_type); */
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
