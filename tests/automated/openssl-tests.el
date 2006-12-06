;;;  openssl-tests.el -- Regression Tests for OpenSSL
;; Copyright (C) 2005 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: tests
;;
;; This file is part of SXEmacs.
;; 
;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;; 
;; SXEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with SXEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Synched up with: Not in FSF.
;;
;;; Commentary:
;; - test for cryptographical facilities
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))


(when (featurep 'openssl)
  ;; this is not a real failure, but without digests OpenSSL is useless
  (Assert (< 0 (length (ossl-available-digests))))
  ;; this is not a real failure, but without ciphers OpenSSL is useless
  (Assert (< 0 (length (ossl-available-ciphers))))

  ;;-----------------------------------------------------
  ;; Testing base64, base16 converters
  ;;-----------------------------------------------------
  (Assert (string-equal (base16-encode-string "AbCdEfG") "41624364456647"))
  (Assert (string-equal (base16-decode-string "61426344") "aBcD"))
  (Assert (string-equal (base16-decode-string "6142634") "aBc"))

  ;;-----------------------------------------------------
  ;; Testing digests
  ;;-----------------------------------------------------
  (let ((pre-comp-hashes
	 `(("a test string to digest"
	    (MD2 . "92ff6a34139fe7c8a4fc8910ab04cfb5")
	    (MD4 . "ce1d60a3d1a5f11a24dd5546eb1d26a9")
	    (MD5 . "68e2515ecffedbcf8ec5384070f17bd8")
	    (SHA . "e03994bcae4cf46d3d3bafa21ac9a80882fd10d0")
	    (SHA1 . "620d4122c2efe326e83d9e255976642c1799ae52")
	    (SHA256
	     . "f344f3c6e0e7d48b01104f71663e25a19554946c36ddf94eaa9ff98055a54220")
	    (SHA512
	     . "8020970e4beb48c07f1076552b34af378e08e9433254e3d24364bdbe77de8b6a82a1ab1045f95702a31f3db05948e8bd05d5765461551e40fb9919209e6a05b4")
	    (RIPEMD160 . "e7ed46ec351da9c23ab99971174e3be1a1e1c9c5")
	    (SHA224 . "5448e12607746b71a5bd97d99cc5896293acef755bb079c46ad95578")
	    (MDC2 . "612611f81794892a71f5bc108cfe1b29"))
	   (,(base16-decode-string "0408040021214150")
	    (MD2 . "0e3bac1631d3ec6a6142742a80deece9")
	    (MD4 . "1e74a3a5f55cd154b138585225fdfd24")
	    (MD5 . "a32f0ed6ac3412c6a492ffe33426f6a8")
	    (SHA . "1fe879ebe32795b1e05346743bb84343e25d94a9")
	    (SHA1 . "da23c727ec421df3810bfb2bd8180de81c7cbd0c")
	    (SHA256
	     . "642bf481623f86a12ddce32271a4a391c93c163a4f44091787f5e6529c985672")
	    (SHA512
	     . "1a267c78623654ac9e3c4c5831ac63c87cafbb14313b05b75ed3143285211d59bb04b70dae2c53c0a02ac60d30dcc474668ba4a8f6d96f7ae723bf09f07b2bb9")
	    (RIPEMD160 . "65d2b6018dd5b86ef63250d615f407f7c1608959")
	    (SHA224 . "17af47acac6b8c42f0ec8d22e09d2961e0c9b6f02e8d564e5c6844c7")
	    (MDC2 . "f8ec01ad32b21c6357c0e498cc52ae68"))
	   (""
	    ;; I am not sure if they change it in the future :|
	    (MD2 . "8350e5a3e24c153df2275c9f80692773")
	    (MD4 . "31d6cfe0d16ae931b73c59d7e0c089c0")
	    (MD5 . "d41d8cd98f00b204e9800998ecf8427e")
	    (SHA . "f96cea198ad1dd5617ac084a3d92c6107708c0ef")
	    (SHA1 . "da39a3ee5e6b4b0d3255bfef95601890afd80709")
	    (SHA256
	     . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
	    (SHA512
	     . "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
	    (RIPEMD160 . "9c1185a5c5e9fc54612808977ee8f548b2258d31")
	    (SHA224 . "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f")
	    (MDC2 . "52525252525252522525252525252525"))))
	(mydigests (ossl-available-digests)))
    (mapc #'(lambda (hash)
	      (let ((str (car hash))
		    (pchashes (cdr hash)))
		(mapc #'(lambda (alg+dgst)
			  (when (member (car alg+dgst) mydigests)
			    (eval `(Assert (string-equal
					    (base16-encode-string
					     (ossl-digest ',(car alg+dgst) ,str))
					    ,(cdr alg+dgst))))))
		      pchashes)))
	  pre-comp-hashes))

  ;; testing the lengths of digests
  (let ((dgst-lens
	 '((MD2 . 16)
	   (MD4 . 16)
	   (MD5 . 16)
	   (SHA . 20)
	   (SHA1 . 20)
	   (SHA224 . 28)
	   (SHA256 . 32)
	   (SHA512 . 64)
	   (MDC2 . 16)
	   (RIPEMD160 . 20)))
	(mydigests (ossl-available-digests)))
    (mapc #'(lambda (algo+len)
	      (when (member (car algo+len) mydigests)
		(let (randstrs)
		  (dotimes (i 10)
		    (setq randstrs
			  (cons (ossl-rand-bytes (random 40))
				randstrs)))
		  (setq randstrs
			(append '("" " " "\n")  randstrs))
		  (mapc #'(lambda (randstr)
			    (eval `(Assert (= (length
					       (ossl-digest ',(car algo+len)
							    ,randstr))
					      ,(cdr algo+len)))))
			randstrs))))
	  dgst-lens))

  ;;-----------------------------------------------------
  ;; Testing HMACs
  ;;-----------------------------------------------------
  (let ((pre-comp-macs
	 `(("a test string to digest"
	    (MD2 . "28f832a5088e6153c5d7342bdd27f133")
	    (MD4 . "1b5fb31ce2e5467e472e4790d77310e1")
	    (MD5 . "15337877e5bc8c7f1dbc3f1c5955ef48")
	    (SHA . "ec3905d4aba1ace1f739494478f1c8fcb61f34ea")
	    (SHA1 . "0180ce61c72849c605ca1e6b1fc790574e8e94f3")
	    (SHA256
	     . "8c6e66238cae2fb7b51469b9a56b07d79ca8b95577a08ef88f71811a80bffacf")
	    (SHA512
	     . "f7d15205cfc10c710ee1547bf02cbc665cafea34244e0daa4707f3eb888dd0d7f27ebdc9bc1237b9a566c52b0185d73f3f03f54e387d87a3b350f1048ed31977")
	    (RIPEMD160 . "8afeab4e091a27b2e6a6cab8adf813c18e22c9f5")
	    (SHA224 . "bdd7013afbd17fede079cdff796b9ad41ae4701e40da605629fca5c6")
	    (MDC2 . "da8db2984052b945db36eb5967b854c6"))
	   (,(base16-decode-string "0408040021214150")
	    (MD2 . "773807b9c622cf7e4720bfdbea2d00a0")
	    (MD4 . "157a3019d5d06ab6861e60985c3f6802")
	    (MD5 . "2e86cfb9cf4e9fe4fde6a484a5ae161d")
	    (SHA . "8acd0238da00c91fdd16a63601a36d60e85b8a3f")
	    (SHA1 . "03a6bd81b30ac05e3a74c5f0434e53660e2ad826")
	    (SHA256
	     . "2d7e1cbba92433c22eb4bf2e5a6670783fa0960b0b2799b16f9d2ec09a8ef634")
	    (SHA512
	     . "e700e1696971d156391cf554e0d521dc92ce3ec1b50837b82854567695a414724390c8882575ece15fa1b0fd4dea25dd4ec1bd98084114167d40d664704c0d1b")
	    (RIPEMD160 . "9fd14450a6a9bab2e0d209ca08012710185b5b4b")
	    (SHA224 . "89a4fadeababc107e1d34cec6d57509015398e21a7f85052ad8f34c0")
	    (MDC2 . "9b038620f59f0f98f0223aa4270aee47"))
	   (""
	    ;; I am not sure if they change it in the future :|
	    (MD2 . "34315994b98b7e46ecc8e92e0968e34e")
	    (MD4 . "ad728e8db14adecc49a3fc361b30b715")
	    (MD5 . "23d466834f70a1d657561a5ec2c51ea0")
	    (SHA . "c1ae479310f17ba8923eb94ddb7d1079f69ec56c")
	    (SHA1 . "f1ccdb92a929f5149ed40c5d0f3f2fbbe7809521")
	    (SHA256
	     . "e56a207acd1e6714735487c199c6f095844b7cc8e5971d86c003a7b6f36ef51e")
	    (SHA512
	     . "ded1bda29058c02f011c01087e12543684821c54d42ac2a9924a1a0ee10830b5a2028a90febc0cda5188517bc79aecdbd720b00895181571496e8688c57ddfa0")
	    (RIPEMD160 . "03ea2ae5c6875de5ec79ce4a9a7b37837660794e")
	    (SHA224 . "bf8cc8d94e30e99fa36e0b66ce4220c8a653b830e3d9fb46d25be803")
	    (MDC2 . "bab8504dfe0337da87a79d85567fbf53"))))
	(mydigests (ossl-available-digests)))
    (mapc #'(lambda (mac)
	      (let ((str (car mac))
		    (pchashes (cdr mac)))
		(mapc #'(lambda (alg+dgst)
			  (when (member (car alg+dgst) mydigests)
			    (eval `(Assert (string-equal
					    (base16-encode-string
					     (ossl-hmac ',(car alg+dgst) ,str
							"password"))
					    ,(cdr alg+dgst))))))
		      pchashes)))
	  pre-comp-macs))

  ;; we expect at least md5, sha1 and ripemd160 in available digests
  ;; otherwise OpenSSL must be considered useless!
  (Assert (member 'MD5 (ossl-available-digests)))
  (Assert (member 'SHA1 (ossl-available-digests)))
  (Assert (member 'RIPEMD160 (ossl-available-digests)))

  ;; for non-existing digests we expect an error
  (Check-Error-Message error "no such digest" (ossl-digest 'FOO "bar"))


  ;;-----------------------------------------------------
  ;; Testing ciphers
  ;;-----------------------------------------------------

  ;; we expect at least DES (in the modes ECB, CFB and CBC)
  ;; also AES (with 192 and 256 bits) would be nice
  (Assert (member 'DES-ECB (ossl-available-ciphers)))
  (Assert (member 'DES-CFB (ossl-available-ciphers)))
  (Assert (member 'DES-CBC (ossl-available-ciphers)))
  (Assert (member 'AES-128-CBC (ossl-available-ciphers)))
  (Assert (member 'AES-128-ECB (ossl-available-ciphers)))
  (Assert (member 'AES-128-CFB (ossl-available-ciphers)))
  (Assert (member 'AES-128-OFB (ossl-available-ciphers)))
  (Assert (member 'AES-192-CBC (ossl-available-ciphers)))
  (Assert (member 'AES-192-ECB (ossl-available-ciphers)))
  (Assert (member 'AES-192-CFB (ossl-available-ciphers)))
  (Assert (member 'AES-192-OFB (ossl-available-ciphers)))

  ;; first we check the key generator
  (let ((encstrs
	 (list "foo string test bar"
	       (base16-decode-string "080004084803526444")
	       "\n"))
	(salts
	 (list nil "salt" "" "toomuchsalt"))
	(ciphers
	 (let (ciphers)
	   (mapc-internal
	    #'(lambda (cipher)
		(let ((ciphmode (substring (symbol-name cipher) -2)))
		  ;; Never use CFB1 and CFB8 modes.
		  ;; Both modes tend to mangle the result strings which
		  ;; yields an assertion error.
		  ;; Bug in openssl?
		  ;; -hroptatyr
		  (unless (or (< (ossl-cipher-bits cipher) 128)
			      (string= "B1" ciphmode)
			      (string= "B8" ciphmode))
		    (setq ciphers
			  (cons cipher ciphers)))))
	    (ossl-available-ciphers))
	   ciphers))
	(digests
	 (let (digests)
	   (mapc-internal
	    #'(lambda (digest)
		(let ((digestname (symbol-name digest)))
		  ;; only use digests without a dash in their names
		  (unless (string-match "-" digestname)
		    (setq digests
			  (cons digest digests)))))
	    (ossl-available-digests))
	   digests))
	key iv
	enc dec)

    (mapc-internal
     #'(lambda (salt)
	 (mapc-internal
	  #'(lambda (digest)
	      (mapc-internal
	       #'(lambda (cipher)
		   (mapc-internal
		    #'(lambda (str)
			(setq key ;; a key and initialisation vector
			      (ossl-bytes-to-key cipher digest salt
						 "password" 1)
			      iv (get key 'iv))

			;; let's see if we always generate the same key/iv pair
			(eval `(Assert (and (string-equal
					     (ossl-bytes-to-key
					      ',cipher ',digest ,salt
					      "password" 1)
					     ,key)
					    (string-equal
					     (get (ossl-bytes-to-key
						   ',cipher ',digest ,salt
						   "password" 1) 'iv)
					     ,iv))))

			;; let's encrypt something, decrypt it and compare
			(setq enc (ossl-encrypt cipher str key iv))
			(setq dec (ossl-decrypt cipher enc key iv))
			(eval `(Assert (string-equal ,dec ,str)))

			;; this should even work if the key+iv is regenerated
			(setq key ;; a new key and initialisation vector
			      (ossl-bytes-to-key cipher digest salt "password" 1)
			      iv (get key 'iv))
			(setq dec ;; the encrypted text
			      (ossl-decrypt cipher enc key iv))

			(eval `(Assert (string-equal ,dec ,str)))

			;; let's doubly-encrypt something
			(let (enc2 dec2)
			  (setq enc (ossl-encrypt cipher str key iv))
			  (setq enc2 (ossl-encrypt cipher enc key iv))
			  (setq dec (ossl-decrypt cipher enc2 key iv))
			  (eval `(Assert (string-equal ,dec ,enc)))
			  (setq dec2 (ossl-decrypt cipher dec key iv))
			  (eval `(Assert (string-equal ,dec2 ,str)))))
		    encstrs))
	       ciphers))
	  digests))
     salts))
  )


;;; openssl-tests.el ends here
