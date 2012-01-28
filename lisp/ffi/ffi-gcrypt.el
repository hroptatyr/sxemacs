;;; ffi-gcrypt.el -- FFI access to libgcrypt
;;
;; Copyright (C) 2005, 2006 Sebastian Freundt
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Version: 0.1
;; URL: none
;; Maintainer: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: cryptography, security
;;
;; This file is part of SXEmacs.
;;
;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Comments:
;; This is an FFI implementation of routines in the libgcrypt library.
;;
;;; Code:

(require 'ffi)
(require 'ffi-libc)

;; this is our spine, barf if it does not exist
(ffi-load "libgcrypt")

(defgroup ffi-gcrypt nil
  "FFI bindings for libgcrypt (part of GPG)."
  :group 'extensions)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Message digests ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(unless (ffi-find-named-type 'gcry_md_hd_t)
  (define-ffi-type gcry_md_hd_t (pointer void)))

(defun gcry:md-handle-p (md-handle)
  "Return non-`nil' iff MD-HANDLE is a valid handle for message digests."
  (and (ffi-object-p md-handle)
       (eq (ffi-object-type md-handle) 'gcry_md_hd_t)))

(ffi-enum gcry:md_flags
  "Flags used with the open function."
  gcry:md_flag_empty
  gcry:md_flag_secure
  gcry:md_flag_hmac)

(defconst gcry:md_open
  (ffi-defun '(function int (pointer gcry_md_hd_t) int unsigned-int)
	     "gcry_md_open")
  "Return a handle for message digests.")

;;;###autoload
(defun gcry:md-open (&optional hash-algo)
  "Return a message digest handle, initialised by HASH-ALGO."
  (let ((md-handle (make-ffi-object '(pointer void)))
	(md-number (if hash-algo
		       (gcry:md-map-name hash-algo)
		     0)))
    (let ((g-hd (ffi-address-of md-handle))
	  (g-algo (ffi-create-fo 'int md-number))
	  (g-flags (gcry:md_flags 'gcry:md_flag_empty)))
      (let ((ret (ffi-get
		  (ffi-call-function gcry:md_open g-hd g-algo g-flags)))
	    (hd (ffi-get g-hd)))
	(when (ffi-null-p hd)
	  (error "gcry:md-open: Cannot get initial MD handle"))
	(and (zerop ret)
	     (ffi-set-object-type md-handle 'gcry_md_hd_t)
	     md-handle)))))

(defalias 'gcry:make-message-digest #'gcry:md-open)

(defconst gcry:md_close
  (ffi-defun '(function void gcry_md_hd_t)
	     "gcry_md_close")
  "Destroy a handle for message digests.")

(defmacro gcry:md-close (md-handle)
  "Free resources occupied by MD-HANDLE."
  (when (gcry:md-handle-p (symbol-value md-handle))
    (ffi-call-function gcry:md_close (symbol-value md-handle))
    (set md-handle nil)
    t))

(defalias 'gcry:destroy-message-digest #'gcry:md-close)

(defconst gcry:md_enable
  (ffi-defun '(function int gcry_md_hd_t int)
	     "gcry_md_enable")
  "Enable hash-algorithm within a message digest context.")

(defun gcry:md-enable (md-handle hash-algo)
  "Additionally make MD-HANDLE support the algorithm HASH-ALGO."
  (let ((g-algo (ffi-create-fo 'int (gcry:md-map-name hash-algo))))
    (when (ffi-object-p md-handle)
      (let ((ret (ffi-get
		  (ffi-call-function gcry:md_enable md-handle g-algo))))
	(zerop ret)))))

(defconst gcry:md_map_name
  (ffi-defun '(function int (pointer char))
	     "gcry_md_map_name")
  "Return the enumeration value of a hash algorithm.")

(defun gcry:md-map-name (string)
  "Return the internal number of a hash algorithm specified by STRING."
  (let ((fo (ffi-create-fo 'c-string string)))
    (ffi-get (ffi-call-function gcry:md_map_name fo))))

(defun gcry:md-algo-to-enum (hash-algo)
  "Return the internal form of HASH-ALGO."
  (let ((g-enum (gcry:md-map-name hash-algo)))
    (unless (zerop g-enum)
      (ffi-create-fo 'int g-enum))))
;; (gcry:md-algo-to-enum "SHA512")
;; (gcry:md-map-name "SHA512")
;; (gcry:md-map-name "SHA44")


(defconst gcry:md_write
  (ffi-defun '(function void gcry_md_hd_t (pointer void) unsigned-int)
	     "gcry_md_write")
  "Write data into message digest context.")

(defun gcry:md-write (md-handle data)
  "Write DATA to the digest machinery specified by MD-HANDLE."
  (let ((g-buffer (ffi-create-fo 'c-data data))
	(g-length (ffi-create-fo 'unsigned-int (length data))))
    (when (ffi-object-p md-handle)
      (ffi-call-function gcry:md_write md-handle g-buffer g-length)
      t)))
;; (gcry:md-write handle "kfjf")
;;(ffi-create-fo 'c-string "sifistring")

(defconst gcry:md_get_algo_dlen
  (ffi-defun '(function unsigned-int int)
	     "gcry_md_get_algo_dlen")
  "Return the length of the message digest.")

(defun gcry:md-get-algo-dlen (hash-algo)
  "Return the length of the resulting message hash of HASH-ALGO."
  (let ((g-algo (ffi-create-fo 'int (gcry:md-map-name hash-algo))))
    (ffi-get (ffi-call-function gcry:md_get_algo_dlen g-algo))))

(defconst gcry:md_read
  (ffi-defun '(function c-data gcry_md_hd_t int)
	     "gcry_md_read")
  "Return the message digest.")

(defun gcry:md-read (md-handle hash-algo)
  "Return the message hash of MD-HANDLE under HASH-ALGO."
  (let ((g-algo (ffi-create-fo 'int (gcry:md-map-name hash-algo))))
    (when (ffi-object-p md-handle)
      (let ((ret (ffi-call-function gcry:md_read md-handle g-algo))
	    (len (ffi-get (ffi-call-function gcry:md_get_algo_dlen g-algo))))
	;;(ffi-get ret :type (cons 'c-data len))
	(ffi-fetch ret 0 (cons 'c-data len))))))

;;(setq handle (gcry:md-open "SHA1"))
;;(gcry:md-enable handle "MD5")
;;(gcry:md-write handle "test string what's my hash value?")
;;(setq md1 (gcry:md-read handle "SHA1"))
;;(setq md2 (gcry:md-read handle "MD5"))
;;(gcry:md-close handle)
;;(ossl-digest 'MD5 "test string what's my hash value?")
;;(setq handle (gcry:md-open "SHA4"))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Symmetric ciphers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (ffi-find-named-type 'gcry_cipher_hd_t)
  (define-ffi-type gcry_cipher_hd_t (pointer void)))

(defun gcry:cipher-handle-p (sc-handle)
  "Return non-`nil' iff SC-HANDLE is a valid handle for symmetric ciphers."
  (and (ffi-object-p sc-handle)
       (eq (ffi-object-type sc-handle) 'gcry_cipher_hd_t)))

(ffi-enum gcry:cipher_flags
  "Flags used with the open function."
  gcry:cipher_flag_empty
  gcry:cipher_flag_secure
  gcry:cipher_flag_enable_sync
  gcry:cipher_flag_cbc_cts
  gcry:cipher_flag_cbc_mac)

(ffi-enum gcry:cipher_modes
  "Supported encryption modes.
Not all of them are supported for each algorithm."
  none
  ecb
  cfb
  cbc
  stream
  ofb
  ctr)

(defconst gcry:cipher_open
  (ffi-defun '(function int gcry_cipher_hd_t int int unsigned-int)
	     "gcry_cipher_open")
  "Return a handle for symmetric ciphers.")

;;;###autoload
(defun gcry:cipher-open (cipher-algo &optional mode)
  "Return a symmetric cipher handle, initialised by CIPHER-ALGO."
  (let ((sc-handle (make-ffi-object '(pointer void)))
	(sc-number (if cipher-algo
		       (gcry:cipher-map-name cipher-algo)
		     0)))
    (when (positivep sc-number)
      (let ((g-hd (ffi-address-of sc-handle))
	    (g-algo (ffi-create-fo 'int sc-number))
	    (g-mode (or (gcry:cipher_modes mode)
		       (gcry:cipher_modes 'none)))
	    (g-flags (gcry:cipher_flags 'gcry:cipher_flag_empty)))
	(let ((ret (ffi-get
		    (ffi-call-function
		     gcry:cipher_open g-hd g-algo g-mode g-flags)))
	      (hd (ffi-get g-hd)))
	  (when (ffi-null-p hd)
	    (error "gcry:cipher-open: Cannot get initial cipher handle"))
	  (and (zerop ret)
	       (ffi-set-object-type sc-handle 'gcry_cipher_hd_t)
	       (put sc-handle 'cipher-algo g-algo)
	       (put sc-handle 'cipher-mode g-mode)
	       sc-handle))))))

(defalias 'gcry:make-symmetric-cipher #'gcry:cipher-open)

(defconst gcry:cipher_close
  (ffi-defun '(function void gcry_cipher_hd_t)
	     "gcry_cipher_close")
  "Destroy a handle for symmetric ciphers.")

(defmacro gcry:cipher-close (sc-handle)
  "Free resources occupied by SC-HANDLE."
  (when (gcry:cipher-handle-p (symbol-value sc-handle))
    (ffi-call-function gcry:cipher_close (symbol-value sc-handle))
    (set sc-handle nil)
    t))

(defalias 'gcry:destroy-symmetric-cipher #'gcry:cipher-close)


(defconst gcry:cipher_map_name
  (ffi-defun '(function int (pointer char))
	     "gcry_cipher_map_name")
  "Return the enumeration value of a cipher algorithm.")

(defun gcry:cipher-map-name (string)
  "Return the internal number of a cipher algorithm specified by STRING."
  (let ((fo (ffi-create-fo 'c-string string)))
    (ffi-get (ffi-call-function gcry:cipher_map_name fo))))

(defun gcry:cipher-algo-to-enum (cipher-algo)
  "Return the internal form of CIPHER-ALGO."
  (let ((g-enum (gcry:cipher-map-name cipher-algo)))
    (unless (zerop g-enum)
      (ffi-create-fo 'int g-enum))))
;; (gcry:cipher-algo-to-enum "SHA512")
;; (gcry:cipher-map-name "SHA512")
;; (gcry:cipher-map-name "AES")

(ffi-enum gcry:ctl_cmds
  "Enum of control commands."
  gcryctl_set_key = 1
  gcryctl_set_iv
  gcryctl_cfb_sync
  gcryctl_reset
  gcryctl_finalize
  gcryctl_get_keylen
  gcryctl_get_blklen
  gcryctl_test_algo
  gcryctl_is_secure
  gcryctl_get_asnoid
  gcryctl_enable_algo
  gcryctl_disable_algo
  gcryctl_dump_random_stats
  gcryctl_dump_secmem_stats
  gcryctl_get_algo_npkey
  gcryctl_get_algo_nskey
  gcryctl_get_algo_nsign
  gcryctl_get_algo_nencr
  gcryctl_set_verbosity
  gcryctl_set_debug_flags
  gcryctl_clear_debug_flags
  gcryctl_use_secure_rndpool
  gcryctl_dump_memory_stats
  gcryctl_init_secmem
  gcryctl_term_secmem
  gcryctl_disable_secmem_warn = 27
  gcryctl_suspend_secmem_warn
  gcryctl_resume_secmem_warn
  gcryctl_drop_privs
  gcryctl_enable_m_guard
  gcryctl_start_dump
  gcryctl_stop_dump
  gcryctl_get_algo_usage
  gcryctl_is_algo_enabled
  gcryctl_disable_internal_locking
  gcryctl_disable_secmem
  gcryctl_initialization_finished
  gcryctl_initialization_finished_p
  gcryctl_any_initialization_p
  gcryctl_set_cbc_cts
  gcryctl_set_cbc_mac
  gcryctl_set_ctr
  gcryctl_enable_quick_random
  gcryctl_set_random_seed_file
  gcryctl_update_random_seed_file
  gcryctl_set_thread_cbs
  gcryctl_fast_poll)

(defconst gcry:cipher_ctl
  (ffi-defun '(function int gcry_cipher_hd_t int (pointer void) unsigned-int)
	     "gcry_cipher_ctl")
  "Generic cipher accessor.")

(defun gcry:cipher-setkey (sc-handle key)
  "Set the key of SC-HANDLE to KEY."
  (when (and (stringp key)
	     (gcry:cipher-handle-p sc-handle))
    (let ((g-cmd (cdr (assq 'gcryctl_set_key gcry:ctl_cmds)))
	  (g-buffer (ffi-create-fo 'c-string key))
	  (g-nbytes (ffi-create-fo 'unsigned-int (length key))))
      (let ((ret
	     (ffi-get
	      (ffi-call-function gcry:cipher_ctl
				 sc-handle g-cmd g-buffer g-nbytes))))
	(when (zerop ret)
	  t)))))

(defun gcry:cipher-setiv (sc-handle iv)
  "Set the initialisation vector of SC-HANDLE to IV."
  (when (and (stringp iv)
	     (gcry:cipher-handle-p sc-handle))
    (let ((g-cmd (cdr (assq 'gcryctl_set_iv gcry:ctl_cmds)))
	  (g-buffer (ffi-create-fo 'c-string iv))
	  (g-nbytes (ffi-create-fo 'unsigned-int (length iv))))
      (let ((ret
	     (ffi-get
	      (ffi-call-function gcry:cipher_ctl
				 sc-handle g-cmd g-buffer g-nbytes))))
	(when (zerop ret)
	  t)))))

(defun gcry:padded-length (string &optional block-length)
  "Return the length of STRING after correct padding to
BLOCK-LENGTH (defaults to 8)."
  (let* ((blklen (or block-length 8))
	 (slen (length string))
	 (blks (1+ (div slen blklen)))
	 (plen (* blks blklen)))
    plen))

(defun gcry:padded-string (string &optional block-length)
  "Return the padded version of STRING after correct padding to
BLOCK-LENGTH (defaults to 8)."
  (let* ((blklen (or block-length 8))
	 (padlen (gcry:padded-length string blklen))
	 (strlen (length string))
	 (defect (- padlen strlen))
	 (pad (make-string defect defect)))
    (concat string pad)))
;; (setq somestring "testffff")
;; (gcry:padded-string somestring)

(defun gcry:unpadded-string (string)
  "Return the unpadded version of STRING assumed a correct padding has
been applied."
  (let* ((strlen (length string))
	 (padchr (char-to-int (aref string (1- strlen))))
	 ;; validate the padding
	 (first-padchr (when (and (positivep padchr)
				  (<= padchr strlen))
			 (char-to-int (aref string (- strlen padchr)))))
	 (pad-valid-p (when first-padchr
			(= first-padchr padchr))))
    (when pad-valid-p
      (substring string 0 (- strlen padchr)))))
;;(gcry:unpadded-string "abcd  ")

;; encryption/decryption routines
(defconst gcry:cipher_encrypt
  (ffi-defun '(function int
			gcry_cipher_hd_t c-data unsigned-int
			c-data unsigned-int)
	     "gcry_cipher_encrypt")
  "Encrypt data under a cipher context.")

(defun gcry:cipher-encrypt (sc-handle plain)
  "Encrypt PLAIN with the settings in SC-HANDLE and return the result."
  (when (and (stringp plain)
	     (gcry:cipher-handle-p sc-handle))
    (let* ((blklen (gcry:cipher-get-block-length (get sc-handle 'cipher-algo)))
	   ;; add openssl conform padding (gcrypt obviously does not care)
	   (plain (gcry:padded-string plain blklen))
	   (outlen (length plain)))

      (let ((g-in (ffi-create-fo (cons 'c-data (1+ outlen)) plain))
	    (g-inlen (ffi-create-fo 'unsigned-int outlen))
	    (g-out (make-ffi-object (cons 'c-data outlen)))
	    (g-outlen (ffi-create-fo 'unsigned-int outlen)))
	(let ((ret
	       (ffi-get
		(ffi-call-function gcry:cipher_encrypt
				   sc-handle g-out g-outlen g-in g-inlen))))
	  (when (zerop ret)
	    (ffi-get g-out)))))))

(defconst gcry:cipher_decrypt
  (ffi-defun '(function int
			gcry_cipher_hd_t c-data unsigned-int
			c-data unsigned-int)
	     "gcry_cipher_decrypt")
  "Decrypt data under a cipher context.")

(defun gcry:cipher-decrypt (sc-handle ciphered)
  "Decrypt CIPHERED with the settings in SC-HANDLE and return the result."
  (when (and (stringp ciphered)
	     (gcry:cipher-handle-p sc-handle))
    (let* (;;(blklen
	   ;; (gcry:cipher-get-block-length (get sc-handle 'cipher-algo)))
	   (outlen (length ciphered)))
      (let ((g-in (ffi-create-fo 'c-data ciphered))
	    (g-inlen (ffi-create-fo 'unsigned-int outlen))
	    (g-out (make-ffi-object (cons 'c-data outlen)))
	    (g-outlen (ffi-create-fo 'unsigned-int outlen)))
	(let ((ret
	       (ffi-get
		(ffi-call-function gcry:cipher_decrypt
				   sc-handle g-out g-outlen g-in g-inlen))))
	  (when (zerop ret)
	    (gcry:unpadded-string
	     (ffi-fetch g-out 0 (cons 'c-data outlen)))))))))


(defconst gcry:cipher_algo_info
  (ffi-defun '(function int int int (pointer void) (pointer unsigned-int))
	     "gcry_cipher_algo_info")
  "Return information generically of a cipher algorithm.")

(defun gcry:cipher-algo-info (cipher-algo which-info)
  "Return information on a specific CIPHER-ALGO.
WHICH-INFO must be one of 'gcryctl_get_keylen, 'gcryctl_get_blklen and
'gcryctl_test_algo."
  (when (or (eq which-info 'gcryctl_get_keylen)
	    (eq which-info 'gcryctl_get_blklen)
	    (eq which-info 'gcryctl_test_algo))
    (let ((g-what (cdr (assq which-info gcry:ctl_cmds)))
	  (g-algo (cond ((stringp cipher-algo)
			(ffi-create-fo 'int (gcry:cipher-map-name cipher-algo)))
		       ((intp cipher-algo)
			(ffi-create-fo 'int cipher-algo))
		       ((and (ffi-object-p cipher-algo)
			     (eq (ffi-object-type cipher-algo) 'int))
			cipher-algo)))
	  (g-buffer (ffi-null-pointer))
	  (g-nbytes (make-ffi-object 'unsigned-int)))
      (let* ((g-nbytes* (if (eq which-info 'gcryctl_test_algo)
			   (ffi-null-pointer)
			 (ffi-address-of g-nbytes)))
	     (ret
	      (ffi-get
	       (ffi-call-function gcry:cipher_algo_info
				  g-algo g-what g-buffer g-nbytes*))))
	(when (zerop ret)
	  (if (eq which-info 'gcryctl_test_algo)
	      t
	    (ffi-get g-nbytes)))))))

;; derived funs
(defun gcry:cipher-get-key-length (cipher)
  "Return the key-length of CIPHER in bytes, or `nil' if an error
has occurred."
  (when cipher
    (gcry:cipher-algo-info cipher 'gcryctl_get_keylen)))

(defun gcry:cipher-get-block-length (cipher)
  "Return the block-length of CIPHER in bytes, or `nil' if an error
has occurred."
  (when cipher
    (gcry:cipher-algo-info cipher 'gcryctl_get_blklen)))

(defun gcry:cipher-available-p (cipher)
  "Return non-`nil' iff CIPHER is available for use."
  (when cipher
    (gcry:cipher-algo-info cipher 'gcryctl_test_algo)))

;;(setq handle (gcry:cipher-open "AES256" 'ecb))
;;(gcry:cipher-algo-info "AES256" 'gcryctl_get_blklen)
;;(gcry:cipher-setkey handle "12345678901234567890123456789012")
;;(gcry:cipher-setiv handle "1234567890123456")
;;(gcry:cipher-available-p "AES256")
;;(gcry:cipher-get-block-length (get handle 'cipher-algo))
;;(setq enc (gcry:cipher-encrypt handle "Hallo dies ist ein Test-Text > 16 Z"))
;;(setq dec (gcry:cipher-decrypt handle enc2))
;;(gcry:cipher-close handle)

;;(setq enc2 (ossl-encrypt 'AES-256-ECB "Hallo dies ist ein Test-Text > 16 Zeichen noch3" "12345678901234567890123456789012" "1234567890123456"))
;;(setq dec2 (ossl-decrypt 'AES-256-ECB enc "12345678901234567890123456789012" "1234567890123456"))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Asymmetric ciphers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'ffi-gcrypt)

;;; ffi-gcrypt.el ends here
