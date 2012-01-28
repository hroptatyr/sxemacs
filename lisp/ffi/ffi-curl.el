;;; ffi-curl.el --- Emacs interface to libcurl.

;; Copyright (C) 2005-2009 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: ffi, curl, ftp

;; This file is part of SXEmacs.

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

;;; Synched up with: Not in FSF

;;; Commentary:

;;

;;; Code:

(require 'ffi)
(require 'ffi-libc)

(ffi-load "libcurl")

;;{{{ Low-level FFI: types and functions

(define-ffi-enum curl-option
  (:fstream 10001)                      ; FILE* stream to write to
  (:url 10002)                          ; full URL to get/put
  (:port 3)                             ; port number to connect to
  (:write-function 20011)
  (:read-function 20012)
  (:timeout 13)                         ; read timeout in seconds
  (:post-fields 10015)                  ; POST static input fields
  (:header 42)                          ; throw the header out too
  (:nobody 44)                          ; use HEAD to get http document
  (:post 47)                            ; HTTP POST method
  (:nosignal 99)
  )

(define-ffi-enum curl-info
  (:effective-url  #x100001)
  (:response-code  #x200002)
  (:header-size    #x20000b)
  (:content-type   #x100012)
  (:size-download  #x300008)
  (:speed-download #x300009))

(cffi:defcfun ("curl_easy_init" curl:curl_easy_init) pointer)
(cffi:defcfun ("curl_easy_cleanup" curl:curl_easy_cleanup) void
  (handler pointer))
(cffi:defcfun ("curl_easy_perform" curl:curl_easy_perform) int
  (handler pointer))

(cffi:defcfun ("curl_easy_setopt" curl:curl_easy_setopt) int
  (handler pointer) (opt curl-option) &rest)
(cffi:defcfun ("curl_easy_getinfo" curl:curl_easy_getinfo) int
  (handler pointer) (info curl-info) &rest)

;;}}}

;;{{{ Errors list

(defconst curl:errors-alist
  '((1 . "Unsupported protocol")
    (2 . "Failed init")
    (3 . "Malformated URL")
    (4 . "NOT USED")
    (5 . "Could not resolve proxy")
    (6 . "Could not resolve host")
    (7 . "Could not connect")
    (8 . "FTP weird server reply")
    (9 . "FTP access denied")
    (10 . "FTP user or password is incorrect")
    (11 . "FTP weird PASS reply")
    (12 . "FTP weird USER reply")
    (13 . "FTP weird PASV reply")
    (14 . "FTP weird 227 format")
    (15 . "FTP can't get host")
    (16 . "FTP can't reconnect")
    (17 . "FTP could not set binary")
    (18 . "partial file")
    (19 . "FTP could not RETR file")
    (20 . "FTP write error")
    (21 . "FTP quote error")
    (22 . "HTTP returned errror")
    (23 . "write error")
    (24 . "NOT USED")
    (25 . "failed FTP upload")
    (26 . "could open/read from file")
    (27 . "Out of memory")
    (28 . "the timeout time was reached")
    (29 . "TYPE A failed")
    (30 . "FTP PORT operation failed")
    (31 . "the REST command failed")
    (32 . "the SIZE command failed")
    (33 . "RANGE \"command\" didn't work")
    (34 . "HTTP port error")
    (35 . "wrong when connecting with SSL")
    (36 . "couldn't resume download")
    (37 . "FILE could not read file")
    (38 . "LDAP cannot bind")
    (39 . "LDAP search failed")
    (40 . "library not found")
    (41 . "function not found")
    (42 . "aborted by callback")
    (43 . "bad function argument")
    (44 . "NOT USED")
    (45 . "CURLOPT_INTERFACE failed")
    (46 . "NOT USED")
    (47 . "catch endless re-direct loops")
    (48 . "User specified an unknown option")
    (49 . "Malformed telnet option")
    (50 . "NOT USED")
    (51 . "peer's certificate wasn't ok")
    (52 . "when this is a specific error")
    (53 . "SSL crypto engine not found")
    (54 . "can not set SSL crypto engine as default")
    (55 . "failed sending network data")
    (56 . "failure in receiving network data")
    (57 . "share is in use")
    (58 . "problem with the local certificate")
    (59 . "couldn't use specified cipher")
    (60 . "problem with the CA cert (path?)")
    (61 . "Unrecognized transfer encoding")
    (62 . "Invalid LDAP URL")
    (63 . "Maximum file size exceeded")
    (64 . "Requested FTP SSL level failed"))
  "Alist of error codes and associated clear-text error messages.")

;;}}}

;;{{{ High level API

(defun curl:easy-init ()
  "Initialize curl easy interface and return a context handle."
  (let ((ret (curl:curl_easy_init)))
    (when (ffi-null-p ret)
      (error "curl:easy-init: Can't init easy interface"))
    ret))

(defun curl:easy-cleanup (ctx)
  "Clean up context CTX and free resources allocated with it.
This function must be called after every easy session."
  (curl:curl_easy_cleanup ctx)
  ;; Remove references to saved values
  (remprop ctx 'saved-values))

(defun curl:easy-setopt (ctx &rest options)
  "Set OPTIONS for curl transfer.
Options are passed as keyword-value-pairs. Supported keywords are:
:url string - a valid Uniform Resource Locator.
:fstream ffi-fo - a file descriptor to which output is redirected."
  (while options
    (let ((option (car options))
	  (value (cadr options))
	  error)
      ;; Handle special cases in options
      (case option
	((:url :post-fields)
	 (unless (stringp value)
	   (error 'invalid-argument
		  "curl:easy-setopt invalid option value(must be string)"
		  option value))
	 (setq value (ffi-create-fo 'c-string value))
	 ;; Keep reference to value until context is destroyed
	 (push value (get ctx 'saved-values)))

	((:read-function :write-function)
	 (setq value (ffi-callback-fo value)))

	((:nobody :header :post :nosignal)
	 (setq value (ffi-create-fo 'int (if value 1 0)))))

      (setq error (curl:curl_easy_setopt ctx option value))
      (unless (zerop error)
	(error 'invalid-operation "curl:easy-setopt error" error))

      (setq options (cddr options)))))

(defun curl:easy-perform (ctx)
  "Perform cURL operation on the context CTX.
To control the behaviour of the session or set options into the
context, see `curl:easy-setopt'."
  (let ((err (curl:curl_easy_perform ctx)))
    (unless (zerop err)
      (error 'invalid-operation "curl:easy-perform error"
	     (cdr (assq err curl:errors-alist))))
    err))

(defun curl:easy-perform& (ctx sentinel fs)
  "Perform cURL operation on CTX, return a worker job object.
Afterwards run SENTINEL.

The original (curl) context CTX is stored in the plist of the worker job
object with key 'ctx to keep it accessible."
  (if (featurep 'workers)
      (let* ((job (ffi-call-function&
		   (get 'curl:curl_easy_perform 'ffi-fun)
		   ctx sentinel fs ctx)))
	;; add ctx to plist of job
	(put job 'ctx ctx)
	job)
    (error 'unimplemented "Asynchronous Event Queues")))

(defun curl:easy-perform-sentinel (job fs ctx)
  (curl:easy-cleanup ctx)
  (unless (car fs) (c:fclose (cdr fs)))
  (run-hook-with-args 'curl:download&-post-hook job))

(defun curl:easy-getinfo (ctx what)
  "Get info from the context CTX about WHAT."
  (let* ((ival (cdr (assq what (ffi-enum-values 'curl-info))))
	 (itype (if (not (numberp ival))
		    (error "Unsupported info" what)
		  (ecase (lsh (logand #xf00000 ival) -20)
		    (1 'c-string) (2 'long) (3 'double))))
	 (retfo (make-ffi-object itype)))
    (unless (zerop (curl:curl_easy_getinfo
		    ctx what (ffi-address-of retfo)))
      (error 'invalid-operation "curl:easy-getinfo error"))
    (ffi-get retfo)))

(defvar curl:download-history nil
  "History for `curl:download' and `curl:download&'.")

(define-ffi-callback curl:cb-write-to-buffer int
  ((ptr pointer) (size int) (nmemb int) (stream pointer))
  "Writer to STREAM buffer."
  (let ((buf (ffi-pointer-to-lisp-object stream))
	(rsz (* size nmemb)))
    (when (and (positivep rsz) (buffer-live-p buf))
      (with-current-buffer buf
	(insert (ffi-get ptr :type (cons 'c-data rsz)))))
    rsz))

;;;###autoload
(defun curl:download (url file-or-buffer &rest options)
  "Download the contents of URL and write them to FILE-OR-BUFFER.

Optionally you can specify keywords in OPTIONS.  The options are
keyword-value-pairs and are set via `curl:easy-setopt'.

When called interactively you can choose, with a prefix arg, to download
the HTTP header instead of the actual remote file.  Obviously this only
works with HTTP URLs."
  (interactive
   (list (read-string "URL: " (and-fboundp #'ffap-url-at-point
				(ffap-url-at-point))
		      curl:download-history)
	 (read-file-name "Local file: " default-directory
			 (expand-file-name (make-temp-name "curl:downloaded:")
					   (temp-directory)))))
  (when current-prefix-arg
    ;; In case of C-u
    (and (y-or-n-p (format "Only download %s's HTTP header? "
			   (file-basename file-or-buffer)))
	 (setq options (list :header t :nobody t))))

  (let* ((ctx (curl:easy-init))
	 (bufferp (bufferp file-or-buffer))
	 (fs (if bufferp
		 (ffi-lisp-object-to-pointer file-or-buffer)
	       (c:fopen (expand-file-name file-or-buffer) "w"))))
    (unwind-protect
	(progn
	  (when bufferp
	    (curl:easy-setopt ctx :write-function 'curl:cb-write-to-buffer))

	  ;; Avoid signalling!
	  (curl:easy-setopt ctx :nosignal t)

	  (apply #'curl:easy-setopt ctx :fstream fs :url url options)
	  (curl:easy-perform ctx))
      (unless bufferp (c:fclose fs))
      (curl:easy-cleanup ctx))))

;;;###autoload
(defun curl:download& (url file-or-buffer &rest options)
  "Download the contents of URL and write them to FILE asynchronously.

Optionally you can specify keywords in OPTIONS.  The options are
keyword-value-pairs and are set via `curl:easy-setopt'.

When called interactively you can choose, with a prefix arg, to download
the HTTP header instead of the actual remote file.  Obviously this only
works with HTTP URLs.

After the download operation succeeded the hook `curl:download&-post-hook'
is run.  Functions in there will be called with an argument JOB."
  (interactive
   (list (read-string "URL: " (and-fboundp #'ffap-url-at-point
				(ffap-url-at-point))
		      curl:download-history)
	 (read-file-name "Local file: " default-directory
			 (expand-file-name (make-temp-name "curl:downloaded:")
					   (temp-directory)))))
  (when current-prefix-arg
    (and (y-or-n-p (format "Only download %s's HTTP header? "
			   (file-basename file-or-buffer)))
	 (setq options (list :header t :nobody t))))

  (if (featurep 'workers)
      (let* ((ctx (curl:easy-init))
	     (bufferp (bufferp file-or-buffer))
	     (fs (if bufferp
		     (ffi-lisp-object-to-pointer file-or-buffer)
		   (c:fopen (expand-file-name file-or-buffer) "w"))))
	(condition-case cerr
	    (progn
	      (when bufferp
		(curl:easy-setopt ctx :write-function 'curl:cb-write-to-buffer))

	      ;; Avoid signalling!
	      (curl:easy-setopt ctx :nosignal t)

	      (apply #'curl:easy-setopt ctx :fstream fs :url url options)
	      (curl:easy-perform& ctx #'curl:easy-perform-sentinel
				  (cons bufferp fs)))

	  ;; Close FS, cleanup CTX and resignal error
	  (t (unless bufferp (c:fclose fs))
	     (curl:easy-cleanup ctx)
	     (signal (car cerr) (cdr cerr)))))
    (error 'unimplemented "Asynchronous Event Queues")))

;;;###autoload
(defvar curl:download&-post-hook nil
  "*Hook run after a `curl:download&' call.
Functions in here are called with one argument JOB containing
the job which just finished.")

;;}}}


(provide 'ffi-curl)

;;; ffi-curl.el ends here
