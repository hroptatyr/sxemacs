;;; ffi-curl.el --- Emacs interface to libcurl.

<<<<<<< HEAD
;; Copyright (C) 2005 by Zajcev Evgeny.
=======
;; Copyright (C) 2005-2009 by Zajcev Evgeny.
>>>>>>> origin/master

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

<<<<<<< HEAD
;; 
=======
;;
>>>>>>> origin/master

;;; Code:

(require 'ffi)
(require 'ffi-libc)

<<<<<<< HEAD
=======
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

>>>>>>> origin/master
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

<<<<<<< HEAD

;; exported ffi objects for curl easy functions
(defvar curl:curl_easy_init nil)
(defvar curl:curl_easy_cleanup nil)
(defvar curl:curl_easy_setopt nil)
(defvar curl:curl_easy_getinfo nil)
(defvar curl:curl_easy_perform nil)


(defun curl:easy-init ()
  "Initialize curl easy interface and return a context handle."
  (let ((ret (ffi-call-function curl:curl_easy_init)))
=======
;;}}}

;;{{{ High level API

(defun curl:easy-init ()
  "Initialize curl easy interface and return a context handle."
  (let ((ret (curl:curl_easy_init)))
>>>>>>> origin/master
    (when (ffi-null-p ret)
      (error "curl:easy-init: Can't init easy interface"))
    ret))

(defun curl:easy-cleanup (ctx)
  "Clean up context CTX and free resources allocated with it.
This function must be called after every easy session."
<<<<<<< HEAD
  (ffi-call-function curl:curl_easy_cleanup ctx)
  ;; Release url fo if any
  (remprop ctx 'url-fo))
=======
  (curl:curl_easy_cleanup ctx)
  ;; Remove references to saved values
  (remprop ctx 'saved-values))
>>>>>>> origin/master

(defun curl:easy-setopt (ctx &rest options)
  "Set OPTIONS for curl transfer.
Options are passed as keyword-value-pairs. Supported keywords are:
:url string - a valid Uniform Resource Locator.
:fstream ffi-fo - a file descriptor to which output is redirected."
<<<<<<< HEAD
  (let ((option (make-ffi-object 'int)))
    (while options
      (let ((opt (car options))
            (val (cadr options))
            error value)
        (ecase opt
          (:url
           (unless (stringp val)
             (error 'invalid-argument
                    "curl:easy-setopt invalid option value(must be string)"
                    opt val))
           (ffi-set option 10002)
           (setq value (ffi-create-fo 'c-string val))
           ;; Comment from curl_easy_setopt for CURLOPT_URL:
           ;;   The actual URL to deal with. The parameter should be a char * to
           ;;   a  zero  terminated string. The string must remain present until
           ;;   curl no longer needs it, as it doesn't copy the string.
           ;; 
           ;; So we put it as property to CTX and will remove it
           ;; lately just after cleaning up curl context CTX
           (put ctx 'url-fo value))

          (:fstream
           (ffi-set option 10001)
           (setq value val))

          (:nobody
           (ffi-set option 44)
           (setq value (ffi-create-fo 'int (if val 1 0))))
          
          (:header
           (ffi-set option 42)
           (setq value (ffi-create-fo 'int (if val 1 0)))))
           
        (setq error (ffi-get (ffi-call-function
                              curl:curl_easy_setopt ctx option value)))
        (unless (zerop error)
          (error 'invalid-operation "curl:easy-setopt error" error))

        (setq options (cddr options))))))
=======
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

        ((:nobody :header :post)
         (setq value (ffi-create-fo 'int (if value 1 0)))))

      (setq error (curl:curl_easy_setopt ctx option value))
      (unless (zerop error)
        (error 'invalid-operation "curl:easy-setopt error" error))

      (setq options (cddr options)))))
>>>>>>> origin/master

(defun curl:easy-perform (ctx)
  "Perform cURL operation on the context CTX.
To control the behaviour of the session or set options into the
context, see `curl:easy-setopt'."
<<<<<<< HEAD
  (let ((err (ffi-get (ffi-call-function curl:curl_easy_perform ctx))))
=======
  (let ((err (curl:curl_easy_perform ctx)))
>>>>>>> origin/master
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
<<<<<<< HEAD
		   curl:curl_easy_perform ctx sentinel fs ctx)))
=======
		   (get 'curl:curl_easy_perform 'ffi-fun)
                   ctx sentinel fs ctx)))
>>>>>>> origin/master
	;; add ctx to plist of job
	(put job 'ctx ctx)
	job)
    (error 'unimplemented "Asynchronous Event Queues")))

(defun curl:easy-perform-sentinel (job fs ctx)
  (curl:easy-cleanup ctx)
<<<<<<< HEAD
  (c:fclose fs)
  (run-hook-with-args 'curl:download&-post-hook job))

(defun curl:easy-getinfo-internal (ctx type subcode)
  "Internal function to obtain information from curl of TYPE and SUBCODE."
  (let ((iop (make-ffi-object 'int))
        (retfo (make-ffi-object type)))
    (ffi-set iop (+ (ecase type
                      (c-string #x100000)
                      (long #x200000)
                      (double #x300000))
                    subcode))
    (unless (zerop (ffi-get
                    (ffi-call-function
                     curl:curl_easy_getinfo ctx iop
                     (ffi-address-of retfo))))
      (error 'invalid-operation "curl:easy-getinfo error"))
    (ffi-get retfo)))
     
(defun curl:easy-getinfo (ctx what)
  "Get info from the context CTX about WHAT."
  (ecase what
    (:effective-url
     (curl:easy-getinfo-internal ctx 'c-string 1))
    (:response-code
     (curl:easy-getinfo-internal ctx 'long 2))
    (:header-size
     (curl:easy-getinfo-internal ctx 'long 11))
    (:request-size
     (curl:easy-getinfo-internal ctx 'long 12))
    (:content-type
     (curl:easy-getinfo-internal ctx 'c-string 18))
    (:speed-download
     (curl:easy-getinfo-internal ctx 'double 9))
    (:size-download
     (curl:easy-getinfo-internal ctx 'double 8))))
=======
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
>>>>>>> origin/master

(defvar curl:download-history nil
  "History for `curl:download' and `curl:download&'.")

<<<<<<< HEAD
;;;###autoload
(defun curl:download (url file &rest options)
  "Download the contents of URL and write them to FILE.
=======
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
>>>>>>> origin/master

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
<<<<<<< HEAD
  (let ((fs (c:fopen (expand-file-name file) "w"))
        (ctx (curl:easy-init)))
    (when current-prefix-arg
      (and (y-or-n-p (format "Only download %s's HTTP header? "
			     (file-basename file)))
	   (setq options (list :header t :nobody t))))
    (apply #'curl:easy-setopt ctx :fstream fs :url url options)
    (prog1
        (curl:easy-perform ctx)
      (curl:easy-cleanup ctx)
      (c:fclose fs))))

;;;###autoload
(defun curl:download& (url file &rest options)
=======
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
          (apply #'curl:easy-setopt ctx :fstream fs :url url options)
          (curl:easy-perform ctx))
      (unless bufferp (c:fclose fs))
      (curl:easy-cleanup ctx))))

;;;###autoload
(defun curl:download& (url file-or-buffer &rest options)
>>>>>>> origin/master
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
<<<<<<< HEAD
  (if (featurep 'workers)
      (let* ((fs (c:fopen file "w"))
	     (ctx (curl:easy-init)))
	(when current-prefix-arg
	  (and (y-or-n-p (format "Only download %s's HTTP header? "
				 (file-basename file)))
	       (setq options (list :header t :nobody t))))
	(apply #'curl:easy-setopt ctx :fstream fs :url url options)
	(curl:easy-perform& ctx #'curl:easy-perform-sentinel fs))
=======
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
              (apply #'curl:easy-setopt ctx :fstream fs :url url options)
              (curl:easy-perform& ctx #'curl:easy-perform-sentinel
                                  (cons bufferp fs)))

          ;; Close FS, cleanup CTX and resignal error
          (t (unless bufferp (c:fclose fs))
             (curl:easy-cleanup ctx)
             (signal (car cerr) (cdr cerr)))))
>>>>>>> origin/master
    (error 'unimplemented "Asynchronous Event Queues")))

;;;###autoload
(defvar curl:download&-post-hook nil
  "*Hook run after a `curl:download&' call.
Functions in here are called with one argument JOB containing
the job which just finished.")

<<<<<<< HEAD

(provide 'ffi-curl)

;; On-load actions:
(ffi-load "libcurl")

(setq curl:curl_easy_init
      (ffi-defun '(function (pointer void) void) "curl_easy_init")
      curl:curl_easy_cleanup
      (ffi-defun '(function void (pointer void)) "curl_easy_cleanup")
      curl:curl_easy_setopt
      (ffi-defun '(function int (pointer void) int) "curl_easy_setopt")
      curl:curl_easy_perform
      (ffi-defun '(function int (pointer void)) "curl_easy_perform")
      curl:curl_easy_getinfo
      (ffi-defun '(function int (pointer void) int) "curl_easy_getinfo"))

=======
;;}}}


(provide 'ffi-curl)

>>>>>>> origin/master
;;; ffi-curl.el ends here
