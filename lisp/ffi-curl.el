;;; ffi-curl.el --- Emacs interface to libcurl.

;; Copyright (C) 2005 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: ffi, curl, ftp

;; This file is part of SXEmacs.

;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SXEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:

(require 'ffi)
(require 'ffi-libc)

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
    (64 . "Requested FTP SSL level failed")))


(defvar curl:context nil
  "CURL context.
Internal variable, do not modify.")

;; exported ffi objects for curl easy functions
(defvar curl:curl_easy_setopt nil)
(defvar curl:curl_easy_getinfo nil)
(defvar curl:curl_easy_perform nil)


(defun curl:easy-init ()
  "Initialize curl easy interface."
  (let ((cei (ffi-defun '(function (pointer void)) "curl_easy_init")))
    (ffi-call-function cei)))

(defvar curl:current-url nil)

(defun curl:easy-setopt (&rest options)
  "Set OPTIONS for curl transfer."
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
           (setq value (let ((fo (ffi-alloc 'c-string (length val))))
                         (ffi-set fo val) fo))

           ;; Comment from curl_easy_setopt for CURLOPT_URL:
           ;;  The actual URL to deal with. The parameter should be a char * to
           ;;  a  zero  terminated string. The string must remain present until
           ;;  curl no longer needs it, as it doesn't copy the string.
           (when (ffi-object-p curl:current-url)
             (ffi-free curl:current-url))
           (setq curl:current-url value))

          (:fstream
           (ffi-set option 10001)
           (setq value val)))
           
        (setq error (ffi-get (ffi-call-function
                              curl:curl_easy_setopt curl:context option value)))
        (unless (zerop error)
          (error 'invalid-operation "curl:easy-setopt error" error))

        (setq options (cddr options))))))

(defun curl:easy-perform ()
  "Performe CURL operation."
  (let ((err (ffi-get (ffi-call-function curl:curl_easy_perform curl:context))))
    (unless (zerop err)
      (error 'invalid-operation "curl:easy-perform error"
             (cdr (assq err curl:errors-alist))))))

(defun curl:easy-getinfo-internal (type subcode)
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
                     curl:curl_easy_getinfo curl:context iop
                     (ffi-address-of retfo))))
      (error 'invalid-operation "curl:easy-getinfo error"))
    (ffi-get retfo)))
     
(defun curl:easy-getinfo (what)
  "Get info about WHAT."
  (ecase what
    (:effective-url
     (curl:easy-getinfo-internal 'c-string 1))
    (:response-code
     (curl:easy-getinfo-internal 'long 2))
    (:header-size
     (curl:easy-getinfo-internal 'long 11))
    (:request-size
     (curl:easy-getinfo-internal 'long 12))
    (:content-type
     (curl:easy-getinfo-internal 'c-string 18))
    (:speed-download
     (curl:easy-getinfo-internal 'double 9))
    (:size-download
     (curl:easy-getinfo-internal 'double 8))))

;;;###autoload
(defun curl:download (url file)
  "Download URL to and write data to FILE."
  (let ((fs (c:fopen file "w")))
    (curl:easy-setopt :fstream fs :url url)
    (curl:easy-perform)
    (c:fclose fs)))


(provide 'ffi-curl)

;; On-load actions:
(ffi-load "libcurl.so")

(setq curl:context (curl:easy-init))
(setq curl:curl_easy_setopt
      (ffi-defun '(function int (pointer void) int) "curl_easy_setopt"))
(setq curl:curl_easy_perform
      (ffi-defun '(function int (pointer void)) "curl_easy_perform"))
(setq curl:curl_easy_getinfo
      (ffi-defun '(function int (pointer void) int) "curl_easy_getinfo"))

;;; ffi-curl.el ends here
