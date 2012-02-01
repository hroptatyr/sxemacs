;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Created: 1998
;; Keywords: tests

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; SXEmacs is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test basic md5 functionality.
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))

(defconst md5-tests
  '(
    ;; Test samples from rfc1321:
    ("" . "d41d8cd98f00b204e9800998ecf8427e")
    ("a" . "0cc175b9c0f1b6a831c399e269772661")
    ("abc" . "900150983cd24fb0d6963f7d28e17f72")
    ("message digest" . "f96b697d7cb7938d525a2f31aaf161d0")
    ("abcdefghijklmnopqrstuvwxyz" . "c3fcd3d76192e4007dfb496cca67e13b")
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
     . "d174ab98d277d9f5a5611c2c9f419d9f")
    ("12345678901234567890123456789012345678901234567890123456789012345678901234567890"
     . "57edf4a22be3c955ac49da2e2107b67a")))

;;-----------------------------------------------------
;; Test `md5' on strings
;;-----------------------------------------------------

(mapcar (lambda (x)
	  (Assert-Equal (md5 (car x)) (cdr x)))
	md5-tests)

;;-----------------------------------------------------
;; Test `md5' on portions of strings
;;-----------------------------------------------------

(let ((large-string (mapconcat #'car md5-tests "")))
  (let ((count 0))
    (mapcar (lambda (x)
	      (Assert-Equal (md5 large-string count (+ count (length (car x))))
			     (cdr x))
	      (incf count (length (car x))))
	    md5-tests)))

;;-----------------------------------------------------
;; Test `md5' on buffer
;;-----------------------------------------------------

(with-temp-buffer
  (mapcar (lambda (x)
	    (erase-buffer)
	    (insert (car x))
	    (Assert-Equal (md5 (current-buffer)) (cdr x)))
	  md5-tests))

;;-----------------------------------------------------
;; Test `md5' on portions of buffer
;;-----------------------------------------------------

(with-temp-buffer
  (insert (mapconcat #'car md5-tests ""))
  (let ((point 1))
    (mapcar (lambda (x)
	      (Assert-Equal (md5 (current-buffer) point (+ point (length (car x))))
			     (cdr x))
	      (incf point (length (car x))))
	    md5-tests)))
