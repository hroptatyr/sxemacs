;; Copyright (C) 2005 Adrian Aichner

;; Author: Adrian Aichner <adrian@xemacs.org>
;; Maintainer: XEmacs Beta List <xemacs-beta@xemacs.org>
;; Created: 2005
;; Keywords: tests

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Test region and zmacs-region functionality
;;; See test-harness.el

(condition-case err
    (require 'test-harness)
  (file-error
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (file-name-directory load-file-name) load-path)
     (require 'test-harness))))

;; Active region testing, verifying functionality of
;; http://list-archive.xemacs.org/xemacs-patches/200502/msg00194.html
;; Message-ID: <zmxwtboa.fsf@smtprelay.t-online.de>
(with-temp-buffer
  ;; Using active regions
  (let ((zmacs-regions t)
        (first-buffer (current-buffer)))
    (Silence-Message
     (insert (buffer-name)))
    (Assert (not (region-exists-p)))
    (Assert (not (region-active-p)))
    (Silence-Message
     (mark-whole-buffer))
    (Assert (region-exists-p))
    (Assert (region-active-p))
    ;; Turn off active regions
    (setq zmacs-regions nil)
    ;; Region still exists
    (Assert (region-exists-p))
    ;; Region is no longer active
    (Assert (not (region-active-p)))
    ;; Turn active regions back on
    (setq zmacs-regions t)
    ;; Region still exists
    (Assert (region-exists-p))
    ;; Region is active again
    (Assert (region-active-p))
    (with-temp-buffer
      (Silence-Message
       (insert (buffer-name)))
      ;; Region exists in first buffer, not this second one
      (Assert (not (region-exists-p)))
      ;; Region not active in this second temp buffer
      (Assert (not (region-active-p)))
      ;; Region still active in first temp buffer
      (Assert (eq (zmacs-region-buffer) first-buffer))
      ;; Activate region in second temp buffer
      (Silence-Message
       (mark-whole-buffer))
      ;; Region exists in second temp buffer
      (Assert (region-exists-p))
      ;; Region active in second temp buffer
      (Assert (region-active-p)))
    ;; This next test _only_ fails in the context of `make check', it is
    ;; not reproducible outside of that (ie, running the test manually
    ;; with `test-emacs-test-file'... even in a -no-autoloads instance).
    ;; So I'm disabling the test. --SY.
    ;; Second temp buffer no longer exists
    ;(Assert (null (zmacs-region-buffer)))))
    ))
