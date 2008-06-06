;; ffi-magic.el --- Lisp bindings into file(1)'s libmagic.so   -*- Emacs-Lisp -*-

;; Copyright (C) 2008 Steve Youngs

;; Author:     Steve Youngs <steve@sxemacs.org>
;; Maintainer: Steve Youngs <steve@sxemacs.org>
;; Created:    <2008-04-02>
;; Homepage:   http://www.sxemacs.org
;; Keywords:   ffi, file, magic, extension

;; This file is part of SXEmacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; 
;;    Mimic file(1)'s basic usage.  At the moment, this is quite raw
;;    and single-minded.  It will only use the default magic db and
;;    doesn't allow use of any of file(1)'s options.
;;
;;    (magic:file-type "~/.sxemacs/init.el")
;;     => "Lisp/Scheme program text"

;;; Todo:
;;
;;    o Optionally output MIME type strings like "text/plain",
;;      "applicaton/octet-stream"
;;
;;    o Other options from file(1).

;;; Code:
(require 'ffi)
(require 'ffi-libc)

;; Can't do anything without this
(ffi-load "libmagic")


(define-ffi-type magic_t (pointer void))

(defvar magic-open
  (ffi-defun '(function magic_t int) "magic_open")
  "FFI object for libmagic's magic_open().")

(defun magic-open (flag)
  "Call libmagic's magic_open()."
  (ffi-call-function magic-open flag))

(defvar magic-load
  (ffi-defun '(function int (pointer magic_t) c-string) "magic_load")
  "FFI object for libmagic's magic_load().")

(defun magic-load (magic &optional magicfile)
  "Call libmagic's magic_load()."
  (ffi-call-function magic-load magic magicfile))

(defvar magic-file
  (ffi-defun '(function c-string (pointer magic_t) c-string) "magic_file")
  "FFI object for libmagic's magic_file().")

(defun magic-file (magic file)
  "Call libmagic's magic_file()."
  (ffi-call-function magic-file magic file))

(defvar magic-close
  (ffi-defun '(function void (pointer magic_t)) "magic_close")
  "FFI object for libmagic's magic_close().")

(defun magic-close (magic)
  "Call libmagic's magic_close()."
  (ffi-call-function magic-close magic))

(defun magic:file-type (file)
  "Return as a string what type FILE is using libmagic."
  (interactive "fFile name: ")
  (let ((magic (magic-open (ffi-create-fo 'int 0)))
	(cfile (ffi-create-fo 'c-string (expand-file-name file)))
	type)
    (magic-load magic (ffi-null-pointer))
    (setq type (magic-file magic cfile))
    (setq type (ffi-get type))
    (magic-close magic)
    (if (interactive-p)
	(message type)
      type)))
  
(provide 'ffi-magic)
;;; ffi-magic.el ends here
