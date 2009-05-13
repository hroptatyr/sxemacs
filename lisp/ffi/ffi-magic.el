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


(define-ffi-type magic_t pointer)

(define-ffi-function magic-open (flag)
  "Call libmagic's magic_open()."
  '(function magic_t int)
  "magic_open")

(define-ffi-function magic-load (magic magicfile)
  "Call libmagic's magic_load()."
  '(function int magic_t c-string)
  "magic_load")

(define-ffi-function magic-file (magic file)
  "Call libmagic's magic_file()."
  '(function c-string magic_t c-string)
  "magic_file")

(define-ffi-function magic-close (magic)
  "Call libmagic's magic_close()."
  '(function void magic_t)
  "magic_close")

;;;###autoload
(defun magic:file-type (file)
  "Return as a string what type FILE is using libmagic."
  (interactive "fFile name: ")
  (let ((magic (magic-open 0))
	(cfile (expand-file-name file)))
    (magic-load magic (ffi-null-pointer))
    (let ((type (magic-file magic cfile)))
      (magic-close magic)
      (if (interactive-p)
          (message type)
        type))))
  
(provide 'ffi-magic)
;;; ffi-magic.el ends here
