;;; coding.el --- Coding-system functions for XEmacs.

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; This file is part of SXEmacs.

;; This file is very similar to mule-coding.el

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

;;; Commentary:

;;; split off of mule.el.

;;; Code:

(defalias 'check-coding-system 'get-coding-system)

(defconst modeline-multibyte-status '("%C")
  "Modeline control for showing multibyte extension status.")

;; override the default value defined in loaddefs.el.
(setq-default modeline-format
	      (cons ""
		    (cons 'modeline-multibyte-status
			  (cdr modeline-format))))

(defun modify-coding-system-alist (target-type regexp coding-system)
  "Modify one of look up tables for finding a coding system on I/O operation.
There are three of such tables, `file-coding-system-alist',
`process-coding-system-alist', and `network-coding-system-alist'.

TARGET-TYPE specifies which of them to modify.
If it is `file', it affects `file-coding-system-alist' (which see).
If it is `process', it affects `process-coding-system-alist' (which see).
If it is `network', it affects `network-coding-system-alist' (which see).

REGEXP is a regular expression matching a target of I/O operation.
The target is a file name if TARGET-TYPE is `file', a program name if
TARGET-TYPE is `process', or a network service name or a port number
to connect to if TARGET-TYPE is `network'.

CODING-SYSTEM is a coding system to perform code conversion on the I/O
operation, or a cons cell (DECODING . ENCODING) specifying the coding systems
for decoding and encoding respectively,
or a function symbol which, when called, returns such a cons cell."
  (or (memq target-type '(file process network))
      (error "Invalid target type: %s" target-type))
  (or (stringp regexp)
      (and (eq target-type 'network) (integerp regexp))
      (error "Invalid regular expression: %s" regexp))
  (if (symbolp coding-system)
      (if (not (fboundp coding-system))
	  (progn
	    (check-coding-system coding-system)
	    (setq coding-system (cons coding-system coding-system))))
    (check-coding-system (car coding-system))
    (check-coding-system (cdr coding-system)))
  (cond ((eq target-type 'file)
	 (let ((slot (assoc regexp file-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq file-coding-system-alist
		   (cons (cons regexp coding-system)
			 file-coding-system-alist)))))
	((eq target-type 'process)
	 (let ((slot (assoc regexp process-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq process-coding-system-alist
		   (cons (cons regexp coding-system)
			 process-coding-system-alist)))))
	(t
	 (let ((slot (assoc regexp network-coding-system-alist)))
	   (if slot
	       (setcdr slot coding-system)
	     (setq network-coding-system-alist
		   (cons (cons regexp coding-system)
			 network-coding-system-alist)))))))

(defsubst keyboard-coding-system ()
  "Return coding-system of what is sent from terminal keyboard."
  keyboard-coding-system)

(defun set-keyboard-coding-system (coding-system)
  "Set the coding system used for TTY keyboard input. Currently broken."
  (interactive "zkeyboard-coding-system: ")
  (get-coding-system coding-system) ; correctness check
  (setq keyboard-coding-system coding-system)
  (if (eq (device-type) 'tty)
      (set-console-tty-input-coding-system
       (device-console) keyboard-coding-system))
  (redraw-modeline t))

(defsubst terminal-coding-system ()
  "Return coding-system of your terminal."
  terminal-coding-system)

(defun set-terminal-coding-system (coding-system)
  "Set the coding system used for TTY display output. Currently broken."
  (interactive "zterminal-coding-system: ")
  (get-coding-system coding-system) ; correctness check
  (setq terminal-coding-system coding-system)
  ; #### should this affect all current tty consoles ?
  (if (eq (device-type) 'tty)
      (set-console-tty-output-coding-system
       (device-console) terminal-coding-system))
  (redraw-modeline t))

(defun set-pathname-coding-system (coding-system)
  "Set the coding system used for file system path names."
  (interactive "zPathname-coding-system: ")
  (get-coding-system coding-system) ; correctness check
  (setq file-name-coding-system coding-system))

(defun what-coding-system (start end &optional arg)
  "Show the encoding of text in the region.
This function is meant to be called interactively;
from a Lisp program, use `detect-coding-region' instead."
  (interactive "r\nP")
  (princ (detect-coding-region start end)))

(defun decode-coding-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM.
Does not modify STR.  Returns the decoded string on successful conversion."
  (with-string-as-buffer-contents
   str (decode-coding-region (point-min) (point-max) coding-system)))

(defun encode-coding-string (str coding-system)
  "Encode the string STR using CODING-SYSTEM.
Does not modify STR.  Returns the encoded string on successful conversion."
  (with-string-as-buffer-contents
   str (encode-coding-region (point-min) (point-max) coding-system)))


;;;; Coding system accessors

(defun coding-system-mnemonic (coding-system)
  "Return the 'mnemonic property of CODING-SYSTEM."
  (coding-system-property coding-system 'mnemonic))

(defalias 'coding-system-docstring 'coding-system-doc-string)

(defun coding-system-eol-type (coding-system)
  "Return the 'eol-type property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-type))

(defun coding-system-eol-lf (coding-system)
  "Return the 'eol-lf property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-lf))

(defun coding-system-eol-crlf (coding-system)
  "Return the 'eol-crlf property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-crlf))

(defun coding-system-eol-cr (coding-system)
  "Return the 'eol-cr property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-cr))

(defun coding-system-post-read-conversion (coding-system)
  "Return the 'post-read-conversion property of CODING-SYSTEM."
  (coding-system-property coding-system 'post-read-conversion))

(defun coding-system-pre-write-conversion (coding-system)
  "Return the 'pre-write-conversion property of CODING-SYSTEM."
  (coding-system-property coding-system 'pre-write-conversion))

(defun coding-system-base (coding-system)
  "Return the base coding system of CODING-SYSTEM."
  (if (not (coding-system-eol-type coding-system))
      coding-system
    (find-coding-system
     (intern
      (substring
       (symbol-name (coding-system-name coding-system))
       0
       (string-match #r"-unix$\|-dos$\|-mac$"
		     (symbol-name (coding-system-name coding-system))))))))

;;;; Definitions of predefined coding systems

(make-coding-system
 'undecided 'undecided
 "Automatic conversion."
 '(mnemonic "Auto"))

;;; Make certain variables equivalent to coding-system aliases
(defun dontusethis-set-value-file-name-coding-system-handler (sym args fun harg handlers)
  (define-coding-system-alias 'file-name (or (car args) 'binary)))

(dontusethis-set-symbol-value-handler
 'file-name-coding-system
 'set-value
 'dontusethis-set-value-file-name-coding-system-handler)

(defun dontusethis-set-value-terminal-coding-system-handler (sym args fun harg handlers)
  (define-coding-system-alias 'terminal (or (car args) 'binary)))

(dontusethis-set-symbol-value-handler
 'terminal-coding-system
 'set-value
 'dontusethis-set-value-terminal-coding-system-handler)

(defun dontusethis-set-value-keyboard-coding-system-handler (sym args fun harg handlers)
  (define-coding-system-alias 'keyboard (or (car args) 'binary)))

(dontusethis-set-symbol-value-handler
 'keyboard-coding-system
 'set-value
 'dontusethis-set-value-keyboard-coding-system-handler)

(unless (boundp 'file-name-coding-system)
  (setq file-name-coding-system nil))

(when (not (featurep 'mule))
  ;; these are so that gnus and friends work when not mule
  (copy-coding-system 'undecided 'iso-8859-1)
  (copy-coding-system 'undecided 'iso-8859-2)
  (copy-coding-system 'undecided 'iso-8859-15)
  (copy-coding-system 'undecided 'emacs-internal)
  (define-coding-system-alias 'ctext 'binary))


;; compatibility for old XEmacsen (don't use it)
(copy-coding-system 'undecided 'automatic-conversion)

(make-compatible-variable 'enable-multibyte-characters "Unimplemented")

(define-obsolete-variable-alias
  'pathname-coding-system 'file-name-coding-system)

;;; coding.el ends here
