;;; ffi-libc.el --- FFI bindings for libc.

;; Copyright (C) 2005 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Keywords: ffi

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; 

;;; Code:
(require 'ffi)

(defconst c:SEEK_SET 0)
(defconst c:SEEK_CUR 1)
(defconst c:SEEK_END 2)

;; libc Bindings
(defconst c:fopen
  (ffi-defun '(function (pointer void) c-string c-string) "fopen")
  "Emacs lisp binding to fopen(3)")
(defconst c:fdopen
  (ffi-defun '(function (pointer void) int c-string) "fdopen")
  "Emacs lisp binding to fdopen(3)")
(defconst c:fread
  (ffi-defun '(function unsigned-int c-string unsigned-int
                        unsigned-int (pointer void)) "fread")
  "Emacs lisp binding to fread(3)")
(defconst c:fwrite
  (ffi-defun '(function unsigned-int c-string unsigned-int
                        unsigned-int (pointer void)) "fwrite")
  "Emacs lisp binding to fwrite(3)")
(defconst c:fseek
  (ffi-defun '(function int (pointer void) long int) "fseek")
  "Emacs lisp binding to fseek(3)")
(defconst c:fclose
  (ffi-defun '(function int (pointer void)) "fclose")
  "Emacs lisp binding to fclose(3)")
(defconst c:strerror
  (ffi-defun '(function c-string int) "strerror")
  "Emacs lisp binding to strerror(3)")

(defconst c:errno (ffi-bind 'int "errno"))

;; libc Functions
(defun c:strerror (error)
  (ffi-get (ffi-call-function c:strerror (ffi-create-fo 'int error))))

(defun c:fopen (file mode)
  "Open the FILE and associates a stream with it.

The argument MODE is a string beginning with one of the following
sequences (Additional characters may follow these sequences.):

``r''   Open text file for reading.  The stream is positioned at the
        beginning of the file.

``r+''  Open for reading and writing.  The stream is positioned at the
        beginning of the file.

``w''   Truncate file to zero length or create text file for writing.
        The stream is positioned at the beginning of the file.

``w+''  Open for reading and writing.  The file is created if it does not
        exist, otherwise it is truncated.  The stream is positioned at
        the beginning of the file.

``a''   Open for writing.  The file is created if it does not exist.  The
        stream is positioned at the end of the file.  Subsequent writes
        to the file will always end up at the then current end of file,
        irrespective of any intervening fseek(3) or similar.

``a+''  Open for reading and writing.  The file is created if it does not
        exist.  The stream is positioned at the end of the file.  Subse-
        quent writes to the file will always end up at the then current
        end of file, irrespective of any intervening fseek(3) or similar.

The mode string can also include the letter ``b'' either as a third char-
acter or as a character between the characters in any of the two-charac-
ter strings described above.  This is strictly for compatibility with
ISO/IEC 9899:1990 (``ISO C90'') and has no effect; the ``b'' is ignored."
  (let ((rv (ffi-call-function
             c:fopen (ffi-create-fo 'c-string file)
             (ffi-create-fo 'c-string mode))))
    (when (ffi-null-p rv)
      (error 'file-error "c:fopen open error"
             file (c:strerror (ffi-get c:errno))))
    rv))

(defun c:fdopen (fd mode)
  "Create stream from descriptor FD using MODE."
  (ffi-call-function
   c:fdopen (ffi-create-fo 'int fd) (ffi-create-fo 'c-string mode)))

(defun c:fclose (fstream)
  "Close FSTREAM."
  (ffi-get (ffi-call-function c:fclose fstream)))

(defun c:fread (fstream len)
  "From FSTREAM, read LEN bytes."
  (let* ((fs (ffi-alloc 'c-string (1+ len)))
         (rv (ffi-get (ffi-call-function
                       c:fread fs (ffi-create-fo 'unsigned-int 1)
                       (ffi-create-fo 'unsigned-int len) fstream))))
    (when (< rv 0)
      (error 'io-error "c:fread reading error" (c:strerror (ffi-get c:errno))))
    ;; Terminating '\0'
    (ffi-store fs rv 'char ?\x0)
    (prog1
        (ffi-get fs)
      (ffi-free fs))))

(defun c:fwrite (fstream str)
  "To FSTREAM write STR."
  (let ((ret (ffi-get (ffi-call-function
                       c:fwrite (ffi-create-fo 'c-string str)
                       (ffi-create-fo 'unsigned-int 1)
                       (ffi-create-fo 'unsigned-int (length str))
                       fstream))))
    (when (< ret 0)
      (error 'io-error "c:fwrite writing error" (c:strerror (ffi-get c:errno))))
    ret))

(provide 'ffi-libc)

;;; ffi-libc.el ends here
