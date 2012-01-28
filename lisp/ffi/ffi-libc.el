;;; ffi-libc.el --- FFI bindings for libc.

;; Copyright (C) 2005-2008 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 2005
;; Keywords: ffi

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

(define-ffi-type c:FILE pointer)

(define-ffi-enum c:WHENCE
  (seek-set 0)
  (seek-cur 1)
  (seek-end 2))

;;; FILE operations
(defconst c:errno (ffi-bind 'int "errno")
  "Current errno value.
Get lisp value for it with `(ffi-get c:errno)'.")

(cffi:defcfun ("fopen" c:fopen-1) c:FILE
  "Elisp binding to fopen(3).
Consider using `c:fopen' instead."
  (filename c-string) (mode c-string))

(cffi:defcfun ("fdopen" c:fdopen) c:FILE
  "Create stream from descriptor FD using MODE."
  (fd int) (mode c-string))

(cffi:defcfun ("clearerr" c:clearerr) void
  "Clear the end-of-file and error indicators for the STREAM."
  (stream c:FILE))

(cffi:defcfun ("ferror" c:ferror-p) boolean
  "Return non-nil if STREAM has errors.
Tests the error indicator for the stream pointed to by stream,
returning non-zero if it is set.  The error indicator can only be
reset by the `c:clearerr' function."
  (stream c:FILE))

(cffi:defcfun ("feof" c:feof-p) boolean
  "Return non-nil if STREAM has end-of-file indicator set.
The end-of-file indicator can only be cleared by the function
`c:clearerr'."
  (stream c:FILE))

(cffi:defcfun ("fread" c:fread-1) unsigned-int
  "Elisp binding to fread(3).
Consider using `c:fread' in your programs."
  (ptr pointer) (size unsigned-int) (nmemb unsigned-int)
  (stream c:FILE))

(defun c:fread (size stream)
  "Read at most SIZE bytes from STREAM.
STREAM is object returned by call to `c:fopen' function.
Empty stream is returned if end-of-file indicated.
Error raises if some error occurs."
  (let* ((fod (make-ffi-object (cons 'c-data size)))
	 (rsz (c:fread-1 fod 1 size stream)))
    (if (zerop rsz)
	(cond ((c:feof-p stream) "")
	      ((c:ferror-p stream)
	       (error 'io-error "c:fread error"
		      (c:strerror (ffi-get c:errno))))
	      (t (error 'io-error "c:fread unknown error")))
      (ffi-get fod :type (cons 'c-data rsz)))))

(cffi:defcfun ("fwrite" c:fwrite-1) unsigned-int
  "Elisp binding to fwrite(3).
Consider using `c:fwrite' in your programs."
  (ptr pointer) (size unsigned-int) (nmemb unsigned-int)
  (stream c:FILE))

(defun c:fwrite (data stream)
  "Write DATA to STREAM.
DATA must be an Emacs lisp string.
STREAM is object returned by call to `c:fopen' function."
  (let ((fod (ffi-create-fo (cons 'c-data (length data)) data)))
    (c:fwrite-1 fod 1 (length data) stream)))

(cffi:defcfun ("fseek" c:fseek) int
  "Set the file position indicator for the STREAM.
The new position, measured in bytes, is obtained by adding OFFSET
bytes to the position specified by WHENCE.
WHENCE is one of:
 'seek-set   - relative to the start of file.
 'seek-cur   - relative to the current position.
 'seek-end   - relative to the end of file."
  (stream c:FILE) (offset long) (whence c:WHENCE))

(cffi:defcfun ("fclose" c:fclose) int
  "Close STREAM.
Elisp binding to close(3)."
  (stream c:FILE))

(cffi:defcfun ("strerror" c:strerror) c-string
  "Emacs lisp binding to strerror(3)"
  (error int))

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
  (let ((rv (c:fopen-1 file mode)))
    (when (ffi-null-p rv)
      (error 'file-error "c:fopen open error"
	     file (c:strerror (ffi-get c:errno))))
    rv))

(cffi:defcfun ("dup" c:dup) int
  "Duplicate existing file descriptor FD.
Return newly created file descriptor.
On error negative value is returned."
  (fd int))

;;; Memory
(cffi:defcfun ("memcpy" c:memcpy) pointer
  "Elisp binding for memcpy(3)."
  (dst pointer) (src pointer) (len unsigned-int))

(cffi:defcfun ("memset" c:memset) pointer
  "Write LEN bytes of value C to the string B."
  (b pointer) (c int) (len unsigned-int))

;;; SHM
(defconst c:IPC-PRIVATE 0)
(defconst c:IPC-R #o000400)
(defconst c:IPC-W #o000200)
(defconst c:IPC-CREAT #o001000)
(defconst c:IPC-0777-MASK #o0777)

(cffi:defcfun ("shmget" c:shmget) int
  "Return id of newly created or previously existing shared memory segment.
KEY names an IPC object.  There are three ways to specify a KEY:

 o  `c:IPC-PRIVATE' may be specified, in which case a new IPC object
    will be created.

 o  An integer constant may be specified.  If no IPC object
    corresponding to key is specified and the `c:IPC-CREAT' bit is set in
    FLAG, a new one will be created.

 o  The `c:ftok' may be used to generate a key from a pathname.

SIZE indicates the desired size of the new segment in bytes."
  (key long) (size unsigned-int) (flag int))

(cffi:defcfun ("shmat" c:shmat) pointer
  "Attaches the shared memory segment identified by SHMID to proccess.
The address where the segment is attached is determined as follows:

 o  If ADDR is null-pointer, the segment is attached at an address
    selected by the kernel.

 o  If ADDR is not null-pointer and `c:SHM-RND' is not specified in
    FLAG, the segment is attached the specified address.

 o  If ADDR is specified and `c:SHM-RND' is specified, ADDR is rounded
    down to the nearest multiple of SHMLBA."
  (shmid int) (addr pointer) (flag int))

(cffi:defcfun ("shmdt" c:shmdt) int
  "Detaches the shared memory segment at the ADDR from process."
  (addr pointer))

(provide 'ffi-libc)

;;; ffi-libc.el ends here
