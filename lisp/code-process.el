;;; code-process.el --- Process coding functions for XEmacs.

;; Copyright (C) 1985-1987, 1993, 1994, 1997, 2003
;;               Free Software Foundation, Inc.
;; Copyright (C) 1995 Ben Wing
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Author: Ben Wing
;;         MORIOKA Tomohiko
;; Maintainer: XEmacs Development Team
;; Keywords: mule, multilingual, coding system, process

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

;;; Commentary:

;; This file has some similarities to code-files.el.

;;; Code:

(defvar process-coding-system-alist nil
  "Alist to decide a coding system to use for a process I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a program name,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the program and encoding what sent to the program.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.")

(defun call-process (program &optional infile buffer displayp &rest args)
  "Call PROGRAM synchronously in separate process.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAYP non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If BUFFER is 0, `call-process' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate and returns a numeric exit status
 or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you
 quit again.

Coding systems are taken from `coding-system-for-read' for input and
`coding-system-for-write' for output if those variables are bound.
Otherwise they are looked up in `process-coding-system-alist'.  If not
found, they default to `nil' for both input and output."
  (let* ((coding-system-for-read
	  (or coding-system-for-read
	      (let (ret)
		(catch 'found
		  (let ((alist process-coding-system-alist)
			(case-fold-search nil))
		    (while alist
		      (if (string-match (car (car alist)) program)
			  (throw 'found (setq ret (cdr (car alist))))
			)
		      (setq alist (cdr alist))
		      )))
		(if (functionp ret)
		    (setq ret (funcall ret 'call-process program))
		  )
		(cond ((consp ret) (car ret))
		      ((not ret) 'undecided)
		      ((find-coding-system ret) ret)
		      )
		))))
    (apply 'call-process-internal program infile buffer displayp args)
    ))

(defun call-process-region (start end program
				  &optional deletep buffer displayp
				  &rest args)
  "Send text from START to END to a synchronous process running PROGRAM.
Delete the text if fourth arg DELETEP is non-nil.

Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Sixth arg DISPLAYP non-nil means redisplay buffer as output is inserted.
Remaining args are passed to PROGRAM at startup as command args.

If BUFFER is 0, returns immediately with value nil.
Otherwise waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is first killed with SIGINT, then with SIGKILL if
you quit again before the process exits.

Coding systems are taken from `coding-system-for-read' for input and
`coding-system-for-write' for output if those variables are bound.
Otherwise they are looked up in `process-coding-system-alist'.  If not
found, they default to `nil' for both input and output."
  (let ((temp
	 (make-temp-name
	  (concat (file-name-as-directory (temp-directory)) "emacs"))))
    (unwind-protect
	(let (cs-r cs-w)
	  (let (ret)
	    (catch 'found
	      (let ((alist process-coding-system-alist)
		    (case-fold-search nil))
		(while alist
		  (if (string-match (car (car alist)) program)
		      (throw 'found (setq ret (cdr (car alist)))))
		  (setq alist (cdr alist))
		  )))
	    (if (functionp ret)
		(setq ret (funcall ret 'call-process-region program)))
	    (cond ((consp ret)
		   (setq cs-r (car ret)
			 cs-w (cdr ret)))
		  ((null ret)
		   (setq cs-r buffer-file-coding-system
			 cs-w buffer-file-coding-system))
		  ((find-coding-system ret)
		   (setq cs-r ret
			 cs-w ret))))
	  (let ((coding-system-for-read
		 (or coding-system-for-read cs-r))
		(coding-system-for-write
		 (or coding-system-for-write cs-w)))
	    (write-region start end temp nil 'silent)
	    (if deletep (delete-region start end))
	    (apply #'call-process program temp buffer displayp args)))
      (ignore-file-errors (delete-file temp)))))

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER PROGRAM &rest PROGRAM-ARGS
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is program file name.  It is searched for as in the shell.
Remaining arguments are strings to give program as arguments.

Coding systems are taken from `coding-system-for-read' for input and
`coding-system-for-write' for output if those variables are bound.
Otherwise they are looked up in `process-coding-system-alist'.  If not
found, they default to `undecided' for input and `nil' (binary) for
output."
  (let (cs-r cs-w)
    (let (ret)
      (catch 'found
	(let ((alist process-coding-system-alist)
	      (case-fold-search nil))
	  (while alist
	    (if (string-match (car (car alist)) program)
		(throw 'found (setq ret (cdr (car alist)))))
	    (setq alist (cdr alist))
	    )))
      (if (functionp ret)
	  (setq ret (funcall ret 'start-process program)))
      (cond ((consp ret)
	     (setq cs-r (car ret)
		   cs-w (cdr ret)))
	    ((find-coding-system ret)
	     (setq cs-r ret
		   cs-w ret))))
    (let ((coding-system-for-read
	   (or coding-system-for-read cs-r 'undecided))
	  (coding-system-for-write
	   (or coding-system-for-write cs-w)))
      (apply 'start-process-internal name buffer program program-args)
      )))

(defvar network-coding-system-alist nil
  "Alist to decide a coding system to use for a network I/O operation.
The format is ((PATTERN . VAL) ...),
where PATTERN is a regular expression matching a network service name
or is a port number to connect to,
VAL is a coding system, a cons of coding systems, or a function symbol.
If VAL is a coding system, it is used for both decoding what received
from the network stream and encoding what sent to the network stream.
If VAL is a cons of coding systems, the car part is used for decoding,
and the cdr part is used for encoding.
If VAL is a function symbol, the function must return a coding system
or a cons of coding systems which are used as above.

See also the function `find-operation-coding-system'.")

(defun open-network-stream (name buffer host service &optional protocol)
  "Open a TCP connection for a service to a host.
Return a process object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
 specifying a port number to connect to.
Fifth argument PROTOCOL is a network protocol.  Currently 'tcp
 (Transmission Control Protocol) and 'udp (User Datagram Protocol) are
 supported.  When omitted, 'tcp is assumed.

Output via `process-send-string' and input via buffer or filter (see
`set-process-filter') are stream-oriented.  That means UDP datagrams are
not guaranteed to be sent and received in discrete packets. (But small
datagrams around 500 bytes that are not truncated by `process-send-string'
are usually fine.)  Note further that UDP protocol does not guard against
lost packets."
  (let (cs-r cs-w)
    (let (ret)
      (catch 'found
	(let ((alist network-coding-system-alist)
	      (case-fold-search nil)
	      pattern)
	  (while alist
	    (setq pattern (car (car alist)))
	    (and
	     (cond ((numberp pattern)
		    (and (numberp service)
			 (eq pattern service)))
		   ((stringp pattern)
		    (or (and (stringp service)
			     (string-match pattern service))
			(and (numberp service)
			     (string-match pattern
					   (number-to-string service))))))
	     (throw 'found (setq ret (cdr (car alist)))))
	    (setq alist (cdr alist))
	    )))
      (if (functionp ret)
	  (setq ret (funcall ret 'open-network-stream service)))
      (cond ((consp ret)
	     (setq cs-r (car ret)
		   cs-w (cdr ret)))
	    ((find-coding-system ret)
	     (setq cs-r ret
		   cs-w ret))))
    (let ((coding-system-for-read
	   (or coding-system-for-read cs-r))
	  (coding-system-for-write
	   (or coding-system-for-write cs-w)))
      (open-network-stream-internal name buffer host service protocol))))

;;; code-process.el ends here
