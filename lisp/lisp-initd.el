;; lisp-initd.el - rc.d inspired configuration management for
;;                 SXEmacs-lisp
;;
;; Copyright (C) 2007, Nelson Ferreira
;; Maintainer: Nelson Ferreira
;;
;; This file is part of SXEmacs
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the
;; distribution.
;;
;; Neither the name of the <ORGANIZATION> nor the names of its
;; contributors may be used to endorse or promote products derived
;; from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;; This file is dumped with SXEmacs

(require 'cl-extra)
;(require 'bytecomp)
;(require 'byte-optimize)
;(require 'bytecomp-runtime)

(defvar lisp-initd-dir (file-name-as-directory
			(expand-file-name "init.d" user-init-directory))
  "The default directory for the init files.")

(defvar lisp-initd-prefix  ""
  "The default prefix for the compiled init file.")

(defvar lisp-initd-keep-elisp t
  "If TRUE the initd resulting lisp file is kept.
Only takes effect when `lisp-initd-byte-compile-elisp' is non-nil.")

(defvar lisp-initd-byte-compile-elisp t
  "If TRUE the initd lisp is byte-compiled.")

(defvar lisp-initd-kill-compile-log nil
  "Kill the byte-compile Compile Log buffers")


(defvar lisp-initd-gather-func #'directory-files 
  "Function used to gather the files used in init.  For acceptable
arguments see `directory-files'.  The function is expected to return a
sorted list of absolute pathnames, accept and honor the MATCH argument
and return files only.")

(defun lisp-initd-compile (&optional dir prefix do-init)
  "Compile the lisp files in DIR into a file named {DIR}/{PREFIX}init.d.el.
If DIR is nil `lisp-initd-dir' is used.
If PREFIX is nil `lisp-initd-prefix' is used.
If DO-INIT is non-nil the file is loaded."
  (let* ((initd-dir    (or dir lisp-initd-dir))
         (initd-file   (concat (or prefix lisp-initd-prefix)
			      "init.d"))
	 (initd-el     (expand-file-name (concat initd-file ".el")
					 (paths-construct-path 
					  (list initd-dir ".."))))
         (initd-elc    (concat initd-el "c"))
         (initd-files  (funcall lisp-initd-gather-func initd-dir
				t "^.*\.el$" nil t))
	 (init-file     (if lisp-initd-byte-compile-elisp initd-elc initd-el))
         init-buffer)

    ;; No use in keeping an outdate byte-compiled file...
    (when (and (file-exists-p initd-el)
	       (file-exists-p initd-elc)
	       (file-newer-than-file-p initd-el initd-elc))
      (delete-file initd-elc))

    ;; If a file (or the directory itself) is newer than the existing
    ;; file then
    (when (some #'(lambda (file) (file-newer-than-file-p file init-file))
		(cons initd-dir initd-files))
      ;; No matter what the elc is outdated now..
      (when (file-exists-p initd-elc) (delete-file initd-elc))
      (when (file-exists-p initd-elc) (delete-file initd-el))
      (message "Recompiling init files....")
      (setq init-buffer (generate-new-buffer (concat "*" initd-el "*")))
      (with-current-buffer init-buffer
        (set-visited-file-name initd-el)
	(insert ";; This is an automatically generated file.\n"
		";; DO NOT EDIT\n"
		";;\n")
	(insert "(message \"Compiled " initd-dir " loading started\")\n")
	(mapc 
	 #'(lambda (current)
	     (condition-case err
		 (insert "(condition-case err (progn\n"
			 ";; ------------------------------------\n"
			 ";; " current "\n"
			 (save-excursion	
			   (save-restriction 
			     (with-temp-buffer
			       (insert-file-contents current)
			       (eval-buffer)
			       (buffer-substring))))
			 "\n"
			 ";; ----------------------------------\n"
			 ")\n"
			 "(error (message \"Error loading " current 
			 ": \\\"%S\\\" (signal:\'%S\' . data:\'%S\')\" "
			 "err (car err) (cdr err))))\n"
			 ";; ----------------------------------\n\n")
	       (error
		(progn
		  (insert "(message \"\\\"" current 
			  "\\\" not inserted "
			  (replace-regexp-in-string 
			   "\"" "\\\""
			   (format (concat "due to syntax error: %S"
					   " (signal:%S . data:%S)")
				   err (car err) (cdr err)))
			  "\")\n")
		  (message
		   "\"%S\" not inserted due to syntax error: %S (signal:%S . data:%S)"
		   current err (car err) (cdr err))))))
	 initd-files)
	(insert "(message \"Compiled " initd-dir " loading finished\")\n")
	(save-buffer init-buffer)))
    (when (and lisp-initd-byte-compile-elisp
	       (file-newer-than-file-p initd-el initd-elc))
      (ignore-errors
	(progn
	  (byte-compile-file initd-el)
	  (when lisp-initd-kill-compile-log
	    (kill-buffer (get-buffer "*Compile-Log*"))
	    (kill-buffer (get-buffer "*Compile-Log-Show*")))
	  (delete-other-windows)
	  (unless lisp-initd-keep-elisp
	    (delete-file initd-el)))))
    (when (and do-init (null init-buffer))
      (load init-file nil nil t))))


(defun lisp-initd-compile-and-load (&optional dir prefix)
  "Compile and load the lisp files in DIR into a file named {DIR}/{PREFIX}init.d.el.

If DIR, a string, is omitted `lisp-initd-dir' is used.  DIR can be
either a complete path, or the last element of a path.  If the latter,
DIR is expanded against the _parent_ directory of `lisp-initd-dir'.

Optional prefix arg, PREFIX is a string that is prepended to the generated
filename to be loaded.  If it is omitted, `lisp-initd-prefix' is used.

See `lisp-initd-compile'."
  (interactive (list (expand-file-name (read-directory-name
					"initd directory: " user-init-directory
					lisp-initd-dir t))))
  (when dir
    (unless (string-match "/" dir)
      (setq dir (file-name-as-directory
		 (expand-file-name dir (paths-construct-path 
					(list lisp-initd-dir "..")))))))
  (when current-prefix-arg
    (setq prefix (read-string "Prefix: ")))
    
  (lisp-initd-compile dir prefix t))

(provide 'lisp-initd)

