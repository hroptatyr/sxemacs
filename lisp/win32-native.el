;;; win32-native.el --- Lisp routines when running on native MS Windows.

;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: mouse, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.
;;; (FSF has stuff in w32-fns.el and term/w32-win.el.)

;;; Commentary:

;; This file is dumped with XEmacs for MS Windows (without cygwin).
;; It is for stuff that is used specifically when `system-type' eq
;; `windows-nt' (i.e. also applies to MinGW), and has nothing to do
;; with the `mswindows' device type.  Thus, it probably applies in
;; non-interactive mode as well, and it DOES NOT APPLY to Cygwin.

;; Based (originally) on NT Emacs version by Geoff Voelker
;; (voelker@cs.washington.edu)
;; Ported to XEmacs by Marc Paquette <marcpa@cam.org>
;; Largely modified by Kirill M. Katsnelson <kkm@kis.ru>
;; Rewritten from scratch by Ben Wing <ben@xemacs.org>.  No code in common
;; with FSF.

;;; Code:

;; For appending suffixes to directories and files in shell
;; completions.  This screws up cygwin users so we leave it out for
;; now. Uncomment this if you only ever want to use cmd.

;(defun nt-shell-mode-hook ()
;  (setq comint-completion-addsuffix '("\\" . " ")
;	comint-process-echoes t))
;(add-hook 'shell-mode-hook 'nt-shell-mode-hook)

;; Use ";" instead of ":" as a path separator (from files.el).
(setq path-separator ";")

;; Set the null device (for compile.el).
;; Backward-compatibility; recent compile.el uses null-device if available.
(setq grep-null-device null-device)

;; Set the grep regexp to match entries with drive letters.
(setq grep-regexp-alist
  '(("^\\(\\([a-zA-Z]:\\)?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 3)))

(defvar mswindows-system-shells '("cmd" "cmd.exe" "command" "command.com"
				  "4nt" "4nt.exe" "4dos" "4dos.exe"
				  "ndos" "ndos.exe")
  "List of strings recognized as Windows NT/9X system shells.
These are shells with native semantics, e.g. they use `/c', not '-c',
to pass a command in.")

(defun mswindows-system-shell-p (shell-name)
  (member (downcase (file-name-nondirectory shell-name)) 
	  mswindows-system-shells))

(defun init-mswindows-at-startup ()
  ;; shell-file-name is initialized in the C code (callproc.c) from
  ;; SHELL or COMSPEC.
  ;; #### If only shell-command-switch could be a function.  But there
  ;; is code littered around that uses it.
  ;; #### Maybe we should set a symbol-value handler on `shell-file-name'
  ;; that automatically sets shell-command-switch?
  (if (mswindows-system-shell-p shell-file-name)
      (setq shell-command-switch "/c")))

;;----------------------------------------------------------------------
;; Quoting process args
;;--------------------

(defvar debug-mswindows-process-command-lines nil
  "If non-nil, output debug information about the command lines constructed.
This can be useful if you are getting process errors where the arguments
to the process appear to be getting passed incorrectly.")

;; properly quotify one arg for the vc runtime argv constructor.
(defun mswindows-quote-one-vc-runtime-arg (arg &optional quote-shell)
  ;; we mess with any arg with whitespace, quotes, or globbing chars in it.
  ;; we also include shell metachars if asked.
  ;; note that \ is NOT included!  it's perfectly OK to include an
  ;; arg like c:\ or c:\foo.
  (cond ((equal arg "") "\"\"")
	((string-match
	  (if quote-shell "[ \t\n\r\f*?\"<>|&^%]" "[ \t\n\r\f*?\"]")
	  arg)
	 ;; handle nested quotes, possibly preceded by backslashes
	 (setq arg (replace-in-string arg "\\([\\]*\\)\"" "\\1\\1\\\\\""))
	 ;; handle trailing backslashes
	 (setq arg (replace-in-string arg "\\([\\]+\\)$" "\\1\\1"))
	 (concat "\"" arg "\""))
	(t arg)))

(defun mswindows-quote-one-simple-arg (arg &optional quote-shell)
  ;; just put double quotes around args with spaces (and maybe shell
  ;; metachars).
  (cond ((equal arg "") "\"\"")
	((string-match
	  (if quote-shell "[ \t\n\r\f*?\"<>|&^%]" "[ \t\n\r\f*?]")
	  arg)
	 (concat "\"" arg "\""))
	(t arg)))

(defun mswindows-quote-one-command-arg (arg)
  ;; quote an arg to get it past COMMAND.COM/CMD.EXE: need to quote shell
  ;; metachars with ^.
  (cond ((equal arg "") "\"\"")
	(t (replace-in-string "[<>|&^%]" "^\\1" arg))))

(defun mswindows-construct-verbatim-command-line (program args)
  (mapconcat #'identity args " "))

;; for use with either standard VC++ compiled programs or Cygwin programs,
;; which emulate the same behavior.
(defun mswindows-construct-vc-runtime-command-line (program args)
  (mapconcat #'mswindows-quote-one-vc-runtime-arg args " "))

;; note: for pulling apart an arg:
;; each arg consists of either

;; something surrounded by single quotes

;; or

;; one or more of

;; 1. a non-ws, non-" char
;; 2. a section of double-quoted text
;; 3. a section of double-quoted text with end-of-string instead of the final
;; quote.

;; 2 and 3 get handled together.

;; quoted text is one of
;;
;; 1. quote + even number of backslashes + quote, or
;; 2. quote + non-greedy anything + non-backslash + even number of
;;    backslashes + quote.

;; we need to separate the two because we unfortunately have no non-greedy
;; ? operator. (urk! we actually do, but it wasn't documented.) --ben

;; if you want to mess around, keep this test case in mind:

;; this string

;; " as'f 'FOO BAR' '' \"\" \"asdf \\ \\\" \\\\\\\" asdfasdf\\\\\" foo\" "

;; should tokenize into this:

;; (" " "as'f" " " "'FOO BAR' " "'' " "\"\"" " " "\"asdf \\ \\\" \\\\\\\" asdfasdf\\\\\"" " " "foo" "\" ")

;; this regexp actually separates the arg into individual args, like a
;; shell (such as sh) does, but using vc-runtime rules.  it's easy to
;; derive the tokenizing regexp from it, and that's exactly what i did.
;; but oh was it hard to get this first regexp right. --ben
;(defvar mswindows-match-one-cmd-exe-arg-regexp
;  (concat
;   "^\\("
;   "'\\([\\]*\\)\\2'" "\\|"
;   "'.*?[^\\]\\(\\([\\]*\\)\\4'\\)" "\\|"
;   "\\("
;   "[^ \t\n\r\f\v\"]" "\\|"
;   "\"\\([\\]*\\)\\6\"" "\\|"
;   "\".*?[^\\]\\(\\([\\]*\\)\\8\"\\|$\\)"
;   "\\)+"
;   "\\)"
;   "\\([ \t\n\r\f\v]+\\|$\\)"))

(defvar mswindows-match-one-cmd-exe-token-regexp
  (concat
   "^\\("
   "[ \t\n\r\f\v]+" "\\|"
   "'\\([\\]*\\)\\2'" "\\([ \t\n\r\f\v]+\\|$\\)" "\\|"
   "'.*?[^\\]\\(\\([\\]*\\)\\5'\\)" "\\([ \t\n\r\f\v]+\\|$\\)" "\\|"
   "[^ \t\n\r\f\v\"]+" "\\|"
   "\"\\([\\]*\\)\\7\"" "\\|"
   "\".*?[^\\]\\(\\([\\]*\\)\\9\"\\|$\\)"
   "\\)"))

(defun mswindows-construct-command-command-line (program args)
  ;; for use with COMMAND.COM and CMD.EXE:
  ;; for each arg, tokenize it into quoted and non-quoted sections;
  ;; then quote all the shell meta-chars with ^; then put everything
  ;; back together.  the truly hard part is the tokenizing -- typically
  ;; we get a single argument (the command to execute) and we have to
  ;; worry about quotes that are backslash-quoted and such.
  (mapconcat
   #'(lambda (arg)
       (mapconcat
	#'(lambda (part)
	    (if (string-match "^'" part)
		(replace-in-string part "\\([<>|^&%]\\)" "^\\1")
	      part))
	(let (parts)
	  (while (and (> (length arg) 0)
		      (string-match
		       mswindows-match-one-cmd-exe-token-regexp
		       arg))
	    (push (match-string 0 arg) parts)
	    (setq arg (substring arg (match-end 0))))
	  (if (> (length arg) 0)
	      (push arg parts))
	  (nreverse parts))
	""))
   args " "))

(defvar mswindows-construct-process-command-line-alist
  '(
    ;; at one point (pre-1.0), this was required for Cygwin bash.
    ;; evidently, Cygwin changed its arg handling to work just like
    ;; any standard VC program, so we no longer need it.
    ;;("[\\/].?.?sh\\." . mswindows-construct-verbatim-command-line)
    ("[\\/]command\\.com$" . mswindows-construct-command-command-line)
    ("[\\/]cmd\\.exe$" . mswindows-construct-command-command-line)
    ("" . mswindows-construct-vc-runtime-command-line))
  "An alist for determining proper argument quoting given executable
file name.  Car of each cons should be a string, a regexp against
which the file name is matched.  Matching is case-insensitive but does
include the directory, so you should begin your regexp with [\\\\/] if
you don't want the directory to matter.  Alternatively, the car can be
a function of one arg, which is called with the executable's name and
should return t if this entry should be processed.  Cdr is a function
symbol, which is called with two args, the executable name and a list
of the args passed to it.  It should return a string, which includes
the executable's args (but not the executable name itself) properly
quoted and pasted together.  The list is matched in order, and the
first matching entry specifies how the processing will happen.")

(defun mswindows-construct-process-command-line (args)
  ;;Properly quote process ARGS for executing (car ARGS).
  ;;Called from the C code.
  (let ((fname (car args))
	(alist mswindows-construct-process-command-line-alist)
	(case-fold-search t)
	(return-me nil)
	(assoc nil))
    (while (and alist
		(null return-me))
      (setq assoc (pop alist))
      (if (if (stringp (car assoc))
	      (string-match (car assoc) fname)
	    (funcall (car assoc) fname))
	  (setq return-me (cdr assoc))))
    (let* ((called-fun (or return-me
			    #'mswindows-construct-vc-runtime-command-line))
	   (retval
	    (let ((str (funcall called-fun fname (cdr args)))
		  (quoted-fname (mswindows-quote-one-simple-arg fname)))
	      (if (and str (> (length str) 0))
		  (concat quoted-fname " " str)
		quoted-fname))))
      (when debug-mswindows-process-command-lines
	(debug-print "mswindows-construct-process-command-line called:\n")
	(debug-print "received args: \n%s"
		     (let ((n -1))
		       (mapconcat #'(lambda (arg)
				      (incf n)
				      (format "  %d %s\n" n arg))
				  args
				  "")))
	(debug-print "called fun %s\n" called-fun)
	(debug-print "resulting command line: %s\n" retval))
      retval)))

;;; win32-native.el ends here
