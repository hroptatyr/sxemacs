;;; auto-save.el -- Safer autosaving for EFS and tmp.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1992 by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Copyright (C) 2001 Ben Wing.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, dumped
;; Version: 1.26

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with XEmacs.

;; Combines autosaving for efs (to a local or remote directory)
;; with the ability to do autosaves to a fixed directory on a local
;; disk, in case NFS is slow.  The auto-save file used for
;;     /usr/foo/bar/baz.txt
;; will be
;;     AUTOSAVE/#=2Fusr=2Ffoo=2Fbar=2Fbaz.txt#"
;; assuming AUTOSAVE is the non-nil value of the variable
;; `auto-save-directory'.

;; Autosaves even if the current directory is not writable.

;; Can limit autosave names to 14 characters using a hash function,
;; see `auto-save-hash-p'.

;; See `auto-save-directory' and `make-auto-save-file-name' and
;; references therein for complete documentation.

;; `M-x recover-all-files' will effectively do recover-file on all
;; files whose autosave file is newer (one of the benefits of having
;; all autosave files in the same place).

;; This file is dumped with XEmacs.

;; If you want to autosave in the fixed directory /tmp/USER-autosave/
;; (setq auto-save-directory
;;       (concat "/tmp/" (user-login-name) "-autosave/"))

;; If you don't want to save in /tmp (e.g., because it is swap
;; mounted) but rather in ~/.autosave/
;;   (setq auto-save-directory (expand-file-name "~/.autosave/"))

;; If you want to save each file in its own directory (the default)
;;   (setq auto-save-directory nil)
;; You still can take advantage of autosaving efs remote files
;; in a fixed local directory, `auto-save-directory-fallback' will
;; be used.

;; If you want to use 14 character hashed autosave filenames
;;   (setq auto-save-hash-p t)

;; Finally, put this line after the others in your ~/.emacs:
;;   (require 'auto-save)


;;; Acknowledgement:

;; This code is loosely derived from autosave-in-tmp.el by Jamie
;; Zawinski <jwz@jwz.org> (the version I had was last modified 22
;; dec 90 jwz) and code submitted to ange-ftp-lovers on Sun, 5 Apr
;; 92 23:20:47 EDT by drw@BOURBAKI.MIT.EDU (Dale R. Worley).
;; auto-save.el tries to cover the functionality of those two
;; packages.

;; Valuable comments and help from Dale Worley, Andy Norman, Jamie
;; Zawinski and Sandy Rutherford are gratefully acknowledged.

(defconst auto-save-version "1.26"
  "Version number of auto-save.")

(provide 'auto-save)


;;; Customization:

(defgroup auto-save nil
  "Autosaving with support for efs and /tmp."
  :group 'data)

(put 'auto-save-interval 'custom-type 'integer)
(put 'auto-save-interval 'factory-value '(300))
(custom-add-to-group 'auto-save 'auto-save-interval 'custom-variable)

(defcustom auto-save-directory nil

  ;; Don't make this user-variable-p, it should be set in .emacs and
  ;; left at that.  In particular, it should remain constant across
  ;; several Emacs session to make recover-all-files work.

  ;; However, it's OK for it to be customizable, as most of the
  ;; customizable variables are set at the time `.emacs' is read.
  ;; -hniksic

  "If non-nil, fixed directory for autosaving: all autosave files go
there.  If this directory does not yet exist at load time, it is
created and its mode is set to 0700 so that nobody else can read your
autosave files.

If nil, each autosave files goes into the same directory as its
corresponding visited file.

A non-nil `auto-save-directory' could be on a local disk such as in
/tmp, then auto-saves will always be fast, even if NFS or the
automounter is slow.  In the usual case of /tmp being locally mounted,
note that if you run emacs on two different machines, they will not
see each other's auto-save files.

The value \(expand-file-name \"~/.autosave/\"\) might be better if /tmp
is mounted from swap (possible in SunOS, type `df /tmp' to find out)
and thus vanishes after a reboot, or if your system is particularly
thorough when cleaning up /tmp, clearing even non-empty subdirectories.

It should never be an efs remote filename because that would
defeat `efs-auto-save-remotely'.

Unless you set `auto-save-hash-p', you shouldn't set this to a
directory in a filesystem that does not support long filenames, since
a file named

    /home/sk/lib/emacs/lisp/auto-save.el

will have a longish filename like

    AUTO-SAVE-DIRECTORY/#=2Fhome=2Fsk=2Flib=2Femacs=2Flisp=2Fauto-save.el#

as auto save file.

See also variables `auto-save-directory-fallback',
`efs-auto-save' and `efs-auto-save-remotely'."
  :type '(choice (const :tag "Same as file" nil)
		 directory)
  :group 'auto-save)


(defcustom auto-save-hash-p nil
  "If non-nil, hashed autosave names of length 14 are used.
This is to avoid autosave filenames longer than 14 characters.
The directory used is `auto-save-hash-directory' regardless of
`auto-save-directory'.
Hashing defeats `recover-all-files', you have to recover files
individually by doing `recover-file'."
  :type 'boolean
  :group 'auto-save)

;;; This defvar is in efs.el now, but doesn't hurt to give it here as
;;; well so that loading first auto-save.el does not abort.

;; #### Now that `auto-save' is dumped, this is looks obnoxious.
(or (boundp 'efs-auto-save) (defvar efs-auto-save 0))
(or (boundp 'efs-auto-save-remotely) (defvar efs-auto-save-remotely nil))

(defcustom auto-save-offer-delete nil
  "*If non-nil, `recover-all-files' offers to delete autosave files
that are out of date or were dismissed for recovering.
Special value 'always deletes those files silently."
  :type '(choice (const :tag "on" t)
		 (const :tag "off" nil)
		 (const :tag "Delete silently" always))
  :group 'auto-save)

;;;; end of customization


;;; Preparations to be done at load time

;; Do not call expand-file-name! This is evaluated at dump time now!
(defvar auto-save-directory-fallback "~/.autosave/"
  ;; not user-variable-p, see above
  "Directory used for local autosaving of remote files if
both `auto-save-directory' and `efs-auto-save-remotely' are nil.
Also used if a working directory to be used for autosaving is not writable.
This *must* always be the name of directory that exists or can be
created by you, never nil.")

(defvar auto-save-hash-directory
  (expand-file-name "hash/" (or auto-save-directory
				auto-save-directory-fallback))
  "If non-nil, directory used for hashed autosave filenames.")

(defun auto-save-checked-directory (dir)
  "Make sure the directory DIR exists and return it expanded if non-nil."
    (when dir
      (setq dir (expand-file-name dir))
      ;; Make sure directory exists
      (unless (file-directory-p dir)
	;; Else we create and chmod 0700 the directory
	(setq dir (directory-file-name dir)) ; some systems need this
	(make-directory dir)
	(set-file-modes dir #o700))
      dir))

;; This make no sense at dump time
;; (mapc #'auto-save-check-directory
;     '(auto-save-directory auto-save-directory-fallback))

;(and auto-save-hash-p
;     (auto-save-check-directory 'auto-save-hash-directory))


;;; Computing an autosave name for a file and vice versa

(defun make-auto-save-file-name (&optional file-name)
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name'; that is checked
before calling this function.

Offers to autosave all files in the same `auto-save-directory'.  All
autosave files can then be recovered at once with function
`recover-all-files'.

Takes care to make autosave files for files accessed through efs
be local files if variable `efs-auto-save-remotely' is nil.

Takes care of slashes in buffer names to prevent autosave errors.

Takes care that autosave files for buffers not visiting any file (such
as `*mail*') from two simultaneous Emacses don't collide by prepending
the Emacs pid.

Uses 14 character autosave names if `auto-save-hash-p' is true.

Autosaves even if the current directory is not writable, using
directory `auto-save-directory-fallback'.

You can redefine this for customization (he he :-).
See also function `auto-save-file-name-p'."

  ;; We have to be very careful about not signalling an error in this
  ;; function since files.el does not provide for this (e.g. find-file
  ;; would fail for each new file).

  (setq file-name (or file-name
		      buffer-file-truename
		      (and buffer-file-name
			   (expand-file-name buffer-file-name))))
  (condition-case error-data
      (let (
	    ;; So autosavename looks like #%...#, roughly as with the
	    ;; old make-auto-save-file-name function.  The
	    ;; make-temp-name inserts the pid of this Emacs: this
	    ;; avoids autosaving from two Emacses into the same file.
	    ;; It cannot be recovered automatically then because in
	    ;; the next Emacs session (the one after the crash) the
	    ;; pid will be different, but file-less buffers like
	    ;; *mail* must be recovered manually anyway.

	    ;; jwz: putting the emacs PID in the auto-save file name is bad
	    ;; news, because that defeats auto-save-recovery of *mail*
	    ;; buffers -- the (sensible) code in sendmail.el calls
	    ;; (make-auto-save-file-name) to determine whether there is
	    ;; unsent, auto-saved mail to recover. If that mail came from a
	    ;; previous emacs process (far and away the most likely case)
	    ;; then this can never succeed as the pid differs.
	    ;;(name-prefix (if file-name nil (make-temp-name "#%")))
	    (name-prefix (if file-name nil "#%"))

	    (save-name (or file-name
			   ;; Prevent autosave errors.  Buffername
			   ;; (to become non-dir part of filename) will
			   ;; be escaped twice.  Don't care.
			   (auto-save-escape-name (buffer-name))))
	    (remote-p (and (stringp file-name)
			   (fboundp 'efs-ftp-path)
			   (efs-ftp-path file-name))))
	;; Return the appropriate auto save file name:
	(expand-file-name;; a buffername needs this, a filename not
	 (cond (remote-p
		(if efs-auto-save-remotely
		    (auto-save-name-in-same-directory save-name)
		  ;; We have to use the `fixed-directory' now since the
		  ;; `same-directory' would be remote.
		  ;; It will use the fallback if needed.
		  (auto-save-name-in-fixed-directory save-name)))
	       ;; Else it is a local file (or a buffer without a file,
	       ;; hence the name-prefix).
	       ((or auto-save-directory auto-save-hash-p)
		;; Hashed files always go into the special hash dir,
		;; never in the same directory, to make recognizing
		;; reliable.
		(auto-save-name-in-fixed-directory save-name name-prefix))
	       (t
		(auto-save-name-in-same-directory save-name name-prefix)))))

    ;; If any error occurs in the above code, return what the old
    ;; version of this function would have done.  It is not ok to
    ;; return nil, e.g., when after-find-file tests
    ;; file-newer-than-file-p, nil would bomb.

    (error (warn "Error caught in `make-auto-save-file-name':\n%s"
		 (error-message-string error-data))
	   (let ((fname
		  (if file-name
		      (concat (file-name-directory file-name)
			      "#"
			      (file-name-nondirectory file-name)
			      "#")
		    (expand-file-name
		     (concat "#%" (auto-save-escape-name (buffer-name))
			     "#")))))
	     (if (or (file-writable-p fname)
		     (file-exists-p fname))
		 fname
	       (expand-file-name (concat "~/"
					 (file-name-nondirectory fname))))))))

(defun auto-save-file-name-p (filename)
  "Return non-nil if FILENAME can be yielded by `make-auto-save-file-name'.
FILENAME should lack slashes.
You can redefine this for customization."
  (string-match "\\`#.*#\\'" filename))

(defun auto-save-original-name (savename)
  "Reverse of `make-auto-save-file-name'.
Returns nil if SAVENAME was not associated with a file (e.g., it came
from an autosaved `*mail*' buffer) or does not appear to be an
autosave file at all.
Hashed files are not understood, see `auto-save-hash-p'."
  (let ((basename (file-name-nondirectory savename))
	(savedir (file-name-directory savename)))
    (cond ((or (not (auto-save-file-name-p basename))
	       (string-match "^#%" basename))
	   nil)
	  ;; now we know it looks like #...# thus substring is safe to use
	  ((or (equal savedir
		      (and auto-save-directory
			   (expand-file-name auto-save-directory)))
					; 2nd arg may be nil
	       (equal savedir
		      (expand-file-name auto-save-directory-fallback)))
	   ;; it is of the `-fixed-directory' type
	   (auto-save-unescape-name (substring basename 1 -1)))
	  (t
	   ;; else it is of `-same-directory' type
	   (concat savedir (substring basename 1 -1))))))

(defun auto-save-name-in-fixed-directory (filename &optional prefix)
  ;; Escape and enclose the whole FILENAME in `#' to make an auto
  ;; save file in the auto-save-directory, or if that is nil, in
  ;; auto-save-directory-fallback (which must be the name of an
  ;; existing directory).  If the results would be too long for 14
  ;; character filenames, and `auto-save-hash-p' is set, hash FILENAME
  ;; into a shorter name.
  ;; Optional PREFIX is string to use instead of "#" to prefix name.
  (let ((base-name (concat (or prefix "#")
			   (auto-save-escape-name filename)
			   "#")))
    (if (and auto-save-hash-p
	     auto-save-hash-directory
	     (> (length base-name) 14))
	(expand-file-name (auto-save-cyclic-hash-14 filename)
			  (auto-save-checked-directory auto-save-hash-directory))
      (expand-file-name base-name
			(auto-save-checked-directory
			   (or auto-save-directory
			       auto-save-directory-fallback))))))

(defun auto-save-name-in-same-directory (filename &optional prefix)
  ;; Enclose the non-directory part of FILENAME in `#' to make an auto
  ;; save file in the same directory as FILENAME.  But if this
  ;; directory is not writable, use auto-save-directory-fallback.
  ;; FILENAME is assumed to be in non-directory form (no trailing slash).
  ;; It may be a name without a directory part (presumably it really
  ;; comes from a buffer name then), the fallback is used then.
  ;; Optional PREFIX is string to use instead of "#" to prefix name.
  (let ((directory (file-name-directory filename)))
    (or (null directory)
	(file-writable-p directory)
	(setq directory (auto-save-checked-directory
			 auto-save-directory-fallback)))
    (concat directory			; (concat nil) is ""
	    (or prefix "#")
	    (file-name-nondirectory filename)
	    "#")))

(defconst auto-save-reserved-chars
  '(
    ?\0 ?\1 ?\2 ?\3 ?\4 ?\5 ?\6 ?\7 ?\10 ?\11 ?\12 ?\13 ?\14 ?\15 ?\16
    ?\17 ?\20 ?\21 ?\22 ?\23 ?\24 ?\25 ?\26 ?\27 ?\30 ?\31 ?\32 ?\33
    ?\34 ?\35 ?\36 ?\37 ?\40 ?? ?* ?: ?< ?> ?| ?/ ?\\ ?& ?^ ?% ?= ?\")
  "List of characters disallowed (or potentially disallowed) in filenames.
Includes everything that can get us into trouble under MS Windows or Unix.")

;; This code based on code in Bill Perry's url.el.
    
(defun auto-save-escape-name (str)
  "Escape any evil nasty characters in a potential filename.
Uses quoted-printable-style escaping -- e.g. the dreaded =3D.
Does not use URL escaping (with %) because filenames beginning with #% are
a special signal for non-file buffers."
  (mapconcat
   (function
    (lambda (char)
      (if (memq char auto-save-reserved-chars)
	  (if (< char 16)
	      (upcase (format "=0%x" char))
	    (upcase (format "=%x" char)))
	(char-to-string char))))
   str ""))

(defun auto-save-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defun auto-save-unescape-name (str)
  "Undo any escaping of evil nasty characters in a file name.
See `auto-save-escape-name'."
  (setq str (or str ""))
  (let ((tmp "")
	(case-fold-search t))
    (while (string-match "=[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (auto-save-unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (auto-save-unhex (elt str (+ start 2))))))
	(setq tmp (concat tmp (substring str 0 start)
			  (char-to-string code))
	      str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

;; The old versions are below.

;(defun auto-save-escape-name (s)
;  ;;  "Quote any slashes in string S by replacing them with the two
;  ;;characters `\\!'.
;  ;;Also, replace any backslash by double backslash, to make it one-to-one."
;  (let ((limit 0))
;    (while (string-match "[/\\]" s limit)
;      (setq s (concat (substring s 0 (match-beginning 0))
;		      (if (string= (substring s
;					      (match-beginning 0)
;					      (match-end 0))
;				   "/")
;			  "\\!"
;			"\\\\")
;		      (substring s (match-end 0))))
;      (setq limit (1+ (match-end 0)))))
;  s)

;(defun auto-save-unescape-name (s)
;  ;;"Reverse of `auto-save-escape-name'."
;  (let (pos)
;    (while (setq pos (string-match "\\\\[\\!]" s pos))
;      (setq s (concat (substring s 0 pos)
;		      (if (eq ?! (aref s (1+ pos))) "/" "\\")
;		      (substring s (+ pos 2)))
;	    pos (1+ pos))))
;  s)


;;; Hashing for autosave names

;;; Hashing function contributed by Andy Norman <ange@hplb.hpl.hp.com>
;;; based upon C code from pot@fly.cnuce.cnr.IT (Francesco Potorti`).

(defun auto-save-cyclic-hash-14 (s)
  ;;   "Hash string S into a string of length 14.
  ;; A 7-bytes cyclic code for burst correction is calculated on a
  ;; byte-by-byte basis. The polynomial used is D^7 + D^6 + D^3 +1.
  ;; The resulting string consists of hexadecimal digits [0-9a-f].
  ;; In particular, it contains no slash, so it can be used as autosave name."
  (let ((crc (make-vector 7 ?\0)))
    (mapc
     (lambda (new)
       (setq new (+ new (aref crc 6)))
       (aset crc 6 (+ (aref crc 5) new))
       (aset crc 5 (aref crc 4))
       (aset crc 4 (aref crc 3))
       (aset crc 3 (+ (aref crc 2) new))
       (aset crc 2 (aref crc 1))
       (aset crc 1 (aref crc 0))
       (aset crc 0 new))
     s)
    (format "%02x%02x%02x%02x%02x%02x%02x"
	    (logand 255 (aref crc 0))
	    (logand 255 (aref crc 1))
	    (logand 255 (aref crc 2))
	    (logand 255 (aref crc 3))
	    (logand 255 (aref crc 4))
	    (logand 255 (aref crc 5))
	    (logand 255 (aref crc 6)))))

;; #### It is unclear to me how the following function is useful.  It
;; should be used in `auto-save-name-in-same-directory', if anywhere.
;; -hniksic

;; This leaves two characters that could be used to wrap it in `#' or
;; make two filenames from it: one for autosaving, and another for a
;; file containing the name of the autosaved file, to make hashing
;; reversible.
;(defun auto-save-cyclic-hash-12 (s)
;  "Outputs the 12-characters ascii hex representation of a 6-bytes
;cyclic code for burst correction calculated on STRING on a
;byte-by-byte basis. The used polynomial is D^6 + D^5 + D^4 + D^3 +1."
;  (let ((crc (make-string 6 0)))
;    (mapc
;     (lambda (new)
;       (setq new (+ new (aref crc 5)))
;       (aset crc 5 (+ (aref crc 4) new))
;       (aset crc 4 (+ (aref crc 3) new))
;       (aset crc 3 (+ (aref crc 2) new))
;       (aset crc 2 (aref crc 1))
;       (aset crc 1 (aref crc 0))
;       (aset crc 0 new))
;     s)
;    (format "%02x%02x%02x%02x%02x%02x"
;            (aref crc 0)
;            (aref crc 1)
;            (aref crc 2)
;            (aref crc 3)
;            (aref crc 4)
;            (aref crc 5))))



;;; Recovering files

(defun recover-all-files (&optional silent)
  "Do recover-file for all autosave files which are current.
Only works if you have a non-nil `auto-save-directory'.

Optional prefix argument SILENT means to be silent about non-current
autosave files.  This is useful if invoked automatically at Emacs
startup.

If `auto-save-offer-delete' is t, this function will offer to delete
old or rejected autosave files.

Hashed files (see `auto-save-hash-p') are not understood, use
`recover-file' to recover them individually."
  (interactive "P")
  (let ((savefiles (directory-files auto-save-directory
				    t "\\`#" nil t))
	afile				; the auto save file
	file				; its original file
	(total 0)			; # of files offered to recover
	(count 0))			; # of files actually recovered
    (or (equal (expand-file-name auto-save-directory)
	       (expand-file-name auto-save-directory-fallback))
	(setq savefiles
	      (nconc savefiles
		     (directory-files auto-save-directory-fallback
				      t "\\`#" nil t))))
    (while savefiles
      (setq afile (car savefiles)
	    file (auto-save-original-name afile)
	    savefiles (cdr savefiles))
      (cond ((and file (not (file-newer-than-file-p afile file)))
	     (warn "Autosave file \"%s\" is not current." afile))
	    (t
	     (incf total)
	     (with-output-to-temp-buffer "*Directory*"
	       (buffer-disable-undo standard-output)
	       (save-excursion
		 (set-buffer "*Directory*")
		 (setq default-directory (file-name-directory afile))
		 (insert-directory afile "-l")
		 (when file
		   (setq default-directory (file-name-directory file))
		   (insert-directory file "-l"))))
	     (if (yes-or-no-p (format "Recover %s from auto save file? "
				      (or file "non-file buffer")))
		 (let* ((obuf (current-buffer)))
		   (set-buffer (if file
				   (find-file-noselect file t)
				 (generate-new-buffer "*recovered*")))
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   (insert-file-contents afile nil)
		   (ignore-errors
		     (after-find-file nil))
		   (setq buffer-auto-save-file-name nil)
		   (incf count)
		   (message "\
Auto-save off in buffer \"%s\" till you do M-x auto-save-mode."
			    (buffer-name))
		   (set-buffer obuf)
		   (sit-for 1))
	       ;; If not used for recovering, offer to delete
	       ;; autosave file
	       (and auto-save-offer-delete
		    (or (eq 'always auto-save-offer-delete)
			(yes-or-no-p
			 (format "Delete autosave file for `%s'? " file)))
		    (delete-file afile))))))
    (if (zerop total)
	(or silent (message "Nothing to recover."))
      (message "%d/%d file%s recovered." count total (if (= count 1) "" "s"))))
  (and (get-buffer "*Directory*")
       (kill-buffer "*Directory*")))

;;; auto-save.el ends here
