;;; files.el --- file input and output commands for XEmacs.

;; Copyright (C) 1985-1987, 1992-1995, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Maintainer: XEmacs Development Team
;; Keywords: extensions, dumped

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
;; Software Foundation, Inc. 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 20.3 (but diverging)
;;; Warning: Merging this file is tough.  Beware.

;;; Commentary:

;; This file is dumped with XEmacs.

;; Defines most of XEmacs's file- and directory-handling functions,
;; including basic file visiting, backup generation, link handling,
;; ITS-id version control, load- and write-hook handling, and the like.

;;; Code:

;; XEmacs: Avoid compilation warnings.
(defvar coding-system-for-read)
(defvar buffer-file-coding-system)

(defgroup files nil
  "Support editing files."
  :group 'emacs)

(defgroup backup nil
  "Backups of edited data files."
  :group 'files)

(defgroup find-file nil
  "Finding and editing files."
  :group 'files)


;; XEmacs: In buffer.c
;(defconst delete-auto-save-files t
;  "*Non-nil means delete auto-save file when a buffer is saved or killed.")

;; FSF has automount-dir-prefix.  Our directory-abbrev-alist is more general.
;; note: tmp_mnt bogosity conversion is established in paths.el.
(defcustom directory-abbrev-alist nil
  "*Alist of abbreviations for file directories.
A list of elements of the form (FROM . TO), each meaning to replace
FROM with TO when it appears in a directory name.
This replacement is done when setting up the default directory of a
newly visited file.  *Every* FROM string should start with \\\\` or ^.

Use this feature when you have directories which you normally refer to
via absolute symbolic links or to eliminate automounter mount points
from the beginning of your filenames.  Make TO the name of the link,
and FROM the name it is linked to."
  :type '(repeat (cons :format "%v"
		       :value ("\\`" . "")
		       (regexp :tag "From")
		       (regexp :tag "To")))
  :group 'find-file)

(defcustom make-backup-files t
  "*Non-nil means make a backup of a file the first time it is saved.
This can be done by renaming the file or by copying.

Renaming means that XEmacs renames the existing file so that it is a
backup file, then writes the buffer into a new file.  Any other names
that the old file had will now refer to the backup file.  The new file
is owned by you and its group is defaulted.

Copying means that XEmacs copies the existing file into the backup
file, then writes the buffer on top of the existing file.  Any other
names that the old file had will now refer to the new (edited) file.
The file's owner and group are unchanged.

The choice of renaming or copying is controlled by the variables
`backup-by-copying', `backup-by-copying-when-linked' and
`backup-by-copying-when-mismatch'.  See also `backup-inhibited'."
  :type 'boolean
  :group 'backup)

;; Do this so that local variables based on the file name
;; are not overridden by the major mode.
(defvar backup-inhibited nil
  "Non-nil means don't make a backup, regardless of the other parameters.
This variable is intended for use by making it local to a buffer.
But it is local only if you make it local.")
(put 'backup-inhibited 'permanent-local t)

(defcustom backup-by-copying nil
 "*Non-nil means always use copying to create backup files.
See documentation of variable `make-backup-files'."
 :type 'boolean
 :group 'backup)

(defcustom backup-by-copying-when-linked nil
 "*Non-nil means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if `backup-by-copying' is nil."
 :type 'boolean
 :group 'backup)

(defcustom backup-by-copying-when-mismatch nil
  "*Non-nil means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files which are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if `backup-by-copying' is nil."
  :type 'boolean
  :group 'backup)

(defvar backup-enable-predicate
  #'(lambda (name)
     (not (or (null name)
	      (string-match "^/tmp/" name)
	      (let ((tmpdir (temp-directory)))
		(and tmpdir
		     (string-match (concat "\\`" (regexp-quote tmpdir) "/")
				   tmpdir))))))
  "Predicate that looks at a file name and decides whether to make backups.
Called with an absolute file name as argument, it returns t to enable backup.")

(defcustom buffer-offer-save nil
  "*Non-nil in a buffer means offer to save the buffer on exit
even if the buffer is not visiting a file.
Automatically local in all buffers."
  :type 'boolean
  :group 'find-file)
(make-variable-buffer-local 'buffer-offer-save)

;; FSF uses normal defconst
(defvaralias 'find-file-visit-truename 'find-file-use-truenames)
(defvaralias 'find-file-existing-other-name 'find-file-compare-truenames)

(defcustom revert-without-query nil
  "*Specify which files should be reverted without query.
The value is a list of regular expressions.
If the file name matches one of these regular expressions,
then `revert-buffer' reverts the file without querying
if the file has changed on disk and you have not edited the buffer."
  :type '(repeat (regexp ""))
  :group 'find-file)

(defvar buffer-file-number nil
  "The device number and file number of the file visited in the current buffer.
The value is a list of the form (FILENUM DEVNUM).
This pair of numbers uniquely identifies the file.
If the buffer is visiting a new file, the value is nil.")
(make-variable-buffer-local 'buffer-file-number)
(put 'buffer-file-number 'permanent-local t)

(defvar buffer-file-numbers-unique (not (memq system-type '(windows-nt)))
  "Non-nil means that buffer-file-number uniquely identifies files.")

(defcustom file-precious-flag nil
  "*Non-nil means protect against I/O errors while saving files.
Some modes set this non-nil in particular buffers.

This feature works by writing the new contents into a temporary file
and then renaming the temporary file to replace the original.
In this way, any I/O error in writing leaves the original untouched,
and there is never any instant where the file is nonexistent.

Note that this feature forces backups to be made by copying.
Yet, at the same time, saving a precious file
breaks any hard links between it and other files."
  :type 'boolean
  :group 'backup)

(defcustom version-control nil
  "*Control use of version numbers for backup files.
t means make numeric backup versions unconditionally.
nil means make them for files that have some already.
`never' means do not make them."
  :type 'boolean
  :group 'backup
  :group 'vc)

;; This is now defined in efs.
;(defvar dired-kept-versions 2
;  "*When cleaning directory, number of versions to keep.")

(defcustom delete-old-versions (when noninteractive 'leave)
  "*If t, delete excess backup versions silently.
If nil, ask confirmation.  Any other value prevents any trimming."
  :type '(choice (const :tag "Delete" t)
                 (const :tag "Ask" nil)
                 (sexp :tag "Leave" :format "%t\n" other))
  :group 'backup)

(defcustom kept-old-versions 2
  "*Number of oldest versions to keep when a new numbered backup is made."
  :type 'integer
  :group 'backup)

(defcustom kept-new-versions 2
  "*Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0"
  :type 'integer
  :group 'backup)

(defcustom require-final-newline nil
  "*Value of t says silently ensure a file ends in a newline when it is saved.
Non-nil but not t says ask user whether to add a newline when there isn't one.
nil means don't add newlines."
  :type '(choice (const :tag "Off" nil)
		 (const :tag "Add" t)
		 (sexp :tag "Ask" :format "%t\n" ask))
  :group 'editing-basics)

(defcustom auto-save-default t
  "*Non-nil says by default do auto-saving of every file-visiting buffer."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-visited-file-name nil
  "*Non-nil says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names."
  :type 'boolean
  :group 'auto-save)

(defcustom save-abbrevs nil
  "*Non-nil means save word abbrevs too when files are saved.
Loading an abbrev file sets this to t."
  :type 'boolean
  :group 'abbrev)

(defcustom find-file-run-dired t
  "*Non-nil says run dired if `find-file' is given the name of a directory."
  :type 'boolean
  :group 'find-file)

;;;It is not useful to make this a local variable.
;;;(put 'find-file-not-found-hooks 'permanent-local t)
(defvar find-file-not-found-hooks nil
  "List of functions to be called for `find-file' on nonexistent file.
These functions are called as soon as the error is detected.
`buffer-file-name' is already set up.
The functions are called in the order given until one of them returns non-nil.")

;;;It is not useful to make this a local variable.
;;;(put 'find-file-hooks 'permanent-local t)
(defvar find-file-hooks nil
  "List of functions to be called after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(defvar write-file-hooks nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called.
These hooks are considered to pertain to the visited file.
So this list is cleared if you change the visited file name.
See also `write-contents-hooks' and `continue-save-buffer'.")
;;; However, in case someone does make it local...
(put 'write-file-hooks 'permanent-local t)

(defvar local-write-file-hooks nil
  "Just like `write-file-hooks', except intended for per-buffer use.
The functions in this list are called before the ones in
`write-file-hooks'.

This variable is meant to be used for hooks that have to do with a
particular visited file.  Therefore, it is a permanent local, so that
changing the major mode does not clear it.  However, calling
`set-visited-file-name' does clear it.")
(make-variable-buffer-local 'local-write-file-hooks)
(put 'local-write-file-hooks 'permanent-local t)


;; #### think about this (added by Sun).
(put 'after-set-visited-file-name-hooks 'permanent-local t)
(defvar after-set-visited-file-name-hooks nil
  "List of functions to be called after \\[set-visited-file-name]
or during \\[write-file].
You can use this hook to restore local values of `write-file-hooks',
`after-save-hook', and `revert-buffer-function', which pertain
to a specific file and therefore are normally killed by a rename.
Put hooks pertaining to the buffer contents on `write-contents-hooks'
and `revert-buffer-insert-file-contents-function'.")

(defvar write-contents-hooks nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called.
These hooks are considered to pertain to the buffer's contents,
not to the particular visited file; thus, `set-visited-file-name' does
not clear this variable, but changing the major mode does clear it.
See also `write-file-hooks' and `continue-save-buffer'.")

;;  XEmacs addition
;;  Energize needed this to hook into save-buffer at a lower level; we need
;;  to provide a new output method, but don't want to have to duplicate all
;;  of the backup file and file modes logic.that does not occur if one uses
;;  a write-file-hook which returns non-nil.
(put 'write-file-data-hooks 'permanent-local t)
(defvar write-file-data-hooks nil
  "List of functions to be called to put the bytes on disk.
These functions receive the name of the file to write to as argument.
The default behavior is to call
  (write-region (point-min) (point-max) filename nil t)
If one of them returns non-nil, the file is considered already written
and the rest are not called.
These hooks are considered to pertain to the visited file.
So this list is cleared if you change the visited file name.
See also `write-file-hooks'.")

(defcustom enable-local-variables t
  "*Control use of local-variables lists in files you visit.
The value can be t, nil or something else.
A value of t means local-variables lists are obeyed;
nil means they are ignored; anything else means query.

The command \\[normal-mode] always obeys local-variables lists
and ignores this variable."
  :type '(choice (const :tag "Obey" t)
		 (const :tag "Ignore" nil)
		 (sexp :tag "Query" :format "%t\n" other))
  :group 'find-file)

(defcustom enable-local-eval 'maybe
  "*Control processing of the \"variable\" `eval' in a file's local variables.
The value can be t, nil or something else.
A value of t means obey `eval' variables;
nil means ignore them; anything else means query.

The command \\[normal-mode] always obeys local-variables lists
and ignores this variable."
  :type '(choice (const :tag "Obey" t)
		 (const :tag "Ignore" nil)
		 (sexp :tag "Query" :format "%t\n" other))
  :group 'find-file)

;; Avoid losing in versions where CLASH_DETECTION is disabled.
(or (fboundp 'lock-buffer)
    (defalias 'lock-buffer 'ignore))
(or (fboundp 'unlock-buffer)
    (defalias 'unlock-buffer 'ignore))

;;FSFmacs bastardized ange-ftp cruft
;; This hook function provides support for ange-ftp host name
;; completion.  It runs the usual ange-ftp hook, but only for
;; completion operations.  Having this here avoids the need
;; to load ange-ftp when it's not really in use.
;(defun ange-ftp-completion-hook-function (op &rest args)
;  (if (memq op '(file-name-completion file-name-all-completions))
;      (apply 'ange-ftp-hook-function op args)
;    (let ((inhibit-file-name-handlers
;	   (cons 'ange-ftp-completion-hook-function
;		 (and (eq inhibit-file-name-operation op)
;		      inhibit-file-name-handlers)))
;	  (inhibit-file-name-operation op))
;      (apply op args))

(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the current OS."
  (if (eq system-type 'windows-nt)
      (let ((name (copy-sequence filename))
	    (start 0))
	;; leave ':' if part of drive specifier
 	(if (and (> (length name) 1)
 		 (eq (aref name 1) ?:))
	    (setq start 2))
	;; destructively replace invalid filename characters with !
	(while (string-match "[?*:<>|\"\000-\037]" name start)
	  (aset name (match-beginning 0) ?!)
	  (setq start (match-end 0)))
	;; FSF: [convert directory separators to Windows format ...]
	;; unneeded in XEmacs.
	name)
    filename))


(defun pwd ()
  "Show the current default directory."
  (interactive nil)
  (message "Directory %s" default-directory))

(defvar cd-path nil
  "Value of the CDPATH environment variable, as a list.
Not actually set up until the first time you use it.")

(defvar cdpath-previous nil
  "Prior value of the CDPATH environment variable.")

(defun parse-colon-path (cd-path)
  "Explode a colon-separated search path into a list of directory names.

If you think you want to use this, you probably don't.  This function
is provided for backward compatibility.  A more robust implementation
of the same functionality is available as `split-path', which see."
  (and cd-path
       (let (cd-list (cd-start 0) cd-colon)
	 (setq cd-path (concat cd-path path-separator))
	 (while (setq cd-colon (string-match path-separator cd-path cd-start))
	   (setq cd-list
		 (nconc cd-list
			(list (if (= cd-start cd-colon)
				   nil
				(substitute-in-file-name
				 (file-name-as-directory
				  (substring cd-path cd-start cd-colon)))))))
	   (setq cd-start (+ cd-colon 1)))
	 cd-list)))

(defun cd-absolute (dir)
  "Change current directory to given absolute file name DIR."
  ;; Put the name into directory syntax now,
  ;; because otherwise expand-file-name may give some bad results.
  (setq dir (file-name-as-directory dir))
  ;; XEmacs change: stig@hackvan.com
  (if find-file-use-truenames
      (setq dir (file-truename dir)))
  (setq dir (abbreviate-file-name (expand-file-name dir)))
  (cond ((not (file-directory-p dir))
         (error "%s is not a directory" dir))
	;; this breaks ange-ftp, which doesn't (can't?) overload `file-executable-p'.
        ;;((not (file-executable-p dir))
        ;; (error "Cannot cd to %s:  Permission denied" dir))
        (t
         (setq default-directory dir))))

(defun cd (dir)
  "Make DIR become the current buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of that
colon-separated list of directories when resolving a relative directory name."
  (interactive
   ;; XEmacs change? (read-file-name => read-directory-name)
   (list (read-directory-name "Change default directory: "
			      default-directory default-directory
			      (and (member cd-path '(nil ("./")))
				   (null (getenv "CDPATH"))))))
  (if (file-name-absolute-p dir)
      (cd-absolute (expand-file-name dir))
    ;; XEmacs
    (unless (and cd-path (equal (getenv "CDPATH") cdpath-previous))
      ;;#### Unix-specific
      (let ((trypath (parse-colon-path
		      (setq cdpath-previous (getenv "CDPATH")))))
	(setq cd-path (or trypath (list "./")))))
    (or (catch 'found
	  (mapcar #'(lambda (x)
		        (let ((f (expand-file-name (concat x dir))))
			  (if (file-directory-p f)
			      (progn
			        (cd-absolute f)
			        (throw 'found t)))))
		  cd-path)
	  nil)
	;; jwz: give a better error message to those of us with the
	;; good taste not to use a kludge like $CDPATH.
	(if (equal cd-path '("./"))
	    (error "No such directory: %s" (expand-file-name dir))
	  (error "Directory not found in $CDPATH: %s" dir)))))

(defun load-file (file)
  "Load the Lisp file named FILE."
  (interactive "fLoad file: ")
  (load (expand-file-name file) nil nil t))

; We now dump utils/lib-complete.el which has improved versions of this.
;(defun load-library (library)
;  "Load the library named LIBRARY.
;This is an interface to the function `load'."
;  (interactive "sLoad library: ")
;  (load library))
;
;(defun find-library (library)
;  "Find the library of Lisp code named LIBRARY.
;This searches `load-path' for a file named either \"LIBRARY\" or \"LIBRARY.el\"."
;  (interactive "sFind library file: ")
;  (let ((f (locate-file library load-path ":.el:")))
;    (if f
;        (find-file f)
;        (error "Couldn't locate library %s" library))))

(defun file-local-copy (file &optional buffer)
  "Copy the file FILE into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  (let ((handler (find-file-name-handler file 'file-local-copy)))
    (if handler
	(funcall handler 'file-local-copy file)
      nil)))

;; XEmacs change block
; We have this in C and use the realpath() system call.

;(defun file-truename (filename &optional counter prev-dirs)
; [... lots of code snipped ...]
;    filename))

;; XEmacs addition.  Called from `insert-file-contents-internal'
;; at the appropriate time.
(defun compute-buffer-file-truename (&optional buffer)
  "Recompute BUFFER's value of `buffer-file-truename'
based on the current value of `buffer-file-name'.
BUFFER defaults to the current buffer if unspecified."
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (cond ((null buffer-file-name)
	   (setq buffer-file-truename nil))
	  ((setq buffer-file-truename (file-truename buffer-file-name))
	   ;; it exists, we're done.
	   nil)
	  (t
	   ;; the file doesn't exist, but maybe the directory does.
	   (let* ((dir (file-name-directory buffer-file-name))
		  (truedir (file-truename dir)))
	     (if truedir (setq dir truedir))
	     (setq buffer-file-truename
		   (expand-file-name (file-name-nondirectory buffer-file-name)
				     dir)))))
    (if (and find-file-use-truenames buffer-file-truename)
	(setq buffer-file-name (abbreviate-file-name buffer-file-truename)
	      default-directory (file-name-directory buffer-file-name)))
    buffer-file-truename))
;; End XEmacs change block

(defun file-chase-links (filename)
  "Chase links in FILENAME until a name that is not a link.
Does not examine containing directories for links,
unlike `file-truename'."
  (let (tem (count 100) (newname filename))
    (while (setq tem (file-symlink-p newname))
      (save-match-data
	(if (= count 0)
	    (error "Apparent cycle of symbolic links for %s" filename))
	;; In the context of a link, `//' doesn't mean what XEmacs thinks.
	(while (string-match "//+" tem)
	  (setq tem (concat (substring tem 0 (1+ (match-beginning 0)))
			    (substring tem (match-end 0)))))
	;; Handle `..' by hand, since it needs to work in the
	;; target of any directory symlink.
	;; This code is not quite complete; it does not handle
	;; embedded .. in some cases such as ./../foo and foo/bar/../../../lose.
	(while (string-match "\\`\\.\\./" tem) ;#### Unix specific
	  (setq tem (substring tem 3))
	  (setq newname (file-name-as-directory
			 ;; Do the .. by hand.
			 (directory-file-name
			  (file-name-directory
			   ;; Chase links in the default dir of the symlink.
			   (file-chase-links
			    (directory-file-name
			     (file-name-directory newname))))))))
	(setq newname (expand-file-name tem (file-name-directory newname)))
	(setq count (1- count))))
    newname))

(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.  With a numeric arg, n, switch to the nth
most recent buffer.  With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))

(defun switch-to-buffer-other-window (buffer)
  "Select buffer BUFFER in another window."
  (interactive "BSwitch to buffer in other window: ")
  (let ((pop-up-windows t))
    ;; XEmacs: this used to have (selected-frame) as the third argument,
    ;; but this is obnoxious.  If the user wants the buffer in a
    ;; different frame, then it should be this way.

    ;; Change documented above undone --mrb
    (pop-to-buffer buffer t (selected-frame))))

(defun switch-to-buffer-other-frame (buffer)
  "Switch to buffer BUFFER in a newly-created frame."
  (interactive "BSwitch to buffer in other frame: ")
  (let* ((name (get-frame-name-for-buffer buffer))
	 (frame (make-frame (if name
				  (list (cons 'name (symbol-name name)))))))
    (pop-to-buffer buffer t frame)
    (make-frame-visible frame)
    buffer))

(defun find-file (filename &optional codesys)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind file: \nZCoding system: ")
  (if codesys
      (let ((coding-system-for-read
	     (get-coding-system codesys)))
	(switch-to-buffer (find-file-noselect filename)))
    (switch-to-buffer (find-file-noselect filename))))

(defun find-file-other-window (filename &optional codesys)
  "Edit file FILENAME, in another window.
May create a new window, or reuse an existing one.
See the function `display-buffer'.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind file in other window: \nZCoding system: ")
  (if codesys
      (let ((coding-system-for-read
	     (get-coding-system codesys)))
	(switch-to-buffer-other-window (find-file-noselect filename)))
    (switch-to-buffer-other-window (find-file-noselect filename))))

(defun find-file-other-frame (filename &optional codesys)
  "Edit file FILENAME, in a newly-created frame.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind file in other frame: \nZCoding system: ")
  (if codesys
      (let ((coding-system-for-read
	     (get-coding-system codesys)))
	(switch-to-buffer-other-frame (find-file-noselect filename)))
    (switch-to-buffer-other-frame (find-file-noselect filename))))

(defun find-file-read-only (filename &optional codesys)
  "Edit file FILENAME but don't allow changes.
Like \\[find-file] but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "fFind file read-only: \nZCoding system: ")
  (if codesys
      (let ((coding-system-for-read
	     (get-coding-system codesys)))
	(find-file filename))
    (find-file filename))
  (setq buffer-read-only t)
  (current-buffer))

(defun find-file-read-only-other-window (filename &optional codesys)
  "Edit file FILENAME in another window but don't allow changes.
Like \\[find-file-other-window] but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "fFind file read-only other window: \nZCoding system: ")
  (if codesys
      (let ((coding-system-for-read
	     (get-coding-system codesys)))
	(find-file-other-window filename))
    (find-file-other-window filename))
  (setq buffer-read-only t)
  (current-buffer))

(defun find-file-read-only-other-frame (filename &optional codesys)
  "Edit file FILENAME in another frame but don't allow changes.
Like \\[find-file-other-frame] but marks buffer as read-only.
Use \\[toggle-read-only] to permit editing.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "fFind file read-only other frame: \nZCoding system: ")
  (if codesys
      (let ((coding-system-for-read
	     (get-coding-system codesys)))
	(find-file-other-frame filename))
    (find-file-other-frame filename))
  (setq buffer-read-only t)
  (current-buffer))

(defun find-alternate-file-other-window (filename &optional codesys)
  "Find file FILENAME as a replacement for the file in the next window.
This command does not select that window.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive
   (save-selected-window
     (other-window 1)
     (let ((file buffer-file-name)
	   (file-name nil)
	   (file-dir nil))
       (and file
	    (setq file-name (file-name-nondirectory file)
		  file-dir (file-name-directory file)))
       (list (read-file-name
	      "Find alternate file: " file-dir nil nil file-name)
	     (if (and current-prefix-arg (featurep 'mule))
		 (read-coding-system "Coding-system: "))))))
  (if (one-window-p)
      (find-file-other-window filename)
    (save-selected-window
      (other-window 1)
      (find-alternate-file filename codesys))))

(defun find-alternate-file (filename &optional codesys)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name
	    "Find alternate file: " file-dir nil nil file-name)
	   (if (and current-prefix-arg (featurep 'mule))
	       (read-coding-system "Coding-system: ")))))
  (and (buffer-modified-p) (buffer-file-name)
       ;; (not buffer-read-only)
       (not (yes-or-no-p (format
			  "Buffer %s is modified; kill anyway? "
			  (buffer-name))))
       (error "Aborted"))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(onum buffer-file-number)
	(otrue buffer-file-truename)
	(oname (buffer-name)))
    (if (get-buffer " **lose**")
	(kill-buffer " **lose**"))
    (rename-buffer " **lose**")
    (setq buffer-file-name nil)
    (setq buffer-file-number nil)
    (setq buffer-file-truename nil)
    (unwind-protect
	(progn
	  (unlock-buffer)
	  (if codesys
	      (let ((coding-system-for-read
		     (get-coding-system codesys)))
		(find-file filename))
	    (find-file filename)))
      (cond ((eq obuf (current-buffer))
	     (setq buffer-file-name ofile)
	     (setq buffer-file-number onum)
	     (setq buffer-file-truename otrue)
	     (lock-buffer)
	     (rename-buffer oname))))
    (or (eq (current-buffer) obuf)
	(kill-buffer obuf))))

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name."
    (let ((handler (find-file-name-handler filename 'create-file-buffer)))
      (if handler
	  (funcall handler 'create-file-buffer filename)
	(let ((lastname (file-name-nondirectory filename)))
	  (if (string= lastname "")
	      (setq lastname filename))
	  (generate-new-buffer lastname)))))

(defun generate-new-buffer (name)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name'."
  (get-buffer-create (generate-new-buffer-name name)))

(defvar abbreviated-home-dir nil
  "The user's homedir abbreviated according to `directory-abbrev-alist'.")

(defun abbreviate-file-name (filename &optional hack-homedir)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
See documentation of variable `directory-abbrev-alist' for more information.
If optional argument HACK-HOMEDIR is non-nil, then this also substitutes
\"~\" for the user's home directory."
  (let ((handler (find-file-name-handler filename 'abbreviate-file-name)))
    (if handler
	(funcall handler 'abbreviate-file-name filename hack-homedir)
      ;; Get rid of the prefixes added by the automounter.
      ;;(if (and (string-match automount-dir-prefix filename)
      ;;         (file-exists-p (file-name-directory
      ;;                         (substring filename (1- (match-end 0))))))
      ;;    (setq filename (substring filename (1- (match-end 0)))))
      (let ((tail directory-abbrev-alist))
	;; If any elt of directory-abbrev-alist matches this name,
	;; abbreviate accordingly.
	(while tail
	  (when (string-match (car (car tail)) filename)
	    (setq filename
		  (concat (cdr (car tail)) (substring filename (match-end 0)))))
	  (setq tail (cdr tail))))
      (when hack-homedir
	;; Compute and save the abbreviated homedir name.
	;; We defer computing this until the first time it's needed,
	;; to give time for directory-abbrev-alist to be set properly.
	;; We include the separator at the end, to avoid spurious
	;; matches such as `/usr/foobar' when the home dir is
	;; `/usr/foo'.
	(or abbreviated-home-dir
	    (setq abbreviated-home-dir
		  (let ((abbreviated-home-dir "$foo"))
		    (concat "\\`"
			    (regexp-quote
			     (abbreviate-file-name (expand-file-name "~")))
			    "\\("
			    (regexp-quote (string directory-sep-char))
			    "\\|\\'\\)"))))
	;; If FILENAME starts with the abbreviated homedir,
	;; make it start with `~' instead.
	(if (and (string-match abbreviated-home-dir filename)
		 ;; If the home dir is just /, don't change it.
		 (not (and (= (match-end 0) 1)
			   (= (aref filename 0) directory-sep-char)))
		 (not (and (eq system-type 'windows-nt)
			   (save-match-data
			     (string-match (concat "\\`[a-zA-Z]:"
						   (regexp-quote
						    (string directory-sep-char))
						   "\\'")
					   filename)))))
	    (setq filename
		  (concat "~"
			  (match-string 1 filename)
			  (substring filename (match-end 0))))))
      filename)))

(defcustom find-file-not-true-dirname-list nil
  "*List of logical names for which visiting shouldn't save the true dirname."
  :type '(repeat (string :tag "Name"))
  :group 'find-file)

;; This function is needed by FSF vc.el.  I hope somebody can make it
;; work for XEmacs.  -sb.
;; #### In what way does it not work?  --hniksic
(defun find-buffer-visiting (filename)
  "Return the buffer visiting file FILENAME (a string).
This is like `get-file-buffer', except that it checks for any buffer
visiting the same file, possibly under a different name.
If there is no such live buffer, return nil."
  (let ((buf (get-file-buffer filename))
	(truename (abbreviate-file-name (file-truename filename))))
    (or buf
	(let ((list (buffer-list)) found)
	  (while (and (not found) list)
	    (save-excursion
	      (set-buffer (car list))
	      (if (and buffer-file-name
		       (string= buffer-file-truename truename))
		  (setq found (car list))))
	    (setq list (cdr list)))
	  found)
	(let ((number (nthcdr 10 (file-attributes truename)))
	      (list (buffer-list)) found)
	  (and buffer-file-numbers-unique
	       number
	       (while (and (not found) list)
		 (save-excursion
		   (set-buffer (car list))
		   (if (and buffer-file-number
                           (equal buffer-file-number number)
			    ;; Verify this buffer's file number
			    ;; still belongs to its file.
			    (file-exists-p buffer-file-name)
			    (equal (nthcdr 10 (file-attributes buffer-file-name))
				   number))
		       (setq found (car list))))
		 (setq list (cdr list))))
	  found))))

(defun insert-file-contents-literally (filename &optional visit start end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as format decoding, character code
conversion, find-file-hooks, automatic uncompression, etc.

  This function ensures that none of these modifications will take place."
  (let ((wrap-func (find-file-name-handler filename
					   'insert-file-contents-literally)))
    (if wrap-func
	(funcall wrap-func 'insert-file-contents-literally filename
		 visit start end replace)
      (let ((file-name-handler-alist nil)
	    (format-alist nil)
	    (after-insert-file-functions nil)
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (find-buffer-file-type-function
	     (if (fboundp 'find-buffer-file-type)
		 (symbol-function 'find-buffer-file-type)
	       nil)))
	(unwind-protect
	    (progn
	      (fset 'find-buffer-file-type (lambda (filename) t))
	      (insert-file-contents filename visit start end replace))
	  (if find-buffer-file-type-function
	      (fset 'find-buffer-file-type find-buffer-file-type-function)
	    (fmakunbound 'find-buffer-file-type)))))))

(defun find-file-noselect (filename &optional nowarn rawfile)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
If NOWARN is non-nil, warning messages will be suppressed.
If RAWFILE is non-nil, the file is read literally."
  (setq filename (abbreviate-file-name (expand-file-name filename)))
  (if (file-directory-p filename)
      (if (and (fboundp 'dired-noselect) find-file-run-dired)
	  (dired-noselect (if find-file-use-truenames
			      (abbreviate-file-name (file-truename filename))
			    filename))
	(error "%s is a directory" filename))
    (let* ((buf (get-file-buffer filename))
	   (truename (abbreviate-file-name (file-truename filename)))
	   (number (nthcdr 10 (file-attributes truename)))
;	   ;; Find any buffer for a file which has same truename.
;	   (other (and (not buf) (find-buffer-visiting filename)))
           (error nil))

;     ;; Let user know if there is a buffer with the same truename.
;      (if (and (not buf) same-truename (not nowarn))
;	  (message "%s and %s are the same file (%s)"
;		   filename (buffer-file-name same-truename)
;		   truename)
;	(if (and (not buf) same-number (not nowarn))
;	  (message "%s and %s are the same file"
;		   filename (buffer-file-name same-number))))
;      ;; Optionally also find that buffer.
;      (if (or find-file-existing-other-name find-file-visit-truename)
;	  (setq buf (or same-truename same-number)))

      (when (and buf
		 (or find-file-compare-truenames find-file-use-truenames)
		 (not nowarn))
	(save-excursion
	  (set-buffer buf)
	  (if (not (string-equal buffer-file-name filename))
	      (message "%s and %s are the same file (%s)"
		       filename buffer-file-name
		       buffer-file-truename))))

      (if buf
	  (or nowarn
	      (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    ;; Certain files should be reverted automatically
		    ;; if they have changed on disk and not in the buffer.
		    ((and (not (buffer-modified-p buf))
			  (dolist (rx revert-without-query nil)
			    (when (string-match rx filename)
			      (return t))))
		     (with-current-buffer buf
		       (message "Reverting file %s..." filename)
		       (revert-buffer t t)
		       (message "Reverting file %s... done" filename)))
		    ((yes-or-no-p
		      (if (string= (file-name-nondirectory filename)
				   (buffer-name buf))
			  (format
			   (if (buffer-modified-p buf)
	(gettext "File %s changed on disk.  Discard your edits? ")
	(gettext "File %s changed on disk.  Reread from disk? "))
			   (file-name-nondirectory filename))
			(format
			 (if (buffer-modified-p buf)
      (gettext "File %s changed on disk.  Discard your edits in %s? ")
      (gettext "File %s changed on disk.  Reread from disk into %s? "))
			 (file-name-nondirectory filename)
			 (buffer-name buf))))
		     (with-current-buffer buf
		       (revert-buffer t t)))))
	;; Else: we must create a new buffer for filename
	(save-excursion
;;; The truename stuff makes this obsolete.
;;;	  (let* ((link-name (car (file-attributes filename)))
;;;		 (linked-buf (and (stringp link-name)
;;;				  (get-file-buffer link-name))))
;;;	    (if (bufferp linked-buf)
;;;		(message "Symbolic link to file in buffer %s"
;;;			 (buffer-name linked-buf))))
	  (setq buf (create-file-buffer filename))
	  ;; Catch various signals, such as QUIT, and kill the buffer
	  ;; in that case.
	  (condition-case data
	      (progn
		(set-buffer-major-mode buf)
		(set-buffer buf)
		(erase-buffer)
		(condition-case ()
		    (if rawfile
			(insert-file-contents-literally filename t)
		      (insert-file-contents filename t))
		  (file-error
		   (when (and (file-exists-p filename)
			      (not (file-readable-p filename)))
		     (signal 'file-error (list "File is not readable" filename)))
		   (if rawfile
		       ;; Unconditionally set error
		       (setq error t)
		     (or
		      ;; Run find-file-not-found-hooks until one returns non-nil.
		      (run-hook-with-args-until-success 'find-file-not-found-hooks)
		      ;; If they fail too, set error.
		      (setq error t)))))
		;; Find the file's truename, and maybe use that as visited name.
		;; automatically computed in XEmacs, unless jka-compr was used!
		(unless buffer-file-truename
		  (setq buffer-file-truename truename))
		(setq buffer-file-number number)
		(and find-file-use-truenames
		     ;; This should be in C.  Put pathname
		     ;; abbreviations that have been explicitly
		     ;; requested back into the pathname.  Most
		     ;; importantly, strip out automounter /tmp_mnt
		     ;; directories so that auto-save will work
		     (setq buffer-file-name (abbreviate-file-name buffer-file-name)))
		;; Set buffer's default directory to that of the file.
		(setq default-directory (file-name-directory buffer-file-name))
		;; Turn off backup files for certain file names.  Since
		;; this is a permanent local, the major mode won't eliminate it.
		(and (not (funcall backup-enable-predicate buffer-file-name))
		     (progn
		       (make-local-variable 'backup-inhibited)
		       (setq backup-inhibited t)))
		(if rawfile
		    ;; #### FSF 20.3 sets buffer-file-coding-system to
		    ;; `no-conversion' here.  Should we copy?  It also
		    ;; makes `find-file-literally' a local variable
		    ;; and sets it to t.
		    nil
		  (after-find-file error (not nowarn))
		  (setq buf (current-buffer))))
	    (t
	     (kill-buffer buf)
	     (signal (car data) (cdr data))))
	))
      buf)))

;; FSF has `insert-file-literally' and `find-file-literally' here.

(defvar after-find-file-from-revert-buffer nil)

(defun after-find-file (&optional error warn noauto
				  after-find-file-from-revert-buffer
				  nomodes)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses local variables.
Optional args ERROR, WARN, and NOAUTO: ERROR non-nil means there was an
error in reading the file.  WARN non-nil means warn if there
exists an auto-save file more recent than the visited file.
NOAUTO means don't mess with auto-save mode.
Fourth arg AFTER-FIND-FILE-FROM-REVERT-BUFFER non-nil
 means this call was from `revert-buffer'.
Fifth arg NOMODES non-nil means don't alter the file's modes.
Finishes by calling the functions in `find-file-hooks'."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (let* (not-serious
	   (msg
	    (cond ((and error (file-attributes buffer-file-name))
		   (setq buffer-read-only t)
		   (gettext "File exists, but cannot be read."))
		  ((not buffer-read-only)
		   (if (and warn
			    (file-newer-than-file-p (make-auto-save-file-name)
						    buffer-file-name))
		       (format "%s has auto save data; consider M-x recover-file"
			       (file-name-nondirectory buffer-file-name))
		     (setq not-serious t)
		     (if error (gettext "(New file)") nil)))
		  ((not error)
		   (setq not-serious t)
		   (gettext "Note: file is write protected"))
		  ((file-attributes (directory-file-name default-directory))
		   (gettext "File not found and directory write-protected"))
		  ((file-exists-p (file-name-directory buffer-file-name))
		   (setq buffer-read-only nil))
		  (t
		   ;; If the directory the buffer is in doesn't exist,
		   ;; offer to create it.  It's better to do this now
		   ;; than when we save the buffer, because we want
		   ;; autosaving to work.
		   (setq buffer-read-only nil)
		   ;; XEmacs
		   (or (file-exists-p (file-name-directory buffer-file-name))
		       (condition-case nil
			   (if (yes-or-no-p
				(format
				 "\
The directory containing %s does not exist.  Create? "
				 (abbreviate-file-name buffer-file-name)))
			       (make-directory (file-name-directory
						buffer-file-name)
					       t))
			 (quit
			  (kill-buffer (current-buffer))
			  (signal 'quit nil))))
		   nil))))
      (if msg
	  (progn
	    (message "%s" msg)
	    (or not-serious (sit-for 1 t)))))
    (if (and auto-save-default (not noauto))
	(auto-save-mode t)))
  (unless nomodes
    (normal-mode t)
    (run-hooks 'find-file-hooks)))

(defun normal-mode (&optional find-file)
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec.

This function is called automatically from `find-file'.  In that case,
we may set up specified local variables depending on the value of
`enable-local-variables': if it is t, we do; if it is nil, we don't;
otherwise, we query.  `enable-local-variables' is ignored if you
run `normal-mode' explicitly."
  (interactive)
  (or find-file (funcall (or default-major-mode 'fundamental-mode)))
  (and (condition-case err
           (progn (set-auto-mode)
                  t)
         (error (message "File mode specification error: %s"
                         (prin1-to-string err))
                nil))
       (condition-case err
           (hack-local-variables (not find-file))
         (error (lwarn 'local-variables 'warning
		  "File local-variables error: %s"
		  (error-message-string err))))))

;; `auto-mode-alist' used to contain entries for modes in core and in packages.
;; The applicable entries are now located in the corresponding modes in
;; packages, the ones here are for core modes.  Ditto for
;; `interpreter-mode-alist' below.
;; Per Abrahamsen suggested splitting auto-mode-alist to
;; several distinct variables such as, in order of precedence,
;; `user-auto-mode-alist' for users, `package-auto-mode-alist' for
;; packages and `auto-mode-alist' (which might also be called
;; `default-auto-mode-alist') for default stuff, such as some of the
;; entries below.

(defvar auto-mode-alist
  '(("\\.te?xt\\'" . text-mode)
    ("\\.el\\'" . emacs-lisp-mode)
    ("\\.c?l\\(?:i?sp\\)?\\'" . lisp-mode)
    ("\\.article\\'" . text-mode)
    ("\\.letter\\'" . text-mode)
    ;; Mailer puts message to be edited in /tmp/Re.... or Message
    ;; #### Unix-specific!
    ("\\`/tmp/Re" . text-mode)
    ("/Message[0-9]*\\'" . text-mode)
    ;; some news reader is reported to use this
    ("^/tmp/fol/" . text-mode)
    ;; .emacs following a directory delimiter in either Unix or
    ;; Windows syntax.
    ("[/\\][._].*emacs\\'" . emacs-lisp-mode)
    ("\\.ml\\'" . lisp-mode)
    )
"Alist of filename patterns vs. corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (REGEXP FUNCTION NON-NIL).
\(NON-NIL stands for anything that is not nil; the value does not matter.)
Visiting a file whose name matches REGEXP specifies FUNCTION as the
mode function to use.  FUNCTION will be called, unless it is nil.

If the element has the form (REGEXP FUNCTION NON-NIL), then after
calling FUNCTION (if it's not nil), we delete the suffix that matched
REGEXP and search the list again for another match.")

(defvar interpreter-mode-alist
  '(("emacs" . emacs-lisp-mode))
  "Alist mapping interpreter names to major modes.
This alist is used to guess the major mode of a file based on the
contents of the first line.  This line often contains something like:
#!/bin/sh
but may contain something more imaginative like
#! /bin/env python
or
eval 'exec perl -w -S $0 ${1+\"$@\"}'.

Each alist element looks like (INTERPRETER . MODE).
The car of each element is a regular expression which is compared
with the name of the interpreter specified in the first line.
If it matches, mode MODE is selected.")

(defvar binary-file-regexps
  '("\\.\\(?:bz2\\|elc\\|g\\(if\\|z\\)\\|jp\\(eg\\|g\\)\\|png\\|t\\(ar\\|gz\\|iff\\)\\|[Zo]\\)\\'")
  "List of regexps of filenames containing binary (non-text) data.")

;   (eval-when-compile
;     (require 'regexp-opt)
;     (list
;      (format "\\.\\(?:%s\\)\\'"
;	      (regexp-opt
;	       '("tar"
;		 "tgz"
;		 "gz"
;		 "bz2"
;		 "Z"
;		 "o"
;		 "elc"
;		 "png"
;		 "gif"
;		 "tiff"
;		 "jpg"
;		 "jpeg"))))))

(defvar inhibit-first-line-modes-regexps
  binary-file-regexps
  "List of regexps; if one matches a file name, don't look for `-*-'.")

(defvar inhibit-first-line-modes-suffixes nil
  "List of regexps for what to ignore, for `inhibit-first-line-modes-regexps'.
When checking `inhibit-first-line-modes-regexps', we first discard
from the end of the file name anything that matches one of these regexps.")

(defvar user-init-file
  nil ; set by command-line
  "File name including directory of user's initialization file.")

(defun set-auto-mode (&optional just-from-file-name)
  "Select major mode appropriate for current buffer.
This checks for a -*- mode tag in the buffer's text,
compares the filename against the entries in `auto-mode-alist',
or checks the interpreter that runs this file against
`interpreter-mode-alist'.

It does not check for the `mode:' local variable in the
Local Variables section of the file; for that, use `hack-local-variables'.

If `enable-local-variables' is nil, this function does not check for a
-*- mode tag.

If the optional argument JUST-FROM-FILE-NAME is non-nil,
then we do not set anything but the major mode,
and we don't even do that unless it would come from the file name."
  (save-excursion
    ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
    ;; Do this by calling the hack-local-variables helper to avoid redundancy.
    ;; We bind enable-local-variables to nil this time because we're going to
    ;; call hack-local-variables-prop-line again later, "for real."  Note that
    ;; this temporary binding does not prevent hack-local-variables-prop-line
    ;; from setting the major mode.
    (or (and enable-local-variables
	     (let ((enable-local-variables nil))
	       (hack-local-variables-prop-line nil))
	     )
	;; It's not in the -*- line, so check the auto-mode-alist, unless
	;; this buffer isn't associated with a file.
	(null buffer-file-name)
	(let ((name (file-name-sans-versions buffer-file-name))
              (keep-going t))
          (while keep-going
            (setq keep-going nil)
            (let ((alist auto-mode-alist)
                  (mode nil))

              ;; Find first matching alist entry.

	      ;; #### This is incorrect. In NT, case sensitivity is a volume
	      ;; property. For instance, NFS mounts *are* case sensitive.
	      ;; Need internal function (file-name-case-sensitive f), F
	      ;; being file or directory name. - kkm
	      (let ((case-fold-search
		     (eq system-type 'windows-nt)))
		(while (and (not mode) alist)
		  (if (string-match (car (car alist)) name)
		      (if (and (consp (cdr (car alist)))
			       (nth 2 (car alist)))
			  (progn
			    (setq mode (car (cdr (car alist)))
				  name (substring name 0 (match-beginning 0))
				  keep-going t))
			(setq mode (cdr (car alist))
			      keep-going nil)))
		  (setq alist (cdr alist))))
	      (unless just-from-file-name
		;; If we can't deduce a mode from the file name,
		;; look for an interpreter specified in the first line.
		(if (and (null mode)
			 (save-excursion ; XEmacs
			   (goto-char (point-min))
			   (looking-at "#!")))
		    (let ((firstline
			   (buffer-substring
			    (point-min)
			    (save-excursion
			      (goto-char (point-min)) (end-of-line) (point)))))
		      (setq alist interpreter-mode-alist)
		      (while alist
			(if (string-match (car (car alist)) firstline)
			    (progn
			      (setq mode (cdr (car alist)))
			      (setq alist nil))
			  (setq alist (cdr alist)))))))
              (if mode
		  (if (not (fboundp mode))
                      (let ((name (package-get-package-provider mode)))
                        (if name
                            (message "Mode %s is not installed.  Download package %s" mode name)
                          (message "Mode %s either doesn't exist or is not a known package" mode))
                        (sit-for 2)
                        (error "%s" mode))
		    (unless (and just-from-file-name
				 (or
				  ;; Don't reinvoke major mode.
				  (eq mode major-mode)
				  ;; Don't lose on minor modes.
				  (assq mode minor-mode-alist)))
		      (funcall mode))))))))))

(defvar hack-local-variables-hook nil
  "Normal hook run after processing a file's local variables specs.
Major modes can use this to examine user-specified local variables
in order to initialize other data structure based on them.

This hook runs even if there were no local variables or if their
evaluation was suppressed.  See also `enable-local-variables' and
`enable-local-eval'.")

(defun hack-local-variables (&optional force)
  "Parse, and bind or evaluate as appropriate, any local variables
for current buffer."
  ;; Don't look for -*- if this file name matches any
  ;; of the regexps in inhibit-first-line-modes-regexps.
  (if (or (null buffer-file-name) ; don't lose if buffer has no file!
	  (not (let ((temp inhibit-first-line-modes-regexps)
		     (name (if buffer-file-name
			       (file-name-sans-versions buffer-file-name)
			     (buffer-name))))
		 (while (let ((sufs inhibit-first-line-modes-suffixes))
			  (while (and sufs (not
					    (string-match (car sufs) name)))
			    (setq sufs (cdr sufs)))
			  sufs)
		   (setq name (substring name 0 (match-beginning 0))))
		 (while (and temp
			     (not (string-match (car temp) name)))
		   (setq temp (cdr temp))
		   temp))))
      (progn
        ;; Look for variables in the -*- line.
        (hack-local-variables-prop-line force)
        ;; Look for "Local variables:" block in last page.
        (hack-local-variables-last-page force)))
  (run-hooks 'hack-local-variables-hook))

;;; Local variables may be specified in the last page of the file (within 3k
;;; from the end of the file and after the last ^L) in the form
;;;
;;;   Local variables:
;;;   variable-name: variable-value
;;;   end:
;;;
;;; The lines may begin with a common prefix, like ";;;   " in the above
;;; example.  They may also have a common suffix (" */" for example).  In
;;; this form, the local variable "mode" can be used to change the major
;;; mode, and the local variable "eval" can be used to evaluate an arbitrary
;;; form.
;;;
;;; Local variables may also be specified in the first line of the file.
;;; Embedded in this line are a pair of "-*-" sequences.  What lies between
;;; them are variable-name/variable-value pairs, like:
;;;
;;;	 -*- mode: emacs-lisp -*-
;;; or	 -*- mode: postscript; version-control: never -*-
;;; or	 -*- tags-file-name: "/foo/bar/TAGS" -*-
;;;
;;; The local variable "eval" is not used with this form. For hysterical
;;; reasons, the syntax "-*- modename -*-" is allowed as well.
;;;

(defun hack-local-variables-p (modeline)
  (or (eq enable-local-variables t)
      (and enable-local-variables
           (save-window-excursion
             (condition-case nil
                 (switch-to-buffer (current-buffer))
               (error
                ;; If we fail to switch in the selected window,
                ;; it is probably a minibuffer.
                ;; So try another window.
                (condition-case nil
                    (switch-to-buffer-other-window (current-buffer))
                  (error
                   (switch-to-buffer-other-frame (current-buffer))))))
             (or modeline (save-excursion
                             (beginning-of-line)
                             (set-window-start (selected-window) (point))))
             (y-or-n-p (format
                        "Set local variables as specified %s of %s? "
                        (if modeline "in -*- line" "at end")
                        (if buffer-file-name
                            (file-name-nondirectory buffer-file-name)
                            (concat "buffer " (buffer-name)))))))))

(defun hack-local-variables-last-page (&optional force)
  ;; Set local variables set in the "Local Variables:" block of the last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (if (let ((case-fold-search t))
	  (and (search-forward "Local Variables:" nil t)
	       (or force
                   (hack-local-variables-p nil))))
	(let ((continue t)
	      prefix prefixlen suffix start
              (enable-local-eval enable-local-eval))
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
	  (skip-chars-forward " \t")
	  (or (eolp)
	      (setq suffix (buffer-substring (point)
					     (progn (end-of-line) (point)))))
	  (goto-char (match-beginning 0))
	  (or (bolp)
	      (setq prefix
		    (buffer-substring (point)
				      (progn (beginning-of-line) (point)))))
	  (if prefix (setq prefixlen (length prefix)
			   prefix (regexp-quote prefix)))
	  (if suffix (setq suffix (concat (regexp-quote suffix) "$")))
	  (while continue
	    ;; Look at next local variable spec.
	    (if selective-display (re-search-forward "[\n\C-m]")
	      (forward-line 1))
	    ;; Skip the prefix, if any.
	    (if prefix
		(if (looking-at prefix)
		    (forward-char prefixlen)
		  (error "Local variables entry is missing the prefix")))
	    ;; Find the variable name; strip whitespace.
	    (skip-chars-forward " \t")
	    (setq start (point))
	    (skip-chars-forward "^:\n")
	    (if (eolp) (error "Missing colon in local variables entry"))
	    (skip-chars-backward " \t")
	    (let* ((str (buffer-substring start (point)))
		   (var (read str))
		  val)
	      ;; Setting variable named "end" means end of list.
	      (if (string-equal (downcase str) "end")
		  (setq continue nil)
		;; Otherwise read the variable value.
		(skip-chars-forward "^:")
		(forward-char 1)
		(setq val (read (current-buffer)))
		(skip-chars-backward "\n")
		(skip-chars-forward " \t")
		(or (if suffix (looking-at suffix) (eolp))
		    (error "Local variables entry is terminated incorrectly"))
		;; Set the variable.  "Variables" mode and eval are funny.
                (hack-one-local-variable var val))))))))

;; jwz - New Version 20.1/19.15
(defun hack-local-variables-prop-line (&optional force)
  ;; Set local variables specified in the -*- line.
  ;; Returns t if mode was set.
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r")
      (let ((end (save-excursion
		   ;; If the file begins with "#!"
		   ;; (un*x exec interpreter magic), look
		   ;; for mode frobs in the first two
		   ;; lines.  You cannot necessarily
		   ;; put them in the first line of
		   ;; such a file without screwing up
		   ;; the interpreter invocation.
		   (end-of-line (and (looking-at "^#!") 2))
		   (point))))
	;; Parse the -*- line into the `result' alist.
	(cond ((not (search-forward "-*-" end t))
	       ;; doesn't have one.
	       (setq force t))
	      ((looking-at "[ \t]*\\([^ \t\n\r:;]+?\\)\\([ \t]*-\\*-\\)")
	       ;; Antiquated form: "-*- ModeName -*-".
	       (setq result
		     (list (cons 'mode
				 (intern (buffer-substring
					  (match-beginning 1)
					  (match-end 1)))))
		     ))
	      (t
	       ;; Usual form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	       ;; (last ";" is optional).
	       (save-excursion
		 (if (search-forward "-*-" end t)
		     (setq end (- (point) 3))
		   (error "-*- not terminated before end of line")))
	       (while (< (point) end)
		 (or (looking-at "[ \t]*\\([^ \t\n:]+\\)[ \t]*:[ \t]*")
		     (error "malformed -*- line"))
		 (goto-char (match-end 0))
		 ;; There used to be a downcase here,
		 ;; but the manual didn't say so,
		 ;; and people want to set var names that aren't all lc.
		 (let ((key (intern (buffer-substring
				     (match-beginning 1)
				     (match-end 1))))
		       (val (save-restriction
			      (narrow-to-region (point) end)
			      (read (current-buffer)))))
		   ;; Case sensitivity!  Icepicks in my forehead!
		   (if (equal (downcase (symbol-name key)) "mode")
		       (setq key 'mode))
		   (setq result (cons (cons key val) result))
		   (skip-chars-forward " \t;")))
	       (setq result (nreverse result))))))

    (let ((set-any-p (or force
			 ;; It's OK to force null specifications.
			 (null result)
			 ;; It's OK to force mode-only specifications.
			 (let ((remaining result)
			       (mode-specs-only t))
			   (while remaining
			     (if (eq (car (car remaining)) 'mode)
				 (setq remaining (cdr remaining))
			       ;; Otherwise, we have a real local.
			       (setq mode-specs-only nil
				     remaining nil))
			     )
			   mode-specs-only)
			 ;; Otherwise, check.
			 (hack-local-variables-p t)))
	  (mode-p nil))
      (while result
	(let ((key (car (car result)))
	      (val (cdr (car result))))
	  (cond ((eq key 'mode)
		 (setq mode-p t)
		 (let ((mode (intern (concat (downcase (symbol-name val))
					     "-mode"))))
		   ;; Without this guard, `normal-mode' would potentially run
		   ;; the major mode function twice: once via `set-auto-mode'
		   ;; and once via `hack-local-variables'.
		   (if (not (eq mode major-mode))
		       (funcall mode))
		   ))
		(set-any-p
		 (hack-one-local-variable key val))
		(t
		 nil)))
	(setq result (cdr result)))
      mode-p)))

(defconst ignored-local-variables
  (list 'enable-local-eval)
  "Variables to be ignored in a file's local variable spec.")

;; Get confirmation before setting these variables as locals in a file.
(put 'debugger 'risky-local-variable t)
(put 'enable-local-eval 'risky-local-variable t)
(put 'ignored-local-variables 'risky-local-variable t)
(put 'eval 'risky-local-variable t)
(put 'file-name-handler-alist 'risky-local-variable t)
(put 'minor-mode-map-alist 'risky-local-variable t)
(put 'after-load-alist 'risky-local-variable t)
(put 'buffer-file-name 'risky-local-variable t)
(put 'buffer-auto-save-file-name 'risky-local-variable t)
(put 'buffer-file-truename 'risky-local-variable t)
(put 'exec-path 'risky-local-variable t)
(put 'load-path 'risky-local-variable t)
(put 'exec-directory 'risky-local-variable t)
(put 'process-environment 'risky-local-variable t)
;; Don't wait for outline.el to be loaded, for the sake of outline-minor-mode.
(put 'outline-level 'risky-local-variable t)
(put 'rmail-output-file-alist 'risky-local-variable t)

;; This one is safe because the user gets to check it before it is used.
(put 'compile-command 'safe-local-variable t)

;(defun hack-one-local-variable-quotep (exp)
;  (and (consp exp) (eq (car exp) 'quote) (consp (cdr exp))))

;; "Set" one variable in a local variables spec.
;; A few variable names are treated specially.
(defun hack-one-local-variable (var val)
  (cond ((eq var 'mode)
	 (funcall (intern (concat (downcase (symbol-name val))
				  "-mode"))))
	((memq var ignored-local-variables)
	 nil)
	;; "Setting" eval means either eval it or do nothing.
	;; Likewise for setting hook variables.
	((or (get var 'risky-local-variable)
	     (and
	      (string-match "-hooks?$\\|-functions?$\\|-forms?$\\|-program$\\|-command$"
			    (symbol-name var))
	      (not (get var 'safe-local-variable))))
;	 ;; Permit evaling a put of a harmless property
;	 ;; if the args do nothing tricky.
;	 (if (or (and (eq var 'eval)
;		      (consp val)
;		      (eq (car val) 'put)
;		      (hack-one-local-variable-quotep (nth 1 val))
;		      (hack-one-local-variable-quotep (nth 2 val))
;		      ;; Only allow safe values of lisp-indent-hook;
;		      ;; not functions.
;		      (or (numberp (nth 3 val))
;			  (equal (nth 3 val) ''defun))
;		      (memq (nth 1 (nth 2 val))
;			    '(lisp-indent-hook)))
	 (if (and (not (zerop (user-uid)))
		  (or (eq enable-local-eval t)
		      (and enable-local-eval
			   (save-window-excursion
			     (switch-to-buffer (current-buffer))
			     (save-excursion
			       (beginning-of-line)
			       (set-window-start (selected-window) (point)))
			     (setq enable-local-eval
				   (y-or-n-p (format "Process `eval' or hook local variables in file %s? "
						     (file-name-nondirectory buffer-file-name))))))))
	     (if (eq var 'eval)
		 (save-excursion (eval val))
	       (make-local-variable var)
	       (set var val))
	   (message "Ignoring `eval:' in file's local variables")))
	;; Ordinary variable, really set it.
	(t (make-local-variable var)
	   (set var val))))

(defcustom change-major-mode-with-file-name t
  "*Non-nil means \\[write-file] should set the major mode from the file name.
However, the mode will not be changed if
\(1) a local variables list or the `-*-' line specifies a major mode, or
\(2) the current major mode is a \"special\" mode,
\    not suitable for ordinary files, or
\(3) the new file name does not particularly specify any mode."
  :type 'boolean
  :group 'editing-basics)

(defun set-visited-file-name (filename &optional no-query along-with-file)
  "Change name of file visited in current buffer to FILENAME.
The next time the buffer is saved it will go in the newly specified file.
nil or empty string as argument means make buffer not be visiting any file.
Remember to delete the initial contents of the minibuffer
if you wish to pass an empty string as the argument.

The optional second argument NO-QUERY, if non-nil, inhibits asking for
confirmation in the case where another buffer is already visiting FILENAME.

The optional third argument ALONG-WITH-FILE, if non-nil, means that
the old visited file has been renamed to the new name FILENAME."
  (interactive "FSet visited file name: ")
  (if (buffer-base-buffer)
      (error "An indirect buffer cannot visit a file"))
  (let (truename)
    (if filename
	(setq filename
	      (if (string-equal filename "")
		  nil
		(expand-file-name filename))))
    (if filename
	(progn
	  (setq truename (file-truename filename))
	  ;; #### Do we need to check if truename is non-nil?
	  (if find-file-use-truenames
	      (setq filename truename))))
    (let ((buffer (and filename (find-buffer-visiting filename))))
      (and buffer (not (eq buffer (current-buffer)))
	   (not no-query)
	   (not (y-or-n-p (message "A buffer is visiting %s; proceed? "
				   filename)))
	   (error "Aborted")))
    (or (equal filename buffer-file-name)
	(progn
	  (and filename (lock-buffer filename))
	  (unlock-buffer)))
    (setq buffer-file-name filename)
    (if filename			; make buffer name reflect filename.
	(let ((new-name (file-name-nondirectory buffer-file-name)))
	  (if (string= new-name "")
	      (error "Empty file name"))
	  (setq default-directory (file-name-directory buffer-file-name))
	  (or (string= new-name (buffer-name))
	      (rename-buffer new-name t))))
    (setq buffer-backed-up nil)
    (or along-with-file
	(clear-visited-file-modtime))
    (compute-buffer-file-truename) ; insert-file-contents does this too.
;    ;; Abbreviate the file names of the buffer.
;    (if truename
;	 (progn
;	   (setq buffer-file-truename (abbreviate-file-name truename))
;	   (if find-file-visit-truename
;	       (setq buffer-file-name buffer-file-truename))))
    (setq buffer-file-number
	  (if filename
	      (nthcdr 10 (file-attributes buffer-file-name))
	      nil)))
  ;; write-file-hooks is normally used for things like ftp-find-file
  ;; that visit things that are not local files as if they were files.
  ;; Changing to visit an ordinary local file instead should flush the hook.
  (kill-local-variable 'write-file-hooks)
  (kill-local-variable 'after-save-hook)
  (kill-local-variable 'local-write-file-hooks)
  (kill-local-variable 'write-file-data-hooks)
  (kill-local-variable 'revert-buffer-function)
  (kill-local-variable 'backup-inhibited)
  ;; If buffer was read-only because of version control,
  ;; that reason is gone now, so make it writable.
  (when (boundp 'vc-mode)
    (if vc-mode
	(setq buffer-read-only nil))
    (kill-local-variable 'vc-mode))
  ;; Turn off backup files for certain file names.
  ;; Since this is a permanent local, the major mode won't eliminate it.
  (and buffer-file-name
       (not (funcall backup-enable-predicate buffer-file-name))
       (progn
	 (make-local-variable 'backup-inhibited)
	 (setq backup-inhibited t)))
  (let ((oauto buffer-auto-save-file-name))
    ;; If auto-save was not already on, turn it on if appropriate.
    (if (not buffer-auto-save-file-name)
	(and buffer-file-name auto-save-default
	     (auto-save-mode t))
      ;; If auto save is on, start using a new name.
      ;; We deliberately don't rename or delete the old auto save
      ;; for the old visited file name.  This is because perhaps
      ;; the user wants to save the new state and then compare with the
      ;; previous state from the auto save file.
      (setq buffer-auto-save-file-name
	    (make-auto-save-file-name)))
    ;; Rename the old auto save file if any.
    (and oauto buffer-auto-save-file-name
	 (file-exists-p oauto)
	 (rename-file oauto buffer-auto-save-file-name t)))
  (if buffer-file-name
      (not along-with-file)
      (set-buffer-modified-p t))
  ;; Update the major mode, if the file name determines it.
  (condition-case nil
      ;; Don't change the mode if it is special.
      (or (not change-major-mode-with-file-name)
	  (get major-mode 'mode-class)
	  ;; Don't change the mode if the local variable list specifies it.
	  (hack-local-variables t)
	  (set-auto-mode t))
    (error nil))
  ;; #### ??
  (run-hooks 'after-set-visited-file-name-hooks))

(defun write-file (filename &optional confirm codesys)
  "Write current buffer into file FILENAME.
Makes buffer visit that file, and marks it not modified.
If the buffer is already visiting a file, you can specify
a directory name as FILENAME, to write a file of the same
old name in that directory.
If optional second arg CONFIRM is non-nil,
ask for confirmation for overwriting an existing file.
Under XEmacs/Mule, optional third argument specifies the
coding system to use when encoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
;;  (interactive "FWrite file: ")
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write file: "
				 nil nil nil nil)
	   (read-file-name "Write file: "
			       (cdr (assq 'default-directory
					  (buffer-local-variables)))
			       nil nil (buffer-name)))
	 t
	 (if (and current-prefix-arg (featurep 'file-coding))
	     (read-coding-system "Coding system: "))))
  (and (eq (current-buffer) mouse-grabbed-buffer)
       (error "Can't write minibuffer window"))
  (or (null filename) (string-equal filename "")
      (progn
	;; If arg is just a directory,
	;; use same file name, but in that directory.
	(if (and (file-directory-p filename) buffer-file-name)
	    (setq filename (concat (file-name-as-directory filename)
				   (file-name-nondirectory buffer-file-name))))
	(and confirm
	     (file-exists-p filename)
	     (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
		 (error "Canceled")))
	(set-visited-file-name filename)))
  (set-buffer-modified-p t)
  (setq buffer-read-only nil)
  (if codesys
      (let ((buffer-file-coding-system (get-coding-system codesys)))
	(save-buffer))
    (save-buffer)))

(defun backup-buffer ()
  "Make a backup of the disk file visited by the current buffer, if appropriate.
This is normally done before saving the buffer the first time.
If the value is non-nil, it is the result of `file-modes' on the original file;
this means that the caller, after saving the buffer, should change the modes
of the new file to agree with the old modes."
  (if buffer-file-name
      (let ((handler (find-file-name-handler buffer-file-name 'backup-buffer)))
	(if handler
	    (funcall handler 'backup-buffer)
	  (if (and make-backup-files
		   (not backup-inhibited)
		   (not buffer-backed-up)
		   (file-exists-p buffer-file-name)
		   (memq (aref (elt (file-attributes buffer-file-name) 8) 0)
			 '(?- ?l)))
	      (let ((real-file-name buffer-file-name)
		    backup-info backupname targets setmodes)
		;; If specified name is a symbolic link, chase it to the target.
		;; Thus we make the backups in the directory where the real file is.
		(setq real-file-name (file-chase-links real-file-name))
		(setq backup-info (find-backup-file-name real-file-name)
		      backupname (car backup-info)
		      targets (cdr backup-info))
;;;     (if (file-directory-p buffer-file-name)
;;;         (error "Cannot save buffer in directory %s" buffer-file-name))
		(if backup-info
		    (condition-case ()
			(let ((delete-old-versions
			       ;; If have old versions to maybe delete,
			       ;; ask the user to confirm now, before doing anything.
			       ;; But don't actually delete till later.
			       (and targets
				    (or (eq delete-old-versions t)
					(eq delete-old-versions nil))
				    (or delete-old-versions
					(y-or-n-p (format "Delete excess backup versions of %s? "
							  real-file-name))))))
			  ;; Actually write the back up file.
			  (condition-case ()
			      (if (or file-precious-flag
					;			  (file-symlink-p buffer-file-name)
				      backup-by-copying
				      (and backup-by-copying-when-linked
					   (> (file-nlinks real-file-name) 1))
				      (and backup-by-copying-when-mismatch
					   (let ((attr (file-attributes real-file-name)))
					     (or (nth 9 attr)
						 (not (file-ownership-preserved-p real-file-name))))))
				  (condition-case ()
				      (copy-file real-file-name backupname t t)
				    (file-error
				     ;; If copying fails because file BACKUPNAME
				     ;; is not writable, delete that file and try again.
				     (if (and (file-exists-p backupname)
					      (not (file-writable-p backupname)))
					 (delete-file backupname))
				     (copy-file real-file-name backupname t t)))
				;; rename-file should delete old backup.
				(rename-file real-file-name backupname t)
				(setq setmodes (file-modes backupname)))
			    (file-error
			     ;; If trouble writing the backup, write it in ~.
			     (setq backupname
				   (expand-file-name
				    (convert-standard-filename "~/%backup%~")))
			     (message "Cannot write backup file; backing up in ~/%%backup%%~")
			     (sleep-for 1)
			     (condition-case ()
				 (copy-file real-file-name backupname t t)
			       (file-error
				;; If copying fails because file BACKUPNAME
				;; is not writable, delete that file and try again.
				(if (and (file-exists-p backupname)
					 (not (file-writable-p backupname)))
				    (delete-file backupname))
				(copy-file real-file-name backupname t t)))))
			  (setq buffer-backed-up t)
			  ;; Now delete the old versions, if desired.
			  (if delete-old-versions
			      (while targets
				(ignore-file-errors (delete-file (car targets)))
				(setq targets (cdr targets))))
			  setmodes)
		      (file-error nil)))))))))

(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return FILENAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions name keep-backup-version)
      (substring name 0
		 (if keep-backup-version
		     (length name)
		   (or (string-match "\\.~[0-9.]+~\\'" name)
		       ;; XEmacs - VC uses extensions like ".~tagname~" or ".~1.1.5.2~"
		       (let ((pos (string-match "\\.~\\([^.~ \t]+\\|[0-9.]+\\)~\\'" name)))
			 (and pos
			      ;; #### - is this filesystem check too paranoid?
			      (file-exists-p (substring name 0 pos))
			      pos))
		       (string-match "~\\'" name)
		       (length name)))))))

(defun file-ownership-preserved-p (file)
  "Return t if deleting FILE and rewriting it would preserve the owner."
  (let ((handler (find-file-name-handler file 'file-ownership-preserved-p)))
    (if handler
	(funcall handler 'file-ownership-preserved-p file)
      (let ((attributes (file-attributes file)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (= (nth 2 attributes) (user-uid)))))))

(defun file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.'."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (string-match "\\.[^.]*\\'" file)
	  (if (setq directory (file-name-directory filename))
	      (expand-file-name (substring file 0 (match-beginning 0))
				directory)
	    (substring file 0 (match-beginning 0)))
	filename))))

(defun file-name-extension (filename &optional period)
  "Return FILENAME's final \"extension\".
The extension, in a file name, is the part that follows the last `.'.
Return nil for extensionless file names such as `foo'.
Return the empty string for file names such as `foo.'.

If PERIOD is non-nil, then the returned value includes the period
that delimits the extension, and if FILENAME has no extension,
the value is \"\"."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (if (string-match "\\.[^.]*\\'" file)
          (substring file (+ (match-beginning 0) (if period 0 1)))
        (if period
            "")))))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  ;; FSF has code here for MS-DOS short filenames, not supported in XEmacs.
    (concat file "~"))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine `file-name-sans-versions' as well."
  (string-match "~\\'" file))

;; This is used in various files.
;; The usage of bv-length is not very clean,
;; but I can't see a good alternative,
;; so as of now I am leaving it alone.
(defun backup-extract-version (fn)
  "Given the name of a numeric backup file, return the backup number.
Uses the free variable `bv-length', whose value should be
the index in the name where the version number begins."
  (declare (special bv-length))
  (if (and (string-match "[0-9]+~\\'" fn bv-length)
	   (= (match-beginning 0) bv-length))
      (string-to-int (substring fn bv-length -1))
      0))

(defun find-backup-file-name (fn)
  "Find a file name for a backup file, and suggestions for deletions.
Value is a list whose car is the name for the backup file
 and whose cdr is a list of old versions to consider deleting now.
If the value is nil, don't make a backup."
  (declare (special bv-length))
  (let ((handler (find-file-name-handler fn 'find-backup-file-name)))
    ;; Run a handler for this function so that ange-ftp can refuse to do it.
    (if handler
	(funcall handler 'find-backup-file-name fn)
      (if (eq version-control 'never)
	  (list (make-backup-file-name fn))
	(let* ((base-versions (concat (file-name-nondirectory fn) ".~"))
	       ;; used by backup-extract-version:
	       (bv-length (length base-versions))
	       possibilities
	       (versions nil)
	       (high-water-mark 0)
	       (deserve-versions-p nil)
	       (number-to-delete 0))
	  (condition-case ()
	      (setq possibilities (file-name-all-completions
				   base-versions
				   (file-name-directory fn))
		    versions (sort (mapcar
				    #'backup-extract-version
				    possibilities)
				   '<)
		    high-water-mark (apply #'max 0 versions)
		    deserve-versions-p (or version-control
					   (> high-water-mark 0))
		    number-to-delete (- (length versions)
					kept-old-versions kept-new-versions -1))
	    (file-error
	     (setq possibilities nil)))
	  (if (not deserve-versions-p)
	      (list (make-backup-file-name fn))
	    (cons (concat fn ".~" (int-to-string (1+ high-water-mark)) "~")
		  (if (and (> number-to-delete 0)
			   ;; Delete nothing if there is overflow
			   ;; in the number of versions to keep.
			   (>= (+ kept-new-versions kept-old-versions -1) 0))
		      (mapcar #'(lambda (n)
				  (concat fn ".~" (int-to-string n) "~"))
			      (let ((v (nthcdr kept-old-versions versions)))
				(rplacd (nthcdr (1- number-to-delete) v) ())
				v))))))))))

(defun file-nlinks (filename)
  "Return number of names file FILENAME has."
  (car (cdr (file-attributes filename))))

(defun file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: default-directory).
This function returns a relative file name which is equivalent to FILENAME
when used with that default directory as the default.
If this is impossible (which can happen on MS Windows when the file name
and directory use different drive names) then it returns FILENAME."
  (save-match-data
    (let ((fname (expand-file-name filename)))
      (setq directory (file-name-as-directory
		       (expand-file-name (or directory default-directory))))
      ;; On Microsoft OSes, if FILENAME and DIRECTORY have different
      ;; drive names, they can't be relative, so return the absolute name.
      (if (and (eq system-type 'windows-nt)
	       (not (string-equal (substring fname  0 2)
				  (substring directory 0 2))))
	  filename
	(let ((ancestor ".")
	      (fname-dir (file-name-as-directory fname)))
	  (while (and (not (string-match (concat "^" (regexp-quote directory))
					 fname-dir))
		      (not (string-match (concat "^" (regexp-quote directory)) fname)))
	    (setq directory (file-name-directory (substring directory 0 -1))
		  ancestor (if (equal ancestor ".")
			       ".."
			     (concat "../" ancestor))))
	  ;; Now ancestor is empty, or .., or ../.., etc.
	  (if (string-match (concat "^" (regexp-quote directory)) fname)
	      ;; We matched within FNAME's directory part.
	      ;; Add the rest of FNAME onto ANCESTOR.
	      (let ((rest (substring fname (match-end 0))))
		(if (and (equal ancestor ".")
			 (not (equal rest "")))
		    ;; But don't bother with ANCESTOR if it would give us `./'.
		    rest
		  (concat (file-name-as-directory ancestor) rest)))
	    ;; We matched FNAME's directory equivalent.
	    ancestor))))))

(defun save-buffer (&optional args)
  "Save current buffer in visited file if modified.  Versions described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
With 1 or 3 \\[universal-argument]'s, marks this version
 to become a backup when the next save is done.
With 2 or 3 \\[universal-argument]'s,
 unconditionally makes the previous version into a backup file.
With argument of 0, never makes the previous version into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 non-nil.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells XEmacs how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
`dired-kept-versions' controls dired's clean-directory (.) command.
If `delete-old-versions' is nil, system will query user
 before trimming versions.  Otherwise it does it silently."
  (interactive "_p")
  (let ((modp (buffer-modified-p))
	(large (> (buffer-size) 50000))
	(make-backup-files (or (and make-backup-files (not (eq args 0)))
			       (memq args '(16 64)))))
    (and modp (memq args '(16 64)) (setq buffer-backed-up nil))
    (if (and modp large) (display-message
			  'progress (format "Saving file %s..."
					    (buffer-file-name))))
    (basic-save-buffer)
    (and modp (memq args '(4 64)) (setq buffer-backed-up nil))))

(defun delete-auto-save-file-if-necessary (&optional force)
  "Delete auto-save file for current buffer if `delete-auto-save-files' is t.
Normally delete only if the file was written by this XEmacs
since the last real save, but optional arg FORCE non-nil means delete anyway."
  (and buffer-auto-save-file-name delete-auto-save-files
       (not (string= buffer-file-name buffer-auto-save-file-name))
       (or force (recent-auto-save-p))
       (progn
	 (ignore-file-errors (delete-file buffer-auto-save-file-name))
	 (set-buffer-auto-saved))))

;; XEmacs change (from Sun)
;; used to communicate with continue-save-buffer:
(defvar continue-save-buffer-hooks-tail nil)

;; Not in FSFmacs
(defun basic-write-file-data (realname truename)
  ;; call the hooks until the bytes are put
  ;; call write-region as a last resort
  (let ((region-written nil)
	(hooks write-file-data-hooks))
    (while (and hooks (not region-written))
      (setq region-written (funcall (car hooks) realname)
	    hooks (cdr hooks)))
    (if (not region-written)
	(write-region (point-min) (point-max) realname nil t truename))))

(put 'after-save-hook 'permanent-local t)
(defvar after-save-hook nil
  "Normal hook that is run after a buffer is saved to its file.
These hooks are considered to pertain to the visited file.
So this list is cleared if you change the visited file name.")

(defun files-fetch-hook-value (hook)
  (let ((localval (symbol-value hook))
	(globalval (default-value hook)))
    (if (memq t localval)
	(setq localval (append (delq t localval) (delq t globalval))))
    localval))

(defun basic-save-buffer ()
  "Save the current buffer in its visited file, if it has been modified.
After saving the buffer, run `after-save-hook'."
  (interactive)
  (save-excursion
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
	(set-buffer (buffer-base-buffer)))
    (if (buffer-modified-p)
	(let ((recent-save (recent-auto-save-p)))
	  ;; If buffer has no file name, ask user for one.
	  (or buffer-file-name
	      (let ((filename
		     (expand-file-name
		      (read-file-name "File to save in: ") nil)))
		(and (file-exists-p filename)
		     (or (y-or-n-p (format "File `%s' exists; overwrite? "
					   filename))
			 (error "Canceled")))
		(set-visited-file-name filename)))
	  (or (verify-visited-file-modtime (current-buffer))
	      (not (file-exists-p buffer-file-name))
	      (yes-or-no-p
	       (format "%s has changed since visited or saved.  Save anyway? "
		       (file-name-nondirectory buffer-file-name)))
	      (error "Save not confirmed"))
	  (save-restriction
	    (widen)

	    ;; Add final newline if required.  See `require-final-newline'.
	    (when (and (not (eq (char-before (point-max)) ?\n)) ; common case
		       (char-before (point-max))                ; empty buffer?
		       (not (and (eq selective-display t)
				 (eq (char-before (point-max)) ?\r)))
		       (or (eq require-final-newline t)
			   (and require-final-newline
				(y-or-n-p
				 (format "Buffer %s does not end in newline.  Add one? "
					 (buffer-name))))))
	      (save-excursion
		(goto-char (point-max))
		(insert ?\n)))

	    ;; Run the write-file-hooks until one returns non-nil.
	    ;; Bind after-save-hook to nil while running the
	    ;; write-file-hooks so that if this function is called
	    ;; recursively (from inside a write-file-hook) the
	    ;; after-hooks will only get run once (from the
	    ;; outermost call).
	    ;;
	    ;; Ugh, have to duplicate logic of run-hook-with-args-until-success
            (let ((hooks (append (files-fetch-hook-value 'write-contents-hooks)
                                 (files-fetch-hook-value
				  'local-write-file-hooks)
                                 (files-fetch-hook-value 'write-file-hooks)))
		  (after-save-hook nil)
                  (local-write-file-hooks nil)
		  (write-contents-hooks nil)
		  (write-file-hooks nil)
		  done)
              (while (and hooks
                          (let ((continue-save-buffer-hooks-tail hooks))
                            (not (setq done (funcall (car hooks))))))
                (setq hooks (cdr hooks)))
	      ;; If a hook returned t, file is already "written".
	      ;; Otherwise, write it the usual way now.
	      (if (not done)
		  (basic-save-buffer-1)))
	    ;; XEmacs: next two clauses (buffer-file-number setting and
	    ;; set-file-modes) moved into basic-save-buffer-1.
	    )
	  ;; If the auto-save file was recent before this command,
	  ;; delete it now.
	  (delete-auto-save-file-if-necessary recent-save)
	  ;; Support VC `implicit' locking.
	  (when (fboundp 'vc-after-save)
	    (vc-after-save))
	  (run-hooks 'after-save-hook))
      (display-message 'no-log "(No changes need to be saved)"))))

;; This does the "real job" of writing a buffer into its visited file
;; and making a backup file.  This is what is normally done
;; but inhibited if one of write-file-hooks returns non-nil.
;; It returns a value to store in setmodes.
(defun basic-save-buffer-1 ()
  (let (setmodes tempsetmodes)
    (if (not (file-writable-p buffer-file-name))
	(let ((dir (file-name-directory buffer-file-name)))
	  (if (not (file-directory-p dir))
	      (error "%s is not a directory" dir)
	    (if (not (file-exists-p buffer-file-name))
		(error "Directory %s write-protected" dir)
	      (if (yes-or-no-p
		   (format "File %s is write-protected; try to save anyway? "
			   (file-name-nondirectory
			    buffer-file-name)))
		  (setq tempsetmodes t)
		(error
		 "Attempt to save to a file which you aren't allowed to write"))))))
    (or buffer-backed-up
	(setq setmodes (backup-buffer)))
    (let ((dir (file-name-directory buffer-file-name)))
      (if (and file-precious-flag
	       (file-writable-p dir))
	  ;; If file is precious, write temp name, then rename it.
	  ;; This requires write access to the containing dir,
	  ;; which is why we don't try it if we don't have that access.
	  (let ((realname buffer-file-name)
		tempname nogood i succeed
		(old-modtime (visited-file-modtime)))
	    (setq i 0)
	    (setq nogood t)
	    ;; Find the temporary name to write under.
	    (while nogood
	      (setq tempname (format "%s#tmp#%d" dir i))
	      (setq nogood (file-exists-p tempname))
	      (setq i (1+ i)))
	    (unwind-protect
		(progn (clear-visited-file-modtime)
		       (write-region (point-min) (point-max)
				     tempname nil realname
				     buffer-file-truename)
		       (setq succeed t))
	      ;; If writing the temp file fails,
	      ;; delete the temp file.
	      (or succeed
		  (progn
		    (delete-file tempname)
		    (set-visited-file-modtime old-modtime))))
	    ;; Since we have created an entirely new file
	    ;; and renamed it, make sure it gets the
	    ;; right permission bits set.
	    (setq setmodes (file-modes buffer-file-name))
	    ;; We succeeded in writing the temp file,
	    ;; so rename it.
	    (rename-file tempname buffer-file-name t))
	;; If file not writable, see if we can make it writable
	;; temporarily while we write it.
	;; But no need to do so if we have just backed it up
	;; (setmodes is set) because that says we're superseding.
	(cond ((and tempsetmodes (not setmodes))
	       ;; Change the mode back, after writing.
	       (setq setmodes (file-modes buffer-file-name))
	       (set-file-modes buffer-file-name 511)))
	(basic-write-file-data buffer-file-name buffer-file-truename)))
    (setq buffer-file-number
	  (if buffer-file-name
	      (nth 10 (file-attributes buffer-file-name))
	    nil))
    (if setmodes
	(condition-case ()
	    (set-file-modes buffer-file-name setmodes)
	  (error nil)))))

;; XEmacs change, from Sun
(defun continue-save-buffer ()
  "Provide a clean way for a write-file-hook to wrap AROUND
the execution of the remaining hooks and writing to disk.
Do not call this function except from a functions
on the `write-file-hooks' or `write-contents-hooks' list.
A hook that calls this function must return non-nil,
to signal completion to its caller.  `continue-save-buffer'
always returns non-nil."
  (let ((hooks (cdr (or continue-save-buffer-hooks-tail
			(error
	 "continue-save-buffer called outside a write-file-hook!"))))
	(done nil))
    ;; Do something like this:
    ;; (let ((write-file-hooks hooks)) (basic-save-buffer))
    ;; First run the rest of the hooks.
    (while (and hooks
		(let ((continue-save-buffer-hooks-tail hooks))
		  (not (setq done (funcall (car hooks))))))
      (setq hooks (cdr hooks)))
    ;;
    ;; If a hook returned t, file is already "written".
    (if (not done)
	(basic-save-buffer-1))
    'continue-save-buffer))

(defcustom save-some-buffers-query-display-buffer t
  "*Non-nil makes `\\[save-some-buffers]' switch to the buffer offered for saving."
  :type 'boolean
  :group 'editing-basics)

(defun save-some-buffers (&optional arg exiting)
  "Save some modified file-visiting buffers.  Asks user about each one.
Optional argument (the prefix) non-nil means save all with no questions.
Optional second argument EXITING means ask about certain non-file buffers
 as well as about file buffers."
  (interactive "P")
  (save-excursion
    ;; `delete-other-windows' can bomb during autoloads generation, so
    ;; guard it well.
    (if (or noninteractive
	    (eq (selected-window) (minibuffer-window))
	    (not save-some-buffers-query-display-buffer))
	;; If playing with windows is unsafe or undesired, just do the
	;; usual drill.
	(save-some-buffers-1 arg exiting nil)
      ;; Else, protect the windows.
      (when (save-window-excursion
	      (save-some-buffers-1 arg exiting t))
	;; Force redisplay.
	(sit-for 0)))))

;; XEmacs - do not use queried flag
(defun save-some-buffers-1 (arg exiting switch-buffer)
  (let* ((switched nil)
	 (last-buffer nil)
	 (files-done
	  (map-y-or-n-p
	   (lambda (buffer)
	     (prog1
		 (and (buffer-modified-p buffer)
		      (not (buffer-base-buffer buffer))
		      ;; XEmacs addition:
		      (not (symbol-value-in-buffer 'save-buffers-skip buffer))
		      (or
		       (buffer-file-name buffer)
		       (and exiting
			    (progn
			      (set-buffer buffer)
			      (and buffer-offer-save (> (buffer-size) 0)))))
		      (if arg
			  t
			;; #### We should provide a per-buffer means to
			;; disable the switching.  For instance, you might
			;; want to turn it off for buffers the contents of
			;; which is meaningless to humans, such as
			;; `.newsrc.eld'.
			(when (and switch-buffer
				   ;; map-y-or-n-p is displaying help
				   (not (eq last-buffer buffer)))
			  (unless (one-window-p)
			    (delete-other-windows))
			  (setq switched t)
			  ;; #### Consider using `display-buffer' here for 21.1!
			  ;;(display-buffer buffer nil (selected-frame)))
			  (switch-to-buffer buffer t))
			(if (buffer-file-name buffer)
			    (format "Save file %s? "
				    (buffer-file-name buffer))
			  (format "Save buffer %s? "
				  (buffer-name buffer)))))
	       (setq last-buffer buffer)))
	   (lambda (buffer)
	     (set-buffer buffer)
	     (condition-case ()
		 (save-buffer)
	       (error nil)))
	   (buffer-list)
	   '("buffer" "buffers" "save")
	   ;;instead of this we just say "yes all", "no all", etc.
	   ;;"save all the rest"
	   ;;"save only this buffer" "save no more buffers")
	   ;; this is rather bogus. --ben
	   ;; (it makes the dialog box too big, and you get an error
	   ;; "wrong type argument: framep, nil" when you hit q after
	   ;; choosing the option from the dialog box)

	   ;; We should fix the dialog box rather than disabling
	   ;; this!  --hniksic
	   (list (list ?\C-r (lambda (buf)
			       ;; #### FSF has an EXIT-ACTION argument
			       ;; to `view-buffer'.
			       (view-buffer buf)
			       (setq view-exit-action
				     (lambda (ignore)
				       (exit-recursive-edit)))
			       (recursive-edit)
			       ;; Return nil to ask about BUF again.
			       nil)
		       "%_Display Buffer"))))
	 (abbrevs-done
	  (and save-abbrevs abbrevs-changed
	       (progn
		 (if (or arg
			 (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
		     (write-abbrev-file nil))
		 ;; Don't keep bothering user if he says no.
		 (setq abbrevs-changed nil)
		 t))))
    (or (> files-done 0) abbrevs-done
	(display-message 'no-log "(No files need saving)"))
    switched))


(defun not-modified (&optional arg)
  "Mark current buffer as unmodified, not needing to be saved.
With prefix arg, mark buffer as modified, so \\[save-buffer] will save.

It is not a good idea to use this function in Lisp programs, because it
prints a message in the minibuffer.  Instead, use `set-buffer-modified-p'."
  (interactive "_P")
  (if arg ;; rewritten for I18N3 snarfing
      (display-message 'command "Modification-flag set")
    (display-message 'command "Modification-flag cleared"))
  (set-buffer-modified-p arg))

(defun toggle-read-only (&optional arg)
  "Toggle the current buffer's read-only status.
With arg, set read-only iff arg is positive."
  (interactive "_P")
  (setq buffer-read-only
	(if (null arg)
            (not buffer-read-only)
            (> (prefix-numeric-value arg) 0)))
  ;; Force modeline redisplay
  (redraw-modeline))

(defun insert-file (filename &optional codesys)
  "Insert contents of file FILENAME into buffer after point.
Set mark after the inserted text.

Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system.

This function is meant for the user to run interactively.
Don't call it from programs!  Use `insert-file-contents' instead.
\(Its calling sequence is different; see its documentation)."
  (interactive "*fInsert file: \nZCoding system: ")
  (if (file-directory-p filename)
      (signal 'file-error (list "Opening input file" "file is a directory"
				filename)))
  (let ((tem
	 (if codesys
	     (let ((coding-system-for-read
		    (get-coding-system codesys)))
	       (insert-file-contents filename))
	   (insert-file-contents filename))))
    (push-mark (+ (point) (car (cdr tem))))))

(defun append-to-file (start end filename &optional codesys)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are buffer positions
saying what text to write.
Under XEmacs/Mule, optional fourth argument specifies the
coding system to use when encoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "r\nFAppend to file: \nZCoding system: ")
  (if codesys
      (let ((buffer-file-coding-system (get-coding-system codesys)))
	(write-region start end filename t))
    (write-region start end filename t)))

(defun file-newest-backup (filename)
  "Return most recent backup file for FILENAME or nil if no backups exist."
  (let* ((filename (expand-file-name filename))
	 (file (file-name-nondirectory filename))
	 (dir  (file-name-directory    filename))
	 (comp (file-name-all-completions file dir))
	 newest)
    (while comp
      (setq file (concat dir (car comp))
	    comp (cdr comp))
      (if (and (backup-file-name-p file)
	       (or (null newest) (file-newer-than-file-p file newest)))
	  (setq newest file)))
    newest))

(defun rename-uniquely ()
  "Rename current buffer to a similar name not already taken.
This function is useful for creating multiple shell process buffers
or multiple mail buffers, etc."
  (interactive)
  (save-match-data
    (let* ((base-name (if (and (string-match "<[0-9]+>\\'" (buffer-name))
			       (not (and buffer-file-name
					 (string= (buffer-name)
						  (file-name-nondirectory
						   buffer-file-name)))))
			  ;; If the existing buffer name has a <NNN>,
			  ;; which isn't part of the file name (if any),
			  ;; then get rid of that.
			  (substring (buffer-name) 0 (match-beginning 0))
			(buffer-name)))
	   (new-buf (generate-new-buffer base-name))
	   (name (buffer-name new-buf)))
      (kill-buffer new-buf)
      (rename-buffer name)
      (redraw-modeline))))

(defun make-directory-path (path)
  "Create all the directories along path that don't exist yet."
  (interactive "Fdirectory path to create: ")
  (make-directory path t))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
Interactively, the default choice of directory to create
is the current default directory for file names.
That is useful when you have visited a file in a nonexistent directory.

Noninteractively, the second (optional) argument PARENTS says whether
to create parent directories if they don't exist."
  (interactive (list (let ((current-prefix-arg current-prefix-arg))
		       (read-directory-name "Create directory: "))
		     current-prefix-arg))
  (let ((handler (find-file-name-handler dir 'make-directory)))
    (if handler
	(funcall handler 'make-directory dir parents)
      (if (not parents)
	  (make-directory-internal dir)
	(let ((dir (directory-file-name (expand-file-name dir)))
	      create-list)
	  (while (not (file-exists-p dir))
	    (setq create-list (cons dir create-list)
		  dir (directory-file-name (file-name-directory dir))))
	  (while create-list
	    (make-directory-internal (car create-list))
	    (setq create-list (cdr create-list))))))))

(put 'revert-buffer-function 'permanent-local t)
(defvar revert-buffer-function nil
  "Function to use to revert this buffer, or nil to do the default.
The function receives two arguments IGNORE-AUTO and NOCONFIRM,
which are the arguments that `revert-buffer' received.")

(put 'revert-buffer-insert-file-contents-function 'permanent-local t)
(defvar revert-buffer-insert-file-contents-function nil
  "Function to use to insert contents when reverting this buffer.
Gets two args, first the nominal file name to use,
and second, t if reading the auto-save file.
If the current buffer contents are to be discarded, the function must do
so itself.")

(defvar before-revert-hook nil
  "Normal hook for `revert-buffer' to run before reverting.
If `revert-buffer-function' is used to override the normal revert
mechanism, this hook is not used.")

(defvar after-revert-hook nil
  "Normal hook for `revert-buffer' to run after reverting.
Note that the hook value that it runs is the value that was in effect
before reverting; that makes a difference if you have buffer-local
hook functions.

If `revert-buffer-function' is used to override the normal revert
mechanism, this hook is not used.")

(defvar revert-buffer-internal-hook nil
  "Don't use this.")

(defun revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Replace the buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
With a prefix argument, offer to revert from latest auto-save file, if
that is more recent than the visited file.

This command also refreshes certain special buffers that contain text
which doesn't come from a file, but reflects some other data base
instead: for example, Dired buffers and buffer-list buffers.  This is
implemented by having the modes set `revert-buffer-function'.

When called from Lisp, the first argument is IGNORE-AUTO; only offer
to revert from the auto-save file when this is nil.  Note that the
sense of this argument is the reverse of the prefix argument, for the
sake of backward compatibility.  IGNORE-AUTO is optional, defaulting
to nil.

Optional second argument NOCONFIRM means don't ask for confirmation at
all.

Optional third argument PRESERVE-MODES non-nil means don't alter
the buffer's modes.  Otherwise, reinitialize them using `normal-mode'.

If the value of `revert-buffer-function' is non-nil, it is called to
do all the work for this command.  Otherwise, the hooks
`before-revert-hook' and `after-revert-hook' are run at the beginning
and the end, and if `revert-buffer-insert-file-contents-function' is
non-nil, it is called instead of rereading visited file contents.

If the buffer-modified flag is nil, and we are not reverting from an
auto-save file, then compare the contents of the buffer and the file.
Revert only if they differ."

  ;; I admit it's odd to reverse the sense of the prefix argument, but
  ;; there is a lot of code out there which assumes that the first
  ;; argument should be t to avoid consulting the auto-save file, and
  ;; there's no straightforward way to encourage authors to notice a
  ;; reversal of the argument sense.  So I'm just changing the user
  ;; interface, but leaving the programmatic interface the same.
  (interactive (list (not current-prefix-arg)))
  (if revert-buffer-function
      (funcall revert-buffer-function ignore-auto noconfirm)
    (let* ((opoint (point))
	   (newbuf nil)
	   (found nil)
	   (delay-prompt nil)
	   (auto-save-p (and (not ignore-auto)
                             (recent-auto-save-p)
			     buffer-auto-save-file-name
			     (file-readable-p buffer-auto-save-file-name)
			     (y-or-n-p
   "Buffer has been auto-saved recently.  Revert from auto-save file? ")))
	   (file-name (if auto-save-p
			  buffer-auto-save-file-name
			buffer-file-name)))
      (cond ((null file-name)
	     (error "Buffer does not seem to be associated with any file"))
	    ((or noconfirm
		 (and (not (buffer-modified-p))
		      (dolist (rx revert-without-query found)
			(when (string-match rx file-name)
			  (setq found t))))
		 ;; If we will call revert-buffer-internal, delay prompting
		 (and (not auto-save-p)
		      (not (buffer-modified-p))
		      (setq delay-prompt t))
		 (yes-or-no-p (format "Revert buffer from file %s? "
				      file-name)))
	     (run-hooks 'before-revert-hook)
	     (cond ((or auto-save-p
			(buffer-modified-p)
			;; Do we need to do expensive reversion?  Compare ...
			(and (setq newbuf (revert-buffer-internal
					   file-name))
			     ;; ... and if different, prompt
			     (or noconfirm found
				 (and delay-prompt
				      (yes-or-no-p 
				       (format "Revert buffer from file %s? "
					       file-name))))))
		    ;; If file was backed up but has changed since,
		    ;; we should make another backup.
		    (and (not auto-save-p)
			 (not (verify-visited-file-modtime (current-buffer)))
			 (setq buffer-backed-up nil))
		    ;; Get rid of all undo records for this buffer.
		    (or (eq buffer-undo-list t)
			(setq buffer-undo-list nil))
		    ;; Effectively copy the after-revert-hook status,
		    ;; since after-find-file will clobber it.
		    (let ((global-hook (default-value 'after-revert-hook))
			  (local-hook-p (local-variable-p 'after-revert-hook
							  (current-buffer)))
			  (local-hook (and (local-variable-p 'after-revert-hook
							     (current-buffer))
					   after-revert-hook)))
		      (let (buffer-read-only
			    ;; Don't make undo records for the reversion.
			    (buffer-undo-list t))
			(if revert-buffer-insert-file-contents-function
			    (funcall revert-buffer-insert-file-contents-function
				     file-name auto-save-p)
			  (if (not (file-exists-p file-name))
			      (error "File %s no longer exists!" file-name))
			  ;; Bind buffer-file-name to nil
			  ;; so that we don't try to lock the file.
			  (let ((buffer-file-name nil))
			    (or auto-save-p
				(unlock-buffer)))
			  (widen)
			  (insert-file-contents file-name (not auto-save-p)
						nil nil t)))
		      (goto-char (min opoint (point-max)))
		      ;; Recompute the truename in case changes in symlinks
		      ;; have changed the truename.
		      ;;XEmacs: already done by insert-file-contents
		      ;;(setq buffer-file-truename
		      ;;(abbreviate-file-name (file-truename buffer-file-name)))
		      (after-find-file nil nil t t preserve-modes)
		      ;; Run after-revert-hook as it was before we reverted.
		      (setq-default revert-buffer-internal-hook global-hook)
		      (if local-hook-p
			  (progn
			    (make-local-variable 'revert-buffer-internal-hook)
			    (setq revert-buffer-internal-hook local-hook))
			(kill-local-variable 'revert-buffer-internal-hook))
		      (run-hooks 'revert-buffer-internal-hook)))
		   ((null newbuf)
		    ;; The resultant buffer is identical, alter
		    ;; modtime, update mods and exit
		    (set-visited-file-modtime)
		    (after-find-file nil nil t t t)
		    ;; We preserved modes above so fixup the local
		    ;; variables manually
		    (condition-case err
			(hack-local-variables)
		      (error (lwarn 'local-variables 'warning
			       "File local-variables error: %s"
			       (error-message-string err)))))
		   (t t))
	     t)))))

;; #### something like `revert-buffer-compare-with-file' is a better name
;; #### why is the argument optional?
(defun revert-buffer-internal (&optional file-name)
  "Read contents of FILE-NAME into a buffer, and compare to current buffer.
Return nil if identical, and the new buffer if different."

  (let* ((newbuf (get-buffer-create " *revert*"))
	 bmin bmax)
    (save-excursion
      (set-buffer newbuf)
      (let (buffer-read-only
	    (buffer-undo-list t)
	    after-change-function
	    after-change-functions
	    before-change-function
	    before-change-functions)
	(if revert-buffer-insert-file-contents-function
	    (funcall revert-buffer-insert-file-contents-function
		     file-name nil)
	  (if (not (file-exists-p file-name))
	      (error "File %s no longer exists!" file-name))
	  (widen)
	  (insert-file-contents file-name nil nil nil t)
	  (setq bmin (point-min)
		bmax (point-max)))))
    (if (not (and (eq bmin (point-min))
		  (eq bmax (point-max))
		  (eq (compare-buffer-substrings 
		       newbuf bmin bmax (current-buffer) bmin bmax) 0)))
	newbuf
      (and (kill-buffer newbuf) nil))))

(defun recover-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  ;; Actually putting the file name in the minibuffer should be used
  ;; only rarely.
  ;; Not just because users often use the default.
  (interactive "FRecover file: ")
  (setq file (expand-file-name file))
  (let ((handler (or (find-file-name-handler file 'recover-file)
		    (find-file-name-handler
		     (let ((buffer-file-name file))
		       (make-auto-save-file-name))
		     'recover-file))))
    (if handler
	(funcall handler 'recover-file file)
      (if (auto-save-file-name-p (file-name-nondirectory file))
	  (error "%s is an auto-save file" file))
      (let ((file-name (let ((buffer-file-name file))
			 (make-auto-save-file-name))))
	(cond ((if (file-exists-p file)
		   (not (file-newer-than-file-p file-name file))
		 (not (file-exists-p file-name)))
	       (error "Auto-save file %s not current" file-name))
	      ((save-window-excursion
		 ;; XEmacs change: use insert-directory instead of
		 ;; calling ls directly.
		 (with-output-to-temp-buffer "*Directory*"
		   (buffer-disable-undo standard-output)
		   (save-excursion
		     (set-buffer "*Directory*")
		     (setq default-directory (file-name-directory file))
		     (insert-directory file
				       (if (file-symlink-p file) "-lL" "-l"))
		     (setq default-directory (file-name-directory file-name))
		     (insert-directory file-name "-l")))
		 (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	       (switch-to-buffer (find-file-noselect file t))
	       (let ((buffer-read-only nil))
		 (erase-buffer)
		 (insert-file-contents file-name nil))
	       (after-find-file nil nil t))
	      (t (error "Recover-file cancelled.")))))))

(defun recover-session ()
  "Recover auto save files from a previous Emacs session.
This command first displays a Dired buffer showing you the
previous sessions that you could recover from.
To choose one, move point to the proper line and then type C-c C-c.
Then you'll be asked about a number of files to recover."
  (interactive)
  (unless (fboundp 'dired)
    (error "recover-session requires dired"))
  (if (null auto-save-list-file-prefix)
      (error
       "You set `auto-save-list-file-prefix' to disable making session files"))
  (dired (concat auto-save-list-file-prefix "*"))
  (goto-char (point-min))
  (or (looking-at "Move to the session you want to recover,")
      (let ((inhibit-read-only t))
	(insert "Move to the session you want to recover,\n"
		"then type C-c C-c to select it.\n\n"
		"You can also delete some of these files;\n"
		"type d on a line to mark that file for deletion.\n\n")))
  (use-local-map (let ((map (make-sparse-keymap)))
		   (set-keymap-parents map (list (current-local-map)))
		   map))
  (define-key (current-local-map) "\C-c\C-c" 'recover-session-finish))

(defun recover-session-finish ()
  "Choose one saved session to recover auto-save files from.
This command is used in the special Dired buffer created by
\\[recover-session]."
  (interactive)
  ;; Get the name of the session file to recover from.
  (let ((file (dired-get-filename))
	files
	(buffer (get-buffer-create " *recover*")))
    ;; #### dired-do-flagged-delete in FSF.
    ;; This version is for ange-ftp
    ;;(dired-do-deletions t)
    ;; This version is for efs
    (dired-expunge-deletions)
    (unwind-protect
	(save-excursion
	  ;; Read in the auto-save-list file.
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert-file-contents file)
	  ;; Loop thru the text of that file
	  ;; and get out the names of the files to recover.
	  (while (not (eobp))
	    (let (thisfile autofile)
	      (if (eolp)
		  ;; This is a pair of lines for a non-file-visiting buffer.
		  ;; Get the auto-save file name and manufacture
		  ;; a "visited file name" from that.
		  (progn
		    (forward-line 1)
		    (setq autofile
			  (buffer-substring-no-properties
			   (point)
			   (save-excursion
			     (end-of-line)
			     (point))))
		    (setq thisfile
			  (expand-file-name
			   (substring
			    (file-name-nondirectory autofile)
			    1 -1)
			   (file-name-directory autofile)))
		    (forward-line 1))
		;; This pair of lines is a file-visiting
		;; buffer.  Use the visited file name.
		(progn
		  (setq thisfile
			(buffer-substring-no-properties
			 (point) (progn (end-of-line) (point))))
		  (forward-line 1)
		  (setq autofile
			(buffer-substring-no-properties
			 (point) (progn (end-of-line) (point))))
		  (forward-line 1)))
	      ;; Ignore a file if its auto-save file does not exist now.
	      (if (file-exists-p autofile)
		  (setq files (cons thisfile files)))))
	  (setq files (nreverse files))
	  ;; The file contains a pair of line for each auto-saved buffer.
	  ;; The first line of the pair contains the visited file name
	  ;; or is empty if the buffer was not visiting a file.
	  ;; The second line is the auto-save file name.
	  (if files
	      (map-y-or-n-p  "Recover %s? "
			     (lambda (file)
			       (condition-case nil
				   (save-excursion (recover-file file))
				 (error
				  "Failed to recover `%s'" file)))
			     files
			     '("file" "files" "recover"))
	    (message "No files can be recovered from this session now")))
      (kill-buffer buffer))))

(defun kill-some-buffers (&optional list)
  "For each buffer in LIST, ask whether to kill it.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (/= (aref name 0) ?\ )
	   (yes-or-no-p
	    (format
	     (if (buffer-modified-p buffer)
		 (gettext "Buffer %s HAS BEEN EDITED.  Kill? ")
	       (gettext "Buffer %s is unmodified.  Kill? "))
	     name))
	   (kill-buffer buffer)))
    (setq list (cdr list))))

(defun auto-save-mode (arg)
  "Toggle auto-saving of contents of current buffer.
With prefix argument ARG, turn auto-saving on if positive, else off."
  (interactive "P")
  (setq buffer-auto-save-file-name
        (and (if (null arg)
		 (or (not buffer-auto-save-file-name)
		     ;; If autosave is off because buffer has shrunk,
		     ;; then toggling should turn it on.
		     (< buffer-saved-size 0))
	       (or (eq arg t) (listp arg) (and (integerp arg) (> arg 0))))
	     (if (and buffer-file-name auto-save-visited-file-name
		      (not buffer-read-only))
		 buffer-file-name
	       (make-auto-save-file-name))))
  ;; If -1 was stored here, to temporarily turn off saving,
  ;; turn it back on.
  (and (< buffer-saved-size 0)
       (setq buffer-saved-size 0))
  (if (interactive-p)
      (if buffer-auto-save-file-name ;; rewritten for I18N3 snarfing
	  (display-message 'command "Auto-save on (in this buffer)")
	(display-message 'command "Auto-save off (in this buffer)")))
  buffer-auto-save-file-name)

(defun rename-auto-save-file ()
  "Adjust current buffer's auto save file name for current conditions.
Also rename any existing auto save file, if it was made in this session."
  (let ((osave buffer-auto-save-file-name))
    (setq buffer-auto-save-file-name
	  (make-auto-save-file-name))
    (if (and osave buffer-auto-save-file-name
	     (not (string= buffer-auto-save-file-name buffer-file-name))
	     (not (string= buffer-auto-save-file-name osave))
	     (file-exists-p osave)
	     (recent-auto-save-p))
	(rename-file osave buffer-auto-save-file-name t))))

;; make-auto-save-file-name and auto-save-file-name-p are now only in
;; auto-save.el.


(defun wildcard-to-regexp (wildcard)
  "Given a shell file name pattern WILDCARD, return an equivalent regexp.
The generated regexp will match a filename iff the filename
matches that wildcard according to shell rules.  Only wildcards known
by `sh' are supported."
  (let* ((i (string-match "[[.*+\\^$?]" wildcard))
	 ;; Copy the initial run of non-special characters.
	 (result (substring wildcard 0 i))
	 (len (length wildcard)))
    ;; If no special characters, we're almost done.
    (if i
	(while (< i len)
	  (let ((ch (aref wildcard i))
		j)
	    (setq
	     result
	     (concat result
		     (cond
		      ((eq ch ?\[)	; [...] maps to regexp char class
		       (progn
			 (setq i (1+ i))
			 (concat
			  (cond
			   ((eq (aref wildcard i) ?!) ; [!...] -> [^...]
			    (progn
			      (setq i (1+ i))
			      (if (eq (aref wildcard i) ?\])
				  (progn
				    (setq i (1+ i))
				    "[^]")
				"[^")))
			   ((eq (aref wildcard i) ?^)
			    ;; Found "[^".  Insert a `\0' character
			    ;; (which cannot happen in a filename)
			    ;; into the character class, so that `^'
			    ;; is not the first character after `[',
			    ;; and thus non-special in a regexp.
			    (progn
			      (setq i (1+ i))
			      "[\000^"))
			   ((eq (aref wildcard i) ?\])
			    ;; I don't think `]' can appear in a
			    ;; character class in a wildcard, but
			    ;; let's be general here.
			    (progn
			      (setq i (1+ i))
			      "[]"))
			   (t "["))
			  (prog1	; copy everything upto next `]'.
			      (substring wildcard
					 i
					 (setq j (string-match
						  "]" wildcard i)))
			    (setq i (if j (1- j) (1- len)))))))
		      ((eq ch ?.)  "\\.")
		      ((eq ch ?*)  "[^\000]*")
		      ((eq ch ?+)  "\\+")
		      ((eq ch ?^)  "\\^")
		      ((eq ch ?$)  "\\$")
		      ((eq ch ?\\) "\\\\") ; probably cannot happen...
		      ((eq ch ??)  "[^\000]")
		      (t (char-to-string ch)))))
	    (setq i (1+ i)))))
    ;; Shell wildcards should match the entire filename,
    ;; not its part.  Make the regexp say so.
    (concat "\\`" result "\\'")))

(defcustom list-directory-brief-switches "-CF"
  "*Switches for list-directory to pass to `ls' for brief listing."
  :type 'string
  :group 'dired)

(defcustom list-directory-verbose-switches "-l"
  "*Switches for list-directory to pass to `ls' for verbose listing,"
  :type 'string
  :group 'dired)

(defun list-directory (dirname &optional verbose)
  "Display a list of files in or matching DIRNAME, a la `ls'.
DIRNAME is globbed by the shell if necessary.
Prefix arg (second arg if noninteractive) means supply -l switch to `ls'.
Actions controlled by variables `list-directory-brief-switches'
and `list-directory-verbose-switches'."
  (interactive (let ((pfx current-prefix-arg))
		 (list (read-file-name (if pfx (gettext "List directory (verbose): ")
					 (gettext "List directory (brief): "))
				       nil default-directory nil)
		       pfx)))
  (let ((switches (if verbose list-directory-verbose-switches
		    list-directory-brief-switches)))
    (or dirname (setq dirname default-directory))
    (setq dirname (expand-file-name dirname))
    (with-output-to-temp-buffer "*Directory*"
      (buffer-disable-undo standard-output)
      (princ "Directory ")
      (princ dirname)
      (terpri)
      (save-excursion
	(set-buffer "*Directory*")
	(setq default-directory (file-name-directory dirname))
	(let ((wildcard (not (file-directory-p dirname))))
	  (insert-directory dirname switches wildcard (not wildcard)))))))

(defvar insert-directory-program "ls"
  "Absolute or relative name of the `ls' program used by `insert-directory'.")

;; insert-directory
;; - must insert _exactly_one_line_ describing FILE if WILDCARD and
;;   FULL-DIRECTORY-P is nil.
;;   The single line of output must display FILE's name as it was
;;   given, namely, an absolute path name.
;; - must insert exactly one line for each file if WILDCARD or
;;   FULL-DIRECTORY-P is t, plus one optional "total" line
;;   before the file lines, plus optional text after the file lines.
;;   Lines are delimited by "\n", so filenames containing "\n" are not
;;   allowed.
;;   File lines should display the basename.
;; - must be consistent with
;;   - functions dired-move-to-filename, (these two define what a file line is)
;;   		 dired-move-to-end-of-filename,
;;		 dired-between-files, (shortcut for (not (dired-move-to-filename)))
;;   		 dired-insert-headerline
;;   		 dired-after-subdir-garbage (defines what a "total" line is)
;;   - variable dired-subdir-regexp
(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
      (cond
       ;; [mswindows-insert-directory should be called
       ;; nt-insert-directory - kkm].  not true any more according to
       ;; my new naming scheme. --ben
       ((and (fboundp 'mswindows-insert-directory)
	     (eq system-type 'windows-nt))
	(mswindows-insert-directory file switches wildcard full-directory-p))
       (t
	(if wildcard
	    ;; Run ls in the directory of the file pattern we asked for.
	    (let ((default-directory
                      (if (file-name-absolute-p file)
                          (file-name-directory file)
                          (file-name-directory (expand-file-name file))))
		  (pattern (file-name-nondirectory file))
		  (start 0))
	      ;; Quote some characters that have special meanings in shells;
	      ;; but don't quote the wildcards--we want them to be special.
	      ;; We also currently don't quote the quoting characters
	      ;; in case people want to use them explicitly to quote
	      ;; wildcard characters.
              ;;#### Unix-specific
	      (while (string-match "[ \t\n;<>&|()#$]" pattern start)
		(setq pattern
		      (concat (substring pattern 0 (match-beginning 0))
			      "\\"
			      (substring pattern (match-beginning 0)))
		      start (1+ (match-end 0))))
	      (call-process shell-file-name nil t nil
			    "-c" (concat "\\"  ;; Disregard shell aliases!
					 insert-directory-program
					 " -d "
					 (if (stringp switches)
					     switches
					   (mapconcat 'identity switches " "))
					 " "
					 pattern)))
	  ;; SunOS 4.1.3, SVr4 and others need the "." to list the
	  ;; directory if FILE is a symbolic link.
	  (apply 'call-process
		 insert-directory-program nil t nil
		 (let (list)
		   (if (listp switches)
		       (setq list switches)
		     (if (not (equal switches ""))
			 (progn
			   ;; Split the switches at any spaces
			   ;; so we can pass separate options as separate args.
			   (while (string-match " " switches)
			     (setq list (cons (substring switches 0 (match-beginning 0))
					      list)
				   switches (substring switches (match-end 0))))
			   (setq list (cons switches list)))))
		   (append list
			   (list
			    (if full-directory-p
				(concat (file-name-as-directory file)
					;;#### Unix-specific
					".")
			      file)))))))))))

(defvar kill-emacs-query-functions nil
  "Functions to call with no arguments to query about killing XEmacs.
If any of these functions returns nil, killing Emacs is cancelled.
`save-buffers-kill-emacs' (\\[save-buffers-kill-emacs]) calls these functions,
but `kill-emacs', the low level primitive, does not.
See also `kill-emacs-hook'.")

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this XEmacs process.
With prefix arg, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (memq t (mapcar #'(lambda (buf) (and (buffer-file-name buf)
						     (buffer-modified-p buf)))
				(buffer-list))))
	   (yes-or-no-p "Modified buffers exist; exit anyway? "))
       (or (not (fboundp 'process-list))
	   ;; process-list is not defined on VMS.
	   (let ((processes (process-list))
		 active)
	     (while processes
	       (and (memq (process-status (car processes)) '(run stop open))
		    (let ((val (process-kill-without-query (car processes))))
		      (process-kill-without-query (car processes) val)
		      val)
		    (setq active t))
	       (setq processes (cdr processes)))
	     (or
	      (not active)
	      (save-excursion
		(save-window-excursion
		  (delete-other-windows)
		  (list-processes)
		  (yes-or-no-p
		   "Active processes exist; kill them and exit anyway? "))))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (kill-emacs)))

(defun symlink-expand-file-name (filename)
  "If FILENAME is a symlink, return its non-symlink equivalent.
Unlike `file-truename', this doesn't chase symlinks in directory
components of the file or expand a relative pathname into an
absolute one."
  (let ((count 20))
    (while (and (> count 0) (file-symlink-p filename))
      (setq filename (file-symlink-p filename)
	    count (1- count)))
    (if (> count 0)
	filename
      (error "Apparently circular symlink path"))))

;; Suggested by Michael Kifer <kifer@CS.SunySB.EDU>
(defun file-remote-p (file-name)
  "Test whether FILE-NAME is looked for on a remote system."
  (cond ((not allow-remote-paths) nil)
	((featurep 'ange-ftp) (ange-ftp-ftp-path file-name))
	((fboundp 'efs-ftp-path) (efs-ftp-path file-name))
	(t nil)))

;; #### FSF has file-name-non-special here.

;;; files.el ends here
