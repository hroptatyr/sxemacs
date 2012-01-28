;;; etags.el --- etags facility for Emacs

;; Copyright 1985, 1986, 1988, 1990, 1997, 2003 Free Software Foundation, Inc.

;; Author: Their Name is Legion (see list below)
;; Maintainer: SXEmacs Development Team
;; Keywords: tools

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

;;; Synched up with: XEmacs 21.5.27 (+CVS-20061118) and then immediately
;;; diverged from it. :-) --SY.

;;; Commentary:

;; This file is completely different from FSF's etags.el.  It appears
;; that an early version of this file (tags.el) has been rewritten by
;; two different people; we got one, FSF got the other.  Various
;; people have said that our version is better and faster.

;; TODO:
;; - DOCUMENT!

;; Derived from the original lisp/tags.el.

;; Ideas and code from the work of the following people:
;; Andy Norman <ange@hplb.hpl.hp.com>, author of ange-tags.el
;; Ramana Rao <rao@arisia.xerox.com>
;; John Sturdy <jcgs@harlqn.co.uk>, author of tags-helper.el
;; Henry Kautz <kautz@allegra.att.com>, author of tag-completion.el
;; Dan LaLiberte <liberte@cs.uiuc.edu>, author of local-tags.el
;; Tom Dietterich <tgd@turing.cs.orst.edu>, author of quest.el
;; The author(s) of lisp/simple.el
;; Duke Briscoe <briscoe@cs.yale.edu>
;; Lynn Slater <lrs@indetech.com>, author of location.el
;; Shinichirou Sugou <shin@sgtp.apple.juice.or.jp>
;; an unidentified anonymous elisp hacker
;; Kyle Jones <kyle_jones@wonderworks.com>
;;   added "Exact match, then inexact" code
;;   added support for include directive.
;; Hrvoje Niksic <hniksic@xemacs.org>
;;   various changes.
;; Steve Youngs <steve@sxemacs.org>
;;   fix support for include directive

;;; Code:
;; Can we use the lightning fast cl-loop DSO?
(eval-and-compile
  (ignore-errors (require 'cl-loop))
  (if (featurep 'cl-loop)
      (progn
	(fset #'tag-loop #'cl:loop)
	(put 'tag-loop 'byte-compile 'byte-compile-cl:loop)
	(put 'tag-loop 'lisp-indent-hook 'defun)
	(put 'tag-loop 'lisp-indent-function 'defun)
	(fset #'tag-dolist #'cl:dolist)
	(put 'tag-dolist 'byte-compile 'byte-compile-cl:dolist)
	(put 'tag-dolist 'lisp-indent-hook 1)
	(put 'tag-dolist 'lisp-indent-function 1)
	(globally-declare-boundp
	 '(key expression inc-files tables)))
    (fset #'tag-loop #'loop)
    (fset #'tag-dolist #'dolist)))


;;; User variables.

(defgroup etags nil
  "Etags facility for Emacs.

This lib provides some useful tools for working with TAGS tables
that were created with the etags binary \(also distributed with this
emacs\)."
  :prefix "tags-"
  :group 'tools)

(defcustom tags-build-completion-table t
  "*When non-nil, build a completion table from all known tags.
Otherwise disable tag completion."
  :type 'boolean
  :group 'etags)

(defcustom tags-always-exact nil
  "*If this variable is non-nil, then tags always looks for exact matches.
If it is nil (the default), tags will first go through exact matches,
then through the non-exact ones."
  :type 'boolean
  :group 'etags)

(defcustom tag-table-alist nil
  "*A list which determines which tags files are active for a buffer.
This is not really an association list, in that all elements are
checked.  The CAR of each element of this list is a pattern against
which the buffer's file name is compared; if it matches, then the CDR
of the list should be the name of the tags table to use.  If more than
one element of this list matches the buffer's file name, then all of
the associated tags tables will be used.  Earlier ones will be
searched first.

If the CAR of elements of this list are strings, then they are treated
as regular-expressions against which the file is compared (like the
auto-mode-alist).  If they are not strings, then they are evaluated.
If they evaluate to non-nil, then the current buffer is considered to
match.

If the CDR of the elements of this list are strings, then they are
assumed to name a TAGS file.  If they name a directory, then the string
\"TAGS\" is appended to them to get the file name.  If they are not
strings, then they are evaluated, and must return an appropriate string.

For example:
  (setq tag-table-alist
	'((\"/usr/src/public/perl/\" . \"/usr/src/public/perl/perl-3.0/\")
	 (\"\\\\.el$\" . \"/usr/local/emacs/src/\")
	 (\"/jbw/gnu/\" . \"/usr15/degree/stud/jbw/gnu/\")
	 (\"\" . \"/usr/local/emacs/src/\")
	 ))

This means that anything in the /usr/src/public/perl/ directory should use
the TAGS file /usr/src/public/perl/perl-3.0/TAGS; and file ending in .el should
use the TAGS file /usr/local/emacs/src/TAGS; and anything in or below the
directory /jbw/gnu/ should use the TAGS file /usr15/degree/stud/jbw/gnu/TAGS.
A file called something like \"/usr/jbw/foo.el\" would use both the TAGS files
/usr/local/emacs/src/TAGS and /usr15/degree/stud/jbw/gnu/TAGS (in that order)
because it matches both patterns.

If the buffer-local variable `buffer-tag-table' is set, then it names a tags
table that is searched before all others when find-tag is executed from this
buffer.

If there is a file called \"TAGS\" in the same directory as the file in
question, then that tags file will always be used as well (after the
`buffer-tag-table' but before the tables specified by this list.)

If the variable tags-file-name is set, then the tags file it names will apply
to all buffers (for backwards compatibility.)  It is searched first."
  :type '(repeat (cons :format "%v"
		       (choice :value ""
			       (regexp :tag "Buffer regexp")
			       sexp)
		       (choice :value ""
			       (string :tag "Tag file or directory")
			       sexp)))
  :group 'etags)

(defvar buffer-tag-table nil
  "*The additional name of one TAGS table to be used for this buffer.
You can set this with `\\[set-buffer-tag-table]'.  See the documentation
for the variable `tag-table-alist' for more information.")
(make-variable-buffer-local 'buffer-tag-table)

(defvar tags-file-name nil
  "The name of the tags-table used by all buffers.
This is for backwards compatibility, and is largely supplanted by the
variable tag-table-alist.")

(defcustom tags-auto-read-changed-tag-files nil
  "*If non-nil, always re-read changed TAGS file without prompting.
If nil, prompt whether to re-read the changed TAGS file."
  :type 'boolean
  :group 'etags)

(defcustom make-tags-files-invisible nil
  "*If non-nil, TAGS-files will not show up in buffer-lists or be
selectable (or deletable.)"
  :type 'boolean
  :group 'etags)

(defcustom tags-search-nuke-uninteresting-buffers t
  "*If non-nil, keep newly-visited files if they contain the search target.
This affects the `tags-search' and `tags-query-replace' commands."
  :type 'boolean
  :group 'etags)

(defcustom tags-check-parent-directories-for-tag-files t
  "*If non-nil, look for TAGS files in all parent directories."
  :type 'boolean
  :group 'etags)

(defcustom tags-exuberant-ctags-optimization-p nil
  "*If this variable is nil (the default), then exact tag search is able
to find tag names in the name part of the tagtable (enclosed by  ^?..^A)
and in the sourceline part of the tagtable ( enclosed by ^..^?).
This is needed by xemacs etags as not every tag has a name field.
It is slower for large tables and less precise than the other option.

If it is non-nil, then exact tag will only search tag names in the name
part (enclosed by ^?..^A). This is faster and more precise than the other
option. This is only usable with exuberant etags, as it has a name field
entry for every tag."
:type 'boolean
:group 'etags)


;; Buffer tag tables.

(defun buffer-tag-table-list ()
  "Returns a list (ordered) of the tags tables which should be used for
the current buffer."
  (let (result)
    ;; Explicitly set buffer-tag-table
    (when buffer-tag-table
      (push buffer-tag-table result))
    ;; Current directory
    (when (file-readable-p (concat default-directory "TAGS"))
      (push (concat default-directory "TAGS") result))
    ;; Parent directories
    (when tags-check-parent-directories-for-tag-files
      (let ((cur default-directory))
	(while (not (and (equal (file-name-as-directory cur) cur)
			 (equal (directory-file-name cur) cur)))
	  (setq cur (expand-file-name ".." cur))
	  (let ((parent-tag-file (expand-file-name "TAGS" cur)))
	    (when (file-readable-p parent-tag-file)
	      (push parent-tag-file result))))))
    ;; tag-table-alist
    (let ((key (or buffer-file-name
		   (concat default-directory (buffer-name))))
	  expression)
      (tag-dolist (item tag-table-alist)
	(setq expression (car item))
	;; If the car of the alist item is a string, apply it as a regexp
	;; to the buffer-file-name.  Otherwise, evaluate it.  If the
	;; regexp matches, or the expression evaluates non-nil, then this
	;; item in tag-table-alist applies to this buffer.
	(when (if (stringp expression)
		  (string-match expression key)
		(ignore-errors
		  (eval expression)))
	  ;; Now evaluate the cdr of the alist item to get the name of
	  ;; the tag table file.
	  (setq expression (ignore-errors
			     (eval (cdr item))))
	  (if (stringp expression)
	      (push expression result)
	    (error "Expression in tag-table-alist evaluated to non-string")))))
    (setq result
	  (mapcar
	   #'(lambda (name)
	       (when (file-directory-p name)
		 (setq name (expand-file-name "TAGS" name)))
	       (and (file-readable-p name)
		    ;; get-tag-table-buffer has side-effects
		    (symbol-value-in-buffer 'buffer-file-name
					    (get-tag-table-buffer name))))
	   result))
    (setq result (delq nil result))
    ;; If no TAGS file has been found, ask the user explicitly.
    ;; #### tags-file-name is *evil*.
    (or result tags-file-name
	(call-interactively 'visit-tags-table))
    (when tags-file-name
      (setq result (nconc result (list tags-file-name))))
    ;; Lets see if we can deal with "include" TAGS files here
    (let ((tag-files result)
	  inc-files)
      (while tag-files
	(set-buffer (find-file-noselect (car tag-files)))
	(when (setq inc-files (tag-table-include-files))
	  (tag-loop for file in inc-files
	    do (with-current-buffer (find-file-noselect file)
		 (or (and (tag-table-include-files)
			  (setq inc-files (append inc-files
						  (tag-table-include-files))))
		     (setq result (append result (list file)))))))
	(setq tag-files (cdr tag-files))))
    (or result (error "Buffer has no associated tag tables"))
    (delete-duplicates (nreverse result) :test 'equal)))

;;;###autoload
(defun visit-tags-table (file)
  "Tell tags commands to use tags table file FILE when all else fails.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory."
  (interactive (list (read-file-name "Visit tags table: (default TAGS) "
				     default-directory
				     (expand-file-name "TAGS" default-directory)
				     t)))
  (if (string-equal file "")
      (setq tags-file-name nil)
    (setq file (expand-file-name file))
    (when (file-directory-p file)
      (setq file (expand-file-name "TAGS" file)))
    ;; It used to be that, if a user pressed RET by mistake, the bogus
    ;; `tags-file-name' would remain, causing the error at
    ;; `buffer-tag-table'.
    (when (file-exists-p file)
      (setq tags-file-name file))))

(defun set-buffer-tag-table (file)
  "In addition to the tags tables specified by the variable `tag-table-alist',
each buffer can have one additional table.  This command sets that.
See the documentation for the variable `tag-table-alist' for more information."
  (interactive
   (list
     (read-file-name "Visit tags table: (directory sufficient) "
		     nil default-directory t)))
  (or file (error "No TAGS file name supplied"))
  (setq file (expand-file-name file))
  (when (file-directory-p file)
    (setq file (expand-file-name "TAGS" file)))
  (or (file-exists-p file) (error "TAGS file missing: %s" file))
  (setq buffer-tag-table file))


;; Manipulating the tag table buffer

(defconst tag-table-completion-status nil
  "Indicates whether a completion table has been built.
Either nil, t, or `disabled'.")
(make-variable-buffer-local 'tag-table-completion-status)

(defvar tag-table-files nil
  "List of files referenced by the known TAGS tables.")

(defun get-tag-table-buffer (tag-table)
  "Returns a buffer visiting the given TAGS table.
If appropriate, reverting the buffer, and possibly build a completion-table."
  (or (stringp tag-table)
      (error "Bad tags file name supplied: %s" tag-table))
  ;; Remove symbolic links from name.
  (setq tag-table (symlink-expand-file-name tag-table))
  (let (buf build-completion check-name)
    (setq buf (get-file-buffer tag-table))
    (unless buf
      (if (file-readable-p tag-table)
	  (setq buf (find-file-noselect tag-table)
		check-name t)
	(error "No such tags file: %s" tag-table)))
    (with-current-buffer buf
      ;; Make the TAGS buffer invisible.
      (when (and check-name
		 make-tags-files-invisible
		 (string-match "\\`[^ ]" (buffer-name)))
	(rename-buffer (generate-new-buffer-name
			(concat " " (buffer-name)))))
      (or (verify-visited-file-modtime buf)
	  (cond ((or tags-auto-read-changed-tag-files
		     (yes-or-no-p
		      (format "Tags file %s has changed, read new contents? "
			      tag-table)))
		 (when tags-auto-read-changed-tag-files
		   (message "Tags file %s has changed, reading new contents..."
			    tag-table))
		 (revert-buffer t t)
		 (when (eq tag-table-completion-status t)
		   (setq tag-table-completion-status nil)))))
      (or (eq (char-after 1) ?\f)
	  (error "File %s not a valid tags file" tag-table))
      (or (memq tag-table-completion-status '(t disabled))
	  (setq build-completion t))
      (when build-completion
	(if tags-build-completion-table
	    (condition-case nil
		(progn
		  (if tags-exuberant-ctags-optimization-p
		      (add-to-tag-completion-table-exuberant-ctags)
		    (let ((multi (tag-table-include-files)))
		      (if multi
			  (tag-loop for file in multi
			    do (with-current-buffer (find-file-noselect file)
				 (or (and (tag-table-include-files)
					  (setq multi (append multi
							      (tag-table-include-files))))
				     (add-to-tag-completion-table))))
			(add-to-tag-completion-table))))
		  (setq tag-table-completion-status t))
	      ;; Allow user to C-g out correctly
	      (quit
	       (message "Tags completion table construction aborted")
	       (setq tag-table-completion-status nil
		     quit-flag t)
	       t))
	  ;; The table is verboten.
	  (setq tag-table-completion-status 'disabled))))
    buf))

(defun file-of-tag ()
  "Return the file name of the file whose tags point is within.
Assumes the tag table is the current buffer.
File name returned is relative to tag table file's directory."
  (let ((opoint (point))
	prev size)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) opoint)
	(forward-line 1)
	(end-of-line)
	(skip-chars-backward "^,\n")
	(setq prev (point)
	      size (read (current-buffer)))
	(goto-char prev)
	(forward-line 1)
	;; New include syntax
	;;   filename,include
	;; tacked on to the end of a tag file means use filename
	;; as a tag file before giving up.
	;; Skip it here.
	(unless (eq size 'include)
	  (forward-char size)))
      (goto-char (1- prev))
      (buffer-substring (point) (point-at-bol)))))

(defun tag-table-include-files ()
  "Return all file names associated with `include' directives in a tag buffer."
  ;; New include syntax
  ;;   filename,include
  ;; tacked on to the end of a tag file means use filename as a
  ;; tag file before giving up.
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\f\n\\(.*\\),include$" nil t)
	(push (match-string 1) files)))
    files))

(defvar tag-table-files-bloom (make-bloom)
  "Bloom filter for tag table file list.")

(defun tag-table-files-from-includes ()
  "Used by `tag-table-files' when dealing with included tables."
  (let ((tables (tag-table-include-files)))
    (tag-loop for tag-table in tables
      do (with-current-buffer (find-file-noselect tag-table)
	   (or (and (tag-table-include-files)
		    (setq tables (append tables
					 (tag-table-include-files))))
	       (tag-table-files tag-table))))))

(defun tag-table-files (tag-table)
  "Returns a list of the files referenced by TAG-TABLE."
  (with-current-buffer (get-tag-table-buffer tag-table)
    (if (tag-table-include-files)
	(tag-table-files-from-includes)
      (let (file files prev size)
	(goto-char (point-min))
	(while (not (eobp))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward "^,\n")
	  (setq prev (point)
		size (read (current-buffer)))
	  (goto-char prev)
	  (setq file (expand-file-name (buffer-substring (1- (point))
							 (point-at-bol))
				       default-directory))
	  (unless (bloom-owns-p tag-table-files-bloom file)
	    (bloom-add tag-table-files-bloom file)
	    (push file files))
	  (forward-line 1)
	  (forward-char size))
	(setq tag-table-files (append tag-table-files files))))
    tag-table-files))

(defun tag-table-directories (tag-table)
  "Return a sorted list of directories referenced by TAG-TABLE."
  (sort (remove-duplicates (mapfam #'file-dirname
				   :result-type #'list
				   (tag-table-files tag-table))
			   :test #'string=)
	#'string<))

(defun buffer-tag-table-files ()
  "Returns a list of all files referenced by all TAGS tables that
this buffer uses."
  (when (zerop (bloom-size tag-table-files-bloom))
    (tag-loop for table in (buffer-tag-table-list)
      do (tag-table-files table)))
  tag-table-files)


;; Building the completion table

;; Test cases for building completion table; must handle these properly:
;; Lisp_Int, XSETINT, current_column 60,2282
;;	   Lisp_Int, XSETINT, point>NumCharacters ? 0 : CharAt(363,9935
;;	   Lisp_Int, XSETINT, point<=FirstCharacter ? 0 : CharAt(366,10108
;;	 point<=FirstCharacter || CharAt(378,10630
;;	 point>NumCharacters || CharAt(382,10825
;; DEFUN ("x-set-foreground-color", Fx_set_foreground_color,191,4562
;; DEFUN ("x-set-foreground-color", Fx_set_foreground_color,191,4562
;; DEFUN ("*", Ftimes,1172,32079
;; DEFUN ("/=", Fneq,1035,28839
;; defun_internal 4199,101362
;; int pure[PURESIZE / sizeof 53,1564
;; char staticvec1[NSTATICS * sizeof 667,17608
;;  Date: 04 May 87 23:53:11 PDT 26,1077
;; #define anymacroname(324,4344
;; (define-key ctl-x-map 311,11784
;; (define-abbrev-table 'c-mode-abbrev-table 24,1016
;; static char *skip_white(116,3443
;; static foo 348,11643
;; (defun texinfo-insert-@code 91,3358
;; (defvar texinfo-kindex)29,1105
;; (defun texinfo-format-\. 548,18376
;; (defvar sm::menu-kludge-y 621,22726
;; (defvar *mouse-drag-window* 103,3642
;; (defun simula-back-level(317,11263
;; } DPxAC,380,14024
;; } BM_QCB;69,2990
;; #define MTOS_DONE\t

;; "^[^ ]+ +\\([^ ]+\\) "

;; void *find_cactus_segment(116,2444
;; void *find_pdb_segment(162,3688
;; void init_dclpool(410,10739
;; WORD insert_draw_command(342,8881
;; void *req_pdbmem(579,15574

(defvar tag-completion-table (make-hash-table :test #'eq :size 4096))

(defvar buffer-tag-table-list)

(defun add-tag-symbol (tag)
  "Turn TAG into a symbol and add it to `tag-completion-table'."
  (let* ((key (make-symbol tag))
	 (val key))
      (puthash key val tag-completion-table)))

;; Can't use "\\s-" in these patterns because that will include newline
;; \2 matches an explicit name.
(defconst tags-explicit-name-pattern "\177\\(\\([^\n\001]+\\)\001\\)?")
;; \1 matches Lisp-name, \2 matches C-name, \5 (from
;; tags-explicit-name-pattern) matches explicit name.
(defconst tags-DEFUN-pattern
  (concat "DEFUN[ \t]*(\"\\([^\"]+\\)\",[ \t]*" #r"\(\(\sw\|\s_\)+\),"
	  tags-explicit-name-pattern))
;; \1 matches an array name.  Explicit names unused?
(defconst tags-array-pattern ".*[ \t]+\\([^ \[]+\\)\\[")
;; \2 matches a Lispish name, \5 (from tags-explicit-name-pattern) matches
;; explicit name.
(defconst tags-def-pattern
  (concat "\\(.*[ \t]+\\)?\\**\\(\\(\\sw\\|\\s_\\)+\\)[ ();,\t]*"
;; "\\(.*[ \t]+\\)?\\(\\(\\sw\\|\\s_\\)+\\)[ ()]*"
;; "\\(\\sw\\|\\s_\\)+[ ()]*"
	  tags-explicit-name-pattern)
      )
;; \1 matches Schemish name, \4 (from tags-explicit-name-pattern) matches
;; explicit name
(defconst tags-schemish-pattern
  (concat #r"\s-*(\s-*def\sw*\s-*(?\s-*\(\(\sw\|\s_\|:\)+\))?\s-*"
	  tags-explicit-name-pattern))
(defconst tags-file-pattern "^\f\n\\(.+\\),[0-9]+\n")

(defun add-to-tag-completion-table-exuberant-ctags ()
  "Sucks the current buffer (a TAGS table) into the completion-table.
This is a version which is optimized for exuberant etags and will not
work with xemacs etags."
  (message "Adding %s to tags completion table..." buffer-file-name)
  (goto-char (point-min))
  (let ((case-fold-search nil)
	name)
    (while (re-search-forward tags-explicit-name-pattern nil t)
      ;; no need to check the mode here
      (setq name (match-string 2))
      (add-tag-symbol name)))
  (message "Adding %s to tags completion table...done" buffer-file-name))

(defun add-to-tag-completion-table ()
  "Sucks the current buffer (a TAGS table) into the completion-table."
  (message "Adding %s to tags completion table..." buffer-file-name)
  (goto-char (point-min))
  (let ((case-fold-search nil)
	filename file-type name name2 name3)
    ;; Loop over the files mentioned in the TAGS file for each file,
    ;; try to find its major-mode, then process tags appropriately.
    (while (looking-at tags-file-pattern)
      (goto-char (match-end 0))
      (setq filename (file-name-sans-versions (match-string 1))
	    ;; We used to check auto-mode-alist for the proper
	    ;; file-type.  This was way too slow, as it had to process
	    ;; an enormous amount of regexps for each time.  Now we
	    ;; use the shotgun approach with only two regexps.
	    file-type (cond ((string-match #r"\.\([cC]\|cc\|cxx\)\'"
					   filename)
			     'c-mode)
			    ((string-match #r"\.\(el\|cl\|lisp\)\'"
					   filename)
			     'lisp-mode)
			    ((string-match #r"\.scm\'" filename)
			     'scheme-mode)
			    (t nil)))
      (defvar c-mode-syntax-table)
      (set-syntax-table (cond ((and (eq file-type 'c-mode)
				    c-mode-syntax-table)
			       c-mode-syntax-table)
			      ((eq file-type 'lisp-mode)
			       lisp-mode-syntax-table)
			      (t (standard-syntax-table))))
      ;; Clear loop variables.
      (setq name nil name2 nil name3 nil)
      (lmessage 'progress "%s..." filename)
      ;; Loop over the individual tag lines.
      (while (not (or (eobp) (eq (char-after) ?\f)))
	(cond ((and (eq file-type 'c-mode)
		    (looking-at "DEFUN[ \t]"))
	       ;; DEFUN
	       (or (looking-at tags-DEFUN-pattern)
		   (error "DEFUN doesn't fit pattern"))
	       (setq name (match-string 1)
		     name2 (match-string 2)
		     name3 (match-string 5)))
	      ;;((looking-at "\\s-")
	      ;; skip probably bogus entry:
	      ;;)
	      ((and (eq file-type 'c-mode)
		    (looking-at ".*\\["))
	       ;; Array
	       (cond ((not (looking-at tags-array-pattern))
		      (message "array definition doesn't fit pattern")
		      (setq name nil))
		     (t
		      (setq name (match-string 1)))))
	      ((and (eq file-type 'scheme-mode)
		    (looking-at tags-schemish-pattern))
	       ;; Something Schemish (is this really necessary??)
	       (setq name (match-string 1)
		     name2 (match-string 4)))
	      ((looking-at tags-def-pattern)
	       ;; ???
	       (setq name (match-string 2)
		     name2 (match-string 5))))
	;; add the tags we found to the completion table
	(and name (add-tag-symbol name))
	(and name2 (add-tag-symbol name2))
	(and name3 (add-tag-symbol name3))
	(forward-line 1)))
    (or (eobp) (error "Bad TAGS file")))
  (message "Adding %s to tags completion table...done" buffer-file-name))



;; Interactive find-tag

(defcustom find-tag-hook nil
  "*Hook called after a tag is found."
  :type 'hook
  :group 'etags)

(defun find-tag-default ()
  "Return a default tag to search for, based on the text at point."
  (symbol-near-point))

(defun buffer-tag-table-symbol-list ()
  (mapfam
   #'(lambda (table-name)
       (puthash (make-symbol table-name)
		(make-symbol table-name) tag-completion-table))
   :result-type #'list
   (buffer-tag-table-list)))

(defvar find-tag-history nil
  "History list for find-tag-tag.")

(defun find-tag-tag (prompt)
  (let* ((default (find-tag-default))
	 (buffer-tag-table-list (buffer-tag-table-symbol-list))
	 (table tag-completion-table)
	 tag-name)
    (setq tag-name
	  (completing-read
	   (if default
	       (format "%s(default %s) " prompt default)
	     prompt)
	   (hash-values-to-vector table) nil nil nil
	   'find-tag-history default))
    tag-name))

(defvar last-tag-data nil
  "Information for continuing a tag search.
Is of the form (TAG POINT MATCHING-EXACT TAG-TABLE TAG-TABLE ...).")

(defvar tags-loop-operate nil
  "Form for `tags-loop-continue' to eval to change one file.")

(defvar tags-loop-scan
  '(error "%s" (substitute-command-keys
		"No \\[tags-search] or \\[tags-query-replace] in progress."))
  "Form for `tags-loop-continue' to eval to scan one file.
If it returns non-nil, this file needs processing by evalling
\`tags-loop-operate'.  Otherwise, move on to the next file.")

(autoload 'get-symbol-syntax-table "symbol-syntax")

(defun find-tag-internal (tagname)

  (let ((next (null tagname))
	(tmpnext (null tagname))
	;; If tagname is a list: (TAGNAME), this indicates
	;; requiring an exact symbol match.
	(exact (or tags-always-exact (consp tagname)))
	(normal-syntax-table (syntax-table))
	(exact-syntax-table (get-symbol-syntax-table (syntax-table)))
	tag-table-currently-matching-exact
	tag-target exact-tagname
	tag-tables tag-table-point file linebeg line startpos buf
	offset found pat syn-tab)
    (when (consp tagname)
      (setq tagname (car tagname)))
    (cond (next
	   (setq tagname (car last-tag-data))
	   (setq tag-table-currently-matching-exact
		 (car (cdr (cdr last-tag-data)))))
	  (t
	   (setq tag-table-currently-matching-exact t)))
    ;; \_ in the tagname is used to indicate a symbol boundary.
    (if tags-exuberant-ctags-optimization-p
	(setq exact-tagname (format "\C-?%s\C-a" tagname))
      (setq exact-tagname (format "\C-?%s\C-a\\|\
\\_%s.?\C-?[0-9]*,[0-9]*$" tagname tagname))
      )
    (while (string-match #r"\\_" exact-tagname)
      (aset exact-tagname (1- (match-end 0)) ?b))
    (save-excursion
      (catch 'found
	;; Loop searching for exact matches and then inexact matches.
	(while (not (eq tag-table-currently-matching-exact 'neither))
	  (cond (tmpnext
		 (setq tag-tables (cdr (cdr (cdr last-tag-data)))
		       tag-table-point (car (cdr last-tag-data)))
		 ;; Start from the beginning of the table list on the
		 ;; next iteration of the loop.
		 (setq tmpnext nil))
		(t
		 (setq tag-tables (buffer-tag-table-list)
		       tag-table-point 1)))
	  (if tag-table-currently-matching-exact
	      (setq tag-target exact-tagname
		    syn-tab exact-syntax-table)
	    (setq tag-target tagname
		  syn-tab normal-syntax-table))
	  (with-search-caps-disable-folding tag-target t
	    (while tag-tables
	      (set-buffer (get-tag-table-buffer (car tag-tables)))
	      (bury-buffer (current-buffer))
	      (goto-char (or tag-table-point (point-min)))
	      (setq tag-table-point nil)
	      (letf (((syntax-table) syn-tab)
		     (case-fold-search nil))
		;; #### should there be support for non-regexp
		;; tag searches?
		(while (re-search-forward tag-target nil t)
		  (and (save-match-data
			 (save-excursion
			   (goto-char (match-beginning 0))
			   (looking-at "[^\n\C-?]*\C-?")))
		       ;; If we're looking for inexact matches, skip
		       ;; exact matches since we've visited them
		       ;; already.
		       (or tag-table-currently-matching-exact
			   (letf (((syntax-table) exact-syntax-table))
			     (save-excursion
			       (goto-char (match-beginning 0))
			       (not (looking-at exact-tagname)))))
		       (throw 'found t))))
	      (setq tag-tables
		    (nconc (tag-table-include-files) (cdr tag-tables)))))
	  (if (and (not exact) (eq tag-table-currently-matching-exact t))
	      (setq tag-table-currently-matching-exact nil)
	    (setq tag-table-currently-matching-exact 'neither)))
	(error "No %sentries %s %s"
	       (if next "more " "")
	       (if exact "matching" "containing")
	       tagname))
      (beginning-of-line)

      ;; from here down, synched with FSF 20.7
      ;; etags-snarf-tag and etags-goto-tag-location. --ben

      (if (save-excursion
	    (forward-line -1)
	    (looking-at "\f\n"))
	  (progn
	    ;; The match was for a source file name, not any tag
	    ;; within a file.  Give text of t, meaning to go exactly
	    ;; to the location we specify, the beginning of the file.
	    (setq linebeg t
		  line nil
		  startpos 1)
	    (setq file
		  (expand-file-name (file-of-tag)
				    ;; In SXEmacs, this needs to be
				    ;; relative to:
				    (or (file-dirname (car tag-tables))
					"./"))))
	(search-forward "\C-?")
	(setq file
	      (expand-file-name (file-of-tag)
				;; In SXEmacs, this needs to be
				;; relative to:
				(or (file-dirname (car tag-tables))
				    "./")))
	(setq linebeg (buffer-substring (1- (point)) (point-at-bol)))
	;; Skip explicit tag name if present.
	(search-forward "\001" (save-excursion (forward-line 1) (point)) t)
	(if (looking-at "[0-9]")
	    (setq line (string-to-int (buffer-substring
				       (point)
				       (progn (skip-chars-forward "0-9")
					      (point))))))
	(search-forward ",")
	(if (looking-at "[0-9]")
	    (setq startpos (string-to-int (buffer-substring
					   (point)
					   (progn (skip-chars-forward "0-9")
						  (point)))))))
      ;; Leave point on the next line of the tags file.
      (forward-line 1)
      (setq last-tag-data
	    (nconc (list tagname (point) tag-table-currently-matching-exact)
		   tag-tables))
      (setq buf (find-file-noselect file))

      ;; LINEBEG is the initial part of a line containing the tag and
      ;; STARTPOS is the character position of LINEBEG within the file
      ;; (starting from 1); LINE is the line number.  If LINEBEG is t,
      ;; it means the tag refers to exactly LINE or STARTPOS
      ;; (whichever is present, LINE having preference, no searching).
      ;; Either LINE or STARTPOS may be nil; STARTPOS is used if
      ;; present.  If the tag isn't exactly at the given position then
      ;; look around that position using a search window which expands
      ;; until it hits the start of file.

      (with-current-buffer buf
	(save-excursion
	  (save-restriction
	    (widen)
	    (if (eq linebeg t)
		;; Direct file tag.
		(cond (line (goto-line line))
		      (startpos (goto-char startpos))
		      (t (error "etags.el BUG: bogus direct file tag")))
	      ;; Here we search for PAT in the range [STARTPOS - OFFSET,
	      ;; STARTPOS + OFFSET], with increasing values of OFFSET.
	      ;;
	      ;; We used to set the initial offset to 1000, but the
	      ;; actual sources show that finer-grained control is
	      ;; needed (e.g. two `hash_string's in src/symbols.c.)  So,
	      ;; I changed 1000 to 100, and (* 3 offset) to (* 5 offset).
	      (setq offset 100)
	      (setq pat (concat (if (eq selective-display t)
				    "\\(^\\|\^m\\)" "^")
				(regexp-quote linebeg)))

	      ;; The character position in the tags table is 0-origin.
	      ;; Convert it to a 1-origin Emacs character position.
	      (if startpos (setq startpos (1+ startpos)))
	      ;; If no char pos was given, try the given line number.
	      (or startpos
		  (if line
		      (setq startpos (progn (goto-line line)
					    (point)))))
	      (or startpos
		  (setq startpos (point-min)))
	      ;; First see if the tag is right at the specified location.
	      (goto-char startpos)
	      (setq found (looking-at pat))
	      (while (and (not found)
			  (progn
			    (goto-char (- startpos offset))
			    (not (bobp))))
		(setq found
		      (re-search-forward pat (+ startpos offset) t)
		      offset (* 5 offset))) ; expand search window
	      ;; Finally, try finding it anywhere in the buffer.
	      (or found
		  (re-search-forward pat nil t)
		  (error "Rerun etags: `%s' not found in %s"
			 pat file))))
	  ;; Position point at the right place
	  ;; if the search string matched an extra Ctrl-m at the beginning.
	  (and (eq selective-display t)
	       (looking-at "\^m")
	       (forward-char 1))
	  (beginning-of-line)
	  (setq startpos (point))))
      (cons buf startpos))))

;;;###autoload
(defun find-tag-at-point (tagname &optional other-window)
  "*Find tag whose name contains TAGNAME.
Identical to `find-tag' but does not prompt for tag when called interactively;
instead, uses tag around or before point."
  (interactive (list (find-tag-default) nil))
  (if current-prefix-arg
      (find-tag tagname 'other-window)
    (find-tag tagname)))

;;;###autoload
(defun find-tag (tagname &optional other-window)
  "Find tag whose name contains TAGNAME.

Selects the buffer that the tag is contained in and puts point at
its definition.  If TAGNAME is a null string, the expression in
the buffer around or before point is used as the tag name.  If
called interactively with a numeric argument, searches for the
next tag in the tag table that matches the tagname used in the
previous find-tag.  If second arg OTHER-WINDOW is non-nil, uses
another window to display the tag.

This version of this function supports multiple active tags tables,
and completion.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember"
  (interactive (list (find-tag-tag "Find tag: ") nil))
  (let* ((next (null tagname))
	 (result (find-tag-internal tagname))
	 (tag-buf (car result))
	 (tag-point (cdr result))
	 (other-window (or other-window current-prefix-arg)))
    ;; Push old position on the tags mark stack.
    (if (or (not next)
	    (not (memq last-command
		       '(find-tag find-tag-other-window tags-loop-continue))))
	(push-tag-mark))
    (if other-window
	(pop-to-buffer tag-buf t)
      (switch-to-buffer tag-buf))
    (widen)
    (push-mark)
    (goto-char tag-point)
    (run-hooks 'find-tag-hook))
  (setq tags-loop-scan (list 'find-tag nil nil)
	tags-loop-operate nil)
  ;; Return t in case used as the tags-loop-scan.
  t)

;;;###autoload
(defun find-tag-other-window (tagname &optional next)
  "*Find tag whose name contains TAGNAME, in another window.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

This version of this function supports multiple active tags tables,
and completion.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember"
  (interactive (list (find-tag-tag "Find tag other window: ")))
  (if (or next current-prefix-arg)
      (find-tag nil t)
    (find-tag tagname t)))


;; Completion on tags in the buffer.


;; Applying a command to files mentioned in tag tables

(defvar next-file-list nil
  "List of files for next-file to process.")

;;;###autoload
(defun next-file (&optional initialize novisit)
  "Select next file among files in current tag table(s).

A first argument of t (prefix arg, if interactive) initializes to the
beginning of the list of files in the (first) tags table.  If the argument
is neither nil nor t, it is evalled to initialize the list of files.

Non-nil second argument NOVISIT means use a temporary buffer
to save time and avoid uninteresting warnings.

Value is nil if the file was already visited;
if the file was newly read in, the value is the filename."
  (interactive "P")
  (cond ((not initialize)
	 ;; Not the first run.
	 )
	((eq initialize t)
	 ;; Initialize the list from the tags table.
	 (setq next-file-list (buffer-tag-table-files)))
	(t
	 ;; Initialize the list by evalling the argument.
	 (setq next-file-list (eval initialize))))
  (when (null next-file-list)
    (and novisit
	 (get-buffer " *next-file*")
	 (kill-buffer " *next-file*"))
    (error "All files processed"))
  (let* ((file (car next-file-list))
	 (buf (get-file-buffer file))
	 (new (not buf)))
    (pop next-file-list)

    (if (not (and new novisit))
	(switch-to-buffer (find-file-noselect file novisit) t)
      ;; Like find-file, but avoids random junk.
      (set-buffer (get-buffer-create " *next-file*"))
      (kill-all-local-variables)
      (erase-buffer)
      (insert-file-contents file nil))
    (widen)
    (when (> (point) (point-min))
      (push-mark nil t)
      (goto-char (point-min)))
    (and new file)))

;;;###autoload
(defun tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument to begin such a command (the
argument is passed to `next-file', which see).
Two variables control the processing we do on each file:
the value of `tags-loop-scan' is a form to be executed on each file
to see if it is interesting (it returns non-nil if so)
and `tags-loop-operate' is a form to execute to operate on an interesting file
If the latter returns non-nil, we exit; otherwise we scan the next file."
  (interactive)
  (let ((messaged nil)
	(more-files-p t)
	new)
    (while more-files-p
      ;; Scan files quickly for the first or next interesting one.
      (while (or first-time
		 (save-restriction
		   (widen)
		   (not (eval tags-loop-scan))))
	(setq new (next-file first-time
			     tags-search-nuke-uninteresting-buffers))
	;; If NEW is non-nil, we got a temp buffer,
	;; and NEW is the file name.
	(if (or messaged
		(and (not first-time)
		     (> (device-baud-rate) search-slow-speed)
		     (setq messaged t)))
	    (lmessage 'progress
		"Scanning file %s..." (or new buffer-file-name)))
	(setq first-time nil)
	(goto-char (point-min)))

      ;; If we visited it in a temp buffer, visit it now for real.
      (if (and new tags-search-nuke-uninteresting-buffers)
	  (let ((pos (point)))
	    (erase-buffer)
	    (set-buffer (find-file-noselect new))
	    (widen)
	    (goto-char pos)))

      (switch-to-buffer (current-buffer))

      ;; Now operate on the file.
      ;; If value is non-nil, continue to scan the next file.
      (setq more-files-p (eval tags-loop-operate)))
    (and messaged
	 (null tags-loop-operate)
	 (message "Scanning file %s...found" buffer-file-name))))

;;;###autoload
(defun tags-search (regexp &optional file-list-form)
  "Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable `tag-table-alist'."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
	   (eq (car tags-loop-scan) 'with-search-caps-disable-folding)
	   (null tags-loop-operate))
      ;; Continue last tags-search as if by `M-,'.
      (tags-loop-continue nil)
    (setq tags-loop-scan `(with-search-caps-disable-folding ,regexp t
			    (re-search-forward ,regexp nil t))
	  tags-loop-operate nil)
    (tags-loop-continue (or file-list-form t))))

;;;###autoload
(defun tags-query-replace (from to &optional delimited file-list-form)
  "Query-replace-regexp FROM with TO through all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable `tag-table-alist'."
  (interactive
   "sTags query replace (regexp): \nsTags query replace %s by: \nP")
  (setq tags-loop-scan `(with-search-caps-disable-folding ,from t
			  (if (re-search-forward ,from nil t)
			      ;; When we find a match, move back
			      ;; to the beginning of it so perform-replace
			      ;; will see it.
			      (progn (goto-char (match-beginning 0)) t)))
	tags-loop-operate (list 'perform-replace from to t t
				(not (null delimited))))
   (tags-loop-continue (or file-list-form t)))

;; Miscellaneous
(defun find-tag-regex (tagname)
  "Use `igrep-find' to find all occurances of tag with TAGNAME."
  (interactive (if current-prefix-arg
		   (list (current-word))
		 (list (find-tag-tag "Find tag: "))))
  (and-fboundp #'igrep-find
    (igrep-find "grep" tagname (tag-table-directories tags-file-name))))

(defun taglist-find-tag ()
  "Jump to a tag from the \"*Tags List*\" buffer.
With a prefix arg, jump to the tag in another window."
  (interactive)
  (save-excursion
    (progn (goto-char (point-at-eol))
	   (backward-word))
    (let ((tagname (find-tag-default)))
      (if current-prefix-arg
	  (find-tag tagname 'other-window)
	(find-tag tagname)))))

(defun taglist-display-tag-info ()
  "Display in the minibuffer short info of tag.

See `display-tag-info'."
  (interactive)
  (save-excursion
    (progn (goto-char (point-at-eol))
	   (backward-word))
    (let ((tagname (find-tag-default)))
      (display-tag-info tagname))))

(defvar taglist-mode-map
  (let ((map (make-keymap 'taglist-mode-map)))
    (suppress-keymap map)
    (define-key map [return] #'taglist-find-tag)
    (define-key map [\?] #'taglist-display-tag-info)
    (define-key map [n] #'next-line)
    (define-key map [p] #'previous-line)
    (define-key map [q] #'bury-buffer)
    (define-key map [space] #'scroll-up-command)
    (define-key map [next] #'scroll-up-command)
    (define-key map [delete] #'scroll-down-command)
    (define-key map [backspace] #'scroll-down-command)
    (define-key map [prior] #'scroll-down-command)
    map)
  "Keymap for `taglist-mode'.")

(define-derived-mode taglist-mode fundamental-mode "Etags"
  "A simple mode for navigating around a tags list."
  :group 'etags
  (set-buffer-modified-p nil))

;;;###autoload
(defun list-tags (file &optional other-window)
  "Display list of tags in FILE.

With optional prefix arg, OTHER-WINDOW, display list there."
  (interactive (list (read-file-name
		      (if (buffer-file-name)
			  (format "List tags (in file, %s by default): "
				  (file-basename (buffer-file-name)))
			"List tags (in file): ")
		      nil (buffer-file-name) t)))
  (find-file-noselect file)
  (let* ((taglist-buf (get-buffer-create "*Tags List*"))
	 (standard-output taglist-buf))
    (with-current-buffer taglist-buf
      (erase-buffer)
      (insert (format "Tags in file: %s\n\n" file)))
    (save-excursion
      (tag-dolist (tags-file (with-current-buffer (get-file-buffer file)
			       (buffer-tag-table-list)))
	;; We don't want completions getting in the way.
	(let ((tags-build-completion-table nil))
	  (set-buffer (get-tag-table-buffer tags-file)))
	(goto-char (point-min))
	(when
	    (re-search-forward (concat "\f\n.*" (file-basename file) ",")
			    nil t)
	  (forward-line 1)
	  (while (not (or (eobp) (looking-at "\f")))
	    (princ (buffer-substring (point)
				     (progn (skip-chars-forward "^\C-?")
					    (point))))
	    (terpri)
	    (forward-line 1)))))
    (if current-prefix-arg
	(pop-to-buffer taglist-buf)
      (switch-to-buffer taglist-buf))
    (goto-char (point-min))
    (forward-line 2)
    (taglist-mode)))

;;;###autoload
(defun tags-apropos (string &optional other-window)
  "Display list of all tags in tag table REGEXP matches.

With optional prefix arg, OTHER-WINDOW, display list there."
  (interactive "sTag apropos (regexp): ")
  (let* ((taglist-buf (get-buffer-create "*Tags List*"))
	 (standard-output taglist-buf))
    (with-current-buffer taglist-buf
      (erase-buffer)
      (insert (format "Tags matching regexp: %S\n\n" string)))
    (save-excursion
      (tag-loop for file in (buffer-tag-table-list)
	do (progn
	     (set-buffer (find-file-noselect file))
	     (goto-char (point-min))
	     (while (re-search-forward string nil t)
	       (beginning-of-line)
	       (princ (buffer-substring (point)
					(progn (skip-chars-forward "^\C-?")
					       (point))))
	       (terpri)
	       (forward-line 1)))))
    (if current-prefix-arg
	(pop-to-buffer taglist-buf)
      (switch-to-buffer taglist-buf))
    (goto-char (point-min))
    (forward-line 2)
    (taglist-mode)))



;; Display short info on tag in minibuffer

;; Don't pollute `M-?' -- we may need it for more important stuff.  --hniksic
;(if (null (lookup-key esc-map "?"))
;    (define-key esc-map "?" 'display-tag-info))

(defun display-tag-info (tagname)
  "Prints a description of the first tag matching TAGNAME in the echo area.
If this is an elisp function, prints something like \"(defun foo (x y z)\".
That is, is prints the first line of the definition of the form.
If this is a C-defined elisp function, it does something more clever."
  (interactive (if current-prefix-arg
		   '(nil)
		 (list (find-tag-tag "Display tag info: "))))
  (let* ((results (find-tag-internal tagname))
	 (tag-buf (car results))
	 (tag-point (cdr results))
	 info lname min max fname args)
    (with-current-buffer tag-buf
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char tag-point)
	  (cond ((let ((case-fold-search nil))
		   (looking-at (concat "^DEFUN(\"" tagname)))
		 (forward-sexp 1)
		 (down-list 1)
		 (setq lname (read (current-buffer))
		       fname (buffer-substring
			      (progn (forward-sexp 1) (point))
			      (progn (backward-sexp 1) (point)))
		       min (buffer-substring
			    (progn (forward-sexp 2) (point))
			    (progn (backward-sexp 1) (point)))
		       max (buffer-substring
			    (progn (forward-sexp 2) (point))
			    (progn (backward-sexp 1) (point))))
		 (search-forward "*/")
		 (setq args (buffer-substring
			     (progn (forward-sexp 1) (point))
			     (progn (backward-sexp 1) (point))))
		 (setq info (format "Elisp: %s, C: %s %s, #args: %s"
				    lname
				    fname args
				    (if (string-equal min max)
					min
				      (format "from %s to %s" min max)))))
		(t
		 (setq info
		       (buffer-substring
			(progn (beginning-of-line) (point))
			(progn (end-of-line) (point)))))))))
    (message "%s" info))
  (setq tags-loop-scan '(display-tag-info nil)
	tags-loop-operate nil)
  ;; Always return non-nil
  t)


;; Tag mark stack.

(defvar tag-mark-stack1 nil)
(defvar tag-mark-stack2 nil)

(defcustom tag-mark-stack-max 16
  "*The maximum number of elements kept on the mark-stack used
by tags-search.  See also the commands `\\[push-tag-mark]' and
and `\\[pop-tag-mark]'."
  :type 'integer
  :group 'etags)

(defun push-mark-on-stack (stack-symbol &optional max-size)
  (let ((stack (symbol-value stack-symbol)))
    (push (point-marker) stack)
    (cond ((and max-size
		(> (length stack) max-size))
	   (set-marker (car (nthcdr max-size stack)) nil)
	   (setcdr (nthcdr (1- max-size) stack) nil)))
    (set stack-symbol stack)))

(defun pop-mark-from-stack (stack-symbol1 stack-symbol2 &optional max-size)
  (let* ((stack (or (symbol-value stack-symbol1)
		    (error "No more tag marks on stack")))
	 (marker (car stack))
	 (m-buf (marker-buffer marker)))
    (set stack-symbol1 (cdr stack))
    (or m-buf
	(error "Marker has no buffer"))
    (or (buffer-live-p m-buf)
	(error "Buffer has been killed"))
    (push-mark-on-stack stack-symbol2 max-size)
    (switch-to-buffer m-buf)
    (widen)
    (goto-char marker)))

(defun push-tag-mark ()
  (push-mark-on-stack 'tag-mark-stack1 tag-mark-stack-max))

;;;###autoload (define-key esc-map "*" 'pop-tag-mark)

;;;###autoload
(defun pop-tag-mark (arg)
  "Go to last tag position.
`find-tag' maintains a mark-stack seperate from the \\[set-mark-command] mark-stack.
This function pops (and moves to) the tag at the top of this stack."
  (interactive "P")
  (if (not arg)
      (pop-mark-from-stack
       'tag-mark-stack1 'tag-mark-stack2 tag-mark-stack-max)
    (pop-mark-from-stack
     'tag-mark-stack2 'tag-mark-stack1 tag-mark-stack-max)))


(provide 'etags)
(provide 'tags)

;;; etags.el ends here
