;;; lisp-mnt.el --- minor mode for Emacs Lisp maintainers

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: Eric S. Raymond <esr@snark.thyrsus.com>
;; Created: 14 Jul 1992
;; Keywords: docs, maint
;; X-Modified-by: Bob Weiner <weiner@beopen.com>, 4/14/95, to support
;;  InfoDock headers.
;; X-Bogus-Bureaucratic-Cruft: Gruad will get you if you don't watch out!

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

;;; Synched up with: FSF 20.2.
 
;;; Commentary:

;; This minor mode adds some services to Emacs-Lisp editing mode.
;;
;; First, it knows about the header conventions for library packages.
;; One entry point supports generating synopses from a library directory.
;; Another can be used to check for missing headers in library files.
;; 
;; Another entry point automatically addresses bug mail to a package's
;; maintainer or author.

;; This file can be loaded by your lisp-mode-hook.  Have it (require 'lisp-mnt)

;; This file is an example of the header conventions.  Note the following
;; features:
;; 
;;    * Header line --- makes it possible to extract a one-line summary of
;; the package's uses automatically for use in library synopses, KWIC
;; indexes and the like.
;; 
;;    Format is three semicolons, followed by the filename, followed by
;; three dashes, followed by the summary.  All fields space-separated.
;; 
;;    * Author line --- contains the name and net address of at least
;; the principal author.
;; 
;;    If there are multiple authors, they should be listed on continuation
;; lines led by ;;<TAB><TAB> (or multiple blanks), like this:
;; 
;; ;; Author: Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;; ;;		Dave Sill <de5@ornl.gov>
;; ;;		David Lawrence <tale@pawl.rpi.edu>
;; ;;		Noah Friedman <friedman@ai.mit.edu>
;; ;;		Joe Wells <jbw@maverick.uswest.com>
;; ;;		Dave Brennan <brennan@hal.com>
;; ;;		Eric Raymond <esr@snark.thyrsus.com>
;; 
;; This field may have some special values; notably "FSF", meaning
;; "Free Software Foundation".
;; 
;;    * Maintainer line --- should be a single name/address as in the Author
;; line, or an address only, or the string "FSF".  If there is no maintainer
;; line, the person(s) in the Author field are presumed to be it.  The example
;; in this file is mildly bogus because the maintainer line is redundant.
;;    The idea behind these two fields is to be able to write a Lisp function
;; that does "send mail to the author" without having to mine the name out by
;; hand. Please be careful about surrounding the network address with <> if
;; there's also a name in the field.
;; 
;;    * Created line --- optional, gives the original creation date of the
;; file.  For historical interest, basically.
;; 
;;    * Version line --- intended to give the reader a clue if they're looking
;; at a different version of the file than the one they're accustomed to.  This
;; may be an RCS or SCCS header.
;; 
;;    * Adapted-By line --- this is for FSF's internal use.  The person named
;; in this field was the one responsible for installing and adapting the
;; package for the distribution.  (This file doesn't have one because the
;; author *is* one of the maintainers.)
;; 
;;    * Keywords line --- used by the finder code (now under construction)
;; for finding Emacs Lisp code related to a topic.
;;
;;    * X-Bogus-Bureaucratic-Cruft line --- this is a joke and an example
;; of a comment header.  Headers starting with `X-' should never be used
;; for any real purpose; this is the way to safely add random headers
;; without invoking the wrath of any program.
;;
;;    * Commentary line --- enables Lisp code to find the developer's and
;; maintainers' explanations of the package internals.
;; 
;;    * Change log line --- optional, exists to terminate the commentary
;; section and start a change-log part, if one exists.
;; 
;;    * Code line --- exists so Lisp can know where commentary and/or
;; change-log sections end.
;; 
;;    * Footer line --- marks end-of-file so it can be distinguished from
;; an expanded formfeed or the results of truncation.

;;; Change Log:

;; Tue Jul 14 23:44:17 1992	ESR
;;	* Created.

;;; Code:

(require 'picture)		; provides move-to-column-force
;(require 'emacsbug) ; XEmacs, not needed for bytecompilation

;;; Variables:

(defvar lm-header-prefix "^;;*[ \t]+\\(@\(#\)\\)?[ \t]*\\([\$]\\)?"
  "Prefix that is ignored before the tag.
For example, you can write the 1st line synopsis string and headers like this
in your Lisp package:

   ;; @(#) package.el -- package description
   ;;
   ;; @(#) $Maintainer:   Person Foo Bar $

The @(#) construct is used by unix what(1) and
then $identifier: doc string $ is used by GNU ident(1)")

(defvar lm-comment-column 16
  "Column used for placing formatted output.")

(defvar lm-commentary-header "Commentary\\|Documentation"
  "Regexp which matches start of documentation section.")

(defvar lm-history-header "Change Log\\|History"
  "Regexp which matches the start of code log section.")

;;; Functions:

;; These functions all parse the headers of the current buffer

(defsubst lm-get-header-re (header &optional mode)
  "Returns regexp for matching HEADER.
If called with optional MODE and with value `section',
return section regexp instead."
  (cond ((eq mode 'section)
	 (concat "^;;;;* " header ":[ \t]*$"))
	(t
	 (concat lm-header-prefix header ":[ \t]*"))))

(defsubst lm-get-package-name ()
  "Returns package name by looking at the first line."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at (concat lm-header-prefix))
	     (progn (goto-char (match-end 0))
		    (looking-at "\\([^\t ]+\\)")
		    (match-end 1)))
	(buffer-substring (match-beginning 1) (match-end 1))
      )))

(defun lm-section-mark (header &optional after)
  "Return the buffer location of a given section start marker.
The HEADER is the section mark string to search for.
If AFTER is non-nil, return the location of the next line."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward (lm-get-header-re header 'section) nil t)
	  (progn
	    (beginning-of-line)
	    (if after (forward-line 1))
	    (point))
	nil))))

(defsubst lm-code-mark ()
  "Return the buffer location of the `Code' start marker."
  (lm-section-mark "Code"))

(defsubst lm-commentary-mark ()
  "Return the buffer location of the `Commentary' start marker."
  (lm-section-mark lm-commentary-header))

(defsubst lm-history-mark ()
  "Return the buffer location of the `History' start marker."
  (lm-section-mark lm-history-header))

(defun lm-header (header)
  "Return the contents of the header named HEADER."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (if (and (re-search-forward (lm-get-header-re header) (lm-code-mark) t)
	     ;;   RCS ident likes format "$identifier: data$"
	     (looking-at "\\([^$\n]+\\)")
	     (match-end 1))
	(buffer-substring (match-beginning 1) (match-end 1))
      nil)))

(defun lm-header-multiline (header)
  "Return the contents of the header named HEADER, with continuation lines.
The returned value is a list of strings, one per line."
  (save-excursion
    (goto-char (point-min))
    (let ((res (lm-header header)))
      (cond
       (res
	(setq res (list res))
	(forward-line 1)

	(while (and (looking-at (concat lm-header-prefix "[\t ]+"))
		    (progn
		      (goto-char (match-end 0))
		      (looking-at #r"\(.*\)"))
		    (match-end 1))
	  (setq res (cons (buffer-substring
			   (match-beginning 1)
			   (match-end 1))
			  res))
	  (forward-line 1))
	))
      res
      )))

;; These give us smart access to the header fields and commentary

(defun lm-summary (&optional file)
  "Return the one-line summary of file FILE, or current buffer if FILE is nil."
  (save-excursion
    (if file
	(find-file file))
    (goto-char (point-min))
    (prog1
	(if (and
	     (looking-at lm-header-prefix)
	     (progn (goto-char (match-end 0))
		    (looking-at "[^ ]+[ \t]+--+[ \t]+\\(.*\\)")))
	    (buffer-substring (match-beginning 1) (match-end 1)))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-crack-address (x)
  "Split up an email address into full name and real email address.
The value is a cons of the form (FULLNAME . ADDRESS)."
  (cond ((string-match #r"\(.+\) [(<]\(\S-+@\S-+\)[>)]" x)
	 (cons (substring x (match-beginning 1) (match-end 1))
	       (substring x (match-beginning 2) (match-end 2))))
	((string-match #r"\(\S-+@\S-+\) [(<]\(.*\)[>)]" x)
	 (cons (substring x (match-beginning 2) (match-end 2))
	       (substring x (match-beginning 1) (match-end 1))))
	((string-match #r"\S-+@\S-+" x)
	 (cons nil x))
	(t
	 (cons x nil))))

(defun lm-authors (&optional file)
  "Return the author list of file FILE, or current buffer if FILE is nil.
Each element of the list is a cons; the car is the full name,
the cdr is an email address."
  (save-excursion
    (if file
	(find-file file))
    ;; XEmacs change (Is E-MAIL an infodock header? -sb)
    (let* ((authorlist (lm-header-multiline "author"))
	   (email-list (lm-header-multiline "E-MAIL"))
	   (authors authorlist))
      (prog1
	  (if (null email-list)
	      (mapcar 'lm-crack-address authorlist)
	    (while (and email-list authors)
	      (setcar authors (cons (car authors) (car email-list)))
	      (setq email-list (cdr email-list)
		    authors (cdr authors)))
	    authorlist)
	(if file
	    (kill-buffer (current-buffer))))
      )))

(defun lm-maintainer (&optional file)
  "Return the maintainer of file FILE, or current buffer if FILE is nil.
The return value has the form (NAME . ADDRESS)."
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(let ((maint (lm-header "maintainer")))
	  (if maint
	      (lm-crack-address maint)
	    (car (lm-authors))))
      (if file
	  (kill-buffer (current-buffer))))))

(defun lm-creation-date (&optional file)
  "Return the created date given in file FILE, or current buffer if FILE is nil."
  (save-excursion
    (if file
	(find-file file))
    (prog1
	;; XEmacs change (Is ORIG-DATE an Infodock header? -sb)
	(or (lm-header "created")
	    (let ((date-and-time (lm-header "ORIG-DATE")))
	      (if date-and-time
		  (substring date-and-time 0
			     (string-match " " date-and-time)))))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-last-modified-date (&optional file)
  "Return the modify-date given in file FILE, or current buffer if FILE is nil."
  (save-excursion 
    (if file
	(find-file file))
    (prog1
	(if (progn
	      (goto-char (point-min))
	      (re-search-forward
	       #r"\$[I]d: [^ ]+ [^ ]+ \([^/]+\)/\([^/]+\)/\([^ ]+\) "
	       (lm-code-mark) t))
	    (format "%s %s %s"
		    (buffer-substring (match-beginning 3) (match-end 3))
		    (nth (string-to-int 
			  (buffer-substring (match-beginning 2) (match-end 2)))
			 '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
			   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
		    (buffer-substring (match-beginning 1) (match-end 1)))
	  ;; XEmacs change (Infodock change? -sb)
	  (let ((date-and-time (lm-header "LAST-MOD")))
	    (if date-and-time
		(substring date-and-time 0
			   (string-match " " date-and-time)))))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-version (&optional file)
  "Return the version listed in file FILE, or current buffer if FILE is nil.
This can be found in an RCS or SCCS header to crack it out of."
  (save-excursion 
    (if file
	(find-file file))
    (prog1
	(or
	 (lm-header "version")
	 (let ((header-max (lm-code-mark)))
	   (goto-char (point-min))
	   (cond
	    ;; Look for an RCS header
	    ((re-search-forward #r"\$[I]d: [^ ]+ \([^ ]+\) " header-max t)
	     (buffer-substring (match-beginning 1) (match-end 1)))

	    ;; Look for an SCCS header
	    ((re-search-forward 
	      (concat
	       (regexp-quote "@(#)")
	       (regexp-quote (file-name-nondirectory (buffer-file-name)))
	       "\t\\([012345679.]*\\)")
	      header-max t)
	     (buffer-substring (match-beginning 1) (match-end 1)))

	    (t nil))))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-keywords (&optional file)
  "Return the keywords given in file FILE, or current buffer if FILE is nil."
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(let ((keywords (lm-header "keywords")))
	  (and keywords (downcase keywords)))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-adapted-by (&optional file)
  "Return the adapted-by names in file FILE, or current buffer if FILE is nil.
This is the name of the person who cleaned up this package for
distribution."
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(lm-header "adapted-by")
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-commentary (&optional file)
  "Return the commentary in file FILE, or current buffer if FILE is nil.
The value is returned as a string.  In the text, the commentary starts
with tag `Commentary' and ends with tag `Change Log' or `History'."
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(let ((commentary	(lm-commentary-mark))
	      (change-log	(lm-history-mark))
	      (code		(lm-code-mark))
	      end)
	  (cond
	   ((and commentary change-log)
	    (buffer-substring commentary change-log))
	   ((and commentary code)
	    (buffer-substring commentary code))
	   (t
	    ;; XEmacs change (Infodock headers? -sb)
	    (setq commentary (lm-section-mark "DESCRIPTION" t))
	    (setq end (lm-section-mark "DESCRIP-END"))
	    (and commentary end (buffer-substring commentary end)))))
      (if file
	  (kill-buffer (current-buffer)))
      )))

;;; Verification and synopses

(defun lm-insert-at-column (col &rest strings)
  "Insert list of STRINGS, at column COL."
  (if (> (current-column) col) (insert "\n"))
  (move-to-column-force col)
  (apply 'insert strings))

(defun lm-verify (&optional file showok &optional verb)
  "Check that the current buffer (or FILE if given) is in proper format.
If FILE is a directory, recurse on its files and generate a report in
a temporary buffer."
  (interactive)
  (let* ((verb    (or verb (interactive-p)))
	 ret
	 name
	 )
    (if verb
	(setq ret "Ok."))		;init value

    (if (and file (file-directory-p file))
	(setq
	 ret
	 (progn
	   (switch-to-buffer (get-buffer-create "*lm-verify*"))
	   (erase-buffer)
	   (mapcar
	    #'(lambda (f)
		(if (string-match ".*\\.el$" f)
		    (let ((status (lm-verify f)))
		      (if status
			  (progn
			    (insert f ":")
			    (lm-insert-at-column lm-comment-column status "\n"))
			(and showok
			     (progn
			       (insert f ":")
			       (lm-insert-at-column lm-comment-column "OK\n")))))))
	    (directory-files file))
	   ))
      (save-excursion
	(if file
	    (find-file file))
	(setq name (lm-get-package-name))

	(setq
	 ret
	 (prog1
	     (cond
	      ((null name)
	       "Can't find a package NAME")

	      ((not (lm-authors))
	       "Author: tag missing.")

	      ((not (lm-maintainer))
	       "Maintainer: tag missing.")

	      ((not (lm-summary))
	       "Can't find a one-line 'Summary' description")

	      ((not (lm-keywords))
	       "Keywords: tag missing.")

	      ((not (lm-commentary-mark))
	       "Can't find a 'Commentary' section marker.")

	      ((not (lm-history-mark))
	       "Can't find a 'History' section marker.")

	      ((not (lm-code-mark))
	       "Can't find a 'Code' section marker")

	      ((progn
		 (goto-char (point-max))
		 (not
		  (re-search-backward
		   (concat "^;;;[ \t]+" name "[ \t]+ends here[ \t]*$"
			   "\\|^;;;[ \t]+ End of file[ \t]+" name)
		   nil t
		   )))
	       (format "Can't find a footer line for [%s]" name))
	      (t
	       ret))
	   (if file
	       (kill-buffer (current-buffer)))
	   ))))
    (if verb
	(message ret))
    ret
    ))

(defun lm-synopsis (&optional file showall)
  "Generate a synopsis listing for the buffer or the given FILE if given.
If FILE is a directory, recurse on its files and generate a report in
a temporary buffer.  If SHOWALL is non-nil, also generate a line for files
which do not include a recognizable synopsis."
  (interactive
   (list
    (read-file-name "Synopsis for (file or dir): ")))

  (if (and file (file-directory-p file))
      (progn
	(switch-to-buffer (get-buffer-create "*lm-verify*"))
	(erase-buffer)
	(mapcar
	  (lambda (f) ; XEmacs - dequote
	    (if (string-match ".*\\.el$" f)
		(let ((syn (lm-synopsis f)))
		  (if syn
		      (progn
			(insert f ":")
			(lm-insert-at-column lm-comment-column syn "\n"))
		    (and showall
			 (progn
			   (insert f ":")
			   (lm-insert-at-column lm-comment-column "NA\n")))))))
	  (directory-files file))
	)
    (save-excursion
      (if file
	  (find-file file))
      (prog1
	  (lm-summary)
	(if file
	    (kill-buffer (current-buffer)))
	))))

(defun lm-report-bug (topic)
  "Report a bug in the package currently being visited to its maintainer.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (let ((package	(lm-get-package-name))
	(addr		(lm-maintainer))
	(version	(lm-version)))
    (declare-fboundp 
     (mail nil
	   (if addr
	       (concat (car addr) " <" (cdr addr) ">")
	     (or (and-boundp 'report-xemacs-bug-beta-address
		   report-xemacs-bug-beta-address)
		 "<xemacs-beta@xemacs.org>"))
	   topic))
    (goto-char (point-max))
    (insert "\nIn "
	    package
	    (if version (concat " version " version) "")
	    "\n\n")
    (message
     (substitute-command-keys "Type \\[mail-send] to send bug report."))))

(provide 'lisp-mnt)

;;; lisp-mnt.el ends here
