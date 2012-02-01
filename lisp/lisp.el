;;; lisp.el --- Lisp editing commands for SXEmacs

;; Copyright (C) 1985, 1986, 1994, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: lisp, languages, dumped

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

;;; Synched up with: Emacs/Mule zeta.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; Lisp editing commands to go with Lisp major mode.

;; 06/11/1997 - Use char-(after|before) instead of
;;  (following|preceding)-char. -slb

;;; Code:

;; Note that this variable is used by non-lisp modes too.
(defcustom defun-prompt-regexp nil
  "*Non-nil => regexp to ignore, before the character that starts a defun.
This is only necessary if the opening paren or brace is not in column 0.
See `beginning-of-defun'."
  :type '(choice (const :tag "none" nil)
		 regexp)
  :group 'lisp)

(make-variable-buffer-local 'defun-prompt-regexp)

(defcustom parens-require-spaces t
  "Non-nil => `insert-parentheses' should insert whitespace as needed."
  :type 'boolean
  :group 'editing-basics
  :group 'lisp)

(defun forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (or arg (setq arg 1))
  ;; XEmacs: evil hack! The other half of the evil hack below.
  (if (and (> arg 0) (looking-at "#s(\\|#r[uU]?\"\\|#p\\["))
    (goto-char (1+ (- (point) (- (match-end 0) (match-beginning 0))))))
  (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
  (when (< arg 0)
    (backward-prefix-chars)
    ;; XEmacs: evil hack! Skip back over #[sr] so that structures and raw
    ;; strings are read properly.  the current cheesified syntax tables just
    ;; aren't up to this.
    (let* ((diff (- (point) (point-min)))
	   (subject (buffer-substring (- (point) (min diff 3))
				      (1+ (point))))
	   (matched (string-match "#s(\\|#r[uU]?\"\\|#p\\[" subject)))
      (if matched
	(goto-char (1+ (- (point) (- (length subject) matched))))))))

(defun backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -N means
move forward across N balanced expressions."
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (forward-sexp (- (or arg 1))))

(defun mark-sexp (&optional arg)
  "Set mark ARG sexps from point.
The place mark goes is the same place \\[forward-sexp] would
move to with the same argument.
Repeat this command to mark more sexps in the same direction."
  (interactive "p")
  (mark-something 'mark-sexp 'forward-sexp (or arg 1)))

(defun forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
With argument, do it that many times.
Negative arg -N means move backward across N groups of parentheses."
  ;; XEmacs change
  (interactive "_p")
  (goto-char (or (scan-lists (point) (or arg 1) 0) (buffer-end (or arg 1)))))

(defun backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
With argument, do it that many times.
Negative arg -N means move forward across N groups of parentheses."
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (forward-list (- (or arg 1))))

(defun down-list (&optional arg)
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level."
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (or arg (setq arg 1))
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc -1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun backward-up-list (&optional arg)
  "Move backward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  (interactive "_p")
  (up-list (- (or arg 1))))

(defun up-list (&optional arg)
  "Move forward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot.
In Lisp programs, an argument is required."
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (or arg (setq arg 1))
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc 1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun kill-sexp (&optional arg)
  "Kill the sexp (balanced expression) following the cursor.
With argument, kill that many sexps after the cursor.
Negative arg -N means kill N sexps before the cursor."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (kill-region opoint (point))))

(defun backward-kill-sexp (&optional arg)
  "Kill the sexp (balanced expression) preceding the cursor.
With argument, kill that many sexps before the cursor.
Negative arg -N means kill N sexps after the cursor."
  (interactive "p")
  (kill-sexp (- (or arg 1))))

;; XEmacs change (optional buffer parameter)
(defun buffer-end (arg &optional buffer)
  "Return `point-max' of BUFFER if ARG is > 0; return `point-min' otherwise.
BUFFER defaults to the current buffer if omitted."
  (if (> arg 0) (point-max buffer) (point-min buffer)))


;; derived stuff from GNU Emacs
(defvar beginning-of-defun-function nil
  "If non-nil, this function will be called by `beginning-of-defun-raw'.
It will be called with one argument, which is a repetition count.
It provides an alternative algorithm to find the beginning of the current
defun instead of using the standard one implemented by `beginning-of-defun'.
See also `defun-prompt-regexp' for minor tweaks.")
(make-variable-buffer-local 'beginning-of-defun-function)

(defvar end-of-defun-function nil
  "If non-nil, this function will be called by `end-of-defun'.
It will be called with no arguments.  \(Repetition is implemented in
`end-of-defun' by calling this function that many times.)
This function provides an alternative algorithm to find the end
of the current defun instead of using the standard one implemented by
`end-of-defun'.
")
(make-variable-buffer-local 'end-of-defun-function)

(defun beginning-of-defun (&optional count)
  "Move backward to the beginning of the current defun COUNT times.
COUNT defaults to 1.  COUNT < 0 means move forward to COUNTth following
beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

In the default implementation provided by `beginning-of-defun-raw',
a defun starts at a char with open-parenthesis syntax at the beginning
of a line.  If `defun-prompt-regexp' is non-nil, then a string which
matches that regexp may precede the open-parenthesis.  Alternatively,
if `beginning-of-defun-function' is non-nil, that function is called,
and none of the default processing is done.

If the beginning of defun function returns t, point moves to the
beginning of the line containing the beginning of defun."
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (and (beginning-of-defun-raw count)
       (progn (beginning-of-line) t)))

(defun beginning-of-defun-raw (&optional count)
  "Move point to the character that starts a defun.
This is identical to beginning-of-defun, except that point does not move
to the beginning of the line when `defun-prompt-regexp' is non-nil."
  (interactive "p")
  (unless count (setq count 1))
  (if beginning-of-defun-function
      (funcall beginning-of-defun-function count)
    (and (< count 0) (not (eobp)) (forward-char 1))
    (and
     (re-search-backward (if defun-prompt-regexp
			     (concat "^\\s(\\|"
				     "\\(" defun-prompt-regexp "\\)\\s(")
			   "^\\s(")
			 nil 'move count)
     (progn (goto-char (1- (match-end 0)))) t)))

(defun end-of-defun (&optional count)
  "Move forward to next end of defun COUNT times.
COUNT defaults to 1.  Negative COUNT means move back to COUNT-th preceding
end of defun.

In the default implementation, the end of a defun is the end of the
s-expression started at the character identified by `beginning-of-defun'.

If `end-of-defun-function' is non-nil, none of the default processing is
done.  For COUNT < 1, `end-of-defun-function' is called that many times.
If COUNT < 1, nothing is done.  \(This is a bug.)"
  ;; XEmacs change (for zmacs regions)
  (interactive "_p")
  (if (or (null count) (= count 0)) (setq count 1))
  (if end-of-defun-function
      (if (> count 0)
	  (dotimes (i count)
	    (funcall end-of-defun-function)))
  (let ((first t))
    (while (and (> count 0) (< (point) (point-max)))
      (let ((pos (point))) ; XEmacs -- remove unused npos.
	(while (progn
		(if (and first
			 (progn
			  (end-of-line 1)
			  (beginning-of-defun-raw 1)))
		    nil
		  (or (bobp) (backward-char 1))
		  (beginning-of-defun-raw -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "\\s<\\|\n")
		    (forward-line 1))
		(<= (point) pos))))
      (setq count (1- count)))
    (while (< count 0)
      (let ((pos (point)))
	(beginning-of-defun-raw 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (beginning-of-defun-raw 2)
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "\\s<\\|\n")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq count (1+ count))))))

(defun mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point) nil t)
  (beginning-of-defun)
  (re-search-backward "^\n" (- (point) 1) t))

(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point."
  (interactive)
  (save-excursion
    (widen)
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (narrow-to-region (point) end))))

(defun insert-parentheses (arg)
  "Enclose following ARG sexps in parentheses.  Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `()' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters."
  (interactive "P")
  (if arg (setq arg (prefix-numeric-value arg))
    (setq arg 0))
  (cond ((> arg 0) (skip-chars-forward " \t"))
	((< arg 0) (forward-sexp arg) (setq arg (- arg))))
  (and parens-require-spaces
       (not (bobp))
       (memq (char-syntax (char-before (point))) '(?w ?_ ?\) ))
       (insert " "))
  (insert ?\()
  (save-excursion
    (or (eq arg 0) (forward-sexp arg))
    (insert ?\))
    (and parens-require-spaces
	 (not (eobp))
	 (memq (char-syntax (char-after (point))) '(?w ?_ ?\( ))
	 (insert " "))))

(defun move-past-close-and-reindent ()
  "Move past next `)', delete indentation before it, then indent after it."
  (interactive)
  (up-list 1)
  (backward-char 1)
  (while (save-excursion		; this is my contribution
	   (let ((before-paren (point)))
	     (back-to-indentation)
	     (= (point) before-paren)))
    (delete-indentation))
  (forward-char 1)
  (newline-and-indent))

(defun lisp-complete-symbol ()
  "Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    ;; XEmacs change
		    (if emacs-lisp-mode-syntax-table
			(set-syntax-table emacs-lisp-mode-syntax-table))
		    (backward-sexp 1)
		    (while (eq (char-syntax (char-after (point))) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    ;; XEmacs change
	    #'(lambda (sym)
		(or (boundp sym) (fboundp sym)
		    (symbol-plist sym)))))
	 (completion (try-completion pattern obarray predicate)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern obarray predicate))
		 ;FSFmacs crock unnecessary in XEmacs
		 ;see minibuf.el
		 ;(completion-fixup-function
		 ; (function (lambda () (if (save-excursion
		 ;		(goto-char (max (point-min)
		 ;				(- (point) 4)))
		 ;		(looking-at " <f>"))
		 ;	      (forward-char -4))))
		 )
	     (or (eq predicate 'fboundp)
		 (let (new)
		   (while list
		     (setq new (cons (if (fboundp (intern (car list)))
					 (list (car list) " <f>")
				       (car list))
				     new))
		     (setq list (cdr list)))
		   (setq list (nreverse new))))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

;;; lisp.el ends here
