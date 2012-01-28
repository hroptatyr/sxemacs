;;; replace.el --- search and replace commands for SXEmacs.

;; Copyright (C) 1985-7, 1992, 1994, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
;; Keywords: dumped, matching

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

;;; Synched up with: FSF 19.34 [Partially].

;;; Commentary:

;; This file is dumped with SXEmacs.

;; This package supplies the string and regular-expression replace functions
;; documented in the XEmacs Reference Manual.

;; All the gettext calls are for XEmacs I18N3 message catalog support.
;; (This is hopelessly broken and we should remove it. -sb)

;;; Code:

(defvar case-replace t
  "*Non-nil means `query-replace' should preserve case in replacements.
What this means is that `query-replace' will change the case of the
replacement text so that it matches the text that was replaced.
If this variable is nil, the replacement text will be inserted
exactly as it was specified by the user, irrespective of the case
of the text that was replaced.

Note that this flag has no effect if `case-fold-search' is nil,
or if the replacement text has any uppercase letters in it.")

(defvar query-replace-history nil)

(defvar query-replace-interactive nil
  "Non-nil means `query-replace' uses the last search string.
That becomes the \"string to replace\".")

(defvar replace-search-function
  (lambda (str limit)
    (search-forward str limit t))
  "Function used by perform-replace to search forward for a string. It will be
called with two arguments: the string to search for and a limit bounding the
search.")

(defvar replace-re-search-function
  (lambda (regexp limit)
    (re-search-forward regexp limit t))
  "Function used by perform-replace to search forward for a regular
expression. It will be called with two arguments: the regexp to search for and
a limit bounding the search.")

(defun query-replace-read-args (string regexp-flag)
  (let (from to)
    (if query-replace-interactive
	(setq from (car (if regexp-flag regexp-search-ring search-ring)))
      (setq from (read-from-minibuffer (format "%s: " (gettext string))
				       nil nil nil
				       'query-replace-history)))
    (setq to (read-from-minibuffer (format "%s %s with: " (gettext string)
					   from)
				   nil nil nil
				   'query-replace-history))
    (list from to current-prefix-arg)))

;; As per suggestion from Per Abrahamsen, limit replacement to the region
;; if the region is active.
(defun query-replace (from-string to-string &optional delimited)
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.

To customize possible responses, change the \"bindings\" in `query-replace-map'."
  (interactive (query-replace-read-args "Query replace" nil))
  (perform-replace from-string to-string t nil delimited))

(defun query-replace-regexp (regexp to-string &optional delimited)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
 whatever what matched the Nth `\\(...\\)' in REGEXP."
  (interactive (query-replace-read-args "Query replace regexp" t))
  (perform-replace regexp to-string t t delimited))

;;#### Not patently useful
(defun map-query-replace-regexp (regexp to-strings &optional arg)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings, separated
by spaces.  This command works like `query-replace-regexp' except
that each successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

Non-interactively, TO-STRINGS may be a list of replacement strings.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

A prefix argument N says to use each replacement string N times
before rotating to the next."
  (interactive
   (let (from to)
     (setq from (if query-replace-interactive
		    (car regexp-search-ring)
		  (read-from-minibuffer "Map query replace (regexp): "
					nil nil nil
					'query-replace-history)))
     (setq to (read-from-minibuffer
	       (format "Query replace %s with (space-separated strings): "
		       from)
	       nil nil nil
	       'query-replace-history))
     (list from to current-prefix-arg)))
  (let (replacements)
    (if (listp to-strings)
	(setq replacements to-strings)
      (while (/= (length to-strings) 0)
	(if (string-match " " to-strings)
	    (setq replacements
		  (append replacements
			  (list (substring to-strings 0
					   (string-match " " to-strings))))
		  to-strings (substring to-strings
				       (1+ (string-match " " to-strings))))
	  (setq replacements (append replacements (list to-strings))
		to-strings ""))))
    (perform-replace regexp replacements t t nil arg)))

(defun replace-string (from-string to-string &optional delimited)
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward FROM-STRING nil t)
    (replace-match TO-STRING nil t))
which will run faster and will not set the mark or print anything."
  (interactive (query-replace-read-args "Replace string" nil))
  (perform-replace from-string to-string nil nil delimited))

(defun replace-regexp (regexp to-string &optional delimited)
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
 whatever what matched the Nth `\\(...\\)' in REGEXP.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (re-search-forward REGEXP nil t)
    (replace-match TO-STRING nil nil))
which will run faster and will not set the mark or print anything."
  (interactive (query-replace-read-args "Replace regexp" t))
  (perform-replace regexp to-string nil t delimited))


(defvar regexp-history nil
  "History list for some commands that read regular expressions.")

(define-function 'keep-lines 'delete-non-matching-lines)
(defun delete-non-matching-lines (regexp)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point."
  (interactive (list (read-from-minibuffer
		      "Keep lines (containing match for regexp): "
		      nil nil nil 'regexp-history)))
  (with-interactive-search-caps-disable-folding regexp t
    (save-excursion
      (or (bolp) (forward-line 1))
      (let ((start (point)))
	(while (not (eobp))
	  ;; Start is first char not preserved by previous match.
	  (if (not (re-search-forward regexp nil 'move))
	      (delete-region start (point-max))
	    (let ((end (save-excursion (goto-char (match-beginning 0))
				       (beginning-of-line)
				       (point))))
	      ;; Now end is first char preserved by the new match.
	      (if (< start end)
		  (delete-region start end))))
	  (setq start (save-excursion (forward-line 1)
				      (point)))
	  ;; If the match was empty, avoid matching again at same place.
	  (and (not (eobp)) (= (match-beginning 0) (match-end 0))
	       (forward-char 1)))))))

(define-function 'flush-lines 'delete-matching-lines)
(defun delete-matching-lines (regexp)
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
  (interactive (list (read-from-minibuffer
		      "Flush lines (containing match for regexp): "
		      nil nil nil 'regexp-history)))
  (with-interactive-search-caps-disable-folding regexp t
    (save-excursion
      (while (and (not (eobp))
		  (re-search-forward regexp nil t))
	(delete-region (save-excursion (goto-char (match-beginning 0))
				       (beginning-of-line)
				       (point))
		       (progn (forward-line 1) (point)))))))

(define-function 'how-many 'count-matches)
(defun count-matches (regexp)
  "Print number of matches for REGEXP following point."
  (interactive (list (read-from-minibuffer
		      "How many matches for (regexp): "
		      nil nil nil 'regexp-history)))
  (with-interactive-search-caps-disable-folding regexp t
    (let ((count 0) opoint)
      (save-excursion
	(while (and (not (eobp))
		    (progn (setq opoint (point))
			   (re-search-forward regexp nil t)))
	  (if (= opoint (point))
	      (forward-char 1)
	    (setq count (1+ count))))
	(message "%d occurrences" count)))))


(defvar occur-mode-map ())
(if occur-mode-map
    ()
  (setq occur-mode-map (make-sparse-keymap))
  (set-keymap-name occur-mode-map 'occur-mode-map) ; XEmacs
  (define-key occur-mode-map 'button2 'occur-mode-mouse-goto) ; XEmacs
  (define-key occur-mode-map "\C-c\C-c" 'occur-mode-goto-occurrence)
  (define-key occur-mode-map "\C-m" 'occur-mode-goto-occurrence))

(defvar occur-buffer nil)
(defvar occur-nlines nil)
(defvar occur-pos-list nil)

(defun occur-mode ()
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name (gettext "Occur")) ; XEmacs
  (make-local-variable 'occur-buffer)
  (make-local-variable 'occur-nlines)
  (make-local-variable 'occur-pos-list)
  (require 'mode-motion) ; XEmacs
  (setq mode-motion-hook 'mode-motion-highlight-line) ; XEmacs
  (run-hooks 'occur-mode-hook))

;; FSF Version of next function:
;  (let (buffer pos)
;    (save-excursion
;      (set-buffer (window-buffer (posn-window (event-end event))))
;      (save-excursion
;       (goto-char (posn-point (event-end event)))
;       (setq pos (occur-mode-find-occurrence))
;       (setq buffer occur-buffer)))
;    (pop-to-buffer buffer)
;    (goto-char (marker-position pos))))

(defun occur-mode-mouse-goto (event)
  "Go to the occurrence highlighted by mouse.
This function should be bound to a mouse key in the `*Occur*' buffer."
  (interactive "e")
  (let ((window-save (selected-window))
	(frame-save (selected-frame)))
    ;; preserve the window/frame setup
    (unwind-protect
	(progn
	  (mouse-set-point event)
	  (occur-mode-goto-occurrence))
      (select-frame frame-save)
      (select-window window-save))))

;; Called occur-mode-find-occurrence in FSF
(defun occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (if (or (null occur-buffer)
	  (null (buffer-name occur-buffer)))
      (progn
	(setq occur-buffer nil
	      occur-pos-list nil)
	(error "Buffer in which occurrences were found is deleted")))
  (let* ((line-count
	  (count-lines (point-min)
		       (save-excursion
			 (beginning-of-line)
			 (point))))
	 (occur-number (save-excursion
			 (beginning-of-line)
			 (/ (1- line-count)
			    (cond ((< occur-nlines 0)
				   (- 2 occur-nlines))
				  ((> occur-nlines 0)
				   (+ 2 (* 2 occur-nlines)))
				  (t 1)))))
	 (pos (nth occur-number occur-pos-list))
	 ;; removed t arg from Bob Weiner, 10/6/95
	 (window (get-buffer-window occur-buffer))
	 (occur-source-buffer occur-buffer))
    (if (< line-count 1)
	(error "No occurrence on this line"))
    (or pos
	(error "No occurrence on this line"))
    ;; XEmacs: don't raise window unless it isn't visible
    ;; allow for the possibility that the occur buffer is on another frame
    (or (and window
	     (window-live-p window)
	     (frame-visible-p (window-frame window))
	     (set-buffer occur-source-buffer))
	(and (pop-to-buffer occur-source-buffer)
	     (setq window (get-buffer-window occur-source-buffer))))
    (goto-char pos)
    (set-window-point window pos)))


(defvar list-matching-lines-default-context-lines 0
  "*Default number of context lines to include around a `list-matching-lines'
match.  A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after.")

;; XEmacs addition
;;; Damn you Jamie, this is utter trash.
(defvar list-matching-lines-whole-buffer t
  "If t, occur operates on whole buffer, otherwise occur starts from point.
default is t.")

(define-function 'occur 'list-matching-lines)
(defun list-matching-lines (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

If variable `list-matching-lines-whole-buffer' is non-nil, the entire
buffer is searched, otherwise search begins at point.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive
   ;; XEmacs change
   (list (let* ((default (or (symbol-near-point)
			     (and regexp-history
				  (car regexp-history))))
		(minibuffer-history-minimum-string-length 0)
		(input
		 (if default
		     ;; rewritten for I18N3 snarfing
		     (read-from-minibuffer
		      (format "List lines matching regexp (default `%s'): "
			      default) nil nil nil 'regexp-history nil
			      default)
		   (read-from-minibuffer
		    "List lines matching regexp: "
		    nil nil nil
		    'regexp-history))))
	   (if (and (equal input "") default)
	       (progn
		 (setq input default)
		 (setcar regexp-history default)))
	   ;; clear extra entries
	   (setcdr regexp-history (delete (car regexp-history)
					  (cdr regexp-history)))
	   input)
	 current-prefix-arg))
  (if (equal regexp "")
      (error "Must pass non-empty regexp to `list-matching-lines'"))
  (setq nlines (if nlines (prefix-numeric-value nlines)
		 list-matching-lines-default-context-lines))
  (let ((first t)
	(dir default-directory)
	(buffer (current-buffer))
	(linenum 1)
	(prevpos (point-min))
	;; The rest of this function is very different from FSF.
	;; Presumably that's due to Jamie's misfeature
	(final-context-start (make-marker)))
    (if (not list-matching-lines-whole-buffer)
	(save-excursion
	  (beginning-of-line)
	  (setq linenum (1+ (count-lines (point-min) (point))))
	  (setq prevpos (point))))
    (with-output-to-temp-buffer "*Occur*"
      (save-excursion
	(set-buffer standard-output)
	(setq default-directory dir)
	;; We will insert the number of lines, and "lines", later.
	;; #### Needs fixing for I18N3
	(let ((print-escape-newlines t))
	  (insert (format " matching %s in buffer %s.\n"
			  regexp (buffer-name buffer))))
	(occur-mode)
	(setq occur-buffer buffer)
	(setq occur-nlines nlines)
	(setq occur-pos-list ()))
      (if (eq buffer standard-output)
	  (goto-char (point-max)))
      (with-interactive-search-caps-disable-folding regexp t
	(save-excursion
	  (if list-matching-lines-whole-buffer
	      (beginning-of-buffer))
	  (message "Searching for %s ..." regexp)
	  ;; Find next match, but give up if prev match was at end of buffer.
	  (while (and (not (= prevpos (point-max)))
		      (re-search-forward regexp nil t))
	    (goto-char (match-beginning 0))
	    (beginning-of-line)
	    (save-match-data
	      (setq linenum (+ linenum (count-lines prevpos (point)))))
	    (setq prevpos (point))
	    (goto-char (match-end 0))
	    (let* ((start (save-excursion
			    (goto-char (match-beginning 0))
			    (forward-line (if (< nlines 0) nlines (- nlines)))
			    (point)))
		   (end (save-excursion
			  (goto-char (match-end 0))
			  (if (> nlines 0)
			      (forward-line (1+ nlines))
			    (forward-line 1))
			  (point)))
		   (tag (format "%5d" linenum))
		   (empty (make-string (length tag) ?\ ))
		   tem)
	      (save-excursion
		(setq tem (make-marker))
		(set-marker tem (point))
		(set-buffer standard-output)
		(setq occur-pos-list (cons tem occur-pos-list))
		(or first (zerop nlines)
		    (insert "--------\n"))
		(setq first nil)
		(insert-buffer-substring buffer start end)
		(set-marker final-context-start
			    (- (point) (- end (match-end 0))))
		(backward-char (- end start))
		(setq tem (if (< nlines 0) (- nlines) nlines))
		(while (> tem 0)
		  (insert empty ?:)
		  (forward-line 1)
		  (setq tem (1- tem)))
		(let ((this-linenum linenum))
		  (while (< (point) final-context-start)
		    (if (null tag)
			(setq tag (format "%5d" this-linenum)))
		    (insert tag ?:)
		    ;; FSFmacs --
		    ;; we handle this using mode-motion-highlight-line, above.
		    ;;		  (put-text-property (save-excursion
		    ;;				       (beginning-of-line)
		    ;;				       (point))
		    ;;				     (save-excursion
		    ;;				       (end-of-line)
		    ;;				       (point))
		    ;;				     'mouse-face 'highlight)
		    (forward-line 1)
		    (setq tag nil)
		    (setq this-linenum (1+ this-linenum)))
		  (while (<= (point) final-context-start)
		    (insert empty ?:)
		    (forward-line 1)
		    (setq this-linenum (1+ this-linenum))))
		(while (< tem nlines)
		  (insert empty ?:)
		  (forward-line 1)
		  (setq tem (1+ tem)))
		(goto-char (point-max)))
	      (forward-line 1)))
	  (set-buffer standard-output)
	  ;; Put positions in increasing order to go with buffer.
	  (setq occur-pos-list (nreverse occur-pos-list))
	  (goto-char (point-min))
	  (if (= (length occur-pos-list) 1)
	      (insert "1 line")
	    (insert (format "%d lines" (length occur-pos-list))))
	  (if (interactive-p)
	      (message "%d matching lines." (length occur-pos-list))))))))

;; It would be nice to use \\[...], but there is no reasonable way
;; to make that display both SPC and Y.
(defconst query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the frame, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match."

  "Help message while in query-replace")

(defvar query-replace-map nil
  "Keymap that defines the responses to questions in `query-replace'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `act-and-show',
`exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
`automatic', `backup', `exit-prefix', and `help'.")

;; Why does it seem that ever file has a different method of doing this?
(if query-replace-map
    nil
    (let ((map (make-sparse-keymap)))
      (set-keymap-name map 'query-replace-map)
      (define-key map " " 'act)
      (define-key map "\d" 'skip)
      (define-key map [delete] 'skip)
      (define-key map [backspace] 'skip)
      (define-key map "y" 'act)
      (define-key map "n" 'skip)
      (define-key map "Y" 'act)
      (define-key map "N" 'skip)
      (define-key map "," 'act-and-show)
      (define-key map [escape] 'exit)
      (define-key map "q" 'exit)
      (define-key map [return] 'exit)
      (define-key map "." 'act-and-exit)
      (define-key map "\C-r" 'edit)
      (define-key map "\C-w" 'delete-and-edit)
      (define-key map "\C-l" 'recenter)
      (define-key map "!" 'automatic)
      (define-key map "^" 'backup)
      (define-key map [(control h)] 'help)      ;; XEmacs change
      (define-key map [f1] 'help)
      (define-key map [help] 'help)
      (define-key map "?" 'help)
      (define-key map "\C-g" 'quit)
      (define-key map "\C-]" 'quit)
      ;FSFmacs (define-key map "\e" 'exit-prefix)
      (define-key map [escape] 'exit-prefix)

      (setq query-replace-map map)))

;; isearch-mode is dumped, so don't autoload.
;(autoload 'isearch-highlight "isearch")

;; XEmacs
(defun perform-replace-next-event (event)
  (if search-highlight
      (let ((aborted t))
	(unwind-protect
	    (progn
	      (if (match-beginning 0)
		  (isearch-highlight (match-beginning 0) (match-end 0)))
	      (next-command-event event)
	      (setq aborted nil))
	  (isearch-dehighlight aborted)))
    (next-command-event event)))

(defun perform-replace (from-string replacements
			query-flag regexp-flag delimited-flag
			&optional repeat-count map)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:
  (while (re-search-forward \"foo[ \t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))
which will run faster and probably do exactly what you want.
When searching for a match, this function uses
`replace-search-function' and `replace-re-search-function'."
  (or map (setq map query-replace-map))
  (let* ((event (make-event))
	 (nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	 (literal (not regexp-flag))
	 (search-function (if regexp-flag
			      replace-re-search-function
			    replace-search-function))
	 (search-string from-string)
	 (real-match-data nil)		; the match data for the current match
	 (next-replacement nil)
	 (replacement-index 0)
	 (keep-going t)
	 (stack nil)
	 (next-rotate-count 0)
	 (replace-count 0)
	 (lastrepl nil)			;Position after last match considered.
	 ;; If non-nil, it is marker saying where in the buffer to
	 ;; stop.
	 (limit nil)
	 (match-again t)
	 ;; XEmacs addition
	 (qr-case-fold-search
	  (if (and case-fold-search search-caps-disable-folding)
	      (no-upper-case-p search-string regexp-flag)
	    case-fold-search))
	 (message
	  (if query-flag
	      (substitute-command-keys
	       "Query replacing %s with %s: (\\<query-replace-map>\\[help] for help) "))))
    ;; If the region is active, operate on region.
    (when (region-active-p)
      ;; Original Per Abrahamsen's code simply narrowed the region,
      ;; thus providing a visual indication of the search boundary.
      ;; Stallman, on the other hand, handles it like this.
      (setq limit (copy-marker (region-end)))
      (goto-char (region-beginning))
      (zmacs-deactivate-region))
    (if (stringp replacements)
	(setq next-replacement replacements)
      (or repeat-count (setq repeat-count 1)))
    (if delimited-flag
	(setq search-function replace-re-search-function
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (not (eobp))
		    (or (null limit) (< (point) limit))
		    (let ((case-fold-search qr-case-fold-search))
		      (funcall search-function search-string limit))
		    ;; If the search string matches immediately after
		    ;; the previous match, but it did not match there
		    ;; before the replacement was done, ignore the match.
		    (if (or (eq lastrepl (point))
			    (and regexp-flag
				 (eq lastrepl (match-beginning 0))
				 (not match-again)))
			(if (or (eobp)
				(and limit (>= (point) limit)))
			    nil
			  ;; Don't replace the null string
			  ;; right after end of previous replacement.
			  (forward-char 1)
			  (let ((case-fold-search qr-case-fold-search))
			    (funcall search-function search-string limit)))
		      t))

	  ;; Save the data associated with the real match.
	  (setq real-match-data (match-data))

	  ;; Before we make the replacement, decide whether the search string
	  ;; can match again just after this match.
	  (if regexp-flag
	      (progn
		(setq match-again (looking-at search-string))
		;; XEmacs addition
		(store-match-data real-match-data)))
	  ;; If time for a change, advance to next replacement string.
	  (if (and (listp replacements)
		   (= next-rotate-count replace-count))
	      (progn
		(setq next-rotate-count
		      (+ next-rotate-count repeat-count))
		(setq next-replacement (nth replacement-index replacements))
		(setq replacement-index (% (1+ replacement-index) (length replacements)))))
	  (if (not query-flag)
	      (progn
		(store-match-data real-match-data)
		(replace-match next-replacement nocasify literal)
		(setq replace-count (1+ replace-count)))
	    (undo-boundary)
	    (let ((help-form
		   '(concat (format "Query replacing %s%s with %s.\n\n"
				    (if regexp-flag (gettext "regexp ") "")
				    from-string next-replacement)
			    (substitute-command-keys query-replace-help)))
		  done replaced def)
	      ;; Loop reading commands until one of them sets done,
	      ;; which means it has finished handling this occurrence.
	      (while (not done)
		;; Don't fill up the message log
		;; with a bunch of identical messages.
		;; XEmacs change
		(display-message 'prompt
				 (format message from-string next-replacement))
		(perform-replace-next-event event)
		(setq def (lookup-key map (vector event)))
		;; Restore the match data while we process the command.
		(store-match-data real-match-data)
		(cond ((eq def 'help)
		       (with-output-to-temp-buffer (gettext "*Help*")
			 (princ (concat
				 (format "Query replacing %s%s with %s.\n\n"
					 (if regexp-flag "regexp " "")
					 from-string next-replacement)
				 (substitute-command-keys
				  query-replace-help)))
			 (save-excursion
			   (set-buffer standard-output)
			   (help-mode))))
		      ((eq def 'exit)
		       (setq keep-going nil)
		       (setq done t))
		      ((eq def 'backup)
		       (if stack
			   (let ((elt (car stack)))
			     (goto-char (car elt))
			     (setq replaced (eq t (cdr elt)))
			     (or replaced
				 (store-match-data (cdr elt)))
			     (setq stack (cdr stack)))
			 (message "No previous match")
			 (ding 'no-terminate)
			 (sit-for 1)))
		      ((eq def 'act)
		       (or replaced
			   (replace-match next-replacement nocasify literal))
		       (setq done t replaced t))
		      ((eq def 'act-and-exit)
		       (or replaced
			   (replace-match next-replacement nocasify literal))
		       (setq keep-going nil)
		       (setq done t replaced t))
		      ((eq def 'act-and-show)
		       (if (not replaced)
			   (progn
			     (replace-match next-replacement nocasify literal)
			     (store-match-data nil)
			     (setq replaced t))))
		      ((eq def 'automatic)
		       (or replaced
			   (replace-match next-replacement nocasify literal))
		       (setq done t query-flag nil replaced t))
		      ((eq def 'skip)
		       (setq done t))
		      ((eq def 'recenter)
		       (recenter nil))
		      ((eq def 'edit)
		       (store-match-data
			(prog1 (match-data)
			  (save-excursion (recursive-edit))))
		       ;; Before we make the replacement,
		       ;; decide whether the search string
		       ;; can match again just after this match.
		       (if regexp-flag
			   (setq match-again (looking-at search-string))))
		      ((eq def 'delete-and-edit)
		       (delete-region (match-beginning 0) (match-end 0))
		       (store-match-data (prog1 (match-data)
					   (save-excursion (recursive-edit))))
		       (setq replaced t))
		      ;; Note: we do not need to treat `exit-prefix'
		      ;; specially here, since we reread
		      ;; any unrecognized character.
		      (t
		       (setq this-command 'mode-exited)
		       (setq keep-going nil)
		       (setq unread-command-events
			     (cons event unread-command-events))
		       (setq done t))))
	      ;; Record previous position for ^ when we move on.
	      ;; Change markers to numbers in the match data
	      ;; since lots of markers slow down editing.
	      (setq stack
		    (cons (cons (point)
				(or replaced
				    (match-data t)))
			  stack))
	      (if replaced (setq replace-count (1+ replace-count)))))
	  (setq lastrepl (point)))
      ;; Useless in XEmacs.  We handle (de)highlighting through
      ;; perform-replace-next-event.
      ;(replace-dehighlight)
      )
    (or unread-command-events
	(message "Replaced %d occurrence%s"
		 replace-count
		 (if (= replace-count 1) "" "s")))
    (and keep-going stack)))

;; FSFmacs code: someone should port it.

;(defvar query-replace-highlight nil
;  "*Non-nil means to highlight words during query replacement.")

;(defvar replace-overlay nil)

;(defun replace-dehighlight ()
;  (and replace-overlay
;       (progn
;	 (delete-overlay replace-overlay)
;	 (setq replace-overlay nil))))

;(defun replace-highlight (start end)
;  (and query-replace-highlight
;       (progn
;	 (or replace-overlay
;	     (progn
;	       (setq replace-overlay (make-overlay start end))
;	       (overlay-put replace-overlay 'face
;			    (if (internal-find-face 'query-replace)
;				'query-replace 'region))))
;	 (move-overlay replace-overlay start end (current-buffer)))))

(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defmacro save-match-data (&rest body)
  "Execute BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
	  (list 'unwind-protect
		(cons 'progn body)
		(list 'store-match-data original)))))

(provide 'replace)

;;; replace.el ends here
