;;; paragraphs.el --- paragraph and sentence parsing.

;; Copyright (C) 1985, 86, 87, 91, 94, 95, 97 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp, dumped

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; This package provides the paragraph-oriented commands documented in the
;; XEmacs Reference Manual.

;; 06/11/1997 - Use char-(after|before) instead of
;;  (following|preceding)-char. -slb

;;; Code:

(defvar use-hard-newlines nil
    "Non-nil means to distinguish hard and soft newlines.
When this is non-nil, the functions `newline' and `open-line' add the
text-property `hard' to newlines that they insert.  Also, a line is
only considered as a candidate to match `paragraph-start' or
`paragraph-separate' if it follows a hard newline.  Newlines not
marked hard are called \"soft\", and are always internal to
paragraphs.  The fill functions always insert soft newlines.

Each buffer has its own value of this variable.")
(make-variable-buffer-local 'use-hard-newlines)

(defun use-hard-newlines (&optional arg insert)
  "Minor mode to distinguish hard and soft newlines.
When active, the functions `newline' and `open-line' add the
text-property `hard' to newlines that they insert, and a line is
only considered as a candidate to match `paragraph-start' or
`paragraph-separate' if it follows a hard newline.

Prefix argument says to turn mode on if positive, off if negative.
When the mode is turned on, if there are newlines in the buffer but no hard
newlines, ask the user whether to mark as hard any newlines preceding a 
`paragraph-start' line.  From a program, second arg INSERT specifies whether
to do this; it can be `never' to change nothing, t or `always' to force
marking, `guess' to try to do the right thing with no questions, nil 
or anything else to ask the user.

Newlines not marked hard are called \"soft\", and are always internal
to paragraphs.  The fill functions insert and delete only soft newlines."
  (interactive (list current-prefix-arg nil))
  (if (or (<= (prefix-numeric-value arg) 0)
	  (and use-hard-newlines (null arg)))
      ;; Turn mode off
      (setq use-hard-newlines nil)
    ;; Turn mode on
    ;; Intuit hard newlines --
    ;;   mark as hard any newlines preceding a paragraph-start line.
    (if (or (eq insert t) (eq insert 'always)
	    (and (not (eq 'never insert))
		 (not use-hard-newlines)
		 (not (text-property-any (point-min) (point-max) 'hard t))
		 (save-excursion
		   (goto-char (point-min))
		   (search-forward "\n" nil t))
		 (or (eq insert 'guess)
		     (y-or-n-p "Make newlines between paragraphs hard? "))))
	(save-excursion
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (let ((pos (point)))
	      (move-to-left-margin)
	      (if (looking-at paragraph-start)
		  (progn
		    (set-hard-newline-properties (1- pos) pos)
		    ;; If paragraph-separate, newline after it is hard too.
		    (if (looking-at paragraph-separate)
			(progn
			  (end-of-line)
			  (if (not (eobp))
			      (set-hard-newline-properties
			       (point) (1+ (point))))))))))))
    (setq use-hard-newlines t)))

(defconst paragraph-start "[ \t\n\f]"
  "*Regexp for beginning of a line that starts OR separates paragraphs.
This regexp should match lines that separate paragraphs
and should also match lines that start a paragraph
\(and are part of that paragraph).

This is matched against the text at the left margin, which is not necessarily
the beginning of the line, so it should never use \"^\" as an anchor.  This
ensures that the paragraph functions will work equally well within a region
of text indented by a margin setting.

The variable `paragraph-separate' specifies how to distinguish
lines that start paragraphs from lines that separate them.

If the variable `use-hard-newlines' is non-nil, then only lines following a
hard newline are considered to match.")

;; paragraph-start requires a hard newline, but paragraph-separate does not:
;; It is assumed that paragraph-separate is distinctive enough to be believed
;; whenever it occurs, while it is reasonable to set paragraph-start to
;; something very minimal, even including "." (which makes every hard newline
;; start a new paragraph).

(defconst paragraph-separate "[ \t\f]*$"
  "*Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change `paragraph-start' also.

A line matching this is not part of any paragraph.

This is matched against the text at the left margin, which is not necessarily
the beginning of the line, so it should not use \"^\" as an anchor.  This
ensures that the paragraph functions will work equally within a region of
text indented by a margin setting.")

(defconst sentence-end "[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*"
  "*Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless.

In order to be recognized as the end of a sentence, the ending period,
question mark, or exclamation point must be followed by two spaces,
unless it's inside some sort of quotes or parenthesis.")

(defconst page-delimiter "^\014"
  "*Regexp describing line-beginnings that separate pages.")

(defvar paragraph-ignore-fill-prefix nil
  "Non-nil means the paragraph commands are not affected by `fill-prefix'.
This is desirable in modes where blank lines are the paragraph delimiters.")

(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (let* ((fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (not paragraph-ignore-fill-prefix)
	       (regexp-quote fill-prefix)))
	 ;; Remove ^ from paragraph-start and paragraph-sep if they are there.
	 ;; These regexps shouldn't be anchored, because we look for them
	 ;; starting at the left-margin.  This allows paragraph commands to
	 ;; work normally with indented text.
	 ;; This hack will not find problem cases like "whatever\\|^something".
	 (paragraph-start (if (and (not (equal "" paragraph-start))
				   (equal ?^ (aref paragraph-start 0)))
			      (substring paragraph-start 1)
			    paragraph-start))
	 (paragraph-separate (if (and (not (equal "" paragraph-start))
				      (equal ?^ (aref paragraph-separate 0)))
			      (substring paragraph-separate 1)
			    paragraph-separate))
	 (paragraph-separate
	  (if fill-prefix-regexp
	      (concat paragraph-separate "\\|"
		      fill-prefix-regexp "[ \t]*$")
	    paragraph-separate))
	 ;; This is used for searching.
	 (sp-paragraph-start (concat "^[ \t]*\\(" paragraph-start "\\)"))
	 start)
    (while (and (< arg 0) (not (bobp)))
      (if (and (not (looking-at paragraph-separate))
	       (re-search-backward "^\n" (max (1- (point)) (point-min)) t)
	       (looking-at paragraph-separate))
	  nil
	(setq start (point))
	;; Move back over paragraph-separating lines.
	(backward-char 1) (beginning-of-line)
	(while (and (not (bobp))
		    (progn (move-to-left-margin)
			   (looking-at paragraph-separate)))
	  (forward-line -1)) 
	(if (bobp)
	    nil
	  ;; Go to end of the previous (non-separating) line.
	  (end-of-line)
	  ;; Search back for line that starts or separates paragraphs.
	  (if (if fill-prefix-regexp
		  ;; There is a fill prefix; it overrides paragraph-start.
		  (let (multiple-lines)
		    (while (and (progn (beginning-of-line) (not (bobp)))
				(progn (move-to-left-margin)
				       (not (looking-at paragraph-separate)))
				(looking-at fill-prefix-regexp))
		      (if (not (= (point) start))
			  (setq multiple-lines t))
		      (forward-line -1))
		    (move-to-left-margin)
		    ;; Don't move back over a line before the paragraph
		    ;; which doesn't start with fill-prefix
		    ;; unless that is the only line we've moved over.
		    (and (not (looking-at fill-prefix-regexp))
			 multiple-lines
			 (forward-line 1))
		    (not (bobp)))
		(while (and (re-search-backward sp-paragraph-start nil 1)
			    ;; Found a candidate, but need to check if it is a
			    ;; REAL paragraph-start.
			    (not (bobp))
			    (progn (setq start (point))
				   (move-to-left-margin)
				   (not (looking-at paragraph-separate)))
			    (or (not (looking-at paragraph-start))
				(and use-hard-newlines
				     (not (get-text-property (1- start)
							     'hard)))))
		  (goto-char start))
		(> (point) (point-min)))
	      ;; Found one.
	      (progn
		;; Move forward over paragraph separators.
		;; We know this cannot reach the place we started
		;; because we know we moved back over a non-separator.
		(while (and (not (eobp))
			    (progn (move-to-left-margin)
				   (looking-at paragraph-separate)))
		  (forward-line 1))
		;; If line before paragraph is just margin, back up to there.
		(end-of-line 0)
		(if (> (current-column) (current-left-margin))
		    (forward-char 1)
		  (skip-chars-backward " \t")
		  (if (not (bolp))
		      (forward-line 1))))
	    ;; No starter or separator line => use buffer beg.
	    (goto-char (point-min)))))
      (setq arg (1+ arg)))
    (while (and (> arg 0) (not (eobp)))
      ;; Move forward over separator lines, and one more line.
      (while (prog1 (and (not (eobp))
			 (progn (move-to-left-margin) (not (eobp)))
			 (looking-at paragraph-separate))
	       (forward-line 1)))
      (if fill-prefix-regexp
	  ;; There is a fill prefix; it overrides paragraph-start.
	  (while (and (not (eobp))
		      (progn (move-to-left-margin) (not (eobp)))
		      (not (looking-at paragraph-separate))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(while (and (re-search-forward sp-paragraph-start nil 1)
		    (progn (setq start (match-beginning 0))
			   (goto-char start)
			   (not (eobp)))
		    (progn (move-to-left-margin)
			   (not (looking-at paragraph-separate)))
		    (or (not (looking-at paragraph-start))
			(and use-hard-newlines
			     (not (get-text-property (1- start) 'hard)))))
	  (forward-char 1))
	(if (< (point) (point-max))
	    (goto-char start)))
      (setq arg (1- arg)))))

(defun backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `forward-paragraph' for more information."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (forward-paragraph (- arg)))

(defun mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point."
  (interactive)
  (forward-paragraph 1)
  (push-mark nil t t)
  (backward-paragraph 1))

(defun kill-paragraph (arg)
  "Kill forward to end of paragraph.
With arg N, kill forward to Nth end of paragraph;
negative arg -N means kill backward to Nth start of paragraph."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (forward-paragraph arg) (point))))

(defun backward-kill-paragraph (arg)
  "Kill back to start of paragraph.
With arg N, kill back to Nth start of paragraph;
negative arg -N means kill forward to Nth end of paragraph."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (backward-paragraph arg) (point))))

(defun transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "*p")
  (transpose-subr 'forward-paragraph arg))

(defun start-of-paragraph-text ()
  (let ((opoint (point)) npoint)
    (forward-paragraph -1)
    (setq npoint (point))
    (skip-chars-forward " \t\n")
    ;; If the range of blank lines found spans the original start point,
    ;; try again from the beginning of it.
    ;; Must be careful to avoid infinite loop
    ;; when following a single return at start of buffer.
    (if (and (>= (point) opoint) (< npoint opoint))
	(progn
	  (goto-char npoint)
	  (if (> npoint (point-min))
	      (start-of-paragraph-text))))))

(defun end-of-paragraph-text ()
  (let ((opoint (point)))
    (forward-paragraph 1)
    (if (eq (char-before (point)) ?\n) (backward-char 1))
    (if (<= (point) opoint)
	(progn
	  (forward-char 1)
	  (if (< (point) (point-max))
	      (end-of-paragraph-text))))))

(defun forward-sentence (&optional arg)
  "Move forward to next `sentence-end'.  With argument, repeat.
With negative argument, move backward repeatedly to `sentence-beginning'.

The variable `sentence-end' is a regular expression that matches ends of
sentences.  A paragraph boundary also terminates a sentence."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (while (< arg 0)
    (let ((par-beg (save-excursion (start-of-paragraph-text) (point))))
      (if (re-search-backward (concat sentence-end "[^ \t\n]") par-beg t)
	  (goto-char (1- (match-end 0)))
	(goto-char par-beg)))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
      (if (re-search-forward sentence-end par-end t)
	  (skip-chars-backward " \t\n")
	(goto-char par-end)))
    (setq arg (1- arg))))

(defun backward-sentence (&optional arg)
  "Move backward to start of sentence.  With arg, do it arg times.
See `forward-sentence' for more information."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (forward-sentence (- arg)))

(defun kill-sentence (&optional arg)
  "Kill from point to end of sentence.
With arg, repeat; negative arg -N means kill back to Nth start of sentence."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (forward-sentence arg) (point))))

(defun backward-kill-sentence (&optional arg)
  "Kill back from point to start of sentence.
With arg, repeat, or kill forward to Nth end of sentence if negative arg -N."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (backward-sentence arg) (point))))

(defun mark-end-of-sentence (arg)
  "Put mark at end of sentence.  Arg works as in `forward-sentence'."
  (interactive "p")
  ;; FSF Version:
;  (push-mark
;   (save-excursion
;     (forward-sentence arg)
;     (point))
;   nil t))
  (mark-something 'mark-end-of-sentence 'forward-sentence arg))

(defun mark-end-of-line (arg)
  "Put mark at end of line.  Arg works as in `end-of-line'."
  (interactive "p")
  (mark-something 'mark-end-of-line 'end-of-line arg))


(defun transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "*p")
  (transpose-subr 'forward-sentence arg))

;;; paragraphs.el ends here
