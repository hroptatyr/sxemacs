;;; page.el --- page motion commands for emacs.

;; Copyright (C) 1985, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This code provides the page-oriented movement and selection commands
;; documented in the XEmacs Reference Manual.

;;; Code:

(defun forward-page (&optional count)
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp
`page-delimiter'."
  (interactive "_p") ; XEmacs
  (or count (setq count 1))
  (while (and (> count 0) (not (eobp)))
    ;; In case the page-delimiter matches the null string,
    ;; don't find a match without moving.
    (if (bolp) (forward-char 1))
    (if (re-search-forward page-delimiter nil t)
	nil
      (goto-char (point-max)))
    (setq count (1- count)))
  (while (and (< count 0) (not (bobp)))
    ;; In case the page-delimiter matches the null string,
    ;; don't find a match without moving.
    (and (save-excursion (re-search-backward page-delimiter nil t))
	 (= (match-end 0) (point))
	 (goto-char (match-beginning 0)))
    (backward-char 1)
    (if (re-search-backward page-delimiter nil t)
	;; We found one--move to the end of it.
	(goto-char (match-end 0))
      ;; We found nothing--go to beg of buffer.
      (goto-char (point-min)))
    (setq count (1+ count))))

(defun backward-page (&optional count)
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any line whose beginning matches the regexp
`page-delimiter'."
  (interactive "_p") ; XEmacs
  (or count (setq count 1))
  (forward-page (- count)))

(defun mark-page (&optional arg)
  "Put mark at end of page, point at beginning.
A numeric arg specifies to move forward or backward by that many pages,
thus marking a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (if (> arg 0)
      (forward-page arg)
    (if (< arg 0)
        (forward-page (1- arg))))
  (forward-page)
  (push-mark nil t t)
  (forward-page -1))

(defun narrow-to-page (&optional arg)
  "Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (forward-page (1- arg))))
    ;; Find the end of the page.
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    (if (save-excursion
	  (goto-char (match-beginning 0)) ; was (beginning-of-line)
	  (looking-at page-delimiter))
	(beginning-of-line))
    (narrow-to-region (point)
		      (progn
			;; Find the top of the page.
			(forward-page -1)
			;; If we found beginning of buffer, stay there.
			;; If extra text follows page delimiter on same line,
			;; include it.
			;; Otherwise, show text starting with following line.
			(if (and (eolp) (not (bobp)))
			    (forward-line 1))
			(point)))))
(put 'narrow-to-page 'disabled t)

(defun count-lines-page ()
  "Report number of lines on current page, and how many are before or after point."
  (interactive "_") ; XEmacs
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	    before (count-lines beg opoint)
	    after (count-lines opoint end))
      (message "Page has %d lines (%d + %d)" total before after))))

(defun what-page ()
  "Print page and line number of point."
  (interactive "_") ; XEmacs
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (let ((count 1)
	    (opoint (point)))
	(goto-char 1)
	(while (re-search-forward page-delimiter opoint t)
	  (setq count (1+ count)))
	(message "Page %d, line %d"
		 count
		 (1+ (count-lines (point) opoint)))))))

;;; Place `provide' at end of file.
(provide 'page)

;;; page.el ends here
