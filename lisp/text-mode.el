;;; text-mode.el --- text mode, and its idiosyncratic commands.

;; Copyright (C) 1985, 1992, 1994, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp, dumped

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

;;; Synched up with: FSF 20.2.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This package provides the fundamental text mode documented in the
;; Emacs user's manual.

;;; Code:

(defvar text-mode-hook nil
  "Normal hook run when entering Text mode and many related modes.")

(defvar text-mode-variant nil
  "Non-nil if this buffer's major mode is a variant of Text mode.")

(defvar text-mode-syntax-table nil
  "Syntax table used while in text mode.")

(defvar text-mode-abbrev-table nil
  "Abbrev table used while in text mode.")
(define-abbrev-table 'text-mode-abbrev-table ())

(if text-mode-syntax-table
    ()
  (setq text-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " text-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " text-mode-syntax-table)
  (modify-syntax-entry ?' "w   " text-mode-syntax-table))

(defvar text-mode-map nil
  "Keymap for Text mode.
Many other modes, such as Mail mode, Outline mode and Indented Text mode,
inherit all the commands defined in this map.")

(if text-mode-map
    ()
  (setq text-mode-map (make-sparse-keymap))
  ;; XEmacs change
  (set-keymap-name text-mode-map 'text-mode-map)
  (define-key text-mode-map "\e\t" 'ispell-complete-word)
  (define-key text-mode-map "\t" 'indent-relative)
  (define-key text-mode-map "\es" 'center-line)
  (define-key text-mode-map "\eS" 'center-paragraph))


(defun text-mode ()
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{text-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|[ \t]*$"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (setq mode-name "Text")
  (setq major-mode 'text-mode)
  (run-hooks 'text-mode-hook))

(defun paragraph-indent-text-mode ()
  "Major mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs
when the first line of the following paragraph starts with whitespace.
Special commands:
\\{text-mode-map}
Turning on Paragraph-Indent Text mode runs the normal hooks
`text-mode-hook' and `paragraph-indent-text-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (setq mode-name "Parindent")
  (setq major-mode 'paragraph-indent-text-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'text-mode-hook 'paragraph-indent-text-mode-hook))

(defalias 'indented-text-mode 'text-mode)

(defun text-mode-hook-identify ()
  "Mark that this mode has run `text-mode-hook'.
This is how `toggle-text-mode-auto-fill' knows which buffers to operate on."
  (make-local-variable 'text-mode-variant)
  (setq text-mode-variant t))

(add-hook 'text-mode-hook 'text-mode-hook-identify)

(defun toggle-text-mode-auto-fill ()
  "Toggle whether to use Auto Fill in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  (let ((enable-mode (not (memq 'turn-on-auto-fill text-mode-hook)))
	(buffers (buffer-list)))
    (if enable-mode
	(add-hook 'text-mode-hook 'turn-on-auto-fill)
      (remove-hook 'text-mode-hook 'turn-on-auto-fill))
    (while buffers
      (with-current-buffer (car buffers)
	(if text-mode-variant
	    (auto-fill-mode (if enable-mode 1 0))))
      (setq buffers (cdr buffers)))
    (message "Auto Fill %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))

(defun center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (center-region (point) end))))

(defun center-region (from to)
  "Center each nonblank line starting in the region.
See `center-line' for more info."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(or (save-excursion (skip-chars-forward " \t") (eolp))
	    (center-line))
	(forward-line 1)))))

(defun center-line (&optional nlines)
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals
the distance between the end of the text and `fill-column'.
The argument NLINES says how many lines to center."
  (interactive "P")
  (if nlines (setq nlines (prefix-numeric-value nlines)))
  (while (not (eq nlines 0))
    (save-excursion
      (let ((lm (current-left-margin))
	    line-length)
	(beginning-of-line)
	(delete-horizontal-space)
	(end-of-line)
	(delete-horizontal-space)
	(setq line-length (current-column))
	(if (> (- fill-column lm line-length) 0)
	    (indent-line-to 
	     (+ lm (/ (- fill-column lm line-length) 2))))))
    (cond ((null nlines)
	   (setq nlines 0))
	  ((> nlines 0)
	   (setq nlines (1- nlines))
	   (forward-line 1))
	  ((< nlines 0)
	   (setq nlines (1+ nlines))
	   (forward-line -1)))))

;;; text-mode.el ends here
