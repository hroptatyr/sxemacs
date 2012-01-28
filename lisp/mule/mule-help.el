;;; mule-help.el --- Mule-ized Help functions

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: SL Baur <steve@xemacs.org>
;; Keywords: help, internal, mule

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

;;; Synched up with: Emacs 20.1

;;; Commentary:

;;

;;; Code:

;; TUTORIAL arg is XEmacs addition
(defun help-with-tutorial (&optional arg tutorial)
  "Select the XEmacs learn-by-doing tutorial.
If there is a tutorial version written in the language
of the selected language environment, that version is used.
If there's no tutorial in that language, `TUTORIAL' is selected.
With arg, you are asked to select which language."
  (interactive "P")
  (let (lang filename file)
    (if arg
	(or (setq lang (read-language-name 'tutorial "Language: "))
	    (error "No tutorial file of the specified language"))
      (setq lang current-language-environment))
    ;; The menubar buttons call this function like this:
    ;; (help-with-tutorial nil "tutorial.lang")
    (setq filename (if (and (not arg) tutorial)
		       tutorial
		     (or (get-language-info lang 'tutorial)
			 (or tutorial "TUTORIAL"))))
    (setq file (expand-file-name (concat "~/" filename)))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (let ((coding-system-for-read
	     (get-language-info lang 'tutorial-coding-system)))
	(insert-file-contents (locate-data-file filename)))
      (goto-char (point-min))
      ;; The 'didactic' blank lines: Possibly insert blank lines
      ;; around <<nya nya nya>>, and change << >> to [ ].
      (if (re-search-forward "^<<.+>>" nil t)
	  (let ((n (- (window-height (selected-window))
		      (count-lines (point-min) (point-at-bol))
		      6)))
	    (if (< n 12)
		(progn (beginning-of-line) (kill-line))
	      ;; Some people get confused by the large gap
	      (delete-backward-char 2)
	      (insert "]")
	      (beginning-of-line)
	      (save-excursion
		(delete-char 2)
		(insert "["))
	      (newline (/ n 2))
	      (next-line 1)
	      (newline (- n (/ n 2))))))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))


(provide 'mule-help)

;;; mule-help.el ends here
