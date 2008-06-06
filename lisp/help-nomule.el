;;; help-nomule.el --- Help functions when not in Mule

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
;; Keywords: help, internal, dumped

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

(defconst tutorial-supported-languages
  '(
    ("Croatian" hr iso-8859-2)
    ("French" fr iso-8859-1)
    ("German" de iso-8859-1)
    ("Norwegian" no iso-8859-1)
    ("Polish" pl iso-8859-2)
    ("Romanian" ro iso-8859-2)
    ("Swedish" se iso-8859-1)
    )
  "Alist of supported languages in TUTORIAL files.
Add languages here, as more are translated.")

;; TUTORIAL arg is XEmacs addition
(defun help-with-tutorial (&optional tutorial language)
  "Select the XEmacs learn-by-doing tutorial.
Optional arg TUTORIAL specifies the tutorial file; default is \"TUTORIAL\".
With a prefix argument, choose the language."
  (interactive "i\nP")
  (or tutorial
      (setq tutorial "TUTORIAL"))
  (when (and language (consp language))
    (let ((completion-ignore-case t))
      (setq language (assoc (completing-read "Language: "
					     tutorial-supported-languages
					     nil t)
			    tutorial-supported-languages))))
  (when language
    (setq tutorial (format "%s.%s" tutorial (cadr language))))
  (let ((file (expand-file-name tutorial "~")))
    (delete-other-windows)
    (let ((buffer (or (get-file-buffer file)
		      (create-file-buffer file)))
	  (window-configuration (current-window-configuration)))
      (condition-case error-data
	  (progn
	    (switch-to-buffer buffer)
	    (setq buffer-file-name file)
	    (setq default-directory (expand-file-name "~/"))
	    (setq buffer-auto-save-file-name nil)
	    ;; Because of non-Mule users, TUTORIALs are not coded
	    ;; independently, so we must guess the coding according to
	    ;; the language.
	    (let ((coding-system-for-read (nth 2 language)))
	      (insert-file-contents (locate-data-file tutorial)))
	    (goto-char (point-min))
	    ;; The 'didactic' blank lines: possibly insert blank lines
	    ;; around <<nya nya nya>> and replace << >> with [ ].
	    (if (re-search-forward "^<<.+>>")
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
	    (set-buffer-modified-p nil))
	;; TUTORIAL was not found: kill the buffer and restore the
	;; window configuration.
	(file-error (kill-buffer buffer)
		    (set-window-configuration window-configuration)
		    ;; Now, signal the error
		    (signal (car error-data) (cdr error-data)))))))

;; General Mule-compatibility stuffs
(define-function 'string-width 'length)

;; The following was originally in subr.el
(unless (featurep 'mule)
  (defun make-char (charset &optional arg1 arg2)
    "Make a character from CHARSET and octets ARG1 and ARG2.
This function is available for compatibility with Mule-enabled XEmacsen.
When CHARSET is `ascii', return (int-char ARG1).  Otherwise, return
that value with the high bit set.  ARG2 is always ignored."
    (int-char (if (eq charset 'ascii)
		  arg1
		(logior arg1 #x80)))))


(provide 'help-nomule)

;;; help-nomule.el ends here
