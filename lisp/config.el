;;; config.el --- access configuration parameters

;; Copyright (C) 1997 Sun Microsystems, Inc.

;; Author:   Martin Buchholz
;; Keywords: configure

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

;;; Synched up with: not in FSF.

;;; Commentary:

;;; Code:


(defvar config-value-file (expand-file-name "config.values" doc-directory)
  "File containing configuration parameters and their values.")

(defvar config-value-hash-table nil
  "Hash table to store configuration parameters and their values.")

;;;###autoload
(defun config-value-hash-table ()
  "Return hash table of configuration parameters and their values."
  (when (null config-value-hash-table)
    (setq config-value-hash-table (make-hash-table :size 300))
    (save-excursion
      (let ((buf (get-buffer-create " *Config*")))
	(set-buffer buf)
	(erase-buffer)
	(insert-file-contents config-value-file)
	(goto-char (point-min))
	(condition-case nil
	    (while t
	      (let* ((key (read buf))
		     (value (read buf))
		     (prev (gethash key config-value-hash-table)))
		(cond ((null prev)
		       (puthash key value config-value-hash-table))
		      ((atom prev)
		       (puthash key (list prev value) config-value-hash-table))
		      (t
		       (nconc prev (list value))))))
	  (end-of-file nil)))
      (kill-buffer " *Config*")))
  config-value-hash-table)

;;;###autoload
(defun config-value (config-symbol)
  "Return the value of the configuration parameter CONFIG_SYMBOL."
  (gethash config-symbol (config-value-hash-table)))

(provide 'config)
;;; config.el ends here
