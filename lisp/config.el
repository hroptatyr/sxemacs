;;; config.el --- access configuration parameters

;; Copyright (C) 1997 Sun Microsystems, Inc.

;; Author:   Martin Buchholz
;; Keywords: configure

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
