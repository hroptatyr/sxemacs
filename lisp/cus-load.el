;;; cus-load.el --- Batch load all available cus-load files

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@xemacs.org>
;; Keywords: internal, help, faces

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

;; In FSF all of the custom loads are in a single `cus-load' file.
;; However, we have them distributed across directories, with optional
;; incremental loading.  Here we simply collect the whole set.


;;; Code:

(require 'custom)

(defun custom-add-loads (symbol list)
  "Update the custom-loads list of a symbol.
This works by adding the elements from LIST to the SYMBOL's
`custom-loads' property, avoiding duplicates.  Also, SYMBOL is
added to `custom-group-hash-table'."
  (let ((loads (get symbol 'custom-loads)))
    (dolist (el list)
      (unless (member el loads)
	(setq loads (nconc loads (list el)))))
    (put symbol 'custom-loads loads)
    (puthash symbol t custom-group-hash-table)))

(message "Loading customization dependencies...")

;; Garbage-collection seems to be very intensive here, and it slows
;; things down.  Nuke it.
(let ((gc-cons-threshold most-positive-fixnum))
  (mapc (lambda (dir)
	  (load (expand-file-name "custom-load" dir) t t))
	load-path))

(message "Loading customization dependencies...done")

(provide 'cus-load)

;;; cus-load.el ends here
