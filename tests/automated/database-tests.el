;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Martin Buchholz <martin@xemacs.org>
;; Maintainer: Martin Buchholz <martin@xemacs.org>
;; Created: 1998
;; Keywords: tests, database

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; SXEmacs is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Test database functionality
;;; See test-harness.el

(condition-case nil
    (require 'test-harness)
  (file-error
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (file-name-directory load-file-name) load-path)
     (require 'test-harness))))

(flet ((delete-database-files (filename)
	(dolist (fn (list filename
			  (concat filename ".db")
			  (concat filename ".pag")
			  (concat filename ".dir")))
	  (ignore-file-errors (delete-file fn))))

       (test-database (db)
	(Assert (databasep db))
	(put-database "key1" "val1" db)
	(Assert-Equal "val1" (get-database "key1" db))
	(remove-database "key1" db)
	(Assert-Equal nil (get-database "key1" db))
	(close-database db)
	(Assert (not (database-live-p db)))
	(Assert (databasep db))))

  (let ((filename (expand-file-name "test-harness" (temp-directory))))

    (dolist (db-type '(dbm berkeley-db))
      (when (featurep db-type)
	(princ "\n")
	(delete-database-files filename)
	(test-database (open-database filename db-type))
	(delete-database-files filename)))))
