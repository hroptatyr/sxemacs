;; Copyright (C) 2004 Vin Shelton

;; Author: Vin Shelton <acs@xemacs.org>
;; Maintainer: Vin Shelton <acs@xemacs.org>
;; Created: 2004
;; Keywords: tests

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

;; Test tag support.
;; See test-harness.el for instructions on how to run these tests.

(let ((testfile "tag-test.c")
      (tagfile "TAGS")
      (tags-build-completion-table nil))

  (cd (temp-directory))

  ;; Create a TAGS file
  (with-temp-file tagfile
    (insert
"
tag-test.c,99
struct mystruct mystruct2,1
struct mystruct *foo\(4,23
DEFUN \(\"require\", Frequire,require,7,51
"))

  ;; Create the test file
  (with-temp-file testfile
    (insert
"
struct mystruct { };

struct mystruct *foo\(\) {
}

DEFUN \(\"require\", Frequire, 1, 2, 0, /*
If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name.
*/
       \(feature, filename\)\)
{
}
"))

  (let ((tags-always-exact t))

    ;; Search for the tag "mystruct"; this should succeed
    (Silence-Message
     (find-tag "mystruct"))
    (Assert (eq (point) 2))

    ;; Search again.  The search should fail, based on the patch that
    ;; Sven Grundmann submitted for 21.4.16.
    (Check-Error-Message error "No more entries matching mystruct"
			 (Silence-Message
			  (tags-loop-continue))))

  (let ((tags-always-exact nil))

    ;; Search for the definition of "require". Until the etags.el upgrade
    ;; from 21.5 in 21.4.16, this test would fail.
    (condition-case nil
	(Silence-Message
	 (find-tag "require"))
      (t t))
    (Assert (eq (point) 52)))

  (kill-buffer testfile)
  (delete-file testfile)
  (kill-buffer tagfile)
  (delete-file tagfile))
