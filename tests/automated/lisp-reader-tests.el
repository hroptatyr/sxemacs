;; Copyright (C) 2005 Martin Kuehl.

;; Author: Martin Kuehl <martin.kuehl@gmail.com>
;; Maintainer: Martin Kuehl <martin.kuehl@gmail.com>
;; Created: 2005
;; Keywords: tests

;; This file is NOT part of SXEmacs.

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

;; Test the lisp reader.
;; See test-harness.el for instructions on how to run these tests.

;;; Raw Strings
;;; ===========

;; Equality to "traditional" strings
;; ---------------------------------
(dolist (strings '((#r"xyz"   "xyz")	 ; no backslashes
		   (#r"\xyz"  "\\xyz")   ; backslash at start
		   (#r"\\xyz" "\\\\xyz") ; backslashes at start
		   (#r"\nxyz" "\\nxyz")  ; escape seq. at start
		   (#r"\"xyz" "\\\"xyz") ; quote at start
		   (#r"xy\z"  "xy\\z")   ; backslash in middle
		   (#r"xy\\z" "xy\\\\z") ; backslashes in middle
		   (#r"xy\nz" "xy\\nz")  ; escape seq. in middle
		   (#r"xy\"z" "xy\\\"z") ; quote in middle
		   ;;(#r"xyz\"  "xyz\\")   ; backslash at end: error
		   (#r"xyz\\" "xyz\\\\") ; backslashes at end
		   (#r"xyz\n" "xyz\\n")  ; escape seq. at end
		   (#r"xyz\"" "xyz\\\"") ; quote at end
		   ))
  (Assert (apply #'string= strings)))

;; Odd number of backslashes at the end
;; ------------------------------------
(dolist (string '("#r\"xyz\\\""         ; `#r"abc\"': escaped delimiter
		  "#r\"xyz\\\\\\\""     ; `#r"abc\\\"': escaped delimiter
		  ))
  (with-temp-buffer
    (insert string)
    (Check-Error end-of-file (eval-buffer))))

;; Alternate string/regex delimiters
;; ---------------------------------
(dolist (string '("#r/xyz/"             ; Perl syntax
		  "#r:ix/xyz/"          ; Extended Perl syntax
		  "#r|xyz|"             ; TeX syntax
		  "#r[xyz]"             ; (uncommon) Perl syntax
		  "#r<xyz>"             ; Perl6 syntax?
		  "#r(xyz)"             ; arbitrary santax
		  "#r{xyz}"             ; arbitrary santax
		  "#r,xyz,"             ; arbitrary santax
		  "#r!xyz!"             ; arbitrary santax
		  ))
  (with-temp-buffer
    (insert string)
    (Check-Error-Message invalid-read-syntax "unrecognized raw string"
			 (eval-buffer))))
