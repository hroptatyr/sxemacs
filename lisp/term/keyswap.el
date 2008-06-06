;; keyswap.el --- swap BS and DEL keys

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Keywords: terminals

;; Copyright (C) 1992 Free Software Foundation, Inc.

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

;;; Commentary:

;;; This package is meant to be called by other terminal packages.

;;; Code:

(keyboard-translate ?\177 ?\^h)
(keyboard-translate ?\^h ?\177)

;;; keyswap.el ends here
