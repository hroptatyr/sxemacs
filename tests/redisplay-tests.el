;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Maintainer: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Created: 2000
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

;; Redisplay related tests.

;; This used to crash XEmacs.
(when (featurep 'mule)
  (let ((buffer (generate-new-buffer "*split test*")))
    (set-window-buffer (selected-window) buffer)
    (split-window-vertically)
    (insert (make-char 'japanese-jisx0208 36 44))
    (backward-char)
    (redraw-frame)
    (delete-other-windows)
    (split-window)
    (kill-buffer buffer)
    (delete-other-windows)))
