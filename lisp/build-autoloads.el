;;; build-autoloads.el --- Guess what!
;;
;; Copyright (C) 2006 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: internal
;;
;; This file is part of SXEmacs.
;;
;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Code:

(unless (fboundp #'error)
  (load "loadup-el.el"))
(load "autoload.el")

(batch-create-autoloads)

;; indicate success
(kill-emacs 0)

;;; build-autoloads.el ends here
