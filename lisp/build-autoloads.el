;;; build-autoloads.el --- Guess what!
;;
;; Copyright (C) 2006 Sebastian Freundt
;; Copyright (C) 2007 Steve Youngs
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: internal
;;
;; This file is part of SXEmacs.
;;
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
;;
;;; Synched up with: Not in FSF.

;;; Commentary:
;;
;;  This file is only used during SXEmacs builds, it does what the
;;  name implies... builds the auto-autoloads.el files for lisp,
;;  lisp/mule, lisp/ffi, plus it generates the custom-load.el and
;;  custom-define.el files for same.  Emodule autoloads are handled
;;  here too.

;;; Code:

(unless (fboundp #'error)
  (load "loadup-el.el"))
(load "autoload.el")
(load "bytecomp.el")
(load "byte-optimize.el")
(load "cus-dep.el")

;; lisp/term is missing, but it currently doesn't have any autoloads
;; or customs. --SY.
(defvar autodirs '("."))

(when (featurep 'mule) (setq autodirs (cons "mule" autodirs)))
(when (fboundp #'ffi-defun) (setq autodirs (cons "ffi" autodirs)))
(setq autodirs (nreverse autodirs))

(defvar srcdir "../.sxemacs.source.tree/lisp/")

(mapcar
 #'(lambda (dir)
     (let ((pname (if (string= dir ".") "auto" dir))
	   (adir (concat srcdir dir)))
       (update-autoload-files adir pname (expand-file-name "auto-autoloads.el" dir) t)
       (Custom-make-dependencies adir (expand-file-name "custom-load.el" dir))
       (update-custom-define-files adir pname (expand-file-name "custom-defines.el" dir) t)
       ))
 autodirs)

;; emods
(defun find-emod-directories ()
  (let* ((objdir "../modules/")
	 (files (directory-files-recur
		 objdir 'full (concat
			       "\\.\\("
			       (mapconcat
				#'(lambda (e)
				    (replace-in-string e "\\." ""))
				module-extensions "\\|")
			       "\\)$")
		              ;; http://issues.sxemacs.org/show_bug.cgi?id=162
		              ;; (mapfam
			      ;;  #'(lambda (e)
			      ;;      (replace-in-string e "\\." ""))
			      ;;  :initiator "\\.\\("
			      ;;  :terminator "\\)$"
			      ;;  :separator "\\|"
			      ;;  :result-type #'concat module-extensions)
		 'list t 1))
	 (dir-bloom (make-bloom))
	 directories)
    (mapfam
     #'(lambda (f)
	 (let ((d (car (last (split-string-by-char (file-dirname f) ?/) 2))))
	   (unless (bloom-owns-p dir-bloom d)
	     (bloom-add dir-bloom d)
	     (setq directories (cons d directories)))))
     :result-type 'void files)
    directories))

(when (featurep 'modules)
  (let* ((modsrc "../.sxemacs.source.tree/modules/")
	 (mods (mapfam
		#'(lambda (d) (concat modsrc d))
		:result-type #'list (find-emod-directories)))
	 (feat "modules")
	 (autofile (expand-file-name "auto-autoloads.el" "../modules/")))
    (update-autoload-files mods feat autofile t)))

;; indicate success
(kill-emacs 0)

;;; build-autoloads.el ends here
