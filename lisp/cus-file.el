;;; cus-file.el --- Manage location of the customize init file

;; Copyright (C) 2000 by Free Software Foundation, Inc.
;; Copyright (C) 2007 Steve Youngs.

;; Author: Mike Sperber <mike@xemacs.org>
;; Keywords: internal

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

;; This file manages the location of the custom init file without
;; loading all of the custom code itself.


;;; Code:
(provide 'cus-file)

;;;###autoload
(defconst custom-file-base (concat "custom-" (user-login-name) ".el")
  "Base of file name for storing customization information.")

;;;###autoload
(defvar custom-file nil
  "File used for storing customization information.
If you change this from the default you need to
explicitly load that file for the settings to take effect.")

;;;###autoload
(defun make-custom-file-name (init-file &optional force-new)
  "Construct the default custom file name from the init file name.
If FORCE-NEW is non-nil, force post-migration location."
  (let ((init-file (or init-file user-init-file)))
    (if (or force-new
	    (not init-file)
	    (string= (file-name-directory init-file)
		     (expand-file-name
		      (file-name-as-directory user-init-directory))))
	;; back compat with ~/.sxemacs/custom.el
	(if (file-readable-p (expand-file-name "custom.el" user-init-directory))
	    (expand-file-name "custom.el" user-init-directory)
	  (expand-file-name custom-file-base user-init-directory))
      init-file)))

;;; cus-file.el ends here
