;;; msw-init.el --- initialization code for mswindows
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Rewritten for mswindows by: Jonathan Harris

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar mswindows-win-initted nil)
(defvar mswindows-pre-win-initted nil)
(defvar mswindows-post-win-initted nil)

(defun init-pre-mswindows-win ()
  "Initialize mswindows GUI at startup (pre).  Don't call this."
  (unless mswindows-pre-win-initted
    (setq mswindows-pre-win-initted t)))

(defun init-mswindows-win ()
  "Initialize mswindows GUI at startup.  Don't call this."
  (unless mswindows-win-initted
    (init-pre-mswindows-win)
    (make-mswindows-device)
    (init-post-mswindows-win (selected-console))
    (setq mswindows-win-initted t)))

(defun init-post-mswindows-win (console)
  "Initialize mswindows GUI at startup (post).  Don't call this."
  (unless mswindows-post-win-initted
    (if (featurep 'toolbar)
	(if (featurep 'infodock)
	    (require 'id-x-toolbar)
	  (init-x-toolbar)))
    (if (featurep 'gutter) (init-gutter))
    (add-hook 'zmacs-deactivate-region-hook
	      (lambda ()
		(if (console-on-window-system-p)
		    (disown-selection))))
    (add-hook 'zmacs-activate-region-hook
	      (lambda ()
		(if (console-on-window-system-p)
		    (activate-region-as-selection))))
    (add-hook 'zmacs-update-region-hook
	      (lambda ()
		(if (console-on-window-system-p)
		    (activate-region-as-selection))))
    ;; Old-style mswindows bindings. The new-style mswindows bindings
    ;; (namely Ctrl-X, Ctrl-C and Ctrl-V) are already spoken for by XEmacs.
    (global-set-key '(shift delete)   'kill-primary-selection)
    (global-set-key '(control delete) 'delete-primary-selection)
    (global-set-key '(shift insert)   'yank-clipboard-selection)
    (global-set-key '(control insert) 'copy-primary-selection)

    (global-set-key '(meta f4)	      'save-buffers-kill-emacs)

    ;; Random stuff
    (global-set-key 'menu	'popup-mode-menu)

    (setq mswindows-post-win-initted t)))

