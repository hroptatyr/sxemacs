;;; help-macro.el --- Makes command line help such as help-for-help

;; Copyright (C) 1993, 1994, 1997 Free Software Foundation, Inc.

;; Author: Lynn Slater <lrs@indetech.com>
;; Maintainer: FSF
;; Created: : Mon Oct  1 11:42:39 1990
;; Adapted-By: ESR

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

;;; Synched up with: FSF 20.2.

;; This file supplies the macro make-help-screen which constructs
;; single character dispatching with browsable help such as that provided
;; by help-for-help. This can be used to make many modes easier to use; for
;; example, the Gnu Emacs Empire Tool uses this for every "nested" mode map
;; called from the main mode map.

;;       The name of this package was changed from help-screen.el to
;; help-macro.el in order to fit in a 14-character limit.

;;-> ***********************  Example of use *********************************

;;->(make-help-screen help-for-empire-redistribute-map
;;->		  "c:civ m:mil p:population f:food ?"
;;->		  "You have discovered the GEET redistribution commands
;;->   From here, you can use the following options:
;;->
;;->c	Redistribute civs from overfull sectors into connected underfull ones
;;->	  The functions typically named by empire-ideal-civ-fcn control
;;->          based in part on empire-sector-civ-threshold
;;->m	Redistribute military using levels given by empire-ideal-mil-fcn
;;->p	Redistribute excess population to highways for max pop growth
;;->	  Excess is any sector so full babies will not be born.
;;->f	Even out food on highways to highway min and leave levels
;;->	  This is good to pump max food to all warehouses/dist pts
;;->
;;->
;;->Use \\[help-for-empire-redistribute-map] for help on redistribution.
;;->Use \\[help-for-empire-extract-map] for help on data extraction.
;;->Please use \\[describe-key] to find out more about any of the other keys."
;;->		  empire-shell-redistribute-map)

;;->  (define-key c-mp "\C-h" 'help-for-empire-redistribute-map)
;;->  (define-key c-mp help-character 'help-for-empire-redistribute-map)

;;; Code:

(provide 'help-macro)

;;;###autoload
(defcustom three-step-help t
  "*Non-nil means give more info about Help command in three steps.
The three steps are simple prompt, prompt with all options,
and window listing and describing the options.
A value of nil means skip the middle step, so that
\\[help-command] \\[help-command] gives the window that lists the options."
  :type 'boolean
  :group 'help-appearance)

(defmacro make-help-screen (fname help-line help-text helped-map)
  "Construct help-menu function name FNAME.
When invoked, FNAME shows HELP-LINE and reads a command using HELPED-MAP.
If the command is the help character, FNAME displays HELP-TEXT
and continues trying to read a command using HELPED-MAP.
When FNAME finally does get a command, it executes that command
and then returns."
  `(defun ,fname ()
     ,help-text
     (interactive)
     (flet ((help-read-key (prompt)
	      ;; This is in `flet' to avoid problems with autoloading.
	      ;; #### The function is ill-conceived -- there should be
	      ;; a way to do it without all the hassle!
	      (let (events)
		(while (not (key-press-event-p
			     (aref (setq events (read-key-sequence prompt)) 0)))
		  ;; Mouse clicks are not part of the help feature, so
		  ;; reexecute them in the standard environment.
		  (mapc 'dispatch-event events))
		(let ((key (nconc (event-modifiers (aref events 0))
				  (list (event-key (aref events 0))))))
		  ;; Make the HELP key translate to C-h.
		  (when (lookup-key function-key-map key)
		    (setq key (lookup-key function-key-map key)))
		  (if (eq (length key) 1)
		      (car key)
		    key)))))
       (let ((line-prompt
	      (substitute-command-keys ,help-line)))
	 (when three-step-help
	   (message "%s" line-prompt))
	 (let* ((help-screen
		 (condition-case nil
		     (documentation (quote ,fname))
		   (void-function "(alias for undefined function)")
		   (error "(unexpected error from `documention')")))
		;; We bind overriding-local-map for very small
		;; sections, *excluding* where we switch buffers and
		;; where we execute the chosen help command.
		(local-map (make-sparse-keymap))
		(minor-mode-map-alist nil)
		(prev-frame (selected-frame))
		config new-frame key)
	   (unwind-protect
	       (progn
		 (set-keymap-parents local-map (list ,helped-map))
		 (cond (three-step-help
			(let* ((overriding-local-map local-map))
			  (setq key (help-read-key nil))))
		       (t
			(setq key ??)))
		 (when (or (equal key ??)
			   (equal key (list help-char)))
		   (setq config (current-window-configuration))
		   (switch-to-buffer-other-window "*Help*")
		   (and (not (eq (window-frame (selected-window))
				 prev-frame))
			(setq new-frame (window-frame (selected-window))
			      config nil))
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   (insert help-screen)
		   (help-mode)
		   (goto-char (point-min))
		   (while (member key `((,help-char) ?? (control v) space ?\177
					delete backspace (meta v)))
		     (ignore-errors
		       (cond ((member key '((control v) space))
			      (scroll-up))
			     ((member key '(?\177 delete (meta v) backspace))
			      (scroll-down))))
		     (let ((cursor-in-echo-area t)
			   (overriding-local-map local-map))
		       (setq key (help-read-key
				  (format "Type one of the options listed%s: "
					  (if (pos-visible-in-window-p
					       (point-max))
					      "" " or Space to scroll")))))))
		 ;; We don't need the prompt any more.
		 (message nil)
		 (let ((defn (lookup-key local-map key)))
		   (cond (defn
			   (when config
			     (set-window-configuration config)
			     (setq config nil))
			   (when new-frame
			     (iconify-frame new-frame)
			     (setq new-frame nil))
			   (call-interactively defn))
			 (t
			  (ding)))))
	     (and (get-buffer "*Help*")
		  (bury-buffer "*Help*"))
	     (and new-frame (iconify-frame new-frame))
	     (and config
		  (set-window-configuration config))))))))

;;; help-macro.el
