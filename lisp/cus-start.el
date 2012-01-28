;;; cus-start.el --- define customization properties of builtins.

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; The following code is used to define the customization properties
;; for builtin variables, and variables in the packages that are
;; preloaded /very/ early, before custom.el itself (replace.el is such
;; an example).  The way it handles custom stuff is dirty, and should
;; be regarded as a last resort.  DO NOT add variables here, unless
;; you know what you are doing.

;; Must be run before the user has changed the value of any options!


;;; Code:

(require 'custom)

(let ((all '(;; boolean
	     (abbrev-all-caps abbrev boolean)
	     (allow-deletion-of-last-visible-frame frames boolean)
	     (debug-on-quit debug boolean)
	     (delete-auto-save-files auto-save boolean)
	     (delete-exited-processes processes-basics boolean)
	     (indent-tabs-mode editing-basics boolean)
	     (load-ignore-elc-files maint boolean)
	     (load-warn-when-source-newer maint boolean)
	     (load-warn-when-source-only maint boolean)
	     (modifier-keys-are-sticky keyboard boolean)
	     (no-redraw-on-reenter display boolean)
	     (scroll-on-clipped-lines display boolean)
	     (truncate-partial-width-windows display boolean)
	     (visible-bell sound boolean)
	     (x-allow-sendevents x boolean)
	     (zmacs-regions editing-basics boolean)
	     (load-home-init-file installation boolean)
	     ;; integer
	     (auto-save-interval auto-save integer)
	     (bell-volume sound integer)
	     (bell-inhibit-time sound integer)
	     (echo-keystrokes keyboard integer)
	     (gc-cons-threshold alloc integer)
	     (next-screen-context-lines display integer)
	     (scroll-conservatively display integer)
	     (scroll-step windows integer)
	     (window-min-height windows integer)
	     (window-min-width windows integer)
	     ;; object
	     (auto-save-file-format auto-save
				    (choice (const :tag "Normal" t)
					    (repeat (symbol :tag "Format"))))
	     (completion-ignored-extensions minibuffer
					    (repeat
					     (string :format "%v")))
	     (debug-ignored-errors debug (repeat (choice :format "%v"
							 (symbol :tag "Class")
							 regexp)))
	     (debug-on-error debug  (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (debug-on-signal debug (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (exec-path processes-basics (repeat
					  (choice :tag "Directory"
						  (const :tag "Default" nil)
						  (directory :format "%v"))))
	     (file-name-handler-alist data (repeat
					    (cons regexp
						  (function :tag "Handler"))))
	     (shell-file-name execute file)
	     (stack-trace-on-error debug (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (stack-trace-on-signal debug (choice (const :tag "off" nil)
					    (const :tag "Always" t)
					    (repeat :menu-tag "When"
						    :value (nil)
						    (symbol
						     :tag "Condition"))))
	     (modifier-keys-sticky-time keyboard
					(choice (integer :tag "Milliseconds")
						(const :tag "Unbounded" 'unbounded)))
	     ;; buffer-local
	     (case-fold-search matching boolean)
	     (ctl-arrow display (choice (integer 160)
					(sexp :tag "160 (default)"
					      :format "%t\n")))
	     (fill-column fill integer)
	     (left-margin fill integer)
	     (tab-width editing-basics integer)
	     (truncate-lines display boolean)
	     (overwrite-mode editing-basics ;; for the options menu - dverna
			     (choice (const :tag "disabled" nil)
				     (const :tag "textual"
					    'overwrite-mode-textual)
				     (const :tag "binary"
					    'overwrite-mode-binary)))
	     ;; not documented as user-options, but should still be
	     ;; customizable:
	     (bar-cursor display (choice (const :tag "Block Cursor" nil)
					 (const :tag "Bar Cursor (1 pixel)" t)
					 (sexp :tag "Bar Cursor (2 pixels)"
					       :format "%t\n" 'other)))
	     (default-frame-plist frames plist)
	     (default-tty-frame-plist frames plist)
	     (default-x-frame-plist frames plist)
	     (disable-auto-save-when-buffer-shrinks auto-save boolean)
	     (find-file-use-truenames find-file boolean)
	     (find-file-compare-truenames find-file boolean)
	     (focus-follows-mouse x boolean)
	     (help-char keyboard (choice character
					 (sexp :tag "Single key specifier")))
	     (max-lisp-eval-depth limits integer)
	     (max-specpdl-size limits integer)
	     (meta-prefix-char keyboard character)
	     (parse-sexp-ignore-comments editing-basics boolean)
	     (selective-display display
				(choice (const :tag "off" nil)
					(integer :tag "space"
						 :format "%v"
						 1)
					(const :tag "on" t)))
	     (selective-display-ellipses display boolean)
	     (signal-error-on-buffer-boundary internal boolean)
	     (temp-buffer-show-function
	      windows (radio (function-item :tag "Temp Buffers Always in Same Frame"
					    :format "%t\n"
					    show-temp-buffer-in-current-frame)
			     (const :tag "Temp Buffers Like Other Buffers" nil)
			     (function :tag "Other")))
	     (undo-threshold undo integer)
	     (undo-high-threshold undo integer)
	     (words-include-escapes editing-basics boolean)
	     ;; These are from replace.el, which is loaded too early
	     ;; to be customizable.
	     (case-replace matching boolean)
	     (query-replace-highlight matching boolean)
	     (list-matching-lines-default-context-lines matching integer)))
      this symbol group type)
  (while all
    (setq this (car all)
	  all (cdr all)
	  symbol (nth 0 this)
	  group (nth 1 this)
	  type (nth 2 this))
    (if (not (boundp symbol))
	;; This is loaded so early, there is no message
	(if (fboundp 'message)
	    ;; If variables are removed from C code, give an error here!
	    (message "Intrinsic `%S' not bound" symbol))
      ;; This is called before any user can have changed the value.
      (put symbol 'standard-value
	   (list (quote-maybe (default-value symbol))))
      ;; Add it to the right group.
      (custom-add-to-group group symbol 'custom-variable)
      ;; Set the type.
      (put symbol 'custom-type type))))

;; This is to prevent it from being reloaded by `cus-load.el'.
(provide 'cus-start)

;;; cus-start.el ends here.
