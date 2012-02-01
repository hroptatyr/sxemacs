;;; code-cmds.el --- Commands for manipulating coding systems..

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2000 Free Software Foundation
;; Copyright (C) 1997 MORIOKA Tomohiko


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

;;
;; This code defines the keybindings and utility commands for the
;; user to manipulate coding systems.
;; This code used to be in mule-cmds.el which now only needs the
;; additional bindings/commands that are avaible on the real mule.


;;; Code:

;;; Coding related key bindings and menus.

(defvar coding-keymap (make-sparse-keymap "Mule/Conding")
  "Keymap for Mule and Coding cystem specific commands.")

;; Keep "C-x C-m ..." for mule specific commands.
(define-key ctl-x-map "\C-m" coding-keymap)

(define-key coding-keymap "f" 'set-buffer-file-coding-system)
(define-key coding-keymap "F" 'set-default-buffer-file-coding-system) ; XEmacs
(define-key coding-keymap "t" 'set-terminal-coding-system)
(define-key coding-keymap "p" 'set-buffer-process-coding-system)
;(define-key coding-keymap "x" 'set-selection-coding-system)
;(define-key coding-keymap "X" 'set-next-selection-coding-system)
(define-key coding-keymap "c" 'universal-coding-system-argument)
;;(define-key coding-keymap "c" 'list-coding-system-briefly) ; XEmacs
;;(define-key coding-keymap "C" 'describe-coding-system)	 ; XEmacs


(defun coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system which differs from CODING-SYSTEM in eol conversion.
The returned coding system converts end-of-line by EOL-TYPE
but text as the same way as CODING-SYSTEM.
EOL-TYPE should be `lf', `crlf', `cr' or nil.
If EOL-TYPE is nil, the returned coding system detects
how end-of-line is formatted automatically while decoding.

EOL-TYPE can be specified by an symbol `unix', `dos' or `mac'.
They means `lf', `crlf', and `cr' respectively."
  (if (symbolp eol-type)
      (setq eol-type (cond ((or (eq eol-type 'unix)
				(eq eol-type 'lf))
			    'eol-lf)
			   ((or (eq eol-type 'dos)
				(eq eol-type 'crlf))
			    'eol-crlf)
			   ((or (eq eol-type 'mac)
				(eq eol-type 'cr))
			    'eol-cr)
			   (t eol-type))))
  (let ((orig-eol-type (coding-system-eol-type coding-system)))
    (if (null orig-eol-type)
	(if (not eol-type)
	    coding-system
	  (coding-system-property coding-system eol-type))
      (let ((base (coding-system-base coding-system)))
	(if (not eol-type)
	    base
	  (if (= eol-type orig-eol-type)
	      coding-system
	    (setq orig-eol-type (coding-system-eol-type base))
	    (if (null orig-eol-type)
		(coding-system-property base eol-type))))))))


(defun universal-coding-system-argument ()
  "Execute an I/O command using the specified coding system."
  (interactive)
  (let* ((default (and buffer-file-coding-system
		       (not (eq (coding-system-type buffer-file-coding-system)
				t))
		       (coding-system-name buffer-file-coding-system)))
	 (coding-system
	  (read-coding-system
	   (if default
	       (format "Coding system for following command (default, %s): "
		       default)
	     "Coding system for following command: ")
	   default))
	 (keyseq (read-key-sequence
		  (format "Command to execute with %s:" coding-system)))
	 (cmd (key-binding keyseq)))
    (let ((coding-system-for-read coding-system)
	  (coding-system-for-write coding-system))
      (message "")
      (call-interactively cmd))))

(defun set-default-coding-systems (coding-system)
  "Set default value of various coding systems to CODING-SYSTEM.
This sets the following coding systems:
  o coding system of a newly created buffer
  o default coding system for terminal output
  o default coding system for keyboard input
  o default coding system for subprocess I/O
  o default coding system for converting file names."
  (check-coding-system coding-system)
  ;;(setq-default buffer-file-coding-system coding-system)
  (set-default-buffer-file-coding-system coding-system)
  ;; (if default-enable-multibyte-characters
  ;;     (setq default-file-name-coding-system coding-system))
  ;; If coding-system is nil, honor that on MS-DOS as well, so
  ;; that they could reset the terminal coding system.
  ;; (unless (and (eq window-system 'pc) coding-system)
  ;;   (setq default-terminal-coding-system coding-system))
  (set-terminal-coding-system coding-system)
  ;;(setq default-keyboard-coding-system coding-system)
  (set-keyboard-coding-system coding-system)
  ;;(setq default-process-coding-system (cons coding-system coding-system))
  ;; Refer to coding-system-for-read and coding-system-for-write
  ;; so that C-x RET c works.
  (add-hook 'comint-exec-hook
	    `(lambda ()
	       (let ((proc (get-buffer-process (current-buffer))))
		 (set-process-input-coding-system
		  proc (or coding-system-for-read ',coding-system))
		 (set-process-output-coding-system
		  proc (or coding-system-for-write ',coding-system))))
	    'append)
  (setq file-name-coding-system coding-system))

(defun prefer-coding-system (coding-system)
  "Add CODING-SYSTEM at the front of the priority list for automatic detection.
This also sets the following coding systems:
  o coding system of a newly created buffer
  o default coding system for terminal output
  o default coding system for keyboard input
  o default coding system for converting file names.

If CODING-SYSTEM specifies a certain type of EOL conversion, the coding
systems set by this function will use that type of EOL conversion.

This command does not change the default value of terminal coding system
for MS-DOS terminal, because DOS terminals only support a single coding
system, and Emacs automatically sets the default to that coding system at
startup."
  (interactive "zPrefer coding system: ")
  (if (not (and coding-system (find-coding-system coding-system)))
      (error "Invalid coding system `%s'" coding-system))
  (let ((coding-category (coding-system-category coding-system))
	(base (coding-system-base coding-system))
	(eol-type (coding-system-eol-type coding-system)))
    (if (not coding-category)
	;; CODING-SYSTEM is no-conversion or undecided.
	(error "Can't prefer the coding system `%s'" coding-system))
    (set-coding-category-system coding-category (or base coding-system))
    ;; (update-coding-systems-internal)
    (or (eq coding-category (car (coding-category-list)))
	;; We must change the order.
	(set-coding-priority-list (list coding-category)))
    (if (and base (interactive-p))
	(message "Highest priority is set to %s (base of %s)"
		 base coding-system))
    ;; If they asked for specific EOL conversion, honor that.
    (if (memq eol-type '(lf crlf mac))
	(setq coding-system
	      (coding-system-change-eol-conversion base eol-type))
      (setq coding-system base))
    (set-default-coding-systems coding-system)))

;;; Commands

(defun set-buffer-process-coding-system (decoding encoding)
  "Set coding systems for the process associated with the current buffer.
DECODING is the coding system to be used to decode input from the process,
ENCODING is the coding system to be used to encode output to the process.

For a list of possible values of CODING-SYSTEM, use \\[list-coding-systems]."
  (interactive
   "zCoding-system for process input: \nzCoding-system for process output: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (null proc)
	(error "no process")
      (check-coding-system decoding)
      (check-coding-system encoding)
      (set-process-coding-system proc decoding encoding)))
  (force-mode-line-update))

(provide 'code-cmds)

;;; code-cmds.el ends here
