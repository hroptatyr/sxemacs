;;; view-less.el --- Minor mode for browsing files with keybindings like `less'

;; Copyright (C) 1994, 1995 Tinker Systems and INS Engineering Corp.

;; Author: Jonathan Stigelman <stig@hackvan.com>
;; Maintainer: SXEmacs Development Team
;; Keywords: wp, unix

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This mode is for browsing files without changing them.  Keybindings
;; similar to those used by the less(1) program are used.
;;
;; Originally written for v18 by David Gudeman (gudeman@arizona.edu)
;; Mods by Bengt Martensson, to closely resemble less (July 1987)
;;
;; If you would like all write-protected files to be visited in view-mode,
;; then add the following to your .emacs file:
;;
;;      (add-hook 'find-file-hooks 'auto-view-mode)

;;; Code:

(defvar view-search-string ""
  "Last string searched for with view-search functions.")

(defvar view-search-arg 1
  "Argument to last view search.")

(defvar view-default-lines 10
  "Default value for the \"d\" and \"u\" commands in view-mode")

(defvar view-minor-mode nil
  "Non-nil when view-mode is active.  Call `view-mode' to toggle.")
(make-variable-buffer-local 'view-minor-mode)

;;;###autoload
(defvar view-minor-mode-map
  (let ((map (make-keymap)))
    (set-keymap-name map 'view-minor-mode-map)
    (suppress-keymap map)
    (define-key map "-" 'negative-argument)
    (define-key map " " 'scroll-up)
    (define-key map "f" 'scroll-up)
    (define-key map "b" 'scroll-down)
    (define-key map 'backspace 'scroll-down)
    (define-key map 'delete 'scroll-down)
    (define-key map "\r" 'view-scroll-lines-up)
    (define-key map "\n" 'view-scroll-lines-up)
    (define-key map "e" 'view-scroll-lines-up)
    (define-key map "j" 'view-scroll-lines-up)
    (define-key map "y" 'view-scroll-lines-down)
    (define-key map "k" 'view-scroll-lines-down)
    (define-key map "d" 'view-scroll-some-lines-up)
    (define-key map "u" 'view-scroll-some-lines-down)
    (define-key map "r" 'recenter)
    (define-key map "t" 'toggle-truncate-lines)
    (define-key map "N" 'view-buffer)
    (define-key map "E" 'view-file)
    (define-key map "P" 'view-buffer)
    (define-key map "!" 'shell-command)
    (define-key map "|" 'shell-command-on-region)
    (define-key map "=" 'what-line)
    (define-key map "?" 'view-search-backward)
    (define-key map "h" 'view-mode-describe)
    (define-key map "s" 'view-repeat-search)
    (define-key map "n" 'view-repeat-search)
    (define-key map "/" 'view-search-forward)
    (define-key map "\\" 'view-search-backward)
    (define-key map "g" 'view-goto-line)
    (define-key map "G" 'view-last-windowful)
    (define-key map "%" 'view-goto-percent)
    (define-key map "p" 'view-goto-percent)
    (define-key map "m" 'point-to-register)
    (define-key map "'" 'register-to-point)
    (define-key map "C" 'view-cleanup-backspaces)
    (define-key map "\C-c\C-c" 'view-quit)
    ;; #### - should this use substitute-command-keys?
    (define-key map "\C-x\C-q" 'view-quit-toggle-ro)
    (define-key map "q" 'view-quit)
    map
    ))

(add-minor-mode 'view-minor-mode " View" view-minor-mode-map)

;;;###autoload
(defvar view-mode-map
  (let ((map (copy-keymap view-minor-mode-map)))
    (set-keymap-name map 'view-mode-map)
    map))

;;;###autoload
(defun view-file (filename &optional other-window-p)
  "Find FILENAME, enter view mode.  With prefix arg OTHER-WINDOW-P, use other window."
  (interactive "fView File: \nP")
  (let ((old-p (get-file-buffer filename))
	(obuf (current-buffer)))
    (if other-window-p
	(find-file-other-window filename)
      (find-file filename))
    (view-mode (if other-window-p nil obuf)
	       (if old-p nil 'kill-buffer))
    nil))

;;;###autoload
(defun view-buffer (buffer &optional other-window-p)
  "Switch to BUFFER, enter view mode.  With prefix arg use other window."
  (interactive "bView Buffer: \nP")
  (let ((obuf (current-buffer)))
    (if other-window-p
	(switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))
    (view-mode (if other-window-p nil obuf)
	       (if other-window-p nil 'bury-buffer))))

;;;###autoload
(defun view-file-other-window (filename)
  "Find FILENAME in other window, and enter view mode."
  (interactive "fView File: ")
  (view-file filename t))

;;;###autoload
(defun view-buffer-other-window (buffer)
  "Switch to BUFFER in another window, and enter view mode."
  (interactive "bView Buffer: ")
  (view-buffer buffer t))

(defun view-brief-help ()
  (message
   (substitute-command-keys
    "\\<view-minor-mode-map>\\[scroll-up] = page forward; \\[scroll-down] = page back; \
\\[view-mode-describe] = help; \\[view-quit] = quit.")))

(defvar view-major-mode)
(defvar view-exit-position)
(defvar view-prev-buffer)
(defvar view-exit-action)
(defvar view-old-buffer-read-only)

;;;###autoload
(defun view-minor-mode (&optional prev-buffer exit-action)
  "Minor mode for viewing text, with bindings like `less'.
Commands are:
\\<view-minor-mode-map>
0..9	prefix args
-	prefix minus
\\[scroll-up]	page forward
\\[scroll-down]	page back
\\[view-scroll-lines-up]	scroll prefix-arg lines forward, default 1.
\\[view-scroll-lines-down]	scroll prefix-arg lines backward, default 1.
\\[view-scroll-some-lines-down]	scroll prefix-arg lines backward, default 10.
\\[view-scroll-some-lines-up]	scroll prefix-arg lines forward, default 10.
\\[what-line]	print line number
\\[view-mode-describe]	print this help message
\\[view-search-forward]	regexp search, uses previous string if you just hit RET
\\[view-search-backward]	as above but searches backward
\\[view-repeat-search]	repeat last search
\\[view-goto-line]	goto line prefix-arg, default 1
\\[view-last-windowful]	goto line prefix-arg, default last line
\\[view-goto-percent]	goto a position by percentage
\\[toggle-truncate-lines]	toggle truncate-lines
\\[view-file]	view another file
\\[view-buffer]	view another buffer
\\[view-cleanup-backspaces]	cleanup backspace constructions
\\[shell-command]	execute a shell command
\\[shell-command-on-region]\
	execute a shell command with the region as input
\\[view-quit]	exit view-mode, and bury the current buffer.

If invoked with the optional (prefix) arg non-nil, view-mode cleans up
backspace constructions.

More precisely:
\\{view-minor-mode-map}"
  (interactive)

  (make-local-variable 'view-default-lines)
  (set (make-local-variable 'view-exit-position)      (point))
  (set (make-local-variable 'view-prev-buffer)   prev-buffer)
  (set (make-local-variable 'view-exit-action)   exit-action)
  (set (make-local-variable 'view-old-buffer-read-only)     buffer-read-only)
  (add-hook (make-local-variable 'change-major-mode-hook)
	    'view-fixup-read-only)
  (setq view-minor-mode  t
	buffer-read-only t)
  (view-brief-help))

;;;###autoload
(defun view-mode (&optional prev-buffer exit-action clean-bs)
  "View the current buffer using view-minor-mode.  This exists to be 99.9%
compatible with the implementations of `view-mode' in view.el and older
versions of view-less.el."
  (interactive (list nil 'bury-buffer current-prefix-arg))
  ;; #### - The first two arguments provide compatibility with view.el (and
  ;; thus FSFmacs), while the third argument as a prefix argument maintains
  ;; interactive compatibility with older versions of view-less.  --Stig
  (if clean-bs (cleanup-backspaces))
  (view-minor-mode prev-buffer exit-action))

;;;###autoload
(defun view-major-mode (&optional prev-buffer exit-action clean-bs)
  "View the current buffer using view-mode, as a major mode.
This function has a nonstandard name because `view-mode' is wrongly
named but is like this for compatibility reasons."
  ;; #### - The first two arguments provide compatibility with view.el (and
  ;; thus FSFmacs), while the third argument as a prefix argument maintains
  ;; interactive compatibility with older versions of view-less.  --Stig
  (interactive (list nil 'bury-buffer current-prefix-arg))
  (kill-all-local-variables)
  (use-local-map view-mode-map)
  (setq major-mode 'view-mode)
  (set (make-local-variable 'view-exit-position)      (point))
  (set (make-local-variable 'view-prev-buffer)   prev-buffer)
  (set (make-local-variable 'view-exit-action)   exit-action)
  (set (make-local-variable 'view-old-buffer-read-only)     buffer-read-only)
  (set (make-local-variable 'view-major-mode) t)
  (setq buffer-read-only t)
  (if clean-bs (cleanup-backspaces))
  (run-hooks 'view-mode-hook))

;;;###autoload
(defun auto-view-mode ()
  "If the file of the current buffer is not writable, call view-mode.
This is meant to be added to `find-file-hooks'."
  (or (file-writable-p buffer-file-name)
      (view-minor-mode)))

(defun view-fixup-read-only ()
  ;; doing M-x normal mode should NOT leave the buffer read-only
  (and (boundp 'view-old-buffer-read-only)
       (progn (setq buffer-read-only view-old-buffer-read-only)
	      (kill-local-variable 'view-old-buffer-read-only))))

(defun view-quit-toggle-ro ()
  "Exit view mode and execute the global binding of the key that invoked this
command.  Normally, this will toggle the state of `buffer-read-only', perhaps
invoking some version-control mechanism."
  (interactive)
  (setq view-exit-position nil)
  ;; Kludge so this works as advertised.  Stig, why can't you write
  ;; bug-free code???
  (let ((buffer-read-only buffer-read-only))
    (view-quit t))
  ;; no longer in view-minor-mode, so the keymap has changed...
  (call-interactively (key-binding (this-command-keys))))

(defun view-quit (&optional no-exit-action)
  "Exit view mode.  With prefix argument, keep the current buffer selected."
  (interactive "P")
  (view-fixup-read-only)
  (setq view-minor-mode nil)
  (if view-exit-position (goto-char view-exit-position))
  (if (and (boundp 'view-major-mode) view-major-mode)
      (fundamental-mode)
    (let ((pbuf view-prev-buffer)
	  (exitact view-exit-action))
      (if no-exit-action
	  nil
	(if exitact (funcall exitact (current-buffer)))
	(if pbuf (switch-to-buffer pbuf))))))

;; #### - similar to what's in man.el and this ought to be written in C anyway...  --Stig
(defun cleanup-backspaces ()
  "Cleanup backspace constructions.
_^H and ^H_ sequences are deleted.  x^Hx sequences are turned into x for all
characters x.  ^^H| and |^H^ sequences are turned into ^.  +^Ho and o^H+ are
turned into (+)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (= (following-char) ?\C-h)
      (delete-char 1))
    (while (search-forward "\C-h" nil t)
      (backward-char 2)
      (cond ((looking-at "_\C-h\\|\\(.\\)\C-h\\1\\||\C-h\\^")
	     (delete-char 2))
	    ((looking-at ".\C-h_\\|\\^\C-h|")
	     (forward-char 1)
	     (delete-char 2))
	    ((looking-at "+\C-ho\\|o\C-h+")
	     (delete-char 3)
	     (insert "(+)"))
	    ((looking-at "|\C-h-")
	     (delete-char 3)
	     (insert "*"))
	    (t (forward-char 2))))))

(defun view-cleanup-backspaces ()
  "Cleanup backspaces and if buffer is currently unmodified, don't flag it
as a modified buffer.  This works even if the buffer is read-only."
  (interactive)
  (let ((buffer-read-only)
	(buf-mod (buffer-modified-p)))
    (cleanup-backspaces)
    ;; #### - THIS IS PROBABLY A REALLY DANGEROUS THING TO DO IN A MINOR MODE!!
    (set-buffer-modified-p buf-mod)))

;;;###autoload
(defun toggle-truncate-lines (&optional p)
  "Toggles the values of truncate-lines.
Positive prefix arg sets, negative disables."
  (interactive "P")
  (setq truncate-lines (if p
			   (> (prefix-numeric-value p) 0)
			 (not truncate-lines)))
  (recenter))

(defun view-scroll-lines-up (p)
  "Scroll up prefix-arg lines, default 1."
  (interactive "p")
  (scroll-up p))

(defun view-scroll-lines-down (p)
  "Scroll down prefix-arg lines, default 1."
  (interactive "p")
  (scroll-up (- p)))

(defun view-scroll-some-lines-down (&optional n)
  "Scroll down prefix-arg lines, default 10, or last argument."
  (interactive "p")
  (if (> n 1) (setq view-default-lines n))
  (scroll-down view-default-lines))

(defun view-scroll-some-lines-up (&optional n)
  "Scroll up prefix-arg lines, default 10, or last argument."
  (interactive "p")
  (if (> n 1) (setq view-default-lines n))
  (scroll-up view-default-lines))

(defun view-goto-line (&optional n)
  "Goto prefix arg line N.  N = 1 by default.."
  (interactive "p")
  (goto-line n))

(defun view-last-windowful (&optional n)
  "Goto prefix arg line N or the first line of the last windowful in buffer."
  (interactive "p")
  (if current-prefix-arg
      (goto-line n)
    (end-of-buffer)
    (recenter -1)
    (move-to-window-line 0)))

(defun view-goto-percent (&optional percent)
  "Set mark and go to a position PERCENT way into the current buffer."
  (interactive "p")
  (set-mark-command nil)
  (goto-char (+ (point-min) (/ (* percent (- (point-max) (point-min))) 100)))
  (beginning-of-line))

(defun view-mode-describe ()
  (interactive)
  (let ((mode-name "View")
	(major-mode 'view-mode))
    (describe-mode)))

(defun view-search-forward (s p)
  "Search forward for REGEXP.  If regexp is empty, use last search string.
With prefix ARG, search forward that many occurrences."
  (interactive "sView search: \np")
  (unwind-protect
      (re-search-forward
       (if (string-equal "" s) view-search-string s) nil nil p)
    (setq view-search-arg p)
    (or (string-equal "" s)
	(setq view-search-string s))))

(defun view-search-backward (s p)
  "Search backward for REGEXP.  If regexp is empty, use last search string.
With prefix ARG, search forward that many occurrences."
  (interactive "sView search backward: \np")
  (view-search-forward s (- p)))

(defun view-repeat-search (p)
  "Repeat last view search command.  If a prefix arg is given, use that
instead of the previous arg, if the prefix is just a -, then take the
negative of the last prefix arg."
  (interactive "P")
  (view-search-forward
   view-search-string
   (cond ((null p) view-search-arg)
	 ((eq p '-) (- view-search-arg))
	 (t (prefix-numeric-value p)))))

(provide 'view)
(provide 'view-less)

;;; view-less.el ends here
