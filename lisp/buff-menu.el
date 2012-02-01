;;; buff-menu.el --- buffer menu main function and support functions.

;; Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 19.34 except as noted.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; Edit, delete, or change attributes of all currently active Emacs
;; buffers from a list summarizing their state.  A good way to browse
;; any special or scratch buffers you have loaded, since you can't find
;; them by filename.  The single entry point is `Buffer-menu-mode',
;; normally bound to C-x C-b.

;;; Change Log:

;; Merged by esr with recent mods to Emacs 19 buff-menu, 23 Mar 1993
;;
;; Modified by Bob Weiner, Motorola, Inc., 4/14/89
;;
;; Added optional backup argument to 'Buffer-menu-unmark' to make it undelete
;; current entry and then move to previous one.
;;
;; Based on FSF code dating back to 1985.

;;; Code:

;;;Trying to preserve the old window configuration works well in
;;;simple scenarios, when you enter the buffer menu, use it, and exit it.
;;;But it does strange things when you switch back to the buffer list buffer
;;;with C-x b, later on, when the window configuration is different.
;;;The choice seems to be, either restore the window configuration
;;;in all cases, or in no cases.
;;;I decided it was better not to restore the window config at all. -- rms.

;;;But since then, I changed buffer-menu to use the selected window,
;;;so q now once again goes back to the previous window configuration.

;;;(defvar Buffer-menu-window-config nil
;;;  "Window configuration saved from entry to `buffer-menu'.")

; Put buffer *Buffer List* into proper mode right away
; so that from now on even list-buffers is enough to get a buffer menu.

(defvar Buffer-menu-buffer-column 4)

(defvar Buffer-menu-mode-map nil)

(if Buffer-menu-mode-map
    ()
  (setq Buffer-menu-mode-map (make-keymap))
  (suppress-keymap Buffer-menu-mode-map t)
  (set-keymap-name Buffer-menu-mode-map 'Buffer-menu-mode-map) ; XEmacs
  (define-key Buffer-menu-mode-map "q" 'Buffer-menu-quit)
  (define-key Buffer-menu-mode-map "v" 'Buffer-menu-select)
  (define-key Buffer-menu-mode-map "2" 'Buffer-menu-2-window)
  (define-key Buffer-menu-mode-map "1" 'Buffer-menu-1-window)
  (define-key Buffer-menu-mode-map "f" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "\C-m" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "o" 'Buffer-menu-other-window)
  (define-key Buffer-menu-mode-map "\C-o" 'Buffer-menu-switch-other-window)
  (define-key Buffer-menu-mode-map "s" 'Buffer-menu-save)
  (define-key Buffer-menu-mode-map "d" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "\C-d" 'Buffer-menu-delete-backwards)
  (define-key Buffer-menu-mode-map "\C-k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "x" 'Buffer-menu-execute)
  (define-key Buffer-menu-mode-map " " 'next-line)
  (define-key Buffer-menu-mode-map "n" 'next-line)
  (define-key Buffer-menu-mode-map "p" 'previous-line)
  (define-key Buffer-menu-mode-map 'backspace 'Buffer-menu-backup-unmark)
  (define-key Buffer-menu-mode-map 'delete 'Buffer-menu-backup-unmark)
  (define-key Buffer-menu-mode-map "~" 'Buffer-menu-not-modified)
  (define-key Buffer-menu-mode-map "?" 'describe-mode)
  (define-key Buffer-menu-mode-map "u" 'Buffer-menu-unmark)
  (define-key Buffer-menu-mode-map "m" 'Buffer-menu-mark)
  (define-key Buffer-menu-mode-map "t" 'Buffer-menu-visit-tags-table)
  (define-key Buffer-menu-mode-map "%" 'Buffer-menu-toggle-read-only)
  (define-key Buffer-menu-mode-map "g" 'revert-buffer)
  (define-key Buffer-menu-mode-map 'button2 'Buffer-menu-mouse-select)
  (define-key Buffer-menu-mode-map 'button3 'Buffer-menu-popup-menu)
  )

;; Buffer Menu mode is suitable only for specially formatted data.
(put 'Buffer-menu-mode 'mode-class 'special)

(defun Buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<Buffer-menu-mode-map>
\\[Buffer-menu-mouse-select] -- select buffer you click on, in place of the buffer menu.
\\[Buffer-menu-this-window] -- select current line's buffer in place of the buffer menu.
\\[Buffer-menu-other-window] -- select that buffer in another window,
  so the buffer menu buffer remains visible in its window.
\\[Buffer-menu-switch-other-window] -- make another window display that buffer.
\\[Buffer-menu-mark] -- mark current line's buffer to be displayed.
\\[Buffer-menu-select] -- select current line's buffer.
  Also show buffers marked with m, in other windows.
\\[Buffer-menu-1-window] -- select that buffer in full-frame window.
\\[Buffer-menu-2-window] -- select that buffer in one window,
  together with buffer selected before this one in another window.
\\[Buffer-menu-visit-tags-table] -- visit-tags-table this buffer.
\\[Buffer-menu-not-modified] -- clear modified-flag on that buffer.
\\[Buffer-menu-save] -- mark that buffer to be saved, and move down.
\\[Buffer-menu-delete] -- mark that buffer to be deleted, and move down.
\\[Buffer-menu-delete-backwards] -- mark that buffer to be deleted, and move up.
\\[Buffer-menu-execute] -- delete or save marked buffers.
\\[Buffer-menu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[Buffer-menu-backup-unmark] -- back up a line and remove marks.
\\[Buffer-menu-toggle-read-only] -- toggle read-only status of buffer on this line."
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'Buffer-menu-revert-function)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (make-local-hook 'mouse-track-click-hook) ; XEmacs
  (add-hook 'mouse-track-click-hook 'Buffer-menu-maybe-mouse-select t t) ; XEmacs
  (run-hooks 'buffer-menu-mode-hook))

(defun Buffer-menu-revert-function (ignore1 ignore2)
  (list-buffers))

(defun Buffer-menu-buffer (error-if-non-existent-p)
  "Return buffer described by this line of buffer menu."
  (let* ((where (save-excursion
		  (beginning-of-line)
		  (+ (point) Buffer-menu-buffer-column)))
	 (name (and (not (eobp)) (get-text-property where 'buffer-name))))
    (if name
	(or (get-buffer name)
	    (if error-if-non-existent-p
		(error "No buffer named `%s'" name)
	      nil))
      (if error-if-non-existent-p
	  (error "No buffer on this line")
	nil))))

(defun buffer-menu (&optional arg)
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away."
  (interactive "P")
;;;  (setq Buffer-menu-window-config (current-window-configuration))
  (switch-to-buffer (list-buffers-noselect arg))
  (message
   "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun buffer-menu-other-window (&optional arg)
  "Display a list of buffers in another window.
With the buffer list buffer, you can save, delete or select the buffers.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away."
  (interactive "P")
;;;  (setq Buffer-menu-window-config (current-window-configuration))
  (switch-to-buffer-other-window (list-buffers-noselect arg))
  (message
   "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun Buffer-menu-quit ()
  "Quit the buffer menu."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; Switch away from the buffer menu and bury it.
    (switch-to-buffer (other-buffer))
    (bury-buffer buffer)))

(defun Buffer-menu-mark ()
  "Mark buffer on this line for being displayed by \\<Buffer-menu-mode-map>\\[Buffer-menu-select] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil))
      (delete-char 3)
      (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
  (forward-line (if backup -1 1)))

(defun Buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above."
  (interactive)
  (forward-line -1)
  (Buffer-menu-unmark)
  (forward-line -1))

(defun Buffer-menu-delete (&optional arg)
  "Mark buffer on this line to be deleted by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command.
Prefix arg is how many buffers to delete.
Negative arg means delete backwards."
  (interactive "p")
  (beginning-of-line)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (if (or (null arg) (= arg 0))
	  (setq arg 1))
      (while (> arg 0)
	(delete-char 1)
	(insert ?D)
	(forward-line 1)
	(setq arg (1- arg)))
      (while (< arg 0)
	(delete-char 1)
	(insert ?D)
	(forward-line -1)
	(setq arg (1+ arg))))))

(defun Buffer-menu-delete-backwards (&optional arg)
  "Mark buffer on this line to be deleted by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command
and then move up one line.  Prefix arg means move that many lines."
  (interactive "p")
  (Buffer-menu-delete (- (or arg 1)))
  (while (looking-at " [-M]")
    (forward-line 1)))

(defun Buffer-menu-save ()
  "Mark buffer on this line to be saved by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (forward-char 1)
      (delete-char 1)
      (insert ?S)
      (forward-line 1))))

(defun Buffer-menu-not-modified (&optional arg)
  "Mark buffer on this line as unmodified (no changes to save)."
  (interactive "P")
  (save-excursion
    (set-buffer (Buffer-menu-buffer t))
    (set-buffer-modified-p arg))
  (save-excursion
   (beginning-of-line)
   (forward-char 1)
   (if (= (char-after (point)) (if arg ?  ?*))
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert (if arg ?* ? ))))))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-save] or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^.S" nil t)
      (let ((modp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (save-buffer)
	  (setq modp (buffer-modified-p)))
	(let ((buffer-read-only nil))
	  (delete-char -1)
	  (insert (if modp ?* ? ))))))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
	  (buffer-read-only nil))
      (while (search-forward "\nD" nil t)
	(backward-char 1)
	(let ((buf (Buffer-menu-buffer nil)))
	  (or (eq buf nil)
	      (eq buf buff-menu-buffer)
	      (save-excursion (kill-buffer buf))))
	(if (Buffer-menu-buffer nil)
	    (progn (delete-char 1)
		   (insert ? ))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (backward-char 1))))))

(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with the \\<Buffer-menu-mode-map>\\[Buffer-menu-mark] command.
This command deletes and replaces all the previously existing windows
in the selected frame."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(others ())
	tem)
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil))
	(delete-char -1)
	(insert ?\ ))
      (or (eq tem buff) (memq tem others) (setq others (cons tem others))))
    (setq others (nreverse others)
	  tem (/ (1- (frame-height)) (1+ (length others))))
    (delete-other-windows)
    (switch-to-buffer buff)
    (or (eq menu buff)
	(bury-buffer menu))
    (if (equal (length others) 0)
	(progn
;;;	  ;; Restore previous window configuration before displaying
;;;	  ;; selected buffers.
;;;	  (if Buffer-menu-window-config
;;;	      (progn
;;;		(set-window-configuration Buffer-menu-window-config)
;;;		(setq Buffer-menu-window-config nil)))
	  (switch-to-buffer buff))
      (while others
	(split-window nil tem)
	(other-window 1)
	(switch-to-buffer (car others))
	(setq others (cdr others)))
      (other-window 1)			;back to the beginning!
)))



(eval-when-compile (autoload 'visit-tags-table "etags"))

(defun Buffer-menu-visit-tags-table ()
  "Visit the tags table in the buffer on this line.  See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (Buffer-menu-buffer t))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun Buffer-menu-1-window ()
  "Select this line's buffer, alone, in full frame."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t))
  (bury-buffer (other-buffer))
  (delete-other-windows)
  ;; XEmacs:
  ;; This is to get w->force_start set to nil.  Don't ask me, I only work here.
  (set-window-buffer (selected-window) (current-buffer)))

(defun Buffer-menu-mouse-select (event)
  "Select the buffer whose line you click on."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (event-buffer event)) ; XEmacs
      (save-excursion
	(goto-char (event-point event)) ; XEmacs
	(setq buffer (Buffer-menu-buffer t))))
    (select-window (event-window event)) ; XEmacs
    (if (and (window-dedicated-p (selected-window))
	     (eq (selected-window) (frame-root-window)))
	(switch-to-buffer-other-frame buffer)
      (switch-to-buffer buffer))))

;; XEmacs
(defun Buffer-menu-maybe-mouse-select (event &optional click-count)
  (interactive "e")
  (and (>= click-count 2)
       (let ((buffer (current-buffer))
	     (point (point))
	     (config (current-window-configuration)))
	 (condition-case nil
	     (progn
	       (Buffer-menu-mouse-select event)
	       t)
	   (error
	    (set-window-configuration config)
	    (set-buffer buffer)
	    (goto-char point)
	    nil)))))

(defun Buffer-menu-this-window ()
  "Select this line's buffer in this window."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t)))

(defun Buffer-menu-other-window ()
  "Select this line's buffer in other window, leaving buffer menu visible."
  (interactive)
  (switch-to-buffer-other-window (Buffer-menu-buffer t)))

(defun Buffer-menu-switch-other-window ()
  "Make the other window select this line's buffer.
The current window remains selected."
  (interactive)
  (display-buffer (Buffer-menu-buffer t)))

(defun Buffer-menu-2-window ()
  "Select this line's buffer, with previous buffer in second window."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(pop-up-windows t))
    (delete-other-windows)
    (switch-to-buffer (other-buffer))
    (pop-to-buffer buff)
    (bury-buffer menu)))

(defun Buffer-menu-toggle-read-only ()
  "Toggle read-only status of buffer on this line, perhaps via version control."
  (interactive)
  (let (char)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (modeline-toggle-read-only)
      (setq char (if buffer-read-only ?% ? )))
    (save-excursion
      (beginning-of-line)
      (forward-char 2)
      (if (/= (following-char) char)
	  (let (buffer-read-only)
	    (delete-char 1)
	    (insert char))))))

;; XEmacs
(defvar Buffer-menu-popup-menu
  '("Buffer Commands"
    ["Select Buffer"			Buffer-menu-select		t]
    ["Select buffer Other Window"	Buffer-menu-other-window	t]
    ["Clear Buffer Modification Flag"	Buffer-menu-not-modified	t]
    "----"
    ["Mark Buffer for Selection"	Buffer-menu-mark		t]
    ["Mark Buffer for Save"		Buffer-menu-save		t]
    ["Mark Buffer for Deletion"		Buffer-menu-delete		t]
    ["Unmark Buffer"			Buffer-menu-unmark		t]
    "----"
    ["Delete/Save Marked Buffers"	Buffer-menu-execute		t]
    ))

;; XEmacs
(defun Buffer-menu-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (let ((buffer (Buffer-menu-buffer nil)))
    (if buffer
	(popup-menu
	 (nconc (list (car Buffer-menu-popup-menu)
		      (concat
		       "Commands on buffer \"" (buffer-name buffer) "\":")
		      "----")
		(cdr Buffer-menu-popup-menu)))
      (error "no buffer on this line"))))


;; XEmacs
(defvar list-buffers-header-line
  (concat " MR Buffer           Size  Mode         File\n"
	  " -- ------           ----  ----         ----\n"))

;; XEmacs
(defvar list-buffers-identification 'default-list-buffers-identification
  "String used to identify this buffer, or a function of one argument
to generate such a string.  This variable is always buffer-local.")
(make-variable-buffer-local 'list-buffers-identification)

;; XEmacs
;;;###autoload
(defvar list-buffers-directory nil)

;;;###autoload
(make-variable-buffer-local 'list-buffers-directory)

;; #### not synched
(defun default-list-buffers-identification (output)
  (save-excursion
    (let ((file (or (buffer-file-name (current-buffer))
		    (and (boundp 'list-buffers-directory)
			 list-buffers-directory)))
	  (size (buffer-size))
	  (mode mode-name)
	  eob p s col)
      (set-buffer output)
      (end-of-line)
      (setq eob (point))
      (prin1 size output)
      (setq p (point))
      ;; right-justify the size
      (move-to-column 19 t)
      (setq col (point))
      (if (> eob col)
	  (goto-char eob))
      (setq s (- 6 (- p col)))
      (while (> s 0) ; speed/consing tradeoff...
	(insert ? )
	(setq s (1- s)))
      (end-of-line)
      (indent-to 27 1)
      (insert mode)
      (if (not file)
	  nil
	;; if the mode-name is really long, clip it for the filename
	(if (> 0 (setq s (- 39 (current-column))))
	    (delete-char (max s (- eob (point)))))
	(indent-to 40 1)
	(insert file)))))

;; #### not synched
(defun list-buffers-internal (output &optional predicate)
  (let ((current (current-buffer))
	(buffers (buffer-list)))
    (save-excursion
      (set-buffer output)
      (setq buffer-read-only nil)
      (erase-buffer)
      (buffer-disable-undo output)
      (insert list-buffers-header-line)

      (while buffers
	(let* ((col1 19)
	       (buffer (car buffers))
	       (name (buffer-name buffer))
	       this-buffer-line-start)
	  (setq buffers (cdr buffers))
	  (cond ((null name))           ;deleted buffer
		((and predicate
		      (not (if (stringp predicate)
			       (string-match predicate name)
			       (funcall predicate buffer))))
		 nil)
		(t
		 (set-buffer buffer)
		 (let ((ro buffer-read-only)
		       (id list-buffers-identification))
		   (set-buffer output)
		   (setq this-buffer-line-start (point))
		   (insert (if (eq buffer current)
			       (progn (setq current (point)) ?\.)
			       ?\ ))
		   (insert (if (buffer-modified-p buffer)
			       ?\*
			       ?\ ))
		   (insert (if ro
			       ?\%
			       ?\ ))
		   (if (string-match "[\n\"\\ \t]" name)
		       (let ((print-escape-newlines t))
			 (prin1 name output))
		       (insert ?\  name))
		   (indent-to col1 1)
		   (cond ((stringp id)
			  (insert id))
			 (id
			  (set-buffer buffer)
			  (condition-case e
			      (funcall id output)
			    (error
			     (princ "***" output) (prin1 e output)))
			  (set-buffer output)
			  (goto-char (point-max)))))
		 (put-nonduplicable-text-property this-buffer-line-start
						  (point)
						  'buffer-name name)
		 (put-nonduplicable-text-property this-buffer-line-start
						  (point)
						  'highlight t)
		 (insert ?\n)))))

      (Buffer-menu-mode)
      (if (not (bufferp current))
	  (goto-char current)))))
;(define-key ctl-x-map "\C-b" 'list-buffers)

(defun list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-nil optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive (list (if current-prefix-arg t nil))) ; XEmacs
  (display-buffer (list-buffers-noselect files-only)))

;; #### not synched
(defun list-buffers-noselect (&optional files-only)
  "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-nil optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (let ((buffer (get-buffer-create "*Buffer List*")))
    (list-buffers-internal buffer
			   (if (memq files-only '(t nil))
			       #'(lambda (b)
				   (let ((n (buffer-name b)))
				     (cond ((and (/= 0 (length n))
						 (= (aref n 0) ?\ ))
					    ;;don't mention if starts with " "
					    nil)
					   (files-only
					    (buffer-file-name b))
					   (t
					    t))))
			     files-only))
    buffer))

(defun buffers-menu-omit-invisible-buffers (buf)
  "For use as a value of `buffers-menu-omit-function'.
Omits normally invisible buffers (those whose name begins with a space)."
  (not (null (string-match "\\` " (buffer-name buf)))))

(provide 'buff-menu)

;;; buff-menu.el ends here
