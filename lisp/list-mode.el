;;; list-mode.el --- Major mode for buffers containing lists of items

;; Copyright (C) 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996, 2000 Ben Wing.

;; Maintainer: SXEmacs Development Team
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

;;; Synched up with: Not synched

;;; Commentary:

;; This file is dumped with SXEmacs.

;; Cleanup, merging with FSF by Ben Wing, January 1996

;;; Code:

(defvar list-mode-extent nil)
(make-variable-buffer-local 'list-mode-extent)

(defvar list-mode-map nil
  "Local map for buffers containing lists of items.")
(or list-mode-map
    (let ((map (setq list-mode-map (make-sparse-keymap 'list-mode-map))))
      (suppress-keymap map)
      (define-key map 'button2up 'list-mode-item-mouse-selected)
      (define-key map 'button2 'undefined)
      (define-key map "\C-m" 'list-mode-item-keyboard-selected)
;;
;; The following calls to `substitute-key-definition' losed because
;; they were based on an incorrect assumption that `forward-char' and
;; `backward-char' are bound to keys in the global map. This might not
;; be the case if a user binds motion keys to different functions,
;; and was not actually the case since 20.5 beta 28 or around.
;;
;;    (substitute-key-definition 'forward-char 'next-list-mode-item map
;;				 global-map)
;;    (substitute-key-definition 'backward-char 'previous-list-mode-item map
;;				 global-map)
;;
;; We bind standard keys to motion commands instead.
;;
      (dolist (key '(kp-right right (control ?f)))
	(define-key map key 'next-list-mode-item))
      (dolist (key '(kp-left left (control ?b)))
	(define-key map key 'previous-list-mode-item))))

;; #### We make list-mode-hook, as well as completion-setup-hook and
;; minibuffer-setup-hook, permanent-local so that it's possible to create
;; buffers in these modes and then set up some buffer-specific
;; customizations without resorting to awful kludges.  (The problem here
;; is that when you switch a buffer into a mode, reset-buffer is usually
;; called, which destroys all buffer-local settings that you carefully
;; tried to set up when you created the buffer.  Therefore, the only way
;; to set these variables is to use the setup hooks -- but if they are
;; not declared permanent local, then any local hook functions that you
;; put on them (which is exactly what you want to do) also get removed,
;; so you would have to resort to putting a global hook function on the
;; setup hook, and then making sure it gets removed later.  I actually
;; added some support for doing this with one-shot hooks, but this is
;; clearly not the correct way to do things, and it fails in some cases,
;; particularly when the buffer gets put into the mode more than once,
;; which typically happens with completion buffers, for example.)  In
;; fact, all setup hooks should be made permanent local, but I didn't
;; feel like making a global change like this quite yet.  The proper way
;; to do it would be to declare new def-style forms, such as defhook and
;; define-local-setup-hook, which are used to initialize hooks in place
;; of the current generic defvars. --ben

(put 'list-mode-hook 'permanent-local t)
(defvar list-mode-hook nil
  "Normal hook run when entering List mode.")

(defun list-mode ()
  "Major mode for buffer containing lists of items."
  (interactive)
  (kill-all-local-variables)
  (use-local-map list-mode-map)
  (setq mode-name "List")
  (setq major-mode 'list-mode)
  (add-local-hook 'post-command-hook 'set-list-mode-extent)
  (add-local-hook 'pre-command-hook 'list-mode-extent-pre-hook)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (setq list-mode-extent nil)
;; It is visually disconcerting to have the text cursor disappear within list
;; buffers, especially when moving from window to window, so leave it
;; visible.  -- Bob Weiner, 06/20/1999
; (set-specifier text-cursor-visible-p nil (current-buffer))
  (setq buffer-read-only t)
  (goto-char (point-min))
  (run-hooks 'list-mode-hook))

;; List mode is suitable only for specially formatted data.
(put 'list-mode 'mode-class 'special)

(defvar list-mode-extent-old-point nil
  "The value of point when pre-command-hook is called.
Used to determine the direction of motion.")
(make-variable-buffer-local 'list-mode-extent-old-point)

(defun list-mode-extent-pre-hook ()
  (setq list-mode-extent-old-point (point))
  ;(setq atomic-extent-goto-char-p nil)
)

(defun set-list-mode-extent ()
  "Move to the closest list item and set up the extent for it.
This is called from `post-command-hook'."
  (cond ((get-char-property (point) 'list-mode-item))
	((and (> (point) (point-min))
	      (get-char-property (1- (point)) 'list-mode-item))
	 (goto-char (1- (point))))
	(t
	 (let ((pos (point))
	       dirflag)
	   ;this fucks things up more than it helps.
	   ;atomic-extent-goto-char-p as currently defined is all broken,
	   ;since it will be triggered if the command *ever* runs goto-char!
	   ;(if atomic-extent-goto-char-p
	   ;    (setq dirflag 1)
	   (if (and list-mode-extent-old-point
		    (> pos list-mode-extent-old-point))
	       (setq dirflag 1)
	     (setq dirflag -1))
	   (next-list-mode-item dirflag)
	   (or (get-char-property (point) 'list-mode-item)
	       (next-list-mode-item (- dirflag))))))
  (or (and list-mode-extent
	   (eq (current-buffer) (extent-object list-mode-extent)))
      (progn
	(setq list-mode-extent (make-extent nil nil (current-buffer)))
	(set-extent-face list-mode-extent 'list-mode-item-selected)))
  (let ((ex (extent-at (point) nil 'list-mode-item nil 'at)))
    (if ex
	(progn
	  (set-extent-endpoints list-mode-extent
				(extent-start-position ex)
				(extent-end-position ex))
	  (auto-show-make-region-visible (extent-start-position ex)
					 (extent-end-position ex)))
      (detach-extent list-mode-extent))))

(defun previous-list-mode-item (n)
  "Move to the previous item in list-mode."
  (interactive "p")
  (next-list-mode-item (- n)))

(defun next-list-mode-item (n)
  "Move to the next item in list-mode.
With prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (while (and (> n 0) (not (eobp)))
    (let ((extent (extent-at (point) (current-buffer) 'list-mode-item))
	  (end (point-max)))
      ;; If in a completion, move to the end of it.
      (if extent (goto-char (extent-end-position extent)))
      ;; Move to start of next one.
      (or (extent-at (point) (current-buffer) 'list-mode-item)
	  (goto-char (next-single-property-change (point) 'list-mode-item
						  nil end))))
    (setq n (1- n)))
  (while (and (< n 0) (not (bobp)))
    (let ((extent (extent-at (point) (current-buffer) 'list-mode-item))
	  (end (point-min)))
      ;; If in a completion, move to the start of it.
      (if extent (goto-char (extent-start-position extent)))
      ;; Move to the start of that one.
      (if (setq extent (extent-at (point) (current-buffer) 'list-mode-item
				  nil 'before))
	  (goto-char (extent-start-position extent))
	(goto-char (previous-single-property-change
		    (point) 'list-mode-item nil end))
	(if (setq extent (extent-at (point) (current-buffer) 'list-mode-item
				    nil 'before))
	    (goto-char (extent-start-position extent)))))
    (setq n (1+ n))))

(defun list-mode-item-selected-1 (extent event)
  (let ((func (extent-property extent 'list-mode-item-activate-callback))
	(user-data (extent-property extent 'list-mode-item-user-data)))
    (if func
	(funcall func event extent user-data))))

;; we could make these two be just one function, but we want to be
;; able to refer to them in DOC strings.

(defun list-mode-item-keyboard-selected ()
  (interactive)
  (list-mode-item-selected-1 (extent-at (point) (current-buffer)
					'list-mode-item nil 'at)
			     nil))

(defun list-mode-item-mouse-selected (event)
  (interactive "e")
  ;; Sometimes event-closest-point returns nil.
  ;; So beep instead of bombing.
  (let ((point (event-closest-point event)))
    (if point
	(list-mode-item-selected-1 (extent-at point
					      (event-buffer event)
					      'list-mode-item nil 'at)
				   event)
      (ding))))

(defun add-list-mode-item (start end &optional buffer activate-callback
				 user-data)
  "Add a new list item in list-mode, from START to END in BUFFER.
BUFFER defaults to the current buffer.
This works by creating an extent for the span of text in question.
If ACTIVATE-CALLBACK is non-nil, it should be a function of three
  arguments (EVENT EXTENT USER-DATA) that will be called when button2
  is pressed on the extent.  USER-DATA comes from the optional
  USER-DATA argument."
  (let ((extent (make-extent start end buffer)))
    (set-extent-property extent 'list-mode-item t)
    (set-extent-property extent 'start-open t)
    (if activate-callback
	(progn
	  (set-extent-property extent 'mouse-face 'highlight)
	  (set-extent-property extent 'list-mode-item-activate-callback
			       activate-callback)
	  (set-extent-property extent 'list-mode-item-user-data user-data)))
    extent))


;; Define the major mode for lists of completions.


(defvar completion-highlight-first-word-only nil
  "*Completion will only highlight the first blank delimited word if t.
If the variable in not t or nil, the string is taken as a regexp to match for end
of highlight")

;; see comment at list-mode-hook.
(put 'completion-setup-hook 'permanent-local t)
(defvar completion-setup-hook nil
  "Normal hook run at the end of setting up the text of a completion buffer.
When run, the completion buffer is the current buffer.")

; Unnecessary FSFmacs crock.  We frob the extents directly in
; display-completion-list, so no "heuristics" like this are necessary.
;(defvar completion-fixup-function nil
;  "A function to customize how completions are identified in completion lists.
;`completion-setup-function' calls this function with no arguments
;each time it has found what it thinks is one completion.
;Point is at the end of the completion in the completion list buffer.
;If this function moves point, it can alter the end of that completion.")

(defvar completion-default-help-string
  '(concat
    (if (device-on-window-system-p)
	(substitute-command-keys
	 "Click \\<list-mode-map>\\[list-mode-item-mouse-selected] on a completion to select it.\n") "")
    (substitute-command-keys
     "Type \\<minibuffer-local-completion-map>\\[advertised-switch-to-completions] or \\[switch-to-completions] to move to this buffer, for keyboard selection.\n\n"))
  "Form the evaluate to get a help string for completion lists.
This string is inserted at the beginning of the buffer.
See `display-completion-list'.")

(defun display-completion-list (completions &rest cl-keys)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string or may be a list of two
 strings to be printed as if concatenated.
Frob a mousable extent onto each completion.  This extent has properties
 'mouse-face (so it highlights when the mouse passes over it) and
 'list-mode-item (so it can be located).

Keywords:
  :activate-callback (default is `default-choose-completion')
    See `add-list-mode-item'.
  :user-data
    Value passed to activation callback.
  :window-width
    If non-nil, width to use in displaying the list, instead of the
    actual window's width.
  :window-height
    If non-nil, use no more than this many lines, and extend line width as
    necessary.
  :help-string (default is the value of `completion-default-help-string')
    Form to evaluate to get a string to insert at the beginning of
    the completion list buffer.  This is evaluated when that buffer
    is the current buffer and after it has been put into
    completion-list-mode.
  :reference-buffer (default is the current buffer)
    This specifies the value of `completion-reference-buffer' in
    the completion buffer.  This specifies the buffer (normally a
    minibuffer) that `default-choose-completion' will insert the
    completion into.

At the end, run the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.
If `completion-highlight-first-word-only' is non-nil, then only the start
 of the string is highlighted."
   ;; #### I18N3 should set standard-output to be (temporarily)
   ;; output-translating.
  (cl-parsing-keywords
      ((:activate-callback 'default-choose-completion)
       :user-data
       :reference-buffer
       (:help-string completion-default-help-string)
       (:completion-string "Possible completions are:")
       :window-width
       :window-height)
      ()
    (let ((old-buffer (current-buffer))
	  (bufferp (bufferp standard-output)))
      (if bufferp
	  (set-buffer standard-output))
      (if (null completions)
	  (princ (gettext
		  "There are no possible completions of what you have typed."))
	(let ((win-width
	       (or cl-window-width
		   (if bufferp
		       ;; We have to use last-nonminibuf-frame here
		       ;; and not selected-frame because if a
		       ;; minibuffer-only frame is being used it will
		       ;; be the selected-frame at the point this is
		       ;; run.  We keep the selected-frame call around
		       ;; just in case.
	       (window-width (get-lru-window (last-nonminibuf-frame)))
		     80))))
	  (let ((count 0)
		(max-width 0)
		old-max-width)
	    ;; Find longest completion
	    (let ((tail completions))
	      (while tail
		(let* ((elt (car tail))
		       (len (cond ((stringp elt)
				   (length elt))
				  ((and (consp elt)
					(stringp (car elt))
					(stringp (car (cdr elt))))
				   (+ (length (car elt))
				      (length (car (cdr elt)))))
				  (t
				   (signal 'wrong-type-argument
					   (list 'stringp elt))))))
		  (if (> len max-width)
		      (setq max-width len))
		  (setq count (1+ count)
			tail (cdr tail)))))

	    (setq max-width (+ 2 max-width)) ; at least two chars between cols
	    (setq old-max-width max-width)
	    (let ((rows (let ((cols (min (/ win-width max-width) count)))
			  (if (<= cols 1)
			      count
			    (progn
			      ;; re-space the columns
			      (setq max-width (/ win-width cols))
			      (if (/= (% count cols) 0) ; want ceiling...
				  (1+ (/ count cols))
				(/ count cols)))))))
	      (when
		  (and cl-window-height
		       (> rows cl-window-height))
		(setq max-width old-max-width)
		(setq rows cl-window-height))
	      (when (and (stringp cl-completion-string)
			 (> (length cl-completion-string) 0))
		(princ (gettext cl-completion-string))
		(terpri))
	      (let ((tail completions)
		    (r 0)
		    (regexp-string
		     (if (eq t
			     completion-highlight-first-word-only)
			 "[ \t]"
		       completion-highlight-first-word-only)))
		(while (< r rows)
		  (and (> r 0) (terpri))
		  (let ((indent 0)
			(column 0)
			(tail2 tail))
		    (while tail2
		      (let ((elt (car tail2)))
			(if (/= indent 0)
			    (if bufferp
				(indent-to indent 2)
			      (while (progn (write-char ?\ )
					    (setq column (1+ column))
					    (< column indent)))))
			(setq indent (+ indent max-width))
			(let ((start (point))
			      end)
			  ;; Frob some mousable extents in there too!
			  (if (consp elt)
			      (progn
				(princ (car elt))
				(princ (car (cdr elt)))
				(or bufferp
				    (setq column
					  (+ column
					     (length (car elt))
					     (length (car (cdr elt)))))))
			    (progn
			      (princ elt)
			      (or bufferp
				  (setq column (+ column (length
							  elt))))))
			  (add-list-mode-item
			   start
			   (progn
			     (setq end (point))
			     (or
			      (and completion-highlight-first-word-only
				   (goto-char start)
				   (re-search-forward regexp-string end t)
				   (match-beginning 0))
			      end))
			   nil cl-activate-callback cl-user-data)
			  (goto-char end)))
		      (setq tail2 (nthcdr rows tail2)))
		    (setq tail (cdr tail)
			  r (1+ r)))))))))
      (if bufferp
	  (set-buffer old-buffer)))
    (save-excursion
      (let ((mainbuf (or cl-reference-buffer (current-buffer))))
	(set-buffer standard-output)
	(completion-list-mode)
	(make-local-variable 'completion-reference-buffer)
	(setq completion-reference-buffer mainbuf)
;;; The value 0 is right in most cases, but not for file name completion.
;;; so this has to be turned off.
;;;      (setq completion-base-size 0)
	(goto-char (point-min))
	(let ((buffer-read-only nil))
	  (insert (eval cl-help-string)))
	  ;; unnecessary FSFmacs crock
	  ;;(forward-line 1)
	  ;;(while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
	  ;;	  (let ((beg (match-beginning 0))
	  ;;		(end (point)))
	  ;;	    (if completion-fixup-function
	  ;;		(funcall completion-fixup-function))
	  ;;	    (put-text-property beg (point) 'mouse-face 'highlight)
	  ;;	    (put-text-property beg (point) 'list-mode-item t)
	  ;;	    (goto-char end)))))
	))
    (save-excursion
      (set-buffer standard-output)
      (run-hooks 'completion-setup-hook))))

(defvar completion-display-completion-list-function 'display-completion-list
  "Function to set up the list of completions in the completion buffer.
The function is called with one argument, the sorted list of completions.
Particular minibuffer interface functions (e.g. `read-file-name') may
want to change this.  To do that, set a local value for this variable
in the minibuffer; that ensures that other minibuffer invocations will
not be affected.")

(defun minibuffer-completion-help ()
  "Display a list of possible completions of the current minibuffer contents.
The list of completions is determined by calling `all-completions',
passing it the current minibuffer contents, the value of
`minibuffer-completion-table', and the value of
`minibuffer-completion-predicate'.  The list is displayed by calling
the value of `completion-display-completion-list-function' on the sorted
list of completions, with the standard output set to the completion
buffer."
  (interactive)
  (message "Making completion list...")
  (let ((completions (all-completions (buffer-string)
				      minibuffer-completion-table
				      minibuffer-completion-predicate)))
    (message nil)
    (if (null completions)
	(progn
	  (ding nil 'no-completion)
	  (temp-minibuffer-message " [No completions]"))
	(with-output-to-temp-buffer "*Completions*"
	  (funcall completion-display-completion-list-function
		   (sort completions #'string-lessp))))))

(define-derived-mode completion-list-mode list-mode
  "Completion List"
  "Major mode for buffers showing lists of possible completions.
\\{completion-list-mode-map}"
  (make-local-variable 'completion-base-size)
  (setq completion-base-size nil))

(let ((map completion-list-mode-map))
  (define-key map 'button2up 'mouse-choose-completion)
  (define-key map 'button2 'undefined)
  (define-key map "\C-m" 'choose-completion)
  (define-key map "\e\e\e" 'delete-completion-window)
  (define-key map "\C-g" 'minibuffer-keyboard-quit)
  (define-key map "q" 'completion-list-mode-quit)
  (define-key map " " 'completion-switch-to-minibuffer)
  ;; [Tab] used to switch to the minibuffer but since [space] does that and
  ;; since most applications in the world use [Tab] to select the next item
  ;; in a list, do that in the *Completions* buffer too.  -- Bob Weiner,
  ;; BeOpen.com, 06/23/1999.
  (define-key map "\t" 'next-list-mode-item))

(defvar completion-reference-buffer nil
  "Record the buffer that was current when the completion list was requested.
This is a local variable in the completion list buffer.
Initial value is nil to avoid some compiler warnings.")

(defvar completion-base-size nil
  "Number of chars at beginning of minibuffer not involved in completion.
This is a local variable in the completion list buffer
but it talks about the buffer in `completion-reference-buffer'.
If this is nil, it means to compare text to determine which part
of the tail end of the buffer's text is involved in completion.")

;; These names are referenced in the doc string for `completion-list-mode'.
(defalias 'choose-completion 'list-mode-item-keyboard-selected)
(defalias 'mouse-choose-completion 'list-mode-item-mouse-selected)

(defun delete-completion-window ()
  "Delete the completion list window.
Go to the window from which completion was requested."
  (interactive)
  (let ((buf completion-reference-buffer))
    (delete-window (selected-window))
    (if (get-buffer-window buf)
	 (select-window (get-buffer-window buf)))))

(defun completion-switch-to-minibuffer ()
  "Move from a completions buffer to the active minibuffer window."
  (interactive)
  (select-window (minibuffer-window)))

(defun completion-list-mode-quit ()
  "Abort any recursive edit and bury the completions buffer."
  (interactive)
  (condition-case ()
      (abort-recursive-edit)
    (error nil))
  ;; If there was no recursive edit to abort, simply bury the completions
  ;; list buffer.
  (if (eq major-mode 'completion-list-mode) (bury-buffer)))

(defun completion-do-in-minibuffer ()
  (interactive "_")
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (call-interactively (key-binding (this-command-keys)))))

(defun default-choose-completion (event extent buffer)
  "Click on an alternative in the `*Completions*' buffer to choose it."
  (and (button-event-p event)
       ;; Give temporary modes such as isearch a chance to turn off.
       (run-hooks 'mouse-leave-buffer-hook))
  (let ((list-buffer (or (and (button-event-p event)
			      (event-buffer event))
			 (current-buffer))))
    (or buffer (setq buffer (symbol-value-in-buffer
			     'completion-reference-buffer
			     list-buffer)))
    (save-selected-window
      (and (button-event-p event)
	   (select-window (event-window event)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer))))
    (choose-completion-string (extent-string extent)
			      buffer
			      (symbol-value-in-buffer 'completion-base-size
						      list-buffer))))

;; Delete the longest partial match for STRING
;; that can be found before POINT.
(defun choose-completion-delete-max-match (string)
  (let ((len (min (length string)
		  (- (point) (point-min)))))
    (goto-char (- (point) (length string)))
    (if completion-ignore-case
	 (setq string (downcase string)))
    (while (and (> len 0)
		 (let ((tail (buffer-substring (point)
					       (+ (point) len))))
		   (if completion-ignore-case
		       (setq tail (downcase tail)))
		   (not (string= tail (substring string 0 len)))))
      (setq len (1- len))
      (forward-char 1))
    (delete-char len)))

;; Switch to BUFFER and insert the completion choice CHOICE.
;; BASE-SIZE, if non-nil, says how many characters of BUFFER's text
;; to keep.  If it is nil, use choose-completion-delete-max-match instead.
(defun choose-completion-string (choice &optional buffer base-size)
  (let ((buffer (or buffer completion-reference-buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and (string-match #r"\` \*Minibuf-[0-9]+\*\'" (buffer-name buffer))
	      (or (not (active-minibuffer-window))
		  (not (equal buffer
			      (window-buffer (active-minibuffer-window))))))
	 (error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	   (delete-region (+ base-size (point-min)) (point))
	 (choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point)
			       '(highlight nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	 (set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice.
      (and (equal buffer (window-buffer (minibuffer-window)))
	    minibuffer-completion-table
	    (exit-minibuffer)))))

(define-key minibuffer-local-completion-map [prior]
  'switch-to-completions)
(define-key minibuffer-local-must-match-map [prior]
  'switch-to-completions)
(define-key minibuffer-local-completion-map "\M-v"
  'advertised-switch-to-completions)
(define-key minibuffer-local-must-match-map "\M-v"
  'advertised-switch-to-completions)

(defalias 'advertised-switch-to-completions 'switch-to-completions)
(defun switch-to-completions ()
  "Select the completion list window."
  (interactive)
  ;; Make sure we have a completions window.
  (or (get-buffer-window "*Completions*")
      (minibuffer-completion-help))
  (if (not (get-buffer-window "*Completions*"))
      nil
    (select-window (get-buffer-window "*Completions*"))
    (goto-char (next-single-property-change (point-min) 'list-mode-item nil
					    (point-max)))))

;;; list-mode.el ends here
