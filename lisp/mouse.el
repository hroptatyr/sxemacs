;;; mouse.el --- window system-independent mouse support.

;; Copyright (C) 1988, 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems
;; Copyright (C) 1995, 1996, 2000 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: mouse, dumped

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

;;; Synched up with: Not synched with FSF.  Almost completely divergent.

;;; Commentary:

;; This file is dumped with SXEmacs (when window system support is compiled in).

;;; Authorship:

;; Probably originally derived from FSF 19 pre-release.
;; much hacked upon by Jamie Zawinski and crew, pre-1994.
;;   (only mouse-motion stuff currently remains from that era)
;; all mouse-track stuff completely rewritten by Ben Wing, 1995-1996.
;; mouse-eval-sexp and *-inside-extent-p from Stig, 1995.
;; vertical divider code c. 1998 from ?.

;;; Code:

(provide 'mouse)

(global-set-key 'button1 'mouse-track)
(global-set-key '(shift button1) 'mouse-track-adjust)
(global-set-key '(control button1) 'mouse-track-insert)
(global-set-key '(control shift button1) 'mouse-track-delete-and-insert)
(global-set-key '(meta button1) 'mouse-track-do-rectangle)
(global-set-key 'button2 'mouse-track)

(defgroup mouse nil
  "Window system-independent mouse support."
  :group 'editing)

(defcustom mouse-track-rectangle-p nil
  "*If true, then dragging out a region with the mouse selects rectangles
instead of simple start/end regions."
  :type 'boolean
  :group 'mouse)

(defcustom mouse-yank-at-point nil
  "*If non-nil, the function `mouse-yank' will yank text at the cursor location.
Otherwise, the cursor will be moved to the location of the pointer click before
text is inserted."
  :type 'boolean
  :group 'mouse)

(defcustom mouse-highlight-text 'context
  "*Choose the default double-click highlighting behavior.
If set to `context', double-click will highlight words when the mouse
 is at a word character, or a symbol if the mouse is at a symbol
 character.
If set to `word', double-click will always attempt to highlight a word.
If set to `symbol', double-click will always attempt to highlight a
 symbol (the default behavior in previous XEmacs versions)."
  :type '(choice (const context)
		 (const word)
		 (const symbol))
  :group 'mouse)

(defvar mouse-yank-function 'mouse-consolidated-yank
  "Function that is called upon by `mouse-yank' to actually insert text.")

(defun mouse-consolidated-yank ()
  "Insert the current selection or, if there is none under X insert
the X cutbuffer.  A mark is pushed, so that the inserted text lies
between point and mark."
  (interactive)
  (if (and (not (console-on-window-system-p))
	   (and (featurep 'gpm)
		(not (declare-boundp gpm-minor-mode))))
      (yank)
    (push-mark)
    (if (region-active-p)
	(if (consp zmacs-region-extent)
	    ;; pirated code from insert-rectangle in rect.el
	    ;; perhaps that code should be modified to handle a list of extents
	    ;; as the rectangle to be inserted?
	    (let ((lines zmacs-region-extent)
		  (insertcolumn (current-column))
		  (first t))
	      (push-mark)
	      (while lines
		(or first
		    (progn
		      (forward-line 1)
		      (or (bolp) (insert ?\n))
		      (move-to-column insertcolumn t)))
		(setq first nil)
		(insert (extent-string (car lines)))
		(setq lines (cdr lines))))
	  (insert (extent-string zmacs-region-extent)))
      (insert-selection t))))

(defun insert-selection (&optional check-cutbuffer-p move-point-event)
  "Insert the current selection into buffer at point."
  (interactive "P")
  ;; we fallback to the clipboard if the current selection is not existent
  (let ((text (or (get-selection-no-error 'PRIMARY   'UTF8_STRING)
		  (get-selection-no-error 'PRIMARY   'STRING)
		  (and check-cutbuffer-p (get-cutbuffer))
		  (get-selection-no-error 'CLIPBOARD 'UTF8_STRING)
		  (get-selection-no-error 'CLIPBOARD 'STRING)
		  (error "no selection: PRIMARY or CLIPBOARD")
		  )))
    (cond (move-point-event
	   (mouse-set-point move-point-event)
	   (push-mark (point)))
	  ((interactive-p)
	   (push-mark (point))))
    (insert text)
    ))


(defun mouse-select ()
  "Select Emacs window the mouse is on."
  (interactive "@"))

(defun mouse-delete-window ()
  "Delete the Emacs window the mouse is on."
  (interactive "@")
  (delete-window))

(defun mouse-keep-one-window ()
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (interactive "@")
  (delete-other-windows))

(defun mouse-select-and-split ()
  "Select Emacs window mouse is on, then split it vertically in half."
  (interactive "@")
  (split-window-vertically nil))

(defun mouse-set-point (event)
  "Select Emacs window mouse is on, and move point to mouse position."
  (interactive "@e")
  (let ((window (event-window event))
	(pos (event-point event))
	(close-pos (event-closest-point event)))
    (or window (error "not in a window"))
    (select-window window)
    (if (and pos (> pos 0))
	;; If the event was over a text char, it's easy.
	(goto-char (max (min pos (point-max)) (point-min)))
      (if (and close-pos (> close-pos 0))
	  (goto-char (max (min close-pos (point-max)) (point-min)))
	;; When the event occurs outside of the frame directly to the
	;; left or right of a modeline, close-point is nil, but
	;; event-over-modeline is also nil.  That will drop us to this
	;; point.  So instead of erroring, just return nil.
	nil))))

(defun mouse-yank (event)
  "Paste text with the mouse.
If the variable `mouse-yank-at-point' is nil, then pasting occurs at the
location of the click; otherwise, pasting occurs at the current cursor
location."
  (interactive "e")
  (and (not mouse-yank-at-point)
       (mouse-set-point event))
  (funcall mouse-yank-function))

(defun click-inside-extent-p (click extent)
  "Return non-nil if the button event is within the primary selection-extent.
Return nil otherwise."
  (let ((ewin (event-window click))
	(epnt (event-point click)))
    (and ewin
	 epnt
	 extent
	 (eq (window-buffer ewin)
	     (extent-object extent))
	 (extent-start-position extent)
	 (> epnt (extent-start-position extent))
	 (> (extent-end-position extent) epnt))))

(defun click-inside-selection-p (click)
  (or (click-inside-extent-p click primary-selection-extent)
      (click-inside-extent-p click zmacs-region-extent)
      ))

(defun point-inside-extent-p (extent)
  "Return t if point is within the bounds of the primary selection extent.
Return t is point is at the end position of the extent.
Return nil otherwise."
  (and extent
       (eq (current-buffer)
	   (extent-object extent))
       (> (point) (extent-start-position extent))
       (>= (extent-end-position extent) (point))))

(defun point-inside-selection-p ()
  (or (point-inside-extent-p primary-selection-extent)
      (point-inside-extent-p zmacs-region-extent)))

(defun mouse-eval-sexp (click force-window)
  "Evaluate the sexp under the mouse.  Usually, this is the last sexp before
the click, but if you click on a left paren, then it is the sexp beginning
with the paren that is evaluated.  Also, since strings evaluate to themselves,
they're fed to `re-search-forward' and the matched region is highlighted until
the mouse button is released.

Perhaps the most useful thing about this function is that the evaluation of
the expression which is clicked upon is relative not to the window where you
click, but to the current window and the current position of point.  Thus,
you can use `mouse-eval-sexp' to interactively test code that acts upon a
buffer...something you cannot do with the standard `eval-last-sexp' function.
It's also fantastic for debugging regular expressions."
  (interactive "e\nP")
  (let (exp val result-str)
    (setq exp (save-window-excursion
		(save-excursion
		  (mouse-set-point click)
		  (save-excursion
		    (or (looking-at "(") (forward-sexp -1))
		    (read (point-marker))))))
    (cond ((stringp exp)
	   (if (setq val (re-search-forward exp nil t))
	       (let* ((oo (make-extent (match-beginning 0) (match-end 0))))
		 (set-extent-face oo 'highlight)
		 (set-extent-priority oo 1000)
		 ;; wait for button release...
		 (setq unread-command-event (next-command-event))
		 (delete-extent oo))
	     (message "Regex \"%s\" not found" exp)
	     (ding nil 'quiet)))
	  (t (setq val (if (fboundp 'eval-interactive)
			   (eval-interactive exp)
			 (eval exp)))))
    (setq result-str (prin1-to-string val))
    ;; #### -- need better test
    (if (and (not force-window)
	     (<= (length result-str) (window-width (selected-window))))
	(message "%s" result-str)
      (with-output-to-temp-buffer "*Mouse-Eval*"
	(condition-case nil
	    (declare-fboundp (pprint val))
	  (error (prin1 val))))
      )))

(defun mouse-line-length (event)
  "Print the length of the line indicated by the pointer."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (message "Line length: %d" (- (point-at-eol) (point-at-bol))))
  (sleep-for 1))

(defun mouse-set-mark (event)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save))))

(defun mouse-scroll (event)
  "Scroll point to the mouse position."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (recenter 0)
    (scroll-right (event-x event))))

(defun mouse-del-char (event)
  "Delete the char pointed to by the mouse."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (delete-char 1 nil)))

(defun mouse-kill-line (event)
  "Kill the line pointed to by the mouse."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (kill-line nil)))

(defun mouse-bury-buffer (event)
  "Bury the buffer pointed to by the mouse, thus selecting the next one."
  (interactive "e")
  (save-selected-window
    (select-window (event-window event))
    (bury-buffer)))

(defun mouse-unbury-buffer (event)
  "Unbury and select the most recently buried buffer."
  (interactive "e")
  (save-selected-window
    (select-window (event-window event))
    (let* ((bufs (buffer-list))
	   (entry (1- (length bufs)))
	   val)
      (while (not (setq val (nth entry bufs)
			val (and (/= (aref (buffer-name val) 0)
				     ? )
				 val)))
	(setq entry (1- entry)))
      (switch-to-buffer val))))

(defun narrow-window-to-region (m n)
  "Narrow window to region between point and last mark."
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (eq (selected-window) (next-window))
	  (split-window))
      (goto-char m)
      (recenter 0)
      (if (eq (selected-window)
	      (if (zerop (minibuffer-depth))
		  (next-window)))
	  ()
	(shrink-window (- (- (window-height) (count-lines m n)) 1))))))

(defun mouse-window-to-region (event)
  "Narrow window to region between cursor and mouse pointer."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save)
      (narrow-window-to-region (region-beginning) (region-end)))))

(defun mouse-ignore ()
  "Don't do anything."
  (interactive))


;;; mouse/selection tracking
;;; generalized mouse-track

(defvar default-mouse-track-normalize-point-function
  'default-mouse-track-normalize-point
  "Function called to normalize position of point.
Called with two arguments: TYPE depends on the number of times that the
mouse has been clicked and is a member of `default-mouse-track-type-list',
FORWARDP determines the direction in which the point should be moved.")

(defvar mouse-track-down-hook nil
  "Function or functions called when the user presses the mouse.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-press event and a click
count (see `mouse-track-click-hook').

If any function returns non-nil, the remaining functions will not be
called.

Note that most applications should take action when the mouse is
released, not when it is pressed.'")

(defvar mouse-track-drag-hook nil
  "Function or functions called when the user drags the mouse.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with three arguments: the mouse-motion event, a click
count (see `mouse-track-click-hook'), and whether the call to
this hook occurred as a result of a drag timeout (see
`mouse-track-scroll-delay').

If any function returns non-nil, the remaining functions will not be
called.

Note that no calls to this function will be made until the user
initiates a drag (i.e. moves the mouse more than a certain
threshold in either the X or the Y direction, as defined by
`mouse-track-x-threshold' and `mouse-track-y-threshold').

See also `mouse-track-drag-up-hook'.")

(defvar mouse-track-drag-up-hook nil
  "Function or functions called when the user finishes a drag.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-press event and a click
count (see `mouse-track-click-hook').

If any function returns non-nil, the remaining functions will not be
called.

Note that this hook will not be invoked unless the user has
initiated a drag, i.e. moved the mouse more than a certain threshold
(see `mouse-track-drag-hook').  When this function is invoked,
`mouse-track-drag-hook' will have been invoked at least once.

See also `mouse-track-click-hook'.")

(defvar mouse-track-click-hook nil
  "Function or functions called when the user clicks the mouse.
`Clicking' means pressing and releasing the mouse without having
initiated a drag (i.e. without having moved more than a certain
threshold -- see `mouse-track-drag-hook').

This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-release event and a click
count, which specifies the number of times that the mouse has been
clicked in a series of clicks, each of which is separated by at most
`mouse-track-multi-click-time'.  This can be used to implement actions
that are called on double clicks, triple clicks, etc.

If any function returns non-nil, the remaining functions will not be
called.

See also `mouse-track-drag-up-hook.")

(defvar mouse-track-up-hook nil
  "Function or functions called when the user releases the mouse.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-release event and a click
count (see `mouse-track-click-hook').

For many applications, it is more appropriate to use one or both
of `mouse-track-click-hook' and `mouse-track-drag-up-hook'.")

(defvar mouse-track-cleanup-hook nil
  "Function or functions called when `mouse-track' terminates.
This hook will be called in all circumstances, even upon a
non-local exit out of `mouse-track', and so is useful for
doing cleanup work such as removing extents that may have
been created during the operation of `mouse-track'.

Unlike all of the other mouse-track hooks, this is a \"normal\"
hook: the hook functions are called with no arguments, and
all hook functions are called regardless of their return
values.

This function is called with the buffer where the mouse was clicked
set to the current buffer, unless that buffer was killed.")

(defcustom mouse-track-multi-click-time 400
  "*Maximum number of milliseconds allowed between clicks for a multi-click.
See `mouse-track-click-hook'."
  :type 'integer
  :group 'mouse)

(defcustom mouse-track-scroll-delay 100
  "Maximum of milliseconds between calls to `mouse-track-drag-hook'.
If the user is dragging the mouse (i.e. the button is held down and
a drag has been initiated) and does not move the mouse for this many
milliseconds, the hook will be called with t as the value of the
WAS-TIMEOUT parameter.  This can be used to implement scrolling
in a selection when the user drags the mouse out the window it
was in.

A value of nil disables the timeout feature."
  :type '(choice integer (const :tag "Disabled" nil))
  :group 'mouse)

(defcustom mouse-track-activate-strokes '(button1-double-click button2-click)
  "List of mouse strokes that can cause \"activation\" of the text extent
under the mouse.  The exact meaning of \"activation\" is dependent on the
text clicked on and the mode of the buffer, but typically entails actions
such as following a hyperlink or selecting an entry in a completion buffer.

Possible list entries are

button1-click
button1-double-click
button1-triple-click
button1-down
button2-click
button2-double-click
button2-triple-click
button2-down

As a general rule, you should not use the \"-down\" values, because this
makes it impossible to have other simultaneous actions, such as selection."
  :type '(set
	  button1-click
	  button1-double-click
	  button1-triple-click
	  button1-down
	  button2-click
	  button2-double-click
	  button2-triple-click
	  button2-down)
  :group 'mouse)

(defvar mouse-track-x-threshold '(face-width 'default)
  "Minimum number of pixels in the X direction for a drag to be initiated.
If the mouse is moved more than either the X or Y threshold while the
button is held down (see also `mouse-track-y-threshold'), then a drag
is initiated; otherwise the gesture is considered to be a click.
See `mouse-track'.

The value should be either a number or a form to be evaluated to
produce a number.")

(defvar mouse-track-y-threshold '(face-height 'default)
  "Minimum number of pixels in the Y direction for a drag to be initiated.
If the mouse is moved more than either the X or Y threshold while the
button is held down (see also `mouse-track-x-threshold'), then a drag
is initiated; otherwise the gesture is considered to be a click.
See `mouse-track'.

The value should be either a number of a form to be evaluated to
produce a number.")

;; these variables are private to mouse-track.
(defvar mouse-track-up-time nil)
(defvar mouse-track-up-x nil)
(defvar mouse-track-up-y nil)
(defvar mouse-track-timeout-id nil)
(defvar mouse-track-click-count nil)

(defun mouse-track-set-timeout (event)
  (if mouse-track-timeout-id
      (disable-timeout mouse-track-timeout-id))
  (if mouse-track-scroll-delay
      (setq mouse-track-timeout-id
	    (add-timeout (/ mouse-track-scroll-delay 1000.0)
			 'mouse-track-scroll-undefined
			 (copy-event event)))))

(defun mouse-track-do-activate (event)
  "Execute the activate function under EVENT, if any.
Return true if the function was activated."
  (let ((ex (extent-at-event event 'activate-function)))
    (when ex
      (funcall (extent-property ex 'activate-function)
	       event ex)
      t)))

(defvar Mouse-track-gensym (gensym))

(defun mouse-track-run-hook (hook override event &rest args)
  ;; ugh, can't use run-hook-with-args-until-success because we have
  ;; to get the value using symbol-value-in-buffer.  Doing a
  ;; save-excursion/set-buffer is wrong because the hook might want to
  ;; change the buffer, but just doing a set-buffer is wrong because
  ;; the hook might not want to change the buffer.
  ;; #### What we need here is a Lisp interface to
  ;; run_hook_with_args_in_buffer.  Here is a poor man's version.
  (let ((overridden (plist-get override hook Mouse-track-gensym)))
    (if (not (eq overridden Mouse-track-gensym))
	(if (and (listp overridden) (not (eq (car overridden) 'lambda)))
	    (some #'(lambda (val) (apply val event args)) overridden)
	  (apply overridden event args))
      (let ((buffer (event-buffer event)))
	(and mouse-grabbed-buffer (setq buffer mouse-grabbed-buffer))
	(when buffer
	  (let ((value (symbol-value-in-buffer hook buffer nil)))
	    (if (and (listp value) (not (eq (car value) 'lambda)))
		;; List of functions.
		(let (retval)
		  (while (and value (null retval))
		    ;; Found `t': should process default value.  We could
		    ;; splice it into the buffer-local value, but that
		    ;; would cons, which is not a good thing for
		    ;; mouse-track hooks.
		    (if (eq (car value) t)
			(let ((global (default-value hook)))
			  (if (and (listp global) (not (eq (car global)
							   'lambda)))
			      ;; List of functions.
			      (while (and global
					  (null (setq retval
						      (apply (car global)
							     event args))))
				(pop global))
			    ;; lambda
			    (setq retval (apply (car global) event args))))
		      (setq retval (apply (car value) event args)))
		    (pop value))
		  retval)
	      ;; lambda
	      (apply value event args))))))))

(defun mouse-track-scroll-undefined (random)
  ;; the old implementation didn't actually define this function,
  ;; and in normal use it won't ever be called because the timeout
  ;; will either be removed before it fires or will be picked off
  ;; with next-event and not dispatched.  However, if you're
  ;; attempting to debug a click-hook (which is pretty damn
  ;; difficult to do), this function may get called.
)

(defun mouse-track (event &optional overriding-hooks)
  "Generalized mouse-button handler.  This should be bound to a mouse button.
The behavior of this function is customizable using various hooks and
variables: see `mouse-track-click-hook', `mouse-track-drag-hook',
`mouse-track-drag-up-hook', `mouse-track-down-hook', `mouse-track-up-hook',
`mouse-track-cleanup-hook', `mouse-track-multi-click-time',
`mouse-track-scroll-delay', `mouse-track-x-threshold', and
`mouse-track-y-threshold'.

Default handlers are provided to implement standard selecting/positioning
behavior.  You can explicitly request this default behavior, and override
any custom-supplied handlers, by using the function `mouse-track-default'
instead of `mouse-track'.

\(In general, you can override specific hooks by using the argument
OVERRIDING-HOOKS, which should be a plist of alternating hook names
and values.)

Default behavior is as follows:

If you click-and-drag, the selection will be set to the region between the
point of the initial click and the point at which you release the button.
These positions need not be ordered.

If you click-and-release without moving the mouse, then the point is moved
and the selection is disowned (there will be no selection owner).  The mark
will be set to the previous position of point.

If you double-click, the selection will extend by symbols instead of by
characters.  If you triple-click, the selection will extend by lines.

If you drag the mouse off the top or bottom of the window, you can select
pieces of text which are larger than the visible part of the buffer; the
buffer will scroll as necessary.

The selected text becomes the current X Selection.  The point will be left
at the position at which you released the button, and the mark will be left
at the initial click position."
  (interactive "e")
  (let ((mouse-down t)
	(xthresh (eval mouse-track-x-threshold))
	(ythresh (eval mouse-track-y-threshold))
	(orig-x (event-x-pixel event))
	(orig-y (event-y-pixel event))
	(buffer (event-buffer event))
	(mouse-grabbed-buffer (event-buffer event))
	mouse-moved)
    (if (or (not mouse-track-up-x)
	    (not mouse-track-up-y)
	    (not mouse-track-up-time)
	    (> (- (event-timestamp event) mouse-track-up-time)
	       mouse-track-multi-click-time)
	    (> (abs (- mouse-track-up-x orig-x)) xthresh)
	    (> (abs (- mouse-track-up-y orig-y)) ythresh))
	(setq mouse-track-click-count 1)
      (setq mouse-track-click-count (1+ mouse-track-click-count)))
    (if (not (event-window event))
	(error "Not over a window."))
    (mouse-track-run-hook 'mouse-track-down-hook overriding-hooks
			  event mouse-track-click-count)
    (unwind-protect
	(while mouse-down
	  (setq event (next-event event))
	  (cond ((motion-event-p event)
		 (if (and (not mouse-moved)
			  (or (> (abs (- (event-x-pixel event) orig-x))
				 xthresh)
			      (> (abs (- (event-y-pixel event) orig-y))
				 ythresh)))
		     (setq mouse-moved t))
		 (if mouse-moved
		     (mouse-track-run-hook 'mouse-track-drag-hook
					   overriding-hooks
					   event mouse-track-click-count nil))
		 (mouse-track-set-timeout event))
		((and (timeout-event-p event)
		      (eq (event-function event)
			  'mouse-track-scroll-undefined))
		 (if mouse-moved
		     (mouse-track-run-hook 'mouse-track-drag-hook
					   overriding-hooks
					   (event-object event)
					   mouse-track-click-count t))
		 (mouse-track-set-timeout (event-object event)))
		((button-release-event-p event)
		 (setq mouse-track-up-time (event-timestamp event))
		 (setq mouse-track-up-x (event-x-pixel event))
		 (setq mouse-track-up-y (event-y-pixel event))
		 (setq mouse-down nil)
		 (mouse-track-run-hook 'mouse-track-up-hook
				       overriding-hooks
				       event mouse-track-click-count)
		 (if mouse-moved
		     (mouse-track-run-hook 'mouse-track-drag-up-hook
					   overriding-hooks
					   event mouse-track-click-count)
		   (mouse-track-run-hook 'mouse-track-click-hook
					 overriding-hooks
					 event mouse-track-click-count)))
		((or (key-press-event-p event)
		     (and (misc-user-event-p event)
			  (eq (event-function event) 'cancel-mode-internal)))
		 (error "Selection aborted"))
		(t
		 (dispatch-event event))))
      ;; protected
      (if mouse-track-timeout-id
	  (disable-timeout mouse-track-timeout-id))
      (setq mouse-track-timeout-id nil)
      (and (buffer-live-p buffer)
	   (save-excursion
	     (set-buffer buffer)
	     (let ((override (plist-get overriding-hooks
					'mouse-track-cleanup-hook
					Mouse-track-gensym)))
	       (if (not (eq override Mouse-track-gensym))
		   (if (and (listp override) (not (eq (car override) 'lambda)))
		       (mapc #'funcall override)
		     (funcall override))
		 (run-hooks 'mouse-track-cleanup-hook))))))))


;;;;;;;;;;;; default handlers: new version of mouse-track

(defvar default-mouse-track-type nil)
(defvar default-mouse-track-type-list '(char word line))
(defvar default-mouse-track-window nil)
(defvar default-mouse-track-extent nil)
(defvar default-mouse-track-adjust nil)
(defvar default-mouse-track-min-anchor nil)
(defvar default-mouse-track-max-anchor nil)
(defvar default-mouse-track-result nil)
(defvar default-mouse-track-down-event nil)

;; D. Verna Feb. 17 1998
;; This function used to assume that when (event-window event) differs from
;; window, we have to scroll. This is WRONG, for instance when there are
;; toolbars on the side, in which case window-event returns nil.
(defun default-mouse-track-set-point-in-window (event window)
  (if (event-over-modeline-p event)
      nil ;; Scroll
    ;; Not over a modeline
    (if (eq (event-window event) window)
	(let ((p (event-closest-point event)))
	  (if  (or (not p) (not (pos-visible-in-window-p p window)))
	      nil ;; Scroll
	    (mouse-set-point event)
	    t))
      ;; Not over a modeline, not the same window. Check if the Y position
      ;; is still overlapping the original window.
      (let* ((edges (window-pixel-edges window))
	     (row (event-y-pixel event))
	     (text-start (nth 1 edges))
	     (text-end (nth 3 edges)))
	(if (or (< row text-start)
		(> row text-end))
	    nil ;; Scroll
	  ;; The Y pos in overlapping the original window. Check however if
	  ;; the position is really visible, because there could be a
	  ;; scrollbar or a modeline at this place.
	  ;; Find the mean line height (height / lines nb), and approximate
	  ;; the line number for Y pos.
	  (select-window window)
	  (let ((line (/ (* (- row text-start) (window-height))
			 (- text-end text-start))))
	    (if (not (save-excursion
		       (goto-char (window-start))
		       (pos-visible-in-window-p
			(point-at-bol (+ 1 line)))))
		nil ;; Scroll
	      ;; OK, we can go to that position
	      (goto-char (window-start))
	      (forward-line line)
	      ;; On the right side: go to end-of-line.
	      (when (>= (event-x-pixel event) (nth 2 edges))
		(goto-char (point-at-eol)))
	      t))))
      )))


(defun default-mouse-track-scroll-and-set-point (event window)
  (select-window window)
  (let ((edges (window-pixel-edges window))
	(row (event-y-pixel event))
	(height (face-height 'default)))
    (cond ((< (abs (- row (nth 1 edges))) (abs (- row (nth 3 edges))))
	   ;; closer to window's top than to bottom, so move up
	   (let ((delta (max 1 (/ (- (nth 1 edges) row) height))))
	     (condition-case () (scroll-down delta) (error))
	     (goto-char (window-start))))
	  ((>= (point) (point-max)))
	  (t
	   ;; scroll by one line if over the modeline or a clipped line
	   (let ((delta (if (or (event-over-modeline-p event)
				(< row (nth 3 edges)))
			    1
			  (+ (/ (- row (nth 3 edges)) height) 1)))
		 (close-pos (event-closest-point event)))
	     (condition-case () (scroll-up delta) (error))
	     (if (and close-pos (pos-visible-in-window-p close-pos))
		 (goto-char close-pos)
	       (goto-char (window-end))
	       (vertical-motion delta)
	       ;; window-end reports the end of the clipped line, even if
	       ;; scroll-on-clipped-lines is t.  compensate.
	       ;; (If window-end gets fixed this can be removed.)
	       (if (not (pos-visible-in-window-p (max (1- (point))
						      (point-min))))
		   (vertical-motion -1))
	       (condition-case () (backward-char 1)
		 (error (end-of-line)))))))))


;; This remembers the last position at which the user clicked, for the
;; benefit of mouse-track-adjust (for example, button1; scroll until the
;; position of the click is off the frame; then Sh-button1 to select the
;; new region.
(defvar default-mouse-track-previous-point nil)

(defun default-mouse-track-set-point (event window)
  (if (default-mouse-track-set-point-in-window event window)
      nil
    (default-mouse-track-scroll-and-set-point event window)))

(defsubst default-mouse-track-beginning-of-word (symbolp)
  (let ((word-constituent (cond ((eq symbolp t) #r"\w\|\s_\|\s'")
				((null symbolp) "\\w")
				(t "[^ \t\n]")))
	(white-space "[ \t]"))
    (cond ((bobp) nil)
	  ((looking-at word-constituent)
	   (backward-char)
	   (while (and (not (bobp)) (looking-at word-constituent))
	     (backward-char))
	   (if (or (not (bobp)) (not (looking-at word-constituent)))
	       (forward-char)))
	  ((looking-at white-space)
	   (backward-char)
	   (while (looking-at white-space)
	     (backward-char))
	   (forward-char)))))

(defun default-mouse-track-end-of-word (symbolp)
  (let ((word-constituent (cond ((eq symbolp t) #r"\w\|\s_\|\s'")
				((null symbolp) "\\w")
				(t "[^ \t\n]")))
	(white-space "[ \t]"))
    (cond ((looking-at word-constituent) ; word or symbol constituent
	   (while (looking-at word-constituent)
	     (forward-char)))
	  ((looking-at white-space) ; word or symbol constituent
	   (while (looking-at white-space)
	     (forward-char))))))

;; Decide what will be the SYMBOLP argument to
;; default-mouse-track-{beginning,end}-of-word, according to the
;; syntax of the current character and value of mouse-highlight-text.
(defsubst default-mouse-track-symbolp (syntax)
  (cond ((eq mouse-highlight-text 'context)
	 (eq syntax ?_))
	((eq mouse-highlight-text 'symbol)
	 t)
	(t
	 nil)))

;; Return t if point is at an opening quote character.  This is
;; determined by testing whether the syntax of the following character
;; is `string', which will always be true for opening quotes and
;; always false for closing quotes.
(defun default-mouse-track-point-at-opening-quote-p ()
  (save-excursion
    (forward-char 1)
    (eq (buffer-syntactic-context) 'string)))

(defun default-mouse-track-normalize-point (type forwardp)
  (cond ((eq type 'word)
	 ;; trap the beginning and end of buffer errors
	 (ignore-errors
	   (setq type (char-syntax (char-after (point))))
	   (if forwardp
	       (if (or (= type ?\()
		       (and (= type ?\")
			    (default-mouse-track-point-at-opening-quote-p)))
		   (goto-char (scan-sexps (point) 1))
		 (default-mouse-track-end-of-word
		   (default-mouse-track-symbolp type)))
	     (if (or (= type ?\))
		     (and (= type ?\")
			  (not (default-mouse-track-point-at-opening-quote-p))))
		 (goto-char (scan-sexps (1+ (point)) -1))
	       (default-mouse-track-beginning-of-word
		 (default-mouse-track-symbolp type))))))
	((eq type 'line)
	 (if forwardp (end-of-line) (beginning-of-line)))
	((eq type 'buffer)
	 (if forwardp (end-of-buffer) (beginning-of-buffer)))))

(defun default-mouse-track-next-move (min-anchor max-anchor extent)
  (let ((anchor (if (<= (point) min-anchor) max-anchor min-anchor)))
    (funcall default-mouse-track-normalize-point-function
	     default-mouse-track-type (> (point) anchor))
    (if (consp extent)
	(default-mouse-track-next-move-rect anchor (point) extent)
      (if extent
	  (if (<= anchor (point))
	      (set-extent-endpoints extent anchor (point))
	    (set-extent-endpoints extent (point) anchor))))))

(defun default-mouse-track-next-move-rect (start end extents &optional pad-p)
  (if (< end start)
      (let ((tmp start)) (setq start end end tmp)))
  (cond
   ((= start end)		; never delete the last remaining extent
    (mapcar 'delete-extent (cdr extents))
    (setcdr extents nil)
    (set-extent-endpoints (car extents) start start))
   (t
    (let ((indent-tabs-mode nil)	; if pad-p, don't use tabs
	  (rest extents)
	  left right last p)
      (save-excursion
	(save-restriction
	  (goto-char end)
	  (setq right (current-column))
	  (goto-char start)
	  (setq left (current-column))
	  (if (< right left)
	      (let ((tmp left))
		(setq left right right tmp)
		(setq start (- start (- right left))
		      end (+ end (- right left)))))
	  ;; End may have been set to a value greater than point-max if drag
	  ;; or movement extends to end of buffer, so reset it.
	  (setq end (min end (point-max)))
	  (beginning-of-line)
	  (narrow-to-region (point) end)
	  (goto-char start)
	  (while (and rest (not (eobp)))
	    (setq p (point))
	    (move-to-column right pad-p)
	    (set-extent-endpoints (car rest) p (point))
	    ;; this code used to look at the return value
	    ;; of forward-line, but that doesn't work because
	    ;; forward-line has bogus behavior: If you're on
	    ;; the last line of a buffer but not at the very
	    ;; end, forward-line will move you to the very
	    ;; end and return 0 instead of 1, like it should.
	    ;; the result was frequent infinite loops here,
	    ;; creating very large numbers of extents at
	    ;; the same position.  There was an N^2 sorting
	    ;; algorithm in extents.c for extents at a
	    ;; particular position, and the result was very
	    ;; bad news.
	    (forward-line 1)
	    (if (not (eobp))
		(move-to-column left pad-p))
	    (setq last rest
		  rest (cdr rest)))
	  (cond (rest
		 (mapcar 'delete-extent rest)
		 (setcdr last nil))
		((not (eobp))
		 (while (not (eobp))
		   (setq p (point))
		   (move-to-column right pad-p)
		   (let ((e (make-extent p (point))))
		     (set-extent-face e (extent-face (car extents)))
		     (set-extent-priority e (extent-priority (car extents)))
		     (setcdr last (cons e nil))
		     (setq last (cdr last)))
		   (forward-line 1)
		   (if (not (eobp))
		       (move-to-column left pad-p))
		   )))))
      ))))

(defun default-mouse-track-has-selection-p (buffer)
  (and (selection-owner-p)
       (extent-live-p primary-selection-extent)
       (not (extent-detached-p primary-selection-extent))
       (eq buffer (extent-object primary-selection-extent))))

(defun default-mouse-track-anchor (adjust previous-point)
  (if adjust
      (if (default-mouse-track-has-selection-p (current-buffer))
	  (let ((start (extent-start-position primary-selection-extent))
		(end (extent-end-position primary-selection-extent)))
	    (cond ((< (point) start) end)
		  ((> (point) end) start)
		  ((> (- (point) start) (- end (point))) start)
		  (t end)))
	previous-point)
    (point)))

(defun default-mouse-track-maybe-own-selection (pair type)
  (let ((start (car pair))
	(end (cdr pair)))
    (or (= start end) (push-mark (if (= (point) start) end start)))
    (cond (zmacs-regions
	   (if (= start end)
	       nil
	     ;; #### UTTER KLUDGE.
	     ;; If we don't have this sit-for here, then triple-clicking
	     ;; will result in the line not being highlighted as it
	     ;; should.  What appears to be happening is this:
	     ;;
	     ;; -- each time the button goes down, the selection is
	     ;;    disowned (see comment "remove the existing selection
	     ;;    to unclutter the display", below).
	     ;; -- this causes a SelectionClear event to be sent to
	     ;;    SXEmacs.
	     ;; -- each time the button goes up except the first, the
	     ;;    selection is owned again.
	     ;; -- later, SXEmacs processes the SelectionClear event.
	     ;;    The selection code attempts to keep track of the
	     ;;    time that it last asserted the selection, and
	     ;;    compare it to the time of the SelectionClear event,
	     ;;    to see if it's a bogus notification or not (as
	     ;;    is the case here).  However, for some unknown
	     ;;    reason this doesn't work in the triple-clicking
	     ;;    case, and the selection code bogusly thinks this
	     ;;    SelectionClear event is the real thing.
	     ;; -- putting the sit-for in causes the pending
	     ;;    SelectionClear events to get processed before
	     ;;    the selection is reasserted, so everything works
	     ;;    out OK.
	     ;;
	     ;; Presumably(?) this means there is a weird timing bug
	     ;; in the selection code, but there's not a chance in hell
	     ;; that I have the patience to track it down.  Blame the
	     ;; designers of X for fucking everything up so badly.
	     ;;
	     ;; This was originally a sit-for 0 but that wasn't
	     ;; sufficient to make things work.  Even this isn't
	     ;; always sufficient but it seems to give something
	     ;; approaching a 99% success rate.  Making it higher yet
	     ;; would help guarantee success with the price that the
	     ;; delay would start to become noticeable.
	     ;;
	     (and (eq (console-type) 'x)
		  (sit-for 0.15 t))
	     ;; zmacs-activate-region -> zmacs-activate-region-hook ->
	     ;; activate-region-as-selection -> either own-selection or
	     ;; mouse-track-activate-rectangular-selection
	     (zmacs-activate-region)))
	  ((console-on-window-system-p)
	   ;; #### do we need this?  we don't do it when zmacs-regions = t
	   (if (= start end)
	       (disown-selection type)
	     (activate-region-as-selection))))
    (if (and (eq 'x (console-type))
	     (not (= start end)))
	;; I guess cutbuffers should do something with rectangles too.
	;; does anybody use them?
	(x-store-cutbuffer (buffer-substring start end)))))

(defun mouse-track-activate-rectangular-selection ()
  (if (consp default-mouse-track-extent)
      ;; own the rectangular region
      ;; this is a hack
      (let ((r default-mouse-track-extent))
	(save-excursion
	  (set-buffer (get-buffer-create " *rect yank temp buf*"))
	  (erase-buffer)
	  (while r
	    (insert (extent-string (car r)) "\n")
	    (setq r (cdr r)))
	  (own-selection (buffer-substring (point-min) (point-max)))))))

(defun default-mouse-track-deal-with-down-event (click-count)
  (let ((event default-mouse-track-down-event))
    (if (null event) nil
      (select-frame (event-frame event))
      (let ((adjust default-mouse-track-adjust)
	    ;; ####When you click on the splash-screen,
	    ;; event-{closest-,}point can be out of bounds.  Should
	    ;; event-closest-point really be allowed to return a bad
	    ;; position like that?  Maybe pixel_to_glyph_translation
	    ;; needs to invalidate its cache when the buffer changes.
	    ;; -dkindred@cs.cmu.edu
	    (close-pos  (save-excursion
			  (set-buffer (event-buffer event))
			  (let ((p (event-closest-point event)))
			    (and p (min (max p (point-min)) (point-max))))))
	    extent previous-point)

	(if (not (event-window event))
	    (error "not over window?"))
	(setq default-mouse-track-type
	      (nth (mod (1- click-count)
			(length default-mouse-track-type-list))
		   default-mouse-track-type-list))
	(setq default-mouse-track-window (event-window event))
	;; Note that the extent used here is NOT the extent which
	;; ends up as the value of zmacs-region-extent - this one is used
	;; just during mouse-dragging.
	(setq default-mouse-track-extent
	      (make-extent close-pos close-pos (event-buffer event)))
	(setq extent default-mouse-track-extent)
	(set-extent-face extent 'zmacs-region)
	;; While the selection is being dragged out, give the selection extent
	;; slightly higher priority than any mouse-highlighted extent, so that
	;; the exact endpoints of the selection will be visible while the mouse
	;; is down.  Normally, the selection and mouse highlighting have the
	;; same priority, so that conflicts between the two of them are
	;; resolved by the usual size-and-endpoint-comparison method.
	(set-extent-priority extent (1+ mouse-highlight-priority))
	(if mouse-track-rectangle-p
	    (setq default-mouse-track-extent
		  (list default-mouse-track-extent)))

	(setq previous-point
	      (if (and adjust
		       (markerp default-mouse-track-previous-point)
		       (eq (current-buffer)
			   (marker-buffer default-mouse-track-previous-point)))
		  (marker-position default-mouse-track-previous-point)
		(point)))
	(default-mouse-track-set-point event default-mouse-track-window)
	(if (not adjust)
	    (if (markerp default-mouse-track-previous-point)
		(set-marker default-mouse-track-previous-point (point))
	      (setq default-mouse-track-previous-point (point-marker))))
	;;
	;; adjust point to a word or line boundary if appropriate
	(let ((anchor (default-mouse-track-anchor adjust previous-point)))
	  (setq default-mouse-track-min-anchor
		(save-excursion (goto-char anchor)
				(funcall
				 default-mouse-track-normalize-point-function
				 default-mouse-track-type nil)
				(point)))
	  (setq default-mouse-track-max-anchor
		(save-excursion (goto-char anchor)
				(funcall
				 default-mouse-track-normalize-point-function
				 default-mouse-track-type t)
				(point))))
	;;
	;; remove the existing selection to unclutter the display
	(if (not adjust)
	    (cond (zmacs-regions
		   (zmacs-deactivate-region))
		  ((console-on-window-system-p)
		   (disown-selection)))))
      (setq default-mouse-track-down-event nil))))

;; return t if the button or motion event involved the specified button.
(defun default-mouse-track-event-is-with-button (event n)
  (cond ((button-event-p event)
	 (= n (event-button event)))
	((motion-event-p event)
	 (memq (cdr
		(assq n '((1 . button1) (2 . button2) (3 . button3)
			  (4 . button4) (5 . button5))))
	       (event-modifiers event)))))

(defun default-mouse-track-down-hook (event click-count)
  (cond ((default-mouse-track-event-is-with-button event 1)
	 (if (and (memq 'button1-down mouse-track-activate-strokes)
		  (mouse-track-do-activate event))
	     t
	   (setq default-mouse-track-down-event (copy-event event))
	   nil))
	((default-mouse-track-event-is-with-button event 2)
	 (and (memq 'button2-down mouse-track-activate-strokes)
	      (mouse-track-do-activate event)))))

(defun default-mouse-track-cleanup-extents-hook ()
  (remove-hook 'pre-command-hook 'default-mouse-track-cleanup-extents-hook)
  (let ((extent default-mouse-track-extent))
    (if (consp extent) ; rectangle-p
	(mapcar 'delete-extent extent)
      (if extent
	  (delete-extent extent)))))

(defun default-mouse-track-cleanup-hook ()
  (if zmacs-regions
      (funcall 'default-mouse-track-cleanup-extents-hook)
    (let ((extent default-mouse-track-extent)
	  (func #'(lambda (e)
		    (and (extent-live-p e)
			 (set-extent-face e 'primary-selection)))))
      (add-hook 'pre-command-hook 'default-mouse-track-cleanup-extents-hook)
      (if (consp extent)		; rectangle-p
	  (mapcar func extent)
	(if extent
	    (funcall func extent)))))
  t)

(defun default-mouse-track-cleanup-extent ()
  (let ((dead-func
	 (function (lambda (x)
		     (or (not (extent-live-p x))
			 (extent-detached-p x)))))
	(extent default-mouse-track-extent))
    (if (consp extent)
	(if (funcall dead-func extent)
	    (let (newval)
	      (mapcar (function (lambda (x)
				  (if (not (funcall dead-func x))
				      (setq newval (cons x newval)))))
		      extent)
	      (setq default-mouse-track-extent (nreverse newval))))
      (if (funcall dead-func extent)
	  (setq default-mouse-track-extent nil)))))

(defun default-mouse-track-drag-hook (event click-count was-timeout)
  (cond ((default-mouse-track-event-is-with-button event 1)
	 (default-mouse-track-deal-with-down-event click-count)
	 (default-mouse-track-set-point event default-mouse-track-window)
	 (default-mouse-track-cleanup-extent)
	 (default-mouse-track-next-move default-mouse-track-min-anchor
	   default-mouse-track-max-anchor
	   default-mouse-track-extent)
	 t)
	;; we nuked dnd, so this kinda won't work anymore --SY.
	;((default-mouse-track-event-is-with-button event 2)
	; (mouse-begin-drag-n-drop event))))
	))

(defun default-mouse-track-return-dragged-selection (event)
  (default-mouse-track-cleanup-extent)
  (let ((extent default-mouse-track-extent)
	result)
    (default-mouse-track-set-point-in-window event default-mouse-track-window)
    (default-mouse-track-next-move default-mouse-track-min-anchor
			   default-mouse-track-max-anchor
			   extent)
    (cond ((consp extent) ; rectangle-p
	   (let ((first (car extent))
		 (last (car (setq extent (nreverse extent)))))
	     ;; nreverse is destructive so we need to reset this
	     (setq default-mouse-track-extent extent)
	     (setq result (cons (extent-start-position first)
				(extent-end-position last)))
	     ;; kludge to fix up region when dragging backwards...
	     (if (and (/= (point) (extent-start-position first))
		      (/= (point) (extent-end-position last))
		      (= (point) (extent-end-position first)))
		 (goto-char (car result)))))
	  (extent
	   (setq result (cons (extent-start-position extent)
			      (extent-end-position extent)))))
    ;; Minor kludge: if we're selecting in line-mode, include the
    ;; final newline.  It's hard to do this in *-normalize-point.
    (if (and result (eq default-mouse-track-type 'line))
	(let ((end-p (= (point) (cdr result))))
	  (goto-char (cdr result))
	  (if (not (eobp))
	      (setcdr result (1+ (cdr result))))
	  (goto-char (if end-p (cdr result) (car result)))))
;;;	  ;; Minor kludge sub 2.  If in char mode, and we drag the
;;;	  ;; mouse past EOL, include the newline.
;;;	  ;;
;;;	  ;; Major problem: can't easily distinguish between being
;;;	  ;; just past the last char on a line, and well past it,
;;;	  ;; to determine whether or not to include it in the region
;;;	  ;;
;;;	  (if nil ; (eq default-mouse-track-type 'char)
;;;	      (let ((after-end-p (and (not (eobp))
;;;				      (eolp)
;;;				      (> (point) (car result)))))
;;;		(if after-end-p
;;;		    (progn
;;;		      (setcdr result (1+ (cdr result)))
;;;		      (goto-char (cdr result))))))
    result))

(defun default-mouse-track-drag-up-hook (event click-count)
  (when (default-mouse-track-event-is-with-button event 1)
    (let ((result (default-mouse-track-return-dragged-selection event)))
      (if result
	  (default-mouse-track-maybe-own-selection result 'PRIMARY)))
    t))

(defun default-mouse-track-click-hook (event click-count)
  (cond ((default-mouse-track-event-is-with-button event 1)
	 (if (and
	      (or (and (= click-count 1)
		       (memq 'button1-click
			     mouse-track-activate-strokes))
		  (and (= click-count 2)
		       (memq 'button1-double-click
			     mouse-track-activate-strokes))
		  (and (= click-count 3)
		       (memq 'button1-triple-click
			     mouse-track-activate-strokes)))
	      (mouse-track-do-activate event))
	     t
	   (default-mouse-track-drag-hook event click-count nil)
	   (default-mouse-track-drag-up-hook event click-count)
	   t))
	((default-mouse-track-event-is-with-button event 2)
	 (if (and
	      (or (and (= click-count 1)
		       (memq 'button2-click
			     mouse-track-activate-strokes))
		  (and (= click-count 2)
		       (memq 'button2-double-click
			     mouse-track-activate-strokes))
		  (and (= click-count 3)
		       (memq 'button2-triple-click
			     mouse-track-activate-strokes)))
	      (mouse-track-do-activate event))
	     t
	   (mouse-yank event)
	   t))))


(add-hook 'mouse-track-down-hook 'default-mouse-track-down-hook)
(add-hook 'mouse-track-drag-hook 'default-mouse-track-drag-hook)
(add-hook 'mouse-track-drag-up-hook 'default-mouse-track-drag-up-hook)
(add-hook 'mouse-track-click-hook 'default-mouse-track-click-hook)
(add-hook 'mouse-track-cleanup-hook 'default-mouse-track-cleanup-hook)


;;;;;;;;;;;; other mouse-track stuff (mostly associated with the
;;;;;;;;;;;; default handlers)

(defun mouse-track-default (event)
  "Invoke `mouse-track' with only the default handlers active."
  (interactive "e")
  (mouse-track event
	       '(mouse-track-down-hook
		 default-mouse-track-down-hook
		 mouse-track-up-hook nil
		 mouse-track-drag-hook default-mouse-track-drag-hook
		 mouse-track-drag-up-hook default-mouse-track-drag-up-hook
		 mouse-track-click-hook default-mouse-track-click-hook
		 mouse-track-cleanup-hook default-mouse-track-cleanup-hook)))

(defun mouse-track-do-rectangle (event)
  "Like `mouse-track' but selects rectangles instead of regions."
  (interactive "e")
  (let ((mouse-track-rectangle-p t))
	(mouse-track event)))

(defun mouse-track-adjust (event)
  "Extend the existing selection.  This should be bound to a mouse button.
The selection will be enlarged or shrunk so that the point of the mouse
click is one of its endpoints.  This function in fact behaves fairly
similarly to `mouse-track', but begins by extending the existing selection
(or creating a new selection from the previous text cursor position to
the current mouse position) instead of creating a new, empty selection.

The mouse-track handlers are run from this command just like from
`mouse-track'.  Therefore, do not call this command from a mouse-track
handler!"
  (interactive "e")
  (let ((default-mouse-track-adjust t))
    (mouse-track event)))

(defun mouse-track-adjust-default (event)
  "Extend the existing selection, using only the default handlers.
This is just like `mouse-track-adjust' but will override any
custom mouse-track handlers that the user may have installed."
  (interactive "e")
  (let ((default-mouse-track-adjust t))
    (mouse-track-default event)))

(defun mouse-track-insert (event &optional delete)
  "Make a selection with the mouse and insert it at point.
This is exactly the same as the `mouse-track' command on \\[mouse-track],
except that point is not moved; the selected text is immediately inserted
after being selected\; and the selection is immediately disowned afterwards."
  (interactive "*e")
  (let (s selreg)
    (flet ((Mouse-track-insert-drag-up-hook (event count)
	     (setq selreg
		   (default-mouse-track-return-dragged-selection event))
	     t)
	   (Mouse-track-insert-click-hook (event count)
	     (default-mouse-track-drag-hook event count nil)
	     (setq selreg
		   (default-mouse-track-return-dragged-selection event))
	     t))
      (save-excursion
	(save-window-excursion
	  (mouse-track
	   event
	   '(mouse-track-drag-up-hook
	     Mouse-track-insert-drag-up-hook
	     mouse-track-click-hook
	     Mouse-track-insert-click-hook))
	  (if (consp selreg)
	      (let ((pair selreg))
		(setq s (prog1
			    (buffer-substring (car pair) (cdr pair))
			  (if delete
			      (kill-region (car pair) (cdr pair))))))))))
    (or (null s) (equal s "") (insert s))))

(defun mouse-track-delete-and-insert (event)
  "Make a selection with the mouse and insert it at point.
This is exactly the same as the `mouse-track' command on \\[mouse-track],
except that point is not moved; the selected text is immediately inserted
after being selected\; and the text of the selection is deleted."
  (interactive "*e")
  (mouse-track-insert event t))

;;;;;;;;;;;;;;;;;;;;;;;;


(defvar inhibit-help-echo nil
  "Inhibits display of `help-echo' extent properties in the minibuffer.")
(defvar last-help-echo-object nil)
(defvar help-echo-owns-message nil)

(defun clear-help-echo (&optional ignored-frame)
  (if help-echo-owns-message
      (progn
	(setq help-echo-owns-message nil
	      last-help-echo-object nil)
	(clear-message 'help-echo))))

(defun show-help-echo (mess)
  ;; (clear-help-echo)
  (setq help-echo-owns-message t)
  (display-message 'help-echo mess))

(add-hook 'mouse-leave-frame-hook 'clear-help-echo)

;; It may be a good idea to move this to C, for better performance of
;; extent highlighting and pointer changes.
(defun default-mouse-motion-handler (event)
  "For use as the value of `mouse-motion-handler'.
This implements the various pointer-shape variables,
as well as extent highlighting, help-echo, toolbar up/down,
and `mode-motion-hook'."
  (let* ((frame (or (event-frame event) (selected-frame)))
	 (window (event-window event))
	 (buffer (event-buffer event))
	 (modeline-point (and buffer (event-modeline-position event)))
	 (modeline-string (and modeline-point
			       (symbol-value-in-buffer
				'generated-modeline-string buffer)))
	 ;; point must be invalidated by modeline-point.
	 (point (and buffer (not modeline-point)
		     (event-point event)))
	 (extent (or (and point
			  (extent-at point buffer 'mouse-face))
		     (and modeline-point
			  (extent-at modeline-point modeline-string
				     ;; Modeline extents don't have a
				     ;; mouse-face property set.
				     'help-echo))))
	 (glyph-extent1 (event-glyph-extent event))
	 (glyph-extent (and glyph-extent1
			    (extent-live-p glyph-extent1)
			    glyph-extent1))
	 ;; This is an extent:
	 (user-pointer1 (or (and glyph-extent
				 (extent-property glyph-extent 'pointer)
				 glyph-extent)
			    (and point (extent-at point buffer 'pointer))
			    (and modeline-point
				 (extent-at modeline-point modeline-string
					    'pointer))))
	 ;; And this should be a glyph:
	 (user-pointer (and user-pointer1 (extent-live-p user-pointer1)
			    (extent-property user-pointer1 'pointer)))
	 (button (event-toolbar-button event))
	 (help (or (and glyph-extent (extent-property glyph-extent 'help-echo)
			glyph-extent)
		   (and button (not (null (toolbar-button-help-string button)))
			button)
		   (and point
			(extent-at point buffer 'help-echo))
		   (and modeline-point
			(extent-at modeline-point modeline-string
				   'help-echo))))
	 ;; vars is a list of glyph variables to check for a pointer
	 ;; value.
	 (vars (cond
		;; Checking if button is non-nil is not sufficient
		;; since the pointer could be over a blank portion
		;; of the toolbar.
		((event-over-toolbar-p event)
		 '(toolbar-pointer-glyph nontext-pointer-glyph
					 text-pointer-glyph))
		((or extent glyph-extent)
		 '(selection-pointer-glyph text-pointer-glyph))
		((event-over-modeline-p event)
		 '(modeline-pointer-glyph nontext-pointer-glyph
					  text-pointer-glyph))
		((and (event-over-vertical-divider-p event)
		      ;; #### I disagree with the check below.
		      ;; Discuss it with Kirill for 21.1.  --hniksic
		      (specifier-instance vertical-divider-always-visible-p
					  (event-window event)))
		 '(divider-pointer-glyph nontext-pointer-glyph
					 text-pointer-glyph))
		(point '(text-pointer-glyph))
		(buffer '(nontext-pointer-glyph text-pointer-glyph))
		(t '(nontext-pointer-glyph text-pointer-glyph))))
	 pointer)
    (and user-pointer (glyphp user-pointer)
	 (push 'user-pointer vars))
    (while (and vars (not (pointer-image-instance-p pointer)))
      (setq pointer (glyph-image-instance (symbol-value (car vars))
					  (or window frame))
	    vars (cdr vars)))

    (if (pointer-image-instance-p pointer)
	(set-frame-pointer frame pointer))

    ;; If last-pressed-toolbar-button is not nil, then check and see
    ;; if we have moved to a new button and adjust the down flags
    ;; accordingly.
    (when (and (featurep 'toolbar) toolbar-active)
      (unless (eq last-pressed-toolbar-button button)
	(release-previous-toolbar-button event)
	(and button (press-toolbar-button event))))

    (cond (extent (highlight-extent extent t))
	  (glyph-extent (highlight-extent glyph-extent t))
	  (t (highlight-extent nil nil)))
    (cond ((extentp help)
	   (or inhibit-help-echo
	       (eq help last-help-echo-object) ;save some time
	       (eq (selected-window) (minibuffer-window))
	       (let ((hprop (extent-property help 'help-echo)))
		 (setq last-help-echo-object help)
		 (or (stringp hprop)
		     (setq hprop (funcall hprop help)))
		 (and hprop (show-help-echo hprop)))))
	  ((and (featurep 'toolbar)
		(toolbar-button-p help)
		(toolbar-button-enabled-p help))
	   (or (not toolbar-help-enabled)
	       (eq help last-help-echo-object) ;save some time
	       (eq (selected-window) (minibuffer-window))
	       (let ((hstring (toolbar-button-help-string button)))
		 (setq last-help-echo-object help)
		 (or (stringp hstring)
		     (setq hstring (funcall hstring help)))
		 (and hstring (show-help-echo hstring)))))
	  (last-help-echo-object
	   (clear-help-echo)))
    (if mouse-grabbed-buffer (setq buffer mouse-grabbed-buffer))
    (if (and buffer (symbol-value-in-buffer 'mode-motion-hook buffer nil))
	(with-current-buffer buffer
	  (run-hook-with-args 'mode-motion-hook event)

	  ;; If the mode-motion-hook created a highlightable extent around
	  ;; the mouse-point, highlight it right away.  Otherwise it wouldn't
	  ;; be highlighted until the *next* motion event came in.
	  (if (and point
		   (null extent)
		   (setq extent (extent-at point
					   (event-buffer event) ; not buffer
					   'mouse-face)))
	      (highlight-extent extent t)))))
  nil)

(setq mouse-motion-handler 'default-mouse-motion-handler)

;;
;; Vertical divider dragging
;;
(defun drag-window-divider (event)
  "Handle resizing windows by dragging window dividers.
This is an internal function, normally bound to button1 event in
window-divider-map. You would not call it, but you may bind it to
other mouse buttons."
  (interactive "e")
  ;; #### I disagree with the check below.
  ;; Discuss it with Kirill for 21.1.  --hniksic
  (if (not (specifier-instance vertical-divider-always-visible-p
			       (event-window event)))
      (error "Not over a window"))
  (let-specifier ((vertical-divider-shadow-thickness
		   (- (specifier-instance vertical-divider-shadow-thickness
					  (event-window event)))
		   (event-window event)))
    (let* ((window (event-window event))
	   (frame (event-channel event))
	   (last-timestamp (event-timestamp event))
	   done)
      (while (not done)
	(let* ((edges (window-pixel-edges window))
	       (old-right (caddr edges))
	       (old-left (car edges))
	       (backup-conf (current-window-configuration frame))
	       (old-edges-all-windows (mapcar 'window-pixel-edges
					      (window-list))))

	  ;; This is borrowed from modeline.el:
	  ;; requeue event and quit if this is a misc-user, eval or
	  ;;   keypress event.
	  ;; quit if this is a button press or release event, or if the event
	  ;;   occurred in some other frame.
	  ;; drag if this is a mouse motion event and the time
	  ;;   between this event and the last event is greater than
	  ;;   drag-divider-event-lag.
	  ;; do nothing if this is any other kind of event.
	  (setq event (next-event event))
	  (cond ((or (misc-user-event-p event)
		     (key-press-event-p event))
		 (setq unread-command-events (nconc unread-command-events
						    (list event))
		       done t))
		((button-release-event-p event)
		 (setq done t))
		((button-event-p event)
		 (setq done t))
		((not (motion-event-p event))
		 (dispatch-event event))
		((not (eq frame (event-frame event)))
		 (setq done t))
		((< (abs (- (event-timestamp event) last-timestamp))
		    drag-divider-event-lag))
		(t
		 (setq last-timestamp (event-timestamp event))
		 ;; Enlarge the window, calculating change in characters
		 ;; of default font. Do not let the window to become
		 ;; less than allowed minimum (not because that's critical
		 ;; for the code performance, just the visual effect is
		 ;; better: when cursor goes to the left of the next left
		 ;; divider, the window being resized shrinks to minimal
		 ;; size.
		 (enlarge-window (max (- window-min-width (window-width window))
				      (/ (- (event-x-pixel event) old-right)
					 (face-width 'default window)))
				 t window)
		 ;; Backout the change if some windows got deleted, or
		 ;; if the change caused more than two windows to resize
		 ;; (shifting the whole stack right is ugly), or if the
		 ;; left window side has slipped (right side cannot be
		 ;; moved any further to the right, so enlarge-window
		 ;; plays bad games with the left edge.
		 (if (or (/= (count-windows) (length old-edges-all-windows))
			 (/= old-left (car (window-pixel-edges window)))
			 ;; This check is very hairy. We allow any number
			 ;; of left edges to change, but only to the same
			 ;; new value. Similar procedure is for the right edges.
			 (let ((all-that-bad nil)
			       (new-left-ok nil)
			       (new-right-ok nil))
			   (mapcar* (lambda (window old-edges)
				      (let ((new (car (window-pixel-edges window))))
					(if (/= new (car old-edges))
					    (if (and new-left-ok
						     (/= new-left-ok new))
						(setq all-that-bad t)
					      (setq new-left-ok new)))))
				    (window-list) old-edges-all-windows)
			   (mapcar* (lambda (window old-edges)
				      (let ((new (caddr (window-pixel-edges window))))
					(if (/= new (caddr old-edges))
					    (if (and new-right-ok
						     (/= new-right-ok new))
						(setq all-that-bad t)
					      (setq new-right-ok new)))))
				    (window-list) old-edges-all-windows)
			   all-that-bad))
		     (set-window-configuration backup-conf)))))))))

(setq vertical-divider-map (make-keymap))
(define-key vertical-divider-map 'button1 'drag-window-divider)

;;; mouse.el ends here
