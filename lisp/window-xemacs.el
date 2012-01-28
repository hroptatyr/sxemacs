;;; window-xemacs.el --- SXEmacs window commands aside from those written in C.

;; Copyright (C) 1985, 1989, 1993-94, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: frames, extensions, dumped

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

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; slb - 5/29/97
;; Split apart from window.el in order to keep that file better in synch
;; with Emacs.

;;; Code:

(defgroup windows nil
  "Windows within a frame."
  :group 'environment)

(defun recenter (&optional n window)
  "Center point in WINDOW and redisplay frame.  With N, put point on line N.
The desired position of point is always relative to the window.
Just C-u as prefix means put point in the center of the window.
No N (i.e., it is nil) erases the entire frame and then
redraws with point in the center of the window.
If WINDOW is nil, the selected window is used."
  (interactive "_P")
  (center-to-window-line (if (consp n) nil n) window)
  (when (null n)
    (redraw-frame (window-frame window) t)))

(defun backward-other-window (count &optional which-frames which-devices)
  "Select the COUNT'th different window on this frame, going backwards.
This is just like calling `other-window' with COUNT negated."
  (interactive "p")
  (other-window (- count) which-frames which-devices))

(defalias 'windows-of-buffer 'get-buffer-window-list)

(defun buffer-in-multiple-windows-p (&optional buffer)
  "Return t if BUFFER is in multiple windows.
If BUFFER is not specified, the current buffer will be used."
  (setq buffer (or buffer
		   (get-buffer buffer)
		   (get-file-buffer buffer)
		   (current-buffer)))
  (> (length (windows-of-buffer buffer)) 1))

(defun window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, beginning with WINDOW.
FRAME and WINDOW default to the selected ones.
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active."
  (setq window (or window (selected-window))
	frame (or frame (selected-frame)))
  (if (not (eq (window-frame window) frame))
      (error "Window must be on frame."))
  (let ((current-frame (selected-frame))
	list)
    (unwind-protect
	(save-window-excursion
	  (select-frame frame)
	  (walk-windows
	   (function (lambda (cur-window)
		       (if (not (eq window cur-window))
			   (setq list (cons cur-window list)))))
	   minibuf)
	  (setq list (cons window list)))
      (select-frame current-frame))))

;; We used to have set-window-dedicated-p as an obsolete version
;; of set-window-buffer-dedicated, but it really makes more sense
;; this way.

(make-obsolete 'set-window-buffer-dedicated 'set-window-dedicated-p)
(defun set-window-buffer-dedicated (window buffer)
  "Make WINDOW display BUFFER and be dedicated to that buffer.
Then Emacs will not automatically change which buffer appears in WINDOW.
If BUFFER is nil, make WINDOW not be dedicated (but don't change which
buffer appears in it currently)."
  (if (bufferp buffer)
      (set-window-buffer window (get-buffer-create buffer)))
  (set-window-dedicated-p window (not (null buffer))))


;; The window-config stack is stored as a list in frame property
;; 'window-config-stack, with the most recent element at the front.
;; When you pop off an element, the popped off element gets put at the
;; front of frame property 'window-config-unpop-stack, so you can
;; retrieve it using unpop-window-configuration.

(defcustom window-config-stack-max 16
  "*Maximum size of window configuration stack.
Start discarding off end if it gets this big."
  :type 'integer
  :group 'windows)

(defun window-config-stack (&optional frame)
  (or frame (setq frame (selected-frame)))
  (let ((stack (frame-property frame 'window-config-stack)))
    (if stack
	(set-undoable-stack-max stack window-config-stack-max)
      (progn
	(setq stack (make-undoable-stack window-config-stack-max))
	(set-frame-property frame 'window-config-stack stack)))
    stack))

(defun push-window-configuration (&optional config)
  "Push the current window configuration onto the window-config stack.
If CONFIG is specified, push it instead of the current window configuration.
Each frame has its own window-config stack."
  (interactive)
  (let ((wc (or config (current-window-configuration)))
	(stack (window-config-stack)))
    (if (or (= 0 (undoable-stack-a-length stack))
	    (not (equal (undoable-stack-a-top stack) wc)))
	(undoable-stack-push stack wc))))

(defun pop-window-configuration ()
  "Pop the top window configuration off the window-config stack and set it.
Before setting the new window configuration, the current window configuration
 is pushed onto the \"unpop\" stack.
`unpop-window-configuration' undoes what this function does.
Each frame has its own window-config and \"unpop\" stack."
  (interactive)
  (let ((stack (window-config-stack))
	(wc (current-window-configuration))
	popped)
    (condition-case nil
	(progn
	  (setq popped (undoable-stack-pop stack))
	  (while (equal popped wc)
	    (setq popped (undoable-stack-pop stack)))
	  (undoable-stack-push stack wc)
	  (undoable-stack-undo stack)
	  (set-window-configuration popped)
	  popped)
      (trunc-stack-bottom
       (error "Bottom of window config stack")))))

(defun unpop-window-configuration ()
  "Undo the effect of the most recent `pop-window-configuration'.
This does exactly the inverse of what `pop-window-configuration' does:
 i.e. it pops a window configuration off of the \"unpop\" stack and
 pushes the current window configuration onto the window-config stack.
Each frame has its own window-config and \"unpop\" stack."
  (interactive)
  (let ((stack (window-config-stack))
	(wc (current-window-configuration))
	popped)
    (condition-case nil
	(progn
	  (setq popped
		(progn
		  (undoable-stack-redo stack)
		  (undoable-stack-pop stack)))
	  (while (equal popped wc)
	    (setq popped
		  (progn
		    (undoable-stack-redo stack)
		    (undoable-stack-pop stack))))
	  (undoable-stack-push stack wc)
	  (set-window-configuration popped)
	  popped)
      (trunc-stack-bottom
       (error "Top of window config stack")))))


;;;;;;;;;;;;; display-buffer, moved here from C.  Hallelujah.

(make-variable-buffer-local '__buffer-dedicated-frame)

(defun buffer-dedicated-frame (&optional buffer)
  "Return the frame dedicated to this BUFFER, or nil if there is none.
No argument or nil as argument means use current buffer as BUFFER."
  (let ((buffer (decode-buffer buffer)))
    (let ((frame (symbol-value-in-buffer '__buffer-dedicated-frame buffer)))
      ;; XEmacs addition: if the frame is dead, silently make it go away.
      (when (and (framep frame) (not (frame-live-p frame)))
	    (with-current-buffer buffer
	      (setq __buffer-dedicated-frame nil))
	    (setq frame nil))
      frame)))

(defun set-buffer-dedicated-frame (buffer frame)
  "For this BUFFER, set the FRAME dedicated to it.
FRAME must be a frame or nil."
  (let ((buffer (decode-buffer buffer)))
    (and frame
	 (check-argument-type #'frame-live-p frame))
    (with-current-buffer buffer
      (setq __buffer-dedicated-frame frame))))

(defvar display-buffer-function nil
  "If non-nil, function to call to handle `display-buffer'.
It will receive four args: the same as those to `display-buffer'.")

(defvar pre-display-buffer-function nil
  "If non-nil, function that will be called from `display-buffer'
as the first action.  It will receive four args: the same as those
to `display-buffer'.
This function may be used to select an appropriate frame for the buffer,
for example.  See also the variable `display-buffer-function', which may
be used to completely replace the `display-buffer' function.
If the return value of this function is non-nil, it should be a frame,
and that frame will be used to display the buffer.")

(defcustom pop-up-frames nil
  "*Non-nil means `display-buffer' should make a separate frame."
  :type 'boolean
  :group 'frames)

(defvar pop-up-frame-function nil
  "Function to call to handle automatic new frame creation.
It is called with no arguments and should return a newly created frame.

A typical value might be `(lambda () (new-frame pop-up-frame-alist))'
where `pop-up-frame-alist' would hold the default frame parameters.")

(defcustom special-display-buffer-names nil
  "*List of buffer names that should have their own special frames.
Displaying a buffer whose name is in this list makes a special frame for it
using `special-display-function'.

An element of the list can be a cons cell instead of just a string.
Then the car should be a buffer name, and the cdr specifies frame
parameters for creating the frame for that buffer.
More precisely, the cdr is passed as the second argument to
the function found in `special-display-function', when making that frame.
See also `special-display-regexps'."
  :type '(repeat (choice :value ""
			 (string :tag "Name")
			 (cons :menu-tag "Properties"
			       :value ("" . nil)
			       (string :tag "Name")
			       (repeat :tag "Properties"
				       (group :inline t
					      (symbol :tag "Property")
					      (sexp :tag "Value"))))))
  :group 'frames)

(defcustom special-display-regexps nil
  "*List of regexps saying which buffers should have their own special frames.
If a buffer name matches one of these regexps, it gets its own frame.
Displaying a buffer whose name is in this list makes a special frame for it
using `special-display-function'.

An element of the list can be a cons cell instead of just a string.
Then the car should be the regexp, and the cdr specifies frame
parameters for creating the frame for buffers that match.
More precisely, the cdr is passed as the second argument to
the function found in `special-display-function', when making that frame.
See also `special-display-buffer-names'."
  :type '(repeat (choice :value ""
			 regexp
			 (cons :menu-tag "Properties"
			       :value ("" . nil)
			       regexp
			       (repeat :tag "Properties"
				       (group :inline t
					      (symbol :tag "Property")
					      (sexp :tag "Value"))))))
  :group 'frames)

(defvar special-display-function nil
  "Function to call to make a new frame for a special buffer.
It is called with two arguments, the buffer and optional buffer specific
data, and should return a window displaying that buffer.
The default value makes a separate frame for the buffer,
using `special-display-frame-alist' to specify the frame parameters.

A buffer is special if its is listed in `special-display-buffer-names'
or matches a regexp in `special-display-regexps'.")

(defcustom same-window-buffer-names nil
  "*List of buffer names that should appear in the selected window.
Displaying one of these buffers using `display-buffer' or `pop-to-buffer'
switches to it in the selected window, rather than making it appear
in some other window.

An element of the list can be a cons cell instead of just a string.
Then the car must be a string, which specifies the buffer name.
This is for compatibility with `special-display-buffer-names';
the cdr of the cons cell is ignored.

See also `same-window-regexps'."
  :type '(repeat (string :tag "Name"))
  :group 'windows)

(defcustom same-window-regexps nil
  "*List of regexps saying which buffers should appear in the selected window.
If a buffer name matches one of these regexps, then displaying it
using `display-buffer' or `pop-to-buffer' switches to it
in the selected window, rather than making it appear in some other window.

An element of the list can be a cons cell instead of just a string.
Then the car must be a string, which specifies the buffer name.
This is for compatibility with `special-display-buffer-names';
the cdr of the cons cell is ignored.

See also `same-window-buffer-names'."
  :type '(repeat regexp)
  :group 'windows)

(defcustom pop-up-windows t
  "*Non-nil means display-buffer should make new windows."
  :type 'boolean
  :group 'windows)

(defcustom split-height-threshold 500
 "*display-buffer would prefer to split the largest window if this large.
If there is only one window, it is split regardless of this value."
 :type 'integer
 :group 'windows)

(defcustom split-width-threshold 500
  "*display-buffer would prefer to split the largest window if this large.
If there is only one window, it is split regardless of this value."
  :type 'integer
  :group 'windows)

;; Deiconify the frame containing the window WINDOW, then return WINDOW.

(defun display-buffer-1 (window)
  (if (frame-iconified-p (window-frame window))
      (make-frame-visible (window-frame window)))
  window)

;; Can you believe that all of this crap was formerly in C?
;; Praise Jesus that it's not there any more.

(defun display-buffer (buffer &optional not-this-window-p override-frame
			      shrink-to-fit)
  "Make BUFFER appear in some window on the current frame, but don't select it.
BUFFER can be a buffer or a buffer name.
If BUFFER is shown already in some window in the current frame,
just uses that one, unless the window is the selected window and
NOT-THIS-WINDOW-P is non-nil (interactively, with prefix arg).

If BUFFER has a dedicated frame, display on that frame instead of
the current frame, unless OVERRIDE-FRAME is non-nil.

If OVERRIDE-FRAME is non-nil, display on that frame instead of
the current frame (or the dedicated frame).

If SHRINK-TO-FIT is non-nil and splitting the window is appropriate, give
the new buffer less than half the space if it is small enough to fit.

If `pop-up-windows' is non-nil, always use the
current frame and create a new window regardless of whether the
buffer has a dedicated frame, and regardless of whether
OVERRIDE-FRAME was specified.

If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.

If the buffer name is a member of the `same-window-buffer-names' list,
or matches one of the `same-window-regexps' expressions, display the
buffer in the currently selected window.

Returns the window displaying BUFFER."
  (interactive "BDisplay buffer:\nP")

  (let ((wconfig (current-window-configuration))
	(result
	 ;; We just simulate a `return' in C.  This function is way ugly
	 ;; and does `returns' all over the place and there's no sense
	 ;; in trying to rewrite it to be more Lispy.
	 (catch 'done
	   (let (window old-frame target-frame explicit-frame shrink-it)
	     (setq old-frame (or (last-nonminibuf-frame) (selected-frame)))
	     (setq buffer (get-buffer buffer))
	     (check-argument-type 'bufferp buffer)

	     (setq explicit-frame
		   (if pre-display-buffer-function
		       (funcall pre-display-buffer-function buffer
				not-this-window-p
				override-frame
				shrink-to-fit)))

	     ;; Give the user the ability to completely reimplement
	     ;; this function via the `display-buffer-function'.
	     (if display-buffer-function
		 (throw 'done
			(funcall display-buffer-function buffer
				 not-this-window-p
				 override-frame
				 shrink-to-fit)))

	     ;; If the buffer has a dedicated frame, that takes
	     ;; precedence over the current frame, and over what the
	     ;; pre-display-buffer-function did.
	     (let ((dedi (buffer-dedicated-frame buffer)))
	       (if (frame-live-p dedi) (setq explicit-frame dedi)))

	     ;; if override-frame is supplied, that takes precedence over
	     ;; everything.  This is gonna look bad if the
	     ;; pre-display-buffer-function raised some other frame
	     ;; already.
	     (if override-frame
		 (progn
		   (check-argument-type 'frame-live-p override-frame)
		   (setq explicit-frame override-frame)))

	     (setq target-frame
		   (or explicit-frame
		       (last-nonminibuf-frame)
		       (selected-frame)))

	     ;; If we have switched frames, then set not-this-window-p
	     ;; to false.  Switching frames means that selected-window
	     ;; is no longer the same as it was on entry -- it's the
	     ;; selected-window of target_frame instead of old_frame,
	     ;; so it's a fine candidate for display.
	     (if (not (eq old-frame target-frame))
		 (setq not-this-window-p nil))

	     ;; if it's in the selected window, and that's ok, then we're done.
	     (if (and (not not-this-window-p)
		      (eq buffer (window-buffer (selected-window))))
		 (throw 'done (display-buffer-1 (selected-window))))

	     ;; See if the user has specified this buffer should appear
	     ;; in the selected window.

	     (if not-this-window-p
		 nil

	       (if (or (member (buffer-name buffer) same-window-buffer-names)
		       (assoc (buffer-name buffer) same-window-buffer-names))
		   (progn
		     (switch-to-buffer buffer)
		     (throw 'done (display-buffer-1 (selected-window)))))

	       (let ((tem same-window-regexps))
		 (while tem
		   (let ((car (car tem)))
		     (if (or
			  (and (stringp car)
			       (string-match car (buffer-name buffer)))
			  (and (consp car) (stringp (car car))
			       (string-match (car car) (buffer-name buffer))))
			 (progn
			   (switch-to-buffer buffer)
			   (throw 'done (display-buffer-1
					 (selected-window))))))
		   (setq tem (cdr tem)))))

	     ;; If pop-up-frames, look for a window showing BUFFER on
	     ;; any visible or iconified frame.  Otherwise search only
	     ;; the current frame.
	     (if (and (not explicit-frame)
		      (or pop-up-frames (not (last-nonminibuf-frame))))
		 (setq target-frame 0))

	     ;; Otherwise, find some window that it's already in, and
	     ;; return that, unless that window is the selected window
	     ;; and that isn't ok.  What a contorted mess!
	     (setq window (or (if (not explicit-frame)
				  ;; search the selected frame
				  ;; first if the user didn't
				  ;; specify an explicit frame.
				  (get-buffer-window buffer nil))
			      (get-buffer-window buffer target-frame)))
	     (if (and window
		      (or (not not-this-window-p)
			  (not (eq window (selected-window)))))
		 (throw 'done (display-buffer-1 window)))

	     ;; Certain buffer names get special handling.
	     (if special-display-function
		 (progn
		   (if (member (buffer-name buffer)
			       special-display-buffer-names)
		       (throw 'done (funcall special-display-function buffer)))

		   (let ((tem (assoc (buffer-name buffer)
				     special-display-buffer-names)))
		     (if tem
			 (throw 'done (funcall special-display-function
					       buffer (cdr tem)))))

		   (let ((tem special-display-regexps))
		     (while tem
		       (let ((car (car tem)))
			 (if (and (stringp car)
				  (string-match car (buffer-name buffer)))
			     (throw 'done
				    (funcall special-display-function buffer)))
			 (if (and (consp car)
				  (stringp (car car))
				  (string-match (car car)
						(buffer-name buffer)))
			     (throw 'done (funcall
					   special-display-function buffer
					   (cdr car)))))
		       (setq tem (cdr tem))))))

	     ;; If there are no frames open that have more than a minibuffer,
	     ;; we need to create a new frame.
	     (if (or pop-up-frames
		     (null (last-nonminibuf-frame)))
		 (progn
		   (setq window (frame-selected-window
				 (funcall pop-up-frame-function)))
		   (set-window-buffer window buffer)
		   (throw 'done (display-buffer-1 window))))

	     ;; Otherwise, make it be in some window, splitting if
	     ;; appropriate/possible.  Do not split a window if we are
	     ;; displaying the buffer in a different frame than that which
	     ;; was current when we were called.  (It is already in a
	     ;; different window by virtue of being in another frame.)
	     (if (or (and pop-up-windows (eq target-frame old-frame))
		     (eq 'only (frame-property (selected-frame) 'minibuffer))
		     ;; If the current frame is a special display frame,
		     ;; don't try to reuse its windows.
		     (window-dedicated-p (frame-root-window (selected-frame))))
		 (progn
		   (if (eq 'only (frame-property (selected-frame) 'minibuffer))
		       (setq target-frame (last-nonminibuf-frame)))

		   ;; Don't try to create a window if would get an error with
		   ;; height.
		   (if (< split-height-threshold (* 2 window-min-height))
		       (setq split-height-threshold (* 2 window-min-height)))

		   ;; Same with width.
		   (if (< split-width-threshold (* 2 window-min-width))
		       (setq split-width-threshold (* 2 window-min-width)))

		   ;; If the frame we would try to split cannot be split,
		   ;; try other frames.
		   (if (frame-property (if (null target-frame)
					   (selected-frame)
					 (last-nonminibuf-frame))
				       'unsplittable)
		       (setq window
			     ;; Try visible frames first.
			     (or (get-largest-window 'visible)
				 ;; If that didn't work, try iconified frames.
				 (get-largest-window 0)
				 (get-largest-window t)))
		     (setq window (get-largest-window target-frame)))

		   ;; If we got a tall enough full-width window that
		   ;; can be split, split it.
		   (if (and window
			    (not (frame-property (window-frame window)
						 'unsplittable))
			    (>= (window-height window) split-height-threshold)
			    (or (>= (window-width window)
				    split-width-threshold)
				(and (window-leftmost-p window)
				     (window-rightmost-p window))))
		       (setq window (split-window window))
		     (let (upper other)
		       (setq window (get-lru-window target-frame))
		       ;; If the LRU window is selected, and big enough,
		       ;; and can be split, split it.
		       (if (and window
				(not (frame-property (window-frame window)
						     'unsplittable))
				(or (eq window (selected-window))
				    (not (window-parent window)))
				(>= (window-height window)
				    (* 2 window-min-height)))
			   (setq window (split-window window)))
		       ;; If get-lru-window returned nil, try other approaches.
		       ;; Try visible frames first.
		       (or window
			   (setq window (or (get-largest-window 'visible)
					    ;; If that didn't work, try
					    ;; iconified frames.
					    (get-largest-window 0)
					    ;; Try invisible frames.
					    (get-largest-window t)
					    ;; As a last resort, make
					    ;; a new frame.
					    (frame-selected-window
					     (funcall
					      pop-up-frame-function)))))
		       ;; If window appears above or below another,
		       ;; even out their heights.
		       (if (window-previous-child window)
			   (setq other (window-previous-child window)
				 upper other))
		       (if (window-next-child window)
			   (setq other (window-next-child window)
				 upper window))
		       ;; Check that OTHER and WINDOW are vertically arrayed.
		       (if (and other
				(not (= (nth 1 (window-pixel-edges other))
					(nth 1 (window-pixel-edges window))))
				(> (window-pixel-height other)
				   (window-pixel-height window)))
			   (enlarge-window (- (/ (+ (window-height other)
						    (window-height window))
						 2)
					      (window-height upper))
					   nil upper))
		       ;; Klaus Berndl <klaus.berndl@sdm.de>: Only in
		       ;; this situation we shrink-to-fit but we can do
		       ;; this first after we have displayed buffer in
		       ;; window (s.b. (set-window-buffer window buffer))
		       (setq shrink-it shrink-to-fit))))

	       (setq window (get-lru-window target-frame)))

	     ;; Bring the window's previous buffer to the top of the MRU chain.
	     (if (window-buffer window)
		 (save-excursion
		   (save-selected-window
		     (select-window window)
		     (record-buffer (window-buffer window)))))

	     (set-window-buffer window buffer)

	     ;; Now window's previous buffer has been brought to the top
	     ;; of the MRU chain and window displays buffer - now we can
	     ;; shrink-to-fit if necessary
	     (if shrink-it
		 (shrink-window-if-larger-than-buffer window))

	     (display-buffer-1 window)))))
    (or (equal wconfig (current-window-configuration))
	(push-window-configuration wconfig))
    result))

;;; window-xemacs.el ends here
