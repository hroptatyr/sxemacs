;;; mode-motion.el --- Mode-specific mouse-highlighting of text.

;; Copyright (C) 1992, 1993, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
;; Keywords: internal, mouse, dumped

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs (when window system support is compiled in).

;;; Code:

(defvar mode-motion-hook nil
  "Function or functions which are called whenever the mouse moves.
Each function must take a single argument of the motion event.
You should normally use this rather than `mouse-motion-handler', which
does some additional window-system-dependent things.  This hook is local
to every buffer, and should normally be set up by major-modes which want
to use special highlighting.  Every time the mouse moves over a window,
the mode-motion-hook of the buffer of that window is run.")

(make-variable-buffer-local 'mode-motion-hook)

(defvar mode-motion-extent nil)
(make-variable-buffer-local 'mode-motion-extent)

(defvar mode-motion-help-echo-string nil
  "String to be added as the 'help-echo property of the mode-motion extent.
In order for this to work, you need to add the hook function
`mode-motion-add-help-echo' to the mode-motion hook.  If this is a function,
it will be called with one argument (the event) and should return a string
to be added.  This variable is local to every buffer.")
(make-variable-buffer-local 'mode-motion-help-echo-string)

(defun mode-motion-ensure-extent-ok (event)
  (let ((buffer (event-buffer event)))
    (if (and (extent-live-p mode-motion-extent)
	     (eq buffer (extent-object mode-motion-extent)))
	nil
      (setq mode-motion-extent (make-extent nil nil buffer))
      (set-extent-property mode-motion-extent 'mouse-face 'highlight))))

(defun mode-motion-highlight-internal (event backward forward)
  (let* ((buffer (event-buffer event))
	 (point (and buffer (event-point event))))
    (if (and buffer
	     (not (eq buffer mouse-grabbed-buffer)))
	;; #### ack!! Too many calls to save-window-excursion /
	;; save-excursion (x-track-pointer calls, so does
	;; minibuf-mouse-tracker ...) This needs to be looked
	;; into.  It's complicated by the fact that sometimes
	;; a mode-motion-hook might really want to change
	;; the point.
	;;
	;; #### The save-excursion must come before the
	;; save-window-excursion in order to function properly.  I
	;; haven't given this much thought.  Is it a bug that this
	;; ordering is necessary or is it correct behavior?
	(save-excursion
	  (save-window-excursion
	    (set-buffer buffer)
	    (mode-motion-ensure-extent-ok event)
	    (if point
		;; Use save-excursion here to avoid
		;; save-window-excursion seeing a change in
		;; window point's value which would make the
		;; display code do a whole lot of useless work
		;; and making the display flicker horribly.
		(save-excursion
		  (goto-char point)
		  (condition-case nil (funcall backward) (error nil))
		  (setq point (point))
		  (condition-case nil (funcall forward) (error nil))
		  (if (eq point (point))
		      (detach-extent mode-motion-extent)
		    (set-extent-endpoints mode-motion-extent point (point))))
	      ;; not over text; zero the extent.
	      (detach-extent mode-motion-extent)))))))

(defun mode-motion-highlight-line (event)
  "For use as the value of `mode-motion-hook' -- highlight line under mouse."
  (mode-motion-highlight-internal event 'beginning-of-line 'end-of-line))

(defun mode-motion-highlight-word (event)
  "For use as the value of `mode-motion-hook' -- highlight word under mouse."
  (mode-motion-highlight-internal
   event
   #'(lambda () (default-mouse-track-beginning-of-word nil))
   #'(lambda () (default-mouse-track-end-of-word nil))))

(defun mode-motion-highlight-symbol (event)
  "For use as the value of `mode-motion-hook' -- highlight symbol under mouse."
  (mode-motion-highlight-internal
   event
   #'(lambda () (default-mouse-track-beginning-of-word t))
   #'(lambda () (default-mouse-track-end-of-word t))))

(defun mode-motion-highlight-sexp (event)
  "For use as the value of `mode-motion-hook' -- highlight form under mouse."
  (mode-motion-highlight-internal
   event
   #'(lambda ()
       (if (= (char-syntax (following-char)) ?\()
	   nil
	 (goto-char (scan-sexps (point) -1))))
   #'(lambda ()
       (if (= (char-syntax (following-char)) ?\))
	   (forward-char 1))
       (goto-char (scan-sexps (point) 1)))))

(defun mode-motion-add-help-echo (event)
  "For use as the value of `mode-motion-hook' -- add a 'help-echo property.
This causes the string in the 'help-echo property to be displayed when the
mouse moves over the extent.  See `mode-motion-help-echo-string' for
documentation on how to control the string that is added."
  (mode-motion-ensure-extent-ok event)
  (let ((string (cond ((null mode-motion-help-echo-string) nil)
		      ((stringp mode-motion-help-echo-string)
		       mode-motion-help-echo-string)
		      (t (funcall mode-motion-help-echo-string event)))))
    (if (stringp string)
	(set-extent-property mode-motion-extent 'help-echo string))))


(provide 'mode-motion)

;;; mode-motion.el ends here
