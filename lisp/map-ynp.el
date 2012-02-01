;;; map-ynp.el --- General-purpose boolean question-asker.

;; Copyright (C) 1991-1995, 1997 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; Keywords: lisp, extensions, dumped

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

;;; Synched up with: Emacs/Mule zeta.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; map-y-or-n-p is a general-purpose question-asking function.
;; It asks a series of y/n questions (a la y-or-n-p), and decides to
;; applies an action to each element of a list based on the answer.
;; The nice thing is that you also get some other possible answers
;; to use, reminiscent of query-replace: ! to answer y to all remaining
;; questions; ESC or q to answer n to all remaining questions; . to answer
;; y once and then n for the remainder; and you can get help with C-h.

;;; Code:

(defun map-y-or-n-p (prompter actor list &optional help action-alist
			      no-cursor-in-echo-area)
  "Ask a series of boolean questions.
Takes args PROMPTER ACTOR LIST, and optional args HELP and ACTION-ALIST.

LIST is a list of objects, or a function of no arguments to return the next
object or nil.

If PROMPTER is a string, the prompt is \(format PROMPTER OBJECT\).  If not
a string, PROMPTER is a function of one arg (an object from LIST), which
returns a string to be used as the prompt for that object.  If the return
value is not a string, it may be nil to ignore the object or non-nil to act
on the object without asking the user.

ACTOR is a function of one arg (an object from LIST),
which gets called with each object that the user answers `yes' for.

If HELP is given, it is a list (OBJECT OBJECTS ACTION),
where OBJECT is a string giving the singular noun for an elt of LIST;
OBJECTS is the plural noun for elts of LIST, and ACTION is a transitive
verb describing ACTOR.  The default is \(\"object\" \"objects\" \"act on\"\).

At the prompts, the user may enter y, Y, or SPC to act on that object;
n, N, or DEL to skip that object; ! to act on all following objects;
ESC or q to exit (skip all following objects); . (period) to act on the
current object and then exit; or \\[help-command] to get help.

If ACTION-ALIST is given, it is an alist (KEY FUNCTION HELP) of extra keys
that will be accepted.  KEY is a character; FUNCTION is a function of one
arg (an object from LIST); HELP is a string.  When the user hits KEY,
FUNCTION is called.  If it returns non-nil, the object is considered
\"acted upon\", and the next object from LIST is processed.  If it returns
nil, the prompt is repeated for the same object.

Final optional argument NO-CURSOR-IN-ECHO-AREA non-nil says not to set
`cursor-in-echo-area' while prompting.

This function uses `query-replace-map' to define the standard responses,
but not all of the responses which `query-replace' understands
are meaningful here.

Returns the number of actions taken."
  (let* ((actions 0)
	 user-keys mouse-event map prompt char elt def
	 ;; Non-nil means we should use mouse menus to ask.
	 ;; use-menus
	 ;;delayed-switch-frame
	 (next (if (or (and list (symbolp list))
		       (subrp list)
		       (compiled-function-p list)
		       (and (consp list)
			    (eq (car list) 'lambda)))
		   #'(lambda () (setq elt (funcall list)))
		 #'(lambda ()
		     (if list
			 (progn
			   (setq elt (car list)
				 list (cdr list))
			   t)
		       nil)))))
    (if (should-use-dialog-box-p)
	;; Make a list describing a dialog box.
	(let (;; (object (capitalize (or (nth 0 help) "object")))
	      ;; (objects (capitalize (or (nth 1 help) "objects")))
	      ;; (action (capitalize (or (nth 2 help) "act on")))
	      )
	  (setq map `(("%_Yes" . act) ("%_No" . skip)
; bogus crap.  --ben
;			((, (if help
;				(capitalize
;				 (or (nth 3 help)
;				     (concat action " All " objects)))
;			      "Do All")) . automatic)
;			((, (if help
;				(capitalize
;				 (or (nth 4 help)
;				     (concat action " " object " And Quit")))
;			      "Do it and Quit")) . act-and-exit)
;			((, (capitalize
;			     (or (and help (nth 5 help)) "Quit")))
;			 . exit)
			("Yes %_All" . automatic)
			("No A%_ll" . exit)
			("%_Cancel" . quit)
			,@(mapcar #'(lambda (elt)
				      (cons (capitalize (nth 2 elt))
					    (vector (nth 1 elt))))
				  action-alist))
		mouse-event last-command-event))
      (setq user-keys (if action-alist
			  (concat (mapconcat #'(lambda (elt)
						 (key-description
						  (if (characterp (car elt))
						      ;; XEmacs
						      (char-to-string (car elt))
						    (car elt))))
					     action-alist ", ")
				  " ")
			"")
	    ;; Make a map that defines each user key as a vector containing
	    ;; its definition.
	    ;; XEmacs
	    map (let ((foomap (make-sparse-keymap)))
		  (mapcar #'(lambda (elt)
			      (define-key
				foomap
				(if (characterp (car elt))
				    (char-to-string (car elt))
				  (car elt))
				(vector (nth 1 elt))))
			  action-alist)
		  (set-keymap-parents foomap (list query-replace-map))
		  foomap)))
    (unwind-protect
	(progn
	  (if (stringp prompter)
	      (setq prompter `(lambda (object)
				(format ,prompter object))))
	  (while (funcall next)
	    (setq prompt (funcall prompter elt))
	    (cond ((stringp prompt)
		   ;; Prompt the user about this object.
		   (setq quit-flag nil)
		   (if mouse-event ; XEmacs
		       (setq def (or (get-dialog-box-response
				      mouse-event
				      (cons prompt map))
				     'quit))
		     ;; Prompt in the echo area.
		     (let ((cursor-in-echo-area (not no-cursor-in-echo-area)))
		       (display-message
			'prompt
			(format "%s(y, n, !, ., q, %sor %s) "
				prompt user-keys
				(key-description (vector help-char))))
		       (setq char (next-command-event))
		       ;; Show the answer to the question.
		       (display-message
			'prompt
			(format
			 "%s(y, n, !, ., q, %sor %s) %s"
			 prompt user-keys
			 (key-description (vector help-char))
			 (single-key-description char))))
		     (setq def (lookup-key map (vector char))))
		   (cond ((eq def 'exit)
			  (setq next #'(lambda () nil)))
			 ((eq def 'act)
			  ;; Act on the object.
			  (funcall actor elt)
			  (setq actions (1+ actions)))
			 ((eq def 'skip)
			  ;; Skip the object.
			  )
			 ((eq def 'act-and-exit)
			  ;; Act on the object and then exit.
			  (funcall actor elt)
			  (setq actions (1+ actions)
				next (function (lambda () nil))))
			 ((or (eq def 'quit) (eq def 'exit-prefix))
			  (setq quit-flag t)
			  (setq next `(lambda ()
					(setq next ',next)
					  ',elt)))
			 ((eq def 'automatic)
			  ;; Act on this and all following objects.
			  ;; (if (funcall prompter elt) ; Emacs
			  (if (eval (funcall prompter elt))
			      (progn
				(funcall actor elt)
				(setq actions (1+ actions))))
			  (while (funcall next)
			    ;; (funcall prompter elt) ; Emacs
			    (if (eval (funcall prompter elt))
				(progn
				  (funcall actor elt)
				  (setq actions (1+ actions))))))
			 ((eq def 'help)
			  (with-output-to-temp-buffer "*Help*"
			    (princ
			     (let ((object (if help (nth 0 help) "object"))
				   (objects (if help (nth 1 help) "objects"))
				   (action (if help (nth 2 help) "act on")))
			       (concat
				(format "Type SPC or `y' to %s the current %s;
DEL or `n' to skip the current %s;
! to %s all remaining %s;
ESC or `q' to exit;\n"
					action object object action objects)
				(mapconcat (function
					    (lambda (elt)
					      (format "%c to %s"
						      (nth 0 elt)
						      (normalize-menu-item-name
						       (nth 2 elt)))))
					   action-alist
					   ";\n")
				(if action-alist ";\n")
				(format "or . (period) to %s \
the current %s and exit."
					action object))))
			    (save-excursion
			      (set-buffer standard-output)
			      (help-mode)))

			  (setq next `(lambda ()
					(setq next ',next)
					',elt)))
			 ((vectorp def)
			  ;; A user-defined key.
			  (if (funcall (aref def 0) elt) ;Call its function.
			      ;; The function has eaten this object.
			      (setq actions (1+ actions))
			    ;; Regurgitated; try again.
			    (setq next `(lambda ()
					  (setq next ',next)
					  ',elt))))
			 ;((and (consp char) ; Emacs
			 ;	(eq (car char) 'switch-frame))
			 ; ;; switch-frame event.  Put it off until we're done.
			 ; (setq delayed-switch-frame char)
			 ; (setq next `(lambda ()
			 ;		 (setq next ',next)
			 ;		 ',elt)))
			 (t
			  ;; Random char.
			  (message "Type %s for help."
				   (key-description (vector help-char)))
			  (beep)
			  (sit-for 1)
			  (setq next `(lambda ()
					(setq next ',next)
					',elt)))))
		  ((eval prompt)
		   (progn
		     (funcall actor elt)
		     (setq actions (1+ actions)))))))
      ;;(if delayed-switch-frame
      ;;	   (setq unread-command-events
      ;;		 (cons delayed-switch-frame unread-command-events))))
      ;;		   ((eval prompt)
      ;;		    (progn
      ;;		      (funcall actor elt)
      ;;		      (setq actions (1+ actions)))))
      )
    ;; Clear the last prompt from the minibuffer.
    (clear-message 'prompt)
    ;; Return the number of actions that were taken.
    actions))

;;; map-ynp.el ends here
