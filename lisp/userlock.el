;;; userlock.el --- handle file access contention between multiple users

;; Copyright (C) 1985, 1986, 1993 Free Software Foundation, inc.

;; Maintainer: FSF
;; Keywords: internal

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is autoloaded to handle certain conditions
;; detected by the file-locking code within XEmacs.
;; The two entry points are `ask-user-about-lock' and
;; `ask-user-about-supersession-threat'.

;;; Code:

(define-error 'file-locked "File is locked" 'file-error) ; XEmacs

(defun ask-user-about-lock-minibuf (filename other-user)
  (save-window-excursion
    (let (answer)
      (while (null answer)
	(message "%s is locking %s: action (s, q, p, ?)? " other-user filename)
	(let ((tem (let ((inhibit-quit t)
			 (cursor-in-echo-area t))
		     (prog1 (downcase (read-char))
			    (setq quit-flag nil)))))
	  (if (= tem help-char)
	      (ask-user-about-lock-help)
	    (setq answer (assoc tem '((?s . t)
				      (?q . yield)
				      (?\C-g . yield)
				      (?p . nil)
				      (?? . help))))
	    (cond ((null answer)
		   (beep)
		   (message "Please type q, s, or p; or ? for help")
		   (sit-for 3))
		  ((eq (cdr answer) 'help)
		   (ask-user-about-lock-help)
		   (setq answer nil))
		  ((eq (cdr answer) 'yield)
		   (signal 'file-locked (list "File is locked" filename other-user)))))))
      (cdr answer))))

(defun ask-user-about-lock-help ()
  (with-output-to-temp-buffer "*Help*"
    (princ "It has been detected that you want to modify a file that someone else has
already started modifying in EMACS.

You can <s>teal the file; The other user becomes the
  intruder if (s)he ever unmodifies the file and then changes it again.
You can <p>roceed; you edit at your own (and the other user's) risk.
You can <q>uit; don't modify this file.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

(define-error 'file-supersession "File changed on disk" 'file-error) ; XEmacs

(defun ask-user-about-supersession-threat-minibuf (filename)
  (save-window-excursion
    (let (answer)
      (while (null answer)
	(message "%s changed on disk; really edit the buffer? (y, n, r or C-h) "
		 (file-name-nondirectory filename))
	(let ((tem (downcase (let ((cursor-in-echo-area t))
			       (read-char)))))
	  (setq answer
		(if (= tem help-char)
		    'help
		  (cdr (assoc tem '((?n . yield)
				    (?\C-g . yield)
				    (?y . proceed)
				    (?r . revert)
				    (?? . help))))))
	  (cond ((null answer)
		 (beep)
		 (message "Please type y, n or r; or ? for help")
		 (sit-for 3))
		((eq answer 'help)
		 (ask-user-about-supersession-help)
		 (setq answer nil))
		((eq answer 'revert)
		 (revert-buffer nil (not (buffer-modified-p)))
		 ; ask confirmation iff buffer modified
		 (signal 'file-supersession
			 (list "File reverted" filename)))
		((eq answer 'yield)
		 (signal 'file-supersession
			 (list "File changed on disk" filename))))))
      (message
	"File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))

(defun ask-user-about-supersession-help ()
  (with-output-to-temp-buffer "*Help*"
    (princ "You want to modify a buffer whose disk file has changed
since you last read it in or saved it with this buffer.

If you say `y' to go ahead and modify this buffer,
you risk ruining the work of whoever rewrote the file.
If you say `r' to revert, the contents of the buffer are refreshed
from the file on disk.
If you say `n', the change you started to make will be aborted.

Usually, you should type `n' and then `M-x revert-buffer',
to get the latest version of the file, then make the change again.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

;;; dialog-box versions [XEmacs]

(defun ask-user-about-lock-dbox (filename other-user)
  (let ((echo-keystrokes 0))
    (make-dialog-box
     'question
     :question (format "%s is locking %s\n
	It has been detected that you want to modify a file that
	someone else has already started modifying in XEmacs."
		       other-user filename)
     :buttons
     '(["Steal Lock\n\nThe other user will\nbecome the intruder" steal t]
       ["Proceed\n\nEdit file at your own\n\(and the other user's) risk"
	proceed t]
       nil
       ["Abort\n\nDon't modify the buffer\n" yield t]))
    (catch 'aual-done
      (while t
	(let ((event (next-command-event)))
	  (cond ((and (misc-user-event-p event)
		      (eq (event-object event) 'proceed))
		 (throw 'aual-done nil))
		((and (misc-user-event-p event)
		      (eq (event-object event) 'steal))
		 (throw 'aual-done t))
		((and (misc-user-event-p event)
		      (eq (event-object event) 'yield))
		 (signal 'file-locked (list "File is locked" filename other-user)))
		((and (misc-user-event-p event)
		      (eq (event-object event) 'menu-no-selection-hook))
		 (signal 'quit nil))
		;; safety check, so we're not endlessly stuck when no
		;; dialog box up
		((not (popup-up-p))
		 (signal 'quit nil))
		((button-release-event-p event) ;; don't beep twice
		 nil)
		(t
		 (beep)
		 (message "please answer the dialog box"))))))))

(defun ask-user-about-supersession-threat-dbox (filename)
  (let ((echo-keystrokes 0))
    (make-dialog-box
     'question
     :question
     (format "File %s has changed on disk
since its buffer was last read in or saved.

Do you really want to edit the buffer? " filename)
     :buttons
     '(["Yes\n\nEdit the buffer anyway,\nignoring the disk file"
	proceed t]
       ["No\n\nDon't modify the buffer\n" yield t]
       nil
       ["No\n\nDon't modify the buffer\nbut revert it" revert t]
       ))
    (catch 'auast-done
      (while t
	(let ((event (next-command-event)))
	  (cond ((and (misc-user-event-p event) (eq (event-object event) 'proceed))
		 (throw 'auast-done nil))
		((and (misc-user-event-p event) (eq (event-object event) 'yield))
		 (signal 'file-supersession (list filename)))
		((and (misc-user-event-p event) (eq (event-object event) 'revert))
		 (or (equal filename (buffer-file-name))
		     (error
		      "ask-user-about-supersession-threat called bogusly"))
		 (revert-buffer nil t)
		 (signal 'file-supersession
			 (list filename "(reverted)")))
		((and (misc-user-event-p event)
		      (eq (event-object event) 'menu-no-selection-hook))
		 (signal 'quit nil))
		;; safety check, so we're not endlessly stuck when no
		;; dialog box up
		((not (popup-up-p))
		 (signal 'quit nil))
		((button-release-event-p event) ;; don't beep twice
		 nil)
		(t
		 (beep)
		 (message "please answer the dialog box"))))))))


;;; top-level

;;;###autoload
(defun ask-user-about-lock (filename other-user)
  "Ask user wanting to edit FILENAME, locked by OTHER-USER, what to do.
This function has a choice of three things to do:
  do (signal 'file-locked (list FILENAME OTHER-USER))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can rewrite it to use any criteria you like to choose which one to do."
  (discard-input)
  (if (should-use-dialog-box-p)
      (ask-user-about-lock-dbox filename other-user)
    (ask-user-about-lock-minibuf filename other-user)))

;;;###autoload
(defun ask-user-about-supersession-threat (filename)
  "Ask user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (FILENAME)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criteria you like to choose which one to do.
The buffer in question is current when this function is called."
  (discard-input)
  (if (should-use-dialog-box-p)
      (ask-user-about-supersession-threat-dbox filename)
    (ask-user-about-supersession-threat-minibuf filename)))

;;; userlock.el ends here
