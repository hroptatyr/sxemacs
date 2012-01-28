;;; select.el --- Lisp interface to windows selections.

;; Copyright (C) 1998 Andy Piper.
;; Copyright (C) 1990, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with SXEmacs

;;; Code:

(defvar selected-text-type
  (if (featurep 'mule) '(COMPOUND_TEXT STRING) 'STRING)
  "The type atom used to obtain selections from the X server.
Can be either a valid X selection data type, or a list of such types.
COMPOUND_TEXT and STRING are the most commonly used data types.
If a list is provided, the types are tried in sequence until
there is a successful conversion.")

(defvar selection-sets-clipboard nil
  "Controls the selection's relationship to the clipboard.
When non-nil, any operation that sets the primary selection will also
set the clipboard.")

(defun copy-primary-selection ()
  "Copy the selection to the Clipboard and the kill ring."
  (interactive)
  (and (console-on-window-system-p)
       (cut-copy-clear-internal 'copy)))

(defun kill-primary-selection ()
  "Copy the selection to the Clipboard and the kill ring, then delete it."
  (interactive "*")
  (and (console-on-window-system-p)
       (cut-copy-clear-internal 'cut)))

(defun delete-primary-selection ()
  "Delete the selection without copying it to the Clipboard or the kill ring."
  (interactive "*")
  (and (console-on-window-system-p)
       (cut-copy-clear-internal 'clear)))

(defun yank-clipboard-selection ()
  "Insert the current Clipboard selection at point."
  (interactive "*")
  (when (console-on-window-system-p)
    (setq last-command nil)
    (setq this-command 'yank) ; so that yank-pop works.
    (let ((clip (get-clipboard)))
      (or clip (error "there is no clipboard selection"))
      (push-mark)
      (insert clip))))

(defun get-clipboard ()
  "Return text pasted to the clipboard.
Not suitable for `interprogram-paste-function', use `get-clipboard-foreign'."
  (get-selection 'CLIPBOARD))

(defun get-clipboard-foreign ()
  "Return text pasted to the clipboard by another program.
See `interprogram-paste-function' for more information."
  (get-selection-foreign 'CLIPBOARD))

(define-device-method get-cutbuffer
  "Return the value of one of the cut buffers.
This will do nothing under anything other than X.")

(defun get-selection-no-error (&optional type data-type)
  "Return the value of a window-system selection.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING', or `COMPOUND_TEXT' under Mule)
says how to convert the data. Returns NIL if there is no selection."
  (condition-case nil (get-selection type data-type) (t nil)))

(defun get-selection (&optional type data-type)
  "Return the value of a window-system selection.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING', or `COMPOUND_TEXT' under Mule)
says how to convert the data. If there is no selection an error is signalled.
Not suitable in a `interprogram-paste-function', q.v."
  (or type (setq type 'PRIMARY))
  (or data-type (setq data-type selected-text-type))
  (if (consp data-type)
      (condition-case err
	  (get-selection-internal type (car data-type))
	(selection-conversion-error
	 (if (cdr data-type)
	     (get-selection type (cdr data-type))
	   (signal (car err) (cdr err)))))
    (get-selection-internal type data-type)))

(defun get-selection-foreign (&optional type data-type)
  "Return the value of a window-system selection, or nil if SXEmacs owns it.
The argument TYPE (default `PRIMARY') says which selection,
and the argument DATA-TYPE (default `STRING', or `COMPOUND_TEXT' under Mule)
says how to convert the data. If there is no selection an error is signalled.
See `interprogram-paste-function' for more information."
  (unless (selection-owner-p type)
    (get-selection type data-type)))

;; FSFmacs calls this `x-set-selection', and reverses the
;; first two arguments (duh ...).  This order is more logical.
(defun own-selection (data &optional type how-to-add data-type)
  "Make a window-system selection of type TYPE and value DATA.
The argument TYPE (default `PRIMARY') says which selection,
and DATA specifies the contents.  DATA may be any lisp data type
that can be converted using the function corresponding to DATA-TYPE
in `select-converter-alist'---strings are the usual choice, but
other types may be permissible depending on the DATA-TYPE parameter
(if DATA-TYPE is not supplied, the default behavior is window
system specific, but strings are always accepted).
HOW-TO-ADD may be any of the following:

  'replace-all or nil -- replace all data in the selection.
  'replace-existing   -- replace data for specified DATA-TYPE only.
  'append or t        -- append data to existing DATA-TYPE data.

DATA-TYPE is the window-system specific data type identifier
(see `register-selection-data-type' for more information).

The selection may also be a cons of two markers pointing to the same buffer,
or an overlay.  In these cases, the selection is considered to be the text
between the markers *at whatever time the selection is examined* (note
that the window system clipboard does not necessarily duplicate this
behavior).
Thus, editing done in the buffer after you specify the selection
can alter the effective value of the selection.

The data may also be a vector of valid non-vector selection values.

Interactively, the text of the region is used as the selection value."
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (substring (region-beginning) (region-end)))))
  ;; calling own-selection-internal will mess this up, so preserve it.
  (let ((zmacs-region-stays zmacs-region-stays))
					;FSFmacs huh??  It says:
    ;; "This is for temporary compatibility with pre-release Emacs 19."
					;(if (stringp type)
					;    (setq type (intern type)))
    (or type (setq type 'PRIMARY))
    (if (null data)
	(disown-selection-internal type)
      (own-selection-internal type data how-to-add data-type)
      (when (and (eq type 'PRIMARY)
		 selection-sets-clipboard)
	 (own-selection-internal 'CLIPBOARD data how-to-add data-type)))
    (cond ((eq type 'PRIMARY)
	   (setq primary-selection-extent
		 (select-make-extent-for-selection
		  data primary-selection-extent)))
	  ((eq type 'SECONDARY)
	   (setq secondary-selection-extent
		 (select-make-extent-for-selection
		  data secondary-selection-extent)))))
  ;; zmacs-region-stays is for commands, not low-level functions.
  ;; when behaving as the latter, we better not set it, or we will
  ;; cause unwanted sticky-region behavior in kill-region and friends.
  (if (interactive-p)
  (setq zmacs-region-stays t))
  data)

(defun dehilight-selection (selection)
  "for use as a value of `lost-selection-hooks'."
  (cond ((eq selection 'PRIMARY)
	 (if primary-selection-extent
	     (let ((inhibit-quit t))
	       (if (consp primary-selection-extent)
		   (mapcar 'delete-extent primary-selection-extent)
		 (delete-extent primary-selection-extent))
	       (setq primary-selection-extent nil)))
	 (if zmacs-regions (zmacs-deactivate-region)))
	((eq selection 'SECONDARY)
	 (if secondary-selection-extent
	     (let ((inhibit-quit t))
	       (if (consp secondary-selection-extent)
		   (mapcar 'delete-extent secondary-selection-extent)
		 (delete-extent secondary-selection-extent))
	       (setq secondary-selection-extent nil)))))
  nil)

(setq lost-selection-hooks 'dehilight-selection)

(defun own-clipboard (string &optional push)
  "Paste the given string to the window system Clipboard.
See `interprogram-cut-function' for more information."
  (own-selection string 'CLIPBOARD))

(defun disown-selection (&optional secondary-p)
  "Assuming we own the selection, disown it.
With an argument, discard the secondary selection instead of the
primary selection."
  (disown-selection-internal (if secondary-p 'SECONDARY 'PRIMARY))
  (when (and selection-sets-clipboard
	     (or (not secondary-p)
		 (eq secondary-p 'PRIMARY)
		 (eq secondary-p 'CLIPBOARD)))
    (disown-selection-internal 'CLIPBOARD)))

;; from x-init.el
;; selections and active regions

;; If and only if zmacs-regions is true:

;; When a mark is pushed and the region goes into the "active" state, we
;; assert it as the Primary selection.  This causes it to be hilighted.
;; When the region goes into the "inactive" state, we disown the Primary
;; selection, causing the region to be dehilighted.

;; Note that it is possible for the region to be in the "active" state
;; and not be hilighted, if it is in the active state and then some other
;; application asserts the selection.  This is probably not a big deal.

(defun activate-region-as-selection ()
  (if (marker-buffer (mark-marker t))
      (own-selection (cons (point-marker t) (mark-marker t)))))

; moved from x-select.el
(defvar primary-selection-extent nil
  "The extent of the primary selection; don't use this.")

(defvar secondary-selection-extent nil
  "The extent of the secondary selection; don't use this.")

(defun select-make-extent-for-selection (selection previous-extent)
  ;; Given a selection, this makes an extent in the buffer which holds that
  ;; selection, for highlighting purposes.  If the selection isn't associated
  ;; with a buffer, this does nothing.
  (let ((buffer nil)
	(valid (and (extentp previous-extent)
		    (extent-object previous-extent)
		    (buffer-live-p (extent-object previous-extent))))
	start end)
    (cond ((stringp selection)
	   ;; if we're selecting a string, lose the previous extent used
	   ;; to highlight the selection.
	   (setq valid nil))
	  ((consp selection)
	   (setq start (min (car selection) (cdr selection))
		 end (max (car selection) (cdr selection))
		 valid (and valid
			    (eq (marker-buffer (car selection))
				(extent-object previous-extent)))
		 buffer (marker-buffer (car selection))))
	  ((extentp selection)
	   (setq start (extent-start-position selection)
		 end (extent-end-position selection)
		 valid (and valid
			    (eq (extent-object selection)
				(extent-object previous-extent)))
		 buffer (extent-object selection)))
	  (t
	   (signal 'error (list "invalid selection" selection))))

    (if valid
	nil
      (condition-case ()
	  (if (listp previous-extent)
	      (mapcar 'delete-extent previous-extent)
	    (delete-extent previous-extent))
	(error nil)))

    (if (not buffer)
	;; string case
	nil
      ;; normal case
      (if valid
	  (set-extent-endpoints previous-extent start end)
	(setq previous-extent (make-extent start end buffer))

	;; Make the extent be closed on the right, which means that if
	;; characters are inserted exactly at the end of the extent, the
	;; extent will grow to cover them.  This is important for shell
	;; buffers - suppose one makes a selection, and one end is at
	;; point-max.  If the shell produces output, that marker will remain
	;; at point-max (its position will increase).  So it's important that
	;; the extent exhibit the same behavior, lest the region covered by
	;; the extent (the visual indication), and the region between point
	;; and mark (the actual selection value) become different!
	(set-extent-property previous-extent 'end-open nil)

	(cond
	 (mouse-track-rectangle-p
	  (setq previous-extent (list previous-extent))
	  (default-mouse-track-next-move-rect start end previous-extent)
	  ))
	previous-extent))))

;; moved from x-select.el
(defun valid-simple-selection-p (data)
  "An obsolete function that tests whether something was a valid simple
selection using the old XEmacs selection support. You shouldn't use this
any more, because just about anything could be a valid selection now."
  (or (stringp data)
      ;FSFmacs huh?? (symbolp data)
      (integerp data)
      (and (consp data)
	   (integerp (car data))
	   (or (integerp (cdr data))
	       (and (consp (cdr data))
		    (integerp (car (cdr data))))))
      (extentp data)
      (and (consp data)
	   (markerp (car data))
	   (markerp (cdr data))
	   (marker-buffer (car data))
	   (marker-buffer (cdr data))
	   (eq (marker-buffer (car data))
	       (marker-buffer (cdr data)))
	   (buffer-live-p (marker-buffer (car data)))
	   (buffer-live-p (marker-buffer (cdr data))))))

(globally-declare-boundp 'killed-rectangle)

(defun cut-copy-clear-internal (mode)
  (or (memq mode '(cut copy clear)) (error "unknown mode %S" mode))
  (or (selection-owner-p)
      (error "SXEmacs does not own the primary selection"))
  (setq last-command nil)
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (let (rect-p b s e)
      (cond
       ((consp primary-selection-extent)
	(setq rect-p t
	      b (extent-object (car primary-selection-extent))
	      s (extent-start-position (car primary-selection-extent))
	      e (extent-end-position (car (reverse primary-selection-extent)))))
       (t
	(setq rect-p nil
	      b (extent-object primary-selection-extent)
	      s (extent-start-position primary-selection-extent)
	      e (extent-end-position primary-selection-extent))))
      (set-buffer b)
      (cond ((memq mode '(cut copy))
	     (if rect-p
		 (progn
		   ;; why is killed-rectangle free?  Is it used somewhere?
		   ;; should it be defvarred?
		   (setq killed-rectangle (declare-fboundp (extract-rectangle s e)))
		   (kill-new (mapconcat #'identity killed-rectangle "\n")))
	       (copy-region-as-kill s e))
	     ;; Maybe killing doesn't own clipboard.  Make sure it happens.
	     ;; This memq is kind of grody, because they might have done it
	     ;; some other way, but owning the clipboard twice in that case
	     ;; wouldn't actually hurt anything.
	     (or (and (consp kill-hooks) (memq 'own-clipboard kill-hooks))
		 (own-clipboard (car kill-ring)))))
      (cond ((memq mode '(cut clear))
	     (if rect-p
		 (declare-fboundp (delete-rectangle s e))
	       (delete-region s e))))
      (disown-selection nil)
      )))


;;; Functions to convert the selection into various other selection
;;; types.

;; These next three functions get called by C code...
(defun select-convert-in (selection type value)
  "Attempt to convert the specified external VALUE to the specified DATA-TYPE,
for the specified SELECTION. Return nil if this is impossible, or a
suitable internal representation otherwise."
  (when value
    (let ((handler-fn (cdr (assq type selection-converter-in-alist))))
      (if handler-fn
	  (apply handler-fn (list selection type value))
	value))))

(defun select-convert-out (selection type value)
  "Attempt to convert the specified internal VALUE for the specified DATA-TYPE
and SELECTION. Return nil if this is impossible, or a suitable external
representation otherwise."
  (when value
    (let ((handler-fn (cdr (assq type selection-converter-out-alist))))
      (when handler-fn
	(apply handler-fn (list selection type value))))))

(defun select-coerce (selection type value)
  "Attempt to convert the specified internal VALUE to a representation
suitable for return from `get-selection' in the specified DATA-TYPE. Return
nil if this is impossible, or a suitable representation otherwise."
  (when value
    (let ((handler-fn (cdr (assq type selection-coercion-alist))))
      (when handler-fn
	(apply handler-fn (list selection type value))))))

;; The rest of the functions on this "page" are conversion handlers,
;; append handlers and buffer-kill handlers.
(defun select-convert-to-text (selection type value)
  (cond ((stringp value)
	 value)
	((extentp value)
	 (save-excursion
	   (set-buffer (extent-object value))
	   (save-restriction
	     (widen)
	     (buffer-substring (extent-start-position value)
			       (extent-end-position value)))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (or (eq (marker-buffer (car value)) (marker-buffer (cdr value)))
	     (signal 'error
		     (list "markers must be in the same buffer"
			   (car value) (cdr value))))
	 (save-excursion
	   (set-buffer (or (marker-buffer (car value))
			   (error "selection is in a killed buffer")))
	   (save-restriction
	     (widen)
	     (buffer-substring (car value) (cdr value)))))
	(t nil)))

(defun select-coerce-to-text (selection type value)
  (select-convert-to-text selection type value))

(defun select-convert-to-string (selection type value)
  (let ((outval (select-convert-to-text selection type value)))
    ;; force the string to be not in Compound Text format. This grubby
    ;; hack will go soon, to be replaced by a more general mechanism.
    (if (stringp outval)
	(cons 'STRING outval)
      outval)))

(defun select-convert-to-compound-text (selection type value)
  ;; converts to compound text automatically
  (select-convert-to-text selection type value))

(defun select-convert-to-length (selection type value)
  (let ((value
	 (cond ((stringp value)
		(length value))
	       ((extentp value)
		(extent-length value))
	       ((and (consp value)
		     (markerp (car value))
		     (markerp (cdr value)))
		(or (eq (marker-buffer (car value))
			(marker-buffer (cdr value)))
		    (signal 'error
			    (list "markers must be in the same buffer"
				  (car value) (cdr value))))
		(abs (- (car value) (cdr value)))))))
    (if value ; force it to be in 32-bit format.
	(cons (ash value -16) (logand value 65535))
      nil)))

(defun select-convert-to-targets (selection type value)
  ;; return a vector of atoms, but remove duplicates first.
  (let* ((all (cons 'TIMESTAMP (mapcar 'car selection-converter-alist)))
	 (rest all))
    (while rest
      (cond ((memq (car rest) (cdr rest))
	     (setcdr rest (delq (car rest) (cdr rest))))
	    (t
	     (setq rest (cdr rest)))))
    (apply 'vector all)))

(defun select-convert-to-delete (selection type value)
  (disown-selection-internal selection)
  ;; A return value of nil means that we do not know how to do this conversion,
  ;; and replies with an "error".  A return value of NULL means that we have
  ;; done the conversion (and any side-effects) but have no value to return.
  'NULL)

(defun select-convert-to-filename (selection type value)
  (cond ((extentp value)
	 (buffer-file-name (or (extent-object value)
			       (error "selection is in a killed buffer"))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (buffer-file-name (or (marker-buffer (car value))
			       (error "selection is in a killed buffer"))))
	(t nil)))

(defun select-convert-to-charpos (selection type value)
  (let (a b tmp)
    (cond ((cond ((extentp value)
		  (setq a (extent-start-position value)
			b (extent-end-position value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (car value)
			b (cdr value))))
	   (setq a (1- a) b (1- b)) ; zero-based
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun select-convert-to-lineno (selection type value)
  (let (a b buf tmp)
    (cond ((cond ((extentp value)
		  (setq buf (extent-object value)
			a (extent-start-position value)
			b (extent-end-position value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (marker-position (car value))
			b (marker-position (cdr value))
			buf (marker-buffer (car value)))))
	   (save-excursion
	     (set-buffer buf)
	     (save-restriction
	       (widen)
	       (goto-char a)
	       (beginning-of-line)
	       (setq a (1+ (count-lines 1 (point))))
	       (goto-char b)
	       (beginning-of-line)
	       (setq b (1+ (count-lines 1 (point))))))
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun select-convert-to-colno (selection type value)
  (let (a b buf tmp)
    (cond ((cond ((extentp value)
		  (setq buf (extent-object value)
			a (extent-start-position value)
			b (extent-end-position value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (car value)
			b (cdr value)
			buf (marker-buffer a))))
	   (save-excursion
	     (set-buffer buf)
	     (goto-char a)
	     (setq a (current-column))
	     (goto-char b)
	     (setq b (current-column)))
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun select-convert-to-sourceloc (selection type value)
  (let (a b buf file-name tmp)
    (cond ((cond ((extentp value)
		  (setq buf (or (extent-object value)
				(error "selection is in a killed buffer"))
			a (extent-start-position value)
			b (extent-end-position value)
			file-name (buffer-file-name buf)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (marker-position (car value))
			b (marker-position (cdr value))
			buf (or (marker-buffer (car value))
				(error "selection is in a killed buffer"))
			file-name (buffer-file-name buf))))
	   (save-excursion
	     (set-buffer buf)
	     (save-restriction
	       (widen)
	       (goto-char a)
	       (beginning-of-line)
	       (setq a (1+ (count-lines 1 (point))))
	       (goto-char b)
	       (beginning-of-line)
	       (setq b (1+ (count-lines 1 (point))))))
	   (if (< b a) (setq tmp a a b b tmp))
	   (format "%s:%d" file-name a)))))

(defun select-convert-to-os (selection type size)
  (symbol-name system-type))

(defun select-convert-to-host (selection type size)
  (system-name))

(defun select-convert-to-user (selection type size)
  (user-full-name))

(defun select-convert-to-class (selection type size)
  (symbol-value 'x-emacs-application-class))

;; We do not try to determine the name Emacs was invoked with,
;; because it is not clean for a program's behavior to depend on that.
(defun select-convert-to-name (selection type size)
  ;invocation-name
  "sxemacs")

(defun select-convert-to-integer (selection type value)
  (and (integerp value)
       (cons (ash value -16) (logand value 65535))))

;; Can convert from the following integer representations
;;
;;    integer
;;    (integer . integer)
;;    (integer integer)
;;    (list [integer|(integer . integer)]*)
;;    (vector [integer|(integer . integer)]*)
;;
;; Cons'd integers get cleaned up a little.

(defun select-convert-from-integer (selection type value)
  (cond ((integerp value)		; Integer
	 value)

	((and (consp value)		; (integer . integer)
	      (integerp (car value))
	      (integerp (cdr value)))
	 (if (eq (car value) 0)
	     (cdr value)
	   (if (and (eq (car value) -1)
		    (< (cdr value) 0))
	       (cdr value)
	     value)))

	((and (listp value)		; (integer integer)
	      (eq (length value) 2)
	      (integerp (car value))
	      (integerp (cadr value)))
	 (if (eq (car value) 0)
	     (cadr value)
	   (if (and (eq (car value) -1)
		    (< (cdr value) 0))
	       (- (cadr value))
	     (cons (car value) (cadr value)))))

	((listp value)			; list
	 (if (cdr value)
	     (mapcar '(lambda (x)
			(select-convert-from-integer selection type x))
		     value)
	   (select-convert-from-integer selection type (car value))))

	((vectorp value)		; vector
	 (if (eq (length value) 1)
	     (select-convert-from-integer selection type (aref value 0))
	   (mapvector '(lambda (x)
			(select-convert-from-integer selection type x))
		     value)))

	(t nil)
	))

(defun select-convert-from-ip-address (selection type value)
  (if (and (stringp value)
	   (= (length value) 4))
      (format "%d.%d.%d.%d"
	      (aref value 0) (aref value 1) (aref value 2) (aref value 3))))

(defun select-convert-to-atom (selection type value)
  (and (symbolp value) value))

(defun select-convert-from-utf8-text (selection type value)
  (require 'un-define)
  (decode-coding-string value 'utf-8))

(defun select-convert-from-utf16-le-text (selection type value)
  (require 'un-define)
  (decode-coding-string value 'utf-16-le))

;;; CF_xxx conversions
(defun select-convert-from-cf-text (selection type value)
  (replace-in-string (if (string-match "\0" value)
			 (substring value 0 (match-beginning 0))
		       value)
		     "\\(\r\n\\|\n\r\\)" "\n" t))

(defun select-convert-to-cf-text (selection type value)
  (let ((text (select-convert-to-text selection type value)))
    (concat (replace-in-string text "\n" "\r\n" t) "\0")))

;;; Appenders
(defun select-append-to-text (selection type value1 value2)
  (let ((text1 (select-convert-to-text selection 'STRING value1))
	(text2 (select-convert-to-text selection 'STRING value2)))
    (if (and text1 text2)
	(concat text1 text2)
      nil)))

(defun select-append-to-string (selection type value1 value2)
  (select-append-to-text selection type value1 value2))

(defun select-append-to-compound-text (selection type value1 value2)
  (select-append-to-text selection type value1 value2))

(defun select-append-to-cf-text (selection type value1 value2)
  (let ((text1 (select-convert-from-cf-text selection 'CF_TEXT value1))
	(text2 (select-convert-from-cf-text selection 'CF_TEXT value2)))
    (if (and text1 text2)
	(select-convert-to-cf-text selection type (concat text1 text2))
      nil)))

(defun select-append-default (selection type value1 value2)
;; This appender gets used if the type is "nil" - i.e. default.
;; It should probably have more cases implemented than it does - e.g.
;; appending numbers to strings, etc...
  (cond ((and (stringp value1) (stringp value2))
	 (select-append-to-string selection 'STRING value1 value2))
	(t nil)))

;;; Buffer kill handlers

(defun select-buffer-killed-default (selection type value buffer)
;; This handler gets used if the type is "nil".
  (cond ((extentp value)
	 (if (eq (extent-object value) buffer)
	     ; If this selection is on the clipboard, grab it quick
	     (when (eq selection 'CLIPBOARD)
	       (save-excursion
		 (set-buffer (extent-object value))
		 (save-restriction
		  (widen)
		  (buffer-substring (extent-start-position value)
				    (extent-end-position value)))))
	   value))
	((markerp value)
	 (unless (eq (marker-buffer value) buffer)
	   value))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (if (or (eq (marker-buffer (car value)) buffer)
		 (eq (marker-buffer (cdr value)) buffer))
	     ; If this selection is on the clipboard, grab it quick
	     (when (eq selection 'CLIPBOARD)
	       (save-excursion
		 (set-buffer (marker-buffer (car value)))
		 (save-restriction
		   (widen)
		   (buffer-substring (car value) (cdr value)))))
	     value))
	(t value)))

(defun select-buffer-killed-text (selection type value buffer)
  (select-buffer-killed-default selection type value buffer))

;; Types listed in here can be selections of SXEmacs
(setq selection-converter-out-alist
      '((TEXT . select-convert-to-text)
	(STRING . select-convert-to-string)
	(COMPOUND_TEXT . select-convert-to-compound-text)
	(TARGETS . select-convert-to-targets)
	(LENGTH . select-convert-to-length)
	(DELETE . select-convert-to-delete)
	(FILE_NAME . select-convert-to-filename)
	(CHARACTER_POSITION . select-convert-to-charpos)
	(SOURCE_LOC . select-convert-to-sourceloc)
	(LINE_NUMBER . select-convert-to-lineno)
	(COLUMN_NUMBER . select-convert-to-colno)
	(OWNER_OS . select-convert-to-os)
	(HOST_NAME . select-convert-to-host)
	(USER . select-convert-to-user)
	(CLASS . select-convert-to-class)
	(NAME . select-convert-to-name)
	(ATOM . select-convert-to-atom)
	(INTEGER . select-convert-to-integer)
	(CF_TEXT . select-convert-to-cf-text)
	))

;; Types listed here can be selections foreign to SXEmacs
;; the default is to return the raw data (works for strings and atoms)
(setq selection-converter-in-alist
      '((INTEGER . select-convert-from-integer)
	(TIMESTAMP . select-convert-from-integer)
	(LENGTH . select-convert-from-integer)
	(LIST_LENGTH . select-convert-from-integer)
	(CLIENT_WINDOW . select-convert-from-integer)
	(PROCESS . select-convert-from-integer)
	(IP_ADDRESS . select-convert-from-ip-address)
	(CF_TEXT . select-convert-from-cf-text)
	(UTF8_STRING . select-convert-from-utf8-text)
	(text/html . select-convert-from-utf16-le-text)  ; Mozilla
	(text/_moz_htmlcontext . select-convert-from-utf16-le-text)
	(text/_moz_htmlinfo . select-convert-from-utf16-le-text)
	))

;; Types listed here have special coercion functions that can munge
;; other types. This can also be used to add special features - e.g.
;; being able to pass a region or a cons of markers to own-selection,
;; but getting the *current* text in the region back when calling
;; get-selection.
;;
;; Any function listed in here *will be called* whenever a value of
;; its type is retrieved from the internal selection cache, or when
;; no suitable values could be found in which case SXEmacs looks for
;; values with types listed in selection-coercible-types.
(setq selection-coercion-alist
      '((TEXT . select-coerce-to-text)
	(STRING . select-coerce-to-text)
	(COMPOUND_TEXT . select-coerce-to-text)
	(CF_TEXT . select-coerce-to-text)))

;; Types listed here can be appended by own-selection
(setq selection-appender-alist
      '((nil . select-append-default)
	(TEXT . select-append-to-text)
	(STRING . select-append-to-string)
	(COMPOUND_TEXT . select-append-to-compound-text)
	(CF_TEXT . select-append-to-cf-text)
	))

;; Types listed here have buffer-kill handlers
(setq selection-buffer-killed-alist
      '((nil . select-buffer-killed-default)
	(TEXT . select-buffer-killed-text)
	(STRING . select-buffer-killed-text)
	(COMPOUND_TEXT . select-buffer-killed-text)
	(CF_TEXT . select-buffer-killed-text)))

;; Lists of types that are coercible (can be converted to other types)
(setq selection-coercible-types '(TEXT STRING COMPOUND_TEXT))

;;; select.el ends here
