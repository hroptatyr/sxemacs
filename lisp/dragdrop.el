;;; dragdrop.el --- window system-independent Drag'n'Drop support.

;; Copyright (C) 1998 Oliver Graf <ograf@fga.de>

;; Maintainer: SXEmacs Development Team, Oliver Graf <ograf@fga.de>
;; Keywords: mouse, gui, dumped

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

;; This file is dumped with SXEmacs (when drag'n'drop support is compiled in).

;;; Code:

;; we need mouse-set-point
(require 'mouse)
(provide 'dragdrop)

;; I think this is a better name for the custom group
;; looks better in the menu and the group display as dragdrop
;; Anyway: is dragdrop- a good prefix for all this?
;; What if someone trys drop<TAB> in the minibuffer?
(defgroup drag-n-drop nil
  "*{EXPERIMENTAL} Window system-independent drag'n'drop support."
  :group 'editing)

(defcustom dragdrop-drop-at-point nil
  "*{EXPERIMENTAL} If non-nil, drop text at the cursor location.
Otherwise, the cursor will be moved to the location of the pointer drop before
text is inserted."
  :type 'boolean
  :group 'drag-n-drop)

(defcustom dragdrop-autoload-tm-view nil
  "*{EXPERIMENTAL} If non-nil, autoload tm-view to decode MIME data.
Otherwise, the buffer is only decoded if tm-view is already available."
  :type 'boolean
  :group 'drag-n-drop)

;; the widget for editing the drop-functions
(define-widget 'dragdrop-function-widget 'list
  "*{EXPERIMENTAL} Widget for editing drop dispatch functions."
  :args `((choice :tag "Function"
		  (function-item experimental-dragdrop-drop-url-default)
		  (function-item experimental-dragdrop-drop-mime-default)
		  (function-item experimental-dragdrop-drop-log-function)
		  (function :tag "Other" nil))
	  (choice :tag "Button" :value t
		  (choice-item :tag "Ignore" t)
		  (choice-item 0) (choice-item 1) (choice-item 2)
		  (choice-item 3) (choice-item 4) (choice-item 5)
		  (choice-item 6) (choice-item 7))
	  (radio-button-choice :tag "Modifiers"
			       (const :tag "Ignore Modifier Keys" t)
			       (checklist :greedy t
					  :format "Modifier Keys:\n%v"
					  :extra-offset 6
					  (const shift)
					  (const control)
					  (const meta)
					  (const alt)
					  (const hyper)
					  (const super)))
	  (repeat :inline t :value nil :tag "Extra Function Arguments"
		  (sexp :tag "Arg" :value nil)))
  :value '(nil t t))

(defcustom experimental-dragdrop-drop-functions '((experimental-dragdrop-drop-url-default t t)
						  (experimental-dragdrop-drop-mime-default t t))
  "*{EXPERIMENTAL} This is the standart drop function search list.
Each element is a list of a function, a button selector, a modifier
selector and optional argumets to the function call.
The function must accept at least two arguments: first is the event
of the drop, second the object data, followed by any of the optional
arguments provided in this list.
The functions are called in order, until one returns t."
  :group 'drag-n-drop
  :type '(repeat dragdrop-function-widget))

(defgroup dnd-debug nil
  "*{EXPERIMENTAL} Drag'n'Drop debugging options."
  :group 'drag-n-drop)

(defcustom dragdrop-drop-log nil
  "*{EXPERIMENTAL} If non-nil, every drop is logged.
The name of the buffer is set in the custom 'dragdrop-drop-log-name"
  :group 'dnd-debug
  :type 'boolean)

(defcustom dragdrop-drop-log-name "*drop log buffer*"
  "*{EXPERIMENTAL} The name of the buffer used to log drops.
Set dragdrop-drop-log to non-nil to enable this feature."
  :group 'dnd-debug
  :type 'string)

(defvar dragdrop-drop-log-buffer nil
  "*{EXPERIMENTAL} Buffer to log drops in debug mode.")

;;
;; Drop API
;;
(defun dragdrop-drop-dispatch (object)
  "*{EXPERIMENTAL} This function identifies DROP type misc-user-events.
It calls functions which will handle the drag."
  (let ((event current-mouse-event))
    (and dragdrop-drop-log
	 (experimental-dragdrop-drop-log-function event object))
    (dragdrop-drop-find-functions event object)))

(defun dragdrop-drop-find-functions (event object)
  "Finds valid drop-handle functions and executes them to dispose the drop.
It does this by looking for extent-properties called
'experimental-dragdrop-drop-functions and for variables named like this."
  (catch 'dragdrop-drop-is-done
    (and (event-over-text-area-p event)
	 ;; let's search the extents
	 (catch 'dragdrop-extents-done
	   (let ((window (event-window event))
		 (pos (event-point event))
		 (cpos (event-closest-point event))
		 (buffer nil))
	     (or window (throw 'dragdrop-extents-done nil))
	     (or pos (setq pos cpos))
	     (select-window window)
	     (setq buffer (window-buffer))
	     (let ((ext (extent-at pos buffer 'experimental-dragdrop-drop-functions)))
	       (while (not (eq ext nil))
		 (dragdrop-drop-do-functions
		  (extent-property ext 'experimental-dragdrop-drop-functions)
		  event
		  object)
		 (setq ext (extent-at pos buffer
				      'experimental-dragdrop-drop-functions
				      ext)))))))
    ;; now look into the variable experimental-dragdrop-drop-functions
    (dragdrop-drop-do-functions experimental-dragdrop-drop-functions event object)))

(defun dragdrop-compare-mods (first-mods second-mods)
  "Returns t if both first-mods and second-mods contain the same elements.
Order is not important."
  (let ((moda (copy-sequence first-mods))
	(modb (copy-sequence second-mods)))
    (while (and (not (eq moda ()))
		(not (eq modb ())))
      (setq modb (delete (car moda) modb))
      (setq moda (delete (car moda) moda)))
    (and (eq moda ())
	 (eq modb ()))))

(defun dragdrop-drop-do-functions (drop-funs event object)
  "Calls all functions in drop-funs with object until one returns t.
Returns t if one of drop-funs returns t. Otherwise returns nil."
  (let ((flist nil)
	(button (event-button event))
	(mods (event-modifiers event)))
    (while (not (eq drop-funs ()))
      (setq flist (car drop-funs))
      (and (or (eq (cadr flist) t)
	       (= (cadr flist) button))
	   (or (eq (caddr flist) t)
	       (dragdrop-compare-mods (caddr flist) mods))
	   (apply (car flist) `(,event ,object ,@(cdddr flist)))
	   ;; (funcall (car flist) event object)
	   (throw 'dragdrop-drop-is-done t))
      (setq drop-funs (cdr drop-funs))))
  nil)

(defun experimental-dragdrop-drop-log-function (event object &optional message buffer)
  "*{EXPERIMENTAL} Logs any drops into a buffer.
If buffer is nil, it inserts the data into a buffer called after
dragdrop-drop-log-name.
If dragdrop-drop-log is non-nil, this is done automatically for each drop.
The function always returns nil."
  (save-excursion
    (cond ((buffer-live-p buffer)
	   (set-buffer buffer))
	  ((stringp buffer)
	   (set-buffer (get-buffer-create buffer)))
	  ((buffer-live-p dragdrop-drop-log-buffer)
	   (set-buffer dragdrop-drop-log-buffer))
	  (t
	   (setq dragdrop-drop-log-buffer (get-buffer-create dragdrop-drop-log-name))
	   (set-buffer dragdrop-drop-log-buffer)))
    (insert (format "* %s: %s\n"
		    (current-time-string)
		    (if message message "received a drop")))
    (insert (format "  at %d,%d (%d,%d) with button %d and mods %s\n"
		    (event-x event)
		    (event-y event)
		    (event-x-pixel event)
		    (event-y-pixel event)
		    (event-button event)
		    (event-modifiers event)))
    (insert (format "  data is of type %s (%d %s)\n"
	     (cond ((eq (car object) 'dragdrop-URL) "URL")
		   ((eq (car object) 'dragdrop-MIME) "MIME")
		   (t "UNKNOWN"))
	     (length (cdr object))
	     (if (= (length (cdr object)) 1) "element" "elements")))
    (let ((i 1)
	  (data (cdr object)))
      (while (not (eq data ()))
	(insert (format "    Element %d: %S\n"
			i (car data)))
	(setq i (1+ i))
	(setq data (cdr data))))
    (insert "----------\n"))
  nil)

(defun experimental-dragdrop-drop-url-default (event object)
  "*{EXPERIMENTAL} Default handler for dropped URL data.
Finds files and URLs. Returns nil if object does not contain URL data."
  (cond ((eq (car object) 'dragdrop-URL)
	 (let ((data (cdr object))
	       (frame (event-channel event))
	       (x pop-up-windows)
	       (window (event-window event)))
	   (setq pop-up-windows nil)
	   (while (not (eq data ()))
	     (cond ((dragdrop-is-some-url "file" (car data))
		    ;; if it is some file, pop it to a buffer
		    (cond (window
			   (select-window window)))
		    (switch-to-buffer (find-file-noselect
				       (substring (car data) 5))))
		   ;; to-do: open ftp URLs with efs...
		   (t
		    ;; some other URL, try to fire up some browser for it
		    (if-fboundp 'browse-url
			(browse-url (car data))
		      (display-message 'error
			"Can't show URL, no browser selected"))))
	     (undo-boundary)
	     (setq data (cdr data)))
	   (make-frame-visible frame)
	   (setq pop-up-windows x)
	   t))
	(t nil)))

(defun experimental-dragdrop-drop-mime-default (event object)
  "*{EXPERIMENTAL} Default handler for dropped MIME data.
Inserts text into buffer, creates MIME buffers for other types.
Returns nil if object does not contain MIME data."
  (cond ((eq (car object) 'dragdrop-MIME)
	 (let ((ldata (cdr object))
	       (frame (event-channel event))
	       (x pop-up-windows)
	       (data nil))
	   ;; how should this be handled???
	   ;; insert drops of text/* into buffer
	   ;; create new buffer if pointer is outside buffer...
	   ;; but there are many other ways...
	   ;;
	   ;; first thing: check if it's only text/plain and if the
	   ;; drop happened inside some buffer. if yes insert it into
	   ;; this buffer (hope it is not encoded in some MIME way)
	   ;;
	   ;; Remember: ("text/plain" "dosnotmatter" "somedata")
	   ;; drops are inserted at mouse-point, if inside a buffer
	   (while (not (eq ldata ()))
	     (setq data (car ldata))
	     (if (and (listp data)
		      (= (length data) 3)
		      (listp (car data))
		      (stringp (caar data))
		      (string= (caar data) "text/plain")
		      (event-over-text-area-p event))
		 (let ((window (event-window event)))
		   (and window
			(select-window window))
		   (and (not dragdrop-drop-at-point)
			(mouse-set-point event))
		   (insert (caddr data)))
	       (let ((buf (get-buffer-create "*MIME-Drop data*")))
		 (set-buffer buf)
		 (pop-to-buffer buf nil frame)
		 (or (featurep 'tm-view)
		     (and dragdrop-autoload-tm-view
			  (require 'tm-view)))
		 (cond ((stringp data)
			;; this is some raw MIME stuff
			;; create some buffer and let tm do the job
			;;
			;; this is always the same buffer!!!
			;; change?
			(erase-buffer)
			(insert data)
			(and (featurep 'tm-view)
			     (declare-fboundp (mime/viewer-mode buf))))
		       ((and (listp data)
			     (= (length data) 3))
			;; change the internal content-type representation to the
			;; way tm does it ("content/type" (key . value)*)
			;; but for now list will do the job
			;;
			;; this is always the same buffer!!!
			;; change?
			(erase-buffer)
			(insert (caddr data))
			(and (featurep 'tm-view)
			     ;; this list of (car data) should be done before
			     ;; enqueing the event
			     (declare-fboundp (mime/viewer-mode buf (car data) (cadr data)))))
		       (t
			(display-message 'error "Wrong drop data")))))
	     (undo-boundary)
	     (setq ldata (cdr ldata)))
	   (make-frame-visible frame)
	   (setq pop-up-windows x))
	 t)
	(t nil)))

(defun dragdrop-is-some-url (method url)
  "Returns true if method equals the start of url.
If method does not end into ':' this is appended before the
compare."
  (cond ((and (stringp url)
	      (stringp method)
	      (> (length url) (length method)))
	 ;; is this ?: check efficient enough?
	 (if (not (string= (substring method -1) ":"))
	     (setq method (concat method ":")))
	 (string= method (substring url 0 (length method))))
	(t nil)))

;;
;; Drag API
;;
(defun experimental-dragdrop-drag (event object)
  "*{EXPERIMENTAL} The generic drag function.
Tries to do the best with object in the selected protocol.
Object must comply to the standart drag'n'drop object
format."
  (error "Not implemented"))

(defun experimental-dragdrop-drag-region (event begin end)
  "*{EXPERIMENTAL} Drag a region.
This function uses special data types if the low-level
protocol requires it. It does so by calling
dragdrop-drag-pure-text."
  (experimental-dragdrop-drag-pure-text event
			   (buffer-substring-no-properties begin end)))

(defun experimental-dragdrop-drag-pure-text (event text)
  "*{EXPERIMENTAL} Drag text-only data.
Takes care of special low-level protocol data types.
Text must be a list of strings."
  (error "Not implemented"))

(defun experimental-dragdrop-drag-pure-file (event file)
  "*{EXPERIMENTAL} Drag filepath-only data.
Takes care of special low-level protocol data types.
file must be a list of strings."
  (error "Not implemented"))

;;
;; The following ones come from frame.el but the better belong here
;; until changed
;;
(defun cde-start-drag (event type data)
  "Implement the CDE drag operation.
Calls the internal function cde-start-drag-internal to do the actual work."
  (interactive "_eXX")
  (if (featurep 'cde)
      ;; Avoid build-time doc string warning by calling the function
      ;; in the following roundabout way:
      (funcall (intern "cde-start-drag-internal")
	       event type data)
    (error "CDE functionality not compiled in.")))

(defun cde-start-drag-region (event begin end)
  "Implement the CDE drag operation for a region.
Calls the internal function CDE-start-drag-internal to do the actual work.
This always does buffer transfers."
  ;; Oliver Graf <ograf@fga.de>
  (interactive "_er")
  (if (featurep 'cde)
      (funcall (intern "cde-start-drag-internal")
	       event nil (list (buffer-substring-no-properties begin end)))
    (error "CDE functionality not compiled in.")))

;; the OffiX drag stuff will soon move also (perhaps mouse.el)
;; if the drag event is done
(defun offix-start-drag (event data &optional type)
  "Implement the OffiX drag operation.
Calls the internal function offix-start-drag-internal to do the actual work.
If type is not given, DndText is assumed."
  ;; Oliver Graf <ograf@fga.de>
  (interactive "esi")
  (if (featurep 'offix)
      (funcall (intern "offix-start-drag-internal") event data type)
    (error "OffiX functionality not compiled in.")))

(defun offix-start-drag-region (event begin end)
  "Implement the OffiX drag operation for a region.
Calls the internal function offix-start-drag-internal to do the actual work.
This always assumes DndText as type."
  ;; Oliver Graf <ograf@fga.de>
  (interactive "_er")
  (if (featurep 'offix)
      (funcall (intern "offix-start-drag-internal")
	       event (buffer-substring-no-properties begin end))
    (error "OffiX functionality not compiled in.")))

;;; dragdrop.el ends here
