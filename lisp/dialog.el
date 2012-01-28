;;; dialog.el --- Dialog-box support for XEmacs

;; Copyright (C) 1991-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, internal, dumped

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

;; This file is dumped with SXEmacs (when dialog boxes are compiled in).

;; Dialog boxes are non-modal at the C level, but made modal at the
;; Lisp level via hacks in functions such as yes-or-no-p-dialog-box
;; below.  Perhaps there should be truly modal dialog boxes
;; implemented at the C level for safety.  All code using dialog boxes
;; should be careful to assume that the environment, for example the
;; current buffer, might be completely different after returning from
;; yes-or-no-p-dialog-box, but such code is difficult to write and test.

;;; Code:
(defun yes-or-no-p-dialog-box (prompt)
  "Ask user a yes-or-no question with a popup dialog box.
Return t if the answer is \"yes\", nil if \"no\".  Takes one argument,
the question string to display."
  (save-selected-frame
    (make-dialog-box 'question
		     :question prompt
		     :modal t
		     :buttons '(["Yes" (dialog-box-finish t)]
				["No" (dialog-box-finish nil)]
				nil
				["Cancel" (dialog-box-cancel)]))))

;; FSF has a similar function `x-popup-dialog'.
(defun get-dialog-box-response (position contents)
  "Pop up a dialog box and return user's selection.
POSITION specifies which frame to use.
This is normally an event or a window or frame.
If POSITION is t or nil, it means to use the frame the mouse is on.
The dialog box appears in the middle of the specified frame.

CONTENTS specifies the alternatives to display in the dialog box.
It is a list of the form (TITLE ITEM1 ITEM2...).
Each ITEM is a cons cell (STRING . VALUE).
The return value is VALUE from the chosen item.

An ITEM may also be just a string--that makes a nonselectable item.
An ITEM may also be nil--that means to put all preceding items
on the left of the dialog box and all following items on the right."
  (cond
   ((eventp position)
    (select-frame (event-frame position)))
   ((framep position)
    (select-frame position))
   ((windowp position)
    (select-window position)))
  (make-dialog-box 'question
		   :question (car contents)
		   :modal t
		   :buttons
		   (mapcar #'(lambda (x)
			       (cond
				((null x)
				 nil)
				((stringp x)
				 ;;this will never get selected
				 `[,x 'ignore nil])
				(t
				 `[,(car x) (dialog-box-finish ',(cdr x)) t])))
			   (cdr contents))))

(defun message-box (fmt &rest args)
  "Display a message, in a dialog box if possible.
If the selected device has no dialog-box support, use the echo area.
The arguments are the same as to `format'.

If the only argument is nil, clear any existing message; let the
minibuffer contents show."
  (if (and (null fmt) (null args))
      (progn
	(clear-message nil)
	nil)
    (let ((str (apply 'format fmt args)))
      (if (device-on-window-system-p)
	  (get-dialog-box-response nil (list str (cons "%_OK" t)))
	(display-message 'message str))
      str)))

(defun message-or-box (fmt &rest args)
  "Display a message in a dialog box or in the echo area.
If this command was invoked with the mouse, use a dialog box.
Otherwise, use the echo area.
The arguments are the same as to `format'.

If the only argument is nil, clear any existing message; let the
minibuffer contents show."
  (if (should-use-dialog-box-p)
      (apply 'message-box fmt args)
    (apply 'message fmt args)))

(defun make-dialog-box (type &rest cl-keys)
  "Pop up a dialog box.
TYPE is a symbol, the type of dialog box.  Remaining arguments are
keyword-value pairs, specifying the particular characteristics of the
dialog box.  The allowed keywords are particular to each type, but
some standard keywords are common to many types:

:title
  The title of the dialog box's window.

:modal
  If true, indicates that XEmacs will wait until the user is \"done\"
  with the dialog box (usually, this means that a response has been
  given).  Typically, the response is returned.  NOTE: Some dialog
  boxes are always modal.  If the dialog box is modal, `make-dialog-box'
  returns immediately.  The return value will be either nil or a
  dialog box handle of some sort, e.g. a frame for type `general'.

---------------------------------------------------------------------------

Recognized types are

general
  A dialog box consisting of an XEmacs glyph, typically a `layout'
  widget specifying a dialog box arrangement.  This is the most
  general and powerful dialog box type, but requires more work than
  the other types below.

question
  A simple dialog box that displays a question and contains one or
  more user-defined buttons to specify possible responses. (This is
  compatible with the old built-in dialog boxes formerly specified
  using `popup-dialog-box'.)

file
  A file dialog box, of the type typically used in the window system
  XEmacs is running on.

color
  A color picker.

find
  A find dialog box.

font
  A font chooser.

print
  A dialog box used when printing (e.g. number of pages, printer).

page-setup
  A dialog box for setting page options (e.g. margins) for printing.

replace
  A find/replace dialog box.

---------------------------------------------------------------------------

For type `general':

This type creates a frame and puts the specified widget layout in it.
\(Currently this is done by eliminating all areas but the gutter and placing
the layout there; but this is an implementation detail and may change.)

The keywords allowed for `general' are

:spec
  The widget spec -- anything that can be passed to `make-glyph'.
:title
  The title of the frame.
:parent
  The frame is made a child of this frame (defaults to the selected frame).
:properties
  Additional properties of the frame, as well as `dialog-frame-plist'.
:autosize
  If t the frame is sized to exactly fit the widgets given by :spec.

---------------------------------------------------------------------------

For type `question':

The keywords allowed are

:modal
  t or nil.  When t, the dialog box callback should exit the dialog box
  using the functions `dialog-box-finish' or `dialog-box-cancel'.
:title
  The title of the frame.
:question
  A string, the question.
:buttons
  A list, describing the buttons below the question.  Each of these is a
  vector, the syntax of which is essentially the same as that of popup menu
  items.  They may have any of the following forms:

   [ \"name\" callback <active-p> ]
   [ \"name\" callback <active-p> \"suffix\" ]
   [ \"name\" callback :<keyword> <value>  :<keyword> <value> ... ]

  The name is the string to display on the button; it is filtered through the
  resource database, so it is possible for resources to override what string
  is actually displayed.

  Accelerators can be indicated in the string by putting the sequence
  \"%_\" before the character corresponding to the key that will invoke
  the button.  Uppercase and lowercase accelerators are equivalent.  The
  sequence \"%%\" is also special, and is translated into a single %.

  If the `callback' of a button is a symbol, then it must name a command.
  It will be invoked with `call-interactively'.  If it is a list, then it is
  evaluated with `eval'.

  One (and only one) of the buttons may be `nil'.  This marker means that all
  following buttons should be flushright instead of flushleft.

  Though the keyword/value syntax is supported for dialog boxes just as in
  popup menus, the only keyword which is both meaningful and fully implemented
  for dialog box buttons is `:active'.

---------------------------------------------------------------------------

For type `file':

The keywords allowed are

:initial-filename
  The initial filename to be placed in the dialog box (defaults to nothing).
:initial-directory
  The initial directory to be selected in the dialog box (defaults to the
  current buffer's `default-directory).
:filter-list
  A list of                     (filter-desc filter ...)
:title
  The title of the dialog box (defaults to \"Open\").
:allow-multi-select             t or nil
:create-prompt-on-nonexistent   t or nil
:overwrite-prompt               t or nil
:file-must-exist                t or nil
:no-network-button              t or nil
:no-read-only-return            t or nil

---------------------------------------------------------------------------

For type `directory':

The keywords allowed are

:initial-directory
  The initial directory to be selected in the dialog box (defaults to the
  current buffer's `default-directory).
:title
  The title of the dialog box (defaults to \"Open\").

---------------------------------------------------------------------------

For type `print':

This invokes the Windows standard Print dialog.
This dialog is usually invoked when the user selects the Print command.
After the user presses OK, the program should start actual printout.

The keywords allowed are

:device
  An 'msprinter device.
:print-settings
  A printer settings object.
:allow-selection
  t or nil -- whether the \"Selection\" button is enabled (defaults to nil).
:allow-pages
  t or nil -- whether the \"Pages\" button and associated edit controls
  are enabled (defaults to t).
:selected-page-button
  `all', `selection', or `pages' -- which page button is initially
  selected.

Exactly one of :device and :print-settings must be given.

The function brings up the Print dialog, where the user can
select a different printer and/or change printer options.  Connection
name can change as a result of selecting a different printer device.  If
a device is specified, then changes are stored into the settings object
currently selected into that printer.  If a settings object is supplied,
then changes are recorded into it, and, it is selected into a
printer, then changes are propagated to that printer
too.

Return value is nil if the user has canceled the dialog.  Otherwise, it
is a new plist, with the following properties:
  name                   Printer device name, even if unchanged by the user.
  from-page              First page to print, 1-based.  Returned if
			 `selected-page-button' is `pages'.
			 user, then this value is not included in the plist.
  to-page                Last page to print, inclusive, 1-based.  Returned if
			 `selected-page-button' is `pages'.
  copies                 Number of copies to print.  Always returned.
  selected-page-button   Which page button was selected (`all', `selection',
			 or `pages').

The DEVICE is destroyed and an error is signaled in case of
initialization problem with the new printer.

See also the `page-setup' dialog box type.

---------------------------------------------------------------------------

For type `page-setup':

This invokes the Windows standard Page Setup dialog.
This dialog is usually invoked in response to the Page Setup command,
and used to choose such parameters as page orientation, print margins
etc.  Note that this dialog contains the \"Printer\" button, which
invokes the Printer Setup dialog so that the user can update the
printer options or even select a different printer as well.

The keywords allowed are

:device
  An 'msprinter device.
:print-settings
  A printer settings object.
:properties
  A plist of job properties.

Exactly one of these keywords must be given.

The function brings up the Page Setup dialog, where the user
can select a different printer and/or change printer options.
Connection name can change as a result of selecting a different printer
device.  If a device is specified, then changes are stored into the
settings object currently selected into that printer.  If a settings
object is supplied, then changes are recorded into it, and, it is
selected into a printer, then changes are propagated to that printer
too.

:properties specifies a plist of job properties;
see `default-msprinter-frame-plist' for the complete list.  The plist
is used to initialize the dialog.

Return value is nil if the user has canceled the dialog.  Otherwise,
it is a new plist, containing the new list of properties.

NOTE: The margin properties (returned by this function) are *NOT* stored
into the print-settings or device object.

The DEVICE is destroyed and an error is signaled in case of
initialization problem with the new printer.

See also the `print' dialog box type."
  (flet ((dialog-box-modal-loop (thunk)
	   (let* ((frames (frame-list))
		  (result
		   ;; ok, this is extremely tricky.  normally a modal
		   ;; dialog will pop itself down using (dialog-box-finish)
		   ;; or (dialog-box-cancel), which throws back to this
		   ;; catch.  but question dialog boxes pop down themselves
		   ;; regardless, so a badly written question dialog box
		   ;; that does not use (dialog-box-finish) could seriously
		   ;; wedge us.  furthermore, we disable all other frames
		   ;; in order to implement modality; we need to restore
		   ;; them before the dialog box is destroyed, because
		   ;; otherwise windows at least will notice that no top-
		   ;; level window can have the focus and will shift the
		   ;; focus to a different app, raising it and obscuring us.
		   ;; so we create `delete-dialog-box-hook', which is
		   ;; called right *before* the dialog box gets destroyed.
		   ;; here, we put a hook on it, and when it's our dialog
		   ;; box and not someone else's that's being destroyed,
		   ;; we reenable all the frames and remove the hook.
		   ;; BUT ...  we still have to deal with exiting the
		   ;; modal loop in case it doesn't happen before us.
		   ;; we can't do this until after the callbacks for this
		   ;; dialog box get executed, and that doesn't happen until
		   ;; after the dialog box is destroyed.  so to keep things
		   ;; synchronous, we enqueue an eval event, which goes into
		   ;; the same queue as the misc-user events encapsulating
		   ;; the dialog callbacks and will go after it (because
		   ;; destroying the dialog box happens after processing
		   ;; its selection).  if the dialog boxes are written
		   ;; properly, we don't see this eval event, because we've
		   ;; already exited our modal loop. (Thus, we make sure the
		   ;; function given in this eval event is actually defined
		   ;; and does nothing.) If we do see it, though, we know
		   ;; that we encountered a badly written dialog box and
		   ;; need to exit now.  Currently we just return nil, but
		   ;; maybe we should signal an error or issue a warning.
		   (catch 'internal-dialog-box-finish
		     (let ((id (eval thunk))
			   (sym (gensym)))
		       (fset sym
			     `(lambda (did)
				(when (eq ',id did)
				  (mapc 'enable-frame ',frames)
				  (enqueue-eval-event
				   'internal-make-dialog-box-exit did)
				  (remove-hook 'delete-dialog-box-hook
					       ',sym))))
		       (if (framep id)
			   (add-hook 'delete-frame-hook sym)
			 (add-hook 'delete-dialog-box-hook sym))
		       (mapc 'disable-frame frames)
		       (block nil
			 (while t
			   (let ((event (next-event)))
			     (if (and (eval-event-p event)
				      (eq (event-function event)
					  'internal-make-dialog-box-exit)
				      (eq (event-object event) id))
				 (return '(nil))
			       (dispatch-event event)))))))))
	     (if (listp result)
		 (car result)
	       (signal 'quit nil)))))
    (case type
      (general
	(cl-parsing-keywords
	    ((:title "XEmacs")
	     (:parent (selected-frame))
	     :modal
	     :properties
	     :autosize
	     :spec)
	    ()
	  (flet ((create-dialog-box-frame ()
		   (let* ((ftop (frame-property cl-parent 'top))
			  (fleft (frame-property cl-parent 'left))
			  (fwidth (frame-pixel-width cl-parent))
			  (fheight (frame-pixel-height cl-parent))
			  (fonth (font-height (face-font 'default)))
			  (fontw (font-width (face-font 'default)))
			  (cl-properties (append cl-properties
						 dialog-frame-plist))
			  (dfheight (plist-get cl-properties 'height))
			  (dfwidth (plist-get cl-properties 'width))
			  (unmapped (plist-get cl-properties
					       'initially-unmapped))
			  (gutter-spec cl-spec)
			  (name (or (plist-get cl-properties 'name) "XEmacs"))
			  (frame nil))
		     (plist-remprop cl-properties 'initially-unmapped)
		     ;; allow the user to just provide a glyph
		     (or (glyphp cl-spec) (setq cl-spec (make-glyph cl-spec)))
		     (setq gutter-spec (copy-sequence "\n"))
		     (set-extent-begin-glyph (make-extent 0 1 gutter-spec)
					     cl-spec)
		     ;; under FVWM at least, if I don't specify the
		     ;; initial position, it ends up always at (0, 0).
		     ;; xwininfo doesn't tell me that there are any
		     ;; program-specified position hints, so it must be
		     ;; an FVWM bug.  So just be smashing and position in
		     ;; the center of the selected frame.
		     (setq frame
			   (make-frame
			    (append cl-properties
				    `(popup ,cl-parent initially-unmapped t
					    menubar-visible-p nil
					    has-modeline-p nil
					    default-toolbar-visible-p nil
					    top-gutter-visible-p t
					    top-gutter-height ,
					    (* dfheight fonth)
					    top-gutter ,gutter-spec
					    minibuffer none
					    name ,name
					    modeline-shadow-thickness 0
					    vertical-scrollbar-visible-p nil
					    horizontal-scrollbar-visible-p nil
					    unsplittable t
					    internal-border-width 8
					    left ,(+ fleft (- (/ fwidth 2)
							      (/ (* dfwidth
								    fontw)
								 2)))
					    top ,(+ ftop (- (/ fheight 2)
							    (/ (* dfheight
								  fonth)
							       2)))))))
		     (set-face-foreground 'modeline [default foreground] frame)
		     (set-face-background 'modeline [default background] frame)
		     ;; resize before mapping
		     (when cl-autosize
		       (set-frame-pixel-size
			frame
			(image-instance-width
			 (glyph-image-instance cl-spec
					       (frame-selected-window frame)))
			(image-instance-height
			 (glyph-image-instance cl-spec
					       (frame-selected-window frame)))))
		     ;; somehow, even though the resizing is supposed
		     ;; to be while the frame is not visible, a
		     ;; visible resize is perceptible
		     (unless unmapped (make-frame-visible frame))
		     (let ((newbuf (generate-new-buffer " *dialog box*")))
		       (set-buffer-dedicated-frame newbuf frame)
		       (set-frame-property frame 'dialog-box-buffer newbuf)
		       (set-window-buffer (frame-root-window frame) newbuf)
		       (with-current-buffer newbuf
			 (set (make-local-variable 'frame-title-format)
			      cl-title)
			 (add-local-hook 'delete-frame-hook
					 #'(lambda (frame)
					     (kill-buffer
					      (frame-property
					       frame
					       'dialog-box-buffer))))))
		     frame)))
	    (if cl-modal
		(dialog-box-modal-loop '(create-dialog-box-frame))
	      (create-dialog-box-frame)))))
      (question
	(cl-parsing-keywords
	    ((:modal nil))
	    t
	  (remf cl-keys :modal)
	  (if cl-modal
	      (dialog-box-modal-loop `(make-dialog-box-internal ',type
								',cl-keys))
	    (make-dialog-box-internal type cl-keys))))
      (t
	(make-dialog-box-internal type cl-keys)))))

(defun dialog-box-finish (result)
  "Exit a modal dialog box, returning RESULT.
This is meant to be executed from a dialog box callback function."
  (throw 'internal-dialog-box-finish (list result)))

(defun dialog-box-cancel ()
  "Cancel a modal dialog box.
This is meant to be executed from a dialog box callback function."
  (throw 'internal-dialog-box-finish 'cancel))

;; an eval event, used as a trigger inside of the dialog modal loop.
(defun internal-make-dialog-box-exit (did)
  nil)

(make-obsolete 'popup-dialog-box 'make-dialog-box)
(defun popup-dialog-box (desc)
  "Obsolete equivalent of (make-dialog-box 'question ...).

\(popup-dialog-box (QUESTION BUTTONS ...)

is equivalent to

\(make-dialog-box 'question :question QUESTION :buttons BUTTONS)"
  (check-argument-type 'stringp (car desc))
  (or (consp (cdr desc))
      (error 'syntax-error
	     "Dialog descriptor must supply at least one button"
	     desc))
  (make-dialog-box 'question :question (car desc) :buttons (cdr desc)))

;;; dialog.el ends here
