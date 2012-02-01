;;; frame.el --- multi-frame management independent of window systems.

;; Copyright (C) 1993-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: FSF 19.30.

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

(defgroup frames nil
  "Support for Emacs frames and window systems."
  :group 'environment)

; No need for `frame-creation-function'.

;;; The initial value given here for this must ask for a minibuffer.
;;; There must always exist a frame with a minibuffer, and after we
;;; delete the terminal frame, this will be the only frame.
(defcustom initial-frame-plist '(minibuffer t)
  "Plist of frame properties for creating the initial X window frame.
You can set this in your `.emacs' file; for example,
  (setq initial-frame-plist '(top 1 left 1 width 80 height 55))
Properties specified here supersede the values given in `default-frame-plist'.
The format of this can also be an alist for backward compatibility.

If the value calls for a frame without a minibuffer, and you have not created
a minibuffer frame on your own, one is created according to
`minibuffer-frame-plist'.

You can specify geometry-related options for just the initial frame
by setting this variable in your `.emacs' file; however, they won't
take effect until Emacs reads `.emacs', which happens after first creating
the frame.  If you want the frame to have the proper geometry as soon
as it appears, you need to use this three-step process:
* Specify X resources to give the geometry you want.
* Set `default-frame-plist' to override these options so that they
  don't affect subsequent frames.
* Set `initial-frame-plist' in a way that matches the X resources,
  to override what you put in `default-frame-plist'."
  :type 'plist
  :group 'frames)

(defcustom minibuffer-frame-plist '(width 80 height 2 menubar-visible-p nil
				       default-toolbar-visible-p nil)
  "Plist of frame properties for initially creating a minibuffer frame.
You can set this in your `.emacs' file; for example,
  (setq minibuffer-frame-plist '(top 1 left 1 width 80 height 2))
Properties specified here supersede the values given in
`default-frame-plist'.
The format of this can also be an alist for backward compatibility."
  :type 'plist
  :group 'frames)

(defcustom pop-up-frame-plist nil
  "Plist of frame properties used when creating pop-up frames.
Pop-up frames are used for completions, help, and the like.
This variable can be set in your init file, like this:
  (setq pop-up-frame-plist '(width 80 height 20))
These supersede the values given in `default-frame-plist'.
The format of this can also be an alist for backward compatibility."
  :type 'plist
  :group 'frames)

(setq pop-up-frame-function
      (function (lambda ()
		  (make-frame pop-up-frame-plist))))

(defcustom special-display-frame-plist '(height 14 width 80 unsplittable t)
  "*Plist of frame properties used when creating special frames.
Special frames are used for buffers whose names are in
`special-display-buffer-names' and for buffers whose names match
one of the regular expressions in `special-display-regexps'.
This variable can be set in your init file, like this:
  (setq special-display-frame-plist '(width 80 height 20))
These supersede the values given in `default-frame-plist'.
The format of this can also be an alist for backward compatibility."
  :type 'plist
  :group 'frames)

(defun safe-alist-to-plist (cruftiness)
  (if (consp (car cruftiness))
      (alist-to-plist cruftiness)
    cruftiness))

;; Display BUFFER in its own frame, reusing an existing window if any.
;; Return the window chosen.
;; Currently we do not insist on selecting the window within its frame.
;; If ARGS is a plist, use it as a list of frame property specs.
;; #### Change, not compatible with FSF: This stuff is all so incredibly
;; junky anyway that I doubt it makes any difference.
;; If ARGS is a list whose car is t,
;; use (cadr ARGS) as a function to do the work.
;; Pass it BUFFER as first arg, and (cddr ARGS) gives the rest of the args.
(defun special-display-popup-frame (buffer &optional args)
  ;; if we can't display simultaneous multiple frames, just return
  ;; nil and let the normal behavior take over.
  (and (device-on-window-system-p)
       (if (and args (eq t (car args)))
	   (apply (cadr args) buffer (cddr args))
	 (let ((window (get-buffer-window buffer t)))
	   (if window
	       ;; If we have a window already, make it visible.
	       (let ((frame (window-frame window)))
		 (make-frame-visible frame)
		 (raise-frame frame)
		 window)
	     ;; If no window yet, make one in a new frame.
	     (let ((frame
		    (make-frame (append (safe-alist-to-plist args)
					(safe-alist-to-plist
					 special-display-frame-plist)))))
	       (set-window-buffer (frame-selected-window frame) buffer)
	       (set-window-dedicated-p (frame-selected-window frame) t)
	       (frame-selected-window frame)))))))

(setq special-display-function 'special-display-popup-frame)

;;; Handle delete-frame events from the X server.
;(defun handle-delete-frame (event)
;  (interactive "e")
;  (let ((frame (posn-window (event-start event)))
;	(i 0)
;	(tail (frame-list)))
;    (while tail
;      (and (frame-visible-p (car tail))
;	   (not (eq (car tail) frame))
;	  (setq i (1+ i)))
;      (setq tail (cdr tail)))
;    (if (> i 0)
;	(delete-frame frame t)
;      (kill-emacs))))


;;;; Arrangement of frames at startup

;;; 1) Load the window system startup file from the lisp library and read the
;;; high-priority arguments (-q and the like).  The window system startup
;;; file should create any frames specified in the window system defaults.
;;;
;;; 2) If no frames have been opened, we open an initial text frame.
;;;
;;; 3) Once the init file is done, we apply any newly set properties
;;; in initial-frame-plist to the frame.

;; These are now called explicitly at the proper times,
;; since that is easier to understand.
;; Actually using hooks within Emacs is bad for future maintenance. --rms.
;; (add-hook 'before-init-hook 'frame-initialize)
;; (add-hook 'window-setup-hook 'frame-notice-user-settings)

;;; If we create the initial frame, this is it.
(defvar frame-initial-frame nil)

;; Record the properties used in frame-initialize to make the initial frame.
(defvar frame-initial-frame-plist)

(defvar frame-initial-geometry-arguments nil)

(defun canonicalize-frame-plists ()
  (setq initial-frame-plist (safe-alist-to-plist initial-frame-plist))
  (setq default-frame-plist (safe-alist-to-plist default-frame-plist)))

;;; startup.el calls this function before loading the user's init
;;; file - if there is no frame with a minibuffer open now, create
;;; one to display messages while loading the init file.
(defun frame-initialize ()
  ;; In batch mode, we actually use the initial terminal device for output.
  (canonicalize-frame-plists)
  (if (not (noninteractive))
      (progn
	;; Don't call select-frame here - focus is a matter of WM policy.

	;; If there is no frame with a minibuffer besides the terminal
	;; frame, then we need to create the opening frame.  Make sure
	;; it has a minibuffer, but let initial-frame-plist omit the
	;; minibuffer spec.
	(or (delq terminal-frame (minibuffer-frame-list))
	    (progn
	      (setq frame-initial-frame-plist
		    (append initial-frame-plist default-frame-plist))
	      ;; FSFmacs has scroll-bar junk here that we don't need.
	      (setq default-minibuffer-frame
		    (setq frame-initial-frame
			  (make-frame initial-frame-plist
				      (car (delq terminal-device
						 (device-list))))))
	      ;; Delete any specifications for window geometry properties
	      ;; so that we won't reapply them in frame-notice-user-settings.
	      ;; It would be wrong to reapply them then,
	      ;; because that would override explicit user resizing.
	      (setq initial-frame-plist
		    (frame-remove-geometry-props initial-frame-plist))))
	;; At this point, we know that we have a frame open, so we
	;; can delete the terminal device.
	;; (delete-device terminal-device)
	;; Do it the same way Fkill_emacs does it. -slb
	(delete-console terminal-console)
	(setq terminal-frame nil)

	;; FSFmacs sets frame-creation-function here, but no need.
	)))

;;; startup.el calls this function after loading the user's init
;;; file.  Now default-frame-plist and initial-frame-plist contain
;;; information to which we must react; do what needs to be done.
(defun frame-notice-user-settings ()

  ;; FSFmacs has menu-bar junk here that we don't need.

  (canonicalize-frame-plists)

  ;; Creating and deleting frames may shift the selected frame around,
  ;; and thus the current buffer.  Protect against that.  We don't
  ;; want to use save-excursion here, because that may also try to set
  ;; the buffer of the selected window, which fails when the selected
  ;; window is the minibuffer.
  (let ((old-buffer (current-buffer)))

    ;; If the initial frame is still around, apply initial-frame-plist
    ;; and default-frame-plist to it.
    (if (frame-live-p frame-initial-frame)

	;; The initial frame we create above always has a minibuffer.
	;; If the user wants to remove it, or make it a minibuffer-only
	;; frame, then we'll have to delete the selected frame and make a
	;; new one; you can't remove or add a root window to/from an
	;; existing frame.
	;;
	;; NOTE: default-frame-plist was nil when we created the
	;; existing frame.  We need to explicitly include
	;; default-frame-plist in the properties of the screen we
	;; create here, so that its new value, gleaned from the user's
	;; .emacs file, will be applied to the existing screen.
	(if (not (eq (car
		      (or (and (lax-plist-member
				initial-frame-plist 'minibuffer)
			       (list (lax-plist-get initial-frame-plist
						    'minibuffer)))
			  (and (lax-plist-member default-frame-plist
						 'minibuffer)
			       (list (lax-plist-get default-frame-plist
						    'minibuffer)))
			 '(t)))
		     t))
	    ;; Create the new frame.
	    (let (props
		  )
	      ;; If the frame isn't visible yet, wait till it is.
	      ;; If the user has to position the window,
	      ;; Emacs doesn't know its real position until
	      ;; the frame is seen to be visible.

	      (if (frame-property frame-initial-frame 'initially-unmapped)
		  nil
		(while (not (frame-visible-p frame-initial-frame))
		  (sleep-for 1)))
	      (setq props (frame-properties frame-initial-frame))
	      ;; Get rid of `name' unless it was specified explicitly before.
	      (or (lax-plist-member frame-initial-frame-plist 'name)
		  (setq props (lax-plist-remprop props 'name)))
	      (setq props (append initial-frame-plist default-frame-plist
				  props
				  nil))
	      ;; Get rid of `reverse', because that was handled
	      ;; when we first made the frame.
	      (laxputf props 'reverse nil)
	      ;; Get rid of `window-id', otherwise make-frame will
	      ;; think we're trying to setup an external widget.
	      (laxremf props 'window-id)
	      (if (lax-plist-member frame-initial-geometry-arguments 'height)
		  (laxremf props 'height))
	      (if (lax-plist-member frame-initial-geometry-arguments 'width)
		  (laxremf props 'width))
	      (if (lax-plist-member frame-initial-geometry-arguments 'left)
		  (laxremf props 'left))
	      (if (lax-plist-member frame-initial-geometry-arguments 'top)
		  (laxremf props 'top))

	      ;; Now create the replacement initial frame.
	      (make-frame
	       ;; Use the geometry args that created the existing
	       ;; frame, rather than the props we get for it.
	       (append '(user-size t user-position t)
		       frame-initial-geometry-arguments
		       props))
	      ;; The initial frame, which we are about to delete, may be
	      ;; the only frame with a minibuffer.  If it is, create a
	      ;; new one.
	      (or (delq frame-initial-frame (minibuffer-frame-list))
		  (make-initial-minibuffer-frame nil))

	      ;; If the initial frame is serving as a surrogate
	      ;; minibuffer frame for any frames, we need to wean them
	      ;; onto a new frame.  The default-minibuffer-frame
	      ;; variable must be handled similarly.
	      (let ((users-of-initial
		     (filtered-frame-list
		      #'(lambda (frame)
				  (and (not (eq frame frame-initial-frame))
				       (eq (window-frame
					    (minibuffer-window frame))
					   frame-initial-frame))))))
		(if (or users-of-initial
			(eq default-minibuffer-frame frame-initial-frame))

		    ;; Choose an appropriate frame.  Prefer frames which
		    ;; are only minibuffers.
		    (let* ((new-surrogate
			    (car
			     (or (filtered-frame-list
				  #'(lambda (frame)
				      (eq 'only
					  (frame-property frame 'minibuffer))))
				 (minibuffer-frame-list))))
			   (new-minibuffer (minibuffer-window new-surrogate)))

		      (if (eq default-minibuffer-frame frame-initial-frame)
			  (setq default-minibuffer-frame new-surrogate))

		      ;; Wean the frames using frame-initial-frame as
		      ;; their minibuffer frame.
		      (mapcar
		       #'
			(lambda (frame)
			  (set-frame-property frame 'minibuffer
					      new-minibuffer))
			users-of-initial))))

	      ;; Redirect events enqueued at this frame to the new frame.
	      ;; Is this a good idea?
	      ;; Probably not, since this whole redirect-frame-focus
	      ;; stuff is a load of trash, and so is this function we're in.
	      ;; --ben
	      ;(redirect-frame-focus frame-initial-frame new)

	      ;; Finally, get rid of the old frame.
	      (delete-frame frame-initial-frame t))

	  ;; Otherwise, we don't need all that rigamarole; just apply
	  ;; the new properties.
	  (let (newprops allprops tail)
	    (setq allprops (append initial-frame-plist
				   default-frame-plist))
	    (if (lax-plist-member frame-initial-geometry-arguments 'height)
		(laxremf allprops 'height))
	    (if (lax-plist-member frame-initial-geometry-arguments 'width)
		(remf allprops 'width))
	    (if (lax-plist-member frame-initial-geometry-arguments 'left)
		(laxremf allprops 'left))
	    (if (lax-plist-member frame-initial-geometry-arguments 'top)
		(laxremf allprops 'top))
	    (setq tail allprops)
	    ;; Find just the props that have changed since we first
	    ;; made this frame.  Those are the ones actually set by
	    ;; the init file.  For those props whose values we already knew
	    ;; (such as those spec'd by command line options)
	    ;; it is undesirable to specify the parm again
	    ;; once the user has seen the frame and been able to alter it
	    ;; manually.
	    (while tail
	      (let (newval oldval)
		(setq oldval (lax-plist-get frame-initial-frame-plist
					    (car tail)))
		(setq newval (lax-plist-get allprops (car tail)))
		(or (eq oldval newval)
		    (laxputf newprops (car tail) newval)))
	      (setq tail (cddr tail)))
	    (set-frame-properties frame-initial-frame newprops)
	    ;silly FSFmacs junk
	    ;if (lax-plist-member newprops 'font)
	    ;   (frame-update-faces frame-initial-frame))

	    )))

    ;; Restore the original buffer.
    (set-buffer old-buffer)

    ;; Make sure the initial frame can be GC'd if it is ever deleted.
    ;; Make sure frame-notice-user-settings does nothing if called twice.
    (setq frame-initial-frame nil)))

(defun make-initial-minibuffer-frame (device)
  (let ((props (append '(minibuffer only)
		       (safe-alist-to-plist minibuffer-frame-plist))))
    (make-frame props device)))


;;;; Creation of additional frames, and other frame miscellanea

(defun get-other-frame ()
 "Return some frame other than the selected frame, creating one if necessary."
  (let* ((this (selected-frame))
	 ;; search visible frames first
	 (next (next-frame this 'visible-nomini)))
    ;; then search iconified frames
    (if (eq this next)
	(setq next (next-frame 'visible-iconic-nomini)))
    (if (eq this next)
	;; otherwise, make a new frame
	(make-frame)
      next)))

(defun next-multiframe-window ()
  "Select the next window, regardless of which frame it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      t)))

(defun previous-multiframe-window ()
  "Select the previous window, regardless of which frame it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  t)))

(defun make-frame-on-device (type connection &optional props)
  "Create a frame of type TYPE on CONNECTION.
TYPE should be a symbol naming the device type, i.e. one of

x	    An X display.  CONNECTION should be a standard display string
	    such as \"unix:0\", or nil for the display specified on the
	    command line or in the DISPLAY environment variable.  Only if
	    support for X was compiled into XEmacs.
tty	    A standard TTY connection or terminal.  CONNECTION should be
	    a TTY device name such as \"/dev/ttyp2\" (as determined by
	    the Unix command `tty') or nil for XEmacs' standard input
	    and output (usually the TTY in which XEmacs started).  Only
	    if support for TTY's was compiled into XEmacs.
ns	    A connection to a machine running the NeXTstep windowing
	    system.  Not currently implemented.
pc	    A direct-write MS-DOS frame.  Not currently implemented.

PROPS should be a plist of properties, as in the call to `make-frame'.

If a connection to CONNECTION already exists, it is reused; otherwise,
a new connection is opened."
  (make-frame props (make-device type connection props)))

;; Alias, kept temporarily.
(defalias 'new-frame 'make-frame)

; FSFmacs has make-frame here.  We have it in C, so no need for
; frame-creation-function.

(defun filtered-frame-list (predicate &optional device)
  "Return a list of all live frames which satisfy PREDICATE.
If optional second arg DEVICE is non-nil, restrict the frames
 returned to that device."
  (let ((frames (if device (device-frame-list device)
		  (frame-list)))
	good-frames)
    (while (consp frames)
      (if (funcall predicate (car frames))
	  (setq good-frames (cons (car frames) good-frames)))
      (setq frames (cdr frames)))
    good-frames))

(defun minibuffer-frame-list (&optional device)
  "Return a list of all frames with their own minibuffers.
If optional second arg DEVICE is non-nil, restrict the frames
 returned to that device."
  (filtered-frame-list
   #'(lambda (frame)
	       (eq frame (window-frame (minibuffer-window frame))))
   device))

(defun frame-minibuffer-only-p (frame)
  "Return non-nil if FRAME is a minibuffer-only frame."
  (eq (frame-root-window frame) (minibuffer-window frame)))

(defun frame-remove-geometry-props (plist)
  "Return the property list PLIST, but with geometry specs removed.
This deletes all bindings in PLIST for `top', `left', `width',
`height', `user-size' and `user-position' properties.
Emacs uses this to avoid overriding explicit moves and resizings from
the user during startup."
  (setq plist (canonicalize-lax-plist (copy-sequence plist)))
  (mapcar #'(lambda (property)
	      (if (lax-plist-member plist property)
		  (progn
		    (setq frame-initial-geometry-arguments
			  (cons property
				(cons (lax-plist-get plist property)
				      frame-initial-geometry-arguments)))
		    (setq plist (lax-plist-remprop plist property)))))
	  '(height width top left user-size user-position))
  plist)

(defun other-frame (arg)
  "Select the ARG'th different visible frame, and raise it.
All frames are arranged in a cyclic order.
This command selects the frame ARG steps away in that order.
A negative ARG moves in the opposite order.

This sets the window system focus, regardless of the value
of `focus-follows-mouse'."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame 'visible-nomini))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame 'visible-nomini))
      (setq arg (1+ arg)))
    (raise-frame frame)
    (focus-frame frame)
    ;this is a bad idea; you should in general never warp the
    ;pointer unless the user asks for this.  Furthermore,
    ;our version of `set-mouse-position' takes a window,
    ;not a frame.
    ;(set-mouse-position (selected-frame) (1- (frame-width)) 0)
    ;some weird FSFmacs randomness
    ;(if (fboundp 'unfocus-frame)
    ;	(unfocus-frame))))
    ))

;; XEmacs-added utility functions

(defmacro save-selected-frame (&rest body)
  "Execute forms in BODY, then restore the selected frame.
The value returned is the value of the last form in BODY."
  (let ((old-frame (gensym "ssf")))
    `(let ((,old-frame (selected-frame)))
       (unwind-protect
	   (progn ,@body)
	 (select-frame ,old-frame)))))

(defmacro with-selected-frame (frame &rest body)
  "Execute forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY."
  `(save-selected-frame
     (select-frame ,frame)
     ,@body))

; this is in C in FSFmacs
(defun frame-list ()
  "Return a list of all frames on all devices/consoles."
  ;; Lists are copies, so nconc is safe here.
  (apply 'nconc (mapcar 'device-frame-list (device-list))))

(defun frame-type (&optional frame)
  "Return the type of the specified frame (e.g. `x' or `tty').
This is equivalent to the type of the frame's device.
Value is `tty' for a tty frame (a character-only terminal),
`x' for a frame that is an X window,
`ns' for a frame that is a NeXTstep window (not yet implemented),
`stream' for a stream frame (which acts like a stdio stream), and
`dead' for a deleted frame."
  (or frame (setq frame (selected-frame)))
  (if (not (frame-live-p frame)) 'dead
    (device-type (frame-device frame))))

(defun device-or-frame-p (object)
  "Return non-nil if OBJECT is a device or frame."
  (or (devicep object)
      (framep object)))

(defun device-or-frame-type (device-or-frame)
  "Return the type (e.g. `x' or `tty') of DEVICE-OR-FRAME.
DEVICE-OR-FRAME should be a device or a frame object.  See `device-type'
for a description of the possible types."
  (if (devicep device-or-frame)
      (device-type device-or-frame)
    (frame-type device-or-frame)))

(defun fw-frame (obj)
  "Given a frame or window, return the associated frame.
Return nil otherwise."
  (cond ((windowp obj) (window-frame obj))
	((framep obj) obj)
	(t nil)))


;;;; Frame configurations

(defun current-frame-configuration ()
  "Return a list describing the positions and states of all frames.
Its car is `frame-configuration'.
Each element of the cdr is a list of the form (FRAME PLIST WINDOW-CONFIG),
where
  FRAME is a frame object,
  PLIST is a property list specifying some of FRAME's properties, and
  WINDOW-CONFIG is a window configuration object for FRAME."
  (cons 'frame-configuration
	(mapcar (function
		 (lambda (frame)
		   (list frame
			 (frame-properties frame)
			 (current-window-configuration frame))))
		(frame-list))))

(defun set-frame-configuration (configuration &optional nodelete)
  "Restore the frames to the state described by CONFIGURATION.
Each frame listed in CONFIGURATION has its position, size, window
configuration, and other properties set as specified in CONFIGURATION.
Ordinarily, this function deletes all existing frames not
listed in CONFIGURATION.  But if optional second argument NODELETE
is given and non-nil, the unwanted frames are iconified instead."
  (or (frame-configuration-p configuration)
      (signal 'wrong-type-argument
	      (list 'frame-configuration-p configuration)))
  (let ((config-plist (cdr configuration))
	frames-to-delete)
    (mapc (lambda (frame)
	    (let ((properties (assq frame config-plist)))
	      (if properties
		  (progn
		    (set-frame-properties
		     frame
		     ;; Since we can't set a frame's minibuffer status,
		     ;; we might as well omit the parameter altogether.
		     (lax-plist-remprop (nth 1 properties) 'minibuffer))
		    (set-window-configuration (nth 2 properties)))
		(setq frames-to-delete (cons frame frames-to-delete)))))
	  (frame-list))
    (if nodelete
	;; Note: making frames invisible here was tried
	;; but led to some strange behavior--each time the frame
	;; was made visible again, the window manager asked afresh
	;; for where to put it.
	(mapc 'iconify-frame frames-to-delete)
      (mapc 'delete-frame frames-to-delete))))

; this function is in subr.el in FSFmacs.
; that's because they don't always include frame.el, while we do.

(defun frame-configuration-p (object)
  "Return non-nil if OBJECT seems to be a frame configuration.
Any list whose car is `frame-configuration' is assumed to be a frame
configuration."
  (and (consp object)
       (eq (car object) 'frame-configuration)))


;; FSFmacs has functions `frame-width', `frame-height' here.
;; We have them in C.

;; FSFmacs has weird functions `set-default-font', `set-background-color',
;; `set-foreground-color' here.  They don't do sensible things like
;; set faces; instead they set frame properties (??!!) and call
;; useless functions such as `frame-update-faces' and
;; `frame-update-face-colors'.

;; FSFmacs has functions `set-cursor-color', `set-mouse-color', and
;; `set-border-color', which refer to frame properties.
;; #### We need to use specifiers here.

;(defun auto-raise-mode (arg)
;  "Toggle whether or not the selected frame should auto-raise.
;With arg, turn auto-raise mode on if and only if arg is positive.
;Note that this controls Emacs's own auto-raise feature.
;Some window managers allow you to enable auto-raise for certain windows.
;You can use that for Emacs windows if you wish, but if you do,
;that is beyond the control of Emacs and this command has no effect on it."
;  (interactive "P")
;  (if (null arg)
;      (setq arg
;	    (if (frame-property (selected-frame) 'auto-raise)
;		-1 1)))
;  (set-frame-property (selected-frame) 'auto-raise (> arg 0)))

;(defun auto-lower-mode (arg)
;  "Toggle whether or not the selected frame should auto-lower.
;With arg, turn auto-lower mode on if and only if arg is positive.
;Note that this controls Emacs's own auto-lower feature.
;Some window managers allow you to enable auto-lower for certain windows.
;You can use that for Emacs windows if you wish, but if you do,
;that is beyond the control of Emacs and this command has no effect on it."
;  (interactive "P")
;  (if (null arg)
;      (setq arg
;	    (if (frame-property (selected-frame) 'auto-lower)
;		-1 1)))
;  (set-frame-property (selected-frame) 'auto-lower (> arg 0)))

;; FSFmacs has silly functions `toggle-scroll-bar',
;; `toggle-horizontal-scrollbar'

;;; Iconifying emacs.
;;;
;;; The function iconify-emacs replaces every non-iconified emacs window
;;; with a *single* icon.  Iconified emacs windows are left alone.  When
;;; emacs is in this globally-iconified state, de-iconifying any emacs icon
;;; will uniconify all frames that were visible, and iconify all frames
;;; that were not.  This is done by temporarily changing the value of
;;; `map-frame-hook' to `deiconify-emacs' (which should never be called
;;; except from the map-frame-hook while emacs is iconified).
;;;
;;; The title of the icon representing all emacs frames is controlled by
;;; the variable `icon-name'.  This is done by temporarily changing the
;;; value of `frame-icon-title-format'.  Unfortunately, this changes the
;;; titles of all emacs icons, not just the "big" icon.
;;;
;;; It would be nice if existing icons were removed and restored by
;;; iconifying the emacs process, but I couldn't make that work yet.

(defvar icon-name nil) ; set this at run time, not load time.

(defvar iconification-data nil)

(defun iconify-emacs ()
  "Replace every non-iconified FRAME with a *single* icon.
Iconified frames are left alone.  When XEmacs is in this
globally-iconified state, de-iconifying any emacs icon will uniconify
all frames that were visible, and iconify all frames that were not."
  (interactive)
  (if iconification-data (error "already iconified?"))
  (let* ((frames (frame-list))
	 (rest frames)
	 (me (selected-frame))
	 frame)
    (while rest
      (setq frame (car rest))
      (setcar rest (cons frame (frame-visible-p frame)))
;      (if (memq (cdr (car rest)) '(icon nil))
;	  (progn
;	    (make-frame-visible frame) ; deiconify, and process the X event
;	    (sleep-for 500 t) ; process X events; I really want to XSync() here
;	    ))
      (or (eq frame me) (make-frame-invisible frame))
      (setq rest (cdr rest)))
    (or (boundp 'map-frame-hook) (setq map-frame-hook nil))
    (or icon-name
	(setq icon-name (concat invocation-name " @ " (system-name))))
    (setq iconification-data
	    (list frame-icon-title-format map-frame-hook frames)
	  frame-icon-title-format icon-name
	  map-frame-hook 'deiconify-emacs)
    (iconify-frame me)))


(defun deiconify-emacs (&optional ignore)
  (or iconification-data (error "not iconified?"))
  (setq frame-icon-title-format (car iconification-data)
	map-frame-hook (car (cdr iconification-data))
	iconification-data (car (cdr (cdr iconification-data))))
  (while iconification-data
    (let ((visibility (cdr (car iconification-data))))
      (cond (visibility  ;; JV  (Note non-nil means visible in XEmacs)
	     (make-frame-visible (car (car iconification-data))))
;	    (t ;; (eq visibility 'icon) ;; JV Not in XEmacs!!!
;	     (make-frame-visible (car (car iconification-data)))
;	     (sleep-for 500 t) ; process X events; I really want to XSync() here
;	     (iconify-frame (car (car iconification-data))))
	    ;; (t nil)
	    ))
    (setq iconification-data (cdr iconification-data))))

(defun suspend-or-iconify-emacs ()
  "Call iconify-emacs if using a window system, otherwise suspend Emacs."
  (interactive)
  (cond ((device-on-window-system-p)
	 (iconify-emacs))
	((and (eq (device-type) 'tty)
	      (console-tty-controlling-process (selected-console)))
	 (suspend-console (selected-console)))
	(t
	 (suspend-emacs))))

;; This is quite a mouthful, but it should be descriptive, as it's
;; bound to C-z.  FSF takes the easy way out by binding C-z to
;; different things depending on window-system.  We can't do the same,
;; because we allow simultaneous X and TTY consoles.
(defun suspend-emacs-or-iconify-frame ()
  "Iconify the selected frame if using a window system, otherwise suspend Emacs."
  (interactive)
  (cond ((device-on-window-system-p)
	 (iconify-frame))
	((and (eq (frame-type) 'tty)
	      (console-tty-controlling-process (selected-console)))
	 (suspend-console (selected-console)))
	(t
	 (suspend-emacs))))


;;; auto-raise and auto-lower

(defcustom auto-raise-frame nil
  "*If true, frames will be raised to the top when selected.
Under X, most ICCCM-compliant window managers will have an option to do this
for you, but this variable is provided in case you're using a broken WM."
  :type 'boolean
  :group 'frames)

(defcustom auto-lower-frame nil
  "*If true, frames will be lowered to the bottom when no longer selected.
Under X, most ICCCM-compliant window managers will have an option to do this
for you, but this variable is provided in case you're using a broken WM."
  :type 'boolean
  :group 'frames)

(defun default-select-frame-hook ()
  "Implement the `auto-raise-frame' variable.
For use as the value of `select-frame-hook'."
  (if auto-raise-frame (raise-frame (selected-frame))))

(defun default-deselect-frame-hook ()
  "Implement the `auto-lower-frame' variable.
For use as the value of `deselect-frame-hook'."
  (if auto-lower-frame (lower-frame (selected-frame)))
  (highlight-extent nil nil))

(or select-frame-hook
    (add-hook 'select-frame-hook 'default-select-frame-hook))

(or deselect-frame-hook
    (add-hook 'deselect-frame-hook 'default-deselect-frame-hook))


;;; Application-specific frame-management

(defcustom get-frame-for-buffer-default-frame-name nil
  "*The default frame to select; see doc of `get-frame-for-buffer'."
  :type 'string
  :group 'frames)

(defcustom get-frame-for-buffer-default-instance-limit nil
  "*The default instance limit for creating new frames;
see doc of `get-frame-for-buffer'."
  :type 'integer
  :group 'frames)

(defun get-frame-name-for-buffer (buffer)
  (let ((mode (and (get-buffer buffer)
		   (save-excursion (set-buffer buffer)
				   major-mode))))
    (or (get mode 'frame-name)
	get-frame-for-buffer-default-frame-name)))

(defun get-frame-for-buffer-make-new-frame (buffer &optional frame-name plist)
  (let* ((fr (make-frame plist))
	 (w (frame-root-window fr)))
    ;;
    ;; Make the one buffer being displayed in this newly created
    ;; frame be the buffer of interest, instead of something
    ;; random, so that it won't be shown in two-window mode.
    ;; Avoid calling switch-to-buffer here, since that's something
    ;; people might want to call this routine from.
    ;;
    ;; (If the root window doesn't have a buffer, then that means
    ;; there is more than one window on the frame, which can only
    ;; happen if the user has done something funny on the frame-
    ;; creation-hook.  If that's the case, leave it alone.)
    ;;
    (if (window-buffer w)
	(set-window-buffer w buffer))
    fr))

(defcustom get-frame-for-buffer-default-to-current nil
  "*When non-nil, `get-frame-for-buffer' will default to the selected frame."
  :type 'boolean
  :group 'frames)

(defun get-frame-for-buffer-noselect (buffer
				      &optional not-this-window-p on-frame)
  "Return a frame in which to display BUFFER.
This is a subroutine of `get-frame-for-buffer' (which see)."
  (let (name limit)
    (cond
     ((or on-frame (eq (selected-window) (minibuffer-window)))
      ;; don't switch frames if a frame was specified, or to list
      ;; completions from the minibuffer, etc.
      nil)

     ((setq name (get-frame-name-for-buffer buffer))
      ;;
      ;; This buffer's mode expressed a preference for a frame of a particular
      ;; name.  That always takes priority.
      ;;
      (let ((limit (get name 'instance-limit))
	    (defaults (get name 'frame-defaults))
	    (matching-frames '())
	    frames frame already-visible)
	;; Sort the list so that iconic frames will be found last.  They
	;; will be used too, but mapped frames take precedence.  And
	;; fully visible frames come before occluded frames.
	;; Hidden frames come after really visible ones
	(setq frames
	      (sort (frame-list)
		    #'(lambda (s1 s2)
			(cond ((frame-totally-visible-p s2)
			       nil)
			      ((not (frame-visible-p s2))
			       (frame-visible-p s1))
			      ((eq (frame-visible-p s2) 'hidden)
			       (eq (frame-visible-p s1) t ))
			      ((not (frame-totally-visible-p s2))
			       (and (frame-visible-p s1)
				    (frame-totally-visible-p s1)))))))
	;; but the selected frame should come first, even if it's occluded,
	;; to minimize thrashing.
	(setq frames (cons (selected-frame)
			   (delq (selected-frame) frames)))

	(setq name (symbol-name name))
	(while frames
	  (setq frame (car frames))
	  (if (equal name (frame-name frame))
	      (if (get-buffer-window buffer frame)
		  (setq already-visible frame
			frames nil)
		(setq matching-frames (cons frame matching-frames))))
	  (setq frames (cdr frames)))
	(cond (already-visible
	       already-visible)
	      ((or (null matching-frames)
		   (eq limit 0) ; means create with reckless abandon
		   (and limit (< (length matching-frames) limit)))
	       (get-frame-for-buffer-make-new-frame
		buffer
		name
		(alist-to-plist (acons 'name name
				       (plist-to-alist defaults)))))
	      (t
	       ;; do not switch any of the window/buffer associations in an
	       ;; existing frame; this function only picks a frame; the
	       ;; determination of which windows on it get reused is up to
	       ;; display-buffer itself.
;;	       (or (window-dedicated-p (selected-window))
;;		   (switch-to-buffer buffer))
	       (car matching-frames)))))

     ((setq limit get-frame-for-buffer-default-instance-limit)
      ;;
      ;; This buffer's mode did not express a preference for a frame of a
      ;; particular name, but the user wants a new frame rather than
      ;; reusing the existing one.
      (let* ((defname
	       (or (plist-get default-frame-plist 'name)
		   default-frame-name))
	     (frames
	      (sort (filtered-frame-list #'(lambda (x)
					     (or (frame-visible-p x)
						 (frame-iconified-p x))))
		    #'(lambda (s1 s2)
			(cond ((and (frame-visible-p s1)
				    (not (frame-visible-p s2))))
			      ((and (eq (frame-visible-p s1) t)
				    (eq (frame-visible-p s2) 'hidden)))
			      ((and (frame-visible-p s2)
				    (not (frame-visible-p s1)))
			       nil)
			      ((and (equal (frame-name s1) defname)
				    (not (equal (frame-name s2) defname))))
			      ((and (equal (frame-name s2) defname)
				    (not (equal (frame-name s1) defname)))
			       nil)
			      ((frame-totally-visible-p s2)
			       nil)
			      (t))))))
	;; put the selected frame last.  The user wants a new frame,
	;; so don't reuse the existing one unless forced to.
	(setq frames (append (delq (selected-frame) frames) (list frames)))
	(if (or (eq limit 0) ; means create with reckless abandon
		(< (length frames) limit))
	    (get-frame-for-buffer-make-new-frame buffer)
	  (car frames))))

     (not-this-window-p
      (let ((w-list (windows-of-buffer buffer))
	    f w
	    (first-choice nil)
	    (second-choice (if get-frame-for-buffer-default-to-current
			       (selected-frame)
			     nil))
	    (last-resort nil))
	(while (and w-list (null first-choice))
	  (setq w (car w-list)
		f (window-frame w))
	  (cond ((eq w (selected-window)) nil)
		((not (frame-visible-p f))
		 (if (null last-resort)
		     (setq last-resort f)))
		((eq f (selected-frame))
		 (setq first-choice f))
		((null second-choice)
		 (setq second-choice f)))
	  (setq w-list (cdr w-list)))
	(or first-choice second-choice last-resort)))

     (get-frame-for-buffer-default-to-current (selected-frame))

     (t
      ;;
      ;; This buffer's mode did not express a preference for a frame of a
      ;; particular name.  So try to find a frame already displaying this
      ;; buffer.
      ;;
      (let ((w (or (get-buffer-window buffer nil)	; check current first
		   (get-buffer-window buffer 'visible)	; then visible
		   (get-buffer-window buffer 0))))	; then iconic
	(cond ((null w)
	       ;; It's not in any window - return nil, meaning no frame has
	       ;; preference.
	       nil)
	      (t
	       ;; Otherwise, return the frame of the buffer's window.
	       (window-frame w))))))))


;; The pre-display-buffer-function is called for effect, so this needs to
;; actually select the frame it wants.  Fdisplay_buffer() takes notice of
;; changes to the selected frame.
(defun get-frame-for-buffer (buffer &optional not-this-window-p on-frame
				    shrink-to-fit)
  "Select and return a frame in which to display BUFFER.
Normally, the buffer will simply be displayed in the selected frame.
But if the symbol naming the major-mode of the buffer has a 'frame-name
property (which should be a symbol), then the buffer will be displayed in
a frame of that name.  If there is no frame of that name, then one is
created.

If the major-mode doesn't have a 'frame-name property, then the frame
named by `get-frame-for-buffer-default-frame-name' will be used.  If
that is nil (the default) then the currently selected frame will used.

If the frame-name symbol has an 'instance-limit property (an integer)
then each time a buffer of the mode in question is displayed, a new frame
with that name will be created, until there are `instance-limit' of them.
If instance-limit is 0, then a new frame will be created each time.

If a buffer is already displayed in a frame, then `instance-limit' is
ignored, and that frame is used.

If the frame-name symbol has a 'frame-defaults property, then that is
prepended to the `default-frame-plist' when creating a frame for the
first time.

This function may be used as the value of `pre-display-buffer-function',
to cause the `display-buffer' function and its callers to exhibit the
above behavior."
  (let ((frame (get-frame-for-buffer-noselect
		buffer not-this-window-p on-frame)))
    (if (not (eq frame (selected-frame)))
	frame
      (select-frame frame)
      (or (frame-visible-p frame)
	  ;; If the frame was already visible, just focus on it.
	  ;; If it wasn't visible (it was just created, or it used
	  ;; to be iconified) then uniconify, raise, etc.
	  (make-frame-visible frame))
      frame)))

(defun frames-of-buffer (&optional buffer visible-only)
  "Return list of frames that BUFFER is currently being displayed on.
If the buffer is being displayed on the currently selected frame, that frame
is first in the list.  VISIBLE-ONLY will only list non-iconified frames."
  (let ((list (windows-of-buffer buffer))
	(cur-frame (selected-frame))
	next-frame frames save-frame)

    (while list
      (if (memq (setq next-frame (window-frame (car list)))
		frames)
	  nil
	(if (eq cur-frame next-frame)
	    (setq save-frame next-frame)
	  (and
	   (or (not visible-only)
	       (frame-visible-p next-frame))
	   (setq frames (append frames (list next-frame))))))
	(setq list (cdr list)))

    (if save-frame
	(append (list save-frame) frames)
      frames)))

(defcustom temp-buffer-shrink-to-fit nil
  "*When non-nil resize temporary output buffers to minimize blank lines."
  :type 'boolean
  :group 'frames)

(defcustom temp-buffer-max-height .5
  "*Proportion of frame to use for temp windows."
  :type 'number
  :group 'frames)

(defun show-temp-buffer-in-current-frame (buffer)
  "For use as the value of `temp-buffer-show-function':
always displays the buffer in the selected frame, regardless of the behavior
that would otherwise be introduced by the `pre-display-buffer-function', which
is normally set to `get-frame-for-buffer' (which see)."
  (let ((pre-display-buffer-function nil)) ; turn it off, whatever it is
    (let ((window (display-buffer buffer nil nil temp-buffer-shrink-to-fit)))
      (if (not (eq (last-nonminibuf-frame) (window-frame window)))
	  ;; only the pre-display-buffer-function should ever do this.
	  (error "display-buffer switched frames on its own!!"))
      (setq minibuffer-scroll-window window)
      (set-window-start window 1) ; obeys narrowing
      (set-window-point window 1)
      nil)))

(setq pre-display-buffer-function 'get-frame-for-buffer)
(setq temp-buffer-show-function 'show-temp-buffer-in-current-frame)


;; from Bob Weiner <bweiner@pts.mot.com>, modified by Ben Wing
(defun delete-other-frames (&optional frame)
  "Delete all but FRAME (or the selected frame)."
  (interactive)
  (mapc 'delete-frame (delq (or frame (selected-frame)) (frame-list))))

;; By adding primitives to directly access the window hierarchy,
;; we can move many functions into Lisp.  We do it this way
;; because the implementations are simpler in Lisp, and because
;; new functions like this can be added without requiring C
;; additions.

(defun frame-utmost-window-2 (window position left-right-p major-end-p
				     minor-end-p)
  ;; LEFT-RIGHT-P means we're looking for the leftmost or rightmost
  ;; window, instead of the highest or lowest.  In this case, we
  ;; say that the "major axis" goes left-to-right instead of top-to-
  ;; bottom.  The "minor axis" always goes perpendicularly.
  ;;
  ;; If MAJOR-END-P is t, we're looking for a windows that abut the
  ;; end (i.e. right or bottom) of the major axis, instead of the
  ;; start.
  ;;
  ;; If MINOR-END-P is t, then we want to start counting from the
  ;; end of the minor axis instead of the beginning.
  ;;
  ;; Here's the general idea: Imagine we're trying to count the number
  ;; of windows that abut the top; call this function foo().  So, we
  ;; start with the root window.  If this is a vertical combination
  ;; window, then foo() applied to the root window is the same as
  ;; foo() applied to the first child.  If the root is a horizontal
  ;; combination window, then foo() applied to the root is the
  ;; same as the sum of foo() applied to each of the children.
  ;; Otherwise, the root window is a leaf window, and foo() is 1.
  ;; Now it's clear that, each time foo() encounters a leaf window,
  ;; it's encountering a different window that abuts the top.
  ;; With a little examining, you can see that foo encounters the
  ;; top-abutting windows in order from left to right.  We can
  ;; modify foo() to return the nth top-abutting window by simply
  ;; keeping a global variable that is decremented each time
  ;; foo() encounters a leaf window and would return 1.  If the
  ;; global counter gets to zero, we've encountered the window
  ;; we were looking for, so we exit right away using a `throw'.
  ;; Otherwise, we make sure that all normal paths return nil.

  (let (child)
    (cond ((setq child (if left-right-p
			   (window-first-hchild window)
			 (window-first-vchild window)))
	   (if major-end-p
	       (while (window-next-child child)
		 (setq child (window-next-child child))))
	   (frame-utmost-window-2 child position left-right-p major-end-p
				  minor-end-p))
	  ((setq child (if left-right-p
			   (window-first-vchild window)
			 (window-first-hchild window)))
	   (if minor-end-p
	       (while (window-next-child child)
		 (setq child (window-next-child child))))
	   (while child
	     (frame-utmost-window-2 child position left-right-p major-end-p
				    minor-end-p)
	     (setq child (if minor-end-p
			     (window-previous-child child)
			   (window-next-child child))))
	   nil)
	  (t
	   (setcar position (1- (car position)))
	   (if (= (car position) 0)
	       (throw 'fhw-exit window)
	     nil)))))

(defun frame-utmost-window-1 (frame position left-right-p major-end-p)
  (let (minor-end-p)
    (or frame (setq frame (selected-frame)))
    (or position (setq position 0))
    (if (>= position 0)
	(setq position (1+ position))
      (setq minor-end-p t)
      (setq position (- position)))
    (catch 'fhw-exit
      ;; we use a cons here as a simple form of call-by-reference.
      ;; scheme has "boxes" for the same purpose.
      (frame-utmost-window-2 (frame-root-window frame) (list position)
			     left-right-p major-end-p minor-end-p))))


(defun frame-highest-window (&optional frame position)
  "Return the highest window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the top of the frame: 0 means the leftmost window abutting the
 top of the frame, 1 the next-leftmost, etc.  POSITION can also
 be less than zero: -1 means the rightmost window abutting the
 top of the frame, -2 the next-rightmost, etc.
If omitted, POSITION defaults to 0, i.e. the leftmost highest window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position nil nil))

(defun frame-lowest-window (&optional frame position)
  "Return the lowest window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the bottom of the frame: 0 means the leftmost window abutting the
 bottom of the frame, 1 the next-leftmost, etc.  POSITION can also
 be less than zero: -1 means the rightmost window abutting the
 bottom of the frame, -2 the next-rightmost, etc.
If omitted, POSITION defaults to 0, i.e. the leftmost lowest window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position nil t))

(defun frame-leftmost-window (&optional frame position)
  "Return the leftmost window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the left edge of the frame: 0 means the highest window abutting the
 left edge of the frame, 1 the next-highest, etc.  POSITION can also
 be less than zero: -1 means the lowest window abutting the
 left edge of the frame, -2 the next-lowest, etc.
If omitted, POSITION defaults to 0, i.e. the highest leftmost window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position t nil))

(defun frame-rightmost-window (&optional frame position)
  "Return the rightmost window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the right edge of the frame: 0 means the highest window abutting the
 right edge of the frame, 1 the next-highest, etc.  POSITION can also
 be less than zero: -1 means the lowest window abutting the
 right edge of the frame, -2 the next-lowest, etc.
If omitted, POSITION defaults to 0, i.e. the highest rightmost window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position t t))



;; frame properties.

(defun set-frame-property (frame prop val)
  "Set property PROP of FRAME to VAL.  See `set-frame-properties'."
  (set-frame-properties frame (list prop val)))

(defun frame-height (&optional frame)
  "Return number of lines available for display on FRAME."
  (frame-property frame 'height))

(defun frame-width (&optional frame)
  "Return number of columns available for display on FRAME."
  (frame-property frame 'width))

(put 'cursor-color 'frame-property-alias [text-cursor background])
(put 'modeline 'frame-property-alias 'has-modeline-p)


(provide 'frame)

;;; frame.el ends here
