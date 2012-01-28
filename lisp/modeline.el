;;; modeline.el --- modeline hackery.

;; Copyright (C) 1988, 1992-1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     General mouse modeline stuff                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup modeline nil
  "Modeline customizations."
  :group 'environment)

(defcustom modeline-3d-p ;; added for the options menu
  (let ((thickness
	 (specifier-instance modeline-shadow-thickness)))
    (and (integerp thickness)
	 (> thickness 0)))
  "Whether the default toolbar is globally visible. This option can be
customized through the options menu."
  :group 'display
  :type 'boolean
  :set #'(lambda (var val)
	   (if val
	       (set-specifier modeline-shadow-thickness 2)
	     (set-specifier modeline-shadow-thickness 0))
	   (redraw-modeline t)
	   (setq modeline-3d-p val))
  )

(defcustom drag-divider-event-lag 150
  "*The pause (in msecs) between divider drag events before redisplaying.
If this value is too small, dragging will be choppy because redisplay cannot
keep up. If it is too large, dragging will be choppy because of the explicit
redisplay delay specified."
  :type 'integer
  ;; #### Fix group.
  :group 'modeline)

(define-obsolete-variable-alias
  'drag-modeline-event-lag
  'drag-divider-event-lag)

(defcustom modeline-click-swaps-buffers nil
  "*If non-nil, clicking on the modeline changes the current buffer.
Click on the left half of the modeline cycles forward through the
buffer list and clicking on the right half cycles backward."
  :type 'boolean
  :group 'modeline)

(defcustom modeline-scrolling-method nil
  "*If non-nil, dragging the modeline with the mouse may also scroll its
text horizontally (vertical motion controls window resizing and horizontal
motion controls modeline scrolling).

With a value of t, the modeline text is scrolled in the same direction as
the mouse motion. With a value of 'scrollbar, the modeline is considered as
a scrollbar for its own text, which then moves in the opposite direction."
  :type '(choice (const :tag "none" nil)
		 (const :tag "text" t)
		 (const :tag "scrollbar" scrollbar))
  :set (lambda (sym val)
	 (set-default sym val)
	 (when (featurep 'x)
	   (cond ((eq val t)
		  (set-glyph-image modeline-pointer-glyph "hand2" 'global 'x))
		 ((eq val 'scrollbar)
		  (set-glyph-image modeline-pointer-glyph "fleur" 'global 'x))
		 (t
		  (set-glyph-image modeline-pointer-glyph "sb_v_double_arrow"
				   'global 'x)))))
  :group 'modeline)

(defun mouse-drag-modeline (event)
  "Resize a window by dragging its modeline.
This command should be bound to a button-press event in modeline-map.
Holding down a mouse button and moving the mouse up and down will
make the clicked-on window taller or shorter.

See also the variable `modeline-scrolling-method'."
  (interactive "e")
  (or (button-press-event-p event)
      (error "%s must be invoked by a mouse-press" this-command))
  (or (event-over-modeline-p event)
      (error "not over a modeline"))
  ;; Give the modeline a "pressed" look.  --hniksic
  (let-specifier ((modeline-shadow-thickness
		   (- (specifier-instance modeline-shadow-thickness
					  (event-window event)))
		   (event-window event)))
    (let ((done nil)
	  (depress-line (event-y event))
	  (start-event-frame (event-frame event))
	  (start-event-window (event-window event))
	  (start-nwindows (count-windows t))
	  (hscroll-delta (face-width 'modeline))
	  (start-hscroll (modeline-hscroll (event-window event)))
	  (start-x-pixel (event-x-pixel event))
	  (last-timestamp 0)
	  default-line-height
	  modeline-height
	  should-enlarge-minibuffer
	  event min-height minibuffer y top bot edges wconfig growth)
      (setq minibuffer (minibuffer-window start-event-frame)
	    default-line-height (face-height 'default start-event-window)
	    min-height (+ (* window-min-height default-line-height)
			  ;; Don't let the window shrink by a
			  ;; non-multiple of the default line
			  ;; height.  (enlarge-window -1) will do
			  ;; this if the difference between the
			  ;; current window height and the minimum
			  ;; window height is less than the height
			  ;; of the default font.  These extra
			  ;; lost pixels of height don't come back
			  ;; if you grow the window again.  This
			  ;; can make it impossible to drag back
			  ;; to the exact original size, which is
			  ;; disconcerting.
			  (% (window-pixel-height start-event-window)
			     default-line-height))
	    modeline-height
	    (if (specifier-instance has-modeline-p start-event-window)
		(+ (face-height 'modeline start-event-window)
		   (* 2 (specifier-instance modeline-shadow-thickness
					    start-event-window)))
	      (* 2 (specifier-instance modeline-shadow-thickness
				       start-event-window))))
      (if (not (eq (window-frame minibuffer) start-event-frame))
	  (setq minibuffer nil))
      (if (and (null minibuffer) (one-window-p t))
	  (error "Attempt to resize sole window"))
      ;; if this is the bottommost ordinary window, then to
      ;; move its modeline the minibuffer must be enlarged.
      (setq should-enlarge-minibuffer
	    (and minibuffer (window-lowest-p start-event-window)))
      ;; loop reading events
      (while (not done)
	(setq event (next-event event))
	;; requeue event and quit if this is a misc-user, eval or
	;;   keypress event.
	;; quit if this is a button press or release event, or if the event
	;;   occurred in some other frame.
	;; drag if this is a mouse motion event and the time
	;;   between this event and the last event is greater than
	;;   drag-divider-event-lag.
	;; do nothing if this is any other kind of event.
	(cond ((or (misc-user-event-p event)
		   (key-press-event-p event))
	       (setq unread-command-events (nconc unread-command-events
						  (list event))
		     done t))
	      ((button-release-event-p event)
	       (setq done t)
	       ;; Consider we have a mouse click neither X pos (modeline
	       ;; scroll) nore Y pos (modeline drag) have changed.
	       (and modeline-click-swaps-buffers
		    (= depress-line (event-y event))
		    (or (not modeline-scrolling-method)
			(= start-hscroll
			   (modeline-hscroll start-event-window)))
		    (modeline-swap-buffers event)))
	      ((button-event-p event)
	       (setq done t))
	      ((not (motion-event-p event))
	       (dispatch-event event))
	      ((not (eq start-event-frame (event-frame event)))
	       (setq done t))
	      ((< (abs (- (event-timestamp event) last-timestamp))
		  drag-divider-event-lag)
	       nil)
	      (t
	       (when modeline-scrolling-method
		 (let ((delta (/ (- (event-x-pixel event) start-x-pixel)
				 hscroll-delta)))
		   (set-modeline-hscroll start-event-window
					 (if (eq modeline-scrolling-method t)
					     (- start-hscroll delta)
					   (+ start-hscroll delta)))
		   ))
	       (setq last-timestamp (event-timestamp event)
		     y (event-y-pixel event)
		     edges (window-pixel-edges start-event-window)
		     top (nth 1 edges)
		     bot (nth 3 edges))
	       ;; scale back a move that would make the
	       ;; window too short.
	       (cond ((< (- y top (- modeline-height)) min-height)
		      (setq y (+ top min-height (- modeline-height)))))
	       ;; compute size change needed
	       (setq growth (- y bot (/ (- modeline-height) 2))
		     wconfig (current-window-configuration))
	       ;; grow/shrink minibuffer?
	       (if should-enlarge-minibuffer
		   (progn
		     ;; yes.  scale back shrinkage if it
		     ;; would make the minibuffer less than 1
		     ;; line tall.
		     ;;
		     ;; also flip the sign of the computed growth,
		     ;; since if we want to grow the window with the
		     ;; modeline we need to shrink the minibuffer
		     ;; and vice versa.
		     (if (and (> growth 0)
			      (< (- (window-pixel-height minibuffer)
				    growth)
				 default-line-height))
			 (setq growth
			       (- (window-pixel-height minibuffer)
				  default-line-height)))
		     (setq growth (- growth))))
	       ;; window grow and shrink by lines not pixels, so
	       ;; divide the pixel height by the height of the
	       ;; default face.
	       (setq growth (/ growth default-line-height))
	       ;; grow/shrink the window
	       (enlarge-window growth nil (if should-enlarge-minibuffer
					      minibuffer
					    start-event-window))
	       ;; if this window's growth caused another
	       ;; window to be deleted because it was too
	       ;; short, rescind the change.
	       ;;
	       ;; if size change caused space to be stolen
	       ;; from a window above this one, rescind the
	       ;; change, but only if we didn't grow/shrink
	       ;; the minibuffer.  minibuffer size changes
	       ;; can cause all windows to shrink... no way
	       ;; around it.
	       (if (or (/= start-nwindows (count-windows t))
		       (and (not should-enlarge-minibuffer)
			    (/= top (nth 1 (window-pixel-edges
					    start-event-window)))))
		   (set-window-configuration wconfig))))))))

;; from Bob Weiner (bob_weiner@pts.mot.com)
;; Whether this function should be called is now decided in
;; mouse-drag-modeline - dverna feb. 98
(defun modeline-swap-buffers (event)
  "Handle mouse clicks on modeline by switching buffers.
If click on left half of a frame's modeline, bury current buffer.
If click on right half of a frame's modeline, raise bottommost buffer.
Arg EVENT is the button release event that occurred on the modeline."
  (or (event-over-modeline-p event)
      (error "not over a modeline"))
  (or (button-release-event-p event)
      (error "not a button release event"))
  (if (< (event-x event) (/ (window-width (event-window event)) 2))
      ;; On left half of modeline, bury current buffer,
      ;; displaying second buffer on list.
      (mouse-bury-buffer event)
    ;; On right half of modeline, raise and display bottommost
    ;; buffer in buffer list.
    (mouse-unbury-buffer event)))

(defconst modeline-menu
  '("Window Commands"
    ["Delete Window Above"	 delete-window			t]
    ["Delete Other Windows"	 delete-other-windows		t]
    ["Split Window Above"	 split-window-vertically	t]
    ["Split Window Horizontally" split-window-horizontally	t]
    ["Balance Windows"		 balance-windows		t]
    ))

(defun modeline-menu (event)
  (interactive "e")
  (popup-menu-and-execute-in-window
   (cons (format "Window Commands for %S:"
		 (buffer-name (event-buffer event)))
	 (cdr modeline-menu))
   event))

(defvar modeline-map (make-sparse-keymap 'modeline-map)
  "Keymap consulted for mouse-clicks on the modeline of a window.
This variable may be buffer-local; its value will be looked up in
the buffer of the window whose modeline was clicked upon.")

(define-key modeline-map 'button1 'mouse-drag-modeline)
;; button2 selects the window without setting point
(define-key modeline-map 'button2 (lambda () (interactive "@")))
(define-key modeline-map 'button3 'modeline-menu)

(make-face 'modeline-mousable "Face for mousable portions of the modeline.")
(set-face-parent 'modeline-mousable 'modeline nil '(default))
(when (featurep 'window-system)
  (set-face-foreground 'modeline-mousable "firebrick" nil '(default color win))
  (set-face-font 'modeline-mousable [bold] nil '(default mono win))
  (set-face-font 'modeline-mousable [bold] nil '(default grayscale win)))
(when (featurep 'tty)
  (set-face-foreground 'modeline-mousable '("firebrick" "red") nil '(default color tty)))

(defmacro make-modeline-command-wrapper (command)
  `#'(lambda (event)
       (interactive "e")
       (save-selected-window
	 (select-window (event-window event))
	 (call-interactively ',(eval command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Minor modes                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar minor-mode-alist nil
  "Alist saying how to show minor modes in the modeline.
Each element looks like (VARIABLE STRING);
STRING is included in the modeline iff VARIABLE's value is non-nil.

Actually, STRING need not be a string; any possible modeline element
is okay.  See `modeline-format'.")

;; Used by C code (lookup-key and friends) but defined here.
(defvar minor-mode-map-alist nil
  "Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings iff VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.")

(make-face 'modeline-mousable-minor-mode
	   "Face for mousable minor-mode strings in the modeline.")
(set-face-parent 'modeline-mousable-minor-mode 'modeline-mousable nil
		 '(default))
(when (featurep 'window-system)
  (set-face-foreground 'modeline-mousable-minor-mode '("green4" "forestgreen")
		       nil '(default color win))
  (set-face-foreground 'modeline-mousable-minor-mode '("green4" "forestgreen" "green")
		       nil '(default color tty)))

(defvar modeline-mousable-minor-mode-extent (make-extent nil nil)
  ;; alliteration at its finest.
  "Extent managing the mousable minor mode modeline strings.")
(set-extent-face modeline-mousable-minor-mode-extent
		 'modeline-mousable-minor-mode)

;; This replaces the idiom
;;
;; (or (assq 'isearch-mode minor-mode-alist)
;;     (setq minor-mode-alist
;;           (purecopy
;;            (append minor-mode-alist
;;                    '((isearch-mode isearch-mode))))))

(defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Add a minor mode to `minor-mode-alist' and `minor-mode-map-alist'.

TOGGLE is a symbol whose value as a variable specifies whether the
minor mode is active.

NAME is the name that should appear in the modeline.  It should either
be a string beginning with a space, or a symbol with a similar string
as its value.

KEYMAP is a keymap to make active when the minor mode is active.

AFTER is the toggling symbol used for another minor mode.  If AFTER is
non-nil, then it is used to position the new mode in the minor-mode
alists.

TOGGLE-FUN specifies an interactive function that is called to toggle
the mode on and off; this affects what happens when button2 is pressed
on the mode, and when button3 is pressed somewhere in the list of
modes.  If TOGGLE-FUN is nil and TOGGLE names an interactive function,
TOGGLE is used as the toggle function.

Example: (add-minor-mode 'view-minor-mode \" View\" view-mode-map)"
  (let* ((add-elt #'(lambda (elt sym)
		      (let (place)
			(cond ((null after) ; add to front
			       (push elt (symbol-value sym)))
			      ((and (not (eq after t))
				    (setq place (memq (assq after
							    (symbol-value sym))
						      (symbol-value sym))))
			       (push elt (cdr place)))
			      (t
			       (set sym (append (symbol-value sym)
						(list elt))))))
		      (symbol-value sym)))
	 el toggle-keymap)
    (if toggle-fun
	(check-argument-type 'commandp toggle-fun)
      (when (commandp toggle)
	(setq toggle-fun toggle)))
    (when (and toggle-fun name)
      (setq toggle-keymap (make-sparse-keymap
			   (intern (concat "modeline-minor-"
					   (symbol-name toggle)
					   "-map"))))
      (define-key toggle-keymap 'button2
	;; defeat the DUMB-ASS byte-compiler, which tries to
	;; expand the macro at compile time and fucks up.
	(eval '(make-modeline-command-wrapper toggle-fun)))
      (put toggle 'modeline-toggle-function toggle-fun))
    (when name
      (let ((hacked-name
	     (if toggle-keymap
		 (cons (let ((extent (make-extent nil nil)))
			 (set-extent-keymap extent toggle-keymap)
			 (set-extent-property
			  extent 'help-echo
			  (concat "button2 turns off "
				  (if (symbolp toggle-fun)
				      (symbol-name toggle-fun)
				    (symbol-name toggle))))
			 extent)
		       (cons modeline-mousable-minor-mode-extent name))
	       name)))
	(if (setq el (assq toggle minor-mode-alist))
	    (setcdr el (list hacked-name))
	  (funcall add-elt
		   (list toggle hacked-name)
		   'minor-mode-alist))))
    (when keymap
      (if (setq el (assq toggle minor-mode-map-alist))
	  (setcdr el keymap)
	(funcall add-elt
		 (cons toggle keymap)
		 'minor-mode-map-alist)))))

;; #### TODO: Add `:menu-tag' keyword to add-minor-mode.  Or create a
;; separate function to manage the minor mode menu.

;(put 'abbrev-mode :menu-tag "Abbreviation Expansion")
(add-minor-mode 'abbrev-mode " Abbrev")
;; only when visiting a file...
(add-minor-mode 'overwrite-mode 'overwrite-mode)
;(put 'auto-fill-function :menu-tag "Auto Fill")
(add-minor-mode 'auto-fill-function " Fill" nil nil 'auto-fill-mode)

;(put 'defining-kbd-macro :menu-tag "Keyboard Macro")
(add-minor-mode 'defining-kbd-macro " Def" nil nil
		(lambda ()
		  (interactive)
		  (if defining-kbd-macro
		      (progn
			;; #### This means to disregard the last event.
			;; It is needed because the last recorded
			;; event is usually the mouse event that
			;; invoked the menu item (and this function),
			;; and having it in the macro causes problems.
			(zap-last-kbd-macro-event)
			(end-kbd-macro nil))
		    (start-kbd-macro nil))))

(defun modeline-minor-mode-menu (event)
  "The menu that pops up when you press `button3' inside the
parentheses on the modeline."
  (interactive "e")
  (save-excursion
    (set-buffer (event-buffer event))
    (popup-menu-and-execute-in-window
     (cons
      "Minor Mode Toggles"
      (sort
       (delq nil (mapcar
		 #'(lambda (x)
		     (let* ((toggle-sym (car x))
			    (toggle-fun (or (get toggle-sym
						 'modeline-toggle-function)
					    (and (commandp toggle-sym)
						 toggle-sym)))
			    (menu-tag (symbol-name (if (symbolp toggle-fun)
						       toggle-fun
						     toggle-sym))
				      ;; Here a function should
				      ;; maybe be invoked to
				      ;; beautify the symbol's
				      ;; menu appearance.
				      ))
		       (and toggle-fun
			    (vector menu-tag
				    toggle-fun
				    ;; The following two are wrong
				    ;; because of possible name
				    ;; clashes.
				    ;:active (get toggle-sym :active t)
				    ;:included (get toggle-sym :included t)
				    :style 'toggle
				    :selected (and (boundp toggle-sym)
						   toggle-sym)))))
		 minor-mode-alist))
       (lambda (e1 e2)
	 (string< (aref e1 0) (aref e2 0)))))
     event)))

(defvar modeline-minor-mode-map (make-sparse-keymap 'modeline-minor-mode-map)
  "Keymap consulted for mouse-clicks on the minor-mode modeline list.")
(define-key modeline-minor-mode-map 'button3 'modeline-minor-mode-menu)

(defvar modeline-minor-mode-extent (make-extent nil nil)
  "Extent covering the minor mode modeline strings.")
(set-extent-face modeline-minor-mode-extent 'modeline-mousable)
(set-extent-keymap modeline-minor-mode-extent modeline-minor-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              Other                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modeline-buffers-menu (event)
  (interactive "e")
  (popup-menu-and-execute-in-window
   '("Buffers Popup Menu"
     :filter buffers-menu-filter
     ["List All Buffers" list-buffers t]
     "--"
     )
   event))

(defvar modeline-buffer-id-left-map
  (make-sparse-keymap 'modeline-buffer-id-left-map)
"Keymap consulted for mouse-clicks on the left half of the buffer-id string.")

(defvar modeline-buffer-id-right-map
  (make-sparse-keymap 'modeline-buffer-id-right-map)
"Keymap consulted for mouse-clicks on the right half of the buffer-id string.")

(define-key modeline-buffer-id-left-map 'button2 'mouse-unbury-buffer)
(define-key modeline-buffer-id-right-map 'button2 'mouse-bury-buffer)
(define-key modeline-buffer-id-left-map 'button3 'modeline-buffers-menu)
(define-key modeline-buffer-id-right-map 'button3 'modeline-buffers-menu)

(make-face 'modeline-buffer-id
	   "Face for the buffer ID string in the modeline.")
(set-face-parent 'modeline-buffer-id 'modeline nil '(default))
(when (featurep 'window-system)
  (set-face-foreground 'modeline-buffer-id "blue4" nil '(default color win))
  (set-face-font 'modeline-buffer-id [bold-italic] nil '(default mono win))
  (set-face-font 'modeline-buffer-id [bold-italic] nil '(default grayscale win)))
(when (featurep 'tty)
  (set-face-foreground 'modeline-buffer-id '("blue4" "blue") nil '(default color tty))
  (set-face-font 'modeline-buffer-id [bold-italic] nil '(default mono tty)))

(defvar modeline-buffer-id-extent (make-extent nil nil)
  "Extent covering the whole of the buffer-id string.")
(set-extent-face modeline-buffer-id-extent 'modeline-buffer-id)

(defvar modeline-buffer-id-left-extent (make-extent nil nil)
"Extent covering the left half of the buffer-id string.")
(set-extent-keymap modeline-buffer-id-left-extent
		   modeline-buffer-id-left-map)
(set-extent-property modeline-buffer-id-left-extent 'help-echo
		     "button2 cycles to the previous buffer")

(defvar modeline-buffer-id-right-extent (make-extent nil nil)
"Extent covering the right half of the buffer-id string.")
(set-extent-keymap modeline-buffer-id-right-extent
		   modeline-buffer-id-right-map)
(set-extent-property modeline-buffer-id-right-extent 'help-echo
		     "button2 cycles to the next buffer")

(defconst modeline-buffer-identification
  (list (cons modeline-buffer-id-left-extent "SXEmacs%N:")
					; this used to be "SXEmacs:"
	(cons modeline-buffer-id-right-extent " %17b"))
  "Modeline control for identifying the buffer being displayed.
Its default value is

  (list (cons modeline-buffer-id-left-extent \"SXEmacs%N:\")
	(cons modeline-buffer-id-right-extent \" %17b\")))

Major modes that edit things other than ordinary files may change this
(e.g. Info, Dired,...).")
(make-variable-buffer-local 'modeline-buffer-identification)

;; These are for the sake of minor mode menu.  #### All of this is
;; kind of dirty.  `add-minor-mode' started out as a simple substitute
;; for (or (assq ...) ...) FSF stuff, but now is used for all kind of
;; stuff.  There should perhaps be a separate function to add toggles
;; to the minor-mode-menu.
(add-minor-mode 'line-number-mode "")
(add-minor-mode 'column-number-mode "")

(defconst modeline-process nil
  "Modeline control for displaying info on process status.
Normally nil in most modes, since there is no process to display.")
(make-variable-buffer-local 'modeline-process)

(defvar modeline-modified-map (make-sparse-keymap 'modeline-modified-map)
  "Keymap consulted for mouse-clicks on the modeline-modified string.")
(define-key modeline-modified-map 'button2
  (make-modeline-command-wrapper 'modeline-toggle-read-only))

(defvar modeline-modified-extent (make-extent nil nil)
  "Extent covering the modeline-modified string.")
(set-extent-face modeline-modified-extent 'modeline-mousable)
(set-extent-keymap modeline-modified-extent modeline-modified-map)
(set-extent-property modeline-modified-extent 'help-echo
		     "button2 toggles the buffer's read-only status")

(defconst modeline-modified '("--%1*%1+-")
  "Modeline control for displaying whether current buffer is modified.")
(make-variable-buffer-local 'modeline-modified)

(defvar modeline-narrowed-map (make-sparse-keymap 'modeline-narrowed-map)
  "Keymap consulted for mouse-clicks on the modeline-narrowed string.")
(define-key modeline-narrowed-map 'button2
  (make-modeline-command-wrapper 'widen))

(defvar modeline-narrowed-extent (make-extent nil nil)
  "Extent covering the modeline-narrowed string.")
(set-extent-face modeline-narrowed-extent 'modeline-mousable)
(set-extent-keymap modeline-narrowed-extent modeline-narrowed-map)
(set-extent-property modeline-narrowed-extent 'help-echo
		     "button2 widens the buffer")

(setq-default
 modeline-format
 (list
  ""
  (cons modeline-modified-extent 'modeline-modified)
  (list 'line-number-mode (cons 3 "L%l"))
  (list 'column-number-mode "-")
  (list 'column-number-mode (cons 3 "C%c"))
  "--"
  (cons modeline-buffer-id-extent 'modeline-buffer-identification)
  " "
  'global-mode-string
  " %[("
  (cons modeline-minor-mode-extent
	(list "" 'mode-name 'minor-mode-alist))
  (cons modeline-narrowed-extent "%n")
  'modeline-process
  ")%]----"
  (cons -3 "%p")
  "-%-"))

;;; Added for XEmacs 20.3.  Provide wrapper for vc since it may not always be
;;; present, and its symbols are not visible this early in the dump if it
;;; is.

(defun modeline-toggle-read-only ()
  "Change whether this buffer is visiting its file read-only.
With arg, set read-only iff arg is positive.
This function is designed to be called when the read-only indicator on the
modeline is clicked.  It will call `vc-toggle-read-only' if available,
otherwise it will call the usual `toggle-read-only'."
  (interactive)
  (if-fboundp 'vc-toggle-read-only
      (vc-toggle-read-only)
    (toggle-read-only)))

;;; modeline.el ends here
