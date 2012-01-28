;;; x-init.el --- initialization code for X windows

;; Copyright (C) 1990, 1993, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: terminals, dumped

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

;; This file is dumped with SXEmacs (when X support is compiled in).

;;; Code:

;; If you want to change this variable, this is the place you must do it.
;; Do not set it to a string containing periods.  X doesn't like that.
;(setq x-emacs-application-class "Emacs")

(defgroup x nil
  "The X Window system."
  :group 'environment)

;; selections and active regions

;; If and only if zmacs-regions is true:

;; When a mark is pushed and the region goes into the "active" state, we
;; assert it as the Primary selection.  This causes it to be hilighted.
;; When the region goes into the "inactive" state, we disown the Primary
;; selection, causing the region to be dehilighted.

;; Note that it is possible for the region to be in the "active" state
;; and not be hilighted, if it is in the active state and then some other
;; application asserts the selection.  This is probably not a big deal.

(defun x-activate-region-as-selection ()
  (if (marker-buffer (mark-marker t))
      (own-selection (cons (point-marker t) (mark-marker t)))))

;; OpenWindows-like "find" processing.  These functions are really Sunisms,
;; but we put them here instead of in x-win-sun.el in case someone wants
;; to use them when not running on a Sun console (presumably after binding
;; them to different keys, or putting them on menus.)

(defvar ow-find-last-string nil)
(defvar ow-find-last-clipboard nil)

(defun ow-find (&optional backward-p)
  "Search forward the next occurrence of the text of the selection."
  (interactive)
  (let ((sel  (ignore-errors (get-selection)))
	(clip (ignore-errors (get-clipboard)))
	text)
    (setq text (cond
		(sel)
		((not (equal clip ow-find-last-clipboard))
		 (setq ow-find-last-clipboard clip))
		(ow-find-last-string)
		(t (error "No selection available"))))
    (setq ow-find-last-string text)
    (cond (backward-p
	   (search-backward text)
	   (set-mark (+ (point) (length text))))
	  (t
	   (search-forward text)
	   (set-mark (- (point) (length text)))))
    (zmacs-activate-region)))

(defun ow-find-backward ()
  "Search backward for the previous occurrence of the text of the selection."
  (interactive)
  (ow-find t))

;; Load X-server specific code.
;; Specifically, load some code to repair the grievous damage that MIT and
;; Sun have done to the default keymap for the Sun keyboards.

(eval-when-compile
  (defmacro x-define-dead-key (key map)
    `(when (x-keysym-on-keyboard-p ',key)
       (define-key function-key-map [,key] ',map))))

(defun x-initialize-compose ()
  "Enable compose key and dead key processing."
  (autoload 'compose-map	    "x-compose" nil t 'keymap)
  (autoload 'compose-acute-map	    "x-compose" nil t 'keymap)
  (autoload 'compose-grave-map	    "x-compose" nil t 'keymap)
  (autoload 'compose-cedilla-map    "x-compose" nil t 'keymap)
  (autoload 'compose-diaeresis-map  "x-compose" nil t 'keymap)
  (autoload 'compose-circumflex-map "x-compose" nil t 'keymap)
  (autoload 'compose-tilde-map	    "x-compose" nil t 'keymap)

  (when (x-keysym-on-keyboard-p 'multi-key)
    (define-key function-key-map [multi-key] 'compose-map))

  ;; The dead keys might really be called just about anything, depending
  ;; on the vendor.  MIT thinks that the prefixes are "SunFA_", "D", and
  ;; "hpmute_" for Sun, DEC, and HP respectively.  However, OpenWindows 3
  ;; thinks that the prefixes are "SunXK_FA_", "DXK_", and "hpXK_mute_".
  ;; And HP (who don't mention Sun and DEC at all) use "XK_mute_".
  ;; Go figure.

  ;; Presumably if someone is running OpenWindows, they won't be using
  ;; the DEC or HP keysyms, but if they are defined then that is possible,
  ;; so in that case we accept them all.

  ;; If things seem not to be working, you might want to check your
  ;; /usr/lib/X11/XKeysymDB file to see if your vendor has an equally
  ;; mixed up view of what these keys should be called.

  ;; Canonical names:
  (x-define-dead-key acute			compose-acute-map)
  (x-define-dead-key grave			compose-grave-map)
  (x-define-dead-key cedilla			compose-cedilla-map)
  (x-define-dead-key diaeresis			compose-diaeresis-map)
  (x-define-dead-key circumflex			compose-circumflex-map)
  (x-define-dead-key tilde			compose-tilde-map)
  (x-define-dead-key degree			compose-ring-map)

  ;; Sun according to MIT:
  (x-define-dead-key SunFA_Acute		compose-acute-map)
  (x-define-dead-key SunFA_Grave		compose-grave-map)
  (x-define-dead-key SunFA_Cedilla		compose-cedilla-map)
  (x-define-dead-key SunFA_Diaeresis		compose-diaeresis-map)
  (x-define-dead-key SunFA_Circum		compose-circumflex-map)
  (x-define-dead-key SunFA_Tilde		compose-tilde-map)

  ;; Sun according to OpenWindows 2:
  (x-define-dead-key Dead_Grave			compose-grave-map)
  (x-define-dead-key Dead_Circum		compose-circumflex-map)
  (x-define-dead-key Dead_Tilde			compose-tilde-map)

  ;; Sun according to OpenWindows 3:
  (x-define-dead-key SunXK_FA_Acute		compose-acute-map)
  (x-define-dead-key SunXK_FA_Grave		compose-grave-map)
  (x-define-dead-key SunXK_FA_Cedilla		compose-cedilla-map)
  (x-define-dead-key SunXK_FA_Diaeresis		compose-diaeresis-map)
  (x-define-dead-key SunXK_FA_Circum		compose-circumflex-map)
  (x-define-dead-key SunXK_FA_Tilde		compose-tilde-map)

  ;; DEC according to MIT:
  (x-define-dead-key Dacute_accent		compose-acute-map)
  (x-define-dead-key Dgrave_accent		compose-grave-map)
  (x-define-dead-key Dcedilla_accent		compose-cedilla-map)
  (x-define-dead-key Dcircumflex_accent		compose-circumflex-map)
  (x-define-dead-key Dtilde			compose-tilde-map)
  (x-define-dead-key Dring_accent		compose-ring-map)

  ;; DEC according to OpenWindows 3:
  (x-define-dead-key DXK_acute_accent		compose-acute-map)
  (x-define-dead-key DXK_grave_accent		compose-grave-map)
  (x-define-dead-key DXK_cedilla_accent		compose-cedilla-map)
  (x-define-dead-key DXK_circumflex_accent	compose-circumflex-map)
  (x-define-dead-key DXK_tilde			compose-tilde-map)
  (x-define-dead-key DXK_ring_accent		compose-ring-map)

  ;; HP according to MIT:
  (x-define-dead-key hpmute_acute		compose-acute-map)
  (x-define-dead-key hpmute_grave		compose-grave-map)
  (x-define-dead-key hpmute_diaeresis		compose-diaeresis-map)
  (x-define-dead-key hpmute_asciicircum		compose-circumflex-map)
  (x-define-dead-key hpmute_asciitilde		compose-tilde-map)

  ;; Empirically discovered on Linux XFree86 MetroX:
  (x-define-dead-key usldead_acute		compose-acute-map)
  (x-define-dead-key usldead_grave		compose-grave-map)
  (x-define-dead-key usldead_diaeresis		compose-diaeresis-map)
  (x-define-dead-key usldead_asciicircum	compose-circumflex-map)
  (x-define-dead-key usldead_asciitilde		compose-tilde-map)

  ;; HP according to OpenWindows 3:
  (x-define-dead-key hpXK_mute_acute		compose-acute-map)
  (x-define-dead-key hpXK_mute_grave		compose-grave-map)
  (x-define-dead-key hpXK_mute_diaeresis	compose-diaeresis-map)
  (x-define-dead-key hpXK_mute_asciicircum	compose-circumflex-map)
  (x-define-dead-key hpXK_mute_asciitilde	compose-tilde-map)

  ;; HP according to HP-UX 8.0:
  (x-define-dead-key XK_mute_acute		compose-acute-map)
  (x-define-dead-key XK_mute_grave		compose-grave-map)
  (x-define-dead-key XK_mute_diaeresis		compose-diaeresis-map)
  (x-define-dead-key XK_mute_asciicircum	compose-circumflex-map)
  (x-define-dead-key XK_mute_asciitilde		compose-tilde-map)

  ;; Xfree86 seems to use lower case and a hyphen
  (x-define-dead-key dead-acute			compose-acute-map)
  (x-define-dead-key dead-grave			compose-grave-map)
  (x-define-dead-key dead-cedilla		compose-cedilla-map)
  (x-define-dead-key dead-diaeresis		compose-diaeresis-map)
  (x-define-dead-key dead-circum		compose-circumflex-map)
  (x-define-dead-key dead-circumflex		compose-circumflex-map)
  (x-define-dead-key dead-tilde			compose-tilde-map)
  )

(eval-when-compile
  (load "x-win-sun"     nil t)
  (load "x-win-xfree86" nil t))

(defun x-initialize-keyboard ()
  "Perform X-Server-specific initializations.  Don't call this."
  ;; This is some heuristic junk that tries to guess whether this is
  ;; a Sun keyboard.
  ;;
  ;; One way of implementing this (which would require C support) would
  ;; be to examine the X keymap itself and see if the layout looks even
  ;; remotely like a Sun - check for the Find key on a particular
  ;; keycode, for example.  It'd be nice to have a table of this to
  ;; recognize various keyboards; see also xkeycaps.
  ;;
  ;; Note that we cannot use most vendor-provided proprietary keyboard
  ;; APIs to identify the keyboard - those only work on the console.
  ;; xkeycaps has the same problem when running `remotely'.
  (let ((vendor (x-server-vendor)))
    (cond ((or (string-match "Sun Microsystems" vendor)
	       ;; MIT losingly fails to tell us what hardware the X server
	       ;; is managing, so assume all MIT displays are Suns...  HA HA!
	       (string-equal "MIT X Consortium" vendor)
	       (string-equal "X Consortium" vendor))
	   ;; Ok, we think this could be a Sun keyboard.  Run the Sun code.
	   (x-win-init-sun))
	  ((string-match "XFree86" vendor)
	   ;; Those XFree86 people do some weird keysym stuff, too.
	   (x-win-init-xfree86)))))


;; Moved from x-toolbar.el, since InfoDock doesn't dump a x-toolbar.el.
(defun x-init-toolbar-from-resources (locale)
  (loop for (specifier . resname) in
    `((   ,top-toolbar-height       .    "topToolBarHeight")
      (,bottom-toolbar-height       . "bottomToolBarHeight")
      (  ,left-toolbar-width        .   "leftToolBarWidth")
      ( ,right-toolbar-width        .  "rightToolBarWidth")

      (   ,top-toolbar-border-width .    "topToolBarBorderWidth")
      (,bottom-toolbar-border-width . "bottomToolBarBorderWidth")
      (  ,left-toolbar-border-width .   "leftToolBarBorderWidth")
      ( ,right-toolbar-border-width .  "rightToolBarBorderWidth"))
    do
    (x-init-specifier-from-resources
     specifier 'natnum locale (cons resname (upcase-initials resname)))))

(defvar pre-x-win-initted nil)

(defun init-pre-x-win ()
  "Initialize X Windows at startup (pre).  Don't call this."
  (when (not pre-x-win-initted)
    (require 'x-iso8859-1)
    (setq character-set-property 'x-iso8859/1) ; see x-iso8859-1.el

    (setq initial-frame-plist (if initial-frame-unmapped-p
				  '(initially-unmapped t)
				nil))
    (setq pre-x-win-initted t)))

(defvar x-win-initted nil)

(defun init-x-win ()
  "Initialize X Windows at startup.  Don't call this."
  (when (not x-win-initted)
    (defvar x-app-defaults-directory)
    (init-pre-x-win)

    ;; Open the X display when this file is loaded
    ;; (Note that the first frame is created later.)
    (setq x-initial-argv-list (cons (car command-line-args)
				    command-line-args-left))
    ;; Locate the app-defaults directory
    (when (and (boundp 'x-app-defaults-directory)
	       (null x-app-defaults-directory))
      (setq x-app-defaults-directory
	    (locate-data-directory "app-defaults")))
    (make-x-device nil)
    (setq command-line-args-left (cdr x-initial-argv-list))
    (setq x-win-initted t)))

(defvar post-x-win-initted nil)

(defun init-post-x-win ()
  "Initialize X Windows at startup (post).  Don't call this."
  (when (not post-x-win-initted)
    ;; We can't load this until after the initial X device is created
    ;; because the icon initialization needs to access the display to get
    ;; any toolbar-related color resources.
    (if (and (not (featurep 'infodock)) (featurep 'toolbar))
	(init-x-toolbar))
    (if (and (featurep 'infodock) (featurep 'toolbar))
	(require 'id-x-toolbar))
    (if (featurep 'gutter) (init-gutter))
    (if (featurep 'mule) (init-mule-x-win))
    ;; these are only ever called if zmacs-regions is true.
    (add-hook 'zmacs-deactivate-region-hook
	      (lambda ()
		(when (console-on-window-system-p)
		  (disown-selection))))
    (add-hook 'zmacs-activate-region-hook
	      (lambda ()
		(when (console-on-window-system-p)
		  (x-activate-region-as-selection))))
    (add-hook 'zmacs-update-region-hook
	      (lambda ()
		(when (console-on-window-system-p)
		  (x-activate-region-as-selection))))
    ;; Motif-ish bindings
    ;; The following two were generally unliked.
    ;;(define-key global-map '(shift delete)   'kill-primary-selection)
    ;;(define-key global-map '(control delete) 'delete-primary-selection)
    (define-key global-map '(shift insert)   'yank-clipboard-selection)
    (define-key global-map '(control insert) 'copy-primary-selection)
    ;; These are Sun-isms.
    (define-key global-map 'copy	'copy-primary-selection)
    (define-key global-map 'paste	'yank-clipboard-selection)
    (define-key global-map 'cut		'kill-primary-selection)

    (define-key global-map 'menu	'popup-mode-menu)
    ;;(define-key global-map '(shift menu) 'x-goto-menubar) ;NYI

    (setq post-x-win-initted t)))

;;; Keyboard initialization needs to be done differently for each X
;;; console, so use create-console-hook.
(when (featurep 'x)
  (add-hook
   'create-console-hook
   (lambda (console)
     (letf (((selected-console) console))
       (when (eq 'x (console-type console))
	 (x-initialize-keyboard)
	 (x-initialize-compose))))))

(defun make-frame-on-display (display &optional props)
  "Create a frame on the X display named DISPLAY.
DISPLAY should be a standard display string such as \"unix:0\",
or nil for the display specified on the command line or in the
DISPLAY environment variable.

PROPS should be a plist of properties, as in the call to `make-frame'.

This function opens a connection to the display or reuses an existing
connection.

This function is a trivial wrapper around `make-frame-on-device'."
  (interactive "sMake frame on display: ")
  (if (equal display "") (setq display nil))
  (make-frame-on-device 'x display props))

;; Character 160 (octal 0240) displays incorrectly under X apparently
;; due to a universally crocked font width specification.  Display it
;; as a space since that's what seems to be expected.
;;
;; (make-vector 256 nil) instead of (make-display-table) because
;; make-display-table doesn't exist when this file is loaded.

(let ((tab (make-vector 256 nil)))
  (aset tab 160 " ")
  (set-specifier current-display-table tab 'global 'x))

;;; x-init.el ends here
