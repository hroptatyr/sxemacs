;;; gtk-init.el --- initialization code for mswindows
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Rewritten for Gtk by: William Perry

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar gtk-win-initted nil)
(defvar gtk-pre-win-initted nil)
(defvar gtk-post-win-initted nil)

(defvar gtk-command-switch-alist
  '(
    ;; GNOME Options
    ("--disable-sound" . nil)
    ("--enable-sound"  . nil)
    ("--espeaker"      . t)

    ;; GTK Options
    ("--gdk-debug"    . t)
    ("--gdk-no-debug" . t)
    ("--display"      . t)
    ("--sync"         . nil)
    ("--no-xshm"      . nil)
    ("--name"         . t)
    ("--class"        . t)
    ("--gxid_host"    . t)
    ("--gxid_port"    . t)
    ("--xim-preedit"  . t)
    ("--xim-status"   . t)
    ("--gtk-debug"    . t)
    ("--gtk-no-debug" . t)
    ("--gtk-module"   . t)

    ;; Glib options
    ("--g-fatal-warnings" . nil)

    ;; Session management options
    ("--sm-client-id"     . t)
    ("--sm-config-prefix" . t)
    ("--sm-disable"       . t)
    )

  "An assoc list of command line arguments that should in gtk-initial-argv-list.
This is necessary because GTK and GNOME consider it a fatal error if they receive
unknown command line arguments (perfectly reasonable).  But this means that if
the user specifies a file name on the command line they will be unable to start.
So we filter the command line and allow only items in this list in.

The CDR of the assoc list is whether it accepts an argument.  All options are in
GNU long form though.")

(defun init-pre-gtk-win ()
  "Initialize Gtk GUI at startup (pre).  Don't call this."
  (when (not gtk-pre-win-initted)
    (setq initial-frame-plist (if initial-frame-unmapped-p
				  '(initially-unmapped t)
				nil)
	  gtk-pre-win-initted t)))

(defun gtk-init-handle-geometry (arg)
  "Set up initial geometry info for GTK devices."
  (setq gtk-initial-geometry (pop command-line-args-left)))

(defun gtk-filter-arguments ()
  (let ((accepted nil)
	(rejected nil)
	(todo nil))
    (setq todo (mapcar (lambda (argdesc)
			 (if (cdr argdesc)
			     ;; Need to look for --foo=bar
			     (concat "^" (car argdesc) "=")
			   ;; Just a simple arg
			   (concat "^" (regexp-quote (car argdesc)) "$")))
		       gtk-command-switch-alist))

    (while command-line-args-left
      (if (catch 'found
	    (mapc (lambda (r)
		    (if (string-match r (car command-line-args-left))
			(throw 'found t))) todo)
	    (mapc (lambda (argdesc)
		    (if (cdr argdesc)
			;; This time we only care about argument items
			;; that take an argument.  We'll check to see if
			;; someone used --foo bar instead of --foo=bar
			(if (string-match (concat "^" (car argdesc) "$") (car command-line-args-left))
			    ;; Yup!  Need to push
			    (progn
			      (push (pop command-line-args-left) accepted)
			      (throw 'found t)))))
		  gtk-command-switch-alist)
	    nil)
	  (push (pop command-line-args-left) accepted)
	(push (pop command-line-args-left) rejected)))
    (setq command-line-args-left (nreverse rejected))
    (nreverse accepted)))

(defun init-gtk-win ()
  "Initialize Gtk GUI at startup.  Don't call this."
  (unless gtk-win-initted
    (init-pre-gtk-win)
    (setq gtk-initial-argv-list (cons (car command-line-args) (gtk-filter-arguments))
	  gtk-initial-geometry (nth 1 (member "-geometry" command-line-args-left)))
    (make-gtk-device)
    (init-post-gtk-win)
    (setq gtk-win-initted t)))

(defun init-post-gtk-win ()
  (unless gtk-post-win-initted
    (if (and (not (featurep 'infodock)) (featurep 'toolbar))
        (init-x-toolbar))
    (if (and (featurep 'infodock) (featurep 'toolbar))
	(require 'id-x-toolbar))

    (when (featurep 'mule)
      (define-specifier-tag 'mule-fonts
	(lambda (device) (eq 'gtk (device-type device))))
      (set-face-font
       'default
       '("-*-fixed-medium-r-*--16-*-iso8859-1"
	 "-*-fixed-medium-r-*--*-iso8859-1"
	 "-*-fixed-medium-r-*--*-iso8859-2"
	 "-*-fixed-medium-r-*--*-iso8859-3"
	 "-*-fixed-medium-r-*--*-iso8859-4"
	 "-*-fixed-medium-r-*--*-iso8859-7"
	 "-*-fixed-medium-r-*--*-iso8859-8"
	 "-*-fixed-medium-r-*--*-iso8859-5"
	 "-*-fixed-medium-r-*--*-iso8859-9"

	 ;; Following 3 fonts proposed by Teruhiko.Kurosaka@Japan.eng.sun
	 "-sun-gothic-medium-r-normal--14-120-75-75-c-60-jisx0201.1976-0"
	 "-sun-gothic-medium-r-normal--14-120-75-75-c-120-jisx0208.1983-0"
	 "-wadalab-gothic-medium-r-normal--14-120-75-75-c-120-jisx0212.1990-0"
	 ;; Other Japanese fonts
	 "-*-fixed-medium-r-*--*-jisx0201.1976-*"
	 "-*-fixed-medium-r-*--*-jisx0208.1983-*"
	 "-*-fixed-medium-r-*--*-jisx0212*-*"

	 ;; Chinese fonts
	 "-*-*-medium-r-*--*-gb2312.1980-*"
       
	 ;; Use One font specification for CNS chinese
	 ;; Too many variations in font naming
	 "-*-fixed-medium-r-*--*-cns11643*-*"
	 ;; "-*-fixed-medium-r-*--*-cns11643*2"
	 ;; "-*-fixed-medium-r-*--*-cns11643*3"
	 ;; "-*-fixed-medium-r-*--*-cns11643*4"
	 ;; "-*-fixed-medium-r-*--*-cns11643.5-0"
	 ;; "-*-fixed-medium-r-*--*-cns11643.6-0"
	 ;; "-*-fixed-medium-r-*--*-cns11643.7-0"
       
	 "-*-fixed-medium-r-*--*-big5*-*"
	 "-*-fixed-medium-r-*--*-sisheng_cwnn-0"

	 ;; Other fonts
       
	 ;; "-*-fixed-medium-r-*--*-viscii1.1-1"
       
	 ;; "-*-fixed-medium-r-*--*-mulearabic-0"
	 ;; "-*-fixed-medium-r-*--*-mulearabic-1"
	 ;; "-*-fixed-medium-r-*--*-mulearabic-2"

	 ;; "-*-fixed-medium-r-*--*-muleipa-1"
	 ;; "-*-fixed-medium-r-*--*-ethio-*"

	 "-*-mincho-medium-r-*--*-ksc5601.1987-*" ; Korean
	 "-*-fixed-medium-r-*--*-tis620.2529-1" ; Thai
	 )
       'global '(mule-fonts) 'append))
    
    (add-hook 'zmacs-deactivate-region-hook
	      (lambda ()
		(if (console-on-window-system-p)
		    (disown-selection))))
    (add-hook 'zmacs-activate-region-hook
	      (lambda ()
		(if (console-on-window-system-p)
		    (activate-region-as-selection))))
    (add-hook 'zmacs-update-region-hook
	      (lambda ()
		(if (console-on-window-system-p)
		    (activate-region-as-selection))))

    (define-key global-map 'menu 'popup-mode-menu)
    (setq gtk-post-win-initted t)))
    
(push '("-geometry" . gtk-init-handle-geometry) command-switch-alist)

;;; Stuff to get compose keys working on GTK
(eval-when-compile
  (defmacro gtk-define-dead-key (key map)
    `(when (gtk-keysym-on-keyboard-p ',key)
       (define-key function-key-map [,key] ',map))))

(defun gtk-initialize-compose ()
  "Enable compose processing"
  (autoload 'compose-map	    "gtk-compose" nil t 'keymap)
  (autoload 'compose-acute-map	    "gtk-compose" nil t 'keymap)
  (autoload 'compose-grave-map	    "gtk-compose" nil t 'keymap)
  (autoload 'compose-cedilla-map    "gtk-compose" nil t 'keymap)
  (autoload 'compose-diaeresis-map  "gtk-compose" nil t 'keymap)
  (autoload 'compose-circumflex-map "gtk-compose" nil t 'keymap)
  (autoload 'compose-tilde-map	    "gtk-compose" nil t 'keymap)

  (when (gtk-keysym-on-keyboard-p 'multi-key)
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
  (gtk-define-dead-key acute			compose-acute-map)
  (gtk-define-dead-key grave			compose-grave-map)
  (gtk-define-dead-key cedilla			compose-cedilla-map)
  (gtk-define-dead-key diaeresis		compose-diaeresis-map)
  (gtk-define-dead-key circumflex		compose-circumflex-map)
  (gtk-define-dead-key tilde			compose-tilde-map)
  (gtk-define-dead-key degree			compose-ring-map)

  ;; Sun according to MIT:
  (gtk-define-dead-key SunFA_Acute		compose-acute-map)
  (gtk-define-dead-key SunFA_Grave		compose-grave-map)
  (gtk-define-dead-key SunFA_Cedilla		compose-cedilla-map)
  (gtk-define-dead-key SunFA_Diaeresis		compose-diaeresis-map)
  (gtk-define-dead-key SunFA_Circum		compose-circumflex-map)
  (gtk-define-dead-key SunFA_Tilde		compose-tilde-map)

  ;; Sun according to OpenWindows 2:
  (gtk-define-dead-key Dead_Grave		compose-grave-map)
  (gtk-define-dead-key Dead_Circum		compose-circumflex-map)
  (gtk-define-dead-key Dead_Tilde		compose-tilde-map)

  ;; Sun according to OpenWindows 3:
  (gtk-define-dead-key SunXK_FA_Acute		compose-acute-map)
  (gtk-define-dead-key SunXK_FA_Grave		compose-grave-map)
  (gtk-define-dead-key SunXK_FA_Cedilla		compose-cedilla-map)
  (gtk-define-dead-key SunXK_FA_Diaeresis	compose-diaeresis-map)
  (gtk-define-dead-key SunXK_FA_Circum		compose-circumflex-map)
  (gtk-define-dead-key SunXK_FA_Tilde		compose-tilde-map)

  ;; DEC according to MIT:
  (gtk-define-dead-key Dacute_accent		compose-acute-map)
  (gtk-define-dead-key Dgrave_accent		compose-grave-map)
  (gtk-define-dead-key Dcedilla_accent		compose-cedilla-map)
  (gtk-define-dead-key Dcircumflex_accent	compose-circumflex-map)
  (gtk-define-dead-key Dtilde			compose-tilde-map)
  (gtk-define-dead-key Dring_accent		compose-ring-map)

  ;; DEC according to OpenWindows 3:
  (gtk-define-dead-key DXK_acute_accent		compose-acute-map)
  (gtk-define-dead-key DXK_grave_accent		compose-grave-map)
  (gtk-define-dead-key DXK_cedilla_accent	compose-cedilla-map)
  (gtk-define-dead-key DXK_circumflex_accent	compose-circumflex-map)
  (gtk-define-dead-key DXK_tilde		compose-tilde-map)
  (gtk-define-dead-key DXK_ring_accent		compose-ring-map)

  ;; HP according to MIT:
  (gtk-define-dead-key hpmute_acute		compose-acute-map)
  (gtk-define-dead-key hpmute_grave		compose-grave-map)
  (gtk-define-dead-key hpmute_diaeresis		compose-diaeresis-map)
  (gtk-define-dead-key hpmute_asciicircum	compose-circumflex-map)
  (gtk-define-dead-key hpmute_asciitilde	compose-tilde-map)

  ;; Empirically discovered on Linux XFree86 MetroX:
  (gtk-define-dead-key usldead_acute		compose-acute-map)
  (gtk-define-dead-key usldead_grave		compose-grave-map)
  (gtk-define-dead-key usldead_diaeresis	compose-diaeresis-map)
  (gtk-define-dead-key usldead_asciicircum	compose-circumflex-map)
  (gtk-define-dead-key usldead_asciitilde	compose-tilde-map)

  ;; HP according to OpenWindows 3:
  (gtk-define-dead-key hpXK_mute_acute		compose-acute-map)
  (gtk-define-dead-key hpXK_mute_grave		compose-grave-map)
  (gtk-define-dead-key hpXK_mute_diaeresis	compose-diaeresis-map)
  (gtk-define-dead-key hpXK_mute_asciicircum	compose-circumflex-map)
  (gtk-define-dead-key hpXK_mute_asciitilde	compose-tilde-map)

  ;; HP according to HP-UX 8.0:
  (gtk-define-dead-key XK_mute_acute		compose-acute-map)
  (gtk-define-dead-key XK_mute_grave		compose-grave-map)
  (gtk-define-dead-key XK_mute_diaeresis	compose-diaeresis-map)
  (gtk-define-dead-key XK_mute_asciicircum	compose-circumflex-map)
  (gtk-define-dead-key XK_mute_asciitilde	compose-tilde-map)

  ;; Xfree86 seems to use lower case and a hyphen
  (gtk-define-dead-key dead-acute		compose-acute-map)
  (gtk-define-dead-key dead-grave		compose-grave-map)
  (gtk-define-dead-key dead-cedilla		compose-cedilla-map)
  (gtk-define-dead-key dead-diaeresis		compose-diaeresis-map)
  (gtk-define-dead-key dead-circum		compose-circumflex-map)
  (gtk-define-dead-key dead-circumflex		compose-circumflex-map)
  (gtk-define-dead-key dead-tilde		compose-tilde-map)
  )

(when (featurep 'gtk)
  (add-hook
   'create-console-hook
   (lambda (console)
     (letf (((selected-console) console))
       (when (eq 'gtk (console-type console))
	 (gtk-initialize-compose))))))
