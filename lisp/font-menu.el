;; font-menu.el --- Managing menus of fonts.

;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1997 Sun Microsystems

;; Adapted from x-font-menu.el by Andy Piper <andy@xemacs.org>

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

;; This file contains the device-nospecific font menu stuff

;;; Commentary:
;;;
;;; Creates three menus, "Font", "Size", and "Weight", and puts them on the
;;; "Options" menu.  The contents of these menus are the superset of those
;;; properties available on any fonts, but only the intersection of the three
;;; sets is selectable at one time.
;;;
;;; Known Problems:
;;; ===============
;;; Items on the Font menu are selectable if and only if that font exists in
;;; the same size and weight as the current font.  This means that some fonts
;;; are simply not reachable from some other fonts - if only one font comes
;;; in only one point size (like "Nil", which comes only in 2), you will never
;;; be able to select it.  It would be better if the items on the Fonts menu
;;; were always selectable, and selecting them would set the size to be the
;;; closest size to the current font's size.
;;;
;;; This attempts to change all other faces in an analogous way to the change
;;; that was made to the default face; if it can't, it will skip over the face.
;;; However, this could leave incongruous font sizes around, which may cause
;;; some nonreversibility problems if further changes are made.  Perhaps it
;;; should remember the initial fonts of all faces, and derive all subsequent
;;; fonts from that initial state.
;;;
;;; xfontsel(1) is a lot more flexible (but probably harder to understand).
;;;
;;; The code to construct menus from all of the x11 fonts available from the
;;; server is autoloaded and executed the very first time that one of the Font
;;; menus is selected on each device.  That is, if XEmacs has frames on two
;;; different devices, then separate font menu information will be maintained
;;; for each X display.  If the font path changes after emacs has already
;;; asked the X server on a particular display for its list of fonts, this
;;; won't notice.  Also, the first time that a font menu is posted on each
;;; display will entail a lengthy delay, but that's better than slowing down
;;; XEmacs startup.  At any time (i.e.: after a font-path change or
;;; immediately after device creation), you can call
;;; `reset-device-font-menus' to rebuild the menus from all currently
;;; available fonts.
;;;
;;; There are at least three kinds of fonts under X11r5:
;;;
;;; - bitmap fonts, which can be assumed to look as good as possible;
;;; - bitmap fonts which have been (or can be) automatically scaled to
;;;   a new size, and which almost always look awful;
;;; - and true outline fonts, which should look ok at any size, but in
;;;   practice (on at least some systems) look awful at any size, and
;;;   even in theory are unlikely ever to look as good as non-scaled
;;;   bitmap fonts.
;;;
;;; It would be nice to get this code to look for non-scaled bitmap fonts
;;; first, then outline fonts, then scaled bitmap fonts as a last resort.
;;; But it's not clear to me how to tell them apart based on their truenames
;;; and/or the result of XListFonts().  I welcome any and all explanations
;;; of the subtleties involved...
;;;
;;;
;;; If You Think You'Re Seeing A Bug:
;;; =================================
;;; When reporting problems, send the following information:
;;;
;;; - Exactly what behavior you're seeing;
;;; - The output of the `xlsfonts' program;
;;; - The value of the variable `device-fonts-cache';
;;; - The values of the following expressions, both before and after
;;;   making a selection from any of the fonts-related menus:
;;;	(face-font 'default)
;;;	(font-truename   (face-font 'default))
;;;	(font-properties (face-font 'default))
;;; - The values of the following variables after making a selection:
;;;	font-menu-preferred-resolution
;;;	font-menu-registry-encoding
;;;
;;; There is a common misconception that "*-courier-medium-r-*-11-*", also
;;; known as "-adobe-courier-medium-r-normal--11-80-100-100-m-60-iso8859-1",
;;; is an 11-point font.  It is not -- it is an 11-pixel font at 100dpi,
;;; which is an 8-point font (the number after -11- is the size in tenths
;;; of points).  So if you expect to be seeing an "11" entry in the "Size"
;;; menu and are not, this may be why.
;;;
;;; In the real world (aka Solaris), one has to deal with fonts that
;;; appear to be medium-i but are really light-r, and fonts that
;;; resolve to different resolutions depending on the charset:
;;;
;;; (font-instance-truename
;;;  (make-font-instance "-*-mincho-medium-i-normal-*-*-*-*-*-*-*-jisx0201*-*"))
;;; ==>
;;; "-morisawa-ryumin light kl-light-r-normal--10-100-72-72-m-50-jisx0201.1976-0"
;;;
;;; (list-fonts "-dt-interface user-medium-r-normal-s*-*-*-*-*-*-*-*-*")
;;; ==>
;;; ("-dt-interface user-medium-r-normal-s sans-12-120-72-72-m-70-iso8859-1"
;;;  "-dt-interface user-medium-r-normal-s-14-120-75-75-m-120-jisx0208.1983-0"
;;;  "-dt-interface user-medium-r-normal-s-14-120-75-75-m-60-jisx0201.1976-0")

;;;###autoload
(defcustom font-menu-ignore-scaled-fonts nil
  "*If non-nil, then the font menu will try to show only bitmap fonts."
  :type 'boolean
  :group 'font-menu)

;;;###autoload
(defcustom font-menu-this-frame-only-p nil
  "*If non-nil, then changing the default font from the font menu will only
affect one frame instead of all frames."
  :type 'boolean
  :group 'font-menu)

(defcustom font-menu-max-items 25
  "*Maximum number of items in the font menu
If number of entries in a menu is larger than this value, split menu
into submenus of nearly equal length.  If nil, never split menu into
submenus."
  :group 'font-menu
  :type '(choice (const :tag "no submenus" nil)
		 (integer)))

(defcustom font-menu-submenu-name-format "%-12.12s ... %.12s"
  "*Format specification of the submenu name.
Used by `font-menu-split-long-menu' if the number of entries in a menu is
larger than `font-menu-menu-max-items'.
This string should contain one %s for the name of the first entry and
one %s for the name of the last entry in the submenu.
If the value is a function, it should return the submenu name.  The
function is be called with two arguments, the names of the first and
the last entry in the menu."
  :group 'font-menu
  :type '(choice (string :tag "Format string")
		 (function)))

(defvar font-menu-preferred-resolution
  (make-specifier-and-init 'generic '((global
				       ((x) . "*-*"))) t)
  "Preferred horizontal and vertical font menu resolution (e.g. \"75:75\").")

(defvar font-menu-size-scaling
  (make-specifier-and-init 'integer '((global
				       ((x) . 10))) t)
  "Scale factor used in defining font sizes.")

;; only call XListFonts (and parse) once per device.
;; ( (device . [parsed-list-fonts family-menu size-menu weight-menu]) ...)
(defvar device-fonts-cache nil)

(defsubst device-fonts-cache ()
  (or (cdr (assq (selected-device) device-fonts-cache))
      (and (reset-device-font-menus (selected-device))
	   (cdr (assq (selected-device) device-fonts-cache)))))

;;;###autoload
(fset 'install-font-menus 'reset-device-font-menus)
(make-obsolete 'install-font-menus 'reset-device-font-menus)

;;;###autoload
(defun reset-device-font-menus (&optional device debug)
  "Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system,
or if you change your font path, you can call this to re-initialize the menus."
  (message "Getting list of fonts from server... ")
  (if (or noninteractive
	  (not (or device (setq device (selected-device)))))
      nil
    (call-device-method 'reset-device-font-menus device device debug)
    (message "Getting list of fonts from server... done.")))

(defun font-menu-split-long-menu (menu)
  "Split MENU according to `font-menu-max-items' and add accelerator specs."
  (let ((len (length menu)))
    (if (or (null font-menu-max-items)
	    (null (featurep 'lisp-float-type))
	    (<= len font-menu-max-items))
	(submenu-generate-accelerator-spec menu)
      ;; Submenu is max 2 entries longer than menu, never shorter, number of
      ;; entries in submenus differ by at most one (with longer submenus first)
      (let* ((outer (floor (sqrt len)))
	     (inner (/ len outer))
	     (rest (% len outer))
	     (result nil))
	(setq menu (reverse menu))
	(while menu
	  (let ((in inner)
		(sub nil)
		(to (car menu)))
	    (while (> in 0)
	      (setq in   (1- in)
		    sub  (cons (car menu) sub)
		    menu (cdr menu)))
	    (setq result
		  (cons (cons (if (stringp font-menu-submenu-name-format)
				  (format font-menu-submenu-name-format
					  (menu-item-strip-accelerator-spec
					   (aref (car sub) 0))
					  (menu-item-strip-accelerator-spec
					   (aref to 0)))
				(funcall font-menu-submenu-name-format
					 (menu-item-strip-accelerator-spec
					  (aref (car sub) 0))
					 (menu-item-strip-accelerator-spec
					  (aref to 0))))
			      (submenu-generate-accelerator-spec sub))
			result)
		  rest  (1+ rest))
	    (if (= rest outer) (setq inner (1+ inner)))))
	(submenu-generate-accelerator-spec result)))))

;;;###autoload
(defun font-menu-family-constructor (ignored)
  (catch 'menu
    (unless (console-on-window-system-p)
      (throw 'menu '(["Cannot parse current font" ding nil])))
    (let* ((dcache (device-fonts-cache))
	   (font-data (font-menu-font-data 'default dcache))
	   (entry  (aref font-data 0))
	   (family (aref font-data 1))
	   (size   (aref font-data 2))
	   (weight (aref font-data 3))
	   f)
      (unless family
	(throw 'menu '(["Cannot parse current font" ding nil])))
      ;; Items on the Font menu are enabled iff that font exists in
      ;; the same size and weight as the current font (scalable fonts
      ;; exist in every size).  Only the current font is marked as
      ;; selected.
      (font-menu-split-long-menu
       (mapcar
	(lambda (item)
	  (setq f (menu-item-strip-accelerator-spec (aref item 0))
		entry (vassoc f (aref dcache 0)))
	  (if (and (or (member weight (aref entry 1))
		       ;; mswindows often allows any weight - who cares?
		       (member "" (aref entry 1)))
		   (or (member size (aref entry 2))
		       (and (not font-menu-ignore-scaled-fonts)
			    (member 0 (aref entry 2)))))
	      (enable-menu-item item)
	    (disable-menu-item item))
	  (if (string-equal family f)
	      (select-toggle-menu-item item)
	    (deselect-toggle-menu-item item))
	  item)
	(aref dcache 1))))))

(define-device-method* font-menu-font-data)

;;;###autoload
(defun font-menu-size-constructor (ignored)
  (catch 'menu
    (unless (console-on-window-system-p)
      (throw 'menu '(["Cannot parse current font" ding nil])))
    (let* ((dcache (device-fonts-cache))
	   (font-data (font-menu-font-data 'default dcache))
	   (entry  (aref font-data 0))
	   (family (aref font-data 1))
	   (size   (aref font-data 2))
	   ;;(weight (aref font-data 3))
	   s)
      (unless family
	(throw 'menu '(["Cannot parse current font" ding nil])))
      ;; Items on the Size menu are enabled iff current font has
      ;; that size.  Only the size of the current font is selected.
      ;; (If the current font comes in size 0, it is scalable, and
      ;; thus has every size.)
      (mapcar
       (lambda (item)
	 (setq s (nth 3 (aref item 1)))
	 (if (or (member s (aref entry 2))
		 (and (not font-menu-ignore-scaled-fonts)
		      (member 0 (aref entry 2))))
	     (enable-menu-item item)
	   (disable-menu-item item))
	 (if (eq size s)
	     (select-toggle-menu-item item)
	   (deselect-toggle-menu-item item))
	 item)
       (submenu-generate-accelerator-spec (aref dcache 2))))))

;;;###autoload
(defun font-menu-weight-constructor (ignored)
  (catch 'menu
    (unless (console-on-window-system-p)
      (throw 'menu '(["Cannot parse current font" ding nil])))
    (let* ((dcache (device-fonts-cache))
	   (font-data (font-menu-font-data 'default dcache))
	   (entry  (aref font-data 0))
	   (family (aref font-data 1))
	   ;;(size   (aref font-data 2))
	   (weight (aref font-data 3))
	   w)
      (unless family
	(throw 'menu '(["Cannot parse current font" ding nil])))
      ;; Items on the Weight menu are enabled iff current font
      ;; has that weight.  Only the weight of the current font
      ;; is selected.
      (mapcar
       (lambda (item)
	 (setq w (aref item 0))
	 (if (member w (aref entry 1))
	     (enable-menu-item item)
	   (disable-menu-item item))
	 (if (string-equal weight w)
	     (select-toggle-menu-item item)
	   (deselect-toggle-menu-item item))
	 item)
       (submenu-generate-accelerator-spec (aref dcache 3))))))


;;; Changing font sizes

(defun font-menu-set-font (family weight size)
  ;; This is what gets run when an item is selected from any of the three
  ;; fonts menus.  It needs to be rather clever.
  ;; (size is measured in 10ths of points.)
  (let* ((dcache (device-fonts-cache))
	 (font-data (font-menu-font-data 'default dcache))
	 (from-family (aref font-data 1))
	 (from-size   (aref font-data 2))
	   (from-weight (aref font-data 3))
	 (from-slant  (aref font-data 4))
	 (face-list-to-change (delq 'default (face-list)))
	 new-default-face-font)
    (unless from-family
      (signal 'error '("couldn't parse font name for default face")))
    (when weight
      (signal 'error '("Setting weight currently not supported")))
    (setq new-default-face-font
	  (font-menu-load-font
	   (or family from-family)
	   (or weight from-weight)
	   (or size   from-size)
	   from-slant
	   (specifier-instance
	    font-menu-preferred-resolution (selected-device))))
    (dolist (face face-list-to-change)
      (when (face-font-instance face)
	(message "Changing font of `%s'..." face)
	(condition-case c
	    (font-menu-change-face face
				   from-family from-weight from-size
				   (or family from-family)
				   (or weight from-weight)
				   (or size from-size))
	  (error
	   (display-error c nil)
	   (sit-for 1)))))
    ;; Set the default face's font after hacking the other faces, so that
    ;; the frame size doesn't change until we are all done.

    ;; If we need to be frame local we do the changes ourselves.
    (if font-menu-this-frame-only-p
    ;;; WMP - we need to honor font-menu-this-frame-only-p here!
	(set-face-font 'default new-default-face-font
		       (and font-menu-this-frame-only-p (selected-frame)))
      ;; OK Let Customize do it.
      (custom-set-face-update-spec 'default
				   (list (list 'type (device-type)))
				   (list :family (or family from-family)
					 :size (concat
						(int-to-string
						 (/ (or size from-size)
						    (specifier-instance font-menu-size-scaling
									(selected-device))))
						"pt")))
      (message "Font %s" (face-font-name 'default)))))


(defun font-menu-change-face (face
			      from-family from-weight from-size
			      to-family   to-weight   to-size)
  (check-type face symbol)
  (let* ((dcache (device-fonts-cache))
	 (font-data (font-menu-font-data face dcache))
	 (face-family (aref font-data 1))
	 (face-size   (aref font-data 2))
	 (face-weight (aref font-data 3))
	 (face-slant  (aref font-data 4)))

    (or face-family
	(signal 'error (list "couldn't parse font name for face" face)))

    ;; If this face matches the old default face in the attribute we
    ;; are changing, then change it to the new attribute along that
    ;; dimension.  Also, the face must have its own global attribute.
    ;; If its value is inherited, we don't touch it.  If any of this
    ;; is not true, we leave it alone.
    (when (and (face-font face 'global)
	       (cond
		(to-family (string-equal face-family from-family))
		(to-weight (string-equal face-weight from-weight))
		(to-size   (=            face-size   from-size))))
      (set-face-font face
		     (font-menu-load-font (or to-family face-family)
					  (or to-weight face-weight)
					  (or to-size   face-size)
					  face-slant
					  (specifier-instance
					   font-menu-preferred-resolution
					   (selected-device)))
		     (and font-menu-this-frame-only-p
			  (selected-frame))))))

(define-device-method font-menu-load-font)

(defun flush-device-fonts-cache (device)
  ;; by Stig@hackvan.com
  (let ((elt (assq device device-fonts-cache)))
    (and elt
	 (setq device-fonts-cache (delq elt device-fonts-cache)))))

(add-hook 'delete-device-hook 'flush-device-fonts-cache)

(provide 'font-menu)

;; font-menu ends here
