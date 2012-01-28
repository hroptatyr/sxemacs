;;; font.el --- New font model
;; Author: wmperry
;; Created: 1997/09/05 15:44:37
;; Version: 1.52
;; Keywords: faces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995, 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is part of SXEmacs.
;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cus-face)

(globally-declare-fboundp
 '(x-list-fonts internal-facep fontsetp
		get-font-info get-fontset-info font-color-rgb-components
		font-rgb-color-p cancel-function-timers run-at-time
		set-font-oblique-p set-font-italic-p set-font-bold-p
		font-dropcaps-p font-bigcaps-p font-smallcaps-p font-blink-p
		font-reverse-p font-strikethru-p font-linethrough-p
		font-overline-p font-underline-p font-dim-p font-oblique-p
		font-italic-p font-bold-p))

(globally-declare-boundp 'global-face-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The emacsen compatibility package - load it up before anything else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)

(eval-and-compile
  (defvar device-fonts-cache)
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      `(defvar ,var ,value ,doc))))

(if (not (fboundp 'try-font-name))
    (defun try-font-name (fontname &rest args)
      (case window-system
	((x) (car-safe (x-list-fonts fontname)))
	(otherwise nil))))

(if (not (fboundp 'facep))
    (defun facep (face)
      "Return t if X is a face name or an internal face vector."
      (if (not window-system)
	  nil				; FIXME if FSF ever does TTY faces
	(and (or (internal-facep face)
		 (and (symbolp face) (assq face global-face-data)))
	     t))))

(if (not (fboundp 'set-face-property))
    (defun set-face-property (face property value &optional locale
				   tag-set how-to-add)
      "Change a property of FACE."
      (and (symbolp face)
	   (put face property value))))

(if (not (fboundp 'face-property))
    (defun face-property (face property &optional locale tag-set exact-p)
      "Return FACE's value of the given PROPERTY."
      (and (symbolp face) (get face property))))

(require 'disp-table)

(eval-and-compile
  (unless (fboundp #'<<)
    (fset #'<< #'lsh))
  (unless (fboundp #'&)
    (fset #'& #'logand))
  (unless (fboundp #'|)
    (fset #'| #'logior))
  (unless (fboundp #'~)
    (fset #'~ #'lognot))
  (unless (fboundp #'>>)
    (defun >> (value count)
      (<< value (- count)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lots of variables / keywords for use later in the program
;;; Not much should need to be modified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst font-running-xemacs (string-match "XEmacs" (emacs-version))
  "Whether we are running in XEmacs or not.")

(defmacro define-font-keywords (&rest keys)
  `(eval-and-compile
     (let ((keywords (quote ,keys)))
       (while keywords
	 (or (boundp (car keywords))
	     (set (car keywords) (car keywords)))
	 (setq keywords (cdr keywords))))))

(defconst font-window-system-mappings
  '((x         . (x-font-create-name x-font-create-object))
    (tty       . (tty-font-create-plist tty-font-create-object)))
  "An assoc list mapping device types to a list of translations.

The first function creates a font name from a font descriptor object.
The second performs the reverse translation.")

(defconst x-font-weight-mappings
  '((:extra-light . "extralight")
    (:light       . "light")
    (:demi-light  . "demilight")
    (:demi        . "demi")
    (:book        . "book")
    (:medium      . "medium")
    (:normal      . "medium")
    (:demi-bold   . "demibold")
    (:bold        . "bold")
    (:extra-bold  . "extrabold"))
  "An assoc list mapping keywords to actual Xwindow specific strings
for use in the 'weight' field of an X font string.")

(defconst font-possible-weights
  (mapcar 'car x-font-weight-mappings))

(defvar font-rgb-file nil
  "Where the RGB file was found.")

(defvar font-maximum-slippage "1pt"
  "How much a font is allowed to vary from the desired size.")

;; Canonical (internal) sizes are in points.
;; Registry
(define-font-keywords :family :style :size :registry :encoding)

(define-font-keywords
  :weight :extra-light :light :demi-light :medium :normal :demi-bold
  :bold :extra-bold)

(defvar font-style-keywords nil)

(defsubst set-font-family (fontobj family)
  (aset fontobj 1 family))

(defsubst set-font-weight (fontobj weight)
  (aset fontobj 3 weight))

(defsubst set-font-style (fontobj style)
  (aset fontobj 5 style))

(defsubst set-font-size (fontobj size)
  (aset fontobj 7 size))

(defsubst set-font-registry (fontobj reg)
  (aset fontobj 9 reg))

(defsubst set-font-encoding (fontobj enc)
  (aset fontobj 11 enc))

(defsubst font-family (fontobj)
  (aref fontobj 1))

(defsubst font-weight (fontobj)
  (aref fontobj 3))

(defsubst font-style (fontobj)
  (aref fontobj 5))

(defsubst font-size (fontobj)
  (aref fontobj 7))

(defsubst font-registry (fontobj)
  (aref fontobj 9))

(defsubst font-encoding (fontobj)
  (aref fontobj 11))

(eval-when-compile
  (defmacro define-new-mask (attr mask)
    `(progn
       (setq font-style-keywords
	     (cons (cons (quote ,attr)
			 (cons
			  (quote ,(intern (format "set-font-%s-p" attr)))
			  (quote ,(intern (format "font-%s-p" attr)))))
		   font-style-keywords))
       (defconst ,(intern (format "font-%s-mask" attr)) (<< 1 ,mask)
	 ,(format
	   "Bitmask for whether a font is to be rendered in %s or not."
	   attr))
       (defun ,(intern (format "font-%s-p" attr)) (fontobj)
	 ,(format "Whether FONTOBJ will be renderd in `%s' or not." attr)
	 (if (/= 0 (& (font-style fontobj)
		      ,(intern (format "font-%s-mask" attr))))
	     t
	   nil))
       (defun ,(intern (format "set-font-%s-p" attr)) (fontobj val)
	 ,(format "Set whether FONTOBJ will be renderd in `%s' or not."
		  attr)
	 (cond
	  (val
	   (set-font-style fontobj (| (font-style fontobj)
				      ,(intern
					(format "font-%s-mask" attr)))))
	  ((,(intern (format "font-%s-p" attr)) fontobj)
	   (set-font-style fontobj (- (font-style fontobj)
				      ,(intern
					(format "font-%s-mask" attr)))))))
       )))

(let ((mask 0))
  (define-new-mask bold        (setq mask (1+ mask)))
  (define-new-mask italic      (setq mask (1+ mask)))
  (define-new-mask oblique     (setq mask (1+ mask)))
  (define-new-mask dim         (setq mask (1+ mask)))
  (define-new-mask underline   (setq mask (1+ mask)))
  (define-new-mask overline    (setq mask (1+ mask)))
  (define-new-mask linethrough (setq mask (1+ mask)))
  (define-new-mask strikethru  (setq mask (1+ mask)))
  (define-new-mask reverse     (setq mask (1+ mask)))
  (define-new-mask blink       (setq mask (1+ mask)))
  (define-new-mask smallcaps   (setq mask (1+ mask)))
  (define-new-mask bigcaps     (setq mask (1+ mask)))
  (define-new-mask dropcaps    (setq mask (1+ mask))))

(defvar font-caps-display-table
  (let ((table (make-display-table))
	(i 0))
    ;; Standard ASCII characters
    (while (< i 26)
      (aset table (+ i ?a) (+ i ?A))
      (setq i (1+ i)))
    ;; Now ISO translations
    (setq i 224)
    (while (< i 247)			;; Agrave - Ouml
      (aset table i (- i 32))
      (setq i (1+ i)))
    (setq i 248)
    (while (< i 255)			;; Oslash - Thorn
      (aset table i (- i 32))
      (setq i (1+ i)))
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst set-font-style-by-keywords (fontobj styles)
  (make-local-variable 'font-func)
  (declare (special font-func))
  (if (listp styles)
      (while styles
	(setq font-func (car-safe (cdr-safe (assq (car styles) font-style-keywords)))
	      styles (cdr styles))
	(and (fboundp font-func) (funcall font-func fontobj t)))
    (setq font-func (car-safe (cdr-safe (assq styles font-style-keywords))))
    (and (fboundp font-func) (funcall font-func fontobj t))))

(defsubst font-properties-from-style (fontobj)
  (let (;(style (font-style fontobj))
	(todo font-style-keywords)
	type func retval)
    (while todo
      (setq func (cdr (cdr (car todo)))
	    type (car (pop todo)))
      (if (funcall func fontobj)
	  (setq retval (cons type retval))))
    retval))

(defun font-unique (list)
  (let ((retval)
	(cur))
    (while list
      (setq cur (car list)
	    list (cdr list))
      (if (member cur retval)
	  nil
	(setq retval (cons cur retval))))
    (nreverse retval)))

(defun font-higher-weight (w1 w2)
  (let ((index1 (length (memq w1 font-possible-weights)))
	(index2 (length (memq w2 font-possible-weights))))
    (cond
     ((<= index1 index2)
      (or w1 w2))
     ((not w2)
      w1)
     (t
      w2))))

(defun font-spatial-to-canonical (spec &optional device)
  "Convert SPEC (in inches, millimeters, points, picas, or pixels) into points.

Canonical sizes are in points.  If SPEC is null, nil is returned.  If SPEC is
a number, it is interpreted as the desired point size and returned unchanged.
Otherwise SPEC must be a string consisting of a number and an optional type.
The type may be the strings \"px\", \"pix\", or \"pixel\" (pixels), \"pt\" or
\"point\" (points), \"pa\" or \"pica\" (picas), \"in\" or \"inch\" (inches), \"cm\"
(centimeters), or \"mm\" (millimeters).

1 in = 2.54 cm = 6 pa = 25.4 mm = 72 pt.  Pixel size is device-dependent."
  (cond
   ((numberp spec)
    spec)
   ((null spec)
    nil)
   (t
    (let ((num nil)
	  (type nil)
	  ;; If for any reason we get null for any of this, default
	  ;; to 1024x768 resolution on a 17" screen
	  (pix-width (float (or (device-pixel-width device) 1024)))
	  (mm-width (float (or (device-mm-width device) 293)))
	  (retval nil))
      (cond
       ;; the following string-match is broken, there will never be a
       ;; left operand detected
       ((string-match #r"^ *\([-+*/]\) *" spec) ; math!  whee!
	(let ((math-func (intern (match-string 1 spec)))
	      (other (font-spatial-to-canonical
		      (substring spec (match-end 0) nil)))
	      (default (font-spatial-to-canonical
			(font-default-size-for-device device))))
	  (if (fboundp math-func)
	      (setq type "px"
		    spec (int-to-string (funcall math-func default other)))
	    (setq type "px"
		  spec (int-to-string other)))))
       ((string-match "[^0-9.]+$" spec)
	(setq type (substring spec (match-beginning 0))
	      spec (substring spec 0 (match-beginning 0))))
       (t
	(setq type "px"
	      spec spec)))
      (setq num (string-to-number spec))
      (cond
       ((member type '("pixel" "px" "pix"))
	(setq retval (* num (/ pix-width mm-width) (/ 25.4 72.0))))
       ((member type '("point" "pt"))
	(setq retval num))
       ((member type '("pica" "pa"))
	(setq retval (* num 12.0)))
       ((member type '("inch" "in"))
	(setq retval (* num 72.0)))
       ((string= type "mm")
	(setq retval (* num (/ 72.0 25.4))))
       ((string= type "cm")
	(setq retval (* num 10 (/ 72.0 25.4))))
       (t
	(setq retval num))
       )
      retval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main interface routines - constructors and accessor functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-font (&rest args)
  (vector :family
	  (if (stringp (plist-get args :family))
	      (list (plist-get args :family))
	    (plist-get args :family))
	  :weight
	  (plist-get args :weight)
	  :style
	  (if (numberp (plist-get args :style))
	      (plist-get args :style)
	    0)
	  :size
	  (plist-get args :size)
	  :registry
	  (plist-get args :registry)
	  :encoding
	  (plist-get args :encoding)))

(defun font-create-name (fontobj &optional device)
  "Return a font name constructed from FONTOBJ, appropriate for DEVICE."
  (let* ((type (device-type device))
	 (func (car (cdr-safe (assq type font-window-system-mappings)))))
    (and func (fboundp func) (funcall func fontobj device))))

;;;###autoload
(defun font-create-object (fontname &optional device)
  "Return a font descriptor object for FONTNAME, appropriate for DEVICE."
  (let* ((type (device-type device))
	 (func (car (cdr (cdr-safe (assq type font-window-system-mappings))))))
    (and func (fboundp func) (funcall func fontname device))))

(defun font-combine-fonts-internal (fontobj-1 fontobj-2)
  (let ((retval (make-font))
	(size-1 (and (font-size fontobj-1)
		     (font-spatial-to-canonical (font-size fontobj-1))))
	(size-2 (and (font-size fontobj-2)
		     (font-spatial-to-canonical (font-size fontobj-2)))))
    (set-font-weight retval (font-higher-weight (font-weight fontobj-1)
						(font-weight fontobj-2)))
    (set-font-family retval (font-unique (append (font-family fontobj-1)
						 (font-family fontobj-2))))
    (set-font-style retval (| (font-style fontobj-1) (font-style fontobj-2)))
    (set-font-registry retval (or (font-registry fontobj-1)
				  (font-registry fontobj-2)))
    (set-font-encoding retval (or (font-encoding fontobj-1)
				  (font-encoding fontobj-2)))
    (set-font-size retval (cond
			   ((and size-1 size-2 (>= size-2 size-1))
			    (font-size fontobj-2))
			   ((and size-1 size-2)
			    (font-size fontobj-1))
			   (size-1
			    (font-size fontobj-1))
			   (size-2
			    (font-size fontobj-2))
			   (t nil)))

    retval))

(defun font-combine-fonts (&rest args)
  (cond
   ((null args)
    (error "Wrong number of arguments to font-combine-fonts"))
   ((= (length args) 1)
    (car args))
   (t
    (let ((retval (font-combine-fonts-internal (nth 0 args) (nth 1 args))))
      (setq args (cdr (cdr args)))
      (while args
	(setq retval (font-combine-fonts-internal retval (car args))
	      args (cdr args)))
      retval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (TTY-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tty-font-create-object (fontname &optional device)
  "Return a font descriptor object for FONTNAME, appropriate for TTY devices."
  (make-font :size "12pt"))

(defun tty-font-create-plist (fontobj &optional device)
  "Return a font name constructed from FONTOBJ, appropriate for TTY devices."
  (list
   (cons 'underline (font-underline-p fontobj))
   (cons 'highlight (if (or (font-bold-p fontobj)
			    (memq (font-weight fontobj) '(:bold :demi-bold)))
			t))
   (cons 'dim       (font-dim-p fontobj))
   (cons 'blinking  (font-blink-p fontobj))
   (cons 'reverse   (font-reverse-p fontobj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (X-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar font-x-font-regexp (or (and font-running-xemacs
				    (boundp 'x-font-regexp)
				    x-font-regexp)
 (let
     ((-		"[-?]")
      (foundry		"[^-]*")
      (family		"[^-]*")
      ;(weight		#r"\(bold\|demibold\|medium\|black\)")
      (weight\?		#r"\([^-]*\)")
      ;(slant		#r"\([ior]\)")
      (slant\?		#r"\([^-]?\)")
      (swidth		#r"\([^-]*\)")
      (adstyle		#r"\([^-]*\)")
      (pixelsize	#r"\(\*\|[0-9]+\)")
      (pointsize	#r"\(\*\|0\|[0-9][0-9]+\)")
      (resx		#r"\([*0]\|[0-9][0-9]+\)")
      (resy		#r"\([*0]\|[0-9][0-9]+\)")
      (spacing		"[cmp?*]")
      (avgwidth		#r"\(\*\|[0-9]+\)")
      (registry		"[^-]*")
      (encoding	"[^-]+")
      )
   (concat #r"\`\*?[-?*]"
	   foundry - family - weight\? - slant\? - swidth - adstyle -
	   pixelsize - pointsize - resx - resy - spacing - avgwidth -
	   registry - encoding "\\'"
	   ))))

(defvar font-x-registry-and-encoding-regexp
  (or (and font-running-xemacs
	   (boundp 'x-font-regexp-registry-and-encoding)
	   (symbol-value 'x-font-regexp-registry-and-encoding))
      (let ((- "[-?]")
	    (registry "[^-]*")
	    (encoding "[^-]+"))
	(concat - "\\(" registry "\\)" - "\\(" encoding "\\)\\'"))))

(defvar font-x-family-mappings
  '(
    ("serif"        . ("new century schoolbook"
		       "utopia"
		       "charter"
		       "times"
		       "lucidabright"
		       "garamond"
		       "palatino"
		       "times new roman"
		       "baskerville"
		       "bookman"
		       "bodoni"
		       "computer modern"
		       "rockwell"
		       ))
    ("sans-serif"   . ("lucida"
		       "helvetica"
		       "gills-sans"
		       "avant-garde"
		       "univers"
		       "optima"))
    ("elfin"        . ("tymes"))
    ("monospace"    . ("courier"
		       "fixed"
		       "lucidatypewriter"
		       "clean"
		       "terminal"))
    ("cursive"      . ("sirene"
		       "zapf chancery"))
    )
  "A list of font family mappings on X devices.")

(defun x-font-create-object (fontname &optional device)
  "Return a font descriptor object for FONTNAME, appropriate for X devices."
  (let ((case-fold-search t))
    (if (or (not (stringp fontname))
	    (not (string-match font-x-font-regexp fontname)))
	(make-font)
      (let ((family nil)
	    ;(style nil)
	    (size nil)
	    (weight  (match-string 1 fontname))
	    (slant   (match-string 2 fontname))
	    (swidth  (match-string 3 fontname))
	    (adstyle (match-string 4 fontname))
	    (pxsize  (match-string 5 fontname))
	    (ptsize  (match-string 6 fontname))
	    (retval nil)
	    (case-fold-search t)
	    )
	(if (not (string-match x-font-regexp-foundry-and-family fontname))
	    nil
	  (setq family (list (downcase (match-string 1 fontname)))))
	(if (string= "*" weight)  (setq weight  nil))
	(if (string= "*" slant)   (setq slant   nil))
	(if (string= "*" swidth)  (setq swidth  nil))
	(if (string= "*" adstyle) (setq adstyle nil))
	(if (string= "*" pxsize)  (setq pxsize  nil))
	(if (string= "*" ptsize)  (setq ptsize  nil))
	(if ptsize (setq size (/ (string-to-int ptsize) 10)))
	(if (and (not size) pxsize) (setq size (concat pxsize "px")))
	(if weight (setq weight (intern-soft (concat ":" (downcase weight)))))
	(if (and adstyle (not (equal adstyle "")))
	    (setq family (append family (list (downcase adstyle)))))
	(setq retval (make-font :family family
				:weight weight
				:size size))
	(set-font-bold-p retval (eq :bold weight))
	(cond
	 ((null slant) nil)
	 ((member slant '("i" "I"))
	  (set-font-italic-p retval t))
	 ((member slant '("o" "O"))
	  (set-font-oblique-p retval t)))
	(when (string-match font-x-registry-and-encoding-regexp fontname)
	  (set-font-registry retval (match-string 1 fontname))
	  (set-font-encoding retval (match-string 2 fontname)))
	retval))))

(defun x-font-families-for-device (&optional device no-resetp)
  (ignore-errors (require 'x-font-menu))
  (or device (setq device (selected-device)))
  (if (boundp 'device-fonts-cache)
      (let ((menu (or (cdr-safe (assq device device-fonts-cache)))))
	(if (and (not menu) (not no-resetp))
	    (progn
	      (declare-fboundp (reset-device-font-menus device))
	      (x-font-families-for-device device t))
	  (let ((scaled (mapcar #'(lambda (x) (if x (aref x 0)))
				(aref menu 0)))
		(normal (mapcar #'(lambda (x) (if x (aref x 0)))
				(aref menu 1))))
	    (sort (font-unique (nconc scaled normal)) 'string-lessp))))
    (cons "monospace" (mapcar 'car font-x-family-mappings))))

(defvar font-default-cache nil)

;;;###autoload
(defun font-default-font-for-device (&optional device)
  (or device (setq device (selected-device)))
  (if font-running-xemacs
      (font-truename
       (make-font-specifier
	(face-font-name 'default device)))
    (let ((font (cdr-safe (assq 'font (frame-parameters device)))))
      (if (and (fboundp 'fontsetp) (fontsetp font))
	  (aref (get-font-info (aref (cdr (get-fontset-info font)) 0)) 2)
	font))))

;;;###autoload
(defun font-default-object-for-device (&optional device)
  (let ((font (font-default-font-for-device device)))
    (or (cdr-safe (assoc font font-default-cache))
	(let ((object (font-create-object font)))
	  (push (cons font object) font-default-cache)
	  object))))

;;;###autoload
(defun font-default-family-for-device (&optional device)
  (font-family (font-default-object-for-device (or device (selected-device)))))

;;;###autoload
(defun font-default-registry-for-device (&optional device)
  (font-registry (font-default-object-for-device (or device (selected-device)))))

;;;###autoload
(defun font-default-encoding-for-device (&optional device)
  (font-encoding (font-default-object-for-device (or device (selected-device)))))

;;;###autoload
(defun font-default-size-for-device (&optional device)
  ;; face-height isn't the right thing (always 1 pixel too high?)
  ;; (if font-running-xemacs
  ;;    (format "%dpx" (face-height 'default device))
  (font-size (font-default-object-for-device (or device (selected-device)))))

(defun x-font-create-name (fontobj &optional device)
  "Return a font name constructed from FONTOBJ, appropriate for X devices."
  (if (and (not (or (font-family fontobj)
		    (font-weight fontobj)
		    (font-size fontobj)
		    (font-registry fontobj)
		    (font-encoding fontobj)))
	   (= (font-style fontobj) 0))
      (face-font 'default)
    (or device (setq device (selected-device)))
    (let* ((default (font-default-object-for-device device))
	   (family (or (font-family fontobj)
		       (font-family default)
		       (x-font-families-for-device device)))
	   (weight (or (font-weight fontobj) :medium))
	   ;(style (font-style fontobj))
	   (size (or (if font-running-xemacs
			 (font-size fontobj))
		     (font-size default)))
	   (registry (or (font-registry fontobj)
			 (font-registry default)
			 "*"))
	   (encoding (or (font-encoding fontobj)
			 (font-encoding default)
			 "*")))
      (if (stringp family)
	  (setq family (list family)))
      (setq weight (font-higher-weight weight
				       (and (font-bold-p fontobj) :bold)))
      (if (stringp size)
	  (setq size (truncate (font-spatial-to-canonical size device))))
      (setq weight (or (cdr-safe (assq weight x-font-weight-mappings)) "*"))
      (let ((done nil)			; Did we find a good font yet?
	    (font-name nil)		; font name we are currently checking
	    (cur-family nil)		; current family we are checking
	    )
	(while (and family (not done))
	  (setq cur-family (car family)
		family (cdr family))
	  (if (assoc cur-family font-x-family-mappings)
	      ;; If the family name is an alias as defined by
	      ;; font-x-family-mappings, then append those families
	      ;; to the front of 'family' and continue in the loop.
	      (setq family (append
			    (cdr-safe (assoc cur-family
					     font-x-family-mappings))
			    family))
	    ;; Not an alias for a list of fonts, so we just check it.
	    ;; First, convert all '-' to spaces so that we don't screw up
	    ;; the oh-so wonderful X font model.  Wheee.
	    (let ((x (length cur-family)))
	      (while (> x 0)
		(if (= ?- (aref cur-family (1- x)))
		    (aset cur-family (1- x) ? ))
		(setq x (1- x))))
	    ;; We treat oblique and italic as equivalent.  Don't ask.
	    (let ((slants '("o" "i")))
	      (while (and slants (not done))
		(setq font-name (format "-*-%s-%s-%s-*-*-*-%s-*-*-*-*-%s-%s"
					cur-family weight
					(if (or (font-italic-p fontobj)
						(font-oblique-p fontobj))
					    (car slants)
					  "r")
					(if size
					    (int-to-string (* 10 size)) "*")
					registry
					encoding
					)
		      slants (cdr slants)
		      done (try-font-name font-name device))))))
	(if done font-name)))))


;;; Cache building code
;;;###autoload
(defun x-font-build-cache (&optional device)
  (let ((hash-table (make-hash-table :test 'equal :size 15))
	(fonts (mapcar 'x-font-create-object
		       (x-list-fonts "-*-*-*-*-*-*-*-*-*-*-*-*-*-*")))
	(plist nil)
	(cur nil))
    (while fonts
      (setq cur (car fonts)
	    fonts (cdr fonts)
	    plist (cl-gethash (car (font-family cur)) hash-table))
      (if (not (memq (font-weight cur) (plist-get plist 'weights)))
	  (setq plist (plist-put plist 'weights (cons (font-weight cur)
						      (plist-get plist 'weights)))))
      (if (not (member (font-size cur) (plist-get plist 'sizes)))
	  (setq plist (plist-put plist 'sizes (cons (font-size cur)
						    (plist-get plist 'sizes)))))
      (if (and (font-oblique-p cur)
	       (not (memq 'oblique (plist-get plist 'styles))))
	  (setq plist (plist-put plist 'styles (cons 'oblique (plist-get plist 'styles)))))
      (if (and (font-italic-p cur)
	       (not (memq 'italic (plist-get plist 'styles))))
	  (setq plist (plist-put plist 'styles (cons 'italic (plist-get plist 'styles)))))
      (cl-puthash (car (font-family cur)) plist hash-table))
    hash-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now overwrite the original copy of set-face-font with our own copy that
;;; can deal with either syntax.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ###autoload
(defun font-set-face-font (&optional face font &rest args)
  (cond
   ((and (vectorp font) (= (length font) 12))
    (let ((font-name (font-create-name font)))
      (set-face-property face 'font-specification font)
      (cond
       ((null font-name)		; No matching font!
	nil)
       ((listp font-name)		; For TTYs
	(let (cur)
	  (while font-name
	    (setq cur (car font-name)
		  font-name (cdr font-name))
	    (apply 'set-face-property face (car cur) (cdr cur) args))))
       (font-running-xemacs
	(apply 'set-face-font face font-name args)
	(apply 'set-face-underline-p face (font-underline-p font) args)
	(if (and (or (font-smallcaps-p font) (font-bigcaps-p font))
		 (fboundp 'set-face-display-table))
	    (apply 'set-face-display-table
		   face font-caps-display-table args))
	(apply 'set-face-property face 'strikethru (or
						    (font-linethrough-p font)
						    (font-strikethru-p font))
	       args))
       (t
	(condition-case nil
	    (apply 'set-face-font face font-name args)
	  (error
	   (let ((args (car-safe args)))
	     (and (or (font-bold-p font)
		      (memq (font-weight font) '(:bold :demi-bold)))
		  (make-face-bold face args t))
	     (and (font-italic-p font) (make-face-italic face args t)))))
	(apply 'set-face-underline-p face (font-underline-p font) args)))))
   (t
    ;; Let the original set-face-font signal any errors
    (set-face-property face 'font-specification nil)
    (apply 'set-face-font face font args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now for emacsen specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-update-device-fonts (device)
  ;; Update all faces that were created with the 'font' package
  ;; to appear correctly on the new device.  This should be in the
  ;; create-device-hook.  This is XEmacs 19.12+ specific
  (let ((faces (face-list 2))
	(cur nil)
	;(font nil)
	(font-spec nil))
    (while faces
      (setq cur (car faces)
	    faces (cdr faces)
	    font-spec (face-property cur 'font-specification))
      (if font-spec
	  (set-face-font cur font-spec device)))))

(defun font-update-one-face (face &optional device-list)
  ;; Update FACE on all devices in DEVICE-LIST
  ;; DEVICE_LIST defaults to a list of all active devices
  (setq device-list (or device-list (device-list)))
  (if (devicep device-list)
      (setq device-list (list device-list)))
  (let* ((cur-device nil)
	 (font-spec (face-property face 'font-specification))
	 ;(font nil))
	 )
    (if (not font-spec)
	;; Hey!  Don't mess with fonts we didn't create in the
	;; first place.
	nil
      (while device-list
	(setq cur-device (car device-list)
	      device-list (cdr device-list))
	(if (not (device-live-p cur-device))
	    ;; Whoah!
	    nil
	  (if font-spec
	      (set-face-font face font-spec cur-device)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various color related things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((fboundp 'display-warning)
  (fset 'font-warn 'display-warning))
 ((fboundp 'w3-warn)
  (fset 'font-warn 'w3-warn))
 ((fboundp 'url-warn)
  (fset 'font-warn 'url-warn))
 ((fboundp 'warn)
  (defun font-warn (class message &optional level)
    (warn "(%s/%s) %s" class (or level 'warning) message)))
 (t
  (defun font-warn (class message &optional level)
    (save-excursion
      (set-buffer (get-buffer-create "*W3-WARNINGS*"))
      (goto-char (point-max))
      (save-excursion
	(insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
      (display-buffer (current-buffer))))))

(require 'x-color)



(defun font-normalize-color (color &optional device)
  "Return an RGB tuple, given any form of input.  If an error occurs, black
is returned."
  (case (device-type device)
   ((x tty)
    (apply 'format "#%02x%02x%02x" (font-color-rgb-components color)))
   (otherwise
    color)))

(defun font-set-face-background (&optional face color &rest args)
  (interactive)
  (condition-case nil
      (cond
       ((or (font-rgb-color-p color)
	    (string-match "^#[0-9a-fA-F]+$" color))
	(apply 'set-face-background face
	       (font-normalize-color color) args))
       (t
	(apply 'set-face-background face color args)))
    (error nil)))

(defun font-set-face-foreground (&optional face color &rest args)
  (interactive)
  (condition-case nil
      (cond
       ((or (font-rgb-color-p color)
	    (string-match "^#[0-9a-fA-F]+$" color))
	(apply 'set-face-foreground face (font-normalize-color color) args))
       (t
	(apply 'set-face-foreground face color args)))
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for 'blinking' fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-map-windows (func &optional arg frame)
  (let* ((start (selected-window))
	 (cur start)
	 (result nil))
    (push (funcall func start arg) result)
    (while (not (eq start (setq cur (next-window cur))))
      (push (funcall func cur arg) result))
    result))

(defun font-face-visible-in-window-p (window face)
  (let ((st (window-start window))
	(nd (window-end window))
	(found nil)
	(face-at nil))
    (setq face-at (get-text-property st 'face (window-buffer window)))
    (if (or (eq face face-at) (and (listp face-at) (memq face face-at)))
	(setq found t))
    (while (and (not found)
		(/= nd
		    (setq st (next-single-property-change
			      st 'face
			      (window-buffer window) nd))))
      (setq face-at (get-text-property st 'face (window-buffer window)))
      (if (or (eq face face-at) (and (listp face-at) (memq face face-at)))
	  (setq found t)))
    found))

(defun font-blink-callback ()
  ;; Optimized to never invert the face unless one of the visible windows
  ;; is showing it.
  (let ((faces (if font-running-xemacs (face-list t) (face-list)))
	(obj nil))
    (while faces
      (if (and (setq obj (face-property (car faces) 'font-specification))
	       (font-blink-p obj)
	       (memq t
		     (font-map-windows 'font-face-visible-in-window-p (car faces))))
	  (invert-face (car faces)))
      (pop faces))))

(defcustom font-blink-interval 0.5
  "How often to blink faces"
  :type 'number
  :group 'faces)

(defun font-blink-initialize ()
  (cond
   ((featurep 'itimer)
    (if (get-itimer "font-blinker")
	(delete-itimer (get-itimer "font-blinker")))
    (start-itimer "font-blinker" 'font-blink-callback
		  font-blink-interval
		  font-blink-interval))
   ((fboundp 'run-at-time)
    (cancel-function-timers 'font-blink-callback)
    (run-at-time font-blink-interval
		 font-blink-interval
		 'font-blink-callback))
   (t nil)))

(provide 'font)
