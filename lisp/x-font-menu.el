;; x-font-menu.el --- Managing menus of X fonts.

;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1997 Sun Microsystems

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Restructured by: Jonathan Stigelman <Stig@hackvan.com>
;; Mule-ized by: Martin Buchholz
;; More restructuring for MS-Windows by Andy Piper <andy@xemacs.org>

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
;;; Code:

;; #### - implement these...
;;
;;; (defvar font-menu-ignore-proportional-fonts nil
;;;   "*If non-nil, then the font menu will only show fixed-width fonts.")

(require 'font-menu)

(defvar x-font-menu-registry-encoding nil
  "Registry and encoding to use with font menu fonts.")

(defvar x-fonts-menu-junk-families
  (mapconcat
   #'identity
   '("cursor" "glyph" "symbol"	; Obvious losers.
     #r"\`Ax...\'"		; FrameMaker fonts - there are just way too
				;  many of these, and there is a different
				;  font family for each font face!  Losers.
				;  "Axcor" -> "Applix Courier Roman",
				;  "Axcob" -> "Applix Courier Bold", etc.
     )
   "\\|")
  "A regexp matching font families which are uninteresting (e.g. cursor fonts).")

(defun hack-font-truename (fn)
  "Filter the output of `font-instance-truename' to deal with Japanese fontsets."
  (if (string-match "," (font-instance-truename fn))
      (let ((fpnt (nth 8 (split-string (font-instance-name fn) "-")))
	    (flist (split-string (font-instance-truename fn) ","))
	    ret)
	(while flist
	  (if (string-equal fpnt (nth 8 (split-string (car flist) "-")))
	      (progn (setq ret (car flist)) (setq flist nil))
	    (setq flist (cdr flist))
	    ))
	ret)
    (font-instance-truename fn)))

(defvar x-font-regexp-ascii nil
  "This is used to filter out font families that can't display ASCII text.
It must be set at run-time.")

;;;###autoload
(defun x-reset-device-font-menus (device &optional debug)
  "Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system,
or if you change your font path, you can call this to re-initialize the menus."
  ;; by Stig@hackvan.com
  ;; #### - this should implement a `menus-only' option, which would
  ;; recalculate the menus from the cache w/o having to do list-fonts again.
  (unless x-font-regexp-ascii
    (setq x-font-regexp-ascii (if (featurep 'mule)
				  (charset-registry 'ascii)
				"iso8859-1")))
  (setq x-font-menu-registry-encoding
	(if (featurep 'mule) "*-*" "iso8859-1"))
  (let ((case-fold-search t)
	family size weight entry monospaced-p
	dev-cache cache families sizes weights)
    (dolist (name (cond ((null debug)	; debugging kludge
			 (list-fonts "*-*-*-*-*-*-*-*-*-*-*-*-*-*" device))
			((stringp debug) (split-string debug "\n"))
			(t debug)))
      (when (and (string-match x-font-regexp-ascii name)
		 (string-match x-font-regexp name))
	(setq weight (capitalize (match-string 1 name))
	      size   (string-to-int (match-string 6 name)))
	(or (string-match x-font-regexp-foundry-and-family name)
	    (error "internal error"))
	(setq family (capitalize (match-string 1 name)))
	(or (string-match x-font-regexp-spacing name)
	    (error "internal error"))
	(setq monospaced-p (string= "m" (match-string 1 name)))
	(unless (string-match x-fonts-menu-junk-families family)
	  (setq entry (or (vassoc family cache)
			  (car (setq cache
				     (cons (vector family nil nil t)
					   cache)))))
	  (or (member family families) (push family families))
	  (or (member weight weights)  (push weight weights))
	  (or (member size   sizes)    (push size   sizes))
	  (or (member weight (aref entry 1)) (push weight (aref entry 1)))
	  (or (member size   (aref entry 2)) (push size   (aref entry 2)))
	  (aset entry 3 (and (aref entry 3) monospaced-p)))))
    ;;
    ;; Hack scalable fonts.
    ;; Some fonts come only in scalable versions (the only size is 0)
    ;; and some fonts come in both scalable and non-scalable versions
    ;; (one size is 0).  If there are any scalable fonts at all, make
    ;; sure that the union of all point sizes contains at least some
    ;; common sizes - it's possible that some sensible sizes might end
    ;; up not getting mentioned explicitly.
    ;;
    (if (member 0 sizes)
	(let ((common '(60 80 100 120 140 160 180 240)))
	  (while common
	    (or;;(member (car common) sizes)   ; not enough slack
	     (let ((rest sizes)
		   (done nil))
	       (while (and (not done) rest)
		 (if (and (> (car common) (- (car rest) 5))
			  (< (car common) (+ (car rest) 5)))
		     (setq done t))
		 (setq rest (cdr rest)))
	       done)
	     (setq sizes (cons (car common) sizes)))
	    (setq common (cdr common)))
	  (setq sizes (delq 0 sizes))))

    (setq families (sort families 'string-lessp)
	  weights  (sort weights 'string-lessp)
	  sizes    (sort sizes '<))

    (dolist (entry cache)
      (aset entry 1 (sort (aref entry 1) 'string-lessp))
      (aset entry 2 (sort (aref entry 2) '<)))

    (setq dev-cache (assq device device-fonts-cache))
    (or dev-cache
	(setq dev-cache (car (push (list device) device-fonts-cache))))
    (setcdr
     dev-cache
     (vector
      cache
      (mapcar (lambda (x)
		(vector x
			(list 'font-menu-set-font x nil nil)
			':style 'radio ':active nil ':selected nil))
	      families)
      (mapcar (lambda (x)
		(vector (if (/= 0 (% x 10))
			    ;; works with no LISP_FLOAT_TYPE
			    (concat (int-to-string (/ x 10)) "."
				    (int-to-string (% x 10)))
			  (int-to-string (/ x 10)))
			(list 'font-menu-set-font nil nil x)
			':style 'radio ':active nil ':selected nil))
	      sizes)
      (mapcar (lambda (x)
		(vector x
			(list 'font-menu-set-font nil x nil)
			':style 'radio ':active nil ':selected nil))
	      weights)))
    (cdr dev-cache)))

;; Extract font information from a face.  We examine both the
;; user-specified font name and the canonical (`true') font name.
;; These can appear to have totally different properties.
;; For examples, see the prolog above.

;; We use the user-specified one if possible, else use the truename.
;; If the user didn't specify one (with "-dt-*-*", for example)
;; get the truename and use the possibly suboptimal data from that.
;;;###autoload
(defun* x-font-menu-font-data (face dcache)
  (defvar x-font-regexp)
  (defvar x-font-regexp-foundry-and-family)
  (let* ((case-fold-search t)
	 (domain (if font-menu-this-frame-only-p
				  (selected-frame)
				(selected-device)))
	 (name (font-instance-name (face-font-instance face domain)))
	 (truename (font-instance-truename
		    (face-font-instance face domain
					(if (featurep 'mule) 'ascii))))
	 family size weight entry slant)
    (when (string-match x-font-regexp-foundry-and-family name)
      (setq family (capitalize (match-string 1 name)))
      (setq entry (vassoc family (aref dcache 0))))
    (when (and (null entry)
	       (string-match x-font-regexp-foundry-and-family truename))
      (setq family (capitalize (match-string 1 truename)))
      (setq entry  (vassoc family (aref dcache 0))))
    (when (null entry)
      (return-from x-font-menu-font-data (make-vector 5 nil)))

    (when (string-match x-font-regexp name)
      (setq weight (capitalize    (match-string 1 name)))
      (setq size   (string-to-int (match-string 6 name))))

    (when (string-match x-font-regexp truename)
      (when (not (member weight (aref entry 1)))
	(setq weight (capitalize (match-string 1 truename))))
      (when (not (member size   (aref entry 2)))
	(setq size (string-to-int (match-string 6 truename))))
      (setq slant (capitalize (match-string 2 truename))))

    (vector entry family size weight slant)))

(defun x-font-menu-load-font (family weight size slant resolution)
  "Try to load a font with the requested properties.
The weight, slant and resolution are only hints."
  (when (integerp size) (setq size (int-to-string size)))
  (let (font)
    (catch 'got-font
      (dolist (weight (list weight "*"))
	(dolist (slant
		 (cond ((string-equal slant "O") '("O" "I" "*"))
		       ((string-equal slant "I") '("I" "O" "*"))
		       ((string-equal slant "*") '("*"))
		       (t (list slant "*"))))
	  (dolist (resolution
		   (if (string-equal resolution "*-*")
		       (list resolution)
		     (list resolution "*-*")))
	    (when (setq font
			(make-font-instance
			 (concat  "-*-" family "-" weight "-" slant "-*-*-*-"
				  size "-" resolution "-*-*-"
				  x-font-menu-registry-encoding)
			 nil t))
	      (throw 'got-font font))))))))

(provide 'x-font-menu)

;;; x-font-menu.el ends here
