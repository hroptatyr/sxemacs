;; msw-font-menu.el --- Managing menus of mswindows fonts.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Adapted from x-font-menu.el by Andy Piper <andy@xemacs.org>

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

;;; mswindows fonts look like:
;;;	fontname[:[weight][ style][:pointsize[:effects]]][:charset]
;;; ie:
;;;	Lucida Console:Regular:10
;;; minimal:
;;;	Courier New
;;; maximal:
;;;	Courier New:Bold Italic:10:underline strikeout:western

;;; Code:

;; #### - implement these...
;;
;;; (defvar font-menu-ignore-proportional-fonts nil
;;;   "*If non-nil, then the font menu will only show fixed-width fonts.")

(require 'font-menu)

(defvar mswindows-font-menu-registry-encoding nil
  "Registry and encoding to use with font menu fonts.")

(defvar mswindows-font-menu-junk-families
  (mapconcat
   #'identity
   '("Symbol" 
     )
   "\\|")
  "A regexp matching font families which are uninteresting (e.g. cursor fonts).")

(defvar mswindows-font-regexp-ascii nil
  "This is used to filter out font families that can't display ASCII text.
It must be set at run-time.")

;;;###autoload
(defun mswindows-reset-device-font-menus (device &optional debug)
  "Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system, 
or if you change your font path, you can call this to re-initialize the menus."
  (unless mswindows-font-regexp-ascii
    (setq mswindows-font-regexp-ascii (if (featurep 'mule)
					  (charset-registry 'ascii)
					"Western")))
  (setq mswindows-font-menu-registry-encoding (if (featurep 'mule) "" "Western"))
  (let ((case-fold-search t)
	family size weight entry
	dev-cache cache families sizes weights)
    (dolist (name (cond ((null debug)	; debugging kludge
			 (list-fonts "::::" device))
			((stringp debug) (split-string debug "\n"))
			(t debug)))
      (when (and (string-match mswindows-font-regexp-ascii name)
		 (string-match mswindows-font-regexp name))
	(setq weight (capitalize (match-string 2 name))
	      size   (string-to-int (or (match-string 4 name) "0"))
	      family (match-string 1 name))
	(unless (string-match mswindows-font-menu-junk-families family)
	  (setq entry (or (vassoc name cache)
			  (car (setq cache
				     (cons (vector family nil nil t)
					   cache)))))
	  (or (member family families) (push family families))
	  (or (member weight weights)  (push weight weights))
	  (or (member size   sizes)    (push size   sizes))
	  (or (member weight (aref entry 1)) (push weight (aref entry 1)))
	  (or (member size   (aref entry 2)) (push size   (aref entry 2))))))
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
	  (let ((common '(6 8 10 12 14 16 18 24)))
	    (while common
	      (or;;(member (car common) sizes)   ; not enough slack
	       (let ((rest sizes)
		     (done nil))
		 (while (and (not done) rest)
		   (if (and (> (car common) (- (car rest) 1))
			    (< (car common) (+ (car rest) 1)))
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
		  (vector (int-to-string x)
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

;; We use the user-specified one if possible, else use the truename.
;; If the user didn't specify one get the truename and use the
;; possibly suboptimal data from that.
;;;###autoload
(defun* mswindows-font-menu-font-data (face dcache)
  (let* ((case-fold-search t)
	 (domain (if font-menu-this-frame-only-p
				  (selected-frame)
				(selected-device)))
	 (name (font-instance-name (face-font-instance face domain)))
	 (truename (font-instance-truename
		    (face-font-instance face domain
					(if (featurep 'mule) 'ascii))))
	 family size weight entry slant)
    (when (string-match mswindows-font-regexp name)
      (setq family (match-string 1 name))
      (setq entry (vassoc family (aref dcache 0))))
    (when (and (null entry)
	       (string-match mswindows-font-regexp truename))
      (setq family (match-string 1 truename))
      (setq entry  (vassoc family (aref dcache 0))))
    (when (null entry)
      (return-from mswindows-font-menu-font-data (make-vector 5 nil)))
    
    (when (string-match mswindows-font-regexp name)
      (setq weight (match-string 2 name))
      (setq size   (string-to-int (or (match-string 4 name) "0"))))
      
    (when (string-match mswindows-font-regexp truename)
      (when (not (member weight (aref entry 1)))
	(setq weight (match-string 2 truename)))
      (when (not (member size   (aref entry 2)))
	(setq size (string-to-int (or (match-string 4 truename) "0"))))
      (setq slant (match-string 5 truename)))
      
    (vector entry family size weight slant)))

(defun mswindows-font-menu-load-font (family weight size slant resolution)
  "Try to load a font with the requested properties.
The weight, slant and resolution are only hints."
  (when (integerp size) (setq size (int-to-string size)))
  (let (font)
    (catch 'got-font
      (dolist (weight (list weight ""))
	(dolist (slant
		 ;; oblique is not currently implemented
		 (cond ((string-equal slant "Oblique") '(" Italic" ""))
		       ((string-equal slant "Italic") '(" Italic" ""))
		       (t (list slant ""))))
	  (when (setq font
		      (make-font-instance
		       (concat  family ":" weight slant ":"
				size "::"
				mswindows-font-menu-registry-encoding)
		       nil t))
	    (throw 'got-font font)))))))

(provide 'mswindows-font-menu)

;;; msw-font-menu.el ends here
