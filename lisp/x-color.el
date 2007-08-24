;;; x-color.el --- X11 color definition support for SXEmacs

;; Copyright (C) 2007 Nelson Ferreira

;; Author: Nelson Ferreira
;; Created: 12-May-07
;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

;; This file is part of SXEmacs.

;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: all the minibuffer history stuff is synched with
;;; 19.30.  Not sure about the rest.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; Code:

(defvar x-library-search-path '("/usr/X11R7/lib/X11/"
				"/usr/X11R6/lib/X11/"
				"/usr/X11R5/lib/X11/"
				"/usr/lib/X11R7/X11/"
				"/usr/lib/X11R6/X11/"
				"/usr/lib/X11R5/X11/"
				"/usr/local/X11R7/lib/X11/"
				"/usr/local/X11R6/lib/X11/"
				"/usr/local/X11R5/lib/X11/"
				"/usr/local/lib/X11R7/X11/"
				"/usr/local/lib/X11R6/X11/"
				"/usr/local/lib/X11R5/X11/")
  "Search path used by `read-color' to find rgb.txt.")

(defvar x-color-slist nil "Map of color names and their RGB values")

(defun x-read-color-completion-table ()
  "Color table for interactive completion"
  (unless (and (skiplistp x-color-slist) 
	       (> (skiplist-size x-color-slist) 0))
    (x-color-read-system-colors))
  (let ((res))
    (map-skiplist #'(lambda (key val) 
		      (setq res (nconc res (list (list (format "%s" key)))))) 
		  x-color-slist)
    res))
  
(defun find-color-rgb (name)
  "Retrieve the color by name"
  (unless (or (symbolp name) (stringp name))
    (error 'wrong-type-argument name))
  (unless (and (skiplistp x-color-slist) 
	       (> (skiplist-size x-color-slist) 0))
    (x-color-read-system-colors))
  (get-skiplist x-color-slist (if (symbolp name) name (intern name))))


(defun x-color-parse-rgb-components (color)
  "Parse RGB color specification and return a list of integers (R G B).
#FEFEFE and rgb:fe/fe/fe style specifications are parsed.
Returns NIL if RGB color specification is invalid."
  (let ((case-fold-search t) 
	matches)
    (if (string-match #r"\([0-9.]+\)\s-+\([0-9.]+\)\s-+\([0-9.]+\)" 
		      color)
	;; recurse and parse hexadecimal color
	(x-color-parse-rgb-components 
	 (apply 'format "#%02X%02X%02X" 
		(mapcar #'(lambda (c) (if (floatp c) c (* 255 c)))
			(mapcar #'(lambda (i)
				    (let ((m 
					   (string-to-number 
					    (match-string i color))))
				      (if (<= 0 m 1)
					  (* 255 m)
					m)))
				(list 1 2 3)))))
      (when (cond ((string-match "^#[0-9a-f]+$" color)
		   (let* ((size (/ (1- (length color)) 3))
			  (dig-regex (format #r"\([0-9a-f]\{%s,%s\}\)"
					     size size)))
		     ;; Check the intege division had no remainder
		     ;; which means no "odd" component sizes
		     (when (= (1+ (* 3 size)) (length color))
		       (string-match
			(concat "^#" dig-regex dig-regex dig-regex "$")
			color))))
		  ((string-match #r"rgb:\([0-9a-f]+\)/\([0-9a-f]+\)/\([0-9a-f]+\)"
				 color))))
      (setq matches (mapcar #'(lambda (i) (match-string i color))
			    '(1 2 3)))
      ;; Make sure all components have at most 4 hex digits
      (when (eval 
	     (append '(and)
		     (mapcar #'(lambda (component)
				 (> 5 (length component) 0))
			     matches)))
	(mapcar #'(lambda (component)
		    (* (expt 16 (- 4 (length component)))
		       (string-to-number component 16)))
		matches)))))

(defsubst x-rgb-color-p (obj)
  (or (and (vectorp obj)
	   (= (length obj) 4)
	   (eq (aref obj 0) 'rgb))))

(defsubst x-rgb-color-red (obj) (aref obj 1))
(defsubst x-rgb-color-green (obj) (aref obj 2))
(defsubst x-rgb-color-blue (obj) (aref obj 3))

(defun x-color-rgb-components (color)
  "Return the RGB components of COLOR as a list of integers (R G B).
16-bit values are always returned.
#FEFEFE and rgb:fe/fe/fe style color specifications are parsed directly
into their components.
RGB values for color names are looked up using 'find-color-rgb'."
  (let ((case-fold-search t)
	(color-rgb 
	 (cond ((x-rgb-color-p color)
		(mapcar #'(lambda (f)
			    (funcall f color))
			(list 'x-rgb-color-red
			      'x-rgb-color-green
			      'x-rgb-color-blue)))
	       ((and (vectorp color)
		     (= 3 (length color)))
		(mapcar #'(lambda (p)
			    (aref color p))
			(list 0 1 2)))
	       ((and (listp color)
		     (= 3 (length color)))
		color))))
    (cond ((and color-rgb
		(eval (append '(and) 
			      (mapcar #'(lambda (c) 
					  (and (numberp c) (<= 0 c 1)))
				      color))))
	   (mapcar #'(lambda (c) (* 65535 c)) color))
	  ((and (stringp color)
		(or (string-match #r"^\(#\|rgb:\)" color)
		    (string-match #r"[0-9.]+\s-+[0-9.]+\s-+[0-9.]+"
				  color)))
	   (if-fboundp 'x-parse-rgb-components
	       (x-parse-rgb-components color)))
	  (t
	   (find-color-rgb color)))))

(defun x-read-rgb-file (filename)
  "Read the colors from FILENAME. The file is expected to have the same
format as X11 rgb.txt"
  (let ((rgb-regex 
	 #r"^\([0-9]+\)\s-+\([0-9]+\)\s-+\([0-9]+\)\s-+\([a-zA-Z0-9 ]+\)\s-*$"))
    (unless (skiplistp x-color-slist)
      (setq x-color-slist (make-skiplist)))
    (when (file-readable-p filename)
      (save-excursion
	(set-buffer (find-file-noselect filename t))
	(if (not (= (aref (buffer-name) 0) ? ))
	    (rename-buffer (generate-new-buffer-name " *rgb-tmp-buffer*")))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (re-search-forward rgb-regex nil t)
	      (let ((rgb-matches 
		     (mapcar #'(lambda (i) 
				 (lsh (read (match-string i)) 8))
			     '(1 2 3)))
		    (color-name (match-string 4)))
		(mapc #'(lambda (name)
			  (put-skiplist x-color-slist
					(intern color-name) 
					rgb-matches))
		      (list color-name 
			    (downcase color-name)
			    (replace-in-string color-name " " "")
			    (replace-in-string (downcase color-name) 
					       " " "")))))))))))
  
(defun x-color-read-system-colors ()
  "Read the system colors"
  (when (locate-data-file "rgb.txt")
    (x-read-rgb-file (locate-data-file "rgb.txt")))
  (mapc 'x-read-rgb-file
	(mapcar #'(lambda (dir)
		    (expand-file-name "rgb.txt" dir))
		x-library-search-path))
  x-color-slist)

(provide 'x-color)

;;; x-color.el ends here
