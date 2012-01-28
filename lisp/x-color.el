;;; x-color.el --- X11 color definition support for SXEmacs

;; Copyright (C) 2007 Nelson Ferreira

;; Author: Nelson Ferreira
;; Created: 12-May-07
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

(defun x-color-list ()
  "Color list"
  (unless (and (skiplistp x-color-slist)
	       (> (skiplist-size x-color-slist) 0))
    (x-color-read-system-colors))
  (let ((res))
    (map-skiplist #'(lambda (key val)
		      (setq res (nconc res (list (format "%s" key)))))
		  x-color-slist)
    res))


(defun find-color-rgb (name &optional nearest)
  "Retrieve the color by NAME"
  (interactive)
  (x-color-rgb-components name))

(defun x-find-color-rgb (name &optional nearest)
  "Retrieve the color by NAME"
  (unless (or (symbolp name)
	      (stringp name)
	      (x-rgb-color-p name))
    (error 'wrong-type-argument name))
  (unless (and (skiplistp x-color-slist)
	       (> (skiplist-size x-color-slist) 0))
    (x-color-read-system-colors))
  (if (x-rgb-color-p name)
      (list (x-rgb-color-red name)
	    (x-rgb-color-green name)
	    (x-rgb-color-blue name))
    (let* ((color-name (if (symbolp name)
			   (symbol-name name)
			 name))
	   (color-sym (intern color-name))
	   (color-lc-sym (intern (downcase color-name)))
	   (color-ns-sym (intern (replace-in-string color-name " " "")))
	   (color-lcns-sym (intern (replace-in-string
				    (downcase color-name)
				    " " ""))))
      (or (get-skiplist x-color-slist color-sym)
	  (get-skiplist x-color-slist color-lc-sym)
	  (get-skiplist x-color-slist color-ns-sym)
	  (get-skiplist x-color-slist color-lcns-sym)))))



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
	   (eq (aref obj 0) 'rgb))
      (and (vectorp obj)
	   (= (length obj) 3))
      (and (listp obj)
	   (= (length obj) 3))
      (and (listp obj)
	   (= (length obj) 4)
	   (eq (nth 0 obj) 'rgb))))

(defsubst x-rgb-color-nth (n obj)
  (or (and (vectorp obj)
	   (= (length obj) 4)
	   (eq (aref obj 0) 'rgb)
	   (aref obj n))
      (and (vectorp obj)
	   (= (length obj) 3)
	   (aref obj (1- n)))
      (and (listp obj)
	   (= (length obj) 3)
	   (nth (1- n) obj))
      (and (listp obj)
	   (= (length obj) 4)
	   (eq (nth 0 obj) 'rgb)
	   (nth n obj))))


(defsubst x-rgb-color-red (obj)   (x-rgb-color-nth 1 obj))
(defsubst x-rgb-color-green (obj) (x-rgb-color-nth 2 obj))
(defsubst x-rgb-color-blue (obj)  (x-rgb-color-nth 3 obj))

(defun x-color-rgb-components (color)
  "Return the RGB components of COLOR as a list of integers (R G B).
16-bit values are always returned.
#FEFEFE and rgb:fe/fe/fe style color specifications are parsed directly
into their components.
RGB values for color names are looked up using 'x-find-color-rgb'."
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
	  ((and color-rgb
		(eval (append '(and)
			      (mapcar #'(lambda (c)
					  (and (numberp c) (<= 0 c 255)))
				      color))))
	   (mapcar #'(lambda (c) (lsh c 8)) color))
	  ((and color-rgb
		(eval (append '(or)
			      (mapcar #'(lambda (c)
					  (and (numberp c) (<= 0 c 65535)))
				      color))))
	   color)
	  ((and (stringp color)
		(or (string-match #r"^\(#\|rgb:\)" color)
		    (string-match #r"[0-9.]+\s-+[0-9.]+\s-+[0-9.]+"
				  color)))
	   (x-color-parse-rgb-components color))
	  (t
	   (x-find-color-rgb color)))))

(defun x-read-rgb-file (filename)
  "Read the colors from FILENAME. The file is expected to have the same
format as X11 rgb.txt"
  (let ((rgb-regex
	 #r"^\s-*\([0-9]+\)\s-+\([0-9]+\)\s-+\([0-9]+\)\s-+\([a-zA-Z0-9 ]+\)\s-*$"))
    (unless (skiplistp x-color-slist)
      (setq x-color-slist (make-skiplist)))
    (when (file-readable-p filename)
      (save-excursion
	(set-buffer (or (get-file-buffer filename)
			(create-file-buffer filename)))
	(erase-buffer)
	(insert-file-contents-literally filename)
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
					(intern name)
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


(defun x-color-off-gray-diag (r g b)
  "Compute the angle between the color given by R,G,B and the gray diagonal.
The gray diagonal is the diagonal of the 3D cube in RGB space which
connects the points corresponding to the black and white colors.  All the
colors whose RGB coordinates belong to this diagonal are various shades
of gray, thus the name."
  (let ((mag (sqrt (* 3 (+ (* r r) (* g g) (* b b))))))
    (if (< mag 1) 0 (acos (/ (+ r g b) mag)))))


(defsubst x-rgb-to-8bits (rgb)
  "Convert a 16-bit components rgb to an 8-bit components one."
  (mapcar #'(lambda (c) (lsh c -8)) rgb))

(defsubst x-color-distance-2 (color1 color2)
  "Return the color cube distance between the two colors as 8 bit rgb tupples."
  (when (and color1 color2)
    (unless (and (listp color1)
		 (= (length color1) 3))
      (error 'wrong-type-argument color1))
    (unless (and (listp color2)
		 (= (length color2) 3))
      (error 'wrong-type-argument color2))
    (let ((dR (- (car color1) (car color2)))
	  (dG (- (cadr color1) (cadr color2)))
	  (dB (- (caddr color1) (caddr color2))))
      (+ (* dR dR) (* dG dG) (* dB dB)))))


(defsubst x-color-distance-1 (color1 color2)
  "Return the color cube distance between the two colors.
Assumes COLOR1 is an 8 bit rgb tupple. "
;; Verification is done in x-color-distance-2
  (x-color-distance-2 color1
		      (x-rgb-to-8bits (x-color-rgb-components color2))))


(defun x-color-distance (color1 color2)
  "Return the color cube distance between the two colors."
    (x-color-distance-1 (x-rgb-to-8bits (x-color-rgb-components color1))
			color2))


(defvar x-nearest-color-favor-non-gray-threshold 0.065
  "If the approximated color is not close enough to the
gray diagonal of the RGB cube, favor non-gray colors.
The default number 0.065 is an empirical ad-hoc'ery")

(defun x-nearest-color (color &optional colorlist) "
Return the nearest COLOR in COLORLIST.
COLOR can be a color name, an '(r g b) tuple or a color specification.
#FEFEFE and rgb:fe/fe/fe style specifications are parsed.
COLORLIST is a list of colors in the same acceptable formats as COLOR.
Returns NIL if color specification is invalid, or no colors
close enough are found."
  (let (color-rgb)
    (when (or (stringp color) (symbolp color))
      (setq color-rgb (x-rgb-to-8bits (find-color-rgb color))))
    (when (not color-rgb)
      (error 'invalid-argument color))
    (let ((favor-non-gray (>= (apply 'x-color-off-gray-diag color-rgb)
			      x-nearest-color-favor-non-gray-threshold))
	  (best-distance 195076)	;; Max possible distance: 3 * 255^2 + 15
	  best-color)
      (mapc
       #'(lambda (candidate)
	   (when candidate
	     (let* ((cand-rgb (find-color-rgb candidate))
		    (distance (x-color-distance color-rgb cand-rgb)))
	       (if (and distance cand-rgb
			(< distance best-distance)
			;; The candidate color is on the gray diagonal
			;; if its RGB components are all equal.
			(or (/= (x-rgb-color-red cand-rgb)
				(x-rgb-color-green cand-rgb))
			    (/= (x-rgb-color-green cand-rgb)
				(x-rgb-color-blue cand-rgb))
			    (not favor-non-gray)))
		   (setq best-distance distance
			 best-color candidate)))))
       (or colorlist (x-color-list)))
      best-color)))

(provide 'x-color)

;;; x-color.el ends here
