;;; disp-table.el --- functions for dealing with char tables.

;; Copyright (C) 1987, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Author: Howard Gayle
;; Maintainer: SXEmacs Development Team
;; Keywords: i18n, internal

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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; #### Need lots of work.  make-display-table depends on a value
;; that is a define in the C code.  Maybe we should just move the
;; function into C.

;; #### display-tables-as-vectors is really evil and a big pain in
;; the ass.

;; Rewritten for XEmacs July 1995, Ben Wing.


;;; Code:

(defun describe-display-table (dt)
  "Describe the display table DT in a help buffer."
  (with-displaying-help-buffer
   (lambda ()
     (princ "\nCharacter display glyph sequences:\n")
     (save-excursion
       (let ((vector (make-vector 256 nil))
	     (i 0))
	 (while (< i 256)
	   (aset vector i (aref dt i))
	   (incf i))
	 ;; FSF calls `describe-vector' here, but it is so incredibly
	 ;; lame a function for that name that I cannot bring myself
	 ;; to porting it.  Here is what `describe-vector' does:
	 (terpri)
	 (let ((old (aref vector 0))
	       (oldpos 0)
	       (i 1)
	       str)
	   (while (<= i 256)
	     (when (or (= i 256)
		       (not (equal old (aref vector i))))
	       (if (eq oldpos (1- i))
		   (princ (format "%s\t\t%s\n"
				  (single-key-description (int-char oldpos))
				  old))
		 (setq str (format "%s - %s"
				   (single-key-description (int-char oldpos))
				   (single-key-description (int-char (1- i)))))
		 (princ str)
		 (princ (make-string (max (- 2 (/ (length str)
						  tab-width)) 1) ?\t))
		 (princ old)
		 (terpri))
	       (or (= i 256)
		   (setq old (aref vector i)
			 oldpos i)))
	     (incf i))))))))

;;;###autoload
(defun describe-current-display-table (&optional domain)
  "Describe the display table in use in the selected window and buffer."
  (interactive)
  (or domain (setq domain (selected-window)))
  (let ((disptab (specifier-instance current-display-table domain)))
    (if disptab
	(describe-display-table disptab)
      (message "No display table"))))

;;;###autoload
(defun make-display-table ()
  "Return a new, empty display table."
  (make-vector 256 nil))

;; #### we need a generic frob-specifier function.
;; #### this also needs to be redone like frob-face-property.

;; Let me say one more time how much dynamic scoping sucks.

(defun frob-display-table (fdt-function fdt-locale)
  (or fdt-locale (setq fdt-locale 'global))
  (or (specifier-spec-list current-display-table fdt-locale)
      (add-spec-to-specifier current-display-table (make-display-table)
			     fdt-locale))
  (add-spec-list-to-specifier
   current-display-table
   (list (cons fdt-locale
	       (mapcar
		(lambda (fdt-x)
		  (funcall fdt-function (cdr fdt-x))
		  fdt-x)
		(cdar (specifier-spec-list current-display-table
					   fdt-locale)))))))

(defun standard-display-8bit-1 (dt l h)
  (while (<= l h)
    (aset dt l (char-to-string l))
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-8bit (l h &optional locale)
  "Display characters in the range L to H literally."
  (frob-display-table
   (lambda (x)
     (standard-display-8bit-1 x l h))
   locale))

(defun standard-display-default-1 (dt l h)
  (while (<= l h)
    (aset dt l nil)
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-default (l h &optional locale)
  "Display characters in the range L to H using the default notation."
  (frob-display-table
   (lambda (x)
     (standard-display-default-1 x l h))
   locale))

;;;###autoload
(defun standard-display-ascii (c s &optional locale)
  "Display character C using printable string S."
  (frob-display-table
   (lambda (x)
     (aset x c s))
   locale))


;;; #### should frob in a 'tty locale.

;;;###autoload
(defun standard-display-g1 (c sc &optional locale)
  "Display character C as character SC in the g1 character set.
This function assumes that your terminal uses the SO/SI characters;
it is meaningless for an X frame."
  (frob-display-table
   (lambda (x)
     (aset x c (concat "\016" (char-to-string sc) "\017")))
   locale))


;;; #### should frob in a 'tty locale.

;;;###autoload
(defun standard-display-graphic (c gc &optional locale)
  "Display character C as character GC in graphics character set.
This function assumes VT100-compatible escapes; it is meaningless for an
X frame."
  (frob-display-table
   (lambda (x)
     (aset x c (concat "\e(0" (char-to-string gc) "\e(B")))
   locale))

;;; #### should frob in a 'tty locale.
;;; #### the FSF equivalent of this makes this character be displayed
;;; in the 'underline face.  There's no current way to do this with
;;; XEmacs display tables.

;;;###autoload
(defun standard-display-underline (c uc &optional locale)
  "Display character C as character UC plus underlining."
  (frob-display-table
   (lambda (x)
     (aset x c (concat "\e[4m" (char-to-string uc) "\e[m")))
   locale))

;;;###autoload
(defun standard-display-european (arg &optional locale)
  "Toggle display of European characters encoded with ISO 8859.
When enabled, characters in the range of 160 to 255 display not
as octal escapes, but as accented characters.
With prefix argument, enable European character display iff arg is positive."
  (interactive "P")
  (frob-display-table
   (lambda (x)
     (if (or (<= (prefix-numeric-value arg) 0)
	     (and (null arg)
		  (equal (aref x 160) (char-to-string 160))))
	 (standard-display-default-1 x 160 255)
       (standard-display-8bit-1 x 160 255)))
   locale))

(provide 'disp-table)

;;; disp-table.el ends here
