;;; mule-x-init.el --- initialization code for X Windows under MULE
;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing <ben@xemacs.org>

;; Author: various
;; Keywords: mule X11

;; This file is part of SXEmacs.
;;
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

;;; Commentary:

;;; Code:

;;; Work around what is arguably a Sun CDE bug.

(defun x-use-halfwidth-roman-font (fullwidth-charset roman-registry)
  "Maybe set charset registry of the 'ascii charset to ROMAN-REGISTRY.

Do this only if:
 - the current display is an X device
 - the displayed width of FULLWIDTH-CHARSET is twice the displayed
   width of the 'ascii charset, but only when using ROMAN-REGISTRY.

Traditionally, Asian characters have been displayed so that they
occupy exactly twice the screen space of ASCII (`halfwidth')
characters.  On many systems, e.g. Sun CDE systems, this can only be
achieved by using a national variant roman font to display ASCII."
  (let* ((charset-font-width
	  (lambda (charset)
	    (font-instance-width
	     (face-font-instance 'default (selected-device) charset))))

	 (twice-as-wide
	  (lambda (cs1 cs2)
	    (let ((width1 (funcall charset-font-width cs1))
		  (width2 (funcall charset-font-width cs2)))
	      (and width1 width2 (eq (+ width1 width1) width2))))))

    (when (eq 'x (device-type))
      (condition-case nil
	  (unless (funcall twice-as-wide 'ascii fullwidth-charset)
	    (set-charset-registry 'ascii roman-registry)
	    (unless (funcall twice-as-wide 'ascii fullwidth-charset)
	      ;; Restore if roman-registry didn't help
	      (set-charset-registry 'ascii "iso8859-1")))
	(error (set-charset-registry 'ascii "iso8859-1"))))))

;;;;

(defvar mule-x-win-initted nil)

(defun init-mule-x-win ()
  "Initialize X Windows for MULE at startup.  Don't call this."
  (when (not mule-x-win-initted)
    (define-specifier-tag 'mule-fonts
      (lambda (device) (eq 'x (device-type device))))

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
       "-*-fixed-medium-r-*--*-tis620.2529-1"   ; Thai
       )
     'global '(mule-fonts) 'append)

    (setq mule-x-win-initted t)))
