;;; objects.el --- Lisp interface to C window-system objects

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Ben Wing

;; Author: Chuck Thompson <cthomp@xemacs.org>
;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: faces, internal, dumped

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

(defun ws-object-property-1 (function object domain &optional matchspec)
  (let ((instance (if matchspec
		      (specifier-matching-instance object matchspec domain)
		    (specifier-instance object domain))))
    (and instance (funcall function instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; font specifiers

(defun make-font-specifier (spec-list)
  "Return a new `font' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Valid instantiators for font specifiers are:

-- a string naming a font (e.g. under X this might be
   \"-*-courier-medium-r-*-*-*-140-*-*-*-*-iso8859-*\" for a 14-point
   upright medium-weight Courier font)
-- a font instance (use that instance directly if the device matches,
   or use the string that generated it)
-- a vector of no elements (only on TTY's; this means to set no font
   at all, thus using the \"natural\" font of the terminal's text)
-- a vector of one element (a face to inherit from)
"
  (make-specifier-and-init 'font spec-list))

(defun font-name (font &optional domain charset)
  "Return the name of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-name' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-name font domain charset))

(defun font-ascent (font &optional domain charset)
  "Return the ascent of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-ascent' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-ascent font domain charset))

(defun font-descent (font &optional domain charset)
  "Return the descent of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-descent' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-descent font domain charset))

(defun font-width (font &optional domain charset)
  "Return the width of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-width' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-width font domain charset))

(defun font-height (font &optional domain charset)
  "Return the height of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-height' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-height font domain charset))

(defun font-proportional-p (font &optional domain charset)
  "Return whether FONT is proportional in the specified DOMAIN, if known.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-proportional-p' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-proportional-p font domain charset))

(defun font-properties (font &optional domain charset)
  "Return the properties of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-properties'
to the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-properties font domain charset))

(defun font-truename (font &optional domain charset)
  "Return the truename of the FONT in the specified DOMAIN, if any.
FONT should be a font specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `font-instance-truename'
to the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'font-instance-truename font domain charset))

(defun font-instance-height (font-instance)
  "Return the height in pixels of FONT-INSTANCE.
The returned value is the maximum height for all characters in the font,\n\
and is equivalent to the sum of the font instance's ascent and descent."
  (+ (font-instance-ascent font-instance)
     (font-instance-descent font-instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color specifiers

(defun make-color-specifier (spec-list)
  "Return a new `color' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for a detailed description of
how specifiers work.

Valid instantiators for color specifiers are:

-- a string naming a color (e.g. under X this might be \"lightseagreen2\"
   or \"#F534B2\")
-- a color instance (use that instance directly if the device matches,
   or use the string that generated it)
-- a vector of no elements (only on TTY's; this means to set no color
   at all, thus using the \"natural\" color of the terminal's text)
-- a vector of one or two elements: a face to inherit from, and
   optionally a symbol naming which property of that face to inherit,
   either `foreground' or `background' (if omitted, defaults to the same
   property that this color specifier is used for; if this specifier is
   not part of a face, the instantiator would not be valid)."
  (make-specifier-and-init 'color spec-list))

(defun color-name (color &optional domain)
  "Return the name of the COLOR in the specified DOMAIN, if any.
COLOR should be a color specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `color-instance-name' to
the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'color-instance-name color domain))

(defun color-rgb-components (color &optional domain)
  "Return the RGB components of the COLOR in the specified DOMAIN, if any.
COLOR should be a color specifier object and DOMAIN is normally a window
and defaults to the selected window if omitted.  This is equivalent
to using `specifier-instance' and applying `color-instance-rgb-components'
to the result.  See `make-specifier' for more information about specifiers."
  (ws-object-property-1 'color-instance-rgb-components color domain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; face-boolean specifiers

(defun make-face-boolean-specifier (spec-list)
  "Return a new `face-boolean' specifier object with the given spec list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for a detailed description of
how specifiers work.

Valid instantiators for face-boolean specifiers are

-- t or nil
-- a vector of two or three elements: a face to inherit from,
   optionally a symbol naming the property of that face to inherit from
   (if omitted, defaults to the same property that this face-boolean
   specifier is used for; if this specifier is not part of a face,
   the instantiator would not be valid), and optionally a value which,
   if non-nil, means to invert the sense of the inherited property."
  (make-specifier-and-init 'color spec-list))

;;; objects.el ends here.
