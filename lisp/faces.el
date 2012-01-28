;;; faces.el --- Lisp interface to the C "face" structure

;; Copyright (C) 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 1995, 1996 Ben Wing

;; Author: Ben Wing <ben@xemacs.org>
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

;;; Synched up with: Not synched with FSF.  Almost completely divergent.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; face implementation #1 (used Lisp vectors and parallel C vectors;
;; FSFmacs still uses this) authored by Jamie Zawinski <jwz@jwz.org>
;; pre Lucid-Emacs 19.0.

;; face implementation #2 (used one face object per frame per face)
;; authored by Jamie Zawinski for 19.9.

;; face implementation #3 (use one face object per face) originally
;; authored for 19.12 by Chuck Thompson <cthomp@cs.uiuc.edu>,
;; rewritten by Ben Wing with the advent of specifiers.


;;; Some stuff in FSF's faces.el is in our x-faces.el.

;;; Code:
(eval-when-compile (require 'font))

(defgroup faces nil
  "Support for multiple text attributes (fonts, colors, ...)
Such a collection of attributes is called a \"face\"."
  :group 'emacs)


(defun read-face-name (prompt)
  (let (face)
    (while (= (length face) 0) ; nil or ""
      (setq face (completing-read prompt
				  (mapcar (lambda (x) (list (symbol-name x)))
					  (face-list))
				  nil t)))
    (intern face)))

(defun face-interactive (what &optional bool)
  (let* ((fn (intern (concat "face-" what "-instance")))
	 (face (read-face-name (format "Set %s of face: " what)))
	 (default (if (fboundp fn)
		      ;; #### we should distinguish here between
		      ;; explicitly setting the value to be the
		      ;; same as the default face's value, and
		      ;; not setting a value at all.
		      (funcall fn face)))
	 (value (if bool
		    (y-or-n-p (format "Should face %s be %s? "
				      (symbol-name face) bool))
		  (read-string (format "Set %s of face %s to: "
				       what (symbol-name face))
		   (cond ((font-instance-p default)
			  (font-instance-name default))
			 ((color-instance-p default)
			  (color-instance-name default))
			 ((image-instance-p default)
			  (image-instance-file-name default))
			 (t default))))))
    (list face (if (equal value "") nil value))))

(defconst built-in-face-specifiers
  (built-in-face-specifiers)
  "A list of the built-in face properties that are specifiers.")

(defun face-property (face property &optional locale tag-set exact-p)
  "Return FACE's value of the given PROPERTY.

If LOCALE is omitted, the FACE's actual value for PROPERTY will be
  returned.  For built-in properties, this will be a specifier object
  of a type appropriate to the property (e.g. a font or color
  specifier).  For other properties, this could be anything.

If LOCALE is supplied, then instead of returning the actual value,
  the specification(s) for the given locale or locale type will
  be returned.  This will only work if the actual value of
  PROPERTY is a specifier (this will always be the case for built-in
  properties, but not or not may apply to user-defined properties).
  If the actual value of PROPERTY is not a specifier, this value
  will simply be returned regardless of LOCALE.

The return value will be a list of instantiators (e.g. strings
  specifying a font or color name), or a list of specifications, each
  of which is a cons of a locale and a list of instantiators.
  Specifically, if LOCALE is a particular locale (a buffer, window,
  frame, device, or 'global), a list of instantiators for that locale
  will be returned.  Otherwise, if LOCALE is a locale type (one of
  the symbols 'buffer, 'window, 'frame, or 'device), the specifications
  for all locales of that type will be returned.  Finally, if LOCALE is
  'all, the specifications for all locales of all types will be returned.

The specifications in a specifier determine what the value of
  PROPERTY will be in a particular \"domain\" or set of circumstances,
  which is typically a particular Emacs window along with the buffer
  it contains and the frame and device it lies within.  The value is
  derived from the instantiator associated with the most specific
  locale (in the order buffer, window, frame, device, and 'global)
  that matches the domain in question.  In other words, given a domain
  (i.e. an Emacs window, usually), the specifier for PROPERTY will
  first be searched for a specification whose locale is the buffer
  contained within that window; then for a specification whose locale
  is the window itself; then for a specification whose locale is the
  frame that the window is contained within; etc.  The first
  instantiator that is valid for the domain (usually this means that
  the instantiator is recognized by the device [i.e. MS Windows, the X
  server or TTY device] that the domain is on.  The function
  `face-property-instance' actually does all this, and is used to
  determine how to display the face.

See `set-face-property' for the built-in property-names."

  (setq face (get-face face))
  (let ((value (get face property)))
    (if (and locale
	     (or (memq property built-in-face-specifiers)
		 (specifierp value)))
	(setq value (specifier-specs value locale tag-set exact-p)))
    value))

(defun convert-face-property-into-specifier (face property)
  "Convert PROPERTY on FACE into a specifier, if it's not already."
  (setq face (get-face face))
  (let ((specifier (get face property)))
    ;; if a user-property does not have a specifier but a
    ;; locale was specified, put a specifier there.
    ;; If there was already a value there, convert it to a
    ;; specifier with the value as its 'global instantiator.
    (unless (specifierp specifier)
      (let ((new-specifier (make-specifier 'generic)))
	(if (or (not (null specifier))
		;; make sure the nil returned from `get' wasn't
		;; actually the value of the property
		(null (get face property t)))
	    (add-spec-to-specifier new-specifier specifier))
	(setq specifier new-specifier)
	(put face property specifier)))))

(defun face-property-instance (face property
				    &optional domain default no-fallback)
  "Return the instance of FACE's PROPERTY in the specified DOMAIN.

Under most circumstances, DOMAIN will be a particular window,
  and the returned instance describes how the specified property
  actually is displayed for that window and the particular buffer
  in it.  Note that this may not be the same as how the property
  appears when the buffer is displayed in a different window or
  frame, or how the property appears in the same window if you
  switch to another buffer in that window; and in those cases,
  the returned instance would be different.

The returned instance will typically be a color-instance,
  font-instance, or pixmap-instance object, and you can query
  it using the appropriate object-specific functions.  For example,
  you could use `color-instance-rgb-components' to find out the
  RGB (red, green, and blue) components of how the 'background
  property of the 'highlight face is displayed in a particular
  window.  The results might be different from the results
  you would get for another window (perhaps the user
  specified a different color for the frame that window is on;
  or perhaps the same color was specified but the window is
  on a different X server, and that X server has different RGB
  values for the color from this one).

DOMAIN defaults to the selected window if omitted.

DOMAIN can be a frame or device, instead of a window.  The value
  returned for a such a domain is used in special circumstances
  when a more specific domain does not apply; for example, a frame
  value might be used for coloring a toolbar, which is conceptually
  attached to a frame rather than a particular window.  The value
  is also useful in determining what the value would be for a
  particular window within the frame or device, if it is not
  overridden by a more specific specification.

If PROPERTY does not name a built-in property, its value will
  simply be returned unless it is a specifier object, in which case
  it will be instanced using `specifier-instance'.

Optional arguments DEFAULT and NO-FALLBACK are the same as in
  `specifier-instance'."

  (setq face (get-face face))
  (let ((value (get face property)))
    (if (specifierp value)
	(setq value (specifier-instance value domain default no-fallback)))
    value))

(defun face-property-matching-instance (face property matchspec
					     &optional domain default
					     no-fallback)
  "Return the instance of FACE's PROPERTY matching MATCHSPEC in DOMAIN.
Currently the only useful value for MATCHSPEC is a charset, when used
in conjunction with the face's font; this allows you to retrieve a
font that can be used to display a particular charset, rather than just
any font.

Other than MATCHSPEC, this function is identical to `face-property-instance'.
See also `specifier-matching-instance' for a fuller description of the
matching process."

  (setq face (get-face face))
  (let ((value (get face property)))
    (if (specifierp value)
	(setq value (specifier-matching-instance value matchspec domain
						 default no-fallback)))
    value))

(defun set-face-property (face property value &optional locale tag-set
			       how-to-add)
  "Change a property of FACE.

NOTE: If you want to remove a property from a face, use `remove-face-property'
  rather than attempting to set a value of nil for the property.

For built-in properties, the actual value of the property is a
  specifier and you cannot change this; but you can change the
  specifications within the specifier, and that is what this function
  will do.  For user-defined properties, you can use this function
  to either change the actual value of the property or, if this value
  is a specifier, change the specifications within it.

If PROPERTY is a built-in property, the specifications to be added to
  this property can be supplied in many different ways:

  -- If VALUE is a simple instantiator (e.g. a string naming a font or
     color) or a list of instantiators, then the instantiator(s) will
     be added as a specification of the property for the given LOCALE
     (which defaults to 'global if omitted).
  -- If VALUE is a list of specifications (each of which is a cons of
     a locale and a list of instantiators), then LOCALE must be nil
     (it does not make sense to explicitly specify a locale in this
     case), and specifications will be added as given.
  -- If VALUE is a specifier (as would be returned by `face-property'
     if no LOCALE argument is given), then some or all of the
     specifications in the specifier will be added to the property.
     In this case, the function is really equivalent to
     `copy-specifier' and LOCALE has the same semantics (if it is
     a particular locale, the specification for the locale will be
     copied; if a locale type, specifications for all locales of
     that type will be copied; if nil or 'all, then all
     specifications will be copied).

HOW-TO-ADD should be either nil or one of the symbols 'prepend,
  'append, 'remove-tag-set-prepend, 'remove-tag-set-append, 'remove-locale,
  'remove-locale-type, or 'remove-all.  See `copy-specifier' and
  `add-spec-to-specifier' for a description of what each of
  these means.  Most of the time, you do not need to worry about
  this argument; the default behavior usually is fine.

In general, it is OK to pass an instance object (e.g. as returned
  by `face-property-instance') as an instantiator in place of
  an actual instantiator.  In such a case, the instantiator used
  to create that instance object will be used (for example, if
  you set a font-instance object as the value of the 'font
  property, then the font name used to create that object will
  be used instead).  If some cases, however, doing this
  conversion does not make sense, and this will be noted in
  the documentation for particular types of instance objects.

If PROPERTY is not a built-in property, then this function will
  simply set its value if LOCALE is nil.  However, if LOCALE is
  given, then this function will attempt to add VALUE as the
  instantiator for the given LOCALE, using `add-spec-to-specifier'.
  If the value of the property is not a specifier, it will
  automatically be converted into a 'generic specifier.


The following symbols have predefined meanings:

 foreground         The foreground color of the face.
		    For valid instantiators, see `make-color-specifier'.

 background         The background color of the face.
		    For valid instantiators, see `make-color-specifier'.

 font               The font used to display text covered by this face.
		    For valid instantiators, see `make-font-specifier'.

 display-table      The display table of the face.
		    This should be a vector of 256 elements.

 background-pixmap  The pixmap displayed in the background of the face.
		    Only used by faces on X and MS Windows devices.
		    For valid instantiators, see `make-image-specifier'.

 underline          Underline all text covered by this face.
		    For valid instantiators, see `make-face-boolean-specifier'.

 strikethru         Draw a line through all text covered by this face.
		    For valid instantiators, see `make-face-boolean-specifier'.

 highlight          Highlight all text covered by this face.
		    Only used by faces on TTY devices.
		    For valid instantiators, see `make-face-boolean-specifier'.

 dim                Dim all text covered by this face.
		    For valid instantiators, see `make-face-boolean-specifier'.

 blinking           Blink all text covered by this face.
		    Only used by faces on TTY devices.
		    For valid instantiators, see `make-face-boolean-specifier'.

 reverse            Reverse the foreground and background colors.
		    Only used by faces on TTY devices.
		    For valid instantiators, see `make-face-boolean-specifier'.

 inherit	    Face name or face object from which to inherit attributes,
		    or a list of such elements.  Attributes from inherited
		    faces are merged into the face like an underlying face
		    would be, with higher priority than underlying faces.

 doc-string         Description of what the face's normal use is.
		    NOTE: This is not a specifier, unlike all
		    the other built-in properties, and cannot
		    contain locale-specific values."

  (setq face (get-face face))
  (if (memq property built-in-face-specifiers)
      (set-specifier (get face property) value locale tag-set how-to-add)

    ;; This section adds user defined properties.
    (if (not locale)
	(put face property value)
      (convert-face-property-into-specifier face property)
      (add-spec-to-specifier (get face property) value locale tag-set
			     how-to-add)))
  value)

(defun remove-face-property (face property &optional locale tag-set exact-p)
  "Remove a property from FACE.
For built-in properties, this is analogous to `remove-specifier'.
See `remove-specifier' for the meaning of the LOCALE, TAG-SET, and EXACT-P
arguments."
  (or locale (setq locale 'all))
  (if (memq property built-in-face-specifiers)
      (remove-specifier (face-property face property) locale tag-set exact-p)
    (if (eq locale 'all)
	(remprop (get-face face) property)
      (convert-face-property-into-specifier face property)
      (remove-specifier (face-property face property) locale tag-set
			exact-p))))

(defun reset-face (face &optional locale tag-set exact-p)
  "Clear all existing built-in specifications from FACE.
This makes FACE inherit all its display properties from 'default.
WARNING: Be absolutely sure you want to do this!!!  It is a dangerous
operation and is not undoable.

The arguments LOCALE, TAG-SET and EXACT-P are the same as for
`remove-specifier'."
  (mapc (lambda (x)
	  (remove-specifier (face-property face x) locale tag-set exact-p))
	built-in-face-specifiers)
  nil)

(defun set-face-parent (face parent &optional locale tag-set how-to-add)
  "Set the parent of FACE to PARENT, for all properties.
This makes all properties of FACE inherit from PARENT."
  (setq parent (get-face parent))
  (mapcar (lambda (x)
	    (set-face-property face x (vector parent) locale tag-set
			       how-to-add))
	  (delq 'display-table
		(delq 'background-pixmap
		      (copy-sequence built-in-face-specifiers))))
  (set-face-background-pixmap face (vector 'inherit ':face parent)
			      locale tag-set how-to-add)
  nil)

(defun face-doc-string (face)
  "Return the documentation string for FACE."
  (face-property face 'doc-string))

(defun set-face-doc-string (face doc-string)
  "Change the documentation string of FACE to DOC-STRING."
  (interactive (face-interactive "doc-string"))
  (set-face-property face 'doc-string doc-string))

(defun face-font-name (face &optional domain charset)
  "Return the font name of FACE in DOMAIN, or nil if it is unspecified.
DOMAIN is as in `face-font-instance'."
  (let ((f (face-font-instance face domain charset)))
    (and f (font-instance-name f))))

(defun face-font (face &optional locale tag-set exact-p)
  "Return the font of FACE in LOCALE, or nil if it is unspecified.

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), 'all (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'font locale tag-set exact-p))

(defun face-font-instance (face &optional domain charset)
  "Return the instance of FACE's font in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the font appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (if charset
      (face-property-matching-instance face 'font charset domain)
    (face-property-instance face 'font domain)))

(defun set-face-font (face font &optional locale tag-set how-to-add)
  "Change the font of FACE to FONT in LOCALE.

FACE may be either a face object or a symbol representing a face.

FONT should be an instantiator (see `make-font-specifier'), a list of
  instantiators, an alist of specifications (each mapping a
  locale to an instantiator list), or a font specifier object.

If FONT is an alist, LOCALE must be omitted.  If FONT is a
  specifier object, LOCALE can be a locale, a locale type, 'all,
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to 'global.

See `set-face-property' for more information."
  (interactive (face-interactive "font"))
  (set-face-property face 'font font locale tag-set how-to-add))

(defun face-foreground (face &optional locale tag-set exact-p)
  "Return the foreground of FACE in LOCALE, or nil if it is unspecified.

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), 'all (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'foreground locale tag-set exact-p))

(defun face-foreground-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's foreground in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the foreground appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'foreground domain default no-fallback))

(defun face-foreground-name (face &optional domain default no-fallback)
  "Return the name of FACE's foreground color in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (color-instance-name (face-foreground-instance
			face domain default no-fallback)))

(defun set-face-foreground (face color &optional locale tag-set how-to-add)
  "Change the foreground color of FACE to COLOR in LOCALE.

FACE may be either a face object or a symbol representing a face.

COLOR should be an instantiator (see `make-color-specifier'), a list of
  instantiators, an alist of specifications (each mapping a locale to
  an instantiator list), or a color specifier object.

If COLOR is an alist, LOCALE must be omitted.  If COLOR is a
  specifier object, LOCALE can be a locale, a locale type, 'all,
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to 'global.

See `set-face-property' for more information."
  (interactive (face-interactive "foreground"))
  (set-face-property face 'foreground color locale tag-set how-to-add))

(defun face-background (face &optional locale tag-set exact-p)
  "Return the background color of FACE in LOCALE, or nil if it is unspecified.

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), 'all (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'background locale tag-set exact-p))

(defun face-background-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's background in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'background domain default no-fallback))

(defun face-background-name (face &optional domain default no-fallback)
  "Return the name of FACE's background color in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (color-instance-name (face-background-instance
			face domain default no-fallback)))

(defun set-face-background (face color &optional locale tag-set how-to-add)
  "Change the background color of FACE to COLOR in LOCALE.

FACE may be either a face object or a symbol representing a face.

COLOR should be an instantiator (see `make-color-specifier'), a list of
  instantiators, an alist of specifications (each mapping a locale to
  an instantiator list), or a color specifier object.

If COLOR is an alist, LOCALE must be omitted.  If COLOR is a
  specifier object, LOCALE can be a locale, a locale type, 'all,
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to 'global.

See `set-face-property' for more information."
  (interactive (face-interactive "background"))
  (set-face-property face 'background color locale tag-set how-to-add))

(defun face-background-pixmap (face &optional locale tag-set exact-p)
  "Return the background pixmap of FACE in LOCALE, or nil if it is unspecified.
This property is only used on window system devices.

FACE may be either a face object or a symbol representing a face.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), 'all (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'background-pixmap locale tag-set exact-p))

(defun face-background-pixmap-instance (face &optional domain default
					     no-fallback)
  "Return the instance of FACE's background pixmap in DOMAIN.

FACE may be either a face object or a symbol representing a face.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the background appears in that
  particular window and buffer will be returned.

See `face-property-instance' for more information."
  (face-property-instance face 'background-pixmap domain default no-fallback))

(defun set-face-background-pixmap (face pixmap &optional locale tag-set
					how-to-add)
  "Change the background pixmap of FACE to PIXMAP in LOCALE.
This property is only used on window system devices.

FACE may be either a face object or a symbol representing a face.

PIXMAP should be an instantiator (see `make-image-specifier'), a list
  of instantiators, an alist of specifications (each mapping a locale
  to an instantiator list), or an image specifier object.

If PIXMAP is an alist, LOCALE must be omitted.  If PIXMAP is a
  specifier object, LOCALE can be a locale, a locale type, 'all,
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to 'global.

See `set-face-property' for more information."
  (interactive (face-interactive "background-pixmap"))
  (set-face-property face 'background-pixmap pixmap locale tag-set how-to-add))

(defun face-display-table (face &optional locale tag-set exact-p)
  "Return the display table of FACE in LOCALE.

A vector (as returned by `make-display-table') will be returned.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), 'all (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `face-property' for more information."
  (face-property face 'display-table locale tag-set exact-p))

(defun face-display-table-instance (face &optional domain default no-fallback)
  "Return the instance of FACE's display table in DOMAIN.
A vector (as returned by `make-display-table') will be returned.

See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'display-table domain default no-fallback))

(defun set-face-display-table (face display-table &optional locale tag-set
				    how-to-add)
  "Change the display table of FACE to DISPLAY-TABLE in LOCALE.
DISPLAY-TABLE should be a vector as returned by `make-display-table'.

See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
  HOW-TO-ADD arguments."
  (interactive (face-interactive "display-table"))
  (set-face-property face 'display-table display-table locale tag-set
		     how-to-add))

;; The following accessors and mutators are, IMHO, good
;; implementation.  Cf. with `make-face-bold'.

(defun face-underline-p (face &optional domain default no-fallback)
  "Return t if FACE is underlined in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'underline domain default no-fallback))

(defun set-face-underline-p (face underline-p &optional locale tag-set
				  how-to-add)
  "Change the underline property of FACE to UNDERLINE-P.
UNDERLINE-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "underline-p" "underlined"))
  (set-face-property face 'underline underline-p locale tag-set how-to-add))

(defun face-strikethru-p (face &optional domain default no-fallback)
  "Return t if FACE is strikethru-d (i.e. struck through) in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'strikethru domain default no-fallback))

(defun set-face-strikethru-p (face strikethru-p &optional locale tag-set
				  how-to-add)
  "Change whether FACE is strikethru-d (i.e. struck through) in LOCALE.
STRIKETHRU-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "strikethru-p" "strikethru-d"))
  (set-face-property face 'strikethru strikethru-p locale tag-set how-to-add))

(defun face-highlight-p (face &optional domain default no-fallback)
  "Return t if FACE is highlighted in DOMAIN (TTY domains only).
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'highlight domain default no-fallback))

(defun set-face-highlight-p (face highlight-p &optional locale tag-set
				  how-to-add)
  "Change whether FACE is highlighted in LOCALE (TTY locales only).
HIGHLIGHT-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "highlight-p" "highlighted"))
  (set-face-property face 'highlight highlight-p locale tag-set how-to-add))

(defun face-dim-p (face &optional domain default no-fallback)
  "Return t if FACE is dimmed in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'dim domain default no-fallback))

(defun set-face-dim-p (face dim-p &optional locale tag-set how-to-add)
  "Change whether FACE is dimmed in LOCALE.
DIM-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "dim-p" "dimmed"))
  (set-face-property face 'dim dim-p locale tag-set how-to-add))

(defun face-blinking-p (face &optional domain default no-fallback)
  "Return t if FACE is blinking in DOMAIN (TTY domains only).
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'blinking domain default no-fallback))

(defun set-face-blinking-p (face blinking-p &optional locale tag-set
				 how-to-add)
  "Change whether FACE is blinking in LOCALE (TTY locales only).
BLINKING-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "blinking-p" "blinking"))
  (set-face-property face 'blinking blinking-p locale tag-set how-to-add))

(defun face-reverse-p (face &optional domain default no-fallback)
  "Return t if FACE is reversed in DOMAIN (TTY domains only).
See `face-property-instance' for the semantics of the DOMAIN argument."
  (face-property-instance face 'reverse domain default no-fallback))

(defun set-face-reverse-p (face reverse-p &optional locale tag-set how-to-add)
  "Change whether FACE is reversed in LOCALE (TTY locales only).
REVERSE-P is normally a face-boolean instantiator; see
 `make-face-boolean-specifier'.
See `set-face-property' for the semantics of the LOCALE, TAG-SET, and
 HOW-TO-ADD arguments."
  (interactive (face-interactive "reverse-p" "reversed"))
  (set-face-property face 'reverse reverse-p locale tag-set how-to-add))


(defun face-property-equal (face1 face2 prop domain)
  (equal (face-property-instance face1 prop domain)
	 (face-property-instance face2 prop domain)))

(defun face-equal-loop (props face1 face2 domain)
  (while (and props
	      (face-property-equal face1 face2 (car props) domain))
    (setq props (cdr props)))
  (null props))

(defun face-equal (face1 face2 &optional domain)
  "Return t if FACE1 and FACE2 will display in the same way in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (if (null domain) (setq domain (selected-window)))
  (if (not (valid-specifier-domain-p domain))
      (error "Invalid specifier domain"))
  (let ((device (dfw-device domain))
	(common-props '(foreground background font display-table underline
				   dim))
	(win-props '(background-pixmap strikethru))
	(tty-props '(highlight blinking reverse)))

    ;; First check the properties which are used in common between the
    ;; x and tty devices.  Then, check those properties specific to
    ;; the particular device type.
    (and (face-equal-loop common-props face1 face2 domain)
	 (cond ((eq 'tty (device-type device))
		(face-equal-loop tty-props face1 face2 domain))
	       ;; #### Why isn't this (console-on-window-system-p (device-console device))?
	       ;; #### FIXME!
	       ((eq 'x (device-type device))
		(face-equal-loop win-props face1 face2 domain))
	       (t t)))))

(defun face-differs-from-default-p (face &optional domain)
  "Return t if FACE will display differently from the default face in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (not (face-equal face 'default domain)))

; moved from x-faces.el
(defun try-font-name (name &optional device)
  ;; yes, name really should be here twice.
  (and name (make-font-instance name device t) name))


;; This function is a terrible, disgusting hack!!!!  Need to
;; separate out the font elements as separate face properties!

;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
;; WE DEMAND LEXICAL SCOPING!!!
(defun frob-face-property (face property func device-tags &optional
locale tags)
  "Change the specifier for FACE's PROPERTY according to FUNC, in LOCALE.
This function is ugly and messy and is primarily used as an internal
helper function for `make-face-bold' et al., so you probably don't
want to use it or read the rest of the documentation.  But if you do ...

FUNC should be a function of two arguments (an instance and a device)
that returns a modified name that is valid for the given device.
If LOCALE specifies a valid domain (i.e. a window, frame, or device),
this function instantiates the specifier over that domain, applies FUNC
to the resulting instance, and adds the result back as an instantiator
for that locale.  Otherwise, LOCALE should be a locale, locale type, or
'all (defaults to 'all if omitted).  For each specification thusly
included: if the locale given is a valid domain, FUNC will be
iterated over all valid instantiators for the device of the domain
until a non-nil result is found (if there is no such result, the
first valid instantiator is used), and that result substituted for
the specification; otherwise, the process just outlined is
iterated over each existing device and the concatenated results
substituted for the specification.

DEVICE-TAGS is a list of tags that each device must match in order for
the function to be called on it."
  (let ((sp (face-property face property))
	temp-sp)
    (if (valid-specifier-domain-p locale)
	;; this is easy.
	(let* ((inst (face-property-instance face property locale))
	       (name (and inst
			  (device-matches-specifier-tag-set-p
			   (dfw-device locale) device-tags)
			  (funcall func inst (dfw-device locale)))))
	  (when name
	    (add-spec-to-specifier sp name locale tags)))
      ;; otherwise, map over all specifications ...
      ;; but first, some further kludging:
      ;; (1) if we're frobbing the global property, make sure
      ;;     that something is there (copy from the default face,
      ;;     if necessary).  Otherwise, something like
      ;;     (make-face-larger 'modeline)
      ;;     won't do anything at all if the modeline simply
      ;;     inherits its font from 'default.
      ;; (2) if we're frobbing a particular locale, nothing would
      ;;     happen if that locale has no instantiators.  So signal
      ;;     an error to indicate this.


      (setq temp-sp (copy-specifier sp))
      (if (or (eq locale 'global) (eq locale 'all) (not locale))
	  (when (not (specifier-specs temp-sp 'global))
	    ;; Try fallback via the official ways and then do it "by hand"
	    (let* ((fallback (specifier-fallback sp))
		   (fallback-sp
		    (cond ((specifierp fallback) fallback)
			  ;; just an inst list
			  (fallback
			   (make-specifier-and-init (specifier-type sp)
						    fallback))
			  ((eq (get-face face) (get-face 'default))
			   (error "Unable to find global specification"))
			  ;; If no fallback we snoop from default
			  (t (face-property 'default property)))))
	      (copy-specifier fallback-sp temp-sp 'global))))
      (if (and (valid-specifier-locale-p locale)
	       (not (specifier-specs temp-sp locale)))
	  (error "Property must have a specification in locale %S" locale))
      (map-specifier
       temp-sp
       (lambda (sp-arg locale inst-list func)
	 (let* ((device (dfw-device locale))
		;; if a device can be derived from the locale,
		;; call frob-face-property-1 for that device.
		;; Otherwise map frob-face-property-1 over each device.
		(result
		 (if device
		     (list (and (device-matches-specifier-tag-set-p
				 device device-tags)
				(frob-face-property-1 sp-arg device inst-list
						      func)))
		   (mapcar (lambda (device)
			     (and (device-matches-specifier-tag-set-p
				   device device-tags)
				  (frob-face-property-1 sp-arg device
							inst-list func)))
			   (device-list))))
		new-result)
	   ;; remove duplicates and nils from the obtained list of
	   ;; instantiators. Also add tags amd remove 'defaults'.
	   (mapcar (lambda (arg)
		     (when arg
		       (if (not (consp arg))
			   (setq arg (cons tags arg))
			 (setcar arg (append tags (delete 'default
							  (car arg))))))
		     (when (and arg (not (member arg new-result)))
		       (setq new-result (cons arg new-result))))
		   result)
	   ;; add back in.
	   (add-spec-list-to-specifier sp (list (cons locale new-result)))
	   ;; tell map-specifier to keep going.
	   nil))
       locale
       func))))

(defun frob-face-property-1 (sp device inst-list func)
  (let
      (first-valid result)
    (while (and inst-list (not result))
      (let* ((inst-pair (car inst-list))
	     (tag-set (car inst-pair))
	     (sp-inst (specifier-instance-from-inst-list
		       sp device (list inst-pair))))
	(if sp-inst
	    (progn
	      (if (not first-valid)
		  (setq first-valid inst-pair))
	      (setq result (funcall func sp-inst device))
	      (if result
		  (setq result (cons tag-set result))))))
      (setq inst-list (cdr inst-list)))
    (or result first-valid)))

(defcustom face-frob-from-locale-first nil
  "*If non nil, use kludgy way of frobbing fonts suitable for non-mule
multi-charset environments."
  :group 'faces
  :type 'boolean)

(defun frob-face-font-2 (face locale tags unfrobbed-face frobbed-face
			      tty-thunk ws-thunk standard-face-mapping)
  ;; another kludge to make things more intuitive.  If we're
  ;; inheriting from a standard face in this locale, frob the
  ;; inheritance as appropriate.  Else, if, after the first
  ;; window-system frobbing pass, the face hasn't changed and still
  ;; looks like the standard unfrobbed face (e.g. 'default), make it
  ;; inherit from the standard frobbed face (e.g. 'bold).  Regardless
  ;; of things, do the TTY frobbing.

  ;; yuck -- The LOCALE argument to make-face-bold is not actually a locale,
  ;; but is a "locale, locale-type, or nil for all".  So ...  do our extra
  ;; frobbing only if it's actually a locale; or for nil, do the frobbing
  ;; on 'global.  This specifier stuff needs some rethinking.
  (let* ((the-locale (cond ((null locale) 'global)
			   ((valid-specifier-locale-p locale) locale)
			   (t nil)))
	 (spec-list
	  (and
	   the-locale
	   (specifier-spec-list (get (get-face face) 'font) the-locale tags t)))
	 (change-it
	  (and
	   spec-list
	   (cdr (assoc (cdadar spec-list) standard-face-mapping)))))
    (if (and change-it
	     (not (memq (face-name (find-face face))
			'(default bold italic bold-italic))))
	(progn
	  (or (equal change-it t)
	      (set-face-property face 'font change-it the-locale tags))
	  (funcall tty-thunk))
      (let* ((domain (cond ((null the-locale) nil)
			   ((valid-specifier-domain-p the-locale) the-locale)
			   ;; OK, this next one is truly a kludge, but
			   ;; it results in more intuitive behavior most
			   ;; of the time. (really!)
			   ((or (eq the-locale 'global) (eq the-locale 'all))
			    (selected-device))
			   (t nil)))
	     (inst (and domain (face-property-instance face 'font domain))))
	;; If it's reasonable to do the inherit-from-standard-face trick,
	;; and it's called for, then do it now.
	(if (and
	     face-frob-from-locale-first
	     (eq the-locale 'global)
	     domain
	     (equal inst (face-property-instance face 'font domain))
	     ;; don't do it for standard faces, or you'll get inheritance loops.
	     ;; #### This makes XEmacs seg fault! fix this bug.
	     (not (memq (face-name (find-face face))
			'(default bold italic bold-italic)))
	     (equal (face-property-instance face 'font domain)
		    (face-property-instance unfrobbed-face 'font domain)))
	    (set-face-property face 'font (vector frobbed-face)
			       the-locale tags)
	  ;; and only otherwise try to build new property value artificially
	  (funcall tty-thunk)
	  (funcall ws-thunk)
	  (and
	   domain
	   (equal inst (face-property-instance face 'font domain))
	   ;; don't do it for standard faces, or you'll get inheritance loops.
	   ;; #### This makes XEmacs seg fault! fix this bug.
	   (not (memq (face-name (find-face face))
		      '(default bold italic bold-italic)))
	   (equal (face-property-instance face 'font domain)
		  (face-property-instance unfrobbed-face 'font domain))
	   (set-face-property face 'font (vector frobbed-face) the-locale tags)))))))

;; WE DEMAND FOUNDRY FROBBING!

;; Family frobbing
;; Thx Jan Vroonhof, Ref xemacs-beta <87oflypbum.fsf@petteflet.ntlworld.com>
;; Brainlessly derived from make-face-size by Stephen; don't blame Jan.
;; I'm long since flown to Rio, it does you little good to blame me, either.
(defun make-face-family (face family &optional locale tags)
  "Set FACE's family to FAMILY in LOCALE, if possible.

Add/replace settings specified by TAGS only."
  (frob-face-property face 'font
		      ;; uses dynamic scope of family
		      #'(lambda (f d)
			  ;; keep the dependency on font.el for now
			  (let ((fo (font-create-object (font-instance-name f)
							d)))
			    (set-font-family fo family)
			    (font-create-name fo d)))
		      nil locale tags))

;; Style (ie, typographical face) frobbing
(defun make-face-bold (face &optional locale tags)
  "Make FACE bold in LOCALE, if possible.
This will attempt to make the font bold for X locales and will set the
highlight flag for TTY locales.

If LOCALE is nil, omitted, or `all', this will attempt to frob all
font specifications for FACE to make them appear bold.  Similarly, if
LOCALE is a locale type, this frobs all font specifications for locales
of that type.  If LOCALE is a particular locale, what happens depends on
what sort of locale is given.  If you gave a device, frame, or window,
then it's always possible to determine what the font actually will be,
so this is determined and the resulting font is frobbed and added back as a
specification for this locale.  If LOCALE is a buffer, however, you can't
determine what the font will actually be unless there's actually a
specification given for that particular buffer (otherwise, it depends
on what window and frame the buffer appears in, and might not even be
well-defined if the buffer appears multiple times in different places);
therefore you will get an error unless there's a specification for the
buffer.

Finally, in some cases (specifically, when LOCALE is not a locale type),
if the frobbing didn't actually make the font look any different
\(this happens, for example, if your font specification is already bold
or has no bold equivalent), and currently looks like the font of the
'default face, it is set to inherit from the 'bold face.  This is kludgy
but it makes `make-face-bold' have more intuitive behavior in many
circumstances."
  (interactive (list (read-face-name "Make which face bold: ")))
  (frob-face-font-2
   face locale tags 'default 'bold
   (lambda ()
     ;; handle TTY specific entries
     (when (featurep 'tty)
       (set-face-highlight-p face t locale (cons 'tty tags))))
   (lambda ()
     ;; handle window-system specific entries
     (when (featurep 'x)
       (frob-face-property face 'font 'x-make-font-bold
			   '(x) locale tags))
     )
   '(([default] . [bold])
     ([bold] . t)
     ([italic] . [bold-italic])
     ([bold-italic] . t))))

(defun make-face-italic (face &optional locale tags)
  "Make FACE italic in LOCALE, if possible.
This will attempt to make the font italic for X/MS Windows locales and
will set the underline flag for TTY locales.  See `make-face-bold' for
the semantics of the LOCALE argument and for more specifics on exactly
how this function works."
  (interactive (list (read-face-name "Make which face italic: ")))
  (frob-face-font-2
   face locale tags 'default 'italic
   (lambda ()
     ;; handle TTY specific entries
     (when (featurep 'tty)
       (set-face-underline-p face t locale (cons 'tty tags))))
   (lambda ()
     ;; handle window-system specific entries
     (when (featurep 'x)
       (frob-face-property face 'font 'x-make-font-italic
			   '(x) locale tags))
     )
   '(([default] . [italic])
     ([bold] . [bold-italic])
     ([italic] . t)
     ([bold-italic] . t))))

(defun make-face-bold-italic (face &optional locale tags)
  "Make FACE bold and italic in LOCALE, if possible.
This will attempt to make the font bold-italic for X/MS Windows
locales and will set the highlight and underline flags for TTY
locales.  See `make-face-bold' for the semantics of the LOCALE
argument and for more specifics on exactly how this function works."
  (interactive (list (read-face-name "Make which face bold-italic: ")))
  (frob-face-font-2
   face locale tags 'default 'bold-italic
   (lambda ()
     ;; handle TTY specific entries
     (when (featurep 'tty)
       (set-face-highlight-p face t locale (cons 'tty tags))
       (set-face-underline-p face t locale (cons 'tty tags))))
   (lambda ()
     ;; handle window-system specific entries
     (when (featurep 'x)
       (frob-face-property face 'font 'x-make-font-bold-italic
			   '(x) locale tags))
     )
   '(([default] . [italic])
     ([bold] . [bold-italic])
     ([italic] . [bold-italic])
     ([bold-italic] . t))))

(defun make-face-unbold (face &optional locale tags)
  "Make FACE non-bold in LOCALE, if possible.
This will attempt to make the font non-bold for X/MS Windows locales
and will unset the highlight flag for TTY locales.  See
`make-face-bold' for the semantics of the LOCALE argument and for more
specifics on exactly how this function works."
  (interactive (list (read-face-name "Make which face non-bold: ")))
  (frob-face-font-2
   face locale tags 'bold 'default
   (lambda ()
     ;; handle TTY specific entries
     (when (featurep 'tty)
       (set-face-highlight-p face nil locale (cons 'tty tags))))
   (lambda ()
     ;; handle window-system specific entries
     (when (featurep 'x)
       (frob-face-property face 'font 'x-make-font-unbold
			   '(x) locale tags))
     )
   '(([default] . t)
     ([bold] . [default])
     ([italic] . t)
     ([bold-italic] . [italic]))))

(defun make-face-unitalic (face &optional locale tags)
  "Make FACE non-italic in LOCALE, if possible.
This will attempt to make the font non-italic for X/MS Windows locales
and will unset the underline flag for TTY locales.  See
`make-face-bold' for the semantics of the LOCALE argument and for more
specifics on exactly how this function works."
  (interactive (list (read-face-name "Make which face non-italic: ")))
  (frob-face-font-2
   face locale tags 'italic 'default
   (lambda ()
     ;; handle TTY specific entries
     (when (featurep 'tty)
       (set-face-underline-p face nil locale (cons 'tty tags))))
   (lambda ()
     ;; handle window-system specific entries
     (when (featurep 'x)
       (frob-face-property face 'font 'x-make-font-unitalic
			   '(x) locale tags))
     )
   '(([default] . t)
     ([bold] . t)
     ([italic] . [default])
     ([bold-italic] . [bold]))))


;; Size frobbing
;; Thx Jan Vroonhof, Ref xemacs-beta <87oflypbum.fsf@petteflet.ntlworld.com>
;; Jan had a separate helper function
(defun make-face-size (face size &optional locale tags)
  "Adjust FACE to SIZE in LOCALE, if possible.

Add/replace settings specified by TAGS only."
  (frob-face-property face 'font
		      ;; uses dynamic scope of size
		      #'(lambda (f d)
			  ;; keep the dependency on font.el for now
			  (let ((fo (font-create-object (font-instance-name f)
							d)))
			    (set-font-size fo size)
			    (font-create-name fo d)))
		      nil locale tags))

;; Why do the following two functions lose so badly in so many
;; circumstances?

(defun make-face-smaller (face &optional locale)
  "Make the font of FACE be smaller, if possible.
LOCALE works as in `make-face-bold' et al., but the ``inheriting-
from-the-bold-face'' operations described there are not done
because they don't make sense in this context."
  (interactive (list (read-face-name "Shrink which face: ")))
  ;; handle X specific entries
  (when (featurep 'x)
    (frob-face-property face 'font 'x-find-smaller-font
			'(x) locale)))

(defun make-face-larger (face &optional locale)
  "Make the font of FACE be larger, if possible.
See `make-face-smaller' for the semantics of the LOCALE argument."
  (interactive (list (read-face-name "Enlarge which face: ")))
  ;; handle X specific entries
  (when (featurep 'x)
    (frob-face-property face 'font 'x-find-larger-font
			'(x) locale)))

(defun invert-face (face &optional locale)
  "Swap the foreground and background colors of the face."
  (interactive (list (read-face-name "Invert face: ")))
  (if (valid-specifier-domain-p locale)
      (let ((foreface (face-foreground-instance face locale)))
	(set-face-foreground face (face-background-instance face locale)
			     locale)
	(set-face-background face foreface locale))
    (let ((forespec (copy-specifier (face-foreground face) nil locale)))
      (copy-specifier (face-background face) (face-foreground face) locale)
      (copy-specifier forespec (face-background face) locale))))


;;; Convenience functions

(defun face-ascent (face &optional domain charset)
  "Return the ascent of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-ascent (face-font face) domain charset))

(defun face-descent (face &optional domain charset)
  "Return the descent of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-descent (face-font face) domain charset))

(defun face-width (face &optional domain charset)
  "Return the width of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-width (face-font face) domain charset))

(defun face-height (face &optional domain charset)
  "Return the height of FACE in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (+ (face-ascent face domain charset) (face-descent face domain charset)))

(defun face-proportional-p (face &optional domain charset)
  "Return t if FACE is proportional in DOMAIN.
See `face-property-instance' for the semantics of the DOMAIN argument."
  (font-proportional-p (face-font face) domain charset))


;; Functions that used to be in cus-face.el, but logically go here.

(defcustom frame-background-mode nil
  "*The brightness of the background.
Set this to the symbol dark if your background color is dark, light if
your background is light, or nil (default) if you want Emacs to
examine the brightness for you."
  :group 'faces
  :type '(choice (choice-item dark)
		 (choice-item light)
		 (choice-item :tag "Auto" nil)))

;; The old variable that many people still have in .emacs files.
(define-obsolete-variable-alias 'custom-background-mode
  'frame-background-mode)

(defun get-frame-background-mode (frame)
  "Detect background mode for FRAME."
  (let* ((color-instance (face-background-instance 'default frame))
	 (mode (condition-case nil
		   (if (< (apply '+ (color-instance-rgb-components
				     color-instance)) 65536)
		       'dark 'light)
		 ;; Here, we get an error on a TTY.  As we don't have
		 ;; a good way of detecting whether a TTY is light or
		 ;; dark, we'll guess it's dark.
		 (error 'dark))))
    (set-frame-property frame 'background-mode mode)
    mode))

(defun extract-custom-frame-properties (frame)
  "Return a plist with the frame properties of FRAME used by custom."
  (list 'type (or (frame-property frame 'display-type)
		  (device-type (frame-device frame)))
	'class (device-class (frame-device frame))
	'background (or frame-background-mode
			(frame-property frame 'background-mode)
			(get-frame-background-mode frame))))

(defcustom init-face-from-resources t
  "If non nil, attempt to initialize faces from the resource database."
  :group 'faces
  :type 'boolean)

;; Old name, used by custom.  Also, FSFmacs name.
(defvaralias 'initialize-face-resources 'init-face-from-resources)

;; Make sure all custom setting are added with this tag so we can
;; identify-them
(define-specifier-tag 'custom)

(defun face-spec-set (face spec &optional frame tags)
  "Set FACE's face attributes according to the first matching entry in SPEC.
If optional FRAME is non-nil, set it for that frame only.
If it is nil, then apply SPEC to each frame individually.
See `defface' for information about SPEC."
  (if frame
      (progn
	(reset-face face frame tags)
	(face-display-set face spec frame tags)
	(init-face-from-resources face frame))
    (let ((frames (relevant-custom-frames)))
      (reset-face face nil tags)
      ;; This should not be needed. We only remove our own specifiers
      ;; (if (and (eq 'default face) (featurep 'x))
      ;;	  (x-init-global-faces))
      (face-display-set face spec nil tags)
      (while frames
	(face-display-set face spec (car frames) tags)
	(pop frames))
      (init-face-from-resources face))))

(defun face-display-set (face spec &optional frame tags)
  "Set FACE to the attributes to the first matching entry in SPEC.
Iff optional FRAME is non-nil, set it for that frame only.
See `defface' for information about SPEC."
  (while spec
    (let ((display (caar spec))
	  (atts (cadar spec)))
      (pop spec)
      (when (face-spec-set-match-display display frame)
	;; Avoid creating frame local duplicates of the global face.
	(unless (and frame (eq display (get face 'custom-face-display)))
	  (apply 'face-custom-attributes-set face frame tags atts))
	(unless frame
	  (put face 'custom-face-display display))
	(setq spec nil)))))

(defvar default-custom-frame-properties nil
  "The frame properties used for the global faces.
Frames not matching these properties should have frame local faces.
The value should be nil, if uninitialized, or a plist otherwise.
See `defface' for a list of valid keys and values for the plist.")

(defun get-custom-frame-properties (&optional frame)
  "Return a plist with the frame properties of FRAME used by custom.
If FRAME is nil, return the default frame properties."
  (cond (frame
	 ;; Try to get from cache.
	 (let ((cache (frame-property frame 'custom-properties)))
	   (unless cache
	     ;; Oh well, get it then.
	     (setq cache (extract-custom-frame-properties frame))
	     ;; and cache it...
	     (set-frame-property frame 'custom-properties cache))
	   cache))
	(default-custom-frame-properties)
	(t
	 (setq default-custom-frame-properties
	       (extract-custom-frame-properties (selected-frame))))))

(defun face-spec-update-all-matching (spec display plist)
  "Update all entries in the face spec that could match display to
have the entries from the new plist and return the new spec."
  (mapcar
   (lambda (e)
     (let ((entries (car e))
	   (options (cadr e))
	   (match t)
	   dplist
	   (new-options plist)
	   )
       (unless (eq display t)
	 (mapc (lambda (arg)
		 (setq dplist (plist-put dplist (car arg) (cadr arg))))
	       display))
       (unless (eq entries t)
	 (mapc (lambda (arg)
		 (setq match (and match (eq (cadr arg)
					    (plist-get
					      dplist (car arg)
					      (cadr arg))))))
	       entries))
       (if (not match)
	   e
	 (while new-options
	   (setq options
		 (plist-put options (car new-options) (cadr new-options)))
	   (setq new-options (cddr new-options)))
	 (list entries options))))
   (copy-sequence spec)))



(defun face-spec-set-match-display (display &optional frame)
  "Return non-nil if DISPLAY matches FRAME.
DISPLAY is part of a spec such as can be used in `defface'.
If FRAME is nil or omitted, the selected frame is used."
  (if (eq display t)
      t
    (let* ((props (get-custom-frame-properties frame))
	   (type (plist-get props 'type))
	   (class (plist-get props 'class))
	   (background (plist-get props 'background))
	   (match t)
	   (entries display)
	   entry req options)
      (while (and entries match)
	(setq entry (car entries)
	      entries (cdr entries)
	      req (car entry)
	      options (cdr entry)
	      match (case req
		      (type       (memq type options))
		      (class      (memq class options))
		      (background (memq background options))
		      (t (warn "Unknown req `%S' with options `%S'"
			       req options)
			 nil))))
      match)))

(defun relevant-custom-frames ()
  "List of frames whose custom properties differ from the default."
  (let ((relevant nil)
	(default (get-custom-frame-properties))
	(frames (frame-list))
	frame)
    (while frames
      (setq frame (car frames)
	    frames (cdr frames))
      (unless (equal default (get-custom-frame-properties frame))
	(push frame relevant)))
    relevant))

(defun initialize-custom-faces (&optional frame)
  "Initialize all custom faces for FRAME.
If FRAME is nil or omitted, initialize them for all frames."
  (mapc (lambda (symbol)
	  (let ((spec (or (get symbol 'saved-face)
			  (get symbol 'face-defface-spec))))
	    (when spec
	      ;; No need to init-face-from-resources -- code in
	      ;; `init-frame-faces' does it already.
	      (face-display-set symbol spec frame))))
	(face-list)))

(defun custom-initialize-frame (frame)
  "Initialize frame-local custom faces for FRAME if necessary."
  (unless (equal (get-custom-frame-properties)
		 (get-custom-frame-properties frame))
    (initialize-custom-faces frame)))

(defun startup-initialize-custom-faces ()
  "Reset faces created by defface.  Only called at startup.
Don't use this function in your program."
  (when default-custom-frame-properties
    ;; Reset default value to the actual frame, not stream.
    (setq default-custom-frame-properties
	  (extract-custom-frame-properties (selected-frame)))
    ;; like initialize-custom-faces but removes property first.
    (mapc (lambda (symbol)
	    (let ((spec (or (get symbol 'saved-face)
			    (get symbol 'face-defface-spec))))
	      (when spec
		;; Reset faces created during auto-autoloads loading.
		(reset-face symbol)
		;; And set it according to the spec.
		(face-display-set symbol spec nil))))
	  (face-list))))


(defun make-empty-face (name &optional doc-string temporary)
  "Like `make-face', but doesn't query the resource database."
  (let ((init-face-from-resources nil))
    (make-face name doc-string temporary)))

(defun init-face-from-resources (face &optional locale)
  "Initialize FACE from the resource database.
If LOCALE is specified, it should be a frame, device, or 'global, and
the face will be resourced over that locale.  Otherwise, the face will
be resourced over all possible locales (i.e. all frames, all devices,
and 'global)."
  (cond ((null init-face-from-resources)
	 ;; Do nothing.
	 )
	((not locale)
	 ;; Global, set for all frames.
	 (progn
	   (init-face-from-resources face 'global)
	   (let ((devices (device-list)))
	     (while devices
	       (init-face-from-resources face (car devices))
	       (setq devices (cdr devices))))
	   (let ((frames (frame-list)))
	     (while frames
	       (init-face-from-resources face (car frames))
	       (setq frames (cdr frames))))))
	(t
	 ;; Specific.
	 (let ((devtype (cond ((devicep locale) (device-type locale))
			      ((framep locale) (frame-type locale))
			      (t nil))))
	   (cond ((or (and (not devtype) (featurep 'x)) (eq 'x devtype))
		  (x-init-face-from-resources face locale))
		 ((or (not devtype) (eq 'tty devtype))
		  ;; Nothing to do for TTYs?
		  ))))))

(defun init-device-faces (device)
  ;; First, add any device-local face resources.
  (when init-face-from-resources
    (loop for face in (face-list) do
	  (init-face-from-resources face device))
    ;; Then do any device-specific initialization.
    (cond ((eq 'x (device-type device))
	   (declare-fboundp (x-init-device-faces device)))
	  ;; Nothing to do for TTYs?
	  )
    (or (eq 'stream (device-type device))
	(init-other-random-faces device))))

(defun init-frame-faces (frame)
  (when init-face-from-resources
    ;; First, add any frame-local face resources.
    (loop for face in (face-list) do
	  (init-face-from-resources face frame))
    ;; Then do any frame-specific initialization.
    (cond ((eq 'x (frame-type frame))
	   (declare-fboundp (x-init-frame-faces frame)))
	  ;; Is there anything which should be done for TTY's?
	  )))

;; #### This is somewhat X-specific, and is called when the first
;; X device is created (even if there were TTY devices created
;; beforehand).  The concept of resources has not been generalized
;; outside of X-specificness, so we have to live with this
;; breach of device-independence.

(defun init-global-faces ()
  ;; Look for global face resources.
  (loop for face in (face-list) do
	(init-face-from-resources face 'global))
  ;; Further X frobbing.
  (and (featurep 'x) (declare-fboundp (x-init-global-faces)))

  ;; for bold and the like, make the global specification be bold etc.
  ;; if the user didn't already specify a value.  These will also be
  ;; frobbed further in init-other-random-faces.
  (unless (face-font 'bold 'global)
    (make-face-bold 'bold 'global))
  ;;
  (unless (face-font 'italic 'global)
    (make-face-italic 'italic 'global))
  ;;
  (unless (face-font 'bold-italic 'global)
    (make-face-bold-italic 'bold-italic 'global)
    (unless (face-font 'bold-italic 'global)
      (copy-face 'bold 'bold-italic)
      (make-face-italic 'bold-italic)))

  (when (face-equal 'bold 'bold-italic)
    (copy-face 'italic 'bold-italic)
    (make-face-bold 'bold-italic))
  ;;
  ;; Nothing more to be done for X or TTY's?
  )


;; These warnings are there for a reason.  Just specify your fonts
;; correctly.  Deal with it.  Additionally, one can use
;; `log-warning-minimum-level' instead of this.
;(defvar inhibit-font-complaints nil
;  "Whether to suppress complaints about incomplete sets of fonts.")

(defun face-complain-about-font (face device)
  (if (symbolp face) (setq face (symbol-name face)))
;;  (if (not inhibit-font-complaints)
  ;; complaining for printers is generally annoying.
  (unless (device-printer-p device)
    (display-warning
	'font
      (let ((default-name (face-font-name 'default device)))
	(format "%s: couldn't deduce %s %s version of the font
%S.

Please specify X resources to make the %s face
visually distinguishable from the default face.
For example, you could add one of the following to $HOME/Emacs:

Emacs.%s.attributeFont: -dt-*-medium-i-*
or
Emacs.%s.attributeForeground: hotpink\n"
		invocation-name
		(if (string-match "\\`[aeiouAEIOU]" face) "an" "a")
		face
		default-name
		face
		face
		face
		)))))


;; #### This is quite a mess.  We should use the custom mechanism for
;; most of this stuff.  Currently we don't do it, because Custom
;; doesn't use specifiers (yet.)  FSF does it the Right Way.

;; For instance, the definition of `bold' should be something like
;; (defface bold ((t (:bold t))) "Bold text.") -- and `:bold t' should
;; make sure that everything works properly.

(defun init-other-random-faces (device)
  "Initialize the colors and fonts of the bold, italic, bold-italic,
zmacs-region, list-mode-item-selected, highlight, primary-selection,
secondary-selection, and isearch faces when each device is created.  If
you want to add code to do stuff like this, use the create-device-hook."

  ;; try to make 'bold look different from the default on this device.
  ;; If that doesn't work at all, then issue a warning.
  (unless (face-differs-from-default-p 'bold device)
    (make-face-bold 'bold device)
    (unless (face-differs-from-default-p 'bold device)
      (make-face-unbold 'bold device)
      (unless (face-differs-from-default-p 'bold device)
	;; the luser specified one of the bogus font names
	(face-complain-about-font 'bold device))))

  ;; Similar for italic.
  ;; It's unreasonable to expect to be able to make a font italic all
  ;; the time.  For many languages, italic is an alien concept.
  ;; Basically, because italic is not a globally meaningful concept,
  ;; the use of the italic face should really be obsoleted.

  ;; I disagree with above.  In many languages, the concept of capital
  ;; letters is just as alien, and yet we use them.  Italic is here to
  ;; stay.  -hniksic

  ;; In a Solaris Japanese environment, there just aren't any italic
  ;; fonts - period.  CDE recognizes this reality, and fonts
  ;; -dt-interface user-medium-r-normal-*-*-*-*-*-*-*-*-* don't come
  ;; in italic versions.  So we first try to make the font bold before
  ;; complaining.
  (unless (face-differs-from-default-p 'italic device)
    (make-face-italic 'italic device)
    (unless (face-differs-from-default-p 'italic device)
      (make-face-bold 'italic device)
      (unless (face-differs-from-default-p 'italic device)
	(face-complain-about-font 'italic device))))

  ;; similar for bold-italic.
  (unless (face-differs-from-default-p 'bold-italic device)
    (make-face-bold-italic 'bold-italic device)
    ;; if we couldn't get a bold-italic version, try just bold.
    (unless (face-differs-from-default-p 'bold-italic device)
      (make-face-bold 'bold-italic device)
      ;; if we couldn't get bold or bold-italic, then that's probably because
      ;; the default font is bold, so make the `bold-italic' face be unbold.
      (unless (face-differs-from-default-p 'bold-italic device)
	(make-face-unbold 'bold-italic device)
	(make-face-italic 'bold-italic device)
	(unless (face-differs-from-default-p 'bold-italic device)
	  ;; if that didn't work, try plain italic
	  ;; (can this ever happen? what the hell.)
	  (make-face-italic 'bold-italic device)
	  (unless (face-differs-from-default-p 'bold-italic device)
	    ;; then bitch and moan.
	    (face-complain-about-font 'bold-italic device))))))

  ;; Set the text-cursor colors unless already specified.
  (when (and (not (eq 'tty (device-type device)))
	     (not (face-background 'text-cursor 'global))
	     (face-property-equal 'text-cursor 'default 'background device))
    (set-face-background 'text-cursor [default foreground] 'global
			 nil 'append))
  (when (and (not (eq 'tty (device-type device)))
	     (not (face-foreground 'text-cursor 'global))
	     (face-property-equal 'text-cursor 'default 'foreground device))
    (set-face-foreground 'text-cursor [default background] 'global
			 nil 'append))
  )

;; New function with 20.1, suggested by Per Abrahamsen, coded by Kyle
;; Jones and Hrvoje Niksic.
(defun set-face-stipple (face pixmap &optional frame)
  "Change the stipple pixmap of FACE to PIXMAP.
This is an Emacs compatibility function; consider using
set-face-background-pixmap instead.

PIXMAP should be a string, the name of a file of pixmap data.
The directories listed in the variable `x-bitmap-file-path'
under X is searched.

Alternatively, PIXMAP may be a list of the form (WIDTH HEIGHT
DATA) where WIDTH and HEIGHT are the size in pixels, and DATA is
a string, containing the raw bits of the bitmap.  XBM data is
expected in this case, other types of image data will not work.

If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (while (not (find-face face))
    (setq face (wrong-type-argument 'facep face)))
  (let ((bitmap-path (ecase (console-type)
		       (x         x-bitmap-file-path)))
	instantiator)
    (while
	(null
	 (setq instantiator
	       (cond ((stringp pixmap)
		      (let ((file (if (file-name-absolute-p pixmap)
				      pixmap
				    (locate-file pixmap bitmap-path
						 '(".xbm" "")))))
			(and file
			     `[xbm :file ,file])))
		     ((and (listp pixmap) (= (length pixmap) 3))
		      `[xbm :data ,pixmap])
		     (t nil))))
      ;; We're signaling a continuable error; let's make sure the
      ;; function `stipple-pixmap-p' at least exists.
      (flet ((stipple-pixmap-p (pixmap)
	       (or (stringp pixmap)
		   (and (listp pixmap) (= (length pixmap) 3)))))
	(setq pixmap (signal 'wrong-type-argument
			     (list 'stipple-pixmap-p pixmap)))))
    (check-type frame (or null frame))
    (set-face-background-pixmap face instantiator frame)))


;; Create the remaining standard faces now.  This way, packages that we dump
;; can reference these faces as parents.
;;
;; The default, modeline, left-margin, right-margin, text-cursor,
;; and pointer faces are created in C.

(make-face 'bold "Bold text.")
(make-face 'italic "Italic text.")
(make-face 'bold-italic "Bold-italic text.")
(make-face 'underline "Underlined text.")
(or (face-differs-from-default-p 'underline)
    (set-face-underline-p 'underline t 'global '(default)))
(make-face 'zmacs-region "Used on highlighted region between point and mark.")
(make-face 'isearch "Used on region matched by isearch.")
(make-face 'isearch-secondary "Face to use for highlighting all matches.")
(make-face 'list-mode-item-selected
	   "Face for the selected list item in list-mode.")
(make-face 'highlight "Highlight face.")
(make-face 'primary-selection "Primary selection face.")
(make-face 'secondary-selection "Secondary selection face.")

;; Several useful color faces.
(eval-when-compile (load "cl-macs"))
(dolist (color '(red green blue yellow))
  (make-face color (concat (symbol-name color) " text."))
  (set-face-foreground color (symbol-name color) nil 'color))

;; Make some useful faces.  This happens very early, before creating
;; the first non-stream device.

(set-face-background 'text-cursor
		     '(((x default) . "Red3"))
		     'global)

;; some older X servers don't recognize "darkseagreen2"
(set-face-background 'highlight
		     '(((x default color) . "darkseagreen2")
		       ((x default color) . "green")
		       ((x default grayscale) . "gray53"))
		     'global)
(set-face-background-pixmap 'highlight
			    '(((x default mono) . "gray1"))
			    'global)

(set-face-background 'zmacs-region
		     '(((x default color) . "gray65")
		       ((x default grayscale) . "gray65"))
		     'global)
(set-face-background-pixmap 'zmacs-region
			    '(((x default mono) . "gray3"))
			    'global)

(set-face-background 'list-mode-item-selected
		     '(((x default color) . "gray68")
		       ((x default grayscale) . "gray68")
		       ((x default mono) . [default foreground]))
		     'global)
(set-face-foreground 'list-mode-item-selected
		     '(((x default mono) . [default background]))
		     'global)

(set-face-background 'primary-selection
		     '(((x default color) . "gray65")
		       ((x default grayscale) . "gray65"))
		     'global)
(set-face-background-pixmap 'primary-selection
			    '(((x default mono) . "gray3"))
			    'global)

(set-face-background 'secondary-selection
		     '(((x default color) . "paleturquoise")
		       ((x default color) . "green")
		       ((x default grayscale) . "gray53"))
		     'global)
(set-face-background-pixmap 'secondary-selection
			    '(((x default mono) . "gray1"))
			    'global)

(set-face-background 'isearch
		     '(((x default color) . "paleturquoise")
		       ((x default color) . "green"))
		     'global)

;; #### This should really, I mean *really*, be converted to some form
;; of `defface' one day.
(set-face-foreground 'isearch-secondary
		     '(((x default color) . "red3"))
		     'global)

;; Define some logical color names to be used when reading the pixmap files.
(if (featurep 'xpm)
    (setq xpm-color-symbols
	  (list
	   '("foreground" (face-foreground 'default))
	   '("background" (face-background 'default))
	   '("backgroundToolBarColor"
	     (or
	      (and
	       (featurep 'x)
	       (x-get-resource "backgroundToolBarColor"
			       "BackgroundToolBarColor" 'string
			       nil nil 'warn))

	      (face-background 'toolbar)))
	   '("foregroundToolBarColor"
	     (or
	      (and
	       (featurep 'x)
	       (x-get-resource "foregroundToolBarColor"
			       "ForegroundToolBarColor" 'string
			       nil nil 'warn))
	      (face-foreground 'toolbar)))
	   )))

(when (featurep 'tty)
  (set-face-highlight-p 'bold                    t 'global '(default tty))
  (set-face-underline-p 'italic                  t 'global '(default tty))
  (set-face-highlight-p 'bold-italic             t 'global '(default tty))
  (set-face-underline-p 'bold-italic             t 'global '(default tty))
  (set-face-highlight-p 'highlight               t 'global '(default tty))
  (set-face-reverse-p   'text-cursor             t 'global '(default tty))
  (set-face-reverse-p   'modeline                t 'global '(default tty))
  (set-face-reverse-p   'zmacs-region            t 'global '(default tty))
  (set-face-reverse-p   'primary-selection       t 'global '(default tty))
  (set-face-underline-p 'secondary-selection     t 'global '(default tty))
  (set-face-reverse-p   'list-mode-item-selected t 'global '(default tty))
  (set-face-reverse-p   'isearch                 t 'global '(default tty))
  )

;;; faces.el ends here
