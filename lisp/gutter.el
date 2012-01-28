;;; gutter.el --- Gutter manipulation for SXEmacs.

;; Copyright (C) 1999 Free Software Foundation, Inc.
;; Copyright (C) 1999, 2000 Andy Piper.

;; Maintainer: SXEmacs Development Team
;; Keywords: frames, gui, internal, dumped

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

;; Some of this is taken from the buffer-menu stuff in menubar-items.el
;; and the custom specs in toolbar.el.

(defgroup gutter nil
  "Input from the gutters."
  :group 'environment)

;; Although these customizations appear bogus, they are necessary in
;; order to be able to save options through the options menu.
(defcustom default-gutter-position
  (default-gutter-position)
  "The location of the default gutter. It can be 'top, 'bottom, 'left or
'right. This option should be customized through the options menu.
To set the gutter position explicitly use `set-default-gutter-position'"
  :group 'gutter
  :type '(choice (const :tag "top" top)
		 (const :tag "bottom" bottom)
		 (const :tag "left" left)
		 (const :tag "right" right))
  :set #'(lambda (var val)
	   (set-default-gutter-position val)
	   (setq default-gutter-position val)))

;;; Gutter helper functions

;; called by Fset_default_gutter_position()
(defvar default-gutter-position-changed-hook nil
  "Function or functions to be called when the gutter position is changed.
The value of this variable may be buffer-local.")

;; called by set-gutter-element-visible-p
(defvar gutter-element-visibility-changed-hook nil
  "Function or functions to be called when the visibility of an
element in the gutter changes.  The value of this variable may be
buffer-local. The gutter element symbol is passed as an argument to
the hook, as is the visibility flag.")

(defun set-gutter-element (gutter-specifier prop value &optional locale tag-set)
  "Set GUTTER-SPECIFIER gutter element PROP to VALUE in optional LOCALE.
This is a convenience function for setting gutter elements.
VALUE in general must be a string. If VALUE is a glyph then a string
will be created to put the glyph into."
  (let ((spec value))
    (when (glyphp value)
      (setq spec (copy-sequence "\n"))
      (set-extent-begin-glyph (make-extent 0 1 spec) value))
    (map-extents #'(lambda (extent arg)
		     (set-extent-property extent 'duplicable t)) spec)
    (modify-specifier-instances gutter-specifier #'plist-put (list prop spec)
				'force nil locale tag-set)))

(defun remove-gutter-element (gutter-specifier prop &optional locale tag-set)
  "Remove gutter element PROP from GUTTER-SPECIFIER in optional LOCALE.
This is a convenience function for removing gutter elements."
  (modify-specifier-instances gutter-specifier #'plist-remprop (list prop)
			      'force nil locale tag-set))

(defun set-gutter-element-visible-p (gutter-visible-specifier-p
				     prop &optional visible-p
				     locale tag-set)
  "Change the visibility of gutter elements.
Set the visibility of element PROP to VISIBLE-P for
GUTTER-SPECIFIER-VISIBLE-P in optional LOCALE.
This is a convenience function for hiding and showing gutter elements."
  (modify-specifier-instances
   gutter-visible-specifier-p #'(lambda (spec prop visible-p)
				  (if (consp spec)
				      (if visible-p
					  (if (memq prop spec) spec
					    (cons prop spec))
					(delq prop spec))
				    (if visible-p (list prop))))
   (list prop visible-p)
   'force nil locale tag-set)
  (run-hook-with-args 'gutter-element-visibility-changed-hook prop visible-p))

(defun gutter-element-visible-p (gutter-visible-specifier-p
				 prop &optional domain)
  "Determine whether a gutter element is visible.
Given GUTTER-VISIBLE-SPECIFIER-P and gutter element PROP, return
non-nil if it is visible in optional DOMAIN."
  (let ((spec (specifier-instance gutter-visible-specifier-p domain)))
    (or (and (listp spec) (memq 'buffers-tab spec))
	spec)))

(defun set-gutter-dirty-p (gutter-or-location)
  "Make GUTTER-OR-LOCATION dirty to force redisplay updates."
  ;; set-glyph-image will not make the gutter dirty
  (when (or (gutter-specifier-p gutter-or-location)
	    (eq gutter-or-location 'top)
	    (eq gutter-or-location 'bottom)
	    (eq gutter-or-location 'left)
	    (eq gutter-or-location 'right))
    (or (gutter-specifier-p gutter-or-location)
	(setq gutter-or-location
	      (eval (intern (concat
			     (symbol-name gutter-or-location)
			     "-gutter")))))
    (set-specifier-dirty-flag gutter-or-location)))

(defun make-gutter-specifier (spec-list)
  "Return a new `gutter' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Gutter specifiers are used to specify the format of a gutter.
The values of the variables `default-gutter', `top-gutter',
`left-gutter', `right-gutter', and `bottom-gutter' are always
gutter specifiers.

Valid gutter instantiators are called \"gutter descriptors\" and are
either strings or property-lists of strings.  See `default-gutter' for
a description of the exact format."
  (make-specifier-and-init 'gutter spec-list))

(defun make-gutter-size-specifier (spec-list)
  "Return a new `gutter-size' specifier object with the given spec list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Gutter-size specifiers are used to specify the size of a gutter.  The
values of the variables `default-gutter-size', `top-gutter-size',
`left-gutter-size', `right-gutter-size', and `bottom-gutter-size' are
always gutter-size specifiers.

Valid gutter-size instantiators are either integers or the special
symbol 'autodetect. If a gutter-size is set to 'autodetect them the
size of the gutter will be adjusted to just accommodate the gutters
contents. 'autodetect only works for top and bottom gutters."
  (make-specifier-and-init 'gutter-size spec-list))

(defun make-gutter-visible-specifier (spec-list)
  "Return a new `gutter-visible' specifier object with the given spec list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Gutter-visible specifiers are used to specify the visibility of a
gutter.  The values of the variables `default-gutter-visible-p',
`top-gutter-visible-p', `left-gutter-visible-p',
`right-gutter-visible-p', and `bottom-gutter-visible-p' are always
gutter-visible specifiers.

Valid gutter-visible instantiators are t, nil or a list of symbols.
If a gutter-visible instantiator is set to a list of symbols, and the
corresponding gutter specification is a property-list strings, then
elements of the gutter specification will only be visible if the
corresponding symbol occurs in the gutter-visible instantiator."
  (make-specifier-and-init 'gutter-visible spec-list))

(defun init-gutter ()
  "Initialize the gutter."
  ;; do nothing as yet.
  )

;;; gutter.el ends here.
