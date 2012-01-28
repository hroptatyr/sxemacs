;;; extents.el --- miscellaneous extent functions not written in C

;; Copyright (C) 1993-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2000 Ben Wing.

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Authorship:

;; Created 1995 Ben Wing.
;; mapcar-extents (and extent-list?) from stig@hackvan.com, c. 1996.

;;; Code:

;; an alternative to map-extents.
(defun mapcar-extents (function &optional predicate buffer-or-string from to
				flags property value)
  "Apply FUNCTION to all extents which overlap a region in BUFFER-OR-STRING.
The region is delimited by FROM and TO.  FUNCTION is called with
one argument, the extent.  A list of the values returned by FUNCTION
is returned.  An optional PREDICATE may be used to further limit the
extents over which FUNCTION is mapped.  The optional arguments FLAGS,
PROPERTY, and VALUE may also be used to control the extents passed to
PREDICATE or FUNCTION.  See also `map-extents'."
  (let (*result*)
    (map-extents (if predicate
		     #'(lambda (ex junk)
			 (and (funcall predicate ex)
			      (setq *result* (cons (funcall function ex)
						   *result*)))
			 nil)
		   #'(lambda (ex junk)
			 (setq *result* (cons (funcall function ex)
					      *result*))
			 nil))
		 buffer-or-string from to nil flags property value)
    (nreverse *result*)))

(defun extent-list (&optional buffer-or-string from to flags property value)
  "Return a list of the extents in BUFFER-OR-STRING.
BUFFER-OR-STRING defaults to the current buffer if omitted.
FROM and TO can be used to limit the range over which extents are
returned; if omitted, all extents in the buffer or string are returned.

More specifically, if a range is specified using FROM and TO, only
extents that overlap the range (i.e. begin or end inside of the range)
are included in the list.  FROM and TO default to the beginning and
end of BUFFER-OR-STRING, respectively.

FLAGS controls how end cases are treated.  For a discussion of this,
and exactly what ``overlap'' means, see `map-extents'.  PROPERTY and VALUE
are also as in `map-extents'.

If you want to map a function over the extents in a buffer or string,
consider using `map-extents' or `mapcar-extents' instead.

See also `extents-at'."
  (mapcar-extents 'identity nil buffer-or-string from to flags property value))

(defun extent-at-event (event &optional property before at-flag)
  "Return the smallest extent under EVENT, if any.
PROPERTY, BEFORE, and AT-FLAG are as in `extent-at'."
  (let* ((win (event-window event))
	 (p (event-point event)))
    (and win p (extent-at p (window-buffer win) property before at-flag))))

(defun extents-at-event (event &optional property before at-flag)
  "Return a list of all extents under EVENT.
PROPERTY, BEFORE, and AT-FLAG are as in `extent-at'."
  (let* ((win (event-window event))
	 (p (event-point event)))
    (and win p (extents-at p (window-buffer win) property before at-flag))))

(defun extent-string (extent)
  "Return the string delimited by the bounds of EXTENT."
  (let ((object (extent-object extent)))
    (if (bufferp object)
	(buffer-substring (extent-start-position extent)
			  (extent-end-position extent)
			  object)
      (substring object
		 (extent-start-position extent)
		 (extent-end-position extent)))))

(defun extent-descendants (extent)
  "Return a list of all descendants of EXTENT, including EXTENT.
This recursively applies `extent-children' to any children of
EXTENT, until no more children can be found."
  (let ((children (extent-children extent)))
    (if children
	(apply 'nconc (mapcar 'extent-descendants children))
      (list extent))))

(defun set-extent-keymap (extent keymap)
  "Set EXTENT's `keymap' property to KEYMAP."
  (set-extent-property extent 'keymap keymap))

(defun extent-keymap (extent)
  "Return EXTENT's `keymap' property."
  (extent-property extent 'keymap))

;;; extents.el ends here
