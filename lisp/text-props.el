;;; text-props.el --- implements properties of characters

;; Copyright (C) 1993-4, 1997  Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Ben Wing.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, wp, faces, dumped

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

;; This is a nearly complete implementation of the FSF19 text properties API.
;; Please let me know if you notice any differences in behavior between
;; this implementation and the FSF implementation.

;; However, keep in mind that this interface has been implemented because it
;; is useful.  Compatibility with code written for FSF19 is a secondary goal
;; to having a clean and useful interface.

;; The cruftier parts of the FSF API, such as the special handling of
;; properties like `mouse-face', `front-sticky', and other properties whose
;; value is a list of names of *other* properties set at this position, are
;; not implemented.  The reason for this is that if you feel you need that
;; kind of functionality, it's a good hint that you should be using extents
;; instead of text properties.

;; When should I use Text Properties, and when should I use Extents?
;; ==================================================================

;; If you are putting a `button' or `hyperlink' of some kind into a buffer,
;; the most natural interface is one which deals with properties of regions
;; with explicit endpoints that behave more-or-less like markers.  That is
;; what `make-extent', `extent-at', and `extent-property' are for.

;; If you are dealing with styles of text, where things do not have explicit
;; endpoints (as is done in font-lock.el and shell-font.el) or if you want to
;; partition a buffer (that is, change some attribute of a range from one
;; value to another without disturbing the properties outside of that range)
;; then an interface that deals with properties of characters may be most
;; natural.

;; Another way of thinking of it is, do you care where the endpoints of the
;; region are?  If you do, then you should use extents.  If it's ok for the
;; region to become divided, and for two regions with identical properties to
;; be merged into one region, then you might want to use text properties.

;; Some applications want the attributes they add to be copied by the killing
;; and yanking commands, and some do not.  This is orthogonal to whether text
;; properties or extents are used.  Remember that text properties are
;; implemented in terms of extents, so anything you can do with one you can
;; do with the other.  It's just a matter of which way of creating and
;; managing them is most appropriate to your application.

;; Implementation details:
;; =======================

;; This package uses extents with a non-nil 'text-prop property.  It assumes
;; free reign over the endpoints of any extent with that property.  It will
;; not alter any extent which does not have that property.

;; Right now, the text-property functions create one extent for each distinct
;; property; that is, if a range of text has two text-properties on it, there
;; will be two extents.  As the set of text-properties is going to be small,
;; this is probably not a big deal.  It would be possible to share extents.

;; One tricky bit is that undo/kill/yank must be made to not fragment things:
;; these extents must not be allowed to overlap.  We accomplish this by using
;; a custom `paste-function' property on the extents.

;; shell-font.el and font-lock.el could put-text-property to attach fonts to
;; the buffer.  However, what these packages are interested in is the
;; efficient extent partitioning behavior which this code exhibits, not the
;; duplicability aspect of it.  In fact, either of these packages could be
;; implemented by creating a one-character non-expandable extent for each
;; character in the buffer, except that that would be extremely wasteful of
;; memory.  (Redisplay performance would be fine, however.)

;; If these packages were to use put-text-property to make the extents, then
;; when one copied text from a shell buffer or a font-locked source buffer
;; and pasted it somewhere else (a sendmail buffer, or a buffer not in
;; font-lock mode) then the fonts would follow, and there's no easy way to
;; get rid of them (other than pounding out a call to put-text-property by
;; hand.)  This is annoying.  Maybe it wouldn't be so annoying if there was a
;; more general set of commands for handling styles of text (in fact, if
;; there were such a thing, copying the fonts would probably be exactly what
;; one wanted) but we aren't there yet.  So these packages use the interface
;; of `put-nonduplicable-text-property' which is the same, except that it
;; doesn't make duplicable extents.

;; `put-text-property' and `put-nonduplicable-text-property' don't get along:
;; they will interfere with each other, reusing each others' extents without
;; checking that the "duplicableness" is correct.  This is a bug, but it's
;; one that I don't care enough to fix this right now.

;;; Code:

(defun set-text-properties (start end props &optional buffer-or-string)
  "You should NEVER use this function.  It is ideologically blasphemous.
It is provided only to ease porting of broken FSF Emacs programs.
Instead, use `remove-text-properties' to remove the specific properties
you do not want.

Completely replace properties of text from START to END.
The third argument PROPS is the new property list.
The optional fourth argument, BUFFER-OR-STRING,
is the string or buffer containing the text."
  (map-extents #'(lambda (extent ignored)
		   ;; #### dmoore - shouldn't this use
		   ;; (extent-start-position extent)
		   ;; (extent-end-position extent)
		   (remove-text-properties start end
					   (list (extent-property extent
								  'text-prop)
						 nil)
					   buffer-or-string)
		   nil)
	       buffer-or-string start end nil nil 'text-prop)
  (add-text-properties start end props buffer-or-string))


;;; The following functions can probably stay in lisp, since they're so simple.

;(defun get-text-property (pos prop &optional buffer)
;  "Returns the value of the PROP property at the given position."
;  (let ((e (extent-at pos buffer prop)))
;    (if e
;	(extent-property e prop)
;      nil)))

(defun extent-properties-at-1 (position buffer-or-string text-props-only)
  (let ((extent nil)
	(props nil)
	new-props)
    (while (setq extent (extent-at position buffer-or-string
				   (if text-props-only 'text-prop nil)
				   extent))
      (if text-props-only
	  ;; Only return the one prop which the `text-prop' property points at.
	  (let ((prop (extent-property extent 'text-prop)))
	    (setq new-props (list prop (extent-property extent prop))))
	;; Return all the properties...
	(setq new-props (extent-properties extent))
	;; ...but!  Don't return the `begin-glyph' or `end-glyph' properties
	;; unless the position is exactly at the appropriate endpoint.  Yeah,
	;; this is kind of a kludge.
	;; #### Bug, this doesn't work for end-glyphs (on end-open extents)
	;; because we've already passed the extent with the glyph by the time
	;; it's appropriate to return the glyph.  We could return the end
	;; glyph one character early I guess...  But then next-property-change
	;; would have to stop one character early as well.  It could back up
	;; when it hit an end-glyph...
	;; #### Another bug, if there are multiple glyphs at the same position,
	;; we only see the first one.
	(cond ((or (extent-begin-glyph extent) (extent-end-glyph extent))
	       (if (/= position (if (extent-property extent 'begin-glyph)
				    (extent-start-position extent)
				  (extent-end-position extent)))
		   (let ((rest new-props)
			 prev)
		     (while rest
		       (cond ((or (eq (car rest) 'begin-glyph)
				  (eq (car rest) 'end-glyph))
			      (if prev
				  (setcdr prev (cdr (cdr rest)))
				(setq new-props (cdr (cdr new-props))))
			      (setq rest nil)))
		       (setq prev rest
			     rest (cdr rest))))))))
      (cond ((null props)
	     (setq props new-props))
	    (t
	     (while new-props
	       (or (getf props (car new-props))
		   (setq props (cons (car new-props)
				     (cons (car (cdr new-props))
					   props))))
	       (setq new-props (cdr (cdr new-props)))))))
    props))

(defun extent-properties-at (position &optional object)
  "Return the properties of the character at the given position in OBJECT.
OBJECT is either a string or a buffer. The properties of overlapping
extents are merged.  The returned value is a property list, some of
which may be shared with other structures.  You must not modify it.

If POSITION is at the end of OBJECT, the value is nil.

This returns all properties on all extents.
See also `text-properties-at'."
  (extent-properties-at-1 position object nil))

(defun text-properties-at (position &optional object)
  "Return the properties of the character at the given position in OBJECT.
OBJECT is either a string or a buffer. The properties of overlapping
extents are merged.  The returned value is a property list, some of
which may be shared with other structures.  You must not modify it.

If POSITION is at the end of OBJECT, the value is nil.

This returns only those properties added with `put-text-property'.
See also `extent-properties-at'."
  (extent-properties-at-1 position object t))

(defun text-property-any (start end prop value &optional buffer-or-string)
  "Check text from START to END to see if PROP is ever `eq' to VALUE.
If so, return the position of the first character whose PROP is `eq'
to VALUE.  Otherwise return nil.
The optional fifth argument, BUFFER-OR-STRING, is the buffer or string
containing the text and defaults to the current buffer."
  (while (and start (< start end)
	      (not (eq value (get-text-property start prop buffer-or-string))))
    (setq start (next-single-property-change start prop buffer-or-string end)))
  ;; we have to insert a special check for end due to the illogical
  ;; definition of next-single-property-change (blame FSF for this).
  (if (and start (>= start end)) nil start))

(defun text-property-not-all (start end prop value &optional buffer-or-string)
  "Check text from START to END to see if PROP is ever not `eq' to VALUE.
If so, return the position of the first character whose PROP is not
`eq' to VALUE.  Otherwise, return nil.
The optional fifth argument, BUFFER-OR-STRING, is the buffer or string
containing the text and defaults to the current buffer."
  (if (not (eq value (get-text-property start prop buffer-or-string)))
      start
    (let ((retval (next-single-property-change start prop
					       buffer-or-string end)))
      ;; we have to insert a special check for end due to the illogical
      ;; definition of previous-single-property-change (blame FSF for this).
      (if (and retval (>= retval end)) nil retval))))

;; Older versions that only work sometimes (when VALUE is non-nil
;; for text-property-any, and maybe only when VALUE is nil for
;; text-property-not-all).  They might be faster in those cases,
;; but that's not obvious.

;(defun text-property-any (start end prop value &optional buffer)
;  "Check text from START to END to see if PROP is ever `eq' to VALUE.
;If so, return the position of the first character whose PROP is `eq'
;to VALUE.  Otherwise return nil."
;  ;; #### what should (text-property-any x y 'foo nil) return when there
;  ;; is no foo property between x and y?  Either t or nil seems sensible,
;  ;; since a character with a property of nil is indistinguishable from
;  ;; a character without that property set.
;  (map-extents
;   #'(lambda (e ignore)
;       (if (eq value (extent-property e prop))
;	   ;; return non-nil to stop mapping
;	   (max start (extent-start-position e))
;	 nil))
;   nil start end buffer))
;
;(defun text-property-not-all (start end prop value &optional buffer)
;  "Check text from START to END to see if PROP is ever not `eq' to VALUE.
;If so, return the position of the first character whose PROP is not
;`eq' to VALUE.  Otherwise, return nil."
;  (let (maxend)
;    (map-extents
;     #'(lambda (e ignore)
;	 ;;### no, actually, this is harder.  We need to collect all props
;	 ;; for a given character, and then determine whether no extent
;	 ;; contributes the given value.  Doing this without consing lots
;	 ;; of lists is the tricky part.
;	 (if (eq value (extent-property e prop))
;	     (progn
;	       (setq maxend (extent-end-position e))
;	       nil)
;	   (max start maxend)))
;     nil start end buffer)))

(defun next-property-change (pos &optional buffer-or-string limit)
  "Return the position of next property change.
Scans forward from POS in BUFFER-OR-STRING (defaults to the current buffer)
 until it finds a change in some text property, then returns the position of
 the change.
Returns nil if the properties remain unchanged all the way to the end.
If the value is non-nil, it is a position greater than POS, never equal.
If the optional third argument LIMIT is non-nil, don't search
 past position LIMIT; return LIMIT if nothing is found before LIMIT.
If two or more extents with conflicting non-nil values for a property overlap
 a particular character, it is undefined which value is considered to be
 the value of the property. (Note that this situation will not happen if
 you always use the text-property primitives.)"
  (let ((limit-was-nil (null limit)))
    (or limit (setq limit (if (bufferp buffer-or-string)
			      (point-max buffer-or-string)
			    (length buffer-or-string))))
    (let ((value (extent-properties-at pos buffer-or-string)))
      (while
	  (and (< (setq pos (next-extent-change pos buffer-or-string)) limit)
	       (plists-eq value (extent-properties-at pos buffer-or-string)))))
    (if (< pos limit) pos
      (if limit-was-nil nil
	limit))))

(defun previous-property-change (pos &optional buffer-or-string limit)
  "Return the position of previous property change.
Scans backward from POS in BUFFER-OR-STRING (defaults to the current buffer)
 until it finds a change in some text property, then returns the position of
 the change.
Returns nil if the properties remain unchanged all the way to the beginning.
If the value is non-nil, it is a position less than POS, never equal.
If the optional third argument LIMIT is non-nil, don't search back
 past position LIMIT; return LIMIT if nothing is found until LIMIT.
If two or more extents with conflicting non-nil values for a property overlap
 a particular character, it is undefined which value is considered to be
 the value of the property. (Note that this situation will not happen if
 you always use the text-property primitives.)"
  (let ((limit-was-nil (null limit)))
    (or limit (setq limit (if (bufferp buffer-or-string)
			      (point-min buffer-or-string)
			    0)))
    (let ((value (extent-properties-at (1- pos) buffer-or-string)))
      (while
	  (and (> (setq pos (previous-extent-change pos buffer-or-string))
		  limit)
	       (plists-eq value (extent-properties-at (1- pos)
						      buffer-or-string)))))
    (if (> pos limit) pos
      (if limit-was-nil nil
	limit))))

(defun text-property-bounds (pos prop &optional object at-flag)
  "Return the bounds of property PROP at POS.
This returns a cons (START . END) of the largest region of text containing
POS which has a non-nil value for PROP.  The return value is nil if POS
does not have a non-nil value for PROP.  OBJECT specifies the buffer
or string to search in.  Optional arg AT-FLAG controls what \"at POS\"
means, and has the same meaning as for `extent-at'."
  (or object (setq object (current-buffer)))
  (and (get-char-property pos prop object at-flag)
       (let ((begin (if (stringp object) 0 (point-min object)))
	     (end (if (stringp object) (length object) (point-max object))))
	 (cons (previous-single-property-change (1+ pos) prop object begin)
	       (next-single-property-change pos prop object end)))))

(defun next-text-property-bounds (count pos prop &optional object)
  "Return the COUNTth bounded property region of property PROP after POS.
If COUNT is less than zero, search backwards.  This returns a cons
\(START . END) of the COUNTth maximal region of text that begins after POS
\(starts before POS) and has a non-nil value for PROP.  If there aren't
that many regions, nil is returned.  OBJECT specifies the buffer or
string to search in."
  (or object (setq object (current-buffer)))
  (let ((begin (if (stringp object) 0 (point-min object)))
	(end (if (stringp object) (length object) (point-max object))))
    (catch 'hit-end
      (if (> count 0)
	  (progn
	    (while (> count 0)
	      (if (>= pos end)
		  (throw 'hit-end nil)
		(and (get-char-property pos prop object)
		     (setq pos (next-single-property-change pos prop
							    object end)))
		(setq pos (next-single-property-change pos prop object end)))
	      (setq count (1- count)))
	    (and (< pos end)
		 (cons pos (next-single-property-change pos prop object end))))
	(while (< count 0)
	  (if (<= pos begin)
	      (throw 'hit-end nil)
	    (and (get-char-property (1- pos) prop object)
		 (setq pos (previous-single-property-change pos prop
							    object begin)))
	    (setq pos (previous-single-property-change pos prop object
						       begin)))
	  (setq count (1+ count)))
	(and (> pos begin)
	     (cons (previous-single-property-change pos prop object begin)
		   pos))))))

;(defun detach-all-extents (&optional buffer)
;  (map-extents #'(lambda (x i) (detach-extent x) nil)
;	       buffer))


(provide 'text-props)

;;; text-props.el ends here
