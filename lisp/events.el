;;; events.el --- event functions for XEmacs.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996-7 Sun Microsystems, Inc.
;; Copyright (C) 1996 Ben Wing.

;; Maintainer: Martin Buchholz
;; Keywords: internal, event, dumped

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


(defun event-console (event)
  "Return the console that EVENT occurred on.
This will be nil for some types of events (e.g. eval events)."
  (cdfw-console (event-channel event)))

(defun event-device (event)
  "Return the device that EVENT occurred on.
This will be nil for some types of events (e.g. keyboard and eval events)."
  (dfw-device (event-channel event)))

(defun event-frame (event)
  "Return the frame that EVENT occurred on.
This will be nil for some types of events (e.g. keyboard and eval events)."
  (fw-frame (event-channel event)))

(defun event-buffer (event)
  "Return the buffer of the window over which mouse event EVENT occurred.
Return nil unless both (mouse-event-p EVENT) and
(event-over-text-area-p EVENT) are non-nil."
  (let ((window (event-window event)))
    (and (windowp window) (window-buffer window))))

(defalias 'allocate-event 'make-event)


(defun key-press-event-p (object)
  "Return t if OBJECT is a key-press event."
  (and (event-live-p object) (eq 'key-press (event-type object))))

(defun button-press-event-p (object)
  "Return t if OBJECT is a mouse button-press event."
  (and (event-live-p object) (eq 'button-press (event-type object))))

(defun button-release-event-p (object)
  "Return t if OBJECT is a mouse button-release event."
  (and (event-live-p object) (eq 'button-release (event-type object))))

(defun button-event-p (object)
  "Return t if OBJECT is a mouse button-press or button-release event."
  (and (event-live-p object)
       (memq (event-type object) '(button-press button-release))
       t))

(defun motion-event-p (object)
  "Return t if OBJECT is a mouse motion event."
  (and (event-live-p object) (eq 'motion (event-type object))))

(defun mouse-event-p (object)
  "Return t if OBJECT is a mouse button-press, button-release or motion event."
  (and (event-live-p object)
       (memq (event-type object) '(button-press button-release motion))
       t))

(defun process-event-p (object)
  "Return t if OBJECT is a process-output event."
  (and (event-live-p object) (eq 'process (event-type object))))

(defun timeout-event-p (object)
  "Return t if OBJECT is a timeout event."
  (and (event-live-p object) (eq 'timeout (event-type object))))

(defun eval-event-p (object)
  "Return t if OBJECT is an eval event."
  (and (event-live-p object) (eq 'eval (event-type object))))

(defun misc-user-event-p (object)
  "Return t if OBJECT is a misc-user event.
A misc-user event is a user event that is not a keypress or mouse click;
normally this means a menu selection or scrollbar action."
  (and (event-live-p object) (eq 'misc-user (event-type object))))

;; You could just as easily use event-glyph but we include this for
;; consistency.

(defun event-over-glyph-p (object)
  "Return t if OBJECT is a mouse event occurring over a glyph.
Mouse events are events of type button-press, button-release or motion."
  (and (event-live-p object) (event-glyph object) t))

(defun keyboard-translate (&rest pairs)
  "Translate character or keysym FROM to TO at a low level.
Multiple FROM-TO pairs may be specified.

See `keyboard-translate-table' for more information."
  (while pairs
    (puthash (pop pairs) (pop pairs) keyboard-translate-table)))

(put 'tab       'ascii-character ?\t)
(put 'linefeed  'ascii-character ?\n)
(put 'clear     'ascii-character 12)
(put 'return    'ascii-character ?\r)
(put 'escape    'ascii-character ?\e)
(put 'space	'ascii-character ? )

 ;; Do the same voodoo for the keypad keys.  I used to bind these to keyboard
 ;; macros (for instance, kp-0 was bound to "0") so that they would track the
 ;; bindings of the corresponding keys by default, but that made the display
 ;; of M-x describe-bindings much harder to read, so now we'll just bind them
 ;; to self-insert by default.  Not a big difference...

(put 'kp-0 'ascii-character ?0)
(put 'kp-1 'ascii-character ?1)
(put 'kp-2 'ascii-character ?2)
(put 'kp-3 'ascii-character ?3)
(put 'kp-4 'ascii-character ?4)
(put 'kp-5 'ascii-character ?5)
(put 'kp-6 'ascii-character ?6)
(put 'kp-7 'ascii-character ?7)
(put 'kp-8 'ascii-character ?8)
(put 'kp-9 'ascii-character ?9)

(put 'kp-space     'ascii-character ? )
(put 'kp-tab       'ascii-character ?\t)
(put 'kp-enter     'ascii-character ?\r)
(put 'kp-equal     'ascii-character ?=)
(put 'kp-multiply  'ascii-character ?*)
(put 'kp-add       'ascii-character ?+)
(put 'kp-separator 'ascii-character ?,)
(put 'kp-subtract  'ascii-character ?-)
(put 'kp-decimal   'ascii-character ?.)
(put 'kp-divide    'ascii-character ?/)

;;; events.el ends here
