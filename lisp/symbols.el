;;; symbols.el --- functions for working with symbols and symbol values

;; Copyright (C) 1996 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: internal

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

;; Not yet dumped into SXEmacs.

;; The idea behind magic variables is that you can specify arbitrary
;; behavior to happen when setting or retrieving a variable's value.  The
;; purpose of this is to make it possible to cleanly provide support for
;; obsolete variables (e.g. unread-command-event, which is obsolete for
;; unread-command-events) and variable compatibility
;; (e.g. suggest-key-bindings, the FSF equivalent of
;; teach-extended-commands-p and teach-extended-commands-timeout).

;; There are a large number of functions pertaining to a variable's
;; value:

;; boundp
;; globally-boundp
;; makunbound
;; symbol-value
;; set / setq
;; default-boundp
;; default-value
;; set-default / setq-default
;; make-variable-buffer-local
;; make-local-variable
;; kill-local-variable
;; kill-console-local-variable
;; symbol-value-in-buffer
;; symbol-value-in-console
;; local-variable-p / local-variable-if-set-p

;; Plus some "meta-functions":

;; defvaralias
;; variable-alias
;; indirect-variable

;; I wanted an implementation that:

;; -- would work with all the above functions, but (a) didn't require
;;    a separate handler for every function, and (b) would work OK
;;    even if more functions are added (e.g. `set-symbol-value-in-buffer'
;;    or `makunbound-default') or if more arguments are added to a
;;    function.
;; -- avoided consing if at all possible.
;; -- didn't slow down operations on non-magic variables (therefore,
;;    storing the magic information using `put' is ruled out).
;;

;;; Code:

;; perhaps this should check whether the functions are bound, so that
;; some handlers can be unspecified.  That requires that all functions
;; are defined before `define-magic-variable-handlers' is called,
;; though.

;; perhaps there should be something that combines
;; `define-magic-variable-handlers' with `defvaralias'.

(globally-declare-fboundp
 '(set-magic-variable-handler))

(defun define-magic-variable-handlers (variable handler-class harg)
  "Set the magic variable handles for VARIABLE to those in HANDLER-CLASS.
HANDLER-CLASS should be a symbol.  The handlers are constructed by adding
the handler type to HANDLER-CLASS.  HARG is passed as the HARG value for
each of the handlers."
  (mapcar
   #'(lambda (htype)
       (set-magic-variable-handler variable htype
				   (intern (concat (symbol-value handler-class)
						   "-"
						   (symbol-value htype)))
				   harg))
   '(get-value set-value other-predicate other-action)))

;; unread-command-event

(defun mvh-first-of-list-get-value (sym fun args harg)
  (car (apply fun harg args)))

(defun mvh-first-of-list-set-value (sym value setfun getfun args harg)
  (apply setfun harg (cons value (apply getfun harg args)) args))

(defun mvh-first-of-list-other-predicate (sym fun args harg)
  (apply fun harg args))

(defun mvh-first-of-list-other-action (sym fun args harg)
  (apply fun harg args))

(define-magic-variable-handlers 'unread-command-event
  'mvh-first-of-list
  'unread-command-events)

;; last-command-char, last-input-char, unread-command-char

(defun mvh-char-to-event-get-value (sym fun args harg)
  (event-to-character (apply fun harg args)))

(defun mvh-char-to-event-set-value (sym value setfun getfun args harg)
  (let ((event (apply getfun harg args)))
       (if (event-live-p event)
	   nil
	 (setq event (make-event))
	 (apply setfun harg event args))
       (character-to-event value event)))

(defun mvh-char-to-event-other-predicate (sym fun args harg)
  (apply fun harg args))

(defun mvh-char-to-event-other-action (sym fun args harg)
  (apply fun harg args))

(define-magic-variable-handlers 'last-command-char
  'mvh-char-to-event
  'last-command-event)

(define-magic-variable-handlers 'last-input-char
  'mvh-char-to-event
  'last-input-event)

(define-magic-variable-handlers 'unread-command-char
  'mvh-char-to-event
  'unread-command-event)

;; suggest-key-bindings

(set-magic-variable-handler
 'suggest-key-bindings 'get-value
 #'(lambda (sym fun args harg)
     (and (apply fun 'teach-extended-commands-p args)
	  (apply fun 'teach-extended-commands-timeout args))))

(set-magic-variable-handler
 'suggest-key-bindings 'set-value
 #'(lambda (sym value setfun getfun args harg)
     (apply setfun 'teach-extended-commands-p (not (null value)) args)
     (if value
	 (apply 'teach-extended-commands-timeout
	       (if (numberp value) value 2) args))))

(set-magic-variable-handler
 'suggest-key-bindings 'other-action
 #'(lambda (sym fun args harg)
     (apply fun 'teach-extended-commands-p args)
     (apply fun 'teach-extended-commands-timeout args)))

(set-magic-variable-handler
 'suggest-key-bindings 'other-predicate
 #'(lambda (sym fun args harg)
     (and (apply fun 'teach-extended-commands-p args)
	  (apply fun 'teach-extended-commands-timeout args))))

;;; symbols.el ends here
