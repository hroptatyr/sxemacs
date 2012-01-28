;;; disass.el --- disassembler for compiled Emacs Lisp code

;;; Copyright (C) 1986, 1991-1994 Free Software Foundation, Inc.

;; Author: Doug Cutting <doug@csli.stanford.edu>
;;	Jamie Zawinski <jwz@jwz.org>
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

;;; Synched up with: FSF 19.28.

;;; Commentary:

;; The single entry point, `disassemble', disassembles a code object generated
;; by the Emacs Lisp byte-compiler.  This doesn't invert the compilation
;; operation, not by a long shot, but it's useful for debugging.

;;
;; Original version by Doug Cutting (doug@csli.stanford.edu)
;; Substantially modified by Jamie Zawinski for
;; the new lapcode-based byte compiler.

;;; Code:

(require 'byte-optimize)

(defvar disassemble-column-1-indent 8 "*")
(defvar disassemble-column-2-indent 10 "*")
(defvar disassemble-recursive-indent 3 "*")

;;;###autoload
(defun disassemble (object &optional buffer indent interactive-p)
  "Print disassembled code for OBJECT in (optional) BUFFER.
OBJECT can be a symbol defined as a function, or a function itself
\(a lambda expression or a compiled-function object).
If OBJECT is not already compiled, we compile it, but do not
redefine OBJECT if it is a symbol."
  (interactive (list (intern (completing-read "Disassemble function: "
					      obarray 'fboundp t))
		     nil 0 t))
  (if (eq (car-safe object) 'byte-code)
      (setq object (list 'lambda () object)))
  (or indent (setq indent 0))		;Default indent to zero
  (save-excursion
    (if (or interactive-p (null buffer))
	(with-output-to-temp-buffer "*Disassemble*"
	  (set-buffer "*Disassemble*")
	  (disassemble-internal object indent (not interactive-p)))
      (set-buffer buffer)
      (disassemble-internal object indent nil)))
  nil)


(defun disassemble-internal (obj indent interactive-p)
  (let ((macro nil)
	(name nil)
	args)
    (while (symbolp obj)
      (setq name obj
	    obj (symbol-function obj)))
    (if (subrp obj)
	(error "Can't disassemble #<subr %s>" name))
    (if (eq (car-safe obj) 'autoload)
	(progn
	  (load (elt obj 1))
	  (setq obj (symbol-function name))))
    (if (eq (car-safe obj) 'macro)	;handle macros
	(setq macro t
	      obj (cdr obj)))
    (if (and (listp obj) (eq (car obj) 'byte-code))
	(setq obj (list 'lambda nil obj)))
    (if (and (listp obj) (not (eq (car obj) 'lambda)))
	(error "not a function"))
    (if (consp obj)
	(if (assq 'byte-code obj)
	    nil
	  (if interactive-p (message (if name
					 "Compiling %s's definition..."
				       "Compiling definition...")
				     name))
	  (setq obj (byte-compile obj))
	  (if interactive-p (message "Done compiling.  Disassembling..."))))
    (cond ((consp obj)
	   (setq obj (cdr obj))		;throw lambda away
	   (setq args (car obj))	;save arg list
	   (setq obj (cdr obj)))
	  (t
	   (setq args (compiled-function-arglist obj))))
    (if (zerop indent)			; not a nested function
	(progn
	  (indent-to indent)
	  (insert (format "byte code%s%s%s:\n"
			  (if (or macro name) " for" "")
			  (if macro " macro" "")
			  (if name (format " %s" name) "")))))
    (let ((doc (if (consp obj)
		   (and (stringp (car obj)) (car obj))
		 (condition-case error
		     (documentation obj)
		   (error (format "%S" error))))))
      (if (and doc (stringp doc))
	  (progn (and (consp obj) (setq obj (cdr obj)))
		 (indent-to indent)
		 (princ "  doc:  " (current-buffer))
		 (let ((frobbed nil))
		   (if (string-match "\n" doc)
		       (setq doc (substring doc 0 (match-beginning 0))
			     frobbed t))
		   (if (> (length doc) 70)
		       (setq doc (substring doc 0 65) frobbed t))
		   (if frobbed (setq doc (concat doc " ..."))))
		 (insert doc "\n"))))
    (indent-to indent)
    (insert "  args: ")
    (prin1 args (current-buffer))
    (insert "\n")
    (if (condition-case ()
	    (commandp obj)                  ; ie interactivep
	  (error nil))
	(let ((interactive (if (consp obj)
			       (elt (assq 'interactive obj) 1)
			     (elt (compiled-function-interactive obj) 1))))
	  (if (eq (car-safe (car-safe obj)) 'interactive)
	      (setq obj (cdr obj)))
	  (indent-to indent)
	  (insert " interactive: ")
	  (if (eq (car-safe interactive) 'byte-code)
	      (progn
		(insert "\n")
		(disassemble-1 interactive
			       (+ indent disassemble-recursive-indent)))
	    (let ((print-escape-newlines t))
	      (prin1 interactive (current-buffer))))
	  (insert "\n")))
    (cond ((and (consp obj) (assq 'byte-code obj))
	   (disassemble-1 (assq 'byte-code obj) indent))
	  ((compiled-function-p obj)
	   (disassemble-1 obj indent))
	  (t
	   (insert "Uncompiled body:  ")
	   (let ((print-escape-newlines t))
	     (prin1 (if (cdr obj) (cons 'progn obj) (car obj))
		    (current-buffer))))))
  (if interactive-p
      (message nil)))


(defun disassemble-1 (obj indent)
  "Print the byte-code call OBJ in the current buffer.
OBJ should be a compiled-function object generated by the byte compiler."
  (let (bytes constvec)
    (if (consp obj)
	(setq bytes (car (cdr obj))		; the byte code
	      constvec (car (cdr (cdr obj))))	; constant vector
      (setq bytes (compiled-function-instructions obj)
	    constvec (compiled-function-constants obj)))
    (let ((lap (byte-decompile-bytecode bytes constvec))
	  op arg opname pc-value)
      (let ((tagno 0)
	    tmp
	    (lap lap))
	(while (setq tmp (assq 'TAG lap))
	  (setcar (cdr tmp) (setq tagno (1+ tagno)))
	  (setq lap (cdr (memq tmp lap)))))
      (while lap
	;; Take off the pc value of the next thing
	;; and put it in pc-value.
	(setq pc-value nil)
	(if (numberp (car lap))
	    (setq pc-value (car lap)
		  lap (cdr lap)))
	;; Fetch the next op and its arg.
	(setq op (car (car lap))
	      arg (cdr (car lap)))
	(setq lap (cdr lap))
	(indent-to indent)
	(if (eq 'TAG op)
	    (progn
	      ;; We have a label.  Display it, but first its pc value.
	      (if pc-value
		  (insert (format "%d:" pc-value)))
	      (insert (int-to-string (car arg))))
	  ;; We have an instruction.  Display its pc value first.
	  (if pc-value
	      (insert (format "%d" pc-value)))
	  (indent-to (+ indent disassemble-column-1-indent))
	  (if (and op
		   (string-match "^byte-" (setq opname (symbol-name op))))
	      (setq opname (substring opname 5))
	    (setq opname "<not-an-opcode>"))
	  (if (eq op 'byte-constant2)
	      (insert " #### shouldn't have seen constant2 here!\n  "))
	  (insert opname)
	  (indent-to (+ indent disassemble-column-1-indent
			disassemble-column-2-indent
			-1))
	  (insert " ")
	  (cond ((memq op byte-goto-ops)
		 (insert (int-to-string (nth 1 arg))))
		((memq op '(byte-call byte-unbind
				      byte-listN byte-concatN byte-insertN))
		 (insert (int-to-string arg)))
		((memq op '(byte-varref byte-varset byte-varbind))
		 (prin1 (car arg) (current-buffer)))
		((memq op '(byte-constant byte-constant2))
		 ;; it's a constant
		 (setq arg (car arg))
		 ;; but if the value of the constant is compiled code, then
		 ;; recursively disassemble it.
		 (cond ((or (compiled-function-p arg)
			    (and (eq (car-safe arg) 'lambda)
				 (assq 'byte-code arg))
			    (and (eq (car-safe arg) 'macro)
				 (or (compiled-function-p (cdr arg))
				     (and (eq (car-safe (cdr arg)) 'lambda)
					  (assq 'byte-code (cdr arg))))))
			(cond ((compiled-function-p arg)
			       (insert "<compiled-function>\n"))
			      ((eq (car-safe arg) 'lambda)
			       (insert "<compiled lambda>"))
			      (t (insert "<compiled macro>\n")))
			(disassemble-internal
			 arg
			 (+ indent disassemble-recursive-indent 1)
			 nil))
		       ((eq (car-safe arg) 'byte-code)
			(insert "<byte code>\n")
			(disassemble-1	;recurse on byte-code object
			 arg
			 (+ indent disassemble-recursive-indent)))
		       ((eq (car-safe (car-safe arg)) 'byte-code)
			(insert "(<byte code>...)\n")
			(mapcar		;recurse on list of byte-code objects
			 #'(lambda (obj)
			     (disassemble-1
			      obj
			      (+ indent disassemble-recursive-indent)))
			 arg))
		       (t
			;; really just a constant
			(let ((print-escape-newlines t))
			  (prin1 arg (current-buffer))))))
		)
	  (insert "\n")))))
  nil)

(provide 'disass)

;;; disass.el ends here
