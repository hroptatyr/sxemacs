;;; abbrev.el --- abbrev mode commands for Emacs

;; Copyright (C) 1985, 1986, 1987, 1992, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
;; Keywords: abbrev, dumped

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; SXEmacs is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Synched up with: FSF 19.34 (With some additions)

;;; Commentary:

;; This file is dumped with XEmacs.

;; This facility is documented in the Emacs Manual.

;;; Code:

(defgroup abbrev nil
  "Abbreviation handling, typing shortcuts, macros."
  :tag "Abbreviations"
  :group 'editing)

(defgroup abbrev-mode nil
  "Word abbreviations mode."
  :group 'abbrev)

;jwz: this is preloaded so don't ;;;###autoload
(defcustom only-global-abbrevs nil
  "*Non-nil means user plans to use global abbrevs only.
Makes the commands to define mode-specific abbrevs define global ones instead."
  :type 'boolean
  :group 'abbrev)

;;; XEmacs: the following block of code is not in FSF
(defvar abbrev-table-name-list '()
  "List of symbols whose values are abbrev tables.")

(defvar abbrevs-changed nil
  "Set non-nil by defining or altering any word abbrevs.
This causes `save-some-buffers' to offer to save the abbrevs.")

(defun make-abbrev-table ()
  "Return a new, empty abbrev table object."
  (make-vector 59 0)) ; 59 is prime

(defun clear-abbrev-table (table)
  "Undefine all abbrevs in abbrev table TABLE, leaving it empty."
  (fillarray table 0)
  (setq abbrevs-changed t)
  nil)


(defun define-abbrev-table (table-name definitions)
  "Define TABLE-NAME (a symbol) as an abbrev table name.
Define abbrevs in it according to DEFINITIONS, which is a list of elements
of the form (ABBREVNAME EXPANSION HOOK USECOUNT)."
  (let ((table (and (boundp table-name) (symbol-value table-name))))
    (cond ((vectorp table))
	  ((not table)
	   (setq table (make-abbrev-table))
	   (set table-name table)
	   (setq abbrev-table-name-list (cons table-name abbrev-table-name-list)))
	  (t
	   (setq table (wrong-type-argument 'vectorp table))
	   (set table-name table)))
    (while definitions
      (apply (function define-abbrev) table (car definitions))
      (setq definitions (cdr definitions)))))

(defun define-abbrev (table name &optional expansion hook count)
  "Define an abbrev in TABLE named NAME, to expand to EXPANSION or call HOOK.
NAME and EXPANSION are strings.  Hook is a function or `nil'.
To undefine an abbrev, define it with an expansion of `nil'."
  (check-type expansion (or null string))
  (check-type count (or null integer))
  (check-type table vector)
  (let* ((sym (intern name table))
	 (oexp (and (boundp sym) (symbol-value sym)))
	 (ohook (and (fboundp sym) (symbol-function sym))))
    (unless (and (equal ohook hook)
		 (stringp oexp)
		 (stringp expansion)
		 (string-equal oexp expansion))
      (setq abbrevs-changed t)
      ;; If there is a non-word character in the string, set the flag.
      (if (string-match "\\W" name)
	  (set (intern " " table) nil)))
    (set sym expansion)
    (fset sym hook)
    (setplist sym (or count 0))
    name))


;; Fixup stuff from bootstrap def of define-abbrev-table in subr.el
(let ((l abbrev-table-name-list))
  (while l
    (let ((fixup (car l)))
      (if (consp fixup)
	  (progn
	    (setq abbrev-table-name-list (delq fixup abbrev-table-name-list))
	    (define-abbrev-table (car fixup) (cdr fixup))))
      (setq l (cdr l))))
  ;; These are no longer initialized by C code
  (if (not global-abbrev-table)
      (progn
	(setq global-abbrev-table (make-abbrev-table))
	(setq abbrev-table-name-list (cons 'global-abbrev-table
					   abbrev-table-name-list))))
  (if (not fundamental-mode-abbrev-table)
      (progn
	(setq fundamental-mode-abbrev-table (make-abbrev-table))
	(setq abbrev-table-name-list (cons 'fundamental-mode-abbrev-table
					   abbrev-table-name-list))))
  (and (eq major-mode 'fundamental-mode)
       (not local-abbrev-table)
       (setq local-abbrev-table fundamental-mode-abbrev-table)))


(defun define-global-abbrev (name expansion)
  "Define ABBREV as a global abbreviation for EXPANSION."
  (interactive "sDefine global abbrev: \nsExpansion for %s: ")
  (define-abbrev global-abbrev-table
		 (downcase name) expansion nil 0))

(defun define-mode-abbrev (name expansion)
  "Define ABBREV as a mode-specific abbreviation for EXPANSION."
  (interactive "sDefine mode abbrev: \nsExpansion for %s: ")
  (define-abbrev (or local-abbrev-table
		     (error "Major mode has no abbrev table"))
		 (downcase name) expansion nil 0))

(defun abbrev-symbol (abbrev &optional table)
  "Return the symbol representing abbrev named ABBREV.
This symbol's name is ABBREV, but it is not the canonical symbol of that name;
it is interned in an abbrev-table rather than the normal obarray.
The value is nil if that abbrev is not defined.
Optional second arg TABLE is abbrev table to look it up in.
The default is to try buffer's mode-specific abbrev table, then global table."
  (let ((frob (function (lambda (table)
		(let ((sym (intern-soft abbrev table)))
		  (if (and (boundp sym)
			   (stringp (symbol-value sym)))
		      sym
		      nil))))))
    (if table
	(funcall frob table)
	(or (and local-abbrev-table
		 (funcall frob local-abbrev-table))
	    (funcall frob global-abbrev-table)))))

(defun abbrev-expansion (abbrev &optional table)
  "Return the string that ABBREV expands into in the current buffer.
Optionally specify an abbrev table as second arg;
then ABBREV is looked up in that table only."
  (let ((sym (abbrev-symbol abbrev table)))
    (if sym
	(symbol-value sym)
	nil)))

(defun unexpand-abbrev ()
  "Undo the expansion of the last abbrev that expanded.
This differs from ordinary undo in that other editing done since then
is not undone."
  (interactive)
  (if (or (< last-abbrev-location (point-min))
	  (> last-abbrev-location (point-max))
	  (not (stringp last-abbrev-text)))
      nil
    (let* ((opoint (point))
	   (val (symbol-value last-abbrev))
	   (adjust (length val)))
      ;; This isn't correct if (symbol-function last-abbrev-text)
      ;;  was used to do the expansion
      (goto-char last-abbrev-location)
      (delete-region last-abbrev-location (+ last-abbrev-location adjust))
      (insert last-abbrev-text)
      (setq adjust (- adjust (length last-abbrev-text)))
      (setq last-abbrev-text nil)
      (if (< last-abbrev-location opoint)
	  (goto-char (- opoint adjust))
	  (goto-char opoint)))))



(defun insert-abbrev-table-description (name &optional human-readable)
  "Insert before point a full description of abbrev table named NAME.
NAME is a symbol whose value is an abbrev table.
If optional second argument HUMAN-READABLE is non-nil, insert a
human-readable description. Otherwise the description is an
expression, a call to `define-abbrev-table', which would define the
abbrev table NAME exactly as it is currently defined."
  (let ((table (symbol-value name))
	(stream (current-buffer)))
    (message "Abbrev-table %s..." name)
    (if human-readable
	(progn
	  (prin1 (list name) stream)
	  ;; Need two terpri's or cretinous edit-abbrevs blows out
	  (terpri stream)
	  (terpri stream)
	  (mapatoms (function (lambda (sym)
		      (if (symbol-value sym)
			  (let* ((n (prin1-to-string (symbol-name sym)))
				 (pos (length n)))
			    (princ n stream)
			    (while (< pos 14)
			      (write-char ?\  stream)
			      (setq pos (1+ pos)))
			    (princ (format " %-5S " (symbol-plist sym))
				   stream)
			    (if (not (symbol-function sym))
				(prin1 (symbol-value sym) stream)
			      (progn
				(setq n (prin1-to-string (symbol-value sym))
				      pos (+ pos 6 (length n)))
				(princ n stream)
				(while (< pos 45)
				  (write-char ?\  stream)
				  (setq pos (1+ pos)))
				(prin1 (symbol-function sym) stream)))
			    (terpri stream)))))
		    table)
	  (terpri stream))
	(progn
	  (princ "\(define-abbrev-table '" stream)
	  (prin1 name stream)
	  (princ " '\(\n" stream)
	  (mapatoms (function (lambda (sym)
		      (if (symbol-value sym)
			  (progn
			    (princ "    " stream)
			    (prin1 (list (symbol-name sym)
					 (symbol-value sym)
					 (symbol-function sym)
					 (symbol-plist sym))
				   stream)
			    (terpri stream)))))
		    table)
	  (princ "    \)\)\n" stream)))
    (terpri stream))
  (message ""))
;;; End code not in FSF

(defun abbrev-mode (arg)
  "Toggle abbrev mode.
With argument ARG, enable abbrev mode if ARG is positive, else disable.
In abbrev mode, inserting an abbreviation causes it to expand
and be replaced by its expansion."
  (interactive "P")
  (setq abbrev-mode
	(if (null arg) (not abbrev-mode)
	  (> (prefix-numeric-value arg) 0)))
  ;; XEmacs change
  (redraw-modeline))


(defvar edit-abbrevs-map nil
  "Keymap used in edit-abbrevs.")
(if edit-abbrevs-map
    nil
  (setq edit-abbrevs-map (make-sparse-keymap))
  ;; XEmacs change
  (set-keymap-name edit-abbrevs-map 'edit-abbrevs-map)
  (define-key edit-abbrevs-map "\C-x\C-s" 'edit-abbrevs-redefine)
  (define-key edit-abbrevs-map "\C-c\C-c" 'edit-abbrevs-redefine))

(defun kill-all-abbrevs ()
  "Undefine all defined abbrevs."
  (interactive)
  (let ((tables abbrev-table-name-list))
    (while tables
      (clear-abbrev-table (symbol-value (car tables)))
      (setq tables (cdr tables)))))

(defun insert-abbrevs ()
  "Insert after point a description of all defined abbrevs.
Mark is set after the inserted text."
  (interactive)
  (push-mark
   (save-excursion
    (let ((tables abbrev-table-name-list))
      (while tables
	(insert-abbrev-table-description (car tables) t)
	(setq tables (cdr tables))))
    (point))))

(defun list-abbrevs ()
  "Display a list of all defined abbrevs."
  (interactive)
  (display-buffer (prepare-abbrev-list-buffer)))

(defun prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (let ((tables abbrev-table-name-list))
      (while tables
	(insert-abbrev-table-description (car tables) t)
	(setq tables (cdr tables))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun edit-abbrevs-mode ()
  "Major mode for editing the list of abbrev definitions.
\\{edit-abbrevs-map}"
  (interactive)
  (setq major-mode 'edit-abbrevs-mode)
  (setq mode-name "Edit-Abbrevs")
  (use-local-map edit-abbrevs-map))

(defun edit-abbrevs ()
  "Alter abbrev definitions by editing a list of them.
Selects a buffer containing a list of abbrev definitions.
You can edit them and type \\<edit-abbrevs-map>\\[edit-abbrevs-redefine] to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted)."
  (interactive)
  (switch-to-buffer (prepare-abbrev-list-buffer)))

(defun edit-abbrevs-redefine ()
  "Redefine abbrevs according to current buffer contents."
  (interactive)
  (define-abbrevs t)
  (set-buffer-modified-p nil))

(defun define-abbrevs (&optional arg)
  "Define abbrevs according to current visible buffer contents.
See documentation of `edit-abbrevs' for info on the format of the
text you must have in the buffer.
With argument, eliminate all abbrev definitions except
the ones defined from the buffer now."
  (interactive "P")
  (if arg (kill-all-abbrevs))
  (save-excursion
   (goto-char (point-min))
   (while (and (not (eobp)) (re-search-forward "^(" nil t))
     (let* ((buf (current-buffer))
	    (table (read buf))
	    abbrevs name hook exp count)
       (forward-line 1)
       (while (progn (forward-line 1)
		     (not (eolp)))
	 (setq name (read buf) count (read buf) exp (read buf))
	 (skip-chars-backward " \t\n\f")
	 (setq hook (if (not (eolp)) (read buf)))
	 (skip-chars-backward " \t\n\f")
	 (setq abbrevs (cons (list name exp hook count) abbrevs)))
       (define-abbrev-table table abbrevs)))))

(defun read-abbrev-file (&optional file quietly)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Optional second argument QUIETLY non-nil means don't print anything."
  (interactive "fRead abbrev file: ")
  (load (if (and file (> (length file) 0)) file abbrev-file-name)
	nil quietly)
  (setq save-abbrevs t abbrevs-changed nil))

(defun quietly-read-abbrev-file (&optional file)
  "Read abbrev definitions from file written with `write-abbrev-file'.
Optional argument FILE is the name of the file to read;
it defaults to the value of `abbrev-file-name'.
Does not print anything."
  ;(interactive "fRead abbrev file: ")
  (read-abbrev-file file t))

(defun write-abbrev-file (file)
  "Write all abbrev definitions to a file of Lisp code.
The file written can be loaded in another session to define the same abbrevs.
The argument FILE is the file name to write."
  (interactive
   (list
    (read-file-name "Write abbrev file: "
		    (file-name-directory (expand-file-name abbrev-file-name))
		    abbrev-file-name)))
  (or (and file (> (length file) 0))
      (setq file abbrev-file-name))
  (save-excursion
   (set-buffer (get-buffer-create " write-abbrev-file"))
   (erase-buffer)
   (let ((tables abbrev-table-name-list))
     (while tables
       (insert-abbrev-table-description (car tables) nil)
       (setq tables (cdr tables))))
   (write-region 1 (point-max) file)
   (erase-buffer)))

(defun abbrev-string-to-be-defined (arg)
  "Return the string for which an abbrev will be defined.
ARG is the argument to `add-global-abbrev' or `add-mode-abbrev'."
  (if (and (not arg) (region-active-p)) (setq arg 0)
    (setq arg (prefix-numeric-value arg)))
  (and (>= arg 0)
       (buffer-substring
	(point)
	(if (= arg 0) (mark)
	  (save-excursion (backward-word arg) (point))))))

(defun add-mode-abbrev (arg)
  "Define mode-specific abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
Reads the abbreviation in the minibuffer.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  ;; XEmacs change:
  (interactive "P")
  (add-abbrev
   (if only-global-abbrevs
       global-abbrev-table
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" arg))

(defun add-global-abbrev (arg)
  "Define global (all modes) abbrev for last word(s) before point.
The prefix argument specifies the number of words before point that form the
expansion; or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
This command uses the minibuffer to read the abbreviation.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  ;; XEmacs change:
  (interactive "P")
  (add-abbrev global-abbrev-table "Global" arg))

(defun add-abbrev (table type arg)
  "Add an abbreviation to abbrev table TABLE.
TYPE is a string describing in English the kind of abbrev this will be
(typically, \"global\" or \"mode-specific\"); this is used in
prompting the user.  ARG is the number of words in the expansion.

Return the symbol that internally represents the new abbrev, or nil if
the user declines to confirm redefining an existing abbrev."
  ;; XEmacs change:
  (let ((exp (abbrev-string-to-be-defined arg))
	name)
    (setq name
	  (read-string (format (if exp "%s abbrev for \"%s\": "
				 "Undefine %s abbrev: ")
			       type exp)))
    (set-text-properties 0 (length name) nil name)
    (if (or (null exp)
	    (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp))))

(defun inverse-abbrev-string-to-be-defined (arg)
  "Return the string for which an inverse abbrev will be defined.
ARG is the argument to `inverse-add-global-abbrev' or
`inverse-add-mode-abbrev'."
  (save-excursion
    (backward-word arg)
    (buffer-substring (point) (progn (forward-word 1) (point)))))

(defun inverse-add-mode-abbrev (arg)
  "Define last word before point as a mode-specific abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev
   (if only-global-abbrevs
       global-abbrev-table
     (or local-abbrev-table
	 (error "No per-mode abbrev table")))
   "Mode" arg))

(defun inverse-add-global-abbrev (arg)
  "Define last word before point as a global (mode-independent) abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  (interactive "p")
  (inverse-add-abbrev global-abbrev-table "Global" arg))

(defun inverse-add-abbrev (table type arg)
  (let (name nameloc exp)
    (save-excursion
     (backward-word arg)
     (setq name (buffer-substring (point) (progn (forward-word 1)
					       (setq nameloc (point))))))
    (set-text-properties 0 (length name) nil name)
    (setq exp (read-string (format "%s expansion for \"%s\": "
				   type name)))
    (if (or (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(progn
	 (define-abbrev table (downcase name) exp)
	 (save-excursion
	  (goto-char nameloc)
	  (expand-abbrev))))))

(defun abbrev-prefix-mark (&optional arg)
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word.
This way, you can expand an abbrev with a prefix: insert the prefix,
use this command, then insert the abbrev."
  (interactive "P")
  (or arg (expand-abbrev))
  (setq abbrev-start-location (point-marker)
	abbrev-start-location-buffer (current-buffer))
  (let ((e (make-extent (point) (point))))
    (set-extent-begin-glyph e (make-glyph [string :data "-"]))))

(defun expand-region-abbrevs (start end &optional noquery)
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type y or n for each occurrence.
A prefix argument means don't query; expand all abbrevs.
If called from a Lisp program, arguments are START END &optional NOQUERY."
  (interactive "r\nP")
  (save-excursion
    (goto-char start)
    (let ((lim (- (point-max) end))
	  pnt string)
      (while (and (not (eobp))
		  (progn (forward-word 1)
			 (<= (setq pnt (point)) (- (point-max) lim))))
	(if (abbrev-expansion
	     (setq string
		   (buffer-substring
		    (save-excursion (backward-word) (point))
		    pnt)))
	    (if (or noquery (y-or-n-p (format "Expand `%s'? " string)))
		(expand-abbrev)))))))

;;; abbrev.el ends here
