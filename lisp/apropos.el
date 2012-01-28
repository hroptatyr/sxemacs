;;; apropos.el --- apropos commands for users and programmers.

;; Copyright (C) 1989, 1994, 1995 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@bigbird.bu.edu>
;; Rewritten: Daniel.Pfeiffer@Informatik.START.dbp.de, fax (+49 69) 7588-2389
;; Maintainer: SL Baur <steve@xemacs.org>
;; Keywords: help

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

;;; Synched up with: Last synched with FSF 19.34, diverged since.

;;; Commentary:

;; The ideas for this package were derived from the C code in
;; src/keymap.c and elsewhere.  The functions in this file should
;; always be byte-compiled for speed.  Someone should rewrite this in
;; C (as part of src/keymap.c) for speed.

;; The idea for super-apropos is based on the original implementation
;; by Lynn Slater <lrs@esl.com>.

;;; ChangeLog:

;; Fixed bug, current-local-map can return nil.
;; Change, doesn't calculate key-bindings unless needed.
;; Added super-apropos capability, changed print functions.
;;; Made fast-apropos and super-apropos share code.
;;; Sped up fast-apropos again.
;; Added apropos-do-all option.
;;; Added fast-command-apropos.
;; Changed doc strings to comments for helping functions.
;;; Made doc file buffer read-only, buried it.
;; Only call substitute-command-keys if do-all set.

;; Optionally use configurable faces to make the output more legible.
;; Differentiate between command, function and macro.
;; Apropos-command (ex command-apropos) does cmd and optionally user var.
;; Apropos shows all 3 aspects of symbols (fn, var and plist)
;; Apropos-documentation (ex super-apropos) now finds all it should.
;; New apropos-value snoops through all values and optionally plists.
;; Reading DOC file doesn't load nroff.
;; Added hypertext following of documentation, mouse-2 on variable gives value
;;   from buffer in active window.
;; Added apropos-rewrite-regexp from FSF apropos.
;;; Code:

;; I see a degradation of maybe 10-20% only.
;; [sb -- FSF protects the face declarations with `if window-system'
;;  I see no reason why we should do so]
(defvar apropos-do-all nil
  "*Whether the apropos commands should do more.
Slows them down more or less.  Set this non-nil if you have a fast machine.")

;; XEmacs addition
(defvar apropos-symbol-face (if-boundp 'font-lock-keyword-face
				font-lock-keyword-face
			      'bold)
  "*Face for symbol name in apropos output or `nil'.
This looks good, but slows down the commands several times.")

;; XEmacs addition
(defvar apropos-keybinding-face (if-boundp 'font-lock-string-face
				    font-lock-string-face
				  'underline)
  "*Face for keybinding display in apropos output or `nil'.
This looks good, but slows down the commands several times.")

;; XEmacs addition
(defvar apropos-label-face (if-boundp 'font-lock-comment-face
			       font-lock-comment-face
			     'italic)
  "*Face for label (Command, Variable ...) in apropos output or `nil'.
If this is `nil' no mouse highlighting occurs.
This looks good, but slows down the commands several times.
When this is a face name, as it is initially, it gets transformed to a
text-property list for efficiency.")

;; XEmacs addition
(defvar apropos-property-face (if-boundp 'font-lock-variable-name-face
				  font-lock-variable-name-face
				'bold-italic)
  "*Face for property name in apropos output or `nil'.
This looks good, but slows down the commands several times.")

(defvar apropos-match-face 'secondary-selection
  "*Face for matching part in apropos-documentation/value output or `nil'.
This looks good, but slows down the commands several times.")


(defvar apropos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control m)] 'apropos-follow)
    (define-key map [return] 'apropos-follow)
    (define-key map [(button2up)] 'apropos-mouse-follow)
    (define-key map [(button2)] 'undefined)
    map)
  "Keymap used in Apropos mode.")


(defvar apropos-regexp nil
  "Regexp used in current apropos run.")

(defvar apropos-files-scanned ()
  "List of elc files already scanned in current run of `apropos-documentation'.")

(defvar apropos-accumulator ()
  "Alist of symbols already found in current apropos run.")

(defvar apropos-item ()
  "Current item in or for apropos-accumulator.")

(defvar apropos-synonyms '(
  ("find" "open" "edit")
  ("kill" "cut")
  ("yank" "paste"))
  "List of synonyms known by apropos.
Each element is a list of words where the first word is the standard emacs
term, and the rest of the words are alternative terms.")


(defvar apropos-mode-hook nil) ; XEmacs

(defcustom apropos-rewrite-regexps nil  ; SXEmacs
  "*Non-nil mean regexps with spaces are rewritten to match all words permutations."
  :type 'boolean)

(defun apropos-mode ()
  "Major mode for following hyperlinks in output of apropos commands.

\\{apropos-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map apropos-mode-map)
  (setq major-mode 'apropos-mode
	mode-name "Apropos")
  (run-hooks 'apropos-mode-hook)) ; XEmacs

(defun apropos-words-to-regexp (words wild)
  "Make regexp matching any two of the words in WORDS."
  (concat "\\("
	  (mapconcat 'identity words "\\|")
	  "\\)"
	  (if (cdr words)
	      (concat wild
		      "\\("
		      (mapconcat 'identity words "\\|")
		      "\\)")
	    "")))

(defun apropos-rewrite-regexp (regexp)
  "Rewrite a list of words to a regexp matching all permutations.
If REGEXP is already a regexp, don't modify it.
Also if `apropos-rewrite-regexps' is nil, don't modify it."
  (with-boundp '(apropos-orig-regexp apropos-words apropos-all-words
				     apropos-all-regexp)
    (if (not apropos-rewrite-regexps)
	regexp
      (setq apropos-orig-regexp regexp)
      (setq apropos-words () apropos-all-words ())
      (if (string-equal (regexp-quote regexp) regexp)
	  ;; We don't actually make a regexp matching all permutations.
	  ;; Instead, for e.g. "a b c", we make a regexp matching
	  ;; any combination of two or more words like this:
	  ;; (a|b|c).*(a|b|c) which may give some false matches,
	  ;; but as long as it also gives the right ones, that's ok.
	  (let ((words (split-string regexp "[ \t]+")))
	    (dolist (word words)
	      (let ((syn apropos-synonyms) (s word) (a word))
		(while syn
		  (if (member word (car syn))
		      (progn
			(setq a (mapconcat 'identity (car syn) "\\|"))
			(if (member word (cdr (car syn)))
			    (setq s a))
			(setq syn nil))
		    (setq syn (cdr syn))))
		(setq apropos-words (cons s apropos-words)
		      apropos-all-words (cons a apropos-all-words))))
	    (setq apropos-all-regexp (apropos-words-to-regexp apropos-all-words ".+"))
	    (apropos-words-to-regexp apropos-words ".*?"))
	(setq apropos-all-regexp regexp)))))

;; For auld lang syne:
;;;###autoload
(fset 'command-apropos 'apropos-command)

;;;###autoload
(defun apropos-command (apropos-regexp &optional do-all)
  "Shows commands (interactively callable functions) that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
variables."
  ;; XEmacs: All code related to special treatment of buffer has been removed
  (interactive (list (read-string (concat "Apropos command "
					  (if (or current-prefix-arg
						  apropos-do-all)
					      "or variable ")
					  "(regexp): "))
		     current-prefix-arg))
  (setq apropos-regexp (apropos-rewrite-regexp apropos-regexp))
  (or do-all (setq do-all apropos-do-all))
  (setq apropos-accumulator
	(apropos-internal apropos-regexp
			  (if do-all
			      (lambda (symbol) (or (commandp symbol)
						   (user-variable-p symbol)))
			    'commandp)))
  (apropos-print
   t
   (lambda (p)
     (let (doc symbol)
       (while p
	 (setcar p (list
		    (setq symbol (car p))
		    (if (commandp symbol)
			(if (setq doc
				  ;; XEmacs change: if obsolete,
				  ;; only mention that.
				  (or (function-obsoleteness-doc symbol)
				      (condition-case nil
					  (documentation symbol t)
					(void-function "(aliased to undefined function)")
					(error "(unexpected error from `documention')"))))
			    (substring doc 0 (string-match "\n" doc))
			  "(not documented)"))
		    (and do-all
			 (user-variable-p symbol)
			 (if (setq doc
				   (or
				    ;; XEmacs change: if obsolete,
				    ;; only mention that.
				    (variable-obsoleteness-doc symbol)
				    (documentation-property
				     symbol 'variable-documentation t)))
			     (substring doc 0
					    (string-match "\n" doc))))))
	 (setq p (cdr p)))))
   nil))


;;;###autoload
(defun apropos (apropos-regexp &optional do-all)
  "Show all bound symbols whose names match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show unbound
symbols and key bindings, which is a little more time-consuming.
Returns list of symbols and documentation found."
  (interactive "sApropos symbol (regexp): \nP")
  ;; XEmacs change: hitting ENTER by mistake is a common mess-up and
  ;; shouldn't make Emacs hang for a long time trying to list all symbols.
  (or (> (length apropos-regexp) 0)
      (error "Must pass non-empty regexp to `apropos'"))
  (setq apropos-regexp (apropos-rewrite-regexp apropos-regexp))
  (setq apropos-accumulator
	(apropos-internal apropos-regexp
			  (and (not do-all)
			       (not apropos-do-all)
			       (lambda (symbol)
				 (or (fboundp symbol)
				     (boundp symbol)
				     (find-face symbol)
				     (symbol-plist symbol))))))
  (apropos-print
   (or do-all apropos-do-all)
   (lambda (p)
     (let (symbol doc)
       (while p
	 (setcar p (list
		    (setq symbol (car p))
		    (if (fboundp symbol)
			(if (setq doc
				  ;; XEmacs change: if obsolete,
				  ;; only mention that.
				  (or (function-obsoleteness-doc symbol)
				      (condition-case nil
					  (documentation symbol t)
					(void-function "(aliased to undefined function)")
					(error "(unexpected error from `documention')"))))
			    (substring doc 0 (string-match "\n" doc))
			  "(not documented)"))
		    (if (boundp symbol)
			(if (setq doc
				  (or
				   ;; XEmacs change: if obsolete,
				   ;; only mention that.
				   (variable-obsoleteness-doc symbol)
				   (documentation-property
				    symbol 'variable-documentation t)))
			    (substring doc 0
				       (string-match "\n" doc))
			  "(not documented)"))
		    (if (setq doc (symbol-plist symbol))
			(if (eq (/ (length doc) 2) 1)
			    (format "1 property (%s)" (car doc))
			  (format "%d properties" (/ (length doc) 2))))
		    (if (get symbol 'widget-type)
			(if (setq doc (documentation-property
				       symbol 'widget-documentation t))
			    (substring doc 0
				       (string-match "\n" doc))
			  "(not documented)"))
		    (if (find-face symbol)
			(if (setq doc (face-doc-string symbol))
			    (substring doc 0
				       (string-match "\n" doc))
			  "(not documented)"))
		    (when (get symbol 'custom-group)
		      (if (setq doc (documentation-property
				     symbol 'group-documentation t))
			  (substring doc 0
				     (string-match "\n" doc))
			"(not documented)"))))
	 (setq p (cdr p)))))
   nil))


;;;###autoload
(defun apropos-value (apropos-regexp &optional do-all)
  "Show all symbols whose value's printed image matches REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also looks
at the function and at the names and values of properties.
Returns list of symbols and values found."
  (interactive "sApropos value (regexp): \nP")
  (setq apropos-regexp (apropos-rewrite-regexp apropos-regexp))
  (or do-all (setq do-all apropos-do-all))
  (setq apropos-accumulator ())
   (let (f v p)
     (mapatoms
      (lambda (symbol)
	(setq f nil v nil p nil)
	(or (memq symbol '(apropos-regexp do-all apropos-accumulator
					  symbol f v p))
	    (setq v (apropos-value-internal 'boundp symbol 'symbol-value)))
	(if do-all
	    (setq f (apropos-value-internal 'fboundp symbol 'symbol-function)
		  p (apropos-format-plist symbol "\n    " t)))
	(if (or f v p)
	    (setq apropos-accumulator (cons (list symbol f v p)
					    apropos-accumulator))))))
  (apropos-print nil nil t))


;;;###autoload
(defun apropos-documentation (apropos-regexp &optional do-all)
  "Show symbols whose documentation contain matches for REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also use
documentation that is not stored in the documentation file and show key
bindings.
Returns list of symbols and documentation found."
  (interactive "sApropos documentation (regexp): \nP")
  (setq apropos-regexp (apropos-rewrite-regexp apropos-regexp))
  (or do-all (setq do-all apropos-do-all))
  (setq apropos-accumulator () apropos-files-scanned ())
  (let ((standard-input (get-buffer-create " apropos-temp"))
	f v)
    (unwind-protect
	(save-excursion
	  (set-buffer standard-input)
	  (apropos-documentation-check-doc-file)
	  (if do-all
	      (mapatoms
	       (lambda (symbol)
		 (setq f (apropos-safe-documentation symbol)
		       v (get symbol 'variable-documentation))
		 (when (integerp v) (setq v nil))
		 (setq f (apropos-documentation-internal f)
		       v (apropos-documentation-internal v))
		 (if (or f v)
		     (if (setq apropos-item
			       (cdr (assq symbol apropos-accumulator)))
			 (progn
			   (if f
			       (setcar apropos-item f))
			   (if v
			       (setcar (cdr apropos-item) v)))
		       (setq apropos-accumulator
			     (cons (list symbol f v)
				   apropos-accumulator)))))))
	  (apropos-print nil nil t))
      (kill-buffer standard-input))))


(defun apropos-value-internal (predicate symbol function)
  (if (funcall predicate symbol)
      (progn
	(setq symbol (prin1-to-string (funcall function symbol)))
	(if (string-match apropos-regexp symbol)
	    (progn
	      (if apropos-match-face
		  (put-text-property (match-beginning 0) (match-end 0)
				     'face apropos-match-face
				     symbol))
	      symbol)))))

(defun apropos-documentation-internal (doc)
  (if (consp doc)
      (apropos-documentation-check-elc-file (car doc))
    (and doc
	 (string-match apropos-regexp doc)
	 (progn
	   (if apropos-match-face
	       (put-text-property (match-beginning 0)
				  (match-end 0)
				  'face apropos-match-face
				  (setq doc (copy-sequence doc))))
	   doc))))

(defun apropos-format-plist (pl sep &optional compare)
  (setq pl (symbol-plist pl))
  (let (p p-out)
    (while pl
      (setq p (format "%s %S" (car pl) (nth 1 pl)))
      (if (or (not compare) (string-match apropos-regexp p))
	  (if apropos-property-face
	      (put-text-property 0 (length (symbol-name (car pl)))
				 'face apropos-property-face p))
	(setq p nil))
      (if p
	  (progn
	    (and compare apropos-match-face
		 (put-text-property (match-beginning 0) (match-end 0)
				    'face apropos-match-face
				    p))
	    (setq p-out (concat p-out (if p-out sep) p))))
      (setq pl (nthcdr 2 pl)))
    p-out))


;; Finds all documentation related to APROPOS-REGEXP in internal-doc-file-name.

(defun apropos-documentation-check-doc-file ()
  (let (type symbol (sepa 2) sepb start end doc)
    (insert ?\^_)
    (backward-char)
    (insert-file-contents (concat doc-directory internal-doc-file-name))
    (forward-char)
    (while (save-excursion
	     (setq sepb (search-forward "\^_"))
	     (not (eobp)))
      (beginning-of-line 2)
      (if (save-restriction
	    (narrow-to-region (point) (1- sepb))
	    (re-search-forward apropos-regexp nil t))
	  (progn
	    (setq start (match-beginning 0)
		  end (point))
	    (goto-char (1+ sepa))
	    (or (setq type (if (eq ?F (preceding-char))
			       1	; function documentation
			     2)		; variable documentation
		      symbol (read)
		      start (- start (point) 1)
		      end (- end (point) 1)
		      doc (buffer-substring (1+ (point)) (1- sepb))
		      apropos-item (assq symbol apropos-accumulator))
		(setq apropos-item (list symbol nil nil)
		      apropos-accumulator (cons apropos-item
						apropos-accumulator)))
	    (if apropos-match-face
		(put-text-property start end 'face apropos-match-face doc))
	    (setcar (nthcdr type apropos-item) doc)))
      (setq sepa (goto-char sepb)))))

(defun apropos-documentation-check-elc-file (file)
  (if (member file apropos-files-scanned)
      nil
    (let (symbol doc start end this-is-a-variable)
      (setq apropos-files-scanned (cons file apropos-files-scanned))
      (erase-buffer)
      (insert-file-contents file)
      (while (search-forward "\n#@" nil t)
	;; Read the comment length, and advance over it.
	(setq end (read)
	      start (1+ (point))
	      end (+ (point) end -1))
	(forward-char)
	(if (save-restriction
	      ;; match ^ and $ relative to doc string
	      (narrow-to-region start end)
	      (re-search-forward apropos-regexp nil t))
	    (progn
	      (goto-char (+ end 2))
	      (setq doc (buffer-substring start end)
		    end (- (match-end 0) start)
		    start (- (match-beginning 0) start)
		    this-is-a-variable (looking-at #r"(def\(var\|const\) ")
		    symbol (progn
			     (skip-chars-forward "(a-z")
			     (forward-char)
			     (read))
		    symbol (if (consp symbol)
			       (nth 1 symbol)
			     symbol))
	      (if (if this-is-a-variable
		      (get symbol 'variable-documentation)
		    (and (fboundp symbol) (apropos-safe-documentation symbol)))
		  (progn
		    (or (setq apropos-item (assq symbol apropos-accumulator))
			(setq apropos-item (list symbol nil nil)
			      apropos-accumulator (cons apropos-item
							apropos-accumulator)))
		    (if apropos-match-face
			(put-text-property start end 'face apropos-match-face
					   doc))
		    (setcar (nthcdr (if this-is-a-variable 2 1)
				    apropos-item)
			    doc)))))))))



(defun apropos-safe-documentation (function)
  "Like documentation, except it avoids calling `get_doc_string'.
Will return nil instead."
  (while (and function (symbolp function))
    (setq function (if (fboundp function)
		       (symbol-function function))))
  (if (eq (car-safe function) 'macro)
      (setq function (cdr function)))
  ;; XEmacs change from: (setq function (if (byte-code-function-p function)
  (setq function (if (compiled-function-p function)
		     (if (fboundp 'compiled-function-doc-string)
			 (compiled-function-doc-string function)
		       (if (> (length function) 4)
			   (aref function 4)))
		   (if (eq (car-safe function) 'autoload)
		       (nth 2 function)
		     (if (eq (car-safe function) 'lambda)
			 (if (stringp (nth 2 function))
			     (nth 2 function)
			   (if (stringp (nth 3 function))
			       (nth 3 function)))))))
  (if (integerp function)
      nil
    function))



(defun apropos-print (do-keys doc-fn spacing)
  "Output result of various apropos commands with `apropos-regexp'.
APROPOS-ACCUMULATOR is a list.  Optional DOC-FN is called for each element
of apropos-accumulator and may modify it resulting in (symbol fn-doc
var-doc [plist-doc]).  Returns sorted list of symbols and documentation
found."
  (if (null apropos-accumulator)
      (message "No apropos matches for `%s'" apropos-regexp)
    (if doc-fn
	(funcall doc-fn apropos-accumulator))
    (setq apropos-accumulator
	  (sort apropos-accumulator (lambda (a b)
				      (string-lessp (car a) (car b)))))
    (and apropos-label-face
	 (or (symbolp apropos-label-face)
	     (facep apropos-label-face)) ; XEmacs
	 (setq apropos-label-face `(face ,apropos-label-face
					 mouse-face highlight)))
    (let ((help-buffer-prefix-string "Apropos"))
      (with-displaying-help-buffer
       (lambda ()
	 (with-current-buffer standard-output
	   (run-hooks 'apropos-mode-hook)
	   (let ((p apropos-accumulator)
		 (old-buffer (current-buffer))
		 symbol item point1 point2)
	     ;; Mostly useless but to provide better keymap
	     ;; explanation. help-mode-map will be used instead.
	     (use-local-map apropos-mode-map)
	     ;; XEmacs change from (if window-system
	     (if (device-on-window-system-p)
		 (progn
		   (princ "If you move the mouse over text that changes color,\n")
		   (princ (substitute-command-keys
			   "you can click \\[apropos-mouse-follow] to get more information.\n"))))
	     (princ (substitute-command-keys
		     "Type \\[apropos-follow] in this buffer to get full documentation.\n\n"))
	     (while (consp p)
	       (or (not spacing) (bobp) (terpri))
	       (setq apropos-item (car p)
		     symbol (car apropos-item)
		     p (cdr p)
		     point1 (point))
	       (princ symbol)		; print symbol name
	       (setq point2 (point))
	       ;; Calculate key-bindings if we want them.
	       (and do-keys
		    (commandp symbol)
		    (indent-to 30 1)
		    (if (let ((keys
			       (save-excursion
				 (set-buffer old-buffer)
				 (where-is-internal symbol)))
			      filtered)
			  ;; Copy over the list of key sequences,
			  ;; omitting any that contain a buffer or a frame.
			  (while keys
			    (let ((key (car keys))
				  (i 0)
				  loser)
			      (while (< i (length key))
				(if (or (framep (aref key i))
					(bufferp (aref key i)))
				    (setq loser t))
				(setq i (1+ i)))
			      (or loser
				  (setq filtered (cons key filtered))))
			    (setq keys (cdr keys)))
			  (setq item filtered))
			;; Convert the remaining keys to a string and insert.
			(princ
			 (mapconcat
			  (lambda (key)
			    (setq key (key-description key))
			    (if apropos-keybinding-face
				(put-text-property 0 (length key)
						   'face apropos-keybinding-face
						   key))
			    key)
			  item ", "))
		      (princ "Type ")
		      (princ "M-x")
		      (put-text-property (- (point) 3) (point)
					 'face apropos-keybinding-face)
		      (princ (format " %s " (symbol-name symbol)))
		      (princ "RET")
		      (put-text-property (- (point) 3) (point)
					 'face apropos-keybinding-face)))
	       (terpri)
	       ;; only now so we don't propagate text attributes all over
	       (put-text-property point1 point2 'item
				  (if (eval `(or ,@(cdr apropos-item)))
				      (car apropos-item)
				    apropos-item))
	       (if apropos-symbol-face
		   (put-text-property point1 point2 'face apropos-symbol-face))
	       ;; Add text-property on symbol, too.
	       (put-text-property point1 point2 'keymap apropos-mode-map)
	       (apropos-print-doc 'describe-function 1
				  (if (commandp symbol)
				      "Command"
				    (if (apropos-macrop symbol)
					"Macro"
				      "Function"))
				  do-keys)
	       (if (get symbol 'custom-type)
		   (apropos-print-doc 'customize-variable-other-window 2
				      "User Option" do-keys)
		 (apropos-print-doc 'describe-variable 2
				    "Variable" do-keys))
	       (apropos-print-doc 'customize-other-window 6 "Group" do-keys)
	       (apropos-print-doc 'customize-face-other-window 5 "Face" do-keys)
	       (apropos-print-doc 'widget-browse-other-window 4 "Widget" do-keys)
	       (apropos-print-doc 'apropos-describe-plist 3
				  "Plist" nil)))))
       apropos-regexp))
    (prog1 apropos-accumulator
      (setq apropos-accumulator ()))))	; permit gc


(defun apropos-macrop (symbol)
  "Return t if SYMBOL is a Lisp macro."
  (and (fboundp symbol)
       (consp (setq symbol
		    (symbol-function symbol)))
       (or (eq (car symbol) 'macro)
	   (if (eq (car symbol) 'autoload)
	       (memq (nth 4 symbol)
		     '(macro t))))))


(defun apropos-print-doc (action i str do-keys)
  (with-current-buffer standard-output
    (if (stringp (setq i (nth i apropos-item)))
	(progn
	  (insert "  ")
	  (put-text-property (- (point) 2) (1- (point))
			     'action action)
	  (insert str ": ")
	  (if apropos-label-face
	      (add-text-properties (- (point) (length str) 2)
				   (1- (point))
				   apropos-label-face))
	  (add-text-properties (- (point) (length str) 2)
			       (1- (point))
			       (list 'keymap apropos-mode-map))
	  (insert (if do-keys (substitute-command-keys i) i))
	  (or (bolp) (terpri))))))

(defun apropos-mouse-follow (event)
  (interactive "e")
  ;; XEmacs change:  We're using the standard help buffer code now, don't
  ;; do special tricks about trying to preserve current-buffer about mouse
  ;; clicks.

  (save-excursion
    ;; XEmacs change from:
    ;; (set-buffer (window-buffer (posn-window (event-start event))))
    ;; (goto-char (posn-point (event-start event)))
    (set-buffer (event-buffer event))
    (goto-char (event-closest-point event))
    ;; XEmacs change: following code seems useless
    ;;(or (and (not (eobp)) (get-text-property (point) 'mouse-face))
    ;;	  (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
    ;;	  (error "There is nothing to follow here"))
    (apropos-follow)))


(defun apropos-follow (&optional other)
  (interactive)
  (let* (;; Properties are always found at the beginning of the line.
	 (bol (save-excursion (beginning-of-line) (point)))
	 ;; If there is no `item' property here, look behind us.
	 (item (get-text-property bol 'item))
	 (item-at (if item nil (previous-single-property-change bol 'item)))
	 ;; Likewise, if there is no `action' property here, look in front.
	 (action (get-text-property bol 'action))
	 (action-at (if action nil (next-single-property-change bol 'action))))
    (and (null item) item-at
	 (setq item (get-text-property (1- item-at) 'item)))
    (and (null action) action-at
	 (setq action (get-text-property action-at 'action)))
    (if (not (and item action))
	(error "There is nothing to follow here"))
    (if (consp item) (error "There is nothing to follow in `%s'" (car item)))
    (if other (set-buffer other))
    (funcall action item)))



(defun apropos-describe-plist (symbol)
  "Display a pretty listing of SYMBOL's plist."
  (let ((help-buffer-prefix-string "Apropos-plist"))
    (with-displaying-help-buffer
     (lambda ()
       (run-hooks 'apropos-mode-hook)
       (princ "Symbol ")
       (prin1 symbol)
       (princ "'s plist is\n (")
       (with-current-buffer standard-output
	 (if apropos-symbol-face
	     (put-text-property 8 (- (point) 14) 'face apropos-symbol-face)))
       (princ (apropos-format-plist symbol "\n  "))
       (princ ")")
       (terpri)
       (print-help-return-message))
     (symbol-name symbol))))

(provide 'apropos) ; XEmacs

;;; apropos.el ends here
