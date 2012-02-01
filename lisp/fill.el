;;; fill.el --- fill commands for SXEmacs.

;; Copyright (C) 1985, 86, 92, 94, 95, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
;; Keywords: wp, dumped

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; All the commands for filling text.  These are documented in the XEmacs
;; Reference Manual.

;; 97/3/14 Jareth Hein (jhod@po.iijnet.or.jp) added functions for kinsoku (asian text
;; line break processing)
;; 97/06/11 Steve Baur (steve@xemacs.org) converted broken
;;  following-char/preceding-char calls to char-after/char-before.

;;; Code:

(defgroup fill nil
  "Indenting and filling text."
  :group 'editing)

(defcustom fill-individual-varying-indent nil
  "*Controls criterion for a new paragraph in `fill-individual-paragraphs'.
Non-nil means changing indent doesn't end a paragraph.
That mode can handle paragraphs with extra indentation on the first line,
but it requires separator lines between paragraphs.
A value of nil means that any change in indentation starts a new paragraph."
  :type 'boolean
  :group 'fill)

(defcustom sentence-end-double-space t
  "*Non-nil means a single space does not end a sentence.
This variable applies only to filling, not motion commands.  To
change the behavior of motion commands, see `sentence-end'."
  :type 'boolean
  :group 'fill)

(defcustom colon-double-space nil
  "*Non-nil means put two spaces after a colon when filling."
  :type 'boolean
  :group 'fill)

(defvar fill-paragraph-function nil
  "Mode-specific function to fill a paragraph, or nil if there is none.
If the function returns nil, then `fill-paragraph' does its normal work.")

(defun set-fill-prefix ()
  "Set the fill prefix to the current line up to point.
Filling expects lines to start with the fill prefix and
reinserts the fill prefix in each resulting line."
  (interactive)
  (setq fill-prefix (buffer-substring
		     (save-excursion (move-to-left-margin) (point))
		     (point)))
  (if (equal fill-prefix "")
      (setq fill-prefix nil))
  (if fill-prefix
      (message "fill-prefix: \"%s\"" fill-prefix)
    (message "fill-prefix cancelled")))

(defcustom adaptive-fill-mode t
  "*Non-nil means determine a paragraph's fill prefix from its text."
  :type 'boolean
  :group 'fill)

;; #### - this is still weak.  Yeah, there's filladapt, but this should
;; still be better...  --Stig
(defcustom adaptive-fill-regexp "[ \t]*\\([#;>*]+ +\\)?"
  "*Regexp to match text at start of line that constitutes indentation.
If Adaptive Fill mode is enabled, whatever text matches this pattern
on the second line of a paragraph is used as the standard indentation
for the paragraph.  If the paragraph has just one line, the indentation
is taken from that line."
  :type 'regexp
  :group 'fill)

(defcustom adaptive-fill-function nil
  "*Function to call to choose a fill prefix for a paragraph.
This function is used when `adaptive-fill-regexp' does not match."
  :type 'function
  :group 'fill)

;; Added for kinsoku processing. Use this instead of
;; (skip-chars-backward "^ \t\n")
;; (skip-chars-backward "^ \n" linebeg)
(defun fill-move-backward-to-break-point (regexp &optional lim)
  (let ((opoint (point)))
    ;; 93.8.23 by kawamoto@ics.es.osaka-u.ac.jp
    ;;  case of first 'word' being longer than fill-column
    (if (not (re-search-backward regexp lim 'move))
	nil
      ;; we have skipped backward SPC or WAN (word-across-newline).  So move point forward again.
      (forward-char)
      (if (< opoint (point))
	  (forward-char -1)))))

;; Added for kinsoku processing. Use instead of
;; (re-search-forward "[ \t]" opoint t)
;; (skip-chars-forward "^ \n")
;; (skip-chars-forward "^ \n")
(defun fill-move-forward-to-break-point (regexp &optional lim)
  (let ((opoint (point)))
    (if (not (re-search-forward regexp lim 'move))
	nil
      (forward-char -1)
      (if (< (point) opoint)
	  (forward-char))))
  (if (featurep 'mule) (kinsoku-process-extend)))

(defun fill-end-of-sentence-p ()
  (save-excursion
    (skip-chars-backward " ]})\"'")
    (memq (char-before (point)) '(?. ?? ?!))))

(defun current-fill-column ()
  "Return the fill-column to use for this line.
The fill-column to use for a buffer is stored in the variable `fill-column',
but can be locally modified by the `right-margin' text property, which is
subtracted from `fill-column'.

The fill column to use for a line is the first column at which the column
number equals or exceeds the local fill-column - right-margin difference."
  (save-excursion
    (if fill-column
	(let* ((here (progn (beginning-of-line) (point)))
	       (here-col 0)
	       (eol (progn (end-of-line) (point)))
	       margin fill-col change col)
	  ;; Look separately at each region of line with a different right-margin.
	  (while (and (setq margin (get-text-property here 'right-margin)
			    fill-col (- fill-column (or margin 0))
			    change (text-property-not-all
				    here eol 'right-margin margin))
		      (progn (goto-char (1- change))
			     (setq col (current-column))
			     (< col fill-col)))
	    (setq here change
		  here-col col))
	  (max here-col fill-col)))))

(defun canonically-space-region (start end)
  "Remove extra spaces between words in region.
Leave one space between words, two at end of sentences or after colons
\(depending on values of `sentence-end-double-space' and `colon-double-space').
Remove indentation from each line."
  (interactive "r")
  ;;;### 97/3/14 jhod: Do I have to add anything here for kinsoku?
  (save-excursion
    (goto-char start)
    ;; XEmacs - (ENE/stig from fa-extras.el): Skip the start of a comment.
    (and comment-start-skip
	 (looking-at comment-start-skip)
	 (goto-char (match-end 0)))
    ;; Nuke tabs; they get screwed up in a fill.
    ;; This is quick, but loses when a tab follows the end of a sentence.
    ;; Actually, it is difficult to tell that from "Mr.\tSmith".
    ;; Blame the typist.
    (subst-char-in-region start end ?\t ?\ )
    (while (and (< (point) end)
		(re-search-forward "   *" end t))
      (delete-region
       (+ (match-beginning 0)
	  ;; Determine number of spaces to leave:
	  (save-excursion
	    (skip-chars-backward " ]})\"'")
	    (cond ((and sentence-end-double-space
			(memq (char-before (point)) '(?. ?? ?!)))  2)
		  ((and colon-double-space
			(eq (char-before (point)) ?:))  2)
		  ((char-equal (char-before (point)) ?\n)  0)
		  (t 1))))
       (match-end 0)))
    ;; Make sure sentences ending at end of line get an extra space.
    ;; loses on split abbrevs ("Mr.\nSmith")
    (goto-char start)
    (while (and (< (point) end)
		(re-search-forward "[.?!][])}\"']*$" end t))
      ;; We insert before markers in case a caller such as
      ;; do-auto-fill has done a save-excursion with point at the end
      ;; of the line and wants it to stay at the end of the line.
      (insert ? ))))
;; XEmacs: we don't have this function.
;; (insert-before-markers-and-inherit ? ))))

;; XEmacs -- added DONT-SKIP-FIRST.  Port of older code changes by Stig.
;; #### probably this junk is broken -- do-auto-fill doesn't actually use
;; it.  If so, it should be removed.

(defun fill-context-prefix (from to &optional first-line-regexp
				 dont-skip-first)
  "Compute a fill prefix from the text between FROM and TO.
This uses the variables `adaptive-fill-prefix' and `adaptive-fill-function'.
If FIRST-LINE-REGEXP is non-nil, then when taking a prefix from the
first line, insist it must match FIRST-LINE-REGEXP."
  (save-excursion
    (goto-char from)
    (if (eolp) (forward-line 1))
    ;; Move to the second line unless there is just one.
    (let ((firstline (point))
	  ;; Non-nil if we are on the second line.
	  at-second
	  result)
      ;; XEmacs change
      (if (not dont-skip-first)
	  (forward-line 1))
      (cond ((>= (point) to)
	     (goto-char firstline))
	    ((/= (point) from)
	     (setq at-second t)))
      (move-to-left-margin)
      ;; XEmacs change
      (let ((start (point))
	    ; jhod: no longer used?
	    ;(eol (save-excursion (end-of-line) (point)))
	    )
	(setq result
	      (if (or dont-skip-first (not (looking-at paragraph-start)))
		  (cond ((and adaptive-fill-regexp (looking-at adaptive-fill-regexp))
			 (buffer-substring-no-properties start (match-end 0)))
			(adaptive-fill-function (funcall adaptive-fill-function)))))
	(and result
	     (or at-second
		 (null first-line-regexp)
		 (string-match first-line-regexp result))
	     result)))))

;; XEmacs (stig) - this is pulled out of fill-region-as-paragraph so that it
;; can also be called from do-auto-fill
;; #### But it's not used there.  Chuck pulled it out because it broke things.
(defun maybe-adapt-fill-prefix (&optional from to dont-skip-first)
  (if (and adaptive-fill-mode
	   (or (null fill-prefix) (string= fill-prefix "")))
      (setq fill-prefix (fill-context-prefix from to nil dont-skip-first))))

(defun fill-region-as-paragraph (from to &optional justify
				      nosqueeze squeeze-after)
  "Fill the region as one paragraph.
It removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.
It leaves point at the beginning of the line following the paragraph.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification.  Fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.  Fifth arg SQUEEZE-AFTER, if non-nil,
means don't canonicalize spaces before that position.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive
   (progn
     ;; XEmacs addition:
     (barf-if-buffer-read-only nil (region-beginning) (region-end))
     (list (region-beginning) (region-end)
	   (if current-prefix-arg 'full))))
  ;; Arrange for undoing the fill to restore point.
  (if (and buffer-undo-list (not (eq buffer-undo-list t)))
      (setq buffer-undo-list (cons (point) buffer-undo-list)))

  ;; Make sure "to" is the endpoint.
  (goto-char (min from to))
  (setq to   (max from to))
  ;; Ignore blank lines at beginning of region.
  (skip-chars-forward " \t\n")

  (let ((from-plus-indent (point))
	(oneleft nil))

    (beginning-of-line)
    (setq from (point))

    ;; Delete all but one soft newline at end of region.
    ;; And leave TO before that one.
    (goto-char to)
    (while (and (> (point) from) (eq ?\n (char-after (1- (point)))))
      (if (and oneleft
	       (not (and use-hard-newlines
			 (get-text-property (1- (point)) 'hard))))
	  (delete-backward-char 1)
	(backward-char 1)
	(setq oneleft t)))
    (setq to (point))

    ;; If there was no newline, and there is text in the paragraph, then
    ;; create a newline.
    (if (and (not oneleft) (> to from-plus-indent))
	(newline))
    (goto-char from-plus-indent))

  (if (not (> to (point)))
      nil ; There is no paragraph, only whitespace: exit now.

    (or justify (setq justify (current-justification)))

    ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
    (let ((fill-prefix fill-prefix))
      ;; Figure out how this paragraph is indented, if desired.
      ;; XEmacs: move some code here to a separate function.
      (maybe-adapt-fill-prefix from to t)

      (save-restriction
	(goto-char from)
	(beginning-of-line)
	(narrow-to-region (point) to)

	(if (not justify)	    ; filling disabled: just check indentation
	    (progn
	      (goto-char from)
	      (while (not (eobp))
		(if (and (not (eolp))
			 (< (current-indentation) (current-left-margin)))
		    (indent-to-left-margin))
		(forward-line 1)))

	  (if use-hard-newlines
	      (remove-text-properties from (point-max) '(hard nil)))
	  ;; Make sure first line is indented (at least) to left margin...
	  (if (or (memq justify '(right center))
		  (< (current-indentation) (current-left-margin)))
	      (indent-to-left-margin))
	  ;; Delete the fill prefix from every line except the first.
	  ;; The first line may not even have a fill prefix.
	  (goto-char from)
	  (let ((fpre (and fill-prefix (not (equal fill-prefix ""))
			   (concat "[ \t]*"
				   (regexp-quote fill-prefix)
				   "[ \t]*"))))
	    (and fpre
		 (progn
		   (if (>= (+ (current-left-margin) (length fill-prefix))
			   (current-fill-column))
		       (error "fill-prefix too long for specified width"))
		   (goto-char from)
		   (forward-line 1)
		   (while (not (eobp))
		     (if (looking-at fpre)
			 (delete-region (point) (match-end 0)))
		     (forward-line 1))
		   (goto-char from)
		   (if (looking-at fpre)
		       (goto-char (match-end 0)))
		   (setq from (point)))))
	  ;; Remove indentation from lines other than the first.
	  (beginning-of-line 2)
	  (indent-region (point) (point-max) 0)
	  (goto-char from)

	  ;; FROM, and point, are now before the text to fill,
	  ;; but after any fill prefix on the first line.

	  ;; Make sure sentences ending at end of line get an extra space.
	  ;; loses on split abbrevs ("Mr.\nSmith")
	  (while (re-search-forward "[.?!][])}\"']*$" nil t)
	    ;; XEmacs change (no insert-and-inherit)
	    (or (eobp) (insert ?\  ?\ )))
	  (goto-char from)
	  (skip-chars-forward " \t")
	  ;; Then change all newlines to spaces.
	  ;;; 97/3/14 jhod: Kinsoku change
	  ;; Spacing is not necessary for characters of no word-separator.
	  ;; The regexp word-across-newline is used for this check.
	  (defvar word-across-newline)
	  (if (not (and (featurep 'mule)
			(stringp word-across-newline)))
	      (subst-char-in-region from (point-max) ?\n ?\ )
	    ;;
	    ;; WAN     +NL+WAN       --> WAN            + WAN
	    ;; not(WAN)+NL+WAN       --> not(WAN)       + WAN
	    ;; WAN     +NL+not(WAN)  --> WAN            + not(WAN)
	    ;; SPC     +NL+not(WAN)  --> SPC            + not(WAN)
	    ;; not(WAN)+NL+not(WAN)  --> not(WAN) + SPC + not(WAN)
	    ;;
	    (goto-char from)
	    (end-of-line)
	    (while (not (eobp))
	      ;; Insert SPC only when point is between nonWAN.  Insert
	      ;; before deleting to preserve marker if possible.
	      (if (or (prog2		; check following char.
			  (forward-char)	; skip newline
			  (or (eobp)
			      (looking-at word-across-newline))
			(forward-char -1))
		      (prog2		; check previous char.
			  (forward-char -1)
			  (or (eq (char-after (point)) ?\ )
			      (looking-at word-across-newline))
			(forward-char)))
		  nil
		(insert ?\ ))
	      (delete-char 1)		; delete newline
	      (end-of-line)))
	  ;; end patch
	  (goto-char from)
	  (skip-chars-forward " \t")
	  (if (and nosqueeze (not (eq justify 'full)))
	      nil
	    (canonically-space-region (or squeeze-after (point)) (point-max))
	    (goto-char (point-max))
	    (delete-horizontal-space)
	    ;; XEmacs change (no insert-and-inherit)
	    (insert " "))
	  (goto-char (point-min))

	  ;; This is the actual filling loop.
	  (let ((prefixcol 0) linebeg
		(re-break-point (if (featurep 'mule)
				    (concat "[ \n\t]\\|" word-across-newline
					    ".\\|." word-across-newline)
				  "[ \n\t]")))
	    (while (not (eobp))
	      (setq linebeg (point))
	      (move-to-column (1+ (current-fill-column)))
	      (if (eobp)
		  (or nosqueeze (delete-horizontal-space))
		;; Move back to start of word.
		;; 97/3/14 jhod: Kinsoku
		;(skip-chars-backward "^ \n" linebeg)
		(fill-move-backward-to-break-point re-break-point linebeg)
		;; end patch
		;; Don't break after a period followed by just one space.
		;; Move back to the previous place to break.
		;; The reason is that if a period ends up at the end of a line,
		;; further fills will assume it ends a sentence.
		;; If we now know it does not end a sentence,
		;; avoid putting it at the end of the line.
		(if sentence-end-double-space
		    (while (and (> (point) (+ linebeg 2))
				(eq (char-before (point)) ?\ )
				(not (eq (char-after (point)) ?\ ))
				(eq (char-after (- (point) 2)) ?\.))
		      (forward-char -2)
		      ;; 97/3/14 jhod: Kinsoku
		      ;(skip-chars-backward "^ \n" linebeg)))
		      (fill-move-backward-to-break-point re-break-point linebeg)))
		(if (featurep 'mule) (kinsoku-process))
		;end patch

		;; If the left margin and fill prefix by themselves
		;; pass the fill-column. or if they are zero
		;; but we have no room for even one word,
		;; keep at least one word anyway.
		;; This handles ALL BUT the first line of the paragraph.
		(if (if (zerop prefixcol)
			(save-excursion
			  (skip-chars-backward " \t" linebeg)
			  (bolp))
		      (>= prefixcol (current-column)))
		    ;; Ok, skip at least one word.
		    ;; Meanwhile, don't stop at a period followed by one space.
		    (let ((first t))
		      (move-to-column prefixcol)
		      (while (and (not (eobp))
				  (or first
				      (and (not (bobp))
					   sentence-end-double-space
					   (save-excursion (forward-char -1)
							   (and (looking-at "\\. ")
								(not (looking-at "\\.  ")))))))
			(skip-chars-forward " \t")
			;; 94/3/14 jhod: Kinsoku
			;(skip-chars-forward "^ \n\t")
			(fill-move-forward-to-break-point re-break-point)
			;; end patch
			(setq first nil)))
		  ;; Normally, move back over the single space between the words.
		  (if (eq (char-before (point)) ?\ )
		      (forward-char -1)))
		;; If the left margin and fill prefix by themselves
		;; pass the fill-column, keep at least one word.
		;; This handles the first line of the paragraph.
		(if (and (zerop prefixcol)
			 (let ((fill-point (point)) nchars)
			   (save-excursion
			     (move-to-left-margin)
			     (setq nchars (- fill-point (point)))
			     (or (< nchars 0)
				 (and fill-prefix
				      (< nchars (length fill-prefix))
				      (string= (buffer-substring (point) fill-point)
					       (substring fill-prefix 0 nchars)))))))
		    ;; Ok, skip at least one word.  But
		    ;; don't stop at a period followed by just one space.
		    (let ((first t))
		      (while (and (not (eobp))
				  (or first
				      (and (not (bobp))
					   sentence-end-double-space
					   (save-excursion (forward-char -1)
							   (and (looking-at "\\. ")
								(not (looking-at "\\.  ")))))))
			(skip-chars-forward " \t")
			;; 97/3/14 jhod: Kinsoku
			;(skip-chars-forward "^ \t\n")
			(fill-move-forward-to-break-point re-break-point)
			;; end patch
			(setq first nil))))
		;; Check again to see if we got to the end of the paragraph.
		(if (save-excursion (skip-chars-forward " \t") (eobp))
		    (or nosqueeze (delete-horizontal-space))
		  ;; Replace whitespace here with one newline, then indent to left
		  ;; margin.
		  (skip-chars-backward " \t")
		  ;; 97/3/14 jhod: More kinsoku stuff
		  (if (featurep 'mule)
		      ;; WAN means chars which match word-across-newline.
		      ;; (0)     | SPC + SPC* <EOB>	--> NL
		      ;; (1) WAN | SPC + SPC*		--> WAN + SPC + NL
		      ;; (2)     | SPC + SPC* + WAN	--> SPC + NL  + WAN
		      ;; (3) '.' | SPC + nonSPC		--> '.' + SPC + NL + nonSPC
		      ;; (4) '.' | SPC + SPC		--> '.' + NL
		      ;; (5)     | SPC*			--> NL
		      (let ((start (point))	; 92.6.30 by K.Handa
			    (ch (char-after (point))))
			(if (and (= ch ? )
				 (progn		; not case (0) -- 92.6.30 by K.Handa
				   (skip-chars-forward " \t")
				   (not (eobp)))
				 (or
				  (progn	; case (1)
				    (goto-char start)
				    (forward-char -1)
				    (looking-at word-across-newline))
				  (progn	; case (2)
				    (goto-char start)
				    (skip-chars-forward " \t")
				    (and (not (eobp))
					 (looking-at word-across-newline)
					 ;; never leave space after the end of sentence
					 (not (fill-end-of-sentence-p))))
				  (progn	; case (3)
				    (goto-char (1+ start))
				    (and (not (eobp))
					 (not (eq (char-after (point)) ? ))
					 (fill-end-of-sentence-p)))))
			    ;; We should keep one SPACE before NEWLINE. (1),(2),(3)
			    (goto-char (1+ start))
			  ;; We should delete all SPACES around break point. (4),(5)
			  (goto-char start))))
		  ;; end of patch
		  (insert ?\n)
		  ;; Give newline the properties of the space(s) it replaces
		  (set-text-properties (1- (point)) (point)
				       (text-properties-at (point)))
		  (indent-to-left-margin)
		  ;; Insert the fill prefix after indentation.
		  ;; Set prefixcol so whitespace in the prefix won't get lost.
		  (and fill-prefix (not (equal fill-prefix ""))
		       (progn
			 (insert fill-prefix)
			 (setq prefixcol (current-column))))))
	      ;; Justify the line just ended, if desired.
	      (if justify
		  (if (save-excursion (skip-chars-forward " \t") (eobp))
		      (progn
			(delete-horizontal-space)
			(justify-current-line justify t t))
		    (forward-line -1)
		    (justify-current-line justify nil t)
		    (forward-line 1))))))
	;; Leave point after final newline.
	(goto-char (point-max)))
    (forward-char 1))))

(defun fill-paragraph (arg)
  "Fill paragraph at or after point.  Prefix arg means justify as well.
If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there.

If `fill-paragraph-function' is non-nil, we call it (passing our
argument to it), and if it returns non-nil, we simply return its value."
  (interactive (list (if current-prefix-arg 'full)))
  (or (and fill-paragraph-function
	   (let ((function fill-paragraph-function)
		 fill-paragraph-function)
	     (funcall function arg)))
      (let ((before (point)))
	(save-excursion
	  (forward-paragraph)
	  (or (bolp) (newline 1))
	  (let ((end (point))
		(start (progn (backward-paragraph) (point))))
	    (goto-char before)
	    (if use-hard-newlines
		;; Can't use fill-region-as-paragraph, since this paragraph may
		;; still contain hard newlines.  See fill-region.
		(fill-region start end arg)
	      (fill-region-as-paragraph start end arg)))))))

(defun fill-region (from to &optional justify nosqueeze to-eop)
  "Fill each of the paragraphs in the region.
Prefix arg (non-nil third arg, if called from program) means justify as well.

Noninteractively, fourth arg NOSQUEEZE non-nil means to leave
whitespace other than line breaks untouched, and fifth arg TO-EOP
non-nil means to keep filling to the end of the paragraph (or next
hard newline, if `use-hard-newlines' is on).

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive
   (progn
     ;; XEmacs addition:
     (barf-if-buffer-read-only nil (region-beginning) (region-end))
     (list (region-beginning) (region-end)
	   (if current-prefix-arg 'full))))
  (let (end start)
    (save-restriction
      (goto-char (max from to))
      (if to-eop
	  (progn (skip-chars-backward "\n")
		 (forward-paragraph)))
      (setq end (point))
      (goto-char (setq start (min from to)))
      (beginning-of-line)
      (narrow-to-region (point) end)
      (while (not (eobp))
	(let ((initial (point))
	      end)
	  ;; If using hard newlines, break at every one for filling
	  ;; purposes rather than using paragraph breaks.
	  (if use-hard-newlines
	      (progn
		(while (and (setq end (text-property-any (point) (point-max)
							 'hard t))
			    (not (eq ?\n (char-after end)))
			    (not (= end (point-max))))
		  (goto-char (1+ end)))
		(setq end (if end (min (point-max) (1+ end)) (point-max)))
		(goto-char initial))
	    (forward-paragraph 1)
	    (setq end (point))
	    (forward-paragraph -1))
	  (if (< (point) start)
	      (goto-char start))
	  (if (>= (point) initial)
	      (fill-region-as-paragraph (point) end justify nosqueeze)
	    (goto-char end)))))))

;; XEmacs addition: from Tim Bradshaw <tfb@edinburgh.ac.uk>
(defun fill-paragraph-or-region (arg)
  "Fill the current region, if it's active; otherwise, fill the paragraph.
See `fill-paragraph' and `fill-region' for more information."
  (interactive "*P")
  (if (region-active-p)
      (fill-region (point) (mark) arg)
    (fill-paragraph arg)))


(defconst default-justification 'left
  "*Method of justifying text not otherwise specified.
Possible values are `left', `right', `full', `center', or `none'.
The requested kind of justification is done whenever lines are filled.
The `justification' text-property  can locally override this variable.
This variable automatically becomes buffer-local when set in any fashion.")
(make-variable-buffer-local 'default-justification)

(defun current-justification ()
  "How should we justify this line?
This returns the value of the text-property `justification',
or the variable `default-justification' if there is no text-property.
However, it returns nil rather than `none' to mean \"don't justify\"."
  (let ((j (or (get-text-property
		;; Make sure we're looking at paragraph body.
		(save-excursion (skip-chars-forward " \t")
				(if (and (eobp) (not (bobp)))
				    (1- (point)) (point)))
		'justification)
	       default-justification)))
    (if (eq 'none j)
	nil
      j)))

(defun set-justification (begin end value &optional whole-par)
  "Set the region's justification style.
The kind of justification to use is prompted for.
If the mark is not active, this command operates on the current paragraph.
If the mark is active, the region is used.  However, if the beginning and end
of the region are not at paragraph breaks, they are moved to the beginning and
end of the paragraphs they are in.
If `use-hard-newlines' is true, all hard newlines are taken to be paragraph
breaks.

When calling from a program, operates just on region between BEGIN and END,
unless optional fourth arg WHOLE-PAR is non-nil.  In that case bounds are
extended to include entire paragraphs as in the interactive command."
  ;; XEmacs change (was mark-active)
  (interactive (list (if (region-active-p) (region-beginning) (point))
		     (if (region-active-p) (region-end) (point))
		     (let ((s (completing-read
			       "Set justification to: "
			       '(("left") ("right") ("full")
				 ("center") ("none"))
			       nil t)))
		       (if (equal s "") (error ""))
		       (intern s))
		     t))
  (save-excursion
    (save-restriction
      (if whole-par
	  (let ((paragraph-start (if use-hard-newlines "." paragraph-start))
		(paragraph-ignore-fill-prefix (if use-hard-newlines t
						paragraph-ignore-fill-prefix)))
	    (goto-char begin)
	    (while (and (bolp) (not (eobp))) (forward-char 1))
	    (backward-paragraph)
	    (setq begin (point))
	    (goto-char end)
	    (skip-chars-backward " \t\n" begin)
	    (forward-paragraph)
	    (setq end (point))))

      (narrow-to-region (point-min) end)
      (unjustify-region begin (point-max))
      (put-text-property begin (point-max) 'justification value)
      (fill-region begin (point-max) nil t))))

(defun set-justification-none (b e)
  "Disable automatic filling for paragraphs in the region.
If the mark is not active, this applies to the current paragraph."
  ;; XEmacs change (was mark-active)
  (interactive (list (if (region-active-p) (region-beginning) (point))
		     (if (region-active-p) (region-end) (point))))
  (set-justification b e 'none t))

(defun set-justification-left (b e)
  "Make paragraphs in the region left-justified.
This is usually the default, but see the variable `default-justification'.
If the mark is not active, this applies to the current paragraph."
  ;; XEmacs change (was mark-active)
  (interactive (list (if (region-active-p) (region-beginning) (point))
		     (if (region-active-p) (region-end) (point))))
  (set-justification b e 'left t))

(defun set-justification-right (b e)
  "Make paragraphs in the region right-justified:
Flush at the right margin and ragged on the left.
If the mark is not active, this applies to the current paragraph."
  ;; XEmacs change (was mark-active)
  (interactive (list (if (region-active-p) (region-beginning) (point))
		     (if (region-active-p) (region-end) (point))))
  (set-justification b e 'right t))

(defun set-justification-full (b e)
  "Make paragraphs in the region fully justified:
This makes lines flush on both margins by inserting spaces between words.
If the mark is not active, this applies to the current paragraph."
  ;; XEmacs change (was mark-active)
  (interactive (list (if (region-active-p) (region-beginning) (point))
		     (if (region-active-p) (region-end) (point))))
  (set-justification b e 'full t))

(defun set-justification-center (b e)
  "Make paragraphs in the region centered.
If the mark is not active, this applies to the current paragraph."
  ;; XEmacs change (was mark-active)
  (interactive (list (if (region-active-p) (region-beginning) (point))
		     (if (region-active-p) (region-end) (point))))
  (set-justification b e 'center t))

;; 97/3/14 jhod: This functions are added for Kinsoku support
(defun find-space-insertable-point ()
 "Search backward for a permissible point for inserting justification spaces."
 (if (boundp 'space-insertable)
     (if (re-search-backward space-insertable nil t)
	 (progn (forward-char 1)
		t)
       nil)
   (search-backward " " nil t)))

;; A line has up to six parts:
;;
;;           >>>                    hello.
;; [Indent-1][FP][    Indent-2     ][text][trailing whitespace][newline]
;;
;; "Indent-1" is the left-margin indentation; normally it ends at column
;;     given by the `current-left-margin' function.
;; "FP" is the fill-prefix.  It can be any string, including whitespace.
;; "Indent-2" is added to justify a line if the `current-justification' is
;;     `center' or `right'.  In `left' and `full' justification regions, any
;;     whitespace there is part of the line's text, and should not be changed.
;; Trailing whitespace is not counted as part of the line length when
;; center- or right-justifying.
;;
;; All parts of the line are optional, although the final newline can
;;     only be missing on the last line of the buffer.

(defun justify-current-line (&optional how eop nosqueeze)
  "Do some kind of justification on this line.
Normally does full justification: adds spaces to the line to make it end at
the column given by `current-fill-column'.
Optional first argument HOW specifies alternate type of justification:
it can be `left', `right', `full', `center', or `none'.
If HOW is t, will justify however the `current-justification' function says to.
If HOW is nil or missing, full justification is done by default.
Second arg EOP non-nil means that this is the last line of the paragraph, so
it will not be stretched by full justification.
Third arg NOSQUEEZE non-nil means to leave interior whitespace unchanged,
otherwise it is made canonical."
  (interactive)
  (if (eq t how) (setq how (or (current-justification) 'none))
    (if (null how) (setq how 'full)
      (or (memq how '(none left right center))
	  (setq how 'full))))
  (or (memq how '(none left))  ; No action required for these.
      (let ((fc (current-fill-column))
	    (pos (point-marker))
	    fp-end			; point at end of fill prefix
	    start				; point at beginning of line's text
	    end				; point at end of line's text
	    indent			; column of `start'
	    endcol			; column of `end'
	    ncols)			; new indent point or offset
	(end-of-line)
	;; Check if this is the last line of the paragraph.
	(if (and use-hard-newlines (null eop)
		 (get-text-property (point) 'hard))
	    (setq eop t))
	(skip-chars-backward " \t")
	;; Quick exit if it appears to be properly justified already
	;; or there is no text.
	(if (or (bolp)
		(and (memq how '(full right))
		     (= (current-column) fc)))
	    nil
	  (setq end (point))
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  ;; Skip over fill-prefix.
	  (if (and fill-prefix
		   (not (string-equal fill-prefix ""))
		   (equal fill-prefix
			  (buffer-substring
			   (point) (min (point-max) (+ (length fill-prefix)
						       (point))))))
	      (forward-char (length fill-prefix))
	    (if (and adaptive-fill-mode
		     (looking-at adaptive-fill-regexp))
		(goto-char (match-end 0))))
	  (setq fp-end (point))
	  (skip-chars-forward " \t")
	  ;; This is beginning of the line's text.
	  (setq indent (current-column))
	  (setq start (point))
	  (goto-char end)
	  (setq endcol (current-column))

	  ;; HOW can't be null or left--we would have exited already
	  (cond ((eq 'right how)
		 (setq ncols (- fc endcol))
		 (if (< ncols 0)
		     ;; Need to remove some indentation
		     (delete-region
		      (progn (goto-char fp-end)
			     (if (< (current-column) (+ indent ncols))
				 (move-to-column (+ indent ncols) t))
			     (point))
		      (progn (move-to-column indent) (point)))
		   ;; Need to add some
		   (goto-char start)
		   (indent-to (+ indent ncols))
		   ;; If point was at beginning of text, keep it there.
		   (if (= start pos)
		       (move-marker pos (point)))))

		((eq 'center how)
		 ;; Figure out how much indentation is needed
		 (setq ncols (+ (current-left-margin)
				(/ (- fc (current-left-margin) ;avail. space
				      (- endcol indent)) ;text width
				   2)))
		 (if (< ncols indent)
		     ;; Have too much indentation - remove some
		     (delete-region
		      (progn (goto-char fp-end)
			     (if (< (current-column) ncols)
				 (move-to-column ncols t))
			     (point))
		      (progn (move-to-column indent) (point)))
		   ;; Have too little - add some
		   (goto-char start)
		   (indent-to ncols)
		   ;; If point was at beginning of text, keep it there.
		   (if (= start pos)
		       (move-marker pos (point)))))

		((eq 'full how)
		 ;; Insert extra spaces between words to justify line
		 (save-restriction
		   (narrow-to-region start end)
		   (or nosqueeze
		       (canonically-space-region start end))
		   (goto-char (point-max))
		   (setq ncols (- fc endcol))
		   ;; Ncols is number of additional spaces needed
		   (if (> ncols 0)
		       (if (and (not eop)
				;; 97/3/14 jhod: Kinsoku
				(find-space-insertable-point)) ;(search-backward " " nil t))
			   (while (> ncols 0)
			     (let ((nmove (+ 3 (random 3))))
			       (while (> nmove 0)
				 (or (find-space-insertable-point) ;(search-backward " " nil t)
				     (progn
				       (goto-char (point-max))
				       (find-space-insertable-point))) ;(search-backward " ")))
				 (skip-chars-backward " ")
				 (setq nmove (1- nmove))))
			     ;; XEmacs change
			     (insert " ")
			     (skip-chars-backward " ")
			     (setq ncols (1- ncols)))))))
		(t (error "Unknown justification value"))))
	(goto-char pos)
	(move-marker pos nil)))
  nil)

(defun unjustify-current-line ()
  "Remove justification whitespace from current line.
If the line is centered or right-justified, this function removes any
indentation past the left margin.  If the line is full-justified, it removes
extra spaces between words.  It does nothing in other justification modes."
  (let ((justify (current-justification)))
    (cond ((eq 'left justify) nil)
	  ((eq  nil  justify) nil)
	  ((eq 'full justify)		; full justify: remove extra spaces
	   (beginning-of-line-text)
	   (canonically-space-region
	    (point) (save-excursion (end-of-line) (point))))
	  ((memq justify '(center right))
	   (save-excursion
	     (move-to-left-margin nil t)
	     ;; Position ourselves after any fill-prefix.
	     (if (and fill-prefix
		      (not (string-equal fill-prefix ""))
		      (equal fill-prefix
			     (buffer-substring
			      (point) (min (point-max) (+ (length fill-prefix)
							  (point))))))
		 (forward-char (length fill-prefix)))
	     (delete-region (point) (progn (skip-chars-forward " \t")
					   (point))))))))

(defun unjustify-region (&optional begin end)
  "Remove justification whitespace from region.
For centered or right-justified regions, this function removes any indentation
past the left margin from each line.  For full-justified lines, it removes
extra spaces between words.  It does nothing in other justification modes.
Arguments BEGIN and END are optional; default is the whole buffer."
  (save-excursion
    (save-restriction
      (if end (narrow-to-region (point-min) end))
      (goto-char (or begin (point-min)))
      (while (not (eobp))
	(unjustify-current-line)
	(forward-line 1)))))


(defun fill-nonuniform-paragraphs (min max &optional justifyp mailp)
  "Fill paragraphs within the region, allowing varying indentation within each.
This command divides the region into \"paragraphs\",
only at paragraph-separator lines, then fills each paragraph
using as the fill prefix the smallest indentation of any line
in the paragraph.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFY and MAIL-FLAG:
JUSTIFY to justify paragraphs (prefix arg),
MAIL-FLAG for a mail message, i. e. don't fill header lines."
  (interactive (list (region-beginning) (region-end)
		     (if current-prefix-arg 'full)))
  (let ((fill-individual-varying-indent t))
    (fill-individual-paragraphs min max justifyp mailp)))

(defun fill-individual-paragraphs (min max &optional justify mailp)
  "Fill paragraphs of uniform indentation within the region.
This command divides the region into \"paragraphs\",
treating every change in indentation level as a paragraph boundary,
then fills each paragraph using its indentation level as the fill prefix.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFY and MAIL-FLAG:
JUSTIFY to justify paragraphs (prefix arg),
MAIL-FLAG for a mail message, i. e. don't fill header lines."
  (interactive (list (region-beginning) (region-end)
		     (if current-prefix-arg 'full)))
  (save-restriction
    (save-excursion
      (goto-char min)
      (beginning-of-line)
      (narrow-to-region (point) max)
      (if mailp
	  (while (and (not (eobp))
		      (or (looking-at "[ \t]*[^ \t\n]+:")
			  (looking-at "[ \t]*$")))
	    (if (looking-at "[ \t]*[^ \t\n]+:")
		(search-forward "\n\n" nil 'move)
		(forward-line 1))))
      (narrow-to-region (point) max)
      ;; Loop over paragraphs.
      (while (progn (skip-chars-forward " \t\n") (not (eobp)))
	(move-to-left-margin)
	(let ((start (point))
	      fill-prefix fill-prefix-regexp)
	  ;; Find end of paragraph, and compute the smallest fill-prefix
	  ;; that fits all the lines in this paragraph.
	  (while (progn
		   ;; Update the fill-prefix on the first line
		   ;; and whenever the prefix good so far is too long.
		   (if (not (and fill-prefix
				 (looking-at fill-prefix-regexp)))
		       (setq fill-prefix
			     (if (and adaptive-fill-mode adaptive-fill-regexp
				      (looking-at adaptive-fill-regexp))
				 (match-string 0)
			       (buffer-substring
				(point)
				(save-excursion (skip-chars-forward " \t")
						(point))))
			     fill-prefix-regexp (regexp-quote fill-prefix)))
		   (forward-line 1)
		   (if (bolp)
		       ;; If forward-line went past a newline
		       ;; move further to the left margin.
		       (move-to-left-margin))
		   ;; Now stop the loop if end of paragraph.
		   (and (not (eobp))
			(if fill-individual-varying-indent
			    ;; If this line is a separator line, with or
			    ;; without prefix, end the paragraph.
			    (and
			     (not (looking-at paragraph-separate))
			     (save-excursion
			       (not (and (looking-at fill-prefix-regexp)
					 ;; XEmacs change
					 (progn
					   (forward-char (length fill-prefix))
					   (looking-at paragraph-separate))))))
			    ;; If this line has more or less indent
			    ;; than the fill prefix wants, end the paragraph.
			    (and (looking-at fill-prefix-regexp)
				 (save-excursion
				   (not
				    (progn
				      (forward-char (length fill-prefix))
				      (or (looking-at paragraph-separate)
					  (looking-at paragraph-start))))))))))
	  ;; Fill this paragraph, but don't add a newline at the end.
	  (let ((had-newline (bolp)))
	    (fill-region-as-paragraph start (point) justify)
	    (or had-newline (delete-char -1))))))))

;;; fill.el ends here
