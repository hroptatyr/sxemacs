;;; isearch-mode.el --- Incremental search minor mode.

;; Copyright (C) 1992,93,94,95,96,97,98,1999 Free Software Foundation, Inc.

;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>
;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 20.4.

;;; Commentary:

;; Instructions

;; Searching with isearch-mode.el should work just like isearch.el
;; [the one from Emacs 18], except it is done in a temporary minor
;; mode that terminates when you finish searching.

;; For programmed use of isearch-mode, e.g. calling (isearch-forward),
;; isearch-mode behaves modally and does not return until the search
;; is completed.  It uses a recursive-edit to behave this way.  In
;; that case, you should still be able switch buffers, so be careful
;; not to get things confused.

;; The key bindings active within isearch-mode are defined below in
;; `isearch-mode-map' which is given bindings close to the default
;; characters of the original isearch.el.  With `isearch-mode',
;; however, you can bind multi-character keys and it should be easier
;; to add new commands.  One bug though: keys with meta-prefix cannot
;; be longer than two chars.  Also see minibuffer-local-isearch-map
;; for bindings active during `isearch-edit-string'.

;; The search ring and completion commands automatically put you in
;; the minibuffer to edit the string.  This gives you a chance to
;; modify the search string before executing the search.  There are
;; three commands to terminate the editing: C-s and C-r exit the
;; minibuffer and search forward and reverse respectively, while C-m
;; exits and does a nonincremental search.

;; Exiting immediately from isearch uses isearch-edit-string instead
;; of nonincremental-search, if search-nonincremental-instead is non-nil.
;; The name of this option should probably be changed if we decide to
;; keep the behavior.  No point in forcing nonincremental search until
;; the last possible moment.

;; TODO
;; - Integrate generalized command history to isearch-edit-string.
;; - Think about incorporating query-replace.
;; - Hooks and options for failed search.

;;; Change Log:

;; Changes before those recorded in ChangeLog:

;; 20-aug-92  Hacked by jwz for Lucid Emacs 19.3.
;;
;; Revision 1.3  92/06/29  13:10:08  liberte
;; Moved modal isearch-mode handling into isearch-mode.
;; Got rid of buffer-local isearch variables.
;; isearch-edit-string used by ring adjustments, completion, and
;; nonincremental searching.  C-s and C-r are additional exit commands.
;; Renamed all regex to regexp.
;; Got rid of found-start and found-point globals.
;; Generalized handling of upper-case chars.

;; Revision 1.2  92/05/27  11:33:57  liberte
;; Emacs version 19 has a search ring, which is supported here.
;; Other fixes found in the version 19 isearch are included here.
;;
;; Also see variables search-caps-disable-folding,
;; search-nonincremental-instead, search-whitespace-regexp, and
;; commands isearch-toggle-regexp, isearch-edit-string.
;;
;; semi-modal isearching is supported.

;; Changes for 1.1
;; 3/18/92 Fixed invalid-regexp.
;; 3/18/92 Fixed yanking in regexps.

;;; Code:


;;;=========================================================================
;;; User-accessible variables

(defgroup isearch nil
  "Incremental search minor mode."
  :prefix "search-"
  :group 'matching)


(defcustom search-exit-option t
  "*Non-nil means random control characters terminate incremental search."
  :type 'boolean
  :group 'isearch)

(defcustom search-slow-window-lines 1
  "*Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines."
  :type 'integer
  :group 'isearch)

(defcustom search-slow-speed 1200
  "*Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached."
  :type 'integer
  :group 'isearch)

;; We have `search-caps-disable-folding'.
;(defcustom search-upper-case 'not-yanks
;  "*If non-nil, upper case chars disable case fold searching.
;That is, upper and lower case chars must match exactly.
;This applies no matter where the chars come from, but does not
;apply to chars in regexps that are prefixed with `\\'.
;If this value is `not-yanks', yanked text is always downcased."
;  :type '(choice (const :tag "off" nil)
;		 (const not-yanks)
;		 (other :tag "on" t))
;  :group 'isearch)

(defcustom search-nonincremental-instead t
  "*If non-nil, do a nonincremental search instead if exiting immediately.
Actually, `isearch-edit-string' is called to let you enter the search
string, and RET terminates editing and does a nonincremental search."
  :type 'boolean
  :group 'isearch)

;; FSF default is "\\s-+", but I think our default is better so I'm
;; leaving it.
(defcustom search-whitespace-regexp "\\(\\s-\\|[\n\r]\\)+"
  "*If non-nil, regular expression to match a sequence of whitespace chars."
  :type 'regexp
  :group 'isearch)

(defcustom search-highlight t
  "*Whether incremental search and query-replace should highlight
the text that currently matches the search string."
  :type 'boolean
  :group 'isearch)

;; I think the name `search-highlight' makes more sense, both because
;; of consistency with other search-* variables above, and because it
;; also applies to query-replace.
(define-obsolete-variable-alias 'isearch-highlight 'search-highlight)

(defcustom search-invisible 'open
  "If t incremental search can match hidden text.
nil means don't match invisible text.
If the value is `open', if the text matched is made invisible by
an overlay having an `invisible' property and that overlay has a property
`isearch-open-invisible', then incremental search will show the contents.
\(This applies when using `outline.el' and `hideshow.el'.)"
  :type '(choice (const :tag "Match hidden text" t)
		 (const :tag "Open overlays" open)
		 (const :tag "Don't match hidden text" nil))
  :group 'isearch)

(defcustom isearch-hide-immediately t
  "If non-nil, re-hide an invisible match right away.
This variable makes a difference when `search-invisible' is set to `open'.
It means that after search makes some invisible text visible
to show the match, it makes the text invisible again when the match moves.
Ordinarily the text becomes invisible again at the end of the search."
  :type 'boolean
  :group 'isearch)

(defvar isearch-mode-hook nil
  "Function(s) to call after starting up an incremental search.")

(defvar isearch-mode-end-hook nil
  "Function(s) to call after terminating an incremental search.")

;;;==================================================================
;;; Search ring.

(defvar search-ring nil
  "List of search string sequences.")
(defvar regexp-search-ring nil
  "List of regular expression search string sequences.")

(defcustom search-ring-max 16
  "*Maximum length of search ring before oldest elements are thrown away."
  :type 'integer
  :group 'isearch)
(defcustom regexp-search-ring-max 16
  "*Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'isearch)

;; The important difference between pre-20.4-merge yank-pointers and
;; current code is that the yank pointers positions used to be
;; preserved across the isearch sessions.  I changed this because I
;; think the FSF code is closer to how the feature is supposed to
;; behave (read: to minibuffer histories.)
(defvar search-ring-yank-pointer nil
  "Index in `search-ring' of last string reused.
nil if none yet.")
(defvar regexp-search-ring-yank-pointer nil
  "Index in `regexp-search-ring' of last string reused.
nil if none yet.")

(defcustom search-ring-update nil
  "*Non-nil if advancing or retreating in the search ring should cause search.
Default nil means edit the string from the search ring first."
  :type 'boolean
  :group 'isearch)

;;;====================================================
;;; Define isearch-mode keymap.

(defvar isearch-mode-map
  (let ((map (make-keymap)))
    (set-keymap-name map 'isearch-mode-map)

    ;; Bind all printing characters to `isearch-printing-char'.
    ;; This isn't normally necessary, but if a printing character were
    ;; bound to something other than self-insert-command in global-map,
    ;; then it would terminate the search and be executed without this.
    (let ((i 32)
	  (str (make-string 1 0)))
      ;; #### GR (and C1 too, in KOI8 and Windows-land at least) should
      ;; be printing.  But that breaks on high-bit-is-meta brain-damage.
      ;; At least in no-mule, the high bit is treated as a meta bit.
      ;; With GR treated as printable in isearch, any meta command
      ;; events will not be executed because they are treated as GR
      ;; characters by isearch, but then there is an error because
      ;; event-to-character (properly) returns nil.
      (while (< i 127)
	(aset str 0 i)
	(define-key map str 'isearch-printing-char)
	(setq i (1+ i))))

    ;; Here FSF sets up various kludges to handle local bindings with
    ;; meta char prefix keys.  We don't need isearch-other-meta-char
    ;; because we handle things differently (via pre-command-hook).

    ;; Several non-printing chars change the searching behavior.
    ;;
    (define-key map "\C-s" 'isearch-repeat-forward)
    (define-key map "\M-\C-s" 'isearch-repeat-forward)
    (define-key map "\C-r" 'isearch-repeat-backward)
    (define-key map "\C-g" 'isearch-abort)

    ;; I wish this worked...
    ;(define-key map  [escape escape escape] 'isearch-cancel)
    (define-key map [(meta escape) escape] 'isearch-cancel)

    (define-key map "\C-q" 'isearch-quote-char)

    (define-key map "\C-m" 'isearch-exit)
    (define-key map "\C-j" 'isearch-printing-char)
    (define-key map "\t" 'isearch-printing-char)
    ;; I prefer our default.
    ;(define-key map " " 'isearch-whitespace-chars)
    (define-key map "\M- " 'isearch-whitespace-chars)

    (define-key map "\C-w" 'isearch-yank-word)
    (define-key map "\C-y" 'isearch-yank-line)
    (define-key map "\M-y" 'isearch-yank-kill)

    ;; Define keys for regexp chars * ? |.
    ;; Nothing special for + because it matches at least once.
    (define-key map "*" 'isearch-*-char)
    (define-key map "?" 'isearch-*-char)
    (define-key map "|" 'isearch-|-char)

    ;; delete and backspace delete backward, f1 is help, and C-h can be either
    (define-key map 'delete 'isearch-delete-char)
    (define-key map 'backspace 'isearch-delete-char)
    (define-key map '(control h) 'isearch-help-or-delete-char)
    (define-key map 'f1 'isearch-mode-help)
    (define-key map 'help 'isearch-mode-help)

    (define-key map "\M-n" 'isearch-ring-advance)
    (define-key map "\M-p" 'isearch-ring-retreat)
    (define-key map "\M-\t" 'isearch-complete)

    ;; I find this binding somewhat unintuitive, because it doesn't
    ;; work if the mouse pointer is over the echo area -- it has to be
    ;; over the search window.
    (define-key map 'button2 'isearch-yank-selection)

    map)
  "Keymap for isearch-mode.")

;; Some bindings you may want to put in your isearch-mode-hook.
;; Suggest some alternates...
;; (define-key isearch-mode-map "\C-t" 'isearch-toggle-case-fold)
;; (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
;; (define-key isearch-mode-map "\C-^" 'isearch-edit-string)

(defvar minibuffer-local-isearch-map
  (let ((map (make-sparse-keymap)))
    ;; #### - this should also be minor-mode-ified
    (set-keymap-parents map (list minibuffer-local-map))
    (set-keymap-name map 'minibuffer-local-isearch-map)

    ;;#### This should just arrange to use the usual Emacs minibuffer histories
    (define-key map "\r" 'isearch-nonincremental-exit-minibuffer)
    (define-key map "\M-n" 'isearch-ring-advance-edit)
    (define-key map "\M-p" 'isearch-ring-retreat-edit)
    (define-key map 'down 'isearch-ring-advance-edit)
    (define-key map 'up 'isearch-ring-retreat-edit)
    (define-key map "\M-\t" 'isearch-complete-edit)
    (define-key map "\C-s" 'isearch-forward-exit-minibuffer)
    (define-key map "\C-r" 'isearch-reverse-exit-minibuffer)
    map)
  "Keymap for editing isearch strings in the minibuffer.")

;;;========================================================
;; Internal variables declared globally for byte-compiler.
;; These are all set with setq while isearching
;; and bound locally while editing the search string.

(defvar isearch-forward nil)	; Searching in the forward direction.
(defvar isearch-regexp nil)	; Searching for a regexp.
(defvar isearch-word nil)	; Searching for words.

(defvar isearch-cmds nil)   ; Stack of search status sets.
(defvar isearch-string "")  ; The current search string.
(defvar isearch-message "") ; text-char-description version of isearch-string

(defvar isearch-success t)		; Searching is currently successful.
(defvar isearch-invalid-regexp nil)	; Regexp not well formed.
(defvar isearch-within-brackets nil)	; Regexp has unclosed [.
(defvar isearch-other-end nil)	; Start (end) of match if forward (backward).
(defvar isearch-wrapped nil)	; Searching restarted from the top (bottom).
(defvar isearch-barrier 0)
(defvar isearch-just-started nil)
(defvar isearch-buffer nil)	; the buffer we've frobbed the keymap of

(defvar isearch-case-fold-search nil)

;; Need this for toggling case in isearch-toggle-case-fold.  When this
;; is non-nil, the case-sensitiveness of the search is set by the
;; user, and is may no longer be dynamically changed as per
;; search-caps-disable-folding.
(defvar isearch-fixed-case nil)

(defvar isearch-adjusted nil)
(defvar isearch-slow-terminal-mode nil)
;;; If t, using a small window.
(defvar isearch-small-window nil)
(defvar isearch-opoint 0)
;;; The window configuration active at the beginning of the search.
(defvar isearch-window-configuration nil)
(defvar isearch-selected-frame nil)

;; Flag to indicate a yank occurred, so don't move the cursor.
(defvar isearch-yank-flag nil)

;;; A function to be called after each input character is processed.
;;; (It is not called after characters that exit the search.)
;;; It is only set from an optional argument to `isearch-mode'.
(defvar isearch-op-fun nil)

;;;  Is isearch-mode in a recursive edit for modal searching.
(defvar isearch-recursive-edit nil)

;;; Should isearch be terminated after doing one search?
(defvar isearch-nonincremental nil)

;; New value of isearch-forward after isearch-edit-string.
(defvar isearch-new-forward nil)

;; Accumulate here the extents unhidden during searching.
(defvar isearch-unhidden-extents nil)	; in FSF: isearch-opened-overlays


;;;==============================================================
;; Minor-mode-alist changes - kind of redundant with the
;; echo area, but if isearching in multiple windows, it can be useful.

(add-minor-mode 'isearch-mode 'isearch-mode)

(defvar isearch-mode nil) ;; Name of the minor mode, if non-nil.
(make-variable-buffer-local 'isearch-mode)

;; We bind these in keydefs.el.
;(define-key global-map "\C-s" 'isearch-forward)
;(define-key global-map "\C-r" 'isearch-backward)
;(define-key global-map "\M-\C-s" 'isearch-forward-regexp)
;(define-key global-map "\M-\C-r" 'isearch-backward-regexp)

;;;===============================================================
;;; Entry points to isearch-mode.
;;; These four functions should replace those in loaddefs.el
;;; An alternative is to defalias isearch-forward etc to isearch-mode,
;;; and look at this-command to set the options accordingly.

(defun isearch-forward (&optional regexp-p no-recursive-edit)
  "Do incremental search forward.
With a prefix argument, do an incremental regular expression search instead.
\\<isearch-mode-map>
As you type characters, they add to the search string and are found.
The following non-printing keys are bound in `isearch-mode-map'.

Type \\[isearch-delete-char] to cancel characters from end of search string.
Type \\[isearch-exit] to exit, leaving point at location found.
Type LFD (C-j) to match end of line.
Type \\[isearch-repeat-forward] to search again forward,
\\[isearch-repeat-backward] to search again backward.
Type \\[isearch-yank-word] to yank word from buffer onto end of search
string and search for it.
Type \\[isearch-yank-line] to yank rest of line onto end of search string
and search for it.
Type \\[isearch-yank-kill] to yank last killed text onto end of search string
and search for it.
Type \\[isearch-quote-char] to quote control character to search for it.
Type \\[isearch-whitespace-chars] to match all whitespace chars in regexp.
\\[isearch-abort] while searching or when search has failed cancels input
back to what has been found successfully.
\\[isearch-abort] when search is successful aborts and moves point to
starting point.

Also supported is a search ring of the previous 16 search strings.
Type \\[isearch-ring-advance] to search for the next item in the search ring.
Type \\[isearch-ring-retreat] to search for the previous item in the search
ring.
Type \\[isearch-complete] to complete the search string using the search ring.

The above keys are bound in the isearch-mode-map.  To change the keys which
 are special to isearch-mode, simply change the bindings in that map.

Other control and meta characters terminate the search
 and are then executed normally (depending on `search-exit-option').

If this function is called non-interactively, it does not return to
the calling function until the search is done.

The bindings, more precisely:
\\{isearch-mode-map}"

;; Non-standard bindings
;; Type \\[isearch-toggle-regexp] to toggle regular expression with normal searching.
;; Type \\[isearch-edit-string] to edit the search string in the minibuffer.
;;  Terminate editing and return to incremental searching with CR.

  (interactive "_P\np")
  (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit)))

(defun isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  "Do incremental search forward for regular expression.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive "_P\np")
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit)))

(defun isearch-backward (&optional regexp-p no-recursive-edit)
  "Do incremental search backward.
With a prefix argument, do a regular expression search instead.
See \\[isearch-forward] for more information."
  (interactive "_P\np")
  (isearch-mode nil (not (null regexp-p)) nil (not no-recursive-edit)))

(defun isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  "Do incremental search backward for regular expression.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive "_P\np")
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit)))

;; The problem here is that you can't scroll the help screen; as soon
;; as you press a key, it's gone.  I don't know of a good way to fix
;; it, though.  -hniksic
(defun isearch-mode-help ()
  (interactive "_")
  (let ((w (selected-window)))
    (describe-function 'isearch-forward)
    (select-window w))
  (isearch-update))


;;;==================================================================
;; isearch-mode only sets up incremental search for the minor mode.
;; All the work is done by the isearch-mode commands.

(defun isearch-mode (forward &optional regexp op-fun recursive-edit word-p)
  "Start isearch minor mode.  Called by `isearch-forward', etc.

\\{isearch-mode-map}"

  (if executing-kbd-macro (setq recursive-edit nil))

  (let ((inhibit-quit t)) ; don't leave things in an inconsistent state...

    ;; Initialize global vars.
    (setq isearch-buffer (current-buffer)
	  isearch-forward forward
	  isearch-regexp regexp
	  isearch-word word-p
	  isearch-op-fun op-fun
	  isearch-case-fold-search case-fold-search
	  isearch-fixed-case nil
	  isearch-string ""
	  isearch-message ""
	  isearch-cmds nil
	  isearch-success t
	  isearch-wrapped nil
	  isearch-barrier (point)
	  isearch-adjusted nil
	  isearch-yank-flag nil
	  isearch-invalid-regexp nil
	  isearch-within-brackets nil
	  isearch-slow-terminal-mode (and (<= (device-baud-rate)
					      search-slow-speed)
					  (> (window-height)
					     (* 4 search-slow-window-lines)))
	  isearch-other-end nil
	  isearch-small-window nil
	  isearch-just-started t

	  isearch-opoint (point)
	  search-ring-yank-pointer nil
	  regexp-search-ring-yank-pointer nil
	  isearch-unhidden-extents nil
	  isearch-window-configuration (current-window-configuration)

	  ;; #### What we really need is a buffer-local
	  ;; overriding-local-map.  See isearch-pre-command-hook for
	  ;; more details.
	  overriding-local-map (progn
				 (set-keymap-parents isearch-mode-map
				  (nconc (current-minor-mode-maps)
					 (and (current-local-map)
					      (list (current-local-map)))))
				 isearch-mode-map)
	  isearch-selected-frame (selected-frame)

	  )

    ;; XEmacs change: without clearing the match data, sometimes old values
    ;; of isearch-other-end get used.  Don't ask me why...
    (store-match-data nil)

    (add-hook 'pre-command-hook 'isearch-pre-command-hook)

    (setq isearch-mode (gettext " Isearch"))
    (redraw-modeline)

    (isearch-push-state)

    ) ; inhibit-quit is t before here

  (isearch-update)
  (run-hooks 'isearch-mode-hook)

  ;; isearch-mode can be made modal (in the sense of not returning to
  ;; the calling function until searching is completed) by entering
  ;; a recursive-edit and exiting it when done isearching.
  (if recursive-edit
      (let ((isearch-recursive-edit t))
	(recursive-edit)))
  isearch-success)


;;;====================================================
;; Some high level utilities.  Others below.

(defun isearch-update ()
  ;; Called after each command to update the display.
  (if (null unread-command-events)
      (progn
	(if (not (input-pending-p))
	    (isearch-message))
	(if (and isearch-slow-terminal-mode
		 (not (or isearch-small-window
			  (pos-visible-in-window-p))))
	    (let ((found-point (point)))
	      (setq isearch-small-window t)
	      (move-to-window-line 0)
	      (let ((window-min-height 1))
		(split-window nil (if (< search-slow-window-lines 0)
				      (1+ (- search-slow-window-lines))
				    (- (window-height)
				       (1+ search-slow-window-lines)))))
	      (if (< search-slow-window-lines 0)
		  (progn (vertical-motion (- 1 search-slow-window-lines))
			 (set-window-start (next-window) (point))
			 (set-window-hscroll (next-window)
					     (window-hscroll))
			 (set-window-hscroll (selected-window) 0))
		(other-window 1))
	      (goto-char found-point)))
	(if isearch-other-end
	    (if (< isearch-other-end (point))
		(isearch-highlight isearch-other-end (point))
	      (isearch-highlight (point) isearch-other-end))
	  (isearch-dehighlight))
	))
  (setq ;; quit-flag nil  not for isearch-mode
   isearch-adjusted nil
   isearch-yank-flag nil)
  (isearch-highlight-all-update)
  )


(defun isearch-done (&optional nopush edit)
  ;; Called by all commands that terminate isearch-mode.
  (let ((inhibit-quit t)) ; danger danger!
    (if (and isearch-buffer (buffer-live-p isearch-buffer))
	;; Some loser process filter might have switched the window's
	;; buffer, so be sure to set these variables back in the
	;; buffer we frobbed them in.  But only if the buffer is still
	;; alive.
	(with-current-buffer isearch-buffer
	  (setq overriding-local-map nil)
	  ;; Use remove-hook instead of just setting it to our saved value
	  ;; in case some process filter has created a buffer and modified
	  ;; the pre-command-hook in that buffer...  yeah, this is obscure,
	  ;; and yeah, I was getting screwed by it. -jwz
	  (remove-hook 'pre-command-hook 'isearch-pre-command-hook)
	  (set-keymap-parents isearch-mode-map nil)
	  (setq isearch-mode nil)
	  (redraw-modeline)
	  (isearch-dehighlight)
	  (isearch-highlight-all-cleanup)
	  (isearch-restore-invisible-extents nil nil)
	  ))

    ;; it's not critical that this be inside inhibit-quit, but leaving
    ;; things in small-window-mode would be bad.
    (let ((found-start (window-start (selected-window)))
	  (found-point (point)))
      (cond ((eq (selected-frame) isearch-selected-frame)
	     (set-window-configuration isearch-window-configuration)

	     (if isearch-small-window
		 (goto-char found-point)
	       ;; Exiting the save-window-excursion clobbers
	       ;; window-start; restore it.
	       (set-window-start (selected-window) found-start t))))
      ;; If there was movement, mark the starting position.
      ;; Maybe should test difference between and set mark iff > threshold.
      (if (and (buffer-live-p isearch-buffer)
	       (/= (point isearch-buffer) isearch-opoint))
	  ;; #### FSF doesn't do this if the region is active.  Should
	  ;; we do the same?
	  (progn
	    (push-mark isearch-opoint t nil isearch-buffer)
	    (or executing-kbd-macro (> (minibuffer-depth) 0)
		(display-message 'command "Mark saved where search started")))))
    (setq isearch-buffer nil)
    ) ; inhibit-quit is t before here

  (if (and (> (length isearch-string) 0) (not nopush))
      ;; Update the ring data.
      (isearch-update-ring isearch-string isearch-regexp))

  (run-hooks 'isearch-mode-end-hook)

  (and (not edit) isearch-recursive-edit (exit-recursive-edit)))

(defun isearch-update-ring (string &optional regexp)
  "Add STRING to the beginning of the search ring.
REGEXP says which ring to use."
  (if regexp
      (if (or (null regexp-search-ring)
	      (not (string= string (car regexp-search-ring))))
	  (progn
	    (setq regexp-search-ring
		  (cons string regexp-search-ring))
	    (if (> (length regexp-search-ring) regexp-search-ring-max)
		(setcdr (nthcdr (1- search-ring-max) regexp-search-ring)
			nil))))
    (if (or (null search-ring)
	    (not (string= string (car search-ring))))
	(progn
	  (setq search-ring (cons string search-ring))
	  (if (> (length search-ring) search-ring-max)
	      (setcdr (nthcdr (1- search-ring-max) search-ring) nil))))))


;;;====================================================
;; Commands active while inside of the isearch minor mode.

(defun isearch-exit ()
  "Exit search normally.
However, if this is the first command after starting incremental
search and `search-nonincremental-instead' is non-nil, do a
nonincremental search instead via `isearch-edit-string'."
  (interactive)
  (if (and search-nonincremental-instead
	   (= 0 (length isearch-string)))
      (let ((isearch-nonincremental t)
	    ;; Highlighting only gets in the way of nonincremental
	    ;; search.
	    (search-highlight nil)
	    (isearch-highlight-all-matches nil))
	(isearch-edit-string))
    (isearch-done)))


(defun isearch-edit-string ()
  "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-ring-advance-edit] to replace the search string with the next item in the search ring.
\\[isearch-ring-retreat-edit] to replace the search string with the previous item in the search ring.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\<isearch-mode-map>
If first char entered is \\[isearch-yank-word], then do word search instead."

  ;; This code is very hairy for several reasons, explained in the code.
  ;; Mainly, isearch-mode must be terminated while editing and then restarted.
  ;; If there were a way to catch any change of buffer from the minibuffer,
  ;; this could be simplified greatly.
  ;; Editing doesn't back up the search point.  Should it?
  (interactive)

  (condition-case nil
      (progn
	(let ((isearch-nonincremental isearch-nonincremental)

	      ;; Locally bind all isearch global variables to protect them
	      ;; from recursive isearching.
	      ;; isearch-string -message and -forward are not bound
	      ;; so they may be changed.  Instead, save the values.
	      (isearch-new-string isearch-string)
	      (isearch-new-message isearch-message)
	      (isearch-new-forward isearch-forward)
	      (isearch-new-word isearch-word)

	      (isearch-regexp isearch-regexp)
	      (isearch-op-fun isearch-op-fun)
	      (isearch-cmds isearch-cmds)
	      (isearch-success isearch-success)
	      (isearch-wrapped isearch-wrapped)
	      (isearch-barrier isearch-barrier)
	      (isearch-adjusted isearch-adjusted)
	      (isearch-fixed-case isearch-fixed-case)
	      (isearch-yank-flag isearch-yank-flag)
	      (isearch-invalid-regexp isearch-invalid-regexp)
	      (isearch-within-brackets isearch-within-brackets)
  ;;; Don't bind this.  We want isearch-search, below, to set it.
  ;;; And the old value won't matter after that.
  ;;;	    (isearch-other-end isearch-other-end)
	      (isearch-opoint isearch-opoint)
	      (isearch-slow-terminal-mode isearch-slow-terminal-mode)
	      (isearch-small-window isearch-small-window)
	      (isearch-recursive-edit isearch-recursive-edit)
	      (isearch-window-configuration (current-window-configuration))
	      (isearch-selected-frame (selected-frame))
	      )
	  ;; Actually terminate isearching until editing is done.
	  ;; This is so that the user can do anything without failure,
	  ;; like switch buffers and start another isearch, and return.
;;	(condition-case nil
	  (isearch-done t t)
	  ;;#### What does this mean?  There is no such condition!
;;	(exit nil))			; was recursive editing

	  (unwind-protect
	      (progn
		;; Fake the prompt message for the sake of
		;; next-command-event below.
		(isearch-message)
		;; If the first character the user types when we
		;; prompt them for a string is the yank-word
		;; character, then go into word-search mode.
		;; Otherwise unread that character and read a string
		;; the normal way.
		(let* ((cursor-in-echo-area t)
		       (event (next-command-event)))
		  (if (eq 'isearch-yank-word
			  (lookup-key isearch-mode-map (vector event)))
		      (setq isearch-word t;; so message-prefix is right
			    isearch-new-word t)
		    (setq unread-command-event event)))
		(setq isearch-new-string
		      (read-from-minibuffer
		       (isearch-message-prefix nil isearch-nonincremental)
		       isearch-string
		       minibuffer-local-isearch-map
		       nil
		       't		;does its own history (but shouldn't)
		       )
		      isearch-new-message (mapconcat
					   'isearch-text-char-description
					   isearch-new-string "")))
	    ;; Always resume isearching by restarting it.
	    (isearch-mode isearch-forward
			  isearch-regexp
			  isearch-op-fun
			  isearch-recursive-edit
			  isearch-word)

	    ;; Copy new values in outer locals to isearch globals
	    (setq isearch-string isearch-new-string
		  isearch-message isearch-new-message
		  isearch-forward isearch-new-forward
		  isearch-word isearch-new-word))

	  ;; Empty isearch-string means use default.
	  (if (= 0 (length isearch-string))
	      (setq isearch-string (or (car (if isearch-regexp
						regexp-search-ring
					      search-ring))
				       ""))))

	;; Reinvoke the pending search.
	(isearch-push-state)
	(isearch-search)
	(isearch-update)
	(if isearch-nonincremental (isearch-done)))

    (quit  ; handle abort-recursive-edit
     (isearch-abort)  ;; outside of let to restore outside global values
     )))

(defun isearch-nonincremental-exit-minibuffer ()
  (interactive)
  (setq isearch-nonincremental t)
  (exit-minibuffer))

(defun isearch-forward-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward t)
  (exit-minibuffer))

(defun isearch-reverse-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward nil)
  (exit-minibuffer))

(defun isearch-cancel ()
  "Terminate the search and go back to the starting point."
  (interactive)
  (goto-char isearch-opoint)
  (isearch-done t)
  (signal 'quit '(isearch)))		; and pass on quit signal

(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signaling quit.
Otherwise, revert to previous successful search and continue searching.
Use `isearch-exit' to quit without signaling."
  (interactive)
;;  (ding)  signal instead below, if quitting
  (discard-input)
  (if isearch-success
      ;; If search is successful, move back to starting point
      ;; and really do quit.
      (progn (goto-char isearch-opoint)
	     (setq isearch-success nil)
	     (isearch-done)		 ; exit and push target string
	     (signal 'quit '(isearch)))  ; and pass on quit signal
    ;; If search is failing, or has an incomplete regexp,
    ;; rub out until it is once more successful.
    (while (or (not isearch-success) isearch-invalid-regexp)
      (isearch-pop-state))
    (isearch-update)))

(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (setq isearch-string
		(or (if isearch-regexp
			(car regexp-search-ring)
		      (car search-ring))
		    "")
		isearch-message
		(mapconcat 'isearch-text-char-description
			   isearch-string ""))
	;; If already have what to search for, repeat it.
	(or isearch-success
	    (progn
	      (goto-char (if isearch-forward (point-min) (point-max)))
	      (setq isearch-wrapped t))))
    ;; C-s in reverse or C-r in forward, change direction.
    (setq isearch-forward (not isearch-forward)))

  (setq isearch-barrier (point)) ; For subsequent \| if regexp.

  (if (equal isearch-string "")
      (setq isearch-success t)
    (if (and isearch-success (equal (match-end 0) (match-beginning 0))
	     (not isearch-just-started))
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (if isearch-forward (eobp) (bobp))
	    ;; If there's nowhere to advance to, fail (and wrap next time).
	    (progn
	      (setq isearch-success nil)
	      (and executing-kbd-macro
		   (not defining-kbd-macro)
		   (isearch-done))
	      (ding nil 'isearch-failed))
	  (forward-char (if isearch-forward 1 -1))
	  (isearch-search))
      (isearch-search)))

  (isearch-push-state)
  (isearch-update))

(defun isearch-repeat-forward ()
  "Repeat incremental search forwards."
  (interactive)
  (isearch-repeat 'forward))

(defun isearch-repeat-backward ()
  "Repeat incremental search backwards."
  (interactive)
  (isearch-repeat 'backward))

(defun isearch-toggle-regexp ()
  "Toggle regexp searching on or off."
  ;; The status stack is left unchanged.
  (interactive)
  (setq isearch-regexp (not isearch-regexp))
  (if isearch-regexp (setq isearch-word nil))
  (isearch-update))

(defun isearch-toggle-case-fold ()
  "Toggle case folding in searching on or off."
  (interactive)
  (setq isearch-case-fold-search (if isearch-case-fold-search nil 'yes)
	isearch-fixed-case t)
  (lmessage 'progress "%s%s [case %ssensitive]"
    (isearch-message-prefix)
    isearch-message
    (if isearch-case-fold-search "in" ""))
  (setq isearch-adjusted t)
  ;; Update the highlighting here so that it gets done before the
  ;; one-second pause.
  (isearch-highlight-all-update)
  (sit-for 1)
  (isearch-update))

(defun isearch-delete-char ()
  "Discard last input item and move point back.
If no previous match was done, just beep."
  (interactive)
  (if (null (cdr isearch-cmds))
      (ding nil 'isearch-quit)
    (isearch-pop-state))
  (isearch-update))

(defun isearch-help-or-delete-char ()
  "Show Isearch help or delete backward in the search string.
Deletes when `delete-key-deletes-forward' is t and C-h is used for deleting
backwards."
  (interactive)
  (if (and delete-key-deletes-forward
	   (case (device-type)
	     ('tty (eq tty-erase-char ?\C-h))
	     ('x (not (x-keysym-on-keyboard-sans-modifiers-p 'backspace)))))
      (isearch-delete-char)
    (isearch-mode-help)))

;; This is similar to FSF isearch-yank-string, but more general.
(defun isearch-yank (chunk)
  ;; Helper for isearch-yank-* functions.  CHUNK can be a string or a
  ;; function.
  (let ((word (if (stringp chunk)
		  chunk
		(save-excursion
		  (and (not isearch-forward) isearch-other-end
		       (goto-char isearch-other-end))
		  (buffer-substring
		   (point)
		   (progn
		     (funcall chunk)
		     (point)))))))
    ;; if configured so that typing upper-case characters turns off case
    ;; folding, then downcase the string so that yanking an upper-case
    ;; word doesn't mess with case-foldedness.
    (if (and search-caps-disable-folding isearch-case-fold-search)
	(setq word (downcase word)))
    (if isearch-regexp (setq word (regexp-quote word)))
    (setq isearch-string (concat isearch-string word)
	  isearch-message
	  (concat isearch-message
		  (mapconcat 'isearch-text-char-description
			     word ""))
	  ;; Don't move cursor in reverse search.
	  isearch-yank-flag t))
  (isearch-search-and-update))

(defun isearch-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank (function (lambda () (forward-word 1)))))

(defun isearch-yank-line ()
  "Pull rest of line from buffer into search string."
  (interactive)
  (isearch-yank 'end-of-line))

(defun isearch-yank-kill ()
  "Pull rest of line from kill ring into search string."
  (interactive)
  (isearch-yank (current-kill 0)))

(defun isearch-yank-sexp ()
  "Pull next expression from buffer into search string."
  (interactive)
  (isearch-yank 'forward-sexp))

(defun isearch-yank-selection ()
  "Pull the current selection into the search string."
  (interactive)
  (isearch-yank (get-selection)))

(defun isearch-yank-clipboard ()
  "Pull the current clipboard selection into the search string."
  (interactive)
  (isearch-yank (get-clipboard)))

(defun isearch-fix-case ()
  ;; The commented-out (and ...) form implies that, once
  ;; isearch-case-fold-search becomes nil due to a capital letter
  ;; typed in, it can never be restored to the original value.  In
  ;; that case, it's impossible to revert a case-sensitive search back
  ;; to case-insensitive.
  (if ;(and isearch-case-fold-search search-caps-disable-folding)
      (and case-fold-search
	   ;; Make sure isearch-toggle-case-fold works.
	   (not isearch-fixed-case)
	   search-caps-disable-folding)
      (setq isearch-case-fold-search
	    (no-upper-case-p isearch-string isearch-regexp)))
  (setq isearch-mode (if case-fold-search
			 (if isearch-case-fold-search
			     " Isearch"  ;As God Intended Mode
			   " ISeARch") ;Warn about evil case via StuDLYcAps.
		       " Isearch")))

(defun isearch-search-and-update ()
  ;; Do the search and update the display.
  (if (and (not isearch-success)
	   ;; unsuccessful regexp search may become
	   ;;  successful by addition of characters which
	   ;;  make isearch-string valid
	   (not isearch-regexp))
      nil
    ;; In reverse search, adding stuff at
    ;; the end may cause zero or many more chars to be
    ;; matched, in the string following point.
    ;; Allow all those possibilities without moving point as
    ;; long as the match does not extend past search origin.
    (if (and (not isearch-forward) (not isearch-adjusted)
	     (condition-case ()
		 (progn
		   (isearch-fix-case)
		   (let ((case-fold-search isearch-case-fold-search))
		     (looking-at (if isearch-regexp isearch-string
				   (regexp-quote isearch-string)))))
	       (error nil))
	     (or isearch-yank-flag
		 (<= (match-end 0)
		     (min isearch-opoint isearch-barrier))))
	(setq isearch-success t
	      isearch-invalid-regexp nil
	      isearch-within-brackets nil
	      isearch-other-end (match-end 0))
      ;; Not regexp, not reverse, or no match at point.
      (if (and isearch-other-end (not isearch-adjusted))
	  (goto-char (if isearch-forward isearch-other-end
		       (min isearch-opoint
			    isearch-barrier
			    (1+ isearch-other-end)))))
      (isearch-search)
      ))
  (isearch-push-state)
  (if isearch-op-fun (funcall isearch-op-fun))
  (isearch-update))


;; *, ?, and | chars can make a regexp more liberal.
;; They can make a regexp match sooner or make it succeed instead of failing.
;; So go back to place last successful search started
;; or to the last ^S/^R (barrier), whichever is nearer.
;; + needs no special handling because the string must match at least once.

(defun isearch-*-char ()
  "Handle * and ? specially in regexps."
  (interactive)
  (if isearch-regexp
      (let ((idx (length isearch-string)))
	(while (and (> idx 0)
		    (eq (aref isearch-string (1- idx)) ?\\))
	  (setq idx (1- idx)))
	(when (= (mod (- (length isearch-string) idx) 2) 0)
	  (setq isearch-adjusted t)
	  ;; Get the isearch-other-end from before the last search.
	  ;; We want to start from there,
	  ;; so that we don't retreat farther than that.
	  ;; (car isearch-cmds) is after last search;
	  ;; (car (cdr isearch-cmds)) is from before it.
	  (let ((cs (nth 5 (car (cdr isearch-cmds)))))
	    (setq cs (or cs isearch-barrier))
	    (goto-char
	     (if isearch-forward
		 (max cs isearch-barrier)
	       (min cs isearch-barrier)))))))
  (isearch-process-search-char last-command-event))



(defun isearch-|-char ()
  "If in regexp search, jump to the barrier."
  (interactive)
  (if isearch-regexp
      (progn
	(setq isearch-adjusted t)
	(goto-char isearch-barrier)))
  (isearch-process-search-char last-command-event))

;; FSF:
;(defalias 'isearch-other-control-char 'isearch-other-meta-char)
;
;(defun isearch-other-meta-char ()
;...
;

(defun isearch-quote-char ()
  "Quote special characters for incremental search."
  (interactive)
  ;; #### Here FSF does some special conversion of chars in 0200-0377
  ;; range.  Maybe we should do the same.
  (isearch-process-search-char (read-quoted-char (isearch-message t))))

(defun isearch-return-char ()
  "Convert return into newline for incremental search.
Obsolete."
  (interactive)
  (isearch-process-search-char ?\n))

(defun isearch-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (let ((event last-command-event))
    ;; If we are called by isearch-whitespace-chars because the
    ;; context disallows whitespace search (e.g. within brackets),
    ;; replace M-SPC with a space.  FSF has similar code.
    (and (eq this-command 'isearch-whitespace-chars)
	 (null (event-to-character event))
	 (setq event (character-to-event ?\ )))
    (isearch-process-search-char event)))

(defun isearch-whitespace-chars ()
  "Match all whitespace chars, if in regexp mode."
  ;; FSF docstring adds: "If you want to search for just a space, type
  ;; C-q SPC."  But we don't need the addition because we have a
  ;; different (better) default for the variable.
  (interactive)
  (if isearch-regexp
      (if (and search-whitespace-regexp (not isearch-within-brackets)
	       (not isearch-invalid-regexp))
	  (isearch-process-search-string search-whitespace-regexp " ")
	(isearch-printing-char))
    (progn
      ;; This way of doing word search doesn't correctly extend current search.
      ;;      (setq isearch-word t)
      ;;      (setq isearch-adjusted t)
      ;;      (goto-char isearch-barrier)
      (isearch-printing-char))))

(defun isearch-process-search-char (char)
  ;; Append the char to the search string, update the message and re-search.
  (isearch-process-search-string (isearch-char-to-string char)
				 (isearch-text-char-description char)))

(defun isearch-process-search-string (string message)
  (setq isearch-string (concat isearch-string string)
	isearch-message (concat isearch-message message))
  (isearch-search-and-update))


;;===========================================================
;; Search Ring

(defun isearch-ring-adjust1 (advance)
  ;; Helper for isearch-ring-adjust
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
	 (length (length ring))
	 (yank-pointer-name (if isearch-regexp
				'regexp-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (mod (+ (or yank-pointer 0)
			 ;; XEmacs change
			 (if advance -1 (if yank-pointer 1 0)))
		      length)))
      (setq isearch-string (nth yank-pointer ring)
	    isearch-message (mapconcat 'isearch-text-char-description
				       isearch-string "")))))

(defun isearch-ring-adjust (advance)
  ;; Helper for isearch-ring-advance and isearch-ring-retreat
;  (if (cdr isearch-cmds)  ;; is there more than one thing on stack?
;      (isearch-pop-state))
  (isearch-ring-adjust1 advance)
  (if search-ring-update
      (progn
	(isearch-search)
	(isearch-update))
    (isearch-edit-string)
    )
  (isearch-push-state))

(defun isearch-ring-advance ()
  "Advance to the next search string in the ring."
  ;; This could be more general to handle a prefix arg, but who would use it.
  (interactive)
  (isearch-ring-adjust 'advance))

(defun isearch-ring-retreat ()
  "Retreat to the previous search string in the ring."
  (interactive)
  (isearch-ring-adjust nil))

(defun isearch-ring-advance-edit (n)
  "Insert the next element of the search history into the minibuffer."
  (interactive "p")
  (let* ((yank-pointer-name (if isearch-regexp
				'regexp-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name))
	 (ring (if isearch-regexp regexp-search-ring search-ring))
	 (length (length ring)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (mod (- (or yank-pointer 0) n)
		      length)))

      (erase-buffer)
      (insert (nth yank-pointer ring))
      (goto-char (point-max)))))

(defun isearch-ring-retreat-edit (n)
  "Inserts the previous element of the search history into the minibuffer."
  (interactive "p")
  (isearch-ring-advance-edit (- n)))

;; Merging note: FSF comments out these functions and implements them
;; differently (see above), presumably because the versions below mess
;; with isearch-string, while what we really want them to do is simply
;; to insert the correct string to the minibuffer.

;;(defun isearch-ring-adjust-edit (advance)
;;  "Use the next or previous search string in the ring while in minibuffer."
;;  (isearch-ring-adjust1 advance)
;;  (erase-buffer)
;;  (insert isearch-string))

;;(defun isearch-ring-advance-edit ()
;;  (interactive)
;;  (isearch-ring-adjust-edit 'advance))

;;(defun isearch-ring-retreat-edit ()
;;  "Retreat to the previous search string in the ring while in the minibuffer."
;;  (interactive)
;;  (isearch-ring-adjust-edit nil))


(defun isearch-complete1 ()
  ;; Helper for isearch-complete and isearch-complete-edit
  ;; Return t if completion OK, nil if no completion exists.
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
	 (alist (mapcar (function (lambda (string) (list string))) ring))
	 (completion-ignore-case case-fold-search)
	 (completion (try-completion isearch-string alist)))
    (cond
     ((eq completion t)
      ;; isearch-string stays the same
      t)
     ((or completion ; not nil, must be a string
	  (= 0 (length isearch-string))) ; shouldn't have to say this
      (if (equal completion isearch-string)  ;; no extension?
	  (progn
	    (if completion-auto-help
		(with-output-to-temp-buffer "*Isearch completions*"
		  (display-completion-list
		   (all-completions isearch-string alist))))
	    t)
	(and completion
	     (setq isearch-string completion))))
     (t
      (temp-minibuffer-message "No completion")
      nil))))

(defun isearch-complete ()
  "Complete the search string from the strings on the search ring.
The completed string is then editable in the minibuffer.
If there is no completion possible, say so and continue searching."
  (interactive)
  (if (isearch-complete1)
      (isearch-edit-string)
    ;; else
    (sit-for 1)
    (isearch-update)))

(defun isearch-complete-edit ()
  "Same as `isearch-complete' except in the minibuffer."
  (interactive)
  (setq isearch-string (buffer-string))
  (if (isearch-complete1)
      (progn
	(erase-buffer)
	(insert isearch-string))))


;;;==============================================================
;; The search status stack.

(defun isearch-top-state ()
  (let ((cmd (car isearch-cmds)))
    ;; #### Grr, this is so error-prone.  If you add something to
    ;; isearch-push-state, don't forget to update this.  I thought I'd
    ;; make a list of variables, and just do (mapcar* #'set vars
    ;; values), but the (point) thing would spoil it, leaving to more
    ;; complication.
    (setq isearch-string (car cmd)
	  isearch-message (car (cdr cmd))
	  isearch-success (nth 3 cmd)
	  isearch-forward (nth 4 cmd)
	  isearch-other-end (nth 5 cmd)
	  isearch-word (nth 6 cmd)
	  isearch-invalid-regexp (nth 7 cmd)
	  isearch-wrapped (nth 8 cmd)
	  isearch-barrier (nth 9 cmd)
	  isearch-within-brackets (nth 10 cmd))
    (goto-char (car (cdr (cdr cmd))))))

(defun isearch-pop-state ()
  (pop isearch-cmds)
  (isearch-top-state)

  ;; Make sure isearch-case-fold-search gets the correct value.  FSF
  ;; simply stores isearch-case-fold-search to isearch-cmds.  We
  ;; should probably do the same.
  (isearch-fix-case)

  ;; Here, as well as in isearch-search we must deal with the point
  ;; landing at an invisible area which may need unhiding.
  (if (or (not (eq search-invisible 'open))
	  (not isearch-hide-immediately))
      ;; If search-invisible is t, invisible text is just like any
      ;; other text.  If it is nil, it is always skipped and we can't
      ;; land inside.  In both cases, we don't need to do anything.
      ;;
      ;; Similarly, if isearch-hide-immediately is nil, needn't
      ;; re-hide the area here, and neither can we land back into a
      ;; hidden one.
      nil
    (when isearch-other-end
      ;; This will unhide the extents.
      (isearch-range-invisible (point) isearch-other-end))
    (isearch-restore-invisible-extents (point)
				       (or isearch-other-end (point)))))

(defun isearch-push-state ()
  (setq isearch-cmds
	(cons (list isearch-string isearch-message (point)
		    isearch-success isearch-forward isearch-other-end
		    isearch-word
		    isearch-invalid-regexp isearch-wrapped isearch-barrier
		    isearch-within-brackets)
	      isearch-cmds)))


;;;==================================================================
;; Message string

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; Generate and print the message string.
  (let ((cursor-in-echo-area ellipsis)
	(m (concat
	    (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
	    isearch-message
	    (isearch-message-suffix c-q-hack ellipsis)
	    )))
    (if c-q-hack
	m
      (display-message 'progress (format "%s" m)))))

(defun isearch-message-prefix (&optional c-q-hack ellipsis nonincremental)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and isearch-invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward isearch-string (point) t)
		  (setq isearch-invalid-regexp nil
			isearch-within-brackets nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or isearch-success (setq ellipsis nil))
  ;; #### - !  Emacs assembles strings all over the place, they can't
  ;; all be internationalized in the manner proposed below...  Add an
  ;; explicit call to `gettext' and have the string snarfer pluck the
  ;; english strings out of the comment below.  XEmacs is on a
  ;; purespace diet! -Stig

  ;; The comment below is dead and buried, but it can be rebuilt if
  ;; necessary.  -hniksic
  (let ((m (concat (if isearch-success nil "failing ")
		   (if (and isearch-wrapped
			    (if isearch-forward
				(> (point) isearch-opoint)
			      (< (point) isearch-opoint)))
		       "overwrapped "
		     (if isearch-wrapped "wrapped "))
		   (if isearch-word "word ")
		   (if isearch-regexp "regexp ")
		   (if nonincremental "search" "I-search")
		   (if isearch-forward nil " backward")
		   ": "
		   )))
    (aset m 0 (upcase (aref m 0)))
    (gettext m)))

(defun isearch-message-suffix (&optional c-q-hack ellipsis)
  (concat (if c-q-hack "^Q" "")
	  (if isearch-invalid-regexp
	      (concat " [" isearch-invalid-regexp "]")
	    "")))

;;;(let ((i (logior (if isearch-success 32 0)
;;;                (if isearch-wrapped 16 0)
;;;                (if isearch-word     8 0)
;;;                (if isearch-regexp   4 0)
;;;                (if nonincremental   2 0)
;;;                (if isearch-forward  1 0))))
;;;  (cond
;;;   ((= i 63) (gettext "Wrapped word regexp search: "))              ; 111111
;;;   ...and so on, ad nauseam...
;;;   ((= i  0) (gettext "Failing I-search backward: "))	       ; 000000
;;;   (t (error "Something's rotten")))))


;;;========================================================
;;; Exiting

(put 'isearch-printing-char			'isearch-command t)
(put 'isearch-return-char			'isearch-command t)
(put 'isearch-repeat-forward			'isearch-command t)
(put 'isearch-repeat-backward			'isearch-command t)
(put 'isearch-delete-char			'isearch-command t)
(put 'isearch-help-or-delete-char		'isearch-command t)
(put 'isearch-cancel				'isearch-command t)
(put 'isearch-abort				'isearch-command t)
(put 'isearch-quote-char			'isearch-command t)
(put 'isearch-exit				'isearch-command t)
(put 'isearch-printing-char			'isearch-command t)
(put 'isearch-printing-char			'isearch-command t)
(put 'isearch-yank-word				'isearch-command t)
(put 'isearch-yank-line				'isearch-command t)
(put 'isearch-yank-kill				'isearch-command t)
(put 'isearch-yank-sexp				'isearch-command t)
(put 'isearch-*-char				'isearch-command t)
(put 'isearch-*-char				'isearch-command t)
(put 'isearch-|-char				'isearch-command t)
(put 'isearch-toggle-regexp			'isearch-command t)
(put 'isearch-toggle-case-fold			'isearch-command t)
(put 'isearch-edit-string			'isearch-command t)
(put 'isearch-mode-help				'isearch-command t)
(put 'isearch-ring-advance			'isearch-command t)
(put 'isearch-ring-retreat			'isearch-command t)
(put 'isearch-ring-advance-edit			'isearch-command t)
(put 'isearch-ring-retreat-edit			'isearch-command t)
(put 'isearch-whitespace-chars			'isearch-command t)
(put 'isearch-complete				'isearch-command t)
(put 'isearch-complete-edit			'isearch-command t)
(put 'isearch-edit-string			'isearch-command t)
(put 'isearch-toggle-regexp			'isearch-command t)
(put 'isearch-forward-exit-minibuffer		'isearch-command t)
(put 'isearch-reverse-exit-minibuffer		'isearch-command t)
(put 'isearch-nonincremental-exit-minibuffer	'isearch-command t)
(put 'isearch-yank-selection			'isearch-command t)
(put 'isearch-yank-clipboard			'isearch-command t)
(put 'isearch-yank-x-selection			'isearch-command t)
(put 'isearch-yank-x-clipboard			'isearch-command t)

;; scrolling the scrollbar should not terminate isearch.

;; vertical scrollbar:
(put 'scrollbar-line-up				'isearch-command t)
(put 'scrollbar-line-down			'isearch-command t)
(put 'scrollbar-page-up				'isearch-command t)
(put 'scrollbar-page-down			'isearch-command t)
(put 'scrollbar-to-top				'isearch-command t)
(put 'scrollbar-to-bottom			'isearch-command t)
(put 'scrollbar-vertical-drag			'isearch-command t)

;; horizontal scrollbar:
(put 'scrollbar-char-left			'isearch-command t)
(put 'scrollbar-char-right			'isearch-command t)
(put 'scrollbar-page-left			'isearch-command t)
(put 'scrollbar-page-right			'isearch-command t)
(put 'scrollbar-to-left				'isearch-command t)
(put 'scrollbar-to-right			'isearch-command t)
(put 'scrollbar-horizontal-drag			'isearch-command t)

(defun isearch-pre-command-hook ()
  ;;
  ;; For use as the value of `pre-command-hook' when isearch-mode is active.
  ;; If the command about to be executed is not one of the isearch commands,
  ;; then isearch-mode is turned off before that command is executed.
  ;;
  ;; If the command about to be executed is self-insert-command, or is a
  ;; keyboard macro of a single key sequence which is bound to self-insert-
  ;; command, then we add those chars to the search ring instead of inserting
  ;; them in the buffer.  In this way, the set of self-searching characters
  ;; need not be exhaustively enumerated, but is derived from other maps.
  ;;
  (cond ((not (eq (current-buffer) isearch-buffer))
	 ;; If the buffer (likely meaning "frame") has changed, bail.
	 ;; This can happen if the user types something into another
	 ;; frame.  It can also happen if a proc filter has popped up
	 ;; another buffer, which is arguably a bad thing for it to
	 ;; have done, but the way in which isearch would have hosed
	 ;; you in that case is unarguably even worse. -jwz
	 (isearch-done)

	 ;; `this-command' is set according to the value of
	 ;; `overriding-local-map', set by isearch-mode.  This is
	 ;; wrong because that keymap makes sense only in isearch
	 ;; buffer.  To make sure the right command is called, adjust
	 ;; `this-command' to the appropriate value, now that
	 ;; `isearch-done' has set `overriding-local-map' to nil.

	 ;; FSF does similar magic in `isearch-other-meta-char', which
	 ;; is horribly complex.  I *hope* what we do works in all
	 ;; cases.
	 (setq this-command (key-binding (this-command-keys))))
	(t
	 (isearch-maybe-frob-keyboard-macros)
	 (if (and this-command
		  (symbolp this-command)
		  (get this-command 'isearch-command))
	     nil ; then continue.
	   (isearch-done)))))

(defun isearch-maybe-frob-keyboard-macros ()
  ;;
  ;; If the command about to be executed is `self-insert-command' then change
  ;; the command to `isearch-printing-char' instead, meaning add the last-
  ;; typed character to the search string.
  ;;
  ;; If `this-command' is a string or a vector (that is, a keyboard macro)
  ;; and it contains only one command, which is bound to self-insert-command,
  ;; then do the same thing as for self-inserting commands: arrange for that
  ;; character to be added to the search string.  If we didn't do this, then
  ;; typing a compose sequence (a la x-compose.el) would terminate the search
  ;; and insert the character, instead of searching for that character.
  ;;
  ;; We should continue doing this, since it's pretty much the behavior one
  ;; would expect, but it will stop being so necessary once key-translation-
  ;; map exists and is used by x-compose.el and things like it, since the
  ;; translation will have been done before we see the keys.
  ;;
  (cond ((eq this-command 'self-insert-command)
	 (setq this-command 'isearch-printing-char))
	((and (or (stringp this-command) (vectorp this-command))
	      (eq (key-binding this-command) 'self-insert-command))
	 (setq last-command-event (character-to-event (aref this-command 0))
	       last-command-char (and (stringp this-command)
				      (aref this-command 0))
	       this-command 'isearch-printing-char))
	))


;;;========================================================
;;; Highlighting

(defvar isearch-extent nil)

;; this face is initialized by faces.el since isearch is preloaded.
;(make-face 'isearch)

(defun isearch-make-extent (begin end)
  (let ((x (make-extent begin end (current-buffer))))
    ;; make the isearch extent always take precedence over any mouse-
    ;; highlighted extents we may be passing through, since isearch, being
    ;; modal, is more interesting (there's nothing they could do with a
    ;; mouse-highlighted extent while in the midst of a search anyway).
    (set-extent-priority x (+ mouse-highlight-priority 2))
    (set-extent-face x 'isearch)
    (setq isearch-extent x)))

(defun isearch-highlight (begin end)
  (if (null search-highlight)
      nil
    ;; make sure isearch-extent is in the current buffer
    (or (and (extentp isearch-extent)
	     (extent-live-p isearch-extent))
	(isearch-make-extent begin end))
    (set-extent-endpoints isearch-extent begin end (current-buffer))))

;; This used to have a TOTALLY flag that also deleted the extent.  I
;; don't think this is necessary any longer, as isearch-highlight can
;; simply move the extent to another buffer.  The IGNORED argument is
;; for the code that calls this function with an argument.  --hniksic
(defun isearch-dehighlight (&optional ignored)
  (and search-highlight
       (extentp isearch-extent)
       (extent-live-p isearch-extent)
       (detach-extent isearch-extent)))


;;;========================================================
;;; Searching

(defun isearch-search ()
  ;; Do the search with the current search string.
  (isearch-message nil t)
  (isearch-fix-case)
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search)
	    (retry t))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
	(setq isearch-within-brackets nil)
	(while retry
	  (setq isearch-success
		(funcall
		 (cond (isearch-word
			(if isearch-forward
			    'word-search-forward 'word-search-backward))
		       (isearch-regexp
			(if isearch-forward
			    're-search-forward 're-search-backward))
		       (t
			(if isearch-forward 'search-forward 'search-backward)))
		 isearch-string nil t))
	  ;; Clear RETRY unless we matched some invisible text
	  ;; and we aren't supposed to do that.
	  (if (or (eq search-invisible t)
		  (not isearch-success)
		  (bobp) (eobp)
		  (= (match-beginning 0) (match-end 0))
		  (not (isearch-range-invisible
			(match-beginning 0) (match-end 0))))
	      (setq retry nil)))
	(setq isearch-just-started nil)
	(when isearch-success
	  (setq isearch-other-end
		(if isearch-forward (match-beginning 0) (match-end 0)))
	  (and isearch-hide-immediately
	       (isearch-restore-invisible-extents (match-beginning 0)
						  (match-end 0)))))

    (quit (setq unread-command-events (nconc unread-command-events
					     (character-to-event (quit-char))))
	  (setq isearch-success nil))

    (invalid-regexp
     (setq isearch-invalid-regexp (car (cdr lossage)))
     (setq isearch-within-brackets (string-match #r"\`Unmatched \["
						 isearch-invalid-regexp))
     (if (string-match
	  #r"\`Premature \|\`Unmatched \|\`Invalid "
	  isearch-invalid-regexp)
	 (setq isearch-invalid-regexp (gettext "incomplete input"))))
    (error
     ;; stack overflow in regexp search.
     (setq isearch-invalid-regexp (car (cdr lossage)))))

  (if isearch-success
      nil

    ;; If we're being run inside a keyboard macro, then the call to
    ;; ding will signal an error (to terminate the macro).  We must
    ;; turn off isearch-mode first, so that we aren't still in isearch
    ;; mode after the macro exits.  Note that isearch-recursive-edit
    ;; must not be true if a keyboard macro is executing.
    (if (and executing-kbd-macro (not defining-kbd-macro))
	(progn
	  (isearch-done)
	  (ding nil 'isearch-failed)))

    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car isearch-cmds))
	 (ding nil 'isearch-failed))
    (goto-char (nth 2 (car isearch-cmds)))))

;; Replaced with isearch-edit-string.
;(defun nonincremental-search (forward regexp)
;...

(defun isearch-unhide-extent (extent)
  ;; Store the values for the `invisible' and `intangible'
  ;; properties, and then set them to nil. This way the text hidden
  ;; by this extent becomes visible.
  (put extent 'isearch-invisible (get extent 'invisible))
  (put extent 'isearch-intangible (get extent 'intangible))
  (put extent 'invisible nil)
  (put extent 'intangible nil))

(defun isearch-range-invisible (start end)
  "Return t if all the text from START to END is invisible.
Before that, if search-invisible is `open', unhide the extents with an
`isearch-open-invisible' property."
  ;; isearch-search uses this to skip the extents that are invisible,
  ;; but don't have `isearch-open-invisible' set.  It is unclear
  ;; what's supposed to happen if only a part of [START, END) overlaps
  ;; the extent.
  (let (to-be-unhidden)
    (if (map-extents
	 (lambda (extent ignored)
	   (if (and (<= (extent-start-position extent) start)
		    (>= (extent-end-position extent) end))
	       ;; All of the region is covered by the extent.
	       (if (and (eq search-invisible 'open)
			(get extent 'isearch-open-invisible))
		   (progn
		     (push extent to-be-unhidden)
		     nil)		; keep mapping
		 ;; We can't or won't unhide this extent, so we must
		 ;; skip the whole match.  We return from map-extents
		 ;; immediately.
		 t)
	     ;; Else, keep looking.
	     nil))
	 nil start end nil 'all-extents-closed 'invisible)
	;; The whole match must be skipped.  Signal it by returning t
	;; to the caller.
	t
      ;; If any extents need to be unhidden, unhide them.
      (mapc #'isearch-unhide-extent to-be-unhidden)
      ;; Will leave this assert for some time, to catch bugs.
      (assert (null (intersection to-be-unhidden isearch-unhidden-extents)))
      (setq isearch-unhidden-extents (nconc to-be-unhidden
					    isearch-unhidden-extents))
      nil)))

(defun isearch-restore-extent (extent)
  (put extent 'invisible (get extent 'isearch-invisible))
  (put extent 'intangible (get extent 'isearch-intangible))
  (remprop extent 'isearch-invisible)
  (remprop extent 'isearch-intangible))

;; FSF calls this function `isearch-clean-overlays'.
(defun isearch-restore-invisible-extents (start end)
  (cond
   ((null start)
    ;; Delete all -- this is called at the end of isearch.
    (mapc #'isearch-restore-extent isearch-unhidden-extents)
    (setq isearch-unhidden-extents nil))
   (t
    ;; Extents that do not overlap the match area can be safely
    ;; restored to their hidden state.
    (setq isearch-unhidden-extents
	  (delete-if (lambda (extent)
		       (unless (extent-in-region-p extent start end
						   'all-extents-closed)
			 (isearch-restore-extent extent)
			 t))
		     isearch-unhidden-extents)))))

(defun isearch-no-upper-case-p (string)
  "Return t if there are no upper case chars in string.
But upper case chars preceded by \\ do not count since they
have special meaning in a regexp."
  ;; this incorrectly returns t for "\\\\A"
  (let ((case-fold-search nil))
    (not (string-match #r"\(^\|[^\]\)[A-Z]" string))))
(make-obsolete 'isearch-no-upper-case-p 'no-upper-case-p)

;; Portability functions to support various Emacs versions.

(defun isearch-char-to-string (c)
  (if (eventp c)
      (make-string 1 (event-to-character c nil nil t))
    (make-string 1 c)))

;(defun isearch-text-char-description (c)
;  (isearch-char-to-string c))

(define-function 'isearch-text-char-description 'text-char-description)

;; Used by etags.el and info.el
(defmacro with-caps-disable-folding (string &rest body)
  "Eval BODY with `case-fold-search' let to nil if STRING contains
uppercase letters and `search-caps-disable-folding' is t."
  `(let ((case-fold-search
	  (if (and case-fold-search search-caps-disable-folding)
	      (isearch-no-upper-case-p ,string)
	    case-fold-search)))
     ,@body))
(make-obsolete 'with-caps-disable-folding 'with-search-caps-disable-folding)
(put 'with-caps-disable-folding 'lisp-indent-function 1)
(put 'with-caps-disable-folding 'edebug-form-spec '(form body))


;;;========================================================
;;; Advanced highlighting

;; When active, *every* visible match for the current search string is
;; highlighted: the current one using the normal isearch match color
;; and all the others using the `isearch-secondary' face.  The extra
;; highlighting makes it easier to anticipate where the cursor will
;; land each time you press C-s or C-r to repeat a pending search.
;; Only the matches visible at any point are highlighted -- when you
;; move through the buffer, the highlighting is readjusted.

;; This is based on ideas from Bob Glickstein's `ishl' package.  It
;; has been merged with XEmacs by Darryl Okahata, and then completely
;; rewritten by Hrvoje Niksic.

;; The code makes the following assumptions about the rest of this
;; file, so be careful when modifying it.

;; * `isearch-highlight-all-update' should get called when the search
;;   string changes, or when the search advances.  This is done from
;;   `isearch-update'.
;; * `isearch-highlight-all-cleanup' should get called when the search
;;   is done.  This is performed in `isearch-done'.
;; * `isearch-string' is expected to contain the current search string
;;   as entered by the user.
;; * `isearch-opoint' is expected to contain the location where the
;;   current search began.
;; * the type of the current search is expected to be given by
;;   `isearch-word' and `isearch-regexp'.
;; * the variable `isearch-invalid-regexp' is expected to be true iff
;;   `isearch-string' is an invalid regexp.

(defcustom isearch-highlight-all-matches search-highlight
  "*Non-nil means highlight all visible matches."
  :type 'boolean
  :group 'isearch)

;; We can't create this face here, as isearch.el is preloaded.
;; #### Think up a better name for this!
;(defface isearch-secondary '((t (:foreground "red3")))
;  "Face to use for highlighting all matches."
;  :group 'isearch)

(defvar isearch-highlight-extents nil)
(defvar isearch-window-start nil)
(defvar isearch-window-end nil)
;; We compare isearch-string and isearch-case-fold-search to saved
;; values for better efficiency.
(defvar isearch-highlight-last-string nil)
(defvar isearch-highlight-last-case-fold-search nil)
(defvar isearch-highlight-last-regexp nil)

(defun isearch-delete-extents-in-range (start end)
  ;; Delete all highlighting extents that overlap [START, END).
  (setq isearch-highlight-extents
	(delete-if (lambda (extent)
		     (when (extent-in-region-p extent start end)
		       (delete-extent extent)
		       t))
		   isearch-highlight-extents)))

(defun isearch-highlight-all-cleanup ()
  ;; Stop lazily highlighting and remove extra highlighting from
  ;; buffer.
  (mapc #'delete-extent isearch-highlight-extents)
  (setq isearch-highlight-extents nil)
  (setq isearch-window-end nil
	isearch-highlight-last-string nil))

(defun isearch-highlight-all-update ()
  ;; Update the highlighting if necessary.  This needs to check if the
  ;; search string has changed, or if the window has changed position
  ;; in the buffer.
  (let ((need-start-over nil))
    ;; NB: we don't check for isearch-success because if the point is
    ;; after the last match, the search can be unsuccessful, and yet
    ;; there are things to highlight.
    (cond ((not isearch-highlight-all-matches))
	  ((or (equal isearch-string "")
	       isearch-invalid-regexp)
	   (isearch-highlight-all-cleanup))
	  ((not (eq isearch-case-fold-search
		    isearch-highlight-last-case-fold-search))
	   ;; This case is usually caused by search string being
	   ;; changed, which would be caught below, but it can also be
	   ;; tripped using isearch-toggle-case-fold.
	   (setq need-start-over t))
	  ((not (eq isearch-regexp isearch-highlight-last-regexp))
	   ;; Ditto for isearch-toggle-regexp.
	   (setq need-start-over t))
	  ((equal isearch-string isearch-highlight-last-string)
	   ;; The search string is the same.  We need to do something
	   ;; if our position has changed.

	   ;; It would be nice if we didn't have to do this; however,
	   ;; window-start doesn't support a GUARANTEE flag, so we must
	   ;; force redisplay to get the correct value for start and end
	   ;; of window.
	   (sit-for 0)

	   ;; Check whether our location has changed.
	   (let ((start (window-start))
		 (end (min (window-end) (point-max))))
	     (cond ((and (= start isearch-window-start)
			 (= end isearch-window-end))
		    ;; Our position is unchanged -- do nothing.
		    )
		   ((and (> start isearch-window-start)
			 (> end isearch-window-end)
			 (<= start isearch-window-end))
		    ;; We've migrated downward, but we overlap the old
		    ;; region.  Delete the old non-overlapping extents
		    ;; and fill in the rest.
		    (isearch-delete-extents-in-range isearch-window-start start)
		    (isearch-highlightify-region isearch-window-end end)
		    (setq isearch-window-start start
			  isearch-window-end   end))
		   ((and (<= start isearch-window-start)
			 (<= end isearch-window-end)
			 (> end isearch-window-start))
		    ;; We've migrated upward, but we overlap the old
		    ;; region.  Delete the old non-overlapping extents
		    ;; and fill in the rest.
		    (isearch-delete-extents-in-range
		     end isearch-window-end)
		    (isearch-highlightify-region start isearch-window-start)
		    (setq isearch-window-start start
			  isearch-window-end   end))
		   (t
		    ;; The regions don't overlap, or they overlap in a
		    ;; weird way.
		    (setq need-start-over t)))))
	  (t
	   ;; The search string has changed.

	   ;; If more input is pending, don't start over because
	   ;; starting over forces redisplay, and that slows down
	   ;; typing.
	   (unless (input-pending-p)
	     (setq need-start-over t))))
    (when need-start-over
      ;; Force redisplay before removing the old extents, in order to
      ;; avoid flicker.
      (sit-for 0)
      (isearch-highlight-all-cleanup)
      (setq isearch-window-start (window-start)
	    isearch-window-end   (min (window-end) (point-max)))
      (isearch-highlightify-region isearch-window-start isearch-window-end))

    (setq isearch-highlight-last-string isearch-string
	  isearch-highlight-last-case-fold-search isearch-case-fold-search
	  isearch-highlight-last-regexp isearch-regexp)))

(defun isearch-highlight-advance (string forwardp)
  ;; Search ahead for the next or previous match.  This is the same as
  ;; isearch-search, but without the extra baggage.  Maybe it should
  ;; be in a separate function.
  (let ((case-fold-search isearch-case-fold-search))
    (funcall (cond (isearch-word (if forwardp
				     'word-search-forward
				   'word-search-backward))
		   (isearch-regexp (if forwardp
				       're-search-forward
				     're-search-backward))
		   (t (if forwardp
			  'search-forward
			'search-backward)))
	     string nil t)))

(defun isearch-highlightify-region (start end)
  ;; Highlight all occurrences of isearch-string between START and
  ;; END.  To do this right, we have to search forward as long as
  ;; there are matches that overlap [START, END), and then search
  ;; backward the same way.
  (save-excursion
    (goto-char isearch-opoint)
    (let ((lastpoint (point)))
      (while (and (isearch-highlight-advance isearch-string t)
		  (/= lastpoint (point))
		  (< (match-beginning 0) end))
	(let ((extent (make-extent (match-beginning 0)
				   (match-end 0))))
	  (set-extent-priority extent (1+ mouse-highlight-priority))
	  (put extent 'face 'isearch-secondary)
	  (push extent isearch-highlight-extents))
	(setq lastpoint (point))))
    (goto-char isearch-opoint)
    (let ((lastpoint (point)))
      (while (and (isearch-highlight-advance isearch-string nil)
		  (/= lastpoint (point))
		  (>= (match-end 0) start))
	(let ((extent (make-extent (match-beginning 0)
				   (match-end 0))))
	  (set-extent-priority extent (1+ mouse-highlight-priority))
	  (put extent 'face 'isearch-secondary)
	  (push extent isearch-highlight-extents))
	(setq lastpoint (point))))))

;;; isearch-mode.el ends here
