;;; apollo.el --- Apollo Graphics Primitive Support Functions

;; Copyright (C) 1998 by Free Software Foundation, Inc.
;; Copyright (C) 1991 by Lucid, Inc.

;; Author: Leonard N. Zubkoff <lnz@dandelion.com>
;; Keywords: hardware

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

;;; Synched up with: InfoDock 3.6.2.

;;; Commentary:

;;		    GNU Emacs Apollo GPR Support Functions

;;			      Leonard N. Zubkoff

;;				 lnz@dandelion.com
;;			      Lucid, Incorporated
;;				23 January 1991

;; This file defines functions that support GNU Emacs using the Apollo
;; Graphics Primitives (GPR).  See the file "APOLLO.README" for a description
;; of the key bindings set up by this file.

;;			       Acknowledgements

;; The following people have contributed ideas that have helped make this
;; interface possible: Nathaniel Mishkin, Rob Stanzel, and Mark Weissman of
;; Apollo Computer, Dave Holcomb of CAECO, Vincent Broman of NOSC, and J. W.
;; Peterson of the University of Utah.

;;; Change Log:
;; Bob Weiner, Motorola, Inc., 2/2/89

;; Added section to 'apollo-clean-help-file()' to remove underlining
;;   and overstriking (only by the same letter) from Apollo '.hlp' files.
;;   Based on the 'nuke-nroff-bs' function in man.el.
;; Changed apollo-mouse-{cut,copy,paste} commands so that they work
;;   with the DM paste buffer.  This combined with cut,copy,paste
;;   bindings of the mouse keys allows quick and easy copying from
;;   Emacs windows to DM windows.
;; Added 'unbind-apollo-mouse-button' and 'unbind-apollo-function-key'
;;   commands.
;; Added 'apollo-mouse-cut-copy-paste' command which provides a
;;   second set of mouse key functions that can be set with one key
;;   press and cleared with another key press.  Put default mouse key
;;   bindings into a command called 'apollo-mouse-defaults' so that
;;   they can be used to clear any other mouse bindings.
;; Both these commands affect the DM mouse key bindings as well.
;; Added 'apollo-mouse-cancel-cut-copy-paste' command which resets the mouse
;;   key defaults within Emacs and the DM.  The variable
;;   '*dm-mouse-key-bindings-file*' should be set within an initialization
;;   file to the pathname of file that executes a user's default DM mouse
;;   key bindings.

;; Bob Weiner, Motorola, Inc., 2/23/89

;; Added ':' as valid character within a filename (if not at the end)
;; in the command 'extract-file-name-around-point'.  For remote UNIX
;; operations such as rcp and rsh commands which use the syntax,
;; <host>:<path>.

;; Bob Weiner, Motorola, Inc., 3/09/89
;;
;; Modified 'apollo-mouse-find-file' and 'apollo-find-file' so that they
;; recognize buffer names in addition to directory or file paths.  A buffer
;; name is recognized before a path name, if the match buffer names flag is
;; enabled.  Added the command 'extract-buf-or-file-name-around-point' to
; support this functionality.  Added find file in other window option to
;; these two find-file commands.

;; Bob Weiner, Motorola, Inc., 3/20/89

;; Changed (funcall *apollo-key-bindings-hook*) to (run-hooks
;; '*apollo-key-bindings-hook*) which is what it should be.

;; Bob Weiner, Motorola, Inc., 4/20/89

;; Rebound M2D button to perform different functions by buffer and location in
;; buffer.  Executes 'smart-key-mouse' command found in smart-key.el.
;; Meta-M2D executes 'smart-key-mouse-meta'.  M2U is unbound.

;; Bob Weiner, Motorola, Inc., 8/1/89

;; Fixed 'apollo-mouse-move-point' and 'apollo-mouse-move-mark' so they do
;; not set the mark gratuitously.  They are bound to M1D and M1U respectively.

;; Bob Weiner, Motorola, Inc., 4/11/90

;; Bound left and right box arrow keys to scroll right and left,
;; respectively, which most closely emulates their DM functions.

;;; Code:
(eval-when-compile
  (globally-declare-fboundp
   '(enable-apollo-function-key disable-apollo-function-key
				set-apollo-meta-key enable-apollo-mouse-button
				disable-apollo-mouse-button
				write-region-to-default-apollo-paste-buffer
				insert-contents-of-default-apollo-paste-buffer
				execute-dm-command window-edges)))


(defvar *dm-mouse-key-bindings-file* "/sys/dm/std_keys3"
  "Path of the DM key binding file which sets up a user's default mouse key
bindings.  If none exists, this value should be set to one of the
/sys/dm/std_key* files which set up DM key defaults.")

;;; Set this variable in your ".emacs" to a function to call to set up
;;; additional key bindings.
;;;
(defvar *apollo-key-bindings-hook* nil)

;;; Set this variable non-NIL in your ".emacs" to enable preemption of normal
;;; Display Manager bindings.
;;;
(defvar *preempt-display-manager-bindings* nil)



;;; Determine whether or not we're running diskless and define
;;; *paste-buffer-directory* to point to the paste buffers directory.

(defvar *paste-buffer-directory*
  (let ((test-directory (concat "/sys/node_data."
				(downcase (getenv "NODEID"))
				"paste_buffers/")))
    (if (file-directory-p test-directory)
	test-directory
	"/sys/node_data/paste_buffers/")))


;;; Bind this variable non-NIL to allow apollo-mouse-move-point to leave the
;;; minibuffer area.

(defvar *apollo-mouse-move-point-allow-minibuffer-exit* nil)


;;; Define the Apollo Function Keys.

(defvar *apollo-function-keys*
  '(("ESC" . 0) ("L1" . 1) ("L2" . 2) ("L3" . 3)
    ("L1A" . 4) ("L2A" . 5) ("L3A" . 6) ("L4" . 7)
    ("L5" . 8) ("L6" . 9) ("L7" . 10) ("L8" . 11)
    ("L9" . 12) ("LA" . 13) ("LB" . 14) ("LC" . 15)
    ("LD" . 16) ("LE" . 17) ("LF" . 18) ("F0" . 19)
    ("F1" . 20) ("F2" . 21) ("F3" . 22) ("F4" . 23)
    ("F5" . 24) ("F6" . 25) ("F7" . 26) ("F8" . 27)
    ("F9" . 28) ("R1" . 29) ("R2" . 30) ("R3" . 31)
    ("R4" . 32) ("R5" . 33) ("R6" . 34) ("NP0" . 35)
    ("NP1" . 36) ("NP2" . 37) ("NP3" . 38) ("NP4" . 39)
    ("NP5" . 40) ("NP6" . 41) ("NP7" . 42) ("NP8" . 43)
    ("NP9" . 44) ("NPA" . 45) ("NPB" . 46) ("NPC" . 47)
    ("NPD" . 48) ("NPE" . 49) ("NPF" . 50) ("NPG" . 51)
    ("NPP" . 52) ("AL" . 53) ("AR" . 54) ("SHL" . 55)
    ("SHR" . 56) ("LCK" . 57) ("CTL" . 58) ("RPT" . 59)
    ("TAB" . 60) ("RET" . 61) ("BS" . 62) ("DEL" . 63)
    ("ESCS" . 64) ("L1S" . 65) ("L2S" . 66) ("L3S" . 67)
    ("L1AS" . 68) ("L2AS" . 69) ("L3AS" . 70) ("L4S" . 71)
    ("L5S" . 72) ("L6S" . 73) ("L7S" . 74) ("L8S" . 75)
    ("L9S" . 76) ("LAS" . 77) ("LBS" . 78) ("LCS" . 79)
    ("LDS" . 80) ("LES" . 81) ("LFS" . 82) ("F0S" . 83)
    ("F1S" . 84) ("F2S" . 85) ("F3S" . 86) ("F4S" . 87)
    ("F5S" . 88) ("F6S" . 89) ("F7S" . 90) ("F8S" . 91)
    ("F9S" . 92) ("R1S" . 93) ("R2S" . 94) ("R3S" . 95)
    ("R4S" . 96) ("R5S" . 97) ("R6S" . 98) ("NP0S" . 99)
    ("NP1S" . 100) ("NP2S" . 101) ("NP3S" . 102) ("NP4S" . 103)
    ("NP5S" . 104) ("NP6S" . 105) ("NP7S" . 106) ("NP8S" . 107)
    ("NP9S" . 108) ("NPAS" . 109) ("NPBS" . 110) ("NPCS" . 111)
    ("NPDS" . 112) ("NPES" . 113) ("NPFS" . 114) ("NPGS" . 115)
    ("NPPS" . 116) ("ALS" . 117) ("ARS" . 118) ("SHLS" . 119)
    ("SHRS" . 120) ("LCKS" . 121) ("CTLS" . 122) ("RPTS" . 123)
    ("TABS" . 124) ("RETS" . 125) ("BSS" . 126) ("DELS" . 127)
    ("ESCC" . 128) ("L1C" . 129) ("L2C" . 130) ("L3C" . 131)
    ("L1AC" . 132) ("L2AC" . 133) ("L3AC" . 134) ("L4C" . 135)
    ("L5C" . 136) ("L6C" . 137) ("L7C" . 138) ("L8C" . 139)
    ("L9C" . 140) ("LAC" . 141) ("LBC" . 142) ("LCC" . 143)
    ("LDC" . 144) ("LEC" . 145) ("LFC" . 146) ("F0C" . 147)
    ("F1C" . 148) ("F2C" . 149) ("F3C" . 150) ("F4C" . 151)
    ("F5C" . 152) ("F6C" . 153) ("F7C" . 154) ("F8C" . 155)
    ("F9C" . 156) ("R1C" . 157) ("R2C" . 158) ("R3C" . 159)
    ("R4C" . 160) ("R5C" . 161) ("R6C" . 162) ("NP0C" . 163)
    ("NP1C" . 164) ("NP2C" . 165) ("NP3C" . 166) ("NP4C" . 167)
    ("NP5C" . 168) ("NP6C" . 169) ("NP7C" . 170) ("NP8C" . 171)
    ("NP9C" . 172) ("NPAC" . 173) ("NPBC" . 174) ("NPCC" . 175)
    ("NPDC" . 176) ("NPEC" . 177) ("NPFC" . 178) ("NPGC" . 179)
    ("NPPC" . 180) ("ALC" . 181) ("ARC" . 182) ("SHLC" . 183)
    ("SHRC" . 184) ("LCKC" . 185) ("CTLC" . 186) ("RPTC" . 187)
    ("TABC" . 188) ("RETC" . 189) ("BSC" . 190) ("DELC" . 191)
    ("ESCU" . 192) ("L1U" . 193) ("L2U" . 194) ("L3U" . 195)
    ("L1AU" . 196) ("L2AU" . 197) ("L3AU" . 198) ("L4U" . 199)
    ("L5U" . 200) ("L6U" . 201) ("L7U" . 202) ("L8U" . 203)
    ("L9U" . 204) ("LAU" . 205) ("LBU" . 206) ("LCU" . 207)
    ("LDU" . 208) ("LEU" . 209) ("LFU" . 210) ("F0U" . 211)
    ("F1U" . 212) ("F2U" . 213) ("F3U" . 214) ("F4U" . 215)
    ("F5U" . 216) ("F6U" . 217) ("F7U" . 218) ("F8U" . 219)
    ("F9U" . 220) ("R1U" . 221) ("R2U" . 222) ("R3U" . 223)
    ("R4U" . 224) ("R5U" . 225) ("R6U" . 226) ("NP0U" . 227)
    ("NP1U" . 228) ("NP2U" . 229) ("NP3U" . 230) ("NP4U" . 231)
    ("NP5U" . 232) ("NP6U" . 233) ("NP7U" . 234) ("NP8U" . 235)
    ("NP9U" . 236) ("NPAU" . 237) ("NPBU" . 238) ("NPCU" . 239)
    ("NPDU" . 240) ("NPEU" . 241) ("NPFU" . 242) ("NPGU" . 243)
    ("NPPU" . 244) ("ALU" . 245) ("ARU" . 246) ("SHLU" . 247)
    ("SHRU" . 248) ("LCKU" . 249) ("CTLU" . 250) ("RPTU" . 251)
    ("TABU" . 252) ("RETU" . 253) ("BSU" . 254) ("DELU" . 255)
    ("MARK" . "L1") ("LINE_DEL" . "L2") ("CHAR_DEL" . "L3")
    ("L_BAR_ARROW" . "L4") ("CMD" . "L5") ("R_BAR_ARROW" . "L6")
    ("L_BOX_ARROW" . "L7") ("UP_ARROW" . "L8") ("R_BOX_ARROW" . "L9")
    ("LEFT_ARROW" . "LA") ("NEXT_WIN" . "LB") ("RIGHT_ARROW" . "LC")
    ("UP_BOX_ARROW" . "LD") ("DOWN_ARROW" . "LE") ("DOWN_BOX_ARROW" . "LF")
    ("COPY" . "L1A") ("PASTE" . "L2A") ("GROW" . "L3A") ("INS_MODE" . "L1S")
    ("SHELL" . "L5S") ("CUT" . "L1AS") ("UNDO" . "L2AS") ("MOVE" . "L3AS")
    ("POP" . "R1") ("AGAIN" . "R2") ("READ" . "R3") ("EDIT" . "R4")
    ("EXIT" . "R5") ("HOLD" . "R6") ("SAVE" . "R4S") ("ABORT" . "R5S")
    ("UNIXHELP" . "R6S") ("AEGISHELP" . "R6C")))


;;; Define the Apollo Mouse Buttons.

(defvar *apollo-mouse-buttons*
  '(("M1D" . 97) ("M2D" . 98) ("M3D" . 99) ("M4D" . 100)
    ("M1S" . 33) ("M2S" . 34) ("M3S" . 35) ("M4S" . 36)
    ("M1C" . 1) ("M2C" . 2) ("M3C" . 3) ("M4C" . 4)
    ("M1U" . 65) ("M2U" . 66) ("M3U" . 67) ("M4U" . 68)))


;;; Define functions to simplify making function key and mouse button bindings.

(defun bind-apollo-function-key (function-key binding &optional meta-binding)
  "Enable an Apollo Function Key and assign a binding to it."
  (interactive "sFunction Key: \nCCommand: \nCMeta Command: ")
  (let ((numeric-code (cdr (assoc function-key *apollo-function-keys*))))
    (if (null numeric-code)
	(error "%s is not a legal Apollo Function Key name" function-key))
    (if (stringp numeric-code)
	(setq numeric-code
	      (cdr (assoc numeric-code *apollo-function-keys*))))
    (enable-apollo-function-key numeric-code)
    (let ((normal-sequence
	   (concat (char-to-string (logior 72 (lsh numeric-code -6)))
		   (char-to-string (logior 64 (logand numeric-code 63)))))
	  (meta-sequence
	   (concat (char-to-string (logior 76 (lsh numeric-code -6)))
		   (char-to-string (logior 64 (logand numeric-code 63))))))
      (define-key 'apollo-prefix normal-sequence binding)
      (define-key 'apollo-prefix meta-sequence (or meta-binding binding)))))

(defun unbind-apollo-function-key (function-key)
  "Disable an Apollo Function Key and return control of it to the DM."
  (interactive "sFunction key: ")
  (let ((numeric-code (cdr (assoc function-key *apollo-function-keys*))))
    (if (null numeric-code)
	(error "%s is not a legal Apollo Function Key name" function-key))
    (if (stringp numeric-code)
	(setq numeric-code
	      (cdr (assoc numeric-code *apollo-function-keys*))))
    (disable-apollo-function-key numeric-code)))

(defun select-apollo-meta-key (meta-key)
  "Select the Function Key used as the Meta Key."
  (interactive "sMeta Key: ")
  (let ((numeric-code (cdr (assoc meta-key *apollo-function-keys*))))
    (if (null numeric-code)
	(error "%s is not a legal Apollo Function Key name" meta-key))
    (if (stringp numeric-code)
	(setq numeric-code
	      (cdr (assoc numeric-code *apollo-function-keys*))))
    (set-apollo-meta-key numeric-code)))

(defun bind-apollo-mouse-button (mouse-button binding &optional meta-binding)
  "Enable an Apollo Mouse Button and assign a binding to it."
  (interactive "sMouse Button: \nCCommand: \nCMeta Command: ")
  (let ((numeric-code (cdr (assoc mouse-button *apollo-mouse-buttons*))))
    (if (null numeric-code)
	(error "%s is not a legal Apollo Mouse Button name" mouse-button))
    (enable-apollo-mouse-button numeric-code)
    (let ((normal-sequence (char-to-string numeric-code))
	  (meta-sequence (char-to-string (+ numeric-code 16))))
      (define-key 'apollo-prefix normal-sequence binding)
      (define-key 'apollo-prefix meta-sequence (or meta-binding binding)))))

(defun unbind-apollo-mouse-button (mouse-button)
  "Disable an Apollo Mouse Button and return control of it to the DM."
  (interactive "sMouse Button: ")
  (let ((numeric-code (cdr (assoc mouse-button *apollo-mouse-buttons*))))
    (if (null numeric-code)
	(error "%s is not a legal Apollo Mouse Button name" mouse-button))
    (disable-apollo-mouse-button numeric-code)))


;;; Initialize the Apollo Keymaps.

(define-prefix-command 'apollo-prefix)
(global-set-key "\C-^" 'apollo-prefix)
(define-prefix-command 'apollo-prefix-1)
(define-prefix-command 'apollo-prefix-2)
(define-prefix-command 'apollo-prefix-3)
(define-prefix-command 'apollo-prefix-4)
(define-prefix-command 'apollo-prefix-5)
(define-prefix-command 'apollo-prefix-6)
(define-prefix-command 'apollo-prefix-7)
(define-prefix-command 'apollo-prefix-8)
(define-key 'apollo-prefix "H" 'apollo-prefix-1)
(define-key 'apollo-prefix "I" 'apollo-prefix-2)
(define-key 'apollo-prefix "J" 'apollo-prefix-3)
(define-key 'apollo-prefix "K" 'apollo-prefix-4)
(define-key 'apollo-prefix "L" 'apollo-prefix-5)
(define-key 'apollo-prefix "M" 'apollo-prefix-6)
(define-key 'apollo-prefix "N" 'apollo-prefix-7)
(define-key 'apollo-prefix "O" 'apollo-prefix-8)


;;; Commands to COPY, CUT, and PASTE.

(defun apollo-copy-region ()
  "Copy region between point and mark to the default DM paste buffer."
  (interactive)
  (write-region-to-default-apollo-paste-buffer (mark) (point))
  (message "Region Copied"))

(defun apollo-cut-region ()
  "Copy region between point and mark to the default DM paste buffer."
  (interactive)
  (write-region-to-default-apollo-paste-buffer (mark) (point))
  (kill-region (mark) (point))
  (message "Region Cut"))

(defun apollo-paste ()
  "Copy region between point and mark to the default DM paste buffer."
  (interactive)
  (let ((x (insert-contents-of-default-apollo-paste-buffer)))
    (push-mark (+ (point) x)))
  (message "Pasted and Mark set"))


;;; Miscellaneous Commands.

(defun minibuffer-prompt-length ()
  "Returns the length of the current minibuffer prompt."
  (let ((window (selected-window))
	length)
    (select-window (minibuffer-window))
    (let ((point (point)))
      (goto-char (point-min))
      (insert-char ?a 200)
      (goto-char (point-min))
      (vertical-motion 1)
      (setq length (- (frame-width) (point)))
      (goto-char (point-min))
      (delete-char 200)
      (goto-char point))
    (select-window window)
    length))

(defun extract-file-or-buffer-name-around-point (&optional buffer-flag)
  (let ((skip-characters (if buffer-flag
			     "!#-%*-9=?-{}~:<>"
			     "!#-%*-9=?-{}~:"))
	(skip-at-end (if buffer-flag
			 '(?@ ?. ?, ?: ?<)
			 '(?* ?@ ?. ?, ?:))))
    (save-excursion
      (skip-chars-backward skip-characters)
      (let ((start (point)))
	(skip-chars-forward skip-characters)
	(let* ((filename (buffer-substring start (point)))
	       (last-char (aref filename (- (length filename) 1))))
	  (if (memq last-char skip-at-end)
	      (substring filename 0 -1)
	      filename))))))
(fset 'extract-file-name-around-point
      'extract-file-or-buffer-name-around-point)
(fset 'extract-buf-or-file-name-around-point
      'extract-file-or-buffer-name-around-point)

(defun apollo-find-file (&optional find-buffer-flag other-window)
  "Find the file or buffer whose name the cursor is over.  Buffer names are
matched only if the optional argument FIND-BUFFER-FLAG is non-NIL.  If the
optional argument OTHER-WINDOW is non-NIL, the file is displayed in the other
window.  When matching file names, ignores trailing '*' or '@' as in 'ls -F'
output."
  (interactive)
  (let* ((file-or-buffer-name
	  (extract-file-or-buffer-name-around-point find-buffer-flag))
	 (buffer (and find-buffer-flag (get-buffer file-or-buffer-name))))
    (if (or buffer (file-exists-p file-or-buffer-name))
	(funcall (if other-window
		     'switch-to-buffer-other-window
		     'switch-to-buffer)
		 (or buffer (find-file-noselect file-or-buffer-name)))
	(error "Cannot find %s \"%s\""
	       (if find-buffer-flag "buffer or file" "file")
	       file-or-buffer-name))))

(defun apollo-grow-emacs-window ()
  "Grow Emacs's Apollo window with rubberbanding."
  (interactive)
  (execute-dm-command "WGE"))

(defun apollo-move-emacs-window ()
  "Move Emacs's Apollo window with rubberbanding."
  (interactive)
  (execute-dm-command "WME"))

(defun apollo-again ()
  "Copy the remainder of the current line to the end of the buffer."
  (interactive)
  (set-mark-command nil)
  (end-of-line)
  (copy-region-as-kill (mark) (point))
  (end-of-buffer)
  (yank))

(defun apollo-exit ()
  "Kill current buffer after saving changes."
  (interactive)
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun apollo-abort ()
  "Kill current buffer without saving changes."
  (interactive)
  (kill-buffer (current-buffer)))

(defun apollo-aegis-help (filename)
  "Prompt for topic and find the Apollo help file."
  (interactive "sHelp on: ")
  (let ((help-file (concat "/sys/help/" filename ".hlp")))
    (with-output-to-temp-buffer "*Help File*"
      (buffer-disable-undo standard-output)
      (save-excursion
	(set-buffer standard-output)
	(insert-man-file help-file)
	(if (> (buffer-size) 0)
	    (progn
	      (message "Cleaning help file entry...")
	      (apollo-clean-help-file)
	      (message ""))
	    (message "No help found in %s" help-file))
	(set-buffer-modified-p nil)))))
(fset 'apollo-help 'apollo-aegis-help)

;;; Make sure this will be loaded if necessary.

(autoload 'insert-man-file "man")

(defun apollo-clean-help-file ()
  (interactive "*")
  ;; Remove underlining and overstriking by the same letter.
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let ((preceding (char-after (- (point) 2)))
	  (following (following-char)))
      (cond ((= preceding following)	; x\bx
	     (delete-char -2))
	    ((= preceding ?\_)		; _\b
	     (delete-char -2))
	    ((= following ?\_)		; \b_
	     (delete-region (1- (point)) (1+ (point)))))))
  ;; Remove overstriking and carriage returns before newline.
  (goto-char (point-min))
  (while (re-search-forward "\r$" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "^.*\r" nil t)
    (replace-match ""))
  ;; Fit in 79 cols rather than 80.
  (indent-rigidly (point-min) (point-max) -1)
  ;; Delete excess multiple blank lines.
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))
  ;; Remove blank lines at the beginning.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point))
  ;; Separate the header from the main subject line.
  (end-of-line)
  (insert "\n")
  (goto-char (point-min)))

(defun kill-whole-line ()
  "Kill the line containing point.  Try to retain column cursor is on."
  (interactive)
  (let ((old-column (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (move-to-column old-column)))

(defun apollo-key-undefined ()
  "Signal that an Apollo Function Key is undefined."
  (interactive)
  (error "Apollo Function Key undefined"))


;;; Define the mouse commands.

(defun apollo-mouse-move-point (&optional no-mark)
  "Used so that pressing the left mouse button, moving the cursor, and
releasing the left mouse button leaves the mark set to the initial position
and the point set to the final position.  Useful for easily marking regions
of text.  If the left mouse button is pressed and released at the same place,
the mark is left at the original position of the character cursor.

Returns (x y) frame coordinates of point in columns and lines."
  (interactive)
  (let* ((opoint (point))
	 (owindow (selected-window))
	 (x (- (read-char) 8))
	 (y (- (read-char) 8))
	 (edges (window-edges))
	 (window nil))
    (while (and (not (eq window (selected-window)))
		(or (<  y (nth 1 edges))
		    (>= y (nth 3 edges))
		    (<  x (nth 0 edges))
		    (>= x (nth 2 edges))))
      (setq window (next-window window))
      (setq edges (window-edges window)))
    (if (and window (not (eq window (selected-window))))
	(progn
	  (if (and (not *apollo-mouse-move-point-allow-minibuffer-exit*)
		   (eq (selected-window) (minibuffer-window)))
	      (error "Cannot use mouse to leave minibuffer!"))
	  (if (eq window (minibuffer-window))
	      (error "Cannot use mouse to enter minibuffer!"))))
    (if window (select-window window))
    (move-to-window-line (- y (nth 1 edges)))
    (let* ((width-1 (1- (window-width window)))
	   (wraps (/ (current-column) width-1))
	   (prompt-length (if (eq (selected-window) (minibuffer-window))
			      (minibuffer-prompt-length)
			    0)))
      (move-to-column (+ (- x (nth 0 edges) prompt-length)
			 (* wraps width-1))))
    (if no-mark
	(progn (setq window (selected-window))
	       (if (eq owindow window)
		   (if (equal opoint (point))
		       (pop-mark))
		 (select-window owindow)
		 (pop-mark)
		 (select-window window)))
      (set-mark-command nil))
    ;; Return (x y) coords of point in column and frame line numbers.
    (list x y)))

(defun apollo-mouse-move-mark ()
  "Used so that pressing the left mouse button, moving the cursor, and
releasing the left mouse button leaves the mark set to the initial position
and the point set to the final position.  Useful for easily marking regions
of text.  If the left mouse button is pressed and released at the same place,
the mark is left at the original position of the character cursor."
  (interactive)
  (apollo-mouse-move-point)
  (if (equal (point) (mark))
      (pop-mark)))

(defun apollo-mouse-cut ()
  "Move point to the location of the mouse cursor and
cut the region to the default DM paste buffer."
  (interactive)
  (apollo-mouse-move-mark)
  (apollo-cut-region))

(defun apollo-mouse-copy ()
  "Move point to the location of the mouse cursor and
copy the region to the default DM paste buffer."
  (interactive)
  (apollo-mouse-move-mark)
  (apollo-copy-region))

(defun apollo-mouse-paste ()
  "Move point to the location of the mouse cursor and
paste in the default DM paste buffer."
  (interactive)
  (apollo-mouse-move-point)
  (apollo-paste))

(defun apollo-mouse-pop-buffer ()
  "Used in conjunction with the 'list-buffers' command, moves
point to cursor location and displays buffer named on current line.
Similar to a DM pop window by name to top."
  (interactive)
  (apollo-mouse-move-point)
  (Buffer-menu-select))

(defun apollo-mouse-find-file ()
  "Find the file or buffer whose name the cursor is over.  Buffers are only
allowed when in the '*Buffer List*' buffer.  When matching file names, ignores
trailing '*' or '@' as in 'ls -F' output."
  (interactive)
  (apollo-mouse-move-point)
  (let ((find-buffer-flag
	 (equal (buffer-name (current-buffer)) "*Buffer List*")))
    (apollo-find-file find-buffer-flag nil)))

(defun apollo-mouse-find-file-other-window ()
  "Find the file or buffer whose name the cursor is over.  Buffers are only
allowed when in the '*Buffer List*' buffer.  When matching file names, ignores
trailing '*' or '@' as in 'ls -F' output.  The file or buffer is displayed in
the other window."
  (interactive)
  (apollo-mouse-move-point)
  (let ((find-buffer-flag
	 (equal (buffer-name (current-buffer)) "*Buffer List*")))
    (apollo-find-file find-buffer-flag t))
  (other-window 1))


;;; Define and Enable the Mouse Key Bindings.

(defun apollo-mouse-defaults ()
"Set up default Apollo mouse key bindings for GNU Emacs."
  (interactive)
  (bind-apollo-mouse-button "M1D" 'apollo-mouse-move-point
			    'apollo-mouse-move-point) ;MOUSE LEFT DOWN
  (bind-apollo-mouse-button "M1U" 'apollo-mouse-move-mark
			    'apollo-mouse-copy) ;MOUSE LEFT UP
  (bind-apollo-mouse-button "M2D" 'sm-depress
			    'sm-depress-meta) ;MOUSE MIDDLE DOWN
  (bind-apollo-mouse-button "M2U" 'smart-key-mouse
			    'smart-key-mouse-meta) ;MOUSE MIDDLE UP
  (bind-apollo-mouse-button "M3D" 'sm-depress-meta) ;MOUSE RIGHT DOWN
  (bind-apollo-mouse-button "M3U" 'smart-key-mouse-meta) ;MOUSE RIGHT UP
)
(apollo-mouse-defaults)

(defun apollo-mouse-cut-copy-paste ()
  "Sets Apollo mouse keys to perform DM-style cut, copy, and paste.
LEFT MOUSE DOWN moves point to cursor location.  LEFT MOUSE UP sets
mark, moves point to cursor location and cuts region.  MID MOUSE works
the same way but does a copy.  RIGHT MOUSE sets point and pastes at
cursor location.  These key bindings are also effective in DM windows
until \\[apollo-mouse-cancel-cut-copy-paste] is executed in the GNU Emacs DM
window."
  (interactive)
  (bind-apollo-mouse-button "M1D" 'apollo-mouse-move-point) ;MOUSE LEFT DOWN
  (bind-apollo-mouse-button "M1U" 'apollo-mouse-cut) ;MOUSE LEFT UP
  (bind-apollo-mouse-button "M2D" 'apollo-mouse-move-point) ;MOUSE MIDDLE DOWN
  (bind-apollo-mouse-button "M2U" 'apollo-mouse-copy) ;MOUSE MIDDLE UP
  (bind-apollo-mouse-button "M3D" 'apollo-mouse-paste) ;MOUSE RIGHT DOWN
  (unbind-apollo-mouse-button "M3U") ;MOUSE RIGHT UP
  (message "Mouse Edit Mode: left=cut, mid=copy, right=paste")
  (execute-dm-command "msg 'Mouse Edit Mode: left=cut, mid=copy, right=paste';kd m1 dr;echo ke;kd m1u xd ke;kd m2 dr;echo ke;kd m2u xc ke; kd m3 xp ke;kd m3u ke")
)

(defun apollo-mouse-cancel-cut-copy-paste ()
  "Sets Apollo mouse keys back to defaults with GNU Emacs and personal
settings within the DM."
  (interactive)
  (apollo-mouse-defaults)
  (message "Default mouse key bindings set")
  (execute-dm-command
    (concat "msg 'Mouse Edit Mode canceled; personal mouse keys restored';"
	    "cmdf " *dm-mouse-key-bindings-file*))
)

;;; Define and Enable the Function Key Bindings.

(bind-apollo-function-key "TABS" "\C-I") ;Shift TAB
(bind-apollo-function-key "TABC" "\C-I") ;Control TAB
(bind-apollo-function-key "RETS" "\C-M") ;Shift RET
(bind-apollo-function-key "RETC" "\C-M") ;Control RET
(bind-apollo-function-key "LINE_DEL" 'kill-whole-line) ;LINE DEL
(bind-apollo-function-key "CHAR_DEL" "\C-D") ;CHAR DEL
(bind-apollo-function-key "L_BAR_ARROW" "\C-A") ;LEFT BAR ARROW
(bind-apollo-function-key "R_BAR_ARROW" "\C-E") ;RIGHT BAR ARROW
(bind-apollo-function-key "L_BOX_ARROW" "\C-x>") ;LEFT BOX ARROW
(bind-apollo-function-key "UP_ARROW" "\C-P") ;UP ARROW
(bind-apollo-function-key "L8S" "\M-1\M-V") ;Shift UP ARROW

;;; RIGHT BOX ARROW is the Default Meta Key.  If the Meta Key is changed with
;;; SELECT-APOLLO-META-KEY, then RIGHT BOX ARROW signals an error.

(select-apollo-meta-key "R1") ; Make POP the META key instead.
(bind-apollo-function-key "R_BOX_ARROW" "\C-x<") ;RIGHT BOX ARROW
(bind-apollo-function-key "LEFT_ARROW" "\C-B") ;LEFT ARROW
(bind-apollo-function-key "RIGHT_ARROW" "\C-F") ;RIGHT ARROW
(bind-apollo-function-key "DOWN_ARROW" "\C-N") ;DOWN ARROW
(bind-apollo-function-key "LES" "\M-1\C-V") ;Shift DOWN ARROW
(bind-apollo-function-key "R3S" 'apollo-find-file) ;Shift READ
(bind-apollo-function-key "MARK" 'set-mark-command) ;MARK
(bind-apollo-function-key "INS_MODE" 'overwrite-mode) ;INS MODE
(bind-apollo-function-key "L2S" "\C-Y")	;Shift LINE DEL
(bind-apollo-function-key "L3S" "\C-D")	;Shift CHAR DEL
(bind-apollo-function-key "COPY" 'apollo-copy-region) ;COPY
(bind-apollo-function-key "CUT" 'apollo-cut-region) ;CUT
(bind-apollo-function-key "PASTE" 'apollo-paste) ;PASTE
(bind-apollo-function-key "UNDO" 'undo) ;UNDO
(bind-apollo-function-key "GROW" 'apollo-grow-emacs-window) ;GROW
(bind-apollo-function-key "MOVE" 'apollo-move-emacs-window) ;MOVE
(bind-apollo-function-key "LAS" "\M-B") ;Shift LEFT ARROW
(bind-apollo-function-key "LCS" "\M-F") ;Shift RIGHT ARROW
(bind-apollo-function-key "UP_BOX_ARROW" "\M-V") ;UP BOX ARROW
(bind-apollo-function-key "LDS" "\M-<") ;Shift UP BOX ARROW
(bind-apollo-function-key "DOWN_BOX_ARROW" "\C-V") ;DOWN BOX ARROW
(bind-apollo-function-key "LFS" "\M->") ;Shift DOWN BOX ARROW
(bind-apollo-function-key "AGAIN" 'apollo-again) ;AGAIN
(bind-apollo-function-key "EXIT" 'apollo-exit) ;EXIT
(bind-apollo-function-key "ABORT" 'apollo-abort) ;ABORT
(bind-apollo-function-key "SAVE" 'save-buffer) ;SAVE
(bind-apollo-function-key "HOLD" 'apollo-key-undefined) ;HOLD

(defun install-apollo-dm-preemptive-key-bindings ()
  (bind-apollo-function-key "L4S" "\M-<") ;Shift LEFT BAR ARROW
  (bind-apollo-function-key "L5" 'execute-dm-command) ;CMD
  (bind-apollo-function-key "L6S" "\M->") ;Shift RIGHT BAR ARROW
  (bind-apollo-function-key "LB" 'other-window) ;NEXT WNDW
  (bind-apollo-function-key "LBS" 'delete-window) ;Shift NEXT WNDW
  (bind-apollo-function-key "READ" 'find-file-read-only) ;READ
  (bind-apollo-function-key "EDIT" 'find-file) ;EDIT
  (bind-apollo-function-key "SHELL" 'shell) ;SHELL
  (bind-apollo-function-key "UNIXHELP" 'manual-entry) ;HELP
  (bind-apollo-function-key "AEGISHELP" 'apollo-aegis-help)) ;HELP

(if *preempt-display-manager-bindings*
    (install-apollo-dm-preemptive-key-bindings))

(run-hooks '*apollo-key-bindings-hook*)

(provide 'apollo)

;;; apollo.el ends here
