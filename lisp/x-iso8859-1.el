;;; x-iso8859-1 --- Mapping between X keysym names and ISO 8859-1

;; Copyright (C) 1992, 1993, 1997 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Created: 15-jun-92
;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, internal, dumped

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

;;; Synched up with: Not synched.

;;; Commentary:

;; created by jwz, 13-jun-92.
;; changed by Heiko Muenkel, 12-jun-1997: Added a grave keysym.

;; Under X, when the user types a character that is ISO-8859/1 but not ASCII,
;; it comes in as a symbol instead of as a character code.  This keeps things
;; nice and character-set independent.  This file takes all of those symbols
;; (the symbols that are the X names for the 8859/1 characters) and puts a
;; property on them which holds the character code that should be inserted in
;; the buffer when they are typed.  The self-insert-command function will look
;; at this.  It also binds them all to self-insert-command.

;; It puts the same property on the keypad keys, so that (read-char) will
;; think that they are the same as the digit characters.  However, those
;; keys are bound to one-character keyboard macros, so that `kp-9' will, by
;; default, do the same thing that `9' does, in whatever the current mode is.

;; The standard case and syntax tables are set in iso8859-1.el, since
;; that is not X-specific.

;;; Code:

(require 'iso8859-1)

(defconst iso8859/1-code-to-x-keysym-table nil
  "Maps iso8859/1 to an X keysym name which corresponds to it.
There may be more than one X name for this keycode; this returns the first one.
Note that this is X specific; one should avoid using this table whenever
possible, in the interest of portability.")

;; (This esoteric little construct is how you do MACROLET in elisp.  It
;; generates the most efficient code for the .elc file by unwinding the
;; loop at compile-time.)

((macro
  . (lambda (&rest syms-and-iso8859/1-codes)
      (cons
       'progn
       (nconc
	;;
	;; First emit code that puts the `x-iso8859/1' property on all of
	;; the keysym symbols.
	;;
	(mapcar '(lambda (sym-and-code)
		  (list 'put (list 'quote (car sym-and-code))
			''x-iso8859/1 (car (cdr sym-and-code))))
		syms-and-iso8859/1-codes)
	;;
	;; Then emit code that binds all of those keysym symbols to
	;; `self-insert-command'.
	;;
	(mapcar '(lambda (sym-and-code)
		  (list 'global-set-key (list 'quote (car sym-and-code))
			''self-insert-command))
		syms-and-iso8859/1-codes)
	;;
	;; Then emit the value of iso8859/1-code-to-x-keysym-table.
	;;
	(let ((v (make-vector 256 nil)))
	  ;; the printing ASCII chars have 1-char names.
	  (let ((i 33))
	    (while (< i 127)
	      (aset v i (intern (make-string 1 i)))
	      (setq i (1+ i))))
	  ;; these are from the keyboard character set.
	  (mapcar '(lambda (x) (aset v (car x) (car (cdr x))))
		  '((8 backspace) (9 tab) (10 linefeed) (13 return)
		    (27 escape) (32 space) (127 delete)))
	  (mapcar '(lambda (sym-and-code)
		    (or (aref v (car (cdr sym-and-code)))
			(aset v (car (cdr sym-and-code)) (car sym-and-code))))
		  syms-and-iso8859/1-codes)
	  (list (list 'setq 'iso8859/1-code-to-x-keysym-table v)))
	))))

 ;; The names and capitalization here are as per the MIT X11R4 and X11R5
 ;; distributions.  If a vendor varies from this, adjustments will need
 ;; to be made...

 (grave			?\140)
 (nobreakspace		?\240)
 (exclamdown		?\241)
 (cent			?\242)
 (sterling		?\243)
 (currency		?\244)
 (yen			?\245)
 (brokenbar		?\246)
 (section		?\247)
 (diaeresis		?\250)
 (copyright		?\251)
 (ordfeminine		?\252)
 (guillemotleft		?\253)
 (notsign		?\254)
 (hyphen		?\255)
 (registered		?\256)
 (macron		?\257)
 (degree		?\260)
 (plusminus		?\261)
 (twosuperior		?\262)
 (threesuperior		?\263)
 (acute			?\264)	; Why is there an acute keysym that is
 (mu			?\265)	; distinct from apostrophe/quote, but
 (paragraph		?\266)	; no grave keysym that is distinct from
 (periodcentered	?\267)	; backquote?
 (cedilla		?\270)  ; I've added the grave keysym, because it's
 (onesuperior		?\271)  ; used in x-compose (Heiko Muenkel).
 (masculine		?\272)
 (guillemotright	?\273)
 (onequarter		?\274)
 (onehalf		?\275)
 (threequarters		?\276)
 (questiondown		?\277)

 (Agrave		?\300)
 (Aacute		?\301)
 (Acircumflex		?\302)
 (Atilde		?\303)
 (Adiaeresis		?\304)
 (Aring			?\305)
 (AE			?\306)
 (Ccedilla		?\307)
 (Egrave		?\310)
 (Eacute		?\311)
 (Ecircumflex		?\312)
 (Ediaeresis		?\313)
 (Igrave		?\314)
 (Iacute		?\315)
 (Icircumflex		?\316)
 (Idiaeresis		?\317)
 (ETH			?\320)
 (Ntilde		?\321)
 (Ograve		?\322)
 (Oacute		?\323)
 (Ocircumflex		?\324)
 (Otilde		?\325)
 (Odiaeresis		?\326)
 (multiply		?\327)
 (Ooblique		?\330)
 (Ugrave		?\331)
 (Uacute		?\332)
 (Ucircumflex		?\333)
 (Udiaeresis		?\334)
 (Yacute		?\335)
 (THORN			?\336)
 (ssharp		?\337)

 (agrave		?\340)
 (aacute		?\341)
 (acircumflex		?\342)
 (atilde		?\343)
 (adiaeresis		?\344)
 (aring			?\345)
 (ae			?\346)
 (ccedilla		?\347)
 (egrave		?\350)
 (eacute		?\351)
 (ecircumflex		?\352)
 (ediaeresis		?\353)
 (igrave		?\354)
 (iacute		?\355)
 (icircumflex		?\356)
 (idiaeresis		?\357)
 (eth			?\360)
 (ntilde		?\361)
 (ograve		?\362)
 (oacute		?\363)
 (ocircumflex		?\364)
 (otilde		?\365)
 (odiaeresis		?\366)
 (division		?\367)
 (oslash		?\370)
 (ugrave		?\371)
 (uacute		?\372)
 (ucircumflex		?\373)
 (udiaeresis		?\374)
 (yacute		?\375)
 (thorn			?\376)
 (ydiaeresis		?\377)

 )

((macro . (lambda (&rest syms-and-iso8859/1-codes)
	    (cons 'progn
		  (mapcar '(lambda (sym-and-code)
			    (list 'put (list 'quote (car sym-and-code))
				  ''x-iso8859/1 (car (cdr sym-and-code))))
			  syms-and-iso8859/1-codes))))
 ;;
 ;; Let's do the appropriate thing for some vendor-specific keysyms too...
 ;; Apparently nobody agrees on what the names of these keysyms are.
 ;;
 (SunFA_Acute		?\264)
 (SunXK_FA_Acute	?\264)
 (Dacute_accent		?\264)
 (DXK_acute_accent	?\264)
 (hpmute_acute		?\264)
 (hpXK_mute_acute	?\264)
 (XK_mute_acute		?\264)

 (SunFA_Grave		 ?`)
 (Dead_Grave		 ?`)
 (SunXK_FA_Grave	 ?`)
 (Dgrave_accent		 ?`)
 (DXK_grave_accent	 ?`)
 (hpmute_grave		 ?`)
 (hpXK_mute_grave	 ?`)
 (XK_mute_grave		 ?`)

 (SunFA_Cedilla		?\270)
 (SunXK_FA_Cedilla	?\270)
 (Dcedilla_accent	?\270)
 (DXK_cedilla_accent	?\270)

 (SunFA_Diaeresis	?\250)
 (SunXK_FA_Diaeresis	?\250)
 (hpmute_diaeresis	?\250)
 (hpXK_mute_diaeresis	?\250)
 (XK_mute_diaeresis	?\250)

 (SunFA_Circum		 ?^)
 (Dead_Circum		 ?^)
 (SunXK_FA_Circum	 ?^)
 (Dcircumflex_accent	 ?^)
 (DXK_circumflex_accent	 ?^)
 (hpmute_asciicircum	 ?^)
 (hpXK_mute_asciicircum	 ?^)
 (XK_mute_asciicircum	 ?^)

 (SunFA_Tilde		 ?~)
 (Dead_Tilde		 ?~)
 (SunXK_FA_Tilde	 ?~)
 (Dtilde		 ?~)
 (DXK_tilde		 ?~)
 (hpmute_asciitilde	 ?~)
 (hpXK_mute_asciitilde	 ?~)
 (XK_mute_asciitilde	 ?~)

 (Dring_accent		?\260)
 (DXK_ring_accent	?\260)
 )

(provide 'x-iso8859-1)

;;; x-iso8859-1.el ends here
