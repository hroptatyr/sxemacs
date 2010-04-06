;;; mule-coding.el --- Coding-system functions for Mule.

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1997 MORIOKA Tomohiko

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

;;; Commentary:

;;; split off of mule.el and mostly moved to coding.el

;;; Code:

(defun coding-system-force-on-output (coding-system register)
  "Return the 'force-on-output property of CODING-SYSTEM for the specified REGISTER."
  (check-type register integer)
  (coding-system-property
   coding-system
   (case register
     (0 'force-g0-on-output)
     (1 'force-g1-on-output)
     (2 'force-g2-on-output)
     (3 'force-g3-on-output)
     (t (signal 'args-out-of-range (list register 0 3))))))

(defun coding-system-short (coding-system)
  "Return the 'short property of CODING-SYSTEM."
  (coding-system-property coding-system 'short))

(defun coding-system-no-ascii-eol (coding-system)
  "Return the 'no-ascii-eol property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-ascii-eol))

(defun coding-system-no-ascii-cntl (coding-system)
  "Return the 'no-ascii-cntl property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-ascii-cntl))

(defun coding-system-seven (coding-system)
  "Return the 'seven property of CODING-SYSTEM."
  (coding-system-property coding-system 'seven))

(defun coding-system-lock-shift (coding-system)
  "Return the 'lock-shift property of CODING-SYSTEM."
  (coding-system-property coding-system 'lock-shift))

;;(defun coding-system-use-japanese-jisx0201-roman (coding-system)
;;  "Return the 'use-japanese-jisx0201-roman property of CODING-SYSTEM."
;;  (coding-system-property coding-system 'use-japanese-jisx0201-roman))

;;(defun coding-system-use-japanese-jisx0208-1978 (coding-system)
;;  "Return the 'use-japanese-jisx0208-1978 property of CODING-SYSTEM."
;;  (coding-system-property coding-system 'use-japanese-jisx0208-2978))

(defun coding-system-no-iso6429 (coding-system)
  "Return the 'no-iso6429 property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-iso6429))

(defun coding-system-ccl-encode (coding-system)
  "Return the CCL 'encode property of CODING-SYSTEM."
  (coding-system-property coding-system 'encode))

(defun coding-system-ccl-decode (coding-system)
  "Return the CCL 'decode property of CODING-SYSTEM."
  (coding-system-property coding-system 'decode))


;;;; Definitions of predefined coding systems

(make-coding-system
 'ctext 'iso2022
 "Coding-system used in X as Compound Text Encoding."
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   eol-type nil
   mnemonic "CText"))

;;; iso-8859-1 and ctext are aliases.

;; (copy-coding-system 'ctext 'iso-8859-1)
(make-coding-system
 'iso-8859-1 'no-conversion
 "Coding-system used in X as Compound Text Encoding."
 '(eol-type nil mnemonic "Noconv"))

(make-coding-system
 'iso-2022-8bit-ss2 'iso2022
 "ISO-2022 coding system using SS2 for 96-charset in 8-bit code."
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   charset-g2 t ;; unspecified but can be used later.
   short t
   mnemonic "ISO8/SS"
   ))

(make-coding-system
 'iso-2022-7bit-ss2 'iso2022
 "ISO-2022 coding system using SS2 for 96-charset in 7-bit code."
 '(charset-g0 ascii
   charset-g2 t ;; unspecified but can be used later.
   seven t
   short t
   mnemonic "ISO7/SS"
   eol-type nil))

;; (copy-coding-system 'iso-2022-7bit-ss2 'iso-2022-jp-2)
(make-coding-system
 'iso-2022-jp-2 'iso2022
 "ISO-2022 coding system using SS2 for 96-charset in 7-bit code."
 '(charset-g0 ascii
   charset-g2 t ;; unspecified but can be used later.
   seven t
   short t
   mnemonic "ISO7/SS"
   eol-type nil))

(make-coding-system
 'iso-2022-7bit 'iso2022
 "ISO 2022 based 7-bit encoding using only G0"
 '(charset-g0 ascii
   seven t
   short t
   mnemonic "ISO7"))

;; compatibility for old XEmacsen
(copy-coding-system 'iso-2022-7bit 'iso-2022-7)

(make-coding-system
 'iso-2022-8 'iso2022
 "ISO-2022 eight-bit coding system.  No single-shift or locking-shift."
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   short t
   mnemonic "ISO8"
   ))

(make-coding-system
 'escape-quoted 'iso2022
 "ISO-2022 eight-bit coding system with escape quoting; used for .ELC files."
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   eol-type lf
   escape-quoted t
   mnemonic "ESC/Quot"
   ))
;; compatibility for new Emacsen (don't use it)
(copy-coding-system 'escape-quoted 'emacs-internal)

(make-coding-system
 'iso-2022-lock 'iso2022
 "ISO-2022 coding system using Locking-Shift for 96-charset."
 '(charset-g0 ascii
   charset-g1 t ;; unspecified but can be used later.
   seven t
   lock-shift t
   mnemonic "ISO7/Lock"
   ))

;; initialize the coding categories to something semi-reasonable
;; so that the remaining Lisp files can contain extended characters.
;; (They will be in ISO-7 format)
;; #### This list needs to be synched with the ones in mule-cmds.el.

(set-coding-priority-list '(iso-7
	    no-conversion
	    ;; utf-8
	    iso-8-1
	    iso-8-2
	    iso-8-designate
	    iso-lock-shift
	    shift-jis
	    big5
	    ;; ucs-4
	    ))

(set-coding-category-system 'iso-7 'iso-2022-7)
(set-coding-category-system 'iso-8-designate 'ctext)
(set-coding-category-system 'iso-8-1 'ctext)
(set-coding-category-system 'iso-lock-shift 'iso-2022-lock)
(set-coding-category-system 'no-conversion 'no-conversion)

(setq-default buffer-file-coding-system 'iso-2022-8)

;;; mule-coding.el ends here
