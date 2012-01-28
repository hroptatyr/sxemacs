;;; iso8859-1.el --- Set case and syntax tables for Latin 1

;; Copyright (C) 1992, 1997 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Created: 19-aug-92
;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with:  Not synched

;;; Commentary:

;; created by jwz, 19-aug-92.
;; Sets the case and syntax tables for the ISO-8859/1 character set.

;;; Code:

(let ((table (standard-syntax-table)))
  ;;
  ;; The symbol characters
  ;;
  (modify-syntax-entry ?\240 "_"     table)   ; nobreakspace
  (modify-syntax-entry ?\241 "."     table)   ; exclamdown
  (modify-syntax-entry ?\242 "_"     table)   ; cent
  (modify-syntax-entry ?\243 "_"     table)   ; sterling
  (modify-syntax-entry ?\244 "_"     table)   ; currency
  (modify-syntax-entry ?\245 "_"     table)   ; yen
  (modify-syntax-entry ?\246 "_"     table)   ; brokenbar
  (modify-syntax-entry ?\247 "_"     table)   ; section
  (modify-syntax-entry ?\250 "_"     table)   ; diaeresis
  (modify-syntax-entry ?\251 "_"     table)   ; copyright
  (modify-syntax-entry ?\252 "_"     table)   ; ordfeminine
  (modify-syntax-entry ?\253 "(\273" table)   ; guillemotleft
  (modify-syntax-entry ?\254 "_"     table)   ; notsign
  (modify-syntax-entry ?\255 "_"     table)   ; hyphen
  (modify-syntax-entry ?\256 "_"     table)   ; registered
  (modify-syntax-entry ?\257 "_"     table)   ; macron
  (modify-syntax-entry ?\260 "_"     table)   ; degree
  (modify-syntax-entry ?\261 "_"     table)   ; plusminus
  (modify-syntax-entry ?\262 "_"     table)   ; twosuperior
  (modify-syntax-entry ?\263 "_"     table)   ; threesuperior
  (modify-syntax-entry ?\264 "_"     table)   ; acute
  (modify-syntax-entry ?\265 "_"     table)   ; mu
  (modify-syntax-entry ?\266 "_"     table)   ; paragraph
  (modify-syntax-entry ?\267 "_"     table)   ; periodcentered
  (modify-syntax-entry ?\270 "_"     table)   ; cedilla
  (modify-syntax-entry ?\271 "_"     table)   ; onesuperior
  (modify-syntax-entry ?\272 "_"     table)   ; masculine
  (modify-syntax-entry ?\273 ")\253" table)   ; guillemotright
  (modify-syntax-entry ?\274 "_"     table)   ; onequarter
  (modify-syntax-entry ?\275 "_"     table)   ; onehalf
  (modify-syntax-entry ?\276 "_"     table)   ; threequarters
  (modify-syntax-entry ?\277 "_"     table)   ; questiondown
  ;;
  ;; the upper-case characters (plus "multiply" and "ssharp")
  ;;
  (modify-syntax-entry ?\300 "w" table)   ; Agrave
  (modify-syntax-entry ?\301 "w" table)   ; Aacute
  (modify-syntax-entry ?\302 "w" table)   ; Acircumflex
  (modify-syntax-entry ?\303 "w" table)   ; Atilde
  (modify-syntax-entry ?\304 "w" table)   ; Adiaeresis
  (modify-syntax-entry ?\305 "w" table)   ; Aring
  (modify-syntax-entry ?\306 "w" table)   ; AE
  (modify-syntax-entry ?\307 "w" table)   ; Ccedilla
  (modify-syntax-entry ?\310 "w" table)   ; Egrave
  (modify-syntax-entry ?\311 "w" table)   ; Eacute
  (modify-syntax-entry ?\312 "w" table)   ; Ecircumflex
  (modify-syntax-entry ?\313 "w" table)   ; Ediaeresis
  (modify-syntax-entry ?\314 "w" table)   ; Igrave
  (modify-syntax-entry ?\315 "w" table)   ; Iacute
  (modify-syntax-entry ?\316 "w" table)   ; Icircumflex
  (modify-syntax-entry ?\317 "w" table)   ; Idiaeresis
  (modify-syntax-entry ?\320 "w" table)   ; ETH
  (modify-syntax-entry ?\321 "w" table)   ; Ntilde
  (modify-syntax-entry ?\322 "w" table)   ; Ograve
  (modify-syntax-entry ?\323 "w" table)   ; Oacute
  (modify-syntax-entry ?\324 "w" table)   ; Ocircumflex
  (modify-syntax-entry ?\325 "w" table)   ; Otilde
  (modify-syntax-entry ?\326 "w" table)   ; Odiaeresis
  (modify-syntax-entry ?\327 "_" table)   ; multiply
  (modify-syntax-entry ?\330 "w" table)   ; Ooblique
  (modify-syntax-entry ?\331 "w" table)   ; Ugrave
  (modify-syntax-entry ?\332 "w" table)   ; Uacute
  (modify-syntax-entry ?\333 "w" table)   ; Ucircumflex
  (modify-syntax-entry ?\334 "w" table)   ; Udiaeresis
  (modify-syntax-entry ?\335 "w" table)   ; Yacute
  (modify-syntax-entry ?\336 "w" table)   ; THORN
  (modify-syntax-entry ?\337 "w" table)   ; ssharp
  ;;
  ;; the lower-case characters (plus "division" and "ydiaeresis")
  ;;
  (modify-syntax-entry ?\340 "w" table)   ; agrave
  (modify-syntax-entry ?\341 "w" table)   ; aacute
  (modify-syntax-entry ?\342 "w" table)   ; acircumflex
  (modify-syntax-entry ?\343 "w" table)   ; atilde
  (modify-syntax-entry ?\344 "w" table)   ; adiaeresis
  (modify-syntax-entry ?\345 "w" table)   ; aring
  (modify-syntax-entry ?\346 "w" table)   ; ae
  (modify-syntax-entry ?\347 "w" table)   ; ccedilla
  (modify-syntax-entry ?\350 "w" table)   ; egrave
  (modify-syntax-entry ?\351 "w" table)   ; eacute
  (modify-syntax-entry ?\352 "w" table)   ; ecircumflex
  (modify-syntax-entry ?\353 "w" table)   ; ediaeresis
  (modify-syntax-entry ?\354 "w" table)   ; igrave
  (modify-syntax-entry ?\355 "w" table)   ; iacute
  (modify-syntax-entry ?\356 "w" table)   ; icircumflex
  (modify-syntax-entry ?\357 "w" table)   ; idiaeresis
  (modify-syntax-entry ?\360 "w" table)   ; eth
  (modify-syntax-entry ?\361 "w" table)   ; ntilde
  (modify-syntax-entry ?\362 "w" table)   ; ograve
  (modify-syntax-entry ?\363 "w" table)   ; oacute
  (modify-syntax-entry ?\364 "w" table)   ; ocircumflex
  (modify-syntax-entry ?\365 "w" table)   ; otilde
  (modify-syntax-entry ?\366 "w" table)   ; odiaeresis
  (modify-syntax-entry ?\367 "_" table)   ; division
  (modify-syntax-entry ?\370 "w" table)   ; ooblique
  (modify-syntax-entry ?\371 "w" table)   ; ugrave
  (modify-syntax-entry ?\372 "w" table)   ; uacute
  (modify-syntax-entry ?\373 "w" table)   ; ucircumflex
  (modify-syntax-entry ?\374 "w" table)   ; udiaeresis
  (modify-syntax-entry ?\375 "w" table)   ; yacute
  (modify-syntax-entry ?\376 "w" table)   ; thorn
  (modify-syntax-entry ?\377 "w" table)   ; ydiaeresis
  )


(defconst iso8859/1-case-table nil
  "The case table for ISO-8859/1 characters.")

;;; This macro expands into
;;;  (setq iso8859/1-case-table (purecopy '("..." nil nil nil)))
;;; doing the computation of the case table at compile-time.

((macro
  . (lambda (&rest pairs)
      (let ((downcase (make-string 256 0))
	    (i 0))
	(while (< i 256)
	  (aset downcase i (if (and (>= i ?A) (<= i ?Z)) (+ i 32) i))
	  (setq i (1+ i)))
	(while pairs
	  (aset downcase (car (car pairs)) (car (cdr (car pairs))))
	  (setq pairs (cdr pairs)))
	(cons 'setq
	      (cons 'iso8859/1-case-table
		    (list
		     (list 'quote
			   (list downcase nil nil nil))))))))

 (?\300  ?\340)		; Agrave
 (?\301  ?\341)		; Aacute
 (?\302  ?\342)		; Acircumflex
 (?\303  ?\343)		; Atilde
 (?\304  ?\344)		; Adiaeresis
 (?\305  ?\345)		; Aring
 (?\306  ?\346)		; AE
 (?\307  ?\347)		; Ccedilla
 (?\310  ?\350)		; Egrave
 (?\311  ?\351)		; Eacute
 (?\312  ?\352)		; Ecircumflex
 (?\313  ?\353)		; Ediaeresis
 (?\314  ?\354)		; Igrave
 (?\315  ?\355)		; Iacute
 (?\316  ?\356)		; Icircumflex
 (?\317  ?\357)		; Idiaeresis
 (?\320  ?\360)		; ETH
 (?\321  ?\361)		; Ntilde
 (?\322  ?\362)		; Ograve
 (?\323  ?\363)		; Oacute
 (?\324  ?\364)		; Ocircumflex
 (?\325  ?\365)		; Otilde
 (?\326  ?\366)		; Odiaeresis
 (?\330  ?\370)		; Ooblique
 (?\331  ?\371)		; Ugrave
 (?\332  ?\372)		; Uacute
 (?\333  ?\373)		; Ucircumflex
 (?\334  ?\374)		; Udiaeresis
 (?\335  ?\375)		; Yacute
 (?\336  ?\376)		; THORN
 )

(set-standard-case-table (mapcar 'copy-sequence iso8859/1-case-table))

(setq-default ctl-arrow 'iso-8859/1)

(provide 'iso8859-1)

;;; iso8859-1.el ends here
