;;; japanese.el --- Japanese support -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Japanese

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

;; For Japanese, character sets JISX0201, JISX0208, JISX0212 are
;; supported.

;;; Code:

;;; Syntax of Japanese characters.
(modify-syntax-entry 'katakana-jisx0201 "w")
(modify-syntax-entry 'japanese-jisx0212 "w")

(modify-syntax-entry 'japanese-jisx0208 "w")
(loop for row in '(33 34 40)
      do (modify-syntax-entry `[japanese-jisx0208 ,row] "_"))
(loop for char in '(?ー ?゛ ?゜ ?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)
      do (modify-syntax-entry char "w"))
(modify-syntax-entry ?\（ "(）")
(modify-syntax-entry ?\［ "(］")
(modify-syntax-entry ?\｛ "(｝")
(modify-syntax-entry ?\「 "(」")
(modify-syntax-entry ?\『 "(』")
(modify-syntax-entry ?\） ")（")
(modify-syntax-entry ?\］ ")［")
(modify-syntax-entry ?\｝ ")｛")
(modify-syntax-entry ?\」 ")「")
(modify-syntax-entry ?\』 ")『")

;;; Character categories S, A, H, K, G, Y, and C
(define-category ?S "Japanese 2-byte symbol character.")
(modify-category-entry [japanese-jisx0208 33] ?S)
(modify-category-entry [japanese-jisx0208 34] ?S)
(modify-category-entry [japanese-jisx0208 40] ?S)
(define-category ?A "Japanese 2-byte Alphanumeric character.")
(modify-category-entry [japanese-jisx0208 35] ?A)
(define-category ?H "Japanese 2-byte Hiragana character.")
(modify-category-entry [japanese-jisx0208 36] ?H)
(define-category ?K "Japanese 2-byte Katakana character.")
(modify-category-entry [japanese-jisx0208 37] ?K)
(define-category ?G "Japanese 2-byte Greek character.")
(modify-category-entry [japanese-jisx0208 38] ?G)
(define-category ?Y "Japanese 2-byte Cyrillic character.")
(modify-category-entry [japanese-jisx0208 39] ?Y)
(define-category ?C "Japanese 2-byte Kanji characters.")
(loop for row from 48 to 126
      do (modify-category-entry `[japanese-jisx0208 ,row] ?C))
(loop for char in '(?ー ?゛ ?゜)
      do (modify-category-entry char ?K)
	 (modify-category-entry char ?H))
(loop for char in '(?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)
      do (modify-category-entry char ?C))
(modify-category-entry 'japanese-jisx0212 ?C)

(defvar japanese-word-regexp
  "\\cA+\\cH*\\|\\cK+\\cH*\\|\\cC+\\cH*\\|\\cH+\\|\\ck+\\|\\sw+"
  "Regular expression used to match a Japanese word.")

(set-word-regexp japanese-word-regexp)
(setq forward-word-regexp  "\\w\\>")
(setq backward-word-regexp "\\<\\w")

;;; Paragraph setting
(setq sentence-end
      (concat
       "\\("
       "\\("
       "[.?!][]\"')}]*"
       "\\|"
       "[．？！][］”’）｝〕〉》」』]*"
       "\\)"
       "\\($\\|\t\\|  \\)"
       "\\|"
       "。"
       "\\)"
       "[ \t\n]*"))

;; allow paragraphs to start with a zenkaku space
(setq paragraph-start    "[ 　\t\n\f]")
(setq paragraph-separate "[ 　\t\f]*$")

;; EGG specific setup
(define-egg-environment 'japanese
  "Japanese settings for egg."
  (lambda ()
    (with-boundp '(its:*standard-modes* its:*current-map* wnn-server-type)
      (with-fboundp 'its:get-mode-map
	(when (not (featurep 'egg-jpn))
	  (load "its-hira")
	  (load "its-kata")
	  (load "its-hankaku")
	  (load "its-zenkaku")
	  (setq its:*standard-modes*
		(append
		 (list (its:get-mode-map "roma-kana")
		       (its:get-mode-map "roma-kata")
		       (its:get-mode-map "downcase")
		       (its:get-mode-map "upcase")
		       (its:get-mode-map "zenkaku-downcase")
		       (its:get-mode-map "zenkaku-upcase"))
		 its:*standard-modes*))
	  (provide 'egg-jpn))
	(setq wnn-server-type 'jserver)
	;; Can't do this here any more.  Must do it when selecting egg-wnn
	;; or egg-sj3
	;; (setq egg-default-startup-file "eggrc-wnn")
	(setq-default its:*current-map* (its:get-mode-map "roma-kana"))))))

;; stuff for providing grammatic processing of Japanese text
;; something like this should probably be created for all environments...
;; #### Arrgh.  This stuff should defvar'd in either fill.el or kinsoku.el.
;; Then the language environment should set these things, probably buffer-
;; locally.

(defvar aletter (concat "\\(" ascii-char "\\|" kanji-char "\\)"))
(defvar kanji-space-insertable (concat
	   "、" aletter                   "\\|"
	   "。" aletter                   "\\|"
	   aletter "（"                   "\\|"
	   "）" aletter                   "\\|"
	   ascii-alphanumeric  kanji-kanji-char "\\|"
	   kanji-kanji-char    ascii-alphanumeric ))

;; #### will be moved to fill.el
(defvar space-insertable (concat " " aletter "\\|" kanji-space-insertable)
  "Regexp for finding points that can have spaces inserted into them for justification")

;; (make-coding-system
;;  'iso-2022-jp 2 ?J
;;  "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)"
;;  '((ascii japanese-jisx0208-1978 japanese-jisx0208
;;           latin-jisx0201 japanese-jisx0212 katakana-jisx0201) nil nil nil
;;    short ascii-eol ascii-cntl seven)
;;  '((safe-charsets ascii japanese-jisx0208-1978 japanese-jisx0208
;;                   latin-jisx0201 japanese-jisx0212 katakana-jisx0201)
;;    (mime-charset . iso-2022-jp)))

(make-coding-system
 'iso-2022-jp 'iso2022
 "Coding-system used for communication with mail and news in Japan."
 '(charset-g0 ascii
   short t
   seven t
   input-charset-conversion ((latin-jisx0201 ascii)
			     (japanese-jisx0208-1978 japanese-jisx0208))
   mnemonic "MULE/7bit"
   ))

(define-coding-system-alias 'junet 'iso-2022-jp)

;; (make-coding-system
;;  'iso-2022-jp-2 2 ?J
;;  "ISO 2022 based 7bit encoding for CJK, Latin-1, and Greek (MIME:ISO-2022-JP-2)"
;;  '((ascii japanese-jisx0208-1978 japanese-jisx0208
;;           latin-jisx0201 japanese-jisx0212 katakana-jisx0201
;;           chinese-gb2312 korean-ksc5601) nil
;;           (nil latin-iso8859-1 greek-iso8859-7) nil
;;  short ascii-eol ascii-cntl seven nil single-shift)
;;  '((safe-charsets ascii japanese-jisx0208-1978 japanese-jisx0208
;;                   latin-jisx0201 japanese-jisx0212 katakana-jisx0201
;;                   chinese-gb2312 korean-ksc5601
;;                   latin-iso8859-1 greek-iso8859-7)
;;    (mime-charset . iso-2022-jp-2)))

;; (make-coding-system
;;  'japanese-shift-jis 1 ?S
;;  "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)"
;;  nil
;;  '((safe-charsets ascii japanese-jisx0208 japanese-jisx0208-1978
;;                   latin-jisx0201 katakana-jisx0201)
;;    (mime-charset . shift_jis)
;;    (charset-origin-alist (japanese-jisx0208 "SJIS" encode-sjis-char)
;;                          (katakana-jisx0201 "SJIS" encode-sjis-char))))

(make-coding-system
 'shift_jis 'shift-jis
 "Coding-system of Shift-JIS used in Japan."
 '(mnemonic "Ja/SJIS"))

;; (define-coding-system-alias 'shift_jis 'japanese-shift-jis)
;; (define-coding-system-alias 'sjis 'japanese-shift-jis)

;; (make-coding-system
;;  'japanese-iso-7bit-1978-irv 2 ?j
;;  "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman"
;;  '((ascii japanese-jisx0208-1978 japanese-jisx0208
;;           latin-jisx0201 japanese-jisx0212 katakana-jisx0201 t) nil nil nil
;;    short ascii-eol ascii-cntl seven nil nil use-roman use-oldjis)
;;  '(ascii japanese-jisx0208-1978 japanese-jisx0208 latin-jisx0201))

(make-coding-system
 'iso-2022-jp-1978-irv 'iso2022
 "Coding-system used for old JIS terminal."
 '(charset-g0 ascii
   short t
   seven t
   output-charset-conversion ((ascii latin-jisx0201)
			      (japanese-jisx0208 japanese-jisx0208-1978))
   mnemonic "Ja-78/7bit"
   ))

;; (define-coding-system-alias 'iso-2022-jp-1978-irv 'japanese-iso-7bit-1978-irv)
;; (define-coding-system-alias 'old-jis 'japanese-iso-7bit-1978-irv)

(define-coding-system-alias 'old-jis 'iso-2022-jp-1978-irv)

;; (make-coding-system
;;  'japanese-iso-8bit 2 ?E
;;  "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)"
;;  '(ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212
;;    short ascii-eol ascii-cntl nil nil single-shift)
;;  '((safe-charsets ascii latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978
;;                  katakana-jisx0201 japanese-jisx0212)
;;    (mime-charset . euc-jp)))

(make-coding-system
 'euc-jp 'iso2022
 "Coding-system of Japanese EUC (Extended Unix Code)."
 '(charset-g0 ascii
   charset-g1 japanese-jisx0208
   charset-g2 katakana-jisx0201
   charset-g3 japanese-jisx0212
   short t
   mnemonic "Ja/EUC"
   ))

;; (define-coding-system-alias 'euc-japan-1990 'japanese-iso-8bit)
;; (define-coding-system-alias 'euc-japan 'japanese-iso-8bit)
;; (define-coding-system-alias 'euc-jp 'japanese-iso-8bit)

(define-coding-system-alias 'euc-japan 'euc-jp) ; only for w3
(define-coding-system-alias 'japanese-euc 'euc-jp)

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment-internal)
	      (exit-function . exit-japanese-environment)
	      (tutorial . "TUTORIAL.ja")
	      (charset japanese-jisx0208 japanese-jisx0208-1978
		       japanese-jisx0212 latin-jisx0201 katakana-jisx0201)
	      (coding-system iso-2022-jp euc-jp
			     shift_jis iso-2022-jp-2)
	      (coding-priority iso-2022-jp euc-jp
			       shift_jis iso-2022-jp-2)
;;	      (input-method . "japanese")
	      (features japan-util)
	      (sample-text . "Japanese (日本語)	こんにちは, ｺﾝﾆﾁﾊ")
	      (documentation . t)))

;;; japanese.el ends here
