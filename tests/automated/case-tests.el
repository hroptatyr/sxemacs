;;; -*- coding: iso-8859-1 -*-

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Maintainer: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Created: 2000
;; Keywords: tests

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test case-table related functionality.

(Assert (case-table-p (standard-case-table)))
;; Old case table test.
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       nil nil nil)))
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       (make-string 256 ?b)
		       nil nil)))
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       (make-string 256 ?b)
		       (make-string 256 ?c)
		       nil)))
(Assert (case-table-p (list
		       (make-string 256 ?a)
		       (make-string 256 ?b)
		       (make-string 256 ?c)
		       (make-string 256 ?d))))
(Assert (not (case-table-p (list (make-string 256 ?a)
				 (make-string 256 ?b)
				 (make-string 256 ?c)
				 (make-string 254 ?d)))))
(Assert (not (case-table-p (list (make-string 256 ?a)))))

(Assert (case-table-p (set-case-table (current-case-table))))

(defvar string-0-through-32
  (let ((result (make-string 33 (int-to-char 0))))
    (dotimes (i 33)
      (aset result i (int-to-char i)))
    result)
  "String containing characters from code point 0 (NUL) through 32 (SPC).")

(defvar string-127-through-160
  (let ((result (make-string 34 (int-to-char 0))))
    (dotimes (i 34)
      (aset result i (int-to-char (+ 127 i))))
    result)
  "String containing characters from code point 127 (DEL) through 160
\(no-break-space).")

;; Case table sanity check.
(let ((downcase-string
       (concat string-0-through-32
	       "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
	       string-127-through-160
		"¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"))
       (upcase-string
	(concat string-0-through-32
		"!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~"
		string-127-through-160
		"¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ"))
       (table (standard-case-table)))
  (dotimes (i 256)
    (Assert (eq (get-case-table 'downcase (int-to-char i) table)
		(aref downcase-string i)))
    (Assert (eq (get-case-table 'upcase (int-to-char i) table)
		(aref upcase-string i)))))

(Check-Error-Message error "Char case must be downcase or upcase"
		     (get-case-table 'foo ?a (standard-case-table)))

(Assert
 (string=
  (upcase "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")
  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(Assert
 (string=
  (upcase "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(Assert
 (string=
  (upcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ"))

(Assert
 (string=
  (upcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ"))

(Assert
 (string=
  (downcase "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")
  "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz"))

(Assert
 (string=
  (downcase "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz"))

(Assert
 (string=
  (downcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"))

(Assert
 (string=
  (downcase " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞÿ")
  " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"))

;; Old case table format test.
(with-temp-buffer
  (set-case-table
   (list
    (concat string-0-through-32
	     "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
	     string-127-through-160
	     "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
     nil nil nil))
  (Assert
   (string=
    (upcase "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")
    "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (Assert
   (string=
    (downcase "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    "!\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz")))

(with-temp-buffer
  (insert "Test Buffer")
  (let ((case-fold-search t))
    (goto-char (point-min))
    (Assert (eq (search-forward "test buffer" nil t) 12))
    (goto-char (point-min))
    (Assert (eq (search-forward "Test buffer" nil t) 12))
    (goto-char (point-min))
    (Assert (eq (search-forward "Test Buffer" nil t) 12))

    (setq case-fold-search nil)
    (goto-char (point-min))
    (Assert (not (search-forward "test buffer" nil t)))
    (goto-char (point-min))
    (Assert (not (search-forward "Test buffer" nil t)))
    (goto-char (point-min))
    (Assert (eq (search-forward "Test Buffer" nil t) 12))))

(with-temp-buffer
  (insert "abcdefghijklmnäopqrstuÄvwxyz")
  ;; case insensitive
  (Assert (not (search-forward "ö" nil t)))
  (goto-char (point-min))
  (Assert (eq 16 (search-forward "ä" nil t)))
  (Assert (eq 24 (search-forward "ä" nil t)))
  (goto-char (point-min))
  (Assert (eq 16 (search-forward "Ä" nil t)))
  (Assert (eq 24 (search-forward "Ä" nil t)))
  (goto-char (point-max))
  (Assert (eq 23 (search-backward "ä" nil t)))
  (Assert (eq 15 (search-backward "ä" nil t)))
  (goto-char (point-max))
  (Assert (eq 23 (search-backward "Ä" nil t)))
  (Assert (eq 15 (search-backward "Ä" nil t)))
  ;; case sensitive
  (setq case-fold-search nil)
  (goto-char (point-min))
  (Assert (not (search-forward "ö" nil t)))
  (goto-char (point-min))
  (Assert (eq 16 (search-forward "ä" nil t)))
  (Assert (not (search-forward "ä" nil t)))
  (goto-char (point-min))
  (Assert (eq 24 (search-forward "Ä" nil t)))
  (goto-char 16)
  (Assert (eq 24 (search-forward "Ä" nil t)))
  (goto-char (point-max))
  (Assert (eq 15 (search-backward "ä" nil t)))
  (goto-char 15)
  (Assert (not (search-backward "ä" nil t)))
  (goto-char (point-max))
  (Assert (eq 23 (search-backward "Ä" nil t)))
  (Assert (not (search-backward "Ä" nil t))))

(with-temp-buffer
  (insert "aaaaäÄäÄäÄäÄäÄbbbb")
  (goto-char (point-min))
  (Assert (eq 15 (search-forward "ää" nil t 5)))
  (goto-char (point-min))
  (Assert (not (search-forward "ää" nil t 6)))
  (goto-char (point-max))
  (Assert (eq 5 (search-backward "ää" nil t 5)))
  (goto-char (point-max))
  (Assert (not (search-backward "ää" nil t 6))))

(when (featurep 'mule)
  (let* ((hiragana-a (make-char 'japanese-jisx0208 36 34))
	 (a-diaeresis ?ä)
	 (case-table (copy-case-table (standard-case-table)))
	 (str-hiragana-a (char-to-string hiragana-a))
	 (str-a-diaeresis (char-to-string a-diaeresis))
	 (string (concat str-hiragana-a str-a-diaeresis)))
    (put-case-table-pair hiragana-a a-diaeresis case-table)
    (with-temp-buffer
      (set-case-table case-table)
      (insert hiragana-a "abcdefg" a-diaeresis)
      ;; forward
      (goto-char (point-min))
      (Assert (not (search-forward "ö" nil t)))
      (goto-char (point-min))
      (Assert (eq 2 (search-forward str-hiragana-a nil t)))
      (goto-char (point-min))
      (Assert (eq 2 (search-forward str-a-diaeresis nil t)))
      (goto-char (1+ (point-min)))
      (Assert (eq (point-max)
		  (search-forward str-hiragana-a nil t)))
      (goto-char (1+ (point-min)))
      (Assert (eq (point-max)
		  (search-forward str-a-diaeresis nil t)))
      ;; backward
      (goto-char (point-max))
      (Assert (not (search-backward "ö" nil t)))
      (goto-char (point-max))
      (Assert (eq (1- (point-max)) (search-backward str-hiragana-a nil t)))
      (goto-char (point-max))
      (Assert (eq (1- (point-max)) (search-backward str-a-diaeresis nil t)))
      (goto-char (1- (point-max)))
      (Assert (eq 1 (search-backward str-hiragana-a nil t)))
      (goto-char (1- (point-max)))
      (Assert (eq 1 (search-backward str-a-diaeresis nil t)))
      (replace-match "a")
      (Assert (looking-at (format "abcdefg%c" a-diaeresis))))
    (with-temp-buffer
      (set-case-table case-table)
      (insert string)
      (insert string)
      (insert string)
      (insert string)
      (insert string)
      (goto-char (point-min))
      (Assert (eq 11 (search-forward string nil t 5)))
      (goto-char (point-min))
      (Assert (not (search-forward string nil t 6)))
      (goto-char (point-max))
      (Assert (eq 1 (search-backward string nil t 5)))
      (goto-char (point-max))
      (Assert (not (search-backward string nil t 6))))))
