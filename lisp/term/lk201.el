;;; lk201.el --- basic editing commands for SXEmacs

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; Define function key sequences for DEC terminals.

;; Termcap or terminfo should set these.
;; (define-key function-key-map "\e[A" [up])
;; (define-key function-key-map "\e[B" [down])
;; (define-key function-key-map "\e[C" [right])
;; (define-key function-key-map "\e[D" [left])

;;; Code:

(define-key function-key-map "\e[1~" [find])
(define-key function-key-map "\e[2~" [insert])
(define-key function-key-map "\e[3~" [delete])
(define-key function-key-map "\e[4~" [select])
(define-key function-key-map "\e[5~" [prior])
(define-key function-key-map "\e[6~" [next])
(define-key function-key-map "\e[11~" [f1])
(define-key function-key-map "\e[12~" [f2])
(define-key function-key-map "\e[13~" [f3])
(define-key function-key-map "\e[14~" [f4])
(define-key function-key-map "\e[15~" [f5])
(define-key function-key-map "\e[17~" [f6])
(define-key function-key-map "\e[18~" [f7])
(define-key function-key-map "\e[19~" [f8])
(define-key function-key-map "\e[20~" [f9])
(define-key function-key-map "\e[21~" [f10])
;; Customarily F11 is used as the ESC key.
;; The file that includes this one, takes care of that.
(define-key function-key-map "\e[23~" [f11])
(define-key function-key-map "\e[24~" [f12])
(define-key function-key-map "\e[25~" [f13])
(define-key function-key-map "\e[26~" [f14])
(define-key function-key-map "\e[28~" [help])
(define-key function-key-map "\e[29~" [menu])
(define-key function-key-map "\e[31~" [f17])
(define-key function-key-map "\e[32~" [f18])
(define-key function-key-map "\e[33~" [f19])
(define-key function-key-map "\e[34~" [f20])

;; Termcap or terminfo should set these.
;; (define-key function-key-map "\eOA" [up])
;; (define-key function-key-map "\eOB" [down])
;; (define-key function-key-map "\eOC" [right])
;; (define-key function-key-map "\eOD" [left])

;; Termcap or terminfo should set these, but doesn't properly.
;; Termcap sets these to k1-k4, which get mapped to f1-f4 in term.c
(define-key function-key-map "\eOP" [kp-f1])
(define-key function-key-map "\eOQ" [kp-f2])
(define-key function-key-map "\eOR" [kp-f3])
(define-key function-key-map "\eOS" [kp-f4])

(define-key function-key-map "\eOI" [kp-tab])
(define-key function-key-map "\eOj" [kp-multiply])
(define-key function-key-map "\eOk" [kp-add])
(define-key function-key-map "\eOl" [kp-separator])
(define-key function-key-map "\eOM" [kp-enter])
(define-key function-key-map "\eOm" [kp-subtract])
(define-key function-key-map "\eOn" [kp-decimal])
(define-key function-key-map "\eOo" [kp-divide])
(define-key function-key-map "\eOp" [kp-0])
(define-key function-key-map "\eOq" [kp-1])
(define-key function-key-map "\eOr" [kp-2])
(define-key function-key-map "\eOs" [kp-3])
(define-key function-key-map "\eOt" [kp-4])
(define-key function-key-map "\eOu" [kp-5])
(define-key function-key-map "\eOv" [kp-6])
(define-key function-key-map "\eOw" [kp-7])
(define-key function-key-map "\eOx" [kp-8])
(define-key function-key-map "\eOy" [kp-9])

;;; lk201.el ends here
