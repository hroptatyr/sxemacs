;; scoansi.el --- set up key names for SCO ansi console

;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

;; Author: Kean Johnston <jkj@paradigm.co.za>

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

;; HISTORY
;;    jkj - Jan 18, 1993: Created.
;;    jkj - Nov 18, 1993: Modified to work with Emacs 19.21

;;
;; First of all, the normal cursor movement keys. Some of these, if not
;; all, should be set up my termcap/terminfo. We reset them anyway for
;; the sake of completeness.
;;
(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\e[D" [left])
(define-key function-key-map "\e[E" [center])
(define-key function-key-map "\e[F" [end])
(define-key function-key-map "\e[G" [next])
(define-key function-key-map "\e[H" [home])
(define-key function-key-map "\e[I" [prior])
(define-key function-key-map "\e[L" [insert])
(define-key function-key-map "\e[-" [kp-subtract])
(define-key function-key-map "\e[+" [kp-add])

;;
;; And now all the function keys
;;

;; Normal, unshifted keys
(define-key function-key-map "\e[M" [f1])
(define-key function-key-map "\e[N" [f2])
(define-key function-key-map "\e[O" [f3])
(define-key function-key-map "\e[P" [f4])
(define-key function-key-map "\e[Q" [f5])
(define-key function-key-map "\e[R" [f6])
(define-key function-key-map "\e[S" [f7])
(define-key function-key-map "\e[T" [f8])
(define-key function-key-map "\e[U" [f9])
(define-key function-key-map "\e[V" [f10])
(define-key function-key-map "\e[W" [f11])
(define-key function-key-map "\e[X" [f12])

;; Shift-function keys
(define-key function-key-map "\e[Y" [(shift f1)])
(define-key function-key-map "\e[Z" [(shift f2)])
(define-key function-key-map "\e[a" [(shift f3)])
(define-key function-key-map "\e[b" [(shift f4)])
(define-key function-key-map "\e[c" [(shift f5)])
(define-key function-key-map "\e[d" [(shift f6)])
(define-key function-key-map "\e[e" [(shift f7)])
(define-key function-key-map "\e[f" [(shift f8)])
(define-key function-key-map "\e[g" [(shift f9)])
(define-key function-key-map "\e[h" [(shift f10)])
(define-key function-key-map "\e[i" [(shift f11)])
(define-key function-key-map "\e[j" [(shift f12)])

;; Control function keys
(define-key function-key-map "\e[k" [(control f1)])
(define-key function-key-map "\e[l" [(control f2)])
(define-key function-key-map "\e[m" [(control f3)])
(define-key function-key-map "\e[n" [(control f4)])
(define-key function-key-map "\e[o" [(control f5)])
(define-key function-key-map "\e[p" [(control f6)])
(define-key function-key-map "\e[q" [(control f7)])
(define-key function-key-map "\e[r" [(control f8)])
(define-key function-key-map "\e[s" [(control f9)])
(define-key function-key-map "\e[t" [(control f10)])
(define-key function-key-map "\e[u" [(control f11)])
(define-key function-key-map "\e[v" [(control f12)])

;; Shift-control function keys
(define-key function-key-map "\e[w" [(control shift f1)])
(define-key function-key-map "\e[x" [(control shift f2)])
(define-key function-key-map "\e[y" [(control shift f3)])
(define-key function-key-map "\e[z" [(control shift f4)])
(define-key function-key-map "\e[@" [(control shift f5)])
(define-key function-key-map "\e[[" [(control shift f6)])
(define-key function-key-map "\e[\\" [(control shift f7)])
(define-key function-key-map "\e[]" [(control shift f8)])
(define-key function-key-map "\e[^" [(control shift f9)])
(define-key function-key-map "\e[_" [(control shift f10)])
(define-key function-key-map "\e[`" [(control shift f11)])
(define-key function-key-map "\e[{" [(control shift f12)])

;;;
;;; Now come the extended key names. Please refer to README.sco for
;;; more information regarding these keys and how to set them up.
;;;
(define-key function-key-map "\e]A" [(shift home)])
(define-key function-key-map "\e]B" [(shift up)])
(define-key function-key-map "\e]C" [(shift prior)])
(define-key function-key-map "\e]D" [(shift left)])
(define-key function-key-map "\e]E" [(shift right)])
(define-key function-key-map "\e]F" [(shift end)])
(define-key function-key-map "\e]G" [(shift down)])
(define-key function-key-map "\e]H" [(shift next)])
(define-key function-key-map "\e]I" [(shift insert)])
(define-key function-key-map "\e]J" [(shift delete)])

(define-key function-key-map "\e]K" [(control home)])
(define-key function-key-map "\e]L" [(control up)])
(define-key function-key-map "\e]M" [(control prior)])
(define-key function-key-map "\e]N" [(control left)])
(define-key function-key-map "\e]O" [(control right)])
(define-key function-key-map "\e]P" [(control end)])
(define-key function-key-map "\e]Q" [(control down)])
(define-key function-key-map "\e]R" [(control next)])
(define-key function-key-map "\e]S" [(control insert)])
(define-key function-key-map "\e]T" [(control delete)])

(define-key function-key-map "\e]U" [(meta home)])
(define-key function-key-map "\e]V" [(meta up)])
(define-key function-key-map "\e]W" [(meta prior)])
(define-key function-key-map "\e]X" [(meta left)])
(define-key function-key-map "\e]Y" [(meta right)])
(define-key function-key-map "\e]Z" [(meta end)])
(define-key function-key-map "\e]a" [(meta down)])
(define-key function-key-map "\e]b" [(meta next)])
(define-key function-key-map "\e]c" [(meta insert)])
(define-key function-key-map "\e]d" [(meta delete)])

(define-key function-key-map "\e]e" [(control center)])
(define-key function-key-map "\e]f" [(control kp-subtract)])
(define-key function-key-map "\e]g" [(control kp-add)])

(define-key function-key-map "\e]h" [(meta center)])
(define-key function-key-map "\e]i" [(meta kp-subtract)])
(define-key function-key-map "\e]j" [(meta kp-add)])
