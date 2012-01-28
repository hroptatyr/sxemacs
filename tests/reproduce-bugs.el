;;; reproduce-bugs.el --- reproduce bugs in SXEmacs;

;; Copyright (C) 1997  Free Software Foundation, Inc.
;; Copyright (C) 1997  Sun Microsystems, Inc.

;; Keywords: bugs, crash, burn, die, croak, munge

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

;; Reproduce XEmacs bugs, so that they can get fixed.
;; Especially, make XEmacs crash.
;; You may need to use a debug version of XEmacs to reproduce some of these.

;; Several global keybindings are created, each of which exhibits a bug.

;; For XEmacs maintainers and other masochists.

;; It's a bad idea to rely on code in this file continuing to work in
;; the same way. :-)

;;; Code:

(defvar bug-hashtable (make-hashtable 10))

(defmacro defbug (bug-number &rest body)
  `(puthash ,bug-number (lambda () ,@body) bug-hashtable))

(put 'defbug 'lisp-indent-function 'defun)

(defconst bug-buffer
  (save-excursion
    (set-buffer (get-buffer-create "*Bug*"))
    (erase-buffer)
    (current-buffer)))

;;;####autoload
(defun reproduce-bug (number)
  "Reproduce XEmacs bugs, so that they can get fixed.
Especially, make XEmacs crash.
See reproduce-bugs.el for bug descriptions and bug numbers.
A debug version of XEmacs may be needed to reproduce some bugs."
  (interactive "nBug Number: ")
  (funcall (gethash number bug-hashtable)))

;;; Change this to your preferred key-binding

(global-set-key  [(control ?Z)] 'reproduce-bug)

;;;; Bugs follow:

;;; ------------------------------------------------------------------
;;; Crash on trace-function
;;; Fatal error: assertion failed, file src/eval.c, line 1405, abort()
(defbug 1
  (trace-function 'record-buffer bug-buffer)
  (pop-to-buffer bug-buffer))


;;; ------------------------------------------------------------------
;;; Crashes with stack overflow
;;; Should give error via barf-if-buffer-read-only
;;; Fatal error: assertion failed, file src/eval.c, line 1874, abort()
;; This bug has been fixed. -sb
(defbug 2
  (switch-to-buffer bug-buffer)
  ;; The following line should contain a number of eight-bit characters
  (insert "²èÌÌËè¤Î°ÜÆ°¤Ï¤Ç¤­¤ë¤è¤¦¤Ë¤Ê¤ê¤Þ¤·¤¿¡£º£ÅÙ¤Ï¡¢²èÌÌ¤ÎÃæ¤Ç¡¢ÆÃÄê¤Î¾ì")
  (setq buffer-read-only t)
  (ignore-errors
    (encode-coding-region (point-min) (point-max) 'euc-japan))
  (garbage-collect))


;;; ------------------------------------------------------------------
;;; Crashes in debug version only
;;; Fatal error: assertion failed, file src/objects.h, line 149,
;;; RECORD_TYPEP (_obj, lrecord_font_instance) || MARKED_RECORD_P (_obj)
(defbug 3
  (let (glyph ext)
    (make-face 'adobe-symbol-face)
    (set-face-font
     'adobe-symbol-face
     "-adobe-symbol-medium-r-normal--*-140-*-*-p-*-adobe-fontspecific")
    (setq glyph (make-glyph (list (vector 'string
					  :data (char-to-string ?\xD3)))))
    (set-glyph-face glyph 'adobe-symbol-face)
    (setq ext (make-extent 14 18))
    (set-extent-property ext 'begin-glyph glyph)))


;;; ------------------------------------------------------------------
;;; (maybe?) crash koi8
;;; ACCL: Invalid command (c)
;;; With debugging on, crashes as follows:
;;; Fatal error: assertion failed, file src/lisp.h, line 1227, INTP (obj)
(defbug 5
  ;;(load "cyrillic")
  ;;(load "cyrillic-hooks")
  (princ (decode-coding-string "\xe1" 'koi8)))


;;; ------------------------------------------------------------------
;;; Completely Uninterruptible hang in re-search-backward (Was: java-mode)
(defbug 6
  (switch-to-buffer bug-buffer)
  (insert "{
public static void main(String[] args) throws java.io.IOException
    {
    }
}
")
  (goto-char (point-min))
  (search-forward "{" nil nil 2)
  (backward-char)
  (re-search-backward
   "^\\s(\\|\\(^[ \t]*\\(\\(\\(public\\|protected\\|static\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*\\)\\s("))


;;; ------------------------------------------------------------------
;;; regexp crash
;; This doesn't crash for me. -sb
(defbug 7
  (string-match "\\(\\s-\\|$\\)" "å"))


;;;; -------------------------------------------------------------------
;;;; Bugs below this line have been fixed.
;;;; Keep these for regression testing
;;;; -------------------------------------------------------------------


;;; ------------------------------------------------------------------
;;; Infinite recursion crash - Segmentation Fault
(defbug 4
  (switch-to-buffer bug-buffer)
  (insert "abcdefg")
  (setq e (make-extent 1 4))
  (set-extent-property e 'face 'bold)
  (set-extent-property e 'duplicable t)
  (set-extent-property e 'replicating t)
  (insert (buffer-string))
  (delete-region 8 9))

(defbug 5
  (interactive)
  (with-temp-buffer
    (insert "abc")
    (forward-char -1)
    (subst-char-in-region 1 4 ?b ?\344)
    (if (not (= (point) 3))
	(message "Bug!  point should equal 3 but is %d" (point)))))

;;; crash popup frames FIXED
;;(global-set-key
;; [(alt meta control f12)]
;; (lambda ()
;;   (interactive)
;;   (let ((f (selected-frame)))
;;     (make-frame `(popup ,(selected-frame)))
;;     (make-frame)
;;     (sit-for 0)
;;     (delete-frame f)
;;     (save-buffers-kill-emacs))))

;;; crash on delete-frame-hook - FIXED!
;;(global-set-key
;; [(alt meta control f10)]
;; (lambda ()
;;   (interactive)
;;   (setq delete-frame-hook
;;         (lambda (frame)
;;           (select-frame frame)
;;         (kill-buffer (window-buffer (frame-selected-window frame)))))))

;;; reproduce-bugs.el ends here
