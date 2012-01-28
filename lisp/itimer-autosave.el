;;; itimer-autosave.el --- Autosave functions with itimers

;; Copyright status unknown

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; itimer-driven auto-saves

;;; Code:

;jwz: this is preloaded so don't ;;;###autoload
(defvar auto-save-timeout 960
  "*Number of seconds idle time before auto-save.
Zero or nil means disable auto-saving due to idleness.

The actual amount of idle time between auto-saves is logarithmically related
to the size of the current buffer.  This variable is the number of seconds
after which an auto-save will happen when the current buffer is 50k or less;
the timeout will be 2 1/4 times this in a 200k buffer, 3 3/4 times this in a
1000k buffer, and 4 1/2 times this in a 2000k buffer.

See also the variable `auto-save-interval', which controls auto-saving based
on the number of characters typed.")

;jwz: this is preloaded so don't ;;;###autoload
(defvar auto-gc-threshold (/ gc-cons-threshold 3)
  "*GC when this many bytes have been consed since the last GC,
and the user has been idle for `auto-save-timeout' seconds.")

(defun auto-save-itimer ()
  "For use as a itimer callback function.
Auto-saves and garbage-collects based on the size of the current buffer
and the value of `auto-save-timeout', `auto-gc-threshold', and the current
keyboard idle-time."
  (if (or (null auto-save-timeout)
	  (<= auto-save-timeout 0)
	  (eq (minibuffer-window) (selected-window)))
      nil
    (let ((buf-size (1+ (ash (buffer-size) -8)))
	  (delay-level 0)
	  (now (current-time))
	  delay)
      (while (> buf-size 64)
	(setq delay-level (1+ delay-level)
	      buf-size (- buf-size (ash buf-size -2))))
      (if (< delay-level 4)
	  (setq delay-level 4))
      ;; delay_level is 4 for files under around 50k, 7 at 100k, 9 at 200k,
      ;; 11 at 300k, and 12 at 500k, 15 at 1 meg, and 17 at 2 meg.
      (setq delay (/ (* delay-level auto-save-timeout) 4))
      (let ((idle-time (if (or (not (consp last-input-time))
			       (/= (car now) (car last-input-time)))
			   (1+ delay)
			 (- (car (cdr now)) (cdr last-input-time)))))
	(and (> idle-time delay)
	     (do-auto-save))
	(and (> idle-time auto-save-timeout)
	     (> (consing-since-gc) auto-gc-threshold)
	     (garbage-collect)))))
  ;; Look at the itimer that's currently running; if the user has changed
  ;; the value of auto-save-timeout, modify this itimer to have the correct
  ;; restart time.  There will be some latency between when the user changes
  ;; this variable and when it takes effect, but it will happen eventually.
  (let ((self (get-itimer "auto-save")))
    (or self (error "auto-save-itimer can't find itself"))
    (if (and auto-save-timeout (> auto-save-timeout 4))
	(or (= (itimer-restart self) (/ auto-save-timeout 4))
	    (set-itimer-restart self (/ auto-save-timeout 4)))))
  nil)

(defun itimer-init-auto-gc ()
  (or noninteractive ; may be being run from after-init-hook in -batch mode.
      (get-itimer "auto-save")
      ;; the time here is just the first interval; if the user changes it
      ;; later, it will adjust.
      (let ((time (max 2 (/ (or auto-save-timeout 30) 4))))
	(start-itimer "auto-save" 'auto-save-itimer time time))))

(cond (purify-flag
       ;; This file is being preloaded into an emacs about to be dumped.
       ;; So arrange for the auto-save itimer to be started once emacs
       ;; is launched.
       (add-hook 'after-init-hook 'itimer-init-auto-gc))
      (t
       ;; Otherwise, this file is being loaded into a normal, interactive
       ;; emacs.  Start the auto-save timer now.
       (itimer-init-auto-gc)))


;;; itimer-autosave.el ends here
