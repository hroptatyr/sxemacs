;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Created: 1999
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

;; Test extents operations.
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))


;;-----------------------------------------------------
;; Creating and attaching.
;;-----------------------------------------------------

(with-temp-buffer
  (let ((extent (make-extent nil nil))
	(string "somecoolstring"))

    ;; Detached extent.
    (Assert (extent-detached-p extent))

    ;; Put it in a buffer.
    (set-extent-endpoints extent 1 1 (current-buffer))
    (Assert (eq (extent-object extent) (current-buffer)))

    ;; And then into another buffer.
    (with-temp-buffer
      (set-extent-endpoints extent 1 1 (current-buffer))
      (Assert (eq (extent-object extent) (current-buffer))))

    ;; Now that the buffer doesn't exist, extent should be detached
    ;; again.
    (Assert (extent-detached-p extent))

    ;; This line crashes XEmacs 21.2.46 and prior.
    (set-extent-endpoints extent 1 (length string) string)
    (Assert (eq (extent-object extent) string))
    )

  (let ((extent (make-extent 1 1)))
    ;; By default, extent should be closed-open
    (Assert (eq (get extent 'start-closed) t))
    (Assert (eq (get extent 'start-open) nil))
    (Assert (eq (get extent 'end-open) t))
    (Assert (eq (get extent 'end-closed) nil))

    ;; Make it closed-closed.
    (set-extent-property extent 'end-closed t)

    (Assert (eq (get extent 'start-closed) t))
    (Assert (eq (get extent 'start-open) nil))
    (Assert (eq (get extent 'end-open) nil))
    (Assert (eq (get extent 'end-closed) t))

    ;; open-closed
    (set-extent-property extent 'start-open t)

    (Assert (eq (get extent 'start-closed) nil))
    (Assert (eq (get extent 'start-open) t))
    (Assert (eq (get extent 'end-open) nil))
    (Assert (eq (get extent 'end-closed) t))

    ;; open-open
    (set-extent-property extent 'end-open t)

    (Assert (eq (get extent 'start-closed) nil))
    (Assert (eq (get extent 'start-open) t))
    (Assert (eq (get extent 'end-open) t))
    (Assert (eq (get extent 'end-closed) nil)))

  )

;;-----------------------------------------------------
;; Insertion behavior.
;;-----------------------------------------------------

(defun et-range (extent)
  "List (START-POSITION END-POSITION) of EXTENT."
  (list (extent-start-position extent)
	(extent-end-position extent)))

(defun et-insert-at (string position)
  "Insert STRING at POSITION in the current buffer."
  (save-excursion
    (goto-char position)
    (insert string)))

;; Test insertion at the beginning, middle, and end of the extent.

;; closed-open

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    ;; current state: "###[eee)###"
    ;;                 123 456 789
    (Assert-Equal (et-range e) '(4 7))

    (et-insert-at "xxx" 4)

    ;; current state: "###[xxxeee)###"
    ;;                 123 456789 012
    (Assert-Equal (et-range e) '(4 10))

    (et-insert-at "yyy" 7)

    ;; current state: "###[xxxyyyeee)###"
    ;;                 123 456789012 345
    (Assert-Equal (et-range e) '(4 13))

    (et-insert-at "zzz" 13)

    ;; current state: "###[xxxyyyeee)zzz###"
    ;;                 123 456789012 345678
    (Assert-Equal (et-range e) '(4 13))
    ))

;; closed-closed

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    (put e 'end-closed t)

    ;; current state: "###[eee]###"
    ;;                 123 456 789
    (Assert-Equal (et-range e) '(4 7))

    (et-insert-at "xxx" 4)

    ;; current state: "###[xxxeee]###"
    ;;                 123 456789 012
    (Assert-Equal (et-range e) '(4 10))

    (et-insert-at "yyy" 7)

    ;; current state: "###[xxxyyyeee]###"
    ;;                 123 456789012 345
    (Assert-Equal (et-range e) '(4 13))

    (et-insert-at "zzz" 13)

    ;; current state: "###[xxxyyyeeezzz]###"
    ;;                 123 456789012345 678
    (Assert-Equal (et-range e) '(4 16))
    ))

;; open-closed

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    (put e 'start-open t)
    (put e 'end-closed t)

    ;; current state: "###(eee]###"
    ;;                 123 456 789
    (Assert-Equal (et-range e) '(4 7))

    (et-insert-at "xxx" 4)

    ;; current state: "###xxx(eee]###"
    ;;                 123456 789 012
    (Assert-Equal (et-range e) '(7 10))

    (et-insert-at "yyy" 8)

    ;; current state: "###xxx(eyyyee]###"
    ;;                 123456 789012 345
    (Assert-Equal (et-range e) '(7 13))

    (et-insert-at "zzz" 13)

    ;; current state: "###xxx(eyyyeezzz]###"
    ;;                 123456 789012345 678
    (Assert-Equal (et-range e) '(7 16))
    ))

;; open-open

(with-temp-buffer
  (insert "###eee###")
  (let ((e (make-extent 4 7)))
    (put e 'start-open t)

    ;; current state: "###(eee)###"
    ;;                 123 456 789
    (Assert-Equal (et-range e) '(4 7))

    (et-insert-at "xxx" 4)

    ;; current state: "###xxx(eee)###"
    ;;                 123456 789 012
    (Assert-Equal (et-range e) '(7 10))

    (et-insert-at "yyy" 8)

    ;; current state: "###xxx(eyyyee)###"
    ;;                 123456 789012 345
    (Assert-Equal (et-range e) '(7 13))

    (et-insert-at "zzz" 13)

    ;; current state: "###xxx(eyyyee)zzz###"
    ;;                 123456 789012 345678
    (Assert-Equal (et-range e) '(7 13))
    ))


;;-----------------------------------------------------
;; Deletion behavior.
;;-----------------------------------------------------

(dolist (props '((start-closed t end-open t)
		 (start-closed t end-open nil)
		 (start-closed nil end-open nil)
		 (start-closed nil end-open t)))
  ;; Deletion needs to behave the same regardless of the open-ness of
  ;; the boundaries.

  (with-temp-buffer
    (insert "xxxxxxxxxx")
    (let ((e (make-extent 3 9)))
      (set-extent-properties e props)

      ;; current state: xx[xxxxxx]xx
      ;;                12 345678 90
      (Assert-Equal (et-range e) '(3 9))

      (delete-region 1 2)

      ;; current state: x[xxxxxx]xx
      ;;                1 234567 89
      (Assert-Equal (et-range e) '(2 8))

      (delete-region 2 4)

      ;; current state: x[xxxx]xx
      ;;                1 2345 67
      (Assert-Equal (et-range e) '(2 6))

      (delete-region 1 3)

      ;; current state: [xxx]xx
      ;;                 123 45
      (Assert-Equal (et-range e) '(1 4))

      (delete-region 3 5)

      ;; current state: [xx]x
      ;;                 12 3
      (Assert-Equal (et-range e) '(1 3))

      )))

;;; #### Should have a test for read-only-ness and insertion and
;;; deletion!

;;-----------------------------------------------------
;; `detachable' property
;;-----------------------------------------------------

(dolist (props '((start-closed t end-open t)
		 (start-closed t end-open nil)
		 (start-closed nil end-open nil)
		 (start-closed nil end-open t)))
  ;; `detachable' shouldn't relate to region properties, hence the
  ;; loop.
  (with-temp-buffer
    (insert "###eee###")
    (let ((e (make-extent 4 7)))
      (set-extent-properties e props)
      (Assert (get e 'detachable))

      (Assert (not (extent-detached-p e)))

      (delete-region 4 5)
      ;; ###ee### (not detached yet)
      (Assert (not (extent-detached-p e)))

      (delete-region 4 6)
      ;; ###### (should be detached now)
      (Assert (extent-detached-p e))))

  (with-temp-buffer
    (insert "###eee###")
    (let ((e (make-extent 4 7)))
      (set-extent-properties e props)
      (put e 'detachable nil)
      (Assert (not (get e 'detachable)))

      (Assert (not (extent-detached-p e)))

      (delete-region 4 5)
      ;; ###ee###
      (Assert (not (extent-detached-p e)))

      (delete-region 4 6)
      ;; ###[]###
      (Assert (not (extent-detached-p e)))
      (Assert-Equal (et-range e) '(4 4))
      ))
  )


;;-----------------------------------------------------
;; Zero-length extents.
;;-----------------------------------------------------

;; closed-open (should stay put)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (et-insert-at "foo" 4)
    (Assert-Equal (et-range e) '(4 4))))

;; open-closed (should move)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (put e 'start-open t)
    (put e 'end-closed t)
    (et-insert-at "foo" 4)
    (Assert-Equal (et-range e) '(7 7))))

;; closed-closed (should extend)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (put e 'end-closed t)
    (et-insert-at "foo" 4)
    (Assert-Equal (et-range e) '(4 7))))

;; open-open (illegal; forced to behave like closed-open)
(with-temp-buffer
  (insert "######")
  (let ((e (make-extent 4 4)))
    (put e 'start-open t)
    (et-insert-at "foo" 4)
    (Assert-Equal (et-range e) '(4 4))))
