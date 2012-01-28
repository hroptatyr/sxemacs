;;; ccl-tests.el --- Testsuites on CCL ; -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2000 MIYASHITA Hisashi

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

;;; Section 0.  Useful functions to construct test suites.

(defvar ccl-test-last-register-state nil)

(defun ccl-test-register-ccl-program (sym prog)
  (let ((compiled (ccl-compile prog)))
    (register-ccl-program sym compiled)
    compiled))

(defun ccl-test (prog &optional regs return-reg-idx)
  (ccl-test-register-ccl-program
   'ccl-test prog)
  (cond ((< (length regs) 8)
	 (setq ccl-test-last-register-state
	       (apply #'vector (append regs (make-list (- 8 (length regs)) 0)))))
	((> (length regs) 8)
	 (setq ccl-test-last-register-state
	       (apply #'vector (subseq regs 0 8))))
	(t
	 (setq ccl-test-last-register-state
	       (apply #'vector regs))))
  (ccl-execute
   'ccl-test
   ccl-test-last-register-state)
  (if (null return-reg-idx)
      (setq return-reg-idx 0))
  (aref ccl-test-last-register-state return-reg-idx))

(defun ccl-test-on-stream (prog string
				&optional not-check-coding-system)
  (ccl-test-register-ccl-program
   'ccl-test-decoder prog)
  (setq ccl-test-last-register-state (make-vector 9 0))
  (let ((str2
	 (ccl-execute-on-string
	  'ccl-test-decoder
	  ccl-test-last-register-state
	  string)))
    (if (not not-check-coding-system)
	(Assert (string=
		 str2
		 (decode-coding-string
		  string 'ccl-test-coding-system))))
    str2))

(defvar ccl-test-symbol-idx 0)
(defun ccl-test-generate-symbol (idx)
  (intern (format "ccl-test-map-sym-%d" idx)))

(defun ccl-test-construct-map-structure (maps &optional idx)
  (setq ccl-test-symbol-idx (if idx idx 0))
  (let (map result sym)
    (while maps
      (setq map (car maps)
	    maps (cdr maps))
      (cond ((vectorp map)
	     (setq sym (ccl-test-generate-symbol
			ccl-test-symbol-idx)
		   ccl-test-symbol-idx
		   (1+ ccl-test-symbol-idx))
	     (register-code-conversion-map
	      sym map)
	     (set sym map)
	     (setq result (cons sym result)))

	    ((symbolp map)
	     (setq result (cons sym result)))

	    ((consp map)
	     (setq result
		   (cons (ccl-test-construct-map-structure
			  map ccl-test-symbol-idx)
			 result)))
	    (t
	     (error "Unknown data:%S" map))))
    (nreverse result)))

(defun ccl-test-map-multiple (val maps)
  (ccl-test
   `(0 ((map-multiple
	 r1 r0
	 ,(ccl-test-construct-map-structure maps))))
   (list val))
  (cons (aref ccl-test-last-register-state 0)
	(aref ccl-test-last-register-state 1)))

(defun ccl-test-iterate-multiple-map (val maps)
  (ccl-test
   `(0 ((iterate-multiple-map
	 r1 r0
	 ,@(ccl-test-construct-map-structure maps))))
   (list val))
  (cons (aref ccl-test-last-register-state 0)
	(aref ccl-test-last-register-state 1)))

(defun ccl-test-setup ()
  (define-ccl-program
    ccl-test-decoder
    '(1 (read r0)
	(loop
	  (write-read-repeat r0))))
  (define-ccl-program
    ccl-test-encoder
    '(1 (read r0)
	(loop
	  (write-read-repeat r0))))
  (make-coding-system
   'ccl-test-coding-system
   'ccl
   "CCL TEST temprary coding-system."
   '(mnemonic "CCL-TEST"
     eol-type lf
     decode ccl-test-decoder
     encode ccl-test-encoder)))

;;; Section 1. arithmetic operations.

(defun ccl-test-normal-expr ()
  ;; normal-expr
  (let ((r0 0) (r1 10) (r2 20) (r3 21) (r4 7))
    (Assert (= (ccl-test '(0 ((r0 = ((((r1 * r2) + r3) % r4) << 2))))
			 (list r0 r1 r2 r3 r4))
	       (ash (% (+ (* r1 r2) r3) r4) 2))))

  (Assert (\= (ccl-test '(0 ((r2 = (r1 < 10))
			     (r0 = (r2 > 10))))
			'(0 5))
	   0))

  (let ((r0 0) (r1 #x10FF) (r2 #xCC) (r3 #xE0))
    (Assert (= (ccl-test '(0 ((r0 = (((r1 & #xFF) ^ r2) | r3))))
			 (list r0 r1 r2 r3))
	       (logior (logxor (logand r1 #xFF) r2) r3))))

  ;; checking range of SJIS
  ;; 81(40-7E, 80-FC), 82, 9F, E0, E1, EF

  (let ((hs '(#x81 #x82 #x9F #xE0 #xE1 #xEF))
	func high low)
    (setq func
	  (lambda (high low)
	    (let (ch c1 c2)
	      (setq ch (split-char (decode-shift-jis-char
				    (cons high low))))
	      (setq c1 (nth 1 ch)
		    c2 (nth 2 ch))
	      (ccl-test '(0 ((r0 = (r1 de-sjis r2))))
			(list 0 high low))
	      (Assert (and (= c1 (aref ccl-test-last-register-state 0))
			   (= c2 (aref ccl-test-last-register-state 7))))
	      (ccl-test '(0 ((r0 = (r1 en-sjis r2))))
			(list 0 c1 c2))
	      (Assert (and (= high (aref ccl-test-last-register-state 0))
			   (= low (aref ccl-test-last-register-state 7)))))))
    (while (setq high (car hs))
      (setq hs (cdr hs))
      (setq low #x40)
      (while (<= low #x7E)
	(funcall func high low)
	(setq low (1+ low)))
      (setq low #x80)
      (while (<= low #xFC)
	(funcall func high low)
	(setq low (1+ low)))))

  ;; self-expr
  (Assert (= (ccl-test '(0 ((r0 += 20)
			    (r0 *= 40)
			    (r0 -= 15)))
		       '(100))
	     (- (* (+ 100 20) 40) 15)))

  ;; ref. array
  (Assert (= (ccl-test '(0 ((r0 = r0 [100 101 102 103 104])))
		       '(3))
	     103)))

;;; Section 2.  Simple read and write
(defun ccl-test-simple-read-and-write ()
  ;; constant
  (let* ((str "1234567890abcdefghij")
	 (dum (make-string 1 ?X)))
    (Assert
     (string= (ccl-test-on-stream
	       `(,(length str)
		 ((loop (read r0) (write ,str)))) dum)
	      str)))
  ;; register
  (let* ((str "1234567890abcdefghij"))
    (Assert
     (string= (ccl-test-on-stream `(1 ((read r0)
				       (loop
					 (write r0)
					 (read r0)
					 (repeat))))
				  str)
	      str))
    (Assert
     (string= (ccl-test-on-stream `(1 ((read r0)
				       (loop
					 (write-read-repeat r0))))
				  str)
	      str)))

  ;; expression
  (let ((str "1234567890abcdefghij")
	str2 i len)
    (setq str2 ""
	  len (length str)
	  i 0)
    (while (< i len)
      (setq str2 (concat str2 (char-to-string
			       (+ (char-to-int (aref str i)) 3))))
      (setq i (1+ i)))
    (Assert
     (string= (ccl-test-on-stream `(1 ((read r0)
				       (loop
					 (write (r0 + 3))
					 (read r0)
					 (repeat))))
				  str)
	      str2))
    (Assert
     (string= (ccl-test-on-stream `(1 ((read r0)
				       (loop
					 (r0 += 3)
					 (write-read-repeat r0))))
				  str)
	      str2)))


  ;; write via array
  (let* ((str (mapconcat (lambda (x) (char-to-string (int-to-char x)))
			 '(0 1 2 3 4 5 6) "")))
    (Assert
     (string= (ccl-test-on-stream
	       `(1 ((read r0)
		    (loop
		      (write r0
			     ,(vector (make-char 'japanese-jisx0208 36 34)
				      (make-char 'japanese-jisx0208 36 36)
				      (make-char 'japanese-jisx0208 36 38)
				      (make-char 'japanese-jisx0208 36 40)
				      (make-char 'japanese-jisx0208 36 42)
				      (make-char 'japanese-jisx0208 36 43)
				      (make-char 'japanese-jisx0208 36 45)
				      (make-char 'japanese-jisx0208 36 47)
				      (make-char 'japanese-jisx0208 36 49)
				      (make-char 'japanese-jisx0208 36 51)))
		      (read r0)
		      (repeat))))
	       str t)
	      (mapconcat #'char-to-string
			 (list (make-char 'japanese-jisx0208 36 34)
			       (make-char 'japanese-jisx0208 36 36)
			       (make-char 'japanese-jisx0208 36 38)
			       (make-char 'japanese-jisx0208 36 40)
			       (make-char 'japanese-jisx0208 36 42)
			       (make-char 'japanese-jisx0208 36 43)
			       (make-char 'japanese-jisx0208 36 45))
			 "")))))

;;; Section 3. read-multibyte-character, and write-multibyte-character
(defun ccl-test-read-write-multibyte-character ()
  ;; simple test.
  (let* ((str (concat "LMDXXX..."
		      (mapconcat #'char-to-string
				 (list (make-char 'japanese-jisx0208 36 36)
				       (make-char 'japanese-jisx0208 36 36)
				       (make-char 'japanese-jisx0208 50 67)
				       (make-char 'japanese-jisx0208 56 58)
				       (make-char 'japanese-jisx0208 72 104)
				       (make-char 'japanese-jisx0208 36 108)
				       (make-char 'japanese-jisx0208 36 70)
				       (make-char 'japanese-jisx0208 36 45)
				       (make-char 'japanese-jisx0208 36 63)
				       (make-char 'japanese-jisx0208 33 35))
				 "")
		      "...")))
    (Assert
     (string=
      (ccl-test-on-stream
       `(1 ((loop
	      (read-multibyte-character r0 r1)
	      (write-multibyte-character r0 r1)
	      (repeat))))
       str t)
      str)))
  ;;
  )

;;; Section 4. CCL call
(defun ccl-test-ccl-call ()
  ;; set up
  (define-ccl-program
    ccl-test-sub1
    '(0
      ((r5 = ?z))))
  (define-ccl-program
    ccl-test-sub2
    '(0
      ((call ccl-test-sub1)
       (r0 = (r5 * 20)))))
  (define-ccl-program
    ccl-test-sub3
    '(1
      ((call ccl-test-sub2)
       (write r5)
       (write (r0 / 20)))))
  (Assert (string=
	   (ccl-test-on-stream
	    '(1 ((loop (read r0) (call ccl-test-sub3))))
	    "A")
	   "zz")))

;;; Section 5. Map-instructions
(defun ccl-test-map-instructions ()
  ;; set up
  (define-ccl-program
    ccl-test-arith-1
    '(0
      ((r0 += 1000000))))

  (define-ccl-program
    ccl-test-lambda
    '(0
      ((r0 = -3))))

  (define-ccl-program
    ccl-test-t
    '(0
      ((r0 = -2))))

  (define-ccl-program
    ccl-test-nil
    '(0
      ((r0 = -1))))

  ;; 1-level normal 1 mapping
  (Assert-Equal
	   (mapcar
	    (lambda (val)
	      (ccl-test-map-multiple
	       val
	       '([100 1 2 3 4 5])))
	    '(0 99 100 101 102 103 104 105 106 107))
	   '((0 . -1) (99 . -1)
	     (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)
	     (105 . -1) (106 . -1) (107 . -1)))

  (Assert-Equal
	   (mapcar
	    (lambda (val)
	      (ccl-test-iterate-multiple-map
	       val
	       '([100 1 2 3 4 5])))
	    '(0 99 100 101 102 103 104 105 106 107))
	   '((0 . -1) (99 . -1)
	     (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)
	     (105 . -1) (106 . -1) (107 . -1)))

  ;; 1-level normal 2 mappings
  (Assert-Equal
	   (mapcar
	    (lambda (val)
	      (ccl-test-map-multiple
	       val
	       '([100 1 2 nil 4 5]
		 [101 12 13 14 15 16 17])))
	    '(0 99 100 101 102 103 104 105 106 107))
	   '((0 . -1) (99 . -1) (1 . 0) (2 . 0)
	     (13 . 1) (4 . 0) (5 . 0) (16 . 1) (17 . 1)
	     (107 . -1)))

  (Assert-Equal
	   (mapcar
	    (lambda (val)
	      (ccl-test-iterate-multiple-map
	       val
	       '([100 1 2 3 4 5]
		 [101 12 13 14 15 16 17])))
	    '(0 99 100 101 102 103 104 105 106 107))
	   '((0 . -1) (99 . -1) (1 . 0) (2 . 0) (3 . 0)
	     (4 . 0) (5 . 0) (16 . 1) (17 . 1) (107 . -1)))


  ;; 1-level normal 7 mappings
  (Assert-Equal
	   (mapcar
	    (lambda (val)
	      (ccl-test-map-multiple
	       val
	       '([100 1 2 nil 4 5]
		 [101 12 13 14 15 16 17]
		 [1000 101 102 103 nil 105 106 nil 108]
		 [1005 1006 1007 1008 1009 1010 1011 1012]
		 [10005 10006 10007 10008 10009 10010 10011 10012]
		 [20000 20000 20001 20002 nil 20004 20005 20006]
		 [20003 30000 30010 30020 30030 30040 30050 30060]
		 )))
	    '(0 99 100 101 102 103 104 105 106 107
		998 999 1000 1001 1002 1003 1004 1005 1006 1007
		9999 10000 10001 10002 10003 10004
		19999 20000 20001 20002 20003 20004
		20005 20006))
	   '((0 . -1) (99 . -1) (1 . 0) (2 . 0) (13 . 1) (4 . 0)
	     (5 . 0) (16 . 1) (17 . 1) (107 . -1) (998 . -1)
	     (999 . -1) (101 . 2) (102 . 2) (103 . 2) (1003 . -1)
	     (105 . 2) (106 . 2) (1007 . 3) (108 . 2) (9999 . -1)
	     (10000 . -1) (10001 . -1) (10002 . -1) (10003 . -1)
	     (10004 . -1) (19999 . -1) (20000 . 5) (20001 . 5)
	     (20002 . 5) (30000 . 6) (20004 . 5) (20005 . 5) (20006 . 5)))

      (Assert-Equal
	       (mapcar
		(lambda (val)
		  (ccl-test-iterate-multiple-map
		   val
		   '([100 1 2 nil 4 5]
		     [101 12 13 14 15 16 17]
		     [1000 101 102 103 nil 105 106 nil 108]
		     [1005 1006 1007 1008 1009 1010 1011 1012]
		     [10005 10006 10007 10008 10009 10010 10011 10012]
		     [20000 20000 20001 20002 nil 20004 20005 20006]
		     [20003 30000 30010 30020 30030 30040 30050 30060]
		     )))
		'(0 99 100 101 102 103 104 105 106 107
		    998 999 1000 1001 1002 1003 1004 1005 1006 1007
		    9999 10000 10001 10002 10003 10004
		    19999 20000 20001 20002 20003 20004
		    20005 20006))
	       '((0 . -1) (99 . -1) (1 . 0) (2 . 0) (13 . 1) (4 . 0)
		 (5 . 0) (16 . 1) (17 . 1) (107 . -1) (998 . -1)
		 (999 . -1) (101 . 2) (102 . 2) (103 . 2) (1003 . -1)
		 (105 . 2) (106 . 2) (1007 . 3) (108 . 2) (9999 . -1)
		 (10000 . -1) (10001 . -1) (10002 . -1) (10003 . -1)
		 (10004 . -1) (19999 . -1) (20000 . 5) (20001 . 5)
		 (20002 . 5)(30000 . 6)(20004 . 5)(20005 . 5)(20006 . 5)))

      ;; 1-level 7 mappings including CCL call

      (Assert-Equal
	       (mapcar
		(lambda (val)
		  (ccl-test-map-multiple
		   val
		   '([100 1 2 nil 4 5]
		     [101 12 13 14 15 16 17]
		     [1000 101 ccl-test-arith-1 103 nil 105 106 ccl-test-nil 108]
		     [1005 1006 1007 1008 1009 ccl-test-lambda 1011 1012]
		     [10005 10006 10007 10008 10009 10010 10011 10012]
		     [20000 20000 20001 20002 nil 20004 20005 20006]
		     [20003 30000 30010 30020 30030 30040 30050 30060]
		     )))
		'(0 99 100 101 102 103 104 105 106 107
		    998 999 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009
		    9999 10000 10001 10002 10003 10004
		    19999 20000 20001 20002 20003 20004
		    20005 20006))
	       '((0 . -1) (99 . -1) (1 . 0) (2 . 0) (13 . 1) (4 . 0)
		 (5 . 0) (16 . 1) (17 . 1) (107 . -1) (998 . -1)
		 (999 . -1) (101 . 2) (1001001 . 2) (103 . 2)
		 (1003 . -1) (105 . 2) (106 . 2) (1007 . 3) (108 . 2)
		 (1009 . 3) (1009 . 3) (9999 . -1) (10000 . -1)
		 (10001 . -1) (10002 . -1) (10003 . -1) (10004 . -1)
		 (19999 . -1) (20000 . 5) (20001 . 5) (20002 . 5)
		 (30000 . 6)(20004 . 5)(20005 . 5)(20006 . 5)))

      (Assert-Equal
	       (mapcar
		(lambda (val)
		  (ccl-test-iterate-multiple-map
		   val
		   '([100 1 2 nil 4 5]
		     [101 12 13 14 15 16 17]
		     [1000 101 ccl-test-arith-1 103 nil 105 106 ccl-test-nil 108]
		     [1005 1006 1007 1008 1009 ccl-test-lambda 1011 1012]
		     [10005 10006 10007 10008 10009 10010 10011 10012]
		     [20000 20000 20001 20002 nil 20004 20005 20006]
		     [20003 30000 30010 30020 30030 30040 30050 30060]
		     )))
		'(0 99 100 101 102 103 104 105 106 107
		    998 999 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009
		    9999 10000 10001 10002 10003 10004
		    19999 20000 20001 20002 20003 20004
		    20005 20006))
	       '((0 . -1) (99 . -1) (1 . 0) (2 . 0) (13 . 1) (4 . 0)
		 (5 . 0) (16 . 1) (17 . 1) (107 . -1) (998 . -1)
		 (999 . -1) (101 . 2) (1001001 . 0) (103 . 2)
		 (1003 . -1) (105 . 2) (106 . 2) (-1 . 0) (108 . 2)
		 (1009 . 3) (-3 . 0) (9999 . -1) (10000 . -1)
		 (10001 . -1) (10002 . -1) (10003 . -1) (10004 . -1)
		 (19999 . -1) (20000 . 5) (20001 . 5) (20002 . 5)
		 (30000 . 6) (20004 . 5) (20005 . 5) (20006 . 5)))

      ;; 3-level mappings
      (Assert-Equal
	       (mapcar
		(lambda (val)
		  (ccl-test-map-multiple
		   val
		   '([100 1 2 nil 4 5]
		     [101 12 13 14 15 16 17]
		     [1000 101 102 103 nil 105 106 nil 108]
		     (([1005 1006 1007 1008 1009 1010 1011 1012]
		       [10005 10006 20007 20008 10009 10010 10011 10012])
		      [20000 20000 20001 20002 nil 20004 20005 20006]
		      [1006 2006 2007 2008 2009 2010]
		      ([20003 30000 30010 30020 30030 30040 30050 30060]))
		     [t t 0 1000000]
		     [1008 1108 1109 1110 1111 1112 1113])))
		'(0 99 100 101 102 103 104 105 106 107
		    998 999 1000 1001 1002 1003 1004 1005 1006 1007
		    1008 1009 1010 1011 1012 1013 1014
		    9999 10000 10001 10002 10003 10004
		    10005 10006 10007 10008 10009 10010
		    19999 20000 20001 20002 20003 20004
		    20005 20006))
	       '((0 . 11) (99 . 11) (1 . 0) (2 . 0) (13 . 1)
		 (4 . 0) (5 . 0) (16 . 1) (17 . 1) (107 . 11)
		 (998 . 11) (999 . 11) (101 . 2) (102 . 2)
		 (103 . 2) (1003 . 11) (105 . 2) (106 . 2)
		 (1006 . 11) (108 . 2) (1108 . 12) (1109 . 12)
		 (1110 . 12)  (1111 . 12) (1112 . 12) (1113 . 12)
		 (1014 . 11) (9999 . 11) (10000 . 11) (10001 . 11)
		 (10002 . 11) (10003 . 11) (10004 . 11) (10005 . 11)
		 (30040 . 10) (30050 . 10) (10008 . 11) (10009 . 11)
		 (10010 . 11) (19999 . 11) (20000 . 11) (20001 . 11)
		 (20002 . 11) (20003 . 11) (20004 . 11) (20005 . 11)
		 (20006 . 11)))


      ;; 3-level mappings including CCL call
      (Assert-Equal
	       (mapcar
		(lambda (val)
		  (ccl-test-map-multiple
		   val
		   '([100 1 2 nil 4 5]
		     [101 12 13 14 15 16 17]
		     [1000 101 102 103 nil ccl-test-arith-1 106 nil 108]
		     (([1005 1006 1007 1008 1009 1010 1011 ccl-test-arith-1
			     70 71 72 73]
		       [10005 10006 20007 20008 10009 10010 10011 10012])
		      [70 ccl-test-t ccl-test-lambda ccl-test-nil ccl-test-nil]
		      [72 lambda]
		      [20000 20000 20001 20002 nil 20004 20005 20006]
		      [1006 2006 2007 2008 2009 2010]
		      ([20003 30000 30010 ccl-test-arith-1 30030 30040
			      ccl-test-arith-1 30060]
		       [1001010 50 51 52 53 54 55]))
		     [t t 0 1000000]
		     [t ccl-test-arith-1 0 10]
		     [1008 1108 1109 1110 1111 1112 1113])))
		'(0 99 100 101 102 103 104 105 106 107
		    998 999 1000 1001 1002 1003 1004 1005 1006 1007
		    1008 1009 1010 1011 1012 1013 1014 1015 1016
		    9999 10000 10001 10002 10003 10004
		    10005 10006 10007 10008 10009 10010
		    19999 20000 20001 20002 20003 20004
		    20005 20006))
	       '((1000000 . 15) (99 . 14) (1 . 0) (2 . 0) (13 . 1)
		 (4 . 0) (5 . 0) (16 . 1) (17 . 1) (107 . 14) (998 . 14)
		 (999 . 14) (101 . 2) (102 . 2) (103 . 2) (1003 . 14)
		 (1001004 . 2) (106 . 2) (1006 . 14) (108 . 2) (1108 . 16)
		 (1109 . 16) (1110 . 16) (51 . 13) (1112 . 16) (71 . 7)
		 (72 . 8) (1015 . 14) (1016 . 14) (9999 . 14) (10000 . 14)
		 (10001 . 14) (10002 . 14) (10003 . 14) (10004 . 14)
		 (10005 . 14) (30040 . 12) (1020008 . 12) (10008 . 14)
		 (10009 . 14) (10010 . 14) (19999 . 14) (20000 . 14)
		 (20001 . 14) (20002 . 14) (20003 . 14) (20004 . 14)
		 (20005 . 14) (20006 . 14)))
      ;; All map-instruction tests ends here.
      )

(defun ccl-test-suites ()
  (ccl-test-setup)
  (ccl-test-normal-expr)
  (ccl-test-simple-read-and-write)
  (ccl-test-read-write-multibyte-character)
  (ccl-test-ccl-call)
  (ccl-test-map-instructions))

;;; start tests only when ccl-execute is enabled.
(if (fboundp 'ccl-execute)
    (ccl-test-suites))

;;; ccl-test.el ends here.
