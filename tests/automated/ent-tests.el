;;;  ent-tests.el -- Tests for Enhanced Number Types
;; Copyright (C) 2005 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: tests
;;
;; This file is part of SXEmacs.
;;
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
;;
;;; Synched up with: Not in FSF.
;;
;;; Commentary:
;; - test for conceptionally correct arithmetic
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
;; Test categories
;;-----------------------------------------------------

;;; test simple syntaxes
;; this tests for `1' being read and coerced to a fixnum
(let ((num 1))
  (Assert (intp num))
  (Assert (not (floatp num)))
  (Assert (integerp num))
  (Assert (rationalp num))
  (Assert (not (realp num)))
  (Assert (comparablep num))
  (Assert (not (complexp num)))
  (Assert (archimedeanp num))
  (Assert (numberp num))

  (when (featurep 'bigz)
    (Assert (not (bigzp num))))
  (when (featurep 'bigq)
    (Assert (not (bigqp num))))
  (when (featurep 'bigf)
    (Assert (not (bigfp num))))
  (when (featurep 'bigfr)
    (Assert (not (bigfrp num))))
  (when (featurep 'bigg)
    (Assert (not (biggp num))))
  (when (featurep 'bigc)
    (Assert (not (bigcp num)))))


;; this tests for `1/2' being read and coerced to a fraction
(when (featurep 'bigq)
  (let ((num 1/2))
    (Assert (not (intp num)))
    (Assert (not (floatp num)))
    (Assert (not (integerp num)))
    (Assert (rationalp num))
    (Assert (not (realp num)))
    (Assert (comparablep num))
    (Assert (not (complexp num)))
    (Assert (archimedeanp num))
    (Assert (numberp num))

    (when (featurep 'bigz)
      (Assert (not (bigzp num))))
    (when (featurep 'bigq)
      (Assert (bigqp num)))
    (when (featurep 'bigf)
      (Assert (not (bigfp num))))
    (when (featurep 'bigfr)
      (Assert (not (bigfrp num))))
    (when (featurep 'bigg)
      (Assert (not (biggp num))))
    (when (featurep 'bigc)
      (Assert (not (bigcp num))))))


;; this tests for `1.0' being read and coerced to a float
(let* ((read-real-as 'float)
       (num 1.0))
  (Assert (not (intp num)))
  (Assert (floatp num))
  (Assert (not (integerp num)))
  (Assert (not (rationalp num)))
  (Assert (realp num))
  (Assert (comparablep num))
  (Assert (not (complexp num)))
  (Assert (archimedeanp num))
  (Assert (numberp num))

  (when (featurep 'bigz)
    (Assert (not (bigzp num))))
  (when (featurep 'bigq)
    (Assert (not (bigqp num))))
  (when (featurep 'bigf)
    (Assert (not (bigfp num))))
  (when (featurep 'bigfr)
    (Assert (not (bigfrp num))))
  (when (featurep 'bigg)
    (Assert (not (biggp num))))
  (when (featurep 'bigc)
    (Assert (not (bigcp num)))))


;; this tests for `1+i' being read and coerced to a Gaussian, if provided
(when (featurep 'bigg)
  (let ((num 1+i))
    (Assert (not (intp num)))
    (Assert (not (floatp num)))
    (Assert (not (integerp num)))
    (Assert (not (rationalp num)))
    (Assert (not (realp num)))
    (Assert (not (comparablep num)))
    (Assert (complexp num))
    (Assert (archimedeanp num))
    (Assert (numberp num))

    (when (featurep 'bigz)
      (Assert (not (bigzp num))))
    (when (featurep 'bigq)
      (Assert (not (bigqp num))))
    (when (featurep 'bigf)
      (Assert (not (bigfp num))))
    (when (featurep 'bigfr)
      (Assert (not (bigfrp num))))
    (Assert (biggp num))
    (when (featurep 'bigc)
      (Assert (not (bigcp num))))))


;; this tests for `1.0+0.0i' being read and coerced to a bigc if provided
(when (featurep 'bigc)
  (let ((num 1.0+0.0i))
    (Assert (not (intp num)))
    (Assert (not (floatp num)))
    (Assert (not (integerp num)))
    (Assert (not (rationalp num)))
    (Assert (not (realp num)))
    (Assert (not (comparablep num)))
    (Assert (complexp num))
    (Assert (archimedeanp num))
    (Assert (numberp num))

    (when (featurep 'bigz)
      (Assert (not (bigzp num))))
    (when (featurep 'bigq)
      (Assert (not (bigqp num))))
    (when (featurep 'bigf)
      (Assert (not (bigfp num))))
    (when (featurep 'bigfr)
      (Assert (not (bigfrp num))))
    (when (featurep 'bigg)
      (Assert (not (biggp num))))
    (Assert (bigcp num))))


;;-----------------------------------------------------
;; Testing coercions
;;-----------------------------------------------------
(when (featurep 'bigz)
  (Assert (bigzp (coerce-number 0 'bigz)))
  (Assert (bigzp (coerce-number 1 'bigz)))
  (Assert (and (bigzp (factorial 100))
	       (bigzp (coerce-number (factorial 100) 'bigz))))
  (Assert (bigzp (coerce-number 1.0 'bigz)))
  (Assert (intp (coerce-number (factorial 100) 'int)))
  (Assert (zerop (coerce-number (factorial 100) 'int)))
  (Assert (let ((more-than-mpf (1+ most-positive-fixnum)))
	    (equal (coerce-number more-than-mpf 'float)
		   (1+ (coerce-number most-positive-fixnum 'float))))))

(when (featurep 'bigq)
  (Assert (bigqp (coerce-number 0 'bigq)))
  (Assert (bigqp (coerce-number 1 'bigq)))
  (Assert (and (bigqp 3/2)
	       (bigqp (coerce-number 3/2 'bigq))))
  (Assert (bigqp (coerce-number 1.5 'bigq)))
  (Assert (intp (coerce-number 3/2 'int)))
  (Assert (bigzp (coerce-number 3/2 'bigz)))
  (Assert (bigqp (// 2)))
  (Assert (bigqp (// 2 3)))
  (Assert (intp (// 4 2)))
  (when (featurep 'bigz)
    (Assert (bigzp (numerator 3/2)))
    (Assert (bigzp (denominator 3/2)))))

(when (and (featurep 'bigg)
	   (featurep 'bigc))
  (Assert (biggp (coerce-number 1.0+2.0i 'bigg)))
  (Assert (bigcp (coerce-number 1+2i 'bigc))))



;;-----------------------------------------------------
;; Testing auto-coercion in operations
;;-----------------------------------------------------
(when (featurep 'bigz)
  (let ((num1 2)
	(num2 2.0))
    ;; this test should reveal re-canonicalisation
    (eval `(Assert (intp (+ ,num1 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (intp (* ,num1 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (intp (- ,num1 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (intp (/ ,num1 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (intp (^ ,num1 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (intp (+ (coerce-number ,num1 'bigz) ,num1))))
    (eval `(Assert (intp (* (coerce-number ,num1 'bigz) ,num1))))
    (eval `(Assert (intp (- (coerce-number ,num1 'bigz) ,num1))))
    (eval `(Assert (intp (/ (coerce-number ,num1 'bigz) ,num1))))
    (eval `(Assert (intp (^ (coerce-number ,num1 'bigz) ,num1))))
    ;; floats and bigz should always result in a float
    (eval `(Assert (floatp (+ ,num2 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (floatp (* ,num2 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (floatp (- ,num2 (coerce-number ,num1 'bigz)))))
    (eval `(Assert (floatp (/ ,num2 (coerce-number ,num1 'bigz)))))
;;     (when (featurep 'bigfr)
;;       (eval `(Assert (bigfrp (^ ,num2 (coerce-number ,num1 'bigz))))))
    (eval `(Assert (floatp (+ (coerce-number ,num1 'bigz) ,num2))))
    (eval `(Assert (floatp (* (coerce-number ,num1 'bigz) ,num2))))
    (eval `(Assert (floatp (- (coerce-number ,num1 'bigz) ,num2))))
    (eval `(Assert (floatp (/ (coerce-number ,num1 'bigz) ,num2))))
    ))

;;-----------------------------------------------------
;; Testing selectors and constructors
;;-----------------------------------------------------
(when (featurep 'bigg)
  (let ((read-real-as 'bigfr)
	(default-real-precision 128))

    ;; testing bigg selector
    (Assert-Not-Equal (real-part (read "2+3i")) 2)
    (Assert-Not-Equal (imaginary-part (read "2+3i")) 3)
    (Assert-Not-Equal (real-part 2+3i) 2)
    (Assert-Not-Equal (imaginary-part 2+3i) 3)
    (Assert-Equal (real-part (read "2+3i")) (bigz 2))
    (Assert-Equal (imaginary-part (read "2+3i")) (bigz 3))
    (Assert-Equal (real-part 2+3i) (bigz 2))
    (Assert-Equal (imaginary-part 2+3i) (bigz 3))
    ;; use numerical equality
    (Assert (= (real-part (read "2+3i")) 2))
    (Assert (= (imaginary-part (read "2+3i")) 3))
    (Assert (= (real-part 2+3i) 2))
    (Assert (= (imaginary-part 2+3i) 3))
    (Assert (= (real-part (read "2+3i")) (bigz 2)))
    (Assert (= (imaginary-part (read "2+3i")) (bigz 3)))
    (Assert (= (real-part 2+3i) (bigz 2)))
    (Assert (= (imaginary-part 2+3i) (bigz 3)))

    ;; testing bigg constructor
    (Assert-Not-Equal (real-part (make-bigg 1 2)) 1)
    (Assert-Not-Equal (imaginary-part (make-bigg 1 2)) 2)
    (Assert-Equal (real-part (make-bigg 1 2)) (bigz 1))
    (Assert-Equal (imaginary-part (make-bigg 1 2)) (bigz 2))
    (Assert (= (real-part (make-bigg 1 2)) 1))
    (Assert (= (imaginary-part (make-bigg 1 2)) 2))

    ;; compare reader and constructor
    (Assert-Equal (make-bigg 1.0 2.0) (read "1+2i"))
    (Assert-Equal (make-bigg 1 2) (read "1+2i"))
    (Assert (and (= (real-part (make-bigg 1.0 2.0))
		    (real-part (read "1+2i")))
		 (= (imaginary-part (make-bigg 1.0 2.0))
		    (imaginary-part (read "1+2i")))))
    (Assert (and (= (real-part (make-bigg 1 2))
		    (real-part (read "1+2i")))
		 (= (imaginary-part (make-bigg 1 2))
		    (imaginary-part (read "1+2i")))))))

(when (featurep 'bigc)
  (let ((read-real-as 'bigfr)
	(default-real-precision 128))

    ;; testing bigc selector
    (Assert-Equal (real-part (read "2.3+3.2i"))
		   (read "2.3"))
    (Assert-Equal (imaginary-part (read "2.3+3.2i"))
		   (read "3.2"))
    ;; use numerical equality
    (Assert (= (real-part (read "2.3+3.2i"))
	       (read "2.3")))
    (Assert (= (imaginary-part (read "2.3+3.2i"))
	       (read "3.2")))

    ;; testing bigc constructor
    (Assert-Not-Equal (real-part (make-bigc 1 2)) 1)
    (Assert-Not-Equal (imaginary-part (make-bigc 1 2)) 2)
    (Assert-Equal (real-part (make-bigc 1 2)) (bigfr 1))
    (Assert-Equal (imaginary-part (make-bigc 1 2)) (bigfr 2))
    (Assert (= (real-part (make-bigc 1 2)) 1))
    (Assert (= (imaginary-part (make-bigc 1 2)) 2))

    ;; now compare reader and constructor
    (Assert-Equal (make-bigc 1.0 2.0) (read "1.0+2.0i"))
    (Assert-Equal (make-bigc 1 2) (read "1.0+2.0i"))
    (Assert (and (= (real-part (make-bigc 1.0 2.0))
		    (real-part (read "1.0+2.0i")))
		 (= (imaginary-part (make-bigc 1.0 2.0))
		    (imaginary-part (read "1.0+2.0i")))))
    (Assert (and (= (real-part (make-bigc 1 2))
		    (real-part (read "1.0+2.0i")))
		 (= (imaginary-part (make-bigc 1 2))
		    (imaginary-part (read "1.0+2.0i")))))))




;;-----------------------------------------------------
;; Testing formatting output
;;-----------------------------------------------------

(Assert-Equal (format "%d" 2) "2")
(Assert-Equal (format "%d" -2) "-2")
(Assert-Equal (format "%2.2E" -2) "-2.00E+00")

(Assert-Equal (format "%x" 100) "64")
(Assert-Equal (format "%#x" 100) "0x64")
(Assert-Equal (format "%X" 122) "7A")
(Assert-Equal (format "%.4X" 122) "007A")
(Assert-Equal (format "%4o" 100) " 144")
(Assert-Equal (format "%x" 10.58) "a")
(Assert-Equal (format "%o" 10.58) "12")
(Assert-Equal (format "%#o" 10.58) "0o12")

;; floats
(let ((forms
       '(1.0 1.00000 0.5 0.005 5.000005 4.0625 8.03125
	     9876.54321 10000.00001 12004.40021
	     1.5e+10 1.125e+11 1.0703125e+12
	     1.1e+15 1.2e+16 1.4e+20 1.45e+24
	     1.52e+28 1.55e+30 1.52105432e+31 1.5445633221e+32
	     1.7777777777777e+33 1.7777777777777776e+33
	     1.8999999999999e+33 1.999989999999999e+33
	     1.99999999e+35 1.9999999999e+36 1.999999999999e+37
	     1.99999999999999e+38 1.999999999999999e+39
	     1.9999999999999999e+40 2.000000000000000000e+40
	     2.000000000000001e+42 2.000000000000009e+44
	     2.002000200002000002000000e+48
	     2.000000200000200002000200e+50
	     200000020000020000200020.0e+50
	     12345555555555555555.999999999e+60
	     12344444444444444444.999999999e+60
	     1234545454545454545454545454545.000
	     123454545454545454545454545454545454545454545.000
	     4444444444444.55555555555e+100
	     5555555555555.55555555555e+102
	     5555555555555.44444444444e+104
	     5555555555555.99999999998e+106
	     50505050505050505050505.0e+200
	     1e+300 1e+301 1e+302 -1e+300 -1e+301 -1e+302
	     1e+304 -1e+304 1e+305 -1e+305 1e+306 -1e+306
	     1e+307 2e+307 8e+307 -8e+307
	     1e+308 -1e+308 8e+308 -8e+308
	     1e+309 -1e+309 -8e+309 8e+309
	     ;; we should be outta range of double floats
	     1.00e+310 2.50e+310 2.55e+310
	     2.125e+312 2.0004500045e+313 1.2e+314 1.2e+320
	     1.22229e+320 100e+320 101e+321 102e+322
	     ;; we're still alive?
	     most-positive-float most-negative-float
	     ;; the following two may not work correctly if the number
	     ;; distribution has many subnormal numbers
	     ;;(1+ most-positive-float) (1- most-negative-float)
	     ;;(1- most-positive-float) (1+ most-negative-float)
	     1.0e+340 1.0e+350 1.0e+380 1.0e+400
	     1.2e+300 1.2e+310 1.2e+320 1.2e+400
	     1.2e+2000 1.2e+3000 1.2e+4000 1.2e+5000))
       (failures
	;; known errors (due to precision issues, not SXE's fault)
	'(1e-300 1e-301 1e-302 1e-303 1e-304 1e-305 1e-306
		 1e-307 1e-308 1e-309
		 2.5e-310 2.55e-311 2.55e-312
		 1.2e-320 1.201e-320 1.25e-320
		 1.22229e-320 100e-306 100e-307 100e-308 100e-309
		 100e-310 100e-311 100e-312 100e-313 100e-314 100e-315
		 100e-316 100e-317 100e-318 100e-319 100e-320 100e-321
		 100e-322 100e-323 100e-324 100e-325 100e-326 100e-327
		 100e-328 100e-329 100e-330 1.2e-330 1.25e-330
		 0.5e-306 0.5e-307 0.5e-308 0.5e-309 0.5e-310 0.5e-311
		 0.5e-312 0.5e-313 0.5e-314 0.5e-315 0.5e-316 0.5e-317
		 0.5e-318 0.5e-319 0.5e-320 0.5e-321 0.5e-322 0.5e-323)))
  (mapc-internal
   #'(lambda (str)
       (unless (or (infinityp (eval str)) (zerop (eval str)))
	 (eval `(Assert (= (read (format "%f" ,str)) ,str)))
	 (eval `(Assert (eql (read (format "%f" ,str)) ,str)))))
   forms)
  (mapc-internal
   #'(lambda (str)
       (unless (or (infinityp (eval str)) (zerop (eval str)))
	 (eval `(Assert (not (= (read (format "%f" ,str)) ,str))))
	 (eval `(Assert (not (eql (read (format "%f" ,str)) ,str))))))
   failures))

;; now testing bigz formatting
(when (featurep 'bigz)
  (let ((forms
	 '((("%Z" 2) . "2")
	   (("%2Z" 2) . " 2")
	   (("%2Z" 200) . "200")
	   (("%+Z" 2) . "+2")
	   (("%+4Z" 2) . "  +2")
	   (("% Z" 2) . " 2")
	   (("%Z" -2) . "-2")
	   (("% Z" -2) . "-2")
	   (("%+Z" -2) . "-2")
	   (("%-4Z" 2) . "2   ")
	   (("%.2Z" 2) . "02")
	   (("%4.2Z" 2) . "  02")
	   (("%04.2Z" 2) . "  02")
	   (("%-4.2Z" 2) . "02  ")
	   (("%0-4.2Z" 2) . "02  ")
	   (("%Z" (factorial 20)) .
	    "2432902008176640000")
	   (("%40Z" (factorial 20)) .
	    "                     2432902008176640000")
	   (("%-40Z" (factorial 20)) .
	    "2432902008176640000                     ")
	   (("%.40Z" (factorial 20)) .
	    "0000000000000000000002432902008176640000")
	   (("%040Z" (factorial 20)) .
	    "0000000000000000000002432902008176640000")
	   (("%.8Z" (factorial 20)) .
	    "2432902008176640000")
	   (("%08Z" (factorial 20)) .
	    "2432902008176640000")
	   (("%24.8Z" (factorial 20)) .
	    "     2432902008176640000")
	   (("%36.28Z" (factorial 20)) .
	    "        0000000002432902008176640000")
	   (("%036.28d" (factorial 20)) .
	    "        0000000002432902008176640000")
	   (("%0-36.28d" (factorial 20)) .
	    "0000000002432902008176640000        ")

	   ;; now the same with the %d specifier
	   (("%d" 2) . "2")
	   (("%2d" 2) . " 2")
	   (("%2d" 200) . "200")
	   (("%+d" 2) . "+2")
	   (("%+4d" 2) . "  +2")
	   (("% d" 2) . " 2")
	   (("%d" -2) . "-2")
	   (("% d" -2) . "-2")
	   (("%+d" -2) . "-2")
	   (("%-4d" 2) . "2   ")
	   (("%.2d" 2) . "02")
	   (("%4.2d" 2) . "  02")
	   (("%04.2d" 2) . "  02")
	   (("%-4.2d" 2) . "02  ")
	   (("%0-4.2d" 2) . "02  ")
	   (("%d" (factorial 20)) . "2432902008176640000")
	   (("%40d" (factorial 20)) .
	    "                     2432902008176640000")
	   (("%-40d" (factorial 20)) .
	    "2432902008176640000                     ")
	   (("%.40d" (factorial 20)) .
	    "0000000000000000000002432902008176640000")
	   (("%040d" (factorial 20)) .
	    "0000000000000000000002432902008176640000")
	   (("%.8d" (factorial 20)) . "2432902008176640000")
	   (("%24.8d" (factorial 20)) . "     2432902008176640000")
	   (("%36.28d" (factorial 20)) .
	    "        0000000002432902008176640000")
	   (("%036.28d" (factorial 20)) .
	    "        0000000002432902008176640000")
	   (("%0-36.28d" (factorial 20)) .
	    "0000000002432902008176640000        ")

	   ;; testing base converters on big ints
	   ;; moved to format-tests
	   )))

    (mapc #'(lambda (f)
	      (let ((format (cons 'format (car f)))
		    (expected (cdr f)))
		(eval `(Assert (string= ,format ,expected)))))
	  forms)))


;; now testing bigq formatting
(when (featurep 'bigq)
  (let ((forms
	 '((("%Q" 2) . "2")
	   (("%2Q" 2) . " 2")
	   (("%2Q" 200) . "200")
	   (("%+Q" 2) . "+2")
	   (("% Q" 2) . " 2")
	   (("% +Q" 2) . "+2")
	   (("%+ Q" 2) . "+2")
	   (("%Q" -2) . "-2")
	   (("% Q" -2) . "-2")
	   (("%+Q" -2) . "-2")
	   (("% +Q" -2) . "-2")
	   (("%+ Q" -2) . "-2")
	   (("%-4Q" 2) . "2   ")
	   (("%.2Q" 2) . "2")
	   (("%4.2Q" 2) . "   2")
	   (("%-4.2Q" 2) . "2   ")

	   ;; testing with proper fractions
	   (("%Q" 2/3) . "2/3")
	   (("%5Q" 2/3) . "  2/3")
	   (("%5.5Q" 2/3) . "  2/3")
	   (("%+Q" 2/3) . "+2/3")
	   (("% Q" 2/3) . " 2/3")
	   (("% +Q" 2/3) . "+2/3")
	   (("%+ Q" 2/3) . "+2/3")
	   (("%Q" (float 1.5)) . "3/2")
	   (("%Q" (float 0.66666)) . "3002369727582815/4503599627370496")
	   (("%-10Q" 2/3) . "2/3       ")

	   ;; testing coercion to Z
	   (("%d" 4/3) . "1")
	   (("%Z" 2/3) . "0"))))

    (mapc #'(lambda (f)
	      (let ((format (cons 'format (car f)))
		    (expected (cdr f)))
		(eval `(Assert (string= ,format ,expected)))))
	  forms)))


(when (featurep 'bigfr)
  (let ((forms
	 '((("%f" (exp 1)) . "2.718282")
	   (("%2.2f" (exp 1)) . "2.72")
	   ;; this test uses a wrong output string deliberately
	   ;; it's wrong because the precision of IEEE-754 doubles is
	   ;; not enough to have 20 correct digits in the fractional part
	   ;; however, since we _can_ have long doubles now, this test
	   ;; is no longer feasible since lisp should know a shit about
	   ;; the internal representation of fpfloats. -hroptatyr
	   ;;(("%2.20f" (exp 1)) . "2.71828182845904509080")

	   ;; now testing with %F
	   (("%F" (exp 1)) . "2.718281828459045235360287471352662497759")
	   (("%2.2F" (exp 1)) . "2.71")
	   (("%+2.2F" (exp 1)) . "+2.71")
	   (("%10.0F" (exp 1)) . "         2")
	   (("%10.1F" (exp 1)) . "       2.7")
	   (("%12.12F" (exp 1)) . "2.718281828459")
	   (("%30.12F" (exp 1)) . "                2.718281828459")
	   (("%5.5F" (exp 13)) . "442413.39200")
	   (("%F" (/ (exp 1))) .
	    "0.3678794411714423215955237701614608674462")
	   (("%2.2F" (/ (exp 1))) . "0.36")
	   (("%3.3F" (/ (exp 1))) . "0.367")
	   (("%.5F" (bigfr 1)) . "1.00000")

	   (("%.4F" (bigfr 23213231 25)) . "23213231.0000")
	   ;; stupid assumption
	   ;;(("%.4F" (bigfr 23213231 8)) . "23200000.0000")
	   (("%Z" (bigfr 23213231 25)) . "23213231")
	   (("%Z" (bigfr 23213231 8)) . "23199744")

	   (("%+.4f" 2) . "+2.0000")
	   (("%+.4F" 2) . "+2.0000")
	   (("% .4F" 2) . " 2.0000")
	   (("%+10.4F" 2) . "   +2.0000")))
	(default-real-precision 128))

    (mapc #'(lambda (f)
	      (let ((format (cons 'format (car f)))
		    (expected (cdr f)))
		(eval `(Assert (string= ,format ,expected)))))
	  forms)))

(when (featurep 'bigg)
  (let ((forms
	 '((("%B" 2+i) . "2+1i")
	   (("%+B" 2+i) . "+2+1i")
	   (("% B" 2+i) . " 2+1i")
	   (("%B" 2+i) .
	    (format "%Z%+Zi" (real-part 2+i) (imaginary-part 2+i)))
	   (("%B" 1) . "1+0i")
	   (("%+10.4B" 1.2) . "     +0001     +0000i")
	   (("%-10.4B" 0+2i) . "0000      +0002     i"))))

    (mapc #'(lambda (f)
	      (let ((format (cons 'format (car f)))
		    (expected (cdr f)))
		(eval `(Assert (string= ,format ,expected)))))
	  forms))

  ;; Gaussian numbers shall not be coerced to comparables
  (Check-Error domain-error (format "%d" 1+i))
  (Check-Error domain-error (format "%f" 1+i))
  (when (featurep 'bigq)
    (Check-Error domain-error (format "%Q" 1+i)))
  (when (featurep 'bigfr)
    (Check-Error domain-error (format "%F" 1+i))))


(when (featurep 'bigc)
  (let ((forms
	 '((("%.2C" 2+i) . "2.00+1.00i")
	   (("%+.2C" 2+i) . "+2.00+1.00i")
	   (("% .2C" 2+i) . " 2.00+1.00i")
	   (("%.2C" 2+i) .
	    (format "%.2F%+.2Fi" (real-part 2+i) (imaginary-part 2+i)))
	   (("%.2C" 1) . "1.00+0.00i")
	   (("%+10.4C" 1.5) . "   +1.5000   +0.0000i")
	   (("%-10.4C" 0+2i) . "0.0000    +2.0000   i"))))

    (mapc #'(lambda (f)
	      (let ((format (cons 'format (car f)))
		    (expected (cdr f)))
		(eval `(Assert (string= ,format ,expected)))))
	  forms))

  ;; complex numbers shall not be coerced to comparables
  (Check-Error domain-error (format "%d" (sqrt -2)))
  (Check-Error domain-error (format "%f" (sqrt -2)))
  (when (featurep 'bigq)
    (Check-Error domain-error (format "%Q" (sqrt -2))))
  (when (featurep 'bigfr)
    (Check-Error domain-error (format "%F" (sqrt -2)))))


;;-----------------------------------------------------
;; Test arithmetic
;;-----------------------------------------------------
(when (featurep 'bigz)
  ;;; addition
  (let ((sums '((1 2 3)
		(12332112344321 10000000000000 22332112344321)
		(12332112344321 1 12332112344322)
		(1 12332112344321 12332112344322)
		(10101010101010 1010101010101 11111111111111)
		(-10101010101010 10101010101010 0)))
	(prods '((2 3 6)
		 (1002004002001 402010204 402815833253238418204)
		 (-1002004002001 402010204 -402815833253238418204)))
	(pows-!clslash
	 '((2 2 4)
	   (-4 4 256)
	   (-4 5 -1024)
	   (32 32 1461501637330902918203684832716283019655932542976)
	   (32 -32 0)))
	(pows-clslash
	 (when (featurep 'bigq)
	   '((2 2 4)
	     (-4 4 256)
	     (-4 5 -1024)
	     (32 32 1461501637330902918203684832716283019655932542976)
	     (32 -32 1/1461501637330902918203684832716283019655932542976)))))
    (mapc #'(lambda (sum)
	      (eval `(Assert (= (+ ,(car sum) ,(cadr sum)) ,(caddr sum))))
	      (eval `(Assert (= (- ,(caddr sum) ,(cadr sum)) ,(car sum))))
	      (unless (bigzp (caddr sum))
		(eval `(Assert (= (bigz (+ (bigz ,(car sum))
					   (bigz ,(cadr sum))))
				  (bigz ,(caddr sum)))))
		(eval `(Assert (= (bigz (- (bigz ,(caddr sum))
					   (bigz ,(cadr sum))))
				  (bigz ,(car sum)))))
		;; testing triangle inequality
		;; | a + b | <= |a| + |b|
		(eval `(Assert (<= (abs (+ ,(car sum) ,(cadr sum)))
				   (+ (abs ,(car sum)) (abs ,(cadr sum))))))))
	  sums)
    (mapc #'(lambda (prod)
	      (eval `(Assert (= (* ,(car prod) ,(cadr prod)) ,(caddr prod))))
	      (eval `(Assert (= (/ ,(caddr prod) ,(cadr prod)) ,(car prod))))
	      (unless (bigzp (caddr prod))
		(eval `(Assert (= (bigz (* (bigz ,(car prod))
					   (bigz ,(cadr prod))))
				  (bigz ,(caddr prod)))))
		(eval `(Assert (= (bigz (/ (bigz ,(caddr prod))
					   (bigz ,(cadr prod))))
				  (bigz ,(car prod)))))
		;; testing multiplicativiy of abs
		;; | a b | = |a| |b|
		(eval `(Assert (= (abs (* ,(car prod) ,(cadr prod)))
				  (* (abs ,(car prod)) (abs ,(cadr prod))))))))
	  prods)
    (let ((common-lisp-slash nil))
      (mapc #'(lambda (pow)
		(eval `(Assert (= (^ ,(car pow) ,(cadr pow)) ,(caddr pow))))
		(unless (bigzp (caddr pow))
		  (eval `(Assert (= (bigz (^ (bigz ,(car pow))
					     (bigz ,(cadr pow))))
				    (bigz ,(caddr pow)))))))
	    pows-!clslash))
    (let ((common-lisp-slash t))
      (mapc #'(lambda (pow)
		(eval `(Assert (= (^ ,(car pow) ,(cadr pow)) ,(caddr pow))))
		(unless (bigzp (caddr pow))
		  (eval `(Assert (= (bigz (^ (bigz ,(car pow))
					     (bigz ,(cadr pow))))
				    (bigz ,(caddr pow)))))))
	    pows-clslash)))

  ;; exponentiation overflows at some point
  ;; we use (factorial 400) which is a ~2887 bit number
  ;; should be large enough to never ever be a native integer (fixnum)
  ;; oh, in case someone already bought a 3072-bit processor,
  ;; please phone me
  (Check-Error range-error (^ (factorial 400) (factorial 400)))
  ;; unless we try a unit or a zero as base
  (Assert (= (^ 1 (factorial 400)) 1))
  (Assert (= (^ -1 (factorial 400)) 1))
  (Assert (= (^ -1 (1+ (factorial 400))) -1))
  (Assert (= (^ 0 (factorial 400)) 0))

  ;;; maxima and minima
  (let ((sets '(((1 2 3 -44) :max 3 :min -44)
		((1 1 1 1 1) :max 1 :min 1)
		((-100 -2000 -4000) :max -100 :min -4000)
		((+infinity 5000 -6000 -8000 -infinity)
		 :max +infinity :min -infinity))))
    (mapc #'(lambda (set)
	      (let ((max (plist-get (cdr set) :max))
		    (min (plist-get (cdr set) :min)))
		(eval `(Assert (= ,max (max ,@(car set)))))
		(eval `(Assert (= ,min (min ,@(car set)))))))
	  sets))

  ;;; % remainder
  ;; we cannot use eq for big integers
  ;; also, (mod (coerce -1 'bigz) 17) => 16 and not -1, therefore
  ;; the result differs by 17 when we have negative x
  (Assert (= 16 (% (coerce -1 'bigz) 17)))
  (dotimes (j 30)
    (let ((x (random))
	  (y (- (random))))
      (eval `(Assert (= ,x (+ (% ,x 17) (* (/ ,x 17) 17)))))
      (eval `(Assert (= (- ,x) (+ (% (- ,x) 17) (* (/ (- ,x) 17) 17)))))
      (let ((z (+ (% y 17) (* (/ y 17) 17))))
	(if (bigzp y)
	    (eval `(Assert (= ,y (- ,z 17))))
	  (eval `(Assert (= ,y z)))))
      ))

  ;;; remove-factor
  (mapc #'(lambda (i)
	    (dotimes (j 10)
	      (let* ((r (abs (random)))
		     (-r (- r))
		     (rf `(remove-factor ,i ,r))
		     (-rf `(remove-factor ,i ,-r))
		     (rf! (remove-factor i r))
		     (-rf! (remove-factor i -r)))
		;; first, test a positive number
		(eval `(Assert (consp ,rf)))
		(eval `(Assert (nonnegativep (cdr ,rf))))
		(eval `(Assert (or (< (car ,rf) ,r)
				   (zerop (cdr ,rf)))))
		;; then a negative number
		(eval `(Assert (consp ,-rf)))
		(eval `(Assert (nonnegativep (cdr ,-rf))))
		(eval `(Assert (or (< ,-r (car ,-rf))
				   (zerop (cdr ,-rf)))))
		;; then test if reduced number is coprime to factor
		(eval `(Assert (= (car (remove-factor ,i ,(car rf!)))
				  ,(car rf!))))
		(eval `(Assert (zerop (cdr (remove-factor ,i ,(car rf!))))))
		(eval `(Assert (= (car (remove-factor ,i ,(car -rf!)))
				  ,(car -rf!))))
		(eval `(Assert (zerop (cdr (remove-factor ,i ,(car -rf!)))))))))
	    '(2 3 4 10 20 50 100 200))

  ;; check the consistency of the result values
  (mapc #'(lambda (i)
	    (dotimes (j 20)
	      (let* ((r (random))
		     (rf `(remove-factor ,i ,r))
		     (rf! (remove-factor i r)))
		;; check if  car*factor^cdr  is the original number
		(eval `(Assert (= (* ,(car rf!) (^ ,i ,(cdr rf!))) ,r)))
		(if (primep i)
		    (eval `(Assert (coprimep ,i ,(car rf!))))))))
	'(-29 -19 -17 -13 -11 -7 -5 -3 -2 -1 0
	      1 2 3 5 7 11 13 17 19 29))
  ;; check coercion
  (mapc #'(lambda (i)
	    (dotimes (j 10)
	      ;; test real args
	      (let* ((r (sqrt (abs (random))))
		     (rf `(remove-factor ,i ,r))
		     (rf! (remove-factor i r)))
		(eval `(Assert (consp ,rf)))
		(eval `(Assert (nonnegativep (cdr ,rf))))
		(eval `(Assert (or (< (car ,rf) ,r)
				   (zerop (cdr ,rf))))))
	      ;; test quotient args
	      (let* ((r (// (random) (random)))
		     (rf `(remove-factor ,i ,r))
		     (rf! (remove-factor i r)))
		(eval `(Assert (consp ,rf)))
		(eval `(Assert (nonnegativep (cdr ,rf))))
		(eval `(Assert (or (/= (car ,rf) ,r)
				   (zerop (cdr ,rf))))))))
	'(-29 -29/3 -19 -19/2 -17 -17.25 -13 -13.2
	      -11 -11/4 -11.7 -7/3
	      7/3 11 11/3 13 13.4 17 17/2 19 19.25 29 29.3))

  ;;; test primep, coprimep, next prime, etc.
  (mapc #'(lambda (i)
	    (eval `(Assert (primep ,i)))
	    (dotimes (j 100)
	      (let ((r (car (remove-factor i (random)))))
		(eval `(Assert (coprimep ,i ,r)))))
	    (eval `(Assert (< ,i (next-prime ,i))))
	    (eval `(Assert (primep ,(next-prime i))))
	    (eval `(Assert (coprimep ,i ,(next-prime i)))))
	'(-521 -101 -61 -29 2 3 5 7 11 13 17 19 29 101))
  ;; test some Mersenne primes (this may take some time)
  (mapc #'(lambda (i)
	    (let ((Mi (1- (2^ i))))
	      (eval `(Assert (primep ,Mi)))
	      (eval `(Assert (oddp ,Mi)))))
	'(2 3 5 7 13 17 19 31 61 89 107 127 521 607))

  ;;; test factorial
  (mapc #'(lambda (i)
	    (let* ((r 1)
		   (r (loop for j from 2 to i
			do
			(setq r (* r j))
			finally return r))
		   (rf `(factorial ,i))
		   (rf-1 `(factorial ,(1- i))))
		;; check if  (factorial i) == 1*2*...*i
		(eval `(Assert (= ,r ,rf)))
		(eval `(Assert (evenp ,rf)))
		(eval `(Assert (not (primep ,rf))))
		(eval `(Assert (= (car (remove-factor ,rf-1 ,rf)) ,i)))
		(eval `(Assert (= (cdr (remove-factor ,rf-1 ,rf)) 1)))))
	'(3 4 5 6 7 8 9 10 11 20 30))
  ;; further tests with inductive Assert
  (mapc #'(lambda (i)
	    (let* ((rf `(factorial ,i))
		   (rf-1 `(factorial ,(1- i))))
		;; check if  (factorial i) == 1*2*...*i
		(eval `(Assert (= ,rf (* ,i ,rf-1))))
		(if (featurep 'mpfr)
		    (eval `(Assert (> (log ,rf) (- (* ,i (log ,i)) ,i))))
		  (eval `(Assert (or (> (log ,rf) (- (* ,i (log ,i)) ,i))
				     (eq +infinity (log ,rf))))))))
	'(60 100 120 150 200 300 500 1000))
  (mapc #'(lambda (i)
	    (eval `(Check-Error wrong-type-argument (factorial ,i))))
	'(-1 -2 3/2 -3/2 1.5 -10.5 10.0))

  ;; test congruency and divisibility
  (let ((divis
	 '((16 . 4) (16 . 2)
	   (17 . 1) (17 . 17)
	   (22 . 2) (22 . 11)
	   (39 . 3) (39 . 13)))
	(ndivis
	 '((16 . 5) (16 . 3)
	   (17 . 2) (17 . 3) (17 . 4) (17 . 5) (17 . 7) (17 . 11)
	   (22 . 3) (22 . 13) (22 . 21) (22 . 23)
	   (39 . 17)))
	(cong
	 '((5 (16 . 1) (16 . 11) (17 . 2) (-17 . 3) (5 . 0))
	   (7 (16 . 2) (16 . 51) (51 . 16) (2 . 16) (-1 . 6))
	   (16 (4 . 20) (32 . 0) (-32 . 16) (16 . -32))))
	(ncong
	 '((5 (16 . -1) (16 . 2) (17 . 16) (16 . 17) (2 . -2))
	   (21 (7 . 21) (21 . 7) (3 . 23) (-3 . 19)))))
    ;; divisibility
    (mapc #'(lambda (val)
	      (eval `(Assert (divisiblep ,(car val) ,(cdr val)))))
	  divis)
    (mapc #'(lambda (val)
	      (eval `(Assert (not (divisiblep ,(car val) ,(cdr val))))))
	  ndivis)
    ;; congruency
    (mapc #'(lambda (val)
	      (let ((module (car val))
		    (congs (cdr val)))
		(mapc #'(lambda (cong)
			  (eval `(Assert
				  (congruentp ,(car cong) ,(cdr cong)
					      ,module))))
		      congs)))
	  cong)
    (mapc #'(lambda (val)
	      (let ((module (car val))
		    (congs (cdr val)))
		(mapc #'(lambda (cong)
			  (eval `(Assert
				  (not (congruentp ,(car cong) ,(cdr cong)
						   ,module)))))
		      congs)))
	  ncong)))

(when (featurep 'bigq)
  ;;; addition
  (let ((sums '((1/2 2/3 7/6)
		(1233211/2344321 10000/125897 25528682181/42163282991)
		(12332112344321/2 1 12332112344323/2)
		(1/3 12332112344321 36996337032964/3)
		(10101/10101 101589/101589 2/1)
		(-100/99 -50/51 -3350/1683)))
	(prods '((2/3 3/4 1/2)
		 (1002004/2001 5/2 2505010/2001)
		 (-1002004/2001 5/2 -2505010/2001)))
	(pows '((2/3 2 4/9)
		(-4/10 4 256/10000)
		(7/3 -16 43046721/33232930569601))))
    (mapc #'(lambda (sum)
	      (eval `(Assert (= (+ ,(car sum) ,(cadr sum)) ,(caddr sum))))
	      (eval `(Assert (= (- ,(caddr sum) ,(cadr sum)) ,(car sum))))
	      (unless (bigqp (caddr sum))
		(eval `(Assert (= (bigq (+ (bigq ,(car sum))
					   (bigq ,(cadr sum))))
				  (bigq ,(caddr sum)))))
		(eval `(Assert (= (bigq (- (bigq ,(caddr sum))
					   (bigq ,(cadr sum))))
				  (bigq ,(car sum)))))
		;; testing triangle inequality
		;; | a + b | <= |a| + |b|
		(eval `(Assert (<= (abs (+ ,(car sum) ,(cadr sum)))
				   (+ (abs ,(car sum)) (abs ,(cadr sum))))))))
	  sums)
    (mapc #'(lambda (prod)
	      (eval `(Assert (= (* ,(car prod) ,(cadr prod)) ,(caddr prod))))
	      (eval `(Assert (= (/ ,(caddr prod) ,(cadr prod)) ,(car prod))))
	      (unless (bigqp (caddr prod))
		(eval `(Assert (= (bigq (* (bigq ,(car prod))
					   (bigq ,(cadr prod))))
				  (bigq ,(caddr prod)))))
		(eval `(Assert (= (bigq (/ (bigq ,(caddr prod))
					   (bigq ,(cadr prod))))
				  (bigq ,(car prod)))))
		;; testing multiplicativiy of abs
		;; | a b | = |a| |b|
		(eval `(Assert (= (abs (* ,(car prod) ,(cadr prod)))
				  (* (abs ,(car prod)) (abs ,(cadr prod))))))))
	  prods)
    (mapc #'(lambda (pow)
	      (eval `(Assert (= (^ ,(car pow) ,(cadr pow)) ,(caddr pow))))
	      (eval `(Assert (= (bigq (^ (bigq ,(car pow))
					 ,(cadr pow)))
				(bigq ,(caddr pow))))))
	  pows)))


;; ceil/floor stuff
(let ((one-arg-floor-list `((0 0)
			    (1 1)
			    (-1 -1)
			    (7.4 7)
			    (-7.4 -8))))
  (when (featurep 'bigz)
    (setq one-arg-floor-list
	  (append one-arg-floor-list
		  `((,(factorial 20) ,(factorial 20))
		    (,(- (factorial 20)) ,(- (factorial 20)))))))
  (when (featurep 'bigq)
    (setq one-arg-floor-list
	  (append one-arg-floor-list
		  `((1/2 0)
		    (-1/2 -1)
		    (40/3 13)
		    (-40/3 -14)))))
  (when (featurep 'bigf)
    (setq one-arg-floor-list
	  (append one-arg-floor-list
		  `((,(bigf 7.4) 7)
		    (,(bigf -7.4) -8)))))
  (when (featurep 'bigfr)
    (setq one-arg-floor-list
	  (append one-arg-floor-list
		  `((,(bigfr 7.4) 7)
		    (,(bigfr -7.4) -8)
		    (,(sqrt 2) 1)
		    (,(log 2) 0)
		    (,(log 0.1) -3)))))
  (mapc #'(lambda (arg-list)
	    (eval `(Assert (= (floor ,(car arg-list))
			      ,(cadr arg-list)))))
	one-arg-floor-list))

(let ((two-arg-floor-list `((0 1 0)
			    (1 2 0)
			    (-1 2 -1)
			    (7.4 2 3)
			    (-7.4 2 -4))))
  (when (featurep 'bigz)
    (setq two-arg-floor-list
	  (append two-arg-floor-list
		  `((,(factorial 20) 100001 24328776793998)
		    (,(- (factorial 20)) 100001 -24328776793999)))))
  (when (featurep 'bigq)
    (setq two-arg-floor-list
	  (append two-arg-floor-list
		  `((1/2 2 0)
		    (1/2 1/2 1)
		    (2 -1/2 -4)
		    (3/2 -1/3 -5)
		    (40/3 1/5 66)
		    (40/3 -1/5 -67)))))
  (when (featurep 'bigf)
    (setq two-arg-floor-list
	  (append two-arg-floor-list
		  `((,(bigf 1) 2 0)
		    (2 ,(bigf 0.5) 4)
		    (,(bigf 3880.5) 2 1940)
		    (,(bigf -3880.5) 2 -1941)))))
  (when (featurep 'bigfr)
    (setq two-arg-floor-list
	  (append two-arg-floor-list
		  `((,(bigfr 1) 2 0)
		    (2 ,(bigfr 0.5) 4)
		    (,(sqrt 12) 2 1)
		    (1 (log 1.2) 5)
		    (,(exp 37) 37 316733577643313)))))

  (mapc #'(lambda (arg-list)
	    (eval `(Assert (= (floor ,(car arg-list) ,(cadr arg-list))
			      ,(caddr arg-list)))))
	two-arg-floor-list))


;;-----------------------------------------------------
;; Testing relations
;;-----------------------------------------------------
(when (featurep 'ent)
  (let ((ones)
	(twos))
    (and (featurep 'bigz)
	 (add-to-list 'ones (coerce 1 'bigz))
	 (add-to-list 'twos (coerce 2 'bigz)))
    (and (featurep 'bigq)
	 (add-to-list 'ones 101/100)
	 (add-to-list 'twos 202/100))
    (and (featurep 'bigf)
	 (add-to-list 'ones (coerce 1.01 'bigf))
	 (add-to-list 'twos (coerce 2.02 'bigf)))
    (and (featurep 'bigfr)
	 (add-to-list 'ones (coerce 1.01 'bigfr))
	 (add-to-list 'twos (coerce 2.02 'bigfr)))
    (dolist (one ones)
      (dolist (two twos)
	(eval `(Assert (< ,one ,two)))
	(eval `(Assert (<= ,one ,two)))
	(eval `(Assert (<= ,two ,two)))
	(eval `(Assert (>  ,two ,one)))
	(eval `(Assert (>= ,two ,one)))
	(eval `(Assert (>= ,two ,two)))
	(eval `(Assert (/= ,one ,two)))
	(eval `(Assert (not (/= ,two ,two))))
	(eval `(Assert (not (< ,one ,one))))
	(eval `(Assert (not (> ,one ,one))))
	(eval `(Assert (<= ,one ,one ,two ,two)))
	(eval `(Assert (not (< ,one ,one ,two ,two))))
	(eval `(Assert (>= ,two ,two ,one ,one)))
	(eval `(Assert (not (> ,two ,two ,one ,one))))
	(eval `(Assert (= ,one ,one ,one)))
	(eval `(Assert (not (= ,one ,one ,one ,two))))
	(eval `(Assert (not (/= ,one ,two ,one))))
	))
    (when (featurep 'bigc)
      ;; now check complexes, these are not comparable
      (dolist (one ones)
	(eval `(Check-Error relation-error (< ,one 1+i)))
	(eval `(Check-Error relation-error (<= ,one 1+i)))
	(eval `(Check-Error relation-error (<= 1+i 1+i)))
	(eval `(Check-Error relation-error (> ,one 1+i)))
	(eval `(Check-Error relation-error (>= ,one 1+i)))
	(eval `(Check-Error relation-error (>= 1+i 1+i)))
	(eval `(Check-Error relation-error (not (/= ,one 1+i))))
	(eval `(Check-Error relation-error (= ,one 1+i)))
	))))

;;-----------------------------------------------------
;; Testing infinities
;;-----------------------------------------------------
(Assert (boundp '+infinity))
(Assert (boundp '-infinity))
(Assert (boundp 'complex-infinity))
(Assert (boundp 'not-a-number))

(Assert (infinityp +infinity))
(Assert (infinityp -infinity))
(Assert (infinityp complex-infinity))
(Assert (indefinitep +infinity))
(Assert (indefinitep -infinity))
(Assert (indefinitep complex-infinity))
(Assert (indefinitep not-a-number))

;;; testing arithmetics with infinity symbols
(let* ((ASSERT-EQUAL
	#'(lambda (form result)
	    (eval `(Assert-Equal ,form ,result))))
       (ASSERT-=
	#'(lambda (form result)
	    (eval `(Assert (= ,form ,result)))))
       (ASSERT-EQUAL-nc
	#'(lambda (form result)
	    (eval `(Check-Error wrong-type-argument (equal ,form ,result)))))
       (ASSERT-=-nc
	#'(lambda (form result)
	    (eval `(Check-Error relation-error (= ,form ,result)))))
       (ASSERT-EQUAL+=
	#'(lambda (form result)
	    (funcall ASSERT-EQUAL form result)
	    (funcall ASSERT-= form result)))
       (ASSERT-EQUAL+=-nc
	#'(lambda (form result)
	    (funcall ASSERT-EQUAL form result)
	    (funcall ASSERT-=-nc form result)))
       (ASSERT-EQUAL-nc+=-nc
	#'(lambda (form result)
	    (funcall ASSERT-EQUAL-nc form result)
	    (funcall ASSERT-=-nc form result))))

  ;; addition
  (funcall ASSERT-EQUAL+= '(+ 0 +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ 1 +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ -1 +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ +infinity 0) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ +infinity 1) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ +infinity -1) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(1+ +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(+ +infinity +infinity) '+infinity)
  (funcall ASSERT-EQUAL+=-nc '(+ complex-infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ -infinity +infinity) 'not-a-number)

  (funcall ASSERT-EQUAL+= '(+ 0 -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(+ 1 -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(+ -1 -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(+ -infinity 0) '-infinity)
  (funcall ASSERT-EQUAL+= '(+ -infinity 1) '-infinity)
  (funcall ASSERT-EQUAL+= '(+ -infinity -1) '-infinity)
  (funcall ASSERT-EQUAL+=-nc '(+ -infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(+ -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(1+ -infinity) '-infinity)
  (funcall ASSERT-EQUAL+=-nc '(+ +infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(+ -infinity -infinity) '-infinity)
  (funcall ASSERT-EQUAL+=-nc '(+ complex-infinity -infinity) 'not-a-number)

  (funcall ASSERT-EQUAL+= '(+ 0 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(+ 1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(+ -1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(+ complex-infinity 0) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(+ complex-infinity 1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(+ complex-infinity -1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(+ complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(1+ complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=-nc '(+ +infinity complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ -infinity complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=
	   '(+ complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(+ 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(1+ not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(+ -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(+ not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(+ complex-infinity not-a-number) 'not-a-number)

  ;; subtraction
  (funcall ASSERT-EQUAL+= '(- 0 +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(- 1 +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(- -1 +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(- +infinity 0) '+infinity)
  (funcall ASSERT-EQUAL+= '(- +infinity 1) '+infinity)
  (funcall ASSERT-EQUAL+= '(- +infinity -1) '+infinity)
  (funcall ASSERT-EQUAL+= '(- +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(1- +infinity) '+infinity)
  (funcall ASSERT-EQUAL+=-nc '(- +infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(- -infinity +infinity) '-infinity)
  (funcall ASSERT-EQUAL+=-nc '(- complex-infinity +infinity) 'not-a-number)

  (funcall ASSERT-EQUAL+= '(- 0 -infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(- 1 -infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(- -1 -infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(- -infinity 0) '-infinity)
  (funcall ASSERT-EQUAL+= '(- -infinity 1) '-infinity)
  (funcall ASSERT-EQUAL+= '(- -infinity -1) '-infinity)
  (funcall ASSERT-EQUAL+= '(- -infinity +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(- -infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(1- -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(- +infinity -infinity) '+infinity)
  (funcall ASSERT-EQUAL+=-nc '(- -infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- complex-infinity -infinity) 'not-a-number)

  (funcall ASSERT-EQUAL+= '(- 0 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(- 1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(- -1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(- complex-infinity 0) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(- complex-infinity 1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(- complex-infinity -1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(- complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(1- complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=-nc '(- +infinity complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- -infinity complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=
	   '(- complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(- 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(1- not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(- -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(- not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(- complex-infinity not-a-number) 'not-a-number)

  ;; multiplication
  (funcall ASSERT-EQUAL+=-nc '(* 0 +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(* 1 +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(* -1 +infinity) '-infinity)
  (funcall ASSERT-EQUAL+=-nc '(* +infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(* +infinity 1) '+infinity)
  (funcall ASSERT-EQUAL+= '(* +infinity -1) '-infinity)
  (funcall ASSERT-EQUAL+= '(* +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(* +infinity +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(* -infinity +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(* complex-infinity +infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(* 0 -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(* 1 -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(* -1 -infinity) '+infinity)
  (funcall ASSERT-EQUAL+=-nc '(* -infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(* -infinity 1) '-infinity)
  (funcall ASSERT-EQUAL+= '(* -infinity -1) '+infinity)
  (funcall ASSERT-EQUAL+= '(* -infinity +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(* -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(* +infinity -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(* -infinity -infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(* complex-infinity -infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(* 0 complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(* 1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(* -1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=-nc '(* complex-infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(* complex-infinity 1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(* complex-infinity -1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(* complex-infinity +infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(* complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(* +infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(* -infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=
	   '(* complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(* 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(* -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(* not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(* complex-infinity not-a-number) 'not-a-number)

  ;; division
  (funcall ASSERT-EQUAL+= '(/ 0 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(/ 1 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(/ -1 +infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(/ +infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(/ +infinity 1) '+infinity)
  (funcall ASSERT-EQUAL+= '(/ +infinity -1) '-infinity)
  (funcall ASSERT-EQUAL+= '(/ +infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(/ +infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ -infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(/ complex-infinity +infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(/ 0 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(/ 1 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(/ -1 -infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(/ -infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(/ -infinity 1) '-infinity)
  (funcall ASSERT-EQUAL+= '(/ -infinity -1) '+infinity)
  (funcall ASSERT-EQUAL+=-nc '(/ -infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(/ -infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(/ +infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ -infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(/ complex-infinity -infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(/ 0 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ 1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ -1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=-nc '(/ complex-infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(/ complex-infinity 1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ complex-infinity -1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ complex-infinity +infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ +infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(/ -infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=
	   '(/ complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(/ 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(/ -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(/ not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(/ complex-infinity not-a-number) 'not-a-number)

  ;; division part 2
  (funcall ASSERT-EQUAL+= '(// 0 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(// 1 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(// -1 +infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(// +infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(// +infinity 1) '+infinity)
  (funcall ASSERT-EQUAL+= '(// +infinity -1) '-infinity)
  (funcall ASSERT-EQUAL+= '(// +infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(// +infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// -infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(// complex-infinity +infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(// 0 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(// 1 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(// -1 -infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(// -infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(// -infinity 1) '-infinity)
  (funcall ASSERT-EQUAL+= '(// -infinity -1) '+infinity)
  (funcall ASSERT-EQUAL+=-nc '(// -infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(// -infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(// +infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// -infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(// complex-infinity -infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(// 0 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// 1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// -1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=-nc '(// complex-infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(// complex-infinity 1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// complex-infinity -1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// complex-infinity +infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// +infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(// -infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=
	   '(// complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(// 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(// -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(// not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(// complex-infinity not-a-number) 'not-a-number)

  ;; reduction modulo number
  (funcall ASSERT-EQUAL+= '(% 0 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(% 1 +infinity) 1)
  (funcall ASSERT-EQUAL+= '(% -1 +infinity) -1)
  (funcall ASSERT-EQUAL+=-nc '(% +infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% +infinity 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% +infinity -1) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(% +infinity +infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(% -infinity +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(% complex-infinity +infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(% 0 -infinity) '0)
  (funcall ASSERT-EQUAL+= '(% 1 -infinity) '1)
  (funcall ASSERT-EQUAL+= '(% -1 -infinity) '-1)
  (funcall ASSERT-EQUAL+=-nc '(% -infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% -infinity 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% -infinity -1) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(% -infinity +infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(% +infinity -infinity) '-infinity)
  (funcall ASSERT-EQUAL+= '(% -infinity -infinity) '+infinity)
  (funcall ASSERT-EQUAL+= '(% complex-infinity -infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(% 0 complex-infinity) '0)
  (funcall ASSERT-EQUAL+= '(% 1 complex-infinity) '1)
  (funcall ASSERT-EQUAL+= '(% -1 complex-infinity) '-1)
  (funcall ASSERT-EQUAL+=-nc '(% complex-infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% complex-infinity 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% complex-infinity -1) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(% complex-infinity +infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(% +infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(% -infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=
	   '(% complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(% 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(% -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(% not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(% complex-infinity not-a-number) 'not-a-number)

  ;; exponentiation
  (funcall ASSERT-EQUAL+= '(^ 0 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(^ 1 +infinity) 1)
  (funcall ASSERT-EQUAL+= '(^ 2 +infinity) +infinity)
  (funcall ASSERT-EQUAL+= '(2^ +infinity) +infinity)
  (funcall ASSERT-EQUAL+= '(^ 10 +infinity) +infinity)
  (funcall ASSERT-EQUAL+= '(10^ +infinity) +infinity)
  (funcall ASSERT-EQUAL+=-nc '(^ -1 +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ +infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(^ +infinity 1) '+infinity)
  (funcall ASSERT-EQUAL+= '(^ +infinity 2) '+infinity)
  (funcall ASSERT-EQUAL+= '(^ +infinity -1) 0)
  (funcall ASSERT-EQUAL+= '(^-1 +infinity) 0)
  (funcall ASSERT-EQUAL+= '(^ +infinity -2) 0)
  (funcall ASSERT-EQUAL+=-nc '(^ +infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ -infinity +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(^ complex-infinity +infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(^ 0 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(^ 1 -infinity) 1)
  (funcall ASSERT-EQUAL+= '(^ 2 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(2^ -infinity) 0)
  (funcall ASSERT-EQUAL+= '(^ 10 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(10^ -infinity) 0)
  (funcall ASSERT-EQUAL+=-nc '(^ -1 -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ -infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(^ -infinity 1) '-infinity)
  (funcall ASSERT-EQUAL+= '(^ -infinity 2) '+infinity)
  (funcall ASSERT-EQUAL+= '(^ -infinity -1) 0)
  (funcall ASSERT-EQUAL+= '(^-1 -infinity) 0)
  (funcall ASSERT-EQUAL+= '(^ -infinity -2) 0)
  (funcall ASSERT-EQUAL+=-nc '(^ +infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ -infinity -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(^ complex-infinity -infinity) 'complex-infinity)

  (funcall ASSERT-EQUAL+= '(^ 0 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(^ 1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(^ -1 complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=-nc '(^ complex-infinity 0) 'not-a-number)
  (funcall ASSERT-EQUAL+= '(^ complex-infinity 1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(^ complex-infinity -1) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(^ complex-infinity +infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(^ +infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+= '(^ -infinity complex-infinity) 'complex-infinity)
  (funcall ASSERT-EQUAL+=
	   '(^ complex-infinity complex-infinity) complex-infinity)

  (funcall ASSERT-EQUAL+=-nc '(^ 0 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ 1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ -1 not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ not-a-number 0) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ not-a-number 1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ not-a-number -1) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ not-a-number +infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ +infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ not-a-number -infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc '(^ -infinity not-a-number) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(^ not-a-number complex-infinity) 'not-a-number)
  (funcall ASSERT-EQUAL+=-nc
	   '(^ complex-infinity not-a-number) 'not-a-number)
  )

;; testing order of numbers and infinities
(Assert (/= -1 +infinity))
(Assert (not (= -1 +infinity)))
(Assert (< -1 +infinity))
(Assert (not (> -1 +infinity)))
(Assert (<= -1 +infinity))
(Assert (not (>= -1 +infinity)))
(Assert (/= +infinity -1))
(Assert (not (= +infinity -1)))
(Assert (not (< +infinity -1)))
(Assert (> +infinity -1))
(Assert (not (<= +infinity -1)))
(Assert (>= +infinity -1))
(Assert (< +infinity +infinity))
(Assert (> +infinity +infinity))
(Assert (= +infinity +infinity))
(Assert (<= +infinity +infinity))
(Assert (>= +infinity +infinity))

(Assert (/= -1 -infinity))
(Assert (not (= -1 -infinity)))
(Assert (not (< -1 -infinity)))
(Assert (> -1 -infinity))
(Assert (not (<= -1 -infinity)))
(Assert (>= -1 -infinity))
(Assert (/= -infinity -1))
(Assert (not (= -infinity -1)))
(Assert (< -infinity -1))
(Assert (not (> -infinity -1)))
(Assert (<= -infinity -1))
(Assert (not (>= -infinity -1)))
(Assert (< -infinity -infinity))
(Assert (> -infinity -infinity))
(Assert (= -infinity -infinity))
(Assert (<= -infinity -infinity))
(Assert (>= -infinity -infinity))

(Assert (/= -infinity +infinity))
(Assert (not (= -infinity +infinity)))
(Assert (< -infinity -1 +infinity))
(Assert (not (> -infinity -1 +infinity)))
(Assert (not (> -1 -infinity +infinity)))
(Assert (<= -infinity -1 +infinity))
(Assert (not (>= -infinity -1 +infinity)))
(Assert (not (< +infinity -1 -infinity)))
(Assert (> +infinity -1 -infinity))
(Assert (not (<= +infinity -1 -infinity)))
(Assert (>= +infinity -1 -infinity))
(Assert (< -infinity -infinity -2 -1 0 +infinity +infinity))
(Assert (> +infinity +infinity 2 1 0 -infinity -infinity))
(Assert (<= -infinity -infinity -2 -1 0 +infinity +infinity))
(Assert (>= +infinity +infinity 2 1 0 -infinity -infinity))

(Check-Error relation-error (< 0 complex-infinity))
(Check-Error relation-error (= 0 complex-infinity))
(Check-Error relation-error (/= 0 complex-infinity))
(Check-Error relation-error (> 0 complex-infinity))
(Check-Error relation-error (<= 0 complex-infinity))
(Check-Error relation-error (>= 0 complex-infinity))
(Check-Error relation-error (< complex-infinity 0))
(Check-Error relation-error (= complex-infinity 0))
(Check-Error relation-error (/= complex-infinity 0))
(Check-Error relation-error (> complex-infinity 0))
(Check-Error relation-error (<= complex-infinity 0))
(Check-Error relation-error (>= complex-infinity 0))

(Check-Error relation-error (< 0 not-a-number))
(Check-Error relation-error (< +infinity not-a-number))
(Check-Error relation-error (< not-a-number not-a-number))
(Check-Error relation-error (< complex-infinity not-a-number))
(Check-Error relation-error (= 0 not-a-number))
(Check-Error relation-error (= +infinity not-a-number))
(Check-Error relation-error (= complex-infinity not-a-number))
(Check-Error relation-error (= not-a-number not-a-number))
(Check-Error relation-error (/= 0 not-a-number))
(Check-Error relation-error (/= +infinity not-a-number))
(Check-Error relation-error (/= complex-infinity not-a-number))
(Check-Error relation-error (/= not-a-number not-a-number))
(Check-Error relation-error (> 0 not-a-number))
(Check-Error relation-error (> +infinity not-a-number))
(Check-Error relation-error (> complex-infinity not-a-number))
(Check-Error relation-error (> not-a-number not-a-number))
(Check-Error relation-error (<= 0 not-a-number))
(Check-Error relation-error (<= +infinity not-a-number))
(Check-Error relation-error (<= complex-infinity not-a-number))
(Check-Error relation-error (<= not-a-number not-a-number))
(Check-Error relation-error (>= 0 not-a-number))
(Check-Error relation-error (>= +infinity not-a-number))
(Check-Error relation-error (>= complex-infinity not-a-number))
(Check-Error relation-error (>= not-a-number not-a-number))
(Check-Error relation-error (< not-a-number 0))
(Check-Error relation-error (< not-a-number +infinity))
(Check-Error relation-error (< not-a-number complex-infinity))
(Check-Error relation-error (< not-a-number 0 not-a-number))
(Check-Error relation-error (= not-a-number 0))
(Check-Error relation-error (= not-a-number +infinity))
(Check-Error relation-error (= not-a-number complex-infinity))
(Check-Error relation-error (= not-a-number 0 not-a-number))
(Check-Error relation-error (/= not-a-number 0))
(Check-Error relation-error (/= not-a-number +infinity))
(Check-Error relation-error (/= not-a-number complex-infinity))
(Check-Error relation-error (/= not-a-number 0 not-a-number))
(Check-Error relation-error (> not-a-number 0))
(Check-Error relation-error (> not-a-number +infinity))
(Check-Error relation-error (> not-a-number complex-infinity))
(Check-Error relation-error (> not-a-number 0 not-a-number))
(Check-Error relation-error (<= not-a-number 0))
(Check-Error relation-error (<= not-a-number +infinity))
(Check-Error relation-error (<= not-a-number complex-infinity))
(Check-Error relation-error (<= not-a-number 0 not-a-number))
(Check-Error relation-error (>= not-a-number 0))
(Check-Error relation-error (>= not-a-number +infinity))
(Check-Error relation-error (>= not-a-number complex-infinity))
(Check-Error relation-error (>= not-a-number 0 not-a-number))


;; testing predicates on infinities
(let ((npreds '(zerop intp bigzp integerp bigqp rationalp floatp
		      bigfp bigfrp realp biggp bigcp
		      consp stringp arrayp evenp oddp primep))
      (comp-inf-preds '(comparablep))
      (inf-preds '(atom numberp infinityp archimedeanp))
      ;; values
      (nvals '(+infinity -infinity complex-infinity not-a-number))
      (comp-inf-vals '(+infinity -infinity))
      (inf-vals '(+infinity -infinity complex-infinity)))
  (mapc #'(lambda (pred)
	    (and (fboundp pred)
		 (mapc #'(lambda (val)
			   (eval `(Assert (not (,pred ,val)))))
		       nvals)))
	npreds)
  (mapc #'(lambda (pred)
	    (mapc #'(lambda (val)
		      (eval `(Assert (,pred ,val))))
		  comp-inf-vals))
	comp-inf-preds)
  (mapc #'(lambda (pred)
	    (mapc #'(lambda (val)
		      (eval `(Assert (,pred ,val))))
		  inf-vals))
	inf-preds))

;; testing lifts
(let ((lift-types
       (remove-if-not
	#'(lambda (type)
	    (condition-case nil
		(coerce-number 0 type)
	      (error nil)))
	'(int bigz integer bigq rational bigf bigfr float real
	      bigc quatern))))
  (mapc-internal
   #'(lambda (type)
       (eval `(Assert (zerop (coerce-number 0 ',type))))
       (eval `(Assert (onep (coerce-number 1 ',type))))
       (eval `(Assert (zerop (coerce-number 0.0 ',type))))
       (eval `(Assert (onep (coerce-number 1.0 ',type))))

       ;; lifts are idempotent
       (eval `(Assert-Equal
		       (coerce-number 0 ',type)
		       (coerce-number (coerce-number 0 ',type) ',type)))
       (eval `(Assert-Equal
		       (coerce-number 1 ',type)
		       (coerce-number (coerce-number 1 ',type) ',type)))
       (eval `(Assert (= (coerce-number 0 ',type)
			 (coerce-number (coerce-number 0 ',type) ',type))))
       (eval `(Assert (= (coerce-number 1 ',type)
			 (coerce-number (coerce-number 1 ',type) ',type))))
       ;; lifts are homomorphic wrt negation (equal'ity needs not hold)
       (eval `(Assert (= (coerce-number -1 ',type)
			 (- (coerce-number 1 ',type)))))

       ;; infinity lifts
       (eval `(Assert (indefinitep (coerce-number +infinity ',type))))
       (eval `(Assert (indefinitep (coerce-number -infinity ',type))))
       (eval `(Assert (infinityp (coerce-number +infinity ',type))))
       (eval `(Assert (infinityp (coerce-number -infinity ',type))))
       (eval `(Assert (= +infinity (coerce-number +infinity ',type))))
       (eval `(Assert (= -infinity (coerce-number -infinity ',type))))
       (if (comparablep (coerce-number 0 type))
	   (eval `(Check-Error domain-error
			       (coerce-number complex-infinity ',type)))
	 (eval `(Assert (infinityp
			 (coerce-number complex-infinity ',type))))
	 (eval `(Assert (= complex-infinity
			   (coerce-number complex-infinity ',type)))))
       (eval `(Check-Error domain-error
			   (coerce-number not-a-number ',type))))
   lift-types))

;; testing string conversion
(Assert (string= (number-to-string +infinity) "+infinity"))
(Assert (string= (number-to-string -infinity) "-infinity"))
(Assert (string= (number-to-string complex-infinity) "complex-infinity"))
(Check-Error wrong-type-argument (number-to-string not-a-number))

(when (featurep 'bigfr)
  ;; test computations which throw out an indefinite
  (Assert (indefinitep (log 0)))
  (Assert (infinityp (log 0)))
  (Assert (indefinitep (log -1)))
  (Assert (indefinitep (log10 0)))
  (Assert (infinityp (log10 0)))
  (Assert (indefinitep (log10 -1)))
  (Assert (indefinitep (log2 0)))
  (Assert (infinityp (log2 0)))
  (Assert (indefinitep (log2 -1)))
  (Assert (or (indefinitep (sqrt -2))
	      (complexp (sqrt -2))))
  ;; especially assert that these throws are not bigfr
  (Assert (not (bigfrp (log 0))))
  (Assert (not (bigfrp (log -1))))
  (Assert (not (bigfrp (log10 0))))
  (Assert (not (bigfrp (log10 -1))))
  (Assert (not (bigfrp (log2 0))))
  (Assert (not (bigfrp (log2 -1))))
  (Assert (not (bigfrp (sqrt -2)))))

;; stress test for trig functions
(let ((nan-funs '(acos asin atan cos sin tan sec csc cot
		       cosh sinh tanh sech csch coth
		       acosh asinh atanh
		       erf erfc log-gamma))
      (more-funs '(abs sqrt cbrt log log10 log2
		       ceiling truncate round
		       ffloor fceiling ftruncate fround
		       next-prime
		       canonical-norm conjugate real-part))
      (vals '(+infinity -infinity complex-infinity not-a-number)))
  (mapc #'(lambda (fun)
	    (when (fboundp fun)
	      (mapc #'(lambda (val)
			(eval `(Assert-Equal (,fun ,val) not-a-number)))
		    vals)))
	nan-funs)
  (mapc #'(lambda (fun)
	    (when (fboundp fun)
	      (mapc #'(lambda (val)
			(eval `(Assert (indefinitep (,fun ,val)))))
		    vals)))
	more-funs)

  ;; some more checks
  (when (or (featurep 'bigg)
	    (featurep 'bigc))
    (Assert (zerop (imaginary-part +infinity)))
    (Assert (zerop (imaginary-part -infinity)))
    (Assert (infinityp (imaginary-part complex-infinity))))
  (Assert (zerop (exp -infinity)))
  ;; logb cannot handle infinity, this might change in the future
  ;; same for logand, logior, logxor and lognot
  (mapc #'(lambda (fun)
	    (mapc #'(lambda (val)
		      (eval `(Check-Error wrong-type-argument (,fun ,val))))
		  vals))
	'(;;logb
	  logand logior logxor lognot)))


(when (featurep 'bigz)
  ;; test remove-factor with infinities
  (mapc #'(lambda (i)
	    (eval `(Assert (consp (remove-factor +infinity ,i))))
	    (if (infinityp (eval i))
		(eval `(Assert (infinityp (cdr (remove-factor +infinity ,i)))))
	      (eval `(Assert (zerop (cdr (remove-factor +infinity ,i))))))
	    (eval `(Assert (= (car (remove-factor +infinity ,i)) ,i)))
	    (eval `(Check-Error wrong-type-argument
				(remove-factor -infinity ,i))))
	'(0 1 2 3 4 10 20 50 100 200 -200 -100 -50 -20 -10 -4 -1 +infinity))
  (mapc #'(lambda (i)
	    (eval `(Assert (consp (remove-factor ,i +infinity))))
	    (eval `(Assert (infinityp (cdr (remove-factor ,i +infinity)))))
	    (eval `(Assert (infinityp (car (remove-factor ,i +infinity)))))
	    (eval `(Assert (consp (remove-factor ,i -infinity))))
	    (eval `(Assert (infinityp (cdr (remove-factor ,i -infinity)))))
	    (eval `(Assert (infinityp (car (remove-factor ,i -infinity))))))
	'(2 3 4 10 20 50 100 200 +infinity)))

;; rounding
(when (featurep 'bigq)
  (Assert (integerp (round 10/3)))
  (Assert (integerp (round 11/3)))
  (Assert (integerp (round -10/3)))
  (Assert (integerp (round -11/3)))
  (Assert (= (round 9/3) 3))
  (Assert (= (round 10/3) 3))
  (Assert (= (round 11/3) 4))
  (Assert (= (round 12/3) 4))
  (Assert (= (round -9/3) -3))
  (Assert (= (round -10/3) -3))
  (Assert (= (round -11/3) -4))
  (Assert (= (round -12/3) -4)))


;;-----------------------------------------------------
;; Test zeroes and ones
;;-----------------------------------------------------
(let ((zero 0)
      (zerof 0.0)
      (one 1)
      (onef 1.0))
  (Assert (zerop zero))
  (Assert (zerop zerof))
  (Assert (onep one))
  (Assert (onep onef))
  ;; these tests are useful because there are rings where one is zero
  (Assert (not (zerop one)))
  (Assert (not (zerop onef)))
  (Assert (not (onep zero)))
  (Assert (not (onep zerof)))
  (Assert (onep (1+ zero)))
  (Assert (onep (1+ zerof)))
  (Assert (zerop (1- one)))
  (Assert (zerop (1- onef)))
  ;; check coercions
  (mapc #'(lambda (cat)
	    (when (featurep cat)
	      (eval `(Assert (zerop (coerce-number ,zero ',cat))))
	      (eval `(Assert (zerop (coerce-number ,zerof ',cat))))
	      (eval `(Assert (onep (coerce-number ,one ',cat))))
	      (eval `(Assert (onep (coerce-number ,onef ',cat))))
	      ;; again we test the null-ring property
	      (eval `(Assert (not (zerop (coerce-number ,one ',cat)))))
	      (eval `(Assert (not (zerop (coerce-number ,onef ',cat)))))
	      (eval `(Assert (not (onep (coerce-number ,zero ',cat)))))
	      (eval `(Assert (not (onep (coerce-number ,zerof ',cat)))))))
	'(bigz bigq bigf bigfr bigg bigc)))

(let ((ints (list 1 4 -23 0))
      (bigzs (when (featurep 'bigz)
	       (list (factorial 23) (bigz 40) -892893489238924308234 (bigz 0))))
      (bigqs (when (featurep 'bigq)
	       (list 3/4 (// (factorial 42) 101) -82759873478/1231 (bigq 0))))
      (floats (list 1.4 22.44 -494.2 (float 0)))
      (bigfs (when (featurep 'bigf)
	       (list (bigf 1.44) (bigf (factorial 20)) (bigf 0))))
      (bigfrs (when (featurep 'bigfr)
		(list (exp 1) (atan 1) (exp 0) (bigfr 0))))
      (biggs (when (featurep 'bigg)
	       (list 2+3i (make-bigg (factorial 20) 213) (bigg 0))))
      (bigcs (when (featurep 'bigc)
	       (list 2.3+1.22i (make-bigc (exp 1) (exp 1)) (bigc 0)))))
  (mapc #'(lambda (cat)
	    (mapc #'(lambda (num)
		      ;; zeroes
		      (eval `(Assert (zerop (zero ,num))))
		      (when (comparablep num)
			(eval `(Assert (= (+ (zero ,num) ,num) ,num)))
			(eval `(Assert (= (* (zero ,num) ,num) (zero ,num)))))
		      (unless (comparablep num)
			(eval `(Assert-Equal (+ (zero ,num) ,num) ,num))
			(eval `(Assert
				(equal (* (zero ,num) ,num) (zero ,num)))))
		      ;; ones
		      (eval `(Assert (onep (one ,num))))
		      (eval `(Assert (zerop (1- (one ,num)))))
		      (eval `(Assert (onep (1+ (zero ,num)))))
		      (when (comparablep num)
			(eval `(Assert (= (* (one ,num) ,num) ,num)))
			(eval `(Assert (= (zero ,num) (1- (one ,num))))))
		      (unless (comparablep num)
			(eval `(Assert-Equal (* (one ,num) ,num) ,num))
			(eval `(Assert-Equal (zero ,num) (1- (one ,num))))))
		  (symbol-value cat)))
	'(ints bigzs bigqs floats bigfs bigfrs biggs bigcs)))


;;-----------------------------------------------------
;; Test units
;;-----------------------------------------------------
(Assert (unitp 1))
(Assert (unitp -1))
(Assert (not (unitp 0)))
(Assert (not (unitp 2)))

(when (featurep 'fpfloat)
  (Assert (unitp 0.0))
  (Assert (unitp 1.0))
  (Assert (unitp -1.0))
  (Assert (unitp -2.2)))

(when (featurep 'bigz)
  (Assert (unitp (bigz 1)))
  (Assert (unitp (bigz -1)))
  (Assert (not (unitp (bigz 0))))
  (Assert (not (unitp (bigz -3)))))

(when (featurep 'bigq)
  (Assert (unitp (bigq 1)))
  (Assert (unitp (bigq -1)))
  (Assert (unitp (bigq 0)))
  (Assert (unitp 1/2))
  (Assert (unitp -2/3)))

(when (featurep 'bigf)
  (Assert (unitp (bigf 1)))
  (Assert (unitp (bigf 1.2)))
  (Assert (unitp (bigf 0)))
  (Assert (unitp (bigf -1.3333))))

(when (featurep 'bigfr)
  (Assert (unitp (bigfr 0)))
  (Assert (unitp (bigfr 1)))
  (Assert (unitp (bigfr -1)))
  (Assert (unitp (bigfr -0.2)))
  (Assert (unitp (bigfr 0.2 4096))))

(when (featurep 'gaussian)
  (Assert (unitp 1+0i))
  (Assert (unitp 0+i))
  (Assert (not (unitp 0+0i)))
  (Assert (unitp 1+i))
  (Assert (unitp -1+0i))
  (Assert (unitp -1-i))
  (Assert (unitp 0-i))
  (Assert (not (unitp -2+i)))
  (Assert (not (unitp 1+2i)))
  (Assert (not (unitp 1-3i))))

(when (featurep 'bigc)
  (Assert (unitp (bigc 0)))
  (Assert (unitp (bigc 1)))
  (Assert (unitp (bigc -1)))
  (Assert (unitp 0.5-0.5i))
  (Assert (unitp -0.25+i))
  (Assert (unitp 0.0+2.0i))
  (Assert (unitp -0.1-22.1i)))

(when (featurep 'quatern)
  (Assert (unitp 1+0i+0j+0k))
  (Assert (unitp 0+i+0j+0k))
  (Assert (unitp 0+0i+j+0k))
  (Assert (unitp 0+0i+0j+k))
  (Assert (not (unitp 0+0i+0j+0k)))
  (Assert (unitp 1+i+0j+0k))
  (Assert (unitp 1+0i+j+0k))
  (Assert (unitp 1+0i+0j+k))
  (Assert (unitp 0+i+j+0k))
  (Assert (unitp 0+i+0j+k))
  (Assert (unitp 0+0i+j+k))
  (Assert (unitp 0+i+j+k))
  (Assert (unitp 1+0i+j+k))
  (Assert (unitp 1+i+0j+k))
  (Assert (unitp 1+i+j+0k))
  (Assert (unitp 1+i+j+k))
  (Assert (unitp -1-i-j-k))
  (Assert (unitp 0-i-j+k))
  (Assert (unitp 0+i-j-k))
  (Assert (unitp 1+0i+j-k))
  (Assert (not (unitp 2+i+j+k)))
  (Assert (not (unitp -2+2i-j+2k))))


;;-----------------------------------------------------
;; Test miscellaneous functions
;;-----------------------------------------------------

(Check-Error wrong-type-argument (random 0))
(when (featurep 'bigz)
  (Check-Error wrong-type-argument (random (bigz 0)))

  (dotimes (i 1000)
    (Assert (intp (random)))

    ;; test random function with limit
    (let ((limit (bigz (random))))
      (cond ((positivep limit)
	     (Assert (nonnegativep (random limit))))
	    ((zerop limit)
	     (Check-Error wrong-type-argument (random limit)))
	    (t
	     (Check-Error wrong-type-argument (random limit)))))

    ;; random with limit of 1 should always return zero
    (Assert (zerop (random 1)))
    (Assert (zerop (random (bigz 1)))))

  ;; expect at least one bigz random number in 1000 trials
  (Assert (let ((some nil))
	    (dotimes (i 1000 some)
	      (setq some
		    (or some (bigzp (random (factorial 20)))))))))
