;;; benchmark.el ---
;; Copyright (C) 2007 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: profiling, benchmark
;;
;; This file is part of SXEmacs.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;; Synched up with: Not in FSF.
;;
;;; Code:

(defmacro bm-profile-time-ord (&rest body)
  `(let ((sttb (current-time)))
     ,@body
     (time-subtract (current-time) sttb)))

(defmacro bm-profile-time-multi-ord (count &rest body)
  `(let* ((cnt ,count)
	  (sttb (current-time)))
     (while (< 0 cnt)
       ,@body
       (setq cnt (1- cnt)))
     (time-subtract (current-time) sttb)))


(defmacro bm-profile-time-btime (&rest body)
  `(let* ((none (garbage-collect))
	  (sttb (current-btime)))
     ,@body
     (- (current-btime) sttb)))

(defmacro bm-profile-time-multi-btime (count &rest body)
  `(let* ((cnt ,count)
	  (sttb (current-btime)))
     (while (nonnegativep cnt)
       ,@body
       (setq cnt (1- cnt)))
     (- (current-btime) sttb)))


(cond ((fboundp #'current-btime)
       (defalias 'bm-profile-time 'bm-profile-time-btime)
       (defalias 'bm-profile-time-multi 'bm-profile-time-multi-btime))
      ((fboundp #'time-subtract)
       (defalias 'bm-profile-time 'bm-profile-time-ord)
       (defalias 'bm-profile-time-multi 'bm-profile-time-multi-ord))
      (t nil))

(put 'bm-profile-time 'lisp-indent-function 'defun)
(put 'bm-profile-time-multi 'lisp-indent-function 'defun)


(defun bm-compute-sample-points (test-range &optional grain)
  (let* ((lo (car test-range))
	 (hi (cdr test-range))
	 (grain-ratio (// hi lo grain))
	 (lol (log lo))
	 (hil (log hi))
	 (step (// (- hi lo) grain))
	 (stepl (// (- hil lol) grain))
	 (i (coerce-number lol 'int))
	 (logp (> grain-ratio 1))
	 (result (make-vector (1+ grain) nil))
	 logf linf)
    (fset #'logf
	  #'(lambda (unused)
	      (let* ((exp (exp lol))
		     (this (coerce-number exp 'int)))
		(setq lol (+ lol stepl))
		this)))
    (fset #'linf
	  #'(lambda (unused)
	      (let* ((this (coerce-number lo 'int)))
		(setq lo (+ lo step))
		this)))
    (mapc-inplace (if logp #'logf #'linf) result)
    result))

(defun bm-util-average (dllv c)
  (let* ((size (dllist-size dllv))
	 (res 0))
    (mapc-internal
     #'(lambda (v)
	 (setq res (+ res (aref v c))))
     dllv)
    (// res size)))

(defun bm-util-deviation (dllv avg c)
  (let* ((size (dllist-size dllv))
	 (res 0))
    (mapc-internal
     #'(lambda (v)
	 (setq res
	       (+ res (^ (- (aref v c) avg) 2))))
     dllv)
    (sqrt (// res size))))

(defun bm-determine-time-stability (test-funv result)
  "Examine RESULT and try to guess if time data behaves like O(1)."
  (let* ((vlen (length test-funv))
	 (resv (make-vector (1+ vlen) 0))
	 (j 0)
	 (avgsum 0)
	 (devsum 0))
    (while (< j vlen)
      (let* ((fun (aref test-funv j))
	     (name (function-documentation fun))
	     (avg (bm-util-average result j))
	     (adev (bm-util-deviation result avg j))
	     (rdev (* (// adev avg) 100)))
	(setq avgsum (+ avgsum avg)
	      devsum (+ devsum adev))
	(aset resv j (vector name avg adev rdev)))
      (setq j (1+ j)))
    (aset resv vlen (vector "SUM" avgsum devsum (* (// devsum avgsum) 100)))
    resv))

(defun bm-examine-time-stability (test-funv point)
  (princ (format "Testing time stability on %d ...\n" point)
	 'external-debugging-output)
  (let* ((grain 100)
	 (pile (dllist)))
    (loop for i below grain
      do (dllist-append
	  pile
	  (prog2
	    ;; for the log
	    (insert (format "%d" point))
	    (mapvector
	     #'(lambda (f)
		 (let* ((time
			 (bm-profile-time
			  (funcall f point))))
		   ;; for the log
		   (insert (format " %d" time))
		   time))
	     test-funv)
	    ;; for the log
	    (insert (format "\n" point)))))
    (let* ((ts (bm-determine-time-stability test-funv pile)))
      ;;(bm-dump-pile test-funv pile ts)
      (mapc-internal
       #'(lambda (v)
	   (let* ((name (aref v 0))
		  (avg (aref v 1))
		  (adev (aref v 2))
		  (rdev (aref v 3)))
	     (princ (format "%32s, avg: %10d us (±%d us = %3.2f %%%s)\n"
			    name avg adev rdev
			    (if (< rdev 25) "" " = UNSTABLE!"))
		    'external-debugging-output)))
       ts)
      (vector pile ts))))

(defun bm-run-tests (test-funv point)
  (bm-examine-time-stability test-funv point))

(defun bm-estimate-time (test &optional grain)
  (let* ((test-funv (plist-get test :test-funv))
	 (test-range (plist-get test :test-range))
	 (grain (or grain (plist-get test :grain) 20))
	 (pf (plist-get test :plot-file))
	 (buf (get-buffer-create (or pf "dontcare")))
	 (tflen (length test-funv))
	 (sample-points (bm-compute-sample-points test-range grain))
	 (samples (make-skiplist)))
    (with-temp-buffer
      ;; for the log
      (insert "N")
      (loop for j from 1 to tflen
	do (insert (format " t%d" j)))
      (insert "\n")
      ;; now run the tests
      (mapcar
       #'(lambda (point)
	   (put-skiplist samples point (bm-run-tests test-funv point)))
       sample-points)
      (write-region-internal (point-min) (point-max) pf)
      (princ "\n" 'external-debugging-output))
    samples))


(defun bm-plot-samples-comp (samples c buf)
  ;; samples is a vector consisting of a pile and an estimation?
  (with-current-buffer (get-buffer-create buf)
    (insert (format "\n### component %d\n" c))
    (map-skiplist
     #'(lambda (key val)
	 (let* ((pile (aref val 0))
		(ts (aref val 1))
		(tslen (length ts))
		(vec (aref ts c)))
	   (insert (format "%d %f %f %f\n"
			   key (aref vec 1) (aref vec 2) (aref vec 3)))))
     samples)
    (insert "\n")))

(defun bm-dump-samples (samples buf)
  ;; samples is a vector consisting of a pile and an estimation?
  (with-current-buffer (get-buffer-create buf)
    (map-skiplist
     #'(lambda (key val)
	 (insert (format "%s %S\n" key val)))
     samples)
    (insert "\n")))

(defun bm-dump-pile (test-funv pile ts)
  (with-temp-buffer
    (let* ((vlen (length test-funv))
	   (resv (make-vector (1+ vlen) 0))
	   (j 0))
      (while (< j vlen)
	(let* ((fun (aref test-funv j))
	       (name (function-documentation fun))
	       (i 0))
	  (insert "\n\n## " name "\n")
	  (mapc-internal
	   #'(lambda (v)
	       (insert (format "%d %f\n" i (aref v j)))
	       (setq i (1+ i)))
	   pile))
	(setq j (1+ j)))
      (write-region (point-min) (point-max) "benchmark.plot")
      (princ "\n" 'external-debugging-output))))


(provide 'benchmark)
