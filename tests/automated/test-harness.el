;; test-harness.el --- Run Emacs Lisp test suites.

;;; Copyright (C) 1998, 2002, 2003 Free Software Foundation, Inc.
;;; Copyright (C) 2002 Ben Wing.

;; Author: Martin Buchholz
;; Maintainer: Stephen J. Turnbull <stephen@xemacs.org>
;; Keywords: testing

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

;;; A test suite harness for testing XEmacs.
;;; The actual tests are in other files in this directory.
;;; Basically you just create files of emacs-lisp, and use the
;;; Assert, Check-Error, Check-Message, and Check-Error-Message functions
;;; to create tests.  See `test-harness-from-buffer' below.
;;; Don't suppress tests just because they're due to known bugs not yet
;;; fixed -- use the Known-Bug-Expect-Failure and
;;; Implementation-Incomplete-Expect-Failure wrapper macros to mark them.
;;; A lot of the tests we run push limits; suppress Ebola message with the
;;; Ignore-Ebola wrapper macro.
;;; Some noisy code will call `message'.  Output from `message' can be
;;; suppressed with the Silence-Message macro.  Functions that are known to
;;; issue messages include `write-region', `find-tag', `tag-loop-continue',
;;; `insert', and `mark-whole-buffer'.  N.B. The Silence-Message macro
;;; currently does not suppress the newlines printed by `message'.
;;; Definitely do not use Silence-Message with Check-Message.
;;; In general it should probably only be used on code that prepares for a
;;; test, not on tests.
;;;
;;; You run the tests using M-x test-emacs-test-file,
;;; or $(EMACS) -batch -l .../test-harness.el -f batch-test-emacs file ...
;;; which is run for you by the `make check' target in the top-level Makefile.

(require 'bytecomp)

(defvar unexpected-test-suite-failures 0
  "Cumulative number of unexpected failures since test-harness was loaded.

\"Unexpected failures\" are those caught by a generic handler established
outside of the test context.  As such they involve an abort of the test
suite for the file being tested.

They often occur during preparation of a test or recording of the results.
For example, an executable used to generate test data might not be present
on the system, or a system error might occur while reading a data file.")

(defvar unexpected-test-suite-failure-files nil
  "List of test files causing unexpected failures.")

;; Declared for dynamic scope; _do not_ initialize here.
(defvar unexpected-test-file-failures)

(defvar test-harness-test-compiled nil
  "Non-nil means the test code was compiled before execution.")

(defvar test-harness-verbose
  (and (not noninteractive) (> (device-baud-rate) search-slow-speed))
  "*Non-nil means print messages describing progress of emacs-tester.")

(defvar test-harness-file-results-alist nil
  "Each element is a list (FILE SUCCESSES TESTS).
The order is the reverse of the order in which tests are run.

FILE is a string naming the test file.
SUCCESSES is a non-negative integer, the number of successes.
TESTS is a non-negative integer, the number of tests run.")

(defvar test-harness-risk-infloops nil
  "*Non-nil to run tests that may loop infinitely in buggy implementations.")

(defvar test-harness-current-file nil)

(defvar emacs-lisp-file-regexp (purecopy "\\.el\\'")
  "*Regexp which matches Emacs Lisp source files.")

(defconst test-harness-file-summary-template
  (format "%%-%ds %%%dd of %%%dd tests successful (%%3d%%%%)."
	  (length "byte-compiler-tests.el:") ; use the longest file name
	  5
	  5)
  "Format for summary lines printed after each file is run.")

(defconst test-harness-null-summary-template
  (format "%%-%ds             No tests run."
	  (length "byte-compiler-tests.el:")) ; use the longest file name
  "Format for \"No tests\" lines printed after a file is run.")

;;;###autoload
(defun test-emacs-test-file (filename)
  "Test a file of Lisp code named FILENAME.
The output file's name is made by appending `c' to the end of FILENAME."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (eq (cdr (assq 'major-mode (buffer-local-variables)))
	      'emacs-lisp-mode)
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name "Test file: " file-dir nil nil file-name))))
  ;; Expand now so we get the current buffer's defaults
  (setq filename (expand-file-name filename))

  ;; If we're testing a file that's in a buffer and is modified, offer
  ;; to save it first.
  (or noninteractive
      (let ((b (get-file-buffer (expand-file-name filename))))
	(if (and b (buffer-modified-p b)
		 (y-or-n-p (format "save buffer %s first? " (buffer-name b))))
	    (save-excursion (set-buffer b) (save-buffer)))))

  (if (or noninteractive test-harness-verbose)
      (message "Testing %s..." filename))
  (let ((test-harness-current-file filename)
	input-buffer)
    (save-excursion
      (setq input-buffer (get-buffer-create " *Test Input*"))
      (set-buffer input-buffer)
      (erase-buffer)
      (insert-file-contents filename)
      ;; Run hooks including the uncompression hook.
      ;; If they change the file name, then change it for the output also.
      (let ((buffer-file-name filename)
	    (default-major-mode 'emacs-lisp-mode)
	    (enable-local-eval nil))
	(normal-mode)
	(setq filename buffer-file-name)))
    (test-harness-from-buffer input-buffer filename)
    (kill-buffer input-buffer)
    ))

(defun test-harness-read-from-buffer (buffer)
  "Read forms from BUFFER, and turn it into a lambda test form."
  (let ((body nil))

    (with-current-buffer buffer
      (save-excursion
	(when (re-search-forward ";;;###eval-me-first" nil t)
	  (let* ((p1 (point))
		 (p2 (progn
		       (forward-sexp)
		       (point))))
	    (eval-region p1 p2)))))

    (goto-char (point-min) buffer)
    (condition-case error-info
	(while t
	  (setq body (cons (read buffer) body)))
      (end-of-file nil)
      (error
       (incf unexpected-test-file-failures)
       (princ (format "Unexpected error %S reading forms from buffer\n"
		      error-info))))
    `(lambda ()
       (defvar passes)
       (defvar assertion-failures)
       (defvar no-error-failures)
       (defvar wrong-error-failures)
       (defvar missing-message-failures)
       (defvar other-failures)

       (defvar trick-optimizer)

       ,@(nreverse body))))

(defun test-harness-from-buffer (inbuffer filename)
  "Run tests in buffer INBUFFER, visiting FILENAME."
  (defvar trick-optimizer)
  (let ((passes 0)
	(assertion-failures 0)
	(no-error-failures 0)
	(wrong-error-failures 0)
	(missing-message-failures 0)
	(other-failures 0)
	(unexpected-test-file-failures 0)

	;; #### perhaps this should be a defvar, and output at the very end
	;; OTOH, this way AC types can use a null EMACSPACKAGEPATH to find
	;; what stuff is needed, and ways to avoid using them
	(skipped-test-reasons (make-hash-table :test 'equal))

	(trick-optimizer nil)
	(debug-on-error t)
	(pass-stream nil))
    (with-output-to-temp-buffer "*Test-Log*"
      (princ (format "Testing %s...\n\n" filename))

      (defconst test-harness-failure-tag "FAIL")
      (defconst test-harness-success-tag "PASS")

      (defmacro Known-Bug-Expect-Failure (&rest body)
	`(let ((test-harness-failure-tag "KNOWN BUG")
	       (test-harness-success-tag "PASS (FAILURE EXPECTED)"))
	  ,@body))

      (defmacro Implementation-Incomplete-Expect-Failure (&rest body)
	`(let ((test-harness-failure-tag "IMPLEMENTATION INCOMPLETE")
	       (test-harness-success-tag "PASS (FAILURE EXPECTED)"))
	  ,@body))

      (defun Print-Failure (fmt &rest args)
	(setq fmt (format "%s: %s" test-harness-failure-tag fmt))
	(if (noninteractive) (apply #'message fmt args))
	(princ (concat (apply #'format fmt args) "\n")))

      (defun Print-Pass (fmt &rest args)
	(setq fmt (format "%s: %s" test-harness-success-tag fmt))
	(and test-harness-verbose
	     (princ (concat (apply #'format fmt args) "\n"))))

      (defun Print-Skip (test reason &optional fmt &rest args)
	(setq fmt (concat "SKIP: %S BECAUSE %S" fmt))
	(princ (concat (apply #'format fmt test reason args) "\n")))

      (defmacro Skip-Test-Unless (condition reason description &rest body)
	"Unless CONDITION is satisfied, skip test BODY.
REASON is a description of the condition failure, and must be unique (it
is used as a hash key).  DESCRIPTION describes the tests that were skipped.
BODY is a sequence of expressions and may contain several tests."
	`(if (not ,condition)
	     (let ((count (gethash ,reason skipped-test-reasons)))
	       (puthash ,reason (if (null count) 1 (1+ count))
			skipped-test-reasons)
	       (Print-Skip ,description ,reason))
	   ,@body))

      (defmacro Assert (assertion)
	`(condition-case error-info
	     (progn
	       (assert ,assertion)
	       (Print-Pass "%S" (quote ,assertion))
	       (incf passes))
	   (cl-assertion-failed
	    (Print-Failure "Assertion failed: %S" (quote ,assertion))
	    (incf assertion-failures))
	   (t (Print-Failure "%S ==> error: %S" (quote ,assertion) error-info)
	      (incf other-failures)
	      )))

      (defmacro Assert-Equal (object1 object2)
	`(condition-case error-info
	     (progn
	       (assert (equal ,object1 ,object2))
	       (Print-Pass "(equal %S %S)" (quote ,object1) (quote ,object2))
	       (incf passes))
	   (cl-assertion-failed
	    (Print-Failure "Assertion failed: (equal %S %S) => (equal %S %S)"
			   (quote ,object1) (quote ,object2) ,object1 ,object2)
	    (incf assertion-failures))
	   (t (Print-Failure "(equal %S %S) ==> error: %S"
			     (quote ,object1) (quote ,object2) error-info)
	      (incf other-failures)
	      )))

      (defmacro Assert-Not-Equal (object1 object2)
	`(condition-case error-info
	     (progn
	       (assert (not (equal ,object1 ,object2)))
	       (Print-Pass "(not (equal %S %S))" (quote ,object1) (quote ,object2))
	       (incf passes))
	   (cl-assertion-failed
	    (Print-Failure "Assertion failed: (not (equal %S %S)) => (not (equal %S %S))"
			   (quote ,object1) (quote ,object2) ,object1 ,object2)
	    (incf assertion-failures))
	   (t (Print-Failure "(not (equal %S %S)) ==> error: %S"
			     (quote ,object1) (quote ,object2) error-info)
	      (incf other-failures)
	      )))

      (defmacro Check-Error (expected-error &rest body)
	(let ((quoted-body (if (= 1 (length body))
			       `(quote ,(car body)) `(quote (progn ,@body)))))
	  `(condition-case error-info
	       (progn
		 (setq trick-optimizer (progn ,@body))
		 (Print-Failure "%S executed successfully, but expected error %S"
				,quoted-body
				',expected-error)
		 (incf no-error-failures))
	     (,expected-error
	      (Print-Pass "%S ==> error %S, as expected"
			  ,quoted-body ',expected-error)
	      (incf passes))
	     (error
	      (Print-Failure "%S ==> expected error %S, got error %S instead"
			     ,quoted-body ',expected-error error-info)
	      (incf wrong-error-failures)))))

      (defmacro Check-Error-Message (expected-error expected-error-regexp
						    &rest body)
	(let ((quoted-body (if (= 1 (length body))
			       `(quote ,(car body)) `(quote (progn ,@body)))))
	  `(condition-case error-info
	       (progn
		 (setq trick-optimizer (progn ,@body))
		 (Print-Failure "%S executed successfully, but expected error %S"
				,quoted-body ',expected-error)
		 (incf no-error-failures))
	     (,expected-error
	      (let ((error-message (second error-info)))
		(if (string-match ,expected-error-regexp error-message)
		    (progn
		      (Print-Pass "%S ==> error %S %S, as expected"
				  ,quoted-body error-message ',expected-error)
		      (incf passes))
		  (Print-Failure "%S ==> got error %S as expected, but error message %S did not match regexp %S"
				 ,quoted-body ',expected-error error-message ,expected-error-regexp)
		  (incf wrong-error-failures))))
	     (error
	      (Print-Failure "%S ==> expected error %S, got error %S instead"
			     ,quoted-body ',expected-error error-info)
	      (incf wrong-error-failures)))))

      ;; Do not use this with Silence-Message.
      (defmacro Check-Message (expected-message-regexp &rest body)
	(Skip-Test-Unless (fboundp 'defadvice)
			  "can't defadvice"
			  expected-message-regexp
	  (let ((quoted-body (if (= 1 (length body))
				 `(quote ,(car body))
			       `(quote (progn ,@body)))))
	    `(let ((messages ""))
	       (defadvice message (around collect activate)
		 (defvar messages)
		 (let ((msg-string (apply 'format (ad-get-args 0))))
		   (setq messages (concat messages msg-string))
		   msg-string))
	       (condition-case error-info
		   (progn
		     (setq trick-optimizer (progn ,@body))
		     (if (string-match ,expected-message-regexp messages)
			 (progn
			   (Print-Pass "%S ==> value %S, message %S, matching %S, as expected"
				       ,quoted-body trick-optimizer messages ',expected-message-regexp)
			   (incf passes))
		       (Print-Failure "%S ==> value %S, message %S, NOT matching expected %S"
				      ,quoted-body  trick-optimizer messages
				      ',expected-message-regexp)
		       (incf missing-message-failures)))
		 (error
		  (Print-Failure "%S ==> unexpected error %S"
				 ,quoted-body error-info)
		  (incf other-failures)))
	       (ad-unadvise 'message)))))

      ;; #### Perhaps this should override `message' itself, too?
      (defmacro Silence-Message (&rest body)
	`(flet ((append-message (&rest args) ())) ,@body))

      (defmacro Ignore-Ebola (&rest body)
	`(let ((debug-issue-ebola-notices -42)) ,@body))

      (defun Int-to-Marker (pos)
	(save-excursion
	  (set-buffer standard-output)
	  (save-excursion
	    (goto-char pos)
	    (point-marker))))

      (princ "Testing Interpreted Lisp\n\n")
      (condition-case error-info
	  (funcall (test-harness-read-from-buffer inbuffer))
	(error
	 (incf unexpected-test-file-failures)
	 (princ (format "Unexpected error %S while executing interpreted code\n"
		error-info))
	 (message "Unexpected error %S while executing interpreted code." error-info)
	 (message "Test suite execution aborted." error-info)
	 ))
      (princ "\nTesting Compiled Lisp\n\n")
      (let (code
	    (test-harness-test-compiled t))
	(condition-case error-info
	    (setq code
		  ;; our lisp code is often intentionally dubious,
		  ;; so throw away _all_ the byte compiler warnings.
		  (letf (((symbol-function 'byte-compile-warn) 'ignore))
		    (byte-compile (test-harness-read-from-buffer inbuffer))))
	  (error
	   (princ (format "Unexpected error %S while byte-compiling code\n"
			  error-info))))
	(condition-case error-info
	    (if code (funcall code))
	  (error
	   (incf unexpected-test-file-failures)
	   (princ (format "Unexpected error %S while executing byte-compiled code\n"
			  error-info))
	   (message "Unexpected error %S while executing byte-compiled code." error-info)
	   (message "Test suite execution aborted." error-info)
	   )))
      (princ (format "\nSUMMARY for %s:\n" filename))
      (princ (format "\t%5d passes\n" passes))
      (princ (format "\t%5d assertion failures\n" assertion-failures))
      (princ (format "\t%5d errors that should have been generated, but weren't\n" no-error-failures))
      (princ (format "\t%5d wrong-error failures\n" wrong-error-failures))
      (princ (format "\t%5d missing-message failures\n" missing-message-failures))
      (princ (format "\t%5d other failures\n" other-failures))
      (let* ((total (+ passes
		       assertion-failures
		       no-error-failures
		       wrong-error-failures
		       missing-message-failures
		       other-failures))
	     (basename (file-name-nondirectory filename))
	     (summary-msg
	      (if (not (noninteractive))
		  (if (> total 0)
		      (format test-harness-file-summary-template
			      (concat basename ":")
			      passes total (/ (* 100 passes) total))
		    (format test-harness-null-summary-template
			    (concat basename ":")))
		""))
	     (reasons ""))
	(maphash (lambda (key value)
		   (setq reasons
			 (concat reasons
				 (format "\n    %d tests skipped because %s."
					 value key))))
		 skipped-test-reasons)
	(when (> (length reasons) 1)
	  (setq summary-msg (concat summary-msg reasons "
    Probably XEmacs cannot find your installed packages.  Set EMACSPACKAGEPATH
    to the package hierarchy root or configure with --package-path to enable
    the skipped tests.")))
	(setq test-harness-file-results-alist
	      (cons (list filename passes total)
		    test-harness-file-results-alist))
	(message "%s" summary-msg))
      (when (> unexpected-test-file-failures 0)
	(setq unexpected-test-suite-failure-files
	      (cons filename unexpected-test-suite-failure-files))
	(setq unexpected-test-suite-failures
	      (+ unexpected-test-suite-failures unexpected-test-file-failures))
	(message "Test suite execution failed unexpectedly."))
      (fmakunbound 'Assert)
      (fmakunbound 'Check-Error)
      (fmakunbound 'Check-Message)
      (fmakunbound 'Check-Error-Message)
      (fmakunbound 'Ignore-Ebola)
      (fmakunbound 'Int-to-Marker)
      (and noninteractive
	   (message "%s" (buffer-substring-no-properties
			  nil nil "*Test-Log*")))
      )))

(defvar test-harness-results-point-max nil)
(defmacro displaying-emacs-test-results (&rest body)
  `(let ((test-harness-results-point-max test-harness-results-point-max))
     ;; Log the file name.
     (test-harness-log-file)
     ;; Record how much is logged now.
     ;; We will display the log buffer if anything more is logged
     ;; before the end of BODY.
     (or test-harness-results-point-max
	 (save-excursion
	   (set-buffer (get-buffer-create "*Test-Log*"))
	   (setq test-harness-results-point-max (point-max))))
     (unwind-protect
	 (condition-case error-info
	     (progn ,@body)
	   (error
	    (test-harness-report-error error-info)))
       (save-excursion
	 ;; If there were compilation warnings, display them.
	 (set-buffer "*Test-Log*")
	 (if (= test-harness-results-point-max (point-max))
	     nil
	   (if temp-buffer-show-function
	       (let ((show-buffer (get-buffer-create "*Test-Log-Show*")))
		 (save-excursion
		   (set-buffer show-buffer)
		   (setq buffer-read-only nil)
		   (erase-buffer))
		 (copy-to-buffer show-buffer
				 (save-excursion
				   (goto-char test-harness-results-point-max)
				   (forward-line -1)
				   (point))
				 (point-max))
		 (funcall temp-buffer-show-function show-buffer))
	      (select-window
	       (prog1 (selected-window)
		 (select-window (display-buffer (current-buffer)))
		 (goto-char test-harness-results-point-max)
		 (recenter 1)))))))))

(defun batch-test-emacs-1 (file)
  (condition-case error-info
      (progn (test-emacs-test-file file) t)
    (error
     (princ ">>Error occurred processing ")
     (princ file)
     (princ ": ")
     (display-error error-info nil)
     (terpri)
     nil)))

(defun batch-test-emacs ()
  "Run `test-harness' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
Each file is processed even if an error occurred previously.
For example, invoke \"xemacs -batch -f batch-test-emacs tests/*.el\""
  ;; command-line-args-left is what is left of the command line (from
  ;; startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (defvar debug-issue-ebola-notices)
  (if (not noninteractive)
      (error "`batch-test-emacs' is to be used only with -batch"))
  (let ((error nil))
    (dolist (file command-line-args-left)
      (if (file-directory-p file)
	  (dolist (file-in-dir (directory-files file t))
	    (when (and (string-match emacs-lisp-file-regexp file-in-dir)
		       (not (or (auto-save-file-name-p file-in-dir)
				(backup-file-name-p file-in-dir)
				(equal (file-name-nondirectory file-in-dir)
				       "test-harness.el"))))
	      (or (batch-test-emacs-1 file-in-dir)
		  (setq error t))))
	(or (batch-test-emacs-1 file)
	    (setq error t))))
    (let ((namelen 0)
	  (succlen 0)
	  (testlen 0)
	  (results test-harness-file-results-alist))
      ;; compute maximum lengths of variable components of report
      ;; probably should just use (length "byte-compiler-tests.el")
      ;; and 5-place sizes -- this will also work for the file-by-file
      ;; printing when Adrian's kludge gets reverted
      (flet ((print-width (i)
	       (let ((x 10) (y 1))
		 (while (>= i x)
		   (setq x (* 10 x) y (1+ y)))
		 y)))
	(while results
	  (let* ((head (car results))
		 (nn (length (file-name-nondirectory (first head))))
		 (ss (print-width (second head)))
		 (tt (print-width (third head))))
	    (when (> nn namelen) (setq namelen nn))
	    (when (> ss succlen) (setq succlen ss))
	    (when (> tt testlen) (setq testlen tt)))
	  (setq results (cdr results))))
      ;; create format and print
      (let ((results (reverse test-harness-file-results-alist)))
	(while results
	  (let* ((head (car results))
		 (basename (file-name-nondirectory (first head)))
		 (nsucc (second head))
		 (ntest (third head)))
	    (if (> ntest 0)
		(message test-harness-file-summary-template
			 (concat basename ":")
			 nsucc
			 ntest
			 (/ (* 100 nsucc) ntest))
	      (message test-harness-null-summary-template
		       (concat basename ":")))
	    (setq results (cdr results)))))
      (when (> unexpected-test-suite-failures 0)
	(message "\n***** There %s %d unexpected test suite %s in %s:"
		 (if (= unexpected-test-suite-failures 1) "was" "were")
		 unexpected-test-suite-failures
		 (if (= unexpected-test-suite-failures 1) "failure" "failures")
		 (if (= (length unexpected-test-suite-failure-files) 1)
		     "file"
		   "files"))
	(while unexpected-test-suite-failure-files
	  (let ((line (pop unexpected-test-suite-failure-files)))
	    (while (and (< (length line) 61)
			unexpected-test-suite-failure-files)
	      (setq line
		    (concat line " "
			    (pop unexpected-test-suite-failure-files))))
	    (message line)))))
    (message "\nDone")
    (kill-emacs (if error 1 0))))

(provide 'test-harness)

;;; test-harness.el ends here
