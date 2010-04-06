;also do this: make two frames, one viewing "*scratch*", the other "foo".
;in *scratch*, type (sit-for 20)^J
;wait a couple of seconds, move cursor to foo, type "a"
;a should be inserted in foo.  Cursor highlighting should not change in
;the meantime.

;do it with sleep-for.  move cursor into foo, then back into *scratch*
;before typing.
;repeat also with (accept-process-output nil 20)

;make sure ^G aborts sit-for, sleep-for and accept-process-output:

 (defun tst ()
  (list (condition-case c
	    (sleep-for 20)
	  (quit c))
	(read-char)))

 (tst)^Ja^G    ==>  ((quit) 97) with no signal
 (tst)^J^Ga    ==>  ((quit) 97) with no signal
 (tst)^Jabc^G  ==>  ((quit) 97) with no signal, and "bc" inserted in buffer

; with sit-for only do the 2nd test.
; Do all 3 tests with (accept-proccess-output nil 20)

/*
Additional test cases for accept-process-output, sleep-for, sit-for.
Be sure you do all of the above checking for C-g and focus, too!

; Make sure that timer handlers are run during, not after sit-for:
(defun timer-check ()
  (add-timeout 2 '(lambda (ignore) (message "timer ran")) nil)
  (sit-for 5)
  (message "after sit-for"))

; The first message should appear after 2 seconds, and the final message
; 3 seconds after that.
; repeat above test with (sleep-for 5) and (accept-process-output nil 5)

; Make sure that process filters are run during, not after sit-for.
(defun fubar ()
  (message "sit-for = %s" (sit-for 30)))
(add-hook 'post-command-hook 'fubar)

; Now type M-x shell RET
; wait for the shell prompt then send: ls RET
; the output of ls should fill immediately, and not wait 30 seconds.

; repeat above test with (sleep-for 30) and (accept-process-output nil 30)



; Make sure that recursive invocations return immediately:
(defmacro test-diff-time (start end)
  `(+ (* (- (car ,end) (car ,start)) 65536.0)
      (- (cadr ,end) (cadr ,start))
      (/ (- (caddr ,end) (caddr ,start)) 1000000.0)))

(defun testee (ignore)
  (sit-for 10))

(defun test-them ()
  (let ((start (current-time))
        end)
    (add-timeout 2 'testee nil)
    (sit-for 5)
    (add-timeout 2 'testee nil)
    (sleep-for 5)
    (add-timeout 2 'testee nil)
    (accept-process-output nil 5)
    (setq end (current-time))
    (test-diff-time start end)))

(test-them) should sit for 15 seconds.
