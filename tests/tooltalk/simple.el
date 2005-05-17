;;; Example of Sending Messages

(defun tooltalk-random-query-handler (msg pat)
  (let ((state (get-tooltalk-message-attribute msg 'state)))
    (cond
      ((eq state 'TT_HANDLED)
       (message (get-tooltalk-message-attribute msg arg_val 0)))
      ((memq state '(TT_FAILED TT_REJECTED))
       (message "Random query turns up nothing")))))

(setq random-query-message
  '(   class TT_REQUEST
       scope TT_SESSION
     address TT_PROCEDURE
	  op "random-query"
        args ((TT_INOUT "?" "string"))
    callback tooltalk-random-query-handler))

(let ((m (make-tooltalk-message random-query-message)))
      (send-tooltalk-message m))

;;; Example of Receiving Messaegs

(defun tooltalk-display-string-handler (msg pat)
  (return-tooltalk-message msg 'reply)
  (describe-tooltalk-message msg)
  (message (get-tooltalk-message-attribute msg 'arg_val 0)))

(setq display-string-pattern
  '(category TT_HANDLE
       scope TT_SESSION
	  op "emacs-eval"
	args ((TT_IN "filename" "string"))
    callback tooltalk-display-string-handler))

(let ((p (make-tooltalk-pattern display-string-pattern)))
  (register-tooltalk-pattern p))
 
