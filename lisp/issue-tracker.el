;; issue-tracker.el --- SXEmacs Bug Reporting/Tracking

;; Copyright (C) 2005 Steve Youngs

;; Author:        Steve Youngs <steve@sxemacs.org>
;; Maintainer:    Steve Youngs <steve@sxemacs.org>
;; Created:       <2005-01-10>
;; Last-Modified: <2005-01-10 11:54:56 (steve)>
;; Homepage:      http://www.sxemacs.org/
;; Keywords:      bugs issues

;; This file is part of SXEmacs.

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

;;; Commentary:
;; 
;;    This will hopefully turn into an interface to the SXEmacs issue
;;    tracker.  For now, all it does is call `report-xemacs-bug' with
;;    the appropiate variables set for SXEmacs rather than XEmacs.

;;; Todo:
;;
;;    Interface to our issue tracker.

;;; Code:
(eval-when-compile (load "cl-macs"))

(ignore-errors
  (require 'xemacsbug))

;;;###autoload
(defun report-sxemacs-bug ()
  (interactive)
  (let ((report-xemacs-bug-address "SXEmacs Devel <sxemacs-devel@sxemacs.org>")
	(xemacs-betaname nil))
    (call-interactively 'report-xemacs-bug)))

(provide 'issue-tracker)
;;; issue-tracker.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
