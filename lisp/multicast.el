;;; multicast.el --- lisp frontend for multicast connections in SXEmacs

;; Copyright (C) 1997-2000 Didier Verna.

;; Author:          Didier Verna <didier@xemacs.org>
;; Maintainer:      Didier Verna <didier@xemacs.org>
;; Created:         Thu Dec  4 16:37:39 1997
;; Last Revision:   Mon Jan 19 19:10:50 1998
;; Current Version: 0.4
;; Keywords:        dumped comm processes

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


;;; Commentary:

;; This file just contains a lisp frontend to the internal function
;; open-multicast-group-internal written in C and belonging to process.c
;; Well, nothing much to say about it ... read the doc string.


;;; Change Log:

;; Rev. of Mon Jan 19 19:04:44 1998 : packaging cleanup
;; Rev. of Thu Dec 11 13:54:26 1997 : updated the docstring
;; Rev. of Mon Dec  8 15:28:47 1997 : Improved the doc string
;; Rev. of Thu Dec  4 16:38:09 1997 : Initial Version.


;;; Code:

(defun open-multicast-group (name buffer address)
  "Open a multicast connection on the specified address.
Returns a process object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER ADDRESS.
NAME is a name for the process. It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at the end of that buffer, unless you specify an output
 stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated with any
 buffer
ADDRESS specifies a standard multicast address \"dest/port/ttl\":
 dest is an internet address between 224.0.0.0 and 239.255.255.255
 port is a communication port like in traditional unicast
 ttl is the time-to-live (15 for site, 63 for region and 127 for world).

WARNING: it is *strongly* recommended to avoid using groups beginning with
	 224 or 239. Such groups are considered 'admin' groups, and may
	 behave in a surprising way ..."
  (let (dest port ttl)
    ;; We check only the general form of the multicast address.
    ;; The rest will be handled by the internal function.
    (string-match #r"^\([0-9\.]+\)/\([0-9]+\)/\([0-9]+\)$" address)
    (and (not (and (= (match-beginning 0) 0)
		   (= (match-end 0) (length address))))
	 (error "malformed multicast address: %s" address))
    (and (not (setq dest (match-string 1 address)))
	 (error "invalid destination specification."))
    (and (= 0 (setq port (string-to-int (match-string 2 address))))
	 (error "invalid port specification."))
    (and (= 0 (setq ttl (string-to-int (match-string 3 address))))
	 (error "invalid ttl specification."))
    (open-multicast-group-internal name buffer dest port ttl)
    ))

;;; multicast.el ends here
