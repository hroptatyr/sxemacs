;;; files-nomule.el --- file I/O stubs when not under Mule.

;; Copyright (C) 1985-1987, 1992-1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 19.34 (files.el).   (Is it?  Please check)

;;; Commentary:

;; This file is dumped with SXEmacs (when Mule is not compiled in).

;; These stubs were moved from the bottom of files.el.

;;; Code:

(defun insert-file-contents (filename &optional visit start end replace)
  "Insert contents of file FILENAME after point.
Returns list of absolute file name and length of data inserted.
If second argument VISIT is non-nil, the buffer's visited filename
and last save file modtime are set, and it is marked unmodified.
If visiting and the file does not exist, visiting is completed
before the error is signaled.

The optional third and fourth arguments START and END
specify what portion of the file to insert.
If VISIT is non-nil, START and END must be nil.
If optional fifth argument REPLACE is non-nil,
it means replace the current buffer contents (in the accessible portion)
with the file contents.  This is better than simply deleting and inserting
the whole thing because (1) it preserves some marker positions
and (2) it puts less data in the undo list."
  (insert-file-contents-internal filename visit start end replace nil nil))

(defun write-region (start end filename &optional append visit lockname coding-system)
  "Write current region into specified file.
By default, the file's existing contents are replaced by the specified region.
When called from a program, takes three arguments:
START, END and FILENAME.  START and END are buffer positions.
Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).
Optional fifth argument VISIT if t means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not print the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to
  use for locking and unlocking, overriding FILENAME and VISIT.
Kludgy feature: if START is a string, then that string is written
to the file, instead of any buffer contents, and END is ignored.
Optional seventh argument CODING-SYSTEM is meaningful only if support
  for Mule is present in XEmacs and specifies the coding system
  used to encode the text when it is written out, and defaults to
  the value of `buffer-file-coding-system' in the current buffer.
  When Mule support is not present, the CODING-SYSTEM argument is
  ignored."
  (interactive "r\nFWrite region to file: ")
  (write-region-internal start end filename append visit lockname nil))

(defun load (file &optional noerror nomessage nosuffix)
  "Execute a file of Lisp code named FILE.
First try FILE with `.elc' appended, then try with `.el',
 then try FILE unmodified.
This function searches the directories in `load-path'.
If optional second arg NOERROR is non-nil,
 report no error if FILE doesn't exist.
Print messages at start and end of loading unless
 optional third arg NOMESSAGE is non-nil (ignored in -batch mode).
If optional fourth arg NOSUFFIX is non-nil, don't try adding
 suffixes `.elc' or `.el' to the specified name FILE.
Return t if file exists."
  (load-internal file noerror nomessage nosuffix nil nil))

;;; files-nomule.el ends here
