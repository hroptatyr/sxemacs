;;; ffi-sqlite.el --- FFI for sqlite3.

<<<<<<< HEAD
;; Copyright (C) 2008 by Zajcev Evegny.
=======
;; Copyright (C) 2008 by Zajcev Evgeny.
>>>>>>> origin/master

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Sat Nov 22 01:08:31 2008
;; Keywords: db, ffi

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; Simple usage example:
;;
;;     (setq db (sqlite-open "~/mtest.db"))
;;       ==> #<ffiobject type=pointer size=4 fotype=0 foptr=0xb6a6600>
;;     (sqlite-execute db "CREATE TABLE PARAMS
;;         (NAME VARCHAR(256) NULL, VALUE VARCHAR(1024) NULL)")
;;       ==> t
;;     (sqlite-execute db "INSERT INTO PARAMS (NAME,VALUE)
;;         VALUES (?, ?)" '("nthoteh" "HRCHRCCR"))
;;       ==> t
;;     (sqlite-rows db "select * from params")
;;       ==> (("nthoteh" "HRCHRCCR"))
;;     (sqlite-close db)
;;       ==> 0
;;
;; To create database in memory use (sqlite-open ":memory:")
;; 
;; Custom collations:
;;
;;     (defun Nfirst-collation (s1 s2)
;;       "S1 is less then S2 by length"
;;       (let ((l1 (length s1))
;;             (l2 (length s2)))
;;         (cond ((= l1 l2) 0)
;;               ((< l1 l2) -1)
;;               (t 1))))
;;
;;     (sqlite-create-collation db "nfirst" 'Nfirst-collation)
;;
;; Then use 'nfirst' name in queries, like this
;;
;;     (sqlite-rows db "select * from TABLE order by NAME collate nfirst")
;;

;;; TODO:
;;
;;   - Support for int64 columns
;;   - sqlite3_collation_needed implementation
;;   - Cached statements
;;

;;; Code:
(require 'ffi)
(ffi-load-library "libsqlite3")


;;{{{ FFI to sqlite3

(define-ffi-type sqlite pointer)
(define-ffi-type sqlite-statement pointer)

(cffi:defcfun ("sqlite3_libversion" sqlite:version) c-string)

(cffi:defcfun ("sqlite3_open" sqlite:open-internal) int
  (filename c-string) (db (pointer sqlite)))

(cffi:defcfun ("sqlite3_close" sqlite:close-internal) int
  (db sqlite))

(cffi:defcfun ("sqlite3_prepare" sqlite:prepare) int
  (db sqlite) (sql c-string) (bytes int)
  (statement (pointer sqlite-statement))
  (tail (pointer c-string)))

(cffi:defcfun ("sqlite3_step" sqlite:step) int
  (statement sqlite-statement))

(cffi:defcfun ("sqlite3_finalize" sqlite:finalize) int
  (statement sqlite-statement))

(cffi:defcfun ("sqlite3_errmsg" sqlite:errmsg) c-string
  (db sqlite))

(cffi:defcfun ("sqlite3_errcode" sqlite:errcode) int
  (db sqlite))

(cffi:defcfun ("sqlite3_changes" sqlite:changes) int
  (db sqlite))

(cffi:defcfun ("sqlite3_get_table" sqlite:get-table) int
  (db sqlite) (sql c-string) (result (pointer (pointer c-string)))
  (rows (pointer int)) (cols (pointer int)) (errmsg (pointer c-string)))

(cffi:defcfun ("sqlite3_free_table" sqlite:free-table) void
  (table (pointer c-string)))

(cffi:defcfun ("sqlite3_free" sqlite:free) void
  (string c-string))

;; Number of columns implied by statement
(cffi:defcfun ("sqlite3_column_count" sqlite:column-count) int
  (statement sqlite-statement))

;; Number of columns actually present in this row
(cffi:defcfun ("sqlite3_data_count" sqlite:data-count) int
  (statement sqlite-statement))

(cffi:defcfun ("sqlite3_reset" sqlite:reset) int
  (statement sqlite-statement))

(cffi:defcfun ("sqlite3_column_type" sqlite:column-type) int
  (statement sqlite-statement) (which int))

(cffi:defcfun ("sqlite3_column_int" sqlite:column-int) int
  (statement sqlite-statement) (which int))

(cffi:defcfun ("sqlite3_column_double" sqlite:column-double) double
  (statement sqlite-statement) (which int))

(cffi:defcfun ("sqlite3_column_text" sqlite:column-text) c-string
  (statement sqlite-statement) (which int))

(cffi:defcfun ("sqlite3_column_blob" sqlite:column-blob) pointer
  (statement sqlite-statement) (which int))

(cffi:defcfun ("sqlite3_column_bytes" sqlite:column-bytes) int
  (statement sqlite-statement) (which int))

(cffi:defcfun ("sqlite3_column_name" sqlite:column-name) c-string
  (statement sqlite-statement) (which int))

;; binding
(cffi:defcfun ("sqlite3_bind_text" sqlite:bind-text) int
  (statment sqlite-statement) (position int) (bound c-string)
  (length int) (cleanup int))

(cffi:defcfun ("sqlite3_bind_null" sqlite:bind-null) int
  (statement sqlite-statement)
  (position int))

(cffi:defcfun ("sqlite3_bind_int" sqlite:bind-int) int
  (statement sqlite-statement)
  (position int)
  (bound int))

(cffi:defcfun ("sqlite3_bind_double" sqlite:bind-double) int
  (statement sqlite-statement) (position int) (bound double))

(cffi:defcfun ("sqlite3_bind_blob" sqlite:bind-blob) int
  (statement sqlite-statement) (position int)
  (bound pointer) (length int) (cleanup int))

(cffi:defcfun ("sqlite3_bind_parameter_count"
               sqlite:bind-parameter-count) int
  (statement sqlite-statement))

(cffi:defcfun ("sqlite3_bind_parameter_index"
               sqlite:bind-parameter-index) int
  (statement sqlite-statement) (column c-string))

(cffi:defcfun ("sqlite3_bind_parameter_name"
               sqlite:bind-parameter-name) c-string
  (statement sqlite-statement)
  (position int))

(cffi:defcfun ("sqlite3_create_collation" sqlite:create-collation) int
  (db sqlite) (name c-string) (e-text-rep int)
  (user-data pointer) (callback pointer))

;;}}}


;;; API
;;{{{ Constants

(defconst sqlite-STATIC 0)
(defconst sqlite-TRANSIENT -1)
(defconst sqlite-UTF-8 1
  "Used as e-text-rep argument to `sqlite:create-collation'.")

(defconst sqlite-ROW 100 "Sqlite_step() has another row ready.")
(defconst sqlite-DONE 101 "Sqlite_step() has finished executing.")

;;}}}
;;{{{ Errors handling

(defconst sqlite-error-codes
  '((1 sqlite-error "SQL error or missing database")
    (2 sqlite-internal "An internal logic error in SQLite")
    (3 sqlite-perm "Access permission denied")
    (4 sqlite-abort "Callback routine requested an abort")
    (5 sqlite-busy "The database file is locked")
    (6 sqlite-locked "A table in the database is locked")
    (7 sqlite-nomem "A malloc() failed")
    (8 sqlite-readonly "Attempt to write a readonly database")
    (9 sqlite-interrupt "Operation terminated by sqlite_interrupt()")
    (10 sqlite-ioerr "Some kind of disk I/O error occurred")
    (11 sqlite-corrupt "The database disk image is malformed")
    (13 sqlite-full "Insertion failed because database is full")
    (14 sqlite-cantopen "Unable to open the database file")
    (15 sqlite-protocol "Database lock protocol error")
    (17 sqlite-schema "The database schema changed")
    (18 sqlite-toobig "Too much data for one row of a table")
    (19 sqlite-constraint "Abort due to constraint violation")
    (20 sqlite-mismatch "Data type mismatch")
    (21 sqlite-misuse "Library used incorrectly")
    (22 sqlite-nolfs "Uses OS features not supported on host")
    (23 sqlite-auth "Authorization denied"))
  "Error codes hash table for sqlite.")

(define-error 'sqlite-error "SQLite error")
(define-error 'sqlite-sql-error "SQLite SQL error")
(define-error 'sqlite-datatype-error "SQLite datatype error")

(defun sqlite-check-result (result error)
  "Signall an error if RESULT is not good enough.
ERROR is message to be signaled."
  (let ((tuple (cdr (assq result sqlite-error-codes))))
    (cond ((and tuple (eq (first tuple) 'sqlite-error))
           (error 'sqlite-sql-error (if (equal error "not an error") "" error)
                  :code 1 :name (first tuple)
                  :comment (second tuple)))
          (tuple
           (error 'sqlite-error error
                  :code result :name (first tuple) :comment (second tuple)))
          (t result))))

;;}}}
;;{{{ Open/close database

(defvar sqlite-databases nil
  "List of currently onen databases.")

;;;###autoload
(defun sqlite-file-p (filename)
  "Return non-nil if FILENAME is actually SQLite format 3 file."
  ;; Unfortunately `magic:file-type' does not recognizes SQLite files,
  ;; so do it by hand
  (with-temp-buffer
    (insert-file-contents-literally filename nil 0 15)
    (string= (buffer-substring) "SQLite format 3")))

;;;###autoload
(defun sqlite-open (file)
  "Try to open SQLite dabase stored in FILE.
On success return database object."
  (let* ((db (make-ffi-object 'sqlite))
         (result (sqlite:open-internal
                  (expand-file-name file) (ffi-address-of db)))
         (retdb (ffi-get db)))
    (sqlite-check-result result (sqlite:errmsg retdb))
    (push retdb sqlite-databases)
    retdb))

(defun sqlite-close (db)
  "Close SQLite database associated with DB."
  (setq sqlite-databases (delq db sqlite-databases))
  (sqlite-check-result
   (sqlite:close-internal db) (sqlite:errmsg db)))

;;}}}
;;{{{ Statement operations: binding, fetching

(defun sqlite-column-names (statement)
  "For STATEMENT get list of column names."
  (loop for i from 0 below (sqlite:column-count statement)
    collect (sqlite:column-name statement i)))

(defun sqlite-prepare (db sql)
  "For DB prepare statement for given SQL."
  (let ((stat (make-ffi-object 'sqlite-statement))
        (tail (make-ffi-object 'pointer)))
    (sqlite-check-result
     (sqlite:prepare db sql (length sql)
                     (ffi-address-of stat)
                     (ffi-address-of tail))
     (sqlite:errmsg db))
    (ffi-get stat)))

(defun sqlite-bind-value (statement key-object value &optional copy-flag)
  "Take a STATEMENT pointer, a KEY-OBJECT and bind VALUE to it.
KEY-OBJECT should be an integer position or a symbol or a string.
Optional COPY-FLAG is one of `sqlite-STATIC' or `sqlite-TRANSIENT'
\(the default one\).  You must know exactly what you are doing if you
provide COPY-FLAG argument."
  (let ((key (if (integerp key-object)
                 key-object
               (sqlite:bind-parameter-index
                statement
                (if (symbolp key-object)
                    (concat ":" (symbol-name key-object))
                  key-object)))))
    (when key
      (cond ((null value) (sqlite:bind-null statement key))
            ((integerp value)
             (sqlite:bind-int statement key value))
            ((floatp value)
             (sqlite:bind-double statement key value))
            ((stringp value)
             (sqlite:bind-text statement key value (length value)
                               (or copy-flag sqlite-TRANSIENT)))
            ((and (consp value) (eq (car value) 'blob)
                  (stringp (cdr value)))
             (let ((bval (ffi-create-fo `(c-data . ,(length (cdr value)))
                                        (cdr value))))
               (sqlite:bind-blob
                statement key bval (length (cdr value))
                (or copy-flag sqlite-TRANSIENT))))
            (t (error 'sqlite-datatype-error value
                      :comment (concat "Attempt to insert data not one of "
                                       "integer, float, text, or blob."))))
      )))

(defun sqlite-bind-seq (statement sequence)
  "For STATEMENT bind each value in SEQUENCE."
  (dotimes (i (length sequence))
    (sqlite-bind-value statement (1+ i) (elt sequence i))))

(defun sqlite-bind-plist (statement plist)
  "For STATEMENT bind values in PLIST."
  (loop for (key val) on plist by #'cddr
    do (sqlite-bind-value statement key val)))

(defun sqlite-bind (statement binding)
  "For STATEMENT perform BINDING.
BINDING could be plist, list or vector."
  (when binding
    (cond ((and (listp binding) (keywordp (car binding)))
           (sqlite-bind-plist statement binding))
          ((or (vectorp binding) (listp binding))
           (sqlite-bind-seq statement binding)))
    t))

(defun sqlite-fetch-column (statement index)
  "For STATEMENT fetch data from column INDEX.
There is currently no way to specify a casting operation."
  ;; INTEGER==1,FLOAT==2,TEXT==3,BLOB==4,NULL==5
  (case (sqlite:column-type statement index)
    (1 (sqlite:column-int statement index))
    (2 (sqlite:column-double statement index))
    (3 (sqlite:column-text statement index))
    (4 (let ((blob (sqlite:column-blob statement index))
             (blen (sqlite:column-bytes statement index)))
         (ffi-get blob :type (cons 'c-data blen))))
    (5 nil)))

(defun sqlite-fetch (statement)
  "For STATEMENT return vector containing all the columns.
nil is returned for empty row."
  (when (= sqlite-ROW (sqlite:step statement))
    (let ((cols (sqlite:column-count statement)))
      (loop for i from 0 below cols
        collect (sqlite-fetch-column statement i)))))

(defun sqlite-reset (statement &optional clear-bindings)
  "Reset STATEMENT, so it could be used again.
If CLEAR-BINDINGS is specified also clear all bound variables."
  (sqlite:reset statement)
  (when clear-bindings
    (sqlite-bind-seq
     statement (make-list (sqlite:bind-parameter-count statement) nil)))
  statement)

(defun sqlite-flush (statement)
  "Clean up a prepared STATEMENT.
DO NOT use STATEMENT after."
  (sqlite:finalize statement))

;;}}}
;;{{{ Transactions

(defun sqlite-begin-transaction (db &optional type)
  "Begin transaction for DB.
TYPE is one of:
 'deferred
 'exclusive
 'immediate
Default is deffered."
  (let ((ttype (multiple-value-bind (major minor sub)
                   (mapcar 'string-to-int
                           (split-string-by-char (sqlite:version) ?\.))
                 (setq major major)     ; shut up compiler
                 (if (or (>= minor 0) (>= sub 8))
                     (cond ((eq type 'exclusive) " exclusive ")
                           ((eq type 'immediate) " immediate ")
                           (t " "))
                   " "))))
    (sqlite-execute db (concat "begin" ttype "transaction;") :begin nil)))

(defun sqlite-commit (db)
  "For DB commit transaction."
  (sqlite-execute db "commit transaction;" :begin nil))

(defun sqlite-rollback (db)
  "For DB roll back transaction."
  (sqlite-execute db "rollback transaction;" :begin nil))

(defmacro* sqlite-with-transaction ((database &optional type) &body body)
  (let ((db-err (gensym "dberror"))
        (db-obj (gensym "dbobject")))
    `(let ((,db-obj ,database)
           (,db-err t))
       (sqlite-begin-transaction ,db-obj ,type)
       (unwind-protect
           (prog1
               (progn ,@body)
             (setq ,db-err nil))
         (if ,db-err
             (sqlite-rollback ,db-obj)
           (sqlite-commit ,db-obj))))))
(put 'sqlite-with-transaction 'lisp-indent-function 'defun)

;;}}}
;;{{{ execute, mapcar, rows

(defmacro* sqlite-with-prep ((statement-var db sql &optional bind) &body body)
  (let ((db-obj (gensym "dbobject"))
        (sql-in (gensym "sqlin"))
        (bind-in (gensym "bindin")))
    `(let* ((,db-obj ,db)
            (,sql-in ,sql)
            (,bind-in ,bind)
            (,statement-var (sqlite-prepare ,db-obj ,sql-in)))
       (unwind-protect
           (progn
             (sqlite-bind ,statement-var ,bind-in)
             ,@body)
         (sqlite-flush ,statement-var)))))
(put 'sqlite-with-prep 'lisp-indent-function 'defun)

(defun sqlite-execute (db sql &optional bind begin)
  "For DB execute SQL query.
Use this for queries with no return results.
BIND specifies bindings for SQL query.
If BEGIN is given, then perform a transaction."
  (if begin
      (sqlite-with-transaction (db)
        (sqlite-with-prep (st db sql bind)
          (sqlite-check-result (sqlite:step st) (sqlite:errmsg db))
          t))
    (sqlite-with-prep (st db sql bind)
      (sqlite-check-result (sqlite:step st) (sqlite:errmsg db))
      t)))

(defun sqlite-mapcar (db function sql &optional bind)
  "For DB apply FUNCTION to results of SQL query execution.
BIND specifies bindings list for SQL query.
FUNCTION must take at least as many arguments as values in row."
  (sqlite-with-prep (statement db sql bind)
    (loop with row = nil
      while (setq row (sqlite-fetch statement))
      collect (apply function row))))

(defun sqlite-rows (db sql &optional bind)
  "For DB return list of rows after executing SQL query.
BIND specifies bindings for SQL query."
  (sqlite-mapcar db #'list sql bind))

;;}}}
;;{{{ Custom collations

(define-ffi-callback sqlite-generic-collation 'int
  ((user-data 'pointer) (len1 'int) (str1 'pointer)
   (len2 'int) (str2 'pointer))
  (let ((fun (ffi-pointer-to-lisp-object user-data))
        (s1 (ffi-get str1 :type (cons 'c-data len1)))
        (s2 (ffi-get str2 :type (cons 'c-data len2))))
    (funcall fun s1 s2)))

(defun sqlite-create-collation (db name compare-function)
  "For DB register new collation named NAME.
COMPARE-FUNCTION must get exactly two string arguments and return:
  -1  if first string is less then second
   0  if strings are equal
   1  if first string is greater then second"
  (let* ((ccolls (get db 'custom-collations))
         (colla (assoc name ccolls)))
    (if colla
        (setcdr colla compare-function)
      (put db 'custom-collations
           (cons (cons name compare-function) ccolls))))
  (sqlite-check-result
   (sqlite:create-collation
    db name sqlite-UTF-8
    (ffi-lisp-object-to-pointer compare-function)
    (ffi-callback-fo 'sqlite-generic-collation))
   (sqlite:errmsg db)))

(defun sqlite-remove-collation (db name)
  "For DB remove collation by NAME."
  (let* ((ccolls (get db 'custom-collations))
         (colla (assoc name ccolls)))
    (when colla
      (sqlite-check-result
       (sqlite:create-collation
        db name sqlite-UTF-8
        (ffi-lisp-object-to-pointer (cdr colla))
        (ffi-null-pointer))
       (sqlite:errmsg db))
      ;; Remove it from custom-collations
      (put db 'custom-collations (del-alist name ccolls))
      t)))

;;}}}
            
(provide 'ffi-sqlite)

;;; ffi-sqlite.el ends here
