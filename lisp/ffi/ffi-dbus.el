;;; ffi-dbus.el --- FFI for D-Bus.

;; Copyright (C) 2008 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <lg@sxemacs.org>
;; Keywords: interface, ffi

;; This file is part of SXEmacs.
;;
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
;;
;;; Synched up with: Not in FSF

;;; Commentary:

;; Th is the D-Bus for SXEmacs.  For usage please look at TESTING
;; section at the end of this file.


;;; Code:
(defun dbus-load-lib (libs)
  "Try load any library from LIBS."
  (condition-case nil
      (ffi-load (car libs))
    (t (dbus-load-lib (cdr libs)))))

(dbus-load-lib '("libdbus-1.so" "libdbus.so"))

(define-ffi-type dbus-bus-type int)
(define-ffi-type dbus-bool boolean)

(defconst dbus-type-invalid 0
  "Type code that is never equal to a legitimate type code.")

(defconst dbus-type-string (char-int ?s)
  "Type code marking a UTF-8 encoded, nul-terminated Unicode string.")

(defconst dbus-type-int32 (char-int ?i)
  "Type code marking a 32-bit signed integer.")

;;{{{ Constants

(defconst DBUS_BUS_SESSION 0 "DBus type: Session")
(defconst DBUS_BUS_SYSTEM 1 "DBus type: System")
(defconst DBUS_BUS_STARTER 2 "DBus type: Starter")

(defconst DBUS_HANDLER_RESULT_HANDLED 0 "Result handled")
(defconst DBUS_HANDLER_RESULT_NOT_YET_HANDLED 1 "Result not yet handled")
(defconst DBUS_HANDLER_RESULT_NEED_MEMORY 2 "Result need memory")

(defconst dbus-service-dbus "org.freedesktop.DBus"
  "The bus name used to talk to the bus itself.")

(defconst dbus-path-dbus "/org/freedesktop/DBus"
  "The object path used to talk to the bus itself.")

(defconst dbus-path-local "/org/freedesktop/DBus/Local"
  "The object path used in local/in-process-generated messages.")

(defconst dbus-interface-dbus "org.freedesktop.DBus"
  "The interface exported by the object with `dbus-service-dbus'
and `dbus-path-dbus'.")

(defconst dbus-interface-introspecable "org.freedesktop.DBus.Introspectable"
  "The interface supported by introspectable objects.")

(defconst dbus-interface-properties "org.freedesktop.DBus.Properties"
  "The interface supported by objects with properties.")

(defconst dbus-interface-peer "org.freedesktop.DBus.Peer"
  "The interface supported by most dbus peers.")

(defconst dbus-interface-local "org.freedesktop.DBus.Local"
  "This is a special interface whose methods can only be invoked by
the local implementation (messages from remote apps aren't allowed to
specify this interface). ")

(defconst dbus-name-flag-allow-replacement 1
  "Allow another service to become the primary owner if requested.")

(defconst dbus-name-flag-replace-existing 2
  "Request to replace the current primary owner.")

(defconst dbus-name-flag-do-not-queue 4
  "If we can not become the primary owner do not place us in the queue.")

(defconst dbus-request-name-reply-primary-owner 1
  "Service has become the primary owner of the requested name.")

(defconst dbus-request-name-reply-in-queue 2
  "Service could not become the primary owner and has been placed in
the queue. ")

(defconst dbus-request-name-reply-exists 3
  "Service is already in the queue.")

(defconst dbus-request-name-reply-already-owner 4
  "Service is already the primary owner.")

(defconst dbus-release-name-reply-released 1
  "Service was released from the given name.")

(defconst dbus-release-name-reply-non-existant 2
  "The given name does not exist on the bus.")

(defconst dbus-release-name-reply-not-owner 3
  "Service is not an owner of the given name.")

(defconst dbus-start-reply-success 1
  "Service was auto started.")

(defconst dbus-start-reply-already-running 2
  "Service was already running.")

;;}}}
;;{{{ D-Bus connection functions

(cffi:defcfun ("dbus_bus_get" dbus:bus-get) :pointer
  (type dbus-bus-type)
  (error :pointer))

(cffi:defcfun ("dbus_bus_request_name" dbus:bus-request-name) :int
  (connection :pointer)
  (name :string)
  (flags :uint)
  (error :pointer))

(cffi:defcstruct dbus-object-path-vtable
  (unregister-function :pointer)
  (message-function :pointer)
  (dummy1 :pointer)
  (dummy2 :pointer)
  (dummy3 :pointer)
  (dummy4 :pointer))

(cffi:defcfun ("dbus_connection_send_with_reply_and_block"
	       dbus:connection-send-with-reply-and-block) :pointer
  (connection :pointer)
  (message :pointer)
  (timeout_milliseconds :int)
  (error :pointer))

(cffi:defcfun ("dbus_connection_send" dbus:connection-send) dbus-bool
  (connection :pointer)
  (message :pointer)
  (serial :pointer))

(cffi:defcfun ("dbus_connection_read_write"
	       dbus:connection-read-write) dbus-bool
  (connection :pointer)
  (timeout-milliseconds :int))

(cffi:defcfun ("dbus_connection_read_write_dispatch"
	       dbus:connection-read-write-dispatch) dbus-bool
  (connection :pointer)
  (timeout-milliseconds :int))

(cffi:defcfun ("dbus_connection_pop_message"
	       dbus:connection-pop-message) :pointer
  (connection :pointer))

(cffi:defcfun ("dbus_connection_flush" dbus:connection-flush) :void
  (connection :pointer))

(cffi:defcfun ("dbus_connection_register_object_path"
	       dbus:connection-register-object-path) dbus-bool
  (connection :pointer)
  (path :string)
  (vtable :pointer)
  (user-data :pointer))

;;}}}
;;{{{ Error functions

(cffi:defcstruct dbus-error
  (name :string)
  (message :string)
  (dummy1 :uint)
  (dummy2 :uint)
  (dummy3 :uint)
  (dummy4 :uint)
  (dummy5 :uint)
  (padding :pointer))

(cffi:defcfun ("dbus_error_init" dbus:error-init) :void
  (error :pointer))

(cffi:defcfun ("dbus_error_free" dbus:error-free) :void
  (error :pointer))

(cffi:defcfun ("dbus_error_is_set" dbus:error-is-set) dbus-bool
  (error :pointer))

;;}}}
;;{{{ Message functions

(cffi:defcfun ("dbus_message_new_method_call"
	       dbus:message-new-method-call) :pointer
  (destination :string)
  (path :string)
  (interface :string)
  (method :string))

(cffi:defcfun ("dbus_message_get_args" dbus:message-get-args) dbus-bool
  (message :pointer)
  (error :pointer)
  &rest)

(cffi:defcfun ("dbus_message_append_args"
	       dbus:message-append-args) dbus-bool
  (message :pointer)
  (first-arg-type :int)
  &rest)

(cffi:defcfun ("dbus_message_get_interface"
	       dbus:message-get-interface) :string
  (message :pointer))

(cffi:defcfun ("dbus_message_get_member"
	       dbus:message-get-member) :string
  (message :pointer))

(cffi:defcfun ("dbus_message_get_path"
	       dbus:message-get-path) :string
  (message :pointer))

(cffi:defcfun ("dbus_message_unref" dbus:message-unref) :void
  (message :pointer))

(cffi:defcfun ("dbus_message_is_method_call"
	       dbus:message-is-method-call) dbus-bool
  (message :pointer)
  (interface :string)
  (method :string))

(cffi:defcfun ("dbus_message_iter_init"
	       dbus:message-iter-init) dbus-bool
  (message :pointer)
  (iter :pointer))

(cffi:defcfun ("dbus_message_iter_get_arg_type"
	       dbus:message-iter-get-arg-type) :int
  (iter :pointer))

(cffi:defcfun ("dbus_message_iter_get_basic"
	       dbus:message-iter-get-basic) :void
  (iter :pointer)
  (value :pointer))

(cffi:defcfun ("dbus_message_new_method_return"
	       dbus:message-new-method-return) :pointer
  (method_call :pointer))

(cffi:defcfun ("dbus_message_iter_init_append"
	       dbus:message-iter-init-append) :void
  (message :pointer)
  (iter :pointer))

(cffi:defcfun ("dbus_message_iter_append_basic"
	       dbus:message-iter-append-basic) dbus-bool
  (iter :pointer)
  (type :int)
  (value :pointer))

;;}}}

;;; TODO: elisp-friendly d-bus implementation.
;;   - Use macros, like python-dbus uses its decorators
(defstruct (dbus-connection (:type vector) :named
			    (:print-function
			     (lambda (dc s pl)
			       (setq pl pl) ; steenkin byte-compiler! --SY.
			       (princ (format "#<dbus connection: %s>"
					      (dbus-connection-object dc)) s))))
  bus-name
  object
  ffi-error
  ffi-conn)

(defun dbus-create-connection (object-path bus-name)
  "Create connection to D-Bus.
Return newly created connection structure."
  (let ((dcon (make-dbus-connection :bus-name bus-name
				    :object object-path)))
    (setf (dbus-connection-ffi-error dcon)
	  (cffi:foreign-alloc 'dbus-error))
    (dbus:error-init (dbus-connection-ffi-error dcon))
    (setf (dbus-connection-ffi-conn dcon)
	  (dbus:bus-get DBUS_BUS_SESSION
			(dbus-connection-ffi-error dcon)))
    dcon))

(defmacro* define-dbus-signal (dcon iface signature &rest body)
  "Define new D-Bus signal emiter for interface IFACE.
Optionally you can specify signal SIGNATURE."
  )

(provide 'ffi-dbus)


;;; TESTING:

;;; To toggle gajim's roster use something like
; (let* ((derr (make-ffi-object 'dbus-error))
;        (dcon (progn
;                (dbus:error-init (ffi-address-of derr))
;                (dbus:bus-get DBUS_BUS_SESSION (ffi-address-of derr))))
;       (gajm-meth (dbus:message-new-method-call
;                   "org.gajim.dbus"
;                  "/org/gajim/dbus/RemoteObject"
;                  "org.gajim.dbus.RemoteInterface"
;                  "toggle_roster_appearance")))
;   (unwind-protect
;       (unless (ffi-null-p dcon)
;         (dbus:connection-send dcon gajm-meth -1))
;     (dbus:message-unref gajm-meth)))

;; To get list of services
; (let* ((derr (make-ffi-object 'dbus-error))
;        (dcon (progn
;                (dbus:error-init (ffi-address-of derr))
;                (dbus:bus-get DBUS_BUS_SESSION (ffi-address-of derr)))))
;   (unless (ffi-null-p dcon)
;     (let* ((lmeth (dbus:message-new-method-call
;                    "org.freedesktop.DBus"
;                    "/org/freedesktop/DBus"
;                    "org.freedesktop.DBus"
;                    "ListNames"))
;            (lreply (dbus:connection-send-with-reply-and-block
;                     dcon lmeth -1 (ffi-address-of derr)))
;            (slist (cffi:foreign-alloc :pointer))
;            (slist-len (make-ffi-object :int)))
;       (unless (ffi-null-p lreply)
;         (let ((rargs (dbus:message-get-args
;                       lreply (ffi-address-of derr)
;                       :int 97 :int 115 :pointer slist
;                       :pointer (ffi-address-of slist-len)
;                       :int 0)))
;           (prog1
;               (ffi-get (ffi-get (ffi-get slist))
;                        :type `(array :string ,(ffi-get slist-len)))
;             (dbus:message-unref lmeth)
;             (dbus:message-unref lreply)))))))

;;; ffi-dbus.el ends here
