;;; cus-face.el -- Support for Custom faces.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: help, faces
;; Version: 1.9960-x
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Synched with: Not synched.

;;; Commentary:
;;
;; See `custom.el'.

;;; Code:

;; it is now safe to put the `provide' anywhere.  if an error occurs while
;; loading, all provides (and fsets) will be undone.  put it first to
;; prevent require/provide loop with custom and cus-face.
(provide 'cus-face)
(require 'custom)

;; To elude the warnings for font functions.
(eval-when-compile
  (require 'font))

;;; Declaring a face.

;;;###autoload
(defun custom-declare-face (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument."
  ;; (when (fboundp 'pureload)
    ;; (error "Attempt to declare a face during dump"))
  ;; #### should we possibly reset force-face here?
  (unless (get face 'face-defface-spec)
    (put face 'face-defface-spec spec)
    (unless (find-face face)
      ;; If the user has already created the face, respect that.
      (let ((value (or (get face 'saved-face) spec))
	    (frames (relevant-custom-frames))
	    frame)
	;; Create global face.
	(make-empty-face face)
	(face-display-set face value nil '(custom))
	;; Create frame local faces
	(while frames
	  (setq frame (car frames)
		frames (cdr frames))
	  (face-display-set face value frame '(custom)))
	(init-face-from-resources face)))
    ;; Don't record SPEC until we see it causes no errors.
    (put face 'face-defface-spec spec)
    (push (cons 'defface face) current-load-list)
    (when (and doc (null (face-doc-string face)))
      (set-face-doc-string face doc))
    (custom-handle-all-keywords face args 'custom-face)
    (run-hooks 'custom-define-hook))
  face)

;;; Font Attributes.

;; Consider adding the stuff in the XML font model here.
(defconst custom-face-attributes
  '((:foreground (color :tag "Foreground"
			:value ""
			:help-echo "Set foreground color.")
		 set-face-foreground face-foreground-name)
    (:background (color :tag "Background"
			:value ""
			:help-echo "Set background color.")
		 set-face-background face-background-name)
    (:size (editable-field :format "Size: %v"
			   :help-echo "Text size (e.g. 9pt or 2mm).")
	   custom-set-face-font-size custom-face-font-size)
    (:family (editable-field :format "Font Family: %v"
			     :help-echo "Name of font family to use (e.g. times).")
	     custom-set-face-font-family custom-face-font-family)
    (:background-pixmap (editable-field :format "Background pixmap: %v"
					:help-echo "Name of background pixmap file.")
	      set-face-background-pixmap custom-face-background-pixmap)
    (:dim (toggle :format "%[Dim%]: %v\n"
		  :help-echo "Control whether the text should be dimmed.")
	  set-face-dim-p face-dim-p)
    (:bold (toggle :format "%[Bold%]: %v\n"
		   :help-echo "Control whether a bold font should be used.")
	   custom-set-face-bold custom-face-bold)
    (:italic (toggle :format "%[Italic%]: %v\n"
		     :help-echo "Control whether an italic font should be used.")
	     custom-set-face-italic custom-face-italic)
    (:underline (toggle :format "%[Underline%]: %v\n"
			:help-echo "Control whether the text should be underlined.")
		set-face-underline-p face-underline-p)
    (:strikethru (toggle :format "%[Strikethru%]: %v\n"
			 :help-echo "Control whether the text should be strikethru.")
		 set-face-strikethru-p face-strikethru-p)
    (:inverse-video (toggle :format "%[Inverse Video%]: %v\n"
			    :help-echo "\
Control whether the text should be inverted.  Works only on TTY-s")
		    set-face-reverse-p face-reverse-p)
    (:inherit
     (repeat :tag "Inherit"
	     :help-echo "List of faces to inherit attributes from."
	     (face :Tag "Face" default))
     ;; FSF 21.3
;      ;; filter to make value suitable for customize
;      (lambda (real-value)
;        (cond ((or (null real-value) (eq real-value 'unspecified))
;	      nil)
;	     ((symbolp real-value)
;	      (list real-value))
;	     (t
;	      real-value)))
;      ;; filter to make customized-value suitable for storing
;      (lambda (cus-value)
;        (if (and (consp cus-value) (null (cdr cus-value)))
;	   (car cus-value)
;	 cus-value))
     custom-set-face-inherit custom-face-inherit))
  "Alist of face attributes.

The elements are lists of the form (KEY TYPE SET GET) where:
 KEY is a symbol identifying the attribute.
 TYPE is a widget type for editing the attribute.
 SET is a function for setting the attribute value.
 GET is a function for getting the attribute value.

The SET function should take three arguments: the face to modify, the
value of the attribute, and optionally the frame where the face should
be changed.

The GET function should take two arguments, the face to examine, and
optionally the frame where the face should be examined.")

(defun face-custom-attributes-set (face frame tags &rest atts)
  "For FACE on FRAME set the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, set the default face."
  (while atts
    (let* ((name (nth 0 atts))
	   (value (nth 1 atts))
	   (fun (nth 2 (assq name custom-face-attributes))))
      (setq atts (cdr (cdr atts)))
      (condition-case nil
	  (funcall fun face value frame tags)
	(error nil)))))

(defun face-custom-attributes-get (face frame)
  "For FACE on FRAME get the attributes [KEYWORD VALUE]....
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, use the default face."
  (condition-case nil
      ;; Attempt to get `font.el' from w3.
      (require 'font)
    (error nil))
  (let ((atts custom-face-attributes)
	att result get)
    (while atts
      (setq att (car atts)
	    atts (cdr atts)
	    get (nth 3 att))
      (condition-case nil
	  ;; This may fail if w3 doesn't exist.
	  (when get
	    (let ((answer (funcall get face frame)))
	      (unless (equal answer (funcall get 'default frame))
		(when (widget-apply (nth 1 att) :match answer)
		  (setq result (cons (nth 0 att) (cons answer result)))))))
	(error nil)))
    result))

(defsubst custom-face-get-spec (symbol)
  (or (get symbol 'customized-face)
      (get symbol 'saved-face)
      (get symbol 'face-defface-spec)
      ;; Attempt to construct it.
      (list (list t (face-custom-attributes-get
		     symbol (selected-frame))))))

(defun custom-set-face-bold (face value &optional frame tags)
  "Set the bold property of FACE to VALUE."
  (if value
      (make-face-bold face frame tags)
    (make-face-unbold face frame tags)))

;; Really, we should get rid of these font.el dependencies...  They
;; are still presenting a problem with dumping the faces (font.el is
;; too bloated for us to dump).  I am thinking about hacking up
;; font-like functionality myself for the sake of this file.  It will
;; probably be to-the-point and more efficient.

(defun custom-face-bold (face &rest args)
  "Return non-nil if the font of FACE is bold."
  (let* ((font (apply 'face-font-name face args))
	 ;; Gag
	 (fontobj (font-create-object font)))
    (font-bold-p fontobj)))

(defun custom-set-face-italic (face value &optional frame tags)
  "Set the italic property of FACE to VALUE."
  (if value
      (make-face-italic face frame tags)
    (make-face-unitalic face frame tags)))

(defun custom-face-italic (face &rest args)
  "Return non-nil if the font of FACE is italic."
  (let* ((font (apply 'face-font-name face args))
	 ;; Gag
	 (fontobj (font-create-object font)))
    (font-italic-p fontobj)))

(defun custom-face-background-pixmap (face &rest args)
  "Return the name of the background pixmap file used for FACE."
  (let ((image  (apply 'specifier-instance
		       (face-background-pixmap face) args)))
    (and image
	 (image-instance-file-name image))))

(defun custom-set-face-inherit (face value &optional frame tags)
  "Set FACE to inherit its properties from another face."
  (if (listp value) (setq value (car value))) ;; #### Temporary hack!
  (if (find-face value)
      (set-face-parent face value frame tags)))

(defun custom-face-inherit (face &rest args)
  "Return the value (instance) of the `inherit' property for FACE."
  ;; #### Major, temporary hack!
  (let ((spec (apply 'specifier-instantiator
		     (face-font face) args)))
    (and spec (vector spec) (aref spec 0))))

;; This consistently fails to dtrt
;;(defun custom-set-face-font-size (face size &optional locale tags)
;;  "Set the font of FACE to SIZE."
;;  ;; #### should this call have tags in it?
;;  (let* ((font (apply 'face-font-name face (list locale)))
;;	 ;; Gag
;;	 (fontobj (font-create-object font)))
;;    (set-font-size fontobj size)
;;    (apply 'font-set-face-font face fontobj locale tags)))

;; From Jan Vroonhof -- see faces.el
(defun custom-set-face-font-size (face size &optional locale tags)
  "Set the font of FACE to SIZE."
  (make-face-size face size locale tags))

(defun custom-face-font-size (face &rest args)
  "Return the size of the font of FACE as a string."
  (let* ((font (apply 'face-font-name face args))
	 ;; Gag
	 (fontobj (font-create-object font)))
    (format "%s" (font-size fontobj))))

;; Jan suggests this may not dtrt
;;(defun custom-set-face-font-family (face family &optional locale tags)
;;  "Set the font of FACE to FAMILY."
;;  ;; #### should this call have tags in it?
;;  (let* ((font (apply 'face-font-name face (list locale)))
;;	 ;; Gag
;;	 (fontobj (font-create-object font)))
;;    (set-font-family fontobj family)
;;    (apply 'font-set-face-font face fontobj locale tags)))

;; From Jan Vroonhof -- see faces.el
(defun custom-set-face-font-family (face family &optional locale tags)
  "Set the font of FACE to FAMILY."
  (make-face-family face family locale tags))

(defun custom-face-font-family (face &rest args)
  "Return the name of the font family of FACE."
  (let* ((font (apply 'face-font-name face args))
	 ;; Gag
	 (fontobj (font-create-object font)))
    (font-family fontobj)))

;;;###autoload
(defun custom-set-face-update-spec (face display plist)
  "Customize the FACE for display types matching DISPLAY, merging
  in the new items from PLIST."
  (let ((spec (face-spec-update-all-matching (custom-face-get-spec face)
					     display plist)))
    (put face 'customized-face spec)
    (face-spec-set face spec nil '(custom))))

;;; Initializing.

;;;###autoload
(defun custom-set-faces (&rest args)
  "Initialize faces according to user preferences.
This asociates the setting with the USER theme.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW [COMMENT]])

SPEC will be stored as the saved value for FACE.  If NOW is present
and non-nil, FACE will also be created according to SPEC.
COMMENT is a string comment about FACE.

See `defface' for the format of SPEC."
  (apply #'custom-theme-set-faces 'user args))

;;;###autoload
(defun custom-theme-set-faces (theme &rest args)
  "Initialize faces according to settings specified by args.
Records the settings as belonging to THEME.

See `custom-set-faces' for a description of the arguments ARGS."
  (custom-check-theme theme)
  (let ((immediate (get theme 'theme-immediate)))
    (while args
      (let ((entry (car args)))
	(if (listp entry)
	    (let ((face (nth 0 entry))
		  (spec (nth 1 entry))
		  (now (nth 2 entry))
		  (comment (nth 3 entry)))
	      (put face 'saved-face spec)
	      (custom-push-theme 'theme-face face theme 'set spec)
	      (put face 'saved-face-comment comment)
	      (when (or now immediate)
		(put face 'force-face (if now 'rogue 'immediate)))
	      (when (or now immediate (find-face face))
		(put face 'face-comment comment)
		(unless (find-face face)
		  (make-empty-face face))
		(face-spec-set face spec nil '(custom)))
	      (setq args (cdr args)))
	  ;; Old format, a plist of FACE SPEC pairs.
	  (let ((face (nth 0 args))
		(spec (nth 1 args)))
	    (put face 'saved-face spec)
	    (custom-push-theme 'theme-face face theme 'set spec))
	  (setq args (cdr (cdr args))))))))

;;;###autoload
(defun custom-theme-face-value (face theme)
  "Return spec of FACE in THEME if the THEME modifies the
FACE.  Nil otherwise."
  (car-safe (custom-theme-value theme (get face 'theme-face))))

(defun custom-theme-reset-internal-face (face to-theme)
  (let ((spec (custom-theme-face-value face to-theme))
	was-in-theme)
    (setq was-in-theme spec)
    (setq spec (or spec (get face 'standard-value)))
    (when spec
      (put face 'save-face was-in-theme)
      (when (or (get face 'force-face) (find-face face))
	      (unless (find-face face)
		(make-empty-face face))
	      (face-spec-set face spec)))
    spec))

;;;###autoload
(defun custom-theme-reset-faces (theme &rest args)
  "Reset the value of the face to values previously defined.
Associate this setting with THEME.

ARGS is a list of lists of the form

    (face to-theme)

This means reset face to its value in to-theme."
  (custom-check-theme theme)
  (mapc #'(lambda (arg)
	    (apply #'custom-theme-reset-internal-face arg)
	    (custom-push-theme (car arg) 'theme-face theme 'reset (cadr arg)))
	args))

;;;###autoload
(defun custom-reset-faces (&rest args)
  "Reset the value of the face to values previously defined.
Associate this setting with the 'user' theme.

ARGS is defined as for `custom-theme-reset-faces'."
  (apply #'custom-theme-reset-faces 'user args))


;;; The End.

;; cus-face.el ends here
