(require 'gtk-ffi)

(defconst GTK_TYPE_INVALID 0)
(defconst GTK_TYPE_NONE 1)
(defconst GTK_TYPE_CHAR 2)
(defconst GTK_TYPE_UCHAR 3)
(defconst GTK_TYPE_BOOL 4)
(defconst GTK_TYPE_INT 5)
(defconst GTK_TYPE_UINT 6)
(defconst GTK_TYPE_LONG 7)
(defconst GTK_TYPE_ULONG 8)
(defconst GTK_TYPE_FLOAT 9)
(defconst GTK_TYPE_DOUBLE 10)
(defconst GTK_TYPE_STRING 11)
(defconst GTK_TYPE_ENUM 12)
(defconst GTK_TYPE_FLAGS 13)
(defconst GTK_TYPE_BOXED 14)
(defconst GTK_TYPE_POINTER 15)
(defconst GTK_TYPE_SIGNAL 16)
(defconst GTK_TYPE_ARGS 17)
(defconst GTK_TYPE_CALLBACK 18)
(defconst GTK_TYPE_C_CALLBACK 19)
(defconst GTK_TYPE_FOREIGN 20)
(defconst GTK_TYPE_OBJECT 21)

(defconst gtk-value-accessor-names
  '("INVALID" "NONE" "CHAR" "UCHAR" "BOOL" "INT" "UINT" "LONG" "ULONG" "FLOAT" "DOUBLE"
    "STRING" "ENUM" "FLAGS" "BOXED" "POINTER" "SIGNAL" "ARGS" "CALLBACK" "C_CALLBACK"
    "FOREIGN" "OBJECT"))

(defun define-widget-accessors (gtk-class
				wrapper
				prefix args)
  "Output stub C code to access parts of a widget from lisp.
GTK-CLASS is the GTK class to grant access to.
WRAPPER is a fragment to construct GTK C macros for typechecking/etc. (ie: WIDGET)
ARGS is a list of (type . name) cons cells.
Defines a whole slew of functions to access & set the slots in the
structure."
  (set-buffer (get-buffer-create "emacs-widget-accessors.c"))
  (goto-char (point-max))
  (let ((arg)
	(base-arg-type nil)
	(lisp-func-name nil)
	(c-func-name nil)
	(func-names nil))
    (setq gtk-class (symbol-name gtk-class)
	  wrapper (upcase wrapper))
    (while (setq arg (pop args))
      (setq lisp-func-name (format "gtk-%s-%s" prefix (cdr arg))
	    lisp-func-name (replace-in-string lisp-func-name "_" "-")
	    c-func-name (concat "F" (replace-in-string lisp-func-name "-" "_")))
      (insert
       "DEFUN (\"" lisp-func-name "\", " c-func-name ", 1, 1, 0, /*\n"
       "Access the `" (symbol-name (cdr arg)) "' slot of OBJ, a " gtk-class " object.\n"
       "*/\n"
       "\t(obj))\n"
       "{\n"
       (format "\t%s *the_obj = NULL;\n" gtk-class)
       "\tGtkArg arg;\n"
       "\n"
       "\tCHECK_GTK_OBJECT (obj);\n"
       "\n"
       (format "\tif (!GTK_IS_%s (XGTK_OBJECT (obj)->object))\n" wrapper)
       "\t{\n"
       (format "\t\tsignal_simple_error (\"Object is not a %s\", obj);\n" gtk-class)
       "\t};\n"
       "\n"
       (format "\tthe_obj = GTK_%s (XGTK_OBJECT (obj)->object);\n" wrapper)

       (format "\targ.type = gtk_type_from_name (\"%s\");\n" (symbol-name (car arg))))
;       (format "\targ.type = GTK_TYPE_%s;\n" (or
;					       (nth (gtk-fundamental-type (car arg))
;						    gtk-value-accessor-names)
;					       (case (car arg)
;						 (GtkListOfString "STRING_LIST")
;						 (GtkListOfObject "OBJECT_LIST")
;						 (otherwise
;						  "POINTER")))))

      (setq base-arg-type (gtk-fundamental-type (car arg)))
      (cond
       ((= base-arg-type GTK_TYPE_OBJECT)
	(insert
	 (format "\tGTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->%s);"
		 (cdr arg))))
       ((or (= base-arg-type GTK_TYPE_POINTER)
	    (= base-arg-type GTK_TYPE_BOXED))
	(insert
	 (format "\tGTK_VALUE_%s (arg) = (void *)the_obj->%s;"
		 (nth (gtk-fundamental-type (car arg)) gtk-value-accessor-names)
		 (cdr arg))))
       (t
	(insert
	 (format "\tGTK_VALUE_%s (arg) = the_obj->%s;"
		 (or (nth (gtk-fundamental-type (car arg)) gtk-value-accessor-names) "POINTER")
		 (cdr arg)))))
      (insert
       "\n"
       "\treturn (gtk_type_to_lisp (&arg));\n"
       "}\n\n")
      (push c-func-name func-names))
    func-names))

(defun import-widget-accessors (file syms-function-name &rest description)
  "Import multiple widgets, and emit a suitable vars_of_foo() function for them.\n"
  (let ((c-mode-common-hook nil)
	(c-mode-hook nil))
    (find-file file))
  (erase-buffer)
  (let ((c-funcs nil))
    (while description
      (setq c-funcs (nconc (define-widget-accessors
			     (pop description) (pop description)
			     (pop description) (pop description)) c-funcs)))
    (goto-char (point-max))
    (insert "void " syms-function-name " (void)\n"
	    "{\n\t"
	    (mapconcat (lambda (x)
			 (concat "DEFSUBR (" x ");")) c-funcs "\n\t")
	    "\n}"))
  (save-buffer))

;; Because the new FFI layer imports GTK types lazily, we need to load
;; up all of the gtk types we know about, or we get errors about
;; unknown GTK types later on.
(mapatoms (lambda (sym)
	    (if (string-match "gtk-[^-]+-get-type" (symbol-name sym))
		(funcall sym))))

(import-widget-accessors
 "../../src/emacs-widget-accessors.c"
 "syms_of_widget_accessors "

 'GtkAdjustment "ADJUSTMENT" "adjustment"
 '((gfloat . lower)
   (gfloat . upper)
   (gfloat . value)
   (gfloat . step_increment)
   (gfloat . page_increment)
   (gfloat . page_size))

 'GtkWidget "WIDGET" "widget"
 '((GtkStyle     . style)
   (GdkWindow    . window)
   (GtkStateType . state)
   (GtkString    . name)
   (GtkWidget    . parent))

 'GtkButton "BUTTON" "button"
 '((GtkWidget  . child)
   (gboolean   . in_button)
   (gboolean   . button_down))

 'GtkCombo "COMBO" "combo"
 '((GtkWidget  . entry)
   (GtkWidget  . button)
   (GtkWidget  . popup)
   (GtkWidget  . popwin)
   (GtkWidget  . list))

 'GtkGammaCurve "GAMMA_CURVE" "gamma-curve"
 '((GtkWidget  . table)
   (GtkWidget  . curve)
   (gfloat      . gamma)
   (GtkWidget  . gamma_dialog)
   (GtkWidget  . gamma_text))

 'GtkCheckMenuItem "CHECK_MENU_ITEM" "check-menu-item"
 '((gboolean   . active))

 'GtkNotebook "NOTEBOOK" "notebook"
 '((GtkPositionType . tab_pos))

 'GtkText "TEXT" "text"
 '((GtkAdjustment . hadj)
   (GtkAdjustment . vadj))

 'GtkFileSelection "FILE_SELECTION" "file-selection"
 '((GtkWidget . dir_list)
   (GtkWidget . file_list)
   (GtkWidget . selection_entry)
   (GtkWidget . selection_text)
   (GtkWidget . main_vbox)
   (GtkWidget . ok_button)
   (GtkWidget . cancel_button)
   (GtkWidget . help_button)
   (GtkWidget . action_area))

 'GtkFontSelectionDialog "FONT_SELECTION_DIALOG" "font-selection-dialog"
 '((GtkWidget . fontsel)
   (GtkWidget . main_vbox)
   (GtkWidget . action_area)
   (GtkWidget . ok_button)
   (GtkWidget . apply_button)
   (GtkWidget . cancel_button))

 'GtkColorSelectionDialog "COLOR_SELECTION_DIALOG" "color-selection-dialog"
 '((GtkWidget . colorsel)
   (GtkWidget . main_vbox)
   (GtkWidget . ok_button)
   (GtkWidget . reset_button)
   (GtkWidget . cancel_button)
   (GtkWidget . help_button))

 'GtkDialog "DIALOG" "dialog"
 '((GtkWidget . vbox)
   (GtkWidget . action_area))

 'GtkInputDialog "INPUT_DIALOG" "input-dialog"
 '((GtkWidget . close_button)
   (GtkWidget . save_button))

 'GtkPlug "PLUG" "plug"
 '((GdkWindow . socket_window)
   (gint      . same_app))

 'GtkObject "OBJECT" "object"
 '((guint     . flags)
   (guint     . ref_count))

 'GtkPaned "PANED" "paned"
 '((GtkWidget . child1)
   (GtkWidget . child2)
   (gboolean  . child1_resize)
   (gboolean  . child2_resize)
   (gboolean  . child1_shrink)
   (gboolean  . child2_shrink))

 'GtkCList "CLIST" "clist"
 '((gint . rows)
   (gint . columns)
   (GtkAdjustment . hadjustment)
   (GtkAdjustment . vadjustment)
   (GtkSortType   . sort_type)
   (gint . focus_row)
   (gint . sort_column))

 'GtkList "LIST" "list"
 '((GtkListOfObject . children)
   (GtkListOfObject . selection))

 'GtkTree "TREE" "tree"
 '((GtkListOfObject . children)
   (GtkTree         . root_tree)
   (GtkWidget       . tree_owner)
   (GtkListOfObject . selection))

 'GtkTreeItem "TREE_ITEM" "tree-item"
 '((GtkWidget       . subtree))

 'GtkScrolledWindow "SCROLLED_WINDOW" "scrolled-window"
 '((GtkWidget . hscrollbar)
   (GtkWidget . vscrollbar)
   (gboolean  . hscrollbar_visible)
   (gboolean  . vscrollbar_visible))

 )
