(defvar packages-hardcoded-lisp
  '(
    ;; Nothing at this time
    )
  "Lisp packages that are always dumped with XEmacs.
This includes every package that is loaded directly by a package listed
in dumped-lisp.el and is not itself listed.")

(setq preloaded-file-list
      (assemble-list
	"backquote"		; needed for defsubst etc.
	"bytecomp-runtime"	; define defsubst
	"find-paths"
	"packages"		; Bootstrap run-time lisp environment
	"setup-paths"
	"dump-paths"
	"subr"			; load the most basic Lisp functions
	"replace"		; match-string used in version.el.
	; Ignore compiled-by-mistake version.elc
	"version.el"
	"cl"
	"cl-extra"
	"cl-seq"
	"widget"
	"custom"		; Before the world so everything can be
				; customized
	"cus-start"		; for customization of builtin variables
	"cmdloop"
	"keymap"
	"syntax"
	"device"
	"console"
	"obsolete"
	"specifier"
	"faces"			; must be loaded before any make-face call
;;(pureload "facemenu") #### not yet ported
	"glyphs"
	"objects"
	"extents"
	"events"
	"text-props"
	"process" ;; This is bad. network-streams may not be defined.
	(when-feature multicast "multicast") ; #+network-streams implicitly true
	"frame"			; move up here cause some stuff needs it here
	"map-ynp"
	"undo-stack"
	"window"
	"window-xemacs"
	"resize-minibuffer"	; Needed by simple
	"simple"
	"keydefs"		; Before loaddefs so that keymap vars exist.
	"abbrev"
	"number"
	"derived"
	"minibuf"
	"list-mode"
	"modeline"		; needs simple.el to be loaded first
;; If SparcWorks support is included some additional packages are
;; dumped which would normally have autoloads.  To avoid
;; duplicate doc string warnings, SparcWorks uses a separate
;; autoloads file with the dumped packages removed.
;; After fixing, eos/loaddefs-eos and loaddefs appear identical?!!
;; So just make loaddefs-eos go away...
;;(pureload (if (featurep 'sparcworks) "eos/loaddefs-eos" "loaddefs"))
	"cus-file"
	"startup"		; For initialization of
				;  `emacs-user-extension-dir'
	"lisp-initd"
	"misc"
	"loadhist"		; Must be dumped before loaddefs is loaded
				; Used by help.
	;; (pureload "profile")
	(unless-feature mule "help-nomule")
	"help"
	;; (pureload "hyper-apropos")  Soon...
	(unless-feature file-coding "files-nomule")
	"files"
	"lib-complete"
	(when-feature modules "emod-utils")
	"format"
	"indent"
	"isearch-mode"
	"buffer"
	"buff-menu"
	"paths.el"		; don't get confused if paths compiled.
	"lisp"
	"page"
	"register"
	"iso8859-1"		; This must be before any modes
					; (sets standard syntax table.)
	"paragraphs"
	"easymenu"		; Added for 20.3.
	"lisp-mode"
	"text-mode"
	"fill"
	"auto-save"		; Added for 20.4
	"movemail"              ; Added for 21.2
	(when-feature lisp-float-type "float-sup")
	"itimer"		; for vars auto-save-timeout and
				; auto-gc-threshold
	"itimer-autosave"
	"printer"

	;;;;;;;;;;;;;;;;;; GUI support
	(when-feature window-system "gui")
	(when-feature window-system "mouse")
	(when-feature window-system "mode-motion")
	(when-feature toolbar "toolbar")
	(when-feature scrollbar "scrollbar")
	(when-feature menubar "menubar")
	(when-feature dialog "dialog")
	(when-feature gutter "gutter")
	(when-feature dragdrop-api "dragdrop")
	"select"

	;;;;;;;;;;;;;;;;;; Content for GUI's
	;; There used to be window-system inserted in the when-feature,
	;; but IMHO your configure script should turn off the menubar,
	;; toolbar, etc. features when there is no window system.  We
	;; should just be able to assume that, if (featurep 'menubar),
	;; the menubar should work and if items are added, they can be
	;; seen clearly and usefully.
	(when-feature (and (not infodock) menubar) "menubar-items")
	(when-feature (and gutter) "gutter-items")
	(when-feature (and (not infodock) toolbar) "toolbar-items")
	(when-feature (and (not infodock) dialog) "dialog-items")

	;;;;;;;;;;;;;;;;;; Coding-system support
	(when-feature file-coding "coding")
	(when-feature file-coding "code-files")
	;; Handle process with encoding/decoding coding-system.
	(when-feature file-coding "code-process")
	;; Provide basic commands to set coding systems to user
	(when-feature file-coding "code-cmds")
	;;;;;;;;;;;;;;;;;; MULE support
	(when-feature mule "mule-charset")
	(when-feature mule "mule-coding")
	;; All files after this can have extended characters in them.
	(when-feature mule "mule-help")
	(when-feature mule "mule-category")
	(when-feature mule "mule-misc")
	(when-feature mule "kinsoku")
	(when-feature (and mule x) "mule-x-init")
	(when-feature (and mule tty) "mule-tty-init")
	(when-feature mule "mule-cmds") ; to sync with Emacs 20.1

;; after this goes the specific lisp routines for a particular input system
;; 97.2.5 JHod Shouldn't these go into a site-load file to allow site
;; or user switching of input systems???
;(if (featurep 'wnn)
;    (progn
;      (pureload "egg")
;      (pureload "egg-wnn")
;      (setq egg-default-startup-file "eggrc-wnn")))

;; (if (and (boundp 'CANNA) CANNA)
;;     (pureload "canna")
;;   )

;; Now load files to set up all the different languages/environments
;; that Mule knows about.

	(when-feature mule "arabic")
	(when-feature mule "chinese")
	(when-feature mule "mule/cyrillic") ; overloaded in leim/quail
	(when-feature mule "english")
	(when-feature mule "ethiopic")
	(when-feature mule "european")
	(when-feature mule "mule/greek") ; overloaded in leim/quail
	(when-feature mule "hebrew")
	(when-feature mule "japanese")
	(when-feature mule "korean")
	(when-feature mule "latin")
	(when-feature mule "misc-lang")
	(when-feature mule "thai-xtis-chars")
	(when-feature mule "mule/thai-xtis") ; overloaded in leim/quail
	(when-feature mule "viet-chars")
	(when-feature mule "vietnamese")

	;; Specialized language support
	(when-feature (and mule CANNA) "canna-leim")
;; Egg/Its is now a package
;	(when-feature (and mule wnn) "egg-leim")
;	(when-feature (and mule wnn) "egg-kwnn-leim")
;	(when-feature (and mule wnn) "egg-cwnn-leim")
;	(when-feature mule "egg-sj3-leim")
;; SKK is now a package
;	(when-feature mule "skk-leim")

;; Set up the XEmacs environment for Mule.
;; Assumes the existence of various stuff above.
	(when-feature mule "mule-init")

;; Enable Mule capability for Gnus, mail, etc...
;; Moved to sunpro-load.el - the default only for Sun.
;;(pureload "mime-setup")
;;; mule-load.el ends here

;; preload InfoDock stuff.  should almost certainly not be here if
;; id-menus is not here.  infodock needs to figure out a clever way to
;; advise this stuff or we need to export a clean way for infodock or
;; others to control this programmatically.
	(when-feature (and infodock x menubar) "id-menus")
;; preload the X code.
	(when-feature x "x-faces")
	(when-feature x "x-iso8859-1")
	(when-feature x "x-mouse")
	(when-feature x "x-select")
	(when-feature (and x scrollbar) "x-scrollbar")
	(when-feature x "x-misc")
	(when-feature x "x-init")
	(when-feature x "x-win-xfree86")
	(when-feature x "x-win-sun")

;; preload the TTY init code.
	(when-feature tty "tty-init")
	"x-color"
	"cus-face"
	"font-lock"
	"fontl-hooks"
	"auto-show"
	(when-feature ldap "ldap")

	"loaddefs"		; <=== autoloads get loaded here
))
