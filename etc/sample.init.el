;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2000, 2001 Ben Wing.

;; Author: Mostly Ben Wing <ben@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: sample, initialization

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; #### to do:
;; -- scan for #### markers and fix the problems noted there.
;; -- #### maybe the setqs in this file should be changed to defvars
;;    to avoid tromping on customizations when custom.el is loaded
;;    early (dv and sjt at least favor making this the default)
;; -- #### update documentation in (lispref)Starting Up XEmacs, in
;;    (xemacs)Entering Emacs, and in (custom), then point to them
;;    instead of going into detail here.

;;; This is a sample init file.  It can be used without modification
;;; as your init.el or .emacs.  In older versions of XEmacs, this file
;;; was called .emacs and placed in your home directory. (Under MS
;;; Windows, that directory is controlled by the HOME environment
;;; variable and defaults to C:\.  You can find out where XEmacs
;;; thinks your home directory is using
;;;
;;;   ESC : (expand-file-name "~")
;;;
;;; .  This means type ESC, then colon, then the following text, then hit
;;; return.) In more recent versions of XEmacs, this file has migrated to
;;; the .xemacs/ subdirectory and is called init.el.  Other files are
;;; also located here, such as custom.el (the auto-generated file
;;; containing Customization options that you saved when using
;;; Options->Save Options).

;;; Changes to your init.el file will not take effect until the next
;;; time you start up XEmacs, unless you load it explicitly with
;;;
;;;   M-x load-file RET ~/.xemacs/init.el RET

;;; The language that this file (and most other XEmacs init files) is
;;; written in is called "XEmacs Lisp" or more commonly "Elisp".

;;; Brief descriptions of how the init process works and how to
;;; accomplish many useful customizations are given below in this
;;; file.  There are many sources of further information:

;;; -- the XEmacs User's Manual (Access using the online Info browser:
;;;       Use `Help->Info (Online Docs)->XEmacs User's Manual' (if
;;;       there is such an entry); or get to the Info contents page
;;;       using `Help->Info Contents' or `C-h i', and then
;;;       *middle-click* the XEmacs link or move the cursor into the
;;;       link and hit ENTER.  This manual contains a great deal of
;;;       documentation on customization: Scroll down to the
;;;       Customization link and select it in the same fashion as for
;;;       the XEmacs link just mentioned.)

;;; -- the XEmacs FAQ (`C-h F' for the local version; get either the
;;;       local version or the very latest version off the net using
;;;       the Help menu)

;;; -- the XEmacs Lisp Reference Manual, containing detailed
;;;       documentation on Elisp. (Access using Info, just like for the
;;;       XEmacs User's Manual.)

;;; -- the documentation strings for specific commands, functions,
;;;       key sequences, and variables.  NOTE: This is *not* the same
;;;       information as in the XEmacs User's Manual or XEmacs Lisp
;;;       Reference Manual!  In general, the doc strings are more
;;;       terse and more up-to-date than what is found in the manuals.
;;;       Once you understand the general concepts, these doc strings
;;;       should be your first point of reference for further
;;;       info. (Access using menu entries under `Help->Commands,
;;;       Variables, Keys' or using the keyboard: `C-h k' for a key
;;;       sequence, `C-h f' for a named command or Elisp function,
;;;       `C-h v' for a variable.  There is various other useful
;;;       information accessible similarly, such as `C-h a'
;;;       ["Apropos", i.e. search for a command, function, or variable
;;;       by name]; `C-h C-a' ["Apropos Docs", i.e. search through the
;;;       text of the doc strings]; `C-h b' to list all key bindings;
;;;       `C-h m' to describe the current major and minor modes; etc.
;;;       Type `C-h ? ?' for a complete list.)

;;; -- Getting Started with XEmacs [aka the "New User's Guide"], a
;;;       more introductory manual than the XEmacs User's Manual.
;;;       (Access using Info, just like for the XEmacs User's Manual.
;;;       There are some sections on customization here.)

;;; -- the XEmacs tutorial, a very simple introduction to XEmacs for
;;;       total beginners. (`C-h t' for English; get the version in
;;;       various languages from the Help menu)

;;; -- the XEmacs web site, www.xemacs.org.

;;; -- the XEmacs mailing lists (xemacs-FOO@xemacs.org;
;;;       see http://www.xemacs.org/Lists/ for more info.  Before
;;;       posting, consider looking through the archives -- they go back
;;;       years and there is a powerful searching interface.  Currently
;;;       the archives are at http://list-archive.xemacs.org/, but if
;;;       this doesn't work, you can always access them through
;;;       www.xemacs.org.)

;;; -- the XEmacs newsgroup, comp.emacs.xemacs.  This is
;;;       bi-directionally gatewayed with xemacs@xemacs.org.  WARNING:
;;;       The developers do not normally hang out on this newsgroup.  If
;;;       you need to contact them, use xemacs-beta@xemacs.org.

;;; -- the XEmacs internals manual, for those interested in working on
;;;       the XEmacs C code. (Available through Info.)

;;; -- `Help->About XEmacs' to find out who the maintainers are.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Theory of Operation                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; XEmacs allows you to make persistent changes to editor behavior by
;;; saving code in files which are by default loaded at startup.

;; These files are just Lisp libraries with names built in to XEmacs.
;; There are files for the use of the user (the init file and the
;; custom file), for the site administrator (default.el and
;; site-start.el), and for the XEmacs maintainers (auto-autoloads
;; files).  See the Lispref for user and site files (node Starting Up
;; XEmacs, currently inaccurate (it doesn't describe the custom
;; file)).  Interactions among the files are complex; see
;; lisp/startup.el for details.

;; Briefly, after very basic initializations including processing a
;; special command line options (including GUI toolkit options),
;; setting up the terminal, and setting up `load-path', it executes
;; customization code as follows:

;; 1. It runs the normal hook `before-init-hook'.
;; 2. It loads the library `site-start' (by default `site-start.el').
;; 3. It loads the init file (by default `~/.xemacs/init.el').
;; 4. It loads the custom file (by default `~/.xemacs/custom.el').
;; 5. It loads the library `default' (by default `default.el').
;; 6. It runs the normal hook `after-init-hook'.

;; After this the *scratch* buffer is set up and the remaining command
;; line arguments (actions and file names) are processed.

;; N.B. Switching the order of steps 3 and 4 is under discussion and
;; favored by several core developers.

;; Step 2 is inhibited by the -no-site-file command line switch.
;; Steps 3 and 4 are inhibited (as a unit) by the -no-init-file
;; command line switch (-q is a convenient synonym).  Step 5 is
;; inhibited by -no-init-file or a non-nil value of
;; `inhibit-default-init' (set it in the init file).  From now on the
;; hooks and the site initialization files will be ignored.

;; The custom file and the init file contain customizations managed by
;; XEmacs itself via the Custom subsystem and manual customizations,
;; respectively.  Originally both were placed in the same file,
;; usually ~/.emacs, but occasionally XEmacs would trash user settings
;; when automatically changing options, and more frequently users
;; would trash the automatically generated code.  So these functions
;; have been reallocated to separate files, usually named custom.el
;; and init.el, respectively.

;; The Custom system is accessed most conveniently from the
;; Options->Advanced (Customize) menu (also, the Options->Fonts and
;; Options->Sizes menus are implicitly managed by Custom, and
;; Options->Edit Faces explicitly invokes Custom).  You can also use
;; the suite of customize commands directly (cf C-h a customize RET).
;; Currently, Custom possesses specialized facilities for setting
;; ordinary variables of many types, and for customizing faces.  As a
;; general rule, variable and face initialization should be done using
;; Custom, and other initializations should be done in the init file.

;; A possible exception is a subsystem with its own complex init file,
;; eg, Gnus and .gnus.  In these cases it is often preferable to keep
;; even simple variable initializations together, and you may wish to
;; maintain these configurations by hand.

;; You should avoid editing the custom file by hand.  The syntax used
;; is complex but concise, and it is easy to silently break the whole
;; file with a single error that happens to result in a valid Lisp
;; form.  On the other hand, the init file is just a Lisp library that
;; is loaded before starting the read-eval-redisplay loop.

;; The interactions between the custom file and other init files are
;; governed by a simple idea:

;; Custom to User:  ALL VARIABLES YOURS OURS NOW ARE.

;; To be precise, Custom is pretty good about noticing and respecting
;; existing settings in interactive use.  However, it is weak in
;; understanding advanced use of specifier variables (these are used
;; for customizations which depend on display characteristics and
;; configuration in complex ways), and can be quite brutal at
;; initialization.

;; Normal practice for Custom at initialization is to (1) reset all
;; customized faces before applying customizations and (2) force all
;; variables to the values specified in custom.el.  For this reason,
;; and because it is generally the case that the init file can
;; usefully depend on customized variables, but Custom pays no
;; attention to behavior of the init file, it is probably a good idea
;; to force custom.el to be loaded before the init file.  (As
;; mentioned, this will probably become the default in future versions
;; of XEmacs.)

;; To enable early loading of custom.el, uncomment the following line:
;(setq Init-inhibit-custom-file-p (not (assoc custom-file load-history)))

;; Code to implement early loading where late loading is the default.
;; A crucial snippet of code must be the last thing in this file.

;; defvars only initialize uninitialized variables; if the setq above
;; is active, the variable below is defined but the value will not be
;; altered.
(defvar Init-inhibit-custom-file-p nil
  "Internal user init flag.  Don't use this yourself.

Non-nil if we need to inhibit XEmacs from loading custom.el after init.el.")

(when Init-inhibit-custom-file-p
  ;; This is the default custom-file.
  (let ((file (expand-file-name "~/.xemacs/custom.el")))
    (add-one-shot-hook 'after-init-hook
		       `(lambda () (setq custom-file ,file)))
    (cond ((file-readable-p file)
	   (load file))
	  ((file-exists-p file)
	   (warn "Existing custom file \"%s\" is not readable!" file)))
    (cond ((not (file-exists-p file))
	   (display-warning ' resource
	     (format "Custom file \"%s\" not found." file)
	     'info))
	  ((not (file-writable-p file))
	   (warn "Existing custom file \"%s\" is not writable!" file)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Basic Customization                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TIP: Control-L characters are ignored in Lisp files and are the
;; standard way of indicating major section divisions.  You can enter
;; such a character using C-q C-l.

;; Define a variable to indicate whether we're running XEmacs/Lucid
;; Emacs.  (You do not have to defvar a global variable before using
;; it -- you can just call `setq' directly.  It's clearer this way,
;; though.  Note also how we check if this variable already exists
;; using `boundp', because it's defined in recent versions of
;; XEmacs.)

(or (boundp 'running-xemacs)
    (defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)))

;; Define a function to make it easier to check which version we're
;; running.  This function already exists in recent XEmacs versions,
;; and in fact all we've done is copied the definition.  Note again
;; how we check to avoid clobbering an existing definition. (It's good
;; style to do this, in case some improvement was made to the
;; already-existing function -- otherwise we might substitute an older
;; definition and possibly break some code elsewhere.)
;;
;; NOTE ALSO: It is in general *NOT* a good idea to do what we're
;; doing -- i.e. provide a definition of a function that is present in
;; newer versions of XEmacs but not older ones.  The reason is that it
;; may confuse code that notices the presence of the function and
;; proceeds to use it and other functionality that goes along with it
;; -- but which we may not have defined.  What's better is to create
;; the function with a different name -- typically, prefix it with the
;; name of your module, which in this case might be `Init-'.  For
;; `emacs-version>=' we make an exception because (a) the function has
;; been around a long time, (b) there isn't really any other
;; functionality that is paired with it, (c) it's definition hasn't
;; changed and isn't likely to, and (d) the calls to `emacs-version>='
;; or its renamed replacement would be scattered throughout the code
;; below, and with a replacement name the code would become
;; significantly less portable into someone else's init.el file. (BUT
;; NOTE BELOW: We do follow the procedure outlined above with renaming
;; in a different case where the specifics are much different.)
;;
;; TIP: At this point you may be wondering how I wrote all these nice,
;; long, nicely-justified textual stretches -- didn't I go crazy
;; sticking in the semicolons everywhere and having to delete them and
;; rearrange everything whenever I wanted to make any corrections to
;; the text?  The answer is -- of course not!  Use M-q.  This does all
;; the magic for you, justifying and breaking lines appropriately and
;; putting any necessary semicolons or whatever at the left (it
;; figures out what this ought to be by looking in a very clever
;; fashion at what's already at the beginning of each line in the
;; paragraph).  You may need `filladapt' set up (it's done below in
;; this file) in order for this to work properly.  Finally, if you
;; want to turn on automatic filling (like in a word processor, but
;; not quite as automatic), use M-x auto-fill-mode or the binding set
;; up below in this file (Meta-F9).

(or (fboundp 'emacs-version>=)
    (defun emacs-version>= (major &optional minor patch)
      "Return true if the Emacs version is >= to the given MAJOR, MINOR,
   and PATCH numbers.
The MAJOR version number argument is required, but the other arguments
argument are optional. Only the Non-nil arguments are used in the test."
      (let ((emacs-patch (or emacs-patch-level emacs-beta-version -1)))
	(cond ((> emacs-major-version major))
	      ((< emacs-major-version major) nil)
	      ((null minor))
	      ((> emacs-minor-version minor))
	      ((< emacs-minor-version minor) nil)
	      ((null patch))
	      ((>= emacs-patch patch))))))

;; 19.13 was released ages ago (Sep. 1995), and lots of graphic and
;; window-system stuff doesn't work before then.

(or (not running-xemacs)
    (emacs-version>= 19 13)
    (error "This init file does not support XEmacs before 19.13"))

;; Here are some example code snippets that you can use if you need to
;; conditionalize on a particular version of Emacs (in general, though,
;; it is much better to use `fboundp', `featurep', or other such
;; feature-specific checks rather than version-specific checks):

; (cond ((and running-xemacs
; 	    (emacs-version>= 21 2))
;        ;;
;        ;; Code requiring XEmacs version 21.2 or newer goes here
;        ;;
;        ))

; (cond ((emacs-version >= 19 0)
;        ;;
;        ;; Code for any vintage-19 Emacs goes here
;        ;;
;        ))

; (cond ((and (not running-xemacs)
; 	    (emacs-version>= 20 0))
;        ;;
;        ;; Code specific to GNU Emacs 20 or newer (not XEmacs) goes here
;        ;;
;        ))

(defun Init-safe-require (feat)
"Try to REQUIRE the specified feature.  Errors occurring are silenced.
\(Perhaps in the future there will be a way to get at the error.)
Returns t if the feature was successfully required."
  (condition-case nil
      (progn (require feat) t)
    (error nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Key Definitions                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up the function keys to do common tasks to reduce Emacs pinky
;;; and such.

;; You can set a key sequence either to a command or to another key
;; sequence. (Use `C-h k' to map a key sequence to its command.  Use
;; `C-h w' to go the other way.) In general, however, it works better
;; to specify the command name.  For example, it does not currently
;; work to say

;;   (global-set-key 'f5 "\C-x\C-f")

;; The reason is that macros (which is what the string on the right
;; really is) can't currently use the minibuffer.  This is an
;; extremely longstanding bug in Emacs.  Eventually, it will be
;; fixed. (Hopefully ..)

;; Note also that you may sometimes see the idiom

;;   (define-key global-map ...)

;; in place of (global-set-key ...).  These are exactly the same.

;; Here I've tried to put all the most common commands on simple
;; non-modifier function keys to take the pressure off your modifier
;; fingers.  Furthermore, on my keyboard at least, the function keys
;; are grouped into three groups of four with spaces between them, and
;; so it's easier to hit the keys at the edge of the groups --
;; i.e. f1, f4, f5, f8, f9, and f12.  Finally, you may note that f9,
;; f11, and f12 are purposely left blank. [F6 is defined below.]
;; That's because I use them for _, {, and } -- see below.

(global-set-key 'f1 'advertised-undo) ;; Undo
(global-set-key 'f2 'kill-primary-selection) ;; Cut
(global-set-key 'f3 'copy-primary-selection) ;; Copy
(global-set-key 'f4 'yank-clipboard-selection) ;; Paste
(global-set-key 'f5 'find-file) ;; C-x C-f
(global-set-key 'f7 'save-buffer) ;; C-x C-s

;; I considered having this retain the current column after killing
;; the line, but that messes up the common idiom `f8 move-cursor f4'.

(defun Init-kill-entire-line (&optional arg)
"Kill the entire line.
With prefix argument, kill that many lines from point.  Negative
arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "*P")
  (let ((kill-whole-line t))
    (beginning-of-line)
    (call-interactively 'kill-line)))

(global-set-key 'f8
  (if (fboundp 'kill-entire-line) 'kill-entire-line 'Init-kill-entire-line))

;; A keystroke repeated incredible amounts of times.  We need to patch
;; into the isearch keymap so that repeat searches while in isearch
;; mode still work.  Here we show how to make a key in a keymap have the
;; same binding as another key in the keymap, without knowing what the
;; binding is in advance; instead, we find it with `lookup-key'.  This
;; way, if the binding of C-s changes (e.g. to a different function) but
;; the meaning is basically the same, we automatically do the right thing.
;; If we put in the actual binding, which is 'isearch-repeat-forward,
;; this automatic tracking wouldn't happen.
;;
;; TIP: To find out what the (lookup-key ...) expression evaluates to,
;; move just to the right of the closing paren and type C-x C-e.

(global-set-key 'f10 'isearch-forward)
(define-key isearch-mode-map 'f10 (lookup-key isearch-mode-map "\C-s"))
(define-key minibuffer-local-isearch-map 'f10
  (lookup-key minibuffer-local-isearch-map "\C-s"))
(global-set-key '(shift f10) 'isearch-backward)
(define-key isearch-mode-map '(shift f10) (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map '(shift f10)
  (lookup-key minibuffer-local-isearch-map "\C-r"))

;; Here we define our own function and then bind a key to it.

(defun start-or-end-kbd-macro ()
  ;; A doc string.  This is optional.
  "Start defining a keyboard macro, or stop if we're already defining."
  ;; IMPORTANT: Any function bound to a key MUST have an interactive spec,
  ;; usually just the following line:
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

;; The macros used to have their place in the function keys, but I
;; find that I use them significantly less than the really basic
;; things on the function keys.  When using a macro, you call the
;; macro much more than define it, so the setup below makes some
;; sense.

(global-set-key '(shift kp-multiply) 'start-or-end-kbd-macro)
(global-set-key 'kp-multiply 'call-last-kbd-macro) ;; C-x e

;; Note that you can refer to a key sequence either using an ASCII
;; string or the "long way", with vectors and conses.  You saw above
;; (in a comment) the string form for specifying the key sequence `C-x
;; C-f', which is "\C-x\C-f". (For those curious, \C-x is just an
;; escape sequence that puts a ^X character into the string.  Thus,
;; the string just mentioned really just contains two characters, a ^X
;; and a ^F.) The long way to specify the sequence `C-x C-f' would be
;;
;; [(control x) (control f)]
;;
;; The long format lets you specify all possible key sequences, while the
;; string form only lets you specify sequences involving ASCII characters
;; and/or modifiers and in fact only a subset of them.
;;
;; Other examples are:
;;
;; [(control x) n]
;;
;;   (You can leave out the parens when there is no modifier specified in
;;    the keystroke, and that's normally done.)
;;
;; [(shift control meta left)]
;;
;;   (You can put more than one modifier in a keystroke.)
;;
;; (shift control meta left)
;;
;;   (This is the same as the previous.  when there's only one keystroke in
;;    the sequence, you can leave out the brackets, and that's normally
;;    done.)
;;
;; [(control x) (shift button3)]
;;
;;   (You can refer to mouse buttons just like keys -- apply modifiers,
;;    intermingle them in key sequences, etc.  But there's only problem
;;    here, which is that with the mouse you don't just have one possible
;;    gesture, like with keys.  You'd really like to control button-down,
;;    button-up, button-click (down and up without selecting anything),
;;    button drag, button double-click, etc.  This is normally done by
;;    binding your key sequence to `mouse-track', and then putting hooks
;;    onto `mouse-track-click-hook', `mouse-track-drag-up-hook', etc. to
;;    customize the specific behavior.)
;;
;; 'left
;;
;;   (Ultimate reductionism -- no brackets, no parens.  This is the form, in
;;    that, that the 'f1, 'f2, etc. took, which where in fact "long"
;;    forms.)
;; 
;; '(control C)
;;
;;   (You cannot use '(control shift c) here.  This applies whenever Shift +
;;    key translates to a single character.  Note also that you can't use
;;    "\C-C" either; this refers to the non-shifted C-c, just like "\C-c"
;;    would.)
;;
;; '(control \()
;;   (Put a backslash in front of characters used in Lisp syntax.)
;;
;; Also, you can find out the name of a key using C-h c.  WARNING:
;; This does not report the correct name of the keys named `delete',
;; `backspace', `return', `tab', `space', `escape', and `linefeed'!
;; (More correct results can be achieved using
;;
;; ESC : (read-key-sequence "foo: ")
;;
;; .)

;;;;;;;;;;;;;;;;;;;;;;;;

;; Keystrokes to conveniently switch buffers.

;; F6 is invaluable for flipping back and forth between two buffers
;; you're working with.

(global-set-key 'f6 'switch-to-other-buffer) ;; M-C-l
(global-set-key '(meta n) 'switch-to-next-buffer-in-group)
(global-set-key '(meta p) 'switch-to-previous-buffer-in-group)
(global-set-key '(meta N) 'switch-to-next-buffer)
(global-set-key '(meta P) 'switch-to-previous-buffer)

;; Define our own function to deal with the possibility that the newer
;; stuff in the gutter code may not be present -- i.e. we're running
;; an older XEmacs.  Note that we avoid trying to "helpfully" define a
;; function that is present in new versions of XEmacs, but not in
;; older ones.  That can very easily screw up code trying to determine
;; what functionality is present using `fboundp' checks.  See above,
;; near `emacs-version>=', for a full discussion of this.

(defun Init-buffers-tab-omit (buf)
  ;; a function specifying the buffers to omit from the buffers tab.
  ;; This is passed a buffer and should return non-nil if the buffer
  ;; should be omitted.  If the standard buffers-tab functionality is
  ;; there, we just call it to do things "right".  Otherwise we just
  ;; omit invisible buffers, snarfing the code from
  ;; `buffers-menu-omit-invisible-buffers'.
  (if (boundp 'buffers-tab-omit-function)
      (funcall buffers-tab-omit-function buf)
    (not (null (string-match "\\` " (buffer-name buf))))))

(defun switch-to-next-buffer (&optional n)
  "Switch to the next-most-recent buffer.
This essentially rotates the buffer list forward.
N (interactively, the prefix arg) specifies how many times to rotate
forward, and defaults to 1.  Buffers whose name begins with a space
\(i.e. \"invisible\" buffers) are ignored."
  ;; Here is a different interactive spec.  Look up the function
  ;; `interactive' (i.e. `C-h f interactive') to understand how this
  ;; all works.
  (interactive "p")
  (dotimes (n (or n 1))
    (loop
      do (bury-buffer (car (buffer-list)))
      while (Init-buffers-tab-omit (car (buffer-list))))
    (switch-to-buffer (car (buffer-list)))))

(defun buffers-menu-omit-invisible-buffers (buf)
  "For use as a value of `buffers-menu-omit-function'.
Omits normally invisible buffers (those whose name begins with a space)."
  (not (null (string-match "\\` " (buffer-name buf)))))

(defvar Init-buffers-tab-grouping-regexp 
  '("^\\(gnus-\\|message-mode\\|mime/viewer-mode\\)"
    "^\\(emacs-lisp-\\|lisp-\\)")
;; If non-nil, a list of regular expressions for buffer grouping.
;; Each regular expression is applied to the current major-mode symbol
;; name and mode-name, if it matches then any other buffers that match
;; the same regular expression be added to the current group.  This is
;; a copy of `buffers-tab-grouping-regexp'.
  )

(defun Init-select-buffers-tab-buffers (buffer-to-select buf1)
  ;; Specifies the buffers to select from the buffers tab.  This is
  ;; passed two buffers and should return non-nil if the second buffer
  ;; should be selected.  If the standard buffers-tab functionality is
  ;; there, we just call it to do things "right".  Otherwise, we group
  ;; buffers by major mode and by `Init-buffers-tab-grouping-regexp'.
  ;; [We've copied `select-buffers-tab-buffers-by-mode' and
  ;; `buffers-tab-grouping-regexp'.]
  (if (boundp 'buffers-tab-selection-function)
      (funcall buffers-tab-selection-function buffer-to-select buf1)
    (let ((mode1 (symbol-name (symbol-value-in-buffer 'major-mode buf1)))
	  (mode2 (symbol-name (symbol-value-in-buffer 'major-mode 
						      buffer-to-select)))
	  (modenm1 (symbol-value-in-buffer 'mode-name buf1))
	  (modenm2 (symbol-value-in-buffer 'mode-name buffer-to-select)))
      (cond ((or (eq mode1 mode2)
		 (eq modenm1 modenm2)
		 (and (string-match "^[^-]+-" mode1)
		      (string-match
		       (concat "^" (regexp-quote 
				    (substring mode1 0 (match-end 0))))
		       mode2))
		 (and Init-buffers-tab-grouping-regexp
		      (find-if #'(lambda (x)
				   (or
				    (and (string-match x mode1)
					 (string-match x mode2))
				    (and (string-match x modenm1)
					 (string-match x modenm2))))
			       Init-buffers-tab-grouping-regexp)))
	     t)
	    (t nil)))))

(defun switch-to-previous-buffer (&optional n)
  "Switch to the previously most-recent buffer.
This essentially rotates the buffer list backward.
N (interactively, the prefix arg) specifies how many times to rotate
backward, and defaults to 1.  Buffers whose name begins with a space
\(i.e. \"invisible\" buffers) are ignored."
  (interactive "p")
  (dotimes (n (or n 1))
    (loop
      do (switch-to-buffer (car (last (buffer-list))))
      while (Init-buffers-tab-omit (car (buffer-list))))))

(defun switch-to-next-buffer-in-group (&optional n)
  "Switch to the next-most-recent buffer in the current group.
This essentially rotates the buffer list forward.
N (interactively, the prefix arg) specifies how many times to rotate
forward, and defaults to 1.  Buffers whose name begins with a space
\(i.e. \"invisible\" buffers) are ignored."
  (interactive "p")
  (dotimes (n (or n 1))
    (let ((curbuf (car (buffer-list))))
      (loop
	do (bury-buffer (car (buffer-list)))
	while (or (Init-buffers-tab-omit (car (buffer-list)))
		  (not (Init-select-buffers-tab-buffers
			curbuf (car (buffer-list)))))))
    (switch-to-buffer (car (buffer-list)))))

(defun switch-to-previous-buffer-in-group (&optional n)
  "Switch to the previously most-recent buffer in the current group.
This essentially rotates the buffer list backward.
N (interactively, the prefix arg) specifies how many times to rotate
backward, and defaults to 1.  Buffers whose name begins with a space
\(i.e. \"invisible\" buffers) are ignored."
  (interactive "p")
  (dotimes (n (or n 1))
    (let ((curbuf (car (buffer-list))))
      (loop
	do (switch-to-buffer (car (last (buffer-list))))
	while (or (Init-buffers-tab-omit (car (buffer-list)))
		  (not (Init-select-buffers-tab-buffers
			curbuf (car (buffer-list)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;

;; Other text keystrokes.

;; Make a keystroke to insert a literal TAB character. (`C-q TAB' is
;; annoying because difficult to repeat.) Note that this does not work
;; in TTY frames, where TAB and Shift-TAB are indistinguishable.
(define-key global-map '(shift tab) 'tab-to-tab-stop)

;; Toggle auto-filling.  Useful with text but annoying with code.  You
;; can manually fill with M-q.
(global-set-key '(meta f9) 'auto-fill-mode)

;; You cannot say '(meta shift t) here -- see above.
(if (fboundp 'transpose-line-down)
    (global-set-key '(meta T) 'transpose-line-down))
(if (fboundp 'transpose-line-up)
    (global-set-key '(control T) 'transpose-line-up))

;;;;;;;;;;;;;;;;;;;;;;;;

;; Rearrange some inconvenient bindings.

;; ESC ESC ESC is a useful command, but too long.  ESC ESC would be
;; much more logical, but interferes with Meta + keypad/arrow keys on
;; TTY's.  But most people only use window systems and no such problem
;; exists there, so set up the more logical binding there.
;;
;; Note also the use of if vs. cond/when/unless/or/and to express
;; conditional statements.  The difference is purely stylistic.

(when (console-on-window-system-p)
  (global-set-key '(meta escape) 'keyboard-escape-quit)
  (define-key isearch-mode-map '(meta escape) 'isearch-cancel))

;; The standard definition of C-z causes iconification on window
;; systems, which is both useless and annoying.  Instead, bind it to a
;; useful command that's not on any keys. (This also makes a neat
;; parallelism with M-z, which does zap-to-char.) Don't override the
;; TTY binding, which does "Suspend".  If you want this new binding on
;; TTY's, and can train yourself to use C-x C-z to suspend, then
;; remove or comment out the `when' statement. (Here's the proper way
;; to comment out such a statement:
;;
;; ;(when (console-on-window-system-p)
;;   (global-set-key "\C-z" 'zap-up-to-char)
;; ;  )
;;
;; To do this, I first moved the closing paren to a new line,
;; reindented with TAB, then added the semicolons.)
 
(when (console-on-window-system-p)
  (global-set-key "\C-z" 'zap-up-to-char))

;; When not on a TTY, remove the binding of C-x C-c, which normally
;; exits XEmacs.  It's easy to hit this by mistake, and that can be
;; annoying.  You can always quit with the "Exit XEmacs" option on the
;; File menu.

(when (console-on-window-system-p)
    (global-set-key "\C-x\C-c"
      #'(lambda () (interactive)
	  (beep)
	  (message "Use the \"File/Exit XEmacs\" menu item to exit XEmacs"))))

;; Make C-k always delete the whole line, which is what most people want,
;; anyway.
(setq kill-whole-line 'always)
;; M-k does the old behavior (kill to end of line).
(global-set-key '(meta k) #'(lambda ()
			      (interactive)
			      (if (fboundp 'historical-kill-line)
				  (call-interactively #'historical-kill-line)
				(let ((kill-whole-line nil))
				  (call-interactively #'kill-line)))))
;; and Meta-Shift-K does what used to be on M-k, and should
;; (hopefully) even work under TTY's.
(global-set-key '(meta K) 'kill-sentence)

;; Make sure we get Windows-like shifted-motion key selection behavior
;; on recent XEmacs versions.
(cond ((boundp 'shifted-motion-keys-select-region)
       (setq shifted-motion-keys-select-region t))
      ;; otherwise, try the pc-select package -- 
      ((Init-safe-require 'pc-select)
       (pc-select-mode 1)))

;; The following commented-out code rearranges the keymap in an
;; unconventional but extremely useful way for programmers.  Parens
;; and braces are both available without using the shift key (using
;; the bracket keys and f11/f12, respectively).  Brackets (much less
;; used) are the shifted versions of the new paren keys (i.e. where
;; the braces normally are).
;;
;; The idea for this comes from Jamie Zawinski.
;;
;; Also make a convenient keystroke for _, used constantly in C code.
;;
;; NOTE: you can (semi-) conveniently uncomment a region using
;; C-u M-x comment-region, or the "Uncomment Region" menu item on the
;; Lisp menu in new enough versions of XEmacs.

;(keyboard-translate ?[ ?()
;(keyboard-translate ?] ?))
;(keyboard-translate ?{ ?[)
;(keyboard-translate ?} ?])
;;; We don't use `keyboard-translate' for these because it messes up
;;; bindings for M-F9 and the like.
;(define-key key-translation-map 'f11 "{")
;(define-key key-translation-map 'f12 "}")
;(define-key key-translation-map 'f9 "_")


;;;;;;;;;;;;;;;;;;;;;;;;

;; Useful programming-related keystrokes.

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:

-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym))
	  ;; now let it operate fully -- i.e. also check the
	  ;; surrounding sexp for a function call.
	  ((setq sym (function-at-point)) (describe-function sym)))))

(global-set-key '(shift f4) 'next-error) ;; C-x `
(global-set-key '(control f4) 'previous-error)
(global-set-key '(shift f5) 'find-library)
(global-set-key '(control f5) 'find-function)
(global-set-key '(meta f5) 'find-variable)
(global-set-key '(shift f11) 'describe-foo-at-point)
(global-set-key '(control f11) 'eval-last-sexp)
;; Edebug is a source-level debugger for Emacs Lisp programs.  Put
;; the cursor at the end of a function definition and "instrument" it
;; with this command; then, you can single step through it the next
;; time it's run.
(global-set-key '(meta f11) 'edebug-defun)
(global-set-key '(meta f12) 'add-change-log-entry)

;; This nicely parallels M-*, which pops the tag stack.  See below for
;; how to set up tags.
(global-set-key '(control *) 'find-tag-at-point)

;; Define a function to conveniently determine where time is being
;; spent when executing commands or Lisp code.
(defun toggle-profiling ()
  "Start profiling, or stop it and print results.
This lets you figure out where time is being spent when executing Lisp code."
  (interactive)  
  (if (profiling-active-p) 
      (progn  
	(stop-profiling) 
	(message "...Finished profiling")
	(profile-results))
    (message "Profiling...") 
    (clear-profiling-info) 
    (start-profiling)))

;; Note that sequences of C-c plus a letter are specifically
;; reserved for users and should never be bound by any packages.

(global-set-key "\C-cp" 'toggle-profiling)

;; LISPM bindings of Control-Shift-C and Control-Shift-E.
;; See comment above about bindings like this.
(define-key emacs-lisp-mode-map '(control C) 'compile-defun)
(define-key emacs-lisp-mode-map '(control E) 'eval-defun)

;;;;;;;;;;;;;;;;;;;;;;;;

;; Numeric keypad.

;; The numeric keypad as a whole is underused, and it's a good source
;; of keys to bind to commands.  Here we add some useful bindings.
;; Because this is a sample file and I want to avoid unpleasant
;; surprises for novices, I don't actually bind the shared
;; numeric/cursor-motion keys because
;;
;; (a) someone keypads don't have separate motion keys (e.g. laptops?), and
;; (b) TTY's and some X servers might not distinguish the regular and
;;     numeric-keypad motion keys.

;; `kill-current-buffer' (defined below) deletes the current
;; buffer. (Don't worry, you will be prompted to save if it's
;; modified.) By repeatedly pressing keypad-minus, you can
;; conveniently reduce the number of open buffers to a manageable size
;; after you've opened a whole bunch of files and finished working on
;; them.  Shift plus keypad-minus kills both the current buffer and
;; its window, and Control plus keypad-minus kills just the current
;; window.

(global-set-key 'kp-subtract 'kill-current-buffer)
(global-set-key '(shift kp-subtract) 'kill-current-buffer-and-window)
(global-set-key '(control kp-subtract) 'delete-window)
;; Ugh, modes that use `suppress-keymap' and are dumped with XEmacs will
;; need their own definition.  There is no easy way to fix this.
(define-key help-mode-map 'kp-subtract 'kill-current-buffer)
(define-key help-mode-map '(shift kp-subtract)
  'kill-current-buffer-and-window)
(define-key list-mode-map 'kp-subtract 'kill-current-buffer)
(define-key list-mode-map '(shift kp-subtract)
  'kill-current-buffer-and-window)

(defun kill-current-buffer ()
  "Kill the current buffer (prompting if it is modified)."
  (interactive)
  (kill-buffer (current-buffer)))

(defun kill-current-buffer-and-window ()
  "Kill the current buffer (prompting if it is modified) and its window."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defvar grep-all-files-history nil)

(defvar grep-all-files-omitted-expressions
  '("*~" "#*" ".#*" ",*" "*.elc" "*.obj" "*.o" "*.exe" "*.dll" "*.lib" "*.a"
    "*.dvi" "*.class" "*.bin")
  "List of expressions matching files to be omitted in `grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression.")

(defvar grep-all-files-omitted-directories '("CVS" "RCS" "SCCS")
  "List of directories not to recurse into in `grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression.")

(defun construct-grep-all-files-command (find-segment grep-segment)
  (let ((omit-annoying
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -or "))
		    grep-all-files-omitted-expressions
		    "")))
    (cond ((eq grep-find-use-xargs 'gnu)
	   (format "find . %s %s -type f -print0 | xargs -0 -e %s"
		   find-segment omit-annoying grep-segment))
	  (grep-find-use-xargs
	   (format "find . %s %s -type f -print | xargs %s"
		   find-segment omit-annoying grep-segment))
	  (t
	   (format "find . %s %s -type f -exec %s {} /dev/null \\;"
		   find-segment omit-annoying grep-segment)))))

(defun grep-all-files-in-current-directory (command)
  "Run `grep' in all non-annoying files in the current directory.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function does not recurse into subdirectories.  If you want this,
use \\[grep-all-files-in-current-directory-and-below]."
  (interactive
   (progn
     (require 'compile)
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'grep-all-files-history))))
  (require 'compile)
  (grep (construct-grep-all-files-command
	 "-name . -or -type d -prune -or" command)))

(defun grep-all-files-in-current-directory-and-below (command)
  "Run `grep' in all non-annoying files in the current directory and below.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function recurses into subdirectories.  If you do not want this,
use \\[grep-all-files-in-current-directory]."
  (interactive
   (progn
     (require 'compile)
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'grep-all-files-history))))
  (require 'compile)
  (grep (construct-grep-all-files-command
	 ;; prune all specified directories.
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -prune -or "))
		    grep-all-files-omitted-directories
		    "")
	 command)))

(defun clear-select ()
  "Repeatedly select ever larger balanced expressions around the cursor.
Once you have such an expression marked, you can expand to the end of
the following expression with \\[mark-sexp] and to the beginning of the
previous with \\[backward-sexp]."
  (interactive "_") ;this means "preserve the active region after this command"
  (backward-up-list 1)
  (let ((end (save-excursion (forward-sexp) (point))))
    (push-mark end nil t)))

;; #### no kp-divide because it doesn't (currently) work on MS Windows
;; -- always reports as /. #### this should be fixable.
(global-set-key 'kp-add 'query-replace)
(global-set-key '(shift kp-add) 'query-replace-regexp)
(global-set-key '(control kp-add) 'grep-all-files-in-current-directory)
(global-set-key '(meta kp-add) 'grep-all-files-in-current-directory-and-below)
(global-set-key 'clear 'clear-select)
;; Note that you can use a "lambda" expression (an anonymous function)
;; in place of a function name.  This function would be called
;; `pop-local-mark' and lets you repeatedly cycle back through recent
;; marks (marks are set whenever you begin a selection, begin a
;; successful search, are about to jump to the beginning or end of the
;; buffer, etc.).
(global-set-key 'kp-enter (lambda () (interactive) (set-mark-command t)))
(global-set-key '(shift kp-enter) 'repeat-complex-command)
(global-set-key 'pause 'repeat-complex-command) ;; useful on Windows-style kbds
(global-set-key '(control kp-enter) 'eval-expression)

;;;;;;;;;;;;;;;;;;;;;;;;

;; Misc.

;; If you want button2 to insert the selected text
;; at point (where the text cursor is), instead of at the
;; position clicked, uncomment the following:

;(setq mouse-yank-at-point t)

;; If you like the FSF Emacs binding of button3 (single-click
;; extends the selection, double-click kills the selection),
;; uncomment the following:

;(define-key global-map 'button3 'mouse-track-adjust)

;(add-hook 'mouse-track-click-hook
;          (lambda (event count)
;            (if (or (/= (event-button event) 3)
;                    (/= count 2))
;                nil ;; do the normal operation
;              (kill-region (point) (mark))
;              t ;; don't do the normal operations.
;              )))

;; Uncomment this to enable "sticky modifier keys".  With sticky
;; modifier keys enabled, you can press and release a modifier key
;; before pressing the key to be modified, like how the ESC key works
;; always.  If you hold the modifier key down, however, you still get
;; the standard behavior.  I personally think this is the best thing
;; since sliced bread (and a *major* win when it comes to reducing
;; Emacs pinky), but it's disorienting at first so I'm not enabling it
;; here by default.

;(setq modifier-keys-are-sticky t)

;; Enable the command `narrow-to-region' ("C-x n n").  It's a useful
;; command, but possibly confusing to a new user, so it's disabled by
;; default.
(put 'narrow-to-region 'disabled nil)

;; Enable obvious hyperlink following with button1.
(setq Info-button1-follows-hyperlink t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Change Some Basic Behaviors                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change the values of some variables.
;; (t means true; nil means false.)
;;
;; Use C-h v or `Help->Commands, Variables, Keys->Describe Variable...'
;; to find out what these variables mean.
(setq
 find-file-compare-truenames t
 minibuffer-max-depth nil
 )

;; When running ispell, consider all 1-3 character words as correct.
(setq ispell-extra-args '("-W" "3"))

;;; pending-delete-mode causes typed text to replace a selection,
;;; rather than append -- standard behavior under all window systems
;;; nowadays.

(if (fboundp 'pending-delete-mode)
    (pending-delete-mode 1))

;;; NOTE: In this context, `windows-nt' actually refers to all MS
;;; Windows operating systems!
(when (eq system-type 'windows-nt)
  ;; Get mail working under Windows.
  (setq send-mail-function 'smtpmail-send-it)
  (setq smtpmail-debug-info t)
  ;; Substitute your info here.
  ;(setq user-mail-address "ben@xemacs.org")
  ;(setq user-full-name "Ben Wing")
  ;(setq smtpmail-smtp-server "pop.tcsn.uswest.net")

  ;; Make Meta+accelerator traverse to the menu in new enough XEmacs
  ;; versions.  Note that this only overrides Meta bindings that would
  ;; actually invoke a menu, and the most common commands that are
  ;; overridden have preferred alternative bindings using the arrow
  ;; keys.  You can always access the overridden ones using
  ;; Shift+Meta+Key. (Note that "Alt" and "Meta" normally refer to the
  ;; same key, except on some Sun keyboards [where "Meta" is actually
  ;; labelled with a diamond] or if you have explicitly made them
  ;; different under X Windows using `xmodmap'.)
  ;;
  ;; More specifically, the following bindings are overridden:
  ;;
  ;; M-f		(use C-right or Sh-M-f instead)
  ;; M-e		(use M-C-right or Sh-M-e instead)
  ;; M-v		(use Prior aka PgUp or Sh-M-v instead)
  ;; M-m		(use Sh-M-m instead)
  ;; M-t		(use Sh-M-t instead)
  ;; M-o		(normally undefined)
  ;; M-b		(use C-left or Sh-M-b instead)
  ;; M-h		(use M-e h or Sh-M-h instead)
  ;; in Lisp mode, M-l	(use Sh-M-l instead)
  ;; in C mode, M-c	(use Sh-M-c instead)

  (setq menu-accelerator-enabled 'menu-force)

  ;; Make Cygwin `make' work inside a shell buffer.
  (if (boundp 'setenv) (setenv "MAKE_MODE" "UNIX")))

;; This shows how to set up the XEmacs side of tags. (To create the
;; TAGS table, use the `etags' program found in the XEmacs bin
;; directory.  Run it in the root directory of your source tree and
;; specify all source and include files on the command line.)
;(setq tag-table-alist
;      '(
;	;; Everywhere in the /src/xemacs/gui/ source tree will use the TAGS
;	;; file in /src/xemacs/gui/.
;	("/src/xemacs/gui/" . "/src/xemacs/gui/")
;	;; Everywhere in the /src/xemacs/mule/ source tree will use the TAGS
;	;; file in /src/xemacs/mule/.
;	("/src/xemacs/mule/" . "/src/xemacs/mule/")
;	;; etc.
;	("/src/xemacs/fixup/" . "/src/xemacs/fixup/")
;	("/src/emacs/emacs-20.6/" . "/src/emacs/emacs-20.6/")
;	("/src/xemacs/latest/" . "/src/xemacs/latest/")
;	;; Everywhere else will use the TAGS file in
;	;; /src/xemacs/fixup/.
;	("" . "/src/xemacs/fixup/")
;	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Change Some Aspects of GUI Appearance              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Changes the text in the window title bar, to switch to MS Windows
;; format (filename goes first, for best identification in icons) and
;; add the version and full executable path. (However, it is not
;; changed unless it currently has the default value, to avoid
;; interfering with a -wn command line argument I may have started
;; XEmacs with.)

(if (or (equal frame-title-format "%S: %b")
	(equal frame-title-format "%b - XEmacs"))
    (setq frame-title-format
	  (concat "%b - XEmacs "
		  (progn (string-match "\\(.*?\\)\\( XEmacs Lucid\\)?$"
				       emacs-version)
			 (match-string 1 emacs-version))
		  " [" invocation-directory invocation-name "]")))

;; Load some nifty sounds that will replace the default beep.
;;
;; (Note that sampled sounds only work if XEmacs was compiled with
;; sound support and we're running on MS Windows, on a machine which
;; has a NetAudio or ESD server, or on the console of a Linux, Sparc,
;; HP, or SGI machine.  Otherwise, you just get the standard beep.)

(cond ((and (fboundp 'load-default-sounds)
	    (or (and (getenv "DISPLAY") 
		     (string-match ":0" (getenv "DISPLAY")))
		(and (eq (console-type) 'mswindows)
		     (device-sound-enabled-p))))
       (condition-case nil
	   (progn
	     (load-default-sounds)
	     ;; On Windows, at least, the sound "quiet-beep", which is normally
	     ;; given the symbolic name `quiet' and is used for Quit and such,
	     ;; is just totally disgusting.  So make this name correspond to a
	     ;; more innocuous sound.
	     (load-sound-file "drum-beep" 'quiet 80))
	 (error nil)))
      (t
       (setq bell-volume 40)
       (setq sound-alist
	     (append sound-alist '((no-completion :pitch 500))))
       ))

;; Change the continuation glyph face so it stands out more
(make-face-bold (glyph-face continuation-glyph))

;; Change the pointer used during garbage collection.
;;
;; Note that this pointer image is rather large as pointers go,
;; and so it won't work on some X servers (such as the MIT
;; R5 Sun server) because servers may have lamentably small
;; upper limits on pointer size.
;;(if (featurep 'xpm)
;;   (set-glyph-image gc-pointer-glyph
;;	 (expand-file-name "trash.xpm" data-directory)))

;; Here's another way to do that: it first tries to load the
;; pointer once and traps the error, just to see if it's
;; possible to load that pointer on this system; if it is,
;; then it sets gc-pointer-glyph, because we know that
;; will work.  Otherwise, it doesn't change that variable
;; because we know it will just cause some error messages.
(if (featurep 'xpm)
    (let ((file (expand-file-name "recycle.xpm" data-directory)))
      (if (condition-case nil
	      ;; check to make sure we can use the pointer.
	      (make-image-instance file nil
				   '(pointer))
	    (error nil))		; returns nil if an error occurred.
	  (set-glyph-image gc-pointer-glyph file))))

;(when (featurep 'menubar)
;  ;; Add `dired' to the File menu
;  (add-menu-button '("File") ["Edit Directory" dired])

;  ;; Here's a way to add scrollbar-like buttons to the menubar
;  (add-menu-button nil ["Top" beginning-of-buffer])
;  (add-menu-button nil ["<<<" scroll-down])
;  (add-menu-button nil [" . " recenter])
;  (add-menu-button nil [">>>" scroll-up])
;  (add-menu-button nil ["Bot" end-of-buffer]))

;; Here's a cute hack that shows how to programmatically change some
;; text colors.  It changes the background color of the window if it's
;; not on the local machine, or if it's running as root:

;; local emacs background:  whitesmoke [i.e. the default color]
;; remote emacs background: palegreen1
;; root emacs background:   coral2

;; Uncomment to enable.

;(cond
; ((and running-xemacs
;       (console-on-window-system-p)
;       ;; this does not make much sense on Windows.
;       (not (eq system-type 'windows-nt)))
;  (let* ((root-p (eq 0 (user-uid)))
;	 (dpy (or (getenv "DISPLAY") ""))
;	 (remote-p (not
;		    (or (string-match "^\\(\\|unix\\|localhost\\):" dpy)
;			(let ((s (system-name)))
;			  (if (string-match "\\.\\(netscape\\|mcom\\)\\.com" s)
;			      (setq s (substring s 0 (match-beginning 0))))
;			  (string-match (concat "^" (regexp-quote s)) dpy)))))
;	 (bg (cond (root-p "coral2")
;		   (remote-p "palegreen1")
;		   (t nil))))
;    (cond (bg
;	   (let ((def (color-name (face-background 'default)))
;		 (faces (face-list)))
;	     (while faces
;	       (let ((obg (face-background (car faces))))
;		 (if (and obg (equal def (color-name obg)))
;		     (set-face-background (car faces) bg)))
;	       (setq faces (cdr faces)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Changing the Modeline                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable line numbers and column numbers.  This is done in C code now
;; and is very fast.
(line-number-mode 1)
(column-number-mode 1)

;; Rearrange the modeline so that everything is to the left of the
;; long list of minor modes, which is relatively unimportant but takes
;; up so much room that anything to the right is obliterated.

(setq-default
 modeline-format
 (list
  ""
  (if (boundp 'modeline-multibyte-status) 'modeline-multibyte-status "")
  (cons modeline-modified-extent 'modeline-modified)
  (cons modeline-buffer-id-extent
	(list (cons modeline-buffer-id-left-extent
		    (cons 15 (list
			      (list 'line-number-mode "L%l ")
			      (list 'column-number-mode "C%c ")
			      (cons -3 "%p"))))
	      (cons modeline-buffer-id-right-extent "%17b")))
  "   "
  'global-mode-string
  "   %[("
  (cons modeline-minor-mode-extent
	(list "" 'mode-name 'minor-mode-alist))
  (cons modeline-narrowed-extent "%n")
  'modeline-process
  ")%]----"
  "%-"
  ))

;; Get rid of modeline information taking up too much space -- in
;; particular, minor modes that are always enabled.
(setq pending-delete-modeline-string "")
(setq filladapt-mode-line-string "")
;; lazy-lock doesn't have a variable for its modeline name, so we have
;; to do a bit of surgery.
(and (assoc 'lazy-lock-mode minor-mode-alist)
     (setcdr (cdr (cadr (assoc 'lazy-lock-mode minor-mode-alist))) ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Customization of Specific Packages                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ********************
;;; Load gnuserv, which will allow you to connect to XEmacs sessions
;;; using `gnuclient'.

;; If you never run more than one XEmacs at a time, you might want to
;; always start gnuserv.  Otherwise it is preferable to specify
;; `-f gnuserv-start' on the command line to one of the XEmacsen.
; (gnuserv-start)


;;; ********************
;;; Load efs, which uses the FTP protocol as a pseudo-filesystem.
;;; When this is loaded, the pathname syntax /user@host:/remote/path
;;; refers to files accessible through ftp.
;;;
(Init-safe-require 'dired)

(or (Init-safe-require 'efs-auto) (Init-safe-require 'ange-ftp))

;;; ********************
;;; Load the default-dir.el package which installs fancy handling of
;;; the initial contents in the minibuffer when reading file names.
;; #### but it seems to cause some breakage.
;(Init-safe-require 'default-dir))

;;; ********************
;;; Put all of your autosave files in one place, instead of scattering
;;; them around the file system.  This has many advantages -- e.g. it
;;; will eliminate slowdowns caused by editing files on a slow NFS
;;; server.  (*Provided* that your home directory is local or on a
;;; fast server!  If not, pick a value for `auto-save-directory' that
;;; is fast fast fast!)
;;;
;;; Unfortunately, the code that implements this (auto-save.el) is
;;; broken on Windows prior to 21.4.
(unless (and (eq system-type 'windows-nt)
	     (not (emacs-version>= 21 4)))
  (setq auto-save-directory (expand-file-name "~/.autosave/")
	auto-save-directory-fallback auto-save-directory
	auto-save-hash-p nil
	efs-auto-save t
	efs-auto-save-remotely nil
	;; now that we have auto-save-timeout, let's crank this up
	;; for better interactive response.
	auto-save-interval 2000
	)
  )


;;; ********************
;;; cc-mode (the mode you're in when editing C, C++, and Objective C files)

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; Change the indentation amount to 4 spaces instead of 2.
;; You have to do it in this complicated way because of the
;; strange way the cc-mode initializes the value of `c-basic-offset'.
;; (add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))


;;; ********************
;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.
;;;
(Init-safe-require 'completer)


;;; ********************
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;; 
(setq crypt-encryption-type 'pgp   ; default encryption mechanism
      crypt-confirm-password t	   ; make sure new passwords are correct
      ;crypt-never-ever-decrypt t  ; if you don't encrypt anything, set this to
				   ; tell it not to assume that "binary" files
				   ; are encrypted and require a password.
      )
(Init-safe-require 'crypt)


;;; ********************
;;; Filladapt is an adaptive text-filling package.  When it is enabled it
;;; makes filling (e.g. using M-q) much much smarter about paragraphs
;;; that are indented and/or are set off with semicolons, dashes, etc.

(Init-safe-require 'filladapt)
(setq-default filladapt-mode t)
(when (fboundp 'turn-off-filladapt-mode)
  (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
  (add-hook 'outline-mode-hook 'turn-off-filladapt-mode))


;;; ********************
;;; Font-Lock is a syntax-highlighting package.  When it is enabled and you
;;; are editing a program, different parts of your program will appear in
;;; different fonts or colors.  For example, with the code below, comments
;;; appear in red italics, function names in function definitions appear in
;;; blue bold, etc.  The code below will cause font-lock to automatically be
;;; enabled when you edit C, C++, Emacs-Lisp, and many other kinds of
;;; programs.
;;;
;;; The "Options" menu has some commands for controlling this as well.
;;;
(cond (running-xemacs

;; The commented-out code below is an example of setting up custom
;; font-lock colors.

;       ;; If you want the default colors, you could do this:
;       ;; (setq font-lock-use-default-fonts nil)
;       ;; (setq font-lock-use-default-colors t)
;       ;; but I want to specify my own colors, so I turn off all
;       ;; default values.
;       (setq font-lock-use-default-fonts nil)
;       (setq font-lock-use-default-colors nil)

       (Init-safe-require 'font-lock)

;       ;; Mess around with the faces a bit.  Note that you have
;       ;; to change the font-lock-use-default-* variables *before*
;       ;; loading font-lock, and wait till *after* loading font-lock
;       ;; to customize the faces.

;       ;; string face is green
;       (set-face-foreground 'font-lock-string-face "forest green")

;       ;; comments are italic and red; doc strings are italic
;       (set-face-font 'font-lock-comment-face [italic])
;       ;; Underlining comments looks terrible on tty's
;       (set-face-underline-p 'font-lock-comment-face nil 'global 'tty)
;       (set-face-highlight-p 'font-lock-comment-face t 'global 'tty)
;       (copy-face 'font-lock-comment-face 'font-lock-doc-string-face)
;       (set-face-foreground 'font-lock-comment-face "red")

;       ;; function names are bold and blue
;       (set-face-font 'font-lock-function-name-face [bold])
;       (set-face-foreground 'font-lock-function-name-face "blue")

;       ;; misc. faces
;       (set-face-font 'font-lock-preprocessor-face [bold])
;       (set-face-font 'font-lock-type-face [italic])
;       (set-face-font 'font-lock-keyword-face [bold])
       ))


;;; ********************
;;; lazy-lock is a package which speeds up the highlighting of files
;;; by doing it "on-the-fly" -- only the visible portion of the
;;; buffer is fontified.  The results may not always be quite as
;;; accurate as using full font-lock or fast-lock, but it's *much*
;;; faster.  No more annoying pauses when you load files.

(if (fboundp 'turn-on-lazy-lock)
    (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock))

;; I personally don't like "stealth mode" (where lazy-lock starts
;; fontifying in the background if you're idle for 30 seconds)
;; because it takes too long to wake up again.
(setq lazy-lock-stealth-time nil)


;;; ********************
;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;;
(cond ((and running-xemacs (Init-safe-require 'func-menu))
       (global-set-key '(shift f12) 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (global-set-key "\C-cl" 'fume-list-functions)
       (global-set-key "\C-cg" 'fume-prompt-function-goto)

       ;; The Hyperbole information manager package uses (shift button2) and
       ;; (shift button3) to provide context-sensitive mouse keys.  If you
       ;; use this next binding, it will conflict with Hyperbole's setup.
       ;; Choose another mouse key if you use Hyperbole.
       (global-set-key '(shift button3) 'mouse-function-menu)

       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq fume-max-items 25
             fume-fn-window-position 3
             fume-auto-position-popup t
             fume-display-in-modeline-p t
             fume-menubar-menu-name
	     (if (fboundp 'submenu-generate-accelerator-spec)
		 "Function%_s" "Functions")
             fume-buffer-name "*Function List*"
             fume-no-prompt-on-valid-default nil)
       ))


;;; ********************
;;; MH is a mail-reading system from the Rand Corporation that relies on a
;;; number of external filter programs (which do not come with emacs.)
;;; Emacs provides a nice front-end onto MH, called "mh-e".
;;;
;; Bindings that let you send or read mail using MH
;(global-set-key "\C-xm"  'mh-smail)
;(global-set-key "\C-x4m" 'mh-smail-other-window)
;(global-set-key "\C-cr"  'mh-rmail)

;; Customization of MH behavior.
(setq mh-delete-yanked-msg-window t)
(setq mh-yank-from-start-of-msg 'body)
(setq mh-summary-height 11)

;; Use lines like the following if your version of MH
;; is in a special place.
;(setq mh-progs "/usr/dist/pkgs/mh/bin.svr4/")
;(setq mh-lib "/usr/dist/pkgs/mh/lib.svr4/")


;;; ********************
;;; resize-minibuffer-mode makes the minibuffer automatically
;;; resize as necessary when it's too small to hold its contents.

(when (fboundp 'resize-minibuffer-mode)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil))


;;; ********************
;;; scroll-in-place is a package that keeps the cursor on the same line (and in the same column) when scrolling by a page using PgUp/PgDn.

(if (Init-safe-require 'scroll-in-place)
    (turn-on-scroll-in-place))


;;; ********************
;;; W3 is a browser for the World Wide Web, and takes advantage of the very
;;; latest redisplay features in XEmacs.  You can access it simply by typing 
;;; 'M-x w3'; however, if you're unlucky enough to be on a machine that is 
;;; behind a firewall, you will have to do something like this first:

;(setq w3-use-telnet t
;      ;;
;      ;; If the Telnet program you use to access the outside world is
;      ;; not called "telnet", specify its name like this.
;      w3-telnet-prog "itelnet"
;      ;;
;      ;; If your Telnet program adds lines of junk at the beginning
;      ;; of the session, specify the number of lines here.
;      w3-telnet-header-length 4
;      )

;;; Inhibit loading of custom-file

;; make-temp-name returns a name which does not refer to an existing file,
;; and thus the named file is unreadable.
(when Init-inhibit-custom-file-p
  (setq custom-file (make-temp-name "/tmp/non-existent-")))

