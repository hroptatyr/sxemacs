;;; about.el --- the About The Authors page (shameless self promotion).

;; Copyright (c) 1997 Free Software Foundation, Inc.
;; Copyright (C) 2001 Ben Wing.

;; Keywords: extensions
;; Version: 2.5
;; Maintainer: XEmacs Development Team

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;; Original code: Jamie Zawinski <jwz@jwz.org>
;; Text: Ben Wing <ben@xemacs.org>, Jamie Zawinski <jwz@jwz.org>
;; Hard: Amiga 1000, Progressive Peripherals Frame Grabber.
;; Soft: FG 2.0, DigiPaint 3.0, pbmplus (dec 91), xv 3.0.
;; Modified for 19.11 by Eduardo Pelegri-Llopart <pelegri@eng.sun.com>
;;		      and Chuck Thompson <cthomp@xemacs.org>
;; More hacking for 19.12 by Chuck Thompson and Ben Wing.
;; 19.13 and 19.14 updating done by Chuck Thompson.
;; 19.15 and 20.0 updating done by Steve Baur and Martin Buchholz.

;; Completely rewritten for 20.3 by Hrvoje Niksic <hniksic@xemacs.org>.
;; The original had no version numbers; I numbered the rewrite as 2.0.
;; Extensively revamped and most text rewritten by Ben Wing
;; <ben@xemacs.org> for 21.4.

;; Many things in this file are to gag.  Ideally, we should just use
;; HTML (or some other extension, e.g. info) for this sort of thing.
;; However, W3 loads too long and is too large to be dumped with
;; XEmacs.

;; If you think this is ugly now -- o boy, you should have seen it
;; before.

(require 'wid-edit)

;; People in this list have their individual links from the main page,
;; or from the `Legion' page.  If they have an image, it should be
;; named after the CAR of the list element (baw -> baw.png).
;;
;; If you add to this list, you'll want to update
;; `about-personal-info' and `about-hackers', and add the name to one
;; of the three mutually exclusive lists just below.

(defface about-headline-face
  '((((class color) (background dark))
     (:foreground "red" :bold t))
    ;; red4 is hardly different from black on windows.
    (((class color) (background light)
      (type mswindows))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "red4" :bold t))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t))
    (t (:bold t)))
  "Face used for color-highlighted headlines in the About page.")

(defface about-link-face
  '((((class color) (background dark))
     (:foreground "blue" :underline t))
    ;; blue4 is hardly different from black on windows.
    (((class color) (background light) (type mswindows))
     (:foreground "blue3" :underline t))
    (((class color) (background light))
     (:foreground "blue4" :underline t))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t :underline t))
    (t (:underline t)))
  "Face used for links in the About page.")

(defvar xemacs-hackers
  '(
    ;; to sort the stuff below, use M-x sort-regexp-fields RET
    ;; ^.*$ RET (\([a-z]*\) RET
    (adrian   "Adrian Aichner"    "adrian@xemacs.org")
    (aj       "Andreas Jaeger"    "aj@xemacs.org")
    (ajc      "Andrew Cosgriff"   "ajc@xemacs.org")
    (alastair "Alastair Houghton" "alastair@xemacs.org")
    (baw      "Barry Warsaw"      "bwarsaw@xemacs.org")
    (ben      "Ben Wing"          "ben@xemacs.org")
    (bw       "Bob Weiner"        "weiner@xemacs.org")
    (cgw      "Charles Waldman"   "cgw@xemacs.org")
    (chr      "Christian Nybø"    "chr@xemacs.org")
    (craig    "Craig Lanning"     "craig@xemacs.org")
    (cthomp   "Chuck Thompson"    "cthomp@xemacs.org")
    (daiki    "Daiki Ueno"        "daiki@xemacs.org")
    (dan      "Dan Holmsand"      "dan@xemacs.org")
    (darrylo  "Darryl Okahata"    "darrylo@xemacs.org")
    (devin    "Matthieu Devin"    "devin@xemacs.org")
    (dkindred "Darrell Kindred"   "dkindred@xemacs.org")
    (dmoore   "David Moore"       "dmoore@xemacs.org")
    (dv       "Didier Verna"      "didier@xemacs.org")
    (eb       "Eric Benson"       "eb@xemacs.org")
    (fabrice  "Fabrice Popineau"  "fabrice@xemacs.org")
    (golubev  "Ilya Golubev"      "golubev@xemacs.org")
    (gunnar   "Gunnar Evermann"   "gunnar@xemacs.org")
    (hbs      "Harlan Sexton"     "hbs@xemacs.org")
    (hisashi  "Hisashi Miyashita" "hisashi@xemacs.org")
    (hmuller  "Hans Muller"       "hmuller@xemacs.org")
    (hniksic  "Hrvoje Niksic"     "hniksic@xemacs.org")
    (hobley   "David hobley"      "hobley@xemacs.org")
    (jan      "Jan Vroonhof"      "jan@xemacs.org")
    (jareth   "Jareth Hein"       "jareth@xemacs.org")
    (jason    "Jason R. Mastaler" "jason@xemacs.org")
    (jens     "Jens Lautenbacher" "jens@xemacs.org")
    (jmiller  "Jeff Miller"       "jmiller@xemacs.org")
    (jonathan "Jonathan Harris"   "jonathan@xemacs.org")
    (juhp     "Jens-Ulrik Holger Petersen" "petersen@xemacs.org")
    (jwz      "Jamie Zawinski"    "jwz@xemacs.org")
    (kazz     "IENAGA Kazuyuki"   "ienaga@xemacs.org")
    (kirill   "Kirill Katsnelson" "kirill@xemacs.org")
    (kyle     "Kyle Jones"        "kyle@xemacs.org")
    (larsi    "Lars Magne Ingebrigtsen" "larsi@xemacs.org")
    (marcpa   "Marc Paquette"     "marcpa@xemacs.org")
    (martin   "Martin Buchholz"   "martin@xemacs.org")
    (mcook    "Michael R. Cook"   "mcook@xemacs.org")
    (mly      "Richard Mlynarik"  "mly@xemacs.org")
    (morioka  "MORIOKA Tomohiko"  "morioka@xemacs.org")
    (mta      "Mike Alexander"    "mta@xemacs.org")
    (ograf    "Oliver Graf"       "ograf@xemacs.org")
    (olivier  "Olivier Galibert"  "olivier@xemacs.org")
    (oscar    "Oscar Figueiredo"  "oscar@xemacs.org")
    (pelegri  "Eduardo Pelegri-Llopart" "pelegri@xemacs.org")
    (pez      "Peter Pezaris"     "pez@xemacs.org")
    (piper    "Andy Piper"        "andy@xemacs.org")
    (pittman  "Daniel Pittman"    "pittman@xemacs.org")
    (rickc    "Rick Campbell"     "rickc@xemacs.org")
    (rose     "John Rose"         "rose@xemacs.org")
    (rossini  "Anthony Rossini"   "rossini@xemacs.org")
    (slb      "Steve Baur"        "steve@xemacs.org")
    (sperber  "Michael Sperber"   "mike@xemacs.org")
    (stig     "Jonathan Stigelman" "stig@xemacs.org")
    (stigb    "Stig Bjorlykke"    "stigb@xemacs.org")
    (thiessel "Marcus Thiessel"   "marcus@xemacs.org")
    (tomonori "Tomonori Ikeyama"  "tomonori@xemacs.org")
    (tuck     "Matt Tucker"       "tuck@xemacs.org")
    (turnbull "Stephen Turnbull"  "turnbull@xemacs.org")
    (vin      "Vin Shelton"       "acs@xemacs.org")
    (vladimir "Vladimir Ivanovic" "vladimir@xemacs.org")
    (wmperry  "William Perry"     "wmperry@xemacs.org")
    (yoshiki  "Yoshiki Hayashi"   "yoshiki@xemacs.org")
    (youngs   "Steve Youngs"      "youngs@xemacs.org")
    )
  "Alist of XEmacs hackers.")

(defvar about-current-release-maintainers
  ;; this list should not necessarily be in sorted order.
  '(vin turnbull adrian ben martin piper sperber youngs))

(defvar about-other-current-hackers
  ;; to sort this list or the one below, use:
  ;; M-x sort-regexp-fields RET [a-z]+ RET \(.*\) RET
  '(aj alastair cgw craig daiki dan dv fabrice golubev gunnar hisashi hniksic
       jan jareth jmiller jason jonathan kazz kirill larsi morioka mta ograf
       olivier oscar pittman tomonori tuck wmperry yoshiki))

(defvar about-once-and-future-hackers
  '(ajc baw bw chr cthomp darrylo devin dkindred dmoore eb hbs hmuller
	hobley jens juhp jwz kyle marcpa mcook mly ograf pelegri pez
	rickc rose rossini slb stig stigb thiessel vladimir))

;; The CAR of alist elements is a valid argument to `about-url-link'.
;; It is preferred to a simple string, because it makes maintenance
;; easier.  Please add new URLs to this list.
(defvar about-url-alist
    ;; to sort the stuff below, use M-x sort-regexp-fields RET
    ;; ^.*$ RET (\([a-z]*\) RET
  '((ajc        . "http://www-personal.monash.edu.au/~ajc/")
    (alastair   . "http://website.lineone.net/~ajhoughton/")
    (baw        . "http://barry.wooz.org/")
    (ben        . "http://www.666.com/ben/")
    (ben-xemacs . "http://www.xemacs.org/Architecting-XEmacs/index.html")
    (beopen     . "http://www.beopen.com/")
    (cc-mode    . "http://cc-mode.sourceforge.net/")
    (chr        . "http://www.xemacs.org/faq/")
    (daiki      . "http://deisui.bug.org/diary/servlet/view")
    (dkindred   . "http://www.cs.cmu.edu/People/dkindred/me.html")
    (dmoore     . "http://oj.egbt.org/dmoore/")
    (dv         . "http://www.lrde.epita.fr/~didier/")
    (fabrice    . "http://www.ese-metz.fr/~popineau/")
    (fptex      . "http://www.fptex.org/")
    (jason      . "http://www.mastaler.com/")
    (juhp       . "http://www.01.246.ne.jp/~juhp/")
    (jwz        . "http://www.jwz.org/")
    (kazz       . "http://www.imasy.or.jp/~kazz/")
    (kyle       . "http://www.wonderworks.com/kyle/")
    (larsi      . "http://quimby.gnus.org/lmi/")
    (marcpa     . "http://www.positron911.com/products/power.htm")
    (ograf      . "http://www.fga.de/~ograf/")
    (pez        . "http://cbs.sportsline.com/")
    (piper      . "http://www.andypiper.com/")
    (rossini    . "http://faculty.washington.edu/rossini/")
    (stigb      . "http://www.tihlde.hist.no/~stigb/")
    (vin        . "http://www.upa.org/")
    (vladimir   . "http://www.leonora.org/~vladimir/")
    (wget       . "http://sunsite.dk/wget/")
    (xemacs     . "http://www.xemacs.org/")
    (youngs     . "http://eicq.sourceforge.net/"))
  "Some of the more important URLs.")

(defvar about-left-margin 3)

(defun about-lookup-url (name)
  (let ((result (cdr (assq name about-url-alist))))
    (assert result)
    result))

;; Insert a URL link in the buffer.  TEXT-TO-INSERT is the text that will
;; be hyperlinked; if omitted, the URL is used.  HELP-ECHO is some text that
;; will be displayed when the mouse moves over the link.
(defun about-url-link (url &optional text-to-insert help-echo)
  (assert url)
  (when (symbolp url)
    (setq url (about-lookup-url url)))
  (when (and text-to-insert (symbolp text-to-insert))
    (setq text-to-insert (about-lookup-url text-to-insert)))
  (widget-create 'url-link
		 :button-prefix ""
		 :button-suffix ""
		 :help-echo help-echo
		 :tag (or text-to-insert url)
		 url))

;; Insert a mail link in the buffer.
(defun about-mailto-link (address)
  (lexical-let ((address address))
    (widget-create 'link
		   :tag address
		   :button-prefix ""
		   :button-suffix ""
		   :action (lambda (widget &optional event)
			     (compose-mail address))
		   :help-echo (format "Send mail to %s" address))))

;; Attach a face to a string, in order to be inserted into the buffer.
;; Make sure that the extent is duplicable, but unique.  Returns the
;; string.
(defun about-with-face (string face)
  (let ((ext (make-extent 0 (length string) string)))
    (set-extent-property ext 'duplicable t)
    (set-extent-property ext 'unique t)
    (set-extent-property ext 'start-open t)
    (set-extent-property ext 'end-open t)
    (set-extent-face ext face))
  string)

;; Switch to buffer NAME.  If it doesn't exist, make it and switch to it.
(defun about-get-buffer (name)
  (cond ((get-buffer name)
	 (switch-to-buffer name)
	 (delete-other-windows)
	 (goto-char (point-min))
	 name)
	(t
	 (switch-to-buffer name)
	 (delete-other-windows)
	 (buffer-disable-undo)
	 ;; #### This is a temporary fix until wid-edit gets fixed right.
	 ;; We don't do everything that widget-button-click does -- i.e.
	 ;; we don't change the link color on button down -- but that's
	 ;; not important.
	 (add-local-hook
	  'mouse-track-click-hook
	  #'(lambda (event count)
	      (cond
	       ((widget-event-point event)
		(let* ((pos (widget-event-point event))
		       (button (get-char-property pos 'button)))
		  (when button
		    (widget-apply-action button event)
		    t))))))
	 (set-specifier left-margin-width about-left-margin (current-buffer))
	 (set (make-local-variable 'widget-button-face) 'about-link-face)
	 nil)))

;; Set up the stuff needed by widget.  Allowed types are `bury' and
;; `kill'.  The reason why we offer both types is performance: when a
;; large buffer is merely buried, `about' will find it again when the
;; user requests it, instead of recreating it.  Small buffers can be
;; killed because it is cheap to generate their contents.

(defun about-finish-buffer (&optional type)
  (or type (setq type 'bury))
  (widget-insert "\n")
  (if (eq type 'bury)
      (widget-create 'link
		     :help-echo "Bury this buffer"
		     :action (lambda (widget event)
			       (if event
				   ;; For some reason,
				   ;; (bury-buffer (event-buffer event))
				   ;; doesn't work.
				   (with-selected-window (event-window event)
				     (bury-buffer))
				 (bury-buffer)))
		     :tag "Bury")
    (widget-create 'link
		   :help-echo "Kill this buffer"
		   :action (lambda (widget event)
			     (if event
				 (kill-buffer (event-buffer event))
			       (kill-buffer (current-buffer))))
		   :tag "Kill"))
  (widget-insert " this buffer and return to previous.\n")
  (use-local-map (make-sparse-keymap))
  (set-keymap-parent (current-local-map) widget-keymap)
  (if (eq type 'bury)
      (progn
	(local-set-key "q" 'bury-buffer)
	(local-set-key "l" 'bury-buffer))
    (let ((dispose (lambda () (interactive) (kill-buffer (current-buffer)))))
      (local-set-key "q" dispose)
      (local-set-key "l" dispose)))
  (local-set-key " " 'scroll-up)
  (local-set-key [backspace] 'scroll-down)
  (local-set-key "\177" 'scroll-down)
  (widget-setup)
  (goto-char (point-min))
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

;; Make the appropriate number of spaces.
(defun about-center (string-or-glyph)
  (let ((n (- (startup-center-spaces string-or-glyph) about-left-margin)))
    (make-string (if (natnump n) n 0) ?\ )))

;; Main entry page.

;;;###autoload
(defun about-xemacs ()
  "Describe the True Editor and its minions."
  (interactive)
  (unless (about-get-buffer "*About XEmacs*")
    (widget-insert (about-center xemacs-logo))
    (widget-create 'default
		   :format "%t"
		   :tag-glyph xemacs-logo)
    (widget-insert "\n")
    (let* ((emacs-short-version (format "%d.%d.%d"
					emacs-major-version
					emacs-minor-version
					emacs-patch-level))
	   (emacs-about-version (format "version %s; %s %s"
					emacs-short-version
					(cdr (assoc (substring emacs-build-time
							       4 7)
						    '(("Jan" . "January")
						      ("Feb" . "February")
						      ("Mar" . "March")
						      ("Apr" . "April")
						      ("May" . "May")
						      ("Jun" . "June")
						      ("Jul" . "July")
						      ("Aug" . "August")
						      ("Sep" . "September")
						      ("Oct" . "October")
						      ("Nov" . "November")
						      ("Dec" . "December"))))
					(substring emacs-build-time -4))))
      (widget-insert (about-center emacs-about-version))
      (widget-create 'link :help-echo "What's new in XEmacs"
		     :action 'about-news
		     emacs-about-version))

    (widget-insert
     "\n\n"
     (about-with-face "XEmacs" 'bold-italic)
     " is a powerful, highly customizable open source text editor and
application development system, with full GUI support.  It is protected
under the GNU Public License and related to other versions of Emacs, in
particular GNU Emacs.  Its emphasis is on modern graphical user
interface support and an open software development model, similar to
Linux.  XEmacs has an active development community numbering in the
hundreds (and thousands of active beta testers on top of this), and runs
on all versions of MS Windows, on Linux, and on nearly every other
version of Unix in existence.  ")
    (widget-create 'link :help-echo "An XEmacs history lesson"
		   :action 'about-collaboration
		   :button-prefix ""
		   :button-suffix ""
		   "Support for XEmacs")
    (widget-insert
     " has been supplied by
Sun Microsystems, University of Illinois, Lucid, ETL/Electrotechnical
Laboratory, Amdahl Corporation, BeOpen, and others, as well as the
unpaid time of a great number of individual developers.

XEmacs has many ")
    (widget-create 'link :help-echo "See a list of XEmacs advantages over GNU Emacs"
		   :action 'about-advantages
		   :button-prefix ""
		   :button-suffix ""
		   "advantages")
    (widget-insert " over GNU Emacs.  In addition, XEmacs 21.4
provides many ")
    (widget-create 'link :help-echo "See a list of new features in XEmacs 21.4"
		   :action 'about-news
		   :button-prefix ""
		   :button-suffix ""
		   "new features")
    (widget-insert " not found in previous versions of XEmacs.
More details on XEmacs's functionality, including bundled packages, can
be obtained through the ")
    (widget-create 'info-link
		   :help-echo "Browse the info system"
		   :button-prefix ""
		   :button-suffix ""
		   :tag "info"
		   "(dir)")

    (widget-insert
     " on-line information system.\n
The XEmacs web page can be browsed, using any WWW browser at\n
\t\t    ")
    (about-url-link 'xemacs nil "Visit XEmacs WWW page")
    (widget-insert "\n
Note that W3 (XEmacs's own browser), might need customization (due to
firewalls) in order to work correctly.

XEmacs is the result of the time and effort of many people.  The
developers responsible for this release are:\n\n")

    (flet ((setup-person (who)
	    (widget-insert "\t* ")
	    (let* ((entry (assq who xemacs-hackers))
		   (name (cadr entry))
		   (address (caddr entry)))
	      (widget-create 'link
			     :help-echo (concat "Find out more about " name)
			     :button-prefix ""
			     :button-suffix ""
			     :action 'about-maintainer
			     :tag name
			     :value who)
	      (widget-insert (format "  <%s>\n" address)))))
      ;; Setup persons responsible for this release.
      (mapc 'setup-person about-current-release-maintainers)
      (widget-insert "\n\t* ")
      (widget-create 'link :help-echo "A legion of XEmacs hackers"
		     :action 'about-hackers
		     :button-prefix ""
		     :button-suffix ""
		     "The full list of contributors...")
      (widget-insert "\n
Steve Baur was the primary maintainer for 19.15 through 21.0.\n\n")
      (setup-person 'slb)
      (widget-insert "
Chuck Thompson and Ben Wing were the maintainers for 19.11 through 19.14
and heavy code contributors for 19.8 through 19.10.\n\n")
      (setup-person 'cthomp)
      (setup-person 'ben)
      (widget-insert "
Jamie Zawinski was the maintainer for 19.0 through 19.10 (the entire
history of Lucid Emacs).\n\n")
      (setup-person 'jwz))
    (about-finish-buffer)
    ;; it looks horrible with the cursor on the first line, since it's
    ;; so big.
    (goto-line 2)))

;; View news
(defun about-news (&rest ignore)
  (view-emacs-news)
  (message "%s" (substitute-command-keys
		 "Press \\[kill-buffer] to exit this buffer")))

(defun about-collaboration (&rest ignore)
  (unless (about-get-buffer "*About Collaboration*")
    (let ((title "Why Another Version of Emacs"))
      (widget-insert
       "\n"
       (about-center title)
       (about-with-face title 'bold)))
    (widget-insert
     "\n\n"
     (about-with-face "The Lucid, Inc. Point of View"
		      'italic)
     " (quite outdated)\n
At the time of the inception of Lucid Emacs (the former name of
XEmacs), Lucid's latest product was Energize, a C/C++ development
environment.  Rather than invent (and force our users to learn) a new
user interface, we chose to build part of our environment on top of
the world's best editor, GNU Emacs.  (Though our product is
commercial, the work we did on GNU Emacs is free software, and is
useful in its own right.)

We needed a version of Emacs with mouse-sensitive regions, multiple
fonts, the ability to mark sections of a buffer as read-only, the
ability to detect which parts of a buffer have been modified, and many
other features.

For our purposes, the existing version of Epoch was not sufficient; it
did not allow us to put arbitrary pixmaps/icons in buffers, `undo' did
not restore changes to regions, regions did not overlap and merge
their attributes in the way we needed, and several other things.

We could have devoted our time to making Epoch do what we needed (and,
in fact, we spent some time doing that in 1990) but, since the FSF
planned to include Epoch-like features in their version 19, we decided
that our efforts would be better spent improving Emacs 19 instead of
Epoch.

Our original hope was that our changes to Emacs would be incorporated
into the \"official\" v19.  However, scheduling conflicts arose, and
we found that, given the amount of work still remaining to be done, we
didn't have the time or manpower to do the level of coordination that
would be necessary to get our changes accepted by the FSF.
Consequently, we released our work as a forked branch of Emacs,
instead of delaying any longer.

Roughly a year after Lucid Emacs 19.0 was released, a beta version of
the FSF branch of Emacs 19 was released.  The FSF version is better in
some areas, and worse in others, as reflects the differing focus of
our development efforts.

We plan to continue developing and supporting Lucid Emacs, and merging
in bug fixes and new features from the FSF branch as appropriate; we
do not plan to discard any of the functionality that we implemented
which RMS has chosen not to include in his version.

Certain elements of Lucid Emacs, or derivatives of them, have been
ported to the FSF version.  We have not been doing work in this
direction, because we feel that Lucid Emacs has a cleaner and more
extensible substrate, and that any kind of merger between the two
branches would be far easier by merging the FSF changes into our
version than the other way around.

We have been working closely with the Epoch developers to merge in the
remaining Epoch functionality which Lucid Emacs does not yet have.
Epoch and Lucid Emacs will soon be one and the same thing.  Work is
being done on a compatibility package which will allow Epoch 4 code to
run in XEmacs with little or no change.\n\n"
     (about-with-face "The Sun Microsystems, Inc. Point of View"
		      'italic)
     "\n
Emacs 18 has been around for a long, long time.  Version 19 was
supposed to be the successor to v18 with X support.  It was going to
be available \"real soon\" for a long time (some people remember
hearing about v19 as early as 1984!), but it never came out.  v19
development was going very, very slowly, and from the outside it
seemed that it was not moving at all.  In the meantime other people
gave up waiting for v19 and decided to build their own X-aware
Emacsen.  The most important of these was probably Epoch, which came
from the University of Illinois (\"UofI\") and was based on v18.

Around 1990, the Developer Products group within Sun Microsystems
Inc., decided that it wanted an integrated editor.  (This group is now
known as DevPro.  It used to be known as SunPro - the name was changed
in mid-1994.)  They contracted with the University of Illinois to
provide a number of basic enhancements to the functionality in Epoch.
UofI initially was planning to deliver this on top of Epoch code.

In the meantime, (actually some time before they talked with UofI)
Lucid had decided that it also wanted to provide an integrated
environment with an integrated editor.  Lucid decided that the Version
19 base was a better one than Version 18 and thus decided not to use
Epoch but instead to work with Richard Stallman, the head of the Free
Software Foundation and principal author of Emacs, on getting v19 out.
At some point Stallman and Lucid parted ways.  Lucid kept working and
got a v19 out that they called Lucid Emacs 19.

After Lucid's v19 came out it became clear to us (the UofI and Sun)
that the right thing to do was to push for an integration of both
Lucid Emacs and Epoch, and to get the deliverables that Sun was asking
from the University of Illinois on top of this integrated platform.
Until 1994, Sun and Lucid both actively supported XEmacs as part of
their product suite and invested a comparable amount of effort into
it.  Substantial portions of the current code have originated under
the support of Sun, either directly within Sun, or at UofI but paid
for by Sun.  This code was kept away from Lucid for a while, but later
was made available to them.  Initially Lucid didn't know that Sun was
supporting UofI, but later Sun was open about it.

Around 1992 DevPro-originated code started showing up in Lucid Emacs,
starting with the infusion of the Epoch redisplay code.  The separate
code bases at Lucid, Sun, and the University of Illinois were merged,
allowing a single XEmacs to evolve from that point on.

Sun originally called the integrated product ERA, for \"Emacs
Rewritten Again\".  SunPro and Lucid eventually came to an agreement
to find a name for the product that was not specific to either
company.  An additional constraint that Lucid placed on the name was
that it must contain the word \"Emacs\" in it -- thus \"ERA\" was not
acceptable.  The tentatively agreed-upon name was \"XEmacs\", and this
has been the name of the program since version 19.11.)

As of 1997, Sun is shipping XEmacs as part of its Developer Products
integrated programming environment \"Sun WorkShop\".  Sun is
continuing to support XEmacs development, with focus on
internationalization and quality improvement.\n\n"
     (about-with-face "Lucid goes under" 'italic)
     "\n
Around mid-'94, Lucid went out of business.  Lucid founder Richard
Gabriel's book \"Patterns of Software\", which is highly recommended
reading in any case, documents the demise of Lucid and suggests
lessons to be learned for the whole software development community.

Development on XEmacs, however, has continued unabated under the
auspices of Sun Microsystems and the University of Illinois, with help
from Amdahl Corporation and INS Engineering Corporation.  Sun plans to
continue to support XEmacs into the future.\n\n"
     (about-with-face "The Amdahl Corporation point of view"
		      'italic)
     "\n
Amdahl Corporation's Storage Products Group (SPG) uses XEmacs as the
focal point of a environment for development of the microcode used in
Amdahl's large-scale disk arrays, or DASD's.  SPG has joint ventures
with Japanese companies, and decided in late 1994 to contract out for
work on XEmacs in order to hasten the development of Mule support
\(i.e. support for Japanese, Chinese, etc.) in XEmacs and as a gesture
of goodwill towards the XEmacs community for all the work they have
done on making a powerful, modern, freely available text editor.
Through this contract, Amdahl provided a large amount of work in
XEmacs in the form of rewriting the basic text-processing mechanisms
to allow for Mule support and writing a large amount of the support
for multiple devices.

Although Amdahl is no longer hiring a full-time contractor, they are
still funding part-time work on XEmacs and providing resources for
further XEmacs development.\n\n"
     (about-with-face "The INS Engineering point of view"
		      'italic)
     "\n
INS Engineering Corporation, based in Tokyo, bought rights to sell
Energize when Lucid went out of business.  Unhappy with the
performance of the Japanese support in XEmacs 19.11, INS also
contributed to the XEmacs development from late 1994 to early
1995.\n")
    (about-finish-buffer)))

(defun about-advantages (&rest ignore)
  (unless (about-get-buffer "*About Advantages*")
    (let ((title "XEmacs Advantages over GNU Emacs"))
      (widget-insert
       "\n"
       (about-center title)
       (about-with-face title 'bold)))
    (widget-insert
     "\n
* Much better GUI support:

  -- a real toolbar
  -- more comprehensive and better-designed menubars
  -- horizontal and vertical scrollbars in all windows
  -- proper dialog boxes
  -- tabs for selecting buffers
  -- support for variable-width and variable height fonts
  -- support for arbitrary pixmaps and widgets in a buffer
  -- face support on TTY's, including color

* An installable package system, with a huge number of packages available
  that have been tested and are known to work with the latest version
  of XEmacs.

* Comprehensive support for the GTK toolkit.

* An open development community, with contributions welcome and no need
  to sign over your copyright to any organization. (Please send
  contributions to xemacs-patches@xemacs.org.  See http://www.xemacs.org
  for more information on XEmacs mailing lists, and other info.)

* Support for display on multiple simultaneous X and/or TTY devices.

* Powerful, flexible control over the display characteristics of most
  of the visual aspects of XEmacs through the use of specifiers, which
  allow separate values to be specified for individual buffers,
  windows, frames, devices, device classes, and device types.

* A clean, modern, abstracted Lisp interface to the menubar, toolbar,
  window-system events, key combinations, extents (regions in a buffer
  with specific properties), and all other display aspects.

* Proper integration with Xt and Motif (including Motif menubars and
  scrollbars).  Motif look-alike menubars and scrollbars are provided
  for those systems without real Motif support.

* Many improvements to the multilingual support, such as the ability to
  enter text for complex languages using the XIM mechanism and
  localization of menubar text for the Japanese locale.
\n\n")
    (about-finish-buffer)))

(defvar about-glyphs nil
  "Cached glyphs")

;; Return a maintainer's glyph
(defun about-maintainer-glyph (who)
  (let ((glyph (cdr (assq who about-glyphs))))
    (unless glyph
      (let ((file (expand-file-name
		   (concat (symbol-name who)
			   (if (memq (device-class)
				     '(color grayscale))
			       "" "m")
			   ".png")
		   (locate-data-directory "photos")))
	    (data nil))
	(setq glyph
	      (cond ((stringp data)
		     (make-glyph
		      (if (featurep 'png)
			  `([png :data ,data]
			    [string :data "[Image]"])
			`([string :data "[Image]"]))))
		    ((eq data 'error)
		     (make-glyph [string :data "[Error]"]))
		    (file
		     (make-glyph
		      (if (featurep 'png)
			  `([png :file ,file]
			    [string :data "[Image]"])
			`([string :data "[Image]"]))))
		    (t
		     (make-glyph [nothing]))))
	(set-glyph-property glyph 'baseline 100)
	;; Cache the glyph
	(push (cons who glyph) about-glyphs)))
    glyph))

;; Insert personal info about a maintainer.  See also
;; `about-hacker-contribution'.  Note that the info in
;; `about-hacker-contribution' is automatically displayed in the
;; person's own page, so there is no need to duplicate it.
(defun about-personal-info (entry)
  (ecase (car entry)
    ;; you can sort the stuff below with something like
    ;;(sort-regexp-fields nil
    ;; " *(\\([^()]\\|([^()]*)\\|(\\([^()]\\|([^()]*)\\)*)\\)*)\n"
    ;; " *(\\([a-z]*\\)"
    ;; (region-beginning) (region-end))
    (adrian
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (aj
     (widget-insert "\
I'm a software developer working for the SuSE Labs of the Linux
distributor SuSE.  My main task is to improve the GNU C library.")
     (widget-insert ".\n"))
    (ajc
     (widget-insert "\
When not helping maintain the XEmacs website, Andrew is a Network
Software Engineer(tm) for Monash University in Australia, maintaining
webservers and doing random other things.  As well as spending spare
time being an Eager Young Space Cadet and fiddling with XEmacs/Gnus
et. al., he spends his time pursuing, among other things, a Life.
Some of this currently involves doing an A-Z (by country) of
restaurants with friends, and has, in the past, involved dyeing his
hair various colours (see ")
     (about-url-link 'ajc nil "Visit Andrew's home page")
     (widget-insert ".\n"))
    (alastair
     (widget-insert
      "\
Alastair, apart from being an all-round hacker, occasional contributor
to free software projects and general good egg(!), currently works for
Telsis, a manufacturer of telephony equipment on the south coast of
England.  He'd quite like to have his own company one day, but has yet
to think of that killer product...

See also ")
        (about-url-link 'alastair nil "Visit Alastair's home page")
        (widget-insert ".\n"))
    (baw
     (widget-insert "\
As of November 2000, I am a software engineer with the Pythonlabs at
Digital Creations.  Pythonlabs is the core team developing and
maintaining the Python open source, object-oriented scripting
language.  Digital Creations is the publisher of Zope, an open source
content management system written in Python.

In addition to my Python and Zope work, I am lead developer for the
GNU Mailman project, a mailing list management system written,
naturally, in Python.  See the trend?

On the side I play bass with a number of Washington DC area bands and
also write poems about cows, milk, and fathers.  Here's a sample, and
drop me an email if you live in the NYC to Charlotte region; I'll let
you know when the band's playing in your area.  It'd be cool to meet
you, and talking about XEmacs would make my wife very happy by helping
to fend off the legions of groupies that seem to follow me everywhere.

    Milk Me Daddy
    (C) 1990 Warsaw
    ===============
    Oh daddy with your fingers pink 
    From whose udders do you drink? 
    Thy milk offends with putrid stink 
    I'll vomit now, lactose I think 

    If I could dream, I'd be a cow 
    Not horse, or mule, or barnyard sow 
    The cud I'd chew would drip and how! 
    So milk me daddy, milk me now! 

    My bovine nature knows no bounds 
    I'd naught awake at midnight sounds 
    Of teens approaching o'er the grounds 
    To tip with glee, then screech like clowns 

    And so I stare into this glass 
    Of sweaty juice, I gulp so fast 
    Each drop I lick, down to the last 
    The vertigo I know will pass 

    My mother smiles and pats my head 
    She's proud of me, so she has said 
    My pop just now gets out of bed 
    His eyes quite comatose and red 

    He'll empathize my milky fate 
    Whilest sopping gravy from his plate 
    And as the hour is getting late 
    His belly taut with all he ate 

    He isn't often quite so chatty 
    His arteries clogged with meat so fatty 
    With burps that launch soup, thick and splatty 
    Oh how I wish you'd milk me daddy\n\n\t")
     (about-url-link 'baw nil "Visit Barry's home page")
     (widget-insert "\n"))
    (ben
     (widget-insert
      "\
Since September 1992, I've worked on XEmacs as a contractor for
various companies and more recently as an unpaid volunteer.

Alas, life has not been good to me recently.  This former San
Francisco \"Mission Critter\" developed insidious hand and neck
problems after a brief stint working on a Java-based VRML toolkit for
the now defunct Dimension X, and I was forced to quit working.  I was
exiled first to \"Stroller Valley\" and later all the way to Tucson,
Arizona, and for two years was almost completely disabled due to pain.
More recently I have fought my way back with loads and loads of
narcotic painkillers, and currently I'm an art student at the
University of Arizona.\n\n")
     (widget-insert "Architecting XEmacs: ")
     (about-url-link 'ben-xemacs nil "Find the miracles in store for XEmacs")
     (widget-insert "\nBen's home page:     ")
     (about-url-link 'ben nil "Visit Ben's page")
     (widget-insert "\n"))
    (bw
     (widget-insert "\
His interests include user interfaces, information management, CASE
tools, communications and enterprise integration.\n"))
    (cgw
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (chr
     (widget-insert "\
Christian is a student at the Norwegian School of Economics and
Business Administration in Bergen, Norway.  He used to work for an
internet startup called New Media Science, doing scripting and
violation of HTML DTD's.  After graduation, spring 1999, he'll be
looking for a job involving lisp programming, French and Russian.\n"))
    (craig
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (cthomp
     (widget-insert "\
Chuck is a senior system and network administrator for the Computer
Science department at the Unversity of Illinois.  In one previous life
he spent every waking hour working on XEmacs.  In another he dabbled
as a project manager for a streaming video startup (RIP).  His current
reason for not having time to contribute to XEmacs is the Thompson
Twins.\n"))
    (daiki
     (about-url-link 'daiki nil "Visit Daiki's page"))
    (dan
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (darrylo
     (widget-insert
      "\
Perennial Emacs hacker since 1986 or so, when he first started on GNU
Emacs 17.something.  Over the years, he's developed \"OEmacs\", the first
version of GNU Emacs 19 for MSDOS, and \"bigperl\", a 32-bit version of
Perl4 for MSDOS.  In recent years, reality has intruded and he no longer
has much time for playing with cool programs.  What little time he has
now goes to XEmacs hacking, where he's worked on speeding up dired under
MS Windows, and to feeding his two cats.\n"))
    (devin
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (dkindred
     (widget-insert "\
Darrell is currently a doctoral student in computer science at
Carnegie Mellon University, but he's trying hard to kick that
habit.

See ")
     (about-url-link 'dkindred nil "Visit Darrell's WWW page")
     (widget-insert ".\n"))
    (dmoore
     (widget-insert "\
David is a student in the Computer Systems Laboratory at UCSD.  When
he manages to have free time, he usually spends it on 200 mile bicycle
rides, learning German or showing people the best mail & news
environment he's found in 10 years.  (That'd be XEmacs, Gnus and bbdb,
of course.)  He can be found at `druidmuck.egbt.org 4201' at various
hours of the day.

He has a page at ")
     (about-url-link 'dmoore nil "Visit David's home page")
     (widget-insert ".\n"))
    (dv
     (widget-insert "\
I graduated at ENST (an engineering school in Paris) and have a Ph.D.
in computer science. I'm currently a teacher at EPITA (another
engineering school, still in Paris) and a researcher at LRDE (EPITA's
research and development laboratory). Our research topics include
generic programming and distributed virtual reality.

Apart from XEmacs, I'm also involved in other free software projects,
including Gnus, BBDB, and the GNU \"autotools\". I also wrote some
LaTeX packages (ugh :-).

All of this, actually, is only 60% true. Two days per week, I'm also a
semi-professional Jazz guitar player (and singer), which means that it
is not the way I earn my crust, but things may very well reverse in
the future ...\n\n")
     (widget-insert "Visit Didier's home page: ")
     (about-url-link 'dv nil "Visit Didier's home page")
     (widget-insert "\n"))
    (eb
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (fabrice
     (widget-insert
      "\
I'm a computer science researcher and teacher in a French electrical
engineering institution called Supelec. My fields of interest are
symbolic artificial intelligence, theoretical computer science, functional
languages ... and TeX.

Lately, my hacking time has been devoted to porting the Web2C/teTeX
distribution of TeX for Unix to Win32, and I'm still maintaining it.
It is included in the TeX Live cdrom edited by Sebastian Rahtz.\n")
     (widget-insert "Visit fpTeX home page: ")
     (about-url-link 'fptex nil "Visit fpTeX home page")
     (widget-insert "\nFabrice's home page:   ")
     (about-url-link 'fabrice nil "Visit Fabrice's page")
     (widget-insert "\n"))
    (golubev
     (widget-insert
      "\
I appreciate power of XEmacs, but elementary editing operations should
be done by single keystrokes with no modifiers.  So would not use
XEmacs until discovered viper, and now can't live without viper.
Occasionally dislike something in there or in other free software, and
try to get it fixed.  .plan file contains classic (perhaps reinvented
independently) formula:

Hacking world for ever

(borrowed from \"Hacking X for Y\" in ")
     (about-url-link "http://www.jargon.org/"
		     "Jargon File" "www.jargon.org")
     (widget-insert ").\n"))
    (gunnar
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (hbs
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (hisashi
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (hmuller
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (hniksic
     (widget-insert "\
Hrvoje thinks he works in the server-side web business.  In reality,
he cranks out huge quantities of HTML, Tcl, and Java for the German
branch of ")
     (about-url-link "http://www.arsdigita.com/"
		     "ArsDigita, Inc." "www.arsdigita.com")
     ;; Avoid literal I18N characters in strings.  *Displaying* a
     ;; Latin 1 character should always be safe, though, with or
     ;; without Mule.
     (let ((muenchen (format "M%cnchen" (make-char 'latin-iso8859-1 252))))
       (widget-insert (format "\
  He joined the ranks of Gastarbeiters only
recently; he is trying to learn German and get attuned to %s
and Bav^H^H^HGermany.\n" muenchen)))

     (widget-insert "\

Before ArsDigita, he worked as a programmer at ")
     (about-url-link "http://www.iskon.hr/" "Iskon," "www.iskon.hr")
     (widget-insert " a fast-growing
Croatian ISP.  Even before that, he worked part-time for academic
institutions like ")
     (about-url-link "http://www.srce.hr/" "SRCE" "www.srce.hr")
     (widget-insert " and ")
     (about-url-link "http://www.carnet.hr/" "CARNet," "www.carnet.hr")
     (widget-insert " and tried to attend university.

He takes perverse pleasure in building and maintaining free software
in his free time.  Apart from XEmacs, his major contribution is ")
     (about-url-link 'wget "Wget," "Wget home page")
     (widget-insert "
his very own creation, now jointly maintained by a happy crew.

He dreams of having a home page.\n"))
    (hobley
     (widget-insert "\
I used to do real work, but now I am a Project Manager for one of the
Telco's in Australia. In my spare time I like to get back to basics and
muck around with things. As a result I started the NT port. Hopefully I
will get to finish it sometime sooner rather than later. I do vaguely
remember University where it seems like I had more spare time that I can
believe now. Oh well, such is life.\n"))
    (jan
     (widget-insert "\
Jan Vroonhof has been using XEmacs since he needed to write .tex files
for his work as a physics and maths student at the Univerisity of Leiden.
His XEmacs hacking started when XEmacs kept freezing up under a his
window manager. He submitted a fix and has been hooked every since.

XEmacs has followed him first to Switzerland where he did a maths
doctorate at the ETH in Zurich, working on a conjecture by Migdal on
the behavior of vertex corrections in Electron-Phonon theory.  Finally
sharing a house with his loved one, he now lives in Oxford (UK)
working on the Jeode Java Virtual Machine, which like XEmacs is
portable, implements a language, includes a non-trivial bit of
graphics and a garbage collector, but is multithreaded to boot!
Unfortunately his XEmacs time is directly limited by the amount of
traffic on the M40.\n"))
    (jareth
     (widget-insert "\
Jareth Hein is a mountain boy who abandoned his home state of Colorado
for the perpetual state of chaos known as Tokyo in a failed attempt to
become a cel-animator, and a more successful one to become a
computer-game programmer. As he happens to be bilingual (guess which
two?) he's been doing quite a bit of MULE hacking.  He's also getting
his hands dirty in the graphics areas as well.\n"))
    (jason
     (widget-insert "\
Jason resides in Northern New Mexico where he works as a Systems
Scientist(tm) in the Los Alamos National Laboratory's Advanced
Computing Group.

See: ")
      (about-url-link 'jason nil "Visit Jason's homepage")
      (widget-insert ".\n"))
    (jens
     (widget-insert "\
I'm currently working for 1&1 Internet AG, a large Domain and Webspace
Provider in Germany and Europe.  I do mostly Java/XML/OO/Component
stuff today.  I'm interested EJB, Corba and other middleware or
distributed Systems.  Besides work, I occasionally hack on The Gimp
and other gtk/gnome related projects.  Maybe the advent of XEmacs/Gtk
will get me back to spend some time again hacking on XEmacs in the
near future.\n"))
    (jmiller
     (widget-insert "\
Jeff grew up in Indiana and is a country boy at heart.  He currently
lives in, of all places, Millersville Maryland.  He spends a lot of
his free time tinkering with Linux and hacking on XEmacs and loves it
when he finds new cool features in either.  When he's not doing that,
he enjoys downhill skiing, puzzles, and sci-fi.  Jeff is also really
interested in classical Roman history and enjoys making trips to
Italy, where he was born, and seeing the sights")
     (widget-insert ".\n"))
    (jonathan
     (widget-insert "\
I work for Symbian Ltd in London, England, looking after low-level
kernel, peripheral and toolchain stuff for the EPOC OS.

I've been using XEmacs since 1994, but didn't start hacking on it
until late 1997 when I started working at Symbian, a Windows-only
company, and felt lost without my favourite editing environment.\n"))
    (juhp
     (widget-insert "\
Jens was born in Copenhagen, grew up in Britain and is now living in
Japan.  He started using XEmacs 20 (instead of Emacs) as his
work-environment in June 1997 while still an EU postdoc at RIMS, Kyoto
University, and quickly got involved in XEmacs development.  Recently
he is getting into Haskell, a very nice pure functional programming
language.

")
     (about-url-link 'juhp nil "Visit Jens' homepage")
     (widget-insert "\n"))
    (jwz
     (widget-insert
      "\t"
      (about-with-face "\"So much to do, so little time.\"" 'italic)
      "\n
Jamie Zawinski was primarily to blame for Lucid Emacs from its
inception in 1991, to 1994 when Lucid Inc. finally died.  After that,
he was one of the initial employees of Netscape Communications, writing
the first Unix version of Netscape Navigator, and designing and
implementing the first version of the Netscape Mail and News readers.
He then helped create and run ")
     (about-url-link "http://www.mozilla.org/"
		     "mozilla.org"
		     "Visit The Mozilla Organization")
     (widget-insert " for its first two years,
until America Online bought Netscape Communications, at which point he
gave up in disgust and dropped out of the computer industry entirely.

He now runs a ")
     (about-url-link "http://www.dnalounge.com/"
		     "nightclub"
		     "Visit The DNA Lounge")
     (widget-insert " in San Francisco, and occasionally writes
screen savers.\n\n")
     (widget-insert "Visit jwz's ")
     (about-url-link 'jwz "home page" "Visit jwz's home page")
     (widget-insert ".\n"))
    (kazz
     (widget-insert "\
Kazz is the XEmacs lead on BSD (especially FreeBSD).
His main workspace is, probably, the latest stable version of
FreeBSD and it makes him comfortable and not.
His *mission* is to make XEmacs runs on FreeBSD without
any problem.

In real life, he is working on a PDM product based on CORBA,
and doing consultation, design and implemention.
He loves to play soccer, yes football!
See also:")
     (about-url-link 'kazz nil "Visit Kazz's home page")
     (widget-insert ".\n"))
    (kirill
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (kyle
     (widget-insert "\
See\n")
     (about-url-link 'kyle nil "Visit Kyle's Home page")
     (widget-insert ".\n"))
    (larsi
     (widget-insert "\
Lars's day job is as the head of the IT department of a Norwegian
Internet stock broker.  He claims no responsibility for the Dot
Com Bomb, but he snickers a lot.

See ")
     (about-url-link 'larsi nil "Visit the Larsissistic pages")
     (widget-insert ".\n"))
    (marcpa
     (widget-insert "\
I work for Positron Industries Inc., Public Safety Division.
I'm part of the team producing POWER 911, a 911 emergency response
system written in Modula3:\n")
     (about-url-link 'marcpa nil "Visit POWER 911")
     (widget-insert "\
\n\nPreviously, I worked at Softimage Inc., now a Microsoft company
\(eeekkk!), as a UNIX system administrator.  This is where I've been
converted to NT.

In a previous life, I was a programmer/sysadmin at CRIM (Centre de
Recherche Informatique de Montreal) for the speech recognition group.\n"))
    (martin
     (widget-insert "\
Martin was the XEmacs guy at DevPro, a part of Sun Microsystems.
Martin used to do XEmacs as a `hobby' while at IBM, and was crazy
enough to try to make a living doing it at Sun.

Martin starting using Emacs originally not to edit files, but to get
the benefit of shell mode. He actually used to run nothing but a shell
buffer, and use `xterm -e vi' to edit files.  But then he saw the
light.  He dreams of rewriting shell mode from scratch.  Stderr should
show up in red!!

Martin is no longer doing XEmacs for a living, and is Just Another
Volunteer.\n"))
    (mcook
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (mly
     (widget-insert "Cars are evil.  Ride a bike.\n"))
    (morioka
     (widget-insert "\
I am a doctoral student at School of Information Science of JAIST
\(Japan Advanced Institute of Science and Technology, Hokuriku).  I'm
interested in Natural Language, Affordance and writing systems.\n"))
    (mta
     (widget-insert
      "\
I am a software developer who worked for the University of Michigan
for many years where I was one of the principal architects of the
Michigan Terminal System.  For the last several years I've been
working for Arbortext, a publisher of XML publishing and content
management software.\n"))
    (ograf
     (widget-insert "\
I'm a student of computer sciences at the University of Koblenz. My
major is computational linguistics (human language generation and
analysis).

I make my living as a managing director of a small but fine company
which I started two years ago with one of my friends. We provide
business network solutions based on linux servers and various other
networking products.

Most of my spare time I spent on the development of the XEmacs
Drag'n'Drop API, a enhanced version of Tk called TkStep (better looks,
also Drag'n'Drop, and more), and various other hacks: ISDN-tools,
cd players, python, etc...

To see some of these have a look at ")
     (about-url-link 'ograf nil "one of my homepages")
     (widget-insert ".\n"))
    (olivier
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (oscar
     (widget-insert "\
Oscar heads the Computer Science department at CPE Lyon, a french
engineering school in France. Besides his administrative tasks he
teaches networking basics, Internet technologies (you know, all these
xxML and hairy script languages !)  and the Scheme language.\n"))
    (pelegri
     (widget-insert
      "\
I did my PhD at UCB and a postdoc at CSL/PARC.  I joined Sun in 1990,
spent some time in DevPro (that is when I made my contribution to
XEmacs) and joined JavaSoft in fall '95, where I've been the lead for
several JSP-related specifications and JAX-RPC.  I'm currently the Web
Layer architect for J2EE. 

I was born in Barcelona and I grew up mostly in Caracas; I have two kids
and I speak only catalan to them; I can juggle some (career, family, and
4 balls or 3 pins); and my english can be idiosyncratic!.\n"))
    (pez
     (widget-insert "\
Peter currently serves as Senior Vice President, Product Development
for CBS SportsLine.  See ")
     (about-url-link 'pez nil "CBS SportsLine")
     (widget-insert ".\n"))
    (piper
     (widget-insert "\
My home page is here:\n")
     (about-url-link 'piper nil "Visit andy's home page")
     (widget-insert "\n
 Andy has been active in the XEmacs team for a number of years,
helping port XEmacs to MS Windows operating systems. He is also the
current MS Windows release manager and maintains the MS Windows
netinstaller.\n"))
    (pittman
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (rickc
     (widget-insert "\
The hacker formerly known as Rick Busdiecker is a developer and
technical manager at Deutsche Bank in New York during daylight hours.
In the evenings he maintains three children, and when he ought to be
sleeping he builds XEmacs betas, and tinkers with various personal
hacking projects.\n"))
    (rose
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (rossini
     (widget-insert "\
Current development lead for ESS (Emacs Speaks Statistics), a mode and
inferior mode for statistical programming and data analysis for SAS,
S, S-PLUS, R, XLispStat; configurable for nearly any other statistical
language/package one might want.  In spare time, chases his son around
and acts as a Ph.D. (bio)statistician for money and amusement,
primarily focusing on statistical computing, visualization, and the
design and analysis of HIV vaccine trials.  Current position: Research
Assistant Professor of Biostatistics at the University of Washington
and the Fred Hutchinson Cancer Research Center.

See ")
     (about-url-link 'rossini nil "Visit Anthony's home page")
     (widget-insert ".\n"))
    (slb
     (widget-insert "\
Peaches Baur, 1986-1999.
Rest in peace")
     (widget-insert ".\n"))
    (sperber
     (widget-insert "\
When Mike isn't busy putting together patches for free software he has
just installed or changing his hairstyle, he does research in modern
programming languages and their implementation, and hopes that one day
XEmacs will speak Scheme.\n"))
    (stig
     (widget-insert "\
Peripatetic uninominal Emacs hacker.  Stig sometimes operates out of a
big white van set up for nomadic living and hacking.  Stig is sort of
a tool fetishist.  He has a hate/love relationship with computers and
he hacks on XEmacs because it's a good tool that makes computers
somewhat less of a nuisance.  Besides XEmacs, Stig especially likes
his Leatherman, his Makita, and his lockpicks.  Stig wants a MIG
welder and air tools.

Stig likes to perch, hang from the ceiling, and climb on the walls.
Stig has a cool van.  Stig would like to be able to telecommute from,
say, the north rim of the Grand Canyon or the midst of Baja.\n"))
    (stigb
     (widget-insert "\
Currently studying computer science in Trondheim, Norway.  Full time
Linux user and proud of it.  XEmacs hacker light.

See:\t")
     (about-url-link 'stigb nil "Visit Stig's home page"))
    (thiessel
     (widget-insert "\
Worked at University of Kaiserslautern where he took part in the
development and design of a CAD framework for analog integrated
circuits with special emphasis on distributed software concepts. He
has now joined HP as technical consultant.

                      All of the buildings,
                      all of the cars
                      were once just a dream
                      in somebody's head.\n
                                     P. Gabriel\n"))
    (tomonori
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (tuck
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (turnbull
     (widget-insert "\
Stephen lives with his Japanese wife and children in Tsukuba, Japan,
where he is a professor of economics at the University of Tsukuba.\n"))
    (vin
     (widget-insert "\
I'm a software engineer and manager for Teradyne in Boston.  I used
to play a lot of Ultimate - see ")
     (about-url-link 'vin nil "Visit the Ultimate Players Association homepage")
     (widget-insert " for more details.
Nowadays I'm a family man, so I spend a lot of time with my wife,
Becky, and my son, Noah.\n"))
    (vladimir
     (widget-insert "\
Former technical lead for XEmacs at Sun.  He is now writing a book on
distributed Java and is working at Xerox PARC documenting AspectJ, a
light-weight extension to Java that supports crosscutting concerns.
See ")
     (about-url-link 'vladimir nil "Visit Vladimir's home page")
     (widget-insert ".\n"))
    (wmperry
     (widget-insert "\
Happily living in Indiana telecommuting for a company based in Seattle
\(who I now prefer not to name), wishing I was in Ireland instead.\n"))
    (yoshiki
     (widget-insert
      "\
Sorry, no personal information available about me yet.\n"))
    (youngs
     (widget-insert "\
I live in Brisbane, Australia with my wife, Michelle and our daughter,
Kaitlyn.  I've only been hacking XEmacs for a short time (approx 18
mths), but I've been fooling around with computers since the early
80's.

In the past, I've been a bank officer, car salesman, insurance agent,
managed a computer firm and owned and operated my own business.  I now
divide my time between my family, planning my next business idea (a
computer consulting firm that uses zero Microsoft products), looking
after the XEmacs Packages and hacking my own XEmacs package, Eicq.

\tSee: ")
     (about-url-link 'youngs nil "Visit the Eicq homepage")
     (widget-insert ".\n"))
    ))

;; Insert info about a maintainer's contribution to XEmacs.  See also
;; `about-personal-info'.
(defun about-hacker-contribution (entry)
  (ecase (car entry)
    ;; to sort the entries below, use M-x sort-regexp-fields RET
    ;; then this regexp: ([^(]*([^"]*"[^"]*"[^)]*))
    ;; then this regexp: (\([a-z]*\)
    (adrian
     (widget-insert
      "\
Adrian has done invaluable work rewriting and maintaining the XEmacs
web pages at www.xemacs.org.  During his tenureship, he has
established a consistent look and feel, placed the web pages under
CVS, set up maintenance procedures, written scripts to handle
automatic updating, validation and mirroring, and done innumerable
other tasks.  He has also helped with many other administrative tasks,
such as the thankless work of dealing with the providers of resources
to XEmacs at SourceForge and tux.org.\n"))
    (aj
     (widget-insert "\
Former `Package Patch Tender', beta tester and GNU libc developer.\n"))
    (ajc
     (widget-insert "\
Former XEmacs web site maintainer.\n"))
    (alastair
     (widget-insert
      "\
Rewrote the selection code, adding many new features such as better
support for arbitrary selection types (especially under MS Windows,
where the full power of the clipboard system is available under
XEmacs).\n"))
    (baw
     (widget-insert "\
I'm the author of ")
     (about-url-link 'cc-mode "CC Mode" "Visit the CC Mode page")
     (widget-insert ", for C, C++, Objective-C and Java editing,
Supercite for mail and news citing, and sundry other XEmacs packages
such as ELP (the Emacs Lisp Profiler), Reporter, xrdb-mode, and
winring.  Even though I still live almost 100% in XEmacs these days,
my Lisp hacking has fallen off in recent years as I became more
involved in Python, and in fact, I currently maintain the Python
editing mode.  See also: ")
     (about-url-link "http://www.python.org/emacs" nil
		     "Visit the python.org Emacs Goodies page")
     (widget-insert ".\n"))
    (ben
     (widget-insert
      "\
I am the largest code contributor to XEmacs, and the architect of many
of the features that distinguish XEmacs from GNU Emacs and other Emacs
versions.  My main contributions to XEmacs include rewriting large
parts of the internals and the gory Xt/Xlib interfacing, adding the
Mule \(international) support, improving the MS Windows support,
adding many GUI features to XEmacs, architecting the
device-abstraction and specifier code, writing most of the XEmacs
Internals Manual and the XEmacs-specific parts of the XEmacs Lisp
Reference Manual, synching a great deal of code with GNU Emacs, and
being a general nuisance ... er, brainstormer for many of the new
features of XEmacs.\n"))
    (bw
     (widget-insert "\
Author of the Hyperbole everyday information management hypertext
system and the OO-Browser multi-language code browser.  He also
designed the BeOpen InfoDock integrated development environment
for software engineers.  It runs atop XEmacs and is available from
his firm, BeOpen, which offers distributions, custom development,
support, and training packages for corporate users of XEmacs, GNU
Emacs and InfoDock.  See ")
     (about-url-link 'beopen nil "Visit BeOpen WWW page")
     (widget-insert ".\n"))
    (cgw
     (widget-insert
      "\
Author of an earlier version of the MS Windows setup program for XEmacs.\n"))
    (chr
     (widget-insert "\
Maintainer of the XEmacs FAQ and proud author of `zap-up-to-char'.\n"))
    (craig
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (cthomp
     (widget-insert
      "\
Maintainer of XEmacs from mid-1994 through 1996.  Author of the
redisplay engine, the original toolbar and scrollbars and some of the
device-abstraction, TTY and glyph code.  Creator of the xemacs.org
domain and comp.emacs.xemacs.\n"))
    (daiki
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (dan
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (darrylo
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (devin
     (widget-insert "\
Part of the original (pre-19.0) Lucid Emacs development team.
Matthieu wrote the initial Energize interface, designed the
toolkit-independent Lucid Widget library, and fixed enough redisplay
bugs to last a lifetime.  The features in Lucid Emacs were largely
inspired by Matthieu's initial prototype of an Energize interface
using Epoch.\n"))
    (dkindred
     (widget-insert "\
Darrell tends to come out of the woodwork a couple of weeks
before a new release with a flurry of fixes for bugs that 
annoy him.  He hopes he's spared you from a core dump or two.\n"))
    (dmoore
     (widget-insert "\
David has contributed greatly to the quest to speed up XEmacs.\n"))
    (dv
     (widget-insert "\
I joined the development of XEmacs in 1996, and have been one of the
core maintainers since 1998. Although I'm mostly interested in the
GUI, ergonomics, redisplay and autoconf issues, it's probably simpler
to describe what I'm *not* involved in: I've never touched the Lisp
implementation, and I probably never will...

I'm the author of the multicast support, I wrote and maintain some
external Emacs Lisp packages (including mchat) and I'm also
responsible for some of the core Lisp code (including the rectangle
library which I rewrote for both XEmacs and GNU Emacs).\n"))
    (eb
     (widget-insert "\
Also part of the original Lucid Emacs development team.  Eric played a
big part in the design of many aspects of the system, including the
new command loop and keymaps, fixed numerous bugs, and has been a
reliable beta tester ever since.\n"))
    (fabrice
     (widget-insert
      "\
I have started to provide binary kits for the 21.2 series when there
was no installer available. I contributed a few lines of core code
occasionally to make things smoother with the native win32 port which
I'm using all the day.

I also contributed elisp code long ago to make Gnus run under XEmacs.\n"))
    (golubev
     (widget-insert
      "\
Used XEmacs since early 1997.  Fixed bugs that annoy me, both in
XEmacs core and in packages I use, mostly viper.  Hoping to get
coding-cookie package distributed, which is also a fix of what I
consider a bug.\n"))
    (gunnar
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))	
    (hbs
     (widget-insert "\
Part of the original (pre-19.0) Lucid Emacs development team.  Harlan
designed and implemented many of the low level data structures which
are original to the Lucid version of Emacs, including extents and hash
tables.\n"))
    (hisashi
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (hmuller
     (widget-insert "\
Author of the code used to connect XEmacs with ToolTalk, and of an
early client of the external Emacs widget.\n"))
    (hniksic
     (widget-insert
      "\
Hrvoje's contribution to XEmacs consists of many hours spent working
on code and taking part in public discussions.

He wrote `savehist' and `htmlize' packages, the latter having a pretty
large gathering of users.  He worked to improve many parts of XEmacs
Lisp code, including isearch (FSF synch and new features), cl, edmacro
\(FSF synch and an almost complete rewrite), profile, gnuserv,
hyper-apropos, etags, about, and custom.

He has worked on improving and optimizing the C core.  He ported many
FSF core features such as indirect buffers, tty-erase-char,
save-current-buffer and friends, debug-ignored-errors, etc.  He also
wrote line numbering optimizations for large buffers, initial support
for TTY frames, abbrev improvements, Lisp printer and reader
improvements, support for extent modification functions, and lots of
minor bugfixes, optimizations, and Muleifications.

He contributed to Lispref and Internals documentation, including a
section on writing Mule-compliant C code.  Maintains NEWS.  He
participated on xemacs-beta since 1996 and on the Patch Review Board
since its inception in 1998.\n"))
    (hobley
     (widget-insert
      "\
Creator of the earliest version of the MS Windows port of XEmacs.\n"))
    (jan
     (widget-insert "\
Apart from hunting down redisplay bugs Jan has worked on such
things as improvements to the package system, implementing lazy-shot,
a short stint at tracking patches and currently acts as a guardian
of the XEmacs custom subsystem and gnuserv.\n"))
    (jareth
     (widget-insert "\
Owner of cvs.xemacs.org, the machine that holds the XEmacs CVS
repository, and author of some of the graphics code in XEmacs.\n"))
    (jason
     (widget-insert "\
Beta tester, manager of the various XEmacs mailing lists and binary
kit manager.  Also, originator and maintainer of the gnus.org domain.\n"))
    (jens
     (widget-insert "\
Jens did the artwork for graphics added to XEmacs 20.2 and 19.15. He's
also the author of \"XEmacs Mine\", a game similar to Minesweeper, but
running in XEmacs\n"))
    (jmiller
     (widget-insert "\
Beta tester and last hacker of calendar.\n"))
    (jonathan
     (widget-insert "\
I started the native port of XEmacs to MS Windows. Author of the
Windows frame, redisplay, face and event loop support.\n"))
    (juhp
     (widget-insert "\
Author of \"find-func.el\", improvements to \"help.el\" and a good
number of bug fixes during June 1997 to December 1998.\n"))
    (jwz
     (widget-insert
      "\
Creator and maintainer of Lucid Emacs (the predecessor of XEmacs),
from 1991 through mid-1994.\n"))
    (kazz
     (widget-insert "\
IENAGA Kazuyuki is the XEmacs technical lead on BSD, particularly
FreeBSD.\n"))
    (kirill
     (widget-insert
      "\
Abstracted the subprocess code and wrote much of the MS Windows
support in XEmacs, including the subprocess interface, dialog boxes,
printing support, and much of the event loop.\n"))
    (kyle
     (widget-insert "\
Author of VM, a mail-reading package that is included in the standard
XEmacs distribution, and contributor of many improvements and bug
fixes.  Unlike RMAIL and MH-E, VM uses the standard UNIX mailbox
format for its folders; thus, you can use VM concurrently with other
UNIX mail readers such as Berkeley Mail and ELM.

Also rewrote the object allocation system in XEmacs to support full
32-bit pointers and 31-bit integers.\n"))
    (larsi
     (widget-insert "\
Author of Gnus the Usenet news and Mail reading package in the
standard XEmacs distribution, and contributor of various enhancements
and portability fixes.\n"))
    (marcpa
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (martin
     (widget-insert
      "\
Beta release manager and author of many stability fixes and speed
improvements in XEmacs.\n"))
    (mcook
     (widget-insert "\
Author of the \"shy groups\" and minimal matching regular expression
extensions.\n"))
    (mly
     (widget-insert "\
Early code contributor to Lucid Emacs.  Synched up Lucid Emacs with
the first actual release of GNU Emacs 19, and architected and wrote
the first version of XEmacs's object allocation system.\n"))
    (morioka
     (widget-insert "\
I am the author of tm-view (general MIME Viewer for GNU Emacs) and
major author and maintainer of tm (Tools for MIME; general MIME
package for GNU Emacs).  In addition, I am working to unify MULE API
for Emacs and XEmacs.  In XEmacs, I have ported many mule features.\n"))
    (mta
     (widget-insert
      "\
Contributed minor improvements to the Windows support, especially
related to subprocess communication and portable dumping as well as
a bit of general bug fixing.\n"))
    (ograf
     (widget-insert "\
Author of the XEmacs Drag'n'Drop API.\n"))
    (olivier
     (widget-insert
      "\
Author of the portable dumper.\n"))
    (oscar
     (widget-insert "\
Oscar's major contributions to XEmacs are the internal LDAP support
and the EUDC package, an interface to query various directory services
in a uniform manner (when composing mail for instance).\n"))
    (pelegri
     (widget-insert "\
Author of EOS, a package included in the standard XEmacs distribution
that integrates XEmacs with the SPARCworks development environment
from Sun.  Past lead for XEmacs at Sun; advocated the validity of
using Epoch, and later Lemacs, at Sun through several early
prototypes.\n"))
    (pez
     (widget-insert "\
Author of SQL Mode, edit-toolbar, mailtool-mode, and various other
small packages with varying degrees of usefulness.\n"))
    (piper
     (widget-insert "\
Author of the Cygwin port of XEmacs including unexec, the widget,
gutter and buffer-tab support, glyphs under MS-Windows, toolbars under
MS-Windows, the original \"fake\" XEmacs toolbar, outl-mouse for mouse
gesture based outlining, and the original CDE drag-n-drop
support.\n"))
    (pittman
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (rickc
     (widget-insert "\
Maintainer of ILISP.\n"))
    (rose
     (widget-insert "\
Author of many extensions to the `extents' code, including the initial
implementation of `duplicable' properties.\n"))
    (rossini
     (widget-insert "\
Author of the first XEmacs FAQ;
Development lead on Emacs Speaks Statistics;
Assisted Jareth Hein with setting up the JitterBug tracking system.\n"))
    (slb
     (widget-insert
      "\
Maintainer of XEmacs from 1996 through 1998.  Author of the package
system.\n"))
    (sperber
     (widget-insert "\
Mike ported EFS to XEmacs 20 and integrated EFS into XEmacs.  He's
also responsible for the ports of facemenu.el and enriched.el, the
code to handle path-frobbing at startup for the XEmacs core and the
package system, the init file migration from .emacs to
.xemacs/init.el, and the CVS Great Trunk Move.\n"))
    (stig
     (widget-insert "\
Implemented the faster stay-up Lucid menus and hyper-apropos.
Contributor of many dispersed improvements in the core Lisp code, and
back-seat contributor for several of its major packages.\n"))
    (stigb
     (widget-insert "\
Maintainer of the RPM package.\n"))
    (thiessel
     (widget-insert "\
Does beta testing and helps take care of the XEmacs web site.\n"))
    (tomonori
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (tuck
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (turnbull
     (widget-insert
      "\
Responsible for getting the current release of XEmacs out the
door.\n"))
    (vin
     (widget-insert "\
Vin maintains the stable version of XEmacs.  This involves reviewing
a lot of other peoples' patches and testing and applying them.
He also gets to generate his own patches from time to time.  Being
release manager is a fun way to contribute to the XEmacs project.
Write me at acs@xemacs.org if you're interested in learning more.\n"))
    (vladimir
     (widget-insert "\
Former technical lead for XEmacs at Sun.\n"))
    (wmperry
     (widget-insert "\
Author of the GTK support in XEmacs, Emacs-w3 (the builtin web browser
that comes with XEmacs), and various additions to the C code (e.g. the
database support, the PNG support, some of the GIF/JPEG support, the
strikethru face attribute support).\n"))
    (yoshiki
     (widget-insert
      "\
Sorry, no information about my XEmacs contributions yet.\n"))
    (youngs
     (widget-insert "\
Maintainer and release manager of the packages.\n"))
    ))

;; Setup the buffer for a maintainer.
(defun about-maintainer (widget &optional event)
  (let* ((entry (assq (widget-value widget) xemacs-hackers))
	 (who (car entry))
	 (name (cadr entry))
	 (address (caddr entry))
	 (bufname (format "*About %s*" name)))
    (unless (about-get-buffer bufname)
      ;; Display the glyph and name
      (widget-insert "\n")
      (widget-create 'default :format "%t"
		     :tag-glyph (about-maintainer-glyph who))
      (widget-insert
       "\n\n" (about-with-face (format "%s" name) 'bold)
       " <")
      (about-mailto-link address)
      (widget-insert ">\n\n")
      ;; Display the actual info
      (about-personal-info entry)
      (widget-insert "\n")
      (widget-insert
       (about-with-face "Contributions to XEmacs:\n\n" 'about-headline-face))
      (about-hacker-contribution entry)
      (widget-insert "\n")
      (about-finish-buffer 'kill)
      (forward-line 2))))

(defsubst about-tabs (str)
  (let ((x (length str)))
    (cond ((>= x 24) " ")
	  ((>= x 16) "\t")
	  ((>= x 8) "\t\t")
	  (t "\t\t\t"))))

(defun about-show-linked-info (who)
  (let* ((entry (assq who xemacs-hackers))
	 (name (cadr entry))
	 (address (caddr entry)))
    (widget-create 'link :help-echo (concat "Find out more about " name)
		   :action 'about-maintainer
		   :button-prefix ""
		   :button-suffix ""
		   :tag name
		   :value who)
    (widget-insert (about-tabs name)
		   "<")
    (about-mailto-link address)
    (widget-insert ">\n")
    (about-hacker-contribution entry)
    (widget-insert "\n")))

(defun about-hackers (&rest ignore)
  (unless (about-get-buffer "*About Contributors*")
    (let ((title "A Legion of Contributors to XEmacs"))
      (widget-insert
       (about-center title)
       (about-with-face title 'bold)))
    (widget-insert
     "\n
Like most free software, XEmacs is a collaborative effort.  These are
some of the contributors.  We have no doubt forgotten someone; we
apologize!  You can see some of our faces under the links.\n\n"
     (about-with-face "Primary maintainers for this release:"
		      'about-headline-face)
     "\n\n")
    (mapc 'about-show-linked-info about-current-release-maintainers)
    (widget-insert
     "\n"
     (about-with-face "Other notable current hackers:"
		      'about-headline-face)
     "\n\n")
    (mapc 'about-show-linked-info about-other-current-hackers)
    (widget-insert
     "\n"
     (about-with-face "Other notable once and future hackers:"
		      'about-headline-face)
     "\n\n")
    (mapc 'about-show-linked-info about-once-and-future-hackers)
    (flet ((print-short (name addr &optional shortinfo)
	     (widget-insert (concat (about-with-face name 'italic)
				    (about-tabs name)
				    "<"))
	     (about-mailto-link addr)
	     (widget-insert
	      (concat ">\n"
		      (if shortinfo (concat shortinfo "\n") "")))))
      (widget-insert
       "\n\
In addition to those just mentioned, the following people have spent a
great deal of effort providing feedback, testing beta versions of
XEmacs, providing patches to the source code, or doing all of the
above.  We couldn't have done it without them.\n\n")
      (print-short "Nagi M. Aboulenein" "aboulene@ponder.csci.unt.edu")
      (print-short "Per Abrahamsen" "abraham@dina.kvl.dk")
      (print-short "Gary Adams" "gra@zeppo.East.Sun.COM")
      (print-short "Gennady Agranov" "agranov@csa.CS.Technion.Ac.IL")
      (print-short "Mark Allender" "allender@vnet.IBM.COM")
      (print-short "Stephen R. Anderson" "sra@bloch.ling.yale.edu")
      (print-short "Butch Anton" "butch@zaphod.uchicago.edu")
      (print-short "Fred Appelman" "Fred.Appelman@cv.ruu.nl")
      (print-short "Erik \"The Pope\" Arneson" "lazarus@mind.net")
      (print-short "Tor Arntsen" "tor@spacetec.no")
      (print-short "Marc Aurel" "4-tea-2@bong.saar.de")
      (print-short "Larry Auton" "lda@control.att.com")
      (print-short "Larry Ayers" "layers@marktwain.net")
      (print-short "Oswald P. Backus IV" "backus@altagroup.com")
      (print-short "Mike Battaglia" "mbattagl@dsccc.com")
      (print-short "Neal Becker" "neal@ctd.comsat.com")
      (print-short "Paul Bibilo" "peb@delcam.com")
      (print-short "Leonard Blanks" "ltb@haruspex.demon.co.uk")
      (print-short "Jan Borchers" "job@tk.uni-linz.ac.at")
      (print-short "Mark Borges" "mdb@cdc.noaa.gov")
      (print-short "David P. Boswell" "daveb@tau.space.thiokol.com")
      (print-short "Tim Bradshaw" "tfb@edinburgh.ac.uk")
      (print-short "Rick Braumoeller" "rickb@mti.sgi.com")
      (print-short "Matthew J. Brown" "mjb@doc.ic.ac.uk")
      (print-short "Alastair Burt" "burt@dfki.uni-kl.de")
      (print-short "David Bush" "david.bush@adn.alcatel.com")
      (print-short "Richard Caley" "rjc@cstr.edinburgh.ac.uk")
      (print-short "Stephen Carney" "carney@gvc.dec.com")
      (print-short "Lorenzo M. Catucci" "lorenzo@argon.roma2.infn.it")
      (print-short "Philippe Charton" "charton@lmd.ens.fr")
      (print-short "Peter Cheng" "peter.cheng@sun.com")
      (print-short "Jin S. Choi" "jin@atype.com")
      (print-short "Tomasz J. Cholewo" "tjchol01@mecca.spd.louisville.edu")
      (print-short "Serenella Ciongoli" "czs00@ladybug.oes.amdahl.com")
      (print-short "Glynn Clements" "glynn@sensei.co.uk")
      (print-short "Richard Cognot" "cognot@ensg.u-nancy.fr")
      (print-short "Andy Cohen" "cohen@andy.bu.edu")
      (print-short "Richard Coleman" "coleman@math.gatech.edu")
      (print-short "Mauro Condarelli" "MC5686@mclink.it")
      (print-short "Nick J. Crabtree" "nickc@scopic.com")
      (print-short "Christopher Davis" "ckd@kei.com")
      (print-short "Soren Dayton" "csdayton@cs.uchicago.edu")
      (print-short "Chris Dean" "ctdean@cogit.com")
      (print-short "Michael Diers" "mdiers@logware.de")
      (print-short "William G. Dubuque" "wgd@martigny.ai.mit.edu")
      (print-short "Steve Dunham" "dunham@dunham.tcimet.net")
      (print-short "Samuel J. Eaton" "samuele@cogs.susx.ac.uk")
      (print-short "Carl Edman" "cedman@Princeton.EDU")
      (print-short "Dave Edmondson" "davided@sco.com")
      (print-short "Jonathan Edwards" "edwards@intranet.com")
      (print-short "Eric Eide" "eeide@asylum.cs.utah.edu")
      (print-short "EKR" "ekr@terisa.com")
      (print-short "David Fletcher" "frodo@tsunami.com")
      (print-short "Paul Flinders" "ptf@delcam.co.uk")
      (print-short "Jered J Floyd" "jered@mit.edu")
      (print-short "Gary D. Foster" "Gary.Foster@Corp.Sun.COM")
      (print-short "Jerry Frain" "jerry@sneffels.tivoli.com")
      (print-short "Holger Franz" "hfranz@physik.rwth-aachen.de")
      (print-short "Benjamin Fried" "bf@morgan.com")
      (print-short "Barry Friedman" "friedman@nortel.ca")
      (print-short "Noah Friedman" "friedman@splode.com")
      (print-short "Kazuyoshi Furutaka" "furutaka@Flux.tokai.jaeri.go.jp")
      (print-short "Lew Gaiter III" "lew@StarFire.com")
      (print-short "Itay Gat" "itay@cs.huji.ac.il")
      (print-short "Tim Geisler" "Tim.Geisler@informatik.uni-muenchen.de")
      (print-short "Dave Gillespie" "daveg@synaptics.com")
      (print-short "Christian F. Goetze" "cg@bigbook.com")
      (print-short "Yusuf Goolamabbas" "yusufg@iss.nus.sg")
      (print-short "Wolfgang Grieskamp" "wg@cs.tu-berlin.de")
      (print-short "John Griffith" "griffith@sfs.nphil.uni-tuebingen.de")
      (print-short "James Grinter" "jrg@demon.net")
      (print-short "Ben Gross" "bgross@uiuc.edu")
      (print-short "Dirk Grunwald" "grunwald@foobar.cs.Colorado.EDU")
      (print-short "Michael Guenther" "michaelg@igor.stuttgart.netsurf.de")
      (print-short "Dipankar Gupta" "dg@hplb.hpl.hp.com")
      (print-short "Markus Gutschke" "gutschk@GOEDEL.UNI-MUENSTER.DE")
      (print-short "Kai Haberzettl" "khaberz@synnet.de")
      (print-short "Adam Hammer" "hammer@cs.purdue.edu")
      (print-short "Magnus Hammerin" "magnush@epact.se")
      (print-short "ChangGil Han" "cghan@phys401.phys.pusan.ac.kr")
      (print-short "Derek Harding" "dharding@lssec.bt.co.uk")
      (print-short "Michael Harnois" "mharnois@sbt.net")
      (print-short "John Haxby" "J.Haxby@isode.com")
      (print-short "Karl M. Hegbloom" "karlheg@inetarena.com")
      (print-short "Benedikt Heinen" "beh@icemark.thenet.ch")
      (print-short "Stephan Herrmann" "sh@first.gmd.de")
      (print-short "August Hill" "awhill@inlink.com")
      (print-short "Mike Hill" "mikehill@hgeng.com")
      (print-short "Charles Hines" "chuck_hines@VNET.IBM.COM")
      (print-short "Shane Holder" "holder@rsn.hp.com")
      (print-short "Chris Holt" "xris@migraine.stanford.edu")
      (print-short "Tetsuya HOYANO" "hoyano@ari.bekkoame.or.jp")
      (print-short "David Hughes" "djh@harston.cv.com")
      (print-short "Tudor Hulubei" "tudor@cs.unh.edu")
      (print-short "Tatsuya Ichikawa" "ichikawa@hv.epson.co.jp")
      (print-short "Andrew Innes" "andrewi@harlequin.co.uk")
      (print-short "Markku Jarvinen" "Markku.Jarvinen@simpukka.funet.fi")
      (print-short "Robin Jeffries" "robin.jeffries@sun.com")
      (print-short "Philip Johnson" "johnson@uhics.ics.Hawaii.Edu")
      (print-short "J. Kean Johnston" "jkj@paradigm-sa.com")
      (print-short "John W. Jones" "jj@asu.edu")
      (print-short "Andreas Kaempf" "andreas@sccon.com")
      (print-short "Yoshiaki Kasahara" "kasahara@nc.kyushu-u.ac.jp")
      (print-short "Amir Katz" "amir@ndsoft.com")
      (print-short "Doug Keller" "dkeller@vnet.ibm.com")
      (print-short "Hunter Kelly" "retnuh@corona")
      (print-short "Gregor Kennedy" "gregork@dadd.ti.com")
      (print-short "Michael Kifer" "kifer@cs.sunysb.edu")
      (print-short "Yasuhiko Kiuchi" "kiuchi@dsp.ksp.fujixerox.co.jp")
      (print-short "Greg Klanderman" "greg.klanderman@alum.mit.edu")
      (print-short "Valdis Kletnieks" "Valdis.Kletnieks@vt.edu")
      (print-short "Norbert Koch" "n.koch@delta-ii.de")
      (print-short "Rob Kooper" "kooper@cc.gatech.edu")
      (print-short "Peter Skov Knudsen" "knu@dde.dk")
      (print-short "Jens Krinke" "krinke@ips.cs.tu-bs.de")
      (print-short "Maximilien Lincourt" "max@toonboom.com")
      (print-short "Mats Larsson" "Mats.Larsson@uab.ericsson.se")
      (print-short "Simon Leinen" "simon@instrumatic.ch")
      (print-short "Carsten Leonhardt" "leo@arioch.oche.de")
      (print-short "James LewisMoss" "moss@cs.sc.edu")
      (print-short "Mats Lidell" "mats.lidell@contactor.se")
      (print-short "Matt Liggett" "mliggett@seven.ucs.indiana.edu")
      (print-short "Christian Limpach" "Christian.Limpach@nice.ch")
      (print-short "Maximilien Lincourt" "max@toonboom.com")
      (print-short "Markus Linnala" "maage@b14b.tupsu.ton.tut.fi")
      (print-short "Robert Lipe" "robertl@arnet.com")
      (print-short "Derrell Lipman" "derrell@vis-av.com")
      (print-short "Damon Lipparelli" "lipp@aa.net")
      (print-short "Hamish Macdonald" "hamish@bnr.ca")
      (print-short "Ian MacKinnon" "imackinnon@telia.co.uk")
      (print-short "Patrick MacRoberts" "macro@hpcobr30.cup.hp.com")
      (print-short "Tonny Madsen" "Tonny.Madsen@netman.dk")
      (print-short "Ketil Z Malde" "ketil@ii.uib.no")
      (print-short "Steve March" "smarch@quaver.urbana.mcd.mot.com")
      (print-short "Ricardo Marek" "ricky@ornet.co.il")
      (print-short "Pekka Marjola" "pema@iki.fi")
      (print-short "Simon Marshall" "simon@gnu.ai.mit.edu")
      (print-short "Dave Mason" "dmason@plg.uwaterloo.ca")
      (print-short "Jaye Mathisen" "mrcpu@cdsnet.net")
      (print-short "Jason McLaren" "mclaren@math.mcgill.ca")
      (print-short "Michael McNamara" "mac@silicon-sorcery.com")
      (print-short "Michael Meissner" "meissner@osf.org")
      (print-short "David M. Meyer" "meyer@ns.uoregon.edu")
      (print-short "John Mignault" "jbm@panix.com")
      (print-short "Brad Miller" "bmiller@cs.umn.edu")
      (print-short "John Morey" "jmorey@crl.com")
      (print-short "Rob Mori" "rob.mori@sun.com")
      (print-short "Heiko Muenkel" "muenkel@tnt.uni-hannover.de")
      (print-short "Arup Mukherjee" "arup+@cs.cmu.edu")
      (print-short "Colas Nahaboo" "Colas.Nahaboo@sophia.inria.fr")
      (print-short "Lynn D. Newton" "lynn@ives.phx.mcd.mot.com")
      (print-short "Casey Nielson" "knielson@joule.elee.calpoly.edu")
      (print-short "Georg Nikodym" "Georg.Nikodym@canada.sun.com")
      (print-short "Andy Norman" "ange@hplb.hpl.hp.com")
      (print-short "Joe Nuspl" "nuspl@sequent.com")
      (print-short "Kim Nyberg" "kny@tekla.fi")
      (print-short "Kevin Oberman" "oberman@es.net")
      (print-short "David Ofelt" "ofelt@getalife.Stanford.EDU")
      (print-short "Alexandre Oliva" "oliva@dcc.unicamp.br")
      (print-short "Tore Olsen" "toreo@colargol.idb.hist.no")
      (print-short "Greg Onufer" "Greg.Onufer@eng.sun.com")
      (print-short "Achim Oppelt" "aoppelt@theorie3.physik.uni-erlangen.de")
      (print-short "Rebecca Ore" "rebecca.ore@op.net")
      (print-short "Sudeep Kumar Palat" "palat@idt.unit.no")
      (print-short "Joel Peterson" "tarzan@aosi.com")
      (print-short "Thomas A. Peterson" "tap@src.honeywell.com")
      (print-short "Tibor Polgar" "tibor@alteon.com")
      (print-short "Frederic Poncin" "fp@info.ucl.ac.be")
      (print-short "E. Rehmi Post" "rehmi@asylum.sf.ca.us")
      (print-short "Martin Pottendorfer" "Martin.Pottendorfer@aut.alcatel.at")
      (print-short "Colin Rafferty" "colin@xemacs.org")
      (print-short "Rick Rankin" "Rick_Rankin-P15254@email.mot.com")
      (print-short "Paul M Reilly" "pmr@pajato.com")
      (print-short "Jack Repenning" "jackr@sgi.com")
      (print-short "Daniel Rich" "drich@cisco.com")
      (print-short "Roland Rieke" "rol@darmstadt.gmd.de")
      (print-short "Art Rijos" "art.rijos@SNET.com")
      (print-short "Russell Ritchie" "ritchier@britannia-life.co.uk")
      (print-short "Roland" "rol@darmstadt.gmd.de")
      (print-short "Mike Russell" "mjruss@rchland.vnet.ibm.com")
      (print-short "Hajime Saitou" "hajime@jsk.t.u-tokyo.ac.jp")
      (print-short "Jan Sandquist" "etxquist@iqa.ericsson.se")
      (print-short "Marty Sasaki" "sasaki@spdcc.com")
      (print-short "SATO Daisuke" "densuke@ga2.so-net.or.jp")
      (print-short "Kenji Sato" "ken@ny.kdd.com")
      (print-short "Mike Scheidler" "c23mts@eng.delcoelect.com")
      (print-short "Daniel Schepler" "daniel@shep13.wustl.edu")
      (print-short "Holger Schauer" "schauer@coling.uni-freiburg.de")
      (print-short "Darrel Schneider" "darrel@slc.com")
      (print-short "Hayden Schultz" "haydens@ll.mit.edu")
      (print-short "Cotton Seed" "cottons@cybercom.net")
      (print-short "Axel Seibert" "seiberta@informatik.tu-muenchen.de")
      (print-short "Odd-Magne Sekkingstad" "oddms@ii.uib.no")
      (print-short "Gregory Neil Shapiro" "gshapiro@sendmail.org")
      (print-short "Justin Sheehy" "justin@linus.mitre.org")
      (print-short "John Shen" "zfs60@cas.org")
      (print-short "Murata Shuuichirou" "mrt@mickey.ai.kyutech.ac.jp")
      (print-short "Matt Simmons" "simmonmt@acm.org")
      (print-short "Dinesh Somasekhar" "somasekh@ecn.purdue.edu")
      (print-short "Jeffrey Sparkes" "jsparkes@bnr.ca")
      (print-short "Manoj Srivastava" "srivasta@pilgrim.umass.edu")
      (print-short "Francois Staes" "frans@kiwi.uia.ac.be")
      (print-short "Anders Stenman" "stenman@isy.liu.se")
      (print-short "Jason Stewart" "jasons@cs.unm.edu")
      (print-short "Rick Tait" "rickt@gnu.ai.mit.edu")
      (print-short "TANAKA Hayashi" "tanakah@mxa.mesh.ne.jp")
      (print-short "Samuel Tardieu" "sam@inf.enst.fr")
      (print-short "James Thompson" "thompson@wg2.waii.com")
      (print-short "Nobu Toge" "toge@accad1.kek.jp")
      (print-short "Raymond L. Toy" "toy@rtp.ericsson.se")
      (print-short "Remek Trzaska" "remek@npac.syr.edu")
      (print-short "TSUTOMU Nakamura" "tsutomu@rs.kyoto.omronsoft.co.jp")
      (print-short "Stefanie Teufel" "s.teufel@ndh.net")
      (print-short "Gary Thomas" "g.thomas@opengroup.org")
      (print-short "John Turner" "turner@xdiv.lanl.gov")
      (print-short "UENO Fumihiro" "7m2vej@ritp.ye.IHI.CO.JP")
      (print-short "Aki Vehtari" "Aki.Vehtari@hut.fi")
      (print-short "Juan E. Villacis" "jvillaci@cs.indiana.edu")
      (print-short "Vladimir Vukicevic" "vladimir@intrepid.com")
      (print-short "David Walte" "djw18@cornell.edu")
      (print-short "Peter Ware" "ware@cis.ohio-state.edu")
      (print-short "Christoph Wedler" "wedler@fmi.uni-passau.de")
      (print-short "Yoav Weiss" "yoav@zeus.datasrv.co.il")
      (print-short "Peter B. West" "p.west@uq.net.au")
      (print-short "Rod Whitby" "rwhitby@asc.corp.mot.com")
      (print-short "Rich Williams" "rdw@hplb.hpl.hp.com")
      (print-short "Raymond Wiker" "raymond@orion.no")
      (print-short "Peter Windle" "peterw@SDL.UG.EDS.COM")
      (print-short "David C Worenklein" "dcw@gcm.com")
      (print-short "Takeshi Yamada" "yamada@sylvie.kecl.ntt.jp")
      (print-short "Katsumi Yamaoka" "yamaoka@ga.sony.co.jp")
      (print-short "Jason Yanowitz" "yanowitz@eternity.cs.umass.edu")
      (print-short "La Monte Yarroll" "piggy@hilbert.maths.utas.edu.au")
      (print-short "Blair Zajac" "blair@olympia.gps.caltech.edu")
      (print-short "Volker Zell" "vzell@de.oracle.com")
      (print-short "Daniel Zivkovic" "daniel@canada.sun.com")
      (print-short "Karel Zuiderveld" "Karel.Zuiderveld@cv.ruu.nl")
      (widget-insert "\n"))
    (about-finish-buffer)))

;;; about.el ends here
