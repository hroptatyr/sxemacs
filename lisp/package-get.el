;;; package-get.el --- Retrieve XEmacs package

;; Copyright (C) 1998 by Pete Ware
;; Copyright (C) 2002 Ben Wing.
;; Copyright (C) 2003, Steve Youngs

;; Author: Pete Ware <ware@cis.ohio-state.edu>
;; Heavy-Modifications: Greg Klanderman <greg@alphatech.com>
;;                      Jan Vroonhof    <vroonhof@math.ethz.ch>
;;                      Steve Youngs    <youngs@xemacs.org>
;; Keywords: internal

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; package-get -
;;	Retrieve a package and any other required packages from an archive
;;
;;
;; Note (JV): Most of this no longer applies!
;;
;; The idea:
;;	A new XEmacs lisp-only release is generated with the following steps:
;;	1. The maintainer runs some yet to be written program that
;;	   generates all the dependency information.  This should
;;	   determine all the require and provide statements and associate
;;	   them with a package.
;;	2. All the packages are then bundled into their own tar balls
;;	   (or whatever format)
;;	3. Maintainer automatically generates a new `package-get-base'
;;	   data structure which contains information such as the
;;	   package name, the file to be retrieved, an md5 checksum,
;;	   etc (see `package-get-base').
;;	4. The maintainer posts an announcement with the new version
;;	   of `package-get-base'.
;;	5. A user/system manager saves this posting and runs
;;	   `package-get-update' which uses the previously saved list
;;	   of packages, `package-get-here' that the user/site
;;	   wants to determine what new versions to download and
;;	   install.
;;
;;	A user/site manager can generate a new `package-get-here' structure
;;	by using `package-get-setup' which generates a customize like
;;	interface to the list of packages.  The buffer looks something
;;	like:
;;
;;	gnus	- a mail and news reader
;;	[]	Always install
;;	[]	Needs updating
;;	[]	Required by other [packages]
;;	version: 2.0
;;
;;	vm	- a mail reader
;;	[]	Always install
;;	[]	Needs updating
;;	[]	Required by other [packages]
;;
;;	Where `[]' indicates a toggle box
;;
;;	- Clicking on "Always install" puts this into
;;	  `package-get-here' list.  "Needs updating" indicates a new
;;	  version is available.  Anything already in
;;	  `package-get-here' has this enabled.
;;	- "Required by other" means some other packages are going to force
;;	  this to be installed.  Clicking on  [packages] gives a list
;;	  of packages that require this.
;;
;;	The `package-get-base' should be installed in a file in
;;	`data-directory'.  The `package-get-here' should be installed in
;;	site-lisp.  Both are then read at run time.
;;
;; TODO:
;;	- Implement `package-get-setup'
;;	- Actually put `package-get-base' and `package-get-here' into
;;	  files that are read.
;;	- Allow users to have their own packages that they want installed
;;	  in ~/.xemacs/.
;;	- SOMEONE needs to write the programs that generate the
;;	  provides/requires database and makes it into a lisp data
;;	  structure suitable for `package-get-base'
;;	- Handle errors such as no package providing a required symbol.
;;	- Tie this into the `require' function to download packages
;;	  transparently.

;;; Change Log

;;; Code:

(require 'package-admin)
;; (require 'package-get-base)

(defgroup package-tools nil
  "Tools to manipulate packages."
  :group 'emacs)

(defgroup package-get nil
  "Automatic Package Fetcher and Installer."
  :prefix "package-get"
  :group 'package-tools)

;;;###autoload
(defvar package-get-base nil
  "List of packages that are installed at this site.
For each element in the alist,  car is the package name and the cdr is
a plist containing information about the package.   Typical fields
kept in the plist are:

version		- version of this package
provides	- list of symbols provided
requires	- list of symbols that are required.
		  These in turn are provided by other packages.
filename	- name of the file.
size		- size of the file (aka the bundled package)
md5sum		- computed md5 checksum
description	- What this package is for.
type		- Whether this is a 'binary (default) or 'single file package

More fields may be added as needed.  An example:

'(
 (name
  (version \"<version 2>\"
   file \"filename\"
   description \"what this package is about.\"
   provides (<list>)
   requires (<list>)
   size <integer-bytes>
   md5sum \"<checksum\"
   type single
   )
  (version \"<version 1>\"
   file \"filename\"
   description \"what this package is about.\"
   provides (<list>)
   requires (<list>)
   size <integer-bytes>
   md5sum \"<checksum\"
   type single
   )
   ...
   ))

For version information, it is assumed things are listed in most
recent to least recent -- in other words, the version names don't have to
be lexically ordered.  It is debatable if it makes sense to have more than
one version of a package available.")

(defcustom package-get-dir (temp-directory)
  "*Where to store temporary files for staging."
  :tag "Temporary directory"
  :type 'directory
  :group 'package-get)

;;;###autoload
(defcustom package-get-package-index-file-location 
  (car (split-path (or (getenv "EMACSPACKAGEPATH") user-init-directory)))
  "*The directory where the package-index file can be found."
  :type 'directory
  :group 'package-get)

;;;###autoload
(defcustom package-get-install-to-user-init-directory nil
  "*If non-nil install packages under `user-init-directory'."
  :type 'boolean
  :group 'package-get)

(define-widget 'host-name 'string
  "A Host name."
  :tag "Host")

(defcustom package-get-remote nil
  "*The remote site to contact for downloading packages.
Format is '(site-name directory-on-site).  As a special case, `site-name'
can be `nil', in which case `directory-on-site' is treated as a local
directory."
  :tag "Package repository"
  :type '(set (choice (const :tag "None" nil)
		      (list :tag "Local" (const :tag "Local" nil) directory)
		      (list :tag "Remote" host-name directory)))
  :group 'package-get)

;;;###autoload
(defcustom package-get-download-sites
  '(
    ;; Main XEmacs Site (ftp.xemacs.org)
    ("US (Main XEmacs Site)"
     "ftp.xemacs.org" "pub/xemacs/packages")
    ;; In alphabetical order of Country, our mirrors...
    ("Australia (aarnet.edu.au)" "mirror.aarnet.edu.au" "pub/xemacs/packages")
    ("Australia (au.xemacs.org)" "ftp.au.xemacs.org" "pub/xemacs/packages")
    ("Austria (at.xemacs.org)" "ftp.at.xemacs.org" "editors/xemacs/packages")
    ("Belgium (be.xemacs.org)" "ftp.be.xemacs.org" "xemacs/packages")
    ("Brazil (br.xemacs.org)" "ftp.br.xemacs.org" "pub/xemacs/packages")
    ("Canada (ca.xemacs.org)" "ftp.ca.xemacs.org" "pub/Mirror/xemacs/packages")
    ("Canada (crc.ca)" "ftp.crc.ca" "pub/packages/editors/xemacs/packages")
    ("Canada (ualberta.ca)" "sunsite.ualberta.ca" "pub/Mirror/xemacs/packages")
    ("Czech Republic (cz.xemacs.org)" "ftp.cz.xemacs.org" "MIRRORS/ftp.xemacs.org/pub/xemacs/packages")
    ("Denmark (dk.xemacs.org)" "ftp.dk.xemacs.org" "pub/emacs/xemacs/packages")
    ("Finland (fi.xemacs.org)" "ftp.fi.xemacs.org" "pub/mirrors/ftp.xemacs.org/pub/tux/xemacs/packages")
    ("France (fr.xemacs.org)" "ftp.fr.xemacs.org" "pub/xemacs/packages")
    ("France (mirror.cict.fr)" "mirror.cict.fr" "xemacs/packages")
    ("France (pasteur.fr)" "ftp.pasteur.fr" "pub/computing/xemacs/packages")
    ("Germany (de.xemacs.org)" "ftp.de.xemacs.org" "pub/ftp.xemacs.org/tux/xemacs/packages")
    ("Germany (tu-darmstadt.de)" "ftp.tu-darmstadt.de" "pub/editors/xemacs/packages")
    ("Ireland (ie.xemacs.org)" "ftp.ie.xemacs.org" "mirrors/ftp.xemacs.org/pub/xemacs/packages")
    ("Italy (it.xemacs.org)" "ftp.it.xemacs.org" "unix/packages/XEMACS/packages")
    ("Japan (aist.go.jp)" "ring.aist.go.jp" "pub/text/xemacs/packages")
    ("Japan (asahi-net.or.jp)" "ring.asahi-net.or.jp" "pub/text/xemacs/packages")
    ("Japan (dti.ad.jp)" "ftp.dti.ad.jp" "pub/unix/editor/xemacs/packages")
    ("Japan (jaist.ac.jp)" "ftp.jaist.ac.jp" "pub/GNU/xemacs/packages")
    ("Japan (jp.xemacs.org)" "ftp.jp.xemacs.org" "pub/GNU/xemacs/packages")
    ("Japan (nucba.ac.jp)" "mirror.nucba.ac.jp" "mirror/xemacs/packages")
    ("Japan (sut.ac.jp)" "sunsite.sut.ac.jp" "pub/archives/packages/xemacs/packages")
    ("Korea (kr.xemacs.org)" "ftp.kr.xemacs.org" "pub/tools/emacs/xemacs/packages")
    ("New Zealand (nz.xemacs.org)" "ftp.nz.xemacs.org" "mirror/ftp.xemacs.org/packages")
    ("Norway (no.xemacs.org)" "ftp.no.xemacs.org" "pub/xemacs/packages")
    ("Poland (pl.xemacs.org)" "ftp.pl.xemacs.org" "pub/unix/editors/xemacs/packages")
    ("Russia (ru.xemacs.org)" "ftp.ru.xemacs.org" "pub/xemacs/packages")
    ("Slovakia (sk.xemacs.org)" "ftp.sk.xemacs.org" "pub/mirrors/xemacs/packages")
    ("South Africa (za.xemacs.org)" "ftp.za.xemacs.org" "mirrorsites/ftp.xemacs.org/packages")
    ("Sweden (se.xemacs.org)" "ftp.se.xemacs.org" "pub/gnu/xemacs/packages")
    ("Switzerland (ch.xemacs.org)" "ftp.ch.xemacs.org" "mirror/xemacs/packages")
    ("UK (uk.xemacs.org)" "ftp.uk.xemacs.org" "sites/ftp.xemacs.org/pub/xemacs/packages")
    ("US (ibiblio.org)" "ibiblio.org" "pub/packages/editors/xemacs/packages")
    ("US (stealth.net)" "ftp.stealth.net" "pub/mirrors/ftp.xemacs.org/pub/xemacs/packages")
    ("US (unc.edu)" "metalab.unc.edu" "pub/packages/editors/xemacs/packages")
    ("US (us.xemacs.org)" "ftp.us.xemacs.org" "pub/xemacs/packages")
    ("US (utk.edu)" "ftp.sunsite.utk.edu" "pub/xemacs/packages")
    )
  "*List of remote sites available for downloading packages.
List format is '(site-description site-name directory-on-site).
SITE-DESCRIPTION is a textual description of the site.  SITE-NAME
is the internet address of the download site.  DIRECTORY-ON-SITE
is the directory on the site in which packages may be found.
This variable is used to initialize `package-get-remote', the
variable actually used to specify package download sites."
  :tag "Package download sites"
  :type '(repeat (list (string :tag "Name") host-name directory))
  :group 'package-get)

;;;###autoload
(defcustom package-get-pre-release-download-sites
  '(
    ;; Main XEmacs Site (ftp.xemacs.org)
    ("Pre-Releases (Main XEmacs Site)" "ftp.xemacs.org"
     "pub/xemacs/beta/experimental/packages")
    ;; In alphabetical order of Country, our mirrors...
    ("Australia Pre-Releases (aarnet.edu.au)" "mirror.aarnet.edu.au"
     "pub/xemacs/beta/experimental/packages")
    ("Australia Pre-Releases (au.xemacs.org)" "ftp.au.xemacs.org"
     "pub/xemacs/beta/experimental/packages")
    ("Austria Pre-Releases (at.xemacs.org)" "ftp.at.xemacs.org"
     "editors/xemacs/beta/experimentsl/packages")
    ("Brazil Pre-Releases (br.xemacs.org)" "ftp.br.xemacs.org"
     "pub/xemacs/xemacs-21.5/experimental/packages")
    ("Canada Pre-Releases (ca.xemacs.org)" "ftp.ca.xemacs.org"
     "pub/Mirror/xemacs/beta/experimental/packages")
    ("Canada Pre-Releases (crc.ca)" "ftp.crc.ca"
     "pub/packages/editors/xemacs/beta/experimental/packages")
    ("Canada Pre-Releases (ualberta.ca)" "sunsite.ualberta.ca"
     "pub/Mirror/xemacs/beta/experimental/packages")
    ("Czech Republic Pre-Releases (cz.xemacs.org)" "ftp.cz.xemacs.org"
     "MIRRORS/ftp.xemacs.org/pub/xemacs/xemacs-21.5/experimental/packages")
    ("Denmark Pre-Releases (dk.xemacs.org)" "ftp.dk.xemacs.org"
     "pub/emacs/xemacs/beta/experimental/packages")
    ("Finland Pre-Releases (fi.xemacs.org)" "ftp.fi.xemacs.org"
     "pub/mirrors/ftp.xemacs.org/pub/tux/xemacs/beta/experimental/packages")
    ("France Pre-Releases (fr.xemacs.org)" "ftp.fr.xemacs.org"
     "pub/xemacs/beta/experimental/packages")
    ("France Pre-Releases (mirror.cict.fr)" "mirror.cict.fr"
     "xemacs/beta/experimental/packages")
    ("France Pre-Releases (pasteur.fr)" "ftp.pasteur.fr"
     "pub/computing/xemacs/beta/experimental/packages")
    ("Germany Pre-Releases (de.xemacs.org)" "ftp.de.xemacs.org"
     "pub/ftp.xemacs.org/tux/xemacs/beta/experimental/packages")
    ("Germany Pre-Releases (tu-darmstadt.de)" "ftp.tu-darmstadt.de"
     "pub/editors/xemacs/beta/experimental/packages")
    ("Ireland Pre-Releases (ie.xemacs.org)" "ftp.ie.xemacs.org"
     "mirrors/ftp.xemacs.org/pub/xemacs/beta/experimental/packages")
    ("Italy Pre-Releases (it.xemacs.org)" "ftp.it.xemacs.org"
     "unix/packages/XEMACS/beta/experimental/packages")
    ("Japan Pre-Releases (aist.go.jp)" "ring.aist.go.jp"
     "pub/text/xemacs/beta/experimental/packages")
    ("Japan Pre-Releases (asahi-net.or.jp)" "ring.asahi-net.or.jp"
     "pub/text/xemacs/beta/experimental/packages")
    ("Japan Pre-Releases (dti.ad.jp)" "ftp.dti.ad.jp"
     "pub/unix/editor/xemacs/beta/experimental/packages")
    ("Japan Pre-Releases (jaist.ac.jp)" "ftp.jaist.ac.jp"
     "pub/GNU/xemacs/beta/experimental/packages")
    ("Japan Pre-Releases (jp.xemacs.org)" "ftp.jp.xemacs.org"
     "pub/GNU/xemacs/beta/experimental/packages")
    ("Japan Pre-Releases (sut.ac.jp)" "sunsite.sut.ac.jp"
     "pub/archives/packages/xemacs/xemacs-21.5/experimental/packages")
    ("New Zealand Pre-Releases (nz.xemacs.org)" "ftp.nz.xemacs.org" "mirror/ftp.xemacs.org/packages")
    ("Norway Pre-Releases (no.xemacs.org)" "ftp.no.xemacs.org"
     "pub/xemacs/beta/experimental/packages")
    ("Poland Pre-Releases (pl.xemacs.org)" "ftp.pl.xemacs.org"
     "pub/unix/editors/xemacs/beta/experimental/packages")
    ("Russia Pre-Releases (ru.xemacs.org)" "ftp.ru.xemacs.org"
     "pub/xemacs/beta/experimental/packages")
    ("Saudi Arabia Pre-Releases (sa.xemacs.org)" "ftp.sa.xemacs.org"
     "pub/mirrors/ftp.xemacs.org/xemacs/xemacs-21.5/experimental/packages")
    ("Slovakia Pre-Releases (sk.xemacs.org)" "ftp.sk.xemacs.org"
     "pub/mirrors/xemacs/beta/experimental/packages")
    ("South Africa Pre-Releases (za.xemacs.org)" "ftp.za.xemacs.org"
     "mirrorsites/ftp.xemacs.org/beta/experimental/packages")
    ("Sweden Pre-Releases (se.xemacs.org)" "ftp.se.xemacs.org"
     "pub/gnu/xemacs/beta/experimental/packages")
    ("Switzerland Pre-Releases (ch.xemacs.org)" "ftp.ch.xemacs.org"
     "mirror/xemacs/beta/experimental/packages")
    ("UK Pre-Releases (uk.xemacs.org)" "ftp.uk.xemacs.org"
     "sites/ftp.xemacs.org/pub/xemacs/beta/experimental/packages")
    ("US Pre-Releases (ibiblio.org)" "ibiblio.org"
     "pub/packages/editors/xemacs/beta/experimental/packages")
    ("US Pre-Releases (stealth.net)" "ftp.stealth.net"
     "pub/mirrors/ftp.xemacs.org/pub/xemacs/beta/experimental/packages")
    ("US Pre-Releases (unc.edu)" "metalab.unc.edu"
     "pub/packages/editors/xemacs/beta/experimental/packages")
    ("US Pre-Releases (us.xemacs.org)" "ftp.us.xemacs.org"
     "pub/xemacs/beta/experimental/packages")
    ("US Pre-Releases (utk.edu)" "ftp.sunsite.utk.edu"
     "pub/xemacs/beta/experimental/packages"))
  "*List of remote sites available for downloading \"Pre-Release\" packages.
List format is '(site-description site-name directory-on-site).
SITE-DESCRIPTION is a textual description of the site.  SITE-NAME
is the internet address of the download site.  DIRECTORY-ON-SITE
is the directory on the site in which packages may be found.
This variable is used to initialize `package-get-remote', the
variable actually used to specify package download sites."
  :tag "Pre-Release Package download sites"
  :type '(repeat (list (string :tag "Name") host-name directory))
  :group 'package-get)

;;;###autoload
(defcustom package-get-site-release-download-sites
  nil
  "*List of remote sites available for downloading \"Site Release\" packages.
List format is '(site-description site-name directory-on-site).
SITE-DESCRIPTION is a textual description of the site.  SITE-NAME
is the internet address of the download site.  DIRECTORY-ON-SITE
is the directory on the site in which packages may be found.
This variable is used to initialize `package-get-remote', the
variable actually used to specify package download sites."
  :tag "Site Release Package download sites"
  :type '(repeat (list (string :tag "Name") host-name directory))
  :group 'package-get)

(defcustom package-get-remove-copy t
  "*After copying and installing a package, if this is t, then remove the
copy.  Otherwise, keep it around."
  :type 'boolean
  :group 'package-get)

;; #### it may make sense for this to be a list of names.
;; #### also, should we rename "*base*" to "*index*" or "*db*"?
;;      "base" is a pretty poor name.
(defcustom package-get-base-filename "package-index.LATEST.gpg"
  "*Name of the default package-get database file.
This may either be a relative path, in which case it is interpreted
with respect to `package-get-remote', or an absolute path."
  :type 'file
  :group 'package-get)

(defcustom package-get-always-update nil
  "*If Non-nil always make sure we are using the latest package index (base).
Otherwise respect the `force-current' argument of `package-get-require-base'."
  :type 'boolean
  :group 'package-get)

(defun package-get-pgp-available-p ()
  "Checks the availability of Mailcrypt and PGP executable.

Returns t if both are found, nil otherwise.  As a side effect, set
`mc-default-scheme' dependent on the PGP executable found."
  (let (result)
    (when (featurep 'mailcrypt-autoloads)
      (autoload 'mc-setversion "mc-setversion"))
    (when (fboundp 'mc-setversion)
      (cond ((locate-file "gpg" exec-path
			  '("" ".btm" ".bat" ".cmd" ".exe" ".com")
			  'executable)
	     (mc-setversion "gpg")
	     (setq result t))
	    ((locate-file "pgpe" exec-path
			  '("" ".btm" ".bat" ".cmd" ".exe" ".com")
			  'executable)
	     (mc-setversion "5.0")
	     (setq result t))
	    ((locate-file "pgp" exec-path
			  '("" ".btm" ".bat" ".cmd" ".exe" ".com")
			  'executable)
	     (mc-setversion "2.6")
	     (setq result t))))
    (if result
	result
      nil)))

(defcustom package-get-require-signed-base-updates nil
  "*If non-nil, try to verify the package index database via PGP.

If nil, no PGP verification is done.  If the package index database
entries are not PGP signed and this variable is non-nil, require user
confirmation to continue with the package-get procedure."
  :type 'boolean
  :group 'package-get)

(defvar package-entries-are-signed nil
  "Non-nil when the package index file has been PGP signed.")

(defvar package-get-continue-update-base nil
  "Non-nil update the index even if it hasn't been signed.")

(defvar package-get-was-current nil
  "Non-nil we did our best to fetch a current database.")

;;;###autoload
(defun package-get-require-base (&optional force-current)
  "Require that a package-get database has been loaded.
If the optional FORCE-CURRENT argument or the value of
`package-get-always-update' is Non-nil, try to update the database
from a location in `package-get-remote'. Otherwise a local copy is used
if available and remote access is never done.

Please use FORCE-CURRENT only when the user is explictly dealing with packages
and remote access is likely in the near future."
  (setq force-current (or force-current package-get-always-update))
  (unless (and (boundp 'package-get-base)
	       package-get-base
	       (or (not force-current) package-get-was-current))
    (package-get-update-base nil force-current))
  (if (or (not (boundp 'package-get-base))
	  (not package-get-base))
      (error 'void-variable
	     "Package-get database not loaded")
    (setq package-get-was-current force-current)))

(defconst package-get-pgp-signed-begin-line "^-----BEGIN PGP SIGNED MESSAGE-----"
  "Text for start of PGP signed messages.")
(defconst package-get-pgp-signature-begin-line "^-----BEGIN PGP SIGNATURE-----"
  "Text for beginning of PGP signature.")
(defconst package-get-pgp-signature-end-line "^-----END PGP SIGNATURE-----"
  "Text for end of PGP signature.")

;;;###autoload
(defun package-get-update-base-entry (entry)
  "Update an entry in `package-get-base'."
  (let ((existing (assq (car entry) package-get-base)))
    (if existing
        (setcdr existing (cdr entry))
      (setq package-get-base (cons entry package-get-base)))))

(defun package-get-locate-file (file &optional nil-if-not-found no-remote)
  "Locate an existing FILE with respect to `package-get-remote'.
If FILE is an absolute path or is not found, simply return FILE.
If optional argument NIL-IF-NOT-FOUND is non-nil, return nil
if FILE can not be located.
If NO-REMOTE is non-nil never search remote locations."
  (if (file-name-absolute-p file)
      file
    (let ((site package-get-remote)
          (expanded nil))
      (when site
	(unless (and no-remote (caar (list site)))
	  (let ((expn (package-get-remote-filename (car (list site)) file)))
	    (if (and expn (file-exists-p expn))
		(setq site nil
		      expanded expn)))))
      (or expanded
          (and (not nil-if-not-found)
               file)))))

(defun package-get-locate-index-file (no-remote)
  "Locate the package-get index file.  

Do not return remote paths if NO-REMOTE is non-nil.  If the index
file doesn't exist in `package-get-package-index-file-location', ask
the user if one should be created using the index file in core as a
template."
  (or (package-get-locate-file package-get-base-filename t no-remote)
      (if (file-exists-p (expand-file-name package-get-base-filename
					   package-get-package-index-file-location))
	  (expand-file-name package-get-base-filename
			    package-get-package-index-file-location)
	(if (y-or-n-p (format "No index file, shall I create one in %s? "
			      package-get-package-index-file-location))
	    (progn
	      (save-excursion
		(set-buffer 
		 (find-file-noselect (expand-file-name
				      package-get-base-filename
				      package-get-package-index-file-location)))
		(let ((coding-system-for-write 'binary))
		  (erase-buffer)
		  (insert-file-contents-literally
		   (locate-data-file package-get-base-filename))
		  (save-buffer (current-buffer))
		  (kill-buffer (current-buffer))))
	      (expand-file-name package-get-base-filename
				package-get-package-index-file-location))
	  (error 'search-failed
		 "Can't locate a package index file.")))))

(defun package-get-maybe-save-index (filename)
  "Offer to save the current buffer as the local package index file,
if different."
  (let ((location (package-get-locate-index-file t)))
    (unless (and filename (equal filename location))
      (unless (and location
		   (equal (md5 (current-buffer))
			  (with-temp-buffer
			    (insert-file-contents-literally location)
			    (md5 (current-buffer)))))
	(when (not (file-writable-p location))
	  (if (y-or-n-p (format "Sorry, %s is read-only, can I use %s? "
				location user-init-directory))
	      (setq location (expand-file-name
			      package-get-base-filename
			      package-get-package-index-file-location))
	    (error 'file-error
		   (format "%s is read-only" location))))
	(when (y-or-n-p (concat "Update package index in " location "? "))
	  (let ((coding-system-for-write 'binary))
	    (write-file location)))))))

;;;###autoload
(defun package-get-update-base (&optional db-file force-current)
  "Update the package-get database file with entries from DB-FILE.
Unless FORCE-CURRENT is non-nil never try to update the database."
  (interactive
   (let ((dflt (package-get-locate-index-file nil)))
     (list (read-file-name "Load package-get database: "
                           (file-name-directory dflt)
                           dflt
                           t
                           (file-name-nondirectory dflt)))))
  (setq db-file (expand-file-name (or db-file
                                      (package-get-locate-index-file
				         (not force-current)))))
  (if (not (file-exists-p db-file))
      (error 'file-error
	     (format "Package-get database file `%s' does not exist" db-file)))
  (if (not (file-readable-p db-file))
      (error 'file-error
	     (format "Package-get database file `%s' not readable" db-file)))
  (let ((buf (get-buffer-create "*package database*")))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (erase-buffer buf)
          (insert-file-contents-literally db-file)
          (package-get-update-base-from-buffer buf)
	  (if (file-remote-p db-file)
	      (package-get-maybe-save-index db-file)))
      (kill-buffer buf))))

;; This is here because the `process-error' datum doesn't exist in
;; 21.4. --SY.
(define-error 'process-error "Process error")

;;;###autoload
(defun package-get-update-base-from-buffer (&optional buf)
  "Update the package-get database with entries from BUFFER.
BUFFER defaults to the current buffer.  This command can be
used interactively, for example from a mail or news buffer."
  (interactive)
  (setq buf (or buf (current-buffer)))
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	content-beg content-end)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (setq content-beg (point))
      (setq content-end (save-excursion (goto-char (point-max)) (point)))
      (when (re-search-forward package-get-pgp-signed-begin-line nil t)
        (setq content-beg (match-end 0)))
      (when (re-search-forward package-get-pgp-signature-begin-line nil t)
        (setq content-end (match-beginning 0))
	(setq package-entries-are-signed t))
      (re-search-forward package-get-pgp-signature-end-line nil t)
      (setq package-get-continue-update-base t)
      ;; This is a little overkill because the default value of
      ;; `package-get-require-signed-base-updates' is the return of
      ;; `package-get-pgp-available-p', but we have to allow for
      ;; someone explicitly setting
      ;; `package-get-require-signed-base-updates' to t. --SY
      (when (and package-get-require-signed-base-updates
		 (package-get-pgp-available-p))
	(if package-entries-are-signed
	    (let (good-sig)
	      (setq package-get-continue-update-base nil)
	      (autoload 'mc-verify "mc-toplev")
	      (when (mc-verify)
		(setq good-sig t))
	      (if good-sig
		  (setq package-get-continue-update-base t)
		(error 'process-error 
		       "GnuPG error.  Package database not updated")))
	  (if (yes-or-no-p
	       "Package Index is not PGP signed.  Continue anyway? ")
	      (setq package-get-continue-update-base t)
	    (setq package-get-continue-update-base nil)
	    (warn "Package database not updated"))))
      ;; ToDo: We should call package-get-maybe-save-index on the region
      (when package-get-continue-update-base
	(package-get-update-base-entries content-beg content-end)
	(message "Updated package database")))))

(defun package-get-update-base-entries (start end)
  "Update the package-get database with the entries found between
START and END in the current buffer."
  (save-excursion
    (goto-char start)
    (if (not (re-search-forward "^(package-get-update-base-entry" nil t))
        (error 'search-failed
	       "Buffer does not contain package-get database entries"))
    (beginning-of-line)
    (let ((count 0))
      (while (and (< (point) end)
                  (re-search-forward "^(package-get-update-base-entry" nil t))
        (beginning-of-line)
        (let ((entry (read (current-buffer))))
          (if (or (not (consp entry))
                  (not (eq (car entry) 'package-get-update-base-entry)))
              (error 'syntax-error
		     "Invalid package-get database entry found"))
          (package-get-update-base-entry
           (car (cdr (car (cdr entry)))))
          (setq count (1+ count))))
      (message "Got %d package-get database entries" count))))

;;;###autoload
(defun package-get-save-base (file)
  "Write the package-get database to FILE.

Note: This database will be unsigned of course."
  (interactive "FSave package-get database to: ")
  (package-get-require-base t)
  (let ((buf (get-buffer-create "*package database*")))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (erase-buffer buf)
          (goto-char (point-min))
          (let ((entries package-get-base) entry plist)
            (insert ";; Package Index file -- Do not edit manually.\n")
            (insert ";;;@@@\n")
            (while entries
              (setq entry (car entries))
              (setq plist (car (cdr entry)))
              (insert "(package-get-update-base-entry (quote\n")
              (insert (format "(%s\n" (symbol-name (car entry))))
              (while plist
                (insert (format "  %s%s %S\n"
                                (if (eq plist (car (cdr entry))) "(" " ")
                                (symbol-name (car plist))
                                (car (cdr plist))))
                (setq plist (cdr (cdr plist))))
              (insert "))\n))\n;;;@@@\n")
              (setq entries (cdr entries))))
          (insert ";; Package Index file ends here\n")
          (write-region (point-min) (point-max) file))
      (kill-buffer buf))))

(defun package-get-interactive-package-query (get-version package-symbol)
  "Perform interactive querying for package and optional version.
Query for a version if GET-VERSION is non-nil.  Return package name as
a symbol instead of a string if PACKAGE-SYMBOL is non-nil.
The return value is suitable for direct passing to `interactive'."
  (package-get-require-base t)
  (let ((table (mapcar #'(lambda (item)
			   (let ((name (symbol-name (car item))))
			     (cons name name)))
		       package-get-base))
	package package-symbol default-version version)
    (save-window-excursion
      (setq package (completing-read "Package: " table nil t))
      (setq package-symbol (intern package))
      (if get-version
	  (progn
	    (setq default-version
		  (package-get-info-prop
		   (package-get-info-version
		    (package-get-info-find-package package-get-base
						   package-symbol) nil)
		   'version))
	    (while (string=
		    (setq version (read-string "Version: " default-version))
		    ""))
	    (if package-symbol
		(list package-symbol version)
	      (list package version)))
	(if package-symbol
	    (list package-symbol)
	  (list package))))))

;;;###autoload
(defun package-get-delete-package (package &optional pkg-topdir)
  "Delete an installation of PACKAGE below directory PKG-TOPDIR.
PACKAGE is a symbol, not a string.
This is just an interactive wrapper for `package-admin-delete-binary-package'."
  (interactive (package-get-interactive-package-query nil t))
  (package-admin-delete-binary-package package pkg-topdir))

;;;###autoload
(defun package-get-update-all ()
  "Fetch and install the latest versions of all currently installed packages."
  (interactive)
  (package-get-require-base t)
  ;; Load a fresh copy
  (catch 'exit
    (mapcar (lambda (pkg)
	      (if (not (package-get (car pkg) nil 'never))
		  (throw 'exit nil)))		;; Bail out if error detected
	    packages-package-list))
  (package-net-update-installed-db))

;;;###autoload
(defun package-get-all (package version &optional fetched-packages install-dir)
  "Fetch PACKAGE with VERSION and all other required packages.
Uses `package-get-base' to determine just what is required and what
package provides that functionality.  If VERSION is nil, retrieves
latest version.  Optional argument FETCHED-PACKAGES is used to keep
track of packages already fetched.  Optional argument INSTALL-DIR,
if non-nil, specifies the package directory where fetched packages
should be installed.

Returns nil upon error."
  (interactive (package-get-interactive-package-query t nil))
  (let* ((the-package (package-get-info-find-package package-get-base
						     package))
	 (this-package (package-get-info-version
			the-package version))
	 (this-requires (package-get-info-prop this-package 'requires)))
    (catch 'exit
      (setq version (package-get-info-prop this-package 'version))
      (unless (package-get-installedp package version)
	(if (not (package-get package version nil install-dir))
	    (progn
	      (setq fetched-packages nil)
	      (throw 'exit nil))))
      (setq fetched-packages
	    (append (list package)
		    (package-get-info-prop this-package 'provides)
		    fetched-packages))
      ;; grab everything that this package requires plus recursively
      ;; grab everything that the requires require.  Keep track
      ;; in `fetched-packages' the list of things provided -- this
      ;; keeps us from going into a loop
      (while this-requires
	(if (not (member (car this-requires) fetched-packages))
	    (let* ((reqd-package (package-get-package-provider
				  (car this-requires) t))
		   (reqd-version (cadr reqd-package))
		   (reqd-name (car reqd-package)))
	      (if (null reqd-name)
		  (error 'search-failed
			 (format "Unable to find a provider for %s"
				 (car this-requires))))
	      (if (not (setq fetched-packages
			     (package-get-all reqd-name reqd-version
					      fetched-packages
                                              install-dir)))
		  (throw 'exit nil))))
	(setq this-requires (cdr this-requires))))
    fetched-packages))

;;;###autoload
(defun package-get-dependencies (packages)
  "Compute dependencies for PACKAGES.
Uses `package-get-base' to determine just what is required and what
package provides that functionality.  Returns the list of packages
required by PACKAGES."
  (package-get-require-base t)
  (let ((orig-packages packages)
        dependencies provided)
    (while packages
      (let* ((package (car packages))
             (the-package (package-get-info-find-package
                           package-get-base package))
             (this-package (package-get-info-version
                            the-package nil))
             (this-requires (package-get-info-prop this-package 'requires))
             (new-depends   (set-difference
                             (mapcar
                              #'(lambda (reqd)
                                  (let* ((reqd-package (package-get-package-provider reqd))
                                         (reqd-name    (car reqd-package)))
                                    (if (null reqd-name)
                                        (error 'search-failed
					       (format "Unable to find a provider for %s" reqd)))
                                    reqd-name))
                              this-requires)
                             dependencies))
             (this-provides (package-get-info-prop this-package 'provides)))
        (setq dependencies
              (union dependencies new-depends))
        (setq provided
              (union provided (union (list package) this-provides)))
        (setq packages
              (union new-depends (cdr packages)))))
    (set-difference dependencies orig-packages)))

(defun package-get-load-package-file (lispdir file)
  (let (pathname)
    (setq pathname (expand-file-name file lispdir))
    (condition-case err
	(progn
	  (load pathname t)
	  t)
      (t
       (message "Error loading package file \"%s\" %s!" pathname err)
       nil))
    ))

(defun package-get-init-package (lispdir)
  "Initialize the package.
This really assumes that the package has never been loaded.  Updating
a newer package can cause problems, due to old, obsolete functions in
the old package.

Return `t' upon complete success, `nil' if any errors occurred."
  (progn
    (if (and lispdir
	     (file-accessible-directory-p lispdir))
	(progn
	  ;; Add lispdir to load-path if it doesn't already exist.
	  ;; NOTE: this does not take symlinks, etc., into account.
	  (if (let ((dirs load-path))
		(catch 'done
		  (while dirs
		    (if (string-equal (car dirs) lispdir)
			(throw 'done nil))
		    (setq dirs (cdr dirs)))
		  t))
	      (setq load-path (cons lispdir load-path)))
	  (if (not (package-get-load-package-file lispdir "auto-autoloads"))
	      (package-get-load-package-file lispdir "_pkg"))
	  t)
      nil)))

;;;###autoload
(defun package-get-info (package information &optional arg remote)
  "Get information about a package.

Quite similar to `package-get-info-prop', but can retrieve a lot more
information.

Argument PACKAGE is the name of an XEmacs package (a symbol).  It must
be a valid package, ie, a member of `package-get-base'.

Argument INFORMATION is a symbol that can be any one of:

   standards-version     Package system version (not used).
   version               Version of the XEmacs package.
   author-version        The upstream version of the package.
   date                  The date the package was last modified.
   build-date            The date the package was last built.
   maintainer            The maintainer of the package.
   distribution          Will always be \"xemacs\" (not used).
   priority              \"low\", \"medium\", or \"high\" (not used).
   category              Either \"standard\", \"mule\", or \"unsupported\"..
   dump                  Is the package dumped (not used).
   description           A description of the package.
   filename              The filename of the binary tarball of the package.
   md5sum                The md5sum of filename.
   size                  The size in bytes of filename.
   provides              A list of symbols that this package provides.
   requires              A list of packages that this package requires.
   type                  Can be either \"regular\" or \"single-file\".

If optional argument ARG is non-nil insert INFORMATION into current
buffer at point.  This is very useful for doing things like inserting
a maintainer's email address into a mail buffer.

If optional argument REMOTE is non-nil use a package list from a
remote site.  For this to work `package-get-remote' must be non-nil.

If this function is called interactively it will display INFORMATION
in the minibuffer."
  (interactive "SPackage: \nSInfo: \nP")
    (if remote
	(package-get-require-base t)
      (package-get-require-base nil))
    (let ((all-pkgs package-get-base)
	  info)
      (loop until (equal package (caar all-pkgs))
	do (setq all-pkgs (cdr all-pkgs))
	do (if (not all-pkgs)
	       (error 'invalid-argument
		      (format "%s is not a valid package" package))))
      (setq info (plist-get (cadar all-pkgs) information))
      (if (interactive-p)
	  (if arg
	      (insert (format "%s" info))
	    (if (package-get-key package :version)
		(message "%s" info)
	      (message "%s (Package: %s is not installed)" info package)))
	(if arg
	    (insert (format "%s" info))
	  info))))

;;;###autoload
(defun package-get-list-packages-where (item field &optional arg)
  "Return a list of packages that fulfill certain criteria.

Argument ITEM, a symbol, is what you want to check for.  ITEM must be a
symbol even when it doesn't make sense to be a symbol \(think, searching
maintainers, descriptions, etc\).  The function will convert the symbol
to a string if a string is what is needed.  The downside to this is that
ITEM can only ever be a single word.

Argument FIELD, a symbol, is the field to check in.  You can specify
any one of:

      Field            Sane or Allowable Content
    description          any single word
    category             `standard' or `mule'
    maintainer           any single word
    build-date           yyyy-mm-dd
    date                 yyyy-mm-dd
    type                 `regular' or `single'
    requires             any package name
    provides             any symbol
    priority             `low', `medium', or `high'

Optional Argument ARG, a prefix arg, insert output at point in the
current buffer."
  (interactive "SList packages that have (item): \nSin their (field): \nP")
  (package-get-require-base nil)
  (let ((pkgs package-get-base)
	(strings '(description category maintainer build-date date))
	(symbols '(type requires provides priority))
	results)
    (cond ((memq field strings)
	   (setq item (symbol-name item))
	   (while pkgs
	     (when (string-match item (package-get-info (caar pkgs) field))
	       (setq results (push (caar pkgs) results)))
	     (setq pkgs (cdr pkgs))))
	  ((memq field symbols)
	   (if (or (eq field 'type)
		   (eq field 'priority))
	       (while pkgs
		 (when (eq item (package-get-info (caar pkgs) field))
		   (setq results (push (caar pkgs) results)))
		 (setq pkgs (cdr pkgs)))
	     (while pkgs
	       (when (memq item (package-get-info (caar pkgs) field))
		 (setq results (push (caar pkgs) results)))
	       (setq pkgs (cdr pkgs)))))
	  (t 
	   (error 'wrong-type-argument field)))
    (if (interactive-p)
	(if arg
	    (insert (format "%s" results))
	  (message "%s" results)))
    results))

;;;###autoload
(defun package-get (package &optional version conflict install-dir)
  "Fetch PACKAGE from remote site.
Optional arguments VERSION indicates which version to retrieve, nil
means most recent version.  CONFLICT indicates what happens if the
package is already installed.  Valid values for CONFLICT are:
'always	always retrieve the package even if it is already installed
'never	do not retrieve the package if it is installed.
INSTALL-DIR, if non-nil, specifies the package directory where
fetched packages should be installed.

The value of `package-get-base' is used to determine what files should
be retrieved.  The value of `package-get-remote' is used to determine
where a package should be retrieved from.

Once the package is retrieved, its md5 checksum is computed.  If that
sum does not match that stored in `package-get-base' for this version
of the package, an error is signalled.

Returns `t' upon success, the symbol `error' if the package was
successfully installed but errors occurred during initialization, or
`nil' upon error."
  (interactive (package-get-interactive-package-query nil t))
  (catch 'skip-update
  (let* ((this-package
	  (package-get-info-version
	   (package-get-info-find-package package-get-base
					  package) version))
         (latest (package-get-info-prop this-package 'version))
         (installed (package-get-key package :version))
	 (found nil)
	 (search-dir package-get-remote)
	 (base-filename (package-get-info-prop this-package 'filename))
	 (package-status t)
	 filenames full-package-filename)
    (if (and (equal (package-get-info package 'category) "mule")
	     (not (featurep 'mule)))
	(error 'invalid-state 
	       "Mule packages can't be installed with a non-Mule XEmacs"))
    (if (null this-package)
	(if package-get-remote
	    (error 'search-failed
		   (format "Couldn't find package %s with version %s"
			   package version))
	  (error 'syntax-error
		 "No download site or local package location specified.")))
    (if (null base-filename)
	(error 'syntax-error
	       (format "No filename associated with package %s, version %s"
		       package version)))
    (setq install-dir (package-admin-get-install-dir package install-dir))

    ;; If they asked for the latest using version=nil, don't get an older
    ;; version than we already have.
    (if installed
        (if (> (if (stringp installed)
                   (string-to-number installed)
                 installed)
               (if (stringp latest)
                   (string-to-number latest)
                 latest))
            (if (not (null version))
                (warn "Installing %s package version %s, you had a newer version %s"
		  package latest installed)
              (warn "Skipping %s package, you have a newer version %s"
		package installed)
              (throw 'skip-update t))))

    ;; Contrive a list of possible package filenames.
    ;; Ugly.  Is there a better way to do this?
    (setq filenames (cons base-filename nil))
    (if (string-match "^\\(..*\\)\.tar\.gz$" base-filename)
	(setq filenames (append filenames
				(list (concat (match-string 1 base-filename)
					      ".tgz")))))

    (setq version latest)
    (unless (and (eq conflict 'never)
		 (package-get-installedp package version))
      ;; Find the package from the search list in package-get-remote
      ;; and copy it into the staging directory.  Then validate
      ;; the checksum.  Finally, install the package.
      (catch 'done
	(let (search-filenames host dir current-filename dest-filename)
	  ;; In each search directory ...
	  (when search-dir
	    (setq host (car search-dir)
		  dir (car (cdr search-dir))
		  search-filenames filenames)

	    ;; Look for one of the possible package filenames ...
	    (while search-filenames
	      (setq current-filename (car search-filenames)
		    dest-filename (package-get-staging-dir current-filename))
	      (cond
	       ;; No host means look on the current system.
	       ((null host)
		(setq full-package-filename
		      (substitute-in-file-name
		       (expand-file-name current-filename
					 (file-name-as-directory dir)))))

	       ;; If it's already on the disk locally, and the size is
	       ;; correct
	       ((and (file-exists-p dest-filename)
		     (eq (nth 7 (file-attributes dest-filename))
			 (package-get-info package 'size)))
		 (setq full-package-filename dest-filename))

	       ;; If the file exists on the remote system ...
	       ((file-exists-p (package-get-remote-filename
				search-dir current-filename))
		;; Get it
		(setq full-package-filename dest-filename)
		(message "Retrieving package `%s' ..."
			 current-filename)
		(sit-for 0)
		(copy-file (package-get-remote-filename search-dir
							current-filename)
			   full-package-filename t)))

	      ;; If we found it, we're done.
	      (if (and full-package-filename
		       (file-exists-p full-package-filename))
		  (throw 'done nil))
	      ;; Didn't find it.  Try the next possible filename.
	      (setq search-filenames (cdr search-filenames))))))

      (if (or (not full-package-filename)
	      (not (file-exists-p full-package-filename)))
	  (if package-get-remote
	      (error 'search-failed
		     (format "Unable to find file %s" base-filename))
	    (error 'syntax-error
		   "No download sites or local package locations specified.")))
      ;; Validate the md5 checksum
      ;; Doing it with XEmacs removes the need for an external md5 program
      (message "Validating checksum for `%s'..." package) (sit-for 0)
      (with-temp-buffer
	(insert-file-contents-literally full-package-filename)
	(if (not (string= (md5 (current-buffer))
			  (package-get-info-prop this-package
						 'md5sum)))
	    (progn
	      (delete-file full-package-filename)
	      (error 'process-error
		     (format "Package %s does not match md5 checksum %s has been deleted"
			     base-filename full-package-filename)))))

      (package-admin-delete-binary-package package install-dir)

      (message "Installing package `%s' ..." package) (sit-for 0)
      (let ((status
	     (package-admin-add-binary-package full-package-filename
					       install-dir)))
	(if (= status 0)
	    (progn
	      ;; clear messages so that only messages from
	      ;; package-get-init-package are seen, below.
	      (clear-message)
	      (if (package-get-init-package (package-admin-get-lispdir
					     install-dir package))
		  (progn
		    (run-hook-with-args 'package-install-hook package install-dir)
		    (message "Added package `%s'" package)
		    (sit-for 0))
		(progn
		  ;; display message only if there isn't already one.
		  (if (not (current-message))
		      (progn
			(message "Added package `%s' (errors occurred)"
				 package)
			(sit-for 0)))
		  (if package-status
		      (setq package-status 'errors)))))
	  (message "Installation of package %s failed." base-filename)
	  (sit-for 0)
	  (switch-to-buffer package-admin-temp-buffer)
	  (delete-file full-package-filename)
	  (setq package-status nil)))
      (setq found t))
    (if (and found package-get-remove-copy)
	(delete-file full-package-filename))
    package-status)))

(defun package-get-info-find-package (which name)
  "Look in WHICH for the package called NAME and return all the info
associated with it.  See `package-get-base' for info on the format
returned.

 To access fields returned from this, use
`package-get-info-version' to return information about particular a
version.  Use `package-get-info-find-prop' to find particular property
from a version returned by `package-get-info-version'."
  (interactive "xPackage list: \nsPackage Name: ")
  (if which
      (if (eq (caar which) name)
	  (cdar which)
	(if (cdr which)
	    (package-get-info-find-package (cdr which) name)))))

(defun package-get-info-version (package version)
  "In PACKAGE, return the plist associated with a particular VERSION of the
  package.  PACKAGE is typically as returned by
  `package-get-info-find-package'.  If VERSION is nil, then return the
  first (aka most recent) version.  Use `package-get-info-find-prop'
  to retrieve a particular property from the value returned by this."
  (interactive (package-get-interactive-package-query t t))
  (while (and version package (not (string= (plist-get (car package) 'version) version)))
    (setq package (cdr package)))
  (if package (car package)))

(defun package-get-info-prop (package-version property)
  "In PACKAGE-VERSION, return the value associated with PROPERTY.
PACKAGE-VERSION is typically returned by `package-get-info-version'
and PROPERTY is typically (although not limited to) one of the
following:

version		- version of this package
provides		- list of symbols provided
requires		- list of symbols that are required.
		  These in turn are provided by other packages.
size		- size of the bundled package
md5sum		- computed md5 checksum"
  (interactive "xPackage Version: \nSProperty")
  (plist-get package-version property))

(defun package-get-info-version-prop (package-list package version property)
  "In PACKAGE-LIST, search for PACKAGE with this VERSION and return
  PROPERTY value."
  (package-get-info-prop
   (package-get-info-version
    (package-get-info-find-package package-list package) version) property))

(defun package-get-staging-dir (filename)
  "Return a good place to stash FILENAME when it is retrieved.
Use `package-get-dir' for directory to store stuff.
Creates `package-get-dir'  if it doesn't exist."
  (interactive "FPackage filename: ")
  (if (not (file-exists-p package-get-dir))
      (make-directory package-get-dir))
  (expand-file-name
   (file-name-nondirectory (or (and (fboundp 'efs-ftp-path)
				    (nth 2 (efs-ftp-path filename)))
			       filename))
   (file-name-as-directory package-get-dir)))

(defun package-get-remote-filename (search filename)
  "Return FILENAME as a remote filename.
It first checks if FILENAME already is a remote filename.  If it is
not, then it uses the (car search) as the remote site-name and the (cadr
search) as the remote-directory and concatenates filename.  In other
words
	site-name:remote-directory/filename.

If (car search) is nil, (cadr search is interpreted as  a local directory).
"
  (if (file-remote-p filename)
      filename
    (let ((dir (cadr search)))
      (concat (when (car search)
		(concat
		 (if (string-match "@" (car search))
		     "/"
		   "/anonymous@")
		 (car search) ":"))
	      (if (string-match "/$" dir)
		  dir
		(concat dir "/"))
	      filename))))

(defun package-get-installedp (package version)
  "Determine if PACKAGE with VERSION has already been installed.
I'm not sure if I want to do this by searching directories or checking
some built in variables.  For now, use packages-package-list."
  ;; Use packages-package-list which contains name and version
  (equal (plist-get
	  (package-get-info-find-package packages-package-list
					 package) ':version)
	 (if (floatp version)
	     version
	   (string-to-number version))))

;;;###autoload
(defun package-get-package-provider (sym &optional force-current)
  "Search for a package that provides SYM and return the name and
  version.  Searches in `package-get-base' for SYM.   If SYM is a
  consp, then it must match a corresponding (provide (SYM VERSION)) from
  the package.

If FORCE-CURRENT is non-nil make sure the database is up to date. This might
lead to Emacs accessing remote sites."
  (interactive "SSymbol: ")
  (package-get-require-base force-current)
  (let ((packages package-get-base)
	(done nil)
	(found nil))
    (while (and (not done) packages)
      (let* ((this-name (caar packages))
	     (this-package (cdr (car packages)))) ;strip off package name
	(while (and (not done) this-package)
	  (if (or (eq this-name sym)
		  (eq (cons this-name
			  (package-get-info-prop (car this-package) 'version))
		      sym)
		  (member sym
			(package-get-info-prop (car this-package) 'provides)))
	      (progn (setq done t)
		     (setq found
		       (list (caar packages)
			 (package-get-info-prop (car this-package) 'version))))
	    (setq this-package (cdr this-package)))))
      (setq packages (cdr packages)))
    (when (interactive-p)
      (if found
          (message "%S" found)
        (message "No appropriate package found")))
    found))

(defun package-get-ever-installed-p (pkg &optional notused)
  (string-match "-package$" (symbol-name pkg))
  (custom-initialize-set
   pkg
   (if (package-get-info-find-package
	packages-package-list
	(intern (substring (symbol-name pkg) 0 (match-beginning 0))))
       t)))

(provide 'package-get)
;;; package-get.el ends here
