;;; ldap.el --- LDAP support for Emacs

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Created: Jan 1998
;; Version: $Revision: 1.10 $
;; Keywords: help comm

;; This file is part of SXEmacs

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
;;    This file provides mid-level and user-level functions to access directory
;;    servers using the LDAP protocol (RFC 1777).

;;; Installation:
;;    LDAP support must have been built into XEmacs.


;;; Code:

(globally-declare-fboundp '(ldapp ldap-open ldap-close ldap-add ldap-modify
				  ldap-delete))

(eval-when '(load)
  (if (not (fboundp 'ldap-open))
      (error "No LDAP support compiled in this XEmacs")))

(defgroup ldap nil
  "Lightweight Directory Access Protocol"
  :group 'comm)

(defcustom ldap-default-host nil
  "*Default LDAP server hostname.
A TCP port number can be appended to that name using a colon as
a separator."
  :type '(choice (string :tag "Host name")
		 (const :tag "Use library default" nil))
  :group 'ldap)

(defcustom ldap-default-port nil
  "*Default TCP port for LDAP connections.
Initialized from the LDAP library at build time. Default value is 389."
  :type '(choice (const :tag "Use library default" nil)
		 (integer :tag "Port number"))
  :group 'ldap)

(defcustom ldap-default-base nil
  "*Default base for LDAP searches.
This is a string using the syntax of RFC 1779.
For instance, \"o=ACME, c=US\" limits the search to the
Acme organization in the United States."
  :type '(choice (const :tag "Use library default" nil)
		 (string :tag "Search base"))
  :group 'ldap)


(defcustom ldap-host-parameters-alist nil
  "*Alist of host-specific options for LDAP transactions.
The format of each list element is:
\(HOST PROP1 VAL1 PROP2 VAL2 ...)
HOST is the hostname of an LDAP server (with an optional TCP port number
appended to it  using a colon as a separator).
PROPn and VALn are property/value pairs describing parameters for the server.
Valid properties include:
  `binddn' is the distinguished name of the user to bind as
    (in RFC 1779 syntax).
  `passwd' is the password to use for simple authentication.
  `auth' is the authentication method to use.
    Possible values are: `simple', `krbv41' and `krbv42'.
  `base' is the base for the search as described in RFC 1779.
  `scope' is one of the three symbols `subtree', `base' or `onelevel'.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return."
  :type '(repeat :menu-tag "Host parameters"
		 :tag "Host parameters"
		 (list :menu-tag "Host parameters"
		       :tag "Host parameters"
		       :value nil
		       (string :tag "Host name")
		       (checklist :inline t
				  :greedy t
				  (list
				   :tag "Search Base"
				   :inline t
				   (const :tag "Search Base" base)
				   string)
				  (list
				   :tag "Binding DN"
				   :inline t
				   (const :tag "Binding DN" binddn)
				   string)
				  (list
				   :tag "Password"
				   :inline t
				   (const :tag "Password" passwd)
				   string)
				  (list
				   :tag "Authentication Method"
				   :inline t
				   (const :tag "Authentication Method" auth)
				   (choice
				    (const :menu-tag "None" :tag "None" nil)
				    (const :menu-tag "Simple" :tag "Simple" simple)
				    (const :menu-tag "Kerberos 4.1" :tag "Kerberos 4.1" krbv41)
				    (const :menu-tag "Kerberos 4.2" :tag "Kerberos 4.2" krbv42)))
				  (list
				   :tag "Search Scope"
				   :inline t
				   (const :tag "Search Scope" scope)
				   (choice
				    (const :menu-tag "Default" :tag "Default" nil)
				    (const :menu-tag "Subtree" :tag "Subtree" subtree)
				    (const :menu-tag "Base" :tag "Base" base)
				    (const :menu-tag "One Level" :tag "One Level" onelevel)))
				  (list
				   :tag "Dereferencing"
				   :inline t
				   (const :tag "Dereferencing" deref)
				   (choice
				    (const :menu-tag "Default" :tag "Default" nil)
				    (const :menu-tag "Never" :tag "Never" never)
				    (const :menu-tag "Always" :tag "Always" always)
				    (const :menu-tag "When searching" :tag "When searching" search)
				    (const :menu-tag "When locating base" :tag "When locating base" find)))
				  (list
				   :tag "Time Limit"
				   :inline t
				   (const :tag "Time Limit" timelimit)
				   (integer :tag "(in seconds)"))
				  (list
				   :tag "Size Limit"
				   :inline t
				   (const :tag "Size Limit" sizelimit)
				   (integer :tag "(number of records)")))))
:group 'ldap)

(defcustom ldap-verbose nil
  "*If non-nil, LDAP operations echo progress messages."
  :type 'boolean
  :group 'ldap)

(defcustom ldap-ignore-attribute-codings nil
  "*If non-nil, do not perform any encoding/decoding on LDAP attribute values."
  :type 'boolean
  :group 'ldap)

(defcustom ldap-default-attribute-decoder nil
  "*Decoder function to use for attributes whose syntax is unknown."
  :type 'symbol
  :group 'ldap)

(defcustom ldap-coding-system nil
  "*Coding system of LDAP string values.
LDAP v3 specifies the coding system of strings to be UTF-8.
Mule support is needed for this."
  :type 'symbol
  :group 'ldap)

(defvar ldap-attribute-syntax-encoders
  [nil					; 1  ACI Item                        N
   nil					; 2  Access Point                    Y
   nil					; 3  Attribute Type Description      Y
   nil					; 4  Audio                           N
   nil					; 5  Binary                          N
   nil					; 6  Bit String                      Y
   ldap-encode-boolean			; 7  Boolean                         Y
   nil					; 8  Certificate                     N
   nil					; 9  Certificate List                N
   nil					; 10 Certificate Pair                N
   ldap-encode-country-string		; 11 Country String                  Y
   ldap-encode-string			; 12 DN                              Y
   nil					; 13 Data Quality Syntax             Y
   nil					; 14 Delivery Method                 Y
   ldap-encode-string			; 15 Directory String                Y
   nil					; 16 DIT Content Rule Description    Y
   nil					; 17 DIT Structure Rule Description  Y
   nil					; 18 DL Submit Permission            Y
   nil					; 19 DSA Quality Syntax              Y
   nil					; 20 DSE Type                        Y
   nil					; 21 Enhanced Guide                  Y
   nil					; 22 Facsimile Telephone Number      Y
   nil					; 23 Fax                             N
   nil					; 24 Generalized Time                Y
   nil					; 25 Guide                           Y
   nil					; 26 IA5 String                      Y
   number-to-string			; 27 INTEGER                         Y
   nil					; 28 JPEG                            N
   nil					; 29 Master And Shadow Access Points Y
   nil					; 30 Matching Rule Description       Y
   nil					; 31 Matching Rule Use Description   Y
   nil					; 32 Mail Preference                 Y
   nil					; 33 MHS OR Address                  Y
   nil					; 34 Name And Optional UID           Y
   nil					; 35 Name Form Description           Y
   nil					; 36 Numeric String                  Y
   nil					; 37 Object Class Description        Y
   nil					; 38 OID                             Y
   nil					; 39 Other Mailbox                   Y
   nil					; 40 Octet String                    Y
   ldap-encode-address			; 41 Postal Address                  Y
   nil					; 42 Protocol Information            Y
   nil					; 43 Presentation Address            Y
   ldap-encode-string			; 44 Printable String                Y
   nil					; 45 Subtree Specification           Y
   nil					; 46 Supplier Information            Y
   nil					; 47 Supplier Or Consumer            Y
   nil					; 48 Supplier And Consumer           Y
   nil					; 49 Supported Algorithm             N
   nil					; 50 Telephone Number                Y
   nil					; 51 Teletex Terminal Identifier     Y
   nil					; 52 Telex Number                    Y
   nil					; 53 UTC Time                        Y
   nil					; 54 LDAP Syntax Description         Y
   nil					; 55 Modify Rights                   Y
   nil					; 56 LDAP Schema Definition          Y
   nil					; 57 LDAP Schema Description         Y
   nil					; 58 Substring Assertion             Y
   ]
  "A vector of functions used to encode LDAP attribute values.
The sequence of functions corresponds to the sequence of LDAP attribute syntax
object identifiers of the form 1.3.6.1.4.1.1466.1115.121.1.* as defined in
RFC2252 section 4.3.2")

(defvar ldap-attribute-syntax-decoders
  [nil					; 1  ACI Item                        N
   nil					; 2  Access Point                    Y
   nil					; 3  Attribute Type Description      Y
   nil					; 4  Audio                           N
   nil					; 5  Binary                          N
   nil					; 6  Bit String                      Y
   ldap-decode-boolean			; 7  Boolean                         Y
   nil					; 8  Certificate                     N
   nil					; 9  Certificate List                N
   nil					; 10 Certificate Pair                N
   ldap-decode-string			; 11 Country String                  Y
   ldap-decode-string			; 12 DN                              Y
   nil					; 13 Data Quality Syntax             Y
   nil					; 14 Delivery Method                 Y
   ldap-decode-string			; 15 Directory String                Y
   nil					; 16 DIT Content Rule Description    Y
   nil					; 17 DIT Structure Rule Description  Y
   nil					; 18 DL Submit Permission            Y
   nil					; 19 DSA Quality Syntax              Y
   nil					; 20 DSE Type                        Y
   nil					; 21 Enhanced Guide                  Y
   nil					; 22 Facsimile Telephone Number      Y
   nil					; 23 Fax                             N
   nil					; 24 Generalized Time                Y
   nil					; 25 Guide                           Y
   nil					; 26 IA5 String                      Y
   string-to-number			; 27 INTEGER                         Y
   nil					; 28 JPEG                            N
   nil					; 29 Master And Shadow Access Points Y
   nil					; 30 Matching Rule Description       Y
   nil					; 31 Matching Rule Use Description   Y
   nil					; 32 Mail Preference                 Y
   nil					; 33 MHS OR Address                  Y
   nil					; 34 Name And Optional UID           Y
   nil					; 35 Name Form Description           Y
   nil					; 36 Numeric String                  Y
   nil					; 37 Object Class Description        Y
   nil					; 38 OID                             Y
   nil					; 39 Other Mailbox                   Y
   nil					; 40 Octet String                    Y
   ldap-decode-address			; 41 Postal Address                  Y
   nil					; 42 Protocol Information            Y
   nil					; 43 Presentation Address            Y
   ldap-decode-string			; 44 Printable String                Y
   nil					; 45 Subtree Specification           Y
   nil					; 46 Supplier Information            Y
   nil					; 47 Supplier Or Consumer            Y
   nil					; 48 Supplier And Consumer           Y
   nil					; 49 Supported Algorithm             N
   nil					; 50 Telephone Number                Y
   nil					; 51 Teletex Terminal Identifier     Y
   nil					; 52 Telex Number                    Y
   nil					; 53 UTC Time                        Y
   nil					; 54 LDAP Syntax Description         Y
   nil					; 55 Modify Rights                   Y
   nil					; 56 LDAP Schema Definition          Y
   nil					; 57 LDAP Schema Description         Y
   nil					; 58 Substring Assertion             Y
   ]
  "A vector of functions used to decode LDAP attribute values.
The sequence of functions corresponds to the sequence of LDAP attribute syntax
object identifiers of the form 1.3.6.1.4.1.1466.1115.121.1.* as defined in
RFC2252 section 4.3.2")


(defvar ldap-attribute-syntaxes-alist
  '((createtimestamp . 24)
    (modifytimestamp . 24)
    (creatorsname . 12)
    (modifiersname . 12)
    (subschemasubentry . 12)
    (attributetypes . 3)
    (objectclasses . 37)
    (matchingrules . 30)
    (matchingruleuse . 31)
    (namingcontexts . 12)
    (altserver . 26)
    (supportedextension . 38)
    (supportedcontrol . 38)
    (supportedsaslmechanisms . 15)
    (supportedldapversion . 27)
    (ldapsyntaxes . 16)
    (ditstructurerules . 17)
    (nameforms . 35)
    (ditcontentrules . 16)
    (objectclass . 38)
    (aliasedobjectname . 12)
    (cn . 15)
    (sn . 15)
    (serialnumber . 44)
    (c . 15)
    (l . 15)
    (st . 15)
    (street . 15)
    (o . 15)
    (ou . 15)
    (title . 15)
    (description . 15)
    (searchguide . 25)
    (businesscategory . 15)
    (postaladdress . 41)
    (postalcode . 15)
    (postofficebox . 15)
    (physicaldeliveryofficename . 15)
    (telephonenumber . 50)
    (telexnumber . 52)
    (telexterminalidentifier . 51)
    (facsimiletelephonenumber . 22)
    (x121address . 36)
    (internationalisdnnumber . 36)
    (registeredaddress . 41)
    (destinationindicator . 44)
    (preferreddeliverymethod . 14)
    (presentationaddress . 43)
    (supportedapplicationcontext . 38)
    (member . 12)
    (owner . 12)
    (roleoccupant . 12)
    (seealso . 12)
    (userpassword . 40)
    (usercertificate . 8)
    (cacertificate . 8)
    (authorityrevocationlist . 9)
    (certificaterevocationlist . 9)
    (crosscertificatepair . 10)
    (name . 15)
    (givenname . 15)
    (initials . 15)
    (generationqualifier . 15)
    (x500uniqueidentifier . 6)
    (dnqualifier . 44)
    (enhancedsearchguide . 21)
    (protocolinformation . 42)
    (distinguishedname . 12)
    (uniquemember . 34)
    (houseidentifier . 15)
    (supportedalgorithms . 49)
    (deltarevocationlist . 9)
    (dmdname . 15))
  "A map of LDAP attribute names to their type object id minor number.
This table is built from RFC2252 Section 5 and RFC2256 Section 5")


;; Coding/decoding functions

(defun ldap-encode-boolean (bool)
  (if bool
      "TRUE"
    "FALSE"))

(defun ldap-decode-boolean (str)
  (cond
   ((string-equal str "TRUE")
    t)
   ((string-equal str "FALSE")
    nil)
   (t
    (error "Wrong LDAP boolean string: %s" str))))

(defun ldap-encode-country-string (str)
  ;; We should do something useful here...
  (if (not (= 2 (length str)))
      (error "Invalid country string: %s" str)))

(defun ldap-decode-string (str)
  (if (fboundp 'decode-coding-string)
      (decode-coding-string str ldap-coding-system)))

(defun ldap-encode-string (str)
   (if (fboundp 'encode-coding-string)
       (encode-coding-string str ldap-coding-system)))

(defun ldap-decode-address (str)
  (mapconcat 'ldap-decode-string
	     (split-string str "\\$")
	     "\n"))

(defun ldap-encode-address (str)
  (mapconcat 'ldap-encode-string
	     (split-string str "\n")
	     "$"))


;; LDAP protocol functions

(defun ldap-get-host-parameter (host parameter)
  "Get the value of PARAMETER for HOST in `ldap-host-parameters-alist'."
  (plist-get (cdr (assoc host ldap-host-parameters-alist))
	     parameter))

(defun ldap-decode-attribute (attr)
  "Decode the attribute/value pair ATTR according to LDAP rules.
The attribute name is looked up in `ldap-attribute-syntaxes-alist'
and the corresponding decoder is then retrieved from
`ldap-attribute-syntax-decoders' and applied on the value(s)."
  (let* ((name (car attr))
	 (values (cdr attr))
	 (syntax-id (cdr (assq (intern (downcase name))
			       ldap-attribute-syntaxes-alist)))
	 decoder)
    (if syntax-id
	(setq decoder (aref ldap-attribute-syntax-decoders
			    (1- syntax-id)))
      (setq decoder ldap-default-attribute-decoder))
    (if decoder
	(cons name (mapcar decoder values))
      attr)))

(defun ldap-decode-entry (entry)
  "Decode the attributes of ENTRY according to LDAP rules."
  (let (dn decoded)
    (setq dn (car entry))
    (if (stringp dn)
	(setq entry (cdr entry))
      (setq dn nil))
    (setq decoded (mapcar 'ldap-decode-attribute entry))
    (if dn
	(cons dn decoded)
      decoded)))

(defun ldap-search (arg1 &rest args)
  "Perform an LDAP search."
      (apply (if (ldapp arg1)
		 'ldap-search-basic
	       'ldap-search-entries) arg1 args))

(make-obsolete 'ldap-search
	       "Use `ldap-search-entries' instead or
`ldap-search-basic' for the low-level search API.")

(defun ldap-search-entries (filter &optional host attributes attrsonly withdn)
  "Perform an LDAP search.
FILTER is the search filter in RFC1558 syntax, i.e., something that
looks like \"(cn=John Smith)\".
HOST is the LDAP host on which to perform the search.
ATTRIBUTES is a list of attributes to retrieve; nil means retrieve all.
If ATTRSONLY is non nil, the attributes will be retrieved without
the associated values.
If WITHDN is non-nil each entry in the result will be prepennded with
its distinguished name DN.
Additional search parameters can be specified through
`ldap-host-parameters-alist' which see.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs optionally preceded by the DN of the
entry according to the value of WITHDN."
  (interactive "sFilter:")
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap
	result)
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if ldap-verbose
	(message "Searching with LDAP on %s..." host))
    (setq result (with-obsolete-function 'ldap-search ldap filter
					 (plist-get host-plist 'base)
					 (plist-get host-plist 'scope)
					 attributes attrsonly withdn
					 ldap-verbose))
    (ldap-close ldap)
    (if ldap-ignore-attribute-codings
	result
      (mapcar 'ldap-decode-entry result))))

(defun ldap-add-entries (entries &optional host binddn passwd)
  "Add entries to an LDAP directory.
ENTRIES is a list of entry specifications of
the form (DN (ATTR . VALUE) (ATTR . VALUE) ...) where
DN is the distinguished name of an entry to add, the following
are cons cells containing attribute/value string pairs.
HOST is the LDAP host, defaulting to `ldap-default-host'.
BINDDN is the DN to bind as to the server.
PASSWD is the corresponding password."
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap
	(i 1))
    (if (or binddn passwd)
	(setq host-plist (copy-seq host-plist)))
    (if binddn
	(setq host-plist (plist-put host-plist 'binddn binddn)))
    (if passwd
	(setq host-plist (plist-put host-plist 'passwd passwd)))
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if ldap-verbose
	(message "Adding LDAP entries..."))
    (mapcar (function
	     (lambda (thisentry)
	       (ldap-add ldap (car thisentry) (cdr thisentry))
	       (if ldap-verbose
		   (message "%d added" i))
	       (setq i (1+ i))))
	    entries)
    (ldap-close ldap)))


(defun ldap-modify-entries (entry-mods &optional host binddn passwd)
  "Modify entries of an LDAP directory.
ENTRY_MODS is a list of entry modifications of the form
  (DN MOD-SPEC1 MOD-SPEC2 ...) where DN is the distinguished name of
the entry to modify, the following are modification specifications.
A modification specification is itself a list of the form
(MOD-OP ATTR VALUE1 VALUE2 ...) MOD-OP and ATTR are mandatory,
VALUEs are optional depending on MOD-OP.
MOD-OP is the type of modification, one of the symbols `add', `delete'
or `replace'. ATTR is the LDAP attribute type to modify.
HOST is the LDAP host, defaulting to `ldap-default-host'.
BINDDN is the DN to bind as to the server.
PASSWD is the corresponding password."
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap
	(i 1))
    (if (or binddn passwd)
	(setq host-plist (copy-seq host-plist)))
    (if binddn
	(setq host-plist (plist-put host-plist 'binddn binddn)))
    (if passwd
	(setq host-plist (plist-put host-plist 'passwd passwd)))
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if ldap-verbose
	(message "Modifying LDAP entries..."))
    (mapcar (function
	     (lambda (thisentry)
	       (ldap-modify ldap (car thisentry) (cdr thisentry))
	       (if ldap-verbose
		   (message "%d modified" i))
	       (setq i (1+ i))))
	    entry-mods)
    (ldap-close ldap)))


(defun ldap-delete-entries (dn &optional host binddn passwd)
  "Delete an entry from an LDAP directory.
DN is the distinguished name of an entry to delete or
a list of those.
HOST is the LDAP host, defaulting to `ldap-default-host'.
BINDDN is the DN to bind as to the server.
PASSWD is the corresponding password."
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	ldap)
    (if (or binddn passwd)
	(setq host-plist (copy-seq host-plist)))
    (if binddn
	(setq host-plist (plist-put host-plist 'binddn binddn)))
    (if passwd
	(setq host-plist (plist-put host-plist 'passwd passwd)))
    (if ldap-verbose
	(message "Opening LDAP connection to %s..." host))
    (setq ldap (ldap-open host host-plist))
    (if (consp dn)
	(let ((i 1))
	  (if ldap-verbose
	      (message "Deleting LDAP entries..."))
	  (mapcar (function
		   (lambda (thisdn)
		     (ldap-delete ldap thisdn)
		     (if ldap-verbose
			 (message "%d deleted" i))
		     (setq i (1+ i))))
		  dn))
      (if ldap-verbose
	  (message "Deleting LDAP entry..."))
      (ldap-delete ldap dn))
    (ldap-close ldap)))


(provide 'ldap)

;;; ldap.el ends here
