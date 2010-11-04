/* Generic select data structures functions
   Copyright (C) 1999 Andy Piper

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Not in FSF. */

#ifndef INCLUDED_select_h_
#define INCLUDED_select_h_

/* X Atoms */
extern Lisp_Object QPRIMARY, QSECONDARY, QSTRING, QINTEGER, QCLIPBOARD,
    QTIMESTAMP, QTEXT, QDELETE, QMULTIPLE, QINCR, QEMACS_TMP, QTARGETS, QATOM,
    QNULL, QATOM_PAIR, QCOMPOUND_TEXT;

/* Windows clipboard formats */
extern Lisp_Object QCF_TEXT, QCF_BITMAP, QCF_METAFILEPICT, QCF_SYLK, QCF_DIF,
    QCF_TIFF, QCF_OEMTEXT, QCF_DIB, QCF_DIBV5, QCF_PALETTE, QCF_PENDATA,
    QCF_RIFF, QCF_WAVE, QCF_UNICODETEXT, QCF_ENHMETAFILE, QCF_HDROP, QCF_LOCALE,
    QCF_OWNERDISPLAY, QCF_DSPTEXT, QCF_DSPBITMAP, QCF_DSPMETAFILEPICT,
    QCF_DSPENHMETAFILE;

/* Selection strategies */
extern Lisp_Object Qreplace_all, Qreplace_existing, Qappend;

/* "Selection owner couldn't convert selection" */
extern Lisp_Object Qselection_conversion_error;

/* Selection input & output */
Lisp_Object select_convert_in(Lisp_Object selection,
			      Lisp_Object type, Lisp_Object value);
Lisp_Object select_convert_out(Lisp_Object selection,
			       Lisp_Object type, Lisp_Object value);
Lisp_Object select_coerce(Lisp_Object selection,
			  Lisp_Object type, Lisp_Object value);

/* Notifications */
void handle_selection_clear(Lisp_Object selection_symbol);

void select_notify_buffer_kill(Lisp_Object buffer);

/* Lisp functions we export for other files' use */
EXFUN(Fregister_selection_data_type, 2);
EXFUN(Fselection_data_type_name, 2);
EXFUN(Favailable_selection_types, 2);
EXFUN(Fselection_owner_p, 1);
EXFUN(Fselection_exists_p, 3);
EXFUN(Fget_selection_timestamp, 1);

#endif				/* INCLUDED_select_h_ */
