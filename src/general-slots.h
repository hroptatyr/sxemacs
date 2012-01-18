/* Commonly-used symbols -- include file
   Copyright (C) 1995 Sun Microsystems.
   Copyright (C) 1995, 1996, 2000 Ben Wing.

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

/* The purpose of this file is as a central place to stick symbols
   that don't have any obvious connection to any particular module
   and might be used in many different contexts.

   Three types of declarations are allowed here:

   SYMBOL (Qfoo); declares a symbol "foo"
   SYMBOL_KEYWORD (Q_foo); declares a keyword symbol ":foo"
   SYMBOL_GENERAL (Qfoo, "bar"); declares a symbol named "bar" but stored in
     the variable Qfoo

To sort the crap in this file, use the following:

(sort-regexp-fields nil
		    "^.*(Q_?\\(.*\\));$" "\\1"
		    (progn
		      (search-forward "SYMBOL")
		      (match-beginning 0))
		    (point-max))
*/

SYMBOL(Qabort);
SYMBOL_KEYWORD(Q_accelerator);
SYMBOL_KEYWORD(Q_active);
SYMBOL(Qactually_requested);
SYMBOL(Qafter);
SYMBOL(Qall);
SYMBOL(Qand);
SYMBOL(Qappend);
SYMBOL(Qassoc);
SYMBOL(Qat);
SYMBOL(Qauto);
SYMBOL(Qautodetect);
SYMBOL(Qbad_variable);
SYMBOL(Qbefore);
SYMBOL(Qbigc);
SYMBOL(Qbigf);
SYMBOL(Qbigfloat);
SYMBOL(Qbigfr);
SYMBOL(Qbigg);
SYMBOL(Qbignum);
SYMBOL(Qbigq);
SYMBOL(Qbigz);
SYMBOL(Qbinary);
SYMBOL(Qbitmap);
SYMBOL(Qboolean);
SYMBOL_KEYWORD(Q_border);
SYMBOL(Qbottom);
SYMBOL(Qbottom_margin);
SYMBOL(Qbuffer);
SYMBOL(Qbuilt_in);
SYMBOL(Qbutton);
SYMBOL_KEYWORD(Q_buttons);
SYMBOL_KEYWORD(Q_callback);
SYMBOL_KEYWORD(Q_callback_ex);
SYMBOL(Qcancel);
SYMBOL(Qcategory);
SYMBOL(Qcenter);
SYMBOL(Qchannel);
SYMBOL(Qchar);
SYMBOL(Qcharacter);
SYMBOL(Qchars);
SYMBOL(Qcolor);
SYMBOL(Qcolumns);
SYMBOL(Qcommand);
SYMBOL_KEYWORD(Q_config);
SYMBOL(Qconsole);
SYMBOL(Qcopies);
SYMBOL(Qcritical);
SYMBOL(Qctext);
SYMBOL(Qcursor);
SYMBOL(Qdata);
SYMBOL_KEYWORD(Q_data);
SYMBOL(Qdead);
SYMBOL(Qdefault);
SYMBOL(Qdelete);
SYMBOL(Qdelq);
SYMBOL_KEYWORD(Q_descriptor);
SYMBOL(Qdevice);
SYMBOL_KEYWORD(Q_device);
SYMBOL(Qdialog);
SYMBOL(Qdimension);
SYMBOL(Qdirectory);
SYMBOL(Qdisplay);
SYMBOL(Qdoc_string);
SYMBOL(Qduplex);
SYMBOL(Qdynarr_overhead);
SYMBOL(Qempty);
SYMBOL(Qeq);
SYMBOL(Qeql);
SYMBOL(Qequal);
SYMBOL(Qeval);
SYMBOL(Qextents);
SYMBOL(Qface);
SYMBOL(Qfallback);
SYMBOL(Qfile);
SYMBOL_KEYWORD(Q_file);
SYMBOL(Qfile_name);
SYMBOL_KEYWORD(Q_filter);
SYMBOL(Qfixnum);
SYMBOL(Qfloat);
SYMBOL(Qfont);
SYMBOL(Qframe);
SYMBOL(Qfrom_page);
SYMBOL(Qfull_assoc);
SYMBOL(Qfuncall);
SYMBOL(Qfunction);
SYMBOL(Qgap_overhead);
SYMBOL(Qgeneric);
SYMBOL(Qgeometry);
SYMBOL(Qglobal);
SYMBOL(Qgutter);
SYMBOL(Qheight);
SYMBOL_KEYWORD(Q_height);
SYMBOL(Qhelp);
SYMBOL(Qhighlight);
SYMBOL(Qhorizontal);
SYMBOL_KEYWORD(Q_horizontally_justify);
SYMBOL(Qicon);
SYMBOL(Qid);
SYMBOL(Qignore);
SYMBOL(Qimage);
SYMBOL_KEYWORD(Q_image);
SYMBOL_KEYWORD(Q_included);
SYMBOL(Qinfo);
SYMBOL(Qinherit);
SYMBOL_KEYWORD(Q_initial_focus);
SYMBOL(Qint);
SYMBOL(Qinteger);
SYMBOL(Qinternal);
SYMBOL_KEYWORD(Q_items);
SYMBOL_KEYWORD(Q_justify);
SYMBOL(Qkey);
SYMBOL(Qkey_assoc);
SYMBOL_KEYWORD(Q_key_sequence);
SYMBOL(Qkeyboard);
SYMBOL(Qkeymap);
SYMBOL_KEYWORD(Q_keys);
SYMBOL_KEYWORD(Q_label);
SYMBOL(Qlandscape);
SYMBOL(Qlast_command);
SYMBOL(Qleft);
SYMBOL(Qleft_margin);
SYMBOL(Qlet);
SYMBOL(Qlist);
SYMBOL(Qmagic);
SYMBOL(Qmalloc_overhead);
SYMBOL_KEYWORD(Q_margin_width);
SYMBOL(Qmarkers);
SYMBOL(Qmax);
SYMBOL(Qmemory);
SYMBOL(Qmenubar);
SYMBOL(Qmessage);
SYMBOL_GENERAL(Qminus, "-");
SYMBOL(Qmodifiers);
SYMBOL(Qmotion);
SYMBOL(Qmsprinter);
SYMBOL(Qmswindows);
SYMBOL(Qname);
SYMBOL(Qnatnum);
SYMBOL(Qno);
SYMBOL(Qnone);
SYMBOL(Qnot);
SYMBOL(Qnothing);
SYMBOL(Qnotice);
SYMBOL(Qobject);
SYMBOL(Qok);
SYMBOL(Qold_assoc);
SYMBOL(Qold_delete);
SYMBOL(Qold_delq);
SYMBOL(Qold_rassoc);
SYMBOL(Qold_rassq);
SYMBOL(Qonly);
SYMBOL(Qor);
SYMBOL(Qorientation);
SYMBOL_KEYWORD(Q_orientation);
SYMBOL(Qother);
SYMBOL(Qpage_setup);
SYMBOL(Qpages);
SYMBOL(Qpeer);
SYMBOL(Qpointer);
SYMBOL(Qpopup);
SYMBOL(Qportrait);
SYMBOL(Qprepend);
SYMBOL(Qprint);
SYMBOL(Qprinter);
SYMBOL_KEYWORD(Q_printer_settings);
SYMBOL(Qprocess);
SYMBOL_KEYWORD(Q_properties);
SYMBOL(Qprovide);
SYMBOL(Qquestion);
SYMBOL_KEYWORD(Q_question);
SYMBOL(Qradio);
SYMBOL(Qrassoc);
SYMBOL(Qrassq);
SYMBOL(Qratio);
SYMBOL(Qrational);
SYMBOL(Qreal);
SYMBOL(Qremove_all);
SYMBOL(Qrequire);
SYMBOL(Qresource);
SYMBOL(Qretry);
SYMBOL(Qreturn);
SYMBOL(Qreverse);
SYMBOL(Qright);
SYMBOL(Qright_margin);
SYMBOL(Qsearch);
SYMBOL(Qselected);
SYMBOL_KEYWORD(Q_selected);
SYMBOL(Qselection);
SYMBOL(Qset_glyph_image);
SYMBOL(Qshort);
SYMBOL(Qsignal);
SYMBOL(Qsimple);
SYMBOL(Qsize);
SYMBOL(Qspace);
SYMBOL(Qspecifier);
SYMBOL(Qstandard);
SYMBOL(Qstream);
SYMBOL(Qstring);
SYMBOL_KEYWORD(Q_style);
SYMBOL_KEYWORD(Q_suffix);
SYMBOL(Qsymbol);
SYMBOL(Qsyntax);
SYMBOL(Qterminal);
SYMBOL(Qtest);
SYMBOL(Qtext);
SYMBOL_KEYWORD(Q_text);
SYMBOL(Qthis_command);
SYMBOL(Qtimeout);
SYMBOL(Qtimestamp);
SYMBOL_KEYWORD(Q_title);
SYMBOL(Qto_page);
SYMBOL(Qtoggle);
SYMBOL(Qtoolbar);
SYMBOL(Qtop);
SYMBOL(Qtop_margin);
SYMBOL(Qtty);
SYMBOL(Qtype);
SYMBOL(Qundecided);
SYMBOL(Qundefined);
SYMBOL(Qunimplemented);
SYMBOL_KEYWORD(Q_url);
SYMBOL_KEYWORD(Q_value);
SYMBOL(Qvalue_assoc);
SYMBOL(Qvertical);
SYMBOL_KEYWORD(Q_vertically_justify);
SYMBOL(Qvoid);
SYMBOL(Qwarning);
SYMBOL(Qwidget);
SYMBOL(Qwidth);
SYMBOL_KEYWORD(Q_width);
SYMBOL(Qwindow);
SYMBOL(Qwindow_system);
SYMBOL(Qworker_finished_work);
SYMBOL(Qworker_started_work);
SYMBOL(Qworker_suicide);
SYMBOL(Qx);
SYMBOL(Qy);
SYMBOL(Qyes);
