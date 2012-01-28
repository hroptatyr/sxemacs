/* CANNA interface -*- coding: euc-jp -*-

   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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


/* Synched up with: Mule 2.3.  Not in FSF. */

/* Japanese comments were translated 2000-12-06 by Stephen Turnbull
   <stephen@xemacs.org>.  I haven't verified that the Japanese comments
   were correct.  YMMV, NO WARRANTY, not even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  (^^;;; as the
   Japanese say. */

/*

  Authors: Akira Kon (kon@uxd.fc.nec.co.jp)
	   Ichiro Hirakura (hirakura@uxd.fc.nec.co.jp)

  Functions defined in this file are

  (canna-key-proc key)
		key: single STRING
		RETURNS:
			 Length of converted string if no error occurs.
			 Error string if error occurs.
		DESCRIPTION:
			 Convert a key input to a set of strings.  The
			 strings contain both well-formed string and a
			 intermediate result to show the translation
			 information to a user.  converted strings are
			 stored in specific variables.

  (canna-initialize)
		RETURNS:
			List of the following things:
			- list of keys to toggle Japanese-mode
			- error message
			- list of warning messages
		DESCRIPTION:
			Initialize ``canna'', which is a kana-to-kanji
			converter for GNU Emacs.  The first arg
			specifies if inserting space character between
			BUNSETSU when candidates are displayed.  The
			second arg specifies server.  The third arg
			specifies a file which will be used as a
			customization description.  If nil is
			specified for each arg, the default value will
			be used.

  (canna-finalize)
		RETURNS:
			list of warning messages
		DESCRIPTION:
			finalize ``canna'', which is a kana-to-kanji
			converter for GNU Emacs.  This cause to write
			miscellaneous informations to kana-to-kanji
			dictionary.

  (canna-touroku-string string)
		string:
			String to register to a dictionary.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Register Kanji words into kana-to-kanji
			conversion dictionary.

  (canna-set-width width)
		width:
			Column width of the place where the candidates
			of kana-to-kanji conversion will be shown.
		RETURNS:
			nil
		DESCRIPTION:
			Set status-line width information, which is
			used to display kanji candidates.

  (canna-change-mode num)
		num:
			The mode number of Canna.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Change Japanese pre-edit mode.

  (canna-store-yomi yomi roma)
		yomi:
			``Yomi'' to be stored.
		roma:
			``Romaji'' which corresponds to the ``Yomi''.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Store yomi characters as a YOMI of
			kana-to-kanji conversion.

  (canna-do-function num ch)
		num:
			A function number to be called.
		ch:
			A character will be specified in order to feed
			the character to the function if the function
			needs a input character.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Do specified function at current mode.

  (canna-parse string)
		string:
			To be parsed.
		RETURNS:
			List of warning messages.
		DESCRIPTION:
			Parse customize string.

  (canna-query-mode)
		RETURNS:
			A string which indicate the current mode.
		DESCRIPTION:
			Get current mode string.

  Functions below are used for KKCP compatible library.  These
  functions provides a base kana-to-kanji conversion system for EGG.
  These functions may be used when users want to change the engine
  from Wnn to Canna without changing user interface of Japanese input.

  (canna-henkan-begin)
  (canna-henkan-next)
  (canna-bunsetu-henkou)
  (canna-henkan-kakutei)
  (canna-henkan-end)
  (canna-henkan-quit)

 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "file-coding.h"

#ifdef CANNA2
#define IROHA_BC
#include "canna/jrkanji.h"
#include "canna/RK.h"
#else				/* !CANNA2 */
#include "iroha/jrkanji.h"
#include "iroha/RK.h"
#endif				/* !CANNA2 */
extern char *jrKanjiError;

/* #### is this global really necessary? */
#define KEYTOSTRSIZE 2048
static unsigned char key_buffer[KEYTOSTRSIZE];
static char **warning;

static int canna_empty_info, canna_through_info;
static int canna_underline;
static int canna_inhibit_hankakukana;

static Lisp_Object Vcanna_kakutei_string;
static Lisp_Object Vcanna_kakutei_yomi;
static Lisp_Object Vcanna_kakutei_romaji;
static Lisp_Object Vcanna_henkan_string;
static Fixnum canna_henkan_length;
static Fixnum canna_henkan_revPos;
static Fixnum canna_henkan_revLen;
static Lisp_Object Vcanna_ichiran_string;
static Fixnum canna_ichiran_length;
static Fixnum canna_ichiran_revPos;
static Fixnum canna_ichiran_revLen;
static Lisp_Object Vcanna_mode_string;

static int IRCP_context;

static Lisp_Object storeResults(unsigned char *, int, jrKanjiStatus *);
static Lisp_Object kanjiYomiList(int, int);
static Lisp_Object CANNA_mode_keys(void);

#ifdef CANNA_MULE
static void m2c(unsigned char *, int, unsigned char *);
static Lisp_Object mule_make_string(unsigned char *, int);
static int mule_strlen(unsigned char *, int);
static void count_char(unsigned char *, int, int, int, Fixnum *, Fixnum *,
		       Fixnum *);
#define make_string mule_make_string
#endif

/* Lisp functions definition */

DEFUN("canna-key-proc", Fcanna_key_proc, 1, 1, 0,	/*
Translate a key input to a set of strings.  The strings contain both
well-formed string and intermediate result to show the translation
information to a user.  Converted strings are stored in specific
variables.
*/
      (ch))
{
	jrKanjiStatus ks;
	int len;

	CHECK_CHAR_COERCE_INT(ch);
	len = jrKanjiString(0, XCHAR(ch), key_buffer, KEYTOSTRSIZE, &ks);
	return storeResults(key_buffer, len, &ks);
}

static Lisp_Object storeResults(unsigned char *buf, int len, jrKanjiStatus * ks)
{
	Lisp_Object val = Qnil;

	if (len < 0) {		/* Error detected */
		val =
		    make_string((unsigned char *)jrKanjiError,
				strlen(jrKanjiError));
	} else {
		/* 確定した文字列 (the confirmed string) */
		Vcanna_kakutei_string = make_string(buf, len);
		val = make_int(len);
		/* 確定した文字列の読みの情報...
		   (info about the reading of the confirmed string) */
		Vcanna_kakutei_yomi = Vcanna_kakutei_romaji = Qnil;
		if (ks->info & KanjiYomiInfo) {
			unsigned char *p = buf + len + 1;
			int yomilen = strlen(p);

			if (len + yomilen + 1 < KEYTOSTRSIZE) {
				int yomilen2;

				Vcanna_kakutei_yomi = make_string(p, yomilen);	/* 読み
										   (reading) */
				p += yomilen + 1;
				yomilen2 = strlen(p);
				if (len + yomilen + yomilen2 + 2 < KEYTOSTRSIZE) {
					Vcanna_kakutei_romaji =
					    make_string(p, yomilen2);
					/* ローマ字 (romanization) */
				}
			}
		}

		/* 候補表示の文字列です。
		   (string for displaying candidate translations) */
		Vcanna_henkan_string = Qnil;
		if (ks->length >= 0) {
			Vcanna_henkan_string =
			    make_string(ks->echoStr, ks->length);
#ifndef CANNA_MULE
			canna_henkan_length = ks->length;
			canna_henkan_revPos = ks->revPos;
			canna_henkan_revLen = ks->revLen;
#else				/* CANNA_MULE */
			if (canna_underline) {
				canna_henkan_length =
				    mule_strlen(ks->echoStr, ks->length);
				canna_henkan_revPos =
				    mule_strlen(ks->echoStr, ks->revPos);
				canna_henkan_revLen =
				    mule_strlen(ks->echoStr + ks->revPos,
						ks->revLen);
			} else {
				count_char(ks->echoStr, ks->length, ks->revPos,
					   ks->revLen, &canna_henkan_length,
					   &canna_henkan_revPos,
					   &canna_henkan_revLen);
			}
#endif				/* CANNA_MULE */
		}

		/* 一覧の情報 (information about the echo area menu) */
		Vcanna_ichiran_string = Qnil;
		if (ks->info & KanjiGLineInfo && ks->gline.length >= 0) {
			Vcanna_ichiran_string = make_string(ks->gline.line,
							    ks->gline.length);
#ifndef CANNA_MULE
			canna_ichiran_length = ks->gline.length;
			canna_ichiran_revPos = ks->gline.revPos;
			canna_ichiran_revLen = ks->gline.revLen;
#else				/* CANNA_MULE */
			count_char(ks->gline.line, ks->gline.length,
				   ks->gline.revPos, ks->gline.revLen,
				   &canna_ichiran_length,
				   &canna_ichiran_revPos,
				   &canna_ichiran_revLen);
#endif				/* CANNA_MULE */
		}

		/* モードの情報 (mode information) */
		Vcanna_mode_string = Qnil;
		if (ks->info & KanjiModeInfo) {
			Vcanna_mode_string =
			    make_string(ks->mode, strlen(ks->mode));
		}

		/* その他の情報 (other information) */
		canna_empty_info = (ks->info & KanjiEmptyInfo) ? 1 : 0;
		canna_through_info = (ks->info & KanjiThroughInfo) ? 1 : 0;
	}

	return val;
}

DEFUN("canna-set-bunsetsu-kugiri", Fcanna_set_bunsetsu, 0, 1, 0,	/*
This function sets the clause separator.
If non-nil value is specified, the white space separator will be used.
No separator will be used otherwise.
*/
      (num))
{
	int kugiri;		/* 文節区切りをするか？ (display clause separator?) */

	kugiri = NILP(num) ? 0 : 1;

	jrKanjiControl(0, KC_SETBUNSETSUKUGIRI, (char *)kugiri);

	return Qnil;
}

/* For whatever reason, calling Fding directly from libCanna loses */
static void call_Fding(void)
{
	Fding(Qnil, Qnil, Qnil);
}

DEFUN("canna-initialize", Fcanna_initialize, 0, 3, 0,	/*
Initialize ``canna'', which is a kana-to-kanji converter for GNU Emacs.
The first arg specifies if inserting space character between BUNSETSU when
candidates are displayed.
The second arg specifies server.
The third arg specifies a file which will be used as a customization
description.
If nil is specified for each arg, the default value will be used.
*/
      (num, server, rcfile))
{
	Lisp_Object val;
	int res;
	unsigned char **p, **q;

	int kugiri;		/* 文節区切りをするか？ (display clause separator?) */

	IRCP_context = -1;

	if (NILP(num)) {
		kugiri = 1;
	} else {
		CHECK_INT(num);
		kugiri = XINT(num);
		kugiri = (kugiri == 1) ? 1 : 0;
	}

	if (NILP(server)) {
		jrKanjiControl(0, KC_SETSERVERNAME, (char *)0);
	} else {
		char servername[256];

		CHECK_STRING(server);
		strncpy(servername, XSTRING_DATA(server),
			XSTRING_LENGTH(server));
		servername[XSTRING_LENGTH(server)] = '\0';
		jrKanjiControl(0, KC_SETSERVERNAME, servername);
	}

	if (NILP(rcfile)) {
		jrKanjiControl(0, KC_SETINITFILENAME, (char *)0);
	} else {
		char rcname[256];

		CHECK_STRING(rcfile);
		strncpy(rcname, XSTRING_DATA(rcfile), XSTRING_LENGTH(rcfile));
		rcname[XSTRING_LENGTH(rcfile)] = '\0';
		jrKanjiControl(0, KC_SETINITFILENAME, rcname);
	}

	warning = (char **)0;
#ifdef nec_ews_svr4
	stop_polling();
#endif				/* nec_ews_svr4 */
	res = jrKanjiControl(0, KC_INITIALIZE, (char *)&warning);
#ifdef nec_ews_svr4
	start_polling();
#endif				/* nec_ews_svr4 */
	val = Qnil;
	if (warning) {
		for (p = q = (unsigned char **)warning; *q; q++) ;
		while (p < q) {
			q--;
			val = Fcons(make_string(*q, strlen(*q)), val);
		}
	}
	val = Fcons(val, Qnil);

	if (res == -1) {
		val = Fcons(make_string((unsigned char *)jrKanjiError,
					strlen(jrKanjiError)), val);
		/* イニシャライズで失敗した場合。 (on initialization failure) */
		return Fcons(Qnil, val);
	} else {
		extern void (*jrBeepFunc) (void);

		jrBeepFunc = call_Fding;

#ifdef KC_SETAPPNAME
#ifndef CANNA_MULE
		wcKanjiControl(0, KC_SETAPPNAME, "nemacs");
#else				/* CANNA_MULE */
		wcKanjiControl(0, KC_SETAPPNAME, "mule");
#endif				/* CANNA_MULE */
#endif				/* KC_SETAPPNAME */

		jrKanjiControl(0, KC_SETBUNSETSUKUGIRI, (char *)kugiri);
		jrKanjiControl(0, KC_SETWIDTH, (char *)78);
#ifndef CANNA_MULE
		jrKanjiControl(0, KC_INHIBITHANKAKUKANA, (char *)1);
#else
		/* mule だったら半角カタカナも使える
		   (Mule can use half-width katakana) */
		if (canna_inhibit_hankakukana)
			jrKanjiControl(0, KC_INHIBITHANKAKUKANA, (char *)1);
#endif
		jrKanjiControl(0, KC_YOMIINFO, (char *)2);	/* ※２: ローマ字まで返す
								   (*2: return to
								   romanized form) */
		val = Fcons(Qnil, val);
		return Fcons(CANNA_mode_keys(), val);
	}
}

DEFUN("canna-finalize", Fcanna_finalize, 0, 0, 0,	/*
finalize ``canna'', which is a kana-to-kanji converter for GNU Emacs.
This cause to write miscellaneous informations to kana-to-kanji dictionary.
*/
      ())
{
	Lisp_Object val;
	unsigned char **p;

	jrKanjiControl(0, KC_FINALIZE, (char *)&warning);

	val = Qnil;
	if (warning) {
		for (p = (unsigned char **)warning; *p; p++) {
			val = Fcons(make_string(*p, strlen(*p)), val);
		}
	}
	val = Fcons(val, Qnil);
	IRCP_context = -1;
	return val;
}

DEFUN("canna-touroku-string", Fcanna_touroku_string, 1, 1, 0,	/*
Register Kanji words into kana-to-kanji conversion dictionary.
*/
      (str))
{
	jrKanjiStatusWithValue ksv;
	jrKanjiStatus ks;
	int len;
	Lisp_Object val;
#ifdef CANNA_MULE
	unsigned char cbuf[4096];
#endif

	CHECK_STRING(str);
	ksv.buffer = (unsigned char *)key_buffer;
	ksv.bytes_buffer = KEYTOSTRSIZE;
#ifndef CANNA_MULE
	ks.echoStr = XSTRING_DATA(str);
	ks.length = XSTRING_LENGTH(str);
#else				/* CANNA_MULE */
	m2c(XSTRING_DATA(str), XSTRING_LENGTH(str), cbuf);
	ks.echoStr = cbuf;
	ks.length = strlen(cbuf);
#endif				/* CANNA_MULE */
	ksv.ks = &ks;
	len = jrKanjiControl(0, KC_DEFINEKANJI, (char *)&ksv);
	val = storeResults(key_buffer, ksv.val, ksv.ks);
	return val;
}

DEFUN("canna-set-width", Fcanna_set_width, 1, 1, 0,	/*
Set status-line width information, which is used to display
kanji candidates.
*/
      (num))
{
	CHECK_INT(num);

	jrKanjiControl(0, KC_SETWIDTH, (char *)XINT(num));
	return Qnil;
}

DEFUN("canna-change-mode", Fcanna_change_mode, 1, 1, 0,	/*
Change Japanese pre-edit mode.
*/
      (num))
{
	jrKanjiStatusWithValue ksv;
	jrKanjiStatus ks;
	Lisp_Object val;

	CHECK_INT(num);

	ksv.buffer = (unsigned char *)key_buffer;
	ksv.bytes_buffer = KEYTOSTRSIZE;
	ksv.ks = &ks;
	ksv.val = XINT(num);
	jrKanjiControl(0, KC_CHANGEMODE, (char *)&ksv);
	val = storeResults(key_buffer, ksv.val, ksv.ks);
	return val;
}

static Lisp_Object CANNA_mode_keys(void)
{
#define CANNAWORKBUFSIZE 32
	char xxx[CANNAWORKBUFSIZE];
	Lisp_Object val;
	int i, n;

	n = jrKanjiControl(0, KC_MODEKEYS, xxx);
	val = Qnil;
	for (i = n; i > 0;) {
		--i;
		/* !!#### something fucked here */
		val =
		    Fcons(make_char((int)(0xFF & (unsigned char)xxx[i])), val);
	}
	return val;
}

DEFUN("canna-store-yomi", Fcanna_store_yomi, 1, 2, 0,	/*
Store yomi characters as a YOMI of kana-to-kanji conversion.
*/
      (yomi, roma))
{
	jrKanjiStatusWithValue ksv;
	jrKanjiStatus ks;

	CHECK_STRING(yomi);
#ifndef CANNA_MULE
	strncpy(key_buffer, XSTRING_DATA(yomi), XSTRING_LENGTH(yomi));
	ks.length = XSTRING_LENGTH(yomi);
	key_buffer[ks.length] = '\0';
#else				/* CANNA_MULE */
	m2c(XSTRING_DATA(yomi), XSTRING_LENGTH(yomi), key_buffer);
	ks.length = strlen(key_buffer);
#endif				/* CANNA_MULE */

	if (NILP(roma)) {
		ks.mode = 0;
	} else {
		CHECK_STRING(roma);

#ifndef CANNA_MULE
		strncpy(key_buffer + XSTRING_LENGTH(yomi) + 1,
			XSTRING_DATA(roma), XSTRING_LENGTH(roma));
		key_buffer[XSTRING_LENGTH(yomi) + 1 + XSTRING_LENGTH(roma)] =
		    '\0';
		ks.mode =
		    (unsigned char *)(key_buffer + XSTRING_LENGTH(yomi) + 1);
#else				/* CANNA_MULE */
		ks.mode = (unsigned char *)(key_buffer + ks.length + 1);
		m2c(XSTRING_DATA(roma), XSTRING_LENGTH(roma), ks.mode);
#endif				/* CANNA_MULE */
	}

	ks.echoStr = (unsigned char *)key_buffer;
	ksv.buffer = (unsigned char *)key_buffer;	/* 返値用 (return value) */
	ksv.bytes_buffer = KEYTOSTRSIZE;
	ksv.ks = &ks;

	jrKanjiControl(0, KC_STOREYOMI, (char *)&ksv);

	return storeResults(key_buffer, ksv.val, ksv.ks);
}

DEFUN("canna-do-function", Fcanna_do_function, 1, 2, 0,	/*
Do specified function at current mode.
*/
      (num, ch))
{
	jrKanjiStatusWithValue ksv;
	jrKanjiStatus ks;
	Lisp_Object val;

	CHECK_INT(num);

	if (NILP(ch)) {
		*key_buffer = '@';
	} else {
		CHECK_CHAR(ch);
		*key_buffer = XCHAR(ch);
	}

	ksv.buffer = (unsigned char *)key_buffer;
	ksv.bytes_buffer = KEYTOSTRSIZE;
	ksv.ks = &ks;
	ksv.val = XINT(num);
	jrKanjiControl(0, KC_DO, (char *)&ksv);
	val = storeResults(key_buffer, ksv.val, ksv.ks);
	return val;
}

DEFUN("canna-parse", Fcanna_parse, 1, 1, 0,	/*
Parse customize string.
*/
      (str))
{
	Lisp_Object val;
	unsigned char **p;
	int n;

	CHECK_STRING(str);

#ifndef CANNA_MULE
	strncpy(key_buffer, XSTRING_DATA(str), XSTRING_LENGTH(str));
	key_buffer[XSTRING_LENGTH(str)] = '\0';
#else				/* CANNA_MULE */
	m2c(XSTRING_DATA(str), XSTRING_LENGTH(str), key_buffer);
#endif				/* CANNA_MULE */
	p = (unsigned char **)key_buffer;
	n = jrKanjiControl(0, KC_PARSE, (char *)&p);
	val = Qnil;
	while (n > 0) {
		n--;
		val = Fcons(make_string(p[n], strlen(p[n])), val);
	}
	return val;
}

DEFUN("canna-query-mode", Fcanna_query_mode, 0, 0, 0,	/*
Get current mode string.
*/
      ())
{
	unsigned char buf[256];

	jrKanjiControl(0, KC_QUERYMODE, buf);
	return make_string(buf, strlen(buf));
}

/*
 * Functions following this line are for KKCP interface compatible
 * library.  These functions may be used by MILK system.
 */

#define RKBUFSIZE 1024

static unsigned char yomibuf[RKBUFSIZE];
static short kugiri[RKBUFSIZE / 2];

static int confirmContext(void)
{
	if (IRCP_context < 0) {
		int context;

		if ((context =
		     jrKanjiControl(0, KC_GETCONTEXT, (char *)0)) == -1) {
			return 0;
		}
		IRCP_context = context;
	}
	return 1;
}

static int byteLen(int bun, int len)
{
	int i = 0, offset = 0, ch;

	if (0 <= bun && bun < RKBUFSIZE) {
		offset = kugiri[bun];
	}

	while (len-- > 0 && (ch = (int)yomibuf[offset + i])) {
		i++;
		if (ch & 0x80) {
			i++;
		}
	}
	return i;
}

DEFUN("canna-henkan-begin", Fcanna_henkan_begin, 1, 1, 0,	/*
Return the result of kana-to-kanji conversion.
Clause separator is set.
*/
      (yomi))
{
	int nbun;

	CHECK_STRING(yomi);
	if (confirmContext() == 0) {
		return Qnil;
	}
#ifndef CANNA_MULE
	strncpy(yomibuf, XSTRING_DATA(yomi), XSTRING_LENGTH(yomi));
	yomibuf[XSTRING_LENGTH(yomi)] = '\0';
	nbun = RkBgnBun(IRCP_context, XSTRING_DATA(yomi), XSTRING_LENGTH(yomi),
			(RK_XFER << RK_XFERBITS) | RK_KFER);
#else				/* CANNA_MULE */
	m2c(XSTRING_DATA(yomi), XSTRING_LENGTH(yomi), yomibuf);
	nbun = RkBgnBun(IRCP_context, (char *)yomibuf, strlen(yomibuf),
			(RK_XFER << RK_XFERBITS) | RK_KFER);
#endif				/* CANNA_MULE */

	return kanjiYomiList(IRCP_context, nbun);
}

static Lisp_Object kanjiYomiList(int context, int nbun)
{
	Lisp_Object val, res = Qnil;
	unsigned char RkBuf[RKBUFSIZE];
	int len, i, total;

	for (i = nbun; i > 0;) {
		i--;
		RkGoTo(context, i);
		len = RkGetKanji(context, RkBuf, RKBUFSIZE);
		val = make_string(RkBuf, len);
		len = RkGetYomi(context, RkBuf, RKBUFSIZE);
		res = Fcons(Fcons(val, make_string(RkBuf, len)), res);
		if (i < RKBUFSIZE / 2) {
			kugiri[i] = len;
		}
	}
	for (i = 0, total = 0; i < nbun; i++) {
		int temp = kugiri[i];
		kugiri[i] = total;
		total += temp;
	}
	return res;
}

DEFUN("canna-henkan-next", Fcanna_henkan_next, 1, 1, 0,	/*
Return the list of candidates.
*/
      (bunsetsu))
{
	int i, slen, len;
	unsigned char *p, RkBuf[RKBUFSIZE];
	Lisp_Object res = Qnil, endp;

	CHECK_INT(bunsetsu);
	if (confirmContext() == 0) {
		return Qnil;
	}
	RkGoTo(IRCP_context, XINT(bunsetsu));
	len = RkGetKanjiList(IRCP_context, RkBuf, RKBUFSIZE);
	p = RkBuf;
	for (i = 0; i < len; i++) {
		slen = strlen(p);
		if (NILP(res)) {
			endp = res = Fcons(make_string(p, slen), Qnil);
		} else {
			endp = XCDR(res) = Fcons(make_string(p, slen), Qnil);
		}
		p += slen + 1;
	}
	return res;
}

DEFUN("canna-bunsetu-henkou", Fcanna_bunsetu_henkou, 2, 2, 0,	/*
Specify the length of a clause.
*/
      (bunsetsu, bunlen))
{
	int nbun, len;

	CHECK_INT(bunsetsu);
	CHECK_INT(bunlen);

	nbun = XINT(bunsetsu);
	if (confirmContext() == 0) {
		return Qnil;
	}
	RkGoTo(IRCP_context, nbun);
	len = byteLen(nbun, XINT(bunlen));
	return kanjiYomiList(IRCP_context, RkResize(IRCP_context, len));
}

DEFUN("canna-henkan-kakutei", Fcanna_henkan_kakutei, 2, 2, 0,	/*
Select a candidate.
*/
      (bun, kouho))
{
	int nbun, nkouho;

	if (confirmContext() == 0) {
		return Qnil;
	}
	nbun = XINT(bun);
	RkGoTo(IRCP_context, nbun);

	nkouho = XINT(kouho);
	RkXfer(IRCP_context, nkouho);
	return Qt;
}

DEFUN("canna-henkan-end", Fcanna_henkan_end, 0, 0, 0,	/*
End conversion.
*/
      ())
{
	if (confirmContext() == 0) {
		return Qnil;
	}
	RkEndBun(IRCP_context, 1);	/* 学習はいつでも行って良いものなのか？
					   (is it OK to invoke learning function
					   at arbitrary times?) */
	return Qt;
}

DEFUN("canna-henkan-quit", Fcanna_henkan_quit, 0, 0, 0,	/*
Quit conversion.
*/
      ())
{
	if (confirmContext() == 0) {
		return Qnil;
	}
	RkEndBun(IRCP_context, 0);
	return Qt;
}

/* variables below this line is constants of Canna */

static Fixnum canna_mode_AlphaMode;
static Fixnum canna_mode_EmptyMode;
static Fixnum canna_mode_KigoMode;
static Fixnum canna_mode_YomiMode;
static Fixnum canna_mode_JishuMode;
static Fixnum canna_mode_TankouhoMode;
static Fixnum canna_mode_IchiranMode;
static Fixnum canna_mode_YesNoMode;
static Fixnum canna_mode_OnOffMode;
#ifdef CANNA_MODE_AdjustBunsetsuMode
static Fixnum canna_mode_AdjustBunsetsuMode;
#endif
#ifdef CANNA_MODE_ChikujiYomiMode
static Fixnum canna_mode_ChikujiYomiMode;
static Fixnum canna_mode_ChikujiTanMode;
#endif

static Fixnum canna_mode_HenkanMode;
#ifdef CANNA_MODE_HenkanNyuryokuMode
static Fixnum canna_mode_HenkanNyuryokuMode;
#endif
#ifdef CANNA_MODE_ZenHiraHenkanMode
static Fixnum canna_mode_ZenHiraHenkanMode;
#ifdef CANNA_MODE_HanHiraHenkanMode
static Fixnum canna_mode_HanHiraHenkanMode;
#endif
static Fixnum canna_mode_ZenKataHenkanMode;
static Fixnum canna_mode_HanKataHenkanMode;
static Fixnum canna_mode_ZenAlphaHenkanMode;
static Fixnum canna_mode_HanAlphaHenkanMode;
#endif
static Fixnum canna_mode_ZenHiraKakuteiMode;
#ifdef CANNA_MODE_HanHiraKakuteiMode
static Fixnum canna_mode_HanHiraKakuteiMode;
#endif
static Fixnum canna_mode_ZenKataKakuteiMode;
static Fixnum canna_mode_HanKataKakuteiMode;
static Fixnum canna_mode_ZenAlphaKakuteiMode;
static Fixnum canna_mode_HanAlphaKakuteiMode;
static Fixnum canna_mode_HexMode;
static Fixnum canna_mode_BushuMode;
static Fixnum canna_mode_ExtendMode;
static Fixnum canna_mode_RussianMode;
static Fixnum canna_mode_GreekMode;
static Fixnum canna_mode_LineMode;
static Fixnum canna_mode_ChangingServerMode;
static Fixnum canna_mode_HenkanMethodMode;
static Fixnum canna_mode_DeleteDicMode;
static Fixnum canna_mode_TourokuMode;
static Fixnum canna_mode_TourokuEmptyMode;
static Fixnum canna_mode_TourokuHinshiMode;
static Fixnum canna_mode_TourokuDicMode;
static Fixnum canna_mode_QuotedInsertMode;
static Fixnum canna_mode_BubunMuhenkanMode;
static Fixnum canna_mode_MountDicMode;

static Fixnum canna_fn_SelfInsert;
static Fixnum canna_fn_FunctionalInsert;
static Fixnum canna_fn_QuotedInsert;
static Fixnum canna_fn_JapaneseMode;
static Fixnum canna_fn_AlphaMode;
static Fixnum canna_fn_HenkanNyuryokuMode;
static Fixnum canna_fn_Forward;
static Fixnum canna_fn_Backward;
static Fixnum canna_fn_Next;
static Fixnum canna_fn_Prev;
static Fixnum canna_fn_BeginningOfLine;
static Fixnum canna_fn_EndOfLine;
static Fixnum canna_fn_DeleteNext;
static Fixnum canna_fn_DeletePrevious;
static Fixnum canna_fn_KillToEndOfLine;
static Fixnum canna_fn_Henkan;
static Fixnum canna_fn_Kakutei;
static Fixnum canna_fn_Extend;
static Fixnum canna_fn_Shrink;
#ifdef CANNA_FN_AdjustBunsetsu
static Fixnum canna_fn_AdjustBunsetsu;
#endif
static Fixnum canna_fn_Quit;
static Fixnum canna_fn_ConvertAsHex;
static Fixnum canna_fn_ConvertAsBushu;
static Fixnum canna_fn_KouhoIchiran;
static Fixnum canna_fn_BubunMuhenkan;
static Fixnum canna_fn_Zenkaku;
static Fixnum canna_fn_Hankaku;
static Fixnum canna_fn_ToUpper;
static Fixnum canna_fn_Capitalize;
static Fixnum canna_fn_ToLower;
static Fixnum canna_fn_Hiragana;
static Fixnum canna_fn_Katakana;
static Fixnum canna_fn_Romaji;
#ifdef CANNA_FN_BaseHiragana
static Fixnum canna_fn_BaseHiragana;
static Fixnum canna_fn_BaseKatakana;
static Fixnum canna_fn_BaseEisu;
static Fixnum canna_fn_BaseZenkaku;
static Fixnum canna_fn_BaseHankaku;
static Fixnum canna_fn_BaseKana;
static Fixnum canna_fn_BaseKakutei;
static Fixnum canna_fn_BaseHenkan;
static Fixnum canna_fn_BaseHiraKataToggle;
static Fixnum canna_fn_BaseZenHanToggle;
static Fixnum canna_fn_BaseKanaEisuToggle;
static Fixnum canna_fn_BaseKakuteiHenkanToggle;
static Fixnum canna_fn_BaseRotateForward;
static Fixnum canna_fn_BaseRotateBackward;
#endif
static Fixnum canna_fn_ExtendMode;
static Fixnum canna_fn_Touroku;
static Fixnum canna_fn_HexMode;
static Fixnum canna_fn_BushuMode;
static Fixnum canna_fn_KigouMode;
#ifdef CANNA_FN_Mark
static Fixnum canna_fn_Mark;
#endif
#ifdef CANNA_FN_TemporalMode
static Fixnum canna_fn_TemporalMode;
#endif

static Fixnum canna_key_Nfer;
static Fixnum canna_key_Xfer;
static Fixnum canna_key_Up;
static Fixnum canna_key_Left;
static Fixnum canna_key_Right;
static Fixnum canna_key_Down;
static Fixnum canna_key_Insert;
static Fixnum canna_key_Rollup;
static Fixnum canna_key_Rolldown;
static Fixnum canna_key_Home;
static Fixnum canna_key_Help;
static Fixnum canna_key_KP_Key;
static Fixnum canna_key_Shift_Nfer;
static Fixnum canna_key_Shift_Xfer;
static Fixnum canna_key_Shift_Up;
static Fixnum canna_key_Shift_Left;
static Fixnum canna_key_Shift_Right;
static Fixnum canna_key_Shift_Down;
static Fixnum canna_key_Cntrl_Nfer;
static Fixnum canna_key_Cntrl_Xfer;
static Fixnum canna_key_Cntrl_Up;
static Fixnum canna_key_Cntrl_Left;
static Fixnum canna_key_Cntrl_Right;
static Fixnum canna_key_Cntrl_Down;

Lisp_Object VCANNA;		/* by MORIOKA Tomohiko <morioka@jaist.ac.jp>
				   1996/6/7 */

void syms_of_mule_canna(void)
{
	DEFSUBR(Fcanna_key_proc);
	DEFSUBR(Fcanna_initialize);
	DEFSUBR(Fcanna_finalize);
	DEFSUBR(Fcanna_touroku_string);
	DEFSUBR(Fcanna_set_width);
	DEFSUBR(Fcanna_change_mode);
	DEFSUBR(Fcanna_store_yomi);
	DEFSUBR(Fcanna_do_function);
	DEFSUBR(Fcanna_parse);
	DEFSUBR(Fcanna_query_mode);
	DEFSUBR(Fcanna_set_bunsetsu);

	DEFSUBR(Fcanna_henkan_begin);
	DEFSUBR(Fcanna_henkan_next);
	DEFSUBR(Fcanna_bunsetu_henkou);
	DEFSUBR(Fcanna_henkan_kakutei);
	DEFSUBR(Fcanna_henkan_end);
	DEFSUBR(Fcanna_henkan_quit);
}

void vars_of_mule_canna(void)
{
	DEFVAR_LISP("CANNA", &VCANNA);	/* hir@nec, 1992.5.21 */
	VCANNA = Qt;		/* hir@nec, 1992.5.21 */

	DEFVAR_LISP("canna-kakutei-string", &Vcanna_kakutei_string	/*

									 */ );
	Vcanna_kakutei_string = Qnil;

	DEFVAR_LISP("canna-kakutei-yomi", &Vcanna_kakutei_yomi	/*

								 */ );
	Vcanna_kakutei_yomi = Qnil;

	DEFVAR_LISP("canna-kakutei-romaji", &Vcanna_kakutei_romaji	/*

									 */ );
	Vcanna_kakutei_romaji = Qnil;

	DEFVAR_LISP("canna-henkan-string", &Vcanna_henkan_string	/*

									 */ );
	Vcanna_henkan_string = Qnil;

	DEFVAR_INT("canna-henkan-length", &canna_henkan_length	/*

								 */ );
	canna_henkan_length = 0;

	DEFVAR_INT("canna-henkan-revpos", &canna_henkan_revPos	/*

								 */ );
	canna_henkan_revPos = 0;

	DEFVAR_INT("canna-henkan-revlen", &canna_henkan_revLen	/*

								 */ );
	canna_henkan_revLen = 0;

	DEFVAR_LISP("canna-ichiran-string", &Vcanna_ichiran_string	/*

									 */ );
	Vcanna_ichiran_string = Qnil;

	DEFVAR_INT("canna-ichiran-length", &canna_ichiran_length	/*

									 */ );
	canna_ichiran_length = 0;

	DEFVAR_INT("canna-ichiran-revpos", &canna_ichiran_revPos	/*

									 */ );
	canna_ichiran_revPos = 0;

	DEFVAR_INT("canna-ichiran-revlen", &canna_ichiran_revLen	/*

									 */ );
	canna_ichiran_revLen = 0;

	DEFVAR_LISP("canna-mode-string", &Vcanna_mode_string	/*

								 */ );
	Vcanna_mode_string = Qnil;

	DEFVAR_BOOL("canna-empty-info", &canna_empty_info	/*
								   For canna
								 */ );
	canna_empty_info = 0;

	DEFVAR_BOOL("canna-through-info", &canna_through_info	/*
								   For canna
								 */ );
	canna_through_info = 0;

	DEFVAR_BOOL("canna-underline", &canna_underline	/*
							   For canna
							 */ );
	canna_underline = 0;

	DEFVAR_BOOL("canna-inhibit-hankakukana", &canna_inhibit_hankakukana	/*
										   For canna
										 */ );
	canna_inhibit_hankakukana = 0;

	DEFVAR_INT("canna-mode-alpha-mode", &canna_mode_AlphaMode	/*

									 */ );
	canna_mode_AlphaMode = IROHA_MODE_AlphaMode;

	DEFVAR_INT("canna-mode-empty-mode", &canna_mode_EmptyMode	/*

									 */ );
	canna_mode_EmptyMode = IROHA_MODE_EmptyMode;

	DEFVAR_INT("canna-mode-kigo-mode", &canna_mode_KigoMode	/*

								 */ );
	canna_mode_KigoMode = IROHA_MODE_KigoMode;

	DEFVAR_INT("canna-mode-yomi-mode", &canna_mode_YomiMode	/*

								 */ );
	canna_mode_YomiMode = IROHA_MODE_YomiMode;

	DEFVAR_INT("canna-mode-jishu-mode", &canna_mode_JishuMode	/*

									 */ );
	canna_mode_JishuMode = IROHA_MODE_JishuMode;

	DEFVAR_INT("canna-mode-tankouho-mode", &canna_mode_TankouhoMode	/*

									 */ );
	canna_mode_TankouhoMode = IROHA_MODE_TankouhoMode;

	DEFVAR_INT("canna-mode-ichiran-mode", &canna_mode_IchiranMode	/*

									 */ );
	canna_mode_IchiranMode = IROHA_MODE_IchiranMode;

	DEFVAR_INT("canna-mode-yes-no-mode", &canna_mode_YesNoMode	/*

									 */ );
	canna_mode_YesNoMode = IROHA_MODE_YesNoMode;

	DEFVAR_INT("canna-mode-on-off-mode", &canna_mode_OnOffMode	/*

									 */ );
	canna_mode_OnOffMode = IROHA_MODE_OnOffMode;

#ifdef CANNA_MODE_AdjustBunsetsuMode
	DEFVAR_INT("canna-mode-adjust-bunsetsu-mode", &canna_mode_AdjustBunsetsuMode	/*

											 */ );
	canna_mode_AdjustBunsetsuMode = CANNA_MODE_AdjustBunsetsuMode;
#endif
#ifdef CANNA_MODE_ChikujiYomiMode
	DEFVAR_INT("canna-mode-chikuji-yomi-mode", &canna_mode_ChikujiYomiMode	/*

										 */ );
	canna_mode_ChikujiYomiMode = CANNA_MODE_ChikujiYomiMode;

	DEFVAR_INT("canna-mode-chikuji-bunsetsu-mode", &canna_mode_ChikujiTanMode	/*

											 */ );
	canna_mode_ChikujiTanMode = CANNA_MODE_ChikujiTanMode;
#endif

	DEFVAR_INT("canna-mode-henkan-mode", &canna_mode_HenkanMode	/*

									 */ );
	canna_mode_HenkanMode = IROHA_MODE_HenkanMode;

#ifdef CANNA_MODE_HenkanNyuryokuMode
	DEFVAR_INT("canna-mode-henkan-nyuuryoku-mode", &canna_mode_HenkanNyuryokuMode	/*

											 */ );
	canna_mode_HenkanNyuryokuMode = CANNA_MODE_HenkanNyuryokuMode;
#endif
#ifdef CANNA_MODE_ZenHiraHenkanMode
	DEFVAR_INT("canna-mode-zen-hira-henkan-mode", &canna_mode_ZenHiraHenkanMode	/*

											 */ );
	canna_mode_ZenHiraHenkanMode = CANNA_MODE_ZenHiraHenkanMode;
#ifdef CANNA_MODE_HanHiraHenkanMode
	DEFVAR_INT("canna-mode-han-hira-henkan-mode", &canna_mode_HanHiraHenkanMode	/*

											 */ );
	canna_mode_HanHiraHenkanMode = CANNA_MODE_HanHiraHenkanMode;
#endif
	DEFVAR_INT("canna-mode-zen-kata-henkan-mode", &canna_mode_ZenKataHenkanMode	/*

											 */ );
	canna_mode_ZenKataHenkanMode = CANNA_MODE_ZenKataHenkanMode;

	DEFVAR_INT("canna-mode-han-kata-henkan-mode", &canna_mode_HanKataHenkanMode	/*

											 */ );
	canna_mode_HanKataHenkanMode = CANNA_MODE_HanKataHenkanMode;

	DEFVAR_INT("canna-mode-zen-alpha-henkan-mode", &canna_mode_ZenAlphaHenkanMode	/*

											 */ );
	canna_mode_ZenAlphaHenkanMode = CANNA_MODE_ZenAlphaHenkanMode;

	DEFVAR_INT("canna-mode-han-alpha-henkan-mode", &canna_mode_HanAlphaHenkanMode	/*

											 */ );
	canna_mode_HanAlphaHenkanMode = CANNA_MODE_HanAlphaHenkanMode;
#endif
	DEFVAR_INT("canna-mode-zen-hira-kakutei-mode", &canna_mode_ZenHiraKakuteiMode	/*

											 */ );
	canna_mode_ZenHiraKakuteiMode = IROHA_MODE_ZenHiraKakuteiMode;
#ifdef CANNA_MODE_HanHiraKakuteiMode
	DEFVAR_INT("canna-mode-han-hira-kakutei-mode", &canna_mode_HanHiraKakuteiMode	/*

											 */ );
	canna_mode_HanHiraKakuteiMode = CANNA_MODE_HanHiraKakuteiMode;
#endif
	DEFVAR_INT("canna-mode-zen-kata-kakutei-mode", &canna_mode_ZenKataKakuteiMode	/*

											 */ );
	canna_mode_ZenKataKakuteiMode = IROHA_MODE_ZenKataKakuteiMode;

	DEFVAR_INT("canna-mode-han-kata-kakutei-mode", &canna_mode_HanKataKakuteiMode	/*

											 */ );
	canna_mode_HanKataKakuteiMode = IROHA_MODE_HanKataKakuteiMode;

	DEFVAR_INT("canna-mode-zen-alpha-kakutei-mode", &canna_mode_ZenAlphaKakuteiMode	/*

											 */ );
	canna_mode_ZenAlphaKakuteiMode = IROHA_MODE_ZenAlphaKakuteiMode;

	DEFVAR_INT("canna-mode-han-alpha-kakutei-mode", &canna_mode_HanAlphaKakuteiMode	/*

											 */ );
	canna_mode_HanAlphaKakuteiMode = IROHA_MODE_HanAlphaKakuteiMode;

	DEFVAR_INT("canna-mode-hex-mode", &canna_mode_HexMode	/*

								 */ );
	canna_mode_HexMode = IROHA_MODE_HexMode;

	DEFVAR_INT("canna-mode-bushu-mode", &canna_mode_BushuMode	/*

									 */ );
	canna_mode_BushuMode = IROHA_MODE_BushuMode;

	DEFVAR_INT("canna-mode-extend-mode", &canna_mode_ExtendMode	/*

									 */ );
	canna_mode_ExtendMode = IROHA_MODE_ExtendMode;

	DEFVAR_INT("canna-mode-russian-mode", &canna_mode_RussianMode	/*

									 */ );
	canna_mode_RussianMode = IROHA_MODE_RussianMode;

	DEFVAR_INT("canna-mode-greek-mode", &canna_mode_GreekMode	/*

									 */ );
	canna_mode_GreekMode = IROHA_MODE_GreekMode;

	DEFVAR_INT("canna-mode-line-mode", &canna_mode_LineMode	/*

								 */ );
	canna_mode_LineMode = IROHA_MODE_LineMode;

	DEFVAR_INT("canna-mode-changing-server-mode", &canna_mode_ChangingServerMode	/*

											 */ );
	canna_mode_ChangingServerMode = IROHA_MODE_ChangingServerMode;

	DEFVAR_INT("canna-mode-henkan-method-mode", &canna_mode_HenkanMethodMode	/*

											 */ );
	canna_mode_HenkanMethodMode = IROHA_MODE_HenkanMethodMode;

	DEFVAR_INT("canna-mode-delete-dic-mode", &canna_mode_DeleteDicMode	/*

										 */ );
	canna_mode_DeleteDicMode = IROHA_MODE_DeleteDicMode;

	DEFVAR_INT("canna-mode-touroku-mode", &canna_mode_TourokuMode	/*

									 */ );
	canna_mode_TourokuMode = IROHA_MODE_TourokuMode;

	DEFVAR_INT("canna-mode-touroku-empty-mode", &canna_mode_TourokuEmptyMode	/*

											 */ );
	canna_mode_TourokuEmptyMode = IROHA_MODE_TourokuEmptyMode;

	DEFVAR_INT("canna-mode-touroku-hinshi-mode", &canna_mode_TourokuHinshiMode	/*

											 */ );
	canna_mode_TourokuHinshiMode = IROHA_MODE_TourokuHinshiMode;

	DEFVAR_INT("canna-mode-touroku-dic-mode", &canna_mode_TourokuDicMode	/*

										 */ );
	canna_mode_TourokuDicMode = IROHA_MODE_TourokuDicMode;

	DEFVAR_INT("canna-mode-quoted-insert-mode", &canna_mode_QuotedInsertMode	/*

											 */ );
	canna_mode_QuotedInsertMode = IROHA_MODE_QuotedInsertMode;

	DEFVAR_INT("canna-mode-bubun-muhenkan-mode", &canna_mode_BubunMuhenkanMode	/*

											 */ );
	canna_mode_BubunMuhenkanMode = IROHA_MODE_BubunMuhenkanMode;

	DEFVAR_INT("canna-mode-mount-dic-mode", &canna_mode_MountDicMode	/*

										 */ );
	canna_mode_MountDicMode = IROHA_MODE_MountDicMode;

	DEFVAR_INT("canna-func-self-insert", &canna_fn_SelfInsert	/*

									 */ );
	canna_fn_SelfInsert = IROHA_FN_SelfInsert;

	DEFVAR_INT("canna-func-functional-insert", &canna_fn_FunctionalInsert	/*

										 */ );
	canna_fn_FunctionalInsert = IROHA_FN_FunctionalInsert;

	DEFVAR_INT("canna-func-quoted-insert", &canna_fn_QuotedInsert	/*

									 */ );
	canna_fn_QuotedInsert = IROHA_FN_QuotedInsert;

	DEFVAR_INT("canna-func-japanese-mode", &canna_fn_JapaneseMode	/*

									 */ );
	canna_fn_JapaneseMode = IROHA_FN_JapaneseMode;

	DEFVAR_INT("canna-func-alpha-mode", &canna_fn_AlphaMode	/*

								 */ );
	canna_fn_AlphaMode = IROHA_FN_AlphaMode;

	DEFVAR_INT("canna-func-henkan-nyuryoku-mode", &canna_fn_HenkanNyuryokuMode	/*

											 */ );
	canna_fn_HenkanNyuryokuMode = IROHA_FN_HenkanNyuryokuMode;

	DEFVAR_INT("canna-func-forward", &canna_fn_Forward	/*

								 */ );
	canna_fn_Forward = IROHA_FN_Forward;

	DEFVAR_INT("canna-func-backward", &canna_fn_Backward	/*

								 */ );
	canna_fn_Backward = IROHA_FN_Backward;

	DEFVAR_INT("canna-func-next", &canna_fn_Next	/*

							 */ );
	canna_fn_Next = IROHA_FN_Next;

	DEFVAR_INT("canna-func-previous", &canna_fn_Prev	/*

								 */ );
	canna_fn_Prev = IROHA_FN_Prev;

	DEFVAR_INT("canna-func-beginning-of-line", &canna_fn_BeginningOfLine	/*

										 */ );
	canna_fn_BeginningOfLine = IROHA_FN_BeginningOfLine;

	DEFVAR_INT("canna-func-end-of-line", &canna_fn_EndOfLine	/*

									 */ );
	canna_fn_EndOfLine = IROHA_FN_EndOfLine;

	DEFVAR_INT("canna-func-delete-next", &canna_fn_DeleteNext	/*

									 */ );
	canna_fn_DeleteNext = IROHA_FN_DeleteNext;

	DEFVAR_INT("canna-func-delete-previous", &canna_fn_DeletePrevious	/*

										 */ );
	canna_fn_DeletePrevious = IROHA_FN_DeletePrevious;

	DEFVAR_INT("canna-func-kill-to-end-of-line", &canna_fn_KillToEndOfLine	/*

										 */ );
	canna_fn_KillToEndOfLine = IROHA_FN_KillToEndOfLine;

	DEFVAR_INT("canna-func-henkan", &canna_fn_Henkan	/*

								 */ );
	canna_fn_Henkan = IROHA_FN_Henkan;

	DEFVAR_INT("canna-func-kakutei", &canna_fn_Kakutei	/*

								 */ );
	canna_fn_Kakutei = IROHA_FN_Kakutei;

	DEFVAR_INT("canna-func-extend", &canna_fn_Extend	/*

								 */ );
	canna_fn_Extend = IROHA_FN_Extend;

	DEFVAR_INT("canna-func-shrink", &canna_fn_Shrink	/*

								 */ );
	canna_fn_Shrink = IROHA_FN_Shrink;

#ifdef CANNA_FN_AdjustBunsetsu
	DEFVAR_INT("canna-func-adjust-bunsetsu", &canna_fn_AdjustBunsetsu	/*

										 */ );
	canna_fn_AdjustBunsetsu = CANNA_FN_AdjustBunsetsu;
#endif
	DEFVAR_INT("canna-func-quit", &canna_fn_Quit	/*

							 */ );
	canna_fn_Quit = IROHA_FN_Quit;

	DEFVAR_INT("canna-func-convert-as-hex", &canna_fn_ConvertAsHex	/*

									 */ );
	canna_fn_ConvertAsHex = IROHA_FN_ConvertAsHex;

	DEFVAR_INT("canna-func-convert-as-bushu", &canna_fn_ConvertAsBushu	/*

										 */ );
	canna_fn_ConvertAsBushu = IROHA_FN_ConvertAsBushu;

	DEFVAR_INT("canna-func-kouho-ichiran", &canna_fn_KouhoIchiran	/*

									 */ );
	canna_fn_KouhoIchiran = IROHA_FN_KouhoIchiran;

	DEFVAR_INT("canna-func-bubun-muhenkan", &canna_fn_BubunMuhenkan	/*

									 */ );
	canna_fn_BubunMuhenkan = IROHA_FN_BubunMuhenkan;

	DEFVAR_INT("canna-func-zenkaku", &canna_fn_Zenkaku	/*

								 */ );
	canna_fn_Zenkaku = IROHA_FN_Zenkaku;

	DEFVAR_INT("canna-func-hankaku", &canna_fn_Hankaku	/*

								 */ );
	canna_fn_Hankaku = IROHA_FN_Hankaku;

	DEFVAR_INT("canna-func-to-upper", &canna_fn_ToUpper	/*

								 */ );
	canna_fn_ToUpper = IROHA_FN_ToUpper;

	DEFVAR_INT("canna-func-capitalize", &canna_fn_Capitalize	/*

									 */ );
	canna_fn_Capitalize = IROHA_FN_Capitalize;

	DEFVAR_INT("canna-func-to-lower", &canna_fn_ToLower	/*

								 */ );
	canna_fn_ToLower = IROHA_FN_ToLower;

	DEFVAR_INT("canna-func-hiragana", &canna_fn_Hiragana	/*

								 */ );
	canna_fn_Hiragana = IROHA_FN_Hiragana;

	DEFVAR_INT("canna-func-katakana", &canna_fn_Katakana	/*

								 */ );
	canna_fn_Katakana = IROHA_FN_Katakana;

	DEFVAR_INT("canna-func-romaji", &canna_fn_Romaji	/*

								 */ );
	canna_fn_Romaji = IROHA_FN_Romaji;

#ifdef CANNA_FN_BaseHiragana
	DEFVAR_INT("canna-func-base-hiragana", &canna_fn_BaseHiragana	/*

									 */ );
	canna_fn_BaseHiragana = CANNA_FN_BaseHiragana;

	DEFVAR_INT("canna-func-base-katakana", &canna_fn_BaseKatakana	/*

									 */ );
	canna_fn_BaseKatakana = CANNA_FN_BaseKatakana;

	DEFVAR_INT("canna-func-base-eisu", &canna_fn_BaseEisu	/*

								 */ );
	canna_fn_BaseEisu = CANNA_FN_BaseEisu;

	DEFVAR_INT("canna-func-base-zenkaku", &canna_fn_BaseZenkaku	/*

									 */ );
	canna_fn_BaseZenkaku = CANNA_FN_BaseZenkaku;

	DEFVAR_INT("canna-func-base-hankaku", &canna_fn_BaseHankaku	/*

									 */ );
	canna_fn_BaseHankaku = CANNA_FN_BaseHankaku;

	DEFVAR_INT("canna-func-base-kana", &canna_fn_BaseKana	/*

								 */ );
	canna_fn_BaseKana = CANNA_FN_BaseKana;

	DEFVAR_INT("canna-func-base-kakutei", &canna_fn_BaseKakutei	/*

									 */ );
	canna_fn_BaseKakutei = CANNA_FN_BaseKakutei;

	DEFVAR_INT("canna-func-base-henkan", &canna_fn_BaseHenkan	/*

									 */ );
	canna_fn_BaseHenkan = CANNA_FN_BaseHenkan;

	DEFVAR_INT("canna-func-base-hiragana-katakana-toggle", &canna_fn_BaseHiraKataToggle	/*

												 */ );
	canna_fn_BaseHiraKataToggle = CANNA_FN_BaseHiraKataToggle;

	DEFVAR_INT("canna-func-base-zenkaku-hankaku-toggle", &canna_fn_BaseZenHanToggle	/*

											 */ );
	canna_fn_BaseZenHanToggle = CANNA_FN_BaseZenHanToggle;

	DEFVAR_INT("canna-func-base-kana-eisu-toggle", &canna_fn_BaseKanaEisuToggle	/*

											 */ );
	canna_fn_BaseKanaEisuToggle = CANNA_FN_BaseKanaEisuToggle;

	DEFVAR_INT("canna-func-base-kakutei-henkan-toggle", &canna_fn_BaseKakuteiHenkanToggle	/*

												 */ );
	canna_fn_BaseKakuteiHenkanToggle = CANNA_FN_BaseKakuteiHenkanToggle;

	DEFVAR_INT("canna-func-base-rotate-forward", &canna_fn_BaseRotateForward	/*

											 */ );
	canna_fn_BaseRotateForward = CANNA_FN_BaseRotateForward;

	DEFVAR_INT("canna-func-base-rotate-backward", &canna_fn_BaseRotateBackward	/*

											 */ );
	canna_fn_BaseRotateBackward = CANNA_FN_BaseRotateBackward;

#endif
	DEFVAR_INT("canna-func-extend-mode", &canna_fn_ExtendMode	/*

									 */ );
	canna_fn_ExtendMode = IROHA_FN_ExtendMode;

	DEFVAR_INT("canna-func-touroku", &canna_fn_Touroku	/*

								 */ );
	canna_fn_Touroku = IROHA_FN_Touroku;

	DEFVAR_INT("canna-func-hex-mode", &canna_fn_HexMode	/*

								 */ );
	canna_fn_HexMode = IROHA_FN_HexMode;

	DEFVAR_INT("canna-func-bushu-mode", &canna_fn_BushuMode	/*

								 */ );
	canna_fn_BushuMode = IROHA_FN_BushuMode;

	DEFVAR_INT("canna-func-kigo-mode", &canna_fn_KigouMode	/*

								 */ );
	canna_fn_KigouMode = IROHA_FN_KigouMode;

#ifdef CANNA_FN_Mark
	DEFVAR_INT("canna-func-mark", &canna_fn_Mark	/*

							 */ );
	canna_fn_Mark = CANNA_FN_Mark;
#endif
#ifdef CANNA_FN_TemporalMode
	DEFVAR_INT("canna-func-temporal-mode", &canna_fn_TemporalMode	/*

									 */ );
	canna_fn_TemporalMode = CANNA_FN_TemporalMode;
#endif

	DEFVAR_INT("canna-key-nfer", &canna_key_Nfer	/*

							 */ );
	canna_key_Nfer = IROHA_KEY_Nfer;

	DEFVAR_INT("canna-key-xfer", &canna_key_Xfer	/*

							 */ );
	canna_key_Xfer = IROHA_KEY_Xfer;

	DEFVAR_INT("canna-key-up", &canna_key_Up	/*

							 */ );
	canna_key_Up = IROHA_KEY_Up;

	DEFVAR_INT("canna-key-left", &canna_key_Left	/*

							 */ );
	canna_key_Left = IROHA_KEY_Left;

	DEFVAR_INT("canna-key-right", &canna_key_Right	/*

							 */ );
	canna_key_Right = IROHA_KEY_Right;

	DEFVAR_INT("canna-key-down", &canna_key_Down	/*

							 */ );
	canna_key_Down = IROHA_KEY_Down;

	DEFVAR_INT("canna-key-insert", &canna_key_Insert	/*

								 */ );
	canna_key_Insert = IROHA_KEY_Insert;

	DEFVAR_INT("canna-key-rollup", &canna_key_Rollup	/*

								 */ );
	canna_key_Rollup = IROHA_KEY_Rollup;

	DEFVAR_INT("canna-key-rolldown", &canna_key_Rolldown	/*

								 */ );
	canna_key_Rolldown = IROHA_KEY_Rolldown;

	DEFVAR_INT("canna-key-home", &canna_key_Home	/*

							 */ );
	canna_key_Home = IROHA_KEY_Home;

	DEFVAR_INT("canna-key-help", &canna_key_Help	/*

							 */ );
	canna_key_Help = IROHA_KEY_Help;

	DEFVAR_INT("canna-key-kp-key", &canna_key_KP_Key	/*

								 */ );
	canna_key_KP_Key = IROHA_KEY_KP_Key;

	DEFVAR_INT("canna-key-shift-nfer", &canna_key_Shift_Nfer	/*

									 */ );
	canna_key_Shift_Nfer = IROHA_KEY_Shift_Nfer;

	DEFVAR_INT("canna-key-shift-xfer", &canna_key_Shift_Xfer	/*

									 */ );
	canna_key_Shift_Xfer = IROHA_KEY_Shift_Xfer;

	DEFVAR_INT("canna-key-shift-up", &canna_key_Shift_Up	/*

								 */ );
	canna_key_Shift_Up = IROHA_KEY_Shift_Up;

	DEFVAR_INT("canna-key-shift-left", &canna_key_Shift_Left	/*

									 */ );
	canna_key_Shift_Left = IROHA_KEY_Shift_Left;

	DEFVAR_INT("canna-key-shift-right", &canna_key_Shift_Right	/*

									 */ );
	canna_key_Shift_Right = IROHA_KEY_Shift_Right;

	DEFVAR_INT("canna-key-shift-down", &canna_key_Shift_Down	/*

									 */ );
	canna_key_Shift_Down = IROHA_KEY_Shift_Down;

	DEFVAR_INT("canna-key-control-nfer", &canna_key_Cntrl_Nfer	/*

									 */ );
	canna_key_Cntrl_Nfer = IROHA_KEY_Cntrl_Nfer;

	DEFVAR_INT("canna-key-control-xfer", &canna_key_Cntrl_Xfer	/*

									 */ );
	canna_key_Cntrl_Xfer = IROHA_KEY_Cntrl_Xfer;

	DEFVAR_INT("canna-key-control-up", &canna_key_Cntrl_Up	/*

								 */ );
	canna_key_Cntrl_Up = IROHA_KEY_Cntrl_Up;

	DEFVAR_INT("canna-key-control-left", &canna_key_Cntrl_Left	/*

									 */ );
	canna_key_Cntrl_Left = IROHA_KEY_Cntrl_Left;

	DEFVAR_INT("canna-key-control-right", &canna_key_Cntrl_Right	/*

									 */ );
	canna_key_Cntrl_Right = IROHA_KEY_Cntrl_Right;

	DEFVAR_INT("canna-key-control-down", &canna_key_Cntrl_Down	/*

									 */ );
	canna_key_Cntrl_Down = IROHA_KEY_Cntrl_Down;

	Fprovide(intern("CANNA"));
}

#ifdef CANNA_MULE
/* To handle MULE internal code and EUC.
   I assume CANNA can handle only Japanese EUC. */

/* EUC multibyte string to MULE internal string */

static void c2mu(unsigned char *cp, int l, unsigned char *mp)
{
	unsigned char ch, *ep = cp + l;

	while ((cp < ep) && (ch = *cp)) {
		if ((unsigned char)ch == ISO_CODE_SS2) {
			*mp++ = LEADING_BYTE_KATAKANA_JISX0201;
			cp++;
		} else if ((unsigned char)ch == ISO_CODE_SS3) {
			*mp++ = LEADING_BYTE_JAPANESE_JISX0212;
			cp++;
			*mp++ = *cp++;
		} else if (ch & 0x80) {
			*mp++ = LEADING_BYTE_JAPANESE_JISX0208;
			*mp++ = *cp++;
		}
		*mp++ = *cp++;
	}
	*mp = 0;
}

/* MULE internal string to EUC multibyte string */

static void m2c(unsigned char *mp, int l, unsigned char *cp)
{
	unsigned char ch, *ep = mp + l;

	while ((mp < ep) && (ch = *mp++)) {
		switch (ch) {
		case LEADING_BYTE_KATAKANA_JISX0201:
			*cp++ = ISO_CODE_SS2;
			*cp++ = *mp++;
			break;
		case LEADING_BYTE_JAPANESE_JISX0212:
			*cp++ = ISO_CODE_SS3;
		case LEADING_BYTE_JAPANESE_JISX0208:
			*cp++ = *mp++;
			*cp++ = *mp++;
			break;
		default:
			*cp++ = ch;
			break;
		}
	}
	*cp = 0;
}

#undef make_string

/* make_string after converting EUC string to MULE internal string */
static Lisp_Object mule_make_string(unsigned char *p, int l)
{
	unsigned char cbuf[4096];

	c2mu(p, l, cbuf);
	return (make_string(cbuf, strlen(cbuf)));
}

/* return the MULE internal string length of EUC string */
/* Modified by sb to return a character count not byte count. */
static int mule_strlen(unsigned char *p, int l)
{
	unsigned char ch, *cp = p;
	int len = 0;

	while ((cp < p + l) && (ch = *cp)) {
		if ((unsigned char)ch == ISO_CODE_SS2) {
			len++;
			cp += 2;
		} else if ((unsigned char)ch == ISO_CODE_SS3) {
			len++;
			cp += 3;
		} else if (ch & 0x80) {
			len++;
			cp += 2;
		} else {
			len++;
			cp++;
		}
	}
	return (len);
}

/* count number of characters */
static void
count_char(unsigned char *p, int len, int pos, int rev,
	   Fixnum * clen, Fixnum * cpos, Fixnum * crev)
{
	unsigned char *q = p;

	*clen = *cpos = *crev = 0;
	if (len == 0)
		return;
	while (q < p + pos) {
		(*clen)++;
		(*cpos)++;
		if (*q++ & 0x80)
			q++;
	}
	while (q < p + pos + rev) {
		(*clen)++;
		(*crev)++;
		if (*q++ & 0x80)
			q++;
	}
	while (q < p + len) {
		(*clen)++;
		if (*q++ & 0x80)
			q++;
	}
}
#endif				/* CANNA_MULE */
