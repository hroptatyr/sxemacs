/* Copyright (C) 2000 Free Software Foundation, Inc.
   Copyright (C) 2000 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Authorship:

   Created May 2000 by Andy Piper.
   Windows-Mule stuff added by Ben Wing.
*/

#ifndef INCLUDED_syswindows_h_
#define INCLUDED_syswindows_h_

/* Note that there are currently FOUR different general
   Windows-related include files in src!

   Uses are approximately:

   syswindows.h: Mostly a wrapper around <windows.h>, including missing
   defines as necessary.  Also includes stuff needed on both Cygwin and
   native Windows, regardless of window system chosen.

   console-msw.h: Used on both Cygwin and native Windows, but only when
   native window system (as opposed to X) chosen.

   nt.h: [will be renamed to win32.h] Used only on native Windows, and
   regardless of window system chosen -- but used on both purely native
   Windows (s/windowsnt.h) and MinGW (s/mingw32.h).

   ntheap.h: Used only on native Windows and only when standard dumping
   mechanism (unexnt.c) used.

   All of the last three files include the first.
*/

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#include <windows.h>

#if defined (CYGWIN) && CYGWIN_VERSION_DLL_MAJOR < 21
extern BOOL WINAPI DdeFreeStringHandle(DWORD,HSZ);
extern BOOL WINAPI PlaySound(LPCSTR,HMODULE,DWORD);
#define stricmp strcasecmp
#define FONTENUMPROC FONTENUMEXPROC
#define ntmTm ntmentm
#elif defined (WIN32_LEAN_AND_MEAN)
#ifdef HAVE_X_WINDOWS
/* Christ almighty.  The problems you get when combining two large code bases,
   neither with any respect for namespace purity. */
#undef Status
#endif
#include <winspool.h>
#ifdef HAVE_X_WINDOWS
#define Status int
#endif
#include <mmsystem.h>
#include <shlobj.h>
#include <shellapi.h>
#include <ddeml.h>
#endif

/* mmsystem.h defines. */
#ifndef SND_ASYNC
#define SND_ASYNC		1
#endif
#ifndef SND_NODEFAULT
#define SND_NODEFAULT		2
#endif
#ifndef SND_MEMORY
#define SND_MEMORY		4
#endif
#ifndef SND_FILENAME
#define SND_FILENAME		0x2000L
#endif

/* winspool.h defines. */
#ifndef PHYSICALWIDTH
#define PHYSICALWIDTH 110
#endif
#ifndef PHYSICALHEIGHT
#define PHYSICALHEIGHT 111
#endif
#ifndef PHYSICALOFFSETX
#define PHYSICALOFFSETX 112
#endif
#ifndef PHYSICALOFFSETY
#define PHYSICALOFFSETY 113
#endif

/* shlobj.h defines. */
#ifndef BIF_EDITBOX
#define BIF_EDITBOX 0x10
#endif
#ifndef BIF_VALIDATE
#define BIF_VALIDATE 0x20
#endif
#ifndef BFFM_VALIDATEFAILED
#define BFFM_VALIDATEFAILED 3
#endif

/* windows.h defines. */
#if defined (CYGWIN) && (CYGWIN_VERSION_DLL_MAJOR < 20)
typedef NMHDR *LPNMHDR;
#endif

#ifndef SPI_GETWHEELSCROLLLINES
#define SPI_GETWHEELSCROLLLINES 104
#endif
#ifndef WHEEL_PAGESCROLL
#define WHEEL_PAGESCROLL (UINT_MAX)
#endif
#ifndef WHEEL_DELTA
#define WHEEL_DELTA 120
#endif
#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL 0x20A
#endif
#ifndef VK_APPS
#define VK_APPS			0x5D
#endif
#ifndef SIF_TRACKPOS
#define SIF_TRACKPOS		0x0010
#endif
#ifndef FW_BLACK
#define FW_BLACK	FW_HEAVY
#endif
#ifndef FW_ULTRABOLD
#define FW_ULTRABOLD	FW_EXTRABOLD
#endif
#ifndef FW_DEMIBOLD
#define FW_DEMIBOLD	FW_SEMIBOLD
#endif
#ifndef FW_ULTRALIGHT
#define FW_ULTRALIGHT	FW_EXTRALIGHT
#endif
#ifndef APPCMD_FILTERINITS
#define APPCMD_FILTERINITS	0x20L
#endif
#ifndef CBF_FAIL_SELFCONNECTIONS
#define CBF_FAIL_SELFCONNECTIONS 0x1000
#endif
#ifndef CBF_SKIP_ALLNOTIFICATIONS
#define CBF_SKIP_ALLNOTIFICATIONS	0x3C0000
#endif
#ifndef CBF_FAIL_ADVISES
#define CBF_FAIL_ADVISES	0x4000
#endif
#ifndef CBF_FAIL_POKES
#define CBF_FAIL_POKES		0x10000
#endif
#ifndef CBF_FAIL_REQUESTS
#define CBF_FAIL_REQUESTS	0x20000
#endif
#ifndef SZDDESYS_TOPIC
#define SZDDESYS_TOPIC		"System"
#endif
#ifndef JOHAB_CHARSET
#define JOHAB_CHARSET 		130
#endif
#ifndef MAC_CHARSET
#define MAC_CHARSET 		77
#endif
#ifndef LOCALE_RETURN_NUMBER
#define LOCALE_RETURN_NUMBER	0x20000000
#endif

/***************************************************************/

/* Definitions for Mule under MS Windows */

#include <wchar.h>

#ifdef CYGWIN

/* All but wcscmp and wcslen left out of Cygwin headers -- but present
   in /usr/include/mingw32/string.h! */
wchar_t* wcscat (wchar_t*, const wchar_t*);
wchar_t* wcschr (const wchar_t*, wchar_t);
int	wcscoll (const wchar_t*, const wchar_t*);
wchar_t* wcscpy (wchar_t*, const wchar_t*);
size_t	wcscspn (const wchar_t*, const wchar_t*);
/* Note: No wcserror in CRTDLL. */
wchar_t* wcsncat (wchar_t*, const wchar_t*, size_t);
int	wcsncmp(const wchar_t*, const wchar_t*, size_t);
wchar_t* wcsncpy(wchar_t*, const wchar_t*, size_t);
wchar_t* wcspbrk(const wchar_t*, const wchar_t*);
wchar_t* wcsrchr(const wchar_t*, wchar_t);
size_t	wcsspn(const wchar_t*, const wchar_t*);
wchar_t* wcsstr(const wchar_t*, const wchar_t*);
wchar_t* wcstok(wchar_t*, const wchar_t*);
size_t	wcsxfrm(wchar_t*, const wchar_t*, size_t);

#endif /* CYGWIN */

// extern int mswindows_windows9x_p;
/* #define XEUNICODE_P (!mswindows_windows9x_p) */
#define XEUNICODE_P 0

#define XETCHAR_SIZE (XEUNICODE_P ? sizeof (WCHAR) : sizeof (CHAR))
#define MAX_XETCHAR_SIZE sizeof (WCHAR)
#define XETEXT1(arg) (XEUNICODE_P ? ((char *) (L##arg)) : (arg))
/* We need to do this indirection in case ARG is also a manifest constant.
   I don't really understand why. --ben */
#define XETEXT(arg) XETEXT1(arg)
#define XECOPY_TCHAR(ptr, ch) \
  (XEUNICODE_P ? (* (LPWSTR) (ptr) = L##ch) : (* (LPSTR) (ptr) = (ch)))
#define xetcslen(arg) (XEUNICODE_P ? wcslen ((wchar_t *) arg) : strlen (arg))
#define xetcscmp(s1, s2) \
  (XEUNICODE_P ? wcscmp ((wchar_t *) s1, (wchar_t *) s2) \
   : strcmp (s1, s2))
#define xetcscpy(s1, s2) \
  (XEUNICODE_P ? (char *) wcscpy ((wchar_t *) s1, (wchar_t *) s2) \
   : strcpy (s1, s2))
#define xetcschr(s, ch) \
  (XEUNICODE_P ? (char *) wcschr ((wchar_t *) s, (WCHAR) ch) \
   : strchr (s, ch))
#define xetcsrchr(s, ch) \
  (XEUNICODE_P ? (char *) wcsrchr ((wchar_t *) s, (WCHAR) ch) \
   : strrchr (s, ch))


#define LOCAL_FILE_FORMAT_TO_TSTR(path, out)		\
do {							\
  Bufbyte *lttff;					\
							\
  LOCAL_TO_WIN32_FILE_FORMAT (path, lttff);		\
  C_STRING_TO_EXTERNAL (lttff, out, Qmswindows_tstr);	\
} while (0)

Lisp_Object tstr_to_local_file_format (Extbyte *pathout);

#ifdef CYGWIN
#define LOCAL_TO_WIN32_FILE_FORMAT(path, pathout)			\
do {									\
  Lisp_Object ltwff1 = (path);						\
  Bufbyte* ltwffp = XSTRING_DATA (ltwff1);				\
  if (isalpha (ltwffp[0]) && (IS_DEVICE_SEP (ltwffp[1])))			\
    pathout = ltwffp;							\
  else {									\
    int ltwff2 =								\
      cygwin_posix_to_win32_path_list_buf_size (ltwffp);	\
    pathout = (Bufbyte *) alloca (ltwff2);				\
    cygwin_posix_to_win32_path_list (ltwffp, pathout);	\
  }									\
} while (0)
#else
#define LOCAL_TO_WIN32_FILE_FORMAT(path, pathout)	\
do {							\
  (pathout) = XSTRING_DATA (path);			\
} while (0)
#endif

#ifdef CYGWIN
#define WIN32_TO_LOCAL_FILE_FORMAT(path, pathout)	\
do {							\
  Bufbyte *wtlff1 = (path);				\
  int wtlff2 =						\
    cygwin_win32_to_posix_path_list_buf_size (wtlff1);	\
  Bufbyte *wtlff3 = (Bufbyte *) alloca (wtlff2);	\
  cygwin_win32_to_posix_path_list (wtlff1, wtlff3);	\
  (pathout) = build_string (wtlff3);			\
} while (0)
#else
#define WIN32_TO_LOCAL_FILE_FORMAT(path, pathout)	\
do {							\
  (pathout) = build_string (path);			\
} while (0)
#endif

extern BOOL (WINAPI *xSwitchToThread) (VOID);

extern HKL (WINAPI *xGetKeyboardLayout) (DWORD);
extern BOOL (WINAPI *xSetMenuDefaultItem) (HMENU, UINT, UINT);
extern BOOL (WINAPI *xInsertMenuItemA) (HMENU, UINT, BOOL, LPCMENUITEMINFOA);
extern BOOL (WINAPI *xInsertMenuItemW) (HMENU, UINT, BOOL, LPCMENUITEMINFOW);
extern HANDLE (WINAPI *xLoadImageA) (HINSTANCE, LPCSTR, UINT, int, int, UINT);
extern HANDLE (WINAPI *xLoadImageW) (HINSTANCE, LPCWSTR, UINT, int, int, UINT);
extern ATOM (WINAPI *xRegisterClassExA) (CONST WNDCLASSEXA *);
extern ATOM (WINAPI *xRegisterClassExW) (CONST WNDCLASSEXW *);

extern int (WINAPI *xEnumFontFamiliesExA) (HDC, LPLOGFONTA, FONTENUMPROCA,
					   LPARAM, DWORD);
extern int (WINAPI *xEnumFontFamiliesExW) (HDC, LPLOGFONTW, FONTENUMPROCW,
					   LPARAM, DWORD);

extern DWORD (WINAPI *xSHGetFileInfoA) (LPCSTR, DWORD, SHFILEINFOA FAR *, UINT,
					UINT);
extern DWORD (WINAPI *xSHGetFileInfoW) (LPCWSTR, DWORD, SHFILEINFOW FAR *,
					UINT, UINT);

#endif /* INCLUDED_syswindows_h_ */
