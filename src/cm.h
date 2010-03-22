/* Cursor motion calculation definitions for XEmacs
   Copyright (C) 1985, 1989, 1992, 1993 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.30. */

/* #### Chuck -- This file should be deleted.  I'm not deleting it yet
   because there might be something you want out of it. */

#ifndef INCLUDED_cm_h_
#define INCLUDED_cm_h_

/* Holds the minimum and maximum costs for the parametrized capabilities.  */
struct parmcap {
	int mincost, maxcost;
};

/* This structure holds everything needed to do cursor motion except the pad
   character (PC) and the output speed of the terminal (ospeed), which
   termcap wants in global variables.  */

struct cm {
#if 0
	/* Cursor position.  -1 in *both* variables means the cursor
	   position is unknown, in order to force absolute cursor motion. */

	int cm_curY;		/* Current row */
	int cm_curX;		/* Current column */

	/* Capabilities from termcap */
	const char *cm_up;	/* up (up) */
	const char *cm_down;	/* down (do) */
	const char *cm_left;	/* left (le) */
	const char *cm_right;	/* right (nd) */
	const char *cm_home;	/* home (ho) */
	const char *cm_cr;	/* carriage return (cr) */
	const char *cm_ll;	/* last line (ll) */
#endif				/* 0 */
	const char *cm_tab;	/* tab (ta) */
	const char *cm_backtab;	/* backtab (bt) */
#if 0
	const char *cm_abs;	/* absolute (cm) */
	const char *cm_habs;	/* horizontal absolute (ch) */
	const char *cm_vabs;	/* vertical absolute (cv) */
	const char *cm_ds;	/* "don't send" string (ds) */
	const char *cm_multiup;	/* multiple up (UP) */
	const char *cm_multidown;	/* multiple down (DO) */
	const char *cm_multileft;	/* multiple left (LE) */
	const char *cm_multiright;	/* multiple right (RI) */
	int cm_cols;		/* number of cols on frame (co) */
	int cm_rows;		/* number of rows on frame (li) */
	int cm_tabwidth;	/* tab width (it) */
	unsigned int cm_autowrap:1;	/* autowrap flag (am) */
	unsigned int cm_magicwrap:1;	/* VT-100: cursor stays in last col but
					   will cm_wrap if next char is
					   printing (xn) */
	unsigned int cm_usetabs:1;	/* if set, use tabs */
	unsigned int cm_losewrap:1;	/* if reach right margin, forget cursor
					   location */
	unsigned int cm_autolf:1;	/* \r performs a \r\n (rn) */
#endif

	/* Parametrized capabilities.  This needs to be a struct since
	   the costs are accessed through pointers.  */

#if 0
	struct parmcap cc_abs;	/* absolute (cm) */
	struct parmcap cc_habs;	/* horizontal absolute (ch) */
	struct parmcap cc_vabs;	/* vertical absolute (cv) */
	struct parmcap cc_multiup;	/* multiple up (UP) */
	struct parmcap cc_multidown;	/* multiple down (DO) */
	struct parmcap cc_multileft;	/* multiple left (LE) */
	struct parmcap cc_multiright;	/* multiple right (RI) */
#endif

#if 0
	/* Costs for the non-parametrized capabilities */
	int cc_up;		/* cost for up */
	int cc_down;		/* etc. */
	int cc_left;
	int cc_right;
	int cc_home;
	int cc_cr;
	int cc_ll;
	int cc_tab;
	int cc_backtab;
	/* These are temporary, until the code is installed to use the
	   struct parmcap fields above.  */
	int cc_abs;
	int cc_habs;
	int cc_vabs;
#endif
};

#if 0
extern struct cm Wcm;		/* Terminal capabilities */
extern char PC;			/* Pad character */

/* Shorthand */
#ifndef NoCMShortHand
#define curY		Wcm.cm_curY
#define curX		Wcm.cm_curX
#define Up		Wcm.cm_up
#define Down		Wcm.cm_down
#define Left		Wcm.cm_left
#define Right		Wcm.cm_right
#define Tab		Wcm.cm_tab
#define BackTab		Wcm.cm_backtab
#define TabWidth	Wcm.cm_tabwidth
#define CR		Wcm.cm_cr
#define Home		Wcm.cm_home
#define LastLine	Wcm.cm_ll
#define AbsPosition	Wcm.cm_abs
#define ColPosition	Wcm.cm_habs
#define RowPosition	Wcm.cm_vabs
#define MultiUp		Wcm.cm_multiup
#define MultiDown	Wcm.cm_multidown
#define MultiLeft	Wcm.cm_multileft
#define MultiRight	Wcm.cm_multiright
#define AutoWrap	Wcm.cm_autowrap
#define MagicWrap	Wcm.cm_magicwrap
#define UseTabs		Wcm.cm_usetabs
#define FrameRows	Wcm.cm_rows
#define FrameCols	Wcm.cm_cols

#define UpCost		Wcm.cc_up
#define DownCost	Wcm.cc_down
#define LeftCost	Wcm.cc_left
#define RightCost	Wcm.cc_right
#define HomeCost	Wcm.cc_home
#define CRCost		Wcm.cc_cr
#define LastLineCost	Wcm.cc_ll
#define TabCost		Wcm.cc_tab
#define BackTabCost	Wcm.cc_backtab
#define AbsPositionCost	Wcm.cc_abs
#define ColPositionCost	Wcm.cc_habs
#define RowPositionCost	Wcm.cc_vabs
#define MultiUpCost	Wcm.cc_multiup
#define MultiDownCost	Wcm.cc_multidown
#define MultiLeftCost	Wcm.cc_multileft
#define MultiRightCost	Wcm.cc_multiright
#endif
#endif				/* 0 */

#define cmat(row,col)	(curY = (row), curX = (col))
#define cmplus(n)					\
  {							\
    if ((curX += (n)) >= FrameCols && !MagicWrap)	\
      {							\
	if (Wcm.cm_losewrap) losecursor ();		\
	else if (AutoWrap) curX = 0, curY++;		\
	else curX--;					\
      }							\
  }

#define losecursor()	(curX = -1, curY = -1)

extern int cost;
void cmputc(int c);
void cmcheckmagic(void);
void cm_cost_init(struct console *c);
void cmgoto(int, int);
void Wcm_clear(void);
int Wcm_init(void);

#endif				/* INCLUDED_cm_h_ */
