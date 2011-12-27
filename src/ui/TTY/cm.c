/* Cursor motion subroutines for SXEmacs.
   Copyright (C) 1985, 1994, 1995 Free Software Foundation, Inc.
    loosely based primarily on public domain code written by Chris Torek

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


/* Synched up with: FSF 19.30.  Substantially different from FSF. */

/* #### This file is extremely junky and needs major fixup. */

#include <config.h>
#include "lisp.h"

#include "console-tty.h"
#include "ui/frame.h"
#include "lstream.h"
#include "ui/redisplay.h"

#define	EXPENSIVE 2000

EXTERN_C char *tgoto(const char *cm, int hpos, int vpos);
EXTERN_C int tputs(const char *, int, void (*)(int));

static void cmgoto_for_real(struct console *c, int row, int col);

static int cm_cost_counter;	/* sums up costs */

static void evalcost(int c)
{
	cm_cost_counter++;
}

/* Ugh -- cmputc() can't take a console argument, so we pass it in a global */
struct console *cmputc_console;

void send_string_to_tty_console(struct console *c, unsigned char *str, int len)
{
	/* #### Ben sez: don't some terminals need nulls outputted
	   for proper timing? */
	Lstream *lstr = XLSTREAM(CONSOLE_TTY_DATA(c)->outstream);

	if (CONSOLE_TTY_REAL_CURSOR_X(c) != CONSOLE_TTY_CURSOR_X(c)
	    || CONSOLE_TTY_REAL_CURSOR_Y(c) != CONSOLE_TTY_CURSOR_Y(c)) {
		int row = CONSOLE_TTY_CURSOR_Y(c);
		int col = CONSOLE_TTY_CURSOR_X(c);
		cmgoto_for_real(c, row, col);
	}

	if (len == 1)
		Lstream_putc(lstr, *str);
	else if (len > 0)
		Lstream_write(lstr, str, len);
}

void cmputc(int c)
{
	unsigned char ch = (unsigned char)c;

	if (termscript)
		fputc(c, termscript);

	send_string_to_tty_console(cmputc_console, &ch, 1);
}

#if 0

/*
 * Terminals with magicwrap (xn) don't all behave identically.
 * The VT100 leaves the cursor in the last column but will wrap before
 * printing the next character.  I hear that the Concept terminal does
 * the wrap immediately but ignores the next newline it sees.  And some
 * terminals just have buggy firmware, and think that the cursor is still
 * in limbo if we use direct cursor addressing from the phantom column.
 * The only guaranteed safe thing to do is to emit a CRLF immediately
 * after we reach the last column; this takes us to a known state.
 */
void cmcheckmagic(void)
{
	if (curX == FrameCols) {
		if (!MagicWrap || curY >= FrameRows - 1)
			abort();
		if (termscript)
			putc('\r', termscript);
		putchar('\r');
		if (termscript)
			putc('\n', termscript);
		putchar('\n');
		curX = 0;
		curY++;
	}
}

#endif				/* 0 */

/*
 * (Re)Initialize the cost factors, given the output speed of the
 * terminal in DEVICE_TTY_DATA (dev)->ospeed.  (Note: this holds B300,
 * B9600, etc -- ie stuff out of <sgtty.h>.)
 */
void cm_cost_init(struct console *c)
{
	char *tmp;

	cm_cost_counter = 0;
#define	COST(x,e) (x \
		   ? (cm_cost_counter = 0, tputs (x, 1, e), cm_cost_counter) \
		   : EXPENSIVE)
#define MINCOST(x,e) ((x == 0) \
		      ? EXPENSIVE \
		      : (tmp = tgoto(x, 0, 0), COST(tmp,e)))

	TTY_COST(c).cm_up = COST(TTY_CM(c).up, evalcost);
	TTY_COST(c).cm_down = COST(TTY_CM(c).down, evalcost);
	TTY_COST(c).cm_left = COST(TTY_CM(c).left, evalcost);
	TTY_COST(c).cm_right = COST(TTY_CM(c).right, evalcost);
	TTY_COST(c).cm_home = COST(TTY_CM(c).home, evalcost);
	TTY_COST(c).cm_low_left = COST(TTY_CM(c).low_left, evalcost);
	TTY_COST(c).cm_car_return = COST(TTY_CM(c).car_return, evalcost);

	/*
	 * These last three are actually minimum costs.  When (if) they are
	 * candidates for the least-cost motion, the real cost is computed.
	 * (Note that "0" is the assumed to generate the minimum cost.
	 * While this is not necessarily true, I have yet to see a terminal
	 * for which is not; all the terminals that have variable-cost
	 * cursor motion seem to take straight numeric values.  --ACT)
	 */

	TTY_COST(c).cm_abs = MINCOST(TTY_CM(c).abs, evalcost);
	TTY_COST(c).cm_hor_abs = MINCOST(TTY_CM(c).hor_abs, evalcost);
	TTY_COST(c).cm_ver_abs = MINCOST(TTY_CM(c).ver_abs, evalcost);

#undef MINCOST
#undef COST
}

/*
 * Calculate the cost to move from (srcy, srcx) to (dsty, dstx) using
 * up and down, and left and right, and motions.  If doit is set
 * actually perform the motion.
 */

#ifdef NOT_YET
static int
calccost(struct frame *f, int srcy, int srcx, int dsty, int dstx, int doit)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));
	int totalcost = 0;
	int deltay, deltax;
	char *motion;
	int motion_cost;

#if 0
	int ntabs, n2tabs, tabx, tab2x, tabcost;
#endif

	cmputc_console = c;
#if 0
	/* If have just wrapped on a terminal with xn,
	   don't believe the cursor position: give up here
	   and force use of absolute positioning.  */
	if (curX == Wcm.cm_cols)
		goto fail;
#endif

	deltay = dsty - srcy;
	if (!deltay)
		goto calculate_x;

	if (deltay < 0) {
		motion = TTY_CM(c).up;
		motion_cost = TTY_COST(c).cm_up;
		deltay = -deltay;
	} else {
		motion = TTY_CM(c).down;
		motion_cost = TTY_COST(c).cm_down;
	}

	if (motion_cost == EXPENSIVE) {
/*      if (doit) */
		/* #### printing OOF is not acceptable */
		return motion_cost;
	}

	totalcost = motion_cost * deltay;

	if (doit)
		while (--deltay >= 0)
			tputs(motion, 1, cmputc);

      calculate_x:

	deltax = dstx - srcx;
	if (!deltax)
		goto done;

	if (deltax < 0) {
		motion = TTY_CM(c).left;
		motion_cost = TTY_COST(c).cm_left;
		deltax = -deltax;
	} else {
		motion = TTY_CM(c).right;
		motion_cost = TTY_COST(c).cm_right;
	}

	if (motion_cost == EXPENSIVE) {
/*	if (doit) */
		/* #### printing OOF is not acceptable */
		return motion_cost;
	}

	totalcost += motion_cost * deltax;

	if (doit)
		while (--deltax >= 0)
			tputs(motion, 1, cmputc);

      done:
	return totalcost;
}
#endif				/* NOT_YET */

#define	USEREL	0
#define	USEHOME	1
#define	USELL	2
#define	USECR	3

/*****************************************************************************
 cmgoto

 This function is responsible for getting the cursor from its current
 location to the passed location in the most efficient manner
 possible.
 ****************************************************************************/
static void cmgoto_for_real(struct console *c, int row, int col)
{
	char *motion;

	cmputc_console = c;

	/* First make sure that we actually have to do any work at all. */
	if (CONSOLE_TTY_REAL_CURSOR_X(c) == col
	    && CONSOLE_TTY_REAL_CURSOR_Y(c) == row)
		return;

	CONSOLE_TTY_REAL_CURSOR_X(c) = col;
	CONSOLE_TTY_REAL_CURSOR_Y(c) = row;

	/* #### Need to reimplement cost analysis and potential relative
	   movement. */

	/* If all else fails, use absolute movement. */
	motion = tgoto(TTY_CM(c).abs, col, row);
	tputs(motion, 1, cmputc);
	CONSOLE_TTY_CURSOR_X(c) = col;
	CONSOLE_TTY_CURSOR_Y(c) = row;
}

void cmgoto(struct frame *f, int row, int col)
{
	/* We delay cursor motion until we do something other than cursor motion,
	   to optimize the case where cmgoto() is called twice in a row. */
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));
	CONSOLE_TTY_CURSOR_X(c) = col;
	CONSOLE_TTY_CURSOR_Y(c) = row;
}

#if 0
/* Clear out all terminal info.
   Used before copying into it the info on the actual terminal.
 */

void Wcm_clear(void)
{
	xzero(Wcm);
	UP = 0;
	BC = 0;
}
#endif

#if 0
/*
 * Initialized stuff
 * Return 0 if can do CM.
 * Return -1 if cannot.
 * Return -2 if size not specified.
 */

int Wcm_init(void)
{
#if 0
	if (Wcm.cm_abs && !Wcm.cm_ds)
		return 0;
#endif
	if (Wcm.cm_abs)
		return 0;
	/* Require up and left, and, if no absolute, down and right */
	if (!Wcm.cm_up || !Wcm.cm_left)
		return -1;
	if (!Wcm.cm_abs && (!Wcm.cm_down || !Wcm.cm_right))
		return -1;
	/* Check that we know the size of the frame.... */
	if (Wcm.cm_rows <= 0 || Wcm.cm_cols <= 0)
		return -2;
	return 0;
}
#endif
