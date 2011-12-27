/* Image processing aux functions header file
   Copyright (C) 1998 Jareth Hein

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


#ifndef INCLUDED_imgproc_h_
#define INCLUDED_imgproc_h_

/* Synched up with: Not in FSF. */

/* Original author: Jareth Hein */

#define	MAX_CMAP_SIZE	256
#define	COLOR_DEPTH	8
#define	MAX_COLOR	256

#define	B_DEPTH		5	/* # bits/pixel to use */
#define	B_LEN		(1L<<B_DEPTH)

#define	C_DEPTH		2
#define	C_LEN		(1L<<C_DEPTH)	/* # cells/color to use */

#define	COLOR_SHIFT	(COLOR_DEPTH-B_DEPTH)
#define COLOR_MASK      (B_LEN-1)

typedef struct colorbox {
	struct colorbox *next, *prev;
	int rmin, rmax;
	int gmin, gmax;
	int bmin, bmax;
	int total;
} Colorbox;

typedef struct {
	int num_ents;
	int entries[MAX_CMAP_SIZE][2];
} C_cell;

typedef struct {
	unsigned short rm[MAX_CMAP_SIZE], gm[MAX_CMAP_SIZE], bm[MAX_CMAP_SIZE];	/* map values */
	int um[MAX_CMAP_SIZE];	/* usage counts for each mapentry */
	int histogram[B_LEN][B_LEN][B_LEN];
	int num_active_colors;
	Colorbox *freeboxes;	/* used and freed internally */
	Colorbox *usedboxes;	/* used and freed internally */
	C_cell **ColorCells;	/* used and freed internally */
} quant_table;

#define QUANT_GET_COLOR(qt,r,g,b) (qt->histogram[r>>COLOR_SHIFT][g>>COLOR_SHIFT][b>>COLOR_SHIFT])

quant_table *build_EImage_quantable(unsigned char *eimage, int width,
				    int height, int num_colors);

#endif				/* INCLUDED_imgproc_h_ */
