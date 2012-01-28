/* Image processing functions
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


/* Synched up with: Not in FSF. */

/* Original author: Jareth Hein */

/* Parts of this file are based on code from Sam Leffler's tiff library,
   with the original copyright displayed here:

   Copyright (c) 1988-1997 Sam Leffler
   Copyright (c) 1991-1997 Silicon Graphics, Inc.

   Permission to use, copy, modify, distribute, and sell this software and
   its documentation for any purpose is hereby granted without fee, provided
   that (i) the above copyright notices and this permission notice appear in
   all copies of the software and related documentation, and (ii) the names of
   Sam Leffler and Silicon Graphics may not be used in any advertising or
   publicity relating to the software without the specific, prior written
   permission of Sam Leffler and Silicon Graphics. */

/* Quantizing code based off of the paper
   Color Image Quantization for Frame Buffer Display, Paul Heckbert,
   Siggraph '82 proceedings, pp. 297-307 */

#include <config.h>
#include "lisp.h"
#include "imgproc.h"

static void
get_histogram(quant_table * qt, unsigned char *pic,
	      int width, int height, Colorbox * box)
{
	register unsigned char *inptr;
	register int red, green, blue;
	register int j, i;

	box->rmin = box->gmin = box->bmin = 999;
	box->rmax = box->gmax = box->bmax = -1;
	box->total = width * height;

	inptr = pic;
	for (i = 0; i < height; i++) {
		for (j = width; j-- > 0;) {
			red = (*inptr++ >> COLOR_SHIFT) & COLOR_MASK;
			green = (*inptr++ >> COLOR_SHIFT) & COLOR_MASK;
			blue = (*inptr++ >> COLOR_SHIFT) & COLOR_MASK;
			if (red < box->rmin)
				box->rmin = red;
			if (red > box->rmax)
				box->rmax = red;
			if (green < box->gmin)
				box->gmin = green;
			if (green > box->gmax)
				box->gmax = green;
			if (blue < box->bmin)
				box->bmin = blue;
			if (blue > box->bmax)
				box->bmax = blue;
#if defined(DEBUG_SXEMACS) && DEBUG_SXEMACS
			if( red < 0 || green < 0 || blue < 0 ||
			    ! (red < B_LEN && green < B_LEN && blue < B_LEN) ) {
				abort();
			} else
#endif
				qt->histogram[red][green][blue]++;
		}
	}
}

static Colorbox *largest_box(quant_table * qt)
{
	register Colorbox *p, *b;
	register int size;

	b = NULL;
	size = -1;
	for (p = qt->usedboxes; p != NULL; p = p->next)
		if ((p->rmax > p->rmin || p->gmax > p->gmin ||
		     p->bmax > p->bmin) && p->total > size)
			size = (b = p)->total;
	return (b);
}

static void shrinkbox(quant_table * qt, Colorbox * box)
{
	register int *histp, ir, ig, ib;

	if (box->rmax > box->rmin) {
		for (ir = box->rmin; ir <= box->rmax; ++ir)
			for (ig = box->gmin; ig <= box->gmax; ++ig) {
				histp = &(qt->histogram[ir][ig][box->bmin]);
				for (ib = box->bmin; ib <= box->bmax; ++ib)
					if (*histp++ != 0) {
						box->rmin = ir;
						goto have_rmin;
					}
			}
	      have_rmin:
		if (box->rmax > box->rmin)
			for (ir = box->rmax; ir >= box->rmin; --ir)
				for (ig = box->gmin; ig <= box->gmax; ++ig) {
					histp =
					    &(qt->histogram[ir][ig][box->bmin]);
					ib = box->bmin;
					for (; ib <= box->bmax; ++ib)
						if (*histp++ != 0) {
							box->rmax = ir;
							goto have_rmax;
						}
				}
	}
      have_rmax:
	if (box->gmax > box->gmin) {
		for (ig = box->gmin; ig <= box->gmax; ++ig)
			for (ir = box->rmin; ir <= box->rmax; ++ir) {
				histp = &(qt->histogram[ir][ig][box->bmin]);
				for (ib = box->bmin; ib <= box->bmax; ++ib)
					if (*histp++ != 0) {
						box->gmin = ig;
						goto have_gmin;
					}
			}
	      have_gmin:
		if (box->gmax > box->gmin)
			for (ig = box->gmax; ig >= box->gmin; --ig)
				for (ir = box->rmin; ir <= box->rmax; ++ir) {
					histp =
					    &(qt->histogram[ir][ig][box->bmin]);
					ib = box->bmin;
					for (; ib <= box->bmax; ++ib)
						if (*histp++ != 0) {
							box->gmax = ig;
							goto have_gmax;
						}
				}
	}
      have_gmax:
	if (box->bmax > box->bmin) {
		for (ib = box->bmin; ib <= box->bmax; ++ib)
			for (ir = box->rmin; ir <= box->rmax; ++ir) {
				histp = &(qt->histogram[ir][box->gmin][ib]);
				for (ig = box->gmin; ig <= box->gmax; ++ig) {
					if (*histp != 0) {
						box->bmin = ib;
						goto have_bmin;
					}
					histp += B_LEN;
				}
			}
	      have_bmin:
		if (box->bmax > box->bmin)
			for (ib = box->bmax; ib >= box->bmin; --ib)
				for (ir = box->rmin; ir <= box->rmax; ++ir) {
					histp =
					    &(qt->histogram[ir][box->gmin][ib]);
					ig = box->gmin;
					for (; ig <= box->gmax; ++ig) {
						if (*histp != 0) {
							box->bmax = ib;
							goto have_bmax;
						}
						histp += B_LEN;
					}
				}
	}
      have_bmax:
	;
}

static void splitbox(quant_table * qt, Colorbox * ptr)
{
	int hist2[B_LEN];
	int first = 0, last = 0;
	register Colorbox *new;
	register int *iptr, *histp;
	register int i, j;
	register int ir, ig, ib;
	register int sum, sum1, sum2;
	enum { RED, GREEN, BLUE } axis;

	/*
	 * See which axis is the largest, do a histogram along that
	 * axis.  Split at median point.  Contract both new boxes to
	 * fit points and return
	 */
	i = ptr->rmax - ptr->rmin;
	if (i >= ptr->gmax - ptr->gmin && i >= ptr->bmax - ptr->bmin)
		axis = RED;
	else if (ptr->gmax - ptr->gmin >= ptr->bmax - ptr->bmin)
		axis = GREEN;
	else
		axis = BLUE;
	/* get histogram along longest axis */
	switch (axis) {
	case RED:
		histp = &hist2[ptr->rmin];
		for (ir = ptr->rmin; ir <= ptr->rmax; ++ir) {
			*histp = 0;
			for (ig = ptr->gmin; ig <= ptr->gmax; ++ig) {
				iptr = &(qt->histogram[ir][ig][ptr->bmin]);
				for (ib = ptr->bmin; ib <= ptr->bmax; ++ib)
					*histp += *iptr++;
			}
			histp++;
		}
		first = ptr->rmin;
		last = ptr->rmax;
		break;
	case GREEN:
		histp = &hist2[ptr->gmin];
		for (ig = ptr->gmin; ig <= ptr->gmax; ++ig) {
			*histp = 0;
			for (ir = ptr->rmin; ir <= ptr->rmax; ++ir) {
				iptr = &(qt->histogram[ir][ig][ptr->bmin]);
				for (ib = ptr->bmin; ib <= ptr->bmax; ++ib)
					*histp += *iptr++;
			}
			histp++;
		}
		first = ptr->gmin;
		last = ptr->gmax;
		break;
	case BLUE:
		histp = &hist2[ptr->bmin];
		for (ib = ptr->bmin; ib <= ptr->bmax; ++ib) {
			*histp = 0;
			for (ir = ptr->rmin; ir <= ptr->rmax; ++ir) {
				iptr = &(qt->histogram[ir][ptr->gmin][ib]);
				for (ig = ptr->gmin; ig <= ptr->gmax; ++ig) {
					*histp += *iptr;
					iptr += B_LEN;
				}
			}
			histp++;
		}
		first = ptr->bmin;
		last = ptr->bmax;
		break;
	default:
		/* just crash */
		abort();
		break;
	}
	/* find median point */
	sum2 = ptr->total / 2;
	histp = &hist2[first];
	sum = 0;
	for (i = first; i <= last && (sum += *histp++) < sum2; ++i) ;
	if (i == first)
		i++;

	/* Create new box, re-allocate points */
	new = qt->freeboxes;
	qt->freeboxes = new->next;
	if (qt->freeboxes)
		qt->freeboxes->prev = NULL;
	if (qt->usedboxes)
		qt->usedboxes->prev = new;
	new->next = qt->usedboxes;
	qt->usedboxes = new;

	histp = &hist2[first];
	for (sum1 = 0, j = first; j < i; j++)
		sum1 += *histp++;
	for (sum2 = 0, j = i; j <= last; j++)
		sum2 += *histp++;
	new->total = sum1;
	ptr->total = sum2;

	new->rmin = ptr->rmin;
	new->rmax = ptr->rmax;
	new->gmin = ptr->gmin;
	new->gmax = ptr->gmax;
	new->bmin = ptr->bmin;
	new->bmax = ptr->bmax;
	switch (axis) {
	case RED:
		new->rmax = i - 1;
		ptr->rmin = i;
		break;
	case GREEN:
		new->gmax = i - 1;
		ptr->gmin = i;
		break;
	case BLUE:
		new->bmax = i - 1;
		ptr->bmin = i;
		break;
	default:
		/* just crash */
		abort();
		break;
	}
	shrinkbox(qt, new);
	shrinkbox(qt, ptr);
}

static C_cell *create_colorcell(quant_table * qt, int num_colors, int red,
				int green, int blue)
{
	register int ir, ig, ib, i;
	register C_cell *ptr;
	int mindist, next_n;
	register int tmp, dist, n;

	ir = red >> (COLOR_DEPTH - C_DEPTH);
	ig = green >> (COLOR_DEPTH - C_DEPTH);
	ib = blue >> (COLOR_DEPTH - C_DEPTH);
	ptr = (C_cell *)xmalloc_atomic(sizeof(C_cell));
	*(qt->ColorCells + ir * C_LEN * C_LEN + ig * C_LEN + ib) = ptr;
	ptr->num_ents = 0;

	/*
	 * Step 1: find all colors inside this cell, while we're at
	 *       it, find distance of centermost point to furthest corner
	 */
	mindist = 99999999;
	for (i = 0; i < num_colors; ++i) {
		if (qt->rm[i] >> (COLOR_DEPTH - C_DEPTH) != ir ||
		    qt->gm[i] >> (COLOR_DEPTH - C_DEPTH) != ig ||
		    qt->bm[i] >> (COLOR_DEPTH - C_DEPTH) != ib)
			continue;
		ptr->entries[ptr->num_ents][0] = i;
		ptr->entries[ptr->num_ents][1] = 0;
		++ptr->num_ents;
		tmp = qt->rm[i] - red;
		if (tmp < (MAX_COLOR / C_LEN / 2))
			tmp = MAX_COLOR / C_LEN - 1 - tmp;
		dist = tmp * tmp;
		tmp = qt->gm[i] - green;
		if (tmp < (MAX_COLOR / C_LEN / 2))
			tmp = MAX_COLOR / C_LEN - 1 - tmp;
		dist += tmp * tmp;
		tmp = qt->bm[i] - blue;
		if (tmp < (MAX_COLOR / C_LEN / 2))
			tmp = MAX_COLOR / C_LEN - 1 - tmp;
		dist += tmp * tmp;
		if (dist < mindist)
			mindist = dist;
	}

	/*
	 * Step 3: find all points within that distance to cell.
	 */
	for (i = 0; i < num_colors; ++i) {
		if (qt->rm[i] >> (COLOR_DEPTH - C_DEPTH) == ir &&
		    qt->gm[i] >> (COLOR_DEPTH - C_DEPTH) == ig &&
		    qt->bm[i] >> (COLOR_DEPTH - C_DEPTH) == ib)
			continue;
		dist = 0;
		if ((tmp = red - qt->rm[i]) > 0 ||
		    (tmp = qt->rm[i] - (red + MAX_COLOR / C_LEN - 1)) > 0)
			dist += tmp * tmp;
		if ((tmp = green - qt->gm[i]) > 0 ||
		    (tmp = qt->gm[i] - (green + MAX_COLOR / C_LEN - 1)) > 0)
			dist += tmp * tmp;
		if ((tmp = blue - qt->bm[i]) > 0 ||
		    (tmp = qt->bm[i] - (blue + MAX_COLOR / C_LEN - 1)) > 0)
			dist += tmp * tmp;
		if (dist < mindist) {
			ptr->entries[ptr->num_ents][0] = i;
			ptr->entries[ptr->num_ents][1] = dist;
			++ptr->num_ents;
		}
	}

	/*
	 * Sort color cells by distance, use cheap exchange sort
	 */
	for (n = ptr->num_ents - 1; n > 0; n = next_n) {
		next_n = 0;
		for (i = 0; i < n; ++i)
			if (ptr->entries[i][1] > ptr->entries[i + 1][1]) {
				tmp = ptr->entries[i][0];
				ptr->entries[i][0] = ptr->entries[i + 1][0];
				ptr->entries[i + 1][0] = tmp;
				tmp = ptr->entries[i][1];
				ptr->entries[i][1] = ptr->entries[i + 1][1];
				ptr->entries[i + 1][1] = tmp;
				next_n = i;
			}
	}
	return (ptr);
}

static int map_colortable(quant_table * qt, int num_colors)
{
	register int *histp = &(qt->histogram[0][0][0]);
	register C_cell *cell;
	register int j, tmp, d2, dist;
	int ir, ig, ib, i;

	for (ir = 0; ir < B_LEN; ++ir)
		for (ig = 0; ig < B_LEN; ++ig)
			for (ib = 0; ib < B_LEN; ++ib, histp++) {
				if (*histp == 0) {
					*histp = -1;
					continue;
				}
				cell = *(qt->ColorCells +
					 (((ir >> (B_DEPTH - C_DEPTH)) <<
					   C_DEPTH * 2) +
					  ((ig >> (B_DEPTH - C_DEPTH)) <<
					   C_DEPTH) + (ib >> (B_DEPTH -
							      C_DEPTH))));
				if (cell == NULL)
					cell = create_colorcell(qt, num_colors,
								ir <<
								COLOR_SHIFT,
								ig <<
								COLOR_SHIFT,
								ib <<
								COLOR_SHIFT);
				if (cell == NULL)	/* memory exhausted! punt! */
					return -1;
				dist = 9999999;
				for (i = 0; i < cell->num_ents &&
				     dist > cell->entries[i][1]; ++i) {
					j = cell->entries[i][0];
					d2 = qt->rm[j] - (ir << COLOR_SHIFT);
					d2 *= d2;
					tmp = qt->gm[j] - (ig << COLOR_SHIFT);
					d2 += tmp * tmp;
					tmp = qt->bm[j] - (ib << COLOR_SHIFT);
					d2 += tmp * tmp;
					if (d2 < dist) {
						dist = d2;
						*histp = j;
					}
				}
			}
	return 0;
}

quant_table *build_EImage_quantable(unsigned char *eimage, int width,
				    int height, int num_colors)
{
	quant_table *qt;
	Colorbox *box_list, *ptr;
	int i, res;

	qt = (quant_table *)xmalloc_and_zero(sizeof(quant_table));
	if (qt == NULL)
		return NULL;

	assert(num_colors < 257 && num_colors > 2);
	/*
	 * STEP 1:  create empty boxes
	 */
	qt->usedboxes = NULL;
	box_list = qt->freeboxes = xnew_array(Colorbox, num_colors);
	qt->freeboxes[0].next = &(qt->freeboxes[1]);
	qt->freeboxes[0].prev = NULL;
	for (i = 1; i < num_colors - 1; ++i) {
		qt->freeboxes[i].next = &(qt->freeboxes[i + 1]);
		qt->freeboxes[i].prev = &(qt->freeboxes[i - 1]);
	}
	qt->freeboxes[num_colors - 1].next = NULL;
	qt->freeboxes[num_colors - 1].prev = &(qt->freeboxes[num_colors - 2]);

	/*
	 * STEP 2: get histogram, initialize first box
	 */
	ptr = qt->freeboxes;
	qt->freeboxes = ptr->next;
	if (qt->freeboxes)
		qt->freeboxes->prev = NULL;
	ptr->next = qt->usedboxes;
	qt->usedboxes = ptr;
	if (ptr->next)
		ptr->next->prev = ptr;
	get_histogram(qt, eimage, width, height, ptr);

	/*
	 * STEP 3: continually subdivide boxes until no more free
	 * boxes remain or until all colors assigned.
	 */
	while (qt->freeboxes != NULL) {
		ptr = largest_box(qt);
		if (ptr != NULL)
			splitbox(qt, ptr);
		else
			qt->freeboxes = NULL;
	}

	/*
	 * STEP 4: assign colors to all boxes
	 */
	for (i = 0, ptr = qt->usedboxes; ptr != NULL; ++i, ptr = ptr->next) {
		qt->rm[i] = ((ptr->rmin + ptr->rmax) << COLOR_SHIFT) / 2;
		qt->gm[i] = ((ptr->gmin + ptr->gmax) << COLOR_SHIFT) / 2;
		qt->bm[i] = ((ptr->bmin + ptr->bmax) << COLOR_SHIFT) / 2;
		qt->um[i] = ptr->total;
	}
	qt->num_active_colors = i;

	/* We're done with the boxes now */
	xfree(box_list);
	qt->freeboxes = qt->usedboxes = NULL;

	/*
	 * STEP 5: scan histogram and map all values to closest color
	 */
	/* 5a: create cell list as described in Heckbert */
	qt->ColorCells = xnew_array_and_zero(C_cell*, C_LEN * C_LEN * C_LEN);
	/* 5b: create mapping from truncated pixel space to color
	   table entries */
	res = map_colortable(qt, num_colors);

	/* 5c: done with ColorCells */
	for (i = 0; i < C_LEN * C_LEN * C_LEN; i++) {
		if (qt->ColorCells[i])
			xfree(qt->ColorCells[i]);
	}
	xfree(qt->ColorCells);

	if (res) {
		/* we failed in memory allocation, so clean up an leave */
		xfree(qt);
		return NULL;
	}

	return qt;
}
