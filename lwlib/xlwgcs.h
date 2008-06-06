 /* Tabs Widget for SXEmacs.
    Copyright (C) 1999 Edward A. Falk

    This file is part of SXEmacs.

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

 /* Synched up with: Gcs 1.7 */

#ifndef	GCS_H
#define	GCS_H

/* Overview of functions provided here:
 *
 * AllocFgGC()
 *	Given a foreground pixel & a font, return an appropriate GC
 *
 * AllocBackgroundGC()
 *	Given a widget, return a GC for painting the background color
 *
 * AllocShadeGC()
 *	Given foreground, background, a contrast value & be_nice_to_colormap
 *	flag, return a GC suitable for rendering in an intermediate color,
 *	as determined by contrast.  May return a dither pattern or a
 *	solid color, as appropriate.
 *
 *	Contrast 0 = background color, 100 = foreground color.  It is legal
 *	for contrast to be more than 100 or less than 0.
 *
 * AllocGreyGC()
 *	Given widget, foreground, font, contrast & be_nice_to_colormap,
 *	return a shade GC (see above) based on foreground and widget
 *	background.
 *
 * AllocTopShadowGC()
 *	Given widget, contrast & be_nice_to_colormap, return a GC suitable
 *	for rendering the top shadow.
 *
 *	Contrast 0 = use background pixel.  Contrast > 0 = use brighter
 *	colors.
 *
 * AllocBotShadowGC()
 *	Given widget, contrast & be_nice_to_colormap, return a GC suitable
 *	for rendering the bottom shadow.
 *
 *	Contrast 0 = use background pixel.  Contrast > 0 = use darker
 *	colors.
 *
 * AllocArmShadowGC()
 *	Given widget, contrast & be_nice_to_colormap, return a GC suitable
 *	for rendering the "armed" shadow.
 *
 *	Contrast 0 = use background pixel.  Contrast > 0 = use darker
 *	colors.
 *
 * AllocShadowPixel()
 *	Given a widget & scale factor, allocate & return a color darker
 *	or lighter than the background pixel, as determined by scale.
 *
 *	Scale 100 = use background pixel.  Scale > 100 = brighter color,
 *	Scale < 100 = darker color.
 *
 * AllocGreyPixel()
 *	Given two pixel values and scale factor, allocate & return a
 *	pixel value between them, according to scale.
 *
 *	Scale == 0:	background color
 *	Scale == 100:	foreground color
 *	0<Scale<100:	intermediate color
 *	Scale > 100:	more foreground
 *	Scale < 0:	more background
 *
 *
 * AllocGreyPixelC()
 *	Given two color values and scale factor, allocate & return a
 *	pixel value between them, according to scale.
 *
 *	Scale == 0:	background color
 *	Scale == 100:	foreground color
 *	0<Scale<100:	intermediate color
 *	Scale > 100:	more foreground
 *	Scale < 0:	more background
 *
 * Draw3dBox()
 *	Given box dimensions, shadow width, top shadow GC & bottom shadow GC,
 *	draw a 3-d box.
 */

#if	NeedFunctionPrototypes

extern GC AllocFgGC(Widget w, Pixel fg, Font font);
extern GC AllocBackgroundGC(Widget w, Font font);
extern GC AllocShadeGC(Widget w, Pixel fg, Pixel bg, Font, int contrast, Bool);
extern GC AllocGreyGC(Widget w, Pixel fg, Font, int, Bool);
extern GC AllocTopShadowGC(Widget w, int contrast, int);
extern GC AllocBotShadowGC(Widget w, int contrast, int);
extern GC AllocArmGC(Widget w, int contrast, int);
extern Pixel AllocShadowPixel(Widget, int scale);
extern Pixel AllocGreyPixel(Widget, Pixel fg, Pixel bg, int scale);
extern Pixel AllocGreyPixelC(Widget, XColor * fg, XColor * bg, int scale);
extern void Draw3dBox(Widget w, int x, int y, int wid, int hgt, int s,
		      GC topgc, GC botgc);

#if XtSpecificationRelease < 5
extern GC XtAllocateGC(Widget, int depth, unsigned long mask,
		       XGCValues *,
		       unsigned long dynamic, unsigned long dontcare);
#endif

#else

extern GC AllocFgGC();
extern GC AllocBackgroundGC();
extern GC AllocShadeGC();
extern GC AllocGreyGC();
extern GC AllocTopShadowGC();
extern GC AllocBotShadowGC();
extern GC AllocArmGC();
extern Pixel AllocShadowPixel();
extern Pixel AllocGreyPixel();
extern Pixel AllocGreyPixelC();
extern void Draw3dBox();

#if XtSpecificationRelease < 5
extern GC XtAllocateGC();
#endif

#endif

#endif				/* GCS_H */
