/* mswindows-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1997 Jonathan Harris.

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

   Jamie Zawinski, Chuck Thompson, Ben Wing
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
 */


/* TODO: palette handling */

#include <config.h>
#include "lisp.h"
#include "hash.h"

#include "console-msw.h"
#include "objects-msw.h"

#ifdef MULE
#include "mule-charset.h"
#endif

#include "buffer.h"
#include "device.h"
#include "insdel.h"

typedef struct colormap_t
{
  const char *name;
  COLORREF colorref;
} colormap_t;

/* Colors from X11R6 "XConsortium: rgb.txt,v 10.41 94/02/20 18:39:36 rws Exp" */
/* MSWindows tends to round up the numbers in it's palette, ie where X uses
 * 127, MSWindows uses 128. Colors commented as "Adjusted" are tweaked to
 * match the Windows standard palette to increase the likelihood of
 * mswindows_color_to_string() finding a named match.
 */
static const colormap_t mswindows_X_color_map[] =
{
  {"white"		, PALETTERGB (255, 255, 255) },
  {"black"		, PALETTERGB (0, 0, 0) },
  {"snow"		, PALETTERGB (255, 250, 250) },
  {"GhostWhite"		, PALETTERGB (248, 248, 255) },
  {"WhiteSmoke"		, PALETTERGB (245, 245, 245) },
  {"gainsboro"		, PALETTERGB (220, 220, 220) },
  {"FloralWhite"	, PALETTERGB (255, 250, 240) },
  {"OldLace"		, PALETTERGB (253, 245, 230) },
  {"linen"		, PALETTERGB (250, 240, 230) },
  {"AntiqueWhite"	, PALETTERGB (250, 235, 215) },
  {"PapayaWhip"		, PALETTERGB (255, 239, 213) },
  {"BlanchedAlmond"	, PALETTERGB (255, 235, 205) },
  {"bisque"		, PALETTERGB (255, 228, 196) },
  {"PeachPuff"		, PALETTERGB (255, 218, 185) },
  {"NavajoWhite"	, PALETTERGB (255, 222, 173) },
  {"moccasin"		, PALETTERGB (255, 228, 181) },
  {"cornsilk"		, PALETTERGB (255, 248, 220) },
  {"ivory"		, PALETTERGB (255, 255, 240) },
  {"LemonChiffon"	, PALETTERGB (255, 250, 205) },
  {"seashell"		, PALETTERGB (255, 245, 238) },
  {"honeydew"		, PALETTERGB (240, 255, 240) },
  {"MintCream"		, PALETTERGB (245, 255, 250) },
  {"azure"		, PALETTERGB (240, 255, 255) },
  {"AliceBlue"		, PALETTERGB (240, 248, 255) },
  {"lavender"		, PALETTERGB (230, 230, 250) },
  {"LavenderBlush"	, PALETTERGB (255, 240, 245) },
  {"MistyRose"		, PALETTERGB (255, 228, 225) },
  {"DarkSlateGray"	, PALETTERGB (47, 79, 79) },
  {"DarkSlateGrey"	, PALETTERGB (47, 79, 79) },
  {"DimGray"		, PALETTERGB (105, 105, 105) },
  {"DimGrey"		, PALETTERGB (105, 105, 105) },
  {"SlateGray"		, PALETTERGB (112, 128, 144) },
  {"SlateGrey"		, PALETTERGB (112, 128, 144) },
  {"LightSlateGray"	, PALETTERGB (119, 136, 153) },
  {"LightSlateGrey"	, PALETTERGB (119, 136, 153) },
  {"gray"		, PALETTERGB (190, 190, 190) },
  {"grey"		, PALETTERGB (190, 190, 190) },
  {"LightGrey"		, PALETTERGB (211, 211, 211) },
  {"LightGray"		, PALETTERGB (211, 211, 211) },
  {"MidnightBlue"	, PALETTERGB (25, 25, 112) },
  {"navy"		, PALETTERGB (0, 0, 128) },
  {"NavyBlue"		, PALETTERGB (0, 0, 128) },
  {"CornflowerBlue"	, PALETTERGB (100, 149, 237) },
  {"DarkSlateBlue"	, PALETTERGB (72, 61, 139) },
  {"SlateBlue"		, PALETTERGB (106, 90, 205) },
  {"MediumSlateBlue"	, PALETTERGB (123, 104, 238) },
  {"LightSlateBlue"	, PALETTERGB (132, 112, 255) },
  {"MediumBlue"		, PALETTERGB (0, 0, 205) },
  {"RoyalBlue"		, PALETTERGB (65, 105, 225) },
  {"blue"		, PALETTERGB (0, 0, 255) },
  {"DodgerBlue"		, PALETTERGB (30, 144, 255) },
  {"DeepSkyBlue"	, PALETTERGB (0, 191, 255) },
  {"SkyBlue"		, PALETTERGB (135, 206, 235) },
  {"LightSkyBlue"	, PALETTERGB (135, 206, 250) },
  {"SteelBlue"		, PALETTERGB (70, 130, 180) },
  {"LightSteelBlue"	, PALETTERGB (176, 196, 222) },
  {"LightBlue"		, PALETTERGB (173, 216, 230) },
  {"PowderBlue"		, PALETTERGB (176, 224, 230) },
  {"PaleTurquoise"	, PALETTERGB (175, 238, 238) },
  {"DarkTurquoise"	, PALETTERGB (0, 206, 209) },
  {"MediumTurquoise"	, PALETTERGB (72, 209, 204) },
  {"turquoise"		, PALETTERGB (64, 224, 208) },
  {"cyan"		, PALETTERGB (0, 255, 255) },
  {"LightCyan"		, PALETTERGB (224, 255, 255) },
  {"CadetBlue"		, PALETTERGB (95, 158, 160) },
  {"MediumAquamarine"	, PALETTERGB (102, 205, 170) },
  {"aquamarine"		, PALETTERGB (127, 255, 212) },
  {"DarkGreen"		, PALETTERGB (0, 128, 0) },	/* Adjusted */
  {"DarkOliveGreen"	, PALETTERGB (85, 107, 47) },
  {"DarkSeaGreen"	, PALETTERGB (143, 188, 143) },
  {"SeaGreen"		, PALETTERGB (46, 139, 87) },
  {"MediumSeaGreen"	, PALETTERGB (60, 179, 113) },
  {"LightSeaGreen"	, PALETTERGB (32, 178, 170) },
  {"PaleGreen"		, PALETTERGB (152, 251, 152) },
  {"SpringGreen"	, PALETTERGB (0, 255, 127) },
  {"LawnGreen"		, PALETTERGB (124, 252, 0) },
  {"green"		, PALETTERGB (0, 255, 0) },
  {"chartreuse"		, PALETTERGB (127, 255, 0) },
  {"MediumSpringGreen"	, PALETTERGB (0, 250, 154) },
  {"GreenYellow"	, PALETTERGB (173, 255, 47) },
  {"LimeGreen"		, PALETTERGB (50, 205, 50) },
  {"YellowGreen"	, PALETTERGB (154, 205, 50) },
  {"ForestGreen"	, PALETTERGB (34, 139, 34) },
  {"OliveDrab"		, PALETTERGB (107, 142, 35) },
  {"DarkKhaki"		, PALETTERGB (189, 183, 107) },
  {"khaki"		, PALETTERGB (240, 230, 140) },
  {"PaleGoldenrod"	, PALETTERGB (238, 232, 170) },
  {"LightGoldenrodYellow", PALETTERGB (250, 250, 210) },
  {"LightYellow"	, PALETTERGB (255, 255, 224) },
  {"LightYellow"	, PALETTERGB (255, 255, 225) },	/* Adjusted */
  {"yellow"		, PALETTERGB (255, 255, 0) },
  {"gold"		, PALETTERGB (255, 215, 0) },
  {"LightGoldenrod"	, PALETTERGB (238, 221, 130) },
  {"goldenrod"		, PALETTERGB (218, 165, 32) },
  {"DarkGoldenrod"	, PALETTERGB (184, 134, 11) },
  {"RosyBrown"		, PALETTERGB (188, 143, 143) },
  {"IndianRed"		, PALETTERGB (205, 92, 92) },
  {"SaddleBrown"	, PALETTERGB (139, 69, 19) },
  {"sienna"		, PALETTERGB (160, 82, 45) },
  {"peru"		, PALETTERGB (205, 133, 63) },
  {"burlywood"		, PALETTERGB (222, 184, 135) },
  {"beige"		, PALETTERGB (245, 245, 220) },
  {"wheat"		, PALETTERGB (245, 222, 179) },
  {"SandyBrown"		, PALETTERGB (244, 164, 96) },
  {"tan"		, PALETTERGB (210, 180, 140) },
  {"chocolate"		, PALETTERGB (210, 105, 30) },
  {"firebrick"		, PALETTERGB (178, 34, 34) },
  {"brown"		, PALETTERGB (165, 42, 42) },
  {"DarkSalmon"		, PALETTERGB (233, 150, 122) },
  {"salmon"		, PALETTERGB (250, 128, 114) },
  {"LightSalmon"	, PALETTERGB (255, 160, 122) },
  {"orange"		, PALETTERGB (255, 165, 0) },
  {"DarkOrange"		, PALETTERGB (255, 140, 0) },
  {"coral"		, PALETTERGB (255, 127, 80) },
  {"LightCoral"		, PALETTERGB (240, 128, 128) },
  {"tomato"		, PALETTERGB (255, 99, 71) },
  {"OrangeRed"		, PALETTERGB (255, 69, 0) },
  {"red"		, PALETTERGB (255, 0, 0) },
  {"HotPink"		, PALETTERGB (255, 105, 180) },
  {"DeepPink"		, PALETTERGB (255, 20, 147) },
  {"pink"		, PALETTERGB (255, 192, 203) },
  {"LightPink"		, PALETTERGB (255, 182, 193) },
  {"PaleVioletRed"	, PALETTERGB (219, 112, 147) },
  {"maroon"		, PALETTERGB (176, 48, 96) },
  {"MediumVioletRed"	, PALETTERGB (199, 21, 133) },
  {"VioletRed"		, PALETTERGB (208, 32, 144) },
  {"magenta"		, PALETTERGB (255, 0, 255) },
  {"violet"		, PALETTERGB (238, 130, 238) },
  {"plum"		, PALETTERGB (221, 160, 221) },
  {"orchid"		, PALETTERGB (218, 112, 214) },
  {"MediumOrchid"	, PALETTERGB (186, 85, 211) },
  {"DarkOrchid"		, PALETTERGB (153, 50, 204) },
  {"DarkViolet"		, PALETTERGB (148, 0, 211) },
  {"BlueViolet"		, PALETTERGB (138, 43, 226) },
  {"purple"		, PALETTERGB (160, 32, 240) },
  {"MediumPurple"	, PALETTERGB (147, 112, 219) },
  {"thistle"		, PALETTERGB (216, 191, 216) },
  {"snow1"		, PALETTERGB (255, 250, 250) },
  {"snow2"		, PALETTERGB (238, 233, 233) },
  {"snow3"		, PALETTERGB (205, 201, 201) },
  {"snow4"		, PALETTERGB (139, 137, 137) },
  {"seashell1"		, PALETTERGB (255, 245, 238) },
  {"seashell2"		, PALETTERGB (238, 229, 222) },
  {"seashell3"		, PALETTERGB (205, 197, 191) },
  {"seashell4"		, PALETTERGB (139, 134, 130) },
  {"AntiqueWhite1"	, PALETTERGB (255, 239, 219) },
  {"AntiqueWhite2"	, PALETTERGB (238, 223, 204) },
  {"AntiqueWhite3"	, PALETTERGB (205, 192, 176) },
  {"AntiqueWhite4"	, PALETTERGB (139, 131, 120) },
  {"bisque1"		, PALETTERGB (255, 228, 196) },
  {"bisque2"		, PALETTERGB (238, 213, 183) },
  {"bisque3"		, PALETTERGB (205, 183, 158) },
  {"bisque4"		, PALETTERGB (139, 125, 107) },
  {"PeachPuff1"		, PALETTERGB (255, 218, 185) },
  {"PeachPuff2"		, PALETTERGB (238, 203, 173) },
  {"PeachPuff3"		, PALETTERGB (205, 175, 149) },
  {"PeachPuff4"		, PALETTERGB (139, 119, 101) },
  {"NavajoWhite1"	, PALETTERGB (255, 222, 173) },
  {"NavajoWhite2"	, PALETTERGB (238, 207, 161) },
  {"NavajoWhite3"	, PALETTERGB (205, 179, 139) },
  {"NavajoWhite4"	, PALETTERGB (139, 121, 94) },
  {"LemonChiffon1"	, PALETTERGB (255, 250, 205) },
  {"LemonChiffon2"	, PALETTERGB (238, 233, 191) },
  {"LemonChiffon3"	, PALETTERGB (205, 201, 165) },
  {"LemonChiffon4"	, PALETTERGB (139, 137, 112) },
  {"cornsilk1"		, PALETTERGB (255, 248, 220) },
  {"cornsilk2"		, PALETTERGB (238, 232, 205) },
  {"cornsilk3"		, PALETTERGB (205, 200, 177) },
  {"cornsilk4"		, PALETTERGB (139, 136, 120) },
  {"ivory1"		, PALETTERGB (255, 255, 240) },
  {"ivory2"		, PALETTERGB (240, 240, 208) },	/* Adjusted */
  {"ivory3"		, PALETTERGB (205, 205, 193) },
  {"ivory4"		, PALETTERGB (139, 139, 131) },
  {"honeydew1"		, PALETTERGB (240, 255, 240) },
  {"honeydew2"		, PALETTERGB (224, 238, 224) },
  {"honeydew3"		, PALETTERGB (193, 205, 193) },
  {"honeydew4"		, PALETTERGB (131, 139, 131) },
  {"LavenderBlush1"	, PALETTERGB (255, 240, 245) },
  {"LavenderBlush2"	, PALETTERGB (238, 224, 229) },
  {"LavenderBlush3"	, PALETTERGB (205, 193, 197) },
  {"LavenderBlush4"	, PALETTERGB (139, 131, 134) },
  {"MistyRose1"		, PALETTERGB (255, 228, 225) },
  {"MistyRose2"		, PALETTERGB (238, 213, 210) },
  {"MistyRose3"		, PALETTERGB (205, 183, 181) },
  {"MistyRose4"		, PALETTERGB (139, 125, 123) },
  {"azure1"		, PALETTERGB (240, 255, 255) },
  {"azure2"		, PALETTERGB (224, 238, 238) },
  {"azure3"		, PALETTERGB (193, 205, 205) },
  {"azure4"		, PALETTERGB (131, 139, 139) },
  {"SlateBlue1"		, PALETTERGB (131, 111, 255) },
  {"SlateBlue2"		, PALETTERGB (122, 103, 238) },
  {"SlateBlue3"		, PALETTERGB (105, 89, 205) },
  {"SlateBlue4"		, PALETTERGB (71, 60, 139) },
  {"RoyalBlue1"		, PALETTERGB (72, 118, 255) },
  {"RoyalBlue2"		, PALETTERGB (67, 110, 238) },
  {"RoyalBlue3"		, PALETTERGB (58, 95, 205) },
  {"RoyalBlue4"		, PALETTERGB (39, 64, 139) },
  {"blue1"		, PALETTERGB (0, 0, 255) },
  {"blue2"		, PALETTERGB (0, 0, 238) },
  {"blue3"		, PALETTERGB (0, 0, 205) },
  {"blue4"		, PALETTERGB (0, 0, 139) },
  {"DodgerBlue1"	, PALETTERGB (30, 144, 255) },
  {"DodgerBlue2"	, PALETTERGB (28, 134, 238) },
  {"DodgerBlue3"	, PALETTERGB (24, 116, 205) },
  {"DodgerBlue4"	, PALETTERGB (16, 78, 139) },
  {"SteelBlue1"		, PALETTERGB (99, 184, 255) },
  {"SteelBlue2"		, PALETTERGB (92, 172, 238) },
  {"SteelBlue3"		, PALETTERGB (79, 148, 205) },
  {"SteelBlue4"		, PALETTERGB (54, 100, 139) },
  {"DeepSkyBlue1"	, PALETTERGB (0, 191, 255) },
  {"DeepSkyBlue2"	, PALETTERGB (0, 178, 238) },
  {"DeepSkyBlue3"	, PALETTERGB (0, 154, 205) },
  {"DeepSkyBlue4"	, PALETTERGB (0, 104, 139) },
  {"SkyBlue1"		, PALETTERGB (135, 206, 255) },
  {"SkyBlue2"		, PALETTERGB (126, 192, 238) },
  {"SkyBlue3"		, PALETTERGB (108, 166, 205) },
  {"SkyBlue4"		, PALETTERGB (74, 112, 139) },
  {"LightSkyBlue1"	, PALETTERGB (176, 226, 255) },
  {"LightSkyBlue2"	, PALETTERGB (164, 211, 238) },
  {"LightSkyBlue3"	, PALETTERGB (141, 182, 205) },
  {"LightSkyBlue4"	, PALETTERGB (96, 123, 139) },
  {"SlateGray1"		, PALETTERGB (198, 226, 255) },
  {"SlateGray2"		, PALETTERGB (185, 211, 238) },
  {"SlateGray3"		, PALETTERGB (159, 182, 205) },
  {"SlateGray4"		, PALETTERGB (108, 123, 139) },
  {"LightSteelBlue1"	, PALETTERGB (202, 225, 255) },
  {"LightSteelBlue2"	, PALETTERGB (188, 210, 238) },
  {"LightSteelBlue3"	, PALETTERGB (162, 181, 205) },
  {"LightSteelBlue4"	, PALETTERGB (110, 123, 139) },
  {"LightBlue1"		, PALETTERGB (191, 239, 255) },
  {"LightBlue2"		, PALETTERGB (178, 223, 238) },
  {"LightBlue3"		, PALETTERGB (154, 192, 205) },
  {"LightBlue4"		, PALETTERGB (104, 131, 139) },
  {"LightCyan1"		, PALETTERGB (224, 255, 255) },
  {"LightCyan2"		, PALETTERGB (209, 238, 238) },
  {"LightCyan3"		, PALETTERGB (180, 205, 205) },
  {"LightCyan4"		, PALETTERGB (122, 139, 139) },
  {"PaleTurquoise1"	, PALETTERGB (187, 255, 255) },
  {"PaleTurquoise2"	, PALETTERGB (174, 238, 238) },
  {"PaleTurquoise3"	, PALETTERGB (150, 205, 205) },
  {"PaleTurquoise4"	, PALETTERGB (102, 139, 139) },
  {"CadetBlue1"		, PALETTERGB (152, 245, 255) },
  {"CadetBlue2"		, PALETTERGB (144, 220, 240) },	/* Adjusted */
  {"CadetBlue3"		, PALETTERGB (122, 197, 205) },
  {"CadetBlue4"		, PALETTERGB (83, 134, 139) },
  {"turquoise1"		, PALETTERGB (0, 245, 255) },
  {"turquoise2"		, PALETTERGB (0, 229, 238) },
  {"turquoise3"		, PALETTERGB (0, 197, 205) },
  {"turquoise4"		, PALETTERGB (0, 134, 139) },
  {"cyan1"		, PALETTERGB (0, 255, 255) },
  {"cyan2"		, PALETTERGB (0, 238, 238) },
  {"cyan3"		, PALETTERGB (0, 205, 205) },
  {"cyan4"		, PALETTERGB (0, 139, 139) },
  {"DarkSlateGray1"	, PALETTERGB (151, 255, 255) },
  {"DarkSlateGray2"	, PALETTERGB (141, 238, 238) },
  {"DarkSlateGray3"	, PALETTERGB (121, 205, 205) },
  {"DarkSlateGray4"	, PALETTERGB (82, 139, 139) },
  {"aquamarine1"	, PALETTERGB (127, 255, 212) },
  {"aquamarine2"	, PALETTERGB (118, 238, 198) },
  {"aquamarine3"	, PALETTERGB (102, 205, 170) },
  {"aquamarine4"	, PALETTERGB (69, 139, 116) },
  {"DarkSeaGreen1"	, PALETTERGB (193, 255, 193) },
  {"DarkSeaGreen2"	, PALETTERGB (180, 238, 180) },
  {"DarkSeaGreen3"	, PALETTERGB (155, 205, 155) },
  {"DarkSeaGreen4"	, PALETTERGB (105, 139, 105) },
  {"SeaGreen1"		, PALETTERGB (84, 255, 159) },
  {"SeaGreen2"		, PALETTERGB (78, 238, 148) },
  {"SeaGreen3"		, PALETTERGB (67, 205, 128) },
  {"SeaGreen4"		, PALETTERGB (46, 139, 87) },
  {"PaleGreen1"		, PALETTERGB (154, 255, 154) },
  {"PaleGreen2"		, PALETTERGB (144, 238, 144) },
  {"PaleGreen3"		, PALETTERGB (124, 205, 124) },
  {"PaleGreen4"		, PALETTERGB (84, 139, 84) },
  {"SpringGreen1"	, PALETTERGB (0, 255, 127) },
  {"SpringGreen2"	, PALETTERGB (0, 238, 118) },
  {"SpringGreen3"	, PALETTERGB (0, 205, 102) },
  {"SpringGreen4"	, PALETTERGB (0, 139, 69) },
  {"green1"		, PALETTERGB (0, 255, 0) },
  {"green2"		, PALETTERGB (0, 238, 0) },
  {"green3"		, PALETTERGB (0, 205, 0) },
  {"green4"		, PALETTERGB (0, 139, 0) },
  {"chartreuse1"	, PALETTERGB (127, 255, 0) },
  {"chartreuse2"	, PALETTERGB (118, 238, 0) },
  {"chartreuse3"	, PALETTERGB (102, 205, 0) },
  {"chartreuse4"	, PALETTERGB (69, 139, 0) },
  {"OliveDrab1"		, PALETTERGB (192, 255, 62) },
  {"OliveDrab2"		, PALETTERGB (179, 238, 58) },
  {"OliveDrab3"		, PALETTERGB (154, 205, 50) },
  {"OliveDrab4"		, PALETTERGB (105, 139, 34) },
  {"DarkOliveGreen1"	, PALETTERGB (202, 255, 112) },
  {"DarkOliveGreen2"	, PALETTERGB (188, 238, 104) },
  {"DarkOliveGreen3"	, PALETTERGB (162, 205, 90) },
  {"DarkOliveGreen4"	, PALETTERGB (110, 139, 61) },
  {"khaki1"		, PALETTERGB (255, 246, 143) },
  {"khaki2"		, PALETTERGB (238, 230, 133) },
  {"khaki3"		, PALETTERGB (205, 198, 115) },
  {"khaki4"		, PALETTERGB (139, 134, 78) },
  {"LightGoldenrod1"	, PALETTERGB (255, 236, 139) },
  {"LightGoldenrod2"	, PALETTERGB (238, 220, 130) },
  {"LightGoldenrod3"	, PALETTERGB (205, 190, 112) },
  {"LightGoldenrod4"	, PALETTERGB (139, 129, 76) },
  {"LightYellow1"	, PALETTERGB (255, 255, 224) },
  {"LightYellow2"	, PALETTERGB (238, 238, 209) },
  {"LightYellow3"	, PALETTERGB (205, 205, 180) },
  {"LightYellow4"	, PALETTERGB (139, 139, 122) },
  {"yellow1"		, PALETTERGB (255, 255, 0) },
  {"yellow2"		, PALETTERGB (238, 238, 0) },
  {"yellow3"		, PALETTERGB (205, 205, 0) },
  {"yellow4"		, PALETTERGB (139, 139, 0) },
  {"gold1"		, PALETTERGB (255, 215, 0) },
  {"gold2"		, PALETTERGB (238, 201, 0) },
  {"gold3"		, PALETTERGB (205, 173, 0) },
  {"gold4"		, PALETTERGB (139, 117, 0) },
  {"goldenrod1"		, PALETTERGB (255, 193, 37) },
  {"goldenrod2"		, PALETTERGB (238, 180, 34) },
  {"goldenrod3"		, PALETTERGB (205, 155, 29) },
  {"goldenrod4"		, PALETTERGB (139, 105, 20) },
  {"DarkGoldenrod1"	, PALETTERGB (255, 185, 15) },
  {"DarkGoldenrod2"	, PALETTERGB (238, 173, 14) },
  {"DarkGoldenrod3"	, PALETTERGB (205, 149, 12) },
  {"DarkGoldenrod4"	, PALETTERGB (139, 101, 8) },
  {"RosyBrown1"		, PALETTERGB (255, 193, 193) },
  {"RosyBrown2"		, PALETTERGB (238, 180, 180) },
  {"RosyBrown3"		, PALETTERGB (205, 155, 155) },
  {"RosyBrown4"		, PALETTERGB (139, 105, 105) },
  {"IndianRed1"		, PALETTERGB (255, 106, 106) },
  {"IndianRed2"		, PALETTERGB (238, 99, 99) },
  {"IndianRed3"		, PALETTERGB (205, 85, 85) },
  {"IndianRed4"		, PALETTERGB (139, 58, 58) },
  {"sienna1"		, PALETTERGB (255, 130, 71) },
  {"sienna2"		, PALETTERGB (238, 121, 66) },
  {"sienna3"		, PALETTERGB (205, 104, 57) },
  {"sienna4"		, PALETTERGB (139, 71, 38) },
  {"burlywood1"		, PALETTERGB (255, 211, 155) },
  {"burlywood2"		, PALETTERGB (238, 197, 145) },
  {"burlywood3"		, PALETTERGB (205, 170, 125) },
  {"burlywood4"		, PALETTERGB (139, 115, 85) },
  {"wheat1"		, PALETTERGB (255, 231, 186) },
  {"wheat2"		, PALETTERGB (238, 216, 174) },
  {"wheat3"		, PALETTERGB (205, 186, 150) },
  {"wheat4"		, PALETTERGB (139, 126, 102) },
  {"tan1"		, PALETTERGB (255, 165, 79) },
  {"tan2"		, PALETTERGB (238, 154, 73) },
  {"tan3"		, PALETTERGB (205, 133, 63) },
  {"tan4"		, PALETTERGB (139, 90, 43) },
  {"chocolate1"		, PALETTERGB (255, 127, 36) },
  {"chocolate2"		, PALETTERGB (238, 118, 33) },
  {"chocolate3"		, PALETTERGB (205, 102, 29) },
  {"chocolate4"		, PALETTERGB (139, 69, 19) },
  {"firebrick1"		, PALETTERGB (255, 48, 48) },
  {"firebrick2"		, PALETTERGB (238, 44, 44) },
  {"firebrick3"		, PALETTERGB (205, 38, 38) },
  {"firebrick4"		, PALETTERGB (139, 26, 26) },
  {"brown1"		, PALETTERGB (255, 64, 64) },
  {"brown2"		, PALETTERGB (238, 59, 59) },
  {"brown3"		, PALETTERGB (205, 51, 51) },
  {"brown4"		, PALETTERGB (139, 35, 35) },
  {"salmon1"		, PALETTERGB (255, 140, 105) },
  {"salmon2"		, PALETTERGB (238, 130, 98) },
  {"salmon3"		, PALETTERGB (205, 112, 84) },
  {"salmon4"		, PALETTERGB (139, 76, 57) },
  {"LightSalmon1"	, PALETTERGB (255, 160, 122) },
  {"LightSalmon2"	, PALETTERGB (238, 149, 114) },
  {"LightSalmon3"	, PALETTERGB (205, 129, 98) },
  {"LightSalmon4"	, PALETTERGB (139, 87, 66) },
  {"orange1"		, PALETTERGB (255, 165, 0) },
  {"orange2"		, PALETTERGB (238, 154, 0) },
  {"orange3"		, PALETTERGB (205, 133, 0) },
  {"orange4"		, PALETTERGB (139, 90, 0) },
  {"DarkOrange1"	, PALETTERGB (255, 127, 0) },
  {"DarkOrange2"	, PALETTERGB (238, 118, 0) },
  {"DarkOrange3"	, PALETTERGB (205, 102, 0) },
  {"DarkOrange4"	, PALETTERGB (139, 69, 0) },
  {"coral1"		, PALETTERGB (255, 114, 86) },
  {"coral2"		, PALETTERGB (238, 106, 80) },
  {"coral3"		, PALETTERGB (205, 91, 69) },
  {"coral4"		, PALETTERGB (139, 62, 47) },
  {"tomato1"		, PALETTERGB (255, 99, 71) },
  {"tomato2"		, PALETTERGB (238, 92, 66) },
  {"tomato3"		, PALETTERGB (205, 79, 57) },
  {"tomato4"		, PALETTERGB (139, 54, 38) },
  {"OrangeRed1"		, PALETTERGB (255, 69, 0) },
  {"OrangeRed2"		, PALETTERGB (238, 64, 0) },
  {"OrangeRed3"		, PALETTERGB (205, 55, 0) },
  {"OrangeRed4"		, PALETTERGB (139, 37, 0) },
  {"red1"		, PALETTERGB (255, 0, 0) },
  {"red2"		, PALETTERGB (238, 0, 0) },
  {"red3"		, PALETTERGB (205, 0, 0) },
  {"red4"		, PALETTERGB (139, 0, 0) },
  {"DeepPink1"		, PALETTERGB (255, 20, 147) },
  {"DeepPink2"		, PALETTERGB (238, 18, 137) },
  {"DeepPink3"		, PALETTERGB (205, 16, 118) },
  {"DeepPink4"		, PALETTERGB (139, 10, 80) },
  {"HotPink1"		, PALETTERGB (255, 110, 180) },
  {"HotPink2"		, PALETTERGB (238, 106, 167) },
  {"HotPink3"		, PALETTERGB (205, 96, 144) },
  {"HotPink4"		, PALETTERGB (139, 58, 98) },
  {"pink1"		, PALETTERGB (255, 181, 197) },
  {"pink2"		, PALETTERGB (238, 169, 184) },
  {"pink3"		, PALETTERGB (205, 145, 158) },
  {"pink4"		, PALETTERGB (139, 99, 108) },
  {"LightPink1"		, PALETTERGB (255, 174, 185) },
  {"LightPink2"		, PALETTERGB (238, 162, 173) },
  {"LightPink3"		, PALETTERGB (205, 140, 149) },
  {"LightPink4"		, PALETTERGB (139, 95, 101) },
  {"PaleVioletRed1"	, PALETTERGB (255, 130, 171) },
  {"PaleVioletRed2"	, PALETTERGB (238, 121, 159) },
  {"PaleVioletRed3"	, PALETTERGB (205, 104, 137) },
  {"PaleVioletRed4"	, PALETTERGB (139, 71, 93) },
  {"maroon1"		, PALETTERGB (255, 52, 179) },
  {"maroon2"		, PALETTERGB (238, 48, 167) },
  {"maroon3"		, PALETTERGB (205, 41, 144) },
  {"maroon4"		, PALETTERGB (139, 28, 98) },
  {"VioletRed1"		, PALETTERGB (255, 62, 150) },
  {"VioletRed2"		, PALETTERGB (238, 58, 140) },
  {"VioletRed3"		, PALETTERGB (205, 50, 120) },
  {"VioletRed4"		, PALETTERGB (139, 34, 82) },
  {"magenta1"		, PALETTERGB (255, 0, 255) },
  {"magenta2"		, PALETTERGB (238, 0, 238) },
  {"magenta3"		, PALETTERGB (205, 0, 205) },
  {"magenta4"		, PALETTERGB (139, 0, 139) },
  {"orchid1"		, PALETTERGB (255, 131, 250) },
  {"orchid2"		, PALETTERGB (238, 122, 233) },
  {"orchid3"		, PALETTERGB (205, 105, 201) },
  {"orchid4"		, PALETTERGB (139, 71, 137) },
  {"plum1"		, PALETTERGB (255, 187, 255) },
  {"plum2"		, PALETTERGB (238, 174, 238) },
  {"plum3"		, PALETTERGB (205, 150, 205) },
  {"plum4"		, PALETTERGB (139, 102, 139) },
  {"MediumOrchid1"	, PALETTERGB (224, 102, 255) },
  {"MediumOrchid2"	, PALETTERGB (209, 95, 238) },
  {"MediumOrchid3"	, PALETTERGB (180, 82, 205) },
  {"MediumOrchid4"	, PALETTERGB (122, 55, 139) },
  {"DarkOrchid1"	, PALETTERGB (191, 62, 255) },
  {"DarkOrchid2"	, PALETTERGB (178, 58, 238) },
  {"DarkOrchid3"	, PALETTERGB (154, 50, 205) },
  {"DarkOrchid4"	, PALETTERGB (104, 34, 139) },
  {"purple1"		, PALETTERGB (155, 48, 255) },
  {"purple2"		, PALETTERGB (145, 44, 238) },
  {"purple3"		, PALETTERGB (125, 38, 205) },
  {"purple4"		, PALETTERGB (85, 26, 139) },
  {"MediumPurple1"	, PALETTERGB (171, 130, 255) },
  {"MediumPurple2"	, PALETTERGB (159, 121, 238) },
  {"MediumPurple3"	, PALETTERGB (137, 104, 205) },
  {"MediumPurple4"	, PALETTERGB (93, 71, 139) },
  {"thistle1"		, PALETTERGB (255, 225, 255) },
  {"thistle2"		, PALETTERGB (238, 210, 238) },
  {"thistle3"		, PALETTERGB (205, 181, 205) },
  {"thistle4"		, PALETTERGB (139, 123, 139) },
  {"gray0"		, PALETTERGB (0, 0, 0) },
  {"grey0"		, PALETTERGB (0, 0, 0) },
  {"gray1"		, PALETTERGB (3, 3, 3) },
  {"grey1"		, PALETTERGB (3, 3, 3) },
  {"gray2"		, PALETTERGB (5, 5, 5) },
  {"grey2"		, PALETTERGB (5, 5, 5) },
  {"gray3"		, PALETTERGB (8, 8, 8) },
  {"grey3"		, PALETTERGB (8, 8, 8) },
  {"gray4"		, PALETTERGB (10, 10, 10) },
  {"grey4"		, PALETTERGB (10, 10, 10) },
  {"gray5"		, PALETTERGB (13, 13, 13) },
  {"grey5"		, PALETTERGB (13, 13, 13) },
  {"gray6"		, PALETTERGB (15, 15, 15) },
  {"grey6"		, PALETTERGB (15, 15, 15) },
  {"gray7"		, PALETTERGB (18, 18, 18) },
  {"grey7"		, PALETTERGB (18, 18, 18) },
  {"gray8"		, PALETTERGB (20, 20, 20) },
  {"grey8"		, PALETTERGB (20, 20, 20) },
  {"gray9"		, PALETTERGB (23, 23, 23) },
  {"grey9"		, PALETTERGB (23, 23, 23) },
  {"gray10"		, PALETTERGB (26, 26, 26) },
  {"grey10"		, PALETTERGB (26, 26, 26) },
  {"gray11"		, PALETTERGB (28, 28, 28) },
  {"grey11"		, PALETTERGB (28, 28, 28) },
  {"gray12"		, PALETTERGB (31, 31, 31) },
  {"grey12"		, PALETTERGB (31, 31, 31) },
  {"gray13"		, PALETTERGB (33, 33, 33) },
  {"grey13"		, PALETTERGB (33, 33, 33) },
  {"gray14"		, PALETTERGB (36, 36, 36) },
  {"grey14"		, PALETTERGB (36, 36, 36) },
  {"gray15"		, PALETTERGB (38, 38, 38) },
  {"grey15"		, PALETTERGB (38, 38, 38) },
  {"gray16"		, PALETTERGB (41, 41, 41) },
  {"grey16"		, PALETTERGB (41, 41, 41) },
  {"gray17"		, PALETTERGB (43, 43, 43) },
  {"grey17"		, PALETTERGB (43, 43, 43) },
  {"gray18"		, PALETTERGB (46, 46, 46) },
  {"grey18"		, PALETTERGB (46, 46, 46) },
  {"gray19"		, PALETTERGB (48, 48, 48) },
  {"grey19"		, PALETTERGB (48, 48, 48) },
  {"gray20"		, PALETTERGB (51, 51, 51) },
  {"grey20"		, PALETTERGB (51, 51, 51) },
  {"gray21"		, PALETTERGB (54, 54, 54) },
  {"grey21"		, PALETTERGB (54, 54, 54) },
  {"gray22"		, PALETTERGB (56, 56, 56) },
  {"grey22"		, PALETTERGB (56, 56, 56) },
  {"gray23"		, PALETTERGB (59, 59, 59) },
  {"grey23"		, PALETTERGB (59, 59, 59) },
  {"gray24"		, PALETTERGB (61, 61, 61) },
  {"grey24"		, PALETTERGB (61, 61, 61) },
  {"gray25"		, PALETTERGB (64, 64, 64) },
  {"grey25"		, PALETTERGB (64, 64, 64) },
  {"gray26"		, PALETTERGB (66, 66, 66) },
  {"grey26"		, PALETTERGB (66, 66, 66) },
  {"gray27"		, PALETTERGB (69, 69, 69) },
  {"grey27"		, PALETTERGB (69, 69, 69) },
  {"gray28"		, PALETTERGB (71, 71, 71) },
  {"grey28"		, PALETTERGB (71, 71, 71) },
  {"gray29"		, PALETTERGB (74, 74, 74) },
  {"grey29"		, PALETTERGB (74, 74, 74) },
  {"gray30"		, PALETTERGB (77, 77, 77) },
  {"grey30"		, PALETTERGB (77, 77, 77) },
  {"gray31"		, PALETTERGB (79, 79, 79) },
  {"grey31"		, PALETTERGB (79, 79, 79) },
  {"gray32"		, PALETTERGB (82, 82, 82) },
  {"grey32"		, PALETTERGB (82, 82, 82) },
  {"gray33"		, PALETTERGB (84, 84, 84) },
  {"grey33"		, PALETTERGB (84, 84, 84) },
  {"gray34"		, PALETTERGB (87, 87, 87) },
  {"grey34"		, PALETTERGB (87, 87, 87) },
  {"gray35"		, PALETTERGB (89, 89, 89) },
  {"grey35"		, PALETTERGB (89, 89, 89) },
  {"gray36"		, PALETTERGB (92, 92, 92) },
  {"grey36"		, PALETTERGB (92, 92, 92) },
  {"gray37"		, PALETTERGB (94, 94, 94) },
  {"grey37"		, PALETTERGB (94, 94, 94) },
  {"gray38"		, PALETTERGB (97, 97, 97) },
  {"grey38"		, PALETTERGB (97, 97, 97) },
  {"gray39"		, PALETTERGB (99, 99, 99) },
  {"grey39"		, PALETTERGB (99, 99, 99) },
  {"gray40"		, PALETTERGB (102, 102, 102) },
  {"grey40"		, PALETTERGB (102, 102, 102) },
  {"gray41"		, PALETTERGB (105, 105, 105) },
  {"grey41"		, PALETTERGB (105, 105, 105) },
  {"gray42"		, PALETTERGB (107, 107, 107) },
  {"grey42"		, PALETTERGB (107, 107, 107) },
  {"gray43"		, PALETTERGB (110, 110, 110) },
  {"grey43"		, PALETTERGB (110, 110, 110) },
  {"gray44"		, PALETTERGB (112, 112, 112) },
  {"grey44"		, PALETTERGB (112, 112, 112) },
  {"gray45"		, PALETTERGB (115, 115, 115) },
  {"grey45"		, PALETTERGB (115, 115, 115) },
  {"gray46"		, PALETTERGB (117, 117, 117) },
  {"grey46"		, PALETTERGB (117, 117, 117) },
  {"gray47"		, PALETTERGB (120, 120, 120) },
  {"grey47"		, PALETTERGB (120, 120, 120) },
  {"gray48"		, PALETTERGB (122, 122, 122) },
  {"grey48"		, PALETTERGB (122, 122, 122) },
  {"gray49"		, PALETTERGB (125, 125, 125) },
  {"grey49"		, PALETTERGB (125, 125, 125) },
  {"gray50"		, PALETTERGB (128, 128, 128) },	/* Adjusted */
  {"grey50"		, PALETTERGB (128, 128, 128) },	/* Adjusted */
  {"gray51"		, PALETTERGB (130, 130, 130) },
  {"grey51"		, PALETTERGB (130, 130, 130) },
  {"gray52"		, PALETTERGB (133, 133, 133) },
  {"grey52"		, PALETTERGB (133, 133, 133) },
  {"gray53"		, PALETTERGB (135, 135, 135) },
  {"grey53"		, PALETTERGB (135, 135, 135) },
  {"gray54"		, PALETTERGB (138, 138, 138) },
  {"grey54"		, PALETTERGB (138, 138, 138) },
  {"gray55"		, PALETTERGB (140, 140, 140) },
  {"grey55"		, PALETTERGB (140, 140, 140) },
  {"gray56"		, PALETTERGB (143, 143, 143) },
  {"grey56"		, PALETTERGB (143, 143, 143) },
  {"gray57"		, PALETTERGB (145, 145, 145) },
  {"grey57"		, PALETTERGB (145, 145, 145) },
  {"gray58"		, PALETTERGB (148, 148, 148) },
  {"grey58"		, PALETTERGB (148, 148, 148) },
  {"gray59"		, PALETTERGB (150, 150, 150) },
  {"grey59"		, PALETTERGB (150, 150, 150) },
  {"gray60"		, PALETTERGB (153, 153, 153) },
  {"grey60"		, PALETTERGB (153, 153, 153) },
  {"gray61"		, PALETTERGB (156, 156, 156) },
  {"grey61"		, PALETTERGB (156, 156, 156) },
  {"gray62"		, PALETTERGB (158, 158, 158) },
  {"grey62"		, PALETTERGB (158, 158, 158) },
  {"gray63"		, PALETTERGB (161, 161, 161) },
  {"grey63"		, PALETTERGB (161, 161, 161) },
  {"gray64"		, PALETTERGB (163, 163, 163) },
  {"grey64"		, PALETTERGB (163, 163, 163) },
  {"gray65"		, PALETTERGB (166, 166, 166) },
  {"grey65"		, PALETTERGB (166, 166, 166) },
  {"gray66"		, PALETTERGB (168, 168, 168) },
  {"grey66"		, PALETTERGB (168, 168, 168) },
  {"gray67"		, PALETTERGB (171, 171, 171) },
  {"grey67"		, PALETTERGB (171, 171, 171) },
  {"gray68"		, PALETTERGB (173, 173, 173) },
  {"grey68"		, PALETTERGB (173, 173, 173) },
  {"gray69"		, PALETTERGB (176, 176, 176) },
  {"grey69"		, PALETTERGB (176, 176, 176) },
  {"gray70"		, PALETTERGB (179, 179, 179) },
  {"grey70"		, PALETTERGB (179, 179, 179) },
  {"gray71"		, PALETTERGB (181, 181, 181) },
  {"grey71"		, PALETTERGB (181, 181, 181) },
  {"gray72"		, PALETTERGB (184, 184, 184) },
  {"grey72"		, PALETTERGB (184, 184, 184) },
  {"gray73"		, PALETTERGB (186, 186, 186) },
  {"grey73"		, PALETTERGB (186, 186, 186) },
  {"gray74"		, PALETTERGB (189, 189, 189) },
  {"grey74"		, PALETTERGB (189, 189, 189) },
  {"gray75"		, PALETTERGB (192, 192, 192) },	/* Adjusted */
  {"grey75"		, PALETTERGB (192, 192, 192) },	/* Adjusted */
  {"gray76"		, PALETTERGB (194, 194, 194) },
  {"grey76"		, PALETTERGB (194, 194, 194) },
  {"gray77"		, PALETTERGB (196, 196, 196) },
  {"grey77"		, PALETTERGB (196, 196, 196) },
  {"gray78"		, PALETTERGB (199, 199, 199) },
  {"grey78"		, PALETTERGB (199, 199, 199) },
  {"gray79"		, PALETTERGB (201, 201, 201) },
  {"grey79"		, PALETTERGB (201, 201, 201) },
  {"gray80"		, PALETTERGB (204, 204, 204) },
  {"grey80"		, PALETTERGB (204, 204, 204) },
  {"gray81"		, PALETTERGB (207, 207, 207) },
  {"grey81"		, PALETTERGB (207, 207, 207) },
  {"gray82"		, PALETTERGB (209, 209, 209) },
  {"grey82"		, PALETTERGB (209, 209, 209) },
  {"gray83"		, PALETTERGB (212, 212, 212) },
  {"grey83"		, PALETTERGB (212, 212, 212) },
  {"gray84"		, PALETTERGB (214, 214, 214) },
  {"grey84"		, PALETTERGB (214, 214, 214) },
  {"gray85"		, PALETTERGB (217, 217, 217) },
  {"grey85"		, PALETTERGB (217, 217, 217) },
  {"gray86"		, PALETTERGB (219, 219, 219) },
  {"grey86"		, PALETTERGB (219, 219, 219) },
  {"gray87"		, PALETTERGB (222, 222, 222) },
  {"grey87"		, PALETTERGB (222, 222, 222) },
  {"gray88"		, PALETTERGB (224, 224, 224) },
  {"grey88"		, PALETTERGB (224, 224, 224) },
  {"gray89"		, PALETTERGB (227, 227, 227) },
  {"grey89"		, PALETTERGB (227, 227, 227) },
  {"gray90"		, PALETTERGB (229, 229, 229) },
  {"grey90"		, PALETTERGB (229, 229, 229) },
  {"gray91"		, PALETTERGB (232, 232, 232) },
  {"grey91"		, PALETTERGB (232, 232, 232) },
  {"gray92"		, PALETTERGB (235, 235, 235) },
  {"grey92"		, PALETTERGB (235, 235, 235) },
  {"gray93"		, PALETTERGB (237, 237, 237) },
  {"grey93"		, PALETTERGB (237, 237, 237) },
  {"gray94"		, PALETTERGB (240, 240, 240) },
  {"grey94"		, PALETTERGB (240, 240, 240) },
  {"gray95"		, PALETTERGB (242, 242, 242) },
  {"grey95"		, PALETTERGB (242, 242, 242) },
  {"gray96"		, PALETTERGB (245, 245, 245) },
  {"grey96"		, PALETTERGB (245, 245, 245) },
  {"gray97"		, PALETTERGB (247, 247, 247) },
  {"grey97"		, PALETTERGB (247, 247, 247) },
  {"gray98"		, PALETTERGB (250, 250, 250) },
  {"grey98"		, PALETTERGB (250, 250, 250) },
  {"gray99"		, PALETTERGB (252, 252, 252) },
  {"grey99"		, PALETTERGB (252, 252, 252) },
  {"gray100"		, PALETTERGB (255, 255, 255) },
  {"grey100"		, PALETTERGB (255, 255, 255) },
  {"DarkGrey"		, PALETTERGB (169, 169, 169) },
  {"DarkGray"		, PALETTERGB (169, 169, 169) },
  {"DarkBlue"		, PALETTERGB (0, 0, 128) },	/* Adjusted == Navy */
  {"DarkCyan"		, PALETTERGB (0, 128, 128) },	/* Adjusted */
  {"DarkMagenta"	, PALETTERGB (128, 0, 128) },	/* Adjusted */
  {"DarkRed"		, PALETTERGB (128, 0, 0) },	/* Adjusted */
  {"LightGreen"		, PALETTERGB (144, 238, 144) },
  /* Added to match values in the default Windows palette: */
  {"DarkYellow"		, PALETTERGB (128, 128, 0) },
  {"PaleYellow"		, PALETTERGB (255, 255, 128) }
};


typedef struct fontmap_t
{
  const char *name;
  int value;
} fontmap_t;

/* Default weight first, preferred names listed before synonyms */
static const fontmap_t fontweight_map[] =
{
  {"Regular"		, FW_REGULAR},	/* The standard font weight */
  {"Thin"		, FW_THIN},
  {"Extra Light"	, FW_EXTRALIGHT},
  {"Ultra Light"	, FW_ULTRALIGHT},
  {"Light"		, FW_LIGHT},
  {"Normal"		, FW_NORMAL},
  {"Medium"		, FW_MEDIUM},
  {"Semi Bold"		, FW_SEMIBOLD},
  {"Demi Bold"		, FW_DEMIBOLD},
  {"Bold"		, FW_BOLD},	/* The standard bold font weight */
  {"Extra Bold"		, FW_EXTRABOLD},
  {"Ultra Bold"		, FW_ULTRABOLD},
  {"Heavy"		, FW_HEAVY},
  {"Black"		, FW_BLACK}
};

/* Default charset first, no synonyms allowed because these names are
 * matched against the names reported by win32 by match_font() */
static const fontmap_t charset_map[] =
{
  {"Western"		, ANSI_CHARSET},
  {"Symbol"		, SYMBOL_CHARSET},
  {"Shift JIS"		, SHIFTJIS_CHARSET},	/* #### Name to be verified */
  {"GB2312"		, GB2312_CHARSET},	/* #### Name to be verified */
  {"Hanguel"		, HANGEUL_CHARSET},
  {"Chinese Big 5"	, CHINESEBIG5_CHARSET},	/* #### Name to be verified */
#if (WINVER >= 0x0400)
  {"Johab"		, JOHAB_CHARSET},	/* #### Name to be verified */
  {"Hebrew"		, HEBREW_CHARSET},	/* #### Name to be verified */
  {"Arabic"		, ARABIC_CHARSET},	/* #### Name to be verified */
  {"Greek"		, GREEK_CHARSET},
  {"Turkish"		, TURKISH_CHARSET},
  {"Vietnamese"		, VIETNAMESE_CHARSET},	/* #### Name to be verified */
  {"Thai"		, THAI_CHARSET},	/* #### Name to be verified */
  {"Central European"	, EASTEUROPE_CHARSET},
  {"Cyrillic"		, RUSSIAN_CHARSET},
  {"Mac"		, MAC_CHARSET},
  {"Baltic"		, BALTIC_CHARSET},
#endif
  {"OEM/DOS"		, OEM_CHARSET}
};


/************************************************************************/
/*                               helpers                                */
/************************************************************************/

static int
hexval (char c)
{
  /* assumes ASCII and isxdigit(c) */
  if (c >= 'a')
    return c-'a' + 10;
  else if (c >= 'A')
    return c-'A' + 10;
  else
    return c-'0';
}

COLORREF
mswindows_string_to_color(const char *name)
{
  int i;

  if (*name == '#')
    {
      /* numeric names look like "#RRGGBB", "#RRRGGGBBB" or "#RRRRGGGGBBBB"
	 or "rgb:rrrr/gggg/bbbb" */
      unsigned int r, g, b;

      for (i=1; i<strlen(name); i++)
	{
	  if (!isxdigit ((int)name[i]))
	    return (COLORREF) -1;
	}
      if (strlen(name)==7)
	{
	  r = hexval (name[1]) * 16 + hexval (name[2]);
	  g = hexval (name[3]) * 16 + hexval (name[4]);
	  b = hexval (name[5]) * 16 + hexval (name[6]);
	  return (PALETTERGB (r, g, b));
	}
      else if (strlen(name)==10)
	{
	  r = hexval (name[1]) * 16 + hexval (name[2]);
	  g = hexval (name[4]) * 16 + hexval (name[5]);
	  b = hexval (name[7]) * 16 + hexval (name[8]);
	  return (PALETTERGB (r, g, b));
	}
      else if (strlen(name)==13)
	{
	  r = hexval (name[1]) * 16 + hexval (name[2]);
	  g = hexval (name[5]) * 16 + hexval (name[6]);
	  b = hexval (name[9]) * 16 + hexval (name[10]);
	  return (PALETTERGB (r, g, b));
	}
    }
  else if (!strncmp(name, "rgb:", 4))
    {
      unsigned int r,g,b;

      if (sscanf(name, "rgb:%04x/%04x/%04x", &r, &g, &b) == 3)
	{
	  int len = strlen (name);
	  if (len == 18)
	    {
	      r /= 257;
	      g /= 257;
	      b /= 257;
	    }
	  else if (len == 15)
	    {
	      r /= 17;
	      g /= 17;
	      b /= 17;
	    }
	  return (PALETTERGB (r, g, b));
	}
      else
	return (COLORREF) -1;
    }
  else if (*name)	/* Can't be an empty string */
    {
      char *nospaces = (char*) alloca (strlen (name)+1);
      char *c = nospaces;
      while (*name)
	if (*name != ' ')
	  *c++ = *name++;
	else
	  name++;
      *c = '\0';

      for (i = 0; i < countof (mswindows_X_color_map); i++)
	if (!stricmp (nospaces, mswindows_X_color_map[i].name))
	  return (mswindows_X_color_map[i].colorref);
    }
  return (COLORREF) -1;
}

Lisp_Object
mswindows_color_to_string (COLORREF color)
{
  int i;
  char buf[8];
  COLORREF pcolor = PALETTERGB (GetRValue (color), GetGValue (color),
				GetBValue (color));

  for (i=0; i < countof (mswindows_X_color_map); i++)
    if (pcolor == (mswindows_X_color_map[i].colorref))
      return  build_string (mswindows_X_color_map[i].name);

  sprintf (buf, "#%02X%02X%02X",
	   GetRValue (color), GetGValue (color), GetBValue (color));
  return build_string (buf);
}

/*
 * Returns non-zero if the two supplied font patterns match.
 * If they match and fontname is not NULL, copies the logical OR of the
 * patterns to fontname (which is assumed to be at least MSW_FONTSIZE in size).
 *
 * The patterns 'match' iff for each field that is not blank in either pattern,
 * the corresponding field in the other pattern is either identical or blank.
 */
static int
match_font (char *pattern1, char *pattern2, char *fontname)
{
  char *c1=pattern1, *c2=pattern2, *e1=0, *e2=0;
  int i;

  if (fontname)
    fontname[0] = '\0';

  for (i=0; i<5; i++)
    {
      if (c1 && (e1 = strchr (c1, ':')))
        *(e1) = '\0';
      if (c2 && (e2 = strchr (c2, ':')))
        *(e2) = '\0';

      if (c1 && c1[0]!='\0')
        {
	  if (c2 && c2[0]!='\0' && stricmp(c1, c2))
	    {
	      if (e1) *e1 = ':';
	      if (e2) *e2 = ':';
	      return 0;
	    }
	  else if (fontname)
	    strcat (strcat (fontname, c1), ":");
	}
      else if (fontname)
        {
	  if (c2 && c2[0]!='\0')
	    strcat (strcat (fontname, c2), ":");
	  else
	    strcat (fontname, ":");
	}

      if (e1) *(e1++) = ':';
      if (e2) *(e2++) = ':';
      c1=e1;
      c2=e2;
    }

  if (fontname)
    fontname[strlen (fontname) - 1] = '\0';	/* Trim trailing ':' */
  return 1;
}





/************************************************************************/
/*                                 exports                              */
/************************************************************************/

struct font_enum_t
{
  HDC hdc;
  Lisp_Object list;
};

static int CALLBACK
old_font_enum_callback_2 (ENUMLOGFONT FAR *lpelfe, NEWTEXTMETRIC FAR *lpntme,
			  int FontType, struct font_enum_t *font_enum)
{
  char fontname[MSW_FONTSIZE];
  Lisp_Object fontname_lispstr;
  int i;

  /*
   * The enumerated font weights are not to be trusted because:
   *  a) lpelfe->elfStyle is only filled in for TrueType fonts.
   *  b) Not all Bold and Italic styles of all fonts (including some Vector,
   *     Truetype and Raster fonts) are enumerated.
   * I guess that fonts for which Bold and Italic styles are generated
   * 'on-the-fly' are not enumerated. It would be overly restrictive to
   * disallow Bold And Italic weights for these fonts, so we just leave
   * weights unspecified. This means that we have to weed out duplicates of
   * those fonts that do get enumerated with different weights.
   */
  if (FontType == 0 /*vector*/ || FontType == TRUETYPE_FONTTYPE)
    /* Scalable, so leave pointsize blank */
    sprintf (fontname, "%s::::", lpelfe->elfLogFont.lfFaceName);
  else
    /* Formula for pointsize->height from LOGFONT docs in Platform SDK */
    sprintf (fontname, "%s::%d::", lpelfe->elfLogFont.lfFaceName,
	     MulDiv (lpntme->tmHeight - lpntme->tmInternalLeading,
	             72, GetDeviceCaps (font_enum->hdc, LOGPIXELSY)));

  /*
   * The enumerated font character set strings are not to be trusted because
   * lpelfe->elfScript is returned in the host language and not in English.
   * We can't know a priori the translations of "Western", "Central European"
   * etc into the host language, so we must use English. The same argument
   * applies to the font weight string when matching fonts.
   */
  for (i=0; i<countof (charset_map); i++)
    if (lpelfe->elfLogFont.lfCharSet == charset_map[i].value)
      {
	strcat (fontname, charset_map[i].name);
	break;
      }
  if (i==countof (charset_map))
    strcpy (fontname, charset_map[0].name);

  /* Add the font name to the list if not already there */
  fontname_lispstr = build_string (fontname);
  if (NILP (memq_no_quit (fontname_lispstr, font_enum->list)))
    font_enum->list = Fcons (fontname_lispstr, font_enum->list);

  return 1;
}

static int CALLBACK
old_font_enum_callback_1 (ENUMLOGFONT FAR *lpelfe, NEWTEXTMETRIC FAR *lpntme,
			  int FontType, struct font_enum_t *font_enum)
{
  /* This function gets called once per facename per character set.
   * We call a second callback to enumerate the fonts in each facename */
  return EnumFontFamilies (font_enum->hdc, lpelfe->elfLogFont.lfFaceName,
			   (FONTENUMPROC) old_font_enum_callback_2,
			   (LPARAM) font_enum);
}

static int CALLBACK
font_enum_callback_2 (ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme,
		      int FontType, struct font_enum_t *font_enum)
{
  char fontname[MSW_FONTSIZE];
  Lisp_Object fontname_lispstr;
  int i;

  /*
   * The enumerated font weights are not to be trusted because:
   *  a) lpelfe->elfStyle is only filled in for TrueType fonts.
   *  b) Not all Bold and Italic styles of all fonts (including some Vector,
   *     Truetype and Raster fonts) are enumerated.
   * I guess that fonts for which Bold and Italic styles are generated
   * 'on-the-fly' are not enumerated. It would be overly restrictive to
   * disallow Bold And Italic weights for these fonts, so we just leave
   * weights unspecified. This means that we have to weed out duplicates of
   * those fonts that do get enumerated with different weights.
   */
  if (FontType == 0 /*vector*/ || FontType == TRUETYPE_FONTTYPE)
    /* Scalable, so leave pointsize blank */
    sprintf (fontname, "%s::::", lpelfe->elfLogFont.lfFaceName);
  else
    /* Formula for pointsize->height from LOGFONT docs in Platform SDK */
    sprintf (fontname, "%s::%d::", lpelfe->elfLogFont.lfFaceName,
	     MulDiv (lpntme->ntmTm.tmHeight - lpntme->ntmTm.tmInternalLeading,
	             72, GetDeviceCaps (font_enum->hdc, LOGPIXELSY)));

  /*
   * The enumerated font character set strings are not to be trusted because
   * lpelfe->elfScript is returned in the host language and not in English.
   * We can't know a priori the translations of "Western", "Central European"
   * etc into the host language, so we must use English. The same argument
   * applies to the font weight string when matching fonts.
   */
  for (i=0; i<countof (charset_map); i++)
    if (lpelfe->elfLogFont.lfCharSet == charset_map[i].value)
      {
	strcat (fontname, charset_map[i].name);
	break;
      }
  if (i==countof (charset_map))
    strcpy (fontname, charset_map[0].name);

  /* Add the font name to the list if not already there */
  fontname_lispstr = build_string (fontname);
  if (NILP (memq_no_quit (fontname_lispstr, font_enum->list)))
    font_enum->list = Fcons (fontname_lispstr, font_enum->list);

  return 1;
}

static int CALLBACK
font_enum_callback_1 (ENUMLOGFONTEX *lpelfe, NEWTEXTMETRICEX *lpntme,
		      int FontType, struct font_enum_t *font_enum)
{
  /* This function gets called once per facename per character set.
   * We call a second callback to enumerate the fonts in each facename */
  return xEnumFontFamiliesExA (font_enum->hdc, &lpelfe->elfLogFont,
			       (FONTENUMPROC) font_enum_callback_2,
			       (LPARAM) font_enum, 0);
}

/*
 * Enumerate the available on the HDC fonts and return a list of string
 * font names.
 */
Lisp_Object
mswindows_enumerate_fonts (HDC hdc)
{
  /* This cannot CG */
  LOGFONT logfont;
  struct font_enum_t font_enum;

  assert (hdc!=NULL);
  logfont.lfCharSet = DEFAULT_CHARSET;
  logfont.lfFaceName[0] = '\0';
  logfont.lfPitchAndFamily = DEFAULT_PITCH;
  font_enum.hdc = hdc;
  font_enum.list = Qnil;
  if (xEnumFontFamiliesExA)
    xEnumFontFamiliesExA (hdc, &logfont, (FONTENUMPROC) font_enum_callback_1,
			  (LPARAM) (&font_enum), 0);
  else /* NT 3.5x */
    EnumFontFamilies (hdc, 0, (FONTENUMPROC) old_font_enum_callback_1,
		      (LPARAM) (&font_enum));

  return font_enum.list;
}

static HFONT
mswindows_create_font_variant (Lisp_Font_Instance* f,
			       int under, int strike)
{
  /* Cannot GC */

  LOGFONT lf;
  HFONT hfont;

  assert (FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, under, strike) == NULL);

  if (GetObject (FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, 0, 0),
		 sizeof (lf), (void*) &lf) == 0)
    {
      hfont = MSWINDOWS_BAD_HFONT;
    }
  else
    {
      lf.lfUnderline = under;
      lf.lfStrikeOut = strike;

      hfont = CreateFontIndirect (&lf);
      if (hfont == NULL)
	hfont = MSWINDOWS_BAD_HFONT;
    }

  FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, under, strike) = hfont;
  return hfont;
}

HFONT
mswindows_get_hfont (Lisp_Font_Instance* f,
		     int under, int strike)
{
  /* Cannot GC */
  HFONT hfont = FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, under, strike);

  if (hfont == NULL)
    hfont = mswindows_create_font_variant (f, under, strike);

  /* If strikeout/underline variant of the font could not be
     created, then use the base version of the font */
  if (hfont == MSWINDOWS_BAD_HFONT)
    hfont = FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f, 0, 0);

  assert (hfont != NULL && hfont != MSWINDOWS_BAD_HFONT);

  return hfont;
}

/************************************************************************/
/*                               methods                                */
/************************************************************************/

static int
mswindows_initialize_color_instance (Lisp_Color_Instance *c, Lisp_Object name,
			       Lisp_Object device, Error_behavior errb)
{
  const char *extname;
  COLORREF color;

  TO_EXTERNAL_FORMAT (LISP_STRING, name,
		      C_STRING_ALLOCA, extname,
		      Qctext);
  color = mswindows_string_to_color(extname);
  if (color != -1)
    {
      c->data = xnew (struct mswindows_color_instance_data);
      COLOR_INSTANCE_MSWINDOWS_COLOR (c) = color;
      return 1;
    }
  maybe_signal_simple_error ("Unrecognized color", name, Qcolor, errb);
  return(0);
}

#if 0
static void
mswindows_mark_color_instance (Lisp_Color_Instance *c)
{
}
#endif

static void
mswindows_print_color_instance (Lisp_Color_Instance *c,
				Lisp_Object printcharfun,
				int escapeflag)
{
  char buf[32];
  COLORREF color = COLOR_INSTANCE_MSWINDOWS_COLOR (c);
  sprintf (buf, " %06ld=(%04X,%04X,%04X)", color & 0xffffff,
	   GetRValue(color)*257, GetGValue(color)*257, GetBValue(color)*257);
  write_c_string (buf, printcharfun);
}

static void
mswindows_finalize_color_instance (Lisp_Color_Instance *c)
{
  if (c->data)
    {
      xfree (c->data);
      c->data = 0;
    }
}

static int
mswindows_color_instance_equal (Lisp_Color_Instance *c1,
				Lisp_Color_Instance *c2,
				int depth)
{
  return (COLOR_INSTANCE_MSWINDOWS_COLOR(c1) == COLOR_INSTANCE_MSWINDOWS_COLOR(c2));
}

static unsigned long
mswindows_color_instance_hash (Lisp_Color_Instance *c, int depth)
{
  return (unsigned long) COLOR_INSTANCE_MSWINDOWS_COLOR(c);
}

static Lisp_Object
mswindows_color_instance_rgb_components (Lisp_Color_Instance *c)
{
  COLORREF color = COLOR_INSTANCE_MSWINDOWS_COLOR (c);
  return list3 (make_int (GetRValue (color) * 257),
		make_int (GetGValue (color) * 257),
		make_int (GetBValue (color) * 257));
}

static int
mswindows_valid_color_name_p (struct device *d, Lisp_Object color)
{
  const char *extname;

  TO_EXTERNAL_FORMAT (LISP_STRING, color,
		      C_STRING_ALLOCA, extname,
		      Qctext);
  return (mswindows_string_to_color(extname)!=-1);
}



static void
mswindows_finalize_font_instance (Lisp_Font_Instance *f);

/*
 * This is a work horse for both mswindows_initialize_font_instance and
 * msprinter_initialize_font_instance.
 */
static int
initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
			  Lisp_Object device_font_list, HDC hdc,
			  Error_behavior errb)
{
  const char *extname;
  LOGFONT logfont;
  int fields, i;
  int pt;
  char fontname[LF_FACESIZE], weight[LF_FACESIZE], *style, points[8];
  char effects[LF_FACESIZE], charset[LF_FACESIZE];
  char *c;
  HFONT hfont, hfont2;
  TEXTMETRIC metrics;

  extname = XSTRING_DATA (name);

  /*
   * mswindows fonts look like:
   *	fontname[:[weight ][style][:pointsize[:effects]]][:charset]
   * The font name field shouldn't be empty.
   *
   * ie:
   *	Lucida Console:Regular:10
   * minimal:
   *	Courier New
   * maximal:
   *	Courier New:Bold Italic:10:underline strikeout:western
   */

  fields = sscanf (extname, "%31[^:]:%31[^:]:%7[^:]:%31[^:]:%31s",
		   fontname, weight, points, effects, charset);

  /* This function is implemented in a fairly ad-hoc manner.
   * The general idea is to validate and canonicalize each of the above fields
   * at the same time as we build up the win32 LOGFONT structure. This enables
   * us to use match_font() on a canonicalized font string to check the
   * availability of the requested font */

  if (fields < 0)
  {
    maybe_signal_simple_error ("Invalid font", name, Qfont, errb);
    return (0);
  }

  if (fields>0 && strlen(fontname))
  {
    strncpy (logfont.lfFaceName, fontname, LF_FACESIZE);
    logfont.lfFaceName[LF_FACESIZE-1] = 0;
  }
  else
  {
    maybe_signal_simple_error ("Must specify a font name", name, Qfont, errb);
    return (0);
  }

  /* weight */
  if (fields < 2)
    strcpy (weight, fontweight_map[0].name);

  /* Maybe split weight into weight and style */
  if ((c=strchr(weight, ' ')))
  {
    *c = '\0';
    style = c+1;
  }
  else
    style = NULL;

  for (i=0; i<countof (fontweight_map); i++)
    if (!stricmp (weight, fontweight_map[i].name))
      {
	logfont.lfWeight = fontweight_map[i].value;
	break;
      }
  if (i == countof (fontweight_map))	/* No matching weight */
    {
      if (!style)
	{
	  logfont.lfWeight = FW_REGULAR;
	  style = weight;	/* May have specified style without weight */
	}
      else
	{
	  maybe_signal_simple_error ("Invalid font weight", name, Qfont, errb);
	  return (0);
	}
    }

  if (style)
    {
      /* #### what about oblique? */
      if (stricmp (style,"italic") == 0)
	logfont.lfItalic = TRUE;
      else
      {
        maybe_signal_simple_error ("Invalid font weight or style", name, Qfont, errb);
	return (0);
      }

      /* Glue weight and style together again */
      if (weight != style)
        *c = ' ';
    }
  else
    logfont.lfItalic = FALSE;

  if (fields < 3)
    pt = 10;	/* #### Should we reject strings that don't specify a size? */
  else if ((pt=atoi(points)) == 0)
    {
      maybe_signal_simple_error ("Invalid font pointsize", name, Qfont, errb);
      return (0);
    }

  /* Formula for pointsize->height from LOGFONT docs in MSVC5 Platform SDK */
  logfont.lfHeight = -MulDiv(pt, GetDeviceCaps (hdc, LOGPIXELSY), 72);
  logfont.lfWidth = 0;

  /* Effects */
  logfont.lfUnderline = FALSE;
  logfont.lfStrikeOut = FALSE;
  if (fields >= 4 && effects[0] != '\0')
    {
      char *effects2;

      /* Maybe split effects into effects and effects2 */
      if ((c=strchr (effects, ' ')))
        {
          *c = '\0';
          effects2 = c+1;
        }
      else
        effects2 = NULL;

      if (stricmp (effects, "underline") == 0)
	logfont.lfUnderline = TRUE;
      else if (stricmp (effects, "strikeout") == 0)
	logfont.lfStrikeOut = TRUE;
      else
        {
          maybe_signal_simple_error ("Invalid font effect", name, Qfont, errb);
	  return (0);
	}

      if (effects2 && effects2[0] != '\0')
	{
	  if (stricmp (effects2, "underline") == 0)
	    logfont.lfUnderline = TRUE;
	  else if (stricmp (effects2, "strikeout") == 0)
	    logfont.lfStrikeOut = TRUE;
	  else
	    {
	      maybe_signal_simple_error ("Invalid font effect", name,
					 Qfont, errb);
	      return (0);
	    }
        }

      /* Regenerate sanitised effects string */
      if (logfont.lfUnderline)
	{
	  if (logfont.lfStrikeOut)
	    strcpy (effects, "underline strikeout");
	  else
	    strcpy (effects, "underline");
	}
      else if (logfont.lfStrikeOut)
	strcpy (effects, "strikeout");
    }
  else
    effects[0] = '\0';

  /* Charset */
  /* charset can be specified even if earlier fields haven't been */
  if (fields < 5)
    {
      if ((c=strchr (extname, ':')) && (c=strchr (c+1, ':')) &&
	  (c=strchr (c+1, ':')) && (c=strchr (c+1, ':')))
	{
	  strncpy (charset, c+1, LF_FACESIZE);
	  charset[LF_FACESIZE-1] = '\0';
	}
      else
	strcpy (charset, charset_map[0].name);
    }

  for (i=0; i<countof (charset_map); i++)
    if (!stricmp (charset, charset_map[i].name))
      {
	logfont.lfCharSet = charset_map[i].value;
	break;
      }

  if (i == countof (charset_map))	/* No matching charset */
    {
      maybe_signal_simple_error ("Invalid charset", name, Qfont, errb);
      return 0;
    }

  /* Misc crud */
  logfont.lfEscapement = logfont.lfOrientation = 0;
#if 1
  logfont.lfOutPrecision = OUT_DEFAULT_PRECIS;
  logfont.lfClipPrecision = CLIP_DEFAULT_PRECIS;
  logfont.lfQuality = DEFAULT_QUALITY;
#else
  logfont.lfOutPrecision = OUT_STROKE_PRECIS;
  logfont.lfClipPrecision = CLIP_STROKE_PRECIS;
  logfont.lfQuality = PROOF_QUALITY;
#endif
  /* Default to monospaced if the specified fontname doesn't exist. */
  logfont.lfPitchAndFamily = FF_MODERN;

  /* Windows will silently substitute a default font if the fontname specifies
     a non-existent font. This is bad for screen fonts because it doesn't
     allow higher-level code to see the error and to act appropriately.
     For instance complex_vars_of_faces() sets up a fallback list of fonts
     for the default face. */

  if (!NILP (device_font_list))
    {
      Lisp_Object fonttail;
      char truename[MSW_FONTSIZE];

      sprintf (truename, "%s:%s:%d:%s:%s", fontname, weight, pt, effects, charset);
      LIST_LOOP (fonttail, device_font_list)
	{
	  if (match_font (XSTRING_DATA (XCAR (fonttail)), truename, NULL))
	    break;
	}
      if (NILP (fonttail))
	{
	  maybe_signal_simple_error ("No matching font", name, Qfont, errb);
	  return 0;
	}
    }

  if ((hfont = CreateFontIndirect(&logfont)) == NULL)
  {
    maybe_signal_simple_error ("Couldn't create font", name, Qfont, errb);
    return 0;
  }

  f->data = xnew_and_zero (struct mswindows_font_instance_data);
  FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f,0,0) = hfont;

  /* Some underlined fonts have the descent of one pixel more than their
     non-underlined counterparts. Font variants though are assumed to have
     identical metrics. So get the font metrics from the underlined variant
     of the font */
  hfont2 = mswindows_create_font_variant (f, 1, 0);
  if (hfont2 != MSWINDOWS_BAD_HFONT)
    hfont = hfont2;

  hfont2 = (HFONT) SelectObject (hdc, hfont);
  if (!hfont2)
    {
      mswindows_finalize_font_instance (f);
      maybe_signal_simple_error ("Couldn't map font", name, Qfont, errb);
      return 0;
    }
  GetTextMetrics (hdc, &metrics);
  SelectObject(hdc, hfont2);

  f->width = (unsigned short) metrics.tmAveCharWidth;
  f->height = (unsigned short) metrics.tmHeight;
  f->ascent = (unsigned short) metrics.tmAscent;
  f->descent = (unsigned short) metrics.tmDescent;
  f->proportional_p = (metrics.tmPitchAndFamily & TMPF_FIXED_PITCH);

  return 1;
}

static int
mswindows_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
				    Lisp_Object device, Error_behavior errb)
{
  HDC hdc = CreateCompatibleDC (NULL);
  Lisp_Object font_list = DEVICE_MSWINDOWS_FONTLIST (XDEVICE (device));
  int res = initialize_font_instance (f, name, font_list, hdc, errb);
  DeleteDC (hdc);
  return res;
}

static int
msprinter_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
				    Lisp_Object device, Error_behavior errb)
{
  HDC hdc = DEVICE_MSPRINTER_HDC (XDEVICE (device));
  Lisp_Object font_list = DEVICE_MSPRINTER_FONTLIST (XDEVICE (device));
  return initialize_font_instance (f, name, font_list, hdc, errb);
}

static void
mswindows_finalize_font_instance (Lisp_Font_Instance *f)
{
  int i;

  if (f->data)
    {
      for (i = 0; i < MSWINDOWS_NUM_FONT_VARIANTS; i++)
	{
	  if (FONT_INSTANCE_MSWINDOWS_HFONT_I (f, i) != NULL
	      && FONT_INSTANCE_MSWINDOWS_HFONT_I (f, i) != MSWINDOWS_BAD_HFONT)
	    DeleteObject (FONT_INSTANCE_MSWINDOWS_HFONT_I (f, i));
	}

      xfree (f->data);
      f->data = 0;
   }
}

#if 0
static void
mswindows_mark_font_instance (Lisp_Font_Instance *f)
{
}
#endif

static void
mswindows_print_font_instance (Lisp_Font_Instance *f,
			       Lisp_Object printcharfun,
			       int escapeflag)
{
  char buf[10];
  sprintf (buf, " 0x%lx",
	   (unsigned long)FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT (f,0,0));
  write_c_string (buf, printcharfun);
}

static Lisp_Object
mswindows_list_fonts (Lisp_Object pattern, Lisp_Object device)
{
  Lisp_Object fonttail, result = Qnil;
  char *extpattern;

  TO_EXTERNAL_FORMAT (LISP_STRING, pattern,
		      C_STRING_ALLOCA, extpattern,
		      Qctext);

  LIST_LOOP (fonttail, DEVICE_MSWINDOWS_FONTLIST (XDEVICE (device)))
    {
      if (match_font (XSTRING_DATA (XCAR (fonttail)), extpattern, NULL))
	result = Fcons (XCAR (fonttail), result);
    }

  return Fnreverse (result);
}

/* Fill in missing parts of a font spec. This is primarily intended as a
 * helper function for the functions below.
 * mswindows fonts look like:
 *	fontname[:[weight][ style][:pointsize[:effects]]][:charset]
 * A minimal mswindows font spec looks like:
 *	Courier New
 * A maximal mswindows font spec looks like:
 *	Courier New:Bold Italic:10:underline strikeout:Western
 * Missing parts of the font spec should be filled in with these values:
 *	Courier New:Regular:10::Western */
static Lisp_Object
mswindows_font_instance_truename (Lisp_Font_Instance *f, Error_behavior errb)
{
  int nsep=0;
  char *name = (char *) XSTRING_DATA (f->name);
  char* ptr = name;
  char* extname = (char*) alloca (strlen (name) + 19);
  strcpy (extname, name);

  while ((ptr = strchr (ptr, ':')) != 0)
    {
      ptr++;
      nsep++;
    }

  switch (nsep)
    {
    case 0:
      strcat (extname, ":Regular:10::Western");
      break;
    case 1:
      strcat (extname, ":10::Western");
      break;
    case 2:
      strcat (extname, "::Western");
      break;
    case 3:
      strcat (extname, ":Western");
      break;
    default:;
    }

  return build_ext_string (extname, Qnative);
}

#ifdef MULE

static int
mswindows_font_spec_matches_charset (struct device *d, Lisp_Object charset,
			     const Bufbyte *nonreloc, Lisp_Object reloc,
			     Bytecount offset, Bytecount length)
{
  /* #### Implement me */
  if (UNBOUNDP (charset))
    return 1;

  return 1;
}

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
mswindows_find_charset_font (Lisp_Object device, Lisp_Object font,
		     Lisp_Object charset)
{
  /* #### Implement me */
  return build_string ("Courier New:Regular:10");
}

#endif /* MULE */


/************************************************************************/
/*                             non-methods                              */
/************************************************************************/

DEFUN ("mswindows-color-list", Fmswindows_color_list, 0, 0, 0, /*
Return a list of the colors available on mswindows devices.
*/
       ())
{
  Lisp_Object result = Qnil;
  int i;

  for (i=0; i<countof (mswindows_X_color_map); i++)
    result = Fcons (build_string (mswindows_X_color_map[i].name), result);

  return Fnreverse (result);
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_mswindows (void)
{
  DEFSUBR (Fmswindows_color_list);
}

void
console_type_create_objects_mswindows (void)
{
  /* object methods */
  CONSOLE_HAS_METHOD (mswindows, initialize_color_instance);
/*  CONSOLE_HAS_METHOD (mswindows, mark_color_instance); */
  CONSOLE_HAS_METHOD (mswindows, print_color_instance);
  CONSOLE_HAS_METHOD (mswindows, finalize_color_instance);
  CONSOLE_HAS_METHOD (mswindows, color_instance_equal);
  CONSOLE_HAS_METHOD (mswindows, color_instance_hash);
  CONSOLE_HAS_METHOD (mswindows, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (mswindows, valid_color_name_p);

  CONSOLE_HAS_METHOD (mswindows, initialize_font_instance);
/*  CONSOLE_HAS_METHOD (mswindows, mark_font_instance); */
  CONSOLE_HAS_METHOD (mswindows, print_font_instance);
  CONSOLE_HAS_METHOD (mswindows, finalize_font_instance);
  CONSOLE_HAS_METHOD (mswindows, font_instance_truename);
  CONSOLE_HAS_METHOD (mswindows, list_fonts);
#ifdef MULE
  CONSOLE_HAS_METHOD (mswindows, font_spec_matches_charset);
  CONSOLE_HAS_METHOD (mswindows, find_charset_font);
#endif

  /* Printer methods - delegate most to windows methods,
     since graphical objects behave the same way. */

  CONSOLE_INHERITS_METHOD (msprinter, mswindows, initialize_color_instance);
/*  CONSOLE_INHERITS_METHOD (msprinter, mswindows, mark_color_instance); */
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, print_color_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, finalize_color_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_instance_equal);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_instance_hash);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, color_instance_rgb_components);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, valid_color_name_p);

  CONSOLE_HAS_METHOD (msprinter, initialize_font_instance);
/*  CONSOLE_INHERITS_METHOD (msprinter, mswindows, mark_font_instance); */
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, print_font_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, finalize_font_instance);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, font_instance_truename);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, list_fonts);
#ifdef MULE
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, font_spec_matches_charset);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, find_charset_font);
#endif
}

void
vars_of_objects_mswindows (void)
{
}
