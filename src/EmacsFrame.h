/* Public header for the Emacs frame widget.
   Copyright (C) 1993-1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

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

#ifndef INCLUDED_EmacsFrame_h_
#define INCLUDED_EmacsFrame_h_

#ifndef XtNminibuffer
#define XtNminibuffer "minibuffer"
#endif
#ifndef XtCMinibuffer
#define XtCMinibuffer "Minibuffer"
#endif

#ifndef XtNunsplittable
#define XtNunsplittable "unsplittable"
#endif
#ifndef XtCUnsplittable
#define XtCUnsplittable "Unsplittable"
#endif

#ifndef XtNinternalBorderWidth
#define XtNinternalBorderWidth "internalBorderWidth"
#endif
#ifndef XtCInternalBorderWidth
#define XtCInternalBorderWidth "InternalBorderWidth"
#endif

#ifndef XtNscrollBarWidth
#define XtNscrollBarWidth "scrollBarWidth"
#endif
#ifndef XtCScrollBarWidth
#define XtCScrollBarWidth "ScrollBarWidth"
#endif

#ifndef XtNscrollBarHeight
#define XtNscrollBarHeight "scrollBarHeight"
#endif
#ifndef XtCScrollBarHeight
#define XtCScrollBarHeight "ScrollBarHeight"
#endif

#ifndef XtNtopToolBarHeight
#define XtNtopToolBarHeight "topToolBarHeight"
#endif
#ifndef XtCTopToolBarHeight
#define XtCTopToolBarHeight "TopToolBarHeight"
#endif

#ifndef XtNbottomToolBarHeight
#define XtNbottomToolBarHeight "bottomToolBarHeight"
#endif
#ifndef XtCBottomToolBarHeight
#define XtCBottomToolBarHeight "BottomToolBarHeight"
#endif

#ifndef XtNleftToolBarWidth
#define XtNleftToolBarWidth "leftToolBarWidth"
#endif
#ifndef XtCLeftToolBarWidth
#define XtCLeftToolBarWidth "LeftToolBarWidth"
#endif

#ifndef XtNrightToolBarWidth
#define XtNrightToolBarWidth "rightToolBarWidth"
#endif
#ifndef XtCRightToolBarWidth
#define XtCRightToolBarWidth "RightToolBarWidth"
#endif

#ifndef XtNtopToolBarBorderWidth
#define XtNtopToolBarBorderWidth "topToolBarBorderWidth"
#endif
#ifndef XtCTopToolBarBorderWidth
#define XtCTopToolBarBorderWidth "TopToolBarBorderWidth"
#endif

#ifndef XtNbottomToolBarBorderWidth
#define XtNbottomToolBarBorderWidth "bottomToolBarBorderWidth"
#endif
#ifndef XtCBottomToolBarBorderWidth
#define XtCBottomToolBarBorderWidth "BottomToolBarBorderWidth"
#endif

#ifndef XtNleftToolBarBorderWidth
#define XtNleftToolBarBorderWidth "leftToolBarBorderWidth"
#endif
#ifndef XtCLeftToolBarBorderWidth
#define XtCLeftToolBarBorderWidth "LeftToolBarBorderWidth"
#endif

#ifndef XtNrightToolBarBorderWidth
#define XtNrightToolBarBorderWidth "rightToolBarBorderWidth"
#endif
#ifndef XtCRightToolBarBorderWidth
#define XtCRightToolBarBorderWidth "RightToolBarBorderWidth"
#endif

#ifndef XtNtopToolBarShadowColor
#define XtNtopToolBarShadowColor "topToolBarShadowColor"
#endif
#ifndef XtCTopToolBarShadowColor
#define XtCTopToolBarShadowColor "TopToolBarShadowColor"
#endif

#ifndef XtNbottomToolBarShadowColor
#define XtNbottomToolBarShadowColor "bottomToolBarShadowColor"
#endif
#ifndef XtCBottomToolBarShadowColor
#define XtCBottomToolBarShadowColor "BottomToolBarShadowColor"
#endif

#ifndef XtNbackgroundToolBarColor
#define XtNbackgroundToolBarColor "backgroundToolBarColor"
#endif
#ifndef XtCBackgroundToolBarColor
#define XtCBackgroundToolBarColor "BackgroundToolBarColor"
#endif

#ifndef XtNforegroundToolBarColor
#define XtNforegroundToolBarColor "foregroundToolBarColor"
#endif
#ifndef XtCForegroundToolBarColor
#define XtCForegroundToolBarColor "ForegroundToolBarColor"
#endif

#ifndef XtNtopToolBarShadowPixmap
#define XtNtopToolBarShadowPixmap "topToolBarShadowPixmap"
#endif
#ifndef XtCTopToolBarShadowPixmap
#define XtCTopToolBarShadowPixmap "TopToolBarShadowPixmap"
#endif

#ifndef XtNbottomToolBarShadowPixmap
#define XtNbottomToolBarShadowPixmap "bottomToolBarShadowPixmap"
#endif
#ifndef XtCBottomToolBarShadowPixmap
#define XtCBottomToolBarShadowPixmap "BottomToolBarShadowPixmap"
#endif

#ifndef XtNtoolBarShadowThickness
#define XtNtoolBarShadowThickness "toolBarShadowThickness"
#endif
#ifndef XtCToolBarShadowThickness
#define XtCToolBarShadowThickness "ToolBarShadowThickness"
#endif

#ifndef XtNscrollBarPlacement
#define XtNscrollBarPlacement "scrollBarPlacement"
#endif
#ifndef XtCScrollBarPlacement
#define XtCScrollBarPlacement "ScrollBarPlacement"
#endif
#ifndef XtRScrollBarPlacement
#define XtRScrollBarPlacement "ScrollBarPlacement"
#endif

#ifndef XtNinterline
#define XtNinterline "interline"
#endif
#ifndef XtCInterline
#define XtCInterline "Interline"
#endif

#ifndef XtNfont
#define XtNfont "font"
#endif
#ifndef XtCFont
#define XtCFont "Font"
#endif

#ifndef XtNforeground
#define XtNforeground "foreground"
#endif
#ifndef XtCForeground
#define XtCForeground "Foreground"
#endif

#ifndef XtNbackground
#define XtNbackground "background"
#endif
#ifndef XtCBackground
#define XtCBackground "Background"
#endif

#ifndef XtNiconic
#define XtNiconic "iconic"
#endif
#ifndef XtCIconic
#define XtCIconic "Iconic"
#endif

#ifndef XtNcursorColor
#define XtNcursorColor "cursorColor"
#endif
#ifndef XtCCursorColor
#define XtCCursorColor "CursorColor"
#endif

#ifndef XtNbarCursor
#define XtNbarCursor "barCursor"
#endif
#ifndef XtCBarCursor
#define XtCBarCursor "BarCursor"
#endif

#ifndef XtNvisualBell
#define XtNvisualBell "visualBell"
#endif
#ifndef XtCVisualBell
#define XtCVisualBell "VisualBell"
#endif

#ifndef XtNbellVolume
#define XtNbellVolume "bellVolume"
#endif
#ifndef XtCBellVolume
#define XtCBellVolume "BellVolume"
#endif

#ifndef XtNpointerBackground
#define XtNpointerBackground "pointerBackground"
#endif

#ifndef XtNpointerColor
#define XtNpointerColor "pointerColor"
#endif

#ifndef XtNtextPointer
#define XtNtextPointer "textPointer"
#endif

#ifndef XtNspacePointer
#define XtNspacePointer "spacePointer"
#endif

#ifndef XtNmodeLinePointer
#define XtNmodeLinePointer "modePointer"
#endif

#ifndef XtNgcPointer
#define XtNgcPointer "gcPointer"
#endif

#ifndef XtNemacsFrame
#define XtNemacsFrame "emacsFrame"
#endif
#ifndef XtCEmacsFrame
#define XtCEmacsFrame "EmacsFrame"
#endif

#ifndef XtNgeometry
#define XtNgeometry "geometry"
#endif
#ifndef XtCGeometry
#define XtCGeometry "Geometry"
#endif

#ifndef XtNinitialGeometry
#define XtNinitialGeometry "initialGeometry"
#endif
#ifndef XtCInitialGeometry
#define XtCInitialGeometry "InitialGeometry"
#endif

#ifndef XtNmenubar
#define XtNmenubar "menubar"
#endif
#ifndef XtCMenubar
#define XtCMenubar "Menubar"
#endif

#ifndef XtNinitiallyUnmapped
#define XtNinitiallyUnmapped "initiallyUnmapped"
#endif
#ifndef XtCInitiallyUnmapped
#define XtCInitiallyUnmapped "InitiallyUnmapped"
#endif

#ifndef XtNpreferredWidth
#define XtNpreferredWidth "preferredWidth"
#endif
#ifndef XtCPreferredWidth
#define XtCPreferredWidth "PreferredWidth"
#endif

#ifndef XtNpreferredHeight
#define XtNpreferredHeight "preferredHeight"
#endif
#ifndef XtCPreferredHeight
#define XtCPreferredHeight "PreferredHeight"
#endif

#ifndef XtNuseBackingStore
#define XtNuseBackingStore "useBackingStore"
#endif
#ifndef XtCUseBackingStore
#define XtCUseBackingStore "UseBackingStore"
#endif

#define XtNximStyles "ximStyles"
#define XtCXimStyles "XimStyles"
#define XtRXimStyles "XimStyles"

#define XtNximForeground "ximForeground"
#define XtNximBackground "ximBackground"

/* scrollbar placement types; like in ScrolledW.h */
#define EM_TOP		1
#define EM_BOTTOM	0
#define EM_LEFT		2
#define EM_RIGHT	0

#define XtTOP_LEFT	(EM_TOP    | EM_LEFT)
#define XtBOTTOM_LEFT	(EM_BOTTOM | EM_LEFT)
#define XtTOP_RIGHT	(EM_TOP    | EM_RIGHT)
#define XtBOTTOM_RIGHT	(EM_BOTTOM | EM_RIGHT)

/* structures */
typedef struct _EmacsFrameRec *EmacsFrame;
typedef struct _EmacsFrameClassRec *EmacsFrameClass;

extern WidgetClass emacsFrameClass;

extern struct _DisplayContext *display_context;

/* Special entrypoints */
void EmacsFrameRecomputeCellSize(Widget widget);
void EmacsFrameSetCharSize(Widget widget, int rows, int cols);

#endif				/* INCLUDED_EmacsFrame_h_ */
