/* Definitions of marked slots in buffers
   Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

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


/* Synched up with: Mule 2.0, FSF 19.30.  Split out of buffer.h.  */

/* Authorship:

   FSF: long ago (part of buffer.h).
   JWZ: separated out from buffer.h, early in Lemacs.
   XEmacs: a few other changes.
 */

/* In the declaration of the buffer structure, this file is included
   after defining MARKED_SLOT(x) to be Lisp_Object x; i.e. just a slot
   definition.  In the garbage collector this file is included after
   defining MARKED_SLOT(x) to be mark_object(buffer->x). */

#ifndef BUFFER_SLOTS_FIRST_NAME
#define BUFFER_SLOTS_FIRST_NAME name
#endif

    /* The name of this buffer.  */
MARKED_SLOT(name);

    /* The name of the file visited in this buffer, or nil.  */
MARKED_SLOT(filename);

    /* Dir for expanding relative file names.  */
MARKED_SLOT(directory);

    /* True iff this buffer has been backed up (if you write to the
       visited file and it hasn't been backed up, then a backup will
       be made).  */
    /* #### This isn't really used by the C code, so could be deleted.  */
MARKED_SLOT(backed_up);

    /* Length of file when last read or saved.
       This is not in the  struct buffer_text
       because it's not used in indirect buffers at all.  */
MARKED_SLOT(saved_size);

    /* File name used for auto-saving this buffer.
       This is not in the  struct buffer_text
       because it's not used in indirect buffers at all.  */
MARKED_SLOT(auto_save_file_name);

    /* Non-nil if buffer read-only.  */
MARKED_SLOT(read_only);

    /* "The mark".  This is a marker which may
       point into this buffer or may point nowhere.  */
MARKED_SLOT(mark);

    /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER)
       for all per-buffer variables of this buffer.
       Specifically, this lists those variables that have
       a buffer-local value in this buffer: i.e. those
       whose value does not shadow the default value.
       (Remember that for any particular variable created
       with `make-local-variable' or `make-variable-buffer-local',
       it will have a per-buffer value in some buffers and a
       default value in others.)

       Variables declared in C with DEFVAR_BUFFER_LOCAL() (i.e.
       those stored in the struct buffer) are not listed here. */
MARKED_SLOT(local_var_alist);

    /* Symbol naming major mode (eg, lisp-mode).  */
MARKED_SLOT(major_mode);

    /* Pretty name of major mode (eg, "Lisp"). */
MARKED_SLOT(mode_name);

    /* Modeline element that controls format of modeline.  */
MARKED_SLOT(modeline_format);

    /* Keys that are bound local to this buffer.  */
MARKED_SLOT(keymap);

    /* This buffer's local abbrev table.  */
MARKED_SLOT(abbrev_table);
    /* This buffer's syntax table.  */
MARKED_SLOT(syntax_table);
    /* Massaged values from the syntax table, for faster lookup. */
MARKED_SLOT(mirror_syntax_table);

#ifdef MULE
    /* This buffer's category table. */
MARKED_SLOT(category_table);
#endif				/* MULE */
#ifdef FILE_CODING
    /* This buffer's coding system. */
MARKED_SLOT(buffer_file_coding_system);
#endif
    /* Values of several buffer-local variables.

       tab-width is buffer-local so that redisplay can find it
       in buffers that are not current */
MARKED_SLOT(case_fold_search);
MARKED_SLOT(tab_width);
MARKED_SLOT(fill_column);
MARKED_SLOT(left_margin);

    /* Function to call when insert space past fill column.  */
MARKED_SLOT(auto_fill_function);

    /* Case table for case-conversion in this buffer. */
MARKED_SLOT(case_table);
    /* It contais following char-tables: */
    /* Char-table maps each char into its lower-case version.  */
    /* Char-table mapping each char to its upper-case version.  */
    /* Char-table for conversion for case-folding search.  */
    /* Char-table of equivalences for case-folding search.  */

    /* #### This ought to be a specifier: */
    /* Non-nil means do not display continuation lines.  */
MARKED_SLOT(truncate_lines);
    /* #### This ought to be a specifier: */
    /* #### Better yet, it ought to be junked.  It really sucks. */
    /* Non-nil means display ctl chars with uparrow.  */
MARKED_SLOT(ctl_arrow);
    /* #### This ought to be a specifier: */
    /* #### Better yet, it ought to be junked.  It really sucks. */
    /* Non-nil means do selective display;
       see doc string in syms_of_buffer (buffer.c) for details.  */
MARKED_SLOT(selective_display);
    /* #### This ought to be a specifier: */
    /* #### Better yet, it ought to be junked.  It really sucks. */
    /* Non-nil means show ... at end of line followed by invisible lines.  */
MARKED_SLOT(selective_display_ellipses);
    /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer.  */
    /* Unused: MARKED_SLOT (minor_modes); */
    /* t if "self-insertion" should overwrite */
MARKED_SLOT(overwrite_mode);
    /* non-nil means abbrev mode is on.  Expand abbrevs automatically.  */
MARKED_SLOT(abbrev_mode);

    /* No display table here.  It's a specifier. */
#if 0				/* FSFmacs */
    /* t means the mark and region are currently active.  */
MARKED_SLOT(mark_active);
#endif

    /* Changes in the buffer are recorded here for undo.
       t means don't record anything.
       This information belongs to the base buffer of an indirect buffer,
       But we can't store it in the  struct buffer_text
       because local variables have to be right in the  struct buffer.
       So we copy it around in set_buffer_internal.  */
MARKED_SLOT(undo_list);

    /* FSFmacs has overlay stuff here.  We have extent info elsewhere in the
       struct buffer.  */

    /* dedicated_frame in lisp */

    /* Lisp of symbols naming the file format used for visited file. */
MARKED_SLOT(file_format);

#ifdef REGION_CACHE_NEEDS_WORK
    /* True if the newline position cache and width run cache are
       enabled.  See search.c and indent.c.  */
MARKED_SLOT(cache_long_line_scans);

    /* If the width run cache is enabled, this table contains the
       character widths width_run_cache (see above) assumes.  When we
       do a thorough redisplay, we compare this against the buffer's
       current display table to see whether the display table has
       affected the widths of any characters.  If it has, we
       invalidate the width run cache, and re-initialize width_table.  */
MARKED_SLOT(width_table);
#endif				/* REGION_CACHE_NEEDS_WORK */

    /* A redundant copy of text.pt, in the form of a marker.  Every time one
       is updated, so is the other.
     */
MARKED_SLOT(point_marker);

    /* FSFmacs has pt_marker, begv_marker, zv_marker here, used for
       indirect buffers.  We don't need them because we handle these
       values directly instead of playing games with markers.  */

    /* This holds the point value before the last scroll operation.
       Explicitly setting point sets this to nil.  */
MARKED_SLOT(point_before_scroll);

    /* Truename of the visited file (via the realpath() system call),
       or nil.  */
MARKED_SLOT(file_truename);

    /* Invisibility spec of this buffer.
       t => any non-nil `invisible' property means invisible.
       A list => `invisible' property means invisible
       if it is memq in that list.  */
MARKED_SLOT(invisibility_spec);

    /* The string generated by formatting the modeline in this buffer. */
MARKED_SLOT(generated_modeline_string);

    /* A hash table that maps from a "generic extent" (an extent in
       `modeline-format') into a buffer-specific extent. */
MARKED_SLOT(modeline_extent_table);

#ifndef BUFFER_SLOTS_LAST_NAME
#define BUFFER_SLOTS_LAST_NAME modeline_extent_table
#endif

#if 0				/* FSFmacs */
    /* This is silly and stupid */
    /* These are so we don't have to recompile everything
       the next few times we add a new slot.  */
MARKED_SLOT(extra1, extra2, extra3);
#endif
