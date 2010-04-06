/* Buffer insertion/deletion and gap motion for XEmacs.
   Copyright (C) 1985-1994 Free Software Foundation, Inc.

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

/* Mostly rewritten by Ben Wing. */

#ifndef INCLUDED_insdel_h_
#define INCLUDED_insdel_h_

/************************************************************************/
/*                        changing a buffer's text                      */
/************************************************************************/

int begin_multiple_change(struct buffer *buf, Bufpos start, Bufpos end);
void end_multiple_change(struct buffer *buf, int count);

/* flags for functions below */

#define INSDEL_BEFORE_MARKERS 1
#define INSDEL_NO_LOCKING 2

Charcount buffer_insert_string_1(struct buffer *buf, Bufpos pos,
				 const Bufbyte * nonreloc, Lisp_Object reloc,
				 Bytecount offset, Bytecount length, int flags);
Charcount buffer_insert_raw_string_1(struct buffer *buf, Bufpos pos,
				     const Bufbyte * nonreloc,
				     Bytecount length, int flags);
Charcount buffer_insert_lisp_string_1(struct buffer *buf, Bufpos pos,
				      Lisp_Object str, int flags);
Charcount buffer_insert_c_string_1(struct buffer *buf, Bufpos pos,
				   const char *s, int flags);
Charcount buffer_insert_emacs_char_1(struct buffer *buf, Bufpos pos,
				     Emchar ch, int flags);
Charcount buffer_insert_c_char_1(struct buffer *buf, Bufpos pos, char c,
				 int flags);
Charcount buffer_insert_from_buffer_1(struct buffer *buf, Bufpos pos,
				      struct buffer *buf2, Bufpos pos2,
				      Charcount length, int flags);

/* Macros for insertion functions that insert at point after markers.
   All of these can GC. */

#define buffer_insert_string(buf, nonreloc, reloc, offset, length) \
  buffer_insert_string_1 (buf, -1, nonreloc, reloc, offset, length, 0)
#define buffer_insert_raw_string(buf, string, length) \
  buffer_insert_raw_string_1 (buf, -1, string, length, 0)
#define buffer_insert_c_string(buf, s) \
  buffer_insert_c_string_1 (buf, -1, s, 0)
#define buffer_insert_lisp_string(buf, str) \
  buffer_insert_lisp_string_1 (buf, -1, str, 0)
#define buffer_insert_c_char(buf, c) \
  buffer_insert_c_char_1 (buf, -1, c, 0)
#define buffer_insert_emacs_char(buf, ch) \
  buffer_insert_emacs_char_1 (buf, -1, ch, 0)
#define buffer_insert_from_buffer(buf, b, index, length) \
  buffer_insert_from_buffer_1 (buf, -1, b, index, length, 0)

void buffer_delete_range(struct buffer *buf, Bufpos from, Bufpos to, int flags);
void buffer_replace_char(struct buffer *b, Bufpos pos, Emchar ch,
			 int not_real_change, int force_lock_check);

/************************************************************************/
/*                        tracking buffer changes                       */
/************************************************************************/

/* Split into two parts.  One part goes with a buffer's text (possibly
   shared), the other with the buffer itself. */

struct buffer_text_change_data {
	/* multiple change stuff */
	int in_multiple_change;
	Bufpos mc_begin, mc_orig_end, mc_new_end;
	int mc_begin_signaled;
};

struct each_buffer_change_data {
	Charcount begin_unchanged, end_unchanged;
	/* redisplay needs to know if a newline was deleted so its
	   incremental-redisplay algorithm will fail */
	int newline_was_deleted;
	Charcount begin_extent_unchanged, end_extent_unchanged;
};

/* Number of characters at the beginning and end of the buffer that
   have not changed since the last call to buffer_reset_changes().
   If no changes have occurred since then, both values will be -1.

   "Changed" means that the text has changed. */

#define BUF_BEGIN_UNCHANGED(buf) ((buf)->changes->begin_unchanged)
#define BUF_END_UNCHANGED(buf) ((buf)->changes->end_unchanged)

/* Number of characters at the beginning and end of the buffer that
   have not had a covering extent change since the last call to
   buffer_reset_changes ().  If no changes have occurred since then,
   both values will be -1.

   "Changed" means that the extents covering the text have changed. */

#define BUF_EXTENT_BEGIN_UNCHANGED(buf) \
  ((buf)->changes->begin_extent_unchanged)
#define BUF_EXTENT_END_UNCHANGED(buf) \
  ((buf)->changes->end_extent_unchanged)

#define BUF_NEWLINE_WAS_DELETED(buf) \
  ((buf)->changes->newline_was_deleted)

void buffer_extent_signal_changed_region(struct buffer *buf,
					 Bufpos start, Bufpos end);
void buffer_reset_changes(struct buffer *buf);

/************************************************************************/
/*                        other related functions                       */
/************************************************************************/

Memind do_marker_adjustment(Memind mpos, Memind from,
			    Memind to, Bytecount amount);

void fixup_internal_substring(const Bufbyte * nonreloc,
			      Lisp_Object reloc,
			      Bytecount offset, Bytecount * len);

/* In font-lock.c */
void font_lock_maybe_update_syntactic_caches(struct buffer *buf,
					     Bufpos start,
					     Bufpos orig_end, Bufpos new_end);
void font_lock_buffer_was_killed(struct buffer *buf);

void barf_if_buffer_read_only(struct buffer *buf, Bufpos from, Bufpos to);

void init_buffer_text(struct buffer *b);
void uninit_buffer_text(struct buffer *b);

#endif				/* INCLUDED_insdel_h_ */
