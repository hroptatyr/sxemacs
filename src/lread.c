/* Lisp parsing and input streams.
   Copyright (C) 1985-1989, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1996 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "elhash.h"
#include "lstream.h"
#include "opaque.h"
#ifdef FILE_CODING
#include "file-coding.h"
#endif

#include "sysfile.h"

#ifdef LISP_FLOAT_TYPE
#define THIS_FILENAME lread
#include "sysfloat.h"
#endif /* LISP_FLOAT_TYPE */

Lisp_Object Qread_char, Qstandard_input;
Lisp_Object Qvariable_documentation;
#define LISP_BACKQUOTES
#ifdef LISP_BACKQUOTES
/*
   Nonzero means inside a new-style backquote
   with no surrounding parentheses.
   Fread initializes this to zero, so we need not specbind it
   or worry about what happens to it when there is an error.

XEmacs:
   Nested backquotes are perfectly legal and fail utterly with
   this silliness. */
static int new_backquote_flag, old_backquote_flag;
Lisp_Object Qbackquote, Qbacktick, Qcomma, Qcomma_at, Qcomma_dot;
#endif
Lisp_Object Qvariable_domain;	/* I18N3 */
Lisp_Object Vvalues, Vstandard_input, Vafter_load_alist;
Lisp_Object Qcurrent_load_list;
Lisp_Object Qload, Qload_file_name;
Lisp_Object Qfset;

/* Hash-table that maps directory names to hashes of their contents.  */
static Lisp_Object Vlocate_file_hash_table;

Lisp_Object Qexists, Qreadable, Qwritable, Qexecutable;

/* See read_escape() for an explanation of this.  */
#if 0
int fail_on_bucky_bit_character_escapes;
#endif

/* This symbol is also used in fns.c */
#define FEATUREP_SYNTAX

#ifdef FEATUREP_SYNTAX
Lisp_Object Qfeaturep;
#endif

/* non-zero if inside `load' */
int load_in_progress;

/* Whether Fload_internal() should check whether the .el is newer
   when loading .elc */
int load_warn_when_source_newer;
/* Whether Fload_internal() should check whether the .elc doesn't exist */
int load_warn_when_source_only;
/* Whether Fload_internal() should ignore .elc files when no suffix is given */
int load_ignore_elc_files;

/* Search path for files to be loaded. */
Lisp_Object Vload_path;

/* Search path for files when dumping. */
/* Lisp_Object Vdump_load_path; */

/* This is the user-visible association list that maps features to
   lists of defs in their load files. */
Lisp_Object Vload_history;

/* This is used to build the load history.  */
Lisp_Object Vcurrent_load_list;

/* Name of file actually being read by `load'.  */
Lisp_Object Vload_file_name;

/* Same as Vload_file_name but not Lisp-accessible.  This ensures that
   our #$ checks are reliable. */
Lisp_Object Vload_file_name_internal;

Lisp_Object Vload_file_name_internal_the_purecopy;

/* Function to use for reading, in `load' and friends.  */
Lisp_Object Vload_read_function;

/* The association list of objects read with the #n=object form.
   Each member of the list has the form (n . object), and is used to
   look up the object for the corresponding #n# construct.
   It must be set to nil before all top-level calls to read0.  */
Lisp_Object Vread_objects;

/* Nonzero means load should forcibly load all dynamic doc strings.  */
/* Note that this always happens (with some special behavior) when
   purify_flag is set. */
static int load_force_doc_strings;

/* List of descriptors now open for Fload_internal.  */
static Lisp_Object Vload_descriptor_list;

/* In order to implement "load_force_doc_strings", we keep
   a list of all the compiled-function objects and such
   that we have created in the process of loading this file.
   See the rant below.

   We specbind this just like Vload_file_name, so there's no
   problems with recursive loading. */
static Lisp_Object Vload_force_doc_string_list;

/* A resizing-buffer stream used to temporarily hold data while reading */
static Lisp_Object Vread_buffer_stream;

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
Lisp_Object Vcurrent_compiled_function_annotation;
#endif

static int load_byte_code_version;

/* An array describing all known built-in structure types */
static structure_type_dynarr *the_structure_type_dynarr;

#if 0 /* FSF defun hack */
/* When nonzero, read conses in pure space */
static int read_pure;
#endif

#if 0 /* FSF stuff */
/* For use within read-from-string (this reader is non-reentrant!!)  */
static int read_from_string_index;
static int read_from_string_limit;
#endif

#if 0 /* More FSF implementation kludges. */
/* In order to implement load-force-doc-string, FSF saves the
   #@-quoted string when it's seen, and goes back and retrieves
   it later.

   This approach is not only kludgy, but it in general won't work
   correctly because there's no stack of remembered #@-quoted-strings
   and those strings don't generally appear in the file in the same
   order as their #$ references. (Yes, that is amazingly stupid too.

   It would be trivially easy to always encode the #@ string
   [which is a comment, anyway] in the middle of the (#$ . INT) cons
   reference.  That way, it would be really easy to implement
   load-force-doc-string in a non-kludgy way by just retrieving the
   string immediately, because it's delivered on a silver platter.)

   And finally, this stupid approach doesn't work under Mule, or
   under MS-DOS or Windows NT, or under VMS, or any other place
   where you either can't do an ftell() or don't get back a byte
   count.

   Oh, and one more lossage in this approach: If you attempt to
   dump any ELC files that were compiled with `byte-compile-dynamic'
   (as opposed to just `byte-compile-dynamic-docstring'), you
   get hosed.  FMH! (as the illustrious JWZ was prone to utter)

   The approach we use is clean, solves all of these problems, and is
   probably easier to implement anyway.  We just save a list of all
   the containing objects that have (#$ . INT) conses in them (this
   will only be compiled-function objects and lists), and when the
   file is finished loading, we go through and fill in all the
   doc strings at once. */

 /* This contains the last string skipped with #@.  */
static char *saved_doc_string;
/* Length of buffer allocated in saved_doc_string.  */
static int saved_doc_string_size;
/* Length of actual data in saved_doc_string.  */
static int saved_doc_string_length;
/* This is the file position that string came from.  */
static int saved_doc_string_position;
#endif

EXFUN (Fread_from_string, 3);

/* When errors are signaled, the actual readcharfun should not be used
   as an argument if it is an lstream, so that lstreams don't escape
   to the Lisp level.  */
#define READCHARFUN_MAYBE(x) (LSTREAMP (x)					\
			      ? (build_string ("internal input stream"))	\
			      : (x))


static DOESNT_RETURN
read_syntax_error (const char *string)
{
  signal_error (Qinvalid_read_syntax,
		list1 (build_translated_string (string)));
}

static Lisp_Object
continuable_read_syntax_error (const char *string)
{
  return Fsignal (Qinvalid_read_syntax,
		  list1 (build_translated_string (string)));
}


/* Handle unreading and rereading of characters. */
static Emchar
readchar (Lisp_Object readcharfun)
{
  /* This function can GC */

  if (BUFFERP (readcharfun))
    {
      Emchar c;
      struct buffer *b = XBUFFER (readcharfun);

      if (!BUFFER_LIVE_P (b))
        error ("Reading from killed buffer");

      if (BUF_PT (b) >= BUF_ZV (b))
        return -1;
      c = BUF_FETCH_CHAR (b, BUF_PT (b));
      BUF_SET_PT (b, BUF_PT (b) + 1);

      return c;
    }
  else if (LSTREAMP (readcharfun))
    {
      Emchar c = Lstream_get_emchar (XLSTREAM (readcharfun));
#ifdef DEBUG_XEMACS /* testing Mule */
      static int testing_mule = 0; /* Change via debugger */
      if (testing_mule)
	{
	  if (c >= 0x20 && c <= 0x7E) stderr_out ("%c", c);
	  else if (c == '\n')         stderr_out ("\\n\n");
	  else                        stderr_out ("\\%o ", c);
	}
#endif /* testing Mule */
      return c;
    }
  else if (MARKERP (readcharfun))
    {
      Emchar c;
      Bufpos mpos = marker_position (readcharfun);
      struct buffer *inbuffer = XMARKER (readcharfun)->buffer;

      if (mpos >= BUF_ZV (inbuffer))
	return -1;
      c = BUF_FETCH_CHAR (inbuffer, mpos);
      set_marker_position (readcharfun, mpos + 1);
      return c;
    }
  else
    {
      Lisp_Object tem = call0 (readcharfun);

      if (!CHAR_OR_CHAR_INTP (tem))
	return -1;
      return XCHAR_OR_CHAR_INT (tem);
    }
}

/* Unread the character C in the way appropriate for the stream READCHARFUN.
   If the stream is a user function, call it with the char as argument.  */

static void
unreadchar (Lisp_Object readcharfun, Emchar c)
{
  if (c == -1)
    /* Don't back up the pointer if we're unreading the end-of-input mark,
       since readchar didn't advance it when we read it.  */
    ;
  else if (BUFFERP (readcharfun))
    BUF_SET_PT (XBUFFER (readcharfun), BUF_PT (XBUFFER (readcharfun)) - 1);
  else if (LSTREAMP (readcharfun))
    {
      Lstream_unget_emchar (XLSTREAM (readcharfun), c);
#ifdef DEBUG_XEMACS /* testing Mule */
      {
        static int testing_mule = 0; /* Set this using debugger */
        if (testing_mule)
          fprintf (stderr,
                   (c >= 0x20 && c <= 0x7E) ? "UU%c" :
                   ((c == '\n') ? "UU\\n\n" : "UU\\%o"), c);
      }
#endif
    }
  else if (MARKERP (readcharfun))
    set_marker_position (readcharfun, marker_position (readcharfun) - 1);
  else
    call1 (readcharfun, make_char (c));
}

static Lisp_Object read0 (Lisp_Object readcharfun);
static Lisp_Object read1 (Lisp_Object readcharfun);
/* allow_dotted_lists means that something like (foo bar . baz)
   is acceptable.  If -1, means check for starting with defun
   and make structure pure. (not implemented, probably for very
   good reasons)
*/
/*
   If check_for_doc_references, look for (#$ . INT) doc references
   in the list and record if load_force_doc_strings is non-zero.
   (Such doc references will be destroyed during the loadup phase
   by replacing with Qzero, because Snarf-documentation will fill
   them in again.)

   WARNING: If you set this, you sure as hell better not call
   free_list() on the returned list here. */

static Lisp_Object read_list (Lisp_Object readcharfun,
                              Emchar terminator,
                              int allow_dotted_lists,
			      int check_for_doc_references);

static void readevalloop (Lisp_Object readcharfun,
                          Lisp_Object sourcefile,
                          Lisp_Object (*evalfun) (Lisp_Object),
                          int printflag);

static Lisp_Object
load_unwind (Lisp_Object stream)  /* used as unwind-protect function in load */
{
  Lstream_close (XLSTREAM (stream));
  if (--load_in_progress < 0)
    load_in_progress = 0;
  return Qnil;
}

static Lisp_Object
load_descriptor_unwind (Lisp_Object oldlist)
{
  Vload_descriptor_list = oldlist;
  return Qnil;
}

static Lisp_Object
load_file_name_internal_unwind (Lisp_Object oldval)
{
  Vload_file_name_internal = oldval;
  return Qnil;
}

static Lisp_Object
load_file_name_internal_the_purecopy_unwind (Lisp_Object oldval)
{
  Vload_file_name_internal_the_purecopy = oldval;
  return Qnil;
}

static Lisp_Object
load_byte_code_version_unwind (Lisp_Object oldval)
{
  load_byte_code_version = XINT (oldval);
  return Qnil;
}

/* The plague is coming.

   Ring around the rosy, pocket full of posy,
   Ashes ashes, they all fall down.
   */
void
ebolify_bytecode_constants (Lisp_Object vector)
{
  int len = XVECTOR_LENGTH (vector);
  int i;

  for (i = 0; i < len; i++)
    {
      Lisp_Object el = XVECTOR_DATA (vector)[i];

      /* We don't check for `eq', `equal', and the others that have
	 bytecode opcodes.  This might lose if someone passes #'eq or
	 something to `funcall', but who would really do that?  As
	 they say in law, we've made a "good-faith effort" to
	 unfuckify ourselves.  And doing it this way avoids screwing
	 up args to `make-hash-table' and such.  As it is, we have to
	 add an extra Ebola check in decode_weak_list_type(). --ben */
      if      (EQ (el, Qassoc))  el = Qold_assoc;
      else if (EQ (el, Qdelq))   el = Qold_delq;
#if 0
      /* I think this is a bad idea because it will probably mess
	 with keymap code. */
      else if (EQ (el, Qdelete)) el = Qold_delete;
#endif
      else if (EQ (el, Qrassq))  el = Qold_rassq;
      else if (EQ (el, Qrassoc)) el = Qold_rassoc;

      XVECTOR_DATA (vector)[i] = el;
    }
}

static Lisp_Object
pas_de_lache_ici (int fd, Lisp_Object victim)
{
  Lisp_Object tem;
  EMACS_INT pos;

  if (!INTP (XCDR (victim)))
    signal_simple_error ("Bogus doc string reference", victim);
  pos = XINT (XCDR (victim));
  if (pos < 0)
    pos = -pos; /* kludge to mark a user variable */
  tem = unparesseuxify_doc_string (fd, pos, 0, Vload_file_name_internal);
  if (!STRINGP (tem))
    signal_error (Qerror, tem);
  return tem;
}

static Lisp_Object
load_force_doc_string_unwind (Lisp_Object oldlist)
{
  struct gcpro gcpro1;
  Lisp_Object list = Vload_force_doc_string_list;
  Lisp_Object tail;
  int fd = XINT (XCAR (Vload_descriptor_list));

  GCPRO1 (list);
  /* restore the old value first just in case an error occurs. */
  Vload_force_doc_string_list = oldlist;

  LIST_LOOP (tail, list)
    {
      Lisp_Object john = Fcar (tail);
      if (CONSP (john))
	{
	  assert (CONSP (XCAR (john)));
	  assert (!purify_flag); /* should have been handled in read_list() */
	  XCAR (john) = pas_de_lache_ici (fd, XCAR (john));
	}
      else
	{
	  Lisp_Object doc;

	  assert (COMPILED_FUNCTIONP (john));
	  if (CONSP (XCOMPILED_FUNCTION (john)->instructions))
	    {
	      struct gcpro ngcpro1;
	      Lisp_Object juan = (pas_de_lache_ici
				  (fd, XCOMPILED_FUNCTION (john)->instructions));
	      Lisp_Object ivan;

	      NGCPRO1 (juan);
	      ivan = Fread (juan);
	      if (!CONSP (ivan))
		signal_simple_error ("invalid lazy-loaded byte code", ivan);
	      XCOMPILED_FUNCTION (john)->instructions = XCAR (ivan);
	      /* v18 or v19 bytecode file.  Need to Ebolify. */
	      if (XCOMPILED_FUNCTION (john)->flags.ebolified
		  && VECTORP (XCDR (ivan)))
		ebolify_bytecode_constants (XCDR (ivan));
	      XCOMPILED_FUNCTION (john)->constants = XCDR (ivan);
	      NUNGCPRO;
	    }
	  doc = compiled_function_documentation (XCOMPILED_FUNCTION (john));
	  if (CONSP (doc))
	    {
	      assert (!purify_flag); /* should have been handled in
					read_compiled_function() */
	      doc = pas_de_lache_ici (fd, doc);
	      set_compiled_function_documentation (XCOMPILED_FUNCTION (john),
						   doc);
	    }
	}
    }

  if (!NILP (list))
    free_list (list);

  UNGCPRO;
  return Qnil;
}

/* Close all descriptors in use for Fload_internal.
   This is used when starting a subprocess.  */

void
close_load_descs (void)
{
  Lisp_Object tail;
  LIST_LOOP (tail, Vload_descriptor_list)
    close (XINT (XCAR (tail)));
}

#ifdef I18N3
Lisp_Object Vfile_domain;

Lisp_Object
restore_file_domain (Lisp_Object val)
{
  Vfile_domain = val;
  return Qnil;
}
#endif /* I18N3 */

DEFUN ("load-internal", Fload_internal, 1, 6, 0, /*
Execute a file of Lisp code named FILE; no coding-system frobbing.
This function is identical to `load' except for the handling of the
CODESYS and USED-CODESYS arguments under XEmacs/Mule. (When Mule
support is not present, both functions are identical and ignore the
CODESYS and USED-CODESYS arguments.)

If support for Mule exists in this Emacs, the file is decoded
according to CODESYS; if omitted, no conversion happens.  If
USED-CODESYS is non-nil, it should be a symbol, and the actual coding
system that was used for the decoding is stored into it.  It will in
general be different from CODESYS if CODESYS specifies automatic
encoding detection or end-of-line detection.
*/
       (file, noerror, nomessage, nosuffix, codesys, used_codesys))
{
  /* This function can GC */
  int fd = -1;
  int speccount = specpdl_depth ();
  int source_only = 0;
  Lisp_Object newer   = Qnil;
  Lisp_Object handler = Qnil;
  Lisp_Object found   = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int reading_elc = 0;
  int message_p = NILP (nomessage);
/*#ifdef DEBUG_XEMACS*/
  static Lisp_Object last_file_loaded;
/*#endif*/
  struct stat s1, s2;
  GCPRO3 (file, newer, found);

  CHECK_STRING (file);

/*#ifdef DEBUG_XEMACS*/
  if (purify_flag && noninteractive)
    {
      message_p = 1;
      last_file_loaded = file;
    }
/*#endif / * DEBUG_XEMACS */

  /* If file name is magic, call the handler.  */
  handler = Ffind_file_name_handler (file, Qload);
  if (!NILP (handler))
    RETURN_UNGCPRO (call5 (handler, Qload, file, noerror,
			  nomessage, nosuffix));

  /* Do this after the handler to avoid
     the need to gcpro noerror, nomessage and nosuffix.
     (Below here, we care only whether they are nil or not.)  */
  file = Fsubstitute_in_file_name (file);
#ifdef FILE_CODING
  if (!NILP (used_codesys))
    CHECK_SYMBOL (used_codesys);
#endif

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file.
     Unix truly sucks. */
  if (XSTRING_LENGTH (file) > 0)
    {
      char *foundstr;
      int foundlen;

      fd = locate_file (Vload_path, file,
                        ((!NILP (nosuffix)) ? Qnil :
			 build_string (load_ignore_elc_files ? ".el:" :
				       ".elc:.el:")),
                        &found,
                        -1);

      if (fd < 0)
	{
	  if (NILP (noerror))
	    signal_file_error ("Cannot open load file", file);
	  else
	    {
	      UNGCPRO;
	      return Qnil;
	    }
	}

      foundstr = (char *) alloca (XSTRING_LENGTH (found) + 1);
      strcpy (foundstr, (char *) XSTRING_DATA (found));
      foundlen = strlen (foundstr);

      /* The omniscient JWZ thinks this is worthless, but I beg to
	 differ. --ben */
      if (load_ignore_elc_files)
	{
	  newer = Ffile_name_nondirectory (found);
	}
      else if (load_warn_when_source_newer &&
	       !memcmp (".elc", foundstr + foundlen - 4, 4))
	{
	  if (! fstat (fd, &s1))	/* can't fail, right? */
	    {
	      int result;
	      /* temporarily hack the 'c' off the end of the filename */
	      foundstr[foundlen - 1] = '\0';
	      result = xemacs_stat (foundstr, &s2);
	      if (result >= 0 &&
		  (unsigned) s1.st_mtime < (unsigned) s2.st_mtime)
              {
		Lisp_Object newer_name = make_string ((Bufbyte *) foundstr,
						      foundlen - 1);
                struct gcpro nngcpro1;
                NNGCPRO1 (newer_name);
		newer = Ffile_name_nondirectory (newer_name);
                NNUNGCPRO;
              }
	      /* put the 'c' back on (kludge-o-rama) */
	      foundstr[foundlen - 1] = 'c';
	    }
	}
      else if (load_warn_when_source_only &&
	       /* `found' ends in ".el" */
	       !memcmp (".el", foundstr + foundlen - 3, 3) &&
	       /* `file' does not end in ".el" */
	       memcmp (".el",
		       XSTRING_DATA (file) + XSTRING_LENGTH (file) - 3,
		       3))
	{
	  source_only = 1;
	}

      if (!memcmp (".elc", foundstr + foundlen - 4, 4))
	reading_elc = 1;
    }

#define PRINT_LOADING_MESSAGE(done) do {				\
  if (load_ignore_elc_files)						\
    {									\
      if (message_p)							\
	message ("Loading %s..." done, XSTRING_DATA (newer));		\
    }									\
  else if (!NILP (newer))						\
    message ("Loading %s..." done " (file %s is newer)",		\
	     XSTRING_DATA (file),					\
	     XSTRING_DATA (newer));					\
  else if (source_only)							\
    message ("Loading %s..." done " (file %s.elc does not exist)",	\
	     XSTRING_DATA (file),					\
	     XSTRING_DATA (Ffile_name_nondirectory (file)));		\
  else if (message_p)							\
    message ("Loading %s..." done, XSTRING_DATA (file));		\
  } while (0)

  PRINT_LOADING_MESSAGE ("");

  {
    /* Lisp_Object's must be malloc'ed, not stack-allocated */
    Lisp_Object lispstream = Qnil;
    const int block_size = 8192;
    struct gcpro ngcpro1;

    NGCPRO1 (lispstream);
    lispstream = make_filedesc_input_stream (fd, 0, -1, LSTR_CLOSING);
    /* 64K is used for normal files; 8K should be OK here because Lisp
       files aren't really all that big. */
    Lstream_set_buffering (XLSTREAM (lispstream), LSTREAM_BLOCKN_BUFFERED,
			   block_size);
#ifdef FILE_CODING
    lispstream = make_decoding_input_stream
      (XLSTREAM (lispstream), Fget_coding_system (codesys));
    Lstream_set_buffering (XLSTREAM (lispstream), LSTREAM_BLOCKN_BUFFERED,
			   block_size);
#endif
    /* NOTE: Order of these is very important.  Don't rearrange them. */
    record_unwind_protect (load_unwind, lispstream);
    record_unwind_protect (load_descriptor_unwind, Vload_descriptor_list);
    record_unwind_protect (load_file_name_internal_unwind,
			   Vload_file_name_internal);
    record_unwind_protect (load_file_name_internal_the_purecopy_unwind,
			   Vload_file_name_internal_the_purecopy);
    record_unwind_protect (load_force_doc_string_unwind,
			   Vload_force_doc_string_list);
    Vload_file_name_internal = found;
    Vload_file_name_internal_the_purecopy = Qnil;
    specbind (Qload_file_name, found);
    Vload_descriptor_list = Fcons (make_int (fd), Vload_descriptor_list);
    Vload_force_doc_string_list = Qnil;
#ifdef I18N3
    record_unwind_protect (restore_file_domain, Vfile_domain);
    Vfile_domain = Qnil; /* set it to nil; a call to #'domain will set it. */
#endif
    load_in_progress++;

    /* Now determine what sort of ELC file we're reading in. */
    record_unwind_protect (load_byte_code_version_unwind,
			   make_int (load_byte_code_version));
    if (reading_elc)
      {
	char elc_header[8];
	int num_read;

	num_read = Lstream_read (XLSTREAM (lispstream), elc_header, 8);
	if (num_read < 8
	    || strncmp (elc_header, ";ELC", 4))
	  {
	    /* Huh?  Probably not a valid ELC file. */
	    load_byte_code_version = 100; /* no Ebolification needed */
	    Lstream_unread (XLSTREAM (lispstream), elc_header, num_read);
	  }
	else
	  load_byte_code_version = elc_header[4];
      }
    else
      load_byte_code_version = 100; /* no Ebolification needed */

    readevalloop (lispstream, file, Feval, 0);
#ifdef FILE_CODING
    if (!NILP (used_codesys))
      Fset (used_codesys,
	    XCODING_SYSTEM_NAME
	    (decoding_stream_coding_system (XLSTREAM (lispstream))));
#endif
    unbind_to (speccount, Qnil);

    NUNGCPRO;
  }

  {
    Lisp_Object tem;
    /* #### Disgusting kludge */
    /* Run any load-hooks for this file.  */
    /* #### An even more disgusting kludge.  There is horrible code */
    /* that is relying on the fact that dumped lisp files are found */
    /* via `load-path' search. */
    Lisp_Object name = file;

    if (!NILP(Ffile_name_absolute_p(file)))
      {
	name = Ffile_name_nondirectory(file);
      }

    {
      struct gcpro ngcpro1;

      NGCPRO1 (name);
      tem = Fassoc (name, Vafter_load_alist);
      NUNGCPRO;
    }
    if (!NILP (tem))
      {
	struct gcpro ngcpro1;

	NGCPRO1 (tem);
	/* Use eval so that errors give a semi-meaningful backtrace.  --Stig */
	tem = Fcons (Qprogn, Fcdr (tem));
	Feval (tem);
	NUNGCPRO;
      }
  }

/*#ifdef DEBUG_XEMACS*/
  if (purify_flag && noninteractive)
    {
      if (!EQ (last_file_loaded, file))
	message ("Loading %s ...done", XSTRING_DATA (file));
    }
/*#endif / * DEBUG_XEMACS */

  if (!noninteractive)
    PRINT_LOADING_MESSAGE ("done");

  UNGCPRO;
  return Qt;
}


/* ------------------------------- */
/*          locate_file            */
/* ------------------------------- */

static int
decode_mode_1 (Lisp_Object mode)
{
  if (EQ (mode, Qexists))
    return F_OK;
  else if (EQ (mode, Qexecutable))
    return X_OK;
  else if (EQ (mode, Qwritable))
    return W_OK;
  else if (EQ (mode, Qreadable))
    return R_OK;
  else if (INTP (mode))
    {
      check_int_range (XINT (mode), 0, 7);
      return XINT (mode);
    }
  else
    signal_simple_error ("Invalid value", mode);
  return 0;			/* unreached */
}

static int
decode_mode (Lisp_Object mode)
{
  if (NILP (mode))
    return R_OK;
  else if (CONSP (mode))
    {
      Lisp_Object tail;
      int mask = 0;
      EXTERNAL_LIST_LOOP (tail, mode)
	mask |= decode_mode_1 (XCAR (tail));
      return mask;
    }
  else
    return decode_mode_1 (mode);
}

DEFUN ("locate-file", Flocate_file, 2, 4, 0, /*
Search for FILENAME through PATH-LIST.

If SUFFIXES is non-nil, it should be a list of suffixes to append to
file name when searching.

If MODE is non-nil, it should be a symbol or a list of symbol representing
requirements.  Allowed symbols are `exists', `executable', `writable', and
`readable'.  If MODE is nil, it defaults to `readable'.

`locate-file' keeps hash tables of the directories it searches through,
in order to speed things up.  It tries valiantly to not get confused in
the face of a changing and unpredictable environment, but can occasionally
get tripped up.  In this case, you will have to call
`locate-file-clear-hashing' to get it back on track.  See that function
for details.
*/
       (filename, path_list, suffixes, mode))
{
  /* This function can GC */
  Lisp_Object tp;

  CHECK_STRING (filename);

  if (LISTP (suffixes))
    {
      Lisp_Object tail;
      EXTERNAL_LIST_LOOP (tail, suffixes)
	CHECK_STRING (XCAR (tail));
    }
  else
    CHECK_STRING (suffixes);

  locate_file (path_list, filename, suffixes, &tp, decode_mode (mode));
  return tp;
}

/* Recalculate the hash table for the given string.  DIRECTORY should
   better have been through Fexpand_file_name() by now.  */

static Lisp_Object
locate_file_refresh_hashing (Lisp_Object directory)
{
  Lisp_Object hash =
    make_directory_hash_table ((char *) XSTRING_DATA (directory));

  if (!NILP (hash))
    Fputhash (directory, hash, Vlocate_file_hash_table);
  return hash;
}

/* find the hash table for the given directory, recalculating if necessary */

static Lisp_Object
locate_file_find_directory_hash_table (Lisp_Object directory)
{
  Lisp_Object hash = Fgethash (directory, Vlocate_file_hash_table, Qnil);
  if (NILP (hash))
    return locate_file_refresh_hashing (directory);
  else
    return hash;
}

/* The SUFFIXES argument in any of the locate_file* functions can be
   nil, a list, or a string (for backward compatibility), with the
   following semantics:

   a) nil    - no suffix, just search for file name intact
               (semantically different from "empty suffix list", which
               would be meaningless.)
   b) list   - list of suffixes to append to file name.  Each of these
               must be a string.
   c) string - colon-separated suffixes to append to file name (backward
               compatibility).

   All of this got hairy, so I decided to use a mapper.  Calling a
   function for each suffix shouldn't slow things down, since
   locate_file is rarely called with enough suffixes for funcalls to
   make any difference.  */

/* Map FUN over SUFFIXES, as described above.  FUN will be called with a
   char * containing the current file name, and ARG.  Mapping stops when
   FUN returns non-zero. */
static void
locate_file_map_suffixes (Lisp_Object filename, Lisp_Object suffixes,
			  int (*fun) (char *, void *),
			  void *arg)
{
  /* This function can GC */
  char *fn;
  int fn_len, max;

  /* Calculate maximum size of any filename made from
     this path element/specified file name and any possible suffix.  */
  if (CONSP (suffixes))
    {
      /* We must traverse the list, so why not do it right. */
      Lisp_Object tail;
      max = 0;
      LIST_LOOP (tail, suffixes)
	{
	  if (XSTRING_LENGTH (XCAR (tail)) > max)
	    max = XSTRING_LENGTH (XCAR (tail));
	}
    }
  else if (NILP (suffixes))
    max = 0;
  else
    /* Just take the easy way out */
    max = XSTRING_LENGTH (suffixes);

  fn_len = XSTRING_LENGTH (filename);
  fn = (char *) alloca (max + fn_len + 1);
  memcpy (fn, (char *) XSTRING_DATA (filename), fn_len);

  /* Loop over suffixes.  */
  if (!STRINGP (suffixes))
    {
      if (NILP (suffixes))
	{
	  /* Case a) discussed in the comment above. */
	  fn[fn_len] = 0;
	  if ((*fun) (fn, arg))
	    return;
	}
      else
	{
	  /* Case b) */
	  Lisp_Object tail;
	  LIST_LOOP (tail, suffixes)
	    {
	      memcpy (fn + fn_len, XSTRING_DATA (XCAR (tail)),
		      XSTRING_LENGTH (XCAR (tail)));
	      fn[fn_len + XSTRING_LENGTH (XCAR (tail))] = 0;
	      if ((*fun) (fn, arg))
		return;
	    }
	}
    }
  else
    {
      /* Case c) */
      const char *nsuffix = (const char *) XSTRING_DATA (suffixes);

      while (1)
	{
	  char *esuffix = (char *) strchr (nsuffix, ':');
	  int lsuffix = esuffix ? esuffix - nsuffix : (int) strlen (nsuffix);

	  /* Concatenate path element/specified name with the suffix.  */
	  strncpy (fn + fn_len, nsuffix, lsuffix);
	  fn[fn_len + lsuffix] = 0;

	  if ((*fun) (fn, arg))
	    return;

	  /* Advance to next suffix.  */
	  if (esuffix == 0)
	    break;
	  nsuffix += lsuffix + 1;
	}
    }
}

struct locate_file_in_directory_mapper_closure {
  int fd;
  Lisp_Object *storeptr;
  int mode;
};

static int
locate_file_in_directory_mapper (char *fn, void *arg)
{
  struct locate_file_in_directory_mapper_closure *closure =
    (struct locate_file_in_directory_mapper_closure *)arg;
  struct stat st;

  /* Ignore file if it's a directory.  */
  if (xemacs_stat (fn, &st) >= 0
      && (st.st_mode & S_IFMT) != S_IFDIR)
    {
      /* Check that we can access or open it.  */
      if (closure->mode >= 0)
	closure->fd = access (fn, closure->mode);
      else
	closure->fd = open (fn, O_RDONLY | OPEN_BINARY, 0);

      if (closure->fd >= 0)
	{
	  /* We succeeded; return this descriptor and filename.  */
	  if (closure->storeptr)
	    *closure->storeptr = build_string (fn);

#ifndef WIN32_NATIVE
	  /* If we actually opened the file, set close-on-exec flag
	     on the new descriptor so that subprocesses can't whack
	     at it.  */
	  if (closure->mode < 0)
	    (void) fcntl (closure->fd, F_SETFD, FD_CLOEXEC);
#endif

	  return 1;
	}
    }
  /* Keep mapping. */
  return 0;
}


/* look for STR in PATH, optionally adding SUFFIXES.  DIRECTORY need
   not have been expanded.  */

static int
locate_file_in_directory (Lisp_Object directory, Lisp_Object str,
			  Lisp_Object suffixes, Lisp_Object *storeptr,
			  int mode)
{
  /* This function can GC */
  struct locate_file_in_directory_mapper_closure closure;
  Lisp_Object filename = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (directory, str, filename);

  filename = Fexpand_file_name (str, directory);
  if (NILP (filename) || NILP (Ffile_name_absolute_p (filename)))
    /* If there are non-absolute elts in PATH (eg ".") */
    /* Of course, this could conceivably lose if luser sets
       default-directory to be something non-absolute ... */
    {
      if (NILP (filename))
	/* NIL means current directory */
	filename = current_buffer->directory;
      else
	filename = Fexpand_file_name (filename,
				      current_buffer->directory);
      if (NILP (Ffile_name_absolute_p (filename)))
	{
	  /* Give up on this directory! */
	  UNGCPRO;
	  return -1;
	}
    }

  closure.fd = -1;
  closure.storeptr = storeptr;
  closure.mode = mode;

  locate_file_map_suffixes (filename, suffixes, locate_file_in_directory_mapper,
			    &closure);

  UNGCPRO;
  return closure.fd;
}

/* do the same as locate_file() but don't use any hash tables. */

static int
locate_file_without_hash (Lisp_Object path, Lisp_Object str,
			  Lisp_Object suffixes, Lisp_Object *storeptr,
			  int mode)
{
  /* This function can GC */
  int absolute = !NILP (Ffile_name_absolute_p (str));

  EXTERNAL_LIST_LOOP (path, path)
    {
      int val = locate_file_in_directory (XCAR (path), str, suffixes, storeptr,
					  mode);
      if (val >= 0)
	return val;
      if (absolute)
	break;
    }
  return -1;
}

static int
locate_file_construct_suffixed_files_mapper (char *fn, void *arg)
{
  Lisp_Object *tail = (Lisp_Object *)arg;
  *tail = Fcons (build_string (fn), *tail);
  return 0;
}

/* Construct a list of all files to search for.
   It makes sense to have this despite locate_file_map_suffixes()
   because we need Lisp strings to access the hash-table, and it would
   be inefficient to create them on the fly, again and again for each
   path component.  See locate_file(). */

static Lisp_Object
locate_file_construct_suffixed_files (Lisp_Object filename,
				      Lisp_Object suffixes)
{
  Lisp_Object tail = Qnil;
  struct gcpro gcpro1;
  GCPRO1 (tail);

  locate_file_map_suffixes (filename, suffixes,
			    locate_file_construct_suffixed_files_mapper,
			    &tail);

  UNGCPRO;
  return Fnreverse (tail);
}

DEFUN ("locate-file-clear-hashing", Flocate_file_clear_hashing, 1, 1, 0, /*
Clear the hash records for the specified list of directories.
`locate-file' uses a hashing scheme to speed lookup, and will correctly
track the following environmental changes:

-- changes of any sort to the list of directories to be searched.
-- addition and deletion of non-shadowing files (see below) from the
   directories in the list.
-- byte-compilation of a .el file into a .elc file.

`locate-file' will primarily get confused if you add a file that shadows
\(i.e. has the same name as) another file further down in the directory list.
In this case, you must call `locate-file-clear-hashing'.

If PATH is t, it means to fully clear all the accumulated hashes.  This
can be used if the internal tables grow too large, or when dumping.
*/
       (path))
{
  if (EQ (path, Qt))
    Fclrhash (Vlocate_file_hash_table);
  else
    {
      Lisp_Object pathtail;
      EXTERNAL_LIST_LOOP (pathtail, path)
	{
	  Lisp_Object pathel = Fexpand_file_name (XCAR (pathtail), Qnil);
	  Fremhash (pathel, Vlocate_file_hash_table);
	}
    }
  return Qnil;
}

/* Search for a file whose name is STR, looking in directories
   in the Lisp list PATH, and trying suffixes from SUFFIXES.
   SUFFIXES is a list of possible suffixes, or (for backward
   compatibility) a string containing possible suffixes separated by
   colons.
   On success, returns a file descriptor.  On failure, returns -1.

   MODE nonnegative means don't open the files,
   just look for one for which access(file,MODE) succeeds.  In this case,
   returns a nonnegative value on success.  On failure, returns -1.

   If STOREPTR is nonzero, it points to a slot where the name of
   the file actually found should be stored as a Lisp string.
   Nil is stored there on failure.

   Called openp() in FSFmacs. */

int
locate_file (Lisp_Object path, Lisp_Object str, Lisp_Object suffixes,
	     Lisp_Object *storeptr, int mode)
{
  /* This function can GC */
  Lisp_Object suffixtab = Qnil;
  Lisp_Object pathtail, pathel_expanded;
  int val;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  if (storeptr)
    *storeptr = Qnil;

  /* Is it really necessary to gcpro path and str?  It shouldn't be
     unless some caller has fucked up.  There are known instances that
     call us with build_string("foo:bar") as SUFFIXES, though. */
  GCPRO4 (path, str, suffixes, suffixtab);

  /* if this filename has directory components, it's too complicated
     to try and use the hash tables. */
  if (!NILP (Ffile_name_directory (str)))
    {
      val = locate_file_without_hash (path, str, suffixes, storeptr, mode);
      UNGCPRO;
      return val;
    }

  suffixtab = locate_file_construct_suffixed_files (str, suffixes);

  EXTERNAL_LIST_LOOP (pathtail, path)
    {
      Lisp_Object pathel = XCAR (pathtail);
      Lisp_Object hash_table;
      Lisp_Object tail;
      int found = 0;

      /* If this path element is relative, we have to look by hand. */
      if (NILP (pathel) || NILP (Ffile_name_absolute_p (pathel)))
	{
	  val = locate_file_in_directory (pathel, str, suffixes, storeptr,
					  mode);
	  if (val >= 0)
	    {
	      UNGCPRO;
	      return val;
	    }
	  continue;
	}

      pathel_expanded = Fexpand_file_name (pathel, Qnil);
      hash_table = locate_file_find_directory_hash_table (pathel_expanded);

      if (!NILP (hash_table))
	{
	  /* Loop over suffixes.  */
	  LIST_LOOP (tail, suffixtab)
	    if (!NILP (Fgethash (XCAR (tail), hash_table, Qnil)))
	      {
		found = 1;
		break;
	      }
	}

      if (found)
	{
	  /* This is a likely candidate.  Look by hand in this directory
	     so we don't get thrown off if someone byte-compiles a file. */
	  val = locate_file_in_directory (pathel, str, suffixes, storeptr,
					  mode);
	  if (val >= 0)
	    {
	      UNGCPRO;
	      return val;
	    }

	  /* Hmm ...  the file isn't actually there. (Or possibly it's
	     a directory ...)  So refresh our hashing. */
	  locate_file_refresh_hashing (pathel_expanded);
	}
    }

  /* File is probably not there, but check the hard way just in case. */
  val = locate_file_without_hash (path, str, suffixes, storeptr, mode);
  if (val >= 0)
    {
      /* Sneaky user added a file without telling us. */
      Flocate_file_clear_hashing (path);
    }

  UNGCPRO;
  return val;
}


#ifdef LOADHIST

/* Merge the list we've accumulated of globals from the current input source
   into the load_history variable.  The details depend on whether
   the source has an associated file name or not. */

static void
build_load_history (int loading, Lisp_Object source)
{
  REGISTER Lisp_Object tail, prev, newelt;
  REGISTER Lisp_Object tem, tem2;
  int foundit;

#if !defined(LOADHIST_DUMPED)
  /* Don't bother recording anything for preloaded files.  */
  if (purify_flag)
    return;
#endif

  tail = Vload_history;
  prev = Qnil;
  foundit = 0;
  while (!NILP (tail))
    {
      tem = Fcar (tail);

      /* Find the feature's previous assoc list... */
      if (internal_equal (source, Fcar (tem), 0))
	{
	  foundit = 1;

	  /*  If we're loading, remove it. */
	  if (loading)
	    {
	      if (NILP (prev))
		Vload_history = Fcdr (tail);
	      else
		Fsetcdr (prev, Fcdr (tail));
	    }

	  /*  Otherwise, cons on new symbols that are not already members.  */
	  else
	    {
	      tem2 = Vcurrent_load_list;

	      while (CONSP (tem2))
		{
		  newelt = XCAR (tem2);

		  if (NILP (Fmemq (newelt, tem)))
		    Fsetcar (tail, Fcons (Fcar (tem),
					  Fcons (newelt, Fcdr (tem))));

		  tem2 = XCDR (tem2);
		  QUIT;
		}
	    }
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }

  /* If we're loading, cons the new assoc onto the front of load-history,
     the most-recently-loaded position.  Also do this if we didn't find
     an existing member for the current source.  */
  if (loading || !foundit)
    Vload_history = Fcons (Fnreverse (Vcurrent_load_list),
			   Vload_history);
}

#else /* !LOADHIST */
#define build_load_history(x,y)
#endif /* !LOADHIST */


#if 0 /* FSFmacs defun hack */
Lisp_Object
unreadpure (void)	/* Used as unwind-protect function in readevalloop */
{
  read_pure = 0;
  return Qnil;
}
#endif /* 0 */

static void
readevalloop (Lisp_Object readcharfun,
              Lisp_Object sourcename,
              Lisp_Object (*evalfun) (Lisp_Object),
              int printflag)
{
  /* This function can GC */
  REGISTER Emchar c;
  REGISTER Lisp_Object val = Qnil;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2;
  struct buffer *b = 0;

  if (BUFFERP (readcharfun))
    b = XBUFFER (readcharfun);
  else if (MARKERP (readcharfun))
    b = XMARKER (readcharfun)->buffer;

  /* Don't do this.  It is not necessary, and it needlessly exposes
     READCHARFUN (which can be a stream) to Lisp.  --hniksic */
  /*specbind (Qstandard_input, readcharfun);*/

  specbind (Qcurrent_load_list, Qnil);

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  Vcurrent_compiled_function_annotation = Qnil;
#endif
  GCPRO2 (val, sourcename);

  LOADHIST_ATTACH (sourcename);

  while (1)
    {
      QUIT;

      if (b != 0 && !BUFFER_LIVE_P (b))
	error ("Reading from killed buffer");

      c = readchar (readcharfun);
      if (c == ';')
	{
          /* Skip comment */
	  while ((c = readchar (readcharfun)) != '\n' && c != -1)
            QUIT;
	  continue;
	}
      if (c < 0)
        break;

      /* Ignore whitespace here, so we can detect eof.  */
      if (c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r')
        continue;

#if 0 /* FSFmacs defun hack */
      if (purify_flag && c == '(')
	{
	  int count1 = specpdl_depth ();
	  record_unwind_protect (unreadpure, Qnil);
	  val = read_list (readcharfun, ')', -1, 1);
	  unbind_to (count1, Qnil);
	}
      else
#else /* No "defun hack" -- Emacs 19 uses read-time syntax for bytecodes */
	{
	  unreadchar (readcharfun, c);
	  Vread_objects = Qnil;
	  if (NILP (Vload_read_function))
	    val = read0 (readcharfun);
	  else
	    val = call1 (Vload_read_function, readcharfun);
	}
#endif
      val = (*evalfun) (val);
      if (printflag)
	{
	  Vvalues = Fcons (val, Vvalues);
	  if (EQ (Vstandard_output, Qt))
	    Fprin1 (val, Qnil);
	  else
	    Fprint (val, Qnil);
	}
    }

  build_load_history (LSTREAMP (readcharfun) ||
		      /* This looks weird, but it's what's in FSFmacs */
		      (b ? BUF_NARROWED (b) : BUF_NARROWED (current_buffer)),
                      sourcename);
  UNGCPRO;

  unbind_to (speccount, Qnil);
}

DEFUN ("eval-buffer", Feval_buffer, 0, 2, "bBuffer: ", /*
Execute BUFFER as Lisp code.
Programs can pass two arguments, BUFFER and PRINTFLAG.
BUFFER is the buffer to evaluate (nil means use current buffer).
PRINTFLAG controls printing of output:
nil means discard it; anything else is a stream for printing.

If there is no error, point does not move.  If there is an error,
point remains at the end of the last character read from the buffer.
*/
       (buffer, printflag))
{
  /* This function can GC */
  int speccount = specpdl_depth ();
  Lisp_Object tem, buf;

  if (NILP (buffer))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (buffer);
  if (NILP (buf))
    error ("No such buffer.");

  if (NILP (printflag))
    tem = Qsymbolp;             /* #### #@[]*&$#*[& SI:NULL-STREAM */
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  BUF_SET_PT (XBUFFER (buf), BUF_BEGV (XBUFFER (buf)));
  readevalloop (buf, XBUFFER (buf)->filename, Feval,
		!NILP (printflag));

  return unbind_to (speccount, Qnil);
}

#if 0
xxDEFUN ("eval-current-buffer", Feval_current_buffer, 0, 1, "", /*
Execute the current buffer as Lisp code.
Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print.

If there is no error, point does not move.  If there is an error,
point remains at the end of the last character read from the buffer.
*/
	 (printflag))
{
  code omitted;
}
#endif /* 0 */

DEFUN ("eval-region", Feval_region, 2, 3, "r", /*
Execute the region as Lisp code.
When called from programs, expects two arguments START and END
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third optional argument STREAM which controls output:
nil means discard it; anything else is stream for printing it.

If there is no error, point does not move.  If there is an error,
point remains at the end of the last character read from the buffer.

Note:  Before evaling the region, this function narrows the buffer to it.
If the code being eval'd should happen to trigger a redisplay you may
see some text temporarily disappear because of this.
*/
       (start, end, stream))
{
  /* This function can GC */
  int speccount = specpdl_depth ();
  Lisp_Object tem;
  Lisp_Object cbuf = Fcurrent_buffer ();

  if (NILP (stream))
    tem = Qsymbolp;             /* #### #@[]*&$#*[& SI:NULL-STREAM */
  else
    tem = stream;
  specbind (Qstandard_output, tem);

  if (NILP (stream))
    record_unwind_protect (save_excursion_restore, save_excursion_save ());
  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  /* This both uses start and checks its type.  */
  Fgoto_char (start, cbuf);
  Fnarrow_to_region (make_int (BUF_BEGV (current_buffer)), end, cbuf);
  readevalloop (cbuf, XBUFFER (cbuf)->filename, Feval,
		!NILP (stream));

  return unbind_to (speccount, Qnil);
}

DEFUN ("read", Fread, 0, 1, 0, /*
Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' (which see).
STREAM or the value of `standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it).
*/
       (stream))
{
  if (NILP (stream))
    stream = Vstandard_input;
  if (EQ (stream, Qt))
    stream = Qread_char;

  Vread_objects = Qnil;

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  Vcurrent_compiled_function_annotation = Qnil;
#endif
  if (EQ (stream, Qread_char))
    {
      Lisp_Object val = call1 (Qread_from_minibuffer,
			       build_translated_string ("Lisp expression: "));
      return Fcar (Fread_from_string (val, Qnil, Qnil));
    }

  if (STRINGP (stream))
    return Fcar (Fread_from_string (stream, Qnil, Qnil));

  return read0 (stream);
}

DEFUN ("read-from-string", Fread_from_string, 1, 3, 0, /*
Read one Lisp expression which is represented as text by STRING.
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
START and END optionally delimit a substring of STRING from which to read;
 they default to 0 and (length STRING) respectively.
*/
       (string, start, end))
{
  Bytecount startval, endval;
  Lisp_Object tem;
  Lisp_Object lispstream = Qnil;
  struct gcpro gcpro1;

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  Vcurrent_compiled_function_annotation = Qnil;
#endif
  GCPRO1 (lispstream);
  CHECK_STRING (string);
  get_string_range_byte (string, start, end, &startval, &endval,
			 GB_HISTORICAL_STRING_BEHAVIOR);
  lispstream = make_lisp_string_input_stream (string, startval,
					      endval - startval);

  Vread_objects = Qnil;

  tem = read0 (lispstream);
  /* Yeah, it's ugly.  Gonna make something of it?
     At least our reader is reentrant ... */
  tem =
    (Fcons (tem, make_int
	    (bytecount_to_charcount
	     (XSTRING_DATA (string),
	      startval + Lstream_byte_count (XLSTREAM (lispstream))))));
  Lstream_delete (XLSTREAM (lispstream));
  UNGCPRO;
  return tem;
}


#ifdef LISP_BACKQUOTES

static Lisp_Object
backquote_unwind (Lisp_Object ptr)
{  /* used as unwind-protect function in read0() */
  int *counter = (int *) get_opaque_ptr (ptr);
  if (--*counter < 0)
    *counter = 0;
  free_opaque_ptr (ptr);
  return Qnil;
}

#endif

/* Use this for recursive reads, in contexts where internal tokens
   are not allowed.  See also read1(). */
static Lisp_Object
read0 (Lisp_Object readcharfun)
{
  Lisp_Object val = read1 (readcharfun);

  if (CONSP (val) && UNBOUNDP (XCAR (val)))
    {
      Emchar c = XCHAR (XCDR (val));
      free_cons (XCONS (val));
      return Fsignal (Qinvalid_read_syntax,
		      list1 (Fchar_to_string (make_char (c))));
    }

  return val;
}

static Emchar
read_escape (Lisp_Object readcharfun)
{
  /* This function can GC */
  Emchar c = readchar (readcharfun);

  if (c < 0)
    signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));

  switch (c)
    {
    case 'a': return '\007';
    case 'b': return '\b';
    case 'd': return 0177;
    case 'e': return 033;
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';
    case '\n': return -1;

    case 'M':
      c = readchar (readcharfun);
      if (c < 0)
	signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));
      if (c != '-')
	error ("Invalid escape character syntax");
      c = readchar (readcharfun);
      if (c < 0)
	signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | 0200;

      /* Originally, FSF_KEYS provided a degree of FSF Emacs
	 compatibility by defining character "modifiers" alt, super,
	 hyper and shift to infest the characters (i.e. integers).

	 However, this doesn't cut it for XEmacs 20, which
	 distinguishes characters from integers.  Without Mule, ?\H-a
	 simply returns ?a because every character is clipped into
	 0-255.  Under Mule it is much worse -- ?\H-a with FSF_KEYS
	 produces an illegal character, and moves us to crash-land.

         For these reasons, FSF_KEYS hack is useless and without hope
         of ever working under XEmacs 20.  */
#undef FSF_KEYS

#ifdef FSF_KEYS
#define alt_modifier   (0x040000)
#define super_modifier (0x080000)
#define hyper_modifier (0x100000)
#define shift_modifier (0x200000)
/* fsf uses a different modifiers for meta and control.  Possibly
   byte_compiled code will still work fsfmacs, though... --Stig

   #define ctl_modifier   (0x400000)
   #define meta_modifier  (0x800000)
*/
#define FSF_LOSSAGE(mask)							\
      if (fail_on_bucky_bit_character_escapes ||				\
	  ((c = readchar (readcharfun)) != '-'))				\
	error ("Invalid escape character syntax");				\
      c = readchar (readcharfun);						\
      if (c < 0)								\
	signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));	\
      if (c == '\\')								\
	c = read_escape (readcharfun);						\
      return c | mask

    case 'S': FSF_LOSSAGE (shift_modifier);
    case 'H': FSF_LOSSAGE (hyper_modifier);
    case 'A': FSF_LOSSAGE (alt_modifier);
    case 's': FSF_LOSSAGE (super_modifier);
#undef alt_modifier
#undef super_modifier
#undef hyper_modifier
#undef shift_modifier
#undef FSF_LOSSAGE

#endif /* FSF_KEYS */

    case 'C':
      c = readchar (readcharfun);
      if (c < 0)
	signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));
      if (c != '-')
	error ("Invalid escape character syntax");
    case '^':
      c = readchar (readcharfun);
      if (c < 0)
	signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));
      if (c == '\\')
	c = read_escape (readcharfun);
      /* FSFmacs junk for non-ASCII controls.
	 Not used here. */
      if (c == '?')
	return 0177;
      else
        return c & (0200 | 037);

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      /* An octal escape, as in ANSI C.  */
      {
	REGISTER Emchar i = c - '0';
	REGISTER int count = 0;
	while (++count < 3)
	  {
	    if ((c = readchar (readcharfun)) >= '0' && c <= '7')
              i = (i << 3) + (c - '0');
	    else
	      {
		unreadchar (readcharfun, c);
		break;
	      }
	  }
	return i;
      }

    case 'x':
      /* A hex escape, as in ANSI C, except that we only allow latin-1
	 characters to be read this way.  What is "\x4e03" supposed to
	 mean, anyways, if the internal representation is hidden?
         This is also consistent with the treatment of octal escapes. */
      {
	REGISTER Emchar i = 0;
	REGISTER int count = 0;
	while (++count <= 2)
	  {
	    c = readchar (readcharfun);
	    /* Remember, can't use isdigit(), isalpha() etc. on Emchars */
	    if      (c >= '0' && c <= '9')  i = (i << 4) + (c - '0');
	    else if (c >= 'a' && c <= 'f')  i = (i << 4) + (c - 'a') + 10;
            else if (c >= 'A' && c <= 'F')  i = (i << 4) + (c - 'A') + 10;
	    else
	      {
		unreadchar (readcharfun, c);
		break;
	      }
	  }
	return i;
      }

#ifdef MULE
      /* #### need some way of reading an extended character with
	 an escape sequence. */
#endif

    default:
	return c;
    }
}



/* read symbol-constituent stuff into `Vread_buffer_stream'. */
static Bytecount
read_atom_0 (Lisp_Object readcharfun, Emchar firstchar, int *saw_a_backslash)
{
  /* This function can GC */
  Emchar c = ((firstchar) >= 0 ? firstchar : readchar (readcharfun));
  Lstream_rewind (XLSTREAM (Vread_buffer_stream));

  *saw_a_backslash = 0;

  while (c > 040	/* #### - comma should be here as should backquote */
         && !(c == '\"' || c == '\'' || c == ';'
              || c == '(' || c == ')'
#ifndef LISP_FLOAT_TYPE
	      /* If we have floating-point support, then we need
		 to allow <digits><dot><digits>.  */
	      || c =='.'
#endif /* not LISP_FLOAT_TYPE */
              || c == '[' || c == ']' || c == '#'
              ))
    {
      if (c == '\\')
	{
	  c = readchar (readcharfun);
	  if (c < 0)
	    signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));
	  *saw_a_backslash = 1;
	}
      Lstream_put_emchar (XLSTREAM (Vread_buffer_stream), c);
      QUIT;
      c = readchar (readcharfun);
    }

  if (c >= 0)
    unreadchar (readcharfun, c);
  /* blasted terminating 0 */
  Lstream_put_emchar (XLSTREAM (Vread_buffer_stream), 0);
  Lstream_flush (XLSTREAM (Vread_buffer_stream));

  return Lstream_byte_count (XLSTREAM (Vread_buffer_stream)) - 1;
}

static Lisp_Object parse_integer (const Bufbyte *buf, Bytecount len, int base);

static Lisp_Object
read_atom (Lisp_Object readcharfun,
           Emchar firstchar,
           int uninterned_symbol)
{
  /* This function can GC */
  int saw_a_backslash;
  Bytecount len = read_atom_0 (readcharfun, firstchar, &saw_a_backslash);
  char *read_ptr = (char *)
    resizing_buffer_stream_ptr (XLSTREAM (Vread_buffer_stream));

  /* Is it an integer? */
  if (! (saw_a_backslash || uninterned_symbol))
    {
      /* If a token had any backslashes in it, it is disqualified from
	 being an integer or a float.  This means that 123\456 is a
	 symbol, as is \123 (which is the way (intern "123") prints).
	 Also, if token was preceded by #:, it's always a symbol.
       */
      char *p = read_ptr + len;
      char *p1 = read_ptr;

      if (*p1 == '+' || *p1 == '-') p1++;
      if (p1 != p)
	{
          int c;

          while (p1 != p && (c = *p1) >= '0' && c <= '9')
            p1++;
#ifdef LISP_FLOAT_TYPE
	  /* Integers can have trailing decimal points.  */
	  if (p1 > read_ptr && p1 < p && *p1 == '.')
	    p1++;
#endif
          if (p1 == p)
            {
              /* It is an integer. */
#ifdef LISP_FLOAT_TYPE
	      if (p1[-1] == '.')
		p1[-1] = '\0';
#endif
#if 0
	      {
		int number = 0;
		if (sizeof (int) == sizeof (EMACS_INT))
		  number = atoi (read_buffer);
		else if (sizeof (long) == sizeof (EMACS_INT))
		  number = atol (read_buffer);
		else
		  abort ();
		return make_int (number);
	      }
#else
              return parse_integer ((Bufbyte *) read_ptr, len, 10);
#endif
	    }
	}
#ifdef LISP_FLOAT_TYPE
      if (isfloat_string (read_ptr))
	return make_float (atof (read_ptr));
#endif
    }

  {
    Lisp_Object sym;
    if (uninterned_symbol)
      sym = Fmake_symbol ( make_string ((Bufbyte *) read_ptr, len));
    else
      {
	Lisp_Object name = make_string ((Bufbyte *) read_ptr, len);
	sym = Fintern (name, Qnil);
      }
    return sym;
  }
}


static Lisp_Object
parse_integer (const Bufbyte *buf, Bytecount len, int base)
{
  const Bufbyte *lim = buf + len;
  const Bufbyte *p = buf;
  EMACS_UINT num = 0;
  int negativland = 0;

  if (*p == '-')
    {
      negativland = 1;
      p++;
    }
  else if (*p == '+')
    {
      p++;
    }

  if (p == lim)
    goto loser;

  for (; (p < lim) && (*p != '\0'); p++)
    {
      int c = *p;
      EMACS_UINT onum;

      if (isdigit (c))
	c = c - '0';
      else if (isupper (c))
	c = c - 'A' + 10;
      else if (islower (c))
	c = c - 'a' + 10;
      else
	goto loser;

      if (c < 0 || c >= base)
	goto loser;

      onum = num;
      num = num * base + c;
      if (num < onum)
	goto overflow;
    }

  {
    EMACS_INT int_result = negativland ? - (EMACS_INT) num : (EMACS_INT) num;
    Lisp_Object result = make_int (int_result);
    if (num && ((XINT (result) < 0) != negativland))
      goto overflow;
    if (XINT (result) != int_result)
      goto overflow;
    return result;
  }
 overflow:
  return Fsignal (Qinvalid_read_syntax,
                  list3 (build_translated_string
			 ("Integer constant overflow in reader"),
                         make_string (buf, len),
                         make_int (base)));
 loser:
  return Fsignal (Qinvalid_read_syntax,
                  list3 (build_translated_string
			 ("Invalid integer constant in reader"),
                         make_string (buf, len),
                         make_int (base)));
}


static Lisp_Object
read_integer (Lisp_Object readcharfun, int base)
{
  /* This function can GC */
  int saw_a_backslash;
  Bytecount len = read_atom_0 (readcharfun, -1, &saw_a_backslash);
  return (parse_integer
	  (resizing_buffer_stream_ptr (XLSTREAM (Vread_buffer_stream)),
	   ((saw_a_backslash)
	    ? 0 /* make parse_integer signal error */
	    : len),
	   base));
}

static Lisp_Object
read_bit_vector (Lisp_Object readcharfun)
{
  unsigned_char_dynarr *dyn = Dynarr_new (unsigned_char);
  Lisp_Object val;

  while (1)
    {
      unsigned char bit;
      Emchar c = readchar (readcharfun);
      if (c == '0')
	bit = 0;
      else if (c == '1')
	bit = 1;
      else
	{
	  if (c >= 0)
	    unreadchar (readcharfun, c);
	  break;
	}
      Dynarr_add (dyn, bit);
    }

  val = make_bit_vector_from_byte_vector (Dynarr_atp (dyn, 0),
					  Dynarr_length (dyn));
  Dynarr_free (dyn);

  return val;
}



/* structures */

struct structure_type *
define_structure_type (Lisp_Object type,
		       int (*validate) (Lisp_Object data,
					Error_behavior errb),
		       Lisp_Object (*instantiate) (Lisp_Object data))
{
  struct structure_type st;

  st.type = type;
  st.keywords = Dynarr_new (structure_keyword_entry);
  st.validate = validate;
  st.instantiate = instantiate;
  Dynarr_add (the_structure_type_dynarr, st);

  return Dynarr_atp (the_structure_type_dynarr,
		     Dynarr_length (the_structure_type_dynarr) - 1);
}

void
define_structure_type_keyword (struct structure_type *st, Lisp_Object keyword,
			       int (*validate) (Lisp_Object keyword,
						Lisp_Object value,
						Error_behavior errb))
{
  struct structure_keyword_entry en;

  en.keyword = keyword;
  en.validate = validate;
  Dynarr_add (st->keywords, en);
}

static struct structure_type *
recognized_structure_type (Lisp_Object type)
{
  int i;

  for (i = 0; i < Dynarr_length (the_structure_type_dynarr); i++)
    {
      struct structure_type *st = Dynarr_atp (the_structure_type_dynarr, i);
      if (EQ (st->type, type))
	return st;
    }

  return 0;
}

static Lisp_Object
read_structure (Lisp_Object readcharfun)
{
  Emchar c = readchar (readcharfun);
  Lisp_Object list = Qnil;
  Lisp_Object orig_list = Qnil;
  Lisp_Object already_seen = Qnil;
  int keyword_count;
  struct structure_type *st;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (orig_list, already_seen);
  if (c != '(')
    RETURN_UNGCPRO (continuable_read_syntax_error ("#s not followed by paren"));
  list = read_list (readcharfun, ')', 0, 0);
  orig_list = list;
  {
    int len = XINT (Flength (list));
    if (len == 0)
      RETURN_UNGCPRO (continuable_read_syntax_error
		      ("structure type not specified"));
    if (!(len & 1))
      RETURN_UNGCPRO
	(continuable_read_syntax_error
	 ("structures must have alternating keyword/value pairs"));
  }

  st = recognized_structure_type (XCAR (list));
  if (!st)
    RETURN_UNGCPRO (Fsignal (Qinvalid_read_syntax,
			     list2 (build_translated_string
				    ("unrecognized structure type"),
				    XCAR (list))));

  list = Fcdr (list);
  keyword_count = Dynarr_length (st->keywords);
  while (!NILP (list))
    {
      Lisp_Object keyword, value;
      int i;
      struct structure_keyword_entry *en = NULL;

      keyword = Fcar (list);
      list = Fcdr (list);
      value = Fcar (list);
      list = Fcdr (list);

      if (!NILP (memq_no_quit (keyword, already_seen)))
	RETURN_UNGCPRO (Fsignal (Qinvalid_read_syntax,
				 list2 (build_translated_string
					("structure keyword already seen"),
					keyword)));

      for (i = 0; i < keyword_count; i++)
	{
	  en = Dynarr_atp (st->keywords, i);
	  if (EQ (keyword, en->keyword))
	    break;
	}

      if (i == keyword_count)
	RETURN_UNGCPRO (Fsignal (Qinvalid_read_syntax,
				   list2 (build_translated_string
					  ("unrecognized structure keyword"),
					  keyword)));

      if (en->validate && ! (en->validate) (keyword, value, ERROR_ME))
	RETURN_UNGCPRO
	  (Fsignal (Qinvalid_read_syntax,
		    list3 (build_translated_string
			   ("invalid value for structure keyword"),
			   keyword, value)));

      already_seen = Fcons (keyword, already_seen);
    }

  if (st->validate && ! (st->validate) (orig_list, ERROR_ME))
    RETURN_UNGCPRO (Fsignal (Qinvalid_read_syntax,
			     list2 (build_translated_string
				    ("invalid structure initializer"),
				    orig_list)));

  RETURN_UNGCPRO ((st->instantiate) (XCDR (orig_list)));
}


static Lisp_Object read_compiled_function (Lisp_Object readcharfun,
					   int terminator);
static Lisp_Object read_vector (Lisp_Object readcharfun, int terminator);

/* Get the next character; filter out whitespace and comments */

static Emchar
reader_nextchar (Lisp_Object readcharfun)
{
  /* This function can GC */
  Emchar c;

 retry:
  QUIT;
  c = readchar (readcharfun);
  if (c < 0)
    signal_error (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));

  switch (c)
    {
    default:
      {
	/* Ignore whitespace and control characters */
	if (c <= 040)
	  goto retry;
	return c;
      }

    case ';':
      {
        /* Comment */
        while ((c = readchar (readcharfun)) >= 0 && c != '\n')
          QUIT;
        goto retry;
      }
    }
}

#if 0
static Lisp_Object
list2_pure (int pure, Lisp_Object a, Lisp_Object b)
{
  return pure ? pure_cons (a, pure_cons (b, Qnil)) : list2 (a, b);
}
#endif

/* Read the next Lisp object from the stream READCHARFUN and return it.
   If the return value is a cons whose car is Qunbound, then read1()
   encountered a misplaced token (e.g. a right bracket, right paren,
   or dot followed by a non-number).  To filter this stuff out,
   use read0(). */

static Lisp_Object
read1 (Lisp_Object readcharfun)
{
  Emchar c;

retry:
  c = reader_nextchar (readcharfun);

  switch (c)
    {
    case '(':
      {
#ifdef LISP_BACKQUOTES	/* old backquote compatibility in lisp reader */
	/* if this is disabled, then other code in eval.c must be enabled */
	Emchar ch = reader_nextchar (readcharfun);
	switch (ch)
	  {
	  case '`':
	    {
	      Lisp_Object tem;
	      int speccount = specpdl_depth ();
	      ++old_backquote_flag;
	      record_unwind_protect (backquote_unwind,
				     make_opaque_ptr (&old_backquote_flag));
	      tem = read0 (readcharfun);
	      unbind_to (speccount, Qnil);
	      ch = reader_nextchar (readcharfun);
	      if (ch != ')')
		{
		  unreadchar (readcharfun, ch);
		  return Fsignal (Qinvalid_read_syntax,
				  list1 (build_string
					 ("Weird old-backquote syntax")));
		}
	      return list2 (Qbacktick, tem);
	    }
	  case ',':
	    {
	      if (old_backquote_flag)
		{
		  Lisp_Object tem, comma_type;
		  ch = readchar (readcharfun);
		  if (ch == '@')
		    comma_type = Qcomma_at;
		  else
		    {
		      if (ch >= 0)
			unreadchar (readcharfun, ch);
		      comma_type = Qcomma;
		    }
		  tem = read0 (readcharfun);
		  ch = reader_nextchar (readcharfun);
		  if (ch != ')')
		    {
		      unreadchar (readcharfun, ch);
		      return Fsignal (Qinvalid_read_syntax,
				      list1 (build_string
					     ("Weird old-backquote syntax")));
		    }
		  return list2 (comma_type, tem);
		}
	      else
		{
		  unreadchar (readcharfun, ch);
#if 0
		  return Fsignal (Qinvalid_read_syntax,
		       list1 (build_string ("Comma outside of backquote")));
#else
		  /* #### - yuck....but this is reverse compatible. */
		  /* mostly this is required by edebug, which does its own
		     annotated reading.  We need to have an annotated_read
		     function that records (with markers) the buffer
		     positions of the elements that make up lists, then that
		     can be used in edebug and bytecomp and the check above
		     can go back in. --Stig */
		  break;
#endif
		}
	    }
	  default:
	    unreadchar (readcharfun, ch);
	  }			/* switch(ch) */
#endif /* old backquote crap... */
	return read_list (readcharfun, ')', 1, 1);
      }
    case '[':
      return read_vector (readcharfun, ']');

    case ')':
    case ']':
      /* #### - huh? these don't do what they seem... */
      return noseeum_cons (Qunbound, make_char (c));
    case '.':
      {
#ifdef LISP_FLOAT_TYPE
	/* If a period is followed by a number, then we should read it
	   as a floating point number.  Otherwise, it denotes a dotted
	   pair.
	 */
	c = readchar (readcharfun);
	unreadchar (readcharfun, c);

	/* Can't use isdigit on Emchars */
	if (c < '0' || c > '9')
	  return noseeum_cons (Qunbound, make_char ('.'));

	/* Note that read_atom will loop
	   at least once, assuring that we will not try to UNREAD
           two characters in a row.
	   (I think this doesn't matter anymore because there should
	   be no more danger in unreading multiple characters) */
        return read_atom (readcharfun, '.', 0);

#else /* ! LISP_FLOAT_TYPE */
	return noseeum_cons (Qunbound, make_char ('.'));
#endif /* ! LISP_FLOAT_TYPE */
      }

    case '#':
      {
	c = readchar (readcharfun);
	switch (c)
	  {
#if 0 /* FSFmacs silly char-table syntax */
	  case '^':
#endif
#if 0 /* FSFmacs silly bool-vector syntax */
	  case '&':
#endif
            /* "#["-- byte-code constant syntax */
            /* purecons #[...] syntax */
	  case '[': return read_compiled_function (readcharfun, ']'
						   /*, purify_flag */ );
            /* "#:"-- gensym syntax */
	  case ':': return read_atom (readcharfun, -1, 1);
            /* #'x => (function x) */
	  case '\'': return list2 (Qfunction, read0 (readcharfun));
#if 0
	    /* RMS uses this syntax for fat-strings.
	       If we use it for vectors, then obscure bugs happen.
	     */
            /* "#(" -- Scheme/CL vector syntax */
	  case '(': return read_vector (readcharfun, ')');
#endif
#if 0 /* FSFmacs */
	  case '(':
	    {
	      Lisp_Object tmp;
	      struct gcpro gcpro1;

	      /* Read the string itself.  */
	      tmp = read1 (readcharfun);
	      if (!STRINGP (tmp))
		{
		  if (CONSP (tmp) && UNBOUNDP (XCAR (tmp)))
		    free_cons (XCONS (tmp));
		  return Fsignal (Qinvalid_read_syntax,
				   list1 (build_string ("#")));
		}
	      GCPRO1 (tmp);
	      /* Read the intervals and their properties.  */
	      while (1)
		{
		  Lisp_Object beg, end, plist;
		  Emchar ch;
		  int invalid = 0;

		  beg = read1 (readcharfun);
		  if (CONSP (beg) && UNBOUNDP (XCAR (beg)))
		    {
		      ch = XCHAR (XCDR (beg));
		      free_cons (XCONS (beg));
		      if (ch == ')')
			break;
		      else
			invalid = 1;
		    }
		  if (!invalid)
		    {
		      end = read1 (readcharfun);
		      if (CONSP (end) && UNBOUNDP (XCAR (end)))
			{
			  free_cons (XCONS (end));
			  invalid = 1;
			}
		    }
		  if (!invalid)
		    {
		      plist = read1 (readcharfun);
		      if (CONSP (plist) && UNBOUNDP (XCAR (plist)))
			{
			  free_cons (XCONS (plist));
			  invalid = 1;
			}
		    }
		  if (invalid)
		    RETURN_UNGCPRO
		      (Fsignal (Qinvalid_read_syntax,
				list2
				(build_string ("invalid string property list"),
				 XCDR (plist))));
		  Fset_text_properties (beg, end, plist, tmp);
		}
	      UNGCPRO;
	      return tmp;
	    }
#endif /* 0 */
	  case '@':
	    {
	      /* #@NUMBER is used to skip NUMBER following characters.
		 That's used in .elc files to skip over doc strings
		 and function definitions.  */
	      int i, nskip = 0;

	      /* Read a decimal integer.  */
	      while ((c = readchar (readcharfun)) >= 0
		     && c >= '0' && c <= '9')
                nskip = (10 * nskip) + (c - '0');
	      if (c >= 0)
		unreadchar (readcharfun, c);

	      /* FSF has code here that maybe caches the skipped
		 string.  See above for why this is totally
		 losing.  We handle this differently. */

	      /* Skip that many characters.  */
	      for (i = 0; i < nskip && c >= 0; i++)
		c = readchar (readcharfun);

	      goto retry;
	    }
	  case '$': return Vload_file_name_internal;
            /* bit vectors */
	  case '*': return read_bit_vector (readcharfun);
            /* #o10 => 8 -- octal constant syntax */
	  case 'o': return read_integer (readcharfun, 8);
            /* #xdead => 57005 -- hex constant syntax */
	  case 'x': return read_integer (readcharfun, 16);
            /* #b010 => 2 -- binary constant syntax */
	  case 'b': return read_integer (readcharfun, 2);
            /* #s(foobar key1 val1 key2 val2) -- structure syntax */
	  case 's': return read_structure (readcharfun);
	  case '<':
	    {
	      unreadchar (readcharfun, c);
	      return Fsignal (Qinvalid_read_syntax,
		    list1 (build_string ("Cannot read unreadable object")));
	    }
#ifdef FEATUREP_SYNTAX
	  case '+':
	  case '-':
	    {
	      Lisp_Object feature_exp, obj, tem;
	      struct gcpro gcpro1, gcpro2;

	      feature_exp = read0(readcharfun);
	      obj = read0(readcharfun);

	      /* the call to `featurep' may GC. */
	      GCPRO2 (feature_exp, obj);
	      tem = call1 (Qfeaturep, feature_exp);
	      UNGCPRO;

	      if (c == '+' &&  NILP(tem)) goto retry;
	      if (c == '-' && !NILP(tem)) goto retry;
	      return obj;
	    }
#endif
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    /* Reader forms that can reuse previously read objects.  */
	    {
	      int n = 0;
	      Lisp_Object found;

	      /* Using read_integer() here is impossible, because it
                 chokes on `='.  Using parse_integer() is too hard.
                 So we simply read it in, and ignore overflows, which
                 is safe.  */
	      while (c >= '0' && c <= '9')
		{
		  n *= 10;
		  n += c - '0';
		  c = readchar (readcharfun);
		}
	      found = assq_no_quit (make_int (n), Vread_objects);
	      if (c == '=')
		{
		  /* #n=object returns object, but associates it with
		     n for #n#.  */
		  Lisp_Object obj;
		  if (CONSP (found))
		    return Fsignal (Qinvalid_read_syntax,
				    list2 (build_translated_string
					   ("Multiply defined symbol label"),
					   make_int (n)));
		  obj = read0 (readcharfun);
		  Vread_objects = Fcons (Fcons (make_int (n), obj),
					 Vread_objects);
		  return obj;
		}
	      else if (c == '#')
		{
		  /* #n# returns a previously read object.  */
		  if (CONSP (found))
		    return XCDR (found);
		  else
		    return Fsignal (Qinvalid_read_syntax,
				    list2 (build_translated_string
					   ("Undefined symbol label"),
					   make_int (n)));
		}
	      return Fsignal (Qinvalid_read_syntax,
			      list1 (build_string ("#")));
	    }
	  default:
	    {
	      unreadchar (readcharfun, c);
	      return Fsignal (Qinvalid_read_syntax,
			      list1 (build_string ("#")));
	    }
	  }
      }

      /* Quote */
    case '\'': return list2 (Qquote, read0 (readcharfun));

#ifdef LISP_BACKQUOTES
    case '`':
      {
	Lisp_Object tem;
	int speccount = specpdl_depth ();
	++new_backquote_flag;
	record_unwind_protect (backquote_unwind,
			       make_opaque_ptr (&new_backquote_flag));
	tem = read0 (readcharfun);
	unbind_to (speccount, Qnil);
	return list2 (Qbackquote, tem);
      }

    case ',':
      {
	if (new_backquote_flag)
	  {
	    Lisp_Object comma_type = Qnil;
	    int ch = readchar (readcharfun);

	    if (ch == '@')
	      comma_type = Qcomma_at;
	    else if (ch == '.')
	      comma_type = Qcomma_dot;
	    else
	      {
		if (ch >= 0)
		  unreadchar (readcharfun, ch);
		comma_type = Qcomma;
	      }
	    return list2 (comma_type, read0 (readcharfun));
	  }
	else
	  {
	    /* YUCK.  99.999% backwards compatibility.  The Right
	       Thing(tm) is to signal an error here, because it's
	       really invalid read syntax.  Instead, this permits
	       commas to begin symbols (unless they're inside
	       backquotes).  If an error is signalled here in the
	       future, then commas should be invalid read syntax
	       outside of backquotes anywhere they're found (i.e.
	       they must be quoted in symbols) -- Stig */
	    return read_atom (readcharfun, c, 0);
	  }
      }
#endif

    case '?':
      {
	/* Evil GNU Emacs "character" (ie integer) syntax */
	c = readchar (readcharfun);
	if (c < 0)
	  return Fsignal (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));

	if (c == '\\')
	  c = read_escape (readcharfun);
	return make_char (c);
      }

    case '\"':
      {
	/* String */
#ifdef I18N3
	/* #### If the input stream is translating, then the string
	   should be marked as translatable by setting its
	   `string-translatable' property to t.  .el and .elc files
	   normally are translating input streams.  See Fgettext()
	   and print_internal(). */
#endif
	int cancel = 0;

	Lstream_rewind (XLSTREAM (Vread_buffer_stream));
	while ((c = readchar (readcharfun)) >= 0
	       && c != '\"')
	  {
	    if (c == '\\')
	      c = read_escape (readcharfun);
	    /* c is -1 if \ newline has just been seen */
	    if (c == -1)
	      {
		if (Lstream_byte_count (XLSTREAM (Vread_buffer_stream)) == 0)
		  cancel = 1;
	      }
	    else
	      Lstream_put_emchar (XLSTREAM (Vread_buffer_stream), c);
	    QUIT;
	  }
	if (c < 0)
	  return Fsignal (Qend_of_file, list1 (READCHARFUN_MAYBE (readcharfun)));

	/* If purifying, and string starts with \ newline,
	   return zero instead.  This is for doc strings
	   that we are really going to find in lib-src/DOC.nn.nn  */
	if (purify_flag && NILP (Vinternal_doc_file_name) && cancel)
	  return Qzero;

	Lstream_flush (XLSTREAM (Vread_buffer_stream));
	return
	  make_string
	  (resizing_buffer_stream_ptr (XLSTREAM (Vread_buffer_stream)),
	   Lstream_byte_count (XLSTREAM (Vread_buffer_stream)));
      }

    default:
      {
	/* Ignore whitespace and control characters */
	if (c <= 040)
	  goto retry;
	return read_atom (readcharfun, c, 0);
      }
    }
}



#ifdef LISP_FLOAT_TYPE

#define LEAD_INT 1
#define DOT_CHAR 2
#define TRAIL_INT 4
#define E_CHAR 8
#define EXP_INT 16

int
isfloat_string (const char *cp)
{
  int state = 0;
  const Bufbyte *ucp = (const Bufbyte *) cp;

  if (*ucp == '+' || *ucp == '-')
    ucp++;

  if (*ucp >= '0' && *ucp <= '9')
    {
      state |= LEAD_INT;
      while (*ucp >= '0' && *ucp <= '9')
	ucp++;
    }
  if (*ucp == '.')
    {
      state |= DOT_CHAR;
      ucp++;
    }
  if (*ucp >= '0' && *ucp <= '9')
    {
      state |= TRAIL_INT;
      while (*ucp >= '0' && *ucp <= '9')
	ucp++;
    }
  if (*ucp == 'e' || *ucp == 'E')
    {
      state |= E_CHAR;
      ucp++;
      if ((*ucp == '+') || (*ucp == '-'))
	ucp++;
    }

  if (*ucp >= '0' && *ucp <= '9')
    {
      state |= EXP_INT;
      while (*ucp >= '0' && *ucp <= '9')
	ucp++;
    }
  return (((*ucp == 0) || (*ucp == ' ') || (*ucp == '\t') || (*ucp == '\n')
	   || (*ucp == '\r') || (*ucp == '\f'))
	  && (state == (LEAD_INT|DOT_CHAR|TRAIL_INT)
	      || state == (DOT_CHAR|TRAIL_INT)
	      || state == (LEAD_INT|E_CHAR|EXP_INT)
	      || state == (LEAD_INT|DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)
	      || state == (DOT_CHAR|TRAIL_INT|E_CHAR|EXP_INT)));
}
#endif /* LISP_FLOAT_TYPE */

static void *
sequence_reader (Lisp_Object readcharfun,
                 Emchar terminator,
                 void *state,
                 void * (*conser) (Lisp_Object readcharfun,
                                   void *state, Charcount len))
{
  Charcount len;

  for (len = 0; ; len++)
    {
      Emchar ch;

      QUIT;
      ch = reader_nextchar (readcharfun);

      if (ch == terminator)
	return state;
      else
	unreadchar (readcharfun, ch);
#ifdef FEATUREP_SYNTAX
      if (ch == ']')
	read_syntax_error ("\"]\" in a list");
      else if (ch == ')')
	read_syntax_error ("\")\" in a vector");
#endif
      state = ((conser) (readcharfun, state, len));
    }
}


struct read_list_state
  {
    Lisp_Object head;
    Lisp_Object tail;
    int length;
    int allow_dotted_lists;
    Emchar terminator;
  };

static void *
read_list_conser (Lisp_Object readcharfun, void *state, Charcount len)
{
  struct read_list_state *s = (struct read_list_state *) state;
  Lisp_Object elt;

  elt = read1 (readcharfun);

  if (CONSP (elt) && UNBOUNDP (XCAR (elt)))
    {
      Lisp_Object tem = elt;
      Emchar ch;

      elt = XCDR (elt);
      free_cons (XCONS (tem));
      tem = Qnil;
      ch = XCHAR (elt);
#ifdef FEATUREP_SYNTAX
      if (ch == s->terminator) /* deal with #+, #- reader macros */
	{
	  unreadchar (readcharfun, s->terminator);
	  goto done;
	}
      else if (ch == ']')
	read_syntax_error ("']' in a list");
      else if (ch == ')')
	read_syntax_error ("')' in a vector");
      else
#endif
      if (ch != '.')
	signal_simple_error ("BUG! Internal reader error", elt);
      else if (!s->allow_dotted_lists)
	read_syntax_error ("\".\" in a vector");
      else
	{
	  if (!NILP (s->tail))
	    XCDR (s->tail) = read0 (readcharfun);
          else
	    s->head = read0 (readcharfun);
	  elt = read1 (readcharfun);
	  if (CONSP (elt) && UNBOUNDP (XCAR (elt)))
	    {
	      ch = XCHAR (XCDR (elt));
	      free_cons (XCONS (elt));
	      if (ch == s->terminator)
		{
		  unreadchar (readcharfun, s->terminator);
		  goto done;
		}
	    }
	  read_syntax_error (". in wrong context");
	}
    }

#if 0 /* FSFmacs defun hack, or something ... */
  if (NILP (tail) && defun_hack && EQ (elt, Qdefun) && !read_pure)
    {
      record_unwind_protect (unreadpure, Qzero);
      read_pure = 1;
    }
#endif

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  if (s->length == 1 && s->allow_dotted_lists && EQ (XCAR (s->head), Qfset))
    {
      if (CONSP (elt) && EQ (XCAR (elt), Qquote) && CONSP (XCDR (elt)))
	Vcurrent_compiled_function_annotation = XCAR (XCDR (elt));
      else
	Vcurrent_compiled_function_annotation = elt;
    }
#endif

  elt = Fcons (elt, Qnil);
  if (!NILP (s->tail))
    XCDR (s->tail) = elt;
  else
    s->head = elt;
  s->tail = elt;
 done:
  s->length++;
  return s;
}


#if 0 /* FSFmacs defun hack */
/* -1 for allow_dotted_lists means allow_dotted_lists and check
   for starting with defun and make structure pure. */
#endif

static Lisp_Object
read_list (Lisp_Object readcharfun,
           Emchar terminator,
           int allow_dotted_lists,
	   int check_for_doc_references)
{
  struct read_list_state s;
  struct gcpro gcpro1, gcpro2;
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  Lisp_Object old_compiled_function_annotation =
    Vcurrent_compiled_function_annotation;
#endif

  s.head = Qnil;
  s.tail = Qnil;
  s.length = 0;
  s.allow_dotted_lists = allow_dotted_lists;
  s.terminator = terminator;
  GCPRO2 (s.head, s.tail);

  sequence_reader (readcharfun, terminator, &s, read_list_conser);
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  Vcurrent_compiled_function_annotation = old_compiled_function_annotation;
#endif

  if ((purify_flag || load_force_doc_strings) && check_for_doc_references)
    {
      /* check now for any doc string references and record them
	 for later. */
      Lisp_Object tail;

      /* We might be dealing with an imperfect list so don't
	 use LIST_LOOP */
      for (tail = s.head; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object holding_cons = Qnil;

	  {
	    Lisp_Object elem = XCAR (tail);
	    /* elem might be (#$ . INT) ... */
	    if (CONSP (elem) && EQ (XCAR (elem), Vload_file_name_internal))
	      holding_cons = tail;
	    /* or it might be (quote (#$ . INT)) i.e.
	       (quote . ((#$ . INT) . nil)) in the case of
	       `autoload' (autoload evaluates its arguments, while
	       `defvar', `defun', etc. don't). */
	    if (CONSP (elem) && EQ (XCAR (elem), Qquote)
		&& CONSP (XCDR (elem)))
	      {
		elem = XCAR (XCDR (elem));
		if (CONSP (elem) && EQ (XCAR (elem), Vload_file_name_internal))
		  holding_cons = XCDR (XCAR (tail));
	      }
	  }

	  if (CONSP (holding_cons))
	    {
	      if (purify_flag)
		{
		  if (NILP (Vinternal_doc_file_name))
		    /* We have not yet called Snarf-documentation, so
		       assume this file is described in the DOC file
		       and Snarf-documentation will fill in the right
		       value later.  For now, replace the whole list
		       with 0.  */
		    XCAR (holding_cons) = Qzero;
		  else
		    /* We have already called Snarf-documentation, so
		       make a relative file name for this file, so it
		       can be found properly in the installed Lisp
		       directory.  We don't use Fexpand_file_name
		       because that would make the directory absolute
		       now.  */
		    XCAR (XCAR (holding_cons)) =
		      concat2 (build_string ("../lisp/"),
			       Ffile_name_nondirectory
			       (Vload_file_name_internal));
		}
	      else
		/* Not pure.  Just add to Vload_force_doc_string_list,
		   and the string will be filled in properly in
		   load_force_doc_string_unwind(). */
		Vload_force_doc_string_list =
		  /* We pass the cons that holds the (#$ . INT) so we
		     can modify it in-place. */
		  Fcons (holding_cons, Vload_force_doc_string_list);
	    }
	}
    }

  UNGCPRO;
  return s.head;
}

static Lisp_Object
read_vector (Lisp_Object readcharfun,
             Emchar terminator)
{
  Lisp_Object tem;
  Lisp_Object *p;
  int len;
  int i;
  struct read_list_state s;
  struct gcpro gcpro1, gcpro2;

  s.head = Qnil;
  s.tail = Qnil;
  s.length = 0;
  s.allow_dotted_lists = 0;
  GCPRO2 (s.head, s.tail);

  sequence_reader (readcharfun, terminator, &s, read_list_conser);

  UNGCPRO;
  tem = s.head;
  len = XINT (Flength (tem));

#if 0 /* FSFmacs defun hack */
  if (read_pure)
    s.head = make_pure_vector (len, Qnil);
  else
#endif
    s.head = make_vector (len, Qnil);

  for (i = 0, p = &(XVECTOR_DATA (s.head)[0]);
       i < len;
       i++, p++)
  {
    Lisp_Cons *otem = XCONS (tem);
    tem = Fcar (tem);
    *p = tem;
    tem = otem->cdr;
    free_cons (otem);
  }
  return s.head;
}

static Lisp_Object
read_compiled_function (Lisp_Object readcharfun, Emchar terminator)
{
  /* Accept compiled functions at read-time so that we don't
     have to build them at load-time. */
  Lisp_Object stuff;
  Lisp_Object make_byte_code_args[COMPILED_DOMAIN + 1];
  struct gcpro gcpro1;
  int len;
  int iii;
  int saw_a_doc_ref = 0;

  /* Note: we tell read_list not to search for doc references
     because we need to handle the "doc reference" for the
     instructions and constants differently. */
  stuff = read_list (readcharfun, terminator, 0, 0);
  len = XINT (Flength (stuff));
  if (len < COMPILED_STACK_DEPTH + 1 || len > COMPILED_DOMAIN + 1)
    return
      continuable_read_syntax_error ("#[...] used with wrong number of elements");

  for (iii = 0; CONSP (stuff); iii++)
    {
      Lisp_Cons *victim = XCONS (stuff);
      make_byte_code_args[iii] = Fcar (stuff);
      if ((purify_flag || load_force_doc_strings)
	   && CONSP (make_byte_code_args[iii])
	  && EQ (XCAR (make_byte_code_args[iii]), Vload_file_name_internal))
	{
	  if (purify_flag && iii == COMPILED_DOC_STRING)
	    {
	      /* same as in read_list(). */
	      if (NILP (Vinternal_doc_file_name))
		make_byte_code_args[iii] = Qzero;
	      else
		XCAR (make_byte_code_args[iii]) =
		  concat2 (build_string ("../lisp/"),
			   Ffile_name_nondirectory
			   (Vload_file_name_internal));
	    }
	  else
	    saw_a_doc_ref = 1;
	}
      stuff = Fcdr (stuff);
      free_cons (victim);
    }
  GCPRO1 (make_byte_code_args[0]);
  gcpro1.nvars = len;

  /* v18 or v19 bytecode file.  Need to Ebolify. */
  if (load_byte_code_version < 20 && VECTORP (make_byte_code_args[2]))
    ebolify_bytecode_constants (make_byte_code_args[2]);

  /* make-byte-code looks at purify_flag, which should have the same
   *  value as our "read-pure" argument */
  stuff = Fmake_byte_code (len, make_byte_code_args);
  XCOMPILED_FUNCTION (stuff)->flags.ebolified = (load_byte_code_version < 20);
  if (saw_a_doc_ref)
    Vload_force_doc_string_list = Fcons (stuff, Vload_force_doc_string_list);
  UNGCPRO;
  return stuff;
}



void
init_lread (void)
{
  Vvalues = Qnil;

  load_in_progress = 0;

  Vload_descriptor_list = Qnil;

  /* kludge: locate-file does not work for a null load-path, even if
     the file name is absolute. */

  Vload_path = Fcons (build_string (""), Qnil);

  /* This used to get initialized in init_lread because all streams
     got closed when dumping occurs.  This is no longer true --
     Vread_buffer_stream is a resizing output stream, and there is no
     reason to close it at dump-time.

     Vread_buffer_stream is set to Qnil in vars_of_lread, and this
     will initialize it only once, at dump-time.  */
  if (NILP (Vread_buffer_stream))
    Vread_buffer_stream = make_resizing_buffer_output_stream ();

  Vload_force_doc_string_list = Qnil;
}

void
syms_of_lread (void)
{
  DEFSUBR (Fread);
  DEFSUBR (Fread_from_string);
  DEFSUBR (Fload_internal);
  DEFSUBR (Flocate_file);
  DEFSUBR (Flocate_file_clear_hashing);
  DEFSUBR (Feval_buffer);
  DEFSUBR (Feval_region);

  defsymbol (&Qstandard_input, "standard-input");
  defsymbol (&Qread_char, "read-char");
  defsymbol (&Qcurrent_load_list, "current-load-list");
  defsymbol (&Qload, "load");
  defsymbol (&Qload_file_name, "load-file-name");
  defsymbol (&Qfset, "fset");

#ifdef LISP_BACKQUOTES
  defsymbol (&Qbackquote, "backquote");
  defsymbol (&Qbacktick, "`");
  defsymbol (&Qcomma, ",");
  defsymbol (&Qcomma_at, ",@");
  defsymbol (&Qcomma_dot, ",.");
#endif

  defsymbol (&Qexists, "exists");
  defsymbol (&Qreadable, "readable");
  defsymbol (&Qwritable, "writable");
  defsymbol (&Qexecutable, "executable");
}

void
structure_type_create (void)
{
  the_structure_type_dynarr = Dynarr_new (structure_type);
}

void
reinit_vars_of_lread (void)
{
  Vread_buffer_stream = Qnil;
  staticpro_nodump (&Vread_buffer_stream);
}

void
vars_of_lread (void)
{
  reinit_vars_of_lread ();

  DEFVAR_LISP ("values", &Vvalues /*
List of values of all expressions which were read, evaluated and printed.
Order is reverse chronological.
*/ );

  DEFVAR_LISP ("standard-input", &Vstandard_input /*
Stream for read to get input from.
See documentation of `read' for possible values.
*/ );
  Vstandard_input = Qt;

  DEFVAR_LISP ("load-path", &Vload_path /*
*List of directories to search for files to load.
Each element is a string (directory name) or nil (try default directory).

Note that the elements of this list *may not* begin with "~", so you must
call `expand-file-name' on them before adding them to this list.

Initialized based on EMACSLOADPATH environment variable, if any,
otherwise to default specified in by file `paths.h' when XEmacs was built.
If there were no paths specified in `paths.h', then XEmacs chooses a default
value for this variable by looking around in the file-system near the
directory in which the XEmacs executable resides.
*/ );
  Vload_path = Qnil;

/*  xxxDEFVAR_LISP ("dump-load-path", &Vdump_load_path,
    "*Location of lisp files to be used when dumping ONLY."); */

  DEFVAR_BOOL ("load-in-progress", &load_in_progress /*
Non-nil iff inside of `load'.
*/ );

  DEFVAR_LISP ("after-load-alist", &Vafter_load_alist /*
An alist of expressions to be evalled when particular files are loaded.
Each element looks like (FILENAME FORMS...).
When `load' is run and the file-name argument is FILENAME,
the FORMS in the corresponding element are executed at the end of loading.

FILENAME must match exactly!  Normally FILENAME is the name of a library,
with no directory specified, since that is how `load' is normally called.
An error in FORMS does not undo the load,
but does prevent execution of the rest of the FORMS.
*/ );
  Vafter_load_alist = Qnil;

  DEFVAR_BOOL ("load-warn-when-source-newer", &load_warn_when_source_newer /*
*Whether `load' should check whether the source is newer than the binary.
If this variable is true, then when a `.elc' file is being loaded and the
corresponding `.el' is newer, a warning message will be printed.
*/ );
  load_warn_when_source_newer = 0;

  DEFVAR_BOOL ("load-warn-when-source-only", &load_warn_when_source_only /*
*Whether `load' should warn when loading a `.el' file instead of an `.elc'.
If this variable is true, then when `load' is called with a filename without
an extension, and the `.elc' version doesn't exist but the `.el' version does,
then a message will be printed.  If an explicit extension is passed to `load',
no warning will be printed.
*/ );
  load_warn_when_source_only = 0;

  DEFVAR_BOOL ("load-ignore-elc-files", &load_ignore_elc_files /*
*Whether `load' should ignore `.elc' files when a suffix is not given.
This is normally used only to bootstrap the `.elc' files when building XEmacs.
*/ );
  load_ignore_elc_files = 0;

#ifdef LOADHIST
  DEFVAR_LISP ("load-history", &Vload_history /*
Alist mapping source file names to symbols and features.
Each alist element is a list that starts with a file name,
except for one element (optional) that starts with nil and describes
definitions evaluated from buffers not visiting files.
The remaining elements of each list are symbols defined as functions
or variables, and cons cells `(provide . FEATURE)' and `(require . FEATURE)'.
*/ );
  Vload_history = Qnil;

  DEFVAR_LISP ("current-load-list", &Vcurrent_load_list /*
Used for internal purposes by `load'.
*/ );
  Vcurrent_load_list = Qnil;
#endif

  DEFVAR_LISP ("load-file-name", &Vload_file_name /*
Full name of file being loaded by `load'.
*/ );
  Vload_file_name = Qnil;

  DEFVAR_LISP ("load-read-function", &Vload_read_function /*
Function used by `load' and `eval-region' for reading expressions.
The default is nil, which means use the function `read'.
*/ );
  Vload_read_function = Qnil;

  DEFVAR_BOOL ("load-force-doc-strings", &load_force_doc_strings /*
Non-nil means `load' should force-load all dynamic doc strings.
This is useful when the file being loaded is a temporary copy.
*/ );
  load_force_doc_strings = 0;

  /* See read_escape().  */
#if 0
  /* Used to be named `puke-on-fsf-keys' */
  DEFVAR_BOOL ("fail-on-bucky-bit-character-escapes",
	       &fail_on_bucky_bit_character_escapes /*
Whether `read' should signal an error when it encounters unsupported
character escape syntaxes or just read them incorrectly.
*/ );
  fail_on_bucky_bit_character_escapes = 0;
#endif

  /* This must be initialized in init_lread otherwise it may start out
     with values saved when the image is dumped. */
  staticpro (&Vload_descriptor_list);

  /* Initialized in init_lread. */
  staticpro (&Vload_force_doc_string_list);

  Vload_file_name_internal = Qnil;
  staticpro (&Vload_file_name_internal);

  Vload_file_name_internal_the_purecopy = Qnil;
  staticpro (&Vload_file_name_internal_the_purecopy);

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  Vcurrent_compiled_function_annotation = Qnil;
  staticpro (&Vcurrent_compiled_function_annotation);
#endif

  /* So that early-early stuff will work */
  Ffset (Qload, intern ("load-internal"));

#ifdef FEATUREP_SYNTAX
  defsymbol (&Qfeaturep, "featurep");
  Fprovide(intern("xemacs"));
#ifdef INFODOCK
  Fprovide(intern("infodock"));
#endif /* INFODOCK */
#endif /* FEATUREP_SYNTAX */

#ifdef LISP_BACKQUOTES
  old_backquote_flag = new_backquote_flag = 0;
#endif

#ifdef I18N3
  Vfile_domain = Qnil;
#endif

  Vread_objects = Qnil;
  staticpro (&Vread_objects);

  Vlocate_file_hash_table = make_lisp_hash_table (200,
						  HASH_TABLE_NON_WEAK,
						  HASH_TABLE_EQUAL);
  staticpro (&Vlocate_file_hash_table);
#ifdef DEBUG_XEMACS
  symbol_value (XSYMBOL (intern ("Vlocate-file-hash-table")))
    = Vlocate_file_hash_table;
#endif
}
