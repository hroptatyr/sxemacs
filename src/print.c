/* Lisp object printing and output streams.
   Copyright (C) 1985, 1986, 1988, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2000, 2002 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* This file has been Mule-ized. */

/* Seriously hacked on by Ben Wing for Mule. */

#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "console-tty.h"
#include "console-stream.h"
#include "extents.h"
#include "frame.h"
#include "insdel.h"
#include "lstream.h"
#include "sysfile.h"
#ifdef WIN32_NATIVE
#include "console-msw.h"
#endif

#include <float.h>
/* Define if not in float.h */
#ifndef DBL_DIG
#define DBL_DIG 16
#endif

Lisp_Object Vstandard_output, Qstandard_output;

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output, Qalternate_debugging_output;

#ifdef HAVE_MS_WINDOWS
Lisp_Object Qmswindows_debugging_output;
#endif

/* Avoid actual stack overflow in print.  */
static int print_depth;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE 200
static Lisp_Object being_printed[PRINT_CIRCLE];

/* Maximum length of list or vector to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_length;
Lisp_Object Qprint_length;

/* Maximum length of string to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_string_length;
Lisp_Object Qprint_string_length;

/* Maximum depth of list to print in full; noninteger means
   effectively infinity.  */

Lisp_Object Vprint_level;

/* Label to use when making echo-area messages. */

Lisp_Object Vprint_message_label;

/* Nonzero means print newlines in strings as \n.  */

int print_escape_newlines;
int print_readably;

/* Non-nil means print #: before uninterned symbols.
   Neither t nor nil means so that and don't clear Vprint_gensym_alist
   on entry to and exit from print functions.  */
Lisp_Object Vprint_gensym;
Lisp_Object Vprint_gensym_alist;

Lisp_Object Qdisplay_error;
Lisp_Object Qprint_message_label;

/* Force immediate output of all printed data.  Used for debugging. */
int print_unbuffered;

FILE *termscript;	/* Stdio stream being used for copy of all output.  */



int stdout_needs_newline;

static void
std_handle_out_external (FILE *stream, Lisp_Object lstream,
			 const Extbyte *extptr, Extcount extlen,
			 /* is this really stdout/stderr?
			    (controls termscript writing) */
			 int output_is_std_handle,
			 int must_flush)
{
  if (stream)
    {
#ifdef WIN32_NATIVE
      HANDLE errhand = GetStdHandle (STD_INPUT_HANDLE);
      int no_useful_stderr = errhand == 0 || errhand == INVALID_HANDLE_VALUE;

      if (!no_useful_stderr)
	no_useful_stderr = !PeekNamedPipe (errhand, 0, 0, 0, 0, 0);
      /* we typically have no useful stdout/stderr under windows if we're
	 being invoked graphically. */
      if (no_useful_stderr)
	mswindows_output_console_string (extptr, extlen);
      else
#endif
	{
	  fwrite (extptr, 1, extlen, stream);
#ifdef WIN32_NATIVE
	  /* Q122442 says that pipes are "treated as files, not as
	     devices", and that this is a feature. Before I found that
	     article, I thought it was a bug. Thanks MS, I feel much
	     better now. - kkm */
	  must_flush = 1;
#endif
	  if (must_flush)
	    fflush (stream);
	}
    }
  else
    Lstream_write (XLSTREAM (lstream), extptr, extlen);

  if (output_is_std_handle)
    {
      if (termscript)
	{
	  fwrite (extptr, 1, extlen, termscript);
	  fflush (termscript);
	}
      stdout_needs_newline = (extptr[extlen - 1] != '\n');
    }
}

/* #### The following function should be replaced a call to the
   emacs_doprnt_*() functions.  This is the only way to ensure that
   I18N3 works properly (many implementations of the *printf()
   functions, including the ones included in glibc, do not implement
   the %###$ argument-positioning syntax).

   Note, however, that to do this, we'd have to

   1) pre-allocate all the lstreams and do whatever else was necessary
   to make sure that no allocation occurs, since these functions may be
   called from fatal_error_signal().

   2) (to be really correct) make a new lstream that outputs using
   mswindows_output_console_string().  */

static int
std_handle_out_va (FILE *stream, const char *fmt, va_list args)
{
  Bufbyte kludge[8192];
  Extbyte *extptr;
  Extcount extlen;
  int retval;

  retval = vsprintf ((char *) kludge, fmt, args);
  if (initialized && !inhibit_non_essential_printing_operations)
    TO_EXTERNAL_FORMAT (DATA, (kludge, strlen ((char *) kludge)),
			ALLOCA, (extptr, extlen),
			Qnative);
  else
    {
      extptr = (Extbyte *) kludge;
      extlen = (Extcount) strlen ((char *) kludge);
    }

  std_handle_out_external (stream, Qnil, extptr, extlen, 1, 1);
  return retval;
}

/* Output portably to stderr or its equivalent; call GETTEXT on the
   format string.  Automatically flush when done. */

int
stderr_out (const char *fmt, ...)
{
  int retval;
  va_list args;
  va_start (args, fmt);
  retval =
    std_handle_out_va
    (stderr, initialized && !fatal_error_in_progress ? GETTEXT (fmt) : fmt,
     args);
  va_end (args);
  return retval;
}

/* Output portably to stdout or its equivalent; call GETTEXT on the
   format string.  Automatically flush when done. */

int
stdout_out (const char *fmt, ...)
{
  int retval;
  va_list args;
  va_start (args, fmt);
  retval =
    std_handle_out_va
    (stdout, initialized && !fatal_error_in_progress ? GETTEXT (fmt) : fmt,
     args);
  va_end (args);
  return retval;
}

DOESNT_RETURN
fatal (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  stderr_out ("\nXEmacs: ");
  std_handle_out_va (stderr, GETTEXT (fmt), args);
  stderr_out ("\n");

  va_end (args);
  exit (1);
}

/* Write a string (in internal format) to stdio stream STREAM. */

void
write_string_to_stdio_stream (FILE *stream, struct console *con,
			      const Bufbyte *str,
			      Bytecount offset, Bytecount len,
			      Lisp_Object coding_system,
			      int must_flush)
{
  Extcount extlen;
  const Extbyte *extptr;

  /* #### yuck! sometimes this function is called with string data,
     and the following call may gc. */
  {
    Bufbyte *puta = (Bufbyte *) alloca (len);
    memcpy (puta, str + offset, len);

    if (initialized && !inhibit_non_essential_printing_operations)
    TO_EXTERNAL_FORMAT (DATA, (puta, len),
			ALLOCA, (extptr, extlen),
			coding_system);
    else
      {
	extptr = (Extbyte *) puta;
	extlen = (Bytecount) len;
      }
  }

  if (stream)
    std_handle_out_external (stream, Qnil, extptr, extlen,
			     stream == stdout || stream == stderr, must_flush);
  else
    {
      assert (CONSOLE_TTY_P (con));
      std_handle_out_external (0, CONSOLE_TTY_DATA (con)->outstream,
			       extptr, extlen,
			       CONSOLE_TTY_DATA (con)->is_stdio, must_flush);
    }
}

/* Write a string to the output location specified in FUNCTION.
   Arguments NONRELOC, RELOC, OFFSET, and LEN are as in
   buffer_insert_string_1() in insdel.c. */

static void
output_string (Lisp_Object function, const Bufbyte *nonreloc,
	       Lisp_Object reloc, Bytecount offset, Bytecount len)
{
  /* This function can GC */
  Charcount cclen;
  /* We change the value of nonreloc (fetching it from reloc as
     necessary), but we don't want to pass this changed value on to
     other functions that take both a nonreloc and a reloc, or things
     may get confused and an assertion failure in
     fixup_internal_substring() may get triggered. */
  const Bufbyte *newnonreloc = nonreloc;
  struct gcpro gcpro1, gcpro2;

  /* Emacs won't print while GCing, but an external debugger might */
  if (gc_in_progress) return;

  /* Perhaps not necessary but probably safer. */
  GCPRO2 (function, reloc);

  fixup_internal_substring (newnonreloc, reloc, offset, &len);

  if (STRINGP (reloc))
    newnonreloc = XSTRING_DATA (reloc);

  cclen = bytecount_to_charcount (newnonreloc + offset, len);

  if (LSTREAMP (function))
    {
      if (STRINGP (reloc))
	{
	  /* Protect against Lstream_write() causing a GC and
	     relocating the string.  For small strings, we do it by
	     alloc'ing the string and using a copy; for large strings,
	     we inhibit GC.  */
	  if (len < 65536)
	    {
	      Bufbyte *copied = alloca_array (Bufbyte, len);
	      memcpy (copied, newnonreloc + offset, len);
	      Lstream_write (XLSTREAM (function), copied, len);
	    }
	  else
	    {
	      int speccount = specpdl_depth ();
	      record_unwind_protect (restore_gc_inhibit,
				     make_int (gc_currently_forbidden));
	      gc_currently_forbidden = 1;
	      Lstream_write (XLSTREAM (function), newnonreloc + offset, len);
	      unbind_to (speccount, Qnil);
	    }
	}
      else
	Lstream_write (XLSTREAM (function), newnonreloc + offset, len);

      if (print_unbuffered)
	Lstream_flush (XLSTREAM (function));
    }
  else if (BUFFERP (function))
    {
      CHECK_LIVE_BUFFER (function);
      buffer_insert_string (XBUFFER (function), nonreloc, reloc, offset, len);
    }
  else if (MARKERP (function))
    {
      /* marker_position() will err if marker doesn't point anywhere.  */
      Bufpos spoint = marker_position (function);

      buffer_insert_string_1 (XMARKER (function)->buffer,
			      spoint, nonreloc, reloc, offset, len,
			      0);
      Fset_marker (function, make_int (spoint + cclen),
		   Fmarker_buffer (function));
    }
  else if (FRAMEP (function))
    {
      /* This gets used by functions not invoking print_prepare(),
         such as Fwrite_char, Fterpri, etc..  */
      struct frame *f = XFRAME (function);
      CHECK_LIVE_FRAME (function);

      if (!EQ (Vprint_message_label, echo_area_status (f)))
	clear_echo_area_from_print (f, Qnil, 1);
      echo_area_append (f, nonreloc, reloc, offset, len, Vprint_message_label);
    }
  else if (EQ (function, Qt) || EQ (function, Qnil))
    {
      write_string_to_stdio_stream (stdout, 0, newnonreloc, offset, len,
				    Qterminal, print_unbuffered);
    }
  else
    {
      Charcount ccoff = bytecount_to_charcount (newnonreloc, offset);
      Charcount iii;

      for (iii = ccoff; iii < cclen + ccoff; iii++)
	{
	  call1 (function,
		 make_char (charptr_emchar_n (newnonreloc, iii)));
	  if (STRINGP (reloc))
	    newnonreloc = XSTRING_DATA (reloc);
	}
    }

  UNGCPRO;
}

#define RESET_PRINT_GENSYM do {			\
  if (!CONSP (Vprint_gensym))			\
    Vprint_gensym_alist = Qnil;			\
} while (0)

static Lisp_Object
canonicalize_printcharfun (Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;

  if (EQ (printcharfun, Qt) || NILP (printcharfun))
    printcharfun = Fselected_frame (Qnil); /* print to minibuffer */

  return printcharfun;
}

static Lisp_Object
print_prepare (Lisp_Object printcharfun, Lisp_Object *frame_kludge)
{
  /* Emacs won't print while GCing, but an external debugger might */
  if (gc_in_progress)
    return Qnil;

  RESET_PRINT_GENSYM;

  printcharfun = canonicalize_printcharfun (printcharfun);

  /* Here we could safely return the canonicalized PRINTCHARFUN.
     However, if PRINTCHARFUN is a frame, printing of complex
     structures becomes very expensive, because `append-message'
     (called by echo_area_append) gets called as many times as
     output_string() is called (and that's a *lot*).  append-message
     tries to keep top of the message-stack in sync with the contents
     of " *Echo Area" buffer, consing a new string for each component
     of the printed structure.  For instance, if you print (a a),
     append-message will cons up the following strings:

         "("
	 "(a"
	 "(a "
	 "(a a"
	 "(a a)"

     and will use only the last one.  With larger objects, this turns
     into an O(n^2) consing frenzy that locks up XEmacs in incessant
     garbage collection.

     We prevent this by creating a resizing_buffer stream and letting
     the printer write into it.  print_finish() will notice this
     stream, and invoke echo_area_append() with the stream's buffer,
     only once.  */
  if (FRAMEP (printcharfun))
    {
      CHECK_LIVE_FRAME (printcharfun);
      *frame_kludge = printcharfun;
      printcharfun = make_resizing_buffer_output_stream ();
    }

  return printcharfun;
}

static void
print_finish (Lisp_Object stream, Lisp_Object frame_kludge)
{
  /* Emacs won't print while GCing, but an external debugger might */
  if (gc_in_progress)
    return;

  RESET_PRINT_GENSYM;

  /* See the comment in print_prepare().  */
  if (FRAMEP (frame_kludge))
    {
      struct frame *f = XFRAME (frame_kludge);
      Lstream *str = XLSTREAM (stream);
      CHECK_LIVE_FRAME (frame_kludge);

      Lstream_flush (str);
      if (!EQ (Vprint_message_label, echo_area_status (f)))
	clear_echo_area_from_print (f, Qnil, 1);
      echo_area_append (f, resizing_buffer_stream_ptr (str),
			Qnil, 0, Lstream_byte_count (str),
			Vprint_message_label);
      Lstream_delete (str);
    }
}

/* Used for printing a single-byte character (*not* any Emchar).  */
#define write_char_internal(string_of_length_1, stream)			\
  output_string (stream, (const Bufbyte *) (string_of_length_1),	\
		 Qnil, 0, 1)

/* NOTE: Do not call this with the data of a Lisp_String, as
   printcharfun might cause a GC, which might cause the string's data
   to be relocated.  To princ a Lisp string, use:

       print_internal (string, printcharfun, 0);

   Also note that STREAM should be the result of
   canonicalize_printcharfun() (i.e. Qnil means stdout, not
   Vstandard_output, etc.)  */
void
write_string_1 (const Bufbyte *str, Bytecount size, Lisp_Object stream)
{
  /* This function can GC */
#ifdef ERROR_CHECK_BUFPOS
  assert (size >= 0);
#endif
  output_string (stream, str, Qnil, 0, size);
}

void
write_c_string (const char *str, Lisp_Object stream)
{
  /* This function can GC */
  write_string_1 ((const Bufbyte *) str, strlen (str), stream);
}

static void
write_fmt_string (Lisp_Object stream, const char *fmt, ...)
{
  va_list va;
  char bigbuf[666];

  va_start (va, fmt);
  vsprintf (bigbuf, fmt, va);
  va_end (va);
  write_c_string (bigbuf, stream);
}


DEFUN ("write-char", Fwrite_char, 1, 2, 0, /*
Output character CHARACTER to stream STREAM.
STREAM defaults to the value of `standard-output' (which see).
*/
       (character, stream))
{
  /* This function can GC */
  Bufbyte str[MAX_EMCHAR_LEN];
  Bytecount len;

  CHECK_CHAR_COERCE_INT (character);
  len = set_charptr_emchar (str, XCHAR (character));
  output_string (canonicalize_printcharfun (stream), str, Qnil, 0, len);
  return character;
}

void
temp_output_buffer_setup (Lisp_Object bufname)
{
  /* This function can GC */
  struct buffer *old = current_buffer;
  Lisp_Object buf;

#ifdef I18N3
  /* #### This function should accept a Lisp_Object instead of a char *,
     so that proper translation on the buffer name can occur. */
#endif

  Fset_buffer (Fget_buffer_create (bufname));

  current_buffer->read_only = Qnil;
  Ferase_buffer (Qnil);

  XSETBUFFER (buf, current_buffer);
  specbind (Qstandard_output, buf);

  set_buffer_internal (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (Lisp_Object bufname,
                                     Lisp_Object (*function) (Lisp_Object arg),
                                     Lisp_Object arg,
                                     Lisp_Object same_frame)
{
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object buf = Qnil;

  GCPRO3 (buf, arg, same_frame);

  temp_output_buffer_setup (bufname);
  buf = Vstandard_output;

  arg = (*function) (arg);

  temp_output_buffer_show (buf, same_frame);
  UNGCPRO;

  return unbind_to (speccount, arg);
}

DEFUN ("with-output-to-temp-buffer", Fwith_output_to_temp_buffer, 1, UNEVALLED, 0, /*
Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.
The buffer is cleared out initially, and marked as unmodified when done.
All output done by BODY is inserted in that buffer by default.
The buffer is displayed in another window, but not selected.
The value of the last form in BODY is returned.
If BODY does not finish normally, the buffer BUFNAME is not displayed.

If variable `temp-buffer-show-function' is non-nil, call it at the end
to get the buffer displayed.  It gets one argument, the buffer to display.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object name = Qnil;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2;
  Lisp_Object val = Qnil;

#ifdef I18N3
  /* #### should set the buffer to be translating.  See print_internal(). */
#endif

  GCPRO2 (name, val);
  name = Feval (XCAR (args));

  CHECK_STRING (name);

  temp_output_buffer_setup (name);
  UNGCPRO;

  val = Fprogn (XCDR (args));

  temp_output_buffer_show (Vstandard_output, Qnil);

  return unbind_to (speccount, val);
}

DEFUN ("terpri", Fterpri, 0, 1, 0, /*
Output a newline to STREAM.
If STREAM is omitted or nil, the value of `standard-output' is used.
*/
       (stream))
{
  /* This function can GC */
  write_char_internal ("\n", canonicalize_printcharfun (stream));
  return Qt;
}

DEFUN ("prin1", Fprin1, 1, 2, 0, /*
Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (object, stream))
{
  /* This function can GC */
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (object, stream);

  print_depth = 0;
  stream = print_prepare (stream, &frame);
  print_internal (object, stream, 1);
  print_finish (stream, frame);

  UNGCPRO;
  return object;
}

DEFUN ("prin1-to-string", Fprin1_to_string, 1, 2, 0, /*
Return a string containing the printed representation of OBJECT,
any Lisp object.  Quoting characters are used when needed to make output
that `read' can handle, whenever this is possible, unless the optional
second argument NOESCAPE is non-nil.
*/
       (object, noescape))
{
  /* This function can GC */
  Lisp_Object result = Qnil;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Lstream *str = XLSTREAM (stream);
  /* gcpro OBJECT in case a caller forgot to do so */
  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (object, stream, result);

  print_depth = 0;
  RESET_PRINT_GENSYM;
  print_internal (object, stream, NILP (noescape));
  RESET_PRINT_GENSYM;
  Lstream_flush (str);
  UNGCPRO;
  result = make_string (resizing_buffer_stream_ptr (str),
			Lstream_byte_count (str));
  Lstream_delete (str);
  return result;
}

DEFUN ("princ", Fprinc, 1, 2, 0, /*
Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (object, stream))
{
  /* This function can GC */
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (object, stream);
  stream = print_prepare (stream, &frame);
  print_depth = 0;
  print_internal (object, stream, 0);
  print_finish (stream, frame);
  UNGCPRO;
  return object;
}

DEFUN ("print", Fprint, 1, 2, 0, /*
Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (object, stream))
{
  /* This function can GC */
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (object, stream);
  stream = print_prepare (stream, &frame);
  print_depth = 0;
  write_char_internal ("\n", stream);
  print_internal (object, stream, 1);
  write_char_internal ("\n", stream);
  print_finish (stream, frame);
  UNGCPRO;
  return object;
}

/* Print an error message for the error DATA to STREAM.  This is a
   complete implementation of `display-error', which used to be in
   Lisp (see prim/cmdloop.el).  It was ported to C so it can be used
   efficiently by Ferror_message_string.  Fdisplay_error and
   Ferror_message_string are trivial wrappers around this function.

   STREAM should be the result of canonicalize_printcharfun().  */
static void
print_error_message (Lisp_Object error_object, Lisp_Object stream)
{
  /* This function can GC */
  Lisp_Object type = Fcar_safe (error_object);
  Lisp_Object method = Qnil;
  Lisp_Object tail;

  /* No need to GCPRO anything under the assumption that ERROR_OBJECT
     is GCPRO'd.  */

  if (! (CONSP (error_object) && SYMBOLP (type)
	 && CONSP (Fget (type, Qerror_conditions, Qnil))))
    goto error_throw;

  tail = XCDR (error_object);
  while (!NILP (tail))
    {
      if (CONSP (tail))
	tail = XCDR (tail);
      else
	goto error_throw;
    }
  tail = Fget (type, Qerror_conditions, Qnil);
  while (!NILP (tail))
    {
      if (!(CONSP (tail) && SYMBOLP (XCAR (tail))))
	goto error_throw;
      else if (!NILP (Fget (XCAR (tail), Qdisplay_error, Qnil)))
	{
	  method = Fget (XCAR (tail), Qdisplay_error, Qnil);
	  goto error_throw;
	}
      else
	tail = XCDR (tail);
    }
  /* Default method */
  {
    int first = 1;
    int speccount = specpdl_depth ();
    Lisp_Object frame = Qnil;
    struct gcpro gcpro1;
    GCPRO1 (stream);

    specbind (Qprint_message_label, Qerror);
    stream = print_prepare (stream, &frame);

    tail = Fcdr (error_object);
    if (EQ (type, Qerror))
      {
	print_internal (Fcar (tail), stream, 0);
	tail = Fcdr (tail);
      }
    else
      {
	Lisp_Object errmsg = Fget (type, Qerror_message, Qnil);
	if (NILP (errmsg))
	  print_internal (type, stream, 0);
	else
	  print_internal (LISP_GETTEXT (errmsg), stream, 0);
      }
    while (!NILP (tail))
      {
	write_c_string (first ? ": " : ", ", stream);
	print_internal (Fcar (tail), stream, 1);
	tail = Fcdr (tail);
	first = 0;
      }
    print_finish (stream, frame);
    UNGCPRO;
    unbind_to (speccount, Qnil);
    return;
    /* not reached */
  }

 error_throw:
  if (NILP (method))
    {
      write_c_string (GETTEXT ("Peculiar error "), stream);
      print_internal (error_object, stream, 1);
      return;
    }
  else
    {
      call2 (method, error_object, stream);
    }
}

DEFUN ("error-message-string", Ferror_message_string, 1, 1, 0, /*
Convert ERROR-OBJECT to an error message, and return it.

The format of ERROR-OBJECT should be (ERROR-SYMBOL . DATA).  The
message is equivalent to the one that would be issued by
`display-error' with the same argument.
*/
       (error_object))
{
  /* This function can GC */
  Lisp_Object result = Qnil;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;
  GCPRO1 (stream);

  print_error_message (error_object, stream);
  Lstream_flush (XLSTREAM (stream));
  result = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
			Lstream_byte_count (XLSTREAM (stream)));
  Lstream_delete (XLSTREAM (stream));

  UNGCPRO;
  return result;
}

DEFUN ("display-error", Fdisplay_error, 2, 2, 0, /*
Display ERROR-OBJECT on STREAM in a user-friendly way.
*/
       (error_object, stream))
{
  /* This function can GC */
  print_error_message (error_object, canonicalize_printcharfun (stream));
  return Qnil;
}


#ifdef LISP_FLOAT_TYPE

Lisp_Object Vfloat_output_format;

/*
 * This buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtedly
 * 20d float_output_format, with the negative of the C-constant "HUGE"
 * from <math.h>.
 *
 * On the vax the worst case is -1e38 in 20d format which takes 61 bytes.
 *
 * I assume that IEEE-754 format numbers can take 329 bytes for the worst
 * case of -1e307 in 20d float_output_format. What is one to do (short of
 * re-writing _doprnt to be more sane)?
 * 			-wsr
 */
void
float_to_string (char *buf, double data)
{
  Bufbyte *cp, c;
  int width;

  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    sprintf (buf, "%.16g", data);
  else			/* oink oink */
    {
      /* Check that the spec we have is fully valid.
	 This means not only valid for printf,
	 but meant for floats, and reasonable.  */
      cp = XSTRING_DATA (Vfloat_output_format);

      if (cp[0] != '%')
	goto lose;
      if (cp[1] != '.')
	goto lose;

      cp += 2;
      for (width = 0; (c = *cp, isdigit (c)); cp++)
	{
	  width *= 10;
	  width += c - '0';
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g' && *cp != 'E' && *cp != 'G')
	goto lose;

      if (width < (int) (*cp != 'e' && *cp != 'E') || width > DBL_DIG)
	goto lose;

      if (cp[1] != 0)
	goto lose;

      sprintf (buf, (char *) XSTRING_DATA (Vfloat_output_format),
	       data);
    }

  /* added by jwz: don't allow "1.0" to print as "1"; that destroys
     the read-equivalence of lisp objects.  (* x 1) and (* x 1.0) do
     not do the same thing, so it's important that the printed
     representation of that form not be corrupted by the printer.
   */
  {
    Bufbyte *s = (Bufbyte *) buf; /* don't use signed chars here!
				     isdigit() can't hack them! */
    if (*s == '-') s++;
    for (; *s; s++)
      /* if there's a non-digit, then there is a decimal point, or
	 it's in exponential notation, both of which are ok. */
      if (!isdigit (*s))
	goto DONE_LABEL;
    /* otherwise, we need to hack it. */
    *s++ = '.';
    *s++ = '0';
    *s = 0;
  }
 DONE_LABEL:

  /* Some machines print "0.4" as ".4".  I don't like that. */
  if (buf [0] == '.' || (buf [0] == '-' && buf [1] == '.'))
    {
      int i;
      for (i = strlen (buf) + 1; i >= 0; i--)
	buf [i+1] = buf [i];
      buf [(buf [0] == '-' ? 1 : 0)] = '0';
    }
}
#endif /* LISP_FLOAT_TYPE */

/* Print NUMBER to BUFFER.
   This is equivalent to sprintf (buffer, "%ld", number), only much faster.

   BUFFER should accept 24 bytes.  This should suffice for the longest
   numbers on 64-bit machines, including the `-' sign and the trailing
   '\0'.  Returns a pointer to the trailing '\0'. */
char *
long_to_string (char *buffer, long number)
{
#if (SIZEOF_LONG != 4) && (SIZEOF_LONG != 8)
  /* Huh? */
  sprintf (buffer, "%ld", number);
  return buffer + strlen (buffer);
#else /* (SIZEOF_LONG == 4) || (SIZEOF_LONG == 8) */
  char *p = buffer;
  int force = 0;

  if (number < 0)
    {
      *p++ = '-';
      number = -number;
    }

#define FROB(figure) do {						\
    if (force || number >= figure)					\
      *p++ = number / figure + '0', number %= figure, force = 1;	\
    } while (0)
#if SIZEOF_LONG == 8
  FROB (1000000000000000000L);
  FROB (100000000000000000L);
  FROB (10000000000000000L);
  FROB (1000000000000000L);
  FROB (100000000000000L);
  FROB (10000000000000L);
  FROB (1000000000000L);
  FROB (100000000000L);
  FROB (10000000000L);
#endif /* SIZEOF_LONG == 8 */
  FROB (1000000000);
  FROB (100000000);
  FROB (10000000);
  FROB (1000000);
  FROB (100000);
  FROB (10000);
  FROB (1000);
  FROB (100);
  FROB (10);
#undef FROB
  *p++ = number + '0';
  *p = '\0';
  return p;
#endif /* (SIZEOF_LONG == 4) || (SIZEOF_LONG == 8) */
}

static void
print_vector_internal (const char *start, const char *end,
                       Lisp_Object obj,
                       Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  int i;
  int len = XVECTOR_LENGTH (obj);
  int last = len;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (INTP (Vprint_length))
    {
      int max = XINT (Vprint_length);
      if (max < len) last = max;
    }

  write_c_string (start, printcharfun);
  for (i = 0; i < last; i++)
    {
      Lisp_Object elt = XVECTOR_DATA (obj)[i];
      if (i != 0) write_char_internal (" ", printcharfun);
      print_internal (elt, printcharfun, escapeflag);
    }
  UNGCPRO;
  if (last != len)
    write_c_string (" ...", printcharfun);
  write_c_string (end, printcharfun);
}

void
print_cons (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;

  /* If print_readably is on, print (quote -foo-) as '-foo-
     (Yeah, this should really be what print-pretty does, but we
     don't have the rest of a pretty printer, and this actually
     has non-negligible impact on size/speed of .elc files.)
  */
  if (print_readably &&
      EQ (XCAR (obj), Qquote) &&
      CONSP (XCDR (obj)) &&
      NILP (XCDR (XCDR (obj))))
    {
      obj = XCAR (XCDR (obj));
      GCPRO2 (obj, printcharfun);
      write_char_internal ("\'", printcharfun);
      UNGCPRO;
      print_internal (obj, printcharfun, escapeflag);
      return;
    }

  GCPRO2 (obj, printcharfun);
  write_char_internal ("(", printcharfun);

  {
    int len;
    int max = INTP (Vprint_length) ? XINT (Vprint_length) : INT_MAX;
    Lisp_Object tortoise;
    /* Use tortoise/hare to make sure circular lists don't infloop */

    for (tortoise = obj, len = 0;
	 CONSP (obj);
	 obj = XCDR (obj), len++)
      {
	if (len > 0)
	  write_char_internal (" ", printcharfun);
	if (EQ (obj, tortoise) && len > 0)
	  {
	    if (print_readably)
	      error ("printing unreadable circular list");
	    else
	      write_c_string ("... <circular list>", printcharfun);
	    break;
	  }
	if (len & 1)
	  tortoise = XCDR (tortoise);
	if (len > max)
	  {
	    write_c_string ("...", printcharfun);
	    break;
	  }
	print_internal (XCAR (obj), printcharfun, escapeflag);
      }
  }
  if (!LISTP (obj))
    {
      write_c_string (" . ", printcharfun);
      print_internal (obj, printcharfun, escapeflag);
    }
  UNGCPRO;

  write_char_internal (")", printcharfun);
  return;
}

void
print_vector (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  print_vector_internal ("[", "]", obj, printcharfun, escapeflag);
}

void
print_string (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  Lisp_String *s = XSTRING (obj);
  /* We distinguish between Bytecounts and Charcounts, to make
     Vprint_string_length work correctly under Mule.  */
  Charcount size = string_char_length (s);
  Charcount max = size;
  Bytecount bcmax = string_length (s);
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (INTP (Vprint_string_length) &&
      XINT (Vprint_string_length) < max)
    {
      max = XINT (Vprint_string_length);
      bcmax = charcount_to_bytecount (string_data (s), max);
    }
  if (max < 0)
    {
      max = 0;
      bcmax = 0;
    }

  if (!escapeflag)
    {
      /* This deals with GC-relocation and Mule. */
      output_string (printcharfun, 0, obj, 0, bcmax);
      if (max < size)
	write_c_string (" ...", printcharfun);
    }
  else
    {
      Bytecount i, last = 0;

      write_char_internal ("\"", printcharfun);
      for (i = 0; i < bcmax; i++)
	{
	  Bufbyte ch = string_byte (s, i);
	  if (ch == '\"' || ch == '\\'
	      || (ch == '\n' && print_escape_newlines))
	    {
	      if (i > last)
		{
		  output_string (printcharfun, 0, obj, last,
				 i - last);
		}
	      if (ch == '\n')
		{
		  write_c_string ("\\n", printcharfun);
		}
	      else
		{
		  write_char_internal ("\\", printcharfun);
		  /* This is correct for Mule because the
		     character is either \ or " */
		  write_char_internal (string_data (s) + i, printcharfun);
		}
	      last = i + 1;
	    }
	}
      if (bcmax > last)
	{
	  output_string (printcharfun, 0, obj, last,
			 bcmax - last);
	}
      if (max < size)
	write_c_string (" ...", printcharfun);
      write_char_internal ("\"", printcharfun);
    }
  UNGCPRO;
}

static void
default_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			int escapeflag)
{
  struct lcrecord_header *header =
    (struct lcrecord_header *) XPNTR (obj);
  char buf[200];

  if (print_readably)
    error ("printing unreadable object #<%s 0x%x>",
	   LHEADER_IMPLEMENTATION (&header->lheader)->name,
	   header->uid);

  sprintf (buf, "#<%s 0x%x>",
	   LHEADER_IMPLEMENTATION (&header->lheader)->name,
	   header->uid);
  write_c_string (buf, printcharfun);
}

void
internal_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			 int escapeflag)
{
  char buf[200];
  sprintf (buf, "#<INTERNAL OBJECT (XEmacs bug?) (%s) 0x%lx>",
	   XRECORD_LHEADER_IMPLEMENTATION (obj)->name,
	   (unsigned long) XPNTR (obj));
  write_c_string (buf, printcharfun);
}

enum printing_badness
{
  BADNESS_INTEGER_OBJECT,
  BADNESS_POINTER_OBJECT,
  BADNESS_NO_TYPE
};

static void
printing_major_badness (Lisp_Object printcharfun,
			Char_ASCII *badness_string, int type, void *val,
			enum printing_badness badness)
{
  char buf[666];

  switch (badness)
    {
    case BADNESS_INTEGER_OBJECT:
      sprintf (buf, "%s %d object %ld", badness_string, type,
		  (EMACS_INT) val);
      break;

    case BADNESS_POINTER_OBJECT:
      sprintf (buf, "%s %d object %p", badness_string, type, val);
      break;

    case BADNESS_NO_TYPE:
      sprintf (buf, "%s object %p", badness_string, val);
      break;
    }

  /* Don't abort or signal if called from debug_print() or already
     crashing */
  if (!inhibit_non_essential_printing_operations)
    {
#ifdef ERROR_CHECK_TYPES
      abort ();
#else  /* not ERROR_CHECK_TYPES */
      if (print_readably)
	type_error (Qinternal_error, "printing %s", buf);
#endif /* not ERROR_CHECK_TYPES */
    }
  write_fmt_string (printcharfun,
		    "#<EMACS BUG: %s Save your buffers immediately and "
		    "please report this bug>", buf);
}

void
print_internal (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */

  QUIT;

  /* Emacs won't print while GCing, but an external debugger might */
  if (gc_in_progress) return;

#ifdef I18N3
  /* #### Both input and output streams should have a flag associated
     with them indicating whether output to that stream, or strings
     read from the stream, get translated using Fgettext().  Such a
     stream is called a "translating stream".  For the minibuffer and
     external-debugging-output this is always true on output, and
     with-output-to-temp-buffer sets the flag to true for the buffer
     it creates.  This flag should also be user-settable.  Perhaps it
     should be split up into two flags, one for input and one for
     output. */
#endif

  /* Detect circularities and truncate them.
     No need to offer any alternative--this is better than an error.  */
  if (CONSP (obj) || VECTORP (obj) || COMPILED_FUNCTIONP (obj))
    {
      int i;
      for (i = 0; i < print_depth; i++)
	if (EQ (obj, being_printed[i]))
	  {
	    char buf[32];
	    *buf = '#';
	    long_to_string (buf + 1, i);
	    write_c_string (buf, printcharfun);
	    return;
	  }
    }

  being_printed[print_depth] = obj;
  print_depth++;

  if (print_depth > PRINT_CIRCLE)
    error ("Apparently circular structure being printed");

  switch (XTYPE (obj))
    {
    case Lisp_Type_Int_Even:
    case Lisp_Type_Int_Odd:
      {
	/* ASCII Decimal representation uses 2.4 times as many bits as
	   machine binary.  */
	char buf[3 * sizeof (EMACS_INT) + 5];
	long_to_string (buf, XINT (obj));
	write_c_string (buf, printcharfun);
	break;
      }

    case Lisp_Type_Char:
      {
	/* God intended that this be #\..., you know. */
	char buf[16];
	Emchar ch = XCHAR (obj);
	char *p = buf;
	*p++ = '?';
	if (ch < 32)
	  {
	    *p++ = '\\';
	    switch (ch)
	      {
	      case '\t': *p++ = 't'; break;
	      case '\n': *p++ = 'n'; break;
	      case '\r': *p++ = 'r'; break;
	      default:
		*p++ = '^';
		*p++ = ch + 64;
		if ((ch + 64) == '\\')
		  *p++ = '\\';
		break;
	      }
	  }
	else if (ch < 127)
	  {
	    /* syntactically special characters should be escaped. */
	    switch (ch)
	      {
	      case ' ':
	      case '"':
	      case '#':
	      case '\'':
	      case '(':
	      case ')':
	      case ',':
	      case '.':
	      case ';':
	      case '?':
	      case '[':
	      case '\\':
	      case ']':
	      case '`':
		*p++ = '\\';
	      }
	    *p++ = ch;
	  }
	else if (ch == 127)
	  {
	    *p++ = '\\', *p++ = '^', *p++ = '?';
	  }
	else if (ch < 160)
	  {
	    *p++ = '\\', *p++ = '^';
	    p += set_charptr_emchar ((Bufbyte *) p, ch + 64);
	  }
	else
	  {
	    p += set_charptr_emchar ((Bufbyte *) p, ch);
	  }

	output_string (printcharfun, (Bufbyte *) buf, Qnil, 0, p - buf);

	break;
      }

    case Lisp_Type_Record:
      {
	struct lrecord_header *lheader = XRECORD_LHEADER (obj);

	/* Try to check for various sorts of bogus pointers if we're in a
	   situation where it may be likely -- i.e. called from
	   debug_print() or we're already crashing.  In such cases,
	   (further) crashing is counterproductive. */

	if (inhibit_non_essential_printing_operations &&
	    !debug_can_access_memory (lheader, sizeof (*lheader)))
	    {
	      write_fmt_string (printcharfun,
				"#<EMACS BUG: BAD MEMORY ACCESS %p>",
				lheader);
	      break;
	    }

	if (CONSP (obj) || VECTORP (obj))
	  {
	    /* If deeper than spec'd depth, print placeholder.  */
	    if (INTP (Vprint_level)
		&& print_depth > XINT (Vprint_level))
	      {
		write_c_string ("...", printcharfun);
		break;
	      }
	  }

	if (lheader->type == lrecord_type_free)
	  {
	    printing_major_badness (printcharfun, "freed lrecord", 0,
				    lheader, BADNESS_NO_TYPE);
	    break;
	  }
	else if (lheader->type == lrecord_type_undefined)
	  {
	    printing_major_badness (printcharfun, "lrecord_type_undefined", 0,
				    lheader, BADNESS_NO_TYPE);
	    break;
	  }
	else if (lheader->type >= lrecord_type_count)
	  {
	    printing_major_badness (printcharfun, "illegal lrecord type",
				    (int) (lheader->type),
				    lheader, BADNESS_POINTER_OBJECT);
	    break;
	  }

	/* Further checks for bad memory in critical situations.  We don't
	   normally do these because they may be expensive or weird
	   (e.g. under Unix we typically have to set a SIGSEGV handler and
	   try to trigger a seg fault). */

	if (inhibit_non_essential_printing_operations)
	  {
	    const struct lrecord_implementation *imp =
	      LHEADER_IMPLEMENTATION (lheader);

  	  if (!debug_can_access_memory
		(lheader, imp->size_in_bytes_method ?
		 imp->size_in_bytes_method (lheader) :
		 imp->static_size))
	      {
		write_fmt_string (printcharfun,
				  "#<EMACS BUG: type %s BAD MEMORY ACCESS %p>",
				  LHEADER_IMPLEMENTATION (lheader)->name,
				  lheader);
		break;
	      }

	    if (STRINGP (obj))
	      {
		Lisp_String *l = (Lisp_String *) lheader;
		if (!debug_can_access_memory (l->data, l->size))
		  {
		    write_fmt_string
		      (printcharfun,
		       "#<EMACS BUG: %p (CAN'T ACCESS STRING DATA %p)>",
		       lheader, l->data);
		    break;
		  }
	      }
	  }

	if (LHEADER_IMPLEMENTATION (lheader)->printer)
	  ((LHEADER_IMPLEMENTATION (lheader)->printer)
	   (obj, printcharfun, escapeflag));
	else
	  default_object_printer (obj, printcharfun, escapeflag);
	break;
      }

    default:
      {
	/* We're in trouble if this happens! */
	printing_major_badness (printcharfun, "illegal data type", XTYPE (obj),
				LISP_TO_VOID (obj), BADNESS_INTEGER_OBJECT);
	break;
      }
    }

  print_depth--;
}


#ifdef LISP_FLOAT_TYPE
void
print_float (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char pigbuf[350];	/* see comments in float_to_string */

  float_to_string (pigbuf, XFLOAT_DATA (obj));
  write_c_string (pigbuf, printcharfun);
}
#endif /* LISP_FLOAT_TYPE */

void
print_symbol (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  /* #### Bug!! (intern "") isn't printed in some distinguished way */
  /* ####  (the reader also loses on it) */
  Lisp_String *name = symbol_name (XSYMBOL (obj));
  Bytecount size = string_length (name);
  struct gcpro gcpro1, gcpro2;

  if (!escapeflag)
    {
      /* This deals with GC-relocation */
      Lisp_Object nameobj;
      XSETSTRING (nameobj, name);
      output_string (printcharfun, 0, nameobj, 0, size);
      return;
    }
  GCPRO2 (obj, printcharfun);

  /* If we print an uninterned symbol as part of a complex object and
     the flag print-gensym is non-nil, prefix it with #n= to read the
     object back with the #n# reader syntax later if needed.  */
  if (!NILP (Vprint_gensym)
      /* #### Test whether this produces a noticeable slow-down for
         printing when print-gensym is non-nil.  */
      && !EQ (obj, oblookup (Vobarray,
			     string_data (symbol_name (XSYMBOL (obj))),
			     string_length (symbol_name (XSYMBOL (obj))))))
    {
      if (print_depth > 1)
	{
	  Lisp_Object tem = Fassq (obj, Vprint_gensym_alist);
	  if (CONSP (tem))
	    {
	      write_char_internal ("#", printcharfun);
	      print_internal (XCDR (tem), printcharfun, escapeflag);
	      write_char_internal ("#", printcharfun);
	      UNGCPRO;
	      return;
	    }
	  else
	    {
	      if (CONSP (Vprint_gensym_alist))
		{
		  /* Vprint_gensym_alist is exposed to Lisp, so we
                     have to be careful.  */
		  CHECK_CONS (XCAR (Vprint_gensym_alist));
		  CHECK_INT (XCDR (XCAR (Vprint_gensym_alist)));
		  XSETINT (tem, XINT (XCDR (XCAR (Vprint_gensym_alist))) + 1);
		}
	      else
		XSETINT (tem, 1);
	      Vprint_gensym_alist = Fcons (Fcons (obj, tem), Vprint_gensym_alist);

	      write_char_internal ("#", printcharfun);
	      print_internal (tem, printcharfun, escapeflag);
	      write_char_internal ("=", printcharfun);
	    }
	}
      write_c_string ("#:", printcharfun);
    }

  /* Does it look like an integer or a float? */
  {
    Bufbyte *data = string_data (name);
    Bytecount confusing = 0;

    if (size == 0)
      goto not_yet_confused;    /* Really confusing */
    else if (isdigit (data[0]))
      confusing = 0;
    else if (size == 1)
      goto not_yet_confused;
    else if (data[0] == '-' || data[0] == '+')
      confusing = 1;
    else
      goto not_yet_confused;

    for (; confusing < size; confusing++)
      {
        if (!isdigit (data[confusing]))
          {
            confusing = 0;
            break;
          }
      }
  not_yet_confused:

#ifdef LISP_FLOAT_TYPE
    if (!confusing)
      /* #### Ugh, this is needlessly complex and slow for what we
         need here.  It might be a good idea to copy equivalent code
         from FSF.  --hniksic */
      confusing = isfloat_string ((char *) data);
#endif
    if (confusing)
      write_char_internal ("\\", printcharfun);
  }

  {
    Lisp_Object nameobj;
    Bytecount i;
    Bytecount last = 0;

    XSETSTRING (nameobj, name);
    for (i = 0; i < size; i++)
      {
	switch (string_byte (name, i))
	  {
	  case  0: case  1: case  2: case  3:
	  case  4: case  5: case  6: case  7:
	  case  8: case  9: case 10: case 11:
	  case 12: case 13: case 14: case 15:
	  case 16: case 17: case 18: case 19:
	  case 20: case 21: case 22: case 23:
	  case 24: case 25: case 26: case 27:
	  case 28: case 29: case 30: case 31:
	  case ' ': case '\"': case '\\': case '\'':
	  case ';': case '#' : case '(' : case ')':
	  case ',': case '.' : case '`' :
	  case '[': case ']' : case '?' :
	    if (i > last)
	      output_string (printcharfun, 0, nameobj, last, i - last);
	    write_char_internal ("\\", printcharfun);
	    last = i;
	  }
      }
    output_string (printcharfun, 0, nameobj, last, size - last);
  }
  UNGCPRO;
}


/* Useful on systems or in places where writing to stdout is unavailable or
   not working. */

static int alternate_do_pointer;
static char alternate_do_string[5000];

DEFUN ("alternate-debugging-output", Falternate_debugging_output, 1, 1, 0, /*
Append CHARACTER to the array `alternate_do_string'.
This can be used in place of `external-debugging-output' as a function
to be passed to `print'.  Before calling `print', set `alternate_do_pointer'
to 0.
*/
       (character))
{
  Bufbyte str[MAX_EMCHAR_LEN];
  Bytecount len;
  int extlen;
  const Extbyte *extptr;

  CHECK_CHAR_COERCE_INT (character);
  len = set_charptr_emchar (str, XCHAR (character));
  TO_EXTERNAL_FORMAT (DATA, (str, len),
		      ALLOCA, (extptr, extlen),
		      Qterminal);
  memcpy (alternate_do_string + alternate_do_pointer, extptr, extlen);
  alternate_do_pointer += extlen;
  alternate_do_string[alternate_do_pointer] = 0;
  return character;
}

DEFUN ("external-debugging-output", Fexternal_debugging_output, 1, 3, 0, /*
Write CHAR-OR-STRING to stderr or stdout.
If optional arg STDOUT-P is non-nil, write to stdout; otherwise, write
to stderr.  You can use this function to write directly to the terminal.
This function can be used as the STREAM argument of Fprint() or the like.

Under MS Windows, this writes output to the console window (which is
created, if necessary), unless XEmacs is being run noninteractively
\(i.e. using the `-batch' argument).

If you have opened a termscript file (using `open-termscript'), then
the output also will be logged to this file.
*/
       (char_or_string, stdout_p, device))
{
  FILE *file = 0;
  struct console *con = 0;

  if (NILP (device))
    {
      if (!NILP (stdout_p))
	file = stdout;
      else
	file = stderr;
    }
  else
    {
      CHECK_LIVE_DEVICE (device);
      if (!DEVICE_TTY_P (XDEVICE (device)) &&
	  !DEVICE_STREAM_P (XDEVICE (device)))
	signal_simple_error ("Must be tty or stream device", device);
      con = XCONSOLE (DEVICE_CONSOLE (XDEVICE (device)));
      if (DEVICE_TTY_P (XDEVICE (device)))
	file = 0;
      else if (!NILP (stdout_p))
	file = CONSOLE_STREAM_DATA (con)->out;
      else
	file = CONSOLE_STREAM_DATA (con)->err;
    }

  if (STRINGP (char_or_string))
    write_string_to_stdio_stream (file, con,
				  XSTRING_DATA (char_or_string),
				  0, XSTRING_LENGTH (char_or_string),
				  Qterminal, 1);
  else
    {
      Bufbyte str[MAX_EMCHAR_LEN];
      Bytecount len;

      CHECK_CHAR_COERCE_INT (char_or_string);
      len = set_charptr_emchar (str, XCHAR (char_or_string));
      write_string_to_stdio_stream (file, con, str, 0, len, Qterminal, 1);
    }

  return char_or_string;
}

DEFUN ("open-termscript", Fopen_termscript, 1, 1, "FOpen termscript file: ", /*
Start writing all terminal output to FILENAME as well as the terminal.
FILENAME = nil means just close any termscript file currently open.
*/
       (filename))
{
  /* This function can GC */
  if (termscript != 0)
    {
      fclose (termscript);
      termscript = 0;
    }

  if (! NILP (filename))
    {
      filename = Fexpand_file_name (filename, Qnil);
      termscript = fopen ((char *) XSTRING_DATA (filename), "w");
      if (termscript == NULL)
	report_file_error ("Opening termscript", list1 (filename));
    }
  return Qnil;
}

#if 1
/* Debugging kludge -- unbuffered */
static int debug_print_length   = 50;
static int debug_print_level    = 15;
static int debug_print_readably = -1;

static void
debug_print_no_newline (Lisp_Object debug_print_obj)
{
  /* This function can GC */
  int save_print_readably = print_readably;
  int save_print_depth    = print_depth;
  Lisp_Object save_Vprint_length = Vprint_length;
  Lisp_Object save_Vprint_level  = Vprint_level;
  Lisp_Object save_Vinhibit_quit = Vinhibit_quit;
  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (save_Vprint_level, save_Vprint_length, save_Vinhibit_quit);

  if (gc_in_progress)
    stderr_out ("** gc-in-progress!  Bad idea to print anything! **\n");

  print_depth = 0;
  print_readably = debug_print_readably != -1 ? debug_print_readably : 0;
  print_unbuffered++;
  inhibit_non_essential_printing_operations = 1;
  /* Could use unwind-protect, but why bother? */
  if (debug_print_length > 0)
    Vprint_length = make_int (debug_print_length);
  if (debug_print_level > 0)
    Vprint_level = make_int (debug_print_level);

  print_internal (debug_print_obj, Qexternal_debugging_output, 1);
  alternate_do_pointer = 0;
  print_internal (debug_print_obj, Qalternate_debugging_output, 1);
#ifdef WIN32_NATIVE
  /* Write out to the debugger, as well */
  print_internal (debug_print_obj, Qmswindows_debugging_output, 1);
#endif

  Vinhibit_quit  = save_Vinhibit_quit;
  Vprint_level   = save_Vprint_level;
  Vprint_length  = save_Vprint_length;
  print_depth    = save_print_depth;
  print_readably = save_print_readably;
  inhibit_non_essential_printing_operations = 0;
  print_unbuffered--;
  UNGCPRO;
}

void
debug_print (Lisp_Object debug_print_obj)
{
  debug_print_no_newline (debug_print_obj);
  stderr_out ("\n");
}

/* Debugging kludge -- unbuffered */
/* This function provided for the benefit of the debugger.  */
void debug_backtrace (void);
void
debug_backtrace (void)
{
  /* This function can GC */
  int         old_print_readably = print_readably;
  int         old_print_depth    = print_depth;
  Lisp_Object old_print_length   = Vprint_length;
  Lisp_Object old_print_level    = Vprint_level;
  Lisp_Object old_inhibit_quit   = Vinhibit_quit;

  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (old_print_level, old_print_length, old_inhibit_quit);

  if (gc_in_progress)
    stderr_out ("** gc-in-progress!  Bad idea to print anything! **\n");

  print_depth = 0;
  print_readably = 0;
  print_unbuffered++;
  inhibit_non_essential_printing_operations = 1;
  /* Could use unwind-protect, but why bother? */
  if (debug_print_length > 0)
    Vprint_length = make_int (debug_print_length);
  if (debug_print_level > 0)
    Vprint_level = make_int (debug_print_level);

  Fbacktrace (Qexternal_debugging_output, Qt);
  stderr_out ("\n");

  Vinhibit_quit  = old_inhibit_quit;
  Vprint_level   = old_print_level;
  Vprint_length  = old_print_length;
  print_depth    = old_print_depth;
  print_readably = old_print_readably;
  inhibit_non_essential_printing_operations = 0;
  print_unbuffered--;

  UNGCPRO;
}

void
debug_short_backtrace (int length)
{
  int first = 1;
  struct backtrace *bt = backtrace_list;
  stderr_out ("   [");
  while (length > 0 && bt)
    {
      if (!first)
	{
	  stderr_out (", ");
	}
      if (COMPILED_FUNCTIONP (*bt->function))
	{
#if defined(COMPILED_FUNCTION_ANNOTATION_HACK)
	  Lisp_Object ann =
	    compiled_function_annotation (XCOMPILED_FUNCTION (*bt->function));
#else
	  Lisp_Object ann = Qnil;
#endif
	  if (!NILP (ann))
	    {
	      stderr_out ("<compiled-function from ");
	      debug_print_no_newline (ann);
	      stderr_out (">");
	    }
	  else
	    {
	      stderr_out ("<compiled-function of unknown origin>");
	    }
	}
      else
	debug_print_no_newline (*bt->function);
      first = 0;
      length--;
      bt = bt->next;
    }
  stderr_out ("]\n");
}

#endif /* debugging kludge */


void
syms_of_print (void)
{
  defsymbol (&Qstandard_output, "standard-output");

  defsymbol (&Qprint_length, "print-length");

  defsymbol (&Qprint_string_length, "print-string-length");

  defsymbol (&Qdisplay_error, "display-error");
  defsymbol (&Qprint_message_label, "print-message-label");

  DEFSUBR (Fprin1);
  DEFSUBR (Fprin1_to_string);
  DEFSUBR (Fprinc);
  DEFSUBR (Fprint);
  DEFSUBR (Ferror_message_string);
  DEFSUBR (Fdisplay_error);
  DEFSUBR (Fterpri);
  DEFSUBR (Fwrite_char);
  DEFSUBR (Falternate_debugging_output);
  DEFSUBR (Fexternal_debugging_output);
  DEFSUBR (Fopen_termscript);
  defsymbol (&Qexternal_debugging_output, "external-debugging-output");
  defsymbol (&Qalternate_debugging_output, "alternate-debugging-output");
#ifdef HAVE_MS_WINDOWS
  defsymbol (&Qmswindows_debugging_output, "mswindows-debugging-output");
#endif
  DEFSUBR (Fwith_output_to_temp_buffer);
}

void
reinit_vars_of_print (void)
{
  alternate_do_pointer = 0;
}

void
vars_of_print (void)
{
  reinit_vars_of_print ();

  DEFVAR_LISP ("standard-output", &Vstandard_output /*
Output stream `print' uses by default for outputting a character.
This may be any function of one argument.
It may also be a buffer (output is inserted before point)
or a marker (output is inserted and the marker is advanced)
or the symbol t (output appears in the minibuffer line).
*/ );
  Vstandard_output = Qt;

#ifdef LISP_FLOAT_TYPE
  DEFVAR_LISP ("float-output-format", &Vfloat_output_format /*
The format descriptor string that lisp uses to print floats.
This is a %-spec like those accepted by `printf' in C,
but with some restrictions.  It must start with the two characters `%.'.
After that comes an integer precision specification,
and then a letter which controls the format.
The letters allowed are `e', `f' and `g'.
Use `e' for exponential notation "DIG.DIGITSeEXPT"
Use `f' for decimal point notation "DIGITS.DIGITS".
Use `g' to choose the shorter of those two formats for the number at hand.
The precision in any of these cases is the number of digits following
the decimal point.  With `f', a precision of 0 means to omit the
decimal point.  0 is not allowed with `f' or `g'.

A value of nil means to use `%.16g'.

Regardless of the value of `float-output-format', a floating point number
will never be printed in such a way that it is ambiguous with an integer;
that is, a floating-point number will always be printed with a decimal
point and/or an exponent, even if the digits following the decimal point
are all zero.  This is to preserve read-equivalence.
*/ );
  Vfloat_output_format = Qnil;
#endif /* LISP_FLOAT_TYPE */

  DEFVAR_LISP ("print-length", &Vprint_length /*
Maximum length of list or vector to print before abbreviating.
A value of nil means no limit.
*/ );
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-string-length", &Vprint_string_length /*
Maximum length of string to print before abbreviating.
A value of nil means no limit.
*/ );
  Vprint_string_length = Qnil;

  DEFVAR_LISP ("print-level", &Vprint_level /*
Maximum depth of list nesting to print before abbreviating.
A value of nil means no limit.
*/ );
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", &print_escape_newlines /*
Non-nil means print newlines in strings as backslash-n.
*/ );
  print_escape_newlines = 0;

  DEFVAR_BOOL ("print-readably", &print_readably /*
If non-nil, then all objects will be printed in a readable form.
If an object has no readable representation, then an error is signalled.
When print-readably is true, compiled-function objects will be written in
 #[...] form instead of in #<compiled-function [...]> form, and two-element
 lists of the form (quote object) will be written as the equivalent 'object.
Do not SET this variable; bind it instead.
*/ );
  print_readably = 0;

  /* #### I think this should default to t.  But we'd better wait
     until we see that it works out.  */
  DEFVAR_LISP ("print-gensym", &Vprint_gensym /*
If non-nil, then uninterned symbols will be printed specially.
Uninterned symbols are those which are not present in `obarray', that is,
those which were made with `make-symbol' or by calling `intern' with a
second argument.

When print-gensym is true, such symbols will be preceded by "#:",
which causes the reader to create a new symbol instead of interning
and returning an existing one.  Beware: the #: syntax creates a new
symbol each time it is seen, so if you print an object which contains
two pointers to the same uninterned symbol, `read' will not duplicate
that structure.

If the value of `print-gensym' is a cons cell, then in addition
refrain from clearing `print-gensym-alist' on entry to and exit from
printing functions, so that the use of #...# and #...= can carry over
for several separately printed objects.
*/ );
  Vprint_gensym = Qnil;

  DEFVAR_LISP ("print-gensym-alist", &Vprint_gensym_alist /*
Association list of elements (GENSYM . N) to guide use of #N# and #N=.
In each element, GENSYM is an uninterned symbol that has been associated
with #N= for the specified value of N.
*/ );
  Vprint_gensym_alist = Qnil;

  DEFVAR_LISP ("print-message-label", &Vprint_message_label /*
Label for minibuffer messages created with `print'.  This should
generally be bound with `let' rather than set.  (See `display-message'.)
*/ );
  Vprint_message_label = Qprint;
}
