/* Code conversion functions.
   Copyright (C) 1991, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Synched up with: Mule 2.3.   Not in FSF. */

/* Rewritten by Ben Wing <ben@xemacs.org>. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "elhash.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"
#ifdef MULE
#include "mule-ccl.h"
#include "chartab.h"
#endif
#include "file-coding.h"

Lisp_Object Qcoding_system_error;

Lisp_Object Vkeyboard_coding_system;
Lisp_Object Vterminal_coding_system;
Lisp_Object Vcoding_system_for_read;
Lisp_Object Vcoding_system_for_write;
Lisp_Object Vfile_name_coding_system;

/* Table of symbols identifying each coding category. */
Lisp_Object coding_category_symbol[CODING_CATEGORY_LAST];



struct file_coding_dump {
  /* Coding system currently associated with each coding category. */
  Lisp_Object coding_category_system[CODING_CATEGORY_LAST];

  /* Table of all coding categories in decreasing order of priority.
     This describes a permutation of the possible coding categories. */
  int coding_category_by_priority[CODING_CATEGORY_LAST];

#ifdef MULE
  Lisp_Object ucs_to_mule_table[65536];
#endif
} *fcd;

static const struct lrecord_description fcd_description_1[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (struct file_coding_dump, coding_category_system), CODING_CATEGORY_LAST },
#ifdef MULE
  { XD_LISP_OBJECT_ARRAY, offsetof (struct file_coding_dump, ucs_to_mule_table), countof (fcd->ucs_to_mule_table) },
#endif
  { XD_END }
};

static const struct struct_description fcd_description = {
  sizeof (struct file_coding_dump),
  fcd_description_1
};

Lisp_Object mule_to_ucs_table;

Lisp_Object Qcoding_systemp;

Lisp_Object Qraw_text, Qno_conversion, Qccl, Qiso2022;
/* Qinternal in general.c */

Lisp_Object Qmnemonic, Qeol_type;
Lisp_Object Qcr, Qcrlf, Qlf;
Lisp_Object Qeol_cr, Qeol_crlf, Qeol_lf;
Lisp_Object Qpost_read_conversion;
Lisp_Object Qpre_write_conversion;

#ifdef MULE
Lisp_Object Qucs4, Qutf8;
Lisp_Object Qbig5, Qshift_jis;
Lisp_Object Qcharset_g0, Qcharset_g1, Qcharset_g2, Qcharset_g3;
Lisp_Object Qforce_g0_on_output, Qforce_g1_on_output;
Lisp_Object Qforce_g2_on_output, Qforce_g3_on_output;
Lisp_Object Qno_iso6429;
Lisp_Object Qinput_charset_conversion, Qoutput_charset_conversion;
Lisp_Object Qescape_quoted;
Lisp_Object Qshort, Qno_ascii_eol, Qno_ascii_cntl, Qseven, Qlock_shift;
#endif
Lisp_Object Qencode, Qdecode;

Lisp_Object Vcoding_system_hash_table;

int enable_multibyte_characters;

#ifdef MULE
/* Additional information used by the ISO2022 decoder and detector. */
struct iso2022_decoder
{
  /* CHARSET holds the character sets currently assigned to the G0
     through G3 variables.  It is initialized from the array
     INITIAL_CHARSET in CODESYS. */
  Lisp_Object charset[4];

  /* Which registers are currently invoked into the left (GL) and
     right (GR) halves of the 8-bit encoding space? */
  int register_left, register_right;

  /* ISO_ESC holds a value indicating part of an escape sequence
     that has already been seen. */
  enum iso_esc_flag esc;

  /* This records the bytes we've seen so far in an escape sequence,
     in case the sequence is invalid (we spit out the bytes unchanged). */
  unsigned char esc_bytes[8];

  /* Index for next byte to store in ISO escape sequence. */
  int esc_bytes_index;

#ifdef ENABLE_COMPOSITE_CHARS
  /* Stuff seen so far when composing a string. */
  unsigned_char_dynarr *composite_chars;
#endif

  /* If we saw an invalid designation sequence for a particular
     register, we flag it here and switch to ASCII.  The next time we
     see a valid designation for this register, we turn off the flag
     and do the designation normally, but pretend the sequence was
     invalid.  The effect of all this is that (most of the time) the
     escape sequences for both the switch to the unknown charset, and
     the switch back to the known charset, get inserted literally into
     the buffer and saved out as such.  The hope is that we can
     preserve the escape sequences so that the resulting written out
     file makes sense.  If we don't do any of this, the designation
     to the invalid charset will be preserved but that switch back
     to the known charset will probably get eaten because it was
     the same charset that was already present in the register. */
  unsigned char invalid_designated[4];

  /* We try to do similar things as above for direction-switching
     sequences.  If we encountered a direction switch while an
     invalid designation was present, or an invalid designation
     just after a direction switch (i.e. no valid designation
     encountered yet), we insert the direction-switch escape
     sequence literally into the output stream, and later on
     insert the corresponding direction-restoring escape sequence
     literally also. */
  unsigned int switched_dir_and_no_valid_charset_yet :1;
  unsigned int invalid_switch_dir :1;

  /* Tells the decoder to output the escape sequence literally
     even though it was valid.  Used in the games we play to
     avoid lossage when we encounter invalid designations. */
  unsigned int output_literally :1;
  /* We encountered a direction switch followed by an invalid
     designation.  We didn't output the direction switch
     literally because we didn't know about the invalid designation;
     but we have to do so now. */
  unsigned int output_direction_sequence :1;
};
#endif /* MULE */
EXFUN (Fcopy_coding_system, 2);
#ifdef MULE
struct detection_state;
static int detect_coding_sjis (struct detection_state *st,
			       const Extbyte *src, Lstream_data_count n);
static void decode_coding_sjis (Lstream *decoding, const Extbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static void encode_coding_sjis (Lstream *encoding, const Bufbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static int detect_coding_big5 (struct detection_state *st,
			       const Extbyte *src, Lstream_data_count n);
static void decode_coding_big5 (Lstream *decoding, const Extbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static void encode_coding_big5 (Lstream *encoding, const Bufbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static int detect_coding_ucs4 (struct detection_state *st,
			       const Extbyte *src, Lstream_data_count n);
static void decode_coding_ucs4 (Lstream *decoding, const Extbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static void encode_coding_ucs4 (Lstream *encoding, const Bufbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static int detect_coding_utf8 (struct detection_state *st,
			       const Extbyte *src, Lstream_data_count n);
static void decode_coding_utf8 (Lstream *decoding, const Extbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static void encode_coding_utf8 (Lstream *encoding, const Bufbyte *src,
				unsigned_char_dynarr *dst, Lstream_data_count n);
static int postprocess_iso2022_mask (int mask);
static void reset_iso2022 (Lisp_Object coding_system,
			   struct iso2022_decoder *iso);
static int detect_coding_iso2022 (struct detection_state *st,
				  const Extbyte *src, Lstream_data_count n);
static void decode_coding_iso2022 (Lstream *decoding, const Extbyte *src,
				   unsigned_char_dynarr *dst, Lstream_data_count n);
static void encode_coding_iso2022 (Lstream *encoding, const Bufbyte *src,
				   unsigned_char_dynarr *dst, Lstream_data_count n);
#endif /* MULE */
static void decode_coding_no_conversion (Lstream *decoding, const Extbyte *src,
					 unsigned_char_dynarr *dst, Lstream_data_count n);
static void encode_coding_no_conversion (Lstream *encoding, const Bufbyte *src,
					 unsigned_char_dynarr *dst, Lstream_data_count n);
static void mule_decode (Lstream *decoding, const Extbyte *src,
			 unsigned_char_dynarr *dst, Lstream_data_count n);
static void mule_encode (Lstream *encoding, const Bufbyte *src,
			 unsigned_char_dynarr *dst, Lstream_data_count n);

typedef struct codesys_prop codesys_prop;
struct codesys_prop
{
  Lisp_Object sym;
  int prop_type;
};

typedef struct
{
  Dynarr_declare (codesys_prop);
} codesys_prop_dynarr;

static const struct lrecord_description codesys_prop_description_1[] = {
  { XD_LISP_OBJECT, offsetof (codesys_prop, sym) },
  { XD_END }
};

static const struct struct_description codesys_prop_description = {
  sizeof (codesys_prop),
  codesys_prop_description_1
};

static const struct lrecord_description codesys_prop_dynarr_description_1[] = {
  XD_DYNARR_DESC (codesys_prop_dynarr, &codesys_prop_description),
  { XD_END }
};

static const struct struct_description codesys_prop_dynarr_description = {
  sizeof (codesys_prop_dynarr),
  codesys_prop_dynarr_description_1
};

codesys_prop_dynarr *the_codesys_prop_dynarr;

enum codesys_prop_enum
{
  CODESYS_PROP_ALL_OK,
  CODESYS_PROP_ISO2022,
  CODESYS_PROP_CCL
};


/************************************************************************/
/*                       Coding system functions                        */
/************************************************************************/

static Lisp_Object mark_coding_system (Lisp_Object);
static void print_coding_system (Lisp_Object, Lisp_Object, int);
static void finalize_coding_system (void *header, int for_disksave);

#ifdef MULE
static const struct lrecord_description ccs_description_1[] = {
  { XD_LISP_OBJECT, offsetof (charset_conversion_spec, from_charset) },
  { XD_LISP_OBJECT, offsetof (charset_conversion_spec, to_charset) },
  { XD_END }
};

static const struct struct_description ccs_description = {
  sizeof (charset_conversion_spec),
  ccs_description_1
};

static const struct lrecord_description ccsd_description_1[] = {
  XD_DYNARR_DESC (charset_conversion_spec_dynarr, &ccs_description),
  { XD_END }
};

static const struct struct_description ccsd_description = {
  sizeof (charset_conversion_spec_dynarr),
  ccsd_description_1
};
#endif

static const struct lrecord_description coding_system_description[] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, doc_string) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, mnemonic) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, post_read_conversion) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, pre_write_conversion) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, eol_lf) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, eol_crlf) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, eol_cr) },
#ifdef MULE
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Coding_System, iso2022.initial_charset), 4 },
  { XD_STRUCT_PTR,  offsetof (Lisp_Coding_System, iso2022.input_conv),  1, &ccsd_description },
  { XD_STRUCT_PTR,  offsetof (Lisp_Coding_System, iso2022.output_conv), 1, &ccsd_description },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, ccl.decode) },
  { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, ccl.encode) },
#endif
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("coding-system", coding_system,
			       mark_coding_system, print_coding_system,
			       finalize_coding_system,
			       0, 0, coding_system_description,
			       Lisp_Coding_System);

static Lisp_Object
mark_coding_system (Lisp_Object obj)
{
  Lisp_Coding_System *codesys = XCODING_SYSTEM (obj);

  mark_object (CODING_SYSTEM_NAME (codesys));
  mark_object (CODING_SYSTEM_DOC_STRING (codesys));
  mark_object (CODING_SYSTEM_MNEMONIC (codesys));
  mark_object (CODING_SYSTEM_EOL_LF (codesys));
  mark_object (CODING_SYSTEM_EOL_CRLF (codesys));
  mark_object (CODING_SYSTEM_EOL_CR (codesys));

  switch (CODING_SYSTEM_TYPE (codesys))
    {
#ifdef MULE
      int i;
    case CODESYS_ISO2022:
      for (i = 0; i < 4; i++)
	mark_object (CODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i));
      if (codesys->iso2022.input_conv)
	{
	  for (i = 0; i < Dynarr_length (codesys->iso2022.input_conv); i++)
	    {
	      struct charset_conversion_spec *ccs =
		Dynarr_atp (codesys->iso2022.input_conv, i);
	      mark_object (ccs->from_charset);
	      mark_object (ccs->to_charset);
	    }
	}
      if (codesys->iso2022.output_conv)
	{
	  for (i = 0; i < Dynarr_length (codesys->iso2022.output_conv); i++)
	    {
	      struct charset_conversion_spec *ccs =
		Dynarr_atp (codesys->iso2022.output_conv, i);
	      mark_object (ccs->from_charset);
	      mark_object (ccs->to_charset);
	    }
	}
      break;

    case CODESYS_CCL:
      mark_object (CODING_SYSTEM_CCL_DECODE (codesys));
      mark_object (CODING_SYSTEM_CCL_ENCODE (codesys));
      break;
#endif /* MULE */
    default:
      break;
    }

  mark_object (CODING_SYSTEM_PRE_WRITE_CONVERSION (codesys));
  return CODING_SYSTEM_POST_READ_CONVERSION (codesys);
}

static void
print_coding_system (Lisp_Object obj, Lisp_Object printcharfun,
		     int escapeflag)
{
  Lisp_Coding_System *c = XCODING_SYSTEM (obj);
  if (print_readably)
    error ("printing unreadable object #<coding_system 0x%x>",
	   c->header.uid);

  write_c_string ("#<coding_system ", printcharfun);
  print_internal (c->name, printcharfun, 1);
  write_c_string (">", printcharfun);
}

static void
finalize_coding_system (void *header, int for_disksave)
{
  Lisp_Coding_System *c = (Lisp_Coding_System *) header;
  /* Since coding systems never go away, this function is not
     necessary.  But it would be necessary if we changed things
     so that coding systems could go away. */
  if (!for_disksave) /* see comment in lstream.c */
    {
      switch (CODING_SYSTEM_TYPE (c))
	{
#ifdef MULE
	case CODESYS_ISO2022:
	  if (c->iso2022.input_conv)
	    {
	      Dynarr_free (c->iso2022.input_conv);
	      c->iso2022.input_conv = 0;
	    }
	  if (c->iso2022.output_conv)
	    {
	      Dynarr_free (c->iso2022.output_conv);
	      c->iso2022.output_conv = 0;
	    }
	  break;
#endif /* MULE */
	default:
	  break;
	}
    }
}

static eol_type_t
symbol_to_eol_type (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol))      return EOL_AUTODETECT;
  if (EQ (symbol, Qlf))   return EOL_LF;
  if (EQ (symbol, Qcrlf)) return EOL_CRLF;
  if (EQ (symbol, Qcr))   return EOL_CR;

  signal_simple_error ("Unrecognized eol type", symbol);
  return EOL_AUTODETECT; /* not reached */
}

static Lisp_Object
eol_type_to_symbol (eol_type_t type)
{
  switch (type)
    {
    default: abort ();
    case EOL_LF:         return Qlf;
    case EOL_CRLF:       return Qcrlf;
    case EOL_CR:         return Qcr;
    case EOL_AUTODETECT: return Qnil;
    }
}

static void
setup_eol_coding_systems (Lisp_Coding_System *codesys)
{
  Lisp_Object codesys_obj;
  int len = string_length (XSYMBOL (CODING_SYSTEM_NAME (codesys))->name);
  char *codesys_name = (char *) alloca (len + 7);
  int mlen = -1;
  char *codesys_mnemonic=0;

  Lisp_Object codesys_name_sym, sub_codesys_obj;

  /* kludge */

  XSETCODING_SYSTEM (codesys_obj, codesys);

  memcpy (codesys_name,
	  string_data (XSYMBOL (CODING_SYSTEM_NAME (codesys))->name), len);

  if (STRINGP (CODING_SYSTEM_MNEMONIC (codesys)))
    {
      mlen = XSTRING_LENGTH (CODING_SYSTEM_MNEMONIC (codesys));
      codesys_mnemonic = (char *) alloca (mlen + 7);
      memcpy (codesys_mnemonic,
	      XSTRING_DATA (CODING_SYSTEM_MNEMONIC (codesys)), mlen);
    }

#define DEFINE_SUB_CODESYS(op_sys, op_sys_abbr, Type) do {			\
  strcpy (codesys_name + len, "-" op_sys);					\
  if (mlen != -1)								\
    strcpy (codesys_mnemonic + mlen, op_sys_abbr);				\
  codesys_name_sym = intern (codesys_name);					\
  sub_codesys_obj = Fcopy_coding_system (codesys_obj, codesys_name_sym);	\
  XCODING_SYSTEM_EOL_TYPE (sub_codesys_obj) = Type;				\
  if (mlen != -1)								\
    XCODING_SYSTEM_MNEMONIC(sub_codesys_obj) =					\
      build_string (codesys_mnemonic);						\
  CODING_SYSTEM_##Type (codesys) = sub_codesys_obj;				\
} while (0)

  DEFINE_SUB_CODESYS("unix", "", EOL_LF);
  DEFINE_SUB_CODESYS("dos",  ":T", EOL_CRLF);
  DEFINE_SUB_CODESYS("mac",  ":t", EOL_CR);
}

DEFUN ("coding-system-p", Fcoding_system_p, 1, 1, 0, /*
Return t if OBJECT is a coding system.
A coding system is an object that defines how text containing multiple
character sets is encoded into a stream of (typically 8-bit) bytes.
The coding system is used to decode the stream into a series of
characters (which may be from multiple charsets) when the text is read
from a file or process, and is used to encode the text back into the
same format when it is written out to a file or process.

For example, many ISO2022-compliant coding systems (such as Compound
Text, which is used for inter-client data under the X Window System)
use escape sequences to switch between different charsets -- Japanese
Kanji, for example, is invoked with "ESC $ ( B"; ASCII is invoked
with "ESC ( B"; and Cyrillic is invoked with "ESC - L".  See
`make-coding-system' for more information.

Coding systems are normally identified using a symbol, and the
symbol is accepted in place of the actual coding system object whenever
a coding system is called for. (This is similar to how faces work.)
*/
       (object))
{
  return CODING_SYSTEMP (object) ? Qt : Qnil;
}

DEFUN ("find-coding-system", Ffind_coding_system, 1, 1, 0, /*
Retrieve the coding system of the given name.

If CODING-SYSTEM-OR-NAME is a coding-system object, it is simply
returned.  Otherwise, CODING-SYSTEM-OR-NAME should be a symbol.
If there is no such coding system, nil is returned.  Otherwise the
associated coding system object is returned.
*/
       (coding_system_or_name))
{
  if (NILP (coding_system_or_name))
    coding_system_or_name = Qbinary;
  else if (CODING_SYSTEMP (coding_system_or_name))
    return coding_system_or_name;
  else
    CHECK_SYMBOL (coding_system_or_name);

  while (1)
    {
      coding_system_or_name =
	Fgethash (coding_system_or_name, Vcoding_system_hash_table, Qnil);

      if (CODING_SYSTEMP (coding_system_or_name) || NILP (coding_system_or_name))
	return coding_system_or_name;
    }
}

DEFUN ("get-coding-system", Fget_coding_system, 1, 1, 0, /*
Retrieve the coding system of the given name.
Same as `find-coding-system' except that if there is no such
coding system, an error is signaled instead of returning nil.
*/
       (name))
{
  Lisp_Object coding_system = Ffind_coding_system (name);

  if (NILP (coding_system))
    signal_simple_error ("No such coding system", name);
  return coding_system;
}

/* We store the coding systems in hash tables with the names as the key and the
   actual coding system object as the value.  Occasionally we need to use them
   in a list format.  These routines provide us with that. */
struct coding_system_list_closure
{
  Lisp_Object *coding_system_list;
};

static int
add_coding_system_to_list_mapper (Lisp_Object key, Lisp_Object value,
				  void *coding_system_list_closure)
{
  /* This function can GC */
  struct coding_system_list_closure *cscl =
    (struct coding_system_list_closure *) coding_system_list_closure;
  Lisp_Object *coding_system_list = cscl->coding_system_list;

  *coding_system_list = Fcons (key, *coding_system_list);
  return 0;
}

DEFUN ("coding-system-list", Fcoding_system_list, 0, 0, 0, /*
Return a list of the names of all defined coding systems.
*/
       ())
{
  Lisp_Object coding_system_list = Qnil;
  struct gcpro gcpro1;
  struct coding_system_list_closure coding_system_list_closure;

  GCPRO1 (coding_system_list);
  coding_system_list_closure.coding_system_list = &coding_system_list;
  elisp_maphash (add_coding_system_to_list_mapper, Vcoding_system_hash_table,
		 &coding_system_list_closure);
  UNGCPRO;

  return coding_system_list;
}

DEFUN ("coding-system-name", Fcoding_system_name, 1, 1, 0, /*
Return the name of the given coding system.
*/
       (coding_system))
{
  coding_system = Fget_coding_system (coding_system);
  return XCODING_SYSTEM_NAME (coding_system);
}

static Lisp_Coding_System *
allocate_coding_system (enum coding_system_type type, Lisp_Object name)
{
  Lisp_Coding_System *codesys =
    alloc_lcrecord_type (Lisp_Coding_System, &lrecord_coding_system);

  zero_lcrecord (codesys);
  CODING_SYSTEM_PRE_WRITE_CONVERSION (codesys) = Qnil;
  CODING_SYSTEM_POST_READ_CONVERSION (codesys) = Qnil;
  CODING_SYSTEM_EOL_TYPE (codesys) = EOL_AUTODETECT;
  CODING_SYSTEM_EOL_CRLF (codesys) = Qnil;
  CODING_SYSTEM_EOL_CR   (codesys) = Qnil;
  CODING_SYSTEM_EOL_LF   (codesys) = Qnil;
  CODING_SYSTEM_TYPE     (codesys) = type;
  CODING_SYSTEM_MNEMONIC (codesys) = Qnil;
#ifdef MULE
  if (type == CODESYS_ISO2022)
    {
      int i;
      for (i = 0; i < 4; i++)
	CODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i) = Qnil;
    }
  else if (type == CODESYS_CCL)
    {
      CODING_SYSTEM_CCL_DECODE (codesys) = Qnil;
      CODING_SYSTEM_CCL_ENCODE (codesys) = Qnil;
    }
#endif /* MULE */
  CODING_SYSTEM_NAME (codesys) = name;

  return codesys;
}

#ifdef MULE
/* Given a list of charset conversion specs as specified in a Lisp
   program, parse it into STORE_HERE. */

static void
parse_charset_conversion_specs (charset_conversion_spec_dynarr *store_here,
				Lisp_Object spec_list)
{
  Lisp_Object rest;

  EXTERNAL_LIST_LOOP (rest, spec_list)
    {
      Lisp_Object car = XCAR (rest);
      Lisp_Object from, to;
      struct charset_conversion_spec spec;

      if (!CONSP (car) || !CONSP (XCDR (car)) || !NILP (XCDR (XCDR (car))))
	signal_simple_error ("Invalid charset conversion spec", car);
      from = Fget_charset (XCAR (car));
      to = Fget_charset (XCAR (XCDR (car)));
      if (XCHARSET_TYPE (from) != XCHARSET_TYPE (to))
	signal_simple_error_2
	  ("Attempted conversion between different charset types",
	   from, to);
      spec.from_charset = from;
      spec.to_charset = to;

      Dynarr_add (store_here, spec);
    }
}

/* Given a dynarr LOAD_HERE of internally-stored charset conversion
   specs, return the equivalent as the Lisp programmer would see it.

   If LOAD_HERE is 0, return Qnil. */

static Lisp_Object
unparse_charset_conversion_specs (charset_conversion_spec_dynarr *load_here)
{
  int i;
  Lisp_Object result;

  if (!load_here)
    return Qnil;
  for (i = 0, result = Qnil; i < Dynarr_length (load_here); i++)
    {
      struct charset_conversion_spec *ccs = Dynarr_atp (load_here, i);
      result = Fcons (list2 (ccs->from_charset, ccs->to_charset), result);
    }

  return Fnreverse (result);
}

#endif /* MULE */

DEFUN ("make-coding-system", Fmake_coding_system, 2, 4, 0, /*
Register symbol NAME as a coding system.

TYPE describes the conversion method used and should be one of

nil or 'undecided
     Automatic conversion.  XEmacs attempts to detect the coding system
     used in the file.
'no-conversion
     No conversion.  Use this for binary files and such.  On output,
     graphic characters that are not in ASCII or Latin-1 will be
     replaced by a ?. (For a no-conversion-encoded buffer, these
     characters will only be present if you explicitly insert them.)
'shift-jis
     Shift-JIS (a Japanese encoding commonly used in PC operating systems).
'ucs-4
     ISO 10646 UCS-4 encoding.
'utf-8
     ISO 10646 UTF-8 encoding.
'iso2022
     Any ISO2022-compliant encoding.  Among other things, this includes
     JIS (the Japanese encoding commonly used for e-mail), EUC (the
     standard Unix encoding for Japanese and other languages), and
     Compound Text (the encoding used in X11).  You can specify more
     specific information about the conversion with the PROPS argument.
'big5
     Big5 (the encoding commonly used for Taiwanese).
'ccl
     The conversion is performed using a user-written pseudo-code
     program.  CCL (Code Conversion Language) is the name of this
     pseudo-code.
'internal
     Write out or read in the raw contents of the memory representing
     the buffer's text.  This is primarily useful for debugging
     purposes, and is only enabled when XEmacs has been compiled with
     DEBUG_XEMACS defined (via the --debug configure option).
     WARNING: Reading in a file using 'internal conversion can result
     in an internal inconsistency in the memory representing a
     buffer's text, which will produce unpredictable results and may
     cause XEmacs to crash.  Under normal circumstances you should
     never use 'internal conversion.

DOC-STRING is a string describing the coding system.

PROPS is a property list, describing the specific nature of the
character set.  Recognized properties are:

'mnemonic
     String to be displayed in the modeline when this coding system is
     active.

'eol-type
     End-of-line conversion to be used.  It should be one of

	nil
		Automatically detect the end-of-line type (LF, CRLF,
		or CR).  Also generate subsidiary coding systems named
		`NAME-unix', `NAME-dos', and `NAME-mac', that are
		identical to this coding system but have an EOL-TYPE
		value of 'lf, 'crlf, and 'cr, respectively.
	'lf
		The end of a line is marked externally using ASCII LF.
		Since this is also the way that XEmacs represents an
		end-of-line internally, specifying this option results
		in no end-of-line conversion.  This is the standard
		format for Unix text files.
	'crlf
		The end of a line is marked externally using ASCII
		CRLF.  This is the standard format for MS-DOS text
		files.
	'cr
		The end of a line is marked externally using ASCII CR.
		This is the standard format for Macintosh text files.
	t
		Automatically detect the end-of-line type but do not
		generate subsidiary coding systems.  (This value is
		converted to nil when stored internally, and
		`coding-system-property' will return nil.)

'post-read-conversion
     Function called after a file has been read in, to perform the
     decoding.  Called with two arguments, START and END, denoting
     a region of the current buffer to be decoded.

'pre-write-conversion
     Function called before a file is written out, to perform the
     encoding.  Called with two arguments, START and END, denoting
     a region of the current buffer to be encoded.


The following additional properties are recognized if TYPE is 'iso2022:

'charset-g0
'charset-g1
'charset-g2
'charset-g3
     The character set initially designated to the G0 - G3 registers.
     The value should be one of

          -- A charset object (designate that character set)
	  -- nil (do not ever use this register)
	  -- t (no character set is initially designated to
		the register, but may be later on; this automatically
		sets the corresponding `force-g*-on-output' property)

'force-g0-on-output
'force-g1-on-output
'force-g2-on-output
'force-g2-on-output
     If non-nil, send an explicit designation sequence on output before
     using the specified register.

'short
     If non-nil, use the short forms "ESC $ @", "ESC $ A", and
     "ESC $ B" on output in place of the full designation sequences
     "ESC $ ( @", "ESC $ ( A", and "ESC $ ( B".

'no-ascii-eol
     If non-nil, don't designate ASCII to G0 at each end of line on output.
     Setting this to non-nil also suppresses other state-resetting that
     normally happens at the end of a line.

'no-ascii-cntl
     If non-nil, don't designate ASCII to G0 before control chars on output.

'seven
     If non-nil, use 7-bit environment on output.  Otherwise, use 8-bit
     environment.

'lock-shift
     If non-nil, use locking-shift (SO/SI) instead of single-shift
     or designation by escape sequence.

'no-iso6429
     If non-nil, don't use ISO6429's direction specification.

'escape-quoted
     If non-nil, literal control characters that are the same as
     the beginning of a recognized ISO2022 or ISO6429 escape sequence
     (in particular, ESC (0x1B), SO (0x0E), SI (0x0F), SS2 (0x8E),
     SS3 (0x8F), and CSI (0x9B)) are "quoted" with an escape character
     so that they can be properly distinguished from an escape sequence.
     (Note that doing this results in a non-portable encoding.) This
     encoding flag is used for byte-compiled files.  Note that ESC
     is a good choice for a quoting character because there are no
     escape sequences whose second byte is a character from the Control-0
     or Control-1 character sets; this is explicitly disallowed by the
     ISO2022 standard.

'input-charset-conversion
     A list of conversion specifications, specifying conversion of
     characters in one charset to another when decoding is performed.
     Each specification is a list of two elements: the source charset,
     and the destination charset.

'output-charset-conversion
     A list of conversion specifications, specifying conversion of
     characters in one charset to another when encoding is performed.
     The form of each specification is the same as for
     'input-charset-conversion.


The following additional properties are recognized (and required)
if TYPE is 'ccl:

'decode
     CCL program used for decoding (converting to internal format).

'encode
     CCL program used for encoding (converting to external format).
*/
       (name, type, doc_string, props))
{
  Lisp_Coding_System *codesys;
  enum coding_system_type ty;
  int need_to_setup_eol_systems = 1;

  /* Convert type to constant */
  if (NILP (type) || EQ (type, Qundecided))
                                      { ty = CODESYS_AUTODETECT; }
#ifdef MULE
  else if (EQ (type, Qshift_jis))     { ty = CODESYS_SHIFT_JIS; }
  else if (EQ (type, Qiso2022))       { ty = CODESYS_ISO2022; }
  else if (EQ (type, Qbig5))          { ty = CODESYS_BIG5; }
  else if (EQ (type, Qucs4))          { ty = CODESYS_UCS4; }
  else if (EQ (type, Qutf8))          { ty = CODESYS_UTF8; }
  else if (EQ (type, Qccl))           { ty = CODESYS_CCL; }
#endif
  else if (EQ (type, Qno_conversion)) { ty = CODESYS_NO_CONVERSION; }
#ifdef DEBUG_XEMACS
  else if (EQ (type, Qinternal))      { ty = CODESYS_INTERNAL; }
#endif
  else
    signal_simple_error ("Invalid coding system type", type);

  CHECK_SYMBOL (name);

  codesys = allocate_coding_system (ty, name);

  if (NILP (doc_string))
    doc_string = build_string ("");
  else
    CHECK_STRING (doc_string);
  CODING_SYSTEM_DOC_STRING (codesys) = doc_string;

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, props)
      {
	if (EQ (key, Qmnemonic))
	  {
	    if (!NILP (value))
	      CHECK_STRING (value);
	    CODING_SYSTEM_MNEMONIC (codesys) = value;
	  }

	else if (EQ (key, Qeol_type))
	  {
	    need_to_setup_eol_systems = NILP (value);
	    if (EQ (value, Qt))
	      value = Qnil;
	    CODING_SYSTEM_EOL_TYPE (codesys) = symbol_to_eol_type (value);
	  }

	else if (EQ (key, Qpost_read_conversion)) CODING_SYSTEM_POST_READ_CONVERSION (codesys) = value;
	else if (EQ (key, Qpre_write_conversion)) CODING_SYSTEM_PRE_WRITE_CONVERSION (codesys) = value;
#ifdef MULE
	else if (ty == CODESYS_ISO2022)
	  {
#define FROB_INITIAL_CHARSET(charset_num) \
  CODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, charset_num) = \
    ((EQ (value, Qt) || EQ (value, Qnil)) ? value : Fget_charset (value))

	    if      (EQ (key, Qcharset_g0)) FROB_INITIAL_CHARSET (0);
	    else if (EQ (key, Qcharset_g1)) FROB_INITIAL_CHARSET (1);
	    else if (EQ (key, Qcharset_g2)) FROB_INITIAL_CHARSET (2);
	    else if (EQ (key, Qcharset_g3)) FROB_INITIAL_CHARSET (3);

#define FROB_FORCE_CHARSET(charset_num) \
  CODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (codesys, charset_num) = !NILP (value)

	    else if (EQ (key, Qforce_g0_on_output)) FROB_FORCE_CHARSET (0);
	    else if (EQ (key, Qforce_g1_on_output)) FROB_FORCE_CHARSET (1);
	    else if (EQ (key, Qforce_g2_on_output)) FROB_FORCE_CHARSET (2);
	    else if (EQ (key, Qforce_g3_on_output)) FROB_FORCE_CHARSET (3);

#define FROB_BOOLEAN_PROPERTY(prop) \
  CODING_SYSTEM_ISO2022_##prop (codesys) = !NILP (value)

	    else if (EQ (key, Qshort))         FROB_BOOLEAN_PROPERTY (SHORT);
	    else if (EQ (key, Qno_ascii_eol))  FROB_BOOLEAN_PROPERTY (NO_ASCII_EOL);
	    else if (EQ (key, Qno_ascii_cntl)) FROB_BOOLEAN_PROPERTY (NO_ASCII_CNTL);
	    else if (EQ (key, Qseven))         FROB_BOOLEAN_PROPERTY (SEVEN);
	    else if (EQ (key, Qlock_shift))    FROB_BOOLEAN_PROPERTY (LOCK_SHIFT);
	    else if (EQ (key, Qno_iso6429))    FROB_BOOLEAN_PROPERTY (NO_ISO6429);
	    else if (EQ (key, Qescape_quoted)) FROB_BOOLEAN_PROPERTY (ESCAPE_QUOTED);

	    else if (EQ (key, Qinput_charset_conversion))
	      {
		codesys->iso2022.input_conv =
		  Dynarr_new (charset_conversion_spec);
		parse_charset_conversion_specs (codesys->iso2022.input_conv,
						value);
	      }
	    else if (EQ (key, Qoutput_charset_conversion))
	      {
		codesys->iso2022.output_conv =
		  Dynarr_new (charset_conversion_spec);
		parse_charset_conversion_specs (codesys->iso2022.output_conv,
						value);
	      }
	    else
	      signal_simple_error ("Unrecognized property", key);
	  }
	else if (EQ (type, Qccl))
	  {
	    Lisp_Object sym;
	    struct ccl_program test_ccl;
	    Extbyte *suffix;

	    /* Check key first.  */
	    if (EQ (key, Qdecode))
	      suffix = "-ccl-decode";
	    else if (EQ (key, Qencode))
	      suffix = "-ccl-encode";
	    else
	      signal_simple_error ("Unrecognized property", key);

	    /* If value is vector, register it as a ccl program
	       associated with an newly created symbol for
	       backward compatibility.  */
	    if (VECTORP (value))
	      {
		sym = Fintern (concat2 (Fsymbol_name (name),
					build_string (suffix)),
			       Qnil);
		Fregister_ccl_program (sym, value);
	      }
	    else
	      {
		CHECK_SYMBOL (value);
		sym = value;
	      }
	    /* check if the given ccl programs are valid.  */
	    if (setup_ccl_program (&test_ccl, sym) < 0)
	      signal_simple_error ("Invalid CCL program", value);

	    if (EQ (key, Qdecode))
	      CODING_SYSTEM_CCL_DECODE (codesys) = sym;
	    else if (EQ (key, Qencode))
	      CODING_SYSTEM_CCL_ENCODE (codesys) = sym;

	  }
#endif /* MULE */
	else
	  signal_simple_error ("Unrecognized property", key);
      }
  }

  if (need_to_setup_eol_systems)
    setup_eol_coding_systems (codesys);

  {
    Lisp_Object codesys_obj;
    XSETCODING_SYSTEM (codesys_obj, codesys);
    Fputhash (name, codesys_obj, Vcoding_system_hash_table);
    return codesys_obj;
  }
}

DEFUN ("copy-coding-system", Fcopy_coding_system, 2, 2, 0, /*
Copy OLD-CODING-SYSTEM to NEW-NAME.
If NEW-NAME does not name an existing coding system, a new one will
be created.
*/
       (old_coding_system, new_name))
{
  Lisp_Object new_coding_system;
  old_coding_system = Fget_coding_system (old_coding_system);
  new_coding_system = Ffind_coding_system (new_name);
  if (NILP (new_coding_system))
    {
      XSETCODING_SYSTEM (new_coding_system,
			 allocate_coding_system
			 (XCODING_SYSTEM_TYPE (old_coding_system),
			  new_name));
      Fputhash (new_name, new_coding_system, Vcoding_system_hash_table);
    }

  {
    Lisp_Coding_System *to = XCODING_SYSTEM (new_coding_system);
    Lisp_Coding_System *from = XCODING_SYSTEM (old_coding_system);
    memcpy (((char *) to  ) + sizeof (to->header),
	    ((char *) from) + sizeof (from->header),
	    sizeof (*from) - sizeof (from->header));
    to->name = new_name;
  }
  return new_coding_system;
}

DEFUN ("coding-system-canonical-name-p", Fcoding_system_canonical_name_p, 1, 1, 0, /*
Return t if OBJECT names a coding system, and is not a coding system alias.
*/
       (object))
{
  return CODING_SYSTEMP (Fgethash (object, Vcoding_system_hash_table, Qnil))
    ? Qt : Qnil;
}

DEFUN ("coding-system-alias-p", Fcoding_system_alias_p, 1, 1, 0, /*
Return t if OBJECT is a coding system alias.
All coding system aliases are created by `define-coding-system-alias'.
*/
       (object))
{
  return SYMBOLP (Fgethash (object, Vcoding_system_hash_table, Qzero))
    ? Qt : Qnil;
}

DEFUN ("coding-system-aliasee", Fcoding_system_aliasee, 1, 1, 0, /*
Return the coding-system symbol for which symbol ALIAS is an alias.
*/
       (alias))
{
  Lisp_Object aliasee = Fgethash (alias, Vcoding_system_hash_table, Qnil);
  if (SYMBOLP (aliasee))
    return aliasee;
  else
    signal_simple_error ("Symbol is not a coding system alias", alias);
  return Qnil;		/* To keep the compiler happy */
}

static Lisp_Object
append_suffix_to_symbol (Lisp_Object symbol, const char *ascii_string)
{
  return Fintern (concat2 (Fsymbol_name (symbol), build_string (ascii_string)),
		  Qnil);
}

/* A maphash function, for removing dangling coding system aliases. */
static int
dangling_coding_system_alias_p (Lisp_Object alias,
				Lisp_Object aliasee,
				void *dangling_aliases)
{
  if (SYMBOLP (aliasee)
      && NILP (Fgethash (aliasee, Vcoding_system_hash_table, Qnil)))
    {
      (*(int *) dangling_aliases)++;
      return 1;
    }
  else
    return 0;
}

DEFUN ("define-coding-system-alias", Fdefine_coding_system_alias, 2, 2, 0, /*
Define symbol ALIAS as an alias for coding system ALIASEE.

You can use this function to redefine an alias that has already been defined,
but you cannot redefine a name which is the canonical name for a coding system.
\(a canonical name of a coding system is what is returned when you call
`coding-system-name' on a coding system).

ALIASEE itself can be an alias, which allows you to define nested aliases.

You are forbidden, however, from creating alias loops or `dangling' aliases.
These will be detected, and an error will be signaled if you attempt to do so.

If ALIASEE is nil, then ALIAS will simply be undefined.

See also `coding-system-alias-p', `coding-system-aliasee',
and `coding-system-canonical-name-p'.
*/
       (alias, aliasee))
{
  Lisp_Object real_coding_system, probe;

  CHECK_SYMBOL (alias);

  if (!NILP (Fcoding_system_canonical_name_p (alias)))
    signal_simple_error
      ("Symbol is the canonical name of a coding system and cannot be redefined",
       alias);

  if (NILP (aliasee))
    {
      Lisp_Object subsidiary_unix = append_suffix_to_symbol (alias, "-unix");
      Lisp_Object subsidiary_dos  = append_suffix_to_symbol (alias, "-dos");
      Lisp_Object subsidiary_mac  = append_suffix_to_symbol (alias, "-mac");

      Fremhash (alias, Vcoding_system_hash_table);

      /* Undefine subsidiary aliases,
	 presumably created by a previous call to this function */
      if (! NILP (Fcoding_system_alias_p (subsidiary_unix)) &&
	  ! NILP (Fcoding_system_alias_p (subsidiary_dos))  &&
	  ! NILP (Fcoding_system_alias_p (subsidiary_mac)))
	{
	  Fdefine_coding_system_alias (subsidiary_unix, Qnil);
	  Fdefine_coding_system_alias (subsidiary_dos,  Qnil);
	  Fdefine_coding_system_alias (subsidiary_mac,  Qnil);
	}

      /* Undefine dangling coding system aliases. */
      {
	int dangling_aliases;

	do {
	  dangling_aliases = 0;
	  elisp_map_remhash (dangling_coding_system_alias_p,
			     Vcoding_system_hash_table,
			     &dangling_aliases);
	} while (dangling_aliases > 0);
      }

      return Qnil;
    }

  if (CODING_SYSTEMP (aliasee))
    aliasee = XCODING_SYSTEM_NAME (aliasee);

  /* Checks that aliasee names a coding-system */
  real_coding_system = Fget_coding_system (aliasee);

  /* Check for coding system alias loops */
  if (EQ (alias, aliasee))
    alias_loop: signal_simple_error_2
      ("Attempt to create a coding system alias loop", alias, aliasee);

  for (probe = aliasee;
       SYMBOLP (probe);
       probe = Fgethash (probe, Vcoding_system_hash_table, Qzero))
    {
      if (EQ (probe, alias))
	goto alias_loop;
    }

  Fputhash (alias, aliasee, Vcoding_system_hash_table);

  /* Set up aliases for subsidiaries.
     #### There must be a better way to handle subsidiary coding systems. */
  {
    static const char *suffixes[] = { "-unix", "-dos", "-mac" };
    int i;
    for (i = 0; i < countof (suffixes); i++)
      {
	Lisp_Object alias_subsidiary =
	  append_suffix_to_symbol (alias, suffixes[i]);
	Lisp_Object aliasee_subsidiary =
	  append_suffix_to_symbol (aliasee, suffixes[i]);

	if (! NILP (Ffind_coding_system (aliasee_subsidiary)))
	  Fdefine_coding_system_alias (alias_subsidiary, aliasee_subsidiary);
      }
  }
  /* FSF return value is a vector of [ALIAS-unix ALIAS-dos ALIAS-mac],
     but it doesn't look intentional, so I'd rather return something
     meaningful or nothing at all. */
  return Qnil;
}

static Lisp_Object
subsidiary_coding_system (Lisp_Object coding_system, eol_type_t type)
{
  Lisp_Coding_System *cs = XCODING_SYSTEM (coding_system);
  Lisp_Object new_coding_system;

  if (CODING_SYSTEM_EOL_TYPE (cs) != EOL_AUTODETECT)
    return coding_system;

  switch (type)
    {
    case EOL_AUTODETECT: return coding_system;
    case EOL_LF:   new_coding_system = CODING_SYSTEM_EOL_LF   (cs); break;
    case EOL_CR:   new_coding_system = CODING_SYSTEM_EOL_CR   (cs); break;
    case EOL_CRLF: new_coding_system = CODING_SYSTEM_EOL_CRLF (cs); break;
    default:       abort (); return Qnil;
    }

  return NILP (new_coding_system) ? coding_system : new_coding_system;
}

DEFUN ("subsidiary-coding-system", Fsubsidiary_coding_system, 2, 2, 0, /*
Return the subsidiary coding system of CODING-SYSTEM with eol type EOL-TYPE.
*/
       (coding_system, eol_type))
{
  coding_system = Fget_coding_system (coding_system);

  return subsidiary_coding_system (coding_system,
				   symbol_to_eol_type (eol_type));
}


/************************************************************************/
/*                         Coding system accessors                      */
/************************************************************************/

DEFUN ("coding-system-doc-string", Fcoding_system_doc_string, 1, 1, 0, /*
Return the doc string for CODING-SYSTEM.
*/
       (coding_system))
{
  coding_system = Fget_coding_system (coding_system);
  return XCODING_SYSTEM_DOC_STRING (coding_system);
}

DEFUN ("coding-system-type", Fcoding_system_type, 1, 1, 0, /*
Return the type of CODING-SYSTEM.
*/
       (coding_system))
{
  switch (XCODING_SYSTEM_TYPE (Fget_coding_system (coding_system)))
    {
    default: abort ();
    case CODESYS_AUTODETECT:	return Qundecided;
#ifdef MULE
    case CODESYS_SHIFT_JIS:	return Qshift_jis;
    case CODESYS_ISO2022:	return Qiso2022;
    case CODESYS_BIG5:		return Qbig5;
    case CODESYS_UCS4:		return Qucs4;
    case CODESYS_UTF8:		return Qutf8;
    case CODESYS_CCL:		return Qccl;
#endif
    case CODESYS_NO_CONVERSION:	return Qno_conversion;
#ifdef DEBUG_XEMACS
    case CODESYS_INTERNAL:	return Qinternal;
#endif
    }
}

#ifdef MULE
static
Lisp_Object coding_system_charset (Lisp_Object coding_system, int gnum)
{
  Lisp_Object cs
    = XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (coding_system, gnum);

  return CHARSETP (cs) ? XCHARSET_NAME (cs) : Qnil;
}

DEFUN ("coding-system-charset", Fcoding_system_charset, 2, 2, 0, /*
Return initial charset of CODING-SYSTEM designated to GNUM.
GNUM allows 0 .. 3.
*/
       (coding_system, gnum))
{
  coding_system = Fget_coding_system (coding_system);
  CHECK_INT (gnum);

  return coding_system_charset (coding_system, XINT (gnum));
}
#endif /* MULE */

DEFUN ("coding-system-property", Fcoding_system_property, 2, 2, 0, /*
Return the PROP property of CODING-SYSTEM.
*/
       (coding_system, prop))
{
  int i, ok = 0;
  enum coding_system_type type;

  coding_system = Fget_coding_system (coding_system);
  CHECK_SYMBOL (prop);
  type = XCODING_SYSTEM_TYPE (coding_system);

  for (i = 0; !ok && i < Dynarr_length (the_codesys_prop_dynarr); i++)
    if (EQ (Dynarr_at (the_codesys_prop_dynarr, i).sym, prop))
      {
	ok = 1;
	switch (Dynarr_at (the_codesys_prop_dynarr, i).prop_type)
	  {
	  case CODESYS_PROP_ALL_OK:
	    break;
#ifdef MULE
	  case CODESYS_PROP_ISO2022:
	    if (type != CODESYS_ISO2022)
	      signal_simple_error
		("Property only valid in ISO2022 coding systems",
		 prop);
	    break;

	  case CODESYS_PROP_CCL:
	    if (type != CODESYS_CCL)
	      signal_simple_error
		("Property only valid in CCL coding systems",
		 prop);
	    break;
#endif /* MULE */
	  default:
	    abort ();
	  }
      }

  if (!ok)
    signal_simple_error ("Unrecognized property", prop);

  if (EQ (prop, Qname))
    return XCODING_SYSTEM_NAME (coding_system);
  else if (EQ (prop, Qtype))
    return Fcoding_system_type (coding_system);
  else if (EQ (prop, Qdoc_string))
    return XCODING_SYSTEM_DOC_STRING (coding_system);
  else if (EQ (prop, Qmnemonic))
    return XCODING_SYSTEM_MNEMONIC (coding_system);
  else if (EQ (prop, Qeol_type))
    return eol_type_to_symbol (XCODING_SYSTEM_EOL_TYPE (coding_system));
  else if (EQ (prop, Qeol_lf))
    return XCODING_SYSTEM_EOL_LF (coding_system);
  else if (EQ (prop, Qeol_crlf))
    return XCODING_SYSTEM_EOL_CRLF (coding_system);
  else if (EQ (prop, Qeol_cr))
    return XCODING_SYSTEM_EOL_CR (coding_system);
  else if (EQ (prop, Qpost_read_conversion))
    return XCODING_SYSTEM_POST_READ_CONVERSION (coding_system);
  else if (EQ (prop, Qpre_write_conversion))
    return XCODING_SYSTEM_PRE_WRITE_CONVERSION (coding_system);
#ifdef MULE
  else if (type == CODESYS_ISO2022)
    {
      if (EQ (prop, Qcharset_g0))
	return coding_system_charset (coding_system, 0);
      else if (EQ (prop, Qcharset_g1))
	return coding_system_charset (coding_system, 1);
      else if (EQ (prop, Qcharset_g2))
	return coding_system_charset (coding_system, 2);
      else if (EQ (prop, Qcharset_g3))
	return coding_system_charset (coding_system, 3);

#define FORCE_CHARSET(charset_num) \
  (XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT \
   (coding_system, charset_num) ? Qt : Qnil)

      else if (EQ (prop, Qforce_g0_on_output)) return FORCE_CHARSET (0);
      else if (EQ (prop, Qforce_g1_on_output)) return FORCE_CHARSET (1);
      else if (EQ (prop, Qforce_g2_on_output)) return FORCE_CHARSET (2);
      else if (EQ (prop, Qforce_g3_on_output)) return FORCE_CHARSET (3);

#define LISP_BOOLEAN(prop) \
  (XCODING_SYSTEM_ISO2022_##prop (coding_system) ? Qt : Qnil)

      else if (EQ (prop, Qshort))         return LISP_BOOLEAN (SHORT);
      else if (EQ (prop, Qno_ascii_eol))  return LISP_BOOLEAN (NO_ASCII_EOL);
      else if (EQ (prop, Qno_ascii_cntl)) return LISP_BOOLEAN (NO_ASCII_CNTL);
      else if (EQ (prop, Qseven))         return LISP_BOOLEAN (SEVEN);
      else if (EQ (prop, Qlock_shift))    return LISP_BOOLEAN (LOCK_SHIFT);
      else if (EQ (prop, Qno_iso6429))    return LISP_BOOLEAN (NO_ISO6429);
      else if (EQ (prop, Qescape_quoted)) return LISP_BOOLEAN (ESCAPE_QUOTED);

      else if (EQ (prop, Qinput_charset_conversion))
	return
	  unparse_charset_conversion_specs
	    (XCODING_SYSTEM (coding_system)->iso2022.input_conv);
      else if (EQ (prop, Qoutput_charset_conversion))
	return
	  unparse_charset_conversion_specs
	    (XCODING_SYSTEM (coding_system)->iso2022.output_conv);
      else
	abort ();
    }
  else if (type == CODESYS_CCL)
    {
      if (EQ (prop, Qdecode))
	return XCODING_SYSTEM_CCL_DECODE (coding_system);
      else if (EQ (prop, Qencode))
	return XCODING_SYSTEM_CCL_ENCODE (coding_system);
      else
	abort ();
    }
#endif /* MULE */
  else
    abort ();

  return Qnil; /* not reached */
}


/************************************************************************/
/*                       Coding category functions                      */
/************************************************************************/

static int
decode_coding_category (Lisp_Object symbol)
{
  int i;

  CHECK_SYMBOL (symbol);
  for (i = 0; i < CODING_CATEGORY_LAST; i++)
    if (EQ (coding_category_symbol[i], symbol))
      return i;

  signal_simple_error ("Unrecognized coding category", symbol);
  return 0; /* not reached */
}

DEFUN ("coding-category-list", Fcoding_category_list, 0, 0, 0, /*
Return a list of all recognized coding categories.
*/
       ())
{
  int i;
  Lisp_Object list = Qnil;

  for (i = CODING_CATEGORY_LAST - 1; i >= 0; i--)
    list = Fcons (coding_category_symbol[i], list);
  return list;
}

DEFUN ("set-coding-priority-list", Fset_coding_priority_list, 1, 1, 0, /*
Change the priority order of the coding categories.
LIST should be list of coding categories, in descending order of
priority.  Unspecified coding categories will be lower in priority
than all specified ones, in the same relative order they were in
previously.
*/
       (list))
{
  int category_to_priority[CODING_CATEGORY_LAST];
  int i, j;
  Lisp_Object rest;

  /* First generate a list that maps coding categories to priorities. */

  for (i = 0; i < CODING_CATEGORY_LAST; i++)
    category_to_priority[i] = -1;

  /* Highest priority comes from the specified list. */
  i = 0;
  EXTERNAL_LIST_LOOP (rest, list)
    {
      int cat = decode_coding_category (XCAR (rest));

      if (category_to_priority[cat] >= 0)
	signal_simple_error ("Duplicate coding category in list", XCAR (rest));
      category_to_priority[cat] = i++;
    }

  /* Now go through the existing categories by priority to retrieve
     the categories not yet specified and preserve their priority
     order. */
  for (j = 0; j < CODING_CATEGORY_LAST; j++)
    {
      int cat = fcd->coding_category_by_priority[j];
      if (category_to_priority[cat] < 0)
	category_to_priority[cat] = i++;
    }

  /* Now we need to construct the inverse of the mapping we just
     constructed. */

  for (i = 0; i < CODING_CATEGORY_LAST; i++)
    fcd->coding_category_by_priority[category_to_priority[i]] = i;

  /* Phew!  That was confusing. */
  return Qnil;
}

DEFUN ("coding-priority-list", Fcoding_priority_list, 0, 0, 0, /*
Return a list of coding categories in descending order of priority.
*/
       ())
{
  int i;
  Lisp_Object list = Qnil;

  for (i = CODING_CATEGORY_LAST - 1; i >= 0; i--)
    list = Fcons (coding_category_symbol[fcd->coding_category_by_priority[i]],
		  list);
  return list;
}

DEFUN ("set-coding-category-system", Fset_coding_category_system, 2, 2, 0, /*
Change the coding system associated with a coding category.
*/
       (coding_category, coding_system))
{
  int cat = decode_coding_category (coding_category);

  coding_system = Fget_coding_system (coding_system);
  fcd->coding_category_system[cat] = coding_system;
  return Qnil;
}

DEFUN ("coding-category-system", Fcoding_category_system, 1, 1, 0, /*
Return the coding system associated with a coding category.
*/
       (coding_category))
{
  int cat = decode_coding_category (coding_category);
  Lisp_Object sys = fcd->coding_category_system[cat];

  if (!NILP (sys))
    return XCODING_SYSTEM_NAME (sys);
  return Qnil;
}


/************************************************************************/
/*                     Detecting the encoding of data                   */
/************************************************************************/

struct detection_state
{
  eol_type_t eol_type;
  int seen_non_ascii;
  int mask;
#ifdef MULE
  struct
    {
      int mask;
      int in_second_byte;
    }
  big5;

  struct
    {
      int mask;
      int in_second_byte;
    }
  shift_jis;

  struct
    {
      int mask;
      int in_byte;
  }
  ucs4;

  struct
    {
      int mask;
      int in_byte;
    }
  utf8;

  struct
    {
      int mask;
      int initted;
      struct iso2022_decoder iso;
      unsigned int flags;
      int high_byte_count;
      unsigned int saw_single_shift:1;
    }
  iso2022;
#endif
  struct
    {
      int seen_anything;
      int just_saw_cr;
    }
  eol;
};

static int
acceptable_control_char_p (int c)
{
  switch (c)
    {
      /* Allow and ignore control characters that you might
	 reasonably see in a text file */
    case '\r':
    case '\n':
    case '\t':
    case  7: /* bell */
    case  8: /* backspace */
    case 11: /* vertical tab */
    case 12: /* form feed */
    case 26: /* MS-DOS C-z junk */
    case 31: /* '^_' -- for info */
      return 1;
    default:
      return 0;
    }
}

static int
mask_has_at_most_one_bit_p (int mask)
{
  /* Perhaps the only thing useful you learn from intensive Microsoft
     technical interviews */
  return (mask & (mask - 1)) == 0;
}

static eol_type_t
detect_eol_type (struct detection_state *st, const Extbyte *src,
		 Lstream_data_count n)
{
  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      if (c == '\n')
	{
	  if (st->eol.just_saw_cr)
	    return EOL_CRLF;
	  else if (st->eol.seen_anything)
	    return EOL_LF;
	}
      else if (st->eol.just_saw_cr)
	return EOL_CR;
      else if (c == '\r')
	st->eol.just_saw_cr = 1;
      else
	st->eol.just_saw_cr = 0;
      st->eol.seen_anything = 1;
    }

  return EOL_AUTODETECT;
}

/* Attempt to determine the encoding and EOL type of the given text.
   Before calling this function for the first type, you must initialize
   st->eol_type as appropriate and initialize st->mask to ~0.

   st->eol_type holds the determined EOL type, or EOL_AUTODETECT if
   not yet known.

   st->mask holds the determined coding category mask, or ~0 if only
   ASCII has been seen so far.

   Returns:

   0 == st->eol_type is EOL_AUTODETECT and/or more than coding category
        is present in st->mask
   1 == definitive answers are here for both st->eol_type and st->mask
*/

static int
detect_coding_type (struct detection_state *st, const Extbyte *src,
		    Lstream_data_count n, int just_do_eol)
{
  if (st->eol_type == EOL_AUTODETECT)
    st->eol_type = detect_eol_type (st, src, n);

  if (just_do_eol)
    return st->eol_type != EOL_AUTODETECT;

  if (!st->seen_non_ascii)
    {
      for (; n; n--, src++)
	{
	  unsigned char c = *(unsigned char *) src;
	  if ((c < 0x20 && !acceptable_control_char_p (c)) || c >= 0x80)
	    {
	      st->seen_non_ascii = 1;
#ifdef MULE
	      st->shift_jis.mask = ~0;
	      st->big5.mask = ~0;
	      st->ucs4.mask = ~0;
	      st->utf8.mask = ~0;
	      st->iso2022.mask = ~0;
#endif
	      break;
	    }
	}
    }

  if (!n)
    return 0;
#ifdef MULE
  if (!mask_has_at_most_one_bit_p (st->iso2022.mask))
    st->iso2022.mask = detect_coding_iso2022 (st, src, n);
  if (!mask_has_at_most_one_bit_p (st->shift_jis.mask))
    st->shift_jis.mask = detect_coding_sjis (st, src, n);
  if (!mask_has_at_most_one_bit_p (st->big5.mask))
    st->big5.mask = detect_coding_big5 (st, src, n);
  if (!mask_has_at_most_one_bit_p (st->utf8.mask))
    st->utf8.mask = detect_coding_utf8 (st, src, n);
  if (!mask_has_at_most_one_bit_p (st->ucs4.mask))
    st->ucs4.mask = detect_coding_ucs4 (st, src, n);

  st->mask
    = st->iso2022.mask | st->shift_jis.mask | st->big5.mask
    | st->utf8.mask | st->ucs4.mask;
#endif
  {
    int retval = mask_has_at_most_one_bit_p (st->mask);
    st->mask |= CODING_CATEGORY_NO_CONVERSION_MASK;
    return retval && st->eol_type != EOL_AUTODETECT;
  }
}

static Lisp_Object
coding_system_from_mask (int mask)
{
  if (mask == ~0)
    {
      /* If the file was entirely or basically ASCII, use the
	 default value of `buffer-file-coding-system'. */
      Lisp_Object retval =
	XBUFFER (Vbuffer_defaults)->buffer_file_coding_system;
      if (!NILP (retval))
	{
	  retval = Ffind_coding_system (retval);
	  if (NILP (retval))
	    {
	      warn_when_safe
		(Qbad_variable, Qwarning,
		 "Invalid `default-buffer-file-coding-system', set to nil");
	      XBUFFER (Vbuffer_defaults)->buffer_file_coding_system = Qnil;
	    }
	}
      if (NILP (retval))
	retval = Fget_coding_system (Qraw_text);
      return retval;
    }
  else
    {
      int i;
      int cat = -1;
#ifdef MULE
      mask = postprocess_iso2022_mask (mask);
#endif
      /* Look through the coding categories by priority and find
	 the first one that is allowed. */
      for (i = 0; i < CODING_CATEGORY_LAST; i++)
	{
	  cat = fcd->coding_category_by_priority[i];
	  if ((mask & (1 << cat)) &&
	      !NILP (fcd->coding_category_system[cat]))
	    break;
	}
      if (cat >= 0)
	return fcd->coding_category_system[cat];
      else
	return Fget_coding_system (Qraw_text);
    }
}

/* Given a seekable read stream and potential coding system and EOL type
   as specified, do any autodetection that is called for.  If the
   coding system and/or EOL type are not `autodetect', they will be left
   alone; but this function will never return an autodetect coding system
   or EOL type.

   This function does not automatically fetch subsidiary coding systems;
   that should be unnecessary with the explicit eol-type argument. */

#define LENGTH(string_constant) (sizeof (string_constant) - 1)
/* number of leading lines to check for a coding cookie */
#define LINES_TO_CHECK 2

void
determine_real_coding_system (Lstream *stream, Lisp_Object *codesys_in_out,
			      eol_type_t *eol_type_in_out)
{
  struct detection_state decst;

  if (*eol_type_in_out == EOL_AUTODETECT)
    *eol_type_in_out = XCODING_SYSTEM_EOL_TYPE (*codesys_in_out);

  xzero (decst);
  decst.eol_type = *eol_type_in_out;
  decst.mask = ~0;

  /* If autodetection is called for, do it now. */
  if (XCODING_SYSTEM_TYPE (*codesys_in_out) == CODESYS_AUTODETECT
      || *eol_type_in_out == EOL_AUTODETECT)
    {
      Extbyte buf[4096];
      Lisp_Object coding_system = Qnil;
      Extbyte *p;
      Lstream_data_count nread = Lstream_read (stream, buf, sizeof (buf));
      Extbyte *scan_end;
      int lines_checked = 0;

      /* Look for initial "-*-"; mode line prefix */
      for (p = buf,
	     scan_end = buf + nread - LENGTH ("-*-coding:?-*-");
	   p <= scan_end
	     && lines_checked < LINES_TO_CHECK;
	   p++)
	if (*p == '-' && *(p+1) == '*' && *(p+2) == '-')
	  {
	    Extbyte *local_vars_beg = p + 3;
	    /* Look for final "-*-"; mode line suffix */
	    for (p = local_vars_beg,
		   scan_end = buf + nread - LENGTH ("-*-");
		 p <= scan_end
		   && lines_checked < LINES_TO_CHECK;
		 p++)
	      if (*p == '-' && *(p+1) == '*' && *(p+2) == '-')
		{
		  Extbyte *suffix = p;
		  /* Look for "coding:" */
		  for (p = local_vars_beg,
			 scan_end = suffix - LENGTH ("coding:?");
		       p <= scan_end;
		       p++)
		    if (memcmp ("coding:", p, LENGTH ("coding:")) == 0
			&& (p == local_vars_beg
			    || (*(p-1) == ' '  ||
				*(p-1) == '\t' ||
				*(p-1) == ';')))
		      {
			Extbyte save;
			int n;
			p += LENGTH ("coding:");
			while (*p == ' ' || *p == '\t') p++;

			/* Get coding system name */
			save = *suffix; *suffix = '\0';
			/* Characters valid in a MIME charset name (rfc 1521),
			   and in a Lisp symbol name. */
			n = strspn ( (char *) p,
				    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
				    "abcdefghijklmnopqrstuvwxyz"
				    "0123456789"
				    "!$%&*+-.^_{|}~");
			*suffix = save;
			if (n > 0)
			  {
			    save = p[n]; p[n] = '\0';
			    coding_system =
			      Ffind_coding_system (intern ((char *) p));
			    p[n] = save;
			  }
			break;
		      }
		  break;
		}
	      /* #### file must use standard EOLs or we miss 2d line */
	      /* #### not to mention this is broken for UTF-16 DOS files */
	      else if (*p == '\n' || *p == '\r')
		{
		  lines_checked++;
		  /* skip past multibyte (DOS) newline */
		  if (*p == '\r' && *(p+1) == '\n') p++;
		}
	    break;
	  }
	/* #### file must use standard EOLs or we miss 2d line */
	/* #### not to mention this is broken for UTF-16 DOS files */
	else if (*p == '\n' || *p == '\r')
	  {
	    lines_checked++;
	    /* skip past multibyte (DOS) newline */
	    if (*p == '\r' && *(p+1) == '\n') p++;
	  }

      if (NILP (coding_system))
	do
	  {
	    if (detect_coding_type (&decst, buf, nread,
				    XCODING_SYSTEM_TYPE (*codesys_in_out)
				    != CODESYS_AUTODETECT))
	      break;
	    nread = Lstream_read (stream, buf, sizeof (buf));
	    if (nread == 0)
	      break;
	  }
	while (1);

      else if (XCODING_SYSTEM_TYPE (*codesys_in_out) == CODESYS_AUTODETECT
	       && XCODING_SYSTEM_EOL_TYPE (coding_system) == EOL_AUTODETECT)
	do
	  {
	    if (detect_coding_type (&decst, buf, nread, 1))
	      break;
	    nread = Lstream_read (stream, buf, sizeof (buf));
	    if (!nread)
	      break;
	  }
	while (1);

      *eol_type_in_out = decst.eol_type;
      if (XCODING_SYSTEM_TYPE (*codesys_in_out) == CODESYS_AUTODETECT)
	{
	  if (NILP (coding_system))
	    *codesys_in_out = coding_system_from_mask (decst.mask);
	  else
	    *codesys_in_out = coding_system;
	}
    }

  /* If we absolutely can't determine the EOL type, just assume LF. */
  if (*eol_type_in_out == EOL_AUTODETECT)
    *eol_type_in_out = EOL_LF;

  Lstream_rewind (stream);
}

DEFUN ("detect-coding-region", Fdetect_coding_region, 2, 3, 0, /*
Detect coding system of the text in the region between START and END.
Return a list of possible coding systems ordered by priority.
If only ASCII characters are found, return 'undecided or one of
its subsidiary coding systems according to a detected end-of-line
type.  Optional arg BUFFER defaults to the current buffer.
*/
       (start, end, buffer))
{
  Lisp_Object val = Qnil;
  struct buffer *buf = decode_buffer (buffer, 0);
  Bufpos b, e;
  Lisp_Object instream, lb_instream;
  Lstream *istr, *lb_istr;
  struct detection_state decst;
  struct gcpro gcpro1, gcpro2;

  get_buffer_range_char (buf, start, end, &b, &e, 0);
  lb_instream = make_lisp_buffer_input_stream (buf, b, e, 0);
  lb_istr = XLSTREAM (lb_instream);
  instream = make_encoding_input_stream (lb_istr, Fget_coding_system (Qbinary));
  istr = XLSTREAM (instream);
  GCPRO2 (instream, lb_instream);
  xzero (decst);
  decst.eol_type = EOL_AUTODETECT;
  decst.mask = ~0;
  while (1)
    {
      Extbyte random_buffer[4096];
      Lstream_data_count nread = Lstream_read (istr, random_buffer, sizeof (random_buffer));

      if (!nread)
	break;
      if (detect_coding_type (&decst, random_buffer, nread, 0))
	break;
    }

  if (decst.mask == ~0)
    val = subsidiary_coding_system (Fget_coding_system (Qundecided),
				    decst.eol_type);
  else
    {
      int i;

      val = Qnil;
#ifdef MULE
      decst.mask = postprocess_iso2022_mask (decst.mask);
#endif
      for (i = CODING_CATEGORY_LAST - 1; i >= 0; i--)
	{
	  int sys = fcd->coding_category_by_priority[i];
	  if (decst.mask & (1 << sys))
	    {
	      Lisp_Object codesys = fcd->coding_category_system[sys];
	      if (!NILP (codesys))
		codesys = subsidiary_coding_system (codesys, decst.eol_type);
	      val = Fcons (codesys, val);
	    }
	}
    }
  Lstream_close (istr);
  UNGCPRO;
  Lstream_delete (istr);
  Lstream_delete (lb_istr);
  return val;
}


/************************************************************************/
/*           Converting to internal Mule format ("decoding")            */
/************************************************************************/

/* A decoding stream is a stream used for decoding text (i.e.
   converting from some external format to internal format).
   The decoding-stream object keeps track of the actual coding
   stream, the stream that is at the other end, and data that
   needs to be persistent across the lifetime of the stream. */

/* Handle the EOL stuff related to just-read-in character C.
   EOL_TYPE is the EOL type of the coding stream.
   FLAGS is the current value of FLAGS in the coding stream, and may
   be modified by this macro.  (The macro only looks at the
   CODING_STATE_CR flag.)  DST is the Dynarr to which the decoded
   bytes are to be written.  You need to also define a local goto
   label "label_continue_loop" that is at the end of the main
   character-reading loop.

   If C is a CR character, then this macro handles it entirely and
   jumps to label_continue_loop.  Otherwise, this macro does not add
   anything to DST, and continues normally.  You should continue
   processing C normally after this macro. */

#define DECODE_HANDLE_EOL_TYPE(eol_type, c, flags, dst)		\
do {								\
  if (c == '\r')						\
    {								\
      if (eol_type == EOL_CR)					\
	Dynarr_add (dst, '\n');					\
      else if (eol_type != EOL_CRLF || flags & CODING_STATE_CR)	\
	Dynarr_add (dst, c);					\
      else							\
	flags |= CODING_STATE_CR;				\
      goto label_continue_loop;					\
    }								\
  else if (flags & CODING_STATE_CR)				\
    {	/* eol_type == CODING_SYSTEM_EOL_CRLF */		\
      if (c != '\n')						\
	Dynarr_add (dst, '\r');					\
      flags &= ~CODING_STATE_CR;				\
    }								\
} while (0)

/* C should be a binary character in the range 0 - 255; convert
   to internal format and add to Dynarr DST. */

#define DECODE_ADD_BINARY_CHAR(c, dst)		\
do {						\
  if (BYTE_ASCII_P (c))				\
    Dynarr_add (dst, c);			\
  else if (BYTE_C1_P (c))			\
    {						\
      Dynarr_add (dst, LEADING_BYTE_CONTROL_1);	\
      Dynarr_add (dst, c + 0x20);		\
    }						\
  else						\
    {						\
      Dynarr_add (dst, LEADING_BYTE_LATIN_ISO8859_1); \
      Dynarr_add (dst, c);			\
    }						\
} while (0)

#define DECODE_OUTPUT_PARTIAL_CHAR(ch)	\
do {					\
  if (ch)				\
    {					\
      DECODE_ADD_BINARY_CHAR (ch, dst);	\
      ch = 0;				\
    }					\
} while (0)

#define DECODE_HANDLE_END_OF_CONVERSION(flags, ch, dst)	\
do {					\
  if (flags & CODING_STATE_END)		\
    {					\
      DECODE_OUTPUT_PARTIAL_CHAR (ch);	\
      if (flags & CODING_STATE_CR)	\
	Dynarr_add (dst, '\r');		\
    }					\
} while (0)

#define DECODING_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, decoding)

struct decoding_stream
{
  /* Coding system that governs the conversion. */
  Lisp_Coding_System *codesys;

  /* Stream that we read the encoded data from or
     write the decoded data to. */
  Lstream *other_end;

  /* If we are reading, then we can return only a fixed amount of
     data, so if the conversion resulted in too much data, we store it
     here for retrieval the next time around. */
  unsigned_char_dynarr *runoff;

  /* FLAGS holds flags indicating the current state of the decoding.
     Some of these flags are dependent on the coding system. */
  unsigned int flags;

  /* CH holds a partially built-up character.  Since we only deal
     with one- and two-byte characters at the moment, we only use
     this to store the first byte of a two-byte character. */
  unsigned int ch;

  /* EOL_TYPE specifies the type of end-of-line conversion that
     currently applies.  We need to keep this separate from the
     EOL type stored in CODESYS because the latter might indicate
     automatic EOL-type detection while the former will always
     indicate a particular EOL type. */
  eol_type_t eol_type;
#ifdef MULE
  /* Additional ISO2022 information.  We define the structure above
     because it's also needed by the detection routines. */
  struct iso2022_decoder iso2022;

  /* Additional information (the state of the running CCL program)
     used by the CCL decoder. */
  struct ccl_program ccl;

  /* counter for UTF-8 or UCS-4 */
  unsigned char counter;
#endif
  struct detection_state decst;
};

static Lstream_data_count decoding_reader (Lstream *stream,
				unsigned char *data, Lstream_data_count size);
static Lstream_data_count decoding_writer (Lstream *stream,
				const unsigned char *data, Lstream_data_count size);
static int decoding_rewinder   (Lstream *stream);
static int decoding_seekable_p (Lstream *stream);
static int decoding_flusher    (Lstream *stream);
static int decoding_closer     (Lstream *stream);

static Lisp_Object decoding_marker (Lisp_Object stream);

DEFINE_LSTREAM_IMPLEMENTATION ("decoding", lstream_decoding,
			       sizeof (struct decoding_stream));

static Lisp_Object
decoding_marker (Lisp_Object stream)
{
  Lstream *str = DECODING_STREAM_DATA (XLSTREAM (stream))->other_end;
  Lisp_Object str_obj;

  /* We do not need to mark the coding systems or charsets stored
     within the stream because they are stored in a global list
     and automatically marked. */

  XSETLSTREAM (str_obj, str);
  mark_object (str_obj);
  if (str->imp->marker)
    return (str->imp->marker) (str_obj);
  else
    return Qnil;
}

/* Read SIZE bytes of data and store it into DATA.  We are a decoding stream
   so we read data from the other end, decode it, and store it into DATA. */

static Lstream_data_count
decoding_reader (Lstream *stream, unsigned char *data, Lstream_data_count size)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);
  unsigned char *orig_data = data;
  Lstream_data_count read_size;
  int error_occurred = 0;

  /* We need to interface to mule_decode(), which expects to take some
     amount of data and store the result into a Dynarr.  We have
     mule_decode() store into str->runoff, and take data from there
     as necessary. */

  /* We loop until we have enough data, reading chunks from the other
     end and decoding it. */
  while (1)
    {
      /* Take data from the runoff if we can.  Make sure to take at
	 most SIZE bytes, and delete the data from the runoff. */
      if (Dynarr_length (str->runoff) > 0)
	{
	  Lstream_data_count chunk = min (size, (Lstream_data_count) Dynarr_length (str->runoff));
	  memcpy (data, Dynarr_atp (str->runoff, 0), chunk);
	  Dynarr_delete_many (str->runoff, 0, chunk);
	  data += chunk;
	  size -= chunk;
	}

      if (size == 0)
	break; /* No more room for data */

      if (str->flags & CODING_STATE_END)
	/* This means that on the previous iteration, we hit the EOF on
	   the other end.  We loop once more so that mule_decode() can
	   output any final stuff it may be holding, or any "go back
	   to a sane state" escape sequences. (This latter makes sense
	   during encoding.) */
	break;

      /* Exhausted the runoff, so get some more.  DATA has at least
	 SIZE bytes left of storage in it, so it's OK to read directly
	 into it.  (We'll be overwriting above, after we've decoded it
	 into the runoff.) */
      read_size = Lstream_read (str->other_end, data, size);
      if (read_size < 0)
	{
	  error_occurred = 1;
	  break;
	}
      if (read_size == 0)
	/* There might be some more end data produced in the translation.
	   See the comment above. */
	str->flags |= CODING_STATE_END;
      mule_decode (stream, (Extbyte *) data, str->runoff, read_size);
    }

  if (data - orig_data == 0)
    return error_occurred ? -1 : 0;
  else
    return data - orig_data;
}

static Lstream_data_count
decoding_writer (Lstream *stream, const unsigned char *data, Lstream_data_count size)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);
  Lstream_data_count retval;

  /* Decode all our data into the runoff, and then attempt to write
     it all out to the other end.  Remove whatever chunk we succeeded
     in writing. */
  mule_decode (stream, (Extbyte *) data, str->runoff, size);
  retval = Lstream_write (str->other_end, Dynarr_atp (str->runoff, 0),
			  Dynarr_length (str->runoff));
  if (retval > 0)
    Dynarr_delete_many (str->runoff, 0, retval);
  /* Do NOT return retval.  The return value indicates how much
     of the incoming data was written, not how many bytes were
     written. */
  return size;
}

static void
reset_decoding_stream (struct decoding_stream *str)
{
#ifdef MULE
  if (CODING_SYSTEM_TYPE (str->codesys) == CODESYS_ISO2022)
    {
      Lisp_Object coding_system;
      XSETCODING_SYSTEM (coding_system, str->codesys);
      reset_iso2022 (coding_system, &str->iso2022);
    }
  else if (CODING_SYSTEM_TYPE (str->codesys) == CODESYS_CCL)
    {
      setup_ccl_program (&str->ccl, CODING_SYSTEM_CCL_DECODE (str->codesys));
    }
  str->counter = 0;
#endif /* MULE */
  if (CODING_SYSTEM_TYPE (str->codesys) == CODESYS_AUTODETECT
      || CODING_SYSTEM_EOL_TYPE (str->codesys) == EOL_AUTODETECT)
    {
      xzero (str->decst);
      str->decst.eol_type = EOL_AUTODETECT;
      str->decst.mask = ~0;
    }
  str->flags = str->ch = 0;
}

static int
decoding_rewinder (Lstream *stream)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);
  reset_decoding_stream (str);
  Dynarr_reset (str->runoff);
  return Lstream_rewind (str->other_end);
}

static int
decoding_seekable_p (Lstream *stream)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);
  return Lstream_seekable_p (str->other_end);
}

static int
decoding_flusher (Lstream *stream)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);
  return Lstream_flush (str->other_end);
}

static int
decoding_closer (Lstream *stream)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);
  if (stream->flags & LSTREAM_FL_WRITE)
    {
      str->flags |= CODING_STATE_END;
      decoding_writer (stream, 0, 0);
    }
  Dynarr_free (str->runoff);
#ifdef MULE
#ifdef ENABLE_COMPOSITE_CHARS
  if (str->iso2022.composite_chars)
    Dynarr_free (str->iso2022.composite_chars);
#endif
#endif
  return Lstream_close (str->other_end);
}

Lisp_Object
decoding_stream_coding_system (Lstream *stream)
{
  Lisp_Object coding_system;
  struct decoding_stream *str = DECODING_STREAM_DATA (stream);

  XSETCODING_SYSTEM (coding_system, str->codesys);
  return subsidiary_coding_system (coding_system, str->eol_type);
}

void
set_decoding_stream_coding_system (Lstream *lstr, Lisp_Object codesys)
{
  Lisp_Coding_System *cs = XCODING_SYSTEM (codesys);
  struct decoding_stream *str = DECODING_STREAM_DATA (lstr);
  str->codesys = cs;
  if (CODING_SYSTEM_EOL_TYPE (cs) != EOL_AUTODETECT)
    str->eol_type = CODING_SYSTEM_EOL_TYPE (cs);
  reset_decoding_stream (str);
}

/* WARNING WARNING WARNING WARNING!!!!!  If you open up a decoding
   stream for writing, no automatic code detection will be performed.
   The reason for this is that automatic code detection requires a
   seekable input.  Things will also fail if you open a decoding
   stream for reading using a non-fully-specified coding system and
   a non-seekable input stream. */

static Lisp_Object
make_decoding_stream_1 (Lstream *stream, Lisp_Object codesys,
			const char *mode)
{
  Lstream *lstr = Lstream_new (lstream_decoding, mode);
  struct decoding_stream *str = DECODING_STREAM_DATA (lstr);
  Lisp_Object obj;

  xzero (*str);
  str->other_end = stream;
  str->runoff = (unsigned_char_dynarr *) Dynarr_new (unsigned_char);
  str->eol_type = EOL_AUTODETECT;
  if (!strcmp (mode, "r")
      && Lstream_seekable_p (stream))
    /* We can determine the coding system now. */
    determine_real_coding_system (stream, &codesys, &str->eol_type);
  set_decoding_stream_coding_system (lstr, codesys);
  str->decst.eol_type = str->eol_type;
  str->decst.mask = ~0;
  XSETLSTREAM (obj, lstr);
  return obj;
}

Lisp_Object
make_decoding_input_stream (Lstream *stream, Lisp_Object codesys)
{
  return make_decoding_stream_1 (stream, codesys, "r");
}

Lisp_Object
make_decoding_output_stream (Lstream *stream, Lisp_Object codesys)
{
  return make_decoding_stream_1 (stream, codesys, "w");
}

/* Note: the decode_coding_* functions all take the same
   arguments as mule_decode(), which is to say some SRC data of
   size N, which is to be stored into dynamic array DST.
   DECODING is the stream within which the decoding is
   taking place, but no data is actually read from or
   written to that stream; that is handled in decoding_reader()
   or decoding_writer().  This allows the same functions to
   be used for both reading and writing. */

static void
mule_decode (Lstream *decoding, const Extbyte *src,
	     unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);

  /* If necessary, do encoding-detection now.  We do this when
     we're a writing stream or a non-seekable reading stream,
     meaning that we can't just process the whole input,
     rewind, and start over. */

  if (CODING_SYSTEM_TYPE (str->codesys) == CODESYS_AUTODETECT ||
      str->eol_type == EOL_AUTODETECT)
    {
      Lisp_Object codesys;

      XSETCODING_SYSTEM (codesys, str->codesys);
      detect_coding_type (&str->decst, src, n,
			  CODING_SYSTEM_TYPE (str->codesys) !=
			  CODESYS_AUTODETECT);
      if (CODING_SYSTEM_TYPE (str->codesys) == CODESYS_AUTODETECT &&
	  str->decst.mask != ~0)
	/* #### This is cheesy.  What we really ought to do is
	   buffer up a certain amount of data so as to get a
	   less random result. */
	codesys = coding_system_from_mask (str->decst.mask);
      str->eol_type = str->decst.eol_type;
      if (XCODING_SYSTEM (codesys) != str->codesys)
	{
	  /* Preserve the CODING_STATE_END flag in case it was set.
	     If we erase it, bad things might happen. */
	  int was_end = str->flags & CODING_STATE_END;
          set_decoding_stream_coding_system (decoding, codesys);
	  if (was_end)
	    str->flags |= CODING_STATE_END;
	}
    }

  switch (CODING_SYSTEM_TYPE (str->codesys))
    {
#ifdef DEBUG_XEMACS
    case CODESYS_INTERNAL:
      Dynarr_add_many (dst, src, n);
      break;
#endif
    case CODESYS_AUTODETECT:
      /* If we got this far and still haven't decided on the coding
	 system, then do no conversion. */
    case CODESYS_NO_CONVERSION:
      decode_coding_no_conversion (decoding, src, dst, n);
      break;
#ifdef MULE
    case CODESYS_SHIFT_JIS:
      decode_coding_sjis (decoding, src, dst, n);
      break;
    case CODESYS_BIG5:
      decode_coding_big5 (decoding, src, dst, n);
      break;
    case CODESYS_UCS4:
      decode_coding_ucs4 (decoding, src, dst, n);
      break;
    case CODESYS_UTF8:
      decode_coding_utf8 (decoding, src, dst, n);
      break;
    case CODESYS_CCL:
      str->ccl.last_block = str->flags & CODING_STATE_END;
      /* When applying ccl program to stream, MUST NOT set NULL
	 pointer to src.  */
      ccl_driver (&str->ccl, (src ? (unsigned char *)src : (unsigned char*)""),
		  dst, n, 0, CCL_MODE_DECODING);
      break;
    case CODESYS_ISO2022:
      decode_coding_iso2022 (decoding, src, dst, n);
      break;
#endif /* MULE */
    default:
      abort ();
    }
}

DEFUN ("decode-coding-region", Fdecode_coding_region, 3, 4, 0, /*
Decode the text between START and END which is encoded in CODING-SYSTEM.
This is useful if you've read in encoded text from a file without decoding
it (e.g. you read in a JIS-formatted file but used the `binary' or
`no-conversion' coding system, so that it shows up as "^[$B!<!+^[(B").
Return length of decoded text.
BUFFER defaults to the current buffer if unspecified.
*/
       (start, end, coding_system, buffer))
{
  Bufpos b, e;
  struct buffer *buf = decode_buffer (buffer, 0);
  Lisp_Object instream, lb_outstream, de_outstream, outstream;
  Lstream *istr, *ostr;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  get_buffer_range_char (buf, start, end, &b, &e, 0);

  barf_if_buffer_read_only (buf, b, e);

  coding_system = Fget_coding_system (coding_system);
  instream = make_lisp_buffer_input_stream  (buf, b, e, 0);
  lb_outstream = make_lisp_buffer_output_stream (buf, b, 0);
  de_outstream = make_decoding_output_stream (XLSTREAM (lb_outstream),
					      coding_system);
  outstream = make_encoding_output_stream (XLSTREAM (de_outstream),
					   Fget_coding_system (Qbinary));
  istr = XLSTREAM (instream);
  ostr = XLSTREAM (outstream);
  GCPRO4 (instream, lb_outstream, de_outstream, outstream);

  /* The chain of streams looks like this:

     [BUFFER] <----- send through
                     ------> [ENCODE AS BINARY]
		             ------> [DECODE AS SPECIFIED]
			             ------> [BUFFER]
   */

  while (1)
    {
      char tempbuf[1024]; /* some random amount */
      Bufpos newpos, even_newer_pos;
      Bufpos oldpos = lisp_buffer_stream_startpos (istr);
      Lstream_data_count size_in_bytes = Lstream_read (istr, tempbuf, sizeof (tempbuf));

      if (!size_in_bytes)
	break;
      newpos = lisp_buffer_stream_startpos (istr);
      Lstream_write (ostr, tempbuf, size_in_bytes);
      even_newer_pos = lisp_buffer_stream_startpos (istr);
      buffer_delete_range (buf, even_newer_pos - (newpos - oldpos),
			   even_newer_pos, 0);
    }
  Lstream_close (istr);
  Lstream_close (ostr);
  UNGCPRO;
  Lstream_delete (istr);
  Lstream_delete (ostr);
  Lstream_delete (XLSTREAM (de_outstream));
  Lstream_delete (XLSTREAM (lb_outstream));
  return Qnil;
}


/************************************************************************/
/*           Converting to an external encoding ("encoding")            */
/************************************************************************/

/* An encoding stream is an output stream.  When you create the
   stream, you specify the coding system that governs the encoding
   and another stream that the resulting encoded data is to be
   sent to, and then start sending data to it. */

#define ENCODING_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, encoding)

struct encoding_stream
{
  /* Coding system that governs the conversion. */
  Lisp_Coding_System *codesys;

  /* Stream that we read the encoded data from or
     write the decoded data to. */
  Lstream *other_end;

  /* If we are reading, then we can return only a fixed amount of
     data, so if the conversion resulted in too much data, we store it
     here for retrieval the next time around. */
  unsigned_char_dynarr *runoff;

  /* FLAGS holds flags indicating the current state of the encoding.
     Some of these flags are dependent on the coding system. */
  unsigned int flags;

  /* CH holds a partially built-up character.  Since we only deal
     with one- and two-byte characters at the moment, we only use
     this to store the first byte of a two-byte character. */
  unsigned int ch;
#ifdef MULE
  /* Additional information used by the ISO2022 encoder. */
  struct
    {
      /* CHARSET holds the character sets currently assigned to the G0
	 through G3 registers.  It is initialized from the array
	 INITIAL_CHARSET in CODESYS. */
      Lisp_Object charset[4];

      /* Which registers are currently invoked into the left (GL) and
	 right (GR) halves of the 8-bit encoding space? */
      int register_left, register_right;

      /* Whether we need to explicitly designate the charset in the
	 G? register before using it.  It is initialized from the
	 array FORCE_CHARSET_ON_OUTPUT in CODESYS. */
      unsigned char force_charset_on_output[4];

      /* Other state variables that need to be preserved across
	 invocations. */
      Lisp_Object current_charset;
      int current_half;
      int current_char_boundary;
    } iso2022;

  /* Additional information (the state of the running CCL program)
     used by the CCL encoder. */
  struct ccl_program ccl;
#endif /* MULE */
};

static Lstream_data_count encoding_reader (Lstream *stream, unsigned char *data, Lstream_data_count size);
static Lstream_data_count encoding_writer (Lstream *stream, const unsigned char *data,
				Lstream_data_count size);
static int encoding_rewinder   (Lstream *stream);
static int encoding_seekable_p (Lstream *stream);
static int encoding_flusher    (Lstream *stream);
static int encoding_closer     (Lstream *stream);

static Lisp_Object encoding_marker (Lisp_Object stream);

DEFINE_LSTREAM_IMPLEMENTATION ("encoding", lstream_encoding,
			       sizeof (struct encoding_stream));

static Lisp_Object
encoding_marker (Lisp_Object stream)
{
  Lstream *str = ENCODING_STREAM_DATA (XLSTREAM (stream))->other_end;
  Lisp_Object str_obj;

  /* We do not need to mark the coding systems or charsets stored
     within the stream because they are stored in a global list
     and automatically marked. */

  XSETLSTREAM (str_obj, str);
  mark_object (str_obj);
  if (str->imp->marker)
    return (str->imp->marker) (str_obj);
  else
    return Qnil;
}

/* Read SIZE bytes of data and store it into DATA.  We are a encoding stream
   so we read data from the other end, encode it, and store it into DATA. */

static Lstream_data_count
encoding_reader (Lstream *stream, unsigned char *data, Lstream_data_count size)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);
  unsigned char *orig_data = data;
  Lstream_data_count read_size;
  int error_occurred = 0;

  /* We need to interface to mule_encode(), which expects to take some
     amount of data and store the result into a Dynarr.  We have
     mule_encode() store into str->runoff, and take data from there
     as necessary. */

  /* We loop until we have enough data, reading chunks from the other
     end and encoding it. */
  while (1)
    {
      /* Take data from the runoff if we can.  Make sure to take at
	 most SIZE bytes, and delete the data from the runoff. */
      if (Dynarr_length (str->runoff) > 0)
	{
	  int chunk = min ((int) size, Dynarr_length (str->runoff));
	  memcpy (data, Dynarr_atp (str->runoff, 0), chunk);
	  Dynarr_delete_many (str->runoff, 0, chunk);
	  data += chunk;
	  size -= chunk;
	}

      if (size == 0)
	break; /* No more room for data */

      if (str->flags & CODING_STATE_END)
	/* This means that on the previous iteration, we hit the EOF on
	   the other end.  We loop once more so that mule_encode() can
	   output any final stuff it may be holding, or any "go back
	   to a sane state" escape sequences. (This latter makes sense
	   during encoding.) */
	break;

      /* Exhausted the runoff, so get some more.  DATA at least SIZE bytes
	 left of storage in it, so it's OK to read directly into it.
	 (We'll be overwriting above, after we've encoded it into the
	 runoff.) */
      read_size = Lstream_read (str->other_end, data, size);
      if (read_size < 0)
	{
	  error_occurred = 1;
	  break;
	}
      if (read_size == 0)
	/* There might be some more end data produced in the translation.
	   See the comment above. */
	str->flags |= CODING_STATE_END;
      mule_encode (stream, data, str->runoff, read_size);
    }

  if (data == orig_data)
    return error_occurred ? -1 : 0;
  else
    return data - orig_data;
}

static Lstream_data_count
encoding_writer (Lstream *stream, const unsigned char *data, Lstream_data_count size)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);
  Lstream_data_count retval;

  /* Encode all our data into the runoff, and then attempt to write
     it all out to the other end.  Remove whatever chunk we succeeded
     in writing. */
  mule_encode (stream, data, str->runoff, size);
  retval = Lstream_write (str->other_end, Dynarr_atp (str->runoff, 0),
			  Dynarr_length (str->runoff));
  if (retval > 0)
    Dynarr_delete_many (str->runoff, 0, retval);
  /* Do NOT return retval.  The return value indicates how much
     of the incoming data was written, not how many bytes were
     written. */
  return size;
}

static void
reset_encoding_stream (struct encoding_stream *str)
{
#ifdef MULE
  switch (CODING_SYSTEM_TYPE (str->codesys))
    {
    case CODESYS_ISO2022:
      {
	int i;

	for (i = 0; i < 4; i++)
	  {
	    str->iso2022.charset[i] =
	      CODING_SYSTEM_ISO2022_INITIAL_CHARSET (str->codesys, i);
	    str->iso2022.force_charset_on_output[i] =
	      CODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (str->codesys, i);
	  }
	str->iso2022.register_left = 0;
	str->iso2022.register_right = 1;
	str->iso2022.current_charset = Qnil;
	str->iso2022.current_half = 0;
	str->iso2022.current_char_boundary = 1;
	break;
      }
    case CODESYS_CCL:
      setup_ccl_program (&str->ccl, CODING_SYSTEM_CCL_ENCODE (str->codesys));
      break;
    default:
      break;
    }
#endif /* MULE */

  str->flags = str->ch = 0;
}

static int
encoding_rewinder (Lstream *stream)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);
  reset_encoding_stream (str);
  Dynarr_reset (str->runoff);
  return Lstream_rewind (str->other_end);
}

static int
encoding_seekable_p (Lstream *stream)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);
  return Lstream_seekable_p (str->other_end);
}

static int
encoding_flusher (Lstream *stream)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);
  return Lstream_flush (str->other_end);
}

static int
encoding_closer (Lstream *stream)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);
  if (stream->flags & LSTREAM_FL_WRITE)
    {
      str->flags |= CODING_STATE_END;
      encoding_writer (stream, 0, 0);
    }
  Dynarr_free (str->runoff);
  return Lstream_close (str->other_end);
}

Lisp_Object
encoding_stream_coding_system (Lstream *stream)
{
  Lisp_Object coding_system;
  struct encoding_stream *str = ENCODING_STREAM_DATA (stream);

  XSETCODING_SYSTEM (coding_system, str->codesys);
  return coding_system;
}

void
set_encoding_stream_coding_system (Lstream *lstr, Lisp_Object codesys)
{
  Lisp_Coding_System *cs = XCODING_SYSTEM (codesys);
  struct encoding_stream *str = ENCODING_STREAM_DATA (lstr);
  str->codesys = cs;
  reset_encoding_stream (str);
}

static Lisp_Object
make_encoding_stream_1 (Lstream *stream, Lisp_Object codesys,
			const char *mode)
{
  Lstream *lstr = Lstream_new (lstream_encoding, mode);
  struct encoding_stream *str = ENCODING_STREAM_DATA (lstr);
  Lisp_Object obj;

  xzero (*str);
  str->runoff = Dynarr_new (unsigned_char);
  str->other_end = stream;
  set_encoding_stream_coding_system (lstr, codesys);
  XSETLSTREAM (obj, lstr);
  return obj;
}

Lisp_Object
make_encoding_input_stream (Lstream *stream, Lisp_Object codesys)
{
  return make_encoding_stream_1 (stream, codesys, "r");
}

Lisp_Object
make_encoding_output_stream (Lstream *stream, Lisp_Object codesys)
{
  return make_encoding_stream_1 (stream, codesys, "w");
}

/* Convert N bytes of internally-formatted data stored in SRC to an
   external format, according to the encoding stream ENCODING.
   Store the encoded data into DST. */

static void
mule_encode (Lstream *encoding, const Bufbyte *src,
	     unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);

  switch (CODING_SYSTEM_TYPE (str->codesys))
    {
#ifdef DEBUG_XEMACS
    case CODESYS_INTERNAL:
      Dynarr_add_many (dst, src, n);
      break;
#endif
    case CODESYS_AUTODETECT:
      /* If we got this far and still haven't decided on the coding
	 system, then do no conversion. */
    case CODESYS_NO_CONVERSION:
      encode_coding_no_conversion (encoding, src, dst, n);
      break;
#ifdef MULE
    case CODESYS_SHIFT_JIS:
      encode_coding_sjis (encoding, src, dst, n);
      break;
    case CODESYS_BIG5:
      encode_coding_big5 (encoding, src, dst, n);
      break;
    case CODESYS_UCS4:
      encode_coding_ucs4 (encoding, src, dst, n);
      break;
    case CODESYS_UTF8:
      encode_coding_utf8 (encoding, src, dst, n);
      break;
    case CODESYS_CCL:
      str->ccl.last_block = str->flags & CODING_STATE_END;
      /* When applying ccl program to stream, MUST NOT set NULL
	 pointer to src.  */
      ccl_driver (&str->ccl, ((src) ? src : (unsigned char*)""),
		  dst, n, 0, CCL_MODE_ENCODING);
      break;
    case CODESYS_ISO2022:
      encode_coding_iso2022 (encoding, src, dst, n);
      break;
#endif /* MULE */
    default:
      abort ();
    }
}

DEFUN ("encode-coding-region", Fencode_coding_region, 3, 4, 0, /*
Encode the text between START and END using CODING-SYSTEM.
This will, for example, convert Japanese characters into stuff such as
"^[$B!<!+^[(B" if you use the JIS encoding.  Return length of encoded
text.  BUFFER defaults to the current buffer if unspecified.
*/
       (start, end, coding_system, buffer))
{
  Bufpos b, e;
  struct buffer *buf = decode_buffer (buffer, 0);
  Lisp_Object instream, lb_outstream, de_outstream, outstream;
  Lstream *istr, *ostr;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  get_buffer_range_char (buf, start, end, &b, &e, 0);

  barf_if_buffer_read_only (buf, b, e);

  coding_system = Fget_coding_system (coding_system);
  instream  = make_lisp_buffer_input_stream  (buf, b, e, 0);
  lb_outstream = make_lisp_buffer_output_stream (buf, b, 0);
  de_outstream = make_decoding_output_stream (XLSTREAM (lb_outstream),
					      Fget_coding_system (Qbinary));
  outstream = make_encoding_output_stream (XLSTREAM (de_outstream),
					   coding_system);
  istr = XLSTREAM (instream);
  ostr = XLSTREAM (outstream);
  GCPRO4 (instream, outstream, de_outstream, lb_outstream);
  /* The chain of streams looks like this:

     [BUFFER] <----- send through
                     ------> [ENCODE AS SPECIFIED]
		             ------> [DECODE AS BINARY]
			             ------> [BUFFER]
   */
  while (1)
    {
      char tempbuf[1024]; /* some random amount */
      Bufpos newpos, even_newer_pos;
      Bufpos oldpos = lisp_buffer_stream_startpos (istr);
      Lstream_data_count size_in_bytes = Lstream_read (istr, tempbuf, sizeof (tempbuf));

      if (!size_in_bytes)
	break;
      newpos = lisp_buffer_stream_startpos (istr);
      Lstream_write (ostr, tempbuf, size_in_bytes);
      even_newer_pos = lisp_buffer_stream_startpos (istr);
      buffer_delete_range (buf, even_newer_pos - (newpos - oldpos),
			   even_newer_pos, 0);
    }

  {
    Charcount retlen =
      lisp_buffer_stream_startpos (XLSTREAM (instream)) - b;
    Lstream_close (istr);
    Lstream_close (ostr);
    UNGCPRO;
    Lstream_delete (istr);
    Lstream_delete (ostr);
    Lstream_delete (XLSTREAM (de_outstream));
    Lstream_delete (XLSTREAM (lb_outstream));
    return make_int (retlen);
  }
}

#ifdef MULE

/************************************************************************/
/*                          Shift-JIS methods                           */
/************************************************************************/

/* Shift-JIS is a coding system encoding three character sets: ASCII, right
   half of JISX0201-Kana, and JISX0208.  An ASCII character is encoded
   as is.  A character of JISX0201-Kana (DIMENSION1_CHARS94 character set) is
   encoded by "position-code + 0x80".  A character of JISX0208
   (DIMENSION2_CHARS94 character set) is encoded in 2-byte but two
   position-codes are divided and shifted so that it fit in the range
   below.

   --- CODE RANGE of Shift-JIS ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   JISX0201-Kana	0xA0 .. 0xDF
   JISX0208 (1st byte)	0x80 .. 0x9F and 0xE0 .. 0xEF
	    (2nd byte)	0x40 .. 0x7E and 0x80 .. 0xFC
   -------------------------------

*/

/* Is this the first byte of a Shift-JIS two-byte char? */

#define BYTE_SJIS_TWO_BYTE_1_P(c) \
  (((c) >= 0x81 && (c) <= 0x9F) || ((c) >= 0xE0 && (c) <= 0xEF))

/* Is this the second byte of a Shift-JIS two-byte char? */

#define BYTE_SJIS_TWO_BYTE_2_P(c) \
  (((c) >= 0x40 && (c) <= 0x7E) || ((c) >= 0x80 && (c) <= 0xFC))

#define BYTE_SJIS_KATAKANA_P(c)	\
  ((c) >= 0xA1 && (c) <= 0xDF)

static int
detect_coding_sjis (struct detection_state *st, const Extbyte *src, Lstream_data_count n)
{
  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	return 0;
      if (st->shift_jis.in_second_byte)
	{
	  st->shift_jis.in_second_byte = 0;
	  if (c < 0x40)
	    return 0;
	}
      else if ((c >= 0x80 && c < 0xA0) || c >= 0xE0)
	st->shift_jis.in_second_byte = 1;
    }
  return CODING_CATEGORY_SHIFT_JIS_MASK;
}

/* Convert Shift-JIS data to internal format. */

static void
decode_coding_sjis (Lstream *decoding, const Extbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = str->eol_type;

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;

      if (ch)
	{
	  /* Previous character was first byte of Shift-JIS Kanji char. */
	  if (BYTE_SJIS_TWO_BYTE_2_P (c))
	    {
	      unsigned char e1, e2;

	      Dynarr_add (dst, LEADING_BYTE_JAPANESE_JISX0208);
	      DECODE_SJIS (ch, c, e1, e2);
	      Dynarr_add (dst, e1);
	      Dynarr_add (dst, e2);
	    }
	  else
	    {
	      DECODE_ADD_BINARY_CHAR (ch, dst);
	      DECODE_ADD_BINARY_CHAR (c, dst);
	    }
	  ch = 0;
	}
      else
	{
	  DECODE_HANDLE_EOL_TYPE (eol_type, c, flags, dst);
	  if (BYTE_SJIS_TWO_BYTE_1_P (c))
	    ch = c;
	  else if (BYTE_SJIS_KATAKANA_P (c))
	    {
	      Dynarr_add (dst, LEADING_BYTE_KATAKANA_JISX0201);
	      Dynarr_add (dst, c);
	    }
	  else
	    DECODE_ADD_BINARY_CHAR (c, dst);
	}
    label_continue_loop:;
    }

  DECODE_HANDLE_END_OF_CONVERSION (flags, ch, dst);

  str->flags = flags;
  str->ch    = ch;
}

/* Convert internally-formatted data to Shift-JIS. */

static void
encode_coding_sjis (Lstream *encoding, const Bufbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = CODING_SYSTEM_EOL_TYPE (str->codesys);

  while (n--)
    {
      Bufbyte c = *src++;
      if (c == '\n')
	{
	  if (eol_type != EOL_LF && eol_type != EOL_AUTODETECT)
	    Dynarr_add (dst, '\r');
	  if (eol_type != EOL_CR)
	    Dynarr_add (dst, '\n');
	  ch = 0;
	}
      else if (BYTE_ASCII_P (c))
	{
	  Dynarr_add (dst, c);
	  ch = 0;
	}
      else if (BUFBYTE_LEADING_BYTE_P (c))
	ch = (c == LEADING_BYTE_KATAKANA_JISX0201 ||
	      c == LEADING_BYTE_JAPANESE_JISX0208_1978 ||
	      c == LEADING_BYTE_JAPANESE_JISX0208) ? c : 0;
      else if (ch)
	{
	  if (ch == LEADING_BYTE_KATAKANA_JISX0201)
	    {
	      Dynarr_add (dst, c);
	      ch = 0;
	    }
	  else if (ch == LEADING_BYTE_JAPANESE_JISX0208_1978 ||
		   ch == LEADING_BYTE_JAPANESE_JISX0208)
	    ch = c;
	  else
	    {
	      unsigned char j1, j2;
	      ENCODE_SJIS (ch, c, j1, j2);
	      Dynarr_add (dst, j1);
	      Dynarr_add (dst, j2);
	      ch = 0;
	    }
	}
    }

  str->flags = flags;
  str->ch    = ch;
}

DEFUN ("decode-shift-jis-char", Fdecode_shift_jis_char, 1, 1, 0, /*
Decode a JISX0208 character of Shift-JIS coding-system.
CODE is the character code in Shift-JIS as a cons of type bytes.
Return the corresponding character.
*/
       (code))
{
  unsigned char c1, c2, s1, s2;

  CHECK_CONS (code);
  CHECK_INT (XCAR (code));
  CHECK_INT (XCDR (code));
  s1 = XINT (XCAR (code));
  s2 = XINT (XCDR (code));
  if (BYTE_SJIS_TWO_BYTE_1_P (s1) &&
      BYTE_SJIS_TWO_BYTE_2_P (s2))
    {
      DECODE_SJIS (s1, s2, c1, c2);
      return make_char (MAKE_CHAR (Vcharset_japanese_jisx0208,
				   c1 & 0x7F, c2 & 0x7F));
    }
  else
    return Qnil;
}

DEFUN ("encode-shift-jis-char", Fencode_shift_jis_char, 1, 1, 0, /*
Encode a JISX0208 character CHARACTER to SHIFT-JIS coding-system.
Return the corresponding character code in SHIFT-JIS as a cons of two bytes.
*/
       (character))
{
  Lisp_Object charset;
  int c1, c2, s1, s2;

  CHECK_CHAR_COERCE_INT (character);
  BREAKUP_CHAR (XCHAR (character), charset, c1, c2);
  if (EQ (charset, Vcharset_japanese_jisx0208))
    {
      ENCODE_SJIS (c1 | 0x80, c2 | 0x80, s1, s2);
      return Fcons (make_int (s1), make_int (s2));
    }
  else
    return Qnil;
}


/************************************************************************/
/*                            Big5 methods                              */
/************************************************************************/

/* BIG5 is a coding system encoding two character sets: ASCII and
   Big5.  An ASCII character is encoded as is.  Big5 is a two-byte
   character set and is encoded in two-byte.

   --- CODE RANGE of BIG5 ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   Big5 (1st byte)	0xA1 .. 0xFE
	(2nd byte)	0x40 .. 0x7E and 0xA1 .. 0xFE
   --------------------------

   Since the number of characters in Big5 is larger than maximum
   characters in Emacs' charset (96x96), it can't be handled as one
   charset.  So, in Emacs, Big5 is divided into two: `charset-big5-1'
   and `charset-big5-2'.  Both <type>s are DIMENSION2_CHARS94.  The former
   contains frequently used characters and the latter contains less
   frequently used characters.  */

#define BYTE_BIG5_TWO_BYTE_1_P(c) \
  ((c) >= 0xA1 && (c) <= 0xFE)

/* Is this the second byte of a Shift-JIS two-byte char? */

#define BYTE_BIG5_TWO_BYTE_2_P(c) \
  (((c) >= 0x40 && (c) <= 0x7E) || ((c) >= 0xA1 && (c) <= 0xFE))

/* Number of Big5 characters which have the same code in 1st byte.  */

#define BIG5_SAME_ROW (0xFF - 0xA1 + 0x7F - 0x40)

/* Code conversion macros.  These are macros because they are used in
   inner loops during code conversion.

   Note that temporary variables in macros introduce the classic
   dynamic-scoping problems with variable names.  We use capital-
   lettered variables in the assumption that XEmacs does not use
   capital letters in variables except in a very formalized way
   (e.g. Qstring). */

/* Convert Big5 code (b1, b2) into its internal string representation
   (lb, c1, c2). */

/* There is a much simpler way to split the Big5 charset into two.
   For the moment I'm going to leave the algorithm as-is because it
   claims to separate out the most-used characters into a single
   charset, which perhaps will lead to optimizations in various
   places.

   The way the algorithm works is something like this:

   Big5 can be viewed as a 94x157 charset, where the row is
   encoded into the bytes 0xA1 .. 0xFE and the column is encoded
   into the bytes 0x40 .. 0x7E and 0xA1 .. 0xFE.  As for frequency,
   the split between low and high column numbers is apparently
   meaningless; ascending rows produce less and less frequent chars.
   Therefore, we assign the lower half of rows (0xA1 .. 0xC8) to
   the first charset, and the upper half (0xC9 .. 0xFE) to the
   second.  To do the conversion, we convert the character into
   a single number where 0 .. 156 is the first row, 157 .. 313
   is the second, etc.  That way, the characters are ordered by
   decreasing frequency.  Then we just chop the space in two
   and coerce the result into a 94x94 space.
   */

#define DECODE_BIG5(b1, b2, lb, c1, c2) do				\
{									\
  int B1 = b1, B2 = b2;							\
  unsigned int I							\
    = (B1 - 0xA1) * BIG5_SAME_ROW + B2 - (B2 < 0x7F ? 0x40 : 0x62);	\
									\
  if (B1 < 0xC9)							\
    {									\
      lb = LEADING_BYTE_CHINESE_BIG5_1;					\
    }									\
  else									\
    {									\
      lb = LEADING_BYTE_CHINESE_BIG5_2;					\
      I -= (BIG5_SAME_ROW) * (0xC9 - 0xA1);				\
    }									\
  c1 = I / (0xFF - 0xA1) + 0xA1;					\
  c2 = I % (0xFF - 0xA1) + 0xA1;					\
} while (0)

/* Convert the internal string representation of a Big5 character
   (lb, c1, c2) into Big5 code (b1, b2). */

#define ENCODE_BIG5(lb, c1, c2, b1, b2) do				\
{									\
  unsigned int I = ((c1) - 0xA1) * (0xFF - 0xA1) + ((c2) - 0xA1);	\
									\
  if (lb == LEADING_BYTE_CHINESE_BIG5_2)				\
    {									\
      I += BIG5_SAME_ROW * (0xC9 - 0xA1);				\
    }									\
  b1 = I / BIG5_SAME_ROW + 0xA1;					\
  b2 = I % BIG5_SAME_ROW;						\
  b2 += b2 < 0x3F ? 0x40 : 0x62;					\
} while (0)

static int
detect_coding_big5 (struct detection_state *st, const Extbyte *src, Lstream_data_count n)
{
  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO ||
	  (c >= 0x80 && c <= 0xA0))
	return 0;
      if (st->big5.in_second_byte)
	{
	  st->big5.in_second_byte = 0;
	  if (c < 0x40 || (c >= 0x80 && c <= 0xA0))
	    return 0;
	}
      else if (c >= 0xA1)
	st->big5.in_second_byte = 1;
    }
  return CODING_CATEGORY_BIG5_MASK;
}

/* Convert Big5 data to internal format. */

static void
decode_coding_big5 (Lstream *decoding, const Extbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = str->eol_type;

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      if (ch)
	{
	  /* Previous character was first byte of Big5 char. */
	  if (BYTE_BIG5_TWO_BYTE_2_P (c))
	    {
	      unsigned char b1, b2, b3;
	      DECODE_BIG5 (ch, c, b1, b2, b3);
	      Dynarr_add (dst, b1);
	      Dynarr_add (dst, b2);
	      Dynarr_add (dst, b3);
	    }
	  else
	    {
	      DECODE_ADD_BINARY_CHAR (ch, dst);
	      DECODE_ADD_BINARY_CHAR (c, dst);
	    }
	  ch = 0;
	}
      else
	{
	  DECODE_HANDLE_EOL_TYPE (eol_type, c, flags, dst);
	  if (BYTE_BIG5_TWO_BYTE_1_P (c))
	    ch = c;
	  else
	    DECODE_ADD_BINARY_CHAR (c, dst);
	}
    label_continue_loop:;
    }

  DECODE_HANDLE_END_OF_CONVERSION (flags, ch, dst);

  str->flags = flags;
  str->ch    = ch;
}

/* Convert internally-formatted data to Big5. */

static void
encode_coding_big5 (Lstream *encoding, const Bufbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  unsigned char c;
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = CODING_SYSTEM_EOL_TYPE (str->codesys);

  while (n--)
    {
      c = *src++;
      if (c == '\n')
	{
	  if (eol_type != EOL_LF && eol_type != EOL_AUTODETECT)
	    Dynarr_add (dst, '\r');
	  if (eol_type != EOL_CR)
	    Dynarr_add (dst, '\n');
	}
      else if (BYTE_ASCII_P (c))
	{
	  /* ASCII. */
	  Dynarr_add (dst, c);
	}
      else if (BUFBYTE_LEADING_BYTE_P (c))
	{
	  if (c == LEADING_BYTE_CHINESE_BIG5_1 ||
	      c == LEADING_BYTE_CHINESE_BIG5_2)
	    {
	      /* A recognized leading byte. */
	      ch = c;
	      continue; /* not done with this character. */
	    }
	  /* otherwise just ignore this character. */
	}
      else if (ch == LEADING_BYTE_CHINESE_BIG5_1 ||
	       ch == LEADING_BYTE_CHINESE_BIG5_2)
	{
	  /* Previous char was a recognized leading byte. */
	  ch = (ch << 8) | c;
	  continue; /* not done with this character. */
	}
      else if (ch)
	{
	  /* Encountering second byte of a Big5 character. */
	  unsigned char b1, b2;

	  ENCODE_BIG5 (ch >> 8, ch & 0xFF, c, b1, b2);
	  Dynarr_add (dst, b1);
	  Dynarr_add (dst, b2);
	}

      ch = 0;
    }

  str->flags = flags;
  str->ch    = ch;
}


DEFUN ("decode-big5-char", Fdecode_big5_char, 1, 1, 0, /*
Decode a Big5 character CODE of BIG5 coding-system.
CODE is the character code in BIG5, a cons of two integers.
Return the corresponding character.
*/
       (code))
{
  unsigned char c1, c2, b1, b2;

  CHECK_CONS (code);
  CHECK_INT (XCAR (code));
  CHECK_INT (XCDR (code));
  b1 = XINT (XCAR (code));
  b2 = XINT (XCDR (code));
  if (BYTE_BIG5_TWO_BYTE_1_P (b1) &&
      BYTE_BIG5_TWO_BYTE_2_P (b2))
    {
      int leading_byte;
      Lisp_Object charset;
      DECODE_BIG5 (b1, b2, leading_byte, c1, c2);
      charset = CHARSET_BY_LEADING_BYTE (leading_byte);
      return make_char (MAKE_CHAR (charset, c1 & 0x7F, c2 & 0x7F));
    }
  else
    return Qnil;
}

DEFUN ("encode-big5-char", Fencode_big5_char, 1, 1, 0, /*
Encode the Big5 character CHARACTER in the BIG5 coding-system.
Return the corresponding character code in Big5.
*/
       (character))
{
  Lisp_Object charset;
  int c1, c2, b1, b2;

  CHECK_CHAR_COERCE_INT (character);
  BREAKUP_CHAR (XCHAR (character), charset, c1, c2);
  if (EQ (charset, Vcharset_chinese_big5_1) ||
      EQ (charset, Vcharset_chinese_big5_2))
    {
      ENCODE_BIG5 (XCHARSET_LEADING_BYTE (charset), c1 | 0x80, c2 | 0x80,
		   b1, b2);
      return Fcons (make_int (b1), make_int (b2));
    }
  else
    return Qnil;
}


/************************************************************************/
/*                           UCS-4 methods                              */
/*                                                                      */
/*  UCS-4 character codes are implemented as nonnegative integers.      */
/*                                                                      */
/************************************************************************/


DEFUN ("set-ucs-char", Fset_ucs_char, 2, 2, 0, /*
Map UCS-4 code CODE to Mule character CHARACTER.

Return T on success, NIL on failure.
*/
       (code, character))
{
  size_t c;

  CHECK_CHAR (character);
  CHECK_NATNUM (code);
  c = XINT (code);

  if (c < countof (fcd->ucs_to_mule_table))
    {
      fcd->ucs_to_mule_table[c] = character;
      return Qt;
    }
  else
    return Qnil;
}

static Lisp_Object
ucs_to_char (unsigned long code)
{
  if (code < countof (fcd->ucs_to_mule_table))
    {
      return fcd->ucs_to_mule_table[code];
    }
  else if ((0xe00000 <= code) && (code <= 0xe00000 + 94 * 94 * 14))
    {
      unsigned int c;

      code -= 0xe00000;
      c = code % (94 * 94);
      return make_char
	(MAKE_CHAR (CHARSET_BY_ATTRIBUTES
		    (CHARSET_TYPE_94X94, code / (94 * 94) + '@',
		     CHARSET_LEFT_TO_RIGHT),
		    c / 94 + 33, c % 94 + 33));
    }
  else
    return Qnil;
}

DEFUN ("ucs-char", Fucs_char, 1, 1, 0, /*
Return Mule character corresponding to UCS code CODE (a positive integer).
*/
       (code))
{
  CHECK_NATNUM (code);
  return ucs_to_char (XINT (code));
}

DEFUN ("set-char-ucs", Fset_char_ucs, 2, 2, 0, /*
Map Mule character CHARACTER to UCS code CODE (a positive integer).
*/
       (character, code))
{
  /* #### Isn't this gilding the lily?  Fput_char_table checks its args.
          Fset_char_ucs is more restrictive on index arg, but should
          check code arg in a char_table method. */
  CHECK_CHAR (character);
  CHECK_NATNUM (code);
  return Fput_char_table (character, code, mule_to_ucs_table);
}

DEFUN ("char-ucs", Fchar_ucs, 1, 1, 0, /*
Return the UCS code (a positive integer) corresponding to CHARACTER.
*/
       (character))
{
  return Fget_char_table (character, mule_to_ucs_table);
}

/* Decode a UCS-4 character into a buffer.  If the lookup fails, use
   <GETA MARK> (U+3013) of JIS X 0208, which means correct character
   is not found, instead.
   #### do something more appropriate (use blob?)
        Danger, Will Robinson!  Data loss.  Should we signal user? */
static void
decode_ucs4 (unsigned long ch, unsigned_char_dynarr *dst)
{
  Lisp_Object chr = ucs_to_char (ch);

  if (! NILP (chr))
    {
      Bufbyte work[MAX_EMCHAR_LEN];
      int len;

      ch = XCHAR (chr);
      len = (ch < 128) ?
	simple_set_charptr_emchar (work, ch) :
	non_ascii_set_charptr_emchar (work, ch);
      Dynarr_add_many (dst, work, len);
    }
  else
    {
      Dynarr_add (dst, LEADING_BYTE_JAPANESE_JISX0208);
      Dynarr_add (dst, 34 + 128);
      Dynarr_add (dst, 46 + 128);
    }
}

static unsigned long
mule_char_to_ucs4 (Lisp_Object charset,
		   unsigned char h, unsigned char l)
{
  Lisp_Object code
    = Fget_char_table (make_char (MAKE_CHAR (charset, h & 127, l & 127)),
		       mule_to_ucs_table);

  if (INTP (code))
    {
      return XINT (code);
    }
  else if ( (XCHARSET_DIMENSION (charset) == 2) &&
	    (XCHARSET_CHARS (charset) == 94) )
    {
      unsigned char final = XCHARSET_FINAL (charset);

      if ( ('@' <= final) && (final < 0x7f) )
	{
	  return 0xe00000 + (final - '@') * 94 * 94
	    + ((h & 127) - 33) * 94 + (l & 127) - 33;
	}
      else
	{
	  return '?';
	}
    }
  else
    {
      return '?';
    }
}

static void
encode_ucs4 (Lisp_Object charset,
	     unsigned char h, unsigned char l, unsigned_char_dynarr *dst)
{
  unsigned long code = mule_char_to_ucs4 (charset, h, l);
  Dynarr_add (dst,  code >> 24);
  Dynarr_add (dst, (code >> 16) & 255);
  Dynarr_add (dst, (code >>  8) & 255);
  Dynarr_add (dst,  code        & 255);
}

static int
detect_coding_ucs4 (struct detection_state *st, const Extbyte *src, Lstream_data_count n)
{
  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      switch (st->ucs4.in_byte)
	{
	case 0:
	  if (c >= 128)
	    return 0;
	  else
	    st->ucs4.in_byte++;
	  break;
	case 3:
	  st->ucs4.in_byte = 0;
	  break;
	default:
	  st->ucs4.in_byte++;
	}
    }
  return CODING_CATEGORY_UCS4_MASK;
}

static void
decode_coding_ucs4 (Lstream *decoding, const Extbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);
  unsigned int flags = str->flags;
  unsigned int ch    = str->ch;
  unsigned char counter = str->counter;

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      switch (counter)
	{
	case 0:
	  ch = c;
	  counter = 3;
	  break;
	case 1:
	  decode_ucs4 ( ( ch << 8 ) | c, dst);
	  ch = 0;
	  counter = 0;
	  break;
	default:
	  ch = ( ch << 8 ) | c;
	  counter--;
	}
    }
  if (counter & CODING_STATE_END)
    DECODE_OUTPUT_PARTIAL_CHAR (ch);

  str->flags = flags;
  str->ch    = ch;
  str->counter = counter;
}

static void
encode_coding_ucs4 (Lstream *encoding, const Bufbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);
  unsigned int flags = str->flags;
  unsigned int ch = str->ch;
  unsigned char char_boundary = str->iso2022.current_char_boundary;
  Lisp_Object charset = str->iso2022.current_charset;

#ifdef ENABLE_COMPOSITE_CHARS
  /* flags for handling composite chars.  We do a little switcharoo
     on the source while we're outputting the composite char. */
  unsigned int saved_n = 0;
  const unsigned char *saved_src = NULL;
  int in_composite = 0;

 back_to_square_n:
#endif

  while (n--)
    {
      unsigned char c = *src++;

      if (BYTE_ASCII_P (c))
	{		/* Processing ASCII character */
	  ch = 0;
	  encode_ucs4 (Vcharset_ascii, c, 0, dst);
	  char_boundary = 1;
	}
      else if (BUFBYTE_LEADING_BYTE_P (c) || BUFBYTE_LEADING_BYTE_P (ch))
	{ /* Processing Leading Byte */
	  ch = 0;
	  charset = CHARSET_BY_LEADING_BYTE (c);
	  if (LEADING_BYTE_PREFIX_P(c))
	    ch = c;
	  char_boundary = 0;
	}
      else
	{			/* Processing Non-ASCII character */
	  char_boundary = 1;
	  if (EQ (charset, Vcharset_control_1))
	    {
	      encode_ucs4 (Vcharset_control_1, c, 0, dst);
	    }
	  else
	    {
	      switch (XCHARSET_REP_BYTES (charset))
		{
		case 2:
		  encode_ucs4 (charset, c, 0, dst);
		  break;
		case 3:
		  if (XCHARSET_PRIVATE_P (charset))
		    {
		      encode_ucs4 (charset, c, 0, dst);
		      ch = 0;
		    }
		  else if (ch)
		    {
#ifdef ENABLE_COMPOSITE_CHARS
		      if (EQ (charset, Vcharset_composite))
			{
			  if (in_composite)
			    {
			      /* #### Bother! We don't know how to
				 handle this yet. */
			      Dynarr_add (dst, '\0');
			      Dynarr_add (dst, '\0');
			      Dynarr_add (dst, '\0');
			      Dynarr_add (dst, '~');
			    }
			  else
			    {
			      Emchar emch = MAKE_CHAR (Vcharset_composite,
						       ch & 0x7F, c & 0x7F);
			      Lisp_Object lstr = composite_char_string (emch);
			      saved_n = n;
			      saved_src = src;
			      in_composite = 1;
			      src = XSTRING_DATA   (lstr);
			      n   = XSTRING_LENGTH (lstr);
			    }
			}
		      else
#endif /* ENABLE_COMPOSITE_CHARS */
			{
			  encode_ucs4(charset, ch, c, dst);
			}
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		case 4:
		  if (ch)
		    {
		      encode_ucs4 (charset, ch, c, dst);
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		default:
		  abort ();
		}
	    }
	}
    }

#ifdef ENABLE_COMPOSITE_CHARS
  if (in_composite)
    {
      n = saved_n;
      src = saved_src;
      in_composite = 0;
      goto back_to_square_n; /* Wheeeeeeeee ..... */
    }
#endif /* ENABLE_COMPOSITE_CHARS */

  str->flags = flags;
  str->ch = ch;
  str->iso2022.current_char_boundary = char_boundary;
  str->iso2022.current_charset = charset;

  /* Verbum caro factum est! */
}


/************************************************************************/
/*                           UTF-8 methods                              */
/************************************************************************/

static int
detect_coding_utf8 (struct detection_state *st, const Extbyte *src, Lstream_data_count n)
{
  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      switch (st->utf8.in_byte)
	{
	case 0:
	  if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	    return 0;
	  else if (c >= 0xfc)
	    st->utf8.in_byte = 5;
	  else if (c >= 0xf8)
	    st->utf8.in_byte = 4;
	  else if (c >= 0xf0)
	    st->utf8.in_byte = 3;
	  else if (c >= 0xe0)
	    st->utf8.in_byte = 2;
	  else if (c >= 0xc0)
	    st->utf8.in_byte = 1;
	  else if (c >= 0x80)
	    return 0;
	  break;
	default:
	  if ((c & 0xc0) != 0x80)
	    return 0;
	  else
	    st->utf8.in_byte--;
	}
    }
  return CODING_CATEGORY_UTF8_MASK;
}

static void
decode_coding_utf8 (Lstream *decoding, const Extbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = str->eol_type;
  unsigned char counter = str->counter;

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      switch (counter)
	{
	case 0:
	  if ( c >= 0xfc )
	    {
	      ch = c & 0x01;
	      counter = 5;
	    }
	  else if ( c >= 0xf8 )
	    {
	      ch = c & 0x03;
	      counter = 4;
	    }
	  else if ( c >= 0xf0 )
	    {
	      ch = c & 0x07;
	      counter = 3;
	    }
	  else if ( c >= 0xe0 )
	    {
	      ch = c & 0x0f;
	      counter = 2;
	    }
	  else if ( c >= 0xc0 )
	    {
	      ch = c & 0x1f;
	      counter = 1;
	    }
	  else
	    {
	      DECODE_HANDLE_EOL_TYPE (eol_type, c, flags, dst);
	      decode_ucs4 (c, dst);
	    }
	  break;
	case 1:
	  ch = ( ch << 6 ) | ( c & 0x3f );
	  decode_ucs4 (ch, dst);
	  ch = 0;
	  counter = 0;
	  break;
	default:
	  ch = ( ch << 6 ) | ( c & 0x3f );
	  counter--;
	}
    label_continue_loop:;
    }

  if (flags & CODING_STATE_END)
    DECODE_OUTPUT_PARTIAL_CHAR (ch);

  str->flags = flags;
  str->ch    = ch;
  str->counter = counter;
}

static void
encode_utf8 (Lisp_Object charset,
	     unsigned char h, unsigned char l, unsigned_char_dynarr *dst)
{
  unsigned long code = mule_char_to_ucs4 (charset, h, l);
  if ( code <= 0x7f )
    {
      Dynarr_add (dst, code);
    }
  else if ( code <= 0x7ff )
    {
      Dynarr_add (dst, (code >> 6) | 0xc0);
      Dynarr_add (dst, (code & 0x3f) | 0x80);
    }
  else if ( code <= 0xffff )
    {
      Dynarr_add (dst,  (code >> 12) | 0xe0);
      Dynarr_add (dst, ((code >>  6) & 0x3f) | 0x80);
      Dynarr_add (dst,  (code        & 0x3f) | 0x80);
    }
  else if ( code <= 0x1fffff )
    {
      Dynarr_add (dst,  (code >> 18) | 0xf0);
      Dynarr_add (dst, ((code >> 12) & 0x3f) | 0x80);
      Dynarr_add (dst, ((code >>  6) & 0x3f) | 0x80);
      Dynarr_add (dst,  (code        & 0x3f) | 0x80);
    }
  else if ( code <= 0x3ffffff )
    {
      Dynarr_add (dst,  (code >> 24) | 0xf8);
      Dynarr_add (dst, ((code >> 18) & 0x3f) | 0x80);
      Dynarr_add (dst, ((code >> 12) & 0x3f) | 0x80);
      Dynarr_add (dst, ((code >>  6) & 0x3f) | 0x80);
      Dynarr_add (dst,  (code        & 0x3f) | 0x80);
    }
  else
    {
      Dynarr_add (dst,  (code >> 30) | 0xfc);
      Dynarr_add (dst, ((code >> 24) & 0x3f) | 0x80);
      Dynarr_add (dst, ((code >> 18) & 0x3f) | 0x80);
      Dynarr_add (dst, ((code >> 12) & 0x3f) | 0x80);
      Dynarr_add (dst, ((code >>  6) & 0x3f) | 0x80);
      Dynarr_add (dst,  (code        & 0x3f) | 0x80);
    }
}

static void
encode_coding_utf8 (Lstream *encoding, const Bufbyte *src,
		    unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = CODING_SYSTEM_EOL_TYPE (str->codesys);
  unsigned char char_boundary = str->iso2022.current_char_boundary;
  Lisp_Object charset = str->iso2022.current_charset;

#ifdef ENABLE_COMPOSITE_CHARS
  /* flags for handling composite chars.  We do a little switcharoo
     on the source while we're outputting the composite char. */
  unsigned int saved_n = 0;
  const unsigned char *saved_src = NULL;
  int in_composite = 0;

 back_to_square_n:
#endif /* ENABLE_COMPOSITE_CHARS */

  while (n--)
    {
      unsigned char c = *src++;

      if (BYTE_ASCII_P (c))
	{		/* Processing ASCII character */
	  ch = 0;
	  if (c == '\n')
	    {
	      if (eol_type != EOL_LF && eol_type != EOL_AUTODETECT)
		Dynarr_add (dst, '\r');
	      if (eol_type != EOL_CR)
		Dynarr_add (dst, c);
	    }
	  else
	    encode_utf8 (Vcharset_ascii, c, 0, dst);
	  char_boundary = 1;
	}
      else if (BUFBYTE_LEADING_BYTE_P (c) || BUFBYTE_LEADING_BYTE_P (ch))
	{ /* Processing Leading Byte */
	  ch = 0;
	  charset = CHARSET_BY_LEADING_BYTE (c);
	  if (LEADING_BYTE_PREFIX_P(c))
	    ch = c;
	  char_boundary = 0;
	}
      else
	{			/* Processing Non-ASCII character */
	  char_boundary = 1;
	  if (EQ (charset, Vcharset_control_1))
	    {
	      encode_utf8 (Vcharset_control_1, c, 0, dst);
	    }
	  else
	    {
	      switch (XCHARSET_REP_BYTES (charset))
		{
		case 2:
		  encode_utf8 (charset, c, 0, dst);
		  break;
		case 3:
		  if (XCHARSET_PRIVATE_P (charset))
		    {
		      encode_utf8 (charset, c, 0, dst);
		      ch = 0;
		    }
		  else if (ch)
		    {
#ifdef ENABLE_COMPOSITE_CHARS
		      if (EQ (charset, Vcharset_composite))
			{
			  if (in_composite)
			    {
			      /* #### Bother! We don't know how to
				 handle this yet. */
			      encode_utf8 (Vcharset_ascii, '~', 0, dst);
			    }
			  else
			    {
			      Emchar emch = MAKE_CHAR (Vcharset_composite,
						       ch & 0x7F, c & 0x7F);
			      Lisp_Object lstr = composite_char_string (emch);
			      saved_n = n;
			      saved_src = src;
			      in_composite = 1;
			      src = XSTRING_DATA   (lstr);
			      n   = XSTRING_LENGTH (lstr);
			    }
			}
		      else
#endif /* ENABLE_COMPOSITE_CHARS */
			{
			  encode_utf8 (charset, ch, c, dst);
			}
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		case 4:
		  if (ch)
		    {
		      encode_utf8 (charset, ch, c, dst);
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		default:
		  abort ();
		}
	    }
	}
    }

#ifdef ENABLE_COMPOSITE_CHARS
  if (in_composite)
    {
      n = saved_n;
      src = saved_src;
      in_composite = 0;
      goto back_to_square_n; /* Wheeeeeeeee ..... */
    }
#endif

  str->flags = flags;
  str->ch    = ch;
  str->iso2022.current_char_boundary = char_boundary;
  str->iso2022.current_charset = charset;

  /* Verbum caro factum est! */
}


/************************************************************************/
/*                           ISO2022 methods                            */
/************************************************************************/

/* The following note describes the coding system ISO2022 briefly.
   Since the intention of this note is to help understand the
   functions in this file, some parts are NOT ACCURATE or OVERLY
   SIMPLIFIED.  For thorough understanding, please refer to the
   original document of ISO2022.

   ISO2022 provides many mechanisms to encode several character sets
   in 7-bit and 8-bit environments.  For 7-bit environments, all text
   is encoded using bytes less than 128.  This may make the encoded
   text a little bit longer, but the text passes more easily through
   several gateways, some of which strip off MSB (Most Signigant Bit).

   There are two kinds of character sets: control character set and
   graphic character set.  The former contains control characters such
   as `newline' and `escape' to provide control functions (control
   functions are also provided by escape sequences).  The latter
   contains graphic characters such as 'A' and '-'.  Emacs recognizes
   two control character sets and many graphic character sets.

   Graphic character sets are classified into one of the following
   four classes, according to the number of bytes (DIMENSION) and
   number of characters in one dimension (CHARS) of the set:
   - DIMENSION1_CHARS94
   - DIMENSION1_CHARS96
   - DIMENSION2_CHARS94
   - DIMENSION2_CHARS96

   In addition, each character set is assigned an identification tag,
   unique for each set, called "final character" (denoted as <F>
   hereafter).  The <F> of each character set is decided by ECMA(*)
   when it is registered in ISO.  The code range of <F> is 0x30..0x7F
   (0x30..0x3F are for private use only).

   Note (*): ECMA = European Computer Manufacturers Association

   Here are examples of graphic character set [NAME(<F>)]:
	o DIMENSION1_CHARS94 -- ASCII('B'), right-half-of-JISX0201('I'), ...
	o DIMENSION1_CHARS96 -- right-half-of-ISO8859-1('A'), ...
	o DIMENSION2_CHARS94 -- GB2312('A'), JISX0208('B'), ...
	o DIMENSION2_CHARS96 -- none for the moment

   A code area (1 byte = 8 bits) is divided into 4 areas, C0, GL, C1, and GR.
	C0 [0x00..0x1F] -- control character plane 0
	GL [0x20..0x7F] -- graphic character plane 0
	C1 [0x80..0x9F] -- control character plane 1
	GR [0xA0..0xFF] -- graphic character plane 1

   A control character set is directly designated and invoked to C0 or
   C1 by an escape sequence.  The most common case is that:
   - ISO646's  control character set is designated/invoked to C0, and
   - ISO6429's control character set is designated/invoked to C1,
   and usually these designations/invocations are omitted in encoded
   text.  In a 7-bit environment, only C0 can be used, and a control
   character for C1 is encoded by an appropriate escape sequence to
   fit into the environment.  All control characters for C1 are
   defined to have corresponding escape sequences.

   A graphic character set is at first designated to one of four
   graphic registers (G0 through G3), then these graphic registers are
   invoked to GL or GR.  These designations and invocations can be
   done independently.  The most common case is that G0 is invoked to
   GL, G1 is invoked to GR, and ASCII is designated to G0.  Usually
   these invocations and designations are omitted in encoded text.
   In a 7-bit environment, only GL can be used.

   When a graphic character set of CHARS94 is invoked to GL, codes
   0x20 and 0x7F of the GL area work as control characters SPACE and
   DEL respectively, and codes 0xA0 and 0xFF of the GR area should not
   be used.

   There are two ways of invocation: locking-shift and single-shift.
   With locking-shift, the invocation lasts until the next different
   invocation, whereas with single-shift, the invocation affects the
   following character only and doesn't affect the locking-shift
   state.  Invocations are done by the following control characters or
   escape sequences:

   ----------------------------------------------------------------------
   abbrev  function	             cntrl escape seq	description
   ----------------------------------------------------------------------
   SI/LS0  (shift-in)		     0x0F  none		invoke G0 into GL
   SO/LS1  (shift-out)		     0x0E  none		invoke G1 into GL
   LS2     (locking-shift-2)	     none  ESC 'n'	invoke G2 into GL
   LS3     (locking-shift-3)	     none  ESC 'o'	invoke G3 into GL
   LS1R    (locking-shift-1 right)   none  ESC '~'      invoke G1 into GR (*)
   LS2R    (locking-shift-2 right)   none  ESC '}'      invoke G2 into GR (*)
   LS3R    (locking-shift 3 right)   none  ESC '|'      invoke G3 into GR (*)
   SS2     (single-shift-2)	     0x8E  ESC 'N'	invoke G2 for one char
   SS3     (single-shift-3)	     0x8F  ESC 'O'	invoke G3 for one char
   ----------------------------------------------------------------------
   (*) These are not used by any known coding system.

   Control characters for these functions are defined by macros
   ISO_CODE_XXX in `coding.h'.

   Designations are done by the following escape sequences:
   ----------------------------------------------------------------------
   escape sequence	description
   ----------------------------------------------------------------------
   ESC '(' <F>		designate DIMENSION1_CHARS94<F> to G0
   ESC ')' <F>		designate DIMENSION1_CHARS94<F> to G1
   ESC '*' <F>		designate DIMENSION1_CHARS94<F> to G2
   ESC '+' <F>		designate DIMENSION1_CHARS94<F> to G3
   ESC ',' <F>		designate DIMENSION1_CHARS96<F> to G0 (*)
   ESC '-' <F>		designate DIMENSION1_CHARS96<F> to G1
   ESC '.' <F>		designate DIMENSION1_CHARS96<F> to G2
   ESC '/' <F>		designate DIMENSION1_CHARS96<F> to G3
   ESC '$' '(' <F>	designate DIMENSION2_CHARS94<F> to G0 (**)
   ESC '$' ')' <F>	designate DIMENSION2_CHARS94<F> to G1
   ESC '$' '*' <F>	designate DIMENSION2_CHARS94<F> to G2
   ESC '$' '+' <F>	designate DIMENSION2_CHARS94<F> to G3
   ESC '$' ',' <F>	designate DIMENSION2_CHARS96<F> to G0 (*)
   ESC '$' '-' <F>	designate DIMENSION2_CHARS96<F> to G1
   ESC '$' '.' <F>	designate DIMENSION2_CHARS96<F> to G2
   ESC '$' '/' <F>	designate DIMENSION2_CHARS96<F> to G3
   ----------------------------------------------------------------------

   In this list, "DIMENSION1_CHARS94<F>" means a graphic character set
   of dimension 1, chars 94, and final character <F>, etc...

   Note (*): Although these designations are not allowed in ISO2022,
   Emacs accepts them on decoding, and produces them on encoding
   CHARS96 character sets in a coding system which is characterized as
   7-bit environment, non-locking-shift, and non-single-shift.

   Note (**): If <F> is '@', 'A', or 'B', the intermediate character
   '(' can be omitted.  We refer to this as "short-form" hereafter.

   Now you may notice that there are a lot of ways for encoding the
   same multilingual text in ISO2022.  Actually, there exist many
   coding systems such as Compound Text (used in X11's inter client
   communication, ISO-2022-JP (used in Japanese internet), ISO-2022-KR
   (used in Korean internet), EUC (Extended UNIX Code, used in Asian
   localized platforms), and all of these are variants of ISO2022.

   In addition to the above, Emacs handles two more kinds of escape
   sequences: ISO6429's direction specification and Emacs' private
   sequence for specifying character composition.

   ISO6429's direction specification takes the following form:
	o CSI ']'      -- end of the current direction
	o CSI '0' ']'  -- end of the current direction
	o CSI '1' ']'  -- start of left-to-right text
	o CSI '2' ']'  -- start of right-to-left text
   The control character CSI (0x9B: control sequence introducer) is
   abbreviated to the escape sequence ESC '[' in a 7-bit environment.

   Character composition specification takes the following form:
	o ESC '0' -- start character composition
	o ESC '1' -- end character composition
   Since these are not standard escape sequences of any ISO standard,
   their use with these meanings is restricted to Emacs only.  */

static void
reset_iso2022 (Lisp_Object coding_system, struct iso2022_decoder *iso)
{
  int i;

  for (i = 0; i < 4; i++)
    {
      if (!NILP (coding_system))
	iso->charset[i] =
	  XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (coding_system, i);
      else
	iso->charset[i] = Qt;
      iso->invalid_designated[i] = 0;
    }
  iso->esc = ISO_ESC_NOTHING;
  iso->esc_bytes_index = 0;
  iso->register_left = 0;
  iso->register_right = 1;
  iso->switched_dir_and_no_valid_charset_yet = 0;
  iso->invalid_switch_dir = 0;
  iso->output_direction_sequence = 0;
  iso->output_literally = 0;
#ifdef ENABLE_COMPOSITE_CHARS
  if (iso->composite_chars)
    Dynarr_reset (iso->composite_chars);
#endif
}

static int
fit_to_be_escape_quoted (unsigned char c)
{
  switch (c)
    {
    case ISO_CODE_ESC:
    case ISO_CODE_CSI:
    case ISO_CODE_SS2:
    case ISO_CODE_SS3:
    case ISO_CODE_SO:
    case ISO_CODE_SI:
      return 1;

    default:
      return 0;
    }
}

/* Parse one byte of an ISO2022 escape sequence.
   If the result is an invalid escape sequence, return 0 and
   do not change anything in STR.  Otherwise, if the result is
   an incomplete escape sequence, update ISO2022.ESC and
   ISO2022.ESC_BYTES and return -1.  Otherwise, update
   all the state variables (but not ISO2022.ESC_BYTES) and
   return 1.

   If CHECK_INVALID_CHARSETS is non-zero, check for designation
   or invocation of an invalid character set and treat that as
   an unrecognized escape sequence.

   ********************************************************************

   #### Strategies for error annotation and coding orthogonalization

   We really want to separate out a number of things.  Conceptually,
   there is a nested syntax.

   At the top level is the ISO 2022 extension syntax, including charset
   designation and invocation, and certain auxiliary controls such as the
   ISO 6429 direction specification.  These are octet-oriented, with the
   single exception (AFAIK) of the "exit Unicode" sequence which uses the
   UTF's natural width (1 byte for UTF-7 and UTF-8, 2 bytes for UCS-2 and
   UTF-16, and 4 bytes for UCS-4 and UTF-32).  This will be treated as a
   (deprecated) special case in Unicode processing.

   The middle layer is ISO 2022 character interpretation.  This will depend
   on the current state of the ISO 2022 registers, and assembles octets
   into the character's internal representation.

   The lowest level is translating system control conventions.  At present
   this is restricted to newline translation, but one could imagine doing
   tab conversion or line wrapping here.  "Escape from Unicode" processing
   would be done at this level.

   At each level the parser will verify the syntax.  In the case of a
   syntax error or warning (such as a redundant escape sequence that affects
   no characters), the parser will take some action, typically inserting the
   erroneous octets directly into the output and creating an annotation
   which can be used by higher level I/O to mark the affected region.

   This should make it possible to do something sensible about separating
   newline convention processing from character construction, and about
   preventing ISO 2022 escape sequences from being recognized
   inappropriately.

   The basic strategy will be to have octet classification tables, and
   switch processing according to the table entry.

   It's possible that, by doing the processing with tables of functions or
   the like, the parser can be used for both detection and translation. */

static int
parse_iso2022_esc (Lisp_Object codesys, struct iso2022_decoder *iso,
		   unsigned char c, unsigned int *flags,
		   int check_invalid_charsets)
{
  /* (1) If we're at the end of a designation sequence, CS is the
     charset being designated and REG is the register to designate
     it to.

     (2) If we're at the end of a locking-shift sequence, REG is
     the register to invoke and HALF (0 == left, 1 == right) is
     the half to invoke it into.

     (3) If we're at the end of a single-shift sequence, REG is
     the register to invoke. */
  Lisp_Object cs = Qnil;
  int reg, half;

  /* NOTE: This code does goto's all over the fucking place.
     The reason for this is that we're basically implementing
     a state machine here, and hierarchical languages like C
     don't really provide a clean way of doing this. */

  if (! (*flags & CODING_STATE_ESCAPE))
    /* At beginning of escape sequence; we need to reset our
       escape-state variables. */
    iso->esc = ISO_ESC_NOTHING;

  iso->output_literally = 0;
  iso->output_direction_sequence = 0;

  switch (iso->esc)
    {
    case ISO_ESC_NOTHING:
      iso->esc_bytes_index = 0;
      switch (c)
	{
	case ISO_CODE_ESC:	/* Start escape sequence */
	  *flags |= CODING_STATE_ESCAPE;
	  iso->esc = ISO_ESC;
	  goto not_done;

	case ISO_CODE_CSI:      /* ISO6429 (specifying directionality) */
	  *flags |= CODING_STATE_ESCAPE;
	  iso->esc = ISO_ESC_5_11;
	  goto not_done;

	case ISO_CODE_SO:	/* locking shift 1 */
	  reg = 1; half = 0;
	  goto locking_shift;
	case ISO_CODE_SI:	/* locking shift 0 */
	  reg = 0; half = 0;
	  goto locking_shift;

	case ISO_CODE_SS2:	/* single shift */
	  reg = 2;
	  goto single_shift;
	case ISO_CODE_SS3:	/* single shift */
	  reg = 3;
	  goto single_shift;

	default:			/* Other control characters */
	  return 0;
	}

    case ISO_ESC:
      switch (c)
	{
	  /**** single shift ****/

	case 'N':	/* single shift 2 */
	  reg = 2;
	  goto single_shift;
	case 'O':	/* single shift 3 */
	  reg = 3;
	  goto single_shift;

	  /**** locking shift ****/

	case '~':	/* locking shift 1 right */
	  reg = 1; half = 1;
	  goto locking_shift;
	case 'n':	/* locking shift 2 */
	  reg = 2; half = 0;
	  goto locking_shift;
	case '}':	/* locking shift 2 right */
	  reg = 2; half = 1;
	  goto locking_shift;
	case 'o':	/* locking shift 3 */
	  reg = 3; half = 0;
	  goto locking_shift;
	case '|':	/* locking shift 3 right */
	  reg = 3; half = 1;
	  goto locking_shift;

#ifdef ENABLE_COMPOSITE_CHARS
	  /**** composite ****/

	case '0':
	  iso->esc = ISO_ESC_START_COMPOSITE;
	  *flags = (*flags & CODING_STATE_ISO2022_LOCK) |
	    CODING_STATE_COMPOSITE;
	  return 1;

	case '1':
	  iso->esc = ISO_ESC_END_COMPOSITE;
	  *flags = (*flags & CODING_STATE_ISO2022_LOCK) &
	    ~CODING_STATE_COMPOSITE;
	  return 1;
#endif /* ENABLE_COMPOSITE_CHARS */

	  /**** directionality ****/

	case '[':
	  iso->esc = ISO_ESC_5_11;
	  goto not_done;

	  /**** designation ****/

	case '$':	/* multibyte charset prefix */
	  iso->esc = ISO_ESC_2_4;
	  goto not_done;

	default:
	  if (0x28 <= c && c <= 0x2F)
	    {
	      iso->esc = (enum iso_esc_flag) (c - 0x28 + ISO_ESC_2_8);
	      goto not_done;
	    }

	  /* This function is called with CODESYS equal to nil when
	     doing coding-system detection. */
	  if (!NILP (codesys)
	      && XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
	      && fit_to_be_escape_quoted (c))
	    {
	      iso->esc = ISO_ESC_LITERAL;
	      *flags &= CODING_STATE_ISO2022_LOCK;
	      return 1;
	    }

	  /* bzzzt! */
	  return 0;
	}



      /**** directionality ****/

    case ISO_ESC_5_11:		/* ISO6429 direction control */
      if (c == ']')
	{
	  *flags &= (CODING_STATE_ISO2022_LOCK & ~CODING_STATE_R2L);
	  goto directionality;
	}
      if      (c == '0') iso->esc = ISO_ESC_5_11_0;
      else if (c == '1') iso->esc = ISO_ESC_5_11_1;
      else if (c == '2') iso->esc = ISO_ESC_5_11_2;
      else               return 0;
      goto not_done;

    case ISO_ESC_5_11_0:
      if (c == ']')
	{
	  *flags &= (CODING_STATE_ISO2022_LOCK & ~CODING_STATE_R2L);
	  goto directionality;
	}
      return 0;

    case ISO_ESC_5_11_1:
      if (c == ']')
	{
	  *flags = (CODING_STATE_ISO2022_LOCK & ~CODING_STATE_R2L);
	  goto directionality;
	}
      return 0;

    case ISO_ESC_5_11_2:
      if (c == ']')
	{
	  *flags = (*flags & CODING_STATE_ISO2022_LOCK) | CODING_STATE_R2L;
	  goto directionality;
	}
      return 0;

    directionality:
      iso->esc = ISO_ESC_DIRECTIONALITY;
      /* Various junk here to attempt to preserve the direction sequences
	 literally in the text if they would otherwise be swallowed due
	 to invalid designations that don't show up as actual charset
	 changes in the text. */
      if (iso->invalid_switch_dir)
	{
	  /* We already inserted a direction switch literally into the
	     text.  We assume (#### this may not be right) that the
	     next direction switch is the one going the other way,
	     and we need to output that literally as well. */
	  iso->output_literally = 1;
	  iso->invalid_switch_dir = 0;
	}
      else
	{
	  int jj;

	  /* If we are in the thrall of an invalid designation,
	   then stick the directionality sequence literally into the
	   output stream so it ends up in the original text again. */
	  for (jj = 0; jj < 4; jj++)
	    if (iso->invalid_designated[jj])
	      break;
	  if (jj < 4)
	    {
	      iso->output_literally = 1;
	      iso->invalid_switch_dir = 1;
	    }
	  else
	    /* Indicate that we haven't yet seen a valid designation,
	       so that if a switch-dir is directly followed by an
	       invalid designation, both get inserted literally. */
	    iso->switched_dir_and_no_valid_charset_yet = 1;
	}
      return 1;


      /**** designation ****/

    case ISO_ESC_2_4:
      if (0x28 <= c && c <= 0x2F)
	{
	  iso->esc = (enum iso_esc_flag) (c - 0x28 + ISO_ESC_2_4_8);
	  goto not_done;
	}
      if (0x40 <= c && c <= 0x42)
	{
	  cs = CHARSET_BY_ATTRIBUTES (CHARSET_TYPE_94X94, c,
				      *flags & CODING_STATE_R2L ?
				      CHARSET_RIGHT_TO_LEFT :
				      CHARSET_LEFT_TO_RIGHT);
	  reg = 0;
	  goto designated;
	}
      return 0;

    default:
      {
	int type =-1;

	if (c < '0' || c > '~')
	  return 0; /* bad final byte */

	if (iso->esc >= ISO_ESC_2_8 &&
	    iso->esc <= ISO_ESC_2_15)
	  {
	    type = ((iso->esc >= ISO_ESC_2_12) ?
		    CHARSET_TYPE_96 : CHARSET_TYPE_94);
	    reg = (iso->esc - ISO_ESC_2_8) & 3;
	  }
	else if (iso->esc >= ISO_ESC_2_4_8 &&
		 iso->esc <= ISO_ESC_2_4_15)
	  {
	    type = ((iso->esc >= ISO_ESC_2_4_12) ?
		    CHARSET_TYPE_96X96 : CHARSET_TYPE_94X94);
	    reg = (iso->esc - ISO_ESC_2_4_8) & 3;
	  }
	else
	  {
	    /* Can this ever be reached? -slb */
	    abort();
	    return 0;
	  }

	cs = CHARSET_BY_ATTRIBUTES (type, c,
				    *flags & CODING_STATE_R2L ?
				    CHARSET_RIGHT_TO_LEFT :
				    CHARSET_LEFT_TO_RIGHT);
	goto designated;
      }
    }

 not_done:
  iso->esc_bytes[iso->esc_bytes_index++] = (unsigned char) c;
  return -1;

 single_shift:
  if (check_invalid_charsets && !CHARSETP (iso->charset[reg]))
    /* can't invoke something that ain't there. */
    return 0;
  iso->esc = ISO_ESC_SINGLE_SHIFT;
  *flags &= CODING_STATE_ISO2022_LOCK;
  if (reg == 2)
    *flags |= CODING_STATE_SS2;
  else
    *flags |= CODING_STATE_SS3;
  return 1;

 locking_shift:
  if (check_invalid_charsets &&
      !CHARSETP (iso->charset[reg]))
    /* can't invoke something that ain't there. */
    return 0;
  if (half)
    iso->register_right = reg;
  else
    iso->register_left = reg;
  *flags &= CODING_STATE_ISO2022_LOCK;
  iso->esc = ISO_ESC_LOCKING_SHIFT;
  return 1;

 designated:
  if (NILP (cs) && check_invalid_charsets)
    {
      iso->invalid_designated[reg] = 1;
      iso->charset[reg] = Vcharset_ascii;
      iso->esc = ISO_ESC_DESIGNATE;
      *flags &= CODING_STATE_ISO2022_LOCK;
      iso->output_literally = 1;
      if (iso->switched_dir_and_no_valid_charset_yet)
	{
	  /* We encountered a switch-direction followed by an
	     invalid designation.  Ensure that the switch-direction
	     gets outputted; otherwise it will probably get eaten
	     when the text is written out again. */
	  iso->switched_dir_and_no_valid_charset_yet = 0;
	  iso->output_direction_sequence = 1;
	  /* And make sure that the switch-dir going the other
	     way gets outputted, as well. */
	  iso->invalid_switch_dir = 1;
	}
      return 1;
    }
  /* This function is called with CODESYS equal to nil when
     doing coding-system detection. */
  if (!NILP (codesys))
    {
      charset_conversion_spec_dynarr *dyn =
	XCODING_SYSTEM (codesys)->iso2022.input_conv;

      if (dyn)
	{
	  int i;

	  for (i = 0; i < Dynarr_length (dyn); i++)
	    {
	      struct charset_conversion_spec *spec = Dynarr_atp (dyn, i);
	      if (EQ (cs, spec->from_charset))
		cs = spec->to_charset;
	    }
	}
    }

  iso->charset[reg] = cs;
  iso->esc = ISO_ESC_DESIGNATE;
  *flags &= CODING_STATE_ISO2022_LOCK;
  if (iso->invalid_designated[reg])
    {
      iso->invalid_designated[reg] = 0;
      iso->output_literally = 1;
    }
  if (iso->switched_dir_and_no_valid_charset_yet)
    iso->switched_dir_and_no_valid_charset_yet = 0;
  return 1;
}

static int
detect_coding_iso2022 (struct detection_state *st, const Extbyte *src, Lstream_data_count n)
{
  int mask;

  /* #### There are serious deficiencies in the recognition mechanism
     here.  This needs to be much smarter if it's going to cut it.
     The sequence "\xff\x0f" is currently detected as LOCK_SHIFT while
     it should be detected as Latin-1.
     All the ISO2022 stuff in this file should be synced up with the
     code from FSF Emacs-20.4, in which Mule should be more or less stable.
     Perhaps we should wait till R2L works in FSF Emacs? */

  if (!st->iso2022.initted)
    {
      reset_iso2022 (Qnil, &st->iso2022.iso);
      st->iso2022.mask = (CODING_CATEGORY_ISO_7_MASK |
			  CODING_CATEGORY_ISO_8_DESIGNATE_MASK |
			  CODING_CATEGORY_ISO_8_1_MASK |
			  CODING_CATEGORY_ISO_8_2_MASK |
			  CODING_CATEGORY_ISO_LOCK_SHIFT_MASK);
      st->iso2022.flags = 0;
      st->iso2022.high_byte_count = 0;
      st->iso2022.saw_single_shift = 0;
      st->iso2022.initted = 1;
    }

  mask = st->iso2022.mask;

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      if (c >= 0xA0)
	{
	  mask &= ~CODING_CATEGORY_ISO_7_MASK;
	  st->iso2022.high_byte_count++;
	}
      else
	{
	  if (st->iso2022.high_byte_count && !st->iso2022.saw_single_shift)
	    {
	      if (st->iso2022.high_byte_count & 1)
		/* odd number of high bytes; assume not iso-8-2 */
		mask &= ~CODING_CATEGORY_ISO_8_2_MASK;
	    }
	  st->iso2022.high_byte_count = 0;
	  st->iso2022.saw_single_shift = 0;
	  if (c > 0x80)
	    mask &= ~CODING_CATEGORY_ISO_7_MASK;
	}
      if (!(st->iso2022.flags & CODING_STATE_ESCAPE)
	  && (BYTE_C0_P (c) || BYTE_C1_P (c)))
	{ /* control chars */
	  switch (c)
	    {
	      /* Allow and ignore control characters that you might
		 reasonably see in a text file */
	    case '\r':
	    case '\n':
	    case '\t':
	    case  7: /* bell */
	    case  8: /* backspace */
	    case 11: /* vertical tab */
	    case 12: /* form feed */
	    case 26: /* MS-DOS C-z junk */
	    case 31: /* '^_' -- for info */
	      goto label_continue_loop;

	    default:
	      break;
	    }
	}

      if ((st->iso2022.flags & CODING_STATE_ESCAPE) || BYTE_C0_P (c)
          || BYTE_C1_P (c))
	{
	  if (parse_iso2022_esc (Qnil, &st->iso2022.iso, c,
				 &st->iso2022.flags, 0))
	    {
	      switch (st->iso2022.iso.esc)
		{
		case ISO_ESC_DESIGNATE:
		  mask &= ~CODING_CATEGORY_ISO_8_1_MASK;
		  mask &= ~CODING_CATEGORY_ISO_8_2_MASK;
		  break;
		case ISO_ESC_LOCKING_SHIFT:
		  mask = CODING_CATEGORY_ISO_LOCK_SHIFT_MASK;
		  goto ran_out_of_chars;
		case ISO_ESC_SINGLE_SHIFT:
		  mask &= ~CODING_CATEGORY_ISO_8_DESIGNATE_MASK;
		  st->iso2022.saw_single_shift = 1;
		  break;
		default:
		  break;
		}
	    }
	  else
	    {
	      mask = 0;
	      goto ran_out_of_chars;
	    }
	}
    label_continue_loop:;
    }

 ran_out_of_chars:

  return mask;
}

static int
postprocess_iso2022_mask (int mask)
{
  /* #### kind of cheesy */
  /* If seven-bit ISO is allowed, then assume that the encoding is
     entirely seven-bit and turn off the eight-bit ones. */
  if (mask & CODING_CATEGORY_ISO_7_MASK)
    mask &= ~ (CODING_CATEGORY_ISO_8_DESIGNATE_MASK |
	       CODING_CATEGORY_ISO_8_1_MASK |
	       CODING_CATEGORY_ISO_8_2_MASK);
  return mask;
}

/* If FLAGS is a null pointer or specifies right-to-left motion,
   output a switch-dir-to-left-to-right sequence to DST.
   Also update FLAGS if it is not a null pointer.
   If INTERNAL_P is set, we are outputting in internal format and
   need to handle the CSI differently. */

static void
restore_left_to_right_direction (Lisp_Coding_System *codesys,
				 unsigned_char_dynarr *dst,
				 unsigned int *flags,
				 int internal_p)
{
  if (!flags || (*flags & CODING_STATE_R2L))
    {
      if (CODING_SYSTEM_ISO2022_SEVEN (codesys))
	{
	  Dynarr_add (dst, ISO_CODE_ESC);
	  Dynarr_add (dst, '[');
	}
      else if (internal_p)
	DECODE_ADD_BINARY_CHAR (ISO_CODE_CSI, dst);
      else
	Dynarr_add (dst, ISO_CODE_CSI);
      Dynarr_add (dst, '0');
      Dynarr_add (dst, ']');
      if (flags)
	*flags &= ~CODING_STATE_R2L;
    }
}

/* If FLAGS is a null pointer or specifies a direction different from
   DIRECTION (which should be either CHARSET_RIGHT_TO_LEFT or
   CHARSET_LEFT_TO_RIGHT), output the appropriate switch-dir escape
   sequence to DST.  Also update FLAGS if it is not a null pointer.
   If INTERNAL_P is set, we are outputting in internal format and
   need to handle the CSI differently. */

static void
ensure_correct_direction (int direction, Lisp_Coding_System *codesys,
			  unsigned_char_dynarr *dst, unsigned int *flags,
			  int internal_p)
{
  if ((!flags || (*flags & CODING_STATE_R2L)) &&
      direction == CHARSET_LEFT_TO_RIGHT)
    restore_left_to_right_direction (codesys, dst, flags, internal_p);
  else if (!CODING_SYSTEM_ISO2022_NO_ISO6429 (codesys)
	   && (!flags || !(*flags & CODING_STATE_R2L)) &&
	   direction == CHARSET_RIGHT_TO_LEFT)
    {
      if (CODING_SYSTEM_ISO2022_SEVEN (codesys))
	{
	  Dynarr_add (dst, ISO_CODE_ESC);
	  Dynarr_add (dst, '[');
	}
      else if (internal_p)
	DECODE_ADD_BINARY_CHAR (ISO_CODE_CSI, dst);
      else
	Dynarr_add (dst, ISO_CODE_CSI);
      Dynarr_add (dst, '2');
      Dynarr_add (dst, ']');
      if (flags)
	*flags |= CODING_STATE_R2L;
    }
}

/* Convert ISO2022-format data to internal format. */

static void
decode_coding_iso2022 (Lstream *decoding, const Extbyte *src,
		       unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = str->eol_type;
#ifdef ENABLE_COMPOSITE_CHARS
  unsigned_char_dynarr *real_dst = dst;
#endif
  Lisp_Object coding_system;

  XSETCODING_SYSTEM (coding_system, str->codesys);

#ifdef ENABLE_COMPOSITE_CHARS
  if (flags & CODING_STATE_COMPOSITE)
    dst = str->iso2022.composite_chars;
#endif /* ENABLE_COMPOSITE_CHARS */

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;
      if (flags & CODING_STATE_ESCAPE)
	{	/* Within ESC sequence */
	  int retval = parse_iso2022_esc (coding_system, &str->iso2022,
					  c, &flags, 1);

	  if (retval)
	    {
	      switch (str->iso2022.esc)
		{
#ifdef ENABLE_COMPOSITE_CHARS
		case ISO_ESC_START_COMPOSITE:
		  if (str->iso2022.composite_chars)
		    Dynarr_reset (str->iso2022.composite_chars);
		  else
		    str->iso2022.composite_chars = Dynarr_new (unsigned_char);
		  dst = str->iso2022.composite_chars;
		  break;
		case ISO_ESC_END_COMPOSITE:
		  {
		    Bufbyte comstr[MAX_EMCHAR_LEN];
		    Bytecount len;
		    Emchar emch = lookup_composite_char (Dynarr_atp (dst, 0),
							 Dynarr_length (dst));
		    dst = real_dst;
		    len = set_charptr_emchar (comstr, emch);
		    Dynarr_add_many (dst, comstr, len);
		    break;
		  }
#endif /* ENABLE_COMPOSITE_CHARS */

		case ISO_ESC_LITERAL:
		  DECODE_ADD_BINARY_CHAR (c, dst);
		  break;

		default:
		  /* Everything else handled already */
		  break;
		}
	    }

	  /* Attempted error recovery. */
	  if (str->iso2022.output_direction_sequence)
	    ensure_correct_direction (flags & CODING_STATE_R2L ?
				      CHARSET_RIGHT_TO_LEFT :
				      CHARSET_LEFT_TO_RIGHT,
				      str->codesys, dst, 0, 1);
	  /* More error recovery. */
	  if (!retval || str->iso2022.output_literally)
	    {
	      /* Output the (possibly invalid) sequence */
	      int i;
	      for (i = 0; i < str->iso2022.esc_bytes_index; i++)
		DECODE_ADD_BINARY_CHAR (str->iso2022.esc_bytes[i], dst);
	      flags &= CODING_STATE_ISO2022_LOCK;
	      if (!retval)
		n++, src--;/* Repeat the loop with the same character. */
	      else
		{
		  /* No sense in reprocessing the final byte of the
		     escape sequence; it could mess things up anyway.
		     Just add it now. */
		  DECODE_ADD_BINARY_CHAR (c, dst);
		}
	    }
	  ch = 0;
	}
      else if (BYTE_C0_P (c) || BYTE_C1_P (c))
	{ /* Control characters */

	  /***** Error-handling *****/

	  /* If we were in the middle of a character, dump out the
	     partial character. */
	  DECODE_OUTPUT_PARTIAL_CHAR (ch);

	  /* If we just saw a single-shift character, dump it out.
	     This may dump out the wrong sort of single-shift character,
	     but least it will give an indication that something went
	     wrong. */
	  if (flags & CODING_STATE_SS2)
	    {
	      DECODE_ADD_BINARY_CHAR (ISO_CODE_SS2, dst);
	      flags &= ~CODING_STATE_SS2;
	    }
	  if (flags & CODING_STATE_SS3)
	    {
	      DECODE_ADD_BINARY_CHAR (ISO_CODE_SS3, dst);
	      flags &= ~CODING_STATE_SS3;
	    }

	  /***** Now handle the control characters. *****/

	  /* Handle CR/LF */
	  DECODE_HANDLE_EOL_TYPE (eol_type, c, flags, dst);

	  flags &= CODING_STATE_ISO2022_LOCK;

	  if (!parse_iso2022_esc (coding_system, &str->iso2022, c, &flags, 1))
	    DECODE_ADD_BINARY_CHAR (c, dst);
	}
      else
	{			/* Graphic characters */
	  Lisp_Object charset;
	  int lb;
	  int reg;

	  DECODE_HANDLE_EOL_TYPE (eol_type, c, flags, dst);

	  /* Now determine the charset. */
	  reg = ((flags & CODING_STATE_SS2) ? 2
		 : (flags & CODING_STATE_SS3) ? 3
		 : !BYTE_ASCII_P (c) ? str->iso2022.register_right
		 : str->iso2022.register_left);
	  charset = str->iso2022.charset[reg];

	  /* Error checking: */
	  if (! CHARSETP (charset)
	      || str->iso2022.invalid_designated[reg]
	      || (((c & 0x7F) == ' ' || (c & 0x7F) == ISO_CODE_DEL)
		  && XCHARSET_CHARS (charset) == 94))
	    /* Mrmph.  We are trying to invoke a register that has no
	       or an invalid charset in it, or trying to add a character
	       outside the range of the charset.  Insert that char literally
	       to preserve it for the output. */
	    {
	      DECODE_OUTPUT_PARTIAL_CHAR (ch);
	      DECODE_ADD_BINARY_CHAR (c, dst);
	    }

	  else
	    {
	      /* Things are probably hunky-dorey. */

	      /* Fetch reverse charset, maybe. */
	      if (((flags & CODING_STATE_R2L) &&
		   XCHARSET_DIRECTION (charset) == CHARSET_LEFT_TO_RIGHT)
		  ||
		  (!(flags & CODING_STATE_R2L) &&
		   XCHARSET_DIRECTION (charset) == CHARSET_RIGHT_TO_LEFT))
		{
		  Lisp_Object new_charset =
		    XCHARSET_REVERSE_DIRECTION_CHARSET (charset);
		  if (!NILP (new_charset))
		    charset = new_charset;
		}

	      lb = XCHARSET_LEADING_BYTE (charset);
	      switch (XCHARSET_REP_BYTES (charset))
		{
		case 1:	/* ASCII */
		  DECODE_OUTPUT_PARTIAL_CHAR (ch);
		  Dynarr_add (dst, c & 0x7F);
		  break;

		case 2:	/* one-byte official */
		  DECODE_OUTPUT_PARTIAL_CHAR (ch);
		  Dynarr_add (dst, lb);
		  Dynarr_add (dst, c | 0x80);
		  break;

		case 3:	/* one-byte private or two-byte official */
		  if (XCHARSET_PRIVATE_P (charset))
		    {
		      DECODE_OUTPUT_PARTIAL_CHAR (ch);
		      Dynarr_add (dst, PRE_LEADING_BYTE_PRIVATE_1);
		      Dynarr_add (dst, lb);
		      Dynarr_add (dst, c | 0x80);
		    }
		  else
		    {
		      if (ch)
			{
			  Dynarr_add (dst, lb);
			  Dynarr_add (dst, ch | 0x80);
			  Dynarr_add (dst, c | 0x80);
			  ch = 0;
			}
		      else
			ch = c;
		    }
		  break;

		default:	/* two-byte private */
		  if (ch)
		    {
		      Dynarr_add (dst, PRE_LEADING_BYTE_PRIVATE_2);
		      Dynarr_add (dst, lb);
		      Dynarr_add (dst, ch | 0x80);
		      Dynarr_add (dst, c | 0x80);
		      ch = 0;
		    }
		  else
		    ch = c;
		}
	    }

	  if (!ch)
	    flags &= CODING_STATE_ISO2022_LOCK;
	}

    label_continue_loop:;
    }

  if (flags & CODING_STATE_END)
    DECODE_OUTPUT_PARTIAL_CHAR (ch);

  str->flags = flags;
  str->ch    = ch;
}


/***** ISO2022 encoder *****/

/* Designate CHARSET into register REG. */

static void
iso2022_designate (Lisp_Object charset, unsigned char reg,
		   struct encoding_stream *str, unsigned_char_dynarr *dst)
{
  static const char inter94[] = "()*+";
  static const char inter96[] = ",-./";
  unsigned int type;
  unsigned char final;
  Lisp_Object old_charset = str->iso2022.charset[reg];

  str->iso2022.charset[reg] = charset;
  if (!CHARSETP (charset))
    /* charset might be an initial nil or t. */
    return;
  type = XCHARSET_TYPE (charset);
  final = XCHARSET_FINAL (charset);
  if (!str->iso2022.force_charset_on_output[reg] &&
      CHARSETP (old_charset) &&
      XCHARSET_TYPE (old_charset) == type &&
      XCHARSET_FINAL (old_charset) == final)
    return;

  str->iso2022.force_charset_on_output[reg] = 0;

  {
    charset_conversion_spec_dynarr *dyn =
      str->codesys->iso2022.output_conv;

    if (dyn)
      {
	int i;

	for (i = 0; i < Dynarr_length (dyn); i++)
	  {
	    struct charset_conversion_spec *spec = Dynarr_atp (dyn, i);
	    if (EQ (charset, spec->from_charset))
		charset = spec->to_charset;
	  }
      }
  }

  Dynarr_add (dst, ISO_CODE_ESC);
  switch (type)
    {
    case CHARSET_TYPE_94:
      Dynarr_add (dst, inter94[reg]);
      break;
    case CHARSET_TYPE_96:
      Dynarr_add (dst, inter96[reg]);
      break;
    case CHARSET_TYPE_94X94:
      Dynarr_add (dst, '$');
      if (reg != 0
	  || !(CODING_SYSTEM_ISO2022_SHORT (str->codesys))
	  || final < '@'
	  || final > 'B')
	Dynarr_add (dst, inter94[reg]);
      break;
    case CHARSET_TYPE_96X96:
      Dynarr_add (dst, '$');
      Dynarr_add (dst, inter96[reg]);
      break;
    }
  Dynarr_add (dst, final);
}

static void
ensure_normal_shift (struct encoding_stream *str, unsigned_char_dynarr *dst)
{
  if (str->iso2022.register_left != 0)
    {
      Dynarr_add (dst, ISO_CODE_SI);
      str->iso2022.register_left = 0;
    }
}

static void
ensure_shift_out (struct encoding_stream *str, unsigned_char_dynarr *dst)
{
  if (str->iso2022.register_left != 1)
    {
      Dynarr_add (dst, ISO_CODE_SO);
      str->iso2022.register_left = 1;
    }
}

/* Convert internally-formatted data to ISO2022 format. */

static void
encode_coding_iso2022 (Lstream *encoding, const Bufbyte *src,
		       unsigned_char_dynarr *dst, Lstream_data_count n)
{
  unsigned char charmask, c;
  unsigned char char_boundary;
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);
  unsigned int flags          = str->flags;
  unsigned int ch             = str->ch;
  Lisp_Coding_System *codesys = str->codesys;
  eol_type_t eol_type         = CODING_SYSTEM_EOL_TYPE (str->codesys);
  int i;
  Lisp_Object charset;
  int half;

#ifdef ENABLE_COMPOSITE_CHARS
  /* flags for handling composite chars.  We do a little switcharoo
     on the source while we're outputting the composite char. */
  unsigned int saved_n = 0;
  const unsigned char *saved_src = NULL;
  int in_composite = 0;
#endif /* ENABLE_COMPOSITE_CHARS */

  char_boundary = str->iso2022.current_char_boundary;
  charset = str->iso2022.current_charset;
  half = str->iso2022.current_half;

#ifdef ENABLE_COMPOSITE_CHARS
 back_to_square_n:
#endif
  while (n--)
    {
      c = *src++;

      if (BYTE_ASCII_P (c))
	{		/* Processing ASCII character */
	  ch = 0;

	  restore_left_to_right_direction (codesys, dst, &flags, 0);

	  /* Make sure G0 contains ASCII */
	  if ((c > ' ' && c < ISO_CODE_DEL) ||
	      !CODING_SYSTEM_ISO2022_NO_ASCII_CNTL (codesys))
	    {
	      ensure_normal_shift (str, dst);
	      iso2022_designate (Vcharset_ascii, 0, str, dst);
	    }

	  /* If necessary, restore everything to the default state
	     at end-of-line */
	  if (c == '\n' &&
	      !(CODING_SYSTEM_ISO2022_NO_ASCII_EOL (codesys)))
	    {
	      restore_left_to_right_direction (codesys, dst, &flags, 0);

	      ensure_normal_shift (str, dst);

	      for (i = 0; i < 4; i++)
		{
		  Lisp_Object initial_charset =
		    CODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i);
		  iso2022_designate (initial_charset, i, str, dst);
		}
	    }
	  if (c == '\n')
	    {
	      if (eol_type != EOL_LF && eol_type != EOL_AUTODETECT)
		Dynarr_add (dst, '\r');
	      if (eol_type != EOL_CR)
		Dynarr_add (dst, c);
	    }
	  else
	    {
	      if (CODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
		  && fit_to_be_escape_quoted (c))
		Dynarr_add (dst, ISO_CODE_ESC);
	      Dynarr_add (dst, c);
	    }
	  char_boundary = 1;
	}

      else if (BUFBYTE_LEADING_BYTE_P (c) || BUFBYTE_LEADING_BYTE_P (ch))
	{ /* Processing Leading Byte */
	  ch = 0;
	  charset = CHARSET_BY_LEADING_BYTE (c);
	  if (LEADING_BYTE_PREFIX_P(c))
	    ch = c;
	  else if (!EQ (charset, Vcharset_control_1)
#ifdef ENABLE_COMPOSITE_CHARS
		   && !EQ (charset, Vcharset_composite)
#endif
		   )
	    {
	      int reg;

	      ensure_correct_direction (XCHARSET_DIRECTION (charset),
					codesys, dst, &flags, 0);

	      /* Now determine which register to use. */
	      reg = -1;
	      for (i = 0; i < 4; i++)
		{
		  if (EQ (charset, str->iso2022.charset[i]) ||
		      EQ (charset,
			  CODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i)))
		    {
		      reg = i;
		      break;
		    }
		}

	      if (reg == -1)
		{
		  if (XCHARSET_GRAPHIC (charset) != 0)
		    {
		      if (!NILP (str->iso2022.charset[1]) &&
			  (!CODING_SYSTEM_ISO2022_SEVEN (codesys) ||
			   CODING_SYSTEM_ISO2022_LOCK_SHIFT (codesys)))
			reg = 1;
		      else if (!NILP (str->iso2022.charset[2]))
			reg = 2;
		      else if (!NILP (str->iso2022.charset[3]))
			reg = 3;
		      else
			reg = 0;
		    }
		  else
		    reg = 0;
		}

	      iso2022_designate (charset, reg, str, dst);

	      /* Now invoke that register. */
	      switch (reg)
		{
		case 0:
		  ensure_normal_shift (str, dst);
		  half = 0;
		  break;

		case 1:
		  if (CODING_SYSTEM_ISO2022_SEVEN (codesys))
		    {
		      ensure_shift_out (str, dst);
		      half = 0;
		    }
		  else
		    half = 1;
		  break;

		case 2:
		  if (CODING_SYSTEM_ISO2022_SEVEN (str->codesys))
		    {
		      Dynarr_add (dst, ISO_CODE_ESC);
		      Dynarr_add (dst, 'N');
		      half = 0;
		    }
		  else
		    {
		      Dynarr_add (dst, ISO_CODE_SS2);
		      half = 1;
		    }
		  break;

		case 3:
		  if (CODING_SYSTEM_ISO2022_SEVEN (str->codesys))
		    {
		      Dynarr_add (dst, ISO_CODE_ESC);
		      Dynarr_add (dst, 'O');
		      half = 0;
		    }
		  else
		    {
		      Dynarr_add (dst, ISO_CODE_SS3);
		      half = 1;
		    }
		  break;

		default:
		  abort ();
		}
	    }
	  char_boundary = 0;
	}
      else
	{			/* Processing Non-ASCII character */
	  charmask = (half == 0 ? 0x7F : 0xFF);
	  char_boundary = 1;
	  if (EQ (charset, Vcharset_control_1))
	    {
	      if (CODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
		  && fit_to_be_escape_quoted (c))
		Dynarr_add (dst, ISO_CODE_ESC);
	      /* you asked for it ... */
	      Dynarr_add (dst, c - 0x20);
	    }
	  else
	    {
	      switch (XCHARSET_REP_BYTES (charset))
		{
		case 2:
		  Dynarr_add (dst, c & charmask);
		  break;
		case 3:
		  if (XCHARSET_PRIVATE_P (charset))
		    {
		      Dynarr_add (dst, c & charmask);
		      ch = 0;
		    }
		  else if (ch)
		    {
#ifdef ENABLE_COMPOSITE_CHARS
		      if (EQ (charset, Vcharset_composite))
			{
			  if (in_composite)
			    {
			      /* #### Bother! We don't know how to
				 handle this yet. */
			      Dynarr_add (dst, '~');
			    }
			  else
			    {
			      Emchar emch = MAKE_CHAR (Vcharset_composite,
						       ch & 0x7F, c & 0x7F);
			      Lisp_Object lstr = composite_char_string (emch);
			      saved_n = n;
			      saved_src = src;
			      in_composite = 1;
			      src = XSTRING_DATA   (lstr);
			      n   = XSTRING_LENGTH (lstr);
			      Dynarr_add (dst, ISO_CODE_ESC);
			      Dynarr_add (dst, '0'); /* start composing */
			    }
			}
		      else
#endif /* ENABLE_COMPOSITE_CHARS */
			{
			  Dynarr_add (dst, ch & charmask);
			  Dynarr_add (dst, c & charmask);
			}
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		case 4:
		  if (ch)
		    {
		      Dynarr_add (dst, ch & charmask);
		      Dynarr_add (dst, c & charmask);
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		default:
		  abort ();
		}
	    }
	}
    }

#ifdef ENABLE_COMPOSITE_CHARS
  if (in_composite)
    {
      n = saved_n;
      src = saved_src;
      in_composite = 0;
      Dynarr_add (dst, ISO_CODE_ESC);
      Dynarr_add (dst, '1'); /* end composing */
      goto back_to_square_n; /* Wheeeeeeeee ..... */
    }
#endif /* ENABLE_COMPOSITE_CHARS */

  if (char_boundary && flags & CODING_STATE_END)
    {
      restore_left_to_right_direction (codesys, dst, &flags, 0);
      ensure_normal_shift (str, dst);
      for (i = 0; i < 4; i++)
	{
	  Lisp_Object initial_charset =
	    CODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i);
	  iso2022_designate (initial_charset, i, str, dst);
	}
    }

  str->flags = flags;
  str->ch    = ch;
  str->iso2022.current_char_boundary = char_boundary;
  str->iso2022.current_charset = charset;
  str->iso2022.current_half = half;

  /* Verbum caro factum est! */
}
#endif /* MULE */

/************************************************************************/
/*                     No-conversion methods                            */
/************************************************************************/

/* This is used when reading in "binary" files -- i.e. files that may
   contain all 256 possible byte values and that are not to be
   interpreted as being in any particular decoding. */
static void
decode_coding_no_conversion (Lstream *decoding, const Extbyte *src,
			     unsigned_char_dynarr *dst, Lstream_data_count n)
{
  struct decoding_stream *str = DECODING_STREAM_DATA (decoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = str->eol_type;

  while (n--)
    {
      unsigned char c = *(unsigned char *)src++;

      DECODE_HANDLE_EOL_TYPE (eol_type, c, flags, dst);
      DECODE_ADD_BINARY_CHAR (c, dst);
    label_continue_loop:;
    }

  DECODE_HANDLE_END_OF_CONVERSION (flags, ch, dst);

  str->flags = flags;
  str->ch    = ch;
}

static void
encode_coding_no_conversion (Lstream *encoding, const Bufbyte *src,
			     unsigned_char_dynarr *dst, Lstream_data_count n)
{
  unsigned char c;
  struct encoding_stream *str = ENCODING_STREAM_DATA (encoding);
  unsigned int flags  = str->flags;
  unsigned int ch     = str->ch;
  eol_type_t eol_type = CODING_SYSTEM_EOL_TYPE (str->codesys);

  while (n--)
    {
      c = *src++;
      if (c == '\n')
	{
	  if (eol_type != EOL_LF && eol_type != EOL_AUTODETECT)
	    Dynarr_add (dst, '\r');
	  if (eol_type != EOL_CR)
	    Dynarr_add (dst, '\n');
	  ch = 0;
	}
      else if (BYTE_ASCII_P (c))
	{
	  assert (ch == 0);
	  Dynarr_add (dst, c);
	}
      else if (BUFBYTE_LEADING_BYTE_P (c))
	{
	  assert (ch == 0);
	  if (c == LEADING_BYTE_LATIN_ISO8859_1 ||
	      c == LEADING_BYTE_CONTROL_1)
	    ch = c;
	  else
	    Dynarr_add (dst, '~'); /* untranslatable character */
	}
      else
	{
	  if (ch == LEADING_BYTE_LATIN_ISO8859_1)
	    Dynarr_add (dst, c);
	  else if (ch == LEADING_BYTE_CONTROL_1)
	    {
	      assert (c < 0xC0);
	      Dynarr_add (dst, c - 0x20);
	    }
	  /* else it should be the second or third byte of an
	     untranslatable character, so ignore it */
	  ch = 0;
	}
    }

  str->flags = flags;
  str->ch    = ch;
}



/************************************************************************/
/*                             Initialization                           */
/************************************************************************/

void
syms_of_file_coding (void)
{
  INIT_LRECORD_IMPLEMENTATION (coding_system);

  DEFERROR_STANDARD (Qcoding_system_error, Qio_error);

  DEFSUBR (Fcoding_system_p);
  DEFSUBR (Ffind_coding_system);
  DEFSUBR (Fget_coding_system);
  DEFSUBR (Fcoding_system_list);
  DEFSUBR (Fcoding_system_name);
  DEFSUBR (Fmake_coding_system);
  DEFSUBR (Fcopy_coding_system);
  DEFSUBR (Fcoding_system_canonical_name_p);
  DEFSUBR (Fcoding_system_alias_p);
  DEFSUBR (Fcoding_system_aliasee);
  DEFSUBR (Fdefine_coding_system_alias);
  DEFSUBR (Fsubsidiary_coding_system);

  DEFSUBR (Fcoding_system_type);
  DEFSUBR (Fcoding_system_doc_string);
#ifdef MULE
  DEFSUBR (Fcoding_system_charset);
#endif
  DEFSUBR (Fcoding_system_property);

  DEFSUBR (Fcoding_category_list);
  DEFSUBR (Fset_coding_priority_list);
  DEFSUBR (Fcoding_priority_list);
  DEFSUBR (Fset_coding_category_system);
  DEFSUBR (Fcoding_category_system);

  DEFSUBR (Fdetect_coding_region);
  DEFSUBR (Fdecode_coding_region);
  DEFSUBR (Fencode_coding_region);
#ifdef MULE
  DEFSUBR (Fdecode_shift_jis_char);
  DEFSUBR (Fencode_shift_jis_char);
  DEFSUBR (Fdecode_big5_char);
  DEFSUBR (Fencode_big5_char);
  DEFSUBR (Fset_ucs_char);
  DEFSUBR (Fucs_char);
  DEFSUBR (Fset_char_ucs);
  DEFSUBR (Fchar_ucs);
#endif /* MULE */
  defsymbol (&Qcoding_systemp, "coding-system-p");
  defsymbol (&Qno_conversion, "no-conversion");
  defsymbol (&Qraw_text, "raw-text");
#ifdef MULE
  defsymbol (&Qbig5, "big5");
  defsymbol (&Qshift_jis, "shift-jis");
  defsymbol (&Qucs4, "ucs-4");
  defsymbol (&Qutf8, "utf-8");
  defsymbol (&Qccl, "ccl");
  defsymbol (&Qiso2022, "iso2022");
#endif /* MULE */
  defsymbol (&Qmnemonic, "mnemonic");
  defsymbol (&Qeol_type, "eol-type");
  defsymbol (&Qpost_read_conversion, "post-read-conversion");
  defsymbol (&Qpre_write_conversion, "pre-write-conversion");

  defsymbol (&Qcr, "cr");
  defsymbol (&Qlf, "lf");
  defsymbol (&Qcrlf, "crlf");
  defsymbol (&Qeol_cr, "eol-cr");
  defsymbol (&Qeol_lf, "eol-lf");
  defsymbol (&Qeol_crlf, "eol-crlf");
#ifdef MULE
  defsymbol (&Qcharset_g0, "charset-g0");
  defsymbol (&Qcharset_g1, "charset-g1");
  defsymbol (&Qcharset_g2, "charset-g2");
  defsymbol (&Qcharset_g3, "charset-g3");
  defsymbol (&Qforce_g0_on_output, "force-g0-on-output");
  defsymbol (&Qforce_g1_on_output, "force-g1-on-output");
  defsymbol (&Qforce_g2_on_output, "force-g2-on-output");
  defsymbol (&Qforce_g3_on_output, "force-g3-on-output");
  defsymbol (&Qno_iso6429, "no-iso6429");
  defsymbol (&Qinput_charset_conversion, "input-charset-conversion");
  defsymbol (&Qoutput_charset_conversion, "output-charset-conversion");

  defsymbol (&Qshort, "short");
  defsymbol (&Qno_ascii_eol, "no-ascii-eol");
  defsymbol (&Qno_ascii_cntl, "no-ascii-cntl");
  defsymbol (&Qseven, "seven");
  defsymbol (&Qlock_shift, "lock-shift");
  defsymbol (&Qescape_quoted, "escape-quoted");
#endif /* MULE */
  defsymbol (&Qencode, "encode");
  defsymbol (&Qdecode, "decode");

#ifdef MULE
  defsymbol (&coding_category_symbol[CODING_CATEGORY_SHIFT_JIS],
	     "shift-jis");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_BIG5],
	     "big5");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_UCS4],
	     "ucs-4");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_UTF8],
	     "utf-8");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_ISO_7],
	     "iso-7");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_ISO_8_DESIGNATE],
	     "iso-8-designate");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_ISO_8_1],
	     "iso-8-1");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_ISO_8_2],
	     "iso-8-2");
  defsymbol (&coding_category_symbol[CODING_CATEGORY_ISO_LOCK_SHIFT],
	     "iso-lock-shift");
#endif /* MULE */
  defsymbol (&coding_category_symbol[CODING_CATEGORY_NO_CONVERSION],
	     "no-conversion");
}

void
lstream_type_create_file_coding (void)
{
  LSTREAM_HAS_METHOD (decoding, reader);
  LSTREAM_HAS_METHOD (decoding, writer);
  LSTREAM_HAS_METHOD (decoding, rewinder);
  LSTREAM_HAS_METHOD (decoding, seekable_p);
  LSTREAM_HAS_METHOD (decoding, flusher);
  LSTREAM_HAS_METHOD (decoding, closer);
  LSTREAM_HAS_METHOD (decoding, marker);

  LSTREAM_HAS_METHOD (encoding, reader);
  LSTREAM_HAS_METHOD (encoding, writer);
  LSTREAM_HAS_METHOD (encoding, rewinder);
  LSTREAM_HAS_METHOD (encoding, seekable_p);
  LSTREAM_HAS_METHOD (encoding, flusher);
  LSTREAM_HAS_METHOD (encoding, closer);
  LSTREAM_HAS_METHOD (encoding, marker);
}

void
vars_of_file_coding (void)
{
  int i;

  fcd = xnew (struct file_coding_dump);
  dump_add_root_struct_ptr (&fcd, &fcd_description);

  /* Initialize to something reasonable ... */
  for (i = 0; i < CODING_CATEGORY_LAST; i++)
    {
      fcd->coding_category_system[i] = Qnil;
      fcd->coding_category_by_priority[i] = i;
    }

  Fprovide (intern ("file-coding"));

  DEFVAR_LISP ("keyboard-coding-system", &Vkeyboard_coding_system /*
Coding system used for TTY keyboard input.
Not used under a windowing system.
*/ );
  Vkeyboard_coding_system = Qnil;

  DEFVAR_LISP ("terminal-coding-system", &Vterminal_coding_system /*
Coding system used for TTY display output.
Not used under a windowing system.
*/ );
  Vterminal_coding_system = Qnil;

  DEFVAR_LISP ("coding-system-for-read", &Vcoding_system_for_read /*
Overriding coding system used when reading from a file or process.
You should bind this variable with `let', but do not set it globally.
If this is non-nil, it specifies the coding system that will be used
to decode input on read operations, such as from a file or process.
It overrides `buffer-file-coding-system-for-read',
`insert-file-contents-pre-hook', etc.  Use those variables instead of
this one for permanent changes to the environment.  */ );
  Vcoding_system_for_read = Qnil;

  DEFVAR_LISP ("coding-system-for-write",
               &Vcoding_system_for_write /*
Overriding coding system used when writing to a file or process.
You should bind this variable with `let', but do not set it globally.
If this is non-nil, it specifies the coding system that will be used
to encode output for write operations, such as to a file or process.
It overrides `buffer-file-coding-system', `write-region-pre-hook', etc.
Use those variables instead of this one for permanent changes to the
environment.  */ );
  Vcoding_system_for_write = Qnil;

  DEFVAR_LISP ("file-name-coding-system", &Vfile_name_coding_system /*
Coding system used to convert pathnames when accessing files.
*/ );
  Vfile_name_coding_system = Qnil;

  DEFVAR_BOOL ("enable-multibyte-characters", &enable_multibyte_characters /*
Non-nil means the buffer contents are regarded as multi-byte form
of characters, not a binary code.  This affects the display, file I/O,
and behaviors of various editing commands.

Setting this to nil does not do anything.
*/ );
  enable_multibyte_characters = 1;
}

void
complex_vars_of_file_coding (void)
{
  staticpro (&Vcoding_system_hash_table);
  Vcoding_system_hash_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);

  the_codesys_prop_dynarr = Dynarr_new (codesys_prop);
  dump_add_root_struct_ptr (&the_codesys_prop_dynarr, &codesys_prop_dynarr_description);

#define DEFINE_CODESYS_PROP(Prop_Type, Sym) do	\
{						\
  struct codesys_prop csp;			\
  csp.sym = (Sym);				\
  csp.prop_type = (Prop_Type);			\
  Dynarr_add (the_codesys_prop_dynarr, csp);	\
} while (0)

  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qmnemonic);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qeol_type);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qeol_cr);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qeol_crlf);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qeol_lf);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qpost_read_conversion);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ALL_OK,  Qpre_write_conversion);
#ifdef MULE
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qcharset_g0);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qcharset_g1);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qcharset_g2);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qcharset_g3);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qforce_g0_on_output);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qforce_g1_on_output);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qforce_g2_on_output);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qforce_g3_on_output);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qshort);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qno_ascii_eol);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qno_ascii_cntl);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qseven);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qlock_shift);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qno_iso6429);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qescape_quoted);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qinput_charset_conversion);
  DEFINE_CODESYS_PROP (CODESYS_PROP_ISO2022, Qoutput_charset_conversion);

  DEFINE_CODESYS_PROP (CODESYS_PROP_CCL,     Qencode);
  DEFINE_CODESYS_PROP (CODESYS_PROP_CCL,     Qdecode);
#endif /* MULE */
  /* Need to create this here or we're really screwed. */
  Fmake_coding_system
    (Qraw_text, Qno_conversion,
     build_string ("Raw text, which means it converts only line-break-codes."),
     list2 (Qmnemonic, build_string ("Raw")));

  Fmake_coding_system
    (Qbinary, Qno_conversion,
     build_string ("Binary, which means it does not convert anything."),
     list4 (Qeol_type, Qlf,
	    Qmnemonic, build_string ("Binary")));

  Fdefine_coding_system_alias (Qno_conversion, Qraw_text);

  Fdefine_coding_system_alias (Qfile_name, Qbinary);

  Fdefine_coding_system_alias (Qterminal, Qbinary);
  Fdefine_coding_system_alias (Qkeyboard, Qbinary);

  /* Need this for bootstrapping */
  fcd->coding_category_system[CODING_CATEGORY_NO_CONVERSION] =
    Fget_coding_system (Qraw_text);

#ifdef MULE
  {
    size_t i;

    for (i = 0; i < countof (fcd->ucs_to_mule_table); i++)
      fcd->ucs_to_mule_table[i] = Qnil;
  }
  staticpro (&mule_to_ucs_table);
  mule_to_ucs_table = Fmake_char_table(Qgeneric);
#endif /* MULE */
}
