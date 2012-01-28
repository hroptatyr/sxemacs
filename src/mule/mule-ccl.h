/* Header for CCL (Code Conversion Language) interpreter.
   Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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


#ifndef INCLUDED_mule_ccl_h_
#define INCLUDED_mule_ccl_h_

/* Macros for exit status of CCL program.  */
#define CCL_STAT_SUCCESS	0	/* Terminated successfully.  */
#define CCL_STAT_SUSPEND_BY_SRC	1	/* Terminated by empty input.  */
#define CCL_STAT_SUSPEND_BY_DST	2	/* Terminated by output buffer full.  */
#define CCL_STAT_INVALID_CMD	3	/* Terminated because of invalid
					   command.  */
#define CCL_STAT_QUIT		4	/* Terminated because of quit.  */

/* Structure to hold information about running CCL code.  Read
   comments in the file ccl.c for the detail of each field.  */
struct ccl_program {
	int size;		/* Size of the compiled code.  */
	Lisp_Object *prog;	/* Pointer into the compiled code.  */
	int ic;			/* Instruction Counter (index for PROG).  */
	int eof_ic;		/* Instruction Counter for end-of-file
				   processing code.  */
	int reg[8];		/* CCL registers, reg[7] is used for
				   condition flag of relational
				   operations.  */
	int private_state;	/* CCL instruction may use this
				   for private use, mainly for saving
				   internal states on suspending.
				   This variable is set to 0 when ccl is
				   set up.  */
	int last_block;		/* Set to 1 while processing the last
				   block. */
	int status;		/* Exit status of the CCL program.  */
	int buf_magnification;	/* Output buffer magnification.  How
				   many times bigger the output buffer
				   should be than the input buffer.  */
	int stack_idx;		/* How deep the call of CCL_Call is nested.  */
	int eol_type;		/* When the CCL program is used for
				   encoding by a coding system, set to
				   the eol_type of the coding
				   system.  */
	int multibyte;		/* 1 if the source text is multibyte.  */
};

#define CCL_MODE_ENCODING 0
#define CCL_MODE_DECODING 1

#define CCL_CODING_EOL_LF	0	/* Line-feed only, same as Emacs'
					   internal format.  */
#define CCL_CODING_EOL_CRLF	1	/* Sequence of carriage-return and
					   line-feed.  */
#define CCL_CODING_EOL_CR	2	/* Carriage-return only.  */

/* Alist of fontname patterns vs corresponding CCL program.  */
extern Lisp_Object Vfont_ccl_encoder_alist;

/* Setup fields of the structure pointed by CCL appropriately for the
   execution of ccl program CCL_PROG (symbol or vector).  */
extern int setup_ccl_program(struct ccl_program *, Lisp_Object);

extern int ccl_driver(struct ccl_program *, const unsigned char *,
		      unsigned_char_dynarr *, int, int *, int);

EXFUN(Fregister_ccl_program, 2);

extern Lisp_Object Qccl_program;

/* Vector of CCL program names vs corresponding program data.  */
extern Lisp_Object Vccl_program_table;

/* Symbols of ccl program have this property, a value of the property
   is an index for Vccl_program_table. */
extern Lisp_Object Qccl_program_idx;

#endif				/* INCLUDED_mule_ccl_h_ */
