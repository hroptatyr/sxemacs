/* CCL (Code Conversion Language) interpreter.
   Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
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


/* Synched up with : FSF Emacs 21.0.90 except TranslateCharacter */

#ifdef emacs
#include <config.h>
#endif

#include <stdio.h>

#ifdef emacs

#include "lisp.h"
#include "buffer.h"
#include "mule-charset.h"
#include "mule-ccl.h"
#include "file-coding.h"

#else				/* not emacs */

#include "mulelib.h"

#endif				/* not emacs */

/* This contains all code conversion map available to CCL.  */
Lisp_Object Vcode_conversion_map_vector;

/* Alist of fontname patterns vs corresponding CCL program.  */
Lisp_Object Vfont_ccl_encoder_alist;

/* This symbol is a property which associates with ccl program vector.
   Ex: (get 'ccl-big5-encoder 'ccl-program) returns ccl program vector.  */
Lisp_Object Qccl_program;

/* These symbols are properties which associate with code conversion
   map and their ID respectively.  */
Lisp_Object Qcode_conversion_map;
Lisp_Object Qcode_conversion_map_id;

/* Symbols of ccl program have this property, a value of the property
   is an index for Vccl_program_table. */
Lisp_Object Qccl_program_idx;

/* Table of registered CCL programs.  Each element is a vector of
   NAME, CCL_PROG, and RESOLVEDP where NAME (symbol) is the name of
   the program, CCL_PROG (vector) is the compiled code of the program,
   RESOLVEDP (t or nil) is the flag to tell if symbols in CCL_PROG is
   already resolved to index numbers or not.  */
Lisp_Object Vccl_program_table;

/* CCL (Code Conversion Language) is a simple language which has
   operations on one input buffer, one output buffer, and 7 registers.
   The syntax of CCL is described in `ccl.el'.  Emacs Lisp function
   `ccl-compile' compiles a CCL program and produces a CCL code which
   is a vector of integers.  The structure of this vector is as
   follows: The 1st element: buffer-magnification, a factor for the
   size of output buffer compared with the size of input buffer.  The
   2nd element: address of CCL code to be executed when encountered
   with end of input stream.  The 3rd and the remaining elements: CCL
   codes.  */

/* Header of CCL compiled code */
#define CCL_HEADER_BUF_MAG	0
#define CCL_HEADER_EOF		1
#define CCL_HEADER_MAIN		2

/* CCL code is a sequence of 28-bit non-negative integers (i.e. the
   MSB is always 0), each contains CCL command and/or arguments in the
   following format:

	|----------------- integer (28-bit) ------------------|
	|------- 17-bit ------|- 3-bit --|- 3-bit --|- 5-bit -|
	|--constant argument--|-register-|-register-|-command-|
	   ccccccccccccccccc      RRR        rrr       XXXXX
  or
	|------- relative address -------|-register-|-command-|
	       cccccccccccccccccccc          rrr       XXXXX
  or
	|------------- constant or other args ----------------|
                     cccccccccccccccccccccccccccc

   where, `cc...c' is a non-negative integer indicating constant value
   (the left most `c' is always 0) or an absolute jump address, `RRR'
   and `rrr' are CCL register number, `XXXXX' is one of the following
   CCL commands.  */

/* CCL commands

   Each comment fields shows one or more lines for command syntax and
   the following lines for semantics of the command.  In semantics, IC
   stands for Instruction Counter.  */

#define CCL_SetRegister		0x00	/* Set register a register value:
					   1:00000000000000000RRRrrrXXXXX
					   ------------------------------
					   reg[rrr] = reg[RRR];
					 */

#define CCL_SetShortConst	0x01	/* Set register a short constant value:
					   1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   ------------------------------
					   reg[rrr] = CCCCCCCCCCCCCCCCCCC;
					 */

#define CCL_SetConst		0x02	/* Set register a constant value:
					   1:00000000000000000000rrrXXXXX
					   2:CONSTANT
					   ------------------------------
					   reg[rrr] = CONSTANT;
					   IC++;
					 */

#define CCL_SetArray		0x03	/* Set register an element of array:
					   1:CCCCCCCCCCCCCCCCCRRRrrrXXXXX
					   2:ELEMENT[0]
					   3:ELEMENT[1]
					   ...
					   ------------------------------
					   if (0 <= reg[RRR] < CC..C)
					   reg[rrr] = ELEMENT[reg[RRR]];
					   IC += CC..C;
					 */

#define CCL_Jump		0x04	/* Jump:
					   1:A--D--D--R--E--S--S-000XXXXX
					   ------------------------------
					   IC += ADDRESS;
					 */

/* Note: If CC..C is greater than 0, the second code is omitted.  */

#define CCL_JumpCond		0x05	/* Jump conditional:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   ------------------------------
					   if (!reg[rrr])
					   IC += ADDRESS;
					 */

#define CCL_WriteRegisterJump	0x06	/* Write register and jump:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   ------------------------------
					   write (reg[rrr]);
					   IC += ADDRESS;
					 */

#define CCL_WriteRegisterReadJump 0x07	/* Write register, read, and jump:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:A--D--D--R--E--S--S-rrrYYYYY
					   -----------------------------
					   write (reg[rrr]);
					   IC++;
					   read (reg[rrr]);
					   IC += ADDRESS;
					 */
/* Note: If read is suspended, the resumed execution starts from the
   second code (YYYYY == CCL_ReadJump).  */

#define CCL_WriteConstJump	0x08	/* Write constant and jump:
					   1:A--D--D--R--E--S--S-000XXXXX
					   2:CONST
					   ------------------------------
					   write (CONST);
					   IC += ADDRESS;
					 */

#define CCL_WriteConstReadJump	0x09	/* Write constant, read, and jump:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:CONST
					   3:A--D--D--R--E--S--S-rrrYYYYY
					   -----------------------------
					   write (CONST);
					   IC += 2;
					   read (reg[rrr]);
					   IC += ADDRESS;
					 */
/* Note: If read is suspended, the resumed execution starts from the
   second code (YYYYY == CCL_ReadJump).  */

#define CCL_WriteStringJump	0x0A	/* Write string and jump:
					   1:A--D--D--R--E--S--S-000XXXXX
					   2:LENGTH
					   3:0000STRIN[0]STRIN[1]STRIN[2]
					   ...
					   ------------------------------
					   write_string (STRING, LENGTH);
					   IC += ADDRESS;
					 */

#define CCL_WriteArrayReadJump	0x0B	/* Write an array element, read, and jump:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:LENGTH
					   3:ELEMENET[0]
					   4:ELEMENET[1]
					   ...
					   N:A--D--D--R--E--S--S-rrrYYYYY
					   ------------------------------
					   if (0 <= reg[rrr] < LENGTH)
					   write (ELEMENT[reg[rrr]]);
					   IC += LENGTH + 2; (... pointing at N+1)
					   read (reg[rrr]);
					   IC += ADDRESS;
					 */
/* Note: If read is suspended, the resumed execution starts from the
   Nth code (YYYYY == CCL_ReadJump).  */

#define CCL_ReadJump		0x0C	/* Read and jump:
					   1:A--D--D--R--E--S--S-rrrYYYYY
					   -----------------------------
					   read (reg[rrr]);
					   IC += ADDRESS;
					 */

#define CCL_Branch		0x0D	/* Jump by branch table:
					   1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   2:A--D--D--R--E-S-S[0]000XXXXX
					   3:A--D--D--R--E-S-S[1]000XXXXX
					   ...
					   ------------------------------
					   if (0 <= reg[rrr] < CC..C)
					   IC += ADDRESS[reg[rrr]];
					   else
					   IC += ADDRESS[CC..C];
					 */

#define CCL_ReadRegister	0x0E	/* Read bytes into registers:
					   1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   2:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   ...
					   ------------------------------
					   while (CCC--)
					   read (reg[rrr]);
					 */

#define CCL_WriteExprConst	0x0F	/* write result of expression:
					   1:00000OPERATION000RRR000XXXXX
					   2:CONSTANT
					   ------------------------------
					   write (reg[RRR] OPERATION CONSTANT);
					   IC++;
					 */

/* Note: If the Nth read is suspended, the resumed execution starts
   from the Nth code.  */

#define CCL_ReadBranch		0x10	/* Read one byte into a register,
					   and jump by branch table:
					   1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   2:A--D--D--R--E-S-S[0]000XXXXX
					   3:A--D--D--R--E-S-S[1]000XXXXX
					   ...
					   ------------------------------
					   read (read[rrr]);
					   if (0 <= reg[rrr] < CC..C)
					   IC += ADDRESS[reg[rrr]];
					   else
					   IC += ADDRESS[CC..C];
					 */

#define CCL_WriteRegister	0x11	/* Write registers:
					   1:CCCCCCCCCCCCCCCCCCCrrrXXXXX
					   2:CCCCCCCCCCCCCCCCCCCrrrXXXXX
					   ...
					   ------------------------------
					   while (CCC--)
					   write (reg[rrr]);
					   ...
					 */

/* Note: If the Nth write is suspended, the resumed execution
   starts from the Nth code.  */

#define CCL_WriteExprRegister	0x12	/* Write result of expression
					   1:00000OPERATIONRrrRRR000XXXXX
					   ------------------------------
					   write (reg[RRR] OPERATION reg[Rrr]);
					 */

#define CCL_Call		0x13	/* Call the CCL program whose ID is
					   CC..C or cc..c.
					   1:CCCCCCCCCCCCCCCCCCCCFFFXXXXX
					   [2:00000000cccccccccccccccccccc]
					   ------------------------------
					   if (FFF)
					   call (cc..c)
					   IC++;
					   else
					   call (CC..C)
					 */

#define CCL_WriteConstString	0x14	/* Write a constant or a string:
					   1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   [2:0000STRIN[0]STRIN[1]STRIN[2]]
					   [...]
					   -----------------------------
					   if (!rrr)
					   write (CC..C)
					   else
					   write_string (STRING, CC..C);
					   IC += (CC..C + 2) / 3;
					 */

#define CCL_WriteArray		0x15	/* Write an element of array:
					   1:CCCCCCCCCCCCCCCCCCCCrrrXXXXX
					   2:ELEMENT[0]
					   3:ELEMENT[1]
					   ...
					   ------------------------------
					   if (0 <= reg[rrr] < CC..C)
					   write (ELEMENT[reg[rrr]]);
					   IC += CC..C;
					 */

#define CCL_End			0x16	/* Terminate:
					   1:00000000000000000000000XXXXX
					   ------------------------------
					   terminate ();
					 */

/* The following two codes execute an assignment arithmetic/logical
   operation.  The form of the operation is like REG OP= OPERAND.  */

#define CCL_ExprSelfConst	0x17	/* REG OP= constant:
					   1:00000OPERATION000000rrrXXXXX
					   2:CONSTANT
					   ------------------------------
					   reg[rrr] OPERATION= CONSTANT;
					 */

#define CCL_ExprSelfReg		0x18	/* REG1 OP= REG2:
					   1:00000OPERATION000RRRrrrXXXXX
					   ------------------------------
					   reg[rrr] OPERATION= reg[RRR];
					 */

/* The following codes execute an arithmetic/logical operation.  The
   form of the operation is like REG_X = REG_Y OP OPERAND2.  */

#define CCL_SetExprConst	0x19	/* REG_X = REG_Y OP constant:
					   1:00000OPERATION000RRRrrrXXXXX
					   2:CONSTANT
					   ------------------------------
					   reg[rrr] = reg[RRR] OPERATION CONSTANT;
					   IC++;
					 */

#define CCL_SetExprReg		0x1A	/* REG1 = REG2 OP REG3:
					   1:00000OPERATIONRrrRRRrrrXXXXX
					   ------------------------------
					   reg[rrr] = reg[RRR] OPERATION reg[Rrr];
					 */

#define CCL_JumpCondExprConst	0x1B	/* Jump conditional according to
					   an operation on constant:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:OPERATION
					   3:CONSTANT
					   -----------------------------
					   reg[7] = reg[rrr] OPERATION CONSTANT;
					   if (!(reg[7]))
					   IC += ADDRESS;
					   else
					   IC += 2
					 */

#define CCL_JumpCondExprReg	0x1C	/* Jump conditional according to
					   an operation on register:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:OPERATION
					   3:RRR
					   -----------------------------
					   reg[7] = reg[rrr] OPERATION reg[RRR];
					   if (!reg[7])
					   IC += ADDRESS;
					   else
					   IC += 2;
					 */

#define CCL_ReadJumpCondExprConst 0x1D	/* Read and jump conditional according
					   to an operation on constant:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:OPERATION
					   3:CONSTANT
					   -----------------------------
					   read (reg[rrr]);
					   reg[7] = reg[rrr] OPERATION CONSTANT;
					   if (!reg[7])
					   IC += ADDRESS;
					   else
					   IC += 2;
					 */

#define CCL_ReadJumpCondExprReg	0x1E	/* Read and jump conditional according
					   to an operation on register:
					   1:A--D--D--R--E--S--S-rrrXXXXX
					   2:OPERATION
					   3:RRR
					   -----------------------------
					   read (reg[rrr]);
					   reg[7] = reg[rrr] OPERATION reg[RRR];
					   if (!reg[7])
					   IC += ADDRESS;
					   else
					   IC += 2;
					 */

#define CCL_Extension		0x1F	/* Extended CCL code
					   1:ExtendedCOMMNDRrrRRRrrrXXXXX
					   2:ARGUMENT
					   3:...
					   ------------------------------
					   extended_command (rrr,RRR,Rrr,ARGS)
					 */

/*
   Here after, Extended CCL Instructions.
   Bit length of extended command is 14.
   Therefore, the instruction code range is 0..16384(0x3fff).
 */

/* Read a multibyte characeter.
   A code point is stored into reg[rrr].  A charset ID is stored into
   reg[RRR].  */

#define CCL_ReadMultibyteChar2	0x00	/* Read Multibyte Character
					   1:ExtendedCOMMNDRrrRRRrrrXXXXX  */

/* Write a multibyte character.
   Write a character whose code point is reg[rrr] and the charset ID
   is reg[RRR].  */

#define CCL_WriteMultibyteChar2	0x01	/* Write Multibyte Character
					   1:ExtendedCOMMNDRrrRRRrrrXXXXX  */

/* Translate a character whose code point is reg[rrr] and the charset
   ID is reg[RRR] by a translation table whose ID is reg[Rrr].

   A translated character is set in reg[rrr] (code point) and reg[RRR]
   (charset ID).  */

#define CCL_TranslateCharacter	0x02	/* Translate a multibyte character
					   1:ExtendedCOMMNDRrrRRRrrrXXXXX  */

/* Translate a character whose code point is reg[rrr] and the charset
   ID is reg[RRR] by a translation table whose ID is ARGUMENT.

   A translated character is set in reg[rrr] (code point) and reg[RRR]
   (charset ID).  */

#define CCL_TranslateCharacterConstTbl 0x03	/* Translate a multibyte character
						   1:ExtendedCOMMNDRrrRRRrrrXXXXX
						   2:ARGUMENT(Translation Table ID)
						 */

/* Iterate looking up MAPs for reg[rrr] starting from the Nth (N =
   reg[RRR]) MAP until some value is found.

   Each MAP is a Lisp vector whose element is number, nil, t, or
   lambda.
   If the element is nil, ignore the map and proceed to the next map.
   If the element is t or lambda, finish without changing reg[rrr].
   If the element is a number, set reg[rrr] to the number and finish.

   Detail of the map structure is described in the comment for
   CCL_MapMultiple below.  */

#define CCL_IterateMultipleMap	0x10	/* Iterate multiple maps
					   1:ExtendedCOMMNDXXXRRRrrrXXXXX
					   2:NUMBER of MAPs
					   3:MAP-ID1
					   4:MAP-ID2
					   ...
					 */

/* Map the code in reg[rrr] by MAPs starting from the Nth (N =
   reg[RRR]) map.

   MAPs are supplied in the succeeding CCL codes as follows:

   When CCL program gives this nested structure of map to this command:
	((MAP-ID11
	  MAP-ID12
	  (MAP-ID121 MAP-ID122 MAP-ID123)
	  MAP-ID13)
	 (MAP-ID21
	  (MAP-ID211 (MAP-ID2111) MAP-ID212)
	  MAP-ID22)),
   the compiled CCL code has this sequence:
	CCL_MapMultiple (CCL code of this command)
	16 (total number of MAPs and SEPARATORs)
	-7 (1st SEPARATOR)
	MAP-ID11
	MAP-ID12
	-3 (2nd SEPARATOR)
	MAP-ID121
	MAP-ID122
	MAP-ID123
	MAP-ID13
	-7 (3rd SEPARATOR)
	MAP-ID21
	-4 (4th SEPARATOR)
	MAP-ID211
	-1 (5th SEPARATOR)
	MAP_ID2111
	MAP-ID212
	MAP-ID22

   A value of each SEPARATOR follows this rule:
	MAP-SET := SEPARATOR [(MAP-ID | MAP-SET)]+
	SEPARATOR := -(number of MAP-IDs and SEPARATORs in the MAP-SET)

   (*)....Nest level of MAP-SET must not be over than MAX_MAP_SET_LEVEL.

   When some map fails to map (i.e. it doesn't have a value for
   reg[rrr]), the mapping is treated as identity.

   The mapping is iterated for all maps in each map set (set of maps
   separated by SEPARATOR) except in the case that lambda is
   encountered.  More precisely, the mapping proceeds as below:

   At first, VAL0 is set to reg[rrr], and it is translated by the
   first map to VAL1.  Then, VAL1 is translated by the next map to
   VAL2.  This mapping is iterated until the last map is used.  The
   result of the mapping is the last value of VAL?.  When the mapping
   process reached to the end of the map set, it moves to the next
   map set.  If the next does not exit, the mapping process terminates,
   and regard the last value as a result.

   But, when VALm is mapped to VALn and VALn is not a number, the
   mapping proceeds as follows:

   If VALn is nil, the lastest map is ignored and the mapping of VALm
   proceeds to the next map.

   In VALn is t, VALm is reverted to reg[rrr] and the mapping of VALm
   proceeds to the next map.

   If VALn is lambda, move to the next map set like reaching to the
   end of the current map set.

   If VALn is a symbol, call the CCL program refered by it.
   Then, use reg[rrr] as a mapped value except for -1, -2 and -3.
   Such special values are regarded as nil, t, and lambda respectively.

   Each map is a Lisp vector of the following format (a) or (b):
	(a)......[STARTPOINT VAL1 VAL2 ...]
	(b)......[t VAL STARTPOINT ENDPOINT],
   where
	STARTPOINT is an offset to be used for indexing a map,
	ENDPOINT is a maximum index number of a map,
	VAL and VALn is a number, nil, t, or lambda.

   Valid index range of a map of type (a) is:
	STARTPOINT <= index < STARTPOINT + map_size - 1
   Valid index range of a map of type (b) is:
	STARTPOINT <= index < ENDPOINT	*/

#define CCL_MapMultiple 0x11	/* Mapping by multiple code conversion maps
				   1:ExtendedCOMMNDXXXRRRrrrXXXXX
				   2:N-2
				   3:SEPARATOR_1 (< 0)
				   4:MAP-ID_1
				   5:MAP-ID_2
				   ...
				   M:SEPARATOR_x (< 0)
				   M+1:MAP-ID_y
				   ...
				   N:SEPARATOR_z (< 0)
				 */

#define MAX_MAP_SET_LEVEL 30

typedef struct {
	int rest_length;
	int orig_val;
} tr_stack;

static tr_stack mapping_stack[MAX_MAP_SET_LEVEL];
static tr_stack *mapping_stack_pointer;

/* If this variable is non-zero, it indicates the stack_idx
   of immediately called by CCL_MapMultiple. */
static int stack_idx_of_map_multiple;

#define PUSH_MAPPING_STACK(restlen, orig)		\
  do {							\
    mapping_stack_pointer->rest_length = (restlen);	\
    mapping_stack_pointer->orig_val = (orig);		\
    mapping_stack_pointer++;				\
  } while (0)

#define POP_MAPPING_STACK(restlen, orig)		\
  do {							\
    mapping_stack_pointer--;				\
    (restlen) = mapping_stack_pointer->rest_length;	\
    (orig) = mapping_stack_pointer->orig_val;		\
  } while (0)

#define CCL_CALL_FOR_MAP_INSTRUCTION(symbol, ret_ic)		\
  do {								\
    struct ccl_program called_ccl;				\
    if (stack_idx >= 256					\
	|| (setup_ccl_program (&called_ccl, (symbol)) != 0))	\
      {								\
	if (stack_idx > 0)					\
	  {							\
	    ccl_prog = ccl_prog_stack_struct[0].ccl_prog;	\
	    ic = ccl_prog_stack_struct[0].ic;			\
	  }							\
	CCL_INVALID_CMD;					\
      }								\
    ccl_prog_stack_struct[stack_idx].ccl_prog = ccl_prog;	\
    ccl_prog_stack_struct[stack_idx].ic = (ret_ic);		\
    stack_idx++;						\
    ccl_prog = called_ccl.prog;					\
    ic = CCL_HEADER_MAIN;					\
    /* The "if (1)" prevents warning				\
       "end-of loop code not reached" */			\
    if (1) goto ccl_repeat;					\
  } while (0)

#define CCL_MapSingle		0x12	/* Map by single code conversion map
					   1:ExtendedCOMMNDXXXRRRrrrXXXXX
					   2:MAP-ID
					   ------------------------------
					   Map reg[rrr] by MAP-ID.
					   If some valid mapping is found,
					   set reg[rrr] to the result,
					   else
					   set reg[RRR] to -1.
					 */

/* CCL arithmetic/logical operators. */
#define CCL_PLUS	0x00	/* X = Y + Z */
#define CCL_MINUS	0x01	/* X = Y - Z */
#define CCL_MUL		0x02	/* X = Y * Z */
#define CCL_DIV		0x03	/* X = Y / Z */
#define CCL_MOD		0x04	/* X = Y % Z */
#define CCL_AND		0x05	/* X = Y & Z */
#define CCL_OR		0x06	/* X = Y | Z */
#define CCL_XOR		0x07	/* X = Y ^ Z */
#define CCL_LSH		0x08	/* X = Y << Z */
#define CCL_RSH		0x09	/* X = Y >> Z */
#define CCL_LSH8	0x0A	/* X = (Y << 8) | Z */
#define CCL_RSH8	0x0B	/* X = Y >> 8, r[7] = Y & 0xFF  */
#define CCL_DIVMOD	0x0C	/* X = Y / Z, r[7] = Y % Z */
#define CCL_LS		0x10	/* X = (X < Y) */
#define CCL_GT		0x11	/* X = (X > Y) */
#define CCL_EQ		0x12	/* X = (X == Y) */
#define CCL_LE		0x13	/* X = (X <= Y) */
#define CCL_GE		0x14	/* X = (X >= Y) */
#define CCL_NE		0x15	/* X = (X != Y) */

#define CCL_DECODE_SJIS 0x16	/* X = HIGHER_BYTE (DE-SJIS (Y, Z))
				   r[7] = LOWER_BYTE (DE-SJIS (Y, Z)) */
#define CCL_ENCODE_SJIS 0x17	/* X = HIGHER_BYTE (SJIS (Y, Z))
				   r[7] = LOWER_BYTE (SJIS (Y, Z) */

/* Terminate CCL program successfully.  */
#define CCL_SUCCESS		   	\
  do {				   	\
    ccl->status = CCL_STAT_SUCCESS;	\
  /* The "if (1)" inhibits the warning	\
     "end-of loop code not reached" */	\
  if (1) goto ccl_finish;		\
  } while (0)

/* Suspend CCL program because of reading from empty input buffer or
   writing to full output buffer.  When this program is resumed, the
   same I/O command is executed.  */
#define CCL_SUSPEND(stat)	\
  do {				\
    ic--;			\
  ccl->status = (stat);			\
  /* The "if (1)" inhibits the warning	\
     "end-of loop code not reached" */	\
  if (1) goto ccl_finish;		\
  } while (0)

/* Terminate CCL program because of invalid command.  Should not occur
   in the normal case.  */
#define CCL_INVALID_CMD		     	\
  do {				     	\
    ccl->status = CCL_STAT_INVALID_CMD;	\
  /* The "if (1)" inhibits the warning	\
     "end-of loop code not reached" */	\
  if (1) goto ccl_error_handler;	\
  } while (0)

/* Encode one character CH to multibyte form and write to the current
   output buffer.  At encoding time, if CH is less than 256, CH is
   written as is.  At decoding time, if CH cannot be regarded as an
   ASCII character, write it in multibyte form.  */
#define CCL_WRITE_CHAR(ch)					\
  do {								\
    if (!destination)						\
      CCL_INVALID_CMD;						\
    if (conversion_mode == CCL_MODE_ENCODING)			\
      {								\
	if ((ch) == '\n')					\
	  {							\
	    if (ccl->eol_type == CCL_CODING_EOL_CRLF)		\
	      {							\
		Dynarr_add (destination, '\r');			\
		Dynarr_add (destination, '\n');			\
	      }							\
	    else if (ccl->eol_type == CCL_CODING_EOL_CR)	\
	      Dynarr_add (destination, '\r');			\
	    else						\
	      Dynarr_add (destination, '\n');			\
	  }							\
	else if ((ch) < 0x100)					\
	  {							\
	    Dynarr_add (destination, ch);			\
	  }							\
	else							\
	  {							\
	    Bufbyte work[MAX_EMCHAR_LEN];			\
	    int len;						\
	    len = non_ascii_set_charptr_emchar (work, ch);	\
	    Dynarr_add_many (destination, work, len);		\
	  }							\
      }								\
    else							\
      {								\
	if (!CHAR_MULTIBYTE_P(ch))				\
	  {							\
	    Dynarr_add (destination, ch);			\
	  }							\
	else							\
	  {							\
	    Bufbyte work[MAX_EMCHAR_LEN];			\
	    int len;						\
	    len = non_ascii_set_charptr_emchar (work, ch);	\
	    Dynarr_add_many (destination, work, len);		\
	  }							\
      }								\
  } while (0)

/* Write a string at ccl_prog[IC] of length LEN to the current output
   buffer.  But this macro treat this string as a binary.  Therefore,
   cannot handle a multibyte string except for Control-1 characters. */
#define CCL_WRITE_STRING(len)					\
  do {								\
    Bufbyte work[MAX_EMCHAR_LEN];				\
    int ch, bytes;						\
    if (!destination)						\
      CCL_INVALID_CMD;						\
    else if (conversion_mode == CCL_MODE_ENCODING)		\
      {								\
	for (i = 0; i < (len); i++)				\
	  {							\
	    ch = ((XINT (ccl_prog[ic + (i / 3)]))		\
		  >> ((2 - (i % 3)) * 8)) & 0xFF;		\
	    if (ch == '\n')					\
	      {							\
		if (ccl->eol_type == CCL_CODING_EOL_CRLF)	\
		  {						\
		    Dynarr_add (destination, '\r');		\
		    Dynarr_add (destination, '\n');		\
		  }						\
		else if (ccl->eol_type == CCL_CODING_EOL_CR)	\
		  Dynarr_add (destination, '\r');		\
		else						\
		  Dynarr_add (destination, '\n');		\
	      }							\
	    if (ch < 0x100)					\
	      {							\
		Dynarr_add (destination, ch);			\
	      }							\
	    else						\
	      {							\
		bytes = non_ascii_set_charptr_emchar (work, ch); \
		Dynarr_add_many (destination, work, len);	\
	      }							\
	  }							\
      }								\
    else							\
      {								\
	for (i = 0; i < (len); i++)				\
	  {							\
	    ch = ((XINT (ccl_prog[ic + (i / 3)]))		\
		  >> ((2 - (i % 3)) * 8)) & 0xFF;		\
	    if (!CHAR_MULTIBYTE_P(ch))				\
	      {							\
		Dynarr_add (destination, ch);			\
	      }							\
	    else						\
	      {							\
		bytes = non_ascii_set_charptr_emchar (work, ch); \
		Dynarr_add_many (destination, work, len);	\
	      }							\
	  }							\
      }								\
  } while (0)

/* Read one byte from the current input buffer into Rth register.  */
#define CCL_READ_CHAR(r)				\
  do {							\
    if (!src)						\
      CCL_INVALID_CMD;					\
    if (src < src_end)					\
      (r) = *src++;					\
    else						\
      {							\
	if (ccl->last_block)				\
	  {						\
	    ic = ccl->eof_ic;				\
	    goto ccl_repeat;				\
	  }						\
	else						\
	  CCL_SUSPEND (CCL_STAT_SUSPEND_BY_SRC);	\
      }							\
  } while (0)

/* Set C to the character code made from CHARSET and CODE.  This is
   like MAKE_CHAR but check the validity of CHARSET and CODE.  If they
   are not valid, set C to (CODE & 0xFF) because that is usually the
   case that CCL_ReadMultibyteChar2 read an invalid code and it set
   CODE to that invalid byte.  */

/* On XEmacs, TranslateCharacter is not supported.  Thus, this
   macro is not used.  */
#if 0
#define CCL_MAKE_CHAR(charset, code, c)				\
  do {								\
    if ((charset) == CHARSET_ASCII)				\
      (c) = (code) & 0xFF;						\
    else if (CHARSET_DEFINED_P (charset)			\
	     && ((code) & 0x7F) >= 32				\
	     && ((code) < 256 || ((code >> 7) & 0x7F) >= 32))	\
      {								\
	int c1 = (code) & 0x7F, c2 = 0;				\
								\
	if ((code) >= 256)					\
	  c2 = c1, c1 = ((code) >> 7) & 0x7F;			\
	(c) = MAKE_CHAR (charset, c1, c2);			\
      }								\
    else							\
      (c) = (code) & 0xFF;						\
  } while (0)
#endif

/* Execute CCL code on SRC_BYTES length text at SOURCE.  The resulting
   text goes to a place pointed by DESTINATION, the length of which
   should not exceed DST_BYTES.  The bytes actually processed is
   returned as *CONSUMED.  The return value is the length of the
   resulting text.  As a side effect, the contents of CCL registers
   are updated.  If SOURCE or DESTINATION is NULL, only operations on
   registers are permitted.  */

#ifdef CCL_DEBUG
#define CCL_DEBUG_BACKTRACE_LEN 256
int ccl_backtrace_table[CCL_BACKTRACE_TABLE];
int ccl_backtrace_idx;
#endif

struct ccl_prog_stack {
	Lisp_Object *ccl_prog;	/* Pointer to an array of CCL code.  */
	int ic;			/* Instruction Counter.  */
};

/* For the moment, we only support depth 256 of stack.  */
static struct ccl_prog_stack ccl_prog_stack_struct[256];

int
ccl_driver(struct ccl_program *ccl,
	   const unsigned char *source,
	   unsigned_char_dynarr * destination,
	   int src_bytes, int *consumed, int conversion_mode)
{
	register int *reg = ccl->reg;
	register int ic = ccl->ic;
	register int code = -1;
	register int field1, field2;
	register Lisp_Object *ccl_prog = ccl->prog;
	const unsigned char *src = source, *src_end = src + src_bytes;
	int jump_address;
	int i, j, op;
	int stack_idx = ccl->stack_idx;
	/* Instruction counter of the current CCL code. */
	int this_ic = 0;

	if (ic >= ccl->eof_ic)
		ic = CCL_HEADER_MAIN;

	if (ccl->buf_magnification == 0)	/* We can't produce any bytes.  */
		destination = NULL;

	/* Set mapping stack pointer. */
	mapping_stack_pointer = mapping_stack;

#ifdef CCL_DEBUG
	ccl_backtrace_idx = 0;
#endif

	for (;;) {
	      ccl_repeat:
#ifdef CCL_DEBUG
		ccl_backtrace_table[ccl_backtrace_idx++] = ic;
		if (ccl_backtrace_idx >= CCL_DEBUG_BACKTRACE_LEN)
			ccl_backtrace_idx = 0;
		ccl_backtrace_table[ccl_backtrace_idx] = 0;
#endif

		if (!NILP(Vquit_flag) && NILP(Vinhibit_quit)) {
			/* We can't just signal Qquit, instead break the loop as if
			   the whole data is processed.  Don't reset Vquit_flag, it
			   must be handled later at a safer place.  */
			if (consumed)
				src = source + src_bytes;
			ccl->status = CCL_STAT_QUIT;
			break;
		}

		this_ic = ic;
		code = XINT(ccl_prog[ic]);
		ic++;
		field1 = code >> 8;
		field2 = (code & 0xFF) >> 5;

#define rrr field2
#define RRR (field1 & 7)
#define Rrr ((field1 >> 3) & 7)
#define ADDR field1
#define EXCMD (field1 >> 6)

		switch (code & 0x1F) {
		case CCL_SetRegister:	/* 00000000000000000RRRrrrXXXXX */
			reg[rrr] = reg[RRR];
			break;

		case CCL_SetShortConst:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
			reg[rrr] = field1;
			break;

		case CCL_SetConst:	/* 00000000000000000000rrrXXXXX */
			reg[rrr] = XINT(ccl_prog[ic]);
			ic++;
			break;

		case CCL_SetArray:	/* CCCCCCCCCCCCCCCCCCCCRRRrrrXXXXX */
			i = reg[RRR];
			j = field1 >> 3;
			if ((unsigned int)i < j)
				reg[rrr] = XINT(ccl_prog[ic + i]);
			ic += j;
			break;

		case CCL_Jump:	/* A--D--D--R--E--S--S-000XXXXX */
			ic += ADDR;
			break;

		case CCL_JumpCond:	/* A--D--D--R--E--S--S-rrrXXXXX */
			if (!reg[rrr])
				ic += ADDR;
			break;

		case CCL_WriteRegisterJump:	/* A--D--D--R--E--S--S-rrrXXXXX */
			i = reg[rrr];
			CCL_WRITE_CHAR(i);
			ic += ADDR;
			break;

		case CCL_WriteRegisterReadJump:	/* A--D--D--R--E--S--S-rrrXXXXX */
			i = reg[rrr];
			CCL_WRITE_CHAR(i);
			ic++;
			CCL_READ_CHAR(reg[rrr]);
			ic += ADDR - 1;
			break;

		case CCL_WriteConstJump:	/* A--D--D--R--E--S--S-000XXXXX */
			i = XINT(ccl_prog[ic]);
			CCL_WRITE_CHAR(i);
			ic += ADDR;
			break;

		case CCL_WriteConstReadJump:	/* A--D--D--R--E--S--S-rrrXXXXX */
			i = XINT(ccl_prog[ic]);
			CCL_WRITE_CHAR(i);
			ic++;
			CCL_READ_CHAR(reg[rrr]);
			ic += ADDR - 1;
			break;

		case CCL_WriteStringJump:	/* A--D--D--R--E--S--S-000XXXXX */
			j = XINT(ccl_prog[ic]);
			ic++;
			CCL_WRITE_STRING(j);
			ic += ADDR - 1;
			break;

		case CCL_WriteArrayReadJump:	/* A--D--D--R--E--S--S-rrrXXXXX */
			i = reg[rrr];
			j = XINT(ccl_prog[ic]);
			if ((unsigned int)i < j) {
				i = XINT(ccl_prog[ic + 1 + i]);
				CCL_WRITE_CHAR(i);
			}
			ic += j + 2;
			CCL_READ_CHAR(reg[rrr]);
			ic += ADDR - (j + 2);
			break;

		case CCL_ReadJump:	/* A--D--D--R--E--S--S-rrrYYYYY */
			CCL_READ_CHAR(reg[rrr]);
			ic += ADDR;
			break;

		case CCL_ReadBranch:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
			CCL_READ_CHAR(reg[rrr]);
			/* fall through ... */
		case CCL_Branch:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
			if ((unsigned int)reg[rrr] < field1)
				ic += XINT(ccl_prog[ic + reg[rrr]]);
			else
				ic += XINT(ccl_prog[ic + field1]);
			break;

		case CCL_ReadRegister:	/* CCCCCCCCCCCCCCCCCCCCrrXXXXX */
			while (1) {
				CCL_READ_CHAR(reg[rrr]);
				if (!field1)
					break;
				code = XINT(ccl_prog[ic]);
				ic++;
				field1 = code >> 8;
				field2 = (code & 0xFF) >> 5;
			}
			break;

		case CCL_WriteExprConst:	/* 1:00000OPERATION000RRR000XXXXX */
			rrr = 7;
			i = reg[RRR];
			j = XINT(ccl_prog[ic]);
			op = field1 >> 6;
			jump_address = ic + 1;
			goto ccl_set_expr;

		case CCL_WriteRegister:	/* CCCCCCCCCCCCCCCCCCCrrrXXXXX */
			while (1) {
				i = reg[rrr];
				CCL_WRITE_CHAR(i);
				if (!field1)
					break;
				code = XINT(ccl_prog[ic]);
				ic++;
				field1 = code >> 8;
				field2 = (code & 0xFF) >> 5;
			}
			break;

		case CCL_WriteExprRegister:	/* 1:00000OPERATIONRrrRRR000XXXXX */
			rrr = 7;
			i = reg[RRR];
			j = reg[Rrr];
			op = field1 >> 6;
			jump_address = ic;
			goto ccl_set_expr;

		case CCL_Call:	/* 1:CCCCCCCCCCCCCCCCCCCCFFFXXXXX */
			{
				Lisp_Object slot;
				int prog_id;

				/* If FFF is nonzero, the CCL program ID is in the
				   following code.  */
				if (rrr) {
					prog_id = XINT(ccl_prog[ic]);
					ic++;
				} else
					prog_id = field1;

				if (stack_idx >= 256
				    || prog_id < 0
				    || prog_id >=
				    XVECTOR(Vccl_program_table)->size
				    || (slot =
					XVECTOR(Vccl_program_table)->
					contents[prog_id], !VECTORP(slot))
				    || !VECTORP(XVECTOR(slot)->contents[1])) {
					if (stack_idx > 0) {
						ccl_prog =
						    ccl_prog_stack_struct[0].
						    ccl_prog;
						ic = ccl_prog_stack_struct[0].
						    ic;
					}
					CCL_INVALID_CMD;
				}

				ccl_prog_stack_struct[stack_idx].ccl_prog =
				    ccl_prog;
				ccl_prog_stack_struct[stack_idx].ic = ic;
				stack_idx++;
				ccl_prog =
				    XVECTOR(XVECTOR(slot)->contents[1])->
				    contents;
				ic = CCL_HEADER_MAIN;
			}
			break;

		case CCL_WriteConstString:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
			if (!rrr)
				CCL_WRITE_CHAR(field1);
			else {
				CCL_WRITE_STRING(field1);
				ic += (field1 + 2) / 3;
			}
			break;

		case CCL_WriteArray:	/* CCCCCCCCCCCCCCCCCCCCrrrXXXXX */
			i = reg[rrr];
			if ((unsigned int)i < field1) {
				j = XINT(ccl_prog[ic + i]);
				CCL_WRITE_CHAR(j);
			}
			ic += field1;
			break;

		case CCL_End:	/* 0000000000000000000000XXXXX */
			if (stack_idx > 0) {
				stack_idx--;
				ccl_prog =
				    ccl_prog_stack_struct[stack_idx].ccl_prog;
				ic = ccl_prog_stack_struct[stack_idx].ic;
				break;
			}
			if (src)
				src = src_end;
			/* ccl->ic should points to this command code again to
			   suppress further processing.  */
			ic--;
			CCL_SUCCESS;

		case CCL_ExprSelfConst:	/* 00000OPERATION000000rrrXXXXX */
			i = XINT(ccl_prog[ic]);
			ic++;
			op = field1 >> 6;
			goto ccl_expr_self;

		case CCL_ExprSelfReg:	/* 00000OPERATION000RRRrrrXXXXX */
			i = reg[RRR];
			op = field1 >> 6;

		      ccl_expr_self:
			switch (op) {
			case CCL_PLUS:
				reg[rrr] += i;
				break;
			case CCL_MINUS:
				reg[rrr] -= i;
				break;
			case CCL_MUL:
				reg[rrr] *= i;
				break;
			case CCL_DIV:
				reg[rrr] /= i;
				break;
			case CCL_MOD:
				reg[rrr] %= i;
				break;
			case CCL_AND:
				reg[rrr] &= i;
				break;
			case CCL_OR:
				reg[rrr] |= i;
				break;
			case CCL_XOR:
				reg[rrr] ^= i;
				break;
			case CCL_LSH:
				reg[rrr] <<= i;
				break;
			case CCL_RSH:
				reg[rrr] >>= i;
				break;
			case CCL_LSH8:
				reg[rrr] <<= 8;
				reg[rrr] |= i;
				break;
			case CCL_RSH8:
				reg[7] = reg[rrr] & 0xFF;
				reg[rrr] >>= 8;
				break;
			case CCL_DIVMOD:
				reg[7] = reg[rrr] % i;
				reg[rrr] /= i;
				break;
			case CCL_LS:
				reg[rrr] = reg[rrr] < i;
				break;
			case CCL_GT:
				reg[rrr] = reg[rrr] > i;
				break;
			case CCL_EQ:
				reg[rrr] = reg[rrr] == i;
				break;
			case CCL_LE:
				reg[rrr] = reg[rrr] <= i;
				break;
			case CCL_GE:
				reg[rrr] = reg[rrr] >= i;
				break;
			case CCL_NE:
				reg[rrr] = reg[rrr] != i;
				break;
			default:
				CCL_INVALID_CMD;
			}
			break;

		case CCL_SetExprConst:	/* 00000OPERATION000RRRrrrXXXXX */
			i = reg[RRR];
			j = XINT(ccl_prog[ic]);
			op = field1 >> 6;
			jump_address = ++ic;
			goto ccl_set_expr;

		case CCL_SetExprReg:	/* 00000OPERATIONRrrRRRrrrXXXXX */
			i = reg[RRR];
			j = reg[Rrr];
			op = field1 >> 6;
			jump_address = ic;
			goto ccl_set_expr;

		case CCL_ReadJumpCondExprConst:	/* A--D--D--R--E--S--S-rrrXXXXX */
			CCL_READ_CHAR(reg[rrr]);
		case CCL_JumpCondExprConst:	/* A--D--D--R--E--S--S-rrrXXXXX */
			i = reg[rrr];
			op = XINT(ccl_prog[ic]);
			jump_address = ic++ + ADDR;
			j = XINT(ccl_prog[ic]);
			ic++;
			rrr = 7;
			goto ccl_set_expr;

		case CCL_ReadJumpCondExprReg:	/* A--D--D--R--E--S--S-rrrXXXXX */
			CCL_READ_CHAR(reg[rrr]);
		case CCL_JumpCondExprReg:
			i = reg[rrr];
			op = XINT(ccl_prog[ic]);
			jump_address = ic++ + ADDR;
			j = reg[XINT(ccl_prog[ic])];
			ic++;
			rrr = 7;

		      ccl_set_expr:
			switch (op) {
			case CCL_PLUS:
				reg[rrr] = i + j;
				break;
			case CCL_MINUS:
				reg[rrr] = i - j;
				break;
			case CCL_MUL:
				reg[rrr] = i * j;
				break;
			case CCL_DIV:
				reg[rrr] = i / j;
				break;
			case CCL_MOD:
				reg[rrr] = i % j;
				break;
			case CCL_AND:
				reg[rrr] = i & j;
				break;
			case CCL_OR:
				reg[rrr] = i | j;
				break;
			case CCL_XOR:
				reg[rrr] = i ^ j;;
				break;
			case CCL_LSH:
				reg[rrr] = i << j;
				break;
			case CCL_RSH:
				reg[rrr] = i >> j;
				break;
			case CCL_LSH8:
				reg[rrr] = (i << 8) | j;
				break;
			case CCL_RSH8:
				reg[rrr] = i >> 8;
				reg[7] = i & 0xFF;
				break;
			case CCL_DIVMOD:
				reg[rrr] = i / j;
				reg[7] = i % j;
				break;
			case CCL_LS:
				reg[rrr] = i < j;
				break;
			case CCL_GT:
				reg[rrr] = i > j;
				break;
			case CCL_EQ:
				reg[rrr] = i == j;
				break;
			case CCL_LE:
				reg[rrr] = i <= j;
				break;
			case CCL_GE:
				reg[rrr] = i >= j;
				break;
			case CCL_NE:
				reg[rrr] = i != j;
				break;
			case CCL_DECODE_SJIS:
				/* DECODE_SJIS set MSB for internal format
				   as opposed to Emacs.  */
				DECODE_SJIS(i, j, reg[rrr], reg[7]);
				reg[rrr] &= 0x7F;
				reg[7] &= 0x7F;
				break;
			case CCL_ENCODE_SJIS:
				/* ENCODE_SJIS assumes MSB of SJIS-char is set
				   as opposed to Emacs.  */
				ENCODE_SJIS(i | 0x80, j | 0x80, reg[rrr],
					    reg[7]);
				break;
			default:
				CCL_INVALID_CMD;
			}
			code &= 0x1F;
			if (code == CCL_WriteExprConst
			    || code == CCL_WriteExprRegister) {
				i = reg[rrr];
				CCL_WRITE_CHAR(i);
				ic = jump_address;
			} else if (!reg[rrr])
				ic = jump_address;
			break;

		case CCL_Extension:
			switch (EXCMD) {
			case CCL_ReadMultibyteChar2:
				if (!src)
					CCL_INVALID_CMD;

				if (src >= src_end) {
					src++;
					goto ccl_read_multibyte_character_suspend;
				}

				i = *src++;
				if (i < 0x80) {
					/* ASCII */
					reg[rrr] = i;
					reg[RRR] = LEADING_BYTE_ASCII;
				} else if (i <= MAX_LEADING_BYTE_OFFICIAL_1) {
					if (src >= src_end)
						goto ccl_read_multibyte_character_suspend;
					reg[RRR] = i;
					reg[rrr] = (*src++ & 0x7F);
				} else if (i <= MAX_LEADING_BYTE_OFFICIAL_2) {
					if ((src + 1) >= src_end)
						goto ccl_read_multibyte_character_suspend;
					reg[RRR] = i;
					i = (*src++ & 0x7F);
					reg[rrr] = ((i << 7) | (*src & 0x7F));
					src++;
				} else if (i == PRE_LEADING_BYTE_PRIVATE_1) {
					if ((src + 1) >= src_end)
						goto ccl_read_multibyte_character_suspend;
					reg[RRR] = *src++;
					reg[rrr] = (*src++ & 0x7F);
				} else if (i == PRE_LEADING_BYTE_PRIVATE_2) {
					if ((src + 2) >= src_end)
						goto ccl_read_multibyte_character_suspend;
					reg[RRR] = *src++;
					i = (*src++ & 0x7F);
					reg[rrr] = ((i << 7) | (*src & 0x7F));
					src++;
				} else {
					/* INVALID CODE.  Return a single byte character.  */
					reg[RRR] = LEADING_BYTE_ASCII;
					reg[rrr] = i;
				}
				break;

			      ccl_read_multibyte_character_suspend:
				src--;
				if (ccl->last_block) {
					ic = ccl->eof_ic;
					goto ccl_repeat;
				} else
					CCL_SUSPEND(CCL_STAT_SUSPEND_BY_SRC);

				break;

			case CCL_WriteMultibyteChar2:
				i = reg[RRR];	/* charset */
				if (i == LEADING_BYTE_ASCII)
					i = reg[rrr] & 0xFF;
				else if (XCHARSET_DIMENSION
					 (CHARSET_BY_LEADING_BYTE(i)) == 1)
					i = (((i -
					       FIELD2_TO_OFFICIAL_LEADING_BYTE)
					      << 7)
					     | (reg[rrr] & 0x7F));
				else if (i < MAX_LEADING_BYTE_OFFICIAL_2)
					i = ((i -
					      FIELD1_TO_OFFICIAL_LEADING_BYTE)
					     << 14) | reg[rrr];
				else
					i = ((i -
					      FIELD1_TO_PRIVATE_LEADING_BYTE) <<
					     14) | reg[rrr];

				CCL_WRITE_CHAR(i);

				break;

			case CCL_TranslateCharacter:
#if 0
				/* XEmacs does not have translate_char, and its
				   equivalent nor.  We do nothing on this operation. */
				CCL_MAKE_CHAR(reg[RRR], reg[rrr], i);
				op = translate_char(GET_TRANSLATION_TABLE
						    (reg[Rrr]), i, -1, 0, 0);
				SPLIT_CHAR(op, reg[RRR], i, j);
				if (j != -1)
					i = (i << 7) | j;

				reg[rrr] = i;
#endif
				break;

			case CCL_TranslateCharacterConstTbl:
#if 0
				/* XEmacs does not have translate_char, and its
				   equivalent nor.  We do nothing on this operation. */
				op = XINT(ccl_prog[ic]);	/* table */
				ic++;
				CCL_MAKE_CHAR(reg[RRR], reg[rrr], i);
				op = translate_char(GET_TRANSLATION_TABLE(op),
						    i, -1, 0, 0);
				SPLIT_CHAR(op, reg[RRR], i, j);
				if (j != -1)
					i = (i << 7) | j;

				reg[rrr] = i;
#endif
				break;

			case CCL_IterateMultipleMap:
				{
					Lisp_Object map, content, attrib, value;
					int point, size, fin_ic;

					j = XINT(ccl_prog[ic++]);	/* number of maps. */
					fin_ic = ic + j;
					op = reg[rrr];
					if ((j > reg[RRR]) && (j >= 0)) {
						ic += reg[RRR];
						i = reg[RRR];
					} else {
						reg[RRR] = -1;
						ic = fin_ic;
						break;
					}

					for (; i < j; i++) {

						size =
						    XVECTOR
						    (Vcode_conversion_map_vector)->
						    size;
						point = XINT(ccl_prog[ic++]);
						if (point >= size)
							continue;
						map =
						    XVECTOR
						    (Vcode_conversion_map_vector)->
						    contents[point];

						/* Check map validity.  */
						if (!CONSP(map))
							continue;
						map = XCDR(map);
						if (!VECTORP(map))
							continue;
						size = XVECTOR(map)->size;
						if (size <= 1)
							continue;

						content =
						    XVECTOR(map)->contents[0];

						/* check map type,
						   [STARTPOINT VAL1 VAL2 ...] or
						   [t ELEMENT STARTPOINT ENDPOINT]  */
						if (INTP(content)) {
							point = XUINT(content);
							point = op - point + 1;
							if (!
							    ((point >= 1)
							     && (point < size)))
								continue;
							content =
							    XVECTOR(map)->
							    contents[point];
						} else if (EQ(content, Qt)) {
							if (size != 4)
								continue;
							if ((op >=
							     XUINT(XVECTOR
								   (map)->
								   contents[2]))
							    && (op <
								XUINT(XVECTOR
								      (map)->
								      contents
								      [3])))
								content =
								    XVECTOR
								    (map)->
								    contents[1];
							else
								continue;
						} else
							continue;

						if (NILP(content))
							continue;
						else if (INTP(content)) {
							reg[RRR] = i;
							reg[rrr] =
							    XINT(content);
							break;
						} else if (EQ(content, Qt)
							   || EQ(content,
								 Qlambda)) {
							reg[RRR] = i;
							break;
						} else if (CONSP(content)) {
							attrib = XCAR(content);
							value = XCDR(content);
							if (!INTP(attrib)
							    || !INTP(value))
								continue;
							reg[RRR] = i;
							reg[rrr] = XUINT(value);
							break;
						} else if (SYMBOLP(content))
							CCL_CALL_FOR_MAP_INSTRUCTION
							    (content, fin_ic);
						else
							CCL_INVALID_CMD;
					}
					if (i == j)
						reg[RRR] = -1;
					ic = fin_ic;
				}
				break;

			case CCL_MapMultiple:
				{
					Lisp_Object map, content, attrib, value;
					int point, size, map_vector_size;
					int map_set_rest_length, fin_ic;
					int current_ic = this_ic;

					/* inhibit recursive call on MapMultiple. */
					if (stack_idx_of_map_multiple > 0) {
						if (stack_idx_of_map_multiple <=
						    stack_idx) {
							stack_idx_of_map_multiple
							    = 0;
							mapping_stack_pointer =
							    mapping_stack;
							CCL_INVALID_CMD;
						}
					} else
						mapping_stack_pointer =
						    mapping_stack;
					stack_idx_of_map_multiple = 0;

					map_set_rest_length = XINT(ccl_prog[ic++]);	/* number of maps and separators. */
					fin_ic = ic + map_set_rest_length;
					op = reg[rrr];

					if ((map_set_rest_length > reg[RRR])
					    && (reg[RRR] >= 0)) {
						ic += reg[RRR];
						i = reg[RRR];
						map_set_rest_length -= i;
					} else {
						ic = fin_ic;
						reg[RRR] = -1;
						mapping_stack_pointer =
						    mapping_stack;
						break;
					}

					if (mapping_stack_pointer <=
					    (mapping_stack + 1)) {
						/* Set up initial state. */
						mapping_stack_pointer =
						    mapping_stack;
						PUSH_MAPPING_STACK(0, op);
						reg[RRR] = -1;
					} else {
						/* Recover after calling other ccl program. */
						int orig_op;

						POP_MAPPING_STACK
						    (map_set_rest_length,
						     orig_op);
						POP_MAPPING_STACK
						    (map_set_rest_length,
						     reg[rrr]);
						switch (op) {
						case -1:
							/* Regard it as Qnil. */
							op = orig_op;
							i++;
							ic++;
							map_set_rest_length--;
							break;
						case -2:
							/* Regard it as Qt. */
							op = reg[rrr];
							i++;
							ic++;
							map_set_rest_length--;
							break;
						case -3:
							/* Regard it as Qlambda. */
							op = orig_op;
							i += map_set_rest_length;
							ic +=
							    map_set_rest_length;
							map_set_rest_length = 0;
							break;
						default:
							/* Regard it as normal mapping. */
							i += map_set_rest_length;
							ic +=
							    map_set_rest_length;
							POP_MAPPING_STACK
							    (map_set_rest_length,
							     reg[rrr]);
							break;
						}
					}
					map_vector_size =
					    XVECTOR
					    (Vcode_conversion_map_vector)->size;

					do {
						for (; map_set_rest_length > 0;
						     i++, ic++,
						     map_set_rest_length--) {
							point =
							    XINT(ccl_prog[ic]);
							if (point < 0) {
								/* +1 is for including separator. */
								point =
								    -point + 1;
								if (mapping_stack_pointer >= mapping_stack + countof(mapping_stack))
									CCL_INVALID_CMD;
								PUSH_MAPPING_STACK
								    (map_set_rest_length
								     - point,
								     reg[rrr]);
								map_set_rest_length
								    = point;
								reg[rrr] = op;
								continue;
							}

							if (point >=
							    map_vector_size)
								continue;
							map =
							    (XVECTOR
							     (Vcode_conversion_map_vector)
							     ->contents[point]);

							/* Check map validity.  */
							if (!CONSP(map))
								continue;
							map = XCDR(map);
							if (!VECTORP(map))
								continue;
							size =
							    XVECTOR(map)->size;
							if (size <= 1)
								continue;

							content =
							    XVECTOR(map)->
							    contents[0];

							/* check map type,
							   [STARTPOINT VAL1 VAL2 ...] or
							   [t ELEMENT STARTPOINT ENDPOINT]  */
							if (INTP(content)) {
								point =
								    XUINT
								    (content);
								point =
								    op - point +
								    1;
								if (!
								    ((point >=
								      1)
								     && (point <
									 size)))
									continue;
								content =
								    XVECTOR
								    (map)->
								    contents
								    [point];
							} else
							    if (EQ(content, Qt))
							{
								if (size != 4)
									continue;
								if ((op >=
								     XUINT
								     (XVECTOR
								      (map)->
								      contents
								      [2]))
								    && (op <
									XUINT
									(XVECTOR
									 (map)->
									 contents
									 [3])))
									content
									    =
									    XVECTOR
									    (map)->
									    contents
									    [1];
								else
									continue;
							} else
								continue;

							if (NILP(content))
								continue;

							reg[RRR] = i;
							if (INTP(content)) {
								op = XINT
								    (content);
								i += map_set_rest_length - 1;
								ic +=
								    map_set_rest_length
								    - 1;
								POP_MAPPING_STACK
								    (map_set_rest_length,
								     reg[rrr]);
								map_set_rest_length++;
							} else
							    if (CONSP(content))
							{
								attrib =
								    XCAR
								    (content);
								value =
								    XCDR
								    (content);
								if (!INTP
								    (attrib)
								    ||
								    !INTP
								    (value))
									continue;
								op = XUINT
								    (value);
								i += map_set_rest_length - 1;
								ic +=
								    map_set_rest_length
								    - 1;
								POP_MAPPING_STACK
								    (map_set_rest_length,
								     reg[rrr]);
								map_set_rest_length++;
							} else
							    if (EQ(content, Qt))
							{
								op = reg[rrr];
							} else
							    if (EQ
								(content,
								 Qlambda)) {
								i += map_set_rest_length;
								ic +=
								    map_set_rest_length;
								break;
							} else
							    if (SYMBOLP
								(content)) {
								if (mapping_stack_pointer >= mapping_stack + countof(mapping_stack))
									CCL_INVALID_CMD;
								PUSH_MAPPING_STACK
								    (map_set_rest_length,
								     reg[rrr]);
								PUSH_MAPPING_STACK
								    (map_set_rest_length,
								     op);
								stack_idx_of_map_multiple
								    =
								    stack_idx +
								    1;
								CCL_CALL_FOR_MAP_INSTRUCTION
								    (content,
								     current_ic);
							} else
								CCL_INVALID_CMD;
						}
						if (mapping_stack_pointer <=
						    (mapping_stack + 1))
							break;
						POP_MAPPING_STACK
						    (map_set_rest_length,
						     reg[rrr]);
						i += map_set_rest_length;
						ic += map_set_rest_length;
						POP_MAPPING_STACK
						    (map_set_rest_length,
						     reg[rrr]);
					} while (1);

					ic = fin_ic;
				}
				reg[rrr] = op;
				break;

			case CCL_MapSingle:
				{
					Lisp_Object map, attrib, value, content;
					int size, point;
					j = XINT(ccl_prog[ic++]);	/* map_id */
					op = reg[rrr];
					if (j >=
					    XVECTOR
					    (Vcode_conversion_map_vector)->
					    size) {
						reg[RRR] = -1;
						break;
					}
					map =
					    XVECTOR
					    (Vcode_conversion_map_vector)->
					    contents[j];
					if (!CONSP(map)) {
						reg[RRR] = -1;
						break;
					}
					map = XCDR(map);
					if (!VECTORP(map)) {
						reg[RRR] = -1;
						break;
					}
					size = XVECTOR(map)->size;
					point =
					    XUINT(XVECTOR(map)->contents[0]);
					point = op - point + 1;
					reg[RRR] = 0;
					if ((size <= 1) ||
					    (!((point >= 1) && (point < size))))
						reg[RRR] = -1;
					else {
						reg[RRR] = 0;
						content =
						    XVECTOR(map)->
						    contents[point];
						if (NILP(content))
							reg[RRR] = -1;
						else if (INTP(content))
							reg[rrr] =
							    XINT(content);
						else if (EQ(content, Qt)) ;
						else if (CONSP(content)) {
							attrib = XCAR(content);
							value = XCDR(content);
							if (!INTP(attrib)
							    || !INTP(value))
								continue;
							reg[rrr] = XUINT(value);
							break;
						} else if (SYMBOLP(content))
							CCL_CALL_FOR_MAP_INSTRUCTION
							    (content, ic);
						else
							reg[RRR] = -1;
					}
				}
				break;

			default:
				CCL_INVALID_CMD;
			}
			break;

		default:
			CCL_INVALID_CMD;
		}
	}

      ccl_error_handler:
	if (destination) {
		/* We can insert an error message only if DESTINATION is
		   specified and we still have a room to store the message
		   there.  */
		char msg[256];

		switch (ccl->status) {
		case CCL_STAT_INVALID_CMD:
			sprintf(msg,
				"\nCCL: Invalid command %x (ccl_code = %x) at %d.",
				code & 0x1F, code, this_ic);
#ifdef CCL_DEBUG
			{
				int i = ccl_backtrace_idx - 1;
				int j;

				Dynarr_add_many(destination,
						(unsigned char *)msg,
						strlen(msg));

				for (j = 0; j < CCL_DEBUG_BACKTRACE_LEN;
				     j++, i--) {
					if (i < 0)
						i = CCL_DEBUG_BACKTRACE_LEN - 1;
					if (ccl_backtrace_table[i] == 0)
						break;
					sprintf(msg, " %d",
						ccl_backtrace_table[i]);
					Dynarr_add_many(destination,
							(unsigned char *)msg,
							strlen(msg));
				}
				goto ccl_finish;
			}
#endif
			break;

		case CCL_STAT_QUIT:
			sprintf(msg, "\nCCL: Exited.");
			break;

		default:
			sprintf(msg, "\nCCL: Unknown error type (%d).",
				ccl->status);
		}

		Dynarr_add_many(destination, (unsigned char *)msg, strlen(msg));
	}

      ccl_finish:
	ccl->ic = ic;
	ccl->stack_idx = stack_idx;
	ccl->prog = ccl_prog;
	if (consumed)
		*consumed = src - source;
	if (!destination)
		return 0;
	return Dynarr_length(destination);
}

/* Resolve symbols in the specified CCL code (Lisp vector).  This
   function converts symbols of code conversion maps and character
   translation tables embedded in the CCL code into their ID numbers.

   The return value is a vector (CCL itself or a new vector in which
   all symbols are resolved), Qt if resolving of some symbol failed,
   or nil if CCL contains invalid data.  */

static Lisp_Object resolve_symbol_ccl_program(Lisp_Object ccl)
{
	int i, veclen, unresolved = 0;
	Lisp_Object result, contents, val;

	result = ccl;
	veclen = XVECTOR(result)->size;

	for (i = 0; i < veclen; i++) {
		contents = XVECTOR(result)->contents[i];
		if (INTP(contents))
			continue;
		else if (CONSP(contents)
			 && SYMBOLP(XCAR(contents))
			 && SYMBOLP(XCDR(contents))) {
			/* This is the new style for embedding symbols.  The form is
			   (SYMBOL . PROPERTY).  (get SYMBOL PROPERTY) should give
			   an index number.  */

			if (EQ(result, ccl))
				result = Fcopy_sequence(ccl);

			val = Fget(XCAR(contents), XCDR(contents), Qnil);
			if (NATNUMP(val))
				XVECTOR(result)->contents[i] = val;
			else
				unresolved = 1;
			continue;
		} else if (SYMBOLP(contents)) {
			/* This is the old style for embedding symbols.  This style
			   may lead to a bug if, for instance, a translation table
			   and a code conversion map have the same name.  */
			if (EQ(result, ccl))
				result = Fcopy_sequence(ccl);

			val = Fget(contents, Qcode_conversion_map_id, Qnil);
			if (NATNUMP(val))
				XVECTOR(result)->contents[i] = val;
			else {
				val = Fget(contents, Qccl_program_idx, Qnil);
				if (NATNUMP(val))
					XVECTOR(result)->contents[i] = val;
				else
					unresolved = 1;
			}
			continue;
		}
		return Qnil;
	}

	return (unresolved ? Qt : result);
}

/* Return the compiled code (vector) of CCL program CCL_PROG.
   CCL_PROG is a name (symbol) of the program or already compiled
   code.  If necessary, resolve symbols in the compiled code to index
   numbers.  If we failed to get the compiled code or to resolve
   symbols, return Qnil.  */

static Lisp_Object ccl_get_compiled_code(Lisp_Object ccl_prog)
{
	Lisp_Object val, slot;

	if (VECTORP(ccl_prog)) {
		val = resolve_symbol_ccl_program(ccl_prog);
		return (VECTORP(val) ? val : Qnil);
	}
	if (!SYMBOLP(ccl_prog))
		return Qnil;

	val = Fget(ccl_prog, Qccl_program_idx, Qnil);
	if (!NATNUMP(val)
	    || XINT(val) >= XVECTOR_LENGTH(Vccl_program_table))
		return Qnil;
	slot = XVECTOR_DATA(Vccl_program_table)[XINT(val)];
	if (!VECTORP(slot)
	    || XVECTOR(slot)->size != 3 || !VECTORP(XVECTOR_DATA(slot)[1]))
		return Qnil;
	if (NILP(XVECTOR_DATA(slot)[2])) {
		val = resolve_symbol_ccl_program(XVECTOR_DATA(slot)[1]);
		if (!VECTORP(val))
			return Qnil;
		XVECTOR_DATA(slot)[1] = val;
		XVECTOR_DATA(slot)[2] = Qt;
	}
	return XVECTOR_DATA(slot)[1];
}

/* Setup fields of the structure pointed by CCL appropriately for the
   execution of CCL program CCL_PROG.  CCL_PROG is the name (symbol)
   of the CCL program or the already compiled code (vector).
   Return 0 if we succeed this setup, else return -1.

   If CCL_PROG is nil, we just reset the structure pointed by CCL.  */
int setup_ccl_program(struct ccl_program *ccl, Lisp_Object ccl_prog)
{
	int i;

	if (!NILP(ccl_prog)) {
		ccl_prog = ccl_get_compiled_code(ccl_prog);
		if (!VECTORP(ccl_prog))
			return -1;
		ccl->size = XVECTOR_LENGTH(ccl_prog);
		ccl->prog = XVECTOR_DATA(ccl_prog);
		ccl->eof_ic = XINT(XVECTOR_DATA(ccl_prog)[CCL_HEADER_EOF]);
		ccl->buf_magnification =
		    XINT(XVECTOR_DATA(ccl_prog)[CCL_HEADER_BUF_MAG]);
	}
	ccl->ic = CCL_HEADER_MAIN;
	for (i = 0; i < 8; i++)
		ccl->reg[i] = 0;
	ccl->last_block = 0;
	ccl->private_state = 0;
	ccl->status = 0;
	ccl->stack_idx = 0;
	ccl->eol_type = CCL_CODING_EOL_LF;
	return 0;
}

#ifdef emacs

DEFUN("ccl-program-p", Fccl_program_p, 1, 1, 0,	/*
						   Return t if OBJECT is a CCL program name or a compiled CCL program code.
						   See the documentation of  `define-ccl-program' for the detail of CCL program.
						 */
      (object))
{
	Lisp_Object val;

	if (VECTORP(object)) {
		val = resolve_symbol_ccl_program(object);
		return (VECTORP(val) ? Qt : Qnil);
	}
	if (!SYMBOLP(object))
		return Qnil;

	val = Fget(object, Qccl_program_idx, Qnil);
	return ((!NATNUMP(val)
		 || XINT(val) >= XVECTOR_LENGTH(Vccl_program_table))
		? Qnil : Qt);
}

DEFUN("ccl-execute", Fccl_execute, 2, 2, 0,	/*
						   Execute CCL-PROGRAM with registers initialized by REGISTERS.

						   CCL-PROGRAM is a CCL program name (symbol)
						   or a compiled code generated by `ccl-compile' (for backward compatibility,
						   in this case, the overhead of the execution is bigger than the former case).
						   No I/O commands should appear in CCL-PROGRAM.

						   REGISTERS is a vector of [R0 R1 ... R7] where RN is an initial value
						   of Nth register.

						   As side effect, each element of REGISTERS holds the value of
						   corresponding register after the execution.

						   See the documentation of `define-ccl-program' for the detail of CCL program.
						 */
      (ccl_prog, reg))
{
	struct ccl_program ccl;
	int i;

	if (setup_ccl_program(&ccl, ccl_prog) < 0)
		error("Invalid CCL program");

	CHECK_VECTOR(reg);
	if (XVECTOR_LENGTH(reg) != 8)
		error("Length of vector REGISTERS is not 8");

	for (i = 0; i < 8; i++)
		ccl.reg[i] = (INTP(XVECTOR_DATA(reg)[i])
			      ? XINT(XVECTOR_DATA(reg)[i])
			      : 0);

	ccl_driver(&ccl, (const unsigned char *)0,
		   (unsigned_char_dynarr *) 0, 0, (int *)0, CCL_MODE_ENCODING);
	QUIT;
	if (ccl.status != CCL_STAT_SUCCESS)
		error("Error in CCL program at %dth code", ccl.ic);

	for (i = 0; i < 8; i++)
		XSETINT(XVECTOR(reg)->contents[i], ccl.reg[i]);
	return Qnil;
}

DEFUN("ccl-execute-on-string", Fccl_execute_on_string, 3, 4, 0,	/*
								   Execute CCL-PROGRAM with initial STATUS on STRING.

								   CCL-PROGRAM is a symbol registered by register-ccl-program,
								   or a compiled code generated by `ccl-compile' (for backward compatibility,
								   in this case, the execution is slower).

								   Read buffer is set to STRING, and write buffer is allocated automatically.

								   STATUS is a vector of [R0 R1 ... R7 IC], where
								   R0..R7 are initial values of corresponding registers,
								   IC is the instruction counter specifying from where to start the program.
								   If R0..R7 are nil, they are initialized to 0.
								   If IC is nil, it is initialized to head of the CCL program.

								   If optional 4th arg CONTINUE is non-nil, keep IC on read operation
								   when read buffer is exhausted, else, IC is always set to the end of
								   CCL-PROGRAM on exit.

								   It returns the contents of write buffer as a string,
								   and as side effect, STATUS is updated.

								   See the documentation of `define-ccl-program' for the detail of CCL program.
								 */
      (ccl_prog, status, string, continue_))
{
	Lisp_Object val;
	struct ccl_program ccl;
	int i, produced;
	unsigned_char_dynarr *outbuf;
	struct gcpro gcpro1, gcpro2;

	if (setup_ccl_program(&ccl, ccl_prog) < 0)
		error("Invalid CCL program");

	CHECK_VECTOR(status);
	if (XVECTOR(status)->size != 9)
		error("Length of vector STATUS is not 9");
	CHECK_STRING(string);

	GCPRO2(status, string);

	for (i = 0; i < 8; i++) {
		if (NILP(XVECTOR_DATA(status)[i]))
			XSETINT(XVECTOR_DATA(status)[i], 0);
		if (INTP(XVECTOR_DATA(status)[i]))
			ccl.reg[i] = XINT(XVECTOR_DATA(status)[i]);
	}
	if (INTP(XVECTOR(status)->contents[i])) {
		i = XINT(XVECTOR_DATA(status)[8]);
		if (ccl.ic < i && i < ccl.size)
			ccl.ic = i;
	}
	outbuf = Dynarr_new(unsigned_char);
	ccl.last_block = NILP(continue_);
	produced = ccl_driver(&ccl, XSTRING_DATA(string), outbuf,
			      XSTRING_LENGTH(string),
			      (int *)0, CCL_MODE_DECODING);
	for (i = 0; i < 8; i++)
		XSETINT(XVECTOR_DATA(status)[i], ccl.reg[i]);
	XSETINT(XVECTOR_DATA(status)[8], ccl.ic);
	UNGCPRO;

	val = make_string(Dynarr_atp(outbuf, 0), produced);
	Dynarr_free(outbuf);
	QUIT;
	if (ccl.status == CCL_STAT_SUSPEND_BY_DST)
		error("Output buffer for the CCL programs overflow");
	if (ccl.status != CCL_STAT_SUCCESS
	    && ccl.status != CCL_STAT_SUSPEND_BY_SRC)
		error("Error in CCL program at %dth code", ccl.ic);

	return val;
}

DEFUN("register-ccl-program", Fregister_ccl_program, 2, 2, 0,	/*
								   Register CCL program CCL-PROG as NAME in `ccl-program-table'.
								   CCL-PROG should be a compiled CCL program (vector), or nil.
								   If it is nil, just reserve NAME as a CCL program name.
								   Return index number of the registered CCL program.
								 */
      (name, ccl_prog))
{
	int len = XVECTOR_LENGTH(Vccl_program_table);
	int idx;
	Lisp_Object resolved;

	CHECK_SYMBOL(name);
	resolved = Qnil;
	if (!NILP(ccl_prog)) {
		CHECK_VECTOR(ccl_prog);
		resolved = resolve_symbol_ccl_program(ccl_prog);
		if (!NILP(resolved)) {
			ccl_prog = resolved;
			resolved = Qt;
		}
	}

	for (idx = 0; idx < len; idx++) {
		Lisp_Object slot;

		slot = XVECTOR_DATA(Vccl_program_table)[idx];
		if (!VECTORP(slot))
			/* This is the first unused slot.  Register NAME here.  */
			break;

		if (EQ(name, XVECTOR_DATA(slot)[0])) {
			/* Update this slot.  */
			XVECTOR_DATA(slot)[1] = ccl_prog;
			XVECTOR_DATA(slot)[2] = resolved;
			return make_int(idx);
		}
	}

	if (idx == len) {
		/* Extend the table.  */
		Lisp_Object new_table;
		int j;

		new_table = Fmake_vector(make_int(len * 2), Qnil);
		for (j = 0; j < len; j++)
			XVECTOR_DATA(new_table)[j]
			    = XVECTOR_DATA(Vccl_program_table)[j];
		Vccl_program_table = new_table;
	}

	{
		Lisp_Object elt;

		elt = Fmake_vector(make_int(3), Qnil);
		XVECTOR_DATA(elt)[0] = name;
		XVECTOR_DATA(elt)[1] = ccl_prog;
		XVECTOR_DATA(elt)[2] = resolved;
		XVECTOR_DATA(Vccl_program_table)[idx] = elt;
	}

	Fput(name, Qccl_program_idx, make_int(idx));
	return make_int(idx);
}

/* Register code conversion map.
   A code conversion map consists of numbers, Qt, Qnil, and Qlambda.
   The first element is start code point.
   The rest elements are mapped numbers.
   Symbol t means to map to an original number before mapping.
   Symbol nil means that the corresponding element is empty.
   Symbol lambda means to terminate mapping here.
*/

DEFUN("register-code-conversion-map", Fregister_code_conversion_map, 2, 2, 0,	/*
										   Register SYMBOL as code conversion map MAP.
										   Return index number of the registered map.
										 */
      (symbol, map))
{
	int len = XVECTOR_LENGTH(Vcode_conversion_map_vector);
	int i;
	Lisp_Object idx;

	CHECK_SYMBOL(symbol);
	CHECK_VECTOR(map);

	for (i = 0; i < len; i++) {
		Lisp_Object slot = XVECTOR_DATA(Vcode_conversion_map_vector)[i];

		if (!CONSP(slot))
			break;

		if (EQ(symbol, XCAR(slot))) {
			idx = make_int(i);
			XCDR(slot) = map;
			Fput(symbol, Qcode_conversion_map, map);
			Fput(symbol, Qcode_conversion_map_id, idx);
			return idx;
		}
	}

	if (i == len) {
		Lisp_Object new_vector = Fmake_vector(make_int(len * 2), Qnil);
		int j;

		for (j = 0; j < len; j++)
			XVECTOR_DATA(new_vector)[j]
			    = XVECTOR_DATA(Vcode_conversion_map_vector)[j];
		Vcode_conversion_map_vector = new_vector;
	}

	idx = make_int(i);
	Fput(symbol, Qcode_conversion_map, map);
	Fput(symbol, Qcode_conversion_map_id, idx);
	XVECTOR_DATA(Vcode_conversion_map_vector)[i] = Fcons(symbol, map);
	return idx;
}

void syms_of_mule_ccl(void)
{
	DEFSUBR(Fccl_program_p);
	DEFSUBR(Fccl_execute);
	DEFSUBR(Fccl_execute_on_string);
	DEFSUBR(Fregister_ccl_program);
	DEFSUBR(Fregister_code_conversion_map);
}

void vars_of_mule_ccl(void)
{
	staticpro(&Vccl_program_table);
	Vccl_program_table = Fmake_vector(make_int(32), Qnil);

	defsymbol(&Qccl_program, "ccl-program");
	defsymbol(&Qccl_program_idx, "ccl-program-idx");
	defsymbol(&Qcode_conversion_map, "code-conversion-map");
	defsymbol(&Qcode_conversion_map_id, "code-conversion-map-id");

	DEFVAR_LISP("code-conversion-map-vector", &Vcode_conversion_map_vector	/*
										   Vector of code conversion maps.
										 */ );
	Vcode_conversion_map_vector = Fmake_vector(make_int(16), Qnil);

	DEFVAR_LISP("font-ccl-encoder-alist", &Vfont_ccl_encoder_alist	/*
									   Alist of fontname patterns vs corresponding CCL program.
									   Each element looks like (REGEXP . CCL-CODE),
									   where CCL-CODE is a compiled CCL program.
									   When a font whose name matches REGEXP is used for displaying a character,
									   CCL-CODE is executed to calculate the code point in the font
									   from the charset number and position code(s) of the character which are set
									   in CCL registers R0, R1, and R2 before the execution.
									   The code point in the font is set in CCL registers R1 and R2
									   when the execution terminated.
									   If the font is single-byte font, the register R2 is not used.
									 */ );
	Vfont_ccl_encoder_alist = Qnil;
}

#endif				/* emacs */
