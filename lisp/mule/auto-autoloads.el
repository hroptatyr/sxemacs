;;; DO NOT MODIFY THIS FILE
(if (featurep 'mule-autoloads) (error "Already loaded"))

;;;### (autoloads (ccl-execute-with-args check-ccl-program define-ccl-program declare-ccl-program ccl-dump ccl-compile) "mule-ccl" "mule/mule-ccl.el")

(autoload 'ccl-compile "mule-ccl" "\
Return a compiled code of CCL-PROGRAM as a vector of integer." nil nil)

(autoload 'ccl-dump "mule-ccl" "\
Disassemble compiled CCL-CODE." nil nil)

(autoload 'declare-ccl-program "mule-ccl" "\
Declare NAME as a name of CCL program.

This macro exists for backward compatibility.  In the old version of
Emacs, to compile a CCL program which calls another CCL program not
yet defined, it must be declared as a CCL program in advance.  But,
now CCL program names are resolved not at compile time but before
execution.

Optional arg VECTOR is a compiled CCL code of the CCL program." nil 'macro)

(autoload 'define-ccl-program "mule-ccl" "\
Set NAME the compiled code of CCL-PROGRAM.

CCL-PROGRAM has this form:
	(BUFFER_MAGNIFICATION
	 CCL_MAIN_CODE
	 [ CCL_EOF_CODE ])

BUFFER_MAGNIFICATION is an integer value specifying the approximate
output buffer magnification size compared with the bytes of input data
text.  If the value is zero, the CCL program can't execute `read' and
`write' commands.

CCL_MAIN_CODE and CCL_EOF_CODE are CCL program codes.  CCL_MAIN_CODE
executed at first.  If there's no more input data when `read' command
is executed in CCL_MAIN_CODE, CCL_EOF_CODE is executed.  If
CCL_MAIN_CODE is terminated, CCL_EOF_CODE is not executed.

Here's the syntax of CCL program code in BNF notation.  The lines
starting by two semicolons (and optional leading spaces) describe the
semantics.

CCL_MAIN_CODE := CCL_BLOCK

CCL_EOF_CODE := CCL_BLOCK

CCL_BLOCK := STATEMENT | (STATEMENT [STATEMENT ...])

STATEMENT :=
	SET | IF | BRANCH | LOOP | REPEAT | BREAK | READ | WRITE | CALL
	| TRANSLATE | END

SET :=	(REG = EXPRESSION)
	| (REG ASSIGNMENT_OPERATOR EXPRESSION)
	;; The following form is the same as (r0 = integer).
	| integer

EXPRESSION := ARG | (EXPRESSION OPERATOR ARG)

;; Evaluate EXPRESSION.  If the result is nonzeor, execute
;; CCL_BLOCK_0.  Otherwise, execute CCL_BLOCK_1.
IF :=	(if EXPRESSION CCL_BLOCK_0 CCL_BLOCK_1)

;; Evaluate EXPRESSION.  Provided that the result is N, execute
;; CCL_BLOCK_N.
BRANCH := (branch EXPRESSION CCL_BLOCK_0 [CCL_BLOCK_1 ...])

;; Execute STATEMENTs until (break) or (end) is executed.
LOOP := (loop STATEMENT [STATEMENT ...])

;; Terminate the most inner loop.
BREAK := (break)

REPEAT :=
	;; Jump to the head of the most inner loop.
	(repeat)
	;; Same as: ((write [REG | integer | string])
	;;	     (repeat))
	| (write-repeat [REG | integer | string])
	;; Same as: ((write REG [ARRAY])
	;;	     (read REG)
	;;	     (repeat))
	| (write-read-repeat REG [ARRAY])
	;; Same as: ((write integer)
	;;	     (read REG)
	;;	     (repeat))
	| (write-read-repeat REG integer)

READ := ;; Set REG_0 to a byte read from the input text, set REG_1
	;; to the next byte read, and so on.
	(read REG_0 [REG_1 ...])
	;; Same as: ((read REG)
	;;	     (if (REG OPERATOR ARG) CCL_BLOCK_0 CCL_BLOCK_1))
	| (read-if (REG OPERATOR ARG) CCL_BLOCK_0 CCL_BLOCK_1)
	;; Same as: ((read REG)
	;;	     (branch REG CCL_BLOCK_0 [CCL_BLOCK_1 ...]))
	| (read-branch REG CCL_BLOCK_0 [CCL_BLOCK_1 ...])
	;; Read a character from the input text while parsing
	;; multibyte representation, set REG_0 to the charset ID of
	;; the character, set REG_1 to the code point of the
	;; character.  If the dimension of charset is two, set REG_1
	;; to ((CODE0 << 8) | CODE1), where CODE0 is the first code
	;; point and CODE1 is the second code point.
	| (read-multibyte-character REG_0 REG_1)

WRITE :=
	;; Write REG_0, REG_1, ... to the output buffer.  If REG_N is
	;; a multibyte character, write the corresponding multibyte
	;; representation.
	(write REG_0 [REG_1 ...])
	;; Same as: ((r7 = EXPRESSION)
	;;	     (write r7))
	| (write EXPRESSION)
	;; Write the value of `integer' to the output buffer.  If it
	;; is a multibyte character, write the corresponding multibyte
	;; representation.
	| (write integer)
	;; Write the byte sequence of `string' as is to the output
	;; buffer.  It is encoded by binary coding system, thus,
        ;; by this operation, you cannot write multibyte string
        ;; as it is.
	| (write string)
	;; Same as: (write string)
	| string
	;; Provided that the value of REG is N, write Nth element of
	;; ARRAY to the output buffer.  If it is a multibyte
	;; character, write the corresponding multibyte
	;; representation.
	| (write REG ARRAY)
	;; Write a multibyte representation of a character whose
	;; charset ID is REG_0 and code point is REG_1.  If the
	;; dimension of the charset is two, REG_1 should be ((CODE0 <<
	;; 8) | CODE1), where CODE0 is the first code point and CODE1
	;; is the second code point of the character.
	| (write-multibyte-character REG_0 REG_1)

;; Call CCL program whose name is ccl-program-name.
CALL := (call ccl-program-name)

;; Terminate the CCL program.
END := (end)

;; CCL registers that can contain any integer value.  As r7 is also
;; used by CCL interpreter, its value is changed unexpectedly.
REG := r0 | r1 | r2 | r3 | r4 | r5 | r6 | r7

ARG := REG | integer

OPERATOR :=
	;; Normal arithmethic operators (same meaning as C code).
	+ | - | * | / | %

	;; Bitwize operators (same meaning as C code)
	| & | `|' | ^

	;; Shifting operators (same meaning as C code)
	| << | >>

	;; (REG = ARG_0 <8 ARG_1) means:
	;;	(REG = ((ARG_0 << 8) | ARG_1))
	| <8

	;; (REG = ARG_0 >8 ARG_1) means:
	;;	((REG = (ARG_0 >> 8))
	;;	 (r7 = (ARG_0 & 255)))
	| >8

	;; (REG = ARG_0 // ARG_1) means:
	;;	((REG = (ARG_0 / ARG_1))
	;;	 (r7 = (ARG_0 % ARG_1)))
	| //

	;; Normal comparing operators (same meaning as C code)
	| < | > | == | <= | >= | !=

	;; If ARG_0 and ARG_1 are higher and lower byte of Shift-JIS
	;; code, and CHAR is the corresponding JISX0208 character,
	;; (REG = ARG_0 de-sjis ARG_1) means:
	;;	((REG = CODE0)
	;;	 (r7 = CODE1))
	;; where CODE0 is the first code point of CHAR, CODE1 is the
	;; second code point of CHAR.
	| de-sjis

	;; If ARG_0 and ARG_1 are the first and second code point of
	;; JISX0208 character CHAR, and SJIS is the correponding
	;; Shift-JIS code,
	;; (REG = ARG_0 en-sjis ARG_1) means:
	;;	((REG = HIGH)
	;;	 (r7 = LOW))
	;; where HIGH is the higher byte of SJIS, LOW is the lower
	;; byte of SJIS.
	| en-sjis

ASSIGNMENT_OPERATOR :=
	;; Same meaning as C code
	+= | -= | *= | /= | %= | &= | `|=' | ^= | <<= | >>=

	;; (REG <8= ARG) is the same as:
	;;	((REG <<= 8)
	;;	 (REG |= ARG))
	| <8= 

	;; (REG >8= ARG) is the same as:
	;;	((r7 = (REG & 255))
	;;	 (REG >>= 8))

	;; (REG //= ARG) is the same as:
	;;	((r7 = (REG % ARG))
	;;	 (REG /= ARG))
	| //=

ARRAY := `[' integer ... `]'


TRANSLATE :=
	(translate-character REG(table) REG(charset) REG(codepoint))
	| (translate-character SYMBOL REG(charset) REG(codepoint))
MAP :=
     (iterate-multiple-map REG REG MAP-IDs)
     | (map-multiple REG REG (MAP-SET))
     | (map-single REG REG MAP-ID)
MAP-IDs := MAP-ID ...
MAP-SET := MAP-IDs | (MAP-IDs) MAP-SET
MAP-ID := integer
" nil 'macro)

(autoload 'check-ccl-program "mule-ccl" "\
Check validity of CCL-PROGRAM.
If CCL-PROGRAM is a symbol denoting a CCL program, return
CCL-PROGRAM, else return nil.
If CCL-PROGRAM is a vector and optional arg NAME (symbol) is supplied,
register CCL-PROGRAM by name NAME, and return NAME." nil 'macro)

(autoload 'ccl-execute-with-args "mule-ccl" "\
Execute CCL-PROGRAM with registers initialized by the remaining args.
The return value is a vector of resulting CCL registers.

See the documentation of `define-ccl-program' for the detail of CCL program." nil nil)

;;;***

(provide 'mule-autoloads)
