#include "bsdos3.h"

/* BSD/OS seems to have switched to ELF format for executables. */
#ifdef __ELF__

#undef ORDINARY_LINK
#define ORDINARY_LINK 1
/*
 * everything is pdump now. --SY
 * #define UNEXEC unexelf.o
 */
#undef UNEXEC
#endif				/* ELF */
