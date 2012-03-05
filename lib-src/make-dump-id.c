/* Generate a unique dump-id for use with the portable dumper.
   Copyright (C) 2000 Olivier Galibert, Martin Buchholz

This file is part of SXEmacs.

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

#include "../src/config.h"
#include <stdio.h>
#include <stdlib.h>
#include "../src/systime.h"

/* Generates an (extremely) pseudo random number for the dump-id */
static unsigned int generate_dump_id(void)
{
	EMACS_TIME thyme;
	EMACS_GET_TIME(thyme);

	return (unsigned int)(EMACS_SECS(thyme) ^ EMACS_USECS(thyme));
}

int main(int argc, char *argv[])
{
	FILE *f;

	if ((f = fopen("dump-id.c", "w")) == NULL) {
		perror("open dump-id.c");
		return EXIT_FAILURE;
	}

	fprintf(f, "unsigned int dump_id = %uU;\n", generate_dump_id());

	if ((fclose(f)) != 0) {
		perror("close dump-id.c");
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
