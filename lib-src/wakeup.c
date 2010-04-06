/* Program to produce output at regular intervals.  */

#include <config.h>

#if __STDC__ || defined(STDC_HEADERS)
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif

#include <stdio.h>
#include <sys/types.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

int main(int argc, char *argv[])
{
	int period = 60;

	if (argc > 1)
		period = atoi(argv[1]);

	while (1) {
		/* Make sure wakeup stops when Emacs goes away.  */
		if (getppid() == 1)
			return 0;
		printf("Wake up!\n");
		/* If fflush fails, then our stdout pipe is broken. */
		if (fflush(stdout) != 0)
			return 0;
		/* If using a period of 60, produce the output when the minute
		   changes. */
		if (period == 60) {
			time_t when;
			struct tm *tp;
			time(&when);
			tp = localtime(&when);
			sleep(60 - tp->tm_sec);
		} else
			sleep(period);
	}
}
