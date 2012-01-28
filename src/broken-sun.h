/*

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

/* Authorship:

   JWZ: long ago.
 */

/* Sun's standard and GCC's header files leave out prototypes for
   all sorts of functions. */

#ifndef INCLUDED_broken_sun_h_
#define INCLUDED_broken_sun_h_

#ifdef __GNUC__
#include <stdlib.h>
#include <stddef.h>

/*********************** stdlib functions *********************/

/* void *	memchr (const void *, int, size_t); */

/* int	memcmp (const void *, const void *, size_t); */
/* void *	memcpy (void *, const void *, size_t); */
/* void *	memmove (void *, const void *, size_t);*/
/* void *	memset (void *, int, int); */
/* char *	strcat (char *, const char *); */
/* char *	strchr (const char *, int); */
/* int	strcmp (const char *, const char *); */
int strcasecmp(char *, char *);

/* Yes, they even left these functions out! */
int tolower(int);
int toupper(int);

/*********************** stdio functions *********************/

#include <stdio.h>		/* else can't declare FILE */

/* FILE	*fopen (const char *, const char *); */
/* FILE	*freopen (const char *, const char *, FILE *); */
FILE *tmpfile(void);
int fclose(FILE *);
char *fgets(char *, int, FILE *);
int fgetc(FILE *);
int fflush(FILE *);
int fprintf(FILE *, const char *, ...);
int fputc(char, FILE *);
int fputs(const char *, FILE *);
size_t fread(void *, size_t, size_t, FILE *);
int fscanf(FILE *, const char *, ...);
int fgetpos(FILE *, long *);
int fseek(FILE *, long, int);
int fsetpos(FILE *, const long *);
long ftell(FILE *);
size_t fwrite(const void *, size_t, size_t, FILE *);
char *gets(char *);
int pclose(FILE *);
void perror(const char *);
int printf(const char *, ...);
int puts(const char *);
int remove(const char *);
int rename(const char *, const char *);
int rewind(FILE *);
int scanf(const char *, ...);
int sscanf(const char *, const char *, ...);
void setbuf(FILE *, char *);
int setvbuf(FILE *, char *, int, size_t);
int ungetc(int, FILE *);
int vprintf(const char *, void *);
int vfprintf(FILE *, const char *, void *);
char *vsprintf(char *, const char *, void *);

/*********************** signal functions *********************/

int sigblock(int);
#ifndef sigmask
int sigmask(int);
#endif
int sigsetmask(int);
int sigpause(int);

/*********************** time functions ***********************/

struct timeval;
struct timezone;

int utimes(const char *, struct timeval *);
void tzset(void);
time_t time(time_t *);
int gettimeofday(struct timeval *, struct timezone *);

/*********************** file-system functions *********************/

struct stat;
#include </usr/include/sys/types.h>

int fsync(int);
int lstat(const char *, struct stat *);
int fchmod(int, mode_t);
char *mktemp(char *);
/* int	creat (const char *, mode_t); better no decl than a conflicting one... */
int symlink(const char *, const char *);
int readlink(const char *, char *, int);
void sync(void);
int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
char *getwd(char *);
/* int	lseek (int, long, int); better no decl than a conflicting one... */
int _filbuf();
int _flsbuf();

/**************** interprocess communication functions ******************/

int recv(int, char *, int, int);
int socket(int, int, int);
struct sockaddr;
int connect(int, struct sockaddr *, int);
int bind(int, struct sockaddr *, int);
int listen(int, int);
int accept(int, struct sockaddr *, int *);
int gethostname(char *, int);
struct rusage;
int wait3(void *, int, struct rusage *);
int nice(int);
int killpg(int, int);
int system(char *);

/*********************** low-level OS functions *********************/

int ioctl(int, int, ...);
struct nlist;
int nlist(const char *, struct nlist *);
int munmap(void *, int);
int brk(void *);
void *sbrk(int);
struct rlimit;
int getrlimit(int, struct rlimit *);
int getpagesize(void);
int shutdown(int, int);
int mprotect(void *, int, int);

/*********************** miscellaneous functions *********************/

void tputs(const char *cp, int affcnt, void (*)(int));
long random(void);
int srandom(int seed);

#endif				/* __GNUC__ */

#endif				/* INCLUDED_broken_sun_h_ */
