/* movemail foo bar -- move file foo to file bar,
   locking file foo.
   Copyright (C) 1986, 1992, 1993, 1994, 1996 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.

 Please mail bugs and suggestions to the XEmacs maintainer.
*/

/* Important notice:
 *
 *  You *must* coordinate the locking method used by movemail with that
 *  used by your mail delivery agent, as well as that of the other mail
 *  user agents on your system.  movemail allows you to do this at run
 *  time via the -m flag.  Moreover, it uses a default determined by
 *  the MAIL_LOCK_DOT, MAIL_LOCK_LOCKF, MAIL_LOCK_FLOCK,
 *  MAIL_LOCK_LOCKING, and MAIL_LOCK_MMDF preprocessor settings.
 */

/*
 * Mike Sperber <sperber@informatik.uni-tuebingen.de> reorganized
 * everything that has to with locking in December 1999.
 */

/*
 * Modified January, 1986 by Michael R. Gretzinger (Project Athena)
 *
 * Added POP (Post Office Protocol) service.  When compiled -DMAIL_USE_POP
 * movemail will accept input filename arguments of the form
 * "po:username".  This will cause movemail to open a connection to
 * a pop server running on $MAILHOST (environment variable).  Movemail
 * must be setuid to root in order to work with POP.
 * 
 * New module: popmail.c
 * Modified routines:
 *	main - added code within #ifdef MAIL_USE_POP; added setuid (getuid ())
 *		after POP code. 
 * New routines in movemail.c:
 *	get_errmsg - return pointer to system error message
 *
 * Modified August, 1993 by Jonathan Kamens (OpenVision Technologies)
 *
 * Move all of the POP code into a separate file, "pop.c".
 * Use strerror instead of get_errmsg.
 *
 */

#define NO_SHORTNAMES   /* Tell config not to load remap.h */
#define DONT_ENCAPSULATE
#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include "../src/sysfile.h"
#include "../src/syswait.h"
#ifndef WIN32_NATIVE
#include "../src/systime.h"
#endif
#include <stdlib.h>
#include <string.h>
#include "getopt.h"
#ifdef MAIL_USE_POP
#include "pop.h"
#include "../src/regex.h"
#endif

extern char *optarg;
extern int optind, opterr;

#ifndef HAVE_STRERROR
char * strerror (int errnum);
#endif /* HAVE_STRERROR */

#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif
#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(_c_) ((_c_) == DIRECTORY_SEP)
#endif

#ifdef WIN32_NATIVE
#undef access
#undef unlink
#define fork() 0
#define sys_wait(var) (*(var) = 0)
/* Unfortunately, Samba doesn't seem to properly lock Unix files even
   though the locking call succeeds (and indeed blocks local access from
   other NT programs).  If you have direct file access using an NFS
   client or something other than Samba, the locking call might work
   properly - make sure it does before you enable this! */
#define DISABLE_DIRECT_ACCESS
#include <io.h>
#endif /* WIN32_NATIVE */

#if defined (HAVE_UNISTD_H)
#include <unistd.h>
#endif /* unistd.h */
#ifndef F_OK
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#endif /* No F_OK */

#if defined (HAVE_FCNTL_H)
#include <fcntl.h>
#endif /* fcntl.h */

#ifdef HAVE_LOCKING
#include <sys/locking.h>
#endif

#ifdef HAVE_MMDF
extern int lk_open (), lk_close ();
#endif

/* Cancel substitutions made by config.h for Emacs.  */
#undef open
#undef read
#undef write
#undef close

static void fatal (char *, char*);
static void error (char *, char *, char *);
static void usage(int);
static void pfatal_with_name (char *);
static void pfatal_and_delete (char *);
static char *concat (char *, char *, char *);
static long *xmalloc (unsigned int);
#ifdef MAIL_USE_POP
static int popmail (char *, char *, char *);
static int pop_retr (popserver server, int msgno,
		     int (*action)(char *, FILE *), FILE *arg);
static int mbx_write (char *, FILE *);
static int mbx_delimit_begin (FILE *);
static int mbx_delimit_end (FILE *);
static struct re_pattern_buffer* compile_regex (char* regexp_pattern);
static int pop_search_top (popserver server, int msgno, int lines, 
			   struct re_pattern_buffer* regexp);
#endif

int verbose=0;
#ifdef MAIL_USE_POP
int reverse=0;
int keep_messages=0;
struct re_pattern_buffer* regexp_pattern=0;
int match_lines=10;
#endif

#define VERBOSE(x) if (verbose) { printf x; fflush(stdout); }

struct option longopts[] =
{
  { "inbox",			required_argument,	   NULL,	'i'	},
  { "outfile",			required_argument,	   NULL,	'o'	},
#ifdef MAIL_USE_POP
  { "password",			required_argument,	   NULL,	'p'	},
  { "reverse-pop-order",		no_argument,		   NULL,	'x'	},
  { "keep-messages",		no_argument,		   NULL,	'k'	},
  { "regex",			required_argument,	   NULL,	'r' 	},
  { "match-lines",		required_argument,	   NULL,	'l' 	},
#endif
  { "lock-method",		required_argument,	   NULL,	'm' 	},
  { "help",			no_argument,	   	   NULL,	'h' 	},
  { "verbose", 			no_argument,		   NULL,	'v'	},
  { 0 }
};

#define DOTLOCKING	0
#define FLOCKING 	1
#define LOCKFING	2
#define MMDF   		3
#define LOCKING         4

#if defined(MAIL_LOCK_FLOCK) && defined(HAVE_FLOCK)
#define DEFAULT_LOCKING FLOCKING
#elif defined(MAIL_LOCK_LOCKF) && defined(HAVE_LOCKF)
#define DEFAULT_LOCKING LOCKFING
#elif defined(MAIL_LOCK_MMDF) && defined(HAVE_MMDF)
#define DEFAULT_LOCKING MMDF
#elif defined(MAIL_LOCK_LOCKING) && defined(HAVE_LOCKING)
#define DEFAULT_LOCKING LOCKING
#else
#define DEFAULT_LOCKING DOTLOCKING
#endif

#ifndef DISABLE_DIRECT_ACCESS
static void lock_dot(char *);
#endif
static void unlock_dot(char *);
static int parse_lock_method(char *);
static char *unparse_lock_method(int);

int
main (int argc, char *argv[])
{
  char *inname=0, *outname=0, *poppass=0;
#ifndef DISABLE_DIRECT_ACCESS
  int indesc, outdesc;
  int nread;
  int status;
#endif

  int lock_method = DEFAULT_LOCKING;

  char *maybe_lock_env;

  maybe_lock_env = getenv("EMACSLOCKMETHOD");
  if (maybe_lock_env)
    {
      printf("maybe-lock_env: %s\n", maybe_lock_env);
      lock_method = parse_lock_method(maybe_lock_env);
    }

  for (;;)
    {
#ifdef MAIL_USE_POP
      char* optstring = "i:o:m:p:l:r:xvhk";
#else
      char* optstring = "i:o:m:vh";
#endif
      int opt = getopt_long (argc, argv, optstring, longopts, 0);
  
      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  break;
	case 1:			/* one of the standard arguments seen */
	  if (!inname)
	    inname = optarg;
	  else if (!outname)
	    outname = optarg;
	  else
	    poppass = optarg;
	  break;

	case 'i':		/* infile */
	  inname = optarg;
	  break;
	  
	case 'o':		/* outfile */
	  outname = optarg;
	  break;
#ifdef MAIL_USE_POP
	case 'p':		/* pop password */
	  poppass = optarg;	
	  break;
	case 'k':		keep_messages=1;	break;
	case 'x':		reverse = 1;		break;
	case 'l':		/* lines to match */
	  match_lines = atoi (optarg);
	  break;

	case 'r':		/* regular expression */
	  regexp_pattern = compile_regex (optarg);
	  break;
#endif

	case 'm':
	  lock_method = parse_lock_method(optarg);
	  break;
	case 'h':
	  usage(lock_method);
	  exit(0);
	case 'v':
	  verbose = 1;
	  break;
	}
    }

  while (optind < argc)
      {
	  if (!inname)
	      inname = argv[optind];
	  else if (!outname)
	      outname = argv[optind];
	  else
	      poppass = argv[optind];
	  optind++;
      }
    
  if (!inname || !outname)
    {
      usage(lock_method);
      exit(1);
    }

#ifdef HAVE_MMDF
  if (lock_method == MMDF)
    mmdf_init (argv[0]);
#endif

  if (*outname == 0)
    fatal ("Destination file name is empty", 0);

  VERBOSE(("checking access to output file\n"));
  /* Check access to output file.  */
  if (access (outname, F_OK) == 0 && access (outname, W_OK) != 0)
    pfatal_with_name (outname);

  /* Also check that outname's directory is writable to the real uid.  */
  {
    char *buf = (char *) xmalloc (strlen (outname) + 1);
    char *cp;
    strcpy (buf, outname);
    cp = buf + strlen (buf);
    while (cp > buf && !IS_DIRECTORY_SEP (cp[-1]))
      *--cp = 0;
    if (cp == buf)
      *cp++ = '.';
    if (access (buf, W_OK) != 0)
      pfatal_with_name (buf);
    free (buf);
  }

#ifdef MAIL_USE_POP
  if (!strncmp (inname, "po:", 3))
    {
      int retcode = popmail (inname + 3, outname, poppass);
      exit (retcode);
    }

#ifndef WIN32_NATIVE
  setuid (getuid ());
#endif
#endif /* MAIL_USE_POP */

#ifndef DISABLE_DIRECT_ACCESS

  /* Check access to input file.  */
  if (access (inname, R_OK | W_OK) != 0)
    pfatal_with_name (inname);


  if (fork () == 0)
    {
      setuid (getuid ());

      VERBOSE(("opening input file\n"));

      switch (lock_method)
	{
	case DOTLOCKING:
	  indesc = open (inname, O_RDONLY);
	  break;
#ifdef HAVE_LOCKF
	case LOCKFING:
	  indesc = open (inname, O_RDWR);
	  break;
#endif
#ifdef HAVE_FLOCK
	case FLOCKING:
	  indesc = open (inname, O_RDWR);
	  break;
#endif
#ifdef HAVE_LOCKING
	case LOCKING:
	  indesc = open (inname, O_RDWR);
	  break;
#endif
#ifdef HAVE_MMDF
	case MMDF:
	  indesc = lk_open (inname, O_RDONLY, 0, 0, 10);
	  break;
#endif
	default: abort();
	}

      if (indesc < 0)
	pfatal_with_name (inname);

#ifdef HAVE_UMASK      
      /* In case movemail is setuid to root, make sure the user can
	 read the output file.  */
      umask (umask (0) & 0333);
#endif

      outdesc = open (outname, O_WRONLY | O_CREAT | O_EXCL, 0666);
      if (outdesc < 0)
	pfatal_with_name (outname);

      VERBOSE(("locking input file\n"));

      switch (lock_method)
	{
#ifdef HAVE_LOCKF
	case LOCKFING:
	  if (lockf (indesc, F_LOCK, 0) < 0)
	    pfatal_with_name (inname);
	  break;
#endif
#ifdef HAVE_FLOCK
	case FLOCKING:
	  if (flock (indesc, LOCK_EX) < 0)
	    pfatal_with_name (inname);
	  break;
#endif
#ifdef HAVE_LOCKING
	case LOCKING:
	  if (locking (indesc, LK_RLCK, -1L) < 0)
	    pfatal_with_name (inname);
	  break;
#endif
	case DOTLOCKING:
	  lock_dot(inname);
	  break;
	}

      VERBOSE(("copying input file to output file\n"));
	    
      {
	char buf[1024];

	while (1)
	  {
	    nread = read (indesc, buf, sizeof buf);
	    if (nread != write (outdesc, buf, nread))
	      {
		int saved_errno = errno;
		unlink (outname);
		errno = saved_errno;
		pfatal_with_name (outname);
	      }
	    if (nread < (int) sizeof buf)
	      break;
	  }
      }

#ifdef HAVE_FSYNC
      if (fsync (outdesc) < 0)
	pfatal_and_delete (outname);
#endif

      /* Check to make sure no errors before we zap the inbox.  */
      if (close (outdesc) != 0)
	pfatal_and_delete (outname);

      VERBOSE(("deleting or truncating input file\n"));

      switch (lock_method)
	{
	case LOCKFING:
	case FLOCKING:
	case LOCKING:
#ifdef HAVE_FTRUNCATE
	  ftruncate (indesc, 0L);
#else
	  close (open (inname, O_CREAT | O_TRUNC | O_RDWR, 0666));
#endif
	  close (indesc);
	  break;
#ifdef HAVE_MMDF
	case MMDF:
	  lk_close (indesc, 0, 0, 0);
	  break;
#endif
	case DOTLOCKING:
	  creat (inname, 0600);
	  break;
	}

      exit (0);
    }

  wait (&status);
  if (!WIFEXITED (status))
    exit (1);
  else if (WEXITSTATUS (status) != 0)
    exit (WEXITSTATUS (status));

  if (lock_method == DOTLOCKING)
    unlock_dot(inname);

#endif /* not DISABLE_DIRECT_ACCESS */

  return 0;
}

static void
usage(int lock_method)
{
  printf ("Usage: movemail [-rvxkh] [-l lines ] [-m method ] [-i] inbox [-o] destfile [[-p] POP-password]\n");
  printf("where method is one of: dot");
#ifdef HAVE_LOCKF
  printf(", lockf");
#endif
#ifdef HAVE_FLOCK
  printf(", flock");
#endif
#ifdef HAVE_MMDF
  printf(", mmdf");
#endif
#ifdef HAVE_LOCKING
  printf(", locking");
#endif
  printf("\nDefault is: %s\n", unparse_lock_method(lock_method));
  
}

static char *
unparse_lock_method(int lock_method)
{
  switch (lock_method)
    {
    case DOTLOCKING: return "dot";
    case FLOCKING:   return "flock";
    case LOCKFING:   return "lockf";
    case LOCKING:    return "locking";
    case MMDF:       return "mmdf";
    default: abort();return 0;
    }
}

static int
parse_lock_method(char *method_name)
{
  if (!strcmp("dot", method_name) || !strcmp("file", method_name))
    return DOTLOCKING;
#ifdef HAVE_LOCKF
  else if (!strcmp("lockf", method_name))
    return LOCKFING;
#endif
#ifdef HAVE_FLOCK
  else if (!strcmp("flock", method_name))
    return FLOCKING;
#endif
#ifdef HAVE_MMDF
  else if (!strcmp("mmdf", method_name))
    return MMDF;
#endif
#ifdef HAVE_LOCKING
  else if (!strcmp("locking", method_name))
    return LOCKING;
#endif
  else
    fatal("invalid lock method: %s", method_name);
  return 0; /* unreached */
}

static char *
dot_filename(char *filename)
{
  return concat (filename, ".lock", "");
}

static char *dotlock_filename = NULL;

#ifndef DISABLE_DIRECT_ACCESS
static void
lock_dot(char *filename)
{
  struct stat st;
  long now;
  int tem;
  char *lockname, *p;
  char *tempname;
  int desc;

  dotlock_filename = (char *) xmalloc(strlen(filename) + 1);

  /* Use a lock file named after our first argument with .lock appended:
     If it exists, the mail file is locked. */

  lockname = dot_filename(filename);
  tempname = (char *) xmalloc (strlen (filename) + strlen ("EXXXXXX") + 1);
  strcpy (tempname, filename);
  p = tempname + strlen (tempname);
  while (p != tempname && !IS_DIRECTORY_SEP (p[-1]))
    p--;
  *p = 0;
  strcpy (p, "EXXXXXX");
  mktemp (tempname);
  unlink (tempname);

  for (;;)
    {
      /* Create the lock file, but not under the lock file name.  */
      /* Give up if cannot do that.  */
      desc = open (tempname, O_WRONLY | O_CREAT | O_EXCL, 0666);
      if (desc < 0)
	{
	  char *message = (char *) xmalloc (strlen (tempname) + 50);
	  sprintf (message, "%s--see source file lib-src/movemail.c",
		   tempname);
	  pfatal_with_name (message);
	}
      close (desc);

      tem = link (tempname, lockname);
      unlink (tempname);
      if (tem >= 0)
	break;
      sleep (1);

      /* If lock file is five minutes old, unlock it.
	 Five minutes should be good enough to cope with crashes
	 and wedgitude, and long enough to avoid being fooled
	 by time differences between machines.  */
      if (stat (lockname, &st) >= 0)
	{
	  now = time (0);
	  if (st.st_ctime < now - 300)
	    unlink (lockname);
	}
    }
  strcpy(dotlock_filename, filename);
}
#endif /* not DISABLE_DIRECT_ACCESS */

static void
unlock_dot(char *filename)
{
  unlink(dot_filename(filename));
}

static void
maybe_unlock_dot(void)
{
  if (dotlock_filename)
    unlock_dot(dotlock_filename);
}

/* Print error message and exit.  */

static void
fatal (char *s1, char *s2)
{
  maybe_unlock_dot();
  error (s1, s2, NULL);
  exit (1);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

static void
error (char *s1, char *s2, char *s3)
{
  fprintf (stderr, "movemail: ");
  fprintf (stderr, s1, s2, s3);
  fprintf (stderr, "\n");
}

static void
pfatal_with_name (char *name)
{
  char *s = concat ("", strerror (errno), " for %s");
  fatal (s, name);
}

static void
pfatal_and_delete (char *name)
{
  char *s = concat ("", strerror (errno), " for %s");
  unlink (name);
  fatal (s, name);
}

/* Return a newly-allocated string whose contents concatenate those of s1, s2, s3.  */

static char *
concat (char *s1, char *s2, char *s3)
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Like malloc but get fatal error if memory is exhausted.  */

static long *
xmalloc (unsigned int size)
{
  long *result = (long *) malloc (size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* This is the guts of the interface to the Post Office Protocol.  */

#ifdef MAIL_USE_POP

#ifndef WIN32_NATIVE
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#else
#undef _WINSOCKAPI_
#include <winsock.h>
#endif
#include <stdio.h>
#include "../src/syspwd.h"

#define POP_ERROR 	(-1)
#define POP_RETRIEVED (0)
#define POP_DONE (1)

char *progname;
FILE *sfi;
FILE *sfo;
char ibuffer[BUFSIZ];
char obuffer[BUFSIZ];
char Errmsg[80];

static int
popmail (char *user, char *outfile, char *password)
{
  int nmsgs, nbytes;
  register int i, idx;
  int mbfi;
  short* retrieved_list;
  FILE *mbf;
  popserver server;

  VERBOSE(("opening server\n"));
  server = pop_open (0, user, password, POP_NO_GETPASS);
  if (! server)
    {
      error (pop_error, NULL, NULL);
      return (1);
    }

  VERBOSE(("stat'ing messages\n"));
  if (pop_stat (server, &nmsgs, &nbytes))
    {
      error (pop_error, NULL, NULL);
      return (1);
    }

  if (!nmsgs)
    {
      VERBOSE(("closing server\n"));
      pop_close (server);
      return (0);
    }

  /* build a retrieved table */
  retrieved_list = (short*) xmalloc (sizeof (short) * (nmsgs+1));
  memset (retrieved_list, 0, sizeof (short) * (nmsgs+1));

  mbfi = open (outfile, O_WRONLY | O_CREAT | O_EXCL, 0666);
  if (mbfi < 0)
    {
      pop_close (server);
      error ("Error in open: %s, %s", strerror (errno), outfile);
      return (1);
    }
#if !defined(CYGWIN) && !defined(WIN32_NATIVE)
  fchown (mbfi, getuid (), (gid_t) -1);
#endif

  if ((mbf = fdopen (mbfi, "wb")) == NULL)
    {
      pop_close (server);
      error ("Error in fdopen: %s", strerror (errno), NULL);
      close (mbfi);
      unlink (outfile);
      return (1);
    }

  for (idx = 0; idx < nmsgs; idx++)
    {
      i = reverse ? nmsgs - idx : idx + 1;
      VERBOSE(("checking message %d     \n", i));
      
      if (!regexp_pattern 
	  || 
	  pop_search_top (server, i, match_lines, regexp_pattern) == POP_RETRIEVED)
	{
	  VERBOSE(("retrieving message %d     \n", i));
          mbx_delimit_begin (mbf);
	  if (pop_retr (server, i, mbx_write, mbf) != POP_RETRIEVED)
	    {
	      error (Errmsg, NULL, NULL);
	      close (mbfi);
	      return (1);
	    }

	  retrieved_list[i]=1;

	  mbx_delimit_end (mbf);
	  fflush (mbf);
	  if (ferror (mbf))
	    {
	      error ("Error in fflush: %s", strerror (errno), NULL);
	      pop_close (server);
	      close (mbfi);
	      return (1);
	    }
	}
    }

  /* On AFS, a call to write only modifies the file in the local
   *     workstation's AFS cache.  The changes are not written to the server
   *      until a call to fsync or close is made.  Users with AFS home
   *      directories have lost mail when over quota because these checks were
   *      not made in previous versions of movemail. */

#ifdef HAVE_FSYNC
  if (fsync (mbfi) < 0)
    {
      error ("Error in fsync: %s", strerror (errno), NULL);
      return (1);
    }
#endif

  if (close (mbfi) == -1)
    {
      error ("Error in close: %s", strerror (errno), NULL);
      return (1);
    }

  if (!keep_messages)
    {
      for (i = 1; i <= nmsgs; i++)
	{
	  if (retrieved_list[i] == 1)
	    {
	      VERBOSE(("deleting message %d     \n", i));
	      if (pop_delete (server, i))
		{
		  error (pop_error, NULL, NULL);
		  pop_close (server);
		  return (1);
		}
	    }
	}
    }

  VERBOSE(("closing server             \n"));
  if (pop_quit (server))
    {
      error (pop_error, NULL, NULL);
      return (1);
    }
    
  return (0);
}

static int
pop_retr (popserver server, int msgno, int (*action)(char *, FILE *), FILE *arg)
{
  char *line;
  int ret;

  if (pop_retrieve_first (server, msgno, &line))
    {
      strncpy (Errmsg, pop_error, sizeof (Errmsg));
      Errmsg[sizeof (Errmsg)-1] = '\0';
      return (POP_ERROR);
    }

  while (! (ret = pop_retrieve_next (server, &line)))
    {
      if (! line)
	break;

      if ((*action)(line, arg) != POP_RETRIEVED)
	{
	  strcpy (Errmsg, strerror (errno));
	  pop_close (server);
	  return (POP_ERROR);
	}
    }

  if (ret)
    {
      strncpy (Errmsg, pop_error, sizeof (Errmsg));
      Errmsg[sizeof (Errmsg)-1] = '\0';
      return (POP_ERROR);
    }

  return (POP_RETRIEVED);
}

/* search the top lines of each message looking for a match */
static int
pop_search_top (popserver server, int msgno, int lines, struct re_pattern_buffer* regexp)
{
  char *line;
  int ret;
  int match = POP_DONE;

  if (pop_top_first (server, msgno, lines, &line))
    {
      strncpy (Errmsg, pop_error, sizeof (Errmsg));
      Errmsg[sizeof (Errmsg)-1] = '\0';
      return (POP_ERROR);
    }

  while (! (ret = pop_top_next (server, &line)))
    {
      if (! line)
	break;

      /*      VERBOSE (("checking %s\n", line));*/
      if (match != POP_RETRIEVED)
	{
	  if ((ret = re_match (regexp, line, strlen (line), 0, 0)) == -2 )
	    {
	      strcpy (Errmsg, "error in regular expression");
	      pop_close (server);
	      return (POP_ERROR);
	    }
	  else if (ret >=0)
	    {
	      match = POP_RETRIEVED;
	    }
	}
    }

  if (ret)
    {
      strncpy (Errmsg, pop_error, sizeof (Errmsg));
      Errmsg[sizeof (Errmsg)-1] = '\0';
      return (POP_ERROR);
    }

  return match;
}

/* Do this as a macro instead of using strcmp to save on execution time. */
#define IS_FROM_LINE(a) ((a[0] == 'F') \
			 && (a[1] == 'r') \
			 && (a[2] == 'o') \
			 && (a[3] == 'm') \
			 && (a[4] == ' '))

static int
mbx_write (char *line, FILE *mbf)
{
  if (IS_FROM_LINE (line))
    {
      if (fputc ('>', mbf) == EOF)
	return (POP_ERROR);
    }
  if (fputs (line, mbf) == EOF) 
    return (POP_ERROR);
  if (fputc (0x0a, mbf) == EOF)
    return (POP_ERROR);
  return (POP_RETRIEVED);
}

static int
mbx_delimit_begin (FILE *mbf)
{
  if (fputs ("\f\n0, unseen,,\n", mbf) == EOF)
    return (POP_ERROR);
  return (POP_RETRIEVED);
}

static int
mbx_delimit_end (FILE *mbf)
{
  if (putc ('\037', mbf) == EOF)
    return (POP_ERROR);
  return (POP_RETRIEVED);
}

/* Turn a name, which is an ed-style (but Emacs syntax) regular
   expression, into a real regular expression by compiling it. */
static struct re_pattern_buffer*
compile_regex (char* pattern)
{
  char *err;
  struct re_pattern_buffer *patbuf=0;
  
  patbuf = (struct re_pattern_buffer*) xmalloc (sizeof (struct re_pattern_buffer));
  patbuf->translate = NULL;
  patbuf->fastmap = NULL;
  patbuf->buffer = NULL;
  patbuf->allocated = 0;

  err = (char*) re_compile_pattern (pattern, strlen (pattern), patbuf);
  if (err != NULL)
    {
      error ("%s while compiling pattern", err, NULL);
      return 0;
    }

  return patbuf;
}



#endif /* MAIL_USE_POP */

#ifndef HAVE_STRERROR
char *
strerror (int errnum)
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}

#endif /* ! HAVE_STRERROR */
