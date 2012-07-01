/* -*-C-*-
 Server code for handling requests from clients and forwarding them
 on to the GNU Emacs process.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 'etc/server.c'
	 from the 18.52 GNU Emacs distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

/* HISTORY
 * 11-Nov-1990		bristor@simba
 *    Added EOT stuff.
 */

/*
 * This file incorporates new features added by Bob Weiner <weiner@mot.com>,
 * Darrell Kindred <dkindred@cmu.edu> and Arup Mukherjee <arup@cmu.edu>.
 * Please see the note at the end of the README file for details.
 *
 * (If gnuserv came bundled with your emacs, the README file is probably
 * ../etc/gnuserv.README relative to the directory containing this file)
 */

#include "gnuserv.h"
#include <assert.h>

char gnuserv_version[] = "gnuserv version" GNUSERV_VERSION;

#ifdef USE_LITOUT
#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif
#endif

#ifdef AIX
#include <sys/select.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif				/* HAVE_UNISTD_H */

#ifdef HAVE_STRING_H
#include <string.h>
#endif				/* HAVE_STRING_H */

#define xstrncpy(d_,s_,l_)			\
	do {					\
		char* dst_=d_;			\
		dst_[0]='\0';			\
		strncat((dst_),(s_),(l_)-1);	\
	} while(0)


#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && \
    !defined(INTERNET_DOMAIN_SOCKETS)
int
main(void)
{
	fputs("Sorry, the Emacs server is only supported on systems that have\n\
Unix Domain sockets, Internet Domain sockets or System V IPC\n", stderr);
	return 1;
}
#else  /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

# ifdef SYSV_IPC

/* ipc message queue id */
int ipc_qid = 0;
/* watchdog task pid */
pid_t ipc_wpid = 0;

/** ipc_exit
 * clean up the queue id and queue, then kill the watchdog task
 * if it exists. exit with the given status. **/
static void
__attribute__((noreturn))
ipc_exit(int stat)
{
	msgctl(ipc_qid, IPC_RMID, 0);

	if (ipc_wpid != 0) {
		kill(ipc_wpid, SIGKILL);
	}
	exit(stat);
}

/** ipc_handle_signal
 * catch the signal given and clean up. **/
static void
ipc_handle_signal(int sig)
{
	ipc_exit(0);
}

/** ipc_spawn_watchdog
 * spawn a watchdog task to clean up the message queue should the
 * server process die. **/
static void
ipc_spawn_watchdog(void)
{
	/* child process */
	if ((ipc_wpid = fork()) == 0) {
		/* parent's process id */
		pid_t ppid = getppid();

		/* gnu kills process group on exit */
		setpgrp();

		while (1) {
			/* ppid is no longer valid, parent may have died */
			if (kill(ppid, 0) < 0) {
				ipc_exit(0);
			}
			/* else, have another go later */
			sleep(10);
		}
	}
	return;
}

/** ipc_init
 * initialize server, setting the global msqid that can be listened on. **/
static void
ipc_init(struct msgbuf **msgpp)
{
	/* messge key */
	key_t key;
	/* pathname for key */
	char buf[GSERV_BUFSZ];
	int sz;

	SNPRINTF(sz, buf, sizeof(buf),"%s/gsrv%d", tmpdir, (int)geteuid());
	creat(buf, 0600);
	key = ftok(buf, 1);

	if ((ipc_qid = msgget(key, 0600 | IPC_CREAT)) == -1) {
		perror(progname);
		fprintf(stderr, "%s: unable to create msg queue\n", progname);
		ipc_exit(1);
	}
	ipc_spawn_watchdog();

	signal(SIGTERM, ipc_handle_signal);
	signal(SIGINT, ipc_handle_signal);

	*msgpp = (struct msgbuf*)malloc(sizeof **msgpp + GSERV_BUFSZ);
	if (*msgpp == NULL) {
		fprintf(stderr,
			"%s: unable to allocate space for message buffer\n",
			progname);
		ipc_exit(1);
	}
	return;
}

/** handle_ipc_request
 * accept a request from a client, pass the request on to the GNU Emacs process,
 * then wait for its reply and pass that on to the client. **/
static void
handle_ipc_request(struct msgbuf *msgp)
{
	/* message status */
	struct msqid_ds msg_st;
	char buf[GSERV_BUFSZ];
	/* length of message / read */
	ssize_t len;
	/* tag fields on the response from emacs */
	int s, result_len;
	int offset = 0;
	/* # bytes that will actually be sent off */
	int total = 1;

	if ((len = msgrcv(ipc_qid, msgp, GSERV_BUFSZ - 1, 1, 0)) < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to receive\n", progname);
		ipc_exit(1);
	}
	/* if */
	msgctl(ipc_qid, IPC_STAT, &msg_st);
	xstrncpy(buf, msgp->mtext, sizeof(buf));

	printf("%d %s", ipc_qid, buf);
	fflush(stdout);

	/* now for the response from gnu */
	msgp->mtext[0] = '\0';

	/* read in "n/m:" (n=client fd, m=message length) */
	while (offset < (GSERV_BUFSZ - 1) &&
	       ((len = read(0, buf + offset, 1)) > 0) && buf[offset] != ':') {
		offset += len;
	}

	if (len < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to read\n", progname);
		exit(1);
	}

	/* parse the response from emacs, getting client fd & result length */
	buf[offset] = '\0';
	sscanf(buf, "%d/%d", &s, &result_len);

	while (result_len > 0) {
		len = read(0, buf, min2(result_len, GSERV_BUFSZ - 1));
		if (len < 0) {
			perror(progname);
			fprintf(stderr, "%s: unable to read\n", progname);
			exit(1);
		}

		/* Send this string off, but only if we have enough space */
		if (GSERV_BUFSZ > total) {
			if (total + len <= GSERV_BUFSZ) {
				buf[len] = 0;
			} else {
				buf[GSERV_BUFSZ - total] = 0;
			}
			send_string(s, buf);
			total += strlen(buf);
		}

		result_len -= len;
	}

	/* eat the newline */
	while ((len = read(0, buf, 1)) == 0);

	if (len < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to read\n", progname);
		exit(1);
	}
	if (buf[0] != '\n') {
		fprintf(stderr, "%s: garbage after result [%c]\n", progname,
			buf[0]);
		exit(1);
	}

	/* Send a response back to the client. */
	msgp->mtype = msg_st.msg_lspid;
	if (msgsnd(ipc_qid, msgp, strlen(msgp->mtext) + 1, 0) < 0) {
		perror("msgsend(gnuserv)");
	}
	return;
}
# endif  /* SYSV_IPC */

# if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/** echo_request
 * read request from a given socket descriptor, and send the information
 * to stdout (the gnu process). **/
static void
echo_request(int s)
{
	char buf[GSERV_BUFSZ];
	ssize_t len;

	printf("%d ", s);

	/* read until we get a newline or no characters */
	while ((len = recv(s, buf, GSERV_BUFSZ - 1, 0)) > 0) {
		buf[len] = '\0';
		printf("%s", buf);

		if (buf[len - 1] == EOT_CHR) {
			/* end of message */
			fflush(stdout);
			break;
		}
	}

	if (len < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to recv\n", progname);
		exit(1);
	}
	return;
}

/** handle_response
 * accept a response from stdin (the gnu process) and pass the
 * information on to the relevant client. **/
static void
handle_response(void)
{
	char buf[GSERV_BUFSZ + 1];
	int offset = 0;
	int s;
	int len = 0;
	int result_len;

	/* read in "n/m:" (n=client fd, m=message length) */
	while (offset < GSERV_BUFSZ &&
	       ((len = read(0, buf + offset, 1)) > 0) && buf[offset] != ':') {
		offset += len;
	}

	if (len < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to read\n", progname);
		exit(1);
	}

	/* parse the response from emacs, getting client fd & result length */
	buf[offset] = '\0';
	sscanf(buf, "%d/%d", &s, &result_len);

	while (result_len > 0) {
		if ((len = read(0, buf, min2(result_len, GSERV_BUFSZ))) < 0) {
			perror(progname);
			fprintf(stderr, "%s: unable to read\n", progname);
			exit(1);
		}
		buf[len] = '\0';
		send_string(s, buf);
		result_len -= len;
	}

	/* eat the newline */
	while ((len = read(0, buf, 1)) == 0) ;
	if (len < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to read\n", progname);
		exit(1);
	}
	if (buf[0] != '\n') {
		fprintf(stderr, "%s: garbage after result\n", progname);
		exit(1);
	}
	/* send the newline */
	buf[1] = '\0';
	send_string(s, buf);
	close(s);
	return;
}
# endif  /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */

# ifdef INTERNET_DOMAIN_SOCKETS
struct entry {
	unsigned long host_addr;
	struct entry *next;
};

struct entry *permitted_hosts[TABLE_SIZE];

#  ifdef AUTH_MAGIC_COOKIE
#   include <X11/X.h>
#   include <X11/Xauth.h>

static Xauth *server_xauth = NULL;
#  endif  /* AUTH_MAGIC_COOKIE */

static ssize_t
timed_read(int fd, char *buf, int max, int timeout, int one_line)
{
	fd_set rmask;
	/* = {timeout, 0}; */
	struct timeval tv;
	char c = 0;
	ssize_t nbytes = 0;
	int r;

	tv.tv_sec = timeout;
	tv.tv_usec = 0;

	FD_ZERO(&rmask);
	FD_SET(fd, &rmask);

	do {
		r = select(fd + 1, &rmask, NULL, NULL, &tv);

		if (r > 0) {
			if (read(fd, &c, 1) == 1) {
				*buf++ = c;
				++nbytes;
			} else {
				printf("read error on socket\004\n");
				return -1;
			}
		} else if (r == 0) {
			printf("read timed out\004\n");
			return -1;
		} else {
			printf("error in select\004\n");
			return -1;
		}
	} while ((nbytes < max) && !(one_line && (c == '\n')));

	--buf;
	if (one_line && *buf == '\n') {
		*buf = 0;
	}
	return nbytes;
}

/** permitted
 * return whether a given host is allowed to connect to the server. **/
static int
permitted(unsigned long host_addr, int fd)
{
	int key;
	struct entry *entry;

	char auth_protocol[128];
	char buf[1024];

	if (fd > 0) {
		/* we are checking permission on a real connection */
		ssize_t len;
		long int auth_data_len;

		/* Read auth protocol name */
		if ((len = timed_read(
			     fd, auth_protocol,
			     AUTH_NAMESZ, AUTH_TIMEOUT, 1)) <= 0) {
			return FALSE;
		}

		if (strcmp(auth_protocol, DEFAUTH_NAME) &&
		    strcmp(auth_protocol, MCOOKIE_NAME)) {
			printf("authentication protocol (%s) \
from client is invalid...\n\
... Was the client an old version of gnuclient?\n", auth_protocol);
			return FALSE;
		}

		if (strcmp(auth_protocol, MCOOKIE_NAME)) {
			/*break;*/
			goto old_stuff;
		}
		/*
		 * doing magic cookie auth
		 */

		if (timed_read(fd, buf, 10, AUTH_TIMEOUT, 1) <= 0) {
			return FALSE;
		}
		auth_data_len = strtol(buf, NULL, 10);

		if (auth_data_len <= 0 || (size_t)auth_data_len > sizeof(buf)) {
			return FALSE;
		}

		len = timed_read(fd, buf, auth_data_len, AUTH_TIMEOUT, 0);
		if (len != auth_data_len) {
			return FALSE;
		}

#ifdef AUTH_MAGIC_COOKIE
		if (server_xauth && server_xauth->data) {
			/* Do a compare without comprising info about
			   the size of the cookie */
			int auth_data_pos;
			int auth_mismatches =
				(auth_data_len ^ server_xauth->data_length);

			for (auth_data_pos = 0;
			     auth_data_pos < auth_data_len;
			     ++auth_data_pos) {
				auth_mismatches |=
					(buf[auth_data_pos] ^ server_xauth->
					 data[auth_data_pos %
					      server_xauth->data_length]);
			}
			if (auth_mismatches == 0) {
				return TRUE;
			}
			for (; rand() % 1000;);
		}
#else  /* !AUTH_MAGIC_COOKIE */
		fputs("\
client tried Xauth, but server is not compiled with Xauth\n", stdout);
#endif	/* AUTH_MAGIC_COOKIE */

		/*
		 * auth failed, but allow this to fall through to the GNU_SECURE
		 * protocol....
		 */
		fputs("\
Xauth authentication failed, trying GNU_SECURE auth...\n", stdout);
	}

old_stuff:
	/* Now, try the old GNU_SECURE stuff... */

	/* First find the hash key */
	key = HASH(host_addr) % TABLE_SIZE;

	/* Now check the chain for that hash key */
	for (entry = permitted_hosts[key]; entry != NULL; entry = entry->next) {
		if (host_addr == entry->host_addr) {
			return TRUE;
		}
	}
	return FALSE;
}

/** add_host
 * add the given host to the list of permitted hosts, provided it isn't
 * already there. **/
static void
add_host(unsigned long host_addr)
{
	int key;
	struct entry *new_entry;

	if (!permitted(host_addr, -1)) {
		new_entry = (struct entry *)malloc(sizeof(struct entry));
		if (new_entry == NULL) {
			fprintf(stderr, "\
%s: unable to malloc space for permitted host entry\n", progname);
			exit(1);
		}
		/* if */
		new_entry->host_addr = host_addr;
		key = HASH(host_addr) % TABLE_SIZE;
		new_entry->next = permitted_hosts[key];
		permitted_hosts[key] = new_entry;
	}
	return;
}

/** setup_table
 * initialize the table of hosts allowed to contact the server,
 * by reading from the file specified by the GNU_SECURE
 * environment variable
 * Put in the local machine, and, if a security file is specifed,
 * add each host that is named in the file.
 * Return the number of hosts added. **/
static int
setup_table(void)
{
	FILE *host_file;
	char *file_name;
	char hostname[HOSTNAMSZ];
	unsigned int host_addr;
	int i, hosts = 0;
	int t;

	/* Make sure every entry is null */
	for (i = 0; i < TABLE_SIZE; i++) {
		permitted_hosts[i] = NULL;
	}
	gethostname(hostname, HOSTNAMSZ);

	if ((t = internet_addr(hostname)) == -1) {
		fprintf(stderr, "\
%s: unable to find %s in /etc/hosts or from YP\n", progname, hostname);
		exit(1);
	} else {
		host_addr = t;
	}

#ifdef AUTH_MAGIC_COOKIE
	server_xauth = XauGetAuthByAddr(
		FamilyInternet,
		sizeof(host_addr), (char *)&host_addr,
		strlen(MCOOKIE_SCREEN), MCOOKIE_SCREEN,
		strlen(MCOOKIE_X_NAME), MCOOKIE_X_NAME);
	hosts++;

#endif  /* AUTH_MAGIC_COOKIE */

	if ((file_name = getenv("GNU_SECURE")) == NULL) {
		/* security file not given */
		;
	} else if ((host_file = fopen(file_name, "r")) == NULL) {
		/* host file didn't open/exist */
		;
	} else {
		/* find a host */
		while ((fscanf(host_file, "%s", hostname) != EOF)) {
			t = internet_addr(hostname);
			/* get its addr */
			if (t != -1) {
				host_addr = t;
				/* add the addr */
				add_host(host_addr);
				hosts++;
			}
		}
		fclose(host_file);
	}
	return hosts;
}

/** internet_init
 * initialize server, returning an internet socket that can be listened on. **/
static int
internet_init(void)
{
	/* socket descriptor */
	int ls;
	/* pointer to service information */
	struct servent *sp;
	/* for local socket address */
	struct sockaddr_in server;
	/* ptr to return from getenv */
	char *ptr;

	if (setup_table() == 0) {
		return -1;
	}

	/* clear out address structure */
	memset(&server, '\0', sizeof(server));

	/* Set up address structure for the listen socket. */
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = INADDR_ANY;

	/* Find the information for the gnu server
	 * in order to get the needed port number.
	 */
	if ((ptr = getenv("GNU_PORT")) != NULL) {
		server.sin_port = htons(atoi(ptr));
	} else if ((sp = getservbyname("gnuserv", "tcp")) == NULL) {
		server.sin_port = htons(DEFAULT_PORT + getuid());
	} else {
		server.sin_port = sp->s_port;
	}
	/* Create the listen socket. */
	if ((ls = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror(progname);
		fprintf(stderr, "%s: unable to create socket\n", progname);
		exit(1);
	}

	/* Bind the listen address to the socket. */
	if (bind(ls, (struct sockaddr*)&server, sizeof(server)) < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to bind socket\n", progname);
		exit(1);
	}

	/* Initiate the listen on the socket so remote users
	 * can connect. */
	if (listen(ls, 20) == -1) {
		perror(progname);
		fprintf(stderr, "%s: unable to listen\n", progname);
		exit(1);
	}
	return ls;
}

/** handle_internet_request
 * accept a request from a client and send the information
 * to stdout (the gnu process). **/
static void
handle_internet_request(int ls)
{
	int s;
	socklen_t addrlen = sizeof(struct sockaddr_in);
	/* for peer socket address */
	struct sockaddr_in peer;

	memset(&peer, '\0', sizeof(peer));

	if ((s = accept(ls, (struct sockaddr*)&peer, &addrlen)) == -1) {
		perror(progname);
		fprintf(stderr, "%s: unable to accept\n", progname);
		exit(1);
	}

	/* Check that access is allowed - if not return crud to the client */
	if (!permitted(peer.sin_addr.s_addr, s)) {
		send_string(s, "\
gnuclient: Connection refused\ngnuclient: unable to connect to remote");
		close(s);

		printf("Refused connection from %s\n",
		       inet_ntoa(peer.sin_addr));
		return;
	}
	echo_request(s);
}
# endif  /* INTERNET_DOMAIN_SOCKETS */

# ifdef UNIX_DOMAIN_SOCKETS
/** unix_init
 * initialize server, returning an unix-domain socket
 * that can be listened on. **/
static int
unix_init(void)
{
	/* socket descriptor */
	int ls;
	/* unix socket address */
	struct sockaddr_un server;
	socklen_t bindlen;
	int sz;

	if ((ls = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to create socket\n", progname);
		exit(1);
	}

	/* Set up address structure for the listen socket. */
#  ifdef HIDE_UNIX_SOCKET
	SNPRINTF(sz, server.sun_path, sizeof(server.sun_path),
		 "%s/gsrvdir%d", tmpdir, (int)geteuid());
	if (mkdir(server.sun_path, 0700) < 0) {
		/* assume it already exists, and try to set perms */
		if (chmod(server.sun_path, 0700) < 0) {
			perror(progname);
			fprintf(stderr, "%s: can't set permissions on %s\n",
				progname, server.sun_path);
			exit(1);
		}
	}
	strcat(server.sun_path, "/gsrv");
	/* remove old file if it exists */
	unlink(server.sun_path);
#  else  /* !HIDE_UNIX_SOCKET */
	SNPRINTF(sz, server.sun_path, sizeof(server.sun_path),
		 "%s/gsrv%d", tmpdir, (int)geteuid());
	/* remove old file if it exists */
	unlink(server.sun_path);
#endif  /* HIDE_UNIX_SOCKET */

	server.sun_family = AF_UNIX;
#  ifdef HAVE_SOCKADDR_SUN_LEN
	/* See W. R. Stevens "Advanced Programming in the Unix Environment"
	   p. 502 */
	bindlen = (sizeof(server.sun_len) + sizeof(server.sun_family)
		   + strlen(server.sun_path) + 1);
	server.sun_len = bindlen;
#else  /* !HAVE_SOCKADDR_SUN_LEN */
	bindlen = strlen(server.sun_path) + sizeof(server.sun_family);
#endif	/* HAVE_SOCKADDR_SUN_LEN */

	if (bind(ls, (struct sockaddr *)&server, bindlen) < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to bind socket\n", progname);
		exit(1);
	}
	/* only this user can send commands */
	chmod(server.sun_path, 0700);

	if (listen(ls, 20) < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to listen\n", progname);
		exit(1);
	}

	/* #### there are also better ways of dealing with this when
	   sigvec() is present. */
#  if  defined (HAVE_SIGPROCMASK)
	{
		sigset_t _mask;
		sigemptyset(&_mask);
		sigaddset(&_mask, SIGPIPE);
		sigprocmask(SIG_BLOCK, &_mask, NULL);
	}
#  else  /* !HAVE_SIGPROCMASK */
	signal(SIGPIPE, SIG_IGN);	/* in case user kills client */
#  endif  /* HAVE_SIGPROCMASK */
	return ls;
}

/** handle_unix_request
 * accept a request from a client and send the information
 * to stdout (the gnu process). **/
static void
handle_unix_request(int ls)
{
	int s;
	socklen_t len = sizeof(struct sockaddr_un);
	/* for unix socket address */
	struct sockaddr_un server;

	server.sun_family = AF_UNIX;

	if ((s = accept(ls, (struct sockaddr *)&server, &len)) < 0) {
		perror(progname);
		fprintf(stderr, "%s: unable to accept\n", progname);
		/* Nothing more we can do here... */
		return;
	}
	echo_request(s);
	return;
}
# endif  /* UNIX_DOMAIN_SOCKETS */


int
main(int argc, char *argv[])
{
	/* temporary channel number */
	int chan;
#  ifdef SYSV_IPC
	/* message buffer */
	struct msgbuf *msgp;
#  else	 /* !SYSV_IPC */
	/* internet domain listen socket */
	int ils = -1;
	/* unix domain listen socket */
	int uls = -1;
#  endif  /* SYSV_IPC */

	if (argc >= 0) {
		progname = argv[0];
	}

	/* close unwanted channels */
	for (chan = 3; chan < _NFILE; close(chan++));

#ifdef USE_TMPDIR
	tmpdir = getenv("TMPDIR");
#endif	/* USE_TMPDIR */
	if (!tmpdir) {
		tmpdir = "/tmp";
	}

#ifdef USE_LITOUT
	{
		/* this is to allow ^D to pass to emacs */
		int d = LLITOUT;
		(void)ioctl(fileno(stdout), TIOCLBIS, &d);
	}
#endif	/* USE_LITOUT */

#ifdef SYSV_IPC
	/* get a msqid to listen on, and a message buffer */
	ipc_init(&msgp);
#endif  /* SYSV_IPC */

#ifdef INTERNET_DOMAIN_SOCKETS
	/* get an internet domain socket to listen on */
	ils = internet_init();
#endif  /* INTERNET_DOMAIN_SOCKETS */

#ifdef UNIX_DOMAIN_SOCKETS
	/* get a unix domain socket to listen on */
	uls = unix_init();
#endif  /* UNIX_DOMAIN_SOCKETS */

	while (1) {
# ifdef SYSV_IPC
		handle_ipc_request(msgp);
# else  /* !SYSV_IPC */
		fd_set rmask;
		int max_socks;

		FD_ZERO(&rmask);
		FD_SET(fileno(stdin), &rmask);

		if (uls >= 0) {
			FD_SET(uls, &rmask);
		}
		if (ils >= 0) {
			FD_SET(ils, &rmask);
		}

		max_socks = max2(fileno(stdin), max2(uls, ils));
		if (select(max_socks + 1, &rmask, NULL, NULL, NULL) < 0) {
			perror(progname);
			fprintf(stderr, "%s: unable to select\n", progname);
			return 1;
		}
# ifdef UNIX_DOMAIN_SOCKETS
		if (uls > 0 && FD_ISSET(uls, &rmask)) {
			handle_unix_request(uls);
		}
# endif	 /* UNIX_DOMAIN_SOCKETS */

# ifdef INTERNET_DOMAIN_SOCKETS
		if (ils > 0 && FD_ISSET(ils, &rmask)) {
			handle_internet_request(ils);
		}
# endif  /* INTERNET_DOMAIN_SOCKETS */

		/* from stdin (gnu process) */
		if (FD_ISSET(fileno(stdin), &rmask)) {
			handle_response();
		}
# endif  /* NOT SYSV_IPC */
	}
	/* not reached */
	return 0;
}

#endif  /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
