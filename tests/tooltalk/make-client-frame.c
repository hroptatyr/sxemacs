/* make-client-frame.c - create a new frame in XEmacs using ToolTalk */

/* See `tooltalk-make-client-frame-handler' in the file
   lisp/tooltalk/tooltalk-init.el for the receiver side. */

#include <desktop/tt_c.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>

Tt_status	tter;

#define exit_err_ptr(ptr) \
if ((tter = tt_ptr_error(ptr)) != TT_OK) \
{ fprintf(stderr, "%d:%s\n", __LINE__, tt_status_message(tter)); exit(1); }

#define exit_err(stat) \
if ((tter = stat) != TT_OK) \
{ fprintf(stderr, "%d:%s\n", __LINE__, tt_status_message(tter)); exit(1); }

Tt_callback_action callback_fn(Tt_message msg, Tt_pattern pat);
static Tt_message create_new_message(char *name, int height, int width);
static int initialize_tooltalk(void);
static void usage(void);

static char*		tt_procid;

Tt_callback_action
callback_fn(Tt_message msg, Tt_pattern pat)
{
  tt_message_destroy(msg);
  return TT_CALLBACK_PROCESSED;
}

static Tt_message
create_new_message(char *name, int height, int width)
{
  Tt_message	msg;

  msg = tt_message_create();
  exit_err_ptr(msg);

  exit_err(tt_message_address_set (msg, TT_PROCEDURE));
  exit_err(tt_message_class_set   (msg, TT_REQUEST));
  exit_err(tt_message_scope_set   (msg, TT_SESSION));
  exit_err(tt_message_op_set      (msg, "emacs-make-client-frame"));
  exit_err(tt_message_arg_add     (msg, TT_IN, "string", name));
  exit_err(tt_message_iarg_add    (msg, TT_IN, "int",    height));
  exit_err(tt_message_iarg_add    (msg, TT_IN, "int",    width));
  exit_err(tt_message_callback_add(msg, callback_fn));

  return msg;
}

static int
initialize_tooltalk(void) {
  int rcode;

  tt_procid = tt_open();
  if ((rcode = tt_ptr_error(tt_procid)) != TT_OK) {
    return rcode;
  }
  
  if ((rcode = tt_session_join(tt_default_session())) != TT_OK) { 
    return (rcode);
  }    
}


static void
usage(void)
{
  fprintf(stderr, 
	  "Usage: make-client-frame name height width\n\n");
}

void
main(argc, argv)
     int		argc;
     char		*argv[];
{

  Tt_message	msg;

 if (argc != 4) {
    usage();
    exit(0);
  }

  exit_err(initialize_tooltalk());

  msg = create_new_message(argv[1], atoi(argv[2]), atoi(argv[3]));

  exit_err(tt_message_send(msg));

  return;
}
