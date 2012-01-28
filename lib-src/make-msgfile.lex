%{

/* This is a Lex file. */

/* Localizable-message snarfing.
   Copyright (C) 1994, 1995 Amdahl Corporation.

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

/* Written by Ben Wing, November 1994.  Some code based on earlier
   make-msgfile.c. */

/* Note: there is still much work to be done on this.

   1) Definition of Arg below won't handle a generalized argument
      as might appear in a function call.  This is fine for DEFUN
      and friends, because only simple arguments appear there; but
      it might run into problems if Arg is used for other sorts
      of functions.
   2) snarf() should be modified so that it doesn't output null
      strings and non-textual strings (see the comment at the top
      of make-msgfile.c).
   3) parsing of (insert) should snarf all of the arguments.
   4) need to add set-keymap-prompt and deal with gettext of that.
   5) parsing of arguments should snarf all strings anywhere within
      the arguments, rather than just looking for a string as the
      argument.  This allows if statements as arguments to get parsed.
   6) begin_paren_counting() et al. should handle recursive entry.
   7) handle set-window-buffer and other such functions that take
      a buffer as the other-than-first argument.
   8) there is a fair amount of work to be done on the C code.
      Look through the code for #### comments associated with
      '#ifdef I18N3' or with an I18N3 nearby.
   9) Deal with `get-buffer-process' et al.
   10) Many of the changes in the Lisp code marked
       'rewritten for I18N3 snarfing' should be undone once (5) is
       implemented.
   11) Go through the Lisp code in prim and make sure that all
       strings are gettexted as necessary.  This may reveal more
       things to implement.
   12) Do the equivalent of (8) for the Lisp code.
   13) Deal with parsing of menu specifications.

	--ben

*/

/* Long comment from jwz:

   (much of this comment is outdated, and a lot of it is actually
   implemented)


   PROPOSAL FOR HOW THIS ALL OUGHT TO WORK
   this isn't implemented yet, but this is the plan-in-progress


   In general, it's accepted that the best way to internationalize is for all
   messages to be referred to by a symbolic name (or number) and come out of a
   table or tables, which are easy to change.

   However, with Emacs, we've got the task of internationalizing a huge body
   of existing code, which already contains messages internally.

   For the C code we've got two options:

    - Use a Sun-like gettext() form, which takes an "english" string which
      appears literally in the source, and uses that as a hash key to find
      a translated string;
    - Rip all of the strings out and put them in a table.

   In this case, it's desirable to make as few changes as possible to the C
   code, to make it easier to merge the code with the FSF version of emacs
   which won't ever have these changes made to it.  So we should go with the
   former option.

   The way it has been done (between 19.8 and 19.9) was to use gettext(), but
   *also* to make massive changes to the source code.  The goal now is to use
   gettext() at run-time and yet not require a textual change to every line
   in the C code which contains a string constant.  A possible way to do this
   is described below.

   (gettext() can be implemented in terms of catgets() for non-Sun systems, so
   that in itself isn't a problem.)

   For the Lisp code, we've got basically the same options: put everything in
   a table, or translate things implicitly.

   Another kink that lisp code introduces is that there are thousands of third-
   party packages, so changing the source for all of those is simply not an
   option.

   Is it a goal that if some third party package displays a message which is
   one we know how to translate, then we translate it?  I think this is a
   worthy goal.  It remains to be seen how well it will work in practice.

   So, we should endeavor to minimize the impact on the lisp code.  Certain
   primitive lisp routines (the stuff in lisp/prim/, and especially in
   cmdloop.el and minibuf.el) may need to be changed to know about translation,
   but that's an ideologically clean thing to do because those are considered
   a part of the emacs substrate.

   However, if we find ourselves wanting to make changes to, say, RMAIL, then
   something has gone wrong.  (Except to do things like remove assumptions
   about the order of words within a sentence, or how pluralization works.)

   There are two parts to the task of displaying translated strings to the
   user: the first is to extract the strings which need to be translated from
   the sources; and the second is to make some call which will translate those
   strings before they are presented to the user.

   The old way was to use the same form to do both, that is, GETTEXT() was both
   the tag that we searched for to build a catalog, and was the form which did
   the translation.  The new plan is to separate these two things more: the
   tags that we search for to build the catalog will be stuff that was in there
   already, and the translation will get done in some more centralized, lower
   level place.

   This program (make-msgfile.c) addresses the first part, extracting the
   strings.

   For the emacs C code, we need to recognize the following patterns:

     message ("string" ... )
     error ("string")
     report_file_error ("string" ... )
     signal_simple_error ("string" ... )
     signal_simple_error_2 ("string" ... )

     build_translated_string ("string")
     #### add this and use it instead of build_string() in some places.

     yes_or_no_p ("string" ... )
     #### add this instead of funcalling Qyes_or_no_p directly.

     barf_or_query_if_file_exists	#### restructure this
     check all callers of Fsignal	#### restructure these
     signal_error (Qerror ... )		#### change all of these to error()

     And we also parse out the `interactive' prompts from DEFUN() forms.

     #### When we've got a string which is a candidate for translation, we
     should ignore it if it contains only format directives, that is, if
     there are no alphabetic characters in it that are not a part of a `%'
     directive.  (Careful not to translate either "%s%s" or "%s: ".)

   For the emacs Lisp code, we need to recognize the following patterns:

     (message "string" ... )
     (error "string" ... )
     (format "string" ... )
     (read-from-minibuffer "string" ... )
     (read-shell-command "string" ... )
     (y-or-n-p "string" ... )
     (yes-or-no-p "string" ... )
     (read-file-name "string" ... )
     (temp-minibuffer-message "string")
     (query-replace-read-args "string" ... )

   I expect there will be a lot like the above; basically, any function which
   is a commonly used wrapper around an eventual call to `message' or
   `read-from-minibuffer' needs to be recognized by this program.


     (dgettext "domain-name" "string")		#### do we still need this?

     things that should probably be restructured:
       `princ' in cmdloop.el
       `insert' in debug.el
       face-interactive
       help.el, syntax.el all messed up

   BPW: (format) is a tricky case.  If I use format to create a string
   that I then send to a file, I probably don't want the string translated.
   On the other hand, If the string gets used as an argument to (y-or-n-p)
   or some such function, I do want it translated, and it needs to be
   translated before the %s and such are replaced.  The proper solution
   here is for (format) and other functions that call gettext but don't
   immediately output the string to the user to add the translated (and
   formatted) string as a string property of the object, and have
   functions that output potentially translated strings look for a
   "translated string" property.  Of course, this will fail if someone
   does something like

      (y-or-n-p (concat (if you-p "Do you " "Does he ")
			(format "want to delete %s? " filename))))

   But you shouldn't be doing things like this anyway.

   BPW: Also, to avoid excessive translating, strings should be marked
   as translated once they get translated, and further calls to gettext
   don't do any more translating.  Otherwise, a call like

      (y-or-n-p (format "Delete %s? " filename))

   would cause translation on both the pre-formatted and post-formatted
   strings, which could lead to weird results in some cases (y-or-n-p
   has to translate its argument because someone could pass a string to
   it directly).  Note that the "translating too much" solution outlined
   below could be implemented by just marking all strings that don't
   come from a .el or .elc file as already translated.

   Menu descriptors: one way to extract the strings in menu labels would be
   to teach this program about "^(defvar .*menu\n" forms; that's probably
   kind of hard, though, so perhaps a better approach would be to make this
   program recognize lines of the form

     "string" ... ;###translate

   where the magic token ";###translate" on a line means that the string
   constant on this line should go into the message catalog.  This is analogous
   to the magic ";###autoload" comments, and to the magic comments used in the
   EPSF structuring conventions.

  -----
  So this program manages to build up a catalog of strings to be translated.
  To address the second part of the problem, of actually looking up the
  translations, there are hooks in a small number of low level places in
  emacs.

  Assume the existence of a C function gettext(str) which returns the
  translation of `str' if there is one, otherwise returns `str'.

  - message() takes a char* as its argument, and always filters it through
    gettext() before displaying it.

  - errors are printed by running the lisp function `display-error' which
    doesn't call `message' directly (it princ's to streams), so it must be
    carefully coded to translate its arguments.  This is only a few lines
    of code.

  - Fread_minibuffer_internal() is the lowest level interface to all minibuf
    interactions, so it is responsible for translating the value that will go
    into Vminibuf_prompt.

  - Fpopup_menu filters the menu titles through gettext().

    The above take care of 99% of all messages the user ever sees.

  - The lisp function temp-minibuffer-message translates its arg.

  - query-replace-read-args is funny; it does
      (setq from (read-from-minibuffer (format "%s: " string) ... ))
      (setq to (read-from-minibuffer (format "%s %s with: " string from) ... ))

    What should we do about this?  We could hack query-replace-read-args to
    translate its args, but might this be a more general problem?  I don't
    think we ought to translate all calls to format.  We could just change
    the calling sequence, since this is odd in that the first %s wants to be
    translated but the second doesn't.


  Solving the "translating too much" problem:
  The concern has been raised that in this situation:
   - "Help" is a string for which we know a translation;
   - someone visits a file called Help, and someone does something
     contrived like (error buffer-file-name)
  then we would display the translation of Help, which would not be correct.
  We can solve this by adding a bit to Lisp_String objects which identifies
  them as having been read as literal constants from a .el or .elc file (as
  opposed to having been constructed at run time as it would in the above
  case.)  To solve this:

    - Fmessage() takes a lisp string as its first argument.
      If that string is a constant, that is, was read from a source file
      as a literal, then it calls message() with it, which translates.
      Otherwise, it calls message_no_translate(), which does not translate.

    - Ferror() (actually, Fsignal() when condition is Qerror) works similarly.
*/

/* Some notes:

-- {Arg} below could get confused by commas inside of quotes.
-- {LispToken} below can match some things that are not tokens (e.g.
   numbers) but for all practical purposes it should be fine.
*/

#include <stdio.h>

int snarf_return_state;

%}

%p 6000
%e 2000
%n 1000
%a 4000
%s C_QUOTE C_COMMENT LQUO LCOM
%s CSNARF LSNARF
%s DO_C DO_LISP DEFUN
%s DEFUN2 DEFUN3 LDEF

W	[ \t\n]
Any	(.|"\n")
Q	"\""
NQ	[^"]
NT	[^A-Za-z_0-9]
LP	"("
RP	")"
BS	"\\"
Esc	({BS}{Any})
Wh	({W}*)
LCom	(";"({Esc}|.)*)
LWh	(({W}|{Lcom})*)
Open	({Wh}{LP})
OpWQ	({Open}{Wh}{Q})
String	({Q}({Esc}|{NQ})*{Q})
Arg	([^,]*",")
StringArg	({Wh}{String}{Wh}",")
OpenString	({Open}{StringArg})
LispToken	(({Esc}|[-A-Za-z0-9!@$%^&*_=+|{}`~,<.>/?])+)
%%

<DO_C>{NT}"GETTEXT"{OpWQ} { snarf (); }
<DO_C>{NT}"DEFER_GETTEXT"{OpWQ} { snarf (); }
<DO_C>{NT}"build_translated_string"{OpWQ} { snarf (); }
<DO_C>{NT}"insert_string"{OpWQ} { snarf (); }
<DO_C>{NT}"message"{OpWQ} { snarf (); }
<DO_C>{NT}"warn_when_safe"{OpWQ} { snarf (); }
<DO_C>{NT}"error"{OpWQ} { snarf (); }
<DO_C>{NT}"continuable_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_error_2"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_continuable_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_continuable_error_2"{OpWQ} { snarf (); }
<DO_C>{NT}"report_file_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_file_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_double_file_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_double_file_error_2"{OpWQ} { snarf (); }
<DO_C>{NT}"syntax_error"{OpWQ} { snarf (); }
<DO_C>{NT}"continuable_syntax_error"{OpWQ} { snarf (); }
<DO_C>{NT}"CTB_ERROR"{OpWQ} { snarf (); }
<DO_C>{NT}"fatal"{OpWQ} { snarf (); }
<DO_C>{NT}"stdout_out"{OpWQ} { snarf (); }
<DO_C>{NT}"stderr_out"{OpWQ} { snarf (); }
<DO_C>{NT}"with_output_to_temp_buffer"{OpWQ} { snarf (); }

<DO_C>{NT}"DEFVAR_BOOL"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_LISP"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_SPECIFIER"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_INT"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_BUFFER_LOCAL"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_BUFFER_DEFAULTS"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"deferror"{Open}{Arg}{StringArg}{Wh}{Q} { snarf (); }

<DO_C>{NT}"barf_or_query_if_file_exists"{Open}{Arg}{Wh}{Q} {
  /* #### see comment above about use of Arg */
  snarf ();
}

<DO_C>{NT}"DEFUN"{Open} { BEGIN DEFUN; }

<DO_C>"/*" {
  /* This is hateful, but doc strings are sometimes put inside of comments
     (to get around limits in cpp), so we can't ignore stuff inside of
     comments. */
  /* BEGIN C_COMMENT; */
}
<DO_C>{Q} { BEGIN C_QUOTE; }
<DO_C>{Any} { }

<DEFUN>{StringArg}{Arg}{Arg}{Arg}{Arg}{Wh} { BEGIN DEFUN2; }
<DEFUN>{Any} { bad_c_defun (); }

<DEFUN2>{Q} {
  /* We found an interactive specification. */
  snarf_return_state = DEFUN3;
  snarf ();
}
<DEFUN2>[^,]* {
  /* This function doesn't have an interactive specification.
     Don't use {Arg} in the specification because DEFUN3 looks
     for the comma. */
  BEGIN DEFUN3;
}

<DEFUN3>{Wh}","{Wh}{Q} {
  snarf_return_state = DO_C;
  snarf ();
}
<DEFUN3>{Any} { bad_c_defun (); }

<C_QUOTE>{Esc} { }
<C_QUOTE>{Q} { BEGIN DO_C; }
<C_QUOTE>{Any} { }

<C_COMMENT>"*/" { BEGIN DO_C; }
<C_COMMENT>{Any} { }

<DO_LISP>{LP}{LWh}"gettext"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"purecopy"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"interactive"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"message"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"error"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"warn"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"format"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"substitute-command-keys"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"temp-minibuffer-message"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"momentary-string-display"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"princ"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"prin1"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"prin1-to-string"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"print"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"insert"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"insert-before-markers"{LWh}{Q} { inc_paren (); snarf (); }

<DO_LISP>{LP}{LWh}"get-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"get-buffer-create"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"generate-new-buffer-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"rename-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"set-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"switch-to-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"pop-to-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"with-output-to-temp-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"buffer-enable-undo"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"buffer-disable-undo"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"get-buffer-window"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"delete-windows-on"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"replace-buffer-in-windows"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"display-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"other-buffer"{LWh}{Q} { inc_paren (); snarf (); }

<DO_LISP>{LP}{LWh}"read-from-minibuffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-shell-command"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-file-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-variable"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-command"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-function"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-directory-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-string"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-number"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-minibuffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-quoted-char"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-face-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-itimer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"completing-read"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"y-or-n-p"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"yes-or-no-p"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"query-replace-read-args"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"eval-minibuffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"edit-and-eval-command"{LWh}{Q} { inc_paren (); snarf (); }

<DO_LISP>{LP}{LWh}"defvar"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defconst"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defun"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defmacro"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defsubst"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}

<DO_LISP>{Q} { BEGIN LQUO; }
<DO_LISP>";" { BEGIN LCOM; }
<DO_LISP>{LP} { inc_paren (); }
<DO_LISP>{RP} { dec_paren (); }
<DO_LISP>{Esc} { }
<DO_LISP>{W} { lisp_whitespace (); }
<DO_LISP>{Any} { }

<LQUO>{Esc} { }
<LQUO>{Q} { BEGIN DO_LISP; }
<LQUO>{Any} { }

<LCOM>"\n" { BEGIN DO_LISP; }
<LCOM>{Any} { }

<LDEF>{LWh}{Q} { snarf (); }
<LDEF>{Any} { BEGIN DO_LISP; }

<CSNARF>{Esc} { ECHO; }
<CSNARF>{Q} { ECHO; fprintf (yyout, ")\n"); BEGIN snarf_return_state; }
<CSNARF>{Any} { ECHO; }

<LSNARF>{Esc} { ECHO; }
<LSNARF>"\n" { fprintf (yyout, "\\n\\\n"); }
<LSNARF>{Q} { ECHO; fprintf (yyout, ")\n"); BEGIN snarf_return_state; }
<LSNARF>{Any} { ECHO; }

%%

enum filetype { C_FILE, LISP_FILE, INVALID_FILE };
/* some brain-dead headers define this ... */
#undef FALSE
#undef TRUE
enum boolean { FALSE, TRUE };

void scan_file (char *filename);
void process_C_file (void);
void process_Lisp_file (void);

int in_c;
int in_paren_counting, paren_count;
int paren_return_state;

snarf ()
{
  fprintf (yyout, "gettext(\"");
  if (in_c)
    BEGIN CSNARF;
  else
    BEGIN LSNARF;
}

bad_c_defun ()
{
  fprintf (stderr, "Warning: Invalid DEFUN encountered in C, line %d.\n",
	   yylineno);
  snarf_return_state = DO_C;
  BEGIN DO_C;
  /* REJECT; Sun's lex is broken!  Use Flex! */
}

bad_lisp_def ()
{
  fprintf (stderr,
	   "Warning: Invalid defmumble encountered in Lisp, line %d.\n",
	   yylineno);
  snarf_return_state = DO_LISP;
  BEGIN DO_LISP;
  /* REJECT; Sun's lex is broken!  Use Flex! */
}

inc_paren ()
{
  if (in_paren_counting)
    paren_count++;
}

dec_paren ()
{
  if (in_paren_counting)
    {
      /* If we find a right paren without a matching left paren, it usually
	 just indicates a statement like

	 (defvar foo-mumble nil)

	 where 'nil' is the sexp we are skipping over, and there's no
	 doc string. */
      if (paren_count > 0)
	paren_count--;
      else
	unput (')');
      if (paren_count == 0)
	{
	  in_paren_counting = 0;
	  BEGIN paren_return_state;
	}
    }
}

/* #### begin_paren_counting () does not handle recursive entries */

begin_paren_counting (int return_state)
{
  in_paren_counting = 1;
  paren_count = 0;
  paren_return_state = return_state;
}

lisp_whitespace ()
{
  if (in_paren_counting && !paren_count)
    {
      /* We got to the end of a token and we're not in a parenthesized
	 expression, so we're at the end of an sexp. */
      in_paren_counting = 0;
      BEGIN paren_return_state;
    }
}

yywrap ()
{
  return 1;
}

main (int argc, char *argv[])
{
  register int i;

  yyout = stdout;

  /* If first two args are -o FILE, output to FILE. */
  i = 1;
  if (argc > i + 1 && strcmp (argv[i], "-o") == 0) {
    yyout = fopen (argv[++i], "w");
    ++i;
  }
  /* ...Or if args are -a FILE, append to FILE. */
  if (argc > i + 1 && strcmp (argv[i], "-a") == 0) {
    yyout = fopen (argv[++i], "a");
    ++i;
  }
  if (!yyout) {
    fprintf (stderr, "Unable to open output file %s\n", argv[--i]);
    return;
  }

  for (; i < argc; i++)
    scan_file (argv[i]);

  return 0;
}


void scan_file (char *filename)
{
  enum filetype type = INVALID_FILE;
  register char *p = filename + strlen (filename);

  if (strcmp (p - 4, ".elc") == 0) {
    *--p = '\0';				/* Use .el file instead */
    type = LISP_FILE;
  } else if (strcmp (p - 3, ".el") == 0)
    type = LISP_FILE;
  else if (strcmp (p - 2, ".o") == 0) {
    *--p = 'c';					/* Use .c file instead */
    type = C_FILE;
  } else if (strcmp (p - 2, ".c") == 0)
    type = C_FILE;

  if (type == INVALID_FILE) {
    fprintf (stderr, "File %s being ignored\n", filename);
    return;
  }
  yyin = fopen (filename, "r");
  if (!yyin) {
    fprintf (stderr, "Unable to open input file %s\n", filename);
    return;
  }

  fprintf (yyout, "/* %s */\n", filename);
  if (type == C_FILE)
    process_C_file ();
  else
    process_Lisp_file ();
  fputc ('\n', yyout);

  fclose (yyin);
}

void process_C_file ()
{
  snarf_return_state = DO_C;
  in_c = 1;
  BEGIN DO_C;
  yylex ();
}

void process_Lisp_file ()
{
  snarf_return_state = DO_LISP;
  in_c = 0;
  BEGIN DO_LISP;
  yylex ();
}
