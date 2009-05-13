 /* Lisp functions for making directory listings.
    Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.30. */

#include <config.h>
#include "lisp.h"

#include "sysfile.h"
#include "sysdir.h"
#include "systime.h"
#include "sysdep.h"
#include "syspwd.h"
#include "buffer.h"
#include "commands.h"
#include "elhash.h"
#include "regex.h"
#include "opaque.h"
#include "syntax.h"
#include "dllist.h"
#include "bloom.h"
#include "dynacat.h"

#ifdef FILE_CODING
#include "file-coding.h"
#endif

#define USE_D_TYPE 1
#define USE_MATCH_ARG 1

Lisp_Object Vcompletion_ignored_extensions;
Lisp_Object Vdirectory_files_no_trivial_p;
Lisp_Object Qdirectory_files;
Lisp_Object Qdirectory_files_recur;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;

Lisp_Object Qcompanion_bf;
Lisp_Object Qsorted_list, Qdesc_sorted_list, Qunsorted_list;
Lisp_Object Qnoncyclic_directory, Qcyclic_directory;
Lisp_Object Qsymlink, Qalive_symlink, Qdead_symlink;
Lisp_Object Qwhiteout;

/* On GNU libc systems the declaration is only visible with _GNU_SOURCE.  */
#if defined(HAVE_CANONICALIZE_FILE_NAME)
#  if defined(NEED_DECLARATION_CANONICALIZE_FILE_NAME)
extern char *canonicalize_file_name(const char *);
#  endif
#define CANONICALISE_FILENAME(f)	canonicalize_file_name(f)

#else  /* !defined(HAVE_CANONICALIZE_FILE_NAME) */

static char *dired_realpath(const char *);
#define CANONICALISE_FILENAME(f)	dired_realpath(f)
#endif	/* defined(HAVE_CANONICALIZE_FILE_NAME) */

#ifndef TRIVIAL_DIRECTORY_ENTRY
#define TRIVIAL_DIRECTORY_ENTRY(n) (!strcmp (n, ".") || !strcmp (n, ".."))
#endif

#if 0
	/* this variant is much too slow */
#define FAST_CONCAT(tgt, s1, s2)	tgt = concat2(s1, s2);

#else  /* !0 */
#define FAST_CONCAT(tgt, s1, s2)	\
{					\
	tgt = make_uninit_string(XSTRING_LENGTH(s1)+XSTRING_LENGTH(s2));  \
	memcpy(XSTRING_DATA(tgt), XSTRING_DATA(s1), XSTRING_LENGTH(s1));  \
	memcpy(XSTRING_DATA(tgt)+XSTRING_LENGTH(s1),			  \
	       XSTRING_DATA(s2), XSTRING_LENGTH(s2));			  \
} while (0);
#endif	/* 0 */

/* some more declarations */
typedef struct dired_stack_item_s *dired_stack_item_t;
typedef struct dfr_options_s *dfr_options_t;

struct dired_stack_item_s {
	Lisp_Object dir;
	unsigned int depth;
};

struct dfr_options_s {
	long unsigned int maxdepth;
	_Bool fullp:1;
	_Bool symlink_file_p:1;
};

static Lisp_Object fname_as_directory(Lisp_Object);
static int pathname_matches_p(Lisp_Object, Lisp_Object,
			      struct re_pattern_buffer*);

#define dired_stack_t		dllist_t
#define new_dired_stack()	make_noseeum_dllist()
#define free_dired_stack(ds)	free_noseeum_dllist(ds)
#define dired_stack_pop(ds)	(dired_stack_item_t)dllist_pop_car(ds)
#define dired_stack_push(ds, p)	dllist_append(ds, p)
#define dired_stack_size(ds)	dllist_size(ds)


#if defined(HAVE_LARGEFILE)
#define dirent_t	struct dirent64
#define DFR_READDIR	readdir64_r
#else
#define dirent_t	struct dirent
#define DFR_READDIR	readdir_r
#endif

#if !defined(HAVE_CANONICALIZE_FILE_NAME)
static char *
dired_realpath(const char *file)
{
	char *result = xmalloc_atomic(4096); 	

	realpath(file, result);

	return result;
}
#endif

static Lisp_Object
fname_as_directory(Lisp_Object fname)
{
	if (XSTRING_LENGTH(fname) > 0)
		return Ffile_name_as_directory(fname);
	else
		return fname;
}

static int
pathname_matches_p(Lisp_Object pathname, Lisp_Object match,
		   struct re_pattern_buffer *bufp)
{
	int speccount2;
	char *mstr = NULL;
	int mlen = 0;
	int result = 1;

	if (STRINGP(match)) {
		mstr = (char*)XSTRING_DATA(pathname);
		mlen = XSTRING_LENGTH(pathname);
		if (re_search(bufp, mstr, mlen, 0, mlen, 0) < 0)
			result = 0;
	} else {
		speccount2 = specpdl_depth();
		record_unwind_protect(restore_gc_inhibit,
				      make_int(gc_currently_forbidden));
		gc_currently_forbidden = 1;
		if (NILP(call1_trapping_errors(
				 "Error in match function",
				 match, pathname)))
			result = 0;

		/* clean up */
		restore_match_data();
		unbind_to(speccount2, Qnil);
	}

	return result;
}


static Lisp_Object close_directory_unwind(Lisp_Object unwind_obj)
{
	DIR *d = (DIR *) get_opaque_ptr(unwind_obj);
	closedir(d);
	free_opaque_ptr(unwind_obj);
	return Qnil;
}


static void
dfr_inner(dirent_t *res,
	  Lisp_Object fulldir, Lisp_Object dir, Lisp_Object compbf,
	  dfr_options_t opts, Lisp_Object files_only,
	  unsigned int curdepth, dired_stack_t ds, Lisp_Object match,
	  struct re_pattern_buffer *bufp, Lisp_Object result,
	  Lisp_Object bloom_filter)
{
	/* this function can GC */
	int dir_p = 0;
	int result_p = 0;
	Lisp_Object name = Qnil;
	Lisp_Object fullname = Qnil;
	Lisp_Object resname = Qnil;
	int len;
	struct stat st;
	char *statnam = NULL;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(name, fullname, resname);

	if (!DIRENTRY_NONEMPTY(res) ||
	    (TRIVIAL_DIRECTORY_ENTRY(res->d_name) &&
	     !(NILP(Vdirectory_files_no_trivial_p) && opts->maxdepth == 0))) {
		UNGCPRO;
		return;
	}

	len = NAMLEN(res);
	resname = make_ext_string(res->d_name, len, Qfile_name);

	FAST_CONCAT(fullname, fulldir, resname);
	FAST_CONCAT(name, dir, resname);

	/* we want full file names? */
	if (opts->fullp) {
		resname = fullname;
	} else {
		resname = name;
	}

	/* check if we have to recur, i.e. if res was a
	   directory, otherwise we assume name to be a
	   file and cons it to the result */
#if defined(_DIRENT_HAVE_D_TYPE) && USE_D_TYPE
	if (res->d_type == DT_DIR) {
		dir_p = 1;
	} else if (res->d_type == DT_LNK && !opts->symlink_file_p) {
		char *canon_name = NULL;

		statnam = (char*)XSTRING_DATA(fullname);

		/* ugly things may happen when a link
		 * points back to a directory in our recurring
		 * area, ln -s . foo  is a candidate
		 * now, we canonicalise the filename, i.e.
		 * resolve all symlinks and afterwards we
		 * store it to our companion bloom filter
		 */
		canon_name = CANONICALISE_FILENAME(statnam);

		/* now, recycle full name */
		fullname = make_ext_string(
			canon_name, strlen(canon_name), Qfile_name);
		fullname = fname_as_directory(fullname);

		/* now stat statnam */
		if (sxemacs_stat(statnam, &st) == 0 &&
		    (st.st_mode & S_IFMT) == S_IFDIR &&
		    !NILP(compbf) &&
		    !(bloom_owns_p(XBLOOM(compbf), fullname))) {
			dir_p = 1;
		}

		if (canon_name) {
			xfree(canon_name);
		}
	}
#else  /* defined(_DIRENT_HAVE_D_TYPE) && USE_D_TYPE */
	statnam = (char*)XSTRING_DATA(fullname);
	if (sxemacs_stat(statnam, &st) == 0 &&
	    (st.st_mode & S_IFMT) == S_IFDIR) {
		char *canon_name = NULL;

		/* ugly things may happen when a link
		 * points back to a directory in our recurring
		 * area, ln -s . foo  is a candidate
		 * now, we canonicalise the filename, i.e.
		 * resolve all symlinks and afterwards we
		 * store it to our companion bloom filter
		 * The ugly things are even worse than in the
		 * case of D_TYPE, since we !always! have to
		 * check against the bloom filter.
		 */
		canon_name = CANONICALISE_FILENAME(statnam);

		/* now, recycle full name */
		fullname = make_ext_string(
			canon_name, strlen(canon_name),
			Qfile_name);
		fullname = fname_as_directory(fullname);

		/* now stat statnam */
		if (sxemacs_stat(statnam, &st) == 0 &&
		    (st.st_mode & S_IFMT) == S_IFDIR &&
		    /* does the bloom know about the dir? */
		    !NILP(compbf) &&
		    !(bloom_owns_p(XBLOOM(compbf), fullname))) {
			dir_p = 1;
		}

		if (canon_name) {
			xfree(canon_name);
		}
	}
#endif /* defined(_DIRENT_HAVE_D_TYPE) && USE_D_TYPE */

	/* argh, here is a design flaw!
	   these operations are not commutable, and it's a
	   hard-coded how `match' is interpreted.
	   * There are two possibilites:
	   * (1) check pathname against `match'
	   if nil, do not process further
	   if a directory, recur
	   if non-nil, add to result according to files_only
	   * (2) if a directory, recur
	   check pathname against `match'
	   if nil, do not add to result
	   if non-nil, add to result according to files_only
	   *
	   * Hm, I think I'd choose the latter variant, it is
	   not that performant, but it avoids two problems:

	   - With the former variant it is NOT possible to have
	   the trivial filenames on the result list, since a
	   match against "^[.]$" would exclude everything, while
	   actually it was likely meant to _solely_ exclude "." 
	   from the result list
	   - Furthermore, we _MUST_ traverse in preorder,
	   otherwise there is the possibility that pathnames are
	   on the file list already which turn out later to be
	   excluded
	   * Anyone wants to help brainstorming?
	   */

	/* check if we put it on the list of matches */
	if (NILP(files_only)) {
		result_p = 1;
	} else if (EQ(files_only, Qt) && !dir_p) {
		result_p = 1;
	} else if (!EQ(files_only, Qt) && dir_p) {
		result_p = 1;
	} else {
		result_p = 0;
	}

	if (curdepth >= opts->maxdepth) {
		dir_p = 0;
	}

	if (dir_p) {
		dired_stack_item_t dsi;
		dsi = xnew_and_zero(struct dired_stack_item_s);
		dsi->dir = name;
		dsi->depth = 1+curdepth;
		dired_stack_push(ds, dsi);
	}

#if USE_MATCH_ARG
	if (!NILP(match) && !pathname_matches_p(name, match, bufp)) {
		result_p = 0;
	}
#endif

	if (result_p) {
		dllist_append(XDLLIST(result), (void*)resname);
		/* add the result to the companion bloom-f */
		/* hm, for large trees this yields a bf which
		   owns everything :( ... we need far better and
		   faster bloom techniques for it -hroptatyr */
		if (!NILP(bloom_filter)) {
			bloom_add(XBLOOM(bloom_filter), resname);
		}
	}

	UNGCPRO;
	return;
}

static void
dfr_outer(Lisp_Object directory, dirent_t *ent,
	  Lisp_Object compbf, dfr_options_t opts,
	  Lisp_Object files_only, dired_stack_t ds, Lisp_Object match,
	  struct re_pattern_buffer *bufp, Lisp_Object result,
	  Lisp_Object bloom_filter)
{
	dired_stack_item_t dir_dpt = dired_stack_pop(ds);
	Lisp_Object dir = dir_dpt->dir;
	unsigned int dpt = dir_dpt->depth;
	Lisp_Object fulldir = Fexpand_file_name(dir, directory);
	DIR *d = NULL;
	dirent_t *res = NULL;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(dir, fulldir);

	xfree(dir_dpt);

	dir = fname_as_directory(dir);
	fulldir = fname_as_directory(fulldir);

	/* add the full directory name to the companion bloom filter */
	if (!NILP(compbf))
		bloom_add(XBLOOM(compbf), fulldir);

	/* external format conversion is done in the encapsulation of
	 * opendir in sysdep.c
	 */
	d = opendir((char*)XSTRING_DATA(fulldir));
#if 0
	/* why should we want this? I think spitting a warning
	 * should suffice
	 * -hroptatyr
	 */
	if (!d) {
		xfree(ent);
		report_file_error("Opening directory", list1(fulldir));
		return Qnil;
	}
#else
	if (!d) {
		warn_when_safe(Qfile, Qwarning,
			       "Opening directory `%s' failed",
			       (char*)XSTRING_DATA(fulldir));
		UNGCPRO;
		return;
	}
#endif

	record_unwind_protect(close_directory_unwind,
			      make_opaque_ptr((void *)d));

	while (DFR_READDIR(d, ent, &res) == 0 && res != NULL) {
		dfr_inner(res, fulldir, dir,
			  compbf, opts,
			  files_only, dpt, ds, match, bufp,
			  result, bloom_filter);
	}

	UNGCPRO;
}

static void
dired_stack_mark(Lisp_Object obj)
{
	dired_stack_t ds = get_dynacat(obj);
	WITH_DLLIST_TRAVERSE(
		ds,
		dired_stack_item_t dsi = dllist_item;
		mark_object(dsi->dir));
	return;
}

#if 1
static void
dired_stack_fini(Lisp_Object obj)
{
	dired_stack_t ds = get_dynacat(obj);
	free_dired_stack(ds);
	return;
}
#endif

static Lisp_Object
directory_files_magic(Lisp_Object directory, Lisp_Object match,
		      Lisp_Object files_only, Lisp_Object bloom_filter,
		      dfr_options_t opts)
{
	/* This function can GC */
	Lisp_Object result = wrap_dllist(make_dllist());
	Lisp_Object lds = Qnil;
	dired_stack_t ds = NULL;
	dired_stack_item_t ds_item = NULL;
	/* this is a companion bloom filter,
	 * we register processed directories in here and hence avoid
	 * processing an entry twice */
	Lisp_Object compbf = Qnil;
	int speccount = specpdl_depth();
#if USE_MATCH_ARG
	struct re_pattern_buffer *bufp = NULL;
#endif
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

	ds = new_dired_stack();
	lds = make_dynacat(ds);
	set_dynacat_marker(lds, dired_stack_mark);
	set_dynacat_finaliser(lds, (dynacat_finaliser_f)dired_stack_fini);
	GCPRO5(directory, result, compbf, bloom_filter, lds);

#if USE_MATCH_ARG
	/* SXEmacs: this should come after Ffile_name_as_directory() to avoid
	   potential regexp cache smashage.  It comes before the opendir()
	   because it might signal an error.  */
	if (!NILP(match)) {
		if (STRINGP(match)) {

			/* MATCH might be a flawed regular expression.  Rather
			   than catching and signalling our own errors, we just
			   call compile_pattern to do the work for us.  */
			bufp = compile_pattern(match, 0, Qnil, 0, ERROR_ME);
			/* Now *bufp is the compiled form of MATCH; don't call
			   anything which might compile a new regexp until we
			   are done with the loop!  */

		} else if (!NILP(Ffunctionp(match))) {
			;
		} else {
			return wrong_type_argument(Qstringp, match);
		}
	}

	regex_match_object = Qnil;
	regex_emacs_buffer = current_buffer;
#endif

	if (opts->maxdepth > 0) {
		compbf = make_bloom(8192, 8);
	}

	/* set up the directories queue */
	ds_item = xnew_and_zero(struct dired_stack_item_s);
	ds_item->dir = make_string((Bufbyte*)"", 0);
	ds_item->depth = 0;
	dired_stack_push(ds, ds_item);

	/* alloc the directory entry pointer */
	{
		dirent_t _ent, *ent = &_ent;

		/* clean sweep */
		memset(ent, 0, sizeof(dirent_t));

		while (dired_stack_size(ds) > 0) {
			dfr_outer(directory, ent, compbf,
				  opts, files_only, ds, match,
				  bufp, result, bloom_filter);
			/* This will close the dir */
			unbind_to(speccount, Qnil);
			QUIT;
		}
	}

	/* save the companion bloom filter */
	Fput(result, Qcompanion_bf, compbf);

	UNGCPRO;
	return result;
}

static Lisp_Object
directory_files_canonicalise_dn(Lisp_Object directory)
{
	struct gcpro gcpro1;
	GCPRO1(directory);

	/* expand the directory argument and canonicalise */
	directory = Fexpand_file_name(directory, Qnil);
	directory = fname_as_directory(directory);

	RETURN_UNGCPRO(directory);
}

static Lisp_Object
directory_files_resultify(Lisp_Object result, Lisp_Object result_type)
{
	/* This function can GC */
	Lisp_Object final_result = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	GCPRO3(result, result_type, final_result);

	/* see if the user requested a dllist */
	if (EQ(result_type, Qdllist)) {
		final_result = result;
	} else if (NILP(result_type) || EQ(result_type, Qsorted_list)) {
		final_result = Fdllist_to_list_reversed(result);
		final_result = Fsort(final_result, Qstring_lessp);
	} else if (EQ(result_type, Qdesc_sorted_list)) {
		final_result = Fdllist_to_list(result);
		final_result = Fsort(final_result, Qstring_greaterp);
	} else if (EQ(result_type, Qt) || EQ(result_type, Qlist)) {
		final_result = Fdllist_to_list(result);
	}

	UNGCPRO;
	return final_result;
}

static Lisp_Object
call9(Lisp_Object fn,
      Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
      Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5,
      Lisp_Object arg6, Lisp_Object arg7, Lisp_Object arg8)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object res, args[10] = {fn, arg0, arg1, arg2, arg3,
				     arg4, arg5, arg6, arg7, arg8};

	GCPROn(args, countof(args));
	res = Ffuncall(10, args);

	UNGCPRO;
	return res;
}


EXFUN(Fdirectory_files_recur, 8);

DEFUN("directory-files", Fdirectory_files, 1, 5, 0,	/*
Return a list of names of files in DIRECTORY.
Args are DIRECTORY &optional FULL MATCH RESULT-TYPE FILES_ONLY.

There are four optional arguments:
If FULL is non-nil, absolute pathnames of the files are returned.

If MATCH is non-nil, it may be a string indicating a regular
expression which pathnames must meet in order to be returned.
Moreover, a predicate function can be specified which is called with
one argument, the pathname in question.  On non-nil return value,
the pathname is considered in the final result, otherwise it is
ignored.

Optional argument RESULT-TYPE can be one of:
- sorted-list (default)  to return a list, sorted in alphabetically
  ascending order
- desc-sorted-list  to return a list, sorted in alphabetically
  descending order
- list  to return an unsorted list
- dllist  to return an unsorted dllist
The two latter types can be useful if you plan to sort the result
yourself, or want to feed the result to further processing.

Optional argument FILES-ONLY can be one of:
- t  to return only files and symlinks in DIRECTORY
- nil (default)  to return all entries (files, symlinks, and
  subdirectories) in DIRECTORY
- subdir  to return only subdirectories -- but *NOT* symlinks to
  directories -- in DIRECTORY
							*/
      (directory, full, match, result_type, files_only))
{
	Lisp_Object handler;
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
	struct dfr_options_s opts = {
		.maxdepth = 0,
		.fullp = !NILP(full),
		.symlink_file_p = 0,
	};
	GCPRO6(directory, full, match, result_type, files_only, result);

	directory = directory_files_canonicalise_dn(directory);

	/* If the file name has special constructs in it,
	   call the corresponding file handler.  */
	handler = Ffind_file_name_handler(directory, Qdirectory_files);
	if (!NILP(handler)) {
		UNGCPRO;
		return call6(handler, Qdirectory_files,
			     directory, full, match, result_type, files_only);
	}

	result = directory_files_magic(directory, match,
				       files_only, /* bloom filter */Qnil,
				       &opts);

	UNGCPRO;
	return directory_files_resultify(result, result_type);
}

DEFUN("directory-files-recur", Fdirectory_files_recur, 1, 8, 0,	/*
Like `directory-files' but recursive and much faster.
Args are DIRECTORY &optional FULL MATCH RESULT_TYPE FILES-ONLY MAXDEPTH
SYMLINK_IS_FILE BLOOM_FILTER

If FULL is non-nil, absolute pathnames of the files are returned.

If MATCH is non-nil, it may be a string indicating a regular
expression which pathnames must meet in order to be returned.
Moreover, a predicate function can be specified which is called with
one argument, the pathname in question.  On non-nil return value,
the pathname is considered in the final result, otherwise it is
ignored.

Optional argument RESULT-TYPE can be one of:
- sorted-list (default)  to return a list, sorted in alphabetically
  ascending order
- desc-sorted-list  to return a list, sorted in alphabetically
  descending order
- list  to return an unsorted list
- dllist  to return an unsorted dllist
The two latter types can be useful if you plan to sort the result
yourself, or want to feed the result to further processing.

Optional argument FILES-ONLY can be one of:
- t  to return only files and symlinks in DIRECTORY
- nil (default)  to return all entries (files, symlinks, and
  subdirectories) in DIRECTORY
- subdir  to return only subdirectories -- but *NOT* symlinks to
  directories -- in DIRECTORY

Optional argument MAXDEPTH \(a positive integer\) specifies the
maximal recursion depth, use 0 to emulate old `directory-files'.

Optional argument SYMLINK-IS-FILE specifies whether symlinks
should be resolved \(which is the default behaviour\) or whether 
they are treated as ordinary files \(non-nil\), in the latter
case symlinks to directories are not recurred.

Optional argument BLOOM-FILTER specifies a bloom filter where
to put results in addition to the ordinary result list.
								*/
      (directory, full, match, result_type, files_only, maxdepth,
       symlink_is_file, bloom_filter))
#if 0
      (int nargs, Lisp_Object *args))
#endif
{
	Lisp_Object handler = Qnil, result = Qnil;
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	/* just a convenience array for gc pro'ing */
	Lisp_Object args[8] = {
		directory, match, result_type, files_only,
		symlink_is_file, bloom_filter, handler, result};
#endif	/* !BDWGC */
	struct dfr_options_s opts = {
		.maxdepth = 64,
		.fullp = !NILP(full),
		.symlink_file_p = !NILP(symlink_is_file),
	};
	struct gcpro gcpro1;

	/* argument checks */
	CHECK_STRING(directory);
	if (!NILP(match)) {
		CHECK_STRING(match);
	}
	if (!NILP(maxdepth)) {
		CHECK_NATNUM(maxdepth);
		opts.maxdepth = XUINT(maxdepth);
	}

	GCPROn(args, countof(args));

	directory = directory_files_canonicalise_dn(directory);

	/* If the file name has special constructs in it,
	   call the corresponding file handler.  */
	handler = Ffind_file_name_handler(directory, Qdirectory_files_recur);
	if (!NILP(handler)) {
		Lisp_Object res;

		res = call9(handler, Qdirectory_files_recur,
			    directory, full, match, result_type, files_only,
			    maxdepth, symlink_is_file, bloom_filter);
		UNGCPRO;
		return res;
	}

	result = directory_files_magic(directory, match,
				       files_only, bloom_filter,
				       &opts);
	/* convert to final result type */
	result = directory_files_resultify(result, result_type);
	UNGCPRO;
	return result;
}


static Lisp_Object file_name_completion(Lisp_Object file,
					Lisp_Object directory,
					int all_flag, int ver_flag);

DEFUN("file-name-completion", Ffile_name_completion, 2, 2, 0,	/*
Complete file name PARTIAL-FILENAME in directory DIRECTORY.
Return the longest prefix common to all file names in DIRECTORY
that start with PARTIAL-FILENAME.
If there is only one and PARTIAL-FILENAME matches it exactly, return t.
Return nil if DIRECTORY contains no name starting with PARTIAL-FILENAME.

File names which end with any member of `completion-ignored-extensions'
are not considered as possible completions for PARTIAL-FILENAME unless
there is no other possible completion. `completion-ignored-extensions'
is not applied to the names of directories.
								 */
      (partial_filename, directory))
{
	/* This function can GC.  GC checked 1996.04.06. */
	Lisp_Object handler;

	/* If the directory name has special constructs in it,
	   call the corresponding file handler.  */
	handler = Ffind_file_name_handler(directory, Qfile_name_completion);
	if (!NILP(handler))
		return call3(handler, Qfile_name_completion, partial_filename,
			     directory);

	/* If the file name has special constructs in it,
	   call the corresponding file handler.  */
	handler =
	    Ffind_file_name_handler(partial_filename, Qfile_name_completion);
	if (!NILP(handler))
		return call3(handler, Qfile_name_completion, partial_filename,
			     directory);

	return file_name_completion(partial_filename, directory, 0, 0);
}

DEFUN("file-name-all-completions", Ffile_name_all_completions, 2, 2, 0,	/*
Return a list of all completions of PARTIAL-FILENAME in DIRECTORY.
These are all file names in DIRECTORY which begin with PARTIAL-FILENAME.
									 */
      (partial_filename, directory))
{
	/* This function can GC. GC checked 1997.06.04. */
	Lisp_Object handler;
	struct gcpro gcpro1;

	GCPRO1(directory);
	directory = Fexpand_file_name(directory, Qnil);
	/* If the file name has special constructs in it,
	   call the corresponding file handler.  */
	handler =
	    Ffind_file_name_handler(directory, Qfile_name_all_completions);
	UNGCPRO;
	if (!NILP(handler))
		return call3(handler, Qfile_name_all_completions,
			     partial_filename, directory);

	return file_name_completion(partial_filename, directory, 1, 0);
}

static int
file_name_completion_stat(Lisp_Object directory, DIRENTRY * dp,
			  struct stat *st_addr)
{
	Bytecount len = NAMLEN(dp);
	Bytecount pos = XSTRING_LENGTH(directory);
	int value;
	char *fullname = (char *)alloca(len + pos + 2);

	memcpy(fullname, XSTRING_DATA(directory), pos);
	if (!IS_DIRECTORY_SEP(fullname[pos - 1]))
		fullname[pos++] = DIRECTORY_SEP;

	memcpy(fullname + pos, dp->d_name, len);
	fullname[pos + len] = 0;

#ifdef S_IFLNK
	/* We want to return success if a link points to a nonexistent file,
	   but we want to return the status for what the link points to,
	   in case it is a directory.  */
	value = lstat(fullname, st_addr);
	if (S_ISLNK(st_addr->st_mode))
		sxemacs_stat(fullname, st_addr);
#else
	value = sxemacs_stat(fullname, st_addr);
#endif
	return value;
}

static Lisp_Object file_name_completion_unwind(Lisp_Object locative)
{
	DIR *d;
	Lisp_Object obj = XCAR(locative);

	if (!NILP(obj)) {
		d = (DIR *) get_opaque_ptr(obj);
		closedir(d);
		free_opaque_ptr(obj);
	}
	free_cons(XCONS(locative));
	return Qnil;
}

static Lisp_Object
file_name_completion(Lisp_Object file, Lisp_Object directory, int all_flag,
		     int ver_flag)
{
	/* This function can GC */
	DIR *d = 0;
	int matchcount = 0;
	Lisp_Object bestmatch = Qnil;
	Charcount bestmatchsize = 0;
	struct stat st;
	int passcount;
	int speccount = specpdl_depth();
	Charcount file_name_length;
	Lisp_Object locative;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(file, directory, bestmatch);

	CHECK_STRING(file);

#ifdef FILE_SYSTEM_CASE
	file = FILE_SYSTEM_CASE(file);
#endif
	directory = Fexpand_file_name(directory, Qnil);
	file_name_length = XSTRING_CHAR_LENGTH(file);

	/* With passcount = 0, ignore files that end in an ignored extension.
	   If nothing found then try again with passcount = 1, don't ignore them.
	   If looking for all completions, start with passcount = 1,
	   so always take even the ignored ones.

	   ** It would not actually be helpful to the user to ignore any possible
	   completions when making a list of them.**  */

	/* We cannot use close_directory_unwind() because we change the
	   directory.  The old code used to just avoid signaling errors, and
	   call closedir, but it was wrong, because it made sane handling of
	   QUIT impossible and, besides, various utility functions like
	   regexp_ignore_completion_p can signal errors.  */
	locative = noseeum_cons(Qnil, Qnil);
	record_unwind_protect(file_name_completion_unwind, locative);

	for (passcount = !!all_flag; NILP(bestmatch) && passcount < 2;
	     passcount++) {
		Lisp_Object tmp_dfn = Fdirectory_file_name(directory);
		d = opendir((char *)XSTRING_DATA(tmp_dfn));
		if (!d) {
			report_file_error("Opening directory",
					  list1(directory));
		}
		XCAR(locative) = make_opaque_ptr((void *)d);

		/* Loop reading blocks */
		while (1) {
			DIRENTRY *dp;
			Bytecount len;
			/* scmp() works in characters, not bytes, so we have to compute
			   this value: */
			Charcount cclen;
			int directoryp;
			int ignored_extension_p = 0;
			Bufbyte *d_name;

			dp = readdir(d);
			if (!dp)
				break;

			/* Cast to Bufbyte* is OK, as readdir() Mule-encapsulates.  */
			d_name = (Bufbyte *) dp->d_name;
			len = NAMLEN(dp);
			cclen = bytecount_to_charcount(d_name, len);

			QUIT;

			if (!DIRENTRY_NONEMPTY(dp)
			    || cclen < file_name_length
			    || 0 <= scmp(d_name, XSTRING_DATA(file),
					 file_name_length))
				continue;

			if (file_name_completion_stat(directory, dp, &st) < 0)
				continue;

			directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
			if (directoryp) {
				/* "." and ".." are never interesting as completions, but are
				   actually in the way in a directory containing only one file.  */
				if (!passcount
				    && TRIVIAL_DIRECTORY_ENTRY(dp->d_name))
					continue;
			} else {
				/* Compare extensions-to-be-ignored against end of this file name */
				/* if name is not an exact match against specified string.  */
				if (!passcount && cclen > file_name_length) {
					Lisp_Object tem;
					/* and exit this for loop if a match is found */
					EXTERNAL_LIST_LOOP(tem,
							   Vcompletion_ignored_extensions)
					{
						Lisp_Object elt = XCAR(tem);
						Charcount skip;

						CHECK_STRING(elt);

						skip =
						    cclen -
						    XSTRING_CHAR_LENGTH(elt);
						if (skip < 0)
							continue;

						if (0 >
						    scmp(charptr_n_addr
							 (d_name, skip),
							 XSTRING_DATA(elt),
							 XSTRING_CHAR_LENGTH
							 (elt))) {
							ignored_extension_p = 1;
							break;
						}
					}
				}
			}

			/* If an ignored-extensions match was found,
			   don't process this name as a completion.  */
			if (!passcount && ignored_extension_p)
				continue;

			if (!passcount
			    && regexp_ignore_completion_p(d_name, Qnil, 0,
							  cclen))
				continue;

			/* Update computation of how much all possible completions match */
			matchcount++;

			if (all_flag || NILP(bestmatch)) {
				Lisp_Object name = Qnil;
				struct gcpro ngcpro1;
				NGCPRO1(name);
				/* This is a possible completion */
				name = make_string(d_name, len);
				if (directoryp)	/* Completion is a directory; end it with '/' */
					name = Ffile_name_as_directory(name);
				if (all_flag) {
					bestmatch = Fcons(name, bestmatch);
				} else {
					bestmatch = name;
					bestmatchsize =
					    XSTRING_CHAR_LENGTH(name);
				}
				NUNGCPRO;
			} else {
				Charcount compare = min(bestmatchsize, cclen);
				Bufbyte *p1 = XSTRING_DATA(bestmatch);
				Bufbyte *p2 = d_name;
				Charcount matchsize = scmp(p1, p2, compare);

				if (matchsize < 0)
					matchsize = compare;
				if (completion_ignore_case) {
					/* If this is an exact match except for case,
					   use it as the best match rather than one that is not
					   an exact match.  This way, we get the case pattern
					   of the actual match.  */
					if ((matchsize == cclen
					     && matchsize + !!directoryp
					     < XSTRING_CHAR_LENGTH(bestmatch))
					    ||
					    /* If there is no exact match ignoring case,
					       prefer a match that does not change the case
					       of the input.  */
					    (((matchsize == cclen)
					      ==
					      (matchsize + !!directoryp
					       ==
					       XSTRING_CHAR_LENGTH(bestmatch)))
					     /* If there is more than one exact match aside from
					        case, and one of them is exact including case,
					        prefer that one.  */
					     && 0 > scmp_1(p2,
							   XSTRING_DATA(file),
							   file_name_length, 0)
					     && 0 <= scmp_1(p1,
							    XSTRING_DATA(file),
							    file_name_length,
							    0))) {
						bestmatch =
						    make_string(d_name, len);
						if (directoryp)
							bestmatch =
							    Ffile_name_as_directory
							    (bestmatch);
					}
				}

				/* If this directory all matches,
				   see if implicit following slash does too.  */
				if (directoryp
				    && compare == matchsize
				    && bestmatchsize > matchsize
				    &&
				    IS_ANY_SEP(charptr_emchar_n(p1, matchsize)))
					matchsize++;
				bestmatchsize = matchsize;
			}
		}
		closedir(d);
		free_opaque_ptr(XCAR(locative));
		XCAR(locative) = Qnil;
	}

	unbind_to(speccount, Qnil);

	UNGCPRO;

	if (all_flag || NILP(bestmatch))
		return bestmatch;
	if (matchcount == 1 && bestmatchsize == file_name_length)
		return Qt;
	return Fsubstring(bestmatch, Qzero, make_int(bestmatchsize));
}

static Lisp_Object user_name_completion(Lisp_Object user,
					int all_flag, int *uniq);

DEFUN("user-name-completion", Fuser_name_completion, 1, 1, 0,	/*
Complete user name from PARTIAL-USERNAME.
Return the longest prefix common to all user names starting with
PARTIAL-USERNAME.  If there is only one and PARTIAL-USERNAME matches
it exactly, returns t.  Return nil if there is no user name starting
with PARTIAL-USERNAME.
								 */
      (partial_username))
{
	return user_name_completion(partial_username, 0, NULL);
}

DEFUN("user-name-completion-1", Fuser_name_completion_1, 1, 1, 0,	/*
Complete user name from PARTIAL-USERNAME.

This function is identical to `user-name-completion', except that
the cons of the completion and an indication of whether the
completion was unique is returned.

The car of the returned value is the longest prefix common to all user
names that start with PARTIAL-USERNAME.  If there is only one and
PARTIAL-USERNAME matches it exactly, the car is t.  The car is nil if
there is no user name starting with PARTIAL-USERNAME.  The cdr of the
result is non-nil if and only if the completion returned in the car
was unique.
									 */
      (partial_username))
{
	int uniq;
	Lisp_Object completed =
	    user_name_completion(partial_username, 0, &uniq);
	return Fcons(completed, uniq ? Qt : Qnil);
}

DEFUN("user-name-all-completions", Fuser_name_all_completions, 1, 1, 0,	/*
Return a list of all user name completions from PARTIAL-USERNAME.
These are all the user names which begin with PARTIAL-USERNAME.
									 */
      (partial_username))
{
	return user_name_completion(partial_username, 1, NULL);
}

struct user_name {
	Bufbyte *ptr;
	size_t len;
};

struct user_cache {
	struct user_name *user_names;
	int length;
	int size;
	EMACS_TIME last_rebuild_time;
};
static struct user_cache user_cache;

static void free_user_cache(struct user_cache *cache)
{
	int i;
	for (i = 0; i < cache->length; i++)
		xfree(cache->user_names[i].ptr);
	xfree(cache->user_names);
	xzero(*cache);
}

static Lisp_Object user_name_completion_unwind(Lisp_Object cache_incomplete_p)
{
	endpwent();
	speed_up_interrupts();

	if (!NILP(XCAR(cache_incomplete_p)))
		free_user_cache(&user_cache);

	free_cons(XCONS(cache_incomplete_p));

	return Qnil;
}

#define  USER_CACHE_TTL  (24*60*60)	/* Time to live: 1 day, in seconds */

static Lisp_Object
user_name_completion(Lisp_Object user, int all_flag, int *uniq)
{
	/* This function can GC */
	int matchcount = 0;
	Lisp_Object bestmatch = Qnil;
	Charcount bestmatchsize = 0;
	Charcount user_name_length;
	EMACS_TIME t;
	int i;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(user, bestmatch);

	CHECK_STRING(user);

	user_name_length = XSTRING_CHAR_LENGTH(user);

	/* Cache user name lookups because it tends to be quite slow.
	 * Rebuild the cache occasionally to catch changes */
	EMACS_GET_TIME(t);
	if (user_cache.user_names &&
	    (EMACS_SECS(t) - EMACS_SECS(user_cache.last_rebuild_time)
	     > USER_CACHE_TTL))
		free_user_cache(&user_cache);

	if (!user_cache.user_names) {
		struct passwd *pwd;
		Lisp_Object cache_incomplete_p = noseeum_cons(Qt, Qnil);
		int speccount = specpdl_depth();

		slow_down_interrupts();
		setpwent();
		record_unwind_protect(user_name_completion_unwind,
				      cache_incomplete_p);
		while ((pwd = getpwent())) {
			QUIT;
			DO_REALLOC(user_cache.user_names, user_cache.size,
				   user_cache.length + 1, struct user_name);
			TO_INTERNAL_FORMAT(C_STRING, pwd->pw_name,
					   MALLOC,
					   (user_cache.
					    user_names[user_cache.length].ptr,
					    user_cache.user_names[user_cache.
								  length].len),
					   Qnative);
			user_cache.length++;
		}
		XCAR(cache_incomplete_p) = Qnil;
		unbind_to(speccount, Qnil);

		EMACS_GET_TIME(user_cache.last_rebuild_time);
	}

	for (i = 0; i < user_cache.length; i++) {
		Bufbyte *u_name = user_cache.user_names[i].ptr;
		Bytecount len = user_cache.user_names[i].len;
		/* scmp() works in chars, not bytes, so we have to compute this: */
		Charcount cclen = bytecount_to_charcount(u_name, len);

		QUIT;

		if (cclen < user_name_length
		    || 0 <= scmp_1(u_name, XSTRING_DATA(user), user_name_length,
				   0))
			continue;

		matchcount++;	/* count matching completions */

		if (all_flag || NILP(bestmatch)) {
			Lisp_Object name = Qnil;
			struct gcpro ngcpro1;
			NGCPRO1(name);
			/* This is a possible completion */
			name = make_string(u_name, len);
			if (all_flag) {
				bestmatch = Fcons(name, bestmatch);
			} else {
				bestmatch = name;
				bestmatchsize = XSTRING_CHAR_LENGTH(name);
			}
			NUNGCPRO;
		} else {
			Charcount compare = min(bestmatchsize, cclen);
			Bufbyte *p1 = XSTRING_DATA(bestmatch);
			Bufbyte *p2 = u_name;
			Charcount matchsize = scmp_1(p1, p2, compare, 0);

			if (matchsize < 0)
				matchsize = compare;

			bestmatchsize = matchsize;
		}
	}

	UNGCPRO;

	if (uniq)
		*uniq = (matchcount == 1);

	if (all_flag || NILP(bestmatch))
		return bestmatch;
	if (matchcount == 1 && bestmatchsize == user_name_length)
		return Qt;
	return Fsubstring(bestmatch, Qzero, make_int(bestmatchsize));
}

Lisp_Object make_directory_hash_table(const char *path)
{
	DIR *d;
	if ((d = opendir(path))) {
		DIRENTRY *dp;
		Lisp_Object hash =
		    make_lisp_hash_table(20, HASH_TABLE_NON_WEAK,
					 HASH_TABLE_EQUAL);

		while ((dp = readdir(d))) {
			Bytecount len = NAMLEN(dp);
			if (DIRENTRY_NONEMPTY(dp))
				/* Cast to Bufbyte* is OK, as readdir() Mule-encapsulates.  */
				Fputhash(make_string
					 ((Bufbyte *) dp->d_name, len), Qt,
					 hash);
		}
		closedir(d);
		return hash;
	} else
		return Qnil;
}

#if 0
/* ... never used ... should use list2 directly anyway ... */
/* NOTE: This function can never return a negative value. */
Lisp_Object wasteful_word_to_lisp(unsigned int item)
{
	/* Compatibility: in other versions, file-attributes returns a LIST
	   of two 16 bit integers... */
	Lisp_Object cons = word_to_lisp(item);
	XCDR(cons) = Fcons(XCDR(cons), Qnil);
	return cons;
}
#endif

DEFUN("file-attributes", Ffile_attributes, 1, 1, 0,	/*
Return a list of attributes of file FILENAME.
Value is nil if specified file cannot be opened.
Otherwise, list elements are:
0. t for directory, string (name linked to) for symbolic link, or nil.
1. Number of links to file.
2. File uid.
3. File gid.
4. Last access time, as a list of two integers.
First integer has high-order 16 bits of time, second has low 16 bits.
5. Last modification time, likewise.
6. Last status change time, likewise.
7. Size in bytes. (-1, if number is out of range).
8. File modes, as a string of ten letters or dashes as in ls -l.
9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

If file does not exist, returns nil.
							 */
      (filename))
{
	/* This function can GC. GC checked 1997.06.04. */
	Lisp_Object values[12];
#if defined (BSD4_2) || defined (BSD4_3) ||	\
	!defined HAVE_BDWGC || !defined EF_USE_BDWGC
	Lisp_Object directory = Qnil;
#endif	/* BSD4_2 || BSD4_3 || !BDWGC */
	struct stat s;
	char modes[10];
	Lisp_Object handler;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(filename, directory);
	filename = Fexpand_file_name(filename, Qnil);

	/* If the file name has special constructs in it,
	   call the corresponding file handler.  */
	handler = Ffind_file_name_handler(filename, Qfile_attributes);
	if (!NILP(handler)) {
		UNGCPRO;
		return call2(handler, Qfile_attributes, filename);
	}

	if (lstat((char *)XSTRING_DATA(filename), &s) < 0) {
		UNGCPRO;
		return Qnil;
	}
#ifdef BSD4_2
	directory = Ffile_name_directory(filename);
#endif

	switch (s.st_mode & S_IFMT) {
	default:
		values[0] = Qnil;
		break;
	case S_IFDIR:
		values[0] = Qt;
		break;
#ifdef S_IFLNK
	case S_IFLNK:
		values[0] = Ffile_symlink_p(filename);
		break;
#endif
	}
	values[1] = make_int(s.st_nlink);
	values[2] = make_int(s.st_uid);
	values[3] = make_int(s.st_gid);
	values[4] = make_time(s.st_atime);
	values[5] = make_time(s.st_mtime);
	values[6] = make_time(s.st_ctime);
	values[7] = make_int((EMACS_INT) s.st_size);
	/* If the size is out of range, give back -1.  */
	/* #### Fix when Emacs gets bignums! */
	if (XINT(values[7]) != s.st_size)
		values[7] = make_int(-1);
	filemodestring(&s, modes);
	values[8] = make_string((Bufbyte *) modes, 10);
#if defined (BSD4_2) || defined (BSD4_3)	/* file gid will be dir gid */
	{
		struct stat sdir;

		if (!NILP(directory)
		    && sxemacs_stat((char *)XSTRING_DATA(directory), &sdir) == 0)
			values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
		else		/* if we can't tell, assume worst */
			values[9] = Qt;
	}
#else				/* file gid will be egid */
	values[9] = (s.st_gid != getegid())? Qt : Qnil;
#endif				/* BSD4_2 or BSD4_3 */
	values[10] = make_int(s.st_ino);
	values[11] = make_int(s.st_dev);
	UNGCPRO;
	return Flist(countof(values), values);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_dired(void)
{
	defsymbol(&Qdirectory_files, "directory-files");
	defsymbol(&Qdirectory_files_recur, "directory-files-recur");
	defsymbol(&Qfile_name_completion, "file-name-completion");
	defsymbol(&Qfile_name_all_completions, "file-name-all-completions");
	defsymbol(&Qfile_attributes, "file-attributes");

	defsymbol(&Qcompanion_bf, "companion-bf");
	defsymbol(&Qsorted_list, "sorted-list");
	defsymbol(&Qdesc_sorted_list, "desc-sorted-list");
	defsymbol(&Qunsorted_list, "unsorted-list");

	DEFSUBR(Fdirectory_files);
	DEFSUBR(Fdirectory_files_recur);
	DEFSUBR(Ffile_name_completion);
	DEFSUBR(Ffile_name_all_completions);
	DEFSUBR(Fuser_name_completion);
	DEFSUBR(Fuser_name_completion_1);
	DEFSUBR(Fuser_name_all_completions);
	DEFSUBR(Ffile_attributes);
}

void vars_of_dired(void)
{
	DEFVAR_LISP("completion-ignored-extensions", &Vcompletion_ignored_extensions	/*
*Completion ignores filenames ending in any string in this list.
This variable does not affect lists of possible completions,
but does affect the commands that actually do completions.
It is used by the function `file-name-completion'.
											 */ );
	Vcompletion_ignored_extensions = Qnil;

	DEFVAR_LISP("directory-files-no-trivial-p",
		    &Vdirectory_files_no_trivial_p	/*
Determine whether to _not_ add the trivial directory entries
`.' and `..'.
ATTENTION: This variable is definitely NOT for users.
For easy temporary circumvention use a let binding.
							*/ );
	Vdirectory_files_no_trivial_p = Qnil;
}
