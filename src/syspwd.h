/*

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
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not really in FSF. */

#ifndef WIN32_NATIVE

# include <pwd.h>

#else /* WIN32_NATIVE */

struct passwd {
    char *pw_name;
    char *pw_passwd;
    int   pw_uid;
    int   pw_gid;
    int   pw_quota;
    char *pw_gecos;
    char *pw_dir;
    char *pw_shell;
};

#endif /* WIN32_NATIVE */
