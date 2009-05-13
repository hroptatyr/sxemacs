#! /bin/sh
### gzip-el.sh --- compress superfluous installed source lisp

# Author:	Jeff Miller <jmiller@smart.net>
# Author:	Hrvoje Niksic <hniksic@xemacs.org>
# Maintainer:	Steve Baur <steve@xemacs.org>
# Created:	13 Feb 1997
# Version:	1.0
# Keywords:	internal

# This file is part of SXEmacs.

# SXEmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# SXEmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#
#
echo Compressing .el files in "$1"...

find "$1" -type f -name "*.el" -print |
	while read file; do
		[ -s "${file}c" ] && echo "$file" && gzip -f9 "$file"
	done

echo Compressing .el files in "$1"...done.
