/* Sound in windows nt XEmacs.
   Copyright (C) 1998 Andy Piper.

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
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.*/

#include <config.h>
#include "lisp.h"

#include "sysfile.h"
#include "nt.h"
#include "nativesound.h"

static int play_sound_data_1 (unsigned char *data, int length,
			       int volume, int convert);

void play_sound_file (char *sound_file, int volume)
{
  DWORD flags = SND_ASYNC | SND_NODEFAULT | SND_FILENAME;
  OFSTRUCT ofs;
  Lisp_Object fname = Ffile_name_nondirectory (build_string (sound_file));

  CHECK_STRING (fname);
  if (OpenFile (XSTRING_DATA (fname), &ofs, OF_EXIST) < 0)
    {
      /* file isn't in the path so read it as data */
      int size;
      unsigned char* data;
      int ofd = open (sound_file, O_RDONLY | OPEN_BINARY, 0);
      
      if (ofd <0)
	return;

      size = lseek (ofd, 0, SEEK_END);
      data = (unsigned char *)xmalloc (size);
      lseek (ofd, 0, SEEK_SET);
      
      if (!data)
	{
	  close (ofd);
	  return;
	}

      if (read (ofd, data, size) != size)
	{
	  close (ofd);
	  xfree (data);
	  return;
	}
      close (ofd);
      
      play_sound_data_1 (data, size, 100, FALSE);
    }
  else 
    PlaySound (XSTRING_DATA (fname), NULL, flags);
}

/* mswindows can't cope with playing a sound from alloca space so we
   have to convert if necessary */
static int play_sound_data_1 (unsigned char *data, int length, int volume,
			       int convert_to_malloc)
{
  DWORD flags = SND_ASYNC | SND_MEMORY | SND_NODEFAULT;
  static unsigned char* sound_data=0;
  if (sound_data)
    {
      PlaySound (NULL, NULL, flags);
      xfree (sound_data);
      sound_data=0;
    }

  if (convert_to_malloc)
    {
      sound_data = (unsigned char *)xmalloc (length);
      memcpy (sound_data, data, length);
    }
  else
    sound_data = data;

  PlaySound(sound_data, NULL, flags);

  /* #### Error handling? */ 
  return 1;
}

int play_sound_data (unsigned char *data, int length, int volume)
{
  return play_sound_data_1 (data, length, volume, TRUE);
}
