/* unexec for GNU Emacs on Windows NT.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
02111-1307, USA.

   Geoff Voelker (voelker@cs.washington.edu) 8-12-94 */

/* Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au> */

/* The linkers that come with MSVC >= 4.0 merge .bss into .data and reorder
 * uninitialised data so that the .data section looks like:
 *
 *	crt0 initialised data
 *	emacs initialised data
 *		<my_edata>
 *	library initialised data
 *		<start of bss part of .data>
 *	emacs static uninitialised data
 *	library static uninitialised data
 *	emacs global uninitialised data
 *		<my_ebss>
 *	library global uninitialised data
 *
 * This means that we can't use the normal my_ebss in lastfile.c trick to
 * differentiate between unitialised data that belongs to emacs and
 * uninitialised data that belongs to system libraries. This is bad because
 * we do want to initialise the emacs data, but we don't want to initialise
 * the system library data.
 *
 * To solve this problem using MSVC >= 5.0 we use a pragma directive to tell
 * the compiler to put emacs's data (both initialised and uninitialised) in
 * a separate section in the executable, and we only dump that section. This
 * means that all files that define initialized data must include config.h
 * to pick up the pragma. We don't try to make any part of that section
 * read-only.
 *
 * This pragma directive isn't supported by the MSVC 4.x compiler. Instead,
 * we dump crt0 initialised data and library static uninitialised data in
 * addition to the emacs data. This is wrong, but we appear to be able to
 * get away with it. A proper fix might involve the introduction of a static
 * version of my_ebss in lastfile.c and a new firstfile.c file.  jhar */

#include <config.h>
#include "lisp.h"

#include "syswindows.h"

#include "nt.h"
#include "ntheap.h"

/* From IMAGEHLP.H which is not installed by default by MSVC < 5 */
/* The IMAGEHLP.DLL library is not distributed by default with Windows95 */
typedef PIMAGE_NT_HEADERS
(__stdcall * pfnCheckSumMappedFile_t) (LPVOID BaseAddress, DWORD FileLength,
				       LPDWORD HeaderSum, LPDWORD CheckSum);


#if 0
extern BOOL ctrl_c_handler (unsigned long type);
#endif

/* Sync with FSF Emacs 19.34.6
   note: struct file_data is now defined in nt.h */

enum {
  HEAP_UNINITIALIZED = 1,
  HEAP_UNLOADED,
  HEAP_LOADED
};

/* Basically, our "initialized" flag.  */
int heap_state = HEAP_UNINITIALIZED;

/* So we can find our heap in the file to recreate it.  */
unsigned long heap_index_in_executable = UNINIT_LONG;

void get_section_info (file_data *p_file);
void copy_executable_and_dump_data_section (file_data *, file_data *);
void dump_bss_and_heap (file_data *p_infile, file_data *p_outfile);

/* Cached info about the .data section in the executable.  */
PUCHAR data_start_va = UNINIT_PTR;
DWORD  data_start_file = UNINIT_LONG;
DWORD  data_size = UNINIT_LONG;

/* Cached info about the .bss section in the executable.  */
PUCHAR bss_start = UNINIT_PTR;
DWORD  bss_size = UNINIT_LONG;

/* Startup code for running on NT.  When we are running as the dumped
   version, we need to bootstrap our heap and .bss section into our
   address space before we can actually hand off control to the startup
   code supplied by NT (primarily because that code relies upon malloc ()).  */

/* **********************
   Hackers please remember, this _start() thingy is *not* called neither
   when dumping portably, nor when running from temacs! Do not put
   significant XEmacs initialization here!
   ********************** */

void
_start (void)
{
  extern void mainCRTStartup (void);

  /* Cache system info, e.g., the NT page size.  */
  cache_system_info ();

  /* If we're a dumped version of emacs then we need to recreate
     our heap and play tricks with our .bss section.  Do this before
     start up.  (WARNING:  Do not put any code before this section
     that relies upon malloc () and runs in the dumped version.  It
     won't work.)  */
  if (heap_state == HEAP_UNLOADED) 
    {
      char executable_path[MAX_PATH];

      if (GetModuleFileName (NULL, executable_path, MAX_PATH) == 0) 
	{
	  exit (1);
	}

      /* #### This is super-bogus. When I rename xemacs.exe,
	 the renamed file still loads its heap from xemacs.exe --kkm */
#if 0
      {
	/* To allow profiling, make sure executable_path names the .exe
	   file, not the file created by the profiler */
	char *p = strrchr (executable_path, '\\');
	strcpy (p+1, PATH_PROGNAME ".exe");
      }
#endif

      recreate_heap (executable_path);
      heap_state = HEAP_LOADED;
    }

  /* #### This is bogus, too. _fmode is set to different values
     when we run `xemacs' and `temacs run-emacs'. The sooner we
     hit and fix all the weirdities this causes us, the better --kkm */
#if 0
  /* The default behavior is to treat files as binary and patch up
     text files appropriately.  */
  _fmode = O_BINARY;
#endif

#if 0
  /* This prevents ctrl-c's in shells running while we're suspended from
     having us exit.  */
  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) ctrl_c_handler, TRUE);
#endif

  mainCRTStartup ();
}

/* Dump out .data and .bss sections into a new executable.  */
int
unexec (char *new_name, char *old_name, unsigned int start_data,
	unsigned int start_bss, unsigned int entry_address)
{
  file_data in_file, out_file;
  char out_filename[MAX_PATH], in_filename[MAX_PATH];
  unsigned long size;
  char *ptr;
  HINSTANCE hImagehelp;
  
  /* Make sure that the input and output filenames have the
     ".exe" extension...patch them up if they don't.  */
  strcpy (in_filename, old_name);
  ptr = in_filename + strlen (in_filename) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (in_filename, ".exe");

  strcpy (out_filename, new_name);
  ptr = out_filename + strlen (out_filename) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (out_filename, ".exe");

  printf ("Dumping from %s\n", in_filename);
  printf ("          to %s\n", out_filename);

  /* We need to round off our heap to NT's allocation unit (64KB).  */
  round_heap (get_allocation_unit ());

  /* Open the undumped executable file.  */
  if (!open_input_file (&in_file, in_filename))
    {
      printf ("Failed to open %s (%d)...bailing.\n", 
	      in_filename, GetLastError ());
      exit (1);
    }

  /* Get the interesting section info, like start and size of .bss...  */
  get_section_info (&in_file);

  /* The size of the dumped executable is the size of the original
     executable plus the size of the heap and the size of the .bss section.  */
  heap_index_in_executable = (unsigned long)
    round_to_next ((unsigned char *) in_file.size, get_allocation_unit ());
  size = heap_index_in_executable + get_committed_heap_size () + bss_size;
  if (!open_output_file (&out_file, out_filename, size))
    {
      printf ("Failed to open %s (%d)...bailing.\n", 
	      out_filename, GetLastError ());
      exit (1);
    }

  /* Set the flag (before dumping).  */
  heap_state = HEAP_UNLOADED;

  copy_executable_and_dump_data_section (&in_file, &out_file);
  dump_bss_and_heap (&in_file, &out_file);

  /* Patch up header fields; profiler is picky about this. */
  hImagehelp = LoadLibrary ("imagehlp.dll");
  if (hImagehelp)
  {
    PIMAGE_DOS_HEADER dos_header;
    PIMAGE_NT_HEADERS nt_header;

    DWORD  headersum;
    DWORD  checksum;
    pfnCheckSumMappedFile_t pfnCheckSumMappedFile;

    dos_header = (PIMAGE_DOS_HEADER) out_file.file_base;
    nt_header = (PIMAGE_NT_HEADERS) ((char *) dos_header + dos_header->e_lfanew);

    nt_header->OptionalHeader.CheckSum = 0;
#if 0
    nt_header->FileHeader.TimeDateStamp = time (NULL);
    dos_header->e_cp = size / 512;
    nt_header->OptionalHeader.SizeOfImage = size;
#endif

    pfnCheckSumMappedFile =
      (pfnCheckSumMappedFile_t) GetProcAddress (hImagehelp,
						"CheckSumMappedFile");
    if (pfnCheckSumMappedFile)
      {
#if 0
	nt_header->FileHeader.TimeDateStamp = time (NULL);
#endif
	pfnCheckSumMappedFile (out_file.file_base,
			       out_file.size,
			       &headersum,
			       &checksum);
	nt_header->OptionalHeader.CheckSum = checksum;
      }
    FreeLibrary (hImagehelp);
  }

  close_file_data (&in_file);
  close_file_data (&out_file);

  return 0;
}

/* Routines to manipulate NT executable file sections.  */

#ifndef DUMP_SEPARATE_SECTION
static void
get_bss_info_from_map_file (file_data *p_infile, PUCHAR *p_bss_start, 
			    DWORD *p_bss_size)
{
  int n, start, len;
  char map_filename[MAX_PATH];
  char buffer[256];
  FILE *map;

  /* Overwrite the .exe extension on the executable file name with
     the .map extension.  */
  strcpy (map_filename, p_infile->name);
  n = strlen (map_filename) - 3;
  strcpy (&map_filename[n], "map");

  map = fopen (map_filename, "r");
  if (!map)
    {
      printf ("Failed to open map file %s, error %d...bailing out.\n",
	      map_filename, GetLastError ());
      exit (-1);
    }

  while (fgets (buffer, sizeof (buffer), map))
    {
      if (!(strstr (buffer, ".bss") && strstr (buffer, "DATA")))
	continue;
      n = sscanf (buffer, " %*d:%x %x", &start, &len);
      if (n != 2)
	{
	  printf ("Failed to scan the .bss section line:\n%s", buffer);
	  exit (-1);
	}
      break;
    }
  *p_bss_start = (PUCHAR) start;
  *p_bss_size = (DWORD) len;
}
#endif

/* Flip through the executable and cache the info necessary for dumping.  */
static void
get_section_info (file_data *p_infile)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  PIMAGE_SECTION_HEADER section, data_section;
  unsigned char *ptr;
  int i;
  
  dos_header = (PIMAGE_DOS_HEADER) p_infile->file_base;
  if (dos_header->e_magic != IMAGE_DOS_SIGNATURE) 
    {
      printf ("Unknown EXE header in %s...bailing.\n", p_infile->name);
      exit (1);
    }
  nt_header = (PIMAGE_NT_HEADERS) (((unsigned long) dos_header) + 
				   dos_header->e_lfanew);
  if (nt_header == NULL) 
    {
      printf ("Failed to find IMAGE_NT_HEADER in %s...bailing.\n", 
	     p_infile->name);
      exit (1);
    }

  /* Check the NT header signature ...  */
  if (nt_header->Signature != IMAGE_NT_SIGNATURE) 
    {
      printf ("Invalid IMAGE_NT_SIGNATURE 0x%x in %s...bailing.\n",
	      nt_header->Signature, p_infile->name);
    }

  /* Flip through the sections for .data and .bss ...  */
  section = (PIMAGE_SECTION_HEADER) IMAGE_FIRST_SECTION (nt_header);
  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++) 
    {
#ifndef DUMP_SEPARATE_SECTION
      if (!strcmp (section->Name, ".bss")) 
	{
	  extern int my_ebss;		/* From lastfile.c  */

	  ptr = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  bss_start = ptr;
	  bss_size = (char*)&my_ebss - (char*)bss_start;
	}

      if (!strcmp (section->Name, ".data")) 
#else
      if (!strcmp (section->Name, "xdata"))
#endif
	{
	  extern char my_edata[];	/* From lastfile.c  */

	  /* The .data section.  */
	  data_section = section;
	  ptr = (char *) nt_header->OptionalHeader.ImageBase +
	    section->VirtualAddress;
	  data_start_va = ptr;
	  data_start_file = section->PointerToRawData;

#ifndef DUMP_SEPARATE_SECTION
	  /* Write only the part of the section that contains emacs data. */
	  data_size = my_edata - data_start_va;
#else
	  /* Write back the full section.  */
	  data_size = section->SizeOfRawData;

	  /* This code doesn't know how to grow the raw size of a section. */
	  if (section->SizeOfRawData < section->Misc.VirtualSize)
	    {
	      printf ("The emacs data section is smaller than expected"
		      "...bailing.\n");
	      exit (1);
	    }
#endif
	}
      section++;
    }

#ifndef DUMP_SEPARATE_SECTION
  if (bss_start == UNINIT_PTR)
    {
      /* Starting with MSVC 4.0, the .bss section has been eliminated
	 and appended virtually to the end of the .data section.  Our
	 only hint about where the .bss section starts in the address
	 comes from the SizeOfRawData field in the .data section
	 header.  Unfortunately, this field is only approximate, as it
	 is a rounded number and is typically rounded just beyond the
	 start of the .bss section.  To find the start and size of the
	 .bss section exactly, we have to peek into the map file.  */
      extern int my_ebss;

      get_bss_info_from_map_file (p_infile, &ptr, &bss_size);
      bss_start = ptr + nt_header->OptionalHeader.ImageBase
	+ data_section->VirtualAddress;
      bss_size = (char*)&my_ebss - (char*)bss_start;
    }
#else
  bss_size = 0;
#endif
}


/* The dump routines.  */

#ifdef DEBUG_XEMACS
#define DUMP_MSG(x) printf x
#else
#define DUMP_MSG(x)
#endif

static void
copy_executable_and_dump_data_section (file_data *p_infile,
				       file_data *p_outfile)
{
  unsigned char *data_file, *data_va;
  unsigned long size, index;

  /* Get a pointer to where the raw data should go in the executable file.  */
  data_file = (char *) p_outfile->file_base + data_start_file;

  /* Get a pointer to the raw data in our address space.  */
  data_va = data_start_va;

  size = (DWORD) data_file - (DWORD) p_outfile->file_base;
  DUMP_MSG (("Copying executable up to data section...\n"));
  DUMP_MSG (("\t0x%08x Offset in input file.\n", 0));
  DUMP_MSG (("\t0x%08x Offset in output file.\n", 0));
  DUMP_MSG (("\t0x%08x Size in bytes.\n", size));
  memcpy (p_outfile->file_base, p_infile->file_base, size);

  size = data_size;
  DUMP_MSG (("Dumping data section...\n"));
  DUMP_MSG (("\t0x%08x Address in process.\n", data_va));
  DUMP_MSG (("\t0x%08x Offset in output file.\n", 
	     (char*)data_file - p_outfile->file_base));
  DUMP_MSG (("\t0x%08x Size in bytes.\n", size));
  memcpy (data_file, data_va, size);

  index = (DWORD) data_file + size - (DWORD) p_outfile->file_base;
  size = p_infile->size - index;
  DUMP_MSG (("Copying rest of executable...\n"));
  DUMP_MSG (("\t0x%08x Offset in input file.\n", index));
  DUMP_MSG (("\t0x%08x Offset in output file.\n", index));
  DUMP_MSG (("\t0x%08x Size in bytes.\n", size));
  memcpy ((char *) p_outfile->file_base + index, 
	  (char *) p_infile->file_base + index, size);
}

static void
dump_bss_and_heap (file_data *p_infile, file_data *p_outfile)
{
    unsigned char *heap_data;
    unsigned long size, index;

    DUMP_MSG (("Dumping heap onto end of executable...\n"));

    index = heap_index_in_executable;
    size = get_committed_heap_size ();
    heap_data = get_heap_start ();

    DUMP_MSG (("\t0x%08x Heap start in process.\n", heap_data));
    DUMP_MSG (("\t0x%08x Heap offset in executable.\n", index));
    DUMP_MSG (("\t0x%08x Heap size in bytes.\n", size));

    memcpy ((PUCHAR) p_outfile->file_base + index, heap_data, size);

#ifndef DUMP_SEPARATE_SECTION
    DUMP_MSG (("Dumping bss onto end of executable...\n"));
    
    index += size;
    size = bss_size;

    DUMP_MSG (("\t0x%08x BSS start in process.\n", bss_start));
    DUMP_MSG (("\t0x%08x BSS offset in executable.\n", index));
    DUMP_MSG (("\t0x%08x BSS size in bytes.\n", size));
    memcpy ((char *) p_outfile->file_base + index, bss_start, size);
#endif
}

#undef DUMP_MSG

/* Reload and remap routines.  */


/* Load the dumped .bss section into the .bss area of our address space.  */
/* Already done if the .bss  was part of a separate emacs data section */
void
read_in_bss (char *filename)
{
#ifndef DUMP_SEPARATE_SECTION
  HANDLE file;
  unsigned long index, n_read;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE)
    abort ();
  
  /* Seek to where the .bss section is tucked away after the heap...  */
  index = heap_index_in_executable + get_committed_heap_size ();
  if (SetFilePointer (file, index, NULL, FILE_BEGIN) == 0xFFFFFFFF) 
    abort ();

  /* Ok, read in the saved .bss section and initialize all 
     uninitialized variables.  */
  if (!ReadFile (file, bss_start, bss_size, &n_read, NULL))
    abort ();

  CloseHandle (file);
#endif
}

/* Map the heap dumped into the executable file into our address space.  */
void 
map_in_heap (char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size, n_read;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    abort ();

  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_WRITECOPY, 
				    0, size, NULL);
  if (!file_mapping) 
    abort ();

  size = get_committed_heap_size ();
  file_base = MapViewOfFileEx (file_mapping, FILE_MAP_COPY, 0, 
			       heap_index_in_executable, size,
			       get_heap_start ());
  if (file_base != 0) 
    {
      return;
    }

  /* If we don't succeed with the mapping, then copy from the 
     data into the heap.  */

  CloseHandle (file_mapping);

  if (VirtualAlloc (get_heap_start (), get_committed_heap_size (),
		    MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE) == NULL)
    abort ();

  /* Seek to the location of the heap data in the executable.  */
  if (SetFilePointer (file, heap_index_in_executable,
		      NULL, FILE_BEGIN) == 0xFFFFFFFF)
    abort ();

  /* Read in the data.  */
  if (!ReadFile (file, get_heap_start (), 
		 get_committed_heap_size (), &n_read, NULL))
    abort ();

  CloseHandle (file);
}
