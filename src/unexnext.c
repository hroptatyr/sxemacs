/* Dump Emacs in macho format.
   Copyright (C) 1990-1993 Free Software Foundation, Inc.
   Written by Bradley Taylor (btaylor@next.com).

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

/* Synched up with:  NeXT port */

#include <stdio.h>
#include <libc.h>
#include <nlist.h>
#include <mach/mach.h>
#include <mach-o/ldsyms.h>
#include <mach-o/loader.h>

int malloc_cookie;

static void fatal_unexec(char *format, ...)
   {
   va_list ap;
   
   va_start(ap, format);
   fprintf(stderr, "unexec: ");
   vfprintf(stderr, format, ap);
   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
   }

static void mcopy(int ffd,int tfd,
                  unsigned long fpos,unsigned long tpos,unsigned long len)
   {
   if ((ffd==-1)&&(tfd==-1))
      {
      char *f,*t,*e;
      if (fpos>tpos)
         {
         f=(char *)fpos;
         t=(char *)tpos;
         e=(char *)(fpos+len);
         while(f<e) *t++=*f++;
         }
      else if (tpos>fpos)
         {
         f=(char *)(fpos+len);
         t=(char *)(tpos+len);
         e=(char *)fpos;
         while(f>e) *--t=*--f;         
         }   
      }
   else if (ffd==-1)
      {
      if (lseek(tfd,tpos,L_SET)<0)
         fatal_unexec("cannot seek target");
      if (write(tfd,(void *)fpos,len)!=len)
         fatal_unexec("cannot write target");
      }
   else if (tfd==-1)
      {
      if (lseek(ffd,fpos,L_SET)<0)
         fatal_unexec("cannot seek source");
      if (read(ffd,(void *)tpos,len)!=len)
         fatal_unexec("cannot read source");
      }
   else
      {
      int bread;
      char *buf=alloca(1<<16);
      
      if (lseek(ffd,fpos,L_SET)<0)
         fatal_unexec("cannot seek source");
      
      if (lseek(tfd,tpos,L_SET)<0)
         fatal_unexec("cannot seek target");
      
      while((len>0) && (bread=read(ffd,buf,MIN(1<<16,len)))>0)
         {
         if (bread<0)
            fatal_unexec("cannot read source");
         if (write(tfd,buf,bread)!=bread)
            fatal_unexec("cannot write target");
         len-=bread;
         }
      }
   }

static void unexec_doit(int infd,int outfd)
   {
   int i,j,hpos,opos;
   extern int malloc_freezedry(void);
   struct region
      {
      struct region *next;
      unsigned long addr;
      unsigned long size;
      vm_prot_t prot;
      vm_prot_t mprot;
      } *regions=0,*cregion,**pregions;
   struct mach_header mh;
   struct segment_command *lc,*sp;
   struct symtab_command *st;
   struct section *sect;

   malloc_cookie=malloc_freezedry();
   
      {
      vm_task_t task=task_self();
      vm_address_t addr;
      vm_size_t size;
      vm_prot_t prot,mprot;
      vm_inherit_t inhe;
      boolean_t shrd;
      port_t name;
      vm_offset_t offset;
      
      for(addr=VM_MIN_ADDRESS,pregions=&regions;
          vm_region(task,&addr,&size,&prot,&mprot,
                    &inhe,&shrd,&name,&offset)==KERN_SUCCESS;
          addr += size)
         {
         (*pregions)=alloca(sizeof(struct region));
         (*pregions)->addr=addr;
         (*pregions)->size=size;
         (*pregions)->prot=prot;
         (*pregions)->mprot=mprot;
         (*pregions)->next=0;
         pregions=&((*pregions)->next);
         }
      }
   
   for(cregion=regions;cregion;cregion=cregion->next)
      while ((cregion->next) &&
             (cregion->next->addr==cregion->addr+cregion->size) &&
             (cregion->next->prot==cregion->prot) &&
             (cregion->next->mprot==cregion->mprot))
         {
         cregion->size += cregion->next->size;
         cregion->next = cregion->next->next;
         }

   mcopy(infd,-1,0,(unsigned long) &mh,sizeof(mh));
   lc=alloca(mh.sizeofcmds);
   mcopy(infd,-1,sizeof(mh),(unsigned long) lc,mh.sizeofcmds);
   
   for(pregions=&regions;*pregions;)
      {
      if (!((*pregions)->prot&VM_PROT_WRITE)
          || ((*pregions)->addr>=0x3000000))
         goto kill_region;
      
      for(sp=lc,i=0;
          i<mh.ncmds;
          i++,sp=(struct segment_command *)(((char *)sp)+sp->cmdsize))
         {
         unsigned long ob,oe;
         if (sp->cmd!=LC_SEGMENT||(strcmp(sp->segname,SEG_DATA)==0)) continue;
         ob=MAX((*pregions)->addr,sp->vmaddr);
         oe=MIN((*pregions)->addr+(*pregions)->size,sp->vmaddr+sp->vmsize);
         if (ob >= oe) continue;
         if (ob==(*pregions)->addr)
            if (oe==(*pregions)->addr+(*pregions)->size)
               {
               goto kill_region;
               }
            else
               {
               (*pregions)->addr=oe;
               (*pregions)->size-=(oe-ob);
               }
         else
            if (oe==(*pregions)->addr+(*pregions)->size)
               {
               (*pregions)->size-=(oe-ob);
               }
            else
               {
               cregion=alloca(sizeof(*cregion));
               cregion->addr=oe;
               cregion->size=((*pregions)->addr+(*pregions)->size)-oe;
               cregion->prot=(*pregions)->prot;
               cregion->mprot=(*pregions)->mprot;
               cregion->next=(*pregions)->next;
               (*pregions)->size=ob-(*pregions)->addr;
               (*pregions)->next=cregion;
               }
         }
      pregions=&((*pregions)->next);
      continue;
    kill_region:
      *pregions=(*pregions)->next;
      }

   for(sp=lc,i=mh.ncmds,hpos=sizeof(mh),opos=0;
       i>0;
       i--,sp=(struct segment_command *)(((char *)sp)+sp->cmdsize))
      switch (sp->cmd)
         {
       case LC_SEGMENT:
         if (strcmp(sp->segname,SEG_DATA)==0)
            {
            mh.ncmds--;
            j=sp->cmdsize;
            while (regions)
               {
               mcopy(-1,outfd,regions->addr,opos,regions->size);
               sp->cmd=LC_SEGMENT;
               sp->cmdsize=sizeof(*sp);
               strncpy(sp->segname,SEG_DATA,sizeof(sp->segname));
               sp->vmaddr=regions->addr;
               sp->vmsize=regions->size;
               sp->filesize=regions->size;
               sp->maxprot=regions->prot;
               sp->initprot=regions->mprot;
               sp->nsects=0;
               sp->flags=0;
               sp->fileoff=opos;
               opos+=sp->filesize;
               mcopy(-1,outfd,(unsigned long)sp,hpos,sp->cmdsize);
               hpos+=sp->cmdsize;
               mh.ncmds++;
               regions=regions->next;
               }
            sp->cmdsize=j;
            regions=0;
            }
         else if (strcmp(sp->segname,SEG_LINKEDIT)==0)
            {
            mh.ncmds--;
            }
         else
            {
            mcopy(infd,outfd,sp->fileoff,opos,sp->filesize);
            sect=(struct section *) (((char *)sp)+sizeof(*sp));
            for(j=0;j<sp->nsects;j++)
               {
               if (sect[j].offset!=0)
                  sect[j].offset=(sect[j].offset-sp->fileoff)+opos;
               if (sect[j].reloff!=0)
                  sect[j].reloff=(sect[j].reloff-sp->fileoff)+opos;
               }
            sp->fileoff=opos;
            opos+=sp->filesize;
            mcopy(-1,outfd,(unsigned long)sp,hpos,sp->cmdsize);
            hpos+=sp->cmdsize;
            }
	 break;
       case LC_SYMTAB:
         st=(struct symtab_command *)sp;
         
         mcopy(infd,outfd,st->symoff,opos,st->nsyms*sizeof(struct nlist));
         st->symoff=opos;
         opos+=sizeof(struct nlist)*st->nsyms;
         
         mcopy(infd,outfd,st->stroff,opos,st->strsize);
         ((struct symtab_command *)sp)->stroff=opos;
         opos+=((struct symtab_command *)sp)->strsize;
       default:
         mcopy(-1,outfd,(unsigned long)sp,hpos,sp->cmdsize);
         hpos+=sp->cmdsize;
         }
   mh.sizeofcmds=hpos-sizeof(mh);
   mcopy(-1,outfd,(unsigned long) &mh,0,sizeof(mh));
   }

void unexec(char *outfile,char *infile)
   {
   char tmpfile[MAXPATHLEN];
   int infd,outfd;
   
   if ((infd=open(infile, O_RDONLY, 0))<0)
      fatal_unexec("cannot open input file `%s'", infile);

   strcpy(tmpfile,outfile);
   strcat(tmpfile,"-temp");
   
   if ((outfd=open(tmpfile, O_RDWR|O_TRUNC|O_CREAT, 0755))<0)
      fatal_unexec("cannot open temporary output file `%s'",tmpfile);

   unexec_doit(infd,outfd);

   close(infd);
   close(outfd);
   if (rename(tmpfile, outfile)<0)
      {
      unlink(tmpfile);
      fatal_unexec("cannot rename `%s' to `%s'", tmpfile, outfile);
      }  
   }
