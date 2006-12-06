/* semaphore.h - declarations for multi-threaded code

   Copyright (C) 2006 Sebastian Freundt

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_semaphore_h_
#define INCLUDED_semaphore_h_

#if defined(HAVE_THREADS)
#include <pthread.h>

/* decouple ordinary pthread mtxes */
#define sxe_thread_t	pthread_t
#define sxe_mutex_t	pthread_mutex_t

#define SXE_MUTEX_INIT(_mtx)	pthread_mutex_init(_mtx, NULL)
#define SXE_MUTEX_FINI(_mtx)	pthread_mutex_destroy(_mtx)
#define SXE_MUTEX_LOCK(_mtx)	pthread_mutex_lock(_mtx)
#define SXE_MUTEX_UNLOCK(_mtx)	pthread_mutex_unlock(_mtx)

#define WITH_SXE_MUTEX(_mtx, args...)	\
	SXE_MUTEX_LOCK(_mtx);		\
	args;				\
	SXE_MUTEX_UNLOCK(_mtx);


#define SXE_MSEMAPH_MAX_CONDITIONS	4

typedef struct sxe_semaphore_s sxe_semaphore_t;
typedef struct sxe_msemaphore_s sxe_msemaphore_t;

struct sxe_semaphore_s {
	pthread_mutex_t mtx;
	pthread_cond_t cnd;
};

struct sxe_msemaphore_s {
	pthread_mutex_t mtx;
	pthread_cond_t cnd[SXE_MSEMAPH_MAX_CONDITIONS];
	int num_cnd;
};

#define SXE_SEMAPH_MUTEX(_sem)	((_sem)->mtx)
#define SXE_SEMAPH_COND(_sem)	((_sem)->cnd)
#define SXE_MSEMAPH_MUTEX(_sem)		((_sem)->mtx)
#define SXE_MSEMAPH_COND(_sem, _idx)	((_sem)->cnd[(_idx)])

#define SXE_SEMAPH_INIT(_sem)					\
do {								\
	pthread_mutex_init(&SXE_SEMAPH_MUTEX(_sem), NULL);	\
	pthread_cond_init(&SXE_SEMAPH_COND(_sem), NULL);	\
} while (0)

#define SXE_MSEMAPH_INIT(_sem)						\
do {									\
	int i;								\
	pthread_mutex_init(&SXE_MSEMAPH_MUTEX(_sem), NULL);		\
	for (i = 0; i < SXE_MSEMAPH_MAX_CONDITIONS; i++)		\
		pthread_cond_init(&SXE_MSEMAPH_COND(_sem, i), NULL);	\
} while (0)

#define SXE_SEMAPH_FINI(_sem)					\
do {								\
	pthread_cond_destroy(&SXE_SEMAPH_COND(_sem));		\
	pthread_mutex_destroy(&SXE_SEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_MSEMAPH_FINI(_sem)						\
do {									\
	int i;								\
	for (i = 0; i < SXE_MSEMAPH_MAX_CONDITIONS; i++)		\
		pthread_cond_destroy(&SXE_MSEMAPH_COND(_sem, i));	\
	pthread_mutex_destroy(&SXE_MSEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_SEMAPH_LOCK(_sem)				\
	pthread_mutex_lock(&SXE_SEMAPH_MUTEX(_sem));

#define SXE_SEMAPH_UNLOCK(_sem)				\
	pthread_mutex_unlock(&SXE_SEMAPH_MUTEX(_sem));

#define SXE_SEMAPH_SIGNAL(_sem)				\
	pthread_cond_signal(&SXE_SEMAPH_COND(_sem));

#define SXE_SEMAPH_BROADCAST(_sem)			\
	pthread_cond_broadcast(&SXE_SEMAPH_COND(_sem));

#define SXE_SEMAPH_WAIT(_sem)			\
	pthread_cond_wait(&SXE_SEMAPH_COND(_sem), &SXE_SEMAPH_MUTEX(_sem));

#define SXE_MSEMAPH_LOCK(_sem)				\
	pthread_mutex_lock(&SXE_MSEMAPH_MUTEX(_sem));

#define SXE_MSEMAPH_UNLOCK(_sem)				\
	pthread_mutex_unlock(&SXE_MSEMAPH_MUTEX(_sem));

#define SXE_MSEMAPH_SIGNAL(_sem, _idx)				\
	pthread_cond_signal(&SXE_MSEMAPH_COND(_sem, _idx));

#define SXE_MSEMAPH_BROADCAST(_sem, _idx)			\
	pthread_cond_broadcast(&SXE_SEMAPH_COND(_sem, _idx));

#define SXE_MSEMAPH_WAIT(_sem, _idx)					\
	pthread_cond_wait(&SXE_MSEMAPH_COND(_sem, _idx),		\
			  &SXE_MSEMAPH_MUTEX(_sem));

#define SXE_SEMAPH_TRIGGER(_sem)				\
do {								\
	pthread_mutex_lock(&SXE_SEMAPH_MUTEX(_sem));		\
	pthread_cond_signal(&SXE_SEMAPH_COND(_sem));		\
	pthread_mutex_unlock(&SXE_SEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_MSEMAPH_TRIGGER(_sem, _idx)				\
do {								\
	pthread_mutex_lock(&SXE_MSEMAPH_MUTEX(_sem));		\
	pthread_cond_signal(&SXE_MSEMAPH_COND(_sem, _idx));	\
	pthread_mutex_unlock(&SXE_MSEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_SEMAPH_TRIGGER_ALL(_sem)				\
do {								\
	pthread_mutex_lock(&SXE_SEMAPH_MUTEX(_sem));		\
	pthread_cond_broadcast(&SXE_SEMAPH_COND(_sem));		\
	pthread_mutex_unlock(&SXE_SEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_MSEMAPH_TRIGGER_ALL(_sem, _idx)			\
do {								\
	pthread_mutex_lock(&SXE_MSEMAPH_MUTEX(_sem));		\
	pthread_cond_broadcast(&SXE_MSEMAPH_COND(_sem, _idx));	\
	pthread_mutex_unlock(&SXE_MSEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_SEMAPH_SYNCH(_sem)					\
do {								\
	pthread_mutex_lock(&SXE_SEMAPH_MUTEX(_sem));		\
	pthread_cond_wait(&SXE_SEMAPH_COND(_sem), &SXE_SEMAPH_MUTEX(_sem)); \
	pthread_mutex_unlock(&SXE_SEMAPH_MUTEX(_sem));		\
} while (0)

#define WITH_SXE_SEMAPH_SYNCH(_sem, args...)			\
do {								\
	pthread_mutex_lock(&SXE_SEMAPH_MUTEX(_sem));		\
	args;							\
	pthread_cond_wait(&SXE_SEMAPH_COND(_sem),		\
			  &SXE_SEMAPH_MUTEX(_sem));		\
	pthread_mutex_unlock(&SXE_SEMAPH_MUTEX(_sem));		\
} while (0)

#define SXE_MSEMAPH_SYNCH(_sem, _idx)				\
do {								\
	pthread_mutex_lock(&SXE_MSEMAPH_MUTEX(_sem));		\
	pthread_cond_wait(&SXE_MSEMAPH_COND(_sem, _idx),	\
			  &SXE_MSEMAPH_MUTEX(_sem));		\
	pthread_mutex_unlock(&SXE_MSEMAPH_MUTEX(_sem));		\
} while (0)

#define WITH_SXE_MSEMAPH_SYNCH(_sem, _idx, args...)		\
do {								\
	pthread_mutex_lock(&SXE_MSEMAPH_MUTEX(_sem));		\
	args;							\
	pthread_cond_wait(&SXE_MSEMAPH_COND(_sem, _idx),	\
			  &SXE_MSEMAPH_MUTEX(_sem));		\
	pthread_mutex_unlock(&SXE_MSEMAPH_MUTEX(_sem));		\
} while (0)

#else  /* !HAVE_THREADS */

typedef void sxe_semaphore_t;
#define sxe_mutex_t	void
#define sxe_thread_t	void

#endif
#endif	/* INCLUDED_semaphore_h */

