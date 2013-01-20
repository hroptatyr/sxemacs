/* semaphore.h - declarations for multi-threaded code

   Copyright (C) 2006 Sebastian Freundt

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


/* Synched up with: Not in FSF. */

#ifndef INCLUDED_semaphore_h_
#define INCLUDED_semaphore_h_

#if defined(HAVE_THREADS)
#include <pthread.h>

/* decouple ordinary pthread mtxes */
#define sxe_thread_t	pthread_t
#define sxe_mutex_t	pthread_mutex_t

typedef void*(*sxe_thread_f)(void*);

#define SXE_MUTEX_INIT(_mtx)	pthread_mutex_init(_mtx, NULL)
#define SXE_MUTEX_FINI(_mtx)	pthread_mutex_destroy(_mtx)
#define SXE_MUTEX_LOCK(_mtx)	pthread_mutex_lock(_mtx)
#define SXE_MUTEX_UNLOCK(_mtx)	pthread_mutex_unlock(_mtx)

#define WITH_SXE_MUTEX(_mtx, args...)	\
	SXE_MUTEX_LOCK(_mtx);		\
	args;				\
	SXE_MUTEX_UNLOCK(_mtx);


#define SXE_MSEMAPH_MAX_CONDITIONS	4

typedef struct sxe_semaphore_s *sxe_semaphore_t;
typedef struct sxe_msemaphore_s *sxe_msemaphore_t;
typedef struct sxe_refcounter_s *sxe_refcounter_t;

struct sxe_semaphore_s {
	pthread_mutex_t mtx;
	pthread_cond_t  cnd;
        int generation;
};

struct sxe_msemaphore_s {
	pthread_mutex_t mtx;
	pthread_cond_t cnd[SXE_MSEMAPH_MAX_CONDITIONS];
	int num_cnd;
        int generation;
};

/* a thread-safe reference counter */
struct sxe_refcounter_s {
	pthread_mutex_t mtx;
	int refcnt;
};

#define SXE_SEMAPH_MUTEX(_sem)		((_sem)->mtx)
#define SXE_SEMAPH_COND(_sem)		((_sem)->cnd)
#define SXE_MSEMAPH_MUTEX(_sem)		((_sem)->mtx)
#define SXE_MSEMAPH_COND(_sem, _idx)	((_sem)->cnd[(_idx)])

extern_inline void sxe_semaphore_init(sxe_semaphore_t);
extern_inline void sxe_msemaphore_init(sxe_msemaphore_t);
extern_inline void sxe_semaphore_finish(sxe_semaphore_t);
extern_inline void sxe_msemaphore_finish(sxe_msemaphore_t);
extern_inline void sxe_semaphore_trigger(sxe_semaphore_t);
extern_inline void sxe_msemaphore_trigger(sxe_msemaphore_t, int);
extern_inline void sxe_semaphore_trigger_all(sxe_semaphore_t);
extern_inline void sxe_msemaphore_trigger_all(sxe_msemaphore_t, int);
extern_inline void sxe_semaphore_synchronise(sxe_semaphore_t);
extern_inline void sxe_msemaphore_synchronise(sxe_msemaphore_t, int);

/* thread-safe refcounter */
extern_inline void sxe_refcounter_init(sxe_refcounter_t);
extern_inline void sxe_refcounter_finish(sxe_refcounter_t);
extern_inline sxe_refcounter_t sxe_refcounter_new(void);
extern_inline void sxe_refcounter_free(sxe_refcounter_t);
extern_inline int sxe_refcounter_value(sxe_refcounter_t);
extern_inline int sxe_refcounter_incref(sxe_refcounter_t);
extern_inline int sxe_refcounter_decref(sxe_refcounter_t);


extern_inline void
sxe_semaphore_init(sxe_semaphore_t sem)
{
        sem->generation = 0;
	pthread_mutex_init(&(sem->mtx), NULL);
	pthread_cond_init(&(sem->cnd), NULL);
}
extern_inline void
sxe_msemaphore_init(sxe_msemaphore_t sem)
{
	int i;
        sem->generation = 0;
	pthread_mutex_init(&(sem->mtx), NULL);
	for (i = 0; i < SXE_MSEMAPH_MAX_CONDITIONS; i++)
		pthread_cond_init(&(sem->cnd[i]), NULL);
}
extern_inline void
sxe_semaphore_finish(sxe_semaphore_t sem)
{
	pthread_mutex_lock(&(sem->mtx));
	pthread_mutex_unlock(&(sem->mtx));
	pthread_cond_destroy(&(sem->cnd));
	pthread_mutex_destroy(&(sem->mtx));
}
extern_inline void
sxe_msemaphore_finish(sxe_msemaphore_t sem)
{
	int i;
	pthread_mutex_lock(&(sem->mtx));
	pthread_mutex_unlock(&(sem->mtx));
	for (i = 0; i < SXE_MSEMAPH_MAX_CONDITIONS; i++)
		pthread_cond_destroy(&(sem->cnd[i]));
	pthread_mutex_destroy(&(sem->mtx));
}

extern_inline void
sxe_semaphore_trigger(sxe_semaphore_t sem)
{
	pthread_mutex_lock(&(sem->mtx));
        sem->generation++;
	pthread_cond_signal(&(sem->cnd));
	pthread_mutex_unlock(&(sem->mtx));
}
extern_inline void
sxe_msemaphore_trigger(sxe_msemaphore_t sem, int idx)
{
	pthread_mutex_lock(&(sem->mtx));
        sem->generation++;
	pthread_cond_signal(&(sem->cnd[idx]));
	pthread_mutex_unlock(&(sem->mtx));
}

extern_inline void
sxe_semaphore_trigger_all(sxe_semaphore_t sem)
{
	pthread_mutex_lock(&(sem->mtx));
	sem->generation++;
	pthread_cond_broadcast(&(sem->cnd));
	pthread_mutex_unlock(&(sem->mtx));
}
extern_inline void
sxe_msemaphore_trigger_all(sxe_msemaphore_t sem, int idx)
{
	pthread_mutex_lock(&(sem->mtx));
        sem->generation++;
	pthread_cond_broadcast(&(sem->cnd[idx]));
	pthread_mutex_unlock(&(sem->mtx));
}

extern_inline void
sxe_semaphore_synchronise(sxe_semaphore_t sem)
{
        int generation;

	pthread_mutex_lock(&(sem->mtx));
        generation = sem->generation;
        do {
		pthread_cond_wait(&(sem->cnd), &(sem->mtx));
	} while(sem->generation == generation);
	pthread_mutex_unlock(&(sem->mtx));
}
extern_inline void
sxe_msemaphore_synchronise(sxe_msemaphore_t sem, int idx)
{
        int generation;

	pthread_mutex_lock(&(sem->mtx));
        generation = sem->generation;
        do {
		pthread_cond_wait(&(sem->cnd[idx]), &(sem->mtx));
	} while(sem->generation == generation);
	pthread_mutex_unlock(&(sem->mtx));
}

#define SXE_SEMAPH_INIT(_sem)	sxe_semaphore_init(_sem)
#define SXE_MSEMAPH_INIT(_sem)	sxe_msemaphore_init(_sem)
#define SXE_SEMAPH_FINI(_sem)	sxe_semaphore_finish(_sem)
#define SXE_MSEMAPH_FINI(_sem)	sxe_msemaphore_finish(_sem)

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

#define SXE_SEMAPH_TRIGGER(_sem)	sxe_semaphore_trigger(_sem)
#define SXE_MSEMAPH_TRIGGER(_sem, _idx)	sxe_msemaphore_trigger(_sem, _idx)
#define SXE_SEMAPH_TRIGGER_ALL(_sem)	sxe_semaphore_trigger_all(_sem)
#define SXE_MSEMAPH_TRIGGER_ALL(_sem, _idx)	\
	sxe_msemaphore_trigger_all(_sem, _idx)
#define SXE_SEMAPH_SYNCH(_sem)		sxe_semaphore_synchronise(_sem);
#define SXE_MSEMAPH_SYNCH(_sem, _idx)	sxe_msemaphore_synchronise(_sem, _idx);

#define WITH_SXE_SEMAPH_SYNCH(_sem, args...)			\
do {								\
	pthread_mutex_lock(&SXE_SEMAPH_MUTEX(_sem));		\
	args;							\
	pthread_cond_wait(&SXE_SEMAPH_COND(_sem),		\
			  &SXE_SEMAPH_MUTEX(_sem));		\
	pthread_mutex_unlock(&SXE_SEMAPH_MUTEX(_sem));		\
} while (0)

#define WITH_SXE_MSEMAPH_SYNCH(_sem, _idx, args...)		\
do {								\
	pthread_mutex_lock(&SXE_MSEMAPH_MUTEX(_sem));		\
	args;							\
	pthread_cond_wait(&SXE_MSEMAPH_COND(_sem, _idx),	\
			  &SXE_MSEMAPH_MUTEX(_sem));		\
	pthread_mutex_unlock(&SXE_MSEMAPH_MUTEX(_sem));		\
} while (0)


/* thread-safe refcounter */
extern_inline void
sxe_refcounter_init(sxe_refcounter_t rc)
{
	pthread_mutex_init(&(rc->mtx), NULL);
	pthread_mutex_lock(&(rc->mtx));
	rc->refcnt = 0;
	pthread_mutex_unlock(&(rc->mtx));
}
extern_inline void
sxe_refcounter_finish(sxe_refcounter_t rc)
{
	pthread_mutex_lock(&(rc->mtx));
	rc->refcnt = 0;
	pthread_mutex_unlock(&(rc->mtx));
	pthread_mutex_destroy(&(rc->mtx));
}
extern_inline sxe_refcounter_t
sxe_refcounter_new(void)
{
	sxe_refcounter_t rc = xnew(struct sxe_refcounter_s);
	sxe_refcounter_init(rc);
	return rc;
}
extern_inline void
sxe_refcounter_free(sxe_refcounter_t rc)
{
	sxe_refcounter_finish(rc);
	xfree(rc);
	return;
}
/* accessors */
extern_inline int
sxe_refcounter_value(sxe_refcounter_t rc)
{
	int result;

	pthread_mutex_lock(&(rc->mtx));
	result = rc->refcnt;
	pthread_mutex_unlock(&(rc->mtx));

	return result;
}
extern_inline int
sxe_refcounter_incref(sxe_refcounter_t rc)
{
	int result;

	pthread_mutex_lock(&(rc->mtx));
	result = ++rc->refcnt;
	pthread_mutex_unlock(&(rc->mtx));

	return result;
}
extern_inline int
sxe_refcounter_decref(sxe_refcounter_t rc)
{
	int result;

	pthread_mutex_lock(&(rc->mtx));
	result = --rc->refcnt;
	pthread_mutex_unlock(&(rc->mtx));

	return result;
}

/* pthreads */
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
# define xthread_create		GC_pthread_create
# define xthread_join		GC_pthread_join
# define xthread_detach		GC_pthread_detach
#else  /* !BDWGC */
# define xthread_create		pthread_create
# define xthread_join		pthread_join
# define xthread_detach		pthread_detach
#endif	/* BDWGC */

#else  /* !HAVE_THREADS */

typedef void *sxe_semaphore_t;
#define sxe_mutex_t	void
#define sxe_thread_t	void

#define SXE_MUTEX_INIT(_mtx)
#define SXE_MUTEX_FINI(_mtx)
#define SXE_MUTEX_LOCK(_mtx)
#define SXE_MUTEX_UNLOCK(_mtx)

#endif
#endif	/* INCLUDED_semaphore_h */
