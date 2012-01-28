/* nas.c --- SXEmacs support for the Network Audio System server.
 *
 * Author: Sebastian Freundt <hroptatyr@sxemacs.org>
 *
 * Copyright 2006 Sebastian Freundt
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include "sysdep.h"
#include "syssignal.h"
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <setjmp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "media.h"
#include "sound-nas.h"
#include "ui/device.h"

static JMP_BUF nas_server_sig;
static AuBool nas_error_handler(AuServer* aud, AuErrorEvent* ev);
static AuBool nas_IOerror_handler(AuServer* aud);

DECLARE_AUDIO_DEVICE_SIMPLE_METHS(sound_nas);
DEFINE_AUDIO_DEVICE_SIMPLE(sound_nas);

Lisp_Object Qnas;
#define MYSELF ADRIVER_NAS

#define __NAS_DEBUG__(args...)		fprintf(stderr, "NAS " args)
#ifndef NAS_DEBUG_FLAG
#define NAS_DEBUG(args...)
#else
#define NAS_DEBUG(args...)		__NAS_DEBUG__(args)
#endif
#define NAS_DEBUG_C(args...)		NAS_DEBUG("[connection]: " args)
#define NAS_DEBUG_S(args...)		NAS_DEBUG("[stream]: " args)
#define NAS_DEBUG_COE(args...)		NAS_DEBUG("[coerce]: " args)
#define NAS_DEBUG_EV(args...)		NAS_DEBUG("[event]: " args)
#define NAS_DEBUG_AJ(args...)		NAS_DEBUG("[audio-job]: " args)
#define NAS_CRITICAL(args...)		__NAS_DEBUG__("CRITICAL: " args)


static char *nas_event_types[] = {
	"Undefined",
	"Undefined",
	"ElementNotify",
	"GrabNotify",
	"MonitorNotify",
	"BucketNotify",
	"DeviceNotify"
};

static char *nas_elementnotify_kinds[] = {
	"LowWater",
	"HighWater",
	"State",
	"Unknown"
};

static char *nas_states[] = {
	"Stop",
	"Start",
	"Pause",
	"Any"
};

static char *nas_reasons[] = {
	"User",
	"Underrun",
	"Overrun",
	"EOF",
	"Watermark",
	"Hardware",
	"Any"
};

#if defined __GNUC__
static char* nas_reason(unsigned int reason) __attribute__((unused));
#endif
static char*
nas_reason(unsigned int reason)
{
	if (reason > 6){
		reason = 6;
	}
	return nas_reasons[reason];
}

#if defined __GNUC__
static char* nas_elementnotify_kind(unsigned int kind) __attribute__((unused));
#endif
static char* nas_elementnotify_kind(unsigned int kind)
{
	if (kind > 2) kind = 3;
	return nas_elementnotify_kinds[kind];
}

#if defined __GNUC__
static char* nas_event_type(unsigned int type) __attribute__((unused));
#endif
static char* nas_event_type(unsigned int type)
{
	if (type > 6) type = 0;
	return nas_event_types[type];
}

#if defined __GNUC__
static char* nas_state(unsigned int state) __attribute__((unused));
#endif
static char* nas_state(unsigned int state)
{
	if (state>3) state = 3;
	return nas_states[state];
}


static Lisp_Object
sound_nas_mark(ad_device_data *devdata)
{
	sound_nas_data_t *snd = devdata;

	snd = NULL;;

	return Qnil;
}

static void
sound_nas_print(Lisp_Object device, Lisp_Object pcfun, int ef)
{
	sound_nas_data_t *snd = NULL;

	snd = get_audio_device_data(device);

	/* cannot use incomplete or corrupt audio devices */
	if (XAUDIO_DEVICE_DRIVER(device) != MYSELF || snd == NULL) {
		write_c_string(" VOID", pcfun);
		/* now that we are here, mark AO device as dead */
		XAUDIO_DEVICE_STATE(device) = ASTATE_DEAD;
		return;
	}

	/* info about the connected output plugin */
	write_c_string(" :server ", pcfun);
	if (snd->aud && snd->aud->server_name)
		write_c_string(snd->aud->server_name, pcfun);
	else
		write_c_string("#unknown", pcfun);

	write_c_string(" :server-handle ", pcfun);
	if (snd->aud == NULL)
		write_c_string("#b0rked", pcfun);
	else
		write_fmt_str(pcfun, "0x%x", (unsigned int)snd->aud);

	return;
}


static AuDeviceID
nas_find_device(AuServer *aud, int channels)
{
	int i;
	for (i = 0; i < AuServerNumDevices(aud); i++) {
		AuDeviceAttributes *dev = AuServerDevice(aud, i);
		if ((AuDeviceKind(dev) == AuComponentKindPhysicalOutput) &&
		     AuDeviceNumTracks(dev) == channels) {
			return AuDeviceIdentifier(dev);
		}
	}
	return AuNone;
}

static void
nas_setup_defaults(char **server, int *cnt)
{
	int i;
	Lisp_Object tmp;

	/* considering some defaults */
	NAS_DEBUG("trying to find some defaults\n");

#ifdef HAVE_X_WINDOWS
	/* check for the device connection of the currently active frame */
	tmp = Fselected_device(Qnil);
	if (DEVICEP(tmp) && DEVICE_X_P(XDEVICE(tmp)))
		server[(*cnt)++] =
			(char*)XSTRING_DATA(
				DEVICE_CONNECTION(XDEVICE(tmp)));

	/* tbd: check for conn of the initial frame */
#endif

	/* try to look for $AUDIOSERVER */
	if ((server[(*cnt)] = getenv("AUDIOSERVER"))) {
		/* only add the stuff, if not already in the try queue */
		for (i=0; i < (*cnt); i++)
			if (strcmp(server[i], server[(*cnt)]) == 0)
				break;
		if (i == (*cnt))
			(*cnt)++;
	}
	/* try to look for $DISPLAY */
	if ((server[(*cnt)] = getenv("DISPLAY"))){
		/* only add the stuff, if not already in the try queue */
		for (i=0; i < (*cnt); i++)
			if (strcmp(server[i], server[(*cnt)]) == 0)
				break;
		if (i == (*cnt))
			(*cnt)++;
	}

	/* oh, let's try localhost:0.0, if not already there of course */
	for (i=0; i < (*cnt); i++)
		if (strcmp(server[i], "localhost:0.0") == 0)
			break;
	if (i == (*cnt))
		server[(*cnt)++] = "localhost:0.0";

	/* finally we try NULL, too */
	server[(*cnt)++] = NULL;

	return;
}

static AuServer *
nas_try_connection(char *server)
{
	AuServer *result = NULL;
	char *err_message = NULL;

	/* open server */
	NAS_DEBUG_C("trying to contact NAS server: %s\n", server);
	message(GETTEXT("trying to contact NAS server at %s..."),
		server);
	result = AuOpenServer(server, 0, NULL, 0, NULL, &err_message);

	if (!result) {
		NAS_DEBUG_C("cannot contact NAS server: %s\n",
			     (err_message ? err_message : ":("));
	}

	return result;
}

static ad_device_data *
sound_nas_create(Lisp_Object nas_options)
{
	sound_nas_data_t *snd;
	char *server[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
	int i, server_cnt = 0;
	AuServer *aud = NULL;
	Lisp_Object opt_server = Qnil;

	/* parse options */
	opt_server = Fplist_get(nas_options, intern(":server"), Qnil);
	if (!NILP(opt_server) && !STRINGP(opt_server) && !DEVICEP(opt_server)) {
		wrong_type_argument(Qstringp, opt_server);
		return NULL;
	}

	if (NILP(opt_server))
		nas_setup_defaults(server, &server_cnt);
	else if (STRINGP(opt_server))
		server[server_cnt++] = (char*)XSTRING_DATA(opt_server);
#ifdef HAVE_X_WINDOWS
	else if (DEVICEP(opt_server) && DEVICE_X_P(XDEVICE(opt_server)))
		server[server_cnt++] =
			(char*)XSTRING_DATA(
				DEVICE_CONNECTION(XDEVICE(opt_server)));
#endif

	NAS_DEBUG("trying %d connections\n", server_cnt);
	for (i = 0; i<server_cnt; i++)
		if ((aud = nas_try_connection(server[i])))
			break;

	if (!aud) {
		NAS_DEBUG_C("cannot contact any NAS server\n");
		warn_when_safe(Qnas, Qwarning,
			       GETTEXT("No NAS servers in sight.\n"));
		return NULL; /* Could not contact NAS server */
	}


	/* -- initialise -- */
	snd = xnew_and_zero(sound_nas_data_t);
	snd->aud = aud;

	/* round up SOUND_MAX_AUDIO_FRAME_SIZE to multiple of NAS_FRAG_SIZE
	 * divide by 3 first because of 2:1 split */
	snd->proposed_buffer_size =
		(SOUND_MAX_AUDIO_FRAME_SIZE/3 + NAS_FRAG_SIZE-1)
		& ~(NAS_FRAG_SIZE-1);
	NAS_DEBUG_C("proposed buffer size: %u\n", snd->proposed_buffer_size);

	NAS_DEBUG_C("created: 0x%x\n", (unsigned int)snd);

	return snd;
}

static void
sound_nas_finish(ad_device_data *data)
{
	sound_nas_data_t *snd = data;

	NAS_DEBUG("finishing NAS\n");

	if (snd->aud) {
		NAS_DEBUG("closing 0x%x\n", (unsigned int)snd->aud);
		AuCloseServer(snd->aud);
	}
	snd->aud = NULL;

	NAS_DEBUG("audio-device finished.\n");

	return;
}


#define snsd_t	sound_nas_aj_data_t
static int
nas_fill(audio_job_t aj, int nframes)
{
	size_t len = 0, tmplen = 0;
	sxe_media_sample_t *tmpbuf;
	snsd_t *snsd = audio_job_device_data(aj);
	int i;

	tmpbuf = (sxe_media_sample_t*)(aj->buffer + snsd->writepos);
	len = media_stream_meth(aj->substream->up, read)(
		aj->substream, aj->buffer + snsd->writepos,
		nframes);

	/* set up the volume args */
	snsd->volargs->volume[0] = snsd->volargs->volume[1] =
		aj->volume;
	/* set up the rerate args */
	snsd->rrargs->tweak = aj->ratetrafo;

	/* coerce the stuff */
	tmplen = snsd->channels*len;
	for (i = 0; i < snsd->coe_ch_cnt; i++) {
		NAS_DEBUG_COE("calling coerce "
			      "%d on b:0x%x l:%d\n",
			      i, (unsigned int)tmpbuf, tmplen);
		tmplen = CALL_MEDIA_SAMPLE_EFFECT(
			snsd->coe_chain, i, tmpbuf, tmpbuf, tmplen);
	}
	/* bring back to S16 or U8 */
	MEDIA_SAMPLE_FORMAT_DOWNSAMPLE(snsd->msf)(
		aj->buffer + snsd->writepos,
		aj->buffer + snsd->writepos,
		tmplen);

	/* convert tmplen (#samples) to number of bytes */
	tmplen = tmplen * snsd->framesize/snsd->channels;
	snsd->overfill += tmplen;
	snsd->writepos += tmplen;

	/* increase underrun counter to detect the end */
	if (len == 0)
		snsd->underrun++;

	return tmplen;
}

static int
nas_read(audio_job_t aj, size_t nbytes)
{
	AuStatus as;
	size_t tmplen = 0;
	snsd_t *snsd = audio_job_device_data(aj);

	NAS_DEBUG_EV("want to read: %d\n", nbytes);

	if (snsd->writepos < aj->buffer_alloc_size >> 2 &&
	    snsd->underrun < 4) {
		tmplen = nas_fill(aj, nbytes*snsd->channels/snsd->framesize);
	} else if ((size_t)snsd->overfill > nbytes * 8 || snsd->underrun >= 4) {
		NAS_DEBUG_S("having a rest\n");
	} else {
		NAS_DEBUG_S("resetting write position.\n");
		memcpy(aj->buffer,
		       aj->buffer+snsd->readpos,
		       snsd->writepos - snsd->readpos);
		snsd->writepos -= snsd->readpos;
		snsd->readpos = 0;
	}

	NAS_DEBUG_S("req:%d p, got:%d p, readpos %d, writepos %d, "
		    "overfill: %d, framesize: %d\n",
		    nbytes, tmplen*snsd->framesize,
		    snsd->readpos, snsd->writepos,
		    snsd->overfill, snsd->framesize);

	/* care for buffer underruns */
	if (nbytes > snsd->overfill)
		nbytes = snsd->overfill;

	/*
	 * Now write the new buffer to the network.
	 */
	NAS_DEBUG_C("writing %d bytes\n", nbytes);
	AuWriteElement(snsd->snd->aud, snsd->flow, 0, nbytes,
		       aj->buffer+snsd->readpos, AuFalse, &as);
	if (as != AuSuccess)
		NAS_DEBUG_C("nas_read(): AuWriteElement\n");
	snsd->readpos += nbytes;
	snsd->overfill -= nbytes;

	/* detect end of track */
	if (snsd->underrun >= 4 && snsd->overfill < 64)
		aj->play_state = MTPSTATE_STOP;

	return nbytes;
}

static AuBool
nas_event_handler(AuServer *aud, AuEvent *ev, AuEventHandlerRec *hnd)
{
	AuElementNotifyEvent *event = (AuElementNotifyEvent*)ev;
	audio_job_t aj = hnd->data;
	snsd_t *snsd = audio_job_device_data(aj);

	NAS_DEBUG_EV("event_handler(): "
		     "type %s kind %s state %s->%s reason %s "
		     "numbytes %u\n",
		     nas_event_type(event->type),
		     nas_elementnotify_kind(event->kind),
		     nas_state(event->prev_state),
		     nas_state(event->cur_state),
		     nas_reason(event->reason),
		     (uint32_t)event->num_bytes);

	if (event->num_bytes > INT_MAX) {
		NAS_CRITICAL("num_bytes > 2GB, server buggy?\n");
	}

	switch (event->reason) {
	case AuReasonWatermark:
		nas_read(aj, event->num_bytes);
		break;
	case AuReasonUnderrun:
		/* buffer underrun -> refill buffer */
		if (nas_read(aj, event->num_bytes) != 0) {
			event->cur_state = AuStateStart;
			NAS_DEBUG_EV("restarting\n");
			break;
		}
		NAS_DEBUG_S("Can't refill buffer, stopping flow.\n");
		AuStopFlow(aud, snsd->flow, NULL);
		aj->play_state = MTPSTATE_STOP;
		break;
	default:
		break;
	}

	return AuTrue;
}

static AuBool
nas_error_handler(AuServer* aud, AuErrorEvent* ev)
{
	char s[100];
	AuGetErrorText(aud, ev->error_code, s, 100);
	NAS_CRITICAL("error [%s]\n"
		     "error_code: %d\n"
		     "request_code: %d\n"
		     "minor_code: %d\n",
		     s, ev->error_code, ev->request_code, ev->minor_code);

	LONGJMP(nas_server_sig, 1);
	return AuTrue;
}

static AuBool
nas_IOerror_handler(AuServer *aud)
{
	NAS_CRITICAL("Strange things happened.\n");
	if (aud) {
		NAS_CRITICAL("Connection seems to be broken.\n");
	} else {
		NAS_CRITICAL("Server raised SIGPIPE.\n");
	}

	LONGJMP(nas_server_sig, 1);
	return AuTrue;
}

static int
nas_empty_event_queue(snsd_t *snsd)
{
	AuEvent ev;
	int result = 0;

	while (AuScanForTypedEvent(
		       snsd->snd->aud, AuEventsQueuedAfterFlush,
		       AuTrue, AuEventTypeElementNotify, &ev)) {
		AuDispatchEvent(snsd->snd->aud, &ev);
		result = 1;
	}
	return result;
}

#ifdef EF_USE_ASYNEQ
static inline void
sound_nas_change_volume(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->volume = args->volume_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_nas_change_rate(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	aj->ratetrafo = args->rate_args;
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_nas_change_state(audio_job_t aj, audio_job_event_args_t args)
{
	SXE_MUTEX_LOCK(&aj->mtx);
	switch (args->state_args) {
	case aj_pause:
		NAS_DEBUG_AJ("->pause state\n");
		aj->play_state = MTPSTATE_PAUSE;
		break;
	case aj_resume:
		NAS_DEBUG_AJ("->resume state\n");
		aj->play_state = MTPSTATE_RUN;
		break;
	case aj_start:
		NAS_DEBUG_AJ("->start state\n");
		break;
	case aj_stop:
		NAS_DEBUG_AJ("->stop state\n");
		aj->play_state = MTPSTATE_STOP;
		break;
	case no_audio_job_change_states:
	default:
		NAS_DEBUG_AJ("->unknown state\n");
		break;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
}

static inline void
sound_nas_handle_aj_events(audio_job_t aj)
	__attribute__((always_inline));
static inline void
sound_nas_handle_aj_events(audio_job_t aj)
{
	sound_nas_aj_data_t *sasd;
	audio_job_event_t ev = NULL;

#if 0
	assert(audio_job_queue(aj));
#endif

	SXE_MUTEX_LOCK(&aj->mtx);
	sasd = audio_job_device_data(aj);
	if ((ev = eq_noseeum_dequeue(audio_job_queue(aj))) == NULL) {
		SXE_MUTEX_UNLOCK(&aj->mtx);
		return;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);

	NAS_DEBUG_AJ("Event 0x%lx\n", (long unsigned int)ev);
	switch (audio_job_event_kind(ev)) {
	case aj_change_state:
		NAS_DEBUG_AJ("change state event\n");
		sound_nas_change_state(aj, &audio_job_event_args(ev));
		break;
	case aj_change_volume:
		NAS_DEBUG_AJ("change volume event\n");
		sound_nas_change_volume(aj, &audio_job_event_args(ev));
		break;
	case aj_change_rate:
		NAS_DEBUG_AJ("change rate event\n");
		sound_nas_change_rate(aj, &audio_job_event_args(ev));
		break;
	case no_audio_job_event_kinds:
	default:
		NAS_CRITICAL("unknown event\n");
		break;
	}
	free_audio_job_event(ev);
}
#endif	/* EF_USE_ASYNEQ */

static int
sound_nas_play(audio_job_t aj)
{
	/* stream stuff */
	Lisp_Media_Stream *ms;
	media_substream *mss;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_nas_data_t *snd = NULL;
	/* nas stuff */
	AuElement elms[3];
	AuStatus as;
	int bsize = 0;
	/* subthread stuff */
	sound_nas_aj_data_t _snsd, *snsd = &_snsd;
	sxe_mse_volume_args _volargs, *volargs = &_volargs;
	sxe_mse_rerate_args _rrargs, *rrargs = &_rrargs;
	/* cache stuff */
	int alloced_myself = 0;

	SOUND_UNPACK_MT(aj, device, ms, mss, lad, snd, snsd->mtap);

	/* refuse to do anything if the AuServer pointer is not set */
	if (snd->aud == NULL) {
		NAS_DEBUG_C("b0rked connection, gute Nacht!\n");
		return 0;
	}

	/* install error handlers before anything else */
	AuSetErrorHandler(snd->aud, nas_error_handler);
	AuSetIOErrorHandler(snd->aud, nas_IOerror_handler);

	/* find physical output device */
	snsd->dev = nas_find_device(snd->aud, snsd->mtap->channels);

	if (snsd->dev == AuNone ||
	    !(snsd->flow = AuCreateFlow(snd->aud, NULL))) {
		/* No physical output device found or flow creation failed. */
		NAS_DEBUG_C("no physical devices for this stream\n");
		return 0;
	}

	/* A system call interrupted with a SIGALRM or SIGIO
	   comes back here */
	if (SETJMP(nas_server_sig)) {
		NAS_CRITICAL("Caught the lethal signal.\n");
		snd->aud = NULL;
		sound_nas_finish(snd);
		goto uhoh;
	}

	/* init the snsd */
	snsd->samplerate = 0;
	snsd->framesize = (snsd->channels = snsd->mtap->channels)
		* sizeof(int16_t);
	snsd->msf = MEDIA_SAMPLE_FORMAT(sxe_msf_S16);
	snsd->coe_ch_cnt = 0;
	snsd->resolution =
		(snsd->mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	bsize = (snsd->resolution + NAS_FRAG_SIZE - 1) & ~(NAS_FRAG_SIZE-1);

	snsd->snd = snd;
	snsd->buffer_size = bsize;
	snsd->writepos = snsd->readpos = 0;
	snsd->overfill = snsd->underrun = 0;

	/* set up flow */
	AuMakeElementImportClient(elms+0, snsd->mtap->samplerate,
				  AuFormatLinearSigned16LSB,
				  snsd->mtap->channels,
				  AuTrue,
				  bsize, bsize / 2,
				  0, NULL);
	snsd->gain = AuFixedPointFromFraction(1, 1);
	AuMakeElementMultiplyConstant(elms+1, 0, snsd->gain);
	AuMakeElementExportDevice(elms+2, 0, snsd->dev,
				  snsd->mtap->samplerate,
				  AuUnlimitedSamples, 0, NULL);
	AuSetElements(snd->aud, snsd->flow, AuTrue, 3, elms, &as);

	if (as != AuSuccess) {
		NAS_DEBUG_C("play(): AuSetElements failed\n");
		return 0;
	}

#if 0				/* atm we insist on having stereo access */
	/* the channel effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		snsd->coe_chain, snsd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_2ch_to_1ch), NULL);
#endif

	/* the volume effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		snsd->coe_chain, snsd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_volume), volargs);
	volargs->num_channels = snsd->channels;
	snsd->volargs = volargs;

	/* the rerate effect */
	ADD_MEDIA_SAMPLE_EFFECT(
		snsd->coe_chain, snsd->coe_ch_cnt,
		MEDIA_SAMPLE_EFFECT(sxe_mse_rerate), rrargs);
	rrargs->num_channels = snsd->channels;
	rrargs->srcrate = rrargs->tgtrate = 1;
	snsd->rrargs = rrargs;

	AuRegisterEventHandler(snd->aud, AuEventHandlerIDMask |
			       AuEventHandlerTypeMask,
			       AuEventTypeElementNotify, snsd->flow,
			       nas_event_handler, (AuPointer)aj);

	/* rewind the stream */
	media_stream_meth(ms, rewind)(mss);

	/* play chunks of the stream */
	SXE_MUTEX_LOCK(&aj->mtx);
	if (aj->buffer_alloc_size < SOUND_MAX_AUDIO_FRAME_SIZE) {
		alloced_myself = 1;
		aj->buffer = xmalloc_atomic(SOUND_MAX_AUDIO_FRAME_SIZE);
		aj->buffer_alloc_size = SOUND_MAX_AUDIO_FRAME_SIZE;
	}
	SXE_MUTEX_UNLOCK(&aj->mtx);
	snsd->mtp = MTPSTATE_STOP;
	snsd->volume = -1;
	audio_job_device_data(aj) = snsd;

	/* prefill the buffer */
	NAS_DEBUG_S("prefill the buffer: %d\n", 4*bsize);
	if (!nas_fill(aj, 4*bsize))
		goto uhoh;

	while (aj->play_state != MTPSTATE_STOP) {

#if 0
		if (aj->volume != snsd->volume) {
			AuElementParameters aep;
			NAS_DEBUG_S("Setting volume.\n");
			snsd->volume = aj->volume;
			snsd->gain = AU_FIXED_POINT_SCALE*(snsd->volume)/127;
			aep.parameters[AuParmsMultiplyConstantConstant] =
				snsd->gain;
			aep.flow = snd->flow;
			aep.element_num = 1;
			aep.num_parameters = AuParmsMultiplyConstant;

			AuSetElementParameters(snd->aud, 1, &aep, &as);
			if (as != AuSuccess) {
				NAS_DEBUG_S("Setting volume failed.\n");
			}
		}
#endif

#ifdef EF_USE_ASYNEQ
		/* events for me audio-job? */
		if (audio_job_queue(aj)) {
			sound_nas_handle_aj_events(aj);
		}
#endif

		SXE_MUTEX_LOCK(&aj->mtx);
		mtp = aj->play_state;
		SXE_MUTEX_UNLOCK(&aj->mtx);
		switch (mtp) {
		case MTPSTATE_RUN:
			if (snsd->mtp != mtp) {
				NAS_DEBUG("ah, gotta work again\n");
				AuStartFlow(snd->aud, snsd->flow, &as);
				if (as != AuSuccess) {
					NAS_DEBUG_C("play(): "
						    "AuStartFlow failed\n");
					aj->play_state = MTPSTATE_STOP;
				}
			}
			nas_empty_event_queue(snsd);
			usleep(snsd->resolution);
			break;
		case MTPSTATE_PAUSE:
			if (snsd->mtp != mtp) {
				NAS_DEBUG("sleeping for %d\n",
					  snsd->resolution);
				AuStopFlow(snd->aud, snsd->flow, &as);
			}
			usleep(snsd->resolution);
			break;

		case MTPSTATE_UNKNOWN:
		case MTPSTATE_STOP:
		case NUMBER_OF_MEDIA_THREAD_PLAY_STATES:
		default:
			NAS_DEBUG("ACK, quit\n");
			AuStopFlow(snd->aud, snsd->flow, &as);
			SXE_MUTEX_LOCK(&aj->mtx);
			aj->play_state = MTPSTATE_STOP;
			SXE_MUTEX_UNLOCK(&aj->mtx);
			break;
		}
		snsd->mtp = mtp;
	}

uhoh:
	/* -- Close and shutdown -- */
	SXE_MUTEX_LOCK(&aj->mtx);
	if (alloced_myself && aj->buffer) {
		xfree(aj->buffer);
	}
	aj->buffer = NULL;
	aj->buffer_alloc_size = 0;
	SXE_MUTEX_UNLOCK(&aj->mtx);

	return 1;
}

#undef MYSELF
