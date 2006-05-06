/* media-gstreamer.c - analyse audio files or streams via gstreamer

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

#include <config.h>
#include "lisp.h"

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "media-gstreamer.h"

static uint32_t media_gstreamer_sread_audio(media_substream*, void*, uint32_t);
static void media_gstreamer_srewind_audio(media_substream*);

#define MYSELF MDRIVER_GSTREAMER

typedef struct {
	Lisp_Media_Stream *ms;
	GMainLoop *loop;
	GstElement *bin;
} media_gstreamer_opaque;


static gboolean
idle_exit_loop(gpointer data)
{
	g_main_loop_quit((GMainLoop *)data);

	/* once */
	return FALSE;
}

static gboolean
bus_call(GstBus *bus, GstMessage *msg, gpointer data)
{
	GMainLoop *loop = data;

	switch (GST_MESSAGE_TYPE (msg)) {
	case GST_MESSAGE_EOS:
		fprintf(stderr,"End-of-stream\n");
		g_main_loop_quit(loop);
		break;
	case GST_MESSAGE_ERROR: {
		gchar *debug;
		GError *err;

		gst_message_parse_error(msg, &err, &debug);
		g_free(debug);

		fprintf(stderr, "Error: %s\n", err->message);
		g_error_free(err);

		g_main_loop_quit(loop);
		break;
	}
	default:
		break;
	}

	return TRUE;
}

static void
cb_newpad (GstElement *decodebin, GstPad *pad, gboolean last, gpointer data)
{
	GstCaps *caps;
	GstStructure *str;
	GstElement *audio = ((media_gstreamer_opaque*)data)->bin;
	GstPad *sinkpad;

	/* only link once */
	sinkpad = gst_element_get_pad(audio, "sink");
	if (GST_PAD_IS_LINKED(sinkpad)) {
		g_object_unref(sinkpad);
		return;
	}

	/* check media type */
	caps = gst_pad_get_caps (pad);
	str = gst_caps_get_structure (caps, 0);
	if (!g_strrstr (gst_structure_get_name (str), "audio")) {
		gst_caps_unref (caps);
		g_object_unref(sinkpad);
		return;
	}
	fprintf(stderr, "%s\n", gst_caps_to_string(caps));
	gst_caps_unref (caps);

	/* link'n'play */
	gst_pad_link(pad, sinkpad);
}

static void
cb_typefound (GstElement *typefind, guint probability,
	      GstCaps *caps, gpointer data)
{
	GMainLoop *loop = ((media_gstreamer_opaque*)data)->loop;
	gchar *type;
	/* lisp stream stuff */
	Lisp_Media_Stream *ms = ((media_gstreamer_opaque*)data)->ms;
	media_substream *mss;
	mtype_audio_properties *mtap;
	char *name = xmalloc(256);

	type = gst_caps_to_string(caps);
	snprintf(name, 255, "%s", type);
	g_free(type);


	/* create a substream */
	mss = make_media_substream_append(ms);

	/* retrieve the signal information */
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->name = name;
	mtap->channels = 2; //stinfo->channels;
	mtap->samplerate = 44100; //stinfo->rate;
	mtap->samplewidth = 0;
	mtap->framesize = 0;
	mtap->bitrate = 0;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_substream_sread(mss) = media_gstreamer_sread_audio;
	media_substream_srewind(mss) = media_gstreamer_srewind_audio;


	/* since we connect to a signal in the pipeline thread context, we need
	 * to set an idle handler to exit the main loop in the mainloop context.
	 * Normally, your app should not need to worry about such things. */
	g_idle_add(idle_exit_loop, loop);
}

void media_gstreamer_analyse_stream(Lisp_Media_Stream *ms)
{
	/* gstreamer stuff */
	GMainLoop *loop;
	GstElement *pipeline, *source = NULL, *sink = NULL, *conv = NULL;
	GstElement *typefind = NULL, *dec = NULL;
	GstElement *audio = NULL;
	GstPad *sinkpad;
	media_gstreamer_opaque data;

	/* initialise GStreamer */
	gst_init(NULL, NULL);
	loop = g_main_loop_new(NULL, FALSE);
	pipeline = gst_pipeline_new("SXEmacs");
	gst_bus_add_watch(gst_pipeline_get_bus(GST_PIPELINE(pipeline)),
			  bus_call, loop);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		file = mkfp->filename;

		source = gst_element_factory_make ("filesrc", "file-source");
		g_object_set(G_OBJECT(source), "location", file, NULL);
		break;
	}
	case MKIND_STRING: {
		/* not yet handable */
		break;
	}
	default:
		break;
	}

	/* nah, free the gst stuff */
	if (!source)
		return;

	/* use the gstreamer typefind concept */
	data.ms = ms;
	data.loop = loop;

#if 1
	typefind = gst_element_factory_make("typefind", "typefinder");
	g_signal_connect(typefind, "have-type",
			 G_CALLBACK(cb_typefound), &data);

	/* setup */
	gst_bin_add_many(GST_BIN(pipeline), source, typefind, NULL);
	gst_element_link(source, typefind);
	gst_element_set_state(GST_ELEMENT(pipeline), GST_STATE_PLAYING);
	g_main_loop_run(loop);

	/* unset */
	gst_element_set_state(GST_ELEMENT(pipeline), GST_STATE_NULL);
	gst_object_unref(GST_OBJECT(pipeline));

#else				/* later */
	dec = gst_element_factory_make("decodebin", "decoder");
	g_signal_connect(dec, "new-decoded-pad", G_CALLBACK(cb_newpad), &data);
	gst_bin_add_many(GST_BIN(pipeline), source, dec, NULL);
	gst_element_link(source, dec);

	/* create audio output */
	audio = gst_bin_new("audiobin");
	conv = gst_element_factory_make("audioconvert", "aconv");
	sinkpad = gst_element_get_pad(conv, "sink");
	sink = gst_element_factory_make("fakesink", "sink");
	gst_bin_add_many(GST_BIN(audio), conv, sink, NULL);
	gst_element_link(conv, sink);
	gst_element_add_pad(audio, gst_ghost_pad_new("sink", sinkpad));
	gst_object_unref(sinkpad);
	gst_bin_add(GST_BIN(pipeline), audio);

	data.bin = audio;
#endif

	/* run */
	gst_element_set_state(pipeline, GST_STATE_PLAYING);
	g_main_loop_run(loop);

	/* cleanup */
	gst_element_set_state(pipeline, GST_STATE_NULL);
	gst_object_unref(GST_OBJECT(pipeline));

	/* keep the gstreamer context */
	media_stream_data(ms) = NULL;
	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;
}


static uint32_t
media_gstreamer_sread_audio(media_substream *mss, void *outbuf, uint32_t length)
{
/* read at most `length' frames into `outbuf' */
#if 0
#endif
	return 0;
}

static void
media_gstreamer_srewind_audio(media_substream *mss)
{
/* rewind the stream to the first frame */
#if 0
#endif
}

#undef MYSELF
