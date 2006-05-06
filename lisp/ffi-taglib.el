;;; ffi-taglib.el --- SXEmacs interface to taglib
;;
;; Copyright (C) 2006 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: ffi, taglib
;;
;; This file is part of SXEmacs.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under a BSD-like licence.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.
;; Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.
;; Neither the name of the Technical University of Berlin nor the names of its
;; contributors may be used to endorse or promote products derived from this
;; software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;
;;; Commentary:
;;
;;; Code:

(require 'ffi)
(require 'ffi-libc)

;; this is our spine, barf if it does not exist.
;; But it won't load everywhere unless you first load the C++ libtag.so
;; first.  Not sure why, but I suspect there is some crazy magic voodoo
;; redirection going on between the C++ lib and the C lib. --SY.
(ffi-load "libtag.so")
(ffi-load "libtag_c.so")

(unless (ffi-find-named-type 'TagLib_File)
  (define-ffi-type TagLib_File (pointer void)))
(unless (ffi-find-named-type 'TagLib_Tag)
  (define-ffi-type TagLib_Tag (pointer void)))
(unless (ffi-find-named-type 'TagLib_AudioProperties)
  (define-ffi-type TagLib_AudioProperties (pointer void)))


(defconst taglib:file_new
  (ffi-defun '(function TagLib_File c-string)
             "taglib_file_new")
  "Create and return File object.")
(defun taglib:file-new (file)
  "Create and return File object."
  (when (file-readable-p file)
    (let ((f (ffi-create-fo 'c-string file)))
      (ffi-call-function taglib:file_new f))))

(defconst taglib:file_free
  (ffi-defun '(function void TagLib_File)
             "taglib_file_free")
  "Destruct File object.")
(defun taglib:file-free (file-object)
  "Destruct File object."
  (when (ffi-object-p file-object)
    (ffi-call-function taglib:file_free file-object)))

(defconst taglib:file_save
  (ffi-defun '(function int TagLib_File)
             "taglib_file_save")
  "Save tags back to File object.")
(defun taglib:file-save (file-object)
  "Save tags back to File object."
  (when (ffi-object-p file-object)
    (null
     (zerop
      (ffi-get
       (ffi-call-function taglib:file_save file-object))))))


;;; constructors/destructors
(defconst taglib:file_tag
  (ffi-defun '(function TagLib_Tag TagLib_File)
             "taglib_file_tag")
  "Return the tag object associated with the file object.")
(defun taglib:file-tag (file-object)
  "Return the tag object associated with FILE-OBJECT."
  (when (ffi-object-p file-object)
    (ffi-call-function taglib:file_tag file-object)))

(defconst taglib:tag_free_strings
  (ffi-defun '(function void)
             "taglib_tag_free_strings")
  "Free strings allocated by tag lookup functions.")
(defun taglib:tag-free-strings ()
  "Free strings allocated by tag lookup functions."
  (ffi-call-function taglib:tag_free_strings)
  t)

;;; accessors
;; char *taglib_tag_title(const TagLib_Tag *tag);
(defconst taglib:tag_title
  (ffi-defun '(function c-string TagLib_Tag)
             "taglib_tag_title")
  "Return the title associated with tag.")
(defun taglib:tag-title (tag-object)
  "Return the title associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_title tag-object))
           (result
            (ffi-get raw)))
      (taglib:tag-free-strings)
      (unless (zerop (length result))
        result))))

;; char *taglib_tag_artist(const TagLib_Tag *tag);
(defconst taglib:tag_artist
  (ffi-defun '(function c-string TagLib_Tag)
             "taglib_tag_artist")
  "Return the artist associated with tag.")
(defun taglib:tag-artist (tag-object)
  "Return the artist associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_artist tag-object))
           (result
            (ffi-get raw)))
      (taglib:tag-free-strings)
      (unless (zerop (length result))
        result))))

;; char *taglib_tag_album(const TagLib_Tag *tag);
(defconst taglib:tag_album
  (ffi-defun '(function c-string TagLib_Tag)
             "taglib_tag_album")
  "Return the album associated with tag.")
(defun taglib:tag-album (tag-object)
  "Return the album associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_album tag-object))
           (result
            (ffi-get raw)))
      (taglib:tag-free-strings)
      (unless (zerop (length result))
        result))))

;; char *taglib_tag_comment(const TagLib_Tag *tag);
(defconst taglib:tag_comment
  (ffi-defun '(function c-string TagLib_Tag)
             "taglib_tag_comment")
  "Return the comment associated with tag.")
(defun taglib:tag-comment (tag-object)
  "Return the comment associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_comment tag-object))
           (result
            (ffi-get raw)))
      (taglib:tag-free-strings)
      (unless (zerop (length result))
        result))))

;; char *taglib_tag_genre(const TagLib_Tag *tag);
(defconst taglib:tag_genre
  (ffi-defun '(function c-string TagLib_Tag)
             "taglib_tag_genre")
  "Return the genre associated with tag.")
(defun taglib:tag-genre (tag-object)
  "Return the genre associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_genre tag-object))
           (result
            (ffi-get raw)))
      (taglib:tag-free-strings)
      (unless (zerop (length result))
        result))))

;; unsigned int taglib_tag_year(const TagLib_Tag *tag);
(defconst taglib:tag_year
  (ffi-defun '(function int TagLib_Tag)
             "taglib_tag_year")
  "Return the year associated with tag.")
(defun taglib:tag-year (tag-object)
  "Return the year associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_year tag-object))
           (result
            (ffi-get raw)))
      (unless (zerop result)
        result))))

;; unsigned int taglib_tag_track(const TagLib_Tag *tag);
(defconst taglib:tag_track
  (ffi-defun '(function int TagLib_Tag)
             "taglib_tag_track")
  "Return the track number associated with tag.")
(defun taglib:tag-track (tag-object)
  "Return the track number associated with TAG-OBJECT."
  (when (ffi-object-p tag-object)
    (let* ((raw
            (ffi-call-function taglib:tag_track tag-object))
           (result
            (ffi-get raw)))
      (unless (zerop result)
        result))))

;;; modifiers
;; void taglib_tag_set_title(TagLib_Tag *tag, const char *title);
(defconst taglib:tag_set_title
  (ffi-defun '(function void TagLib_Tag c-string)
             "taglib_tag_set_title")
  "Set the title and associate it with tag.")
(defun taglib:tag-set-title (tag-object title)
  "Set the title to TITLE and associate it with TAG-OBJECT."
  (when (and (stringp title)
             (ffi-object-p tag-object))
    (let ((tit (ffi-create-fo 'c-string title)))
      (ffi-call-function taglib:tag_set_title tag-object tit))
    t))

;; void taglib_tag_set_artist(TagLib_Tag *tag, const char *artist);
(defconst taglib:tag_set_artist
  (ffi-defun '(function void TagLib_Tag c-string)
             "taglib_tag_set_artist")
  "Set the artist and associate it with tag.")
(defun taglib:tag-set-artist (tag-object artist)
  "Set the artist to ARTIST and associate it with TAG-OBJECT."
  (when (and (stringp artist)
             (ffi-object-p tag-object))
    (let ((art (ffi-create-fo 'c-string artist)))
      (ffi-call-function taglib:tag_set_artist tag-object art))
    t))

;; void taglib_tag_set_album(TagLib_Tag *tag, const char *album);
(defconst taglib:tag_set_album
  (ffi-defun '(function void TagLib_Tag c-string)
             "taglib_tag_set_album")
  "Set the album and associate it with tag.")
(defun taglib:tag-set-album (tag-object album)
  "Set the album to ALBUM and associate it with TAG-OBJECT."
  (when (and (stringp album)
             (ffi-object-p tag-object))
    (let ((alb (ffi-create-fo 'c-string album)))
      (ffi-call-function taglib:tag_set_album tag-object alb))
    t))

;; void taglib_tag_set_comment(TagLib_Tag *tag, const char *comment);
(defconst taglib:tag_set_comment
  (ffi-defun '(function void TagLib_Tag c-string)
             "taglib_tag_set_comment")
  "Set the comment and associate it with tag.")
(defun taglib:tag-set-comment (tag-object comment)
  "Set the comment to COMMENT and associate it with TAG-OBJECT."
  (when (and (stringp comment)
             (ffi-object-p tag-object))
    (let ((com (ffi-create-fo 'c-string comment)))
      (ffi-call-function taglib:tag_set_comment tag-object com))
    t))

;; void taglib_tag_set_genre(TagLib_Tag *tag, const char *genre);
(defconst taglib:tag_set_genre
  (ffi-defun '(function void TagLib_Tag c-string)
             "taglib_tag_set_genre")
  "Set the genre and associate it with tag.")
(defun taglib:tag-set-genre (tag-object genre)
  "Set the genre to GENRE and associate it with TAG-OBJECT."
  (when (and (stringp genre)
             (ffi-object-p tag-object))
    (let ((gen (ffi-create-fo 'c-string genre)))
      (ffi-call-function taglib:tag_set_genre tag-object gen))
    t))

;; void taglib_tag_set_year(TagLib_Tag *tag, unsigned int year);
(defconst taglib:tag_set_year
  (ffi-defun '(function void TagLib_Tag int)
             "taglib_tag_set_year")
  "Set the year and associate it with tag.")
(defun taglib:tag-set-year (tag-object year)
  "Set the year to YEAR and associate it with TAG-OBJECT."
  (when (and (natnump year)
             (ffi-object-p tag-object))
    (let ((yea (ffi-create-fo int year)))
      (ffi-call-function taglib:tag_set_year tag-object yea))
    t))

;; void taglib_tag_set_track(TagLib_Tag *tag, unsigned int track);
(defconst taglib:tag_set_track
  (ffi-defun '(function void TagLib_Tag int)
             "taglib_tag_set_track")
  "Set the track number and associate it with tag.")
(defun taglib:tag-set-track (tag-object track)
  "Set the track number to TRACK and associate it with TAG-OBJECT."
  (when (and (natnump track)
             (ffi-object-p tag-object))
    (let ((tra (ffi-create-fo int track)))
      (ffi-call-function taglib:tag_set_track tag-object tra))
    t))


;;; constructors
(defconst taglib:file_audioproperties
  (ffi-defun '(function TagLib_AudioProperties TagLib_File)
             "taglib_file_audioproperties")
  "Return the AudioProperties object associated with the file object.")
(defun taglib:file-audio-properties (file-object)
  "Return the audio properties object associated with FILE-OBJECT."
  (when (ffi-object-p file-object)
    (ffi-call-function taglib:file_audioproperties file-object)))

(defconst taglib:audioproperties_length
  (ffi-defun '(function int TagLib_AudioProperties)
             "taglib_audioproperties_length")
  "Return the length of the audioproperties object in seconds.")
(defun taglib:audioproperties-length (audioprops)
  "Return the length of AUDIOPROPS in seconds."
  (when (ffi-object-p audioprops)
    (let* ((raw
            (ffi-call-function taglib:audioproperties_length audioprops))
           (result
            (ffi-get raw)))
      (unless (zerop result)
        result))))

(defconst taglib:audioproperties_bitrate
  (ffi-defun '(function int TagLib_AudioProperties)
             "taglib_audioproperties_bitrate")
  "Return the bitrate of the audioproperties object in kb/s.")
(defun taglib:audioproperties-bitrate (audioprops)
  "Return the bitrate of AUDIOPROPS in kb/s (kilobit per second)."
  (when (ffi-object-p audioprops)
    (let* ((raw
            (ffi-call-function taglib:audioproperties_bitrate audioprops))
           (result
            (ffi-get raw)))
      (unless (zerop result)
        result))))

(defconst taglib:audioproperties_samplerate
  (ffi-defun '(function int TagLib_AudioProperties)
             "taglib_audioproperties_samplerate")
  "Return the samplerate of the audioproperties object in Hz.")
(defun taglib:audioproperties-samplerate (audioprops)
  "Return the samplerate of AUDIOPROPS in Hz."
  (when (ffi-object-p audioprops)
    (let* ((raw
            (ffi-call-function taglib:audioproperties_samplerate audioprops))
           (result
            (ffi-get raw)))
      (unless (zerop result)
        result))))

(defconst taglib:audioproperties_channels
  (ffi-defun '(function int TagLib_AudioProperties)
             "taglib_audioproperties_channels")
  "Return the number of channels of the audioproperties object.")
(defun taglib:audioproperties-channels (audioprops)
  "Return the number of channels of AUDIOPROPS."
  (when (ffi-object-p audioprops)
    (let* ((raw
            (ffi-call-function taglib:audioproperties_channels audioprops))
           (result
            (ffi-get raw)))
      (unless (zerop result)
        result))))


;;; higher level API
(defun taglib:properties (file)
  "Return an alist of available properties of FILE."
  (when (file-readable-p file)
    (let ((result (dllist))
          (tlf (taglib:file-new file)))
      (when tlf
        (let ((tlt (taglib:file-tag tlf))
              (tlap (taglib:file-audio-properties tlf))
              (tfuns (list
                      (cons 'title #'taglib:tag-title)
                      (cons 'artist #'taglib:tag-artist)
                      (cons 'album #'taglib:tag-album)
                      (cons 'comment #'taglib:tag-comment)
                      (cons 'genre #'taglib:tag-genre)
                      (cons 'year #'taglib:tag-year)
                      (cons 'track #'taglib:tag-track)))
              (apfuns (list
                       (cons 'length #'taglib:audioproperties-length)
                       (cons 'bitrate #'taglib:audioproperties-bitrate)
                       (cons 'samplerate #'taglib:audioproperties-samplerate)
                       (cons 'channels #'taglib:audioproperties-channels))))
          (mapc-internal
           #'(lambda (fun)
               (let ((res (funcall (cdr fun) tlt)))
                 (when res
                   (dllist-append result (cons (car fun) res)))))
           tfuns)
          (mapc-internal
           #'(lambda (fun)
               (let ((res (funcall (cdr fun) tlap)))
                 (when res
                   (dllist-append result (cons (car fun) res)))))
           apfuns)))
      (taglib:file-free tlf)
      (dllist-to-list result))))
;; (taglib:properties "/home/media/audio/bloke.mp3")


(provide 'ffi-taglib)

;;; ffi-taglib.el ends here
