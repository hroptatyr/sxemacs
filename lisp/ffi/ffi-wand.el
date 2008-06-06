;;; ffi-wand.el --- SXEmacs interface to libWand
;;
;; Copyright (C) 2005 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: ffi, wand, ImageMagick
;;
;; This file is part of SXEmacs.
;;
;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Synched up with: Not in FSF
;;
;;; Commentary:
;;
;;; Code:

(require 'ffi)
(require 'ffi-libc)

;; this is our spine, barf if it does not exist
;; ImageMagick version 6.4.0 calls libWand `libMagickWand' so try the
;; old name first and don't error, fall back to the new name, barf if
;; that fails as well --SY.
(or (ffi-load-library "libWand")
    (ffi-load "libMagickWand"))



;;; Basics

;; types
(define-ffi-type MagickWand (pointer void))
(define-ffi-type MagickBooleanType long)
(define-ffi-type MagickStorageType unsigned-long)
(define-ffi-type MagickChannelType unsigned-long)

(defun Wand:Storage (value)
  (ffi-create-fo 'MagickStorageType value))
(defconst Wand:Storage-UndefinedPixel (Wand:Storage 0))
(defconst Wand:Storage-CharPixel (Wand:Storage 1))
(defconst Wand:Storage-ShortPixel (Wand:Storage 2))
(defconst Wand:Storage-IntegerPixel (Wand:Storage 3))
(defconst Wand:Storage-LongPixel (Wand:Storage 4))
(defconst Wand:Storage-FloatPixel (Wand:Storage 5))
(defconst Wand:Storage-DoublePixel (Wand:Storage 6))

(defun Wand:Channel (value)
  (ffi-create-fo 'MagickChannelType value))
(defconst Wand:Channel-UndefinedChannel (Wand:Channel #x0000))
(defconst Wand:Channel-RedChannel (Wand:Channel #x0001))
(defconst Wand:Channel-CyanChannel (Wand:Channel #x0001))
(defconst Wand:Channel-GrayChannel (Wand:Channel #x0001))
(defconst Wand:Channel-GreenChannel (Wand:Channel #x0002))
(defconst Wand:Channel-MagentaChannel (Wand:Channel #x0002))
(defconst Wand:Channel-BlueChannel (Wand:Channel #x0004))
(defconst Wand:Channel-YellowChannel (Wand:Channel #x0004))
(defconst Wand:Channel-AlphaChannel (Wand:Channel #x0008))
(defconst Wand:Channel-OpacityChannel (Wand:Channel #x0008))
(defconst Wand:Channel-BlackChannel (Wand:Channel #x0020))
(defconst Wand:Channel-IndexChannel (Wand:Channel #x0020))
(defconst Wand:Channel-AllChannel (Wand:Channel #x7fff))



;; Wand handlers
(defvar Wand:NewMagicWand
  (ffi-defun '(function MagickWand void)
             "NewMagickWand")
  "Initialise wand environment.")
(defun Wand:make-wand ()
  "Return a newly allocated MagickWand."
  (ffi-call-function Wand:NewMagicWand))

(defvar Wand:ClearMagickWand
  (ffi-defun '(function void MagickWand)
             "ClearMagickWand"))
(defun Wand:clear-wand (wand)
  "Clear all resources associated with the WAND.
This does not free the memory, i.e. @var{wand} can furtherly be used
as a context, see `Wand:delete-wand'."
  (ffi-call-function Wand:ClearMagickWand wand)
  t)

(defvar Wand:CloneMagickWand
  (ffi-defun '(function MagickWand MagickWand)
             "CloneMagickWand"))
(defun Wand:copy-wand (wand)
  "Return a cloned copy of WAND."
  (ffi-call-function Wand:CloneMagickWand wand))

(defvar Wand:DestroyMagickWand
  (ffi-defun '(function MagickWand MagickWand)
             "DestroyMagickWand"))
(defun Wand:delete-wand (wand)
  "Delete the WAND.
This frees all resources associated with the WAND.

WARNING: Do not use WAND after calling this function!"
  (ffi-call-function Wand:DestroyMagickWand wand)
  nil)


;; helper fun
(defun Wand:eval-MagickBoolean (val)
  "Return the elisp pendant of VAL (a MagickBooleanType)."
  (not (zerop (ffi-get val))))

(defvar Wand:IsMagickWand
  (ffi-defun '(function MagickBooleanType MagickWand)
             "IsMagickWand"))
(defun Wand:wandp (wand)
  "Return non-nil if WAND is a magick wand, nil otherwise."
  (Wand:eval-MagickBoolean (ffi-call-function Wand:IsMagickWand wand)))


;;; input/output
(defvar Wand:MagickReadImage
  (ffi-defun '(function MagickBooleanType MagickWand c-string)
             "MagickReadImage"))
(defun Wand:read-image (wand file)
  "Read FILE and associate it with WAND."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-readable-p fname)
      (error "File unreadable"))
    (unless (Wand:wandp wand)
      (wrong-type-argument 'Wand:wandp wand))

    (let ((fcname (ffi-create-fo 'c-string fname)))
      (Wand:eval-MagickBoolean
       (ffi-call-function Wand:MagickReadImage wand fcname)))))

(defvar Wand:MagickDisplayImage
  (ffi-defun '(function MagickBooleanType MagickWand c-string)
             "MagickDisplayImage"))
(defun Wand:display-image (wand)
  "Display the image associated with WAND."
  (let* ((x-server (apply 'format "%s:%s.%s"
                          (split-string (device-name (frame-device))
                          "-")))
         (xs (ffi-create-fo 'c-string x-server)))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickDisplayImage wand xs))))

;;MagickBooleanType MagickSetImageFormat(MagickWand *wand, const char *format)

(defvar Wand:MagickWriteImage
  (ffi-defun '(function MagickBooleanType MagickWand c-string)
             "MagickWriteImage"))
(defun Wand:write-image (wand file)
  "Write the image associated with WAND to FILE."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-writable-p fname)
      (error "File unwritable"))
    (unless (Wand:wandp wand)
      (wrong-type-argument 'Wand:wandp wand))

    (let ((fcname (ffi-create-fo 'c-string fname)))
      (Wand:eval-MagickBoolean
       (ffi-call-function Wand:MagickWriteImage wand fcname)))))

(defvar Wand:MagickGetImagePixels
  (ffi-defun '(function MagickBooleanType MagickWand
                        long long unsigned-long unsigned-long
                        c-string MagickStorageType (pointer int))
             "MagickGetImagePixels"))
(defun Wand:get-image-pixels-internal (wand
                                       from-width from-height
                                       delta-width delta-height)
  "Return a raw string of image pixel data (RGB triples)."
  (let ((x (ffi-create-fo 'long from-width))
        (y (ffi-create-fo 'long from-height))
        (cols (ffi-create-fo 'long delta-width))
        (rows (ffi-create-fo 'long delta-height))
        (map (ffi-create-fo 'c-string "RGB"))
        (storage Wand:Storage-CharPixel)
	(target (make-ffi-object 'c-data (* delta-width delta-height 3))))
    (when (Wand:eval-MagickBoolean
           (ffi-call-function Wand:MagickGetImagePixels
                              wand
                              x y cols rows
                              map storage target))
      (ffi-get target))))
(defun Wand:get-image-pixels (wand)
  "Return a raw string of image pixel data (RGB triples)."
  (let ((height (Wand:get-image-height wand))
        (width (Wand:get-image-width wand)))
    (Wand:get-image-pixels-internal wand 0 0 width height)))



;;; geometry and canvas size
(defvar Wand:MagickScaleImage
  (ffi-defun '(function MagickBooleanType MagickWand
                        unsigned-long unsigned-long)
             "MagickScaleImage"))
(defun Wand:scale-image (wand width height)
  "Scale the image in WAND to the dimensions WIDTHxHEIGHT."
  (unless (Wand:wandp wand)
    (wrong-type-argument 'Wand:wandp wand))
  (unless (integerp width)
    (wrong-type-argument 'integerp width))
  (unless (integerp height)
    (wrong-type-argument 'integerp height))

  (let ((rows (ffi-create-fo 'unsigned-long height))
        (cols (ffi-create-fo 'unsigned-long width)))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickScaleImage wand cols rows))))

(defvar Wand:MagickCropImage
  (ffi-defun '(function MagickBooleanType MagickWand
                        unsigned-long unsigned-long
                        unsigned-long unsigned-long)
             "MagickCropImage"))
(defun Wand:crop-image (wand x y dx dy)
  "Crop to the rectangle spanned at X and Y by width DX and
height DY in the image associated with WAND."
  (let ((atx (ffi-create-fo 'unsigned-long x))
        (aty (ffi-create-fo 'unsigned-long y))
        (wid (ffi-create-fo 'unsigned-long dx))
        (hei (ffi-create-fo 'unsigned-long dy)))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickCropImage wand wid hei atx aty))))

(defvar Wand:MagickFlipImage
  (ffi-defun '(function MagickBooleanType MagickWand)
             "MagickFlipImage"))
(defvar Wand:MagickFlopImage
  (ffi-defun '(function MagickBooleanType MagickWand)
             "MagickFlopImage"))
(defun Wand:flip-image (wand)
  "Mirror the image associated with WAND around the x-axis."
  (Wand:eval-MagickBoolean
   (ffi-call-function Wand:MagickFlipImage wand)))
(defun Wand:flop-image (wand)
  "Mirror the image associated with WAND around the y-axis."
  (Wand:eval-MagickBoolean
   (ffi-call-function Wand:MagickFlopImage wand)))

;;   MagickBooleanType MagickRollImage(MagickWand *wand,const long x,
;;     const unsigned long y)
(defvar Wand:MagickRollImage
  (ffi-defun '(function MagickBooleanType MagickWand long long)
             "MagickRollImage"))
(defun Wand:roll-image (wand x y)
  "Rolls (offsets) the image associated with WAND to an offset
of X and Y."
  (let ((xoff (ffi-create-fo 'long x))
        (yoff (ffi-create-fo 'long y)))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickRollImage wand xoff yoff))))


;;; image improvements and basic image properties
(defvar Wand:MagickContrastImage
  (ffi-defun '(function MagickBooleanType MagickWand MagickBooleanType)
             "MagickContrastImage"))
(defun Wand:increase-contrast-image (wand)
  "Increase the contrast of the image associated with WAND."
  (let ((sh (ffi-create-fo 'MagickBooleanType 1)))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickContrastImage wand sh))))
(defun Wand:decrease-contrast-image (wand)
  "Decrease the contrast of the image associated with WAND."
  (let ((sh (ffi-create-fo 'MagickBooleanType 0)))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickContrastImage wand sh))))

(defvar Wand:MagickDespeckleImage
  (ffi-defun '(function MagickBooleanType MagickWand)
             "MagickDespeckleImage"))
(defun Wand:despeckle-image (wand)
  "Reduce the speckle noise in the image associated with WAND."
  (Wand:eval-MagickBoolean
   (ffi-call-function Wand:MagickDespeckleImage wand)))

(defvar Wand:MagickEnhanceImage
  (ffi-defun '(function MagickBooleanType MagickWand)
             "MagickEnhanceImage"))
(defun Wand:enhance-image (wand)
  "Enhance the image associated with WAND."
  (Wand:eval-MagickBoolean
   (ffi-call-function Wand:MagickEnhanceImage wand)))

(defvar Wand:MagickEqualizeImage
  (ffi-defun '(function MagickBooleanType MagickWand)
             "MagickEqualizeImage"))
(defun Wand:equalize-image (wand)
  "Equalise the image associated with WAND."
  (Wand:eval-MagickBoolean
   (ffi-call-function Wand:MagickEqualizeImage wand)))

(defvar Wand:MagickNormalizeImage
  (ffi-defun '(function MagickBooleanType MagickWand)
             "MagickNormalizeImage"))
(defun Wand:normalize-image (wand)
  "Normalise the image associated with WAND."
  (Wand:eval-MagickBoolean
   (ffi-call-function Wand:MagickNormalizeImage wand)))


;;; effects
(defvar Wand:MagickCharcoalImage
  (ffi-defun '(function MagickBooleanType MagickWand double double)
             "MagickCharcoalImage"))
(defun Wand:charcoal-image (wand radius sigma)
  "Simulate a charcoal drawing of the image associated with WAND.
The RADIUS argument is a float and measured in pixels.
The SIGMA argument is a float and defines a derivation."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickCharcoalImage wand r s))))

(defvar Wand:MagickOilPaintImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickOilPaintImage"))
(defun Wand:oil-paint-image (wand radius)
  "Simulate oil-painting of image associated with WAND.
The RADIUS argument is a float and measured in pixels."
  (let ((r (ffi-create-fo 'double (float radius))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickOilPaintImage wand r))))

(defvar Wand:MagickEdgeImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickEdgeImage"))
(defun Wand:edge-image (wand radius)
  "Enhance the edges of the image associated with WAND.
The RADIUS argument is a float and measured in pixels."
  (let ((r (ffi-create-fo 'double (float radius))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickEdgeImage wand r))))

(defvar Wand:MagickEmbossImage
  (ffi-defun '(function MagickBooleanType MagickWand double double)
             "MagickEmbossImage"))
(defun Wand:emboss-image (wand radius sigma)
  "Emboss the image associated with WAND (a relief effect).
The RADIUS argument is a float and measured in pixels.
The SIGMA argument is a float and defines a derivation."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickEmbossImage wand r s))))

(defvar Wand:MagickWaveImage
  (ffi-defun '(function MagickBooleanType MagickWand double double)
             "MagickWaveImage"))
(defun Wand:wave-image (wand amplitude wavelength)
  "Create a ripple effect on the image associated with WAND.
The AMPLITUDE argument is a float and defines the how large
waves are.
The WAVELENGTH argument is a float and defines how often the
waves occur."
  (let ((r (ffi-create-fo 'double (float amplitude)))
        (s (ffi-create-fo 'double (float wavelength))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickWaveImage wand r s))))

(defvar Wand:MagickSwirlImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickSwirlImage"))
(defun Wand:swirl-image (wand degrees)
  "Swirl the image associated with WAND by DEGREES."
  (let ((d (ffi-create-fo 'double (float degrees))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickSwirlImage wand d))))



(defvar Wand:MagickPosterizeImage
  (ffi-defun '(function MagickBooleanType MagickWand
                        unsigned-long MagickBooleanType)
             "MagickPosterizeImage"))
(defun Wand:posterize-image (wand levels &optional ditherp)
  "Posterize the image associated with WAND,
that is quantise the range of used colours to at most LEVELS.
If optional argument DITHERP is non-`nil' use a dithering
effect to wipe hard contrasts."
  (let ((l (ffi-create-fo 'unsigned-long levels))
        (d (ffi-create-fo 'MagickBooleanType (if ditherp 1 0))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickPosterizeImage wand l d))))

(defvar Wand:MagickReduceNoiseImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickReduceNoiseImage"))
(defun Wand:reduce-noise-image (wand radius)
  "Reduce the noise in the image associated with WAND by RADIUS."
  (let ((r (ffi-create-fo 'double (float radius))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickReduceNoiseImage wand r))))



(defvar Wand:MagickGammaImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickGammaImage"))
(defun Wand:gamma-image (wand level)
  "Perform gamma correction on the image associated with WAND.
The argument LEVEL is a positive float, a value of 1.00 (read 100%)
is a no-op."
  (let ((r (ffi-create-fo 'double (float level))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickGammaImage wand r))))

(defvar Wand:MagickGammaImageChannel
  (ffi-defun '(function MagickBooleanType MagickWand
                        MagickChannelType double)
             "MagickGammaImageChannel"))
(defun Wand:gamma-image-channel (wand channel level)
  "Perform gamma correction on CHANNEL of LEVEL on the image associated with WAND."
  (let ((r (ffi-create-fo 'double (float level))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickGammaImage wand channel r))))


(defvar Wand:MagickMedianFilterImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickMedianFilterImage"))
(defun Wand:median-filter-image (wand radius)
  "Perform median normalisation of the pixels in the image associated with WAND."
  (let ((r (ffi-create-fo 'double (float radius))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickMedianFilterImage wand r))))

(defvar Wand:MagickSolarizeImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickSolarizeImage"))
(defun Wand:solarize-image (wand threshold)
  "Solarise the image associated with WAND."
  (let ((th (ffi-create-fo 'double (float threshold))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickSolarizeImage wand th))))

(defvar Wand:MagickModulateImage
  (ffi-defun '(function MagickBooleanType MagickWand double double double)
             "MagickModulateImage"))
(defun Wand:modulate-image (wand brightness saturation hue)
  "Tweak the image associated with WAND."
  (let ((b (ffi-create-fo 'double (float brightness)))
        (s (ffi-create-fo 'double (float saturation)))
        (h (ffi-create-fo 'double (float hue))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickModulateImage wand b s h))))

(defvar Wand:MagickThresholdImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickThresholdImage"))
(defun Wand:threshold-image (wand threshold)
  "Separate a two-color high contrast image."
  (let ((th (ffi-create-fo 'double (float threshold))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickThresholdImage wand th))))

(defvar Wand:MagickThresholdImageChannel
  (ffi-defun '(function MagickBooleanType MagickWand
                        MagickChannelType double)
             "MagickThresholdImageChannel"))
(defun Wand:threshold-image-channel (wand channel threshold)
  "Separate a two-color high contrast image on CHANNEL."
  (let ((th (ffi-create-fo 'double (float threshold))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickThresholdImageChannel wand channel th))))

(defvar Wand:MagickWhiteThresholdImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickWhiteThresholdImage"))
(defun Wand:white-threshold-image (wand threshold)
  "Separate a two-color high contrast image."
  (let ((th (ffi-create-fo 'double (float threshold))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickWhiteThresholdImage wand th))))


;; blurs
(defvar Wand:MagickBlurImage
  (ffi-defun '(function MagickBooleanType MagickWand double double)
             "MagickBlurImage"))
(defun Wand:blur-image (wand radius sigma)
  "Blur the image associated with WAND.
The RADIUS argument is a float and measured in pixels.
The SIGMA argument is a float and defines a derivation."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickBlurImage wand r s))))

(defvar Wand:MagickBlurImageChannel
  (ffi-defun '(function MagickBooleanType
                        MagickWand MagickChannelType double double)
             "MagickBlurImageChannel"))
(defun Wand:blur-image-channel (wand channel radius sigma)
  "Blur CHANNEL in the image associated with WAND by RADIUS
pixels with derivation SIGMA."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickBlurImageChannel wand channel r s))))

(defvar Wand:MagickGaussianBlurImage
  (ffi-defun '(function MagickBooleanType MagickWand double double)
             "MagickGaussianBlurImage"))
(defun Wand:gaussian-blur-image (wand radius sigma)
  "Blur the image associated with WAND.
The RADIUS argument is a float and measured in pixels.
The SIGMA argument is a float and defines a derivation."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickGaussianBlurImage wand r s))))

(defvar Wand:MagickGaussianBlurImageChannel
  (ffi-defun '(function MagickBooleanType
                        MagickWand MagickChannelType double double)
             "MagickGaussianBlurImageChannel"))
(defun Wand:gaussian-blur-image-channel (wand channel radius sigma)
  "Blur CHANNEL in the image associated with WAND by RADIUS
pixels with derivation SIGMA."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickGaussianBlurImageChannel wand channel r s))))

(defvar Wand:MagickMotionBlurImage
  (ffi-defun '(function MagickBooleanType MagickWand double double double)
             "MagickMotionBlurImage"))
(defun Wand:motion-blur-image (wand radius sigma angle)
  "Blur the image associated with WAND.
The RADIUS argument is a float and measured in pixels.
The SIGMA argument is a float and defines a derivation.
The ANGLE argument is a float and measured in degrees."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma)))
        (a (ffi-create-fo 'double (float angle))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickMotionBlurImage wand r s a))))

(defvar Wand:MagickRadialBlurImage
  (ffi-defun '(function MagickBooleanType MagickWand double)
             "MagickRadialBlurImage"))
(defun Wand:radial-blur-image (wand angle)
  "Blur the image associated with WAND.
The ANGLE argument is a float and measured in degrees."
  (let ((a (ffi-create-fo 'double (float angle))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickRadialBlurImage wand a))))


(defvar Wand:MagickSharpenImage
  (ffi-defun '(function MagickBooleanType MagickWand double double)
             "MagickSharpenImage"))
(defun Wand:sharpen-image (wand radius sigma)
  "Sharpen the image associated with WAND.
The RADIUS argument is a float and measured in pixels.
The SIGMA argument is a float and defines a derivation."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickSharpenImage wand r s))))

(defvar Wand:MagickSharpenImageChannel
  (ffi-defun '(function MagickBooleanType
                        MagickWand MagickChannelType double double)
             "MagickSharpenImageChannel"))
(defun Wand:sharpen-image-channel (wand channel radius sigma)
  "Sharpen CHANNEL in the image associated with WAND by RADIUS
pixels with derivation SIGMA."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickSharpenImageChannel wand channel r s))))

;;   MagickBooleanType MagickUnsharpMaskImage(MagickWand *wand,
;;     const double radius,const double sigma,const double amount,
;;     const double threshold)
(defvar Wand:MagickUnsharpMaskImage
  (ffi-defun '(function MagickBooleanType
                        MagickWand double double double double)
             "MagickUnsharpMaskImage"))
(defun Wand:unsharp-mask-image (wand radius sigma amount threshold)
  "Sharpen the image associated with WAND using an unsharp mask.
The unsharp mask is defined by RADIUS and SIGMA.
The strength of sharpening is controlled by AMOUNT and THRESHOLD."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma)))
        (a (ffi-create-fo 'double (float amount)))
        (th (ffi-create-fo 'double (float threshold))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickUnsharpMaskImage
                        wand r s a th))))

(defvar Wand:MagickUnsharpMaskImageChannel
  (ffi-defun '(function MagickBooleanType
                        MagickWand MagickChannelType
                        double double double double)
             "MagickUnsharpMaskImageChannel"))
(defun Wand:unsharp-mask-image-channel (wand
                                        channel radius sigma
                                        amount threshold)
  "Sharpen CHANNEL in the image associated with WAND with an unsharp mask
defined by RADIUS and SIGMA.  The strength of sharpening is controlled
by AMOUNT and THRESHOLD."
  (let ((r (ffi-create-fo 'double (float radius)))
        (s (ffi-create-fo 'double (float sigma)))
        (a (ffi-create-fo 'double (float amount)))
        (th (ffi-create-fo 'double (float threshold))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickUnsharpMaskImageChannel
                        wand channel r s a th))))


(defvar Wand:MagickNegateImage
  (ffi-defun '(function MagickBooleanType MagickWand MagickBooleanType)
             "MagickNegateImage"))
(defun Wand:negate-image (wand &optional greyp)
  "Perform negation on the image associated with WAND."
  (let ((g (ffi-create-fo 'MagickBooleanType (if greyp 1 0))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickNegateImage wand g))))

(defvar Wand:MagickNegateImageChannel
  (ffi-defun '(function MagickBooleanType MagickWand
                        MagickChannelType MagickBooleanType)
             "MagickNegateImageChannel"))
(defun Wand:negate-image-channel (wand channel &optional greyp)
  "Perform negation of CHANNEL on the image associated with WAND."
  (let ((g (ffi-create-fo 'MagickBooleanType (if greyp 1 0))))
    (Wand:eval-MagickBoolean
     (ffi-call-function Wand:MagickNegateImage wand channel g))))



(defvar Wand:MagickGetImageHeight
  (ffi-defun '(function unsigned-long MagickWand)
             "MagickGetImageHeight"))
(defun Wand:get-image-height (wand)
  "Return the height of the image in WAND in pixels."
  (ffi-get
   (ffi-call-function Wand:MagickGetImageHeight wand)))

(defvar Wand:MagickGetImageWidth
  (ffi-defun '(function unsigned-long MagickWand)
             "MagickGetImageWidth"))
(defun Wand:get-image-width (wand)
  "Return the width of the image in WAND in pixels."
  (ffi-get
   (ffi-call-function Wand:MagickGetImageWidth wand)))



;; I wonder if we actually need this, Wand-API documentation says
;; yeah, but I've seen gazillions of code snippets not using it
;; -hroptatyr
(ignore-errors
  (defvar Wand:MagickWandGenesis
    (ffi-defun '(function void void)
               "MagickWandGenesis"))
  (defvar Wand:MagickWandTerminus
    (ffi-defun '(function void void)
               "MagickWandTerminus")))


;;; higher level API
(defun Wand:show-image-file-here (file)
  "Insert a glyph with the image from FILE at current point,
scale image to fit the buffer window if necessary."
  (interactive "fImage file: ")
  (let ((w (Wand:make-wand))
        (maxw (window-pixel-width))
        (maxh (window-pixel-height)))
    (Wand:read-image w (expand-file-name file))
    (let* ((width (Wand:get-image-width w))
           (height (Wand:get-image-height w))
           (prop (/ (float width) (float height)))
           rescale)

      ;; maybe rescale
      (when (< maxw width)
        (setq width maxw
              height (round (/ maxw prop))
              rescale t))
      (when (< maxh height)
        (setq width (round (* maxh prop))
              height maxh
              rescale t))

      (when rescale
        (message "Rescale to %dx%d" width height)
        (Wand:scale-image w width height))

      (let* ((pixels (Wand:get-image-pixels w))
             (imagei (make-image-instance
                      (vector 'rawrgb
                              :data pixels
                              :pixel-width width
                              :pixel-height height)))
             (glyph (make-glyph imagei))
             (p (point))
             (extent (make-extent p p)))
        (set-extent-end-glyph extent glyph)))
    (Wand:delete-wand w)
    t))

;;;###autoload
(defun Wand:show-image-file (file)
  "Insert a glyph with the image from FILE in a dedicated buffer,
scale image to fit the buffer window if necessary."
  (interactive "fImage file: ")
  (let ((buf (get-buffer-create
	      (concat "*Wand:image::" (file-name-nondirectory file) "*"))))
    (with-current-buffer buf
      (erase-buffer)
      (mapc #'delete-extent
	    (mapcar-extents #'identity
			    nil nil (point-min) (point-max)
			    'all-extents-closed-open))
      (Wand:show-image-file-here file))
    (switch-to-buffer buf t)
    (Wand-mode)))

(defvar Wand-mode-map (make-sparse-keymap)
  "Keymap for Wand mode.")

(define-key Wand-mode-map [q] #'Wand-mode-quit)

(defun Wand-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map Wand-mode-map)
  (setq mode-name "Wand")
  (setq major-mode 'Wand-mode)
  (setq buffer-read-only t)
  (run-hooks 'Wand-mode-hook))

(defun Wand-mode-quit ()
  (interactive)
  (kill-buffer (current-buffer)))


(provide 'ffi-wand)

;; now initialise the environment
(when-boundp 'Wand:MagickWandGenesis
  (ffi-call-function Wand:MagickWandGenesis))

(run-hooks 'ffi-wand-after-load-hook)


;;; ffi-wand.el ends here
