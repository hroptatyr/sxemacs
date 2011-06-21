;;; ffi-wand.el --- SXEmacs interface to libWand.
;;
;;{{{ Copyright (C) 2005 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;;         Zajcev Evgeny <lg@sxemacs.org>
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
;;}}}
;;
;;; Synched up with: Not in FSF
;;
;;{{{ Commentary:
;;
;;  To use `Wand-display' with `C-x C-f' add:
;;
;;    (Wand-find-file-enable)
;;
;;  to your init.el
;;}}}
;;{{{ BUGS:
;;
;; - HUGE memory leaks, but it looks like SXEmacs glyph caching
;;   mechanism eats memory, not wand.
;;     Hmm it might not be an issue any more, i've got no memory
;;     leaks on MacOS processing huge ammount of large images.
;;     --lg (26nov2009)
;;
;; - When saving in some formats like "HTML" ImageMagick core dumps,
;;   so be careful.  Need some assistance from IM developers to solve
;;   this problem.
;;
;; 
;;}}}
;;; Code:

;;{{{ Initialisation

(eval-when-compile
  (globally-declare-boundp
   '(operations-list undo-list buffer-file-name image-wand preview-wand
                     preview-region preview-extent
                     find-file-magic-files-alist)))

(require 'ffi)
(require 'wid-edit)

(defvar Wand-ffio-as-image-data
  (valid-instantiator-p
   (vector 'rawrgb :data (make-ffi-object 'pointer)
           :pixel-width 2 :pixel-height 2) 'image))

(defvar Wand-GM-p nil
  "Non-nil if using GraphicsMagick.")

;; this is our spine, barf if it does not exist
;; ImageMagick version 6.4.0 calls libWand `libMagickWand' so try the
;; old name first and don't error, fall back to the new name, barf if
;; that fails as well --SY.
(or (ffi-load-library "libWand")
    (ffi-load-library "libMagickWand")
    (and (ffi-load "libGraphicsMagickWand")
         (setq Wand-GM-p t)))

;;}}}
;;{{{ [+] FFI for MagickWand
;;{{{  `-- Data types

(define-ffi-type MagickBooleanType long)
(define-ffi-translator-to-foreign MagickBooleanType
  (if value 1 0))
(define-ffi-translator-from-foreign MagickBooleanType
  (not (zerop value)))

(define-ffi-struct MagickWand-private
  (id unsigned-long)
  (name (array char 4096))
  (exception pointer)
  (image-info pointer)
  (quantize-info pointer)
  (images pointer)
  (active MagickBooleanType)
  (pend MagickBooleanType)
  (debug MagickBooleanType)
  (signature unsigned-long))

(define-ffi-type MagickStatusType unsigned-int)
(define-ffi-struct MagickInfo
  (name pointer)
  (description pointer)
  (version pointer)
  (note pointer)
  (module pointer)

  (image-info pointer)
  (decoder pointer)
  (encoder pointer)

  (magick pointer)                      ; IsImageFormatHandler
  (client-date pointer)

  (adjoin MagickBooleanType)
  (raw MagickBooleanType)
  (endian_support MagickBooleanType)
  (blob_support MagickBooleanType)
  (seekable_stream MagickBooleanType)
  (thread-support MagickStatusType)
  (stealth MagickBooleanType)

  ;; deprecated, use GetMagickInfoList()
  (previous pointer)
  (next pointer)

  (signature unsigned-long))

(define-ffi-enum MagickExceptionType
  :UndefinedException
  :WarningException       = 300
  :ResourceLimitWarning   = :WarningException
  :TypeWarning            = 305
  :OptionWarning          = 310
  :DelegateWarning        = 315
  :MissingDelegateWarning = 320
  :CorruptImageWarning    = 325
  :FileOpenWarning        = 330
  :BlobWarning            = 335
  :StreamWarning          = 340
  :CacheWarning           = 345
  :CoderWarning           = 350
  :ModuleWarning          = 355
  :DrawWarning            = 360
  :ImageWarning           = 365
  :WandWarning            = 370
  :RandomWarning          = 375
  :XServerWarning         = 380
  :MonitorWarning         = 385
  :RegistryWarning        = 390
  :ConfigureWarning       = 395
  :ErrorException         = 400
  :ResourceLimitError     = :ErrorException
  :TypeError              = 405
  :OptionError            = 410
  :DelegateError          = 415
  :MissingDelegateError   = 420
  :CorruptImageError      = 425
  :FileOpenError          = 430
  :BlobError              = 435
  :StreamError            = 440
  :CacheError             = 445
  :CoderError             = 450
  :ModuleError            = 455
  :DrawError              = 460
  :ImageError             = 465
  :WandError              = 470
  :RandomError            = 475
  :XServerError           = 480
  :MonitorError           = 485
  :RegistryError          = 490
  :ConfigureError         = 495
  :FatalErrorException    = 700
  :ResourceLimitFatalError = :FatalErrorException
  :TypeFatalError         = 705
  :OptionFatalError       = 710
  :DelegateFatalError     = 715
  :MissingDelegateFatalError = 720
  :CorruptImageFatalError = 725
  :FileOpenFatalError     = 730
  :BlobFatalError         = 735
  :StreamFatalError       = 740
  :CacheFatalError        = 745
  :CoderFatalError        = 750
  :ModuleFatalError       = 755
  :DrawFatalError         = 760
  :ImageFatalError        = 765
  :WandFatalError         = 770
  :RandomFatalError       = 775
  :XServerFatalError      = 780
  :MonitorFatalError      = 785
  :RegistryFatalError     = 790
  :ConfigureFatalError    = 795)

(define-ffi-struct MagickExceptionInfo
  (severity MagickExceptionType)
  (error_number int)
  (reason pointer)
  (description pointer)
  (exceptions pointer)
  (relinquish MagickBooleanType)
  (semaphore pointer)
  (signature unsigned-long))

;; types
(define-ffi-type MagickWand (pointer void))
(define-ffi-type DrawingWand (pointer void))
(define-ffi-type PixelWand (pointer void))

(define-ffi-struct PointInfo
  (x double) (y double))

(define-ffi-enum MagickStorageType
  :undefined-pixel
  :char-pixel = (if Wand-GM-p 0 1)
  :short-pixel
  :integer-pixel
  :long-pixel
  :float-pixel
  :double-pixel)

(define-ffi-enum MagickChannelType
  :undefined-channel
  :red-channel       = #x0001
  :cyan-channel      = :red-channel
  :gray-channel      = :red-channel
  :green-channel     = #x0002
  :magenta-channel   = :green-channel
  :blue-channel      = #x0004
  :yellow-channel    = :blue-channel
  :alpha-channel     = #x0008
  :opacity-channel   = :alpha-channel
  :black-channel     = #x0020
  :index-channel     = :black-channel
  :all-channel       = #x7fff)

(define-ffi-enum WandCompositeOperator
  :UndefinedCompositeOp
  :NoCompositeOp
  :AddCompositeOp
  :AtopCompositeOp
  :BlendCompositeOp
  :BumpmapCompositeOp
  :ChangeMaskCompositeOp
  :ClearCompositeOp
  :ColorBurnCompositeOp
  :ColorDodgeCompositeOp
  :ColorizeCompositeOp
  :CopyBlackCompositeOp
  :CopyBlueCompositeOp
  :CopyCompositeOp
  :CopyCyanCompositeOp
  :CopyGreenCompositeOp
  :CopyMagentaCompositeOp
  :CopyOpacityCompositeOp
  :CopyRedCompositeOp
  :CopyYellowCompositeOp
  :DarkenCompositeOp
  :DstAtopCompositeOp
  :DstCompositeOp
  :DstInCompositeOp
  :DstOutCompositeOp
  :DstOverCompositeOp
  :DifferenceCompositeOp
  :DisplaceCompositeOp
  :DissolveCompositeOp
  :ExclusionCompositeOp
  :HardLightCompositeOp
  :HueCompositeOp
  :InCompositeOp
  :LightenCompositeOp
  :LinearLightCompositeOp
  :LuminizeCompositeOp
  :MinusCompositeOp
  :ModulateCompositeOp
  :MultiplyCompositeOp
  :OutCompositeOp
  :OverCompositeOp
  :OverlayCompositeOp
  :PlusCompositeOp
  :ReplaceCompositeOp
  :SaturateCompositeOp
  :ScreenCompositeOp
  :SoftLightCompositeOp
  :SrcAtopCompositeOp
  :SrcCompositeOp
  :SrcInCompositeOp
  :SrcOutCompositeOp
  :SrcOverCompositeOp
  :SubtractCompositeOp
  :ThresholdCompositeOp
  :XorCompositeOp
  :DivideCompositeOp)

(define-ffi-enum FillRule
  :UndefinedRule
  :EvenOddRule
  :NonZeroRule)

(define-ffi-enum PaintMethod
  :UndefinedMethod
  :PointMethod
  :ReplaceMethod
  :FloodfillMethod
  :FillToBorderMethod
  :ResetMethod)

(define-ffi-enum MagickAlphaType
  :UndefinedAlphaChannel
  :ActivateAlphaChannel
  :DeactivateAlphaChannel
  :ResetAlphaChannel
  :SetAlphaChannel)

(define-ffi-enum MagickNoiseType
  :UndefinedNoise
  :UniformNoise
  :GaussianNoise
  :MultiplicativeGaussianNoise
  :ImpulseNoise
  :LaplacianNoise
  :PoissonNoise
  :RandomNoise)

(define-ffi-enum MagickFilterType
  :UndefinedFilter
  :PointFilter
  :BoxFilter
  :TriangleFilter
  :HermiteFilter
  :HanningFilter
  :HammingFilter
  :BlackmanFilter
  :GaussianFilter
  :QuadraticFilter
  :CubicFilter
  :CatromFilter
  :MitchellFilter
  :LanczosFilter
  :BesselFilter
  :SincFilter
  :KaiserFilter
  :WelshFilter
  :ParzenFilter
  :LagrangeFilter
  :BohmanFilter
  :BartlettFilter
  :SentinelFilter)

(define-ffi-enum MagickColorspaceType
  :UndefinedColorspace
  :RGBColorspace
  :GRAYColorspace
  :TransparentColorspace
  :OHTAColorspace
  :LabColorspace
  :XYZColorspace
  :YCbCrColorspace
  :YCCColorspace
  :YIQColorspace
  :YPbPrColorspace
  :YUVColorspace
  :CMYKColorspace
  :sRGBColorspace
  :HSBColorspace
  :HSLColorspace
  :HWBColorspace
  :Rec601LumaColorspace
  :Rec601YCbCrColorspace
  :Rec709LumaColorspace
  :Rec709YCbCrColorspace
  :LogColorspace
  :CMYColorspace)

(define-ffi-enum MagickAlignType
  :UndefinedAlign
  :LeftAlign
  :CenterAlign
  :RightAlign)

(define-ffi-enum MagickDecorationType
  :UndefinedDecoration
  :NoDecoration
  :UnderlineDecoration
  :OverlineDecoration
  :LineThroughDecoration)

(define-ffi-enum MagickGravityType
  :UndefinedGravity
  :ForgetGravity = :UndefinedGravity
  :NorthWestGravity
  :NorthGravity
  :NorthEastGravity
  :WestGravity
  :CenterGravity
  :EastGravity
  :SouthWestGravity
  :SouthGravity
  :SouthEastGravity
  :StaticGravity)

(define-ffi-enum MagickStretchType
  :UndefinedStretch
  :NormalStretch
  :UltraCondensedStretch
  :ExtraCondensedStretch
  :CondensedStretch
  :SemiCondensedStretch
  :SemiExpandedStretch
  :ExpandedStretch
  :ExtraExpandedStretch
  :UltraExpandedStretch
  :AnyStretch)

(define-ffi-enum MagickStyleType
  :UndefinedStyle
  :NormalStyle
  :ItalicStyle
  :ObliqueStyle
  :AnyStyle)

(defstruct wand-font
  family
  size
  weight
  stretch
  style)

;;}}}
;;{{{  `-- Wand:version

(cffi:defcfun ("GetMagickVersion" Wand:GetMagickVersion) c-string
  (n (pointer unsigned-long)))

(defun Wand:version ()
  "Return Image Magick version string."
  (let ((n (make-ffi-object 'unsigned-long)))
    (Wand:GetMagickVersion (ffi-address-of n))))

;;}}}
;;{{{  `-- Mime Type operations

(cffi:defcfun ("DestroyString" Wand:DestroyString) (pointer char)
  (str pointer))

(cffi:defcfun ("MagickToMime" Wand:MagickToMime) (pointer char)
  (fmt c-string))

(defun wand-format-mime-type (format)
  "Return mime-type for the FORMAT."
  (let ((mt (Wand:MagickToMime format)))
    (unless (ffi-null-p mt)
      (unwind-protect
          (ffi-get mt :type 'c-string)
        (Wand:DestroyString mt)))))

(defun Wand:image-mime-type (wand)
  "Return mime-type for the WAND."
  (wand-format-mime-type (Wand:image-format wand)))

;;}}}
;;{{{  `-- MagickWand operations

;; Return a newly allocated MagickWand.
(cffi:defcfun ("NewMagickWand" Wand:make-wand) MagickWand)

;; Clear all resources associated with the WAND.
;; This does not free the memory, i.e. @var{wand} can furtherly be used
;; as a context, see `Wand:delete-wand'."
(cffi:defcfun ("ClearMagickWand" Wand:clear-wand) void
  (wand MagickWand))

;; Return a cloned copy of WAND.
(cffi:defcfun ("CloneMagickWand" Wand:copy-wand) MagickWand
  (wand MagickWand))

;; Gets the image at the current image index.
(cffi:defcfun ("MagickGetImage" Wand:get-image) MagickWand
  (wand MagickWand))

;; Delete the WAND.
;; This frees all resources associated with the WAND.
;; WARNING: Do not use WAND after calling this function!
(cffi:defcfun ("DestroyMagickWand" Wand:delete-wand) void
  (wand MagickWand))

;; Return non-nil if WAND is a magick wand, nil otherwise.
(cffi:defcfun ("IsMagickWand" Wand:wandp) MagickBooleanType
  (w MagickWand))

(defmacro Wand-with-wand (wand &rest forms)
  "With allocated WAND do FORMS."
  `(let ((,wand (Wand:make-wand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:delete-wand ,wand))))
(put 'Wand-with-wand 'lisp-indent-function 'defun)

(cffi:defcfun ("MagickNewImage" Wand:make-image) MagickBooleanType
  "Adds a blank image canvas to the WAND."
  (wand MagickWand) (cols unsigned-long) (rows unsigned-long)
  (pixel PixelWand))

;; Extracts a region of the image and returns it as a a new wand.
(cffi:defcfun ("MagickGetImageRegion" Wand:image-region) MagickWand
  (wand MagickWand) (dx unsigned-long) (dy unsigned-long)
  (x unsigned-long) (y unsigned-long))

;; MagickIdentifyImage() identifies an image by printing its
;; attributes to the file. Attributes include the image width, height,
;; size, and others.
(cffi:defcfun ("MagickIdentifyImage" Wand:MagickIdentifyImage) pointer
  (wand MagickWand))

(defun Wand:identify-image (wand)
  "Return info about the image stored in WAND."
  (let ((ii (Wand:MagickIdentifyImage wand)))
    (unwind-protect
        (ffi-get ii :type 'c-string)
      (Wand:RelinquishMemory ii))))

;; MagickResetImagePage() resets the Wand page canvas and position.
(cffi:defcfun ("MagickResetImagePage" Wand:MagickResetImagePage)
  MagickBooleanType
  (wand MagickWand) (geom c-string))

(defun Wand:reset-image-page (wand &optional geometry)
  "Reset the WAND page canvas and position to GEOMETRY.
If GEOMETRY is ommited then 0x0+0+0 is used."
  (Wand:MagickResetImagePage wand (or geometry "0x0+0+0")))

;; Magick Properties
(cffi:defcfun ("GetMagickProperty" Wand:GetMagickProperty) pointer
  (info pointer) (image pointer) (property c-string))

(defun Wand:get-magick-property (wand prop)
  "From WAND get magick property PROP.
PROP can be one of: `base', `channels', `colorspace', `depth',
`directory', `extension', `height', `input', `magick', `name',
`page', `size', `width', `xresolution', `yresolution'."
  (when (member prop '("group" "kurtosis" "max" "mean"
                       "min" "output" "scene" "skewness"
                       "standard-deviation" "standard_deviation"
                       "unique" "zero"))
    (error "Unsupported magick property" prop))
  (let ((rt (Wand:GetMagickProperty
             (ffi-null-pointer) (MagickWand-private->images wand)
             prop)))
    (unless (ffi-null-p rt)
      (ffi-get rt :type 'c-string))))

(defun Wand:image-orig-width (wand)
  "Return original width of the image associated with WAND."
  (string-to-int (Wand:get-magick-property wand "width")))

(defun Wand:image-orig-height (wand)
  "Return original height of the image associated with WAND."
  (string-to-int (Wand:get-magick-property wand "height")))

;;}}}
;;{{{  `-- Images list operations

(cffi:defcfun ("MagickGetNumberImages" Wand:images-num) unsigned-long
  (wand MagickWand))

(cffi:defcfun ("MagickHasNextImage" Wand:has-next-image) MagickBooleanType
  (wand MagickWand))

(cffi:defcfun ("MagickNextImage" Wand:next-image) MagickBooleanType
  (wand MagickWand))

(cffi:defcfun ("MagickHasPreviousImage" Wand:has-prev-image) MagickBooleanType
  (wand MagickWand))

(cffi:defcfun ("MagickPreviousImage" Wand:prev-image) MagickBooleanType
  (wand MagickWand))

(cffi:defcfun ("MagickGetIteratorIndex" Wand:iterator-index) long
  (wand MagickWand))

(cffi:defcfun ("MagickSetIteratorIndex" Wand:MagickSetIteratorIndex)
  MagickBooleanType
  (wand MagickWand) (idx long))

(defsetf Wand:iterator-index (w) (idx)
  `(Wand:MagickSetIteratorIndex ,w ,idx))

(cffi:defcfun ("MagickSetFirstIterator" Wand:set-first-iterator) void
  (wand MagickWand))

(cffi:defcfun ("MagickSetLastIterator" Wand:set-last-iterator) void
  (wand MagickWand))

;;}}}
;;{{{  `-- Image data input/output

(cffi:defcfun ("MagickReadImage" Wand:MagickReadImage) MagickBooleanType
  (wand MagickWand)
  (file c-string))

(defun Wand:read-image (wand file)
  "Read FILE and associate it with WAND."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-readable-p fname)
      (error "File unreadable %s" fname))
    (unless (Wand:wandp wand)
      (wrong-type-argument 'Wand:wandp wand))
    (Wand:MagickReadImage wand fname)))

(cffi:defcfun ("MagickReadImageBlob" Wand:MagickReadImageBlob) MagickBooleanType
  (wand MagickWand)
  (blob pointer)
  (len unsigned-int))

(defun Wand:read-image-blob (wand blob)
  "Read image from BLOB and associate it with WAND."
  (let* ((lb (length blob))
         (fob (make-ffi-object 'pointer (1+ lb))))
    (ffi-store fob 0 'c-string blob)
    (Wand:MagickReadImageBlob wand fob lb)))

(cffi:defcfun ("MagickDisplayImage" Wand:MagickDisplayImage) MagickBooleanType
  (wand MagickWand)
  (xserver c-string))

(defun Wand:display-image (wand)
  "Display the image associated with WAND.
WARNING: this will block untill display exits, so be careful."
  (let ((x-server (device-connection (default-x-device))))
    (Wand:MagickDisplayImage wand x-server)))

(cffi:defcfun ("MagickGetImageBlob" Wand:GetImageBlob) pointer
  (wand MagickWand)
  (len (pointer unsigned-int)))

(cffi:defcfun ("MagickRelinquishMemory" Wand:RelinquishMemory) pointer
  (resource pointer))

(defun Wand:image-blob (wand)
  "Return WAND's direct image data according to format.
Use \(setf \(Wand:image-format w\) FMT\) to set format."
  (let* ((len (make-ffi-object 'unsigned-int))
         (data (Wand:GetImageBlob wand (ffi-address-of len))))
    (unwind-protect
        (ffi-get data :type (cons 'c-data (ffi-get len)))
      (Wand:RelinquishMemory data))))

(cffi:defcfun ("MagickWriteImage" Wand:MagickWriteImage) MagickBooleanType
  (wand MagickWand)
  (file c-string))
(defun Wand:write-image (wand file)
  "Write the image associated with WAND to FILE."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-writable-p fname)
      (error "File unwritable %s" fname))
    (unless (Wand:wandp wand)
      (wrong-type-argument 'Wand:wandp wand))
    (Wand:MagickWriteImage wand fname)))

;;}}}
;;{{{  `-- Image format operations

(cffi:defcfun ("MagickQueryFormats"
               Wand:QueryFormats) (pointer c-string)
               (pattern c-string)
               (num-formats (pointer unsigned-long)))

(defun Wand:query-formats (pattern)
  "Return list of supported formats that match PATTERN.
Use \"*\" to query all available formats."
  (let* ((nf (make-ffi-object 'unsigned-long))
         (fmts (Wand:QueryFormats pattern (ffi-address-of nf))))
    (loop for n from 0 below (ffi-get nf)
      collect (ffi-get
               (ffi-get fmts :off (* n (ffi-size-of-type 'pointer)))
               :type 'c-string))))

(cffi:defcfun ("MagickGetFormat" Wand:wand-format) c-string
  (wand MagickWand))
(cffi:defcfun ("MagickSetFormat" Wand:MagickSetFormat) MagickBooleanType
  (wand MagickWand) (format c-string))

(defsetf Wand:wand-format (w) (nfmt)
  `(Wand:MagickSetFormat ,w ,nfmt))

(cffi:defcfun ("MagickGetImageFormat" Wand:GetImageFormat) c-string
  (wand MagickWand))

(cffi:defcfun ("MagickSetImageFormat" Wand:SetImageFormat) MagickBooleanType
  (wand MagickWand)
  (format c-string))

(defun Wand:image-format (w)
  "Return format for the image hold by W.
Use \(setf \(Wand:image-format w\) FMT\) to set new one."
  (Wand:GetImageFormat w))

(defsetf Wand:image-format (w) (fmt)
  `(Wand:SetImageFormat ,w ,fmt))

(cffi:defcfun ("GetMagickInfo" Wand:GetMagickInfo) (pointer MagickInfo)
  (fmt c-string)
  (exception pointer))

(cffi:defcfun ("GetMagickInfoList" Wand:GetMagickInfoList)
  (pointer (pointer MagickInfo))
  (fmt c-string)
  (number-of-items (pointer unsigned-long))
  (exception pointer))

(cffi:defcfun ("GetMagickBlobSupport" Wand:GetMagickBlobSupport)
  MagickBooleanType
  (mi (pointer MagickInfo)))

(cffi:defcfun ("MagickGetImageColorspace" Wand:GetImageColorspace)
  MagickColorspaceType
  (wand MagickWand))

(cffi:defcfun ("MagickSetImageColorspace" Wand:SetImageColorspace)
  MagickBooleanType
  (wand MagickWand)
  (cst MagickColorspaceType))

;;}}}
;;{{{  `-- PixelWand operations

(cffi:defcfun ("NewPixelWand" Wand:NewPixelWand) PixelWand)
(cffi:defcfun ("DestroyPixelWand" Wand:DestroyPixelWand) PixelWand
  (pw PixelWand))

(defmacro Wand-with-pixel-wand (pw &rest forms)
  "With allocated pixel wand PW do FORMS."
  `(let ((,pw (Wand:NewPixelWand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:DestroyPixelWand ,pw))))
(put 'Wand-with-pixel-wand 'lisp-indent-function 'defun)

(cffi:defcfun ("PixelGetHSL" Wand:PixelGetHSL) void
  (pw PixelWand) (hue (pointer double)) (saturation (pointer double))
  (lightness (pointer double)))

(cffi:defcfun ("PixelSetHSL" Wand:PixelSetHSL) void
  (pw PixelWand) (hue double) (saturation double) (lightness double))

(defun Wand:pixel-hsl (pw)
  "Return HSL for pixel wand PW."
  (let ((hue (make-ffi-object 'double))
        (sat (make-ffi-object 'double))
        (light (make-ffi-object 'double)))
    (Wand:PixelGetHSL pw (ffi-address-of hue) (ffi-address-of sat)
                      (ffi-address-of light))
    (mapcar #'ffi-get (list hue sat light))))

(defsetf Wand:pixel-hsl (pw) (hsl)
  `(apply #'Wand:PixelSetHSL ,pw ,hsl))

(cffi:defcfun ("PixelGetRed" Wand:pixel-red) double
  (pw PixelWand))
(cffi:defcfun ("PixelGetGreen" Wand:pixel-green) double
  (pw PixelWand))
(cffi:defcfun ("PixelGetBlue" Wand:pixel-blue) double
  (pw PixelWand))

(cffi:defcfun ("PixelSetRed" Wand:PixelSetRed) void
  (pw pointer) (red double))
(cffi:defcfun ("PixelSetGreen" Wand:PixelSetGreen) void
  (pw pointer) (red double))
(cffi:defcfun ("PixelSetBlue" Wand:PixelSetBlue) void
  (pw pointer) (red double))

(defsetf Wand:pixel-red (pw) (r)
  `(Wand:PixelSetRed ,pw ,r))
(defsetf Wand:pixel-green (pw) (g)
  `(Wand:PixelSetGreen ,pw ,g))
(defsetf Wand:pixel-blue (pw) (b)
  `(Wand:PixelSetBlue ,pw ,b))

(defun Wand:pixel-rgb-components (pw)
  "Return RGB components for pixel wand PW."
  (mapcar #'(lambda (c) (int (* (funcall c pw) 65535.0)))
          '(Wand:pixel-red Wand:pixel-green Wand:pixel-blue)))

(defsetf Wand:pixel-rgb-components (pw) (rgb)
  "For pixel wand PW set RGB components."
  `(mapcar* #'(lambda (sf c) (funcall sf ,pw (/ c 65535.0)))
            '(Wand:PixelSetRed Wand:PixelSetGreen Wand:PixelSetBlue)
            ,rgb))

;; PixelGetColorAsString() returns the color of the pixel wand as a
;; string.
(cffi:defcfun ("PixelGetColorAsString" Wand:pixel-color) c-string
  (pw pointer))

;; PixelSetColor() sets the color of the pixel wand with a string
;; (e.g. "blue", "#0000ff", "rgb(0,0,255)", "cmyk(100,100,100,10)",
;; etc.).
(cffi:defcfun ("PixelSetColor" Wand:PixelSetColor) MagickBooleanType
  (pw pointer)
  (color c-string))

(defsetf Wand:pixel-color (pw) (color)
  `(Wand:PixelSetColor ,pw ,color))

;; PixelGetAlpha() returns the normalized alpha color of the pixel
;; wand.
(cffi:defcfun ("PixelGetAlpha" Wand:pixel-alpha) double
  (pw pointer))

;; PixelSetAlpha() sets the normalized alpha color of the pixel wand.
;; The level of transparency: 1.0 is fully opaque and 0.0 is fully
;; transparent.
(cffi:defcfun ("PixelSetAlpha" Wand:PixelSetAlpha) void
  (pw pointer)
  (alpha double))

(defsetf Wand:pixel-alpha (pw) (alpha)
  `(Wand:PixelSetAlpha ,pw ,alpha))

;;}}}
;;{{{  `-- Image pixels operations

(cffi:defcfun ("MagickGetImagePixels" Wand:MagickGetImagePixels)
  MagickBooleanType
  (wand MagickWand)
  (from-width long)
  (from-height long)
  (delta-width unsigned-long)
  (delta-height unsigned-long)
  (map c-string)
  (storage MagickStorageType)
  (target (pointer int)))

(defun Wand:get-image-pixels-internal
  (wand img-type from-width from-height delta-width delta-height)
  "Return WAND's raw string of image pixel data (RGB triples).
FROM-WIDTH, FROM-HEIGHT, DELTA-WIDTH, DELTA-HEIGHT specifies region to
fetch data from."
  (let* ((tsz (ecase img-type (rawrgb 3) (rawrgba 4)))
         (mapn (ecase img-type (rawrgb "RGB") (rawrgba "RGBA")))
         (target (make-ffi-object 'c-data (* delta-width delta-height tsz))))
    (when (Wand:MagickGetImagePixels
           wand from-width from-height delta-width delta-height
           mapn :char-pixel target)
      (if Wand-ffio-as-image-data
          target
        (ffi-get target)))))

(defun Wand:get-image-pixels (wand)
  "Return WAND's raw string of image pixel data (RGB triples)."
  (Wand:get-image-pixels-internal
   wand 'rawrgb 0 0 (Wand:image-width wand) (Wand:image-height wand)))

(cffi:defcfun ("MagickSetImagePixels" Wand:MagickSetImagePixels)
  MagickBooleanType
  (wand MagickWand) (x-offset long) (y-offset long)
  (columns unsigned-long) (rows unsigned-long)
  (map c-string) (storage-type MagickStorageType)
  (pixels pointer))

(defun Wand:set-image-pixels-internal (wand x y width height pixels)
  (let ((stor (make-ffi-object 'c-data (* width height 3))))
    (ffi-set stor pixels)
    (Wand:MagickSetImagePixels
     wand x y width height "RGB" 'char-pixel stor)))

(defun Wand:pixels-extract-colors (ss &optional n)
  "Extract colors from SS string.
Return list of lists of N int elements representing RBG(A) values."
  (let ((cls (mapcar #'char-to-int (string-to-list ss)))
        (rls nil))
    (while cls
      (push (subseq cls 0 (or n 3)) rls)
      (setq cls (nthcdr (or n 3) cls)))
    (nreverse rls)))

(defun Wand:pixels-arrange-colors (cls)
  "Create pixels string from CLS.
CLS is list of lists of N int elements representing RBG(A) values."
  (mapconcat #'identity
             (mapcan #'(lambda (els)
                         (mapcar #'char-to-string
                                 (mapcar #'int-to-char els)))
                     cls)
             ""))

;; MagickConstituteImage() adds an image to the wand comprised of the
;; pixel data you supply. The pixel data must be in scanline order
;; top-to-bottom. The data can be char, short int, int, float, or
;; double. Float and double require the pixels to be normalized
;; [0..1], otherwise [0..Max], where Max is the maximum value the type
;; can accomodate (e.g. 255 for char). For example, to create a
;; 640x480 image from unsigned red-green-blue character data, use
(cffi:defcfun ("MagickConstituteImage" Wand:MagickConstituteImage)
  MagickBooleanType
  (wand MagickWand) (width unsigned-long) (height unsigned-long)
  (map c-string) (storage MagickStorageType) (pixels pointer))

;;}}}
;;{{{  `-- Image modification functions

(cffi:defcfun ("MagickThumbnailImage" Wand:thumbnail-image)
  MagickBooleanType
  (wand MagickWand) (width unsigned-long) (height unsigned-long))

(cffi:defcfun ("MagickRotateImage" Wand:RotateImage) MagickBooleanType
  (wand MagickWand) (background-pixel PixelWand) (degrees double))

;;Scale the image in WAND to the dimensions WIDTHxHEIGHT.
(cffi:defcfun ("MagickScaleImage" Wand:scale-image) MagickBooleanType
  (wand MagickWand) (width unsigned-long) (height unsigned-long))

;; Sample the image
(cffi:defcfun ("MagickSampleImage" Wand:sample-image) MagickBooleanType
  (wand MagickWand) (width unsigned-long) (height unsigned-long))

(cffi:defcfun ("MagickResizeImage" Wand:resize-image) MagickBooleanType
  (wand MagickWand) (width unsigned-long) (height unsigned-long)
  (filter MagickFilterType) (blur double))

(ignore-errors
  (cffi:defcfun ("MagickLiquidRescaleImage" Wand:liquid-rescale)
    MagickBooleanType
    (wand MagickWand) (width unsigned-long) (height unsigned-long)
    (delta-x double) (rigidity double)))

;; Crop to the rectangle spanned at X and Y by width DX and
;; height DY in the image associated with WAND."
(cffi:defcfun ("MagickCropImage" Wand:crop-image) MagickBooleanType
  (wand MagickWand) (width unsigned-long) (heigth unsigned-long)
  (x unsigned-long) (y unsigned-long))

;; MagickChopImage() removes a region of an image and collapses the
;; image to occupy the removed portion
(cffi:defcfun ("MagickChopImage" Wand:chop-image) MagickBooleanType
  (wand MagickWand) (width unsigned-long) (heigth unsigned-long)
  (x long) (y long))

(cffi:defcfun ("MagickFlipImage" Wand:flip-image) MagickBooleanType
  (wand MagickWand))
(cffi:defcfun ("MagickFlopImage" Wand:flop-image) MagickBooleanType
  (wand MagickWand))
;; Rolls (offsets) the image associated with WAND to an offset
;; of X and Y."
(cffi:defcfun ("MagickRollImage" Wand:roll-image) MagickBooleanType
  (wand MagickWand) (x long) (y long))

;; Composite one image COMPOSITE-WAND onto another WAND at the
;; specified offset X, Y, using composite operator COMPOSE.
(cffi:defcfun ("MagickCompositeImage" Wand:image-composite) MagickBooleanType
  (wand MagickWand) (composite-wand MagickWand) (compose WandCompositeOperator)
  (x long) (y long))

(cffi:defcfun ("MagickCompositeImageChannel" Wand:image-composite-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType) (region-wand MagickWand)
  (compose WandCompositeOperator) (x long) (y long))

;;; image improvements and basic image properties
(cffi:defcfun ("MagickContrastImage" Wand:MagickContrastImage)
  MagickBooleanType
  (wand MagickWand) (contrast MagickBooleanType))
(defun Wand:increase-contrast-image (wand)
  "Increase the contrast of the image associated with WAND."
  (Wand:MagickContrastImage wand t))
(defun Wand:decrease-contrast-image (wand)
  "Decrease the contrast of the image associated with WAND."
  (Wand:MagickContrastImage wand nil))

;; Reduce the speckle noise in the image associated with WAND.
(cffi:defcfun ("MagickDespeckleImage" Wand:despeckle-image) MagickBooleanType
  (wand MagickWand))
;; Enhance the image associated with WAND.
(cffi:defcfun ("MagickEnhanceImage" Wand:enhance-image) MagickBooleanType
  (wand MagickWand))
;; Equalise the image associated with WAND.
(cffi:defcfun ("MagickEqualizeImage" Wand:equalize-image) MagickBooleanType
  (wand MagickWand))
;; Normalise the image associated with WAND.
(cffi:defcfun ("MagickNormalizeImage" Wand:normalize-image) MagickBooleanType
  (wand MagickWand))

;;; Effects

(cffi:defcfun ("MagickColorizeImage" Wand:MagickColorizeImage)
  MagickBooleanType
  (w MagickWand) (color pointer) (opacity pointer))

;; Simulate a charcoal drawing of the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(cffi:defcfun ("MagickCharcoalImage" Wand:charcoal-image) MagickBooleanType
  (wand MagickWand) (radius double) (sigma double))

;; Simulate oil-painting of image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
(cffi:defcfun ("MagickOilPaintImage" Wand:oil-paint-image) MagickBooleanType
  (wand MagickWand) (radius double))

;; MagickSepiaToneImage() applies a special effect to the image,
;; similar to the effect achieved in a photo darkroom by sepia
;; toning. Threshold ranges from 0 to QuantumRange and is a measure of
;; the extent of the sepia toning. A threshold of 80 is a good
;; starting point for a reasonable tone.
(cffi:defcfun ("MagickSepiaToneImage" Wand:sepia-tone-image) MagickBooleanType
  (wand MagickWand) (threshold double))

;; MagickImplodeImage() creates a new image that is a copy of an
;; existing one with the image pixels "implode" by the specified
;; percentage. It allocates the memory necessary for the new Image
;; structure and returns a pointer to the new image.
(cffi:defcfun ("MagickImplodeImage" Wand:implode-image) MagickBooleanType
  (wand MagickWand) (radius double))

;; MagickVignetteImage() softens the edges of the image in vignette
;; style.
(cffi:defcfun ("MagickVignetteImage" Wand:vignette-image)
  MagickBooleanType
  (wand MagickWand) (black-point double) (white-point double)
  (x double) (y double))

;; Enhance the edges of the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
(cffi:defcfun ("MagickEdgeImage" Wand:edge-image) MagickBooleanType
  (wand MagickWand) (radius double))

;; Emboss the image associated with WAND (a relief effect).
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(cffi:defcfun ("MagickEmbossImage" Wand:emboss-image) MagickBooleanType
  (wand MagickWand) (radius double) (sigma double))

;; MagickWaveImage() creates a "ripple" effect in the image by
;; shifting the pixels vertically along a sine wave whose amplitude
;; and wavelength is specified by the given parameters.
;; The AMPLITUDE argument is a float and defines the how large
;; waves are.
;; The WAVELENGTH argument is a float and defines how often the
;; waves occur.
(cffi:defcfun ("MagickWaveImage" Wand:wave-image) MagickBooleanType
  (wand MagickWand) (amplitude double) (wavelength double))

;; Swirl the image associated with WAND by DEGREES.
(cffi:defcfun ("MagickSwirlImage" Wand:swirl-image) MagickBooleanType
  (wand MagickWand) (degrees double))

(cffi:defcfun ("MagickPosterizeImage" Wand:MagickPosterizeImage)
  MagickBooleanType
  (wand MagickWand) (levels unsigned-long) (ditherp MagickBooleanType))
(defun Wand:posterize-image (wand levels &optional ditherp)
  "Posterize the image associated with WAND.
that is quantise the range of used colours to at most LEVELS.
If optional argument DITHERP is non-nil use a dithering
effect to wipe hard contrasts."
  (Wand:MagickPosterizeImage wand levels ditherp))

;; MagickAddNoiseImage() adds random noise to the image.
(cffi:defcfun ("MagickAddNoiseImage" Wand:add-noise-image) MagickBooleanType
  (wand MagickWand) (noise-type MagickNoiseType))

(cffi:defcfun ("MagickAddNoiseImageChannel" Wand:add-noise-image-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType) (noise-type MagickNoiseType))

;; Reduce the noise in the image associated with WAND by RADIUS.
(cffi:defcfun ("MagickReduceNoiseImage" Wand:reduce-noise-image)
  MagickBooleanType
  (wand MagickWand) (radius double))

;; Perform gamma correction on the image associated with WAND.
;; The argument LEVEL is a positive float, a value of 1.00 (read 100%)
;; is a no-op.
(cffi:defcfun ("MagickGammaImage" Wand:gamma-image) MagickBooleanType
  (wand MagickWand) (level double))

;; Perform gamma correction on CHANNEL of LEVEL on the image
;; associated with WAND.
(cffi:defcfun ("MagickGammaImageChannel" Wand:gamma-image-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType) (level double))

;; Perform median normalisation of the pixels in the image associated
;; with WAND.
(cffi:defcfun ("MagickMedianFilterImage" Wand:median-filter-image)
  MagickBooleanType
  (wand MagickWand) (radius double))

;; Solarise the image associated with WAND.
(cffi:defcfun ("MagickSolarizeImage" Wand:solarize-image) MagickBooleanType
  (wand MagickWand)
  (threshold double))

;; Tweak the image associated with WAND.
(cffi:defcfun ("MagickModulateImage" Wand:MagickModulateImage)
  MagickBooleanType
  (wand MagickWand) (brightness double) (saturation double) (hue double))

(defun* Wand:modulate-image (wand &key (brightness 100.0)
                                  (saturation 100.0)
                                  (hue 100.0))
  (Wand:MagickModulateImage wand brightness saturation hue))

;; Separate a two-color high contrast image.
(cffi:defcfun ("MagickThresholdImage" Wand:threshold-image) MagickBooleanType
  (wand MagickWand) (threshold double))

;; Separate a two-color high contrast image on CHANNEL.
(cffi:defcfun ("MagickThresholdImageChannel" Wand:threshold-image-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType) (threshold double))

(cffi:defcfun ("MagickWhiteThresholdImage" Wand:white-threshold-image)
  MagickBooleanType
  (wand MagickWand) (threshold double))

(cffi:defcfun ("MagickRaiseImage" Wand:MagickRaiseImage) MagickBooleanType
  (wand MagickWand) (width unsigned-long) (height unsigned-long)
  (x long) (y long) (raise MagickBooleanType))

(defun Wand:raise-image (wand &optional raise)
  "Raise image."
  (Wand:MagickRaiseImage
   wand (Wand:image-width wand) (Wand:image-height wand)
   0 0 raise))

;;; Blurs

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(cffi:defcfun ("MagickBlurImage" Wand:blur-image) MagickBooleanType
  (wand MagickWand) (radius double) (sigma double))

;; Blur CHANNEL in the image associated with WAND by RADIUS
;; pixels with derivation SIGMA.
(cffi:defcfun ("MagickBlurImageChannel" Wand:blur-image-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType)
  (radius double) (sigma double))

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(cffi:defcfun ("MagickGaussianBlurImage" Wand:gaussian-blur-image)
  MagickBooleanType
  (wand MagickWand) (radius double) (sigma double))

;; Blur CHANNEL in the image associated with WAND by RADIUS
;; pixels with derivation SIGMA.
(cffi:defcfun ("MagickGaussianBlurImageChannel"
               Wand:gaussian-blur-image-channel) MagickBooleanType
               (wand MagickWand) (channel MagickChannelType)
               (radius double) (sigma double))

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
;; The ANGLE argument is a float and measured in degrees.
(cffi:defcfun ("MagickMotionBlurImage" Wand:motion-blur-image)
  MagickBooleanType
  (wand MagickWand) (radius double) (sigma double) (angle double))

;; Blur the image associated with WAND.
;; The ANGLE argument is a float and measured in degrees.
(cffi:defcfun ("MagickRadialBlurImage" Wand:radial-blur-image)
  MagickBooleanType
  (wand MagickWand) (radius double))

;; Simulates an image shadow
(cffi:defcfun ("MagickShadowImage" Wand:shadow-image)
  MagickBooleanType
  (wand MagickWand) (opacity double) (sigma double) (x long) (y long))

;; Sharpen the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(cffi:defcfun ("MagickSharpenImage" Wand:sharpen-image) MagickBooleanType
  (wand MagickWand)
  (radius double) (sigma double))

;; Sharpen CHANNEL in the image associated with WAND by RADIUS
;; pixels with derivation SIGMA.
(cffi:defcfun ("MagickSharpenImageChannel" Wand:sharpen-image-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType)
  (radius double) (sigma double))

;; Sharpen the image associated with WAND using an unsharp mask.
;; The unsharp mask is defined by RADIUS and SIGMA.
;; The strength of sharpening is controlled by AMOUNT and THRESHOLD.
(cffi:defcfun ("MagickUnsharpMaskImage" Wand:unsharp-mask-image)
  MagickBooleanType
  (wand MagickWand) (radius double) (sigma double)
  (amount double) (threshold double))

;; Sharpen CHANNEL in the image associated with WAND with an unsharp mask
;; defined by RADIUS and SIGMA.  The strength of sharpening is controlled
;; by AMOUNT and THRESHOLD.
(cffi:defcfun ("MagickUnsharpMaskImageChannel"
               Wand:unsharp-mask-image-channel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType)
  (radius double) (sigma double) (amount double) (threshold double))

(cffi:defcfun ("MagickNegateImage" Wand:MagickNegateImage) MagickBooleanType
  (wand MagickWand)
  (greyp MagickBooleanType))
(defun Wand:negate-image (wand &optional greyp)
  "Perform negation on the image associated with WAND."
  (Wand:MagickNegateImage wand greyp))

(cffi:defcfun ("MagickNegateImageChannel"
               Wand:MagickNegateImageChannel)
  MagickBooleanType
  (wand MagickWand) (channel MagickChannelType) (greyp MagickBooleanType))
(defun Wand:negate-image-channel (wand channel &optional greyp)
  "Perform negation of CHANNEL on the image associated with WAND."
  (Wand:MagickNegateImageChannel wand channel greyp))

(cffi:defcfun ("MagickSpreadImage" Wand:spread-image) MagickBooleanType
  (wand MagickWand) (radius double))

;; MagickTrimImage() remove edges that are the background color from
;; the image.
(cffi:defcfun ("MagickTrimImage" Wand:trim-image) MagickBooleanType
  (wand MagickWand) (fuzz double))

;;}}}
;;{{{  `-- Image size

(cffi:defcfun ("MagickGetSize" Wand:MagickGetSize) MagickBooleanType
  (w MagickWand) (width (pointer unsigned-long))
  (height (pointer unsigned-long)))
(cffi:defcfun ("MagickSetSize" Wand:MagickSetSize) MagickBooleanType
  (w MagickWand) (width unsigned-long) (height unsigned-long))

(defun Wand:image-size (wand)
  "Return size of the image, associated with WAND."
  (let ((w (make-ffi-object 'unsigned-long))
        (h (make-ffi-object 'unsigned-long)))
    (when (Wand:MagickGetSize wand (ffi-address-of w) (ffi-address-of h))
      (cons (ffi-get w) (ffi-get h)))))
(defsetf Wand:image-size (wand) (size)
  `(Wand:MagickSetSize ,wand (car ,size) (cdr ,size)))

(cffi:defcfun ("MagickGetImageHeight" Wand:image-height) unsigned-long
  (w MagickWand))
(cffi:defcfun ("MagickGetImageWidth" Wand:image-width) unsigned-long
  (w MagickWand))

;;}}}
;;{{{  `-- Image profiles

(defun Wand-fetch-relinquish-strings (strs slen)
  "Fetch strings from strings array STRS of length SLEN."
  (unless (ffi-null-p strs)
    (unwind-protect
        (mapcar #'(lambda (pr)
                    (ffi-get pr :type 'c-string))
                (ffi-get strs :type (list 'array 'pointer slen)))
      (Wand:RelinquishMemory strs))))

;; Profiles
(cffi:defcfun ("MagickGetImageProfiles" Wand:MagickGetImageProfiles) pointer
  (w MagickWand)
  (pattern c-string)
  (number-profiles pointer))

(defun Wand:image-profiles (wand pattern)
  "Get list of WAND's profiles matching PATTERN."
  (let* ((plen (make-ffi-object 'unsigned-long))
         (profs (Wand:MagickGetImageProfiles
                 wand pattern (ffi-address-of plen))))
    (Wand-fetch-relinquish-strings profs (ffi-get plen))))

(cffi:defcfun ("MagickGetImageProfile" Wand:MagickGetImageProfile) pointer
  (w MagickWand)
  (pname c-string)
  (plen pointer))

(cffi:defcfun ("MagickSetImageProfile" Wand:MagickSetImageProfile)
  MagickBooleanType
  (w MagickWand) (pname c-string)
  (prof pointer) (sz unsigned-int))

(defconst Wand-iptc-names-table
  '((120 . caption) (25 . keyword)))

(defun Wand:image-profile-iptc (wand)
  "Fetch IPTC profile from WAND in lisp-friendly form."
  (let* ((plen (make-ffi-object 'unsigned-int))
         (prof (Wand:MagickGetImageProfile wand "iptc" (ffi-address-of plen)))
         (rlen (ffi-get plen)) (coff 0) (rv nil))
    (unless (ffi-null-p prof)
      (unwind-protect
          (flet ((getbyte () (prog1
                                 (ffi-get prof :off coff :type 'byte)
                               (incf coff))))
            ;; 28 - must start any iptc header
            (while (and (< coff rlen) (= (getbyte) 28))
              (let* ((itype (getbyte)) (idset (getbyte))
                     (l1 (getbyte)) (l2 (getbyte))
                     (ln (logior (ash l1 8) l2)))
                (when (= itype 2)
                  ;; only string type supported
                  (push (cons (cdr (assq idset Wand-iptc-names-table))
                              (ffi-get prof :off coff :type `(c-data . ,ln)))
                        rv))
                (incf coff ln)))
            rv)
        (Wand:RelinquishMemory prof)))))

(defun Wand:image-save-iptc-profile (w iptc)
  "For wand W store IPTC profile."
  (let ((oolen (reduce #'(lambda (e1 e2)
                           (+ e1 5 (length (cdr e2))))
                       iptc :initial-value 0)))
    (when (> oolen 0)
      (let ((prof (make-ffi-object 'pointer oolen))
            (coff 0))
        (flet ((savebyte (byte)
                 (prog1
                     (ffi-store prof coff 'byte byte)
                   (incf coff))))
          (loop for ipel in iptc do
            (savebyte 28) (savebyte 2)
            (savebyte (car (find (car ipel)
                                 Wand-iptc-names-table :key #'cdr)))
            (let* ((ln (length (cdr ipel)))
                   (l1 (ash (logand ln #xff00) -8))
                   (l2 (logand ln #x00ff)))
              (savebyte l1) (savebyte l2)
              (ffi-store prof coff 'c-string (cdr ipel))
              (incf coff ln))))
        (Wand:MagickSetImageProfile w "iptc" prof oolen)))
    ))

;;}}}
;;{{{  `-- Image properties

(cffi:defcfun ("MagickGetImageProperties" Wand:MagickGetImageProperties) pointer
  (w MagickWand)
  (pattern c-string)
  (number-properties pointer))

(defun Wand:image-properties (w pattern)
  "Return list of image properties that match PATTERN."
  (let* ((plen (make-ffi-object 'unsigned-long))
         (props (Wand:MagickGetImageProperties
                 w pattern (ffi-address-of plen))))
    (Wand-fetch-relinquish-strings props (ffi-get plen))))

(cffi:defcfun ("MagickGetImageProperty" Wand:MagickGetImageProperty) pointer
  (w MagickWand) (property c-string))

(cffi:defcfun ("MagickSetImageProperty" Wand:MagickSetImageProperty)
  MagickBooleanType
  (w MagickWand) (prop c-string) (val c-string))

(defun Wand:image-property (w property)
  "Return value for PROPERTY.
Use \(setf \(Wand:image-property w prop\) VAL\) to set property."
  (let ((pv (Wand:MagickGetImageProperty w property)))
    (unless (ffi-null-p pv)
      (unwind-protect
          (ffi-get pv :type 'c-string)
        (Wand:RelinquishMemory pv)))))

(defsetf Wand:image-property (w prop) (val)
  `(Wand:MagickSetImageProperty ,w ,prop ,val))

(cffi:defcfun ("MagickGetQuantumRange" Wand:MagickGetQuantumRange) pointer
  (qr (pointer unsigned-long)))
(defun Wand:quantum-range ()
  (let ((qr (make-ffi-object 'unsigned-long)))
    (Wand:MagickGetQuantumRange (ffi-address-of qr))
    (ffi-get qr)))

;; Very simple properties editor
(defun Wand-mode-prop-editor ()
  "Run properties editor."
  (interactive)
  (let* ((iw image-wand)
         (props (remove-if-not
                 #'(lambda (prop)
                     (string-match Wand-mode-properties-pattern prop))
                 (Wand:image-properties iw ""))))
    (save-window-excursion
      (with-temp-buffer
        (save-excursion
          (mapc #'(lambda (prop)
                    (insert prop ": " (Wand:image-property iw prop) "\n"))
                props))
        (pop-to-buffer (current-buffer))
        (text-mode)
        (message "Press %s when done, or %s to cancel"
                 (sorted-key-descriptions
                  (where-is-internal 'exit-recursive-edit))
                 (sorted-key-descriptions
                  (where-is-internal 'abort-recursive-edit)))
        (recursive-edit)

        ;; User pressed C-M-c, parse buffer and store new props
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((st (buffer-substring (point-at-bol) (point-at-eol)))
                 (pv (split-string st ": ")))
            (setf (Wand:image-property iw (first pv)) (second pv)))
          (next-line 1))))))

;;}}}
;;{{{  `-- Image clip mask

(cffi:defcfun ("MagickGetImageClipMask" Wand:clip-mask) MagickWand
  (w MagickWand))

(cffi:defcfun ("SetImageClipMask" Wand:SetImageClipMask) MagickBooleanType
  (i pointer) (m pointer))

(cffi:defcfun ("MagickSetImageClipMask" Wand:MagickSetImageClipMask)
  MagickBooleanType
  (w MagickWand) (cm MagickWand))

(defsetf Wand:clip-mask (w) (cm)
  "Set wand's W clip mask to be CM.
If CM is nil or null-pointer then unset clip mask."
  `(if (and ,cm (not (ffi-null-p ,cm)))
       (Wand:MagickSetImageClipMask ,w ,cm)
     ;; call SetImageClipMask directly to unset the clip mask
     (Wand:SetImageClipMask
      (ffi-fetch ,w (ffi-slot-offset 'MagickWand-private 'images) 'pointer)
      (ffi-null-pointer))))

;;}}}
;;{{{  `-- Misc image functions

;; MagickSetImageMatte() (un)sets the image matte channel
(cffi:defcfun ("MagickSetImageMatte" Wand:MagickSetImageMatte) MagickBooleanType
  (w MagickWand)
  (matte MagickBooleanType))

(cffi:defcfun ("MagickGetImageAlphaChannel" Wand:image-alpha-channel)
  MagickBooleanType
  (wand MagickWand))

(cffi:defcfun ("MagickSetImageAlphaChannel" Wand:MagickSetImageAlphaChannel)
  MagickBooleanType
  (wand MagickWand)
  (alpha MagickAlphaType))

(defsetf Wand:image-alpha-channel (w) (at)
  `(Wand:MagickSetImageAlphaChannel ,w ,at))

;;}}}
;;{{{  `-- DrawingWand operations

;; MagickDrawImage() renders the drawing wand on the current image.
(cffi:defcfun ("MagickDrawImage" Wand:MagickDrawImage) MagickBooleanType
  (w MagickWand) (dw DrawingWand))

(cffi:defcfun ("MagickAnnotateImage" Wand:MagickAnnotateImage)
  MagickBooleanType
  (w MagickWand) (dw DrawingWand) (x double) (y double)
  (angle double) (text c-string))

(cffi:defcfun ("ClearDrawingWand" Wand:clear-drawing-wand) void
  (dw DrawingWand))

(cffi:defcfun ("CloneDrawingWand" Wand:copy-drawing-wand) DrawingWand
  (dw DrawingWand))

(cffi:defcfun ("DestroyDrawingWand" Wand:delete-drawing-wand) DrawingWand
  (dw DrawingWand))

(cffi:defcfun ("NewDrawingWand" Wand:make-drawing-wand) DrawingWand)

(defmacro Wand-with-drawing-wand (dw &rest forms)
  "With allocated drawing wand DW do FORMS."
  `(let ((,dw (Wand:make-drawing-wand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:delete-drawing-wand ,dw))))
(put 'Wand-with-drawing-wand 'lisp-indent-function 'defun)

;; Drawing fonts
(defun Wand:draw-font (dw)
  "For drawing wand DW return draw font as wand-font object."
  (make-wand-font :family (Wand:draw-font-family dw)
                  :size (Wand:draw-font-size dw)
                  :weight (Wand:draw-font-weight dw)
                  :stretch (Wand:draw-font-stretch dw)
                  :style (Wand:draw-font-style dw)))

(defsetf Wand:draw-font (dw) (fn)
  "For drawing wand DW set font to FN.
FN might be a string or wand-font object."
  `(if (stringp ,fn)
       (setf (Wand:draw-font-font ,dw) ,fn)
     (let ((fm (wand-font-family ,fn))
           (sz (wand-font-size ,fn))
           (weight (wand-font-weight ,fn))
           (stretch (wand-font-stretch ,fn))
           (style (wand-font-style ,fn)))
       (when fm (setf (Wand:draw-font-family ,dw) fm))
       (when sz (setf (Wand:draw-font-size ,dw) sz))
       (when weight (setf (Wand:draw-font-weight ,dw) weight))
       (when stretch (setf (Wand:draw-font-stretch ,dw) stretch))
       (when style (setf (Wand:draw-font-style ,dw) style)))))

(cffi:defcfun ("DrawGetFont" Wand:draw-font-font) safe-string
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFont" Wand:DrawSetFont) MagickBooleanType
  (dw DrawingWand) (font-name c-string))

(defsetf Wand:draw-font-font (dw) (fn)
  `(Wand:DrawSetFont ,dw ,fn))

(cffi:defcfun ("DrawGetFontFamily" Wand:draw-font-family) safe-string
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFontFamily" Wand:DrawSetFontFamily) MagickBooleanType
  (dw DrawingWand) (font-family c-string))

(defsetf Wand:draw-font-family (dw) (ff)
  `(Wand:DrawSetFontFamily ,dw ,ff))

(cffi:defcfun ("DrawGetFontSize" Wand:DrawGetFontSize) double
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFontSize" Wand:DrawSetFontSize) void
  (dw DrawingWand) (font-size double))

(defun Wand:draw-font-size (dw)
  (int (Wand:DrawGetFontSize dw)))
(defsetf Wand:draw-font-size (dw) (fn-size)
  `(Wand:DrawSetFontSize ,dw (float ,fn-size)))

(cffi:defcfun ("DrawGetFontStretch" Wand:draw-font-stretch) MagickStretchType
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFontStretch" Wand:DrawSetFontStretch) void
  (dw DrawingWand) (stretch MagickStretchType))
(defsetf Wand:draw-font-stretch (dw) (fs)
  `(Wand:DrawSetFontStretch ,dw ,fs))

(cffi:defcfun ("DrawGetFontStyle" Wand:draw-font-style) MagickStyleType
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFontStyle" Wand:DrawSetFontStyle) void
  (dw DrawingWand) (stretch MagickStyleType))
(defsetf Wand:draw-font-style (dw) (fs)
  `(Wand:DrawSetFontStyle ,dw ,fs))

(cffi:defcfun ("DrawGetFontWeight" Wand:draw-font-weight) unsigned-long
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFontWeight" Wand:DrawSetFontWeight) void
  (dw DrawingWand) (fw unsigned-long))
(defsetf Wand:draw-font-weight (dw) (fw)
  `(Wand:DrawSetFontWeight ,dw ,fw))

(cffi:defcfun ("DrawGetFillRule" Wand:draw-fill-rule) FillRule
  (dw DrawingWand))
(cffi:defcfun ("DrawSetFillRule" Wand:DrawSetFillRule) void
  (dw DrawingWand) (fill-rule FillRule))

(defsetf Wand:draw-fill-rule (dw) (fr)
  `(Wand:DrawSetFillRule ,dw ,fr))

(cffi:defcfun ("DrawPoint" Wand:draw-point) void
  (dw DrawingWand) (x double) (y double))

(defun Wand:draw-points (dw points)
  (mapc #'(lambda (p) (Wand:draw-point dw (car p) (cdr p))) points))

(cffi:defcfun ("DrawAnnotation" Wand:draw-annotation) void
  (dw DrawingWand) (x double) (y double) (text c-string))

(cffi:defcfun ("DrawGetTextAntialias" Wand:text-antialias)
  MagickBooleanType
  (dw DrawingWand))
(cffi:defcfun ("DrawSetTextAntialias" Wand:SetTextAntialias) void
  (dw DrawingWand) (taa MagickBooleanType))
(defsetf Wand:text-antialias (dw) (taa)
  `(Wand:SetTextAntialias ,dw ,taa))

(cffi:defcfun ("DrawGetTextAlignment" Wand:text-alignment)
  MagickAlignType
  (dw DrawingWand))
(cffi:defcfun ("DrawSetTextAlignment" Wand:SetTextAlignment) void
  (dw DrawingWand) (tat MagickAlignType))
(defsetf Wand:text-alignment (dw) (tat)
  `(Wand:SetTextAlignment ,dw ,tat))

(cffi:defcfun ("DrawGetGravity" Wand:text-gravity)
  MagickGravityType
  (dw DrawingWand))
(cffi:defcfun ("DrawSetGravity" Wand:SetTextGravity) void
  (dw DrawingWand) (tat MagickGravityType))
(defsetf Wand:text-gravity (dw) (tgt)
  `(Wand:SetTextGravity ,dw ,tgt))

;  DrawSetTextDecoration(DrawingWand *,const DecorationType),

(cffi:defcfun ("DrawArc" Wand:draw-arc) void
  (dw DrawingWand) (sx double) (sy double) (ex double)
  (ey double) (sd double) (ed double))

(cffi:defcfun ("DrawCircle" Wand:draw-circle) void
  (dw DrawingWand) (ox double) (oy double) (px double) (py double))

(cffi:defcfun ("DrawRectangle" Wand:draw-rectangle) void
  (dw DrawingWand) (ox double) (oy double) (ex double) (ey double))

(cffi:defcfun ("DrawRoundRectangle" Wand:draw-round-rectangle) void
  (dw DrawingWand) (x1 double) (y1 double) (x2 double) (y2 double)
  (rx double) (ry double))

(cffi:defcfun ("DrawColor" Wand:draw-color) void
  (dw DrawingWand) (x double) (y double) (paint-method PaintMethod))

(cffi:defcfun ("DrawPolygon" Wand:DrawPolygon) void
  (dw DrawingWand)
  (number-coordinates unsigned-long)
  (coordinates PointInfo))

(cffi:defcfun ("DrawPolyline" Wand:DrawPolyline) void
  (dw DrawingWand)
  (number-coordinates unsigned-long)
  (coordinates PointInfo))

(cffi:defcfun ("DrawBezier" Wand:DrawBezier) void
  (dw DrawingWand)
  (number-coordinates unsigned-long)
  (coordinates PointInfo))

(defun Wand:points-PointInfo (points)
  (let* ((plen (length points))
         (coords (make-ffi-object (list 'array 'PointInfo plen))))
    (dotimes (n plen)
      (let ((poi (make-ffi-object 'PointInfo))
            (npo (nth n points)))
        (setf (PointInfo->x poi) (float (car npo))
              (PointInfo->y poi) (float (cdr npo)))
        (ffi-aset coords n poi)))
    coords))

(defun Wand:draw-polygon (dw points)
  (Wand:DrawPolygon dw (length points) (Wand:points-PointInfo points)))

(cffi:defcfun ("DrawLine" Wand:draw-line) void
  (dw DrawingWand) (sx double) (sy double)
  (ex double) (ey double))

(defun Wand:draw-lines (dw points)
  (Wand:DrawPolyline dw (length points) (Wand:points-PointInfo points)))

(defun Wand:draw-bezier (dw points)
  (Wand:DrawBezier dw (length points) (Wand:points-PointInfo points)))

(defun Wand:draw-segment (dw seg)
  (Wand:draw-line dw (float (caar seg)) (float (cdar seg))
                  (float (cadr seg)) (float (cddr seg))))

(defun Wand:draw-segments (dw segs)
  (mapc #'(lambda (seg) (Wand:draw-segment dw seg)) segs))

;; DrawComposite() composites an image onto the current image, using
;; the specified composition operator, specified position, and at the
;; specified size.
(cffi:defcfun ("DrawComposite" Wand:DrawComposite) MagickBooleanType
  (dw DrawingWand) (compose WandCompositeOperator)
  (x double) (y double) (width double) (height double) (wand MagickWand))

;; DrawEllipse() draws an ellipse on the image.
(cffi:defcfun ("DrawEllipse" Wand:draw-ellipse) void
  (dw DrawingWand) (ox double) (oy double) (rx double)
  (ry double) (start double) (end double))

(cffi:defcfun ("DrawGetFillColor" Wand:DrawGetFillColor) void
  (dw DrawingWand) (pixel PixelWand))

(cffi:defcfun ("DrawSetFillColor" Wand:DrawSetFillColor) void
  (dw DrawingWand) (pixel pointer))

(defun Wand:draw-fill-color (dw)
  (let ((pw (Wand:NewPixelWand)))
    (Wand:DrawGetFillColor dw pw)
    pw))

(defsetf Wand:draw-fill-color (w) (p)
  `(Wand:DrawSetFillColor ,w ,p))

(cffi:defcfun ("DrawGetStrokeColor" Wand:DrawGetStrokeColor) void
  (dw DrawingWand) (pixel PixelWand))

(cffi:defcfun ("DrawSetStrokeColor" Wand:DrawSetStrokeColor) void
  (dw DrawingWand) (pixel pointer))

(defun Wand:draw-stroke-color (dw)
  (let ((pw (Wand:NewPixelWand)))
    (Wand:DrawGetStrokeColor dw pw)
    pw))

(defsetf Wand:draw-stroke-color (w) (p)
  `(Wand:DrawSetStrokeColor ,w ,p))

(cffi:defcfun ("DrawGetFillOpacity" Wand:draw-fill-opacity) double
  (dw DrawingWand))

(cffi:defcfun ("DrawSetFillOpacity" Wand:DrawSetFillOpacity) void
  (dw DrawingWand) (fo double))

(defsetf Wand:draw-fill-opacity (w) (fo)
  `(Wand:DrawSetFillOpacity ,w ,fo))

(cffi:defcfun ("DrawSetFillRule" Wand:DrawSetFillRule) void
  (dw DrawingWand) (fr FillRule))

(cffi:defcfun ("DrawMatte" Wand:draw-matte) void
  (dw DrawingWand) (x double) (y double) (paint-method PaintMethod))

(cffi:defcfun ("DrawGetStrokeWidth" Wand:draw-stroke-width) double
  (dw DrawingWand))

(cffi:defcfun ("DrawSetStrokeWidth" Wand:DrawSetStrokeWidth) void
  (dw DrawingWand) (stroke-width double))

(defsetf Wand:draw-stroke-width (dw) (sw)
  `(Wand:DrawSetStrokeWidth ,dw ,sw))

(cffi:defcfun ("DrawSetStrokeColor" Wand:DrawSetStrokeColor) void
  (dw DrawingWand) (stroke-color pointer))

(cffi:defcfun ("DrawGetStrokeOpacity" Wand:draw-stroke-opacity) double
  (dw DrawingWand))

(cffi:defcfun ("DrawSetStrokeOpacity" Wand:DrawSetStrokeOpacity) void
  (dw DrawingWand) (stroke-opacity double))

(defsetf Wand:draw-stroke-opacity (dw) (so)
  `(Wand:DrawSetStrokeOpacity ,dw ,so))

(cffi:defcfun ("DrawGetStrokeAntialias" Wand:draw-stroke-antialias)
  MagickBooleanType
  (dw DrawingWand))

(cffi:defcfun ("DrawSetStrokeAntialias" Wand:DrawSetStrokeAntialias) void
  (dw DrawingWand) (aa MagickBooleanType))

(defsetf Wand:draw-stroke-antialias (dw) (aa)
  `(Wand:DrawSetStrokeAntialias ,dw ,aa))

(cffi:defcfun ("DrawGetClipPath" Wand:draw-clip-path) safe-string
  (dw DrawingWand))

(cffi:defcfun ("DrawSetClipPath" Wand:DrawSetClipPath) MagickBooleanType
  (dw DrawingWand) (clip-path c-string))

(defsetf Wand:draw-clip-path (dw) (cp)
  `(Wand:DrawSetClipPath ,dw ,cp))

(cffi:defcfun ("DrawPushClipPath" Wand:draw-push-clip-path) void
  (dw DrawingWand) (clip-mask-id c-string))

(cffi:defcfun ("DrawPopClipPath" Wand:draw-pop-clip-path) void
  (dw DrawingWand))

;; ImageMagick
(cffi:defcfun ("PushDrawingWand" Wand:push-drawing-wand) void
  (dw DrawingWand))
(cffi:defcfun ("PopDrawingWand" Wand:pop-drawing-wand) void
  (dw DrawingWand))

(cffi:defcfun ("DrawPushDefs" Wand:draw-push-defs) void
  (dw DrawingWand))

(cffi:defcfun ("DrawPopDefs" Wand:draw-pop-defs) void
  (dw DrawingWand))

;;}}}


;; I wonder if we actually need this, Wand-API documentation says
;; yeah, but I've seen gazillions of code snippets not using it
;; -hroptatyr
(ignore-errors
  (cffi:defcfun ("MagickWandGenesis" Wand:MagickWandGenesis) void)
  (cffi:defcfun ("MagickWandTerminus" Wand:MagickWandTerminus) void))

;;}}}

;;{{{ Util image, glyph and size related functions

(defun Wand:emacs-image-type (wand)
  "Return `rawrgb' or `rawrgba' image type suitable for WAND."
  'rawrgb)

;; NOTE: 'rawrgba DOES NOT actually works in SXEmacs, so we strip
;;       alpha --lg

;   (if (Wand:image-alpha-channel wand)
;       'rawrgba
;     'rawrgb))

(defun Wand:emacs-image-internal (wand img-type x y w h)
  "Return Emacs image spec."
  (vector img-type
          :data (Wand:get-image-pixels-internal wand img-type x y w h)
          :pixel-width w
          :pixel-height h))

(defun Wand:emacs-image (wand)
  "Return Emacs image for the WAND."
  (Wand:emacs-image-internal
   wand (Wand:emacs-image-type wand)
   0 0 (Wand:image-width wand) (Wand:image-height wand)))

(defun Wand:glyph-internal (wand x y w h)
  "Return glyph for the WAND."
  (make-glyph
   (Wand:emacs-image-internal
    wand (Wand:emacs-image-type wand) x y w h)))

(defun Wand:glyph (wand)
  "Return glyph for the WAND."
  (make-glyph (Wand:emacs-image wand)))

(defun Wand:correct-orientation (wand)
  "Automatically rotate WAND image according to exif:Orientation."
  (let* ((orient (Wand:image-property wand "exif:Orientation"))
         (angle (cond ((string= orient "6") 90)
                      ((string= orient "3") 180)
                      ((string= orient "8") -90))))
    (when angle
      (setf (Wand:image-property wand "exif:Orientation") "1")
      (Wand-operation-apply 'rotate wand angle))))

(defun Wand:fit-size (wand max-width max-height &optional scaler force)
  "Fit WAND image into MAX-WIDTH and MAX-HEIGHT.
This operation keeps aspect ratio of the image.
Use SCALER function to perform scaling, by default `Wand:scale-image'
is used.
Return non-nil if fiting was performed."
  (unless scaler (setq scaler #'Wand:scale-image))
  (let* ((width (Wand:image-width wand))
         (height (Wand:image-height wand))
         (prop (/ (float width) (float height)))
         rescale)
    (when (or force (< max-width width))
      (setq width max-width
            height (round (/ max-width prop))
            rescale t))
    (when (or force (< max-height height))
      (setq width (round (* max-height prop))
            height max-height
            rescale t))

    (when rescale
      (funcall scaler wand width height))
    rescale))

(defun Wand-mode-preview-glyph (wand)
  (let ((off-x (get wand 'offset-x))
        (off-y (get wand 'offset-y)))
    (Wand:glyph-internal
     wand off-x off-y
     (- (Wand:image-width wand) off-x)
     (- (Wand:image-height wand) off-y))))

;;}}}
;;{{{ Custom variables for Wand-mode

(defgroup Wand-mode nil
  "Group to customize Wand mode."
  :prefix "Wand-mode-")

(defcustom Wand-mode-redeye-threshold 1.6
  "*Threshold to fix red eyes."
  :type 'float
  :group 'Wand-mode)

(defcustom Wand-mode-sigma 2.0
  "*Sigma for operations such as gaussian-blur, sharpen, etc."
  :type 'float
  :group 'Wand-mode)

(defcustom Wand-mode-zoom-factor 2
  "Default zoom in/out factor."
  :type 'number
  :group 'Wand-mode)

(defcustom Wand-mode-region-outline-color "black"
  "*Color used to outline region when selecting."
  :type 'color
  :group 'Wand-mode)

(defcustom Wand-mode-region-fill-color "white"
  "*Color used to fill region when selecting."
  :type 'color
  :group 'Wand-mode)

(defcustom Wand-mode-region-outline-width 1.3
  "*Width of outline line for region when selecting."
  :type 'float
  :group 'Wand-mode)

(defcustom Wand-mode-region-outline-opacity 0.7
  "*Opacity of the outline.
1.0 - Opaque
0.0 - Transparent"
  :type 'float
  :group 'Wand-mode)

(defcustom Wand-mode-region-fill-opacity 0.35
  "*Opacity for the region when selecting.
1.0 - Opaque
0.0 - Transparent"
  :type 'float
  :group 'Wand-mode)

(defcustom Wand-mode-show-fileinfo t
  "*Non-nil to show file info on top of display."
  :type 'boolean
  :group 'Wand-mode)

(defcustom Wand-mode-show-iptc-info t
  "*Non-nil to display IPTC info if any."
  :type 'boolean
  :group 'Wand-mode)

(defcustom Wand-mode-show-operations t
  "*Non-nil to show operations done on file."
  :type 'boolean
  :group 'Wand-mode)

(defcustom Wand-mode-auto-fit t
  "*Non-nil to perform fiting to window size.
You can always toggle fitting using `Wand-mode-toggle-fit' command
\(bound to \\<Wand-mode-map>\\[Wand-mode-toggle-fit]\)."
  :type 'boolean
  :group 'Wand-mode)

(defcustom Wand-mode-auto-rotate t
  "*Non-nil to perform automatic rotation according to orientation.
Orientation is taken from EXIF."
  :type 'boolean
  :group 'Wand-mode)

(defcustom Wand-mode-query-for-overwrite t
  "*Non-nil to ask user when overwriting existing files."
  :type 'boolean
  :group 'Wand-mode)

(defcustom Wand-mode-properties-pattern "^exif:"
  "Pattern for properties editor."
  :type 'string
  :group 'Wand-mode)

(defvar Wand-global-operations-list nil
  "Denotes global operations list")

(defcustom Wand-mode-scaler #'Wand:scale-image
  "Function used to scale image for \"fit to size\" operation.
You could use one of `Wand:scale-image', `Wand:sample-image' or create
your own scaler with `Wand-make-scaler'."
  :type 'function
  :group 'Wand-mode)

(defvar Wand-mode-hook nil
  "Hooks to call when entering `Wand-mode'.")

(defvar Wand-insert-info-hook nil
  "Hooks to call when inserting info into `Wand-mode'.")

;;}}}
;;{{{ Wand-mode-map

(defvar Wand-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Undo/Redo operation
    (define-key map [(control /)] #'Wand-mode-undo)
    (define-key map [(control _)] #'Wand-mode-undo)
    (define-key map [undo] #'Wand-mode-undo)
    (define-key map [(control ?x) (control ?/)] #'Wand-mode-redo)
    (define-key map [(control ?x) (meta ?:)] #'Wand-mode-repeat-last-operation)
    (define-key map [(control ?\.)] #'Wand-mode-repeat-last-operation)

    ;; Saving
    (define-key map [(control ?x) (control ?s)] #'Wand-mode-save-file)
    (define-key map [(control ?x) (control ?w)] #'Wand-mode-write-file)

    ;; Navigation
    (define-key map [space] #'Wand-mode-next-image)
    (define-key map [backspace] #'Wand-mode-prev-image)
    (define-key map [(meta ?<)] #'Wand-mode-first-image)
    (define-key map [(meta >)] #'Wand-mode-last-image)

    (define-key map [next] #'Wand-mode-next-page)
    (define-key map [prior] #'Wand-mode-prev-page)
    (define-key map [home] #'Wand-mode-first-page)
    (define-key map [end] #'Wand-mode-last-page)
    (define-key map [?g] #'Wand-mode-goto-page)
    (define-key map [(meta ?g)] #'Wand-mode-goto-page)

    ;; Region
    (define-key map [button1] #'Wand-mode-select-region)
    (define-key map [(control meta ?z)] #'Wand-mode-activate-region)

    ;; General commands
    (define-key map [button3] #'Wand-mode-popup-menu)
    (define-key map [(meta button1)] #'Wand-mode-drag-image)
    (define-key map [(control button1)] #'Wand-mode-drag-image)
    (define-key map [o] #'Wand-mode-operate)
    (define-key map [O] #'Wand-mode-global-operations-list)
    (define-key map [x] #'Wand-mode-toggle-fit)
    (define-key map [i] #'Wand-mode-identify)
    (define-key map [e] #'Wand-mode-prop-editor)
    (define-key map [q] #'Wand-mode-quit)
    (define-key map [(control ?r)] #'Wand-mode-reload)
    (define-key map [p] #'Wand-mode-add-iptc-tag)

    ;; Zooming
    (define-key map [+] #'Wand-mode-zoom-in)
    (define-key map [-] #'Wand-mode-zoom-out)

    ;; Rotations
    (define-key map [r] #'Wand-mode-rotate-right)
    (define-key map [l] #'Wand-mode-rotate-left)

    ;; Region operations
    (define-key map [c] #'Wand-mode-crop)
    (define-key map [\.] #'Wand-mode-redeye-remove)

    map)
  "Keymap for Wand mode.")

;;}}}
;;{{{ Wand-menu

(defvar Wand-menu
  '("Wand"
    ["Next" Wand-mode-next-image
     :active (Wand-next-file buffer-file-name)]
    ["Previous" Wand-mode-prev-image
     :active (Wand-next-file buffer-file-name t)]
    ["First" Wand-mode-first-image]
    ["Last" Wand-mode-last-image]
    ("Page" :filter Wand-menu-page-navigations)
    "---"
    ["Image Info" Wand-mode-identify]
    ["Reload" Wand-mode-reload]
    ["Fitting" Wand-mode-toggle-fit
     :style toggle :selected (get image-wand 'fitting)]
    "---"
    ["Undo" Wand-mode-undo :active operations-list]
    ["Redo" Wand-mode-redo :active undo-list]
    ["Save Image" Wand-mode-save-file]
    ["Save Image As" Wand-mode-write-file]
    "---"
    ["Zoom In" Wand-mode-zoom-in]
    ["Zoom Out" Wand-mode-zoom-out]
    "---"
    ["Rotate right" Wand-mode-rotate-right]
    ["Rotate left" Wand-mode-rotate-left]
    "---"
    ("Region" :filter Wand-menu-region-operations)
    ("Transform" :filter (lambda (not-used)
                           (Wand-menu-generate 'transform-operation)))
    ("Effects" :filter (lambda (not-used)
                         (Wand-menu-generate 'effect-operation)))
    ("Enhance" :filter (lambda (not-used)
                         (Wand-menu-generate 'enhance-operation)))
    ("F/X" :filter (lambda (not-used)
                     (Wand-menu-generate 'f/x-operation)))
    "---"
    ["Quit" Wand-mode-quit])
  "Menu for Wand display mode.")

(defun Wand-menu-page-navigations (not-used)
  "Generate menu for page navigation."
  (list ["Next Page" Wand-mode-next-page
         :active (Wand:has-next-image image-wand)]
        ["Previous Page" Wand-mode-prev-page
         :active (Wand:has-prev-image image-wand)]
        ["First Page" Wand-mode-first-page
         :active (/= (Wand:iterator-index image-wand) 0) ]
        ["Last Page" Wand-mode-last-page
         :active (/= (Wand:iterator-index image-wand)
                     (1- (Wand:images-num image-wand))) ]
        "-"
        ["Goto Page" Wand-mode-goto-page
         :active (/= (Wand:images-num image-wand) 1)]))

(defun Wand-menu-region-operations (not-used)
  "Generate menu for region operations."
  (mapcar #'(lambda (ro)
              (vector (get ro 'menu-name) ro :active 'preview-region))
          (apropos-internal "^Wand-mode-"
                            #'(lambda (c)
                                (and (commandp c)
                                     (get c 'region-operation)
                                     (get c 'menu-name))))))

(defun Wand-mode-commands-by-tag (tag)
  "Return list of wand command for which TAG property is set."
  (apropos-internal "^Wand-mode-"
                    #'(lambda (c) (and (commandp c) (get c tag)))))

(defun Wand-menu-generate (tag)
  "Generate menu structure for TAG commands."
  (mapcar #'(lambda (to)
              (vector (get to 'menu-name) to))
          (remove-if-not #'(lambda (c) (get c tag))
                         (Wand-mode-commands-by-tag 'menu-name))))

(defun Wand-mode-popup-menu (be)
  "Popup wand menu."
  (interactive "e")
  (popup-menu Wand-menu be))

;;}}}
;;{{{ Operations definitions

(defmacro define-Wand-operation (name args &rest body)
  "Define new operation of NAME.
ARGS specifies arguments to operation, first must always be wand."
  (let ((fsym (intern (format "Wand-op-%S" name))))
    `(defun ,fsym ,args
       ,@body)))

(defmacro Wand-possible-for-region (wand &rest body)
  `(if preview-region
       (let* ((iwand ,wand)
              (region (Wand-mode-image-region))
              (wand (apply #'Wand:image-region iwand region)))
         (unwind-protect
             (progn
               ,@body
               (Wand:image-composite iwand wand :CopyCompositeOp
                                     (nth 2 region) (nth 3 region)))
           (setq preview-region nil)
           (Wand:delete-wand wand)))
     ,@body))
(put 'Wand-possible-for-region 'lisp-indent-function 'defun)

(define-Wand-operation flip (wand)
  "Flip the image."
  (Wand-possible-for-region wand
    (Wand:flip-image wand)))

(define-Wand-operation flop (wand)
  "Flop the image."
  (Wand-possible-for-region wand
    (Wand:flop-image wand)))

(define-Wand-operation rotate (wand degree)
  "Rotate image by DEGREE.
This is NOT lossless rotation for jpeg-like formats."
  (Wand-with-pixel-wand pw
    (setf (Wand:pixel-color pw) "black")
    (Wand:RotateImage wand pw (float degree))))

(define-Wand-operation contrast (wand cp)
  "Increase/decrease contrast of the image."
  (Wand-possible-for-region wand
    (Wand:MagickContrastImage wand cp)))

(define-Wand-operation normalize (wand)
  "Normalise image."
  (Wand-possible-for-region wand
    (Wand:normalize-image wand)))

(define-Wand-operation despeckle (wand)
  "Despeckle image."
  (Wand-possible-for-region wand
    (Wand:despeckle-image wand)))

(define-Wand-operation enhance (wand)
  "Enhance image."
  (Wand-possible-for-region wand
    (Wand:enhance-image wand)))

(define-Wand-operation equalize (wand)
  "Equalise image."
  (Wand-possible-for-region wand
    (Wand:equalize-image wand)))

(define-Wand-operation gauss-blur (wand radius sigma)
  "Gauss blur image."
  (Wand-possible-for-region wand
    (Wand:gaussian-blur-image wand (float radius) (float sigma))))

(define-Wand-operation sharpen (wand radius sigma)
  "Sharpenize image."
  (Wand-possible-for-region wand
    (Wand:sharpen-image wand (float radius) (float sigma))))

(define-Wand-operation radial-blur (wand angle)
  "Radial blur."
  (Wand-possible-for-region wand
    (Wand:radial-blur-image wand (float angle))))

(define-Wand-operation negate (wand greyp)
  "Negate image."
  (Wand-possible-for-region wand
    (Wand:negate-image wand greyp)))

(define-Wand-operation modulate (wand mtype minc)
  "Modulate the image WAND using MTYPE by MINC."
  (Wand-possible-for-region wand
    (Wand:modulate-image wand mtype (float (+ 100 minc)))))

(define-Wand-operation grayscale (wand)
  "Grayscale image."
  (Wand-possible-for-region wand
    (Wand:SetImageColorspace wand :GRAYColorspace)))

(define-Wand-operation solarize (wand threshold)
  "Solarise image by THRESHOLD."
  (Wand-possible-for-region wand
    (Wand:solarize-image wand (float threshold))))

(define-Wand-operation swirl (wand degrees)
  "Swirl image."
  (Wand-possible-for-region wand
    (Wand:swirl-image wand (float degrees))))

(define-Wand-operation oil (wand radius)
  "Simulate oil-painting of image."
  (Wand-possible-for-region wand
    (Wand:oil-paint-image wand (float radius))))

(define-Wand-operation charcoal (wand radius sigma)
  "Simulate charcoal painting of image."
  (Wand-possible-for-region wand
    (Wand:charcoal-image wand (float radius) (float sigma))))

(define-Wand-operation sepia-tone (wand threshold)
  "Apply sepia tone to image by THRESHOLD."
  (Wand-possible-for-region wand
    (Wand:sepia-tone-image wand (float threshold))))

(define-Wand-operation implode (wand radius)
  "Implude image by RADIUS."
  (Wand-possible-for-region wand
    (Wand:implode-image wand (float radius))))

(define-Wand-operation wave (wand amplitude wave-length)
  "Create wave effect for image with AMPLITUDE and WAVE-LENGTH."
  (Wand-possible-for-region wand
    (Wand:wave-image wand (float amplitude) (float wave-length))))

(define-Wand-operation vignette (wand white black x y)
  "Vignette from image."
  (Wand-possible-for-region wand
    (Wand:vignette-image wand (float white) (float black) (float x) (float y))))

(define-Wand-operation edge (wand radius)
  "Enhance the edges of the image."
  (Wand-possible-for-region wand
    (Wand:edge-image wand (float radius))))

(define-Wand-operation emboss (wand radius sigma)
  "Emboss the image, i.e. add relief."
  (Wand-possible-for-region wand
    (Wand:emboss-image wand (float radius) (float sigma))))

(define-Wand-operation reduce-noise (wand radius)
  "Reduce noise in the image."
  (Wand-possible-for-region wand
    (Wand:reduce-noise-image wand (float radius))))

(define-Wand-operation add-noise (wand noise-type)
  "Add noise to image."
  (Wand-possible-for-region wand
    (Wand:add-noise-image wand noise-type)))

(define-Wand-operation spread (wand radius)
  "Spread the image."
  (Wand-possible-for-region wand
    (Wand:spread-image wand (float radius))))

(define-Wand-operation trim (wand fuzz)
  "Trim the image."
  (Wand-possible-for-region wand
    (Wand:trim-image wand (float fuzz))))

(define-Wand-operation raise (wand raise)
  "Raise the image."
  (Wand-possible-for-region wand
    (Wand:raise-image wand raise)))

(define-Wand-operation crop (wand region)
  "Crop image to REGION."
  (apply #'Wand:crop-image wand region)
  (Wand:reset-image-page wand))

(define-Wand-operation chop (wand region)
  "Chop REGION in the image."
  (apply #'Wand:chop-image wand region))

(defun Wand:get-image-rgb-pixels (wand x y w h)
  "Extract RGB pixels from WAND."
  (let ((target (make-ffi-object 'c-data (* w h 3))))
    (when (Wand:MagickGetImagePixels
           wand x y w h "RGB" :char-pixel target)
      (Wand:pixels-extract-colors (ffi-get target) 3))))

(defun Wand:get-rgb-pixel-at (wand x y)
  "Return WAND's RGB pixel at X, Y."
  (car (Wand:get-image-rgb-pixels wand x y 1 1)))

(defun Wand-fix-red-pixels (pixels)
  "Simple red PIXELS fixator.
Normalize pixel color if it is too 'red'."
  (let* ((rchan '(0.1 0.6 0.3))
         (gchan '(0.0 1.0 0.0))
         (bchan '(0.0 0.0 1.0))
         (rnorm (/ 1.0 (apply #'+ rchan)))
         (gnorm (/ 1.0 (apply #'+ gchan)))
         (bnorm (/ 1.0 (apply #'+ bchan))))
    (flet ((normalize (chan norm r g b)
             (min 255 (int (* norm (+ (* (first chan) r)
                                      (* (second chan) g)
                                      (* (third chan) b)))))))
      (mapcar #'(lambda (pixel-value)
                  (multiple-value-bind (r g b) pixel-value
                    (if (>= r (* Wand-mode-redeye-threshold g))
                        (list (normalize rchan rnorm r g b)
                              (normalize gchan gnorm r g b)
                              (normalize bchan bnorm r g b))
                      (list r g b))))
              pixels))))

(defun Wand-mode-redeye-blur-radius (w h)
  "Return apropriate blur radius for region of width W and height H.
It should not be too large for large regions, and it should not be
too small for small regions."
  (1- (sqrt (sqrt (sqrt (sqrt (* w h)))))))

(define-Wand-operation redeye-remove (wand region)
  "Remove redeye in the REGION."
  (multiple-value-bind (w h x y) region
    (Wand-with-wand cw
      ;; Consitute new wand with fixed red pixels
      (Wand:MagickConstituteImage
       cw w h "RGB" :char-pixel
       (let ((stor (make-ffi-object 'c-data (* w h 3))))
         (ffi-set stor (Wand:pixels-arrange-colors
                        (Wand-fix-red-pixels
                         (Wand:get-image-rgb-pixels wand x y w h))))
         stor))

      ;; Limit blur effect to ellipse at the center of REGION by
      ;; setting clip mask
      (let ((mask (Wand:copy-wand cw)))
        (unwind-protect
            (progn
              (Wand-with-drawing-wand dw
                (Wand-with-pixel-wand pw
                  (setf (Wand:pixel-color pw) "white")
                  (setf (Wand:draw-fill-color dw) pw)
                  (Wand:draw-color dw 0.0 0.0 :ResetMethod))
                (Wand-with-pixel-wand pw
                  (setf (Wand:pixel-color pw) "black")
                  (setf (Wand:draw-fill-color dw) pw))
                (Wand:draw-ellipse
                 dw (/ w 2.0) (/ h 2.0) (/ w 2.0) (/ h 2.0) 0.0 360.0)
                (Wand:MagickDrawImage mask dw))
              (setf (Wand:clip-mask cw) mask))
          (Wand:delete-wand mask)))

      (Wand:gaussian-blur-image
       cw 0.0 (Wand-mode-redeye-blur-radius w h))
      (setf (Wand:clip-mask cw) nil)

      ;; Finally copy blured image to WAND
      (Wand:image-composite wand cw :CopyCompositeOp x y))))

(define-Wand-operation zoom (wand outp factor)
  (let ((nw (funcall (if outp #'/ #'*)
                     (Wand:image-width wand) (float factor)))
        (nh (funcall (if outp #'/ #'*)
                     (Wand:image-height wand) (float factor))))
    (Wand:scale-image wand (round nw) (round nh))))

(define-Wand-operation sample (wand width height)
  (Wand:scale-image wand width height))

(defmacro Wand-make-scaler (filter-type blur)
  "Create resize function, suitable with `Wand:fit-resize'.
FILTER-TYPE and BLUR specifies smothing applied after resize.
FILTER-TYPE is one of: :PointFilter, :BoxFilter, :TriangleFilter,
:HermiteFilter, :HanningFilter, :HammingFilter, :BlackmanFilter,
:GaussianFilter, :QuadraticFilter, :CubicFilter, :CatromFilter,
:MitchellFilter, :LanczosFilter, :BesselFilter, :SincFilter,
:KaiserFilter, :WelshFilter, :ParzenFilter, :LagrangeFilter,
:BohmanFilter, :BartlettFilter, :SentinelFilter.
BLUR is float, 0.25 for insane pixels, > 2.0 for excessively smoth."
  `(lambda (iw x y)
     (Wand:resize-image iw x y ,filter-type (float ,blur))))

(define-Wand-operation fit-size (wand width height)
  (Wand:fit-size wand width height Wand-mode-scaler t))

(define-Wand-operation liquid-rescale (wand width height)
  (Wand:liquid-rescale wand width height 0.0 0.0))

;;}}}
;;{{{ Operations list functions

(defun Wand-operation-lookup (opname)
  (intern (format "Wand-op-%S" opname)))

(defun Wand-operation-apply (operation wand &rest args)
  "Apply OPERATION to WAND using addition arguments ARGS."
  (setq operations-list
        (append operations-list (list (cons operation args))))
  (setq undo-list nil)                  ; Reset undo
  (apply (Wand-operation-lookup operation) wand args))

(defun Wand-operation-list-apply (wand &optional operations)
  "Apply all operations in OPERATIONS list."
  (dolist (op (or operations operations-list))
    (apply (Wand-operation-lookup (car op))
           wand (cdr op))))

;;}}}
;;{{{ Helper functions

(defun Wand-mode-image-region ()
  "Return region in real image, according to `preview-region'."
  (let ((off-x (get preview-wand 'offset-x))
        (off-y (get preview-wand 'offset-y))
        (xcoeff (// (Wand:image-width image-wand)
                    (Wand:image-width preview-wand)))
        (ycoeff (// (Wand:image-height image-wand)
                    (Wand:image-height preview-wand))))
    (mapcar #'round (list (* (nth 0 preview-region) xcoeff)
                          (* (nth 1 preview-region) ycoeff)
                          (* (+ (nth 2 preview-region) off-x) xcoeff)
                          (* (+ (nth 3 preview-region) off-y) ycoeff)))))

(defun Wand-mode-file-info ()
  "Return info about file as a string."
  (declare (special off-x))
  (declare (special off-y))
  (let ((iw (Wand:image-width image-wand))
        (ih (Wand:image-height image-wand))
        (ow (Wand:image-orig-width image-wand))
        (oh (Wand:image-orig-height image-wand)))
    (concat "File: " (file-name-nondirectory buffer-file-name)
            " (" (Wand:get-magick-property image-wand "size") "), "
            (Wand:image-format image-wand)
            " " (format "%dx%d" iw ih)
            (if (and (not (zerop ow)) (not (zerop oh))
                     (or (/= ow iw) (/= oh ih)))
                (format " (Orig: %dx%d)" ow oh)
              "")
            (if (> (Wand:images-num image-wand) 1)
                (format ", Page: %d/%d" (1+ (Wand:iterator-index image-wand))
                        (Wand:images-num image-wand))
              "")
            ;; Print offset info
            (if (and preview-wand (boundp 'off-x) (boundp 'off-y)
                     (or (positivep off-x) (positivep off-y)))
                (format ", Offset: +%d+%d" off-x off-y)
              "")
            ;; Print region info
            (if preview-region
                (apply #'format ", Region: %dx%d+%d+%d"
                       (Wand-mode-image-region))
              ""))))

(defun Wand-mode-iptc-split-keywords (tag-value)
  (mapcar #'(lambda (kw) (cons 'keyword kw))
          (nreverse
           (split-string tag-value "\\(, \\|,\\)"))))

(defun Wand-mode-iptc-from-widgets (widgets)
  "Return profile made up from WIDGETS info."
  (mapcan
   #'(lambda (widget)
       (let ((iptc-tag (widget-get widget :iptc-tag))
             (tag-value (widget-get widget :value)))
         (cond ((string= tag-value "") nil)
               ((eq iptc-tag 'keywords)
                ;; Special case for keywords
                (Wand-mode-iptc-split-keywords tag-value))
               (t (list (cons iptc-tag tag-value))))))
   widgets))

(defun Wand-mode-iptc-notify (wid &rest args)
  "Called when some IPTC info changed."
  (Wand:image-save-iptc-profile
   image-wand (Wand-mode-iptc-from-widgets (cons wid widget-field-list)))
  (Wand-mode-update-info))

(defun Wand-mode-insert-iptc-tags ()
  "Insert iptc tags info."
  (kill-local-variable 'widget-global-map)
  (kill-local-variable 'widget-field-new)
  (kill-local-variable 'widget-field-last)
  (kill-local-variable 'widget-field-was)
  (kill-local-variable 'widget-field-list)

  (let* ((iptc (Wand:image-profile-iptc image-wand))
         (cpt (cdr (assq 'caption iptc)))
         (kws (mapcar #'cdr (remove-if-not
                             #'(lambda (e) (eq 'keyword (car e)))
                             iptc))))
    (when cpt
      (widget-create 'editable-field
                     :tag "Caption"
                     :format "IPTC Caption: %v"
                     :iptc-tag 'caption
                     :notify #'Wand-mode-iptc-notify
                     cpt))
    (when kws
      (widget-create 'editable-field
                     :format "IPTC Keywords: %v"
                     :tag "Keywords"
                     :iptc-tag 'keywords
                     :notify #'Wand-mode-iptc-notify
                     (mapconcat #'identity kws ", ")))
    (widget-setup)))

(defun Wand-mode-add-iptc-tag (tag value)
  "Add TAG to ITPC profile."
  (interactive (list (completing-read
                      "IPTC Tag: " '(("caption") ("keywords")) nil t)
                     (read-string "ITPC Tag value: ")))
  (let ((tags-val (cond ((string= tag "caption")
                         (list (cons 'caption value)))
                        ((string= tag "keywords")
                         (Wand-mode-iptc-split-keywords value))
                        (t (error "Invalid IPTC tag")))))
    (Wand:image-save-iptc-profile
     image-wand (nconc (Wand-mode-iptc-from-widgets widget-field-list)
                       tags-val))
    (Wand-mode-update-info)))

(defun Wand-mode-insert-info ()
  "Insert some file informations."
  (when Wand-mode-show-fileinfo
    (insert (Wand-mode-file-info) "\n"))
  (when Wand-mode-show-iptc-info
    (Wand-mode-insert-iptc-tags))

  ;; XXX iptc may set those below again
  (let ((inhibit-read-only t)
        (before-change-functions nil)
        (after-change-functions nil))

    (when (and Wand-mode-show-operations)
      (when operations-list
        (insert (format "Operations: %S" operations-list) "\n"))
      (when Wand-global-operations-list
        (insert (format "Global operations: %S"
                        Wand-global-operations-list) "\n")))

    ;; Info about pickup color
    (when (boundp 'pickup-color)
      (declare (special pickup-color))
      (let* ((cf (make-face (gensym "dcolor-") nil t))
             (place (car pickup-color))
             (color (cdr pickup-color))
             (fcol (apply #'format "#%02x%02x%02x" color)))
        (set-face-background cf fcol)
        (insert (format "Color: +%d+%d " (car place) (cdr place)))
        (insert-face "      " cf)
        (insert (format " %s R:%d, G:%d, B:%d\n" fcol
                        (car color) (cadr color) (caddr color)))))

    (run-hooks 'Wand-insert-info-hook)))

(defun Wand-mode-update-info ()
  "Only update info region."
  (let ((inhibit-read-only t)
        before-change-functions
        after-change-functions)
    (mapc 'widget-delete widget-field-list)
    (save-excursion
      (goto-char (point-min))
      (delete-region (point-at-bol)
                     (save-excursion
                       (goto-char (point-max))
                       (point-at-bol)))
      (Wand-mode-insert-info))
    (set-buffer-modified-p nil)))

(defun Wand-mode-update-file-info ()
  "Update file info."
  (when Wand-mode-show-fileinfo
    (let ((inhibit-read-only t)
          before-change-functions
          after-change-functions)
      (save-excursion
        (goto-char (point-min))
        (delete-region (point-at-bol) (point-at-eol))
        (insert (Wand-mode-file-info))))
    (set-buffer-modified-p nil)))

(defun Wand-mode-preview-with-region ()
  "Return highlighted version of `preview-wand' in case region is selected."
  (when preview-region
    (multiple-value-bind (w h x y) preview-region
      ;; Take into account current offset
      (incf x (get preview-wand 'offset-x))
      (incf y (get preview-wand 'offset-y))
      (Wand-with-drawing-wand dw
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) Wand-mode-region-outline-color)
          (Wand:DrawSetStrokeColor dw pw))
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) Wand-mode-region-fill-color)
          (setf (Wand:draw-fill-color dw) pw))
        (setf (Wand:draw-stroke-width dw) Wand-mode-region-outline-width
              (Wand:draw-stroke-opacity dw) Wand-mode-region-outline-opacity
              (Wand:draw-fill-opacity dw) Wand-mode-region-fill-opacity)
        (Wand:draw-lines dw (list (cons x y) (cons (+ x w) y)
                                  (cons (+ x w) (+ y h)) (cons x (+ y h))
                                  (cons x y)))
        (let ((nw (Wand:copy-wand preview-wand)))
          (put nw 'offset-x (get preview-wand 'offset-x))
          (put nw 'offset-y (get preview-wand 'offset-y))
          (Wand:MagickDrawImage nw dw)
          nw)))))

(defun Wand-mode-insert-preview ()
  "Display wand W at the point."
  ;; NOTE: if size not changed, then keep offset-x and offset-y
  ;; properties
  (let ((saved-w (and preview-wand (Wand:image-width preview-wand)))
        (saved-h (and preview-wand (Wand:image-height preview-wand)))
        (off-x (or (get preview-wand 'offset-x) 0))
        (off-y (or (get preview-wand 'offset-y) 0)))
    ;; Delete old preview and create new one
    (when preview-wand (Wand:delete-wand preview-wand))
    (setq preview-wand (Wand:get-image image-wand))

    ;; Rescale preview to fit the window
    (let ((scale-h (- (window-text-area-pixel-height)
                      ;; TODO: we need something to do to count pixels
                      ;; used by displayed text.  Below constructions
                      ;; does not work for some reason --lg
                      (if t             ;(string= (buffer-substring) "")
                          0
                        (window-displayed-text-pixel-height))))
          (scale-w (window-text-area-pixel-width)))
      (when (and (get image-wand 'fitting)
                 (Wand:fit-size preview-wand scale-w scale-h))
        (message "Rescale to %dx%d"
                 (Wand:image-width preview-wand)
                 (Wand:image-height preview-wand))))

    ;; Set offset properties
    (if (and (eq saved-w (Wand:image-width preview-wand))
             (eq saved-h (Wand:image-height preview-wand)))
        (progn (put preview-wand 'offset-x off-x)
               (put preview-wand 'offset-y off-y))
      (put preview-wand 'offset-x 0)
      (put preview-wand 'offset-y 0))

    ;; Hackery to insert invisible char, so widget-delete won't affect
    ;; preview-glyph visibility
    (let ((ext (make-extent (point) (progn (insert " ") (point)))))
      (set-extent-property ext 'invisible t)
      (set-extent-property ext 'start-open t))

    (let ((pwr (Wand-mode-preview-with-region)))
      (unwind-protect
          (progn
            (set-extent-end-glyph
             preview-extent (Wand-mode-preview-glyph (or pwr preview-wand)))
            (set-extent-endpoints
             preview-extent (point) (point) (current-buffer)))
        (when pwr (Wand:delete-wand pwr))))))

(defun Wand-redisplay ()
  (let ((inhibit-read-only t)
	before-change-functions
	after-change-functions)
    (erase-buffer)
    (Wand-mode-insert-info)
    (Wand-mode-insert-preview)
    (goto-char (point-min)))
  (set-buffer-modified-p nil))

;;;###autoload
(defun Wand-display-noselect (file)
  (let* ((bn (format "*Wand: %s*" (file-name-nondirectory file)))
         (buf (if (and (eq major-mode 'Wand-mode)
                       (not (get-buffer bn)))
                  ;; Use current buffer
                  (progn
                    (rename-buffer bn)
                    (current-buffer))
                (get-buffer-create bn))))
    (with-current-buffer buf
      (unless (eq major-mode 'Wand-mode)
        ;; Initialise local variables
        (kill-all-local-variables)
        (make-variable-buffer-local 'image-wand)
        (make-variable-buffer-local 'preview-wand)
        (make-variable-buffer-local 'preview-region)
        (make-variable-buffer-local 'preview-extent)
        (make-variable-buffer-local 'operations-list)
        (make-variable-buffer-local 'undo-list)
        (make-variable-buffer-local 'kill-buffer-hook)
        (setq operations-list nil)
        (setq undo-list nil)
        (setq preview-wand nil)
        (setq preview-extent (make-extent 0 0 ""))
        (setq image-wand (Wand:make-wand))
        (put image-wand 'fitting Wand-mode-auto-fit)

        (use-local-map Wand-mode-map)
        (setq mode-name "Wand")
        (setq major-mode 'Wand-mode)
        (setq buffer-read-only t)
        ;; Setup menubar
        (when (featurep 'menubar)
          (set-buffer-menubar current-menubar)
          (add-submenu '() Wand-menu)
          (setq mode-popup-menu Wand-menu))
        (add-hook 'kill-buffer-hook 'Wand-mode-cleanup))

      (when preview-wand
        (Wand:delete-wand preview-wand))
      (setq preview-wand nil)
      (setq preview-region nil)
      (setq operations-list nil)
      (setq undo-list nil)
      (Wand:clear-wand image-wand)
      ;; Fix buffer-file-name in case of viewing directory
      (when (file-directory-p file)
        (setq file (or (Wand-next-file (concat file "/.")) file)))
      (setq buffer-file-name file)
      (setq default-directory (file-name-directory file))

      (unless (Wand:read-image image-wand file)
        (kill-buffer (current-buffer))
        (error "Can't read file %s" file))
      (when Wand-mode-auto-rotate
        (Wand:correct-orientation image-wand))

      ;; Apply operations in case global operations list is used
      (mapc #'(lambda (op)
                (apply #'Wand-operation-apply
                       (car op) image-wand (cdr op)))
            Wand-global-operations-list)

      (Wand-redisplay)

      ;; Finally run hook
      (run-hooks 'Wand-mode-hook))
    buf))

;;;###autoload
(defun Wand-display (file)
  (interactive "fImage file: ")
  (switch-to-buffer (Wand-display-noselect file) t))

(defun Wand-mode ()
  "Start `Wand-display' on filename associated with current buffer.
Bindings are:
  \\{Wand-mode-map}"
  (interactive)
  (Wand-display (buffer-file-name)))

;;;###autoload
(defun Wand-find-file-enable ()
  "Enable `find-file' to use `Wand-display' for supported filetypes."
  (push '(Wand-file-supported-for-read-p . Wand-display-noselect)
        find-file-magic-files-alist))

(defun Wand-mode-cleanup ()
  "Cleanup when wand buffer is killed."
  (when (extentp preview-extent)
    (delete-extent preview-extent))
  (when preview-wand
    (Wand:delete-wand preview-wand))
  (Wand:delete-wand image-wand))

(defun Wand-mode-quit ()
  "Quit Wand display mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun Wand-mode-reload ()
  "Reload and redisplay image file."
  (interactive)
  (Wand-display buffer-file-name))

(defun Wand-mode-identify ()
  "Show info about image."
  (interactive)
  (let ((iw image-wand))
    (with-displaying-help-buffer
     #'(lambda ()
         (set-buffer standard-output)
         (insert (Wand:identify-image iw)))
     "Wand:info")))

(defun Wand-mode-operations-table ()
  "Return completion table for Wand operations."
  (mapcar #'(lambda (to)
              (cons (downcase (get to 'menu-name)) to))
          (Wand-mode-commands-by-tag 'menu-name)))

(defun Wand-mode-operate (op-name)
  "Operate on image."
  (interactive (list (completing-read
                      "Operation: " (Wand-mode-operations-table)
                      nil t)))
  (let ((op (assoc op-name (Wand-mode-operations-table))))
    (call-interactively (cdr op))))

(defcustom Wand-formats-read-unsupported
  '("a" "b" "c" "g" "h" "o" "k" "m" "r" "x" "y" "txt" "text" "pm")
  "List of formats that are not intented to be opened by Wand."
  :type '(list string)
  :group 'Wand-mode)

(defun Wand-format-supported-for-read-p (format)
  "Return non-nil if Wand can read files in FORMAT."
  (unless (member (downcase format) Wand-formats-read-unsupported)
    (let ((fi (Wand:GetMagickInfo
               format (ffi-address-of
                       (make-ffi-object 'MagickExceptionInfo)))))
      (and (not (ffi-null-p fi))
           (not (ffi-null-p (MagickInfo->decoder fi)))
           ))))
;; ImageMagick on linux treats any format to be RAW for some reason
           ;; We can't read raw formats
;           (not (MagickInfo->raw fi))))))

(defcustom Wand-formats-write-unsupported
  '("html")
  "List of formats that are not intented to be written by Wand."
  :type '(list string)
  :group 'Wand-mode)

(defun Wand-format-supported-for-write-p (format)
  "Return non-nil if Wand can write files in FORMAT."
  (unless (member (downcase format) Wand-formats-write-unsupported)
    (let ((fi (Wand:GetMagickInfo
               format (ffi-address-of
                       (make-ffi-object 'MagickExceptionInfo)))))
      (and (not (ffi-null-p fi))
           (not (ffi-null-p (MagickInfo->encoder fi)))))))

;;;###autoload
(defun Wand-file-supported-for-read-p (file)
  "Return non-nil if Wand can decode FILE."
  ;; Try by extension first, then try heuristic method using
  ;; `magic:file-type'
  (let ((ext (file-name-extension file)))
    (or (and ext (Wand-format-supported-for-read-p ext))

        (multiple-value-bind (itype imagetext)
            (split-string (or (magic:file-type file) " ") " ")
          (and imagetext
               (string= (downcase imagetext) "image")
               (Wand-format-supported-for-read-p itype))))))

(defun Wand-formats-list (fmt-regexp &optional mode)
  "Return names of supported formats that matches FMT-REGEXP.
Optionally you can specify MODE:
  'read  - Only formats that we can read
  'write - Only formats that we can write
  'read-write - Formats that we can and read and write
  'any or nil - Any format (default)."
  (let* ((excp (make-ffi-object 'MagickExceptionInfo))
         (num (make-ffi-object 'unsigned-long))
         (fil (Wand:GetMagickInfoList
               fmt-regexp (ffi-address-of num) (ffi-address-of excp))))
    (unless (ffi-null-p fil)
      (unwind-protect
          (loop for n from 0 below (ffi-get num)
            with minfo = nil
            do (setq minfo (ffi-aref fil n))
            if (ecase (or mode 'any)
                 (read (not (ffi-null-p (MagickInfo->decoder minfo))))
                 (write (not (ffi-null-p (MagickInfo->encoder minfo))))
                 (read-write
                  (and (not (ffi-null-p (MagickInfo->decoder minfo)))
                       (not (ffi-null-p (MagickInfo->encoder minfo)))))
                 (any t))
            collect (ffi-get (MagickInfo->name minfo) :type 'c-string))
        (Wand:RelinquishMemory fil)))))

;;}}}
;;{{{ File navigation commands

(defun Wand-next-file (curfile &optional reverse-order)
  "Return next (to CURFILE) image file in the directory.
If REVERSE-ORDER is specified, then return previous file."
  (let* ((dir (file-name-directory curfile))
         (fn (file-name-nondirectory curfile))
         (dfiles (directory-files dir nil nil 'sorted-list t))
         (nfiles (cdr (member fn (if reverse-order (nreverse dfiles) dfiles)))))
    (while (and nfiles (not (Wand-file-supported-for-read-p
                             (concat dir (car nfiles)))))
      (setq nfiles (cdr nfiles)))
    (and nfiles (concat dir (car nfiles)))))

(defun Wand-mode-next-image (&optional reverse)
  "View next image."
  (interactive)
  (let ((nf (Wand-next-file buffer-file-name reverse)))
    (unless nf
      (error (format "No %s file" (if reverse "previous" "next"))))
    (Wand-display nf)))

(defun Wand-mode-prev-image ()
  "View previous image."
  (interactive)
  (Wand-mode-next-image t))

(defun Wand-mode-last-image (&optional reverse)
  "View last image in the directory."
  (interactive)
  (let ((rf buffer-file-name)
        (ff (Wand-next-file buffer-file-name reverse)))
    (while ff
      (setq rf ff)
      (setq ff (Wand-next-file rf reverse)))
    (Wand-display rf)))

(defun Wand-mode-first-image ()
  "View very first image in the directory."
  (interactive)
  (Wand-mode-last-image t))

;;}}}
;;{{{ Pages navigation commands

(defun Wand-mode-next-page ()
  "Display next image in image chain."
  (interactive)
  (unless (Wand:has-next-image image-wand)
    (error "No next image in chain"))
  (Wand:next-image image-wand)
  (Wand-redisplay))

(defun Wand-mode-prev-page ()
  "Display previous image in image chain."
  (interactive)
  (unless (Wand:has-prev-image image-wand)
    (error "No previous image in chain"))
  (Wand:prev-image image-wand)
  (Wand-redisplay))

(defun Wand-mode-first-page ()
  "Display first image in image chain."
  (interactive)
  (Wand:set-first-iterator image-wand)
  (Wand-redisplay))

(defun Wand-mode-last-page ()
  "Display last image in image chain."
  (interactive)
  (Wand:set-last-iterator image-wand)
  (Wand-redisplay))

(defun Wand-mode-goto-page (n)
  "Display last image in image chain."
  (interactive
   (list (if (numberp current-prefix-arg)
             current-prefix-arg
           (read-number "Goto page: " t))))
  ;; Internally images in chain counts from 0
  (unless (setf (Wand:iterator-index image-wand) (1- n))
    (error "No such page" n))
  (Wand-redisplay))

;;}}}

;;{{{ Transform operations

(defun Wand-mode-flip ()
  "Flip the image."
  (interactive)
  (Wand-operation-apply 'flip image-wand)
  (Wand-redisplay))
(put 'Wand-mode-flip 'transform-operation t)
(put 'Wand-mode-flip 'menu-name "Flip")

(defun Wand-mode-flop ()
  "Flop the image."
  (interactive)
  (Wand-operation-apply 'flop image-wand)
  (Wand-redisplay))
(put 'Wand-mode-flop 'transform-operation t)
(put 'Wand-mode-flop 'menu-name "Flop")

(defun Wand-mode-trim (fuzz)
  "Flop the image."
  (interactive (list (read-number "Fuzz [0%]: " nil "0")))
  (Wand-operation-apply 'trim image-wand (/ fuzz 100.0))
  (Wand-redisplay))
(put 'Wand-mode-trim 'transform-operation t)
(put 'Wand-mode-trim 'menu-name "Trim Edges")

(defun Wand-mode-rotate (arg)
  "Rotate image to ARG degrees.
If ARG is positive then rotate in clockwise direction.
If negative then to the opposite."
  (interactive "nDegrees: ")
  (Wand-operation-apply 'rotate image-wand arg)
  (Wand-redisplay))
(put 'Wand-mode-rotate 'transform-operation t)
(put 'Wand-mode-rotate 'menu-name "Rotate")

(defun Wand-mode-rotate-left (arg)
  "Rotate image to the left.
If ARG is specified then rotate on ARG degree."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         90)))
  (Wand-mode-rotate (- arg)))

(defun Wand-mode-rotate-right (arg)
  "Rotate image to the right.
If ARG is specified then rotate on ARG degree."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         90)))
  (Wand-mode-rotate arg))

(defun Wand-mode-raise (arg)
  "Create button-like 3d effect."
  (interactive "P")
  (Wand-operation-apply 'raise image-wand arg)
  (Wand-redisplay))
(put 'Wand-mode-raise 'transform-operation t)
(put 'Wand-mode-raise 'menu-name "3D Button Effect")

;;}}}
;;{{{ Effect operations

(defun Wand-mode-radial-blur (arg)
  "Blur the image radially by ARG degree."
  (interactive (list (read-number "Blur radius [2.0]: " nil "2.0")))
  (Wand-operation-apply 'radial-blur image-wand arg)
  (Wand-redisplay))
(put 'Wand-mode-radial-blur 'effect-operation t)
(put 'Wand-mode-radial-blur 'menu-name "Radial Blur")

(defun Wand-mode-sharpen (radius sigma)
  "Sharpen image with by RADIUS and SIGMA."
  (interactive (list (read-number "Radius [1]: " nil "1")
                     (read-number (format "Sigma [%d]: " Wand-mode-sigma)
                                  nil (number-to-string Wand-mode-sigma))))
  (Wand-operation-apply 'sharpen image-wand radius sigma)
  (Wand-redisplay))
(put 'Wand-mode-sharpen 'effect-operation t)
(put 'Wand-mode-sharpen 'menu-name "Sharpen")

(defun Wand-mode-gaussian-blur (radius sigma)
  "Apply gaussian blur of RADIUS and SIGMA to the image."
  (interactive (list (read-number "Radius [1]: " nil "1")
                     (read-number (format "Sigma [%d]: " Wand-mode-sigma)
                                  nil (number-to-string Wand-mode-sigma))))
  (Wand-operation-apply 'gauss-blur image-wand radius sigma)
  (Wand-redisplay))
(put 'Wand-mode-gaussian-blur 'effect-operation t)
(put 'Wand-mode-gaussian-blur 'menu-name "Gaussian Blur")

(defun Wand-mode-despeckle ()
  "Despeckle image."
  (interactive)
  (Wand-operation-apply 'despeckle image-wand)
  (Wand-redisplay))
(put 'Wand-mode-despeckle 'effect-operation t)
(put 'Wand-mode-despeckle 'menu-name "Despeckle")

(defun Wand-mode-edge (radius)
  "Enhance edges of the image by RADIUS.
Default is 1."
  (interactive (list (read-number "Radius [1.0]: " nil "1.0")))
  (Wand-operation-apply 'edge image-wand radius)
  (Wand-redisplay))
(put 'Wand-mode-edge 'effect-operation t)
(put 'Wand-mode-edge 'menu-name "Edge Detect")

(defun Wand-mode-emboss (radius sigma)
  "Emboss the image with RADIUS and SIGMA."
  (interactive (list (read-number "Radius [1.0]: " nil "1.0")
                     (read-number (format "Sigma [%d]: " Wand-mode-sigma)
                                  nil (number-to-string Wand-mode-sigma))))
  (Wand-operation-apply 'emboss image-wand radius sigma)
  (Wand-redisplay))
(put 'Wand-mode-emboss 'effect-operation t)
(put 'Wand-mode-emboss 'menu-name "Emboss")

(defun Wand-mode-reduce-noise (arg)
  "Reduce the noise with ARG radius.
Default is 1."
  (interactive "p")
  (Wand-operation-apply 'reduce-noise image-wand arg)
  (Wand-redisplay))
(put 'Wand-mode-reduce-noise 'effect-operation t)
(put 'Wand-mode-reduce-noise 'menu-name "Reduce Noise")

(defun Wand-mode-add-noise (noise-type)
  "Add noise of NOISE-TYPE."
  (interactive
   (list (completing-read "Noise type [poisson]: "
                          (mapcar #'(lambda (ev)
                                      (let ((sn (symbol-name (car ev))))
                                        (list (and (string-match
                                                    ":\\(.+\\)Noise" sn)
                                                   (downcase
                                                    (match-string 1 sn))))))
                                  (ffi-enum-values 'MagickNoiseType))
                          nil t nil nil "poisson")))
  (let ((nt (intern (format ":%sNoise" (capitalize noise-type)))))
    (Wand-operation-apply 'add-noise image-wand nt))
  (Wand-redisplay))
(put 'Wand-mode-add-noise 'effect-operation t)
(put 'Wand-mode-add-noise 'menu-name "Add Noise")

(defun Wand-mode-spread (radius)
  "Add noise of NOISE-TYPE."
  (interactive (list (read-number "Spread radius [1.0]: " nil "1.0")))
  (Wand-operation-apply 'spread image-wand radius)
  (Wand-redisplay))
(put 'Wand-mode-spread 'effect-operation t)
(put 'Wand-mode-spread 'menu-name "Spread")

;;}}}
;;{{{ Enhance operations

(defun Wand-mode-contrast (ctype)
  "Increase or decrease contrast.
By default increase."
  (interactive (list (completing-read
                      "Contrast [increase]: " '(("increase") ("decrease"))
                      nil t nil nil "increase")))
  (Wand-operation-apply 'contrast image-wand (string= ctype "increase"))
  (Wand-redisplay))
(put 'Wand-mode-contrast 'enhance-operation t)
(put 'Wand-mode-contrast 'menu-name "Contrast")

(defun Wand-mode-normalize ()
  "Normalize image."
  (interactive)
  (Wand-operation-apply 'normalize image-wand)
  (Wand-redisplay))
(put 'Wand-mode-normalize 'enhance-operation t)
(put 'Wand-mode-normalize 'menu-name "Normalize")

(defun Wand-mode-enhance ()
  "Enhance image."
  (interactive)
  (Wand-operation-apply 'enhance image-wand)
  (Wand-redisplay))
(put 'Wand-mode-enhance 'enhance-operation t)
(put 'Wand-mode-enhance 'menu-name "Enhance")

(defun Wand-mode-equalize ()
  "Equalise image."
  (interactive)
  (Wand-operation-apply 'equalize image-wand)
  (Wand-redisplay))
(put 'Wand-mode-equalize 'enhance-operation t)
(put 'Wand-mode-equalize 'menu-name "Equalize")

(defun Wand-mode-negate (arg)
  "Negate image.
If prefix ARG is specified then negate by grey."
  (interactive "P")
  (Wand-operation-apply 'negate image-wand arg)
  (Wand-redisplay))
(put 'Wand-mode-negate 'enhance-operation t)
(put 'Wand-mode-negate 'menu-name "Negate")

(defun Wand-mode-grayscale ()
  "Convert image to grayscale colorspace."
  (interactive)
  (Wand-operation-apply 'grayscale image-wand)
  (Wand-redisplay))
(put 'Wand-mode-grayscale 'enhance-operation t)
(put 'Wand-mode-grayscale 'menu-name "Grayscale")

(defun Wand-mode-modulate (type inc)
  "Modulate image's brightness, saturation or hue."
  (interactive (let* ((tp (completing-read
                           "Modulate [saturation]: "
                           '(("brightness") ("saturation") ("hue"))
                           nil t nil nil "saturation"))
                      (tinc (read-number (format "Increase %s [25%%]: " tp)
                                         nil "25")))
                 (list (cond ((string= tp "brightness") :brightness)
                             ((string= tp "hue") :hue)
                             (t :saturation)) tinc)))
  (Wand-operation-apply 'modulate image-wand type inc)
  (Wand-redisplay))
(put 'Wand-mode-modulate 'enhance-operation t)
(put 'Wand-mode-modulate 'menu-name "Modulate")

;;}}}
;;{{{ F/X operations

(defun Wand-mode-solarize (sf)
  "Solarise image with solarize factor SF."
  (interactive (list (read-number "Solarize factor [50%]: " nil "50")))
  (Wand-operation-apply 'solarize image-wand
                        (* (Wand:quantum-range) (/ sf 100.0)))
  (Wand-redisplay))
(put 'Wand-mode-solarize 'f/x-operation t)
(put 'Wand-mode-solarize 'menu-name "Solarize")

(defun Wand-mode-swirl (degrees)
  "Swirl the image by DEGREES."
  (interactive (list (read-number "Degrees [90]: " nil "90")))
  (Wand-operation-apply 'swirl image-wand degrees)
  (Wand-redisplay))
(put 'Wand-mode-swirl 'f/x-operation t)
(put 'Wand-mode-swirl 'menu-name "Swirl")

(defun Wand-mode-oil-paint (radius)
  "Simulate oil painting with RADIUS for the image.
Default radius is 3."
  (interactive (list (read-number "Radius [3.0]: " nil "3.0")))
  (Wand-operation-apply 'oil image-wand radius)
  (Wand-redisplay))
(put 'Wand-mode-oil-paint 'f/x-operation t)
(put 'Wand-mode-oil-paint 'menu-name "Oil Paint")

(defun Wand-mode-charcoal (radius sigma)
  "Simulate charcoal painting for the image.
If prefix ARG is specified then radius for charcoal painting is ARG.
Default is 1."
  (interactive (list (read-number "Radius [1.0]: " nil "1.0")
                     (read-number "Sigma [1.0]: " nil "1.0")))
  (Wand-operation-apply 'charcoal image-wand radius sigma)
  (Wand-redisplay))
(put 'Wand-mode-charcoal 'f/x-operation t)
(put 'Wand-mode-charcoal 'menu-name "Charcoal Draw")

(defun Wand-mode-sepia-tone (threshold)
  "Apply sepia tone to image by THRESHOLD."
  (interactive (list (read-number "Threshold [80%]: " nil "80")))
  (Wand-operation-apply 'sepia-tone image-wand
                        (* (Wand:quantum-range) (/ threshold 100.0)))
  (Wand-redisplay))
(put 'Wand-mode-sepia-tone 'f/x-operation t)
(put 'Wand-mode-sepia-tone 'menu-name "Sepia Tone")

(defun Wand-mode-implode (radius)
  "Implode image by RADIUS.
RADIUS range is [-1.0, 1.0]."
  (interactive (list (read-number "Radius [0.3]: " nil "0.3")))
  (Wand-operation-apply 'implode image-wand radius)
  (Wand-redisplay))
(put 'Wand-mode-implode 'f/x-operation t)
(put 'Wand-mode-implode 'menu-name "Implode")

(defun Wand-mode-vignette (bw)
  "Create vignette using image."
  (interactive (list (read-number "Black/White [10]: " nil "10")))
  (Wand-operation-apply 'vignette image-wand bw bw 0 0)
  (Wand-redisplay))
(put 'Wand-mode-vignette 'f/x-operation t)
(put 'Wand-mode-vignette 'menu-name "Vignette")

(defun Wand-mode-wave (amplitude wave-length)
  "Create wave effect on image with AMPLITUDE and WAVE-LENGTH."
  (interactive (list (read-number "Amplitude [2]: " nil "2")
                     (read-number "Wave length [10]: " nil "10")))
  (Wand-operation-apply 'wave image-wand amplitude wave-length)
  (Wand-redisplay))
(put 'Wand-mode-wave 'f/x-operation t)
(put 'Wand-mode-wave 'menu-name "Wave")

;;}}}

;;{{{ Region commands

(defun Wand-mode-select-region (event)
  "Select region."
  (interactive "e")
  (with-current-buffer (event-buffer event)
    (let ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
          (sx (event-glyph-x-pixel event))
          (sy (event-glyph-y-pixel event))
          (had-preview-region preview-region)
          (mouse-down t))
      (setq preview-region (list 0 0 sx sy))
      (while mouse-down
        (setq event (next-event event))
        (cond ((motion-event-p event)
               (let ((mx (event-glyph-x-pixel event))
                     (my (event-glyph-y-pixel event)))
                 (when (and mx my)
                   (setq preview-region
                         (list (abs (- sx mx)) (abs (- sy my))
                               (min sx mx) (min sy my)))
                   ;; Update info and preview image
                   (Wand-mode-update-file-info)
                   (let ((pwr (Wand-mode-preview-with-region)))
                     (unwind-protect
                         (set-extent-end-glyph
                          preview-extent (Wand-mode-preview-glyph pwr))
                       (Wand:delete-wand pwr))))))

              ((button-release-event-p event)
               (setq mouse-down nil)
               (if (and (positivep (nth 0 preview-region))
                        (positivep (nth 1 preview-region)))
                   ;; Save region
                   (put image-wand 'last-preview-region preview-region)

                 (setq preview-region nil)
                 (if had-preview-region
                     (progn
                       ;; Remove any regions
                       (Wand-mode-update-file-info)
                       (set-extent-end-glyph
                        preview-extent (Wand-mode-preview-glyph preview-wand)))

                   ;; Otherwise pickup color
                   (let* ((col (Wand:get-rgb-pixel-at preview-wand sx sy))
                          (pickup-color (cons (cons sx sy) col)))
                     (declare (special pickup-color))
                     (Wand-mode-update-info)))))
              (t (dispatch-event event)))))))

(defun Wand-mode-activate-region ()
  "Activate last preview-region."
  (interactive)
  (setq preview-region (get image-wand 'last-preview-region))
  (Wand-redisplay))

(defun Wand-mode-drag-image (event)
  "Drag image to view unshown part of the image."
  (interactive "e")
  (let ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
        (sx (event-glyph-x-pixel event))
        (sy (event-glyph-y-pixel event))
        (pw (Wand:image-width preview-wand))
        (ph (Wand:image-height preview-wand))
        (mouse-down t))
    (while mouse-down
      (setq event (next-event event))
      (if (or (motion-event-p event) (button-release-event-p event))
          (let ((off-x (+ (- sx (event-glyph-x-pixel event))
                          (or (get preview-wand 'offset-x) 0)))
                (off-y (+ (- sy (event-glyph-y-pixel event))
                          (or (get preview-wand 'offset-y) 0))))
            (when (< off-x 0) (setq off-x 0))
            (when (< off-y 0) (setq off-y 0))
            (Wand-mode-update-file-info)
            (if (motion-event-p event)
                (set-extent-end-glyph
                 preview-extent (Wand:glyph-internal
                                 preview-wand off-x off-y
                                 (- pw off-x) (- ph off-y)))

              ;; Button released
              (setq mouse-down nil)
              (put preview-wand 'offset-x off-x)
              (put preview-wand 'offset-y off-y)))

        (dispatch-event event)))))

(defun Wand-mode-crop ()
  "Crop image to selected region."
  (interactive)
  (unless preview-region
    (error "Region not selected"))
  (Wand-operation-apply 'crop image-wand (Wand-mode-image-region))
  (setq preview-region nil)
  (Wand-redisplay))
(put 'Wand-mode-crop 'region-operation t)
(put 'Wand-mode-crop 'menu-name "Crop")

(defun Wand-mode-chop ()
  "Chop region from the image."
  (interactive)
  (unless preview-region
    (error "Region not selected"))
  (Wand-operation-apply 'chop image-wand (Wand-mode-image-region))
  (setq preview-region nil)
  (Wand-redisplay))
(put 'Wand-mode-chop 'region-operation t)
(put 'Wand-mode-chop 'menu-name "Chop")

(defun Wand-mode-redeye-remove ()
  "Remove red from the selected region."
  (interactive)
  (unless preview-region
    (error "Region not selected"))
  (let ((gc-cons-threshold most-positive-fixnum)) ; inhibit gc
    (Wand-operation-apply 'redeye-remove image-wand (Wand-mode-image-region))
    (setq preview-region nil)
    (Wand-redisplay)))
(put 'Wand-mode-redeye-remove 'region-operation t)
(put 'Wand-mode-redeye-remove 'menu-name "Remove red eye")

;;}}}
;;{{{ Zooming/Sampling

(defun Wand-mode-zoom-in (factor)
  "Zoom image by FACTOR.
If FACTOR is nil, then `Wand-mode-zoom-factor' is used."
  (interactive "P")
  (Wand-operation-apply 'zoom image-wand nil
                        (if factor
                            (prefix-numeric-value factor)
                          Wand-mode-zoom-factor))
  (Wand-redisplay))

(defun Wand-mode-zoom-out (factor)
  "Zoom image out by `Wand-mode-zoom-factor'."
  (interactive "P")
  (Wand-operation-apply 'zoom image-wand t
                        (if factor
                            (prefix-numeric-value factor)
                          Wand-mode-zoom-factor))
  (Wand-redisplay))

(defun Wand-mode-sample (w h)
  "Sample image to WxH size."
  (interactive
   (list (read-number (format "Width [%d]: " (Wand:image-width image-wand))
                      t (int-to-string (Wand:image-width image-wand)))
         (read-number (format "Height [%d]: " (Wand:image-height image-wand))
                      t (int-to-string (Wand:image-height image-wand)))))
  (Wand-operation-apply 'sample image-wand w h)
  (Wand-redisplay))
(put 'Wand-mode-sample 'transform-operation t)
(put 'Wand-mode-sample 'menu-name "Sample")

(defun Wand-mode-fit-size (w h)
  "Resize image to fit into WxH size."
  (interactive
   (let* ((dw (read-number
               (format "Width [%d]: " (Wand:image-width image-wand))
               t (int-to-string (Wand:image-width image-wand))))
          (dh (round (* (Wand:image-height image-wand)
                        (// dw (Wand:image-width image-wand))))))
     (list dw (read-number (format "Height [%d]: " dh)
                           t (int-to-string dh)))))
  
  (Wand-operation-apply 'fit-size image-wand w h)
  (Wand-redisplay))
(put 'Wand-mode-fit-size 'transform-operation t)
(put 'Wand-mode-fit-size 'menu-name "Fit to size")

(defun Wand-mode-liquid-rescale (w h)
  "Rescale image to WxH using liquid rescale."
  (interactive
   (list (read-number (format "Width [%d]: " (Wand:image-width image-wand))
                      t (int-to-string (Wand:image-width image-wand)))
         (read-number (format "Height [%d]: " (Wand:image-height image-wand))
                      t (int-to-string (Wand:image-height image-wand)))))
  (Wand-operation-apply 'liquid-rescale image-wand w h)
  (Wand-redisplay))
(put 'Wand-mode-liquid-rescale 'transform-operation t)
(put 'Wand-mode-liquid-rescale 'menu-name "Liquid rescale")

;;}}}
;;{{{ Toggle fit, Undo/Redo, Saving

(defun Wand-mode-toggle-fit ()
  "Toggle autofit."
  (interactive)
  (put image-wand 'fitting (not (get image-wand 'fitting)))
  (Wand-redisplay))

(defun Wand-mode-undo (&optional arg)
  "Undo last operation ARG times."
  (interactive "p")
  (unless operations-list
    (error "Nothing to undo"))
  (dotimes (n arg)
    (push (car (last operations-list)) undo-list)
    (setq operations-list (butlast operations-list)))

  ;; Update wand
  (Wand:clear-wand image-wand)
  (Wand:read-image image-wand buffer-file-name)
  (Wand-operation-list-apply image-wand)
  (Wand-redisplay)
  (message "Undo!"))

(defun Wand-mode-redo (&optional arg)
  "Redo last operations ARG times."
  (interactive "p")
  (unless undo-list
    (error "Nothing to redo"))
  (dotimes (n arg)
    (let ((op (pop undo-list)))
      (when op
        (apply #'Wand-operation-apply (car op) image-wand (cdr op)))))
  (Wand-redisplay)
  (message "Redo!"))

(defun Wand-mode-repeat-last-operation ()
  "Repeat last operation on image."
  (interactive)
  (let ((last-op (car (last operations-list))))
    (when last-op
      (apply #'Wand-operation-apply
             (car last-op) image-wand (cdr last-op))
      (Wand-redisplay))))

(defun Wand-mode-global-operations-list (arg)
  "Fix operations list to be global for all images.
If prefix ARG is supplied, then global operations list is reseted.
Useful to skim over images in directory applying operations, for
example zoom."
  (interactive "P")
  (setq Wand-global-operations-list
        (and (not arg) operations-list))
  (Wand-redisplay))

(defun Wand-mode-write-file (format nfile)
  "Write file using output FORMAT."
  (interactive
   (let* ((ofmt (completing-read
                 (format "Output Format [%s]: "
                         (Wand:image-format image-wand))
                 (mapcar #'list (Wand-formats-list "*" 'write))
                 nil t nil nil (Wand:image-format image-wand)))
          (nfname (concat (file-name-sans-extension buffer-file-name)
                          "." (downcase ofmt)))
          (fn (read-file-name
               "Filename: "
               (file-name-directory buffer-file-name)
               nfname nil (file-name-nondirectory nfname))))
     (list ofmt fn)))

  (unless (Wand-format-supported-for-write-p format)
    (error "Unsupported format for writing: %s" format))

  (when (or (not Wand-mode-query-for-overwrite)
            (not (file-exists-p nfile))
            (y-or-n-p (format "File %s exists, overwrite? " nfile)))
    (setf (Wand:image-format image-wand) format)
    (let ((saved-iw image-wand))        ; do this because it is buffer-local
      (with-temp-buffer
        (insert (Wand:image-blob saved-iw))
        (set-visited-file-name nfile t)
        (set-buffer-modified-p t)
        (setq buffer-read-only nil)
        (let ((buffer-file-coding-system (get-coding-system 'binary)))
          (save-buffer))))
    (message "File %s saved" nfile)

    ;; Redisplay in case we can do it
    (if (Wand-format-supported-for-read-p format)
        (Wand-display nfile)
      (find-file nfile))))

(defun Wand-mode-save-file (nfile)
  "Save current wand to file NFILE.
Output format determined by NFILE extension, and no sanity checks
performed, use `Wand-mode-write-file' if are not sure."
  (interactive
   (list (read-file-name "Filename: "
                         (file-name-directory buffer-file-name)
                         buffer-file-name nil
                         (file-name-nondirectory buffer-file-name))))
  (Wand-mode-write-file
   (upcase (file-name-extension nfile)) nfile))

;;}}}

(provide 'ffi-wand)

;; now initialise the environment
(when-fboundp 'Wand:MagickWandGenesis
  (Wand:MagickWandGenesis))

(run-hooks 'ffi-wand-after-load-hook)

;;; ffi-wand.el ends here
