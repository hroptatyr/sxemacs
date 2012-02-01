;;; dialog-items.el --- Dialog-box content for XEmacs

;; Copyright (C) 2000 Andy Piper.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: content, gui, internal, dumped

;; This file is part of SXEmacs.

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;;
;; Simple search dialog
;;
(defvar search-dialog-direction t)
(defvar search-dialog-regexp nil)
(defvar search-dialog nil)

(defun search-dialog-callback (parent image-instance event)
  (save-selected-frame
    (select-frame parent)
    (let ((domain (frame-selected-window  (event-channel event))))
      (funcall (if search-dialog-direction
		   (if search-dialog-regexp
		       're-search-forward
		     'search-forward)
		 (if search-dialog-regexp
		     're-search-backward
		   'search-backward))
	       (glyph-image-property
		(car (glyph-image-property
		      (nth 1 (glyph-image-property
			    search-dialog :items domain))
		      :items domain)) :text domain))
      (isearch-highlight (match-beginning 0) (match-end 0)))))

(defun make-search-dialog ()
  "Popup a search dialog box."
  (interactive)
  (let ((parent (selected-frame)))
    (make-dialog-box
     'general
     :parent parent
     :title "Search"
     :autosize t
     :spec
     (setq search-dialog
	   (make-glyph
	    `[layout
	      :orientation horizontal
	      :vertically-justify top
	      :horizontally-justify center
	      :border [string :data "Search"]
	      :items
	      ([layout :orientation vertical
		       :justify top	; implies left also
		       :items
		       ([string :data "Search for:"]
			[button :descriptor "Match Case"
				:style toggle
				:selected (not case-fold-search)
				:callback (setq case-fold-search
						(not case-fold-search))]
			[button :descriptor "Regular Expression"
				:style toggle
				:selected search-dialog-regexp
				:callback (setq search-dialog-regexp
						(not search-dialog-regexp))]
			[button :descriptor "Forwards"
				:style radio
				:selected search-dialog-direction
				:callback (setq search-dialog-direction t)]
			[button :descriptor "Backwards"
				:style radio
				:selected (not search-dialog-direction)
				:callback (setq search-dialog-direction nil)]
			)]
	       [layout :orientation vertical
		       :vertically-justify top
		       :horizontally-justify right
		       :items
		       ([edit-field :width 15 :descriptor "" :active t
				    :initial-focus t]
			[button :width 10 :descriptor "Find Next"
				:callback-ex
				(lambda (image-instance event)
				  (search-dialog-callback ,parent
							  image-instance
							  event))]
			[button :width 10 :descriptor "Cancel"
				:callback-ex
				(lambda (image-instance event)
				  (isearch-dehighlight)
				  (delete-frame
				   (event-channel event)))])])]))
     ;; These are no longer strictly necessary, but not setting a size
     ;; at all yields a much more noticeable resize since the initial
     ;; frame is so big.
     :properties `(height ,(widget-logical-to-character-height 6)
			  width ,(widget-logical-to-character-width 39))
     )))
