(setq str "Hello\nAgain")
(setq str-ext (make-extent 0 5 str))
(set-extent-begin-glyph
 str-ext
 (make-glyph [xpm :file "../etc/xemacs-icon.xpm"]))
(set-extent-property str-ext 'mouse-face 'highlight)

(setq str2 "Hello\n")
(setq str2-ext (make-extent 0 1 str2))
(set-extent-begin-glyph
 str2-ext
 (make-glyph
  [button :width 5 :height 1
	  :face modeline-mousable
	  :callback (set-specifier bottom-gutter-visible-p '(str2))
	  :descriptor "ok" :selected t]))

(set-specifier bottom-gutter-height 'autodetect)
(set-specifier bottom-gutter-border-width 2)

(set-gutter-element
 bottom-gutter 'str
 (make-glyph
  [layout :orientation vertical
	  :justify left :margin-width 4
	  :items ([string :data "Fontifying glyphs.c..."]
		  [layout :orientation horizontal
			  :items
			  ([progress-gauge :value 0 :pixel-height 24
					   :pixel-width 250 :descriptor
					   "Progress"]
			   [button :pixel-height 24
				   :descriptor " Stop "
				   :callback (quote quit)])])]))

(set-gutter-element bottom-gutter 'str2 str2)
(set-gutter-element-visible-p bottom-gutter-visible-p 'str t)
(set-gutter-element-visible-p bottom-gutter-visible-p 'str2 t)
