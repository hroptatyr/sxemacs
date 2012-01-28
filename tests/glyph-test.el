(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq im (make-glyph [xbm :file "xemacsicon.xbm"])))

(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph [string :data "xemacs"]))

(defun foo ()
  (interactive)
  (setq ok-select (not ok-select)))

(defun fee () (interactive) (message "hello"))

;; button in a group
(setq ok-select nil)
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph
  (setq radio-button1
	[button :face widget
		:descriptor ["ok1" (setq ok-select t)
			     :style radio :selected ok-select]])))
;; button in a group
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph
  (setq radio-button2
	[button :descriptor ["ok2" (setq ok-select nil) :style radio
			     :selected (not ok-select)]])))
;; toggle button
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq tbutton
       (make-glyph [button :descriptor ["ok3" (setq ok-select nil)
					:style toggle
					:selected (not ok-select)]])))
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph
  (setq toggle-button
	[button :descriptor ["ok4" :style toggle
			     :callback
			     (setq ok-select (not ok-select))
			     :selected ok-select]])))

;; normal pushbutton
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq push-button
       (make-glyph [button :width 10 :height 2
			   :face modeline-mousable
			   :descriptor "ok" :callback foo
			   :selected t])))
;; tree view
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq tree (make-glyph
	     [tree-view :width 10
			:descriptor "My Tree"
			:items (["One" foo]
				(["Two" foo]
				 ["Four" foo]
				 "Six")
				"Three")])))

;; tab control
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq tab (make-glyph
	    [tab-control :descriptor "My Tab"
			 :face highlight
			 :orientation right
			 :items (["One" foo :selected t]
				 ["Two" fee :selected nil]
				 ["Three" foo :selected nil])])))

;; progress gauge
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq pgauge (make-glyph
	       [progress-gauge :width 10 :height 2 :value 0
			       :descriptor "ok"])))
;; progress the progress ...
(let ((x 0))
  (while (<= x 100)
    (set-glyph-image pgauge `[progress-gauge :width 10 :height 2
					     :descriptor "ok" :value ,x])
    (setq x (+ x 5))
    (sit-for 0.1)))

;; progress gauge in the modeline
(setq global-mode-string
      (cons (make-extent nil nil)
	    (setq pg (make-glyph
		      [progress-gauge :width 5 :pixel-height 16
				      :descriptor "ok"]))))
;; progress the progress ...
(let ((x 0))
  (while (<= x 100)
    (set-glyph-image pg
		     `[progress-gauge :width 5 :pixel-height 16
				      :descriptor "ok" :value ,x])
    (setq x (+ x 5))
    (redisplay-frame)
    (sit-for 0.1)))

(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph
  [button :face modeline-mousable
	  :descriptor "ok" :callback foo
	  :image [xpm :file "../etc/xemacs-icon.xpm"]]))

;; normal pushbutton
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq pbutton
       (make-glyph [button :descriptor ["A Big Button" foo ]])))

;; edit box
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph (setq edit-field [edit-field :pixel-width 50 :pixel-height 30
					  :face bold-italic
					  :descriptor ["Hello"]])))
;; combo box
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph (setq combo-box
		   [combo-box :width 10 :descriptor ["Hello"]
			      :items ("One" "Two" "Three")])))

;; label
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph (setq label [label :pixel-width 150 :descriptor "Hello"])))

;; string
(set-extent-begin-glyph
 (make-extent (point) (point))
 (make-glyph
  (setq str
	[string :data "Hello There"])))

;; scrollbar
;(set-extent-begin-glyph
; (make-extent (point) (point))
; (make-glyph [scrollbar :width 50 :height 20 :descriptor ["Hello"]]))

;; generic subwindow
(setq sw (make-glyph [subwindow :pixel-width 50 :pixel-height 70]))
(set-extent-begin-glyph (make-extent (point) (point)) sw)

;; layout
(setq layout
      (make-glyph
       `[layout :descriptor "The Layout"
		:orientation vertical
		:justify left
		:border [string :data "Hello There Mrs"]
		:items ([layout :orientation horizontal
				:items (,radio-button1 ,radio-button2)]
			,edit-field ,toggle-button ,label ,str)]))
;(set-glyph-face layout 'gui-element)
(set-extent-begin-glyph
 (make-extent (point) (point)) layout)

;; another test layout
(set-extent-begin-glyph
 (make-extent (point) (point))
 (setq layout-2
       (make-glyph `[layout :descriptor "The Layout"
			    :orientation vertical
			    :items ([progress-gauge :value 0 :width 10 :height 2
						    :descriptor "ok"])])))

(set-glyph-image layout-2 `[layout :descriptor "The Layout"
				   :orientation vertical
				   :items ([progress-gauge :value 4 :width 10 :height 2
							   :descriptor "ok"])])
(setq test-toggle-widget nil)

(defun test-toggle (widget)
  (set-extent-begin-glyph
   (make-extent (point) (point))
   (make-glyph (vector 'button
		       :descriptor "ok"
		       :style 'toggle
		       :selected `(funcall test-toggle-value
					   ,widget)
		       :callback `(funcall test-toggle-action
					   ,widget)))))

(defun test-toggle-action (widget &optional event)
  (if widget
      (message "Widget is t")
    (message "Widget is nil")))

(defun test-toggle-value (widget)
  (setq widget (not widget))
  (not widget))
