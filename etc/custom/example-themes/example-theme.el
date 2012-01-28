;;;autoload
(deftheme example
  "A sample theme for customize theme support."
  :variable-set-string "This variable has been made an example.")

(custom-theme-load-themes 'example
    'europe)

(custom-theme-set-variables 'example
 '(iswitchb-prompt-newbuffer nil))

(provide-theme 'example)
