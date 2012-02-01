;;;autoload
(deftheme europe
  "Settings for European users."
  :set-variable-settings
    "This variable has a value appropriate for European users."
  :set-variable-settings
    "This has been forceed to the value appropriate for European users.")

(custom-theme-set-variables 'europe
   '(sentence-end-double-space nil)
   '(ps-paper-type (quote a4)))

(provide-theme 'europe)
