;; We can just cheat and use the same code that X does.

(setq character-set-property 'x-iso8859/1) ; see x-iso8859-1.el
(require 'x-iso8859-1)
(provide 'gtk-iso8859-1)
