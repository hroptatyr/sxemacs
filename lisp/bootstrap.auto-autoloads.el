(if (featurep 'lisp-autoloads) (error "Already loaded"))

;;;***

(autoload 'list-load-path-shadows "shadow" "" t nil)
(autoload 'dolist "cl-macs" "" nil 'macro)
(autoload 'batch-byte-compile "bytecomp" "" nil nil)

;;;***

(provide 'lisp-autoloads)
