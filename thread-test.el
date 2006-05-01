;; 
;; (dotimes (i 100)
;;   (play-sound 'quit)
;;   ;;(test-threads)
;;   (sleep-for 2))
;; 
;; (dotimes (i 100)
;;   (play-sound 'quit)
;;   ;;(test-threads)
;;   (sleep-for 1))
;; 
;; (dotimes (i 100)
;;   (play-sound 'quit)
;;   ;;(test-threads)
;;   (sleep-for 0.5))
;; 
;; (dotimes (i 100)
;;   (play-sound 'quit)
;;   ;;(test-threads)
;;   (sleep-for 0.1))
;; 
;; (dotimes (i 100)
;;   (test-threads))
;; 

(init-asynchronousity)
(signal-thread)
(signal-thread)
(signal-thread)
(signal-thread)
(signal-thread)
(uninit-asynchronousity)

