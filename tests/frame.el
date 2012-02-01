;;; Test geometry settings for frames
(defmacro check-frame-geometry (xx yy)
  `(loop for frame in (list nil (selected-frame))
	 do
	 (assert (eq (frame-property frame 'top)  ,yy))
	 (assert (eq (frame-property frame 'left) ,xx))
	 (assert (eq (frame-property frame 'top)  ,yy))
	 (assert (eq (frame-property frame 'left) ,xx))
	 (loop for plist in
	       (list (frame-properties)
		     (frame-properties nil)
		     (frame-properties (selected-frame)))
	       do
	       (assert (eq (plist-get plist 'top)  ,yy))
	       (assert (eq (plist-get plist 'left) ,xx)))))

(loop for (x y) in '((0 0) (1 1) (3 3) (9 9) (10 20) (20 40) (40 80) (100 200))
      do
      (loop for frame in (list nil (selected-frame))
	    do
	    (set-frame-properties frame `(left ,x top ,y))
	    (check-frame-geometry x y)
	    (set-frame-property frame 'top (+ y 3))
	    (check-frame-geometry x (+ y 3))
	    (set-frame-property frame 'left (+ x 3))
	    (check-frame-geometry (+ x 3) (+ y 3))))
