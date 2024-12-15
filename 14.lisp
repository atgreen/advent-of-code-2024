(asdf:load-system :cl-ppcre)

;; Comically slow, but I don't care.
(let ((scanner (ppcre:create-scanner "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)"))
      (answers (make-hash-table))
      (data (list)))
  (ppcre:do-register-groups ((#'parse-integer x y vx vy))
      (scanner (uiop:read-file-string #P"14.input"))
    (push (list x y vx vy) data))
  (print (gethash (loop for e from 1 upto (lcm 101 103)
                        minimize (let ((q1 0) (q2 0) (q3 0) (q4 0))
                                   (dolist (d data)
                                     (destructuring-bind (x y vx vy) d
                                       (loop for i from 1 upto e
                                             do (progn
                                                  (setf x (mod (+ x vx) 101))
                                                  (setf y (mod (+ y vy) 103)))
                                             finally (progn
                                                       (cond
                                                         ((and (< x 50) (< y 51)) (incf q1))
                                                         ((and (< x 50) (> y 51)) (incf q2))
                                                         ((and (> x 50) (< y 51)) (incf q3))
                                                         ((and (> x 50) (> y 51)) (incf q4)))))))
                                   (let ((safety (* q1 q2 q3 q4)))
                                     (when (= e 100) (print safety))
                                     (setf (gethash safety answers) e)
                                     safety)))
                  answers)))
(quit)
