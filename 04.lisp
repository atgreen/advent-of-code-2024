(let* ((grid (make-hash-table)))
  (loop for line in (uiop:read-file-lines "04.input")
        for row from 0
        do (loop for col from 0 below (length line)
                 do (setf (gethash (complex row col) grid) (char line col)))
        finally (setf width (length line)
                      height row))
  ;; Part 1
  (let ((count 0))
    (loop for move in '(#c(1 -1) #c(1 0) #c(1 1) #c(0 -1) #c(0 1) #c(-1 -1) #c(-1 0) #c(-1 1))
          do (loop for x from 0 upto width
                   do (loop for y from 0 upto height
                            do (let ((pos (complex x y))
                                     (word nil))
                                 (dotimes (i 4)
                                   (push (gethash pos grid) word)
                                   (incf pos move))
                                 (when (equal word '(#\S #\A #\M #\X))
                                   (incf count)))))
          finally (print count)))

  ;; Part 2
  (let ((count 0))
    (loop for x from 0 upto width
          do (loop for y from 0 upto height
                   do (let ((pos (complex x y))
                            (word nil))
                        (when (and (eq (gethash pos grid) #\A)
                                   (or (and (eq (gethash (+ pos #c(-1 -1)) grid) #\M)
                                            (eq (gethash (+ pos #c(1 1)) grid) #\S))
                                       (and (eq (gethash (+ pos #c(-1 -1)) grid) #\S)
                                            (eq (gethash (+ pos #c(1 1)) grid) #\M)))
                                   (or (and (eq (gethash (+ pos #c(-1 1)) grid) #\M)
                                            (eq (gethash (+ pos #c(1 -1)) grid) #\S))
                                       (and (eq (gethash (+ pos #c(-1 1)) grid) #\S)
                                            (eq (gethash (+ pos #c(1 -1)) grid) #\M))))
                        (incf count))))
          finally (print count))))
