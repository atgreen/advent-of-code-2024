(let ((grid (make-hash-table))
      (height 0))
  (loop for line in (uiop:read-file-lines "10.input")
        for row from 0
        do (progn
             (loop for col from 0 below (length line)
                   do (setf (gethash (complex row col) grid) (- (char-code (char line col)) 48)))
             (incf height)))
  (labels ((dfs (height pos peaks)
             (if (eq height 9) (setf (gethash pos peaks) 1)
                 (loop for dir in '(#c(1 0) #c(0 -1) #c(0 1) #c(-1 0))
                       sum (let ((h (gethash (+ pos dir) grid)))
                             (if (eq h (1+ height)) (dfs h (+ pos dir) peaks) 0))))))
    (dolist (part-1? '(t nil))
      (print (loop for row from 0 below height
                   sum (loop for col from 0
                             for height = (gethash (complex row col) grid)
                             until (null height)
                             when (eq height 0)
                               sum (let* ((peaks (make-hash-table))
                                          (p2 (dfs 0 (complex row col) peaks)))
                                     (if part-1? (hash-table-count peaks) p2))))))))
