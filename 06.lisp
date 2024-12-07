(asdf:load-system :alexandria)

(labels ((run (grid pos)
           (let ((count 0)
                 (dirs #(#c(-1 0) #c(0 1) #c(1 0) #c(0 -1)))
                 (dir 0))
             (loop
               until (eq (gethash pos grid) #\X)
               do (if (eq (gethash (+ pos (aref dirs (mod dir 4))) grid) #\#)
                      (setf dir (mod (1+ dir) 4))
                      (progn (incf pos (aref dirs (mod dir 4)))
                             (cond
                               ((eq (gethash pos grid) #\.)
                                (incf count)
                                (setf (gethash pos grid) dir))
                               ((numberp (gethash pos grid))
                                (when (eq (gethash pos grid) dir)
                                  (error (format nil "Loop ~A ~A" pos dir)))
                                (setf (gethash pos grid) dir))
                               (t t)))))
             count)))
  (let ((grid (make-hash-table))
        (count 0))
    (loop for line in (mapcar (lambda (line) (concatenate 'string "X" line "X"))
                              (uiop:read-file-lines "06.input"))
          for row from 0
          do (loop for col from 0 below (length line)
                   when (eq (char line col) #\^)
                     do (setf pos (complex row col)
                              (char line col) #\.)
                   do (setf (gethash (complex row col) grid) (char line col)))
          finally (setf width (length line)
                        height (1+ row)))
    (loop for col from 0 upto width do (setf (gethash (complex -1 col) grid) #\X))
    (loop for col from 0 upto width do (setf (gethash (complex height col) grid) #\X))
    ;; Part 1
    (print (run (alexandria:copy-hash-table grid)  pos))
    ;; Part 2
    (loop for trow from 0 upto height
          do (progn
               (loop for tcol from 0 upto width
                     when (and (not (equal (complex trow tcol) pos))
                               (eq (gethash (complex trow tcol) grid) #\.))
                       do (progn
                            (let ((tgrid (alexandria:copy-hash-table grid)))
                              (setf (gethash (complex trow tcol) tgrid) #\#)
                              (handler-case
                                  (run tgrid pos)
                                (error (e)
                                  (incf count)))))))
          finally (print count))))
