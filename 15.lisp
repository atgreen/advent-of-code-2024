(asdf:load-system :cl-ppcre)

(let (pos)
  (destructuring-bind (grid-string dirs-string)
      (ppcre:split "\\n\\n" (uiop:read-file-string "15.input"))
    (let ((grid-map (ppcre:split "\\n" grid-string))
          (grid (make-hash-table)))
      (loop for row-string in grid-map
            for row from 0
            do (loop for char across row-string
                     for col from 0
                     when (eq char #\@)
                       do (setf pos (complex row col))
                     do (setf (gethash (complex row col) grid) char)))
      (setf (gethash pos grid) #\.)
      (labels ((move (dir)
                 (let ((next-char (gethash (+ pos dir) grid)))
                   (cond
                     ((eq next-char #\#)
                      nil)
                     ((eq next-char #\.)
                      (setf pos (+ pos dir)))
                     ((eq next-char #\O)
                      (loop for p = (+ pos dir) then (+ p dir)
                            until (member (gethash p grid) '(#\. #\#))
                            finally (if (eq (gethash p grid) #\.)
                                        (progn (setf (gethash p grid) #\O
                                                     (gethash (+ pos dir) grid) #\.)
                                               (setf pos (+ pos dir))))))))))
        (loop for dir-char across dirs-string
              for dir = (cdr (assoc dir-char '((#\< . #c(0 -1)) (#\^ . #c(-1 0)) (#\> . #c(0 1)) (#\v . #c(1 0)))))
              when dir
                do (move dir))
        (print (loop for pos being the hash-keys of grid
                     for value = (gethash pos grid)
                     when (eq value #\O)
                       sum (+ (* 100 (realpart pos)) (imagpart pos))))))))
