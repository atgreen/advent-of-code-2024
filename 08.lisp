(asdf:load-system :alexandria)

(let ((antennas (make-hash-table))
      (grid (make-hash-table)))
  (loop for line in (uiop:read-file-lines "08.input")
        for row from 0
        do (loop for col from 0 below (length line)
                 do (setf (gethash (complex row col) grid) t)
                 unless (eq (char line col) #\.)
                   do (push (complex row col) (gethash (char line col) antennas))))
  (let ((part-1-table (make-hash-table))
        (part-2-table (make-hash-table)))
    (maphash (lambda (freq locations)
               (alexandria:map-combinations
                (lambda (c)
                  (let ((diff (apply #'- c)))
                    (dolist (loc (list (+ (car c) diff) (- (cadr c) diff)))
                      (when (gethash loc grid)
                        (setf (gethash loc part-1-table) t)))
                    (maphash (lambda (loc v)
                               (when (zerop (imagpart (/ (- loc (car c)) diff)))
                                 (setf (gethash loc part-2-table) t)))
                             grid)))
                locations :length 2))
             antennas)
    (print (hash-table-count part-1-table))
    (print (hash-table-count part-2-table))))
