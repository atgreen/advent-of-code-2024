(labels ((part-1-safe? (numbers)
           (let ((differences (loop for (a b) on numbers while b collect (- b a))))
             (if (and (or (every #'(lambda (x) (< x 0)) differences)
                          (every #'(lambda (x) (> x 0)) differences))
                      (every #'(lambda (x) (> 4 (abs x))) differences))
                 1 0)))
         (part-2-safe? (numbers)
           (if (< 0 (loop for i from 0 upto (- (length numbers) 1)
                          sum (part-1-safe? (concatenate 'list
                                                         (subseq numbers 0 i)
                                                         (subseq numbers (+ i 1))))))
               1 0))
         (process (checker)
           (loop for line in (uiop:read-file-lines "02.input")
                 sum (funcall checker (mapcar #'parse-integer (uiop:split-string line))))))
  (print (process #'part-1-safe?))
  (print (process #'part-2-safe?)))
