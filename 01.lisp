(let* ((lines (uiop:read-file-lines "01.input"))
       (left (sort (mapcar (lambda (line) (parse-integer line :junk-allowed t)) lines) #'<))
       (right (sort (mapcar (lambda (line) (parse-integer (subseq line 8))) lines) #'<)))
  (print (loop for lnum in left for rnum in right sum (abs (- lnum rnum))))
  (print (loop for lnum in left sum (* lnum (count lnum right)))))
