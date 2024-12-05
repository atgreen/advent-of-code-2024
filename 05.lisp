(asdf:load-system :cl-ppcre)

(let* ((lines (uiop:read-file-lines "05.input"))
       (rules (remove-if-not (lambda (line) (find #\| line)) lines))
       (updates (remove-if-not (lambda (line) (find #\, line)) lines))
       (bad-updates nil))
  (print (loop for update in updates
               sum (loop for i from 0 upto (- (count #\, update) 1)
                         for pair = (subseq update (* i 3) (+ (* i 3) 5))
                         when (not (find (cl-ppcre:regex-replace "," pair "|") rules :test 'equal))
                           do (progn (push update bad-updates)
                                     (return 0))
                         finally (let ((pages (uiop:split-string update :separator '(#\,))))
                                   (return (parse-integer (nth (floor (length pages) 2) pages)))))))
  (print (loop for update in bad-updates
               sum (let* ((pages (uiop:split-string update :separator '(#\,)))
                          (sorted-pages (sort pages (lambda (a b)
                                                      (find (concatenate 'string a "|" b) rules :test 'equal)))))
                     (parse-integer (nth (floor (length sorted-pages) 2) sorted-pages))))))
