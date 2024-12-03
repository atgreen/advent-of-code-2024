(asdf:load-system :cl-ppcre)

(labels ((part-1 (input)
           (loop for mul in (cl-ppcre:all-matches-as-strings
                             "(mul\\((\\d+),(\\d+)\\))" input)
                 sum (cl-ppcre:register-groups-bind ((#'parse-integer x y))
                         ("mul\\((\\d+),(\\d+)\\)" mul)
                       (* x y)))))
  (print (part-1 (uiop:read-file-string "03.input")))
  (print (loop for code in (cl-ppcre:all-matches-as-strings
                            "do\\(\\)(.*?)don't\\(\\)"
                            (concatenate 'string "do()" (cl-ppcre:regex-replace-all "\\n" (uiop:read-file-string "03.input") "") "don't()"))
               sum (part-1 code))))
