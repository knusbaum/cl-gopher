(defpackage :cl-gopher
  (:use :cl :split-sequence)
  (:export line-interactive
           display-lines
           display-directory
           display-text-file
           gopher-get-directory
           get-text-file-lines))

