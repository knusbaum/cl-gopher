(asdf:defsystem #:cl-gopher
  :name "cl-gopher"
  :description "Gopher protocol client library"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:split-sequence #:iolib)
  :components ((:file "cl-gopher-package")
               (:file "cl-gopher"
                      :depends-on ("cl-gopher-package"))))

