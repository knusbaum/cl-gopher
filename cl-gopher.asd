(asdf:defsystem #:cl-gopher
  :name "cl-gopher"
  :description "Gopher protocol library"
  :license "BSD 2-Clause"
  :author "Kyle Nusbaum"
  :depends-on (#:split-sequence #:usocket #:flexi-streams #:drakma #:bordeaux-threads #:quri)
  :components ((:file "cl-gopher-package")
               (:file "cl-gopher"
                      :depends-on ("cl-gopher-package"))
               (:file "client"
                      :depends-on ("cl-gopher"))))
