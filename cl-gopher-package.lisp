(defpackage :cl-gopher
  (:use :cl)
  (:documentation
   #.(format nil "cl-gopher is a Common Lisp library for interacting with~@
                  the Gopher protocol. It is suitable for building both~@
                  clients and servers, and provides a sample client.~@
                  See (describe 'cl-gopher:text-browser) and~@
                  (describe 'cl-gopher:network-browser)~@
                  ~@
                  cl-gopher has been tested and confirmed to work with:~@
                  SBCL 1.4.6~@
                  ClozureCL Version 1.11.5/v1.11.5~@
                  ECL 16.1.3~@
                  ~@
                  Most classes and functions have documentation associated~@
                  with them, and can be shown with the DESCRIBE function.~@
                  ~@
                  For a brief overview, look at the readme distributed with~@
                  the source, or available at~@
                  https://github.com/knusbaum/cl-gopher/blob/master/README.md"))

  (:export
   ;; Line classes
   gopher-line
   text-file
   submenu
   ccso-nameserver
   error-code
   binhex-file
   dos-file
   uuencoded-file
   search-line
   telnet
   binary-file
   mirror
   gif
   image
   png
   telnet-3270
   html-file
   info-message
   sound-file
   unknown ;Used for unidentifiable or invalid lines

   ;; Line class accessors
   display-string
   selector
   hostname
   port
   terms

   ;; Contents classes
   selector-contents
   submenu-contents
   text-file-contents
   html-file-contents
   binary-file-contents

   ;; Contents accessors
   lines
   content-string
   file-name
   content-array

   ;; Contents functions
   display-contents
   get-line-contents

   ;; Convert gopher line classes to/from lisp
   ;; forms that can be read/written to files, etc.
   gopher-line-to-alist
   gopher-lines-to-alist
   gopher-line-from-alist
   gopher-lines-from-alist

   ;; Reading gopher lines from a stream
   read-gopher-line

   ;; Displaying gopher lines as text
   display-line
   display-lines

   ;; Write a gopher line out in submenu format
   write-gopher-line

   ;; Parse a gopher URI
   parse-gopher-uri
   uri-for-gopher-line
   bad-uri-error

   ;; Various util functions
   line-type
   copy-gopher-line
   download-file
   convert-to-text-line
   bad-submenu-error

   ;; Built-in clients
   text-browser
   network-browser))
