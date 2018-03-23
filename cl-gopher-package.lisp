(defpackage :cl-gopher
  (:use :cl :split-sequence)
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
   marshall-gopher-line
   marshall-gopher-lines
   unmarshall-gopher-line
   unmarshall-gopher-lines

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

   ;; Various util functions
   line-type
   copy-gopher-line
   download-file

   ;; Built-in clients
   text-browser
   network-browser))
