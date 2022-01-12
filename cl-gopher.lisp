(in-package :cl-gopher)

(defclass selector-contents () ()
  (:documentation
   #.(format nil "A SELECTOR-CONTENTS object contains the contents pointed~@
                  to by a GOPHER-LINE object. This class is never directly~@
                  used, but instead, one of its subclasses is returned~@
                  when calling GET-LINE-CONTENTS.")))

(defclass submenu-contents (selector-contents)
  ((lines :initform nil :initarg :lines :accessor lines))
  (:documentation
   #.(format nil "A SUBMENU-CONTENTS object contains a gopher submenu.~@
                  The single slot, LINES, is a list of GOPHER-LINE~@
                  objects representing all of the menu items in the~@
                  submenu.")))

(defclass text-file-contents (selector-contents)
  ((lines :initform nil :initarg :lines :accessor lines))
  (:documentation
   #.(format nil "A TEXT-FILE-CONTENTS object contains a text file.~@
                  The single slot, LINES, is a list of string, each~@
                  element of which is a single line of text from the~@
                  text file.")))

(defclass html-file-contents (selector-contents)
  ((content-string :initform nil :initarg :content-string :accessor content-string))
  (:documentation
   #.(format nil "An HTML-FILE-CONTENTS object contains the raw html~@
                  text pointed to by an HTML-FILE object (a type of~@
                  GOPHER-LINE). You may or may not find this useful,~@
                  since often when dealing with HTTP links through~@
                  gopher, it is more useful to deal with the URL,~@
                  which is available through the HTML-FILE object.")))

(defclass binary-file-contents (selector-contents)
  ((file-name :initform nil :initarg :file-name :accessor file-name)
   (content-array :initform nil :initarg :content-array :accessor content-array))
  (:documentation
   #.(format nil "A BINARY-FILE-CONTENTS object contains the bytes~@
                  from various GOPHER-LINE subclasses, such as~@
                  BINARY-FILE, IMAGE, GIF, PNG, SOUND-FILE, etc.~@
                  ~@
                  The FILE-NAME slot is just a convenience for the~@
                  user, in case they wish to write the contents to~@
                  a file locally, with a similar name to that~@
                  suggested by the GOPHER-LINE selector.~@
                  ~@
                  The CONTENT-ARRAY is a vector of (UNSIGNED-BYTE 8)~@
                  which contains all the bytes from the file.")))

(defgeneric display-contents (contents &key stream)
  (:documentation
   #.(format nil "DISPLAY-CONTENTS will write out a contents object in a~@
                  human-readable format to the stream STREAM.")))

(defmethod display-contents ((contents submenu-contents) &key (stream *standard-output*))
  (display-lines (lines contents) :with-line-nums t :stream stream))

(defmethod display-contents ((contents text-file-contents) &key (stream *standard-output*))
  (loop for line in (lines contents)
        do (format stream "~a~%" line)))

(defmethod display-contents ((contents html-file-contents) &key (stream *standard-output*))
  (write-string (content-string contents) stream))

(defmethod display-contents ((contents binary-file-contents) &key (stream *standard-output*))
  (format stream "Binary file \"~a\" of length ~a bytes~%"
          (file-name contents) (length (content-array contents))))

(defclass gopher-line ()
  ((display-string :initform nil :initarg :display-string :accessor display-string)
   (selector :initform nil :initarg :selector :accessor selector)
   (hostname :initform nil :initarg :hostname :accessor hostname)
   (port :initform nil :initarg :port :accessor port)
   (terms :initform "" :initarg :terms :accessor terms))
  (:documentation
   #.(format nil "A GOPHER-LINE represents a gopher menu item,~@
                  (analogous to an html link).~@
                  ~@
                  See the subclasses of GOPHER-LINE, which represent~@
                  the various kinds of menu items supported by this~@
                  library.")))


(defclass text-file (gopher-line) ())
(defclass submenu (gopher-line) ())
(defclass ccso-nameserver (gopher-line) ())
(defclass error-code (gopher-line) ())
(defclass binhex-file (gopher-line) ())
(defclass dos-file (gopher-line) ())
(defclass uuencoded-file (gopher-line) ())
(defclass search-line (gopher-line))
(defclass telnet (gopher-line) ())
(defclass binary-file (gopher-line) ())
(defclass mirror (gopher-line) ())
(defclass gif (gopher-line) ())
(defclass image (gopher-line) ())
(defclass png (gopher-line) ())
(defclass telnet-3270 (gopher-line) ())
(defclass html-file (gopher-line) ())
(defclass info-message (gopher-line) ())
(defclass sound-file (gopher-line) ())
(defclass unknown (gopher-line) ())

(defun class-for-type (type)
  (find-symbol (string type) :cl-gopher))

(defun line-type (gl)
  #.(format nil "LINE-TYPE returns a keyword describing the type of the~@
                 GOPHER-LINE gl.")
  (find-symbol (string (type-of gl)) :keyword))

(defun type-for-character (c)
  (case c
    (#\0 :text-file)
    (#\1 :submenu)
    (#\2 :ccso-nameserver)
    (#\3 :error-code)
    (#\4 :binhex-file)
    (#\5 :dos-file)
    (#\6 :uuencoded-file)
    (#\7 :search-line)
    (#\8 :telnet)
    (#\9 :binary-file)
    (#\+ :mirror)
    (#\g :gif)
    (#\I :image)
    (#\p :png)
    (#\T :telnet-3270)
    (#\h :html-file)
    (#\i :info-message)
    (#\s :sound-file)
    (t :unknown)))

(defgeneric type-character (gl))
(defmethod type-character ((gl text-file)) #\0)
(defmethod type-character ((gl submenu)) #\1)
(defmethod type-character ((gl ccso-nameserver)) #\2)
(defmethod type-character ((gl error-code)) #\3)
(defmethod type-character ((gl binhex-file)) #\4)
(defmethod type-character ((gl dos-file)) #\5)
(defmethod type-character ((gl uuencoded-file)) #\6)
(defmethod type-character ((gl search-line)) #\7)
(defmethod type-character ((gl telnet)) #\8)
(defmethod type-character ((gl binary-file)) #\9)
(defmethod type-character ((gl mirror)) #\+)
(defmethod type-character ((gl gif)) #\g)
(defmethod type-character ((gl image)) #\I)
(defmethod type-character ((gl png)) #\p)
(defmethod type-character ((gl telnet-3270)) #\T)
(defmethod type-character ((gl html-file)) #\h)
(defmethod type-character ((gl info-message)) #\i)
(defmethod type-character ((gl sound-file)) #\s)
(defmethod type-character ((gl gopher-line)) #\?) ; Catch-all

(defgeneric copy-gopher-line (gl)
  (:documentation
   #.(format nil "Returns a new GOPHER-LINE that is a copy of the~@
                  GOPHER-LINE gl.")))

(defmethod copy-gopher-line ((gl gopher-line))
  (make-instance (class-of gl)
                 :display-string (display-string gl)
                 :selector (selector gl)
                 :hostname (hostname gl)
                 :port (port gl)
                 :terms (terms gl)))

(defun convert-to-text-line (gl)
  #.(format nil "CONVERT-TO-TEXT-LINE takes a GOPHER-LINE and returns~@
                 a copy of it that is of type TEXT-FILE.~@
                 ~@
                 This is only seldom useful, but can be used in case~@
                 you want to treat a gopher resource as a text file~@
                 rather than whatever kind of resource it actually is.~@
                 ~@
                 For example, this can be used to treat a gopher menu~@
                 as a plain-text file, allowing you to see the lines~@
                 as the protocol specifies.")
  (make-instance 'text-file
                 :display-string (display-string gl)
                 :selector (selector gl)
                 :hostname (hostname gl)
                 :port (port gl)))

(defmethod print-object ((gl gopher-line) stream)
  (print-unreadable-object (gl stream :type t)
    (format stream "String: [~a], Selector: [~a], Host: [~a:~a]"
            (display-string gl) (selector gl) (hostname gl) (port gl))))

(defgeneric gopher-line-to-alist (gl)
  (:documentation
   #.(format nil "GOPHER-LINE-TO-ALIST converts a GOPHER-LINE object to~@
                  an alist. These alists are suitable to be read/written~@
                  by the lisp reader and writer. This can be useful in~@
                  conjunction with GOPHER-LINE-FROM-ALIST, which performs~@
                  the reverse operation.")))

(defmethod gopher-line-to-alist ((gl gopher-line))
  (let ((lst))
    (push (cons :line-type (line-type gl)) lst)
    (push (cons :display-string (display-string gl)) lst)
    (push (cons :selector (selector gl)) lst)
    (push (cons :hostname (hostname gl)) lst)
    (push (cons :port (port gl)) lst)
    lst))

(defun gopher-lines-to-alist (gls)
  #.(format nil "GOPHER-LINES-TO-ALIST converts a list of GOPHER-LINE to~@
                 a list of alist using the GOPHER-LINE-TO-ALIST function.")
  (loop for line in gls
        collect (gopher-line-to-alist line)))

(defun gopher-line-from-alist (gl)
  #.(format nil "GOPHER-LINE-FROM-ALIST converts an alist to a GOPHER-LINE.~@
                 This is primarily useful in conjunction with the function~@
                 GOPHER-LINE-TO-ALIST, which converts a GOPHER-LINE to an~@
                 alist.")
  (let ((line-type (cdr (assoc :line-type gl))))
    (make-instance (class-for-type line-type)
                   :display-string (cdr (assoc :display-string gl))
                   :selector (cdr (assoc :selector gl))
                   :hostname (cdr (assoc :hostname gl))
                   :port (cdr (assoc :port gl)))))

(defun gopher-lines-from-alist (gls)
  #.(format nil "GOPHER-LINES-FROM-ALIST converts a list of alists to a~@
                 list of GOPHER-LINES using the GOPHER-LINE-FROM-ALIST~@
                 function.")
  (loop for line in gls
        collect (gopher-line-from-alist line)))

(define-condition bad-submenu-error (error) ())

(defun make-unknown (line-elems)
  ;; Can probably do a better job of determining
  ;; if this line is gopher-like or not.
  ;; Currently, we just check if it has 4 elements
  ;; that are separated by tabs.
  (if (< (length line-elems) 4)
      (error 'bad-submenu-error)
      (let* ((initial-display-string (string-trim '(#\Space #\Tab) (elt line-elems 0)))
             (display-string (if (equal initial-display-string "")
                                "Unknown or Invalid line"
                                initial-display-string)))
        (make-instance 'unknown
                       :display-string display-string
                       :selector (elt line-elems 1)
                       :hostname (elt line-elems 2)
                       :port (parse-integer (elt line-elems 3))))))


(defun read-gopher-line (is)
  #.(format nil "READ-GOPHER-LINE reads and returns a GOPHER-LINE from a~@
                 stream that contains gopher lines in the format a gopher~@
                 server should return upon request of a gopher menu,~@
                 specified by RFC 1436 (https://tools.ietf.org/html/rfc1436)")
  (let* ((line (read-line is nil nil)))
    (when (and line
               (not (equal line "."))
               (> (length line) 0))
      (let ((line-elems (split-sequence #\tab (subseq line 1)))
            (type (type-for-character (elt line 0))))
        (if (eq type :unknown)
            (make-unknown line-elems)
            (make-instance (class-for-type type)
                           :display-string (elt line-elems 0)
                           :selector (elt line-elems 1)
                           :hostname (elt line-elems 2)
                           :port (parse-integer (elt line-elems 3))))))))

(defmacro with-gopher-socket-for-selector ((stream host port selector) &rest body)
  (let ((sock (gensym "sock")))
    `(let* ((,sock (usocket:socket-connect ,host ,port :element-type '(unsigned-byte 8)))
            (,stream (flexi-streams:make-flexi-stream
                      (usocket:socket-stream ,sock)
                      :external-format (flexi-streams:make-external-format :iso-8859-1
                                                                           :eol-style :crlf)))
            (babel-encodings:*suppress-character-coding-errors* t))
       (unwind-protect
            (progn
              (write-line ,selector ,stream)
              (force-output ,stream)
              ,@body)
         (close ,stream)
         (usocket:socket-close ,sock)))))

(defgeneric get-line-contents (gl)
  (:documentation
   #.(format nil "GET-LINE-CONTENTS takes a GOPHER-LINE and returns an~@
                  object of type SELECTOR-CONTENTS (or one of its subclasses)~@
                  It does this by contacting the target server and requesting~@
                  the resource represented by the GOPHER-LINE.")))

(defmethod get-line-contents ((gl gopher-line))
  (let ((byte-arr (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (with-slots (hostname port selector) gl
      (with-gopher-socket-for-selector (sock-stream hostname port selector)
        (loop for c = (read-byte sock-stream nil nil)
              while c
              do (vector-push-extend c byte-arr)))
      (let ((filename (file-namestring selector)))
        (make-instance 'binary-file-contents
                       :content-array byte-arr
                       :file-name filename)))))

(defun retreive-submenu-contents (hostname port selector)
  (with-gopher-socket-for-selector (sock-stream hostname port selector)
    (make-instance 'submenu-contents
                   :lines (loop for line = (read-gopher-line sock-stream)
                                while line
                                collect line))))

(defmethod get-line-contents ((gl submenu))
  (with-slots (hostname port selector) gl
    (retreive-submenu-contents hostname port selector)))

(defmethod get-line-contents ((gl search-line))
  (let ((selector (format nil "~a~a~a"
                          (selector gl)
                          #\tab
                          (terms gl))))
    (retreive-submenu-contents (hostname gl) (port gl) selector)))

(defmethod get-line-contents ((gl text-file))
  (with-slots (hostname port selector) gl
    (with-gopher-socket-for-selector (sock-stream hostname port selector)
      (make-instance 'text-file-contents
                     :lines (loop for line = (read-line sock-stream nil nil)
                                  while line
                                  collect line)))))

(defmethod get-line-contents ((gl html-file))
  (with-slots (hostname port selector) gl
    (when (and
           (> (length selector) 4)
           (equal (subseq selector 0 4) "URL:"))
      (make-instance 'html-file-contents
                     :content-string (drakma:http-request (subseq selector 4))))))

(defgeneric display-line (gl &key stream line-number show-target include-newline)
  (:documentation
   #.(format nil "Display a GOPHER-LINE in a human-readable format. This~@
                  is suitable for displaying lines from a menu in a text~@
                  based client.~@
                  ~@
                  STREAM - the stream to write the line to. Defaults to~@
                           *STANDARD-OUTPUT*~@
                  ~@
                  LINE-NUMBER - if specified, a number to display at the~@
                                beginning of the line.~@
                  ~@
                  SHOW-TARGET - if not nil, display the gopher URI~@
                                that the line links to.~@
                  ~@
                  INCLUDE-NEWLINE - if not nil, include a newline at the~@
                                    end of the line.")))

(defmethod display-line ((gl gopher-line) &key (stream *standard-output*) line-number show-target include-newline)
  (if show-target
      (format stream "~6a ~14a ~a    ~a:~a~a~:[~;~%~]"
              (or line-number #\Space)
              (line-type gl) (display-string gl)
              (hostname gl) (port gl) (selector gl) include-newline)
      (format stream "~6a ~a ~a~:[~;~%~]"
              (or line-number #\Space)
              (line-type gl) (display-string gl) include-newline)))

(defmethod display-line ((gl info-message) &key (stream *standard-output*) line-number show-target include-newline)
  (declare (ignore line-number show-target))
  (format stream "~a~a~:[~;~%~]" #\tab (display-string gl) include-newline))

(defun display-lines (lines &key (stream *standard-output*) with-line-nums show-target)
  #.(format nil "DISPLAY-LINES displays a list of GOPHER-LINE in a human-~@
                 readable way on STREAM by calling DISPLAY-LINE.~@
                 ~@
                 WITH-LINE-NUMS - if not nil, pass a LINE-NUMBER argument~@
                                  to DISPLAY-LINE corresponding to the~@
                                  line's index in LINES.~@
                 ~@
                 SHOW-TARGET - if not nil, display the gopher URI~@
                               that the line links to.")
  (loop for elem in lines
        for i from 0
        do (if with-line-nums
               (display-line elem :stream stream :show-target show-target :line-number i :include-newline t)
               (display-line elem :stream stream :show-target show-target :include-newline t))))

(defgeneric write-gopher-line (gl &key stream)
  (:documentation
   #.(format nil "Write out a gopher line to a stream, such that a gopher~@
                  client reading the stream on the other end will be able~@
                  to read the line, according to RFC 1436~@
                  (https://tools.ietf.org/html/rfc1436)")))

(defmethod write-gopher-line ((gl gopher-line) &key (stream *standard-output*))
  (format stream "~c~a~c~a~c~a~c~a~%"
          (type-character gl)
          (display-string gl)
          #\Tab
          (selector gl)
          #\Tab
          (hostname gl)
          #\Tab
          (port gl)))

(defmethod write-gopher-line ((gl info-message) &key (stream *standard-output*))
  (format stream "~c~a~c ~cerror.host~c1~%"
          (type-character gl)
          (display-string gl)
          #\Tab
          #\Tab
          #\Tab))

(defun download-file (destfile gl)
  #.(format nil "Download a file pointed to by the GOPHER-LINE gl.~@
                 DESTFILE should be a pathname or string. If DESTFILE~@
                 exists, it will be overwritten. This function just~@
                 writes out whatever bytes are returned by the gopher~@
                 server returns for the GOPHER-LINE.")
  (with-slots (hostname port selector) gl
    (with-gopher-socket-for-selector (sock-stream hostname port selector)
      (with-open-file (os destfile :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (loop with arr = (make-array 2048 :element-type '(unsigned-byte 8))
              for count = (read-sequence arr sock-stream)
              while (> count 0)
              do (write-sequence arr os :end count))))))


;;; URL-parsing

(define-condition bad-uri-error (error)
  ((uri :initarg :uri :reader uri)))

(defmethod print-object ((e bad-uri-error) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "Failed to determine selector and gopher type for URI: ~a" (uri e))))

(defun compute-selector (uri path)
  ;; The root selector or null selector should return "/"
  (when (or (null path)
            (equal path "/"))
    (return-from compute-selector "/"))

  (cond
    ((> (length path) 2) (subseq path 2)) ; Cut off the selector type
    ((= (length path) 2) "/") ; If the selector includes *ONLY* the type, return root selector
    (t (error 'bad-uri-error :uri uri))))

(defun compute-item-type (uri path)
  ;; The root selector or null selector should return :submenu
  (when (or (null path)
            (equal path "/"))
    (return-from compute-item-type :submenu))

  ;; Must include at least the initial slash and the type
  (if (and (>= (length path) 2)
           (equal (elt path 0) #\/)
           (type-for-character (elt path 1)))
      (let ((type (type-for-character (elt path 1))))
        (if (not (equal type :unknown))
            type
            (error 'bad-uri-error :uri uri)))))

(defun parse-gopher-uri (uri &key (display-string "???"))
  #.(format nil "PARSE-GOPHER-URI takes a gopher uri as a string, and~@
                 returns a GOPHER-LINE for it.")
  (when (not (null uri))
    (let* ((uri
            (if (and (>= (length uri) 9) (equal "gopher://" (subseq uri 0 9)))
                (quri:uri uri)
                (quri:uri (format nil "gopher://~a" uri))))
           (path (quri:url-decode (quri:uri-path uri)))
           (item-type (compute-item-type uri path))
           (selector (compute-selector uri path))
           (tab-split-selector (uiop:split-string selector :separator '(#\Tab)))
           (selector (first tab-split-selector))
           (terms (second tab-split-selector))
           (host (quri:uri-host uri))
           (port (or (quri:uri-port uri) 70)))
      (apply #'make-instance (class-for-type item-type)
             :display-string display-string
             :selector selector
             :hostname host
             :port port
             :terms terms))))

(defun uri-for-gopher-line (gl)
  #.(format nil "URI-FOR-GOPHER-LINE takes a GOPHER-LINE and returns~@
                 a string containing a gopher uri representing the~@
                 resource the line points to.")
  (format nil "gopher://~a~:[:~a~;~*~]/~@[~*~c~a~]"
          (hostname gl)
          (eql (port gl) 70) (port gl)
          (not (or (null (selector gl))
                   (equal (selector gl) "")
                   (equal (selector gl) "/")))
          (type-character gl) (selector gl)))
