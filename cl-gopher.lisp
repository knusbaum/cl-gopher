(in-package :cl-gopher)

(defclass selector-contents () ())

(defclass submenu-contents (selector-contents)
  ((lines :initform nil :initarg :lines :accessor lines)))

(defclass text-file-contents (selector-contents)
  ((lines :initform nil :initarg :lines :accessor lines)))

(defclass html-file-contents (selector-contents)
  ((content-string :initform nil :initarg :content-string :accessor content-string)))

(defclass binary-file-contents (selector-contents)
  ((file-name :initform nil :initarg :file-name :accessor file-name)
   (content-array :initform nil :initarg :content-array :accessor content-array)))

(defgeneric display-contents (contents &key stream))
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
   (port :initform nil :initarg :port :accessor port)))

(defclass text-file (gopher-line) ())
(defclass submenu (gopher-line) ())
(defclass ccso-nameserver (gopher-line) ())
(defclass error-code (gopher-line) ())
(defclass binhex-file (gopher-line) ())
(defclass dos-file (gopher-line) ())
(defclass uuencoded-file (gopher-line) ())
(defclass search-line (gopher-line)
  ((terms :initform "" :initarg :terms :accessor terms)))
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

(defgeneric copy-gopher-line (gl))
(defmethod copy-gopher-line ((gl gopher-line))
  (make-instance (class-of gl)
                 :display-string (display-string gl)
                 :selector (selector gl)
                 :hostname (hostname gl)
                 :port (port gl)))

(defmethod copy-gopher-line ((gl search-line))
  (make-instance 'search-line
                 :display-string (display-string gl)
                 :selector (selector gl)
                 :hostname (hostname gl)
                 :port (port gl)
                 :terms (terms gl)))

(defun convert-to-text-line (gl)
  (make-instance 'text-file
                 :display-string (display-string gl)
                 :selector (selector gl)
                 :hostname (hostname gl)
                 :port (port gl)))

(defmethod print-object ((gl gopher-line) stream)
  (print-unreadable-object (gl stream :type t)
    (format stream "String: [~a], Selector: [~a], Host: [~a:~a]"
            (display-string gl) (selector gl) (hostname gl) (port gl))))

(defgeneric gopher-line-to-alist (gl))
(defmethod gopher-line-to-alist ((gl gopher-line))
  (let ((lst))
    (push (cons :line-type (line-type gl)) lst)
    (push (cons :display-string (display-string gl)) lst)
    (push (cons :selector (selector gl)) lst)
    (push (cons :hostname (hostname gl)) lst)
    (push (cons :port (port gl)) lst)
    lst))

(defun gopher-lines-to-alist (gls)
  (loop for line in gls
        collect (gopher-line-to-alist line)))

(defun gopher-line-from-alist (gl)
  (let ((line-type (cdr (assoc :line-type gl))))
    (make-instance (class-for-type line-type)
                   :display-string (cdr (assoc :display-string gl))
                   :selector (cdr (assoc :selector gl))
                   :hostname (cdr (assoc :hostname gl))
                   :port (cdr (assoc :port gl)))))

(defun gopher-lines-from-alist (gls)
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

(defgeneric get-line-contents (gl))
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

(defgeneric display-line (gl &key stream line-number show-target include-newline))
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
  (loop for elem in lines
        for i from 0
        do (if with-line-nums
               (display-line elem :stream stream :show-target show-target :line-number i :include-newline t)
               (display-line elem :stream stream :show-target show-target :include-newline t))))

(defgeneric write-gopher-line (gl &key stream))
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
  (when (not (null uri))
    (let* ((uri
            (if (and (>= (length uri) 9) (equal "gopher://" (subseq uri 0 9)))
                (quri:uri uri)
                (quri:uri (format nil "gopher://~a" uri))))
           (path (quri:uri-path uri))
           (item-type (compute-item-type uri path))
           (selector (compute-selector uri path))
           (host (quri:uri-host uri))
           (port (or (quri:uri-port uri) 70)))
      (make-instance (class-for-type item-type)
                     :display-string display-string
                     :selector selector
                     :hostname host
                     :port port))))

(defun uri-for-gopher-line (gl)
  (if (or (null (selector gl))
          (equal (selector gl) "")
          (equal (selector gl) "/"))
      (format nil "gopher://~a:~a/" (hostname gl) (port gl))
      (format nil "gopher://~a:~a/~c~a"
              (hostname gl) (port gl) (type-character gl) (selector gl))))
