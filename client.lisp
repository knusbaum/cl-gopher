(in-package :cl-gopher)

(defvar *allow-downloads*)

(define-condition hangup-error (error) ())

(defgeneric handle-line-selection (line &key input-stream output-stream))
(defmethod handle-line-selection ((line gopher-line) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (get-line-target line))

(defmethod handle-line-selection ((line search-line) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (format output-stream "Enter your search terms:~%> ")
  (force-output output-stream)
  (let ((terms (read-line input-stream nil nil)))
    (when (null terms)
      (error 'hangup-error))
    (setf (terms line) terms)
    (get-line-target line)))

(defgeneric handle-contents (contents &key input-stream output-stream))
(defmethod handle-contents ((contents submenu-contents) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (format output-stream "Select a line number or \"back\" to go back.~%> ")
  (force-output output-stream)
  (let ((line (read-line input-stream nil nil)))
    (when (null line)
      (error 'hangup-error))
    (let ((choice (parse-integer line :junk-allowed t)))
      (cond
        ((equalp line "back") :back)
        ((and choice (< choice (length (lines contents))))
         (handle-line-selection (elt (lines contents) choice)
                                :input-stream input-stream
                                :output-stream output-stream))
        (t (format output-stream "You made an invalid choice. Please select a line number to browse.~%"))))))

(defmethod handle-contents ((contents selector-contents) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (format output-stream "Press return to continue...~%")
  (force-output output-stream)
  (when (null (read-line input-stream nil nil))
    (error 'hangup-error))
  :back)

(defmethod handle-contents ((contents binary-file-contents) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (when *allow-downloads*
    (format output-stream "Save File? [Y/n]: ")
    (force-output output-stream)
    (let ((response (read-line input-stream nil nil)))
      (when (null response)
        (error 'hangup-error))
      (when (not (equalp response "n"))
        (let ((file-path (format nil "/tmp/~a" (file-name contents))))
          (with-open-file (os file-path
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
            (loop for byte across (content-array contents)
                  do (write-byte byte os)))
          (format output-stream "File written to ~a~%" file-path)))))
  (format output-stream "Press return to continue...~%")
  (force-output output-stream)
  (when (null (read-line input-stream nil nil))
    (error 'hangup-error))
  :back)

(defun text-browser (&key (input-stream *standard-input*) (output-stream *standard-output*) allow-downloads)
  (handler-case
      (let ((*allow-downloads* allow-downloads))
        (loop
           with stack = (list (get-line-target (make-instance 'submenu
                                                              :display-string "SDF.org"
                                                              :selector "/"
                                                              :hostname "SDF.org"
                                                              :port 70)))
           do (display-contents (car stack) :stream output-stream)
             (let ((result (handle-contents (car stack) :input-stream input-stream :output-stream output-stream)))
               (when result
                 (if (eq result :back)
                     (when (> (length stack) 1) (pop stack))
                     (push result stack))))))
    (hangup-error () nil)))

(defun network-browser (&optional (port 7070))
  (iolib:with-open-socket (sock
                           :external-format  '(:ISO-8859-1 :eol-style :crlf)
                           :connect :passive
                           :address-family :internet
                           :type :stream)
    (iolib:bind-address sock
                        iolib:+ipv4-unspecified+
                        :port port
                        :reuse-addr t)
    (iolib:listen-on sock)
    (loop
       do (let ((accepted (iolib:accept-connection sock)))
            (bt:make-thread (lambda ()
                              (unwind-protect
                                   (text-browser :input-stream accepted :output-stream accepted)
                                (close accepted))))))))
