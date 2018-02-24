(in-package :cl-gopher)

(defvar *allow-downloads*)

(define-condition hangup-error (error) ())
(define-condition quit-condition () ())

(defun pause-line (input-stream &key prompt-stream)
  (when prompt-stream
    (format prompt-stream "Press return to continue...~%")
    (force-output prompt-stream))
  (when (null (read-line input-stream nil nil))
    (error 'hangup-error)))

(defun get-client-response (stream)
  (let ((response (read-line stream nil nil)))
    (when (null response)
      (error 'hangup-error))
    response))

(defgeneric handle-line-selection (line &key input-stream output-stream))
(defmethod handle-line-selection ((line gopher-line) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (get-line-target line))

(defmethod handle-line-selection ((line search-line) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (format output-stream "Enter your search terms:~%> ")
  (force-output output-stream)
  (let ((terms (get-client-response input-stream)))
    (setf (terms line) terms)
    (get-line-target line)))

(defun write-help (input-stream output-stream)
  (format output-stream "A number navigates to a menu item.~%back moves back a page~%quit quits the browser.~%")
  (pause-line input-stream :prompt-stream output-stream))

(defgeneric handle-contents (contents &key input-stream output-stream))
(defmethod handle-contents ((contents submenu-contents) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (format output-stream "Select a line number, or \"help\".~%> ")
  (force-output output-stream)
  (let ((line (get-client-response input-stream)))
    (let ((choice (parse-integer line :junk-allowed t)))
      (cond
        ((equalp line "back") :back)
        ((equalp line "help") (write-help input-stream output-stream))
        ((equalp line "quit") (error 'quit-condition))
        ((and choice (< choice (length (lines contents))))
         (handle-line-selection (elt (lines contents) choice)
                                :input-stream input-stream
                                :output-stream output-stream))
        (t (format output-stream "You made an invalid choice. Please select a line number to browse.~%")
           (pause-line input-stream :prompt-stream output-stream))))))

(defmethod handle-contents ((contents selector-contents) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (pause-line input-stream :prompt-stream output-stream)
  :back)

(defmethod handle-contents ((contents binary-file-contents) &key (input-stream *standard-input*) (output-stream *standard-output*))
  (when *allow-downloads*
    (format output-stream "Save File? [Y/n]: ")
    (force-output output-stream)
    (let ((response (get-client-response input-stream)))
      (when (not (equalp response "n"))
        (let ((file-path (format nil "/tmp/~a" (file-name contents))))
          (with-open-file (os file-path
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
            (loop for byte across (content-array contents)
                  do (write-byte byte os)))
          (format output-stream "File written to ~a~%" file-path)))))
  (pause-line input-stream :prompt-stream output-stream)
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
             (let ((result (handle-contents (car stack)
                                            :input-stream input-stream
                                            :output-stream output-stream)))
               (when result
                 (if (eq result :back)
                     (when (> (length stack) 1) (pop stack))
                     (push result stack))))))
    (hangup-error () nil)
    (quit-condition () nil)))

(defun network-browser (&optional (port 7070))
  (usocket:with-socket-listener (sock nil port)
    (loop do (let* ((accept-sock (usocket:socket-accept sock :element-type '(unsigned-byte 8)))
                    (accepted (flexi-streams:make-flexi-stream (usocket:socket-stream accept-sock)
                                                               :external-format (flexi-streams:make-external-format :iso-8859-1 :eol-style :crlf))))
               (bt:make-thread (lambda ()
                                 (unwind-protect
                                      (text-browser :input-stream accepted :output-stream accepted)
                                   (close accepted)
                                   (usocket:socket-close accept-sock))))))))
