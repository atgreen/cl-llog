;;;; tests/test-outputs.lisp - Output behaviours
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

(defun %make-temp-path ()
  "Return a fresh temporary pathname and ensure the directory exists."
  (let* ((dir (uiop:temporary-directory))
         (name (format nil "llog-test-~A-~A.log"
                      (get-universal-time)
                      (random 100000)))
         (path (merge-pathnames name dir)))
    (ensure-directories-exist path)
    path))

(def-test file-output-unbuffered ()
  (let* ((path (%make-temp-path))
         (output (make-file-output path :encoder (make-json-encoder)
                                        :buffer-mode :none))
         (logger (make-logger :outputs (list output) :level :trace)))
    (unwind-protect
         (progn
           (log-entry logger
                      (make-log-entry llog:+info+ "file-test"
                                      :fields (list (llog:string "user" "alice"))))
           (flush-output output)
           (close-output output)
           (setf output nil)
           (let ((contents (uiop:read-file-string path)))
             (is (search "\"msg\":\"file-test\"" contents))
             (is (search "\"user\":\"alice\"" contents)))
           (uiop:delete-file-if-exists path))
      (when output (ignore-errors (close-output output))))))

(def-test file-output-block-buffering ()
  (let* ((path (%make-temp-path))
         (output (make-file-output path :encoder (make-console-encoder)
                                        :buffer-mode :block
                                        :buffer-size 4096))
         (logger (make-logger :outputs (list output) :level :trace)))
    (unwind-protect
         (progn
           (log-entry logger (make-log-entry llog:+info+ "buffered"))
           (let ((contents (uiop:read-file-string path)))
             (is (zerop (length contents))))
           (flush-output output)
           (let ((contents (uiop:read-file-string path)))
             (is (search "buffered" contents)))
           (close-output output)
           (setf output nil)
           (uiop:delete-file-if-exists path))
      (when output (ignore-errors (close-output output))))))

(def-test async-output-basic ()
  (let* ((stream (make-string-output-stream))
         (base (make-stream-output stream :encoder (make-console-encoder)))
         (async (make-async-output base :queue-size 8))
         (logger (make-logger :outputs (list async) :level :trace)))
    (unwind-protect
         (progn
           (log-entry logger (make-log-entry llog:+warn+ "async"))
           (flush-output async)
           (let ((contents (get-output-stream-string stream)))
             (is (search "[WARN]" contents))
             (is (search "async" contents)))
           (close-output async)
           (setf async nil))
      (when async (ignore-errors (close-output async))))))
