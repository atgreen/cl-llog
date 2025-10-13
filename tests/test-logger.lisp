;;;; tests/test-logger.lisp - Logger behaviour tests
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

(def-test logger-level-filter ()
  (let ((logger (make-logger :level :warn :outputs nil)))
    (is (should-log-p logger llog:+error+))
    (is (not (should-log-p logger llog:+info+)))
    (set-level logger :debug)
    (is (should-log-p logger llog:+info+))))

(defun %string-output ()
  (make-string-output-stream))

(defun %logger-with-stream (stream)
  (let ((output (make-stream-output stream :encoder (make-console-encoder))))
    (make-logger :name "test" :level :trace :outputs (list output))))

(def-test logger-context-and-output ()
  (let ((stream (%string-output)))
    (let* ((logger (%logger-with-stream stream))
           (child (with-fields logger :request-id 42)))
      (log-entry child
                 (make-log-entry llog:+info+ "hello world"
                                 :fields (list (llog:string "foo" "bar"))))
      (let ((result (get-output-stream-string stream)))
        (is (search "hello world" result))
        (is (search "foo: bar" result))
        (is (search "request-id: 42" result))))))
