;;;; tests/test-api.lisp - API macro tests
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

(defun %capture-log-output (thunk)
  "Capture log output by calling THUNK with a test logger."
  (let ((stream (make-string-output-stream)))
    (let ((llog:*logger* (%logger-with-stream stream)))
      (funcall thunk)
      (get-output-stream-string stream))))

(def-test api-sugared ()
  (let ((output (%capture-log-output
                 (lambda ()
                   (info "login" :user-id 10 :success t)))))
    (is (search "[INFO]" output))
    (is (search "login" output))
    (is (search "user-id: 10" output))
    (is (search "success: true" output))))

(def-test api-typed ()
  (let ((output (%capture-log-output
                 (lambda ()
                   (warn-typed "slow op"
                               (llog:int "duration" 12)
                               (llog:bool "cached" nil))))))
    (is (search "[WARN]" output))
    (is (search "duration: 12" output))
    (is (search "cached: false" output))))
