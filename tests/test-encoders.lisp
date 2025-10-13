;;;; tests/test-encoders.lisp - Encoder behaviour tests
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

(def-test encoders-console-basic ()
  (let* ((stream (make-string-output-stream))
         (encoder (make-console-encoder))
         (entry (make-log-entry llog:+warn+ "enc-test"
                                :logger-name "enc"
                                :fields (list (llog:int "code" 7)))))
    (llog::encode-entry encoder stream entry)
    (let ((out (get-output-stream-string stream)))
      (is (search "[WARN]" out))
      (is (search "enc: enc-test" out))
      (is (search "code: 7" out)))))
