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

(def-test encoders-json-basic ()
  (let* ((stream (make-string-output-stream))
         (encoder (make-json-encoder))
         (entry (make-log-entry llog:+error+ "enc-json"
                                :logger-name "enc"
                                :fields (list (llog:int "code" 500)
                                              (llog:bool "success" nil)))))
    (llog::encode-entry encoder stream entry)
    (let ((out (get-output-stream-string stream)))
      (is (search "\"level\":\"error\"" out))
      (is (search "\"msg\":\"enc-json\"" out))
      (is (search "\"logger\":\"enc\"" out))
      (is (search "\"code\":500" out))
      (is (search "\"success\":false" out)))))

(def-test encoders-sexpr-basic ()
  (let* ((stream (make-string-output-stream))
         (encoder (make-sexpr-encoder))
         (fields (list (llog:string "user" "alice")
                       (llog:bool "active" t)))
         (entry (make-log-entry llog:+info+ "enc-sexpr"
                                :logger-name "enc"
                                :fields fields)))
    (llog::encode-entry encoder stream entry)
    (let* ((out (get-output-stream-string stream))
           (plist (read-from-string out)))
      (is (eql (getf plist :level) :info))
      (is (string= (getf plist :msg) "enc-sexpr"))
      (is (string= (getf plist :logger) "enc"))
      (is (string= (getf plist :user) "alice"))
      (is (eq (getf plist :active) t))
      (is (numberp (getf plist :ts))))))
