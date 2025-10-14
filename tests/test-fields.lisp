;;;; tests/test-fields.lisp - Field constructor tests
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

(def-test fields-constructors ()
  (let ((s (llog:string "name" "value"))
        (i (llog:int "count" 3))
        (f (llog:float "ratio" 1/2))
        (b (llog:bool "flag" t))
        (tstamp (llog:timestamp "ts" 42))
        (dur (llog:duration-ms "duration" 5.5)))
    (is (string= "name" (llog::field-name s)))
    (is (string= "value" (llog::field-value s)))
    (is (eql :int (llog::field-type i)))
    (is (typep (llog::field-value f) 'double-float))
    (is (eql :bool (llog::field-type b)))
    (is (= 42 (llog::field-value tstamp)))
    (is (eql :duration-ms (llog::field-type dur))))

  (let* ((plist '(:foo 10 :bar "wow"))
         (fields (keywords-to-fields plist)))
    (is (= 2 (length fields)))
    (is (equal '("foo" "bar")
               (mapcar #'llog::field-name fields)))
    (is (equal '(:int :string)
               (mapcar #'llog::field-type fields)))))
