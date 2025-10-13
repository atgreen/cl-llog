;;;; tests/test-levels.lisp - Level-related tests
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

(def-test levels-basic ()
  (is (= 0 (parse-level :trace)))
  (is (= 3 (parse-level "warn")))
  (is (= 4 (parse-level 4)))
  (is (null (parse-level :nope)))
  (is (level>= 4 3))
  (is (not (level>= 2 3)))
  (is (string= "INFO" (level-name 2)))
  (is (eq :fatal (level-keyword 5))))
