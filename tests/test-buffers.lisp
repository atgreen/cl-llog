;;;; tests/test-buffers.lisp - Buffer pool and allocation tests
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:llog/tests)

(in-suite :llog)

(def-test char-buffer-basics ()
  (let ((buffer (llog::make-char-buffer :capacity 4)))
    (unwind-protect
         (progn
           (llog::char-buffer-push-char buffer #\A)
           (llog::char-buffer-push-string buffer "BC")
           (is (= 3 (llog::char-buffer-length buffer)))
           (is (char= #\C (llog::char-buffer-last-char buffer)))
           (llog::char-buffer-clear buffer)
           (is (= 0 (llog::char-buffer-length buffer))))
      (llog::release-char-buffer buffer))))

(def-test char-buffer-thread-cache ()
  (let* ((buffer (llog::acquire-char-buffer))
         (first buffer))
    (llog::release-char-buffer first)
    (let ((second (llog::acquire-char-buffer)))
      (is (eq first second))
      (llog::release-char-buffer second))))

#+sbcl
(defmacro %bytes-consed (&body body)
  `(let ((start (sb-ext:get-bytes-consed)))
     (progn ,@body)
     (- (sb-ext:get-bytes-consed) start)))

#+sbcl
(def-test json-buffered-logging-low-allocation ()
  (let* ((stream (make-string-output-stream))
         (output (make-stream-output stream :encoder (make-json-encoder)))
         (logger (make-logger :outputs (list output) :level :trace)))
    (declare (ignore stream))
    (let ((bytes
           (%bytes-consed
             (let ((llog:*logger* logger))
               (dotimes (i 50)
                 (llog:info-typed "pool-test"
                   (llog:int "value" i)
                   (llog:string "tag" "check")))))))
      ;; Allow a small amount of allocation for keyword handling etc.
      (is (< bytes 120000)))))
