;;;; test-hooks-simple.lisp - Basic hook system tests
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

;;; Basic Hook Tests

(def-test hook-add-remove ()
  "Test basic hook addition and removal."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger)
         (called nil))
    (llog:add-hook logger :post-log
                   (lambda (l e) (declare (ignore l e)) (setf called t))
                   :name 'test-hook)
    (info "Test")
    (is-true called)
    (setf called nil)
    (llog:remove-hook logger :post-log :name 'test-hook)
    (info "Test2")
    (is-false called)))

(def-test pre-log-filter ()
  "Test that pre-log hooks can filter entries."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    (llog:add-hook logger :pre-log
                   (lambda (l e) (declare (ignore l e)) nil))  ; Filter all
    (info "Should not appear")
    (is (string= "" (get-output-stream-string stream)))))

(def-test pre-log-modify ()
  "Test that pre-log hooks can modify entries."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    (llog:add-hook logger :pre-log
                   (lambda (l entry)
                     (declare (ignore l))
                     (push (llog:string "added" "field")
                           (llog:log-entry-fields entry))
                     entry))
    (info "Test")
    (let ((output (get-output-stream-string stream)))
      (is (search "added" output))
      (is (search "field" output)))))

(def-test post-log-called ()
  "Test that post-log hooks are called."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger)
         (msg nil))
    (llog:add-hook logger :post-log
                   (lambda (l entry)
                     (declare (ignore l))
                     (setf msg (llog:log-entry-message entry))))
    (info "Hello")
    (is (string= "Hello" msg))))

(def-test hook-priority ()
  "Test that hooks execute in priority order."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger)
         (order nil))
    (llog:add-hook logger :post-log
                   (lambda (l e) (declare (ignore l e)) (push 2 order))
                   :priority 20)
    (llog:add-hook logger :post-log
                   (lambda (l e) (declare (ignore l e)) (push 1 order))
                   :priority 10)
    (info "Test")
    (is (equal '(2 1) order))))

(def-test clear-hooks ()
  "Test clearing hooks."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream)))))
    (llog:add-hook logger :pre-log (lambda (l e) (declare (ignore l)) e))
    (llog:add-hook logger :post-log (lambda (l e) (declare (ignore l e))))
    (is (eql 2 (llog:clear-hooks logger)))
    (is (eql 0 (length (llog:list-hooks logger))))))

(def-test hook-error-isolation ()
  "Test that hook errors don't crash logging."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    (llog:add-hook logger :pre-log
                   (lambda (l e)
                     (declare (ignore l e))
                     (cl:error "Hook error")))
    ;; Should not crash
    (info "Test")
    (let ((output (get-output-stream-string stream)))
      (is (search "Test" output)))))
