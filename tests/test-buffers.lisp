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

(def-test buffer-pool-reuse ()
  "Test that buffers are reused from the pool."
  (let ((pool (llog::make-buffer-pool :buffer-size 1024 :max-buffers 4)))
    ;; Acquire and release multiple buffers
    (let ((buffers nil))
      (dotimes (i 5)
        (push (llog::acquire-char-buffer pool) buffers))
      (dolist (buffer buffers)
        (llog::release-char-buffer buffer pool))
      ;; Pool should now have buffers
      (let ((new-buffer (llog::acquire-char-buffer pool)))
        (is (member new-buffer buffers :test #'eq))
        (llog::release-char-buffer new-buffer pool)))))

(def-test buffer-pool-max-size ()
  "Test that pool respects max-buffers limit."
  (let ((pool (llog::make-buffer-pool :buffer-size 1024 :max-buffers 2)))
    ;; Acquire and release 4 buffers
    (dotimes (i 4)
      (let ((buffer (llog::acquire-char-buffer pool)))
        (llog::release-char-buffer buffer pool)))
    ;; Pool should only hold max-buffers (2) buffers
    (bt:with-lock-held ((llog::buffer-pool-lock pool))
      (is (<= (length (llog::buffer-pool-buffers pool)) 2)))))

(def-test buffer-thread-local-isolation ()
  "Test that each thread has its own cached buffer."
  (let* ((buffer1 nil)
         (buffer2 nil)
         (thread1 (bt:make-thread
                   (lambda ()
                     (setf buffer1 (llog::acquire-char-buffer))
                     (llog::release-char-buffer buffer1)
                     ;; Acquire again - should get the same buffer from thread cache
                     (let ((cached (llog::acquire-char-buffer)))
                       (setf buffer1 (eq buffer1 cached))))
                   :name "thread1"))
         (thread2 (bt:make-thread
                   (lambda ()
                     (setf buffer2 (llog::acquire-char-buffer))
                     (llog::release-char-buffer buffer2)
                     ;; Acquire again - should get the same buffer from thread cache
                     (let ((cached (llog::acquire-char-buffer)))
                       (setf buffer2 (eq buffer2 cached))))
                   :name "thread2")))
    (bt:join-thread thread1)
    (bt:join-thread thread2)
    ;; Both threads should have gotten their cached buffers
    (is (not (null buffer1)))
    (is (not (null buffer2)))))

(def-test buffer-push-operations ()
  "Test various buffer push operations."
  (let ((buffer (llog::make-char-buffer :capacity 10)))
    (unwind-protect
         (progn
           ;; Test push-char
           (llog::char-buffer-push-char buffer #\H)
           (llog::char-buffer-push-char buffer #\i)
           (is (= 2 (llog::char-buffer-length buffer)))

           ;; Test push-string
           (llog::char-buffer-push-string buffer " there")
           (is (= 8 (llog::char-buffer-length buffer)))

           ;; Test push-buffer
           (let ((source (llog::make-char-buffer :capacity 10)))
             (llog::char-buffer-push-char source #\!)
             (llog::char-buffer-push-buffer buffer source)
             (is (= 9 (llog::char-buffer-length buffer))))

           ;; Verify contents
           (let ((contents (with-output-to-string (s)
                            (llog::char-buffer-write-to-stream buffer s))))
             (is (string= "Hi there!" contents))))
      (llog::release-char-buffer buffer))))

(def-test buffer-auto-resize ()
  "Test that buffers automatically resize when needed."
  (let ((buffer (llog::make-char-buffer :capacity 4)))
    (unwind-protect
         (progn
           ;; Push more data than initial capacity
           (llog::char-buffer-push-string buffer "Hello World!")
           (is (= 12 (llog::char-buffer-length buffer)))
           (is (>= (length (llog::char-buffer-string buffer)) 12)))
      (llog::release-char-buffer buffer))))
