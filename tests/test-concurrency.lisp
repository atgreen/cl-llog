;;;; tests/test-concurrency.lisp - Concurrency and thread safety tests
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:llog/tests)

(in-suite :llog)

(def-test concurrent-logging-basic ()
  "Test that multiple threads can log concurrently without data corruption."
  (let* ((stream (make-string-output-stream))
         (output (make-stream-output stream :encoder (make-json-encoder)))
         (logger (make-logger :outputs (list output) :level :info))
         (thread-count 4)
         (logs-per-thread 25)
         (threads nil))
    ;; Create multiple threads that log concurrently
    (dotimes (i thread-count)
      (let ((thread-id i))
        (push (bt:make-thread
               (lambda ()
                 (let ((llog:*logger* logger))
                   (dotimes (j logs-per-thread)
                     (llog:info "Concurrent log"
                               :thread-id thread-id
                               :iteration j))))
               :name (format nil "logger-~D" i))
              threads)))
    ;; Wait for all threads to complete
    (dolist (thread threads)
      (bt:join-thread thread))
    ;; Flush output
    (flush-output output)
    ;; Verify output
    (let ((output-str (get-output-stream-string stream)))
      ;; Should have exactly thread-count * logs-per-thread lines
      (is (= (* thread-count logs-per-thread)
             (count #\Newline output-str)))
      ;; Each line should be valid JSON
      (with-input-from-string (in output-str)
        (loop for line = (read-line in nil nil)
              while line
              do (is (char= #\{ (char line 0)))
                 (is (char= #\} (char line (1- (length line))))))))))

(def-test concurrent-level-changes ()
  "Test that level changes are thread-safe."
  (let* ((logger (make-logger :level :info))
         (thread-count 4)
         (iterations 50)
         (threads nil)
         (levels '(:trace :debug :info :warn :error)))
    ;; Multiple threads changing levels concurrently
    (dotimes (i thread-count)
      (push (bt:make-thread
             (lambda ()
               (dotimes (j iterations)
                 (set-level logger (nth (mod j (length levels)) levels))))
             :name (format nil "level-changer-~D" i))
            threads))
    ;; Wait for all threads
    (dolist (thread threads)
      (bt:join-thread thread))
    ;; Logger should still be functional
    (is (integerp (logger-level logger)))
    (is (member (logger-level logger)
                (list +trace+ +debug+ +info+ +warn+ +error+)))))

(def-test concurrent-output-management ()
  "Test that adding/removing outputs is thread-safe."
  (let* ((logger (make-logger :level :info))
         (thread-count 4)
         (iterations 20)
         (threads nil))
    ;; Multiple threads adding and removing outputs concurrently
    (dotimes (i thread-count)
      (push (bt:make-thread
             (lambda ()
               (dotimes (j iterations)
                 (let ((output (make-stream-output
                               (make-broadcast-stream)
                               :encoder (make-json-encoder))))
                   (add-output logger output)
                   (bt:thread-yield)
                   (remove-output logger output))))
             :name (format nil "output-manager-~D" i))
            threads))
    ;; Wait for all threads
    (dolist (thread threads)
      (bt:join-thread thread))
    ;; Logger should still be functional
    (is (listp (logger-outputs logger)))))

(def-test concurrent-context-logging ()
  "Test that context fields work correctly under concurrency."
  (let* ((stream (make-string-output-stream))
         (output (make-stream-output stream :encoder (make-json-encoder)))
         (base-logger (make-logger :outputs (list output) :level :info))
         (thread-count 4)
         (logs-per-thread 10)
         (threads nil))
    ;; Each thread uses its own logger with different context
    (dotimes (i thread-count)
      (let ((thread-id i))
        (push (bt:make-thread
               (lambda ()
                 (let* ((llog:*logger* (with-fields base-logger
                                         :thread-id thread-id
                                         :context "test"))
                        (counter 0))
                   (dotimes (j logs-per-thread)
                     (llog:info "Context log" :counter (incf counter)))))
               :name (format nil "context-logger-~D" i))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (bt:join-thread thread))
    ;; Flush output
    (flush-output output)
    ;; Verify output
    (let ((output-str (get-output-stream-string stream)))
      (is (= (* thread-count logs-per-thread)
             (count #\Newline output-str)))
      ;; Each thread ID should appear at least once
      (dotimes (i thread-count)
        (is (search (format nil "\"thread-id\":~D" i) output-str))))))

(def-test concurrent-file-output ()
  "Test that file output handles concurrent writes correctly."
  (let* ((temp-file (format nil "/tmp/llog-test-~A-~A.log"
                           (get-universal-time)
                           (random 100000)))
         (output (make-file-output temp-file
                                  :encoder (make-json-encoder)
                                  :buffer-mode :line))
         (logger (make-logger :outputs (list output) :level :info))
         (thread-count 4)
         (logs-per-thread 25)
         (threads nil))
    (unwind-protect
         (progn
           ;; Multiple threads writing to the same file
           (dotimes (i thread-count)
             (let ((thread-id i))
               (push (bt:make-thread
                      (lambda ()
                        (let ((llog:*logger* logger))
                          (dotimes (j logs-per-thread)
                            (llog:info "File log"
                                      :thread-id thread-id
                                      :iteration j))))
                      :name (format nil "file-logger-~D" i))
                     threads)))
           ;; Wait for all threads
           (dolist (thread threads)
             (bt:join-thread thread))
           ;; Close and flush the file
           (close-output output)
           ;; Verify the file contains all log entries
           (with-open-file (in temp-file :direction :input)
             (let ((line-count 0))
               (loop for line = (read-line in nil nil)
                     while line
                     do (incf line-count)
                        (is (char= #\{ (char line 0)))
                        (is (char= #\} (char line (1- (length line))))))
               (is (= (* thread-count logs-per-thread) line-count)))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test concurrent-typed-api ()
  "Test that the typed API is thread-safe and allocation-friendly under concurrency."
  (let* ((stream (make-string-output-stream))
         (output (make-stream-output stream :encoder (make-json-encoder)))
         (logger (make-logger :outputs (list output) :level :info))
         (thread-count 4)
         (logs-per-thread 20)
         (threads nil))
    ;; Multiple threads using typed API
    (dotimes (i thread-count)
      (let ((thread-id i))
        (push (bt:make-thread
               (lambda ()
                 (let ((llog:*logger* logger))
                   (dotimes (j logs-per-thread)
                     (llog:info-typed "Typed concurrent log"
                       (llog:int "thread-id" thread-id)
                       (llog:int "iteration" j)
                       (llog:string "status" "active")))))
               :name (format nil "typed-logger-~D" i))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (bt:join-thread thread))
    ;; Flush output
    (flush-output output)
    ;; Verify output
    (let ((output-str (get-output-stream-string stream)))
      (is (= (* thread-count logs-per-thread)
             (count #\Newline output-str))))))
