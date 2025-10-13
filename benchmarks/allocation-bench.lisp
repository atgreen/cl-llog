;;;; benchmarks/allocation-bench.lisp - Allocation and performance benchmarks
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:cl-user)

(defpackage #:llog/benchmarks
  (:use #:cl)
  (:export #:run-allocation-benchmarks
           #:run-performance-benchmarks
           #:compare-apis))

(in-package #:llog/benchmarks)

#+sbcl
(defmacro measure-allocations (&body body)
  "Measure bytes allocated during BODY execution."
  (let ((start (gensym "START"))
        (end (gensym "END")))
    `(let ((,start (sb-ext:get-bytes-consed)))
       (progn ,@body)
       (let ((,end (sb-ext:get-bytes-consed)))
         (- ,end ,start)))))

#+sbcl
(defmacro measure-time (&body body)
  "Measure time in microseconds for BODY execution."
  (let ((start (gensym "START"))
        (end (gensym "END")))
    `(let ((,start (get-internal-real-time)))
       (progn ,@body)
       (let ((,end (get-internal-real-time)))
         (floor (* (- ,end ,start) 1000000)
                internal-time-units-per-second)))))

(defun format-bytes (bytes)
  "Format BYTES as human-readable string."
  (cond
    ((< bytes 1024) (format nil "~D bytes" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,2F KB" (/ bytes 1024.0)))
    (t (format nil "~,2F MB" (/ bytes (* 1024.0 1024.0))))))

(defun format-time (microseconds)
  "Format MICROSECONDS as human-readable string."
  (cond
    ((< microseconds 1000) (format nil "~D μs" microseconds))
    ((< microseconds 1000000) (format nil "~,2F ms" (/ microseconds 1000.0)))
    (t (format nil "~,2F s" (/ microseconds 1000000.0)))))

#+sbcl
(defun bench-sugared-api (iterations)
  "Benchmark sugared API allocation."
  (let* ((stream (make-string-output-stream))
         (output (llog:make-stream-output stream :encoder (llog:make-json-encoder)))
         (logger (llog:make-logger :outputs (list output) :level :trace)))
    (declare (ignore stream))
    (let ((bytes (measure-allocations
                   (let ((llog:*logger* logger))
                     (dotimes (i iterations)
                       (llog:info "Benchmark message"
                                 :iteration i
                                 :thread-id 1
                                 :status "running"))))))
      (values bytes (/ bytes iterations)))))

#+sbcl
(defun bench-typed-api (iterations)
  "Benchmark typed API allocation."
  (let* ((stream (make-string-output-stream))
         (output (llog:make-stream-output stream :encoder (llog:make-json-encoder)))
         (logger (llog:make-logger :outputs (list output) :level :trace)))
    (declare (ignore stream))
    (let ((bytes (measure-allocations
                   (let ((llog:*logger* logger))
                     (dotimes (i iterations)
                       (llog:info-typed "Benchmark message"
                         (llog:int "iteration" i)
                         (llog:int "thread-id" 1)
                         (llog:string "status" "running")))))))
      (values bytes (/ bytes iterations)))))

#+sbcl
(defun bench-typed-api-file (iterations)
  "Benchmark typed API with file output allocation."
  (let* ((temp-file (format nil "/tmp/llog-bench-~A.log" (get-universal-time)))
         (output (llog:make-file-output temp-file
                                       :encoder (llog:make-json-encoder)
                                       :buffer-mode :block))
         (logger (llog:make-logger :outputs (list output) :level :trace)))
    (unwind-protect
         (let ((bytes (measure-allocations
                        (let ((llog:*logger* logger))
                          (dotimes (i iterations)
                            (llog:info-typed "Benchmark message"
                              (llog:int "iteration" i)
                              (llog:int "thread-id" 1)
                              (llog:string "status" "running")))))))
           (values bytes (/ bytes iterations)))
      (llog:close-output output)
      (when (probe-file temp-file)
        (delete-file temp-file)))))

#+sbcl
(defun bench-throughput (iterations)
  "Measure logging throughput (logs/second)."
  (let* ((stream (make-broadcast-stream))  ; Null stream
         (output (llog:make-stream-output stream :encoder (llog:make-json-encoder)))
         (logger (llog:make-logger :outputs (list output) :level :trace)))
    (let ((time-us (measure-time
                     (let ((llog:*logger* logger))
                       (dotimes (i iterations)
                         (llog:info-typed "Throughput test"
                           (llog:int "iteration" i)
                           (llog:string "status" "running")))))))
      (values time-us
              (if (zerop time-us)
                  most-positive-fixnum
                  (floor (* iterations 1000000) time-us))))))

#+sbcl
(defun run-allocation-benchmarks (&optional (iterations 1000))
  "Run allocation benchmarks and print results."
  (format t "~%=== LLOG Allocation Benchmarks ===~%")
  (format t "Iterations: ~D~%~%" iterations)

  (multiple-value-bind (total per-call)
      (bench-sugared-api iterations)
    (format t "Sugared API (stream):~%")
    (format t "  Total:    ~A~%" (format-bytes total))
    (format t "  Per-call: ~A~%~%" (format-bytes per-call)))

  (multiple-value-bind (total per-call)
      (bench-typed-api iterations)
    (format t "Typed API (stream):~%")
    (format t "  Total:    ~A~%" (format-bytes total))
    (format t "  Per-call: ~A~%~%" (format-bytes per-call)))

  (multiple-value-bind (total per-call)
      (bench-typed-api-file iterations)
    (format t "Typed API (file, block-buffered):~%")
    (format t "  Total:    ~A~%" (format-bytes total))
    (format t "  Per-call: ~A~%~%" (format-bytes per-call))))

#+sbcl
(defun run-performance-benchmarks (&optional (iterations 10000))
  "Run performance benchmarks and print results."
  (format t "~%=== LLOG Performance Benchmarks ===~%")
  (format t "Iterations: ~D~%~%" iterations)

  (multiple-value-bind (time-us throughput)
      (bench-throughput iterations)
    (format t "Throughput (typed API):~%")
    (format t "  Time:       ~A~%" (format-time time-us))
    (format t "  Throughput: ~:D logs/sec~%~%" throughput)))

#+sbcl
(defun compare-apis (&optional (iterations 1000))
  "Compare sugared vs typed API allocations."
  (format t "~%=== API Comparison ===~%")
  (format t "Iterations: ~D~%~%" iterations)

  (multiple-value-bind (sugared-total sugared-per)
      (bench-sugared-api iterations)
    (multiple-value-bind (typed-total typed-per)
        (bench-typed-api iterations)
      (format t "Sugared API: ~A (~A per call)~%"
              (format-bytes sugared-total)
              (format-bytes sugared-per))
      (format t "Typed API:   ~A (~A per call)~%~%"
              (format-bytes typed-total)
              (format-bytes typed-per))
      (let ((reduction (if (zerop typed-total)
                           100.0
                           (* 100.0 (- 1.0 (/ typed-total sugared-total))))))
        (format t "Reduction: ~,1F%~%" reduction)))))

#-sbcl
(defun run-allocation-benchmarks (&optional (iterations 1000))
  (declare (ignore iterations))
  (format t "~%Allocation benchmarks are only available on SBCL.~%"))

#-sbcl
(defun run-performance-benchmarks (&optional (iterations 10000))
  (declare (ignore iterations))
  (format t "~%Performance benchmarks are only available on SBCL.~%"))

#-sbcl
(defun compare-apis (&optional (iterations 1000))
  (declare (ignore iterations))
  (format t "~%API comparison is only available on SBCL.~%"))

;;; Example usage:
;;; (ql:quickload :llog)
;;; (load "benchmarks/allocation-bench.lisp")
;;; (llog/benchmarks:run-allocation-benchmarks 1000)
;;; (llog/benchmarks:run-performance-benchmarks 10000)
;;; (llog/benchmarks:compare-apis 1000)
