;;;; buffer-pool.lisp - Shared buffer utilities
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:llog)

;;; Buffer Pool Infrastructure

(defstruct buffer-pool
  (lock (make-lock "llog/buffer-pool") :type t)
  (buffers nil :type list)
  (buffer-size 8192 :type fixnum)
  (max-buffers 32 :type fixnum))

;;; Character Buffer Representation

(defstruct (char-buffer (:constructor %make-char-buffer (string length)))
  (string string :type (simple-array character (*)))
  (length length :type fixnum))

(defun make-char-buffer (&key (capacity 8192))
  "Create a character buffer with initial CAPACITY."
  (%make-char-buffer (make-array capacity :element-type 'character)
                     0))

(defun char-buffer-clear (buffer)
  "Reset BUFFER to an empty state."
  (setf (char-buffer-length buffer) 0)
  buffer)

(defun char-buffer-reserve (buffer additional)
  "Ensure BUFFER can hold ADDITIONAL characters."
  (let* ((needed (+ (char-buffer-length buffer) additional))
         (string (char-buffer-string buffer))
         (current (length string)))
    (when (> needed current)
      (let* ((new-size (max needed (* 2 current)))
             (new-string (make-array new-size :element-type 'character)))
        (replace new-string string :end1 (char-buffer-length buffer))
        (setf (char-buffer-string buffer) new-string))))
  buffer)

(defun char-buffer-push-char (buffer char)
  "Append CHAR to BUFFER."
  (char-buffer-reserve buffer 1)
  (let ((len (char-buffer-length buffer)))
    (setf (aref (char-buffer-string buffer) len) char)
    (setf (char-buffer-length buffer) (1+ len)))
  buffer)

(defun char-buffer-push-string (buffer string)
  "Append STRING to BUFFER."
  (let ((len (length string)))
    (char-buffer-reserve buffer len)
    (let ((target (char-buffer-string buffer))
          (start (char-buffer-length buffer)))
      (replace target string :start1 start)
      (setf (char-buffer-length buffer) (+ start len))))
  buffer)

(defun char-buffer-push-buffer (target source)
  "Append SOURCE buffer contents into TARGET."
  (char-buffer-reserve target (char-buffer-length source))
  (let ((target-string (char-buffer-string target))
        (source-string (char-buffer-string source))
        (target-start (char-buffer-length target))
        (source-length (char-buffer-length source)))
    (replace target-string source-string
             :start1 target-start
             :end2 source-length)
    (setf (char-buffer-length target) (+ target-start source-length)))
  target)

(defun char-buffer-write-to-stream (buffer stream)
  "Write BUFFER contents to STREAM."
  (write-string (char-buffer-string buffer) stream
                :start 0 :end (char-buffer-length buffer)))

(defun char-buffer-last-char (buffer)
  "Return last character in BUFFER or NIL if empty."
  (let ((len (char-buffer-length buffer)))
    (when (plusp len)
      (aref (char-buffer-string buffer) (1- len)))))

;;; Global Pool + Thread-Local Cache

(defparameter *char-buffer-pool*
  (make-buffer-pool :buffer-size 8192 :max-buffers 32)
  "Global pool of reusable character buffers.")

(defstruct thread-local-cache
  "Thread-local storage for buffer caching."
  (lock (make-lock "llog/thread-cache") :type t)
  (cache (make-hash-table :test 'eq :weakness :key) :type hash-table))

(defparameter *thread-buffer-cache*
  (make-thread-local-cache)
  "Thread-local buffer cache mapping threads to buffers.")

(defun %get-thread-buffer ()
  "Get the thread-local buffer for the current thread, if any."
  (let ((cache (thread-local-cache-cache *thread-buffer-cache*))
        (thread (current-thread)))
    (gethash thread cache)))

(defun %set-thread-buffer (buffer)
  "Set the thread-local buffer for the current thread."
  (let ((cache (thread-local-cache-cache *thread-buffer-cache*))
        (thread (current-thread)))
    (setf (gethash thread cache) buffer)))

(defun %clear-thread-buffer ()
  "Clear the thread-local buffer for the current thread."
  (let ((cache (thread-local-cache-cache *thread-buffer-cache*))
        (thread (current-thread)))
    (remhash thread cache)))

(defun acquire-char-buffer (&optional (pool *char-buffer-pool*))
  "Obtain a character buffer from POOL, reusing a thread-local buffer when available."
  (or (let ((buffer (%get-thread-buffer)))
        (when buffer
          (%clear-thread-buffer)
          (char-buffer-clear buffer)))
      (with-lock-held ((buffer-pool-lock pool))
        (let ((buffer (pop (buffer-pool-buffers pool))))
          (if buffer
              (char-buffer-clear buffer)
              (make-char-buffer :capacity (buffer-pool-buffer-size pool)))))))

(defun release-char-buffer (buffer &optional (pool *char-buffer-pool*))
  "Return BUFFER to POOL for reuse, caching in thread-local storage first."
  (char-buffer-clear buffer)
  (cond
    ((null (%get-thread-buffer))
     (%set-thread-buffer buffer))
    (t
     (with-lock-held ((buffer-pool-lock pool))
       (when (< (length (buffer-pool-buffers pool))
                (buffer-pool-max-buffers pool))
         (push buffer (buffer-pool-buffers pool))))))
  (values))
