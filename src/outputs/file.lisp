;;;; outputs/file.lisp - File output implementation
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:llog)

;;; File Output
;;; Writes log entries to a file with configurable buffering strategies

(defclass file-output (output)
  ((path
    :initarg :path
    :reader file-output-path
    :type pathname
    :documentation "Pathname for the target log file")
   (stream
    :initarg :stream
    :accessor file-output-stream
    :documentation "Open file stream used for writing")
   (buffer-mode
    :initarg :buffer-mode
    :accessor file-output-buffer-mode
    :type symbol
    :documentation "Buffering mode (:none, :line, or :block)")
   (buffer-size
    :initarg :buffer-size
    :reader file-output-buffer-size
   :type fixnum
   :documentation "Maximum buffer size for :block mode")
   (buffer
    :initarg :buffer
    :accessor file-output-buffer
    :documentation "Char buffer used for :block buffering")
   (lock
    :initarg :lock
    :accessor file-output-lock
    :documentation "Lock protecting stream and buffer state"))
  (:documentation "Output that appends encoded log entries to a file."))

(defun %normalize-path (path)
  "Turn PATH into a pathname."
  (etypecase path
    (pathname path)
    (cl:string (parse-namestring path))))

(defun %open-log-stream (path)
  "Open PATH for appending log entries."
  (open path
        :direction :output
        :if-exists :append
        :if-does-not-exist :create
        :element-type 'character
        :external-format :utf-8))

(defun make-file-output (path &key (encoder (make-json-encoder))
                                   (min-level +trace+)
                                   (buffer-mode :line)
                                   (buffer-size 8192))
  "Create a file output that appends encoded entries to PATH.

BUFFER-MODE is one of :none (no buffering), :line (flush after newline),
or :block (flush when BUFFER-SIZE is reached or on explicit flush)."
  (let ((pathname (%normalize-path path)))
    (ensure-directories-exist pathname)
    (let* ((mode (ecase buffer-mode
                   ((:none) :none)
                   ((:line) :line)
                   ((:block) :block)))
           (stream (%open-log-stream pathname))
           (buffer (when (eq mode :block)
                     (make-char-buffer :capacity buffer-size)))
           (lock (make-lock "llog/file-output"))
           (level (if (integerp min-level)
                      min-level
                      (or (parse-level min-level) +trace+))))
      (make-instance 'file-output
                     :encoder encoder
                     :min-level level
                     :path pathname
                     :stream stream
                     :buffer-mode mode
                     :buffer-size (max 1 buffer-size)
                     :buffer buffer
                     :lock lock))))

(defun %ensure-file-stream (output)
  "Ensure OUTPUT has an open file stream, reopening if necessary."
  (unless (and (slot-boundp output 'stream)
               (file-output-stream output)
               (open-stream-p (file-output-stream output)))
    (setf (file-output-stream output)
          (%open-log-stream (file-output-path output)))))

(defun %write-direct (output payload force-p)
  "Write PAYLOAD directly to the file stream."
  (%ensure-file-stream output)
  (let ((stream (file-output-stream output)))
    (cond
      ((typep payload 'char-buffer)
       (char-buffer-write-to-stream payload stream))
      ((stringp payload)
       (write-string payload stream))
      (t
       (write-string (princ-to-string payload) stream)))
    (when force-p
      (force-output stream))))

(defun %flush-buffer (output)
  "Flush the block buffer to disk."
  (let ((buffer (file-output-buffer output)))
    (when (and buffer (> (char-buffer-length buffer) 0))
      (%write-direct output buffer nil)
      (force-output (file-output-stream output))
      (char-buffer-clear buffer))))

(defun %write-block-entry (output buffer)
  "Handle writing BUFFER when file output is in :block mode."
  (let* ((len (char-buffer-length buffer))
         (limit (file-output-buffer-size output)))
    (if (> len limit)
        (progn
          (%flush-buffer output)
          (%write-direct output buffer nil))
        (let ((aggregate (or (file-output-buffer output)
                             (setf (file-output-buffer output)
                                   (make-char-buffer :capacity (max limit len))))))
          (when (> (+ (char-buffer-length aggregate) len) limit)
            (%flush-buffer output))
          (char-buffer-push-buffer aggregate buffer)
          (when (>= (char-buffer-length aggregate) limit)
            (%flush-buffer output))))))

(defmethod write-entry ((output file-output) (entry log-entry))
  (when (level>= (log-entry-level entry) (output-min-level output))
    (let ((buffer (acquire-char-buffer)))
      (unwind-protect
           (progn
             (encode-entry-into-buffer (output-encoder output) entry buffer)
            (with-lock-held ((file-output-lock output))
              (ecase (file-output-buffer-mode output)
                (:none
                 (%write-direct output buffer t))
                (:line
                 (%write-direct output buffer
                                (let ((last (char-buffer-last-char buffer)))
                                  (and last (char= last #\Newline)))))
                (:block
                 (%write-block-entry output buffer)))))
        (release-char-buffer buffer))))
  (values))

(defmethod flush-output ((output file-output))
  (with-lock-held ((file-output-lock output))
    (case (file-output-buffer-mode output)
      (:block (%flush-buffer output))
      (otherwise
       (%ensure-file-stream output)
       (force-output (file-output-stream output)))))
  (values))

(defmethod close-output ((output file-output))
  (with-lock-held ((file-output-lock output))
    (unwind-protect
         (progn
           (when (eq (file-output-buffer-mode output) :block)
             (%flush-buffer output))
           (when (and (file-output-stream output)
                      (open-stream-p (file-output-stream output)))
             (force-output (file-output-stream output))
             (close (file-output-stream output))))
      (setf (file-output-stream output) nil)
      (let ((buffer (file-output-buffer output)))
        (when buffer
          (char-buffer-clear buffer)))))
  (values))
