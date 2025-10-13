;;;; outputs/file.lisp - File output implementation
;;;; SPDX-License-Identifier: MIT

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
   (buffer-stream
    :initarg :buffer-stream
    :accessor file-output-buffer-stream
    :documentation "String output stream for :block buffering")
   (buffer-length
    :initarg :buffer-length
    :accessor file-output-buffer-length
    :type fixnum
    :documentation "Current number of buffered characters in :block mode")
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
           (buffer-stream (when (eq mode :block)
                             (make-string-output-stream)))
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
                     :buffer-stream buffer-stream
                     :buffer-length 0
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
    (write-string payload stream)
    (when force-p
      (force-output stream))))

(defun %flush-buffer (output)
  "Flush the block buffer to disk."
  (let ((buffer-stream (file-output-buffer-stream output)))
    (when buffer-stream
      (let ((buffer (get-output-stream-string buffer-stream)))
        (setf (file-output-buffer-length output) 0
              (file-output-buffer-stream output) (make-string-output-stream))
        (when (> (length buffer) 0)
          (%write-direct output buffer nil)
          (force-output (file-output-stream output)))))))

(defmethod write-entry ((output file-output) (entry log-entry))
  (when (level>= (log-entry-level entry) (output-min-level output))
    (let ((payload (with-output-to-string (stream)
                     (encode-entry (output-encoder output) stream entry))))
      (let ((mode (file-output-buffer-mode output)))
        (with-lock-held ((file-output-lock output))
          (ecase mode
            (:none
             (%write-direct output payload t))
            (:line
             (%write-direct output payload
                            (and (> (length payload) 0)
                                 (char= (char payload (1- (length payload))) #\Newline))))
            (:block
             (let* ((len (length payload))
                    (limit (file-output-buffer-size output)))
               ;; If payload is larger than buffer, flush current buffer and write directly.
               (when (> len limit)
                 (%flush-buffer output)
                 (%write-direct output payload nil)
                 (return-from write-entry))
               ;; Ensure buffer stream exists
               (%ensure-file-stream output)
               (let ((buffer-stream (file-output-buffer-stream output)))
                 (when (> (+ (file-output-buffer-length output) len) limit)
                   (%flush-buffer output)
                   (setf buffer-stream (file-output-buffer-stream output)))
                 (write-string payload buffer-stream)
                 (incf (file-output-buffer-length output) len)
                 (when (>= (file-output-buffer-length output) limit)
                   (%flush-buffer output))))))))))
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
      (setf (file-output-stream output) nil
            (file-output-buffer-stream output) (when (eq (file-output-buffer-mode output) :block)
                                                 (make-string-output-stream))
            (file-output-buffer-length output) 0)))
  (values))
