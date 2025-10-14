;;;; conditions.lisp - Condition system integration
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:llog)

;;; Condition Information Capture

(defstruct condition-info
  "Structured information about a condition."
  (type nil :type symbol)
  (message "" :type cl:string)
  (backtrace nil :type list)
  (restarts nil :type list)
  (cause nil))

(defun capture-backtrace (&optional (count 20))
  "Capture a backtrace with COUNT frames. Returns a list of strings.
   Implementation-specific with graceful fallbacks."
  (declare (type fixnum count))
  #+sbcl
  (let ((frames nil))
    (sb-debug::map-backtrace
     (lambda (frame)
       (push (with-output-to-string (s)
               (sb-debug::print-frame-call frame s))
             frames))
     :count count)
    (nreverse frames))
  #+ccl
  (let ((frames nil))
    (ccl::map-call-frames
     (lambda (frame-ptr context)
       (declare (ignore context))
       (multiple-value-bind (lfun pc)
           (ccl::cfp-lfun frame-ptr)
         (when lfun
           (push (format nil "~A [~X]"
                        (ccl::function-name lfun)
                        pc)
                 frames))))
     :count count)
    (nreverse frames))
  #-(or sbcl ccl)
  (list "Backtrace not available on this implementation"))

(defun capture-restarts ()
  "Capture information about available restarts.
   Returns a list of plists with :name and :description."
  (loop for restart in (compute-restarts)
        collect (list :name (restart-name restart)
                     :description (with-output-to-string (s)
                                   (princ restart s)))))

(defun condition-cause (condition)
  "Extract the cause of CONDITION if it has one.
   Attempts to find nested/wrapped conditions."
  ;; Many condition systems store causes in slots, but there's no standard
  ;; Check common slot names
  (loop for slot-name in '(cause parent wrapped-condition original-condition)
        for value = (handler-case
                        (slot-value condition slot-name)
                      (error () nil))
        when value
          return value
        finally (return nil)))

(defun condition-chain (condition)
  "Return a list of conditions in the causal chain starting from CONDITION."
  (loop with count = 0
        for c = condition then (condition-cause c)
        while c
        collect c
        do (incf count)
        when (> count 10) ; Prevent infinite loops
          do (return (list condition))))

(defun analyze-condition (condition &key (backtrace t) (restarts t) (chain t))
  "Analyze CONDITION and return structured information.

   Keywords:
     :backtrace - Capture backtrace (default: T)
     :restarts - Capture restart information (default: T)
     :chain - Follow condition chain (default: T)

   Returns a CONDITION-INFO structure."
  (make-condition-info
   :type (type-of condition)
   :message (princ-to-string condition)
   :backtrace (when backtrace (capture-backtrace))
   :restarts (when restarts (capture-restarts))
   :cause (when chain (condition-cause condition))))

;;; Enhanced Error Field Constructor

(defun error-field-detailed (name condition &key (backtrace t) (restarts nil) (chain nil))
  "Create an error field with detailed condition information.

   Arguments:
     name - Field name
     condition - The condition object

   Keywords:
     :backtrace - Include backtrace (default: T)
     :restarts - Include restart information (default: NIL)
     :chain - Include condition cause chain (default: NIL)

   Example:
     (llog:error-field-detailed \"error\" err :backtrace t :restarts t)"
  (make-field name
              (analyze-condition condition
                                :backtrace backtrace
                                :restarts restarts
                                :chain chain)
              :error-detailed))

;;; Encoding Support for Enhanced Conditions

(defun encode-condition-info-json (stream info)
  "Encode a CONDITION-INFO structure to JSON."
  (write-char #\{ stream)
  (let ((first t))
    (flet ((add-field (name value-writer)
             (unless first (write-char #\, stream))
             (setf first nil)
             (%write-json-string stream name)
             (write-char #\: stream)
             (funcall value-writer)))

      ;; Type
      (add-field "type"
                 (lambda ()
                   (%write-json-string stream (prin1-to-string (condition-info-type info)))))

      ;; Message
      (add-field "message"
                 (lambda ()
                   (%write-json-string stream (condition-info-message info))))

      ;; Backtrace (if present)
      (when (condition-info-backtrace info)
        (add-field "backtrace"
                   (lambda ()
                     (write-char #\[ stream)
                     (loop for frame in (condition-info-backtrace info)
                           for first-frame = t then nil
                           do (unless first-frame (write-char #\, stream))
                              (%write-json-string stream frame))
                     (write-char #\] stream))))

      ;; Restarts (if present)
      (when (condition-info-restarts info)
        (add-field "restarts"
                   (lambda ()
                     (write-char #\[ stream)
                     (loop for restart in (condition-info-restarts info)
                           for first-restart = t then nil
                           do (unless first-restart (write-char #\, stream))
                              (write-char #\{ stream)
                              (%write-json-string stream "name")
                              (write-char #\: stream)
                              (%write-json-string stream (prin1-to-string (getf restart :name)))
                              (write-char #\, stream)
                              (%write-json-string stream "description")
                              (write-char #\: stream)
                              (%write-json-string stream (getf restart :description))
                              (write-char #\} stream))
                     (write-char #\] stream))))

      ;; Cause (if present)
      (when (condition-info-cause info)
        (add-field "cause"
                   (lambda ()
                     (%encode-json-condition stream (condition-info-cause info)))))))

  (write-char #\} stream))

(defun encode-condition-info-buffer (buffer info)
  "Encode a CONDITION-INFO structure into a character buffer."
  (char-buffer-push-char buffer #\{)
  (let ((first t))
    (flet ((add-field (name value-writer)
             (unless first (char-buffer-push-char buffer #\,))
             (setf first nil)
             (%json-buffer-write-string buffer name)
             (char-buffer-push-char buffer #\:)
             (funcall value-writer)))

      ;; Type
      (add-field "type"
                 (lambda ()
                   (%json-buffer-write-string buffer (prin1-to-string (condition-info-type info)))))

      ;; Message
      (add-field "message"
                 (lambda ()
                   (%json-buffer-write-string buffer (condition-info-message info))))

      ;; Backtrace
      (when (condition-info-backtrace info)
        (add-field "backtrace"
                   (lambda ()
                     (char-buffer-push-char buffer #\[)
                     (loop for frame in (condition-info-backtrace info)
                           for first-frame = t then nil
                           do (unless first-frame (char-buffer-push-char buffer #\,))
                              (%json-buffer-write-string buffer frame))
                     (char-buffer-push-char buffer #\]))))

      ;; Restarts
      (when (condition-info-restarts info)
        (add-field "restarts"
                   (lambda ()
                     (char-buffer-push-char buffer #\[)
                     (loop for restart in (condition-info-restarts info)
                           for first-restart = t then nil
                           do (unless first-restart (char-buffer-push-char buffer #\,))
                              (char-buffer-push-char buffer #\{)
                              (%json-buffer-write-string buffer "name")
                              (char-buffer-push-char buffer #\:)
                              (%json-buffer-write-string buffer (prin1-to-string (getf restart :name)))
                              (char-buffer-push-char buffer #\,)
                              (%json-buffer-write-string buffer "description")
                              (char-buffer-push-char buffer #\:)
                              (%json-buffer-write-string buffer (getf restart :description))
                              (char-buffer-push-char buffer #\}))
                     (char-buffer-push-char buffer #\]))))

      ;; Cause
      (when (condition-info-cause info)
        (add-field "cause"
                   (lambda ()
                     (%json-buffer-write-condition buffer (condition-info-cause info)))))))

  (char-buffer-push-char buffer #\})
  buffer)
