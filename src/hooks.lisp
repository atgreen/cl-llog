;;;; hooks.lisp - Hook system for extensibility
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Hook Structure

(defstruct hook
  "A hook that can be attached to a logger for extensibility.

   TYPE - :pre-log, :post-log, or :error
   FUNCTION - The function to call (signature depends on type)
   NAME - Optional identifier for the hook
   PRIORITY - Numeric priority (lower = earlier, default 50)"
  (type nil :type (member :pre-log :post-log :error))
  (function nil :type function)
  (name nil :type (or null symbol cl:string))
  (priority 50 :type integer))

;;; Hook Function Signatures:
;;; :pre-log  - (lambda (logger entry) ...) => entry or nil (to filter)
;;; :post-log - (lambda (logger entry) ...) => ignored
;;; :error    - (lambda (logger error entry) ...) => ignored

;;; Hook Storage Access

(defun %get-hooks (logger type)
  "Get hooks of TYPE from LOGGER (internal, assumes lock held)."
  (declare (type logger logger)
           (type (member :pre-log :post-log :error) type))
  (ecase type
    (:pre-log (slot-value logger 'pre-log-hooks))
    (:post-log (slot-value logger 'post-log-hooks))
    (:error (slot-value logger 'error-hooks))))

(defun %set-hooks (logger type hooks)
  "Set hooks of TYPE for LOGGER (internal, assumes lock held)."
  (declare (type logger logger)
           (type (member :pre-log :post-log :error) type)
           (type list hooks))
  (ecase type
    (:pre-log (setf (slot-value logger 'pre-log-hooks) hooks))
    (:post-log (setf (slot-value logger 'post-log-hooks) hooks))
    (:error (setf (slot-value logger 'error-hooks) hooks))))

;;; Hook API

(defun add-hook (logger type function &key name (priority 50))
  "Add a hook to LOGGER.

   TYPE - :pre-log, :post-log, or :error
   FUNCTION - The hook function to call
   NAME - Optional identifier for the hook
   PRIORITY - Numeric priority (lower = earlier, default 50)

   Hook function signatures:
   - :pre-log  (lambda (logger entry) ...) => entry or nil (to filter)
   - :post-log (lambda (logger entry) ...) => ignored
   - :error    (lambda (logger error entry) ...) => ignored

   Returns the hook object."
  (declare (type logger logger)
           (type (member :pre-log :post-log :error) type)
           (type function function)
           (type integer priority))
  (let ((hook (make-hook :type type
                         :function function
                         :name name
                         :priority priority)))
    (with-lock-held ((logger-lock logger))
      (let ((current (%get-hooks logger type)))
        ;; Add and sort by priority (stable sort maintains insertion order for equal priorities)
        (%set-hooks logger type
                    (stable-sort (cons hook current)
                                 #'<
                                 :key #'hook-priority))))
    hook))

(defun remove-hook (logger type &key name function)
  "Remove hook(s) from LOGGER matching NAME or FUNCTION.
   At least one of NAME or FUNCTION must be provided.
   Returns the number of hooks removed."
  (declare (type logger logger)
           (type (member :pre-log :post-log :error) type))
  (unless (or name function)
    (cl:error "Must specify at least one of :name or :function"))
  (with-lock-held ((logger-lock logger))
    (let* ((current (%get-hooks logger type))
           (new (remove-if (lambda (hook)
                             (or (and name (equal name (hook-name hook)))
                                 (and function (eql function (hook-function hook)))))
                           current)))
      (%set-hooks logger type new)
      (- (length current) (length new)))))

(defun clear-hooks (logger &optional type)
  "Clear all hooks from LOGGER, optionally only those of TYPE.
   Returns the number of hooks removed."
  (declare (type logger logger)
           (type (or null (member :pre-log :post-log :error)) type))
  (with-lock-held ((logger-lock logger))
    (if type
        (let ((count (length (%get-hooks logger type))))
          (%set-hooks logger type nil)
          count)
        (let ((count (+ (length (slot-value logger 'pre-log-hooks))
                        (length (slot-value logger 'post-log-hooks))
                        (length (slot-value logger 'error-hooks)))))
          (setf (slot-value logger 'pre-log-hooks) nil
                (slot-value logger 'post-log-hooks) nil
                (slot-value logger 'error-hooks) nil)
          count))))

(defun list-hooks (logger &optional type)
  "List all hooks for LOGGER, optionally filtered by TYPE.
   Returns a list of hook objects."
  (declare (type logger logger)
           (type (or null (member :pre-log :post-log :error)) type))
  (with-lock-held ((logger-lock logger))
    (if type
        (copy-list (%get-hooks logger type))
        (append (copy-list (slot-value logger 'pre-log-hooks))
                (copy-list (slot-value logger 'post-log-hooks))
                (copy-list (slot-value logger 'error-hooks))))))

;;; Hook Execution

(defun %call-pre-log-hooks (logger entry)
  "Call all pre-log hooks for LOGGER on ENTRY.
   Returns the (possibly modified) entry, or NIL to filter it out.
   Hooks are called with error isolation."
  (declare (type logger logger)
           (type log-entry entry))
  (let (hooks)
    (with-lock-held ((logger-lock logger))
      (setf hooks (copy-list (slot-value logger 'pre-log-hooks))))

    (let ((result entry))
      (dolist (hook hooks result)
        (when result  ; Stop if a previous hook filtered the entry
          (handler-case
              (setf result (funcall (hook-function hook) logger result))
            (cl:error (e)
              ;; Log hook errors to *error-output* to avoid recursion
              (format *error-output*
                      "~&LLOG: Pre-log hook ~A error: ~A~%"
                      (or (hook-name hook) "(unnamed)")
                      e))))))))

(defun %call-post-log-hooks (logger entry)
  "Call all post-log hooks for LOGGER on ENTRY.
   Hooks are called with error isolation."
  (declare (type logger logger)
           (type log-entry entry))
  (let (hooks)
    (with-lock-held ((logger-lock logger))
      (setf hooks (copy-list (slot-value logger 'post-log-hooks))))

    (dolist (hook hooks)
      (handler-case
          (funcall (hook-function hook) logger entry)
        (cl:error (e)
          (format *error-output*
                  "~&LLOG: Post-log hook ~A error: ~A~%"
                  (or (hook-name hook) "(unnamed)")
                  e))))
    (values)))

(defun %call-error-hooks (logger error entry)
  "Call all error hooks for LOGGER on ERROR and ENTRY.
   Hooks are called with error isolation."
  (declare (type logger logger)
           (type log-entry entry))
  (let (hooks)
    (with-lock-held ((logger-lock logger))
      (setf hooks (copy-list (slot-value logger 'error-hooks))))

    (dolist (hook hooks)
      (handler-case
          (funcall (hook-function hook) logger error entry)
        (cl:error (e)
          ;; Even error hooks can fail - just print to stderr
          (format *error-output*
                  "~&LLOG: Error hook ~A error: ~A~%"
                  (or (hook-name hook) "(unnamed)")
                  e))))
    (values)))
