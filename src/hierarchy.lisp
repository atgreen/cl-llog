;;;; hierarchy.lisp - Hierarchical logger naming and registry
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Logger Registry
;;; Thread-safe registry for hierarchical logger management

(defvar *logger-registry* (make-hash-table :test 'equal)
  "Global registry mapping logger names to logger instances.")

(defvar *registry-lock* (make-lock "llog/registry")
  "Lock protecting the logger registry.")

;;; Name Parsing

(defun split-string (string delimiter)
  "Split STRING on DELIMITER character, returning list of substrings."
  (declare (type cl:string string)
           (type character delimiter))
  (loop with start = 0
        for end = (position delimiter string :start start)
        collect (subseq string start end)
        while end
        do (setf start (1+ end))))

(defun split-logger-name (name)
  "Split a logger name into components.
   Example: \"app.db.query\" => (\"app\" \"app.db\" \"app.db.query\")"
  (declare (type cl:string name))
  (if (string= name "")
      nil
      (let ((parts (split-string name #\.))
            (result nil)
            (current ""))
        (dolist (part parts)
          (setf current (if (string= current "")
                            part
                            (concatenate 'cl:string current "." part)))
          (push current result))
        (nreverse result))))

(defun parent-logger-name (name)
  "Get the parent logger name for NAME.
   Example: \"app.db.query\" => \"app.db\"
            \"app\" => \"\"
            \"\" => NIL"
  (declare (type cl:string name))
  (let ((pos (position #\. name :from-end t)))
    (if pos
        (subseq name 0 pos)
        (if (string= name "")
            nil
            ""))))

;;; Logger Registry API

(defun register-logger (name logger)
  "Register a logger instance in the global registry."
  (declare (type cl:string name)
           (type logger logger))
  (with-lock-held (*registry-lock*)
    (setf (gethash name *logger-registry*) logger))
  logger)

(defun find-logger (name)
  "Find a logger by name in the registry. Returns NIL if not found."
  (declare (type cl:string name))
  (with-lock-held (*registry-lock*)
    (gethash name *logger-registry*)))

(defun find-parent-logger (name)
  "Find the nearest parent logger for NAME. Returns NIL if no parent exists."
  (declare (type cl:string name))
  (let ((parent-name (parent-logger-name name)))
    (when parent-name
      (or (find-logger parent-name)
          (find-parent-logger parent-name)))))

(defun clear-logger-registry ()
  "Clear all loggers from the registry."
  (with-lock-held (*registry-lock*)
    (clrhash *logger-registry*)))

(defun list-loggers ()
  "Return a list of all registered logger names."
  (with-lock-held (*registry-lock*)
    (loop for name being the hash-keys of *logger-registry*
          collect name)))

;;; Hierarchical Logger Creation

(defun get-logger (name &key level outputs)
  "Get or create a logger with the given NAME.

   If the logger already exists, it is returned (LEVEL and OUTPUTS are ignored).
   If creating a new logger:
   - If LEVEL/OUTPUTS are provided, they are used
   - Otherwise, configuration is inherited from the nearest parent logger
   - If no parent exists, defaults are used

   Examples:
     (get-logger \"app\")                      ; Root logger for app
     (get-logger \"app.db\")                   ; Inherits from \"app\"
     (get-logger \"app.db.query\")             ; Inherits from \"app.db\"
     (get-logger \"api\" :level :debug)        ; Explicit level override"
  (declare (type cl:string name))

  ;; Check if logger already exists
  (or (find-logger name)

      ;; Create new logger with inheritance
      (let* ((parent (find-parent-logger name))
             (inherited-level (if parent
                                  (logger-level parent)
                                  +info+))
             (inherited-outputs (if parent
                                    (copy-list (logger-outputs parent))
                                    (list (make-stream-output *standard-output*))))
             (logger-level (if level
                               (if (integerp level)
                                   level
                                   (or (parse-level level) inherited-level))
                               inherited-level))
             (logger-outputs (or outputs inherited-outputs))
             (new-logger (make-instance 'logger
                                        :name name
                                        :level logger-level
                                        :outputs logger-outputs)))
        (register-logger name new-logger))))

(defun ensure-logger-hierarchy (name)
  "Ensure all parent loggers exist for NAME, creating them if needed.
   Returns the logger for NAME."
  (declare (type cl:string name))
  (mapc #'get-logger (split-logger-name name))
  (get-logger name))

;;; Configuration Propagation

(defun set-logger-level-recursive (name level-designator &key (propagate t))
  "Set the level for logger NAME and optionally propagate to children.

   If PROPAGATE is T (default), all child loggers inherit the new level.
   If PROPAGATE is NIL, only the named logger is affected."
  (declare (type cl:string name))
  (let ((level (parse-level level-designator)))
    (unless level
      (cl:error "Invalid log level: ~S" level-designator))

    (let ((logger (get-logger name)))
      (set-level logger level)

      (when propagate
        (dolist (child-name (list-loggers))
          (when (and (not (string= child-name name))
                     (or (string= name "")  ; Root logger - propagate to all
                         (starts-with-p child-name (concatenate 'cl:string name "."))))
            (let ((child (find-logger child-name)))
              (when child
                (set-level child level))))))))
  name)

(defun starts-with-p (string prefix)
  "Check if STRING starts with PREFIX."
  (declare (type cl:string string prefix))
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

;;; Root Logger

(defun root-logger ()
  "Get or create the root logger (empty name \"\")."
  (get-logger ""))

(defun set-root-level (level-designator)
  "Set the level for the root logger and all children."
  (set-logger-level-recursive "" level-designator :propagate t))
