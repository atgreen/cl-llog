;;;; levels.lisp - Log level definitions and utilities
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Log Level Constants
;;; Using fixnum for fast comparison

(defconstant +trace+ 0
  "TRACE level - finest-grained debugging information")

(defconstant +debug+ 1
  "DEBUG level - detailed debugging information")

(defconstant +info+ 2
  "INFO level - general informational messages")

(defconstant +warn+ 3
  "WARN level - warning messages for potentially harmful situations")

(defconstant +error+ 4
  "ERROR level - error events that might still allow continued execution")

(defconstant +fatal+ 5
  "FATAL level - severe errors that lead to application termination")

(defconstant +panic+ 6
  "PANIC level - critical errors that should invoke the debugger")

;;; Level Names
;;; For encoding and display

(defparameter *level-names*
  #("TRACE" "DEBUG" "INFO" "WARN" "ERROR" "FATAL" "PANIC")
  "Vector mapping level integers to their string names")

(defparameter *level-keywords*
  #(:trace :debug :info :warn :error :fatal :panic)
  "Vector mapping level integers to their keyword names")

(defparameter *keyword-to-level*
  '((:trace . 0)
    (:debug . 1)
    (:info . 2)
    (:warn . 3)
    (:error . 4)
    (:fatal . 5)
    (:panic . 6))
  "Alist mapping keyword names to level integers")

;;; Level Utilities

(declaim (inline level-name))
(defun level-name (level)
  "Return the string name for a log LEVEL integer."
  (declare (type (integer 0 6) level)
           (optimize speed (safety 1)))
  (aref *level-names* level))

(declaim (inline level-keyword))
(defun level-keyword (level)
  "Return the keyword name for a log LEVEL integer."
  (declare (type (integer 0 6) level)
           (optimize speed (safety 1)))
  (aref *level-keywords* level))

(defun keyword-to-level (keyword)
  "Convert a keyword to a log level integer. Returns NIL if invalid."
  (declare (type keyword keyword)
           (optimize speed (safety 1)))
  (rest (assoc keyword *keyword-to-level*)))

(defun parse-level (level-designator)
  "Parse a level designator (keyword, string, or integer) to a level integer.
   Returns NIL if invalid."
  (etypecase level-designator
    ((integer 0 6) level-designator)
    (keyword (keyword-to-level level-designator))
    (cl:string
     (let ((keyword (find-symbol (string-upcase level-designator) :keyword)))
       (when keyword
         (keyword-to-level keyword))))))

(declaim (inline level>=))
(defun level>= (level1 level2)
  "Return T if LEVEL1 is greater than or equal to LEVEL2."
  (declare (type fixnum level1 level2)
           (optimize speed (safety 0)))
  (>= level1 level2))
