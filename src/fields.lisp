;;;; fields.lisp - Field types and protocol
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Field Structure
;;; Represents a key-value pair with type information

(defstruct (field (:constructor %make-field))
  "A structured logging field with name, value, and type."
  (name "" :type cl:string)
  (value nil)
  (type nil :type symbol))

(declaim (inline make-field))
(defun make-field (name value type)
  "Create a field with NAME, VALUE, and TYPE."
  (declare (type cl:string name)
           (type symbol type)
           (optimize speed))
  (%make-field :name name :value value :type type))

;;; Typed Field Constructors
;;; For zero-allocation typed API

(declaim (inline string))
(defun string (name value)
  "Create a string field."
  (declare (type cl:string name value)
           (optimize speed (safety 0)))
  (make-field name value :string))

(declaim (inline int))
(defun int (name value)
  "Create an integer field."
  (declare (type cl:string name)
           (type integer value)
           (optimize speed (safety 0)))
  (make-field name value :int))

(declaim (inline float))
(defun float (name value)
  "Create a float field."
  (declare (type cl:string name)
           (type real value)
           (optimize speed (safety 0)))
  (make-field name (cl:float value 1.0d0) :float))

(declaim (inline bool))
(defun bool (name value)
  "Create a boolean field."
  (declare (type cl:string name)
           (optimize speed (safety 0)))
  (make-field name value :bool))

(declaim (inline timestamp))
(defun timestamp (name &optional (value (get-universal-time)))
  "Create a timestamp field. If VALUE is not provided, uses current time."
  (declare (type cl:string name)
           (type unsigned-byte value)
           (optimize speed (safety 0)))
  (make-field name value :timestamp))

(defun duration-ms (name value)
  "Create a duration field in milliseconds."
  (declare (type cl:string name)
           (type real value)
           (optimize speed (safety 1)))
  (make-field name (coerce value 'double-float) :duration-ms))

(defun error-field (name condition)
  "Create an error field from a condition."
  (declare (type cl:string name)
           (optimize speed))
  (make-field name condition :error))

;;; Field Protocol
;;; Generic function for encoding fields - can be extended for custom types

(defgeneric encode-field (encoder stream field)
  (:documentation "Encode a FIELD to STREAM using ENCODER.
   This generic function can be specialized for custom field types."))

;;; Helper for converting keyword args to fields (for sugared API)

(defun keywords-to-fields (keyword-args)
  "Convert a plist of keyword arguments to a list of field structures.
   Automatically infers type from value. If value is already a field,
   uses it directly (for error-field-detailed, etc.)."
  (declare (optimize speed))
  (loop for (key value) on keyword-args by #'cddr
        collect (if (field-p value)
                    value  ; Already a field, use it directly
                    (make-field
                     (string-downcase (symbol-name key))
                     value
                     (infer-field-type value)))))

(defun infer-field-type (value)
  "Infer the field type from a value."
  (declare (optimize speed))
  (typecase value
    (cl:string :string)
    (integer :int)
    (cl:float :float)
    (null :bool)
    ((eql t) :bool)
    (cl:error :error)
    (t :any)))
