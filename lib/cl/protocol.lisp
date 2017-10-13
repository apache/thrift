(in-package #:org.apache.thrift.implementation)

;;; This file defines the abstract '`protocol` layer for the `org.apache.thrift` library.
;;;
;;; copyright 2010 [james anderson](james.anderson@setf.de)
;;;
;;; Licensed to the Apache Software Foundation (ASF) under one
;;; or more contributor license agreements. See the NOTICE file
;;; distributed with this work for additional information
;;; regarding copyright ownership. The ASF licenses this file
;;; to you under the Apache License, Version 2.0 (the
;;; "License"); you may not use this file except in compliance
;;; with the License. You may obtain a copy of the License at
;;; 
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an
;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;; KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations
;;; under the License.


;;; The protocol class is the abstract root for comminucation protocol implementations.
;;; It is specialized for each message structure
;;;
;;; protocol
;;; - encoded-protocol
;;;   - binary-protocol (see binary-protocol.lisp)
;;;
;;; The abstract class determines the abstract representation of message components in terms of
;;; and arrangement of Thrift data types. Each concrete protocol class implements the codec for
;;; base data types in terms of signed bytes and unsigned byte sequences. It then delegates to
;;; its input/output transports to decode and encode that data in terms of the transport's
;;; representation.
;;; nb. there is a bnf, protocols are observed to eliminate and/or reorder fields at will. whatever.

;;; The stream interface operators are implemented in two forms. A generic interface is specialized
;;; by protocol and/or actual data argument type. In addition a compiler-macro complement performs
;;; compile-time in-line codec expansion when the data type is statically specified. As Thrift
;;; requires all types to be declared statically, this compiles IDL files to in-line codecs.
;;;
;;; Type comparisons - both at compile-time and as run-time validation, are according to nominal equality.
;;; As the Thrift type system permits no sub-typing, primtive types are a finite set and the struct/exception
;;; classes permit no super-types.
;;; The only variation would be to to permit integer subtypes for integer container elements, eg i8 sent
;;; where i32 was declared, but that would matter only if supporting a compact protocol.
;;;
;;; Names exists in two domains:
;;; - An 'identifier' is a string. They are used when the package is unknown, which is the situation at
;;;   the start of a message. After that, a package is imputed from the association between a found message
;;;   and its service context
;;; - A 'name' uis a symbol. These are interned into a services 'namespace' when the idl is compiled.
;;;   These interned names are compiled onto request/response functions and the struct codecs.
;;;   When messages are read, the respective service's namespace package applies to intern identifiers
;;;   to match them against decoding type constraints.

;;;
;;; interface

(defgeneric stream-read-type (protocol))
(defgeneric stream-read-message-type (protocol))
(defgeneric stream-read-bool (protocol))
(defgeneric stream-read-i8 (protocol))
(defgeneric stream-read-i16 (protocol))
(defgeneric stream-read-i32 (protocol))
(defgeneric stream-read-i64 (protocol))
(defgeneric stream-read-double (protocol))
(defgeneric stream-read-string (protocol))
(defgeneric stream-read-binary (protocol))

(defgeneric stream-read-message-begin (protocol))
(defgeneric stream-read-message (protocol))
(defgeneric stream-read-message-end (protocol))
(defgeneric stream-read-struct-begin (protocol))
(defgeneric stream-read-struct (protocol &optional type))
(defgeneric stream-read-struct-end (protocol))
(defgeneric stream-read-field-begin (protocol))
(defgeneric stream-read-field (protocol &optional type))
(defgeneric stream-read-field-end (protocol))
(defgeneric stream-read-map-begin (protocol))
(defgeneric stream-read-map (protocol &optional key-type value-type))
(defgeneric stream-read-map-end (protocol))
(defgeneric stream-read-list-begin (protocol))
(defgeneric stream-read-list (protocol &optional type))
(defgeneric stream-read-list-end (protocol))
(defgeneric stream-read-set-begin (protocol))
(defgeneric stream-read-set (protocol &optional type))
(defgeneric stream-read-set-end (protocol))

(defgeneric stream-write-type (protocol type-name))
(defgeneric stream-write-message-type (protocol type-name))
(defgeneric stream-write-bool (protocol value))
(defgeneric stream-write-i8 (protocol value))
(defgeneric stream-write-i16 (protocol value))
(defgeneric stream-write-i32 (protocol value))
(defgeneric stream-write-i64 (protocol value))
(defgeneric stream-write-double (protocol value))
#+digitool ;;digitools stream-write-string signature requires four arguments. leave it to be shadowed
(defgeneric stream-write-string (protocol value &optional start end))
(defgeneric stream-write-binary (protocol value))

(defgeneric stream-write-message-begin (protocol identifier type seq))
(defgeneric stream-write-message (protocol struct message-type &key type sequence-number))
(defgeneric stream-write-message-end (protocol))
(defgeneric stream-write-struct-begin (protocol identifier))
(defgeneric stream-write-struct (protocol value &optional type))
(defgeneric stream-write-struct-end (protocol))
(defgeneric stream-write-field-begin (protocol identifier-name type identifier-number))
(defgeneric stream-write-field (protocol value &key identifier-name identifier-number type))
(defgeneric stream-write-field-end (protocol))
(defgeneric stream-write-field-stop (protocol))
(defgeneric stream-write-map-begin (protocol key-type value-type size))
(defgeneric stream-write-map (protocol value &optional key-type value-type))
(defgeneric stream-write-map-end (protocol))
(defgeneric stream-write-list-begin (protocol etype size))
(defgeneric stream-write-list (protocol value &optional type))
(defgeneric stream-write-list-end (protocol))
(defgeneric stream-write-set-begin (protocol etype size))
(defgeneric stream-write-set (protocol value &optional type))
(defgeneric stream-write-set-end (protocol))



;;;
;;; macros
;;; nb. this does not interact at all nicely with redefined macros.

(defmacro trace-macro (&rest names)
  `(progn ,@(loop for name in names collect `(%trace-macro ',name))))
(defmacro untrace-macro (&rest names)
  `(progn ,@(loop for name in names collect `(%untrace-macro ',name))))

(defun %trace-macro (name)
  (let ((function nil))
    (flet ((make-traced-expander (name function)
             #'(lambda (form environment)
                 (let ((expansion (funcall function form environment)))
                   (unless (eq form expansion)
                     (%format-macro-trace name form expansion))
                   expansion))))
      (cond ((or (get name 'macro-function) (get name 'compiler-macro-function))
             name)
            ((setf function (macro-function name))
             (setf (get name 'macro-function) function)
             (setf (macro-function name) (make-traced-expander name function))
             name)
            ((setf function (compiler-macro-function name))
             (setf (get name 'compiler-macro-function) function)
             (setf (compiler-macro-function name) (make-traced-expander name function))
             name)
            (t
             (error "~a has no macro definition." name))))))

(defun %untrace-macro (name)
  (let ((function nil))
      (cond ((setf function (get name 'macro-function))
             (setf (macro-function name) function)
             (setf (get name 'macro-function) nil)
             name)
            ((setf function (get name 'compiler-macro-function))
             (setf (compiler-macro-function name) function)
             (setf (get name 'compiler-macro-function) nil)
             name)
            (t
             nil))))

(defun %format-macro-trace (name form expansion)
  (declare (ignore name))
  (format *trace-output* "~&~%~:w~%==>~%~:w" form expansion))

(defmacro expand-iff-constant-types (type-variables form &body body)
  "Used in the codec compiler macros to conditionalize expansion on constant types."
  `(cond ((and ,@(loop for tv in type-variables collect `(typep ,tv '(cons (eql quote)))))
          ,@(loop for tv in type-variables collect `(setf ,tv (second , tv)))
          ,@body)
         (t
          ,form)))

#+digitool (setf (ccl:assq 'expand-iff-constant-types ccl:*fred-special-indent-alist*) 2)


;;;
;;; classes

(defclass protocol (#+ccl stream #+sbcl sb-gray:fundamental-stream)
  ((input-transport
    :initform (error "transport is required.") :initarg :input-transport :initarg :transport
    :reader protocol-input-transport)
   (output-transport
    :initform (error "transport is required.") :initarg :output-transport :initarg :transport
    :reader protocol-output-transport)
   (direction
    :initform (error "direction is required.") :initarg :direction
    :reader stream-direction)
   (version-id :initarg :version-id :reader protocol-version-id)
   (version-number :initarg :version-number :reader protocol-version-number)
   (sequence-number :initform 0 :accessor protocol-sequence-number)
   (field-id-mode :initarg :field-key :reader protocol-field-id-mode
                  :type (member :identifier-number :identifier-name))
   (struct-id-mode :initarg :struct-id-mode :reader protocol-struct-id-mode
                   :type (member :identifier-name :none))))


(defclass encoded-protocol (protocol)
  ((string-encoder :initarg :string-encoder :reader transport-string-encoder)
   (string-decoder :initarg :string-decoder :reader transport-string-decoder))
  (:default-initargs :charset :utf8))



;;;
;;; protocol operators


(defmethod initialize-instance ((protocol encoded-protocol) &rest initargs &key (charset nil))
  (declare (dynamic-extent initargs))
  (multiple-value-bind (decoder encoder)
                       (ecase charset
                         ((nil) (values #'(lambda (string) (map 'vector #'char-code string))
                                        #'(lambda (bytes) (map 'string #'code-char bytes))))
                         (:utf8 (values #'trivial-utf-8:utf-8-bytes-to-string
                                        #'trivial-utf-8:string-to-utf-8-bytes)))
    (apply #'call-next-method protocol
           :string-encoder encoder
           :string-decoder decoder
           initargs)))

#-mcl  ;; mcl defines a plain function in terms of stream-direction
(defmethod open-stream-p ((protocol protocol))
  (with-slots (input-transport output-transport) protocol
    (or (open-stream-p input-transport)
        (open-stream-p output-transport))))

(defun protocol-close (protocol &key abort)
  "The protocol close implementation is used by whichever interface the runtime presents for extensions.
 as per the gray interface, close is replaced with a generic function. in other cases, stream-close
 is a generic operator."
  (with-slots (input-transport output-transport stream-direction) protocol
    (when (open-stream-p protocol)
      (close input-transport :abort abort)
      (close output-transport :abort abort)
      (setf (slot-value protocol 'direction) :closed))))

(when (fboundp 'stream-close)
  (defmethod stream-close ((protocol protocol))
    (when (next-method-p) (call-next-method))
    (protocol-close protocol)))

(when (typep #'close 'generic-function)
  (defmethod close ((stream protocol) &rest args)
    (when (next-method-p) (call-next-method))
    (apply #'protocol-close stream args)))


(defgeneric protocol-version (protocol)
  (:method ((protocol protocol))
    (cons (protocol-version-id protocol) (protocol-version-number protocol))))


(defgeneric protocol-find-thrift-class (protocol name)
  (:method ((protocol protocol) (name string))
    (or (find-thrift-class (str-sym name) nil)
        (class-not-found protocol name))))


(defgeneric protocol-next-sequence-number (protocol)
  (:method ((protocol protocol))
    (incf (protocol-sequence-number protocol))))


(defmethod stream-position ((protocol protocol) &optional new-position)
  (if new-position
    (stream-position (protocol-input-transport protocol) new-position)
    (stream-position (protocol-input-transport protocol))))


;;;
;;; type  code <-> name operators are specific to each protocol

(defgeneric type-code-name (protocol code)
  )


(defgeneric type-name-code (protocol name)
  )


(defgeneric message-type-code (protocol message-name)
  )

(defgeneric message-type-name (protocol type-code)
  )



;;;
;;; input implementation
;;; nb. defined in this sequence to ensure compile-macro presence is whether loading one or
;;; reloading while developing

(defmethod stream-read-field-begin ((protocol protocol))
  "Read the field header as per protocol schema and encoding.
 PROTOCOL : protocol
 VALUES = symbol : the field name
        = i16 : the field id number
        = symbol : the field type
 The protocol's field-id-mode determines which id form to expect.
   :identifier-number : decodes a type, and unless the type is STOP a subsequent id number
   :identifier-name : decodes an identifier name and a type."
  
  (let ((type nil)
        (id-number 0)
        (identifier nil))
    (ecase (protocol-field-id-mode protocol)
      (:identifier-number
       (setf type (stream-read-type protocol))
       (unless (eq type 'stop)
         (setf id-number (stream-read-i16 protocol))))
      (:identifier-name
       (setf identifier (str-sym (stream-read-string protocol))
             ;; NB the bnf is broke here, as it says "T_STOP | <field_name> <field_type> <field_id>",
             ;; by which there is no way to distinguish the count field of a string from the stop code
             ;; so perhaps this is excluded for a binary protocol and works only if the
             ;; protocol's field are themselves self-describing
             type (stream-read-type protocol))))
    (values identifier id-number type)))


(defmethod stream-read-field-end ((protocol protocol))
  "The base method does nothing.")

(defmethod stream-read-field((protocol protocol) &optional type)
  "Read a typed field as per protocol scheme and encoding.
 PROTOCOL : protocol
 VALUES = t : the decoded field value
        = symbol : the field name
        = i16 : the field identifier number"

  (multiple-value-bind (identifier idnr read-type)
                       (stream-read-field-begin protocol)
    (if (eq read-type 'stop)
      (values nil nil 0 'stop)
      ;; constraint the read type to match the expected if one is provided.
      ;; allow for the ambiguity between string and binary types as there is just one type code
      (let ((field-value (cond ((and (eq type 'binary) (eq read-type 'string))
                                (stream-read-binary protocol))
                               ((or (null type) (equal (type-category type) read-type))
                                (stream-read-value-as protocol read-type))
                               (t
                                ;; signal an error, but use whatever it returns
                                (invalid-field-type protocol nil idnr identifier type
                                                    (stream-read-value-as protocol read-type))))))
        (stream-read-field-end protocol)
        (values field-value identifier idnr)))))

;;; a compiler macro would find no use, since the macro expansion for reading a struct already
;;; incorporates dispatches on field id to an inline-able call stream-read-value-as, while stream-read-field
;;; never itself knows the type at compile time.



(defmethod stream-read-struct-begin ((protocol protocol))
  (ecase (protocol-struct-id-mode protocol)
    (:identifier-name
     (let ((name (stream-read-string protocol)))
       (protocol-find-thrift-class protocol name)))
    (:none
     nil)))

(defmethod stream-read-struct-end ((protocol protocol)))

(defmethod stream-read-struct ((protocol protocol) &optional expected-type)
  "Interpret an encoded structure as either an expcetion or a struct depending on the specified class.
 Decode each field in turn. When decoding exceptions, build the initargs list and construct it as the
 last step. Otherwise allocate an instacen and bind each value in succession.
 Should the field fail to correspond to a known slot, delegate unknown-field to the class for a field
 defintion. If it supplies none, then resort to the class."
  
  ;; Were it slot classes only, a better protocol would be (setf slot-value-using-class), but that does not
  ;; apply to exceptions. Given both cases, this is coded to stay symmetric.
  (let* ((class (stream-read-struct-begin protocol))
         (type (when class (struct-name class))))
    (when expected-type
      (if type
        (unless (eq type expected-type)
          (invalid-struct-type protocol expected-type type))
        (setf type expected-type
              class (find-thrift-class expected-type))))
    (cond ((null type)              ; anonymous struct
           (let ((struct ()))
             (loop (multiple-value-bind (value name id field-type)
                                        (stream-read-field protocol)
                     (cond ((eq field-type 'stop)
                            (stream-read-struct-end protocol)
                            (return (nreverse struct)))
                           (t
                            (setf struct (acons (or name id) value struct))))))))
          ((subtypep type 'condition)
           ;; allocation-instance and setf slot-value) are not standard for conditions
           ;; if class-slots (as required by class-field-definitions) is not defined, this will need changes
           (let ((initargs ())
                 (fields (class-field-definitions class))
                 (fd nil))
             (loop (multiple-value-bind (value name id field-type)
                                        (stream-read-field protocol)
                     (cond ((eq field-type 'stop)
                            (stream-read-struct-end protocol)
                            (return (apply #'make-condition type initargs)))
                           ((setf fd (or (find id fields :key #'field-definition-identifier-number :test #'eql)
                                         (unknown-field class id name field-type value)))
                            (setf (getf initargs (field-definition-initarg fd)) value))
                           (t
                            (unknown-field protocol id name field-type value)))))))
          (t
           (let* ((instance (allocate-instance class))
                  (fields (class-field-definitions class))
                  (fd nil))
             (loop (multiple-value-bind (value name id field-type)
                                        (stream-read-field protocol)
                     (cond ((eq field-type 'stop)
                            (stream-read-struct-end protocol)
                            (return instance))
                           ((setf fd (or (find id fields :key #'field-definition-identifier-number :test #'eql)
                                         (unknown-field class id name field-type value)))
                            (setf (slot-value instance (field-definition-name fd))
                                  value))
                           (t
                            (unknown-field protocol id name field-type value))))))))))

(define-compiler-macro stream-read-struct (&whole form prot &optional type instance &environment env)
  "Iff the type is a constant, compile the decoder inline. If class is not defined, signal an error.
 The intended use is to compile IDL files, for which the code generator and the definition macros
 arrange that structure definitions preceed references."
  
  (expand-iff-constant-types (type) form
    (with-gensyms (expected-class)
      (with-optional-gensyms (prot) env
        (with-gensyms (initargs)
          (let* ((class (find-thrift-class type))
                 (field-definitions (class-field-definitions class))
                 (struct (gensym)))
            (if (subtypep type 'condition)
              `(let ((,initargs nil)
                     (,expected-class (find-thrift-class ',type)))
                 ,(generate-struct-decoder prot expected-class
                                           (loop for fd in field-definitions
                                                 collect `((getf ,initargs ',(field-definition-initarg fd)) nil
                                                           :id ,(field-definition-identifier-number fd)
                                                           :type ,(field-definition-type fd)))
                                           initargs)
                 (apply #'make-struct ',type ,initargs))
              `(let* ((,initargs nil)
                      (,expected-class (find-thrift-class ',type))
                      (,struct ,(if instance instance `(allocate-instance ,expected-class))))
                 ,(generate-struct-decoder prot expected-class
                                           (loop for fd in field-definitions
                                                 collect `((slot-value ,struct ',(field-definition-name fd)) nil
                                                           :id ,(field-definition-identifier-number fd)
                                                           :type ,(field-definition-type fd)))
                                           initargs)
                 (when ,initargs
                   (apply #'reinitialize-instance ,struct ,initargs))
                 ,struct))))))))

#+(or)                                  ; alternative version with make instance
(define-compiler-macro stream-read-struct (&whole form prot &optional type &environment env)
  "Iff the type is a constant, compile the decoder inline. If class is not defined, signal an error.
 The intended use is to compile IDL files, for which the code generator and the definition macros
 arrange that structure definitions preceed references."
  
  (expand-iff-constant-types (type) form
    (with-gensyms (expected-class)
      (with-optional-gensyms (prot) env
        (with-gensyms (extra-initargs)
          (let* ((class (find-thrift-class type))
                 (field-definitions (class-field-definitions class)))
            `(let (,@(loop for fd in field-definitions
                           collect (list (field-definition-name fd) nil))
                   (,extra-initargs nil)
                   (,expected-class (find-thrift-class ',type)))
               ,(generate-struct-decoder prot expected-class field-definitions extra-initargs)
               (apply #'make-struct ,expected-class
                      ,@(loop for fd in field-definitions
                              nconc (list (field-definition-initarg fd) (field-definition-name fd)))
                      ,extra-initargs))))))))





(defmethod stream-read-message-begin ((protocol protocol))
  "Read a message header strictly.
 PROTOCOL : protocol
 VALUES = string : the message identifier name
        = symbol : the message type
        = i32 : sequence number

  A backwards compatible implementation would read the entire I32, in order
 to use a value w/o the #x80000000 tag bit as a string length, but that is not necessary when reading strictly.
 The jira [issue](https://issues.apache.org/jira/browse/THRIFT-254) indicates that all implementions write
 strict by default, and that a 'next' release should treat non-strict messages as bugs.

 This version recognizes the layout established by the compact protocol, whereby the first byte is the
 protocol id and subsequent to that is specific to the protocol."

  (let* ((id (logand (stream-read-i8 protocol) #xff))          ; actually unsigned
         (ver (logand (stream-read-i8 protocol) #xff))         ; actually unsigned
         (type-name (stream-read-message-type protocol)))
    (unless (and (= (protocol-version-id protocol) id) (= (protocol-version-number protocol) ver))
      (invalid-protocol-version protocol id ver))
    (let ((name (stream-read-string protocol))
          (sequence (stream-read-i32 protocol)))
      (values name type-name sequence))))


(defmethod stream-read-message ((protocol protocol))
  "Perform a generic 'read' of a complete message.
 PROTOCOL : protocol
 VALUES : symbol : the message identifer interned in the current package
        = i32 : the sequence number
        = symbol : the message type (eg, call, reply)
        = thrift-object : the message body object

 This is here for dynamic processing and testing only. Service requests/reponses messages are not first-class
 entities. The request/response operators interpret message content on-the-fly, as side-effects against
 specially tailored lexical contexts. A response, eg, is a 'struct' in which the '0:success' field carries
 the (single) result and other fields capture exceptions. The expansions for def-request-method and
 def-response-method generate the codecs to interpret/generate the data stream without an intermediate
 reified object."

  (multiple-value-bind (message-identifier type sequence)
                       (stream-read-message-begin protocol)
    (let* ((message-name (str-sym message-identifier))
           (body (stream-read-struct protocol message-name)))
      (stream-read-message-end protocol)
      (values message-name type sequence body))))

(defmethod stream-read-message-end ((protocol protocol)))



(defmethod stream-read-map-begin ((protocol protocol))
  ; t_key t_val size
  (values (stream-read-type protocol)
          (stream-read-type protocol)
          (stream-read-i32 protocol)))

(defmethod stream-read-map-end ((protocol protocol)))

(defmethod stream-read-map((protocol protocol) &optional key-type value-type)
  (let ((map ()))
    (multiple-value-bind (read-key-type read-value-type size) (stream-read-map-begin protocol)
      (unless (or (null key-type) (equal read-key-type (type-category key-type)))
        (invalid-element-type protocol 'thrift:map key-type read-key-type))
      (unless (or (null value-type) (equal read-value-type (type-category value-type)))
        (invalid-element-type protocol 'thrift:map value-type read-value-type))
      (unless (typep size 'field-size)
        (invalid-field-size protocol 0 "" 'field-size size))
      (dotimes (i size)
        ;; no type check - presume the respective reader is correct.
        (setf map (acons (stream-read-value-as protocol read-key-type)
                         (stream-read-value-as protocol read-value-type)
                         map)))
      (stream-read-map-end protocol)
      (nreverse map))))

(define-compiler-macro stream-read-map (&whole form prot &optional key-type value-type &environment env)
  (expand-iff-constant-types (key-type value-type) form
    (with-gensyms (map)
      (with-optional-gensyms (prot) env
        `(let ((,map ()))
           (multiple-value-bind (key-type value-type size) (stream-read-map-begin ,prot)
             (unless (equal key-type ',(type-category key-type))
               (invalid-element-type ,prot 'thrift:map ',key-type key-type))
             (unless (equal value-type ',(type-category value-type))
               (invalid-element-type ,prot 'thrift:map ',value-type value-type))
             (unless (typep size 'field-size)
               (invalid-field-size ,prot 0 "" 'field-size size))
             (dotimes (i size)
               ;; no type check - presume the respective reader is correct.
               (setf ,map (acons (stream-read-value-as ,prot ',key-type)
                                 (stream-read-value-as ,prot ',value-type)
                                 ,map)))
             (stream-read-map-end ,prot)
             (nreverse ,map)))))))



(defmethod stream-read-list-begin ((protocol protocol))
  ; t_elt size
  (values (stream-read-type protocol)
          (stream-read-i32 protocol)))

(defmethod stream-read-list-end ((protocol protocol)))

(defmethod stream-read-list((protocol protocol) &optional type)
  (multiple-value-bind (read-type size)
                       (stream-read-list-begin protocol)
    (when type
      (unless (equal read-type (type-category type))
        (invalid-element-type protocol 'thrift:list type read-type)))
    (unless (typep size 'field-size)
        (invalid-field-size protocol 0 "" 'field-size size))
    (prog1 (loop for i from 0 below size
                 collect (stream-read-value-as protocol read-type))
      (stream-read-list-end protocol))))

(define-compiler-macro stream-read-list (&whole form prot &optional type &environment env)
  (expand-iff-constant-types (type) form
    (with-optional-gensyms (prot) env
    `(multiple-value-bind (type size)
                          (stream-read-list-begin ,prot)
       (unless (equal type ',(type-category type))
         (invalid-element-type ,prot 'thrift:list ',type type))
       (unless (typep size 'field-size)
         (invalid-field-size ,prot 0 "" 'field-size size))
       (prog1 (loop for i from 0 below size
                    collect (stream-read-value-as ,prot ',type))
         (stream-read-list-end ,prot))))))



(defmethod stream-read-set-begin ((protocol protocol))
  (values (stream-read-type protocol)
          (stream-read-i32 protocol)))

(defmethod stream-read-set-end ((protocol protocol)))

(defmethod stream-read-set((protocol protocol) &optional type)
  (multiple-value-bind (read-type size)
                       (stream-read-set-begin protocol)
    (when type
      (unless (equal read-type (type-category type))
        (invalid-element-type protocol 'thrift:set type read-type)))
    (unless (typep size 'field-size)
      (invalid-field-size protocol 0 "" 'field-size size))
    (prog1 (loop for i from 0 below size
                 collect (stream-read-value-as protocol read-type))
      (stream-read-set-end protocol))))

(define-compiler-macro stream-read-set (&whole form prot &optional type &environment env)
  (expand-iff-constant-types (type) form
    (with-optional-gensyms (prot) env
    `(multiple-value-bind (type size)
                          (stream-read-set-begin ,prot)
       (unless (equal type ',(type-category type))
         (invalid-element-type ,prot 'thrift:set ',type type))
       (unless (typep size 'field-size)
         (invalid-field-size ,prot 0 "" 'field-size size))
       (prog1 (loop for i from 0 below size
                    collect (stream-read-value-as ,prot ',type))
         (stream-read-set-end ,prot))))))



(defmethod stream-read-enum ((protocol protocol) type)
  "Read an i32 and verify type"
  (let ((value (stream-read-i32 protocol)))
    (unless (typep value type)
      (invalid-enum protocol type value))
    value))

(define-compiler-macro stream-read-enum (&whole form prot type)
  (expand-iff-constant-types (type) form
    #+thrift-check-types
    `(let ((value (stream-read-i32 ,prot)))
       (unless (typep value ',type)
         (invalid-enum protocol ',type value))
       value)
    `(stream-read-i32 ,prot)))


(defgeneric stream-read-value-as (protocol type)
  (:documentation "Read a value if a specified type.")
  (:method ((protocol protocol) (type-code fixnum))
    (stream-read-value-as protocol (type-code-name protocol type-code)))
  (:method ((protocol protocol) (type cons))
    (ecase (first type)
      (thrift:map (stream-read-map protocol (str-sym (second type)) (str-sym (third type))))
      (thrift:list (stream-read-list protocol (str-sym (second type))))
      (thrift:set (stream-read-set protocol (str-sym (second type))))
      (struct (stream-read-struct protocol (str-sym (second type))))
      (enum (stream-read-map protocol (str-sym (second type))))))

  (:method ((protocol protocol) (type-code (eql 'bool)))
    (stream-read-bool protocol))
  (:method ((protocol protocol) (type-code (eql 'thrift:byte)))
    ;; call through the i8 methods as byte ops are transport, not protocol methods
    (stream-read-i8 protocol))
  (:method ((protocol protocol) (type-code (eql 'i8)))
    (stream-read-i8 protocol))
  (:method ((protocol protocol) (type-code (eql 'i16)))
    (stream-read-i16 protocol))
  (:method ((protocol protocol) (type-code (eql 'enum)))
    ;; as a fall-back
    (stream-read-i16 protocol))
  (:method ((protocol protocol) (type-code (eql 'i32)))
    (stream-read-i32 protocol))
  (:method ((protocol protocol) (type-code (eql 'i64)))
    (stream-read-i64 protocol))

  (:method ((protocol protocol) (type-code (eql 'double)))
    (stream-read-double protocol))

  (:method ((protocol protocol) (type-code (eql 'string)))
    (stream-read-string protocol))
  (:method ((protocol protocol) (type-code (eql 'binary)))
    (stream-read-binary protocol))

  (:method ((protocol protocol) (type-code (eql 'struct)))
    (stream-read-struct protocol))
  (:method ((protocol protocol) (type-code (eql 'thrift:map)))
    (stream-read-map protocol))
  (:method ((protocol protocol) (type-code (eql 'thrift:list)))
    (stream-read-list protocol))
  (:method ((protocol protocol) (type-code (eql 'thrift:set)))
    (stream-read-set protocol)))


(define-compiler-macro stream-read-value-as (&whole form protocol type)
  "Given a constant type, generate the respective read operations.
 Recognizes all thrift types, container x element type combinations
 and struct classes. A void specification yields no values.
 If the type is not constant, declare to expand, which leaves the dispatch
 to run-time interpretation."

  (typecase type
    ;; just in case
    (fixnum (setf type (type-name-class type)))
    ((cons (eql quote)) (setf type (second type)))
    ;; if it's not constante, decline to expand
    (t (return-from stream-read-value-as form)))

  ;; given a constant type, attempt an expansion
  (etypecase type
    ((eql void)
     (values))
    (base-type
     `(,(cons-symbol :org.apache.thrift.implementation
                     :stream-read- type) ,protocol))
    ((member thrift:set thrift:list thrift:map)
     (warn "Compiling generic container decoder: ~s." type)
     `(,(cons-symbol :org.apache.thrift.implementation :stream-read- type)
       ,protocol))
    (container-type
     (destructuring-bind (type . element-types) type
       `(,(cons-symbol :org.apache.thrift.implementation :stream-read- type)
         ,protocol ,@(mapcar #'(lambda (type) `(quote ,type)) element-types))))
    (struct-type
     `(stream-read-struct ,protocol ',(str-sym (second type))))
    (enum-type
     `(stream-read-enum ,protocol ',(str-sym (second type))))))
  

(defgeneric stream-read-typed-value (protocol)
  (:documentation "Given a PROTOCOL instance, decode the value's type and then the value itself.
 This is used to decode adhoc data for exchange as a binary value.")

  (:method ((protocol protocol))
    (let ((type-name (stream-read-type protocol)))
      (stream-read-value-as protocol type-name))))



;;; output implementation 
;;; nb. defined in this sequence to ensure compile-macro presence is whether loading one or
;;; reloading while developing

(defmethod stream-write-field-begin ((protocol protocol) (identifier string) type identifier-number)
  (ecase (protocol-field-id-mode protocol)
    (:identifier-number (+ (stream-write-type protocol type)
                           (stream-write-i16 protocol identifier-number)))
    (:identifier-name (+ (stream-write-string protocol identifier)
                         (stream-write-type protocol type)))))

(defmethod stream-write-field-end ((protocol protocol)))

(defmethod stream-write-field-stop ((protocol protocol))
  (stream-write-type protocol 'stop))

(defmethod stream-write-field ((protocol protocol) (value t) &key identifier-name identifier-number (type (thrift:type-of value)))
  (stream-write-field-begin protocol identifier-name type identifier-number)
  (stream-write-value-as protocol value type)
  (stream-write-field-end protocol))

(define-compiler-macro stream-write-field (&whole form prot value &key identifier-name identifier-number type &environment env)
  (expand-iff-constant-types (type) form
    (with-optional-gensyms (prot) env
    `(progn (stream-write-field-begin ,prot ,identifier-name ',type ,identifier-number)
            (stream-write-value-as ,prot ,value ',type)
            (stream-write-field-end ,prot)))))



(defmethod stream-write-struct-begin ((protocol protocol) (name string))
  (ecase (protocol-struct-id-mode protocol)
    (:identifier-name
     (stream-write-string protocol name))
    (:none
     nil)))

(defmethod stream-write-struct-end ((protocol protocol)))

(defmethod stream-write-struct ((protocol protocol) (value standard-object) &optional (type (type-of value)))
  "Given a VALUE, encode it as per PROTOCOL.
 PROTOCOL : protocol
 VALUE : standard-object : the object's class must be a thrift-class to provide field metadata."

  (let ((class (find-thrift-class type)))
    (stream-write-struct-begin protocol (class-identifier class))
    (dolist (fd (class-field-definitions class))
      (let ((name (field-definition-name fd))
            (optional (field-definition-optional fd)))
        ;; if a slot is optional, then skip unbound slots
        ;; otherwise, require the value, possibly signalling the unbound situation
        (unless (and optional (not (slot-boundp value name)))
          (let ((slot-value (slot-value value name)))
            (stream-write-field protocol slot-value
                                :identifier-number (field-definition-identifier-number fd)
                                :identifier-name (field-definition-identifier fd)
                                :type (field-definition-type fd))))))
    (stream-write-field-stop protocol)
    (stream-write-struct-end protocol)))

(defmethod stream-write-struct ((protocol protocol) (value list) &optional type)
  (let* ((class (find-thrift-class type))
         (fields (class-field-definitions class)))
    (stream-write-struct-begin protocol (class-identifier class))
    (loop for (id . field-value) in value
          do (etypecase id
               (fixnum
                (let ((fd (or (find id fields :key #'field-definition-identifier-number)
                              (error 'unknown-field-error :protocol protocol :number id :name nil
                                     :structure-type type :datum field-value))))
                  (stream-write-field protocol field-value
                                      :identifier-number id
                                      :identifier-name (field-definition-identifier fd)
                                      :type (field-definition-type fd))))
               (string
                (let ((fd (or (find id fields :key #'field-definition-identifier)
                              (error 'unknown-field-error :protocol protocol :number nil :name id
                                     :structure-type type :datum field-value))))
                  (stream-write-field protocol field-value
                                      :identifier-number (field-definition-identifier-number fd)
                                      :identifier-name id
                                      :type (field-definition-type fd))))))
    (stream-write-field-stop protocol)
    (stream-write-struct-end protocol)))

(define-compiler-macro stream-write-struct (&whole form prot value &optional type &environment env)
  "Iff the type is a constant, emit the structure in-line. In this case allow also the variation, that the
 structure itself is a thrift:list a-list of (id-number . place)."
  (expand-iff-constant-types (type) form
    (etypecase type
      (symbol )
      (struct-type (setf type (second type))))
    (let* ((class (find-thrift-class type))
           (identifier (class-identifier class))
           (field-definitions (class-field-definitions class)))
      (if (typep value '(cons (eql thrift:list)))
        ;; if it's a literal environment, expand it in-line
        (with-optional-gensyms (prot) env
          `(progn (stream-write-struct-begin ,prot ,identifier)
                  ,@(loop for (nil id place) in (rest value)    ; ignore the 'cons'
                          for fd = (or (or (find id field-definitions :key #'field-definition-identifier-number)
                                           (find id field-definitions :key #'field-definition-name :test #'string-equal))
                                       (error "Field id not found: ~s, ~s" id identifier))
                          do (list id place fd)
                          collect `(stream-write-field ,prot ,place
                                                       :identifier-number ,(field-definition-identifier-number fd)
                                                       :identifier-name ,(field-definition-identifier fd)
                                                       :type ',(field-definition-type fd)))
                  (stream-write-field-stop ,prot)
                  (stream-write-struct-end ,prot)))
        ;; otherwise expand with instance field refences
        (with-optional-gensyms (prot value) env
          `(progn
             (typecase ,value
             (,type
              (stream-write-struct-begin ,prot ,identifier)
              ,@(loop for fd in field-definitions
                      collect (if (field-definition-optional fd)
                                `(when (slot-boundp ,value ',(field-definition-name fd))
                                   (let ((slot-value (,(field-definition-reader fd) ,value)))
                                     (stream-write-field ,prot slot-value
                                                         :identifier-number ,(field-definition-identifier-number fd)
                                                         :identifier-name ,(field-definition-identifier fd)
                                                         :type ',(field-definition-type fd))))
                                `(stream-write-field ,prot (,(field-definition-reader fd) ,value)
                                                     :identifier-number ,(field-definition-identifier-number fd)
                                                     :identifier-name ,(field-definition-identifier fd)
                                                     :type ',(field-definition-type fd))))
              (stream-write-field-stop ,prot)
              (stream-write-struct-end ,prot))
             (list                      ;  allow s-exp encoded structs
              (let ((type ',type))
                (stream-write-struct ,prot ,value type)))
             (t
              (assert (typep ,value ',type) ()
                      "Attempt to serialize ~s as ~s." ,value ',type)))))))))



(defmethod stream-write-message-begin ((protocol protocol) name type sequence)
  (stream-write-i8 protocol (protocol-version-id protocol))
  (stream-write-i8 protocol (protocol-version-number protocol))
  (stream-write-message-type protocol type)
  (stream-write-string protocol name)
  (stream-write-i32 protocol sequence))

(defmethod stream-write-message ((protocol protocol) (object standard-object) (message-type (eql 'call))
                                 &key (type (type-of object))
                                 (sequence-number (protocol-next-sequence-number protocol)))
  (stream-write-message-begin protocol (class-identifier type) message-type sequence-number)
  (stream-write-struct protocol object type)
  (stream-write-message-end protocol))

(defmethod stream-write-message ((protocol protocol) (object standard-object) (message-type (eql 'oneway))
                                 &key (type (type-of object))
                                 (sequence-number (protocol-next-sequence-number protocol)))
  (stream-write-message-begin protocol (class-identifier type) message-type sequence-number)
  (stream-write-struct protocol object type)
  (stream-write-message-end protocol))

(defmethod stream-write-message ((protocol protocol) (object standard-object) (message-type t)
                                 &key (type (type-of object))
                                 (sequence-number (protocol-sequence-number protocol)))
  (stream-write-message-begin protocol (class-identifier type) message-type sequence-number)
  (stream-write-struct protocol object type)
  (stream-write-message-end protocol))
  

(defmethod stream-write-message-end ((protocol protocol))
  (stream-force-output (protocol-output-transport protocol)))


(defgeneric stream-write-exception (protocol exception)
  (:method ((protocol protocol) (exception thrift-error))
    (stream-write-message protocol exception 'exception
                          :identifier (class-identifier exception)))
  
  (:method ((protocol protocol) (exception condition))
    (stream-write-message protocol
                          (make-instance 'application-error :condition exception)
                          'exception)))



(defmethod stream-write-map-begin ((protocol protocol) key-type value-type size)
  (stream-write-type protocol key-type)
  (stream-write-type protocol value-type)
  (stream-write-i32 protocol size))

(defmethod stream-write-map-end ((protocol protocol)))

(defmethod stream-write-map ((protocol protocol) (value list) &optional key-type value-type)
  (let ((size (map-size value)))
    ;; nb. no need to check size as the map size is constrained by array size limits.
    (unless key-type (setf key-type (thrift:type-of (caar value))))
    (unless value-type (setf value-type (thrift:type-of (cdar value))))
    (stream-write-map-begin protocol key-type value-type size)
    (loop for (element-key . element-value) in value
          do (progn (stream-write-value-as protocol element-key key-type)
                    (stream-write-value-as protocol element-value value-type)))
    (stream-write-map-end protocol)))

(define-compiler-macro stream-write-map (&whole form prot value &optional key-type value-type &environment env)
  (expand-iff-constant-types (key-type value-type) form
    (with-optional-gensyms (prot value) env
      `(let ((size (map-size ,value)))
         ;; nb. no need to check size as the map size is constrained by array size limits.
         (stream-write-map-begin ,prot ',key-type ',value-type size)
         (loop for (element-key . element-value) in ,value
               do (progn (stream-write-value-as ,prot element-key ',key-type)
                         (stream-write-value-as ,prot element-value ',value-type)))
         (stream-write-map-end ,prot)))))



(defmethod stream-write-list-begin ((protocol protocol) (type t) length)
  (stream-write-type protocol type)
  (stream-write-i32 protocol length))

(defmethod stream-write-list-end ((protocol protocol)))

(defmethod stream-write-list ((protocol protocol) (value list) &optional
                              (type (if value (thrift:type-of (first value)) (error "The element type is required."))))
  (let ((size (list-length value)))
    (unless (typep size 'field-size)
      (invalid-field-size protocol 0 "" 'field-size size))
    (stream-write-list-begin protocol type size)
    (dolist (elt value)
      (stream-write-value-as protocol elt type))
    (stream-write-list-end protocol)))

(define-compiler-macro stream-write-list (&whole form prot value &optional element-type &environment env)
  (expand-iff-constant-types (element-type) form
    (with-optional-gensyms (prot value) env
      `(let ((size (list-length ,value)))
         (unless (typep size 'field-size)
           (invalid-field-size ,prot 0 "" 'field-size size))
         (stream-write-list-begin ,prot ',element-type size)
         (dolist (element ,value)
           #+thrift-check-types (assert (typep element ',element-type))
           (stream-write-value-as ,prot element ',element-type))
         (stream-write-list-end ,prot)))))



(defmethod stream-write-set-begin ((protocol protocol) (type t) length)
  (stream-write-type protocol type)
  (stream-write-i32 protocol length))

(defmethod stream-write-set-end ((protocol protocol)))

(defmethod stream-write-set ((protocol protocol) (value list) &optional
                             (type (if value (thrift:type-of (first value)) (error "The element type is required."))))
  (let ((size (list-length value)))
    (unless (typep size 'field-size)
      (invalid-field-size protocol 0 "" 'field-size size))
    (stream-write-set-begin protocol type size)
    (dolist (element value)
      #+thrift-check-types (assert (typep element type))
      (stream-write-value-as protocol element type))
    (stream-write-set-end protocol)))

(define-compiler-macro stream-write-set (&whole form prot value &optional element-type &environment env)
  (expand-iff-constant-types (element-type) form
    (with-optional-gensyms (prot value) env
    `(let ((size (list-length ,value)))
       (unless (typep size 'field-size)
         (invalid-field-size ,prot 0 "" 'field-size size))
       (stream-write-set-begin ,prot ',element-type size)
       (dolist (element ,value)
         #+thrift-check-types (assert (typep element ',element-type))
         (stream-write-value-as ,prot element ',element-type))
       (stream-write-set-end ,prot)))))



(defgeneric stream-write-value (protocol value)
  (:method ((protocol protocol) (value null))
    (stream-write-bool protocol value))
  (:method ((protocol protocol) (value (eql t)))
    (stream-write-bool protocol value))
  (:method ((protocol protocol) (value integer))
    (etypecase value
     (i8 (stream-write-i8 protocol value))
     (i16 (stream-write-i16 protocol value))
     (i32 (stream-write-i32 protocol value))
     (i64 (stream-write-i64 protocol value))))

  (:method ((protocol protocol) (value float))
    (unless (typep value 'double)
      (setf value (float value 1.0d0)))
    (stream-write-double protocol value))

  (:method ((protocol protocol) (value string))
    (stream-write-string protocol value))
  (:method ((protocol protocol) (value vector))
    (stream-write-binary protocol value))
  
  (:method ((protocol protocol) (value thrift-object))
    (stream-write-struct protocol value))
  (:method ((protocol protocol) (value list))
    (if (consp (first value))
      (stream-write-map protocol value)
      (stream-write-list protocol value))))


(defgeneric stream-write-value-as (protocol value type)
  (:method ((protocol protocol) (value t) (type-code fixnum))
    (stream-write-value-as protocol value (type-code-name protocol type-code)))

  (:method ((protocol protocol) (value t) (type (eql 'bool)))
    (stream-write-bool protocol value))
  (:method ((protocol protocol) (value integer) (type (eql 'thrift:byte)))
    (stream-write-i8 protocol value))
  (:method ((protocol protocol) (value integer) (type (eql 'i8)))
    (stream-write-i8 protocol value))
  (:method ((protocol protocol) (value integer) (type (eql 'i16)))
    (stream-write-i16 protocol value))
  (:method ((protocol protocol) (value integer) (type (eql 'enum)))
    ;; as a fall-back
    (stream-write-i32 protocol value))
  (:method ((protocol protocol) (value integer) (type cons))
    ;; as a fall-back
    (stream-write-i32 protocol value))
  (:method ((protocol protocol) (value integer) (type (eql 'i32)))
    (stream-write-i32 protocol value))
  (:method ((protocol protocol) (value integer) (type (eql 'i64)))
    (stream-write-i64 protocol value))

  (:method ((protocol protocol) (value float) (type (eql 'double)))
    (unless (typep value 'double)
      (setf value (float value 1.0d0)))
    (stream-write-double protocol value))

  (:method ((protocol protocol) (value string) (type (eql 'string)))
    (stream-write-string protocol value))
  (:method ((protocol protocol) (value symbol) (type (eql 'string)))
    (stream-write-string protocol (symbol-name value)))
  (:method ((protocol protocol) (value string) (type (eql 'binary)))
    (stream-write-binary protocol value))
  (:method ((protocol protocol) (value symbol) (type (eql 'binary)))
    (stream-write-binary protocol (symbol-name value)))
  (:method ((protocol protocol) (value vector) (type (eql 'binary)))
    (stream-write-binary protocol value))

  (:method ((protocol protocol) (value thrift-object) (type (eql 'struct)))
    (stream-write-struct protocol value))
  (:method ((protocol protocol) (value thrift-object) (type symbol))
    (stream-write-struct protocol value type))
  (:method ((protocol protocol) (value thrift-object) (type cons))
    (stream-write-struct protocol value (str-sym (second type))))

  (:method ((protocol protocol) (value list) (type (eql 'thrift:map)))
    (stream-write-map protocol value))
  (:method ((protocol protocol) (value list) (type (eql 'thrift:list)))
    (stream-write-list protocol value))
  (:method ((protocol protocol) (value list) (type (eql 'thrift:set)))
    (stream-write-set protocol value))
  (:method ((protocol protocol) (value list) (type cons))
    (destructuring-bind (type t1 &optional t2) type
      (ecase type
        (thrift:list (stream-write-list protocol value (str-sym t1)))
        (thrift:set (stream-write-set protocol value (str-sym t1)))
        (thrift:map (stream-write-map protocol value (str-sym t1) (str-sym t2)))))))


(define-compiler-macro stream-write-value-as (&whole form protocol value type)
  "See stream-read-value-as."

  (typecase type
    ;; just in case
    (fixnum (setf type (type-name-class type)))
    ((cons (eql quote)) (setf type (second type)))
    ;; if it's not constant, decline to expand
    (t (return-from stream-write-value-as form)))

  ;; given a constant type, attempt an expansion
  (etypecase type
    ((eql void)
     nil)
    (base-type
     `(,(cons-symbol :org.apache.thrift.implementation :stream-write- type)
       ,protocol ,value))
    ((member thrift:set thrift:list thrift:map)
     (warn "Compiling generic container encoder: ~s." type)
     `(,(cons-symbol :org.apache.thrift.implementation :stream-write- type)
       ,protocol ,value))
    (container-type
     (destructuring-bind (type &rest element-types) type
       `(,(cons-symbol :org.apache.thrift.implementation :stream-write- type)
         ,protocol ,value ,@(mapcar #'(lambda (type) `(quote ,type)) element-types))))
    (struct-type
     `(stream-write-struct ,protocol ,value ',(str-sym (second type))))
    (enum-type
     `(stream-write-i32 ,protocol ,value))))


(defgeneric stream-write-typed-value (protocol value)
  (:documentation "Given a PROTOCOL instance and a VALUE, encode the value's type and then the value itself.
 This is used to encode adhoc data for exchange as a binary value.")

  (:method ((protocol protocol) (value t))
    (let ((type-name (thrift:type-of value)))
      (stream-write-type protocol type-name)
      (stream-write-value-as protocol value type-name))))

;;;
;;; protocol exception operators

(defgeneric application-error (protocol &key condition)
  (:method ((protocol protocol) &key condition)
    (error 'application-error :protocol protocol
           :condition condition)))


(defgeneric class-not-found (protocol identifier)
  (:method ((protocol protocol) identifier)
    (error 'class-not-found-error :protocol protocol :identifier identifier)))


(defgeneric invalid-enum (protocol type datum)
  (:method ((protocol protocol) type datum)
    (error 'enum-type-error :protocol protocol :expected-type type :datum datum)))

(defgeneric unknown-field (protocol field-id-number field-name field-type value)
  (:documentation "Called when a decoded field is not present in the specified type.
 The base method for protocols ignores it.
 A prototypical protocol/class combination could extend the class by adding a
 field definition as per the name/id/type specified and bindng the value")
  (:method ((protocol protocol) (id-number integer) (name t) (type t) (value t))
    nil)
  ;; The default method for thrift classes does nothing, which is intended to leave the final
  ;; disposition to the protocol.
  (:method ((class thrift-class) (id t) (name t) (type t) (value t))
    nil))


(defgeneric invalid-field-size (protocol field-id field-name expected-type size)
  (:documentation "Called when a read structure field exceeds the dimension limit.
 The base method for binary protocols signals a field-size-error")

  (:method ((protocol protocol) (id integer) (name t) (expected-type t) (size t))
    (error 'field-size-error :protocol protocol
           :name name :number id :expected-type expected-type :datum size)))


(defgeneric invalid-field-type (protocol structure-type field-id field-name expected-type value)
  (:documentation "Called when a read structure field is not present in the specified type.
 The base method for binary protocols signals a field-type-error")

  (:method ((protocol protocol) (structure-type t) (id-number t) (name t) (expected-type t) (value t))
    (cerror "Use the value."
            'field-type-error :protocol protocol
            :structure-type structure-type :name name :number id-number :expected-type expected-type :datum value)
    value))


(defgeneric invalid-element-type (protocol container-type expected-type type)
  (:documentation "Called when the element type of a received compound value is not the specified type.
 The base method for binary protocols signals an element-type-error")

  (:method ((protocol protocol) container-type (expected-type t) (type t))
    (error 'element-type-error :protocol protocol
           :container-type container-type :expected-type expected-type :element-type type)))


(defgeneric unknown-method (protocol method-identifier sequence message)
  (:method ((protocol protocol) method-identifier (sequence t) (message t))
    (error 'unknown-method-error :identifier method-identifier :request message)))


(defgeneric protocol-error (protocol type &optional message &rest arguments)
  (:method ((protocol protocol) type &optional message &rest arguments)
    (error 'protocol-error :type type :message message :message-arguments arguments)))


(defgeneric invalid-protocol-version (protocol id version)
  (:method ((protocol protocol) id version)
    (error 'protocol-version-error :protocol protocol :datum (cons id version)
           :expected-type (protocol-version protocol))))


(defgeneric invalid-sequence-number (protocol number expected-number)
  (:method ((protocol protocol) number expected-number)
    (error 'sequence-number-error :protocol protocol
           :number number :expected-number expected-number)))


(defgeneric invalid-struct-type (protocol type datum)
  (:method ((protocol protocol) type datum)
    (error 'struct-type-error :protocol protocol :expected-type type :datum datum)))


;;;
;;; response processing exception interface

(defgeneric response-exception (protocol message-name sequence-number exception)
  (:documentation "Called when an exception is read as a response. The base method signals an error.")

  (:method ((protocol protocol) (message-name t) (sequence-number t) (exception condition))
    "The base method signals an error."
    (error exception)))

(defgeneric request-exception (protocol message-name sequence-number exception)
  (:documentation "Called when an exception is read as a request. The base method signals an error.")

  (:method ((protocol protocol) (message-name t) (sequence-number t) (exception condition))
    "The base method signals an error."
    (error exception)))

(defgeneric unexpected-request (protocol message-name sequence-number exception)
  (:documentation "Called when a request is read out of context, eg. by a client. The base method signals an error.")

  (:method ((protocol protocol) (message-name t) (sequence-number t) (content t))
    "The base method signals an error."
    (error "Unexpected request: ~s ~s ~s ~s."
           protocol message-name sequence-number content)))

(defgeneric unexpected-response (protocol message-name sequence-number exception)
  (:documentation "Called when a response is read out of context, eg. by a server The base method signals an error.")

  (:method ((protocol protocol) (message-name t) (sequence-number t) (content t))
    "The base method signals an error."
    (error "Unexpected response: ~s ~s ~s ~s."
           protocol message-name sequence-number content)))
