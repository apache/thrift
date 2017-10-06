;; -*- mode: common-lisp; package: puri -*-
;; Support for URIs
;; For general URI information see RFC2396.
;;
;; copyright (c) 1999-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2005 Franz Inc, Oakland, CA - All rights reserved.
;; copyright (c) 2003-2010 Kevin Rosenberg
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by
;; the Free Software Foundation, as clarified by the
;; preamble found here:
;;     http://opensource.franz.com/preamble.html
;;
;; Versions ported from Franz's opensource release
;; uri.cl,v 2.3.6.4.2.1 2001/08/09 17:42:39 layer
;; uri.cl,v 2.9.84.1 2005/08/11 18:38:52 layer

;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; $Id$

(defpackage #:puri
  (:use #:cl)
  #-allegro (:nicknames #:net.uri)
  (:export
   #:uri                                ; the type and a function
   #:uri-p
   #:copy-uri

   #:uri-scheme                         ; and slots
   #:uri-host #:uri-port
   #:uri-path
   #:uri-query
   #:uri-fragment
   #:uri-plist
   #:uri-authority                      ; pseudo-slot accessor

   #:urn                                ; class
   #:urn-nid                            ; pseudo-slot accessor
   #:urn-nss                            ; pseudo-slot accessor

   #:*strict-parse*
   #:parse-uri
   #:merge-uris
   #:enough-uri
   #:uri-parsed-path
   #:render-uri

   #:make-uri-space                     ; interning...
   #:uri-space
   #:uri=
   #:intern-uri
   #:unintern-uri
   #:do-all-uris

   #:uri-parse-error ;; Added by KMR
   ))

(in-package #:puri)

(eval-when (:compile-toplevel) (declaim (optimize (speed 3))))


#-allegro
(defun parse-body (forms &optional env)
  "Parses a body, returns (VALUES docstring declarations forms)"
  (declare (ignore env))
  ;; fixme -- need to add parsing of multiple declarations
  (let (docstring declarations)
    (when (stringp (car forms))
      (setq docstring (car forms))
      (setq forms (cdr forms)))
    (when (and (listp (car forms))
               (symbolp (caar forms))
               (string-equal (symbol-name '#:declare)
                             (symbol-name (caar forms))))
      (setq declarations (car forms))
      (setq forms (cdr forms)))
    (values docstring declarations forms)))


(defun shrink-vector (str size)
  #+allegro
  (excl::.primcall 'sys::shrink-svector str size)
  #+sbcl
  (setq str (sb-kernel:shrink-vector str size))
  #+cmu
  (lisp::shrink-vector str size)
  #+lispworks
  (system::shrink-vector$vector str size)
  #+scl
  (common-lisp::shrink-vector str size)
  #-(or allegro cmu lispworks sbcl scl)
  (setq str (subseq str 0 size))
  str)


;; KMR: Added new condition to handle cross-implementation variances
;; in the parse-error condition many implementations define

(define-condition uri-parse-error (parse-error)
  ((fmt-control :initarg :fmt-control :accessor fmt-control)
   (fmt-arguments :initarg :fmt-arguments :accessor fmt-arguments ))
  (:report (lambda (c stream)
             (format stream "Parse error:")
             (apply #'format stream (fmt-control c) (fmt-arguments c)))))

(defun .parse-error (fmt &rest args)
  (error 'uri-parse-error :fmt-control fmt :fmt-arguments args))

#-allegro
(defun internal-reader-error (stream fmt &rest args)
  (apply #'format stream fmt args))

#-allegro (defvar *current-case-mode* :case-insensitive-upper)
#+allegro (eval-when (:compile-toplevel :load-toplevel :execute)
            (import '(excl:*current-case-mode*
                      excl:delimited-string-to-list
                      excl::parse-body
                      excl::internal-reader-error
                      excl:if*)))

#-allegro
(defmethod position-char (char (string string) start max)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (fixnum start max) (string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char= char (char string i)) (return i))))

#-allegro
(defun delimited-string-to-list (string &optional (separator #\space)
                                        skip-terminal)
  (declare (optimize (speed 3) (safety 0) (space 0)
                     (compilation-speed 0))
           (type string string)
           (type character separator))
  (do* ((len (length string))
        (output '())
        (pos 0)
        (end (position-char separator string pos len)
             (position-char separator string pos len)))
       ((null end)
        (if (< pos len)
            (push (subseq string pos) output)
          (when (and (plusp len) (not skip-terminal))
            (push "" output)))
        (nreverse output))
    (declare (type fixnum pos len)
             (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

  (defmacro if* (&rest args)
    (do ((xx (reverse args) (cdr xx))
         (state :init)
         (elseseen nil)
         (totalcol nil)
        (lookat nil nil)
         (col nil))
        ((null xx)
         (cond ((eq state :compl)
                `(cond ,@totalcol))
               (t (error "if*: illegal form ~s" args))))
      (cond ((and (symbolp (car xx))
                  (member (symbol-name (car xx))
                          if*-keyword-list
                          :test #'string-equal))
             (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
              (cond (lookat (cond ((string-equal lookat "thenret")
                                   (setq col nil
                                         state :then))
                                  (t (error
                                      "if*: bad keyword ~a" lookat))))
                    (t (setq state :col
                             col nil)
                       (push (car xx) col))))
             ((eq state :col)
              (cond (lookat
                     (cond ((string-equal lookat "else")
                            (cond (elseseen
                                   (error
                                    "if*: multiples elses")))
                            (setq elseseen t)
                            (setq state :init)
                            (push `(t ,@col) totalcol))
                           ((string-equal lookat "then")
                            (setq state :then))
                           (t (error "if*: bad keyword ~s"
                                              lookat))))
                    (t (push (car xx) col))))
             ((eq state :then)
              (cond (lookat
                     (error
                      "if*: keyword ~s at the wrong place " (car xx)))
                    (t (setq state :compl)
                       (push `(,(car xx) ,@col) totalcol))))
             ((eq state :compl)
              (cond ((not (string-equal lookat "elseif"))
                     (error "if*: missing elseif clause ")))
              (setq state :init))))))


(defclass uri ()
  (
;;;; external:
   (scheme :initarg :scheme :initform nil :accessor uri-scheme)
   (host :initarg :host :initform nil :accessor uri-host)
   (port :initarg :port :initform nil :accessor uri-port)
   (path :initarg :path :initform nil :accessor uri-path)
   (query :initarg :query :initform nil :accessor uri-query)
   (fragment :initarg :fragment :initform nil :accessor uri-fragment)
   (plist :initarg :plist :initform nil :accessor uri-plist)

;;;; internal:
   (escaped
    ;; used to prevent unnessary work, looking for chars to escape and
    ;; unescape.
    :initarg :escaped :initform nil :accessor uri-escaped)
   (string
    ;; the cached printable representation of the URI.  It *might* be
    ;; different than the original string, though, because the user might
    ;; have escaped non-reserved chars--they won't be escaped when the URI
    ;; is printed.
    :initarg :string :initform nil :accessor uri-string)
   (parsed-path
    ;; the cached parsed representation of the URI path.
    :initarg :parsed-path
    :initform nil
    :accessor .uri-parsed-path)
   (hashcode
    ;; cached sxhash, so we don't have to compute it more than once.
    :initarg :hashcode :initform nil :accessor uri-hashcode)))

(defclass urn (uri)
  ((nid :initarg :nid :initform nil :accessor urn-nid)
   (nss :initarg :nss :initform nil :accessor urn-nss)))

(eval-when (:compile-toplevel :execute)
  (defmacro clear-caching-on-slot-change (name)
    `(defmethod (setf ,name) :around (new-value (self uri))
       (declare (ignore new-value))
       (prog1 (call-next-method)
         (setf (uri-string self) nil)
         ,@(when (eq name 'uri-path) `((setf (.uri-parsed-path self) nil)))
         (setf (uri-hashcode self) nil))))
  )

(clear-caching-on-slot-change uri-scheme)
(clear-caching-on-slot-change uri-host)
(clear-caching-on-slot-change uri-port)
(clear-caching-on-slot-change uri-path)
(clear-caching-on-slot-change uri-query)
(clear-caching-on-slot-change uri-fragment)


(defmethod make-load-form ((self uri) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(uri-host self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)
     :fragment ,(uri-fragment self)
     :plist ',(uri-plist self)
     :string ,(uri-string self)
     :parsed-path ',(.uri-parsed-path self)))

(defmethod uri-p ((thing uri)) t)
(defmethod uri-p ((thing t)) nil)

(defun copy-uri (uri
                 &key place
                      (scheme (when uri (uri-scheme uri)))
                      (host (when uri (uri-host uri)))
                      (port (when uri (uri-port uri)))
                      (path (when uri (uri-path uri)))
                      (parsed-path
                       (when uri (copy-list (.uri-parsed-path uri))))
                      (query (when uri (uri-query uri)))
                      (fragment (when uri (uri-fragment uri)))
                      (plist (when uri (copy-list (uri-plist uri))))
                      (class (when uri (class-of uri)))
                 &aux (escaped (when uri (uri-escaped uri))))
  (if* place
     then (setf (uri-scheme place) scheme)
          (setf (uri-host place) host)
          (setf (uri-port place) port)
          (setf (uri-path place) path)
          (setf (.uri-parsed-path place) parsed-path)
          (setf (uri-query place) query)
          (setf (uri-fragment place) fragment)
          (setf (uri-plist place) plist)
          (setf (uri-escaped place) escaped)
          (setf (uri-string place) nil)
          (setf (uri-hashcode place) nil)
          place
   elseif (eq 'uri class)
     then ;; allow the compiler to optimize the call to make-instance:
          (make-instance 'uri
            :scheme scheme :host host :port port :path path
            :parsed-path parsed-path
            :query query :fragment fragment :plist plist
            :escaped escaped :string nil :hashcode nil)
     else (make-instance class
            :scheme scheme :host host :port port :path path
            :parsed-path parsed-path
            :query query :fragment fragment :plist plist
            :escaped escaped :string nil :hashcode nil)))

(defmethod uri-parsed-path ((uri uri))
  (when (uri-path uri)
    (when (null (.uri-parsed-path uri))
      (setf (.uri-parsed-path uri)
        (parse-path (uri-path uri) (uri-escaped uri))))
    (.uri-parsed-path uri)))

(defmethod (setf uri-parsed-path) (path-list (uri uri))
  (assert (and (consp path-list)
               (or (member (car path-list) '(:absolute :relative)
                           :test #'eq))))
  (setf (uri-path uri) (render-parsed-path path-list t))
  (setf (.uri-parsed-path uri) path-list)
  path-list)

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((*print-pretty* nil))
      (format nil "~a~@[:~a~]" (uri-host uri) (uri-port uri)))))

(defun uri-nid (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then (uri-host uri)
     else (error "URI is not a URN: ~s." uri)))

(defun uri-nss (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then (uri-path uri)
     else (error "URI is not a URN: ~s." uri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defparameter *excluded-characters*
    (append
     ;; exclude control characters
     (loop for i from 0 to #x1f
	   collect (code-char i))
     '(;; `delims' (except #\%, because it's handled specially):
      #\< #\> #\" #\space #\#
      #\Rubout ;; (code-char #x7f)
      ;; `unwise':
      #\{ #\} #\| #\\ #\^ #\[ #\] #\`))
  "Excluded charcters from RFC2369 (http://www.ietf.org/rfc/rfc2396.txt 2.4.3)")

(defun reserved-char-vector (chars &key except)
  (do* ((a (make-array 128 :element-type 'bit :initial-element 0))
        (chars chars (cdr chars))
        (c (car chars) (car chars)))
      ((null chars) a)
    (if* (and except (member c except :test #'char=))
       thenret
       else (setf (sbit a (char-int c)) 1))))

(defparameter *reserved-characters*
    (reserved-char-vector
     (append *excluded-characters*
             '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\, #\%))))
(defparameter *reserved-authority-characters*
    (reserved-char-vector
     (append *excluded-characters* '(#\; #\/ #\? #\: #\@))))
(defparameter *reserved-path-characters*
    (reserved-char-vector
     (append *excluded-characters*
             '(#\; #\%
;;;;The rfc says this should be here, but it doesn't make sense.
               ;; #\=
               #\/ #\?))))

(defparameter *reserved-fragment-characters*
    (reserved-char-vector (remove #\# *excluded-characters*)))

(eval-when (:compile-toplevel :execute)
(defun gen-char-range-list (start end)
  (do* ((res '())
        (endcode (1+ (char-int end)))
        (chcode (char-int start)
                (1+ chcode))
        (hyphen nil))
      ((= chcode endcode)
       ;; - has to be first, otherwise it signifies a range!
       (if* hyphen
          then (setq res (nreverse res))
               (push #\- res)
               res
          else (nreverse res)))
    (if* (= #.(char-int #\-) chcode)
       then (setq hyphen t)
       else (push (code-char chcode) res))))
)

(defparameter *valid-nid-characters*
    (reserved-char-vector
     '#.(nconc (gen-char-range-list #\a #\z)
               (gen-char-range-list #\A #\Z)
               (gen-char-range-list #\0 #\9)
               '(#\- #\. #\+))))
(defparameter *reserved-nss-characters*
    (reserved-char-vector
     (append *excluded-characters* '(#\& #\~ #\/ #\?))))

(defparameter *illegal-characters*
    (reserved-char-vector (remove #\# *excluded-characters*)))
(defparameter *strict-illegal-query-characters*
    (reserved-char-vector (append '(#\?) (remove #\# *excluded-characters*))))
(defparameter *illegal-query-characters*
    (reserved-char-vector
     *excluded-characters* :except '(#\^ #\| #\#)))


(defun parse-uri (thing &key (class 'uri) &aux escape)
  (when (uri-p thing) (return-from parse-uri thing))

  (setq escape (escape-p thing))
  (multiple-value-bind (scheme host port path query fragment)
      (parse-uri-string thing)
    (when scheme
      (setq scheme
        (intern (funcall
                 (case *current-case-mode*
                   ((:case-insensitive-upper :case-sensitive-upper)
                    #'string-upcase)
                   ((:case-insensitive-lower :case-sensitive-lower)
                    #'string-downcase))
                 (decode-escaped-encoding scheme escape))
                (find-package :keyword))))

    (when (and scheme (eq :urn scheme))
      (return-from parse-uri
        (make-instance 'urn :scheme scheme :nid host :nss path)))

    (when host (setq host (decode-escaped-encoding host escape)))
    (when port
      (setq port (read-from-string port))
      (when (not (numberp port)) (error "port is not a number: ~s." port))
      (when (not (plusp port))
        (error "port is not a positive integer: ~d." port))
      (when (eql port (case scheme
                      (:http 80)
                      (:https 443)
                      (:ftp 21)
                      (:telnet 23)))
        (setq port nil)))
    (when (or (string= "" path)
              (and ;; we canonicalize away a reference to just /:
               scheme
               (member scheme '(:http :https :ftp) :test #'eq)
               (string= "/" path)))
      (setq path nil))
    (when path
      (setq path
        (decode-escaped-encoding path escape *reserved-path-characters*)))
    (when query (setq query (decode-escaped-encoding query escape)))
    (when fragment
      (setq fragment
        (decode-escaped-encoding fragment escape
                                 *reserved-fragment-characters*)))
    (if* (eq 'uri class)
       then ;; allow the compiler to optimize the make-instance call:
            (make-instance 'uri
              :scheme scheme
              :host host
              :port port
              :path path
              :query query
              :fragment fragment
              :escaped escape)
       else ;; do it the slow way:
            (make-instance class
              :scheme scheme
              :host host
              :port port
              :path path
              :query query
              :fragment fragment
              :escaped escape))))

(defmethod uri ((thing uri))
  thing)

(defmethod uri ((thing string))
  (parse-uri thing))

(defmethod uri ((thing t))
  (error "Cannot coerce ~s to a uri." thing))

(defvar *strict-parse* t)

(defun parse-uri-string (string &aux (illegal-chars *illegal-characters*))
  (declare (optimize (speed 3)))
  ;; Speed is important, so use a specialized state machine instead of
  ;; regular expressions for parsing the URI string. The regexp we are
  ;; simulating:
  ;;  ^(([^:/?#]+):)?
  ;;   (//([^/?#]*))?
  ;;   ([^?#]*)
  ;;   (\?([^#]*))?
  ;;   (#(.*))?
  (let* ((state 0)
         (start 0)
         (end (length string))
         (tokval nil)
         (scheme nil)
         (host nil)
         (port nil)
         (path-components '())
         (query nil)
         (fragment nil)
         ;; namespace identifier, for urn parsing only:
         (nid nil))
    (declare (fixnum state start end))
    (flet ((read-token (kind &optional legal-chars)
             (setq tokval nil)
             (if* (>= start end)
                then :end
                else (let ((sindex start)
                           (res nil)
                           c)
                       (declare (fixnum sindex))
                       (setq res
                         (loop
                           (when (>= start end) (return nil))
                           (setq c (char string start))
                           (let ((ci (char-int c)))
                             (if* legal-chars
                                then (if* (and (eq :colon kind) (eq c #\:))
                                        then (return :colon)
                                      elseif (= 0 (sbit legal-chars ci))
                                        then (.parse-error
                                              "~
URI ~s contains illegal character ~s at position ~d."
                                              string c start))
                              elseif (and (< ci 128)
                                          *strict-parse*
                                          (= 1 (sbit illegal-chars ci)))
                                then (.parse-error "~
URI ~s contains illegal character ~s at position ~d."
                                                         string c start)))
                           (case kind
                             (:path (case c
                                      (#\? (return :question))
                                      (#\# (return :hash))))
                             (:query (case c (#\# (return :hash))))
                             (:rest)
                             (t (case c
                                  (#\: (return :colon))
                                  (#\? (return :question))
                                  (#\# (return :hash))
                                  (#\/ (return :slash)))))
                           (incf start)))
                       (if* (> start sindex)
                          then ;; we found some chars
                               ;; before we stopped the parse
                               (setq tokval (subseq string sindex start))
                               :string
                          else ;; immediately stopped at a special char
                               (incf start)
                               res))))
           (failure (&optional why)
             (.parse-error "illegal URI: ~s [~d]~@[: ~a~]"
                                 string state why))
           (impossible ()
             (.parse-error "impossible state: ~d [~s]" state string)))
      (loop
        (case state
          (0 ;; starting to parse
           (ecase (read-token t)
             (:colon (failure))
             (:question (setq state 7))
             (:hash (setq state 8))
             (:slash (setq state 3))
             (:string (setq state 1))
             (:end (setq state 9))))
          (1 ;; seen <token><special char>
           (let ((token tokval))
             (ecase (read-token t)
               (:colon (setq scheme token)
                       (if* (equalp "urn" scheme)
                          then (setq state 15)
                          else (setq state 2)))
               (:question (push token path-components)
                          (setq state 7))
               (:hash (push token path-components)
                      (setq state 8))
               (:slash (push token path-components)
                       (push "/" path-components)
                       (setq state 6))
               (:string (failure))
               (:end (push token path-components)
                     (setq state 9)))))
          (2 ;; seen <scheme>:
           (ecase (read-token t)
             (:colon (failure))
             (:question (setq state 7))
             (:hash (setq state 8))
             (:slash (setq state 3))
             (:string (setq state 10))
             (:end (setq state 9))))
          (10 ;; seen <scheme>:<token>
           (let ((token tokval))
             (ecase (read-token t)
               (:colon (failure))
               (:question (push token path-components)
                          (setq state 7))
               (:hash (push token path-components)
                      (setq state 8))
               (:slash (push token path-components)
                       (setq state 6))
               (:string (failure))
               (:end (push token path-components)
                     (setq state 9)))))
          (3 ;; seen / or <scheme>:/
           (ecase (read-token t)
             (:colon (failure))
             (:question (push "/" path-components)
                        (setq state 7))
             (:hash (push "/" path-components)
                    (setq state 8))
             (:slash (setq state 4))
             (:string (push "/" path-components)
                      (push tokval path-components)
                      (setq state 6))
             (:end (push "/" path-components)
                   (setq state 9))))
          (4 ;; seen [<scheme>:]//
           (ecase (read-token t)
             (:colon (failure))
             (:question (failure))
             (:hash (failure))
             (:slash
              (if* (and (equalp "file" scheme)
                        (null host))
                 then ;; file:///...
                      (push "/" path-components)
                      (setq state 6)
                 else (failure)))
             (:string (setq host tokval)
                      (setq state 11))
             (:end (failure))))
          (11 ;; seen [<scheme>:]//<host>
           (ecase (read-token t)
             (:colon (setq state 5))
             (:question (setq state 7))
             (:hash (setq state 8))
             (:slash (push "/" path-components)
                     (setq state 6))
             (:string (impossible))
             (:end (setq state 9))))
          (5 ;; seen [<scheme>:]//<host>:
           (ecase (read-token t)
             (:colon (failure))
             (:question (failure))
             (:hash (failure))
             (:slash (push "/" path-components)
                     (setq state 6))
             (:string (setq port tokval)
                      (setq state 12))
             (:end (failure))))
          (12 ;; seen [<scheme>:]//<host>:[<port>]
           (ecase (read-token t)
             (:colon (failure))
             (:question (setq state 7))
             (:hash (setq state 8))
             (:slash (push "/" path-components)
                     (setq state 6))
             (:string (impossible))
             (:end (setq state 9))))
          (6 ;; seen /
           (ecase (read-token :path)
             (:question (setq state 7))
             (:hash (setq state 8))
             (:string (push tokval path-components)
                      (setq state 13))
             (:end (setq state 9))))
          (13 ;; seen path
           (ecase (read-token :path)
             (:question (setq state 7))
             (:hash (setq state 8))
             (:string (impossible))
             (:end (setq state 9))))
          (7 ;; seen ?
           (setq illegal-chars
             (if* *strict-parse*
                then *strict-illegal-query-characters*
                else *illegal-query-characters*))
           (ecase (prog1 (read-token :query)
                    (setq illegal-chars *illegal-characters*))
             (:hash (setq state 8))
             (:string (setq query tokval)
                      (setq state 14))
             (:end (setq state 9))))
          (14 ;; query
           (ecase (read-token :query)
             (:hash (setq state 8))
             (:string (impossible))
             (:end (setq state 9))))
          (8 ;; seen #
           (ecase (read-token :rest)
             (:string (setq fragment tokval)
                      (setq state 9))
             (:end (setq state 9))))
          (9 ;; done
           (return
             (values
              scheme host port
              (apply #'concatenate 'string (nreverse path-components))
              query fragment)))
          ;; URN parsing:
          (15 ;; seen urn:, read nid now
           (case (read-token :colon *valid-nid-characters*)
             (:string (setq nid tokval)
                      (setq state 16))
             (t (failure "missing namespace identifier"))))
          (16 ;; seen urn:<nid>
           (case (read-token t)
             (:colon (setq state 17))
             (t (failure "missing namespace specific string"))))
          (17 ;; seen urn:<nid>:, rest is nss
           (return (values scheme
                           nid
                           nil
                           (progn
                             (setq illegal-chars *reserved-nss-characters*)
                             (read-token :rest)
                             tokval))))
          (t (.parse-error
              "internal error in parse engine, wrong state: ~s." state)))))))

(defun escape-p (string)
  (declare (optimize (speed 3)))
  (do* ((i 0 (1+ i))
        (max (the fixnum (length string))))
      ((= i max) nil)
    (declare (fixnum i max))
    (when (char= #\% (char string i))
      (return t))))

(defun parse-path (path-string escape)
  (do* ((xpath-list (delimited-string-to-list path-string #\/))
        (path-list
         (progn
           (if* (string= "" (car xpath-list))
              then (setf (car xpath-list) :absolute)
              else (push :relative xpath-list))
           xpath-list))
        (pl (cdr path-list) (cdr pl))
        segments)
      ((null pl) path-list)

    (if* (cdr (setq segments
                (if* (string= "" (car pl))
                   then '("")
                   else (delimited-string-to-list (car pl) #\;))))
       then ;; there is a param
            (setf (car pl)
              (mapcar #'(lambda (s)
                          (decode-escaped-encoding s escape
                                                   ;; decode all %xx:
                                                   nil))
                      segments))
       else ;; no param
            (setf (car pl)
              (decode-escaped-encoding (car segments) escape
                                       ;; decode all %xx:
                                       nil)))))

(defun decode-escaped-encoding (string escape
                                &optional (reserved-chars
                                           *reserved-characters*))
  ;; Return a string with the real characters.
  (when (null escape) (return-from decode-escaped-encoding string))
  (do* ((i 0 (1+ i))
        (max (length string))
        (new-string (copy-seq string))
        (new-i 0 (1+ new-i))
        ch ch2 chc chc2)
      ((= i max)
       (shrink-vector new-string new-i))
    (if* (char= #\% (setq ch (char string i)))
       then (when (> (+ i 3) max)
              (.parse-error
               "Unsyntactic escaped encoding in ~s." string))
            (setq ch (char string (incf i)))
            (setq ch2 (char string (incf i)))
            (when (not (and (setq chc (digit-char-p ch 16))
                            (setq chc2 (digit-char-p ch2 16))))
              (.parse-error
               "Non-hexidecimal digits after %: %c%c." ch ch2))
            (let ((ci (+ (* 16 chc) chc2)))
              (if* (or (null reserved-chars)
                       (> ci 127)       ; bug11527
                       (= 0 (sbit reserved-chars ci)))
                 then ;; ok as is
                      (setf (char new-string new-i)
                        (code-char ci))
                 else (setf (char new-string new-i) #\%)
                      (setf (char new-string (incf new-i)) ch)
                      (setf (char new-string (incf new-i)) ch2)))
       else (setf (char new-string new-i) ch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Printing

(defun render-uri (uri stream
                   &aux (escape (uri-escaped uri))
                        (*print-pretty* nil))
  (when (null (uri-string uri))
    (setf (uri-string uri)
      (let ((scheme (uri-scheme uri))
            (host (uri-host uri))
            (port (uri-port uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri)))
        (concatenate 'string
          (when scheme
            (encode-escaped-encoding
             (string-downcase ;; for upper case lisps
              (symbol-name scheme))
             *reserved-characters* escape))
          (when scheme ":")
          (when (or host (eq :file scheme)) "//")
          (when host
            (encode-escaped-encoding
             host *reserved-authority-characters* escape))
          (when port ":")
          (when port
            #-allegro (format nil "~D" port)
            #+allegro (with-output-to-string (s)
                        (excl::maybe-print-fast s port))
            )
          (encode-escaped-encoding (or path "/")
                                   nil
                                   ;;*reserved-path-characters*
                                   escape)
          (when query "?")
          (when query (encode-escaped-encoding query nil escape))
          (when fragment "#")
          (when fragment (encode-escaped-encoding fragment nil escape))))))
  (if* stream
     then (format stream "~a" (uri-string uri))
     else (uri-string uri)))

(defun render-parsed-path (path-list escape)
  (do* ((res '())
        (first (car path-list))
        (pl (cdr path-list) (cdr pl))
        (pe (car pl) (car pl)))
      ((null pl)
       (when res (apply #'concatenate 'string (nreverse res))))
    (when (or (null first)
              (prog1 (eq :absolute first)
                (setq first nil)))
      (push "/" res))
    (if* (atom pe)
       then (push
             (encode-escaped-encoding pe *reserved-path-characters* escape)
             res)
       else ;; contains params
            (push (encode-escaped-encoding
                   (car pe) *reserved-path-characters* escape)
                  res)
            (dolist (item (cdr pe))
              (push ";" res)
              (push (encode-escaped-encoding
                     item *reserved-path-characters* escape)
                    res)))))

(defun render-urn (urn stream
                   &aux (*print-pretty* nil))
  (when (null (uri-string urn))
    (setf (uri-string urn)
      (let ((nid (urn-nid urn))
            (nss (urn-nss urn)))
        (concatenate 'string "urn:" nid ":" nss))))
  (if* stream
     then (format stream "~a" (uri-string urn))
     else (uri-string urn)))

(defparameter *escaped-encoding*
    (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

(defun encode-escaped-encoding (string reserved-chars escape)
  (when (null escape) (return-from encode-escaped-encoding string))
  ;; Make a string as big as it possibly needs to be (3 times the original
  ;; size), and truncate it at the end.
  (do* ((max (length string))
        (new-max (* 3 max)) ;; worst case new size
        (new-string (make-string new-max))
        (i 0 (1+ i))
        (new-i -1)
        c ci)
      ((= i max)
       (shrink-vector new-string (incf new-i)))
    (setq ci (char-int (setq c (char string i))))
    (if* (or (null reserved-chars)
             (> ci 127)
             (= 0 (sbit reserved-chars ci)))
       then ;; ok as is
            (incf new-i)
            (setf (char new-string new-i) c)
       else ;; need to escape it
            (multiple-value-bind (q r) (truncate ci 16)
              (setf (char new-string (incf new-i)) #\%)
              (setf (char new-string (incf new-i)) (elt *escaped-encoding* q))
              (setf (char new-string (incf new-i))
                (elt *escaped-encoding* r))))))

(defmethod print-object ((uri uri) stream)
  (if* *print-escape*
     then (print-unreadable-object (uri stream :type t) (render-uri uri stream))
     else (render-uri uri stream)))

(defmethod print-object ((urn urn) stream)
  (if* *print-escape*
     then (print-unreadable-object (urn stream :type t) (render-urn urn stream))
     else (render-urn urn stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merging and unmerging

(defmethod merge-uris ((uri string) (base string) &optional place)
  (merge-uris (parse-uri uri) (parse-uri base) place))

(defmethod merge-uris ((uri uri) (base string) &optional place)
  (merge-uris uri (parse-uri base) place))

(defmethod merge-uris ((uri string) (base uri) &optional place)
  (merge-uris (parse-uri uri) base place))


(defmethod merge-uris ((uri uri) (base uri) &optional place)
  ;; See ../doc/rfc2396.txt for info on the algorithm we use to merge
  ;; URIs.
  ;;
  (tagbody
;;;; step 2
    (when (and (null (uri-parsed-path uri))
               (null (uri-scheme uri))
               (null (uri-host uri))
               (null (uri-port uri))
               (null (uri-query uri)))
      (return-from merge-uris
        (let ((new (copy-uri base :place place)))
          (when (uri-query uri)
            (setf (uri-query new) (uri-query uri)))
          (when (uri-fragment uri)
            (setf (uri-fragment new) (uri-fragment uri)))
          new)))

    (setq uri (copy-uri uri :place place))

;;;; step 3
    (when (uri-scheme uri)
      (return-from merge-uris uri))
    (setf (uri-scheme uri) (uri-scheme base))

;;;; step 4
    (when (uri-host uri) (go :done))
    (setf (uri-host uri) (uri-host base))
    (setf (uri-port uri) (uri-port base))

;;;; step 5
    (let ((p (uri-parsed-path uri)))

      ;; bug13133:
      ;; The following form causes our implementation to be at odds with
      ;; RFC 2396, however this is apparently what was intended by the
      ;; authors of the RFC.  Specifically, (merge-uris "?y" "/foo")
      ;; should return #<uri /foo?y> instead of #<uri ?y>, according to
      ;; this:
;;; http://www.apache.org/~fielding/uri/rev-2002/issues.html#003-relative-query
      (when (null p)
        (setf (uri-path uri) (uri-path base))
        (go :done))

      (when (and p (eq :absolute (car p)))
        (when (equal '(:absolute "") p)
          ;; Canonicalize the way parsing does:
          (setf (uri-path uri) nil))
        (go :done)))

;;;; step 6
    (let* ((base-path
            (or (uri-parsed-path base)
                ;; needed because we canonicalize away a path of just `/':
                '(:absolute "")))
           (path (uri-parsed-path uri))
           new-path-list)
      (when (not (eq :absolute (car base-path)))
        (error "Cannot merge ~a and ~a, since latter is not absolute."
               uri base))

      ;; steps 6a and 6b:
      (setq new-path-list
        (append (butlast base-path)
                (if* path then (cdr path) else '(""))))

      ;; steps 6c and 6d:
      (let ((last (last new-path-list)))
        (if* (atom (car last))
           then (when (string= "." (car last))
                  (setf (car last) ""))
           else (when (string= "." (caar last))
                  (setf (caar last) ""))))
      (setq new-path-list
        (delete "." new-path-list :test #'(lambda (a b)
                                            (if* (atom b)
                                               then (string= a b)
                                               else nil))))

      ;; steps 6e and 6f:
      (let ((npl (cdr new-path-list))
            index tmp fix-tail)
        (setq fix-tail
          (string= ".." (let ((l (car (last npl))))
                          (if* (atom l)
                             then l
                             else (car l)))))
        (loop
          (setq index
            (position ".." npl
                      :test #'(lambda (a b)
                                (string= a
                                         (if* (atom b)
                                            then b
                                            else (car b))))))
          (when (null index) (return))
          (when (= 0 index)
            ;; The RFC says, in 6g, "that the implementation may handle
            ;; this error by retaining these components in the resolved
            ;; path, by removing them from the resolved path, or by
            ;; avoiding traversal of the reference."  The examples in C.2
            ;; imply that we should do the first thing (retain them), so
            ;; that's what we'll do.
            (return))
          (if* (= 1 index)
             then (setq npl (cddr npl))
             else (setq tmp npl)
                  (dotimes (x (- index 2)) (setq tmp (cdr tmp)))
                  (setf (cdr tmp) (cdddr tmp))))
        (setf (cdr new-path-list) npl)
        (when fix-tail (setq new-path-list (nconc new-path-list '("")))))

      ;; step 6g:
      ;; don't complain if new-path-list starts with `..'.  See comment
      ;; above about this step.

      ;; step 6h:
      (when (or (equal '(:absolute "") new-path-list)
                (equal '(:absolute) new-path-list))
        (setq new-path-list nil))
      (setf (uri-path uri)
        (render-parsed-path new-path-list
                            ;; don't know, so have to assume:
                            t)))

;;;; step 7
   :done
    (return-from merge-uris uri)))

(defmethod enough-uri ((uri string) (base string) &optional place)
  (enough-uri (parse-uri uri) (parse-uri base) place))

(defmethod enough-uri ((uri uri) (base string) &optional place)
  (enough-uri uri (parse-uri base) place))

(defmethod enough-uri ((uri string) (base uri) &optional place)
  (enough-uri (parse-uri uri) base place))

(defmethod enough-uri ((uri uri) (base uri) &optional place)
  (let ((new-scheme nil)
        (new-host nil)
        (new-port nil)
        (new-parsed-path nil))

    (when (or (and (uri-scheme uri)
                   (not (equalp (uri-scheme uri) (uri-scheme base))))
              (and (uri-host uri)
                   (not (equalp (uri-host uri) (uri-host base))))
              (not (equalp (uri-port uri) (uri-port base))))
      (return-from enough-uri uri))

    (when (null (uri-host uri))
      (setq new-host (uri-host base)))
    (when (null (uri-port uri))
      (setq new-port (uri-port base)))

    (when (null (uri-scheme uri))
      (setq new-scheme (uri-scheme base)))

    ;; Now, for the hard one, path.
    ;; We essentially do here what enough-namestring does.
    (do* ((base-path (uri-parsed-path base))
          (path (uri-parsed-path uri))
          (bp base-path (cdr bp))
          (p path (cdr p)))
        ((or (null bp) (null p))
         ;; If p is nil, that means we have something like
         ;; (enough-uri "/foo/bar" "/foo/bar/baz.htm"), so
         ;; new-parsed-path will be nil.
         (when (null bp)
           (setq new-parsed-path (copy-list p))
           (when (not (symbolp (car new-parsed-path)))
             (push :relative new-parsed-path))))
      (if* (equal (car bp) (car p))
         thenret ;; skip it
         else (setq new-parsed-path (copy-list p))
              (when (not (symbolp (car new-parsed-path)))
                (push :relative new-parsed-path))
              (return)))

    (let ((new-path
           (when new-parsed-path
             (render-parsed-path new-parsed-path
                                 ;; don't know, so have to assume:
                                 t)))
          (new-query (uri-query uri))
          (new-fragment (uri-fragment uri))
          (new-plist (copy-list (uri-plist uri))))
      (if* (and (null new-scheme)
                (null new-host)
                (null new-port)
                (null new-path)
                (null new-parsed-path)
                (null new-query)
                (null new-fragment))
         then ;; can't have a completely empty uri!
              (copy-uri nil
                        :class (class-of uri)
                        :place place
                        :path "/"
                        :plist new-plist)
         else (copy-uri nil
                        :class (class-of uri)
                        :place place
                        :scheme new-scheme
                        :host new-host
                        :port new-port
                        :path new-path
                        :parsed-path new-parsed-path
                        :query new-query
                        :fragment new-fragment
                        :plist new-plist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for interning URIs

(defun make-uri-space (&rest keys &key (size 777) &allow-other-keys)
  #+allegro
  (apply #'make-hash-table :size size
         :hash-function 'uri-hash
         :test 'uri= :values nil keys)
  #-allegro
  (apply #'make-hash-table :size size keys))

(defun gethash-uri (uri table)
  #+allegro (gethash uri table)
  #-allegro
  (let* ((hash (uri-hash uri))
         (existing (gethash hash table)))
    (dolist (u existing)
      (when (uri= u uri)
        (return-from gethash-uri (values u t))))
    (values nil nil)))

(defun puthash-uri (uri table)
  #+allegro (excl:puthash-key uri table)
  #-allegro
  (let ((existing (gethash (uri-hash uri) table)))
    (dolist (u existing)
      (when (uri= u uri)
        (return-from puthash-uri u)))
    (setf (gethash (uri-hash uri) table)
      (cons uri existing))
    uri))


(defun uri-hash (uri)
  (if* (uri-hashcode uri)
     thenret
     else (setf (uri-hashcode uri)
                (sxhash
                 #+allegro
                 (render-uri uri nil)
                 #-allegro
                 (string-downcase
                  (render-uri uri nil))))))

(defvar *uris* (make-uri-space))

(defun uri-space () *uris*)

(defun (setf uri-space) (new-val)
  (setq *uris* new-val))

;; bootstrapping (uri= changed from function to method):
(when (fboundp 'uri=) (fmakunbound 'uri=))

(defgeneric uri= (uri1 uri2))
(defmethod uri= ((uri1 uri) (uri2 uri))
  (when (not (eq (uri-scheme uri1) (uri-scheme uri2)))
    (return-from uri= nil))
  ;; RFC2396 says: a URL with an explicit ":port", where the port is
  ;; the default for the scheme, is the equivalent to one where the
  ;; port is elided.  Hmmmm.  This means that this function has to be
  ;; scheme dependent.  Grrrr.
  (let ((default-port (case (uri-scheme uri1)
                        (:http 80)
                        (:https 443)
                        (:ftp 21)
                        (:telnet 23))))
    (and (equalp (uri-host uri1) (uri-host uri2))
         (eql (or (uri-port uri1) default-port)
              (or (uri-port uri2) default-port))
         (string= (uri-path uri1) (uri-path uri2))
         (string= (uri-query uri1) (uri-query uri2))
         (string= (uri-fragment uri1) (uri-fragment uri2)))))

(defmethod uri= ((urn1 urn) (urn2 urn))
  (when (not (eq (uri-scheme urn1) (uri-scheme urn2)))
    (return-from uri= nil))
  (and (equalp (urn-nid urn1) (urn-nid urn2))
       (urn-nss-equal (urn-nss urn1) (urn-nss urn2))))

(defun urn-nss-equal (nss1 nss2 &aux len)
  ;; Return t iff the nss values are the same.
  ;; %2c and %2C are equivalent.
  (when (or (null nss1) (null nss2)
            (not (= (setq len (length nss1))
                    (length nss2))))
    (return-from urn-nss-equal nil))
  (do* ((i 0 (1+ i))
        (state :char)
        c1 c2)
      ((= i len) t)
    (setq c1 (char nss1 i))
    (setq c2 (char nss2 i))
    (ecase state
      (:char
       (if* (and (char= #\% c1) (char= #\% c2))
          then (setq state :percent+1)
        elseif (char/= c1 c2)
          then (return nil)))
      (:percent+1
       (when (char-not-equal c1 c2) (return nil))
       (setq state :percent+2))
      (:percent+2
       (when (char-not-equal c1 c2) (return nil))
       (setq state :char)))))

(defmethod intern-uri ((xuri uri) &optional (uri-space *uris*))
  (let ((uri (gethash-uri xuri uri-space)))
    (if* uri
       thenret
       else (puthash-uri xuri uri-space))))

(defmethod intern-uri ((uri string) &optional (uri-space *uris*))
  (intern-uri (parse-uri uri) uri-space))

(defun unintern-uri (uri &optional (uri-space *uris*))
  (if* (eq t uri)
     then (clrhash uri-space)
   elseif (uri-p uri)
     then (remhash uri uri-space)
     else (error "bad uri: ~s." uri)))

(defmacro do-all-uris ((var &optional uri-space result-form)
                       &rest forms
                       &environment env)
  "do-all-uris (var [[uri-space] result-form])
                    {declaration}* {tag | statement}*
Executes the forms once for each uri with var bound to the current uri"
  (let ((f (gensym))
        (g-ignore (gensym))
        (g-uri-space (gensym))
        (body (third (parse-body forms env))))
    `(let ((,g-uri-space (or ,uri-space *uris*)))
       (prog nil
         (flet ((,f (,var &optional ,g-ignore)
                  (declare (ignore-if-unused ,var ,g-ignore))
                  (tagbody ,@body)))
           (maphash #',f ,g-uri-space))
         (return ,result-form)))))

(defun sharp-u (stream chr arg)
  (declare (ignore chr arg))
  (let ((arg (read stream nil nil t)))
    (if *read-suppress*
        nil
      (if* (stringp arg)
         then (parse-uri arg)
         else

         (internal-reader-error
          stream
          "#u takes a string or list argument: ~s" arg)))))


(set-dispatch-macro-character #\# #\u #'puri::sharp-u)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :uri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timings
;; (don't run under emacs with M-x fi:common-lisp)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'excl::gc))

#-allegro
(defun gc (&rest options)
  (declare (ignore options))
  #+sbcl (sb-ext::gc)
  #+cmu (ext::gc)
  )

(defun time-uri-module ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((uri "http://www.franz.com/a/b;x;y;z/c/foo?bar=baz&xxx#foo")
        (uri2 "http://www.franz.com/a/b;x;y;z/c/%2ffoo?bar=baz&xxx#foo"))
    (gc t) (gc :tenure) (gc :tenure) (gc :tenure)
    (format t "~&;;; starting timing testing 1...~%")
    (time (dotimes (i 100000) (parse-uri uri)))

    (gc t) (gc :tenure) (gc :tenure) (gc :tenure)
    (format t "~&;;; starting timing testing 2...~%")
    (let ((uri (parse-uri uri)))
      (time (dotimes (i 100000)
              ;; forces no caching of the printed representation:
              (setf (uri-string uri) nil)
              (format nil "~a" uri))))

    (gc t) (gc :tenure) (gc :tenure) (gc :tenure)
    (format t "~&;;; starting timing testing 3...~%")
    (time
     (progn
       (dotimes (i 100000) (parse-uri uri2))
       (let ((uri (parse-uri uri)))
         (dotimes (i 100000)
           ;; forces no caching of the printed representation:
           (setf (uri-string uri) nil)
           (format nil "~a" uri)))))))

;;******** reference output (ultra, modified 5.0.1):
;;; starting timing testing 1...
; cpu time (non-gc) 13,710 msec user, 0 msec system
; cpu time (gc)     600 msec user, 10 msec system
; cpu time (total)  14,310 msec user, 10 msec system
; real time  14,465 msec
; space allocation:
;  1,804,261 cons cells, 7 symbols, 41,628,832 other bytes, 0 static bytes
;;; starting timing testing 2...
; cpu time (non-gc) 27,500 msec user, 0 msec system
; cpu time (gc)     280 msec user, 20 msec system
; cpu time (total)  27,780 msec user, 20 msec system
; real time  27,897 msec
; space allocation:
;  1,900,463 cons cells, 0 symbols, 17,693,712 other bytes, 0 static bytes
;;; starting timing testing 3...
; cpu time (non-gc) 52,290 msec user, 10 msec system
; cpu time (gc)     1,290 msec user, 30 msec system
; cpu time (total)  53,580 msec user, 40 msec system
; real time  54,062 msec
; space allocation:
;  7,800,205 cons cells, 0 symbols, 81,697,496 other bytes, 0 static bytes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; after improving decode-escaped-encoding/encode-escaped-encoding:

;;; starting timing testing 1...
; cpu time (non-gc) 14,520 msec user, 0 msec system
; cpu time (gc)     400 msec user, 0 msec system
; cpu time (total)  14,920 msec user, 0 msec system
; real time  15,082 msec
; space allocation:
;  1,800,270 cons cells, 0 symbols, 41,600,160 other bytes, 0 static bytes
;;; starting timing testing 2...
; cpu time (non-gc) 27,490 msec user, 10 msec system
; cpu time (gc)     300 msec user, 0 msec system
; cpu time (total)  27,790 msec user, 10 msec system
; real time  28,025 msec
; space allocation:
;  1,900,436 cons cells, 0 symbols, 17,693,712 other bytes, 0 static bytes
;;; starting timing testing 3...
; cpu time (non-gc) 47,900 msec user, 20 msec system
; cpu time (gc)     920 msec user, 10 msec system
; cpu time (total)  48,820 msec user, 30 msec system
; real time  49,188 msec
; space allocation:
;  3,700,215 cons cells, 0 symbols, 81,707,144 other bytes, 0 static bytes
