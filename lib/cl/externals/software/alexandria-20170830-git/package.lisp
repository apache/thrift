(defpackage :alexandria.0.dev
  (:nicknames :alexandria)
  (:use :cl)
  #+sb-package-locks
  (:lock t)
  (:export
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; BLESSED
   ;;
   ;; Binding constructs
   #:if-let
   #:when-let
   #:when-let*
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; REVIEW IN PROGRESS
   ;;
   ;; Control flow
   ;;
   ;; -- no clear consensus yet --
   #:cswitch
   #:eswitch
   #:switch
   ;; -- problem free? --
   #:multiple-value-prog2
   #:nth-value-or
   #:whichever
   #:xor
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; REVIEW PENDING
   ;;
   ;; Definitions
   #:define-constant
   ;; Hash tables
   #:alist-hash-table
   #:copy-hash-table
   #:ensure-gethash
   #:hash-table-alist
   #:hash-table-keys
   #:hash-table-plist
   #:hash-table-values
   #:maphash-keys
   #:maphash-values
   #:plist-hash-table
   ;; Functions
   #:compose
   #:conjoin
   #:curry
   #:disjoin
   #:ensure-function
   #:ensure-functionf
   #:multiple-value-compose
   #:named-lambda
   #:rcurry
   ;; Lists
   #:alist-plist
   #:appendf
   #:nconcf
   #:reversef
   #:nreversef
   #:circular-list
   #:circular-list-p
   #:circular-tree-p
   #:doplist
   #:ensure-car
   #:ensure-cons
   #:ensure-list
   #:flatten
   #:lastcar
   #:make-circular-list
   #:map-product
   #:mappend
   #:nunionf
   #:plist-alist
   #:proper-list
   #:proper-list-length
   #:proper-list-p
   #:remove-from-plist
   #:remove-from-plistf
   #:delete-from-plist
   #:delete-from-plistf
   #:set-equal
   #:setp
   #:unionf
   ;; Numbers
   #:binomial-coefficient
   #:clamp
   #:count-permutations
   #:factorial
   #:gaussian-random
   #:iota
   #:lerp
   #:map-iota
   #:maxf
   #:mean
   #:median
   #:minf
   #:standard-deviation
   #:subfactorial
   #:variance
   ;; Arrays
   #:array-index
   #:array-length
   #:copy-array
   ;; Sequences
   #:copy-sequence
   #:deletef
   #:emptyp
   #:ends-with
   #:ends-with-subseq
   #:extremum
   #:first-elt
   #:last-elt
   #:length=
   #:map-combinations
   #:map-derangements
   #:map-permutations
   #:proper-sequence
   #:random-elt
   #:removef
   #:rotate
   #:sequence-of-length-p
   #:shuffle
   #:starts-with
   #:starts-with-subseq
   ;; Macros
   #:once-only
   #:parse-body
   #:parse-ordinary-lambda-list
   #:with-gensyms
   #:with-unique-names
   ;; Symbols
   #:ensure-symbol
   #:format-symbol
   #:make-gensym
   #:make-gensym-list
   #:make-keyword
   ;; Strings
   #:string-designator
   ;; Types
   #:negative-double-float
   #:negative-fixnum-p
   #:negative-float
   #:negative-float-p
   #:negative-long-float
   #:negative-long-float-p
   #:negative-rational
   #:negative-rational-p
   #:negative-real
   #:negative-single-float-p
   #:non-negative-double-float
   #:non-negative-double-float-p
   #:non-negative-fixnum
   #:non-negative-fixnum-p
   #:non-negative-float
   #:non-negative-float-p
   #:non-negative-integer-p
   #:non-negative-long-float
   #:non-negative-rational
   #:non-negative-real-p
   #:non-negative-short-float-p
   #:non-negative-single-float
   #:non-negative-single-float-p
   #:non-positive-double-float
   #:non-positive-double-float-p
   #:non-positive-fixnum
   #:non-positive-fixnum-p
   #:non-positive-float
   #:non-positive-float-p
   #:non-positive-integer
   #:non-positive-rational
   #:non-positive-real
   #:non-positive-real-p
   #:non-positive-short-float
   #:non-positive-short-float-p
   #:non-positive-single-float-p
   #:ordinary-lambda-list-keywords
   #:positive-double-float
   #:positive-double-float-p
   #:positive-fixnum
   #:positive-fixnum-p
   #:positive-float
   #:positive-float-p
   #:positive-integer
   #:positive-rational
   #:positive-real
   #:positive-real-p
   #:positive-short-float
   #:positive-short-float-p
   #:positive-single-float
   #:positive-single-float-p
   #:coercef
   #:negative-double-float-p
   #:negative-fixnum
   #:negative-integer
   #:negative-integer-p
   #:negative-real-p
   #:negative-short-float
   #:negative-short-float-p
   #:negative-single-float
   #:non-negative-integer
   #:non-negative-long-float-p
   #:non-negative-rational-p
   #:non-negative-real
   #:non-negative-short-float
   #:non-positive-integer-p
   #:non-positive-long-float
   #:non-positive-long-float-p
   #:non-positive-rational-p
   #:non-positive-single-float
   #:of-type
   #:positive-integer-p
   #:positive-long-float
   #:positive-long-float-p
   #:positive-rational-p
   #:type=
   ;; Conditions
   #:required-argument
   #:ignore-some-conditions
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:unwind-protect-case
   ;; Features
   #:featurep
   ;; io
   #:with-input-from-file
   #:with-output-to-file
   #:read-stream-content-into-string
   #:read-file-into-string
   #:write-string-into-file
   #:read-stream-content-into-byte-vector
   #:read-file-into-byte-vector
   #:write-byte-vector-into-file
   #:copy-stream
   #:copy-file
   ;; new additions collected at the end (subject to removal or further changes)
   #:symbolicate
   #:assoc-value
   #:rassoc-value
   #:destructuring-case
   #:destructuring-ccase
   #:destructuring-ecase
   ))
