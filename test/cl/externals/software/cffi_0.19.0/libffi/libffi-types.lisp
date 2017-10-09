;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; libffi-types.lisp -- CFFI-Grovel definitions for libffi
;;;
;;; Copyright (C) 2009, 2010, 2011 Liam M. Healy  <lhealy@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi)

#+linux
(define "_GNU_SOURCE")

;; When installed through Mac Ports, libffi include files
;; will be found in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

#+openbsd
(cc-flags "-I/usr/local/include")

(pkg-config-cflags "libffi" :optional t)

#+darwin
(include "ffi/ffi.h")
#-darwin
(include "ffi.h")

(cenum status
 ((:ok "FFI_OK"))
 ((:bad-typedef "FFI_BAD_TYPEDEF"))
 ((:bad-abi "FFI_BAD_ABI")))

#+freebsd
(cenum abi
 ((:default-abi "FFI_DEFAULT_ABI")))

#+(and windows x86-64)
(cenum abi
 ((:default-abi "FFI_DEFAULT_ABI"))
 ((:win64 "FFI_WIN64")))

#+(and windows (not x86-64))
(cenum abi
 ((:default-abi "FFI_DEFAULT_ABI"))
 ((:sysv "FFI_SYSV"))
 ((:stdcall "FFI_STDCALL")))

#-(or freebsd windows)
(cenum abi
 ((:default-abi "FFI_DEFAULT_ABI"))
 ((:sysv "FFI_SYSV"))
 ((:unix64 "FFI_UNIX64")))

(ctype ffi-abi "ffi_abi")

(ctype size-t "size_t")

(cstruct ffi-type "struct _ffi_type"
  (size      "size"      :type size-t)
  (alignment "alignment" :type :unsigned-short)
  (type      "type"      :type :unsigned-short)
  (elements  "elements"  :type :pointer))

(cstruct ffi-cif "ffi_cif"
 (abi            "abi"       :type ffi-abi)
 (argument-count "nargs"     :type :unsigned-int)
 (argument-types "arg_types" :type :pointer)
 (return-type    "rtype"     :type :pointer)
 (bytes          "bytes"     :type :unsigned-int)
 (flags          "flags"     :type :unsigned-int))

(constant (+type-void+ "FFI_TYPE_VOID"))
(constant (+type-int+ "FFI_TYPE_INT"))
(constant (+type-float+ "FFI_TYPE_FLOAT"))
(constant (+type-double+ "FFI_TYPE_DOUBLE"))
(constant (+type-longdouble+ "FFI_TYPE_LONGDOUBLE"))
(constant (+type-uint8+ "FFI_TYPE_UINT8"))
(constant (+type-sint8+ "FFI_TYPE_SINT8"))
(constant (+type-uint16+ "FFI_TYPE_UINT16"))
(constant (+type-sint16+ "FFI_TYPE_SINT16"))
(constant (+type-uint32+ "FFI_TYPE_UINT32"))
(constant (+type-sint32+ "FFI_TYPE_SINT32"))
(constant (+type-uint64+ "FFI_TYPE_UINT64"))
(constant (+type-sint64+ "FFI_TYPE_SINT64"))
(constant (+type-struct+ "FFI_TYPE_STRUCT"))
(constant (+type-pointer+ "FFI_TYPE_POINTER"))
