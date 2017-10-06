;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; copyright (c) 1999-2001 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2003 Kevin Rosenberg (significant fixes for using
;; tester package)
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; Original version from ACL 6.1:
;; t-uri.cl,v 1.3.6.3.2.1 2001/08/09 17:42:43 layer
;;
;; $Id$


(defpackage #:puri-tests (:use #:puri #:cl #:ptester))
(in-package #:puri-tests)

(unintern-uri t)

(defmacro gen-test-forms ()
  (let ((res '())
        (base-uri "http://a/b/c/d;p?q"))

    (dolist (x `(;; (relative-uri result base-uri compare-function)
;;;; RFC Appendix C.1 (normal examples)
                 ("g:h" "g:h" ,base-uri)
                 ("g" "http://a/b/c/g" ,base-uri)
                 ("./g" "http://a/b/c/g" ,base-uri)
                 ("g/" "http://a/b/c/g/" ,base-uri)
                 ("/g" "http://a/g" ,base-uri)
                 ("//g" "http://g" ,base-uri)
                 ;; Following was changed from appendix C of RFC 2396
                 ;; http://www.apache.org/~fielding/uri/rev-2002/issues.html#003-relative-query
                 #-ignore ("?y" "http://a/b/c/d;p?y" ,base-uri)
                 #+ignore ("?y" "http://a/b/c/?y" ,base-uri)
                 ("g?y" "http://a/b/c/g?y" ,base-uri)
                 ("#s" "http://a/b/c/d;p?q#s" ,base-uri)
                 ("g#s" "http://a/b/c/g#s" ,base-uri)
                 ("g?y#s" "http://a/b/c/g?y#s" ,base-uri)
                 (";x" "http://a/b/c/;x" ,base-uri)
                 ("g;x" "http://a/b/c/g;x" ,base-uri)
                 ("g;x?y#s" "http://a/b/c/g;x?y#s" ,base-uri)
                 ("." "http://a/b/c/" ,base-uri)
                 ("./" "http://a/b/c/" ,base-uri)
                 (".." "http://a/b/" ,base-uri)
                 ("../" "http://a/b/" ,base-uri)
                 ("../g" "http://a/b/g" ,base-uri)
                 ("../.." "http://a/" ,base-uri)
                 ("../../" "http://a/" ,base-uri)
                 ("../../g" "http://a/g" ,base-uri)
;;;; RFC Appendix C.2 (abnormal examples)
                 ("" "http://a/b/c/d;p?q" ,base-uri)
                 ("../../../g" "http://a/../g" ,base-uri)
                 ("../../../../g" "http://a/../../g" ,base-uri)
                 ("/./g" "http://a/./g" ,base-uri)
                 ("/../g" "http://a/../g" ,base-uri)
                 ("g." "http://a/b/c/g." ,base-uri)
                 (".g" "http://a/b/c/.g" ,base-uri)
                 ("g.." "http://a/b/c/g.." ,base-uri)
                 ("..g" "http://a/b/c/..g" ,base-uri)
                 ("./../g" "http://a/b/g" ,base-uri)
                 ("./g/." "http://a/b/c/g/" ,base-uri)
                 ("g/./h" "http://a/b/c/g/h" ,base-uri)
                 ("g/../h" "http://a/b/c/h" ,base-uri)
                 ("g;x=1/./y" "http://a/b/c/g;x=1/y" ,base-uri)
                 ("g;x=1/../y" "http://a/b/c/y" ,base-uri)
                 ("g?y/./x" "http://a/b/c/g?y/./x" ,base-uri)
                 ("g?y/../x" "http://a/b/c/g?y/../x" ,base-uri)
                 ("g#s/./x" "http://a/b/c/g#s/./x" ,base-uri)
                 ("g#s/../x" "http://a/b/c/g#s/../x" ,base-uri)
                 ("http:g" "http:g" ,base-uri)

                 ("foo/bar/baz.htm#foo"
                  "http://a/b/foo/bar/baz.htm#foo"
                  "http://a/b/c.htm")
                 ("foo/bar/baz.htm#foo"
                  "http://a/b/foo/bar/baz.htm#foo"
                  "http://a/b/")
                 ("foo/bar/baz.htm#foo"
                  "http://a/foo/bar/baz.htm#foo"
                  "http://a/b")
                 ("foo/bar;x;y/bam.htm"
                  "http://a/b/c/foo/bar;x;y/bam.htm"
                  "http://a/b/c/")))
      (push `(test (intern-uri ,(second x))
                             (intern-uri (merge-uris (intern-uri ,(first x))
                                                     (intern-uri ,(third x))))
                             :test 'uri=)
            res))

;;;; intern tests
    (dolist (x '(;; default port and specifying the default port are
                 ;; supposed to compare the same:
                 ("http://www.franz.com:80" "http://www.franz.com")
                 ("http://www.franz.com:80" "http://www.franz.com" eq)
                 ;; make sure they're `eq':
                 ("http://www.franz.com:80" "http://www.franz.com" eq)
                 ("http://www.franz.com" "http://www.franz.com" eq)
                 ("http://www.franz.com/foo" "http://www.franz.com/foo" eq)
                 ("http://www.franz.com/foo?bar"
                  "http://www.franz.com/foo?bar" eq)
                 ("http://www.franz.com/foo?bar#baz"
                  "http://www.franz.com/foo?bar#baz" eq)
                 ("http://WWW.FRANZ.COM" "http://www.franz.com" eq)
                 ("http://www.FRANZ.com" "http://www.franz.com" eq)
                 ("http://www.franz.com" "http://www.franz.com/" eq)
                 (;; %72 is "r", %2f is "/", %3b is ";"
                  "http://www.franz.com/ba%72%2f%3b;x;y;z/baz/"
                  "http://www.franz.com/bar%2f%3b;x;y;z/baz/" eq)))
      (push `(test (intern-uri ,(second x))
                             (intern-uri ,(first x))
              :test ',(if (third x)
                          (third x)
                          'uri=))
            res))

;;;; parsing and equivalence tests
    (push `(test
            (parse-uri "http://foo+bar?baz=b%26lob+bof")
            (parse-uri (parse-uri "http://foo+bar?baz=b%26lob+bof"))
            :test 'uri=)
          res)
    (push '(test
            (parse-uri "http://www.foo.com")
            (parse-uri (parse-uri "http://www.foo.com?")) ; allow ? at end
            :test 'uri=)
          res)
    (push `(test
            "baz=b%26lob+bof"
            (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof"))
            :test 'string=)
          res)
    (push `(test
            "baz=b%26lob+bof%3d"
            (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof%3d"))
            :test 'string=)
          res)
    (push
     `(test (parse-uri "xxx?%41") (parse-uri "xxx?A") :test 'uri=)
     res)
    (push
     `(test "A" (uri-query (parse-uri "xxx?%41")) :test 'string=)
     res)

    (push `(test-error (parse-uri " ")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "foo ")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri " foo ")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "<foo")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "foo>")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "<foo>")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "%")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "foo%xyr")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "\"foo\"")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test "%20" (format nil "~a" (parse-uri "%20"))
                           :test 'string=)
          res)
    (push `(test "&" (format nil "~a" (parse-uri "%26"))
                           :test 'string=)
          res)
    (push
     `(test "foo%23bar" (format nil "~a" (parse-uri "foo%23bar"))
                      :test 'string=)
     res)
    (push
     `(test "foo%23bar#foobar"
                      (format nil "~a" (parse-uri "foo%23bar#foobar"))
                      :test 'string=)
     res)
    (push
     `(test "foo%23bar#foobar#baz"
                      (format nil "~a" (parse-uri "foo%23bar#foobar#baz"))
                      :test 'string=)
     res)
    (push
     `(test "foo%23bar#foobar#baz"
                      (format nil "~a" (parse-uri "foo%23bar#foobar%23baz"))
                      :test 'string=)
     res)
    (push
     `(test "foo%23bar#foobar/baz"
                      (format nil "~a" (parse-uri "foo%23bar#foobar%2fbaz"))
                      :test 'string=)
     res)
    (push `(test-error (parse-uri "foobar??")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "foobar?foo?")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test "foobar?%3f"
                           (format nil "~a" (parse-uri "foobar?%3f"))
                           :test 'string=)
          res)
    (push `(test
            "http://foo/bAr;3/baz?baf=3"
            (format nil "~a" (parse-uri "http://foo/b%41r;3/baz?baf=3"))
            :test 'string=)
          res)
    (push `(test
            '(:absolute ("/bAr" "3") "baz")
            (uri-parsed-path (parse-uri "http://foo/%2fb%41r;3/baz?baf=3"))
            :test 'equal)
          res)
    (push `(test
            "/%2fbAr;3/baz"
            (let ((u (parse-uri "http://foo/%2fb%41r;3/baz?baf=3")))
              (setf (uri-parsed-path u) '(:absolute ("/bAr" "3") "baz"))
              (uri-path u))
            :test 'string=)
          res)
    (push `(test
            "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"
            (format nil "~a"
                    (parse-uri
                     "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"))
            :test 'string=)
          res)
    (push `(test
            "ftp://parcftp.xerox.com/pub/pcl/mop/"
            (format nil "~a"
                    (parse-uri "ftp://parcftp.xerox.com:/pub/pcl/mop/"))
            :test 'string=)
          res)

;;;; enough-uri tests
    (dolist (x `(("http://www.franz.com/foo/bar/baz.htm"
                  "http://www.franz.com/foo/bar/"
                  "baz.htm")
                 ("http://www.franz.com/foo/bar/baz.htm"
                  "http://www.franz.com/foo/bar"
                  "baz.htm")
                 ("http://www.franz.com:80/foo/bar/baz.htm"
                  "http://www.franz.com:80/foo/bar"
                  "baz.htm")
                 ("http:/foo/bar/baz.htm" "http:/foo/bar"  "baz.htm")
                 ("http:/foo/bar/baz.htm" "http:/foo/bar/" "baz.htm")
                 ("/foo/bar/baz.htm" "/foo/bar"  "baz.htm")
                 ("/foo/bar/baz.htm" "/foo/bar/" "baz.htm")
                 ("/foo/bar/baz.htm#foo" "/foo/bar/" "baz.htm#foo")
                 ("/foo/bar/baz.htm?bar#foo" "/foo/bar/" "baz.htm?bar#foo")

                 ("http://www.dnai.com/~layer/foo.htm"
                  "http://www.known.net"
                  "http://www.dnai.com/~layer/foo.htm")
                 ("http://www.dnai.com/~layer/foo.htm"
                  "http://www.dnai.com:8000/~layer/"
                  "http://www.dnai.com/~layer/foo.htm")
                 ("http://www.dnai.com:8000/~layer/foo.htm"
                  "http://www.dnai.com/~layer/"
                  "http://www.dnai.com:8000/~layer/foo.htm")
                 ("http://www.franz.com"
                  "http://www.franz.com"
                  "/")))
      (push `(test (parse-uri ,(third x))
                             (enough-uri (parse-uri ,(first x))
                                         (parse-uri ,(second x)))
                             :test 'uri=)
            res))

;;;; urn tests, ideas of which are from rfc2141
    (let ((urn "urn:com:foo-the-bar"))
      (push `(test "com" (urn-nid (parse-uri ,urn))
                             :test #'string=)
            res)
      (push `(test "foo-the-bar" (urn-nss (parse-uri ,urn))
                             :test #'string=)
            res))
    (push `(test-error (parse-uri "urn:")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "urn:foo")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "urn:foo$")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "urn:foo_")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test-error (parse-uri "urn:foo:foo&bar")
                                 :condition-type 'uri-parse-error)
          res)
    (push `(test (parse-uri "URN:foo:a123,456")
                           (parse-uri "urn:foo:a123,456")
                           :test #'uri=)
          res)
    (push `(test (parse-uri "URN:foo:a123,456")
                           (parse-uri "urn:FOO:a123,456")
                           :test #'uri=)
          res)
    (push `(test (parse-uri "urn:foo:a123,456")
                           (parse-uri "urn:FOO:a123,456")
                           :test #'uri=)
          res)
    (push `(test (parse-uri "URN:FOO:a123%2c456")
                           (parse-uri "urn:foo:a123%2C456")
                           :test #'uri=)
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:foo:A123,456")
                  (parse-uri "urn:FOO:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:foo:A123,456")
                  (parse-uri "urn:foo:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:foo:A123,456")
                  (parse-uri "URN:foo:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:foo:a123%2C456")
                  (parse-uri "urn:FOO:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:foo:a123%2C456")
                  (parse-uri "urn:foo:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "URN:FOO:a123%2c456")
                  (parse-uri "urn:foo:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:FOO:a123%2c456")
                  (parse-uri "urn:foo:a123,456")))
          res)
    (push `(test
            nil
            (uri= (parse-uri "urn:foo:a123%2c456")
                  (parse-uri "urn:foo:a123,456")))
          res)

    (push `(test t
                           (uri= (parse-uri "foo") (parse-uri "foo#")))
          res)

    (push
     '(let ((puri::*strict-parse* nil))
       (test-no-error
        (puri:parse-uri
         "http://foo.com/bar?a=zip|zop")))
     res)
    (push
     '(test-error
       (puri:parse-uri "http://foo.com/bar?a=zip|zop")
       :condition-type 'uri-parse-error)
     res)

    (push
     '(let ((puri::*strict-parse* nil))
       (test-no-error
        (puri:parse-uri
         "http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041")))
     res)
    (push
     '(test-error
       (puri:parse-uri
        "http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041")
       :condition-type 'uri-parse-error)
     res)

    (push
     '(let ((puri::*strict-parse* nil))
       (test-no-error
        (puri:parse-uri
         "http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")))
     res)
    (push
     '(test-error
       (puri:parse-uri
        "http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")
       :condition-type 'uri-parse-error)
     res)

    ;;; tests for weird control characters
    ;; http://www.ietf.org/rfc/rfc2396.txt 2.4.3
    (dolist (x '("https://example.com/q?foo%0abar%20baz" ;;an escaped newline
		 "https://example.com/q?%7f" ;; 7f, 127
		 ))
      (push
       `(let ((weird-uri ,x))
	  (test weird-uri
		(puri:render-uri (puri:parse-uri weird-uri) nil)
		:test #'string=)
	  ) res))

    `(progn ,@(nreverse res))))

(defun do-tests ()
  (let ((*break-on-test-failures* t))
    (with-tests (:name "puri")
      (gen-test-forms)))
  t)


