[![Build Status](https://travis-ci.org/joaotavora/fiasco.svg?branch=master)](https://travis-ci.org/joaotavora/fiasco)

Fiasco
======

Fiasco is a simple and powerful test framework for Common Lisp, with
a focus on interactive debugging.

It's a fork of the abandoned [Stefil][stefil] by Attila Lendvai, Tamás 
Borbély and Levente Mészáros.

Up and running
--------------

Assuming you're using [quicklisp][quicklisp], type 
`(ql:quickload :fiasco)` somewhere in your REPL and create some Lisp 
file with:

```lisp
(defpackage #:example-time (:export #:seconds #:hours-and-minutes))
(in-package #:example-time)

(defun seconds (hours-and-minutes)
  (+ (* 3600 (first hours-and-minutes))
     (* 60 (seconds hours-and-minutes))))

(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (truncate seconds 60)))

(fiasco:define-test-package #:fiasco-examples
  (:use #:example-time))
(in-package #:fiasco-examples)

(deftest test-conversion-to-hours-and-minutes ()
  (is (equal (hours-and-minutes 180) '(0 3)))
  (is (equal (hours-and-minutes 4500) '(1 15))))

(deftest test-conversion-to-seconds ()
  (is (= 60 (seconds '(0 1))))
  (is (= 4500 (seconds '(1 15)))))

(deftest double-conversion ()
  (is (= 3600 (seconds (hours-and-minutes 3600))))
  (is (= 1234 (seconds (hours-and-minutes 1234)))))
```
load or compile it, and in your REPL run

    > (in-package :fiasco-examples)
    FIASCO-EXAMPLES> (run-package-tests)
    FIASCO-EXAMPLES (Suite)
      TEST-CONVERSION-TO-SECONDS                                                    [FAIL]
      TEST-CONVERSION-TO-HOURS-AND-MINUTES                                          [FAIL]
      DOUBLE-CONVERSION                                                             [FAIL]

    Fiasco! (3 failures)

      Failure 1: UNEXPECTED-ERROR when running FIASCO-EXAMPLES::TEST-CONVERSION-TO-SECONDS
        Stack overflow (signal 1000)

      Failure 2: FAILED-ASSERTION when running FIASCO-EXAMPLES::TEST-CONVERSION-TO-HOURS-AND-MINUTES
        Binary predicate (EQUAL X Y) failed.
        x: (FIASCO-EXAMPLES::HOURS-AND-MINUTES 4500) => (1 75)
        y: '(1 15) => (1 15)

      Failure 3: UNEXPECTED-ERROR when running FIASCO-EXAMPLES::DOUBLE-CONVERSION
        Stack overflow (signal 1000)
    #<test-run: 4 tests, 4 assertions, 3 failures in 0.031 sec (1 failed assertion, 2 errors, none expected)>

Yay, everything fails!

Debugging failures
------------------

Run the example again, with `:interactive t` to bring up the Lisp debugger 
every time a test failure happens. They are caused by error conditions or 
test assertion failures. We have two of the former and one of the latter.

In this case, we see that the the stack overflow erros are due to a typo 
(`seconds` should be `second` in line 6) and that `hours-and-minutes` should 
be rewritten like:

```lisp
(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (/ (rem seconds 3600) 60)))
```

After that, you'll see a nice

    > (in-package :fiasco-examples)
    FIASCO-EXAMPLES> (run-package-tests)
    FIASCO-EXAMPLES (Suite)
      TEST-CONVERSION-TO-SECONDS                                                    [ OK ]
      TEST-CONVERSION-TO-HOURS-AND-MINUTES                                          [ OK ]
      DOUBLE-CONVERSION                                                             [ OK ]
    #<test-run: 4 tests, 6 assertions, 0 failures in 0.0 sec>

Support
-------

To discuss matters open an [issue][issues] for now or perhaps ask in
the [#lisp][sharp-lisp] IRC channel.


[stefil]: http://common-lisp.net/project/stefil/index-old.shtml
[quicklisp]: http://quicklisp.org
[asdf]: http://common-lisp.net/project/asdf/
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/luismbo/fiasco/issues
