;;;; run-common-tests.lisp

;;; Packages

(format t "~&Loading lisp-unit...~%")
(let ((*muffled-warnings* 'style-warning))
  (load "tests/lisp-unit"))
(format t "Loading common.lisp...~%")
(load "common.lisp")
(use-package :lisp-unit)


;;; Tests

(define-test 2-bytes-to-int ()
  (assert-equal   0 (2-bytes-to-int '(0 0)))
  (assert-equal   1 (2-bytes-to-int '(0 1)))
  (assert-equal 256 (2-bytes-to-int '(1 0)))
  (assert-equal 257 (2-bytes-to-int '(1 1)))
  (assert-equal   0 (2-bytes-to-int '(0 0) :big-endian nil))
  (assert-equal   1 (2-bytes-to-int '(1 0) :big-endian nil))
  (assert-equal 256 (2-bytes-to-int '(0 1) :big-endian nil))
  (assert-equal 257 (2-bytes-to-int '(1 1) :big-endian nil)))


(define-test 4-bytes-to-int ()
  (assert-equal        0 (4-bytes-to-int '(0 0 0 0)))
  (assert-equal        1 (4-bytes-to-int '(0 0 0 1)))
  (assert-equal      256 (4-bytes-to-int '(0 0 1 0)))
  (assert-equal    65536 (4-bytes-to-int '(0 1 0 0)))
  (assert-equal 16777216 (4-bytes-to-int '(1 0 0 0)))
  (assert-equal 16843009 (4-bytes-to-int '(1 1 1 1)))
  (assert-equal        0 (4-bytes-to-int '(0 0 0 0) :big-endian nil))
  (assert-equal        1 (4-bytes-to-int '(1 0 0 0) :big-endian nil))
  (assert-equal      256 (4-bytes-to-int '(0 1 0 0) :big-endian nil))
  (assert-equal    65536 (4-bytes-to-int '(0 0 1 0) :big-endian nil))
  (assert-equal 16777216 (4-bytes-to-int '(0 0 0 1) :big-endian nil))
  (assert-equal 16843009 (4-bytes-to-int '(1 1 1 1) :big-endian nil)))

(define-test int-to-2-bytes ()
  (assert-equal '(0 0) (int-to-2-bytes   0))
  (assert-equal '(0 1) (int-to-2-bytes   1))
  (assert-equal '(1 0) (int-to-2-bytes 256))
  (assert-equal '(1 1) (int-to-2-bytes 257))
  (assert-equal '(0 0) (int-to-2-bytes   0 :big-endian nil))
  (assert-equal '(1 0) (int-to-2-bytes   1 :big-endian nil))
  (assert-equal '(0 1) (int-to-2-bytes 256 :big-endian nil))
  (assert-equal '(1 1) (int-to-2-bytes 257 :big-endian nil)))


(define-test int-to-4-bytes ()
  (assert-equal '(0 0 0 0) (int-to-4-bytes        0))
  (assert-equal '(0 0 0 1) (int-to-4-bytes        1))
  (assert-equal '(0 0 1 0) (int-to-4-bytes      256))
  (assert-equal '(0 1 0 0) (int-to-4-bytes    65536))
  (assert-equal '(1 0 0 0) (int-to-4-bytes 16777216))
  (assert-equal '(1 1 1 1) (int-to-4-bytes 16843009))
  (assert-equal '(0 0 0 0) (int-to-4-bytes        0 :big-endian nil))
  (assert-equal '(1 0 0 0) (int-to-4-bytes        1 :big-endian nil))
  (assert-equal '(0 1 0 0) (int-to-4-bytes      256 :big-endian nil))
  (assert-equal '(0 0 1 0) (int-to-4-bytes    65536 :big-endian nil))
  (assert-equal '(0 0 0 1) (int-to-4-bytes 16777216 :big-endian nil))
  (assert-equal '(1 1 1 1) (int-to-4-bytes 16843009 :big-endian nil)))


;;; Run the tests.

;(format t "Running common.lisp tests...~%")
(run-tests)
(format t "~&")
(cl-user::quit)
