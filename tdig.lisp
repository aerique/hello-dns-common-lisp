;;;;- tdig.lisp
;;;;-
;;;; # tdig
;;;;
;;;; Usage: `sbcl --noinform --load tdig.lisp --eval "(main)"
;;;;
;;;; Create binary: `sbcl --load tdig.lisp --eval "(compile-tdig)"
;;;;
;;;; Inspiration: <https://powerdns.org/hello-dns/tdns/README.md.html>

;;; ## Packages

(load "tdns.lisp")


;;; ## Functions

;; **FIXME**: add ECL, ACL, ABCL
;;
;; **NOTE**: Only SBCL has been tested so I've no idea what the other
;;           implementations are formatted like.
(defun argv ()
  (or #+clisp *args*
      #+cmu extensions:*command-line-words*
      #+lispworks system:*line-arguments-list*
      #+sbcl *posix-argv*
      nil))


(defun describe-response (parsed-response raw-response)
  (format t "Received ~D byte response with RCode ~@(~A~), qname ~A, qtype ~A~%"
          (length raw-response)
          (dns-rcode (rcode (header parsed-response)))
          ;; Is the extra nesting in QUESTION-SECTION needed?
          (when (first (questions parsed-response))
            (to-string (qname (first (questions parsed-response)))))
          (when (first (questions parsed-response))
            (dns-type (qtype (first (questions parsed-response))))))
  (loop for rr in (concatenate 'list (answers parsed-response)
                                     (records parsed-response)
                                     (additional parsed-response))
        do (format t "- ~A ~A ~A ~D ~S~%  - ~S" (to-string (name rr))
                   (dns-class (rclass rr)) (dns-type (rtype rr)) (ttl rr)
                   (rdata rr)
                   rr)))


;;; ## Main Program
;;;
;;; As opposed to C / C++ `main` is just an arbitrary name here.

(defun main ()
  (let ((dn (second (argv)))
        (dt (third (argv)))
        (server (fourth (argv)))
        res)
    (unless dn
      (format t "Usage: tdig DOMAIN TYPE NAMESERVER~%")
      (sb-ext:exit :code 1))
    (when *verbose*
      (format *debug-io* "dn=~S~%dt=~S~%server=~S~%" dn dt server))
    (setf res (get-response dn :host server :port 53 :dns-type dt))
    (when *verbose*
      (format *debug-io* "~S~%" (make-dns-message res)))
    (describe-response (make-dns-message res) res))
  (quit))


(defun compile-tdig ()
  (save-lisp-and-die "tdig" :toplevel #'main :executable t))
