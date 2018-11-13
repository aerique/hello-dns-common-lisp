;;;;- tdig.lisp
;;;;-
;;;; # tdig
;;;;
;;;; Usage: `sbcl --noinform --load tdig.lisp --eval "(main)" [ARG1] [ARG2] ...
;;;;
;;;; Inspiration: <https://powerdns.org/hello-dns/tdns/README.md.html>

;;; ## Packages

(load "common.lisp")


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


(defun describe-response (parsed-response)
  (format t "Received ~D byte response with RCode ~@(~A~), qname ~S, qtype ~A~%"
          (getf parsed-response :response-size)
          (getf parsed-response :rcode)
          ;; Is the extra nesting in QUESTION-SECTION needed?
          (when (first (getf parsed-response :question-section))
            (getf (first (getf parsed-response :question-section)) :qname))
          (when (first (getf parsed-response :question-section))
            (getf (first (getf parsed-response :question-section)) :qtype)))
  (loop for rr in (concatenate 'list (getf parsed-response :answer-section)
                                     (getf parsed-response :record-section)
                                     (getf parsed-response :additional-section))
        do (format t "~S ~S ~S ~S ~S~%" (getf rr :name) (getf rr :class)
                   (getf rr :type) (getf rr :ttl) (getf rr :rdata))))


;;; ## Main Program
;;;
;;; As opposed to C/C++ `main` is just an arbitrary name here.

(defun main ()
  (let ((dn (second (argv)))
        (dt (third (argv)))
        (server (fourth (argv)))
        res)
    (when *verbose*
      (format *debug-io* "dn=~S~%dt=~S~%server=~S~%" dn dt server))
    (setf res (get-response dn :host server :port 53 :dns-type dt))
    (when *verbose*
      (format *debug-io* "~S" (parse-dns-message res)))
    (describe-response (parse-dns-message res)))
  (quit))
