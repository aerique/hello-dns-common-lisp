;;;;- tdns.lisp

;;; ## Packages
;;;
;;; Assumes [Quicklisp](https://www.quicklisp.org/beta/) is installed.

;; https://github.com/fare/command-line-arguments
;(ql:quickload :command-line-arguments)

;; [usocket](https://github.com/usocket/usocket#introduction) is a
;; socket library for many Common Lisp implementations.
;;
;; `:silent t` makes loading the libs more quiet so that running f.e.
;; `bin/tdig` looks nicer but it can hide problems
(ql:quickload :usocket        :silent t)
(ql:quickload :usocket-server :silent t)

(load "common.lisp")


;;; ## Globals

(defvar *verbose* nil)


;; Silence compiler about undefined functions.
;; (Common Lisp has many warts: this is one of them.)
(defun parse-dns-name (a &optional b) (declare (ignore a b)))


;;; ## DNS Types
;;;
;;; Conversion between integers and something readable for DNS class, opcode,
;;; rcode and type.
;;;
;;; We just bluntly dump everything in a hash table for now.

(defun make-dns-hash-table (integer-keyword-values)
  (let ((ht (make-hash-table)))
    (loop for (integer keyword) in integer-keyword-values
          do (setf (gethash integer ht) keyword
                   (gethash keyword ht) integer))
    ht))


(defparameter dns-class-hash-table
  (make-dns-hash-table '((  1 :in)
                         (  3 :ch))))

(defun dns-class (integer-or-keyword)
  (gethash integer-or-keyword dns-class-hash-table))


(defparameter dns-opcode-hash-table
  (make-dns-hash-table '((0 :query)
                         (2 :status)
                         (4 :notify)
                         (5 :update))))

(defun dns-opcode (integer-or-keyword)
  (gethash integer-or-keyword dns-opcode-hash-table))


(defparameter dns-qr-hash-table
  (make-dns-hash-table '((0 :query)
                         (1 :response))))

(defun dns-qr (integer-or-keyword)
  (gethash integer-or-keyword dns-qr-hash-table))


(defparameter dns-rcode-hash-table
  (make-dns-hash-table '(( 0 :noerror)
                         ( 1 :formerr)
                         ( 2 :servfail)
                         ( 3 :nxdomain)
                         ( 4 :notimp)
                         ( 5 :refused)
                         ( 9 :notauth)
                         (16 :badvers))))

(defun dns-rcode (integer-or-keyword)
  (gethash integer-or-keyword dns-rcode-hash-table))


(defparameter dns-type-hash-table
  (make-dns-hash-table '(;; resource records
                         (    1 :a)
                         (    2 :ns)
                         (    5 :cname)
                         (    6 :soa)
                         (   12 :ptr)
                         (   15 :mx)
                         (   16 :txt)
                         (   28 :aaaa)
                         (   33 :srv)
                         (   35 :naptr)
                         (   43 :ds)
                         (   46 :rrsig)
                         (   47 :nsec)
                         (   48 :dnskey)
                         (   50 :nsec3)
                         ;; other types and pseudo resource records
                         (   41 :opt)
                         (  251 :ixfr)
                         (  252 :axfr)
                         (  255 :any)
                         (  257 :caa))))

(defun dns-type (integer-or-keyword)
  (gethash integer-or-keyword dns-type-hash-table))


;;; ## Classes

;;; ### dns-header
;;;
;;; So, because we're in the `feature/do-not-convert-to-keywords` branch, we
;;; cannot use `BOOLEAN` as a type but have to use `(INTEGER 0 1)` :-|
;;;
;;; Note: If *RANDOM-STATE* is not initialised then RANDOM output will be
;;;       deterministic!
;;;
;;; (Note: SBCL will only trigger on invalid values if SAFETY is set to 3 (or
;;; at least not at 0 :-D .)

(defclass dns-header ()
  ((id      :initarg :id      :reader id      :type (integer 0 65535)
            :initform (random 65535))
   (qr      :initarg :qr      :reader qr      :type (integer 0 1)
            :initform 0)
   (opcode  :initarg :opcode  :reader opcode  :type (integer 0 15)
            :initform 0)
   (aa      :initarg :aa      :reader aa      :type (integer 0 1)
            :initform 0)
   (tc      :initarg :tc      :reader tc      :type (integer 0 1)
            :initform 0)
   (rd      :initarg :rd      :reader rd      :type (integer 0 1)
            :initform 0)
   (ra      :initarg :ra      :reader ra      :type (integer 0 1)
            :initform 0)
   (z       :initarg :z       :reader z       :type (integer 0 7)
            :initform 0)
   (rcode   :initarg :rcode   :reader rcode   :type (integer 0 15)
            :initform 0)
   (qdcount :initarg :qdcount :reader qdcount :type (integer 0 65535)
            :initform 0)
   (ancount :initarg :ancount :reader ancount :type (integer 0 65535)
            :initform 0)
   (nscount :initarg :nscount :reader nscount :type (integer 0 65535)
            :initform 0)
   (arcount :initarg :arcount :reader arcount :type (integer 0 65535)
            :initform 0)))


;; https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml
;;
;; This is for printing an object readably.  To see the raw contents, use
;; `(describe object)`.
(defmethod print-object ((obj dns-header) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~D QR=~A OPCODE=~A AA=~S TC=~S RD=~S RA=~S Z=~D ~
                    RCODE=~A QDCOUNT=~D ANCOUNT=~D NSCOUNT=~D ARCOUNT=~D"
            (id obj)
            (dns-qr (qr obj))
            (dns-opcode (opcode obj))
            (= 1 (aa obj))
            (= 1 (tc obj))
            (= 1 (rd obj))
            (= 1 (ra obj))
            (z obj)
            (dns-rcode (rcode obj))
            (qdcount obj)
            (ancount obj)
            (nscount obj)
            (arcount obj))))


(defmethod serialize ((obj dns-header))
  (append (int-to-16bit (id obj))
          (list (+ (if (= 1 (qr obj)) 128 0)
                   (ash (opcode obj) 3)
                   (if (= 1 (aa obj))   4 0)
                   (if (= 1 (tc obj))   2 0)
                   (rd obj)))
          (list (+ (if (= 1 (ra obj)) 128 0)
                   (ash (z obj) 4)
                   (rcode obj)))
          (int-to-16bit (qdcount obj))
          (int-to-16bit (ancount obj))
          (int-to-16bit (nscount obj))
          (int-to-16bit (arcount obj))))


(defun make-dns-header (dns-message)
  (make-instance 'dns-header
               :id (+ (ash (elt dns-message 0) 8) (elt dns-message 1))
               :qr     (ash (logand (elt dns-message 2) #b10000000) -7)
               :opcode (ash (logand (elt dns-message 2) #b01111000) -3)
               :aa     (ash (logand (elt dns-message 2) #b00000100) -2)
               :tc     (ash (logand (elt dns-message 2) #b00000010) -1)
               :rd          (logand (elt dns-message 2) #b00000001)
               :ra    (ash (logand (elt dns-message 3) #b10000000) -7)
               :z     (ash (logand (elt dns-message 3) #b01110000) -4)
               :rcode      (logand (elt dns-message 3) #b00001111)
               :qdcount (+ (ash (elt dns-message  4) 8) (elt dns-message  5))
               :ancount (+ (ash (elt dns-message  6) 8) (elt dns-message  7))
               :nscount (+ (ash (elt dns-message  8) 8) (elt dns-message  9))
               :arcount (+ (ash (elt dns-message 10) 8) (elt dns-message 11))))


;;; ### dns-label
;;;
;;; To Do:
;;;
;;; - check for min size 1 and max size 63
;;; - parse input LABELs to valid DNS labels
;;; - escaping (https://powerdns.org/hello-dns/tdns/README.md.html#objectsin%EE%80%90000l%EE%80%90/dnslabel)

(defclass dns-label ()
  ((label :initarg :label :reader label :type (vector (unsigned-byte 8))
          :initform (error ":LABEL argument to DNS-LABEL class missing."))))


(defmethod print-object ((obj dns-label) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (octets-to-string (label obj)))))


(defmethod make-dns-label ((octets vector))
  (when (> (length octets) 63)
    (error "DNS-LABEL cannot be longer than 63 octets"))
  (make-instance 'dns-label :label octets))


(defmethod make-dns-label ((string string))
  (make-dns-label (string-to-octets string)))


(defmethod serialize ((obj dns-label))
  (concatenate 'vector (vector (length (label obj))) (label obj)))


(defmethod to-string ((obj dns-label))
  (format nil "~A" (octets-to-string (label obj))))


;;; ### dns-name

(defclass dns-name ()
  ((name :initarg :name :reader name :type (vector dns-label)  ; why not use a list?
         :initform (error "Must supply :NAME argument to DNS-NAME class."))))


(defmethod print-object ((obj dns-name) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~{~A~^ ~}" (coerce (name obj) 'list))))


(defmethod make-dns-name ((lst list))
  "Assumes LST is a list of DNS-LABELs."
  (make-instance 'dns-name :name (coerce lst 'vector)))


(defmethod make-dns-name ((str string))
  (loop with result = nil
        with label-start = nil
        for c across str
        for i from 0
        do (cond ((char= c #\.)
                  (when label-start
                    (push (make-dns-label (subseq str label-start i)) result)
                    (setf label-start nil)))
                 (t (unless label-start
                      (setf label-start i))))
        finally (when label-start
                  (push (make-dns-label (subseq str label-start (1+ i)))
                        result))
                (when (> (length result) 255)
                  (error "DNS-NAME cannot be longer than 255 octets"))
                (return (make-instance 'dns-name
                                    :name (coerce (reverse result) 'vector)))))


;; This is pretty horrible.
(defmethod serialize ((obj dns-name))
  (loop with result = nil
        for label across (name obj)
        do (setf result (concatenate 'vector result (serialize label)))
        finally (return (concatenate 'list result #(0)))))


(defmethod to-string ((obj dns-name))
  (format nil "~{~A~^.~}." (loop for label across (name obj)
                                collect (to-string label))))


;;; ### dns-question-section

(defclass dns-question-section ()
  ((qname  :initarg :qname  :reader qname  :type dns-name)
   (qtype  :initarg :qtype  :reader qtype  :type (integer 0 65535))
   (qclass :initarg :qclass :reader qclass :type (integer 0 65535))
   (raw    :initarg :raw    :reader raw    :type (vector (unsigned-byte 8))
           :initform nil)))


(defmethod print-object ((obj dns-question-section) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "QNAME=~A QTYPE=~A QCLASS=~A"
            (to-string (qname obj))
            (dns-type (qtype obj))
            (dns-class (qclass obj)))))


(defmethod serialize ((obj dns-question-section))
  (if (raw obj)
      (coerce (raw obj) 'list)
      (append (serialize (qname obj))
              (int-to-16bit (qtype obj))
              (int-to-16bit (qtype obj)))))


(defun make-dns-question-section (dns-message &optional (offset 12))
  (multiple-value-bind (qname new-offset)
      (parse-dns-name dns-message offset)
    (make-instance 'dns-question-section
                   :qname (make-dns-name qname)
                   :qtype  (+ (ash (elt dns-message (+ new-offset 0)) 8)
                                   (elt dns-message (+ new-offset 1)))
                   :qclass (+ (ash (elt dns-message (+ new-offset 2)) 8)
                                   (elt dns-message (+ new-offset 3)))
                   :raw (subseq dns-message offset (+ new-offset 4)))))


;;; ### dns-resource-record

;; Why `rtype` and `rclass` instead of the RFC names?  Unfortunately `type` and
;; `class` collide with built-in Common Lisp names and this is the easiest
;; solution.  Perhaps in the future I'll shadow the names, but that's usually
;; confusing and / or annoying.
(defclass dns-resource-record ()
  ((name     :initarg :name     :reader name     :type dns-name)
   (rtype    :initarg :rtype    :reader rtype    :type (integer 0 65535))
   (rclass   :initarg :rclass   :reader rclass   :type (integer 0 65535))
   ;; FIXME RFC 1035: "TTL: positive values of a signed 32 bit number."
   (ttl      :initarg :ttl      :reader ttl      :type (integer 0 4294967295))
   (rdlength :initarg :rdlength :reader rdlength :type (integer 0 65535))
   (rdata    :initarg :rdata    :reader rdata    :type (vector (unsigned-byte 8)))
   (raw      :initarg :raw      :reader raw      :type (vector (unsigned-byte 8))
             :initform nil)))


(defmethod print-object ((obj dns-resource-record) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "NAME=~A TYPE=~A"
            (to-string (name obj))
            (dns-type (rtype obj)))))


(defmethod serialize ((obj dns-resource-record))
  (if (raw obj)
      (coerce (raw obj) 'list)
      (append (serialize (name obj))
              (int-to-16bit (rtype obj))
              (int-to-16bit (rclass obj))
              (int-to-32bit (ttl obj))
              (int-to-16bit (rdlength obj))
              (coerce (rdata obj) 'list))))


(defun class-for-rr (rr-type)
  (case rr-type
    (:a        'dns-rr-a)
    (:aaaa     'dns-rr-aaaa)
    (:any      'dns-rr-any)
    (:axfr     'dns-rr-axfr)
    (:caa      'dns-rr-caa)
    (:cname    'dns-rr-cname)
    (:dnskey   'dns-rr-dnskey)
    (:ds       'dns-rr-ds)
    (:ixfr     'dns-rr-ixfr)
    (:mx       'dns-rr-mx)
    (:naptr    'dns-rr-naptr)
    (:ns       'dns-rr-ns)
    (:nsec     'dns-rr-nsec)
    (:nsec3    'dns-rr-nsec3)
    (:opt      'dns-rr-opt)
    (:ptr      'dns-rr-ptr)
    (:rrsig    'dns-rr-rrsig)
    (:soa      'dns-rr-soa)
    (:srv      'dns-rr-srv)
    (:txt      'dns-rr-txt)
    (otherwise 'dns-resource-record)))


(defun make-dns-resource-record (dns-message &optional (offset 12))
  (multiple-value-bind (name new-offset)
      (parse-dns-name dns-message offset)
    (let* ((type     (+ (ash (elt dns-message (+ new-offset 0))  8)
                             (elt dns-message (+ new-offset 1))))
           (class    (+ (ash (elt dns-message (+ new-offset 2))  8)
                             (elt dns-message (+ new-offset 3))))
           (ttl      (+ (ash (elt dns-message (+ new-offset 4)) 24)
                        (ash (elt dns-message (+ new-offset 5)) 16)
                        (ash (elt dns-message (+ new-offset 6))  8)
                             (elt dns-message (+ new-offset 7))))
           (rdlength (+ (ash (elt dns-message (+ new-offset 8))  8)
                             (elt dns-message (+ new-offset 9)))))
      (make-instance (class-for-rr (dns-type type)) :name (make-dns-name name)
                     :rtype type :rclass class :ttl ttl :rdlength rdlength
                     :rdata (if (and (= type 2)
                                     (= class 1))
                                (parse-dns-name dns-message (+ new-offset 10))
                                (subseq dns-message (+ new-offset 10)
                                        (+ new-offset 10 rdlength)))
                     :raw (subseq dns-message offset (+ new-offset 10 rdlength))))))


;;; ### dns-message

(defclass dns-message ()
  ((header     :initarg :header     :reader header     :type dns-header)
   (questions  :initarg :questions  :reader questions  :type (list dns-question-section)
               :initform nil)
   (answers    :initarg :answers    :reader answers    :type (list dns-resource-record)
               :initform nil)
   (records    :initarg :records    :reader records    :type (list dns-resource-record)
               :initform nil)
   (additional :initarg :additional :reader additional :type (list dns-resource-record)
               :initform nil)))


;; Don't really know what to print here.
(defmethod print-object ((obj dns-message) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~D QR=~A QNAME=~A"
            (id (header obj))
            (dns-qr (qr (header obj)))
            (when (questions obj)
              (to-string (qname (first (questions obj))))))))


(defun make-dns-message (dns-message)
  (let ((offset 12)
        (header (make-dns-header dns-message)))
    (make-instance 'dns-message
                   :header header
                   :questions (loop for i from 0 below (qdcount header)
                                    for qs = (make-dns-question-section
                                              dns-message offset)
                                    collect qs
                                    do (incf offset (length (raw qs))))
                   :answers (loop for i from 0 below (ancount header)
                                  for rr = (make-dns-resource-record
                                            dns-message offset)
                                  collect rr
                                  do (incf offset (length (raw rr))))
                   :records (loop for i from 0 below (nscount header)
                                  for rr = (make-dns-resource-record
                                            dns-message offset)
                                  collect rr
                                  do (incf offset (length (raw rr))))
                   :additional (loop for i from 0 below (arcount header)
                                     for rr = (make-dns-resource-record
                                               dns-message offset)
                                     collect rr
                                     do (incf offset (length (raw rr)))))))


(defmethod serialize ((obj dns-message))
  (concatenate 'vector
    (serialize (header obj))          ; Header
    (loop for qs in (questions obj)   ; Question Section
          append (serialize qs))
    (loop for rr in (answers obj)     ; Answer Section
          append (serialize rr))
    (loop for rr in (records obj)     ; Authority Records Section
          append (serialize rr))
    (loop for rr in (additional obj)  ; Additional Section
          append (serialize rr))))


;;; ### Resource Record Classes

;;; ### dns-rr-a

(defclass dns-rr-a (dns-resource-record)
  ())


(defmethod print-object ((obj dns-rr-a) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "NAME=~A TYPE=~A IPv4=~A"
            (to-string (name obj))
            (dns-type (rtype obj))
            (ipv4-to-str (rdata obj)))))

;;; ### dns-rr-aaaa

(defclass dns-rr-aaaa (dns-resource-record)
  ())


(defmethod print-object ((obj dns-rr-aaaa) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "NAME=~A TYPE=~A IPv6=~A"
            (to-string (name obj))
            (dns-type (rtype obj))
            (ipv6-to-str (rdata obj)))))


;;; ### dns-rr-any

(defclass dns-rr-any (dns-resource-record)
  ())


;;; ### dns-rr-axfr

(defclass dns-rr-axfr (dns-resource-record)
  ())


;;; ### dns-rr-caa

(defclass dns-rr-caa (dns-resource-record)
  ())


;;; ### dns-rr-cname

(defclass dns-rr-cname (dns-resource-record)
  ())


;;; ### dns-rr-dnskey

(defclass dns-rr-dnskey (dns-resource-record)
  ())


;;; ### dns-rr-ds

(defclass dns-rr-ds (dns-resource-record)
  ())


;;; ### dns-rr-ixfr

(defclass dns-rr-ixfr (dns-resource-record)
  ())


;;; ### dns-rr-mx

(defclass dns-rr-mx (dns-resource-record)
  ())


;;; ### dns-rr-naptr

(defclass dns-rr-naptr (dns-resource-record)
  ())


;;; ### dns-rr-ns

(defclass dns-rr-ns (dns-resource-record)
  ())


;;; ### dns-rr-nsec

(defclass dns-rr-nsec (dns-resource-record)
  ())


;;; ### dns-rr-nsec3

(defclass dns-rr-nsec3 (dns-resource-record)
  ())


;;; ### dns-rr-opt

(defclass dns-rr-opt (dns-resource-record)
  ())


;;; ### dns-rr-ptr

(defclass dns-rr-ptr (dns-resource-record)
  ())


;;; ### dns-rr-rrsig

(defclass dns-rr-rrsig (dns-resource-record)
  ())


;;; ### dns-rr-soa

(defclass dns-rr-soa (dns-resource-record)
  ())


;;; ### dns-rr-srv

(defclass dns-rr-srv (dns-resource-record)
  ())


;;; ### dns-rr-txt

(defclass dns-rr-txt (dns-resource-record)
  ())


;;; ## Functions

;; Resources:
;;
;; - <https://tools.ietf.org/html/rfc1035>
;;
(defun get-response (dns-name &key (host "9.9.9.9") (port 53) (dns-type "A"))
  "Returns the raw buffer when querying HOST for DNS-NAME (with DNS-TYPE).
  HOST must be a string, for example: \"9.9.9.9\".
  DNS-NAME must be a string, for example: \"www.example.com\".
  PORT must be an integer, for example: 53.
  DNS-TYPE must be a string, for example: \"aaaa\"."
  (when *verbose* (format *debug-io* "Connecting to ~A:~D...~%" host port))
  (let* (;; If not supplied everything defaults to 0, except ID which is a
         ;; random number between 0 and 65535.  (See DNS-HEADER class.)
         (header (make-instance 'dns-header :rd 1 :qdcount 1))
         (question (make-instance 'dns-question-section
                    :qname (make-dns-name dns-name)
                    :qtype (dns-type (intern (string-upcase dns-type) :keyword))
                    :qclass 1))
         (packet (coerce (append (serialize header) (serialize question))
                         '(vector (unsigned-byte 8))))
         (socket (usocket:socket-connect host port :protocol :datagram
                                         :timeout 5))
         buffer)
    (unwind-protect
        (progn (when *verbose*
                 (format *debug-io*
                         "~&Connected...~%Sending ~S~%Sending data...~%"
                         packet))
               (usocket:socket-send socket packet (length packet))
               (when *verbose*
                 (format *debug-io* "~&Waiting for response...~%"))
               (setf buffer (usocket:socket-receive socket nil 1500)))
      (progn (when *verbose* (format *debug-io* "~&Done. Closing socket...~%"))
             (usocket:socket-close socket)))
    buffer))


;; FIXME Recursive, a carefully crafted response (cyclic pointers) can run
;;       this into the ground.
;;       A bandaid would be keeping a DEPTH parameter and just stopping at
;;       a certain point (4, 16, 256?).
(defun parse-dns-name (dns-message &optional (offset 0))
  (let ((length (elt dns-message offset)))
    (cond ;; end of name
          ((= length 0)
           (values nil
                   (+ offset 1)))
          ;; message compression: pointer to (part of) name
          ((= (logand (elt dns-message offset) #b11000000)
              #b11000000)
           (multiple-value-bind (name)
               (parse-dns-name dns-message
                  (+ (ash (logand (elt dns-message (+ offset 0)) #b00111111) 8)
                                  (elt dns-message (+ offset 1))))
             (values name
                     (+ offset 2))))
          ;; normal qname label
          (t
           (multiple-value-bind (next-name next-offset)
               (parse-dns-name dns-message (+ offset 1 length))
             (values
              (append (list (make-dns-label (subseq dns-message (+ offset 1)
                                                    (+ offset 1 length))))
                      next-name)
              next-offset))))))
