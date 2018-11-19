;;;;- common.lisp

;;; ## Packages
;;;
;;; Assumes [Quicklisp](https://www.quicklisp.org/beta/) is installed.

;; https://github.com/fare/command-line-arguments
;(ql:quickload :command-line-arguments)

;; [usocket](https://github.com/usocket/usocket#introduction) is a
;; socket library for many Common Lisp implementations.
(ql:quickload :usocket)
(ql:quickload :usocket-server)


;;; ## Globals

(defvar *verbose* nil)

(defparameter testmsg
  (coerce #(90 26 1 32 0 1 0 0 0 0 0 1 3 119 119 119 8 112 111 119 101 114 100
            110 115 3 99 111 109 0 0 1 0 1 0 0 41 16 0 0 0 0 0 0 12 0 10 0 8
            166 243 81 137 106 22 124 116)
          '(vector (unsigned-byte 8))))


;; Silence compiler about undefined functions.
;; (Common Lisp has many warts: this is one of them.)
(defun int-to-16bit (a &key b) (declare (ignore a b)))
(defun int-to-32bit (a &key b) (declare (ignore a b)))
(defun parse-dns-name (a &optional b) (declare (ignore a b)))
(defun serialize-question-section (a) (declare (ignore a)))
(defun serialize-resource-record (a) (declare (ignore a)))


;;; ## Handlers

(defun user-interrupt (arg)
  (declare (ignore arg))
  (format t "~&User interrupt. Aborting...~%")
  (exit))


;;; ## System Class Methods
;;;
;;; These are methods for classes that already exist in Common Lisp, so don't
;;; go hunting around the Hello DNS sources looking for the, for example,
;;; INTEGER class.

;; - https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml
;;   - winging it here
(defmethod dns-class ((class integer))
  (case class
    (  1 :in)
    (  2 :cs)  ; obsolete, sometimes used in examples
    (  3 :ch)
    (  4 :hs)
    (254 :none)
    (255 :any)
    ;; being explicit
    (otherwise nil)))


(defmethod dns-opcode ((opcode integer))
  (case opcode
    (0 :query)
    (2 :status)
    (4 :notify)
    (5 :update)
    (otherwise nil)))


(defmethod dns-test ((opcode integer))
  (case opcode
    (0 :query)
    (2 :status)
    (4 :notify)
    (5 :update)
    (otherwise nil)))


(defmethod dns-rcode ((rcode integer))
  (case rcode
    ( 0 :noerror)
    ( 1 :formerr)
    ( 2 :servfail)
    ( 3 :nxdomain)
    ( 4 :notimp)
    ( 5 :refused)
    ( 6 :yxdomain)
    ( 7 :yxrrset)
    ( 8 :nxrrset)
    ( 9 :notauth)
    (10 :notzone)
    ;; WTF
    ;(16 :badvers)
    ;(16 :badsig)
    (17 :badkey)
    (18 :badtime)
    (19 :badmode)
    (20 :badname)
    (21 :badalg)
    (22 :badtrunc)
    (23 :badcookie)
    (otherwise nil)))


;;; - https://en.wikipedia.org/wiki/List_of_DNS_record_types
;;;   - obsolete record types have been skipped
(defmethod dns-type ((type integer))
  (case type
    ;; resource records
    (    1 :a)
    (    2 :ns)
    (    5 :cname)
    (    6 :soa)
    (   12 :ptr)
    (   15 :mx)
    (   16 :txt)
    (   17 :rp)
    (   18 :afsdb)
    (   24 :sig)
    (   25 :key)
    (   28 :aaaa)
    (   29 :loc)
    (   33 :srv)
    (   35 :naptr)
    (   36 :kx)
    (   37 :cert)
    (   39 :dname)
    (   42 :apl)
    (   43 :ds)
    (   44 :sshfp)
    (   45 :ipseckey)
    (   46 :rrsig)
    (   47 :nsec)
    (   48 :dnskey)
    (   49 :dhcid)
    (   50 :nsec3)
    (   51 :nsec3param)
    (   52 :tlsa)
    (   53 :smimea)
    (   55 :hip)
    (   59 :cds)
    (   60 :cdnskey)
    (   61 :openpgpkey)
    (  249 :tkey)
    (  250 :tsig)
    (  256 :uri)
    (  257 :caa)
    (32768 :ta)
    (32769 :dlv)
    ;; other types and pseudo resource records
    (   41 :opt)
    (  251 :ixfr)
    (  252 :axfr)
    ;(  255 :*)
    (  255 :any)
    ;; being explicit
    (otherwise nil)))


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
(defmethod print-object ((obj dns-header) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID=~D QR=~A OPCODE=~A AA=~S TC=~S RD=~S RA=~S Z=~D ~
                    RCODE=~A QDCOUNT=~D ANCOUNT=~D NSCOUNT=~D ARCOUNT=~D"
            (id obj)
            (if (= 0 (qr obj))
                "QUERY"
                "RESPONSE")
            (case (opcode obj)
              (0 "QUERY")
              (1 "IQUERY")
              (2 "STATUS")
              (3 "Unassigned")
              (4 "NOTIFY")
              (5 "UPDATE")
              (otherwise (format nil"Unassigned(~D)" (opcode obj))))
            (= 1 (aa obj))
            (= 1 (tc obj))
            (= 1 (rd obj))
            (= 1 (ra obj))
            (z obj)
            (case (rcode obj)
              ( 0 "NoError")
              ( 1 "FormatErr")
              ( 2 "ServFail")
              ( 3 "NXDomain")
              ( 4 "NotImp")
              ( 5 "Refused")
              ( 6 "YXDomain")
              ( 7 "YXRRSet")
              ( 8 "NXRRSet")
              ( 9 "NotAuth")
              (10 "NotZone")
              (otherwise (format nil "Unassigned(~D)" (rcode obj))))
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


(defun make-dns-header (response)
  (make-instance 'dns-header
                 :id (+ (ash (elt response 0) 8) (elt response 1))
                 :qr     (ash (logand (elt response 2) #b10000000) -7)
                 :opcode (ash (logand (elt response 2) #b01111000) -3)
                 :aa     (ash (logand (elt response 2) #b00000100) -2)
                 :tc     (ash (logand (elt response 2) #b00000010) -1)
                 :rd          (logand (elt response 2) #b00000001)
                 :ra    (ash (logand (elt response 3) #b10000000) -7)
                 :z     (ash (logand (elt response 3) #b01110000) -4)
                 :rcode      (logand (elt response 3) #b00001111)
                 :qdcount (+ (ash (elt response  4) 8) (elt response  5))
                 :ancount (+ (ash (elt response  6) 8) (elt response  7))
                 :nscount (+ (ash (elt response  8) 8) (elt response  9))
                 :arcount (+ (ash (elt response 10) 8) (elt response 11))))


;;; ### dns-label
;;;
;;; To Do:
;;;
;;; - check for min size 1 and max size 63
;;; - parse input LABELs to valid DNS labels

(defclass dns-label ()
  ((label :initarg :label :reader label :type (vector (unsigned-byte 8))
          :initform (error "Must supply :LABEL argument to DNS-LABEL class."))))


(defmethod print-object ((object dns-label) stream)
  (format stream "~A" (octets-to-string (label object))))


(defmethod make-dns-label ((octets vector))
  (when (> (length octets) 63)
    (error "DNS-LABEL cannot be longer than 63 octets"))
  (make-instance 'dns-label :label octets))


(defmethod make-dns-label ((string string))
  (make-dns-label (string-to-octets string)))


(defmethod serialize ((obj dns-label))
  (concatenate 'vector (vector (length (label obj))) (label obj)))


;;; ### dns-name

(defclass dns-name ()
  ((name :initarg :name :reader name :type (vector dns-label)
         :initform (error "Must supply :NAME argument to DNS-NAME class."))))


(defmethod print-object ((object dns-name) stream)
  (format stream "~{~A~^.~}" (coerce (name object) 'list)))


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
                  (push (make-dns-label (subseq str label-start (1+ i))) result))
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


;;; ### dns-question-section

(defclass dns-question-section ()
  ((qname  :initarg :qname  :reader qname  :type dns-name)
   (qtype  :initarg :qtype  :reader qtype  :type (integer 0 65535))
   (qclass :initarg :qclass :reader qclass :type (integer 0 65535))
   (raw    :initarg :raw    :reader raw    :type (vector (unsigned-byte 8))
           :initform nil)))


(defmethod print-object ((obj dns-question-section) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "QNAME=~S QTYPE=~A QCLASS=~A"
            (qname obj)
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
;; solution.  Perhaps in the future I'll shadow the names.
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
    (format stream "NAME=~S TYPE=~A CLASS=~A TTL=~D"
            (name obj)
            (dns-type (rtype obj))
            (dns-class (rclass obj))
            (ttl obj))))


(defmethod serialize ((obj dns-resource-record))
  (if (raw obj)
      (coerce (raw obj) 'list)
      (append (serialize (name obj))
              (int-to-16bit (rtype obj))
              (int-to-16bit (rclass obj))
              (int-to-32bit (ttl obj))
              (int-to-16bit (rdlength obj))
              (coerce (rdata obj) 'list))))


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
      (make-instance 'dns-resource-record :name (make-dns-name name) :rtype type
                     :rclass class :ttl ttl :rdlength rdlength
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
    (format stream "ID=~D QNAME=~A"
            (id (header obj))
            (when (questions obj)
              (qname (first (questions obj)))))))


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
                                  for rr = (make-dns-resource-record dns-message
                                                                     offset)
                                  collect rr
                                  do (incf offset (length (raw rr))))
                   :records (loop for i from 0 below (nscount header)
                                  for rr = (make-dns-resource-record dns-message
                                                                     offset)
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


;;; Functions

;; ASH: arithmetic (binary) shift towards most significant bit
;; XXX this hasn't actually been tested with anything else than 1
(defun int-to-16bit (integer &key (big-endian t))
  (if big-endian
      (list (ash    integer -8)
            (logand integer #b0000000011111111))
      ;; little-endian
      (list (logand integer #b0000000011111111)
            (ash    integer -8))))


;; XXX this hasn't actually been tested with anything else than 1
(defun int-to-32bit (integer &key (big-endian t))
  (if big-endian
      (list (ash         integer                                     -24)
            (ash (logand integer #b0000000011111111000000000000000)  -16)
            (ash (logand integer #b00000000000000001111111100000000)  -8)
                 (logand integer #b00000000000000000000000011111111))
      ;; little-endian
      (list      (logand integer #b00000000000000000000000011111111)
            (ash (logand integer #b00000000000000001111111100000000)  -8)
            (ash (logand integer #b0000000011111111000000000000000)  -16)
            (ash         integer                                     -24))))


(defun serialize-question-section (question-section)
  (append (serialize (getf question-section :qname))
          (int-to-16bit (dns-type (getf question-section :qtype)))
          (int-to-16bit (dns-class (getf question-section :qclass)))))


(defun serialize-resource-record (resource-record)
  (append (serialize (getf resource-record :name))
          (int-to-16bit (dns-type (getf resource-record :type)))
          (int-to-16bit (dns-class (getf resource-record :class)))
          (int-to-32bit (getf resource-record :ttl))
          (int-to-16bit (getf resource-record :rdlength))
          (coerce (getf resource-record :rdata) 'list)))


(defun str2type (string)
  (dns-type (intern (string-upcase string) :keyword)))


;; Resources:
;;
;; - <https://tools.ietf.org/html/rfc1035>
;;
;; Example usage:
;;
;; - `(get-response (getf (first *hints*) :address)
;;                  (getf (first *hints*) :port) "www.google.com" nil)`
;; - `(get-response (getf (first *hints*) :address)
;;                  (getf (first *hints*) :port) "." nil)`
;;
(defun get-response (dns-name &key (host "9.9.9.9") (port 53) (dns-type "aaaa"))
  "Returns the raw buffer when querying HOST for DNS-NAME (with DNS-TYPE).
  HOST must be a string, for example: \"9.9.9.9\".
  DNS-NAME must be a string, for example: \"www.example.com\".
  PORT must be an integer, for example: 53.
  DNS-TYPE must be a string, for example: \"aaaa\"."
  (when *verbose* (format *debug-io* "Connecting to ~A:~D...~%" host port))
  (let* ((header ;; we only need to set the ID and QDCOUNT
                 (list (random 256) (random 256)  ; id
                       (+ #b00000000              ; qr
                          #b00000000              ; opcode
                          #b00000000              ; aa
                          #b00000000              ; tc
                          #b00000001)             ; rd
                       (+ #b00000000              ; ra
                          #b00000000              ; z
                          #b00000000)             ; rcode
                       0 1                        ; qd count
                       0 0                        ; an count
                       0 0                        ; ns count
                       0 0))                      ; ar count
         (question (append (serialize (make-dns-name dns-name))
                           ;; FIXME only works for QTYPEs < 256
                           (list 0 (str2type dns-type)  ; qtype  (NS)
                                 0 1)))                 ; qclass (IN)
         (socket (usocket:socket-connect host port :protocol :datagram
                                         :timeout 5))
         buffer)
    (unwind-protect
        (progn (when *verbose* (format *debug-io* "~&Connected...~%Sending ~S~%Sending data...~%" (coerce (append header question) '(vector (unsigned-byte 8)))))
               (usocket:socket-send socket (coerce (append header question)
                                                   '(vector (unsigned-byte 8)))
                                    (+ (length header) (length question)))
               (when *verbose* (format *debug-io* "~&Waiting for response...~%"))
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


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
