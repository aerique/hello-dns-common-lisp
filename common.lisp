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


;; Silence compiler about undefined functions.
;; (Common Lisp has many warts: this is one of them.)
(defun int-to-16bit (a &key b) (declare (ignore a b)))
(defun int-to-32bit (a &key b) (declare (ignore a b)))
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
;;;
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
  (make-instance 'dns-label :label octets))


(defmethod make-dns-label ((string string))
  (make-instance 'dns-label :label (string-to-octets string)))


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
                (return (make-instance 'dns-name :name (coerce (reverse result) 'vector)))))


(defmethod serialize ((obj dns-name))
  (loop with result = nil
        for label across (name obj)
        do (setf result (concatenate 'vector result (serialize label)))
        finally (return (concatenate 'list result #(0)))))


;;; ### dns-message
;;;
;;; #(90 26 1 32 0 1 0 0 0 0 0 1 3 119 119 119 8 112 111 119 101 114 100 110 115 3
;;;   99 111 109 0 0 1 0 1 0 0 41 16 0 0 0 0 0 0 12 0 10 0 8 166 243 81 137 106 22
;;;   124 116)
;;; (:ID 23066 :QR :QUERY :OPCODE :QUERY :AA NIL :TC NIL :RD T :RA NIL :Z 2 :RCODE
;;;  :NOERROR :QDCOUNT 1 :ANCOUNT 0 :NSCOUNT 0 :ARCOUNT 1 :QUESTION-SECTION
;;;  ((:QNAME www.powerdns.com :QTYPE :A :QCLASS :IN)) :ANSWER-SECTION NIL
;;;  :RECORD-SECTION NIL :ADDITIONAL-SECTION
;;;  ((:NAME  :TYPE :OPT :CLASS NIL :TTL 0 :RDLENGTH 12 :RDATA
;;;    #(0 10 0 8 166 243 81 137 106 22 124 116)))
;;;  :MESSAGE-SIZE 57)
;;; (make-instance 'dns-message :id 23066 :qr :query :opcode :query :aa nil :tc nil :rd t :ra nil :z 2 :rcode :noerror :qdcount 1 :ancount 0 :nscount 0 :arcount 1 :questions (list (list :qname (make-dns-name "www.powerdns.com") :qtype :a :qclass :in)))
;;;
;;; (make-instance 'dns-message :id 8765 :qr :answer :opcode :query :aa t :tc nil :rd nil :ra nil :z 0 :rcode :noerror :qdcount 1 :ancount 1 :nscount 0 :arcount 0 :questions (list (list :qname (make-dns-name "www.powerdns.com") :qtype :a :qclass :in)) :answers (list (list :name (make-dns-name "www.powerdns.com") :type :a :class :in :ttl 86400 :rdlength 4 :rdata #(1 2 3 4))))

(defclass dns-message ()
  ((id      :initarg :id      :reader id      :type (integer 0 65535)
            :initform (random 65536))
   (qr      :initarg :qr      :reader qr      :type (or :query :response))
   (opcode  :initarg :opcode  :reader opcode  :type keyword)
   (aa      :initarg :aa      :reader aa      :type boolean)
   (tc      :initarg :tc      :reader tc      :type boolean)
   (rd      :initarg :rd      :reader rd      :type boolean)
   (ra      :initarg :ra      :reader ra      :type boolean)
   (z       :initarg :z       :reader z       :initform 0)
   (rcode   :initarg :rcode   :reader rcode   :type keyword)
   (qdcount :initarg :qdcount :reader qdcount :type (integer 0 65535))
   (ancount :initarg :ancount :reader ancount :type (integer 0 65535))
   (nscount :initarg :nscount :reader nscount :type (integer 0 65535))
   (arcount :initarg :arcount :reader arcount :type (integer 0 65535))
   (questions  :initarg :questions  :reader questions  :initform ())
   (answers    :initarg :answers    :reader answers    :initform ())
   (records    :initarg :records    :reader records    :initform ())
   (additional :initarg :additional :reader additional :initform ())))


(defmethod serialize ((obj dns-message))
  (concatenate 'vector
    (int-to-16bit (id obj))                       ; ID
    (list (+ (if (eq (qr obj) :query) 0 128)      ; QR
             (case (opcode obj)                   ; OPCODE
               (:query   0)
               (:iquery  8)
               (:status 16)
               (otherwise (error "Illegal OPCODE: ~S" (opcode obj))))
             (if (aa obj) 4 0)                    ; AA
             (if (tc obj) 2 0)                    ; TC
             (if (rd obj) 1 0))                   ; RD
          (+ (if (ra obj) 128 0)                  ; RA
             (ash (z obj) 4)))                    ; Z
    (int-to-16bit (qdcount obj))                  ; QDCOUNT
    (int-to-16bit (ancount obj))                  ; ANCOUNT
    (int-to-16bit (nscount obj))                  ; NSCOUNT
    (int-to-16bit (arcount obj))                  ; ARCOUNT
    (loop for q in (questions obj)                ; Question Section
          append (serialize-question-section q))
    (loop for rr in (answers obj)                 ; Answer Section
          append (serialize-resource-record rr))
    (loop for rr in (records obj)                 ; Authority Records Section
          append (serialize-resource-record rr))
    (loop for rr in (additional obj)              ; Additional Section
          append (serialize-resource-record rr))))


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


(defun parse-dns-header (response)
  (values (list :id (+ (ash (elt response  0) 8) (elt response  1))
                :qr (if (= 0 (ash (logand (elt response 2) #b10000000) -7))
                        :query
                        :response)
                :opcode (case (ash (logand (elt response 2) #b01111000) -3)
                          (0 :query)
                          (1 :iquery)
                          (2 :status)
                          (otherwise nil))
                :aa (= 0 (ash (logand (elt response 2) #b00000100) -2))
                :tc (= 0 (ash (logand (elt response 2) #b00000010) -1))
                :rd (= 0 (logand (elt response 2) #b00000001))
                :ra (= 0 (ash (logand (elt response 3) #b10000000) -7))
                :z (ash (logand (elt response 3) #b01110000) -4)
                :rcode (case (logand (elt response 3) #b00001111)
                         (0 :noerror)
                         (1 :formerr)
                         (2 :servfail)
                         (3 :nxdomain)
                         (4 :notimp)
                         (5 :refused))
                :qdcount (+ (ash (elt response  4) 8) (elt response  5))
                :ancount (+ (ash (elt response  6) 8) (elt response  7))
                :nscount (+ (ash (elt response  8) 8) (elt response  9))
                :arcount (+ (ash (elt response 10) 8) (elt response 11)))
          12))


;; FIXME Recursive, a carefully crafted response (cyclic pointers) can run
;;       this into the ground.
;;       A bandaid would be keeping a DEPTH parameter and just stopping at
;;       a certain point (4, 16, 256?).
;; FIXME rename QNAME to NAME
;; FIXME rename `part` to `label`
(defun parse-qname (buffer &optional (offset 0))
  (let ((length (elt buffer offset)))
    (cond ;; end of qname
          ((= length 0)
           (values nil
                   (+ offset 1)))
          ;; message compression: pointer to (part of) qname
          ((= (logand (elt buffer offset) #b11000000)
              #b11000000)
           (multiple-value-bind (qname)
               (parse-qname buffer
                        (+ (ash (logand (elt buffer (+ offset 0)) #b00111111) 8)
                                        (elt buffer (+ offset 1))))
             (values qname
                     (+ offset 2))))
          ;; normal qname part
          (t
           (multiple-value-bind (next-name next-offset)
               (parse-qname buffer (+ offset 1 length))
             (values
              (append (list (make-dns-label (subseq buffer (+ offset 1)
                                                    (+ offset 1 length))))
                      next-name)
              next-offset))))))


(defun parse-dns-question-section (dns-message &optional (offset 12))
  "Parses DNS question section in DNS-MESSAGE at OFFSET.
  Returns plist with QNAME, QTYPE and QCLASS keys as the first value and
  the new offset as the second value."
  (multiple-value-bind (qname new-offset)
      (parse-qname dns-message offset)
    (values (list :qname (make-dns-name qname)
                  :qtype (dns-type
                           (+ (ash (elt dns-message (+ new-offset 0)) 8)
                                   (elt dns-message (+ new-offset 1))))
                  :qclass (dns-class
                           (+ (ash (elt dns-message (+ new-offset 2)) 8)
                                  (elt dns-message (+ new-offset 3)))))
            (+ new-offset 4))))


(defun parse-dns-resource-record (dns-message &optional (offset 12))
  ;(let ((*print-pretty* nil))
  ;  (format t "~S~%" (subseq dns-message offset (+ offset 24))))
  (multiple-value-bind (name new-offset)
      (parse-qname dns-message offset)
    (let* ((type  (dns-type  (+ (ash (elt dns-message (+ new-offset 0))  8)
                                     (elt dns-message (+ new-offset 1)))))
           (class (dns-class (+ (ash (elt dns-message (+ new-offset 2))  8)
                                     (elt dns-message (+ new-offset 3)))))
           (ttl              (+ (ash (elt dns-message (+ new-offset 4)) 24)
                                (ash (elt dns-message (+ new-offset 5)) 16)
                                (ash (elt dns-message (+ new-offset 6))  8)
                                     (elt dns-message (+ new-offset 7))))
           (rdlength         (+ (ash (elt dns-message (+ new-offset 8))  8)
                                     (elt dns-message (+ new-offset 9)))))
      (values
       (list :name (make-dns-name name)
             :type type :class class :ttl ttl :rdlength rdlength
             :rdata (if (eq type :ns)  ; FIXME also test if CLASS==:IN
                        (parse-qname dns-message (+ new-offset 10))
                        (subseq dns-message (+ new-offset 10)
                                (+ new-offset 10 rdlength))))
       (+ new-offset 10 rdlength)))))


(defun parse-dns-message (response)
  (let ((offset 12)
        (header (parse-dns-header response)))
    (append
     header
     (list :question-section (loop for i from 0 below (getf header :qdcount)
                                   for qs = (multiple-value-list
                                             (parse-dns-question-section
                                              response offset))
                                   collect (first qs)
                                   do (setf offset (second qs)))
           :answer-section (loop for i from 0 below (getf header :ancount)
                                 for an = (multiple-value-list
                                           (parse-dns-resource-record response
                                                                      offset))
                                 collect (first an)
                                 do (setf offset (second an)))
           :record-section (loop for i from 0 below (getf header :nscount)
                                 for ns = (multiple-value-list
                                           (parse-dns-resource-record response
                                                                      offset))
                                 collect (first ns)
                                 do (setf offset (second ns)))
           :additional-section (loop for i from 0 below (getf header :arcount)
                                     for ar = (multiple-value-list
                                               (parse-dns-resource-record
                                                response offset))
                                     collect (first ar)
                                     do (setf offset (second ar)))
           :message-size offset))))


;;- FIXME remove: not used
(defun ip2str (ip)
  (cond ((= (length ip) 4)
         (format nil "~D.~D.~D.~D"
                 (elt ip 0) (elt ip 1) (elt ip 2) (elt ip 3)))
        ((= (length ip) 8)
         (string-downcase (format nil "~X:~X:~X:~X:~X:~X:~X:~X"
                                 (elt ip 0) (elt ip 1) (elt ip 2) (elt ip 3)
                                 (elt ip 4) (elt ip 5) (elt ip 6) (elt ip 7))))
        (t (error "Invalid IP: ~S" ip))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
