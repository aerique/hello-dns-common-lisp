;;;;- tauth.lisp
;;;;-
;;;; # Teachable Authoritative Server in Common Lisp
;;;;
;;;; Inspiration: <https://github.com/ahupowerdns/hello-dns/blob/master/tdns/tauth.cc>
;;;;
;;;; Usage:
;;;;
;;;; - `sbcl --noinform --load tauth.lisp --eval "(main :port 5300)"`  
;;;;   (`:port` defaults to 53 (DNS) for which one needs root permissions)
;;;;
;;;; ## To Do
;;;;
;;;; Once `tauth.lisp` kinda works use it with PDNS Recursor and return
;;;; specially crafted responses with message compression pointers pointing
;;;; to themself and see how the Recursor responds.

;;; ## Packages

(load "common.lisp")

;; Prevent compiler warnings about undefined functions.
(defun process-question (buffer) (declare (ignore buffer)))

;;; ## Globals

(defvar *n_queries* 0)

(defparameter normal
  #(;; Header
    34 61  ; ID
    132 0  ; QR Opcode AA TC RD RA Z RCODE
    0 1    ; QDCOUNT
    0 1    ; ANCOUNT
    0 0    ; NSCOUNT
    0 0    ; ARCOUNT
    ;; Question Section
    3 119 119 119
    8 112 111 119 101 114 100 110 115
    3 99 111 109
    0
    0 1  ; QTYPE
    0 1  ; QCLASS
    ;; Answer Section
    3 119 119 119
    8 112 111 119 101 114 100 110 115
    3 99 111 109
    0
    0 1         ; TYPE
    0 1         ; CLASS
    0 1 81 128  ; TTL
    0 4         ; RDLENGTH
    1 2 3 4))   ; RDATA

(defparameter crafted
  #(;; Header
    34 61  ; ID
    132 0  ; QR Opcode AA TC RD RA Z RCODE
    0 1    ; QDCOUNT
    0 1    ; ANCOUNT
    0 0    ; NSCOUNT
    0 0    ; ARCOUNT
    ;; Question Section
    3 119 119 119
    8 112 111 119 101 114 100 110 115
    3 99 111 109
    0
    0 1  ; QTYPE
    0 1  ; QCLASS
    ;; Answer Section
    3 119 119 119
    192 38
    0 1         ; TYPE
    0 1         ; CLASS
    0 1 81 128  ; TTL
    0 4         ; RDLENGTH
    1 2 3 4))   ; RDATA

(defparameter crafted2
  #(;; Header
    34 61  ; ID                              -  0
    132 0  ; QR Opcode AA TC RD RA Z RCODE   -  2
    0 1    ; QDCOUNT                         -  4
    0 1    ; ANCOUNT                         -  6
    0 0    ; NSCOUNT                         -  8
    0 0    ; ARCOUNT                         - 10
    ;; Question Section
    3 119 119 119                          ; - 12
    8 112 111 119 101 114 100 110 115      ; - 16
    3 99 111 109                           ; - 25
    0                                      ; - 29
    0 1  ; QTYPE                           ; - 30
    0 1  ; QCLASS                          ; - 32
    ;; Answer Section
    ;3 119 119 119                          ; - 34
    ;8 112 111 119 101 114 100 110 115      ; - 38
    ;3 99 111 109                           ; - 47
    ;0                                      ; - 51
    192 34
    0 1         ; TYPE                     ; - 52
    0 1         ; CLASS
    0 1 81 128  ; TTL
    0 4         ; RDLENGTH
    1 2 3 4))   ; RDATA


;;; ## Handlers

(defun udp-handler (buffer)
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  ;(format t "=== new dns message (~D bytes) ===~%~S~%" (length buffer) buffer)
  (process-question buffer))


;;; ## Functions

;; Move some of this stuff to UDP-HANDLER.  (Doing it inside RESOLVE for now
;; because I got some kind of threading error when using QUIT inside the
;; handler.)
(defun process-question (buffer)
  (format t "[~D] ~S~%" (incf *n_queries*) (parse-dns-message buffer))
  ;; Always send back CRAFTED.
  (let ((out (coerce crafted2 '(vector (unsigned-byte 8)))))
    (setf (elt out 0) (elt buffer 0)
          (elt out 1) (elt buffer 1))
    out))


;;; ## Main Program

;; As opposed to C/C++ `main` is just an arbitrary name here.
(defun main (&key (listen-address "127.0.0.1") (port 53))
  (format t "tauth listening on ~A:~D (UDP)...~%" listen-address port)
  (handler-case
      (usocket:socket-server listen-address port #'udp-handler nil
                             :protocol :datagram :max-buffer-size 1500)
    (sb-sys:interactive-interrupt (e) (user-interrupt e)))  ; FIXME SBCL dep
  (quit))
