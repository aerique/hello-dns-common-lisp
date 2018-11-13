;;;;- tres.lisp
;;;;-
;;;; # Teachable Resolver in Common Lisp
;;;;
;;;; Inspiration: <https://powerdns.org/hello-dns/tdns/tres.md.html>
;;;;
;;;; Usage:
;;;;
;;;; - `sbcl --noinform --load tres.lisp --eval "(main :port 5300)"`  
;;;;   (`:port` defaults to 53 (DNS) for which one needs root permissions)
;;;;
;;;; So, Common Lisp is a multi-paradigm language: it doesn't tie you to a
;;;; specific programming model.  For DNS packets, messages and response we
;;;; could have used classes & methods, structs & functions, etc.
;;;;
;;;; My only style could be described as basic or simplistic so we're mainly
;;;; using functions and plists here.  Adherents of type-oriented languages
;;;; will probably have an heart attack.
;;;;
;;;; This is how I code during the discovery phase.  Once things are more set
;;;; in stone we start using classes, methods, structs, etc.  However the
;;;; current style is very friendly for REPL-oriented programming.
;;;;
;;;; ## To Do
;;;;
;;;; Once `tauth.lisp` kinda works use it with PDNS Recursor and return
;;;; specially crafted responses with message compression pointers pointing
;;;; to themself and see how the Recursor responds.

;;; ## Packages
;;;
;;; [usocket](https://github.com/usocket/usocket#introduction) is a
;;; socket library for many Common Lisp implementations.
;;;
;;; Assumes [Quicklisp](https://www.quicklisp.org/beta/) is installed.

(ql:quickload :usocket)
(ql:quickload :usocket-server)

(load "common.lisp")


;;; ## Globals

;; <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#plist>
(defparameter *hints*
  '((:name "a.root-servers.net" :address "198.41.0.4"   :port 53)
    (:name "f.root-servers.net" :address "192.5.5.241"  :port 53)
    (:name "k.root-servers.net" :address "193.0.14.129" :port 53)))

(defparameter *skip-ipv6* nil)  ; set this if you have no functioning IPv6

;; A `defvar` will not get overwritten on reload.
(defvar *n_queries* 0)


;; Silence compiler warnings.
(defun resolve (buffer)
  (declare (ignore buffer)))


;;; ## Handlers

(defun udp-handler (buffer)
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  (format t "=== new dns message (~D bytes) ===~%~S~%" (length buffer) buffer)
  (resolve buffer))
  ;(setf tmp buffer)
  ;(let ((msg (parse-dns-message buffer)))
  ;  (format t "--- parsed message ---~%~S~%" msg)
  ;  (format t "--- resolving ---~%")
  ;  (resolve msg)))


;;; ## Common Functions

;; FIXME rename `part` to `label`
(defun make-qname (dns-name)
  (loop with result = nil
        with part = nil
        with part-length = 0
        for c across dns-name
        do ;(format t "part=~S part-length=~S~%" part part-length)
	   (cond ((char= c #\.)
		  (when part
                    (push part-length result)
                    (loop for p in (reverse part) do (push p result)))
                  (setf part nil
                        part-length 0))
                 (t (push (char-code c) part)
                    (incf part-length)))
        finally ;(format t "part=~S part-length=~S~%" part part-length)
		(when part
		  (push part-length result)
                  (loop for p in (reverse part) do (push p result)))
                (push 0 result)
                ;(return (coerce (reverse result)
		;                '(vector (unsigned-byte 8))))
                (return (reverse result))))


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
(defun get-response (host port dns-name dns-type)
  (declare (ignore dns-type))  ; for now, we're going to use it later
  (format t "Connecting to ~A:~D...~%" host port)
  (let* ((header ;; we only need to set the ID and QDCOUNT
                 (list (random 256) (random 256)  ; id
                       (+ #b00000000              ; qr
                          #b00000000              ; opcode
                          #b00000000              ; aa
                          #b00000000              ; tc
                          #b00000000)             ; rd
                       (+ #b00000000              ; ra
                          #b00000000              ; z
                          #b00000000)             ; rcode
                       0 1                        ; qd count
                       0 0                        ; an count
                       0 0                        ; ns count
                       0 0))                      ; ar count
         (question (append (make-qname dns-name)  ; qname
                           (list 0 2              ; qtype  (NS)
                                 0 1)))           ; qclass (IN)
         (socket (usocket:socket-connect host port :protocol :datagram
                                         :timeout 5))
         buffer)
    (unwind-protect
        (progn (format t "~&Connected...~%Sending data...~%")
               (usocket:socket-send socket (coerce (append header question)
                                                   '(vector (unsigned-byte 8)))
                                    (+ (length header) (length question)))
               (format t "~&Waiting for response...~%")
               (setf buffer (usocket:socket-receive socket nil 1500)))
      (progn (format t "~&Done. Closing socket...~%")
             (usocket:socket-close socket)))
    buffer))


;;; ## Functions



;; Move some of this stuff to UDP-HANDLER.  (Doing it inside RESOLVE for now
;; because I got some kind of threading error when using QUIT inside the
;; handler.)
(defun resolve (buffer)
  (format t "#queries: ~D~%" (incf *n_queries*))
  (length buffer)
  )


;;; ## Main Program

;; As opposed to C/C++ `main` is just an arbitrary name here.
(defun main (&key (listen-address "127.0.0.1") (port 53))
  (format t "tres listening on ~A:~D (UDP)...~%" listen-address port)
  (handler-case
      (usocket:socket-server listen-address port #'udp-handler nil
                             :protocol :datagram :max-buffer-size 1500)
    (sb-sys:interactive-interrupt (e) (user-interrupt e)))  ; FIXME SBCL dep
  (quit))
