;;;;- common.lisp
;;;;
;;;; Some functions to make Common Lisp life easier and aren't really related
;;;; to the Hello DNS C++ version.

;;; ## Functions

;; At the top since it can be used anywhere.
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


;; ASH: arithmetic (binary) shift towards most significant bit
(defun int-to-2-bytes (integer &key (big-endian t))
  (if big-endian
      (list (ash    integer -8)
            (logand integer #b0000000011111111))
      ;; little-endian
      (list (logand integer #b0000000011111111)
            (ash    integer -8))))


(defun int-to-4-bytes (integer &key (big-endian t))
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


(defun ipv4-to-str (ipv4)
  (format nil "~D.~D.~D.~D"
          (elt ipv4 0) (elt ipv4 1) (elt ipv4 2) (elt ipv4 3)))


;; https://en.wikipedia.org/wiki/IPv6_address#Recommended_representation_as_text
;;
;; Test cases:
;; - #(32 1 13 184 0 0 0 0 0 0 0 0 0 2 0 1) → "2001:db8::2:1"
;; - #(32 1 13 184 0 0 0 1 0 1 0 1 0 1 0 1) → "2001:db8:0:1:1:1:1:1"
;; - #(32 1 13 184 0 0 0 0 0 1 0 0 0 0 0 1) → "2001:db8::1:0:0:1"
;;
;; These test cases work but there's still issues with this function.  Off the
;; top of my head with all-zeroes at the end, but I do not know what the rules
;; are in that case.  We need to find an official list of test cases.
(defun ipv6-to-str (ipv6)
  (let* (;; make eight groups of 16-bit fields
         (groups (loop for i from 0 to 15 by 2
                       for field = (+ (ash (elt ipv6 i) 8)
                                      (elt ipv6 (+ i 1)))
                       collect (string-downcase (format nil "~X" field))))
         ;; find longest all-zero sequence
         (zeros (loop with longest-begin  = nil
                      with longest-length = nil
                      with current-begin  = nil
                      for group in groups
                      for i from 0
                      do (cond (;; If we come across the first "0" (first
                                ;; because CURRENT-BEGIN is NIL) we set
                                ;; CURRENT-BEGIN to the index of the first "0".
                                (and (string= group "0")
                                     (not current-begin))
                                (setf current-begin i))
                               (;; If we come across something that is not a
                                ;; "0" AND the previous group was "0" (because
                                ;; CURRENT-BEGIN was set) we do one of the
                                ;; following things:
                                (and (string/= group "0")
                                     current-begin)
                                (let ((len (- i current-begin)))
                                  ;(format t "current-begin=~S len=~S~%"
                                  ;        current-begin len)
                                  (if longest-length
                                      ;; If we've already find a long all-zero
                                      ;; sequence we compare them and if the
                                      ;; current one if longer we overwrite
                                      ;; the settings.
                                      (when (> len longest-length)
                                        (setf longest-begin  current-begin
                                              longest-length len))
                                      ;; If we haven't seen an all-zero
                                      ;; sequence AND the current one is longer
                                      ;; than 1 we set it as the longest all-
                                      ;; zero sequence.
                                      (when (> len 1)
                                        (setf longest-begin  current-begin
                                              longest-length len)))
                                  ;; Unset CURRENT-BEGIN since we're not in
                                  ;; an all-zero sequence anymore.
                                  (setf current-begin nil))))
                      finally (return (when (and longest-begin longest-length)
                                        (list :begin longest-begin
                                              :end (+ -1 longest-begin
                                                      longest-length)))))))
    ;(format t "groups=~S~%zeros=~S~%" groups zeros)
    (when zeros
      (setf groups (append (subseq groups 0 (getf zeros :begin))
                           (list "::")
                           (subseq groups (+ (getf zeros :end) 1)))))
    (loop with result = (first groups)
          with prev = nil
          for group in (rest groups)
          do (setf result (if (or (string= prev "::")
                                  (string= group "::"))
                              (mkstr result group)
                              (mkstr result ":" group)))
             (setf prev group)
          finally (return result))))


(defun ip-to-str (ip)
  (case (length ip)
    ( 4 (ipv4-to-str ip))
    (16 (ipv6-to-str ip))
    (otherwise (format *debug-io* "[ERROR] unknown IP length: ~D" (length ip))
               #|(format *log* "[ERROR] unknown IP length ~D for ~S~%"
                       (length ip) ip)|#)))
