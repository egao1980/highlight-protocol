;;;; JSON (and JSONC: comments are allowed).
;;;;
;;;;   whitespace :plain;  // … end of line and /* … */ :comment
;;;;   "…" (\ escapes) ends at its quote or at the end of the line: :property
;;;;   when the next non-whitespace character is :, else :string
;;;;   -? digits (. digits)? (e [+-]? digits)? :number
;;;;   true false null :constant; other words :plain
;;;;   { } [ ] , : :punctuation; anything else :plain
;;;;
;;;; This lexes text for display; it does not parse JSON (json-protocol does).

(in-package #:highlight-protocol)

(defclass json-lexer (lexer) ()
  (:documentation "JSON, with // and /* */ comments tolerated."))

(defun %json-string-end (text pos)
  (let ((n (length text)))
    (loop with i = (1+ pos)
          while (< i n)
          do (case (char text i)
               (#\\ (if (eql (%char-at text (1+ i)) #\Newline) (incf i) (incf i 2)))
               (#\" (return (1+ i)))
               (#\Newline (return i))
               (t (incf i)))
          finally (return n))))

(defun %json-number-end (text pos)
  "The end of a number at POS, or NIL when none starts there."
  (let ((i (if (eql (char text pos) #\-) (1+ pos) pos)))
    (when (digit-char-p (or (%char-at text i) #\x))
      (setf i (%skip #'digit-char-p text i))
      (when (and (eql (%char-at text i) #\.) (digit-char-p (or (%char-at text (1+ i)) #\x)))
        (setf i (%skip #'digit-char-p text (1+ i))))
      (when (find (%char-at text i) "eE")
        (let ((j (if (find (%char-at text (1+ i)) "+-") (+ i 2) (1+ i))))
          (when (digit-char-p (or (%char-at text j) #\x))
            (setf i (%skip #'digit-char-p text j)))))
      i)))

(defmethod lexer-tokens ((lexer json-lexer) text)
  (let ((sink (make-sink text))
        (n (length text))
        (pos 0))
    (loop while (< pos n)
          do (let ((ch (char text pos)))
               (setf pos
                     (cond
                       ((%whitespace-p ch) (emit sink :plain pos (%skip #'%whitespace-p text pos)))
                       ((char= ch #\")
                        (let ((end (%json-string-end text pos)))
                          (emit sink (if (eql (%char-at text (%skip #'%whitespace-p text end)) #\:)
                                         :property
                                         :string)
                                pos end)))
                       ((%looking-at text pos "//") (emit sink :comment pos (%line-end text pos)))
                       ((%looking-at text pos "/*")
                        (let ((close (search "*/" text :start2 (+ pos 2))))
                          (emit sink :comment pos (if close (+ close 2) n))))
                       ((and (or (char= ch #\-) (digit-char-p ch)) (%json-number-end text pos))
                        (emit sink :number pos (%json-number-end text pos)))
                       ((find ch "{}[],:") (emit sink :punctuation pos (1+ pos)))
                       ((alpha-char-p ch)
                        (let ((end (%skip (lambda (c) (or (alphanumericp c) (find c "_$"))) text pos)))
                          (emit sink (if (member (subseq text pos end) '("true" "false" "null")
                                                 :test #'string=)
                                         :constant
                                         :plain)
                                pos end)))
                       (t (emit sink :plain pos (1+ pos)))))))
    (sink-result sink)))
