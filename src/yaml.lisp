;;;; YAML.
;;;;
;;;;   %DIRECTIVE lines and the --- / ... document markers (column 0) :meta
;;;;   # at a line start or after whitespace … end of line :comment
;;;;   indicators :punctuation: "- " and "? " where a node starts, ": " (or :
;;;;   at the end of a line, or before , ] } inside flow), [ ] { } and , in flow
;;;;   &anchor *alias !tag :meta
;;;;   '…' ('' escapes) and "…" (\ escapes) may span lines; unterminated, they
;;;;   run to the end. :property when followed by ": ", else :string
;;;;   plain scalars run to the end of the line, " #", ": " (and , [ ] { } in
;;;;   flow), trailing blanks excluded: a key (followed by :) is :property;
;;;;   a value is :constant (true false yes no on off null ~, any case),
;;;;   :number (decimal, 0x, 0o, .inf, .nan) or :string
;;;;   | and > with chomping / indentation indicators, then only a comment,
;;;;   start a block scalar: the indicator is :punctuation, and the following
;;;;   lines that are blank or indented deeper than the indicator's line are
;;;;   :string, newlines included
;;;;
;;;; This lexes text for display; it does not parse YAML (yaml-protocol does).

(in-package #:highlight-protocol)

(defclass yaml-lexer (lexer) ()
  (:documentation "YAML documents."))

(defun %yaml-number-p (s)
  (let* ((n (length s))
         (i (if (and (plusp n) (find (char s 0) "+-")) 1 0))
         (body (subseq s i)))
    (flet ((all (chars from)
             (and (< from (length body))
                  (every (lambda (ch) (find ch chars)) (subseq body from)))))
      (cond ((member body '(".inf" ".nan") :test #'string-equal) t)
            ((and (> (length body) 2) (char= (char body 0) #\0) (char-equal (char body 1) #\x))
             (all "0123456789abcdefABCDEF_" 2))
            ((and (> (length body) 2) (char= (char body 0) #\0) (char-equal (char body 1) #\o))
             (all "01234567_" 2))
            (t
             (let* ((m (length body))
                    (j (%skip (lambda (ch) (or (digit-char-p ch) (char= ch #\_))) body 0))
                    (int-digits j)
                    (frac-digits 0))
               (when (and (< j m) (char= (char body j) #\.))
                 (let ((k (%skip #'digit-char-p body (1+ j))))
                   (setf frac-digits (- k (1+ j)) j k)))
               (when (and (< j m) (char-equal (char body j) #\e)
                          (plusp (+ int-digits frac-digits)))
                 (let* ((k (if (and (< (1+ j) m) (find (char body (1+ j)) "+-")) (+ j 2) (1+ j)))
                        (e (%skip #'digit-char-p body k)))
                   (when (> e k) (setf j e))))
               (and (= j m) (plusp (+ int-digits frac-digits)))))))))

(defun %yaml-scalar-class (s)
  (cond ((member s '("true" "false" "yes" "no" "on" "off" "null" "~") :test #'string-equal)
         :constant)
        ((%yaml-number-p s) :number)
        (t :string)))

(defmethod lexer-tokens ((lexer yaml-lexer) text)
  (let ((sink (make-sink text))
        (n (length text))
        (pos 0)
        (flow 0)              ; depth of [ and {
        (block nil)           ; indentation of a block scalar's parent line
        (line-start 0)
        (indent 0)            ; of the current line
        (node-start-p t))     ; nothing but indicators so far on this line
    (labels ((char-at (i) (%char-at text i))
             (separator-p (i)
               ;; Whether TEXT[I] ends an indicator: blank, newline or end.
               (let ((c (char-at i)))
                 (or (null c) (%whitespace-p c))))
             (flow-indicator-p (c) (and (plusp flow) c (find c ",[]{}")))
             (key-colon-p (i)
               ;; A : at I that separates a key from its value.
               (and (eql (char-at i) #\:)
                    (or (separator-p (1+ i)) (flow-indicator-p (char-at (1+ i))))))
             (quoted-end (p)
               (let ((q (char text p)))
                 (loop with i = (1+ p)
                       while (< i n)
                       do (let ((c (char text i)))
                            (cond ((and (char= q #\") (char= c #\\)) (incf i 2))
                                  ((and (char= q #\') (char= c #\') (eql (char-at (1+ i)) #\'))
                                   (incf i 2))
                                  ((char= c q) (return (1+ i)))
                                  (t (incf i))))
                       finally (return n))))
             (plain-end (p)
                        ;; (values END STOP): the end of a plain scalar at P with trailing blanks
                        ;; excluded, and where the scan stopped (a key's : is there).
                        (let ((i p))
                          (loop while (< i n)
                                do (let ((c (char text i)))
                                     (cond ((char= c #\Newline) (return))
                                           ((and (%blank-p c) (eql (char-at (1+ i)) #\#)) (return))
                                           ((key-colon-p i) (return))
                                           ((flow-indicator-p c) (return))
                                           (t (incf i)))))
                          (values (max (1+ p) (let ((e i))
                                                (loop while (and (> e p) (%blank-p (char text (1- e))))
                                                      do (decf e))
                                                e))
                                  i)))
             (block-indicator-end (p)
               ;; After | or > at P: the end of the indicator when only
               ;; blanks and a comment follow on the line, else NIL.
               (let* ((e (%skip (lambda (c) (find c "+-0123456789")) text (1+ p)))
                      (after (%skip #'%blank-p text e)))
                 (and (or (>= after n)
                          (char= (char text after) #\Newline)
                          (and (char= (char text after) #\#) (> after e)))
                      e)))
             (token (class end &optional (node-start nil))
               (setf node-start-p node-start)
               (emit sink class pos end))
             (start-line (p)
               ;; P starts a line: handle block scalars and column-0 markers.
               (setf line-start p
                     indent (- (%skip (lambda (c) (char= c #\Space)) text p) p)
                     node-start-p t)
               (when block
                 (let ((le (%line-end text p)))
                   (if (or (> indent block)
                           (every #'%blank-p (subseq text (+ p indent) le)))
                       (return-from start-line (emit sink :string p (min n (1+ le))))
                       (setf block nil))))
               (cond ((and (zerop flow)
                           (or (%looking-at text p "---") (%looking-at text p "..."))
                           (separator-p (+ p 3)))
                      (setf node-start-p t)
                      (emit sink :meta p (+ p 3)))
                     ((and (zerop flow) (eql (char-at p) #\%))
                      (emit sink :meta p (%line-end text p)))
                     (t nil))))
      (loop with at-line-start = t
            while (< pos n)
            do (if at-line-start
                   (let ((next (start-line pos)))
                     (setf at-line-start nil)
                     (when next
                       (setf at-line-start (and (> next pos) (char= (char text (1- next)) #\Newline))
                             pos next)))
                   (let ((ch (char text pos)))
                     (setf pos
                           (cond
                             ((char= ch #\Newline)
                              (setf at-line-start t)
                              (emit sink :plain pos (1+ pos)))
                             ((%blank-p ch) (emit sink :plain pos (%skip #'%blank-p text pos)))
                             ((and (char= ch #\#)
                                   (or (= pos line-start) (%whitespace-p (char text (1- pos)))))
                              (emit sink :comment pos (%line-end text pos)))
                             ((and (find ch "-?") node-start-p (zerop flow) (separator-p (1+ pos)))
                              (token :punctuation (1+ pos) t))
                             ((key-colon-p pos) (token :punctuation (1+ pos) t))
                             ((find ch "[{")
                              (incf flow)
                              (token :punctuation (1+ pos) t))
                             ((find ch "]}")
                              (setf flow (max 0 (1- flow)))
                              (token :punctuation (1+ pos)))
                             ((and (char= ch #\,) (plusp flow)) (token :punctuation (1+ pos) t))
                             ((find ch "&*!")
                              (token :meta (%skip (lambda (c)
                                                    (not (or (%whitespace-p c) (flow-indicator-p c))))
                                                  text (1+ pos))
                                     node-start-p))
                             ((and (find ch "|>") (block-indicator-end pos))
                              (setf block indent)
                              (token :punctuation (block-indicator-end pos)))
                             ((find ch "'\"")
                              (let* ((end (quoted-end pos))
                                     (after (%skip #'%blank-p text end)))
                                (token (if (key-colon-p after) :property :string) end)))
                             (t
                              (multiple-value-bind (end stop) (plain-end pos)
                                (token (if (key-colon-p stop)
                                           :property
                                           (%yaml-scalar-class (subseq text pos end)))
                                       end))))))))
      (sink-result sink))))
