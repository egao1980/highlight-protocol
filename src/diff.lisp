;;;; Unified diffs (git diff, diff -u) and loose +/- snippets.
;;;;
;;;; Line by line; the newline after each line is :plain.
;;;;   @@ … :hunk. When it reads "@@ -a[,b] +c[,d] @@", the next b old and d
;;;;   new lines are the hunk body: " " (or empty) context :plain, - :deleted,
;;;;   + :inserted, \ :comment — so a removed "--- x" line is not a header
;;;;   outside a hunk body:
;;;;     diff, index, ---, +++, new/deleted file, old/new mode, (dis)similarity
;;;;     index, rename, copy from/to, Binary files … :header
;;;;     - :deleted   + :inserted   \ (no newline at end of file) :comment
;;;;     anything else :plain (commit messages, prose)

(in-package #:highlight-protocol)

(defclass diff-lexer (lexer) ()
  (:documentation "Unified diffs and patch snippets."))

(defparameter *diff-header-prefixes*
  '("diff " "index " "--- " "+++ " "new file mode" "deleted file mode" "old mode"
    "new mode" "similarity index" "dissimilarity index" "rename from" "rename to"
    "copy from" "copy to" "Binary files "))

(defun %hunk-counts (text ls le)
  "(values OLD NEW) line counts of the @@ header TEXT[LS, LE), or NIL."
  (let ((i (+ ls 2)))
    (labels ((char-at (k) (and (< k le) (char text k)))
             (expect (ch) (when (eql (char-at i) ch) (incf i)))
             (number ()
               (let ((e (%skip #'digit-char-p text i le)))
                 (when (> e i)
                   (prog1 (parse-integer text :start i :end e)
                     (setf i e)))))
             (range ()
               ;; start[,count] → count (default 1)
               (when (number)
                 (if (expect #\,)
                     (number)
                     1))))
      (let ((old nil) (new nil))
        (and (expect #\Space)
             (expect #\-)
             (setf old (range))
             (expect #\Space)
             (expect #\+)
             (setf new (range))
             (expect #\Space)
             (%looking-at text i "@@")
             (values old new))))))

(defmethod lexer-tokens ((lexer diff-lexer) text)
  (let ((sink (make-sink text))
        (n (length text))
        (old 0)
        (new 0))
    (loop with ls = 0
          while (< ls n)
          do (let* ((le (%line-end text ls))
                    (c (and (< ls le) (char text ls)))
                    (class nil))
               (when (or (plusp old) (plusp new))
                 (case c
                   ((nil #\Space) (decf old) (decf new) (setf class :plain))
                   (#\- (decf old) (setf class :deleted))
                   (#\+ (decf new) (setf class :inserted))
                   (#\\ (setf class :comment))
                   (t (setf old 0 new 0)))
                 (when (and (<= old 0) (<= new 0))
                   (setf old 0 new 0)))
               (unless class
                 (setf class
                       (cond ((%looking-at text ls "@@")
                              (multiple-value-bind (o w) (%hunk-counts text ls le)
                                (when o (setf old o new w)))
                              :hunk)
                             ((some (lambda (prefix) (%looking-at text ls prefix))
                                    *diff-header-prefixes*)
                              :header)
                             ((member (subseq text ls (min le (+ ls 4))) '("---" "+++")
                                      :test #'string=)
                              :header)
                             ((eql c #\-) :deleted)
                             ((eql c #\+) :inserted)
                             ((eql c #\\) :comment)
                             (t :plain))))
               (emit sink class ls le)
               (emit sink :plain le (min n (1+ le)))
               (setf ls (1+ le))))
    (sink-result sink)))
