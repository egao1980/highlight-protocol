;;;; Tokens.
;;;;
;;;; A TOKEN is a cons (CLASS . TEXT): CLASS is one of +TOKEN-CLASSES+ and
;;;; TEXT a non-empty string. TOKENIZE returns a list of tokens whose texts
;;;; concatenate to exactly its input, with no two neighbours of one class.
;;;;
;;;; Classes, and what the built-in lexers mark with them:
;;;;
;;;;   :plain        whitespace, names and anything not below (HTML: no span)
;;;;   :keyword      reserved words: Lisp special operators and macros (and
;;;;                 def… / with-… forms) where they are called, Python and
;;;;                 shell keywords
;;;;   :builtin      standard functions where they are called: CL functions,
;;;;                 Python builtins and exceptions, shell builtins
;;;;   :function     a name being defined (defun foo, def foo, foo() { }), #'foo
;;;;   :type         a type being defined (defclass, defstruct, class), and
;;;;                 Lisp lambda-list keywords (&optional)
;;;;   :symbol       Lisp keywords (:foo), quoted symbols ('foo), #:foo
;;;;   :variable     *specials*, defvar names, $VAR / ${…}, NAME=, self / cls
;;;;   :constant     t nil +const+ #t #f, True False None, true false null ~
;;;;   :string       string and character literals, heredocs, YAML scalars
;;;;   :doc          docstrings
;;;;   :number       numeric literals
;;;;   :comment      comments
;;;;   :operator     operators (Python, shell)
;;;;   :punctuation  brackets, commas, colons, YAML indicators
;;;;   :meta         reader macros (#+, #., `), decorators, shebangs, console
;;;;                 prompts, YAML directives, document markers, tags, anchors
;;;;   :property     JSON and YAML keys
;;;;   :inserted     diff: an added line          :deleted  a removed line
;;;;   :header       diff: file header lines      :hunk     an @@ line
;;;;
;;;; TOKEN-LINES splits tokens at newlines: the shape a styled-run renderer
;;;; (tui-markdown's :highlight hook) wants, one list of tokens per line.

(in-package #:highlight-protocol)

(defparameter +token-classes+
  '(:plain :keyword :builtin :function :type :symbol :variable :constant
    :string :doc :number :comment :operator :punctuation :meta :property
    :inserted :deleted :header :hunk)
  "Every class a token may have, in a fixed order.")

;;; A sink collects a lexer's output: spans of the input, merged when one
;;; class continues, cut out of the text only once per token.

(defstruct (sink (:constructor make-sink (text)))
  (text "" :type string :read-only t)
  (tokens '() :type list)
  (class nil :type symbol)
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defun %flush (sink)
  (when (sink-class sink)
    (push (cons (sink-class sink)
                (subseq (sink-text sink) (sink-start sink) (sink-end sink)))
          (sink-tokens sink))
    (setf (sink-class sink) nil)))

(defun emit (sink class start end)
  "Add TEXT[START, END) of SINK's text as CLASS. An empty span is ignored; a
span that continues the pending token's class joins it."
  (declare (type sink sink) (type fixnum start end))
  (when (< start end)
    (if (and (eq class (sink-class sink)) (= start (sink-end sink)))
        (setf (sink-end sink) end)
        (progn (%flush sink)
               (setf (sink-class sink) class
                     (sink-start sink) start
                     (sink-end sink) end))))
  end)

(defun sink-result (sink)
  "The tokens collected by SINK, in order."
  (%flush sink)
  (nreverse (shiftf (sink-tokens sink) '())))

;;; Normalising a lexer's raw tokens

(defun %check-token (token)
  (unless (and (consp token) (stringp (cdr token)))
    (error 'type-error :datum token :expected-type '(cons symbol string)))
  (unless (member (car token) +token-classes+)
    (error 'type-error :datum (car token) :expected-type (cons 'member +token-classes+)))
  token)

(defun %normal-p (tokens)
  (loop for (a . rest) on tokens
        do (%check-token a)
        always (and (plusp (length (cdr a)))
                    (or (null rest) (not (eq (car a) (car (first rest))))))))

(defun normalize-tokens (tokens)
  "TOKENS (a lexer's output) without empty texts, neighbours of one class
merged. Returned as is when already normal. Signals TYPE-ERROR for a token
that is not (CLASS . STRING) with CLASS in +TOKEN-CLASSES+."
  (if (%normal-p tokens)
      tokens
      (let ((out '()) (class nil) (pieces '()))
        (flet ((flush ()
                 (when pieces
                   (push (cons class (with-output-to-string (s)
                                       (dolist (piece (nreverse pieces))
                                         (write-string piece s))))
                         out)
                   (setf pieces '()))))
          (dolist (token tokens)
            (when (plusp (length (cdr token)))
              (unless (eq (car token) class)
                (flush)
                (setf class (car token)))
              (push (cdr token) pieces)))
          (flush))
        (nreverse out))))

;;; Lines

(defun token-lines (tokens)
  "TOKENS split at newlines: a list with one list of tokens per line (one
more than the newlines in the text), newline characters dropped. A token
spanning lines keeps its class on each; an empty line is NIL."
  (check-type tokens list)
  (let ((lines '()) (line '()))
    (dolist (token tokens)
      (destructuring-bind (class . text) (%check-token token)
        (let ((start 0) (n (length text)))
          (loop for newline = (position #\Newline text :start start)
                do (cond ((and (= start 0) (null newline))
                          (push token line))
                         ((< start (or newline n))
                          (push (cons class (subseq text start (or newline n))) line)))
                   (if newline
                       (progn (push (nreverse line) lines)
                              (setf line '() start (1+ newline)))
                       (return))))))
    (push (nreverse line) lines)
    (nreverse lines)))
