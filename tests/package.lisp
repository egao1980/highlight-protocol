(defpackage #:highlight-protocol/tests
  (:use #:cl #:rove #:highlight-protocol)
  (:local-nicknames (#:html #:html-protocol)
                    (#:css #:css-protocol)))

(in-package #:highlight-protocol/tests)

(defun lines (&rest lines)
  "LINES joined with newlines (no trailing newline)."
  (format nil "~{~A~^~%~}" lines))

(defun toks (language &rest text-lines)
  "TEXT-LINES joined with newlines, tokenized as LANGUAGE."
  (tokenize (apply #'lines text-lines) language))

(defun concat (tokens)
  (with-output-to-string (s)
    (dolist (token tokens)
      (write-string (cdr token) s))))

(defun class-of-piece (tokens piece)
  "The class of the first token whose text is exactly PIECE, or :MISSING."
  (let ((token (find piece tokens :key #'cdr :test #'string=)))
    (if token (car token) :missing)))

(defun well-formed-p (tokens text)
  "TOKENS concatenate to TEXT, use only known classes, have no empty text
and no two neighbours of the same class."
  (and (listp tokens)
       (every (lambda (tk)
                (and (consp tk)
                     (member (car tk) +token-classes+)
                     (stringp (cdr tk))
                     (plusp (length (cdr tk)))))
              tokens)
       (loop for (a b) on tokens while b never (eq (car a) (car b)))
       (string= text (concat tokens))))

(defvar *lexer-names*
  '("common-lisp" "emacs-lisp" "scheme" "python" "shell" "console" "json" "yaml" "diff" "text"))
