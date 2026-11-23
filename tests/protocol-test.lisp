(in-package #:highlight-protocol/tests)

;;; The token protocol: classes, normalisation, lookup, lines.

(deftest token-classes-fixed
  (ok (equal '(:plain :keyword :builtin :function :type :symbol :variable :constant
               :string :doc :number :comment :operator :punctuation :meta :property
               :inserted :deleted :header :hunk)
             +token-classes+)))

(deftest-parametrize alias-lookup
    ((alias name)
     ("lisp" "common-lisp") ("common-lisp" "common-lisp") ("cl" "common-lisp")
     ("commonlisp" "common-lisp") ("asd" "common-lisp")
     ("elisp" "emacs-lisp") ("emacs-lisp" "emacs-lisp")
     ("scheme" "scheme") ("scm" "scheme")
     ("python" "python") ("py" "python") ("python3" "python")
     ("sh" "shell") ("bash" "shell") ("shell" "shell") ("zsh" "shell")
     ("console" "console") ("shell-session" "console")
     ("json" "json") ("jsonc" "json")
     ("yaml" "yaml") ("yml" "yaml")
     ("diff" "diff") ("patch" "diff")
     ("text" "text") ("plain" "text") ("txt" "text"))
  (let ((lexer (find-lexer alias)))
    (ok (typep lexer 'lexer))
    (ok (equal name (and lexer (lexer-name lexer))))))

(deftest find-lexer-designators
  (ok (eq (find-lexer "lisp") (find-lexer "LISP")) "case-insensitive")
  (ok (eq (find-lexer "lisp") (find-lexer :lisp)) "symbols")
  (ok (eq (find-lexer "python") (find-lexer "  Python  ")) "trimmed")
  (ok (eq (find-lexer "lisp") (find-lexer "lisp :results output")) "first word")
  (ok (eq (find-lexer "lisp") (find-lexer (find-lexer "lisp"))) "a lexer is itself")
  (ok (null (find-lexer "cobol")))
  (ok (null (find-lexer "")))
  (ok (null (find-lexer nil))))

(deftest lexer-dialects
  (ok (eq :common-lisp (lisp-lexer-dialect (find-lexer "lisp"))))
  (ok (eq :emacs-lisp (lisp-lexer-dialect (find-lexer "elisp"))))
  (ok (eq :scheme (lisp-lexer-dialect (find-lexer "scheme"))))
  (ok (typep (find-lexer "console") 'shell-lexer) "console lexes commands as shell")
  (ok (typep (find-lexer "text") 'plain-lexer)))

(deftest list-lexers-built-in
  (let ((names (mapcar #'lexer-name (list-lexers))))
    (ok (every (lambda (n) (member n names :test #'string=)) *lexer-names*))
    (ok (= (length names) (length (remove-duplicates names :test #'string=))))))

(deftest tokenize-plain
  (ok (equal '((:plain . "a < b")) (tokenize "a < b" "text")))
  (ok (equal '((:plain . "a < b")) (tokenize "a < b" nil)) "NIL language → plain")
  (ok (equal '() (tokenize "" "lisp")) "empty text → no tokens")
  (ok (equal '((:plain . "x")) (tokenize "x" (find-lexer "text"))) "a lexer object"))

(deftest tokenize-type-errors
  (ok (signals (tokenize 42 "lisp") 'type-error))
  (ok (signals (tokenize "x" 42) 'type-error))
  (ok (signals (token-lines 'x) 'type-error))
  (ok (signals (register-lexer "lisp") 'type-error)))

;;; A user lexer: raw tokens are normalised by TOKENIZE.

(defclass shout-lexer (lexer) ())

(defmethod lexer-tokens ((lexer shout-lexer) text)
  (list (cons :plain "")
        (cons :keyword (subseq text 0 1))
        (cons :keyword (subseq text 1))
        (cons :plain "")))

(deftest register-user-lexer
  (let ((lexer (make-instance 'shout-lexer :name "shout-test" :aliases '("shout" "SHT"))))
    (ok (eq lexer (register-lexer lexer)))
    (ok (eq lexer (find-lexer "shout-test")))
    (ok (eq lexer (find-lexer "sht")) "aliases are case-insensitive")
    (ok (equal '((:keyword . "hey")) (tokenize "hey" "shout"))
        "empty tokens dropped, neighbours of one class merged")
    (ok (member lexer (list-lexers)))
    (let ((again (make-instance 'shout-lexer :name "shout-test" :aliases '("shout"))))
      (register-lexer again)
      (ok (eq again (find-lexer "shout")) "a later registration replaces")
      (ok (eq again (find-lexer "shout-test")))
      (ok (= 1 (count "shout-test" (list-lexers) :key #'lexer-name :test #'string=))))))

;;; Unknown languages: a condition (not an error) with USE-VALUE.

(deftest unknown-language-signalled
  (ok (signals (tokenize "x" "cobol") 'unknown-language))
  (let ((seen nil))
    (handler-bind ((unknown-language (lambda (c) (setf seen (unknown-language-name c)))))
      (ok (equal '((:plain . "MOVE A.")) (tokenize "MOVE A." "cobol")) "unhandled → plain"))
    (ok (equal "cobol" seen)))
  (ok (equal '((:plain . "x")) (tokenize "x" nil)) "no language is not unknown"))

(deftest unknown-language-use-value
  (ok (equal '((:punctuation . "(") (:builtin . "car") (:punctuation . ")"))
             (handler-bind ((unknown-language (lambda (c) (use-value "lisp" c))))
               (tokenize "(car)" "cobol"))))
  (ok (equal '((:plain . "(car)"))
             (handler-bind ((unknown-language
                              (lambda (c) (use-value (find-lexer "text") c))))
               (tokenize "(car)" "cobol")))))

;;; Lines: the shape a styled-run renderer (tui-markdown) consumes.

(deftest token-lines-split
  (ok (equal '(((:string . "\"a")) ((:string . "b\"")) ((:keyword . "x")))
             (token-lines '((:string . "\"a
b\"") (:plain . "
") (:keyword . "x")))))
  (ok (equal '(((:plain . "a")) ()) (token-lines '((:plain . "a
")))) "a trailing newline ends with an empty line")
  (ok (equal '(()) (token-lines '())) "no tokens: one empty line")
  (ok (equal '(() () ((:comment . "; c")))
             (token-lines (tokenize (lines "" "" "; c") "lisp")))))

(deftest tokenize-lines-hook-shape
  (ok (equal '(((:punctuation . "(") (:builtin . "car"))
               ((:plain . "  x") (:punctuation . ")")))
             (tokenize-lines '("(car" "  x)") "lisp")))
  (ok (equal '(((:string . "\"a")) ((:string . "b\"")))
             (tokenize-lines '("\"a" "b\"") "lisp"))
      "a string across lines keeps its class on both")
  (ok (equal '() (tokenize-lines '() "lisp"))))
