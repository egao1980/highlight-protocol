;;;; Lexers: the protocol, the registry and TOKENIZE.
;;;;
;;;; A LEXER is a CLOS object with a NAME and ALIASES (strings). LEXER-TOKENS
;;;; is the generic function a lexer class implements: TEXT (any string) to a
;;;; list of tokens concatenating to TEXT. It must accept every string, never
;;;; signal on its content and run in linear time. Its output may contain
;;;; empty texts or neighbours of one class; TOKENIZE normalises them.
;;;;
;;;; The registry maps lowercase names and aliases to lexers. REGISTER-LEXER
;;;; replaces an earlier lexer of the same name and takes over any of the
;;;; new lexer's names an older lexer had. Register at load time: the
;;;; registry is a plain hash table, not guarded for concurrent writers.
;;;;
;;;; A language designator is a lexer, a string or a symbol. A string or
;;;; symbol is looked up by its first word, trimmed and downcased, so an Org
;;;; "lisp :results output" or a markdown "python {.numberLines}" still
;;;; finds its lexer. NIL and blank strings mean "no language": plain text.

(in-package #:highlight-protocol)

(defclass lexer ()
  ((name :initarg :name :reader lexer-name
         :documentation "The canonical name, e.g. \"common-lisp\".")
   (aliases :initarg :aliases :initform '() :reader lexer-aliases
            :documentation "Other names it is found by, e.g. (\"lisp\" \"cl\")."))
  (:documentation "A source of tokens for one language. Subclass it and
define a LEXER-TOKENS method; REGISTER-LEXER makes it findable."))

(defmethod initialize-instance :after ((lexer lexer) &key)
  (check-type (slot-value lexer 'name) string)
  (check-type (slot-value lexer 'aliases) list)
  (dolist (alias (slot-value lexer 'aliases))
    (check-type alias string)))

(defmethod print-object ((lexer lexer) stream)
  (print-unreadable-object (lexer stream :type t)
    (format stream "~S" (lexer-name lexer))))

(defgeneric lexer-tokens (lexer text)
  (:documentation "TEXT, a string, as a list of (CLASS . TEXT) tokens that
concatenate to TEXT. Total on strings, linear time."))

;;; Plain text

(defclass plain-lexer (lexer) ()
  (:documentation "Everything is :PLAIN."))

(defmethod lexer-tokens ((lexer plain-lexer) text)
  (if (plusp (length text))
      (list (cons :plain (copy-seq text)))
      '()))

(defvar *plain-lexer*
  (make-instance 'plain-lexer :name "text" :aliases '("plain" "txt" "plaintext"))
  "The fallback for unknown languages and for no language.")

;;; Registry

(defvar *lexers* (make-hash-table :test 'equal)
  "Lowercase name or alias → lexer.")

(defvar *lexer-order* '()
  "Registered lexers, oldest first.")

(defun %whitespace-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))

(defun %language-key (designator)
  "DESIGNATOR's first word, downcased; NIL when blank."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) (string designator)))
         (end (position-if #'%whitespace-p s)))
    (when (plusp (length s))
      (string-downcase (subseq s 0 end)))))

(defun register-lexer (lexer)
  "Make LEXER findable by its name and aliases (case-insensitive). Replaces a
lexer registered earlier under the same name. Returns LEXER."
  (check-type lexer lexer)
  (let ((name (lexer-name lexer)))
    (loop for key being the hash-keys of *lexers* using (hash-value old)
          when (string-equal (lexer-name old) name)
            collect key into stale
          finally (dolist (key stale) (remhash key *lexers*)))
    (setf *lexer-order*
          (append (remove name *lexer-order* :key #'lexer-name :test #'string-equal)
                  (list lexer)))
    (dolist (designator (cons name (lexer-aliases lexer)))
      (let ((key (%language-key designator)))
        (when key
          (setf (gethash key *lexers*) lexer))))
    lexer))

(defun find-lexer (language)
  "The lexer for LANGUAGE: a lexer (returned), a string or a symbol (its first
word, case-insensitive), or NIL. Returns NIL when none is registered."
  (typecase language
    (lexer language)
    (null nil)
    ((or string symbol)
     (let ((key (%language-key language)))
       (and key (values (gethash key *lexers*)))))
    (t (error 'type-error :datum language :expected-type '(or lexer string symbol)))))

(defun list-lexers ()
  "Every registered lexer, oldest registration first."
  (remove-if-not (lambda (lexer)
                   (eq lexer (gethash (%language-key (lexer-name lexer)) *lexers*)))
                 *lexer-order*))

(defun %resolve-lexer (language)
  "LANGUAGE's lexer. An unknown name signals UNKNOWN-LANGUAGE (USE-VALUE
restart); NIL, a blank name and an unhandled unknown name give plain text."
  (or (find-lexer language)
      (if (or (null language) (null (%language-key language)))
          *plain-lexer*
          (restart-case
              (progn (signal 'unknown-language :name language)
                     *plain-lexer*)
            (use-value (value)
              :report "Lex with another lexer or language."
              :interactive (lambda ()
                             (format *query-io* "Language: ")
                             (force-output *query-io*)
                             (list (read-line *query-io*)))
              (or (find-lexer value) *plain-lexer*))))))

(defun tokenize (text language)
  "TEXT (a string) as a list of (CLASS . TEXT) tokens for LANGUAGE (a lexer,
name or alias; NIL for plain text). The texts concatenate to TEXT, are never
empty, and neighbours never share a class. An unknown language signals
UNKNOWN-LANGUAGE (not an error) and then lexes as plain text."
  (check-type text string)
  (normalize-tokens (lexer-tokens (%resolve-lexer language) text)))

(defun tokenize-lines (lines language)
  "LINES (strings without newlines) lexed together as LANGUAGE, returned as
TOKEN-LINES: one list of tokens per input line. The shape of tui-markdown's
code-block :highlight hook."
  (check-type lines list)
  (when lines
    (token-lines
     (tokenize (with-output-to-string (s)
                 (loop for (line . more) on lines
                       do (check-type line string)
                          (write-string line s)
                          (when more (terpri s))))
               language))))

;;; Scanning helpers shared by the lexers. Positions are indices into TEXT;
;;; every helper returns a position at most (LENGTH TEXT).

(declaim (inline %char-at))
(defun %char-at (text i)
  "TEXT[I], or NIL past the end."
  (and (< -1 i (length text)) (char text i)))

(defun %line-end (text start &optional (end (length text)))
  "The position of the next newline at or after START, or END."
  (or (position #\Newline text :start (min start end) :end end) end))

(defun %looking-at (text pos prefix)
  "Whether TEXT continues with PREFIX at POS."
  (let ((end (+ pos (length prefix))))
    (and (<= end (length text))
         (string= text prefix :start1 pos :end1 end))))

(defun %skip (predicate text pos &optional (end (length text)))
  "The first position at or after POS whose character fails PREDICATE, or END."
  (or (position-if-not predicate text :start (min pos end) :end end) end))

(defun %blank-p (ch)
  "Space, tab, return: whitespace that does not end a line."
  (member ch '(#\Space #\Tab #\Return #\Page)))

(defun %string-set (names)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (name names table)
      (setf (gethash name table) t))))
