;;;; Common Lisp, Emacs Lisp and Scheme.
;;;;
;;;; One scanner, three dialects (LISP-LEXER-DIALECT :common-lisp,
;;;; :emacs-lisp, :scheme). It reads tokens the way the Lisp reader would,
;;;; but never interns or evaluates, and keeps a stack of open lists to know
;;;; what is called and what is defined. No recursion, so nesting depth is
;;;; unbounded; unterminated strings, comments and |…| run to the end.
;;;;
;;;; Tokens:
;;;;   whitespace :plain;  ; … end of line :comment;  #| … |# (nesting) :comment
;;;;   "…" (\ escapes) :string, or :doc in a documentation position (below)
;;;;   ( ) :punctuation; in Emacs Lisp and Scheme also [ ] { } (they end atoms)
;;;;   'atom :symbol; ' before anything else :meta, and a quoted list is data
;;;;   ` , ,@ :meta (a backquoted list is code)
;;;;   #'atom :function   #:atom :symbol   #\x… :string   #x #b #o #NNr #* :number
;;;;   #+feature #-feature :meta (a feature list is data)   #. #, #p #c #a #s #N= #N# :meta
;;;;   #( :punctuation (data)   Scheme: #t #f #true #false :constant, #; :comment
;;;;   Emacs Lisp: ?c and ?\c :string
;;;;   atoms (with \ and |…| escapes):
;;;;     numbers (integer, ratio, float with e s f d l exponents) :number
;;;;     :key :symbol   &optional … :type   t nil :constant (not Scheme)
;;;;     *special* :variable   +constant+ :constant
;;;;     the first atom of a code list: special operators, CL macros, def… and
;;;;     with-… :keyword; CL functions :builtin (cl: / common-lisp: prefixes
;;;;     are looked through; other packages are :plain); dialect extras below
;;;;     anything else :plain
;;;;
;;;; Data lists (quoted, vectors, feature expressions, lambda lists, a
;;;; definition's name list) never mark their first atom as called.
;;;;
;;;; Definitions: in a list whose operator is
;;;;   defclass defstruct define-condition deftype define-record-type
;;;;   cl-defstruct                                    → name :type
;;;;   defvar defparameter defconstant define-symbol-macro defcustom defconst
;;;;   defvar-local defglobal define-constant defface defgroup → name :variable
;;;;   Scheme define                     → (define name …) :variable,
;;;;                                       (define (name …) …) :function
;;;;   any other def… (not default…) or cl-def… → name :function
;;;; the second element names it. A list there (defstruct (point …),
;;;; defun (setf foo)) names its first atom that is not SETF.
;;;; Lambda lists are data, and a string right after one is :doc: the first
;;;; list after the name of defun defmacro defmethod defgeneric deftype
;;;; define-compiler-macro define-setf-expander defsetf defsubst cl-defun
;;;; cl-defmacro cl-defmethod cl-defgeneric, the name list of a Scheme
;;;; define, and LAMBDA's first list. Also :doc: the fourth element of a
;;;; variable definition, the third of DEFSTRUCT, and any string right after
;;;; :DOCUMENTATION. DEFCLASS / DEFINE-CONDITION superclass and slot lists
;;;; are data.

(in-package #:highlight-protocol)

(defclass lisp-lexer (lexer)
  ((dialect :initarg :dialect :initform :common-lisp :reader lisp-lexer-dialect))
  (:documentation "Common Lisp (:common-lisp), Emacs Lisp (:emacs-lisp) or
Scheme (:scheme) source."))

(defmethod initialize-instance :after ((lexer lisp-lexer) &key)
  (check-type (slot-value lexer 'dialect) (member :common-lisp :emacs-lisp :scheme)))

;;; Symbol tables. The CL ones come from the COMMON-LISP package itself:
;;; its 978 external symbols are fixed by the standard.

(defun %cl-table (predicate)
  (let ((table (make-hash-table :test 'equal)))
    (do-external-symbols (s :common-lisp table)
      (when (funcall predicate s)
        (setf (gethash (string-downcase (symbol-name s)) table) t)))))

(defparameter *cl-keywords*
  (let ((table (%cl-table (lambda (s) (or (special-operator-p s) (macro-function s))))))
    (dolist (name '("declare" "lambda") table)
      (setf (gethash name table) t))))

(defparameter *cl-functions*
  (%cl-table (lambda (s)
               (and (fboundp s) (not (special-operator-p s)) (not (macro-function s))))))

(defparameter *elisp-keywords*
  (%string-set '("setq" "setq-default" "setq-local" "defvar-local" "defcustom" "defgroup"
                 "defface" "defsubst" "defalias" "defconst" "define-minor-mode"
                 "define-derived-mode" "save-excursion" "save-restriction" "save-match-data"
                 "save-window-excursion" "with-current-buffer" "with-temp-buffer"
                 "interactive" "condition-case" "while" "pcase" "pcase-let" "pcase-dolist"
                 "cl-defun" "cl-defmacro" "cl-defmethod" "cl-defgeneric" "cl-loop" "cl-case"
                 "cl-flet" "cl-labels" "cl-letf" "when-let" "if-let" "when-let*" "if-let*"
                 "and-let*" "let-alist" "dolist" "dotimes" "use-package" "ignore-errors"
                 "unwind-protect" "catch" "throw" "inline")))

(defparameter *elisp-builtins*
  (%string-set '("message" "format" "insert" "point" "point-min" "point-max" "goto-char"
                 "forward-line" "forward-char" "buffer-substring" "buffer-string"
                 "buffer-name" "current-buffer" "set-buffer" "get-buffer-create" "concat"
                 "require" "provide" "add-hook" "remove-hook" "global-set-key" "define-key"
                 "kbd" "string-match" "match-string" "re-search-forward" "re-search-backward"
                 "search-forward" "looking-at" "replace-regexp-in-string" "mapconcat"
                 "number-to-string" "string-to-number" "expand-file-name"
                 "file-name-directory" "file-exists-p" "user-error" "signal" "princ"
                 "prin1" "run-hooks" "add-to-list" "plist-get" "plist-put" "alist-get"
                 "string-join" "string-trim" "split-string" "intern" "symbol-name"
                 "propertize" "face-attribute" "set-face-attribute" "called-interactively-p")))

(defparameter *scheme-keywords*
  (%string-set '("define" "define-syntax" "define-record-type" "define-values"
                 "define-library" "lambda" "let" "let*" "letrec" "letrec*" "let-values"
                 "let*-values" "let-syntax" "letrec-syntax" "syntax-rules" "syntax-case"
                 "if" "cond" "case" "and" "or" "when" "unless" "do" "begin" "delay"
                 "delay-force" "quasiquote" "quote" "unquote" "unquote-splicing" "set!"
                 "guard" "parameterize" "case-lambda" "import" "export" "library"
                 "include" "else" "=>" "assert")))

(defparameter *scheme-builtins*
  (%string-set '("display" "newline" "write" "read" "null?" "pair?" "list?" "symbol?"
                 "string?" "number?" "integer?" "procedure?" "boolean?" "vector?" "eq?"
                 "eqv?" "equal?" "zero?" "positive?" "negative?" "even?" "odd?"
                 "vector-ref" "vector-set!" "make-vector" "vector-length" "string-append"
                 "string-length" "string-ref" "substring" "string->symbol" "symbol->string"
                 "number->string" "string->number" "list->vector" "vector->list"
                 "list->string" "string->list" "for-each" "map" "call/cc"
                 "call-with-current-continuation" "call-with-values" "dynamic-wind"
                 "set-car!" "set-cdr!" "list-ref" "list-tail" "assq" "assv" "memq" "memv"
                 "quotient" "remainder" "modulo" "exact->inexact" "inexact->exact" "exit"
                 "error" "raise" "with-exception-handler")))

(defparameter *type-definers*
  (%string-set '("defclass" "defstruct" "define-condition" "deftype" "define-record-type"
                 "cl-defstruct")))

(defparameter *variable-definers*
  (%string-set '("defvar" "defparameter" "defconstant" "define-symbol-macro" "defcustom"
                 "defconst" "defvar-local" "defglobal" "define-constant" "defface"
                 "defgroup")))

(defun %prefix-p (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun %definer-kind (name dialect)
  "NIL, or what the list whose operator is NAME (downcased) defines."
  (cond ((gethash name *type-definers*) :type)
        ((gethash name *variable-definers*) :variable)
        ((and (eq dialect :scheme) (string= name "define")) :define)
        ((or (and (%prefix-p "def" name) (> (length name) 3)
                  (not (%prefix-p "default" name)))
             (%prefix-p "cl-def" name))
         :function)))

(defparameter *lambda-definers*
  (%string-set '("defun" "defmacro" "defmethod" "defgeneric" "deftype" "define-compiler-macro"
                 "define-setf-expander" "defsetf" "defsubst" "cl-defun" "cl-defmacro"
                 "cl-defmethod" "cl-defgeneric"))
  "Definitions with a lambda list (the first list after the name) and a
docstring after it.")

(defun %definer-doc (name kind)
  "(values DOC DATA-FROM) for a definition whose operator is NAME: where its
docstring sits (:AFTER-LAMBDA, an index or NIL), and the index from which its
lists are data (or NIL)."
  (cond ((member name '("defstruct" "cl-defstruct") :test #'string=) (values 2 nil))
        ((eq kind :variable) (values 3 nil))
        ((or (eq kind :define) (gethash name *lambda-definers*)) (values :after-lambda nil))
        ((member name '("defclass" "define-condition") :test #'string=) (values nil 2))
        (t (values nil nil))))

;;; The open-list stack

(defstruct (frame (:constructor make-frame (code-p)))
  (code-p t)                    ; its first atom is an operator
  (count 0 :type fixnum)        ; children started so far
  (kind nil)                    ; what it defines, see %DEFINER-KIND
  (doc nil)                     ; see %DEFINER-DOC
  (data-from nil)               ; lists from this index on are data
  (lambda-from 2 :type fixnum)  ; the lambda list is the first list at or after this
  (lambda-index nil)            ; where the lambda list was
  (name-class nil)              ; a name list: class of its first non-SETF atom
  (doc-key-p nil))              ; the previous child was :documentation            ; the previous child was :documentation

(defun %number-p (s)
  "Whether S (an atom's text) has Lisp number syntax."
  (let ((n (length s)) (i 0))
    (flet ((digits (from) (or (position-if-not #'digit-char-p s :start (min from n)) n)))
      (when (and (< i n) (find (char s i) "+-"))
        (incf i))
      (let* ((int-end (digits i))
             (int-digits (- int-end i)))
        (setf i int-end)
        (cond ((= i n) (plusp int-digits))
              ((char= (char s i) #\/)
               (let ((end (digits (1+ i))))
                 (and (plusp int-digits) (= end n) (> end (1+ i)))))
              (t
               (let ((dot nil) (frac-digits 0))
                 (when (char= (char s i) #\.)
                   (let ((end (digits (1+ i))))
                     (setf dot t frac-digits (- end (1+ i)) i end)))
                 (cond ((= i n) (and dot (or (plusp int-digits) (plusp frac-digits))))
                       ((find (char-downcase (char s i)) "esfdl")
                        (and (or (plusp int-digits) (plusp frac-digits))
                             (let ((j (1+ i)))
                               (when (and (< j n) (find (char s j) "+-"))
                                 (incf j))
                               (let ((end (digits j)))
                                 (and (= end n) (> end j))))))
                       (t nil)))))))))

(defun %literal-class (s dialect)
  "The class S (a downcased atom) has wherever it appears, or NIL."
  (let ((n (length s)))
    (cond ((%number-p s) :number)
          ((char= (char s 0) #\:) :symbol)
          ((and (char= (char s 0) #\&) (> n 1)) :type)
          ((and (not (eq dialect :scheme)) (or (string= s "t") (string= s "nil"))) :constant)
          ((and (>= n 3) (char= (char s 0) #\*) (char= (char s (1- n)) #\*)) :variable)
          ((and (>= n 3) (char= (char s 0) #\+) (char= (char s (1- n)) #\+)) :constant))))

(defun %operator-name (s)
  "(values NAME CL-P): S without a package prefix, and whether that prefix
was absent, cl or common-lisp."
  (let ((first (position #\: s))
        (last (position #\: s :from-end t)))
    (if (or (null first) (zerop first))
        (values s t)
        (values (subseq s (1+ last))
                (member (subseq s 0 first) '("cl" "common-lisp") :test #'string=)))))

(defmethod lexer-tokens ((lexer lisp-lexer) text)
  (let* ((dialect (lisp-lexer-dialect lexer))
         (brackets-p (not (eq dialect :common-lisp)))
         (keywords (case dialect (:emacs-lisp *elisp-keywords*) (:scheme *scheme-keywords*)))
         (builtins (case dialect (:emacs-lisp *elisp-builtins*) (:scheme *scheme-builtins*)))
         (sink (make-sink text))
         (n (length text))
         (pos 0)
         (stack '())
         ;; NIL, :DATA (the next list is data) or :FEATURE (a feature
         ;; expression: data, and not a child of the enclosing list)
         (pending nil))
    (labels ((terminator-p (ch)
               (or (%whitespace-p ch)
                   (find ch "()\";'`,")
                   (and brackets-p (find ch "[]{}"))))
             (atom-start-p (i)
               (let ((ch (%char-at text i)))
                 (and ch (not (terminator-p ch)) (char/= ch #\#))))
             (scan-atom (i)
               ;; I is at a constituent; returns the end of the atom.
               (loop
                 (when (>= i n) (return n))
                 (let ((ch (char text i)))
                   (cond ((char= ch #\\) (setf i (min n (+ i 2))))
                         ((char= ch #\|)
                          (setf i (loop for j from (1+ i) below n
                                        do (case (char text j)
                                             (#\\ (incf j))
                                             (#\| (return (1+ j))))
                                        finally (return n))))
                         ((terminator-p ch) (return i))
                         (t (incf i))))))
             (scan-string (i)
               (loop for j from (1+ i) below n
                     do (case (char text j)
                          (#\\ (incf j))
                          (#\" (return (1+ j))))
                     finally (return n)))
             (scan-block-comment (i)
               (let ((depth 1) (j (+ i 2)))
                 (loop while (< j n)
                       do (cond ((%looking-at text j "|#")
                                 (incf j 2)
                                 (when (zerop (decf depth))
                                   (return-from scan-block-comment j)))
                                ((%looking-at text j "#|")
                                 (incf j 2)
                                 (incf depth))
                                (t (incf j))))
                 n))
             (start-child ()
               ;; A form starts in the innermost list: (values frame index).
               (setf pending nil)
               (let ((f (first stack)))
                 (if f
                     (let ((i (frame-count f)))
                       (incf (frame-count f))
                       (setf (frame-doc-key-p f) nil)
                       (values f i))
                     (values nil 0))))
             (name-class (kind list-p)
               (if (eq kind :define)
                   (if list-p :function :variable)
                   kind))
             (operator (f s)
               ;; S is the operator of F: its class. Sets up a definition.
               (multiple-value-bind (name cl-p) (%operator-name s)
                 (let ((kind (%definer-kind name dialect)))
                   (cond (kind
                          (multiple-value-bind (doc data-from) (%definer-doc name kind)
                            (setf (frame-kind f) kind
                                  (frame-doc f) doc
                                  (frame-data-from f) data-from)))
                         ((string= name "lambda")
                          (setf (frame-doc f) :after-lambda
                                (frame-lambda-from f) 1)))
                   (cond ((or (and cl-p (gethash name *cl-keywords*))
                              (and cl-p keywords (gethash name keywords))
                              kind
                              (%prefix-p "with-" name))
                          :keyword)
                         ((or (and cl-p (gethash name *cl-functions*))
                              (and cl-p builtins (gethash name builtins)))
                          :builtin)
                         (t :plain)))))
             (atom-child (start end)
               (let ((s (string-downcase (subseq text start end))))
                 (multiple-value-bind (f i) (start-child)
                   (let* ((literal (%literal-class s dialect))
                          (name-like (member literal '(nil :variable :constant)))
                          (class
                           (cond ((null f) (or literal :plain))
                                 ((and (frame-name-class f) (not (string= s "setf")))
                                  (if name-like (shiftf (frame-name-class f) nil) literal))
                                 ((and (frame-kind f) (= i 1) name-like)
                                  (name-class (frame-kind f) nil))
                                 (literal literal)
                                 ((and (frame-code-p f) (= i 0)) (operator f s))
                                 (t :plain))))
                     (when (and f (string= s ":documentation"))
                       (setf (frame-doc-key-p f) t))
                     (emit sink class start end)))))
             (string-child (start end)
               (let ((after-documentation-p (and stack (frame-doc-key-p (first stack)))))
                 (multiple-value-bind (f i) (start-child)
                   (let ((doc-p (and f
                                     (or after-documentation-p
                                         (let ((doc (frame-doc f)))
                                           (if (eq doc :after-lambda)
                                               (and (frame-lambda-index f)
                                                    (= i (1+ (frame-lambda-index f))))
                                               (eql doc i)))))))
                     (emit sink (if doc-p :doc :string) start end)))))
             (open-list (start end)
               (let* ((feature-p (eq pending :feature))
                      (data-p (member pending '(:data :feature)))
                      (parent (first stack))
                      (g (make-frame (and (not data-p)
                                          (or (null parent) (frame-code-p parent))))))
                 (if feature-p
                     (setf pending nil)
                     (multiple-value-bind (f i) (start-child)
                       (when f
                         (cond ((and (frame-kind f) (= i 1))
                                (setf (frame-name-class g) (name-class (frame-kind f) t)
                                      (frame-code-p g) nil)
                                (when (eq (frame-kind f) :define)
                                  (setf (frame-lambda-index f) 1)))
                               ((and (eq (frame-doc f) :after-lambda)
                                     (null (frame-lambda-index f))
                                     (>= i (frame-lambda-from f)))
                                (setf (frame-lambda-index f) i
                                      (frame-code-p g) nil))
                               ((and (frame-data-from f) (>= i (frame-data-from f)))
                                (setf (frame-code-p g) nil))))))
                 (push g stack)
                 (emit sink :punctuation start end)))
             (prefix (class start end &optional next-pending)
               ;; A token that modifies the next form: not a child itself.
               (setf pending next-pending)
               (emit sink class start end))
             (other-child (class start end)
               (start-child)
               (emit sink class start end))
             (dispatch (p)
               ;; TEXT[P] is #. Returns the end of what was emitted.
               (let ((c (%char-at text (1+ p))))
                 (cond
                  ((null c) (prefix :meta p n))
                  ((char= c #\|) (emit sink :comment p (scan-block-comment p)))
                  ((char= c #\\)
                   (other-child :string p
                                (if (< (+ p 2) n)
                                    (let ((after (+ p 3)))
                                      (if (and (< after n) (not (terminator-p (char text after))))
                                          (scan-atom after)
                                          after))
                                    n)))
                  ((char= c #\')
                   (if (atom-start-p (+ p 2))
                       (other-child :function p (scan-atom (+ p 2)))
                       (prefix :meta p (+ p 2))))
                  ((char= c #\:)
                   (other-child :symbol p (if (atom-start-p (+ p 2)) (scan-atom (+ p 2)) (+ p 2))))
                  ((find c "+-")
                   (cond ((atom-start-p (+ p 2)) (prefix :meta p (scan-atom (+ p 2))))
                         ((eql (%char-at text (+ p 2)) #\() (prefix :meta p (+ p 2) :feature))
                         (t (prefix :meta p (+ p 2)))))
                  ((find c ".,") (prefix :meta p (+ p 2)))
                  ((char= c #\() (setf pending :data) (open-list p (+ p 2)))
                  ((find c "*xXbBoO") (other-child :number p (scan-atom (1+ p))))
                  ((digit-char-p c)
                   (let* ((j (%skip #'digit-char-p text (1+ p)))
                          (d (%char-at text j)))
                     (cond ((null d) (prefix :meta p j))
                           ((char-equal d #\r)
                            (other-child :number p (if (atom-start-p (1+ j)) (scan-atom (1+ j)) (1+ j))))
                           ((char= d #\=) (prefix :meta p (1+ j)))
                           ((char= d #\#) (other-child :meta p (1+ j)))
                           ((terminator-p d) (prefix :meta p j))
                           (t (prefix :meta p (1+ j) :data)))))
                  ((and (eq dialect :scheme) (char= c #\;)) (prefix :comment p (+ p 2)))
                  ((and (eq dialect :scheme) (find c "tTfF"))
                   (let ((end (scan-atom (1+ p))))
                     (if (member (string-downcase (subseq text (1+ p) end))
                                 '("t" "f" "true" "false") :test #'string=)
                         (other-child :constant p end)
                         (prefix :meta p end))))
                  ((find c "cCaAsS") (prefix :meta p (+ p 2) :data))
                  ((terminator-p c) (prefix :meta p (1+ p)))
                  (t (prefix :meta p (+ p 2)))))))
      (loop while (< pos n)
            do (let ((ch (char text pos)))
                 (setf pos
                       (cond
                        ((%whitespace-p ch) (emit sink :plain pos (%skip #'%whitespace-p text pos)))
                        ((char= ch #\;) (emit sink :comment pos (%line-end text pos)))
                        ((char= ch #\") (string-child pos (scan-string pos)))
                        ((or (char= ch #\() (and brackets-p (find ch "[{")))
                         (when (char/= ch #\() (setf pending :data))
                         (open-list pos (1+ pos)))
                        ((or (char= ch #\)) (and brackets-p (find ch "]}")))
                         (pop stack)
                         (setf pending nil)
                         (emit sink :punctuation pos (1+ pos)))
                        ((char= ch #\')
                         (if (atom-start-p (1+ pos))
                             (other-child :symbol pos (scan-atom (1+ pos)))
                             (prefix :meta pos (1+ pos) :data)))
                        ((char= ch #\`) (prefix :meta pos (1+ pos)))
                        ((char= ch #\,)
                         (prefix :meta pos (if (eql (%char-at text (1+ pos)) #\@) (+ pos 2) (1+ pos))))
                        ((char= ch #\#) (dispatch pos))
                        ((and (char= ch #\?) (eq dialect :emacs-lisp))
                         (other-child :string pos
                                      (min n (if (eql (%char-at text (1+ pos)) #\\) (+ pos 3) (+ pos 2)))))
                        (t (atom-child pos (scan-atom pos)))))))
      (sink-result sink))))
