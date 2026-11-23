;;;; Python.
;;;;
;;;;   whitespace :plain;  # … end of line :comment
;;;;   strings, with an optional prefix of up to two of r b u f (any case):
;;;;     '…' "…" end at their quote or at the end of the line (\ escapes,
;;;;     \ newline continues); '''…''' """…""" span lines. A triple-quoted
;;;;     string that starts a line is :doc, any other string :string
;;;;   numbers: 0x 0o 0b, decimals with _ . exponents and a j suffix :number
;;;;   names (Unicode letters, digits, _):
;;;;     after def :function; after class :type; after . :plain
;;;;     True False None Ellipsis NotImplemented :constant; keywords :keyword
;;;;     self cls :variable; builtin functions and exceptions :builtin
;;;;   @name.attr starting a line :meta (a decorator); @ elsewhere :operator
;;;;   operators (longest first) :operator; ( ) [ ] { } , : ; . \ :punctuation

(in-package #:highlight-protocol)

(defclass python-lexer (lexer) ()
  (:documentation "Python 3 source."))

(defparameter *python-keywords*
  (%string-set '("and" "as" "assert" "async" "await" "break" "class" "continue" "def" "del"
                 "elif" "else" "except" "finally" "for" "from" "global" "if" "import" "in"
                 "is" "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try" "while"
                 "with" "yield")))

(defparameter *python-constants*
  (%string-set '("True" "False" "None" "Ellipsis" "NotImplemented" "__debug__")))

(defparameter *python-builtins*
  (%string-set
   '("abs" "aiter" "all" "anext" "any" "ascii" "bin" "bool" "breakpoint" "bytearray" "bytes"
     "callable" "chr" "classmethod" "compile" "complex" "delattr" "dict" "dir" "divmod"
     "enumerate" "eval" "exec" "filter" "float" "format" "frozenset" "getattr" "globals"
     "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance" "issubclass" "iter" "len"
     "list" "locals" "map" "max" "memoryview" "min" "next" "object" "oct" "open" "ord" "pow"
     "print" "property" "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
     "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip" "__import__"
     "BaseException" "BaseExceptionGroup" "Exception" "ExceptionGroup" "ArithmeticError"
     "AssertionError" "AttributeError" "BlockingIOError" "BrokenPipeError" "BufferError"
     "ChildProcessError" "ConnectionError" "ConnectionAbortedError" "ConnectionRefusedError"
     "ConnectionResetError" "EOFError" "FileExistsError" "FileNotFoundError"
     "FloatingPointError" "GeneratorExit" "ImportError" "IndentationError" "IndexError"
     "InterruptedError" "IsADirectoryError" "KeyError" "KeyboardInterrupt" "LookupError"
     "MemoryError" "ModuleNotFoundError" "NameError" "NotADirectoryError"
     "NotImplementedError" "OSError" "OverflowError" "PermissionError" "ProcessLookupError"
     "RecursionError" "ReferenceError" "RuntimeError" "StopAsyncIteration" "StopIteration"
     "SyntaxError" "SystemError" "SystemExit" "TabError" "TimeoutError" "TypeError"
     "UnboundLocalError" "UnicodeError" "UnicodeDecodeError" "UnicodeEncodeError"
     "ValueError" "ZeroDivisionError" "Warning" "DeprecationWarning" "FutureWarning"
     "RuntimeWarning" "SyntaxWarning" "UserWarning")))

(defparameter *python-operators*
  '("**=" "//=" ">>=" "<<=" "**" "//" "<<" ">>" "<=" ">=" "==" "!=" "->" "+=" "-=" "*=" "/="
    "%=" "&=" "|=" "^=" "@=" ":=" "+" "-" "*" "/" "%" "&" "|" "^" "~" "<" ">" "=" "@" "!")
  "Longest first.")

(defun %python-name-start-p (ch)
  (or (char= ch #\_) (alpha-char-p ch)))

(defun %python-name-char-p (ch)
  (or (char= ch #\_) (alphanumericp ch)))

(defun %python-string-quote (text pos)
  "The position of the opening quote when a string literal starts at POS."
  (let ((q (%skip (lambda (ch) (find ch "rRbBuUfF")) text pos (min (length text) (+ pos 2)))))
    (and (find (%char-at text q) "'\"")
         q)))

(defun %python-string-end (text q)
  "The end of the string literal whose opening quote is at Q, and whether it
is triple-quoted."
  (let* ((n (length text))
         (quote (char text q))
         (triple (make-string 3 :initial-element quote)))
    (if (%looking-at text q triple)
        (values (loop with i = (+ q 3)
                      while (< i n)
                      do (cond ((char= (char text i) #\\) (incf i 2))
                               ((%looking-at text i triple) (return (+ i 3)))
                               (t (incf i)))
                      finally (return n))
                t)
        (values (loop with i = (1+ q)
                      while (< i n)
                      do (let ((ch (char text i)))
                           (cond ((char= ch #\\) (incf i 2))
                                 ((char= ch quote) (return (1+ i)))
                                 ((char= ch #\Newline) (return i))
                                 (t (incf i))))
                      finally (return n))
                nil))))

(defun %python-number-end (text pos)
  (let ((n (length text)))
    (flet ((digits (i chars) (%skip (lambda (ch) (find ch chars)) text i)))
      (if (and (eql (%char-at text pos) #\0) (find (%char-at text (1+ pos)) "xXoObB"))
          (digits (+ pos 2) "0123456789abcdefABCDEF_")
          (let ((i (digits pos "0123456789_")))
            (when (eql (%char-at text i) #\.)
              (setf i (digits (1+ i) "0123456789_")))
            (when (and (find (%char-at text i) "eE")
                       (let ((j (if (find (%char-at text (1+ i)) "+-") (+ i 2) (1+ i))))
                         (and (< j n) (digit-char-p (char text j))
                              (setf i (digits j "0123456789_"))))))
            (if (find (%char-at text i) "jJlL") (1+ i) i))))))

(defmethod lexer-tokens ((lexer python-lexer) text)
  (let ((sink (make-sink text))
        (n (length text))
        (pos 0)
        (line-start-p t)       ; only whitespace so far on this line
        (after nil)            ; "def" / "class" right before the next name
        (after-dot-p nil))
    (flet ((token (class end)
             (setf line-start-p nil after nil after-dot-p nil)
             (emit sink class pos end)))
      (loop while (< pos n)
            do (let ((ch (char text pos)))
                 (setf pos
                       (cond
                         ((%whitespace-p ch)
                          (let ((end (%skip #'%whitespace-p text pos)))
                            (when (find #\Newline text :start pos :end end)
                              (setf line-start-p t))
                            (emit sink :plain pos end)))
                         ((char= ch #\#) (emit sink :comment pos (%line-end text pos)))
                         ((%python-string-quote text pos)
                          (multiple-value-bind (end triple-p)
                              (%python-string-end text (%python-string-quote text pos))
                            (token (if (and triple-p line-start-p) :doc :string) end)))
                         ((or (digit-char-p ch)
                              (and (char= ch #\.) (digit-char-p (or (%char-at text (1+ pos)) #\x))))
                          (token :number (%python-number-end text pos)))
                         ((%python-name-start-p ch)
                          (let* ((end (%skip #'%python-name-char-p text pos))
                                 (name (subseq text pos end))
                                 (class (cond ((equal after "def") :function)
                                              ((equal after "class") :type)
                                              (after-dot-p :plain)
                                              ((gethash name *python-constants*) :constant)
                                              ((gethash name *python-keywords*) :keyword)
                                              ((member name '("self" "cls") :test #'string=)
                                               :variable)
                                              ((gethash name *python-builtins*) :builtin)
                                              (t :plain))))
                            (prog1 (token class end)
                              (when (member name '("def" "class") :test #'string=)
                                (setf after name)))))
                         ((and (char= ch #\@) line-start-p)
                          (token :meta (%skip (lambda (c) (or (char= c #\.) (%python-name-char-p c)))
                                              text (1+ pos))))
                         ((find-if (lambda (op) (%looking-at text pos op)) *python-operators*)
                          (token :operator
                                 (+ pos (length (find-if (lambda (op) (%looking-at text pos op))
                                                         *python-operators*)))))
                         ((find ch "()[]{},:;.\\")
                          (prog1 (token :punctuation (1+ pos))
                            (setf after-dot-p (char= ch #\.))))
                         (t (token :plain (1+ pos)))))))
      (sink-result sink))))
