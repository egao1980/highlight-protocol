(in-package #:highlight-protocol/tests)

(deftest python-function
  (ok (equal '((:keyword . "def") (:plain . " ") (:function . "greet") (:punctuation . "(")
               (:plain . "name") (:punctuation . "):") (:plain . "
    ")
               (:doc . "\"\"\"Say hi.\"\"\"") (:plain . "
    ")
               (:builtin . "print") (:punctuation . "(") (:string . "f\"hi {name}\"")
               (:punctuation . ")") (:plain . "  ") (:comment . "# done") (:plain . "
"))
             (toks "python"
                   "def greet(name):"
                   "    \"\"\"Say hi.\"\"\""
                   "    print(f\"hi {name}\")  # done"
                   ""))))

(deftest python-class
  (ok (equal '((:meta . "@dataclass") (:plain . "
") (:keyword . "class") (:plain . " ")
               (:type . "Point") (:punctuation . "(") (:plain . "Base") (:punctuation . "):")
               (:plain . "
    x") (:punctuation . ":") (:plain . " ") (:builtin . "int") (:plain . " ")
               (:operator . "=") (:plain . " ") (:number . "0x1F") (:plain . "
    y ")
               (:operator . "=") (:plain . " ") (:constant . "None"))
             (toks "python"
                   "@dataclass"
                   "class Point(Base):"
                   "    x: int = 0x1F"
                   "    y = None"))))

(deftest python-operators
  (ok (equal '((:plain . "a ") (:operator . "**=") (:plain . " b ") (:operator . "//")
               (:plain . " ") (:number . "2") (:plain . " ") (:operator . "->") (:plain . " c ")
               (:operator . "!=") (:plain . " d ") (:operator . "@") (:plain . " e"))
             (toks "python" "a **= b // 2 -> c != d @ e"))))

(deftest python-names
  (ok (equal '((:plain . "x") (:punctuation . ".") (:plain . "list") (:punctuation . "(")
               (:variable . "self") (:punctuation . ")"))
             (toks "python" "x.list(self)"))
      "an attribute is not a builtin")
  (let ((tokens (toks "python" "async def run(): await sleep(1); return True or False")))
    (ok (eq :keyword (class-of-piece tokens "async")))
    (ok (eq :keyword (class-of-piece tokens "await")))
    (ok (eq :function (class-of-piece tokens "run")))
    (ok (eq :keyword (class-of-piece tokens "return")))
    (ok (eq :constant (class-of-piece tokens "True")))
    (ok (eq :keyword (class-of-piece tokens "or"))))
  (ok (eq :builtin (class-of-piece (toks "python" "raise ValueError(x)") "ValueError")))
  (ok (equal '((:plain . "naïve") (:operator . "=") (:number . "1"))
             (toks "python" "naïve=1"))
      "unicode names"))

(deftest-parametrize python-numbers
    ((text)
     "0" "42" "1_000" "3.14" ".5" "1e-3" "2E10" "0x1F" "0o17" "0b1010" "1j" "2.5J")
  (ok (equal (list (cons :number text)) (tokenize text "python"))))

(deftest python-strings
  (ok (equal '((:plain . "s ") (:operator . "=") (:plain . " ") (:string . "'''x'''"))
             (toks "python" "s = '''x'''"))
      "a triple-quoted value is a string, not a docstring")
  (ok (equal '((:string . "rb'\\d'") (:plain . " ") (:string . "b\"x\"") (:plain . " ")
               (:string . "'it\\'s'"))
             (toks "python" "rb'\\d' b\"x\" 'it\\'s'")))
  (ok (equal '((:doc . "'''a
b'''")) (toks "python" "'''a" "b'''"))))

(deftest python-truncated
  (ok (equal '((:plain . "x ") (:operator . "=") (:plain . " ") (:string . "'abc")
               (:plain . "
y ") (:operator . "=") (:plain . " ") (:number . "1"))
             (toks "python" "x = 'abc" "y = 1"))
      "a one-line string ends at the end of its line")
  (ok (equal '((:doc . "\"\"\"open
more")) (toks "python" "\"\"\"open" "more")))
  (ok (equal '((:plain . "x ") (:operator . "=") (:plain . " ") (:string . "\"\"\"")) (toks "python" "x = \"\"\"")))
  (ok (equal '((:meta . "@")) (toks "python" "@"))))
