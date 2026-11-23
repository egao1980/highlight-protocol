(in-package #:highlight-protocol/tests)

;;; Every lexer on every input: never signals, tokens concatenate to the
;;; input, classes come from the fixed set, and time is linear.

(defparameter *samples*
  (list (lines "(defun foo (x &optional (y 1)) \"Doc.\" #+sbcl (car x) #| c |# ; c"
               "  `(a ,b ,@c) #'car #\\( #\\) #x1F #:g |a b| 'q #1=(x . #1#))"
               "(defclass p () ((x :documentation \"X\"))) (define (f x) [x]) ?a ?\\n")
        (lines "@decorator"
               "class A(B):"
               "    '''Doc.'''"
               "    def f(self, x=0x1F, *a, **k) -> None:"
               "        return f\"{x!r}\" + r'\\d' + b\"\\x00\"  # c"
               "x = '''open")
        (lines "#!/bin/bash"
               "f() { local a=${1:-x}; echo \"$a $(date)\" | tee >(cat) 2>&1; }"
               "cat <<-'EOF' <<X"
               "	body $x"
               "	EOF"
               "X"
               "for i in 1 2; do [[ $i == 1 ]] && echo 'one' || echo $'two\\n'; done # c"
               "$ ls"
               "(venv) me@box:~$ make")
        (lines "{\"a\": [1, -2.5e3, true, null, {\"b\": \"c\\\"d\"}], // c"
               " /* block */ \"open")
        (lines "%YAML 1.2"
               "---"
               "a: &x {b: [1, 'c''d', \"e\\\"f\"], g: *x}"
               "- !!str ? key"
               "  : value # c"
               "h: |+2"
               "    text"
               "i: >"
               "j: \"open")
        (lines "diff --git a/x b/x"
               "--- a/x"
               "+++ b/x"
               "@@ -1,3 +1,2 @@ f"
               " a"
               "-b"
               "+c"
               "\\ No newline at end of file"
               "@@ broken"
               "-d")
        ;; malformed / truncated fragments
        "\"" "\\" "#" "#|" "|" "'" "`" "," ",@" "#\\" "#+" "#'" "?" "?\\" "\"\\" "'''" "\"\"\""
        "$" "${" "$(" "<<" "<<-" "<<'" "@@" "@@ -" "@@ -1,x +" "---" "- " ": " "&" "*" "!"
        "{" "[" "}" "]" "/*" "//" "-" "+" "0x" "1e" "1." ".e" "#x" "#36r" "#1" "#1="
        (string #\Tab) (string #\Return) (format nil "~C~C" #\Return #\Newline)
        (coerce '(#\a #\Nul #\b) 'string)
        "naïve ünïcödé λ → ∀"))

(defun every-prefix-ok (lexer-name text)
  (loop for end from 0 to (length text)
        for prefix = (subseq text 0 end)
        always (well-formed-p (tokenize prefix lexer-name) prefix)))

(deftest-parametrize every-lexer-every-prefix
    ((lexer-name) :rows *lexer-names*)
  (dolist (sample *samples*)
    (ok (every-prefix-ok lexer-name sample)
        (format nil "~A on ~S" lexer-name (subseq sample 0 (min 30 (length sample)))))))

(defun pseudo-random-strings (count seed)
  "COUNT strings over a syntax-heavy alphabet, from a fixed LCG SEED."
  (let ((alphabet (concatenate 'string "()[]{}\"'`,;:#|\\@$&*!?<>=+-./%~^ "
                               (string #\Newline) (string #\Tab) "abcxyz019"
                               "_" "λ"))
        (state seed))
    (flet ((next (n)
             (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
             (mod (ash state -8) n)))
      (loop repeat count
            collect (let ((len (next 120)))
                      (coerce (loop repeat len collect (char alphabet (next (length alphabet))))
                              'string))))))

(deftest-parametrize fuzz-never-signals
    ((lexer-name) :rows *lexer-names*)
  (ok (every (lambda (text) (well-formed-p (tokenize text lexer-name) text))
             (pseudo-random-strings 400 (sxhash lexer-name)))))

;;; Linear time: pathological inputs of 200k characters lex in well under a
;;; second each when linear; a quadratic lexer would take minutes.

(defun repeat-string (piece times)
  (with-output-to-string (s)
    (loop repeat times do (write-string piece s))))

(defparameter *pathological*
  (list (repeat-string "(" 200000)
        (repeat-string ")" 200000)
        (repeat-string "#|" 100000)
        (repeat-string "\"\\" 100000)
        (repeat-string "'" 200000)
        (repeat-string "#" 200000)
        (repeat-string "(defun f (x) \"d\" " 20000)
        (repeat-string "'''" 70000)
        (repeat-string "\"a\"      " 25000)
        (repeat-string "<<EOF" 40000)
        (concatenate 'string "cat <<EOF" (string #\Newline) (repeat-string "x " 100000))
        (repeat-string "${" 100000)
        (repeat-string "- " 100000)
        (repeat-string "[{" 100000)
        (concatenate 'string "k: |" (string #\Newline) (repeat-string (format nil "  l~%") 50000))
        (concatenate 'string "@@ -1,100000 +1,100000 @@" (string #\Newline)
                     (repeat-string (format nil "-a~%+b~%") 50000))
        (repeat-string " " 200000)
        (make-string 200000 :initial-element #\a)))

(deftest-parametrize linear-time
    ((lexer-name) :rows *lexer-names*)
  (dolist (text *pathological*)
    (let* ((start (get-internal-real-time))
           (tokens (tokenize text lexer-name))
           (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
      (ok (and (string= text (concat tokens)) (< seconds 10))
          (format nil "~A on ~S… (~,2Fs)" lexer-name (subseq text 0 12) seconds)))))
