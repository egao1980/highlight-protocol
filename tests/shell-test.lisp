(in-package #:highlight-protocol/tests)

(deftest shell-script
  (ok (equal '((:meta . "#!/bin/sh") (:plain . "
") (:comment . "# comment") (:plain . "
")
               (:builtin . "export") (:plain . " ") (:variable . "FOO") (:operator . "=")
               (:string . "\"a ") (:variable . "$HOME") (:string . " b\"") (:plain . "
")
               (:keyword . "if") (:plain . " ") (:punctuation . "[") (:plain . " -n ")
               (:string . "\"") (:variable . "$1") (:string . "\"") (:plain . " ")
               (:punctuation . "]") (:operator . ";") (:plain . " ") (:keyword . "then")
               (:plain . " ") (:builtin . "echo") (:plain . " hi ") (:operator . "|")
               (:plain . " grep -v x ") (:operator . ">") (:plain . " /dev/null ")
               (:number . "2") (:operator . ">&") (:number . "1") (:operator . ";")
               (:plain . " ") (:keyword . "fi") (:plain . "
"))
             (toks "sh"
                   "#!/bin/sh"
                   "# comment"
                   "export FOO=\"a $HOME b\""
                   "if [ -n \"$1\" ]; then echo hi | grep -v x > /dev/null 2>&1; fi"
                   ""))))

(deftest shell-functions-and-loops
  (ok (equal '((:function . "greet") (:punctuation . "()") (:plain . " ") (:punctuation . "{")
               (:plain . " ") (:builtin . "local") (:plain . " ") (:variable . "name")
               (:operator . "=") (:variable . "${1:-world}") (:operator . ";") (:plain . " ")
               (:builtin . "echo") (:plain . " ") (:string . "\"hi ") (:variable . "$name")
               (:string . "\"") (:operator . ";") (:plain . " ") (:punctuation . "}")
               (:plain . "
") (:keyword . "for") (:plain . " f ") (:keyword . "in") (:plain . " *.txt")
               (:operator . ";") (:plain . " ") (:keyword . "do") (:plain . " rm ")
               (:string . "\"") (:variable . "$f") (:string . "\"") (:operator . ";")
               (:plain . " ") (:keyword . "done"))
             (toks "bash"
                   "greet() { local name=${1:-world}; echo \"hi $name\"; }"
                   "for f in *.txt; do rm \"$f\"; done")))
  (let ((tokens (toks "bash" "function deploy { cd /srv && make install; }")))
    (ok (eq :keyword (class-of-piece tokens "function")))
    (ok (eq :function (class-of-piece tokens "deploy")))
    (ok (eq :builtin (class-of-piece tokens "cd")))
    (ok (eq :operator (class-of-piece tokens "&&"))))
  (ok (eq :plain (class-of-piece (toks "sh" "echo done") " done"))
      "a keyword is a keyword only where a command starts"))

(deftest shell-substitutions
  (ok (equal '((:operator . "$(") (:plain . "date +%s") (:punctuation . ")") (:plain . " ")
               (:operator . "`") (:plain . "id") (:operator . "`"))
             (toks "sh" "$(date +%s) `id`")))
  (ok (equal '((:variable . "$?") (:plain . " ") (:variable . "$#") (:plain . " ")
               (:variable . "$@") (:plain . " ") (:string . "'$x'") (:plain . " ")
               (:string . "$'a\\'b'"))
             (toks "sh" "$? $# $@ '$x' $'a\\'b'")))
  (ok (equal '((:plain . "a#b ") (:comment . "#c")) (toks "sh" "a#b #c"))
      "# starts a comment only at a word start"))

(deftest shell-heredoc
  (ok (equal '((:plain . "cat ") (:operator . "<<") (:string . "'EOF'") (:plain . "
")
               (:string . "hello $x
EOF") (:plain . "
") (:builtin . "echo") (:plain . " after"))
             (toks "sh" "cat <<'EOF'" "hello $x" "EOF" "echo after")))
  (ok (equal '((:plain . "cat ") (:operator . "<<-") (:string . "END") (:plain . "
")
               (:string . "	body
	END") (:plain . "
") (:builtin . "echo"))
             (toks "sh" "cat <<-END" "	body" "	END" "echo"))
      "<<- strips leading tabs from the delimiter line")
  (ok (equal '((:plain . "cat ") (:operator . "<<") (:string . "EOF") (:plain . "
")
               (:string . "never closed
echo no"))
             (toks "sh" "cat <<EOF" "never closed" "echo no"))
      "an unterminated heredoc runs to the end")
  (ok (equal '((:plain . "cat ") (:operator . "<<<") (:plain . " word"))
             (toks "sh" "cat <<< word"))
      "<<< is a here-string"))

(deftest shell-truncated
  (ok (equal '((:builtin . "echo") (:plain . " ") (:string . "\"unterminated ") (:variable . "$x"))
             (toks "sh" "echo \"unterminated $x")))
  (ok (equal '((:builtin . "echo") (:plain . " ") (:string . "'open")) (toks "sh" "echo 'open")))
  (ok (equal '((:variable . "${open")) (toks "sh" "${open")))
  (ok (equal '((:plain . "$")) (toks "sh" "$")))
  (ok (equal '((:plain . "\\")) (toks "sh" "\\"))))

(deftest console-session
  (ok (equal '((:meta . "$ ") (:plain . "ls -la
file.txt
") (:meta . "# ") (:plain . "whoami
root
")
               (:meta . "(venv) user@host:~$ ") (:builtin . "echo") (:plain . " ")
               (:variable . "$HOME") (:plain . "
50% done"))
             (toks "console"
                   "$ ls -la"
                   "file.txt"
                   "# whoami"
                   "root"
                   "(venv) user@host:~$ echo $HOME"
                   "50% done"))))
