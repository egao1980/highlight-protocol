;;;; POSIX shell / bash / zsh, and console sessions.
;;;;
;;;; Shell:
;;;;   #! on the first line :meta; # at the start of a word … end of line :comment
;;;;   '…' :string;  $'…' (\ escapes) :string;  "…" and $"…" :string with
;;;;   $name, ${…}, $1, $@ … inside as :variable; unterminated quotes run to the end
;;;;   $name ${…} $0-9 $@ $# $? $$ $! $* $- :variable;  $( and ` :operator
;;;;   << and <<- (not <<<) read a delimiter word (:string, quotes allowed);
;;;;   the lines after the current one, up to and including the delimiter
;;;;   line (tabs stripped for <<-), are :string; several queue in order
;;;;   operators ; ;; ;& | || & && |& < > >> <& >& &> <<< … :operator; ( ) :punctuation
;;;;   words:
;;;;     NAME= or NAME+= at a word start: NAME :variable, = :operator
;;;;     digits only :number
;;;;     where a command starts (line start, after ; | & ( ) { $( ` and
;;;;     after if then else elif do while until time !):
;;;;       { } [ [[ :punctuation; keywords :keyword; builtins :builtin;
;;;;       NAME followed by () :function; other commands :plain
;;;;     ] ]] :punctuation; the word after `function` :function; `in` after
;;;;     for / select / case and one word :keyword; anything else :plain
;;;;   \c :plain (an escaped character)
;;;;
;;;; Console (console-lexer): a line that starts with a prompt — "$ ", "# ",
;;;; "% ", "> ", or a word containing @ : or ~ and ending in $ # % followed
;;;; by a space, optionally after a "(venv) " — has the prompt as :meta and
;;;; the rest lexed as a shell command; other lines are output, :plain.

(in-package #:highlight-protocol)

(defclass shell-lexer (lexer) ()
  (:documentation "Shell scripts (sh, bash, zsh)."))

(defclass console-lexer (shell-lexer) ()
  (:documentation "A terminal session: prompts, commands (as shell) and output."))

(defparameter *shell-keywords*
  (%string-set '("if" "then" "else" "elif" "fi" "case" "esac" "for" "select" "while" "until"
                 "do" "done" "in" "function" "time" "coproc" "!")))

(defparameter *shell-builtins*
  (%string-set '("alias" "bg" "bind" "break" "builtin" "caller" "cd" "command" "compgen"
                 "complete" "continue" "declare" "dirs" "disown" "echo" "enable" "eval"
                 "exec" "exit" "export" "false" "fc" "fg" "getopts" "hash" "help" "history"
                 "jobs" "kill" "let" "local" "logout" "mapfile" "popd" "printf" "pushd" "pwd"
                 "read" "readarray" "readonly" "return" "set" "shift" "shopt" "source"
                 "suspend" "test" "times" "trap" "true" "type" "typeset" "ulimit" "umask"
                 "unalias" "unset" "wait" "." ":")))

(defparameter *shell-operators*
  '(";;&" "&>>" "<<<" "<<-" ";;" ";&" "&&" "||" "|&" "<<" ">>" "<&" ">&" "&>" ">|" "<>"
    "|" "&" ";" "<" ">")
  "Longest first.")

(defun %shell-meta-p (ch)
  "Characters that end a word."
  (or (%whitespace-p ch) (find ch "|&;()<>")))

(defun %shell-name-start-p (ch)
  (or (char= ch #\_) (and (char< ch (code-char 128)) (alpha-char-p ch))))

(defun %shell-name-char-p (ch)
  (or (%shell-name-start-p ch) (digit-char-p ch)))

(defun %shell-variable-end (text pos end)
  "The end of the expansion $… at POS, or NIL when $ starts none."
  (let ((c (and (< (1+ pos) end) (char text (1+ pos)))))
    (cond ((null c) nil)
          ((char= c #\{)
           (let ((close (position #\} text :start (+ pos 2) :end end)))
             (if close (1+ close) end)))
          ((%shell-name-start-p c) (%skip #'%shell-name-char-p text (+ pos 2) end))
          ((or (digit-char-p c) (find c "@#?$!*-")) (+ pos 2))
          (t nil))))

(defun %heredoc-end (text start end delimiter strip-tabs-p)
  "The end of the heredoc body starting at START: the end of its delimiter
line, or END."
  (loop with ls = start
        while (< ls end)
        do (let* ((le (%line-end text ls end))
                  (from (if strip-tabs-p (%skip (lambda (ch) (char= ch #\Tab)) text ls le) ls))
                  (to (if (and (> le from) (char= (char text (1- le)) #\Return)) (1- le) le)))
             (when (string= delimiter text :start2 from :end2 to)
               (return le))
             (setf ls (1+ le)))
        finally (return end)))

(defun %lex-shell (text start end sink)
  "Shell tokens for TEXT[START, END) into SINK."
  (let ((pos start)
        (command-p t)        ; a command starts here
        (redirect-p nil)     ; the next word is a redirection target
        (function-name-p nil)
        (in-state nil)       ; NIL, :NAME (after for/case) or :IN ("in" may follow)
        (backquote-p nil)
        (heredocs '()))      ; (delimiter . strip-tabs-p), newest first
    (labels ((word-like ()
               ;; A word, string or expansion was read.
               (setf in-state (if (eq in-state :name) :in nil)
                     function-name-p nil
                     redirect-p nil))
             (argument (class from to)
               ;; Emit a word-like token that is not a command name.
               (prog1 (emit sink class from to)
                 (word-like)
                 (setf command-p nil)))
             (char-at (i) (and (< -1 i end) (char text i)))
             (operator-at (p)
               (find-if (lambda (op) (and (<= (+ p (length op)) end) (%looking-at text p op)))
                        *shell-operators*))
             (scan-double (p seg)
               ;; A "…" string whose quote is at P; SEG is where its first
               ;; :string piece starts. Returns the end.
               (let ((i (1+ p)))
                 (loop
                   (when (>= i end) (return (emit sink :string seg end)))
                   (let ((ch (char text i)))
                     (cond ((char= ch #\\) (incf i 2))
                           ((char= ch #\") (return (emit sink :string seg (1+ i))))
                           ((and (char= ch #\$) (%shell-variable-end text i end))
                            (let ((var-end (%shell-variable-end text i end)))
                              (emit sink :string seg i)
                              (emit sink :variable i var-end)
                              (setf seg var-end i var-end)))
                           (t (incf i)))))))
             (scan-single (p)
               (let ((close (position #\' text :start (1+ p) :end end)))
                 (if close (1+ close) end)))
             (scan-ansi (p)
               ;; $'…' from the $ at P.
               (loop with i = (+ p 2)
                     while (< i end)
                     do (case (char text i)
                          (#\\ (incf i 2))
                          (#\' (return (1+ i)))
                          (t (incf i)))
                     finally (return end)))
             (scan-word (p)
               (loop with i = p
                     while (< i end)
                     do (let ((ch (char text i)))
                          (cond ((char= ch #\\) (incf i 2))
                                ((or (%shell-meta-p ch) (find ch "\"'$`")) (return i))
                                (t (incf i))))
                     finally (return (min i end))))
             (heredoc-delimiter (p)
               ;; After << ending at P: (values start end text) of the
               ;; delimiter word, or NIL.
               (let* ((q (%skip #'%blank-p text p end))
                      (c (char-at q)))
                 (cond ((null c) nil)
                       ((find c "'\"")
                        (let ((close (position c text :start (1+ q) :end end)))
                          (values q (if close (1+ close) end)
                                  (subseq text (1+ q) (or close end)))))
                       ((%shell-meta-p c) nil)
                       (t (let ((e (scan-word q)))
                            (and (> e q)
                                 (values q e (remove #\\ (subseq text q e)))))))))
             (assignment-end (p e)
               ;; NAME= / NAME+= at P: the end of NAME, or NIL.
               (when (%shell-name-start-p (char text p))
                 (let ((k (%skip #'%shell-name-char-p text p e)))
                   (cond ((eql (char-at k) #\=) k)
                         ((and (eql (char-at k) #\+) (eql (char-at (1+ k)) #\=) (< (1+ k) e)) k)))))
             (function-parens-p (e)
               (let ((q (%skip #'%blank-p text e end)))
                 (and (eql (char-at q) #\()
                      (eql (char-at (%skip #'%blank-p text (1+ q) end)) #\)))))
             (word (p e)
               (let ((w (subseq text p e))
                     (assign (assignment-end p e)))
                 (cond
                   (assign
                    (let ((op-end (+ assign (if (char= (char text assign) #\+) 2 1))))
                      (emit sink :variable p assign)
                      (emit sink :operator assign op-end)
                      (emit sink :plain op-end e)
                      (setf function-name-p nil redirect-p nil)))
                   ((every #'digit-char-p w) (argument :number p e))
                   (redirect-p (argument :plain p e))
                   (function-name-p
                    (emit sink :function p e)
                    (setf function-name-p nil command-p t in-state nil))
                   ((and (eq in-state :in) (string= w "in"))
                    (emit sink :keyword p e)
                    (setf in-state nil command-p nil))
                   (command-p
                    (cond ((string= w "{") (emit sink :punctuation p e))
                          ((member w '("}" "[" "[[") :test #'string=)
                           (emit sink :punctuation p e)
                           (setf command-p nil))
                          ((gethash w *shell-keywords*)
                           (emit sink :keyword p e)
                           (cond ((member w '("for" "select" "case") :test #'string=)
                                  (setf command-p nil in-state :name))
                                 ((string= w "function")
                                  (setf command-p nil function-name-p t))
                                 ((member w '("fi" "done" "esac" "in") :test #'string=)
                                  (setf command-p nil))))
                          ((function-parens-p e)
                           (emit sink :function p e)
                           (setf command-p nil))
                          ((gethash w *shell-builtins*) (argument :builtin p e))
                          (t (argument :plain p e))))
                   ((member w '("]" "]]") :test #'string=) (emit sink :punctuation p e))
                   (t (argument :plain p e)))
                 e))
             (operator (p op)
               (let ((op-end (+ p (length op))))
                 (emit sink :operator p op-end)
                 (cond ((member op '("<<" "<<-") :test #'string=)
                        (multiple-value-bind (q delimiter-end delimiter) (heredoc-delimiter op-end)
                          (if q
                              (progn (push (cons delimiter (string= op "<<-")) heredocs)
                                     (emit sink :plain op-end q)
                                     (emit sink :string q delimiter-end))
                              op-end)))
                       ((or (find (char op 0) "<>")
                            (and (find #\& op) (find (char op (1- (length op))) "<>")))
                        (setf redirect-p t)
                        op-end)
                       (t (setf command-p t redirect-p nil)
                          op-end))))
             (newline (p)
               ;; The newline at P, then the bodies of the heredocs it starts.
               (emit sink :plain p (1+ p))
               (setf command-p t redirect-p nil)
               (let ((q (1+ p)))
                 (loop for (delimiter . strip-p) in (nreverse heredocs)
                       for first = t then nil
                       while (< q end)
                       do (unless first
                            (emit sink :plain q (1+ q))
                            (incf q))
                          (let ((body-end (%heredoc-end text q end delimiter strip-p)))
                            (emit sink :string q body-end)
                            (setf q body-end)))
                 (setf heredocs '())
                 q)))
      (loop while (< pos end)
            do (let ((ch (char text pos)))
                 (setf pos
                       (cond
                         ((char= ch #\Newline) (newline pos))
                         ((%blank-p ch) (emit sink :plain pos (%skip #'%blank-p text pos end)))
                         ((and (char= ch #\#) (= pos 0) (eql (char-at 1) #\!))
                          (emit sink :meta pos (%line-end text pos end)))
                         ((and (char= ch #\#)
                               (or (= pos start)
                                   (let ((prev (char text (1- pos))))
                                     (or (%whitespace-p prev) (find prev ";&|()")))))
                          (emit sink :comment pos (%line-end text pos end)))
                         ((char= ch #\') (argument :string pos (scan-single pos)))
                         ((char= ch #\")
                          (prog1 (scan-double pos pos) (word-like) (setf command-p nil)))
                         ((char= ch #\$)
                          (let ((c (char-at (1+ pos))))
                            (cond ((eql c #\') (argument :string pos (scan-ansi pos)))
                                  ((eql c #\")
                                   (prog1 (scan-double (1+ pos) pos) (word-like) (setf command-p nil)))
                                  ((eql c #\()
                                   (setf command-p t)
                                   (emit sink :operator pos (+ pos 2)))
                                  ((%shell-variable-end text pos end)
                                   (argument :variable pos (%shell-variable-end text pos end)))
                                  (t (argument :plain pos (1+ pos))))))
                         ((char= ch #\`)
                          (setf backquote-p (not backquote-p)
                                command-p backquote-p)
                          (emit sink :operator pos (1+ pos)))
                         ((find ch "()")
                          (setf command-p t)
                          (emit sink :punctuation pos (1+ pos)))
                         ((operator-at pos) (operator pos (operator-at pos)))
                         ((char= ch #\\) (argument :plain pos (min end (+ pos 2))))
                         (t (word pos (scan-word pos)))))))
      pos)))

(defmethod lexer-tokens ((lexer shell-lexer) text)
  (let ((sink (make-sink text)))
    (%lex-shell text 0 (length text) sink)
    (sink-result sink)))

(defun %console-prompt-end (text ls le)
  "The end of the prompt that starts the line TEXT[LS, LE), or NIL."
  (flet ((prompt-at (i)
           (when (< i le)
             (let ((c (char text i)))
               (if (and (find c "$#%>")
                        (or (= (1+ i) le) (char= (char text (1+ i)) #\Space)))
                   (min le (+ i 2))
                   (let* ((k (or (position-if #'%whitespace-p text :start i :end le) le))
                          (last (and (> k (1+ i)) (char text (1- k)))))
                     (and last (find last "$#%")
                          (find-if (lambda (ch) (find ch "@:~")) text :start i :end (1- k))
                          (min le (1+ k)))))))))
    (or (and (eql (%char-at text ls) #\()
             (let ((close (position #\) text :start ls :end le)))
               (and close
                    (eql (%char-at text (1+ close)) #\Space)
                    (prompt-at (+ close 2)))))
        (prompt-at ls))))

(defmethod lexer-tokens ((lexer console-lexer) text)
  (let ((sink (make-sink text))
        (n (length text)))
    (loop with ls = 0
          while (< ls n)
          do (let* ((le (%line-end text ls))
                    (prompt-end (%console-prompt-end text ls le)))
               (if prompt-end
                   (progn (emit sink :meta ls prompt-end)
                          (%lex-shell text prompt-end le sink))
                   (emit sink :plain ls le))
               (emit sink :plain le (min n (1+ le)))
               (setf ls (1+ le))))
    (sink-result sink)))
