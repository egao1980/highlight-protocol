;;;; The built-in lexers and their aliases.

(in-package #:highlight-protocol)

(register-lexer *plain-lexer*)
(register-lexer (make-instance 'lisp-lexer :name "common-lisp" :dialect :common-lisp
                                           :aliases '("lisp" "cl" "commonlisp" "asd" "lsp")))
(register-lexer (make-instance 'lisp-lexer :name "emacs-lisp" :dialect :emacs-lisp
                                           :aliases '("elisp" "el")))
(register-lexer (make-instance 'lisp-lexer :name "scheme" :dialect :scheme
                                           :aliases '("scm")))
(register-lexer (make-instance 'python-lexer :name "python" :aliases '("py" "python3")))
(register-lexer (make-instance 'shell-lexer :name "shell" :aliases '("sh" "bash" "zsh" "ksh")))
(register-lexer (make-instance 'console-lexer :name "console"
                                              :aliases '("shell-session" "sh-session")))
(register-lexer (make-instance 'json-lexer :name "json" :aliases '("jsonc")))
(register-lexer (make-instance 'yaml-lexer :name "yaml" :aliases '("yml")))
(register-lexer (make-instance 'diff-lexer :name "diff" :aliases '("patch" "udiff")))
