(defpackage #:highlight-protocol
  (:use #:cl)
  (:nicknames #:stack-highlight)
  (:local-nicknames (#:html #:html-protocol)
                    (#:css #:css-protocol))
  (:export
   ;; conditions (src/conditions.lisp)
   #:unknown-language
   #:unknown-language-name

   ;; tokens (src/tokens.lisp)
   #:+token-classes+
   #:token-lines

   ;; lexers (src/lexer.lisp, src/registry.lisp)
   #:lexer
   #:lexer-name
   #:lexer-aliases
   #:lexer-tokens
   #:register-lexer
   #:find-lexer
   #:list-lexers
   #:tokenize
   #:tokenize-lines
   #:plain-lexer
   #:lisp-lexer
   #:lisp-lexer-dialect
   #:python-lexer
   #:shell-lexer
   #:console-lexer
   #:json-lexer
   #:yaml-lexer
   #:diff-lexer

   ;; HTML (src/html.lisp)
   #:highlight-nodes
   #:highlight-html

   ;; stylesheet (src/style.lisp)
   #:highlight-palette
   #:highlight-theme
   #:highlight-stylesheet
   #:highlight-css))

(in-package #:highlight-protocol)
