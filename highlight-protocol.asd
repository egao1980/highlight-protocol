(defsystem "highlight-protocol"
  :version "0.1.0"
  :description "Build-time syntax highlighting: lexers to (class . text) tokens, HTML spans and a light / dark stylesheet (stack-highlight)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "html-protocol" "0.1.1")
               (:version "css-protocol" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "tokens")
               (:file "lexer")
               (:file "lisp")
               (:file "python")
               (:file "shell")
               (:file "json")
               (:file "yaml")
               (:file "diff")
               (:file "registry")
               (:file "html")
               (:file "style"))
  :in-order-to ((test-op (test-op "highlight-protocol/tests"))))

(defsystem "highlight-protocol/tests"
  :depends-on ("highlight-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "lisp-test")
               (:file "python-test")
               (:file "shell-test")
               (:file "data-test")
               (:file "robust-test")
               (:file "render-test"))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "tests failed for ~A" (component-name c)))))
