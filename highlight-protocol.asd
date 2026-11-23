(defsystem "highlight-protocol"
  :version "0.1.0"
  :description "Build-time syntax highlighting: lexers to (class . text) tokens, HTML spans and a light / dark stylesheet (stack-highlight)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("html-protocol" "css-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")))
