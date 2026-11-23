(in-package #:highlight-protocol/tests)

;;; JSON

(deftest json-document
  (ok (equal '((:punctuation . "{") (:property . "\"name\"") (:punctuation . ":") (:plain . " ")
               (:string . "\"x\"") (:punctuation . ",") (:plain . " ") (:property . "\"n\"")
               (:punctuation . ":") (:plain . " ") (:number . "-1.5e3") (:punctuation . ",")
               (:plain . " ") (:property . "\"ok\"") (:punctuation . ":") (:plain . " ")
               (:punctuation . "[") (:constant . "true") (:punctuation . ",") (:plain . " ")
               (:constant . "null") (:punctuation . "]}") (:plain . " ") (:comment . "// c"))
             (toks "json" "{\"name\": \"x\", \"n\": -1.5e3, \"ok\": [true, null]} // c"))))

(deftest json-details
  (ok (eq :property (class-of-piece (toks "json" "{\"a\"" "  : 1}") "\"a\""))
      "a key may have whitespace before its colon")
  (ok (equal '((:string . "\"a\\\"b\"")) (toks "json" "\"a\\\"b\"")))
  (ok (equal '((:comment . "/* c */") (:plain . " ") (:number . "0")) (toks "json" "/* c */ 0")))
  (ok (equal '((:punctuation . "{") (:plain . "a") (:punctuation . ":") (:plain . " b")
               (:punctuation . "}"))
             (toks "json" "{a: b}"))
      "invalid JSON is still lexed"))

(deftest json-truncated
  (ok (equal '((:punctuation . "{") (:property . "\"a\"") (:punctuation . ":") (:plain . " ")
               (:string . "\"open") (:plain . "
") (:punctuation . "}"))
             (toks "json" "{\"a\": \"open" "}"))
      "a string ends at the end of its line")
  (ok (equal '((:comment . "/* open")) (toks "json" "/* open")))
  (ok (equal '((:plain . "-")) (toks "json" "-"))))

;;; YAML

(deftest yaml-document
  (ok (equal '((:meta . "%YAML 1.2") (:plain . "
") (:meta . "---") (:plain . "
")
               (:comment . "# config") (:plain . "
")
               (:property . "name") (:punctuation . ":") (:plain . " ") (:string . "cl-blog")
               (:plain . "  ") (:comment . "# trailing") (:plain . "
")
               (:property . "version") (:punctuation . ":") (:plain . " ") (:number . "1.0")
               (:plain . "
")
               (:property . "enabled") (:punctuation . ":") (:plain . " ") (:constant . "true")
               (:plain . "
")
               (:property . "nothing") (:punctuation . ":") (:plain . " ") (:constant . "~")
               (:plain . "
")
               (:property . "anchor") (:punctuation . ":") (:plain . " ") (:meta . "&base")
               (:plain . "
  ") (:punctuation . "-") (:plain . " ") (:string . "item one") (:plain . "
  ")
               (:punctuation . "-") (:plain . " ") (:string . "\"quoted\"") (:plain . "
  ")
               (:punctuation . "-") (:plain . " ") (:string . "'it''s'") (:plain . "
")
               (:property . "ref") (:punctuation . ":") (:plain . " ") (:meta . "*base")
               (:plain . "
")
               (:property . "tag") (:punctuation . ":") (:plain . " ") (:meta . "!!str")
               (:plain . " ") (:number . "42") (:plain . "
")
               (:property . "script") (:punctuation . ":") (:plain . " ") (:punctuation . "|")
               (:plain . "
") (:string . "  echo hi
  echo bye
")
               (:property . "after") (:punctuation . ":") (:plain . " ") (:string . "x")
               (:plain . "
") (:meta . "..."))
             (toks "yaml"
                   "%YAML 1.2"
                   "---"
                   "# config"
                   "name: cl-blog  # trailing"
                   "version: 1.0"
                   "enabled: true"
                   "nothing: ~"
                   "anchor: &base"
                   "  - item one"
                   "  - \"quoted\""
                   "  - 'it''s'"
                   "ref: *base"
                   "tag: !!str 42"
                   "script: |"
                   "  echo hi"
                   "  echo bye"
                   "after: x"
                   "..."))))

(deftest yaml-flow-and-sequences
  (ok (equal '((:punctuation . "{") (:property . "a") (:punctuation . ":") (:plain . " ")
               (:number . "1") (:punctuation . ",") (:plain . " ") (:property . "b")
               (:punctuation . ":") (:plain . " ") (:punctuation . "[") (:string . "x")
               (:punctuation . ",") (:plain . " ") (:string . "\"y\"") (:punctuation . "]}"))
             (toks "yaml" "{a: 1, b: [x, \"y\"]}")))
  (ok (equal '((:punctuation . "-") (:plain . " ") (:property . "name") (:punctuation . ":")
               (:plain . " ") (:string . "a") (:plain . "
  ") (:property . "run")
               (:punctuation . ":") (:plain . " ") (:string . "cl-repo test") (:plain . " ")
               (:comment . "# a comment") (:plain . "
"))
             (toks "yaml" "- name: a" "  run: cl-repo test # a comment" ""))
      "\" #\" starts a comment")
  (ok (equal '((:property . "url") (:punctuation . ":") (:plain . " ")
               (:string . "http://x.org/a#frag"))
             (toks "yaml" "url: http://x.org/a#frag"))
      "a colon or # inside a scalar is text")
  (ok (eq :property (class-of-piece (toks "yaml" "\"quoted key\": 1") "\"quoted key\"")))
  (ok (equal '((:property . "k") (:punctuation . ":") (:plain . " ") (:punctuation . ">-")
               (:plain . " ") (:comment . "# folded") (:plain . "
") (:string . "  text
")
               (:property . "next") (:punctuation . ":"))
             (toks "yaml" "k: >- # folded" "  text" "next:"))))

(deftest-parametrize yaml-scalars
    ((text class)
     ("yes" :constant) ("False" :constant) ("NULL" :constant) ("-12" :number)
     ("0x1F" :number) ("3.5e-2" :number) (".inf" :number) ("1.2.3" :string)
     ("hello world" :string))
  (ok (equal (list (cons :property "k") (cons :punctuation ":") (cons :plain " ")
                   (cons class text))
             (tokenize (format nil "k: ~A" text) "yaml"))))

(deftest yaml-truncated
  (ok (equal '((:property . "key") (:punctuation . ":") (:plain . " ") (:string . "\"open
more"))
             (toks "yaml" "key: \"open" "more")))
  (ok (equal '((:property . "k") (:punctuation . ":") (:plain . " ") (:punctuation . "|"))
             (toks "yaml" "k: |")))
  (ok (equal '((:punctuation . "[[") (:string . "a")) (toks "yaml" "[[a"))))

;;; diff

(deftest diff-git
  (ok (equal '((:header . "diff --git a/x b/x") (:plain . "
") (:header . "index 1..2 100644")
               (:plain . "
") (:header . "--- a/x") (:plain . "
") (:header . "+++ b/x")
               (:plain . "
") (:hunk . "@@ -1,2 +1,2 @@ fn") (:plain . "
 a
")
               (:deleted . "--- b") (:plain . "
") (:inserted . "+++ c") (:plain . "
")
               (:comment . "\\ No newline at end of file") (:plain . "
"))
             (toks "diff"
                   "diff --git a/x b/x"
                   "index 1..2 100644"
                   "--- a/x"
                   "+++ b/x"
                   "@@ -1,2 +1,2 @@ fn"
                   " a"
                   "--- b"
                   "+++ c"
                   "\\ No newline at end of file"
                   ""))
      "inside a hunk, --- and +++ lines are changes"))

(deftest diff-loose
  (ok (equal '((:deleted . "-old") (:plain . "
") (:inserted . "+new") (:plain . "
 same"))
             (toks "patch" "-old" "+new" " same"))
      "a snippet without headers")
  (ok (equal '((:hunk . "@@ @@") (:plain . "
") (:deleted . "-a") (:plain . "
")
               (:inserted . "+b"))
             (toks "diff" "@@ @@" "-a" "+b"))
      "a hunk header without counts")
  (ok (equal '((:plain . "commit message
")) (toks "diff" "commit message" ""))
      "other lines are plain"))
