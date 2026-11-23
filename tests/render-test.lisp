(in-package #:highlight-protocol/tests)

;;; HTML

(deftest html-spans-escaped
  (ok (equal (concatenate 'string
                          "<span class=\"hl-punctuation\">(</span>"
                          "<span class=\"hl-builtin\">car</span> "
                          "<span class=\"hl-string\">\"&lt;a&amp;b&gt;\"</span>"
                          "<span class=\"hl-punctuation\">)</span>")
             (highlight-html "(car \"<a&b>\")" "lisp"))
      "plain text is bare, everything is escaped")
  (ok (equal "a &lt;b&gt; &amp;&amp; c" (highlight-html "a <b> && c" "text")))
  (ok (equal "" (highlight-html "" "lisp"))))

(deftest html-options
  (ok (equal "<span class=\"x-keyword\">if</span>"
             (highlight-html "if" "python" :prefix "x-")))
  (ok (equal "<pre><code class=\"language-python\"><span class=\"hl-keyword\">if</span></code></pre>"
             (highlight-html "if" "python" :wrap t)))
  (ok (equal "<pre><code>x</code></pre>" (highlight-html "x" nil :wrap t))
      "no language, no class")
  (ok (equal "<pre><code class=\"language-a&quot;b\">x</code></pre>"
             (handler-bind ((unknown-language (lambda (c) (declare (ignore c)) nil)))
               (highlight-html "x" "a\"b" :wrap t)))
      "the language is an escaped attribute"))

(deftest html-unknown-language
  (let ((names '()))
    (handler-bind ((unknown-language (lambda (c) (push (unknown-language-name c) names))))
      (ok (equal "MOVE &lt;A&gt;" (highlight-html "MOVE <A>" "cobol"))))
    (ok (equal '("cobol") names))))

(deftest html-nodes
  (let ((nodes (highlight-nodes "(if x)" "lisp")))
    (ok (= 4 (length nodes)))
    (ok (every (lambda (n) (typep n 'html:html-node)) nodes))
    (let ((span (second nodes)))
      (ok (html:html-element-p span))
      (ok (equal "span" (html:html-element-name span)))
      (ok (equal "hl-keyword" (html:element-attr span "class")))
      (ok (equal "if" (html:element-text span))))
    (ok (html:html-text-p (third nodes)) "plain text is a text node")
    (ok (equal " x" (html:html-text-data (third nodes)))))
  (ok (equal '() (highlight-nodes "" "lisp"))))

;;; Stylesheet

(defparameter *styled-classes* (remove :plain +token-classes+))

(deftest palette-covers-classes
  (dolist (scheme '(:light :dark))
    (let ((palette (highlight-palette scheme)))
      (ok (every (lambda (class)
                   (assoc (format nil "hl-~(~A~)" class) palette :test #'string=))
                 *styled-classes*)
          (format nil "~A palette names every styled class" scheme))
      (ok (assoc "hl-inserted-bg" palette :test #'string=))
      (ok (assoc "hl-deleted-bg" palette :test #'string=))
      (ok (every (lambda (pair) (and (stringp (cdr pair)) (char= #\# (char (cdr pair) 0))))
                 palette))))
  (ok (equal "#cf222e" (cdr (assoc "hl-keyword" (highlight-palette :light) :test #'string=))))
  (ok (equal "#ff7b72" (cdr (assoc "hl-keyword" (highlight-palette :dark) :test #'string=))))
  (ok (signals (highlight-palette :sepia) 'type-error)))

(defun search-all (needles haystack)
  (every (lambda (n) (search n haystack)) needles))

(deftest stylesheet-default
  (let ((css (highlight-css :pretty nil)))
    (ok (search-all '(":root{color-scheme:light dark;--hl-keyword:#cf222e;"
                      "@media (prefers-color-scheme: dark){:root{--hl-keyword:#ff7b72;"
                      ".hl-keyword{color:var(--hl-keyword)}"
                      ".hl-comment{color:var(--hl-comment);font-style:italic}"
                      ".hl-doc{color:var(--hl-doc);font-style:italic}"
                      ".hl-header{color:var(--hl-header);font-weight:bold}"
                      ".hl-inserted{color:var(--hl-inserted);background-color:var(--hl-inserted-bg)}"
                      ".hl-deleted{color:var(--hl-deleted);background-color:var(--hl-deleted-bg)}")
                    css))
    (ok (not (search ".hl-plain" css)) "plain text gets no rule")
    (ok (equal css (css:write-stylesheet (highlight-stylesheet) :pretty nil)))))

(deftest stylesheet-one-rule-per-class
  (let ((rules (css:stylesheet-rules (highlight-stylesheet :theme nil))))
    (ok (= (length *styled-classes*) (length rules)))
    (ok (equal (mapcar (lambda (c) (format nil ".hl-~(~A~)" c)) *styled-classes*)
               (mapcar (lambda (r) (css:selector-text (first (css:rule-selectors r)) :pretty nil))
                       rules)))))

(deftest stylesheet-options
  (let ((css (highlight-css :pretty nil :theme nil :prefix "code-")))
    (ok (search ".code-keyword{color:var(--hl-keyword)}" css))
    (ok (not (search ":root" css)) "no theme: the caller defines --hl-*"))
  (let ((css (highlight-css :pretty nil :toggle-attribute "data-theme")))
    (ok (search ":root:not([data-theme=\"light\"])" css))
    (ok (search ":root[data-theme=\"dark\"]{color-scheme:dark;--hl-keyword:#ff7b72;" css)))
  (let ((css (highlight-css :pretty nil :selector "pre")))
    (ok (search "pre{color-scheme:light dark;" css)))
  (let ((theme (css:make-css-theme :light '(("hl-keyword" . "red")))))
    (ok (search ":root{--hl-keyword:red}" (highlight-css :pretty nil :theme theme))
        "a css-theme of the caller's"))
  (ok (typep (highlight-theme) 'css:css-theme))
  (ok (equal "data-theme" (css:css-theme-toggle-attribute
                           (highlight-theme :toggle-attribute "data-theme")))))

(deftest stylesheet-prunes-with-used-rules
  ;; How cl-blog drops it in: merge, then prune against the built pages.
  (let* ((page (html:make-html-document
                :children (list (html:make-html-element
                                 "html" :children
                                 (list (html:make-html-element
                                        "body" :children
                                        (list (html:make-html-element
                                               "pre" :children
                                               (list (html:make-html-element
                                                      "code"
                                                      :children (highlight-nodes "(if x)" "lisp")))))))))))
         (css (css:write-stylesheet
               (css:used-rules (css:merge-stylesheets (highlight-stylesheet))
                               (list (html:document-root page)))
               :pretty nil)))
    (ok (search ".hl-keyword{" css))
    (ok (search ".hl-punctuation{" css))
    (ok (not (search ".hl-hunk{" css)) "unused classes are pruned")))
