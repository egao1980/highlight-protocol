;;;; HTML: tokens as html-protocol nodes or a string.
;;;;
;;;; A :plain token is a text node; any other is <span class="PREFIXclass">
;;;; (PREFIX defaults to "hl-": hl-keyword, hl-string, …) around a text node.
;;;; The string form is html-protocol's own writer, so text and attributes
;;;; are escaped there. With :WRAP the spans go inside
;;;; <pre><code class="language-LANG">, LANG being the language as given
;;;; (its first word; a lexer's name); no language, no class.

(in-package #:highlight-protocol)

(defun %token-node (token prefix)
  (let ((text (make-instance 'html:html-text :data (cdr token))))
    (if (eq (car token) :plain)
        text
        (html:make-html-element
         "span"
         :attributes (list (cons "class" (format nil "~A~(~A~)" prefix (car token))))
         :children (list text)))))

(defun highlight-nodes (text language &key (prefix "hl-"))
  "TEXT highlighted as LANGUAGE (see TOKENIZE): a list of html-protocol nodes,
text nodes for :PLAIN and <span class=\"PREFIXclass\"> for the rest, to put
inside a <code> element."
  (check-type prefix string)
  (mapcar (lambda (token) (%token-node token prefix))
          (tokenize text language)))

(defun %language-label (language)
  (typecase language
    (lexer (lexer-name language))
    (null nil)
    (t (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (if (symbolp language)
                                  (string-downcase (symbol-name language))
                                  language)))
              (end (position-if #'%whitespace-p s)))
         (and (plusp (length s)) (subseq s 0 end))))))

(defun highlight-html (text language &key (prefix "hl-") wrap)
  "TEXT highlighted as LANGUAGE (see TOKENIZE), as an HTML string of spans
with escaped text. WRAP true puts it in <pre><code class=\"language-LANG\">."
  (let* ((nodes (highlight-nodes text language :prefix prefix))
         (label (and wrap (%language-label language)))
         (html:*html-backend* nil))
    (html:serialize
     (html:make-html-document
      :children (if wrap
                    (list (html:make-html-element
                           "pre"
                           :children (list (html:make-html-element
                                            "code"
                                            :attributes (and label
                                                             (list (cons "class"
                                                                         (concatenate 'string "language-" label))))
                                            :children nodes))))
                    nodes)))))
