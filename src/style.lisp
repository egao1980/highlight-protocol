;;;; The default stylesheet: css-protocol rules for the hl- classes over
;;;; --hl-* custom properties, with a light and a dark palette.
;;;;
;;;; HIGHLIGHT-STYLESHEET → one css-protocol STYLESHEET:
;;;;   1. unless THEME is NIL, the custom properties: css-protocol's
;;;;      CSS-THEME-STYLESHEET of THEME (T: HIGHLIGHT-THEME, light under
;;;;      SELECTOR, dark under @media (prefers-color-scheme: dark), and with
;;;;      TOGGLE-ATTRIBUTE the [attr="dark"] / [attr="light"] overrides);
;;;;   2. one rule per class but :plain, in +TOKEN-CLASSES+ order:
;;;;        .PREFIXclass { color: var(--hl-class); … }
;;;;      :comment and :doc are italic, :header bold, :inserted / :deleted
;;;;      have a background (--hl-inserted-bg / --hl-deleted-bg).
;;;; The custom property names do not follow PREFIX: a page can restyle
;;;; every highlighter through --hl-*. Merge the result with the page's own
;;;; stylesheet (css:merge-stylesheets) and prune it with css:used-rules.
;;;;
;;;; The palettes follow GitHub's Primer syntax colours; every foreground
;;;; has a 4.5:1 contrast or better on #f3f3f3 (light) and #262a31 (dark).

(in-package #:highlight-protocol)

(defparameter *palettes*
  '((:light
     ("hl-keyword" . "#cf222e") ("hl-builtin" . "#0550ae") ("hl-function" . "#6639ba")
     ("hl-type" . "#953800") ("hl-symbol" . "#136a70") ("hl-variable" . "#7d4e00")
     ("hl-constant" . "#116329") ("hl-string" . "#0a3069") ("hl-doc" . "#0a3069")
     ("hl-number" . "#0550ae") ("hl-comment" . "#57606a") ("hl-operator" . "#cf222e")
     ("hl-punctuation" . "#57606a") ("hl-meta" . "#8250df") ("hl-property" . "#0550ae")
     ("hl-inserted" . "#116329") ("hl-deleted" . "#82071e") ("hl-header" . "#0550ae")
     ("hl-hunk" . "#8250df") ("hl-inserted-bg" . "#dafbe1") ("hl-deleted-bg" . "#ffebe9"))
    (:dark
     ("hl-keyword" . "#ff7b72") ("hl-builtin" . "#79c0ff") ("hl-function" . "#d2a8ff")
     ("hl-type" . "#ffa657") ("hl-symbol" . "#56d4dd") ("hl-variable" . "#e3b341")
     ("hl-constant" . "#7ee787") ("hl-string" . "#a5d6ff") ("hl-doc" . "#a5d6ff")
     ("hl-number" . "#79c0ff") ("hl-comment" . "#8b949e") ("hl-operator" . "#ff7b72")
     ("hl-punctuation" . "#8b949e") ("hl-meta" . "#bc8cff") ("hl-property" . "#79c0ff")
     ("hl-inserted" . "#aff5b4") ("hl-deleted" . "#ffdcd7") ("hl-header" . "#79c0ff")
     ("hl-hunk" . "#d2a8ff") ("hl-inserted-bg" . "#033a16") ("hl-deleted-bg" . "#67060c")))
  "Custom property name (without --) → colour, per scheme.")

(defparameter *class-declarations*
  '((:comment ("font-style" . "italic"))
    (:doc ("font-style" . "italic"))
    (:header ("font-weight" . "bold"))
    (:inserted ("background-color" . "var(--hl-inserted-bg)"))
    (:deleted ("background-color" . "var(--hl-deleted-bg)")))
  "Declarations beyond the colour, per class.")

(defun highlight-palette (&optional (scheme :light))
  "SCHEME's colours (:LIGHT or :DARK) as a fresh alist of custom property
name (\"hl-keyword\", without --) → colour, one per class but :plain plus
hl-inserted-bg and hl-deleted-bg: ready for css:make-css-theme."
  (check-type scheme (member :light :dark))
  (copy-alist (cdr (assoc scheme *palettes*))))

(defun highlight-theme (&key (selector ":root") toggle-attribute)
  "The default palettes as a css-protocol CSS-THEME (light, and dark under
prefers-color-scheme; TOGGLE-ATTRIBUTE as in css:make-css-theme)."
  (css:make-css-theme :light (highlight-palette :light)
                      :dark (highlight-palette :dark)
                      :selector selector
                      :toggle-attribute toggle-attribute))

(defun %class-rules (prefix)
  (loop for class in (remove :plain +token-classes+)
        collect (css:make-rule
                 (format nil ".~A~(~A~)" prefix class)
                 (cons (css:make-declaration "color" (format nil "var(--hl-~(~A~))" class))
                       (loop for (property . value) in (cdr (assoc class *class-declarations*))
                             collect (css:make-declaration property value))))))

(defun highlight-stylesheet (&key (prefix "hl-") (theme t) (selector ":root") toggle-attribute)
  "The highlighting stylesheet (css-protocol): the --hl-* custom properties of
THEME (T: HIGHLIGHT-THEME with SELECTOR and TOGGLE-ATTRIBUTE; a css:css-theme;
NIL: none, the page defines --hl-*), then one .PREFIXclass rule per class."
  (check-type prefix string)
  (let ((theme (cond ((eq theme t)
                      (highlight-theme :selector selector :toggle-attribute toggle-attribute))
                     ((null theme) nil)
                     ((typep theme 'css:css-theme) theme)
                     (t (error 'type-error :datum theme
                                           :expected-type '(or boolean css:css-theme)))))
        (rules (css:make-stylesheet (%class-rules prefix))))
    (if theme
        (css:merge-stylesheets (css:css-theme-stylesheet theme) rules)
        rules)))

(defun highlight-css (&key (pretty t) (prefix "hl-") (theme t) (selector ":root") toggle-attribute)
  "HIGHLIGHT-STYLESHEET written as CSS text (PRETTY as in css:write-stylesheet)."
  (css:write-stylesheet (highlight-stylesheet :prefix prefix :theme theme :selector selector
                                              :toggle-attribute toggle-attribute)
                        :pretty pretty))
