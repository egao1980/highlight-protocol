# highlight-protocol

Build-time syntax highlighting for [cl-stack](https://github.com/egao1980/cl-stack) (package `highlight-protocol`, nickname `stack-highlight`). Lexers turn source text into `(class . text)` tokens whose texts concatenate to exactly the input. An HTML renderer emits `<span class="hl-…">` with escaped text, as [html-protocol](https://github.com/egao1980/html-protocol) nodes or a string. A default stylesheet ([css-protocol](https://github.com/egao1980/css-protocol), light and dark through `prefers-color-scheme`) colours the classes.

Brief: [cl-stack/docs/capabilities/highlight.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/highlight.md).

```lisp
(stack-highlight:highlight-html "(defun hi () \"Hi.\" (print 1))" "lisp")
;; => "<span class=\"hl-punctuation\">(</span><span class=\"hl-keyword\">defun</span> <span class=\"hl-function\">hi</span> <span class=\"hl-punctuation\">()</span> <span class=\"hl-doc\">\"Hi.\"</span> <span class=\"hl-punctuation\">(</span><span class=\"hl-builtin\">print</span> <span class=\"hl-number\">1</span><span class=\"hl-punctuation\">))</span>"

(stack-highlight:tokenize "x = None  # c" "python")
;; => ((:plain . "x ") (:operator . "=") (:plain . " ") (:constant . "None") (:plain . "  ") (:comment . "# c"))

(stack-highlight:highlight-css :pretty nil)   ; or HIGHLIGHT-STYLESHEET for a css-protocol stylesheet
;; => ":root{color-scheme:light dark;--hl-keyword:#cf222e;…}@media (prefers-color-scheme: dark){…}.hl-keyword{color:var(--hl-keyword)}…"
```

## API

```lisp
;;; tokens
(tokenize text language) → ((class . text) …)       ; language: lexer | name | alias | NIL
(tokenize-lines lines language) → lines of tokens   ; tui-markdown's highlight hook shape
(token-lines tokens) → lines of tokens              ; split at newlines
+token-classes+

;;; lexers
(find-lexer language) → lexer | NIL                 ; case-insensitive, first word
(register-lexer lexer) → lexer
(list-lexers) → lexers
(lexer-tokens lexer text)                           ; the generic function a lexer implements
lexer (lexer-name, lexer-aliases), plain-lexer, lisp-lexer (lisp-lexer-dialect),
python-lexer, shell-lexer, console-lexer, json-lexer, yaml-lexer, diff-lexer

;;; HTML
(highlight-nodes text language &key (prefix "hl-")) → html-protocol nodes
(highlight-html text language &key (prefix "hl-") wrap) → string

;;; CSS
(highlight-stylesheet &key (prefix "hl-") (theme t) (selector ":root") toggle-attribute) → css stylesheet
(highlight-css &key (pretty t) prefix theme selector toggle-attribute) → string
(highlight-theme &key (selector ":root") toggle-attribute) → css:css-theme
(highlight-palette &optional (scheme :light)) → (("hl-keyword" . "#cf222e") …)

;;; conditions
unknown-language (condition, not an error; unknown-language-name; restart USE-VALUE)
```

An unknown language signals `unknown-language` and is highlighted as plain text. A handler can supply a lexer or another name with `use-value`.

## Classes

`:plain` (no span) `:keyword` `:builtin` `:function` `:type` `:symbol` `:variable` `:constant` `:string` `:doc` `:number` `:comment` `:operator` `:punctuation` `:meta` `:property`, and for diffs `:inserted` `:deleted` `:header` `:hunk`. What each lexer marks with them is in `src/tokens.lisp` and the header of each lexer's file.

## Languages

| Lexer | Names |
|-------|-------|
| `common-lisp` | lisp, cl, commonlisp, asd, lsp |
| `emacs-lisp` | elisp, el |
| `scheme` | scm |
| `python` | py, python3 |
| `shell` | sh, bash, zsh, ksh |
| `console` | shell-session, sh-session |
| `json` | jsonc |
| `yaml` | yml |
| `diff` | patch, udiff |
| `text` | plain, txt, plaintext |

Every lexer accepts any string: malformed or truncated input never signals (an unterminated string runs to the end), and lexing is linear in the input.

## Adding a lexer

```lisp
(defclass ini-lexer (stack-highlight:lexer) ())

(defmethod stack-highlight:lexer-tokens ((lexer ini-lexer) text)
  ;; any list of (class . text) whose texts concatenate to TEXT
  (list (cons :plain text)))

(stack-highlight:register-lexer (make-instance 'ini-lexer :name "ini" :aliases '("cfg")))
```

## License

MIT, see [LICENSE](LICENSE).
