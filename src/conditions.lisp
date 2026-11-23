;;;; Conditions and restarts.
;;;;
;;;; Highlighting never fails on its input: every lexer accepts any string.
;;;; Bad arguments (a non-string text, a language that is not a designator)
;;;; signal TYPE-ERROR. The one condition of our own is UNKNOWN-LANGUAGE.
;;;;
;;;; UNKNOWN-LANGUAGE is signalled with SIGNAL (it is not an error) by
;;;; TOKENIZE (and so by HIGHLIGHT-NODES / HIGHLIGHT-HTML / TOKENIZE-LINES)
;;;; when a language name has no registered lexer. Restart:
;;;;
;;;;   USE-VALUE (language) - lex with LANGUAGE instead: a lexer, or a name
;;;;                          (looked up again; still unknown -> plain).
;;;;
;;;; Unhandled, or when a handler declines, the text is lexed as plain text.

(in-package #:highlight-protocol)

(define-condition unknown-language (condition)
  ((name :initarg :name :reader unknown-language-name))
  (:report (lambda (c s)
             (format s "No lexer for language ~S; highlighting as plain text"
                     (unknown-language-name c))))
  (:documentation "Signalled (not an error) when no lexer is registered for NAME,
the language name as given. Restart USE-VALUE takes a lexer or a name."))
