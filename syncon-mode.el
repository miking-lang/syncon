;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep these lists sorted
(setq syncon-keywords
     '(
       "token"
       "syncon"
       "prefix"
       "postfix"
       "infix"
       "#assoc"
       "comment"
       "left"
       "right"
       "precedence"
       "except"
       "type"
       "builtin"
       "forbid"
       "rec"
      ))

(setq syncon-danger-words
     '(
       "BODY"
       "EMBED"
       "END"
       "unsafe"
       ))
(setq syncon-constants
     '(
       ))

(setq syncon-primitives
     '(
       ))

(setq syncon-operators
     '(
       "|"
       "+"
       "*"
       "?"
       ))

(setq syncon-keywords-regexp (regexp-opt syncon-keywords 'symbols))
(setq syncon-danger-regexp (regexp-opt syncon-danger-words 'symbols))
(setq syncon-constants-regexp (regexp-opt syncon-constants 'symbols))
(setq syncon-primitives-regexp (regexp-opt syncon-primitives 'symbols))
(setq syncon-operators-regexp (regexp-opt syncon-operators 'symbols))

(setq syncon-types-regexp "\\<[A-Z][a-zA-Z]*\\>")
;; (setq syncon-function-regexp “\\<\\(def\\|stream\\|fun\\|require\\)\\> \\([^(]*\\)([^)]*)\\W*:\\W*.*“)
;; (setq syncon-variable-regexp “\\<\\([A-Za-z0-9_]*\\)\\>\\W*:“)
(setq syncon-comment-regexp "//.?*")
;; (setq syncon-char-regexp “‘\\(\\\\.\\|.\\)‘“)

(setq syncon-font-lock-keywords
     `(
       (,syncon-comment-regexp    . font-lock-comment-face)
       ;; (,syncon-char-regexp       . font-lock-string-face)
       (,syncon-keywords-regexp   . font-lock-keyword-face)
       (,syncon-danger-regexp     . font-lock-warning-face)
       (,syncon-constants-regexp  . font-lock-constant-face)
       (,syncon-primitives-regexp . font-lock-type-face)
       (,syncon-operators-regexp  . font-lock-builtin-face)
       (,syncon-types-regexp      . font-lock-type-face)
       ;; (,syncon-function-regexp   2 font-lock-function-name-face)
       ;; (,syncon-variable-regexp   1 font-lock-variable-name-face)
       )
     )

(defvar syncon-mode-syntax-table nil "Syntax table for `syncon-mode'.")

(setq syncon-mode-syntax-table
     (let ( (synTable (make-syntax-table)))
       ;; Block comment “{- ... -}”
       ;; Inline comment “-- ...”
       (modify-syntax-entry ?\{ "(}1nb" synTable)
       (modify-syntax-entry ?\} "){4nb" synTable)
       (modify-syntax-entry ?/ "_ 123" synTable)
       (modify-syntax-entry ?\n ">" synTable)
       synTable))

;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode syncon-mode prog-mode
 (setq font-lock-defaults '(syncon-font-lock-keywords))
 (setq mode-name "syncon")
 )

;; Open “*.enc” in syncon-mode
(add-to-list 'auto-mode-alist '("\\.syncon\\'" . syncon-mode))

(provide 'syncon-mode)
