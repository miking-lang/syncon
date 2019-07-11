;;; syncon-mode.el

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep this list sorted
(setq syncon-keywords
     '(
       "builtin"
       "comment"
       "except"
       "forbid"
       "grouping"
       "infix"
       "left"
       "postfix"
       "precedence"
       "prefix"
       "rec"
       "right"
       "syncon"
       "token"
       "type"
      ))

(setq syncon-operators
     '(
       "|"
       "+"
       "*"
       "?"
       ))

(setq syncon-keywords-regexp (regexp-opt syncon-keywords 'symbols))
(setq syncon-operators-regexp (regexp-opt syncon-operators 'symbols))

(setq syncon-types-regexp "\\<[[:upper:]][[:word:]]*\\>")

(setq syncon-font-lock-keywords
     `(
       (,syncon-keywords-regexp   . font-lock-keyword-face)
       (,syncon-operators-regexp  . font-lock-builtin-face)
       (,syncon-types-regexp      . font-lock-type-face)
       )
     )

(defvar syncon-mode-syntax-table nil "Syntax table for `syncon-mode'.")

(setq syncon-mode-syntax-table
     (let ( (synTable (make-syntax-table)))
       ;; Inline comment “// ...”
       (modify-syntax-entry ?/ ". 12a" synTable)
       (modify-syntax-entry ?\n ">" synTable)
       synTable))

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;

(add-hook 'syncon-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "syncon-parser " (buffer-name)))))

(setq syncon-error-regexp
      '(syncon " *\\(.*\\)#\\([0-9]*\\):\\([0-9]*\\):" 1 2 3))
(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist syncon-error-regexp)
            (add-to-list 'compilation-error-regexp-alist 'syncon)))


;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode syncon-mode prog-mode
 (setq font-lock-defaults '(syncon-font-lock-keywords))
 (setq mode-name "syncon")
 )

;; Open “*.syncon” in syncon-mode
(add-to-list 'auto-mode-alist '("\\.syncon\\'" . syncon-mode))

(provide 'syncon-mode)
;;; syncon-mode.el ends here
