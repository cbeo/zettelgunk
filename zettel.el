
;; syntax highlighting

(setq zettel-highlights
      '(("#[^ \n]+" . font-lock-doc-face)
        ("\|[^\n\|]+\|" . font-lock-function-name-face)))

(define-derived-mode zettel-mode fundamental-mode "zettel"
  "mode for navigating my zettelkasten system"
  (setq font-lock-defaults '(zettel-highlights)))
