
;; syntax highlighting

(setq zettel-highlights
      '(("#[^ \n]+" . font-lock-doc-face)
        ("\|[^\n\|]+\|" . font-lock-function-name-face)))


(defun filename-to-zettle-name (fname)
  (concat "|"
          (first (last (butlast (split-string fname "[/.]"))))
          "|"))

(defun zettle-file-names ()
  (mapcar #'filename-to-zettle-name deft-all-files))

(defun zettle-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (zettle-file-names) . nil)))


(defun valid-zettle-note-name-p (string)
  (let ((trimmed (string-trim string)))
    (and (s-starts-with-p "|" trimmed)
         (s-ends-with-p "|" trimmed))))

(defun find-zettel-file (thing)
  (if (valid-zettle-note-name-p thing)
      (let* ((name (string-trim thing "[\s\|]+" "[\s\|]+"))
             (path (concat deft-directory name ".zettel")))
        (find-file path))))

(defvar zettel-jump-back-list '())

(defun zettel-jump-to-note ()
  (interactive)
  (push buffer-file-name zettel-jump-back-list)
  (let ((thing (thing-at-point 'symbol t)))
    (find-zettel-file thing)))

(defun zettel-jump-back ()
  (interactive)
  (let ((back (pop zettel-jump-back-list)))
    (if back (find-file back))))



(define-derived-mode zettel-mode fundamental-mode "zettel"
  "mode for navigating my zettelkasten system"
  (setq font-lock-defaults '(zettel-highlights))
  (add-hook 'completion-at-point-functions 'zettle-completion-at-point nil 'local))


(add-to-list 'auto-mode-alist '("\\.zettel\\'" . zettel-mode))

