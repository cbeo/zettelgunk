(setq zettel-highlights
      '(("#[^ \n]+" . font-lock-doc-face)
        ("\|[^\n\|]+\|" . font-lock-function-name-face)))

(defun filename-to-zettel-name (fname)
  (concat "|"
          (first (last (butlast (split-string fname "[/.]"))))
          "|"))

(defun zettel-link-names ()
  (mapcar #'filename-to-zettel-name deft-all-files))


(defun zettel-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (zettel-link-names) . nil)))


(defun valid-zettel-note-name-p (string)
  (let ((trimmed (string-trim string)))
    (and (s-starts-with-p "|" trimmed)
         (s-ends-with-p "|" trimmed))))

(defun zettel-link-to-file-path (link)
  (let ((name (string-trim link "[\s\|]+" "[\s\|]+")))
    (concat deft-directory name ".zettel")))

(defun find-zettel-file (thing)
  (when (valid-zettel-note-name-p thing)
    (find-file (zettel-link-to-file-path thing))))

(defun zettel-file-contains-tag-p (tag file)
  (let ((file-string  
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string))))
    (search tag file-string)))

(defvar zettel-tags-buffer-name "*zettel-tags*")

(defun zettel-show-notes-by-tag (tag)
  (with-output-to-temp-buffer zettel-tags-buffer-name
    (with-current-buffer zettel-tags-buffer-name
      (zettel-mode)
      
      (dolist (name (zettel-link-names))
        (when (zettel-file-contains-tag-p tag (zettel-link-to-file-path name))
          (princ name)
          (terpri)))
      
      (read-only-mode)
      (local-set-key (kbd "q")  'zettel-dismiss-tags-buffer)
      (switch-to-buffer zettel-tags-buffer-name))))



(defun zettel-dismiss-tags-buffer ()
  (interactive)
  (when (get-buffer zettel-tags-buffer-name)
    (kill-buffer zettel-tags-buffer-name)))


(defvar zettel-jump-back-list '())

(defun zettel-jump-to-note ()
  (interactive)
  (when buffer-file-name 
    (push buffer-file-name zettel-jump-back-list))
  (let ((thing (thing-at-point 'symbol t)))
    (find-zettel-file thing)
    (zettel-dismiss-tags-buffer)))

(defun zettel-tag-p (str)
  (s-starts-with-p "#" str))

(defun zettel-browse-tag-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'word t)))
    (print thing)
    (when (zettel-tag-p thing)
      (zettel-show-notes-by-tag thing))))

(defun zettel-jump-back ()
  (interactive)
  (let ((back (pop zettel-jump-back-list)))
    (if back (find-file back))))


(define-derived-mode zettel-mode fundamental-mode "zettel"
  "mode for navigating my zettelkasten system"
  (setq font-lock-defaults '(zettel-highlights))
  (add-hook 'completion-at-point-functions 'zettel-completion-at-point nil 'local))

;; so that the start of tags count as words
(add-hook 'zettel-mode-hook (lambda () (modify-syntax-entry ?# "w")))


(add-to-list 'auto-mode-alist '("\\.zettel\\'" . zettel-mode))

