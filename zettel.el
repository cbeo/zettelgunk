
(defvar zettel-directory "~/zettel/")
(defvar zettel-tags-buffer-name "*zettel-tags*")
(defvar zettel-jump-back-list '())
(defvar zettel-start-file "zettel.zettel")

(defvar zettel-link-regex)
(setq zettel-link-regex "\|[a-zA-Z\-0-9\_]+\|")
(defvar zettel-tag-regex)
(setq zettel-tag-regex "#[a-zA-Z0-9]+")

(setq zettel-highlights
      '(("#[^ \n]+" . font-lock-doc-face)
        ("\|[^\n\|]+\|" . font-lock-function-name-face)))

(defun filename-to-zettel-name (fname)
  (concat "|"
          (first (last (butlast (split-string fname "[/.]"))))
          "|"))

(defun zettel-all-files ()
  (directory-files zettel-directory t ".zettel$"))

(defun zettel-link-names ()
  (mapcar #'filename-to-zettel-name (zettel-all-files)))


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
    (concat zettel-directory name ".zettel")))

(defun find-zettel-file (thing)
  (when (valid-zettel-note-name-p thing)
    (find-file (zettel-link-to-file-path thing))))

(defun zettel-file-contains-tag-p (tag file)
  (let ((file-string  
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string))))
    (search tag file-string)))



(defun zettel-show-notes-by-tag (tag)
  (with-output-to-temp-buffer zettel-tags-buffer-name
    (with-current-buffer zettel-tags-buffer-name
      (zettel-mode)
      
      (dolist (name (zettel-link-names))
        (when (zettel-file-contains-tag-p tag (zettel-link-to-file-path name))
          (princ name)
          (terpri)))
      
      (read-only-mode)
      (use-local-map (copy-keymap (make-sparse-keymap)))
      (local-set-key (kbd "q")  'zettel-dismiss-tags-buffer)
      (local-set-key (kbd "<return>") 'zettel-jump-to-note)
      (switch-to-buffer zettel-tags-buffer-name))))


(defun zettel-dismiss-tags-buffer ()
  (interactive)
  (when (get-buffer zettel-tags-buffer-name)
    (kill-buffer zettel-tags-buffer-name)))



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

(defun zettel-find-next-link-in-buffer ()
  (let ((current-point (point))
        (next-link (search-forward-regexp zettel-link-regex nil t)))
    (goto-char current-point)
    next-link))

(defun zettel-find-prev-link-in-buffer ()
  (let ((current-point (point))
        (next-link (search-backward-regexp zettel-link-regex nil t)))
    (goto-char current-point)
    next-link))

(defun zettel-find-next-tag-in-buffer ()
  (let ((current-point (point))
        (next-tag (search-forward-regexp zettel-tag-regex nil t)))
    (goto-char current-point)
    next-tag))

(defun zettel-find-prev-tag-in-buffer ()
  (let ((current-point (point))
        (next-tag (search-backward-regexp zettel-tag-regex nil t)))
    (goto-char current-point)
    next-tag))


(defun zettel-next-thing-in-buffer ()
  (interactive)
  (let ((next-tag (zettel-find-next-tag-in-buffer))
        (next-link (zettel-find-next-link-in-buffer)))
    (cond
     ((and next-tag next-link)
      (goto-char (min next-tag next-link)))
     (next-tag (goto-char next-tag))
     (next-link (goto-char next-link)))))

(defun zettel-prev-thing-in-buffer ()
  (interactive)
  (let ((prev-tag (zettel-find-prev-tag-in-buffer))
        (prev-link (zettel-find-prev-link-in-buffer)))
    (cond
     ((and prev-tag prev-link)
      (goto-char (max prev-tag prev-link)))
     (prev-tag (goto-char prev-tag))
     (prev-link (goto-char prev-link)))))


(defun zettel ()
  (interactive)
  (let ((val (pop zettel-jump-back-list)))
    (if val (find-file val)
      (find-file (concat zettel-directory zettel-start-file)))))

(define-derived-mode zettel-mode fundamental-mode "zettel"
  "mode for navigating my zettelkasten system"
  (setq font-lock-defaults '(zettel-highlights))
  (add-hook 'completion-at-point-functions 'zettel-completion-at-point nil 'local))

(defun zettel-mode-config-hook ()
  ;; so that tags can be slurped up by thing-at-point
  (modify-syntax-entry ?# "w")
  (modify-syntax-entry ?- "w")
  (local-set-key (kbd "M-n") 'zettel-next-thing-in-buffer)
  (local-set-key (kbd "M-p") 'zettel-prev-thing-in-buffer))


(add-hook 'zettel-mode-hook 'zettel-mode-config-hook)

(add-to-list 'auto-mode-alist '("\\.zettel\\'" . zettel-mode))

