
(defvar zettel-directory "~/zettel/")
(defvar zettel-tags-buffer-name "*zettel-tags*")
(defvar zettel-jump-back-list '())
(defvar zettel-start-file "zettel.zettel")

(defvar zettel-link-regex)
(setq zettel-link-regex "\|[a-zA-Z\-0-9\_]+\|")
(defvar zettel-tag-regex)
(setq zettel-tag-regex "#[a-zA-Z0-9\-]+")

(setq zettel-highlights
      `((,zettel-tag-regex . font-lock-doc-face)
        (,zettel-link-regex . font-lock-function-name-face)))

(defun filename-to-zettel-name (fname)
  (concat "|"
          (first (last (butlast (split-string fname "[/.]"))))
          "|"))

(defvar zettel-file-cache nil)

(defun zettel-all-files ()
  (if zettel-file-cache zettel-file-cache
    (setq zettel-file-cache 
          (directory-files zettel-directory t "^[^\.\#].+zettel$"))))

(defun zettel-all-matches-in-string (regex string &optional acc start)
  (let ((match (string-match regex string (if start start 0))))
    (if match (zettel-all-matches-in-string regex string
                                            (cons (match-string 0 string) acc)
                                            (1+ match))
      acc)))

(defun zettel-tags-in-file (file)
  (zettel-all-matches-in-string zettel-tag-regex (zettel-file-to-string file)))

(defvar zettel-tag-cache nil)
(defun zettel-all-tags ()
  (if zettel-tag-cache zettel-tag-cache
    (setq zettel-tag-cache
          (sort (remove-duplicates
                 (apply 'append
                        (mapcar 'zettel-tags-in-file  (zettel-all-files)))
                 :test 'equal)
                'string-lessp))))

(defun zettel-link-names ()
  (mapcar #'filename-to-zettel-name (zettel-all-files)))

(defun zettel-clear-caches ()
  (setq zettel-tag-cache nil)
  (setq zettel-file-cache nil)
  (setq zettel-completion-cache nil))

(defvar zettel-completion-cache nil)
(defun zettel-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds))
         (completions (if zettel-completion-cache zettel-completion-cache
                        (setq zettel-completion-cache
                              (append (zettel-link-names)
                                      (zettel-all-tags))))))
    (list start end completions . nil)))


(defun valid-zettel-note-name-p (string)
  (and string 
       (let ((trimmed (string-trim string)))
         (and (s-starts-with-p "|" trimmed)
              (s-ends-with-p "|" trimmed)))))


(defun zettel-link-to-file-path (link)
  (let ((name (string-trim link "[\s\|]+" "[\s\|]+")))
    (concat zettel-directory name ".zettel")))

(defun find-zettel-file (thing)
  (when (valid-zettel-note-name-p thing)
    (when buffer-file-name 
      (push buffer-file-name zettel-jump-back-list)
      (save-buffer))
    (find-file (zettel-link-to-file-path thing))
    t))

(defun zettel-file-contains-p (thing file)
  (when (file-exists-p file)
    (let ((file-string (zettel-file-to-string file)))
      (search thing file-string))))

(defun zettel-show-notes-by (thing file-pred  message)
  "FILE-PRED should take two arguments, the first is 'thing' and the second is a file path."
  (with-output-to-temp-buffer zettel-tags-buffer-name
    (with-current-buffer zettel-tags-buffer-name
      (zettel-mode)
      (princ (concat message thing))
      (terpri)
      (dolist (name (zettel-link-names))
        (when (funcall file-pred thing (zettel-link-to-file-path name))
          (princ name)
          (terpri)))
      (read-only-mode)
      (use-local-map (copy-keymap (make-sparse-keymap)))
      (local-set-key (kbd "q")  'zettel-dismiss-tags-buffer)
      (local-set-key (kbd "<return>") 'zettel-jump-to-note)
      (local-set-key (kbd "M-n") 'zettel-next-thing-in-buffer)
      (local-set-key (kbd "M-p") 'zettel-prev-thing-in-buffer)
      (switch-to-buffer zettel-tags-buffer-name)))  )

(defun zettel-show-notes-by-tag (tag)
  (zettel-show-notes-by tag 'zettel-file-contains-p "Notes tagged with "))

(defun zettel-file-to-string (file)
  (with-temp-buffer (insert-file-contents file)
                    (buffer-string)))

(defun zettel-show-notes-linking-here (here-name)
  (zettel-show-notes-by here-name 'zettel-file-contains-p "Notes that link to "))

(defun zettel-browse-notes-linking-here ()
  (interactive)
  (when buffer-file-name
    (let ((here-name (filename-to-zettel-name buffer-file-name)))
      (zettel-show-notes-linking-here here-name))))

(defun zettel-dismiss-tags-buffer ()
  (interactive)
  (when (get-buffer zettel-tags-buffer-name)
    (kill-buffer zettel-tags-buffer-name)))


(defun zettel-jump-to-note ()
  (interactive)
  (let ((thing (thing-at-point 'symbol t)))
    (when (find-zettel-file thing)
      (zettel-clear-caches)
      (zettel-dismiss-tags-buffer)
      t)))

(defun zettel-tag-p (str)
  (s-starts-with-p "#" str))

(defun zettel-browse-tag-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'word t)))
    (when (zettel-tag-p thing)
      (zettel-show-notes-by-tag thing)
      t)))

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

(defun zettel-search-notes (term)
  (interactive "sSearch: ")
  (zettel-show-notes-by term 'zettel-file-contains-p "Notes containing "))

(defun zettel-spanking-new-note (new-note)
  (interactive "sNew Note Called: ")
  (let ((new-note (string-trim (replace-regexp-in-string "[\s.\s-]+" "-" new-note)
                               "-" "-")))
    (find-file (concat zettel-directory "/" new-note ".zettel"))))

(defun zettel-browse-tags ()
  (interactive)
  (with-output-to-temp-buffer zettel-tags-buffer-name
    (with-current-buffer zettel-tags-buffer-name
      (zettel-mode)
      (princ "Zettelgunk Tags: ")
      (terpri)
      (dolist (tag (zettel-all-tags))
        (princ tag)
        (terpri))
      
      (read-only-mode)
      (use-local-map (copy-keymap (make-sparse-keymap)))
      (local-set-key (kbd "q")  'zettel-dismiss-tags-buffer)
      (local-set-key (kbd "<return>") 'zettel-follow-or-insert-newline)
      (local-set-key (kbd "M-n") 'zettel-next-thing-in-buffer)
      (local-set-key (kbd "M-p") 'zettel-prev-thing-in-buffer)
      (switch-to-buffer zettel-tags-buffer-name))))

(defun zettel ()
  (interactive)
  (let ((val (pop zettel-jump-back-list)))
    (if val (find-file val)
      (find-file (concat zettel-directory zettel-start-file)))))

(define-derived-mode zettel-mode fundamental-mode "zettel"
  "mode for navigating my zettelkasten system"
  (setq font-lock-defaults '(zettel-highlights))
  (add-hook 'completion-at-point-functions 'zettel-completion-at-point nil 'local))

(defun zettel-follow-or-insert-newline ()
  (interactive)
  (unless (or (zettel-jump-to-note) (zettel-browse-tag-at-point))
    (newline)))

(defun zettel-mode-config-hook ()
  ;; so that tags can be slurped up by thing-at-point
  (modify-syntax-entry ?# "w")
  (modify-syntax-entry ?- "w")
  (local-set-key (kbd "RET") 'zettel-follow-or-insert-newline)
  (local-set-key (kbd "M-n") 'zettel-next-thing-in-buffer)
  (local-set-key (kbd "M-p") 'zettel-prev-thing-in-buffer))


(add-hook 'zettel-mode-hook 'zettel-mode-config-hook)

(add-to-list 'auto-mode-alist '("\\.zettel\\'" . zettel-mode))

