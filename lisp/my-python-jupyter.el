;;; lisp/my-python-jupyter.el -*- lexical-binding: t; -*-

;; dotsamples/vanilla/karthink-dotfiles-popper/lisp/setup-python.el

;;;; jupyter - corfu and company

(with-eval-after-load 'jupyter
  ;; :bind (:map jupyter-repl-interaction-mode-map
  ;;             ("M-i"   . nil)
  ;;             ("C-h ." . jupyter-inspect-at-point))

  ;; Make jupyter's completion work with corfu-show-documentation
  (unless (fboundp 'company-mode)
    (defun company-doc-buffer (&optional string)
      (with-current-buffer (get-buffer-create "*jupyter help*")
        (erase-buffer)
        (markdown-mode)
        (when string
          (save-excursion
            (insert string)
            (visual-line-mode)))
        (current-buffer))))

  (defun jupyter-completion--company-doc-buffer (arg)
    "Send an inspect request for ARG to the kernel.
Use the `company-doc-buffer' to insert the results."
    (let* ((buf (company-doc-buffer))
           (prefix (car (jupyter-code-context 'inspect)))
           (sym (if (string-match ".*\\." prefix)
                    (concat (match-string 0 prefix) arg)
                  arg)))
      (jupyter-inspect sym (length sym) buf)
      (with-current-buffer buf
        (when (> (point-max) (point-min))
          (let ((inhibit-read-only t))
            (remove-text-properties
             (point-min) (point-max) '(read-only))
            (font-lock-mode 1)
            (goto-char (point-min))
            (current-buffer))))))
  )

;;;; ob-jupyter - ansi block

(with-eval-after-load 'ob-jupyter
  ;; :bind (:map jupyter-org-interaction-mode-map
  ;;             ("M-i"   . nil)
  ;;             ("C-h ." . jupyter-inspect-at-point))
  ;; Clean up ob-jupyter source block output
  ;; From Henrik Lissner
  (defun my/org-babel-jupyter-strip-ansi-escapes-block ()
    (when (string-match-p "^jupyter-"
                          (nth 0 (org-babel-get-src-block-info)))
      (unless (or
               ;; ...but not while Emacs is exporting an org buffer (where
               ;; `org-display-inline-images' can be awfully slow).
               (bound-and-true-p org-export-current-backend)
               ;; ...and not while tangling org buffers (which happens in a temp
               ;; buffer where `buffer-file-name' is nil).
               (string-match-p "^ \\*temp" (buffer-name)))
        (save-excursion
          (when-let* ((beg (org-babel-where-is-src-block-result))
                      (end (progn (goto-char beg)
                                  (forward-line)
                                  (org-babel-result-end))))
            (ansi-color-apply-on-region (min beg end) (max beg end)))))))

  (add-hook 'org-babel-after-execute-hook
            #'my/org-babel-jupyter-strip-ansi-escapes-block)

  (define-key jupyter-org-interaction-mode-map (kbd "C-c h") nil)
  (define-key jupyter-org-interaction-mode-map (kbd "<f2>") 'jupyter-org-hydra/body)

  ;; (setq org-babel-default-header-args:jupyter-python
  ;;       '((:async . "yes") (:session . "py") (:kernel . "python3")
  ;;         ;; (:tangle . "jupyter-python/tangled.py")
  ;;         ;; (:exports . "both")
  ;;         ))
  )

;;; provide

(provide 'my-python-jupyter)
