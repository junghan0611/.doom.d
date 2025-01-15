;;; +gptel.el -*- lexical-binding: t; -*-

;; /home/junghan/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/ai/autoload/gptel.el

(defvar +gptel-improve-text-prompt nil)

(defvar +gptel-improve-text-prompts-history
  (list
   (concat "You are a spelling corrector and text improver. "
           "Only correct mistakes, do not alter the text structure unless stylistic, "
           "orthographic, morphologic and other linguistic errors found. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are a fact-checker and text enhancer. "
           "Fix mistakes and flag factual inaccuracies, do not alter the text structure "
           "unless it is absolutely necessary. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are spelling corrector and text enhancer. "
           "Provide 3 different improved variations of the given text, "
           "separating each variant with: "
           "\n\n---\n\n"
           "Do not include any explanations, titles, headers or bullet points "
           "- ONLY plain text of variants, nothing else!")

   (concat "You are an experienced software developer. Explain the given code snippet, "
           "diving into technical details for better understanding. "
           "Suggest a better approach if necessary. "
           "Strive for concise code that is easy-to-reason about. "
           "Optionally, recommend libraries, tools and literature for better "
           "understanding the problem and improving upon it.")

   (concat "You are a great software developer. "
           "You strive for simplicity in your code "
           "that is both concise and easy-to-reason about. "
           "Add comments to the provide code snippet, without changing the code itself."
           "Do not include any headers, titles or explanations outside of the snippet, "
           "keep the three ticks with the language designator (markdown code markup).")))

(defun +replace-region-with-string (replacement buffer beg end)
  "Replace region or buffer content with REPLACEMENT."
  (with-current-buffer buffer
    (delete-region beg end)
    (insert replacement)
    (insert "\n")))

(transient-define-infix +gptel--improve-text-infix-prompt ()
  "Prompt selection for improving text."
  :description "Set prompt"
  :prompt "Prompt: "
  :variable '+gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "RET"
  :format "%k %d"
  :reader (lambda (prompt &rest _)
            ;; usual bs to keep the preserve the list order
            (let* ((comp-table (lambda (cs)
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action cs str pred)))))
                   (sel (completing-read
                         prompt
                         (funcall
                          comp-table
                          +gptel-improve-text-prompts-history))))
              (add-to-list '+gptel-improve-text-prompts-history
                           sel)
              sel)))

(require 'gptel-transient)

;;;###autoload
(transient-define-prefix +gptel-improve-text-transient ()
  "Improve region with gptel."
  [:description
   (lambda ()
     (concat
      (or +gptel-improve-text-prompt
          (car +gptel-improve-text-prompts-history)) "\n"))
   [(gptel--infix-provider)
    (+gptel--improve-text-infix-prompt)]
   [""
    ("C-<return>" "Let's go" +gptel-improve-text)]])

;;;###autoload
(defun +gptel-improve-text (&optional arg)
  (interactive "P")
  (unless (region-active-p)
    (user-error "no selection"))
  (setq +gptel-improve-text-prompt (or +gptel-improve-text-prompt
                                       (car +gptel-improve-text-prompts-history)))
  (let* ((buffer (current-buffer))
         (beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end))
         (in-place? (string-match-p
                     "fix mistakes\\|correct mistakes"
                     +gptel-improve-text-prompt)))
    (message "beep-bop... checking your crap with %s" gptel-model)
    (gptel-request text
      :system +gptel-improve-text-prompt
      :buffer buffer
      :callback
      (lambda (resp info)
        (let* ((model (let-plist info .data.model)))
          (cond
           (in-place?
            (let* ((_ (+replace-region-with-string resp buffer beg end))
                   (_ (message "딱 그거야!")) ; ¡Ahí está!
                   (fst-buf (with-current-buffer (generate-new-buffer (format "* %s 1 *" model))
                              (insert text)
                              (current-buffer)))
                   (snd-buf (with-current-buffer (generate-new-buffer (format "* %s 2 *" model))
                              (insert resp)
                              (current-buffer)))
                   (diff-win (diff fst-buf snd-buf "--text" 'no-async)))

              ;; cleaner diff
              (with-current-buffer (window-buffer diff-win)
                (read-only-mode -1)
                (goto-char (point-min))
                (dolist (r '("^diff.*\n"
                             "^. No newline at end of file\n"
                             "^. No newline at end of file\n"
                             "^Diff finished.*$"))
                  (re-search-forward r nil :noerror)
                  (replace-match ""))
                (visual-line-mode))
              (kill-buffer fst-buf)
              (kill-buffer snd-buf)))

           (t
            (let ((buf (generate-new-buffer (format "* %s *" model))))
              (with-current-buffer buf
                (markdown-mode)
                (insert resp))
              (switch-to-buffer-other-window buf)))))))))

;;;###autoload
(defun gptel-clear-buffer+ ()
  (interactive)
  (let* ((beg-marker (concat "^" (alist-get gptel-default-mode gptel-prompt-prefix-alist)))
         (keep-line (save-excursion
                      (goto-char (point-max))
                      (when (re-search-backward beg-marker nil t)
                        (unless (save-excursion
                                  (forward-line)
                                  (re-search-forward beg-marker nil t))
                          (point))))))
    (delete-region (point-min) keep-line)
    (evil-insert-state)))

;;;###autoload
(defun gptel+ (&optional arg)
  (interactive "P")
  (let ((last-b (thread-last
                  (buffer-list)
                  (seq-filter
                   (lambda (buf)
                     (buffer-local-value 'gptel-mode buf)))
                  (seq-sort
                   (lambda (a b)
                     (string> (buffer-name a) (buffer-name b))))
                  (seq-first))))
    (if (or arg (null last-b))
        (call-interactively #'gptel)
      (if (get-buffer-window last-b 'visible)
          (switch-to-buffer-other-window last-b)
        (switch-to-buffer last-b)))))

;; (defun gptel-persist-history+ ()
;;   "Save buffer to disk when starting gptel"
;;   (interactive)
;;   (unless (buffer-file-name (current-buffer))
;;     (let ((suffix (format-time-string "%Y%m%dT%H%M%S"))
;;           (chat-dir (concat org-directory "/llmlog"))
;;           (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode))))
;;       (unless (file-directory-p chat-dir)
;;         (make-directory chat-dir :parents))
;;       (write-file
;;        (expand-file-name (concat suffix "__llmlog" "." ext) chat-dir)))))
