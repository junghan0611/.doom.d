;;; +gptel.el -*- lexical-binding: t; -*-

;;;; gptel tips from agzam
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

;;;; model descriptions

;; updated 2025-01-27

;; sonar, sonar-pro

;; (defconst gptel--gemini-models
;;   '((gemini-1.5-pro-latest
;;      :description "Google's latest model with enhanced capabilities across various tasks"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 2000
;;      ;; input & output price is halved for prompts of 128k tokens or less
;;      :input-cost 2.50
;;      :output-cost 10
;;      :cutoff-date "2024-05")
;;     (gemini-2.0-flash-exp
;;      :description "Next generation features, superior speed, native tool use"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 1000
;;      :cutoff-date "2024-12")
;;     (gemini-1.5-flash
;;      :description "A faster, more efficient version of Gemini 1.5 optimized for speed"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 1000
;;      ;; input & output price is halved for prompts of 128k tokens or less
;;      :input-cost 0.15
;;      :output-cost 0.60
;;      :cutoff-date "2024-05")
;;     (gemini-1.5-flash-8b
;;      :description "High volume and lower intelligence tasks"
;;      :capabilities (tool-use json media)
;;      :context-window 1000
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      ;; input & output price is halved for prompts of 128k tokens or less
;;      :input-cost 0.075
;;      :output-cost 0.30
;;      :cutoff-date "2024-10")
;;     (gemini-2.0-flash-thinking-exp
;;      :description "Stronger reasoning capabilities."
;;      :capabilities (tool-use media)
;;      :context-window 32
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "text/plain" "text/csv" "text/html")
;;      :cutoff-date "2024-08")
;;     (gemini-exp-1206
;;      :description "Improved coding, reasoning and vision capabilities"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :cutoff-date "2024-12")
;;     (gemini-pro
;;      :description "The previous generation of Google's multimodal AI model"
;;      :capabilities (tool-use json media)
;;      :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
;;                   "application/pdf" "text/plain" "text/csv" "text/html")
;;      :context-window 32
;;      :input-cost 0.50
;;      :output-cost 1.50
;;      :cutoff-date "2023-02"))
;;   "List of available Gemini models and associated properties.
;; Keys:

;; - `:description': a brief description of the model.

;; - `:capabilities': a list of capabilities supported by the model.

;; - `:mime-types': a list of supported MIME types for media files.

;; - `:context-window': the context window size, in thousands of tokens.

;; - `:input-cost': the input cost, in US dollars per million tokens.

;; - `:output-cost': the output cost, in US dollars per million tokens.

;; - `:cutoff-date': the knowledge cutoff date.

;; - `:request-params': a plist of additional request parameters to
;;   include when using this model.

;; Information about the Gemini models was obtained from the following
;; source:

;; - <https://ai.google.dev/pricing>
;; - <https://cloud.google.com/vertex-ai/generative-ai/docs/learn/models>")
