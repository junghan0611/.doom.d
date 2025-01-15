;;; lisp/omni-search.el -*- lexical-binding: t; -*-

;;; agzam

(defvar +search--github-mode->lang
  '(((clojurescript clojure cider-clojure-interaction) . "Clojure")
    ((emacs-lisp Info lisp-data helpful) . "Emacs Lisp")
    ((tsx-ts) . "TypeScript")
    ((js jtsx-jsx) . "JavaScript")
    ((fennel) . "Fennel"))
  "Associates current mode with a language in Github terms")

;;;###autoload
(defun +search-github-with-lang ()
  "Search on Github with attempt of detecting language associated with current-buffer's mode"
  (interactive)
  (let* ((mode (intern (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
         (lang (cl-some (lambda (entry)
                          (when (memq mode (car entry))
                            (cdr entry)))
                        +search--github-mode->lang))
         (lang-term (if lang (concat "language:\"" lang "\" ") ""))
         (word-at-point (if (region-active-p)
                            (buffer-substring (region-beginning) (region-end))
                          (thing-at-point 'symbol)))
         (search-term (read-string "Search Github: " (concat lang-term word-at-point)))
         (query (format "https://github.com/search?q=%s&type=code"
                        (url-hexify-string search-term))))
    (browse-url query)))

;;;###autoload
(defun consult-omni--set-api-keys ()
  "Read list of keys and set for corresponding consult-omni-source a
function that retrieves the API key from ~/.authinfo.gpg.

It's safer to use a function rather than the concrete value of a key"
  (cl-labels ((split-col (x) (split-string x ":")))
    (let ((keys-list
           `((consult-omni-brave-api-key "api.search.brave.com")
             ;; (consult-omni-scopus-api-key "api.elsevier.com")
             (consult-omni-youtube-search-key "youtube-api")
             ;; googleapis.com record has two, colon separated parts:
             ;; custom search ID and the API key
             (consult-omni-google-customsearch-cx
              "www.googleapis.com" ,(-compose #'car #'split-col))
             (consult-omni-google-customsearch-key
              "www.googleapis.com" ,(-compose #'cadr #'split-col)))))
      (dolist (k keys-list)
        (let ((key (car k))
              (host (nth 1 k))
              (fn (nth 2 k))
              (auth-source-debug nil))
          (set (intern (symbol-name key))
               (lambda ()
                 (when-let ((ps
                             (with-temp-message ""
                               (auth-source-pick-first-password :host host))))
                   (if fn (funcall fn ps)
                     ps)))))))))

;;;###autoload
(defun consult-omni-load-sources+ ()
  (dolist (m '(consult-omni-brave
               consult-omni-browser-history
               consult-omni-duckduckgo
               consult-omni-elfeed
               consult-omni-gh
               consult-omni-google
               consult-omni-gptel
               consult-omni-invidious
               consult-omni-line-multi
               consult-omni-notmuch
               consult-omni-wikipedia
               consult-omni-youtube))
    (require m nil t)))

;;;###autoload
(transient-define-prefix consult-omni-transient ()
  ["consult-omni"
   [("/" "multi" consult-omni-multi)
    ("go" "google" consult-omni-google)
    ("w" "wiki" consult-omni-wikipedia)
    ("y" "youtube" consult-omni-youtube)
    ("gh" "code search" +search-github-with-lang)
    ("gH" "github" consult-omni-github)]
   [("bh" "browser-hist" consult-omni-browser-history)
    ("el" "elfeed" consult-omni-elfeed)
    ("no" "notmuch" consult-omni-notmuch)
    ("gp" "gptel" consult-omni-gptel)]])

(advice-add 'consult-omni-transient :before #'consult-omni-load-sources+)

;;; custom functions

(defun consult-omni-embark-open-consult-gh (cand)
  "Search github for repo in candidate by using `consult-gh-search-repos'."
  (when-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
    (if (string-match ".*github.*" url nil nil)
        (let* ((urlobj (url-generic-parse-url url))
               (path (url-filename urlobj))
               (repo (string-join (take 2 (cdr (string-split path "/"))) "/"))
               (issue (if (string-match ".*\/issues\/\\([[:digit:]]*\\)" path)
                          (substring-no-properties (match-string 1 path))))
               (pr (if (string-match ".*\/pull\/\\(?1:[[:digit:]]*\\)" path)
                       (substring-no-properties (match-string 1 path))))
               )
          (cond
           (pr (consult-gh-search-prs pr repo))
           (issue (consult-gh-search-issues issue repo))
           (repo (consult-gh-search-repos repo))
           (t (consult-gh-search-repos)))
          (message "not a github link"))
      )))

(provide 'my-consult-omni)
