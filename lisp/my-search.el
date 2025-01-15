;;; lisp/my-search.el -*- lexical-binding: t; -*-

(defun my/search-blogs ()
  (interactive)
  (let ((search-terms (format "site:ohyecloudy.com %s"
                              (my/search--use-region-or-read-user-input))))
    (my/search--browse "https://google.com/search"
                       `(("q" ,search-terms)))
    )
  )

(defun my/search-google ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://google.com/search"
                       `(("q" ,search-terms)))
    )
  )
(defun my/search-naver ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://search.naver.com/search.naver"
                       `(("query" ,search-terms)))
    )
  )
;; https://dict.naver.com/dict.search?dicQuery=help&query=help&target=dic&ie=utf8&query_utf&isOnlyViewEE
(defun my/search-dict ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://en.dict.naver.com/#/search"
                       `(("query" ,search-terms)))
    )
  )

(defun my/search-terms-naver ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "http://terms.naver.com/search.naver"
                       `(("query" ,search-terms)))
    )
  )

;; https://terms.naver.com/search.naver?query=%EC%83%81%EB%8C%80%EC%84%B1%EC%9D%B4%EB%A1%A0

(defun my/search-dict-daum ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://small.dic.daum.net/search.do"
                       `(("q" ,search-terms)))
    )
  )

;; (defun my/search-dotnet ()
;;   (interactive)
;;   (let ((search-terms (my/search--use-region-or-read-user-input)))
;;     (my/search--browse "https://learn.microsoft.com/ko-kr/search/"
;;                        `(("terms" ,search-terms)
;;                          ("scope" ".NET")))
;;     )
;;   )

;; Wikiquote Collection of quotations
(defun wikiquote-lookup-quote-en (word)
  (interactive (list (thing-at-point 'word)))
  (eww-browse-url (format "http://en.wikiquote.org/wiki/%s" word)))

(defun wikiquote-lookup-quote-ko (word)
  (interactive (list (thing-at-point 'word)))
  (eww-browse-url (format "http://ko.wikiquote.org/wiki/%s" word)))

;; ;; Wiktionary Dictionary and thesaurus
(defun wiktionary-lookup-word-en (word)
  (interactive (list (thing-at-point 'word)))
  (eww-browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

(defun wiktionary-lookup-word-ko (word)
  (interactive (list (thing-at-point 'word)))
  (eww-browse-url (format "http://ko.wiktionary.org/wiki/%s" word)))

(defun my/search-elixir ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://hexdocs.pm/elixir/search.html"
                       `(("q" ,search-terms)))
    )
  )

;; (defun my/search-flutter ()
;;   (interactive)
;;   (let ((search-terms (my/search--use-region-or-read-user-input)))
;;     (my/search--browse "https://docs.flutter.dev/search"
;;                        `(("q" ,search-terms)))
;;     )
;;   )

(defun my/search-onelook ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://onelook.com"
                       `(("w" ,search-terms)))
    )
  )

(defun my/search-core-dictionary ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "http://www.coredictionary.com/core"
                       `(("w" ,search-terms)))
    )
  )

(defun my/search-thesaurus ()
  (interactive)
  (let* ((search-terms (my/search--use-region-or-read-user-input))
         (url (format "https://www.thesaurus.com/browse/%s" search-terms)))
    (browse-url url)))

(defun my/search--browse (base-url query-string-list)
  (let* ((query-string (url-build-query-string query-string-list))
         (url (format "%s?%s" base-url query-string)))
    (eww-browse-url url)))

(defun my/search--use-region-or-read-user-input ()
  (if (doom-region-active-p)
      (buffer-substring (doom-region-beginning) (doom-region-end))
    (read-string "Enter your search terms: ")))


;;; w3m

;; (defun google-search ()
;;   "search word under cursor in google code search and google.com"
;;   (interactive)
;;   (require 'w3m)
;;   (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
;;     (browse-url (concat "https://www.google.com/search\\?q=" keyword "")))
;;   )

;; (defun github-search ()
;;   "search word under cursor in google code search and google.com"
;;   (interactive)
;;   (require 'w3m)
;;   (let ((keyword (w3m-url-encode-string (read-string "Enter Search Text: "))))
;;     (browse-url (concat "https://www.google.com/search\\?q=" keyword "+site:github.com")))
;;   )

;;; provide

(provide 'my-search)

;;; my-search.el ends here
