;;; lisp/denote-hugo.el -*- lexical-binding: t; -*-

;;; blogging with denote and hugo

;; .dir.locals.el
;; (org-mode . ((eval . (auto-fill-mode 0))
;;              (eval . (org-indent-mode 0))
;;              ;; (eval . (org-rainbow-tags-mode 1))
;;              (eval . (jinx-mode -1))
;;              (eval . (org-glossary-mode 0))
;;              (org-hugo-section . "talks")
;;              (org-hugo-base-dir . "~/git/notes/")
;;              ;; (eval . (org-hugo-auto-export-mode))
;;              ))

;;;; org-export

;; localauthor-dotfiles-zk/my-lisp/gr-org-extras.el
;; org-export-before-processing-hook는 매크로, Babel 코드, 및 include 키워드의 확장이 이루어지기 전에 호출됩니다.
;; org-export-before-parsing-hook는 버퍼가 파싱되기 전에 호출됩니다.

;; (add-hook 'org-export-before-processing-functions (lambda (backend) (org-update-all-dblocks)))
;; (add-hook 'org-export-before-parsing-functions (lambda (backend) (org-update-all-dblocks)))

;;;; my/org-update-all-dblocks

(progn
  (defun my/org-update-all-dblocks ()
    (interactive)
    (save-excursion
      (goto-char 0)
      (org-update-all-dblocks)
      ))

  (defun my/org-update-all-dblocks-on-directory (directory)
    "Export all Org files in the specified DIRECTORY to Markdown using `org-hugo-export-to-md`."
    (interactive "DSelect directory: ")
    (let ((org-files (directory-files-recursively directory "\\.org\\'")))
      (dolist (org-file org-files)
        (with-current-buffer (find-file-noselect org-file)
          (my/org-update-all-dblocks)
          ))))
  )

;;;; my/denote-link-export

;; (require 'ox)
;; https://jiewawa.me/2024/03/blogging-with-denote-and-hugo/
;; https://weblog.masukomi.org/2024/07/19/using-org-mode-with-hugo/


;; 1) private note
;; 2) /blog and draft keyword
;; 3) /blog without keyword

;; (defun my/denote-link-ol-export (link description format)
;;   " Modified version of `denote-link-ol-export'.
;; Replace markdown export with `my/denote-markdown-export'
;; Original docstring below: "

;;   (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
;;          (path (file-relative-name (car path-id)))
;;          ;; (heading-id (denote-org-extras--get-file-id-and-heading-id path))
;;          (p (file-name-sans-extension path))
;;          (id (cdr path-id))
;;          (desc (or description (concat "denote:" id))))
;;     (cond
;;      ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" p desc))
;;      ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
;;      ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
;;      ((eq format 'ascii) (format "[%s] <denote:%s>" desc path)) ; NOTE 2022-06-16: May be tweaked further
;;      ((eq format 'md) (my/denote-markdown-export link desc))
;;      (t path))))

(defun my/denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* (
         (path-id (denote-link--ol-resolve-link-to-target link :full-data))
         (path (file-relative-name (nth 0 path-id)))
         (id (nth 1 path-id))
         (query (nth 2 path-id))
         (anchor (file-name-sans-extension path))
         (desc (cond
                (description)
                (query (format "denote:%s::%s" id query))
                (t (concat "denote:" id)))))
    (cond
     ((eq format 'html)
      (if query
          (format "<a href=\"%s.html%s\">%s</a>" anchor query desc)
        (format "<a href=\"%s.html\">%s</a>" anchor desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
     ((eq format 'md)  (my/denote-markdown-export link desc))
     ;; ((eq format 'md) (format "[%s](%s)" desc path))
     (t path))))

;; [[denote:20240324T074729::#h:5bde73df-7451-4b96-bc99-b878e0fe0cfe][창조적 행위와 도구 : 존재의 방식::ADHD : 그의 존재의 방식]]
;; - [창조적 행위와 도구 : 존재의 방식]({{< relref "/blog/20240324T074729-creative-way.md/#h:5bde73df-7451-4b96-bc99-b878e0fe0cfe" >}})
;; - [창조적 행위와 도구 : 존재의 방식](/blog/20240324T074729-creative-way/#h:5bde73df-7451-4b96-bc99-b878e0fe0cfe)

;; (defun my/get-export-file-name-from-org (filepath)
;;   "Retrieve the value of #+output_file_name from the Org file at FILEPATH in an efficient way."
;;   (if (file-exists-p filepath)
;;       (progn
;;         (message "File found: %s" filepath)
;;         (org-element-map (org-element-parse-buffer 'element) 'keyword
;;           (lambda (el)
;;             (when (string= (org-element-property :key el) "EXPORT_FILE_NAME")
;;               (org-element-property :value el)))
;;           nil t))
;;     (message "File not found: %s" filepath)
;;     nil))

(defun my/get-export-file-name-from-file (filepath)
  "Retrieve the value of #+export_file_name from the Org file at FILEPATH in an efficient way."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath nil 0 1000) ; 파일의 처음 1000바이트만 삽입
        ;; (message "File found: %s" filepath)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+export_file_name: \\(.*\\)$" nil t)
            (match-string 1)
          (progn
            ;; (message "^#\\+export_file_name not found: %s" filepath)
            ;; nil
            "notfound39219129.md"
            )))
    (progn
      (message "File not found: %s" filepath) nil)))

(defun my/is-docs-file (path)
  "Check if the file at PATH belongs to the blog directory."
  (string-match "/docs" path))

(defun my/is-blog-file (path)
  "Check if the file at PATH belongs to the blog directory."
  (string-match "/blog" path))

(defun my/is-journal-file (path)
  "Check if the file at PATH belongs to the journal directory."
  (string-match "/journal" path))

(defun my/is-notes-file (path)
  "Check if the file at PATH belongs to the notes directory."
  (string-match "/notes" path))

;; (defun my/is-llm-file (path)
;;   "Check if the file at PATH belongs to the llm directory."
;;   (string-match "/llmlog" path))

(defun my/is-md-file (path)
  "Check if the file at PATH belongs to the md directory."
  (string-match "/md" path))

(defun my/is-private-file (path)
  "Check if the file at PATH belongs to the private directory."
  (string-match "/private" path))

(defun my/is-draft-file (path)
  "Check if the file at PATH is a draft."
  (string-match "_draft" path))

(defun my/get-hugo-section-directory-from-path (path)
  (let ((directories '("/notes" "/journal" "/talks" "/meta" "/bib")) ; "/llmlog" "/notes"  "/agenda" "/talks" "/blog"
        (matched-dir nil))
    (dolist (dir directories)
      (when (string-match-p (regexp-quote dir) path)
        (setq matched-dir dir)))
    (if (not matched-dir)
        (setq matched-dir ""))
    matched-dir))

;; 1. `my/denote-markdown-export` 함수를 수정하여 `blog` 폴더가 아닌 경우 `exportfilename`이 없을 시 슬러기한 파일명으로 내보내도록 변경.

(defun my/denote-markdown-export (link desc)
  "Format the way Denote links are exported to markdown.
If LINK is considered private or a draft, return DESC.
If LINK is considered a public note, format it as a Hugo relative link.
If USE-RELREF is non-nil, format it as a Hugo relref link."
  (let* (
         ;; (path (denote-get-path-by-id link)) ; only i
         ;; (pathr (file-relative-name (nth 0 path-id)))
         ;; (final-filename (concat (denote-retrieve-filename-identifier path) ".md"))
         (path-id (denote-link--ol-resolve-link-to-target link :full-data))
         (path (nth 0 path-id))
         (section (my/get-hugo-section-directory-from-path path))
         (id (nth 1 path-id))
         (query (nth 2 path-id))
         (exportfilename (my/get-export-file-name-from-file path))
         (content-dir (concat (file-name-as-directory org-hugo-base-dir)
                              (format "content/%s/" section)))
         (exportfilepath (when (and exportfilename org-hugo-base-dir)
                           (expand-file-name exportfilename content-dir)))
         (uri (cond
               (query (format "%s/%s" exportfilename query)) ; custom header
               (t (format "%s" exportfilename)))))

    (format "[%s]" section)
    ;; (message "%s" (format "[%s]({{< relref \"%s/%s\" >}})" desc section uri))

    ;; 내보낸 파일이 없는 경우 링크 만들지 않도록
    ;; (if  (not (file-exists-p exportfilepath))
    ;;     (format "[%s]" desc) ;; 내보낸 파일이 없다면 링크 만들지 마라

    (cond
     ;; 1) For files in Digital Garden for ALL
     ;; ((and (string-match "blog" org-hugo-base-dir) (or (my/is-docs-file path) (my/is-blog-file path))) ;;
     ((string-match "notes" org-hugo-base-dir) ;; for quartz
      (if (or (my/is-draft-file path) (my/is-md-file path)) ; (my/is-llm-file path) (my/is-docs-file path) (my/is-blog-file path)
          (format "[%s]" desc)
        (if exportfilename
            ;; quartz doesn't support custom header
            (format "[%s]({{< relref \"%s/%s\" >}})" desc section exportfilename) ; uri
          (format "[%s]" desc))))

     ;; 2) For files in other directories based on org-hugo-base-dir
     ;; ((and (string-match "notes" org-hugo-base-dir))
     ;; ((string-match "notes" org-hugo-base-dir)
     ;;  (if (my/is-draft-file path)
     ;;      (format "[%s]" desc)
     ;;    (if (or (my/is-docs-file path) (my/is-blog-file path))
     ;;        (format "[%s]" desc)
     ;;      (if exportfilename
     ;;          (format "[%s]({{< relref \"%s/%s\" >}})" desc section uri)
     ;;        (format "[%s]" desc)))
     ;;    ))
     ;; Fallback
     (t (format "[%s]" desc)))
    ;; ) ; end-of if
    ))

(org-link-set-parameters "denote" :export #'my/denote-link-ol-export)

;;;; my/org-hugo-export-directory

;; 2. 선택한 폴더의 모든 Org 파일에 대해 `org-hugo-export-to-md`를 호출하여 내보내기 하는 함수 추가.

(defun my/org-hugo-export-directory (directory)
  "Export all Org files in the specified DIRECTORY to Markdown using `org-hugo-export-to-md`."
  (interactive "DSelect directory: ")
  (let ((org-files (directory-files-recursively directory "\\.org\\'")))
    (dolist (org-file org-files)
      (with-current-buffer (find-file-noselect org-file)
        (org-hugo-export-to-md)))))

;;;; my/open-hugo-markdown-file

(defun my/get-export-file-name-from-buffer ()
  "Retrieve the value of #+EXPORT_FILE_NAME from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+EXPORT_FILE_NAME: \\(.*\\)$" nil t)
        (match-string 1)
      nil)))

;; (defun my/get-hugo-base-dir ()
;;   "Retrieve the value of `org-hugo-base-dir` for the current directory."
;;   (let ((base-dir (or (cdr (assoc 'org-hugo-base-dir (dir-locals-read-from-dir (file-name-directory (buffer-file-name)))))
;;                       org-hugo-base-dir)))
;;     (if base-dir
;;         (expand-file-name base-dir)
;;       nil)))

(defun my/org-open-exported-markdown-in-hugo-content ()
  "Open the Markdown file exported from the current Org-mode buffer, located in the `org-hugo-base-dir` content folder."
  (interactive)
  (let* ((exportfilename (my/get-export-file-name-from-buffer))
         ;; (base-dir (my/get-hugo-base-dir))
         ;; (base-dir (org-hugo-base-dir))
         (content-dir (concat (file-name-as-directory org-hugo-base-dir)
                              (format "content/%s/" org-hugo-section)))
         (exportfilepath (when (and exportfilename org-hugo-base-dir)
                           (expand-file-name exportfilename content-dir))))
    (if (and exportfilepath (file-exists-p exportfilepath))
        (find-file exportfilepath)
      (message "Markdown file not found: %s" exportfilepath))))

;;;; my/denote-convert-note-to-blog-post

;; This is a private note: [[denote:20230708T141810][Books]]
;; This is a draft blog post: [[denote:20240323T143034][Hacking on Denote and Hugo]]
;; This is a published blog post: [[denote:20231104T134226][A DWIM fullscreen function for Emacs and Sway]]

(defun my/insert-hugo-draft-status ()
  "Add metadata to current org-mode file marking it as a Hugo draft."
  (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward "filetags")
    (end-of-line)
    (insert "\n#+hugo_draft: t ")))

(progn
  ;; identifier as 'export-file-name'
  (defun my/insert-hugo-export-file-name ()
    "Add metadata to current org-mode file containing export file name.
  Export File Name is returned by `denote-retrieve-title-value'."
    (interactive)
    (save-excursion
      (goto-char 0)
      (search-forward "identifier")
      (end-of-line)
      (insert (format
               "\n#+export_file_name: %s.md"
               (denote-retrieve-filename-identifier buffer-file-name)))))

  ;; (defun my/insert-hugo-export-file-on-directory (directory)
  ;;   "Export all Org files in the specified DIRECTORY to Markdown using `org-hugo-export-to-md`."
  ;;   (interactive "DSelect directory: ")
  ;;   (let ((org-files (directory-files-recursively directory "\\.org\\'")))
  ;;     (dolist (org-file org-files)
  ;;       (with-current-buffer (find-file-noselect org-file)
  ;;         (my/insert-hugo-export-file-name)
  ;;         ))))
  )

(defun my/move-current-buffer-file-to-subdirectory (subdirectory)
  "Move file of current buffer to SUBDIRECTORY seamlessy.
  There should be no discernable difference in the buffer's appearance."
  (interactive)
  (let ((input-file (buffer-file-name))
        (output-file (concat (denote-directory)
                             subdirectory
                             (file-relative-name (buffer-file-name))))
        (window-pos (window-start))
        (cursor-pos (point)))
    (save-buffer)
    (rename-file input-file output-file)
    (find-file output-file)
    (set-window-start (selected-window) window-pos)
    (goto-char cursor-pos)
    (kill-buffer (get-file-buffer input-file))))

(defun my/denote-convert-note-to-blog-draft ()
  "Mark file of current Denote buffer to be marked as a draft blog post."
  (interactive)
  ;; (my/move-current-buffer-file-to-subdirectory "blog/")
  ;; (denote-rename-file-keywords '("draft")) ;; ?!
  (my/insert-hugo-draft-status)
  (my/insert-hugo-export-file-name))

;;;; KLUDGE my/insert-hugo-xxxx

;; (progn
;;   (defun my/insert-hugo-lastmod-time-stamp ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char 0)
;;       (search-forward "date:")
;;       (end-of-line)
;;       (insert "\n#+hugo_lastmod: "
;;               )))

;;   (defun my/insert-hugo-lastmod-time-stamp-on-directory (directory)
;;     "Export all Org files in the specified DIRECTORY to Markdown using `org-hugo-export-to-md`."
;;     (interactive "DSelect directory: ")
;;     (let ((org-files (directory-files-recursively directory "\\.org\\'")))
;;       (dolist (org-file org-files)
;;         (with-current-buffer (find-file-noselect org-file)
;;           (my/insert-hugo-lastmod-time-stamp)
;;           ))))
;;   )


;; Howto - my/process-files-by-extension
;; (defun my/insert-hugo-tags ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char 0)
;;     (search-forward "export_file_name")
;;     (end-of-line)
;;     (insert "\n#+hugo_tags: notes"
;;             )))

;; ;; (diredp-do-apply/eval-marked 'my/insert-hugo-categories '(4))

;; (progn
;;   (defun my/insert-hugo-bibliography ()
;;     "Add metadata to current org-mode file containing export file name.
;;   Export File Name is returned by `denote-retrieve-title-value'."
;;     (interactive)
;;     (save-excursion
;;       (evil-goto-line)
;;       (end-of-line)
;;       (insert "\n* Related-Notes\n\n#+print_bibliography:"
;;               )))

;;   (defun my/insert-hugo-bibliography-on-directory (directory)
;;     "Export all Org files in the specified DIRECTORY to Markdown using `org-hugo-export-to-md`."
;;     (interactive "DSelect directory: ")
;;     (let ((org-files (directory-files-recursively directory "\\.org\\'")))
;;       (dolist (org-file org-files)
;;         (with-current-buffer (find-file-noselect org-file)
;;           (my/insert-hugo-bibliography)
;;           ))))
;;   )

;;;; my/denote-publish-hugo-post

(defun my/insert-hugo-published-date ()
  "Format the current date and add it to Org-Mode metadata."
  ;; (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward "filetags")
    (end-of-line)
    (insert (concat "\n#+hugo_publishdate: "(format-time-string  "%Y-%m-%d")))))

(defun my/denote-remove-draft-tag-from-metadata ()
  "Remove \"draft\" tag from Org-Mode metadata."
  ;; (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward "filetags")
    (when (search-forward-regexp ":draft\\|draft:" (line-end-position) t)
      (replace-match "")
      (when (and (looking-at ":$\\|: ") (looking-back " "))
        (delete-char 1)))))

(defun my/denote-remove-hugo-draft-status ()
  "Remove Hugo draft entry from Org-Mode metadata."
  ;; (interactive)
  (save-excursion
    (goto-char 0)
    (when (search-forward "#+hugo_draft: t")
      (beginning-of-line)
      (kill-line)
      (kill-line))))

(defun my/denote-remove-keyword-from-filename (keyword)
  "Remove Denote keyword \"draft\" from filename of current file."
  ;; (interactive)
  (denote-rename-file buffer-file-name
                      (denote-retrieve-filename-title buffer-file-name)
                      (delete keyword
                              (denote-retrieve-keywords-value
                               buffer-file-name 'org))
                      nil))

(defun my/denote-convert-blog-ready-to-hugo ()
  "Mark file of current `denote' buffer to be published as a blog post."
  (interactive)
  (my/insert-hugo-published-date)
  (my/denote-remove-hugo-draft-status)
  (my/denote-remove-draft-tag-from-metadata)
  (my/denote-remove-keyword-from-filename "draft"))

;;; provide

(provide 'denote-hugo)

;;; end-of-line
