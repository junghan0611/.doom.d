;;; lisp/denote-config.el -*- lexical-binding: t; -*-

;;; comments

;; 2024-08-26 use denote only

;;; DONT deregister the ':denote' link type

;; (progn
;;   ;; 'id' 를 사용해야 org-roam-buffer 가 유용하다.
;;   ;; I prefer to not make Denote itself a dependency - just use native Org Mode link types
;;   ;; such as =id= and =file=

;;   ;; 저는 Denote 자체를 종속성으로 만들지 않고 =id= 및 =file=과 같은 기본 조직 모드
;;   ;; 링크 유형을 사용하는 것을 선호합니다

;;   ;; 지우고
;;   (setq org-link-parameters
;;         (delq (assoc "denote" org-link-parameters) org-link-parameters))

;;   ;; change to denote to default id
;;   (setq denote-org-link-format "[[id:%s][%s]]")
;;   (setq denote-id-only-link-format "[[id:%s]]")
;;   (setq denote-id-only-link-in-context-regexp
;;         "\\[\\[id:\\(?1:\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)\\)]]")
;;   (setq denote-md-link-format "[%2$s](id:%1$s)"
;;         denote-md-link-in-context-regexp
;;         "\\[.*?](id:\\(?1:\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)\\))")
;;   )

;;; Extensions : category and project agenda

(progn
  (setq denote-known-keywords
        '("aprj"
          ;; "bprj"
          ;; "cprj"
          "meta"

          ;; - KE 엔지니어링 지식공학(KE)
          "pka" ; 어시스턴트
          "pkb" ; 베이스 리포 저장소
          "pkc" ; 컴퓨팅 환경 -> 리눅스
          "pkd" ; 디바이스
          "pke" ; 편집
          ;; "pkf"
          ;; "pkg"
          ;; "pkh"
          "pki" ; 인덱스 색인
          "pkj"
          "pkk"
          "pkl" ; 로직 논리
          "pkm" ; 관리 매니지먼트
          "pkn"
          "pko" ; 온톨로지 시맨틱 지식 그래프
          "pkp" ; 퍼블리시
          "pkq" ; 쿼리 질의
          "pkr"
          "pks" ; 서치 검색
          "pkt" ; 도구 툴
          "pku"
          "pkv"
          "pkw" ; 라이팅 글쓰기
          "pkx"
          "pky"
          "pkz"

          "archive"
          ))

  ;; 데이비드가 정의한 것들 뭐지?
  ;; "ply" "plm" "plw"
  ;; "kt" "ke" "kp" "kl" "ka" "kap"
  ;; "kcp" "kca" "kcc"
  ;; "kra" "krb" "krv" "rn"

  ;; Top-level keywords
  ;; (setq denote-known-keywords '("journal" "workflow" "daily" "weekly" "monthly"))

  ;; (defun dw/denote-find-daily-log ()
  ;;   (interactive)
  ;;   (let* ((default-directory denote-directory)
  ;;          (existing-file (denote--directory-files-matching-regexp (format-time-string "^%Y%m%d.*_daily"))))
  ;;     (if existing-file
  ;;         (find-file (expand-file-name (car existing-file)))
  ;;       ;; TODO: Initialize with daily note format
  ;;       (denote (format-time-string "%A, %B %e, %Y")
  ;;               '("daily")))))

;;; Extensions : agenda

  ;; Update agenda files after notes are created or renamed
  (add-hook 'denote-after-rename-file-hook #'my/refresh-agenda-files)
  (add-hook 'denote-after-new-note-hook #'my/refresh-agenda-files)

  ;; (defvar my/base-agenda-files
  ;;   (list
  ;;    (my/org-inbox-file)
  ;;    (my/org-tasks-file)
  ;;    ;; (my/org-diary-file)
  ;;    ;; (my/org-life-file)
  ;;    )
  ;;   "The base agenda files that will always be included.")

  ;; (defun dw/org-path (path)
  ;;   (expand-file-name path org-directory))

;;;###autoload
  (defun my/refresh-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (append (denote-directory-files "_aprj") ; active project
                  org-user-agenda-files ;; my/base-agenda-files
                  )))

  ;; category
  (defun my/denote-insert-category (category)
    (save-excursion
      (beginning-of-buffer)
      (while (and
              (< (point) (point-max))
              (string= "#+"
                       (buffer-substring-no-properties
                        (point-at-bol)
                        (+ (point-at-bol) 2))))
        (next-line))

      (insert "#+category: " category)
      (save-buffer)))

  (defun my/denote-insert-meta-links ()
    (interactive)
    (let* ((topics (mapcar (lambda (file)
                             (cons (denote-retrieve-front-matter-title-value file 'org)
                                   (denote-retrieve-filename-identifier file)))
                           (denote-directory-files "_meta")))
           (selected (completing-read-multiple "Select topics: " topics nil t)))
      (insert (string-join (mapcar (lambda (topic)
                                     (format
                                      "[[denote:%s][%s]]"
                                      (alist-get topic
                                                 topics
                                                 nil
                                                 nil
                                                 #'string=)
                                      topic))
                                   selected)
                           " "))))

  (defun my/denote-create-meta-note ()
    (interactive)
    (let* ((topic-files (mapcar (lambda (file)
                                  (cons (denote-retrieve-front-matter-title-value file 'org)
                                        file))
                                (denote-directory-files "_meta")))
           (selected-topic (completing-read "Select topic: "
                                            (mapcar #'car topic-files)))
           )

      (denote
       (denote-title-prompt (format "%s: " selected-topic))
       (denote-keywords-prompt)
       'org ; file-type
       (denote-subdirectory-prompt)
       )

      ;; (my/denote-insert-category "META") ;; selected-topic
      )
    )

  (defun my/denote-extract-subtree ()
    (interactive)
    (save-excursion
      (if-let ((text (org-get-entry))
               (heading (denote-link-ol-get-heading)))
          (progn
            (delete-region (org-entry-beginning-position)
                           (save-excursion (org-end-of-subtree t) (point)))
            (denote heading (denote-keywords-prompt) 'org)
            (insert text)))))

  ;; Backlogged Project는  작업 목록에 있으나 아직 시작되지 않은 프로젝트입니다.
  ;; Closed Project는 완료되어 더 이상 작업이 필요 없는 프로젝트입니다.  즉, 상태의 차이입니다.
  (defvar my/denote-keywords
    '(("aprj" . "Active Project")
      ("bprj" . "Backlogged Project")
      ("cprj" . "Closed Project")))

  (defun my/denote-custom-affixation (completions)
    (mapcar (lambda (completion)
              (list completion
                    ""
                    (alist-get completion
                               my/denote-keywords
                               nil
                               nil
                               #'string=)))
            completions))

  (defun my/denote-keyword-prompt ()
    (let ((completion-extra-properties
           (list :affixation-function
                 #'my/denote-custom-affixation)))
      (denote-keywords-prompt)))
  )

;;; my/denote-assign-evergreen : blog - Essays

;; All Growth Stages : Seedling, Budding, Evergreen
(progn
  ;; List of keywords to use for implementing the evergreen method with Denote.
  (setq my/denote-evergreen-keywords '("seedling" "budding" "evergreen"))

  (defun my/denote-assign-evergreen ()
    "Move your note to either Seedling, Budding, Evergreen."
    (interactive)
    (if-let* ((file (buffer-file-name))
              ((denote-filename-is-note-p file))
              (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
              (keywords (seq-remove (lambda (keyword)
                                      (member keyword my/denote-evergreen-keywords))
                                    all-keywords))
              (evergreen (completing-read "Select category: " my/denote-evergreen-keywords))
              (new-keywords (push evergreen keywords)))
        (denote-rename-file
         file
         (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
         new-keywords
         (denote-retrieve-filename-signature file))
      (message "Current buffer is not a Denote file.")))
  )

;;; my/denote-assign-zettel

(progn
  ;; List of keywords to use for implementing the zettelkasten method with Denote.
  (setq my/denote-zettel-keywords '("fleeting" "literature" "permanents" "zettels"))

  (defun my/denote-assign-zettel ()
    "Move your note to either fleeting, literature, permanents, zettels."
    (interactive)
    (if-let* ((file (buffer-file-name))
              ((denote-filename-is-note-p file))
              (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
              (keywords (seq-remove (lambda (keyword)
                                      (member keyword my/denote-zettel-keywords))
                                    all-keywords))
              (zettel (completing-read "Select category: " my/denote-zettel-keywords))
              (new-keywords (push zettel keywords)))
        (denote-rename-file
         file
         (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
         new-keywords
         (denote-retrieve-filename-signature file))
      (message "Current buffer is not a Denote file.")))
  )

;;; DONT denote completion

;; (progn
;;   ;; (( -> <tab> for completion
;;   ;; (defun my/expand-and-complete-with-denote ()
;;   ;;   (interactive)
;;   ;; (let ((limit (- (point) 2)))
;;   ;;   (when (looking-back "((" limit)
;;   ;;     (progn
;;   ;;       (call-interactively #'denote-link-or-create)
;;   ;;       (let ((end-of-link (point)))
;;   ;;         (goto-char limit)
;;   ;;         (delete-char 2)
;;   ;;         (goto-char end-of-link)))))
;;   ;; )
;;   ;; (define-key org-mode-map (kbd "<tab>") #'my/expand-and-complete-with-denote)

;;   (defun my/denote-try-to-complete-then-cycle (&optional arg)
;;     (interactive)
;;     (let ((limit (- (point) 2)))
;;       (if (looking-back "((" limit)
;;           (progn
;;             (call-interactively #'denote-link-or-create)
;;             (let ((end-of-link (point)))
;;               (goto-char limit)
;;               (delete-char 2)
;;               (goto-char end-of-link)))
;;         (org-cycle arg))))
;;   (evil-define-key '(insert) org-mode-map (kbd "<tab>") 'my/denote-try-to-complete-then-cycle))

;;; DONT denote completion capf on buffer

;; https://takeonrules.com/2023/05/07/completion-at-point-function-capf-for-org-mode-links/
;; super capf

;; (progn
;;   (defun jf/org-links-with-text (&optional given-link)
;;     "Return the `distinct-' `org-mode' links in the
;;  `current-buffer'.

;; Each element of the list will be a `propertize' string where the
;; string value is the text of the link and the \"link\" property
;; will be the :raw-link.

;; When provided a GIVEN-LINK stop processing when we encounter the
;; first matching link."

;;     (let ((links
;; 	   (org-element-map
;; 	       (org-element-parse-buffer)
;; 	       'link
;; 	     (lambda (link)
;; 	       (when-let* ((left (org-element-property :contents-begin link))
;; 			   (right (org-element-property :contents-end link)))
;; 	         (let ((returning
;; 		        (propertize
;; 		         (buffer-substring-no-properties left right)
;; 		         'link (org-element-property :raw-link link))))
;; 		   (if given-link
;; 		       (when (string= given-link returning)
;; 		         returning)
;; 		     returning))))
;; 	     nil
;; 	     given-link)))
;;       ;; Ensure that we have a distinct list.
;;       (if (listp links)
;; 	  (-distinct links)
;;         (list links))))

;;   ;; Cribbed from `org-roam' org-roam-complete-link-at-point
;;   (defun jf/org-capf-links ()
;;     "Complete links."
;;     (when (and (thing-at-point 'symbol)
;;                (not (org-in-src-block-p))
;;                (not (save-match-data (org-in-regexp org-link-any-re))))
;;       ;; We want the symbol so that links such performing completion on
;;       ;; "org-mode" will look for links with the text of org-mode and
;;       ;; then replace the text "org-mode" with the returned link.
;;       (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;         (list (car bounds) (cdr bounds)
;;               ;; Call without parameters, getting a links (filtered by CAPF
;;               ;; magic)
;;               (jf/org-links-with-text)
;;               :exit-function
;;               (lambda (text _status)
;;                 ;; We want the properties of that link.  In the case of one
;;                 ;; match, the provided text will have the 'link property.
;;                 ;; However if the
;;                 (let ((link (car (jf/org-links-with-text text))))
;;                   (delete-char (- (length text)))
;;                   (insert "[[" (get-text-property 0 'link link) "]"
;;                           "[" text "]]")))
;;               ;; Proceed with the next completion function if the returned
;;               ;; titles do not match. This allows the default Org capfs or
;;               ;; custom capfs of lower priority to run.
;;               :exclusive 'no))))

;;   ;; if you want multiple completion backends, use cape
;;   ;; (https://github.com/minad/cape):

;;   ;; (defun jf/org-capf-links-cape-tempel ()
;;   ;;   "The `completion-at-point-functions' I envision using for `org-mode'."
;;   ;;   (setq-local completion-at-point-functions
;;   ;;               (list (cape-capf-super
;;   ;;                      #'jf/org-capf-links
;;   ;;                      #'tempel-expand
;;   ;;                      #'cape-dict
;;   ;;                      ;; #'cape-file
;;   ;;                      ))))
;;   ;; (add-hook 'org-mode-hook #'jf/org-capf-links-cape-tempel)
;;   )

;;; DONT denote sort : Luhmann-style signatures

;; check denote-sequence
;; https://protesilaos.com/codelog/2024-08-01-emacs-denote-luhmann-signature-sort/
;; Luhmann-style signatures will be slightly different in their appearance: 1=1, 1=1a, 1=2, 1=2a, 1=2b.
;;  A sorted and filtered Dired listing (per denote-sort-dired) https://protesilaos.com/assets/images/attachments/2024-08-01-denote-luhmann-signatures-sorted.png

;; (progn
;;   (defun my-denote--split-luhman-sig (signature)
;;     "Split numbers and letters in Luhmann-style SIGNATURE string."
;;     (replace-regexp-in-string
;;      "\\([a-zA-Z]+?\\)\\([0-9]\\)" "\\1=\\2"
;;      (replace-regexp-in-string
;;       "\\([0-9]+?\\)\\([a-zA-Z]\\)" "\\1=\\2"
;;       signature)))

;;   (defun my-denote--pad-sig (signature)
;;     "Create a new signature with padded spaces for all components"
;;     (combine-and-quote-strings
;;      (mapcar
;;       (lambda (x)
;;         (string-pad x 5 32 t))
;;       (split-string (my-denote--split-luhman-sig signature) "=" t))
;;      "="))

;;   (defun my-denote-sort-for-signatures (sig1 sig2)
;;     "Return non-nil if SIG1 is smaller that SIG2.
;; Perform the comparison with `string<'."
;;     (string< (my-denote--pad-sig sig1) (my-denote--pad-sig sig2)))

;;   ;; Change the sorting function only when we sort by signature.
;;   (setq denote-sort-signature-comparison-function #'my-denote-sort-for-signatures) ; default - string-collate-lessp
;;   )

;;; DONT denote journal

;; Use a =journals/= subdirectory to integrate nicely with Logseq

;; (require 'denote-journal-extras)
;; (fmakunbound 'denote-journal-extras-new-entry) ;; delete

;; (setq denote-journal-extras-title-format "%Y-%m-%d")

;; Denote templates
;; (add-to-list 'denote-templates '(journal . "* Thoughts\n\n* Tasks (require a category)\n" ))

;; Here's a function for daily journal capture - compatible-ish with Logseq
;; (defun gjg/denote-journal ()
;;   "Create an entry tagged 'journal' with the date as its title.
;;       If an entry for today is already present, visit that file.
;;       If there are more than one files that match, present a choice."
;;   ;; (directory-files (concat denote-directory "/journals") nil "^[0-9T]+--2022-10-27.+org$")
;;   (interactive)
;;   (let* ((dt (decode-time))
;;          (today (format "%d-%02d-%02d" (nth 5 dt) (nth 4 dt) (nth 3 dt)))
;;          (jfileregex (concat "^[0-9T]+--" today ".+org$"))
;;          (jfiles (directory-files (concat denote-directory "journal") t jfileregex)))
;;     (cond
;;      ((= 1 (length jfiles))
;;       (find-file (car jfiles)))
;;      ((> (length jfiles) 1)
;;       (find-file (completing-read "Journal file (more than 1 found for today: " jfiles)))
;;      (t
;;       (denote
;;        (format-time-string "%Y-%m-%d")
;;        '("journal") ; multiple keywords are a list of strings: '("one" "two")
;;        nil          ; default Org file type
;;        (concat denote-directory "journal")
;;        nil          ; date - default to current-time
;;        'journal
;;        )))
;;     (save-buffer) ;; gotta save immediately to avoid creating duplicate dailies
;;     ))

;;; DONT org-id-link-to-org-use-id nil

;; (setq org-id-link-to-org-use-id 'use-existing) ; default nil
;; (setq org-hide-emphasis-markers t)

;;; provide

(provide 'denote-config)

;;; end-of-file
