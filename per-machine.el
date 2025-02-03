;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghanacs
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

;;; User Profile

(defconst user-emacs-version "30.0.93")

;; (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))

;; 나의 공개키는 다음 에서 확인 할수 있다.
;; https://meta.sr.ht/~junghanacs.keys, https://meta.sr.ht/~junghanacs.pgp

(setq user-full-name (if (getenv "USER_FULL_NAME")
                         (getenv "USER_FULL_NAME")
                       "John Doe"))

(setq user-mail-address (if (getenv "USER_MAIL_ADDRESS")
                            (getenv "USER_MAIL_ADDRESS")
                          "john.doe@example.com"))

;; Set my GPG key as the default key
(setq-default epa-file-encrypt-to (if (getenv "EPA_FILE_ENCRYPT_TO")
                                      (list (getenv "EPA_FILE_ENCRYPT_TO"))
                                    (list "ABCDEFGHIJKLMN")))

;; (setq user-mail-address "junghanacs@gmail.com")
;; (setq-default epa-file-encrypt-to '("B5ADD9F47612A9DB"))

;;; TODO Path

;; (if (spacemacs/system-is-mswindows)
;;     (setq
;;      org-agenda-dir "d:/org-notes"
;;      deft-dir "d:/org-notes"
;;      blog-admin-dir "d:/zilongshanren.com")
;;   (setq
;;    org-agenda-dir "~/org-notes"
;;    deft-dir "~/org-notes"
;;    blog-admin-dir "~/zilongshanren.com"
;;    org-roam-directory "~/org-notes/org-roam"))

;;;; Directory Path

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))


(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/git/"))

;; org-hugo-base-dir
(defconst user-hugo-blog-dir (concat user-project-directory "blog/"))
(defconst user-hugo-notes-dir (concat user-project-directory "notes/"))
(defconst user-hugo-notes-static-dir (concat user-hugo-notes-dir "quartz/")) ;; org-hugo-base-dir

;;;; directories

;; 좋은 방법이다. 이렇게 정의하면 좋다.
;; /home/junghan/sync/man/dotsamples/spacemacs/rogue-layer/.spacemacs
;; (defconst user-layer-dir (file-name-as-directory "~/.emacs.d/private/rogue"))
;; (defconst user-secrets-dir (file-name-as-directory (concat user-layer-dir "secrets")))

;; (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))
;; (defconst user-cloud-dir (file-name-as-directory (getenv "CLOUD_DIR")))
;; (defconst user-project-dir (file-name-as-directory (getenv "PROJECTS_DIR")))

;; ;; Task manager directory
;; (defconst user-tasks-dir (file-name-as-directory (concat user-cloud-dir "tasks")))

;; ;; Derived directories
;; (defconst user-notes-dir (file-name-as-directory (concat user-cloud-dir "notes")))
;; (defconst user-journal-dir (file-name-as-directory (concat user-notes-dir "journal")))
;; (defconst user-pdfs-dir (file-name-as-directory (concat user-notes-dir "pdfs")))

;; ;; Files
;; (defconst user-bib-file (concat user-notes-dir "library.bib"))
;; (defconst user-bib-notes-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/notes/documents.org"))
;; (defconst user-books-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/reading-list.org"))
;; (defconst user-clippings-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/clippings.org"))

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

(defun my/expand-org-file-name (filename)
  (expand-file-name filename org-directory))

(defconst user-inbox-file "20230202T020200--inbox-now__aprj.org")
(defun my/org-inbox-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-tasks-file () (my/expand-org-file-name user-inbox-file))
;; (defun my/org-now-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-index-file () (my/expand-org-file-name "20240429T165725--index.org"))
(defun my/org-about-file () (my/expand-org-file-name "20240326T053829--about.org"))
(defun my/org-contacts-file () (my/expand-org-file-name "20230303T030300--contacts.org"))
(defun my/org-links-file () (my/expand-org-file-name "20230219T035500--links.org"))

(defun my/org-mobile-file () (my/expand-org-file-name "agenda/20240312T111900--mobile.org")) ;; agenda
(defun my/org-quote-file () (my/expand-org-file-name "agenda/20240312T031200--quote.org"))

(defun my/org-diary-file () (my/expand-org-file-name "20220101T010100--diary.org"))
(defun my/org-drill-file () (my/expand-org-file-name "20240124T164402--drill.org"))
(defun my/org-life-file () (my/expand-org-file-name "20240327T112315--life.org"))
(defun my/org-elfeed-file () (my/expand-org-file-name "20220706T160000--elfeed.org"))

(defun my/org-reading-file () (my/expand-org-file-name "20240329T154123--reading__lists.org"))

;; meta
(defun my/org-kdc-file () (my/expand-org-file-name "meta/20240312T142358--한국십진분류__classification_kdc_meta.org"))
(defun my/org-tags-file () (my/expand-org-file-name "meta/20231005T133900--tags__meta.org"))
(defun my/org-glossary-file () (my/expand-org-file-name "dict/20240913T145640--general__glossary.txt"))

;; blog
(defun my/org-blog-file () (my/expand-org-file-name "posts/20240104T061355--blog__aprj_posts_schedule.org"))

;; talks
(defun my/org-talks-file () (my/expand-org-file-name "talks/20240827T150414--talks.org"))

(defun my/org-remark-file () (my/expand-org-file-name "20231111T094444--remark.org"))
(defun my/org-remember-file () (my/expand-org-file-name "20231020T210500--remember.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))
(defun my/org-screenshot-directory () (my/expand-org-file-name "screenshot"))

(defvar org-user-agenda-files (list user-org-directory))
(defvar org-screenshot-path (concat user-org-directory "screenshot/"))

;; bib
(setq citar-notes-paths (list (concat user-org-directory "bib/")))
;; (defvar config-bibfiles (list (concat user-org-directory "bib/zotero-biblatex.bib")))
(defvar config-bibfiles (list
                         (concat user-org-directory "resources/Slipbox.bib")
                         (concat user-org-directory "resources/Book.bib")
                         (concat user-org-directory "resources/Category.bib")
                         ;; (concat user-org-directory "resources/zotero-group-junghanacs.bib")
                         ))

;; elisp-demos
(setq elisp-demos-user-files (list (concat org-directory
                                           "/notes/20240926T170706--elisp-demos__emacslisp_notes.org")))

;; My agenda files. keep it simple
;; (defvar org-user-agenda-files (list
;;                                (my/org-inbox-file)
;;                                (my/org-tasks-file)
;;                                (my/org-diary-file)
;;                                (my/org-life-file)

;;                                ;; (my/org-reading-file)
;;                                ;; (my/org-blog-file) ;; blog
;;                                ;; (my/org-talks-file) ;; talks

;;                                ;; (my/org-contacts-file)
;;                                ;; (my/org-drill-file)
;;                                ;; (my/org-quote-file)
;;                                ;; (my/org-mobile-file)
;;                                ;; (my/org-links-file)

;;                                ;; (my/org-kdc-file)
;;                                ;; (my/org-tags-file)
;;                                ;; (my/org-glossary-file)

;;                                ;; (my/org-remember-file)
;;                                ;; ;; (my/org-remark-file)
;;                                ))

(defvar org-user-contacts-files (list (my/org-contacts-file)))

(defvar +org-journal-today-file nil)

;;; emacs-type

(defcustom emacs-type 'doomemacs
  "Select Emacs Distribution Types"
  :group 'emacs
  :type  '(choice
           (const :tag "spacemacs" spacemacs)
           (const :tag "spacemacs" craftedemacs)
           (const :tag "doomemacs" doomemacs)
           (const :tag "vanillaemacs" vanillaemacs)))

(defun is-spacemacs() (eq emacs-type 'spacemacs))
(defun is-doomemacs() (eq emacs-type 'doomemacs))
(defun is-craftedemacs() (eq emacs-type 'craftedemacs))
(defun is-vanillaemacs() (eq emacs-type 'vanillaemacs))

;; (when (is-spacemacs) (message "I Love Spacemacs"))

;;; fortune

(setq user-initial-scratch-message
      (format "%s"
              (if (executable-find "fortune")
                  (string-join
                   (mapcar
                    (lambda (l) (concat "\n " (string-fill l 72)))
                    (if (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
                        (string-lines (shell-command-to-string "fortune"))
                      (string-lines
                       (shell-command-to-string
                        "fortune -c 90% advice 10% .")))))
                ("\nLearn how to take a 20-minute power nap without embarrassment.
- 부끄러워 하지 않고 20분동안 Power Nap(에너지를 보충하는 짧은 낮잠) 자는 법을 익히세요.\n"))
              "\n"))

;;; ten with etags

;; M-x ten-tags-create, ten-update-all
(setq ten-glossary-files-and-directories
      '( ;; "~/sync/emacs/git/default/ten/test/Glossary-philosophy.txt"
        "~/sync/org/dict/20240913T145640--general__glossary.txt"
        "~/sync/org/dict/20240913T150903--philosophy__glossary.txt"
        "~/sync/org/dict/20240913T150904--philosophy-all__glossary.txt"
        "~/sync/org/dict/20241109T120829--physics__glossary.txt"
        "~/sync/org/dict/20241109T120830--physics-all__glossary.txt"
        "~/sync/org/dict/20241109T123634--math__glossary.txt"
        "~/sync/org/dict/20241109T123635--math-all__glossary.txt"
        "~/sync/org/dict/20241110T051844--it-terms__glossary.txt"
        ;;,(concat org-directory "dict/20240913T145640--general__glossary.txt")
        ))
(setq ten-tags-file-default (concat user-org-directory "dict/ten-TAGS"))

;;; cc/url-bookmarks

(setq cc/url-bookmarks
      '(("Google" . "https://www.google.com")
        ("Emacs Home" . "https://www.gnu.org/software/emacs/")
        ("Github/junghan0611" . "https://github.com/junghan0611")
        ("Github/junghanacs" . "https://github.com/junghanacs")
        ))

;;; per-machine.el ends here
