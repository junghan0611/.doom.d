;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghanacs
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


;;; Variables

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

;;; Configuration
;;;; Path

(global-unset-key (kbd "<f10>"))

(setq org-crypt-key (car epa-file-encrypt-to))
(message "`org-directory' has been set to: %s" org-directory)

;; Adding a "/" so that =find-file= finds the files under =~/org/=.
;;
;; The org-directory is computed based on user-emacs-directory.
;; - ".emacs.d" -> "~/org/"
;; - ".emacs.d-personal" -> "~/org-personal/"
;;(concat "~/org" (nth 1 (split-string user-emacs-directory "emacs.d"))))
;; (setq org-directory
;;       (concat "~/org" (nth 1 (split-string dotspacemacs-directory
;;                                            "spacemacs.d"))))

;;;; My Style

(setq org-enforce-todo-dependencies t)
(setq org-cycle-separator-lines 0)

;; 리버스 순서가 익숙하다.
;; (setq org-reverse-note-order t) ; default nil

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)

(setq org-special-ctrl-a/e t) ; doom t
(setq org-special-ctrl-k nil) ; doom nil
(setq org-yank-adjusted-subtrees t)

;; 22/10/11--22:18 :: headline 설정 좋다.
;; (setq org-fontify-todo-headline nil) ; default nil
;; (setq org-fontify-done-headline nil) ; doom t
(setq org-fontify-whole-heading-line nil) ; doom t

(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      ;; (org-cycle)
      (bh/insert-inactive-timestamp))))

;; 2024-02-24 off
;; 2024-09-26 On
(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
;; (setq org-export-with-timestamps nil)

(setq org-return-follows-link t)

(setq org-tags-match-list-sublevels t)
(setq org-agenda-persistent-filter t)

(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id) ; default nil

;; Resume clocking task when emacs is restarted
;; (org-clock-persistence-insinuate)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
;; (setq org-clock-history-length 23) ; doom default 20
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t) ; doom default t

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t) ; doom default t
;; Clock out when moving task to a done state
;; (setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (setq org-clock-persist t) ; doom 'history
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; ex) 2022-09-19 (월)
(setq org-agenda-format-date "%Y-%m-%d (%a)")

(defun bh/make-org-scratch ()
  (interactive)
  (find-file (concat org-directory "/scratch.org"))
  ;; (gnus-make-directory "/tmp/publish")
  )

(defun bh/make-markdown-scratch ()
  (interactive)
  (find-file (concat org-directory "/md/scratch.md"))
  ;; (gnus-make-directory "/tmp/publish")
  )

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun bh/switch-to-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda(n)*"))

;;;###autoload
(defun ash-goto-org-agenda (&optional _)
  (interactive)
  (let ((buf (get-buffer "*Org Agenda(n)*")))
    (if buf
        (progn (switch-to-buffer buf)
               (delete-other-windows))
      (progn
        ;; (tab-bar-new-tab)
        ;; (tab-bar-move-tab-to 2)
        (org-agenda nil "n")
        (org-agenda-goto-today)
        ;; (spacemacs/toggle-current-window-dedication) ; spacemacs Compatibility
        ))))

;;;; global map

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c i") 'org-insert-link)
(global-set-key (kbd "C-c A") 'ash-goto-org-agenda)

;; Sets default-directory to org-directory so that =M-x magit= from the agenda view does not ask me for a dir.
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c A")
;;                 (lambda () (interactive) (let ((default-directory org-directory)) (org-agenda))))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c \\") 'org-tags-sparse-tree)

(global-unset-key (kbd "<f6>"))
(global-set-key (kbd "<C-f6>") #'(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") #'(lambda () (interactive) (bookmark-jump "SAVED")))

(global-set-key (kbd "<f10> c") 'calendar)

(global-set-key (kbd "<f10> O") 'bh/make-org-scratch)

(global-set-key (kbd "<f10> s") 'bh/switch-to-scratch)
(global-set-key (kbd "<f10> S") 'bh/make-markdown-scratch)

(global-set-key (kbd "<f10> i") 'org-clock-in)
(global-set-key (kbd "<f10> o") 'org-clock-out)

(global-set-key (kbd "<f10> M-i") 'bh/punch-in)
(global-set-key (kbd "<f10> M-o") 'bh/punch-out)

(global-set-key (kbd "<f10> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f10> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f10> v") 'visible-mode)
(global-set-key (kbd "<f10> l") 'org-toggle-link-display)
(global-set-key (kbd "C-<f10>") 'previous-buffer)
(global-set-key (kbd "M-<f10>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
;; (global-set-key (kbd "C-<f10>") 'next-buffer)

(global-set-key (kbd "<f10> 9") 'org-clock-goto)
(global-set-key (kbd "<f10> <f10>") 'org-clock-goto)

(global-set-key (kbd "<f10> h") 'outline-hide-other)
(global-set-key (kbd "<f10> k") 'my/get-id-to-clipboard)
(global-set-key (kbd "<f10> r") 'remember)

(define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

;; with markdown-mode-map
(define-key org-mode-map (kbd "<f10> g") 'org-toggle-inline-images)
(define-key org-mode-map (kbd "<f10> m") 'my/org-toggle-emphasis-markers)

(global-set-key (kbd "<f10> f") 'my/logos-focus-editing-toggle)

;; (global-set-key (kbd "<f10> p") 'org-pomodoro)

(global-set-key (kbd "<f10> I") 'org-clock-in-last)

;; ;;;###autoload
;; (defun my/org-clock-goto (&optional _)
;;   (interactive)
;;   (let ((buf (get-buffer "org-clock-in-now")))
;;     (tab-bar-switch-to-tab "time")
;;     (if buf
;;         (progn (switch-to-buffer buf)
;;                ;; (delete-other-windows)
;;                )
;;       (org-clock-goto)
;;       (rename-buffer "org-clock-in-now")
;;       ))

;;;; shift

;; Shift 거슬리는 것을 막아주는 아주 요긴한 설정이다.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-support-shift-select nil) ; default nil
(setq shift-select-mode nil) ; default t

;;;; imenu ellipsis bookmark

(setq org-capture-bookmark nil)
(setq org-imenu-depth 4) ; doom 6, default 2

;; Search on https://www.compart.com/en/unicode/U+25BF
;; Unicode Character “◉” (U+25C9)
;; 0x00262F ☯ YIN YANG
;; 0x0029BE ⦼ CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN
;; § ◉ ⚡, ▼, ↴, † ‡ № ¶
;; (when (display-graphic-p) ; gui
;;   (setq org-ellipsis " ◉"))

;;;; pretty-entities / bullet lists / image-width

(setq org-image-actual-width t)
(setq org-image-max-width (min (/ (display-pixel-width) 3) 640))

;; Org styling, hide markup etc. 테스트
;; 왜 minemacs 는 org-pretty 설정을 둘다 t 로 했을까?  org-pretty-entities 가
;; 설정되면 abc_def 에서 def 가 아래로 기어 들어간다.
;; 2023-10-13: I prefer using M-x org-toggle-pretty-entities instead.
(setq org-pretty-entities nil) ; very important
;; orgmode 익스포트 할 때, underscore 가 subscripts 변환 방지
;; http://ohyecloudy.com/emacsian/2019/01/12/org-export-with-sub-superscripts/
(setq org-pretty-entities-include-sub-superscripts nil)

;; Use utf-8 bullets for bullet lists -- this isn't great, but a bit
;; nicer than nothing. Ideally should use monospace font for spaces
;; before bullet item, and use different bullets by list level.
;; 2024-05-02 replaced by org-modern
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([+]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;;;; DONT prettify-symbols with logic

;; https://en.wikipedia.org/wiki/Glossary_of_mathematical_symbols
;; 2024-03-07 기호를 활용하면 관계를 표현할 수 있다.
;; M-x list-unicode-display
(add-hook 'org-mode-hook (lambda ()
                           (push '("--" . "—") prettify-symbols-alist)
                           (push '("->" . "→" ) prettify-symbols-alist)
                           (push '("<-" . "←" ) prettify-symbols-alist)
                           (push '("<->" . "↔" ) prettify-symbols-alist)
                           ;; (push '("<->" . "" ) prettify-symbols-alist) ; Action
                           (push '("=>" . "⇒") prettify-symbols-alist) ; if 조건 ⇒ ⟹
                           (push '("<=>" . "⟺") prettify-symbols-alist) ; 명제 논리 ; 동치
                           ;; (push '(":not" . "!") prettify-symbols-alist); ¬
                           ;; (push '(":and" . "∧") prettify-symbols-alist) ; 논리곱
                           ;; (push '(":or" . "∨") prettify-symbols-alist) ; 논리합
                           ;; (push '(":xor" . "⊕") prettify-symbols-alist) ; 베타적 논리합
                           ;; (push '(":all" . "∀") prettify-symbols-alist)
                           ;; (push '(":exist" . "∃") prettify-symbols-alist) ; 존재 실존
                           ;; (push '(":vs" . "🆚") prettify-symbols-alist)
                           ;; (push '(":ref" . "※") prettify-symbols-alist)
                           ;; (push '(":prove" . "⊢") prettify-symbols-alist)
                           ;; (push '(":entail" . "⊨") prettify-symbols-alist)
                           ;; (push '(":in" . "∈") prettify-symbols-alist)
                           ;; (push '(":notin" . "∉") prettify-symbols-alist)
                           ;; (push '(":union" . "∪") prettify-symbols-alist)
                           ;; (push '(":intersect" . "∩") prettify-symbols-alist)
                           ;; (push '(":star" . "★") prettify-symbols-alist)
                           (prettify-symbols-mode)))

;;;; visual-line-mode and DONT auto-fill

(add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'toggle-text-mode-auto-fill)

;;;; org-startup-folded

;; fold / overview  - collapse everything, show only level 1 headlines
;; content          - show only headlines
;; nofold / showall - expand all headlines except the ones with :archive:
;;                    tag and property drawers
;; showeverything   - same as above but without exceptions
;; #+startup: fold
;; (setq org-startup-folded 'fold) ; show2levels
(setq org-agenda-inhibit-startup t) ;; ~50x speedup, doom default t

;;;; org-export

(setq org-export-headline-levels 5) ; default 3
(setq org-export-with-toc nil) ; default t - turn off on hugo toc

(progn
  (setq org-publish-use-timestamps-flag t) ; default t
  (setq org-export-with-section-numbers t) ; default t
  (setq org-export-with-timestamps t) ; default t
  ;; (setq org-export-with-drawers '(not "LOGBOOK")) ; default (not "LOGBOOK")
  (setq org-export-with-todo-keywords t) ; default t
  )

;; ;; (setq org-export-preserve-breaks t) ; default nil
(setq org-export-with-broken-links 'mark) ; Do not rise error on broken links, but mark them in the output file - default nil
(setq org-export-date-timestamp-format "%e %B %Y")

(setq org-export-use-babel nil) ; default t  - 2024-10-11
(setq org-export-with-tags 'not-in-toc)

;; 2025-01-24 이게 뭐야?
;; (setq org-cycle-hide-block-startup t); doom nil
;; (setq org-edit-src-auto-save-idle-delay auto-save-timeout) ; use the defaults
;; (setq org-edit-src-turn-on-auto-save t) ; auto-save org-edit-src , doom nil
;; (setq org-export-in-background t) ; run export processes in external emacs process

(setq org-export-with-smart-quotes nil) ; convert "this" to « this » ; doom t

;; (setq org-export-with-sub-superscripts '{}) ; Only explicit _{} ^{} are interpreted as sub/superscripts
;; (setq org-fold-catch-invisible-edits 'smart) ; try not to accidentally do weird stuff in invisible regions - doom 'smart

;;;; DONT 2024-12-28 from zk : smartquotes

;; /vanilla/localauthor-dotfiles-zk/my-lisp/gr-org-extras.el
;; (add-to-list
;;  'org-export-smart-quotes-alist
;;  '("en-us" (primary-opening :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo "``")
;;    (primary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo "''")
;;    (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
;;    (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
;;    (apostrophe :utf-8 "’" :html "&rsquo;")))

;;;; org-num

(require 'org-num)
(setq org-num-skip-unnumbered t
      org-num-skip-commented t
      org-num-skip-footnotes t
      org-num-max-level 3)

;; org-num-skip-tags '("nonum" "noexport") ; doom default
;; (add-hook 'org-mode-hook #'org-num-mode)

;;;; DONT org-footnote-auto-adjust

;; org-footnote-auto-label ; doom t
;; (setq org-footnote-auto-adjust t) ;; renumber footnotes - default nil

;;;; org-block and hide leading stars

;; Hide ~*~, ~~~ and ~/~ in org text.
;; Org styling, hide markup etc. = / ~
(setq org-hide-emphasis-markers nil)
(setq org-hide-block-startup nil)
(setq org-hide-macro-markers nil)

(setq org-indent-mode-turns-on-hiding-stars nil) ; default t -- MINE
(setq org-indent-mode-turns-off-org-adapt-indentation t) ; must t, default t

;; org-indent-mode 사용하면 org-hide-leading-stars 자동 on
;; (setq org-hide-leading-stars nil) ; doom t

;;;; DONT custom indentation

;; (setq org-adapt-indentation t)
(setq org-startup-indented t) ; doom t, spacemacs nil
(setq org-src-preserve-indentation t) ; doom t, spacemacs nil
(setq org-edit-src-content-indentation 0) ; default 2

;; Reduce org-indent-indentation-per-level from 2 to 1.
;; This keeps =org-tags-column= the same for all headings.
;; Avoid inconsistency when eidting outside Emacs, like Orgzly and Beorg.
;; (setq org-indent-indentation-per-level 1) ; 2024-06-19 enable, 2024-06-27 turn-off

;;;; defer-font-lock

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000) ; 50kb
    (setq-local jit-lock-defer-time 0.1 ;; 0.2
                jit-lock-stealth-time 1)))
(add-hook 'org-mode-hook #'locally-defer-font-lock)

;;;; org-blank-before-new-entry : heading and plain-list

;; 순서 없는 목록(unordered list)에서 bullet으로 들여쓰기를 할 때마다 +, -를 번갈아 사용한다
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))

(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . nil)))

;; /ohyecloudy-dot-doom/doom.d/config.org

;; =M-RET= 키로 라인을 분리할 수 있게 한다. org module에서 nil 값을 바인딩한 걸 디폴트 값으로 돌림.
(setq org-M-RET-may-split-line '((default . t))) ; doom nil

;;;; org-table

;; /vedang-dotfiles-clj-agenda/org-mode-crate/org-mode-crate.el:942:
(setq org-table-export-default-format "orgtbl-to-csv")

;;;; org-columns

;; vedang's style from org-mode-crate
(setq org-columns-default-format
      "%50ITEM(Task) %5Effort(Effort){:} %5CLOCKSUM %3PRIORITY %20DEADLINE %20SCHEDULED %20TIMESTAMP %TODO %CATEGORY(Category) %TAGS")

;;; org-agenda

(message "Press `C-c a' to get started with your agenda...")

;;;; org-todo-keywords : whhone

(progn
  ;; https://whhone.com/emacs-config/
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "DONT(o)")))

  (with-no-warnings
    (custom-declare-face '+org-todo-todo  '((t (:inherit (bold error org-todo)))) "")
    (custom-declare-face '+org-todo-next  '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")
    (custom-declare-face '+org-todo-done  '((t (:inherit (bold success org-todo)))) "")
    (custom-declare-face '+org-todo-dont '((t (:inherit (bold warning org-todo)))) "")
    )

  (setq org-todo-keyword-faces
        '(("TODO" . +org-todo-todo) ;; red
          ("DONE" . +org-todo-done) ;; green
          ("NEXT" . +org-todo-next) ;; blue
          ("DONT" . +org-todo-dont) ;; yellow
          ))

  ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
  (setq org-agenda-custom-commands
        '(("n" "Agenda / NEXT"
           ((agenda "" nil)
            (tags "INBOX+LEVEL=2|CATEGORY=\"Inbox\"+LEVEL=1")
            (todo "NEXT" nil)
            ;; (todo "TODO" nil) ;; 2024-03-18 add
            ) nil)
          (" " "Agenda and all TODOs" ; default' view
           ((agenda "")
            (alltodo "")))))
  )

;;;; DONT custom agenda files

;; ;; (setq org-agenda-files org-user-agenda-files)

(setq org-agenda-diary-file (my/org-diary-file))
(setq org-default-notes-file (my/org-inbox-file))

;; doom-emacs capture files : absolute path
;; (setq +org-capture-todo-file (my/org-inbox-file))
;; (setq +org-capture-notes-file (my/org-inbox-file))
;; (setq +org-capture-changelog-file (my/org-inbox-file))
;; (setq +org-capture-projects-file (my/org-tasks-file))
;; (setq +org-capture-journal-file (my/org-diary-file))

;;;; org-agenda

;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
(setq org-agenda-sticky t) ; default nil

;; Shift the agenda to show the previous 3 days and the next 7 days for
;; better context on your week. The past is less important than the future.
(setq org-agenda-span 'day) ; default 'week, doom 10

;; Hide all scheduled todo.
(setq org-agenda-todo-ignore-scheduled 'all)

;; Ignores "far" deadline TODO items from TODO list.
(setq org-agenda-todo-ignore-deadlines 'far)

;; Hide all scheduled todo, from tags search view, like tags-todo.
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Hide all done todo in agenda
(setq org-agenda-skip-scheduled-if-done t)

;; Hide task until the scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

(setq org-log-into-drawer t)

(setq org-log-done 'time)

;; (setcdr (assoc 'note org-log-note-headings) "%d")
;; Interstitial Journaling: add note to CLOCK entry after clocking out
;; https://emacs.stackexchange.com/questions/37526/add-note-to-clock-entry-after-clocking-out
;; (setq org-log-note-clock-out t)

;; 4 priorities to model Eisenhower's matrix.
;; - [#A] means +important +urgent
;; - [#B] means +important -urgent
;; - [#C] means -important +urgent
;; - [#D] means -important -urgent
(setq org-priority-default 68
      org-priority-lowest 68)

;;;; diary-file

(setq diary-file (concat user-dotemacs-dir "diary"))
(setq org-agenda-include-diary t)

;;;; org-agenda-log-mode and clock-mode

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-with-log-mode t)

;; Agenda log mode items to display (closed clock : default)
;; 이전 이맥스는 state 가 기본이었다. 지금은 시간 기준으로 표기한다.
;; closed    Show entries that have been closed on that day.
;; clock     Show entries that have received clocked time on that day.
;; state     Show all logged state changes.
;; (setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-log-mode-add-notes nil)

;; sort 관련 기능을 확인해보고 정의한 함수들이 필요 없으면 빼면 된다.
(setq org-agenda-sort-notime-is-late t) ; Org 9.4
(setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;; Time Clocking
(setq org-clock-idle-time 30) ; 10
;; (setq org-clock-reminder-timer (run-with-timer
;;                                 t (* org-clock-idle-time 20) ; 60
;;                                 (lambda ()
;;                                   ;; (fboundp 'org-clocking-p)
;;                                   (unless (org-clocking-p)
;;                                     (when (fboundp 'alert)
;;                                       (alert "Do you forget to clock-in?"
;;                                              :title "Org Clock"))))))
;; (org-clock-auto-clockout-insinuate) ; auto-clockout
;; modeline 에 보이는 org clock 정보가 너무 길어서 줄임
(setq org-clock-string-limit 30) ; default 0

;; org-clock-persist for share with machines
(setq org-clock-persist-query-save t)
(setq org-clock-persist-query-resume t)

;; current  Only the time in the current instance of the clock
;; today    All time clocked into this task today
;; repeat   All time clocked into this task since last repeat
;; all      All time ever recorded for this task
;; auto     Automatically, either all, or repeat for repeating tasks
(setq org-clock-mode-line-entry t)
(setq org-clock-mode-line-line-total 'auto) ; default nil

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties
      (quote
       (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
        ("STYLE_ALL" . "habit"))))

;; Include the file name into the path in refile target.
;; (setq org-refile-use-outline-path 'file) ; default nil
;; (setq org-outline-path-complete-in-steps nil) ; default t

;; (setq org-refile-targets
;;       `((nil :maxlevel . 2)
;;         (,(my/org-tasks-file) :maxlevel . 2)
;;         (,(my/org-links-file) :maxlevel . 2)))

;; Save Org buffers after refiling!
;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

;;;; org-tag and category

;; (setq org-auto-align-tags nil) ; default t, use doom's custom
;; (setq org-tags-column 0) ; default -77
(setq org-agenda-tags-column -80) ;; 'auto ; org-tags-column
(setq org-agenda-show-inherited-tags nil)

(setq org-tag-alist (quote (
                            ;; (:startgroup) ;; Location
                            ;; ("@errand" . ?E)
                            ;; ("@office" . ?O)
                            ;; ("@library" . ?L)
                            ;; ("@cafe" . ?C)
                            ;; ("@home" . ?H)
                            ;; (:endgroup)
                            ;; (:startgroup) ;; Category
                            ;; ("@personal" . ?P)
                            ;; ("@family" . ?F)
                            ;; (:endgroup) ;; Status
                            (:startgroup) ;; Action
                            ("later" . ?l)
                            ("now" . ?n)
                            ("waiting" . ?w)
                            ("hold" . ?h)
                            (:endgroup)
                            ("important" . ?i) ; 별도 처리
                            ;; ("crypt" . ?E)
                            ("note" . ?o)
                            ("noexport" . ?x)
                            ("fulltext" . ?f)
                            ("nonum" . ?u)
                            ("ATTACH" . ?a)
                            ;; ("latest" . ?t) ;; latest version
                            )))

(add-to-list 'org-tags-exclude-from-inheritance "projects") ; projects 왜 구분했었지?

;;;; org-agenda-custom-commands

;; ol-doi ol-w3m ol-bbdb ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail
;; ol-eww ol-bibtex
;; Adapted from http://stackoverflow.com/a/12751732/584121
;; (require 'org-protocol)
(setq org-protocol-default-template-key "L")
(setq org-modules `(
                    org-habit
                    org-protocol
                    ))

;; (setq org-agenda-prefix-format
;;       '((agenda  . " %i %-14:c%?-12t% s")
;;         (todo  . " %i %-14:c")
;;         (tags  . " %i %-14:c")
;;         (search . " %i %-14:c")))

;; https://www.pygopar.com/creating-new-columns-in-org-agenda
;; Originally from here: https://stackoverflow.com/a/59001859/2178312
(defun gopar/get-schedule-or-deadline-if-available ()
  (let ((scheduled (org-get-scheduled-time (point)))
        (deadline (org-get-deadline-time (point))))
    (if (not (or scheduled deadline))
        (format " ")
      ;; (format "🗓️ ")
      "   ")))

(setq org-agenda-prefix-format
      '((agenda . " %-4e %i %-12:c%?-12t% s ")
        (todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(when IS-TERMUX
  (setq org-agenda-prefix-format
        '((agenda  . " %i %?-12t% s")
          (todo  . " %i ")
          (tags  . " %i ")
          (search . " %i "))))

(setq org-agenda-category-icon-alist nil)

(setq org-agenda-hide-tags-regexp
      "agenda\\|DONT\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")

(add-hook 'org-agenda-finalize-hook
          (lambda ()
            ;; (setq-local line-spacing 0.2)
            (define-key org-agenda-mode-map [(double-mouse-1)] 'org-agenda-goto-mouse)))

(defun cc/org-agenda-goto-now ()
  "Redo agenda view and move point to current time '← now'"
  (interactive)
  (org-agenda-redo)
  (org-agenda-goto-today)

  (if window-system
      (search-forward "← now ─")
    (search-forward "now -"))
  )

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map (kbd "<f2>") 'org-save-all-org-buffers)
            (define-key org-agenda-mode-map (kbd "<backspace>") #'evil-switch-to-windows-last-buffer)
            (define-key org-agenda-mode-map (kbd "DEL") #'evil-switch-to-windows-last-buffer)
            ;; (define-key org-agenda-mode-map (kbd "M-p") 'org-pomodoro)
            ;; (define-key org-agenda-mode-map (kbd "M-P") 'ash/org-pomodoro-til-meeting)
            (define-key org-agenda-mode-map (kbd "M-.") 'cc/org-agenda-goto-now)))

(add-hook 'evil-org-agenda-mode-hook
          (lambda ()
            ;; (evil-define-key 'motion evil-org-agenda-mode-map "<backspace>" 'evil-switch-to-windows-last-buffer)
            ;; (evil-define-key 'motion evil-org-agenda-mode-map "<delete>" 'evil-switch-to-windows-last-buffer)
            ;; (evil-define-key 'motion evil-org-agenda-mode-map "DEL" 'evil-switch-to-windows-last-buffer)
            ;; (evil-define-key 'motion evil-org-agenda-mode-map "gt" 'tab-line-switch-to-next-tab) ; default doom's bindings
            ;; (evil-define-key 'motion evil-org-agenda-mode-map "gT" 'tab-line-switch-to-prev-tab)
            (evil-define-key 'motion evil-org-agenda-mode-map "F" 'org-agenda-follow-mode)))

;; (setq org-archive-location "archives/%s_archive::")
(setq org-archive-location (file-name-concat org-directory "archives/%s_archive::"))

;; nil 이면 C-c C-o 으로 접근한다.
;; (setq org-mouse-1-follows-link t) ; default 450

(setq org-capture-template-dir (concat user-dotemacs-dir "captures/"))
(setq org-datetree-add-timestamp t)

;;;; org-capture-templates : reset

(progn
  (setq org-capture-templates nil)

  ;; Inbox
  (add-to-list
   'org-capture-templates
   `("t" "Todo" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %?\n%i\n# %a"))

  (add-to-list
   'org-capture-templates
   `("T" "Todo ⏰" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* TODO [#C] %?\n%T\n# %a\n" :clock-in t :clock-resume t))

  (add-to-list
   'org-capture-templates
   `("s" "Todo SCHEDULED" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %? \nSCHEDULED: <%(org-read-date)>\n\n\n%i\n# %a")) ; %T

  (add-to-list
   'org-capture-templates
   `("d" "Todo DEADLINE" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %? \nDEADLINE: <%(org-read-date)>\n\n\n%i\n# %a")) ; %T

  ;; Tasks
  (add-to-list
   'org-capture-templates
   `("p" "Project /w template" entry (file+headline ,(my/org-tasks-file) "Projects")
     (file ,(concat org-capture-template-dir "project.capture"))))

  ;; Diary
  (add-to-list
   'org-capture-templates
   `("j" "j Diary" entry (file+olp+datetree ,(my/org-diary-file))
     "* %<%H:%M> - %?\n%T\n# %a\n")) ;; %a

  (add-to-list
   'org-capture-templates
   `("J" "J Diary ⏰" entry (file+olp+datetree ,(my/org-diary-file))
     "* %<%H:%M> - %?\n%U\n# %a\n\n" :clock-in t :clock-resume t)) ; :tree-type week

  ;; Drill
  (add-to-list
   'org-capture-templates
   `("v" "Vocab" entry (file+headline ,(my/org-drill-file) "Translation")
     "* %? :drill:\n\n** Translation\n\n** Definition\n"))

  ;; Drill
  (add-to-list
   'org-capture-templates
   `("l" "links" entry (file ,(my/org-links-file))
     "* TODO %(org-cliplink-capture)" :immediate-finish t))

  ;; Reading
  ;; (add-to-list
  ;;  'org-capture-templates
  ;;  `("R" "Reading new book" entry (file+olp ,(my/org-reading-file) "Books")
  ;;    (file ,(concat org-capture-template-dir "new-book.capture"))))
  )

;;;; link-abbrev

;; 추가
(add-to-list 'org-link-abbrev-alist
             '("wikidata"        . "https://www.wikidata.org/wiki/"))

;;;; org-eldoc-breadcrumb-separator

(setq org-eldoc-breadcrumb-separator " > ")


;;; external dependency

;;;; consult-org-agenda

;; (defun my/consult-org-agenda ()
;;   (interactive)
;;   (consult-org-agenda)
;;   (org-tree-to-indirect-buffer))

(setq my/consult-org-files '())
(add-to-list 'my/consult-org-files (my/org-inbox-file) t)
(add-to-list 'my/consult-org-files (my/org-tasks-file) t)
(add-to-list 'my/consult-org-files (my/org-links-file) t)
(add-to-list 'my/consult-org-files (my/org-contacts-file) t)
(add-to-list 'my/consult-org-files (my/org-mobile-file) t)
;; (add-to-list 'my/consult-org-files (my/org-diary-file) t)

(when (file-exists-p (my/org-blog-file))
  (add-to-list 'my/consult-org-files (my/org-blog-file) t))

(when (file-exists-p (my/org-reading-file))
  (add-to-list 'my/consult-org-files (my/org-reading-file) t))

;; (when (file-exists-p (my/org-emacs-config-file))
;;     (add-to-list 'my/consult-org-files (my/org-emacs-config-file) t))

(defun my/consult-org-all ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=2" ; 3
   my/consult-org-files))

(defun my/consult-org-contacts ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=2"
   (list (my/org-contacts-file))))

(defun my/consult-org-inbox ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-inbox-file))))

(defun my/consult-org-tasks ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-tasks-file))))

(defun my/consult-org-links ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-links-file))))

(defun my/consult-org-quote ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-quote-file))))

(defun my/consult-org-kdc ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-kdc-file))))

(defun my/consult-org-blog ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-blog-file))))

(defun my/consult-org-reading ()
  (interactive)
  (consult-org-heading
   "+LEVEL<=3"
   (list (my/org-reading-file))))

;;;; DONT org-pomodoro

;; (require 'org-pomodoro)

;; Clock break time in pomodoro
;; (setq org-pomodoro-clock-break t)

;; (setq org-pomodoro-manual-break t)
;; (setq org-pomodoro-format "⌛ %s")

;; (defun ash/org-pomodoro-til-meeting ()
;;   "Run a pomodoro until the next 30 minute boundary."
;;   (interactive)
;;   (let ((org-pomodoro-length (mod (- 30 (cadr (decode-time (current-time)))) 30)))
;;     (org-pomodoro)))

;; from gopar
;; (org-pomodoro-started . gopar/load-window-config-and-close-home-agenda)
;; (org-pomodoro-finished . gopar/save-window-config-and-show-home-agenda))
;; (defun gopar/home-pomodoro ()
;;     (interactive)
;;     (setq org-pomodoro-length 25
;;         org-pomodoro-short-break-length 5))

;; (defun gopar/work-pomodoro ()
;;     (interactive)
;;     (setq org-pomodoro-length 60
;;         org-pomodoro-short-break-length 20))

;; (defun gopar/save-window-config-and-show-home-agenda ()
;;     (interactive)
;;     (window-configuration-to-register ?`)
;;     (delete-other-windows)
;;     (org-save-all-org-buffers)
;;     (org-agenda nil "h"))

;; (defun gopar/load-window-config-and-close-home-agenda ()
;;     (interactive)
;;     (org-save-all-org-buffers)
;;     (jump-to-register ?`)))

;;;; org-clock-sound

(setq org-clock-sound (concat user-dotemacs-dir "assets/sounds/meditation_bell.wav"))

;; async
(defun my/play-meditation-bell()
  "Play meditation-bell"
  (interactive)
  (call-process-shell-command "~/.local/bin/play-meditation-bell.sh" nil 0))
;; (global-set-key (kbd "C-c j m") 'my/play-meditation-bell)
(add-hook 'org-clock-in-hook 'my/play-meditation-bell 'append)
;; (add-hook 'org-clock-out-hook 'my/play-meditation-bell 'append)
;; (add-hook 'org-clock-goto-hook 'my/play-meditation-bell 'append)
;; (add-hook 'org-clock-cancel-hook 'my/play-meditation-bell 'append)
;; (add-hook 'org-capture-mode-hook 'my/play-meditation-bell 'append)


;;;; org-journal/agenda

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (calendar-set-date-style 'iso)))

;; (defun my/org-journal-new-entry ()
;;   "Inserts header with inactive timestamp, hours and minutes.
;;      A custom journal helper function."
;;   (interactive)
;;   (org-insert-heading)
;;   (org-insert-time-stamp (current-time) t t))

;; Get a timestamp for tomorrow
(defun my/tomorrow ()
  (format-time-string "%Y-%m-%d" (time-add 86400 (current-time))))

;;; punch-in punch-out

;; A big thanks to Bernt Hansen for providing an awesome guide to
;; beginners so that we can harness the power of org-mode. Almost all of the
;; customization here, and my complete day-to-day workflow,
;; is based on his document about org-mode which can be
;; found here: http://doc.norang.ca/org-mode.html

;; List of TODO states to clock-in
;; "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
(setq vm/todo-list '("TODO" "NEXT"))

;; Change task state to NEXT when clocking in
(defun bh/clock-in-to-working (kw)
  "Switch task from TODO to NEXT when clocking in.
Skips capture tasks and tasks with subtasks"
  (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
             (member kw vm/todo-list))
    "NEXT")) ; doom STRT

(setq org-clock-in-switch-to-state 'bh/clock-in-to-working)

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(setq bh/keep-clock-running nil)

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode)
               (not (org-before-first-heading-p))
               (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

;; Ensuring sane defaults for `org-id'

;; (setq org-id-track-globally t
;;       org-id-link-to-org-use-id t)

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (when (boundp 'bh/organization-task-id)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16)))))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;;; keybindings

(progn
  (define-key org-mode-map (kbd "<f3>") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "<f4>") 'org-toggle-inline-images)

  ;; (define-key org-mode-map (kbd "S-<tab>") (lambda () (interactive) (org-cycle 'FOLDED)))
  ;; (define-key org-mode-map (kbd "S-TAB") (lambda () (interactive) (org-cycle 'FOLDED)))
  ;; (define-key org-mode-map (kbd "<backtab>") (lambda () (interactive) (org-cycle 'FOLDED)))
  ;; (define-key org-mode-map (kbd "S-<iso-lefttab>") (lambda () (interactive) (org-cycle 'FOLDED)))
  (define-key org-mode-map (kbd "C-M-<tab>") 'org-shifttab)

  (define-key org-mode-map (kbd "C-c 1") 'org-show-level-1)
  (define-key org-mode-map (kbd "C-c 2") 'org-show-level-2)
  (define-key org-mode-map (kbd "C-c 3") 'org-show-level-3)
  (define-key org-mode-map (kbd "C-c 4") 'org-show-level-4)

  (define-key org-mode-map (kbd "C-c H") 'org-insert-heading)
  (define-key org-mode-map (kbd "C-c S") 'org-insert-subheading)

  (define-key org-mode-map (kbd "C-c r") #'my/org-random-heading)
  (define-key org-mode-map (kbd "C-c L") #'my/org-store-link-id-optional)

  (evil-define-key '(normal visual) org-mode-map (kbd "C-n") 'org-next-visible-heading)
  (evil-define-key '(normal visual) org-mode-map (kbd "C-p") 'org-previous-visible-heading)

  ;; evil-collection
  (evil-define-key '(normal visual) org-mode-map (kbd "C-j") 'org-forward-heading-same-level)
  (evil-define-key '(normal visual) org-mode-map (kbd "C-k") 'org-backward-heading-same-level)

  (evil-define-key '(normal visual) org-mode-map (kbd "C-S-p") 'outline-up-heading)

  (evil-define-key '(normal visual) org-mode-map "zu" 'outline-up-heading)

  (evil-define-key '(insert) org-mode-map (kbd "C-n") 'next-line)
  (evil-define-key '(insert) org-mode-map (kbd "C-p") 'previous-line)

  (evil-define-key '(insert) text-mode-map (kbd "C-n") 'next-line)
  (evil-define-key '(insert) text-mode-map (kbd "C-p") 'previous-line)

  ;; (evil-define-key '(insert) org-mode-map (kbd "M-h") 'delete-backward-char)
  ;; (evil-define-key '(insert) org-mode-map (kbd "M-l") 'delete-forward-char)

  ;; ordered/unordered list 를 입력 할 때 편함.
  ;; 체크박스가 있는 경우 M-S-RET org-insert-todo-heading 을 활용.
  ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-M-<return>") 'org-insert-item)

  ;; 문단을 한 라인으로 합쳐 준다. 구글 번역기 돌릴 때 매우 유용.
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-M-q") 'my/unfill-paragraph-or-region)

  ;; 복사한 링크는 아래의 방법으로 넣는다. 깔끔해서 좋다.
  ;; org-cliplink 는 insert 니까 i 를 바인딩한다. org-insert-link 를 따른다.
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-c M-i") 'org-cliplink)
  ;; (define-key map (kbd "C-c M-i") 'org-cliplink)

  (evil-define-key '(insert) org-mode-map (kbd "C-u") 'undo-fu-only-undo)
  (evil-define-key '(insert) org-mode-map (kbd "C-r") 'undo-fu-only-redo)

  ;; flameshot 으로 스크린샷 한 뒤, 바로 붙여넣기
  ;; 22/10/04--15:18 :: flameshot 저장하면 자동으로 클립보드에
  ;; full-path 가 복사된다. imglink 스니펫을 부르고 경로를 복사한다.
  ;; 스크린샷 및 이미지를 관리하기에 이러한 방법이 더 좋다.
  ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-c M-y") 'org-download-clipboard)
  (define-key org-mode-map (kbd "C-c M-y") 'org-download-clipboard)

  ;; ;; search to narrow with heading and tag
  (define-key org-mode-map (kbd "C-c o") 'consult-org-heading) ;; GOOD

  (define-key prog-mode-map (kbd "C-M-y") 'evil-yank)

  (define-key org-mode-map (kbd "C-c y") 'org-cliplink)
  (define-key org-mode-map (kbd "C-c I") 'org-insert-link-dwim) ; org-insert-link

  ;; C-x x
  (define-key ctl-x-x-map "h" #'prot-org-id-headline) ; C-x x h
  (define-key ctl-x-x-map "H" #'prot-org-id-headlines)
  ;; (define-key ctl-x-x-map "e" #'prot-org-ox-html)
  (define-key org-mode-map (kbd "C-x x C") 'org-clone-subtree-with-time-shift)

  ;; Shortcuts to Interactive Functions
  (global-set-key (kbd "C-x n m") #'my/split-and-indirect-orgtree)
  (global-set-key (kbd "C-x n M") #'my/kill-and-unsplit-orgtree)
  )

;;; provide

(provide 'org-config)

;;; obsolate
;;;; DONT src-lang-modes

;; (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;;;; DONT projectile

;; Org-projectile stuff
;; (require 'org-projectile)
;; (setq org-projectile-projects-file
;;       "/your/path/to/an/org/file/for/storing/project/todos.org")
;; (push (org-projectile-project-todo-entry) org-capture-templates)
;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)

;;;; DONT denote capture

;; (with-eval-after-load 'denote
;;   (add-to-list 'org-capture-templates
;;                '("N" "New note (with Denote)" plain
;;                  (file denote-last-path)
;;                  #'denote-org-capture
;;                  :no-save t
;;                  :immediate-finish nil
;;                  :kill-buffer t
;;                  :jump-to-captured t)))

;;;; DONT capture-template : more templates

;; (add-to-list
;;  'org-capture-templates
;;  ("L" "Link" entry (file+olp ,(my/org-links-file) "Web Links")
;;   "* %a\n %?\n %i"))

;; (add-to-list
;;  'org-capture-templates
;;  `("L" "Links+" plain (file+function ,(my/org-links-file) my/org-capture-goto-link)
;;    "%i\n%U\n%T\n%a\n" :empty-lines 1 :immediate-finish t))

;; (push `("c" "org-protocol-capture" entry (file ,(my/org-links-file))
;;         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
;;       org-capture-templates)

;; (add-to-list
;;  'org-capture-templates
;;  `("C" "Code" entry (file+headline +org-capture-notes-file "Code")
;;    (file ,(concat org-capture-template-dir "code-snippet.capture"))))

;; (add-to-list
;;  'org-capture-templates
;;  `("B" "Blog post" entry (file+olp ,(my/org-blog-file) "blog")
;;    (file ,(concat org-capture-template-dir "blog-post.capture"))))

;; (add-to-list
;;  'org-capture-templates
;;  `("D" "Decision note" entry (file+headline +org-capture-notes-file "Decision")
;;    (file ,(concat org-capture-template-dir "decision.capture"))))

;; (add-to-list
;;  'org-capture-templates
;;  `("E" "RRR" entry (file "~/sync/org/rrr.org")
;;    (file ,(concat org-capture-template-dir "rrr.capture"))))

;;(setq ob-async-no-async-languages-alist '("gnuplot" "mermaid"))

;;;; DONT Daily Weekly Review

;; Capture some feedback for myself or a quick check-in, which I will into other
;; more refined notes later. 나 자신을 위한 피드백이나 간단한 점검 사항을 기록해
;; 두었다가 나중에 좀 더 세련된 노트로 정리할 수 있습니다.

;; (add-to-list
;;  'org-capture-templates
;;  `("0" "Daily Review" entry
;;    (file+olp+datetree +org-capture-journal-file)
;;    (file ,(concat org-capture-template-dir
;;                   "basb/dailyreview.org")) :tree-type week :clock-in t :clock-resume t))

;; (add-to-list
;;  'org-capture-templates
;;  `("9" "Weekly Review" entry
;;    (file+olp+datetree +org-capture-journal-file)
;;    (file ,(concat org-capture-template-dir
;;                   "basb/weeklyreview.org")) :tree-type week :clock-in t :clock-resume t))

;; (add-to-list
;;  'org-capture-templates
;;  `("S" "The Start of Day Planning Routine" entry
;;    (file+olp+datetree ,(my/org-diary-file)
;;                       (file (concat org-capture-template-dir "workday.start.org"))
;;                       :prepent t :clock-in t :clock-resume t :empty-lines 1)))
;; (add-to-list
;;  'org-capture-templates
;;  `("E" "The End of Day Reflection Routine" entry
;;    (file+olp+datetree ,(my/org-diary-file)
;;                       (file (concat org-capture-template-dir "workday.end.org"))
;;                       :prepend nil :clock-in t :clock-resume t :empty-lines 1)))

;;;; DONT edit-src-code

;; Disable editing source code in dedicated buffer
;; https://emacs.stackexchange.com/questions/73986/how-do-i-stop-org-babel-from-trying-to-edit-a-source-block-in-a-dedicated-buffer/73988#73988
;; (defun org-edit-src-code nil)

;;; end-of-file
