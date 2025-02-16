;;; +user-configs.el -*- lexical-binding: t; -*-

(message "load +user-configs.el")

;;; Doom Common Configuration

;;;; GENERAL SETTINGS

;; /doom/high-school-macos-emacs-dev-env/doom/init.el
(setq-default x-stretch-cursor t) ; make the cursor wide over tabs, etc.
(setq undo-limit 80000000) ; Raise undo-limit to 80Mb
(setq truncate-string-ellipsis "…") ; Unicode ellispis are nicer than "...", and also save /precious/ space

;;;; startup and dashboard

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my notes). I can
;; do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode)

;; Set initial buffer to org
(setq initial-major-mode #'text-mode)

;;;; gc-cons : gcmh

;; (setq gcmh-idle-delay 5) ; doom 'auto
;; (setq gcmh-high-cons-threshold (* 100 1024 1024)) ; doom 16m
;; (setq gc-cons-threshold gcmh-high-cons-threshold)
;; (setq garbage-collection-messages t)

;;;; Leader key

;; Over-ride or add to Doom Emacs default key bindings
;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; 'M-m', '\,' 'SPC m' for localleader
(setq
 doom-localleader-key ","
 doom-localleader-alt-key "C-,") ; emacs insert mode

;; persp-mode and projectile in different prefixes
;; (setq! persp-keymap-prefix (kbd "C-c w"))
;; (after! projectile
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(defun my/call-localleader ()
  (interactive)
  (setq unread-command-events (listify-key-sequence ",")))

(map! :leader (:desc "+major-mode" "m" #'my/call-localleader))

(after! evil
  ;; (global-set-key (kbd "M-m") #'my/call-localleader)
  (evil-define-key '(normal visual) prog-mode-map (kbd "C-,") 'my/call-localleader))

;;;; Doom-Font

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(when (display-graphic-p) ; gui
  (setq
   doom-font (font-spec :family "Monoplex KR Nerd" :size 14.0)
   doom-big-font (font-spec :family "Monoplex KR Nerd" :size 23.0))
  (setq doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size 14.0))
  (setq doom-unicode-font (font-spec :family "Symbola" :size 14.0))
  ;; (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 15.1)
  ;;       doom-big-font (font-spec :family "Sarasa Term K Nerd Font" :size 21.1))
  )

(unless (display-graphic-p) ; terminal
  (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 15.1)))

;;;; Hangul Korean

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
;; (setq default-transient-input-method "TeX") ; C-u set-input-method

(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3) ; use fontaine

;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "ko_KR.UTF-8")

;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")

(setq
 input-method-verbose-flag nil
 input-method-highlight-flag nil)

;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
(global-set-key (kbd "<menu>") 'toggle-input-method) ;; caps lock as <menu>
(add-hook 'context-menu-mode-hook '(lambda () (define-key context-menu-mode-map (kbd "<menu>") #'toggle-input-method)))
;; (global-unset-key (kbd "S-SPC"))

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+

;;;; Basics

;; (setq-default display-line-numbers-width-start t) ; doom's default t
(setq inhibit-compacting-font-caches t)

;; Stop asking abount following symlinks to version controlled files
;; (setq vc-follow-symlinks t)

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files
(setq create-lockfiles nil)

;; Ridiculous path view is vanilla emacs. change truename!
;; truename 을 원하지 않는다. 심볼링링크대로 쓰고 싶다. nil 로 사용한다.
;; (setq find-file-visit-truename t)

;;;; initial-scratch-message

(setq initial-scratch-message user-initial-scratch-message)

;;;; Tab-width

;; ====== Buffer-local variables ======
;; (setq-default
;;  ;; Display long lines
;;  truncate-lines nil ; default t
;;  ;; Default fill column width
;;  fill-column 80
;;  ;; Never mix, use only spaces
;;  indent-tabs-mode nil ;; Width for line numbers display-line-numbers-width 4

;;  ;; 1) per major-mode config or hook
;;  ;; 2) editorconfig
;;  ;; 3) tab-width 4 (below)
;;  ;; tab-width 4 ;; 2024-03-11 org-mode element-cache 사용 시 무조건 8이다. 충돌난다. 끈다.

;;  display-line-numbers-width-start t ; 2024-06-26
;;  )

;;;; Display-Line-Numbers-Mode

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)
(remove-hook! text-mode #'display-line-numbers-mode)

;; 2024-04-01 disable
;; (unless IS-TERMUX
;;   (add-hook 'org-mode-hook 'display-line-numbers-mode)
;;   (add-hook 'markdown-mode-hook 'display-line-numbers-mode))

;;;; Time

(require 'time)
(setq display-time-format " |%a %e %b, %H:%M| ")
;; Covered by `display-time-format'
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (setq display-time-interval 30) ; default 60
(setq display-time-default-load-average nil)

;; NOTE 2022-09-21: For all those, I have implemented my own solution
;; that also shows the number of new items, although it depends on
;; notmuch: the `notmuch-indicator' package.
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)

;;;; Calendar

(require 'calendar)
;; (setq org-agenda-start-on-weekday nil)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq
 calendar-date-style 'iso ;; YYYY/MM/DD
 calendar-mark-holidays-flag t
 calendar-week-start-day 1 ;; 0 Sunday, 1 Monday
 calendar-mark-diary-entries-flag nil
 calendar-latitude user-calendar-latitude
 calendar-longitude user-calendar-longitude
 calendar-location-name user-calendar-location-name
 calendar-time-display-form
 '(24-hours
   ":" minutes
   (if time-zone
       " (")
   time-zone
   (if time-zone
       ")")))


;;;; Which-key

(after! which-key
  (setq which-key-idle-delay 0.4 ; important
        which-key-idle-secondary-delay 0.01)
  (setq which-key-use-C-h-commands t) ; paging key maps
  (setq which-key-max-description-length 29) ; doom 27, spacemacs 36
  )

;;;; evil

;; Key binding guide
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/
;; NOTE: use `map!' macro for convienience

;; ------------------------------------------------
;; Key binding vars
(after! evil
  ;; Implicit /g flag on evil ex substitution, because I use the default behavior less often.
  (setq evil-ex-substitute-global t) ; default nil

  ;; C-h is backspace in insert state
  ;; (setq evil-want-C-h-delete t) ; default nil
  (setq evil-want-C-w-delete t) ; default t
  (setq evil-want-C-u-scroll t) ; default t

  ;; use C-i / C-o  evil-jump-backward/forward
  (setq evil-want-C-i-jump t) ; default nil

  ;; mpereira-dotfiles-evil-clojure/configuration.org
  ;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
  ;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
  ;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
  ;; '(foo.bar/baz)', which I don't like.
  ;; (setq-default evil-symbol-word-search t)
  ;; (setq evil-jumps-cross-buffers nil)
  (setq evil-want-Y-yank-to-eol t)

  ;; 'Important' Prevent the cursor from moving beyond the end of line.
  ;; Don't move the block cursor when toggling insert mode
  (setq evil-move-cursor-back nil) ; nil is better - default t
  (setq evil-move-beyond-eol nil) ; default nil

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil) ; default t

  ;; Change Doom's Default
  (setq +evil-want-o/O-to-continue-comments nil)
  (setq +default-want-RET-continue-comments nil)

  (setq evil-disable-insert-state-bindings t) ; 2024-10-25 default nil

  (setq evil-want-fine-undo t) ; doom 'nil

  ;; Don't create a kill entry on every visual movement.
  ;; More details: https://emacs.stackexchange.com/a/15054:
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; (setq evil-insert-state-cursor '(box "#F86155")) ;; better look
  ;; (setq evil-normal-state-cursor '(box "DarkGoldenrod2"))

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map "L" nil)
    (define-key evil-motion-state-map "M" nil)

    ;; (evil-global-set-key
    ;;  'normal (kbd "DEL") 'evil-switch-to-windows-last-buffer) ; Backspace

    ;; Replace Emacs Tabs key bindings with Workspace key bindings
    ;; replace "." search with consul-line in Evil normal state
    ;; use default "/" evil search
    ;; ;; (define-key evil-insert-state-map (kbd "C-k") 'kill-line) ; 2024-06-11 disable conflict with corfu-previous

    ;; evil macro
    (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

    ;; o :: ace-link-info 이거면 충분하다.
    ;; [[file:~/spacemacs/doc/DOCUMENTATION.org::*Binding keys][Binding keys]]
    (define-key evil-insert-state-map (kbd "C-]") 'forward-char) ; very useful

    ;; =C-w= 'insert 'evil-delete-backward-word
    ;; =C-w= 'visual 'evil-window-map
    ;; use evil bindings $ ^
    ;; (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
    ;; (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
    ;; (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
    ;; (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)

    ;; M-d region delete and C-d char delete
    (define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)

    ;; evil-delete-char -> delete-forward-char
    (define-key evil-normal-state-map "x" 'delete-forward-char)
    (define-key evil-normal-state-map "X" 'delete-backward-char))

  ;; evil-org
  (with-eval-after-load 'evil-org
    ;; (evil-define-key 'insert 'evil-org-mode-map (kbd "C-d") 'delete-forward-char)
    (evil-define-key 'normal 'evil-org-mode-map "x" 'delete-forward-char)
    (evil-define-key 'normal 'evil-org-mode-map "X" 'delete-backward-char))
  ) ; end-of after evil

;;;; evil cursor with toggle-input-method

(after! evil
  ;; keep evil insert cursor status per input-method
  ;; 2024-04-09 커서 상태 기반 한영 입력! 커서를 신뢰하라!
  ;; 2024-09-19 org 모드에서 동작이 영 안좋다. 끈다.
  ;; - 버퍼 전환 시 커서 상태 유지
  ;; - 커서를 보면 input-method 온오프를 알 수 있다.
  ;; - 한영 전환은 insert 모드에서만 가능

;;;###autoload
  (defun block-toggle-input-method ()
    (interactive)
    (message (format "Input method is disabled in <%s> state." evil-state)))

  (mapc
   (lambda (mode)
     (let ((keymap (intern (format "evil-%s-state-map" mode))))
       (define-key (symbol-value keymap) (kbd "<Hangul>") #'block-toggle-input-method)
       (define-key (symbol-value keymap) (kbd "S-SPC") #'block-toggle-input-method)
       (define-key (symbol-value keymap) (kbd "<menu>") #'block-toggle-input-method)))
   '(motion normal visual))

  ;; ;;;###autoload
  ;; (defun check-evil-cursor-state-between-window-switch ()
  ;;   (let ((type
  ;;          (pcase current-input-method ('nil 'bar) ("korean-hangul" 'hbar))))
  ;;     (setq-local evil-insert-state-cursor type)))

  ;; ;;;###autoload
  ;; (defun toggle-input-method-with-evil-cursor-switch ()
  ;;   (interactive)
  ;;   (toggle-input-method)
  ;;   (check-evil-cursor-state-between-window-switch)
  ;;   ;; (message (format "Input method is disabled in <%s> state." evil-state))
  ;;   )

  ;; (mapc
  ;;  (lambda (mode)
  ;;    (let ((keymap (intern (format "evil-%s-state-map" mode))))
  ;;      (define-key
  ;;       (symbol-value keymap) (kbd "<Hangul>") #'toggle-input-method-with-evil-cursor-switch)
  ;;      (define-key
  ;;       (symbol-value keymap) (kbd "S-SPC") #'toggle-input-method-with-evil-cursor-switch)))
  ;;  '(insert))

  ;; (add-hook 'evil-insert-state-entry-hook 'check-evil-cursor-state-between-window-switch 90)

  ;; (defadvice! change-cursor-after-toggle-input (fn &optional arg interactive)
  ;;   :around #'toggle-input-method
  ;;   :around #'set-input-method (funcall fn arg interactive)
  ;;   (let ((type
  ;;          (pcase current-input-method
  ;;            ('nil 'bar)
  ;;            ("korean-hangul" 'hbar))))
  ;;     (setq-local evil-insert-state-cursor type)))
  )


;;;; evil-escape

;; ,. as Esc key binding
;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59/7
(after! evil-escape
  (setq evil-escape-key-sequence ",.") ;; "jk"
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-delay 1.0) ;; 0.5, default 0.1
  (evil-escape-mode 1))

;;;; undo-fu

(after! undo-fu
  ;; undo-fu
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "\C-r") 'undo-fu-only-redo)
  (unbind-key "C-M-_" 'undo-fu-mode-map)
  (global-unset-key (kbd "C-M-_")))

;; 스페이스맥스와 다르네?! 아래는 스페이스맥스 설정
;;         ;; C-r 은 isearch-backward 가 기본
;;         (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
;;         (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

;;         ;; Undo-fu customization options
;;         ;; Undoing with a selection will use undo within that region.
;;         (setq undo-fu-allow-undo-in-region t)
;;         ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
;;         (setq undo-fu-ignore-keyboard-quit t)
;;         ;; By default while in insert all changes are one big blob. Be more granular
;;         (setq evil-want-fine-undo t)

;;;; set-popup-rules

(progn
  ;; Disabling hidden mode-line in popups
  ;; By default, the mode-line is hidden in popups. To disable this, you can either:
  ;; (plist-put +popup-defaults :modeline t)

  ;; Completely disable management of the mode-line in popups:
  ;; (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h) ; important

  ;; (setq +popup--display-buffer-alist nil) ; reset all

  ;; default popup rules
  (set-popup-rules!
    '(
      ;; ("^\\*scratch*" :ignore t) ; for TEST
      ("^\\.doom.d/diary" :ignore t) ; for TEST
      ("^\\*Completions" :ignore t)
      ("*Org Preview LaTeX Output*" :ingnore t) ; 2025-01-24
      ("^\\*Local variables\\*$" :vslot -1 :slot 1 :size +popup-shrink-to-fit)
      ;; ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)" :vslot -2 :size 0.3 :autosave t :quit t :ttl nil) ; +default
      ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl 0 :quit t)
      ("^\\*Messages\\*" :vslot -4 :side right :size 0.4 :quit t :ttl 0) ; jh
      ("^\\*\\(?:doom \\|Pp E\\)" ; transient buffers (no interaction required)
       :vslot -3
       :size +popup-shrink-to-fit
       :autosave t
       :select ignore
       :quit t
       :ttl 0)
      ("^\\*doom:" ; editing buffers (interaction required)
       :vslot -4
       :size 0.35
       :side right
       :autosave t
       :select t
       :modeline t
       :quit nil
       :ttl t)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup" ; editing buffers (interaction required)
       :vslot -5
       :size 0.35
       :select t
       :modeline nil
       :quit nil
       :ttl nil)
      ("^\\*\\(?:Wo\\)?Man " :vslot -6 :size 0.45 :select t :quit t :ttl 0)
      ("^\\*Calc" :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize" :slot 2 :side right :size 0.5 :select t :quit nil)
      ("^ \\*undo-tree\\*" :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*\\([Hh]elp\\|Apropos\\)" :slot 2 :vslot -8 :size 0.42 :select t)
      ;; ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
      ;;  :vslot -11 :size 0.35 :select t)
      ("^\\*xwidget" :vslot -11 :size 0.35 :select nil)
      ;; ("*Org Agenda(n)*" :size 0.5 :side left :select nil :quit nil :ttl 0)
      ("^\\*eww.*" :size 82 :side left :modeline t :select t :quit nil :ttl t) ; jh

      ("*Ilist*" :size 45 :side left :modeline nil :select nil :quit nil)
      ;; ("^ ?\\*Treemacs" :slot 7 :size 45 :side left :modeline nil :select nil :quit nil)
      ;; ("^ ?\\*NeoTree" :slot 7 :size 45 :side left :modeline t :slect nil :quit nil)

      ;; ("^\\*gptel\\*" :size 84 :side right :modeline t :select t :quit nil :ttl t)
      ;; ("^\\*gptel-ask\\*" :size 80 :side right :modeline nil :select nil :quit nil)
      ;; ("^\\*gptel-quick\\*" :size 80 :side right :modeline nil :select nil :quit nil)
      ;; "*EKG Capture.*\\*" "*EKG Edit.*\\*"  "*[Ee][Kk][Gg] .*\\*"
      ;; ("*EKG Capture.*\\*" :slot 3 :side bottom :size 0.4 :select t :quit nil) ; jh
      ;; ("*EKG Edit.*\\*" :slot 3 :side bottom :size 0.4 :select t :quit nil) ; jh
      ("*Go-Translate*" :side bottom :size 0.4 :select t :quit t) ; jh
      ("\\`\\*evil-owl\\*\\'" :side bottom :ttl t :height 20 :quit t)
      ;; ("\\*Embark Actions\\*" :side bottom :ttl t :height 20 :quit t) ; mixed
      ;; ("^\\*info\\*$" :slot 2 :vslot 2 :size 0.45 :select t))
      ("^\\*info\\*$"
       :slot 2
       :vslot 2
       :size 82
       :side left
       :modeline t
       :select t
       :ttl nil)) ;; `Info-mode' jh
    )

  ;; =M-x eldoc-doc-buffer= 함수 호출로 표시하는 buffer 크기 조절
  (set-popup-rule! "^\\*eldoc for" :size 0.2 :vslot -1) ; "*eldoc*"

  ;; (add-hook 'imenu-list-major-mode-hook (lambda (tab-line-mode -1) ))

  ;; 와우 이거다. 태그랑 쓰기랑 나눠야 한다.
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  `("*ekg tag.*\\*"
  ;;    (display-buffer-reuse-window display-buffer-in-direction)
  ;;    (direction . left)
  ;;    (window . root)
  ;;    (window-width . 0.35)))

  ;; 2024-03-19 sdcv 에서 가져옴. 이게 괜찮은듯
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  `("^\\*eww.*"
  ;;    (display-buffer-reuse-window
  ;;     display-buffer-in-direction)
  ;;    (direction . right)
  ;;    (window . root)
  ;;    (window-width . 0.35)))

  (set-popup-rule! "^\\*eww.*" :size 84 :side 'right :modeline t :select t :ttl nil)
  ;; (set-popup-rule! "\\`\\*chatgpt\\* " :ttl t :side 'bottom :height 20 :quit t :select t)
  ;; ;; (set-popup-rule! "^\\*Messages\\*" :ttl t :side 'bottom :height 20 :quit t :ttl t)
  ;; (set-popup-rule! "^\\*doom:vterm*" :ttl t :side 'bottom :height 20 :quit t)
  ;; (set-popup-rule! "^\\*npm*" :ttl t :side 'bottom :height 20 :quit t)
  ;; (set-popup-rule! "^\\*Flycheck*" :ttl t :side 'bottom :height 20 :quit t)

  ;; from prot's dotfiles : important
  (add-to-list
   'display-buffer-alist
   `("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
     (display-buffer-no-window)
     (allow-no-window . t)))
  )

;;;; bookmark

;; On each machine I use, I have different bookmarks, yet they all
;; point to the same location.
(setq bookmark-default-file "~/emacs-bookmarks.el")
;; (setq bookmark-default-file (concat user-dotemacs-dir "assets/bookmarks"))
(setq bookmark-use-annotations nil)
(setq bookmark-automatically-show-annotations t)

;; Save the `bookmark-file' each time I modify a bookmark.
(setq bookmark-save-flag 1)

;; https://www.emacswiki.org/emacs/BookmarkPlus
;;
;; Enhancements to the built-in Emacs bookmarking feature.
;; (use-package! bookmark+)
;; (define-key bookmark-bmenu-mode-map (kbd "s-o") #'ace-window)
;; when this is not set to `nil' explicitly, auto-save bookmarks
;; gets itself into an infinite loop attempting to autosave and
;; write the custom value to custom-file.el.  this happens only when
;; the buffer associated with the bookmark has not been saved. (to
;; reproduce the issue, remove the customize-set-value sexp, find a
;; new file, and wait 30 seconds; it'll start printing messages like
;; mad.  C-g will eventually break the loop.)  i only use one
;; bookmark file so this isn't a problem but it really does seem
;; like a bmkp bug.
;; (customize-set-value 'bmkp-last-as-first-bookmark-file nil)
;; auto-set bookmarks.
;; (setq bmkp-automatic-bookmark-mode-delay 30))

;;;; show-trainling-whitespace

;; https://idiomdrottning.org/show-trailing-whitespace
;; `show-trailing-whitespace' is my friend.
(setq-hook! (text-mode prog-mode conf-mode) show-trailing-whitespace t)

;; White space cleanup, without obtrusive white space removal.
;; Whitespaces at EOL and EOF are trimmed upon file save, and only for lines modified by you.
;; Much better than globally removing EOL whitespace on save, especially when
;; editing collaboratively with others.
;; README/doomemacs-git/lisp/doom-editor.el

;; (ws-butler-keep-whitespace-before-point nil)
;; (ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode diff-mode markdown-mode))

;;;; User Goto Functions

;; (defun goto-emacs-dotfiles.org ()
;;   "Open jh-emacs.org file."
;;   (interactive)
;;   (find-file (concat dotspacemacs-directory "jh-emacs.org")))

(defun goto-pandoc-config ()
  "Open pandoc metadata file."
  (interactive)
  (find-file "~/.config/pandoc/metadata.yml"))

;;;; CJK Word Wrap

;; Emacs 28 adds better word wrap / line break support for CJK.
(setq word-wrap-by-category t) ; default nil

;; Normally when word-wrap is on, Emacs only breaks lines after
;; whitespace characters.  When this option is turned on, Emacs also
;; breaks lines after characters that have the "|" category (defined in
;; characters.el).  This is useful for allowing breaking after CJK
;; characters and improves the word-wrapping for CJK text mixed with
;; Latin text.

;; 일반적으로 단어 줄 바꿈이 켜져 있으면 Emac 은 공백 문자 뒤에 오는 줄만 줄
;; 바꿈합니다. 이 옵션을 켜면 Emac 은 "|" 범주(characters.el 에 정의됨)가 있는 문자
;; 뒤의 줄도 줄 바꿈합니다. 이 옵션은 한중일 문자 뒤에 줄 바꿈을 허용하는 데
;; 유용하며 라틴 텍스트와 혼합된 한중일 텍스트의 단어 줄 바꿈을 개선합니다.

;;;; winner

(after! winner
  (setq winner-boring-buffers-regexp "\\*.*\\*")
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

;;;; abbr

(setq abbrev-file-name (concat user-dotemacs-dir "abbrev_defs"))
(read-abbrev-file abbrev-file-name)
(setq save-abbrevs t)
(setq-default abbrev-mode t)

;;;; fill-column-indicator-mode

;; 2023-04-16 Learn how-to use menu-bar for beginner on GUI mode
(when (display-graphic-p) ; gui
  (add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
  ;; (add-hook 'markdown-mode-hook 'display-fill-column-indicator-mode)
  )

;;;; fortune

;; not work on termux
(unless IS-TERMUX
  (require 'fortune)
  (setq fortune-always-compile nil)
  (setq fortune-dir (concat root-path "usr/share/games/fortunes/advice"))
  (setq fortune-file (concat root-path "usr/share/games/fortunes/advice")))


;;;; autorevert

;; ~/doomemacs-junghan0611/lisp/doom-editor.el
;; (after! autorevert
;; Ensure that files are reloaded from disk (when switching branches, or from dropbox sync)
(global-auto-revert-mode 1) ; doom nil
(setq auto-revert-interval 10)

;;;; DONT custom scrolling

;; (setq
;;  ;; Do not adjust window-vscroll to view tall lines
;;  auto-window-vscroll nil ; doom nil
;;  ;; Keep the point in the same position while scrolling
;;  scroll-preserve-screen-position t ; doom t
;;  ;; Do not move cursor to the center when scrolling
;;  scroll-conservatively 10 ; doom 10
;;  ;; Scroll at a margin of one line
;;  ;; scroll-margin 1 ; doom 0
;;  )

;; (when (fboundp 'pixel-scroll-precision-mode)
;;   ;; Better scrolling on Emacs29+, specially on a touchpad
;;   (setq pixel-scroll-precision-use-momentum t) ; doom nil
;;   (pixel-scroll-precision-mode +1))

;;;; DONT help-mode-hook : visual-line-mode

;; (add-hook 'help-mode-hook #'visual-line-mode)

;;;; Transparency

(if (eq system-type 'gnu/linux)
    (setq default-frame-alist
          (push '(alpha-background . 93) default-frame-alist)) ;; 93
  (setq default-frame-alist (push '(alpha . (95 90)) default-frame-alist)))

;; Emacs 29 ushers in a bold new era where a frame's background can be made
;; transparent without affecting the transparency of foreground text and other
;; elements. Who'd-a thunk? Use that feature when available

(defun toggle-transparency (alpha-level)
  (interactive "p")
  (message (format "%s" alpha-level))
  (when (< alpha-level 50)
    (setq alpha-level 90))
  (let ((myalpha (or (frame-parameter nil 'alpha) 100))
        (frame-param
         (if (< emacs-major-version 29)
             'alpha
           'alpha-background)))

    (set-frame-parameter nil frame-param myalpha)
    (message (format "Frame %s level is %d" frame-param myalpha))))

(defun set-transparency (alpha-level)
  ;; in Emacs 29+, set background opacity
  ;; before 29, we have no choice but to set frame opacity
  (interactive "p")
  (message (format "Alpha level passed in: %s" alpha-level))
  (let ((alpha-level
         (if (< alpha-level 2)
             (read-number "Opacity percentage: " 90)
           alpha-level))
        (frame-param
         (if (< emacs-major-version 29)
             'alpha
           'alpha-background)))
    (set-frame-parameter nil frame-param alpha-level)
    (message (format "Frame %s level is %d" frame-param alpha-level))))
(defalias 'set-opacity 'set-transparency)

(defun set-transparency-low ()
  (interactive)
  (set-transparency 70))

;;;; golden-ratio

(use-package! golden-ratio)

;;;; emms : music player

;; path
(setq
 emms-directory (concat doom-data-dir "emms")
 emms-cache-file (concat doom-cache-dir "emms"))

;; M-x emms-add-directory-tree

;;; :emacs

;; move to modules/custom/emacs

;;; :completion corfu vertico

;;;; vertico

(unless IS-TERMUX

  (require 'vertico-buffer)
  ;; (setq vertico-resize 'grow-only) ; doom nil

  ;; vertico on Top
  (setq vertico-buffer-display-action
        `(display-buffer-in-side-window
          (window-height . ,(+ 3 vertico-count)) (side . top)))
  (vertico-mode +1)
  (vertico-buffer-mode +1)

  ;; sachac-dotfiles/Sacha.org
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))
  )

;;;; DONT vertico hangul

;; from ohyecloudy
;; vertico는 =post-command-hook= 을 사용해서 증분 완성(incremental completion)을
;; 수행한다. 영문 입력시 =post-command-hook= 이 잘 발동하지만 조합해서 입력하는
;; 한글은 =post-command-hook= 이 호출되지 않는다. helm 동작 방법을 참고해
;; timer를 돌려서 해결했다.

;; (after! vertico
;;   (defun my/vertico-setup-then-remove-post-command-hook (&rest args)
;;     "vertico--setup 함수에서 추가하는 post-command-hook을 제거한다.

;;      입력 조합으로 표현하는 한글 입력시 post-command-hook이 입력되지 않는다.
;;      한글 증분 완성을 위해 timer로 호출하기 때문에 제거한다"
;;     (remove-hook 'post-command-hook #'vertico--exhibit 'local))

;;   (defun my/vertico-exhibit-with-timer (&rest args)
;;     "타이머를 넣어 타이머 이벤트 발생시 vertico--exhibit을 호출해 미니버퍼 완성(completion) 후보 리스트를 갱신한다

;;      post-command-hook이 발동하지 않는 한글 입력시에도 한글 증분 완성을 하기 위해 timer를 사용한다"
;;     (let (timer)
;;       (unwind-protect
;;           (progn
;;             (setq timer
;;                   (run-with-idle-timer
;;                    0.01 'repeat
;;                    (lambda ()
;;                      (with-selected-window (or (active-minibuffer-window)
;;                                                (minibuffer-window))
;;                        (vertico--exhibit)))))
;;             (apply args))
;;         (when timer
;;           (cancel-timer timer)))))

;;   (advice-add
;;    #'vertico--setup
;;    :after #'my/vertico-setup-then-remove-post-command-hook)
;;   (advice-add #'vertico--advice :around #'my/vertico-exhibit-with-timer))

;;;; custom consult

(after! consult
  ;; replace "." search with consul-line in Evil normal state
  ;; use default "/" evil search
  ;; (evil-global-set-key 'normal (kbd ".") 'consult-line) ;; see doomkeys.el
  ;; (evil-global-set-key 'motion (kbd ".") 'consult-line)
  ;; (evil-global-set-key 'visual (kbd ".") 'consult-line)

  ;; (map! :leader
  ;;       :g "j i" #'consult-line
  ;;       :g "j I" #'consult-buffer)

  ;; +default/search-cwd
  (defun my/consult-find ()
    (interactive)
    (consult-find "."))

  (defun my/consult-fd ()
    (interactive)
    (consult-fd "."))

  ;; spacemacs/layers/+completion/compleseus/funcs.el
  ;;;###autoload
  (defun my/compleseus-search (use-initial-input initial-directory)
    (let* ((initial-input
            (if use-initial-input
                (doom-pcre-quote ;; rxt-quote-pcre
                 (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (or (thing-at-point 'symbol t) ""))) ""))
           (default-directory
            (or initial-directory
                (read-directory-name "Start from directory: "))))
      (consult-ripgrep default-directory initial-input)))

  (defun my/search-cwd-symbol-at-point ()
    "Search current folder."
    (interactive)
    (my/compleseus-search t default-directory))

  (setq consult-preview-key "M-m")
  ;; (setq consult--customize-alist nil)
  (consult-customize
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd

   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark

   ;; custom below
   my/search-cwd-symbol-at-point
   +lookup/definition
   +lookup/implementations
   :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k")) ; M-j,k
  )

;;;; corfu

;; 2024-11-06 back to default
;; 2024-09-13 기본 설정, jump-out-of-pair 추가
;; DEPRECATED Tab 이 자동 완성이면 괄호 점프랑 충돌 난다. C-j/k C-n/p 는 직관적인 기본 설정이므로 건들이지 않는다.

(after! corfu
  ;; (setq corfu-auto-delay 0.5) ; doom 0.24
  (setq corfu-auto-prefix 4) ; doom 2
  (setq corfu-preselect 'valid) ; doom 'prompt
  (setq completion-cycle-threshold 3) ; doom nil
  (setq tab-always-indent t) ; for jump-out-of-pair - doom 'complete
  (setq +corfu-want-minibuffer-completion nil) ; doom t

  (setq +corfu-want-tab-prefer-expand-snippets t) ; 2024-11-06
  (setq +corfu-want-tab-prefer-navigating-snippets t)
  (setq +corfu-want-tab-prefer-navigating-org-tables t)

  ;; from minemacs
  ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
  (when (eq emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  ;; 2025-02-03 disable - use C-c d @org-mode
  ;; (add-hook! '(org-mode-hook markdown-mode-hook)
  ;;   (defun +corfu-add-cape-dict-h ()
  ;;     (add-hook 'completion-at-point-functions #'cape-dict 0 t)))

  ;; IMO, modern editors have trained a bad habit into us all: a burning need for
  ;; completion all the time -- as we type, as we breathe, as we pray to the
  ;; ancient ones -- but how often do you *really* need that information? I say
  ;; rarely. So opt for manual completion:
  ;; doom/hlissner-dot-doom/config.el
  ;; (setq corfu-auto nil)

  ;; default 'C-S-s'
  (define-key corfu-map (kbd "M-.") '+corfu-move-to-minibuffer)
  )

;;;; DONT corfu-echo

;; 1) add '(package! corfu-popupinfo :disable t)' in packages.el

;; 2) turn on corfu-echo
;; (after! corfu
;;   (require 'corfu-echo)
;;   (add-hook 'corfu-mode-hook 'corfu-echo-mode)
;;   )

;;;; cape with dabbrev

;; (map! :map some-mode-map
;;       "C-x e" #'cape-emoji)
;; (add-hook! some-mode (add-hook 'completion-at-point-functions #'some-capf depth t))
;; ;; OR, but note the different call signature
;; (add-hook 'some-mode-hook (lambda () (add-hook 'completion-at-point-functions #'some-capf depth t)))

;; (after! cape
;;   (setq cape-dabbrev-min-length 5) ; default 4
;;   (setq cape-dabbrev-check-other-buffers #'cape--buffers-major-mode) ; 'some
;;   )

;; ;; 2023-07-08 순서 때문에 따로 확실하게 점검한다.
;; (defun cape-markdown-mode-setup ()
;;   (interactive)
;;   (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; top
;;   )

;; (defun cape-org-mode-setup ()
;;   ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   ;; (add-to-list 'completion-at-point-functions #'zk-completion-at-point) ;; top
;;   )

;; ;; (defun cape-prog-mode-setup ()
;; ;;   ;; (add-to-list 'completion-at-point-functions #'cape-file)
;; ;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; ;;   ;; (add-to-list 'completion-at-point-functions #'cape-history)
;; ;;   ;; (add-to-list 'completion-at-point-functions #'cape-keyword) ;; no.1
;; ;;   )

;; (add-hook 'markdown-mode-hook 'cape-markdown-mode-setup)
;; (add-hook 'org-mode-hook 'cape-org-mode-setup)
;; ;; (add-hook 'conf-mode-hook 'cape-prog-mode-setup)
;; ;; (add-hook 'prog-mode-hook 'cape-prog-mode-setup)

;;; :checkers

;;;; OKAY Flycheck

(after! flycheck
  (setq flycheck-global-modes '(not emacs-lisp-mode org-mode markdown-mode gfm-mode))
  (setq flycheck-checker-error-threshold 1000) ; need more than default of 400
  (global-flycheck-mode +1)
  )

;; (progn
;;   (setq flycheck-help-echo-function nil ; default 'flycheck-help-echo-all-error-messages
;;         flycheck-display-errors-function nil ; default 'flycheck-display-error-messages
;;         )

;;   (after! flycheck
;;     (ignore-errors
;;       (define-key flycheck-mode-map flycheck-keymap-prefix nil))
;;     (setq flycheck-keymap-prefix nil)

;;     (add-hook! flycheck-mode
;;       (defun disable-flycheck-popup-buffer ()
;;         (setq flycheck-display-errors-function #'ignore)))
;;     (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package)
;;     )

;;   (after! elisp-mode
;;     (add-hook! 'doom-scratch-buffer-created-hook
;;       (defun flycheck-off ()
;;         (flycheck-mode -1))))
;;   )

;;;; DEPRECATED Flymake

;;;;; DONT disable flymake-mode default

;; (remove-hook! (prog-mode text-mode) #'flymake-mode)

;;;;; DONT flymake-vale

;; flymake-vale-modes defaults to:
;;  => (text-mode latex-mode org-mode markdown-mode message-mode)
;; (add-to-list 'flymake-vale-modes 'adoc-mode)
;; (require 'flymake-vale)
;; (add-hook 'text-mode-hook #'flymake-vale-load)
;; (add-hook 'latex-mode-hook #'flymake-vale-load)
;; (add-hook 'org-mode-hook #'flymake-vale-load)
;; (add-hook 'markdown-mode-hook #'flymake-vale-load)
;; (add-hook 'message-mode-hook #'flymake-vale-load)

;;; :editor

;; (evil +everywhere); come to the dark side, we have cookies
;; file-templates    ; auto-snippets for empty files
;; fold              ; (nigh) universal code folding
;; format            ; automated prettiness
;; multiple-cursors  ; editing in many places at once
;; rotate-text       ; cycle region at point between text candidates
;; snippets          ; my elves. They type so I don't have to
;; ;;word-wrap         ; soft wrapping with language-aware indent

;;;; NOTE :editor doom

;;;; :editor titlecase

;; (use-package! titlecase
;;   :defer t
;;   :init
;;   (after! embark
;;     (define-key embark-region-map "T" #'titlecase-region)
;;     (define-key embark-heading-map "T" #'titlecase-line)
;;     (define-key embark-sentence-map "T" #'titlecase-sentence)))

;;;; unfill

(use-package! unfill
  :bind
  (([remap fill-paragraph] . unfill-toggle)
   :map
   org-mode-map
   ("M-q" . unfill-toggle)))

;;;; evil-matchit - symbol-overlay

;; % evilmi
;; 절대 글로벌로 켜지 말 것! 각 스페이스맥스 프로그래밍 언어 레이어에 보면 이미 들어가 있다.
;; /mpereira-dotfiles-evil-clojure/configuration.org
;; vim matchit.vim is ported into emacs
;; (use-package! evil-matchit
;;   :config
;;   ;; https://github.com/redguardtoo/evil-matchit/pull/141
;;   (evilmi-load-plugin-rules '(web-mode) '(simple template html))
;;   (add-hook 'web-mode-hook 'turn-on-evil-matchit-mode))

;;;; evil-surround

;; @call-function
;; visual mode S- or gS-
;; normal mode ys- or yS-
;; change surround cs-
;; delete surround ds-
;; @select area
;; call-functionu- - ;현재부터 단어 끝까지
;; {call-function}-i- ;현재 단어
;; {call-function}-s- ;현재 줄
;; @wrap function
;; {select-area}-w
;; ${target}( 바꾸고싶은거 ), ${change}(바뀔거)
;; 감싸기:     => y-s-i-w-${change}( "(", "{", "[")
;; 전부 감싸기 => y-s-s-${change}
;; 바꾸기: => c-s-${target}( "(", "{", "["), ${change}
;; 벗기기: => d-s-${target}( "(", "{", "[")

;;;;;  evil-traces

;; move: m +{n}, delete: +{n},+{n}d, join: .,+{n}j glboal: g/{target}/{change}

;;;; evil-owl

;; gl ${operator}
;; evil-owl-mode                   A minor mode to preview marks and registers before using them.
;; evil-owl-goto-mark            (`)   Wrapper function for ‘evil-goto-mark’ that shows a preview popup.
;; evil-owl-set-marker           (m)   Wrapper function for ‘evil-set-marker’ that shows a preview popup.
;; evil-owl-record-macro         (q)   Wrapper function for ‘evil-record-macro’ that shows a preview popup.
;; evil-owl-use-register         (")   Wrapper function for ‘evil-use-register’ that shows a preview popup.
;; evil-owl-execute-macro        (@)   Wrapper function for ‘evil-execute-macro’ that shows a preview popup.
;; evil-owl-goto-mark-line       (')   Wrapper function for ‘evil-goto-mark-line’ that shows a preview popup.
;; evil-owl-scroll-popup-up        Scroll the popup up one page.
;; evil-owl-scroll-popup-down      Scroll the popup down one page.
;; evil-owl-paste-from-register    Wrapper function for ‘evil-paste-from-register’ that shows a preview popup.

;; Not sure what is in a register? Have it show you when you hit ~”~ or ~@~
(use-package! evil-owl
  :hook (doom-first-input . evil-owl-mode)
  :config
  (setq evil-owl-display-method 'window)
  (setq evil-owl-idle-delay 0.5) ; default 1
  (setq evil-owl-max-string-length 500))

;;;; smartparens

;; Smartparens - Practicalli config and key bindings
;; A Spacemacs like Lisp state menu (without the transient state)

(after! smartparens
  ;; 2023-09-14 global 로 사용하다보니 거슬린다. 잠시만. 글로벌을 빼면 어떤가?
  ;; ("\\\\(" . "\\\\)") ;; emacs regexp parens
  ;; ("\\{"   . "\\}")   ;; latex literal braces in math mode
  ;; ("\\("   . "\\)")   ;; capture parens in regexp in various languages
  ;; ("\\\""  . "\\\"")  ;; escaped quotes in strings
  ;; ("/*"    . "*/")    ;; C-like multi-line comment
  ;; ("\""    . "\"")    ;; string double quotes
  ;; ("'"     . "'")     ;; string single quotes/character quotes
  ;; ("("     . ")")     ;; parens (yay lisp)
  ;; ("["     . "]")     ;; brackets
  ;; ("{"     . "}")     ;; braces (a.k.a. curly brackets)
  ;; ("`"     . "`")     ;; latex strings. tap twice for latex double quotes

  ;; Unbind `M-s' (set by paredit keybindings above) because it's bound
  ;; to some handy occur related functions
  ;; (define-key sp-keymap (kbd "M-s") nil)

  ;; org 모드에서 거슬린다. 제거. 굳.
  (sp-local-pair 'org-mode "(" ")" :actions '(rem)) ; for denote completion
  (sp-local-pair 'org-mode "[" "]" :actions '(rem)) ; temporarly
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "`" "`" :actions '(rem))
  (sp-local-pair 'org-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'org-mode "/" "/" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "~" "~" :actions '(rem))

  ;; markdown 에서도 삭제
  (sp-local-pair 'markdown-mode "(" ")" :actions '(rem))
  (sp-local-pair 'markdown-mode "'" "'" :actions '(rem))
  (sp-local-pair 'markdown-mode "`" "`" :actions '(rem))
  (sp-local-pair 'markdown-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'markdown-mode "/" "/" :actions '(rem))

  ;; pair management
  (sp-with-modes
      '(minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "(" nil :wrap "C-("))

  (sp-with-modes 'markdown-mode (sp-local-pair "**" "***"))

  ;; for latex math
  (sp-with-modes
      'org-mode
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "$$" "$$"))

  (sp-with-modes
      'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))
  ) ;; end-of smartparens

;; lisp modes
;; (sp-with-modes sp--lisp-modes
;;   (sp-local-pair "(" nil
;;                  :wrap "C-("
;;                  :pre-handlers '(my-add-space-before-sexp-insertion)
;;                  :post-handlers '(my-add-space-after-sexp-insertion)))

;; (defun my-add-space-after-sexp-insertion (id action _context)
;;   (when (eq action 'insert)
;;     (save-excursion
;;       (forward-char (sp-get-pair id :cl-l))
;;       (when (or (eq (char-syntax (following-char)) ?w)
;;                 (looking-at (sp--get-opening-regexp)))
;;         (insert " ")))))

;; (defun my-add-space-before-sexp-insertion (id action _context)
;;   (when (eq action 'insert)
;;     (save-excursion
;;       (backward-char (length id))
;;       (when (or (eq (char-syntax (preceding-char)) ?w)
;;                 (and (looking-back (sp--get-closing-regexp))
;;                      (not (eq (char-syntax (preceding-char)) ?'))))
;;         (insert " ")))))

;; ;; SP config for other modes. (from vedang)
;; (eval-after-load 'cider-repl
;;   '(progn
;;      (define-key cider-repl-mode-map (kbd ")") 'sp-up-sexp)
;;      (define-key cider-repl-mode-map (kbd "]") 'sp-up-sexp)
;;      (define-key cider-repl-mode-map (kbd "}") 'sp-up-sexp)))

;; (eval-after-load 'clojure-mode
;;   '(progn
;;      (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
;;      (define-key clojure-mode-map (kbd "]") 'sp-up-sexp)
;;      (define-key clojure-mode-map (kbd "}") 'sp-up-sexp)))

;; indent after inserting any kinds of parens
;; (defun my/smartparens-pair-newline-and-indent (id action context)
;;   (save-excursion
;;     (newline)
;;     (indent-according-to-mode))
;;   (indent-according-to-mode))
;; (sp-pair "(" nil :post-handlers
;;          '(:add (my/smartparens-pair-newline-and-indent "RET")))
;; (sp-pair "{" nil :post-handlers
;;          '(:add (my/smartparens-pair-newline-and-indent "RET")))
;; (sp-pair "[" nil :post-handlers
;;          '(:add (my/smartparens-pair-newline-and-indent "RET")))

;;;; tempel

(defvar jf/denote-base-dir
  (file-truename
   (if (file-exists-p (expand-file-name "~/.my-computer"))
       "~/sync/org/"
     "~/Documents/denote/"))
  "Where I put my notes; I need to provision differently for personal and
work computers.")

;;;###autoload
(cl-defun jf/org-macro-value-list (macro-name
                                   &key (dir jf/denote-base-dir))
  "List the unique inner text of all uses of MACRO-NAME in given DIR."
  (let ((path
         (if current-prefix-arg
             dir
           (or (buffer-file-name (current-buffer)) dir))))
    (s-split
     "\n"
     (s-trim
      (shell-command-to-string
       (concat
        "rg \"\\{\\{\\{"
        macro-name
        "\\((.+?)\\)\\}\\}\\}"
        "\" --only-matching --no-filename -r '$1' "
        path
        " | sort | uniq"))))))

;; (cl-defun my/org-tag-value-list (macro-name
;;                                  &key (dir jf/denote-base-dir))
;;   "list the unique inner text of all uses of macro-name in given dir."
;;   (let ((path
;;          (if current-prefix-arg
;;              dir
;;            (or (buffer-file-name (current-buffer)) dir))))
;;     (s-split
;;      "\n"
;;      (s-trim
;;       (shell-command-to-string
;;        (concat
;;         "fd "
;;         macro-name
;;         path))))))

;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-19: Check the `templates'
(use-package! tempel
  :bind
  (("M-+" . tempel-complete) ;; Alternative tempel-expand
   ("M-*" . tempel-insert))
  :bind (:map tempel-map (([backtab] . tempel-previous)
                          ("TAB" . tempel-next)))
  :config
  ;; (global-tempel-abbrev-mode)
  ;; (setq tempel-trigger-prefix "<") ; conflits with evil-shift
  (setq tempel-path (expand-file-name "tempel-templates.eld" user-dotemacs-dir))

  ;; Setup completion at point
  ;; (defun tempel-setup-capf ()
  ;;   ;; Add the Tempel Capf to
  ;;   ;; `completion-at-point-functions'. `tempel-expand' only triggers on
  ;;   ;; exact matches. Alternatively use `tempel-complete' if you want to
  ;;   ;; see all matches, but then Tempel will probably trigger too often
  ;;   ;; when you don't expect it.  NOTE: We add `tempel-expand' *before*
  ;;   ;; the main programming mode Capf, such that it will be tried first.
  ;;   (setq-local completion-at-point-functions
  ;;               (cons #'tempel-expand
  ;;                     completion-at-point-functions)))
  ;; (add-hook 'prog-mode-hook 'tempel-setup-capf)
  ;; (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Use concrete keys because of org mode
  ;; "M-RET" #'tempel-done
  ;; "M-{" #'tempel-previous
  ;; "M-}" #'tempel-next
  ;; "M-<up>" #'tempel-previous
  ;; "M-<down>" #'tempel-next

  ;; 2023-10-19 disable my custom
  (define-key tempel-map (kbd "RET") #'tempel-done)
  (define-key tempel-map (kbd "M-n") #'tempel-next)
  (define-key tempel-map (kbd "M-p") #'tempel-previous)

  (use-package! tempel-collection))

;;;; imenu-list

;; Show an outline summary of the current buffer.
(use-package! imenu-list
  :init
  (add-hook 'imenu-list-major-mode-hook #'toggle-truncate-lines)
  (setq imenu-list-focus-after-activation nil)
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-position 'left)
  ;; (setq imenu-list-idle-update-delay 2.0) ; default 1.0
  ;; (setq imenu-list-size 45) ; default 0.3
  :config
  ;;;###autoload
  (defun spacemacs/imenu-list-smart-focus ()
    "Focus the `imenu-list' buffer, creating as necessary.
If the imenu-list buffer is displayed in any window, focus it, otherwise create and focus.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
    (interactive)
    (if (get-buffer-window imenu-list-buffer-name t)
        (imenu-list-show)
      (imenu-list-smart-toggle)))
  (after! winum
    (define-key
     winum-keymap
     [remap winum-select-window-8]
     #'spacemacs/imenu-list-smart-focus)))

(global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
(global-set-key (kbd "M-<f8>") 'spacemacs/imenu-list-smart-focus)

;;;; ace-link

(use-package! ace-link
  :config
  (with-eval-after-load 'info
    (define-key Info-mode-map "o" 'ace-link-info))
  (with-eval-after-load 'help-mode
    (define-key help-mode-map "o" 'ace-link-help))
  (with-eval-after-load 'woman
    (define-key woman-mode-map "o" 'link-hint-open-link)))

;;;; deadgrep

(use-package! deadgrep
  :defer t
  :after consult
  :commands deadgrep
  :custom (deadgrep-project-root-function 'projectile-project-root))

;;;; rg ripgrep

(use-package! rg
  :defer t
  :config
  ;; (rg-enable-default-bindings) ;; use =C-c s=
  (setq rg-executable "rg") ; defaults to (executable-find "rg") which can be wrong on Windows
  (rg-enable-menu)          ; start w/ C-c s p, "rg-project"

  ;; (setq rg-command-line-flags '("--hidden" "--follow"))

  ;; rg-mode binds C-n and C-p to go to next/prev file rather than by line
  ;; which is a bit jarring.
  ;; (define-key rg-mode-map (kbd "C-n") nil)
  ;; (define-key rg-mode-map (kbd "C-p") nil)

  ;; 버퍼가 열리면 포커스를 그쪽으로 이동시킨다.
  ;; 이거 없으면 생각보다 귀찮아진다.
  (add-hook 'rg-mode-hook (lambda () (switch-to-buffer-other-window "*rg*")))

  (rg-define-search rg-files-without-match
    :format literal
    :flags ("--files-without-match")
    :menu ("Custom" "@" "Files without matches"))

  (rg-define-search rg-search-all       ; C-c s a: search all in project
    "Search all files in project with rg"
    :files "everything"
    :dir project
    :menu ("Search" "a" "All in project")
    )
  (rg-define-search rg-search-dir       ; C-c s d: search in current dir
    "Search in current dir with rg"
    :files "everything"
    :dir current
    :menu ("Search" "C" "All in current dir")
    )
  )

;;;; affe

;; affe-grep: Filters the content of all text files in the current directory
;; affe-find: Filters the file paths of all files in the current directory
(use-package affe!
  :defer 5
  :config
  ;; (consult-customize affe-grep :preview-key (kbd "M-."))
  (defvar affe-orderless-regexp "")
  (defun affe-orderless-regexp-compiler (input _type)
    (setq affe-orderless-regexp (orderless-pattern-compiler input))
    (cons affe-orderless-regexp
          (lambda (str) (orderless--highlight affe-orderless-regexp str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  )

;;;; fzf fuzzy find

;; git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
(use-package! fzf
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;;fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        ;; fzf/position-bottom t
        fzf/window-height 15))

;;;; expand-region

(use-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  ;; Easily navigate sillycased words
  (global-subword-mode +1)
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"
        expand-region-subword-enabled t))


;;;; separedit

;; ~/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/writing/config.el
(use-package! separedit
  :defer t
  :commands (separedit separedit-dwim)
  :init
  (map! :map prog-mode-map :inv "C-c '"
        (cmd! () (cond
                  ((bound-and-true-p org-src-mode) (org-edit-src-exit))
                  ((eq major-mode 'separedit-double-quote-string-mode) (separedit-commit))
                  (t (separedit-dwim)))))
  (map! :map (separedit-double-quote-string-mode-map
              separedit-single-quote-string-mode-map)
        :inv "C-c '" #'separedit-commit)
  (map! :map minibuffer-local-map "C-c '" #'separedit)
  :config
  (setq separedit-default-mode 'markdown-mode))

;;;; youtube-sub-extractor

;; agzam
(use-package! youtube-sub-extractor
  :commands (youtube-sub-extractor-extract-subs)
  :config
  (map! :map youtube-sub-extractor-subtitles-mode-map
        :desc "copy timestamp URL" :n "RET" #'youtube-sub-extractor-copy-ts-link
        :desc "browse at timestamp" :n "C-c C-o" #'youtube-sub-extractor-browse-ts-link
        :n "q" #'kill-buffer-and-window))

;;; :tools writing


;;;; markdown-mode

;; agzam
(after! (:or markdown-mode chatgpt-shell-mode)
  (load! "+markdown")

  (setq-default markdown-enable-math t)

  (after! evil
    ;; (advice-add #'evil-ex-start-word-search :around #'evil-ex-visual-star-search-a)
    (advice-add 'evil-yank :around #'maybe-yank-and-convert-a))

  (map! :map (markdown-mode-map
              chatgpt-shell-mode-map)
        (:localleader
         (:prefix ("s" . "wrap")
                  "<" #'markdown-wrap-collapsible
                  "C" #'markdown-wrap-code-clojure
                  "c" #'markdown-wrap-code-generic))))

(after! markdown-mode
  (setq markdown-hide-urls nil) ; must
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-display-remote-images t)
  (setq markdown-list-item-bullets '("◦" "-" "•" "–"))

  (advice-add
   'markdown-fontify-list-items :override
   (lambda (last)
     (when (markdown-match-list-items last)
       (when (not (markdown-code-block-at-point-p (match-beginning 2)))
         (let* ((indent (length (match-string-no-properties 1)))
                (level (/ indent markdown-list-indent-width))
                ;; level = 0, 1, 2, ...
                (bullet (nth (mod level (length markdown-list-item-bullets))
                             markdown-list-item-bullets)))
           (add-text-properties
            (match-beginning 2) (match-end 2) '(face markdown-list-face))
           (cond
            ;; Unordered lists
            ((string-match-p "[\\*\\+-]" (match-string 2))
             (add-text-properties
              (match-beginning 2) (match-end 2) `(display ,bullet)))
            ;; Definition lists
            ((string-equal ":" (match-string 2))
             (let ((display-string
                    (char-to-string (markdown--first-displayable
                                     markdown-definition-display-char))))
               (add-text-properties (match-beginning 2) (match-end 2)
                                    `(display ,display-string)))))))
       t)))

  (add-hook 'markdown-mode-hook #'visual-line-mode)
  ;; (add-hook 'markdown-mode-hook #'toggle-text-mode-auto-fill)

  (add-hook
   'markdown-mode-hook
   (lambda ()
     "Beautify Markdown em-dash and checkbox Symbol"
     (push '("--" . "—") prettify-symbols-alist)
     (push '("->" . "→" ) prettify-symbols-alist)
     (push '("=>" . "⟹") prettify-symbols-alist)
     (prettify-symbols-mode)))

  ;; Plain text (text-mode)
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

  (progn
    (define-key markdown-mode-map (kbd "<f3>") 'markdown-toggle-markup-hiding)
    (define-key markdown-mode-map (kbd "<f4>") 'markdown-toggle-inline-images)

    (evil-define-key '(insert) markdown-mode-map (kbd "C-u") 'undo-fu-only-undo)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-r") 'undo-fu-only-redo)

    (evil-define-key '(insert normal visual) markdown-mode-map (kbd "C-<up>") 'markdown-backward-paragraph) ;; same as org-mode
    (evil-define-key '(insert normal visual) markdown-mode-map (kbd "C-<down>") 'markdown-forward-paragraph)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-n") 'markdown-outline-next)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-p") 'markdown-outline-previous)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-n") 'next-line)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-p") 'previous-line)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-j") 'markdown-outline-next-same-level)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-k") 'markdown-outline-previous-same-level)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-S-p") 'markdown-up-heading)
    (evil-define-key '(normal visual) markdown-mode-map "zu" 'markdown-up-heading) ;; same with evil-collection outline
    )
  )

;;;; palimpsest

;; M-x palimpsest-move-region-to-bottom
;; M-x palimpsest-move-region-to-top
;; M-x palimpsest-move-region-to-trash
(use-package! palimpsest
  :after org
  :hook (org-mode . palimpsest-mode))

;;;; centered-cursor-mode

(use-package! centered-cursor-mode
  :commands (centered-cursor-mode
             global-centered-cursor-mode)
  :init
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               mouse-set-region
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region))
  )



;;;; olivetti

(use-package! olivetti
  :after org
  :custom
  ;; (olivetti-body-width 0.7) ; nil
  (olivetti-minimum-body-width 90) ; for compatibility fill-column 80
  (olivetti-recall-visual-line-mode-entry-state t))

;;;; logos

(use-package! logos
  :defer 2
  :commands (logos-focus-mode)
  :init
  ;; If you want to use outlines instead of page breaks (the ^L):
  (setq logos-outlines-are-pages t)
  ;; This is the default value for the outlines:
  ;; (setq logos-outline-regexp-alist
  ;;       `((emacs-lisp-mode . "^;;;+ ")
  ;;         (org-mode . "^\\*+ +")
  ;;         (markdown-mode . "^\\#+ +")
  ;;         (t . ,(if (boundp 'outline-regexp) outline-regexp logos--page-delimiter))))

  (setq logos-outline-regexp-alist `((emacs-lisp-mode . "^;;;+ ")
                                     (org-mode . "^\\*+ +")
                                     (markdown-mode . "^\\#+ +")))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-cursor nil)
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-header-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch nil) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)
  (setq logos-outlines-are-pages t)
  :config
  ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
  ;; I always load them before other relevant potentially packages.
  ;; (add-hook 'modus-themes-after-load-theme-hook #'logos-update-fringe-in-buffers)
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    )
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top
  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top)

  ;; Also consider adding keys to `logos-focus-mode-map'.  They will take
  ;; effect when `logos-focus-mode' is enabled.
  )


;;;; org-web-tools

(use-package! org-web-tools)

;;;; org-supertag

(use-package! org-supertag
  :after org
  ;; :config (org-supertag-setup)
  )

;;;; corg

(use-package! corg
  :after org
  :config
  (add-hook 'org-mode-hook #'corg-setup))

;;;; edit-indirect

;; agzam
(after! edit-indirect
  ;; I want indirect buffers to always appear on the right side of current window
  (add-to-list
   'display-buffer-alist
   `("\\*edit-indirect .*\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right))))

;;;; quarto-mode : jupyter alternates with polymode

;; polymode
;; (use-package! quarto-mode
;;   :mode (("\\.Rmd" . poly-quarto-mode)))
(use-package!  quarto-mode
  :mode (("\\.[qQ][mM][dD]" . poly-quarto-mode)))

;;;; checkers : spelling

;;;;:checkers (spell +flyspell)

;; Default spelling dictionary is English
(unless IS-TERMUX
  ;; (after! flyspell
  (require 'ispell)
  (setq ispell-dictionary "english")
  (setq ispell-personal-dictionary (concat user-dotemacs-dir "assets/aspell.en.pws"))

  (remove-hook! '(org-mode-hook
                  markdown-mode-hook
                  TeX-mode-hook
                  rst-mode-hook
                  mu4e-compose-mode-hook
                  message-mode-hook
                  ;; git-commit-mode-hook
                  )
    #'flyspell-mode)
  )

;;;; jinx - 통합하자

;; 2024-05-21 성능이 빠르다면 한글을 이걸로 써야 한다.
(use-package! jinx
  :config
  (setq jinx-delay 0.5) ; default 0.2
  ;; (dolist (hook '(text-mode-hook conf-mode-hook)) ; prog-mode-hook
  ;;   (add-hook hook #'jinx-mode))


  ;; (add-hook 'org-mode-hook #'jinx-mode)
  ;; (add-hook 'prog-mode-hook #'jinx-mode) ; 주석

  ;; 영어만 검사
  ;; (setenv "LANG" "en_US.UTF-8")
  ;; (setq jinx-languages "en")

  ;; (add-hook 'jinx-mode-hook (lambda () (setq-local jinx-languages "en")))
  ;; (setq jinx-exclude-regexps
  ;;       '((emacs-lisp-mode "Package-Requires:.*$")
  ;;         (t "[가-힣]" "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))

  ;; 1) 개인사전 연결해줄 것
  ;; ~/.config/enchant/en.dic -> /home/junghan/.aspell_en_personal
  ;; ko.dic -> /home/junghan/.aspell_en_personal 뭐지? 이렇게 들어가네?!
  ;; 한글 일 경우는 ko.dic 을 ~/.hunspell_ko_personal 으로 심볼링 링크 해줄 것
  ;; 되는데로 하자
  ;; 2) enchant.ordering aspell 로 변경할 것
  ;; 	*:nuspell,aspell,hunspell
  ;; en_AU:aspell,hunspell,nuspell
  ;; en_CA:aspell,hunspell,nuspell
  ;; en_GB:aspell,hunspell,nuspell
  ;; en_US:aspell,hunspell,nuspell
  ;; en:aspell,hunspell,nuspell

  ;; 1) 영어 제외 : 한글만 검사
  ;; 2) 한글 영어 선택하도록 제공
  (setq jinx-languages "ko")
  ;; (setq jinx-exclude-regexps
  ;;       '((t "[A-Za-z]" "[']")))
  (setq jinx-exclude-regexps
        '((emacs-lisp-mode "Package-Requires:.*$")
          (t "[A-Za-z]" "[']" "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))

  ;; 아래는 기본인데 일단 해보면서 보자.
  ;; "[A-Z]+\\>"         ;; Uppercase words
  ;; "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
  ;; "[a-z]+://\\S-+"    ;; URI
  ;; "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
  ;; "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
  ;; "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")) ;; Local variables

  ;; C-; embark-dwim
  ;; C-: 점 앞의 철자가 틀린 단어에 대한 수정을 트리거합니다.
  ;; C-u M-$전체 버퍼에 대한 수정을 트리거합니다.
  (keymap-global-set "C-:" #'jinx-correct)
  (keymap-global-set "C-M-$" #'jinx-languages)

  ;; 'z =' ispell-word
  (map! :map (org-mode-map
              markdown-mode-map
              text-mode-map
              chatgpt-shell-mode-map)
        :n ", SPC" #'jinx-correct
        ;; :n ", 4" #'jinx-autocorrect-last+
        ;; :i ", ," #'insert-comma
        ;; :i ", m" #'jinx-autocorrect-last+
        ;; :i ", SPC" (cmd! (jinx-autocorrect-last+ :prompt))
        )

  ;; /tecosaur-dot-doom/config.org
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  (after! ispell
    (global-set-key [remap ispell-word] #'jinx-correct))
  (after! evil-commands
    (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
    (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))
  ;; I prefer for `point' to end up at the start of the word,
  ;; not just after the end.
  ;; (advice-add 'jinx-next :after (lambda (_) (left-word)))
  )

;;;; org-glossary

(use-package! org-glossary
  :after org
  :init
  (setq org-glossary-idle-update-period 1.0) ; 0.5
  (setq org-glossary-autodetect-in-headings t) ; 2024-06-13 new
  ;; :hook (org-mode . org-glossary-mode)
  :config
  (setq org-glossary-collection-root (concat org-directory "dict/"))
  ;; (setq org-glossary-global-terms "global")

  (define-key org-mode-map (kbd "C-}") 'org-glossary-insert-term-reference)
  (define-key org-mode-map (kbd "C-{") 'org-glossary-create-definition)
  (define-key org-mode-map (kbd "C-\"") 'org-glossary-create-definition)
  ;; (setq org-glossary-automatic nil) ;; disable auto-export
  )

;; sample from tecosaur/org-glossary
;; (defun +org-glossary--latex-cdef (backend info term-entry form &optional ref-index plural-p capitalized-p extra-parameters)
;;   (org-glossary--export-template
;;    (if (plist-get term-entry :uses)
;;        "*%d*\\emsp{}%v\\ensp{}@@latex:\\labelcpageref{@@%b@@latex:}@@\n"
;;      "*%d*\\emsp{}%v\n")
;;    backend info term-entry ref-index
;;    plural-p capitalized-p extra-parameters))
;; (org-glossary-set-export-spec
;;  'latex t
;;  :backref "gls-%K-use-%r"
;;  :backref-seperator ","
;;  :definition-structure #'+org-glossary--latex-cdef)

;;;; d2 / mermaid / plantuml

(after! org
  ;; M-x plantuml-download-jar
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)

  ;; (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
  ;;       org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

  ;; sudo apt-get install ditaa
  (require 'ob-ditaa)
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

;; Mermaid is a tool for drawing systems diagrams.
;; *NOTE*: The variable =ob-mermaid-cli-path= needs to be set in the config (because it will change from system to system).
;; - npm install -g @mermaid-js/mermaid-cli
;; - mmdc -i input.mmd -o output.svg

(use-package! mermaid-mode
  :config
  (map! :localleader
        :map (mermaid-mode-map)
        "c" 'mermaid-compile
        "f" 'mermaid-compile-file
        "b" 'mermaid-compile-buffer
        "r" 'mermaid-compile-region
        "b" 'mermaid-open-browser
        "d" 'mermaid-open-doc))

;; (use-package! d2-mode
;;   :mode "\\.d2\\'"
;;   :config
;;   (setq d2-output-format ".png")
;;   (map! :localleader
;;         :map (d2-mode-map)
;;         "h" #'d2-open-doc
;;         "v" #'d2-view-current-svg
;;         "o" #'d2-open-browser
;;         "c" #'d2-compile
;;         "f" #'d2-compile-file
;;         "b" #'d2-compile-buffer
;;         "r" #'d2-compile-region
;;         "F" #'d2-compile-file-and-browse
;;         "B" #'d2-compile-buffer-and-browse
;;         "R" #'d2-compile-region-and-browse
;;         "o"  #'d2-open-browser
;;         "v"  #'d2-view-current-svg
;;         "h"  #'d2-open-doc))

;;;; ob-mermaid / ob-d2 / ox-reveal

(use-package! ob-mermaid
  :after org
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

;; (use-package! ob-d2
;;   :after org
;;   :defer 3
;;   :config
;;   (add-to-list 'org-babel-load-languages '(d2 . t)))

;;;; orgabilize

(use-package! orgabilize
  :after org
  :defer 10
  :config
  (setq orgabilize-org-archive-directory (concat org-directory "import/")))

;;;; org-pandoc-import

(use-package! org-pandoc-import
  :defer t
  :after org
  :commands (org-pandoc-import-as-org)
  :config
  (require 'org-pandoc-import))

;; ("C" org-pandoc-import-csv-as-org "Import CSV")

;;;; side-notes

(use-package! side-notes
  :init
  (add-hook 'side-notes-hook #'visual-line-mode) ; Good
  )

;;;; immersive-translate

(use-package! immersive-translate
  :if window-system
  :init
  ;; (setq immersive-translate-auto-idle 2.0) ; default 0.5
  ;; wget git.io/trans; chmod +x trans; sudo mv trans /usr/local/bin
  ;; ko         Korean                         한국어
  (setq immersive-translate-backend 'trans)
  (setq immersive-translate-trans-target-language "ko")
  :config
  (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  (add-hook 'Info-mode-hook #'immersive-translate-setup)
  (add-hook 'help-mode-hook #'immersive-translate-setup)
  (add-hook 'helpful-mode-hook #'immersive-translate-setup)
  ;; (add-hook 'nov-mode-hook #'immersive-translate-setup)
  )

;;;; redacted

(use-package! redacted
  :commands (redacted-mode))

;;;; hypothesis

;; M-x hypothesis-to-org downloads the 200 newest notations and inserts
;; them into a temporary org-mode buffer. M-x hypothesis-to-archive
;; imports notations into hypothesis-archive. It will import up to 200
;; notations but will only import notations made after the last import.
(use-package! hypothesis
  :commands hypothesis-to-org hypothesis-to-archive
  :config
  (setq hypothesis-username user-hypothesis-username)
  (setq hypothesis-token user-hypothesis-token)
  (setq hypothesis-quote-prefix "#+begin_example")
  (setq hypothesis-quote-sufix "#+end_example")
  )

(after! hypothesis
  (setq hypothesis-archive (my/org-links-file)))

;;;; guess-language

(use-package! guess-language
  :demand t
  :init
  (setq guess-language-langcodes
        '((en . ("en" "English" "🇬🇧" "English"))
          (ko . ("ko" "Korean" "🇰🇷" "Korean"))))
  (setq guess-language-languages '(ko en))
  (setq guess-language-min-paragraph-length 35)
  )

;;;; txl

;; deeplx: deepl for free
(progn
  (setq use-deeplx t)
  (setq deeplx-url "http://localhost:1188/v2/translate")
  (setq deeplx-key "fd86e4d6-8227-995a-4258-b61a7ca1efcc:fx")

  (use-package! txl
    :defer 2
    :config
    (setq txl-languages '(EN-US . KO)) ; using guess-language
    (setq txl-deepl-split-sentences nil)

    (defun my/txl-deepl-toggle ()
      (interactive)
      (if (eq use-deeplx t)
          (progn
            (setq txl-deepl-api-url "https://api-free.deepl.com/v2/translate")
            (setq txl-deepl-api-key user-deepl-api-key)
            (setq use-deeplx nil))
        (progn  ; use deeplx for free
          (setq txl-deepl-api-url deeplx-url)
          (setq txl-deepl-api-key deeplx-key)
          (setq use-deeplx t))))

    ;; default use deeplx
    (setq txl-deepl-api-url deeplx-url)
    (setq txl-deepl-api-key deeplx-key)

    (global-set-key (kbd "M-g 0") 'txl-translate-insert)
    (global-set-key (kbd "M-v") 'txl-translate-insert)
    (after! org
      (define-key org-mode-map (kbd "M-v") 'txl-translate-insert))
    (after! centered-cursor-mode
      (define-key ccm-map (kbd "M-v") 'txl-translate-insert))
    )
  )

;; (setq txl-deepl-split-sentences nil
;;       txl-deepl-preserve-formatting nil)

;; (defun txl-translate-insert (&optional prefix-arg)
;;   (interactive "P")
;;   (require 'guess-language)
;;   (setq txl-source-buffer (current-buffer))
;;   (let* ((route (if prefix-arg
;;                     (list (txl-other-language) (txl-guess-language))
;;                   (list (txl-other-language))))
;;          (translation (apply 'txl-translate route)))
;;     (with-current-buffer txl-source-buffer
;;       (unless (derived-mode-p 'text-mode)
;;         (text-mode))
;;       (txl-insert-region-or-paragraph translation)
;;       (unfill-paragraph)
;;       )
;;     (display-buffer txl-source-buffer)
;;     )
;;   )



;;;; TODO focus

(use-package! focus
  :after org
  :config
  (add-to-list 'focus-mode-to-thing '(org-mode . paragraph)))

;;; :workspace

;;;; outli

(use-package! outli
  :defer 1
  :init (setq outli-speed-commands nil)
  :config
  ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))

  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
  (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))

  (add-hook 'prog-mode-hook 'outli-mode) ; not markdown-mode!
  ;; (add-hook 'org-mode-hook 'outli-mode)
  )

;;;; Projectile

;; External tools required to make projectile fly! fd, ag, rg
;; evil-dot-doom/modules/custom/projects/config.el

(after! projectile
  ;; Disable projectile cache - saves requirement to invalidate cache when moving files
  (setq projectile-enable-caching nil)

  ;; create missing test files
  (setq projectile-create-missing-test-files t)

  ;; add clojure specific folders to be ignored by projectile
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '(".clj-kondo"
                  ".cpcache"
                  "tmp" "del"
                  ".local")))

  ;; Search https://discourse.doomemacs.org/ for example configuration
  (setq projectile-ignored-projects
        (list "~/" "/tmp" (expand-file-name "straight/repos" doom-local-dir)))

  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar
         (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

  ;; Define a project path to discover projects using SPC Tab D
  ;; https://docs.projectile.mx/projectile/usage.html
  ;; (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
  ;; (setq projectile-project-search-path '(("~/code" . 2) ("~/git" . 1)))

  ;; direct projectile to look for code in a specific folder.
  (setq projectile-project-search-path '("~/git"))

  (map! :leader
        :desc "Toggle Impl & Test" "pt" #'projectile-toggle-between-implementation-and-test
        ;; :desc "List todos" "pl" #'magit-todos-list
        :desc "See project root dir" "p-" #'projectile-dired
        :desc "Ripgrep" "pG" #'projectile-ripgrep)

  ;; stop $HOME from being recognizes as a project root
  ;; (setq projectile-project-root-files-bottom-up
  ;;       (remove ".git" projectile-project-root-files-bottom-up))
  )

;;;; treemacs

(after! treemacs
  ;; (setq treemacs-follow-mode t)
  (setq
   treemacs-position 'left
   treemacs-width 45
   treemacs-imenu-scope 'current-project
   treemacs-indentation 1
   treemacs-space-between-root-nodes nil ; spacing in treemacs views
   ;; treemacs-fringe-indicator-mode nil ; default t
   )

  (after! winum
    ;; `0', `M-0' and `C-x w 0' are bound to `winum-select-window-0-or-10'
    ;; (define-key winum-keymap [remap winum-select-window-0-or-10] #'treemacs-select-window)
    (define-key winum-keymap
                [remap winum-select-window-9] #'treemacs-select-window) ; spacemacs/switch-to-minibuffer-window

    ;; replace the which-key name
    (push '((nil . "winum-select-window-9") ; winum-select-window-0-or-10
            .
            (nil . "treemacs-select-window"))
          which-key-replacement-alist)

    (dolist (n (number-sequence 1 5))
      (add-to-list
       'winum-ignored-buffers
       (format "%sFramebuffer-%s*" treemacs--buffer-name-prefix n))))

  (winum-mode +1)
  )

;;;; tabgo

(use-package! tabgo)

;;; :tools magit vc

;;;; magit

;; Location of developer tokens - default ~/.authinfo
;; Use XDG_CONFIG_HOME location or HOME
;; Optional:
(setq auth-source-cache-expiry nil)   ; default is 7200 (2h)
(setq auth-sources (list (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg") "~/.authinfo.gpg"))

;; hilsner
(setq magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      )

;; (setq evil-collection-magit-want-horizontal-movement t) ; default nil

;; Location of Git repositories
;; define paths and level of sub-directories to search
(setq magit-repository-directories
      '(("~/doomemacs-git/" . 0)
        ("~/.doom.d/" . 0) ("~/git/" . 1) ("~/mydotfiles/" . 0)
        ;; ("~/sync/code/" . 2)
        ))

(after! magit
  ;; Use Emacs as $EDITOR (or $GIT_EDITOR) for git commits messages
  ;; when using git commit on the command line
  ;; (global-git-commit-mode t)

  ;; Commit message checks
  ;; ~/.config/emacs/modules/emacs/vc/config.el
  ;; - checks for overlong-summary-line non-empty-line
  ;; (setq git-commit-summary-max-length 50
  ;;       git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))

  ;; Enforce git commit conventions.
  ;; See: http://chris.beams.io/posts/git-commit
  (require 'git-commit)
  (setq git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (evil-set-initial-state 'git-commit-mode 'insert)

  (setq git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (setq
   ;; Highlight specific characters changed
   magit-diff-refine-hunk 'all
   ;; Show Libravatar of commit author
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;; Number of topics displayed (issues, pull requests)
  ;; open & closed, negative number for closed topics
  ;; or `forge-toggle-closed-visibility'
  ;; set closed to 0 to never show closed issues
  ;; (setq  forge-topic-list-limit '(100 . 0))
  (setq forge-topic-list-limit '(100 . -10))
  ;; GitHub user and organization accounts owned
  ;; used by @ c f  to create a fork
  (setq forge-owned-accounts '(("junghan0611" "junghanacs")))

  ;; Blacklist specific accounts, over-riding forge-owned-accounts
  ;; (setq forge-owned-blacklist
  ;;       '(("bad-hacks" "really-bad-hacks")))
  ;; End of Version Control configuration

  ;; 2025-01-27 reset transient-display-buffer-action for gptel-menu
  (setq transient-display-buffer-action
        '(display-buffer-in-side-window
          (side . bottom)
          (dedicated . t)
          (inhibit-same-window . t)))
  )

;;;; git-commit : categories

;; from /doom/cashpw-dotfiles-node/config/doom/config-personal.org

(progn
  (require 'git-commit)

  (defgroup cashpw/source-control nil
    "Source control."
    :group 'cashpw)

  (defcustom cashpw/source-control--commit-categories
    '(("Fix" . (:symbol "🐛"
                :shortcode ":bug:"))
      ("UI" . (:symbol "💄"
               :shortcode ":lipstick:"))
      ("UX" . (:symbol "💄"
               :shortcode ":lipstick:"))
      ("Add" . (:symbol "✨"
                :shortcode ":sparkles:"))
      ("Feature" . (:symbol "✨"
                    :shortcode ":sparkles:"))
      ("Document" . (:symbol "📝"
                     :shortcode ":memo:"))
      ("Typo" . (:symbol "✏️"
                 :shortcode ":pencil2:"))
      ("Refactor" . (:symbol "♻"
                     :shortcode ":recycle:"))
      ("Rollout" . (:symbol "🚀"
                    :shortcode ":rocket:"))
      ("Launch" . (:symbol "🚀"
                   :shortcode ":rocket:"))
      ("Version" . (:symbol "🔖"
                    :shortcode ":bookmark:"))
      ("Release" . (:symbol "🔖"
                    :shortcode ":bookmark:"))
      ("Deploy" . (:symbol "🚀"
                   :shortcode ":rocket:"))
      ("Delete" . (:symbol "🔥"
                   :shortcode ":fire:"))
      ("Remove" . (:symbol "🔥"
                   :shortcode ":fire:"))
      ("Test" . (:symbol "✅"
                 :shortcode ":white_check_mark:")))
    "Alist of commit categories and extras."
    :group 'cashpw/source-control
    :type 'string)

  (defun cashpw/source-control--read-commit-category ()
    "Return commit noun as selected by user."
    (let ((category (completing-read "Category: "
                                     cashpw/source-control--commit-categories
                                     ;; predicate
                                     nil
                                     ;; require-match
                                     t)))
      (assoc category
             cashpw/source-control--commit-categories)))

  (defun cashpw/source-control--commit--section (title content)
    "Return formatted section for a commit message."
    (s-lex-format "## ${title}

${content}"))

  (defun cashpw/source-control--commit--build-message ()
    "Return commit message template."
    (let* ((category (cashpw/source-control--read-commit-category))
           (emoji (plist-get (cdr category) :symbol))
           ;; (what-section (cashpw/source-control--commit--section "What does this change?"
           ;;                                                       "1. TODO"))
           ;; (why-section (cashpw/source-control--commit--section "Why make these changes?"
           ;;                                                      "TODO"))
           )
      (s-lex-format "${emoji} ")))

  (defun cashpw/source-control--commit--insert-message ()
    "Insert my commit message template."
    (insert (cashpw/source-control--commit--build-message)))

  (add-hook! 'git-commit-setup-hook
             'cashpw/source-control--commit--insert-message)
  )

;;;; magit-todos

;; Show project TODO lines in Magit Status
(use-package! magit-todos
  :after magit)
;; :hook (magit-mode . magit-todos-mode)

;;;; git-cliff

(use-package! git-cliff
  :defer t
  :after (magit transient)
  :custom (git-cliff-enable-examples t)
  :config
  ;; git-cliff-extra-path : directory storing user defined presets and templates.
  ;; Integrate to `magit-tag'
  (with-eval-after-load 'magit-tag
    (transient-append-suffix
      'magit-tag '(1 0 -1) '("c" "changelog" git-cliff-menu))))

;;;; consult-git-log-grep

(use-package! consult-git-log-grep
  :after magit
  :defer t
  :custom (consult-git-log-grep-open-function #'magit-show-commit)
  :bind (("C-c K" . consult-git-log-grep)))

;;;; gist

(use-package! gist
  :defer t
  :config
  ;; view your Gist using `browse-url` after it is created
  (setq gist-view-gist t))

;;;; consult-gh

(use-package! consult-gh
  :after consult
  :defer t
  :commands (consult-gh-search-repos
             consult-gh-search-code
             consult-gh-search-prs
             consult-gh-search-issues
             consult-gh-pr-list
             consult-gh-issue-list
             consult-gh-default-repos
             consult-gh-find-file
             consult-gh-repo-clone
             consult-gh-repo-fork)
  :custom
  (consult-gh-repo-maxnum 30) ;;set max number of repos to 30
  (consult-gh-issues-maxnum 100) ;;set max number of issues to 100
  (consult-gh-show-preview t)
  (consult-gh-preview-key "M-m")
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-prioritize-local-folder 'suggest)
  (consult-gh-preview-buffer-mode 'org-mode) ;; show previews in org-mode

  (consult-gh-repo-action #'consult-gh--repo-browse-files-action) ;;open file tree of repo on selection
  (consult-gh-issue-action #'consult-gh--issue-view-action) ;;open issues in an emacs buffer
  (consult-gh-pr-action #'consult-gh--pr-view-action) ;;open pull requests in an emacs buffer
  (consult-gh-code-action #'consult-gh--code-view-action) ;;open files that contain code snippet in an emacs buffer
  (consult-gh-file-action #'consult-gh--files-view-action) ;;open files in an emacs buffer
  :config
  (require 'consult-gh-transient)

  (after! projectile
    (add-hook! 'consult-gh-repo-post-clone-hook
      (defun cae-projectile-discover-projects-in-search-path-h (&rest _)
        (projectile-discover-projects-in-search-path))))

  ;; set the default folder for cloning repositories, By default Consult-GH will confirm this before cloning
  (setq consult-gh-default-clone-directory "~/git/default/")
  (setq consult-gh-default-save-directory "~/Downloads")

  (dolist (repo '("junghan0611" "junghanacs" "agzam" "minad" "alphapapa"
                  "LemonBreezes" "protesilaos" "armindarvish"
                  "doomemacs" "tecosaur"))
    (add-to-list 'consult-gh-favorite-orgs-list repo))

  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)

  ;; Install `consult-gh-embark' for embark actions
  (use-package consult-gh-embark
    :config
    (consult-gh-embark-mode +1))

  ;; Install `consult-gh-forge' for forge actions
  (use-package consult-gh-forge
    :config
    (consult-gh-forge-mode +1)
    (setq consult-gh-forge-timeout-seconds 20))
  )

;;; :lang org

;;;; doom packages
;;;;; org-noter

(after! org
  (require 'org-noter)
  (setq org-noter-notes-search-path (list (concat user-org-directory "notes/")))
  (setq org-noter-default-notes-file-names "20240902T165404--org-noter.org")
  )

;;;;; format-on-save-disabled-modes

(setq +format-on-save-disabled-modes
      '( ;; emacs-lisp-mode  ; elisp's mechanisms are good enough
        ;; org-mode
        org-msg-edit-mode
        sql-mode ; sqlformat is currently broken
        tex-mode ; latexindent is broken
        latex-mode))

;;;;; org-mode-hook for ispell

(add-hook 'org-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t)))

;;;;; org-contacts-files

(setq org-contacts-files org-user-contacts-files)

;;;;; TODO org-cliplink

(progn
  (require 'org-cliplink)
  (setq org-cliplink-max-length 72)
  (setq org-cliplink-ellipsis "-")

  ;; from ohyecloudy
  (defun my/org-cliplink ()
    (interactive)
    (org-cliplink-insert-transformed-title
     (org-cliplink-clipboard-content) ;take the URL from the CLIPBOARD
     #'my-org-link-transformer))

  (defun my-org-link-transformer (url title)
    (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
           (host-url
            (replace-regexp-in-string "^www\\." "" (url-host parsed-url)))
           (clean-title
            (cond
             ;; if the host is github.com, cleanup the title
             ((string= (url-host parsed-url) "github.com")
              (replace-regexp-in-string
               "^/" "" (car (url-path-and-query parsed-url))))
             ;; otherwise keep the original title
             (t
              (my-org-cliplink--cleansing-site-title title))))
           (title-with-url (format "%s - %s" clean-title host-url)))
      ;; forward the title to the default org-cliplink transformer
      (org-cliplink-org-mode-link-transformer url title-with-url)))

  (defun my-org-cliplink--cleansing-site-title (title)
    (let ((result title)
          (target-site-titles
           '(" - 위키백과"
             " - Wikipedia"
             " - PUBLY"
             " - YES24"
             "알라딘: "
             " : 클리앙"
             " - YouTube")))
      (dolist (elem target-site-titles)
        (if (string-match elem result)
            (setq result (string-replace elem "" result))
          result))
      result))

  ;; 마지막에 host 를 붙이고 싶어서 link transformer 함수를 짰다. =title -
  ;; ohyecloudy.com= 식으로 org link 를 만든다.
  (define-key org-mode-map [remap org-cliplink] 'my/org-cliplink)
  )

;;;;; ob-jupyter

;; Workaround; see https://github.com/nnicandro/emacs-jupyter/issues/380#issuecomment-1014026589
(after! ob-jupyter
  (defun jupyter-ansi-color-apply-on-region (begin end)
    (ansi-color-apply-on-region begin end t)))

;;;; additional packages

;;;;; DONT org-modern

;; (org-modern-tag t)
;; (org-modern-todo nil)
;; (org-modern-table nil)
;; (org-modern-keyword nil)
;; (org-modern-timestamp nil)
;; (org-modern-priority nil)
;; (org-modern-checkbox nil)
;; (org-modern-block-name nil)
;; (org-modern-keyword nil)
;; (org-modern-footnote nil)
;; (org-modern-internal-target nil)
;; (org-modern-radio-target nil)
;; (org-modern-progress nil)
;; :config
;; (setq org-modern-todo-faces
;;       '(("TODO" :inverse-video t :inherit org-todo)
;;         ("PROJ" :inverse-video t :inherit +org-todo-project)
;;         ("STRT" :inverse-video t :inherit +org-todo-active)
;;         ("[-]"  :inverse-video t :inherit +org-todo-active)
;;         ("HOLD" :inverse-video t :inherit +org-todo-onhold)
;;         ("WAIT" :inverse-video t :inherit +org-todo-onhold)
;;         ("[?]"  :inverse-video t :inherit +org-todo-onhold)
;;         ("KILL" :inverse-video t :inherit +org-todo-cancel)
;;         ("NO"   :inverse-video t :inherit +org-todo-cancel)))
;; (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo))

;; (use-package! org-modern
;;   :after org
;;   :config
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil ; t
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t ; nil
;;    org-pretty-entities t ; nil
;;    org-agenda-tags-column 0)

;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;   (require 'org-modern-indent)
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
;;   )

;;;;; org-download

(use-package! org-download
  :after org
  :hook (;; (dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :commands (org-download-enable
             org-download-yank
             org-download-screenshot)
  :config
  (setq-default org-download-heading-lvl nil)
  (setq org-download-method 'directory) ; doom 'attach
  (setq-default org-download-image-dir (concat org-directory "screenshot" )) ;; share all devieces
  (setq org-download-display-inline-images nil)
  (setq org-download-timestamp"%Y%m%dT%H%M%S--") ;; denote id

  ;; #+caption: "
  ;; #+name: fig-"
  ;; #+attr_html: :width 40% :align center"
  ;; #+attr_latex: :width \\textwidth"
  (setq org-download-image-attr-list
        '("#+attr_html: :width 80% :align center"
          "#+attr_latex: :width \\textwidth"
          "#+attr_org: :width 800px"))

  ;; (defun kimim/org-download-annotate (link)
  ;;   "Annotate LINK with the time of download."
  ;;   (format "#+name: fig:%s\n#+caption: %s\n"
  ;;           (file-name-base link) (file-name-base link)))
  ;; (setq org-download-annotate-function #'kimim/org-download-annotate)
  )

;;;;; org-appear

(use-package! org-appear
  :after org
  :if window-system
  :init
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks t ;; nil
        org-appear-autoemphasis t
        org-appear-autosubmarkers t)
  (setq org-appear-trigger 'manual) ;'on-change
  :config
  (when org-appear-trigger 'manual
        (add-hook 'org-mode-hook
                  (lambda ()
                    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
                    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))
        )
  )

;;;;; org-rich-yank

(use-package! org-rich-yank
  :defer t
  :commands (org-rich-yank))

;;;;; org-transclusion

(use-package! org-transclusion
  :after org
  :defer 2
  :commands org-transclusion-mode
  :config
  (set-face-attribute 'org-transclusion-fringe nil :foreground "light green" :background "lime green")
  )

(after! org-transclusion
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode))

;;;;; org-remark

(use-package! org-remark
  :after org
  :config (setq org-remark-notes-file-name (my/org-remark-file))
  ;; (org-remark-create "red-line"
  ;;                    '(:underline (:color "magenta" :style wave))
  ;;                    '(CATEGORY "review" help-echo "Review this"))
  ;; (org-remark-create "yellow"
  ;;                    '(:underline "gold")
  ;;                    '(CATEGORY "important"))

  ;; It is recommended that `org-remark-global-tracking-mode' be enabled when
  ;; Emacs initializes. Alternatively, you can put it to `after-init-hook' as in
  ;; the comment above
  ;; (require 'org-remark-global-tracking)
  ;; (org-remark-global-tracking-mode +1)

  ;; Optional if you would like to highlight websites via eww-mode
  ;; (with-eval-after-load 'eww (org-remark-eww-mode +1))
  ;; Optional if you would like to highlight EPUB books via nov.el
  ;; (with-eval-after-load 'nov (org-remark-nov-mode +1))
  ;; Optional if you would like to highlight Info documentation via Info-mode
  ;; (with-eval-after-load 'info (org-remark-info-mode +1))
  )

;;;;; remember

(use-package! remember
  :commands remember
  :init
  (setq
   remember-notes-initial-major-mode 'org-mode
   remember-notes-auto-save-visited-file-name t)
  :config (setq remember-data-file (my/org-remember-file)))

;;;;; Repetation : org-drill

(use-package! org-drill
  :defer t
  :init
  ;; save buffers after drill sessions without prompt.
  (setq org-drill-save-buffers-after-drill-sessions-p nil)
  ;; reduce from the default 30 to make it to become a habit.
  (setq org-drill-maximum-items-per-session 10))

;;;;; TODO gnosis

;; (use-package! gnosis
;;   :defer t
;;   :bind (("C-c g" . gnosis-dashboard)))

;;;;; ox-reveal vs. org-re-reveal

(use-package! ox-reveal
  :defer t
  :commands org-reveal-export-to-html) ; org-re-reveal better

;;;;; presentations setup

(defun my/presentation-setup ()
  (hide-mode-line-mode 1)

  ;; (org-display-inline-images 1) ;; Can also use org-startup-with-inline-images

  (display-fill-column-indicator-mode -1)
  (display-line-numbers-mode -1)

  ;; A) Scale the text.  The next line is for basic scaling:
  ;; (setq text-scale-mode-amount 2)
  ;; (text-scale-mode 1)

  ;; B) This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch) ; variable-pitch
  ;;                                    (header-line (:height 2.0) variable-pitch) ; variable-pitch
  ;;                                    (org-document-title (:height 1.5) org-document-title)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block)
  ;;                                    (org-block-begin-line (:height 0.7) org-block)))
  )

(defun my/presentation-end ()
  (hide-mode-line-mode 0)

  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)

  ;; A) Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  ;; (text-scale-mode 0)

  ;; B) If you use face-remapping-alist, this clears the scaling:
  ;; (setq-local face-remapping-alist '((default fixed-pitch default)))
  )

(after! org-tree-slide

  ;; :bind (("<f8>" . 'org-tree-slide-mode)
  ;;        ("S-<f8>" . 'org-tree-slide-skip-done-toggle)
  ;;        :map org-tree-slide-mode-map
  ;;        ("<f10>" . 'org-tree-slide-move-previous-tree)
  ;;        ("<f10>" . 'org-tree-slide-move-next-tree)
  ;;        ("<f11>" . 'org-tree-slide-content))

  (add-hook 'org-tree-slide-play-hook 'my/presentation-setup)
  (add-hook 'org-tree-slide-stop-hook 'my/presentation-end)

  (setq +org-present-text-scale 2) ; 5

  (setq
   org-tree-slide-slide-in-effect nil
   org-tree-slide-activate-message "Presentation started!"
   org-tree-slide-deactivate-message "Presentation finished!"
   org-tree-slide-header nil
   org-tree-slide-breadcrumbs " > "
   org-image-actual-width nil)

  (setq org-tree-slide-skip-outline-level 4) ;; wow!!
  )

;;;;; DONT org-excalidraw

;; (use-package! org-excalidraw
;;   :after org
;;   :defer t
;;   :commands (org-excalidraw-create-drawing)
;;   :config
;;   (setq org-excalidraw-directory (concat user-org-directory "resources/excalidraw")))

;;;;; ews: emacs writing studio

(use-package! ox-epub
  :defer t
  :after org)

;; (after! ox-latex
;;   ;; Multiple LaTeX passes for bibliographies
;;   (setq org-latex-pdf-process
;;         '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;           "bibtex %b"
;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;   ;; Clean temporary files after export
;;   (setq org-latex-logfiles-extensions
;;         (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
;;                 "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
;;                 "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
;;                 "tex" "bcf")))

;;   ;; LaTeX templates
;;   (add-to-list
;;    'org-latex-classes
;;    '("crc"
;;      "\\documentclass[krantz2]{krantz}
;;         \\usepackage{lmodern}
;;         \\usepackage[authoryear]{natbib}
;;         \\usepackage{nicefrac}
;;         \\usepackage[bf,singlelinecheck=off]{caption}
;;         \\captionsetup[table]{labelsep=space}
;;         \\captionsetup[figure]{labelsep=space}
;;         \\usepackage{Alegreya}
;;         \\usepackage[scale=.8]{sourcecodepro}
;;         \\usepackage[breaklines=true]{minted}
;;         \\usepackage{rotating}
;;         \\usepackage[notbib, nottoc,notlot,notlof]{tocbibind}
;;         \\usepackage{amsfonts, tikz, tikz-layers}
;;         \\usetikzlibrary{fadings, quotes, shapes, calc, decorations.markings}
;;         \\usetikzlibrary{patterns, shadows.blur}
;;         \\usetikzlibrary{shapes,shapes.geometric,positioning}
;;         \\usetikzlibrary{arrows, arrows.meta, backgrounds}
;;         \\usepackage{imakeidx} \\makeindex[intoc]
;;         \\renewcommand{\\textfraction}{0.05}
;;         \\renewcommand{\\topfraction}{0.8}
;;         \\renewcommand{\\bottomfraction}{0.8}
;;         \\renewcommand{\\floatpagefraction}{0.75}
;;         \\renewcommand{\\eqref}[1]{(Equation \\ref{#1})}
;;         \\renewcommand{\\LaTeX}{LaTeX}"
;;      ("\\chapter{%s}" . "\\chapter*{%s}")
;;      ("\\section{%s}" . "\\section*{%s}")
;;      ("\\subsection{%s}" . "\\subsection*{%s}")
;;      ("\\subsubsection{%s}" . "\\paragraph*{%s}")))
;;   )

;;;;; tmr

(use-package! tmr
  :after embark
  :config
  (unless IS-TERMUX
    (setq tmr-sound-file
          "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"))
  (setq
   tmr-notification-urgency 'normal
   tmr-description-list 'tmr-description-history)

  (defvar tmr-action-map
    (let ((map (make-sparse-keymap)))
      (define-key map "k" #'tmr-remove)
      (define-key map "r" #'tmr-remove)
      (define-key map "R" #'tmr-remove-finished)
      (define-key map "c" #'tmr-clone)
      (define-key map "e" #'tmr-edit-description)
      (define-key map "s" #'tmr-reschedule)
      map))
  ;; (define-key global-map (kbd "M-g M-t") 'tmr-action-map)

  ;; (with-eval-after-load 'embark
  ;;   (add-to-list 'embark-keymap-alist '(tmr-timer . tmr-action-map))
  ;;   (cl-loop
  ;;    for
  ;;    cmd
  ;;    the
  ;;    key-bindings
  ;;    of
  ;;    tmr-action-map
  ;;    if
  ;;    (commandp cmd)
  ;;    do
  ;;    (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart))))
  )

;;;;; ox-leanpub

;; By default, the ox-leanpub module sets things up for exporting books in
;; Markua format. If you want to export your books in LFM format, you need to
;; additionally load the ox-leanpub-markdown exporter and tell ox-leanpub-book
;; to set up the corresponding menu entries, as follows:
(use-package! ox-leanpub
  :defer t
  :after org
  :config
  (require 'ox-leanpub-markdown)
  (org-leanpub-book-setup-menu-markdown))

;;;;; ox-quarto

(use-package! ox-quarto
  :after org)

;;;;; org-bookmarks with link

(use-package! org-bookmarks
  :defer t
  :after org
  :commands (org-bookmarks)
  :init (setq org-bookmarks-file (my/org-links-file))
  ;; :config
  ;; (org-bookmarks-add-org-capture-template t)
  ;; (org-bookmarks-add-org-capture-template)
  )

;;;;; org-rainbow-tags

(use-package! org-rainbow-tags
  :after org
  :init
  (setq org-rainbow-tags-hash-start-index 0)
  ;; (setq org-rainbow-tags-extra-face-attributes
  ;;       '(:inverse-video t :box nil :weight 'bold))
  ;; :hook (org-mode . org-rainbow-tags-mode)
  )

;;;;; org-ql

(use-package! org-ql
  :after org
  :commands org-ql-search)

;;;;; org-sliced-images

;; for smooth scroll of images in or mode
(use-package! org-sliced-images
  :after org
  :config (org-sliced-images-mode))

;;;;; org-marked-text-overview

(use-package! org-marked-text-overview
  :after org
  :bind ("M-g l" . org-marked-text-overview-mode))

;;; :tools biblio : citar

(progn
  (require 'citar)

  (setq citar-bibliography config-bibfiles)
  (setq org-cite-global-bibliography config-bibfiles)

  ;; use #+cite_export: csl apa.csl
  (setq org-cite-csl-styles-dir (concat user-org-directory ".csl"))
  (setq citar-citeproc-csl-styles-dir (concat user-org-directory ".csl"))
  ;; (setq citar-citeproc-csl-locales-dir "~/.csl/locales")
  ;; (setq citar-citeproc-csl-style "apa.csl") ; ieee.csl

  ;; (setq citar-notes-paths '("~/sync/org/bib/"))
  (setq citar-notes-paths (list (concat org-directory "bib/")))
  (setq citar-symbol-separator " ")

  ;; (setq citar-format-reference-function 'citar-citeproc-format-reference)
  (setq citar-format-reference-function 'citar-format-reference)

  (setq
   citar-templates
   '((main
      .
      ;; [${urldate:10}]
      "[${dateadded:10}] \{${datemodified:10}\} ${author editor:20} ${translator:8} (${date year issued:4}) @${=key= id:16} ${title:68} ")  ; 2024-09-12 김정한
     (suffix
      . "${shorttitle:25} ${=type=:10} ${namea:16} ${url:20} ${tags keywords:*}") ; 2024-11-17 add url
     (preview
      .
      "${title} :${year issued date:4}\n- ${author} ${translator} ${namea}\n- ${abstract}\n- ${shorttitle}") ; citar-copy-reference
     (note . "#+title: ${author translator:10}, ${title}")))
  ;; (note . "Notes on ${author:10 editor:%etal}, ${title}")

  ;; Managing Bibliographies
  ;; (bibtex-user-optional-fields
  ;;  '(("keywords" "Keywords to describe the entry" "")
  ;;    ("file" "Link to a document file." "" )))
  (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
  (add-hook 'bibtex-mode-hook 'visual-line-mode)
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'citar-history))
  )

;;; :custom PKM
;;;; EKG

;; (use-package! ekg
;;   :defer 2
;;   ;; :init (setq ekg-db-file (concat user-org-directory "ekg/ekg.db"))
;;   :commands (ekg-dispatch ekg-capture ekg-capture-url ekg-show-notes-with-all-tags)
;;   :bind
;;   (
;;    ;; ("C-c n u" . ekg-show-notes-with-all-tags)
;;    ;; ("C-c n U" . ekg-capture)
;;    (:map
;;     ekg-notes-mode-map
;;     (("<return>" . ekg-notes-open) ("C-c C-o" . org-open-at-point))))
;;   :config
;;   (setq ekg-db-file (concat org-directory "ekg/ekg.db"))
;;   (require 'ekg-auto-save)
;;   (require 'ekg-embedding)

;;   ;; (ekg-embedding-generate-on-save)
;;   ;; (require 'ekg-llm)

;;   (setq llm-warn-on-nonfree nil)
;;   (require 'llm-openai) ;; The specific provider you are using must be loaded.
;;   (let ((my-provider (make-llm-openai :key user-openai-api-key)))
;;     (setq
;;      ekg-llm-provider my-provider
;;      ekg-embedding-provider my-provider))

;;   ;; (add-to-list 'display-buffer-alist '("*EKG Capture.*\\*"
;;   ;;                                      (display-buffer-in-side-window)
;;   ;;                                      (side . right)
;;   ;;                                      (slot . 0)
;;   ;;                                      (window-width . 80)
;;   ;;                                      ))

;;   ;; (require 'llm-gemini)
;;   ;; (let ((my-provider (make-llm-gemini :key user-gemini-api-key)))
;;   ;;   (setq ekg-llm-provider my-provider
;;   ;;         ekg-embedding-provider my-provider))

;;   ;; (defun ash/capture-literature-note ()
;;   ;;   (interactive)
;;   ;;   (ekg-capture-url (ash/get-current-url) (ash/get-current-title)))

;;   ;; org-store-link 를 해야 한다. org-mode 를 사용하라!
;;   ;; 2024-04-01 아니다. 간단하게 생각해보라. 찾을 수 있게 정보 다루는게 좋을 것 같다. ekg-denote 가보자. 그냥 조직 모드로 간다.
;;   ;; (setq ekg-capture-default-mode 'markdown-mode) ; default 'org-mode

;;   ;; (setq ekg-metadata-separator-text
;;   ;;       "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
;;   ;; (setq ekg-display-note-template
;;   ;;       "%n(id)%n(tagged)%n(titled)%n(text 50)%n(other)")
;;   (setq ekg-notes-display-images nil)

;;   ;; (setq ekg-inline-custom-tag-completion-symbols
;;   ;;       '((?@ . "person") ; default
;;   ;;         (?! . "idea") ; default
;;   ;;         ;; (?$ . "meta")
;;   ;;         ;; (?% . "docs")
;;   ;;         ;; (?\& . "project")
;;   ;;         ))

;;   (unless IS-TERMUX
;;     ;; gleek-dotfiles-ekg/core/lang/core-org.el:802
;;     ;; (setq ekg-logseq-dir (concat +ekg-directory "logseq/"))
;;     ;; (setq ekg-logseq-dir "~/sync/markdown/ekglogseq/")

;;     ;; RESET
;;     ;; (ekg-logseq-set-last-export 0)
;;     ;; (ekg-logseq-set-last-import 0)

;;     (defun +ekg-logseq-sync (&rest args)
;;       (interactive)
;;       (require 'ekg-logseq)
;;       (setq ekg-logseq-dir "~/sync/logseq/logseqfiles/")
;;       (ekg-logseq-sync))
;;     ;; (add-hook 'ekg-note-save-hook '+ekg-logseq-sync)
;;     )

;;   (defun ash/log-to-ekg (text &optional org-mode)
;;     "Log TEXT as a note to EKG's date, appending if possible."
;;     (let ((notes (ekg-get-notes-with-tags (list (ekg-tag-for-date) "log"))))
;;       (if notes
;;           (progn
;;             (setf (ekg-note-text (car notes)) (concat (ekg-note-text (car notes)) "\n" text))
;;             (ekg-save-note (car notes)))
;;         (ekg-save-note (ekg-note-create :text text :mode (if org-mode 'org-mode 'text-mode)
;;                                         :tags `(,(ekg-tag-for-date) "log"))))))
;;   )

;;;; denote

;;;;; denote confuguration

(use-package! denote
  :demand t
  :commands
  (denote denote-create-note denote-insert-link denote-show-backlinks-buffer denote-link-ol-store)
  ;; :hook (dired-mode . denote-dired-mode)
  :init
  (setq denote-directory user-org-directory)
  (require 'denote-silo-extras)
  ;; (require 'denote-journal-extras)
  (require 'denote-org-extras)
  ;; (require 'denote-md-extras) ; markdown-obsidian
  (setq denote-file-type 'org)
  (setq denote-sort-components '(signature title keywords identifier))
  ;; (setq denote-backlinks-show-context nil) ; default nil
  (setq denote-sort-keywords t)
  (setq denote-infer-keywords t)
  (setq denote-excluded-directories-regexp "screenshot")
  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  ;; (setq denote-rename-buffer-format "Denote: %t (%k)")

  (setq denote-org-front-matter
        "#+title:      %1$s
#+hugo_lastmod: %2$s
#+filetags:   %3$s
#+date:       %2$s
#+identifier: %4$s
#+export_file_name: %4$s.md
#+description:
#+hugo_categories: Noname
#+hugo_tags:

#+print_bibliography:

* History
- %2$s

\n")

  ;; (setq denote-modules '(project xref ffap)) ; Enable integration with Emacs modules
  (setq denote-prompts '(subdirectory title keywords)) ; These are the minimum viable prompts for notes
  (setq denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech

  ;; More functionality
  (setq denote-org-store-link-to-heading nil ; default t
        denote-rename-confirmations nil ; default '(rewrite-front-matter modify-file-name)
        denote-save-buffers t) ; default nil

  (add-hook 'org-mode-hook (lambda ()
                             (setq denote-rename-buffer-backlinks-indicator "¶")
                             (setq denote-rename-buffer-format "%t%b")
                             (denote-rename-buffer-mode +1)))

  :config
  (set-register ?n (cons 'file (concat org-directory "notes")))

  ;; (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe) ; from 3.0
  ;; (add-hook 'markdown-mode-hook #'denote-fontify-links-mode-maybe) ; from 3.0
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  (progn ;; vedangs tips
    (setq denote-silo-extras-directories
          (list (expand-file-name denote-directory)))

    (unless IS-TERMUX
      (add-to-list
       'denote-silo-extras-directories
       (expand-file-name "~/git/jh-blogookpub/org"))
      (add-to-list
       'denote-silo-extras-directories
       (expand-file-name "~/Documents/org"))
      ;; (add-to-list
      ;;  'denote-silo-extras-directories (expand-file-name "~/sync/winmacs/org"))
      )

    ;; I use Yasnippet to expand these into a better template.
    (add-to-list 'denote-templates '(reference-note . "reference"))
    (add-to-list 'denote-templates '(morning . "morningpage"))
    (add-to-list 'denote-templates '(emotion . "emotion"))
    (add-to-list 'denote-templates '(insight . "insight"))
    (add-to-list 'denote-templates '(weekly_intentions . "weekint"))
    (add-to-list 'denote-templates '(weekly_report . "weekrpt"))

    (setq
     denote-dired-directories-include-subdirectories t
     denote-dired-directories denote-silo-extras-directories)

    ;; Also check the commands `denote-link-after-creating',
    ;; `denote-link-or-create'.  You may want to bind them to keys as well.

    ;; If you want to have Denote commands available via a right click
    ;; context menu, use the following and then enable
    ;; `context-menu-mode'.
    ;; (add-hook 'context-menu-functions #'denote-context-menu)
    ) ;; end-of progn from vedang's custom

;;;;; add denote-file-types for quarto - qmd

  (after! denote
    (let ((quarto (cdr (assoc 'markdown-yaml denote-file-types))))
      (setf (plist-get quarto :extension) ".qmd")
      (add-to-list 'denote-file-types (cons 'quarto quarto)))
    )

;;;;; docsim

  (use-package! docsim
    :defer 3
    ;; :bind (("C-c n s" . docsim-search)
    ;;        ("C-c n d" . docsim-search-buffer))
    :init
    (setq docsim-assume-english nil) ; default t
    :config
    (setq docsim-search-paths (list org-directory)))

;;;;; consult-denote

  (use-package! consult-denote
    :after denote
    ;; :hook (org-mode . consult-denote-mode)
    :config
    ;; Prefer `ripgrep' and `fd' variants when available
    (when (executable-find "fd")
      (setopt consult-denote-find-command #'consult-fd))
    (when (executable-find "rg")
      (setopt consult-denote-grep-command #'consult-ripgrep))
    (consult-customize
     consult-denote-find
     consult-denote-grep
     :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k"))
    )

;;;;; consult-notes

  (use-package! consult-notes
    :defer 2
    :commands (consult-notes consult-notes-search-in-all-notes)
    :init
    (setq consult-notes-denote-display-id t)
    (setq consult-notes-denote-dir t)
    (setq consult-notes-denote-title-margin 2) ; 24
    :config

    (consult-customize
     consult-notes
     :preview-key '("M-m" :debounce 0.3 "<up>" "<down>" "C-j" "C-k"))

    ;; (unless IS-TERMUX
    ;;   (setq consult-notes-file-dir-sources
    ;;         '(("Clone-notes"  ?c  "~/nosync/clone-notes/"))))

    ;; '(("Denote"  ?d  "~/org/denotes")
    ;;  ("Fleeting"  ?f  "~/org/denotes/fleeting")
    ;;  ("Literature"  ?l  "~/org/denotes/literature")
    ;;  ("Permanent"  ?p  "~/org/denotes/permanent")
    ;;  ("Personal"  ?e  "~/org/denotes/personal")
    ;;  ("Journal"  ?j  "~/org/denotes/journal")
    ;;  ("Hub"  ?h  "~/org/denotes/hubs"))

    ;; 1) denote
    (progn
      (defun my/consult-notes-denote--display-keywords (keywords)
        (format "%30s" (if keywords (concat "#" (mapconcat 'identity keywords " ")) ""))) ; default 18

      (defun my/consult-notes-denote--display-dir (dirs)
        (format "%10s" (concat "/" dirs))) ; default 18

      (setq consult-notes-denote-display-keywords-function #'my/consult-notes-denote--display-keywords)
      (setq consult-notes-denote-display-dir-function #'my/consult-notes-denote--display-dir)

      ;; (vertico-sort-function 'vertico-sort-history-alpha)
      ;; https://github.com/mclear-tools/consult-notes/issues/16
      (after! vertico-multiform
        ;; /doomemacs-junghan0611/modules/completion/vertico/config.el
        (setq vertico-multiform-categories nil) ; reset nil
        (setq vertico-multiform-commands nil) ; reset nil
        (add-to-list 'vertico-multiform-commands
                     '(consult-denote-open (vertico-sort-function . vertico-sort-history-alpha))
                     '(consult-notes (vertico-sort-function . vertico-sort-history-alpha)))) ; vertico-sort-alpha

      (consult-notes-denote-mode 1)

      (defun +consult-notes--unbound-org-roam ()
        (fmakunbound 'consult-notes-org-roam-mode)
        (fmakunbound 'consult-notes-org-roam-find-node-relation))
      (+consult-notes--unbound-org-roam)
      )

    ;; 2) heading
    ;; (setq consult-notes-org-headings-files
    ;;       (list
    ;;        (my/org-inbox-file)
    ;;        (my/org-life-file)
    ;;        (my/org-tasks-file)
    ;;        ;; (my/org-diary-file)
    ;;        (my/org-drill-file)
    ;;        (my/org-quote-file)
    ;;        (my/org-mobile-file)
    ;;        (my/org-contacts-file)
    ;;        (my/org-links-file)))
    ;; (consult-notes-org-headings-mode 1)
    )

;;;;; denote-explore

  ;; 읽어볼 것 https://github.com/pprevos/denote-explore
  (use-package! denote-explore
    :defer 5
    ;; :custom
    ;; Location of graph files
    ;; (denote-explore-network-directory "~/documents/notes/graphs/")
    ;; (denote-explore-network-filename "denote-network")
    ;; Output format
    ;; (denote-explore-network-format 'graphviz)
    ;; (denote-explore-network-graphviz-filetype "svg")
    ;; Exlude keywords or regex
    ;; (denote-explore-network-keywords-ignore '("bib"))
    )

;;;;; citar-denote

  (use-package! citar-denote
    :after citar
    :demand t ;; Ensure minor mode is loaded
    :bind (:map org-mode-map ("C-c B" . citar-insert-citation)) ;; ("M-B" . citar-insert-preset)
    :commands
    (citar-create-note citar-open-notes citar-denote-open citar-denote-add-citekey)
    :init
    (require 'bibtex)
    (require 'citar)
    :custom
    ;; (citar-open-always-create-notes t)
    ;; (citar-denote-signature t)
    (citar-denote-file-type 'org)
    (citar-denote-subdir t)
    (citar-denote-keyword "bib")
    (citar-denote-title-format "author-year-title") ; default title
    (citar-denote-use-bib-keywords nil)
    (citar-denote-title-format-authors 1)
    (citar-denote-title-format-andstr "and")
    :config
    (setq citar-file-open-functions '(("html" . citar-file-open-external)
                                      ;; ("pdf" . citar-file-open-external)
                                      (t . find-file)))

    ;; FIXME for denote-obsidian
    (setq citar-denote-file-types
          `((org
             :reference-format "#+reference:  %s\n"
             :reference-regex "^#\\+reference\\s-*:")
            (markdown-obsidian ;; 2025-02-03
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-yaml
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-toml
             :reference-format "reference  = %s\n"
             :reference-regex "^reference\\s-*=")
            (text
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")))
    (citar-denote-mode))

;;;;; end-of denote
  ) ;; end-of denote

;;;; DONT Obsidian

;; (use-package! obsidian
;;   :defer t
;;   :init
;;   ;; (require 'hydra)
;;   ;; (bind-key (kbd "M-g O") 'obsidian-hydra/body 'obsidian-mode-map)
;;   (setq obsidian-include-hidden-files nil)
;;   (obsidian-specify-path (concat org-directory "md/"))
;;   :config
;;   ;; (global-obsidian-mode t)
;;   )

;;;; binder

(use-package! binder
  :defer t
  :commands binder-toggle-sidebar
  :config
  (require 'binder-tutorial)  ;; optional
  )

;;; :custom AI

;;;; llmclient: gptel - llmclient

;;;;; use-package gptel

;;;;;; 01 - gptel

(use-package! gptel
  :defer 1
  :commands (gptel gptel-send)
  :init
  (setq gptel-default-mode 'org-mode)
  ;; (setq gptel-temperature 0.5) ; gptel 1.0, Perplexity 0.2

  ;; "^\\*gptel-ask\\*"
  ;; ("^\\*ChatGPT\\*" :size 84 :side right :modeline t :select t :quit nil :ttl t)
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
  (set-popup-rule! "^\\*gptel\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
  ;; (set-popup-rule! "^\\*xAI\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4
  :config

;;;;;; 02 - default prompt

  ;; (setq gptel-model 'gpt-4o-mini) ; default 'gpt-4o-mini
  ;; (setq gptel-api-key user-openai-api-key)

  ;; (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** [ME]: ")
  ;; (setf (alist-get 'org-mode gptel-response-prefix-alist) "*** [AI]: ")

  (setf
   (cdr (assoc 'default gptel-directives))
   "You are a large language model living in Emacs and a helpful assistant. Respond concisely using Korean language.")
  (setq gptel--system-message (alist-get 'default gptel-directives))

;;;;;; 03 - gptel-save-as-org-with-denote-metadata

;;;###autoload
  (defun gptel-save-as-org-with-denote-metadata ()
    "Save buffer to disk when starting gptel with metadata."
    (interactive)
    (unless (buffer-file-name (current-buffer))
      (let* ((suffix (format-time-string "%Y%m%dT%H%M%S"))
             (chat-dir (concat org-directory "/notes"))
             (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode)))
             (filename (concat suffix "__llmlog" "." ext))
             (full-path (expand-file-name filename chat-dir)))
        (unless (file-directory-p chat-dir)
          (make-directory chat-dir :parents))
        (write-file full-path)

        ;; Add metadata to the file
        (goto-char 0) (search-forward ":END:") (end-of-line)
        (insert (format "\n#+title: #LLM: %s\n" suffix))
        (insert "#+filetags: :llmlog:\n")
        (insert (format "#+date: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert (format "#+identifier: %s\n" suffix))
        (insert (format "#+export_file_name: %s.md\n" suffix))
        (insert (format "#+hugo_tags: \"fleeting\" \"llmlog\" \n\n"))

        ;; add bib and history
        (insert (format "#+print_bibliography:\n* History\n- %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))

        ;; heading-1 add backlink to today
        (insert (format "* [[denote:%s][%s]]\n"
                        ;; (format-time-string "%Y%m%dT000000")
                        (format-time-string "%Y%m%dT000000"
                                            (org-journal--convert-time-to-file-type-time
                                             (time-subtract (current-time)
                                                            (* 3600 org-extend-today-until))))
                        (format-time-string "%Y-%m-%d W%W")))
        ;; heading-2 [SUM]:
        ;; (insert (format "** TODO [SUM]: \n"))
        (insert "\n"))))

;;;;;; 04 - gptel-org-toggle-branching-context

  (with-eval-after-load 'gptel-org
    (defun gptel-org-toggle-branching-context ()
      "Toggle gptel context between doc and subheading."
      (interactive)
      (if gptel-org-branching-context
          (progn
            (setq-local gptel-org-branching-context nil)
            (message "Context: whole doc"))
        (setq-local gptel-org-branching-context t)
        (message "Context: subheading")))

    ;; (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
    ;;       (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n") ; karthink
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user "
          (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"
          (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")

    (setq-default gptel-org-branching-context t))

;;;;;; 05 - gptel backend configurations

  ;; Google - Gemini
  (gptel-make-gemini "Gemini" :key user-gemini-api-key :stream t)

  ;; Anthropic - Claude
  ;; (gptel-make-anthropic "Claude" :key user-claude-api-key :stream t)

  ;; https://perplexity.mintlify.app/guides/pricing
  ;; Model	Context Length	Model Type
  ;; sonar-reasoning	127k	Chat Completion
  ;; sonar-pro	200k	Chat Completion
  ;; sonar	127k	Chat Completion
  (setq gptel-model 'sonar
        gptel-backend
        (gptel-make-perplexity "Perplexity"
          :host "api.perplexity.ai"
          :key user-perplexity-api-key
          :endpoint "/chat/completions"
          :stream t
          :request-params '(:temperature 0.2) ; default 0.2
          :models '(sonar sonar-pro sonar-reasoning)))

  ;; DeepSeek offers an OpenAI compatible API
  ;; The deepseek-chat model has been upgraded to DeepSeek-V3. deepseek-reasoner points to the new model DeepSeek-R1.
  ;; USE CASE	TEMPERATURE
  ;; Coding / Math	0.0
  ;; Data Cleaning / Data Analysis	1.0
  ;; General Conversation	1.3
  ;; Translation	1.3
  ;; Creative Writing / Poetry	1.5
  ;; https://api-docs.deepseek.com/quick_start/parameter_settings
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :key user-deepseek-api-key
    :endpoint "/chat/completions"
    :stream t
    :request-params '(:temperature 0.0) ; 1.0 default
    :models '(deepseek-chat deepseek-reasoner))

  ;; Upstage: solar
  ;; https://developers.upstage.ai/docs/apis/chat
  (gptel-make-openai "Upstage"
    :host "api.upstage.ai/v1/solar"
    :key user-upstageai-api-key
    :endpoint "/chat/completions"
    :stream t
    :request-params '(:temperature 0.5)
    :models '(solar-pro
              solar-mini))

  ;; OpenRouter offers an OpenAI compatible API
  ;; https://openrouter.ai/
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key user-openrouter-api-key
    :request-params '(:temperature 0.5)
    :models '(
              google/gemini-flash-1.5
              anthropic/claude-3.5-sonnet
              openai/gpt-4o-mini
              deepseek/deepseek-chat
              ;; qwen/qwen-2.5-7b-instruct
              ))

  ;; Kagi’s FastGPT model and the Universal Summarizer are both supported. A couple of notes:
  (gptel-make-kagi "Kagi" :stream t :key user-kagi-api-key)

  ;; Together.ai offers an OpenAI compatible API
  ;; (gptel-make-openai "TogetherAI"
  ;;   :host "api.together.xyz"
  ;;   :key user-togetherai-api-key
  ;;   :stream t
  ;;   :models '(;; has many more, check together.ai
  ;;             "meta-llama/Llama-3.2-11B-Vision-Instruct-Turbo" ;; Meta Llama 3.2 11B Vision Instruct Turbo $0.18
  ;;             "meta-llama/Llama-3.2-3B-Instruct-Turbo" ;; Meta Llama 3.2 3B Instruct Turbo $0.06
  ;;             ))

  ;; Github Models offers an OpenAI compatible API
  ;; https://docs.github.com/en/github-models/prototyping-with-ai-models
  ;; (gptel-make-openai "GithubModels" ; Any name you want
  ;;   :host "models.inference.ai.azure.com"
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :key user-github-api-key
  ;;   :models '(gpt-4o-mini)) ;; low tier

  ;; xAI offers an OpenAI compatible API
  ;; (gptel-make-openai "xAI"
  ;;   :host "api.x.ai"
  ;;   :key user-xai-api-key
  ;;   :endpoint "/v1/chat/completions"
  ;;   :stream t
  ;;   :models '(;; xAI now only offers `grok-beta` as of the time of this writing
  ;;             grok-beta))

  ) ; end-of gptel

;;;;; cashpwd - gptel-send with prompt

(after! gptel
  ;; /home/junghan/sync/man/dotsamples/doom/cashpw-dotfiles-node/config/doom/config-personal.org
  (defvar cashpw/llm--default-prompt
    "You are a large language model living in Emacs and a helpful assistant. Respond concisely using Korean language.")

  (defvar cashpw/llm--chain-of-thought-prompt
    "You are a large language model living and a helpful assistant. First, enumerate a list of steps one should follow to find an appropriate answer. Second, follow those steps and show your work. Respond concisely using Korean language")

  (defvar cashpw/llm--follow-up-prompt
    "Assume the persona of a peer and colleague who is working with me to understand and expand on an idea or question. Respond with between three and ten follow-up questions or considerations. Format your response in markdown using Korean language.")

  (defvar cashpw/llm--writing-prompt
    "You are a large language model and a writing assistant. Respond concisely using Korean language.")

  (defvar cashpw/llm--programming-prompt
    "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt, or note.")

  (defvar cashpw/llm--chat-prompt
    "You are a large language model and a conversation partner. Respond concisely using Korean language")

  ;; cashpw
  (setq gptel-directives `((default . ,cashpw/llm--default-prompt)
                           (chain-of-thought . ,cashpw/llm--chain-of-thought-prompt)
                           (follow-up . ,cashpw/llm--follow-up-prompt)
                           (writing . ,cashpw/llm--writing-prompt)
                           (programming . ,cashpw/llm--programming-prompt)
                           (chat . ,cashpw/llm--chat-prompt)))

;;;###autoload
  (defun cashpw/gptel-send (prompt)
    "Invoke `gptel-send' with specific PROMPT."
    (let ((gptel--system-message prompt))
      (gptel-send)))
  )

;;;;; agzam

(after! gptel
  ;; ~/sync/man/dotsamples/doom/agzam-dot-doom/modules/custom/ai/config.el
  (load! "+gptel") ; agzam-dot-doom

  (add-hook! 'gptel-mode-hook
    (defun gptel-mode-set-local-keys ()
      (map! :map gptel-mode-map
            :iv "M-<return>" #'gptel-send
            :iv "M-RET" #'gptel-send
            :iv "C-RET" #'cashpw/gptel-send
            :iv "C-<return>" #'cashpw/gptel-send
            (:localleader
             :desc "gptel/default" "," #'gptel-menu
             "M-s" #'gptel-save-as-org-with-denote-metadata
             :desc "gptel/default" "1" (cmd! (cashpw/gptel-send (alist-get 'default gptel-directives)))
             :desc "gptel/chain of thought" "2" (cmd! (cashpw/gptel-send (alist-get 'chain-of-thought gptel-directives)))
             :desc "gptel/follow up" "3" (cmd! (cashpw/gptel-send (alist-get 'follow-up gptel-directives)))
             (:prefix ("s" . "session")
              :desc "clear" "l" #'gptel-clear-buffer+
              "p" #'gptel-save-as-org-with-denote-metadata
              )))))

  ;; 2024-12-12 disable
  ;; (add-hook! 'kill-emacs-hook
  ;;   (defun persist-gptel-model ()
  ;;     (customize-save-variable 'gptel-backend gptel-backend)
  ;;     (customize-save-variable 'gptel-model gptel-model)))
  ;; (add-hook! 'gptel-post-stream-hook #'gptel-save-as-org-with-denote-metadata) ;; manually
  )

;;;;; TODO gptel with gregory prompt

;; (after! gptel
;;   ;; classic gptel configuration
;;   (setq
;;    gptel-model 'claude-3-opus-20240229
;;    gptel-backend (gptel-make-anthropic "Claude"
;;                    :stream t :key "sk-..."))
;;   ;; set gptel-directives as AIPIHKAL system-prompts
;;   (let ((build-directives-fun "~/projects/ai/AIPIHKAL/gptel-build-directives.el"))
;;     (when (file-exists-p build-directives-fun)
;;       (load build-directives-fun)
;;       (setq gptel-directives (gjg/gptel-build-directives "~/projects/ai/AIPIHKAL/system-prompts/")
;;             gptel-system-message (alist-get 'default gptel-directives)))))

;;;;; gptel-quick

(use-package! gptel-quick
  :after gptel
  :commands gptel-quick
  :config
  (progn
    (require 'gptel-quick)
    (setq gptel-quick-word-count 30)
    (setq gptel-quick-timeout 20)

    ;;;###autoload
    (defun gptel-quick (query-text &optional count)
      "Explain or summarize region or thing at point with an LLM.
      QUERY-TEXT is the text being explained.  COUNT is the approximate
      word count of the response."
      (interactive (list
                    (cond
                     ((use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end)))
                     ((and (derived-mode-p 'pdf-view-mode)
                           (pdf-view-active-region-p))
                      (mapconcat #'identity (pdf-view-active-region-text)
                                 "\n\n"))
                     (t
                      (thing-at-point 'sexp)))
                    current-prefix-arg))
      (let* ((count (or count gptel-quick-word-count))
             (gptel-max-tokens
              (floor (+ (sqrt (length query-text)) (* count 2.5))))
             (gptel-use-curl)
             (gptel-use-context (and gptel-quick-use-context 'system)))
        (gptel-request
            query-text
          ;; :system (format "Explain in %d words or fewer using Korean" count)
          :system (format "1) Translate the question to English. 2) Respond in %d words or fewer using Korean." count)
          :context
          (list
           query-text count
           (posn-at-point (and (use-region-p) (region-beginning))))
          :callback #'gptel-quick--callback-posframe)))

    ;; keymap
    ;; (map! :n "C-k" #'gptel-quick)
    ;; (map! :map visual-line-mode-map "C-k" #'gptel-quick)
    ) ; progn gptel-quick
  )

;;;; llmclient: chatgpt-shell

;;;;; use-package chatgpt-shell

(use-package! chatgpt-shell
  :defer t
  :commands (chatgpt-shell--primary-buffer chatgpt-shell chatgpt-shell-prompt-compose)
  :bind (("C-x m" . chatgpt-shell)
         ("C-c C-0" . chatgpt-shell-prompt-compose))
  :hook
  (chatgpt-shell-mode
   . (lambda () (setq-local completion-at-point-functions nil)))
  :init (setq shell-maker-history-path (concat user-org-directory "var/"))
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*chatgpt\\*"
  ;;                display-buffer-in-side-window
  ;;                (side . right)
  ;;                (slot . 0)
  ;;                (window-parameters . ((no-delete-other-windows . t)))
  ;;                (dedicated . t)))
  :custom (shell-maker-prompt-before-killing-buffer nil)
  ;; (chatgpt-shell-openai-key
  ;;  (auth-source-pick-first-password :host "api.openai.com"))
  (chatgpt-shell-openai-key user-openai-api-key)
  (chatgpt-shell-transmitted-context-length 5)
  :config
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)

  (setq chatgpt-shell-model-versions '("gpt-4o-mini"
                                       "gpt-3.5-turbo"
                                       "gpt-4o-mini"
                                       "chatgpt-4o-latest" "o1-preview" "o1-mini" "gpt-4o" "gpt-4-0125-preview"
                                       "gpt-4-turbo-preview" "gpt-4-1106-preview" "gpt-4-0613" "gpt-4" "gpt-3.5-turbo-16k-0613" "gpt-3.5-turbo-16k" "gpt-3.5-turbo-0613" ))

  (add-to-list
   'chatgpt-shell-system-prompts
   '("Writing" . "You are a large language model and a writing assistant."))

  ;; (add-to-list
  ;;  'chatgpt-shell-system-prompts
  ;;  `("Cybersecurity" .
  ;;    ,(concat "The user is an aspiring cybersecurity expert. "
  ;;             "You need to go as deep into technical details as possible. "
  ;;             "Elaborate your answers for the highest level of expertise. "
  ;;             "Do not expand abbreviations unless explicitly asked. "
  ;;             "Code examples should be in Emacs Org-mode source blocks. "
  ;;             "Cite relevant RFCs and CVEs, if any. "
  ;;             "Links and citations should be in Org-mode link format.")))

  ;; (add-to-list
  ;;  'chatgpt-shell-system-prompts
  ;;  `("Espanól" .
  ;;    ,(concat "The user is a person trying to learn Spanish. "
  ;;             "Expect request texts mixed in both languages."
  ;;             "Answer in Spanish, unless specifically asked to provide the translation."
  ;;             "Focus on Latin American (primarily Mexican) dialect. "
  ;;             "When applicable, help the user with memorization and building vocabulary. "
  ;;             "Highligh the connection of words with shared etymology, e.g.: 'quieres' to 'inquire', and 'ayuda' to 'aid'. "
  ;;             "When asked about specific words, provide example sentences.")))

  (add-to-list
   'chatgpt-shell-system-prompts
   `("Leetcode" .
     ,(concat
       "Help user to solve Leetcode problems. "
       "Structure responses in Org-Mode format and org-babel source blocks. "
       "Write solutions in javascript. "
       "Avoid using 'for loops' whenever possible, using .map/.reduce instead. "
       "Comment on time and space complexity of each solution. "
       "Advertise alternative algorithms and approaches for further research. ")))

  ;; set default prompt to None
  (setq chatgpt-shell-system-prompt
        (- (length chatgpt-shell-system-prompts)
           (length
            (member
             (assoc "None" chatgpt-shell-system-prompts)
             chatgpt-shell-system-prompts))))

  ;; :bind (:map chatgpt-shell-mode-map
  ;;             (("RET" . newline)
  ;;              ("M-RET" . shell-maker-submit)
  ;;              ("M-." . dictionary-lookup-definition)))
  )

;;;;; Custom ChatGPT Functions

;; Custom ChatGPT Functions
(after! chatgpt-shell
  ;; required for `chatgpt-shell-system-prompts'
  (require 'pcsv)

  (defun my/chatgpt-shell-at-point-or-region (header)
    "Send the header with the token at point or the selected region to ChatGPT."
    (if (region-active-p)
        (chatgpt-shell-send-region-with-header header)

      (if-let ((token (thing-at-point 'symbol)))
          (chatgpt-shell-send-to-buffer (concat header "\n\n" token))
        (user-error "Nothing at point or selected."))))

  ;; Explains
  (defun my/chatgpt-shell-explain-org-heading ()
    "Explain the org heading with GhatGPT."
    (interactive)
    (chatgpt-shell-send-to-buffer
     (concat
      "Explain the given topic briefly:\n\n" (my/org-get-heading-title)
      ;; (my/org-breadcrumbs)
      )))

  (defun my/chatgpt-shell-explain-region ()
    "Explain the topic using ChatGPT."
    (interactive)
    (my/chatgpt-shell-at-point-or-region
     "Explain the given topic briefly and provides 3 fun facts
    to help me remebering and learning it: "))

  ;; Summarizes
  (defun my/chatgpt-shell-summarize-region ()
    "Explain the topic using ChatGPT."
    (interactive)
    (my/chatgpt-shell-at-point-or-region "Summerize the text briefly: "))

  ;; Writes
  (defun my/chatgpt-shell-write-org-heading ()
    "Write the content for the current org heading using ChatGPT."
    (interactive)
    (chatgpt-shell-send-to-buffer
     ;; (format "Write an article for the topic: \n\n%s"
     (format "Write an article for the topic in briefly: \n\n%s"
             (my/org-get-heading-title)
             ;; (my/org-breadcrumbs)
             )))

  ;; Writes in Korean
  (defun my/chatgpt-shell-write-org-heading-korean ()
    "Write the content for the current org heading using ChatGPT in Korean."
    (interactive)
    (chatgpt-shell-send-to-buffer
     (format "Write an article for the topic in Korean : \n\n%s"
             (my/org-get-heading-title)
             ;; (my/org-breadcrumbs)
             ))))


;;;; llmclient: aider.el

(use-package! aider
  :config
  (setq aider-args '("--model" "deepseek/deepseek-chat"))
  (setenv "ANTHROPIC_API_KEY" user-claude-api-key)
  (setenv "OPENAI_API_KEY" user-openai-api-key)
  (setenv "GEMINI_API_KEY" user-gemini-api-key)
  (setenv "PERPLEXITYAI_API_KEY" user-perplexity-api-key)
  (setenv "XAI_API_KEY" user-xai-api-key)
  (setenv "DEEPSEEK_API_KEY" user-deepseek-api-key)
  )

;; aider --list-models deepseek
;; deepseek/deepseek-chat, deepseek/deepseek-coder, deepseek/deepseek-reasoner

;; Or use chatgpt model since it is most well known
;; (setq aider-args '("--model" "gpt-4o-mini"))
;; Or use gemini v2 model since it is very good and free
;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
;; Optional: Set a key binding for the transient menu
;; (global-set-key (kbd "C-c a") 'aider-transient-menu)

;;;; llmclient: kagi

;; cecil
;; agnes
;; daphne
;; muriel
(use-package! kagi
  :defer 3
  :custom
  (kagi-api-token user-kagi-api-key)
  ;; (kagi-api-token (lambda () (password-store-get "Kagi/API")))
  ;; Universal Summarizer settings
  (kagi-summarizer-engine "cecil") ;; Formal, technical, analytical summary.
  (kagi-summarizer-default-language "KO")
  (kagi-summarizer-cache t))

;;;; DONT org-ai

;; (use-package! org-ai
;;   :defer 5
;;   :commands (org-ai-mode org-ai-global-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
;;   (org-ai-global-mode) ; installs global keybindings on C-c M-a
;;   :config
;;   (setq org-ai-openai-api-token user-openai-api-key)
;;   (setq org-ai-default-chat-model "gpt-4o-mini") ;; "gpt-4o-mini"
;;   (org-ai-install-yasnippets))
; if you are using yasnippet and want `ai` snippets

;;;; DONT elisa

;; ELISA (Emacs Lisp Information System Assistant)
;; (use-package! elisa
;;   :if (not IS-TERMUX)
;;   :init
;;   (setopt elisa-limit 5)
;;   (require 'llm-ollama)
;;   (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model "nomic-embed-text"))
;;   (setopt elisa-chat-provider (make-llm-ollama
;; 			       :chat-model "sskostyaev/openchat:8k-rag"
;; 			       :embedding-model "nomic-embed-text")))


;;;; DONT ellama - offline local llm

;; https://github.com/s-kostyaev/ellama
;; (use-package! ellama
;;   :defer 3
;;   :init
;;   ;; setup key bindings
;;   ;; (setopt ellama-keymap-prefix "C-c e")
;;   ;; (setopt ellama-language "Korean")
;;   ;; (setopt ellama-provider emacs-llm-default-provider)

;;   ;; I've looked for this option for 1.5 hours
;;   (setq ellama-long-lines-length 100000)
;;   )

;;;; DONT whisper

;; 데스크탑에서 사용해야 할 듯
;; (use-package! whisper
;;   :defer t
;;   :config
;;   ;; (setq whisper-language "ko") ; "en"
;;   (setq
;;    ;; whisper-install-directory "~/.config/emacs/.local/cache/"
;;    ;; whisper-model "large-v3"
;;    ;; whisper-model "medium"
;;    ;; whisper-model "small"
;;    whisper-model "base"
;;    whisper-language "en"
;;    whisper-translate nil
;;    ;; whisper--ffmpeg-input-device "hw:0"
;;    ;; whisper-return-cursor-to-start nil)
;;    )
;;   )

;;;; DONT DALL-E

;; (setq dall-e-n 1)
;; (setq dall-e-spinner-type 'flipping-line)
;; (setq dall-e-display-width 256)

;;;; TODO llmclient: elysium with gptel

;; (use-package! elysium :after gptel)

;; "sq" #'elysium-query
;; "so" #'elysium-keep-all-suggested-changes
;; "sm" #'elysium-discard-all-suggested-changes
;; "st" #'elysium-toggle-window)
; '(insert normal) 'gptel-mode-map "C-<return>" #'elysium-query

;;;; TODO llmclient: yap - gptel another

;; vanilla/meain-dotfiles/emacs/.config/emacs/init.el
;; (use-package! yap
;;   :after (plz)
;;   :config
;;   (setq yap-service "openai")
;;   (setq yap-model "gpt-4o-mini") ; start with something cheap

;;   (setq yap-api-key:openai user-openai-api-key)
;;   (setq yap-api-key:anthropic user-claude-api-key)

;;   ;; (setq yap-respond-in-buffer nil)
;;   ;; (setq yap-show-diff-before-rewrite t)
;;   ;; (setq yap-log-requests "/Users/meain/.cache/yap")

;;   ;; Add window rules for *yap-response* buffer so that it shows up at
;;   ;; top of the frame
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx bos "*yap-response*" eos)
;;                  (display-buffer-reuse-window
;;                   display-buffer-in-side-window)
;;                  (reusable-frames . visible)
;;                  (side            . top)
;;                  (window-height   . 0.3)))

;;   (defun meain/yap-set-default-model ()
;;     (interactive)
;;     (setq yap-service "openai")
;;     (setq yap-model "gpt-4o-mini"))

;;   ;; (global-unset-key (kbd "M-m"))
;;   ;; (global-set-key (kbd "M-m M-c") 'yap-buffer-toggle)
;;   ;; (global-set-key (kbd "M-m M-m") 'yap-prompt)
;;   ;; (global-set-key (kbd "M-m M-r") 'yap-rewrite)
;;   ;; (global-set-key (kbd "M-m M-w") 'yap-write)
;;   ;; (global-set-key (kbd "M-m M-e") (lambda () (interactive) (yap-prompt 'explain-code)))
;;   )

;;;; TODO llmclient: evedel - gtpel for programmer

;; https://github.com/daedsidog/evedel
;; (use-package! evedel
;;   :after gptel)

;;;; DONT llmclient: wolframalpha

;; ziova/wolfram.el
;; (use-package! wolfram
;;   :config (setq wolfram-alpha-app-id user-wolfram-alpha-app-id))

;;;; llmclient: github copilot

(use-package! copilot
  :commands (copilot-login copilot-diagnose)
  :init
  ;; Sometimes the copilot agent doesn't start. Restarting fixes the issue.
  (setq copilot-indent-offset-warning-disable t
        copilot-max-char 10000) ; default 100000
  :bind (:map copilot-completion-map
              ("C-g" . 'copilot-clear-overlay)
              ("M-p" . 'copilot-previous-completion)
              ("M-n" . 'copilot-next-completion)
              ("<tab>" . 'copilot-accept-completion) ; vscode
              ("TAB" . 'copilot-accept-completion) ; vscode
              ("M-f" . 'copilot-accept-completion-by-word)
              ("M-<return>" . 'copilot-accept-completion-by-line)
              ("M-]" . 'copilot-next-completion) ; vscode
              ("M-[" . 'copilot-next-completion) ; vscode
              ;; ("C-'" . 'copilot-accept-completion)
              ;; ("C-;" . 'copilot-accept-completion)
              )
  ;; :hook ((prog-mode . copilot-mode))
  ;; (org-mode . copilot-mode)
  ;; (markdown-mode . copilot-mode)
  )

;;;; llmclient: github copilot-chat

(use-package! copilot-chat
  :after request
  :config
  (setq copilot-chat-backend 'request)
  (setq copilot-chat-frontend 'markdown)
  ;; From https://github.com/chep/copilot-chat.el/issues/24
  (defun meain/copilot-chat-display (prefix)
    "Opens the Copilot chat window, adding the current buffer to the context.
Called with a PREFIX, resets the context buffer list before opening"
    (interactive "P")

    (require 'copilot-chat)
    (let ((buf (current-buffer)))

      ;; Explicit reset before doing anything, avoid it resetting later on
      ;; target-fn and ignoring the added buffers
      (unless (copilot-chat--ready-p)
        (copilot-chat-reset))

      (when prefix (copilot-chat--clear-buffers))

      (copilot-chat--add-buffer buf)
      (copilot-chat-display)))
  )

;;;; DONT llmclient: codeium

;; (use-package! codeium
;;   :after cape
;;   :commands (codeium-install)
;;   :config
;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   ;; (add-hook 'emacs-startup-hook
;;   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

;;   ;; alternatively for a more extensive mode-line
;;   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

;;   ;; you can also set a config for a single buffer like this:
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local codeium/editor_options/tab_size 4)))

;;   ;; You can overwrite all the codeium configs!
;;   ;; for example, we recommend limiting the string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;   ;; if you change the text, you should also change the cursor_offset
;;   ;; warning: this is measured by UTF-8 encoded bytes
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
;;   )

;;; :lang coding

;;;; lsp-mode - lsp-ui-mode - lsp-treemacs

(progn
  (after! lsp-mode
    ;; lsp 관련 설정 메뉴들. 느리게 만드는 범인중 십중팔구 LSP가 관련되어져 있다고 함.
    ;; 해당 튜닝도 구글링을 통해서 찾았다.
    (setq lsp-file-watch-threshold (* 1024 1024))
    (setq read-process-output-max (* 1024 1024))

    (setq
     ;; lsp-keymap-prefix "M-c l"
     lsp-headerline-breadcrumb-enable t ; Breadcrumb trail
     lsp-headerline-breadcrumb-icons-enable nil
     ;; lsp-headerline-breadcrumb-segments '(symbols) ; namespace & symbols, no file path

     lsp-lens-enable nil ; default t
     ;; lsp-semantic-tokens-enable t ; enhance syntax highlight

     ;; lsp-idle-delay 0.2  ; smooth LSP features response
     lsp-eldoc-enable-hover nil ; disable all hover actions
     ;; lsp-modeline-code-actions-segments '(count icon)
     ;; lsp-navigation 'both ; default 'both ; 'simple or 'peek
     ;; lsp-modeline-diagnostics-enable nil
     ;; lsp-modeline-code-actions-enable nil
     )
    ;; (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))
    )

  (after! lsp-ui
    (setq lsp-ui-doc-enable nil       ;; disable all doc popups
          lsp-ui-sideline-enable nil  ;; disable sideline bar for less distraction
          treemacs-space-between-root-nodes nil  ;; no spacing in treemacs views
          lsp-ui-peek-enable t))

  (after! lsp-treemacs
    (setq lsp-treemacs-error-list-current-project-only t))
  )

;;;; devdocs-browser

;; 한글 번역 문서 지원
;; devdocs-browser-install-doc - index 파일만 저장
;; devdocs-browser-download-offline-data - 오프라인 전체 데이터 저장 (기본 영어)
;; 2024-01-31 Python 3.11, NumPy 1.23 pandas 1.5.0, Elixir 1.13
;; 2024-06-26 common-lisp
(use-package! devdocs-browser
  :defer 2
  :commands (devdocs-browser-open devdocs-browser-open-in)
  :bind (("M-s-," . devdocs-browser-open) ;; M-s-/ yas-next-field
         ("M-s-." . devdocs-browser-open-in))
  :config
  (set-popup-rule! "*devdocs-.*\\*" :width 84 :side 'right :ttl t :select nil :quit nil :ttl 0)
  (setq devdocs-browser-data-directory (concat doom-emacs-dir "devdocs-browser"))
  (add-to-list 'devdocs-browser-major-mode-docs-alist '(js2-mode "javascript" "node"))
  (add-to-list 'devdocs-browser-major-mode-docs-alist '(python-mode "Python" "NumPy" "pandas"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(python-ts-mode "Python" "NumPy" "pandas"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(elixir-ts-mode "Elixir"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(rjsx-mode "react" "javascript" "node"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(typescript-ts-mode "typescript"))
  ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(js-ts-mode "javascript" "node"))
  )

;;;; prog-mode-hooks

;; Color the string of whatever color code they are holding
(add-hook 'prog-mode-hook 'rainbow-mode) ; 2023-11-23 on
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'visual-line-mode) ; 2024-10-24

;;;; DONT symbol-overlay

;; (use-package! symbol-overlay
;;   :commands (symbol-overlay-mode symbol-overlay-put)
;;   :bind
;;   (:map symbol-overlay-mode-map
;;         ("C-c C-n" . symbol-overlay-jump-next)
;;         ("C-c C-p" . symbol-overlay-jump-prev))
;;   :hook
;;   (prog-mode . symbol-overlay-mode)
;;   ;; (emacs-lisp-mode . symbol-overlay-mode)
;;   ;; (python-mode . symbol-overlay-mode)
;;   )

;;;; git-link

(use-package! git-link
  :defer t
  :config
  ;; default is to open the generated link
  (setq git-link-open-in-browser t))

;; (spacemacs/declare-prefix "gl" "links")
;; (spacemacs/set-leader-keys
;;  "glc" 'git-link-commit
;;  "gll" 'git-link

;;;; emacs-lisp-mode-hook

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
   ;; with a tab width of 8. Any smaller and the indentation will be
   ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
   ;; safe to ignore this setting otherwise.
   ;; (setq-local tab-width 8)
   (setq-local comment-column 0)
   (define-key emacs-lisp-mode-map (kbd "M-[") 'backward-sexp)
   (define-key emacs-lisp-mode-map (kbd "M-]") 'forward-sexp)))

;;;; aggressive-indent

(use-package! aggressive-indent
  :defer 1
  :if window-system
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook 'aggressive-indent-mode)
  (add-hook 'racket-mode-hook 'aggressive-indent-mode)
  ;; (add-hook 'hy-mode-hook 'aggressive-indent-mode)
  )

;;;; yasnippet Navigation M-n/M-p and hippie-expand M-/

;; use Meta-n and Meta-p to jump between fields
;; <backspace>    +snippets/delete-backward-char
;; <delete>       +snippets/delete-forward-char-or-field
;; C-a            +snippets/goto-start-of-field
;; C-e            +snippets/goto-end-of-field
;; M-<backspace>  +snippets/delete-to-start-of-field
;; M-<left>       +snippets/goto-start-of-field
;; M-<right>      +snippets/goto-end-of-field
;; yas/expand
(after! yasnippet
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  (yas-global-mode +1)
  (define-key yas-keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "M-p") 'yas-prev-field)
  (define-key yas/keymap (kbd "M-n") 'yas-next-field-or-maybe-expand)
  (define-key yas/keymap (kbd "M-p") 'yas-prev-field)
  )

;;;; DONT Eglot + Flycheck

;; (use-package! eglot
;;   :bind
;;   (:map
;;    eglot-mode-map ("C-c d" . eldoc) ("C-c a" . eglot-code-actions)
;;    ;; ("C-c f" . flymake-show-buffer-diagnostics) ;; flycheck
;;    ("C-c r" . eglot-rename)))

;;;; bats-mode for testing awk bash

(use-package! bats-mode :defer t)

;;;;; remove python-mode-hook

;; (remove-hook 'python-mode-local-vars-hook 'spacemacs//python-setup-backend)
;; (remove-hook 'python-mode-hook 'spacemacs//python-default)

;;;;; TODO python-pytest

;; pytest.ini
;; [pytest]
;; markers =
;;     task: A concept exercise task.

;; (after! python-pytest
;; (add-to-list 'pytest-project-root-files "setup.cfg")
;; (add-to-list 'pytest-project-root-files "pytest.ini")
;; )

;;;;; DONT python-ts-mode

;; (use-package! python
;;   :bind
;;   (:map
;;    python-ts-mode-map ("<f5>" . recompile) ("<f6>" . eglot-format)
;;    :map python-mode-map ("<f5>" . recompile) ("<f6>" . eglot-format))
;;   :hook ((python-ts-mode . eglot-ensure))
;;   ;; :mode (("\\.py\\'" . python-ts-mode))
;;   )

;; (use-package! py-isort
;;   :defer t
;;   :after python-mode
;;   :mode ("[./]flake8\\'" . conf-mode)
;;   :config
;;   (add-hook 'before-save-hook 'py-isort-before-save))

;; (after! poetry
;;   :defer t
;;   :config
;;   (setq poetry-tracking-strategy 'projectile)
;;   :init
;;   (remove-hook 'python-mode-hook #'poetry-tracking-mode))

;;;;; DONT disable +format-with-lsp

;; use apheleia
;; (setq-hook! 'python-mode-hook +format-with-lsp nil)

;;;; hy : hylang

;; (use-package! hy-mode
;;   :mode "\\.hy\\'"
;;   :interpreter "hy"
;;   ;; :hook ((hy-mode . eglot-ensure))
;;   :config
;;   (set-repl-handler! 'hy-mode #'hy-shell-start-or-switch-to-shell)
;;   (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(hy-mode))
;;   (when (executable-find "hyuga") ; it's works!
;;     (set-eglot-client! 'hy-mode '("hyuga"))))

;;; :format

;;;;; apheleia + format-with-lsp

;; +onsave -- global

;; (setq +format-on-save-disabled-modes
;;       '(emacs-lisp-mode  ; elisp's mechanisms are good enough
;;         sql-mode         ; sqlformat is currently broken
;;         tex-mode         ; latexindent is broken
;;         latex-mode
;;         org-msg-edit-mode
;;         ))

;; Disabling the LSP formatter
;; 1) To disable this behavior universally use:
;; (setq +format-with-lsp nil)

;; 2) To disable this behavior in one mode:
;; ;; (setq-hook! 'python-mode-hook +format-with-lsp nil) ; python


;;;;; DONT elisp-autofmt - manually

;; (use-package! elisp-autofmt
;;   ;; :load-path "~/sync/emacs/git/junghan0611/emacs-elisp-autofmt/"
;;   :commands (elisp-autofmt-mode elisp-autofmt-buffer)
;;   :init (setq elisp-autofmt-format-quoted nil)
;;   ;; :hook (emacs-lisp-mode . elisp-autofmt-mode)
;;   )

;;; :ui

;;;; doom-dashboard

(progn
  ;; (defun emacs-dashboard-draw-ascii-banner-fn ()
  ;;   (let* ((banner
  ;;           '("Welcome to                                 "
  ;;             "███████╗███╗   ███╗ █████╗  ██████╗███████╗"
  ;;             "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
  ;;             "█████╗  ██╔████╔██║███████║██║     ███████╗"
  ;;             "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
  ;;             "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
  ;;             "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"))
  ;;          (longest-line (apply #'max (mapcar #'length banner))))
  ;;     (put-text-property
  ;;      (point)
  ;;      (dolist (line banner (point))
  ;;        (insert
  ;;         (+doom-dashboard--center
  ;;          +doom-dashboard--width
  ;;          (concat line (make-string (max 0 (- longest-line (length line))) 32)))
  ;;         "\n"))
  ;;      'face 'bold)))

  ;; doom-dashboard-banner
  ;; (when (display-graphic-p) ; gui
  ;;   (setq +doom-dashboard-ascii-banner-fn 'emacs-dashboard-draw-ascii-banner-fn))

  (setq fancy-splash-image (concat user-dotemacs-dir "assets/images/logo.png"))

  (setq +doom-dashboard-functions
        '(doom-dashboard-widget-banner
          doom-dashboard-widget-shortmenu
          my/dashboard-widget-fortune ;; fortune
          doom-dashboard-widget-loaded doom-dashboard-widget-footer))

  (defun my/dashboard-widget-fortune ()
    (let* ((quotestring
            (if (executable-find "fortune")
                (string-join
                 (mapcar
                  (lambda (l) (concat "\n " (string-fill l 72)))
                  (if IS-TERMUX
                      (string-lines (shell-command-to-string "fortune"))
                    (string-lines
                     (shell-command-to-string
                      "fortune -c 90% advice 10% .")))))))) ;; 10% samples
      (+doom-dashboard--center
       (- +doom-dashboard--width 2)
       ;; (insert (nerd-icons-faicon "nf-fa-pencil" :face 'doom-dashboard-footer-icon :height 1.0 :v-adjust -0.15))
       (insert quotestring "\n"))))
  )

;;;; Font Test:

;; Font test: " & ' ∀ ∃ ∅ ∈ ∉ ∏ ∑ √ ∞ ∧ ∨ ∩ ∪ ∫ ² ³ µ · × ∴ ∼
;; ≅ ≈ ≠ ≡ ≤ ≥ < > ⊂ ⊃ ⊄ ⊆ ⊇ ⊥ ∂ ∇ ∈ ∝ ⊕ ⊗ ← → ↑ ↓ ↔ ⇐ ⇒ ⇔
;; □ ■ | © ¬ ± ° · ˜ Γ Δ α β γ δ ε φ ∀, ∃, ￢(~), ∨, ∧,⊂, ∈,
;; ⇒, ⇔ 𝑀＜1
;; 𝑻𝑼𝑽𝗔𝗕𝗖𝗗 𝞉𝞩𝟃 ϑϕϰ ⊰⊱⊲⊳⊴⊵⫕ 𝚢𝚣𝚤𝖿𝗀𝗁𝗂
;; § † ‡ № ¶

;;;; Math Symbol

;; vanilla/garyo-dotfiles-ekg/lisp/init-fonts-and-frames.el
;; to display Unicode math chars, like math A to z (𝐴 .. 𝑧, #x1D434 .. #x1D467)
;; and pi: #1D70B = 𝜋 Cambria and Segoe UI Symbol should both work on Windows, but Emacs may pick up some other inappropriate font.

;; (when-windows
;;  (set-fontset-font t 'mathematical "Segoe UI Symbol"))

;; Useful things for chars, fonts and fontsets:
;;  M-x describe-fontset
;;  C-u C-x = ; char details at point
;;  C-x 8 RET ; insert char by name or unicode (hex)
;;  var script-representative-chars: list of all (most?) Unicode script ranges with "representative" chars
;;  See https://lists.gnu.org/archive/html/help-gnu-emacs/2021-09/txtRLYx8BDBtJ.txt for useful math fontset test code
;;  As of 2024, Emacs 30 on Windows does not support color emojis, just black & white.
;;  To set or adjust text scale: C-x C-= to enlarge, C-x C-- to shrink, C-x C-0 to reset.
;;    To modify interactively, S-0 followed by +, -, 0, etc.

;;;; Emoji and Fonts

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+

(defun my/set-fonts-scale (
                           ;; default-font-name
                           ;; default-font-height
                           ;; cjk-font-name
                           ;; cjk-font-scale
                           unicode-font-name
                           unicode-font-scale
                           emoji-font-name
                           emoji-font-scale
                           )
  "Helper function to set the default, CJK and Emoji fonts."
  ;; Set the default font
  ;; (when (member default-font-name (font-family-list))
  ;;   (set-face-attribute 'default nil
  ;;                       :family default-font-name
  ;;                       :height default-font-height)
  ;;   (set-frame-font default-font-name nil t))

  ;; Set the CJK font in the default fontset.
  ;; (when (member cjk-font-name (font-family-list))
  ;;   (dolist (script (list 'han 'kana 'cjk-misc))
  ;;     (set-fontset-font t script cjk-font-name)))

  (when (member unicode-font-name (font-family-list))
    (set-fontset-font t 'unicode unicode-font-name nil 'prepend)
    (set-fontset-font t 'mathematical unicode-font-name nil 'prepend)
    (set-fontset-font t 'symbol unicode-font-name nil 'prepend)
    )

  ;; Set the Emoji font in the default fontset.
  (when (member emoji-font-name (font-family-list))
    (set-fontset-font t 'emoji emoji-font-name nil 'prepend))

  ;; Rescale the CJK and emoji fonts.
  (setq face-font-rescale-alist
        `(;; (,(format ".*%s.*" cjk-font-name) . ,cjk-font-scale)
          (,(format ".*%s.*" unicode-font-name) . ,unicode-font-scale)
          (,(format ".*%s.*" emoji-font-name) . ,emoji-font-scale)
          )))

;;;###autoload
(defun my/emoji-set-font ()
  (interactive)

  (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family)))

  (when (display-graphic-p) ; gui
    ;; (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
    ;; (set-fontset-font t 'mathematical (font-spec :family "Symbola") nil 'prepend) ; best
    ;; ;; https://fonts.google.com/noto/specimen/Noto+Sans+Math
    ;; ;; (set-fontset-font t 'mathematical (font-spec :family "Noto Sans Math") nil 'prepend) ;; 2024-10-26 테스트 DONT
    ;; ;; (set-fontset-font t 'mathematical "DejaVu Math TeX Gyre" nil 'prepend) ; DONT for test
    ;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
    ;; (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top - my choice

    ;; Different computers might need different scaling factors with the same fonts.
    (my/set-fonts-scale
     "Symbola" 0.9 ; unicode
     "Noto Emoji" 0.9)
    )

  (unless (display-graphic-p) ; terminal
    ;; (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
    (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; default face
    )

  ;; (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
  ;; (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
  ;; (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend)
  )

(unless IS-TERMUX
  (add-hook 'after-setting-font-hook #'my/emoji-set-font))

;;;; Fontaine

;; Read the manual: <https://protesilaos.com/emacs/fontaine>

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+

;; terminal-mode is nil
;; A narrow focus package for naming font configurations and then selecting them.
(use-package! fontaine
  :if window-system
  :init
  ;; This is defined in Emacs C code: it belongs to font settings.
  ;; (setq x-underline-at-descent-line nil)
  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; Weights :: Thin ExtraLight Light Regular Medium SemiBold Bold ExtraBold Heavy
  ;; Slopes :: Upright Oblique Italic
  ;; Width :: Normal Extended

  :config
  (setq
   fontaine-presets
   ;; 80 120, 136, 151, 180, 211 ; sarasa mono / term
   ;; 120, 140, 170, 190, 210, 230 ; monoplex kr nerd
   '(
     (small12 :default-height 120)
     (regular14 :default-height 140)
     (regular17 :default-height 170)
     (large19 :default-height 190)
     (large21 :default-height 210)
     (present23
      :default-height 230
      ;; :fixed-pitch-family "Sarasa Term Slab K"
      ;; :fixed-pitch-serif-family "Sarasa Term Slab K"
      :bold-weight extrabold)
     (t
      ;; Following Prot’s example, keeping these for for didactic purposes.
      :line-spacing 3
      ;; :default-family "Sarasa Term K Nerd Font"
      ;; :default-height 151
      :default-family "Monoplex KR Nerd"
      :default-height 140
      :default-weight regular
      ;; :fixed-pitch-family "Sarasa Term K Nerd Font"
      ;; :fixed-pitch-height 151
      ;; :fixed-pitch-weight nil
      ;; :fixed-piath-serif-family nil
      ;; :fixed-pitch-serif-weight nil
      ;; :fixed-pitch-serif-height nil
      :variable-pitch-family "Pretendard Variable"
      ;; :variable-pitch-height 1.0
      ;; :variable-pitch-family nil
      ;; :variable-pitch-weight nil
      :bold-family nil
      :bold-weight bold
      ;; :bold-width extended
      :italic-family nil
      :italic-slant italic)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  ;; (fontaine-set-preset 'regular)
  ;; (set-fontset-font t 'hangul (font-spec :family (face-attribute 'default :family))) ; t or nil ?

  ;; store current preset
  (defun my/fontaine-store-preset ()
    (interactive)
    ;; (message "my/fontaine-store-preset")
    (fontaine-store-latest-preset))

  ;; 한글 사용 위해서 필수!
  (defun my/load-font-cjk ()
    (interactive)
    (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))) ; default face
    ;; (set-fontset-font "fontset-default" 'hangul (font-spec :family "Monoplex KR Nerd")) ;  "Sarasa Term K"
    ;; (set-fontset-font "fontset-default" 'cjk-misc (font-spec :family "Sarasa Term SC" )) ; default face
    ;; (set-fontset-font "fontset-default" 'bopomofo (font-spec :family "Sarasa Term SC" )) ; default face
    ;; (set-fontset-font "fontset-default" 'kana (font-spec :family "Sarasa Term J")) ; default face
    ;; (set-fontset-font "fontset-default" 'han (font-spec :family "Sarasa Term SC")) ; default face
    )

  ;; load @ start-up
  (defun my/fontaine-load-preset ()
    (interactive)

    ;; The other side of `fontaine-restore-latest-preset'.
    ;; (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

    (if (string= (system-name) "jhnuc")
        (fontaine-set-preset 'regular17)
      (fontaine-set-preset 'regular14))

    ;; 1) go default spacemacs themes
    (my/load-font-cjk))
  (add-hook 'after-setting-font-hook #'my/fontaine-load-preset 90)

  ;; load @ theme change
  ;; (set-fontset-font "fontset-default" 'hangul (font-spec :family "BHGoo") nil 'append) ; 구본형체 테스트
  ;; (defun my/fontaine-apply-current-preset ()
  ;;   (interactive)
  ;;   ;; (fontaine-apply-current-preset)
  ;;   (my/load-font-cjk))
  ;; (add-hook 'doom-load-theme-hook 'my/fontaine-apply-current-preset 80)
  )

;;;; Show Font (preview fonts)

;; Read the manual: <https://protesilaos.com/emacs/show-font>
(use-package! show-font
  :if (display-graphic-p)
  :commands (show-font-select-preview show-font-list)
  :config
  ;; (setq show-font-pangram 'fox)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"))

;;;; doom-modeline

(after! doom-modeline
  (doom-modeline-def-modeline
    'main
    '(eldoc
      bar
      persp-name
      ;; workspace-name - conflict tab-bar
      window-number
      modals
      input-method
      matches
      follow
      buffer-info
      remote-host
      buffer-position
      word-count
      parrot
      selection-info)
    '(compilation
      objed-state
      misc-info
      battery
      grip
      irc
      mu4e
      gnus
      github
      debug
      repl
      lsp
      minor-modes
      indent-info
      buffer-encoding
      major-mode
      process
      vcs
      check
      time))

  (setq doom-modeline-time nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-minor-modes nil)
  ;; (setq doom-modeline-battery nil)
  ;; (setq Info-breadcrumbs-in-mode-line-mode nil)
  (setq doom-modeline-support-imenu t)

  ;; UTF-8 is default encoding remove it from modeline
  ;; frap-dot-doom/ui-old.el
  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                            (eq buffer-file-coding-system 'utf-8)))))
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

  (setq doom-modeline-enable-word-count nil)
  ;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mod)) ; org-mode

  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-modification-icon t)

  (setq doom-modeline-height 35)
  (setq doom-modeline-bar-width 4)

  (setq doom-modeline-persp-name t) ; doom nil
  (setq doom-modeline-window-width-limit (- fill-column 5))

  (setq doom-modeline-repl t)
  (setq doom-modeline-github t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-hud nil)
  ;; (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ; default 'auto

  (remove-hook 'display-time-mode-hook #'doom-modeline-override-time)
  (remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-time))

;;;; DONT doom-themes

;; (use-package! doom-themes
;;   ;; improve integration w/ org-mode
;;   :hook ((doom-load-theme . doom-themes-org-config)
;;          (doom-load-theme . doom-themes-visual-bell-config))
;;   :init
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic nil) ; if nil, italics is universally disabled
;;   (setq doom-themes-to-toggle
;;         (let ((hr (nth 2 (decode-time))))
;;           (if (or (< hr 6) (< 19 hr)) ; between 8 PM and 7 AM
;;               '(doom-one doom-homage-white) ; load dark theme first
;;             '(doom-homage-white doom-one))))
;;   (setq doom-theme (car doom-themes-to-toggle))
;;   ;; (load-theme doom-theme t)

;;   (defun my/doom-themes-toggle ()
;;     (interactive) (load-theme doom-theme t))
;;   ;; (add-hook 'doom-after-reload-hook #'my/doom-themes-toggle)
;;   )

;;;; celestial-mode-line

(use-package! celestial-mode-line
  :after time
  :init
  (setq celestial-mode-line-update-interval 3600) ; default 60
  (setq celestial-mode-line-sunrise-sunset-alist
        '((sunrise . "🌅") (sunset . "🌄")))
  (setq celestial-mode-line-phase-representation-alist
        '((0 . "🌚") (1 . "🌛") (2 . "🌝") (3 . "🌜")))
  :config (celestial-mode-line-start-timer)
  )

;;;; keycast tab-bar

(use-package! keycast
  :config
  ;; (setq keycast-tab-bar-minimal-width 50) ; 40
  ;; (setq keycast-tab-bar-format "%10s%k%c%r")

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
  (dolist (event
           '(mouse-event-p mouse-movement-p
             mwheel-scroll
             handle-select-window
             mouse-set-point
             mouse-drag-region
             dired-next-line ; j
             dired-previous-line ; k
             next-line
             previous-line
             evil-next-line ; j
             evil-previous-line ; k
             evil-forward-char ; l
             evil-backward-char ; h
             pixel-scroll-interpolate-up ; <prior> page-up
             pixel-scroll-interpolate-down ; <next> page-down

             org-cycle
             ;; toggle-input-method
             ;; block-toggle-input-method
             ;; evil-formal-state
             ;; evil-force-normal-state

             ;; 2023-10-02 Added for clojure-dev
             ;; lsp-ui-doc--handle-mouse-movement
             ignore-preserving-kill-region
             ;; pdf-view-text-region
             ;; pdf-view-mouse-set-region
             ;; mouse-set-region
             ))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

;;;; info-colors Info & Help

;; Info 모드 Node 이동
(use-package! info+
  :commands (info info-display-manual)
  :config
  (setq Info-fontify-angle-bracketed-flag nil)
  (add-hook 'Info-mode-hook (lambda () (require 'info+)))
  ;; (define-key Info-mode-map (kbd "C-p") 'Info-prev)
  ;; (define-key Info-mode-map (kbd "C-n") 'Info-next)

  )

(after! info
  (add-to-list 'Info-directory-list
               (expand-file-name "~/git/junghan0611/sicp-info/")))

;;;; custom eww

(after! eww
  (add-to-list 'evil-buffer-regexps '("\\*eww\\*" . normal))

  (require 'eww-load) ; custom module

  ;; Shr group: Simple HTML Renderer를 의미한다. 여기 설정을 바꾸면 faces를 수정할 수 있음
  ;; Make EWW look like the rest of Emacs
  ;; (setq shr-max-width fill-column)

  (setq eww-browse-url-new-window-is-tab nil ; doom tab-bar
        shr-max-image-proportion 0.6 ; 0.8
        shr-discard-aria-hidden t ; nil
        shr-bullet "• "
        ;; shr-image-animate nil
        ;; shr-inhibit-images t
        eww-header-line-format " %u"
        eww-buffer-name-length 80 ; 40
        eww-form-checkbox-selected-symbol "☒" ; default "[X]"
        eww-readable-urls '("yes24"
                            "naver"
                            "daum")
        )

  ;;  shr-use-fonts nil ; I go back and forth on this one
  ;;  shr-use-colors nil
  ;;  ;; shr-folding-mode nil

  ;; Sometimes EWW makes web pages unreadable by adding a bright background.
  ;; Do not colorize backgrounds at all.
  ;; (advice-add #'shr-colorize-region :around #'ignore)

  ;; Allow switching to these buffers with `C-x b'
  (add-hook 'eww-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Default Browser
  ;; (setq browse-url-browser-function 'eww-browse-url
  ;;       browse-url-secondary-browser-function 'browse-url-default-browser)

  ;; change default browser as eww
  ;; (setq +lookup-open-url-fn #'eww) ; doom browse-url

  ;; This function allows Imenu to offer HTML headings in EWW buffers,
  ;; helpful for navigating long, technical documents.
  ;; https://github.com/alphapapa/unpackaged.el
  (defun +eww/imenu-eww-headings ()
    "Return alist of HTML headings in current EWW buffer for Imenu.
Suitable for `imenu-create-index-function'."
    (let ((faces '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-heading)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (cl-loop
           for
           next-pos
           =
           (next-single-property-change (point) 'face)
           while
           next-pos
           do
           (goto-char next-pos)
           for
           face
           =
           (get-text-property (point) 'face)
           when
           (cl-typecase
               face
             (list (cl-intersection face faces))
             (symbol (member face faces)))
           collect
           (cons (buffer-substring (point-at-bol) (point-at-eol)) (point))
           and
           do
           (forward-line 1))))))

  ;; https://github.com/alphapapa/unpackaged.el
  (add-hook
   'eww-mode-hook
   (lambda ()
     (setq-local imenu-create-index-function #'+eww/imenu-eww-headings)))

  ;; temporary 's'
  (evil-define-key 'normal eww-link-keymap "s" 'ace-link-eww)
  (evil-define-key 'normal eww-mode-map "s" 'ace-link-eww)
  )

;;;;; eww image-slicing : not working

;; (after! eww
;;   (require 'image-slicing)
;;   (add-to-list 'shr-external-rendering-functions '(img . image-slicing-tag-img))
;;   (push #'image-slicing-mode eww-after-render-hook)
;;   )

;;;; mode-minder

(use-package! mode-minder :defer t :commands mode-minder)

;;;; inhibit-double-buffering

;; prevents some cases of emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;;; hl-todo

;; TODO FIXME BUG FAIL REVIEW THEM HACK KLUDGE HOLD
;; NEXT
;; DEPRECATED DONT NOTE DONE OKAY XXX
;; TODO: test

;; Use full hl-todo keywords
(after! hl-todo
  (setq hl-todo-highlight-punctuation "") ; back to default
  (setq hl-todo-keyword-faces
        '( ;; For reminders to change or add something at a later date.
          ("TODO" error bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ("BUG" error bold)

          ("NEXT" warning bold)
          ("PROG" warning bold)

          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ("THEM" font-lock-keyword-face bold)

          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ("TEMP" font-lock-constant-face bold)
          ("KLUDGE" font-lock-constant-face bold)
          ("HOLD" font-lock-constant-face bold)

          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ("FAIL" font-lock-doc-face bold)
          ("DONT" font-lock-doc-face bold)

          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("DONE" success bold)
          ("OKAY" success bold)

          ("XXXX*" font-lock-constant-face bold) ; XXX
          )))

;;;; dired

(after! dired
  (setq dired-make-directory-clickable t) ; Emacs 29.1, doom t
  (setq dired-free-space nil) ; Emacs 29.1, doom first

  ;; Better dired flags:
  ;; `-l' is mandatory
  ;; `-a' shows all files
  ;; `-h' uses human-readable sizes
  ;; `-F' appends file-type classifiers to file names (for better highlighting)
  ;; -g     like -l, but do not list owner
  (setq dired-listing-switches "-AGFhgv --group-directories-first --time-style=long-iso") ;; doom "-ahl -v --group-directories-first"
  (setq dired-recursive-copies 'always ; doom 'always
        dired-dwim-target t) ; doom t
  (setq dired-ls-F-marks-symlinks t ; doom nil -F marks links with @
        delete-by-moving-to-trash t) ; doom nil

  (setq dired-use-ls-dired t)  ; doom t
  (setq dired-do-revert-buffer t) ; doom nil
  ;; (setq dired-clean-confirm-killing-deleted-buffers t) ; doom nil
  (setq dired-kill-when-opening-new-dired-buffer t) ; doom nil

  (require 'wdired)
  (setq wdired-allow-to-change-permissions t) ; doom nil
  (setq wdired-create-parent-directories t)

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (setq-local truncate-lines t) ; Do not wrap lines
              ;; (visual-line-mode -1)
              (hl-line-mode 1)))
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (remove-hook 'dired-mode-hook 'dired-omit-mode)
  )

;;;; dired-preview

(use-package! dired-preview
  :after dired
  :commands dired-preview
  :init
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20)) ;; => 1048576
  (defun my-dired-preview-to-the-right ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-in-side-window)
      (side . right)
      (width . 0.3)))
  ;; default' dired-preview-display-action-alist-dwim
  (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
  )

;;;; nerd-icons-dired

(use-package! nerd-icons-dired
  :if window-system
  :hook (dired-mode . nerd-icons-dired-mode))

;;;; winum

(use-package! winum
  :demand t
  :config
  (defun my/winum-assign-custom ()
    (cond
     ;; 0 minibuffer, 9 treemacs, 8 imenu-list
     ((equal (buffer-name) "*Ilist*") 8)
     ((equal (buffer-name) "*Calculator*") 7)
     ;; ((equal (buffer-name) "*Flycheck errors*") 6) ; use flymake
     )
    )

  (set-face-attribute 'winum-face nil :weight 'bold)
  (add-to-list 'winum-assign-functions #'my/winum-assign-custom)

  (setq winum-scope                      'frame-local
        winum-auto-assign-0-to-minibuffer t ; important
        winum-ignored-buffers '(" *LV*" " *which-key*")
        winum-auto-setup-mode-line nil
        winum-reverse-frame-list nil)
  (winum-mode +1)
  )


;;;; lin - hl-line

;;  “LIN locally remaps the hl-line face to a style that is optimal
;;  for major modes where line selection is the primary mode of
;;  interaction.”  In otherwords, ~lin.el~ improves the highlighted
;;  line behavior for the competing contexts.
;; :init (global-hl-line-mode) ; doom default

(use-package! lin
  :init
  (global-hl-line-mode +1)
  :config
  ;; You can use this to live update the face:
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-cyan);; blue
  (lin-global-mode 1))

;;;; DONT my/switch-themes-toggle

;; (defun my/switch-themes-toggle ()
;;   (interactive)
;;   ;; (message "my/switch-themes-toggle")

;;   (setq doom-theme (car modus-themes-to-toggle)) ; modus-themes-to-toggle
;;   (load-theme doom-theme t)
;;   (my/modus-themes-custom-faces) ;; (my/ef-themes-custom-faces)
;;   )

;; (add-hook 'doom-after-init-hook #'my/switch-themes-toggle)
;; (add-hook 'doom-after-reload-hook #'my/switch-themes-toggle)

;;;; DONT auto-highlight-symbol

;; (use-package! auto-highlight-symbol
;;   :commands (auto-highlight-symbol-mode)
;;   :init
;;   (setq ahs-idle-interval 1.0) ; default 1.0
;;   (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode))

;;;; breadcrumb

(use-package! breadcrumb
  :defer 2
  :hook
  (eglot-connect . breadcrumb-mode))

;; (use-package! breadcrumb
;;   :defer 1
;;   :init
;;   ;; (add-hook 'prog-mode-hook 'breadcrumb-local-mode) ; work well with eglot - conflict with lsp-mode
;;   (add-hook 'emacs-lisp-mode-hook 'breadcrumb-local-mode)
;;   (add-hook 'markdown-mode-hook 'breadcrumb-local-mode)
;;   ;; (add-hook 'org-mode-hook 'breadcrumb-local-mode)

;;   ;; (add-hook 'text-mode-hook 'breadcrumb-local-mode)
;;   (setq breadcrumb-idle-time 5) ; 1
;;   ;; (setq breadcrumb-imenu-crumb-separator "/")

;;   (setq breadcrumb-project-max-length 0.1) ; 0.3
;;   (setq breadcrumb-imenu-max-length 0.2) ; 0.3

;;   ;; Make Org heading style the same.
;;   ;; https://github.com/joaotavora/breadcrumb/issues/35
;;   (defun breadcrumb-org-crumbs ()
;;     "Get the chain from the top level heading down to current heading."
;;     (org-format-outline-path
;;      (org-get-outline-path t) (1- (frame-width)) nil " > "))
;;   (defun breadcrumb--header-line ()
;;     "Helper for `breadcrumb-headerline-mode'."
;;     (let* ((imenu-crumbs
;;             (if (eq major-mode 'org-mode)
;;                 'breadcrumb-org-crumbs
;;               'breadcrumb-imenu-crumbs))
;;            (x
;;             (cl-remove-if
;;              #'seq-empty-p
;;              (mapcar #'funcall `(breadcrumb-project-crumbs ,imenu-crumbs)))))
;;       (mapconcat #'identity x (propertize " : " 'face 'breadcrumb-face)))))

;;; :app

;;;; :app calendar

;; calendar

;;;; :app emms

;; emms

;; A media player for music no one's heard of

;; (after! emms
;;   (emms-mpris-enable)
;;   (emms-source-file-default-directory ews-music-directory)
;;   (emms-browser-covers #'emms-browser-cache-thumbnail-async)
;;   (("<XF86AudioPrev>" . emms-previous)
;;    ("<XF86AudioNext>" . emms-next)
;;    ("<XF86AudioPlay>" . emms-pause))
;;   )

;;;; :app @yeetube

(use-package! yeetube
  :defer t
  :after emms
  :init (define-prefix-command 'my/yeetube-map)
  :config
  (setf yeetube-mpv-disable-video t) ;; Disable video output
  (setf yeetube-play-function #'emms-play-url)
  :bind (("C-c y" . 'my/yeetube-map)
         :map my/yeetube-map
         ("s" . 'yeetube-search)
         ("b" . 'yeetube-play-saved-video)
         ("d" . 'yeetube-download-videos)
         ("p" . 'yeetube-mpv-toggle-pause)
         ("v" . 'yeetube-mpv-toggle-video)
         ("V" . 'yeetube-mpv-toggle-no-video-flag)
         ("k" . 'yeetube-remove-saved-video)))


;;;; :app rss

;; (rss +org +youtube)        ; emacs as an RSS reader

;; gc copy-link
(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq rmh-elfeed-org-files (list (my/org-elfeed-file))) ; default ~/org/elfeed.org
  ;; org-directory
  (setq elfeed-search-filter "") ; "@6-months-ago") ;;  "@1-month-ago +unread"
  (setq elfeed-search-title-max-width 90) ; default 70
  ;; (add-hook 'elfeed-search-mode-hook #'elfeed-update)
  )

(after! elfeed-tube
  (require 'elfeed-tube)
  ;; (setq elfeed-tube-invidious-url "https://vid.puffyan.us")
  (setq elfeed-tube-captions-languages '("en" "ko" "englsh (auto generated)")))

;;;; DONT :app @mastodon

;; (use-package! tp)

;;  "――――――――――――"
;;  "======================================================================"
;; (use-package! mastodon)
;;   ;;:init
;;   ;; (require 'mastodon-toot)
;;   ;; (setq mastodon-tl--horiz-bar "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
;;   ;; (setq mastodon-tl--highlight-current-toot t
;;   ;;       mastodon-tl--tag-timeline-tags t
;;   ;;       mastodon-tl--show-avatars t)
;;   ;;:config
;;   ;; The default emojis take two characters for me
;;   ;; (setq mastodon-tl--symbols
;;   ;;       '((reply "" . "R")
;;   ;;         (boost "" . "B")
;;   ;;         (favourite "" . "F")
;;   ;;         (bookmark "" . "K")
;;   ;;         (media "" . "[media]")
;;   ;;         (verified "" . "V")
;;   ;;         (locked "" . "[locked]")
;;   ;;         (private "" . "[followers]")
;;   ;;         (direct "" . "[direct]")
;;   ;;         (edited "" . "[edited]")))
;;   ;; (mastodon-discover) ; context-mode
;;   ;; (add-hook 'mastodon-toot-mode-hook
;;   ;;           (lambda ()
;;   ;;             (auto-fill-mode -1) ; default
;;   ;;             (display-line-numbers-mode -1)
;;   ;;             (jinx-mode 1)))
;;   )

;;;;; my/dired-attach-to-mastodon

;; (defun my/dired-attach-to-mastodon (files mastodon-buffer)
;;   (interactive
;;    (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
;;          (or (cl-loop for buf being the buffers
;;                       if (eq (buffer-local-value 'mastodon-toot-mode buf) t)
;;                       return buf)
;;              (user-error "No buffer found!"))))
;;   (unless files
;;     (user-error "No (non-directory) files selected"))
;;   (with-current-buffer mastodon-buffer
;;     (dolist (file files)
;;       (mastodon-toot--attach-media
;;        file
;;        (read-from-minibuffer (format "Description for %s: " file))))))

;;;;; HACK mastodon-toot--attach-media

;; (after! mastodon
;;   ;; string-to-unibyte 함수가 문제인데. 대체 하면 된다. 왜 이걸 쓰는 것인가?
;;   (defun mastodon-http--read-file-as-string (filename &optional url)
;;     "Read a file FILENAME as a string.
;; Used to generate image preview.
;; URL means FILENAME is a URL."
;;     (with-temp-buffer
;;       (if url
;;           (url-insert-file-contents filename)
;;         (insert-file-contents filename))
;;       (string-to-multibyte (buffer-string))
;;       ;; (string-to-unibyte (buffer-string))
;;       ))
;;   )

;;;; :app @ebooks

;;;; :app @exercism

(use-package! exercism
  :defer t
  :if (not IS-TERMUX)
  :custom (exercism-display-tests-after-run t)
  :commands exercism
  :config
  (defun ert/eval-and-run-all-tests-in-buffer ()
    "Deletes all loaded tests from the runtime, evaluates the current buffer and runs all loaded tests with ert."
    (interactive)
    (ert-delete-all-tests)
    (eval-buffer)
    (ert 't))
  )

;;;; :app @osm OpenStreetMaps

;; Very cool and the nice thing is it integrates itself with the built-in
;; bookmarking system. So you can bookmark places (or store them as org links)
;; and jump to them whenever needed.

(use-package! osm
  :defer t
  :bind ("C-c M" . osm-prefix-map) ;; Alternatives: `osm-home' or `osm'
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information
  ;; :config
  ;; Add custom servers, see also https://github.com/minad/osm/wiki
  ;; (osm-add-server 'myserver
  ;;   :name "My tile server"
  ;;   :group "Custom"
  ;;   :description "Tiles based on aerial images"
  ;;   :url "https://myserver/tiles/%z/%x/%y.png?apikey=%k")
  )
;;;;; calibredb

;; apt-get install calibre -y
(use-package! calibredb
  :defer t
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Documents/calibre/"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  ;; (setq calibredb-library-alist '(("~/Documents/calibre/")
  ;;                                 ("~/sync/markdown/epub/")))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))

;;;;; nov

;; evil-collection/modes/nov/evil-collection-nov.el
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :commands (nov-org-link-follow nov-org-link-store)
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters "nov"
                             :follow 'nov-org-link-follow
                             :store 'nov-org-link-store))
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up
        :n "d" 'nov-scroll-up
        :n "u" 'nov-scroll-down)

  (defun +nov-mode-setup ()
    "Tweak nov-mode to our liking."
    (face-remap-add-relative 'variable-pitch
                             :family "Pretendard Variable"
                             :height 1.1
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.0)
    (variable-pitch-mode 1)
    (setq-local line-spacing 0.2
                ;; next-screen-context-lines 4
                shr-use-colors nil)
    (when (featurep 'hl-line-mode)
      (hl-line-mode -1))
    (when (featurep 'font-lock-mode)
      (font-lock-mode -1))
    ;; Re-render with new display settings
    (nov-render-document)
    ;; Look up words with the dictionary.
    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition))
  (add-hook 'nov-mode-hook #'+nov-mode-setup 80)
  (setq font-lock-global-modes '(not nov-mode))
  )

;;;; :app @anddo for todo

(progn
  (require 'anddo)
  ;; fix path
  (defun anddo--create-tables ()
    (unless anddo--db
      (setq-local anddo--db
                  (sqlite-open
                   (expand-file-name "resources/anddo.sqlite" org-directory)))
      (sqlite-execute anddo--db "create table if not exists item (id integer primary key, status text, subject text, body text, entry_time text, modification_time text)")))
  )

;;;; :app @gif-screencast

(use-package! gif-screencast
  :defer 5
  :commands gif-screencast
  :bind (:map gif-screencast-mode-map
              ("<f8>". gif-screencast-toggle-pause)
              ("<f9>". gif-screencast-stop))
  :init
  (setq gif-screencast-output-directory org-screenshot-path)
  ;; :init
  ;; (setq gif-screencast-args '("-x")
  ;;       gif-screencast-cropping-program ""
  ;;       gif-screencast-capture-format "ppm")
  :config
  (defun gif-screencast--generate-gif (process event)
    "Generate GIF file."
    (when process
      (gif-screencast-print-status process event))
    (message "Compiling GIF with %s..." gif-screencast-convert-program)
    (let* ((output-filename (expand-file-name
                             (format-time-string
                              (concat "%Y%m%dT%H%M%S--screencast." gif-screencast-output-format) ; "output-%F-%T."
                              (current-time))
                             (or (and (file-writable-p gif-screencast-output-directory)
                                      gif-screencast-output-directory)
                                 (read-directory-name "Save output to directory: "))))
           (delays (cl-loop for (this-frame next-frame . _)
                            on gif-screencast--frames
                            by #'cdr
                            ;; Converters delays are expressed in centiseconds.
                            for delay = (when next-frame
                                          (format "%d" (* 100 (float-time
                                                               (time-subtract (gif-screencast-frame-timestamp next-frame)
                                                                              (gif-screencast-frame-timestamp this-frame))))))
                            when next-frame
                            collect delay))
           (delays (cons gif-screencast-first-delay delays))
           (files-args (cl-loop for frame in gif-screencast--frames
                                for delay in delays
                                append (list "-delay" delay (gif-screencast-frame-filename frame))))
           (convert-args (append gif-screencast-convert-args
                                 files-args
                                 (list output-filename)))
           (convert-process (gif-screencast--start-process
                             gif-screencast-convert-program
                             convert-args)))
      (set-process-sentinel convert-process (lambda (process event)
                                              (gif-screencast-print-status process event)
                                              (when (and gif-screencast-want-optimized
                                                         (eq (process-status process) 'exit)
                                                         (= (process-exit-status process) 0))
                                                (gif-screencast-optimize output-filename))
                                              (when (and gif-screencast-autoremove-screenshots
                                                         (eq (process-status process) 'exit)
                                                         (= (process-exit-status process) 0))
                                                (dolist (f gif-screencast--frames)
                                                  (delete-file (gif-screencast-frame-filename f))))))))
  )


;;; :os tty

;;;; term-keys

(use-package! term-keys
  :unless window-system
  :config
  (unless (display-graphic-p) ; terminal
    (term-keys-mode t)))

;;;;; usage

;; (require 'term-keys-kitty)
;; (with-temp-buffer
;;   (insert (term-keys/kitty-conf))
;;   (write-region (point-min) (point-max) "~/kitty-for-term-keys.conf"))

;;; +user-configs.el ends here
