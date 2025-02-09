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

;;; Commentary:

;;; Code:

;; (when (is-doomemacs)

;;; global-unset-key

(global-unset-key (kbd "M-a"))  ; unset forward-sentence -> use ')'
(global-unset-key (kbd "M-c"))  ; unset capitalize-word
(global-unset-key (kbd "M-e"))  ; unset backward-sentence -> use '('

;;; Emacs Keys

;; (global-set-key (kbd "M-g j") 'side-journal-toggle-notes)
;; (global-set-key (kbd "<f2>") 'eval-last-sexp)

;; (global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "<mouse-8>") 'evil-jump-backward)
(global-set-key (kbd "<mouse-9>") 'evil-jump-forward)

(global-set-key (kbd "C-x x v") 'my/view-text-file-as-info-manual)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

;; Confession time: vi's killing up to a char is better than emacs, so let's change things.
(global-set-key (kbd "M-z") #'zap-up-to-char)

;; Very convenient to close current buffer
;; 확인하자. 메이저 모드 키에 충돌 날라.
;; (define-key evil-normal-state-map (kbd ",`") #'spacemacs/kill-this-buffer)

;; (global-set-key (kbd "C-M-i") 'completion-at-point)
;; (global-set-key (kbd "C-M-;") 'pp-eval-expression)

;; false input
;; (global-unset-key (kbd "M-ESC ESC")) ; 'keyboard-escape-quit
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)

;; turn off kill emacs binding
;; (global-unset-key (kbd "C-x C-c")) ; save-buffer-and-kill-emacs

;; TODO Check!
;; (global-set-key (kbd "s-`'") #')
;; (global-set-key (kbd "s-9") 'spacemacs/switch-to-minibuffer-window)

;; (global-set-key (kbd "C-1") 'kill-this-buffer)
;; (global-set-key (kbd "C-<down>") (kbd "C-u 1 C-v"))
;; (global-set-key (kbd "C-<up>") (kbd "C-u 1 M-v"))
;; (global-set-key (kbd "M-/") #'hippie-expand)
;; (global-set-key (kbd "C-x C-j") 'dired-jump) ; what is?
;; (global-set-key (kbd "C-c r") 'remember) ; what is?

;;; NOTE Switch Buffer / Tab-bar, Tab-line / Workspace (persp-mode)

;;;; DONT 1) Window : winum M-[0-9]

;; 직관적인 원키
;; (when (locate-library "winum")
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
;;     (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
;;     (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
;;     (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
;;     (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
;;     (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
;;     (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
;;     (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
;;     (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
;;     (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
;;     )
;;   )

;;;; 2) Worksapce : SPC Tab [0-9]

;; +workspace/switch-to-0
;; Default

;;;; 3) Buffer : tab-line : s-[, s-]

;; Fast

;;;; 4) Tab-bar :

;;; Packages

;;;; vertico

(with-eval-after-load 'vertico
  ;; M-V -> vertico-multiform-vertical
  ;; M-G -> vertico-multiform-grid
  ;; M-F -> vertico-multiform-flat
  ;; M-R -> vertico-multiform-reverse
  ;; M-U -> vertico-multiform-unobtrusive

  ;; 2023-12-02
  (define-key vertico-map (kbd"C-<return>") 'vertico-quick-exit)
  ;; (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key minibuffer-local-map (kbd "M-r") 'vertico-repeat)

  (define-key vertico-map (kbd "RET")   'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL")   'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)

  ;; default map
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "C-S-j") #'vertico-scroll-up)
  (define-key vertico-map (kbd "C-S-k") #'vertico-scroll-down)
  (define-key vertico-map (kbd "C-M-j") #'vertico-next-group)
  (define-key vertico-map (kbd "C-M-k") #'vertico-previous-group)


  (unless (display-graphic-p) ; terminal
    (define-key vertico-map (kbd "M-<return>") #'vertico-exit-input))
  )

;;;; eldoc

(global-set-key (kbd "C-M-'") 'eldoc-toggle)

;;;; dired

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode
    (kbd "C-c l") 'org-store-link
    (kbd "C-x /") 'dired-narrow-regexp
    (kbd ".") 'consult-line
    ;; (kbd "K") 'dired-kill-subdir
    (kbd "K") 'dired-do-kill-lines
    ;; (kbd "F") 'evil-avy-goto-line-below ;; 2024-01-25 useful
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file
    ;; (kbd "RET") 'dired-find-file
    (kbd "S-<return>") 'dired-find-file-other-window
    ;; evil-force-normal-state
    (kbd "q") 'casual-dired-tmenu
    (kbd "S-SPC") 'dired-toggle-marks
    )
  )

;;;; corfu

(when (locate-library "corfu")
  ;; Tab 이 자동 완성이면 괄호 점프랑 충돌 난다.
  ;; 2024-11-10 C-9, C-0 직관적이다.
  ;; C-j/k C-n/p 는 직관적인 기본 설정이므로 건들이지 않는다.
  (with-eval-after-load 'corfu
    (evil-define-key '(insert) prog-mode-map (kbd "C-0") 'jump-out-of-pair) ; useful
    (evil-define-key '(insert) prog-mode-map (kbd "C-0") 'jump-out-of-pair)
    (evil-define-key '(insert) prog-mode-map (kbd "C-9") 'jump-backward-pair)
    (evil-define-key '(insert) prog-mode-map (kbd "C-9") 'jump-backward-pair)

    ;; 기본 설정을 건들지 말자 왠만하면
    ;;   ;; (evil-define-key '(insert) prog-mode-map (kbd "<tab>") 'jump-out-of-pair)
    ;;   ;; (evil-define-key '(insert) prog-mode-map (kbd "TAB") 'jump-out-of-pair)
    ;;   ;; (evil-define-key '(insert) prog-mode-map (kbd "<backtab>") 'jump-backward-pair)
    ;;   ;; (evil-define-key '(insert) prog-mode-map (kbd "S-<iso-lefttab>") 'jump-backward-pair)
    ;;   ;; 엔터는 컴플리션 되거나 뉴라인 인덴트 되어야 한다.
    ;;   ;; (evil-define-key '(insert) corfu-map (kbd "<return>") 'corfu-insert)
    ;;   ;; (evil-define-key '(insert) prog-mode-map (kbd "<return>") 'newline-and-indent) ;; <return>
    )
  )

;;;; cape-map

(progn
  (define-prefix-command 'my-cape-map)
  (define-key global-map (kbd "C-c SPC") 'my-cape-map)
  (let ((map my-cape-map))
    (define-key map (kbd "a") 'cape-abbrev)
    (define-key map (kbd "SPC") 'cape-dabbrev)
    (define-key map (kbd "d") 'cape-dict)
    (define-key map (kbd "e") 'hippie-expand)
    (define-key map (kbd "i") 'completion-at-point)
    (define-key map (kbd "t") 'complete-tag)
    (define-key map (kbd "h") 'cape-history)
    (define-key map (kbd "y") 'yasnippet-capf)
    (define-key map (kbd "f") 'cape-file)
    (define-key map (kbd "k") 'cape-keyword)
    (define-key map (kbd "s") 'cape-symbol)
    (define-key map (kbd "l") 'cape-line)
    ;; (define-key map (kbd "r") 'cape-rfc1345)
    (define-key map (kbd "\\") 'cape-tex)
    (define-key map (kbd "_") 'cape-tex)
    )
  )

;;;; Remap all C-c prefix keys to M-c?

;; static map
(define-key key-translation-map (kbd "M-c") (kbd "C-c"))

;; dynamic map - copying
;; (progn
;;   (defun my-generate-c-c-map ()
;;     (let ((map (make-sparse-keymap)))
;;       (set-keymap-parent map (lookup-key global-map (kbd "C-c")))
;;       map))
;;   (global-set-key (kbd "M-c") (my-generate-c-c-map)))

;;;; embark - doom vs. spacemacs style

;; C-; embark-ack ; doom default
;; C-c C-; embark-export
;; C-c C-e ; +vertico/embark-export-write

(global-set-key (kbd "M-o") 'embark-act) ;; spacemacs bindings
(global-set-key (kbd "M-O") 'embark-dwim) ;; good alternative: M-.

(global-set-key (kbd "C-h B") 'embark-bindings) ;; alternative for `describe-bindings'

;;;; activities

(when (locate-library "activities")
  (define-prefix-command 'activities-map)
  (define-key global-map (kbd "C-c C-a") 'activities-map)
  (let ((map activities-map))
    (define-key map (kbd "n") #'activities-new)
    (define-key map (kbd "g") #'activities-revert)
    (define-key map (kbd "s") #'activities-suspend)
    ;; For convenience, we also bind `activities-resume' to "C-x C-a C-a", so
    ;; the user need not lift the Control key.
    (define-key map (kbd "k") #'activities-kill) ; Alias for '-suspend'
    (define-key map (kbd "r") #'activities-resume)
    (define-key map (kbd "RET") #'activities-switch)
    (define-key map (kbd "l") #'activities-list)
    )
  )

;;;; org-remark : C-c r

;; Key-bind `org-remark-mark' to global-map so that you can call it
;; globally before the library is loaded.

(when (locate-library "org-remark")
  (define-prefix-command 'org-remark-map)

  (global-set-key (kbd "<f10> M-r") 'org-remark-mark)

  (define-key global-map (kbd "C-c r") 'org-remark-map)
  (let ((map org-remark-map))
    ;; The rest of keybidings are done only on loading `org-remark'
    (define-key map (kbd "m") #'org-remark-mark)
    (define-key map (kbd "l") #'org-remark-mark-line)
    (define-key map (kbd "k") #'org-remark-change))

  (with-eval-after-load 'org-remark
    (define-key org-remark-mode-map (kbd "C-c r o") #'org-remark-open)
    (define-key org-remark-mode-map (kbd "C-c r r") #'org-remark-remove)
    (define-key org-remark-mode-map (kbd "C-c r ]") #'org-remark-view-next)
    (define-key org-remark-mode-map (kbd "C-c r [") #'org-remark-view-prev)
    ;; (define-key org-remark-mode-map (kbd "C-c r n") #'org-remark-view-next)
    ;; (define-key org-remark-mode-map (kbd "C-c r p") #'org-remark-view-prev)
    )
  )

;;;; puni

(when (locate-library "puni")
  (with-eval-after-load 'puni
    ;; /corgi-packages/corgi-bindings/corgi-keys.el:111
    ;; normal visual
    (evil-define-key '(normal visual) prog-mode-map (kbd ">") 'puni-slurp-forward)
    (evil-define-key '(normal visual) prog-mode-map (kbd "<") 'puni-barf-forward)

    ;; The latter two are "text objects" that can be combined with other vim-style operations
    ;; - ~yL~ copy next sexp (paste with ~p~
    ;; - ~dL~ delete next sexp
    ;; - ~cL~ "change" sexp (delete and switch to insert mode)
    (evil-define-key '(normal visual) prog-mode-map (kbd "L") 'puni-forward-sexp)
    (evil-define-key '(normal visual) prog-mode-map (kbd "H") 'puni-backward-sexp)

    (evil-define-key '(normal visual) prog-mode-map (kbd "M-l") 'puni-end-of-sexp)
    (evil-define-key '(normal visual) prog-mode-map (kbd "M-h") 'puni-beginning-of-sexp)
    )
  )

;;;; Expand-region

;; (global-set-key (kbd "C-=") 'eli/expand-region)

(when (locate-library "expand-region")
  (with-eval-after-load 'expand-region
    (evil-define-key '(normal visual) prog-mode-map (kbd "M-<up>") 'er/expand-region)
    (evil-define-key '(normal visual) prog-mode-map (kbd "M-<down>") 'er/contract-region)
    (evil-define-key '(normal visual) org-mode-map (kbd "M-<up>") 'er/expand-region)
    (evil-define-key '(normal visual) org-mode-map (kbd "M-<down>") 'er/contract-region)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-<up>") 'er/expand-region)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-<down>") 'er/contract-region)
    )
  )

;;;; exercism / leetcode

;; (global-set-key (kbd "M-g =") 'my/format-buffer)

(when (locate-library "exercism")
  (global-set-key (kbd "M-g M-e") 'exercism))

;; (when (locate-library "leetcode")
;;   (global-set-key (kbd "M-g l") 'leetcode))

;;;; side-notes

;; add devdocs-browser

(when (locate-library "side-notes")
  (global-set-key (kbd "M-g M-s") 'side-notes-toggle-notes))

;;;; markdown-mode-map

(with-eval-after-load 'markdown-mode
  ;; 문단을 한 라인으로 합쳐 준다. 구글 번역기 돌릴 때 매우 유용.
  (evil-define-key '(normal insert visual) markdown-mode-map (kbd "C-M-q") 'my/unfill-paragraph-or-region)
  )

;;;; window

;; 편집 창 포커스 이동을 간단하게
(progn
  (global-set-key (kbd "M-s-l") 'evil-window-right)
  (global-set-key (kbd "M-s-h") 'evil-window-left)
  ;; (global-set-key (kbd "M-s-]") 'evil-window-right)
  ;; (global-set-key (kbd "M-s-[") 'evil-window-left)
  (global-set-key (kbd "M-s-k") 'evil-window-up)
  (global-set-key (kbd "M-s-j") 'evil-window-down))

;; If you use a window manager be careful of possible key binding clashes
;; (global-unset-key (kbd "M-<tab>"))
;; (global-set-key (kbd "M-<tab>") 'other-window) ; very useful
;; (global-set-key (kbd "M-<iso-lefttab>") (lambda() (interactive) (other-window -1))) ; == M-S-<tab>
;; (global-set-key (kbd "M-<backtab>") (lambda() (interactive) (other-window -1))) ; for terminal

(global-set-key (kbd "C-c <left>") 'winner-undo) ; built-in winner
(global-set-key (kbd "C-c <right>") 'winner-redo)

;;;; tab-bar menu-bar

;; gb / gB
;; Ctrl + Number
(with-eval-after-load 'tab-bar
  (define-key evil-motion-state-map "gb" 'tab-next)
  (define-key evil-motion-state-map "gB" 'tab-previous)
  (define-key evil-normal-state-map "gb" 'tab-next)
  (define-key evil-normal-state-map "gB" 'tab-previous)

  (define-key evil-motion-state-map "gh" 'menu-bar-open)
  (define-key evil-normal-state-map "gh" 'menu-bar-open)

  (global-set-key (kbd "s-\\") 'tab-bar-switch-to-tab)
  (global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab) ; +tabs:previous-or-goto
  (global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab) ; +tabs:next-or-goto
  )

(when (eq emacs-major-version 30)
  (with-eval-after-load 'tab-line
    ;; (define-key evil-motion-state-map (kbd "g SPC") #'centaur-tabs-switch-group)
    (define-key evil-motion-state-map (kbd "g <right>") #'tab-line-switch-to-next-tab)
    (define-key evil-motion-state-map (kbd "g <left>") #'tab-line-switch-to-prev-tab)
    (define-key evil-normal-state-map (kbd "g <right>") #'tab-line-switch-to-next-tab)
    (define-key evil-normal-state-map (kbd "g <left>") #'tab-line-switch-to-prev-tab)

    (define-key evil-motion-state-map  "gt" #'tab-line-switch-to-next-tab)
    (define-key evil-motion-state-map  "gT" #'tab-line-switch-to-prev-tab)
    (define-key evil-normal-state-map  "gt" #'tab-line-switch-to-next-tab)
    (define-key evil-normal-state-map  "gT" #'tab-line-switch-to-prev-tab)
    (global-set-key (kbd "s-[") 'tab-line-switch-to-prev-tab) ; +tabs:previous-or-goto
    (global-set-key (kbd "s-]") 'tab-line-switch-to-next-tab) ; +tabs:next-or-goto

    ;; (global-set-key (kbd "C-<iso-lefttab>") 'tab-line-switch-to-prev-tab) ; +tabs:previous-or-goto
    ;; (global-set-key (kbd "C-<tab>") 'tab-line-switch-to-next-tab) ; +tabs:next-or-goto
    )
  )

;;;; DONT centaur-tabs

;; (when (locate-library "centaur-tabs")
;;   (with-eval-after-load 'centaur-tabs
;;     ;; ;; Replace Emacs Tabs key bindings with Workspace key bindings
;;     (define-key evil-motion-state-map (kbd "g SPC") #'centaur-tabs-switch-group)
;;     (define-key evil-motion-state-map (kbd "g <right>") #'centaur-tabs-forward-group)
;;     (define-key evil-motion-state-map (kbd "g <left>") #'centaur-tabs-backward-group)
;;     (define-key evil-normal-state-map (kbd "g SPC") #'centaur-tabs-switch-group)
;;     (define-key evil-normal-state-map (kbd "g <right>") #'centaur-tabs-forward-group)
;;     (define-key evil-normal-state-map (kbd "g <left>") #'centaur-tabs-backward-group)

;;     (global-set-key (kbd "C-s-h") 'centaur-tabs-backward)
;;     (global-set-key (kbd "C-s-l") 'centaur-tabs-forward)
;;     (global-set-key (kbd "s-[") 'centaur-tabs-backward) ; +tabs:previous-or-goto
;;     (global-set-key (kbd "s-]") 'centaur-tabs-forward) ; +tabs:next-or-goto
;;     (global-set-key (kbd "s-}") 'centaur-tabs-forward-group) ; tab-bar-switch-to-next-tab
;;     (global-set-key (kbd "s-{") 'centaur-tabs-backward-group) ; tab-bar-switch-to-prev-tab
;;     (global-set-key (kbd "s-SPC") 'centaur-tabs-switch-group) ; tab-bar-switch-to-prev-tab
;;     )
;;   )

;; ("C-c t p" . centaur-tabs-group-by-projectile-project)
;; ("C-c t g" . centaur-tabs-group-buffer-groups)

;;;; math-preview

(when (locate-library "math-preview")
  (global-set-key (kbd "M-g /") 'math-preview-at-point)
  (global-set-key (kbd "M-g M-/") 'math-preview-all)
  (global-set-key (kbd "M-g M-.") 'math-preview-clear-all)
  )

;;;; my/backward-delete-word-or-region

(global-set-key (kbd "M-<backspace>") 'my/backward-delete-word-or-region)

;;;; DONT hungry-delete

;;(when (locate-library "hungry-delete")

;; 기본 스타일 바인딩을 사용하자.
;  (global-set-key (kbd "S-<backspace>") 'hungry-delete-backward) ; default bindings
;  (global-set-key (kbd "S-<delete>") 'hungry-delete-forward)
;  (global-set-key (kbd "S-DEL") 'hungry-delete-forward)
;; (global-set-key (kbd "M-<backspace>") 'spacemacs/backward-kill-word-or-region)

;; C 로 하려다가 기본이 S 더라. 기본으로 가자.
;; (global-set-key (kbd "C-<backspace>") 'hungry-delete-backward)
;; (global-set-key (kbd "C-<delete>") 'hungry-delete-forward)
;; (global-set-key (kbd "C-DEL") 'hungry-delete-forward)
;  )

;;;; vterm

(when (locate-library "vterm")
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "<f1>") nil)
    (define-key vterm-mode-map (kbd "<f2>") nil)
    (define-key vterm-mode-map (kbd "<f3>") nil)
    (define-key vterm-mode-map (kbd "<f4>") nil)
    (define-key vterm-mode-map (kbd "<f5>") nil)
    (define-key vterm-mode-map (kbd "<f6>") nil)
    (define-key vterm-mode-map (kbd "<f7>") nil)
    (define-key vterm-mode-map (kbd "<f8>") nil)
    (define-key vterm-mode-map (kbd "<f10>") nil)
    (define-key vterm-mode-map (kbd "<f10>") nil)
    (define-key vterm-mode-map (kbd "<f11>") nil)
    (define-key vterm-mode-map (kbd "<f12>") nil)
    ))


;;;; DONT  Wordreference

;; (when (locate-library "wordreference")
;;   (with-eval-after-load 'wordreference
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "TAB") #'forward-button)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "<backtab>") #'backward-button)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "s") #'wordreference-search)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "w") #'wordreference-search)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "b") #'wordreference-browse-url-results)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "C") #'wordreference-copy-search-term)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "c") #'wordreference-browse-term-cntrl)
;;     (when (require 'sdcv nil :no-error)
;;       (evil-define-key '(normal visual) wordreference-mode-map (kbd "L") #'wordreference-browse-term-sdcv-littre))
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "l") #'wordreference-browse-term-linguee)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "N") #'wordreference-nearby-entries-search)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "n") #'wordreference-next-entry)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "p") #'wordreference-prev-entry)
;;     (when (require 'reverso nil :no-error)
;;       (evil-define-key '(normal visual) wordreference-mode-map (kbd "r") #'wordreference-browse-term-reverso))
;;     (when (require 'wiktionary-bro nil :no-error)
;;       (evil-define-key '(normal visual) wordreference-mode-map (kbd "k") #'wordreference-browse-term-wiktionary-bro))
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd ",") #'wordreference-previous-heading)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd ".") #'wordreference-next-heading)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "RET") #'wordreference-return-search-word)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "v") #'wordreference-paste-to-search)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "S") #'wordreference-switch-source-target-and-search)
;;     (evil-define-key '(normal visual) wordreference-mode-map (kbd "?") #'wordreference-dispatch)
;;     )
;;   )

;;;; calendar :mouse:

(with-eval-after-load 'calendar
  (define-key calendar-mode-map [(double-mouse-1)] 'org-calendar-goto-agenda))

;;;; bm :mouse:

(when (locate-library "bm")
  (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
  (global-set-key (kbd "<C-f6>") 'bm-toggle)
  (global-set-key (kbd "<f6>")   'bm-next)
  (global-set-key (kbd "<S-f6>") 'bm-previous)
  )

;;;; ace-link for eww / woman

(with-eval-after-load 'woman
  (define-key woman-mode-map "o" 'link-hint-open-link))

(with-eval-after-load 'eww
  (define-key eww-link-keymap "o" 'ace-link-eww)
  (define-key eww-mode-map "o" 'ace-link-eww))

;;;; tmr

(when (locate-library "tmr")
  (define-prefix-command 'tmr-map)
  (define-key global-map (kbd "C-c t") 'tmr-map)
  (let ((map tmr-map))
    (define-key map (kbd "t") #'tmr)
    (define-key map (kbd "T") #'tmr-with-description)
    (define-key map (kbd "l") #'tmr-tabulated-view) ; "list timers" mnemonic
    (define-key map (kbd "c") #'tmr-clone)
    (define-key map (kbd "k") #'tmr-cancel)
    (define-key map (kbd "s") #'tmr-reschedule)
    (define-key map (kbd "e") #'tmr-edit-description)
    (define-key map (kbd "r") #'tmr-remove)
    (define-key map (kbd "R") #'tmr-remove-finished)
    )
  )

;;;; mc - multi-cursor

;; Idea taken from "Emacs: Define Key Sequence"
;; ref: http://ergoemacs.org/emacs/emacs_keybinding_power_of_keys_sequence.html
;; define prefix keymap for multiple cursors
(progn
  (define-prefix-command 'my-multi-cursor-keymap)
  (define-key my-multi-cursor-keymap (kbd "e") 'mc/edit-lines)
  (define-key my-multi-cursor-keymap (kbd "a") 'mc/mark-all-like-this-dwim)
  (define-key my-multi-cursor-keymap (kbd "r") 'mc/mark-all-in-region-regexp)
  (define-key my-multi-cursor-keymap (kbd "s") 'mc/mark-all-symbols-like-this-in-defun)
  (define-key my-multi-cursor-keymap (kbd "w") 'mc/mark-all-words-like-this-in-defun)
  (define-key my-multi-cursor-keymap (kbd "C-n") 'mc/mark-next-like-this)
  (define-key my-multi-cursor-keymap (kbd "C-p") 'mc/mark-previous-like-this)
  (define-key my-multi-cursor-keymap (kbd "C-a") 'mc/mark-all-like-this)
  (define-key global-map (kbd "M-g M-c") 'my-multi-cursor-keymap)
  )

;; from ahyatt-dotfiles
;; phi-search : another incremental search & replace, compatible with "multiple-cursors"
;; (global-set-key (kbd "M-s-r") 'mc/mark-all-like-this-dwim)

;;;; denote dired

(when (locate-library "denote")
  ;; Key bindings specifically for Dired.

  (define-key text-mode-map (kbd "<f3>") 'denote-fontify-links-mode)

  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d i") #'my/denote-link-dired-marked-notes)
    (define-key map (kbd "M-s i") #'my/denote-link-dired-marked-notes)
    (define-key map (kbd "M-s G") #'prot-dired-grep-marked-files)
    (define-key map (kbd "C-c C-d r") #'denote-dired-rename-files)
    (define-key map (kbd "C-c C-d k") #'denote-dired-rename-marked-files-with-keywords)
    (define-key map (kbd "C-c C-d R") #'denote-dired-rename-marked-files-using-front-matter))
  )

;;;; outli

(when (locate-library "outli")

  ;; Tab for Heading : outline-mode and org-mode
  ;; search to narrow with heading and tag base on built-in outline-mode
  (with-eval-after-load 'outli
    (progn
      ;; evil normal keybinding is perfer
      (evil-define-key '(normal visual) outli-mode-map (kbd "S-<tab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
      (evil-define-key '(normal visual) outli-mode-map (kbd "S-TAB") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
      (evil-define-key '(normal visual) outli-mode-map (kbd "<backtab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
      (evil-define-key '(normal visual) outli-mode-map (kbd "S-<iso-lefttab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))

      ;; 'TAB' for terminal emacs
      ;; (evil-define-key '(normal visual) outli-mode-map (kbd "<tab>") `(menu-item "" outline-cycle :filter outli--on-heading))
      ;; (evil-define-key '(normal visual) outli-mode-map (kbd "TAB") `(menu-item "" outline-cycle :filter outli--on-heading))
      ;; (evil-define-key '(normal visual) prog-mode-map (kbd "<tab>") 'indent-for-tab-command) ;; 2024-09-13 disable for evil-jump-items
      ;; (evil-define-key '(normal visual) prog-mode-map (kbd "TAB") 'indent-for-tab-command)
      (evil-define-key '(normal) outli-mode-map (kbd "C-c 1") (lambda () (interactive) (outline--show-headings-up-to-level 1)))
      (evil-define-key '(normal) outli-mode-map (kbd "C-c 2") (lambda () (interactive) (outline--show-headings-up-to-level 2)))
      (evil-define-key '(normal) outli-mode-map (kbd "C-c 3") (lambda () (interactive) (outline--show-headings-up-to-level 3)))
      (evil-define-key '(normal) outli-mode-map (kbd "C-c 4") (lambda () (interactive) (outline--show-headings-up-to-level 4)))
      (evil-define-key '(normal) outli-mode-map (kbd "C-c 5") (lambda () (interactive) (outline--show-headings-up-to-level 5)))
      (evil-define-key '(normal) outli-mode-map (kbd "C-M-<tab>") 'outline-cycle-buffer)
      ;; (define-key outli-mode-map (kbd "C-M-<iso-lefttab>")
      ;;             (lambda () (interactive) (outline-cycle-buffer)))

      (evil-define-key '(normal visual) outli-mode-map (kbd "C-n") 'outline-next-heading)
      (evil-define-key '(normal visual) outli-mode-map (kbd "C-p") 'outline-previous-heading)

      (evil-define-key '(insert) outli-mode-map (kbd "C-n") 'next-line)
      (evil-define-key '(insert) outli-mode-map (kbd "C-p") 'previous-line)

      (evil-define-key '(normal visual) outline-mode-map (kbd "C-S-p") 'outline-up-heading)
      (evil-define-key '(normal visual) outline-mode-map "zu" 'outline-up-heading)

      (evil-define-key '(insert) prog-mode-map (kbd "C-k") 'kill-line)

      (define-key prog-mode-map (kbd "C-c H") 'outline-insert-heading)
      (define-key prog-mode-map (kbd "C-c o") 'consult-outline))
    )
  )

;;;; TODO tabgo

;; (when (locate-library "tabgo")
;;   (global-set-key (kbd "M-t") #'tabgo))

;;; C-c M-g M-s
;;;; C-c j

(progn
  (defun my/open-hugo-notes-path ()
    (interactive )
    (find-file user-hugo-notes-dir))
  (defun my/open-fortunes-path ()
    (interactive )
    (find-file (concat org-directory "fortunes/")))
  (defun my/open-tempel-templates ()
    (interactive)
    (find-file (concat user-dotemacs-dir "tempel-templates.eld")))
  (defun my/open-hunspell-personal ()
    (interactive)
    (find-file "~/.hunspell_personal"))
  (defun my/open-elfeed-list ()
    (interactive)
    (find-file (my/org-elfeed-file)))
  ;; update-dic.sh
  (defun my/open-dict-ko-mydata ()
    (interactive)
    (find-file "~/.dict-ko-mydata.yaml"))
  (defun my/open-links-file ()
    (interactive)
    (find-file (my/org-links-file)))
  (defun my/open-quote-file ()
    (interactive)
    (find-file (my/org-quote-file)))
  (defun my/open-tags-file ()
    (interactive)
    (find-file (my/org-tags-file)))
  (defun my/open-glossary-file ()
    (interactive)
    (find-file (my/org-glossary-file)))
  ;; (defun my/open-inbox-file ()
  ;;   (interactive)
  ;;   (find-file (my/org-inbox-file)))
  ;; (defun my/open-tasks-file ()
  ;;   (interactive)
  ;;   (find-file (my/org-tasks-file)))
  (defun my/open-mobile-file ()
    (interactive)
    (find-file (my/org-mobile-file)))
  (defun my/open-remember-file ()
    (interactive)
    (find-file (my/org-remember-file)))
  (defun my/open-remark-file ()
    (interactive)
    (find-file (my/org-remark-file)))
  (defun my/open-now-inbox-file ()
    (interactive)
    (find-file (my/org-inbox-file)))
  (defun my/open-index-file ()
    (interactive)
    (find-file (my/org-index-file)))
  (defun my/open-contacts-file ()
    (interactive)
    (find-file (my/org-contacts-file)))
  (defun my/open-org-emacs-config-file ()
    (interactive)
    (find-file (my/org-emacs-config-file)))

  (global-set-key (kbd "C-c j A") 'edit-abbrevs)

  (global-set-key (kbd "C-c j b") 'my/consult-org-blog)
  (global-set-key (kbd "C-c j c") 'my/consult-org-contacts)

  (global-set-key (kbd "C-c j e") 'my/open-org-emacs-config-file)
  (global-set-key (kbd "C-c j E") 'my/open-external)

  (global-set-key (kbd "C-c j m") 'my/open-tempel-templates)
  (global-set-key (kbd "C-c j n") 'my/open-now-inbox-file)

  (global-set-key (kbd "C-c j SPC") 'my/open-index-file)

  (global-set-key (kbd "C-c j r") 'my/consult-org-reading)

  ;; (global-set-key (kbd "C-c j i") 'my/consult-org-inbox)
  (global-set-key (kbd "C-c j t") 'my/consult-org-tasks)

  (global-set-key (kbd "C-c j k") 'my/consult-org-kdc)
  (global-set-key (kbd "C-c j j") 'my/consult-org-all)

  (global-set-key (kbd "C-c j l") 'my/consult-org-links)

  (global-set-key (kbd "C-c j q") 'my/consult-org-quote)

  (global-set-key (kbd "C-c j s") 'my/consult-ripgrep-org-directory)

  ;; my/open-
  (global-set-key (kbd "C-c j o a") 'my/open-remark-file) ;; annotation
  (global-set-key (kbd "C-c j o d") 'my/open-hunspell-personal)
  (global-set-key (kbd "C-c j o c") 'my/open-contacts-file)
  (global-set-key (kbd "C-c j o D") 'my/open-dict-ko-mydata)
  (global-set-key (kbd "C-c j o R") 'my/open-remember-file)
  (global-set-key (kbd "C-c j o g") 'my/open-glossary-file)
  (global-set-key (kbd "C-c j o m") 'my/open-mobile-file)
  (global-set-key (kbd "C-c j o h") 'my/open-hugo-notes-path)
  (global-set-key (kbd "C-c j o e") 'my/open-elfeed-list)
  (global-set-key (kbd "C-c j o q") 'my/open-fortunes-path)
  ;; (global-set-key (kbd "C-c j o i") 'my/open-inbox-file)
  (global-set-key (kbd "C-c j o l") 'my/open-links-file)
  (global-set-key (kbd "C-c j o t") 'my/open-tags-file)
  ;; (global-set-key (kbd "C-c j o t") 'my/open-tasks-file)
  (global-set-key (kbd "C-c j o q") 'my/open-quote-file)
  )

;; (global-set-key (kbd "C-c j h") 'my/browse-hugo-maybe)

;;;; M-g bindings (goto-map)

(global-set-key (kbd "M-g a") 'consult-org-agenda)

(when (locate-library "git-link")
  (global-set-key (kbd "M-g M-l") 'git-link))

(when (locate-library "consult-flycheck")
  (global-set-key (kbd "M-g F") 'consult-flymake)
  (global-set-key (kbd "M-g f") 'consult-flycheck)
  )

(when (locate-library "consult-flyspell")
  (global-set-key (kbd "M-g s") 'consult-flyspell))

(when (locate-library "org-ql")
  (global-set-key (kbd "M-g q") 'my/org-ql-shuffle-todo)
  (global-set-key (kbd "M-g Q") 'my/org-ql-shuffle-later)
  )

(when (locate-library "immersive-translate")
  (global-set-key (kbd "M-g M-a") 'immersive-translate-auto-mode))

;; (when (locate-library "denote")
;;   (global-set-key (kbd "M-g d") #'my/goto-denote-dired)
;;   ;; (global-set-key (kbd "M-g n") #'my/goto-notes)
;;   (global-set-key (kbd "M-g b") #'my/goto-blog)
;;   (global-set-key (kbd "M-g SPC") #'my/goto-home)
;;   (global-set-key (kbd "M-g .") #'my/goto-dots)
;;   ;; (global-set-key (kbd "M-g f") #'my/denote-find-file)
;;   )

;; (when (locate-library "elfeed")
;;   (global-set-key (kbd "M-g e") #'my/goto-elfeed)
;;   )

;; (global-set-key (kbd "M-g SPC") (lambda() (interactive) (tab-bar-select-tab 1)))

;;;; M-s bindings (search-map)

;;;;; M-s consult-omni, rg, deadgrep, recent-rgrep

; consult-omni
(when (locate-library "consult-omni")
  (global-set-key (kbd "M-s n") 'consult-omni))

;; rg
(when (locate-library "rg")
  (global-set-key (kbd "M-s r") 'rg-menu))

;; deadgrep
(when (locate-library "deadgrep")
  (global-set-key (kbd "M-s v") 'deadgrep)
  )

;; recent-rgrep
(when (locate-library "recent-rgrep")
  (global-set-key (kbd "M-s g") 'recent-rgrep)
  (global-set-key (kbd "M-F") 'recent-rgrep)
  )

;;;;; M-s s my-search-map

(progn
  (define-prefix-command 'my-search-map)
  (define-key global-map (kbd "M-s s") 'my-search-map)
  (let ((map my-search-map))
    ;; (define-key map (kbd "/") 'sdcv-search-pointer)
    ;; (define-key map (kbd "?") 'sdcv-search-input)
    ;; (define-key map (kbd ".") 'sdcv-search-pointer+) ; posframe
    (define-key map (kbd "\[") 'my/wr-koen)
    (define-key map (kbd "\]") 'my/wr-enko)

    ;; (define-key map (kbd "l") 'lexic-search)
    ;; (define-key map (kbd "e") 'external-dict-dwim)

    (define-key map (kbd "N") 'my/search-naver)
    (define-key map (kbd "n") 'my/search-terms-naver)
    (define-key map (kbd "d") 'my/search-dict-daum)
    ))

;;;;; M-s t my-translate-map

(progn
  (define-prefix-command 'my-translate-map)
  (define-key global-map (kbd "M-s t") 'my-translate-map)
  (let ((map my-translate-map))
    (define-key map (kbd "i") 'txl-translate-insert)
    (define-key map (kbd "w") 'wiki-summary-insert)
    (define-key map (kbd "p") 'immersive-translate-paragraph)
    (define-key map (kbd "a") 'immersive-translate-auto-mode)
    (define-key map (kbd "d") 'gt-do-translate)
    ))

;;; Transient

;;;; DONT transient : EKG

;; (when (locate-library "ekg")
;;   (with-eval-after-load 'ekg
;;     ;; (global-set-key (kbd "C-c e") 'ekg-dispatch)
;;     ;; (global-set-key (kbd "<f12>") 'ekg-dispatch)

;;     (defun setup-ekg-transients ()
;;       "Set up Transient menus for EKG"
;;       (interactive)
;;       (transient-define-prefix ekg-dispatch ()
;;         "Top level Transient menu for EKG (Emacs Knowledge Graph)"
;;         [["Show"
;;           ("st" "Today" ekg-show-notes-for-today)
;;           ("sl" "Latest Captured" ekg-show-notes-latest-captured)
;;           ("sm" "Latest Mod" ekg-show-notes-latest-modified)
;;           ("sx" "Trash" ekg-show-notes-in-trash)
;;           ("sd" "Drafts" ekg-show-notes-in-drafts)
;;           "Find Tags"
;;           ("tt" "Tag" ekg-show-notes-with-tag)
;;           ("ta" "All Tags" ekg-show-notes-with-all-tags)
;;           ("tn" "Any Tag" ekg-show-notes-with-any-tags)
;;           ]
;;          ["Capture"
;;           ("cc" "New Note" ekg-capture)
;;           ("cu" "...from URL" ekg-capture-url)
;;           ("cb" "...from current buffer" ekg-capture-file)
;;           ]
;;          ["LLM Query" :if (lambda () (or (featurep 'ekg-llm) (featurep 'ekg-embedding)))
;;           ("lt" "for terms" ekg-embedding-search :if (lambda () (featurep 'ekg-embedding)))
;;           ("lb" "similar to current buffer" ekg-embedding-show-similar-to-current-buffer :if (lambda () (featurep 'ekg-embedding)))
;;           ("lR" "Regenerate embeddings" ekg-embedding-generate-all :if (lambda () (featurep 'ekg-embedding)))
;;           "AI"
;;           ("lq" "AI query, all notes" ekg-llm-query-with-notes :if (lambda () (featurep 'ekg-llm)))
;;           ]
;;          ["Misc"
;;           ("R" "Rename tag globally" ekg-global-rename-tag)
;;           ;; ("e" "ekg-notes-dispatch" ekg-notes-dispatch :if-mode ekg-notes-mode)
;;           ("g" "ekg-notes-dispatch" ekg-notes-dispatch :if-mode ekg-notes-mode)
;;           ("q" "ekg-notes-dispatch" ekg-notes-dispatch :if-mode ekg-notes-mode)
;;           ("Q" "Quit this menu" transient-quit-one)
;;           ]
;;          ])

;;       (transient-define-prefix ekg-notes-dispatch ()
;;         "Notes buffer Transient menu for EKG (Emacs Knowledge Graph)"
;;         [["[E] Notes > Show"
;;           ("sa" "ekg-notes-any-note-tags" ekg-notes-any-note-tags)
;;           ("sA" "ekg-notes-any-tags" ekg-notes-any-tags)
;;           ("st" "ekg-notes-tag" ekg-notes-tag)
;;           ("ss" "search for similar" ekg-embedding-show-similar :if (lambda () (featurep 'ekg-embedding)))
;;           ]
;;          ["AI"
;;           ("aa" "AI send & append" ekg-llm-send-and-append-note :if (lambda () (featurep 'ekg-llm)))
;;           ("ar" "AI send & replace" ekg-llm-send-and-replace-note :if (lambda () (featurep 'ekg-llm)))
;;           ]
;;          ["[E] Notes > Manage"
;;           ("c" "create" ekg-notes-create)
;;           ("d" "delete" ekg-notes-delete)
;;           ("g" "refresh" ekg-notes-refresh)
;;           ("k" "kill (hide) note" ekg-notes-kill)
;;           ("o" "open/edit" ekg-notes-open)
;;           ("m" "Change mode of current note" ekg-change-mode)
;;           ]
;;          ["[E] Notes > Browse"
;;           ("b" "browse resource" ekg-notes-browse)
;;           ("u" "Browse to URL" ekg-browse-url)
;;           ("B" "select & browse" ekg-notes-select-and-browse-url)
;;           ]
;;          ["[E] Notes > Goto"
;;           ("g" "global <Menu>" ekg-dispatch)
;;           ("q" "Back to notes" transient-quit-one)
;;           ("Q" "quit EKG" kill-buffer-and-window)
;;           ]
;;          ])
;;       )

;;     ;; or you might prefer a binding like this one
;;     ;; https://github.com/ahyatt/ekg/discussions/100
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "e") #'ekg-notes-dispatch) ; can you think of a better binding for notes-mode?
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "?") #'ekg-notes-dispatch) ; help when I'm confused

;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "j") #'ekg-notes-next)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "k") #'ekg-notes-previous)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "n") #'ekg-notes-next)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "p") #'ekg-notes-previous)

;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "A") #'ekg-notes-any-tags)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "a") #'ekg-notes-any-note-tags)

;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "C") #'ekg-notes-create)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "D") #'ekg-notes-delete)

;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "r") #'ekg-notes-refresh)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "o") #'ekg-notes-open)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "b") #'ekg-notes-browse)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "B") #'ekg-notes-select-and-browse-url)
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "t") #'ekg-notes-tag)

;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "q") #'ekg-notes-dispatch) ; toggle menu
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "Q") #'kill-buffer-and-window) ; I prefer this behavior
;;     (evil-define-key '(normal visual) ekg-notes-mode-map (kbd "K") #'ekg-notes-kill) ; dired style

;;     ;; load-transient
;;     (setup-ekg-transients) ; only run this once all ekg funcs are loaded
;;     )
;;   )

;;;; transient : ellama

(when (locate-library "ellama")

  (progn
    ;; Change natural text & diff against the results
    ;; One pattern I often want is to change the given text and compare it to the old version.

    ;; LLMs aren't perfectly good at saying what changes they have done, so the pattern here is to query the model and show the changed text together with the diff.

    ;; So first, I need to diff two strings.
    (defun my/diff-strings (str1 str2)
      (let ((file1 (make-temp-file "diff1"))
            (file2 (make-temp-file "diff2")))
        (unwind-protect
            (progn
              (with-temp-file file1
                (insert str1))
              (with-temp-file file2
                (insert str2))
              (with-temp-buffer
                (diff-mode)
                (diff-no-select file1 file2 (diff-switches) t (current-buffer))
                (font-lock-fontify-buffer)
                (buffer-string)))
          (delete-file file1)
          (delete-file file2))))

    ;; And the function to do the prompting iself. Llama tends to output in Markdown, so I use a function from Ellama to convert the output back to Org-mode, if necessary.
    (defun my/ellama-text-with-diff (text is-org-mode prompt)
      (llm-chat-async
       ellama-provider
       (llm-make-chat-prompt
        (format prompt text))
       (lambda (changed-text)
         (when is-org-mode
           (setq changed-text (ellama--translate-markdown-to-org-filter changed-text)))
         (let ((buffer (generate-new-buffer "*ellama-diff*")))
           (with-current-buffer buffer
             (text-mode)
             (insert changed-text)
             (insert "\n\n")
             (insert (my/diff-strings text changed-text)))
           (display-buffer buffer)))
       (lambda (&rest err)
         (message "Error: %s" err))))

    ;; As for prompts, I like the following prompt to proof-read text. It's pretty conservative, but good for fixing typos, missing commas, articles, etc.
    (setq my/ellama-proof-read-prompt
          "Proof-read the following text. Fix any errors but keep the original style and punctuation, including linebreaks. Print the changed text and nothing else, not even \"Here's the proof-read text\".\n\n %s")

    (defun my/ellama--text ()
      (if (region-active-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        (buffer-substring-no-properties (point-min) (point-max))))

    (defun my/ellama-proof-read (text is-org-mode)
      (interactive (list (my/ellama--text) (derived-mode-p 'org-mode)))
      (my/ellama-text-with-diff text is-org-mode my/ellama-proof-read-prompt))

    ;; The following is more expansive, but preserves less of the original text. For instance, it tends to replace my /id est/ and /exempli gratia/. But sometimes it has good ideas.
    (setq my/ellama-improve-wording-prompt
          "Proof-read the following text. Fix any errors and improve wording. Print the changed text and nothing else, not even \"Here's the improved text\".\n\n %s")

    (defun my/ellama-improve-wording (text is-org-mode)
      (interactive (list (my/ellama--text) (derived-mode-p 'org-mode)))
      (my/ellama-text-with-diff text is-org-mode my/ellama-improve-wording-prompt))

    ;; Also, a prompt to make a text more concise.
    (setq my/ellama-improve-concise-prompt
          "Make the following text more concise. Print the changed text and nothing else, not even \"Here's the improved text\".\n\n %s")

    (defun my/ellama-improve-concise (text is-org-mode)
      (interactive (list (my/ellama--text) (derived-mode-p 'org-mode)))
      (my/ellama-text-with-diff text is-org-mode my/ellama-improve-concise-prompt))
    )

  ;; transient
  (with-eval-after-load 'ellama
    (transient-define-prefix my/ellama ()
      "Ellama actions."
      ["General"
       :class transient-row
       ("a" "Chat" ellama-chat)]
      ["Code"
       :class transient-row
       ("ca" "Add" ellama-code-add)
       ("cc" "Complete" ellama-code-complete)
       ("ce" "Edit" ellama-code-edit)
       ("cr" "Review" ellama-code-review)
       ("ci" "Improve" ellama-code-improve)]
      ["Natural Language"
       :class transient-row
       ("np" "Proof-read" my/ellama-proof-read)
       ("nw" "Improve wording" my/ellama-improve-wording)
       ("nc" "Improve conciseness" my/ellama-improve-concise)]
      ["Formatting"
       :class transient-row
       ("ff" "Format" ellama-make-format)
       ("fm" "List" ellama-make-list)
       ("ft" "Table" ellama-make-table)]
      ["Explain & Summarize"
       :class transient-row
       ("es" "Summarize" ellama-summarize)
       ("ea" "Ask about" ellama-ask-about)
       ("es" "Send to chat" ellama-ask-selection)
       ("ew" "Word definition" ellama-define-word)]
      ["Context"
       :class transient-row
       ("xb" "Add buffer" ellama-context-add-buffer)
       ("xf" "Add file" ellama-context-add-file)
       ("xi" "Add info" ellama-context-add-info-node)
       ("xs" "Add selection" ellama-context-add-selection)]
      ["Settings & Sessions"
       :class transient-row
       ("sp" "Provider" ellama-provider-select)
       ("ss" "Session" ellama-session-switch)
       ("sr" "Rename ression" ellama-session-rename)
       ("sd" "Delete session" ellama-session-remove)])
    ;; (my-leader-def "aie" #'my/ellama)
    )

  (global-set-key (kbd "M-g 9") 'my/ellama)
  ) ;; end of ellama


;;;; transient : jinx - spelling

(when (locate-library "jinx")
  (global-set-key (kbd "M-g s") 'my/spell-tmenu)
  ;; powerthesaurus-transient 통합

  (transient-define-prefix my/spell-tmenu ()
    "Spelling commands"
    ["Spelling"
     ["Lookups"
      ("y" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
      ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)]
     ["Spelling Tools"
      ("x" "Jinx" jinx-mode)
      ("c" "Jinx correct" jinx-correct)]
     ["Dictionary"
      ("d" "Lookup" dictionary-lookup-definition)]
     ["Miscellaneous"
      ("q" "Quit" transient-quit-one)]])
  )

;;;; DONT move to ccmenu : transient : casual-suite

;; (when (locate-library "casual-suite")
;;   (require 'casual-suite)

;;   (keymap-set calc-mode-map "<f2>" #'casual-calc-tmenu)
;;   (keymap-set dired-mode-map "<f2>" #'casual-dired-tmenu)
;;   (keymap-set isearch-mode-map "<f2>" #'casual-isearch-tmenu)
;;   (keymap-set calc-mode-map "C-;" #'casual-calc-tmenu)
;;   (keymap-set dired-mode-map "C-;" #'casual-dired-tmenu)
;;   (keymap-set isearch-mode-map "C-;" #'casual-isearch-tmenu)

;;   (keymap-set ibuffer-mode-map "<f2>" #'casual-ibuffer-tmenu)
;;   (keymap-set ibuffer-mode-map "C-;" #'casual-ibuffer-tmenu)

;;   ;; o sort, s filter -> doom default
;;   ;; (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
;;   ;; (evil-define-key '(normal) ibuffer-mode-map (kbd "F") #'casual-ibuffer-filter-tmenu)
;;   ;; (evil-define-key '(normal) ibuffer-mode-map (kbd "s") #'casual-ibuffer-sortby-tmenu)

;;   (keymap-set Info-mode-map "<f2>" #'casual-info-tmenu)
;;   (keymap-set Info-mode-map "C-;" #'casual-info-tmenu)

;;   (require 'casual-re-builder) ;; optional
;;   (keymap-set reb-mode-map "<f2>" #'casual-re-builder-tmenu)
;;   (keymap-set reb-lisp-mode-map "<f2>" #'casual-re-builder-tmenu)
;;   (keymap-set reb-mode-map "C-;" #'casual-re-builder-tmenu)
;;   (keymap-set reb-lisp-mode-map "C-;" #'casual-re-builder-tmenu)

;; ; 'M-a' backward-sentence -> '(' evil-backward-sentence-begin
;;   (keymap-global-set "M-a" #'casual-avy-tmenu)
;;   ;; M-e ews-denote-map

;;   (require 'casual-bookmarks) ;; optional
;;   (keymap-set bookmark-bmenu-mode-map "<f2>" #'casual-bookmarks-tmenu)
;;   (keymap-set bookmark-bmenu-mode-map "C-;" #'casual-bookmarks-tmenu)
;;   ;; (evil-define-key 'normal bookmark-bmenu-mode-map (kbd "J") 'bookmark-jump)
;;   (keymap-set bookmark-bmenu-mode-map "J" #'bookmark-jump)
;;   (easy-menu-add-item global-map '(menu-bar) casual-bookmarks-main-menu "Tools")

;;   (require 'casual-agenda)
;;   (keymap-set org-agenda-mode-map "C-;" #'casual-agenda-tmenu)
;;   ;; org-agenda-clock-goto ; optional
;;   ;; bookmark-jump ; optional

;;   (require 'casual-symbol-overlay) ;; optional
;;   (keymap-set symbol-overlay-map "C-;" #'casual-symbol-overlay-tmenu)
;;   ;; (keymap-set prog-mode-map "C-;" #'casual-symbol-overlay-tmenu) ; C-'

;;   (require 'casual-editkit) ;; optional
;;   (keymap-global-set "C-;" #'casual-editkit-main-tmenu)
;;   )

;;;; transient : casual-mastodon

;; (when (locate-library "mastodon")
;;   (require 'mastodon)
;;   (require 'mastodon-toot)

;;   (transient-define-prefix casual-mastodon-tmenu ()
;;     "Transient menu for MastodonKurecolor."
;;     [["Timelines"
;;       ("H" "home"            mastodon-tl--get-home-timeline)
;;       ("L" "local"           mastodon-tl--get-local-timeline)
;;       ("F" "federated"       mastodon-tl--get-federated-timeline)
;;       ("K" "bookmarks"       mastodon-profile--view-bookmarks)
;;       ("V" "favorites"       mastodon-profile--view-favourites)
;;       ("'" "followed tags"   mastodon-tl--followed-tags-timeline)
;;       ("@" "mentions"        mastodon-notifications--get-mentions)
;;       ("N" "notifications"   mastodon-notifications-get)
;;       ("\\" "of remote host" mastodon-tl--get-remote-local-timeline)]

;;      ;; u                    mastodon-tl--update

;;      ["Search"
;;       ("s" "search"          mastodon-search--query)
;;       ("#" "tagged"          mastodon-tl--get-tag-timeline)
;;       ("\"" "followed tags"  mastodon-tl--list-followed-tags)
;;       ("I" "filter"          mastodon-views--view-filters)
;;       ("X" "lists"           mastodon-views--view-lists)]

;;      ["Toots"
;;       ("n" "next"            mastodon-tl--goto-next-item :transient t)
;;       ("p" "prev"            mastodon-tl--goto-prev-item :transient t)
;;       ("c" "spoiler"         mastodon-tl--toggle-spoiler-text-in-toot :transient t)
;;       ("T" "thread"          mastodon-tl--thread)
;;       ("b" "(un)boost"       mastodon-toot--toggle-boost :transient t)
;;       ("f" "(un)fav"         mastodon-toot--toggle-favourite :transient t)
;;       ("i" "(un)pin"         mastodon-toot--pin-toot-toggle :transient t)
;;       ("k" "(un)bookmark"    mastodon-toot--toggle-bookmark :transient t)
;;       ("v" "vote"            mastodon-tl--poll-vote)]

;;      ;; Z                    mastodon-tl--report-to-mods
;;      ;; o                    mastodon-toot--open-toot-url

;;      ["Own Toots"
;;       ("r" "replay"          mastodon-toot--reply)
;;       ("t" "write"           mastodon-toot)
;;       ("e" "edit"            mastodon-toot--edit-toot-at-point)
;;       ("d" "delete"          mastodon-toot--delete-toot)
;;       ("D" "del & redraft"   mastodon-toot--delete-and-redraft-toot)
;;       ("E" "show edits"      mastodon-toot--view-toot-edits)]

;;      ;; S                    mastodon-views--view-scheduled-toots

;;      ["Users"
;;       ("W" "follow"          mastodon-tl--follow-user)
;;       ("R" "follow req"      mastodon-views--view-follow-requests)
;;       ("G" "suggestions"     mastodon-views--view-follow-suggestions)
;;       ("M" "mute user"       mastodon-tl--mute-user)
;;       ("B" "block user"      mastodon-tl--block-user)
;;       ("m" "message user"    mastodon-tl--dm-user)
;;       ""
;;       ;; ("," "favouriters"     mastodon-toot--list-toot-favouriters)
;;       ;; ("." "boosters"        mastodon-toot--list-toot-boosters)
;;       ]]

;;     ;; S-RET                mastodon-tl--unmute-user
;;     ;; C-S-b                mastodon-tl--unblock-user

;;     [["Profiles"
;;       ("A" "author"          mastodon-profile--get-toot-author)
;;       ("P" "any user"        mastodon-profile--show-user)
;;       ("O" "own"             mastodon-profile--my-profile)
;;       ("U" "update own"      mastodon-profile--update-user-profile-note)]

;;      ["Misc"
;;       ("C" "copy URL"        mastodon-toot--copy-toot-url)
;;       ("?" "help"            describe-mode)
;;       ("q" "quit"            transient-quit-one)
;;       ]]
;;     )

;;   (keymap-set mastodon-mode-map "C-;" #'casual-mastodon-tmenu)

;;   ;; These won't ever go into the transient:
;;   ;; TAB             mastodon-tl--next-tab-item
;;   ;; SPC             mastodon-tl--more
;;   ;; !               mastodon-tl--fold-post-toggle
;;   ;; /               mastodon-switch-to-buffer
;;   ;; ;               mastodon-views--view-instance-description
;;   ;; <               beginning-of-buffer
;;   ;; >               end-of-buffer
;;   ;; Q               mastodon-kill-window
;;   ;; h               describe-mode
;;   ;; l               recenter-top-bottom
;;   ;; z               bury-buffer
;;   ;; DEL             scroll-down-command
;;   ;; S-TAB           mastodon-tl--previous-tab-item
;;   ;; C-S-w           mastodon-tl--unfollow-user
;;   ;; S-SPC           scroll-down-command
;;   ;; <backtab>       mastodon-tl--previous-tab-item
;;   ;; C-M-i           mastodon-tl--previous-tab-item
;;   ;; C-M-q           mastodon-kill-all-buffers
;;   ;; M-n             mastodon-tl--next-tab-item
;;   ;; M-p             mastodon-tl--previous-tab-item
;;   )

;;;; transient : expand-region

(when (locate-library "expand-region")
  (require 'expand-region)
  ;; ;;;###autoload
  ;; (defun expreg--line ()
  ;;   "Return a list of regions containing surrounding sentences."
  ;;   (ignore-errors
  ;;     (let (beg end)
  ;;       (end-of-visual-line)
  ;;       (setq end (point))
  ;;       (beginning-of-visual-line)
  ;;       (setq beg (point))
  ;;       `((line . ,(cons beg end))))))

;;;###autoload
  (transient-define-prefix expand-transient ()
    "expand/contract"
    [[("v" "expand" er/expand-region :transient t)]
     [("V" "contract" er/contract-region :transient t)]]
    ;; [[("v" "expand" expreg-expand :transient t)]
    ;;  [("V" "contract" expreg-contract :transient t)]]
    [
     :hide always
     ("<escape>" "ignore" transient-quit-one)
     ("q" "ignore" transient-quit-one)
     ("RET" "ignore" transient-quit-one)]
    [
     ;; :hide always
     ("y" "yank" evil-yank)
     ("d" "delete" evil-delete)
     ("x" "delete-char" evil-delete-char)
     ("p" "paste-after" evil-paste-after)
     ("P" "paste-before" evil-paste-before)
     ("r" "replace" evil-replace)
     ("c" "change" evil-change)
     ("s" "surround-region" evil-surround-region)
     ("R" "match-all" evil-multiedit-match-all)
     ("o" "exchange-point-and-mark" exchange-point-and-mark :transient t)
     ("0" "beginning-if-line" evil-beginning-of-line :transient t)
     ("$" "end-of-line" evil-end-of-line :transient t)
     ("k" "previous-visual-line" evil-previous-visual-line :transient t)
     ("j" "next-visual-line" evil-next-visual-line :transient t)
     ("h" "backward-char" evil-backward-char :transient t)
     ("l" "forward-char" evil-forward-char :transient t)
     ;; ("%" "jump-items" evilmi-jump-items :transient t) ; evil-matchit
     ("%" "evil-jump-item" evil-jump-item :transient t)
     ("M-o" "embark-act" embark-act)
     (">" "indent-rigidly" indent-rigidly)
     ("~" "invert-char" evil-invert-char)
     ("SPC" "SPC" (lambda () (interactive) (funcall (general-simulate-key "SPC"))))])
  )

;;;; transient : transient-window

(progn

;;;###autoload
  (transient-define-suffix transient-window--enlarge-v ()
    :transient t
    :key "k"
    :description "enlarge vertically"
    (interactive)
    (call-interactively #'enlarge-window))

;;;###autoload
  (transient-define-suffix transient-window--shrink-v ()
    :transient t
    :key "j"
    :description "shrink vertically"
    (interactive)
    (call-interactively #'shrink-window))

;;;###autoload
  (transient-define-suffix transient-window--enlarge-h ()
    :transient t
    :key "l"
    :description "enlarge horizontally"
    (interactive)
    (call-interactively #'enlarge-window-horizontally))

;;;###autoload
  (transient-define-suffix transient-window--shrink-h ()
    :transient t
    :key "h"
    :description "shrink horizontally"
    (interactive)
    (call-interactively #'shrink-window-horizontally))

;;;###autoload
  (transient-define-suffix transient-window--balance ()
    :key "="
    :description "balance"
    (interactive)
    (call-interactively #'balance-windows))

;;;###autoload
  (transient-define-suffix transient-window--golden-ratio ()
    :key "g"
    :description "golden-ratio"
    (interactive)
    (call-interactively #'golden-ratio))

;;;###autoload
  (transient-define-prefix window-transient ()
    "Window manipulations"
    ["Window"
     [(transient-window--enlarge-v)
      (transient-window--shrink-v)
      (transient-window--enlarge-h)
      (transient-window--shrink-h)
      (transient-window--balance)
      (transient-window--golden-ratio)]])

;;;###autoload
  (defun window-cleanup+ ()
    "Deletes duplicate windows. Leaves single window per buffer, removing all duplicates."
    (interactive)
    (when (->>
           (window-list)
           (seq-group-by (lambda (win) (window-buffer win)))
           (seq-filter (lambda (group) (length> (cdr group) 1)))
           (seq-do (lambda (group) (seq-do #'delete-window (cddr group)))))
      (balance-windows-area)))

  (defun delete-other-windows-horizontally ()
    "Delete all windows to the left and right of the current
window."
    (interactive)
    (require 'windmove)
    (save-excursion
      (while (condition-case nil (windmove-left) (error nil))
        (delete-window))
      (while (condition-case nil (windmove-right) (error nil))
        (delete-window))))

  (defun toggle-window-divider ()
    (interactive)
    (setf right-divider-width (if window-divider-mode 1 6))
    (setf left-divider-width (if window-divider-mode 1 6))
    (window-divider-mode 'toggle))

  )

;; ;;;###autoload
;; (defun +scroll-line-down-other-window (&optional count)
;;   "Scrolls in the window COUNT lines downwards."
;;   (interactive "P")
;;   (with-selected-window (other-window-for-scrolling)
;;     (funcall (doom-lookup-key (kbd "C-e")) (or count 1))))

;; ;;;###autoload
;; (defun +scroll-line-up-other-window (&optional count)
;;   "Scrolls in the window COUNT lines downwards."
;;   (interactive "P")
;;   (with-selected-window (other-window-for-scrolling)
;;     (funcall (doom-lookup-key (kbd "C-y")) (or count 1))))

;; ;;;###autoload
;; (defun display-buffer-window-equal-width (buffer alist)
;;   "Keep buffer window width proportional to other windows."
;;   (let* ((win (display-buffer-in-direction buffer alist)))
;;     (window-resize
;;      win
;;      (- (/ (frame-width) (length (window-list)))
;;         (window-width win))
;;      t t)
;;     win))

;;;; transient : casual-anddo

(when (locate-library "anddo")
  (require 'anddo)

  (transient-define-prefix casual-anddo-tmenu ()
    "Transient menu for anddo"
    [["anddo"
      ("n" "new-item"          anddo-new-item)
      ("e" "edit-item"          anddo-edit-item)
      ("s" "change-status"          anddo-change-status)
      ("r" "refresh-toggle-listing" anddo-toggle-listing-mode)
      ("<RET>" "show-body"          anddo-show-body)
      ("l" "show-body"          anddo-show-body)
      ("D" "delete-item"          anddo-delete-item)]
     ["Miscellaneous"
      ("q" "quit" transient-quit-one)
      ("Q" "Kill-buffer-window" kill-buffer-and-window)
      ]
     ]
    )
  (keymap-set anddo-mode-map "C-;" #'casual-anddo-tmenu)
  (keymap-set anddo-mode-map "<f2>" #'casual-anddo-tmenu)
  )

;;; EWS Map : 'M-c n', 'C-c n' and 'SPC RET'

;; org-mode
(defvar-keymap ews-org-noter-map
  :doc "Emacs org-noter keymap."
  "a" #'org-noter-anchor-to-current-page+
  "i" #'org-noter-insert-note
  "j" #'org-noter-pdf-scroll-down
  "k" #'org-noter-pdf-scroll-up
  "N" #'org-noter
  "n" #'org-noter-sync-current-note
  "g" #'org-noter-top-of-the-page
  "G" #'org-noter-bottom-of-the-page
  "C-j" #'org-noter-pdf-next-page
  "C-k" #'org-noter-pdf-prev-page)

(defvar-keymap ews-org-transclusion-map
  :doc "Emacs org-transclusion keymap."
  "u" #'org-transclusion-add
  "U" #'org-transclusion-add-all
  "d" #'org-transclusion-remove
  "D" #'org-transclusion-remove-all
  "l" #'org-transclusion-demote-subtree
  "h" #'org-transclusion-promote-subtree
  "r" #'org-transclusion-refresh
  "g" #'org-transclusion-move-to-source)

(defvar-keymap ews-bibliography-map
  :doc "Bibliograpic functions keymap."

  "b" #'org-cite-insert

  "c" #'citar-open

  "d" #'citar-denote-dwim
  ;; "e" #'citar-open-entry
  "e" #'citar-denote-open-reference-entry

  "a" #'citar-denote-add-reference
  "1" #'citar-denote-find-citation ;; grep [cite @xxx]

  "i" #'citar-insert-citation
  "n" #'citar-create-note
  "o" #'citar-denote-open-note
  "O" #'citar-open-links

  "f" #'citar-denote-find-reference
  "l" #'citar-denote-link-reference

  "s" #'citar-denote-create-silo-note
  "k" #'citar-denote-remove-reference
  )

(defvar-keymap ews-annotate-map
  :doc "Emacs Capture Archive Annotate keymap."
  "a" #'orgabilize-org-archive
  "l" #'orgabilize-insert-org-link
  "f" #'orgabilize-org-find-archived-file
  "t" #'org-web-tools-read-url-as-org
  "T" #'org-web-tools-convert-links-to-page-entries
  "r" #'remember
  "R" #'remember-notes
  ;; "w" #'wikinforg
  "o" #'org-pandoc-import-as-org
  "i" #'bh/insert-inactive-timestamp
  "e" #'jao-eww-to-org
  "m" #'org-remark-mark
  )

(defvar-keymap ews-denote-explore-map
  :doc "Denote-Explore keybindings"
  "i" #'denote-explore-isolated-notes
  "s" #'denote-explore-sync-metadata
  "d" #'denote-explore-identify-duplicate-notes
  "c" #'denote-explore-count-notes
  "C" #'denote-explore-count-keywords
  "S" #'denote-explore-single-keywords
  "K" #'denote-explore-rename-keyword
  "r" #'denote-explore-random-note
  "l" #'denote-explore-random-link
  "k" #'denote-explore-random-keyword
  "x" #'denote-explore-random-regex
  "b" #'denote-explore-barchart-keywords
  "B" #'denote-explore-backlinks-barchart
  "e" #'denote-explore-barchart-filetypes
  "n" #'denote-explore-network
  "z" #'denote-explore-zero-keywords
  )

(defvar-keymap ews-org-publish-map
  :doc "org-publish keybindings"

  "l" #'my/insert-hugo-lastmod-time-stamp
  "o" #'my/org-open-exported-markdown-in-hugo-content
  "E" #'my/org-hugo-export-directory
  "u" #'my/org-update-all-dblocks
  "U" #'my/org-update-all-dblocks-on-directory
  "c" #'my/denote-convert-note-to-blog-draft
  "p" #'my/denote-convert-blog-ready-to-hugo

  ;; "r" 'org-reveal-export-to-html
  "r" #'org-re-reveal-export-to-html-and-browse
  "e" #'org-hugo-export-wim-to-md
  )

(defvar-keymap ews-denote-extra-map
  :doc "Denote-extrea keybindings"

  "0" #'my/denote-info
  "a" #'my/denote-attach
  "e" #'prot-eshell-export

  ;; "s" #'denote-subdirectory
  "S" #'my/denote-sort-with-days

  "P" #'ews-denote-assign-para
  "n" #'my/goto-denote-dired
  ;; "z" #'denote-signature ; "zettelkasten" mnemonic

  "M-i" #'denote-org-extras-dblock-insert-files
  "M-r" #'my/rename-all-screenshot-images-to-denote-id

  ;; "s" #'denote-sort-dired
  ;; "-" #'my/org-create-id-by-denote-identifier
  ;; "M--" #'my/org-create-id-by-denote-identifier-at-once
  )

;; Set keybindings
;; https://www.youtube.com/watch?v=gojOZ3k1mmk
(defvar-keymap ews-denote-map
  :doc "Denote keybindings."
  "a" ews-annotate-map
  "b" ews-bibliography-map

  "B" #'denote-org-extras-backlinks-for-heading
  "d" #'denote-create-note

  "f" #'my/denote-find-file ; find org files
  "g" #'my/denote-grep
  "." #'consult-denote-find ; find all types of files
  "SPC" #'consult-denote-grep

  "F" #'+default/find-in-notes ; find-files
  ;;   "F" #'+default/browse-notes

  "1" #'denote-random-notes
  "2" #'denote-random-bib
  "3" #'denote-random-meta
  "4" #'denote-explore-random-note

  "e" ews-denote-extra-map
  "E" #'my/denote-assign-evergreen
  "o" ews-org-noter-map
  "p" ews-org-publish-map

  "h" #'my/denote-org-store-link-to-heading
  "H" #'denote-org-extras-link-to-heading

  "i" #'my/denote-org-extras-dblock-insert-links
  "I" #'my/denote-org-extras-dblock-insert-backlinks
  "M-i" #'my/denote-org-extras-dblock-insert-meta-links

  "l" #'denote-link-or-create
  "L" #'denote-link-after-creating-with-command

  "n" #'consult-notes

  "!" #'my/consult-org-screenshot
  "G" #'consult-notes-search-in-all-notes

  "m" #'my/denote-insert-meta-links
  "M" #'my/denote-create-meta-note

  "s" #'denote-silo-extras-open-or-create
  "S" #'denote-silo-extras-select-silo-then-command

  "t" #'denote-type

  "u" ews-org-transclusion-map

  "r" #'denote-region ; "contents" mnemonic
  "," #'denote-rename-file-using-front-matter
  "<" #'denote-rename-file-title
  "/" #'my/denote-random-note
  "-" #'denote-show-backlinks-buffer

  "TAB" #'org-journal-open-current-journal-file
  ;; "SPC" #'side-journal-toggle-notes

  "j" #'org-journal-new-entry
  ;; "[" #'org-journal-previous-entry
  ;; "]" #'org-journal-next-entry

  "k" #'denote-rename-file-keywords
  "z" #'denote-rename-file-signature
  "Z" #'my/denote-assign-zettel

  "x" ews-denote-explore-map

  "M-f" #'denote-find-link
  "M-b" #'denote-find-backlink
  )

(defvar-keymap ews-modus-themes-map
  :doc "Doom/Modus/Ef-Themes keymap."
  ;; "c" #'consult-theme
  "m" #'modus-themes-toggle
  "M" #'modus-themes-select
  "e" #'ef-themes-toggle
  "E" #'ef-themes-select
  "," #'ef-themes-load-random-light
  "." #'ef-themes-load-random-dark

  ;; "t" #'my/doom-themes-toggle
  ;; "d" #'my/doom-themes-random-light
  ;; "D" #'my/doom-themes-random-dark
  )

(defvar-keymap ews-emms-map
  :doc "Emacs Multimedia System (EMMS) keymap."
  "b" #'emms-browser
  "e" #'emms)

;; (defvar-keymap ews-ekg-map
;;   :doc "Emacs Knowledge Graph keymap."
;;   "c" #'ekg-capture
;;   "s" #'ekg-show-notes-for-today)

(defvar-keymap ews-tmr-map
  :doc "tmr keymap."
  "t" #'tmr
  "l" #'tmr-tabulated-view)

;; (defvar-keymap ews-note-map
;;   :doc "Org-roam keybindings"
;;   "?" #'org-roam-node-random
;;   "h" #'org-roam-graph

;;   "f" #'+default/find-in-notes ; find-files
;;   "F" #'+default/browse-notes

;;   "SPC" #'org-roam-dailies-goto-today
;;   "c" #'org-roam-dailies-capture-today
;;   "C" #'org-roam-dailies-capture-date
;;   "<" #'org-roam-dailies-goto-yesterday
;;   ">" #'org-roam-dailies-goto-tomorrow

;;   "n" #'denote

;;   "/" #'consult-org-roam-file-find
;;   "M-/" #'org-roam-node-find

;;   "k" #'org-roam-tag-add
;;   "K" #'org-roam-tag-delete

;;   "i" #'org-roam-node-insert
;;   "I" #'ash/org-roam-node-insert-immediate

;;   "l" #'org-roam-alias-add
;;   "-" #'org-roam-buffer-toggle
;;   "s" #'force-org-roam-rebuild-cache
;;   )

(defvar-keymap ews-map
  :doc "Emacs Writing Studio key bindings."
  "RET" #'consult-bookmark
  "a" ews-annotate-map
  "b" ews-bibliography-map
  "D" #'org-drill
  ;; "e" ews-ekg-map
  "f" #'my/logos-focus-editing-toggle
  "F" #'ews-olivetti
  "n" ews-denote-map

  "N" #'my/consult-org-all
  "l" #'elfeed
  "m" #'mastodon
  "M" ews-emms-map

  ;; "t" ews-modus-themes-map
  "T" ews-tmr-map
  "x" ews-denote-explore-map
  )

;; (global-set-key (kbd "C-c A") #'consult-org-agenda)
;; (global-set-key (kbd "C-c N") #'my/consult-org-all)
;; (global-set-key (kbd "<f11>") ews-map)

(keymap-set global-map "C-c n" ews-denote-map)
(keymap-set global-map "M-e" ews-denote-map) ; ews-denote-map

;; Org mode keymap modifications
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-x n b" #'org-cite-insert)
  (keymap-set org-mode-map "C-x n -" #'bh/insert-inactive-timestamp)
  (keymap-set org-mode-map "M-s ," #'denote-rename-file-using-front-matter)
  (keymap-set org-mode-map "M-s <" #'denote-rename-file-title)
  ;; (keymap-set org-mode-map "C-c n o" ews-org-noter-map)
  ;; (keymap-set org-mode-map "C-c n u" ews-org-transclusion-map)
  (keymap-set org-mode-map "C-x n 0" #'ews-org-insert-notes-drawer)
  (keymap-set org-mode-map "C-x n 9" #'ews-org-count-words)
  (keymap-set org-mode-map "C-x n l" #'my/denote-org-store-link-to-heading)

  ;; (keymap-set org-mode-map "C-x n 8" #'ews-org-insert-screenshot)
  ;; (keymap-set org-mode-map "C-x n 7" #'ews-denote-assign-para)
  )

(when (locate-library "which-key")
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements ews-map
      "a" `("Annotate" . ,ews-annotate-map)
      "b" `("Bibliography" . ,ews-bibliography-map)
      ;; "n" `("EKG" . ,ews-ekg-map)
      "d" `("Denote" . ,ews-denote-map)
      "M" `("Music" . ,ews-emms-map)
      ;; "n" `("Note" . ,ews-note-map)
      "T" `("TMR" . ,ews-tmr-map)
      "t" `("Themes" . ,ews-modus-themes-map)
      "x" `("Denote-Explore" . ,ews-denote-explore-map)
      )
    (which-key-add-keymap-based-replacements ews-denote-map
      "a" `("Annotate" . ,ews-annotate-map)
      "b" `("Bibliography" . ,ews-bibliography-map)
      "x" `("Denote-Explore" . ,ews-denote-explore-map)
      "o" `("Org-noter" . ,ews-org-noter-map)
      "u" `("Org-Transclusion" . ,ews-org-transclusion-map)
      "e" `("Denote-Extra" . ,ews-denote-extra-map)
      "p" `("Org-Publish" . ,ews-org-publish-map)
      )
    ))

;;; provide

(provide 'keys)

;;; end-of-func
