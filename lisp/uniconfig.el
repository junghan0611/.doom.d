;;; lisp/uniconfig.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ❶ :: U+2776 ==> 더원싱 태그로 활용
;; ㉽ :: U+327D
;; ㉼ :: U+327C

;; 이맥스 배포판에 호환하는 통합 설정
;; 목적 : 둠 스페이스맥스 등 관계 없이 패키지가 있다면
;; 여기 설정에 따라서 동작하도록 함.

;; 2024-04-20 일단 둠 이맥스 설정을 싹 비우는게 목표

;;; Configs

;;;; backtrace-mode-hook

(add-hook 'backtrace-mode-hook 'display-line-numbers-mode)
(add-hook 'backtrace-mode-hook 'visual-line-mode)

;;;; dabbrev

(progn
  (require 'dabbrev)
  (setq dabbrev-abbrev-char-regexp "[가-힣A-Za-z-_]")
  (setq dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-upcase-means-case-search nil) ; default t
  ;; (setq dabbrev-check-all-buffers t) ;; default t
  )

;;;; visual-line-mode

;; /home/junghan/sync/man/dotsamples/vanilla/localauthor-dotfiles-zk/init.el:119
;; (with-current-buffer "*Messages*"
;;   (visual-line-mode))
;; (with-current-buffer "*scratch*"
;;   (visual-line-mode))

(add-hook 'compilation-mode-hook 'visual-line-mode)
;; (add-hook 'fundamental-mode-hook 'visual-line-mode)

;;;; Xref

;; Denote 23.9. Speed up backlinks’ buffer creation?
;; Prefer ripgrep, then ugrep, and fall back to regular grep.

(setq xref-search-program
      (cond
       ((or (executable-find "ripgrep")
            (executable-find "rg"))
        'ripgrep)
       ((executable-find "ugrep")
        'ugrep)
       (t
        'grep)))

;; check below
;; (setq xref-file-name-display 'project-relative)
;; Use completing-read interface instead of definitions buffer (needs xref 1.1.0)
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; (setq xref-show-definitions-function #'consult-xref) ;; default xref-show-definitions-buffer

;;; functions

;;;; Corfu and electric-Pair and Jump In/Out Parens

;; Linux GUI : <tab> TAB
;; Linux Terminal : TAB
;; Linux GUI : S-<iso-lefttab>
;; Linux Terminal : <backtab>

(progn
;;;###autoload
  (defun jump-out-of-pair ()
    (interactive)
    (let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
      (when found
        (cond
         ((or (looking-back "\\*\\*" 2)
              (looking-back "``" 2)
              (looking-back "\"\"" 2) ; 2023-10-02 added
              (looking-back "''" 2) (looking-back "==" 2))
          (forward-char))
         (t
          (forward-char 0))))))
  ;; 절대 하지 말것! (global-set-key [remap indent-for-tab-command] #'jump-out-of-pair)

;;;###autoload
  (defun jump-backward-pair ()
    (interactive)
    (let ((found (search-backward-regexp "[])}\"'`*=]" nil t)))
      (when found
        (cond
         ((or (looking-back "\\*\\*" 2)
              (looking-back "``" 2)
              (looking-back "\"\"" 2) ; 2023-10-02 added
              (looking-back "''" 2) (looking-back "==" 2))
          (backward-char))
         (t
          (backward-char 0))))))
  )


;;;; my/backward-kill-word-or-region

;; no kill-ring propagate

;;;###autoload
(defun my/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

;;;###autoload
(defun my/backward-delete-word-or-region (&optional arg)
  "Calls `delete-region' when a region is active and doesn't affect the kill-ring"
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'delete-region)
    (my/delete-backward-word arg)))

;;;; my/write-window-setup

;; Writing Window Setup on Dired
;; 괜찮다. 화면 버퍼 구성이 여러모로 집중하기 좋다.
(defun my/write-window-setup ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (find-file "*draft.org" t)
  (windmove-right)
  (find-file "*notes.txt" t) ; txt
  (windmove-left))
(with-eval-after-load 'dired
  (define-key dired-mode-map [f3] #'my/write-window-setup))


;;;; my/convert-hangul-jamo-to-syllable

(progn
  (require 'ucs-normalize)

  (defun my/convert-hangul-jamo-to-syllable ()
    "Convert conjoining jamo to precomposed syllables in the current buffer."
    (interactive)
    (let* ((choseong "[\u1100-\u115F\uA960-\uA97C]")
           (jungseong "[\u1160-\u11A7\uD7B0-\uD7C6]")
           (jongseong "[\u11A8-\u11FF\uD7CB-\uD7FB]?")
           (pattern (concat choseong jungseong jongseong)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (ucs-normalize-NFC-region start end))))))

  ;; 버퍼 전체에 적용하려면 다음 함수를 호출하세요:
  ;; (my/convert-hangul-jamo-to-syllable)

  (defun my/process-files-by-extension (directory extension process-func)
    "주어진 디렉토리에서 특정 확장자를 가진 파일들에 대해 처리 함수를 적용합니다."
    (interactive
     (list (read-directory-name "처리할 디렉토리: ")
           (read-string "파일 확장자 (예: txt): ")
           (intern (completing-read "처리 함수: " obarray 'functionp t))))
    (dolist (file (directory-files-recursively directory (concat "\\." extension "$")))
      (with-current-buffer (find-file-noselect file)
        (funcall process-func)
        (save-buffer)
        (kill-buffer))))
  )

;;;; my/replace-latex-delimiters-with-dollar

(defun my/replace-latex-delimiters-with-dollar ()
  "현재 버퍼에서 \\[와 \\]를 $로 바꿉니다."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\\[" nil t)
    (replace-match "$$" nil nil))
  (goto-char (point-min))
  (while (search-forward "\\]" nil t)
    (replace-match "$$" nil nil)))

;;; kmacro

;;;; mirror-buffer
(defalias 'ash/mirror-buffer
  (kmacro "C-x 1 C-x 3 C-x o"))
;; (general-define-key "s-'" 'ash/mirror-buffer)

;;; Utils
;;;; text-font-scale-up

(defun my/text-font-scale-up ()
  (interactive)
  (text-scale-increase 1))
(defun my/text-font-scale-down ()
  (interactive)
  (text-scale-decrease 1))

;; TODO Evil Keymaap
;; (global-set-key (kbd "C-=") 'my/text-font-scale-up)
;; (global-set-key (kbd "C--") 'my/text-font-scale-down)
;;;; Make Script Files Executable Automatically

;; Make script files (with shebang like #!/bin/bash, #!/bin/sh) executable automatically. See this blog post from Emacs Redux.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;; clear-kill-ring

(defun clear-kill-ring ()
  "Clear the results on the kill ring."
  (interactive) (setq kill-ring nil))
(global-set-key (kbd "M-Y") 'clear-kill-ring)

;;;; disable enable easy ui

(defun disable-easy-ui ()
  (interactive)
  (setq use-dialog-box nil)

  ;; (blink-cursor-mode nil)

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'tooltip-mode)
    (tooltip-mode -1))
  (menu-bar-mode -1))

;; On Emacs without X, tool-bar-mode and scroll-bar-mode are not defined.
(defun enable-easy-ui ()
  (interactive)
  (setq tool-bar-style 'image)
  (setq use-dialog-box t) ; default t
  (blink-cursor-mode t)
  (when (fboundp 'tooltip-mode)
    (tooltip-mode 1))
  (menu-bar-mode -1)
  (menu-bar-mode 1)
  ;; (when (fboundp 'tool-bar-mode)
  ;;   (tool-bar-mode 1))
  )

;;;; goto-addr

;; Actionable URLs in Emacs buffers via [[http://xenodium.com/#actionable-urls-in-emacs-buffers][Álvaro Ramírez]].

(progn
  (require 'goto-addr)
  ;; :hook
  ;; ((compilation-mode . goto-address-mode)
  ;;  (prog-mode . goto-address-prog-mode)
  ;;  (eshell-mode . goto-address-mode)
  ;;  (shell-mode . goto-address-mode))
  ;; :bind (:map goto-address-highlight-keymap ("C-c C-o" . goto-address-at-point))
  ;; :config
  (global-goto-address-mode +1)
  )

;;;; Eldoc

(progn
  (require 'eldoc)
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil) ;  important - default 'truncate-sym-name-if-fit
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t) ; default nil - alway show echo-area

  ;; eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer)
  )

;;;###autoload
(defun eldoc-toggle ()
  "Toggle eldoc's documentation buffer."
  (interactive)
  (let ((buffer (eldoc-doc-buffer)))
    (if-let (w (and buffer (get-buffer-window buffer)))
        (delete-window w)
      (eldoc-doc-buffer t))))

;;;; Run commands in a popup frame

;; i3config : bindsym $mod+$wm.binding.orgcapture exec --no-startup-id emacsclient -e '(progn (select-frame-set-input-focus (selected-frame)) (prot-window-popup-org-capture))'
;; myai.sh : emacsclient -e '(progn (select-frame-set-input-focus (selected-frame)) (ad/ai-from-anywhere))'

;;;;; prot's emacs-command-popup-frame

;; https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/

;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
;; The emacsclient calls that need ot be bound to system-wide keys
;; emacsclient -e '(prot-window-popup-org-capture)'
;; emacsclient -e '(prot-window-popup-tmr)'

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(when (locate-library "tmr")
  (require 'tmr)
  (declare-function tmr "tmr" (time &optional description acknowledgep))
  (defvar tmr-timer-created-functions)
;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
  (prot-window-define-with-popup-frame tmr)
  (add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame))

;;;;; gptel: ai-from-anywhare

;; https://www.armindarvish.com/post/use_emacs_as_a_chatgpt_client/
(when (locate-library "gptel")

;;;###autoload
  (defun my/ai-from-anywhere ()
    (interactive)
    (let* ((screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           (frame-width (/ screen-width 3))
           (frame-height screen-height)
           (frame-left (- screen-width frame-width))
           (frame-top 0)
           (chat-frame (make-frame `((window-system . x) ; macOS use "ns"
                                     (top . ,frame-top)
                                     (left . ,frame-left)
                                     (width . (text-pixels . ,frame-width))
                                     (heigth . (text-pixels . ,frame-height))
                                     (minibuffer . t)
                                     ))))
      (select-frame chat-frame)
      )
    (gptel "My:AI Chat" gptel-api-key nil)
    (switch-to-buffer "My:AI Chat")
    (delete-other-windows)
    ;; (tab-line-mode -1)
    ))

;;; Packages with functions
;;;; goto-last-change

(when (locate-library "goto-last-change")
  (defun my/goto-last-change ()
    (interactive)
    (outline-show-all) ; 전체를 펼치고 찾아라!
    (goto-last-change))
  (global-set-key (kbd "C-x ,") 'my/goto-last-change))

;;;; citar templates and nerd-icons

(when (locate-library "citar")

  (with-eval-after-load 'citar

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

    (progn
      (defvar citar-indicator-files-icons
        (citar-indicator-create
         :symbol (nerd-icons-faicon
                  "nf-fa-file_o"
                  :face 'nerd-icons-green
                  :v-adjust 0.01)
         :function #'citar-has-files
         :padding "  " ; need this because the default padding is too low for these icons
         :tag "has:files"))
      (defvar citar-indicator-links-icons
        (citar-indicator-create
         :symbol (nerd-icons-faicon
                  "nf-fa-link"
                  :face 'nerd-icons-orange
                  :v-adjust 0.0)
         :function #'citar-has-links
         :padding "  "
         :tag "has:links"))
      (defvar citar-indicator-notes-icons
        (citar-indicator-create
         :symbol (nerd-icons-codicon
                  "nf-cod-note"
                  :face 'nerd-icons-blue
                  :v-adjust 0.0)
         :function #'citar-has-notes
         :padding "    "
         :tag "has:notes"))
      (defvar citar-indicator-cited-icons
        (citar-indicator-create
         :symbol (nerd-icons-faicon
                  "nf-fa-circle_o"
                  :face 'nerd-icon-green
                  :v-adjust 0.0)
         :function #'citar-is-cited
         :padding "  "
         :tag "is:cited"))
      (setq citar-indicators
            (list citar-indicator-files-icons
                  citar-indicator-links-icons
                  citar-indicator-notes-icons
                  citar-indicator-cited-icons)))
    ))

;;;; ox-hugo

;; 2024-04-26 move here, Important
(with-eval-after-load 'org
  (require 'ox-hugo)
  ;; (setq org-hugo-base-dir (file-truename "~/git/blog/"))
  (setq org-hugo-base-dir user-hugo-notes-dir) ;; 2024-10-07 fix quartz

  (progn
    ;; Append and update time-stamps for
    ;; #+hugo_lastmod: Time-stamp: <>
    ;; org-hugo-auto-set-lastmod should be nil
    (require 'time-stamp)
    ;; (add-hook 'write-file-functions 'time-stamp)
    ;; M-x time-stamp
    ;; Update last modified date for ox-hugo export
    ;; (add-hook 'before-save-hook 'time-stamp)
    (setq time-stamp-active t
          time-stamp-start "#\\+hugo_lastmod:[ \t]*"
          time-stamp-end "$"
          time-stamp-format "\[%Y-%m-%d\]")
    )

  (setq org-hugo-front-matter-format 'yaml)

  ;; My blog is created using Hugo and ox-hugo. It generates better markdown than what you would get using org-md-export!
  ;; It works well out-of-the-box. However, extra configuration is required to embed video.
  ;; In ox-hugo, uses #+begin_video to generate the <video> HTML5 tag (details in ox-hugo/issues/274).
  ;; In Hugo config, set markup.goldmark.renderer.unsafe to true (details in discourse.gohugo.io).
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webm")

  (setq org-hugo-section "notes") ; 2024-04-26 change
  (setq org-hugo-paired-shortcodes
        "mermaid callout cards details tabs") ; hint sidenote

  ;; https://ox-hugo.scripter.co/doc/formatting/
  ;; if org-hugo-use-code-for-kbd is non-nil
  ;; Requires CSS to render the <kbd> tag as something special.
  ;; eg: ~kbd~
  ;; (setq org-hugo-use-code-for-kbd t)

  ;; https://ox-hugo.scripter.co/doc/linking-numbered-elements/

  ;; org-export-dictionary 에 Figure, Table 에 한글 번역을 넣으면
  ;; 한글로 바뀌어 export 될 것이다.
  (setq org-hugo-link-desc-insert-type t)

  ;; 내보낼 때는 fill-column 끈다.
  (setq org-hugo-preserve-filling nil) ; important

  (setq org-hugo-allow-spaces-in-tags t) ; default t
  (setq org-hugo-prefer-hyphen-in-tags t) ; default t

  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "images") ; imgs
  ;; (setq org-hugo-default-static-subdirectory-for-externals "~/git/temp/notes.junghanacs.com/quartz/static/images") ; imgs

  ;; Override the default `org-hugo-export-creator-string' so that this
  ;; string is consistent in all ox-hugo tests.
  (setq org-hugo-export-creator-string "Emacs + Org-mode + ox-hugo")

  ;; In that normal example of the sidenote, ox-hugo trims the whitespace around
  ;; the sidenote block. That is configured by customizing the
  ;; org-hugo-special-block-type-properties variable:
  (progn
    (add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("details" :raw t)))
  ;; (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t)))

  ;; If this property is set to an empty string, this heading will not be auto-inserted.
  ;; default value is 'References'
  ;; https://ox-hugo.scripter.co/doc/org-cite-citations/
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References")

  (defun +org-export-remove-white-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string " " "" text)))
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-white-space t)

  )

;;;; insert unicode for notetaking

(with-eval-after-load 'org
  (defun my/insert-white-space ()
    (interactive)
    (insert " "))

  ;; "⊢" prove, "⊨" entail , "∉" notin
  (setq my/unicode-notetaking '( " " "§" "¶" "†" "‡" "№" "↔" "←" "→" "⊢" "⊨" "∉" ))

  (defun my/insert-unicode-notetaking ()
    "Insert Unicode for NoteTaking."
    (interactive)
    (insert (completing-read "Select unicode: " my/unicode-notetaking)))

  (evil-define-key '(insert normal) text-mode-map (kbd "M-M") #'my/insert-unicode-notetaking)
  (evil-define-key '(insert normal) text-mode-map (kbd "M-m") #'my/insert-white-space)
  )

;;;; TODO Org-Hugo Links

;; lambda-setup/lem-setup-org-extensions.el
;; (progn
;;   ;; New link type for Org-Hugo internal links
;;   (defun org-hugo-link-complete ()
;;     "Create link with Hugo ref shortcode"
;;     (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

;;   (defun org-hugo-follow (link)
;;     (find-file (expand-file-name link)))

;;   (with-eval-after-load 'org
;;     (org-link-set-parameters "hugo"
;;                              :complete 'org-hugo-link-complete
;;                              :follow 'org-hugo-follow))
;;   )

;;; provide

(provide 'uniconfig)

;;; end-of configuration
