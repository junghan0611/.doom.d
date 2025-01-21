;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; 정말 설정 변경할 만한 녀석들만 짧게 넣어야 한다
;; 나머지는 모듈이나 사용자 설정으로 다 빼놔야 혼란이 적고 손댈일이 없다.

;; Check for missing external software
;;
;; - soffice (LibreOffice): View and create office documents
;; - zip: Unpack ePub documents
;; - pdftotext (poppler-utils): Convert PDF to text
;; - djvu (DjVuLibre): View DjVu files
;; - curl: Reading RSS feeds
;; - divpng: Part of LaTeX
;; - dot (GraphViz): Create note network diagrams
;; - convert (ImageMagick): Convert image files
;; - gm (GraphicsMagick): Convert image files
;; - latex (TexLive, MacTex or MikTeX): Preview LaTex and export Org to PDF
;; - hunspell: Spellcheck. Also requires a hunspell dictionary
;; - grep: Search inside files
;; - ripgrep: Faster alternative for grep
;; - gs (GhostScript): View PDF files
;; - mutool (MuPDF): View PDF files
;; - mpg321, ogg123 (vorbis-tools), mplayer, mpv, vlc: Media players

;;; Commentary:

;; ❶ :: U+2776 ==> 더원싱 태그로 활용
;; ㉽ :: U+327D
;; ㉼ :: U+327C

;;; Load 'Per-Machine' - User Configs

;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.
(let ((per-machine-filename (concat user-dotemacs-dir "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;;; Load 'user-keys'

(let ((user-keys-filename (concat user-dotemacs-dir "user-keys.el")))
  (when (file-exists-p user-keys-filename)
    (load-file user-keys-filename)))

;;; Load 'user-configs'

(load! "+user-configs")

;;; :lang org

;;;; org reloading

(after! org
  (message "after org - config")

  (require 'org-funcs)
  (require 'org-config)
  ;; (load-file (concat user-dotemacs-dir "lisp/org-funcs.el"))
  ;; (load-file (concat user-dotemacs-dir "lisp/org-config.el"))
  (setq org-id-locations-file (file-name-concat org-directory (concat "." system-name "-orgids"))) ; ".org-id-locations"))
  ;; (+org-init-keybinds-h) -> 2024-06-01 여기 키바인딩 관련 부분 뒤에서 다시 잡아줌

  ;; (setq org-attach-use-inheritance nil) ; selective

  ;; overide here! important
  ;; (setq org-insert-heading-respect-content nil) ; doom t

  ;; (progn
  ;;   ;; 2024-06-04 file - id - http/https
  ;;   (org-link-set-parameters "file" :face `(:inherit link :weight semibold :slant italic :underline t)) ;; italic
  ;;   (org-link-set-parameters "http" :face `(:inherit warning :weight semibold :underline t))
  ;;   (org-link-set-parameters "info" :face `(:inherit info-file :weight semibold :underline t))
  ;;   (org-link-set-parameters "https" :face `(:inherit warning :weight semibold :underline t))
  ;;   )
  (org-link-set-parameters "denote" :face `(:inherit success :weight semibold :underline t)) ; id

  ;; 2024-06-24 performance issue
  ;; (remove-hook 'org-mode-hook 'org-eldoc-load)

  ;; 2024-09-15 TODO
  ;; (progn
  ;;   ;; Those advice were designed when using a bottom modeline. Since we are using
  ;;   ;; a header line, we must remove them.
  ;;   (advice-remove 'org-fast-tag-selection #'+popup--org-fix-popup-window-shrinking-a)
  ;;   (advice-remove 'org-fast-todo-selection #'+popup--org-fix-popup-window-shrinking-a)

  ;;   ;; (defadvice! +popup--suppress-delete-other-windows-a (fn &rest args)
  ;;   ;; Courtesy: doom emacs (popup/+hacks.el)
  ;;   (defun +popup--supress-delete-other-windows-a (origin-fn &rest args)
  ;;     (if +popup-mode
  ;;         (cl-letf (((symbol-function #'delete-other-windows) #'ignore)
  ;;                   ((symbol-function #'delete-window)        #'ignore))
  ;;           (apply origin-fn args))
  ;;       (apply origin-fn args)))

  ;;   (advice-add #'org-fast-tag-selection :around #'+popup--supress-delete-other-windows-a)
  ;;   (advice-add #'org-fast-todo-selection :around #'+popup--supress-delete-other-windows-a)
  ;;   )

  )

;;;; DONT org-modern

;; (after! org

;;   (message "after org - org-modern")
;;   (require 'org-modern)
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

;;;; TODO org-src-mode-map

;; (with-eval-after-load 'org-src
;;   ;; "c" 'org-edit-src-exit
;;   ;; "a" 'org-edit-src-abort
;;   ;; "k" 'org-edit-src-abort

;; C-c C-c geiser-eval-definition
;;   ;; I prefer C-c C-c over C-c ' (more consistent)
;;   (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
;;   )

;;;; org-capture templates

;;;;; org-contacts

(after! org
  (require 'org-contacts)
  (add-to-list
   'org-capture-templates
   `("c" "Contacts" entry (file ,(my/org-contacts-file))
     "* %(org-contacts-template-name)\n:PROPERTIES:\n:GITHUB:\n:EMAIL: a@a.com\n:URL:\n:NOTE:\n:END:\n%U\n%T\n%a\n")) ;; :CUSTOM_ID: %(prot-org--id-get)\n
  )

;;;;; org-bookmarks

(after! org
  (require 'org-bookmarks)
  (add-to-list 'org-capture-templates
               `("b" ,(format "%s\tAdd a new bookmark to %s"
                              (when (fboundp 'nerd-icons-mdicon)
                                (nerd-icons-mdicon
                                 "nf-md-bookmark_plus_outline"
                                 :face 'nerd-icons-blue))
                              (file-name-nondirectory org-bookmarks-file))
                 entry (file ,(expand-file-name org-bookmarks-file))
                 ,(concat
                   "* %^{bookmark title}\t\t\t\t"
                   (format ":%s:" org-bookmarks-tag)
                   "
:PROPERTIES:
:URL:  %^C
:DATE: %t
:END:")
                 :empty-lines 1
                 :jump-to-captured t
                 :refile-targets ((,org-bookmarks-file :maxlevel 3)))
               :append))

;;;; org-journal

;; (require 'side-journal)
(progn
  (require 'org-journal)

  (defun my-old-carryover (old_carryover)
    (save-excursion
      (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
        (dolist (entry (reverse old_carryover))
          (save-restriction
            (narrow-to-region (car entry) (cadr entry))
            (goto-char (point-min))
            (org-scan-tags '(lambda ()
                              (org-todo "DONT")
                              (org-set-tags ":ARCHIVE:"))
                           matcher org--matcher-tags-todo-only))))))

  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"NEXT\"")
  (setq org-journal-handle-old-carryover-fn 'my-old-carryover)

  (setq org-journal-dir (concat user-org-directory "journal"))
  (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org")
  (setq org-journal-date-format "%Y-%m-%d %a") ; Week%W:

  ;; (setq org-journal-date-prefix "#+title: ")
  ;; (setq org-journal-time-prefix "** ") ; default **
  ;; (setq org-journal-time-format "%R ") ; "[%<%Y-%m-%d %a %H:%M>]" ; default "%R "

  (setq org-journal-enable-agenda-integration t) ; default nil
  (setq org-journal-file-type 'weekly) ; default 'daily

  (setq org-journal-tag-alist '(("meet" . ?m) ("dev" . ?d) ("idea" . ?i) ("emacs" . ?e) ("discuss" . ?c) ("1on1" . ?o))) ; default nil
  )

;;;; TODO om-dash org-based dashboards

;; [cite:@gavvomdash24] Building blocks for org-based dashboards.
;; (use-package! om-dash
;;   :defer 5
;;   :config
;;   (require 'parse-csv)
;;   )

;;; :ui

;;;; jit-lock-defer-time

;; NOTE: setting this to `0' like it was recommended in the article above seems
;; to cause fontification to happen in real time, which can be pretty slow in
;; large buffers. Giving it a delay seems to be better.
;; (setq jit-lock-defer-time 0.05) ;; better
;; (setq jit-lock-defer-time 0) ; important

;; My guess for how big this number should be for my setup. Call
;; `cae-set-jit-lock-chunk-size-to-optimal' on a few different files to get an
;; idea.
;; (setq jit-lock-chunk-size 2500) ; default 1500

;;;; savehist-auto-save-interval

(setq savehist-autosave-interval 300)

;;;; custom diff-hl

(use-package! diff-hl
  :config
  (setq diff-hl-disable-on-remote t) ; default nil
  (setq diff-hl-flydiff-delay 0.8)  ; doom 0.5, default: 0.3
  ;; (remove-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
  ;; (remove-hook 'diff-hl-flydiff-mode-hook #'+vc-gutter-init-flydiff-mode-h)
  )

;;;; modus-themes

(use-package! modus-themes
  :commands (modus-themes-toggle)
  :init
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t
        modus-themes-custom-auto-reload t

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `italic', `WEIGHT'
        ;; modus-themes-prompts '(bold)

        ;; The `modus-themes-completions' is an alist that reads two
        ;; keys: `matches', `selection'.  Each accepts a nil value (or
        ;; empty list) or a list of properties that can include any of
        ;; the following (for WEIGHT read further below):
        ;; `matches'   :: `underline', `italic', `WEIGHT'
        ;; `selection' :: `underline', `italic', `WEIGHT'
        ;; modus-themes-completions
        ;; '((matches   . (semibold))
        ;;   (selection . (semibold text-also)))

        ;; modus-themes-common-palette-overrides
        ;; `((fg-mode-line-active fg-main) ; Black

        ;;   ;; Comments are yellow, strings are green
        ;;   (comment yellow-cooler)
        ;;   (string green-warmer)

        ;;   ;; "Make the mode line borderless"
        ;;   (border-mode-line-active unspecified)
        ;;   (border-mode-line-inactive unspecified)

        ;;   ;; "Make matching parenthesis more or less intense"
        ;;   (bg-paren-match bg-magenta-intense)
        ;;   (underline-paren-match unspecified)

        ;;   ;; Intense magenta background combined with the main foreground
        ;;   ;; (bg-region bg-magenta-subtle)
        ;;   ;; (fg-region fg-main)

        ;;   ;; Links
        ;;   ;; (underline-link border)
        ;;   ;; (underline-link-visited border)
        ;;   ;; (underline-link-symbolic border)

        ;;   (bg-heading-0 bg-green-subtle) ; green
        ;;   ;; (bg-heading-1 bg-dim)
        ;;   ;; (bg-heading-2 bg-yellow-nuanced)
        ;;   ;; (bg-heading-3 bg-blue-nuanced) ; blue

        ;;   ;; copy from intense
        ;;   (overline-heading-0 unspecified)
        ;;   (overline-heading-1 magenta-cooler)
        ;;   (overline-heading-2 magenta-warmer)

        ;;   ;; And expand the preset here. Note that the ,@ works because we use
        ;;   ;; the backtick for this list, instead of a straight quote.
        ;;   ;; ,@modus-themes-preset-overrides-faint
        ;;   ;; ,@modus-themes-preset-overrides-intense
        ;;   )
        )

  ;; (when (display-graphic-p) ; gui
  ;;   ;; (setq modus-themes-variable-pitch-ui t)
  ;;   ;; The `modus-themes-headings' is an alist: read the manual's
  ;;   ;; node about it or its doc string. Basically, it supports
  ;;   ;; per-level configurations for the optional use of
  ;;   ;; `variable-pitch' typography, a height value as a multiple of
  ;;   ;; the base font size (e.g. 1.5), and a `WEIGHT'.
  ;;   ;; (setq modus-themes-headings
  ;;   ;;       '(
  ;;   ;;         (0                . (variable-pitch bold 1.2))
  ;;   ;;         (1                . (variable-pitch bold  1.1))
  ;;   ;;         (2                . (variable-pitch semibold 1.0))
  ;;   ;;         (3                . (variable-pitch semibold 1.0))
  ;;   ;;         (4                . (medium 1.0))
  ;;   ;;         (5                . (medium 1.0))
  ;;   ;;         (6                . (medium 1.0))
  ;;   ;;         (7                . (medium 1.0))
  ;;   ;;         (agenda-date      . (semibold 1.0))
  ;;   ;;         (agenda-structure . (bold 1.1))
  ;;   ;;         (t                . (medium 1.0)))
  ;;   ;;       )
  ;;   )

  (defun my/modus-themes-custom-faces ()
    (interactive)
    ;; (message "modus-themes-after-hook : my-modus-themes-custom-faces")
    (modus-themes-with-colors
      (custom-set-faces
       `(consult-separator ((,c :inherit default :foreground ,yellow-intense)))
       `(consult-notes-time ((,c :inherit default :foreground ,cyan-intense)))

       ;; `(ekg-notes-mode-title ((,c :inherit outline-1 :weight bold :height 1.0)))
       ;; `(ekg-title ((,c :inherit outline-2 :weight semibold :height 1.0 :underline t)))
       ;; `(ekg-tag ((,c :background ,bg-yellow-nuanced :box (:line-width 1 :color ,fg-dim) :foreground ,fg-main :style nil))) ; prose-tag
       ;; `(ekg-resource ((,c :inherit outline-7 :weight regular :height 1.0 :underline t)))
       ;; `(ekg-metadata ((,c :inherit outline-1 :weight regular :height 1.0)))

       `(org-list-dt ((,c :foreground ,fg-main :weight bold))) ;; 2025-01-14

       `(org-tag ((,c :background ,bg-yellow-nuanced :box (:line-width 1 :color ,fg-dim) :foreground ,fg-main :style nil))) ; prose-tag

       `(diredp-file-name ((,c :foreground ,fg-main)))

       ;; `(diredp-dir-name ((,c :inherit dired-directory)))
       ;; `(diredp-ignored-file-name ((,c :inherit shadow)))
       ;; `(diredp-compressed-file-suffix ((,c :foreground ,err)))
       ;; `(diredp-symlink ((,c :inherit dired-symlink)))
       ;; `(diredp-dir-heading ((,c :inherit bold)))
       ;; `(diredp-file-suffix ((,c :foreground ,variable)))
       ;; `(diredp-date-time ((,c :foreground ,date-common)))
       ;; `(diredp-no-priv ((,c :inherit shadow)))
       ;; `(diredp-number ((,c :inherit shadow)))
       ;; `(diredp-dir-priv ((,c :inherit dired-directory)))
       ;; `(diredp-exec-priv ((,c :foreground ,accent-1)))
       ;; `(diredp-write-priv ((,c :foreground ,accent-0)))
       ;; `(diredp-rare-priv ((,c :foreground ,accent-3)))
       ;; `(diredp-read-priv ((,c :foreground ,fg-main)))
       ;; `(diredp-link-priv ((,c :foreground ,fg-link)))
       ;; `(diredp-other-priv ((,c :foreground ,accent-2)))

       ;; `(diredp-autofile-name ((,c :background ,bg-inactive)))
       ;; `(diredp-compressed-file-name ((,c :foreground ,warning)))
       ;; `(diredp-deletion ((,c :inherit dired-flagged)))
       ;; `(diredp-deletion-file-name ((,c :inherit diredp-deletion)))
       ;; `(diredp-executable-tag ((,c :inherit diredp-exec-priv)))
       ;; `(diredp-flag-mark ((,c :inherit dired-marked)))
       ;; `(diredp-flag-mark-line ((,c :inherit dired-marked)))
       ;; `(diredp-tagged-autofile-name ((,c :inherit (diredp-autofile-name dired-marked))))

       ;; `(org-agenda-diary ((,c :inherit org-agenda-calendar-sexp :foreground ,fg-main :weight semibold)))

       ;; `(org-link ((,c :inherit link :weight bold)))
       ;; `(denote-faces-link ((,c :inherit link :weight bold :slant italic)))

       ;; `(org-drawer ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata :height 0.8)))
       ;; `(org-special-keyword ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))

       ;; 2024-07-03 spacious-padding
       `(tab-bar ((,c :background ,bg-tab-bar)))
       `(tab-bar-tab-group-current ((,c :inherit bold :background ,bg-tab-current :box (:line-width -2 :color ,bg-tab-current) :foreground ,fg-alt)))
       `(tab-bar-tab-group-inactive ((,c :background ,bg-tab-bar :box (:line-width -2 :color ,bg-tab-bar) :foreground ,fg-alt)))
       `(tab-bar-tab ((,c :inherit bold :box (:line-width -2 :color ,bg-tab-current) :background ,bg-tab-current)))
       `(tab-bar-tab-inactive ((,c :box (:line-width -2 :color ,bg-tab-other) :background ,bg-tab-other)))
       `(tab-bar-tab-ungrouped ((,c :inherit tab-bar-tab-inactive)))
       `(fringe ((,c :background ,bg-dim)))

       `(vterm-color-black ((,c :background "gray25" :foreground "gray25")))
       `(vterm-color-yellow ((,c :background ,yellow-intense :foreground ,yellow-intense)))
       `(org-mode-line-clock ((,c :inherit bold :foreground ,modeline-info)))
       `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,modeline-err)))
       `(jinx-misspelled ((,c :underline (:style wave :color ,magenta-cooler))))
       ;; `(ten-id-face ((,c :inherit font-lock-keyword-face :underline (:style double-line :color ,cyan))))

       ;; `(keycast-command ((,c :inherit default :height 0.9)))
       )
      )
    (when (locate-library "spacious-padding")
      (spacious-padding-mode +1))
    )
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
  ) ;; end-of use-package

;;;; ef-themes

(use-package! ef-themes
  :defer t
  :init
  (setq ef-themes-to-toggle
        (let ((hr (nth 2 (decode-time))))
          (if (or (< hr 6) (< 19 hr)) ; between 7 PM and 6 AM
              '(ef-owl ef-eagle) ; load dark theme first
            '(ef-eagle ef-owl))))

  (defun ef-themes-load-random-light ()
    (interactive) (ef-themes-load-random 'light))
  (defun ef-themes-load-random-dark ()
    (interactive) (ef-themes-load-random 'dark))
  :config
  (setq ef-themes-light-themes
        '(
          ef-maris-light ; blue
          ef-eagle ; yellow
          ef-kassio ; pink
          ef-frost ; green
          ef-spring
          ))
  (setq ef-themes-dark-themes
        '(;; new style
          ef-melissa-dark ;; Like solarized but much nicer colors.
          ef-dream ; 보라 - 드라큘라
          ef-rosa ; 자주
          ef-maris-dark
          ef-elea-dark
          ef-owl ; 2024-08-19 new
          ;; ef-bio ef-trio-dark ef-autumn ef-dark ef-duo-dark ef-night ef-winter ef-symbiosis
          ))

  ;; Read the doc string or manual for this one.  The symbols can be
  ;; combined in any order.
  (setq ef-themes-region '(intense no-extend neutral))

  ;; (when (display-graphic-p) ; gui
  ;;   (setq ef-themes-variable-pitch-ui t))

  (defun my/ef-themes-custom-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
  The exact color values are taken from the active Ef theme."
    (interactive)
    ;; (message "ef-themes-post-load-hook : my-ef-themes-custom-faces")
    (ef-themes-with-colors
     (custom-set-faces
      `(consult-separator ((,c :inherit default :foreground ,yellow)))
      `(consult-notes-time ((,c :inherit default :foreground ,cyan)))

      ;; `(ekg-notes-mode-title ((,c :inherit outline-1 :weight bold :height 1.0)))
      ;; `(ekg-title ((,c :inherit outline-2 :weight semibold :height 1.0 :underline t)))
      ;; `(ekg-tag ((,c :background ,bg-yellow-subtle :box (:line-width 1 :color ,fg-dim) :foreground ,fg-main :style nil))) ; prose-tag
      ;; `(ekg-resource ((,c :inherit outline-7 :weight regular :height 1.0 :underline t)))
      ;; `(ekg-metadata ((,c :inherit outline-1 :weight regular :height 1.0)))

      ;; `(org-link ((,c :inherit link :weight bold)))
      ;; `(denote-faces-link ((,c :inherit link :weight bold :slant italic)))
      ;; `(org-agenda-diary ((,c :inherit org-agenda-calendar-sexp :foreground ,fg-main :weight semibold)))

      `(org-list-dt ((,c :foreground ,fg-main :weight bold))) ;; 2025-01-14
      `(org-tag ((,c :background ,bg-yellow-subtle :box (:line-width 1 :color ,fg-dim) :foreground ,fg-main :style nil))) ; prose-tag
      `(diredp-file-name ((,c :foreground ,fg-main)))

      `(tab-bar ((,c :background ,bg-tab-bar)))
      `(tab-bar-tab-group-current ((,c :inherit bold :background ,bg-tab-current :box (:line-width -2 :color ,bg-tab-current) :foreground ,fg-alt)))
      `(tab-bar-tab-group-inactive ((,c :background ,bg-tab-bar :box (:line-width -2 :color ,bg-tab-bar) :foreground ,fg-alt)))
      `(tab-bar-tab ((,c :inherit bold :box (:line-width -2 :color ,bg-tab-current) :background ,bg-tab-current)))
      `(tab-bar-tab-inactive ((,c :box (:line-width -2 :color ,bg-tab-other) :background ,bg-tab-other)))
      `(tab-bar-tab-ungrouped ((,c :inherit tab-bar-tab-inactive)))

      ;; `(keycast-command ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-main :foreground ,fg-main :weight semibold)))
      ;; `(keycast-command ((,c :inherit default :height 0.9)))
      `(fringe ((,c :background ,bg-dim)))
      `(org-mode-line-clock ((,c :inherit bold :foreground ,modeline-info)))
      `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,modeline-err)))
      `(jinx-misspelled ((,c :underline (:style wave :color ,magenta-cooler))))
      ;; `(ten-id-face ((,c :inherit font-lock-keyword-face :underline (:style double-line :color ,cyan))))
      )
     (setq hl-todo-keyword-faces
           `(("HOLD" . ,yellow)
             ("TODO" . ,red)
             ("NEXT" . ,blue)
             ("THEM" . ,magenta)
             ("PROG" . ,cyan-warmer)
             ("OKAY" . ,green-warmer)
             ("DONT" . ,yellow-warmer)
             ("FAIL" . ,red-warmer)
             ("BUG" . ,red-warmer)
             ("DONE" . ,green)
             ("NOTE" . ,blue-warmer)
             ("KLUDGE" . ,cyan)
             ("HACK" . ,cyan)
             ("TEMP" . ,red)
             ("FIXME" . ,red-warmer)
             ("XXX+" . ,red-warmer)
             ("REVIEW" . ,red)
             ("DEPRECATED" . ,yellow))))
    (when (locate-library "spacious-padding")
      (spacious-padding-mode +1))
    ;; (setq ring-bell-function 'jf/pulse)
    )
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-custom-faces)
  )

;;;; DONT my-themes for doom-themes

;; (require 'my-themes)
;; (add-hook 'doom-load-theme-hook 'my/load-custom-set-faces 90) ; for doom themes

;;;; spacious-padding

(use-package! spacious-padding
  ;; :if window-system
  :hook (server-after-make-frame . spacious-padding-mode)
  :init
  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it is very
  ;; flexible.
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line-active
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
  (setq spacious-padding-widths
        '(:header-line-width 4
          :mode-line-width 6
          :tab-width 6 ; sync mode-line-width for keycast-tab-bar
          :internal-border-width 20 ; 15
          :right-divider-width 20 ; 30
          :scroll-bar-width 8
          :fringe-width 20 ;; 8
          ))
  (add-hook 'doom-load-theme-hook #'spacious-padding-mode)
  :config
  ;; (remove-hook 'doom-init-ui-hook #'window-divider-mode)
  ;; (blink-cursor-mode t)
  ;; (when (fboundp 'tooltip-mode) (tooltip-mode 1))
  ;; (when (fboundp 'tool-bar-mode) (tool-bar-mode 1))
  ;; (when (display-graphic-p) ; gui
  ;;   (menu-bar-mode +1))
  (spacious-padding-mode +1)
  )

;;;; list-unicode-display

(use-package! list-unicode-display :defer t)

;;;; pulse : built-in - visual feedback

(progn
  ;; add visual pulse when changing focus, like beacon but built-in
  ;; from from https://karthinks.com/software/batteries-included-with-emacs/
  (require 'pulse)
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command
           '(scroll-up-command
             scroll-down-command recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))
  )

;;; :lang pkm
;;;; after denotes : Load custom denote

(after! denote
  (message "Load: custom denote")
  ;; no dependency on org-roam, use default's org-id
  (require 'denote-funcs)
  (require 'denote-config)
  (require 'denote-hugo) ; for publish
  ;; (add-hook 'doom-first-input-hook #'my/refresh-agenda-files)
  )

;;;; DONT consult-notes-file-dir-sources

(after! consult-notes
  (setq consult-notes-file-dir-sources
        '(
          ;; ("root"  ?r  "~/sync/org")
          ("meta/Hub" ?h "~/sync/org/meta")
          ;; ("bib/Literature" ?b "~/sync/org/bib")
          ;; ("notes/Fleeting" ?n "~/sync/org/notes")
          ;; ("posts/Permanent" ?p "~/sync/org/posts")
          ;; ("llmlog/AI" ?l "~/sync/org/llmlog")
          ;; ("docs/Zettels" ?d "~/sync/org/docs")
          ;; ("07.Journal" ?j "~/sync/org/journal")
          ;; ("09.Ekg" ?e "~/sync/org/ekg")
          ;; ("10.MD" ?m "~/sync/org/md")
          ;; ("11.Import" ?i "~/sync/org/import")
          ;; ("12.Talks" ?t  "~/sync/org/talks")
          )))

;;; Waiting

;;;; atomic-chrome

(use-package! atomic-chrome
  :if window-system
  :defer 4
  :commands (atomic-chrome-start-server)
  :config
  (atomic-chrome-start-server))

;;;; google-translte

(use-package! google-translate
  :defer 3
  :init
  (require 'google-translate)
  :init
  (autoload 'google-translate-translate "google-translate-core-ui" "google-translate-translate" nil nil)

  :config
  ;; load +google-translate
  (load! "+google-translate")

  (defadvice! google-translate-at-point--set-lang-auto (fn &optional override-p)
    :around #'google-translate-at-point
    (pcase-let ((`(,src ,tgt)
                 (alist-get current-input-method
                            '((nil . (en ko))
                              ("korean-hangul" . (ko en)))
                            nil nil #'string-equal)))
      (let ((google-translate-default-source-language (symbol-name src))
            (google-translate-default-target-language (symbol-name tgt)))
        (funcall-interactively fn override-p))))

  (defun google-translate-to-korean (&optional str)
    "Translate given string automatically without language selection prompt."
    (let ((lang
           (cond
            ((string-match "[가-힣]" str)
             "ko")
            ((or (string-match "[ァ-ヶー]" str)
                 (string-match "[ぁ-んー]" str)
                 ;; (string-match "[亜-瑤]" str)
                 )
             "ja")
            ((string-match "[一-龥]" str)
             "zh-CN")
            (t
             "en"))))
      (google-translate-translate
       lang
       (if (string= "ko" lang)
           "en"
         "ko")
       str)))

  (setq google-translate-input-method-auto-toggling t
        google-translate-preferable-input-methods-alist
        '((nil . ("en"))
          (korean-hangul . ("ko"))))

  (setq google-translate-show-phonetic t)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "ko")

  ;; it doesn't pop to the buffer automatically
  (defun google-translate--find-buffer (x)
    (pop-to-buffer "*Google Translate*"))

  (advice-add 'google-translate-buffer-output-translation :after #'google-translate--find-buffer)

  (add-to-list
   'display-buffer-alist
   '("\\*Google Translate\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.25)))
  )


;;;; TODO translate-mode

(use-package! translate-mode
  :defer 5
  ;; :hook (translate-mode . translate//set-translate-mode-paragraph-functions)
  )

;; (progn
;;   (defun translate/translate-current-reference-paragraph ()
;;     "Show all available translations of the reference paragraph at point in a pop-up frame."
;;     (interactive)
;;     (gt-translate translate//paragraph-translator))

;;   (defun translate/translate-word-at-point ()
;;     "Pop-up translations of the word at point."
;;     (interactive)
;;     (gt-translate translate//word-translator))

;;   (defun translate//set-translate-mode-paragraph-functions ()
;;     (cond ((eq major-mode 'markdown-mode)
;;            (setq translate-forward-paragraph-function 'markdown-forward-paragraph
;;                  translate-backward-paragraph-function 'markdown-backward-paragraph))
;;           ((eq major-mode 'org-mode)
;;            (setq translate-forward-paragraph-function 'org-forward-paragraph
;;                  translate-backward-paragraph-function 'org-backward-paragraph))))
;;   )

;;;; TODO go-translate v3 API

;; M-x gt-do-translate
;; /vanilla/douo-dotfiles-kitty/init.el

(use-package! go-translate
  :defer t
  :after pdf-tools
  :if window-system
  :commands (douo/go-do-translate douo/pdf-view-translate)
  :init
  (setq gt-langs '(en ko))
  ;; Translate by paragraph and insert each result at the end of source paragraph
  ;; This configuration is suitable for translation work. That is: Translate -> Modify -> Save
  (defun douo/go-do-translate (text-property-string)
    (gt-start (gt-translator
               :taker (gt-taker
                       ;; 单个换行替换为空格
                       :text (replace-regexp-in-string
                              "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2"
                              text-property-string))
               :engines (gt-google-engine)
               :render (gt-posframe-pop-render))))
  :custom
  (gt-cache-p t)
  (gt-default-translator
   (gt-translator
    :taker (gt-taker :langs '(en ko) :text (lambda () (replace-regexp-in-string
                                                       "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2"
                                                       (thing-at-point 'paragraph)))
                     :prompt t
                     )
    :engines (gt-google-engine)
    :render (gt-buffer-render)))
  :bind
  (:map pdf-view-mode-map
        ;; consult 不支持与 pdf-tools 的交互
        ;; ("C-s" . isearch-forward)
        ;; ("C-r" . isearch-backward)
        ("C-t" . douo/pdf-view-translate))
  (:map embark-prose-map ;; 覆盖 transpose-xxx
        ("t" . douo/go-do-translate))
  (:map embark-region-map ;; 覆盖 transpose-regions
        ("t" . douo/go-do-translate))
  :config
  (setq gt-chatgpt-key user-openai-api-key)
  ;; (setq gt-chatgpt-model "gpt-4o-mini")

  (require 'pdf-tools)
  ;; 自定义 pdf 翻译文本提取器
  ;; 如果有高亮返回高亮文本，无则返回整页文本
  (defun douo/gts-pdf-view-selection-texter ()
    (unless (pdf-view-active-region-p)
      (pdf-view-mark-whole-page))
    ;; remove-newline-characters-if-not-at-the-end-of-sentence
    ;; ::HACK:: 解决 pdf 提取文本不能正确断行的问题
    ;; 移除不是处于句尾[.!?]的换行符
    (replace-regexp-in-string "\\([^.!?]\\)\n\\([^ ]\\)" "\\1 \\2"
                              (car (pdf-view-active-region-text))))
  (defvar douo/pdf-translater
    (gt-translator
     :taker (gt-taker :text 'douo/gts-pdf-view-selection-texter)
     :engines (list (gt-google-engine))
     :render (gt-buffer-render)
     ;; :splitter (gts-paragraph-splitter)
     ))
  (defun douo/pdf-view-translate ()
    (interactive)
    (gt-start douo/pdf-translater)
    ;;  cancel selection in emacs
    (deactivate-mark))
  ) ; end-of go-translate

;; (setq gt-default-translator
;;       (gt-translator
;;        :taker (gt-taker :text 'buffer :pick 'paragraph)
;;        :engines (gt-google-engine)
;;        :render (gt-insert-render :type 'after)))

;; Translate the current paragraph and replace it with the translation result
;; This configuration is suitable for scenes such as live chat. Type some text, translate it, and send it
;; (setq gt-default-translator
;;       (gt-translator
;;        :taker (gt-taker :text 'paragraph :pick nil)
;;        :engines (gt-google-engine)
;;        :render (gt-insert-render :type 'replace)))

;; Translate specific words in current paragraph and insert the result after each word
;; This configuration can help in reading articles with some words you don't know
;; (setq gt-default-translator
;;       (gt-translator
;;        :taker (gt-taker :text 'paragraph
;;                         :pick 'word
;;                         :pick-pred (lambda (w) (length> w 6)))
;;        :engines (gt-google-engine)
;;        :render (gt-insert-render :type 'after
;;                                  :rfmt " (%s)"
;;                                  :rface '(:foreground "grey"))))

;; (setq gt-default-translator
;;       (gt-translator :taker (gt-taker :pick nil :prompt t)
;;                      :engines (gt-chatgpt-engine :stream t)
;;                      :render (gt-insert-render)))

;;;; TODO IDE Layout with Side Windows

;; https://whhone.com/emacs-config/#ide-layout-with-side-windows

;;;; DONT browse-hist

;; (use-package! browser-hist
;;   :defer t
;;   :init
;;   (require 'embark) ; load Embark before the command (if you're using it)
;;   :config
;;   (setq browser-hist-db-paths
;;         '((edge . "/home/junghan/.config/microsoft-edge/Default/History")
;;           (whale . "/home/junghan/.config/naver-whale/Default/History")
;;           (chrome . "$HOME/.config/google-chrome/Default/History")
;;           (brave . "$HOME/.config/BraveSoftware/Brave-Browser/Default/History")
;;           (firefox . "$HOME/.mozilla/firefox/*.default-release-*/places.sqlite")
;;           (qutebrowser . "$HOME/.local/share/qutebrowser/history.sqlite")))
;;   (setq browser-hist--db-fields
;;         '((chrome      "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
;;           (edge    "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
;;           (whale    "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
;;           (qutebrowser "title"    "url"    "History"       "ORDER BY atime           desc")
;;           (brave       "title"    "url"    "urls"          "ORDER BY last_visit_time desc")
;;           (firefox     "title"    "url"    "moz_places"    "ORDER BY last_visit_date desc")
;;           ))
;;   (setq browser-hist-default-browser 'edge)
;;   :commands (browser-hist-search))


;;;; DONT dictionary-overlay

;; 수정이 필요할 듯
;; https://github.com/ginqi7/dictionary-overlay
;; (use-package! dictionary-overlay)

;;;; TODO cfw: my-open-calendar

;; init.el :app calendar
;; (defun my-open-calendar ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     (cfw:org-create-source "Green")  ; org-agenda source
;;     (cfw:org-create-file-source "cal" "/path/to/cal.org" "Cyan")  ; other org source
;;     (cfw:howm-create-source "Blue")  ; howm source
;;     (cfw:cal-create-source "Orange") ; diary source
;;     (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
;;     (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
;;     )))

;;;; TODO paw

;; https://emacs-china.org/t/paw-el-emacs-lingq/27331/71
;; (use-package! paw
;;   :defer t)

;; (set-popup-rules! '(("^\\*paw-view-note*" :size 0.35 :side right :quit t :modeline t :select nil :ttl nil :vslot 2 :slot 1)
;;                     ("^\\*paw-sub-note*" :height 0.5 :side right :quit t :modeline t :select t :ttl nil :vslot 2 :slot 2)))

;;;; TODO presentation - dslide and moc

(use-package! dslide :defer t)
(use-package! default-text-scale :defer t)
(use-package! moc :after default-text-scale :defer t)

;;; :custom 'Local' Packages

;;;; elot : literate ontology tools

;; (add-to-list 'load-path "~/sync/emacs/forked-pkgs/elot")
(use-package! elot
  :defer t)

;;;; TODO emacs-bluesky

;; (add-to-list 'load-path "~/sync/emacs/git/junghan0611/emacs-bluesky/")
;; (load-file "~/sync/emacs/git/junghan0611/emacs-bluesky/bluesky.el")

;;;; pylookup

(use-package! pylookup
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :config
  ;; (spacemacs/set-leader-keys-for-major-mode 'python-mode
  ;;                                           "hH" 'pylookup-lookup)
  ;; (evilified-state-evilify-map pylookup-mode-map
  ;;                              :mode pylookup-mode)
  (setq
   pylookup-dir (concat user-dotemacs-dir "local/pylookup/")
   pylookup-program (concat pylookup-dir "pylookup.py")
   pylookup-db-file (concat pylookup-dir "pylookup.db"))
  (setq pylookup-completing-read 'completing-read)
  (setq pylookup-html-locations '("http://docs.python.org/ko/3.10")))

;;;; prot-dired-grep-marked-files

(require 'prot-dired)

;;; Load Unified Configuration

;; unified config for spacemacs and doom emacs
(require 'uniconfig)

;;; Load Transient & Hydra Menu

(require 'hydrakeys)

;;; Load Keys

(require 'keys)

;;; Load 'Doom' Keybindings

;; override and add doom keybindings
(load! "+doomkeys")

;;; persp-mode with tab-bar for open-workspaces

(after! persp-mode
  ;; shares a common set of buffers between perspectives
  (defvar persp-shared-buffers
    '("*scratch*" "*Org Agenda(n)*" "*Messages*" "*doom:scratch*"))

  (add-hook 'persp-activated-functions
            (lambda (_) (persp-add-buffer persp-shared-buffers))))

;;;; custom tab-bar global-mode-string

(progn
  (require 'tab-bar)
  (setq tab-bar-format
        '( ;; tab-bar-format-history
          tab-bar-format-tabs
          tab-bar-separator
          tab-bar-format-add-tab
          tab-bar-format-align-right
          tab-bar-format-global
          ))

;;;###autoload
  (defun my/load-global-mode-string ()
    (interactive)

    ;; (message "my/load-global-mode-string")
    (when (not (bound-and-true-p display-time-mode))
      (display-time-mode t))

    ;; (when (fboundp 'display-time-mode)
    ;;   (display-time-mode t))

    (setq global-mode-string (remove 'display-time-string global-mode-string))
    (setq global-mode-string '("" celestial-mode-line-string display-time-string))

    (tab-bar-mode +1)

    (when (string= (system-name) "jhnuc")
      (keycast-tab-bar-mode +1))

    ;; load modus-themes
    (modus-themes-toggle)
    )

  (add-hook 'doom-after-init-hook #'my/load-global-mode-string 80)
  (add-hook 'doom-after-reload-hook #'my/load-global-mode-string)
  )

;;;; my/workspaces

;;;###autoload
(defun my/open-workspaces ()
  (interactive)

  ;; (message "my/open-workspaces")
  (+workspace/new-named "git")
  (find-file user-project-directory)

  (+workspace/new-named "dots")
  (find-file doom-user-dir)

  (+workspace/new-named "feed")
  (elfeed)
  ;; (bh/switch-to-scratch)

  (+workspace/switch-to-0) ;; main
  ;; (find-file (concat denote-directory "notes")) ; for denote-dired excerpt
  ;; (evil-window-vsplit)
  (my/denote-random-note-from-directory (concat denote-directory "notes"))

  (setq org-agenda-file org-user-agenda-files) ; reset
  ;; (my/add-today-journal-to-agenda) -- use org-journal-enable-agenda-integration
  (my/refresh-agenda-files)
  ;; (ash-goto-org-agenda) ; tab-bar

  ;; (setq tab-bar-close-button nil)
  ;; (tab-bar-new-tab)
  ;; (bh/switch-to-scratch)
  ;; (tab-bar-select-tab 1)
  )

(add-hook 'doom-first-input-hook #'my/open-workspaces)

;;; tab-line-mode on emacs-30

(when (eq emacs-major-version 30)
  (use-package tab-line
    :demand t
    :bind
    (("C-<iso-lefttab>" . tab-line-switch-to-prev-tab)
     ("C-<tab>" . tab-line-switch-to-next-tab))
    :config
    (global-tab-line-mode 1)
    (setq
     tab-line-new-button-show nil
     tab-line-close-button-show nil))
  )

;;; Ten with etags

;; gavinok-dotfiles/init.el
;; Getting added in emacs 30 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67687
;; (use-package etags-regen
;;   :when (executable-find "etags")
;;   :custom (etags-regen-tags-file "/tmp/TAGS")
;;   :commands etags-regen-mode
;;   :bind (("C-c t" . complete-tag)
;;          ("C-c M-." . my/goto-etags))
;;   :init
;;   (defvar etags-regen-mode-map (make-sparse-keymap))
;;   (add-to-list 'minor-mode-map-alist (cons 'etags-regen-mode etags-regen-mode-map)))

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

;; ;; eww-mode nov-mode -- conflict face 켜면 안된다.

(use-package! ten
  :after consult
  :defer 2
  ;; :bind (("M-c t" . complete-tag)
  ;;        ("C-c M-." . my/goto-etags))
  :hook ((org-mode Info-mode) . ten-font-lock-mode) ;; text-mode
  :init
  (setq ten-file-extensions '("org" "md" "txt"))
  (setq ten-exclude-regexps '("/\\."))
  ;; I am listing two specific dictionary files in the `test/`
  ;; subdirectory as an example below. You can list the
  ;; `~/src/ten/test/' directory to let Ten to search files recursively
  ;; in the directory and subdirectories in it. There are about 5,000
  ;; terms in total and I don't experience any perfomance issue on my
  ;; old Lenovo Thinkpad laptop. Ten looks for files with an extension
  ;; listed in `ten-file-extensions' and excludes files and those in
  ;; directories that match the list of regexps `ten-exclude-regexps'.
  ;; (setq ten-files-and-directories
  ;;       '( "~/sync/emacs/git/default/ten/test/Glossary-philosophy.txt"
  ;;          "~/sync/emacs/git/default/ten/test/Glossary-of-graph-theory.txt"))
  ;; The dictionary file (only one at a time can be active through
  ;; `etags', but you can switch between more than one of them if you
  ;; need to. The switching experience is not intuitive and it's a TODO
  ;; to improve it.)
  ;; (setq ten-tags-file-default "~/sync/emacs/git/default/ten/ten-TAGS")
  :config
  (require 'consult-ten)
  (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
  )

;;; llmclient
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

;;;; TODO llmclient: aider.el

(use-package! aider
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  ;; (setq aider-args '("--model" "xai/grok-beta"))
  (setq aider-args '("--model" "gpt-4o-mini"))
  (setenv "ANTHROPIC_API_KEY" user-claude-api-key)
  (setenv "OPENAI_API_KEY" user-openai-api-key)
  (setenv "GEMINI_API_KEY" user-gemini-api-key)
  (setenv "PERPLEXITYAI_API_KEY" user-perplexity-api-key)
  (setenv "XAI_API_KEY" user-xai-api-key))

;; Or use chatgpt model since it is most well known
;; (setq aider-args '("--model" "gpt-4o-mini"))
;; Or use gemini v2 model since it is very good and free
;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))

;; Optional: Set a key binding for the transient menu
;; (global-set-key (kbd "C-c a") 'aider-transient-menu)

;;;; DONT llmclient: wolframalpha

;; ziova/wolfram.el
;; (use-package! wolfram
;;   :config (setq wolfram-alpha-app-id user-wolfram-alpha-app-id))

;;;; DONT llmclient: github copilot

;; (use-package! copilot
;;   :commands (copilot-login copilot-diagnose)
;;   :init
;;   ;; Sometimes the copilot agent doesn't start. Restarting fixes the issue.
;;   (setq copilot-indent-offset-warning-disable t
;;         copilot-max-char 10000) ; default 100000
;;   :bind (:map copilot-completion-map
;;               ("C-g" . 'copilot-clear-overlay)
;;               ("M-p" . 'copilot-previous-completion)
;;               ("M-n" . 'copilot-next-completion)
;;               ("<tab>" . 'copilot-accept-completion) ; vscode
;;               ("TAB" . 'copilot-accept-completion) ; vscode
;;               ("M-f" . 'copilot-accept-completion-by-word)
;;               ("M-<return>" . 'copilot-accept-completion-by-line)
;;               ("M-]" . 'copilot-next-completion) ; vscode
;;               ("M-[" . 'copilot-next-completion) ; vscode
;;               ;; ("C-'" . 'copilot-accept-completion)
;;               ;; ("C-;" . 'copilot-accept-completion)
;;               )
;;   ;; :hook ((prog-mode . copilot-mode))
;;   ;; (org-mode . copilot-mode)
;;   ;; (markdown-mode . copilot-mode)
;;   )

;;;; DONT llmclient: github copilot-chat

;; (use-package! copilot-chat
;;   :after request
;;   :config
;;   (setq copilot-chat-backend 'request)
;;   (setq copilot-chat-frontend 'markdown)
;;   ;; From https://github.com/chep/copilot-chat.el/issues/24
;;   (defun meain/copilot-chat-display (prefix)
;;     "Opens the Copilot chat window, adding the current buffer to the context.
;; Called with a PREFIX, resets the context buffer list before opening"
;;     (interactive "P")

;;     (require 'copilot-chat)
;;     (let ((buf (current-buffer)))

;;       ;; Explicit reset before doing anything, avoid it resetting later on
;;       ;; target-fn and ignoring the added buffers
;;       (unless (copilot-chat--ready-p)
;;         (copilot-chat-reset))

;;       (when prefix (copilot-chat--clear-buffers))

;;       (copilot-chat--add-buffer buf)
;;       (copilot-chat-display)))
;;   )

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

;;;; DONT llmclient: kagi

;; (use-package! kagi
;;   :custom
;;   (kagi-api-token user-kagi-api-key)
;;   ;; (kagi-api-token (lambda () (password-store-get "Kagi/API")))

;;   ;; Universal Summarizer settings
;;   (kagi-summarizer-engine "cecil") ;; Formal, technical, analytical summary.
;;   (kagi-summarizer-default-language "KO")
;;   (kagi-summarizer-cache t))

;;; yeetube

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

;;; copy-screenshot-markdown

;; https://tristancacqueray.github.io/blog/emacs-30

(defun get-newest-file-from-dir  (path)
  "Return the latest file in PATH."
  (car (directory-files path 'full nil #'file-newer-than-file-p)))

(defun copy-screenshot-markdown (name)
  "Move the latest screenshot and insert markdown link with NAME."
  (interactive "Mname: ")
  (let* ((infile (expand-file-name (get-newest-file-from-dir org-screenshot-path)))
         (outdir (concat (file-name-directory (buffer-file-name)) (concat org-directory "images")))
         (outfile (expand-file-name (concat name ".png") outdir)))
    (unless (file-directory-p outdir) (make-directory outdir t))
    (rename-file infile outfile)
    (insert (concat "![" name "]( ../images/" (file-name-nondirectory outfile) ")"))
    (newline)
    (newline)))

;;; dired+ hugo

;; Batch Export Files with Org-Hugo
;; mark files and then batch export them with this command

(progn
  (defun my/dired-hugo-export-wim-to-md ()
    (interactive)
    (require 'dired+)
    (diredp-do-apply/eval-marked 'org-hugo-export-wim-to-md '(4)))

  (after! org
    (define-key dired-mode-map (kbd "M-p") #'my/dired-hugo-export-wim-to-md)
    ))

;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "M-p")
;;               (lambda() (interactive)
;;                 (require 'dired+)
;;                 (diredp-do-apply/eval-marked 'org-hugo-export-wim-to-md '(4)))))

;;; Math and Latex

;;;; DONT org-latex-preview

;; /home/junghan/sync/man/dotsamples/doom/tecosaur-dot-doom/config.org
;; (use-package! org-latex-preview
;;   :config
;;   (setq org-startup-with-latex-preview t) ; doom nil
;;   (setq org-highlight-latex-and-related '(native script entities)) ; doom org +pretty
;;   ;; (setq org-highlight-latex-and-related '(native)) ; doom nil
;;   ;; Increase preview width
;;   (plist-put org-latex-preview-appearance-options
;;              :page-width 0.8)

;;   ;; Use dvisvgm to generate previews
;;   ;; You don't need this, it's the default:
;;   (setq org-latex-preview-process-default 'dvisvgm)

;;   ;; Turn on auto-mode, it's built into Org and much faster/more featured than org-fragtog.
;;   ;; (Remember to turn off/uninstall org-fragtog.)
;;   (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

;;   ;; Block C-n and C-p from opening up previews when using auto-mode
;;   ;; (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
;;   ;; (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

;;   ;; Enable consistent equation numbering
;;   ;; (setq org-latex-preview-numbered t)

;;   ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
;;   ;; fragment and updates the preview in real-time as you edit it.
;;   ;; To preview only environments, set it to '(block edit-special) instead
;;   (setq org-latex-preview-live t)

;;   ;; More immediate live-previews -- the default delay is 1 second
;;   ;; (setq org-latex-preview-live-debounce 0.25)

;;   ;; FIXME work with ox-hugo
;;   (progn
;;     (require 'ox-html)
;;     (require 'ox-hugo)

;;     ;; (setq org-export-with-LaTeX-fragments t)
;;     (plist-put org-html-latex-image-options :inline '(svg svg-embed))
;;     (advice-add 'org-blackfriday--update-ltximg-path
;;                 :around
;;                 (lambda (orig-fn html-string)
;;                   (if (plist-get org-html-latex-image-options :inline)
;;                       html-string
;;                     (funcall orig-fn html-string)))
;;                 '((name . inline-image-workaround)))

;;     ;; glossary
;;     ;; (after! org-glossary
;;     ;;     (setq org-glossary-toplevel-only nil)
;;     ;;     (org-glossary-set-export-spec 'hugo t
;;     ;;       :use "<a class=\"org-gls\" href=\"#gls-%K\">%t</a>"
;;     ;;       :definition "<a name=\"gls-%K\">%t</a>"
;;     ;;       :definition-structure "%d\n\\colon{} %v [%n uses]\n")
;;     ;;     (org-glossary-set-export-spec 'hugo 'glossary
;;     ;;       :heading "*** Glossary")
;;     ;;     (defun org-glossary--expand-print-keyword (backend terms keyword)
;;     ;;       "Call `org-glossary--expand-print' with paramaters and terms based on KEYWORD.
;;     ;; BACKEND is passed through unmodified, but TERMS may be modified depending on
;;     ;; the :consume parameter extracted from KEYWORD."
;;     ;;       (let ((heading (org-element-lineage keyword '(headline org-data)))
;;     ;;             (parameters (org-combine-plists
;;     ;;                          org-glossary-default-print-parameters
;;     ;;                          (org-glossary--parse-print-keyword-value
;;     ;;                           (org-element-property :value keyword)))))
;;     ;;         (while (and heading
;;     ;;                     (not (eq (org-element-type heading) 'org-data))
;;     ;;                     (> (org-element-property :level heading)
;;     ;;                        (plist-get parameters :level)))
;;     ;;           (setq heading (org-element-lineage heading '(headline org-data))))
;;     ;;         (org-glossary--expand-print
;;     ;;          backend
;;     ;;          (org-glossary--extract-uses-in-region
;;     ;;           terms
;;     ;;           (if heading (org-element-property :begin heading) (point-min))
;;     ;;           (if heading (org-element-property :end heading) (point-max))
;;     ;;           (plist-get parameters :type)
;;     ;;           (plist-get parameters :consume))
;;     ;;          parameters))))

;;     ;; (add-to-list 'org-hugo-special-block-type-properties
;;     ;;              '("sidenote" . (:trim-pre t :trim-post t)))
;;     ;; (setq org-hugo-paired-shortcodes "%sidenote")
;;     ;; (define-minor-mode my/org-hugo-mode
;;     ;;   "Helper mode for org-hugo previews."
;;     ;;   :keymap (make-sparse-keymap)
;;     ;;   :init-value nil)
;;     ;; (defun my/org-hugo-preview (&optional arg)
;;     ;;   (interactive "P")
;;     ;;   (pcase-let* ((sec nil)
;;     ;;                (`(,sec . ,title)
;;     ;;                 (save-excursion
;;     ;;                   (org-previous-visible-heading 1)
;;     ;;                   (let ((title (org-element-property
;;     ;;                                 :EXPORT_FILE_NAME
;;     ;;                                 (org-element-at-point))))
;;     ;;                     (while (and (not sec) (org-up-heading-safe))
;;     ;;                       (setq sec (org-element-property
;;     ;;                                  :EXPORT_HUGO_SECTION
;;     ;;                                  (org-element-at-point))
;;     ;;                             title (or title
;;     ;;                                       (org-element-property
;;     ;;                                        :EXPORT_FILE_NAME
;;     ;;                                        (org-element-at-point)))))
;;     ;;                     (cons sec title)))))
;;     ;;     (if-let ((_ title)
;;     ;;              (url (concat
;;     ;;                    "http://localhost:1313/"
;;     ;;                    (if sec (downcase sec) "software")
;;     ;;                    "/" title)))
;;     ;;         (progn (save-buffer)
;;     ;;                (unless (bound-and-true-p org-hugo-auto-export-mode)
;;     ;;                  (org-hugo-export-wim-to-md))
;;     ;;                (if arg
;;     ;;                    (progn (other-window 1) (eww url) (other-window -1))
;;     ;;                  (browse-url url)))
;;     ;;       (message "No preview url found."))))

;;     (require 'org-compat)
;;     ;; (setq org-html-with-latex t)
;;     (defun org-html-format-latex (latex-frag processing-type info)
;;       "Format a LaTeX fragment LATEX-FRAG into HTML.
;;   PROCESSING-TYPE designates the tool used for conversion.  It can
;;   be `mathjax', `verbatim', `html', nil, t or symbols in
;;   `org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
;;   `imagemagick'.  See `org-html-with-latex' for more information.
;;   INFO is a plist containing export properties."
;;       (let ((cache-relpath "") (cache-dir ""))
;;         (unless (or (eq processing-type 'mathjax)
;;                     (eq processing-type 'html))
;;           (let ((bfn (or (buffer-file-name)
;; 		         (make-temp-name
;; 		          (expand-file-name "latex" temporary-file-directory))))
;; 	        (latex-header
;; 	         (let ((header (plist-get info :latex-header)))
;; 	           (and header
;; 		        (concat (mapconcat
;; 			         (lambda (line) (concat "#+LATEX_HEADER: " line))
;; 			         (org-split-string header "\n")
;; 			         "\n")
;; 			        "\n")))))
;; 	    (setq cache-relpath
;; 	          (concat (file-name-as-directory org-preview-latex-image-directory)
;; 		          (file-name-sans-extension
;; 		           (file-name-nondirectory bfn)))
;; 	          cache-dir (file-name-directory bfn))
;; 	    ;; Re-create LaTeX environment from original buffer in
;; 	    ;; temporary buffer so that dvipng/imagemagick can properly
;; 	    ;; turn the fragment into an image.
;; 	    (setq latex-frag (concat latex-header latex-frag))))
;;         (org-export-with-buffer-copy
;;          :to-buffer (get-buffer-create " *Org HTML Export LaTeX*")
;;          :drop-visibility t :drop-narrowing t :drop-contents t
;;          (erase-buffer)
;;          (insert latex-frag)
;;          (org-format-latex cache-relpath nil nil cache-dir nil
;; 		           "Creating LaTeX Image..." nil processing-type)
;;          (buffer-string))))
;;     )
;;   )

;;;; DONT cdlatex

;; rather use doom's config

;; (use-package! cdlatex
;;   :bind
;;   (("C-\"" . (lambda () (interactive)
;;                (cdlatex-ensure-math)
;;                (cdlatex-math-symbol))))
;;   :init
;;   (setq cdlatex-math-symbol-prefix ?\;)
;;   :config

;;   (defun lauremacs-cdlatex-add-math-symbols ()
;;     (add-multiple-into-list
;;      'cdlatex-math-symbol-alist-comb
;;      '(
;;        (?.  "\\cdot"   "\\dots")
;;        (?\; "\\;")
;;        (?C  ""         "\\mathbb{C}"   "\\arccos")
;;        (?N  "\\nabla"  "\\mathbb{N}"   "\\exp")
;;        (?Q  "\\Theta"  "\\mathbb{Q}")
;;        (?R  "\\Re"     "\\mathbb{R}")
;;        (?Z  ""         "\\mathbb{Z}")
;;        )))

;;   (define-minor-mode org-math-mode
;;     "Some config to write math on `org-mode'."
;;     :lighter "org-math-mode"
;;     (org-fragtog-mode 1)
;;     (org-cdlatex-mode 1)
;;     (lauremacs-cdlatex-add-math-symbols))
;;   )

;;;; TODO aas : auto-activating-snippets

;; https://github.com/ymarco/auto-activating-snippets
;; ~/sync/man/dotsamples/doom/lemon-dot-doom/config.el

;; (use-package! aas
;;   ;; can't defer loading of this as we need it in every single spawned
;;   ;; buffer including scratch
;;   :init (add-hook 'find-file-hook #'aas-activate-for-major-mode)
;;   :config
;;   (aas-global-mode)
;;   (aas-set-snippets 'global
;;     ":-)" "🙂"
;;     ";--" "—"
;;     ";-." "→"
;;     ";=." "⇒"
;;     ";!=" "≠"
;;     "-." "->"
;;     "=." "=>"
;;     "j9" "("))

;;;; org-fragtog

;; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  :init
  (setq org-fragtog-preview-delay 0.2)
  ;; (setq org-startup-with-latex-preview t) ; doom nil
  (setq org-highlight-latex-and-related '(native script entities)) ; doom org +pretty
  ;; (setq org-highlight-latex-and-related '(native)) ; doom nil
  )

;;;; laas : latex-auto-activating-snippets

;; https://github.com/tecosaur/LaTeX-auto-activating-snippets
;; (browse-url-emacs "https://raw.githubusercontent.com/tecosaur/LaTeX-auto-activating-snippets/master/README.org")

(use-package! laas
  :hook ((LaTeX-mode . laas-mode)
	 (org-mode . laas-mode)))

;;   (defun laas-tex-fold-maybe ()
;;     (unless (equal "/" aas-transient-snippet-key)
;;       (+latex-fold-last-macro-a)))
;;   (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe)

;;;; DONT literate-calc-mode

;; (use-package! literate-calc-mode)

;;; HUGO for quartz

(setq hugo-base-static-dir user-hugo-notes-static-dir)
;; (setq hugo-base-static-dir user-hugo-blog-dir)
;; Setup export processor; default csl/citeproc-el, with biblatex for latex

(after! oc
  (require 'citar-citeproc)
  (setq org-cite-csl-link-cites nil) ; default t
  (setq org-cite-export-processors '((latex biblatex) (t csl))))

(after! ox-hugo
  (defun org-hugo--attachment-rewrite-maybe (path info)
    "Copy local images and pdfs to the static/bundle directory if needed.
Also update the link paths to match those.

PATH is the path to the image or any other attachment.

INFO is a plist used as a communication channel."
    ;; (message "[ox-hugo attachment DBG] The Hugo section is: %s" (plist-get info :hugo-section))
    ;; (message "[ox-hugo attachment DBG] The Hugo base dir is: %s" (plist-get info :hugo-base-dir))
    (let* ((pub-dir (org-hugo--get-pub-dir info)) ;This needs to happen first so that the check for HUGO_BASE_DIR happens.
           (hugo-base-dir (file-name-as-directory (plist-get info :hugo-base-dir)))
           (path-unhexified (url-unhex-string path))
           (path-true (file-truename path-unhexified))
           (exportables org-hugo-external-file-extensions-allowed-for-copying)
           (bundle-dir (and (plist-get info :hugo-bundle) pub-dir))
           (bundle-name (when bundle-dir
                          (let* ((content-dir (file-truename
                                               (file-name-as-directory
                                                (expand-file-name "content" hugo-base-dir))))
                                 (is-home-branch-bundle (string= bundle-dir content-dir)))
                            (cond
                             (is-home-branch-bundle
                              "_home")
                             (t ;`bundle-dir'="/foo/bar/" -> `bundle-name'="bar"
                              (file-name-base (directory-file-name bundle-dir)))))))
           (static-dir (file-truename
                        (file-name-as-directory
                         (expand-file-name "static" hugo-base-static-dir))))
           (dest-dir (or bundle-dir static-dir))
           ret)
      (unless (file-directory-p static-dir)
        (user-error "Please create the %s directory" static-dir))
      (if (and (file-exists-p path-true)
               (member (file-name-extension path-unhexified) exportables)
               (file-directory-p dest-dir))
          (progn
            ;; Check if `path-true' is already inside `dest-dir'.
            (if (string-match (regexp-quote dest-dir) path-true)
                (progn
                  ;; If so, return *only* the path considering the
                  ;; destination directory as root.
                  (setq ret (concat "/" (substring path-true (match-end 0)))))
              (let* ((file-name-relative-path
                      (cond
                       ((string-match "/static/" path-true)
                        ;; `path-true' is "/foo/static/bar/baz.png",
                        ;; return "bar/baz.png".
                        ;; (message "[ox-hugo DBG attch rewrite] path contains static")
                        ;; If path-true contains "/static/", set the
                        ;; `dest-dir' to `static-dir' (even if this is a
                        ;; page bundle).
                        (setq dest-dir static-dir)
                        (substring path-true (match-end 0)))
                       (bundle-dir
                        (cond
                         ((string-match (concat "/" (regexp-quote bundle-name) "/") path-true)
                          ;; This is a page bundle.  `bundle-name' is
                          ;; "<BUNDLE_NAME>", `path-true' is
                          ;; "<ORG_FILE_DIR>/bar/<BUNDLE_NAME>/zoo/baz.png",
                          ;; return "zoo/baz.png".
                          ;; (message "[ox-hugo DBG attch rewrite BUNDLE 1] bundle-name: %s" bundle-name)
                          ;; (message "[ox-hugo DBG attch rewrite BUNDLE 1] attch along with Org content: %s"
                          ;;          (substring path-true (match-end 0)))
                          (substring path-true (match-end 0)))
                         ((string-match (regexp-quote default-directory) path-true)
                          ;; This is a page bundle.  `default-path' is
                          ;; "<ORG_FILE_DIR>/", `path-true' is
                          ;; "<ORG_FILE_DIR>/bar/baz.png", return
                          ;; "bar/baz.png".
                          ;; (message "[ox-hugo DBG attch rewrite BUNDLE 2] attch along with Org content: %s"
                          ;;          (substring path-true (match-end 0)))
                          (substring path-true (match-end 0)))
                         (t
                          ;; This is a page bundle.  `default-path' is
                          ;; "<ORG_FILE_DIR>/", `path-true' is
                          ;; "/foo/bar/baz.png", return "baz.png".
                          ;; (message "[ox-hugo DBG attch rewrite BUNDLE 3] attch neither in static nor in Org file dir")
                          (file-name-nondirectory path-unhexified))))
                       (t
                        ;; Else, `path-true' is "/foo/bar/baz.png",
                        ;; return "ox-hugo/baz.png".  "ox-hugo" is the
                        ;; default value of
                        ;; `org-hugo-default-static-subdirectory-for-externals'.
                        ;; (message "[ox-hugo DBG attch rewrite] neither BUNDLE nor contains static")
                        (concat
                         (file-name-as-directory org-hugo-default-static-subdirectory-for-externals)
                         (file-name-nondirectory path-unhexified)))))
                     (dest-path (concat dest-dir file-name-relative-path))
                     (dest-path-dir (file-name-directory dest-path)))
                ;; The `dest-dir' would already exist.  But if
                ;; `file-name-relative-path' is "images/image.png" or
                ;; "foo/bar.txt", it's likely that "`dest-dir'/images"
                ;; or "`dest-dir'/foo" might not exist.  So create those
                ;; if needed below.
                (unless (file-exists-p dest-path-dir)
                  (mkdir dest-path-dir :parents))
                ;; (message "[ox-hugo DBG attch rewrite] file-name-relative-path: %s" file-name-relative-path)
                ;; (message "[ox-hugo DBG attch rewrite] dest-path: %s" dest-path)
                ;; (message "[ox-hugo DBG attch rewrite] dest-path-dir: %s" dest-path-dir)

                ;; Do the copy only if the file to be copied is newer or
                ;; doesn't exist in the static dir.
                (when (file-newer-than-file-p path-true dest-path)
                  (message "[ox-hugo] Copied %S to %S" path-true dest-path)
                  (copy-file path-true dest-path :ok-if-already-exists))
                (setq ret (if (and bundle-dir
                                   (string= bundle-dir dest-dir))
                              ;; If attachments are copied to the bundle
                              ;; directory, don't prefix the path as "/"
                              ;; as those paths won't exist at the site
                              ;; base URL.
                              file-name-relative-path
                            (concat "/" file-name-relative-path))))))
        (setq ret path))
      ;; (message "[ox-hugo DBG attch rewrite] returned path: %s" ret)
      ret))
  )

;;; NOTE consult-omni

(progn
  ;; /home/junghan/sync/man/dotsamples/vanilla/sachac-dotfiles/Sacha.org
  (defun my-insert-or-replace-link (url &optional title)
    "Insert a link, wrap the current region in a link, or replace the current link."
    (cond
     ((derived-mode-p 'org-mode)
      (cond
       ((org-in-regexp org-link-bracket-re 1)
        (when (match-end 2) (setq title (match-string-no-properties 2)))
        (delete-region (match-beginning 0) (match-end 0)))
       ((org-in-regexp org-link-any-re 1)
        (delete-region (match-beginning 0) (match-end 0)))
       ((region-active-p)
        (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))))
      ;; update link
      (insert (org-link-make-string url title)))
     ((derived-mode-p 'org-mode)		 ; not in a link
      (insert (org-link-make-string url title)))
     ((and (region-active-p) (derived-mode-p 'markdown-mode))
      (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (insert (format "[%s](%s)" title url)))
     ((derived-mode-p 'markdown-mode)
      (insert (format "[%s](%s)" title url)))
     (t
      (insert (format "%s (%s)" title url)))))

  ;; override the embark actions
  (defun my-consult-omni-embark-copy-url-as-kill (cand)
    "Don't add spaces."
    (when-let ((s (and (stringp cand) (get-text-property 0 :url cand))))
      (kill-new (string-trim s))))

  (defun my-consult-omni-embark-insert-url (cand)
    "Don't add spaces."
    (when-let ((s (and (stringp cand) (get-text-property 0 :url cand))))
      (insert (string-trim s))))

  (defun my-consult-omni-embark-copy-title-as-kill (cand)
    "Don't add spaces."
    (when-let ((s (and (stringp cand) (get-text-property 0 :title cand))))
      (kill-new (string-trim s))))

  (defun my-consult-omni-embark-insert-title (cand)
    "Don't add spaces."
    (when-let ((s (and (stringp cand) (get-text-property 0 :title cand))))
      (insert (string-trim s))))

  (defun my-consult-omni-embark-insert-link (cand)
    "Don't add spaces."
    (let ((url (and (stringp cand) (get-text-property 0 :url cand )))
          (title (and (stringp cand) (get-text-property 0 :title cand))))
      (my-insert-or-replace-link url title)))
  )

(use-package! consult-omni
  :defer 2
  :after (consult embark)
  :commands (consult-omni-transient consult-omni-multi)
  :custom
  (consult-omni-show-preview t) ;;; show previews
  ;; (consult-omni-preview-key '("M-." "C-SPC")) ;;; set the preview key to C-SPC
  (consult-omni-default-page 0) ;;; set the default page (default is 0 for the first page)
  :bind
  (("M-g w" . consult-omni)
   :map consult-omni-embark-general-actions-map
   ("I l" .  #'my-consult-omni-embark-insert-link)
   ("I u" .  #'my-consult-omni-embark-insert-url)
   ("I t" .  #'my-consult-omni-embark-insert-title)
   ("W u" . #'my-consult-omni-embark-copy-url-as-kill)
   ("W t" . #'my-consult-omni-embark-copy-title-as-kill))
  :config
  (require 'consult-omni-sources)
  (require 'consult-omni-embark)

  ;; custom from agzam
  (require 'my-consult-omni)

  (progn
    ;; (require 'consult-omni-apps)
    ;; (require 'consult-omni-bing)
    ;; (require 'consult-omni-brave-autosuggest)
    ;; (require 'consult-omni-brave)
    ;; (require 'consult-omni-browser-history)
    (require 'consult-omni-buffer)
    (require 'consult-omni-calc)
    ;; (require 'consult-omni-chatgpt)
    (require 'consult-omni-consult-notes)
    ;; (require 'consult-omni-dict)
    ;; (require 'consult-omni-doi)
    ;; (require 'consult-omni-duckduckgo)
    (require 'consult-omni-elfeed)
    (require 'consult-omni-fd)
    ;; (require 'consult-omni-find)
    (require 'consult-omni-gh)
    (require 'consult-omni-git-grep)
    ;; (require 'consult-omni-google)
    ;; (require 'consult-omni-google-autosuggest)
    (require 'consult-omni-gptel)
    ;; (require 'consult-omni-grep)
    ;; (require 'consult-omni-invidious)
    ;; (require 'consult-omni-line-multi)
    ;; (require 'consult-omni-locate)
    ;; (require 'consult-omni-man)
    ;; (require 'consult-omni-mdfind)
    ;; (require 'consult-omni-mu4e)
    (require 'consult-omni-notes)
    ;; (require 'consult-omni-notmuch)
    ;; (require 'consult-omni-numi)
    ;; (require 'consult-omni-org-agenda)
    ;; (require 'consult-omni-pubmed)
    (require 'consult-omni-projects)
    (require 'consult-omni-ripgrep)
    ;; (require 'consult-omni-ripgrep-all)
    ;; (require 'consult-omni-scopus)
    ;; (require 'consult-omni-stackoverflow)
    (require 'consult-omni-wikipedia)
    (require 'consult-omni-youtube)
    )

  (setq consult-omni-multi-sources '(
                                     ;; "DuckDuckGo AP/"
                                     "Google"
                                     "Brave"
                                     "Wikipedia"
                                     ;; "Browser History"
                                     "gptel"
                                     "GitHub"
                                     "elfeed"
                                     ;; "notmuch"
                                     "YouTube"))
  ;; (setq consult-omni-default-preview-function #'eww-browse-url)

  ;; ;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  (setq consult-omni-openai-api-key user-openai-api-key)
  (setq consult-omni-brave-api-key user-brave-api-key)
  ;; Set API KEYs. It is recommended to use a function that returns the string for better security.
  ;; (setq consult-omni-google-customsearch-key "YOUR-GOOGLE-API-KEY-OR-FUNCTION")
  ;; (setq consult-omni-google-customsearch-cx "YOUR-GOOGLE-CX-NUMBER-OR-FUNCTION")

  ;; (consult-omni--set-api-keys) ; agzam

  (setq consult-omni-default-count 30
        consult-omni-dynamic-input-debounce 0.7
        consult-omni-dynamic-refresh-delay 0.5)

  (defadvice! consult-omni-use-thing-at-point-a
    (fn &optional initial no-cb &rest args)
    :around #'consult-omni-multi
    :around #'consult-omni-google
    :around #'consult-omni-wikipedia
    :around #'consult-omni-youtube
    :around #'consult-omni-github
    :around #'consult-omni-gptel
    :around #'consult-omni-browser-history
    :around #'consult-omni-notmuch
    :around #'consult-omni-elfeed
    (let ((init (or initial
                    (if (use-region-p)
                        (buffer-substring (region-beginning) (region-end))
                      (thing-at-point 'symbol :no-props)))))
      (apply fn init no-cb args)))

  (defadvice! consult-omni--multi-dynamic-no-match-a (orig-fn &rest args)
    "Require no match for omni searches."
    :around #'consult-omni--multi-dynamic
    (apply orig-fn (plist-put args :require-match nil)))

  (defun consult-omni-embark-video-process (cand)
    (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
        (+process-external-url url)))

  (map! :map consult-omni-embark-video-actions-map
        "e" #'consult-omni-embark-video-process)
  )

;;; TODO Load +llm summarize-buffer

(load! "+llm")
(global-set-key (kbd "M-g SPC") '+gpt-dwim-current-buffer)

;;; TODO Scheme Clojure Racket with SICP/SICM

;;;; scheme with geiser-mit

(use-package! geiser-mit
  :config
  (setenv "MITSCHEME_HEAP_SIZE" "100000") ; 16384
  (setenv "MITSCHEME_LIBRARY_PATH" "/usr/lib/x86_64-linux-gnu/mit-scheme")
  (setenv "MITSCHEME_BAND" "mechanics.com")

  ;; (setenv "DISPLAY" ":0")
  (setq geiser-active-implementations '(mit))
  (setq geiser-mit-binary "/usr/bin/mit-scheme")
  )

;;;; DONT built-in scheme not geiser

;; /home/junghan/doomemacs-git/modules/lang/scheme/
;; (progn
;;   (defvar calculate-lisp-indent-last-sexp)
;;   ;; Adapted from https://github.com/alezost/emacs-config/blob/master/utils/al-scheme.el#L76-L123
;; ;;;###autoload
;;   (defun +scheme-indent-function-a (indent-point state)
;;     "Advice to replace `scheme-indent-function'.

;; This function is the same as `scheme-indent-function' except it properly indents
;; property lists and names starting with 'default'."
;;     (let ((normal-indent (current-column)))
;;       (goto-char (1+ (elt state 1)))
;;       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
;;       (if (and (elt state 2)
;;                ;; NOTE looking-at -> looking-at-p
;;                (not (looking-at-p "\\sw\\|\\s_")))
;;           (progn
;;             ;; NOTE (if (not ...) (progn ...)) -> (unless ... ...)
;;             (unless (> (save-excursion (forward-line 1) (point))
;;                        calculate-lisp-indent-last-sexp)
;;               (goto-char calculate-lisp-indent-last-sexp)
;;               (beginning-of-line)
;;               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
;;             (backward-prefix-chars)
;;             (current-column))
;;         ;; NOTE let -> let* & moved `method' def into let bindings
;;         (let* ((function (buffer-substring
;;                           (point) (progn (forward-sexp 1) (point))))
;;                (method (or (get (intern-soft function) 'scheme-indent-function)
;;                            (get (intern-soft function) 'scheme-indent-hook))))
;;           (cond ((or (eq method 'defun)
;;                      (and (null method)
;;                           (> (length function) 3)
;;                           ;; NOTE string-match -> string-match-p
;;                           ;; NOTE The original regexp is "\\`def" but it will mess
;;                           ;;      up indentation with such names as 'default-...'.
;;                           (string-match-p "\\`def" function)))
;;                  (lisp-indent-defform state indent-point))
;;                 ;; NOTE Added this clause to handle alignment of keyword symbols
;;                 ((and (null method)
;;                       (> (length function) 1)
;;                       ;; NOTE string-match -> string-match-p
;;                       (string-match-p "\\`:" function))
;;                  (let ((lisp-body-indent 1))
;;                    (lisp-indent-defform state indent-point)))
;;                 ((integerp method)
;;                  (lisp-indent-specform method state indent-point normal-indent))
;;                 (method
;;                  (funcall method state indent-point normal-indent)))))))

;;   (use-package! scheme
;;     :interpreter ("scsh" . scheme-mode)
;;     :hook (scheme-mode . rainbow-delimiters-mode)
;;     :config
;;     (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(scheme-mode))
;;     (advice-add #'scheme-indent-function :override #'+scheme-indent-function-a))

;;   (require 'scheme)
;;   (after! scheme
;;     (add-hook 'scheme-mode-hook #'aggressive-indent-mode)

;;     ;; /home/junghan/sync/man/dotsamples/spacemacs/sritchie-spacemacs-scheme/init.el
;;     ;; required to get org-mode exporting the goodies.
;;     (require 'ob-mit-scheme) ; site-lisp

;;     ;; ;; this is used by xscheme now.
;;     (setq scheme-program-name "mechanics")

;;     (add-to-list 'auto-mode-alist '("\\.vlad\\'" . scheme-mode))
;;     (add-to-list 'auto-mode-alist '("\\.dvl\\'" . scheme-mode))
;;     (add-to-list 'auto-mode-alist '("\\.sc\\'" . scheme-mode))

;;     (defun mechanics-local ()
;;       (interactive)
;;       (run-scheme "mechanics"))

;;     (defun mechanics ()
;;       (interactive)
;;       (let ((default-directory (or (projectile-project-root)
;;                                    default-directory)))
;;         (call-interactively #'mechanics-local)))

;; ;;;###autoload
;;     (defun +scheme/open-repl ()
;;       "Open the Scheme REPL."
;;       (interactive)
;;       ;; (call-interactively #'geiser-repl-switch)
;;       (call-interactively #'mechanics-local)
;;       (current-buffer))

;;     )
;;   )

;;;; clojure

(when (modulep! :lang clojure)

;;;;; clerk-show

  (defun clerk-show ()
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;;;;; emmy


;;;;; clojure-mode

  ;; Do not indent single ; comment characters
  (add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

  (after! clojure-mode
    (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
    (define-key clojure-mode-map (kbd "C-c v") 'vega-view)
    )

;;;;; cider

  (after! cider
    ;; In recent versions, an option has been introduced that attempts to improve
    ;; the experience of CIDER by accessing java source & javadocs, though this
    ;; option is still currently considered beta.
    (setq cider-enrich-classpath t)

    (if (package-installed-p 'corfu)
        (evil-define-key 'insert cider-repl-mode-map
          (kbd "C-j") 'corfu-next
          (kbd "C-k") 'corfu-previous))

    ;; (add-to-list 'auto-mode-alist '("\\.clj_kondo\\'" . clojure-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.endl$" . clojure-mode))
    ;; (add-to-list 'magic-mode-alist '("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode))

    ;; Because of CIDER's insistence to send forms to all linked REPLs, we
    ;; *have* to be able to switch cljc buffer to clj/cljs mode without
    ;; cider complaining.
    ;; (setq clojure-verify-major-mode nil) ; 나중에 해보고
    ;; (setq clojure-indent-style 'align-arguments)

    ;; Vertically align s-expressions
    ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
    ;; (setq clojure-align-forms-automatically t)

    ;; manually use on lsp mode
    (defun my/cider-repl-prompt (namespace)
      "Return a prompt string that mentions NAMESPACE."
      (format "%s🦄 " (cider-abbreviate-ns namespace)))

    ;; NOTE 2022-11-21: for the linter (clj-kondo), refer to the Flymake
    ;; NOTE 2022-11-23: This is not final.  I will iterate on it over
    ;; time as I become more familiar with the requirements.
    (setq cider-repl-result-prefix ";; => "
          cider-eval-result-prefix ""
          cider-connection-message-fn t ; cute, but no!
          cider-repl-prompt-function #'my/cider-repl-prompt
          ;; cider-use-overlays nil ; echo area is fine
          )

    ;; (setq cider-preferred-build-tool 'clojure-cli)
    ;; (setq
    ;;  cider-prompt-for-symbol nil
    ;;  cider-repl-display-help-banner t ;; enable help banner
    ;;  ;; cider-print-fn 'puget                   ;; pretty printing with sorted keys / set values
    ;;  ;; clojure-toplevel-inside-comment-form t
    ;;  ;; cider-result-overlay-position 'at-point   ; results shown right after expression
    ;;  ;; cider-overlays-use-font-lock t
    ;;  cider-repl-buffer-size-limit 100          ; limit lines shown in REPL buffer
    ;;  nrepl-use-ssh-fallback-for-remote-hosts t ; connect via ssh to remote hosts
    ;;  )
    )

;;;;; clj-deps-new

  (use-package! clj-deps-new
    :defer 5
    :commands clj-deps-new)

;;;;; clay

  (use-package! clay
    :after cider
    :config
    (require 'clay))

;;;;; DONT kaocha-runner

  ;; Kaocha test runner from Emacs
  ;; - provides rich test reports
  ;; (use-package! kaocha-runner
  ;;   :after cider
  ;;   :config
  ;;   ;; enable Kaocha test runner
  ;;   (setq clojure-enable-kaocha-runner t))

;;;;; DONT cloure-essential-ref-nov

  ;; (use-package! clojure-essential-ref-nov
  ;;   :after cider
  ;;   :init
  ;;   (setq clojure-essential-ref-default-browse-fn #'clojure-essential-ref-nov-browse)
  ;;   (setq clojure-essential-ref-nov-epub-path "~/Documents/Book/Clojure_The_Essential_Reference_v31.epub")
  ;;   ;; :config
  ;;   ;; (with-eval-after-load 'cider
  ;;   ;;   (evil-define-key '(insert normal) cider-mode-map (kbd "C-0") 'clojure-essential-ref)
  ;;   ;;   (evil-define-key '(insert normal) cider-repl-mode-map (kbd "C-0") 'clojure-essential-ref))
  ;;   )


;;;;; DONT Clojure helper functions

  ;; Toggle reader comment #_ at beginnig of an expression
  ;; (defun clojure-toggle-reader-comment-sexp ()
  ;;   (interactive)
  ;;   (let* ((point-pos1 (point)))
  ;;     (evil-insert-line 0)
  ;;     (let* ((point-pos2 (point))
  ;;            (cmtstr "#_")
  ;;            (cmtstr-len (length cmtstr))
  ;;            (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
  ;;            (point-movement (if (string= cmtstr line-start) -2 2))
  ;;            (ending-point-pos (+ point-pos1 point-movement 1)))
  ;;       (if (string= cmtstr line-start)
  ;;           (delete-char cmtstr-len)
  ;;         (insert cmtstr))
  ;;       (goto-char ending-point-pos)))
  ;;   (evil-normal-state))

  ;; Assign keybinding to the toggle-reader-comment-sexp function
  ;; (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)

;;;;; DONT portal integration

  ;; def portal to the dev namespace to allow dereferencing via @dev/portal
  ;; (defun portal.api/open ()
  ;;   (interactive)
  ;;   (cider-nrepl-sync-request:eval
  ;;    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

  ;; (defun portal.api/clear ()
  ;;   (interactive)
  ;;   (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  ;; (defun portal.api/close ()
  ;;   (interactive)
  ;;   (cider-nrepl-sync-request:eval "(portal.api/close)"))

  ;; Key bindings added to Debug Clojure section
  ;; - , d p p - portal open
  ;; - , d p c - portal clear
  ;; - , d p D - portal clear
;;;;; clojure-cookbook with adoc-mode

  (use-package! adoc-mode
    :mode (("\\.adoc$" . adoc-mode)
           ("\\.asciidoc$" . adoc-mode)))

  (defun increment-clojure-cookbook ()
    "When reading the Clojure cookbook, find the next section, and
close the buffer. If the next section is a sub-directory or in
the next chapter, open Dired so you can find it manually."
    (interactive)
    (let* ((cur (buffer-name))
	   (split-cur (split-string cur "[-_]"))
	   (chap (car split-cur))
	   (rec (car (cdr split-cur)))
	   (rec-num (string-to-number rec))
	   (next-rec-num (1+ rec-num))
	   (next-rec-s (number-to-string next-rec-num))
	   (next-rec (if (< next-rec-num 10)
		         (concat "0" next-rec-s)
		       next-rec-s))
	   (target (file-name-completion (concat chap "-" next-rec) "")))
      (progn
        (if (equal target nil)
	    (dired (file-name-directory (buffer-file-name)))
	  (find-file target))
        (kill-buffer cur))))

  (after! adoc-mode
    (define-key adoc-mode-map (kbd "M-+") 'increment-clojure-cookbook)
    (add-hook 'adoc-mode-hook 'cider-mode))

;;;;; DONT ob-clojure with babashka

  ;; doom's default use cider
  ;; (require 'ob-clojure)
  ;; (setq! org-babel-clojure-backend 'babashka)

;;;;; end-of-clojure

  ) ;; end-of clojure

;;;; DONT racket-review with flycheck

;; (when (modulep! :lang racket)
;;   (require 'flycheck)
;;   ;; https://github.com/Bogdanp/racket-review
;;   (flycheck-define-checker racket-review
;;     "check racket source code using racket-review"
;;     :command ("raco" "review" source)
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
;;      (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
;;     :modes racket-mode)
;;   (add-to-list 'flycheck-checkers 'racket-review)
;;   )

;;; treesit-auto

;; (use-package! treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;;; Load ccmenu

;;;; with ccmenu

;; (use-package! password-store-menu
;;   :config (password-store-menu-enable))

;; (use-package! google-this
;;   :init
;;   (setq google-this-location-suffix "co.kr"))

(use-package! webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region)))

;; (use-package! fireplace :defer t)
;; (use-package! snow :defer t)

;;;; ccmenu: context-menu with casual

(when (display-graphic-p) ;; gui

  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (require 'ccmenu))

;;;; Terminal Mode

;; README /doomemacs-junghan0611/lisp/doom-ui.el

;; Terminal Mode
(unless (display-graphic-p) ; terminal
  (setq visible-cursor nil)
  (xterm-mouse-mode -1) ; important
  (setq fast-but-imprecise-scrolling nil)
  (setq hscroll-step 0)

  (show-paren-mode -1)
  (remove-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  ;; (remove-hook 'marginalia-mode-hook 'nerd-icons-completion-marginalia-setup)
  )

;;;; jupyter - repl

;; (setq jupyter-repl-echo-eval-p t)

;;; left blank on purpose
